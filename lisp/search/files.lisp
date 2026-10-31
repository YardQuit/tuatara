;;;; lisp/search/files.lisp --- Searching the text of many files as you type: search-in-files, search-in-formatted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The files under a directory are read on a thread of their own when the
;;;; search starts, into texts of a few megabytes each, while you type; what is
;;;; typed is searched on a thread of its own too, on nine cores in ten, each
;;;; new text searched as it comes, a new key stopping the search and starting
;;;; the next, and the lines found are the candidates as they come. Choosing
;;;; one visits its file at the match, and stops the reading and the search.

(in-package #:tuatara.defaults)

(defvar *search-files-limit* (* 128 1024 1024)
  "The most octets of text kept in memory for a search of many files
\(\\[search-in-files], \\[search-in-formatted]) and for each directory of
*INDEXED-DIRECTORIES* (and on disk with it, *INDEXES-KEPT-ON-DISK*): 128 MB as
standard. Text files that do not fit are not left out but searched from disk
at each search, a piece at a time, their lines coming after the others: memory
stays at this, and a search of them takes longer, about a second for each 1.5
GB on a fast disk, against a quarter of that kept in memory. Raise it to keep
more in memory, (* 4 1024 1024 1024) for 4 GB.")

(defvar *search-files-min-input* 3
  "How many characters search-in-files and search-in-formatted wait for before searching.")

(defvar *search-files-max-lines* 2000
  "The most matching lines search-in-files and search-in-formatted list.")

(defstruct (file-corpus (:constructor make-file-corpus (rope paths starts root)) (:copier nil))
  "The text of files read for searching: ROPE holds them one after another,
each ending with a newline; PATHS are their paths from ROOT, and STARTS the
offset in ROPE where each begins, in the same order."
  (rope nil :read-only t)
  (paths #() :type simple-vector :read-only t)
  (starts #() :type simple-vector :read-only t)
  (root "" :type string :read-only t))

(setf (documentation 'make-file-corpus 'function) "Return the text of files read for searching (see FILE-CORPUS)."
      (documentation 'file-corpus-p 'function) "True when OBJECT is a FILE-CORPUS."
      (documentation 'file-corpus-rope 'function) "The text of the files of a FILE-CORPUS, one after another."
      (documentation 'file-corpus-paths 'function) "The paths from the root of the files of a FILE-CORPUS."
      (documentation 'file-corpus-starts 'function) "Where each file of a FILE-CORPUS begins in its text."
      (documentation 'file-corpus-root 'function) "The directory the paths of a FILE-CORPUS are from.")

(defun read-text-file (path &key most)
  "The octets of the file at PATH, or NIL when it cannot be read, is no
regular file, looks binary (a zero octet among its first 64 KB, as pictures,
video and programs have: TUATARA.IO:BINARY-HEAD-P, as opening a file tells
it), is of a kind its first octets tell (TUATARA.IO:OCTETS-KIND: a PDF, a
picture, an archive, a disk image; a drawing written as text, SVG, is text),
or is encrypted, as its name or its header says (ENCRYPTED-FILE-P), so that
only text is read for searching and the text of an encrypted file never is.
With MOST, a file larger than MOST octets is not read beyond its first 64 KB,
its size known from the system: NIL, with :TOO-LARGE as a second value, so
that a gigantic file never passes through memory for a search that would not
keep it. Its access time is left alone where the system allows, and a named
pipe cannot hold it up."
  (unless (encrypted-name-p (tuatara.io:native-file-name path))
    (ignore-errors
     (tuatara.io:call-with-file
      path
      (lambda (fd path size)
        ;; a small file read whole at once; of a larger one, the part that tells it binary
        (let ((head (tuatara.io:read-fd-start fd path size tuatara.io:+binary-sample+ :past-size t)))
          (unless (or (tuatara.io:binary-head-p head)
                      (encrypted-head-p (subseq head 0 (min (length head) 200)))
                      ;; a kind of file told by its first octets: a picture, a PDF, an archive, a disk image
                      (not (member (nth-value 1 (tuatara.io:octets-kind head)) '(nil :svg))))
            (cond ((and most (> (max size (length head)) most)) (values nil :too-large))
                  ((< (length head) (min tuatara.io:+binary-sample+ (max 8192 (1+ size)))) head)
                  ;; larger than the system said (/proc, /sys) or than the head: read whole
                  (t (let ((octets (tuatara.io:read-fd-octets fd :path path :size-hint (1+ size))))
                       (and (not (tuatara.io:binary-head-p octets)) octets)))))))))))

(defun corpus-file-at (corpus position)
  "The index in CORPUS of the file holding byte POSITION of its text."
  (let ((starts (file-corpus-starts corpus)))
    (max 0 (1- (tuatara.runtime:bisect (middle 0 (length starts)) (<= (aref starts middle) position))))))

(defvar *search-input-matchers* (cons nil nil)
  "The input last searched for and its matchers, as ((INPUT REGEX) . MATCHERS),
so that a search of many files makes them once, not once a file.")

(defun search-word-matcher (word &optional crlf)
  "A matcher for WORD as a regular expression (MAKE-PATTERN-MATCHER, as a
search makes it), NIL when it is not one (SEARCH-INPUT-PROBLEM says why);
with CRLF, for text whose lines end with a carriage return and a newline."
  (make-pattern-matcher word :regex t :crlf crlf))

(defun search-input-problem (input regex)
  "Why INPUT cannot be searched for, as PATTERN-PROBLEM says it of a word
that is no regular expression, with REGEX; NIL when it can."
  (and regex (some (lambda (word) (pattern-problem word t)) (tuatara.text:split-words input " "))))

(defun search-input-matchers (input regex &optional crlf)
  "The matchers of INPUT, the longest first: with REGEX, one for each of its
words (separated by spaces, SEARCH-WORD-MATCHER), none when a word is no
regular expression; else one for the whole of it as literal text, spaces and
all. With CRLF, for files whose lines end with a
carriage return and a newline. Made once however many pieces and files the
same input is searched in, from any thread."
  (let ((key (list input regex))
        (last *search-input-matchers*))
    (unless (equal (car last) key)
      ;; one cons replaced whole: a thread reads the input and its matchers together
      (setf last (cons key (vector nil nil))
            *search-input-matchers* last))
    (let ((slot (if crlf 1 0)))
      (or (svref (cdr last) slot)
          (setf (svref (cdr last) slot)
                (if regex
                    ;; a word that is no regular expression finds nothing, so neither do they all
                    (let ((matchers (mapcar (lambda (word) (search-word-matcher word crlf))
                                            (stable-sort (tuatara.text:split-words input " ")
                                                         ;; the longest word, likely the rarest, finds the lines to look at
                                                         #'> :key #'length))))
                      (and (notany #'null matchers) matchers))
                    (list (make-pattern-matcher input :regex nil))))))))

(sb-ext:defglobal **corpus-line-endings** (make-hash-table :test 'eq :weakness :key :synchronized t)
  "For each FILE-CORPUS searched, which of its files have lines ending with a
carriage return and a newline (CORPUS-CRLF).")

(defun corpus-crlf (corpus)
  "A bit for each file of CORPUS, 1 when its lines end with a carriage return
and a newline, as its first line end in its first 64 KB tells, as visiting the
file tells it."
  (or (gethash corpus **corpus-line-endings**)
      (setf (gethash corpus **corpus-line-endings**)
            (let* ((rope (file-corpus-rope corpus)) (starts (file-corpus-starts corpus))
                   (size (tuatara.text:rope-bytes rope))
                   (bits (make-array (length starts) :element-type 'bit)))
              (dotimes (i (length starts) bits)
                (let ((start (aref starts i))
                      (end (if (< (1+ i) (length starts)) (aref starts (1+ i)) size)))
                  (when (eq :crlf (tuatara.buffer:detect-line-ending rope :start start :end end))
                    (setf (aref bits i) 1))))))))

(defparameter *search-chunk* (* 1024 1024)
  "The least octets of text each thread searches at once for the lines of a
search (CORPUS-LINES); a text smaller than two of them is searched on one.")

(defun corpus-lines (corpus input &key (regex t) stop (most *search-files-max-lines*) (start 0) end (parallel t) carry)
  "The lines of CORPUS matching INPUT: with REGEX, its words (separated by
spaces) each a regular expression, all found on the line, in any order; else
the whole of it as literal text. A line is matched by the longest word when
any part of a match of it lies on the line, from the line holding the
match's first octet through the line holding its last, a match never running
from one file into the next; each such line is given once, when the other
words are found on it too. A list of (INDEX LINE OFFSET TEXT), the file's
index, the line's number in it (from 1), the offset in the line where the
longest word's first match on it begins (0 for a match coming from the line
before), and the line, in file order, at most MOST
\(*SEARCH-FILES-MAX-LINES*), between the octets START and END of its text
\(all of it by default). A file whose lines end with a carriage return and a
newline is searched as such (CORPUS-CRLF): $ matches before the carriage
return, and the line given leaves it off. The text is cut at line starts
into chunks, at most four for each core, each searched on its own and their
lines joined in order, a chunk into which a match of the one before runs
searched again from where that match ends, so that a search of words found on
many lines takes no longer than one of a rare word and finds what one scan
finds; with PARALLEL false, on this thread alone. The text past END is seen by
a match running on into it, and a third value is where the last such match
ends; CARRY, when given, is where one of the text before START ended, the
lines from START through it matched and the search going on from there. NIL at
once when STOP, asked every few milliseconds on each thread, answers true, and
a second value true then."
  (let* ((rope (file-corpus-rope corpus))
         (crlf (corpus-crlf corpus))
         (starts (file-corpus-starts corpus))
         (size (min (or end (tuatara.text:rope-bytes rope)) (tuatara.text:rope-bytes rope))))
    (labels ((file-end (index)
               (if (< (1+ index) (length starts)) (aref starts (1+ index)) (tuatara.text:rope-bytes rope)))
             (scan (from to carry)
               ;; the lines from FROM below TO, in order, at most MOST, a match before having run on
               ;; to CARRY; T as a second value when stopped; where its last match ends when past TO
               (let* ((lines '()) (count 0) (stopped nil) (marked from) (reach (or carry 0))
                      (halt (and stop (lambda () (when (funcall stop) (setf stopped t))))))
                 (loop with index = (corpus-file-at corpus from)
                       while (and (< from to) (< count most) (not stopped))
                       do (let* ((ending (aref crlf index))
                                 ;; the files from INDEX on whose lines end alike, searched as one
                                 (next (or (loop for k from (1+ index) below (length starts)
                                                 when (/= (aref crlf k) ending) return k)
                                           (length starts)))
                                 (stretch-end (if (< next (length starts)) (min to (aref starts next)) to))
                                 (matchers (search-input-matchers input regex (= ending 1))))
                            (when matchers
                              (setf marked
                                    (scan-stretch from stretch-end (and carry (> carry from) carry) marked
                                                  (first matchers) (rest matchers) (= ending 1)
                                                  (lambda (line) (push line lines) (incf count) (>= count most))
                                                  (lambda (e) (setf reach (max reach e)))
                                                  halt)))
                            (setf from stretch-end index next carry nil)))
                 (values (nreverse lines) stopped (and (> reach to) reach))))
             (scan-stretch (from to carry marked first others crlf give note-end halt)
               ;; GIVE each line from FROM below TO that FIRST finds and OTHERS find on too, those
               ;; before MARKED given already, a match before having run on to CARRY: where the
               ;; next line to give may start
               (let ((done nil) (s 0) (file -1) (file-limit 0))
                 (declare (type fixnum s file file-limit))
                 (labels ((take (line-start text-end after)
                            (declare (ignore after))
                            ;; one line for each line matched, when the other words are on it too
                            (when (and (not done)
                                       (every (lambda (matcher)
                                                (map-matcher-matches (lambda (&rest match) (declare (ignore match)) t)
                                                                     rope matcher line-start text-end))
                                              others))
                              (let ((line (1+ (- (tuatara.text:byte->line rope line-start)
                                                 (tuatara.text:byte->line rope (aref starts file))))))
                                (setf done (funcall give (list file line (max 0 (- s line-start))
                                                               (tuatara.text:rope-string rope :start line-start :end text-end)))))))
                          (unit-of (at from) (line-holding rope at from crlf))
                          (found (start e)
                            (setf s start)
                            (when (>= start file-limit)
                              (setf file (corpus-file-at corpus start) file-limit (file-end file)))
                            (funcall note-end e)
                            (setf marked (map-match-units #'take #'unit-of start e marked to))
                            done)
                          (scan-from (at to bound crossing)
                            ;; the matches from AT below TO seeing no text past BOUND; with
                            ;; CROSSING, one running past its file's end stops it, and is its start
                            (let ((crossed nil))
                              (map-matcher-matches
                               (lambda (start e m g)
                                 (declare (ignore m g))
                                 (if (and crossing (> e (if (>= start file-limit) (file-end (corpus-file-at corpus start)) file-limit)))
                                     (setf crossed start)
                                     (found start e)))
                               rope first at to :bound bound :texts nil :halt halt)
                              crossed)))
                   (when carry
                     (setf file (corpus-file-at corpus from) file-limit (file-end file))
                     (found from carry)
                     (setf from carry))
                   (let ((bound (if (< from to) (file-end (corpus-file-at corpus (1- to))) to)))
                     (loop while (and (< from to) (not done))
                           do (let ((crossed (scan-from from to bound t)))
                                (if crossed
                                    ;; a match may not run into the next file: that file's rest searched alone
                                    (let ((end (file-end (corpus-file-at corpus crossed))))
                                      (scan-from crossed (min end to) end nil)
                                      (setf from end))
                                    (return)))))
                   marked))))
      (when (and stop (funcall stop))
        (return-from corpus-lines (values nil t)))
      (let* ((length (- size start))
             (chunks (if (or (not parallel) (< length (* 2 *search-chunk*)))
                         1
                         (min (* 4 (tuatara.runtime:cpu-count)) (ceiling length *search-chunk*))))
             (bounds (let ((at start))
                       (append (loop for k from 1 below chunks
                                     collect (setf at (max at (tuatara.text:rope-line-start rope (+ start (floor (* k length) chunks))))))
                               (list size))))
             (froms (cons start (butlast bounds)))
             (results (tuatara.runtime:run-tasks
                       (coerce (loop for from in froms
                                     for to in bounds
                                     for first = t then nil
                                     collect (let ((from from) (to to) (carry (and first carry)))
                                               (lambda () (multiple-value-list (scan from to carry)))))
                               'simple-vector))))
        ;; a chunk a match of the one before runs into searched again from where that match ends
        (loop for k from 1 below (length results)
              for carry = (third (svref results (1- k)))
              for from = (nth k froms)
              when (and carry (> carry from) (not (second (svref results (1- k)))))
                do (setf (svref results k) (multiple-value-list (scan from (nth k bounds) carry))))
        (if (some #'second results)
            (values nil t)
            (let ((all (loop for (lines) across results append lines)))
              (values (subseq all 0 (min most (length all))) nil (third (svref results (1- (length results)))))))))))

(defparameter *search-disk-piece* (* 4 1024 1024)
  "How many octets of a file too large to be kept in memory for a search are
searched at a time (LINES-ON-DISK); as many again are read after it, or as
far as a match may reach (TUATARA.SYNTAX:*REGEX-LONGEST-MATCH*) when that is
more, so that a match running on past a piece's end is seen as one scan of
the whole file sees it.")

(defparameter *search-disk-longest-line* (* 64 1024 1024)
  "The longest line of a file searched from disk that is searched whole, and
how much further than the octets read ahead a match is followed
\(LINES-ON-DISK): what is read at a time grows by up to this to hold such a
line or such a match; a longer line is searched in pieces of this size, a
match across their edge missed.")

(defun lines-on-disk (path relative root input &key (regex t) cancel (most *search-files-max-lines*) (inside "") (parallel t))
  "Completion candidates for the lines of the file at PATH matching INPUT (a
regular expression's words with REGEX, else literal text: CORPUS-LINES), as
LINE-CANDIDATES makes them (RELATIVE its path from ROOT, a directory's path
INSIDE left off), the file read from disk and searched a piece of
*SEARCH-DISK-PIECE* octets at a time, each ending at a line end and let go of
before the next, so that a gigantic file is never held in memory whole. The
octets after a piece are read with it, a match running on into them seen
whole and the next piece searched on from where it ends, so that the lines
are those one scan of the whole file finds (what is read grows for a longer
line or match, up to *SEARCH-DISK-LONGEST-LINE* more). Nothing for a file that is
no text (READ-TEXT-FILE). At most MOST; CANCEL, a cons, stops it when its
car turns true."
  (let ((found '()) (count 0))
    (when (and (uiop:string-prefix-p inside relative)
               (multiple-value-bind (octets why) (read-text-file path :most 0)
                 (or octets (eq why :too-large))))
      (let ((fd (ignore-errors (tuatara.io:open-for-reading path t))))
        (when fd
          (unwind-protect
               (let* ((size (let ((stamp (ignore-errors (tuatara.io:fd-stamp fd path))))
                              (if stamp (tuatara.io:stamp-size stamp) *search-disk-piece*)))
                      (ahead (max *search-disk-piece* tuatara.syntax:*regex-longest-match*))
                      (limit (+ *search-disk-longest-line* ahead))
                      ;; no larger than the file, so that a small one costs little
                      (buffer (make-array (max 1 (min (+ *search-disk-piece* ahead) size)) :element-type '(unsigned-byte 8)))
                      (offset 0) (lines-before 0) (carry nil))
                 (loop
                   (when (or (car cancel) (>= count most)) (return))
                   (let ((got (or (ignore-errors (tuatara.io:read-fd-at fd buffer offset :path path)) 0)))
                     (when (zerop got) (return))
                     (let* ((whole (or (< got (length buffer)) (>= (+ offset got) size)))
                            ;; the piece: up to its last line end before what is read ahead
                            (newline (and (not whole)
                                          (or (tuatara.text:octets-line-end-position-backward
                                               buffer 0 (max 0 (min got (- (length buffer) ahead))) got)
                                              (tuatara.text:octets-line-end-position-backward buffer 0 got nil))))
                            (growing (< (length buffer) limit)))
                       (if (and (not whole) (null newline) growing)
                           (setf buffer (grown-octets buffer limit))
                           (let ((end (if newline (1+ newline) got))
                                 (corpus (make-file-corpus (tuatara.text:octets->rope (coerce (subseq buffer 0 got) 'tuatara.text:octets))
                                                           (vector relative) (vector 0) root)))
                             (multiple-value-bind (lines stopped reach)
                                 (corpus-lines corpus input :regex regex :most (- most count) :stop (lambda () (car cancel))
                                                            :parallel parallel :end end :carry carry)
                               (declare (ignore stopped))
                               (if (and reach (>= reach got) (not whole) growing)
                                   ;; a match running to the end of what was read: read more and search again
                                   (setf buffer (grown-octets buffer limit))
                                   (progn
                                     (dolist (candidate (line-candidates corpus
                                                                         (mapcar (lambda (line) (list* (first line) (+ lines-before (second line)) (cddr line)))
                                                                                 lines)
                                                                         inside))
                                       (push candidate found)
                                       (incf count))
                                     (incf lines-before (tuatara.text:octets-line-ends buffer 0 end got))
                                     (incf offset end)
                                     (setf carry (and reach (> reach end) (- reach end))))))))))))
            (ignore-errors (sb-posix:close fd))))))
    (nreverse found)))

(defun grown-octets (octets limit)
  "An empty vector of octets twice as long as OCTETS, but no longer than LIMIT."
  (make-array (min limit (* 2 (length octets))) :element-type '(unsigned-byte 8)))

(defun lines-on-disk-many (files root input &key (regex t) cancel (most *search-files-max-lines*) (inside ""))
  "LINES-ON-DISK for each of FILES, each (RELATIVE PATH), INPUT and REGEX as
it takes them, searched a file to a core at once, a batch after another: their
candidates, in the order of FILES, at most MOST; CANCEL, a cons, stops it when
its car turns true."
  (let ((found '()) (count 0)
        (batch (* 2 (tuatara.runtime:cpu-count))))
    (loop for rest on files by (lambda (list) (nthcdr batch list))
          while (and (< count most) (not (car cancel)))
          do (let* ((some (subseq rest 0 (min batch (length rest))))
                    (results (tuatara.runtime:run-tasks
                              (coerce (mapcar (lambda (file)
                                                (destructuring-bind (relative path) file
                                                  (lambda () (lines-on-disk path relative root input :regex regex :cancel cancel :most most
                                                                                                     :inside inside :parallel nil))))
                                              some)
                                      'simple-vector))))
               (loop for candidates across results
                     do (dolist (candidate candidates)
                          (when (< count most)
                            (push candidate found)
                            (incf count))))))
    (nreverse found)))

(defun line-candidates (corpus lines &optional (inside ""))
  "Completion candidates for LINES of CORPUS (CORPUS-LINES): path:line: text,
each standing for (PATH LINE OFFSET); with INSIDE, a directory's path from
the corpus's root, only those of files in it, their paths from it."
  (loop for (index line offset text) in lines
        for path = (aref (file-corpus-paths corpus) index)
        when (uiop:string-prefix-p inside path)
          collect (let ((shown (string-left-trim '(#\Space #\Tab) text)))
                    (make-candidate (format nil "~a:~d: ~a" (subseq path (length inside)) line
                                            (if (> (length shown) 200) (subseq shown 0 200) shown))
                                    :value (list (concatenate 'string (file-corpus-root corpus) path) line offset)))))

(defparameter *search-files-piece* (* 4 1024 1024)
  "About how many octets of text each piece of the files read for a search
holds: each is searched as soon as it is read.")

(defstruct (corpus-gatherer (:constructor make-corpus-gatherer (limit piece give-piece give-large)) (:copier nil))
  "How the text of files is taken in for searching, in pieces, by the search
of many files and by the directories kept read alike: LIMIT, the most octets
kept in all; PIECE, about how many each piece holds; GIVE-PIECE, a function
given each piece as it is full, its TEXTS, each (RELATIVE OCTETS STAMP), in
order, and its SIZE, their octets with a newline after each; GIVE-LARGE, one
given the RELATIVE and PATH of each text file too large for what is left of
LIMIT, to be searched from disk (LINES-ON-DISK). TOTAL, the octets kept so
far; TEXTS and SIZE, the piece being filled."
  limit piece give-piece give-large (total 0) (texts '()) (size 0))

(setf (documentation 'make-corpus-gatherer 'function) "Return a CORPUS-GATHERER."
      (documentation 'corpus-gatherer-p 'function) "True when OBJECT is a CORPUS-GATHERER.")

(defun gather-piece (gatherer)
  "Give the piece GATHERER is filling, when it holds any text, and begin another."
  (when (corpus-gatherer-texts gatherer)
    (funcall (corpus-gatherer-give-piece gatherer) (nreverse (corpus-gatherer-texts gatherer)) (corpus-gatherer-size gatherer))
    (setf (corpus-gatherer-texts gatherer) '()
          (corpus-gatherer-size gatherer) 0)))

(defun gather-text-file (gatherer relative path &key stamp (octets nil given))
  "Take in the file at PATH, RELATIVE its path from the root, with GATHERER:
its text, OCTETS when GIVEN, else read (READ-TEXT-FILE) unless STAMP says it
is too large for what is left of the limit; one too large given as such, to
be searched from disk. :TEXT when its text was taken, :LARGE when it was too
large, NIL when it is not text."
  (let ((limit (corpus-gatherer-limit gatherer))
        (total (corpus-gatherer-total gatherer)))
    (flet ((large ()
             (funcall (corpus-gatherer-give-large gatherer) relative path)
             :large))
      (if (and (not given) stamp (> (+ total (tuatara.io:stamp-size stamp) 1) limit))
          (large)
          (multiple-value-bind (octets why) (if given octets (read-text-file path :most (- limit total 1)))
            (cond ((eq why :too-large) (large))
                  ((null octets) nil)
                  ((> (+ total (length octets) 1) limit) (large))
                  (t (push (list relative octets stamp) (corpus-gatherer-texts gatherer))
                     (incf (corpus-gatherer-size gatherer) (1+ (length octets)))
                     (incf (corpus-gatherer-total gatherer) (1+ (length octets)))
                     (when (>= (corpus-gatherer-size gatherer) (corpus-gatherer-piece gatherer))
                       (gather-piece gatherer))
                     :text)))))))

(defun gather-file-texts (root)
  "Start reading the text files under the directory ROOT (FILES-UNDER, what
.gitignore files leave out left out) on a thread of their own, up to
*SEARCH-FILES-LIMIT* octets, in pieces of about *SEARCH-FILES-PIECE*, each a
FILE-CORPUS given as it is read (GATHER-TEXT-FILE); the GATHERING. A text
file too large for what is left of the limit is given as (:LARGE RELATIVE
PATH), to be searched from disk (LINES-ON-DISK); when the walk left files
out, the last thing given is :LEFT-OUT."
  (let ((limit *search-files-limit*)
        (piece *search-files-piece*)
        (root (directory-name root)))
    (start-gathering
     "tuatara reading files to search"
     (lambda (give cancel)
       (let ((gatherer (make-corpus-gatherer limit piece
                                             (lambda (texts size)
                                               (funcall give (make-corpus (mapcar (lambda (text) (cons (first text) (second text))) texts)
                                                                          size root)))
                                             (lambda (relative path) (funcall give (list :large relative path))))))
         (multiple-value-bind (files more)
             (files-under root :gitignore t :cancel cancel
                               :each (lambda (path relative)
                                       (unless (car cancel)
                                         (gather-text-file gatherer relative path))))
           (declare (ignore files))
           (gather-piece gatherer)
           (when (and more (not (car cancel)))
             (funcall give :left-out))))))))

(defun make-corpus (texts size root)
  "A FILE-CORPUS of TEXTS, each (RELATIVE . OCTETS), SIZE octets with a
newline after each, their paths from ROOT."
  (let ((all (make-array size :element-type '(unsigned-byte 8)))
        (paths (make-array (length texts)))
        (starts (make-array (length texts)))
        (at 0))
    (loop for (path . octets) in texts
          for i from 0
          do (setf (aref paths i) path (aref starts i) at)
             (replace all octets :start1 at)
             (incf at (length octets))
             (setf (aref all at) 10)
             (incf at))
    (make-file-corpus (tuatara.text:octets->rope all) paths starts root)))

(defun corpus-range (corpus inside)
  "The octets of CORPUS's text holding the files under INSIDE, a directory's
path from its root (\"\" for all), as two values, START and END; NIL when it
holds none. The files of a directory lie together, as they were read."
  (let* ((paths (file-corpus-paths corpus))
         (starts (file-corpus-starts corpus))
         (first (position-if (lambda (path) (uiop:string-prefix-p inside path)) paths)))
    (when first
      (let ((last (or (position-if (lambda (path) (uiop:string-prefix-p inside path)) paths :from-end t) first)))
        (values (aref starts first)
                (if (< (1+ last) (length starts))
                    (aref starts (1+ last))
                    (tuatara.text:rope-bytes (file-corpus-rope corpus))))))))

(defvar *index-searches* (list 0)
  "A cons whose car counts the searches over a directory kept read
\(*INDEXED-DIRECTORIES*) under way now, for the mode line's sign of them.")

(defparameter *sign-lit-seconds* 1/2
  "The least time a sign of the mode line stays lit once its work begins or
ends, so that work done in a moment is seen too.")

(defvar *signs-lit-until* (make-hash-table :test 'eq :synchronized t)
  "For each sign of the mode line, the internal real time it stays lit until.")

(sb-ext:defglobal **sign-fader** (list nil)
  "True in its car while a thread waits to wake the frontend once the signs
lit last are to be drawn faint (NOTE-SIGN-WORK): one at most, however many
signs are lit meanwhile.")

(defun note-sign-work (sign &optional editor)
  "Note that the work SIGN (a keyword: :indexing, :searching) shows began or
ended now, from any thread, so that its sign is lit for *SIGN-LIT-SECONDS*
however quick the work; wake EDITOR, when given, to show it lit now and
again once the moment is over, to show it faint, a question in the minibuffer
open or not. One thread at most waits for that moment, for every sign lit."
  (setf (gethash sign *signs-lit-until*) (tuatara.runtime:deadline-in *sign-lit-seconds*))
  (when editor
    (wake-editor editor)
    (when (null (sb-ext:compare-and-swap (car **sign-fader**) nil t))
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (flet ((left ()
                  (let ((latest 0))
                    (maphash (lambda (sign until) (declare (ignore sign)) (setf latest (max latest until)))
                             *signs-lit-until*)
                    (tuatara.runtime:seconds-left latest))))
           ;; until the last sign lit is due to fade, however often they were lit again
           (loop
             (let ((left (left)))
               (when (plusp left) (sleep left)))
             (wake-editor editor)
             (setf (car **sign-fader**) nil)
             ;; one lit as this one let go waits on here, unless another thread took it
             (unless (and (plusp (left)) (null (sb-ext:compare-and-swap (car **sign-fader**) nil t)))
               (return)))))
       :name "tuatara sign fading"))))

(defstruct (found-lines (:constructor make-found-lines (lines index words)) (:copier nil))
  "The lines a search of files found, as a list of places (STEP-PLACE): LINES,
a vector of (PATH LINE OFFSET), in the order listed, INDEX, the one gone to
last, and WORDS, what was searched for."
  (lines #() :type simple-vector :read-only t)
  (index 0 :type fixnum)
  (words "" :type string :read-only t))

(setf (documentation 'found-lines-p 'function) "True when OBJECT is a FOUND-LINES."
      (documentation 'found-lines-lines 'function) "The lines a search of files found, each (PATH LINE OFFSET)."
      (documentation 'found-lines-index 'function) "The line of a FOUND-LINES gone to last."
      (documentation 'found-lines-words 'function) "What the search of files that found a FOUND-LINES' lines searched for.")

(defmethod places-name ((places found-lines))
  "The search for what was searched for, shortened to a few words: the search for kiwi."
  (format nil "the search for ~a"
          (tuatara.display:shorten-string (escape-controls (found-lines-words places)) 40 :one-line t)))

(defmethod step-place ((places found-lines) direction)
  "Visit the file of the line found after the one gone to last (DIRECTION 1),
or before it (-1), at that line, as choosing it in the search does."
  (let* ((lines (found-lines-lines places))
         (next (+ (found-lines-index places) direction)))
    (if (< -1 next (length lines))
        (destructuring-bind (path line offset) (svref lines next)
          (setf (found-lines-index places) next)
          (show-file-line path line :offset offset)
          (message "Line ~:d of ~:d found" (1+ next) (length lines)))
        (say-places-end places direction))))

(defun search-files-as-read (root prompt &key (left-out-words "Searching the first ~:d files only") preview)
  "Ask with PROMPT for words to find in the files under ROOT, read in the
background meanwhile (GATHER-FILE-TEXTS), or kept read already
\(*INDEXED-DIRECTORIES*): the lines that have them listed as they are typed,
and as more files are read; visit the chosen line's file there. Each input is
searched on a thread of its own, the lines coming in as they are found, a new
key stopping it and starting the next, so that typing never waits; what is
typed is a regular expression's words, \\<pattern>\\[search-toggle-regex] switching to
literal text and back, PROMPT shown after the kind (Regex search files in
~/x/: , Literal search files in ~/x/: , PATTERN-KIND-WORDS); in a
directory under one kept read, only its files' text is searched, unless the
limits cut that one's text short, when the directory is read from disk. Choosing,
or leaving the question, stops the reading and the search. LEFT-OUT-WORDS
says, with how many files were read, that the limit left some out. With
PREVIEW, the file of the line selected is shown meanwhile, at that line,
opened safely (CALL-WITH-WINDOW-PREVIEW)."
  (declare (notinline directory-index-left-out directory-index-corpora directory-index-large directory-index-root))
  (multiple-value-bind (index inside) (index-for root)
    ;; a directory under one kept read whose text was cut short by the limits may
    ;; hold files left out: read from disk, as with no index
    (when (and index (directory-index-left-out index) (plusp (length inside)))
      (setf index nil inside ""))
    (let* ((reading (and (null index) (gather-file-texts root)))
           (inside (or inside ""))
           ;; the pieces of text read so far, kept read (*INDEXED-DIRECTORIES*): all there at once;
           ;; the searching thread takes them under the lock as they come
           (lock (sb-thread:make-mutex :name "search pieces"))
           (corpora (if index (coerce (directory-index-corpora index) 'list) '()))
           ;; the text files too large to keep, each (RELATIVE PATH), searched from disk
           (large (if index (directory-index-large index) '()))
           (root-name (if index (directory-index-root index) (directory-name root)))
           (read-all (null reading))
           (editor *editor*)
           (left-out nil)
           ;; the search of the input typed last, and the lines it found
           (searching nil) (searched-input nil) (found '())
           ;; whether what is typed is a regular expression's words, else literal text;
           ;; TOGGLED once it is switched, till the lines are worked out again
           (regex t) (toggled nil)
           ;; true until the search of the input typed last has given lines: the lines of
           ;; the input before stay shown meanwhile, so that the list and the preview do
           ;; not empty and fill again at every key
           (fresh nil)
           ;; the input and kind last looked at, and why it cannot be searched for: worked out once
           (checked (list nil)))
      (labels ((files-read () (loop for corpus in corpora sum (length (file-corpus-paths corpus))))
               (problem (input)
                 (let ((key (cons input regex)))
                   (if (equal (car checked) key)
                       (cdr checked)
                       (cdr (setf checked (cons key (search-input-problem input regex)))))))
               (piece (i)
                 ;; the I-th piece once read, :WAIT while more may come, NIL when there are no more
                 (sb-thread:with-mutex (lock)
                   (cond ((< i (length corpora)) (nth i corpora))
                         (read-all nil)
                         (t :wait))))
               (start-search (input)
                 (when searching (stop-gathering searching))
                 (setf searched-input (list input regex) fresh t
                       searching
                       (start-gathering
                        "tuatara searching files"
                        (lambda (give cancel)
                          (when index
                            (sb-ext:atomic-incf (car *index-searches*))
                            (note-sign-work :searching editor))
                          (unwind-protect
                              (let ((count 0))
                                (loop for i from 0
                                      for corpus = (loop for p = (piece i)
                                                         while (and (eq p :wait) (not (car cancel)))
                                                         do (sleep 0.02)
                                                         finally (return p))
                                      while (and corpus (not (eq corpus :wait)) (not (car cancel))
                                                 (< count *search-files-max-lines*))
                                      do (multiple-value-bind (from to) (corpus-range corpus inside)
                                           (when from
                                             (let ((lines (corpus-lines corpus input :regex regex :start from :end to
                                                                                     :most (- *search-files-max-lines* count)
                                                                                     :stop (lambda () (car cancel)))))
                                               (incf count (length lines))
                                               (dolist (candidate (line-candidates corpus lines inside))
                                                 (funcall give candidate))))))
                                ;; then the files too large to keep, from disk, once all is read
                                (when (< count *search-files-max-lines*)
                                  (dolist (candidate (lines-on-disk-many (sb-thread:with-mutex (lock) (copy-list large))
                                                                         root-name input :regex regex :cancel cancel
                                                                         :most (- *search-files-max-lines* count) :inside inside))
                                    (funcall give candidate))))
                            (when index
                              (sb-ext:atomic-decf (car *index-searches*))
                              (note-sign-work :searching editor)))))))
               (take ()
                 (let ((changed (shiftf toggled nil)) (working nil))
                   (when reading
                     (multiple-value-bind (new done) (take-gathered reading)
                       (sb-thread:with-mutex (lock)
                         (dolist (thing new)
                           (if (eq thing :left-out)
                               (setf left-out t)
                               (if (and (consp thing) (eq (first thing) :large))
                                   (setf large (append large (list (rest thing))))
                                   (setf corpora (append corpora (list thing))))))
                         (when done (setf read-all t)))
                       (when (and done left-out)
                         (setf left-out nil)
                         (message left-out-words (files-read)))
                       (unless done (setf working t))))
                   (when searching
                     (multiple-value-bind (new done) (take-gathered searching)
                       (cond (new
                              (setf found (if fresh new (append found new)) fresh nil changed t))
                             ;; the search ended with nothing: the lines before go too
                             ((and done fresh)
                              (setf found '() fresh nil changed t)))
                       (unless done (setf working t))))
                   ;; while files are read: how many so far
                   (values changed working (and (not read-all) (files-read)))))
               (lines (input)
                 (cond ((or (< (length (string-trim " " input)) *search-files-min-input*)
                           (problem input))
                        (when searching (stop-gathering searching) (setf searching nil))
                        (setf searched-input nil found '() fresh nil)
                        '())
                       (t
                        (unless (equal (list input regex) searched-input)
                          (start-search input))
                        found))))
        (unwind-protect
             (let ((chosen (let ((*toggle-pattern-kind* (lambda () (setf regex (not regex) toggled t))))
                             (call-with-window-preview
                            (lambda (candidate made)
                              (destructuring-bind (path line offset) (candidate-value candidate)
                                (let ((buffer (preview-file-buffer path made)))
                                  (when buffer
                                    (values buffer (+ (tuatara.buffer:buffer-goto-line buffer (1- line)) offset))))))
                            (lambda (show)
                              (completing-read (lambda ()
                                                 (pattern-prompt regex prompt
                                                                 (problem (editor-minibuffer-text *editor*))))
                                               #'lines
                                               :separator (code-char 0) :filter nil :require-match t
                                               :source #'take :keymap *pattern-keymap*
                                               :preview (and preview show)))))))
               (when (consp chosen)
                 ;; the lines found, for M-g n and M-g p to go on from the one chosen
                 (let ((lines (map 'vector #'candidate-value found)))
                   (use-places (make-found-lines lines (or (position chosen lines :test #'eq) 0) (or (first searched-input) ""))))
                 (destructuring-bind (path line offset) chosen
                   (show-file-line path line :offset offset))))
          (when reading (stop-gathering reading))
          (when searching (stop-gathering searching)))))))

(defvar *project-root-markers* '(".git" ".project")
  "The names of the files or directories that mark the root of a project:
\\[search-in-files] searches all of the project a buffer's file is in, from the
nearest directory above the file holding one of them. Your home directory, and
those above it, are never a project's root.")

(defun project-root (directory)
  "The root of the project DIRECTORY is in: the nearest directory from it up
holding one of *PROJECT-ROOT-MARKERS* (MARKED-DIRECTORY, as REPOSITORY-TOP
finds a repository), below the home directory (by any of its names); NIL when
there is none."
  (marked-directory directory (setting-list '*project-root-markers* *project-root-markers*) :below-home t))

(define-command search-in-files ()
  "Find text in the files of the project the current buffer's file is in (see
*PROJECT-ROOT-MARKERS*), or under its directory when it is in none, or under
the directory it lists (\\[rimu]), or *DEFAULT-DIRECTORY* for another buffer
with no file; with a prefix argument, in a directory asked for. As you
type, every line with all the words typed (each a regular expression, in any
order, ignoring case unless it has a capital) is listed as path:line: text,
after three characters, the files read in the background as you type and
searched as they are read; \\<pattern>\\[search-toggle-regex] switches to literal text, what
is typed found as it stands, spaces and all, and back.
\\<minibuffer>\\[minibuffer-done] visits the file at that line (the first, once all are
read, unless you moved to one), and then \\[next-place] and \\[previous-place] go to the
lines listed after it and before it; with *preview-file-search*, the file of the line
selected is shown meanwhile, at that line, opened safely. Hidden files and
directories, backups, binary files and what .gitignore files exclude are left
out; the access times of those read are not changed."
  (let* ((here (buffer-directory (current-buffer)))
         (root (if (prefix-argument)
                   (read-directory-path "Search files in: " :initial (abbreviated-path here))
                   (or (project-root here) here))))
    (search-files-as-read root (format nil "search files in ~a" (abbreviated-path (directory-name root)))
                          :left-out-words (format nil "Searched the first ~~:d files only (~:d MB)"
                                                  (floor *search-files-limit* (* 1024 1024)))
                          :preview *preview-file-search*)))

(define-command search-in-formatted ()
  "Find text in *FORMATTED-FILE-DIRECTORY*, where your files with formatted
names are: every file under it, as search-in-files finds text in a directory;
the lines with the words typed are listed as you type, the files read in the
background meanwhile, \\<pattern>\\[search-toggle-regex] switching to literal text and back,
and \\<minibuffer>\\[minibuffer-done] visits one. With
*preview-formatted-search*, the file of the line selected is shown meanwhile,
at that line, opened safely."
  (search-files-as-read *formatted-file-directory* "search formatted files"
                        :left-out-words "Searched the first ~:d files only"
                        :preview *preview-formatted-search*))
