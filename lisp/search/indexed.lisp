;;;; lisp/search/indexed.lisp --- Directories kept read in memory, so that searching and choosing files there answer at once.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The directories of *INDEXED-DIRECTORIES* are read on a thread of their own
;;;; when the editor starts: the names of their files with their times, and the
;;;; text of those a search reads, in pieces as search-in-files reads them. The
;;;; watcher watches each of their directories, and when something in one
;;;; changes, that directory alone is listed again in the background, only the
;;;; files whose stamps changed are read again, and only the pieces of text
;;;; holding them are made anew, the rest kept as they were; the new index is
;;;; put in place of the old at once. What is read is kept on disk too, in the
;;;; cache directory, so that the next start reads only what changed meanwhile,
;;;; and the text kept there is mapped from there rather than held in the heap,
;;;; where every full collection of garbage would copy it.
;;;; Choosing a file with a formatted name (C-x n o), searching your files
;;;; (C-x n g, M-s g) and the parts of Org that list your files take what is
;;;; kept there, instead of reading the directory while you wait.

(in-package #:tuatara.defaults)

(defvar *indexed-directories* '()
  "Directories Tuatara keeps read in memory, so that \\[open-file-formatted],
\\[search-in-formatted] and \\[search-in-files] answer at once there, and in
the directories under them: the names of their files and the text of those a
search reads (at most *SEARCH-FILES-LIMIT* octets each), read in the
background when the editor starts and again whenever something in them
changes on disk. NIL, as standard, for none: the files are read when you ask,
listed as they are read. (list *formatted-file-directory*) for your files
with formatted names; any directories, such as '(\"~/Documents/\"
\"~/notes/\"). Each takes memory as large as the text of its files.")

(defvar *indexing-indicator* "U"
  "What the mode line shows while directories are kept read
\(*INDEXED-DIRECTORIES*), always in the same place, before [-], after the
sign of work for the buffer (*PROCESSING-INDICATOR*): in :mode-line-sign
while one is being read in the background (at least *SIGN-LIT-SECONDS*),
else faint, in :mode-line-sign-idle, so that nothing after it moves as
reading starts and ends. U (for update) as standard, which any
font has; any text, such as ↻, or \"\" (nf-oct-sync, U+F46A) or
\"\" (nf-fa-refresh, U+F021) for a Nerd Font, \"indexing\" in words; NIL to
show nothing.")

(defvar *searching-indicator* "S"
  "What the mode line shows after *INDEXING-INDICATOR* while directories are
kept read, always in the same place: in :mode-line-sign while a search runs
over a directory kept read (\\[search-in-files] and the like; at least
*SIGN-LIT-SECONDS*), else faint, in :mode-line-sign-idle. S (for search) as
standard, which any font has; any text, such as ⌕, or for a Nerd Font a
looking glass such as \"\" (nf-fa-search, U+F002) or \"󰍉\"
(nf-md-magnify, U+F0349); \"searching\" in words; NIL to show nothing.")


(defstruct (directory-index (:constructor make-directory-index (root)) (:copier nil))
  "What is kept of a directory of *INDEXED-DIRECTORIES*: ROOT, its native name
ending in /; FILES, every file under it as (RELATIVE . STAMP), the ones
changed last first; CORPORA, the text of those a search reads, as FILE-CORPUS
pieces; LEFT-OUT, true when the walk left some out; LARGE, the text files too
large to keep within the limit, each (RELATIVE PATH), which a search reads
from disk (LINES-ON-DISK); DIRECTORIES, those under
it, watched on disk; READY once it has been read; READING while a thread
reads it, AGAIN when it is to be read once more after, the directories
reported changed meanwhile in PENDING (:ALL for all); RESULT, what the thread
read, until taken in; TREE, each directory's entries as last listed
\(INDEX-DIRECTORY, by native name), which the walks of other threads look at
too (DIRECTORY-LISTING). What only the reading thread uses:
PIECES, the text's pieces with the stamps of their files (INDEX-PIECE); and
NOT-TEXT, the files found not to be text, with their stamps."
  root (files '()) (corpora #()) (left-out nil) (large '()) (directories '())
  (ready nil) (reading nil) (again nil) (pending '()) (result nil)
  (tree nil) (pieces #()) (not-text (make-hash-table :test 'equal)))

(setf (documentation 'directory-index-p 'function) "True when OBJECT is a DIRECTORY-INDEX."
      (documentation 'directory-index-root 'function) "The directory a DIRECTORY-INDEX keeps, a native name ending in /."
      (documentation 'directory-index-files 'function) "The files under a DIRECTORY-INDEX's root, each (RELATIVE . STAMP), the ones changed last first."
      (documentation 'directory-index-corpora 'function) "The text a search reads under a DIRECTORY-INDEX's root, in pieces."
      (documentation 'directory-index-left-out 'function) "True when a DIRECTORY-INDEX's walk left files out."
      (documentation 'directory-index-large 'function) "The text files of a DIRECTORY-INDEX too large to keep, searched from disk.")
(setf
      (documentation 'directory-index-directories 'function) "The directories under a DIRECTORY-INDEX's root, watched on disk."
      (documentation 'directory-index-ready 'function) "True once a DIRECTORY-INDEX has been read."
      (documentation 'directory-index-reading 'function) "True while a thread reads a DIRECTORY-INDEX."
      (documentation 'directory-index-again 'function) "True when a DIRECTORY-INDEX is to be read once more after the reading under way."
      (documentation 'directory-index-pending 'function) "The directories of a DIRECTORY-INDEX reported changed while it was read, or :ALL."
      (documentation 'directory-index-result 'function) "What a DIRECTORY-INDEX's thread read, until it is taken in."
      (documentation 'directory-index-tree 'function) "Each directory under a DIRECTORY-INDEX's root as last listed, by native name."
      (documentation 'directory-index-pieces 'function) "A DIRECTORY-INDEX's text pieces with their files' stamps."
      (documentation 'directory-index-not-text 'function) "The files of a DIRECTORY-INDEX found not to be text, with their stamps.")

(defstruct (index-directory (:constructor make-index-directory (path entries rules)) (:copier nil))
  "A directory as an index last listed it: PATH, its native name ending in /;
ENTRIES, its files and directories in the order listed, each (NAME KIND
STAMP), KIND :REGULAR or :DIRECTORY, hidden names, backups and links to
directories left out; RULES, those of its own .gitignore (GITIGNORE-RULES)."
  path entries rules)

(setf (documentation 'index-directory-p 'function) "True when OBJECT is an INDEX-DIRECTORY.")

(defstruct (index-piece (:constructor make-index-piece (corpus stamps &optional file)) (:copier nil))
  "A piece of an index's text: CORPUS, its FILE-CORPUS; STAMPS, a vector of the
stamps of its files, in the order of the corpus's paths; FILE, the name of the
file it is kept in on disk (*INDEXES-KEPT-ON-DISK*), once written."
  corpus stamps file)

(setf (documentation 'index-piece-p 'function) "True when OBJECT is an INDEX-PIECE.")

(defvar *indexes-kept-on-disk* t
  "True to keep what is read of *INDEXED-DIRECTORIES* on disk between sessions,
in the cache directory ($XDG_CACHE_HOME/tuatara/indexes/, readable by you
alone): at the next start the directories are only listed again, and only the
files changed meanwhile read, rather than all. Only the text of the files a
search reads is kept there (never an encrypted file's). NIL to keep it in
memory only, read whole at each start: for directories whose text should not
be copied anywhere, such as evidence.")

(defvar *directory-indexes* '()
  "The directories kept read, a DIRECTORY-INDEX each, as *INDEXED-DIRECTORIES* asks.")

(defun same-stamp-p (a b)
  "True when the stamps A and B say a file is as it was: its change time, size
and inode alike."
  (and a b
       (= (tuatara.io:stamp-mtime a) (tuatara.io:stamp-mtime b))
       (= (tuatara.io:stamp-size a) (tuatara.io:stamp-size b))
       (= (tuatara.io:stamp-ino a) (tuatara.io:stamp-ino b))))

(defun compact-name (string)
  "STRING, a file's name or path, as a string of one octet a character when
every character of it is ASCII, as most names are, else as it is: an index
keeps hundreds of thousands of them, and a string of any characters takes
four octets for each."
  (if (and (not (typep string 'simple-base-string)) (every (lambda (c) (< (char-code c) 128)) string))
      (coerce string 'simple-base-string)
      string))

(defun compact-join (&rest parts)
  "PARTS, names or paths, joined as COMPACT-NAME keeps one: made at once at
one octet a character when every part is such a string already."
  (if (every (lambda (part) (typep part 'simple-base-string)) parts)
      (apply #'concatenate 'simple-base-string parts)
      (compact-name (apply #'concatenate 'string parts))))

(defun list-index-directory (path &key (rules t) hidden)
  "The directory PATH (a native name ending in /) listed as an index lists it
(INDEX-DIRECTORY): its files and directories by name, hidden names (but with
HIDDEN), backups and links to directories left out, each entry examined once
\(TUATARA.IO:DIRECTORY-ENTRIES); with RULES, those of its .gitignore read."
  (let* ((all (ignore-errors (tuatara.io:directory-entries path :stamps t)))
         (entries (loop for (name kind nil own stamp) in all
                        unless (or (and (not hidden) (char= (char name 0) #\.)) (backup-name-p name))
                          if (and (eq kind :directory) (eq own :directory))
                            collect (list (compact-name name) :directory stamp)
                          else if (eq kind :regular)
                                 collect (list (compact-name name) :regular stamp)))
         (rules (and rules
                     (find-if (lambda (e) (and (string= (first e) ".gitignore") (eq (second e) :regular))) all)
                     (gitignore-rules (concatenate 'string path ".gitignore") path))))
    (make-index-directory (compact-name path) entries rules)))

(defun directory-listing (path &key (rules t) hidden)
  "The directory PATH (a native name ending in /) listed (INDEX-DIRECTORY): as
the index keeping it read has it (*INDEXED-DIRECTORIES*), nothing asked of the
disk, else listed now (LIST-INDEX-DIRECTORY, its .gitignore read with RULES);
with HIDDEN, names starting with a dot too, always from the disk, as an index
keeps none. Every walk of files under a directory lists them so."
  (or (dolist (index (and (not hidden) *directory-indexes*))
        (let ((tree (directory-index-tree index)))
          (when (and tree (directory-index-ready index) (uiop:string-prefix-p (directory-index-root index) path))
            (return (gethash path tree)))))
      (list-index-directory path :rules rules :hidden hidden)))

(defun index-tree-walk (tree path &optional rules listed)
  "List the directory PATH (as LISTED, when it was just listed) and every
directory under it into TREE, but those RULES (GITIGNORE-RULES, those of
the directories above PATH) and the .gitignore files on the way leave out:
git looks into no directory it leaves out, and neither does an index, which
lists and watches none of them."
  (let* ((path (compact-name path))
         (directory (or listed (list-index-directory path)))
         (rules (append rules (index-directory-rules directory))))
    (setf (gethash path tree) directory)
    (loop for (name kind) in (index-directory-entries directory)
          when (and (eq kind :directory)
                    (not (and rules (gitignored-p rules name (concatenate 'string path name) t))))
            do (index-tree-walk tree (concatenate 'string path name "/") rules))))

(defun index-tree-rules (tree root path rules)
  "The rules that apply under the directory PATH of TREE, which keeps ROOT:
RULES (those from outside ROOT, OUTER-GITIGNORE-RULES), then those of the
.gitignore of each directory from ROOT down to PATH's parent."
  (if (<= (length path) (length root))
      rules
      (let ((at (length root)))
        (loop (let ((directory (gethash (subseq path 0 at) tree)))
                (when directory (setf rules (append rules (index-directory-rules directory)))))
              (let ((slash (position #\/ path :start at)))
                (if (and slash (< (1+ slash) (length path)))
                    (setf at (1+ slash))
                    (return rules)))))))

(defun index-tree-forget (tree path)
  "Take the directory PATH and every one under it out of TREE."
  (let ((gone '()))
    (maphash (lambda (key value) (declare (ignore value)) (when (uiop:string-prefix-p path key) (push key gone))) tree)
    (dolist (key gone) (remhash key tree))))

(defun index-tree-update (tree changed root rules)
  "List again the directories CHANGED (native names) that TREE, which keeps
ROOT, holds: those made in them walked, those gone forgotten, what RULES
(from outside ROOT) and the .gitignore files leave out never walked. A
directory whose .gitignore changed is walked again whole, as what it leaves
out may lie at any depth under it."
  (dolist (path changed)
    (let ((old (gethash path tree)))
      (when old
        (let* ((above (index-tree-rules tree root path rules))
               (new (list-index-directory path)))
          (if (equal (index-directory-rules old) (index-directory-rules new))
              (let* ((rules (append above (index-directory-rules new)))
                     (subdirectories (lambda (directory)
                                       (loop for (name kind) in (index-directory-entries directory)
                                             when (and (eq kind :directory)
                                                       (not (and rules (gitignored-p rules name (concatenate 'string path name) t))))
                                               collect name)))
                     (before (funcall subdirectories old))
                     (after (funcall subdirectories new)))
                (setf (gethash path tree) new)
                (dolist (name (set-difference before after :test #'string=))
                  (index-tree-forget tree (concatenate 'string path name "/")))
                (dolist (name (set-difference after before :test #'string=))
                  (index-tree-walk tree (concatenate 'string path name "/") rules)))
              (progn (index-tree-forget tree path)
                     (index-tree-walk tree path above new))))))))

(defun index-tree-files (tree root rules most)
  "What TREE holds under ROOT, as four values: every file RULES and the
.gitignore files do not leave out, each (RELATIVE . STAMP); the same files
as a search reads them, in the order SEARCH-FILES-AS-READ would read them,
at most MOST of them, each (RELATIVE PATH STAMP); true when there were more
than MOST; and every directory, native names. A directory they leave out is
not in TREE (INDEX-TREE-WALK). Nothing on disk is asked."
  (let ((files '()) (texts '()) (count 0) (more nil) (directories '())
        (slash (load-time-value (coerce "/" 'simple-base-string))))
    (labels ((walk (path relative rules)
               (let ((directory (gethash path tree)))
                 (when directory
                   (push path directories)
                   (let ((rules (if (index-directory-rules directory)
                                    (append rules (index-directory-rules directory))
                                    rules)))
                     (loop for (name kind stamp) in (index-directory-entries directory)
                           for full = (compact-join path name)
                           for inside = (compact-join relative name)
                           do (if (eq kind :directory)
                                  (walk (compact-join full slash) (compact-join inside slash) rules)
                                  (unless (and rules (gitignored-p rules name full nil))
                                    (push (cons inside stamp) files)
                                    (if (< count most)
                                        (progn (push (list inside full stamp) texts) (incf count))
                                        (setf more t))))))))))
      (walk (compact-name root) (coerce "" 'simple-base-string) rules))
    (values (nreverse files) (nreverse texts) more (nreverse directories))))

(defun index-pieces (texts old not-text root limit piece)
  "The pieces of an index's text for TEXTS (INDEX-TREE-FILES), as a vector of
INDEX-PIECE, and as a second value the text files LIMIT octets left no room
for, each (RELATIVE PATH), to be searched from disk: a
piece of OLD kept as it is where its files come in the same order, all as
they were; the others made anew as a search of many files makes them
\(GATHER-TEXT-FILE), the text of a file as it was copied from its old piece,
that of a file changed read from disk, one found not to be text noted in
NOT-TEXT with its stamp and not read again while it stays so. Pieces made
anew hold about PIECE octets."
  (let* ((where (make-hash-table :test 'equal))
         (starting (make-hash-table :test 'equal))
         (made '()) (large '())
         (texts (coerce texts 'simple-vector))
         (gatherer (make-corpus-gatherer
                    limit piece
                    (lambda (texts size)
                      (push (make-index-piece (make-corpus (mapcar (lambda (text) (cons (first text) (second text))) texts) size root)
                                              (map 'vector #'third texts))
                            made))
                    (lambda (relative path) (push (list relative path) large)))))
    ;; where each file's text lies in the old pieces, and which old piece each file begins
    (loop for p across old
          for corpus = (index-piece-corpus p)
          do (loop for path across (file-corpus-paths corpus)
                   for i from 0
                   do (setf (gethash path where) (cons p i)))
             (when (plusp (length (file-corpus-paths corpus)))
               (setf (gethash (aref (file-corpus-paths corpus) 0) starting) p)))
    (labels ((old-octets (entry)
               ;; a file's text as an old piece holds it, its newline left off
               (destructuring-bind (p . i) entry
                 (let* ((corpus (index-piece-corpus p))
                        (starts (file-corpus-starts corpus))
                        (rope (file-corpus-rope corpus))
                        (from (aref starts i))
                        (to (if (< (1+ i) (length starts)) (aref starts (1+ i)) (tuatara.text:rope-bytes rope))))
                   (tuatara.text:rope-octets rope :start from :end (max from (1- to))))))
             (reusable (p k)
               ;; the old piece P, all its files as they were, from position K on
               (let* ((paths (file-corpus-paths (index-piece-corpus p)))
                      (stamps (index-piece-stamps p))
                      (n (length paths)))
                 (and (<= (+ k n) (length texts))
                      (loop for j below n
                            for text = (aref texts (+ k j))
                            always (and (string= (first text) (aref paths j))
                                        (same-stamp-p (third text) (aref stamps j))))))))
      (let ((k 0))
        (loop while (< k (length texts))
              do (let* ((text (aref texts k))
                        (p (gethash (first text) starting))
                        (bytes (and p (tuatara.text:rope-bytes (file-corpus-rope (index-piece-corpus p))))))
                   (if (and p (reusable p k)
                            (<= (+ (corpus-gatherer-total gatherer) bytes) limit))
                       ;; an old piece as it was: kept
                       (progn (gather-piece gatherer)
                              (push p made)
                              (incf (corpus-gatherer-total gatherer) bytes)
                              (incf k (length (file-corpus-paths (index-piece-corpus p)))))
                       (destructuring-bind (relative path stamp) text
                         (let ((entry (gethash relative where)))
                           (case (cond ((and entry (same-stamp-p stamp (aref (index-piece-stamps (car entry)) (cdr entry))))
                                        (gather-text-file gatherer relative path :stamp stamp :octets (old-octets entry)))
                                       ((same-stamp-p stamp (gethash relative not-text)) :not-text)
                                       (t (gather-text-file gatherer relative path :stamp stamp)))
                             (:text (remhash relative not-text))
                             ((nil) (when stamp (setf (gethash relative not-text) stamp)))))
                         (incf k)))))
        (gather-piece gatherer)))
    (values (coerce (nreverse made) 'simple-vector) (nreverse large))))

;;; Kept on disk between sessions

(defun index-cache-name (root)
  "The name ROOT's index is kept under in the cache directory: ROOT's name
written as one file name (TUATARA.IO:PATH-FILE-NAME), so that no two
directories share a name and a long one still fits."
  (tuatara.io:path-file-name root))

(defun index-cache-directory (root)
  "The directory the index of ROOT is kept in on disk, made if needed, for you
alone: indexes/ in the cache directory, then ROOT's name written as one file name
\(INDEX-CACHE-NAME)."
  (private-cache-directory (format nil "indexes/~a/" (index-cache-name root))))

(defun stamp-words (stamp)
  "STAMP written as eight numbers, as SAVE-INDEX writes it; - for none."
  (if stamp
      (format nil "~d ~d ~d ~d ~d ~d ~d ~d" (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp)
              (tuatara.io:stamp-size stamp) (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-nlink stamp)
              (tuatara.io:stamp-mode stamp) (tuatara.io:stamp-uid stamp) (tuatara.io:stamp-gid stamp))
      "- - - - - - - -"))

(defun mapped-piece-corpus (path bytes paths starts root)
  "A FILE-CORPUS of the piece of text kept at PATH, BYTES octets long, its
files PATHS beginning at STARTS from ROOT: the file mapped, not read, so
that its text is outside the heap and garbage collection never copies it;
counted for lines on this thread. An error when it is not BYTES long."
  (let ((rope (tuatara.text:open-file-rope path)))
    (unless (= (tuatara.text:rope-bytes rope) bytes) (error "piece ~a is not whole" path))
    (make-file-corpus (tuatara.text:index-rope rope :threads 1) paths starts root)))

(defun save-index (index directory)
  "Keep INDEX's text on disk in DIRECTORY (INDEX-CACHE-DIRECTORY): each piece
not kept yet written to a file of its own, those no longer used deleted, and
the list of them, with their files and stamps and the files found not to be
text, written whole (the file index). Done holding the index's lock, so that
two sessions keeping the same directory never take away each other's pieces;
left to the other when it holds the lock long."
  (tuatara.io:call-with-state-lock (concatenate 'string directory "index")
                                   (lambda () (save-index-now index directory))
                                   :wait 2))

(defun save-index-now (index directory)
  "SAVE-INDEX's work, its lock held."
  (let ((random (make-random-state t)) (used '()))
    (loop for p across (directory-index-pieces index)
          do (unless (index-piece-file p)
               (let* ((name (format nil "piece-~36r" (random (expt 36 12) random)))
                      (corpus (index-piece-corpus p))
                      (rope (file-corpus-rope corpus)))
                 (tuatara.io:write-new-file (concatenate 'string directory name) (tuatara.text:rope-octets rope) :mode #o600)
                 (setf (index-piece-file p) name)
                 ;; read from the file kept from now on: its text out of the heap
                 (setf (index-piece-corpus p)
                       (or (ignore-errors (mapped-piece-corpus (concatenate 'string directory name) (tuatara.text:rope-bytes rope)
                                                               (file-corpus-paths corpus) (file-corpus-starts corpus)
                                                               (file-corpus-root corpus)))
                           corpus))))
             (push (index-piece-file p) used))
    (tuatara.io:write-private-file
     (concatenate 'string directory "index")
     (with-output-to-string (out)
       (format out "tuatara index 2~%R ~a~%" (escape-path (directory-index-root index)))
       (loop for p across (directory-index-pieces index)
             for corpus = (index-piece-corpus p)
             do (format out "P ~a ~d ~d~%" (index-piece-file p) (length (file-corpus-paths corpus))
                        (tuatara.text:rope-bytes (file-corpus-rope corpus)))
                (loop for path across (file-corpus-paths corpus)
                      for start across (file-corpus-starts corpus)
                      for stamp across (index-piece-stamps p)
                      do (format out "F ~d ~a ~a~%" start (stamp-words stamp) (escape-path path))))
       (maphash (lambda (relative stamp) (format out "N ~a ~a~%" (stamp-words stamp) (escape-path relative)))
                (directory-index-not-text index))))
    ;; pieces no longer used
    (dolist (entry (ignore-errors (tuatara.io:directory-entries directory)))
      (let ((name (first entry)))
        (when (and (uiop:string-prefix-p "piece-" name) (not (member name used :test #'string=)))
          (ignore-errors (sb-posix:unlink (concatenate 'string directory name))))))))

(defun load-index (index directory)
  "Take into INDEX what SAVE-INDEX kept in DIRECTORY: its pieces of text and
the files found not to be text. True when they were taken; NIL, and nothing
taken, when there is nothing kept, it cannot be made sense of, or it was kept
for another directory than INDEX's."
  (let ((text (ignore-errors (tuatara.io:file-text (concatenate 'string directory "index"))))
        (head (format nil "tuatara index 2~%R ~a~%" (escape-path (directory-index-root index)))))
    (when (and text (uiop:string-prefix-p head text))
      (handler-case
          (let ((pieces '()) (not-text (make-hash-table :test 'equal)) (current nil))
            (labels ((numbers (words) (mapcar (lambda (w) (if (string= w "-") nil (parse-integer w))) words))
                     (stamp (words)
                       (let ((n (numbers words)))
                         (and (first n) (apply #'tuatara.io:make-stamp n))))
                     (close-piece ()
                       (when current
                         (destructuring-bind (file bytes files) current
                           (let ((files (nreverse files)))
                             (push (make-index-piece
                                    (mapped-piece-corpus (concatenate 'string directory file) bytes
                                                         (map 'simple-vector #'third files) (map 'simple-vector #'first files)
                                                         (directory-index-root index))
                                    (map 'simple-vector #'second files) file)
                                   pieces)))
                         (setf current nil))))
              (dolist (line (uiop:split-string (subseq text (length head)) :separator '(#\Newline)))
                (when (plusp (length line))
                  (let ((words (uiop:split-string line :separator " ")))
                    (cond ((string= (first words) "P")
                           (close-piece)
                           (setf current (list (second words) (parse-integer (fourth words)) '())))
                          ((string= (first words) "F")
                           ;; F START eight numbers PATH (the path may hold spaces)
                           (push (list (parse-integer (second words)) (stamp (subseq words 2 10))
                                       (unescape-path (format nil "~{~a~^ ~}" (subseq words 10))))
                                 (third current)))
                          ((string= (first words) "N")
                           (setf (gethash (unescape-path (format nil "~{~a~^ ~}" (subseq words 9))) not-text)
                                 (stamp (subseq words 1 9))))))))
              (close-piece))
            (setf (directory-index-pieces index) (coerce (nreverse pieces) 'simple-vector)
                  (directory-index-not-text index) not-text)
            t)
        (error () nil)))))

(defun read-directory-index (index changed limit piece &optional kept-on-disk)
  "Bring INDEX up to date on the thread reading it: the whole of its directory
when it has not been read (CHANGED :ALL), else only the directories CHANGED,
the text of only the files changed read from disk and only the pieces holding
them made anew. Returns (FILES CORPORA LEFT-OUT LARGE DIRECTORIES), as
DIRECTORY-INDEX keeps them, its text in pieces of about PIECE octets, at most
LIMIT in all. With KEPT-ON-DISK, the pieces kept at the last session are
taken first, at the start, and what changed is kept again after (SAVE-INDEX)."
  (let* ((root (directory-index-root index))
         (cache (and kept-on-disk (ignore-errors (index-cache-directory root)))))
    (when (and cache (null (directory-index-tree index)) (zerop (length (directory-index-pieces index))))
      (load-index index cache))
    (let ((outer (outer-gitignore-rules root)))
      (if (or (eq changed :all) (null (directory-index-tree index)))
          ;; looked at by the walks of other threads too (DIRECTORY-LISTING)
          (let ((tree (make-hash-table :test 'equal :synchronized t)))
            (index-tree-walk tree root outer)
            (setf (directory-index-tree index) tree))
          (index-tree-update (directory-index-tree index) changed root outer)))
    (multiple-value-bind (files texts more directories)
        (index-tree-files (directory-index-tree index) root (outer-gitignore-rules root) 100000)
      (multiple-value-bind (pieces large)
          (index-pieces texts (directory-index-pieces index) (directory-index-not-text index) root limit piece)
        (setf (directory-index-pieces index) pieces)
        (when cache (ignore-errors (save-index index cache)))
        (list (stable-sort files #'> :key (lambda (file) (if (cdr file) (tuatara.io:stamp-mtime (cdr file)) 0)))
              (map 'simple-vector #'index-piece-corpus pieces)
              more
              large
              directories)))))

(defun start-reading-index (index &optional (changed :all))
  "Bring INDEX up to date on a thread of its own (READ-DIRECTORY-INDEX): the
directories CHANGED, or :ALL. While that is under way already, they are
noted, and read once it is over."
  (if (directory-index-reading index)
      (setf (directory-index-again index) t
            (directory-index-pending index)
            (if (or (eq changed :all) (eq (directory-index-pending index) :all))
                :all
                (union changed (directory-index-pending index) :test #'string=)))
      (let ((limit *search-files-limit*)
            (piece *search-files-piece*)
            (kept-on-disk *indexes-kept-on-disk*)
            (editor *editor*))
        (setf (directory-index-reading index) t
              (directory-index-again index) nil
              (directory-index-pending index) '())
        (note-sign-work :indexing)
        (tuatara.runtime:make-guarded-thread
         (lambda ()
           (unwind-protect
                (setf (directory-index-result index)
                      (or (tuatara.runtime:ignoring-errors-reported ("reading ~a to keep" (directory-index-root index))
                            (read-directory-index index changed limit piece kept-on-disk))
                          :failed))
             ;; however it ends (a lack of memory too), the reading is over, never shown as going on
             (unless (directory-index-result index) (setf (directory-index-result index) :failed))
             (note-sign-work :indexing editor)))
         :name "tuatara reading an indexed directory"))))

(defun indexes-tick (editor)
  "Tick function: keep an index of each of *INDEXED-DIRECTORIES*, read in the
background, and put in place what was read. True while one is read."
  (declare (ignore editor))
  (let ((wanted (remove-duplicates (mapcar (lambda (d) (ignore-errors (directory-name d))) (setting-list '*indexed-directories* *indexed-directories*))
                                   :test #'equal)))
    ;; those no longer asked for go; those newly asked for are read
    (setf *directory-indexes*
          (remove-if-not (lambda (index) (member (directory-index-root index) wanted :test #'equal)) *directory-indexes*))
    (dolist (root (remove nil wanted))
      (unless (find root *directory-indexes* :key #'directory-index-root :test #'string=)
        (let ((index (make-directory-index root)))
          (push index *directory-indexes*)
          (start-reading-index index)))))
  (let ((working nil))
    (dolist (index *directory-indexes*)
      (let ((result (directory-index-result index)))
        (when result
          (setf (directory-index-result index) nil
                (directory-index-reading index) nil)
          (unless (eq result :failed)
            (destructuring-bind (files corpora left-out large directories) result
              (setf (directory-index-files index) files
                    (directory-index-corpora index) corpora
                    (directory-index-left-out index) left-out
                    (directory-index-large index) large
                    (directory-index-directories index) directories
                    (directory-index-ready index) t)))
          (when (directory-index-again index)
            (start-reading-index index (if (eq result :failed) :all (directory-index-pending index))))))
      (when (directory-index-reading index) (setf working t)))
    working))

(defvar *indexed-directories-watched* (cons nil '())
  "The indexes the directories to watch were last gathered for, and those
directories: the same list is given while the indexes are the same, so that
following them costs nothing between changes.")

(defun indexed-directories-watched ()
  "The directories under every directory kept read, to be watched on disk;
the same list as before while no index was read again."
  (let ((key (mapcar #'directory-index-directories *directory-indexes*)))
    (unless (and (= (length key) (length (car *indexed-directories-watched*)))
                 (every #'eq key (car *indexed-directories-watched*)))
      (setf *indexed-directories-watched* (cons key (loop for list in key append list))))
    (cdr *indexed-directories-watched*)))

(defun indexed-directories-changed (changed)
  "Bring up to date, in the background, the indexes with a directory among
CHANGED: only those directories listed again."
  (dolist (index *directory-indexes*)
    (let ((root (directory-index-root index)))
      (let ((mine (remove-if-not (lambda (path) (uiop:string-prefix-p root path)) changed)))
        (when mine
          (start-reading-index index mine))))))

(defun index-for (directory)
  "The index kept read that holds DIRECTORY (its root, or a directory under
it), once it has been read, and as a second value DIRECTORY's path from its
root (\"\" for the root itself); NIL when there is none."
  (let ((wanted (ignore-errors (directory-name directory))))
    (when wanted
      (dolist (index *directory-indexes*)
        (let ((root (directory-index-root index)))
          (when (and (directory-index-ready index) (uiop:string-prefix-p root wanted))
            (return (values index (subseq wanted (length root))))))))))

(defun indexed-files (directory)
  "The files under DIRECTORY as an index keeps them, each (RELATIVE . STAMP),
RELATIVE its path from DIRECTORY, the ones changed last first; NIL, and a
second value NIL, when no index holds DIRECTORY."
  (multiple-value-bind (index inside) (index-for directory)
    (if index
        (values (if (string= inside "")
                    (directory-index-files index)
                    (loop for (relative . stamp) in (directory-index-files index)
                          when (uiop:string-prefix-p inside relative)
                            collect (cons (subseq relative (length inside)) stamp)))
                t)
        (values nil nil))))

(defparameter *sign-idle-fraction* 1/6
  "How far the mode line's signs before [-], while their work is not going on,
stand from its ground towards its text: 0 the ground itself, unseen, 1 the
text's colour.")

(sb-ext:defglobal **sign-colours-generation** nil
  "The face generation (TUATARA.DISPLAY:*FACE-GENERATION*) the idle sign's
colour was last worked out for (FOLLOW-MODE-LINE-COLOURS).")

(defun follow-mode-line-colours ()
  "Give :mode-line-sign-idle a colour a little off the mode line's ground
\(*SIGN-IDLE-FRACTION* towards its text), from the mode line's colours as the
theme in force has them, so that any theme, or a mode line you coloured, gets
a sign that matches. Left alone when you set the face yourself, under a theme
without colours, or where the mode line's colours are not known. Worked out
again only once the faces have changed."
  (unless (or (eql **sign-colours-generation** tuatara.display:*face-generation*)
              (assoc :mode-line-sign-idle tuatara.display:*user-face-settings*)
              (let ((theme (tuatara.display:find-theme tuatara.display:*current-theme*)))
                (and theme (tuatara.display:theme-colourless theme))))
    (let* ((reverse (tuatara.display:face-attribute :mode-line :reverse))
           (fg (or (tuatara.display:face-attribute :mode-line :fg) (tuatara.display:face-attribute :default :fg)))
           (bg (or (tuatara.display:face-attribute :mode-line :bg) (tuatara.display:face-attribute :default :bg)))
           (ground (tuatara.display:colour-hex (if reverse fg bg)))
           (ink (tuatara.display:colour-hex (if reverse bg fg))))
      (when (and ground ink)
        (let ((colour (tuatara.display:mix-colours ground ink *sign-idle-fraction*)))
          (unless (equal colour (tuatara.display:face-attribute :mode-line-sign-idle :fg))
            (tuatara.display:set-face-attributes :mode-line-sign-idle :fg colour :dim nil))))))
  (setf **sign-colours-generation** tuatara.display:*face-generation*))

(defun sign-lit-p (sign working)
  "True while SIGN, a keyword naming a sign of the mode line, is to be lit:
while WORKING, and for *SIGN-LIT-SECONDS* after its work last began or ended
\(NOTE-SIGN-WORK)."
  (or working (not (tuatara.runtime:deadline-passed-p (gethash sign *signs-lit-until* 0)))))

(defun mode-line-sign (indicator working)
  "The part of the mode line for INDICATOR, a sign before [-]: lit in
:mode-line-sign while WORKING, else faint in :mode-line-sign-idle; NIL with
no indicator."
  (when (and indicator (plusp (length indicator)))
    (if working
        (list indicator :mode-line-sign)
        (progn (follow-mode-line-colours)
               (list indicator :mode-line-sign-idle)))))

(defun indexing-mode-line (editor buffer)
  "Mode line function: while directories are kept read, *INDEXING-INDICATOR*,
lit while one is being read (and for a moment after, SIGN-LIT-P), else faint
\(MODE-LINE-SIGN)."
  (declare (ignore editor buffer))
  (and *indexed-directories*
       (mode-line-sign *indexing-indicator*
                       (sign-lit-p :indexing (some #'directory-index-reading *directory-indexes*)))))

(defun searching-mode-line (editor buffer)
  "Mode line function: while directories are kept read, *SEARCHING-INDICATOR*,
lit while a search runs over one (and for a moment after, SIGN-LIT-P), else
faint (MODE-LINE-SIGN)."
  (declare (ignore editor buffer))
  (and *indexed-directories*
       (mode-line-sign *searching-indicator* (sign-lit-p :searching (plusp (car *index-searches*))))))

(defun mode-line-signs (editor buffer)
  "Mode line function, before [-]: the sign of work for BUFFER
\(PROCESSING-MODE-LINE), then while directories are kept read the signs of
their reading (INDEXING-MODE-LINE) and of their searching
\(SEARCHING-MODE-LINE), side by side, PUS; each NIL leaves its sign out."
  (remove nil (list (processing-mode-line editor buffer)
                    (indexing-mode-line editor buffer)
                    (searching-mode-line editor buffer))))

(defun signs-tick (editor)
  "Tick function: while a sign of the mode line is lit, the seconds until the
next is to be drawn faint again, for the editor to look again then; NIL when
none is lit."
  (declare (ignore editor))
  (let ((now (get-internal-real-time)) (soonest nil))
    (maphash (lambda (sign until)
               (declare (ignore sign))
               (when (> until now)
                 (let ((seconds (/ (- until now) internal-time-units-per-second)))
                   (setf soonest (if soonest (min soonest seconds) seconds)))))
             *signs-lit-until*)
    soonest))

(defun install-indexes ()
  "Keep the directories of *INDEXED-DIRECTORIES* read, and read again as they
change on disk, the mode line showing *INDEXING-INDICATOR* meanwhile and
*SEARCHING-INDICATOR* while they are searched, after the sign of work for
the buffer (*PROCESSING-INDICATOR*)."
  (pushnew 'indexes-tick *tick-functions*)
  (pushnew 'signs-tick *tick-functions*)
  (pushnew 'mode-line-signs *mode-line-lead-functions*)
  (follow-directories-for 'indexed-directories-watched 'indexed-directories-changed))
