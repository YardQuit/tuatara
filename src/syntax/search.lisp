;;;; src/syntax/search.lisp --- Literal and regular-expression search over ropes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Literal search runs over octets in chunks that overlap by the needle length,
;;;; so a match across a leaf or chunk boundary is found and the text is never
;;;; decoded. SEARCH-ALL splits the range across threads for replace-all and for
;;;; files of many gigabytes. Regular expressions go through cl-ppcre over
;;;; decoded windows (see the section below). Directional searches of both kinds
;;;; go parallel beyond a serial window, as one batch of pieces taken in order,
;;;; and can be stopped between pieces, so an incremental search gives way to the
;;;; next key.

(in-package #:tuatara.syntax)

(defconstant +search-chunk+ 65536 "Octets searched per chunk.")
(defconstant +regex-overlap+ 4096 "Octets of overlap between regex chunks; the longest match found across a chunk edge.")

(sb-ext:defglobal *fold-table*
    (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
      (dotimes (b 256 table)
        (setf (aref table b) (if (<= 65 b 90) (+ b 32) b))))
  "ASCII case folding for octets: upper-case letters mapped to lower case.")
(declaim (type (simple-array (unsigned-byte 8) (256)) *fold-table*))

(defun needle-octets (string case-fold)
  "STRING as UTF-8 octets, folded when CASE-FOLD."
  (let ((octets (tuatara.text:string-octets string)))
    (when case-fold
      (loop for i from 0 below (length octets) do (setf (aref octets i) (aref *fold-table* (aref octets i)))))
    octets))

(defun case-fold-p (string)
  "True when STRING has no upper-case letter, so a search for it ignores case."
  (notany #'upper-case-p string))

(defun folds-past-ascii-p (string case-fold)
  "True when a search for STRING ignoring case (CASE-FOLD) must fold letters
past ASCII, as é with É, which the octets alone cannot: such a search goes
by the pattern of STRING's text (LITERAL-SCANNER)."
  (and case-fold (some (lambda (c) (and (>= (char-code c) 128) (both-case-p c))) string)))

(defun character-start-from (rope byte)
  "The first character's start of ROPE at or after BYTE."
  (if (tuatara.text:char-boundary-p rope byte) byte (tuatara.text:rope-next-boundary rope byte)))

(defun literal-scanner (string)
  "A scanner for STRING's text, nothing in it special, ignoring case (COMPILE-REGEX)."
  (compile-regex (cl-ppcre:quote-meta-chars string) :case-fold t))

(declaim (inline needle-at-p))
(defun needle-at-p (chunk i needle case-fold)
  "True when NEEDLE, its octets ASCII case folded when CASE-FOLD, stands in
CHUNK at index I, whose octet matches its first already."
  (declare (type tuatara.text:octets chunk needle) (type fixnum i) (optimize speed (safety 0)))
  (let ((table *fold-table*))
    (loop for k of-type fixnum from 1 below (length needle)
          always (= (if case-fold (aref table (aref chunk (+ i k))) (aref chunk (+ i k)))
                    (aref needle k)))))

(defconstant +near-octets+ 32
  "How many octets on a search looks at one by one for where its needle's
first octet stands, before it hands the rest to POSITION: a needle that
begins with a common octet finds it nearby without a call, and a long
stretch without it is gone through many octets at a time.")

(defun scan-chunk (chunk needle case-fold from below &optional (filled (length chunk)) other-case)
  "Index of the first occurrence of NEEDLE in the first FILLED octets of CHUNK
starting at or after FROM and before BELOW (the start index limit), or NIL.
Both are octet vectors. The places its first octet stands are found one
octet at a time nearby (+NEAR-OCTETS+), else by POSITION, which looks at
many octets at a time, and the rest of it is compared there: ignoring case, a letter's other case is looked for no
further than the first case's next place, so the first found is the first
there is, and no octet is looked at twice. OTHER-CASE, a cons kept by a
caller that searches the same CHUNK again from where it left off, holds
where the other case stands next and how far it was looked for, so that it
is looked for once for the whole chunk."
  (declare (type tuatara.text:octets chunk needle) (type fixnum from below filled) (optimize speed (safety 0)))
  (let ((n (length needle)))
    (declare (type fixnum n))
    (when (zerop n) (return-from scan-chunk from))
    (let* ((first (aref needle 0))
           (other (and case-fold (<= 97 first 122) (- first 32)))
           (limit (min below (- (1+ filled) n))))
      (declare (type (unsigned-byte 8) first) (type fixnum limit))
      (when (>= from limit) (return-from scan-chunk nil))
      (flet ((next (octet start end)
               (declare (type (unsigned-byte 8) octet) (type fixnum start end))
               (let ((near (min end (+ start +near-octets+))))
                 (declare (type fixnum near))
                 (loop for k of-type fixnum from start below near
                       when (= (aref chunk k) octet) do (return-from next k))
                 (and (< near end) (position octet chunk :start near :end end)))))
        ;; A, the first case's next place; B, the other case's, looked for only as far
        ;; as A (to LOOKED), so that no octet is looked at twice, nor further than needed
        (let* ((a (next first from limit))
               (kept (and other-case (cdr other-case) (>= (cdr other-case) from)))
               (b (and kept (car other-case) (>= (car other-case) from) (car other-case)))
               (looked (if kept (cdr other-case) from)))
          (declare (type fixnum looked))
          (flet ((done (at)
                   (when other-case (setf (car other-case) b (cdr other-case) looked))
                   (return-from scan-chunk at)))
            (loop
              ;; with OTHER-CASE kept, as far as the chunk: looked for once for all of it
              (let ((end (if other-case limit (or a limit))))
                (when (and other (null b) (< looked end))
                  (setf b (next other looked end)
                        looked (if b (1+ b) end))))
              (let ((at (if (and b (or (null a) (< b a))) b a)))
                (unless at (done nil))
                (when (needle-at-p chunk at needle case-fold) (done at))
                (if (eql at a)
                    (setf a (next first (1+ a) limit))
                    (setf b nil))))))))))

(defun scan-chunk-backward (chunk needle case-fold from &optional (filled (length chunk)))
  "Index of the last occurrence of NEEDLE in the first FILLED octets of CHUNK
starting at or before FROM, or NIL. As SCAN-CHUNK, backwards: the places its
first octet stands found nearby one by one, else by POSITION from the end, a
letter's other case looked for no further back than the first case's next
place, the nearer to FROM taken first."
  (declare (type tuatara.text:octets chunk needle) (type fixnum from filled) (optimize speed (safety 0)))
  (let ((n (length needle)))
    (declare (type fixnum n))
    (when (zerop n) (return-from scan-chunk-backward from))
    (let* ((first (aref needle 0))
           (other (and case-fold (<= 97 first 122) (- first 32)))
           (top (min from (- filled n))))
      (declare (type (unsigned-byte 8) first) (type fixnum top))
      (when (minusp top) (return-from scan-chunk-backward nil))
      (flet ((previous (octet below floor)
               (declare (type (unsigned-byte 8) octet) (type fixnum below floor))
               (let ((near (max floor (- below +near-octets+))))
                 (declare (type fixnum near))
                 (loop for k of-type fixnum from (1- below) downto near
                       when (= (aref chunk k) octet) do (return-from previous k))
                 (and (> near floor) (position octet chunk :start floor :end near :from-end t)))))
        ;; as SCAN-CHUNK, backwards: B looked for only down to A (to LOOKED)
        (let ((a (previous first (1+ top) 0)) (b nil) (looked (1+ top)))
          (declare (type fixnum looked))
          (loop
            (let ((floor (if a (1+ a) 0)))
              (when (and other (null b) (> looked floor))
                (setf b (previous other looked floor)
                      looked (or b floor))))
            (let ((at (if (and b (or (null a) (> b a))) b a)))
              (unless at (return nil))
              (when (needle-at-p chunk at needle case-fold) (return at))
              (if (eql at a)
                  (setf a (previous first a 0))
                  (setf b nil)))))))))

(defvar *serial-window* (* 4 1024 1024)
  "Octets a directional search scans on the calling thread before going
parallel; a common needle is found here without waking the pool.")

(defvar *parallel-piece* (* 2 1024 1024)
  "Octets per task when a directional search goes parallel. The pieces of the
whole range form one batch that the pool takes in order, so small pieces cost
little and keep every thread busy on a file of tens of megabytes; a slow regex
scans a piece in a few tens of milliseconds, which bounds how long a search
takes to notice STOP.")

(defun search-pieces (count function stop threads)
  "The result of FUNCTION, called with a piece index and a HALT function, for
the lowest index below COUNT that gives one, or NIL when none does. The pieces
run as one batch on the worker pool and the calling thread, taken in index
order, on up to THREADS threads; once a piece gives a result the pieces after
it are skipped. HALT answers true once the search is to give up: STOP, when
given, is asked through it on the calling thread only, and when STOP answers
true every piece's HALT does, pieces not yet begun are skipped, and the values
are NIL and :STOPPED, a piece inside one long stoppable call cut short
\(TUATARA.RUNTIME:STOPPABLE). A piece should ask HALT every few milliseconds
of work. STOP is also asked once before the pieces start, so it is heard even when the
pool's workers take every piece."
  (when (and stop (funcall stop))
    (return-from search-pieces (values nil :stopped)))
  (let* ((best (list count)) (cancel (list nil)) (caller sb-thread:*current-thread*)
         (halt (lambda ()
                 (or (car cancel)
                     (and stop (eq sb-thread:*current-thread* caller) (funcall stop)
                          ;; a piece inside one long call is cut short too
                          (tuatara.runtime:stop-work cancel)))))
         (tasks (make-array count)))
    (dotimes (k count)
      (setf (aref tasks k)
            (let ((k k))
              (lambda ()
                (cond ((or (> k (car best)) (funcall halt)) nil)
                      (t (let ((result (funcall function k halt)))
                           (when result
                             ;; lower the index pieces must stay below to be worth running
                             (loop for old = (car best)
                                   while (< k old)
                                   until (eql old (sb-ext:compare-and-swap (car best) old k))))
                           result)))))))
    (let ((results (tuatara.runtime:stoppable-work (cancel)
                     (tuatara.runtime:run-tasks tasks :parallelism (max 1 threads)))))
      (if (car cancel)
          (values nil :stopped)
          (find-if-not #'null results)))))

(defconstant +halt-chunks+ 16
  "Chunks a scan reads between asking whether to give up: a megabyte, a
fraction of a millisecond in most text, a few milliseconds where what is
looked for begins with nearly every octet.")

(defun scan-chunks-forward (rope start end chunk n look &optional halt)
  "The first place starting in [START, END) of ROPE that LOOK finds, reading
ROPE into CHUNK +SEARCH-CHUNK+ octets at a time and N - 1 more, for what is
looked for to be seen whole across the edge: LOOK, a function of the octets
filled and the index below which a place must start, gives the index in
CHUNK of the first, or NIL. NIL when none; NIL and :STOPPED when HALT, a
function asked every +HALT-CHUNKS+ chunks, answers true."
  (declare (type fixnum start end n) (type tuatara.text:octets chunk) (type function look))
  (let ((total (tuatara.text:rope-bytes rope)))
    (loop for pos of-type fixnum from start below end by +search-chunk+
          for k of-type fixnum from 1
          do (when (and halt (zerop (mod k +halt-chunks+)) (funcall halt))
               (return (values nil :stopped)))
             (let* ((chunk-end (min total (+ pos +search-chunk+ (1- n))))
                    (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk))
                    (hit (funcall look filled (min filled +search-chunk+ (- end pos)))))
               (when hit (return (+ pos hit))))
          finally (return nil))))

(defun scan-range-forward (rope needle case-fold start end chunk &optional halt)
  "First occurrence of NEEDLE starting in [START, END) of ROPE, reading through
CHUNK; NIL when none; NIL and :STOPPED when HALT answers true
\(SCAN-CHUNKS-FORWARD)."
  (declare (type fixnum start end) (type tuatara.text:octets needle chunk))
  (flet ((look (filled below) (scan-chunk chunk needle case-fold 0 below filled)))
    (declare (dynamic-extent #'look))
    (scan-chunks-forward rope start end chunk (length needle) #'look halt)))

(defun scan-shape-chunk (chunk shape from below filled)
  "Index of the first place from FROM below BELOW in the first FILLED octets
of CHUNK where SHAPE, a vector of octet sets (REQUIRED-SHAPE), stands whole;
NIL when none."
  (declare (type tuatara.text:octets chunk) (type simple-vector shape) (type fixnum from below filled)
           (optimize speed))
  (let* ((n (length shape))
         (first (svref shape 0))
         (last (- (min below (1+ (- filled n))) 1)))
    (declare (type simple-bit-vector first) (type fixnum n last))
    (loop for i of-type fixnum from from to last
          when (and (= 1 (sbit first (aref chunk i)))
                    (loop for k of-type fixnum from 1 below n
                          always (= 1 (sbit (the simple-bit-vector (svref shape k)) (aref chunk (+ i k))))))
            return i)))

(defun scan-shape-forward (rope shape start end chunk)
  "First place starting in [START, END) of ROPE where SHAPE, a vector of octet
sets, stands whole, reading through CHUNK; NIL when none."
  (declare (type fixnum start end) (type simple-vector shape) (type tuatara.text:octets chunk))
  (flet ((look (filled below) (scan-shape-chunk chunk shape 0 below filled)))
    (declare (dynamic-extent #'look))
    (scan-chunks-forward rope start end chunk (length shape) #'look)))

(defun search-forward (rope string &key (start 0) (end (tuatara.text:rope-bytes rope)) (case-fold (case-fold-p string))
                                        (threads (tuatara.runtime:cpu-count)) stop)
  "Byte offset of the first occurrence of STRING in ROPE starting at or after
START and ending at or before END, and the offset after it, as two values; NIL
when there is none. CASE-FOLD ignores case (the default when STRING has no
upper-case letter): ASCII case on the octets, and for a STRING with letters
past ASCII, case of every letter, through the pattern of its text. The first *SERIAL-WINDOW* octets are scanned on the
calling thread; beyond that the range is cut into pieces of *PARALLEL-PIECE*
octets run in order on THREADS threads (SEARCH-PIECES), so a rare needle costs
O(distance / THREADS). STOP, when given, is asked every megabyte or so
\(+HALT-CHUNKS+); when it answers true the search gives up and returns NIL
and :STOPPED.
Complexity: O(distance scanned)."
  (declare (type fixnum start end))
  (when (folds-past-ascii-p string case-fold)
    (return-from search-forward
      (multiple-value-bind (found after) (regex-search-forward rope (literal-scanner string) :start (character-start-from rope start)
                                                                    :end end :bound end
                                                                    :threads threads :stop stop)
        (values found after))))
  (let* ((needle (needle-octets string case-fold)) (n (length needle))
         (end (- end (1- n))))              ; last position a match may start at, exclusive
    (when (and (plusp n) (< start end))
      (let ((serial-end (min end (+ start *serial-window*))))
        (multiple-value-bind (hit stopped)
            (tuatara.text:with-octet-block (chunk (+ +search-chunk+ n))
              (scan-range-forward rope needle case-fold start serial-end chunk stop))
          (cond (hit (values hit (+ hit n)))
                (stopped (values nil :stopped))
                ((>= serial-end end) nil)
                (t (let ((piece *parallel-piece*))
                     (multiple-value-bind (found stopped)
                         (search-pieces (ceiling (- end serial-end) piece)
                                        (lambda (k halt)
                                          (let ((a (+ serial-end (* k piece))))
                                            (tuatara.text:with-octet-block (chunk (+ +search-chunk+ n))
                                              (scan-range-forward rope needle case-fold a (min end (+ a piece)) chunk halt))))
                                        stop threads)
                       (cond (stopped (values nil :stopped))
                             (found (values found (+ found n)))
                             (t nil)))))))))))

(defun scan-range-backward (rope needle case-fold start end chunk &optional halt)
  "Last occurrence of NEEDLE starting in [END, START) of ROPE, scanning backward
through CHUNK; NIL when none; NIL and :STOPPED when HALT, a function asked
every +HALT-CHUNKS+ chunks, answers true."
  (declare (type fixnum start end) (type tuatara.text:octets needle chunk))
  (let ((n (length needle)) (total (tuatara.text:rope-bytes rope)))
    (when (< end start)
      (loop for pos of-type fixnum = (max end (- start +search-chunk+)) then (max end (- pos +search-chunk+))
            for k of-type fixnum from 1
            do (when (and halt (zerop (mod k +halt-chunks+)) (funcall halt))
                 (return (values nil :stopped)))
               (let* ((chunk-end (min total (+ pos +search-chunk+ (1- n)) (+ start (1- n))))
                      (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk))
                      (hit (scan-chunk-backward chunk needle case-fold (- (min filled (- start pos)) 1) filled)))
                 (when hit (return (+ pos hit)))
                 (when (= pos end) (return nil)))))))

(defun search-backward (rope string &key (start (tuatara.text:rope-bytes rope)) (end 0) (case-fold (case-fold-p string))
                                         (threads (tuatara.runtime:cpu-count)) stop)
  "Byte offset of the last occurrence of STRING in ROPE that starts before START
and at or after END, and the offset after it, as two values; NIL when none.
Serial for the first *SERIAL-WINDOW* octets, then pieces going back, in
parallel, and STOP, as SEARCH-FORWARD."
  (declare (type fixnum start end))
  (when (folds-past-ascii-p string case-fold)
    (return-from search-backward
      (multiple-value-bind (found after) (regex-search-backward rope (literal-scanner string) :start start :end end
                                                                     :threads threads :stop stop)
        (values found after))))
  (let* ((needle (needle-octets string case-fold)) (n (length needle)))
    (when (and (plusp n) (<= (+ end n) start))
      (let ((serial-start (max end (- start *serial-window*))))
        (multiple-value-bind (hit stopped)
            (tuatara.text:with-octet-block (chunk (+ +search-chunk+ n))
              (scan-range-backward rope needle case-fold start serial-start chunk stop))
          (cond (hit (values hit (+ hit n)))
                (stopped (values nil :stopped))
                ((<= serial-start end) nil)
                (t (let ((piece *parallel-piece*))
                     (multiple-value-bind (found stopped)
                         (search-pieces (ceiling (- serial-start end) piece)
                                        (lambda (k halt)
                                          (let ((b (- serial-start (* k piece))))
                                            (tuatara.text:with-octet-block (chunk (+ +search-chunk+ n))
                                              (scan-range-backward rope needle case-fold b (max end (- b piece)) chunk halt))))
                                        stop threads)
                       (cond (stopped (values nil :stopped))
                             (found (values found (+ found n)))
                             (t nil)))))))))))

(defun search-all (rope string &key (start 0) (end (tuatara.text:rope-bytes rope)) (case-fold (case-fold-p string))
                                    (threads (tuatara.runtime:cpu-count)) (limit most-positive-fixnum))
  "All non-overlapping occurrences of STRING in ROPE between START and END as a
sorted vector of byte offsets (fixnums), searched in parallel on up to THREADS
threads, at most LIMIT of them, as one scan from START finds them: a piece
whose first occurrence overlaps the last of the piece before is searched
again from where that one ends. Matches are collected in number vectors, not
lists, so parallel threads do not contend for the allocator.
Complexity: O((END - START) / THREADS)."
  (declare (type fixnum start end))
  (when (folds-past-ascii-p string case-fold)
    (return-from search-all
      (let ((found (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0)))
        (map-regex-matches (lambda (s e m g)
                             (declare (ignore e m g))
                             (vector-push-extend s found)
                             (>= (fill-pointer found) limit))
                           rope (literal-scanner string) (character-start-from rope start) end :bound end :texts nil)
        (coerce found '(simple-array fixnum (*))))))
  (let* ((needle (needle-octets string case-fold)) (n (length needle)))
    (if (or (zerop n) (< (- end start) n))
        (make-array 0 :element-type 'fixnum)
        (let* ((span (- end start))
               ;; eight pieces a thread: the fast cores take more of them than the slow ones
               (pieces (max 1 (min (* 8 threads) (ceiling span (* 4 +search-chunk+)))))
               (piece (ceiling span pieces))
               (tasks (make-array pieces)))
          (labels ((piece-matches (a b)
                     ;; matches starting in [a, b), as (VECTOR . COUNT); reading may
                     ;; extend past b by the needle, through a kept chunk
                     (let ((found (make-array 256 :element-type 'fixnum)) (count 0) (pos a))
                       (declare (type fixnum pos count) (type (simple-array fixnum (*)) found))
                       (tuatara.text:with-octet-block (chunk (+ +search-chunk+ n))
                         (loop while (< pos b)
                               do (let* ((chunk-end (min end (+ pos +search-chunk+ (1- n))))
                                         (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk))
                                         (below (min filled +search-chunk+ (- b pos)))
                                         (i 0)
                                         (other-case (cons nil nil)))
                                    (declare (type fixnum i below filled))
                                    (loop
                                      (let ((hit (and (< count limit) (scan-chunk chunk needle case-fold i below filled other-case))))
                                        (unless hit (return))
                                        (when (= count (length found))
                                          (let ((bigger (make-array (* 2 (length found)) :element-type 'fixnum)))
                                            (replace bigger found)
                                            (setf found bigger)))
                                        (setf (aref found count) (+ pos hit))
                                        (incf count)
                                        (setf i (+ hit n))))
                                    ;; a match running past the chunk: the next chunk from its end
                                    (setf pos (if (> i +search-chunk+) (+ pos i) (+ pos +search-chunk+))))))
                       (cons found count)))
                   (piece-bounds (i)
                     (values (+ start (* i piece)) (min end (+ start (* (1+ i) piece))))))
            (dotimes (i pieces)
              (multiple-value-bind (a b) (piece-bounds i)
                (setf (aref tasks i) (lambda () (piece-matches a b)))))
            (let* ((results (tuatara.runtime:run-tasks tasks :parallelism threads))
                   (out (make-array 256 :element-type 'fixnum))
                   (k 0) (last (- n)))
              (declare (type fixnum k last) (type (simple-array fixnum (*)) out))
              ;; pieces come back in order; one whose first match overlaps the last kept is
              ;; searched again from where that ends, as the scan from START goes on
              (dotimes (i pieces)
                (destructuring-bind (found . count) (aref results i)
                  (declare (type (simple-array fixnum (*)) found) (type fixnum count))
                  (when (and (plusp count) (< (aref found 0) (+ last n)))
                    (multiple-value-bind (a b) (piece-bounds i)
                      (destructuring-bind (again . many) (piece-matches (max a (+ last n)) b)
                        (setf found again count many))))
                  (loop for j of-type fixnum from 0 below count
                        for p of-type fixnum = (aref found j)
                        while (< k limit)
                        do (when (>= p (+ last n))
                             (when (= k (length out))
                               (let ((bigger (make-array (* 2 (length out)) :element-type 'fixnum)))
                                 (replace bigger out)
                                 (setf out bigger)))
                             (setf (aref out k) p last p)
                             (incf k)))))
              (subseq out 0 k)))))))

;;; Regular expressions
;;;
;;; A pattern is matched against windows of decoded text: each window starts
;;; +REGEX-CONTEXT+ octets before the range it answers for, so ^, $, \b and
;;; short lookbehinds see the neighbouring characters. It ends after the line
;;; holding the range's last start when the pattern cannot match across lines
;;; (most can't), else +REGEX-OVERLAP+ octets past the range, growing to twice
;;; its size while a match reaches its end, up to *REGEX-LONGEST-MATCH* octets.
;;; When every match holds some fixed text, the scan jumps to it at literal
;;; speed. Invalid UTF-8 decodes byte by byte as raw bytes, U+DC80 to U+DCFF,
;;; so that a pattern can name them (\xA9) and they match only themselves.

(defconstant +regex-context+ 64
  "Octets of text before a regex window's range decoded with it, so that ^, \b
and short lookbehinds see the characters before the range.")

(defvar *regex-longest-match* (* 1024 1024)
  "Octets a regular-expression window may grow to so that a long match is found
whole; a match longer than this is cut short, but for a block that a filter
reads on past the window, Base32 or Base64 (TEXT-READER).")

(defun literal-pattern-p (pattern)
  "True when PATTERN has none of the characters that make a regular expression
special, nor a raw byte (which as plain octets would be found inside a valid
character too), so searching for it as plain text finds exactly the same
matches."
  (not (find-if (lambda (c) (or (find c "\\^$.|?*+()[]{}") (tuatara.text:raw-byte-code-p (char-code c)))) pattern)))

(defun regex-case-fold-p (pattern)
  "True when a search for PATTERN ignores case (smart case): when it has no
upper-case letter outside an escape such as \\W or \\S, a character's code
\(\\x4A, \\x{1F600}), the name of a group ((?<Year>...), \\k<Year>) and the
name of a character property (\\pL, \\p{Greek}), and names no case either: no
[:upper:] or [:lower:] (or [:^upper:], [:^lower:]) in a set, no \\p{Lu},
\\p{Ll} or \\p{Lt} (written in any case, or by its long name,
Uppercase_Letter), which ask for case as a capital letter does."
  (let ((i 0) (n (length pattern)))
    (flet ((past (char from)
             ;; just past the next CHAR at or after FROM, or the end
             (let ((at (position char pattern :start (min from n)))) (if at (1+ at) n))))
      (loop while (< i n)
            do (let ((c (char pattern i))
                     (d (and (< (1+ i) n) (char pattern (1+ i)))))
                 (cond ((char= c #\\)
                        (cond ((and (eql d #\x) (< (+ i 2) n) (char= (char pattern (+ i 2)) #\{))
                               (setf i (past #\} i)))
                              ((eql d #\x)
                               ;; up to two hexadecimal digits
                               (incf i 2)
                               (loop repeat 2 while (and (< i n) (digit-char-p (char pattern i) 16)) do (incf i)))
                              ((and (eql d #\k) (< (+ i 2) n) (char= (char pattern (+ i 2)) #\<))
                               (setf i (past #\> i)))
                              ((not (and (< (+ i 2) n) (find d "pP"))) (incf i 2))
                              ((char= (char pattern (+ i 2)) #\{)
                               (let ((close (or (position #\} pattern :start i) n)))
                                 (when (case-property-p (subseq pattern (+ i 3) close))
                                   (return-from regex-case-fold-p nil))
                                 (setf i (1+ close))))
                              (t (incf i 3))))
                       ((and (char= c #\() (< (+ i 3) n) (string= "?<" pattern :start2 (1+ i) :end2 (+ i 3))
                             (not (find (char pattern (+ i 3)) "=!")))
                        ;; a named group: its name is no capital letter
                        (setf i (past #\> i)))
                       ((upper-case-p c) (return-from regex-case-fold-p nil))
                       ((char= c #\[)
                        (multiple-value-bind (after name) (posix-class-at pattern i)
                          (when (member (string-left-trim "^" (or name "")) '("upper" "lower") :test #'equal)
                            (return-from regex-case-fold-p nil))
                          (setf i (or after (1+ i)))))
                       (t (incf i))))))
    t))

;;; The pattern language
;;;
;;; Patterns are read as PCRE reads them, by cl-ppcre, with what it lacks put
;;; in first (ENGINE-PATTERN): POSIX classes inside a set ([[:alpha:]], ASCII
;;; as PCRE has them), \Q...\E, \h, \v and \R, character properties (\p{L},
;;; \p{Greek}), named groups ((?<year>...), \k<year>) and possessive
;;; quantifiers (a++ as (?>a+)). An escape PCRE gives a meaning cl-ppcre does
;;; not know (\K, \G) is refused, never read as a plain letter.

(define-condition pattern-refused (error)
  ((words :initarg :words :reader pattern-refused-words))
  (:report (lambda (condition stream) (write-string (pattern-refused-words condition) stream)))
  (:documentation "Signalled for a pattern holding what the regular expressions here do
not support, or a name they do not know: WORDS say what, as the prompt shows
it (\"not supported: \\K\")."))

(setf (documentation 'pattern-refused-words 'function) "What a refused pattern holds that is not supported, in words.")

(defun refuse-pattern (control &rest arguments)
  "Signal PATTERN-REFUSED, its words CONTROL formatted with ARGUMENTS."
  (error 'pattern-refused :words (apply #'format nil control arguments)))

(defparameter *posix-classes*
  `(("alpha" . "A-Za-z") ("digit" . "0-9") ("alnum" . "0-9A-Za-z") ("upper" . "A-Z") ("lower" . "a-z")
    ("space" . ,(coerce (list #\Tab #\Newline (code-char 11) #\Page #\Return #\Space) 'string))
    ("blank" . ,(coerce (list #\Space #\Tab) 'string)) ("punct" . "!-/:-@\\[-`{-~") ("xdigit" . "0-9A-Fa-f")
    ("word" . "\\w") ("cntrl" . ,(format nil "~c-~c~c" (code-char 0) (code-char 31) (code-char 127)))
    ("print" . " -~") ("graph" . "!-~") ("ascii" . ,(format nil "~c-~c" (code-char 0) (code-char 127))))
  "The POSIX classes a set may hold, [:alpha:] in [[:alpha:]x], each (NAME .
MEMBERS), MEMBERS as written inside a set: ASCII, as PCRE has them.")

(defparameter *posix-negations* '(("digit" . "\\D") ("space" . "\\S") ("word" . "\\W"))
  "The POSIX classes whose negation, [:^digit:], is an escape of its own, each
(NAME . MEMBERS); the others are turned round by POSIX-COMPLEMENT.")

(sb-ext:defglobal **posix-complements** (make-hash-table :test 'equal :synchronized t)
  "The members of each negated POSIX class worked out so far (POSIX-COMPLEMENT), by name.")

(defun posix-complement (name)
  "The members, as written inside a set, of every character not in the POSIX
class NAME, for [:^NAME:]: the ASCII characters outside it and every
character past ASCII, as ranges; NIL for a name that is no class."
  (let ((members (cdr (assoc name *posix-classes* :test #'string=))))
    (when members
      (or (gethash name **posix-complements**)
          (setf (gethash name **posix-complements**)
                (let ((scanner (cl-ppcre:create-scanner (format nil "[~a]" members)))
                      (from nil))
                  (with-output-to-string (out)
                    (flet ((range (a b)
                             ;; each end as itself, the characters special in a set escaped
                             (dolist (code (if (= a b) (list a) (list a :to b)))
                               (if (eq code :to)
                                   (write-char #\- out)
                                   (let ((c (code-char code)))
                                     (when (find c "]\\^-[") (write-char #\\ out))
                                     (write-char c out))))))
                      (loop for code from 0 below 128
                            for in = (cl-ppcre:scan scanner (string (code-char code)))
                            do (cond ((and (not in) (null from)) (setf from code))
                                     ((and in from) (range from (1- code)) (setf from nil))))
                      (range (or from 128) (1- char-code-limit))))))))))

(defun posix-class-at (string i)
  "When a POSIX class, [:name:] or [:^name:], or a named pattern ([:ipv4:])
opens at index I of STRING, two values: the index after it and its name, ^
included; else NIL. A name is small letters, then digits too."
  (let ((n (length string)))
    (when (and (< (1+ i) n) (char= (char string i) #\[) (char= (char string (1+ i)) #\:))
      (let* ((from (if (and (< (+ i 2) n) (char= (char string (+ i 2)) #\^)) (+ i 3) (+ i 2)))
             (to (or (position-if-not (lambda (c) (or (char<= #\a c #\z) (char<= #\0 c #\9))) string :start from) n)))
        (when (and (> to from) (char<= #\a (char string from) #\z) (< (1+ to) n) (char= (char string to) #\:) (char= (char string (1+ to)) #\]))
          (values (+ to 2) (subseq string (+ i 2) to)))))))

(defun bracket-end (string i)
  "The index after the set opening at index I of STRING (a regular
expression), or the length of STRING when it is not closed. A ] right after
[ or [^ is a member, as are escaped characters and POSIX classes such as
[:alpha:]. Two more values: true when the set is closed by its ], and the
index of its first member, after the [ and any ^ and leading ]."
  (let* ((n (length string))
         (j (1+ i))
         (members (progn (when (and (< j n) (char= (char string j) #\^)) (incf j))
                         (when (and (< j n) (char= (char string j) #\])) (incf j))
                         j)))
    (loop while (and (< j n) (char/= (char string j) #\]))
          do (cond ((char= (char string j) #\\) (incf j 2))
                   ((posix-class-at string j) (setf j (posix-class-at string j)))
                   (t (incf j))))
    (values (min n (1+ j)) (< j n) members)))

(defparameter *horizontal-space*
  (coerce (append (list #\Tab #\Space) (mapcar #'code-char '(#xA0 #x1680 #x180E #x202F #x205F #x3000))
                  (list (code-char #x2000) #\- (code-char #x200A)))
          'string)
  "What \\h matches, as written inside a set: a tab, a space and the other
horizontal spaces of Unicode.")

(defparameter *vertical-space*
  (coerce (append (list #\Newline (code-char 11) #\Page #\Return) (mapcar #'code-char '(#x85 #x2028 #x2029))) 'string)
  "What \\v matches, as written inside a set: a line feed, a vertical tab, a
form feed, a carriage return and the line and paragraph separators.")

(defun hex-escape (regex i out)
  "Write to OUT the \\x escape at index I of REGEX as ENGINE-PATTERN gives it,
and return the index after it."
  (let* ((n (length regex))
         (brace (and (< (+ i 2) n) (char= (char regex (+ i 2)) #\{)))
         (close (and brace (position #\} regex :start (+ i 3))))
         (code (if brace
                   (and close (> close (+ i 3))
                        (every (lambda (h) (digit-char-p h 16)) (subseq regex (+ i 3) close))
                        (parse-integer regex :start (+ i 3) :end close :radix 16))
                   (and (<= (+ i 4) n)
                        (every (lambda (h) (digit-char-p h 16)) (subseq regex (+ i 2) (+ i 4)))
                        (parse-integer regex :start (+ i 2) :end (+ i 4) :radix 16)))))
    (cond ((and brace code (< code char-code-limit) (not (<= #xD800 code #xDFFF)))
           ;; a character by its code, escaped where the pattern would read it otherwise
           (let ((char (code-char code)))
             (unless (or (alphanumericp char) (>= code 128)) (write-char #\\ out))
             (write-char char out)
             (1+ close)))
          ((and (not brace) code (>= code #x80))
           (write-char (code-char (+ #xDC00 code)) out)
           (+ i 4))
          (t (write-string "\\x" out) (+ i 2)))))

(defun escape-for-engine (regex i out in-set)
  "Write to OUT the escape at index I of REGEX, inside a set when IN-SET, as
ENGINE-PATTERN gives it, and return the index after it."
  (let ((n (length regex)) (d (char regex (1+ i))))
    (case d
      (#\x (hex-escape regex i out))
      (#\Q
       ;; the text as it is, up to \E or the end
       (let ((to (or (search "\\E" regex :start2 (+ i 2)) n)))
         (loop for k from (+ i 2) below to
               for c = (char regex k)
               do (unless (alphanumericp c) (write-char #\\ out))
                  (write-char c out))
         (min n (+ to 2))))
      (#\E (+ i 2))
      ((#\h #\v #\H #\V)
       (let ((members (if (char-equal d #\h) *horizontal-space* *vertical-space*)))
         (cond ((lower-case-p d) (format out (if in-set "~a" "[~a]") members))
               (in-set (refuse-pattern "not supported: \\~c in a set" d))
               (t (format out "[^~a]" members))))
       (+ i 2))
      (#\R
       (when in-set (refuse-pattern "not supported: \\R in a set"))
       (format out "(?:\\r\\n|[~a])" *vertical-space*)
       (+ i 2))
      ((#\p #\P)
       (cond ((and (< (+ i 2) n) (char/= (char regex (+ i 2)) #\{))
              (format out "\\~c{~c}" d (char regex (+ i 2)))
              (+ i 3))
             (t (write-char #\\ out) (write-char d out) (+ i 2))))
      (#\k
       (unless (and (not in-set) (< (+ i 2) n) (char= (char regex (+ i 2)) #\<))
         (refuse-pattern "not supported: \\k without <name>"))
       (write-string "\\k" out)
       (+ i 2))
      (#\c
       ;; a control character: \c and the character after it, whatever it is
       (write-string regex out :start i :end (min n (+ i 3)))
       (min n (+ i 3)))
      (t
       (when (and (alpha-char-p d) (< (char-code d) 128)
                  (not (find d (if in-set "abdDefnrsStwW" "abBdDefnrsStwWAzZ"))))
         (refuse-pattern "not supported~:[~; in a set~]: \\~c" in-set d))
       (write-char #\\ out)
       (write-char d out)
       (+ i 2)))))

(defun quantifier-end (regex i)
  "The index after the quantifier opening at index I of REGEX (*, +, ?,
{n}, {n,} or {n,m}), or NIL when none opens there."
  (let ((n (length regex)))
    (case (char regex i)
      ((#\* #\+ #\?) (1+ i))
      (#\{ (let* ((first-end (or (position-if-not #'digit-char-p regex :start (1+ i)) n))
                  (comma (and (> first-end (1+ i)) (< first-end n) (char= (char regex first-end) #\,)))
                  (close (if comma (or (position-if-not #'digit-char-p regex :start (1+ first-end)) n) first-end)))
             (and (> first-end (1+ i)) (< close n) (char= (char regex close) #\}) (1+ close))))
      (t nil))))

(defun quoted-last-length (regex i)
  "How many characters ENGINE-PATTERN writes for the last character quoted by
the \\Q opening at index I of REGEX: 2 when it is escaped, 1 when not, NIL
when \\Q...\\E quotes nothing."
  (let ((to (or (search "\\E" regex :start2 (+ i 2)) (length regex))))
    (when (> to (+ i 2))
      (if (alphanumericp (char regex (1- to))) 1 2))))

(defun engine-pattern (regex)
  "REGEX as cl-ppcre is given it: a named pattern alone in its brackets
\([[:ipv4:]], *NAMED-PATTERNS*) as the group it stands for, a date with its
bounds ([[:date<2026-07-01:]], DATE-PATTERN-AT) likewise, POSIX classes in a
set ([[:alpha:]]) as the ASCII characters PCRE gives them, \\Q...\\E as that text quoted, \\h and \\H,
\\v and \\V as sets of horizontal and vertical space, \\R as a line break of
any kind, \\pL as \\p{L}, a stray \\E as nothing, each \\xNN whose NN is 80
to FF as the octet NN standing on its own, not inside a character (the raw
byte the display shows as \\xNN), each \\x{H...} as the character of that
code, and a possessive quantifier (a++, a*+, a?+, a{2,5}+) as the atomic group
it stands for ((?>a+)); the rest as it is. Signals PATTERN-REFUSED for an
escape PCRE gives a meaning cl-ppcre does not know (\\K, \\G, \\X), for one
that cannot go in a set (\\R there), for a POSIX class it has no name for,
for [:alpha:] outside a set, and for a named pattern in a set with other
members or turned round."
  (let ((n (length regex)) (i 0) (set-end -1) (set-closed nil)
        (out-text (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        ;; where the output of what a quantifier would repeat begins, and of each group open
        (repeat-from nil) (groups '()))
    (with-output-to-string (out out-text)
      (loop while (< i n)
            do (let ((c (char regex i)) (in-set (< i set-end)) (start (fill-pointer out-text)))
                 (cond
                   ((and (char= c #\\) (< (1+ i) n))
                    (let ((d (char regex (1+ i))) (quoted (quoted-last-length regex i)))
                      (setf i (escape-for-engine regex i out in-set))
                      (unless (or in-set (char= d #\E))
                        (setf repeat-from (if (char= d #\Q)
                                       (and quoted (- (fill-pointer out-text) quoted))
                                       start)))))
                   (in-set
                    (multiple-value-bind (after name) (posix-class-at regex i)
                      (if (null after)
                          (progn (write-char c out) (incf i))
                          (let ((members (if (char= (char name 0) #\^)
                                             (or (cdr (assoc (subseq name 1) *posix-negations* :test #'string=))
                                                 (posix-complement (subseq name 1))
                                                 (and (named-pattern-p (subseq name 1))
                                                      (refuse-pattern "invalid: a named pattern is not turned round: [[:~a:]]" name))
                                                 (refuse-pattern "invalid: no POSIX class [:~a:]" (subseq name 1)))
                                             (or (cdr (assoc name *posix-classes* :test #'string=))
                                                 (and (named-pattern-p name)
                                                      (if set-closed
                                                          (refuse-pattern "invalid: [:~a:] stands alone in its brackets: [[:~a:]]" name name)
                                                          ;; [[:ipv4:] still being typed: a set not closed yet, as cl-ppcre says
                                                          "x"))
                                                 (refuse-pattern "invalid: no POSIX class [:~a:]" name)))))
                            (write-string members out)
                            (setf i after)))))
                   ((and (char= c #\[) (not in-set) (date-pattern-at regex i))
                    ;; a date, with its bounds or not: what it stands for, as a group
                    (multiple-value-bind (after pattern) (date-pattern-at regex i)
                      (format out "(?:~a)" pattern)
                      (setf repeat-from start
                            i after)))
                   ((and (char= c #\[) (not in-set)
                         (multiple-value-bind (after name) (posix-class-at regex (1+ i))
                           (and after name (< after n) (char= (char regex after) #\]) (named-pattern-p name))))
                    ;; a named pattern alone in its brackets: what it stands for, as a group
                    (multiple-value-bind (after name) (posix-class-at regex (1+ i))
                      (format out "(?:~a)" (named-pattern name))
                      (setf repeat-from start
                            i (1+ after))))
                   ((char= c #\[)
                    (let ((after (posix-class-at regex i)))
                      (when after
                        (refuse-pattern "invalid: ~a goes inside a set: [~:*~a]" (subseq regex i after))))
                    ;; the set's opening, ^ and a leading ] as they are; its members by the rules of a set
                    (multiple-value-bind (end closed members) (bracket-end regex i)
                      (setf set-end (1- end)
                            set-closed closed
                            repeat-from start)
                      (write-string regex out :start i :end members)
                      (setf i members)))
                   ((= i set-end) (write-char c out) (incf i))
                   ((and repeat-from (quantifier-end regex i))
                    (let ((end (quantifier-end regex i)))
                      (write-string regex out :start i :end end)
                      (cond ((and (< end n) (char= (char regex end) #\+))
                             ;; possessive: what it repeats, as often as it can, never given back
                             (let ((repeated (subseq out-text repeat-from)))
                               (setf (fill-pointer out-text) repeat-from)
                               (format out "(?>~a)" repeated))
                             (setf i (1+ end)))
                            ((and (< end n) (char= (char regex end) #\?))
                             (write-char #\? out)
                             (setf i (1+ end)))
                            (t (setf i end)))
                      (setf repeat-from nil)))
                   ((and (char= c #\() (< (+ i 2) n) (string= "?#" regex :start2 (1+ i) :end2 (+ i 3)))
                    ;; a comment, as it is
                    (let ((end (min n (1+ (or (position #\) regex :start i) n)))))
                      (write-string regex out :start i :end end)
                      (setf i end)))
                   ((char= c #\()
                    (push start groups)
                    (setf repeat-from nil)
                    (write-char c out)
                    (incf i))
                   ((char= c #\))
                    (setf repeat-from (pop groups))
                    (write-char c out)
                    (incf i))
                   ((char= c #\|)
                    (setf repeat-from nil)
                    (write-char c out)
                    (incf i))
                   (t (setf repeat-from start)
                      (write-char c out)
                      (incf i))))))
    (coerce out-text 'simple-string)))

(sb-ext:defglobal **scripts** nil
  "The names of the Unicode scripts, as keywords, once looked for.")

(defun unicode-scripts ()
  "The names of the Unicode scripts, as keywords: every character's, found once."
  (or **scripts**
      (setf **scripts**
            (let ((names '()))
              (dotimes (code char-code-limit names)
                (let ((char (code-char code)))
                  (when char (pushnew (sb-unicode:script char) names))))))))

(defparameter *property-names*
  '(("letter" . "l") ("mark" . "m") ("number" . "n") ("punctuation" . "p") ("symbol" . "s") ("separator" . "z")
    ("other" . "c") ("uppercaseletter" . "lu") ("lowercaseletter" . "ll") ("titlecaseletter" . "lt")
    ("decimalnumber" . "nd") ("spaceseparator" . "zs") ("casedletter" . "lc") ("l&" . "lc"))
  "Long names of character properties and the short ones they stand for.")

(defparameter *general-categories*
  '("lu" "ll" "lt" "lm" "lo" "mn" "mc" "me" "nd" "nl" "no" "pc" "pd" "ps" "pe" "pi" "pf"
    "po" "sm" "sc" "sk" "so" "zs" "zl" "zp" "cc" "cf" "cs" "co" "cn")
  "The general categories of Unicode, by their two letters.")

(defun property-key (name)
  "NAME, a character property as \\p{NAME} gives it, as CHARACTER-PROPERTY
looks it up: in small letters, without spaces, - and _ or an Is before it, a
long name of a general category as its short one (lu for Uppercase_Letter)."
  (let* ((key (string-downcase (remove-if (lambda (c) (find c " _-")) name)))
         (key (if (and (> (length key) 2) (string= "is" key :end2 2)) (subseq key 2) key)))
    (or (cdr (assoc key *property-names* :test #'string=)) key)))

(defun case-property-p (name)
  "True when the character property NAME names a case: upper-case, lower-case
or title-case letters (Lu, Ll, Lt)."
  (and (member (property-key name) '("lu" "ll" "lt") :test #'string=) t))

(defun character-property (name)
  "A function true of the characters having the property NAME, as \\p{NAME}
reads it: a general category of Unicode by its letter or two (L, Lu, Nd, P,
Zs, LC for any cased letter) or long name (Letter, Uppercase_Letter), a
script (Greek, Latin, Han), Alphabetic, White_Space, Any or ASCII; case,
spaces, - and _ do not matter, and Is before a name is allowed. Signals
PATTERN-REFUSED for a name it does not know."
  (let ((key (property-key name)))
    (cond ((string= key "any") (constantly t))
          ((string= key "ascii") (lambda (c) (< (char-code c) 128)))
          ((string= key "alphabetic") #'sb-unicode:alphabetic-p)
          ((string= key "whitespace") #'sb-unicode:whitespace-p)
          ((string= key "lc") (lambda (c) (member (sb-unicode:general-category c) '(:lu :ll :lt))))
          ((and (= (length key) 1) (find (char key 0) "lmnpszc"))
           (let ((letter (char-upcase (char key 0))))
             (lambda (c) (char= letter (char (symbol-name (sb-unicode:general-category c)) 0)))))
          ((member key *general-categories* :test #'string=)
           (let ((category (intern (string-upcase key) :keyword)))
             (lambda (c) (eq category (sb-unicode:general-category c)))))
          (t (let ((script (find key (unicode-scripts) :key (lambda (s) (remove #\_ (string-downcase (symbol-name s))))
                                                       :test #'string=)))
               (unless script (refuse-pattern "invalid: no character property \\p{~a}" name))
               (lambda (c) (eq script (sb-unicode:script c))))))))

(defmacro with-pattern-syntax (&body body)
  "Run BODY with cl-ppcre reading patterns as ENGINE-PATTERN leaves them:
named groups allowed, \\p{...} read by CHARACTER-PROPERTY; and with the
extended mode that (?x) turns on bound here, so that one pattern's (?x),
read by CL-PPCRE:PARSE-STRING, is not taken up by every pattern read after
it (which would then pass over their spaces and take # for a note)."
  `(let ((cl-ppcre:*allow-named-registers* t)
         (cl-ppcre:*property-resolver* #'character-property)
         (cl-ppcre::*extended-mode-p* nil))
     ,@body))

(defun tuatara-user:define-named-pattern (name words pattern)
  "Make [[:NAME:]] stand for PATTERN in every search, as [[:ipv4:]] does:
NAME, small letters, then digits too; WORDS, what it finds, for you to read;
PATTERN, written as any search's pattern is, other named patterns in it too.
It is put in as a group, so that [[:NAME:]]+ repeats it, and may hold no
group that counts, so that \\1 stays the first group a search writes: write
\(?:...) for a group. It ignores case when the search does, as any part of a
pattern does. Defining NAME again replaces it; a POSIX class's name or a
named pattern of Tuatara's is refused. The starter init file has an
example, commented out."
  (unless (and (stringp name) (cl-ppcre:scan "^[a-z][a-z0-9]*$" name))
    (error "A named pattern's name is small letters, then digits too: not ~s." name))
  (when (or (assoc name *posix-classes* :test #'string=) (assoc name *named-patterns* :test #'string=))
    (error "[[:~a:]] is Tuatara's already: give yours another name." name))
  (unless (stringp pattern)
    (error "The pattern of [[:~a:]] is a string, not ~s." name pattern))
  (let ((before (multiple-value-list (gethash name **own-named-patterns**))))
    (forget-compiled-regexes)
    (setf (gethash name **own-named-patterns**) (cons (princ-to-string words) pattern))
    (handler-case
        (with-pattern-syntax
          (let ((tree (cl-ppcre:parse-string (engine-pattern (format nil "[[:~a:]]" name)))))
            (labels ((counts-p (x)
                       ;; a group that counts, or one counted again, but not a filter's mark
                       (and (consp x)
                            (or (eq (first x) :register)
                                (eq (first x) :back-reference)
                                (and (eq (first x) :named-register)
                                     (not (uiop:string-prefix-p *filter-mark* (second x)))))))
                     (walk (x) (and (consp x) (or (counts-p x) (some #'walk x)))))
              (when (walk tree)
                (refuse-pattern "holds a group that counts: write (?:...)")))
            (compile-regex (format nil "[[:~a:]]" name))))
      (error (e)
        (if (second before)
            (setf (gethash name **own-named-patterns**) (first before))
            (remhash name **own-named-patterns**))
        (forget-compiled-regexes)
        (error "[[:~a:]] is not defined: ~a" name e))))
  name)

(defun literal-runs (tree)
  "Three values for a cl-ppcre parse TREE: the text of every match when TREE
matches exactly one text (\"\" for an assertion, which matches none), else NIL;
a list of texts each of which every match contains; and the text every match
starts with, or NIL. Forms not understood contribute nothing, which is always
safe."
  (flet ((zero-width-p (x)
           (or (member x '(:start-anchor :end-anchor :modeless-start-anchor :modeless-end-anchor
                           :modeless-end-anchor-no-newline :word-boundary :non-word-boundary :void))
               (and (consp x) (member (first x) '(:positive-lookahead :negative-lookahead
                                                  :positive-lookbehind :negative-lookbehind))))))
    (cond ((stringp tree) (values tree (list tree) tree))
          ((characterp tree) (let ((s (string tree))) (values s (list s) s)))
          ((zero-width-p tree) (values "" '() ""))
          ((atom tree) (values nil '() nil))
          (t (case (first tree)
               ((:register :group :standalone) (literal-runs (car (last tree))))
               (:named-register
                (if (uiop:string-prefix-p *filter-mark* (second tree))
                    ;; text a filter reads: nothing known of it
                    (values nil '() nil)
                    (literal-runs (third tree))))
               (:sequence
                (let ((whole "") (runs '()) (current "") (prefix "") (prefix-open t))
                  (dolist (item (rest tree))
                    (multiple-value-bind (w r p) (literal-runs item)
                      (cond (w (setf current (concatenate 'string current w))
                               (when whole (setf whole (concatenate 'string whole w)))
                               (when prefix-open (setf prefix (concatenate 'string prefix w))))
                            (t (when (plusp (length current)) (push current runs))
                               (when prefix-open
                                 (setf prefix (concatenate 'string prefix (or p "")) prefix-open nil))
                               (setf current "" whole nil runs (append r runs))))))
                  (when (plusp (length current)) (push current runs))
                  (values whole runs prefix)))
               ((:greedy-repetition :non-greedy-repetition)
                (destructuring-bind (min max item) (rest tree)
                  (if (and (integerp min) (plusp min))
                      (multiple-value-bind (w r p) (literal-runs item)
                        (cond ((and w (eql min max))
                               (let ((text (apply #'concatenate 'string (make-list min :initial-element w))))
                                 (values text (list text) text)))
                              (w (values nil (list w) w))
                              (t (values nil r p))))
                      (values nil '() nil))))
               (t (values nil '() nil)))))))

(defun required-literal (regex)
  "Two values: the text every match of the pattern REGEX contains, or NIL when
there is none, the pattern sets its own flags or the text holds raw bytes'
characters; and true when every match starts with that text. A
prefix of three octets or more, or as long as the longest text, is preferred."
  (let ((tree (cl-ppcre:parse-string regex)))
    (labels ((has-flags (x) (or (eq x :flags) (and (consp x) (or (has-flags (car x)) (has-flags (cdr x))))))
             (octets (text) (tuatara.text:string-octet-length text)))
      (unless (has-flags tree)
        (multiple-value-bind (whole runs prefix) (literal-runs tree)
          (declare (ignore whole))
          (let ((best nil) (best-length 0))
            (dolist (run runs)
              (when (> (octets run) best-length) (setf best run best-length (octets run))))
            (multiple-value-bind (text prefix-p)
                (if (and prefix (plusp (length prefix)) (>= (octets prefix) (min 3 best-length)))
                    (values prefix t)
                    (values best nil))
              (when (and text (notany (lambda (c) (tuatara.text:raw-byte-code-p (char-code c))) text))
                (values text prefix-p)))))))))

;;; Shapes: what every match holds when no text does, a run of places each of
;;; which is one of a few octets ([0-9A-Fa-f]{8}- in a UUID), found at the speed
;;; of a loop over the octets rather than by trying the pattern everywhere.

(defun octet-set (&rest codes)
  "A set of octets, a bit for each of the 256, holding CODES."
  (let ((set (make-array 256 :element-type 'bit :initial-element 0)))
    (dolist (code codes set) (setf (sbit set code) 1))))

(defun char-octet-sets (char case-fold)
  "The sets of octets (OCTET-SET) CHAR is written in, one for each of its
octets, an ASCII letter's other case in its set when CASE-FOLD; NIL when CHAR
cannot be told by its octets: a newline, which a CR LF text writes otherwise,
a raw octet's character, or a letter beyond ASCII whose case is folded."
  (let ((code (char-code char)))
    (cond ((or (char= char #\Newline) (tuatara.text:raw-byte-code-p code)) nil)
          ((< code 128)
           (list (if (and case-fold (both-case-p char))
                     (octet-set (char-code (char-upcase char)) (char-code (char-downcase char)))
                     (octet-set code))))
          ((and case-fold (both-case-p char)) nil)
          (t (map 'list #'octet-set (tuatara.text:string-octets (string char)))))))

(defun class-octet-set (items case-fold)
  "The set of octets (OCTET-SET) a character class of ITEMS (a cl-ppcre parse
tree's) matches, when all it matches is ASCII other than a newline; else NIL."
  (let ((set (octet-set)))
    (flet ((add (char)
             (let ((sets (char-octet-sets char case-fold)))
               (unless (and sets (null (rest sets)) (< (char-code char) 128))
                 (return-from class-octet-set nil))
               (bit-ior set (first sets) set))))
      (dolist (item items set)
        (cond ((characterp item) (add item))
              ((and (consp item) (eq (first item) :range) (< (char-code (third item)) 128))
               (loop for code from (char-code (second item)) to (char-code (third item))
                     do (add (code-char code))))
              (t (return nil)))))))

(defun shapes-union (shapes)
  "One shape from SHAPES, each a list of octet sets: at each place, as far as
the shortest, every octet any of them allows there."
  (let ((length (reduce #'min shapes :key #'length)))
    (loop for k below length
          collect (let ((set (octet-set)))
                    (dolist (shape shapes set) (bit-ior set (nth k shape) set))))))

(defun shape-runs (tree case-fold)
  "Three values for a cl-ppcre parse TREE, as LITERAL-RUNS gives them for
text, each shape a list of octet sets, a set for each octet (CHAR-OCTET-SETS):
the shape of every match when every match is of one length and shape, else
:NONE; a list of shapes each of which every match holds; and the shape every
match starts with, perhaps empty. A positive lookahead at the start gives the
shape the text there starts with. Forms not understood contribute nothing,
which is always safe."
  (flet ((zero-width-p (x)
           (or (member x '(:start-anchor :end-anchor :modeless-start-anchor :modeless-end-anchor
                           :modeless-end-anchor-no-newline :word-boundary :non-word-boundary :void))
               (and (consp x) (member (first x) '(:negative-lookahead :positive-lookbehind :negative-lookbehind)))))
         (fixed (sets) (if sets (values sets (list sets) sets) (values :none '() '()))))
    (cond ((characterp tree) (fixed (char-octet-sets tree case-fold)))
          ((stringp tree) (shape-runs (cons :sequence (coerce tree 'list)) case-fold))
          ((zero-width-p tree) (values '() '() '()))
          ((atom tree) (values :none '() '()))
          (t (case (first tree)
               (:char-class (let ((set (class-octet-set (rest tree) case-fold)))
                              (if set (fixed (list set)) (values :none '() '()))))
               ((:register :group :standalone) (shape-runs (car (last tree)) case-fold))
               (:named-register
                (if (uiop:string-prefix-p *filter-mark* (second tree))
                    (values :none '() '())
                    (shape-runs (third tree) case-fold)))
               (:positive-lookahead
                ;; no text of the match, but what the text from here starts with
                (multiple-value-bind (w r p) (shape-runs (second tree) case-fold)
                  (declare (ignore w r))
                  (values '() (and p (list p)) '() p)))
               (:sequence
                (let ((whole '()) (runs '()) (current '()) (prefix '()) (prefix-open t) (ahead nil))
                  (dolist (item (rest tree))
                    (multiple-value-bind (w r p looked) (shape-runs item case-fold)
                      (when (and looked prefix-open (null prefix) (null ahead)) (setf ahead looked))
                      (cond ((listp w)
                             (setf current (append current w))
                             (unless (eq whole :none) (setf whole (append whole w)))
                             (when prefix-open (setf prefix (append prefix w)))
                             (setf runs (append r runs)))
                            (t (when current (push current runs))
                               (when prefix-open (setf prefix (append prefix p) prefix-open nil))
                               (setf current '() whole :none runs (append r runs))))))
                  (when current (push current runs))
                  (values whole runs (if (and ahead (> (shape-bits ahead) (shape-bits prefix))) ahead prefix))))
               (:alternation
                (let ((branches (mapcar (lambda (branch) (multiple-value-list (shape-runs branch case-fold)))
                                        (rest tree))))
                  (let ((wholes (mapcar #'first branches)) (prefixes (mapcar #'third branches)))
                    (let ((whole (if (and (notany (lambda (w) (eq w :none)) wholes)
                                          (= 1 (length (remove-duplicates (mapcar #'length wholes)))))
                                     (shapes-union wholes)
                                     :none))
                          (prefix (if (every #'identity prefixes) (shapes-union prefixes) '())))
                      (values whole (and prefix (list prefix)) prefix)))))
               ((:greedy-repetition :non-greedy-repetition)
                (destructuring-bind (min max item) (rest tree)
                  (if (and (integerp min) (plusp min))
                      (multiple-value-bind (w r p) (shape-runs item case-fold)
                        (cond ((and (listp w) w (eql min max))
                               (fixed (loop repeat min append w)))
                              ((and (listp w) w)
                               (let ((least (loop repeat min append w)))
                                 (values :none (list least) least)))
                              (t (values :none r p))))
                      (values :none '() '()))))
               (t (values :none '() '())))))))

(defun shape-bits (shape)
  "How much SHAPE, a list of octet sets, tells of a text: for each place, the
bits of the octets it rules out, eight for one octet allowed."
  (loop for set in shape sum (log (/ 256 (max 1 (count 1 set))) 2)))

(defun required-shape (tree case-fold)
  "Two values: the shape (SHAPE-RUNS) every match of the parse TREE holds that
tells most of a text, as a vector of octet sets, or NIL; and true when every
match starts with it, or the text at a match's start does. A prefix telling
nearly as much is preferred."
  (multiple-value-bind (whole runs prefix) (shape-runs tree case-fold)
    (declare (ignore whole))
    (let ((best (first (sort (copy-list runs) #'> :key #'shape-bits))))
      (multiple-value-bind (shape prefix-p)
          (if (and prefix (>= (shape-bits prefix) (* 3/4 (if best (shape-bits best) 0))))
              (values prefix t)
              (values best nil))
        (when shape (values (coerce shape 'simple-vector) prefix-p))))))

(defun class-item-covers-newline (item surely)
  "Whether the char-class ITEM of a parse tree includes a character that
ends a line: the newline or the carriage return. Items not understood count
as including one unless SURELY is true."
  (cond ((characterp item) (or (char= item #\Newline) (char= item #\Return)))
        ((and (consp item) (eq (first item) :range))
         (or (char<= (second item) #\Newline (third item)) (char<= (second item) #\Return (third item))))
        ((member item '(:whitespace-char-class :non-digit-class :non-word-char-class)) t)
        ((member item '(:digit-class :word-char-class :non-whitespace-char-class)) nil)
        (t (not surely))))

(defparameter *filters-crossing-lines* '("base32" "base64")
  "The filters a named pattern may hold (*PATTERN-FILTERS*) whose matches
may go on over lines.")

(defun filter-crosses-lines-p (name)
  "True when NAME, a group's name in a parse tree, marks a filter whose
matches may go on over lines (*FILTERS-CROSSING-LINES*)."
  (and (stringp name) (uiop:string-prefix-p *filter-mark* name)
       (member (subseq name (length *filter-mark*)) *filters-crossing-lines* :test #'string=)
       t))

(defun crosses-lines-p (tree)
  "True unless every match of the cl-ppcre parse TREE, and all its lookaheads
inspect, surely lies within one line (with . matching neither a newline nor
a carriage return, as COMPILE-REGEX makes it). Forms not understood count as
crossing."
  (labels ((walk (x)
             (cond ((stringp x) (or (find #\Newline x) (find #\Return x)))
                   ((characterp x) (or (char= x #\Newline) (char= x #\Return)))
                   ((member x '(:start-anchor :end-anchor :modeless-start-anchor :modeless-end-anchor
                                :modeless-end-anchor-no-newline :word-boundary :non-word-boundary :void
                                :everything :digit-class :word-char-class :non-whitespace-char-class))
                    nil)
                   ((atom x) t)
                   (t (case (first x)
                        ((:sequence :alternation) (some #'walk (rest x)))
                        ((:group :register :standalone :positive-lookahead :negative-lookahead) (walk (car (last x))))
                        (:named-register (or (filter-crosses-lines-p (second x)) (walk (third x))))
                        ((:greedy-repetition :non-greedy-repetition) (walk (fourth x)))
                        ((:positive-lookbehind :negative-lookbehind) nil) ; before the match, in the context
                        (:property (or (funcall (character-property (second x)) #\Newline)
                                       (funcall (character-property (second x)) #\Return)))
                        (:inverted-property (not (and (funcall (character-property (second x)) #\Newline)
                                                      (funcall (character-property (second x)) #\Return))))
                        (:char-class (some (lambda (item) (class-item-covers-newline item nil)) (rest x)))
                        (:inverted-char-class (notany (lambda (item) (class-item-covers-newline item t)) (rest x)))
                        (t t))))))
    (and (walk tree) t)))

(defun tree-has (tree &rest keywords)
  "True when one of KEYWORDS occurs anywhere in the parse TREE."
  (labels ((walk (x) (or (and (symbolp x) (member x keywords)) (and (consp x) (or (walk (car x)) (walk (cdr x)))))))
    (and (walk tree) t)))

(defstruct (regex-traits (:constructor make-regex-traits (&key needle shape fold prefix line-bound end-anchored reach plain))
                         (:copier nil))
  "What COMPILE-REGEX learnt about a pattern for scanning a rope: NEEDLE, the
octets of text every match holds (ASCII case folded when FOLD), or NIL; SHAPE,
when it tells more than NEEDLE, a vector of octet sets (REQUIRED-SHAPE) every
match holds in its place, or NIL; PREFIX true when every match starts with
the one or the other; LINE-BOUND true when every match lies
within a line; END-ANCHORED true when the pattern uses \\z or \\Z, which only
match at the end of the text; REACH, the octets past the stretch a match starts
in that a scan looks, +REGEX-OVERLAP+ or for a thorough scanner
*REGEX-LONGEST-MATCH*."
  (needle nil) (shape nil) (fold nil) (prefix nil) (line-bound nil) (end-anchored nil) (reach +regex-overlap+) (plain nil))

(setf (documentation 'make-regex-traits 'function) "Return the REGEX-TRAITS of a pattern from what its keys say."
      (documentation 'regex-traits-p 'function) "True when OBJECT is a REGEX-TRAITS."
      (documentation 'regex-traits-needle 'function) "The octets of text every match of the pattern holds, or NIL."
      (documentation 'regex-traits-shape 'function) "The octet sets every match of the pattern holds in their place, or NIL."
      (documentation 'regex-traits-fold 'function) "True when the needle is ASCII case folded and is looked for so."
      (documentation 'regex-traits-prefix 'function) "True when every match of the pattern starts with its needle or shape."
      (documentation 'regex-traits-line-bound 'function) "True when every match of the pattern lies within a line."
      (documentation 'regex-traits-end-anchored 'function) "True when the pattern matches only at the end of the text (\\z, \\Z)."
      (documentation 'regex-traits-reach 'function) "Octets past the stretch a match starts in that a scan for the pattern looks."
      (documentation 'regex-traits-plain 'function) "The pattern's scanner for text without a carriage return, or NIL when it is the same.")

(defvar *regex-thorough* nil
  "True while a thorough command runs (regex-search-forward-thorough and the
others): the scanners COMPILE-REGEX makes then find every match of up to
*REGEX-LONGEST-MATCH* octets whole, however many lines it spans, where the
others may miss one that needs text more than +REGEX-OVERLAP+ octets past the
stretch of +SEARCH-CHUNK+ it starts in. About twice as slow for a pattern whose
matches may cross lines; the same for one kept within a line.")

(sb-ext:defglobal **regex-traits** (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The REGEX-TRAITS of each scanner COMPILE-REGEX made.")

(defun line-tree (tree)
  "TREE, a cl-ppcre parse tree, with its lines ending as every text's do
\(TUATARA.TEXT:LINE-END-OCTET-P), as the Rust regex crate reads a pattern in
its CRLF mode: ^ matches after a newline and after a carriage return no
newline follows, $ before a newline no carriage return comes before and
before a carriage return, neither between a carriage return and its
newline; . matches neither a carriage return nor a newline, but in
single-line mode ((?s)). Where (?-m) makes ^ and $ the text's ends, they are
left as they are. Plain text stays plain: a tree that is a string alone is
given as a sequence."
  (labels ((mode-after (flags mode on off)
             (cond ((member on flags) t)
                   ((member off flags) nil)
                   (t mode)))
           (walk (node single multi)
             (cond ((and multi (eq node :end-anchor))
                    '(:alternation (:positive-lookahead #\Return)
                      (:sequence (:negative-lookbehind #\Return) (:positive-lookahead #\Newline))
                      :modeless-end-anchor-no-newline))
                   ((and multi (eq node :start-anchor))
                    '(:alternation :modeless-start-anchor (:positive-lookbehind #\Newline)
                      (:sequence (:positive-lookbehind #\Return) (:negative-lookahead #\Newline))))
                   ((eq node :everything) (if single node '(:inverted-char-class #\Return #\Newline)))
                   ((atom node) node)
                   ((eq (first node) :sequence)
                    ;; inline flags change the mode for the rest of the sequence
                    (let ((single single) (multi multi))
                      (cons :sequence
                            (loop for child in (rest node)
                                  collect (if (and (consp child) (eq (first child) :flags))
                                              (progn (setf single (mode-after (rest child) single :single-line-mode-p :not-single-line-mode-p)
                                                           multi (mode-after (rest child) multi :multi-line-mode-p :not-multi-line-mode-p))
                                                     child)
                                              (walk child single multi))))))
                   ((eq (first node) :group)
                    (let ((flags (butlast (rest node))))
                      (append (list :group) flags
                              (list (walk (car (last node))
                                          (mode-after flags single :single-line-mode-p :not-single-line-mode-p)
                                          (mode-after flags multi :multi-line-mode-p :not-multi-line-mode-p))))))
                   ((member (first node) '(:char-class :inverted-char-class :property :inverted-property :back-reference :filter))
                    node)
                   (t (cons (first node) (mapcar (lambda (child) (walk child single multi)) (rest node)))))))
    (let ((walked (walk tree nil t)))
      (if (stringp walked) (list :sequence walked) walked))))

(defun tree-max-octets (tree)
  "The most octets a match of TREE, a cl-ppcre parse tree, and the text it
looks at past its end can take, counting four for each character and 90
characters for a filter's mark (*FILTER-MARK*); NIL when nothing bounds it (a
repetition with no most, a back-reference)."
  (labels ((walk (node)
             (cond ((characterp node) 4)
                   ((stringp node) (* 4 (length node)))
                   ((member node '(:everything :digit-class :word-char-class :whitespace-char-class
                                   :non-digit-class :non-word-char-class :non-whitespace-char-class))
                    4)
                   ;; anchors, boundaries, flags and :void take no text
                   ((symbolp node) 0)
                   (t (case (first node)
                        (:sequence (let ((total 0))
                                     (dolist (child (rest node) total)
                                       (let ((most (walk child))) (if most (incf total most) (return nil))))))
                        (:alternation (let ((largest 0))
                                        (dolist (child (rest node) largest)
                                          (let ((most (walk child))) (if most (setf largest (max largest most)) (return nil))))))
                        (:named-register
                         (cond ((filter-crosses-lines-p (second node)) nil)
                               ;; text a filter reads: at most a Bitcoin address's 90 characters
                               ((uiop:string-prefix-p *filter-mark* (second node)) (* 4 90))
                               (t (walk (car (last node))))))
                        ((:group :register :standalone) (walk (car (last node))))
                        ((:greedy-repetition :non-greedy-repetition)
                         (destructuring-bind (least most body) (rest node)
                           (declare (ignore least))
                           (let ((one (walk body))) (and most one (* most one)))))
                        ((:positive-lookahead :negative-lookahead) (walk (second node)))
                        ((:positive-lookbehind :negative-lookbehind :flags) 0)
                        ((:char-class :inverted-char-class :property :inverted-property) 4)
                        (t nil))))))
    (walk tree)))

(sb-ext:defglobal **compiled-regexes** (make-hash-table :test 'equal :weakness :value :synchronized t)
  "The scanners COMPILE-REGEX made that are still in use, by the pattern and
how it was compiled (case folding, CRLF, thoroughness): the same pattern is
compiled once while its scanner is kept.")

(sb-ext:defglobal **recent-regexes** (make-array 4 :initial-element nil)
  "The scanners COMPILE-REGEX made last, kept so that a pattern compiled again
soon after, as a prompt is drawn again, is not compiled anew.")

(sb-ext:defglobal **recent-regex-count** 0
  "How many scanners COMPILE-REGEX has made, which place of **RECENT-REGEXES** the next goes in.")
(declaim (type fixnum **recent-regex-count**))

(defun forget-compiled-regexes ()
  "Compile every pattern anew from now on: what a named pattern stands for changed."
  (sb-ext:with-locked-hash-table (**compiled-regexes**)
    (clrhash **compiled-regexes**)
    (fill **recent-regexes** nil)))

(defun compile-regex (regex &key (case-fold (regex-case-fold-p regex)) crlf (thorough *regex-thorough*))
  "A cl-ppcre scanner for REGEX (PCRE-style syntax) in which ^ and $ match
at line boundaries. CASE-FOLD ignores case; by default when REGEX has no
upper-case letter outside an escape. What cl-ppcre lacks is put in first
\(ENGINE-PATTERN): named patterns ([[:ipv4:]], [[:creditcard:]], whose card
numbers must pass the Luhn check), POSIX classes in a set, \\Q...\\E, \\h, \\v, \\R,
\\xNN from 80 to FF the octet NN standing on its own; named groups and
character properties (\\p{L}) are read too (WITH-PATTERN-SYNTAX). Lines end
as they do in every text: ^ and $ match at a newline, at a carriage return
and a newline, and at a carriage return alone, and . matches neither a
carriage return nor a newline (LINE-TREE); CRLF changes nothing. THOROUGH, by
default *REGEX-THOROUGH*, has a scan find every match of up to
*REGEX-LONGEST-MATCH* octets whole, looking as far as the pattern can reach
(TREE-MAX-OCTETS). What the pattern allows a scan to skip (REGEX-TRAITS) is
registered with the scanner. The same REGEX compiled the same way gives the
same scanner while one is kept, or soon after. Signals PATTERN-REFUSED, or
cl-ppcre's syntax error, for a pattern that cannot be used (REGEX-PROBLEM
words why)."
  (declare (ignore crlf))
  (let ((key (list regex (and case-fold t) (and thorough t))))
    (or (gethash key **compiled-regexes**)
        (let ((scanner (with-pattern-syntax
                         (compile-engine-pattern (engine-pattern regex) case-fold thorough))))
          (setf (first key) (copy-seq regex))
          (sb-ext:with-locked-hash-table (**compiled-regexes**)
            (setf (gethash key **compiled-regexes**) scanner
                  (svref **recent-regexes** (mod (incf **recent-regex-count**) (length **recent-regexes**)))
                  scanner))))))

(defun compile-engine-pattern (regex case-fold thorough)
  "COMPILE-REGEX's work on REGEX, as ENGINE-PATTERN gives it."
  (let* ((tree (ignore-errors (cl-ppcre:parse-string regex)))
         (filtered (and tree (search *filter-mark* regex)))
         (lines (and tree (line-tree tree)))
         (scanner (let ((cl-ppcre:*use-bmh-matchers* t))
                    ;; constant text found by skipping (Boyer-Moore-Horspool): three times faster
                    ;; where matches are many and case is folded
                    (cl-ppcre:create-scanner (cond (filtered
                                                    ;; card numbers: read by code as they are matched
                                                    (filter-tree lines))
                                                   (tree lines)
                                                   (t regex))
                                             :case-insensitive-mode case-fold :multi-line-mode t)))
         ;; for text without a carriage return, where its lines end at newlines alone, the
         ;; pattern as written does the same, and quicker
         (plain (and tree (not (equal (if (stringp tree) (list :sequence tree) tree) lines))
                     (let ((cl-ppcre:*use-bmh-matchers* t))
                       (cl-ppcre:create-scanner (if filtered (filter-tree tree) regex)
                                                :case-insensitive-mode case-fold :multi-line-mode t)))))
    (when tree
      ;; (?i) and the like, and a group's own, (?i:a): what a match must hold, and
      ;; whether it stays within a line, cannot be read from the text then
      (let ((flags (tree-has tree :flags :case-insensitive-p :case-sensitive-p :multi-line-mode-p
                             :not-multi-line-mode-p :single-line-mode-p :not-single-line-mode-p)))
        (multiple-value-bind (literal prefix-p) (and (not flags) (ignore-errors (required-literal regex)))
          ;; octet folding knows ASCII only: letters beyond it keep the full scan
          (unless (or (not case-fold)
                      (every (lambda (c) (or (< (char-code c) 128) (not (both-case-p c)))) (or literal "")))
            (setf literal nil))
          (multiple-value-bind (shape shape-prefix-p) (and (not flags) (required-shape tree case-fold))
            ;; a shape where the text is none or too short to skip by, telling more than
            ;; it, and enough to be worth looking for
            (if (and shape (>= (shape-bits (coerce shape 'list)) 20)
                     (or (null literal) (< (tuatara.text:string-octet-length literal) 3))
                     (> (shape-bits (coerce shape 'list))
                        (if literal (* (if case-fold 7 8) (tuatara.text:string-octet-length literal)) 0)))
                (setf literal nil prefix-p shape-prefix-p)
                (setf shape nil))
          (setf (gethash scanner **regex-traits**)
                (let ((line-bound (and (not flags) (not (crosses-lines-p tree)))))
                  (make-regex-traits :needle (and literal (needle-octets literal case-fold)) :fold case-fold
                                     :shape shape :plain plain
                                     :prefix (and (or literal shape) prefix-p)
                                     :line-bound line-bound
                                     :end-anchored (tree-has tree :modeless-end-anchor :modeless-end-anchor-no-newline)
                                     :reach (let ((most (and thorough (not line-bound) (tree-max-octets tree))))
                                              (cond ((or (not thorough) line-bound) +regex-overlap+)
                                                    (most (min *regex-longest-match* (max +regex-overlap+ most)))
                                                    (t *regex-longest-match*))))))))))
    scanner))

(defun pattern-group-count (pattern)
  "How many numbered groups PATTERN, a regular expression, has, its named
groups among them; 0 for a pattern that cannot be used."
  (let ((count 0))
    (labels ((walk (x)
               (when (consp x)
                 (when (member (first x) '(:register :named-register)) (incf count))
                 (mapc #'walk (rest x)))))
      (walk (ignore-errors (with-pattern-syntax (cl-ppcre:parse-string (engine-pattern pattern))))))
    count))

(defun compile-pattern (pattern &key (regex t) (case-fold nil fold-given) or-text)
  "A scanner for PATTERN as every search reads it (COMPILE-REGEX): a regular
expression when REGEX is true, else literal text, nothing in it special.
CASE-FOLD ignores case; when it is not given, case is ignored while PATTERN
has no upper-case letter (for a regular expression, outside an escape, and
naming no case: REGEX-CASE-FOLD-P). With OR-TEXT, a PATTERN that is no
regular expression is taken as literal text; otherwise that signals, as
COMPILE-REGEX does."
  (flet ((text ()
           (compile-regex (cl-ppcre:quote-meta-chars pattern)
                          :case-fold (if fold-given case-fold (case-fold-p pattern))))
         (regex ()
           (compile-regex pattern :case-fold (if fold-given case-fold (regex-case-fold-p pattern)))))
    (cond ((not regex) (text))
          (or-text (handler-case (regex) (error () (text))))
          (t (regex)))))

(defstruct (decoder (:constructor make-decoder ()) (:copier nil))
  "Buffers reused while decoding windows of a rope for a regex scan: the OCTETS
read, the decoded text in BASE (a base string, for ASCII windows) or WIDE (for
the rest), TABLE from character index to octet offset for WIDE, and SKIP for
the literal scan that skips ahead to a pattern's required text."
  (octets (tuatara.text:make-octets 0) :type tuatara.text:octets)
  (skip (tuatara.text:make-octets 0) :type tuatara.text:octets)
  (base (make-string 0 :element-type 'base-char) :type simple-base-string)
  (wide (make-string 0 :element-type 'character) :type (simple-array character (*)))
  (table (make-array 0 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (*))))

(setf (documentation 'make-decoder 'function) "Return a decoder with empty buffers; they grow on first use."
      (documentation 'decoder-p 'function) "True when OBJECT is a DECODER."
      (documentation 'decoder-octets 'function) "A DECODER's buffer of the octets last read."
      (documentation 'decoder-skip 'function) "A DECODER's buffer for skipping ahead to a pattern's fixed text."
      (documentation 'decoder-base 'function) "A DECODER's text decoded from an ASCII window, a base string."
      (documentation 'decoder-wide 'function) "A DECODER's text decoded from a window past ASCII."
      (documentation 'decoder-table 'function) "A DECODER's octet offset of each character of its wide text.")

(sb-ext:defglobal **spare-decoders**
    (tuatara.runtime:make-spares (lambda () (make-decoder)) :keep-p (lambda (decoder) (<= (length (decoder-octets decoder)) (* 256 1024))))
  "Decoders not in use, kept so that the pieces of a regex search on several
cores reuse their buffers instead of making new ones; one a long match grew
past 256 KB is let go.")

(defmacro with-decoder ((var) &body body)
  "Run BODY with VAR bound to a decoder taken from the spares and given back after."
  `(tuatara.runtime:with-spare (,var **spare-decoders**) ,@body))

(defun decode-window (decoder rope from to)
  "Decode ROPE's octets from FROM below TO into DECODER's buffers. Returns the
string, the number of characters in it (the string may be longer), and the
table from character index to octet offset from FROM, or NIL when every octet
is ASCII and the offsets equal the indices."
  (declare (type decoder decoder) (type fixnum from to) (optimize speed (safety 1)))
  (let ((n (- to from)))
    (declare (type fixnum n))
    (when (< (length (decoder-octets decoder)) n)
      (setf (decoder-octets decoder) (tuatara.text:make-octets n)))
    (let ((octets (decoder-octets decoder)))
      (tuatara.text:rope-copy-into rope from to octets)
      (if (loop for i of-type fixnum from 0 below n always (< (aref octets i) #x80))
          (progn
            (when (< (length (decoder-base decoder)) n)
              (setf (decoder-base decoder) (make-string n :element-type 'base-char)))
            (let ((string (decoder-base decoder)))
              (loop for i of-type fixnum from 0 below n
                    do (setf (schar string i) (code-char (aref octets i))))
              (values string n nil)))
          (progn
            (when (< (length (decoder-wide decoder)) n)
              (setf (decoder-wide decoder) (make-string n :element-type 'character)
                    (decoder-table decoder) (make-array (1+ n) :element-type '(unsigned-byte 32))))
            (let ((string (decoder-wide decoder)) (table (decoder-table decoder)) (i 0) (k 0))
              (declare (type fixnum i k))
              (loop while (< i n)
                    do (multiple-value-bind (code len) (tuatara.text:octets-decode octets i n)
                         (setf (schar string k) (code-char code) (aref table k) i)
                         (incf k) (incf i len)))
              (setf (aref table k) n)
              (values string k table)))))))

(declaim (inline index-octet))
(defun index-octet (table index)
  "Octet offset of character INDEX through TABLE, or INDEX itself for an ASCII window."
  (if table (aref (the (simple-array (unsigned-byte 32) (*)) table) index) index))

(defun octet-index (table octet count)
  "The first character index whose octet offset is at least OCTET through
TABLE (COUNT characters), or OCTET itself for an ASCII window."
  (declare (type fixnum octet count))
  (if table
      (locally (declare (type (simple-array (unsigned-byte 32) (*)) table))
        (tuatara.runtime:bisect (mid 0 count) (< (aref table mid) octet)))
      octet))

(defun matched-text (texts string start end)
  "What a scan gives FUNCTION for the text a match took, from START below END
of STRING: NIL without TEXTS; the text itself when TEXTS is T; else what
TEXTS, a function of STRING, START and END, makes of it (its letter case,
say), so that nothing need be copied for each of a great many matches."
  (cond ((null texts) nil)
        ((eq texts t) (subseq string start end))
        (t (funcall texts string start end))))

(defun matched-groups (texts string group-starts group-ends)
  "The texts of a match's groups, as a vector (NIL for a group that took no
part), when TEXTS is T and the pattern has groups; NIL otherwise."
  (and (eq texts t) (plusp (length group-starts))
       (map 'simple-vector (lambda (gs ge) (and gs (subseq string gs ge))) group-starts group-ends)))

(defun map-slice-matches (function rope scanner line start end &key decoder texts)
  "Call FUNCTION with (MATCH-START MATCH-END MATCHED GROUPS), byte offsets, for
each match of SCANNER in ROPE starting at or after START and ending by END, a
slice of the line that starts at LINE: the line's text before START is the
pattern's context, none after END is seen, empty matches are reported, at END
too. MATCHED and GROUPS only with TEXTS, as MAP-REGEX-MATCHES gives them.
Made for many short slices, a rectangle's rows: it decodes no more than the
line up to END. Stops as soon as FUNCTION returns true and returns that value;
else returns NIL. DECODER, when given, holds the text decoded; else a spare
one does (WITH-DECODER)."
  (declare (type fixnum line start end) (type function function))
  (unless decoder
    (return-from map-slice-matches
      (with-decoder (spare) (map-slice-matches function rope scanner line start end :decoder spare :texts texts))))
  (multiple-value-bind (string count table) (decode-window decoder rope line end)
    (let ((from (octet-index table (- start line) count))
          (scanner (scanner-for-rope scanner rope)))
      (declare (type fixnum from))
      (loop
        (when (> from count) (return nil))
        (multiple-value-bind (ms me group-starts group-ends)
            (tuatara.runtime:stoppable (cl-ppcre:scan scanner string :start from :end count :real-start-pos 0))
          (unless ms (return nil))
          (let* ((bs (+ line (index-octet table ms)))
                 (be (+ line (index-octet table me)))
                 (result (funcall function bs be
                                  (matched-text texts string ms me)
                                  (matched-groups texts string group-starts group-ends))))
            (when result (return result))
            (setf from (if (= ms me) (1+ ms) me))))))))

(defun scanner-for-rope (scanner rope &optional (traits (gethash scanner **regex-traits**)))
  "The scanner to match SCANNER's pattern with in ROPE: where ROPE holds no
carriage return, its lines ending at newlines alone, the pattern as written
\(REGEX-TRAITS-PLAIN), which matches the same there and quicker; else SCANNER.
O(1)."
  (let ((plain (and traits (regex-traits-plain traits))))
    (if (and plain (not (tuatara.text:rope-holds-return-p rope))) plain scanner)))

(defun needle-line (octets filled needle fold from)
  "The line of the first occurrence of NEEDLE (folded when FOLD) in the
first FILLED octets of OCTETS at or after FROM, as two offsets: where it
starts, FROM at the earliest, and where it ends, after the octet that ends it
\(TUATARA.TEXT:LINE-END-OCTET-P; a carriage return as the last octet filled
taken for one a newline may follow) or at FILLED; NIL when NEEDLE is not
there."
  (declare (type tuatara.text:octets octets needle) (type fixnum filled from) (optimize speed))
  (let ((at (scan-chunk octets needle fold from filled filled)))
    (when at
      (flet ((ends-line-p (i)
               (declare (type fixnum i))
               (let ((b (aref octets i)))
                 (or (= b 10) (and (= b 13) (< (1+ i) filled) (/= 10 (aref octets (1+ i))))))))
        (declare (inline ends-line-p))
        (let ((start (loop for i of-type fixnum from (1- (the fixnum at)) downto from
                           when (ends-line-p i) return (1+ i)
                           finally (return from)))
              (end (loop for i of-type fixnum from at below filled
                         when (ends-line-p i) return (1+ i)
                         finally (return filled))))
          (values start end))))))

(defun line-start-within (rope position floor)
  "Where the line holding POSITION of ROPE starts, or FLOOR when that is
later. Complexity: O(log n + POSITION - FLOOR)."
  (let ((newline (tuatara.text:rope-find-line-end-backward rope :start floor :end position)))
    (if newline (1+ newline) floor)))

(defun map-regex-matches (function rope scanner start end &key bound decoder halt (texts t) empty)
  "Call FUNCTION with (MATCH-START MATCH-END MATCHED-STRING GROUPS), byte offsets,
the text matched and a vector of the groups' texts (NIL for a group that took
no part; NIL instead of the vector for a pattern without groups; both NIL when
TEXTS is NIL, which spares copying them; with TEXTS a function, what it makes
of the text and no groups: MATCHED-TEXT), for
each non-empty, non-overlapping match of SCANNER in ROPE that starts at or
after START and before END, in order; with EMPTY, each empty match too (^ at
every line start, for one). The pattern sees no text past BOUND (the
text end by default), so every match ends by then. Stops as soon as FUNCTION
returns true and returns that value; else returns NIL. HALT, when given, is
asked before each window and gives up the scan (returning NIL) when it
answers true. For a scanner from COMPILE-REGEX (see REGEX-TRAITS): stretches
without the text every match holds, or the shape (REQUIRED-SHAPE), are
skipped at literal speed; a pattern
whose matches lie within a line gets windows that end after a line and are
looked for from the start of the line its fixed text is on, so its matches
are found whole and from where they begin on lines up to
*REGEX-LONGEST-MATCH* octets, and when the fixed text does not begin them,
only the lines holding it are tried; \\z and \\Z are looked for at the end only.
Otherwise a match needing text more than
the pattern's reach (REGEX-TRAITS: +REGEX-OVERLAP+ octets, a thorough
scanner's *REGEX-LONGEST-MATCH*) past the stretch it starts in may be cut short
or missed, and so may one whose fixed text lies further than that from its
start. DECODER, when given, holds the text decoded; else a spare one does
\(WITH-DECODER), so that a search piece after piece on several cores makes no
new buffers."
  (declare (type fixnum start end) (type function function))
  (unless decoder
    (return-from map-regex-matches
      (with-decoder (spare)
        (map-regex-matches function rope scanner start end :bound bound :decoder spare :halt halt :texts texts :empty empty))))
  (let* ((total (tuatara.text:rope-bytes rope))
         (bound (if bound (tuatara.text:rope-char-start rope (min bound total)) total))
         (end (min end bound))
         (pos (tuatara.text:rope-char-start rope (min start total)))
         (last-end -1)
         (traits (gethash scanner **regex-traits**))
         (scanner (scanner-for-rope scanner rope traits))
         (needle (and traits (regex-traits-needle traits)))
         (shape (and traits (regex-traits-shape traits)))
         (needle-length (cond (needle (length needle)) (shape (length shape)) (t 0)))
         (prefix-p (and traits (regex-traits-prefix traits)))
         (line-bound (and traits (regex-traits-line-bound traits)))
         ;; each match lies within a line holding NEEDLE: only those lines are tried
         (lines (and needle line-bound (not prefix-p) (plusp (length needle))))
         (overlap (if traits (regex-traits-reach traits) +regex-overlap+))
         ;; the window a filter may read the rope around (*WINDOW-ROPE*): one list, set for each window
         (window (list rope 0 0 bound))
         (chunk (and (or needle shape)
                     (let ((size (+ +search-chunk+ needle-length)))
                       (when (< (length (decoder-skip decoder)) size)
                         (setf (decoder-skip decoder) (tuatara.text:make-octets size)))
                       (decoder-skip decoder)))))
    (declare (type fixnum total bound end pos last-end overlap))
    (when (and traits (regex-traits-end-anchored traits))
      ;; \z and \Z match only at the end: look there alone, in one window reaching BOUND
      (setf pos (max pos (tuatara.text:rope-char-start rope (max 0 (- bound *regex-longest-match*))))))
    (loop while (< pos end)
          do (when (and halt (funcall halt)) (return))
             (let ((range (if (and traits (regex-traits-end-anchored traits)) (max 1 (- end pos)) (max +search-chunk+ overlap))))
               (when (or needle shape)
                 ;; every match holds NEEDLE, or SHAPE: find its next occurrence at literal speed and
                 ;; start there when matches begin with it; else at the start of its line
                 ;; when matches lie within a line, or where a match of the pattern's reach
                 ;; could contain it. After a long jump the occurrences are sparse: only
                 ;; the starts up to this one are worth the regex, the next jump finds the
                 ;; next occurrence.
                 (let ((q (if needle
                              (scan-range-forward rope needle (regex-traits-fold traits) pos
                                                  (min bound (if prefix-p end (+ end overlap))) chunk)
                              (scan-shape-forward rope shape pos (min bound (if prefix-p end (+ end overlap))) chunk))))
                   (unless q (return))
                   (let ((to (cond (prefix-p q)
                                   (line-bound (line-start-within rope q (max pos (- q *regex-longest-match*))))
                                   (t (tuatara.text:rope-char-start rope (max 0 (- (+ q needle-length) overlap)))))))
                     (if shape
                         ;; a shape is found as fast as the window is made: try the pattern
                         ;; only from where it can start up to this place of the shape
                         (setf pos (max pos to) range (max 1 (- (1+ q) pos)))
                         (progn (when (> to (+ pos overlap)) (setf range (max overlap (- (1+ q) to))))
                                (setf pos (max pos to)))))
                   (when (>= pos end) (return))))
             (let* ((context-start (tuatara.text:rope-char-start rope (max 0 (- pos +regex-context+))))
                    (limit (let ((l (tuatara.text:rope-char-start rope (min end (+ pos range)))))
                             (if (> l pos) l (min bound (tuatara.text:rope-next-boundary rope pos)))))
                    (reach (cond ((and traits (regex-traits-end-anchored traits)) bound)
                                 ;; every match lies within a line: the window ends after the
                                 ;; line holding the last start it answers for, and is exact
                                 (line-bound
                                  (let* ((cap (min bound (+ limit *regex-longest-match*)))
                                         (newline (tuatara.text:rope-find-line-end rope :start (max pos (1- limit))
                                                                                        :end cap)))
                                    (if newline (1+ newline) cap)))
                                 (t (+ limit overlap))))
                    ;; a match reported from the range before may run into this one
                    (resume (max pos last-end)))
               (declare (type fixnum context-start limit reach resume))
               (loop ; once per window size: a match reaching the window's end grows the window
                 (let ((window-end (tuatara.text:rope-char-start rope (min bound reach))))
                   (multiple-value-bind (string count table) (decode-window decoder rope context-start window-end)
                     (let ((from (octet-index table (- resume context-start) count))
                           (window (progn (setf (second window) context-start (third window) window-end)
                                          window)))
                       (declare (type fixnum from))
                       (when (loop
                               (when (> from count) (return t))
                               (let ((scan-end count))
                                 (declare (type fixnum scan-end))
                                 (when lines
                                   ;; the pattern is tried on the next line holding its text alone
                                   (multiple-value-bind (line-start line-end)
                                       (needle-line (decoder-octets decoder) (- window-end context-start) needle
                                                    (regex-traits-fold traits) (index-octet table from))
                                     (unless line-start (return t))
                                     (setf from (max from (octet-index table line-start count))
                                           scan-end (octet-index table line-end count))))
                                 (multiple-value-bind (ms me group-starts group-ends reached)
                                     (tuatara.runtime:stoppable
                                       (let ((*window-open* (and (= scan-end count) (< window-end bound)))
                                             (*window-rope* window)
                                             (*window-reached* nil))
                                         (multiple-value-bind (ms me group-starts group-ends)
                                             (cl-ppcre:scan scanner string :start from :end scan-end :real-start-pos 0)
                                           (values ms me group-starts group-ends *window-reached*))))
                                   (if (null ms)
                                       (if (= scan-end count)
                                           (return t)
                                           (setf from scan-end))
                                       (let ((bs (+ context-start (index-octet table ms))))
                                         (when (>= bs limit) (return t))
                                         (when (and reached (= me count) (> me ms))
                                           ;; a filter read the match on past the window: it ends there
                                           (let* ((whole (and texts (rope-text rope bs reached)))
                                                  (result (funcall function bs reached
                                                                   (and whole (matched-text texts whole 0 (length whole)))
                                                                   (matched-groups texts string group-starts group-ends))))
                                             (when result (return-from map-regex-matches result))
                                             (setf last-end reached)
                                             (return t)))
                                         (when (and (= me count) (< window-end bound)
                                                    (< (- window-end context-start) *regex-longest-match*))
                                           ;; the match may go on past the window: look again in a larger one
                                           (setf resume (+ context-start (index-octet table from))
                                                 reach (+ window-end (- window-end context-start)))
                                           (return nil))
                                         (if (= ms me)
                                             (progn ; an empty match: reported when asked for, then look further on
                                               (when empty
                                                 (let ((result (funcall function bs bs (matched-text texts "" 0 0) nil)))
                                                   (when result (return-from map-regex-matches result))))
                                               (setf from (1+ ms)))
                                             (let* ((be (+ context-start (index-octet table me)))
                                                    (result (funcall function bs be (matched-text texts string ms me)
                                                                     (matched-groups texts string group-starts group-ends))))
                                               (when result (return-from map-regex-matches result))
                                               (setf last-end be from me))))))))
                         (return))))))
               (setf pos (max limit last-end)))))
    nil))

(defun rope-text (rope start end)
  "ROPE's text from octet START below END, a base string when every octet is
ASCII, as a window's text is."
  (let ((octets (tuatara.text:rope-octets rope :start start :end end)))
    (declare (type tuatara.text:octets octets) (optimize speed))
    (if (loop for b across octets always (< b #x80))
        (let ((text (make-string (length octets) :element-type 'base-char)))
          (loop for i of-type fixnum from 0 below (length octets)
                do (setf (schar text i) (code-char (aref octets i))))
          text)
        (tuatara.text:octets-string octets))))

(defun regex-scanner (regex case-fold)
  "REGEX itself when it is already a compiled scanner, else its scanner."
  (if (stringp regex) (compile-regex regex :case-fold case-fold) regex))

(defun first-regex-match (rope scanner start end bound decoder &key halt empty)
  "The first match starting in [START, END) and ending by BOUND as a list
(START END STRING GROUPS), or NIL; HALT and EMPTY as for MAP-REGEX-MATCHES."
  (map-regex-matches (lambda (s e m g) (list s e m g)) rope scanner start end
                     :bound bound :decoder decoder :halt halt :empty empty))

(defun regex-resync-start (rope scanner position floor)
  "Where a scan should begin so that the matches of SCANNER it finds from
POSITION of ROPE on are those one scan from further back finds, not a match
begun in the middle of a longer one: the start of POSITION's line for a
pattern whose matches lie within a line, else the pattern's reach before
POSITION (REGEX-TRAITS); never before FLOOR."
  (let ((traits (gethash scanner **regex-traits**)))
    (if (and traits (regex-traits-line-bound traits))
        (line-start-within rope position (max floor (- position *regex-longest-match*)))
        (tuatara.text:rope-char-start
         rope (max floor (- position (if traits (regex-traits-reach traits) +regex-overlap+)))))))

(defun last-regex-match (rope scanner start end bound decoder &key halt (floor start) empty)
  "The last match starting in [START, END) and ending by BOUND as a list
(START END STRING GROUPS), or NIL. Scans +SEARCH-CHUNK+ pieces from END
backward, stopping at the first piece with a match; a piece with one is
scanned again from further back (REGEX-RESYNC-START, never before FLOOR), so
that of a run the pattern matches whole the start of the run is found, as a
forward scan finds it. HALT and EMPTY as for MAP-REGEX-MATCHES."
  (loop for b of-type fixnum = end then a
        for a of-type fixnum = (max start (- b +search-chunk+))
        while (< a b)
        do (let ((last nil))
             (map-regex-matches (lambda (s e m g) (setf last (list s e m g)) nil) rope scanner a b
                                :bound bound :decoder decoder :halt halt :empty empty)
             (when (and halt (funcall halt)) (return nil))
             (when last
               (let ((from (regex-resync-start rope scanner a floor)))
                 (when (< from a)
                   (setf last nil)
                   (map-regex-matches (lambda (s e m g) (when (>= s a) (setf last (list s e m g))) nil)
                                      rope scanner from b :bound bound :decoder decoder :halt halt :empty empty)))
               (when last (return last))))
        until (= a start)))

(defun regex-serial-window ()
  "Octets a regex search scans on the calling thread before going parallel: at
most a mebibyte, since a pattern without fixed text scans at tens of megabytes
a second."
  (min *serial-window* (* 1024 1024)))

(defun regex-search-forward (rope regex &key (start 0) (end (tuatara.text:rope-bytes rope)) bound
                                              (case-fold (and (stringp regex) (regex-case-fold-p regex)))
                                              (threads (tuatara.runtime:cpu-count)) stop empty)
  "The first match of REGEX (a pattern string or a compiled scanner) in ROPE
that starts at or after START and before END and ends by BOUND (the text end by
default; the pattern sees nothing past it): values match start and end (byte
offsets), the matched string and its groups (as MAP-REGEX-MATCHES); NIL when none.
With EMPTY, an empty match counts too (^ at a line start, for one).
A pattern without upper-case letters ignores case. The first
REGEX-SERIAL-WINDOW octets are scanned on the calling thread, the rest in
pieces run in order on THREADS threads (SEARCH-PIECES). STOP, when given, is
asked on the calling thread between windows of the scan, every few
milliseconds; when it answers true the search gives up and returns NIL and
:STOPPED."
  (let* ((scanner (regex-scanner regex case-fold))
         (end (min end (or bound end)))
         (serial-end (min end (+ start (regex-serial-window))))
         (halted nil)
         (halt (and stop (lambda () (or halted (setf halted (and (funcall stop) t))))))
         (hit (with-decoder (decoder) (first-regex-match rope scanner start serial-end bound decoder :halt halt :empty empty))))
    (cond (hit (values-list hit))
          (halted (values nil :stopped))
          ((>= serial-end end) nil)
          (t (let ((piece *parallel-piece*))
               (multiple-value-bind (found stopped)
                   (search-pieces (ceiling (- end serial-end) piece)
                                  (lambda (k halt)
                                    (let ((a (+ serial-end (* k piece))))
                                      (with-decoder (decoder)
                                        (first-regex-match rope scanner a (min end (+ a piece)) bound decoder :halt halt :empty empty))))
                                  stop threads)
                 (cond (stopped (values nil :stopped))
                       (found (values-list found))
                       (t nil))))))))

(defun regex-search-backward (rope regex &key (start (tuatara.text:rope-bytes rope)) (end 0) bound
                                               (case-fold (and (stringp regex) (regex-case-fold-p regex)))
                                               (threads (tuatara.runtime:cpu-count)) stop empty)
  "The last match of REGEX in ROPE that starts before START and at or after END
and ends by BOUND: values as REGEX-SEARCH-FORWARD, EMPTY too; NIL when none. Matches are
found as a forward scan finds them, so of a run that a pattern matches whole
the start of the run is found. Scans back from START a REGEX-SERIAL-WINDOW on
the calling thread, then pieces going back, in parallel; STOP as for
SEARCH-FORWARD."
  (let* ((scanner (regex-scanner regex case-fold))
         (serial-start (max end (- start (regex-serial-window))))
         (halted nil)
         (halt (and stop (lambda () (or halted (setf halted (and (funcall stop) t))))))
         (hit (with-decoder (decoder) (last-regex-match rope scanner serial-start start bound decoder :halt halt :floor end :empty empty))))
    (cond (hit (values-list hit))
          (halted (values nil :stopped))
          ((<= serial-start end) nil)
          (t (let ((piece *parallel-piece*))
               (multiple-value-bind (found stopped)
                   (search-pieces (ceiling (- serial-start end) piece)
                                  (lambda (k halt)
                                    (let ((b (- serial-start (* k piece))))
                                      (with-decoder (decoder)
                                        (last-regex-match rope scanner (max end (- b piece)) b bound decoder :halt halt :floor end :empty empty))))
                                  stop threads)
                 (cond (stopped (values nil :stopped))
                       (found (values-list found))
                       (t nil))))))))

(defun regex-search-all (rope regex &key (start 0) (end (tuatara.text:rope-bytes rope)) bound
                                          (case-fold (and (stringp regex) (regex-case-fold-p regex)))
                                          (limit most-positive-fixnum))
  "All non-empty, non-overlapping matches of REGEX in ROPE that start at or
after START and before END and end by BOUND, as a list of (START END STRING
GROUPS) in order (GROUPS as for MAP-REGEX-MATCHES), at most LIMIT of them."
  (let ((scanner (regex-scanner regex case-fold)) (matches '()) (count 0))
    (map-regex-matches (lambda (s e m g)
                         (push (list s e m g) matches)
                         (>= (incf count) limit))
                       rope scanner start end :bound bound)
    (nreverse matches)))

;;; Collecting every match, for replacing and counting
;;;
;;; A replacement needs all of its matches before it changes anything, and on a
;;; file of many gigabytes they may not fit in memory. COLLECT-MATCHES finds them
;;; in parallel pieces, keeps an estimate of what they will cost, and gives up as
;;; soon as that passes a limit or a key is waiting, before the heap runs out.

(defun map-literal-matches (function rope string start end &key bound (case-fold (case-fold-p string)) texts halt)
  "Call FUNCTION with (MATCH-START MATCH-END TEXT NIL) for each non-overlapping
occurrence of STRING in ROPE that starts at or after START and before END and
ends by BOUND (the text end by default), in order; TEXT is the text matched
when TEXTS is true, else NIL. Stops and returns FUNCTION's value as soon as it
returns true, or returns NIL when HALT, asked before each chunk, answers true;
else returns NIL. CASE-FOLD ignores case, as for SEARCH-FORWARD. The text is read a chunk at a
time into a spare decoder's buffer (WITH-DECODER), so that a search piece
after piece on several cores makes no new ones; a match's text is taken from
that chunk, and given again as it was for a match of the same octets as the
one before, so that billions of matches of a few texts make nothing each."
  (declare (type fixnum start end))
  (when (folds-past-ascii-p string case-fold)
    (return-from map-literal-matches
      (map-regex-matches function rope (literal-scanner string) (character-start-from rope start) end
                         :bound bound :halt halt :texts texts)))
  (with-decoder (decoder)
    (let* ((needle (needle-octets string case-fold)) (n (length needle))
           (total (tuatara.text:rope-bytes rope))
           (end (min end (1+ (- (min (or bound total) total) n)))) ; the last start that fits, exclusive
           (chunk (progn (when (< (length (decoder-skip decoder)) (+ +search-chunk+ n))
                           (setf (decoder-skip decoder) (tuatara.text:make-octets (+ +search-chunk+ n))))
                         (decoder-skip decoder)))
           (next start)
           ;; the octets last matched and what was given for them: a pattern matches few texts
           (last-octets (and texts (tuatara.text:make-octets n))) (last-text nil) (last-p nil))
      (declare (type fixnum n total end next))
      (flet ((text-at (hit)
               (declare (type fixnum hit))
               (if (and last-p (loop for k of-type fixnum below n
                                     always (= (aref chunk (+ hit k)) (aref last-octets k))))
                   last-text
                   (let ((text (tuatara.text:octets-string chunk :start hit :end (+ hit n))))
                     (replace last-octets chunk :start2 hit :end2 (+ hit n))
                     (setf last-p t last-text (matched-text texts text 0 (length text)))))))
      (when (plusp n)
        (loop for pos of-type fixnum from start below end by +search-chunk+
              do (when (and halt (funcall halt)) (return-from map-literal-matches nil))
                 (let* ((chunk-end (min total (+ pos +search-chunk+ (1- n))))
                        (filled (tuatara.text:rope-copy-into rope pos chunk-end chunk))
                        (below (min filled +search-chunk+ (- end pos)))
                        (i (max 0 (- next pos)))
                        (other-case (cons nil nil)))
                   (declare (type fixnum filled below i))
                   (loop
                     (let ((hit (and (< i below) (scan-chunk chunk needle case-fold i below filled other-case))))
                       (unless hit (return))
                       (let* ((s (+ pos hit)) (e (+ s n))
                              (result (funcall function s e (and texts (text-at hit)) nil)))
                         (when result (return-from map-literal-matches result))
                         (setf next e i (+ hit n)))))))))
      nil)))

(defconstant +counted-firsts+ 16
  "How many of its first matches a piece counted without keeping them
remembers, to join it to the piece before (COLLECT-MATCHES).")

(defun collect-matches (mapper start end &key (threads (tuatara.runtime:cpu-count)) stop cost-limit
                                               (match-cost 400) (block-size 16384) (keep t))
  "Every match that MAPPER finds from START below END, in order, exactly as one
scan from START finds them, gathered in parallel pieces of *PARALLEL-PIECE*
octets on THREADS threads. MAPPER is a function of (FUNCTION FROM TO HALT) that
calls FUNCTION with (START END TEXT GROUPS) for each non-overlapping match
starting in [FROM, TO) as a scan from FROM finds them, stopping when FUNCTION
returns true or HALT answers true (MAP-REGEX-MATCHES and MAP-LITERAL-MATCHES
serve, wrapped). A match that runs out of its piece has the next piece's
matches found again from where it ends.
Three values: the matches as a list of (START END TEXT GROUPS); NIL when all
were found, :STOPPED when STOP, asked on the calling thread every few
milliseconds, answered true, or :OVER when the estimated cost passed
COST-LIMIT (octets): MATCH-COST for each match and BLOCK-SIZE for each block of
that many octets the matches touch; then the list is NIL. And how many matches
were found, all or before giving up. STOP is also asked once before the pieces
start, so it is heard even when the pool's workers take every piece.
With KEEP false the matches are only counted, the list always NIL: nothing is
kept for each, so counting a hundred million costs no more memory than one."
  (declare (type fixnum start end))
  (when (and stop (funcall stop))
    (return-from collect-matches (values nil :stopped 0)))
  (let* ((piece *parallel-piece*)
         (count (max 0 (ceiling (- end start) piece)))
         (cost (list 0)) (found (list 0)) (over (list nil)) (cancel (list nil))
         (caller sb-thread:*current-thread*)
         (tasks (make-array count)))
    (dotimes (k count)
      (setf (aref tasks k)
            (let ((a (+ start (* k piece))) (b (min end (+ start (* (1+ k) piece)))))
              (lambda ()
                (let ((acc '()) (local 0) (published 0) (matches 0) (last-block -1) (last-end -1))
                  (declare (type fixnum local published matches last-block last-end))
                  (labels ((publish ()
                             (when (> local published)
                               (let ((delta (- local published)))
                                 (setf published local)
                                 (when (and cost-limit (> (+ (sb-ext:atomic-incf (car cost) delta) delta) cost-limit))
                                   (setf (car over) t)))))
                           (halt ()
                             (publish)
                             (or (car over) (car cancel)
                                 (and stop (eq sb-thread:*current-thread* caller) (funcall stop)
                                      ;; a piece inside one long call is cut short too
                                      (tuatara.runtime:stop-work cancel)))))
                    (unless (halt)
                      (funcall mapper
                               (lambda (s e m g)
                                 (declare (type fixnum s e))
                                 (when (or keep (< matches +counted-firsts+))
                                   (push (list s e m g) acc))
                                 (setf last-end e)
                                 (incf matches)
                                 (incf local match-cost)
                                 (let ((blk (floor s block-size)))
                                   (unless (= blk last-block) (setf last-block blk) (incf local block-size)))
                                 nil)
                               a b #'halt))
                    (publish)
                    (sb-ext:atomic-incf (car found) matches)
                    (list (nreverse acc) matches last-end)))))))
    (let ((results (tuatara.runtime:stoppable-work (cancel)
                     (tuatara.runtime:run-tasks tasks :parallelism (max 1 threads)))))
      (cond ((car cancel) (values nil :stopped (car found)))
            ((car over) (values nil :over (car found)))
            (t (multiple-value-bind (matches total) (join-collected-pieces mapper results start end piece keep)
                 (values matches nil total)))))))

(defun join-collected-pieces (mapper results start end piece keep)
  "The matches COLLECT-MATCHES found in its pieces of PIECE octets from START
below END, from their RESULTS, (OWN COUNT LAST-END) each, OWN the piece's
matches (only its first ones unless KEEP): a piece the match before runs into
has its matches found again from where that match ends (JOIN-PIECES), until
one is among its own; from there they agree, and past its own remembered the
rest is counted again. Two values: the matches in order (NIL unless KEEP),
and how many there are."
  (let ((out '()) (total 0))
    (declare (type fixnum total))
    (join-pieces (length results) (lambda (k) (+ start (* k piece)))
                 (lambda (k from)
                   (destructuring-bind (own count own-end) (aref results k)
                     (declare (type fixnum count own-end))
                     (if (not from)
                         (progn (when keep (setf out (revappend own out)))
                                (incf total count)
                                (and (plusp count) own-end))
                         (let ((b (min end (+ start (* (1+ k) piece)))) (rest own) (passed 0) (met nil) (ends nil))
                           (declare (type fixnum passed))
                           (funcall mapper
                                    (lambda (s e m g)
                                      (loop while (and rest (< (first (car rest)) s)) do (pop rest) (incf passed))
                                      (if (and rest (= (first (car rest)) s) (= (second (car rest)) e))
                                          (setf met t)
                                          (progn (when keep (push (list s e m g) out))
                                                 (incf total) (setf ends e) nil)))
                                    (min from b) b nil)
                           (if met
                               (progn (when keep (setf out (revappend rest out)))
                                      (incf total (- count passed))
                                      own-end)
                               ends))))))
    (values (and keep (nreverse out)) total)))

(defun join-pieces (count start-of join)
  "Go through COUNT pieces of a text cut for work in parallel, in order, as one
scan from the first piece's start goes on, so that a match is never cut at a
piece's start: JOIN, a function of a piece's index and FROM, takes that
piece's part and returns where the last match it took ends, or NIL when it
took none. FROM is NIL, or, when a match of the pieces before runs past the
piece's start (START-OF, a function of the index, gives it), where that match
ends: the piece's matches are then those one scan finds from there, past its
end too when the match runs past it."
  (let ((reach -1))
    (declare (type fixnum reach))
    (dotimes (k count)
      (let ((ends (funcall join k (and (> reach (funcall start-of k)) reach))))
        (when ends (setf reach (max reach ends)))))))
