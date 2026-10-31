;;;; lisp/search/rewrite.lisp --- Replacements too large for memory, rewritten in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A replacement across a file of many gigabytes cannot be made edit by edit
;;;; in memory, and should not keep anyone waiting. It runs as a background job:
;;;; the scope is cut into pieces that begin at line starts, and workers
;;;; rewrite each piece, its matches replaced, into an unnamed file on the same
;;;; filesystem as the buffer's file, one file a worker however many pieces it
;;;; takes (PIECE-FILES). The buffer stays usable meanwhile, edits
;;;; included. The command loop's ticks show the progress and, once every piece
;;;; is done, swap in the new text as one undoable step (BUFFER-SWAP-TEXT). Edits
;;;; made during the job are carried onto its result and win over the
;;;; replacement where they meet. It is a job like any other (jobs.lisp):
;;;; killing the buffer or leaving the editor while it runs asks first, and
;;;; C-x J (stop-job) stops it.
;;;;
;;;; A pattern whose matches may cross lines is cut into pieces as any other:
;;;; where a match runs from one piece into the next, the next is done again
;;;; from where it ends once all are done (MEND-SEAMS), as one scan would.

(in-package #:tuatara.defaults)

(defvar *rewrite-threshold* (* 32 1024 1024)
  "Octets of scope from which a replacement or count runs in the background
instead of in memory, so that it never holds the editor up.")

(defun large-for-here-p (start end)
  "True when the text from START below END is more octets than
*REWRITE-THRESHOLD*: work on it goes to the background."
  (> (abs (- end start)) *rewrite-threshold*))

(defun too-large-here-p (start end what)
  "True, saying so, when the text from START below END is more octets than
*REWRITE-THRESHOLD*: more than WHAT (a phrase such as \"Indenting\") is done on
at once, on the editor's thread, without holding it up."
  (let ((size (abs (- end start))))
    (when (> size *rewrite-threshold*)
      (message "~a works on at most ~a at once, and this is ~a: it was not done"
               what (octets-words *rewrite-threshold*) (octets-words size))
      t)))

(defvar *rewrite-piece* (* 64 1024 1024)
  "Octets of the scope a worker rewrites at a time; each piece ends at a line start.")

(defvar *rewrite-checkpoint* (* 256 1024)
  "Octets between the places a piece notes how far its input and output have
got, so that a position is mapped onto the result by rescanning at most this much.")

(defvar *rewrite-piece-hook* nil
  "A function each worker calls before its piece, or NIL. Tests use it to keep a
job running long enough to act while it runs.")

;;; Pieces and jobs

(defstruct (rewrite-piece (:constructor make-rewrite-piece (start end &aux (done start) (allowed start))) (:copier nil))
  "A stretch of a job's scope from START below END, beginning at a line start,
or where a match of the pieces before ends (MEND-SEAMS). ALLOWED is the first
offset a match may start at under the one-per-line rule, START unless such a
match came before; DONE is how far its worker has got; REACH, where the last
match its worker passed over ends, -1 before one, and ALLOWED-AFTER the first
offset the match after its end may start at; TALLIES, what it counted (TALLY); MATCHES and LINES, how many matches it
replaced and on how many lines; LENGTH, the octets it wrote; WRITER, the
writer of its worker's unnamed file (PIECE-WRITER), and FROM, where in that
file its output begins, until the job maps it as ROPE; CHECKPOINTS, triples of
the input offset, the output offset within the piece and the first offset a
match may start at under the one-per-line rule, noted along the way."
  (start 0 :type fixnum) (end 0 :type fixnum) (done 0 :type fixnum)
  (allowed 0 :type fixnum) (reach -1 :type fixnum) (allowed-after 0 :type fixnum) (tallies '())
  (matches 0 :type fixnum) (lines 0 :type fixnum) (length 0 :type fixnum)
  (writer nil) (from 0 :type fixnum) (rope nil)
  (checkpoints (make-array 48 :element-type 'fixnum :adjustable t :fill-pointer 0)))

(defstruct (rewrite (:include tuatara.runtime:job) (:constructor %make-rewrite) (:copier nil))
  "A replacement or count running in the background over BUFFER, a job
\(TUATARA.RUNTIME:JOB). BASE is the
version it reads and ROPE its text; LOW and HIGH bound the scope and WORDS name
it; MAPPER finds the matches of a piece (one left as it is told as passed
over, PASS-OVER) and EXPAND gives a match's replacement as octets; ALL is true with the g flag; COUNT-ONLY for the n flag. PIECES are
rewritten by the pool's workers under the job's thread. TRACKER holds the edits made to the buffer since BASE (DIRTY records, or :ALL),
noted by TRACKER-FUNCTION; RESULT is the text rewritten, once done;
DIRECTORY is where the unnamed files go, FILES those files, one a worker
\(PIECE-FILES). CHECKPOINT and HOOK are
*REWRITE-CHECKPOINT* and *REWRITE-PIECE-HOOK* as the job began, since the
workers run on other threads. DOING, for work other than a replacement, names
it in the list of jobs and in questions (Cutting), and REPORT, a function of the
job, how it ended (:DONE, :NONE, :STOPPED or :FAILED), the changes made, the
seconds taken and the changes kept as edited meanwhile, says so; for a count,
done, it says what to say in place of the count, or NIL. RUNNER, when
not NIL, rewrites a piece in place of REWRITE-PIECE-RUN: a function of the job
and the piece making the same text, faster for work of its own kind.
COUNTING, once every piece is written, is the cons INDEX-ROPE reports on as it
counts the lines of the text written: the octets counted and the octets to
count. SHADE, when not NIL, gives the text the job changes, shaded as it goes,
for a job that changes less than its scope (a rectangle's columns): a function
of the job and the offsets FROM and TO of the text shown, returning (START END
FACE) ranges; by default the scope's pieces are shaded (JOB-SHADING). WHAT,
when not NIL, says in full what the job does, for the list of jobs; VERB,
what it does as a verb, for when it fails (Could not VERB), and LEAVES what
stopping it leaves (Stopped: LEAVES), when not the replacement's. JOINS, when not
NIL, is the buffer's undo stack as a change made before the job left it (the
typing at a split's places in sight): the job's change joins that one for undo.
SINCE is the buffer's undo stack as the job began (JOINS when given): its
change stands there in the undo history, before the changes made meanwhile
\(TUATARA.BUFFER:BUFFER-CHANGE-WHERE-BEGUN); unless SESSION, for a job that
writes out of sight what typing at places in sight still goes on writing (a
split's, a block's rows): its change joins that typing on top, one change for
undo with it. PREPARE, when not NIL, a function of the job,
runs on the job's thread before the pieces are rewritten, to learn what their
rewriting needs to know of the whole scope. GOAL, when not NIL, a function of the
buffer with the job's text taken in, gives where point goes then, as the same
work done at once leaves it; only when point is still at POINT, where it was
when the job began, and nothing was edited meanwhile. HIDDEN-LEFT, when not
NIL, is a cons whose car counts the matches left alone because a fold hides
them (HIDDEN-LEFT-WORDS). LIGHTS-MATCHES, true for a replacement whose MAPPER
only finds matches, keeping no count of its own, has the matches still to
change shaded rather than the whole stretch (JOB-MATCH-SHADING)."
  base rope (low 0) (high 0) words what verb leaves joins since session prepare goal point hidden-left
  mapper expand all count-only doing report runner (lights-matches nil)
  (pieces #()) (counting nil) (shade nil)
  (tracker '()) tracker-function result directory (files nil)
  (out-starts-cache nil)
  (checkpoint (* 256 1024)) (hook nil))

(setf (documentation 'rewrite-p 'function) "True when OBJECT is a REWRITE."
      (documentation 'rewrite-piece-p 'function) "True when OBJECT is a REWRITE-PIECE."
      (documentation 'make-rewrite-piece 'function) "Return a piece of a rewrite from START below END.")

(defun rewrite-totals (job)
  "How many matches JOB's pieces found and on how many lines, two values."
  (loop for p across (rewrite-pieces job)
        sum (rewrite-piece-matches p) into matches
        sum (rewrite-piece-lines p) into lines
        finally (return (values matches lines))))

(defun hidden-left-words (count)
  "What is said of COUNT matches left alone because a fold hides them, after
what a piece of work did: \"; N hidden in folds left alone\", or \"\" for none.
COUNT is a number, or a job (REWRITE) whose HIDDEN-LEFT counts them."
  (let ((n (cond ((numberp count) count)
                 ((and (rewrite-p count) (rewrite-hidden-left count)) (car (rewrite-hidden-left count)))
                 (t 0))))
    (if (plusp n) (format nil "; ~:d hidden in folds left alone" n) "")))

(defun rewrite-of (buffer)
  "The replacement or count running in BUFFER, or NIL."
  (tuatara.runtime:running-job buffer 'rewrite))

(defun rewrite-progress (job)
  "How much of JOB's scope, its pieces, is done, from 0 to 100."
  (let ((size (loop for p across (rewrite-pieces job) sum (- (rewrite-piece-end p) (rewrite-piece-start p)))))
    (if (<= size 0)
        100
        (min 100 (floor (* 100 (loop for p across (rewrite-pieces job)
                                     sum (- (rewrite-piece-done p) (rewrite-piece-start p))))
                        size)))))

(defun rewrite-mapper-for (rope matcher texts high)
  "A function of (FUNCTION FROM TO HALT) calling FUNCTION with each match of
MATCHER in ROPE starting in [FROM, TO), as one scan from FROM finds them,
empty ones too and the one at HIGH, the scope's end (STRETCH-MATCHES); the
pattern sees no text past HIGH, so that a match may run past TO. TEXTS, what
the replacement needs of each match, as TUATARA.SYNTAX:MATCHED-TEXT takes it
\(REWRITE-EXPANDER)."
  (lambda (function from to halt)
    (stretch-matches function rope matcher from to (eql to high) texts halt high)))

(defvar *piece* nil
  "The piece of a job (REWRITE-PIECE) the worker on this thread is doing, or NIL.")

(defun tally (counter)
  "Add one to the car of COUNTER, a count a job's pieces keep together, and
return it: noted in the piece this thread is doing, so that the piece done
again (MEND-SEAMS) takes its count back first."
  (when *piece*
    (let ((cell (assoc counter (rewrite-piece-tallies *piece*) :test #'eq)))
      (if cell (incf (cdr cell)) (push (cons counter 1) (rewrite-piece-tallies *piece*)))))
  (sb-ext:atomic-incf (car counter) 1)
  (car counter))

(defun run-piece (job piece)
  "Do PIECE of JOB on this thread, by its runner (REWRITE-PIECE-RUN unless
another), what it counted noted in it (TALLY)."
  (let ((*piece* piece))
    (funcall (or (rewrite-runner job) #'rewrite-piece-run) job piece)))

(defun pass-over (function s e)
  "Tell FUNCTION, called by a job's mapper (REWRITE), that a match from S below
E was passed over and left as it is: a scan goes on past it all the same."
  (funcall function s e :passed nil))

(defun rewrite-expander (replacement regex fold &optional (newline (string #\Newline)))
  "A function of a match's TEXT and GROUPS giving its replacement as octets, the
text REPLACEMENT-TEXT-FUNCTION gives, as RUN-SUBSTITUTION makes it: the same
text encoded once and its octets given again, so that a replacement the match
does not enter makes nothing however many matches there are. (An expander may
also give, for a replacement too large to hold, a cons of its length in octets
and a function of a writer \(TUATARA.IO:WRITER) that writes it:
REPLACEMENT-LENGTH.) As a second value, the TEXTS a scan is to give it
\(TUATARA.SYNTAX:MATCHED-TEXT): NIL, T, or MATCH-CASE-KIND when only the
match's letter case counts, so that no match's text is copied. A line break
written in REPLACEMENT is NEWLINE, the buffer's."
  (multiple-value-bind (text-of needs) (replacement-text-function replacement regex fold newline)
    (let (;; the text last encoded and its octets, one cons read whole, shared by the pieces' threads
          (last (cons nil nil)))
      (values (lambda (m g)
                (let ((text (funcall text-of m g))
                      (known last))
                  (if (eq text (car known))
                      (cdr known)
                      (let ((new (cons text (tuatara.text:string-octets text))))
                        (setf last new)
                        (cdr new)))))
              (case needs ((nil) nil) (:case #'match-case-kind) (t t))))))

(defun replacement-length (replacement)
  "The octets REPLACEMENT, as an expander gives it, takes: a vector's length,
or the length a (LENGTH . WRITER) replacement says it writes."
  (if (consp replacement) (car replacement) (length replacement)))

(defstruct (piece-files (:constructor make-piece-files (directory)) (:copier nil))
  "The unnamed files in DIRECTORY that the pieces of a job on disk are written
to: one to each thread that writes a piece, opened as it first does
\(PIECE-WRITER), WRITERS pairing each thread with its writer. A job holds as
many open files as it has workers, however many pieces it has: a file of
gigabytes cut into pieces of a few megabytes would soon pass the thousand or
so files a process may have open."
  (lock (sb-thread:make-mutex :name "piece files"))
  (writers '())
  (directory nil))

(defun piece-writer (files)
  "The writer of this thread's file among FILES', the file opened the first time."
  (let ((thread sb-thread:*current-thread*))
    (sb-thread:with-mutex ((piece-files-lock files))
      (or (cdr (assoc thread (piece-files-writers files)))
          (let ((writer (tuatara.io:make-writer (tuatara.io:open-scratch-file (piece-files-directory files)))))
            (push (cons thread writer) (piece-files-writers files))
            writer)))))

(defun discard-piece-files (files)
  "Close every file of FILES, what was written to them let go."
  (loop for (nil . writer) in (shiftf (piece-files-writers files) '())
        do (ignore-errors (sb-posix:close (tuatara.io:writer-fd writer)))))

(defun piece-files-ropes (files outputs)
  "A rope for each of OUTPUTS, lists beginning (WRITER START END): the octets
from START below END of the file WRITER, one of FILES', wrote. Each file is
written out and mapped once, the mapping holding it open from then on, and
leaves FILES."
  (let ((mappings '()))
    (loop while (piece-files-writers files)
          do (let* ((writer (cdr (first (piece-files-writers files))))
                    (fd (progn (tuatara.io:writer-flush writer) (tuatara.io:writer-fd writer)))
                    (length (tuatara.io:writer-length writer)))
               (push (cons writer (unless (zerop length) (tuatara.text:map-fd fd length "rewritten text"))) mappings)
               (when (zerop length) (sb-posix:close fd))
               (pop (piece-files-writers files))))
    (loop for (writer start end) in outputs
          collect (if (< start end)
                      (tuatara.text:mapping-rope (cdr (assoc writer mappings)) start end)
                      (tuatara.text:make-rope)))))

(defun rewrite-pieces-for (rope low high)
  "The pieces of [LOW, HIGH) in ROPE, each about *REWRITE-PIECE* octets and
ending at a line start, as a simple-vector. A match running past a piece's end
is mended once they are done (MEND-SEAMS)."
  (map 'simple-vector (lambda (piece) (apply #'make-rewrite-piece piece))
       (tuatara.text:rope-line-pieces rope low high *rewrite-piece*)))

;;; The workers

(defun start-piece (job piece &optional (output t))
  "Begin PIECE of JOB on a worker: call the job's hook, then, unless OUTPUT is
NIL, return the writer of this worker's file (PIECE-WRITER), noting in PIECE
where its output begins there."
  (when (rewrite-hook job) (funcall (rewrite-hook job)))
  (when output
    (let ((writer (piece-writer (rewrite-files job))))
      (setf (rewrite-piece-writer piece) writer
            (rewrite-piece-from piece) (tuatara.io:writer-length writer))
      writer)))

(defun piece-written (piece writer)
  "How many octets PIECE has written through WRITER so far."
  (- (tuatara.io:writer-length writer) (rewrite-piece-from piece)))

(defun piece-checkpoint (piece in out allowed)
  "Note in PIECE that its input up to IN came out as OUT octets of its output,
the next match allowed from ALLOWED."
  (let ((checkpoints (rewrite-piece-checkpoints piece)))
    (vector-push-extend in checkpoints)
    (vector-push-extend out checkpoints)
    (vector-push-extend allowed checkpoints)))

(defun finish-piece (piece writer changes lines)
  "End PIECE, CHANGES made on LINES lines: note the octets it wrote through
WRITER (none for a count) and that the whole piece is done."
  (setf (rewrite-piece-matches piece) changes
        (rewrite-piece-lines piece) lines
        (rewrite-piece-length piece) (if writer (piece-written piece writer) 0)
        (rewrite-piece-done piece) (rewrite-piece-end piece)))

(declaim (inline slice-next-line))
(defun slice-next-line (rope total crlf line-end block block-start block-end)
  "Where the line after the one ending at LINE-END of ROPE starts: past its
newline, and past the carriage return before it when CRLF; TOTAL, ROPE's
length, after the last line. BLOCK holds ROPE's octets from BLOCK-START below
BLOCK-END."
  (declare (type fixnum total line-end))
  (cond ((>= line-end total) total)
        ((and crlf (= 13 (block-octet rope block block-start block-end line-end)))
         (min total (+ line-end 2)))
        (t (1+ line-end))))

(defun write-text (writer rope from to block block-start block-end)
  "Write ROPE's text from FROM below TO through WRITER: from BLOCK, which holds
ROPE's text from BLOCK-START below BLOCK-END, when all of it is there."
  (declare (type fixnum from to block-start block-end))
  (if (and (>= from block-start) (<= to block-end))
      (tuatara.io:writer-octets writer block (- from block-start) (- to block-start))
      (tuatara.io:writer-rope writer rope from to)))

(defun rewrite-piece-run (job piece)
  "Rewrite PIECE of JOB into an unnamed file: the text as it is, with the
matches that count replaced; or, for a count, only count them. The text
between matches is copied from a megabyte of it read at a time, and the
output written a megabyte at a time."
  (let* ((rope (rewrite-rope job))
         (total (tuatara.text:rope-bytes rope))
         (start (rewrite-piece-start piece)) (end (rewrite-piece-end piece))
         (count-only (rewrite-count-only job))
         (all (rewrite-all job))
         (expand (rewrite-expand job))
         (cancel (rewrite-cancel job))
         (writer (start-piece job piece (not count-only)))
         ;; the text read, a megabyte at a time
         (window (tuatara.text:make-octets 1048576)) (window-start 0) (window-end 0)
         (pos start) (allowed (rewrite-piece-allowed piece)) (reach -1) (line-end -1)
         (interval (rewrite-checkpoint job))
         (next-checkpoint (+ start interval))
         (matches 0) (lines 0))
    (declare (type fixnum total pos allowed reach line-end next-checkpoint matches lines window-start window-end)
             (type (simple-array (unsigned-byte 8) (*)) window))
    (flet ((copy-to (to)
             (declare (type fixnum to))
             (loop while (< pos to)
                   do (unless (and (<= window-start pos) (< pos window-end))
                        (setf window-start pos
                              window-end (min total (+ pos (length window))))
                        (tuatara.text:rope-copy-into rope window-start window-end window))
                      (let ((stop (min to window-end)))
                        (tuatara.io:writer-octets writer window (- pos window-start) (- stop window-start))
                        (setf pos stop)))))
      (funcall (rewrite-mapper job)
               (lambda (s e m g)
                 (declare (type fixnum s e))
                 (setf reach (max reach e))
                 (when (and (>= s allowed) (not (eq m :passed)))
                   (unless count-only
                     (copy-to s)
                     (let ((r (funcall expand m g)))
                       (if (consp r)
                           (funcall (the function (cdr r)) writer)
                           (tuatara.io:writer-octets writer r)))
                     (setf pos e))
                   (incf matches)
                   (when (> s line-end)
                     (incf lines)
                     ;; the line's end from the text read when it holds it, else looked up
                     (setf line-end (let ((hit (and (<= window-start s) (< s window-end)
                                                    (tuatara.text:octets-line-end-position window (- s window-start)
                                                                                           (- window-end window-start) nil))))
                                      (if hit (+ window-start hit) (tuatara.text:rope-line-end rope s)))))
                   (unless all (setf allowed (next-line-start-after rope e s))))
                 (setf (rewrite-piece-done piece) e)
                 (when (and (not count-only) (>= e next-checkpoint))
                   ;; the chain goes on from E; what lies between POS and E is copied as it is
                   (piece-checkpoint piece e (+ (piece-written piece writer) (- e pos)) allowed)
                   (setf next-checkpoint (+ e interval)))
                 nil)
               start end (lambda () (car cancel)))
      (unless (car cancel)
        ;; up to where the last match passed over ends, when it runs past the piece's end
        (unless count-only (copy-to (max end reach)))
        (setf (rewrite-piece-reach piece) reach (rewrite-piece-allowed-after piece) allowed)
        (finish-piece piece writer matches lines)))))

(defun mend-seams (job)
  "Make what JOB's pieces, done in parallel, found and wrote what one scan of
its scope makes (TUATARA.SYNTAX:JOIN-PIECES): a piece that a match of the
pieces before runs into is done again, on this thread, from where that match
ends, what it counted taken back first (TALLY), and each piece then ends
where the next begins. A pattern whose matches
keep within a line never needs it, the pieces beginning at line starts."
  (let ((pieces (rewrite-pieces job))
        (allowed 0))
    (tuatara.syntax:join-pieces
     (length pieces) (lambda (k) (rewrite-piece-start (aref pieces k)))
     (lambda (k from)
       (let ((piece (aref pieces k)))
         (when (and from (not (car (rewrite-cancel job))))
           (setf (rewrite-piece-start piece) from (rewrite-piece-end piece) (max from (rewrite-piece-end piece))
                 (rewrite-piece-done piece) from (rewrite-piece-allowed piece) (max from allowed)
                 (rewrite-piece-reach piece) -1 (rewrite-piece-writer piece) nil
                 (fill-pointer (rewrite-piece-checkpoints piece)) 0)
           (loop for (counter . n) in (shiftf (rewrite-piece-tallies piece) '())
                 do (sb-ext:atomic-decf (car counter) n))
           (run-piece job piece))
         (setf allowed (max allowed (rewrite-piece-allowed-after piece)))
         (and (>= (rewrite-piece-reach piece) 0) (rewrite-piece-reach piece)))))
    (loop for k from 1 below (length pieces)
          do (setf (rewrite-piece-end (aref pieces (1- k))) (rewrite-piece-start (aref pieces k))))))

(defun rewrite-run (job)
  "What JOB's thread does (TUATARA.RUNTIME:START-JOB-THREAD): rewrite the
pieces on the pool, then build the text rewritten over their files and count
its lines, on nine cores in ten at once, the count's progress in COUNTING; or
note that it stopped. The editor is woken as the count begins. However it
ends, the files not mapped by then are closed."
  (unwind-protect
       (let ((pieces (rewrite-pieces job)))
         (setf (rewrite-files job) (make-piece-files (rewrite-directory job)))
         (when (rewrite-prepare job) (funcall (rewrite-prepare job) job))
         (when (and (plusp (length pieces)) (not (car (rewrite-cancel job))))
           (tuatara.runtime:run-tasks
            (map 'simple-vector (lambda (piece) (lambda () (run-piece job piece)))
                 pieces)
            :parallelism (max 1 (- (tuatara.runtime:cpu-count) 2)))
           (mend-seams job))
         (cond ((car (rewrite-cancel job)) (setf (rewrite-status job) :stopped))
               ((rewrite-count-only job) (setf (rewrite-status job) :done))
               (t (let* ((ropes (piece-files-ropes (rewrite-files job)
                                                   (loop for piece across pieces
                                                         collect (list (rewrite-piece-writer piece) (rewrite-piece-from piece)
                                                                       (+ (rewrite-piece-from piece) (rewrite-piece-length piece))))))
                         (written (apply #'tuatara.text:splice-rope (rewrite-rope job) (rewrite-low job) (rewrite-high job) ropes))
                         (counting (cons 0 0)))
                    (loop for piece across pieces for rope in ropes do (setf (rewrite-piece-rope piece) rope))
                    (setf (rewrite-counting job) counting)
                    (when (rewrite-wake job) (funcall (rewrite-wake job)))
                    (let ((counted (tuatara.text:index-rope written :progress counting :cancel (rewrite-cancel job))))
                      (if counted
                          (setf (rewrite-result job) counted (rewrite-status job) :done)
                          (setf (rewrite-status job) :stopped)))))))
    (when (rewrite-files job) (ignore-errors (discard-piece-files (rewrite-files job))))))

;;; Mapping offsets between the text read and the text written

(defun rewrite-delta (job)
  "How much longer the rewritten scope is than the scope read."
  (- (loop for p across (rewrite-pieces job) sum (rewrite-piece-length p))
     (- (rewrite-high job) (rewrite-low job))))

(defun rewrite-out-starts (job)
  "For each piece, where its output starts in the rewritten text (cached)."
  (or (rewrite-out-starts-cache job)
      (setf (rewrite-out-starts-cache job)
            (let ((at (rewrite-low job)))
              (map 'simple-vector (lambda (p) (prog1 at (incf at (rewrite-piece-length p)))) (rewrite-pieces job))))))

(defun piece-index-at (job base)
  "The index of the piece of JOB holding base offset BASE (its start at or before it)."
  (let ((pieces (rewrite-pieces job)))
    (max 0 (1- (tuatara.runtime:bisect (mid 0 (length pieces)) (<= (rewrite-piece-start (aref pieces mid)) base))))))

(defun piece-index-at-output (job out)
  "The index of the piece of JOB whose output holds rewritten offset OUT."
  (let ((starts (rewrite-out-starts job)))
    (max 0 (1- (tuatara.runtime:bisect (mid 0 (length (rewrite-pieces job))) (<= (aref starts mid) out))))))

(defun checkpoint-before (piece offset &key output)
  "Three values from PIECE's checkpoints: the input offset, the output offset
within the piece and the one-per-line bound of the last checkpoint at or before
OFFSET, an input offset, or with OUTPUT true an output offset; the piece's own
start when there is none."
  (let* ((cps (rewrite-piece-checkpoints piece))
         (n (floor (length cps) 3))
         (key (if output 1 0))
         (lo (1- (tuatara.runtime:bisect (mid 0 n) (<= (aref cps (+ key (* 3 mid))) offset)))))
    (if (< lo 0)
        (values (rewrite-piece-start piece) 0 (rewrite-piece-allowed piece))
        (values (aref cps (* 3 lo)) (aref cps (+ 1 (* 3 lo))) (aref cps (+ 2 (* 3 lo)))))))

(defun rewrite-walk (job index in out allowed function)
  "Call FUNCTION with (S E OS OE) for each match JOB replaced, in order, from
base offset IN in piece INDEX, whose output is at rewritten offset OUT there,
with the one-per-line bound ALLOWED, through the later pieces: S and E in the
text read, OS and OE in the text written. Stops when FUNCTION returns true."
  (let ((pieces (rewrite-pieces job)) (starts (rewrite-out-starts job))
        (rope (rewrite-rope job)) (all (rewrite-all job)) (expand (rewrite-expand job)))
    (loop for i from index below (length pieces)
          do (let* ((piece (aref pieces i))
                    (in (if (= i index) in (rewrite-piece-start piece)))
                    (out (if (= i index) out (aref starts i)))
                    (allowed (if (= i index) allowed (rewrite-piece-allowed piece))))
               (funcall (rewrite-mapper job)
                        (lambda (s e m g)
                          (when (and (>= s allowed) (not (eq m :passed)))
                            (let* ((os (+ out (- s in)))
                                   (oe (+ os (replacement-length (funcall expand m g)))))
                              (when (funcall function s e os oe) (return-from rewrite-walk t))
                              (setf in e out oe)
                              (unless all (setf allowed (next-line-start-after rope e s)))))
                          nil)
                        in (rewrite-piece-end piece) nil)))
    nil))

(defun rewrite-map (job base &optional (bias :before))
  "The offset in JOB's rewritten text of BASE, an offset in the text it read. An
offset inside a replaced match goes to the start of its replacement with BIAS
:BEFORE and to its end with :AFTER."
  (let ((low (rewrite-low job)) (high (rewrite-high job)))
    (cond ((<= base low) base)
          ((>= base high) (+ base (rewrite-delta job)))
          (t (let* ((index (piece-index-at job base))
                    (piece (aref (rewrite-pieces job) index)))
               (multiple-value-bind (in local allowed) (checkpoint-before piece base)
                 (let* ((out (+ (aref (rewrite-out-starts job) index) local))
                        (last-in in) (last-out out) (inside nil))
                   (rewrite-walk job index in out allowed
                                 (lambda (s e os oe)
                                   (cond ((>= s base) t)
                                         ((< base e) (setf inside (if (eq bias :after) oe os)) t)
                                         (t (setf last-in e last-out oe) nil))))
                   (or inside (+ last-out (- base last-in))))))))))

(defun rewrite-unmap (job out &optional (bias :before))
  "The offset in the text JOB read of OUT, an offset in the text it wrote. An
offset inside a replacement goes to the start of the match it replaced with
BIAS :BEFORE and to its end with :AFTER."
  (let* ((low (rewrite-low job)) (delta (rewrite-delta job)) (end (+ (rewrite-high job) delta)))
    (cond ((<= out low) out)
          ((>= out end) (- out delta))
          (t (let* ((index (piece-index-at-output job out))
                    (piece (aref (rewrite-pieces job) index))
                    (piece-out (aref (rewrite-out-starts job) index)))
               (multiple-value-bind (in local allowed) (checkpoint-before piece (- out piece-out) :output t)
                 (let* ((start-out (+ piece-out local))
                        (last-in in) (last-out start-out) (inside nil))
                   (rewrite-walk job index in start-out allowed
                                 (lambda (s e os oe)
                                   (cond ((> os out) t)
                                         ((= os out) (setf inside s) t)
                                         ((< out oe) (setf inside (if (eq bias :after) e s)) t)
                                         (t (setf last-in e last-out oe) nil))))
                   (or inside (+ last-in (- out last-out))))))))))

(defun rewrite-widen (job start end)
  "START and END, offsets in the text JOB read, widened to take in whole every
replaced match they overlap (for an empty range, every one it falls strictly
inside). Values: the widened start and end, and how many matches were taken in."
  (let ((low (rewrite-low job)) (high (rewrite-high job)) (count 0))
    (when (and (< start high) (> end low) (plusp (length (rewrite-pieces job))))
      (let* ((from (max low start))
             (index (piece-index-at job from))
             (piece (aref (rewrite-pieces job) index)))
        (multiple-value-bind (in local allowed) (checkpoint-before piece from)
          (rewrite-walk job index in (+ (aref (rewrite-out-starts job) index) local) allowed
                        (lambda (s e os oe)
                          (declare (ignore os oe))
                          (cond ((if (= start end) (>= s start) (>= s end)) t)
                                ((if (= start end) (< s start e) (and (< s end) (> e start)))
                                 (incf count)
                                 (setf start (min start s) end (max end e))
                                 nil)
                                (t nil)))))))
    (values start end count)))

;;; Edits made while the job runs

(defstruct (dirty (:constructor make-dirty (base-start base-end start end)) (:copier nil))
  "A stretch edited while a job ran: BASE-START below BASE-END in the text the
job read, START below END in the buffer's text now."
  (base-start 0 :type fixnum) (base-end 0 :type fixnum) (start 0 :type fixnum) (end 0 :type fixnum))

(setf (documentation 'make-dirty 'function) "Return a record of a stretch edited while a job ran."
      (documentation 'dirty-p 'function) "True when OBJECT is a DIRTY record.")

(defun track-edits (dirties edits)
  "DIRTIES, sorted DIRTY records in the coordinates before an edit batch, after
the batch's EDITS (sorted, in the same coordinates): stretches that the edits
touch or join are merged into one, which knows its extent in the text the job
read and in the text now."
  (let ((items (sort (nconc (mapcar (lambda (d) (list (dirty-start d) (dirty-end d) d)) dirties)
                            (map 'list (lambda (e) (list (tuatara.text:edit-start e) (tuatara.text:edit-end e) e)) edits))
                     #'< :key #'first))
        (result '()) (before 0) (shift 0))
    ;; BEFORE: how much longer the stretches passed are now than in the text read;
    ;; SHIFT: how much the edits passed moved what follows them
    (loop while items
          do (let* ((head (pop items)) (start (first head)) (end (second head)) (members (list (third head))))
               (loop while (and items (<= (first (first items)) end))
                     do (let ((item (pop items)))
                          (setf end (max end (second item)))
                          (push (third item) members)))
               (let ((grown 0) (moved 0))
                 (dolist (m members)
                   (if (dirty-p m)
                       (incf grown (- (- (dirty-end m) (dirty-start m)) (- (dirty-base-end m) (dirty-base-start m))))
                       (incf moved (- (tuatara.text:edit-insert-length m) (tuatara.text:edit-delete-length m)))))
                 (push (make-dirty (- start before) (- end before grown) (+ start shift) (+ end shift moved)) result)
                 (incf before grown)
                 (incf shift moved))))
    (nreverse result)))

(defun rewrite-note-batch (job batch)
  "Change function while JOB runs: note the stretches BATCH edited."
  (let ((tracker (rewrite-tracker job)))
    (unless (eq tracker :all)
      (setf (rewrite-tracker job)
            (if (tuatara.text:batch-swap batch)
                :all
                (track-edits tracker (tuatara.text:batch-edits batch)))))))

;;; Taking the result in

(defun rewrite-regions (job)
  "The stretches edited while JOB ran, widened over the replaced matches they
meet and merged, as a simple-vector of plists (:base-start :base-end :start :end
:out-start :out-end); and, second, how many replaced matches they took back."
  (let ((merged '()) (taken 0))
    (dolist (d (rewrite-tracker job))
      (multiple-value-bind (b1 b2 count) (rewrite-widen job (dirty-base-start d) (dirty-base-end d))
        (incf taken count)
        ;; the widened parts are text nobody edited, so they move with the stretch
        (let ((c1 (- (dirty-start d) (- (dirty-base-start d) b1)))
              (c2 (+ (dirty-end d) (- b2 (dirty-base-end d))))
              (previous (first merged)))
          (if (and previous (<= b1 (getf previous :base-end)))
              ;; the end now is where the later of the two ends is now
              (let ((pb2 (getf previous :base-end)))
                (setf (getf previous :end) (if (>= b2 pb2) c2 (+ c2 (- pb2 b2)))
                      (getf previous :base-end) (max b2 pb2)))
              (push (list :base-start b1 :base-end b2 :start c1 :end c2) merged)))))
    (values (map 'simple-vector
                 (lambda (r)
                   (list* :out-start (rewrite-map job (getf r :base-start) :before)
                          :out-end (rewrite-map job (getf r :base-end) :after)
                          r))
                 (nreverse merged))
            taken)))

(defun last-region-at-or-before (regions key value)
  "The index of the last of REGIONS whose KEY is at or before VALUE, or -1."
  (1- (tuatara.runtime:bisect (mid 0 (length regions)) (<= (getf (aref regions mid) key) value))))

(defun regions-as-begun (regions)
  "REGIONS (REWRITE-REGIONS) as they were when the job began: each stretch's
text now taken to be its text then, so that REWRITE-MAPS maps the text the job
began on to its result with those stretches left as they were."
  (map 'simple-vector
       (lambda (r) (list* :start (getf r :base-start) :end (getf r :base-end) r))
       regions))

(defun result-as-begun (job regions)
  "JOB's result with the stretches of REGIONS (REWRITE-REGIONS), those edited
while it ran, left as they were when it began, for the edits made meanwhile
to be made again on (TUATARA.BUFFER:BUFFER-CHANGE-WHERE-BEGUN)."
  (let ((base (rewrite-rope job)))
    (if (zerop (length regions))
        (rewrite-result job)
        (tuatara.text:apply-edits
         (rewrite-result job)
         (map 'simple-vector
              (lambda (r)
                (tuatara.text:make-edit (getf r :out-start) (getf r :out-end)
                                        (and (< (getf r :base-start) (getf r :base-end))
                                             (tuatara.text:rope-octets base :start (getf r :base-start) :end (getf r :base-end)))))
              regions)))))

(defun rewrite-maps (job regions)
  "Two functions of (POSITION BIAS): from the buffer's text now to the text
taken in (JOB's result with REGIONS' stretches as edited), and back."
  (let* ((n (length regions))
         (grown (make-array (1+ n) :element-type 'fixnum :initial-element 0)) ; now vs read, through region k-1
         (shifted (make-array (1+ n) :element-type 'fixnum :initial-element 0)) ; taken in vs written
         (final-starts (make-array n :element-type 'fixnum :initial-element 0)))
    (dotimes (k n)
      (let* ((r (aref regions k))
             (now (- (getf r :end) (getf r :start))))
        (setf (aref final-starts k) (+ (getf r :out-start) (aref shifted k))
              (aref grown (1+ k)) (+ (aref grown k) (- now (- (getf r :base-end) (getf r :base-start))))
              (aref shifted (1+ k)) (+ (aref shifted k) (- now (- (getf r :out-end) (getf r :out-start)))))))
    (values
     (lambda (position bias)
       (let ((k (last-region-at-or-before regions :start position)))
         (if (and (>= k 0) (<= position (getf (aref regions k) :end)))
             (+ (aref final-starts k) (- position (getf (aref regions k) :start)))
             (+ (rewrite-map job (- position (aref grown (1+ k))) bias) (aref shifted (1+ k))))))
     (lambda (position bias)
       (let ((k (1- (tuatara.runtime:bisect (mid 0 n) (<= (aref final-starts mid) position)))))
         (if (and (>= k 0) (<= position (+ (aref final-starts k)
                                          (- (getf (aref regions k) :end) (getf (aref regions k) :start)))))
             (+ (getf (aref regions k) :start) (- position (aref final-starts k)))
             (+ (rewrite-unmap job (- position (aref shifted (1+ k))) bias) (aref grown (1+ k)))))))))

(defun rewrite-take-in (job)
  "Swap JOB's result into its buffer as one undoable step, with the stretches
edited meanwhile as they are now, unless the buffer may no longer take it
\(RESULT-REFUSED-P). Returns the number of matches replaced."
  (multiple-value-bind (matches lines) (rewrite-totals job)
    (let ((buffer (rewrite-buffer job))
          (seconds (tuatara.runtime:seconds-since (rewrite-started job))))
      (cond
        ((eq (rewrite-tracker job) :all)
         (message "The text was replaced meanwhile; ~:[the replacement ~a was~;~:*~(~a~) ~a was~] not applied"
                  (rewrite-doing job) (rewrite-words job))
         0)
        ((zerop matches)
         (if (rewrite-report job)
             (message "~a" (funcall (rewrite-report job) job :none 0 seconds 0))
             (message "No match ~a" (rewrite-words job)))
         0)
        (t
         (multiple-value-bind (regions taken) (rewrite-regions job)
           (let* ((now (tuatara.buffer:buffer-text buffer))
                  (edits (remove nil (map 'list (lambda (r)
                                                  (let ((text (and (< (getf r :start) (getf r :end))
                                                                   (tuatara.text:rope-octets now :start (getf r :start) :end (getf r :end)))))
                                                    (unless (and (= (getf r :out-start) (getf r :out-end)) (null text))
                                                      (tuatara.text:make-edit (getf r :out-start) (getf r :out-end) text))))
                                          regions)))
                  (final (if edits
                             (tuatara.text:apply-edits (rewrite-result job) (coerce edits 'simple-vector))
                             (rewrite-result job))))
             ;; made read-only, or turned to the hex view, while it ran: the change is not forced on it
             (when (result-refused-p buffer (format nil "~:[the replacement ~a~;~:*~(~a~) ~a~]" (rewrite-doing job) (rewrite-words job))
                                     (- (tuatara.text:rope-bytes final) (tuatara.buffer:buffer-size buffer)))
               (return-from rewrite-take-in 0))
             (multiple-value-bind (forward backward) (rewrite-maps job regions)
               (let ((unmoved (and (rewrite-goal job) (null edits) (eql (rewrite-point job) (tuatara.buffer:buffer-point buffer)))))
                 (tuatara.display:call-keeping-window-starts
                  (buffer-windows buffer)
                  (lambda ()
                    ;; the change stands where the job began, before the edits made meanwhile, which
                    ;; are made again on the result with the stretches they touched as they are now
                    (multiple-value-bind (begun-forward begun-backward) (rewrite-maps job (regions-as-begun regions))
                      (unless (and (not (rewrite-session job))
                                   (tuatara.buffer:buffer-change-where-begun
                                    buffer (rewrite-since job) (rewrite-rope job) (result-as-begun job regions)
                                    begun-forward begun-backward final forward backward
                                    :join (rewrite-joins job)))
                        ;; on top: a session's typing joins it; a whole-text change made meanwhile cannot be carried over
                        (tuatara.buffer:buffer-swap-text buffer final forward backward)
                        (let ((joins (rewrite-joins job)))
                          (when (and joins (eq (cdr (tuatara.buffer:buffer-undo-stack buffer)) joins))
                            (tuatara.buffer:merge-undo-groups buffer (cdr joins))))))
                    forward))
                 (when unmoved
                   (setf (tuatara.buffer:buffer-point buffer)
                         (min (funcall (rewrite-goal job) buffer) (tuatara.text:rope-bytes final))))))
             (if (rewrite-report job)
                 (message "~a" (funcall (rewrite-report job) job :done (- matches taken) seconds taken))
                 (message "~a ~a ~a"
                          (replaced-words (- matches taken) lines) (rewrite-words job) (job-done-words seconds taken)))
             (- matches taken))))))))

(defmethod tuatara.runtime:job-take-in ((job rewrite))
  "Take in a replacement or count that has ended, on the editor's thread: its
result, its count, or why it stopped."
  (let ((buffer (rewrite-buffer job)))
    (when (rewrite-tracker-function job)
      (setf (tuatara.buffer:buffer-change-functions buffer)
            (remove (rewrite-tracker-function job) (tuatara.buffer:buffer-change-functions buffer))))
    (when (live-buffer-p buffer)
      (ecase (rewrite-status job)
        (:done (cond ((and (rewrite-count-only job) (rewrite-report job))
                      (let ((said (funcall (rewrite-report job) job :done 0 0 0)))
                        (when said (message "~a" said))))
                     ((rewrite-count-only job)
                      (multiple-value-bind (matches lines) (rewrite-totals job)
                        (message "~a ~a" (matches-on-lines-words matches lines) (rewrite-words job))))
                     (t (rewrite-take-in job))))
        ;; a job of its own kind may say more of how it stopped (a split's search past its limit)
        (:stopped (message "~a" (or (and (rewrite-report job) (funcall (rewrite-report job) job :stopped 0 0 0))
                                    (tuatara.runtime:job-ending-words job))))
        (:failed (message "~a" (tuatara.runtime:job-ending-words job)))))))

(defun job-edited-p (job)
  "True when JOB's buffer's text is no longer the text JOB reads: edited since
it began, or since the text it began on, where it was given
\(JOB-BEGINS-WHERE-GIVEN)."
  (or (not (eq (rewrite-base job) (tuatara.buffer:buffer-version (rewrite-buffer job))))
      (and (rewrite-tracker job) t)))

(defun job-pieces-shown (job from to)
  "The pieces of JOB over the text from FROM below TO as it is now, in order,
each (START END) there, followed from the text the job reads through the
edits made since (JOB-POSITION-NOW). The first is found by halving, so that
only the pieces shown are looked at."
  (let* ((pieces (rewrite-pieces job))
         (edited (job-edited-p job))
         (n (length pieces)))
    (flet ((here (position bias)
             (if edited (job-position-now job position bias) position)))
      (let ((lo 0) (hi n))
        (setf lo (tuatara.runtime:bisect (mid lo hi) (<= (here (rewrite-piece-end (aref pieces mid)) :before) from)) hi lo)
        (loop for i from lo below n
              for piece = (aref pieces i)
              for start = (here (rewrite-piece-start piece) :after)
              while (< start to)
              collect (list start (here (rewrite-piece-end piece) :before)))))))

(defun job-shading (job from to)
  "JOB's pieces over the text from FROM below TO, as ranges to shade
:JOB-QUEUED: the matches it will change there (JOB-MATCH-SHADING), or the
whole stretch when those cannot be told quickly; all of them until its result
is taken in, where its workers have been too, as the text shown is the text
before the change till then."
  (let ((stretches (loop for (start end) in (job-pieces-shown job from to)
                         when (< (max start from) (min end to))
                           collect (cons (max start from) (min end to)))))
    (multiple-value-bind (ranges told) (and stretches (job-match-shading job stretches))
      (if told
          ranges
          (loop for (start . end) in stretches collect (list start end :job-queued))))))

(defun job-match-shading (job stretches)
  "What JOB will change within STRETCHES, (START . END) of the text shown, as
ranges in :JOB-QUEUED (CHANGES-IN-VIEW, by the job's own mapper, in the text
it reads), and as a second value true when that could be told: for a
replacement only (REWRITE-LIGHTS-MATCHES), not when the pattern is too slow on
this text to light it. After edits made since the job began, the stretches
are taken back to the text it reads (JOB-POSITION-READ) and its matches
followed forward to the text now (JOB-POSITION-NOW), a match an edit reached
into left out, so that only what will change is lit whatever was typed
meanwhile, at a cost that does not grow with the typing."
  (when (rewrite-lights-matches job)
    (let ((edited (job-edited-p job)))
      (multiple-value-bind (ranges slow)
          (changes-in-view (rewrite-rope job)
                           (lambda (function from to)
                             (funcall (rewrite-mapper job)
                                      (lambda (a z m g) (declare (ignore g)) (unless (eq m :passed) (funcall function a z)) nil)
                                      from to nil))
                           (if edited
                               (loop for (start . end) in stretches
                                     collect (cons (job-position-read job start :before) (job-position-read job end :after)))
                               stretches)
                           (rewrite-all job) :low (rewrite-low job))
        (unless slow
          (values (loop for (start end) in ranges
                        for here-start = (if edited (job-position-now job start :after) start)
                        for here-end = (if edited (job-position-now job end :before) end)
                        ;; a match an edit reached into is no longer what the job will change
                        when (= (- here-end here-start) (- end start))
                          collect (list here-start here-end :job-queued))
                  t))))))

(defun job-position-now (job position bias)
  "Where POSITION in the text JOB reads is in its buffer's text now, through
the stretches edited since it began (its tracker, REWRITE-NOTE-BATCH): moved
by what those before it grew or shrank, one it lies within going to that
stretch's start, or with BIAS :AFTER its end. A few stretches walked however
much was typed; after a change of the whole text, or for a job that does not
follow the edits (a count), through every version since
\(TUATARA.TEXT:MAP-FORWARD)."
  (let ((tracker (rewrite-tracker job)))
    (if (or (eq tracker :all) (null (rewrite-tracker-function job)))
        (tuatara.text:map-forward position (rewrite-base job) (tuatara.buffer:buffer-version (rewrite-buffer job)) bias)
        (let ((delta 0))
          (dolist (d tracker (+ position delta))
            (cond ((< position (dirty-base-start d)) (return (+ position delta)))
                  ((or (< position (dirty-base-end d)) (and (= position (dirty-base-end d)) (eq bias :before)
                                                             (< (dirty-base-start d) position)))
                   (return (if (eq bias :after) (dirty-end d) (dirty-start d))))
                  (t (incf delta (- (- (dirty-end d) (dirty-start d)) (- (dirty-base-end d) (dirty-base-start d)))))))))))

(defun job-position-read (job position bias)
  "Where POSITION in JOB's buffer's text now is in the text JOB reads, from
before the stretches edited since it began, as JOB-POSITION-NOW goes the
other way: one it lies within going to that stretch's start there, or with
BIAS :AFTER its end. After a change of the whole text, or for a job that does
not follow the edits, the first place there that every version since carries
to POSITION or past it, found by halves."
  (let ((tracker (rewrite-tracker job)))
    (if (or (eq tracker :all) (null (rewrite-tracker-function job)))
        (let ((base (rewrite-base job)) (now (tuatara.buffer:buffer-version (rewrite-buffer job))))
          (tuatara.runtime:bisect (mid 0 (tuatara.text:rope-bytes (rewrite-rope job)))
            (< (tuatara.text:map-forward mid base now :after) position)))
        (let ((delta 0))
          (dolist (d tracker (- position delta))
            (cond ((< position (dirty-start d)) (return (- position delta)))
                  ((< position (dirty-end d))
                   (return (if (eq bias :after) (dirty-base-end d) (dirty-base-start d))))
                  (t (incf delta (- (- (dirty-end d) (dirty-start d)) (- (dirty-base-end d) (dirty-base-start d)))))))))))

(defun rewrite-highlights (editor window rows)
  "Job highlight function: the text a job running in WINDOW's buffer is
changing, in ROWS only, :JOB-QUEUED, only what the rows show looked at
\(SHOWN-RANGES: not the lines search filters hide between them, which may be
gigabytes). A count changes nothing, and is not shown."
  (declare (ignore editor))
  (let* ((buffer (tuatara.display:window-buffer window))
         (job (rewrite-of buffer)))
    (when (and job (not (rewrite-count-only job)) (plusp (length rows)))
      (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
            nconc (funcall (or (rewrite-shade job) #'job-shading) job from to)))))

(tuatara.runtime:define-job-words rewrite
  :doing (rewrite-doing-word job)
  :description (or (rewrite-what job) (format nil "~a ~a" (rewrite-doing-word job) (rewrite-words job)))
  :verb (or (rewrite-verb job) (if (rewrite-count-only job) "count" "replace"))
  :left-words (or (rewrite-leaves job) (if (rewrite-count-only job) "nothing counted" "nothing replaced")))

(defmethod tuatara.runtime:job-percent ((job rewrite))
  "How far a replacement has gone, counting first and then replacing, as a percentage."
  (tuatara.runtime:two-phase-percent (rewrite-progress job) (rewrite-counting job) :halves (not (rewrite-count-only job))))

(defmethod tuatara.runtime:job-state-words ((job rewrite))
  "What a replacement is doing now, and how far it has gone, in words."
  (tuatara.runtime:two-phase-words (rewrite-doing-word job) (rewrite-progress job) (rewrite-counting job)))

;;; Starting and stopping

(defun rewrite-description (line words flags)
  "What replacing, or with the n flag counting, LINE's matches WORDS says of
where does, in full, for the list of jobs."
  (if (find #\n flags)
      (format nil "Counting ~a ~a" (search-line-pattern line) words)
      (format nil "Replacing ~a with ~a ~a" (search-line-pattern line) (search-line-replacement line) words)))

(defun launch-rewrite (job name details &key since made)
  "Start JOB, a REWRITE, on a thread called NAME (TUATARA.RUNTIME:START-JOB-THREAD),
DETAILS reported with its start: the edits made to its buffer meanwhile are
followed when it writes a result. SINCE, when given, is the undo stack where
JOB's text stood, MADE the batches made since, the oldest first, followed as
edits made meanwhile (JOB-BEGINS-WHERE-GIVEN). Returns JOB."
  (unless (rewrite-count-only job)
    (setf (rewrite-since job) (or (rewrite-joins job) since (tuatara.buffer:buffer-undo-stack (rewrite-buffer job))))
    (dolist (batch made) (rewrite-note-batch job batch))
    (let ((function (lambda (b batch) (declare (ignore b)) (rewrite-note-batch job batch))))
      (setf (rewrite-tracker-function job) function)
      (push function (tuatara.buffer:buffer-change-functions (rewrite-buffer job)))))
  (tuatara.runtime:start-job-thread job #'rewrite-run :name name :details details))

(defun spans-meet-p (spans s e)
  "True when one of SPANS, a vector of (START . END) in order, meets the
stretch from S to E, the ends of both included."
  (let ((i (tuatara.runtime:bisect (mid 0 (length spans)) (< (cdr (aref spans mid)) s))))
    (and (< i (length spans)) (<= (car (aref spans i)) e))))

(defun changes-in-view (rope finder stretches all &key (low 0))
  "What a replacement changes within STRETCHES, (START . END) of ROPE's text
shown, as (START END) ranges in order: the matches FINDER finds, a function
of (FUNCTION FROM TO) calling FUNCTION with each match's start and end from
FROM below TO; without ALL the first on each line only, looked for from the
line's start (not before LOW, where the replacement begins); a match of no
length shown on the character after it. As a second value :SLOW, the ranges
NIL, when the pattern is too slow on this text to tell them. The one way the
shading of a replacement, running or waiting, finds what it marks."
  (multiple-value-bind (ranges cut)
      (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
        (let ((out '()) (size (tuatara.text:rope-bytes rope)))
          (loop for (start . end) in stretches
                do (let ((allowed (if all start (max low (tuatara.text:rope-line-start rope start)))))
                     (funcall finder
                              (lambda (a z)
                                (when (>= a allowed)
                                  (when (and (>= a start) (< a end))
                                    (push (list a (if (< a z) (min z end) (min size (1+ a)))) out))
                                  (unless all (setf allowed (next-line-start-after rope z a)))))
                              allowed end)))
          (nreverse out)))
    (if (eq cut :slow) (values nil :slow) (values ranges nil))))

(defun matches-shade (buffer line regex flags)
  "A SHADE (QUEUE-IF-BUSY) for LINE's replacement waiting its turn in BUFFER:
what it will change in the text shown, as the text stands now
\(CHANGES-IN-VIEW); the whole stretch when the pattern is too slow on this
text to light it; nothing for a count."
  (let ((matcher (search-line-matcher line regex buffer))
        (all (find #\g flags)))
    (lambda (positions from to)
      (destructuring-bind (low high &rest more) positions
        (declare (ignore more))
        (let ((rope (tuatara.buffer:buffer-text buffer))
              (start (max low from)) (end (min high to)))
          (when (and (< start end) (not (find #\n flags)))
            (multiple-value-bind (ranges slow)
                (changes-in-view rope (lambda (function from to)
                                        (loop for (a z) in (line-matches rope matcher regex from to)
                                              do (funcall function a z)))
                                 (list (cons start end)) all :low low)
              (if slow (list (list start end)) ranges))))))))

(defun start-rewrite (buffer line regex low high words flags &key slow skip joins report)
  "Start replacing, or with the n flag counting, LINE's matches between LOW and
HIGH of BUFFER in the background (see this file's header), and say so, that
the pattern is slow on this text first when SLOW is true; the matches a fold
hides are replaced too, the folds kept. SKIP, a vector of (START . END) in
order, are lines left as they are, those replaced in sight already; the
job's change then joins JOINS for undo, and REPORT says how it ended (both as
REWRITE takes them). Returns the job, or
:QUEUED when another job runs or waits in BUFFER: it then waits its turn
(QUEUE-IF-BUSY), LOW and HIGH followed meanwhile. A replacement in a text
whose lines are still being counted, a large file just opened or the result
of a replacement before it, waits its turn the same way until they are,
the editor answering keys meanwhile."
  (when (queue-if-busy buffer (rewrite-description line words flags)
                       (list (cons low :before) (cons high :after))
                       (lambda (low high) (start-rewrite buffer line regex low high words flags :slow slow
                                                                                                    :skip skip :joins joins :report report))
                       :shade (matches-shade buffer line regex flags)
                       :counted (not (find #\n flags)))
    (return-from start-rewrite :queued))
  (unless (find #\n flags)
    (check-hidden-lines buffer low high)
    (tuatara.buffer:ensure-indexed buffer))
  (let* ((matcher (search-line-matcher line regex buffer))
         (count-only (and (find #\n flags) t)))
    ;; what the replacement needs of each match: no more is copied from the text
    (multiple-value-bind (expand texts)
        (if count-only (values nil nil) (rewrite-expander (search-line-replacement line) regex (matcher-fold matcher)
                                                             (tuatara.buffer:buffer-newline buffer)))
      (multiple-value-bind (rope low high since made)
          (if (or count-only joins)
              (values (tuatara.buffer:buffer-text buffer) low high nil '())
              (job-begins-where-given buffer low high))
      (let* ((job (%make-rewrite :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
                                 :low low :high high :words words :what (rewrite-description line words flags)
                                 :mapper (let ((mapper (rewrite-mapper-for rope matcher texts high)))
                                           (if skip
                                               (lambda (function from to halt)
                                                 (funcall mapper (lambda (s e m g)
                                                                   (if (spans-meet-p skip s s) (pass-over function s e) (funcall function s e m g)))
                                                          from to halt))
                                               mapper))
                                 :joins joins :report report :lights-matches (not count-only)
                                 :expand expand
                                 :all (and (find #\g flags) t) :count-only count-only
                                 :pieces (rewrite-pieces-for rope low high)
                                 :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*
                                 :directory (unless count-only (tuatara.io:buffer-scratch-directory buffer)))))
        (launch-rewrite job "tuatara rewrite"
                        (format nil "octets ~:d to ~:d in ~d piece~:p" low high (length (rewrite-pieces job)))
                        :since since :made made)
        (let ((doing (format nil "~:[replacing~;counting~] ~a" count-only words)))
          (say-in-background (if slow (format nil "The pattern is slow here: ~a" doing) (string-capitalize doing :end 1))))
        job)))))

(defun line-piece-run (job piece per-line crlf)
  "Rewrite PIECE of JOB, a job over lines (START-LINE-JOB), into an unnamed
file: the text PER-LINE makes of each line (CRLF when a carriage return before
a newline ends them), read a block at a time and written a megabyte at a time,
so that no line looks anything up in the text."
  (let* ((rope (rewrite-rope job))
         (total (tuatara.text:rope-bytes rope))
         (start (rewrite-piece-start piece))
         (end (rewrite-piece-end piece))
         (cancel (rewrite-cancel job))
         (writer (start-piece job piece))
         (interval (rewrite-checkpoint job))
         (next-checkpoint (+ start interval))
         (changes 0) (lines 0)
         ;; the line under way, and the block it was read in
         (pos 0) (block nil) (block-start 0) (block-end 0))
    (declare (type fixnum total start end next-checkpoint changes lines pos block-start block-end))
    (flet ((edit (a z groups octets)
             ;; what PER-LINE changes: the text before it as it is, then the change
             (declare (ignore groups) (type fixnum a z))
             (write-text writer rope pos a block block-start block-end)
             (tuatara.io:writer-octets writer octets)
             (setf pos z)
             (incf changes)))
      (when (< start end)
        (map-piece-lines
         (lambda (line line-end rope octets octets-start octets-end)
           (declare (type fixnum line line-end octets-start octets-end))
           (setf block octets block-start octets-start block-end octets-end pos line)
           (let ((next (slice-next-line rope total crlf line-end block block-start block-end)))
             (declare (type fixnum next))
             (funcall per-line #'edit line line-end next rope block block-start block-end)
             (write-text writer rope pos next block block-start block-end)
             (incf lines)
             (setf (rewrite-piece-done piece) next)
             (when (>= next next-checkpoint)
               (piece-checkpoint piece next (piece-written piece writer) start)
               (setf next-checkpoint (+ next interval)))))
         rope start end crlf (lambda () (car cancel))))
      (unless (car cancel)
        (finish-piece piece writer changes lines)))))

(defun line-octets (rope line line-end block block-start block-end)
  "Two values: octets holding the text of ROPE from LINE below LINE-END, and
where it starts in them: BLOCK, the octets from BLOCK-START below BLOCK-END,
when it holds it, else the text read now."
  (declare (type fixnum line line-end block-start block-end))
  (if (and (>= line block-start) (<= line-end block-end))
      (values block (- line block-start))
      (values (tuatara.text:rope-octets rope :start line :end line-end) 0)))

(defun map-piece-lines (function rope start end crlf &optional halt)
  "Call FUNCTION with (LINE LINE-END ROPE BLOCK BLOCK-START BLOCK-END) for each
line of ROPE starting from START below END, read a block at a time (CRLF when
a carriage return before a newline ends them), the block one of a megabyte
kept for reading into (TUATARA.TEXT:WITH-OCTET-BLOCK), which FUNCTION must not
keep; stop when HALT, checked now and then, answers true."
  (let ((n 0))
    (declare (type fixnum n))
    (tuatara.text:with-octet-block (reused (* 1024 1024))
      (block lines
        (tuatara.text:map-line-slices
         (lambda (line s e sc ec line-end block block-start block-end)
           (declare (ignore s e sc ec))
           (when (and halt (zerop (logand (incf n) 1023)) (funcall halt)) (return-from lines))
           (funcall function line line-end rope block block-start block-end))
         rope start end 0 0 :crlf crlf :octets t :block reused)))))

(defun start-line-job (buffer from to per-line &key doing what verb leaves report announce prepare goal)
  "Rewrite the lines of BUFFER from the one starting at FROM to the one ending
at TO, in the background on all but two cores, as PER-LINE says, a line at a
time: a function of (FUNCTION LINE LINE-END NEXT ROPE BLOCK BLOCK-START
BLOCK-END) called for each line, its start, its end (before the line's end:
its newline, or with CRLF the carriage return), the next line's start, and the
text with the octets read around the line (BLOCK-OCTET reads them), which
calls FUNCTION with (START END NIL OCTETS) for each change it makes, in order,
within the line and its line end. DOING names the work in a word, WHAT in
full, for the list of jobs; VERB as a verb, for when it fails, and LEAVES what
stopping it leaves (REWRITE); REPORT says how it ended; ANNOUNCE, the work in
words, is said to go on in the background as it starts (SAY-IN-BACKGROUND);
PREPARE, when given, runs first on the job's thread (REWRITE); GOAL, when
given, a function of FROM as the job starts, gives the job's GOAL (REWRITE).
One change for undo, edits made meanwhile kept, \\[stop-job] stops it. Returns
the job, or :QUEUED when another job runs or waits in BUFFER, or its lines are
still being counted: it then waits its turn, FROM and TO followed meanwhile."
  (when (queue-if-busy buffer what (list (cons from :before) (cons to :after))
                       (lambda (from to) (start-line-job buffer from to per-line :doing doing :what what
                                                                                    :verb verb :leaves leaves
                                                                                    :report report :announce announce
                                                                                    :prepare prepare :goal goal))
                       :counted t)
    (return-from start-line-job :queued))
  (tuatara.buffer:ensure-indexed buffer)
  (multiple-value-bind (rope from to since made) (job-begins-where-given buffer from to)
  (let* ((total (tuatara.text:rope-bytes rope))
         (crlf (buffer-crlf-p buffer))
         (high (min total (1+ (tuatara.text:rope-line-end rope to))))
         (job (%make-rewrite :buffer buffer :base (tuatara.buffer:buffer-version buffer) :rope rope
                             :low from :high high :words "the lines" :what what :doing doing :prepare prepare
                             :verb verb :leaves leaves
                             :goal (and goal (funcall goal from)) :point (tuatara.buffer:buffer-point buffer)
                             :mapper (lambda (function start end halt)
                                       (map-piece-lines (lambda (line line-end rope block block-start block-end)
                                                          (when (>= line start)
                                                            (funcall per-line function line line-end
                                                                     (slice-next-line rope total crlf line-end block block-start block-end)
                                                                     rope block block-start block-end)))
                                                        rope (tuatara.text:rope-line-start rope start) (min end high) crlf halt))
                             :expand (lambda (m g) (declare (ignore m)) g)
                             :runner (lambda (job piece) (line-piece-run job piece per-line crlf))
                             :all t :report report
                             :pieces (rewrite-pieces-for rope from high)
                             :checkpoint *rewrite-checkpoint* :hook *rewrite-piece-hook*
                             :directory (tuatara.io:buffer-scratch-directory buffer))))
    (launch-rewrite job "tuatara lines" (format nil "octets ~:d to ~:d" from high) :since since :made made)
    (when announce (say-in-background announce))
    job)))

(defun rewrite-doing-word (job)
  "What JOB is doing, in a word: Replacing, Counting, or its own DOING."
  (or (rewrite-doing job) (if (rewrite-count-only job) "Counting" "Replacing")))

(defun install-rewrites ()
  "Show the text background replacements are changing."
  (pushnew 'rewrite-highlights *job-highlight-functions*))
