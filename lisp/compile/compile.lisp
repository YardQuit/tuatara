;;;; lisp/compile/compile.lisp --- Running a build or test command and stepping through the places its output names: compile.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x compile runs a command typed in the minibuffer with the shell, from
;;;; the root of the project the buffer's file is in, and shows what it prints
;;;; in *compile* as it comes. The program runs as a job: a thread of its own
;;;; reads its output a piece at a time and finds the lines naming a file and
;;;; a line (GCC's, Clang's, rustc's, Python's, grep -n's) and those saying an
;;;; error, a warning or a note; the editor takes the pieces in between keys,
;;;; a few megabytes at most at once, so that typing never waits however much
;;;; comes, and the thread waits when the editor is that far behind. The
;;;; command runs in a session of its own with no terminal, so that a program
;;;; asking for a password cannot reach the editor's, and stopping it ends
;;;; every program of that session, killed when it does not end. RET on a
;;;; line naming a place opens it, and the output is the list of places M-g n
;;;; and M-g p go through (lisp/files/next-place.lisp). Running a build runs
;;;; the project's own code, so a project not trusted is asked about first
;;;; (lisp/files/trust.lisp); nothing of any file is run, only the command
;;;; typed. As Emacs's compile, whose idea it is.

(in-package #:tuatara.defaults)

(defvar *compile-command* "make -k "
  "The command \\[compile] offers the first time, before you have typed one; after
that it offers the one typed last. Run with your shell ($SHELL, else /bin/sh)
from the root of the project the buffer's file is in (*PROJECT-ROOT-MARKERS*),
else the buffer's directory.")

(defvar *compile-output-most* (* 256 1024 1024)
  "The most octets of a command's output \\[compile] keeps in *compile*: 256 MB as
standard. A command printing more (one that never ends, a loop gone wrong) is
read on to its end and counted, but what comes past this is left out, and the
last line says how much. NIL keeps all of it.")

(defvar *compile-history* '()
  "The commands typed for \\[compile] in this session, the latest first: \\<minibuffer>\\[minibuffer-previous-history] and \\[minibuffer-next-history] go through them.")

(defparameter *compile-piece* 65536
  "The most octets read at once from a compile's output.")

(defparameter *compile-taken-at-once* (* 4 1024 1024)
  "The most octets of a compile's output put into its buffer between two keys:
the rest waits for the next, so that typing never waits on a flood of output.")

(defparameter *compile-waiting-most* (* 32 1024 1024)
  "The most octets of a compile's output read and not yet in its buffer: the
program waits to write more until the editor has taken some in.")

(defparameter *compile-rope-least* 8192
  "The fewest octets of a piece of a compile's output that go into its buffer
as they were read, sharing their memory, rather than copied.")

(defvar *compiles-ending* '()
  "The compiles that have ended whose output is still being put in, a part
at each tick, before the line saying how they ended.")

(defparameter *compile-line-looked-at* 4096
  "How many octets of each line of a compile's output are looked at for a file
and line it names: a longer line's rest is shown but not read for one.")

(defparameter *compile-line-read* 512
  "How many octets at the start of a line of a compile's output are read for
the kind of line it is and the place it names.")

;;; Lines that name a place

(defparameter *compile-line-scanners*
  (flet ((scanner (pattern) (cl-ppcre:create-scanner pattern)))
    (list
     ;; GCC and Clang: file:line:col: error: what
     (list :kinded (scanner "^([^\\s:\"'(][^\\s:]*):(\\d{1,9})(?::(\\d{1,9}))?:\\s*(fatal error|error|warning|note|remark)\\b"))
     ;; rustc and cargo: --> file:line:col, under the line saying what is wrong
     (list :arrow (scanner "^\\s*--> ([^\\s:][^\\s:]*):(\\d{1,9})(?::(\\d{1,9}))?"))
     ;; Python's tracebacks: File "x", line n
     (list :traceback (scanner "^\\s*File \"([^\"<][^\"]*)\", line (\\d{1,9})"))
     ;; Rust's panics, a test's failing among them: thread 'name' panicked at file:line:col
     ;; (before Rust 1.73, the message in quotes and a comma before the place)
     (list :panic (scanner "^thread '[^']*' panicked at (?:'.*', )?([^\\s:'][^\\s:]*):(\\d{1,9}):(\\d{1,9})"))
     ;; TypeScript's tsc, C# and MSVC: file(line,col): error
     (list :parenthesised (scanner "^((?=[^\\s(]*[./])[^\\s(\"'][^\\s(]*)\\((\\d{1,9})(?:,(\\d{1,9}))?\\)\\s*:\\s*(fatal error|error|warning)\\b"))
     ;; what is wrong, without a place: rustc's error[E0308]:, SBCL's ; caught WARNING:, make's ***
     (list :error (scanner "^(?:(?:fatal )?error(?:\\[[^\\]]*\\])?:|; caught ERROR:|(?:g?make|ninja)(?:\\[\\d+\\])?: \\*\\*\\*|FAILED:|Traceback \\(most recent call last\\):|[A-Za-z_][\\w.]*(?:Error|Exception): |.*\\*\\*\\* .*Stop\\.$)"))
     (list :warning (scanner "^(?:warning(?:\\[[^\\]]*\\])?:|; caught WARNING:)"))
     (list :note (scanner "^(?:note:|help:|; caught STYLE-WARNING:)"))
     ;; grep -n and the rest: file:line: or file:line:col:, of a file that is there
     (list :plain (scanner "^([^\\s:\"'(][^\\s:]*):(\\d{1,9}):(?:(\\d{1,9}):)?"))))
  "The patterns lines of a compile's output are matched against, in order,
each (WHAT SCANNER): :KINDED for file:line:col: and error, warning or note;
:ARROW for rustc's --> file:line:col; :TRACEBACK for Python's File \"x\",
line n; :PANIC for Rust's thread 'x' panicked at file:line:col (a test
failing); :PARENTHESISED for file(line,col): error or warning (tsc, C#,
MSVC), the file's name holding a . or /; :ERROR, :WARNING and :NOTE for a
line saying one, naming no place; :PLAIN for file:line: (grep -n). Lines
and columns have nine digits at most.")

(defparameter *make-directory-scanner*
  (cl-ppcre:create-scanner "^g?make(?:\\[\\d+\\])?: (Entering|Leaving) directory [`'‘](.+)['’]$")
  "The pattern of make's lines saying it goes into a directory and comes out of it.")

(defun output-line-words (line end)
  "Where the first two words of LINE, below END, are, as four values: where
the first starts, where it ends, where the second starts (END for none), and
where the first colon in the first word is, NIL for none."
  (declare (type simple-string line) (type fixnum end) (optimize speed))
  (flet ((blank-p (c) (or (char= c #\Space) (char= c #\Tab))))
    (let* ((first (loop for i of-type fixnum from 0 below end unless (blank-p (schar line i)) return i finally (return end)))
           (token-end (loop for i of-type fixnum from first below end when (blank-p (schar line i)) return i finally (return end))))
      (values first token-end
              (loop for i of-type fixnum from token-end below end unless (blank-p (schar line i)) return i finally (return end))
              (loop for i of-type fixnum from first below token-end when (char= #\: (schar line i)) return i)))))

(defun line-may-be (what line end first token-end second colon)
  "True when LINE, below END, may be a line of the kind WHAT names in
*COMPILE-LINE-SCANNERS*, its words where OUTPUT-LINE-WORDS says (FIRST,
TOKEN-END, SECOND and COLON): what such a line must have, looked for in those
words without matching a pattern, so that most lines of output are passed
over at once, nothing made for them. A file:line: of no kind must begin
with a letter, a . / or ~, or have a . or / in its name."
  (declare (type simple-string line) (type fixnum end first token-end second) (optimize speed))
  (flet ((starts (prefix &optional (at 0))
           (declare (type simple-string prefix) (type fixnum at))
           (let ((stop (+ at (length prefix))))
             (and (<= stop end) (string= prefix line :start2 at :end2 stop))))
         (ends (suffix &optional (at token-end))
           (declare (type simple-string suffix) (type fixnum at))
           (let ((from (- at (length suffix))))
             (and (>= from first) (string= suffix line :start2 from :end2 at))))
         (numbered () (and colon (< (1+ (the fixnum colon)) token-end) (digit-char-p (schar line (1+ (the fixnum colon)))))))
    (ecase what
      (:kinded (and (numbered) (or (starts "error" second) (starts "fatal error" second) (starts "warning" second)
                                   (starts "note" second) (starts "remark" second))))
      (:arrow (starts "-->" first))
      (:traceback (starts "File \"" first))
      (:panic (starts "thread '" first))
      ;; file(line,col): error, file(line):error, or MSVC's file(line) : error; a . or / in the
      ;; file's name, so that words such as worker(42): error: are none
      (:parenthesised
       (let ((open (position #\( line :start first :end token-end)))
         (and open
              (find-if (lambda (c) (or (char= c #\.) (char= c #\/))) line :start first :end open)
              (let* ((close (position #\) line :start open :end token-end))
                     (kind (cond ((null close) nil)
                                 ((< (1+ close) token-end)
                                  (and (char= #\: (schar line (1+ close)))
                                       (if (< (+ 2 close) token-end) (+ 2 close) second)))
                                 ((starts ":" second)
                                  (or (position-if-not (lambda (c) (or (char= c #\Space) (char= c #\Tab))) line
                                                       :start (1+ second) :end end)
                                      end)))))
                (and kind (or (starts "error" kind) (starts "fatal error" kind) (starts "warning" kind)))))))
      (:error (or (starts "error") (starts "fatal error") (starts "; caught ERROR") (starts "make") (starts "gmake")
                  (starts "ninja") (starts "FAILED:") (starts "Traceback") (ends "Error:") (ends "Exception:")
                  (ends "Stop." end)))
      (:warning (or (starts "warning") (starts "; caught WARNING")))
      (:note (or (starts "note:") (starts "help:") (starts "; caught STYLE-WARNING")))
      (:plain (and (numbered)
                   (or (alpha-char-p (schar line first)) (find (schar line first) "./~")
                       (loop for i of-type fixnum from first below colon
                             thereis (or (char= #\. (schar line i)) (char= #\/ (schar line i))))))))))

(defun make-directory-line (line end)
  "When LINE, below END, is make's saying it goes into a directory or comes
out of it, :ENTERING or :LEAVING and the directory, as two values; else NIL."
  (when (and (or (uiop:string-prefix-p "make" line) (uiop:string-prefix-p "gmake" line))
             (search "directory" line :end2 end))
    (multiple-value-bind (start match-end starts ends) (cl-ppcre:scan *make-directory-scanner* line :end end)
      (declare (ignore match-end))
      (when start
        (values (if (string= "Entering" line :start2 (aref starts 0) :end2 (aref ends 0)) :entering :leaving)
                (subseq line (aref starts 1) (aref ends 1)))))))

(defun output-line-place (line &key kind-before (end (length line)) exists)
  "What LINE, a line of a compile's output, names, as four values: its kind,
:ERROR, :WARNING, :NOTE or NIL; and the file (as written), line and column
\(from 1, NIL when not given) of the place it names, NIL for none. A line
naming neither gives NIL. KIND-BEFORE is the kind of the last line saying
what is wrong without a place, for a place under it (rustc's --> lines), an
error when there was none. Only LINE's characters below END are looked at.
A file:line: of no kind (grep -n's) names a place only when EXISTS, a
function of the file's name as written, says it is there, so that a time
\(12:30:45:), a date or an address and port is none; with no EXISTS, none
is. Python's File \"<string>\" and the like name no file."
  (multiple-value-bind (first token-end second colon) (output-line-words line end)
    (loop for (what scanner) in *compile-line-scanners*
          do (multiple-value-bind (start match-end starts ends) (and (line-may-be what line end first token-end second colon)
                                                                     (cl-ppcre:scan scanner line :end end))
               (declare (ignore match-end))
               (when start
                 (flet ((group (i) (and (aref starts i) (subseq line (aref starts i) (aref ends i))))
                        (number (i) (and (aref starts i) (parse-integer line :start (aref starts i) :end (aref ends i)))))
                   (case what
                     (:kinded (let ((word (group 3)))
                                (return (values (cond ((search "error" word) :error)
                                                      ((string= word "warning") :warning)
                                                      (t :note))
                                                (group 0) (number 1) (number 2)))))
                     (:arrow (return (values (or kind-before :error) (group 0) (number 1) (number 2))))
                     (:traceback (return (values :error (group 0) (number 1) nil)))
                     (:panic (return (values :error (group 0) (number 1) (number 2))))
                     (:parenthesised (return (values (if (search "error" (group 3)) :error :warning)
                                                     (group 0) (number 1) (number 2))))
                     ((:error :warning :note) (return what))
                     (:plain (let ((file (group 0)))
                               (when (and exists (funcall exists file))
                                 (return (values nil file (number 1) (number 2)))))))))))))

(defun place-code (length kind place)
  "A line of note of a compile's output packed in a fixnum: its LENGTH in
octets, its KIND (:ERROR, :WARNING, :NOTE or NIL), and PLACE true when it
names a file and line."
  (logior (ash length 3) (ash (case kind (:error 1) (:warning 2) (:note 3) (t 0)) 1) (if place 1 0)))

(defun code-length (code) "The length in octets of the line a PLACE-CODE stands for." (ash code -3))
(defun code-place-p (code) "True when the line a PLACE-CODE stands for names a place." (logbitp 0 code))

(defun code-face (code)
  "The face of the line a PLACE-CODE stands for: :compile-error, :compile-warning or :compile-note; NIL for one of no kind."
  (case (ldb (byte 2 1) code) (1 :compile-error) (2 :compile-warning) (3 :compile-note)))

;;; A compile and its places

(defstruct (compile-run (:constructor make-compile-run (buffer command directory base)) (:copier nil))
  "A command run by \\[compile] and what its output names, a list of places
\(STEP-PLACE): BUFFER shows its output after BASE octets of heading; COMMAND
ran in DIRECTORY. STARTS, CODES and FOLDERS hold, in the order of the output,
each line of note (one naming a place, or saying an error, a warning or a
note): where it starts in BUFFER, its PLACE-CODE, and the directory its file
names are taken from, as an index into DIRECTORIES (the first DIRECTORY, the
others those make said it went into); written by the thread reading the
output under LOCK. INDEX is the line gone to last, -1 before any."
  buffer command directory base
  (lock (sb-thread:make-mutex :name "compile places"))
  (starts (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0))
  (codes (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0))
  (folders (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0))
  (directories (make-array 1 :adjustable t :fill-pointer 1 :initial-element directory))
  (index -1 :type fixnum))

(setf (documentation 'compile-run-p 'function) "True when OBJECT is a COMPILE-RUN."
      (documentation 'compile-run-buffer 'function) "The buffer showing a compile's output."
      (documentation 'compile-run-command 'function) "The command a compile ran."
      (documentation 'compile-run-directory 'function) "The directory a compile's command ran in."
      (documentation 'compile-run-base 'function) "The octets of heading before a compile's output in its buffer."
      (documentation 'compile-run-lock 'function) "The lock a compile's lines of note are written and read under."
      (documentation 'compile-run-starts 'function) "Where each line of note of a compile's output starts in its buffer."
      (documentation 'compile-run-codes 'function) "The PLACE-CODE of each line of note of a compile's output."
      (documentation 'compile-run-folders 'function) "The directory each line of note of a compile's output takes its file names from, an index into its DIRECTORIES."
      (documentation 'compile-run-directories 'function) "The directory a compile's command ran in, then those make said it went into."
      (documentation 'compile-run-index 'function) "The line of note of a compile gone to last, -1 before any.")

(defmethod places-buffer ((places compile-run))
  "The buffer of the compile's output: killed, its places are in force no more."
  (compile-run-buffer places))

(defun note-output-line (run start length kind place &optional (folder 0))
  "Keep a line of note of RUN's output, starting at octet START of its buffer,
LENGTH octets long, of KIND, naming a place when PLACE is true, its file names
taken from RUN's directory numbered FOLDER."
  (sb-thread:with-mutex ((compile-run-lock run))
    (vector-push-extend start (compile-run-starts run))
    (vector-push-extend (place-code length kind place) (compile-run-codes run))
    (vector-push-extend folder (compile-run-folders run))))

(defun run-folder (run directory)
  "The index among RUN's directories of DIRECTORY, a directory make said it
went into (taken from RUN's own when it does not start with /), added when new."
  (let ((native (tuatara.io:slash-ended (tuatara.io:clean-path (if (uiop:string-prefix-p "/" directory)
                                                                   directory
                                                                   (tuatara.io:path-join (compile-run-directory run) directory))))))
    (sb-thread:with-mutex ((compile-run-lock run))
      (let ((directories (compile-run-directories run)))
        (or (position native directories :test #'string=)
            (vector-push-extend native directories))))))

(defun run-marks-between (run from to size)
  "The parts of RUN's buffer, SIZE octets long now, from FROM below TO, that
lines of note of a kind cover, each (START END FACE). O(log lines + those
there)."
  (sb-thread:with-mutex ((compile-run-lock run))
    (let* ((starts (compile-run-starts run)) (codes (compile-run-codes run))
           (count (fill-pointer starts))
           ;; a line of note starting before FROM may reach into it: one looked at before
           (first (max 0 (1- (tuatara.runtime:bisect (i 0 count) (< (aref starts i) from))))))
      (loop for i from first below count
            for start = (aref starts i)
            for code = (aref codes i)
            for end = (min size (+ start (code-length code)))
            while (< start to)
            when (and (code-face code) (> end from) (< start end))
              collect (list (max start from) (min end to) (code-face code))))))

(defun compile-entry-at (buffer position)
  "The line of note of the compile shown in BUFFER that names a place, on the
line holding POSITION, as its index; NIL when that line names none."
  (let ((run (tuatara.buffer:buffer-local buffer 'compile-run)))
    (when run
      (let ((line-start (tuatara.text:rope-line-start (tuatara.buffer:buffer-text buffer) position)))
        (sb-thread:with-mutex ((compile-run-lock run))
          (let* ((starts (compile-run-starts run))
                 (i (tuatara.runtime:bisect (k 0 (fill-pointer starts)) (< (aref starts k) line-start))))
            (and (< i (fill-pointer starts)) (= (aref starts i) line-start)
                 (code-place-p (aref (compile-run-codes run) i))
                 i)))))))

(defun compile-place-path (run file &optional (folder 0))
  "The native name of FILE, as a compile's output names it, taken from RUN's
directory numbered FOLDER (where its command ran, or one make went into)
unless it starts with / or ~."
  (tuatara.io:clean-path
   (tuatara.io:native (if (or (uiop:string-prefix-p "/" file) (uiop:string-prefix-p "~" file))
                          (tuatara.io:expand-path file)
                          (tuatara.io:path-join (sb-thread:with-mutex ((compile-run-lock run))
                                                  (aref (compile-run-directories run) folder))
                                                file)))))

(defun compile-file-there-p (run file folder)
  "True when FILE, as a compile's output names it, is a file there, taken from RUN's directory numbered FOLDER."
  (eq :regular (tuatara.io:path-kind (compile-place-path run file folder))))

(defun goto-compile-place (run index &key other)
  "Open the file the line of note INDEX of RUN names, at its line and column,
in another window with OTHER, else in this one; the windows showing the
compile's output put on that line, and the line said."
  (let* ((buffer (compile-run-buffer run))
         (start (sb-thread:with-mutex ((compile-run-lock run)) (aref (compile-run-starts run) index)))
         (code (sb-thread:with-mutex ((compile-run-lock run)) (aref (compile-run-codes run) index)))
         (folder (sb-thread:with-mutex ((compile-run-lock run)) (aref (compile-run-folders run) index)))
         (size (tuatara.buffer:buffer-size buffer))
         (text (string-right-trim '(#\Return)
                                  (tuatara.buffer:buffer-string buffer :start start
                                                                       :end (min size (+ start (min (code-length code) *compile-line-looked-at*)))))))
    (setf (compile-run-index run) index)
    (dolist (window (buffer-windows buffer))
      (setf (tuatara.display:window-point window) start))
    (multiple-value-bind (kind file line column)
        (output-line-place text :exists (lambda (file) (compile-file-there-p run file folder)))
      (declare (ignore kind))
      (cond ((null file) (message "No place on this line"))
            ((show-file-line (compile-place-path run file folder) line :column column :other other)
             (message "~a" (tuatara.display:shorten-string text 200 :one-line t)))))))

(defmethod step-place ((places compile-run) direction)
  "Go to the next line of the compile's output naming a place (DIRECTION 1), or
the one before (-1), opening its file there: in another window when the
output is in the selected one, else in the selected one."
  (let ((buffer (compile-run-buffer places)))
    (if (not (live-buffer-p buffer))
        (message "~a has been killed" (tuatara.buffer:buffer-name buffer))
        (let* ((size (tuatara.buffer:buffer-size buffer))
               (found (sb-thread:with-mutex ((compile-run-lock places))
                        (let ((starts (compile-run-starts places)) (codes (compile-run-codes places)))
                          (loop for i = (+ (compile-run-index places) direction) then (+ i direction)
                                while (and (< -1 i (fill-pointer starts)) (< (aref starts i) size))
                                when (code-place-p (aref codes i)) return i)))))
          (cond (found (goto-compile-place places found
                                           :other (eq buffer (tuatara.display:window-buffer (current-window)))))
                ((and (plusp direction) (compile-under-way buffer))
                 (message "No more places yet: the compile goes on"))
                (t (say-places-end places direction)))))))

(defmethod places-name ((places compile-run))
  "The name of the buffer of the compile's output."
  (tuatara.buffer:buffer-name (compile-run-buffer places)))

;;; Reading the output

(defstruct (compile-job (:include tuatara.runtime:job) (:constructor make-compile-job (buffer run environment most)) (:copier nil))
  "A compile at work in BUFFER, a job: RUN is its COMPILE-RUN, ENVIRONMENT
what its program sees, MOST the most octets of its output kept
\(*COMPILE-OUTPUT-MOST* as it started). PIECES are the pieces read from its output and not
yet in BUFFER, octet vectors or ropes, the latest first, WAITING octets in
all, under LOCK; the reading waits on ROOM while too many wait. ASLEEP is
true once the editor has taken in all there was, to be woken for the next.
SESSION is the number of the session its command runs in (SESSION-COMMAND).
EXIT, how its command ended, (STATUS CODE) as SBCL gives them; ENDED, the
internal real time it did; LEFT-OUT, the octets past *COMPILE-OUTPUT-MOST*
not kept; LEFT-OPEN, true when a program it started held the output open
after it ended. ENDING is true once it has ended and the rest of its output
is being put in; THEN, a function of no arguments called once that is done
\(the compile asked for meanwhile)."
  run environment most
  (lock (sb-thread:make-mutex :name "compile output"))
  (room (sb-thread:make-waitqueue :name "compile output taken"))
  (pieces '()) (waiting 0 :type fixnum) (asleep t) session exit ended (left-out 0) left-open ending then)

(setf (documentation 'compile-job-p 'function) "True when OBJECT is a COMPILE-JOB.")

(tuatara.runtime:define-job-words compile-job
  :doing "Compiling"
  :description (format nil "Compiling: ~a" (tuatara.text:one-line (compile-run-command (compile-job-run job))))
  :verb "run the command"
  :left-words "the output so far"
  :at-exit :stop)

(defmethod tuatara.runtime:job-holds-queue-p ((job compile-job))
  "False: nothing else waits for a compile."
  nil)

(defparameter *compile-stop-grace* 2
  "Seconds the programs of a compile stopped have to end after being asked to
\(SIGTERM), before they are killed (SIGKILL).")

(defparameter *compile-open-grace* 1
  "Seconds a compile's output is read on after its command has ended, for what
the programs it started still write; then it ends, said held open.")

(defvar *compiles-stopping* '()
  "The compiles being stopped, each (PROCESS . SESSION), until all their
programs have ended: killed at once when the editor is left meanwhile
\(KILL-COMPILES-AT-EXIT).")

(defun compile-session-there-p (process session)
  "True while PROCESS, a compile's setsid, or a program of its SESSION has not ended."
  (or (tuatara.io:process-unreaped-p process) (and session (tuatara.io:session-processes session))))

(defun end-compile-session (job process)
  "End every program of JOB's session, PROCESS's, in the background: woken
when stopped (SIGCONT) and asked to end (SIGTERM), then killed (SIGKILL)
those not ended within *COMPILE-STOP-GRACE* seconds; none signalled once all
have ended."
  (let ((entry (cons process (or (compile-job-session job) (tuatara.io:session-leader process)))))
    (push entry *compiles-stopping*)
    (tuatara.runtime:make-guarded-thread
     (lambda ()
       (unwind-protect
            (destructuring-bind (process . session) entry
              (tuatara.io:signal-session process session sb-posix:sigcont)
              (tuatara.io:signal-session process session sb-posix:sigterm)
              (loop repeat (* 20 *compile-stop-grace*)
                    while (compile-session-there-p process session)
                    do (sleep 0.05))
              (when (compile-session-there-p process session)
                (tuatara.io:signal-session process session sb-posix:sigkill)))
         (setf *compiles-stopping* (remove entry *compiles-stopping*))))
     :name "tuatara compile stopping")))

(defun kill-compiles-at-exit (editor)
  "Exit function: kill every program of the compiles still running or being
stopped (SIGKILL) as the editor is left, so that none that will not end
outlives it."
  (declare (ignore editor))
  (let ((sessions (copy-list *compiles-stopping*)))
    (dolist (job (append tuatara.runtime:*jobs* *compiles-ending*))
      (when (and (compile-job-p job) (tuatara.runtime:job-process job))
        (push (cons (tuatara.runtime:job-process job) (compile-job-session job)) sessions)))
    (loop for (process . session) in sessions
          when (compile-session-there-p process session)
            do (tuatara.io:signal-session process session sb-posix:sigcont)
               (tuatara.io:signal-session process session sb-posix:sigkill))))

(defmethod tuatara.runtime:job-stop ((job compile-job))
  "Stop the compile: its work asked to stop, and every program of its session ended (END-COMPILE-SESSION)."
  (unless (car (tuatara.runtime:job-cancel job))
    (tuatara.runtime:stop-work (tuatara.runtime:job-cancel job))
    (let ((process (tuatara.runtime:job-process job)))
      (when process (end-compile-session job process)))))

(defun hand-output (job octets)
  "Hand OCTETS, read from JOB's output, to the editor, waiting first while
*COMPILE-WAITING-MOST* octets wait already (or until JOB is asked to stop);
the editor woken when it took in all there was before."
  (let ((wake nil))
    (sb-thread:with-mutex ((compile-job-lock job))
      (loop while (and (> (compile-job-waiting job) *compile-waiting-most*)
                       (not (car (tuatara.runtime:job-cancel job))))
            do (sb-thread:condition-wait (compile-job-room job) (compile-job-lock job) :timeout 0.5))
      ;; a large piece made a rope here, its lines counted on this thread, to go into the
      ;; buffer as it is
      (push (if (>= (length octets) *compile-rope-least*) (tuatara.text:octets->rope octets) octets)
            (compile-job-pieces job))
      (incf (compile-job-waiting job) (length octets))
      (when (compile-job-asleep job)
        (setf (compile-job-asleep job) nil wake t)))
    (when wake
      (let ((waker (tuatara.runtime:job-wake job)))
        (when waker (ignore-errors (funcall waker)))))))

(defstruct (output-scan (:constructor make-output-scan (run offset &aux (line-start offset))) (:copier nil))
  "Where the reading of a compile's output has got, for finding its lines of
note: RUN, its COMPILE-RUN; OFFSET, the octet of its buffer the next octet
read goes to; LINE-START, where the line being read began; LINE, the octets of
it read so far when it began in a piece read before, at most
*COMPILE-LINE-LOOKED-AT*; LENGTH, how long it is so far; KIND-BEFORE, the kind
of the last line saying what is wrong without a place; TEXT, the string a
line is matched in (LINE-TEXT); FOLDERS, the directories make went into, the
innermost first, as indexes into RUN's (0, where the command ran, when none);
THERE, whether each file name looked for is a file, by directory and name."
  run offset (line-start 0) (line (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (length 0) kind-before (text (make-string *compile-line-read*)) (folders '())
  (there (make-hash-table :test 'equal)))

(setf (documentation 'output-scan-p 'function) "True when OBJECT is an OUTPUT-SCAN.")

(defun line-text (scan octets start end)
  "OCTETS from START below END as text to match, as two values, the string
and how many of its characters are the text: SCAN's own string, written
over each time, when they are all ASCII, as most lines of output are; else
a string of their own (TUATARA.TEXT:OCTETS-STRING)."
  (let ((text (output-scan-text scan)))
    (declare (type (simple-array character (*)) text) (type tuatara.text:octets octets) (type fixnum start end))
    (if (and (<= (- end start) (length text))
             (loop for i of-type fixnum from start below end
                   always (< (aref octets i) 128)
                   do (setf (schar text (- i start)) (code-char (aref octets i)))))
        (values text (- end start))
        (let ((string (tuatara.text:octets-string octets :start start :end end)))
          (values string (length string))))))

(defun scan-file-there-p (scan file)
  "True when FILE, named in a compile's output, is a file in the directory
SCAN is in; looked up once for each name, as many as 4,096 kept."
  (let* ((folder (or (first (output-scan-folders scan)) 0))
         (key (cons folder file))
         (there (output-scan-there scan))
         (known (gethash key there :unknown)))
    (if (eq known :unknown)
        (let ((found (compile-file-there-p (output-scan-run scan) file folder)))
          (when (< (hash-table-count there) 4096) (setf (gethash key there) found))
          found)
        known)))

(defun note-line-octets (scan octets start end length)
  "Find what the line of a compile's output in OCTETS from START below END
names (OUTPUT-LINE-PLACE), the line LENGTH octets long in all, beginning where
SCAN's LINE-START says, and keep it when it is a line of note; a line of
make's going into a directory or out of it changes the directory file names
are taken from."
  (let ((end (if (and (> end start) (= 13 (aref octets (1- end)))) (1- end) end)))
    (when (and (> end start) (or (find 58 octets :start start :end end) (find 34 octets :start start :end end)))
      (multiple-value-bind (text count) (line-text scan octets start (min end (+ start *compile-line-read*)))
        (multiple-value-bind (motion directory) (make-directory-line text count)
          (case motion
            (:entering (push (run-folder (output-scan-run scan) directory) (output-scan-folders scan)))
            (:leaving (pop (output-scan-folders scan)))
            (t (multiple-value-bind (kind file)
                   (output-line-place text :kind-before (output-scan-kind-before scan) :end count
                                           :exists (lambda (file) (scan-file-there-p scan file)))
                 (when (and kind (null file)) (setf (output-scan-kind-before scan) kind))
                 (when (or kind file)
                   (note-output-line (output-scan-run scan) (output-scan-line-start scan) length kind file
                                     (or (first (output-scan-folders scan)) 0)))))))))))

(defun scan-output (scan octets)
  "Find the lines of note in OCTETS, the next piece of a compile's output, as SCAN has got."
  (declare (type tuatara.text:octets octets))
  (let ((line (output-scan-line scan)) (from 0) (count (length octets)))
    (declare (type fixnum from count))
    (loop for newline = (position 10 octets :start from)
          do (let* ((end (or newline count))
                    (taken (min (- end from) (max 0 (- *compile-line-looked-at* (fill-pointer line))))))
               (incf (output-scan-length scan) (- end from))
               (cond ((null newline)
                      ;; a line going on into the next piece: what of it is looked at kept
                      (loop for i from from below (+ from taken) do (vector-push-extend (aref octets i) line))
                      (return))
                     ((zerop (fill-pointer line))
                      (note-line-octets scan octets from (+ from taken) (output-scan-length scan)))
                     (t (loop for i from from below (+ from taken) do (vector-push-extend (aref octets i) line))
                        (note-line-octets scan (coerce line 'tuatara.text:octets) 0 (fill-pointer line) (output-scan-length scan))))
               (setf (fill-pointer line) 0
                     (output-scan-line-start scan) (+ (output-scan-offset scan) newline 1)
                     (output-scan-length scan) 0
                     from (1+ newline))))
    (incf (output-scan-offset scan) count)))

(defun finish-output-scan (scan)
  "Find what the last line of a compile's output names when it has no newline at its end."
  (let ((line (output-scan-line scan)))
    (when (plusp (fill-pointer line))
      (note-line-octets scan (coerce line 'tuatara.text:octets) 0 (fill-pointer line) (output-scan-length scan)))))

(defun read-compile-output (job fd process)
  "Read JOB's output from FD until its end, a piece at a time as it comes,
each piece handed to the editor (HAND-OUTPUT) once its lines of note are
found; a character cut by the end of a piece goes with the next. Past
JOB's most octets (*COMPILE-OUTPUT-MOST*) the output is read on and counted, not kept.
Once PROCESS, the command, has ended, what its programs still write is read
for *COMPILE-OPEN-GRACE* seconds more, then reading stops, the output said
held open, however much is still written; stopped, reading stops once
the command and its session have ended, whatever outside it holds the
output open."
  (let* ((scan (make-output-scan (compile-job-run job) (compile-run-base (compile-job-run job))))
         (most (compile-job-most job))
         (kept 0)
         (piece (make-array *compile-piece* :element-type '(unsigned-byte 8)))
         (filled 0)
         (ended-at nil))
    (flet ((give (count)
             ;; a small piece copied, a large one handed on whole and a new one made
             (if (and most (>= kept most))
                 (progn (incf (compile-job-left-out job) count)
                        (replace piece piece :start2 count :end2 filled))
                 (let ((given (if (< count 8192)
                                  (subseq piece 0 count)
                                  (let ((fresh (make-array *compile-piece* :element-type '(unsigned-byte 8))))
                                    (replace fresh piece :start2 count :end2 filled)
                                    (prog1 (sb-kernel:%shrink-vector piece count) (setf piece fresh))))))
                   (when (< count 8192) (replace piece piece :start2 count :end2 filled))
                   (incf kept count)
                   (scan-output scan given)
                   (hand-output job given)))
             (decf filled count)))
      (loop (when (consp (tuatara.io:wait-for-fds (list fd) 0.25))
              (let ((got (tuatara.io:read-fd-into fd piece :start filled :path "the compile's output")))
                (when (zerop got)
                  (return))
                (incf filled got)
                (let ((ready (- filled (tuatara.text:octets-pending piece 0 filled))))
                  (when (or (plusp ready) (= filled (length piece)))
                    (give (if (plusp ready) ready filled))))))
            ;; at every pass, written to or not: once the command has ended, the programs it
            ;; left are given a moment, and a compile stopped ends with its session
            (unless (sb-ext:process-alive-p process)
              (let ((now (get-internal-real-time)))
                (unless ended-at (setf ended-at now))
                (cond ((> (- now ended-at) (* *compile-open-grace* internal-time-units-per-second))
                       (unless (car (tuatara.runtime:job-cancel job))
                         (setf (compile-job-left-open job) t))
                       (return))
                      ((and (car (tuatara.runtime:job-cancel job))
                            (not (compile-session-there-p process (compile-job-session job))))
                       (return))))))
      (when (plusp filled) (give filled)))
    (finish-output-scan scan)))

(defun run-compile-job (job)
  "The work of JOB: run its command with the shell in its directory, in a
session of its own with no terminal (SESSION-COMMAND), its output and errors
read together as they come (READ-COMPILE-OUTPUT), and note how and when it
ended."
  (let ((run (compile-job-run job)))
    (tuatara.io:call-with-program
     (tuatara.io:session-command (list (tuatara.io:user-shell) "-c" (compile-run-command run)))
     (lambda (process)
       ;; the session's number, for stopping all it starts: setsid's child's
       (loop repeat 100
             until (or (setf (compile-job-session job) (tuatara.io:session-leader process))
                       (not (sb-ext:process-alive-p process)))
             do (sleep 0.01))
       (read-compile-output job (sb-sys:fd-stream-fd (sb-ext:process-output process)) process)
       (sb-ext:process-wait process)
       (setf (compile-job-exit job) (list (sb-ext:process-status process) (sb-ext:process-exit-code process))
             (compile-job-ended job) (get-internal-real-time)))
     :job job :input nil :output :stream :error :output :directory (compile-run-directory run)
     :environment (compile-job-environment job) :external-format :latin-1)))

;;; Taking the output in

(defun piece-size (piece)
  "The octets of PIECE, a piece of a compile's output: an octet vector or a rope."
  (if (tuatara.text:rope-p piece) (tuatara.text:rope-bytes piece) (length piece)))

(defun joined-octets (pieces)
  "PIECES, octet vectors or ropes, one after another in one octet vector."
  (let ((all (make-array (reduce #'+ pieces :key #'piece-size) :element-type '(unsigned-byte 8))) (at 0))
    (dolist (piece pieces all)
      (if (tuatara.text:rope-p piece)
          (tuatara.text:rope-copy-into piece 0 (tuatara.text:rope-bytes piece) all at)
          (replace all piece :start1 at))
      (incf at (piece-size piece)))))

(defun pieces-ropes (pieces)
  "PIECES, octet vectors and ropes in order, as ropes in the same order: the
ropes as they are, each run of small octet vectors between them joined into
one, so that the text is not cut into a leaf for each line written."
  (let ((ropes '()) (small '()))
    (flet ((join-small ()
             (when small
               (push (tuatara.text:octets->rope (joined-octets (nreverse small))) ropes)
               (setf small '()))))
      (dolist (piece pieces)
        (if (tuatara.text:rope-p piece)
            (progn (join-small) (push piece ropes))
            (push piece small)))
      (join-small))
    (nreverse ropes)))

(defun add-compile-output (buffer pieces)
  "Put PIECES, octet vectors or ropes, at the end of BUFFER, a compile's
output, read-only as before, with nothing to undo; point, and each window's
point, at the end staying at the end, following the output as it comes.
Much of it at once, more than a 128th of what BUFFER holds, goes in as the
pieces are, their memory shared, at the cost of making BUFFER's tree of text
again (TUATARA.BUFFER:BUFFER-SPLICE); less is copied in, where only the end
of the tree changes (TUATARA.BUFFER:BUFFER-INSERT)."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (count (reduce #'+ pieces :key #'piece-size))
         (windows (remove-if-not (lambda (window) (= size (tuatara.display:window-point window))) (buffer-windows buffer)))
         (at-end (= size (tuatara.buffer:buffer-point buffer))))
    (tuatara.buffer:with-read-only-lifted (buffer :after t)
      (if (> count (max *compile-rope-least* (floor size 128)))
          (tuatara.buffer:buffer-splice buffer size size
                                        (apply #'tuatara.text:splice-rope (tuatara.text:make-rope) 0 0 (pieces-ropes pieces)))
          (tuatara.buffer:buffer-insert buffer size (if (and (null (rest pieces)) (not (tuatara.text:rope-p (first pieces))))
                                                        (first pieces)
                                                        (joined-octets pieces)))))
    (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '())
    (tuatara.buffer:buffer-mark-saved buffer)
    (let ((end (tuatara.buffer:buffer-size buffer)))
      (when at-end (setf (tuatara.buffer:buffer-point buffer) end))
      (dolist (window windows) (setf (tuatara.display:window-point window) end)))))

(defun take-in-output (job)
  "Put what JOB's output has brought since last time into its buffer, at
most *COMPILE-TAKEN-AT-ONCE* octets of it; true when there was some, else JOB
is noted asleep, to wake the editor with the next."
  (let ((taken '()))
    (sb-thread:with-mutex ((compile-job-lock job))
      (let ((pieces (reverse (compile-job-pieces job))) (sum 0))
        (loop while (and pieces (or (null taken) (< (+ sum (piece-size (first pieces))) *compile-taken-at-once*)))
              do (incf sum (piece-size (first pieces)))
                 (push (pop pieces) taken))
        (setf (compile-job-pieces job) (reverse pieces))
        (decf (compile-job-waiting job) sum)
        (if taken
            (sb-thread:condition-broadcast (compile-job-room job))
            (setf (compile-job-asleep job) t))))
    (let ((buffer (tuatara.runtime:job-buffer job)))
      (when (and taken (live-buffer-p buffer) (eq (compile-job-run job) (tuatara.buffer:buffer-local buffer 'compile-run)))
        (add-compile-output buffer (nreverse taken))))
    (and taken t)))

(defun compile-under-way (buffer)
  "The compile whose output goes into BUFFER that has not finished: running,
or ended with its output still being put in; NIL when there is none."
  (or (tuatara.runtime:running-job buffer 'compile-job)
      (find buffer *compiles-ending* :key #'tuatara.runtime:job-buffer)))

(defun compile-output-tick (editor)
  "Tick function: put what the compiles' output has brought into their
buffers, a part at a time between keys, so that a flood of it never holds
typing up; a compile ended, once all its output is in, finished
\(FINISH-COMPILE). Called again soon while some has come."
  (declare (ignore editor))
  (let ((took nil))
    (dolist (job tuatara.runtime:*jobs*)
      (when (and (compile-job-p job) (take-in-output job))
        (setf took t)))
    (dolist (job (reverse *compiles-ending*))
      (if (take-in-output job)
          (setf took t)
          (progn (setf *compiles-ending* (remove job *compiles-ending*))
                 (finish-compile job))))
    (cond (*compiles-ending* 0.001)
          (took 0.03))))

(defun elapsed-words (seconds)
  "SECONDS as how long something took: 0.4 seconds, 3 minutes 12 seconds, 2 hours 5 minutes."
  (multiple-value-bind (minutes rest) (floor seconds 60)
    (cond ((< seconds 60) (format nil "~,1f seconds" seconds))
          ((< minutes 60) (format nil "~d minute~:p ~d second~:p" minutes (floor rest)))
          (t (format nil "~d hour~:p ~d minute~:p" (floor minutes 60) (mod minutes 60))))))

(defun compile-ending-words (job)
  "How JOB's command ended, how long it took and when, as words: Compile
finished, exit code 0, after 2.1 seconds at 10:30:05; what of its output was
left out, and a program it started holding the output open, said after; and
true as a second value when it ended well, exit code 0."
  (destructuring-bind (&optional status code) (compile-job-exit job)
    (let* ((stopped (car (tuatara.runtime:job-cancel job)))
           (well (and (eq status :exited) (eql code 0) (not stopped)))
           (seconds (/ (- (or (compile-job-ended job) (get-internal-real-time)) (tuatara.runtime:job-started job))
                       internal-time-units-per-second)))
      (values (format nil "~a, after ~a at ~a~@[; ~a~]~:[~;; a program it started held its output open and was left running~]"
                      (cond ((eq (tuatara.runtime:job-status job) :failed) (tuatara.runtime:job-ending-words job))
                            (stopped "Compile stopped")
                            (well "Compile finished, exit code 0")
                            ((eq status :exited) (format nil "Compile failed, exit code ~d" code))
                            ((eq status :signaled) (format nil "Compile ended by signal ~d" code))
                            (t "Compile stopped"))
                      (elapsed-words seconds) (format-time "%T" (get-universal-time))
                      (and (plusp (compile-job-left-out job))
                           (format nil "~a more output left out, past the ~a *compile-output-most* keeps"
                                   (octets-words (compile-job-left-out job)) (octets-words (compile-job-most job))))
                      (compile-job-left-open job))
              well))))

(defmethod tuatara.runtime:job-take-in ((job compile-job))
  "Have the rest of the compile's output put in its buffer, a part at each
tick, then the line saying how it ended (FINISH-COMPILE): at once when none
is left."
  (setf (compile-job-ending job) t)
  (if (take-in-output job)
      (progn (push job *compiles-ending*)
             (when *editor* (wake-editor *editor*)))
      (finish-compile job)))

(defun finish-compile (job)
  "Once all of JOB's output is in its buffer: a line saying how and when it
ended and how long it took, which is said too, in the error face when it did
not end well; then the compile asked for meanwhile, if any, starts."
  (let ((buffer (tuatara.runtime:job-buffer job))
        (run (compile-job-run job)))
    (multiple-value-bind (words well) (compile-ending-words job)
      (when (and (live-buffer-p buffer) (eq run (tuatara.buffer:buffer-local buffer 'compile-run)))
        (let* ((size (tuatara.buffer:buffer-size buffer))
               (open (and (> size (compile-run-base run))
                          (not (tuatara.text:rope-line-start-p (tuatara.buffer:buffer-text buffer) size)))))
          (add-compile-output buffer (list (tuatara.text:string-octets (format nil "~:[~;~%~]~%~a~%" open words))))
          (unless well
            (note-output-line run (+ size (if open 2 1)) (tuatara.text:string-octet-length words) :error nil))))
      (message "~a" words))
    (when (compile-job-then job)
      (funcall (compile-job-then job)))))

;;; The command and its buffer

(define-major-mode compile-output-mode special-mode
  "What a command run by \\[compile] prints, as it comes, read-only, with the
lines naming a file and line and those saying an error, a warning or a note
in faces of their own. \\<compile-output-mode>\\[list-visit] on a line naming a place opens the file
there, in another window; \\[next-place] and \\[previous-place] go from place to place from
anywhere; \\[revert-buffer] runs the command again, \\[compile-stop] stops it, \\[quit-window]
closes the window."
  :lighter "Compile" :interactive nil)

(defvar *compile-output-keymap* (mode-keymap 'compile-output-mode)
  "Keys of a compile's output (compile-output-mode's keymap).")

(defmethod mode-syntax-highlights ((mode compile-output-mode) buffer rows)
  "Colour the lines of a compile's output that say an error, a warning or a
note, as they come."
  (let ((run (tuatara.buffer:buffer-local buffer 'compile-run))
        (size (tuatara.buffer:buffer-size buffer)))
    (when run
      (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
            nconc (run-marks-between run from to size)))))

(defmethod list-entry-noun ((mode compile-output-mode))
  "A place, a line of a compile's output naming a file and line."
  "place")

(defmethod visit-entry ((mode compile-output-mode) list entry other)
  "Open the file the line ENTRY of the compile's output names, at its line, in
another window; the compile's places are the list of places in force then."
  (declare (ignore other))
  (let ((run (tuatara.buffer:buffer-local list 'compile-run)))
    (use-places run)
    (goto-compile-place run entry :other t)))

(defun compile-environment ()
  "What a compile's command sees of the environment: the editor's, with TERM
dumb, for programs that colour their output only for a terminal."
  (cons "TERM=dumb" (remove-if (lambda (entry) (uiop:string-prefix-p "TERM=" entry)) (sb-ext:posix-environ))))

(defun start-compile (command directory)
  "Run COMMAND with the shell in DIRECTORY, its output shown in *compile* as
it comes, the list of places in force then. A compile not finished there is
stopped first, when you say so, and this one starts once it has ended, so
that two never run at once. True when it started or is to start."
  (let* ((buffer (buffer-in-mode "*compile*" 'compile-output-mode))
         (before (compile-under-way buffer)))
    (cond ((null before) (start-compile-now buffer command directory) t)
          ((not (y-or-n-p* "A compile is running; stop it and run this one?"))
           (message "Not run: the compile running goes on")
           nil)
          (t (setf (compile-job-then before) (lambda () (start-compile-now buffer command directory)))
             (when (tuatara.runtime:job-running-p before)
               (tuatara.runtime:job-stop before))
             (message "Stopping the compile running; this one runs once it has ended")
             t))))

(defvar *setsid-missing-said* nil
  "True once it has been said that setsid is not installed, so that a compile
runs without a session of its own (said once in a session).")

(defun start-compile-now (buffer command directory)
  "Run COMMAND with the shell in DIRECTORY, its output shown in BUFFER as it
comes, the list of places in force then."
  (unless (live-buffer-p buffer)
    (message "Not run: ~a was killed before the compile running there had ended" (tuatara.buffer:buffer-name buffer))
    (return-from start-compile-now nil))
  (unless (or *setsid-missing-said* (tuatara.io:find-program "setsid"))
    (setf *setsid-missing-said* t)
    (message "setsid is not installed: the command runs in this terminal's session, so a program asking for a password could reach this terminal"))
  (progn
    (let* ((heading (format nil "Compile in ~a, ~a:~%~a~%~%" (abbreviated-path directory)
                            (format-time "%F %a %T" (get-universal-time)) command))
           (run (make-compile-run buffer command directory (tuatara.text:string-octet-length heading))))
      (set-made-text buffer heading :refill 'compile-again)
      (setf (tuatara.buffer:buffer-local buffer 'compile-run) run
            (tuatara.buffer:buffer-local buffer :entry-at) 'compile-entry-at
            (tuatara.buffer:buffer-local buffer :directory) directory
            (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-size buffer))
      (use-places run)
      (let ((window (display-buffer buffer)))
        (when window (setf (tuatara.display:window-point window) (tuatara.buffer:buffer-size buffer))))
      (tuatara.runtime:start-job-thread (make-compile-job buffer run (compile-environment) *compile-output-most*) 'run-compile-job
                                        :name "tuatara compile" :details command))))

(defun compile-in (command directory)
  "Run COMMAND in DIRECTORY (START-COMPILE) once the project there is trusted
to run its own programs, asking when it is not (TRUST-PROJECT); with no,
nothing runs."
  (if (trust-project directory)
      (start-compile command directory)
      (message "Not run: ~a is not trusted to run its build commands" (abbreviated-path directory))))

(defun compile-again (buffer)
  "Run the command whose output BUFFER shows again, in the same directory."
  (let ((run (tuatara.buffer:buffer-local buffer 'compile-run)))
    (if run
        (compile-in (compile-run-command run) (compile-run-directory run))
        (message "Nothing has run here"))))

(define-command compile ()
  "Run a command asked for, a build or the tests (make -k, cargo build, pytest),
with your shell from the root of the project the current buffer's file is in
\(see *PROJECT-ROOT-MARKERS*), or from its directory when it is in none, and
show what it prints in *compile* as it comes, the editor free meanwhile. The
command typed last is offered (*COMPILE-COMMAND* the first time), and
\\<minibuffer>\\[minibuffer-previous-history] goes back through those before. A project not trusted is asked about
first, since a build runs its own code; with no, nothing runs. In *compile*,
lines naming a file and line (GCC, Clang, rustc and cargo, Python's
tracebacks, grep -n) are places: \\<compile-output-mode>\\[list-visit] opens one, and \\[next-place] and \\[previous-place]
go from place to place from anywhere; error, warning and note lines are in
faces of their own; \\[revert-buffer] runs the command again and \\[compile-stop] stops it. At
the end it says how the command ended (its exit code, or the signal that
ended it) and how long it took."
  (let* ((here (buffer-directory (current-buffer)))
         (directory (or (project-root here) here))
         (command (read-string (format nil "Compile in ~a: " (abbreviated-path directory))
                               :initial (or (first *compile-history*) *compile-command*)
                               :history *compile-history*)))
    (if (zerop (length (string-trim '(#\Space #\Tab) command)))
        (message "Nothing to run")
        (progn (setf *compile-history* (cons command (remove command *compile-history* :test #'string=)))
               (compile-in command directory)))))

(define-command compile-stop ()
  "Stop the command running in this compile's output, and every program it
started: what it printed so far stays."
  (let ((job (tuatara.runtime:running-job (current-buffer) 'compile-job)))
    (if job
        (progn (tuatara.runtime:job-stop job)
               (message "Stopping the compile"))
        (message "No compile is running here"))))

(defun install-compile ()
  "Give a compile's output its keys, \\`RET' opening the place on the line and
\\`C-c C-k' stopping the command, take its output in as it comes, and kill
what it runs when the editor is left."
  (define-keys *compile-output-keymap* '(("RET" list-visit) ("C-c C-k" compile-stop)))
  (commands-for-mode 'compile-output-mode "a compile's output (\\[compile])" 'list-visit 'compile-stop)
  (pushnew 'compile-output-tick *tick-functions*)
  (pushnew 'kill-compiles-at-exit *exit-functions*))
