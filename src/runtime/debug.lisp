;;;; src/runtime/debug.lisp --- Reports for debug mode: what the machinery says when asked.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; While debug mode is on, code anywhere says what it is doing with
;;;; DEBUG-REPORT: the report is queued, stamped with the time and the thread
;;;; that made it, and the editor shows it in *Debug*; it is written at once to
;;;; the debug log in the cache directory too, which a crash or a hang cannot
;;;; keep it from. While it is off a report costs one look at a variable, its
;;;; text not even made, so reports stay in the code for good.

(in-package #:tuatara.runtime)

(defvar *debug-on* nil
  "True while debug mode is on: what DEBUG-REPORT is given is kept, to be shown
in *Debug*.")

(defvar *debug-start* (get-internal-real-time)
  "When the reports' times count from, in internal time units: the start of the
editor.")

(defvar *debug-reports* (sb-concurrency:make-queue :name "debug reports")
  "The reports made and not yet shown in *Debug*, oldest first: each (TIME
THREAD TOPIC TEXT), TIME in internal time units, THREAD the name of the thread
that made it.")

(defvar *debug-log-path* nil
  "The file the reports of debug mode are written to as they are made, the
debug log (debug.log in the cache directory); NIL for none.")

(defvar *debug-log-stream* nil
  "The debug log, open while debug mode is on; NIL otherwise.")

(defvar *debug-log-lock* (sb-thread:make-mutex :name "debug log")
  "Held while a report is written to the debug log.")

(defvar *debug-log-begun* nil
  "True once this session began its debug log, the ones before kept aside.")

(defmacro debug-report (topic control &rest arguments)
  "While debug mode is on, keep a report for *Debug*: TOPIC, a keyword saying
what it is about (:parse, :rules, :job, :file, :redisplay, :gc, :error), and
the text FORMAT makes of CONTROL and ARGUMENTS. While it is off nothing is
evaluated but the look at *DEBUG-ON*."
  `(when *debug-on*
     (queue-debug-report ,topic (format nil ,control ,@arguments))))

(defun debug-report-line (report)
  "REPORT, as DEBUG-REPORT queues it (TIME THREAD TOPIC TEXT), as a line of
*Debug* and the debug log: the seconds since the start, the thread (without a
leading \"tuatara \", cut to the 12 columns it has), the topic and the text."
  (destructuring-bind (time thread topic text) report
    (format nil "~10,3f  ~12a ~10a ~a"
            (/ (- time *debug-start*) internal-time-units-per-second)
            (let* ((name (or thread ""))
                   (name (if (and (> (length name) 8) (string= "tuatara " name :end2 8)) (subseq name 8) name)))
              (subseq name 0 (min 12 (length name))))
            (string-downcase (symbol-name topic)) text)))

(defun queue-debug-report (topic text)
  "Queue TEXT about TOPIC to be shown in *Debug*, stamped with the time and the
thread (DEBUG-REPORT), write it to the debug log at once, and wake the editor
to show it."
  (let ((report (list (get-internal-real-time) (sb-thread:thread-name sb-thread:*current-thread*) topic text)))
    (sb-concurrency:enqueue report *debug-reports*)
    (let ((stream *debug-log-stream*))
      (when stream
        ;; made inside error handlers too: a write that fails in any way is let
        ;; go, as raising here would hide the error being reported
        (ignore-errors
         (sb-thread:with-mutex (*debug-log-lock*)
           (write-line (debug-report-line report) stream)
           (finish-output stream))))))
  ;; reports are made inside error handlers: a wake that fails is let go, as
  ;; saying it would make another report
  (ignore-errors (host-wake *host*))
  nil)

(defun close-fd-quietly (fd &optional control &rest arguments)
  "Close the file descriptor (FD) FD, letting an error in closing it go, for use
where another error is already being handled or nothing is left to tell. With
CONTROL, a format control, and ARGUMENTS saying which file FD is, the error is
reported in the debug log while debug mode is on (REPORTING-ERRORS). An FD of
NIL is left alone. Returns NIL."
  (declare (dynamic-extent arguments))
  (when fd
    (handler-case
        (handler-bind ((error (lambda (condition)
                                (when (and control *debug-on*)
                                  (report-error "closing ~?" (list control (copy-list arguments)) condition)))))
          (sb-posix:close fd))
      (error () nil)))
  nil)

(defmacro with-fd-closed-on-error ((fd &optional control &rest arguments) &body body)
  "Run BODY and return what it returns; when an error leaves BODY, the file
descriptor FD, a form evaluated after BODY has begun, is closed first
\(CLOSE-FD-QUIETLY with CONTROL and ARGUMENTS), and the error goes on to
whatever handles it. For a descriptor that BODY hands on when it succeeds."
  `(handler-bind ((error (lambda (condition)
                           (declare (ignore condition))
                           (close-fd-quietly ,fd ,@(when control `(,control ,@arguments))))))
     ,@body))

(defun open-debug-log ()
  "Open the debug log for writing at its end (OPEN-ROTATED-LOG), for you
alone; the first time in a session, when no other session is writing to it,
beginning a new one, the logs of the sessions before kept as debug.log.1, the
one just before, to debug.log.3. NIL when there is no debug log to open."
  (when *debug-log-path*
    (handler-case
        (let* ((path (pathname *debug-log-path*))
               (native (sb-ext:native-namestring path)))
          (ensure-directories-exist path)
          (let ((fd (open-rotated-log native :rotate (not *debug-log-begun*))))
            (setf *debug-log-begun* t)
            (with-fd-closed-on-error (fd)
              (sb-sys:make-fd-stream fd :output t :external-format :utf-8 :buffering :full
                                        :auto-close t :file native))))
      ((or file-error sb-posix:syscall-error) () nil))))

(defvar *secret-at-hand* nil
  "True while a secret is in the calls under way (a passphrase, a text to be
encrypted or just decrypted): the calls written to the debug log then show
their strings and octets by their length alone (BACKTRACE-LINES).")

(defgeneric host-secret-buffer-p (host buffer)
  (:documentation "True when BUFFER's text is kept secret (decrypted), so that work on it counts as
a secret at hand (SECRET-BUFFER-P). Without the files' code, which keeps that
of a buffer, none is.")
  (:method ((host host) buffer)
    (declare (ignore buffer))
    nil))

(defun secret-buffer-p (buffer)
  "True when BUFFER, a buffer or NIL, holds text kept secret (decrypted), so
that work on it is to keep it out of the debug log as *SECRET-AT-HAND* does."
  (and buffer (host-secret-buffer-p *host* buffer) t))

(define-condition refusal (simple-error) ()
  (:documentation "The editor saying no to what was asked, in words of its own (REFUSE): told in
full even while a secret is at hand, so its words never hold text of a buffer."))

(defun refuse (control &rest arguments)
  "Say no to what was asked, in the words FORMAT makes of CONTROL and
ARGUMENTS, as an error (a REFUSAL): the command stops and the words are told
as they are, a secret at hand or not. ARGUMENTS may name a buffer, a file or
a key, never hold a buffer's text."
  (error 'refusal :format-control control :format-arguments arguments))

(declaim (inline control-code-p))
(defun control-code-p (code)
  "True when CODE is a control character's: 0 to 31, 127 to 159."
  (or (< code 32) (<= 127 code 159)))

(defun one-line (text)
  "TEXT on one line: each run of line ends, tabs and other control
characters in it one space, and the spaces at its ends left out, so that it
can be shown or written where a line is expected (a name, a value, a reason
given by a program). NIL for NIL."
  (and text
       (let ((out (make-string-output-stream)) (blank nil))
         (loop for c across text
               do (if (control-code-p (char-code c))
                      (setf blank t)
                      (progn (when blank (write-char #\Space out) (setf blank nil))
                             (write-char c out))))
         (string-trim " " (get-output-stream-string out)))))

(defun failure-words (condition &key said)
  "CONDITION, an error caught, in words, for the debug log or, with SAID true,
for a message saying what failed. It is what the condition itself says (\"No
such file\"). While a secret is at hand (*SECRET-AT-HAND* true, as the editor
makes it around work on a buffer kept secret), it is only its kind, \"a
file-error\", since what it says could hold the secret. With SAID, a REFUSAL
keeps its words even then: they are the editor's own. Say a failure through
it rather than writing the condition into a message yourself, and nothing
secret is ever shown."
  (if (and *secret-at-hand* (not (and said (typep condition 'refusal))))
      (format nil "a ~(~a~)" (type-of condition))
      (condition-words condition)))

(defun secret-free (value &optional (depth 0))
  "VALUE with each string or vector of octets in it written as its length
alone, a character as ?, and every other object but a number or a symbol as
its kind, as what the debug log shows while a secret is at hand."
  (typecase value
    (string (make-symbol (format nil "#<~d characters>" (length value))))
    ((vector (unsigned-byte 8)) (make-symbol (format nil "#<~d octets>" (length value))))
    (cons (if (> depth 3) (make-symbol "#<...>") (mapcar (lambda (v) (secret-free v (1+ depth))) value)))
    ((or number symbol) value)
    (character #\?)
    (t (make-symbol (format nil "#<a ~(~a~)>" (type-of value))))))

(defun backtrace-lines (&key (count 60) stop (package *package*))
  "The calls under way now, innermost first, as a list of strings, each printed
short, names as PACKAGE sees them, at most COUNT; those of the signalling
itself left out, and those from the first whose call is to the function named
STOP, a symbol, or to a function made inside it, on. While a secret is at
hand (*SECRET-AT-HAND*), each call shows its function alone, never what it
was given."
  (let ((lines '()))
    (ignore-errors
     (let ((*print-length* 8) (*print-level* 3) (*print-circle* t) (*print-pretty* nil) (*print-readably* nil)
           (*package* package))
       (sb-debug:map-backtrace
        (lambda (frame)
          (push (handler-case
                    (let ((text (if *secret-at-hand*
                                    (prin1-to-string (list (sb-debug::frame-call frame)))
                                    (with-output-to-string (s)
                                      (sb-debug::print-frame-call frame s :argument-limit 8)))))
                      ;; a call's line of the log: its first 300 characters
                      (substitute #\Space #\Newline (subseq text 0 (min (length text) 300))))
                  (error () "#<a call that cannot be printed>"))
                lines))
        :count (+ count 12))))
    (let* ((lines (nreverse lines))
           (signalled (position-if (lambda (line)
                                     (or (eql 0 (search "(SB-KERNEL::%SIGNAL " line)) (eql 0 (search "(SIGNAL " line))
                                         (eql 0 (search "(ERROR " line))))
                                   lines :end (min 12 (length lines))))
           (lines (if signalled (nthcdr (1+ signalled) lines) lines))
           (call (and stop (cl-ppcre:create-scanner
                            (let ((name (format nil
                                                "(?:[^\\s():]+::?)?~a"
                                                (cl-ppcre:quote-meta-chars (symbol-name stop)))))
                              ;; (STOP ...), or ((LAMBDA () :IN STOP)) and the like
                              (format nil "^\\((?:~a[\\s)]|\\((?:LAMBDA|FLET|LABELS) [^\\n]*:IN ~a\\))" name name)))))
           (end (or (and call (position-if (lambda (line) (cl-ppcre:scan call line)) lines)) (length lines))))
      (subseq lines 0 (min count end)))))

(defvar *editor-busy-since* nil
  "When the editor's thread began the work it is doing, an internal real time;
NIL while it waits for what comes next.")

(defparameter *hang-report-seconds* 10
  "The seconds the editor's thread may be busy with one piece of work before
the calls under way in it are written to the error log (STDERR.LOG), once.")

(defvar *hang-watch-wakes* '()
  "The semaphores of the hang watches running (START-HANG-WATCH), each
signalled as the editor's thread starts work, to wake its watch, which waits
on it while the editor is at rest. Changed only by the editor's thread, as a
watch starts and ends.")

(defun note-editor-busy (busy)
  "Note that the editor's thread starts work (BUSY true) or waits again."
  (setf *editor-busy-since* (and busy (get-internal-real-time)))
  (when busy
    (dolist (wake *hang-watch-wakes*)
      (when (zerop (sb-thread:semaphore-count wake))
        (sb-thread:signal-semaphore wake)))))

(defun start-hang-watch (thread &key (while (constantly t)))
  "Watch THREAD, the editor's, from a thread of its own, as long as it lives and
WHILE, a function of no arguments, is true. Once it has been busy with one
piece of work for *HANG-REPORT-SECONDS* (*EDITOR-BUSY-SINCE*), have it write
the calls under way in it to the error output, so that a hang leaves a trace
however it came about. At rest it sleeps until the editor starts work
\(NOTE-EDITOR-BUSY), looking once a minute whether THREAD still lives. While
the editor works, it looks once a second. Returns a function of no arguments
that ends the watch at once, as a session ending does."
  (let* ((wake (sb-thread:make-semaphore :name "tuatara editor busy"))
         (ended nil)
         (watch
           (make-guarded-thread
            (lambda ()
              (let ((reported nil))
                (loop (unless *editor-busy-since*
                        (sb-thread:wait-on-semaphore wake :timeout 60))
                      (unless ended
                        (sb-thread:wait-on-semaphore wake :timeout 1))
                      (when (or ended (not (sb-thread:thread-alive-p thread)) (not (funcall while)))
                        (return))
                      (let ((since *editor-busy-since*))
                        (when (and since (not (eql since reported))
                                   (> (seconds-since since) *hang-report-seconds*))
                          (setf reported since)
                          (handler-case
                              (sb-thread:interrupt-thread
                               thread
                               (lambda ()
                                 (let ((lines (backtrace-lines)))
                                   (format *error-output*
                                           "~&Tuatara: the editor has been busy for ~d s; the calls under way:~%~{  ~a~%~}"
                                           (round (seconds-since since)) lines)
                                   (finish-output *error-output*)
                                   (queue-debug-report :error (format nil "busy for ~d s:~{~%    ~a~}"
                                                                      (round (seconds-since since)) lines)))))
                            (sb-thread:interrupt-thread-error () nil)))))))
            :name "tuatara hang watch")))
    (push wake *hang-watch-wakes*)
    (lambda ()
      (setf ended t
            *hang-watch-wakes* (remove wake *hang-watch-wakes*))
      (sb-thread:signal-semaphore wake)
      (sb-thread:join-thread watch :default nil :timeout 5))))

(defparameter *error-reports-per-second* 4
  "The most reports of one kind of failure made at one place (REPORTING-ERRORS)
in a second; those beyond are counted, and the count said with the next
report or once the second has passed.")

(defvar *error-report-places* (make-hash-table :test 'equal)
  "For each place and kind of failure reported, by (CONTROL . KIND), CONTROL
the words saying what was being done there: (SECOND COUNT LEFT-OUT SEEN DOING),
the second of its latest reports, how many were made in it, how many were left
out since one was kept, whether its calls under way were given, and what was
being done the last time, in words.")

(defvar *error-report-lock* (sb-thread:make-mutex :name "tuatara error reports")
  "Held while *ERROR-REPORT-PLACES* is looked at or changed.")

(defvar *left-out-timer* nil
  "The timer that says the counts of failures left out once their second has
passed (SAY-LEFT-OUT), made when first needed.")

(defun left-out-lines (now)
  "The lines saying how many failures were left out at each place, for those
whose second is not NOW, their counts then cleared; called with
*ERROR-REPORT-LOCK* held."
  (let ((lines '()))
    (maphash (lambda (key place)
               (when (and (plusp (third place)) (/= (first place) now))
                 (push (format nil "~a failed ~d more times (a ~(~a~)), left out" (fifth place) (third place) (cdr key))
                       lines)
                 (setf (third place) 0)))
             *error-report-places*)
    lines))

(defun say-left-out ()
  "Report how many failures were left out at each place whose second has
passed (LEFT-OUT-LINES)."
  (ignore-errors
   (let ((lines (sb-thread:with-mutex (*error-report-lock*)
                  (left-out-lines (floor (get-internal-real-time) internal-time-units-per-second)))))
     (when lines
       (queue-debug-report :error (format nil "~{~a~^~%    ~}" lines))))))

(defun report-error (control arguments condition)
  "Put CONDITION, an error signalled while doing what CONTROL and ARGUMENTS
say, in the debug log. The calls under way are written the first time that
kind of error comes from that place, and one line after, at most
*ERROR-REPORTS-PER-SECOND* times a second for each. How many were left out is
said with the next report, or once the second has passed. While a secret is
at hand (*SECRET-AT-HAND*) only the kind of error and the lengths of the
strings in ARGUMENTS are written, never the calls, and the place's calls are
still to be given. \\`C-g' (QUIT) is no error to report, and a failure to
report never takes the place of the error itself."
  (unless (typep condition 'quit)
    (ignore-errors
     (let* ((now (floor (get-internal-real-time) internal-time-units-per-second))
            (secret *secret-at-hand*)
            (key (cons control (type-of condition)))
            (doing (handler-case (apply #'format nil control (if secret (mapcar #'secret-free arguments) arguments))
                     (error () control)))
            (kept nil) (calls nil) (own-left-out 0) (others '()))
       (sb-thread:with-mutex (*error-report-lock*)
         (let ((place (or (gethash key *error-report-places*)
                          (setf (gethash key *error-report-places*) (list now 0 0 nil doing)))))
           (setf (fifth place) doing)
           (if (/= (first place) now)
               (setf (first place) now (second place) 1)
               (incf (second place)))
           (cond ((> (second place) *error-reports-per-second*)
                  (incf (third place))
                  (unless *left-out-timer*
                    (setf *left-out-timer* (sb-ext:make-timer #'say-left-out :name "tuatara left out" :thread t)))
                  (unless (sb-ext:timer-scheduled-p *left-out-timer*)
                    (sb-ext:schedule-timer *left-out-timer* 1.1)))
                 (t
                  (setf kept t
                        calls (and (not secret) (not (fourth place)))
                        own-left-out (shiftf (third place) 0)
                        others (left-out-lines now))
                  (unless secret (setf (fourth place) t))))))
       (when kept
         (queue-debug-report
          :error (format nil "~a failed: ~a~@[ (~d more like it left out)~]~{~%    ~a~}~{~%    ~a~}"
                         doing (failure-words condition)
                         (and (plusp own-left-out) own-left-out)
                         others
                         (and calls (backtrace-lines)))))))))

(defmacro reporting-errors ((control &rest arguments) &body body)
  "Run BODY; when an error is signalled in it while debug mode is on, report it
in the debug log, CONTROL and ARGUMENTS saying what was being done
\(REPORT-ERROR), and let it go on to whatever handles it. \\`C-g' (QUIT) is
no error to report."
  `(handler-bind ((error (lambda (condition)
                           (when *debug-on*
                             (report-error ,control (list ,@arguments) condition)))))
     ,@body))

(defmacro ignoring-errors-reported ((control &rest arguments) &body body)
  "Run BODY as IGNORE-ERRORS does, NIL and the condition when an error is
signalled in it; while debug mode is on, the error is reported first, with the
calls under way and CONTROL and ARGUMENTS saying what was being done
\(REPORTING-ERRORS), so that work in the background that gives up quietly
still leaves a trace."
  `(handler-case (reporting-errors (,control ,@arguments) ,@body)
     (error (condition) (values nil condition))))

(defun set-debug (on)
  "Switch debug mode on when ON is true, else off: on, the debug log is opened
(OPEN-DEBUG-LOG), off, closed. Returns ON."
  (sb-thread:with-mutex (*debug-log-lock*)
    (cond ((and on (null *debug-log-stream*))
           (setf *debug-log-stream* (open-debug-log)))
          ((and (not on) *debug-log-stream*)
           (handler-case (close *debug-log-stream*)
             (stream-error () nil))
           (setf *debug-log-stream* nil))))
  (setf *debug-on* (and on t)))
