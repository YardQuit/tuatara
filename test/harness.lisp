;;;; test/harness.lisp --- Benchmark harness.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.bench)

(defvar *benchmarks* '()
  "Registered benchmarks, each a plist (:name NAME :thunk THUNK :budget-ms MS :most-bytes BYTES :reps N), most
recent first.")

(declaim (inline bytes-consed))
(defun bytes-consed ()
  "Return bytes consed so far, accurate to the byte.
SB-EXT:GET-BYTES-CONSED only counts allocation regions that have been closed, so
small allocations still sitting in the thread's open region are invisible to it
(even SBCL's own TIME under-reports a 100 KB array as 29 KB). Closing the region
first makes the counter exact. Uses an internal SBCL symbol, guarded so a future
SBCL without it degrades to the lagging count instead of failing."
  (when (fboundp 'sb-vm::close-thread-alloc-region)
    (funcall 'sb-vm::close-thread-alloc-region))
  (sb-ext:get-bytes-consed))

(declaim (inline monotonic-seconds))
(defun monotonic-seconds ()
  "Seconds on the monotonic clock as a double with nanosecond resolution.
GET-INTERNAL-REAL-TIME is a coarse clock on Linux (whole milliseconds), useless
for budgets below a millisecond."
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ sec (/ nsec 1d9))))

(defmacro measuring (&body body)
  "Evaluate BODY once and return three values: its primary value, the wall time
in seconds as a double (nanosecond clock), and the bytes consed while it ran
(exact, see BYTES-CONSED)."
  (let ((t0 (gensym "T0")) (b0 (gensym "B0")) (result (gensym "RESULT")))
    `(let ((,t0 (monotonic-seconds))
           (,b0 (bytes-consed)))
       (let ((,result (progn ,@body)))
         (values ,result
                 (- (monotonic-seconds) ,t0)
                 (- (bytes-consed) ,b0))))))

(defmacro define-benchmark (name (&key (budget-ms nil) (most-bytes nil) (reps 5)) &body body)
  "Register a benchmark called NAME (a symbol) that runs BODY.
BUDGET-MS is the wall-time budget in milliseconds, or
NIL for an unbudgeted measurement. MOST-BYTES is the most bytes its best run may
cons, or NIL for no limit: work whose time is mostly fresh memory touched and
collected is held to what it makes, which is the same from run to run, where
its time is not. REPS is how many times BODY runs; the best
time is reported. When REPS is more than one, BODY first runs once more,
untimed, so that the first touch of fresh memory is not in any timed run. Redefining a benchmark replaces the earlier registration."
  `(progn
     (setf *benchmarks* (remove ',name *benchmarks* :key (lambda (b) (getf b :name))))
     (push (list :name ',name :thunk (lambda () ,@body) :budget-ms ,budget-ms :most-bytes ,most-bytes :reps ,reps)
           *benchmarks*)
     ',name))

(defvar *benchmark-passes* 4
  "The most passes of its timed runs a benchmark over its budget is given (RUN-ONE).")

(defvar *fastest-step* nil
  "The fewest seconds an ARITHMETIC-STEP has taken in this process.")

(defvar *step-result* 0
  "What the last ARITHMETIC-STEP worked out, kept so that its work is done.")

(defun arithmetic-step ()
  "Seconds a fixed piece of plain arithmetic takes, a few hundred microseconds on
a processor at its full clock."
  (let ((start (monotonic-seconds))
        (x 1))
    (declare (type (unsigned-byte 62) x))
    (dotimes (i 400000)
      (setf x (logand (+ (* x 31) i) #x3fffffffffffffff)))
    (setf *step-result* x)
    (- (monotonic-seconds) start)))

(defun warm-processor (&optional (least 0.02) (most 2))
  "Keep the calling thread busy for LEAST seconds and then until its processor
runs at its full clock, an ARITHMETIC-STEP taking no more than a tenth longer than
the fastest one seen (*FASTEST-STEP*); MOST seconds at the most. A processor
saving power can run a busy thread at a third of its clock for seconds."
  (let* ((start (monotonic-seconds))
         (from (+ start least))
         (until (+ start most)))
    (loop for took = (arithmetic-step)
          do (setf *fastest-step* (min took (or *fastest-step* took)))
          until (or (and (>= (monotonic-seconds) from) (<= took (* 1.1d0 *fastest-step*)))
                    (> (monotonic-seconds) until)))))

(defun run-one (bench)
  "Run BENCH (a registration plist) and return a plist with :name :best-ms :bytes :budget-ms
:within-budget :over-bytes :again. :OVER-BYTES is true when the best run consed more than
the benchmark's :MOST-BYTES; it is never within budget then.
A collection runs first so that garbage from earlier work does not land in the measurement.
Each pass begins once the processor runs at its full clock (WARM-PROCESSOR). A benchmark over
its budget runs its timed runs again, after another collection, up to *BENCHMARK-PASSES*
passes in all, and the best of all of them counts: neither a processor slowed to save power
nor a few moments of load from other work on the machine is taken for a slowdown, while
code that is slower every time stays over. :AGAIN is true when it ran more than once."
  (let ((best most-positive-double-float) (bytes 0) (budget (getf bench :budget-ms))
        (most-bytes (getf bench :most-bytes)) (again nil))
    (flet ((timed-runs ()
             (sb-ext:gc)
             (warm-processor)
             (when (> (getf bench :reps) 1) (funcall (getf bench :thunk)))
             (dotimes (i (getf bench :reps))
               (multiple-value-bind (result seconds consed) (measuring (funcall (getf bench :thunk)))
                 (declare (ignore result))
                 (when (< seconds best) (setf best seconds bytes consed)))))
           (within () (or (null budget) (<= (* best 1000d0) budget))))
      (timed-runs)
      (loop repeat (1- *benchmark-passes*)
            until (within)
            do (setf again t)
               (timed-runs))
      (let ((over-bytes (and most-bytes (> bytes most-bytes))))
        (list :name (getf bench :name) :best-ms (* best 1000d0) :bytes bytes :budget-ms budget
              :within-budget (and (within) (not over-bytes)) :over-bytes over-bytes :again again)))))

(defun run-benchmarks (&key (stream *standard-output*))
  "Run every registered benchmark, print one table row each, and return T when
all budgeted benchmarks are within budget, NIL otherwise. A row ends in
\"(run again)\" when its benchmark was over budget the first time and within it
on a later pass (RUN-ONE)."
  (let ((all-ok t))
    (format stream "~&~48a ~10@a ~12@a ~10@a~%" "benchmark" "best ms" "bytes" "budget")
    (dolist (bench (reverse *benchmarks*))
      (let ((r (run-one bench)))
        (unless (getf r :within-budget) (setf all-ok nil))
        (format stream "~48a ~10,3f ~12:d ~10@a~a~%"
                (string-downcase (getf r :name)) (getf r :best-ms) (getf r :bytes)
                (if (getf r :budget-ms) (format nil "~,1f" (getf r :budget-ms)) "-")
                (cond ((getf r :over-bytes) "  OVER ITS BYTES")
                      ((not (getf r :within-budget)) "  OVER BUDGET")
                      ((getf r :again) "  (run again)")
                      (t "")))))
    all-ok))
