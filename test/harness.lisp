;;;; test/harness.lisp --- Benchmark harness.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.bench)

(defvar *benchmarks* '()
  "Registered benchmarks, each a plist (:name NAME :thunk THUNK :budget-ms MS :reps N), most recent first.")

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

(defmacro define-benchmark (name (&key (budget-ms nil) (reps 5)) &body body)
  "Register a benchmark called NAME (a symbol) that runs BODY.
BUDGET-MS is the wall-time budget in milliseconds, or
NIL for an unbudgeted measurement. REPS is how many times BODY runs; the best
time is reported. When REPS is more than one, BODY first runs once more,
untimed, so that the first touch of fresh memory is not in any timed run. Redefining a benchmark replaces the earlier registration."
  `(progn
     (setf *benchmarks* (remove ',name *benchmarks* :key (lambda (b) (getf b :name))))
     (push (list :name ',name :thunk (lambda () ,@body) :budget-ms ,budget-ms :reps ,reps)
           *benchmarks*)
     ',name))

(defun run-one (bench)
  "Run BENCH (a registration plist) and return a plist with :name :best-ms :bytes :budget-ms :within-budget.
A collection runs first so that garbage from earlier work does not land in the measurement."
  (let ((best most-positive-double-float) (bytes 0))
    (sb-ext:gc)
    (when (> (getf bench :reps) 1) (funcall (getf bench :thunk)))
    (dotimes (i (getf bench :reps))
      (multiple-value-bind (result seconds consed) (measuring (funcall (getf bench :thunk)))
        (declare (ignore result))
        (when (< seconds best) (setf best seconds bytes consed))))
    (let ((best-ms (* best 1000d0)) (budget (getf bench :budget-ms)))
      (list :name (getf bench :name) :best-ms best-ms :bytes bytes :budget-ms budget
            :within-budget (or (null budget) (<= best-ms budget))))))

(defun run-benchmarks (&key (stream *standard-output*))
  "Run every registered benchmark, print one table row each, and return T when
all budgeted benchmarks are within budget, NIL otherwise."
  (let ((all-ok t))
    (format stream "~&~48a ~10@a ~12@a ~10@a~%" "benchmark" "best ms" "bytes" "budget")
    (dolist (bench (reverse *benchmarks*))
      (let ((r (run-one bench)))
        (unless (getf r :within-budget) (setf all-ok nil))
        (format stream "~48a ~10,3f ~12:d ~10@a~a~%"
                (string-downcase (getf r :name)) (getf r :best-ms) (getf r :bytes)
                (if (getf r :budget-ms) (format nil "~,1f" (getf r :budget-ms)) "-")
                (if (getf r :within-budget) "" "  OVER BUDGET"))))
    all-ok))
