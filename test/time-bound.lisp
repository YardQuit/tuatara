;;;; test/time-bound.lisp --- The time each test may take; a test over it is ended and fails.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defvar *test-time-limit* 300
  "The seconds each test may run, the tests it holds not counted. A test that
needs longer says so with its own :TIME-LIMIT, (define-test name :time-limit 900).")

(defvar *hung-process* nil
  "The program the test that hangs on purpose started.")

(defun end-test-leftovers ()
  "Put the session's state back after a test was ended in the middle: the
background jobs stopped and forgotten, the programs the test started ended."
  (dolist (job (copy-list tuatara.runtime:*jobs*))
    (ignore-errors (tuatara.runtime:stop-and-settle job)))
  (setf tuatara.runtime:*jobs* '())
  (dolist (process (copy-list sb-impl::*active-processes*))
    (ignore-errors
     (when (sb-ext:process-alive-p process)
       (sb-ext:process-kill process 9)
       (sb-ext:process-wait process)))))

(defmethod parachute:eval-in-context ((report parachute:report) (test parachute:test))
  "Run TEST for at most its time limit (*TEST-TIME-LIMIT* when it has none): past it the test is ended
where it is, the state it left is put back (END-TEST-LEFTOVERS), and a failed check says so."
  (let* ((limit (or (parachute:time-limit test) *test-time-limit*))
         (tag (list 'timed-out))
         (active t)
         (timer (sb-ext:make-timer (lambda () (when active (throw tag :timed-out)))
                                   :thread sb-thread:*current-thread*))
         (ended nil))
    (setf ended (catch tag
                  (sb-ext:schedule-timer timer limit)
                  (unwind-protect (progn (call-next-method) :finished)
                    (setf active nil)
                    (sb-ext:unschedule-timer timer))))
    (when (eq ended :timed-out)
      (end-test-leftovers)
      (parachute:true nil "~a was ended after ~d seconds, the time it may take" test limit))))
