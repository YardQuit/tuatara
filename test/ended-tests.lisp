;;;; test/ended-tests.lisp --- A test that hangs fails after its time, and the tests after it run as usual.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defparameter *hanging-tests-package*
  (let ((package (or (find-package '#:tuatara.tests.hanging) (make-package '#:tuatara.tests.hanging :use '(#:cl)))))
    (setf (documentation package t) "The tests that hang on purpose, to see they are ended.")
    package)
  "The package of the tests that hang on purpose, kept from the suite's own.")

(define-test ended-tests
  :description "A test that does not end fails after its time limit and leaves nothing running: its jobs and programs are ended, and the tests after it run.")

(define-test (ended-tests a-hanging-test-fails-and-the-next-runs)
  (let ((*package* *hanging-tests-package*)
        (process nil))
    (eval `(parachute:define-test hangs-in-a-job
             :time-limit 1
             (let ((job (tuatara.defaults::%make-hash-job :what "a sum")))
               (tuatara.runtime:start-job-thread job (lambda (job)
                                                       (declare (ignore job))
                                                       (loop (tuatara.runtime:job-check-stop) (sleep 0.01))))
               (setf (symbol-value 'tuatara.tests::*hung-process*)
                     (sb-ext:run-program "/bin/sleep" '("300") :wait nil))
               (loop (sleep 0.01)))))
    (eval `(parachute:define-test runs-after-it
             (parachute:true t)))
    (let* ((parachute::*parent* nil)
           (parachute::*context* nil)
           (tuatara.runtime:*jobs* '())
           (output (make-string-output-stream))
           (report (let ((*standard-output* output))
                     (parachute:test (list (parachute:find-test 'hangs-in-a-job *hanging-tests-package*)
                                           (parachute:find-test 'runs-after-it *hanging-tests-package*))
                                     :report 'parachute:plain)))
           (statuses (lambda (name)
                       (loop for result across (parachute:results report)
                             when (and (typep (parachute:expression result) (quote parachute:test))
                                    (string= (string (parachute:name (parachute:expression result))) name))
                               collect (parachute:status result)))))
      (setf process *hung-process*)
      (true (member :failed (funcall statuses "HANGS-IN-A-JOB")))
      (false (member :failed (funcall statuses "RUNS-AFTER-IT")))
      (true (member :passed (funcall statuses "RUNS-AFTER-IT")))
      (true (search "was ended after 1 seconds" (get-output-stream-string output)))
      (false (some #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*))
      (false (sb-ext:process-alive-p process)))))
