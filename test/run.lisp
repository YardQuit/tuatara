;;;; test/run.lisp --- Command-line test runner: sbcl --non-interactive --load test/run.lisp
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Exits 0 when every test passes, 1 otherwise. The tests keep away from the
;;;; developer's own files however they are run (test/fences.lisp).

;; The nursery size the saved executable uses: fewer,
;; cheaper collections on a machine with tens of GB of RAM.
(setf (sb-ext:bytes-consed-between-gcs) (* 256 1024 1024))
(load (merge-pathnames "../tools/quicklisp.lisp" *load-truename*))
;; Compile the project afresh every run: ASDF does not track structure-layout
;; dependencies between files, and a stale fasl would fail to load. It is the
;; first time the project is loaded in this Lisp, so that a variable used in a
;; file before the one defining it is a warning here, as it is nowhere later.
;; Full compiler warnings (undefined variables, wrong argument counts) fail the
;; run even when ASDF would carry on; style warnings do not.
(load (merge-pathnames "compile-gate.lisp" *load-truename*))
(defvar *compiler-warnings*
  (tuatara-compile-gate:compile-warnings "tuatara/tests"
                                         :load-outside (lambda (names) (ql:quickload names :silent t))))
(when *compiler-warnings*
  (format t "~&~%Compiler warnings (~a):~%~{  ~a~%~}" (length *compiler-warnings*) *compiler-warnings*)
  (sb-ext:exit :code 2))
(defclass lean-report (parachute:plain) ()
  (:documentation "The plain report, keeping of each check that passed only what
the summary counts: its value, what it was compared with, its words and the
closure that ran it are let go once it is printed, so that a run of some
hundred thousand checks holds no more than the tests themselves do."))

(defmethod parachute:eval-in-context :around ((report lean-report) (result parachute:result))
  (multiple-value-prog1 (call-next-method)
    (when (eq (parachute:status result) :passed)
      (setf (parachute:description result) nil)
      (when (typep result 'parachute:value-result)
        (setf (parachute:value result) nil
              (parachute:body result) nil))
      (when (typep result 'parachute:comparison-result)
        (setf (parachute:expected result) nil))
      (when (typep result 'parachute:group-result)
        (setf (parachute:body result) nil)))))

(let ((report (parachute:test :tuatara.tests :report 'lean-report)))
  (sb-ext:exit :code (if (eql (parachute:status report) :passed) 0 1)))
