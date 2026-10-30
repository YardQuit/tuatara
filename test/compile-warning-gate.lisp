;;;; test/compile-warning-gate.lisp --- The compile in test/run.lisp sees a variable used before its definition.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test compile-warning-gate
  :description "The first compile of the project gives the warnings the test run fails on: a variable used in a file before the file defining it is one.")

(defun gate-warnings-of (dir first second)
  "Make a system of two files in DIR, FIRST and SECOND (their text) compiled in
that order, load it in a Lisp of its own as test/run.lisp loads the project,
and return the warnings that gave, one string each."
  (flet ((put (name text)
           (with-open-file (out (merge-pathnames name dir) :direction :output :if-exists :supersede)
             (write-string text out))))
    (put "gate-scratch.asd" "(asdf:defsystem \"gate-scratch\" :serial t :components ((:file \"first\") (:file \"second\")))")
    (put "first.lisp" first)
    (put "second.lisp" second)
    (let ((output (uiop:run-program
                   (list (namestring sb-ext:*runtime-pathname*) "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                         "--eval" "(require :asdf)"
                         "--load" (namestring (asdf:system-relative-pathname :tuatara "test/compile-gate.lisp"))
                         "--eval" (format nil "(push #p~s asdf:*central-registry*)" (namestring dir))
                         "--eval" "(format t \"~&~{GATE: ~a~%~}\" (tuatara-compile-gate:compile-warnings \"gate-scratch\" :own \"gate-scratch\"))")
                   :output :string :error-output nil :ignore-error-status t)))
      (loop for line in (uiop:split-string output :separator '(#\Newline))
            when (uiop:string-prefix-p "GATE: " line) collect line))))

(define-test (compile-warning-gate variable-used-before-its-file)
  (with-test-directory (dir)
    (let ((warnings (gate-warnings-of dir "(defun gate-use () (setq *gate-late* 1))" "(defvar *gate-late* nil)")))
      (true (some (lambda (line) (search "*GATE-LATE*" line)) warnings))))
  (with-test-directory (dir)
    (is equal '() (gate-warnings-of dir "(defvar *gate-late* nil)" "(defun gate-use () (setq *gate-late* 1))"))))
