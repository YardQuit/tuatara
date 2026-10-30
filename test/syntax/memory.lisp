;;;; test/syntax/memory.lisp --- A scanner writing past its own memory, in a process of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by the tests of test/syntax.lisp with sbcl --script: CASE OUT
;;;; DIRECTORY, DIRECTORY holding the grammar of test/grammars/misbehaving/
;;;; compiled. Has a scanner write past the memory given to it, parses, and
;;;; writes to OUT what happened, a property list (see the tests). Nothing is
;;;; written when the process ends before.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara :silent t)

(defun parse-done (syntax)
  "Wait for SYNTAX's parses to finish, a minute at most, and take their result."
  (loop repeat 6000
        do (tuatara.syntax:catch-up syntax)
        while (tuatara.syntax:syntax-job syntax)
        do (sleep 0.01)))

(defun run-case (case directory)
  "Run CASE; what it found, as a property list."
  (let ((found '()))
    (flet ((note (key value) (setf (getf found key) value)))
      (cond
        ((string= case "overflow")
         ;; a scanner writing past the buffer for its state, a little or much
         (let ((grammar (let ((tuatara.syntax:*grammar-directories*
                                (cons (pathname directory) (tuatara.syntax:grammar-directories))))
                          (tuatara.syntax:find-grammar "misbehaving"))))
           (note :faults (loop for text in '("abc !o def" "abc !O def")
                               collect (handler-case (progn (tuatara.syntax:parse-rope (tuatara.syntax:make-parser grammar)
                                                                                       (tuatara.text:string->rope text))
                                                            :parsed)
                                         (sb-sys:memory-fault-error () :fault))))
           (let* ((buffer (tuatara.buffer:make-buffer "*overflow*" :text "abc !O def"))
                  (syntax (tuatara.syntax:attach-syntax buffer grammar)))
             (parse-done syntax)
             (note :buffer-fault (and (typep (tuatara.syntax:grammar-faulted grammar) 'sb-sys:memory-fault-error) t))
             (note :buffer-broken (tuatara.syntax:syntax-broken syntax))
             ;; editing goes on
             (tuatara.buffer:buffer-insert buffer 0 "more "))
           (let ((tree (tuatara.syntax:parse-rope (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json"))
                                                  (tuatara.text:string->rope "[1, 2]"))))
             (note :other-parses (and tree t)))))))
    found))

(destructuring-bind (case out directory) (last sb-ext:*posix-argv* 3)
  (let ((found (run-case case directory)))
    (with-open-file (o out :direction :output :if-exists :supersede)
      (with-standard-io-syntax (prin1 found o)))))
