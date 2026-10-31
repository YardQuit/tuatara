;;;; test/grammars.lisp --- Which grammars read text: the common ones, those turned on, the question.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test grammars-on
  :description "Only the grammars on read text: the common ones and those turned on. A file in a
language whose grammar is off asks once whether to turn it on, the answer kept between sessions;
text in such a language inside another file is left plain; a file opened safely asks nothing.")

(defmacro with-grammar-answers-forgotten ((&rest names) &body body)
  "Run BODY with no answer kept for the grammars of the languages NAMES (strings),
none given up in this session, and *GRAMMARS-ON* as it is by default; the same
afterwards."
  `(flet ((forget ()
            (tuatara.defaults::state-list-drop tuatara.defaults::*grammar-answers* (list ,@names))
            (setf tuatara.defaults::*grammar-answers-read* nil
                  tuatara.defaults::*grammars-not-now* '())))
     (forget)
     ;; the grammars loaded first, as a session started earlier would have them
     (tuatara.syntax:rules-helper)
     (dolist (grammar '("awk" "markdown" "markdown_inline" "python")) (tuatara.syntax:find-grammar grammar))
     (let ((tuatara-user:*grammars-on* '(:common)))
       (unwind-protect (progn ,@body)
         (forget)))))

(defun row-faces (f row)
  "The faces of the spans of ROW in the last frame of the fake frontend F, each once."
  (remove-duplicates (alexandria:flatten (mapcar #'third (aref (tuatara.frontend.fake:fake-spans f) row)))))

(defparameter *awk-in-markdown* (format nil "# Notes~%~%```awk~%BEGIN { print \"x\" }~%```~%")
  "A Markdown file holding a code block in AWK, whose grammar is not a common one.")

(define-test (grammars-on text-inside-left-plain-when-off)
  ;; the AWK in a Markdown code block is coloured by AWK's grammar only once it is on
  (with-grammar-answers-forgotten ("awk")
    (let ((off (row-faces (colour-session "" *awk-in-markdown* "md") 3)))
      (true (member :markup.raw.block off) "the code block is coloured as Markdown's")
      (false (member :namespace off) "AWK's BEGIN not coloured while its grammar is off"))
    (let ((tuatara-user:*grammars-on* '(:common awk)))
      (true (member :namespace (row-faces (colour-session "" *awk-in-markdown* "md") 3))
            "AWK's BEGIN coloured once its grammar is on"))))

(define-test (grammars-on file-asks-and-yes-turns-on)
  ;; a file in AWK asks; y turns the grammar on, the file is coloured, and the
  ;; answer is kept: the next file in AWK asks nothing
  (with-grammar-answers-forgotten ("awk")
    (with-file-session ((f editor) "y" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar"))
      (true (member :keyword (row-faces f 0)) "the file is coloured once its grammar is on"))
    (is eq :on (gethash "awk" (tuatara.defaults::grammar-answers :again t)))
    (with-file-session ((f editor) "" (format nil "BEGIN { print 1 }~%") "awk")
      (false (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar") "asked once only")
      (true (member :keyword (row-faces f 0))))))

(define-test (grammars-on file-asks-and-no-says-how-later)
  ;; n keeps the grammar off, says how to turn it on later, and is kept: the
  ;; next file asks nothing; turn-on-grammar turns it on after all
  (with-grammar-answers-forgotten ("awk")
    (with-file-session ((f editor) "n" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar"))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "The AWK grammar stays off: M-x turn-on-grammar turns it on"))
      (false (member :keyword (row-faces f 0)) "not coloured while its grammar is off"))
    (with-file-session ((f editor) (format nil "M-x ~a RET RET" (key-string "turn-on-grammar"))
                        (format nil "BEGIN { print 1 }~%") "awk")
      (false (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar") "asked once only")
      (true (tuatara.frontend.fake:fake-frame-showing f "The AWK grammar is on"))
      (true (member :keyword (row-faces f 0)) "coloured once turned on by the command"))))

(define-test (grammars-on common-grammar-asks-nothing)
  ;; a file in a common language is parsed as it opens, without a question
  (with-grammar-answers-forgotten ()
    (with-file-session ((f editor) "" (format nil "def f(x):~%    return x~%") "py")
      (false (tuatara.frontend.fake:fake-frame-showing f "grammar?"))
      (true (row-faces f 0)))))

(define-test (grammars-on safe-visit-asks-nothing)
  ;; a file opened safely is never parsed, and asks nothing
  (with-grammar-answers-forgotten ("awk")
    (with-test-directory (directory)
      (let ((path (merge-pathnames "evidence.awk" directory)))
        (with-open-file (o path :direction :output) (format o "BEGIN { print 1 }~%"))
        (let ((tuatara-user:*safe-directories* (list (namestring directory))))
          (multiple-value-bind (f editor) (run-keys "" :file path :width 60)
            (unwind-protect
                 (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (true (tuatara.io:safe-visit-p buffer))
                   (false (tuatara.frontend.fake:fake-frame-showing f "grammar?"))
                   (false (tuatara.syntax:buffer-syntax buffer)))
              (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))))))

(define-test (grammars-on nothing-done-after-commands-unless-asking)
  ;; nothing is looked at after each command while no question is pending,
  ;; nor once the question is answered
  (with-grammar-answers-forgotten ("awk")
    (multiple-value-bind (f editor) (run-keys "a b c" :text "abc")
      (declare (ignore f))
      (false (member 'tuatara.defaults::ask-about-shown-grammar (tuatara.command:editor-post-command-functions editor))))
    (with-file-session ((f editor) "n a b" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "Turn on the AWK grammar"))
      (false (member 'tuatara.defaults::ask-about-shown-grammar (tuatara.command:editor-post-command-functions editor))))))

(defun typed-ahead-session (keys text extension)
  "Run KEYS (as QUEUE-TEST-KEYS takes them) on a file with EXTENSION holding
TEXT, every key queued counting as typed ahead, waiting in the frontend;
return the fake frontend."
  (with-test-directory (directory)
    (let ((path (merge-pathnames (format nil "sample.~a" extension) directory))
          (f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 8)))
      (with-open-file (o path :direction :output) (write-string text o))
      (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
      (queue-test-keys f keys)
      (let ((editor (tuatara.command:run-editor f :files (list path) :setup #'tuatara.defaults:install-defaults)))
        (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      f)))

(define-test (grammars-on keys-typed-ahead-never-answer)
  ;; a key typed before the question shows never answers it: one pressed
  ;; after does, one key alone; with none, it is "not now", nothing kept
  (with-grammar-answers-forgotten ("awk")
    (let ((f (typed-ahead-session (list "y" :tick "n") (format nil "BEGIN { print 1 }~%") "awk")))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "The AWK grammar stays off"))
      (is eq :off (gethash "awk" (tuatara.defaults::grammar-answers :again t)) "the n pressed after")))
  (with-grammar-answers-forgotten ("awk")
    (let ((f (typed-ahead-session "y" (format nil "BEGIN { print 1 }~%") "awk")))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "asked again when Tuatara starts again"))
      (false (gethash "awk" (tuatara.defaults::grammar-answers :again t)) "nothing kept")
      (true (member "awk" tuatara.defaults::*grammars-not-now* :test #'string=) "not now"))))

(define-test (grammars-on other-key-is-not-now)
  ;; a key other than y or n is "not now" for this session, and nothing is kept
  (with-grammar-answers-forgotten ("awk")
    (with-file-session ((f editor) "x" (format nil "BEGIN { print 1 }~%") "awk")
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "asked again when Tuatara starts again")))
    (false (gethash "awk" (tuatara.defaults::grammar-answers :again t)))
    (true (member "awk" tuatara.defaults::*grammars-not-now* :test #'string=))))

(define-test (grammars-on answers-read-again-only-when-changed)
  ;; looking at the answers again, as each command does while a question is
  ;; pending, makes nothing new while no answer has changed
  (with-grammar-answers-forgotten ("awk")
    (let ((table (tuatara.defaults::grammar-answers :again t))
          (on (tuatara.defaults::grammars-on-table)))
      (is eq table (tuatara.defaults::grammar-answers :again t))
      (is eq on (tuatara.defaults::grammars-on-table)))))
