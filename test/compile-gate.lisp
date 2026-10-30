;;;; test/compile-gate.lisp --- Load the project, collecting the compiler's warnings.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded by test/run.lisp before any of the project is, in a package of its
;;;; own: a variable used in a file before the one defining it is a warning only
;;;; while the project is not loaded yet, so the project's systems are compiled
;;;; here for the first time, and every warning of that compile is kept.

(defpackage #:tuatara-compile-gate
  (:use #:cl)
  (:export #:compile-warnings)
  (:documentation "Loading a system with the warnings its compile gives collected."))

(in-package #:tuatara-compile-gate)

(defun own-system-p (name own)
  "True when the system called NAME is OWN or one of OWN's parts (OWN/part)."
  (and (stringp name)
       (or (string= name own)
           (and (> (length name) (length own))
                (string= own name :end2 (length own))
                (char= #\/ (char name (length own)))))))

(defun needed-systems (system)
  "The names of SYSTEM and every system it needs, each one's needs too."
  (let ((seen '()))
    (labels ((visit (name)
               (let ((name (string-downcase (string name))))
                 (unless (member name seen :test #'string=)
                   (push name seen)
                   (let ((found (asdf:find-system name nil)))
                     (when found
                       (mapc #'visit (remove-if-not #'stringp (asdf:system-depends-on found)))
                       (mapc #'visit (remove-if-not #'stringp (asdf:system-defsystem-depends-on found)))))))))
      (visit system))
    (reverse seen)))

(defun compile-warnings (system &key (own "tuatara") (load-outside #'identity))
  "Load SYSTEM, every part of OWN compiled afresh, and return the full
warnings that gave, in words, in order (style warnings are not counted). What
SYSTEM needs from outside OWN is loaded first, as LOAD-OUTSIDE, a function of
their names, says; nothing of OWN is loaded before, so that a variable used
before the file that defines it is seen."
  (let* ((needed (needed-systems system))
         (warnings '()))
    (funcall load-outside (remove-if (lambda (name) (own-system-p name own)) needed))
    (handler-bind ((warning (lambda (w)
                              (unless (or (typep w 'style-warning) (typep w 'uiop:compile-warned-warning))
                                (push (princ-to-string w) warnings)))))
      (asdf:load-system system :force (remove-if-not (lambda (name) (own-system-p name own)) needed)))
    (reverse warnings)))
