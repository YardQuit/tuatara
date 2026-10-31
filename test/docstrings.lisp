;;;; test/docstrings.lisp --- Every public symbol is documented.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(defun tuatara-packages ()
  "Return every package whose name starts with TUATARA, excluding the test packages."
  (remove-if-not (lambda (p)
                   (let ((n (package-name p)))
                     (and (>= (length n) 7) (string= "TUATARA" n :end2 7)
                          (not (member n '("TUATARA.TESTS" "TUATARA.BENCH") :test #'string=)))))
                 (list-all-packages)))

(defun undocumented-symbols (package)
  "Return a list of strings naming external symbols of PACKAGE that lack a
docstring in any role they fill: function or macro, special variable (or a
name standing for one), class, type. A symbol that fills no role at all (exported but never defined) is
reported too, since exporting it was a promise."
  (let ((missing '()))
    (do-external-symbols (s package)
      (let ((roles 0))
        (when (fboundp s)
          (incf roles)
          (unless (documentation s 'function) (push (format nil "~a (function)" s) missing)))
        (when (member (sb-int:info :variable :kind s) '(:special :constant :macro))
          (incf roles)
          (unless (documentation s 'variable) (push (format nil "~a (variable)" s) missing)))
        (when (find-class s nil)
          (incf roles)
          (unless (documentation (find-class s) t) (push (format nil "~a (class)" s) missing)))
        (when (and (not (find-class s nil)) (sb-int:info :type :kind s))
          (incf roles)
          (unless (documentation s 'type) (push (format nil "~a (type)" s) missing)))
        (when (zerop roles) (push (format nil "~a (exported but undefined)" s) missing))))
    (sort missing #'string<)))

(define-test documentation
  :description "Self-documentation gate: every public symbol of every TUATARA package has a docstring.")

(define-test (documentation packages-have-docstrings)
  (let ((undocumented (remove-if (lambda (p) (documentation p t)) (tuatara-packages))))
    (true (null undocumented) "Packages without a docstring: ~{~a~^, ~}"
          (mapcar #'package-name undocumented))))

(define-test (documentation public-symbols-have-docstrings)
  (dolist (p (tuatara-packages))
    (let ((missing (undocumented-symbols p)))
      (true (null missing) "~a: undocumented public symbols: ~{~a~^, ~}" (package-name p) missing))))

(defparameter *borrowed-phrases*
  '("as emacs" "emacs's" "your emacs" "as helix" "helix's" "as kitty does" "as vim" "vim's")
  "Phrases that explain a thing by how another program behaves, which no
docstring may hold: a docstring says what a thing does on its own.")

(defun borrowed-phrase-docstrings ()
  "Return a list of strings naming the definitions of the TUATARA packages
whose docstring holds one of *BORROWED-PHRASES*, any case, each with the phrase
found."
  (let ((found '()))
    (dolist (p (tuatara-packages))
      (let ((doc (documentation p t)))
        (dolist (phrase *borrowed-phrases*)
          (when (and doc (search phrase doc :test #'char-equal))
            (push (format nil "package ~a (~a)" (package-name p) phrase) found))))
      (do-symbols (s p)
        (when (eq (symbol-package s) p)
          (dolist (doc (list (and (fboundp s) (documentation s 'function))
                             (documentation s 'variable)
                             (let ((class (find-class s nil))) (and class (documentation class t)))))
            (dolist (phrase *borrowed-phrases*)
              (when (and doc (search phrase doc :test #'char-equal))
                (push (format nil "~s (~a)" s phrase) found)))))))
    (sort (remove-duplicates found :test #'string=) #'string<)))

(define-test (documentation docstrings-stand-on-their-own)
  (let ((found (borrowed-phrase-docstrings)))
    (true (null found) "Docstrings that explain by another program: ~{~a~^, ~}" found)))

(define-test (documentation csv-column-faces-count-from-zero)
  (dotimes (n 8)
    (let* ((face (tuatara.display:find-face (intern (format nil "CSV-COLUMN-~d" n) :keyword)))
           (doc (tuatara.display:face-doc face)))
      (true (search (format nil "columns ~d, ~d, ~d" n (+ n 8) (+ n 16)) doc)
            "The face for column ~d says ~s" n doc))))
