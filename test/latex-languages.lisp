;;;; test/latex-languages.lisp --- The language an Org note says it is in sets the export's language.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test latex-languages
  :description "#+language: in an Org note sets the language of the PDF: polyglossia for xelatex and lualatex,
babel for pdflatex, so that words are broken at the line ends by its rules and the contents and captions are
named in it; a code the export does not know, or a program that cannot set it, is said once and the document
keeps LaTeX's own; a template or #+latex_header: that loads polyglossia or babel itself is left alone; the
Word export names its tables, pictures and contents in it. M-x check-health says which hyphenation patterns
are installed.")

(define-test (latex-languages polyglossia)
  (let ((latex (org-latex-of (format nil "#+title: T~%#+language: sv~%* Del~%Text.~%"))))
    (true (search (format nil "\\usepackage{polyglossia}~%\\setmainlanguage{swedish}") latex))
    ;; after fontspec, which polyglossia needs
    (true (< (search "\\usepackage{fontspec}" latex) (search "\\usepackage{polyglossia}" latex))))
  (is string= "british" (third (tuatara.defaults::export-language "en-GB")))
  (is string= "brazil" (third (tuatara.defaults::export-language "pt_BR")))
  ;; a country with no row of its own: the language's
  (is string= "swedish" (third (tuatara.defaults::export-language "sv-FI")))
  (false (tuatara.defaults::export-language "xx"))
  (let ((latex (org-latex-of (format nil "#+language: sv~%Text.~%") :enhanced t)))
    (true (search "\\setmainlanguage{swedish}" latex))))

(define-test (latex-languages babel)
  (let ((latex (org-latex-of (format nil "#+language: de~%Text.~%") :engine "pdflatex")))
    (true (search "\\usepackage[ngerman]{babel}" latex))
    (false (search "polyglossia" latex))))

(define-test (latex-languages no-language-line)
  (dolist (enhanced '(nil t))
    (let ((latex (org-latex-of (format nil "Text.~%") :enhanced enhanced)))
      (false (search "polyglossia" latex))
      (false (search "babel" latex)))))

(define-test (latex-languages unknown-code)
  (multiple-value-bind (latex named map missing notes) (org-latex-of (format nil "#+language: xx~%Text.~%"))
    (declare (ignore named map missing))
    (false (search "polyglossia" latex))
    (is = 1 (count-if (lambda (note) (search "#+language: xx is not a language the export knows" note)) notes))))

(defun occurrences (needle text)
  "How many times NEEDLE is in TEXT."
  (let ((count 0) (start 0))
    (loop for at = (search needle text :start2 start)
          while at do (incf count) (setf start (1+ at)))
    count))

(define-test (latex-languages template-sets-its-own)
  (loop for (header needle) in '(("\\usepackage{polyglossia}" "polyglossia")
                                 ("\\usepackage[english]{babel}" "babel")
                                 ("\\setmainlanguage{german}" "setmainlanguage"))
        do (let ((latex (org-latex-of (format nil "#+language: sv~%#+latex_header: ~a~%Text.~%" header))))
             (false (search "{swedish}" latex))
             (is = 1 (occurrences needle latex))))
  ;; a template of its own that loads polyglossia where it likes
  (with-test-directory (dir)
    (alexandria:write-string-into-file
     (format nil "\\documentclass{article}~%{{{packages}}}\\usepackage{polyglossia}~%\\setmainlanguage{german}~%\\begin{document}~%{{{body}}}~%\\end{document}~%")
     (merge-pathnames "own.tex" dir))
    (let* ((file (namestring (merge-pathnames "n.org" dir)))
           (latex (org-latex-of (format nil "#+language: sv~%#+template: ./own.tex~%Text.~%") :file file :enhanced t)))
      (false (search "{swedish}" latex))
      (is = 1 (occurrences "usepackage{polyglossia}" latex))
      (is = 1 (occurrences "setmainlanguage" latex)))))

(define-test (latex-languages cannot-set)
  (multiple-value-bind (latex named map missing notes) (org-latex-of (format nil "#+language: tr~%Text.~%") :engine "pdflatex")
    (declare (ignore named map missing notes))
    (true (search "\\usepackage[turkish]{babel}" latex)))
  (let ((tuatara.defaults::*export-languages* (list (list "zz" "Zed" "zed" nil "zz" "hyphen-zed" "T" "F" "C"))))
    (multiple-value-bind (latex named map missing notes) (org-latex-of (format nil "#+language: zz~%Text.~%") :engine "pdflatex")
      (declare (ignore named map missing))
      (false (search "babel" latex))
      (is = 1 (count-if (lambda (note) (search "Zed cannot be set by pdflatex" note)) notes)))))

(define-test (latex-languages word-document)
  (flet ((docx (language)
           (docx-text (tuatara.defaults::org-docx-document
                       (format nil "~@[#+language: ~a~%~]#+options: toc:t~%#+caption: Mått~%| a |~%~%* Del~%Text.~%" language)))))
    (let ((text (docx "sv")))
      (true (search "Tabell 1: " text))
      (true (search ">Innehåll<" text))
      (false (search "Table 1" text)))
    (let ((text (docx nil)))
      (true (search "Table 1: " text))
      (true (search ">Contents<" text)))
    (true (search "Tabelle 1: " (docx "de")))))

(defun stand-in-kpsewhich (directory present)
  "A kpsewhich in DIRECTORY that finds the hyphenation files named in PRESENT, endings (sv),
and no other. Returns the folder of its engine's name beside it."
  (let ((script (namestring (merge-pathnames "kpsewhich" directory))))
    (alexandria:write-string-into-file
     (format nil "#!/bin/sh~%for f in \"$@\"; do case \"$f\" in ~{loadhyph-~a.tex) echo /texmf/hyph/$f;; ~}esac; done~%" present)
     script :if-exists :supersede)
    (sb-posix:chmod script #o700)
    script))

(define-test (latex-languages patterns-missing)
  (with-test-directory (dir)
    (let* ((engine (stand-in-latex dir "xelatex"))
           (tuatara-user:*latex-engines* (list engine)))
      (stand-in-kpsewhich dir '("en-us" "da"))
      (let ((note (tuatara.defaults::latex-language-patterns-note "xelatex" "sv")))
        (true (search "no hyphenation patterns for Swedish" note))
        (true (search "hyphen-swedish" note)))
      (false (tuatara.defaults::latex-language-patterns-note "xelatex" "da"))
      (let ((words (tuatara.defaults::latex-language-words)))
        (true (search "Hyphenation in PDFs by #+language: en, en-us" words))
        (true (search "patterns missing for" words))
        (true (search "sv" (subseq words (search "patterns missing for" words)))))
      ;; no kpsewhich to ask: nothing is said
      (delete-file (merge-pathnames "kpsewhich" dir))
      (let ((tuatara.defaults::*tex-live-directories* '()))
        (false (tuatara.defaults::latex-language-patterns-note "xelatex" "sv"))))))

(defun real-latex-log (dir latex name)
  "Run the real xelatex on LATEX (text) in DIR as NAME.tex; its log's text, or NIL when it is not here."
  (let ((engine (cdr (assoc "xelatex" (tuatara.defaults::latex-engines-found) :test #'string=))))
    (when (and engine (tuatara.defaults::kpsewhich-for "xelatex"))
      (alexandria:write-string-into-file latex (merge-pathnames (format nil "~a.tex" name) dir))
      (uiop:run-program (list engine "-interaction=nonstopmode" "-halt-on-error" (format nil "~a.tex" name))
                        :directory dir :ignore-error-status t :output nil :error-output nil)
      (alexandria:read-file-into-string (merge-pathnames (format nil "~a.log" name) dir)))))

(define-test (latex-languages swedish-hyphenation-in-a-real-pdf)
  (when (and (tuatara.defaults::latex-engines-found)
             (null (tuatara.defaults::latex-language-patterns-note "xelatex" "sv")))
    (with-test-directory (dir)
      (flet ((shown (language)
               (let* ((note (format nil "~@[#+language: ~a~%~]#+latex: \\showhyphens{kärlekshistoria}~%" language))
                      (log (real-latex-log dir (org-latex-of note) (format nil "t-~a" (or language "none")))))
                 (and log (true (search "Output written" log)))
                 log)))
        (let ((swedish (shown "sv")) (plain (shown nil)))
          (when (and swedish plain)
            ;; Swedish breaks kärlekshistoria as kär-leks-hi-sto-ria; English patterns do not
            (true (search "kär-leks-hi-sto-ria" swedish))
            (false (search "kär-leks-hi-sto-ria" plain))
            (true (search "Output written" swedish))))))))
