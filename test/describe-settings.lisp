;;;; test/describe-settings.lisp --- The commands that say what to write in a file: their texts hold every word read.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test describe-settings
  :description "describe-export-settings, describe-inline-picture-settings, describe-org-keyword-settings,
describe-file-local-settings, describe-table-formula-settings, describe-capture-template-settings and
describe-timestamp-settings show their sections, and each keyword, word or placeholder the code reads is in their
text.")

(defun described (command)
  "The text the command named COMMAND (a string) shows in the *Help* buffer,
run as M-x runs it."
  (run-keys (format nil "M-x ~a RET" (typed-keys command)) :width 100 :height 8)
  (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*")))

(defun source-file-text (relative)
  "The text of the source file RELATIVE, a path from the source tree."
  (alexandria:read-file-into-string (merge-pathnames relative tuatara.runtime:*source-tree*) :external-format :utf-8))

(defun source-definition (relative name)
  "The text of the definition of NAME (the form beginning \"(defun NAME\" or
\"(defparameter NAME\") in the source file RELATIVE, to the next form that
begins a line."
  (let* ((text (source-file-text relative))
         (start (or (search (format nil "~%(defun ~a " name) text) (search (format nil "~%(defparameter ~a" name) text)
                    (search (format nil "~%(define-command ~a " name) text))))
    (true start "~a is defined in ~a" name relative)
    (let ((end (search (format nil "~%(") text :start2 (1+ start))))
      (subseq text start end))))

(defun quoted-strings (text)
  "The strings written between double quotes in the Lisp TEXT, escapes
undone."
  (let ((found '()))
    (cl-ppcre:do-register-groups (body) ("\"((?:[^\"\\\\]|\\\\.)*)\"" text)
      (push (cl-ppcre:regex-replace-all "\\\\(.)" body "\\1") found))
    (nreverse found)))

(defun source-matches-in (text regex)
  "What the first group of REGEX matches in TEXT, as a list."
  (let ((found '()))
    (cl-ppcre:do-register-groups (word) (regex text)
      (push word found))
    (nreverse found)))

(defun source-matches (regex)
  "What the first group of REGEX matches in every Lisp file under lisp/, as a list."
  (let ((found '()))
    (dolist (file (directory (merge-pathnames "lisp/**/*.lisp" tuatara.runtime:*source-tree*)))
      (cl-ppcre:do-register-groups (word) (regex (alexandria:read-file-into-string file :external-format :utf-8))
        (pushnew word found :test #'string=)))
    (sort found #'string<)))

(defun missing-from (text words &key (test #'search))
  "The WORDS that TEXT does not hold, in any case."
  (remove-if (lambda (word) (funcall test word text :test #'char-equal)) words))

(define-test (describe-settings every-command-shows-its-sections)
  (loop for (command . headings) in '(("describe-export-settings" "Settings of an :EXPORT: drawer" "A heading" "A table"
                                       "In the PDF and the Word document alike")
                                      ("describe-inline-picture-settings" "Pictures among the lines"
                                       "What shows as a picture" "#+ATTR_ORG: and the lines like it"
                                       "Other #+ATTR_...: lines" "Turning pictures on" "Keys" "Settings")
                                      ("describe-org-keyword-settings" "Keyword lines of an Org note" "The lines"
                                       "Words of #+OPTIONS:" "Words of #+STARTUP:" "Only Tuatara reads")
                                      ("describe-file-local-settings" "Settings a file gives itself"
                                       "On the first line" "In a block near the end" "Settings taken" "Values"
                                       "What is left alone")
                                      ("describe-table-formula-settings" "Formulas under a table"
                                       "What a formula fills" "References" "What a field holds" "Operators"
                                       "Functions" "Flags after ;")
                                      ("describe-capture-template-settings" "Capture templates" "The head"
                                       "In the text")
                                      ("describe-timestamp-settings" "Timestamps" "Active and inactive" "Repeaters"
                                       "Warnings and delays" "Planning lines" "Dates by rule"
                                       "One time of a repeat"))
        do (let ((text (described command)))
             (is equal '() (missing-from text headings) "~a lacks sections" command))))

(define-test (describe-settings every-text-is-filled-to-the-help-column)
  (dolist (command '("describe-export-settings" "describe-inline-picture-settings" "describe-org-keyword-settings"
                     "describe-file-local-settings" "describe-table-formula-settings"
                     "describe-capture-template-settings" "describe-timestamp-settings"))
    (let ((long (remove-if-not (lambda (line) (> (length line) tuatara.defaults::*help-column*))
                               (cl-ppcre:split "\\n" (described command)))))
      (is equal '() long "~a has lines past the help column" command))))

(defun indents-in-blocks (text)
  "For each run of lines of TEXT between blank lines, the different indents
of its lines set in more than two columns, as a list of lists."
  (let ((blocks '()))
    (dolist (block (cl-ppcre:split "\\n\\n+" text))
      (let ((indents '()))
        (dolist (line (cl-ppcre:split "\\n" block))
          (let ((indent (or (position #\Space line :test #'char/=) 0)))
            (when (> indent 2) (pushnew indent indents))))
        (push (sort indents #'<) blocks)))
    (nreverse blocks)))

(define-test (describe-settings rows-keep-two-columns)
  ;; what is written stands at column 2 and what it does at one column for the whole block, the later lines of a
  ;; row under the first
  (dolist (command '("describe-export-settings" "describe-inline-picture-settings" "describe-org-keyword-settings"
                     "describe-file-local-settings" "describe-table-formula-settings"
                     "describe-capture-template-settings" "describe-timestamp-settings"))
    (let ((text (described command)))
      (dolist (indents (indents-in-blocks text))
        (true (<= (length indents) 1) "~a has a block whose lines are set in by ~a" command indents))
      ;; a name run into its words, a colon and one space, is no row
      (false (cl-ppcre:scan "(?m)^  [a-z ]+: [a-z]" (if (string= command "describe-export-settings")
                                                         (subseq text (search (format nil "~%A heading~%") text)
                                                         (search "A table's lines" text)) ""))
             "~a runs a name into its words" command)))
  ;; the settings every kind takes are written once
  (let ((text (described "describe-export-settings")))
    (is = 1 (count-matches (format nil "~%  new page:") text))
    (true (search "Any of them takes" text))))

(define-test (describe-settings org-keywords-are-all-described)
  (let* ((text (described "describe-org-keyword-settings"))
         (read (append (source-matches "org-export-keyword(?:-all)? \"([A-Z_]+)\"")
                       (source-matches "org-file-keyword [a-z]+ \"([A-Z_]+)\"")
                       (source-matches "\\(string-equal key \"([A-Z_]+)\"")
                       (loop for list in (source-matches "org-export-tag-list '\\(([^)]*)\\)")
                             append (quoted-strings list))
                       '("TODO" "SEQ_TODO" "TYP_TODO" "PROPERTY" "FILETAGS" "STARTUP" "OPTIONS"))))
    (is equal '() (missing-from text (mapcar (lambda (word) (format nil "#+~a" word))
                                             (remove-duplicates read :test #'string=)))
        "keyword lines the code reads but the text does not name")
    ;; every option a reader asks for, and the two that hide text
    (let ((options (append (source-matches "org-export-option \"([^\"]+)\"") '("redact" "protect"))))
      (is equal '() (missing-from text (mapcar (lambda (option)
                                                 (format nil "~a:" (cl-ppcre:regex-replace-all "\\\\\\\\" option "\\\\")))
                                               (remove-duplicates options :test #'string=)))
          "options read but not described"))
    (is equal '() (missing-from text (mapcar #'first tuatara.defaults::*org-startup-words*))
        "#+STARTUP: words not described")))

(define-test (describe-settings tuatara-only-names-of-the-manual-are-described)
  ;; the keyword lines the manual's table "What Tuatara adds to Org" lists as Tuatara's own (MANUAL-TUATARA-WRITTEN)
  ;; are in the text; the drawer, macros, tags and properties of that table have their own sections or tips
  (let ((text (described "describe-org-keyword-settings"))
        (keywords (loop for shown in (manual-tuatara-written)
                        when (uiop:string-prefix-p "#+" shown)
                          collect (first (cl-ppcre:split "[ :]" shown)))))
    (true keywords "the table names keyword lines")
    (is equal '() (missing-from text keywords) "keyword lines the manual lists as Tuatara's own, not in the text")))

(define-test (describe-settings startup-kinds-are-those-the-readers-ask-for)
  (let ((kinds (mapcar (lambda (kind) (string-upcase (string-left-trim ":" kind)))
                       (source-matches "org-startup-choice options (:[a-z]+)")))
        (known (remove-duplicates (mapcar (lambda (entry) (symbol-name (second entry)))
                                          tuatara.defaults::*org-startup-words*)
                                  :test #'string=)))
    (is equal '() (set-difference kinds known :test #'string=))
    (is equal '() (set-difference known kinds :test #'string=))))

(define-test (describe-settings inline-picture-settings-are-all-described)
  (let ((text (described "describe-inline-picture-settings")))
    (is equal '() (missing-from text (mapcar (lambda (name) (format nil ":~a" name))
                                             (source-matches-in (source-definition "lisp/org/pictures.lisp"
                                                                                   "org-picture-attributes")
                                                                "\\(setting arguments \"([a-z]+)\"")))
        "arguments org-picture-attributes reads that the text lacks")
    (is equal '() (missing-from text (list "width" "align" "center" "float" "placement" "left" "right" "wrap" "px")))
    (is equal '() (missing-from text (mapcar (lambda (keyword) (format nil "#+~a" keyword))
                                             tuatara.defaults::*org-attribute-keywords*)))
    (is equal '() (missing-from text (mapcar #'cdr tuatara.defaults::*image-formats*)))
    (is equal '() (missing-from text '("inlineimages" "noinlineimages" "latexpreview" "nolatexpreview"
                                       "*org-startup-shows-pictures*" "*inline-picture-width*"
                                       "*global-inline-pictures-mode*" "*inline-pictures-modes*"
                                       "*formula-preamble*" "inline-pictures-mode"
                                       "C-c C-x w" "C-c C-x p" "C-c C-l")))
    (is equal '() (missing-from text tuatara.defaults::*latex-math-environments*))))

(define-test (describe-settings file-local-settings-are-all-described)
  (let ((text (described "describe-file-local-settings")))
    (is equal '() (missing-from text (mapcar #'first tuatara.defaults::*file-local-settings*)))
    (is equal '() (missing-from text '("-*-" "Local Variables:" "End:" "eval" "coding" "lexical-binding"
                                       "*file-local-variables*" "*file-local-setting-functions*" "#!")))
    ;; the table is what takes a setting: a name outside it is refused, those inside are taken
    (let ((buffer (tuatara.buffer:make-buffer "*local-names*" :text "")))
      (unwind-protect
           (progn
             (false (tuatara.defaults::apply-file-local-setting buffer "eval" "(delete-file)"))
             (false (tuatara.defaults::apply-file-local-setting buffer "tab-width" 0))
             (true (tuatara.defaults::apply-file-local-setting buffer "tab-width" 4))
             (true (tuatara.defaults::apply-file-local-setting buffer "fill-column" 60))
             (false (tuatara.defaults::apply-file-local-setting buffer "indent-tabs-mode" "yes")))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (describe-settings table-formulas-are-all-described)
  (let* ((text (described "describe-table-formula-settings"))
         (functions (remove-if-not (lambda (word) (cl-ppcre:scan "^[a-z][a-z0-9]+$" word))
                                   (quoted-strings (source-definition "lisp/org/formulas.lisp" "org-calc-function"))))
         (flags (let ((found '()))
                  (cl-ppcre:do-register-groups (letters) ("\\(\\(?((?:#\\\\[A-Za-z] ?)+)\\)? \\("
                                                           (source-definition "lisp/org/formulas.lisp"
                                                                              "org-read-formula-flags"))
                    (cl-ppcre:do-register-groups (letter) ("#\\\\([A-Za-z])" letters) (push letter found)))
                  found))
         (operators (loop for string in (quoted-strings (source-definition "lisp/org/formulas.lisp"
                                                                                  "org-formula-tokens"))
                                 append (remove-if-not (lambda (word) (cl-ppcre:scan "^[=!<>&|.]{2}$" word))
                                                       (uiop:split-string string :separator '(#\Space))))))
    (true functions "functions found in the reader")
    (true flags "flags found in the reader")
    (is equal '() (missing-from text functions) "functions the reader calls, not in the text")
    (is equal '() (remove-if (lambda (letter) (some (lambda (entry) (search letter (first entry)))
                                                    tuatara.defaults::*org-formula-flags*))
                             flags)
        "flags the reader takes, not in the list")
    (is equal '() (missing-from text operators) "two-letter operators not in the text")
    (is equal '() (missing-from text '("#+TBLFM:" "$3=" "@4$2=" "@2$2..@3$3" "@<" "@>" "@I" "@#" "$#" "::" "Calc"
                                       "C-c *" "C-c =" "C-c C-c")))))

(define-test (describe-settings capture-placeholders-are-all-described)
  (let* ((text (described "describe-capture-template-settings"))
         (letters (let ((found '()))
                    (dolist (name '("org-capture-escape" "org-capture-question"))
                      (cl-ppcre:do-register-groups (char) ("#\\\\([?%UTutaigt<^{])"
                                                       (source-definition "lisp/org/capture.lisp" name))
                        (pushnew char found :test #'string=)))
                    found))
         (fields (let ((found '()))
                   (cl-ppcre:do-register-groups (name) ("\\((?:field|yes) \"([a-z-]+)\""
                                                        (source-definition "lisp/org/capture.lisp"
                                                                           "read-capture-template"))
                     (pushnew name found :test #'string=))
                   found)))
    (true (and letters fields))
    (is equal '() (set-difference (list "?" "%" "U" "T" "u" "t" "a" "i" "<" "^" "{" "g") letters :test #'string=))
    (dolist (letter (remove-if (lambda (c) (member c '("^" "{" "g") :test #'string=)) letters))
      (true (some (lambda (entry) (search (format nil "%~a" letter) (first entry)))
                  tuatara.defaults::*org-capture-escapes*)
            "the placeholder %~a is read and not listed" letter))
    (true (search "%^{Prompt}" text))
    (is equal '() (set-difference fields (mapcar #'first tuatara.defaults::*org-capture-head-fields*) :test #'string=)
        "head fields read and not listed")
    (is equal '() (set-difference (mapcar #'first tuatara.defaults::*org-capture-head-fields*) fields :test #'string=)
        "head fields listed and not read")
    (is equal '() (missing-from text (mapcar #'first tuatara.defaults::*org-capture-escapes*)))))

(define-test (describe-settings timestamps-are-all-described)
  (let* ((text (described "describe-timestamp-settings"))
         (forms (let ((found '()))
                  (cl-ppcre:do-register-groups (name) ("\\(string= name \"([a-z-]+)\""
                                                       (source-definition "lisp/org/dates.lisp" "org-rule-from-data"))
                    (pushnew name found :test #'string=))
                  (append found
                          (quoted-strings (cl-ppcre:scan-to-strings "\\(member name '\\([^)]*\\)"
                                                                    (source-definition "lisp/org/dates.lisp"
                                                                                       "org-rule-from-data"))))))
         (planning (cl-ppcre:all-matches-as-strings "[A-Z]{6,9}" tuatara.defaults::*org-planning-re*))
         (units (coerce (aref (nth-value 1 (cl-ppcre:scan-to-strings "\\[([hdwmy]+)\\]"
                                                                     (tuatara.defaults::org-timestamp-body)))
                              0)
                        'list)))
    (true (and forms planning units))
    (is equal '() (sort (set-difference forms (mapcar #'first tuatara.defaults::*org-rule-forms*) :test #'string=)
                        #'string<)
        "rule forms read and not listed")
    (is equal '() (missing-from text (mapcar #'first tuatara.defaults::*org-rule-forms*)))
    (is equal '() (missing-from text planning) "planning words")
    (is equal (sort (copy-list units) #'char<) (sort (mapcar #'first tuatara.defaults::*org-timestamp-units*) #'char<)
        "units read and units described")
    (is equal '() (missing-from text '("<2026" "[2026" "+1w" "++1w" ".+1w" "-3d" "--3d" "09:00-10:00" "%%(" "CANCELLED"
                                       "MOVED" "C-c ." "C-c !" "C-c C-s" "C-c C-d")))))
