;;;; tools/manual-tables.lisp --- The manual's reference tables, from the program itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:manual-tables
  (:use #:cl)
  (:export #:run))

(in-package #:manual-tables)

(defparameter *most-keys* 3
  "The most key sequences a command's row names.")

(defun modal-keymaps ()
  "The keymaps of the modal layer: the NORMAL, INSERT and support states' and
the keys a major mode has in those states. They work only with *modal-editing* on."
  (append (list tuatara.defaults::*tuatara-normal-keymap* tuatara.defaults::*tuatara-insert-keymap*
                tuatara.defaults::*tuatara-support-keymap*)
          (loop for (nil . state) being the hash-keys of tuatara.defaults::*mode-layers* using (hash-value map)
                when (member state '(:normal :insert :support))
                  collect map)))

(defun non-modal-keymaps ()
  "The keymaps that work with modal editing on and off: the global one and each
major mode's."
  (cons tuatara.defaults::*global-keymap*
        (loop for mode being the hash-keys of tuatara.modes::*modes*
              collect (tuatara.modes:mode-keymap mode))))

(defun plain-backslashes (string)
  "STRING with each word naming a backslash command (\\p{Lu}, [\\s\\S]*?) set as
code, its last punctuation left outside, so that a docstring saying one is
read as those characters and not as LaTeX; a word holding = stays as it is."
  (cl-ppcre:regex-replace-all "(\\S*\\\\[A-Za-z]\\S*?)([.,;:!?)]*(?:\\s|$))" string
                              (lambda (match word end)
                                (if (find #\= word)
                                    match
                                    (format nil "=~a=~a" word end)))
                              :simple-calls t))

(defun cell (string)
  "STRING made fit for a cell of an Org table."
  (let ((one-line (substitute #\Space #\Newline string)))
    (with-output-to-string (out)
      (loop for char across one-line
            do (if (char= char #\|) (write-string "\\vert{}" out) (write-char char out))))))

(defun sentence (doc)
  "The first sentence of DOC, a docstring, with the keys it names written in, plain."
  (let ((words (and doc (tuatara.command:substitute-command-keys doc))))
    (cell (plain-backslashes (remove-if (lambda (char) (< (char-code char) 32))
                                        (tuatara.command::first-sentence words))))))

(defun key-cells (keys)
  "KEYS, key sequence strings, as one table cell: the shortest first, each in the
Org markup for a key, at most *MOST-KEYS* of them; \"none\" for no key."
  (let ((shortest (sort (copy-list keys) (lambda (a b) (or (< (length a) (length b))
                                                         (and (= (length a) (length b)) (string< a b)))))))
    (if keys
        (format nil "~{~a~^, ~}"
                (mapcar (lambda (key) (if (find #\~ key) key (format nil "~~~a~~" key)))
                        (subseq shortest 0 (min *most-keys* (length shortest)))))
        "none")))

(defun keys-in (symbol keymaps)
  "The key sequences that run the command SYMBOL in KEYMAPS, each given once."
  (remove-duplicates (loop for map in keymaps
                           append (tuatara.command:where-is symbol (list map)))
                     :test #'string=))

(defun key-columns (symbol)
  "The two key cells of the command SYMBOL's row (MODAL NON-MODAL): the keys of
the modal layer, and the other keys, which work with modal editing off. A command
with no key at all is run by name, and says so in the second."
  (let* ((modal (keys-in symbol (modal-keymaps)))
         (non-modal (set-difference (keys-in symbol (non-modal-keymaps)) modal :test #'string=)))
    (list (key-cells modal)
          (if (or modal non-modal)
              (key-cells non-modal)
              (format nil "~~M-x ~(~a~)~~" symbol)))))

(defvar *manual-package* nil
  "The name of the package whose manual is made (\"palimpsest\"), or NIL for
Tuatara's own: the tables then hold that package's commands, settings and
faces alone.")

(defun own-name-p (name)
  "True when NAME, a setting's or face's name without its marks, is the manual's
own: one of Tuatara's, or with *MANUAL-PACKAGE* one beginning with its name."
  (let ((prefix (and *manual-package* (format nil "~:@(~a~)-" *manual-package*))))
    (or (null prefix) (uiop:string-prefix-p prefix (string-upcase name)))))

(defun command-rows ()
  "Rows (NAME (MODAL NON-MODAL) WORDS) for every command offered by name, sorted
by name: Tuatara's own, or with *MANUAL-PACKAGE* that package's."
  (sort (loop for symbol being the hash-keys of tuatara.command::*commands*
              for home = (package-name (symbol-package symbol))
              when (and (if *manual-package*
                            (string= home (string-upcase *manual-package*))
                            (and (uiop:string-prefix-p "TUATARA" home) (string/= home "TUATARA.TESTS")))
                        (not (tuatara.command:key-only-command-p symbol)))
                collect (list (string-downcase symbol)
                              (key-columns symbol)
                              (sentence (documentation symbol 'function))))
        #'string< :key #'first))

(defun mode-switch-row-p (row)
  "True when ROW, a command row, is one of the commands that switch the current
buffer to a major mode."
  (uiop:string-prefix-p "Switch the current buffer to " (third row)))

(defun value-words (symbol)
  "The default value of the setting SYMBOL, short enough for a table."
  (let* ((*print-case* :downcase) (*print-pretty* nil) (*package* (find-package :tuatara-user))
         (words (prin1-to-string (symbol-value symbol))))
    (cell (if (> (length words) 40) (format nil "~a..." (subseq words 0 40)) words))))

(defun setting-rows ()
  "Rows (NAME DEFAULT VALUES WORDS) for every setting, sorted by name: VALUES
the values it takes in words, empty when its definition does not say them."
  (sort (let ((rows '()))
          (do-external-symbols (symbol :tuatara-user)
            (when (and (boundp symbol) (own-name-p (string-trim "*" (symbol-name symbol))))
              (push (list (string-downcase symbol) (value-words symbol)
                          (cell (or (tuatara.runtime:setting-values-words symbol) ""))
                          (sentence (documentation symbol 'variable)))
                    rows)))
          rows)
        #'string< :key #'first))

(defun face-rows ()
  "Rows (NAME WORDS) for every face, sorted by name."
  (sort (loop for name being the hash-keys of tuatara.display:*faces*
              using (hash-value face)
              when (or (null *manual-package*) (own-name-p (symbol-name name)))
                collect (list (format nil ":~(~a~)" name)
                            (sentence (tuatara.display::face-doc face))))
        #'string< :key #'first))

(defun option-rows ()
  "One row for each option on the command line, in the order --help gives them:
how it is written, what it does and, where there is a choice to make, why you
would use it and what it costs, from the list --help and tuatara(1) are made
from."
  (loop for entry in tuatara.terminal:*option-help*
        for text = (format nil "~{~a~^ ~}" (getf entry :text))
        for why = (format nil "~{~a~^ ~}" (remove "" (getf entry :why) :test #'string=))
        collect (list (tuatara.terminal::option-label (getf entry :flags))
                      (cell (format nil "~:(~a~)~a~:[.~;~]~@[ ~a~]" (subseq text 0 1) (subseq text 1)
                                    (uiop:string-suffix-p text ".") (and (plusp (length why)) why))))))

(defun write-table (stream heads rows)
  "Write ROWS, lists of cells, under the column names HEADS to STREAM as an Org
table."
  (format stream "|~{ ~a |~}~%|-~%" heads)
  (dolist (row rows)
    (format stream "|~{ ~a |~}~%" row)))

(defun write-file (path heads rows)
  "Write ROWS under HEADS to the file PATH as an Org table."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-table out heads rows)))

(defun code (words)
  "WORDS in the Org markup for code."
  (format nil "=~a=" words))

(defun package-manual-name (directory)
  "The name of the package whose repository is DIRECTORY: tuatara-palimpsest/
holds palimpsest."
  (let ((name (car (last (pathname-directory directory)))))
    (if (uiop:string-prefix-p "tuatara-" name) (subseq name 8) name)))

(defparameter *kinds*
  '(("commands" "commands" ("Modal" "Non-modal" "Command" "What it does") :keys-first)
    ("settings" "settings" ("Setting" "Default" "Values" "What it changes") :setting)
    ("faces" "faces" ("Face" "What it paints") :face)
    ("all-commands" "commands" ("Modal" "Non-modal" "Command" "What it does") :keys-first)
    ("mode-commands" "commands" ("Modal" "Non-modal" "Command" "What it does") :keys-first)
    ("all-settings" "settings" ("Setting" "Default" "Values" "What it changes") :setting)
    ("options" "options" ("Option" "What it does") :option))
  "The kinds of table a chapter can ask for with \"#+table: KIND NAME ...\", each
(KIND SOURCE COLUMN-HEADS LAYOUT): the rows come from the source table SOURCE.
The kinds WHOLE-TABLE-P accepts take no names and give every row that belongs, in
the order of the source; \"mode-commands\" those that switch to a major mode,
\"all-commands\" the others.")

(defun whole-table-p (kind)
  "True when a table of KIND lists every row that belongs and takes no names."
  (or (uiop:string-prefix-p "all-" kind) (uiop:string-prefix-p "mode-" kind) (string= kind "options")))

(defun cells (layout row)
  "The cells of ROW, a row of a source table, as the table LAYOUT shows it."
  (destructuring-bind (name second &optional third fourth) row
    (ecase layout
      (:keys-first (append second (list (code name) third)))
      (:setting (list (code name) (code second) third fourth))
      ((:face :option) (list (code name) second)))))

(defun kind-rows (kind table)
  "The rows of TABLE, the source table of the whole-table KIND, that KIND takes."
  (cond ((string= kind "mode-commands") (remove-if-not #'mode-switch-row-p table))
        ((string= kind "all-commands") (remove-if #'mode-switch-row-p table))
        (t table)))

(defun table-rows (line from tables)
  "The cells of the table the chapter line LINE, \"#+table: KIND NAME ...\", asks
for, with its column names, as a list (HEADS ROWS); a name TABLES lacks is
reported against the chapter FROM."
  (destructuring-bind (kind . names) (uiop:split-string (subseq line 9) :separator " ")
    (destructuring-bind (source heads layout) (cdr (assoc kind *kinds* :test #'string=))
      (let* ((table (cdr (assoc source tables :test #'string=)))
             (rows (if (whole-table-p kind)
                       (kind-rows kind table)
                       (loop for name in (remove "" names :test #'string=)
                             for row = (find name table :key #'first :test #'string=)
                             unless row do (format t "~&~a: no ~a named ~a~%" (file-namestring from) kind name)
                             when row collect row))))
        (list heads (mapcar (lambda (row) (cells layout row)) rows))))))

(defun outline-chapters (dir)
  "The chapters of the manual in DIR, the numbered list of OUTLINE.org, as a list
of (NUMBER TITLE FILE-NAME); the order of the manual is read from there alone."
  (loop for line in (uiop:read-file-lines (merge-pathnames "OUTLINE.org" dir) :external-format :utf-8)
        for dot = (position #\. line)
        when (and dot (plusp dot) (every #'digit-char-p (subseq line 0 dot)))
          collect (let* ((number (parse-integer line :end dot))
                         (prefix (format nil "~2,'0d-" number))
                         (file (find-if (lambda (file) (uiop:string-prefix-p prefix (file-namestring file)))
                                        (directory (merge-pathnames "*.org" dir)))))
                    (list number (string-trim " " (subseq line (1+ dot)))
                          (and file (file-namestring file))))))

(defun nav-line (previous next)
  "The line of Org links to the chapters PREVIOUS and NEXT, each (NUMBER TITLE
FILE-NAME) or NIL, written at the top and the bottom of a chapter."
  (flet ((link (word chapter)
           (when chapter
             (format nil "~a: [[file:~a][~d. ~a]]" word (third chapter) (first chapter) (second chapter)))))
    (format nil "~{~a~^ | ~}" (remove nil (list (link "Previous" previous) (link "Next" next))))))

(defun expand-chapter (from to tables links)
  "Copy the chapter FROM to TO, each line \"#+table: KIND NAME ...\" replaced by
the rows of TABLES for those names in that order, and the line LINKS, of links to
the chapters on either side, put under the title lines and at the end."
  (ensure-directories-exist to)
  (with-open-file (out to :direction :output :if-exists :supersede :external-format :utf-8)
    (let ((header t))
      (dolist (line (uiop:read-file-lines from :external-format :utf-8))
        (when (and header (not (uiop:string-prefix-p "#+" line)))
          (setf header nil)
          (format out "~%~a~%" links))
        (if (uiop:string-prefix-p "#+table: " line)
            (destructuring-bind (heads rows) (table-rows line from tables)
              (write-table out heads rows))
            (write-line line out)))
      (format out "~%~a~%" links))))

(defun name-tokens (text)
  "The names TEXT mentions: each run of letters, digits and the characters - * :
+, also without its trailing - or :."
  (let ((names '()) (start nil))
    (flet ((close-run (end)
             (when start
               (let ((run (subseq text start end)))
                 (push run names)
                 (push (string-right-trim "-:." run) names))
               (setf start nil))))
      (dotimes (i (length text))
        (if (or (alphanumericp (char text i)) (find (char text i) "-*:+."))
            (unless start (setf start i))
            (close-run i)))
      (close-run (length text)))
    names))

(defun covered-names (chapters)
  "A table of every name the source files CHAPTERS mention, except those of the
chapters that list every name by whole-table kinds, which mention all of them."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (chapter chapters seen)
      (let ((text (uiop:read-file-string chapter :external-format :utf-8)))
        (unless (or (search (format nil "~%#+table: all-") text) (search (format nil "~%#+table: mode-") text))
          (dolist (name (name-tokens text))
            (setf (gethash name seen) t)))))))

(defun report-missing (tables chapters)
  "Say, for each of TABLES, how many names no source file of CHAPTERS mentions,
and which, leaving out the commands that switch to a major mode, only counted."
  (let ((seen (covered-names chapters)))
    (dolist (table tables)
      (let* ((missing (remove-if (lambda (row) (gethash (first row) seen)) (cdr table)))
             (modes (remove-if-not #'mode-switch-row-p missing))
             (others (set-difference missing modes)))
        (format t "~&~a: ~d, ~d in no chapter (~d of them mode switches)~%~{  ~a~%~}"
                (car table) (length (cdr table)) (length missing) (length modes)
                (mapcar #'first (sort others #'string< :key #'first)))))))

(defun run (tree &key package)
  "Generate the tables of the manual in TREE, with the links between its chapters,
and report what no chapter covers. With PACKAGE, the directory of a package's
repository (tuatara-palimpsest/), the manual is that package's, in its
doc/manual/: the package is loaded from there and the tables hold its own
commands, settings and faces."
  ;; the editor started below must not touch this account's own directories, nor sweep its cache
  (tuatara.fence:fence-person-data nil)
  (let* ((*manual-package* (and package (package-manual-name package)))
         (dir (merge-pathnames "doc/manual/" (or package tree)))
         (out (merge-pathnames "generated/" dir))
         ;; Tuatara's settings as they stand before an editor starts; the keymaps are filled in as one does,
         ;; and a package's settings are there once it is loaded
         (settings (and (not package) (setting-rows)))
         (tables (progn
                   (tuatara.command:run-editor (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)
                                               :setup #'tuatara.defaults:install-defaults)
                   (when package
                     (let ((refused (tuatara.packages:load-package-for-test *manual-package* package :tests nil)))
                       (when refused (error "The package ~a was not loaded: ~a" *manual-package* refused))))
                   (list (cons "commands" (command-rows)) (cons "settings" (or settings (setting-rows)))
                         (cons "faces" (face-rows)) (cons "options" (if package '() (option-rows))))))
         (chapters (outline-chapters dir)))
    (dolist (kind '("commands" "settings" "faces"))
      (let ((spec (find kind *kinds* :key #'first :test #'string=)))
        (write-file (merge-pathnames (format nil "~a.org" kind) out) (third spec)
                    (mapcar (lambda (row) (cells (fourth spec) row))
                            (cdr (assoc kind tables :test #'string=))))))
    (loop for (previous chapter next) on (cons nil chapters)
          while chapter
          do (when (third chapter)
               (expand-chapter (merge-pathnames (third chapter) dir) (merge-pathnames (third chapter) out) tables
                               (nav-line previous next))))
    (report-missing (remove "options" tables :key #'car :test #'string=)
                    (mapcar (lambda (chapter) (merge-pathnames (third chapter) dir))
                            (remove nil chapters :key #'third)))))
