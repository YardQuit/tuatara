;;;; lisp/org/latex.lisp --- An Org document written as LaTeX, for a PDF.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; ORG-LATEX-DOCUMENT writes an Org document as a LaTeX document, read by
;;;; ORG-PARSE, the way Org's own LaTeX exporter (ox-latex, Org 9.7) writes it:
;;;; the same preamble, keywords (#+title, #+author, #+date, #+subtitle,
;;;; #+options, #+latex_class, #+latex_class_options, #+latex_header,
;;;; #+export_file_name, #+export_select_tags, #+export_exclude_tags,
;;;; #+setupfile, #+macro, #+toc, #+attr_latex), sectioning, lists, tables,
;;;; blocks, markup, links, footnotes and entities, so that a document and its
;;;; setup file give the PDF they give there. Nothing in the document is run:
;;;; code blocks are written as they are, a macro whose value would be
;;;; evaluated is left empty, and only local setup files are read.

(in-package #:tuatara.defaults)

(defvar *org-macro-limit* (* 16 1024 1024)
  "The most characters a note may grow to as its macro calls are put in place
for an export, a whole number; 16 MB when not set. The calls past it are left
as they are written, and that is said.")

(defstruct (org-latex (:constructor make-org-latex (text directory file)) (:copier nil))
  "What writing one Org document as LaTeX knows: TEXT, the document as
parsed; DIRECTORY and FILE, those of its file (NIL for none); KEYWORDS, its
keywords and those of its setup files, each (KEY . VALUE), in order;
OPTIONS, a table of its #+OPTIONS; ENGINE, the LaTeX program it is for;
MIN-LEVEL, the level of its highest headlines; FOOTNOTES, its footnote
definitions by label; LABELS, the label given each node linked to; COUNTER,
for new labels; KEPT, the headlines exported when some carry a select tag
\(NIL: all); MACRO-COUNTERS, those of the n macro; TREE, the document parsed;
MARKED, where in the text each element whose LaTeX is marked is, (BEGIN .
END) (ORG-LATEX-MARK); EXPANSIONS, how each round of macros expanded moved the
text, the last first (ORG-LATEX-ORIGINAL-POSITION); ENHANCED, true for
Tuatara's own improvements on what Org writes; APPENDIX, true once the
annexes have begun (ORG-LATEX-APPENDIX-TAG-P); FONTS, the families chosen
for the template's {{{fonts}}}, (PART . FAMILY); FALLBACKS, each (FAMILY .
COMMAND) a character the fonts lack is set in (ORG-LATEX-FONT-PLAN); NOTES,
what is said of the document after it is made (a picture not shown), the
latest first; SETTINGS, what the :EXPORT: drawers set, by node
\(ORG-LATEX-GATHER-SETTINGS), in the enhanced export; CONSUMED, the nodes
written already as part of another (a paragraph beside a picture); TRUSTED,
the texts of the setup files read from your own folders (EXPORT-TRUSTED-PATH-P);
PATHS, the files the template's {{{NAME:path}}} places were filled with;
REFUSED, the names of the pictures the template asks for that the note names
from where they may not come (TEMPLATE-REFUSED-PICTURES), left out;
PICTURES, the files the note's own pictures are drawn from (ORG-LATEX-IMAGE);
TARGETS, what a link inside the document may lead to, made once
\(ORG-LATEX-LINK-TARGETS); SETUPS, the text of each setup file read, by its
path, NIL for one that could not be (ORG-LATEX-SETUP-TEXT); MACRO-ROOM, how
many more characters its macro calls may add to it, below 0 once they would
have made it longer than *ORG-MACRO-LIMIT* (ORG-LATEX-MACRO-VALUE)."
  text directory file (keywords '()) (options (make-hash-table :test 'equalp)) (engine "xelatex")
  (min-level 1) (footnotes (make-hash-table :test 'equal)) (labels (make-hash-table :test 'eq))
  (counter 0) kept (macro-counters (make-hash-table :test 'equal)) tree
  (marked (make-array 64 :adjustable t :fill-pointer 0)) (expansions '()) enhanced appendix
  (fonts '()) (fallbacks '()) (notes '()) settings (consumed (make-hash-table :test 'eq)) (trusted '()) (paths '()) (refused '()) (pictures '()) targets
  (setups (make-hash-table :test 'equal)) (macro-room *org-macro-limit*))

(setf (documentation 'org-latex-p 'function) "True when OBJECT is an ORG-LATEX.")

(defvar *org-latex* nil
  "The ORG-LATEX of the document being written as LaTeX.")

(defun export-note (format &rest arguments)
  "Say, after the document being exported (*ORG-LATEX*) is made, FORMAT with
ARGUMENTS, once however often it comes; NIL."
  (when *org-latex*
    (pushnew (apply #'format nil format arguments) (org-latex-notes *org-latex*) :test #'string=))
  nil)

(defvar *org-latex-text* ""
  "The text whose nodes are being written as LaTeX: the document's, or a keyword's value.")

(defparameter *org-latex-setup-depth* 5
  "How deep setup files may name further setup files.")

(defparameter *org-latex-image-types* '("png" "jpg" "jpeg" "gif" "pdf" "eps" "tif" "tiff" "bmp")
  "The kinds of file, by their names' endings, a link to which is shown as a picture.")

(defparameter *org-latex-enhanced-image-types* '("svg" "webp" "heic" "heif" "avif" "ico")
  "The kinds of file, by their names' endings, a link to which is shown as a
picture in the enhanced export too, made into PNG (LATEX-PICTURE).")

(defparameter *org-latex-sections*
  '(("article" "section" "subsection" "subsubsection" "paragraph" "subparagraph")
    ("report" "part" "chapter" "section" "subsection" "subsubsection")
    ("book" "part" "chapter" "section" "subsection" "subsubsection"))
  "The sectioning commands of each document class, from the highest level;
a class not named here has the article's.")

;;; Keywords and options

(defvar *export-trusted-folders* '()
  "The folders, besides a note's own folder and the folders in it and your
note templates' folder (NOTE-TEMPLATES-DIRECTORY), that exporting the note
\(to PDF or to a Word document) may take files from: the setup files its
#+SETUPFILE lines name and the pictures its [[file:...]] links show. A list
of folder names, ~ standing for your home folder; NIL for the note's own
folder alone. A file anywhere else is left out of the export,
and the export says so when it is done, so that a note given to you cannot
have its export carry your other files into the PDF or document it makes.
A folder of setup files shared by notes kept in folders of their own goes
here: '(\"~/notes/latex/\").")

(defun org-export-path (name &optional (directory (and *org-latex* (org-latex-directory *org-latex*))))
  "NAME, a file a note names, as a native path: ~ standing for your home
folder, a relative name taken from DIRECTORY, the folder of the note being
exported when not given, your home folder for a note with no file."
  (tuatara.io:native (tuatara.io:expand-path name :directory (or directory (tuatara.io:home-directory)))))

(defun org-export-folder-p (path)
  "True when PATH (native) stands where the export of the document *ORG-LATEX*
is writing may take files from: the document's folder or a folder in it,
your note templates' folder (NOTE-TEMPLATES-DIRECTORY, where the pictures of
a template made from a Word document are), or one of *EXPORT-TRUSTED-FOLDERS*,
links followed (ORG-PATH-BESIDE-P)."
  (let ((directory (and *org-latex* (org-latex-directory *org-latex*))))
    (or (and directory (org-path-beside-p path directory))
        (path-in-folders-p path (cons (note-templates-directory) *export-trusted-folders*)))))

(defun org-export-file-allowed-p (path)
  "True when exporting the document *ORG-LATEX* is writing may take in the
file at PATH (native), it standing where exports take files from
\(ORG-EXPORT-FOLDER-P). When it may not, the export says so (ORG-LATEX-NOTES)
and NIL is returned; a file that does not exist may be named, there being
nothing to take in."
  (or (null (ignore-errors (tuatara.io:path-stamp path))) ; nothing there to take in
      (org-export-folder-p path)
      (export-note "~a: outside the note's folder, so left out of the export (*export-trusted-folders* names the folders it may also come from)"
                   (abbreviated-path path))))

(defvar *logos-directory* nil
  "The folder of the pictures your templates ask a note for, the logo its
#+logo: names (LOGOS-DIRECTORY); NIL, as standard, for logos/ in the
configuration directory.")

(defun logos-directory ()
  "Where the pictures your templates ask a note for are kept (the logo its
#+logo: names): *LOGOS-DIRECTORY*, or logos/ in the configuration directory
\(~/.config/tuatara/logos/), ending in /."
  (let ((folder (if *logos-directory*
                    (tuatara.io:native (tuatara.io:expand-path *logos-directory*))
                    (config-path "logos/"))))
    (tuatara.io:slash-ended folder)))

(defun template-picture-path (value)
  "The file of the picture a template asks the note for with {{{NAME:path}}},
VALUE what the note's keyword says: a name alone (logo.png) in your logos
folder (LOGOS-DIRECTORY), else a path, read as the note's links are; NIL,
said after the export, for one that is not there, or stands elsewhere than
the logos folder and the folders an export takes files from
\(ORG-EXPORT-FOLDER-P), so that a note given to you cannot have your
template take in a picture of yours."
  (let* ((name (string-trim " " value))
         (path (if (or (find #\/ name) (uiop:string-prefix-p "~" name))
                   (org-export-path name)
                   (concatenate 'string (logos-directory) name))))
    (cond ((not (or (path-in-folders-p path (list (logos-directory))) (org-export-folder-p path)))
           (export-note "~a: a picture a template asks for comes from ~a or the note's own folder, so left out"
                        (abbreviated-path path) (abbreviated-path (logos-directory))))
          ((not (tuatara.io:path-stamp path))
           (export-note "~a: no such picture, so left out" (abbreviated-path path)))
          (t path))))

(defun export-trusted-path-p (path)
  "True when the file at PATH (native) is your own to an export: in your
LaTeX templates' folder or one of *EXPORT-TRUSTED-FOLDERS*, links followed;
not merely beside the note, which may have come with it."
  (path-in-folders-p path (cons (latex-templates-directory) *export-trusted-folders*)))

(defun org-latex-read-setup (value directory)
  "The text of the setup file VALUE names, relative to DIRECTORY, with its
directory as a second value; NIL for one on the network, not readable, or
outside the folders an export may take files from (ORG-EXPORT-FILE-ALLOWED-P)."
  (let ((name (string-trim "\" " value)))
    (unless (or (zerop (length name)) (search "://" name))
      (let ((path (ignore-errors (org-export-path name directory))))
        (when (and path (eq :regular (tuatara.io:path-kind path))
                   (org-export-file-allowed-p path))
          (let ((text (org-latex-setup-text path)))
            (and text (values text (tuatara.io:parent-path path) path))))))))

(defun org-latex-setup-text (path)
  "The text of the setup file at PATH (native), read once for the document
being exported: NIL when it cannot be read, or when it is larger than
*ORG-FILE-READ-LIMIT* octets, which is said."
  (let ((setups (and *org-latex* (org-latex-setups *org-latex*))))
    (multiple-value-bind (text known) (if setups (gethash path setups) (values nil nil))
      (if known
          text
          (let ((text (ignore-errors
                       (tuatara.io:file-text path :limit *org-file-read-limit*
                                                  :too-large (lambda (name size)
                                                               (declare (ignore name size))
                                                               (export-note "~a: a setup file larger than ~a, so not read"
                                                                            (abbreviated-path path) (octets-words *org-file-read-limit*))
                                                               nil)))))
            (when (and text *org-latex* (export-trusted-path-p path))
              (push text (org-latex-trusted *org-latex*)))
            (when setups (setf (gethash path setups) text))
            text)))))

(defun org-latex-collect-keywords (tree directory &optional (depth 0) (seen '()))
  "The keywords of TREE, a parsed document whose file is in DIRECTORY, each
(KEY . VALUE) in order, KEY in capitals; a #+SETUPFILE's in its place."
  (let ((found '()))
    (org-walk-tree tree (lambda (node)
                           (when (eq (org-node-type node) 'keyword)
                             (let ((key (org-property node :key)) (value (or (org-property node :value) "")))
                               (if (string-equal key "SETUPFILE")
                                   (when (< depth *org-latex-setup-depth*)
                                     (multiple-value-bind (text setup-directory path) (org-latex-read-setup value directory)
                                       (when (and text (not (member path seen :test #'equal)))
                                         (setf found (revappend (org-latex-collect-keywords (org-parse text) setup-directory (1+ depth)
                                                                                    (cons path seen))
                                                                found)))))
                                   (push (cons (string-upcase key) value) found))))))
    (nreverse found)))

(defun org-latex-keyword (key &optional (latex *org-latex*))
  "The value of the last #+KEY in the document, or NIL."
  (cdr (find key (org-latex-keywords latex) :key #'car :test #'string= :from-end t)))

(defun org-latex-keyword-all (key &optional (latex *org-latex*))
  "The values of every #+KEY in the document, in order."
  (loop for (k . v) in (org-latex-keywords latex) when (string= k key) collect v))

(defun org-latex-read-options (latex)
  "Fill LATEX's table of options from the document's #+OPTIONS lines, anew."
  (clrhash (org-latex-options latex))
  (dolist (line (org-latex-keyword-all "OPTIONS" latex))
    (cl-ppcre:do-register-groups (key value) ("(?:^|\\s)([^\\s:]+):(\\S+)" line)
      (setf (gethash key (org-latex-options latex)) value))))

(defun org-latex-option (key default)
  "The value of the option KEY (#+OPTIONS: KEY:VALUE): T, NIL, a number, or
the word as written; DEFAULT when not given."
  (multiple-value-bind (value found) (gethash key (org-latex-options *org-latex*))
    (if (not found)
        default
        (cond ((string-equal value "t") t)
              ((string-equal value "nil") nil)
              ((every #'digit-char-p value) (parse-integer value))
              (t value)))))

(defun org-latex-attributes (node key)
  "NODE's #+ATTR_KEY: line as a plist of keywords and the text after each;
a setting no export uses (ORG-KNOWN-KEYWORD) is left out."
  (let ((line (getf (org-property node :affiliated) (org-known-keyword (format nil "ATTR_~a" key)))))
    (when line
      (loop for (setting . value) in (org-keyword-arguments line)
            for name = (org-known-keyword setting)
            when name
              append (list name value)))))

;;; Macros

(defvar *org-latex-expanding* '()
  "The footnotes and macro calls being written, each (KIND . NAME): one met
again within itself (a footnote that refers to itself, macros that call each
other) is written as it is written, not again, never without end.")

(defmacro with-org-latex-expanding ((kind name) (&body again) &body body)
  "Run BODY with KIND's NAME among *ORG-LATEX-EXPANDING*, or AGAIN when it is
there already: being written within itself."
  (let ((key (gensym "KEY")))
    `(let ((,key (cons ,kind ,name)))
       (if (member ,key *org-latex-expanding* :test #'equal)
           (progn ,@again)
           (let ((*org-latex-expanding* (cons ,key *org-latex-expanding*)))
             ,@body)))))

(defun org-latex-macro-value (node)
  "What the macro call NODE stands for, as Org text, or NIL for a macro not
known, and NIL for every call once those put in place for the document
would have made it longer than *ORG-MACRO-LIMIT* characters. In the
enhanced export redact and protect are always Tuatara's own
\(ORG-LATEX-REDACTION); elsewhere a call of them with no #+MACRO: line for it
is an error, never its text printed."
  (let ((room (if *org-latex* (org-latex-macro-room *org-latex*) *org-macro-limit*)))
    (unless (minusp room)
      (let ((value (%org-latex-macro-text node room))
            (growth (- (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))))
        (when (stringp value) (incf growth (length value)))
        (cond ((null value) nil)
              ((or (eq value :too-long) (> growth room))
               (when *org-latex*
                 (setf (org-latex-macro-room *org-latex*) -1)
                 (export-note "Macro calls would make the note longer than ~:d characters, so those past that are left as written (*org-macro-limit*)"
                              *org-macro-limit*))
               nil)
              (t (when *org-latex* (decf (org-latex-macro-room *org-latex*) growth))
                 value))))))

(defun %org-latex-macro-text (node left)
  "What the macro call NODE stands for, as ORG-LATEX-MACRO-VALUE gives it, or
:TOO-LONG when a macro of your own would put more than LEFT characters of
its arguments in."
  (let ((key (org-property node :key))
        (args (mapcar (lambda (a) (cl-ppcre:regex-replace-all "\\\\," a ",")) (org-property node :args))))
    (flet ((joined (key) (format nil "~{~a~^ ~}" (org-latex-keyword-all key))))
      (cond ((and (org-hiding-kind key) (org-latex-enhanced *org-latex*))
             ;; the whole of what is inside, commas and all
             (org-latex-redaction (org-hiding-kind key) (format nil "~{~a~^, ~}" args)))
            ((string= key "title") (joined "TITLE"))
            ((member key '("author" "date" "email") :test #'string=) (or (org-latex-keyword (string-upcase key)) ""))
            ((string= key "keyword") (or (org-latex-keyword (string-upcase (or (first args) ""))) ""))
            ((string= key "input-file") (if (org-latex-file *org-latex*) (tuatara.io:native-file-name (org-latex-file *org-latex*)) ""))
            ((member key '("time" "modification-time") :test #'string=)
             (format-time (or (first args) "%Y-%m-%d") (get-universal-time)))
            ((string= key "n")
             (let* ((name (or (first args) ""))
                    (counters (org-latex-macro-counters *org-latex*)))
               (cond ((equal (second args) "-") (princ-to-string (gethash name counters 0)))
                     ((and (second args) (every #'digit-char-p (second args)) (plusp (length (second args))))
                      (princ-to-string (setf (gethash name counters) (parse-integer (second args)))))
                     (t (princ-to-string (incf (gethash name counters 0)))))))
            (t
             (let ((definition (assoc key (org-macro-definitions) :test #'string-equal)))
               (when (and (null definition) (org-hiding-kind key))
                 (error "{{{~a(...)}}} hides text only in the enhanced export (M-x org-export-latex-pdf-enhanced), ~
                         or with a #+macro: ~a line of your own" key key))
               (when definition
                 (let ((body (cdr definition)))
                   ;; a value that is code is never run
                   (if (uiop:string-prefix-p "(eval" body)
                       ""
                       (let ((value (cl-ppcre:regex-replace-all "\\$([0-9])" body
                                                                (lambda (match n)
                                                                  (declare (ignore match))
                                                                  ;; $1 the first argument; $0 none
                                                                  (let* ((k (parse-integer n))
                                                                         (argument (or (and (plusp k) (nth (1- k) args)) "")))
                                                                    (if (minusp (decf left (length argument))) "" argument)))
                                                                :simple-calls t)))
                         (if (minusp left) :too-long value)))))))))))

(defun org-latex-redacting-p (kind)
  "True when the text KIND marks, redact or protect, is hidden: redact unless
#+OPTIONS: redact:nil, protect when #+OPTIONS: protect:t."
  (and (org-latex-option kind (string= kind "redact")) t))

(defvar *org-latex-in-columns* nil
  "True while what is written is set in columns (multicols), where LaTeX's
tables across pages (longtable) cannot be: a table is then kept whole.")

(defun org-keyword-lines (text)
  "The lines of the Org TEXT that begin with #+, alone: enough to read its
keywords, the lines that begin and end its blocks still telling them from
what a block holds, the rest never parsed."
  (with-output-to-string (out)
    (cl-ppcre:do-matches-as-strings (line "(?m)^[ \\t]*#\\+.*$" text)
      (write-line line out))))

(defmacro with-org-note-keywords ((text directory &optional file) &body body)
  "Run BODY with the keywords of the Org note TEXT read as its export reads
them, those of its #+SETUPFILE: in their place (ORG-LATEX-KEYWORD), DIRECTORY
the note's folder for setup files named relative to it and FILE its file.
Only its #+ lines are read, so that a long note is looked at in an instant."
  (let ((note (gensym)) (folder (gensym)))
    `(let* ((,note ,text) (,folder ,directory)
            (*org-latex* (make-org-latex ,note ,folder ,file)))
       (setf (org-latex-keywords *org-latex*) (org-latex-collect-keywords (org-parse (org-keyword-lines ,note)) ,folder))
       ,@body)))

(defun org-hidden-kinds (text directory)
  "Which of redact and protect the Org note TEXT hides, as its export decides
(ORG-LATEX-REDACTING-P): its #+OPTIONS: read with those of its #+SETUPFILE:,
DIRECTORY the note's folder for setup files named relative to it."
  (with-org-note-keywords (text directory)
    (org-latex-read-options *org-latex*)
    (remove-if-not #'org-latex-redacting-p '("redact" "protect"))))

(defun org-macro-definitions (&optional (latex *org-latex*))
  "The macros the document LATEX defines with #+MACRO: lines, its setup
files' too, as (NAME . BODY), each name once with its last definition, in
the order of their first."
  (let ((definitions '()))
    (dolist (line (org-latex-keyword-all "MACRO" latex))
      (let ((name (first (tuatara.text:split-words line))))
        (when name
          (let ((body (string-trim " 	" (subseq line (+ (search name line) (length name)))))
                (known (assoc name definitions :test #'string-equal)))
            (if known (setf (cdr known) body) (push (cons name body) definitions))))))
    (nreverse definitions)))

(defparameter *redacted-words* "[redacted]"
  "What stands for hidden text where only plain words can be: a PDF's
bookmarks and facts, a Word document's contents, links and properties.")

(defparameter *org-redaction-scanner*
  (cl-ppcre:create-scanner "(?:@@latex:\\\\texorpdfstring\\{\\\\redactedbox\\{n*\\}\\}\\{[^}]*\\}@@)?@@docx:redacted:(\\d+)@@")
  "Hidden text as ORG-LATEX-REDACTION writes it, how many characters it hid a
group; the Word document's part alone too.")

(defun org-latex-redaction (kind text)
  "What {{{KIND(TEXT)}}} stands for, KIND redact or protect: TEXT as it is
when it is shown (ORG-LATEX-REDACTING-P), else a black box as wide as TEXT
would be: for LaTeX made from as many letters n, [redacted] in its
bookmarks, and for a Word document as many characters (@@docx:redacted:N@@),
so that TEXT is in neither, nor in what is made from them. What is made so
is read back by *ORG-REDACTION-SCANNER*."
  (if (org-latex-redacting-p kind)
      (format nil "@@latex:\\texorpdfstring{\\redactedbox{~a}}{~a}@@@@docx:redacted:~d@@"
              (make-string (max 1 (length text)) :initial-element #\n) *redacted-words* (max 1 (length text)))
      text))

(defun org-latex-plain (string)
  "STRING, the value of a keyword, as plain text for the PDF's facts: the
text redacted or protected in it, when it is hidden, as [redacted]."
  (cl-ppcre:regex-replace-all *org-hiding-call-re* string
                              (lambda (match name inside)
                                (declare (ignore match))
                                (if (and (org-latex-enhanced *org-latex*) (org-latex-redacting-p (org-hiding-kind name))) *redacted-words* inside))
                              :simple-calls t))

(defun org-latex-expand-macros (text)
  "TEXT with the macro calls it holds replaced by what they stand for, again
while what they stand for holds more, at most eight times: a call Org's
syntax reads as one (never in a block), a macro not known left as it is,
and the calls that would make TEXT longer than *ORG-MACRO-LIMIT* characters
left as they are."
  (setf (org-latex-macro-room *org-latex*) (- *org-macro-limit* (length text)))
  (loop repeat 8
        until (minusp (org-latex-macro-room *org-latex*))
        do (let ((tree (org-parse text)) (calls '()))
             (setf (org-latex-keywords *org-latex*) (org-latex-collect-keywords tree (org-latex-directory *org-latex*)))
             ;; the options before the calls: they say what redact and protect hide
             (org-latex-read-options *org-latex*)
             (org-walk-tree tree (lambda (node)
                                    (when (eq (org-node-type node) 'macro)
                                      (let ((value (org-latex-macro-value node)))
                                        (when value (push (list node value) calls))))))
             (when (null calls) (return text))
             ;; where each call's text lands after the round, for errors to be told at the text as written
             (let ((shift 0) (moves '()))
               (dolist (call (sort (copy-list calls) #'< :key (lambda (c) (org-node-begin (first c)))))
                 (destructuring-bind (node value) call
                   (let* ((begin (org-node-begin node))
                          (end (- (org-node-end node) (org-node-post-blank node)))
                          (after (+ begin shift)))
                     (incf shift (- (length value) (- end begin)))
                     (push (list after (+ after (length value)) begin shift) moves))))
               (push (nreverse moves) (org-latex-expansions *org-latex*)))
             ;; the text written once, each call's value in its place
             (setf text (with-output-to-string (out)
                          (let ((at 0))
                            (dolist (call (sort calls #'< :key (lambda (c) (org-node-begin (first c)))))
                              (destructuring-bind (node value) call
                                (write-string text out :start at :end (org-node-begin node))
                                (write-string value out)
                                (setf at (- (org-node-end node) (org-node-post-blank node)))))
                            (write-string text out :start at)))))
        finally (return text)))

(defun org-latex-original-position (position)
  "Where POSITION, in the text with its macros expanded, was in the text as
written: the start of the call when it is inside what a call stood for."
  (dolist (moves (org-latex-expansions *org-latex*) position)
    (let ((last (find-if (lambda (move) (<= (first move) position)) moves :from-end t)))
      (when last
        (destructuring-bind (after after-end begin shift) last
          (declare (ignore after))
          (setf position (if (< position after-end) begin (- position shift))))))))

;;; Text

(defun latex-escape (string &key (special-strings t) (newlines nil))
  "STRING with the characters LaTeX gives meanings to written so they print
as themselves; with SPECIAL-STRINGS, ... as an ellipsis, \\- as a place to
hyphenate, and the words LaTeX and TeX as their logos; with NEWLINES, each
line break kept as one. A zero-width space, which in a note only keeps the
text beside it from being read as markup, is left out."
  (with-output-to-string (out)
    (loop with n = (length string)
          for i from 0 below n
          for c = (char string i)
          do (cond ((and special-strings (char= c #\\) (< (1+ i) n) (char= (char string (1+ i)) #\-))
                    (write-string "\\-" out) (incf i))
                   ((and special-strings (char= c #\L) (<= (+ i 5) n) (string= "LaTeX" string :start2 i :end2 (+ i 5)))
                    (write-string "\\LaTeX{}" out) (incf i 4))
                   ((and special-strings (char= c #\T) (<= (+ i 3) n) (string= "TeX" string :start2 i :end2 (+ i 3)))
                    (write-string "\\TeX{}" out) (incf i 2))
                   ((and special-strings (char= c #\.) (< (+ i 2) n) (string= "..." string :start2 i :end2 (+ i 3)))
                    (write-string "\\ldots{}" out) (incf i 2))
                   ((char= c #\\) (write-string "\\textbackslash{}" out))
                   ((char= c #\ZERO_WIDTH_SPACE))
                   ((char= c #\~) (write-string "\\textasciitilde{}" out))
                   ((char= c #\^) (write-string "\\textasciicircum{}" out))
                   ((find c "{}$%#&_") (write-char #\\ out) (write-char c out))
                   ((and newlines (char= c #\Newline)) (write-string "\\\\\\relax" out) (write-char c out))
                   (t (write-char c out))))))

(defparameter *latex-code-break-after* "-/._:=,;|)@"
  "The characters after which code in a line of text may go on to the next
line in the enhanced export, when a letter, a digit, \\, ( or [ follows and
three characters at least stay on each side, so that a key such as C-x is
kept whole; a number such as 10.25 and a repeat such as {1,2} are kept
whole too.")

(defparameter *latex-code-run-limit* 16
  "The most characters of code in a line of text the enhanced export keeps
together where nothing else lets it go on to the next line: a long digest,
hexadecimal number or address goes on after that many, so that no table
column is made as wide as the longest of them.")

(defun latex-code-break-p (string i &optional (run 0))
  "True when the code STRING may go on to the next line after its character
at I: after one of *LATEX-CODE-BREAK-AFTER*, or anywhere once RUN, the
characters since the last place it could, reaches *LATEX-CODE-RUN-LIMIT*;
three characters at least kept on each side."
  (let ((c (char string i))
        (next (and (< (1+ i) (length string)) (char string (1+ i))))
        (blank (lambda (c) (member c '(#\Space #\Tab #\Newline)))))
    (and next (not (funcall blank next)) (not (funcall blank c))
         (or (and (find c *latex-code-break-after*) (or (alphanumericp next) (find next "\\(["))
                  (not (and (digit-char-p next) (plusp i) (digit-char-p (char string (1- i))))))
             (>= run *latex-code-run-limit*))
         (let ((start (let ((b (position-if blank string :end i :from-end t))) (if b (1+ b) 0)))
               (end (or (position-if blank string :start i) (length string))))
           (and (>= (- (1+ i) start) 3) (>= (- end (1+ i)) 3))))))

(defun latex-texttt (string)
  "STRING in a typewriter face, printed as it is; in the enhanced export, a
long name, path or pattern may go on to the next line after a - / . _ : =
, ; | ) or @, and a long digest or number after every *LATEX-CODE-RUN-LIMIT*
characters, rather than run into the margin, the next line beginning with
a small arrow, as a code block's lines do when they go on, so that the
reader takes the two parts as one (\\codebreak, LATEX-CODE-BREAK-P). The
arrow is drawn but not copied: text copied from the PDF has none."
  (if (and *org-latex* (org-latex-enhanced *org-latex*))
      (with-output-to-string (out)
        (write-string "\\texttt{" out)
        (loop with start = 0 and run = 0
              for i from 0 below (length string)
              do (if (member (char string i) '(#\Space #\Tab #\Newline)) (setf run 0) (incf run))
              when (latex-code-break-p string i run)
                do (write-string (latex-escape (subseq string start (1+ i)) :special-strings nil) out)
                   (write-string "\\codebreak{}" out)
                   (setf start (1+ i) run 0)
              finally (write-string (latex-escape (subseq string start) :special-strings nil) out))
        (write-string "}" out))
      (format nil "\\texttt{~a}" (latex-escape string :special-strings nil))))

(defun latex-url (url)
  "URL written for \\href and \\url."
  (with-output-to-string (out)
    (loop for c across url
          do (if (find c "%#") (progn (write-char #\\ out) (write-char c out)) (write-char c out)))))

(defun latex-label-name (string)
  "STRING made fit for a LaTeX label."
  (map 'string (lambda (c) (if (or (alphanumericp c) (find c ":-_.")) c #\-)) string))

(defun org-latex-new-label (prefix)
  "A label not given before, beginning with PREFIX."
  (format nil "~a~d" prefix (incf (org-latex-counter *org-latex*))))

;;; Headlines

(defun org-latex-label (node)
  "The label of NODE, a headline or an element with a name, given once."
  (or (gethash node (org-latex-labels *org-latex*))
      (setf (gethash node (org-latex-labels *org-latex*))
            (let ((custom (and (eq (org-node-type node) 'headline) (org-headline-property node "CUSTOM_ID"))))
              (if custom (latex-label-name custom) (org-latex-new-label (if (eq (org-node-type node) 'headline) "sec:" "org")))))))

(defun org-latex-tag-list (setting default)
  "The tags of the keyword SETTING (EXPORT_EXCLUDE_TAGS...), or DEFAULT when it is not given."
  (let ((values (org-latex-keyword-all setting)))
    (if values (mapcan #'tuatara.text:split-words values) default)))

(defun org-latex-skipped-p (headline)
  "True when HEADLINE and what is under it are left out: commented, a tag of
#+EXPORT_EXCLUDE_TAGS (noexport), or not among those kept by
#+EXPORT_SELECT_TAGS (export) when some headline has one."
  (let ((tags (org-property headline :tags)))
    (or (org-property headline :commentedp)
        (intersection tags (org-latex-tag-list "EXPORT_EXCLUDE_TAGS" '("noexport")) :test #'string=)
        (and (org-property headline :archivedp) (not (eq (org-latex-option "arch" 'headline) t)))
        (let ((kept (org-latex-kept *org-latex*)))
          (and kept (not (gethash headline kept)))))))

(defun org-latex-find-kept (tree)
  "The headlines exported when some carry a select tag: those, what is above
them and what is under them, as a table; NIL when none carries one."
  (let ((select (org-latex-tag-list "EXPORT_SELECT_TAGS" '("export")))
        (kept (make-hash-table :test 'eq))
        (any nil))
    (org-walk-tree tree (lambda (node)
                           (when (and (eq (org-node-type node) 'headline)
                                      (intersection (org-property node :tags) select :test #'string=))
                             (setf any t)
                             (org-walk-tree node (lambda (n) (when (eq (org-node-type n) 'headline) (setf (gethash n kept) t))))
                             (loop for up = (org-node-parent node) then (org-node-parent up)
                                   while up do (setf (gethash up kept) t)))))
    (and any kept)))

(defun org-latex-section-command (level)
  "The sectioning command for a headline of LEVEL (from 1, after the
highest), or NIL when it is written as a list item instead (past H)."
  (let* ((class (or (org-latex-keyword "LATEX_CLASS") "article"))
         (commands (rest (or (assoc class *org-latex-sections* :test #'string-equal) (first *org-latex-sections*))))
         (h (org-latex-option "H" 3)))
    (and (<= level (if (integerp h) h 3)) (nth (1- level) commands))))

(defun org-latex-appendix-tag-p (tag)
  "True when TAG begins the annexes in the enhanced export: appendix, in any case."
  (and (org-latex-enhanced *org-latex*) (string-equal tag "appendix")))

(defun org-latex-headline-title (headline out)
  "Write HEADLINE's title, with its TODO keyword and tags as the options say, to OUT."
  (let ((todo (org-property headline :todo-keyword)))
    (when (and todo (org-latex-option "todo" t))
      (format out "\\textbf{~a} " (latex-escape todo))))
  (let ((priority (org-property headline :priority)))
    (when (and priority (org-latex-option "pri" nil))
      (format out "\\framebox{\\#~c} " (code-char priority))))
  (org-latex-objects-between (org-property headline :title) (org-property headline :title-begin)
                             (org-property headline :title-end) out)
  (let ((tags (remove-if #'org-latex-appendix-tag-p (org-property headline :tags))))
    (when (and tags (org-latex-option "tags" t))
      (format out "\\hfill{}\\textsc{~{~a~^:~}}" (mapcar #'latex-escape tags)))))

(defun org-latex-numbered-p (headline)
  "True when HEADLINE is printed with a number: as #+OPTIONS: num says, but
not with :UNNUMBERED: t, nor numbered: no in its :EXPORT: drawer."
  (and (org-latex-option "num" t) (not (org-headline-property headline "UNNUMBERED"))
       (not (export-setting-no-p headline "numbered"))))

(defun org-latex-headline (headline out)
  "Write HEADLINE and what is under it to OUT: a section, or past the
deepest level sections go to (H), an item of a list; out of the contents
when its :EXPORT: drawer says contents: no."
  (unless (org-latex-skipped-p headline)
    (let* ((level (1+ (- (org-property headline :level) (org-latex-min-level *org-latex*))))
           (command (org-latex-section-command level))
           (title (with-output-to-string (s) (org-latex-headline-title headline s)))
           (numbered (org-latex-numbered-p headline))
           (listed (not (export-setting-no-p headline "contents")))
           ;; what is under it, in the columns and type its :EXPORT: drawer gives it
           (contents (with-output-to-string (s)
                       (org-latex-with-settings headline s
                                                (lambda (s) (dolist (child (org-node-children headline)) (org-latex-element child s)))))))
      ;; the first headline tagged appendix, or set annex: yes, begins the annexes, on a page of
      ;; their own, the rest as the template makes them
      (when (and (not (org-latex-appendix *org-latex*))
                 (or (some #'org-latex-appendix-tag-p (org-property headline :tags))
                     (string-equal (org-latex-setting headline "annex") "yes")))
        (setf (org-latex-appendix *org-latex*) t)
        (format out "\\clearpage~%\\appendix~%"))
      (write-string (org-latex-mark headline) out)
      (if command
          (progn
            (if (and numbered (not listed))
                ;; numbered, but its line in the contents never written
                (format out "{\\expandafter\\let\\expandafter\\addcontentsline\\csname @gobblethree\\endcsname~%\\~a{~a}~%\\label{~a}}~%"
                        command title (org-latex-label headline))
                (progn
                  (format out "\\~a~:[*~;~]{~a}~%" command numbered title)
                  (when (and (not numbered) listed (org-latex-option "toc" t))
                    (format out "\\addcontentsline{toc}{~a}{~a}~%" command title))
                  (format out "\\label{~a}~%" (org-latex-label headline))))
            (write-string contents out))
          ;; past H: a list, one for the headlines side by side
          (let* ((siblings (remove-if-not (lambda (n) (and (eq (org-node-type n) 'headline) (not (org-latex-skipped-p n))))
                                          (org-node-children (org-node-parent headline))))
                 (environment (if (org-latex-option "num" t) "enumerate" "itemize")))
            (when (eq headline (first siblings)) (format out "\\begin{~a}~%" environment))
            (format out "\\item ~a~%\\label{~a}~%~a" title (org-latex-label headline) contents)
            (when (eq headline (car (last siblings))) (format out "\\end{~a}~%" environment)))))))

;;; Elements

(defun org-latex-elements (node out)
  "Write the elements in NODE to OUT."
  (dolist (child (org-node-children node))
    (org-latex-element child out)))

(defun org-latex-caption (node)
  "NODE's #+CAPTION as LaTeX, or NIL."
  (let ((caption (getf (org-property node :affiliated) :caption)))
    (and caption (org-latex-string caption))))

(defun org-latex-name-label (node)
  "\\label{...} for NODE when it has a #+NAME, else an empty string."
  (if (org-property node :name) (format nil "\\label{~a}" (org-latex-label node)) ""))

(defun latex-verbatim-escape (string &key (tab-width 8))
  "STRING for a Verbatim environment that reads commands (commandchars \\, {
and }): those three as the characters they are, tabs as spaces to the next
stop of TAB-WIDTH, and a place the line may break after every
*LATEX-CODE-RUN-LIMIT* characters with no space or mark of punctuation
\(a break the Verbatim environment makes of its own) among them, the rest as
it is."
  (with-output-to-string (out)
    (let ((column 0) (run 0))
      (loop for c across string
            do (case c
                 (#\\ (write-string "\\char92{}" out) (incf column))
                 (#\{ (write-string "\\char123{}" out) (incf column))
                 (#\} (write-string "\\char125{}" out) (incf column))
                 (#\Tab (let ((spaces (- tab-width (mod column tab-width))))
                          (dotimes (i spaces) (write-char #\Space out))
                          (incf column spaces)))
                 (#\Newline (write-char c out) (setf column 0))
                 (t (write-char c out) (incf column)))
               (if (or (find c '(#\Space #\Tab #\Newline)) (find c *latex-code-break-after*))
                   (setf run 0)
                   (when (>= (incf run) *latex-code-run-limit*)
                     (write-string "\\discretionary{}{}{}" out)
                     (setf run 0)))))))

(defun org-latex-code-faces (code name)
  "The face of each character of CODE read as the language NAME is (as a
source block names it, emacs-lisp, sh...), as Tuatara colours it: a vector,
NIL where no face is; NIL when the language is not known or not coloured."
  (let ((language (and name (plusp (length name)) (org-src-language name))))
    (when language
      (multiple-value-bind (style level) (language-colouring language)
        (unless (eq style :none)
          ;; its rules made here, quietly, on whatever thread exports
          (let ((*rules-quiet* t))
            (language-compiled-query language :colouring) (language-compiled-query language :injection) (language-compiled-query language :locals)
            (let* ((rope (tuatara.text:make-rope code))
                   (bytes (tuatara.text:rope-bytes rope))
                   (faces (make-array (length code) :initial-element nil)))
              ;; outer ranges first: the innermost face of a character is its last
              (loop for (start end face) in (ignore-errors
                                             (tuatara.syntax:language-highlights (injection-resolver style level)
                                                                                 (symbol-name (language-name language))
                                                                                 (list (cons 0 bytes)) rope 0 bytes))
                    do (loop for i from (tuatara.text:byte->char rope start) below (min (length code) (tuatara.text:byte->char rope end))
                             do (setf (aref faces i) face)))
              faces)))))))

(defun org-latex-face-look (face cache)
  "How FACE prints in the enhanced export, from *LIGHT-THEME* (the page is
paper): a list of its colour as RRGGBB or NIL, bold and italic; NIL when it
prints as plain text. CACHE, a hash table, keeps what was worked out."
  (multiple-value-bind (look found) (gethash face cache)
    (if found
        look
        (setf (gethash face cache)
              (let* ((theme tuatara-user:*light-theme*)
                     (hex (tuatara.display:colour-hex (tuatara.display:face-attribute-in-theme face :fg theme)))
                     (bold (tuatara.display:face-attribute-in-theme face :bold theme))
                     (italic (tuatara.display:face-attribute-in-theme face :italic theme)))
                (and (or hex bold italic) (list (and hex (string-upcase (subseq hex 1))) bold italic)))))))

(defun org-latex-code-lines (code faces)
  "CODE for a Verbatim environment that reads commands, each run of one face
of FACES (ORG-LATEX-CODE-FACES) begun by switches to its colour, bold and
italic (only those that change, each line beginning plain), so that the text
stays where its lines can be broken anywhere."
  (let ((cache (make-hash-table :test 'eq))
        (plain '(nil nil nil)))
    (with-output-to-string (out)
      (loop with n = (length code)
            with i = 0
            with now = plain
            while (< i n)
            do (let* ((face (and faces (aref faces i)))
                      (end (loop for j from i below n
                                 when (or (char= (char code j) #\Newline) (not (eq (and faces (aref faces j)) face)))
                                   return j
                                 finally (return n))))
                 (if (= end i)
                     ;; the line's end: the next begins plain
                     (progn (write-char #\Newline out) (setf now plain) (incf i))
                     (destructuring-bind (colour bold italic) (or (and face (org-latex-face-look face cache)) plain)
                       (destructuring-bind (was-colour was-bold was-italic) now
                         (unless (equal colour was-colour)
                           (if colour (format out "\\color[HTML]{~a}" colour) (write-string "\\normalcolor{}" out)))
                         (unless (eq (and bold t) (and was-bold t))
                           (write-string (if bold "\\bfseries{}" "\\mdseries{}") out))
                         (unless (eq (and italic t) (and was-italic t))
                           (write-string (if italic "\\itshape{}" "\\upshape{}") out)))
                       (setf now (list colour bold italic))
                       (write-string (latex-verbatim-escape (subseq code i end)) out)
                       (setf i end))))))))

(defun org-latex-verbatim (value out &key caption node plain)
  "Write VALUE as it is, in a verbatim environment, to OUT, in a figure with
CAPTION when there is one; nothing when VALUE is blank. In the enhanced
export its long lines are broken where they meet the margin (at a space,
or after / - . , ; : =, or in a run with none, as a long digest, after every
*LATEX-CODE-RUN-LIMIT* characters; the rest of the line is indented one em,
with a mark where it breaks that text copied from the PDF leaves out), the
code of a source block NODE coloured as Tuatara colours its language
\(ORG-LATEX-CODE-FACES), and NODE's #+ATTR_LATEX :columns N sets it in N
columns, :font in that type (\\small); with PLAIN, those two are not read."
  (let* ((value (string-right-trim '(#\Newline) value))
         (enhanced (org-latex-enhanced *org-latex*))
         (attributes (and node enhanced (not plain) (org-latex-attributes node "latex")))
         (columns (let ((c (getf attributes :columns))) (and c (ignore-errors (parse-integer c)))))
         (font (let ((f (getf attributes :font))) (and f (plusp (length f)) f))))
    (when (or (and columns (> columns 1)) font)
      (let ((inner (with-output-to-string (s) (org-latex-verbatim value s :caption caption :node node :plain t))))
        (format out "~@[{~a~%~]~:[~*~a~;\\begin{multicols}{~d}~%~a\\end{multicols}~%~]~:[~;}~%~]"
                font (and columns (> columns 1)) columns inner font)
        (return-from org-latex-verbatim)))
    (let ((block (cond (enhanced
                        (format nil "\\begin{Verbatim}[breaklines,breakautoindent=false,breakindent=1em,breakafter=/-.\\,;:=,commandchars=\\\\\\{\\}]~%~a~%\\end{Verbatim}~%"
                                (org-latex-code-lines value (and node (eq (org-node-type node) 'src-block)
                                                                 (org-latex-code-faces value (org-property node :language))))))
                       (t (format nil "\\begin{verbatim}~%~a~%\\end{verbatim}~%" value)))))
      (cond
        ((zerop (length (string-trim '(#\Space #\Tab #\Newline) value))) nil)
        (caption
         (format out "\\begin{figure}[H]~%\\caption{~a}~a~%~a\\end{figure}~%" caption (org-latex-name-label node) block))
        (t (write-string block out))))))

(defparameter *org-latex-mark-open* (code-char #xE000)
  "The character that opens a mark in LaTeX being written (ORG-LATEX-MARK): one
for private use, never in text written by this program.")

(defparameter *org-latex-mark-close* (code-char #xE001)
  "The character that closes a mark (ORG-LATEX-MARK).")

(defun org-latex-mark (node)
  "A mark saying that the LaTeX after it comes from NODE, taken out again
when the document is done (ORG-LATEX-UNMARK)."
  (let ((marked (org-latex-marked *org-latex*)))
    (vector-push-extend (cons (org-node-begin node) (max (org-node-begin node) (1- (- (org-node-end node) (org-node-post-blank node)))))
                        marked)
    (format nil "~c~d~c" *org-latex-mark-open* (1- (fill-pointer marked)) *org-latex-mark-close*)))

(defun org-latex-unmark (latex)
  "LATEX with its marks taken out, and as a second value which place in the
text as written each of its lines comes from: a vector of (LINE BEGIN END),
LINE from 1, in order, for the lines where an element's LaTeX begins, BEGIN
and END where the element is in the text."
  (let ((map (make-array 64 :adjustable t :fill-pointer 0))
        (line 1)
        (marked (org-latex-marked *org-latex*)))
    (values
     (with-output-to-string (out)
       (loop with n = (length latex)
             with i = 0
             while (< i n)
             do (let ((c (char latex i)))
                  (cond ((char= c *org-latex-mark-open*)
                         (let* ((close (position *org-latex-mark-close* latex :start i))
                                (index (parse-integer latex :start (1+ i) :end close))
                                (place (list line (org-latex-original-position (car (aref marked index)))
                                             (org-latex-original-position (cdr (aref marked index))))))
                           ;; the last element begun on a line, the innermost, speaks for it
                           (if (and (plusp (fill-pointer map)) (= line (first (aref map (1- (fill-pointer map))))))
                               (setf (aref map (1- (fill-pointer map))) place)
                               (vector-push-extend place map))
                           (setf i (1+ close))))
                        (t (when (char= c #\Newline) (incf line))
                           (write-char c out)
                           (incf i))))))
     (coerce map 'simple-vector))))

(defun org-latex-visible-start (string)
  "The first character of STRING that is not part of a mark, or NIL."
  (loop with i = 0
        while (< i (length string))
        do (if (char= (char string i) *org-latex-mark-open*)
               (setf i (1+ (position *org-latex-mark-close* string :start i)))
               (return (char string i)))))

(defun org-latex-element (node out)
  "Write the element NODE to OUT, marked as coming from it (ORG-LATEX-MARK)
when it is printed, in what its :EXPORT: drawer says of any element
\(ORG-LATEX-WITH-SETTINGS), a new page and space before and after it as it
says, then the blank lines after it; nothing for one written already as
part of another."
  (let ((type (org-node-type node)))
    (unless (gethash node (org-latex-consumed *org-latex*))
      (when (org-latex-printed-p type)
        (write-string (org-latex-mark node) out))
      (call-with-export-breaks
       node out
       (lambda (out)
         (org-latex-with-page node out
                              (lambda (out)
                                (if (eq type 'headline)
                                    (org-latex-element-itself node type out)
                                    (org-latex-with-settings node out (lambda (s) (org-latex-element-itself node type s)))))))
       :length #'org-latex-length
       :page (lambda (out) (format out "~&\\clearpage~%"))
       :space (lambda (out length) (format out "~&\\par\\vspace{~a}~%" length)))
      (when (and (org-latex-printed-p type) (plusp (org-node-post-blank node)))
        (terpri out)))))

(defun org-latex-element-itself (node type out)
  "Write the element NODE, of TYPE, to OUT, as its kind is written."
  (case type
    ((org-data section dynamic-block) (org-latex-elements node out))
    (headline (org-latex-headline node out))
    (paragraph (org-latex-paragraph node out))
    (plain-list (org-latex-plain-list node out))
    (table (org-latex-table node out))
    (src-block (org-latex-verbatim (org-block-code node) out
                                   :caption (org-latex-caption node) :node node))
    ((example-block fixed-width) (org-latex-verbatim (or (org-property node :value) "") out
                                                     :caption (org-latex-caption node) :node node))
    (export-block (when (member (org-property node :type) '("LATEX" "TEX") :test #'string=)
                    (write-string (org-property node :value) out)))
    (latex-environment (write-string (org-property node :value) out))
    (keyword (org-latex-keyword-element node out))
    (horizontal-rule (format out "\\noindent\\rule{\\linewidth}{0.5pt}~%"))
    (quote-block (format out "\\begin{quote}~%") (org-latex-elements node out) (format out "\\end{quote}~%"))
    (center-block (format out "\\begin{center}~%") (org-latex-elements node out) (format out "\\end{center}~%"))
    (special-block (let ((name (org-property node :type)))
                     (format out "\\begin{~a}~a~%" name (org-latex-name-label node))
                     (org-latex-elements node out)
                     (format out "\\end{~a}~%" name)))
    (verse-block (format out "\\begin{verse}~%")
                 (write-string (org-latex-objects-text node :newlines t) out)
                 (format out "~%\\end{verse}~%"))
    (drawer (let ((drawers (org-latex-option "d" t)))
              ;; an :EXPORT: drawer holds settings, never text
              (unless (or (null drawers) (member (org-property node :drawer-name) '("LOGBOOK" "EXPORT") :test #'string-equal))
                (org-latex-elements node out))))
    (table-row nil)
    ;; what is never printed: properties, planning, clocks, comments, calls, footnotes' definitions (at their references)
    (t nil)))

(defun org-latex-printed-p (type)
  "True when an element of TYPE is printed, as it is (a headline is printed by
its own writer), so that it is marked and the blank lines after it are kept."
  (not (member type '(org-data section headline property-drawer node-property planning clock comment comment-block
                      babel-call diary-sexp footnote-definition))))

(defun org-latex-keyword-element (node out)
  "Write the keyword line NODE to OUT: #+LATEX: lines as they are, #+TOC: as a table of contents."
  (let ((key (org-property node :key)) (value (or (org-property node :value) "")))
    (cond ((string-equal key "LATEX") (format out "~a~%" value))
          ((string-equal key "TOC")
           (let ((words (tuatara.text:split-words value)))
             (when (string-equal (first words) "headlines")
               (when (and (second words) (every #'digit-char-p (second words)))
                 (format out "\\setcounter{tocdepth}{~a}~%" (second words)))
               (format out "\\tableofcontents~%"))
             (when (string-equal (first words) "tables") (format out "\\listoftables~%"))
             (when (string-equal (first words) "figures") (format out "\\listoffigures~%")))))))

(defun org-latex-picture-alone-p (paragraph)
  "True when PARAGRAPH holds a picture alone, a link to one with nothing else
but white space around it."
  (let ((children (org-node-children paragraph)))
    (and (= 1 (length children)) (org-latex-image-link-p (first children))
         (string= "" (string-trim '(#\Space #\Tab #\Newline)
                                  (concatenate 'string
                                               (subseq *org-latex-text* (org-node-contents-begin paragraph) (org-node-begin (first children)))
                                               (subseq *org-latex-text* (org-node-end (first children)) (org-node-contents-end paragraph))))))))

(defun org-latex-paragraph-after (node)
  "The paragraph right after NODE among its parent's children, or NIL."
  (let ((next (second (member node (org-node-children (org-node-parent node))))))
    (and next (eq (org-node-type next) 'paragraph) next)))

(defun org-latex-image-link-p (node)
  "True when NODE is a link to a picture, with no description, shown as the picture."
  (and (eq (org-node-type node) 'link)
       (string= (org-property node :type) "file")
       (null (org-node-contents-begin node))
       (member (tuatara.io:native-file-type (org-property node :path))
               (if (org-latex-enhanced *org-latex*)
                   (append *org-latex-image-types* *org-latex-enhanced-image-types*)
                   *org-latex-image-types*)
               :test #'string-equal)))

(defun org-latex-image (node attributes)
  "\\includegraphics for the picture NODE links to, as ATTRIBUTES (#+ATTR_LATEX) size it;
its name in a box instead when it cannot be shown, is not there (which the
export then says), or is outside the folders an export may take files from
\(ORG-EXPORT-FILE-ALLOWED-P)."
  (let* ((path (org-property node :path))
         (full (or (ignore-errors (org-export-path path))
                   path))
         (width (getf attributes :width))
         (height (getf attributes :height))
         (options (format nil "~{~a~^,~}" (remove nil (list (and width (plusp (length width)) (format nil "width=~a" width))
                                                             (and height (plusp (length height)) (format nil "height=~a" height))
                                                             (let ((o (getf attributes :options))) (and o (plusp (length o)) o)))))))
    (flet ((not-shown ()
             (return-from org-latex-image
               (format nil "\\fbox{\\texttt{~a}}" (latex-escape (tuatara.io:native-file-name full) :special-strings nil)))))
      (unless (org-export-file-allowed-p full)
        (not-shown))
      ;; one not there, or not to be read, said and boxed: the LaTeX program would stop at it
      (unless (org-latex-enhanced *org-latex*)
        (let ((why (nth-value 2 (file-kind full))))
          (when why
            (export-note "~a cannot be read: ~a" (tuatara.io:native-file-name full) why)
            (not-shown))))
      ;; in the enhanced export, a picture the LaTeX programs cannot take made into one they can
      (when (org-latex-enhanced *org-latex*)
        (multiple-value-bind (picture why) (latex-picture full)
          (if picture
              (setf full picture)
              (progn (export-note "~a" why)
                     (not-shown)))))
      (pushnew full (org-latex-pictures *org-latex*) :test #'string=))
    (format nil "\\includegraphics[~a]{~a}" (if (and (null width) (null height) (zerop (length options)))
                                                 "width=.9\\linewidth"
                                                 options)
            full)))

(defun org-latex-paragraph (node out)
  "Write the paragraph NODE to OUT: a picture alone in it centred, or in a
figure when it has a caption; in the enhanced export as wide, placed,
against a margin and beside the paragraph after it as its :EXPORT: drawer
says (width, place, position, beside). Text is set as its align says."
  (let* ((children (org-node-children node))
         (alone (org-latex-picture-alone-p node)))
    (if alone
        (let* ((attributes (picture-attributes node :latex t))
               (beside (org-latex-setting node "beside"))
               (text (and beside (org-latex-paragraph-after node)))
               (position (export-position node))
               (caption (org-latex-caption node)))
          (cond (text
                 ;; side by side: the picture as wide as its half, its caption under it, the paragraph after it beside it
                 (setf (gethash text (org-latex-consumed *org-latex*)) t)
                 (let ((picture (format nil "\\begin{minipage}[c]{0.47\\linewidth}~%\\centering~%~a~%~@[\\captionof{figure}{~a}~a~%~]\\end{minipage}~%"
                                        (org-latex-image (first children) (list* :width "\\linewidth" attributes))
                                        caption (if caption (org-latex-name-label node) "")))
                       (words (format nil "\\begin{minipage}[c]{0.47\\linewidth}~%~a~a~%\\end{minipage}~%"
                                      (org-latex-mark text) (org-latex-objects-text text :newlines (org-latex-option "\\n" nil)))))
                   (format out "\\par\\noindent~%~a\\hfill~%~a\\par~%"
                           (if (string-equal beside "right") words picture) (if (string-equal beside "right") picture words))))
                ((and caption position)
                 ;; against a margin: the picture and its caption together, the caption under the picture
                 (format out "\\begin{figure}[~a]~%~a~%\\sbox0{~a}%~%\\begin{minipage}{\\wd0}~%\\usebox0\\par~%\\caption{~a}~a~%\\end{minipage}~%\\end{figure}~%"
                         (or (getf attributes :placement) "htbp")
                         (if (eq position :left) "\\raggedright" "\\raggedleft")
                         (org-latex-image (first children) attributes) caption (org-latex-name-label node)))
                ((or caption (getf attributes :float))
                 (format out "\\begin{figure}[~a]~%~a~%~a~%~@[\\caption{~a}~]~a~%\\end{figure}~%"
                         (or (getf attributes :placement) "htbp")
                         (case position (:left "\\raggedright") (:right "\\raggedleft") (t "\\centering"))
                         (org-latex-image (first children) attributes) caption (org-latex-name-label node)))
                (t (let ((environment (case position (:left "flushleft") (:right "flushright") (t "center"))))
                     (format out "\\begin{~a}~%~a~%\\end{~a}~%" environment (org-latex-image (first children) attributes) environment)))))
        (let ((text (org-latex-objects-text node :newlines (org-latex-option "\\n" nil))))
          (case (export-paragraph-align node)
            (:left (format out "{\\raggedright~%~a\\par}~%" text))
            (:right (format out "{\\raggedleft~%~a\\par}~%" text))
            (:centre (format out "{\\centering~%~a\\par}~%" text))
            (t (format out "~a~%" text)))))))

(defun org-latex-roman (n)
  "N, from 1 to 4, in small roman numerals, as LaTeX names its list counters (enumii)."
  (nth (1- n) '("i" "ii" "iii" "iv")))

(defun org-latex-enumerate-label (bullet)
  "How a numbered list whose first BULLET is as written labels its items, a
FORMAT string of the counter's name (\\alph{~a}) for a, \\Alph for A,
\\arabic for numbers, with the bullet's . or ); NIL for 1. as LaTeX has it."
  (let* ((bullet (string-trim " 	" (or bullet "")))
         (mark (and (plusp (length bullet)) (char bullet (1- (length bullet)))))
         (first (and (plusp (length bullet)) (char bullet 0))))
    (cond ((null first) nil)
          ((lower-case-p first) (format nil "\\alph{~~a}~c" mark))
          ((upper-case-p first) (format nil "\\Alph{~~a}~c" mark))
          ((eql mark #\)) "\\arabic{~a})")
          (t nil))))

(defun org-latex-plain-list (node out)
  "Write the plain list NODE to OUT, as itemize, enumerate or description;
in the enhanced export as the style its :EXPORT: drawer gives says (bullets,
none, numbers, letters, Letters, roman, Roman)."
  (let* ((style (org-latex-setting node "style"))
         (numbered (cdr (assoc style '(("numbers" . "\\arabic{~a}.") ("letters" . "\\alph{~a})") ("Letters" . "\\Alph{~a}.")
                                       ("roman" . "\\roman{~a}.") ("Roman" . "\\Roman{~a}."))
                               :test #'equal)))
         (environment (cond ((eq (org-property node :type) 'descriptive) "description")
                            (numbered "enumerate")
                            ((member style '("bullets" "none") :test #'string-equal) "itemize")
                            ((eq (org-property node :type) 'ordered) "enumerate")
                            (t "itemize")))
         (depth (count 'plain-list (loop for up = (org-node-parent node) then (org-node-parent up) while up collect (org-node-type up))
                       :test #'eq))
         (level (org-latex-roman (1+ (min depth 3)))))
    (format out "\\begin{~a}~%" environment)
    (cond (numbered
           (format out "\\renewcommand{\\labelenum~a}{~a}~%" level (format nil numbered (format nil "enum~a" level))))
          ((and (string-equal style "none") (string= environment "itemize"))
           (format out "\\renewcommand{\\labelitem~a}{}~%" level)))
    ;; in the enhanced export a numbered list keeps its kind of number: a) A. 1)
    (when (and (org-latex-enhanced *org-latex*) (string= environment "enumerate") (not numbered)
               (eq (org-property node :type) 'ordered))
      (let ((label (org-latex-enumerate-label (org-property (first (org-node-children node)) :bullet))))
        (when label
          (format out "\\renewcommand{\\labelenum~a}{~a}~%" (org-latex-roman (1+ (min depth 3)))
                  (format nil label (format nil "enum~a" (org-latex-roman (1+ (min depth 3)))))))))
    (dolist (item (org-node-children node))
      (when (eq (org-node-type item) 'item)
        (let ((counter (org-property item :counter))
              (checkbox (case (org-property item :checkbox)
                          (on "$\\boxtimes$") (off "$\\square$") (trans "$\\boxminus$")))
              (tag (org-property item :tag))
              (contents (string-trim '(#\Newline #\Space) (with-output-to-string (s) (org-latex-elements item s)))))
          (when (and (integerp counter) (string= environment "enumerate"))
            (format out "\\setcounter{enum~a}{~d}~%" (case depth (0 "i") (1 "ii") (2 "iii") (t "iv")) (1- counter)))
          ;; a checkbox in place of the bullet, before the tag of a description
          (let ((label (cond (tag (format nil "~@[~a ~]~a" checkbox (org-latex-string tag))) (checkbox checkbox))))
            (write-string (org-latex-mark item) out)
            (write-string "\\item" out)
            (cond (label (format out "[{~a}] " label))
                  ;; text beginning with [ is not taken for the item's label
                  ((eql #\[ (org-latex-visible-start contents)) (write-string " {}" out))
                  (t (write-char #\Space out)))
            (format out "~a~%" contents)))))
    (format out "\\end{~a}~%" environment)))

;;; Tables

(defun org-latex-cell-text (cell)
  "What CELL holds as written, without the spaces around it."
  (if (org-node-contents-begin cell)
      (string-trim " 	" (subseq *org-latex-text* (org-node-contents-begin cell) (org-node-contents-end cell)))
      ""))

(defun org-latex-cookie-row-p (row)
  "True when ROW holds only column widths and alignments (<l> <r10>...), which are not printed."
  (let ((cells (mapcar #'org-latex-cell-text (org-node-children row))))
    (and (some (lambda (c) (plusp (length c))) cells)
         (every (lambda (c) (or (string= c "") (org-table-cookie-p c))) cells))))

(defun org-latex-special-column-p (rows)
  "True when the first column of ROWS only marks rows for calculations (! # * ^ _ $ or nothing)."
  (let ((firsts (loop for row in rows
                      when (eq (org-property row :type) 'standard)
                        collect (let ((cell (first (org-node-children row)))) (if cell (org-latex-cell-text cell) "")))))
    (and firsts
         (some (lambda (c) (plusp (length c))) firsts)
         (every (lambda (c) (member c '("" "!" "#" "*" "^" "_" "$") :test #'string=)) firsts))))

(defun org-export-table-shape (table)
  "The parts of TABLE an export lays out, as five values: its rows without
the row of widths and alignments, whether its first column only marks rows
for calculations, its rows of cells, its row of widths and alignments or
NIL, and how many columns it prints."
  (let* ((rows (remove-if #'org-latex-cookie-row-p (org-node-children table)))
         (special (org-latex-special-column-p rows))
         (standard (remove 'rule rows :key (lambda (r) (org-property r :type)))))
    (values rows special standard
            (find-if #'org-latex-cookie-row-p (org-node-children table))
            (max 1 (loop for row in standard maximize (- (length (org-node-children row)) (if special 1 0)))))))

(defun org-latex-column-letter (side)
  "The letter of LaTeX's column specification for a column aligned at SIDE
\(:LEFT, :RIGHT or :CENTRE): l, r or c."
  (ecase side (:left "l") (:right "r") (:centre "c")))

(defparameter *org-latex-text-characters* 78
  "About how many characters of the normal size go across the text, for the
enhanced export to tell a table too wide for it.")

(defun org-latex-visible-text (text)
  "About what TEXT, a table cell as written, prints as: a redaction its box,
a link its description, markup without its marks."
  (let ((text (cl-ppcre:regex-replace-all *org-redaction-scanner* text
                                           (lambda (match count)
                                             (declare (ignore match))
                                             (make-string (parse-integer count) :initial-element #\n))
                                           :simple-calls t)))
    (org-plain-text (cl-ppcre:regex-replace-all *org-export-snippet-re* text ""))))

(defun org-latex-visible-length (text)
  "About how many characters TEXT, a table cell as written, prints as
\(ORG-LATEX-VISIBLE-TEXT)."
  (length (org-latex-visible-text text)))

(defun org-latex-longest-piece (text)
  "How many characters the longest part of TEXT, a table cell as written, is
that cannot go on to the next line: a word, or the part of a name or path
between the places LATEX-CODE-BREAK-P allows."
  (let ((text (org-latex-visible-text text))
        (run 0)
        (longest 0))
    (dotimes (i (length text) longest)
      (cond ((member (char text i) '(#\Space #\Tab #\Newline)) (setf run 0))
            (t (setf longest (max longest (incf run)))
               (when (latex-code-break-p text i run) (setf run 0)))))))

(defun org-latex-least-width (piece)
  "How many characters of text a table column is at least as wide as, its
longest part that cannot go on to the next line PIECE characters long: a
character of code is a third wider than one of text, and a cell has room on
each side."
  (+ (ceiling (* 4 piece) 3) 2))

(defun org-latex-table-columns (node columns lengths widest wraps longest fitted table-width special standard cookies lines
                                &optional (characters *org-latex-text-characters*))
  "The column specification of the table NODE in the enhanced export, its
COLUMNS each as its :EXPORT: drawer's widths and align say; else every
column a share of TABLE-WIDTH, as wide as its widest text (WIDEST) is of
theirs, when the table is given a width; else, when FITTED, the columns
with words to wrap (WRAPS) sharing what the others leave in proportion to
their text (LENGTHS, the lengths of each column's cells), each at least as
wide as its longest part that cannot go on to the next line (LONGEST), the
others as wide as their text, CHARACTERS going across the text in the type
the table is printed in. Lines between the columns when LINES
\(TABLE-LINES) has them. SPECIAL, STANDARD and COOKIES are as
ORG-EXPORT-TABLE-SHAPE found them."
  (let ((widths (export-table-widths node columns))
        (aligns (tuatara.text:split-words (org-latex-setting node "align")))
        (shares (and fitted
                     (org-latex-column-shares
                      (loop for i below columns for wrap in wraps
                            when wrap collect (/ (reduce #'+ (nth i lengths)) (max 1 (length (nth i lengths)))))
                      (loop for wrap in wraps for piece in longest when wrap collect (org-latex-least-width piece))
                      (- characters (* 2 (1- columns))
                         (loop for wrap in wraps for w in widest unless wrap sum w))))))
    (flet ((letter (i) (org-latex-column-letter (export-column-alignment node i special standard cookies)))
           (ragged (i) (case (export-side (nth i aligns))
                         (:right "\\raggedleft") (:centre "\\centering") (t "\\raggedright"))))
      (format nil (if (member :columns lines) "~{~a~^ | ~}" "~{~a~^ ~}")
              (cond (widths
                     ;; as the drawer says: a share, a length, * an equal share of what is left, auto as the text is
                     (loop for i below columns
                           for width in widths
                           collect (cond ((eq width :share) (format nil ">{~a\\arraybackslash}X" (ragged i)))
                                         ((or (eq width :auto) (null (org-latex-length width))) (letter i))
                                         (t (format nil ">{~a\\arraybackslash}p{~a}" (ragged i) (org-latex-length width))))))
                    (table-width
                     ;; every column a share of the table's width, as wide as its widest text is of theirs
                     (loop for i below columns
                           collect (format nil ">{~a\\arraybackslash\\hsize=~,3f\\hsize}X" (ragged i)
                                           (/ (* columns (nth i widest)) (max 1 (reduce #'+ widest))))))
                    (t
                     (loop with left = shares
                           for i below columns
                           collect (if (and fitted (nth i wraps))
                                       (format nil ">{~a\\arraybackslash\\hsize=~,3f\\hsize}X" (ragged i) (pop left))
                                       (letter i)))))))))

(defun org-latex-column-shares (weights pieces room)
  "How much of the width the wrapping columns of a table each take, as
\\hsize does: numbers that add up to how many there are, in proportion to
WEIGHTS, yet each column at least as wide as its longest part that cannot
go on to the next line (PIECES), out of ROOM characters for them all."
  (let* ((n (length weights))
         (floors (mapcar (lambda (piece) (/ (* n piece) (max 1 room))) pieces)))
    (if (>= (reduce #'+ floors) n)
        ;; even their unbreakable parts do not fit: each in proportion to those
        (mapcar (lambda (least) (/ (* n least) (reduce #'+ floors))) floors)
        (let ((pinned (make-list n :initial-element nil)))
          ;; the columns whose share would be narrower than their floor take
          ;; the floor; the others share what is left, until none is too narrow
          (loop
            (let* ((free (- n (loop for p in pinned for f in floors when p sum f)))
                   (total (loop for p in pinned for w in weights unless p sum w))
                   (shares (loop for p in pinned for w in weights for f in floors
                                 collect (cond (p f) ((plusp total) (/ (* free w) total)) (t 0))))
                   (narrow (loop for p in pinned for s in shares for f in floors
                                 collect (and (not p) (< s f)))))
              (cond ((every #'identity pinned)
                     (return (mapcar (lambda (least) (/ (* n least) (reduce #'+ floors))) floors)))
                    ((some #'identity narrow)
                     (setf pinned (mapcar (lambda (p q) (or p q)) pinned narrow)))
                    (t (return shares)))))))))

(defun org-latex-table-body (out rows columns special lines repeat caption rule-across)
  "Write ROWS, a table's, to OUT in the enhanced export, each of its COLUMNS
\(SPECIAL true when the first column, only marking rows, is left out), with
the lines LINES (TABLE-LINES) asks for, each rule drawn by RULE-ACROSS, a
function of :TOP, :MIDDLE or :BOTTOM: its header in bold, and with REPEAT
again at the top of each page, under CAPTION continued when it has one."
  (let* ((header (export-table-header rows))
         (top (member :top lines))
         (under-header (and (or (member :header lines) (member :rows lines)) (funcall rule-across :middle))))
    (flet ((row-line (row bold)
             (format out "~a~{~a~^ & ~}\\\\~%" (org-latex-mark row)
                     (org-latex-row-start
                      (loop with cells = (nthcdr (if special 1 0) (org-node-children row))
                            for i below columns
                            for cell = (nth i cells)
                            for text = (if cell (org-latex-objects-text cell) "")
                            collect (if (and bold (plusp (length text))) (format nil "\\textbf{~a}" text) text))))))
      (when top (format out "~a~%" (funcall rule-across :top)))
      (loop for tail on rows
            for row = (car tail)
            for next = (cadr tail)
            for last = (null (cdr tail))
            for index from 0
            do (cond ((eq (org-property row :type) 'rule)
                      (cond ((and (eql index header) repeat caption)
                             ;; the caption once; on the pages after, the header again under the caption continued
                             (format out "~@[~a~%~]\\endfirsthead~%\\caption[]{~a (continued)}\\\\~%~@[~a~%~]"
                                     under-header caption (and top (funcall rule-across :top)))
                             (loop for above in rows
                                   for i below header
                                   unless (eq (org-property above :type) 'rule)
                                     do (row-line above t))
                             (format out "~@[~a~%~]\\endhead~%" under-header))
                            ((eql index header) (format out "~@[~a~%~]~:[~;\\endhead~%~]" under-header repeat))
                            ((or (zerop index) last) nil)
                            ((or (member :rules lines) (member :rows lines)) (format out "~a~%" (funcall rule-across :middle)))))
                     (t (row-line row (and header (< index header)))
                        ;; between every row: under each but the last and those a rule follows
                        (when (and (member :rows lines) next (not (eq (org-property next :type) 'rule)))
                          (format out "~a~%" (funcall rule-across :middle))))))
      (when (member :bottom lines) (format out "~a~%" (funcall rule-across :bottom))))))

(defun org-latex-table-run-limit (columns special standard)
  "The most characters of code in a cell of the table with COLUMNS columns
\(SPECIAL and STANDARD as ORG-EXPORT-TABLE-SHAPE found them) the enhanced
export keeps together (*LATEX-CODE-RUN-LIMIT*): the usual, or fewer, the
first that lets the table, in the smallest type it is set in, go across the
text; the fewest when none does. A second value is the measure of the
columns at that limit (EXPORT-TABLE-MEASURE), as a list."
  (let ((limits (list *latex-code-run-limit* 10 6)))
    (loop for (limit . more) on limits
          do (let* ((*latex-code-run-limit* limit)
                    (measure (multiple-value-list (export-table-measure columns special standard))))
               (destructuring-bind (lengths widest wraps longest) measure
                 (declare (ignore lengths))
                 (when (or (null more)
                           (<= (+ (loop for w in widest for wrap in wraps unless wrap sum w) (* 2 (1- columns))
                                  (loop for wrap in wraps for piece in longest when wrap sum (max 12 (org-latex-least-width piece))))
                               (* 1.25 *org-latex-text-characters*)))
                   (return (values limit measure))))))))

(defun org-latex-fitted-table (node out rows special standard columns cookies &optional measure)
  "Write the table NODE to OUT in the enhanced export, laid out to fit: as
wide as it is when that fits across the text, else as wide as the text, the
columns with words in them wrapping and sharing what the others leave, in
proportion to their text; in smaller type when even that is too wide. It
breaks across pages, the rows above its first rule (its header) in bold,
again at the top of each page. Its :EXPORT: drawer may give each column's
width and alignment, the table's width (full: margin to margin, its columns
sharing it as their text does), its position (left, centre or right),
whether the header repeats, and the lines printed (TABLE-LINES)
\(ORG-LATEX-SETTING). ROWS, SPECIAL, STANDARD, COLUMNS and COOKIES are as
ORG-LATEX-TABLE found them, MEASURE the columns' EXPORT-TABLE-MEASURE as a
list when it has been made."
  (multiple-value-bind (lengths widest wraps longest)
      (values-list (or measure (multiple-value-list (export-table-measure columns special standard))))
    (let* ((natural (+ (reduce #'+ widest) (* 2 (1- columns))))
           ;; the least it can be: the columns with words to wrap no narrower than their longest part kept whole
           (fixed (+ (loop for w in widest for wrap in wraps unless wrap sum w) (* 2 (1- columns))
                     (loop for wrap in wraps for piece in longest when wrap sum (max 12 (org-latex-least-width piece)))))
           (widths (export-table-widths node columns))
           (lines (table-lines node))
           ;; with lines down the table, rules that meet them (booktabs' leave gaps where they cross)
           (upright (intersection lines '(:left :right :columns)))
           (rule-across (lambda (kind) (if upright "\\hline" (ecase kind (:top "\\toprule") (:middle "\\midrule") (:bottom "\\bottomrule")))))
           (font (cond (widths nil)
                       ((or (<= natural *org-latex-text-characters*) (<= fixed *org-latex-text-characters*)) nil)
                       ((<= fixed (* 1.12 *org-latex-text-characters*)) "\\small")
                       (t "\\footnotesize")))
           ;; how many characters go across the text in the type the table is printed in
           (characters (* *org-latex-text-characters* (cond ((equal font "\\small") 1.12) (font 1.25) (t 1))))
           ;; the table's own width (full: margin to margin), its columns sharing it as their text does
           (table-width (let ((w (org-latex-setting node "width")))
                          (and w (if (string-equal w "full") "\\textwidth" (org-latex-length w)))))
           (fitted (cond (widths (and (or table-width (member :share widths)) t))
                         (table-width t)
                         (t (and (> natural (* (if font 1.12 1) *org-latex-text-characters*)) (some #'identity wraps)))))
           (align (org-latex-table-columns node columns lengths widest wraps longest fitted table-width special standard cookies lines
                                           characters))
           ;; in columns a table is kept whole, a float of its own: no header repeated on later pages
           (in-columns *org-latex-in-columns*)
           (position (and (not in-columns)
                          (case (export-position node)
                            (:left "{\\setlength{\\LTleft}{0pt}\\setlength{\\LTright}{\\fill}")
                            (:right "{\\setlength{\\LTleft}{\\fill}\\setlength{\\LTright}{0pt}"))))
           (repeat (and (not in-columns) (not (export-setting-no-p node "repeat header"))))
           ;; rows a little taller between lines that meet, as booktabs' rules leave room
           (font (if upright (format nil "~@[~a~]\\renewcommand{\\arraystretch}{1.2}" font) font))
           (caption (org-latex-caption node))
           (environment (cond (in-columns (if fitted "tabularx" "tabular")) (fitted "xltabular") (t "longtable")))
           (left (if (member :left lines) "|" "@{}"))
           (right (if (member :right lines) "|" "@{}")))
      (if in-columns
          (progn
            (format out "~@[{~a~%~]\\begin{table}[H]~%" font)
            (when caption (format out "\\caption{~a}~a~%" caption (org-latex-name-label node)))
            (format out "\\begin{~a}~:[~*~;{~a}~]{~a~a~a}~%" environment fitted (or table-width "\\linewidth") left align right))
          (progn
            (format out "~@[{~a~%~]~@[~a~%~]\\begin{~a}~:[~*~;{~a}~]{~a~a~a}~%"
                    font position environment fitted (or table-width "\\textwidth") left align right)
            (when caption
              (format out "\\caption{~a}~a\\\\~%" caption (org-latex-name-label node)))))
      (org-latex-table-body out rows columns special lines repeat caption rule-across)
      (format out "\\end{~a}~%~:[~;\\end{table}~%~]~:[~;}~%~]~:[~;}~%~]" environment in-columns position font))))

(defun org-latex-row-start (texts)
  "TEXTS, the cells of a table's row, with an empty group before the first
when it begins with [, which LaTeX would read as a length after the row
before it ends (\\\\[2mm])."
  (let ((first (first texts)))
    (if (and first (plusp (length first)) (char= (char first 0) #\[))
        (cons (concatenate 'string "{}" first) (rest texts))
        texts)))

(defun org-latex-table (node out)
  "Write the table NODE to OUT, laid out as its #+ATTR_LATEX says: its
environment (tabular, tabularx, tabulary, longtable), :width, :align,
:placement, :center, :font and :booktabs, and captioned by #+CAPTION. In the
enhanced export a table with no :environment or :align of its own is laid
out to fit (ORG-LATEX-FITTED-TABLE)."
  (if (not (eq (org-property node :type) 'org))
      (org-latex-verbatim (subseq *org-latex-text* (org-node-post-affiliated node)
                                  (- (org-node-end node) (org-node-post-blank node)))
                          out)
      (multiple-value-bind (rows special standard cookies columns) (org-export-table-shape node)
        (let* ((attributes (org-latex-attributes node "latex"))
               (environment (or (getf attributes :environment) "tabular"))
               (width (getf attributes :width))
               (booktabs (let ((b (getf attributes :booktabs))) (and b (not (string-equal b "nil")))))
               (align (or (getf attributes :align)
                          (format nil "~{~a~}" (loop for column below columns
                                                     collect (org-latex-column-letter (table-column-alignment (+ column (if special 1 0)) standard cookies))))))
               (caption (org-latex-caption node))
               (placement (or (getf attributes :placement) "htbp"))
               (centred (not (equal (getf attributes :center) "nil")))
               (font (getf attributes :font))
               (fit (and (org-latex-enhanced *org-latex*) (not (getf attributes :environment)) (not (getf attributes :align))))
               (body (with-output-to-string (s)
                       (loop for (row . more) on rows
                             for first = t then nil
                             do (if (eq (org-property row :type) 'rule)
                                    (format s "~a~%" (cond ((not booktabs) "\\hline")
                                                           (first "\\toprule")
                                                           ((null more) "\\bottomrule")
                                                           (t "\\midrule")))
                                    (let ((cells (nthcdr (if special 1 0) (org-node-children row))))
                                      (format s "~a~{~a~^ & ~}\\\\~%" (org-latex-mark row)
                                              (org-latex-row-start
                                               (loop for i from 0 below columns
                                                     for cell = (nth i cells)
                                                     collect (if cell (org-latex-objects-text cell) "")))))))))
               (sized (member environment '("tabularx" "tabulary") :test #'string=))
               (table (format nil "\\begin{~a}~:[~*~;{~a}~]{~a}~%~a\\end{~a}~%"
                              environment sized (or width "\\textwidth") align body environment)))
          (cond (fit (multiple-value-bind (limit measure) (org-latex-table-run-limit columns special standard)
                       (let ((*latex-code-run-limit* limit))
                         (org-latex-fitted-table node out rows special standard columns cookies measure))))
                ((string= environment "longtable")
                 (format out "~@[~a~%~]\\begin{longtable}{~a}~%~@[\\caption{~a}~a\\\\~%~]~a\\end{longtable}~%"
                         font align caption (if caption (org-latex-name-label node) "") body))
                ((or caption (getf attributes :float))
                 (format out "\\begin{table}[~a]~%~@[\\caption{~a}~]~a~%~:[~;\\centering~%~]~@[~a~%~]~a\\end{table}~%"
                         placement caption (org-latex-name-label node) centred font table))
                (centred (format out "\\begin{center}~%~@[~a~%~]~a\\end{center}~%" font table))
                (t (format out "~@[{~a~%~]~a~:[~;}~%~]" font table font)))))))

;;; Objects

(defun org-export-objects-between (objects begin end out &key text object blank)
  "Write the note's text from BEGIN below END to OUT, OBJECTS the objects in
it: the text between them as TEXT, a function of a string, gives it, each
object by OBJECT, a function of it and OUT, and the spaces after one as
BLANK, a function of how many, gives them."
  (let ((p begin))
    (dolist (o objects)
      (write-string (funcall text (subseq *org-latex-text* p (org-node-begin o))) out)
      (funcall object o out)
      (when (plusp (org-node-post-blank o))
        (write-string (funcall blank (org-node-post-blank o)) out))
      (setf p (org-node-end o)))
    (when (and p end (< p end))
      (write-string (funcall text (subseq *org-latex-text* p end)) out))))

(defun org-latex-objects-between (objects begin end out &key newlines)
  "Write the text from BEGIN below END to OUT, OBJECTS, the objects in it, as LaTeX and the rest escaped."
  (let ((special (org-latex-option "-" t)))
    (org-export-objects-between objects begin end out
                                :text (lambda (s) (latex-escape s :newlines newlines :special-strings special))
                                :object #'org-latex-object
                                :blank (lambda (n) (make-string n :initial-element #\Space)))))

(defun org-latex-objects-text (node &key newlines)
  "The contents of NODE, text and objects, as LaTeX, without the line break at its end."
  (if (org-node-contents-begin node)
      (string-right-trim '(#\Newline)
                         (with-output-to-string (s)
                           (org-latex-objects-between (org-node-children node) (org-node-contents-begin node)
                                                      (org-node-contents-end node) s :newlines newlines)))
      ""))

(defun org-latex-string (string)
  "STRING, a line of Org text such as a keyword's value, as LaTeX (ORG-PARSE-LINE)."
  (let ((*org-latex-text* string))
    (org-latex-objects-text (org-parse-line string))))

(defun org-latex-link-targets ()
  "What a link inside the document being written may lead to, a table made
once from its tree, the first of each in order: headlines by (:CUSTOM-ID .
ID), (:ID . ID) and (:TITLE . TITLE), targets by (:TARGET . WORDS), elements
named by (:NAME . NAME). What is left out of the export (ORG-LATEX-SKIPPED-P)
is not among them: a link to it is written as its words."
  (or (org-latex-targets *org-latex*)
      (setf (org-latex-targets *org-latex*)
            (let ((table (make-hash-table :test 'equal)))
              (labels ((put (key value n) (let ((key (cons key value))) (unless (gethash key table) (setf (gethash key table) n))))
                       (walk (n)
                         (let ((type (org-node-type n)))
                           (unless (and (eq type 'headline) (org-latex-skipped-p n))
                             (cond ((eq type 'headline)
                                    (put :custom-id (org-headline-property n "CUSTOM_ID") n)
                                    (put :id (org-headline-property n "ID") n)
                                    (put :title (org-property n :raw-value) n))
                                   ((member type '(target radio-target)) (put :target (org-property n :value) n))
                                   ((org-property n :name) (put :name (org-property n :name) n)))
                             (when (eq type 'headline) (mapc #'walk (org-property n :title)))
                             (mapc #'walk (org-node-children n))))))
                (walk (org-latex-tree *org-latex*)))
              table))))

(defun org-latex-link-target (node)
  "The node a link NODE inside the document leads to, or NIL (ORG-LATEX-LINK-TARGETS)."
  (let ((type (org-property node :type)) (path (org-property node :path)) (targets (org-latex-link-targets)))
    (flet ((found (key value) (gethash (cons key value) targets)))
      (cond ((string= type "id") (found :id path))
            ((member type '("custom-id" "fuzzy") :test #'string=)
             (loop for (kind . value) in (org-link-target-kinds (if (string= type "custom-id") (concatenate 'string "#" path) path))
                   thereis (found kind value)))))))

(defun org-latex-link (node out)
  "Write the link NODE to OUT: to the web as \\href, a picture shown, a place
in the document as \\hyperref with its description. One with none shows what
it leads to by its number (\\ref: 3), and in the enhanced export by its kind
and number (\\autoref: section 3, Table 2, Figure 1); one to a heading with no
number by its title, to a target with none by its words. Anything else as its
description."
  (let* ((type (org-property node :type))
         (path (org-property node :path))
         (description (and (org-node-contents-begin node) (org-latex-objects-text node))))
    (cond ((org-latex-image-link-p node)
           (write-string (org-latex-image node (org-latex-attributes (org-node-parent node) "latex")) out))
          ((org-link-address type path)
           (let ((url (org-link-address type path)))
             (if description
                 (format out "\\href{~a}{~a}" (latex-url url) description)
                 (format out "\\url{~a}" (latex-url url)))))
          ((string= type "file")
           ;; a full name as file:///..., one relative to the document as it is
           (let ((target (if (or (uiop:string-prefix-p "/" path) (uiop:string-prefix-p "~" path))
                             (path-uri (tuatara.io:expand-path path))
                             path)))
             (format out "\\href{~a}{~a}" (latex-url target) (or description (latex-escape path)))))
          (t
           (let ((target (org-latex-link-target node)))
             (cond ((and target (null description)
                         (if (eq (org-node-type target) 'headline)
                             (org-latex-numbered-p target)
                             ;; an element has a number when captioned, a target that of where it stands
                             (or (not (org-latex-enhanced *org-latex*)) (org-latex-caption target))))
                    (format out "\\~:[ref~;autoref~]{~a}" (org-latex-enhanced *org-latex*) (org-latex-label target)))
                   ((and target (eq (org-node-type target) 'headline))
                    (format out "\\hyperref[~a]{~a}" (org-latex-label target)
                            (or description (with-output-to-string (s)
                                              (let ((*org-latex-text* *org-latex-text*))
                                                (org-latex-objects-between (org-property target :title) (org-property target :title-begin)
                                                                           (org-property target :title-end) s))))))
                   (target
                    (format out "\\hyperref[~a]{~a}" (org-latex-label target) (or description (latex-escape path))))
                   ;; a note by its identifier, or a link leading nowhere here: its description, else its target
                   ((uiop:string-prefix-p "denote:" path) (write-string (or description (latex-escape (subseq path 7))) out))
                   (t (write-string (or description (latex-escape path)) out))))))))

(defun org-latex-footnote (node out)
  "Write the footnote reference NODE to OUT as \\footnote with its definition."
  (let ((definition (with-org-latex-expanding (:footnote (or (org-property node :label) node)) (nil)
                      (if (eq (org-property node :type) 'inline)
                          (org-latex-objects-text node)
                          (let ((found (gethash (org-property node :label) (org-latex-footnotes *org-latex*))))
                            (and found (string-trim '(#\Newline #\Space)
                                                    (with-output-to-string (s) (org-latex-elements found s)))))))))
    (when (org-latex-option "f" t)
      (format out "\\footnote{~a}" (or definition "")))))

(defun org-latex-object (node out)
  "Write the object NODE to OUT."
  (case (org-node-type node)
    (bold (format out "\\textbf{~a}" (org-latex-objects-text node)))
    (italic (format out "\\emph{~a}" (org-latex-objects-text node)))
    (underline (format out "\\uline{~a}" (org-latex-objects-text node)))
    (strike-through (format out "\\sout{~a}" (org-latex-objects-text node)))
    ((code verbatim) (write-string (latex-texttt (org-property node :value)) out))
    (inline-src-block (write-string (latex-texttt (org-property node :value)) out))
    (entity (let ((entry (gethash (org-property node :name) *org-entities-latex*)))
              (if (and entry (org-latex-option "e" t))
                  (format out (if (cdr entry) "\\(~a\\)" "~a~:[~;{}~]") (car entry)
                          (and (not (cdr entry)) (alpha-char-p (char (car entry) (1- (length (car entry)))))))
                  (write-string (latex-escape (subseq *org-latex-text* (org-node-begin node)
                                                      (- (org-node-end node) (org-node-post-blank node))))
                                out))))
    (latex-fragment (write-string (org-property node :value) out))
    ;; \relax, as a line after the break beginning with [ would be read as a length
    (line-break (format out "\\\\\\relax~%"))
    (export-snippet (when (member (org-property node :back-end) '("latex" "tex" "beamer") :test #'string-equal)
                      (write-string (org-property node :value) out)))
    (footnote-reference (org-latex-footnote node out))
    (link (org-latex-link node out))
    (target (format out "\\label{~a}" (org-latex-label node)))
    (radio-target (format out "\\label{~a}~a" (org-latex-label node) (org-latex-objects-text node)))
    (timestamp (when (org-latex-option "<" t)
                 (format out "\\textit{~a}" (latex-escape (org-property node :raw-value)))))
    (statistics-cookie (write-string (latex-escape (org-property node :value)) out))
    ((subscript superscript)
     (let ((script (org-latex-option "^" (if (org-latex-enhanced *org-latex*) "{}" t)))
           (contents (org-latex-objects-text node)))
       (if (or (null script) (and (equal script "{}") (not (org-property node :use-brackets-p))))
           (write-string (latex-escape (subseq *org-latex-text* (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))) out)
           (format out (if (eq (org-node-type node) 'subscript) "\\textsubscript{~a}" "\\textsuperscript{~a}") contents))))
    ((inline-babel-call) nil)
    ;; a call in a keyword's value (the text's own are expanded before it is read)
    (macro (let ((value (org-latex-macro-value node))
                 (written (subseq *org-latex-text* (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))))
             (if value
                 (with-org-latex-expanding (:macro (org-property node :key)) ((write-string (latex-escape written) out))
                   (write-string (org-latex-string value) out))
                 (write-string (latex-escape written) out))))
    ;; what is left as written: a macro not known, a citation
    (t (write-string (latex-escape (subseq *org-latex-text* (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))) out))))

;;; The document

(defparameter *org-latex-landscape*
  "\\makeatletter
\\newcommand\\tuatara@turn{\\clearpage
  \\setlength{\\@tempdima}{\\paperwidth}\\setlength{\\paperwidth}{\\paperheight}\\setlength{\\paperheight}{\\@tempdima}%
  \\ifdefined\\pdfpagewidth\\pdfpagewidth=\\paperwidth\\pdfpageheight=\\paperheight\\fi
  \\ifdefined\\pagewidth\\pagewidth=\\paperwidth\\pageheight=\\paperheight\\fi
  \\setlength{\\@tempdimb}{\\paperwidth}\\addtolength{\\@tempdimb}{-\\paperheight}%
  \\addtolength{\\textwidth}{\\@tempdimb}\\addtolength{\\textheight}{-\\@tempdimb}%
  \\setlength{\\hsize}{\\textwidth}\\setlength{\\columnwidth}{\\textwidth}\\setlength{\\linewidth}{\\textwidth}%
  \\setlength{\\vsize}{\\textheight}\\setlength{\\@colht}{\\textheight}\\setlength{\\@colroom}{\\textheight}%
  \\ifdefined\\headwidth\\setlength{\\headwidth}{\\textwidth}\\fi}
\\newenvironment{tuataralandscape}{\\tuatara@turn}{\\tuatara@turn}
\\makeatother
"
  "LaTeX for pages turned sideways (a page: landscape setting): the paper's
width and height swapped from a page of their own on and back after, the
text as much wider and shorter, so that headers, footers, columns and tables
lie across the page as a Word document's landscape section has them.")

(defun org-latex-packages (engine)
  "The packages every document loads, for the LaTeX program ENGINE."
  (format nil "~:[\\usepackage[utf8]{inputenc}~%\\usepackage[T1]{fontenc}~%~;~]~
\\usepackage{graphicx}~%\\usepackage{longtable}~%\\usepackage{wrapfig}~%\\usepackage{rotating}~%~
\\usepackage[normalem]{ulem}~%\\usepackage{amsmath}~%\\usepackage{amssymb}~%~:[~;\\usepackage{fontspec}~%~]~
\\usepackage{capt-of}~%\\usepackage{hyperref}~%"
          (member engine '("xelatex" "lualatex") :test #'string-equal)
          (member engine '("xelatex" "lualatex") :test #'string-equal)))

(defun replaced-type (path type)
  "PATH with the ending of its name (after its last dot) replaced by TYPE, or TYPE added when it has none."
  (let ((dot (position #\. path :start (length (tuatara.io:parent-path path)) :from-end t)))
    (concatenate 'string (subseq path 0 (or dot (length path))) "." type)))

(defun org-latex-output-name ()
  "Where the document's #+EXPORT_FILE_NAME puts its LaTeX, a full name ending
in .tex whatever ending it gives (so that an export never writes over a file
of another kind), or NIL when it gives none."
  (let ((name (org-latex-keyword "EXPORT_FILE_NAME")))
    (when (and name (plusp (length (string-trim " " name))))
      (replaced-type (org-export-path (string-trim " " name)) "tex"))))

(defun org-export-file-name (text file)
  "Where the Org note TEXT, of FILE, says its export goes (#+EXPORT_FILE_NAME,
a full name ending in .tex), its #+ lines alone read; NIL when it says none."
  (with-org-note-keywords (text (tuatara.io:parent-path file) file)
    (org-latex-output-name)))

(defun org-latex-title ()
  "The title of the document being written: its #+TITLE: lines joined by
spaces, else its file's name without its ending, else an empty string."
  (let ((title (format nil "~{~a~^ ~}" (org-latex-keyword-all "TITLE")))
        (file (org-latex-file *org-latex*)))
    (if (and (zerop (length title)) file) (tuatara.io:native-file-stem file) title)))

(defun org-latex-author ()
  "The author of the document being written, when #+OPTIONS: says to show one:
its #+AUTHOR:, else your name (ORG-LATEX-USER-NAME); NIL when not shown."
  (and (org-latex-option "author" t) (or (org-latex-keyword "AUTHOR") (user-full-name))))

(defun org-latex-created ()
  "The time the document is being written, as its {{{created}}} place says it."
  (format-time "%Y-%m-%d %a %H:%M" (get-universal-time)))

(defun org-latex-field (body)
  "A function giving the value of each place of a template
\(FILL-LATEX-TEMPLATE) for the document being written, BODY its text as LaTeX:
the document's title, author, date, class, packages, fonts, headers, table
of contents and PDF facts, and any keyword of its own, KEY for #+KEY:, as
Org text written as LaTeX (several lines of it joined by \\newline), as
written (:RAW, joined by line breaks) or as a file's full name (:PATH).
The code font has its ligatures off, so that :: and // in code are copied
from the PDF as they are written."
  (let* ((engine (org-latex-engine *org-latex*))
         (title (org-latex-title))
         (author (org-latex-author))
         (date (and (org-latex-option "date" t) (org-latex-keyword "DATE")))
         (toc (org-latex-option "toc" t))
         (num (org-latex-option "num" t))
         (language (or (org-latex-keyword "LANGUAGE") "en")))
    (lambda (name way)
      (flet ((keyword-values () (org-latex-keyword-all (string-upcase name))))
        (cond
          ((string= name "body") body)
          ((string= name "created") (org-latex-created))
          ((string= name "engine") engine)
          ((string= name "latex_class") (or (org-latex-keyword "LATEX_CLASS") "article"))
          ((string= name "latex_class_options") (or (org-latex-keyword "LATEX_CLASS_OPTIONS") "[11pt]"))
          ((string= name "packages")
           (concatenate 'string (org-latex-packages engine)
                        (if (org-latex-enhanced *org-latex*)
                            (format nil "\\usepackage{booktabs}~%\\usepackage{xltabular}~%\\usepackage{multicol}~%\\usepackage{float}~%~
                                         \\usepackage{xcolor}~%\\usepackage{fvextra}~%\\usepackage{pict2e}~%~a~
                                         \\DeclareRobustCommand{\\redactedbox}[1]{{\\sbox0{#1}\\rule[-0.8ex]{\\wd0}{2.5ex}}}~%~
                                         \\DeclareRobustCommand{\\breakmark}{\\hbox{\\setlength{\\unitlength}{0.07em}\\linethickness{0.035em}\\begin{picture}(10,4)(0,-0.5)~
                                         \\qbezier(2,3)(0.5,3)(0.5,2)\\qbezier(0.5,2)(0.5,1)(2,1)\\put(2,1){\\line(1,0){7.5}}~
                                         \\put(9.5,1){\\line(-3,2){2}}\\put(9.5,1){\\line(-3,-2){2}}\\end{picture}}}~%~
                                         \\fvset{breaksymbolleft=\\breakmark}~%~
                                         \\DeclareRobustCommand{\\codebreak}{\\discretionary{}{\\breakmark}{}}~%~
                                         \\pdfstringdefDisableCommands{\\let\\codebreak\\relax}~%~
                                         \\setlength{\\emergencystretch}{3em}~%\\tolerance=1000~%"
                                    *org-latex-landscape*)
                            "")))
          ((string= name "latex_header")
           (format nil "~{~a~%~}" (append (org-latex-keyword-all "LATEX_HEADER") (org-latex-keyword-all "LATEX_HEADER_EXTRA"))))
          ((string= name "secnumdepth") (and (integerp num) (princ-to-string num)))
          ((string= name "fonts")
           (format nil "~:{\\set~(~a~)font~a{~a}~%~}"
                   (loop for (part . family) in (org-latex-fonts *org-latex*)
                         collect (list part (if (eq part :mono) "[Scale=MatchLowercase,Contextuals=NoAlternate,Ligatures=NoCommon]" "") family))))
          ((string= name "font_fallbacks")
           ;; itself for bold and italic: a font of symbols has no such faces, whatever a setup file's defaults say
           (format nil "~:{\\newfontfamily\\~a{~a}[BoldFont={~a},ItalicFont={~a},BoldItalicFont={~a}]~%~}"
                   (mapcar (lambda (f) (list (cdr f) (car f) (car f) (car f) (car f))) (org-latex-fallbacks *org-latex*))))
          ((string= name "title") (if (eq way :raw) title (org-latex-string title)))
          ((string= name "author") (if author (if (eq way :raw) author (org-latex-string author)) ""))
          ((string= name "date") (if date (if (eq way :raw) date (org-latex-string date)) ""))
          ((string= name "maketitle") (and (org-latex-option "title" t) (plusp (length title)) "t"))
          ((string= name "toc") (and toc "t"))
          ((string= name "tocdepth") (and (integerp toc) (princ-to-string toc)))
          ((string= name "pdfauthor") (latex-escape (org-latex-plain (or author ""))))
          ((string= name "pdftitle") (latex-escape (org-latex-plain title)))
          ((string= name "pdfkeywords") (latex-escape (org-latex-plain (or (org-latex-keyword "KEYWORDS") ""))))
          ((string= name "pdfsubject") (latex-escape (org-latex-plain (or (org-latex-keyword "DESCRIPTION") ""))))
          ((string= name "pdflang") (if (string-equal language "en") "English" (latex-escape language)))
          ((null (keyword-values)) nil)
          ;; a picture the note names from where it may not come: its place and its conditions empty
          ((member name (org-latex-refused *org-latex*) :test #'string=) nil)
          ((progn (org-refuse-hidden-as-written name way (keyword-values)) nil))
          ((eq way :raw) (format nil "~{~a~^~%~}" (keyword-values)))
          ((eq way :path)
           (let ((path (template-picture-path (first (keyword-values)))))
             (when path (push path (org-latex-paths *org-latex*)))
             path))
          (t (format nil "~{~a~^\\newline ~}" (mapcar #'org-latex-string (keyword-values)))))))))

(defun org-refuse-hidden-as-written (name way values)
  "Refuse the template's place NAME read WAY when it prints VALUES, a keyword's,
as written (:RAW or :PATH) and one holds a name to hide: printed as written,
the call hides nothing."
  (when (and (member way '(:raw :path)) (some #'org-latex-hidden-call-p values))
    (error "#+~(~a~): holds {{{redact(...)}}} or {{{protect(...)}}}, which the template prints as written, ~
            its text with it: not exported" name)))

(defun org-latex-hidden-call-p (string)
  "True when STRING holds a call of redact or protect whose text the enhanced
export hides (ORG-LATEX-REDACTING-P)."
  (let ((found nil))
    (cl-ppcre:do-register-groups (name) (*org-hiding-call-re* string)
      (when (org-latex-redacting-p (org-hiding-kind name)) (setf found t)))
    found))

(defun org-latex-check-hidden (text original lines)
  "Refuse, with an error naming its line, a call of redact or protect left in
TEXT once its macros are expanded (ORIGINAL the text as written): one Org
reads no macros in (a code or example block, a link's address, raw LaTeX),
which would print its text, when that text is to be hidden. A keyword's line
is let be (its value is printed as Org text, where the call works), but for
#+LATEX_HEADER(_EXTRA) and #+LATEX:, printed as written. LINES, when given,
is the line of the file each line of the text comes from (a part exported
alone)."
  (let ((start 0))
    (loop
      (multiple-value-bind (at end groups-start groups-end) (cl-ppcre:scan *org-hiding-call-re* text :start start)
        (declare (ignore end))
        (unless at (return))
        (let* ((kind (org-hiding-kind (subseq text (aref groups-start 0) (aref groups-end 0))))
               (line-start (string-line-start text at))
               (keyword (nth-value 1 (cl-ppcre:scan-to-strings "^[ \\t]*#\\+([A-Za-z_]+):" text :start line-start))))
          (when (and (org-latex-redacting-p kind)
                     (or (null keyword)
                         (member (aref keyword 0) '("LATEX_HEADER" "LATEX_HEADER_EXTRA" "LATEX") :test #'string-equal)))
            (let* ((position (org-latex-original-position at))
                   (line (1+ (count #\Newline original :end (min position (length original)))))
                   (line (if lines (aref lines (min (1- (length lines)) (1- line))) line)))
              (error "{{{~a(...)}}} on line ~d is where Org reads no macros (a code or example block, a link's address, ~
                      raw LaTeX): its text would be printed, not hidden; not exported" kind line)))
          (setf start (1+ at)))))))

(defun org-template-name ()
  "The template the note being written names, by name or path: its
#+TEMPLATE:, or #+LATEX_TEMPLATE: or #+LATEX_STYLE:, that keyword's older
names; NIL when none."
  (let ((value (or (org-latex-keyword "TEMPLATE") (org-latex-keyword "LATEX_TEMPLATE") (org-latex-keyword "LATEX_STYLE"))))
    (and value (plusp (length (string-trim " " value))) (string-trim " " value))))

(defun org-latex-take-in (text tree directory)
  "Have *ORG-LATEX* know the note being written, TEXT (its macros expanded)
parsed as TREE, DIRECTORY its folder: its keywords and options, its lowest
level of headline, its footnotes' definitions by label, the parts it keeps
out, and, in the enhanced export, its :EXPORT: drawers' settings; for the
PDF and the Word export alike."
  (setf (org-latex-text *org-latex*) text
        (org-latex-tree *org-latex*) tree
        (org-latex-targets *org-latex*) nil
        (org-latex-keywords *org-latex*) (org-latex-collect-keywords tree directory))
  (org-latex-read-options *org-latex*)
  (let ((levels '()))
    (org-walk-tree tree (lambda (n)
                           (case (org-node-type n)
                             (headline (push (org-property n :level) levels))
                             (footnote-definition (setf (gethash (org-property n :label) (org-latex-footnotes *org-latex*)) n)))))
    (setf (org-latex-min-level *org-latex*) (if levels (reduce #'min levels) 1)))
  (setf (org-latex-kept *org-latex*) (org-latex-find-kept tree))
  (when (org-latex-enhanced *org-latex*)
    (setf (org-latex-settings *org-latex*) (org-latex-gather-settings tree))))

(defun org-latex-document (text &key file (engine "xelatex") enhanced lines)
  "TEXT, an Org document, written as a LaTeX document for the LaTeX program
ENGINE (#+LATEX_COMPILER chooses another), FILE its file or NIL, as the
built-in template org lays it out (FILL-LATEX-TEMPLATE); with ENHANCED, Tuatara's
own improvements on what Org writes, in the template #+TEMPLATE names
\(ORG-TEMPLATE-NAME, LATEX-TEMPLATE-FOR). Returns the
LaTeX; as a second value where #+EXPORT_FILE_NAME says to write it, a full
name ending in .tex, or NIL; as a third which place in TEXT each line of
the LaTeX comes from, a vector of (LINE BEGIN END) (ORG-LATEX-UNMARK), for
what LaTeX says of a line to be told at the Org text's; as a fourth the
characters no font here has, each (CHARACTER . LINE) (enhanced only); and
as a fifth what else to say of it, a list of sentences (a picture not shown);
as a sixth the texts of its template and setup files that are your own
\(EXPORT-TRUSTED-PATH-P), which alone may name pictures from other folders,
with the files your own template asks the note for by its {{{NAME:path}}}
places (a logo); and as a seventh the files its own pictures are drawn
from (ORG-LATEX-IMAGE), the only ones of the note's folder a LaTeX program
is shown when that folder may not be shown whole.
LINES, for a part of a file exported alone, is the file's line of each line
of TEXT, for the lines an error names."
  (let* ((directory (and file (tuatara.io:parent-path file)))
         (*org-list-allow-alphabetical* enhanced)
         (*org-latex* (let ((latex (make-org-latex text directory file)))
                        (setf (org-latex-enhanced latex) enhanced)
                        latex))
         (original text)
         (text (let ((expanded (org-latex-expand-macros text)))
                 ;; no name to hide printed where Org reads no macros
                 (when enhanced (org-latex-check-hidden expanded original lines))
                 expanded))
         (tree (org-parse text))
         (*org-latex-text* text))
    (org-latex-take-in text tree directory)
    (let ((compiler (org-latex-keyword "LATEX_COMPILER")))
      (setf (org-latex-engine *org-latex*) (if (and compiler (plusp (length compiler))) (string-trim " " compiler) engine)))
    (let* ((named (and enhanced (org-template-name)))
           (template-path nil)
           (template (cond (named (multiple-value-bind (text path) (latex-template-for named directory)
                                    (setf template-path path)
                                    text))
                           ((not enhanced) (find-latex-template "org" :built-in t))
                           ;; a note that sets no look of its own, in one of LaTeX's own kinds of
                           ;; document (article, report, book) or none: the enhanced export's own look
                           ((and (notany #'org-latex-keyword '("LATEX_HEADER" "LATEX_HEADER_EXTRA"))
                                 (member (string-trim " " (or (org-latex-keyword "LATEX_CLASS") "article"))
                                         '("article" "report" "book") :test #'string-equal))
                            (find-latex-template "article"))
                           (t (find-latex-template "org"))))
           ;; a template setting the note's text in columns: its tables kept whole
           (body (let ((*org-latex-in-columns* (latex-template-body-in-columns-p template))
                       (*org-latex-text-characters* (or (latex-template-text-characters template) *org-latex-text-characters*)))
                   (with-output-to-string (s) (org-latex-element tree s))))
           (missing '()))
      (when enhanced
        (multiple-value-setq (body missing) (org-latex-font-plan body (search "{{{fonts}}}" template))))
      (multiple-value-bind (latex map)
          (progn
            (setf (org-latex-refused *org-latex*) (template-refused-pictures template))
            (org-latex-unmark (fill-latex-template template (org-latex-field body))))
        (values latex (org-latex-output-name) map
                ;; each character no font here has, with the line of the LaTeX it is first on
                (let ((start (or (search "\\begin{document}" latex) 0)))
                  (loop for c in missing
                        for at = (position c latex :start start)
                        when at collect (cons c (1+ (count #\Newline latex :end at)))))
                (reverse (org-latex-notes *org-latex*))
                (if (or (null template-path) (export-trusted-path-p template-path))
                    (list* template (append (org-latex-paths *org-latex*) (org-latex-trusted *org-latex*)))
                    (org-latex-trusted *org-latex*))
                (reverse (org-latex-pictures *org-latex*)))))))

(defun org-latex-header-fonts ()
  "The fonts the document's #+LATEX_HEADER lines set with fontspec, (PART .
FAMILY) for \\setmainfont, \\setsansfont and \\setmonofont."
  (let ((found '()))
    (dolist (line (append (org-latex-keyword-all "LATEX_HEADER") (org-latex-keyword-all "LATEX_HEADER_EXTRA")) (nreverse found))
      (cl-ppcre:do-register-groups (part family) ("\\\\set(main|sans|mono)font(?:\\[[^]]*\\])?\\{([^}]*)\\}" line)
        (push (cons (intern (string-upcase part) :keyword) (string-trim " " family)) found)))))

(defun org-latex-font-plan (body chosen)
  "Plan the fonts of the enhanced export's document, BODY its text as LaTeX,
CHOSEN true when its template sets fonts found here ({{{fonts}}}): the fonts set
\(CHOSEN-FONT, else those its #+LATEX_HEADER lines name, else LaTeX's own,
taken to hold *LATEX-ASSUMED-CHARACTERS*), and for each character of BODY
they lack, a font installed that has it, under a command of its own. Returns
BODY with each such character set in its font, and as a second value the
characters no font here has. Only for xelatex and lualatex, which load the
system's fonts."
  (let ((engine (org-latex-engine *org-latex*)))
    (unless (member engine '("xelatex" "lualatex") :test #'string-equal)
      (return-from org-latex-font-plan (values body '())))
    (when chosen
      (setf (org-latex-fonts *org-latex*)
            (loop for part in '(:main :sans :mono)
                  for family in (chosen-fonts)
                  when family collect (cons part family))))
    (let* ((families (remove-duplicates (mapcar #'cdr (or (org-latex-fonts *org-latex*) (org-latex-header-fonts)))
                                        :test #'string-equal))
           ;; a font fontconfig does not know (one of LaTeX's own) taken to hold Latin
           (ranges (mapcar (lambda (family) (or (font-characters family) *latex-assumed-characters*))
                           (or families (list nil))))
           (lacking (let ((seen (make-hash-table)))
                      (loop for c across body
                            for code = (char-code c)
                            ;; the marks of the LaTeX's lines are no characters of the text
                            when (and (> code 127) (not (<= #xE000 code #xE001)) (not (gethash c seen))
                                      (notevery (lambda (r) (and r (characters-have-p r code))) ranges))
                              do (setf (gethash c seen) t)
                              and collect c)))
           (commands (make-hash-table)) (missing '()) (fonts '())
           (installed (and lacking (fallback-fonts))))
      (dolist (c lacking)
        (let ((family (font-for-character (char-code c) installed)))
          (if family
              (setf (gethash c commands)
                    (or (cdr (assoc family fonts :test #'string-equal))
                        (let ((name (format nil "tuataraFallback~:@(~r~)" (1+ (length fonts)))))
                          ;; letters only, as a command's name must be
                          (setf name (remove-if-not #'alpha-char-p name))
                          (push (cons family name) fonts)
                          name)))
              (push c missing))))
      (setf (org-latex-fallbacks *org-latex*) (reverse fonts))
      (values (if (plusp (hash-table-count commands))
                  (with-output-to-string (out)
                    (loop for c across body
                          for command = (gethash c commands)
                          do (if command (format out "{\\~a{}~c}" command c) (write-char c out))))
                  body)
              (nreverse missing)))))
