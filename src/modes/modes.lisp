;;;; src/modes/modes.lisp --- The major-mode tree.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A major mode is a CLOS class; a buffer holds an instance in its :major-mode
;;;; local. Each mode has a keymap whose parent is its parent mode's keymap, so
;;;; bindings inherit down the tree, and behaviour that differs by mode is a
;;;; generic function specialised on the class, so it inherits the same way.

(in-package #:tuatara.modes)

(defstruct (mode-info (:constructor make-mode-info (name)) (:copier nil))
  "What the editor knows about major mode NAME: its PARENT mode, DOC, the
LIGHTER shown in the mode line, its KEYMAP, the MODAL-CLASS it gives modal editing
(:editing, :support, :excluded, or NIL to inherit), its HOOKS, and how its
language writes comments: COMMENT, the marker a commented line starts with and
what commenting a line puts there, and BLOCK-COMMENT, what begins and ends a
comment that has an end, each a list of two strings or NIL to inherit; and
LARGEST, the most octets a file opened in the mode may have (LARGEST-FILE);
and KNOWN-MODAL-CLASS, the modal class it gives its buffers once worked out
along its inheritance, NIL until then."
  (name nil :read-only t)
  (parent nil)
  (doc "")
  (lighter "")
  (keymap nil)
  (modal-class nil)
  (hooks '())
  (comment nil)
  (block-comment nil)
  (largest nil)
  (known-modal-class nil))

(setf (documentation 'mode-info-p 'function) "True when OBJECT is a MODE-INFO."
      (documentation 'mode-info-name 'function) "The mode's name, a symbol naming its class."
      (documentation 'mode-info-parent 'function) "The parent mode's name, or NIL for the root."
      (documentation 'mode-info-doc 'function) "What the mode is for."
      (documentation 'mode-info-lighter 'function) "The name shown in the mode line."
      (documentation 'mode-info-keymap 'function) "The mode's keymap; its parent is the parent mode's keymap."
      (documentation 'mode-info-modal-class 'function) "The modal class the mode sets, or NIL to inherit."
      (documentation 'mode-info-known-modal-class 'function) "The modal class the mode gives its buffers once worked out, or NIL until then."
      (documentation 'mode-info-largest 'function) "The most octets a file opened in the mode may have, a number or a variable holding one; NIL for no limit."
      (documentation 'mode-info-hooks 'function) "Functions called with the buffer when the mode is entered."
      (documentation 'mode-info-comment 'function) "The marker of the mode's comments at the start of a line and what commenting a line puts there, as a list of two strings; NIL to inherit."
      (documentation 'mode-info-block-comment 'function) "What begins and ends the mode's comments that have an end, as a list of two strings; NIL to inherit.")

(defvar *modes* (make-hash-table :test 'eq)
  "Every major mode's MODE-INFO, keyed by the mode's name.")

(defvar *auto-mode-extensions* (make-hash-table :test 'equal)
  "File extensions (without the dot) and the mode they select. A file's extension
is looked for as it is written, then in any case: .C selects C++ while .c
selects C, and .PY selects Python.")
(defvar *auto-mode-file-names* (make-hash-table :test 'equal)
  "Exact file names (Makefile, Dockerfile) and the mode they select.")
(defvar *auto-mode-interpreters* (make-hash-table :test 'equal)
  "Interpreter names from a #! line (python3, bash) and the mode they select.")
(defvar *auto-mode-patterns* '()
  "Regular expressions on the whole path, tried before extensions, each
\(REGEX MODE SCANNER): the expression as written, the mode it selects, and the
expression compiled once.")

(defun find-mode (name)
  "The MODE-INFO of major mode NAME, or NIL."
  (gethash name *modes*))

(defun mode-keymap (name)
  "The keymap of major mode NAME."
  (let ((info (find-mode name)))
    (unless info (error "No major mode named ~(~a~)." name))
    (mode-info-keymap info)))

(defun mode-lighter (name)
  "The mode-line name of major mode NAME."
  (mode-info-lighter (find-mode name)))

(defun mode-names ()
  "The names of all major modes, sorted."
  (sort (hash-table-keys *modes*) #'string< :key #'symbol-name))

(defun mode-file-rules (name)
  "The rules that choose major mode NAME for a file, as four values, each a
sorted list of strings: the extensions (without their dot), the whole file
names, the interpreters of a #! line, and the regular expressions on the
whole path."
  (flet ((keys (table)
           (sort (loop for key being the hash-keys of table using (hash-value mode)
                       when (eq mode name) collect key)
                 #'string<)))
    (values (keys *auto-mode-extensions*) (keys *auto-mode-file-names*) (keys *auto-mode-interpreters*)
            (sort (loop for (regex mode) in *auto-mode-patterns* when (eq mode name) collect regex) #'string<))))

(defun default-lighter (name)
  "The mode-line name derived from mode NAME: python-mode gives \"Python\"."
  (let ((s (string-downcase (symbol-name name))))
    (when (and (> (length s) 5) (string= "-mode" s :start2 (- (length s) 5)))
      (setf s (subseq s 0 (- (length s) 5))))
    (string-capitalize s)))

(defun drop-auto-mode-rules (name)
  "Take away every rule that opens files in major mode NAME."
  (dolist (table (list *auto-mode-extensions* *auto-mode-file-names* *auto-mode-interpreters*))
    (maphash (lambda (k v) (when (eq v name) (remhash k table))) table))
  (setf *auto-mode-patterns* (remove name *auto-mode-patterns* :key #'second)))

(defun forget-major-mode (name)
  "Make NAME no longer a major mode: no file is opened in it and it is no
longer listed; a buffer in it stays so. Its command is forgotten apart
\(FORGET-COMMAND)."
  (drop-auto-mode-rules name)
  (remhash name *modes*)
  (forget-modal-classes))

(defun forget-modal-classes ()
  "Make every mode work out its modal class again when next asked, after a
mode is defined again or forgotten: a mode's class follows the modes it is
made from."
  (loop for info being the hash-values of *modes*
        do (setf (mode-info-known-modal-class info) nil)))

(defun register-major-mode (name parent doc &key lighter modal-class comment block-comment extensions file-names interpreters patterns largest-file)
  "Record major mode NAME with PARENT, DOC and the rest, creating its keymap
on first registration (a re-registration keeps the keymap and its bindings) and
replacing its auto-mode rules. Called by DEFINE-MAJOR-MODE."
  (let ((info (or (find-mode name) (setf (gethash name *modes*) (make-mode-info name)))))
    (setf (mode-info-parent info) parent
          (mode-info-doc info) doc
          (mode-info-lighter info) (or lighter (default-lighter name))
          (mode-info-modal-class info) modal-class
          (mode-info-comment info) (if (stringp comment) (list comment comment) comment)
          (mode-info-block-comment info) block-comment
          (mode-info-largest info) largest-file)
    (forget-modal-classes)
    (unless (mode-info-keymap info)
      (setf (mode-info-keymap info) (tuatara.command:make-keymap :name (string-downcase (symbol-name name)))))
    (setf (tuatara.command:keymap-parent (mode-info-keymap info)) (and parent (mode-keymap parent)))
    (drop-auto-mode-rules name)
    (dolist (e extensions) (setf (gethash e *auto-mode-extensions*) name))
    (dolist (f file-names) (setf (gethash f *auto-mode-file-names*) name))
    (dolist (i interpreters) (setf (gethash i *auto-mode-interpreters*) name))
    (dolist (p patterns) (push (list p name (cl-ppcre:create-scanner p)) *auto-mode-patterns*))
    name))

(defmacro define-major-mode (name parent doc &key lighter modal-class comment block-comment extensions file-names interpreters patterns largest-file (interactive t))
  "Define major mode NAME as a subclass of mode PARENT (NIL only for the root),
documented by DOC. LIGHTER is the mode-line name (derived from NAME by
default); MODAL-CLASS is :editing, :support or :excluded for modal editing (inherited
when omitted). COMMENT is how the mode's language writes comments at the start
of a line: the marker, \"#\", or a list of the marker and what commenting a
line puts there, (\";\" \";;\"). BLOCK-COMMENT, for a language with no such
comments, is a list of what begins a comment and what ends it, (\"<!--\"
\"-->\"). Both are inherited when omitted. EXTENSIONS, FILE-NAMES,
INTERPRETERS and PATTERNS (regexes on the path) select the mode for files.
LARGEST-FILE, a number of octets or a variable holding one, is the largest
file opened in the mode by itself: a larger one opens in the nearest parent
mode without that limit (MODE-FOR-SIZE). Also defines the command NAME, which
switches the current buffer to the mode, unless INTERACTIVE is NIL: a mode only
for the buffer a command of its own makes, as the list of jobs is."
  (check-type doc string "a docstring; every mode documents itself")
  `(progn
     (defclass ,name ,(if parent (list parent) '()) ()
       (:documentation ,doc))
     (register-major-mode ',name ',parent ,doc :lighter ,lighter :modal-class ,modal-class
                          :comment ',comment :block-comment ',block-comment
                          :extensions ',extensions :file-names ',file-names
                          :interpreters ',interpreters :patterns ',patterns :largest-file ',largest-file)
     ,@(when interactive
         `((tuatara.command:define-command ,name ()
             ,(format nil "Switch the current buffer to ~(~a~). ~a" name (tuatara.command:first-sentence doc))
             (set-major-mode (tuatara.command:current-buffer) ',name))))
     ',name))

(define-major-mode fundamental-mode nil
  "The root of the major-mode tree: no assumptions about the text. Buffers without
another mode behave this way."
  :modal-class :editing)

;;; Behaviour by mode

(defgeneric mode-enter (mode buffer)
  (:documentation "Called when BUFFER switches to MODE, after the mode's keymap and name are
installed and before its hooks run. Methods should call CALL-NEXT-METHOD.")
  (:method (mode buffer) (declare (ignore mode buffer)) nil))

(defgeneric mode-exit (mode buffer)
  (:documentation "Called when BUFFER leaves MODE, or is killed, to release what MODE-ENTER set up.")
  (:method (mode buffer) (declare (ignore mode buffer)) nil))

(defgeneric mode-syntax-highlights (mode buffer rows)
  (:documentation "Syntax colours for the text laid out in ROWS of BUFFER, as a list of
(START END FACE) byte ranges; NIL when MODE colours nothing.")
  (:method (mode buffer rows) (declare (ignore mode buffer rows)) nil))

(defgeneric mode-marks-point-p (mode)
  (:documentation "True when MODE shows where point is in a look of its own (the calendar
its day), so that no cursor is drawn over it.")
  (:method (mode) (declare (ignore mode)) nil))

(defgeneric mode-keeps-place-p (mode)
  (:documentation "True when where point is in a file in MODE is remembered and put back
on the next visit (*SAVE-PLACE-MODE*); false for a file written afresh each
time, as git's commit messages are.")
  (:method (mode) (declare (ignore mode)) t))

(defun mode-comment-syntax (mode)
  "How MODE's language writes comments at the start of a line, as two values:
the marker a commented line starts with (\";\" in Lisp, \"//\" in Rust), and what
commenting a line puts there (\";;\" in Lisp); NIL when the language has no such
comments. The :COMMENT of MODE's definition, or of the nearest mode it is made
from that gives one."
  (let ((comment (loop for name in (mode-precedence mode) thereis (mode-info-comment (find-mode name)))))
    (and comment (values (first comment) (second comment)))))

(defun mode-block-comment-syntax (mode)
  "How MODE's language writes a comment that has an end, for a language with
no comments at the start of a line (MODE-COMMENT-SYNTAX), as two values: what
begins it and what ends it (\"<!--\" and \"-->\" in HTML); NIL when there is none.
The :BLOCK-COMMENT of MODE's definition, or of the nearest mode it is made from
that gives one."
  (let ((comment (loop for name in (mode-precedence mode) thereis (mode-info-block-comment (find-mode name)))))
    (and comment (values (first comment) (second comment)))))

(defgeneric mode-indent-lines (mode buffer start end &optional blank-lines)
  (:documentation "How MODE indents the lines of BUFFER from the one holding octet START to the
one holding END: a list of (LINE-START . COLUMN), in order, for each line whose
indentation should become COLUMN, positions as the text is now and each column
worked out as if the lines before it were already indented; lines left out
stay as they are, blank ones among them unless BLANK-LINES. :NONE when MODE
has no rules for indenting.")
  (:method (mode buffer start end &optional blank-lines)
    (declare (ignore mode buffer start end blank-lines))
    :none))

(defgeneric mode-line-extra (mode buffer)
  (:documentation "Extra text MODE shows in the mode line for BUFFER (the CSV column at point,
for example), or NIL.")
  (:method (mode buffer) (declare (ignore mode buffer)) nil))

(defun mode-precedence (mode)
  "The names of the registered modes MODE is made from, most specific first,
MODE's own first: MODE a major mode, or the name of one (none for a name of
no class)."
  (let ((class (if (symbolp mode) (find-class mode nil) (class-of mode))))
    (when class
      (unless (sb-mop:class-finalized-p class) (sb-mop:finalize-inheritance class))
      (loop for c in (sb-mop:class-precedence-list class)
            for name = (class-name c)
            when (find-mode name) collect name))))

(defun names-mode-p (given name)
  "True when GIVEN, a name written in a setting, stands for the major mode
called NAME: a symbol from any package or a string, with or without -mode,
upper or lower case, so that org, org-mode and \"org-mode\" all name
org-mode. NIL and T name no mode."
  (or (eq given name)
      (and (or (stringp given) (and (symbolp given) given (not (eq given t))))
           (let* ((written (string given)) (mode (symbol-name name)) (n (length written)))
             (or (string-equal written mode)
                 (and (= (length mode) (+ n 5))
                      (string-equal written mode :end2 n)
                      (string-equal "-mode" mode :start2 n)))))))

(defun mode-setting (mode settings)
  "What SETTINGS says of major mode MODE (an instance), as two values: the
value and T; NIL and NIL when it says nothing of MODE or the modes MODE is
made from. The nearest mode decides: what is said of org-mode counts before
what is said of text-mode, which Org is made from. SETTINGS is a hash table
keyed by modes' names, or a list whose entries are (NAME . VALUE) or a bare
NAME, which means T; a NAME is matched as NAMES-MODE-P has it, and the first
entry for a mode counts. Linear in the modes MODE is made from times the
entries."
  (if (not (and mode (if (hash-table-p settings) (plusp (hash-table-count settings)) settings)))
      (values nil nil)
      (dolist (class (sb-mop:class-precedence-list (class-of mode)) (values nil nil))
        (let ((name (class-name class)))
          (when (find-mode name)
            (if (hash-table-p settings)
                (multiple-value-bind (value found) (gethash name settings)
                  (when found (return (values value t))))
                (dolist (entry settings)
                  (when (names-mode-p (if (consp entry) (car entry) entry) name)
                    (return-from mode-setting (values (if (consp entry) (cdr entry) t) t))))))))))

(defun buffer-setting (buffer key settings global)
  "BUFFER's value of a setting it may choose for itself: its own, the
buffer-local KEY, when it has one; else what SETTINGS says of its major mode
or the nearest mode that mode is made from (MODE-SETTING); else GLOBAL."
  (let ((own (tuatara.buffer:buffer-local buffer key '%unset)))
    (if (not (eq own '%unset))
        own
        (multiple-value-bind (value found) (mode-setting (buffer-major-mode buffer) settings)
          (if found value global)))))

(defun mode-modal-class (mode)
  "The modal class MODE gives its buffers: the nearest modal class set along
its inheritance, :editing when none is. Worked out once a mode, until a mode is
defined again."
  (flet ((worked-out ()
           (or (loop for name in (mode-precedence mode)
                     thereis (mode-info-modal-class (find-mode name)))
               :editing)))
    (let ((info (find-mode (if (symbolp mode) mode (class-name (class-of mode))))))
      (if info
          (or (mode-info-known-modal-class info)
              (setf (mode-info-known-modal-class info) (worked-out)))
          (worked-out)))))

;;; Buffers and modes

(defvar *fundamental* nil "The shared FUNDAMENTAL-MODE instance of buffers that have no mode yet.")

(defun buffer-major-mode (buffer)
  "The major mode instance of BUFFER; FUNDAMENTAL-MODE when it has none yet."
  (or (tuatara.buffer:buffer-local buffer :major-mode)
      *fundamental*
      (setf *fundamental* (make-instance 'fundamental-mode))))

(defun add-mode-hook (mode function)
  "Call FUNCTION with the buffer whenever a buffer enters MODE or a mode derived from it."
  (pushnew function (mode-info-hooks (find-mode mode))))

(defun set-major-mode (buffer name)
  "Switch BUFFER to major mode NAME: leave the old mode, install the new mode's
keymap and name, call MODE-ENTER, then run the hooks of NAME and its ancestors,
the root's first. Returns the new mode instance."
  (let ((old (tuatara.buffer:buffer-local buffer :major-mode))
        (info (or (find-mode name) (error "No major mode named ~(~a~)." name))))
    (when old (mode-exit old buffer))
    (let ((mode (make-instance name)))
      (setf (tuatara.buffer:buffer-local buffer :major-mode) mode
            (tuatara.buffer:buffer-local buffer :keymap) (mode-info-keymap info)
            (tuatara.buffer:buffer-local buffer :mode-name) (mode-info-lighter info))
      (mode-enter mode buffer)
      (dolist (ancestor (reverse (mode-precedence mode)))
        (dolist (hook (reverse (mode-info-hooks (find-mode ancestor))))
          (funcall hook buffer)))
      mode)))

(defun find-or-make-buffer (name &key mode made)
  "The buffer called NAME; when there is none, a new one, in the major MODE
when given, MADE (a function of the buffer) called on it to set it up."
  (or (tuatara.buffer:find-buffer name)
      (let ((buffer (tuatara.buffer:make-buffer name)))
        (when mode (set-major-mode buffer mode))
        (when made (funcall made buffer))
        buffer)))

(defun shebang-interpreter (line)
  "The interpreter named by LINE if it is a #! line (#!/usr/bin/env python3 gives \"python3\"), else NIL."
  (when (and (> (length line) 2) (string= "#!" line :end2 2))
    (let* ((words (tuatara.text:split-words (subseq line 2) '(#\Space #\Tab)))
           (program (first words)))
      (when program
        (let ((base (tuatara.io:native-file-name program)))
          (if (and (string= base "env") (second words))
              (let ((next (find-if-not (lambda (w) (char= (char w 0) #\-)) (rest words))))
                (and next (tuatara.io:native-file-name next)))
              base))))))

(defun extension-mode (type)
  "The mode extension TYPE selects: as it is written, else in any case."
  (or (gethash type *auto-mode-extensions*)
      (gethash (string-downcase type) *auto-mode-extensions*)
      (loop for extension being the hash-keys of *auto-mode-extensions* using (hash-value mode)
            when (string-equal extension type) return mode)))

(defun mode-for-file (path &optional first-line)
  "The major mode for a file at PATH whose first line is FIRST-LINE: by exact
file name, then path pattern, then extension (as it is written, else in any
case), then #! interpreter (with a trailing version number removed, so
python3.12 finds python), else FUNDAMENTAL-MODE."
  (let* ((name (tuatara.io:native-file-name path))
         (type (pathname-type path)))
    ;; an encrypted file has the mode of what it holds: notes.org.gpg, Org
    (when (and type (string-equal type "gpg") (> (length name) 4))
      (return-from mode-for-file (mode-for-file (subseq (tuatara.io:native path) 0 (- (length (tuatara.io:native path)) 4)) first-line)))
    (or (gethash name *auto-mode-file-names*)
        (loop with native = (tuatara.io:native path)
              for (nil mode scanner) in *auto-mode-patterns*
              when (cl-ppcre:scan scanner native) return mode)
        (and type (extension-mode type))
        (let ((interpreter (and first-line (shebang-interpreter first-line))))
          (and interpreter
               (or (gethash interpreter *auto-mode-interpreters*)
                   (gethash (string-right-trim "0123456789." interpreter) *auto-mode-interpreters*))))
        'fundamental-mode)))

(defun mode-largest-file (name)
  "The most octets a file opened in major mode NAME by itself may have, or NIL
for no limit (DEFINE-MAJOR-MODE's LARGEST-FILE)."
  (let* ((info (find-mode name))
         (largest (and info (mode-info-largest info))))
    (if (and largest (symbolp largest)) (symbol-value largest) largest)))

(defun mode-for-size (name octets)
  "Major mode NAME, or for a file of OCTETS too large for it (MODE-LARGEST-FILE)
the nearest of its parents that takes a file that large."
  (loop for mode = name then (mode-info-parent (find-mode mode))
        for largest = (and mode (mode-largest-file mode))
        while mode
        when (or (null largest) (<= octets largest)) return mode
        finally (return 'fundamental-mode)))

(defun set-auto-mode (buffer)
  "Give BUFFER the major mode its file name and first line call for, or one
of that mode's parents when the file is too large for it (MODE-FOR-SIZE),
saying so."
  (let* ((path (tuatara.buffer:buffer-file buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (first-line (let ((end (min (tuatara.text:rope-bytes rope) 512)))
                       (and (plusp end)
                            (let ((octets (tuatara.text:rope-octets rope :start 0 :end end)))
                              ;; a Windows file's first line ends with a carriage return too
                              (string-right-trim '(#\Return)
                                                 (tuatara.text:lenient-string octets :end (or (tuatara.text:octets-line-end-position octets 0 (length octets))
                                                                              (length octets)))))))))
    (let* ((wanted (if path (mode-for-file path first-line) 'fundamental-mode))
           (mode (mode-for-size wanted (tuatara.text:rope-bytes rope))))
      (unless (eq mode wanted)
        (tuatara.command:message "~a is larger than ~(~a~) opens (~a): opened in ~(~a~)"
                                 (tuatara.buffer:buffer-name buffer) wanted
                                 (tuatara.io:octets-words (mode-largest-file wanted)) mode))
      (set-major-mode buffer mode))))
