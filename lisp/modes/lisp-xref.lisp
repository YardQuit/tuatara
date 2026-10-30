;;;; lisp/modes/lisp-xref.lisp --- Going to where a name is defined: M-. M-, C-M-, and the *xref* list.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's xref: M-. goes to the definition of the name at point (asking
;;;; for one with C-u, or where there is none), and M-, comes back, C-M-, goes
;;;; forward again, through one history for every buffer. A name defined in
;;;; more than one place lists its definitions in *xref*, file by file, where
;;;; RET goes to the one on the line, n and p show the next and the previous
;;;; beside the list, and q closes it. The major mode says what a name is and
;;;; where it is defined; Lisp asks the running Lisp: the source files the
;;;; editor was built from, and the buffers definitions were evaluated from.

(in-package #:tuatara.defaults)

;;; The history

(defvar *xref-back* '()
  "The places \\[xref-find-definitions] left, newest first, each (BUFFER . ANCHOR):
where \\[xref-go-back] goes back to; the newest *HISTORY-KEPT* of them.")

(defvar *xref-forward* '()
  "The places \\[xref-go-back] went back from, newest first, each
(BUFFER . ANCHOR): where \\[xref-go-forward] goes.")

(defun here-marker ()
  "The current buffer and an anchor at point, for the history of
\\[xref-find-definitions] and \\[xref-go-back]."
  (let ((b (current-buffer)))
    (cons b (tuatara.text:make-anchor (tuatara.buffer:buffer-version b) (point)))))

(defun go-to-marker (marker)
  "Show MARKER's buffer in the selected window, point where MARKER's anchor is now."
  (destructuring-bind (b . anchor) marker
    (show-buffer b)
    (goto-shown b (min (tuatara.buffer:buffer-size b)
                       (or (ignore-errors (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version b))) 0)))))

(defun live-marker-p (marker)
  "True when MARKER's buffer is still alive."
  (live-buffer-p (car marker)))

(defun push-xref-marker ()
  "Remember point as the place \\[xref-go-back] comes back to; the places to go
forward to are forgotten."
  (setf *xref-back* (pushed-and-kept (here-marker) *xref-back*)
        *xref-forward* '()))

(define-command xref-go-back ()
  "Go back to where \\[xref-find-definitions] was typed: each \\[xref-go-back]
a step further back, \\[xref-go-forward] forward again."
  (setf *xref-back* (remove-if-not #'live-marker-p *xref-back*))
  (if (null *xref-back*)
      (message "At start of xref history")
      (let ((marker (pop *xref-back*)))
        (setf *xref-forward* (pushed-and-kept (here-marker) *xref-forward*))
        (go-to-marker marker))))

(define-command xref-go-forward ()
  "Go forward again to where \\[xref-go-back] came back from."
  (setf *xref-forward* (remove-if-not #'live-marker-p *xref-forward*))
  (if (null *xref-forward*)
      (message "At end of xref history")
      (let ((marker (pop *xref-forward*)))
        (setf *xref-back* (pushed-and-kept (here-marker) *xref-back*))
        (go-to-marker marker))))

;;; Definitions

(defstruct (definition (:constructor make-definition (summary group locate)) (:copier nil))
  "Where a name is defined: SUMMARY, how the list shows it, (defun name);
GROUP, the file or buffer it is in, as the list heads it; LOCATE, a function of
no arguments returning the buffer and the octet where the definition begins,
or NIL and what to say when it cannot be reached."
  (summary "" :type string :read-only t)
  (group "" :type string :read-only t)
  (locate nil :read-only t))

(setf (documentation 'make-definition 'function) "A DEFINITION from its SUMMARY, GROUP and LOCATE function."
      (documentation 'definition-p 'function) "True when OBJECT is a DEFINITION."
      (documentation 'definition-summary 'function) "How the list of definitions shows a DEFINITION."
      (documentation 'definition-group 'function) "The file or buffer a DEFINITION is in."
      (documentation 'definition-locate 'function) "The function that finds a DEFINITION's buffer and octet.")

(defgeneric mode-name-at-point (mode buffer position)
  (:documentation "The name at POSITION of BUFFER that MODE can look up the definitions of,
as a string; NIL when there is none.")
  (:method (mode buffer position) (declare (ignore mode buffer position)) nil))

(defgeneric mode-find-definitions (mode buffer name)
  (:documentation "Where NAME, a string, is defined, as MODE looks it up for BUFFER: a list of
DEFINITION; :NONE when MODE looks up no definitions.")
  (:method (mode buffer name) (declare (ignore mode buffer name)) :none))

(defgeneric mode-name-candidates (mode buffer)
  (:documentation "Completion candidates for the names MODE can look up the definitions of,
for \\[xref-find-definitions] to ask with; NIL for none.")
  (:method (mode buffer) (declare (ignore mode buffer)) nil))

(defun go-to-definition (definition &key other)
  "Go to DEFINITION in the selected window, or with OTHER in another, or say
why it cannot be reached."
  (multiple-value-bind (b position) (funcall (definition-locate definition))
    (if (typep b 'tuatara.buffer:buffer)
        (progn (if other (pop-to-buffer b :other t) (show-buffer b)) (goto-shown b position))
        (message "~a" position))))

(define-command xref-find-definitions ()
  "Go to the definition of the name at point.
With a prefix argument (\\[universal-argument]), or where there is no name at
point, ask for one. A name defined in more than one place lists its
definitions in *xref*. \\[xref-go-back] comes back. In Lisp the running Lisp
is asked: the files the editor was built from, and the buffers definitions
were evaluated from."
  (let* ((b (current-buffer))
         (mode (buffer-major-mode b)))
    (when (eq (mode-find-definitions mode b "") :none)
      (message "No way to find definitions in ~a buffers" (mode-lighter (type-of mode)))
      (return-from xref-find-definitions nil))
    (xref-find-definitions-in b mode)))

(defun qualified-name (symbol)
  "SYMBOL's name with its package's, pkg::name, so that it is found whatever
package reads it; the bare name for a symbol of no package."
  (let ((package (symbol-package symbol)))
    (if package
        (format nil "~(~a::~a~)" (package-name package) (symbol-name symbol))
        (string-downcase (symbol-name symbol)))))

(defun xref-find-definitions-in (b mode)
  "Go to the definition of the name at point of B, whose major mode MODE finds
definitions, as xref-find-definitions does."
  (let* ((at-point (mode-name-at-point mode b (point)))
         (name (if (or (null at-point) (prefix-argument))
                   (let ((chosen (completing-read "Find definitions of: "
                                                  (mode-name-candidates mode b)
                                                  :initial (or at-point ""))))
                     (if (symbolp chosen) (qualified-name chosen) chosen))
                   at-point))
         (definitions (mode-find-definitions mode b name)))
    (cond ((null definitions) (message "No definitions found for: ~a" name))
          ((null (rest definitions))
           (push-xref-marker)
           (go-to-definition (first definitions)))
          (t
           (push-xref-marker)
           (show-definitions name definitions)))))

;;; The list of definitions: *xref*

(define-major-mode xref-mode special-mode
  "The definitions of a name, file by file, as \\[xref-find-definitions] lists
them when there is more than one: \\<xref-mode>\\`RET' (or \\[list-visit]) goes
to the one on the line, \\[list-next-entry] and \\[list-previous-entry] show the next
and the previous in the window \\[xref-find-definitions] was typed in,
\\[revert-buffer] finds them again, \\[quit-window] closes the list."
  :lighter "XREF" :interactive nil)

(defvar *xref-keymap* (mode-keymap 'xref-mode)
  "Keys of the *xref* list (xref-mode's keymap).")

(defun xref-list-render (list name definitions &optional (point :keep))
  "Fill LIST, a *xref* buffer, with DEFINITIONS of NAME, file by file, each
file's line in :heading, point as RENDER-PIECES puts it."
  (let ((pieces '()) (entries '()) (group nil))
    (dolist (d definitions)
      (unless (equal group (definition-group d))
        (setf group (definition-group d))
        (push nil entries)
        (push (cons (format nil "~a" group) :heading) pieces)
        (push (string #\Newline) pieces))
      (push d entries)
      (push (format nil "  ~a~%" (definition-summary d)) pieces))
    (setf (tuatara.buffer:buffer-local list 'xref-name) name
          (tuatara.buffer:buffer-local list 'xref-definitions) definitions)
    (render-pieces list (nreverse pieces) :point point :entries (coerce (nreverse entries) 'simple-vector)
                   :refill 'xref-refill)))

(defun xref-refill (list)
  "Find the definitions *xref* LIST shows again, from the buffer they were
first looked for in while it is there, and list them, point on the same line
\(\\[revert-buffer])."
  (let* ((source (tuatara.buffer:buffer-local list 'xref-source))
         (name (tuatara.buffer:buffer-local list 'xref-name))
         (again (tuatara.buffer:buffer-local list 'xref-again))
         (found (and source (live-buffer-p source)
                     (if again (funcall again) (mode-find-definitions (buffer-major-mode source) source name)))))
    (xref-list-render list name (if (consp found) found (tuatara.buffer:buffer-local list 'xref-definitions)))))

(defun show-definitions (name definitions &key again)
  "List DEFINITIONS of NAME in *xref*, file by file, and select its window.
AGAIN, a function of no arguments giving them anew, is how \\[revert-buffer]
there finds them again; else as the definitions of NAME."
  (let ((list (buffer-in-mode "*xref*" 'xref-mode))
        (window (current-window))
        (source (current-buffer)))
    (setf (tuatara.buffer:buffer-local list :kind) :xref
          (tuatara.buffer:buffer-local list 'xref-window) window
          (tuatara.buffer:buffer-local list 'xref-source) source
          (tuatara.buffer:buffer-local list 'xref-again) again)
    ;; on the first definition
    (xref-list-render list name definitions 1)
    (pop-to-buffer list)))

(defun xref-show-in-window (definition select)
  "Show DEFINITION in the window \\[xref-find-definitions] was typed in, and
select that window when SELECT; in another window when that one is gone."
  (let* ((list (current-buffer))
         (window (tuatara.buffer:buffer-local list 'xref-window)))
    (multiple-value-bind (b position) (funcall (definition-locate definition))
      (if (not (typep b 'tuatara.buffer:buffer))
          (message "~a" position)
          (let ((target (if (member window (editor-windows *editor*)) window (display-buffer b :other t))))
            (when target
              (let ((here (current-window)))
                (select-window target)
                (show-buffer b)
                (goto-shown b position)
                (unless select (select-window here)))))))))

(defmethod list-entry-noun ((mode xref-mode))
  "A definition."
  "definition")

(defmethod visit-entry ((mode xref-mode) list definition other)
  "Go to DEFINITION in the window \\[xref-find-definitions] was typed in, and select that
window; the list stays."
  (declare (ignore list other))
  (xref-show-in-window definition t))

(defmethod list-entry-stepped ((mode xref-mode) list definition)
  "Show DEFINITION in the window \\[xref-find-definitions] was typed in, the list staying selected."
  (declare (ignore list))
  (xref-show-in-window definition nil))

;;; Lisp's definitions

(defparameter *definition-heads*
  '((:variable . "defvar") (:constant . "defconstant") (:function . "defun") (:macro . "defmacro")
    (:generic-function . "defgeneric") (:method . "defmethod") (:class . "defclass") (:structure . "defstruct")
    (:condition . "define-condition") (:type . "deftype") (:compiler-macro . "define-compiler-macro")
    (:setf-expander . "defsetf") (:symbol-macro . "define-symbol-macro") (:method-combination . "define-method-combination"))
  "The kinds of definition sb-introspect knows, and what the list of definitions calls each.")

(defun definition-pattern (symbol)
  "A regular expression finding the start of a definition of SYMBOL in Lisp
text: an open bracket, a word that defines (DEFINITION-HEAD-P), and SYMBOL's
name, (setf name)'s too, or the name quoted ('name, as recorded keys are
named), in any case."
  (format nil "(?i)\\((?:[^\\s():]+::?)?(?:~{~a|~}define-[a-z0-9-]+)\\s+'?(?:\\(setf\\s+)?(?:[^\\s():]+::?)?~a(?=[\\s()])"
          (mapcar #'cl-ppcre:quote-meta-chars *definition-head-names*)
          (cl-ppcre:quote-meta-chars (symbol-name symbol))))

(defun definitions-in-text (text symbol)
  "The character indices in TEXT, Lisp text, where definitions of SYMBOL begin."
  (let ((found '()))
    (cl-ppcre:do-matches (start end (definition-pattern symbol) text)
      (push start found))
    (nreverse found)))

(defun top-level-form-start (buffer number)
  "The octet of BUFFER where its top-level form NUMBER begins, counting from 0
as the reader reads them; NIL when there are fewer."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (tops (remove-if-not (lambda (e) (zerop (third e)))
                              (lisp-elements (tuatara.text:rope-octets rope)))))
    (let ((form (nth number tops)))
      (and form (first form)))))

(defun locate-in-file (path symbol offset form-number)
  "The buffer visiting PATH and the octet where the definition of SYMBOL there
begins: near character OFFSET, or top-level form FORM-NUMBER, as the compiler
recorded them, the nearest definition of SYMBOL in the text now, so that
changes since do not lead astray. NIL and what to say when PATH is not there."
  (if (not (probe-file path))
      (values nil (format nil "The source of ~a is not here: ~a" (lisp-name-text symbol) (tuatara.io:native path)))
      (let* ((b (tuatara.io:visit-file path))
             (rope (progn (tuatara.buffer:ensure-indexed b) (tuatara.buffer:buffer-text b)))
             (hint (cond (offset (tuatara.text:char->byte rope (min offset (tuatara.text:rope-chars rope))))
                         (form-number (top-level-form-start b form-number))
                         (t 0)))
             (text (tuatara.buffer:buffer-string b))
             (starts (mapcar (lambda (i) (tuatara.text:char->byte rope i)) (definitions-in-text text symbol))))
        (values b (if starts
                      (reduce (lambda (a c) (if (< (abs (- c (or hint 0))) (abs (- a (or hint 0)))) c a)) starts)
                      (or hint 0))))))

(defun method-summary (symbol description)
  "How the list shows a method of SYMBOL whose qualifiers and specializers are
DESCRIPTION: (defmethod name :around (class t)), names without their packages."
  (labels ((show (x)
             (cond ((keywordp x) (format nil ":~(~a~)" (symbol-name x)))
                   ((symbolp x) (lisp-name-text x))
                   ((consp x) (format nil "(~{~a~^ ~})" (mapcar #'show x)))
                   (t (let ((*print-case* :downcase)) (prin1-to-string x))))))
    (let ((qualifiers (loop for x in description while (keywordp x) collect x))
          (specializers (member-if-not #'keywordp description)))
      (format nil "(defmethod ~a~{ ~a~} ~a)" (lisp-name-text symbol) (mapcar #'show qualifiers) (show specializers)))))

(defun lisp-definitions (symbol)
  "Where SYMBOL is defined, a list of DEFINITION: the sources the running Lisp
recorded for it, every kind of definition, and the buffers definitions of it
were evaluated from in this session."
  (let ((seen (make-hash-table :test 'equal)) (out '()))
    ;; evaluated from buffers of this session
    (dolist (b (gethash symbol *evaluated-definitions*))
      (when (live-buffer-p b)
        (let ((text (tuatara.buffer:buffer-string b)))
          (dolist (start (definitions-in-text text symbol))
            (let* ((octet (tuatara.text:string-octet-length text :end start))
                   (line (subseq text start (string-line-end text start)))
                   ;; followed through the edits made before the list is used
                   (anchor (tuatara.text:make-anchor (tuatara.buffer:buffer-version b) octet)))
              (push (make-definition (string-trim " " line) (tuatara.buffer:buffer-name b)
                                     (let ((b b) (anchor anchor) (name (tuatara.buffer:buffer-name b)))
                                       (lambda ()
                                         (if (live-buffer-p b)
                                             (values b (min (tuatara.buffer:buffer-size b)
                                                            (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version b))))
                                             (values nil (format nil "~a, where it was defined, has been killed" name))))))
                    out))))))
    ;; defined by the init file, in place of the source records, which may name only the
    ;; file of what made the function (recorded keys)
    (let ((init (init-file-definition symbol)))
      (when init
        (destructuring-bind (file line head) init
          (declare (ignore line))
          (push (make-definition (format nil "(~(~a~) ~a)" head (lisp-name-text symbol))
                                 file
                                 (lambda () (locate-in-file file symbol nil nil)))
                out))))
    ;; recorded by the compiler
    (loop for (type . head) in *definition-heads*
          ;; a generic function or a macro is no plain function
          unless (or (and (eq type :function) (fboundp symbol)
                          (or (macro-function symbol) (typep (fdefinition symbol) 'generic-function)))
                     (and (eq type :function) (init-file-definition symbol)))
          do (dolist (source (ignore-errors (sb-introspect:find-definition-sources-by-name symbol type)))
               (let* ((path (sb-introspect:definition-source-pathname source))
                      (offset (sb-introspect:definition-source-character-offset source))
                      (form-path (sb-introspect:definition-source-form-path source))
                      (file (and path (ignore-errors (namestring (translate-logical-pathname path)))))
                      (description (ignore-errors (sb-introspect::definition-source-description source)))
                      (key (list file offset form-path description)))
                 (when (and file (not (gethash key seen)))
                   (setf (gethash key seen) t)
                   (push (make-definition (if (eq type :method)
                                              (method-summary symbol description)
                                              (format nil "(~a ~a)" (if (and (eq type :function) (gethash symbol *commands*))
                                                                        "define-command"
                                                                        head)
                                                      (lisp-name-text symbol)))
                                          file
                                          (let ((file file) (offset offset) (form (first form-path)))
                                            (lambda () (locate-in-file file symbol offset form))))
                         out)))))
    ;; a file's definitions together, in the order they come in it
    (stable-sort (nreverse out) #'string< :key #'definition-group)))

(defmethod mode-name-at-point ((mode lisp-mode) buffer position)
  "The Lisp name at point in BUFFER."
  (declare (ignore position))
  (ignore-errors (symbol-at-point buffer)))

(defmethod mode-find-definitions ((mode lisp-mode) buffer name)
  "Where NAME, read in BUFFER's package, is defined."
  (let ((symbol (lisp-symbol-named (string-trim "'#`, " name) (lisp-package buffer))))
    (and symbol (lisp-definitions symbol))))

(defmethod mode-name-candidates ((mode lisp-mode) buffer)
  "The names of the functions and variables there are, to choose one to go to."
  (declare (ignore buffer))
  (append (function-candidates) (variable-candidates)))

(defun forget-killed-buffer-definitions (buffer)
  "Forget BUFFER, killed: the places \\[xref-go-back] and \\[xref-go-forward] go to
in it, and the definitions evaluated from it, so that its text is not kept for
them."
  (flet ((alive (markers) (remove buffer markers :key #'car)))
    (setf *xref-back* (alive *xref-back*)
          *xref-forward* (alive *xref-forward*)))
  (let ((emptied '()))
    (maphash (lambda (name buffers)
               (when (member buffer buffers)
                 (let ((left (remove buffer buffers)))
                   (if left
                       (setf (gethash name *evaluated-definitions*) left)
                       (push name emptied)))))
             *evaluated-definitions*)
    (dolist (name emptied) (remhash name *evaluated-definitions*))))

(defun install-xref ()
  "Bind \\`M-.' \\`M-,' and \\`C-M-,' everywhere, and the keys of *xref*;
forget a buffer's places and definitions when it is killed."
  (pushnew 'forget-killed-buffer-definitions tuatara.buffer:*kill-buffer-functions*)
  (define-key *global-keymap* "M-." 'xref-find-definitions)
  (define-key *global-keymap* "M-," 'xref-go-back)
  (define-key *global-keymap* "C-M-," 'xref-go-forward)
  (entry-list-keys 'xref-mode "the list of definitions (*xref*)"))
