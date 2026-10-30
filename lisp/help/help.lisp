;;;; lisp/help/help.lisp --- Self-documentation commands: describe keys, commands, variables; apropos.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Everything a user can name is documented in the image;
;;;; these commands show it in the *Help* buffer, a read-only buffer with its own
;;;; keymap (q closes its window, or returns to what the window showed) and
;;;; the first "special" buffer.

(in-package #:tuatara.defaults)

(define-major-mode help-mode special-mode
  "Documentation shown by the help commands, read-only; \\<help-mode>\\[quit-window]
closes the help window, or shows again what its window showed before. An
underlined name is a link to its description: \\[push-button] on it, or a click,
shows that; \\[forward-button] and \\[backward-button] go from one link to the next."
  :lighter "Help" :interactive nil)

(defvar *help-keymap* (mode-keymap 'help-mode)
  "Keys of the *Help* and *Apropos* buffers (help-mode's keymap):
\\<help-mode>\\[quit-window] goes back, \\[push-button] follows a link.")

(defvar *help-window-select* nil
  "True to move to the window a help buffer is shown in (\\[describe-function],
\\[describe-key] and the rest); \\<help-mode>\\[quit-window] there then
closes it and goes back; NIL (the standard) leaves you where you were.")

(defun show-help-pieces (pieces &key (name "*Help*"))
  "Show PIECES (as PIECES-TEXT takes them) in the buffer called NAME, *Help*
unless given, read-only, from its start, where the rules of where buffers are
shown put help (another window, as they are made). The keys named show in
:help-key-binding; a piece with an action is a link, which
\\<help-mode>\\[push-button] on it, or a click, follows. PIECES may be a
function of no arguments giving them, called again when \\[revert-buffer]
makes the help again; given as they are, they are shown again as they are.
With *HELP-WINDOW-SELECT* its window is selected."
  (let ((help (buffer-in-mode name 'help-mode)))
    (setf (tuatara.buffer:buffer-local help :kind) :help)
    (render-pieces help (if (functionp pieces) pieces (constantly pieces)) :keys t :point :start)
    (let ((window (display-buffer help :at :start)))
      (when (and window *help-window-select*) (select-window window))
      window)))

(defun show-help (text &key fill)
  "Show TEXT in the *Help* buffer (SHOW-HELP-PIECES), the keys it names
\(SUBSTITUTE-COMMAND-KEYS) in :help-key-binding. With FILL, TEXT is written
as documentation is (WRITE-DOCUMENTATION): the lines of each paragraph
filled to *HELP-COLUMN*, a line beginning - or * an item of a list, and a
line set in, a table's or an example's, kept as written. TEXT may be a
function of no arguments giving it, called again when \\[revert-buffer]
makes the help again."
  (flet ((made (text)
           (if fill (with-output-to-string (out) (write-documentation out text)) text)))
    (show-help-pieces (if (functionp text)
                          (lambda () (list (list (made (funcall text)))))
                          (list (list (made text)))))))

(defmethod mode-syntax-highlights ((mode help-mode) buffer rows)
  "The colours of help: its first line, what is described, as a heading, and
the keys it names as keys."
  (append (loop for row across rows
                when (and (zerop (tuatara.display:row-start row)) (not (tuatara.display:row-continuation-p row)))
                  return (list (list 0 (tuatara.display:row-shown-end row) :heading)))
          (made-spans-shown buffer rows)))

(defun command-bindings-text (symbol)
  "The keys that run command SYMBOL, marked as keys (MARK-KEY), or a note that it has none."
  (let ((keys (where-is symbol (active-keymaps *editor*))))
    (if keys
        (format nil "~{~a~^, ~}" (mapcar #'mark-key keys))
        (format nil "not bound to any key; use ~a" (mark-key (format nil "M-x ~(~a~)" symbol))))))

(defun documentation-text (symbol kind)
  "SYMBOL's documentation of KIND (FUNCTION, VARIABLE) filled to *HELP-COLUMN*
\(WRITE-DOCUMENTATION), the keys it names written in and marked
\(SUBSTITUTE-COMMAND-KEYS), or (no documentation)."
  (let ((doc (documentation symbol kind)))
    (if doc
        (string-right-trim '(#\Newline) (with-output-to-string (out) (write-documentation out doc)))
        "(no documentation)")))

(defun documentation-field (doc width)
  "The first sentence of DOC, a docstring, as a field beside a candidate (see
CANDIDATE) WIDTH wide, in :completion-annotation, the keys it names written
in and in :help-key-binding."
  (multiple-value-bind (text keys) (unmark-keys (first-sentence (and doc (substitute-command-keys doc :marks t))))
    (if (null keys)
        (list text :completion-annotation width)
        (list text
              (let ((spans '()) (at 0))
                (loop for (start end) in keys
                      do (when (< at start) (push (list at start :completion-annotation) spans))
                         (push (list start end :help-key-binding) spans)
                         (setf at end))
                (when (< at (length text)) (push (list at (length text) :completion-annotation) spans))
                (nreverse spans))
              width))))

(defun source-file-words (file)
  "FILE, a source file of Tuatara, as help names it: its path within the
source tree (lisp/search/files.lisp), never the folders above it."
  (let* ((path (pathname file))
         (directory (rest (pathname-directory path)))
         (top (position-if (lambda (name) (member name '("src" "lisp" "frontends" "test" "tools") :test #'equal))
                           directory)))
    (format nil "~{~a/~}~a" (if top (subseq directory top) '()) (file-namestring path))))

(defun visit-source (symbol)
  "Show where SYMBOL is defined in another window, the cursor on the
definition, as \\[xref-find-definitions] finds it; defined in more than one
place, list them in *xref*."
  (let ((definitions (lisp-definitions symbol)))
    (cond ((null definitions) (message "No definitions found for: ~a" (lisp-name-text symbol)))
          ((rest definitions) (show-definitions (qualified-name symbol) definitions))
          (t (go-to-definition (first definitions) :other t)))))

(defun source-location-pieces (symbol kind)
  "Where SYMBOL's definition of KIND (:function, :variable) is, as help
pieces: Defined in, then its file within the source tree (SOURCE-FILE-WORDS)
as a link that shows it there (VISIT-SOURCE); NIL when it is not known. A
function the init file defined is in the init file (INIT-FILE-DEFINITION);
others are where SBCL's source records say."
  (let* ((init (and (eq kind :function) (init-file-definition symbol)))
         (sources (and (not init) (ignore-errors (sb-introspect:find-definition-sources-by-name symbol kind))))
         (file (if init
                   (first init)
                   (and sources (sb-introspect:definition-source-pathname (first sources))))))
    (when file
      (list (format nil "~%Defined in ")
            (list (source-file-words file) :button (lambda () (visit-source symbol)))))))

(defun describe-command-pieces (symbol)
  "The help for command SYMBOL, as pieces (SHOW-HELP-PIECES)."
  (append (list (format nil "~(~a~) is a command.~%~%Keys: ~a~%" symbol (command-bindings-text symbol)))
          (source-location-pieces symbol :function)
          (list (format nil "~%~a~%" (documentation-text symbol 'function)))))

(define-command describe-command ()
  "Show what a command does, which keys run it and where it is defined, the
file a link that shows the definition. Every command is offered, those made
for another kind of buffer too."
  (let ((chosen (completing-read "Describe command: " (command-candidates :everywhere t) :require-match t)))
    (when (symbolp chosen)
      (show-help-pieces (describe-command-pieces chosen)))))

(defun function-kind (symbol)
  "What SYMBOL names as an operator, in words: a command, a special operator, a
macro, a generic function or a function."
  (cond ((gethash symbol *commands*) "command")
        ((special-operator-p symbol) "special operator")
        ((macro-function symbol) "macro")
        ((typep (fdefinition symbol) 'generic-function) "generic function")
        (t "function")))

(defun editor-packages ()
  "The editor's own packages, TUATARA and those whose names begin TUATARA.,
TUATARA-USER less."
  (remove-if-not (lambda (p)
                   (let ((name (package-name p)))
                     (and (not (string= name "TUATARA-USER"))
                          (or (string= name "TUATARA") (and (> (length name) 8) (string= "TUATARA." name :end2 8))))))
                 (list-all-packages)))

(defun function-candidates ()
  "Completion candidates for the operators one can name: those the Lisp one
types sees (Common Lisp's and TUATARA-USER's) and the commands by their names,
the editor's packages' others by package:name; each with what it is and the
first sentence of its documentation beside it."
  (let ((seen (make-hash-table :test 'eq)) (candidates '()))
    (flet ((add (symbol name)
             (unless (gethash symbol seen)
               (setf (gethash symbol seen) t)
               (push (make-candidate name
                                     :annotation (list (list (function-kind symbol) :completion-annotation 18)
                                                       (documentation-field (documentation symbol 'function) 1/2))
                                     :value symbol)
                     candidates))))
      (let ((user (find-package :tuatara-user)))
        (do-symbols (s user)
          (when (fboundp s) (add s (string-downcase (symbol-name s)))))
        (maphash (lambda (s c) (declare (ignore c)) (add s (string-downcase (symbol-name s)))) *commands*)
        (dolist (p (editor-packages))
          (do-external-symbols (s p)
            (when (and (fboundp s) (not (eq (find-symbol (symbol-name s) user) s)))
              (add s (format nil "~(~a:~a~)" (package-name p) (symbol-name s))))))))
    (sort candidates #'string< :key #'candidate-string)))

(defun describe-function-pieces (symbol)
  "The help for the operator SYMBOL, as pieces (SHOW-HELP-PIECES): what it is
and where, its parameters, what it does, and its source file; a command's as
describe-command gives it, with its keys."
  (if (gethash symbol *commands*)
      (describe-command-pieces symbol)
      (let ((kind (function-kind symbol)))
        (append (list (format nil "~(~a~) is a~:[~;n~] ~a in ~a.~%~%~@[~a~%~%~]~a~%"
                              symbol (find (char kind 0) "aeiou") kind (package-name (symbol-package symbol))
                              (lisp-signature symbol)
                              (documentation-text symbol 'function)))
                (source-location-pieces symbol :function)
                (list (string #\Newline))))))

(define-command describe-function ()
  "Show what a function, macro or command does: its parameters, its
documentation, the keys that run it when it is a command, and where it is
defined, the file a link that shows the definition. The one named at point in a Lisp buffer is the default, offered
first: \\<minibuffer>\\[minibuffer-done] alone describes it."
  (let* ((b (current-buffer))
         (here (and (typep (buffer-major-mode b) 'lisp-mode)
                    (let ((name (ignore-errors (symbol-at-point b))))
                      (and name (let ((symbol (lisp-symbol-named name (lisp-package b))))
                                  (and symbol (fboundp symbol) symbol))))))
         (chosen (completing-read "Describe function: " (function-candidates)
                                  :require-match t :default here)))
    (when (and (symbolp chosen) chosen)
      (show-help-pieces (describe-function-pieces chosen)))))

(define-command describe-key ()
  "Read a key sequence and show the command it runs and that command's
documentation, the question taken away once the keys are read."
  (let ((specs '()))
    (loop
      (let* ((key (read-key (format nil "Describe key: ~@[~a ~]" (and specs (tuatara.frontend:key-sequence-description specs)))))
             (spec (tuatara.frontend:key-spec key))
             (binding (progn (setf specs (append specs (list spec)))
                             (resolve-keys *editor* specs))))
        (unless (keymap-p binding) (setf (editor-message *editor*) nil))
        (cond ((keymap-p binding))          ; a prefix: keep reading
              ((null binding)
               (show-help (format nil "~a is undefined.~%"
                                  (mark-key (tuatara.frontend:key-sequence-description specs))))
               (return))
              (t (show-help-pieces (cons (format nil "~a runs the command ~(~a~).~%~%"
                                                 (mark-key (tuatara.frontend:key-sequence-description specs))
                                                 binding)
                                         (describe-command-pieces binding)))
                 (return)))))))

(defun tuatara-variables ()
  "All special variables of the editor's packages (EDITOR-PACKAGES) and
TUATARA-USER, as symbols, sorted by name."
  (let ((symbols '()))
    (dolist (p (cons (find-package :tuatara-user) (editor-packages)))
      (do-symbols (s p)
        (when (and (eq (symbol-package s) p) (boundp s) (eq (sb-int:info :variable :kind s) :special)
                   (not (constantp s)))
          (push s symbols))))
    (sort (remove-duplicates symbols) #'string< :key #'symbol-name)))

(defun variable-candidates ()
  "Completion candidates for the editor's variables, each with the first sentence of its documentation beside it."
  (mapcar (lambda (s)
            (make-candidate (format nil "~(~a::~a~)" (package-name (symbol-package s)) (symbol-name s))
                            :annotation (list (documentation-field (documentation s 'variable) 1/2))
                            :value s))
          (tuatara-variables)))

(defparameter *variable-value-shown* 4000
  "The most characters of a variable's value \\[describe-variable] shows; past
them the value is cut short, and said to be.")

(defun variable-value-text (value)
  "VALUE written as \\[describe-variable] shows it: lists and vectors to 50
items and 8 levels deep, at most *VARIABLE-VALUE-SHOWN* characters, a value
cut short said to be."
  (let ((text (let ((*print-length* 50) (*print-level* 8) (*print-circle* t) (*print-lines* 200))
                (prin1-to-string value))))
    (if (> (length text) *variable-value-shown*)
        (format nil "~a ... (cut short)" (subseq text 0 *variable-value-shown*))
        text)))

(defun describe-variable-pieces (symbol)
  "The help for the variable SYMBOL, as pieces (SHOW-HELP-PIECES): its value
\(VARIABLE-VALUE-TEXT), its documentation and its source file."
  (let ((source (source-location-pieces symbol :variable)))
    (append (list (format nil "~(~a::~a~) is a variable.~%~%Value: ~a~%~%~a~%"
                          (package-name (symbol-package symbol)) symbol (variable-value-text (symbol-value symbol))
                          (documentation-text symbol 'variable)))
            source
            (and source (list (string #\Newline))))))

(define-command describe-variable ()
  "Show a variable's value and documentation, and where it is defined, the
file a link that shows the definition."
  (let ((chosen (completing-read "Describe variable: " (variable-candidates) :require-match t)))
    (when (symbolp chosen)
      (show-help-pieces (lambda () (describe-variable-pieces chosen))))))

(define-command apropos-command ()
  "List the commands and variables whose names or documentation match the
words typed, in *Apropos*. Each name is a link to its description:
\\<help-mode>\\[push-button] on it, or a click, shows that in *Help*;
\\[forward-button] and \\[backward-button] go from one link to the next."
  (let ((pattern (read-string "Apropos (words, any order): ")))
    (unless (zerop (length pattern))
      (let ((commands (loop for s being the hash-keys of *commands*
                            for name = (string-downcase (symbol-name s))
                            when (candidate-score pattern (make-candidate (format nil "~a ~a" name (first-sentence (documentation s 'function)))))
                              collect s))
            (variables (loop for s in (tuatara-variables)
                             when (candidate-score pattern (make-candidate (format nil "~(~a~) ~a" s (first-sentence (documentation s 'variable)))))
                               collect s)))
        (show-help-pieces
         (collecting-pieces (put)
           (put (format nil "Apropos ~a~%~%Commands:~%" pattern))
           (dolist (s (sort commands #'string< :key #'symbol-name))
             (let ((s s) (name (string-downcase (symbol-name s))))
               (put "  ")
               (put name :button (lambda () (show-help-pieces (describe-command-pieces s))))
               ;; the keys from the 31st column
               (put (format nil "~a~a~%    ~a~%" (column-text "" (max 1 (- 28 (text-columns name)))) (command-bindings-text s)
                            (first-sentence (documentation-text s 'function))))))
           (put (format nil "~%Variables:~%"))
           (dolist (s variables)
             (let ((s s))
               (put "  ")
               (put (format nil "~(~a::~a~)" (package-name (symbol-package s)) s) :button
                    (lambda () (show-help-pieces (lambda () (describe-variable-pieces s)))))
               (put (format nil "~%    ~a~%" (first-sentence (documentation-text s 'variable)))))))
         :name "*Apropos*")))))

(defun key-line (out indent keys what &key marked)
  "Write to OUT a line of a list of keys: INDENT, the KEYS (a string), marked as
keys (MARK-KEY) when MARKED, then WHAT they run from the 25th column after
INDENT on, at least a space after the keys."
  ;; padded by the keys' own length: the marks take no room
  (format out "~a~a~a~a~%" indent (if marked (mark-key keys) keys) (column-text "" (max 1 (- 22 (text-columns keys)))) what))

(define-command describe-bindings ()
  "List every key binding in effect, keymap by keymap, each followed by the
keymaps it falls through to. A key that does nothing here, as one a mode
takes away, is listed as (none here), in the mode's keymap and in those
under it."
  (show-help
   (with-output-to-string (out)
     (dolist (map (remove-duplicates (loop for active in (active-keymaps *editor*)
                                           append (loop for map = active then (keymap-parent map)
                                                        while map collect map))
                                     :from-end t))
       (format out "~a keymap:~%" (or (keymap-name map) "unnamed"))
       (labels ((walk (map prefix)
                  (dolist (entry (sort (keymap-entries map :own t) #'string< :key (lambda (e) (tuatara.frontend:key-description (car e)))))
                    (let* ((keys (append prefix (list (car entry))))
                           (key (tuatara.frontend:key-sequence-description keys)))
                      (cond ((keymap-p (cdr entry)) (walk (cdr entry) keys))
                            (t (key-line out "  " key
                                         ;; a key a mode takes away, here and in the keymaps under it, does nothing
                                         (if (or (eq (cdr entry) :undefined) (null (resolve-keys *editor* keys)))
                                             "(none here)"
                                             (string-downcase (princ-to-string (cdr entry))))
                                         :marked t)))))))
         (walk map '()))
       (terpri out)))))

(defun prefix-keys-text (keymaps prefix)
  "Every key under PREFIX (key specs) in KEYMAPS, consulted in order as the
key hints do (PREFIX-ENTRIES), as text: a line a key with what it runs, a
prefix's keys under its name, indented."
  (with-output-to-string (out)
    (labels ((walk (prefix depth)
               (loop for (key . name) in (prefix-entries keymaps prefix)
                     for keys = (tuatara.frontend:key-sequence-description (append prefix (tuatara.frontend:parse-key-sequence key)))
                     for indent = (make-string (* 2 depth) :initial-element #\Space)
                     do (if (uiop:string-prefix-p "+" name)
                            (progn (format out "~%~a~a  ~a~%" indent keys name)
                                   (walk (append prefix (tuatara.frontend:parse-key-sequence key)) (1+ depth))
                                   (terpri out))
                            (key-line out indent keys name)))))
      (walk prefix 1))))

(defun describe-prefix-keys (editor prefix)
  "Show in *Help* every key under PREFIX (key specs) where the cursor is:
\\`C-h' after a prefix key (\\`C-x C-h', \\`SPC C-h')."
  (let* ((keymaps (active-keymaps editor))
         (name (tuatara.frontend:key-sequence-description prefix))
         (text (prefix-keys-text keymaps prefix)))
    (show-help (format nil "Keys under ~a~%~a" name text))))

(defun messages-in-special-mode (buffer)
  "Put BUFFER, *Messages*, in special mode, a modal support buffer, unless it
is in it already."
  (unless (typep (buffer-major-mode buffer) 'special-mode)
    (set-major-mode buffer 'special-mode)))

(define-command tuatara-version ()
  "Say which Tuatara this is: its version, the commit it was built from and
when, the SBCL it runs on, and where it shows (a window, the terminal). With
\\[universal-argument], put that at point instead."
  (let ((line (tuatara:version-line (tuatara.frontend:frontend-description (editor-frontend *editor*)))))
    (if (prefix-argument)
        (goto (tuatara.buffer:buffer-insert (current-buffer) (point) line))
        (message "~a" line))
    line))

(define-command version ()
  "Say which Tuatara this is, as \\[tuatara-version] does."
  (tuatara-version))

(define-command view-echo-area-messages ()
  "Show the *Messages* buffer: the messages shown in the echo area so far,
repeats counted, the latest at the end. It goes where place-buffers puts it: as
standard, in a window across the bottom, 30% of the height, which is selected
and which \\<special-mode>\\[quit-window] closes."
  (display-buffer (messages-buffer) :at :end))

(defun install-help ()
  "Bind the help keys; \\`C-h' after a prefix key lists the keys under it;
*Messages* in special mode, made so again when it has been killed."
  (buttons-in-mode 'help-mode "*Help* and *Apropos*")
  (setf *prefix-help-function* 'describe-prefix-keys
        *menu-help-function* 'show-help)
  (define-key *global-keymap* "C-h k" 'describe-key)
  (define-key *global-keymap* "C-h f" 'describe-function)
  (define-key *global-keymap* "C-h x" 'describe-command)
  (define-key *global-keymap* "C-h v" 'describe-variable)
  (define-key *global-keymap* "C-h a" 'apropos-command)
  (define-key *global-keymap* "C-h b" 'describe-bindings)
  (define-key *global-keymap* "C-h e" 'view-echo-area-messages)
  (setf *messages-made-function* 'messages-in-special-mode)
  (messages-in-special-mode (messages-buffer)))
