;;;; lisp/help/help.lisp --- Self-documentation commands: describe keys, commands, variables; apropos.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Everything a user can name is documented in the image;
;;;; these commands show it in the *Help* buffer, a read-only buffer with its own
;;;; keymap (q closes its window, or returns to what the window showed) and
;;;; the first "special" buffer.

(in-package #:tuatara.defaults)

;;; Help text filled

(defparameter *help-column* 72
  "The column the lines of help made of words are filled to: a command's,
a function's or a variable's documentation, and the help on settings.")

(defun marked-words (text)
  "The words of TEXT, split at spaces and line ends; a key written into it
between its marks (SUBSTITUTE-COMMAND-KEYS) is one word, spaces and all."
  (let ((words '()) (word (make-string-output-stream)) (open nil))
    (flet ((finish ()
             (let ((w (get-output-stream-string word)))
               (when (plusp (length w)) (push w words)))))
      (loop for c across text
            do (cond ((and (not open) (member c '(#\Space #\Newline))) (finish))
                     (t (when (key-mark-p c) (setf open (not open)))
                        (write-char c word))))
      (finish))
    (nreverse words)))

(defun write-filled (stream head text &key (indent (length head)) (column *help-column*))
  "Write HEAD, then the words of TEXT filled after it into lines no longer than
COLUMN (TUATARA.DISPLAY:FILL-WORDS), the lines after the first set in by
INDENT, to STREAM. The keys TEXT names are written in and marked to show as
keys \(SUBSTITUTE-COMMAND-KEYS), each word as wide as it is shown. A word
longer than a line has a line of its own."
  (let* ((gap (and (plusp (length head)) (char/= (char head (1- (length head))) #\Space)))
         (lines (tuatara.display:fill-words (marked-words (substitute-command-keys text :marks t)) column
                                            :start (+ (length head) (if gap 1 0)) :indent indent
                                            :width (lambda (c)
                                                     (if (key-mark-p c) 0 (tuatara.display:char-columns c))))))
    (write-string head stream)
    (when (and gap lines) (write-char #\Space stream))
    (format stream "~{~a~^~%~}" (cons (or (first lines) "")
                                      (mapcar (lambda (line) (format nil "~va~a" indent "" line)) (rest lines))))
    (terpri stream)))

(defun write-help-section (stream heading &key intro example rows (width 26))
  "Write one section of a help text to STREAM, in the layout the commands that
say what is written in a file share: HEADING on a line of its own; INTRO, a
paragraph filled to *HELP-COLUMN* (WRITE-FILLED); EXAMPLE, lines set in as they
are; then ROWS, a flat list of what is written and what it does, each written
in a column WIDTH wide with its words filled beside it, later lines kept under
them; what is written too long for the column has a line of its own, the
words under the second column. Then a blank line.
Any part may be NIL. The keys the words name are shown as keys."
  (when heading (format stream "~a~%" heading))
  (when (and heading (or intro example)) (terpri stream))
  (when intro (write-filled stream "" intro))
  (when example
    (when intro (terpri stream))
    (dolist (line (uiop:split-string example :separator '(#\Newline)))
      (format stream "  ~a~%" line)))
  (when (and rows (or intro example)) (terpri stream))
  (loop for (written words) on rows by #'cddr
        do (if (> (length written) width)
               (progn (format stream "  ~a~%" written)
                      (write-filled stream (make-string (+ width 3) :initial-element #\Space) words))
               (write-filled stream (format nil "  ~va " width written) words :indent (+ width 3))))
  (terpri stream))

(defun write-documentation (stream doc &key (column *help-column*))
  "Write DOC, a docstring, to STREAM filled to COLUMN (WRITE-FILLED), its parts
as TUATARA.DISPLAY:DOCSTRING-PARAGRAPHS finds them, as the completion popup
shows them too. The lines of a paragraph are joined and filled. A line
beginning at its start with - or * or a number and a full stop begins a list
item. The lines after it are its words until a blank line or the next item,
filled under its first word. Any other line is set in (a table, an example;
under an item, set in further than its words), and the blank lines are kept as
written."
  (let ((keymap ""))
    (flet ((keep-keymap (line)
             ;; a \\<KEYMAP> names the keymap for the keys after it, in the later parts too
             (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "\\\\<([^>]+)>" line)
               (when match (setf keymap (format nil "\\<~a>" (aref groups 0)))))))
      (dolist (part (tuatara.display:docstring-paragraphs doc))
        (ecase (first part)
          (:blank (terpri stream))
          (:as-is (let ((line (second part)))
                    (write-line (substitute-command-keys (concatenate 'string keymap line) :marks t) stream)
                    (keep-keymap line)))
          (:filled (destructuring-bind (head indent words lines) (rest part)
                     (write-filled stream head (format nil "~a~{~a~^ ~}" keymap words) :indent indent :column column)
                     (mapc #'keep-keymap lines))))))))

(define-major-mode help-mode special-mode
  "Documentation shown by the help commands, read-only; \\<help-mode>\\[quit-window]
closes the help window, or shows again what its window showed before. An
underlined name is a link to its description: \\[push-button] on it, or a click,
shows that; \\[forward-button] and \\[backward-button] go from one link to the next."
  :lighter "Help" :interactive nil)

(defmethod mode-enter ((mode help-mode) buffer)
  "Have the rows a long line of help wraps into continue under their own text:
an item's words, a table's second column (SET-HANGING)."
  (set-hanging buffer :cells)
  (call-next-method))

(defmethod mode-exit ((mode help-mode) buffer)
  "Let go of the hanging rows help has (SET-HANGING)."
  (set-hanging buffer nil)
  (call-next-method))

(defvar *help-keymap* (mode-keymap 'help-mode)
  "Keys of the *Help* and *Apropos* buffers (help-mode's keymap):
\\<help-mode>\\[quit-window] goes back, \\[push-button] follows a link.")

(define-setting *help-window-select* nil
  "Whether you move to the window a help buffer is shown in
\(\\[describe-function], \\[describe-key] and the rest).
- t: you move to it; \\<help-mode>\\[quit-window] there then closes it and
  goes back.
- nil: you stay where you were, as standard."
  :values boolean)

(defun show-help-pieces (pieces &key (name "*Help*") (mode 'help-mode))
  "Show PIECES (as PIECES-TEXT takes them) in the buffer called NAME, *Help*
unless given, read-only, from its start, where the rules of where buffers are
shown put help (another window, as they are made). The keys named show in
:help-key-binding; a piece with an action is a link, which
\\<help-mode>\\[push-button] on it, or a click, follows. PIECES may be a
function of no arguments giving them, called again when \\[revert-buffer]
makes the help again; given as they are, they are shown again as they are.
With *HELP-WINDOW-SELECT* its window is selected. MODE is the buffer's major
mode, one made from HELP-MODE."
  (let ((help (buffer-in-mode name mode)))
    (setf (buffer-kind help) :help)
    (render-pieces help (if (functionp pieces) pieces (constantly pieces)) :keys t :point :start)
    (let ((window (display-buffer help :at :start)))
      (when (and window *help-window-select*) (select-window window))
      window)))

(defun call-with-details-shown (details function &key (name "*Question*"))
  "Call FUNCTION, which asks a question at a short prompt, with DETAILS, words
or pieces too long for that, in a buffer called NAME at the bottom of the
frame, as tall as they need, long lines wrapped. It goes when the question
ends, however that is. With no editor running, FUNCTION alone. FUNCTION's
values."
  (unless *editor*
    (return-from call-with-details-shown (funcall function)))
  (let ((buffer (buffer-in-mode name 'help-mode)))
    (setf (buffer-kind buffer) :help)
    (render-pieces buffer (constantly (if (listp details) details (list details))) :point :start)
    (let ((window (display-buffer-at buffer :bottom :size :fit)))
      (when window
        (setf (tuatara.display:window-start window) 0 (tuatara.display:window-point window) 0)
        ;; as tall as its lines are once wrapped, up to half the screen
        (let ((needed (length (tuatara.display:window-layout-rows
                               window 0 (floor (tuatara.display:area-height (tuatara.display:area-root window)) 2))))
              (shown (- (tuatara.display:area-height window) 1 (tuatara.display:window-header-rows window))))
          (tuatara.display:resize-area window (- needed shown) :stacked)))
      (unwind-protect (funcall function)
        (when (and window (member window (editor-windows *editor*))) (leave-window window))
        (tuatara.buffer:kill-buffer buffer)))))

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
  (list (loop for row across rows
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

(defun documentation-summary (symbol kind)
  "The first sentence of SYMBOL's documentation of KIND (FUNCTION, VARIABLE),
the keys it names written in; (no documentation) when there is none. Only that
sentence is worked on, however long the documentation is."
  (let ((doc (documentation symbol kind)))
    (if doc
        (substitute-command-keys (first-sentence doc))
        "(no documentation)")))

(defun documentation-field (doc width)
  "The first sentence of DOC, a docstring, as a field beside a candidate (see
CANDIDATE) WIDTH wide, in :completion-annotation, the keys it names written
in and in :help-key-binding."
  (multiple-value-bind (text keys) (unmark-keys (substitute-command-keys (first-sentence doc) :marks t))
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

(defun source-file-in-tree (file)
  "Where FILE, a source file of Tuatara as the compiler recorded it, is found
on this machine, and its path within the source tree, as two values. The copy
looked for is in source/ beside the program, as a release has it, or in the
source tree. The directories above the tree are whatever the build had, so each
path FILE may have within it is tried, the longest first. With no copy here,
NIL and the longest path; for a file outside any tree, NIL and NIL."
  (let* ((path (pathname file))
         (tails (loop for directories on (rest (pathname-directory path))
                      when (member (first directories) '("src" "lisp" "frontends" "test" "tools") :test #'string=)
                      collect (format nil "~{~a/~}~a" directories (file-namestring path))))
         (roots (tuatara.runtime:resource-directories "source/" "" :existing t)))
    (dolist (tail tails (values nil (first tails)))
      (dolist (root roots)
        (let ((found (probe-file (uiop:merge-pathnames* (uiop:parse-native-namestring tail) root))))
          (when found
            (return-from source-file-in-tree (values found tail))))))))

(defun source-file-here (file)
  "The name of FILE, a source file as the compiler recorded it, on this
machine: FILE when it is there, else its copy (SOURCE-FILE-IN-TREE), else FILE."
  (or (and (not (probe-file file))
           (let ((found (source-file-in-tree file)))
             (and found (namestring found))))
      file))

(defun source-file-words (file)
  "FILE, a source file of Tuatara, as help names it: its path within the
source tree (lisp/search/files.lisp), never the directories above it."
  (or (nth-value 1 (source-file-in-tree file))
      (file-namestring (pathname file))))

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
others are where Steel Bank Common Lisp (SBCL)'s source records say."
  (let* ((init (and (eq kind :function) (init-file-definition symbol)))
         (sources (and (not init)
                       (ignoring-errors-reported ("finding where ~s is defined" symbol)
                                                 (sb-introspect:find-definition-sources-by-name symbol kind))))
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
first: \\<minibuffer>\\[minibuffer-finish] alone describes it."
  (let* ((b (current-buffer))
         (here (and (typep (buffer-major-mode b) 'lisp-mode)
                    (let ((name (ignoring-errors-reported ("reading the name at point") (symbol-at-point b))))
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
     (let* ((key (read-key (format nil
                                   "Describe key: ~@[~a ~]"
                                   (and specs (tuatara.frontend:key-sequence-description specs)))))
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
\(VARIABLE-VALUE-TEXT), for a setting the values it takes, its documentation
and its source file."
  (let ((source (source-location-pieces symbol :variable))
        (values (setting-values-words symbol)))
    (append (list (format nil "~(~a::~a~) is a variable.~%~%Value: ~a~%~@[~a~]~%~a~%"
                          (package-name (symbol-package symbol)) symbol (variable-value-text (symbol-value symbol))
                          (and values (with-output-to-string (out) (write-filled out "Values: " values)))
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
                            when (candidate-score pattern (make-candidate
                                                           (format nil "~a ~a" name
                                                                   (first-sentence (documentation s 'function)))))
                            collect s))
            (variables (loop for s in (tuatara-variables)
                             when (candidate-score pattern (make-candidate
                                                            (format nil "~(~a~) ~a" s
                                                                    (first-sentence (documentation s 'variable)))))
                             collect s)))
        (show-help-pieces
         (collecting-pieces (put)
                            (put (format nil "Apropos ~a~%~%Commands:~%" pattern))
                            (dolist (s (sort commands #'string< :key #'symbol-name))
                              (let ((s s) (name (string-downcase (symbol-name s))))
                                (put "  ")
                                (put name :button (lambda () (show-help-pieces (describe-command-pieces s))))
                                ;; the keys from the 31st column
                                (put (format nil
                                             "~a~a~%    ~a~%"
                                             (column-text "" (max 1 (- 28 (text-columns name))))
                                             (command-bindings-text s)
                                             (documentation-summary s 'function)))))
                            (put (format nil "~%Variables:~%"))
                            (dolist (s variables)
                              (let ((s s))
                                (put "  ")
                                (put (format nil "~(~a::~a~)" (package-name (symbol-package s)) s) :button
                                     (lambda () (show-help-pieces (lambda () (describe-variable-pieces s)))))
                                (put (format nil "~%    ~a~%" (documentation-summary s 'variable))))))
         :name "*Apropos*")))))

(defun key-line (out indent keys what &key marked)
  "Write to OUT a line of a list of keys: INDENT, the KEYS (a string), marked as
keys (MARK-KEY) when MARKED, then WHAT they run from the 25th column after
INDENT on, at least a space after the keys."
  ;; padded by the keys' own length: the marks take no room
  (format out "~a~a~a~a~%" indent (if marked (mark-key keys) keys)
          (column-text "" (max 1 (- 22 (text-columns keys)))) what))

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
                  (dolist (entry (sort (keymap-entries map :own t) #'string<
                                       :key (lambda (e) (tuatara.frontend:key-description (car e)))))
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
                     for keys = (tuatara.frontend:key-sequence-description
                                 (append prefix (tuatara.frontend:parse-key-sequence key)))
                     for indent = (spaces (* 2 depth))
                     do (if (uiop:string-prefix-p "+" name)
                            (progn (format out "~%~a~a  ~a~%" indent keys name)
                                   (walk (append prefix (tuatara.frontend:parse-key-sequence key)) (1+ depth))
                                   (terpri out))
                            (key-line out indent keys name)))))
      (walk prefix 1))))

(defmethod host-show-prefix-help ((host defaults-host) editor prefix)
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

(defmethod host-messages-made ((host defaults-host) buffer)
  "Put BUFFER, a log just made, in special mode (MESSAGES-IN-SPECIAL-MODE)."
  (messages-in-special-mode buffer))

(defmethod host-show-help ((host defaults-host) text)
  "Show TEXT in *Help* (SHOW-HELP)."
  (show-help text))

(defun say-or-insert (line)
  "Say LINE, or with \\[universal-argument] put it at point instead; LINE."
  (if (prefix-argument)
      (goto (tuatara.buffer:buffer-insert (current-buffer) (point) line))
      (message "~a" line))
  line)

(define-command tuatara-version ()
  "Say which Tuatara this is: its version, the commit it was built from and
when, the Steel Bank Common Lisp (SBCL) it runs on, and where it shows (a
window, the terminal). With \\[universal-argument], put that at point instead."
  (say-or-insert (tuatara:version-line (tuatara.frontend:frontend-description (editor-frontend *editor*)))))

(define-command view-echo-area-messages ()
  "Show the *Messages* buffer: the messages shown in the echo area so far,
repeats counted, the latest at the end. It goes where place-buffers puts it: as
standard, in a window across the bottom, 30% of the height, which is selected
and which \\<special-mode>\\[quit-window] closes."
  (display-buffer (messages-buffer) :at :end))

(defun install-help ()
  "Bind the help keys, and put *Messages* in special mode, as it is again
when it is made anew after it has been killed."
  (buttons-in-mode 'help-mode "*Help* and *Apropos*")
  (define-key *global-keymap* "C-h k" 'describe-key)
  (define-key *global-keymap* "C-h f" 'describe-function)
  (define-key *global-keymap* "C-h x" 'describe-command)
  (define-key *global-keymap* "C-h v" 'describe-variable)
  (define-key *global-keymap* "C-h a" 'apropos-command)
  (define-key *global-keymap* "C-h b" 'describe-bindings)
  (define-key *global-keymap* "C-h e" 'view-echo-area-messages)
  (messages-in-special-mode (messages-buffer)))
