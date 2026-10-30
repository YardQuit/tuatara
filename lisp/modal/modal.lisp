;;;; lisp/modal/modal.lisp --- Tuatara's modal editing: the NORMAL, INSERT and SUPPORT states.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The keys began as donkey.el's (YardQuit/donkey) and are Tuatara's own:
;;;; they change with Tuatara alone. The state keymap slot of the editor holds
;;;; the NORMAL or INSERT map for the current buffer; letters are claimed only
;;;; in NORMAL; every Control and Meta binding of the global map works in both
;;;; states. This layer holds the states, their keymaps and keys, the buffers'
;;;; classes, the cursor and the mode line's TUATARA[N], and the commands that
;;;; only switch state; the commands its keys run are core commands
;;;; (lisp/editing/). *modal-editing* NIL turns it off.

(in-package #:tuatara.defaults)

(define-command tuatara-insert ()
  "Insert at point, and enter INSERT state. Any selection is dropped, a
rectangle too; banked lines stay, \\<normal>\\[tuatara-change] being the key that acts on
them."
  (drop-selection)
  (set-modal-state :insert))

(define-command tuatara-normal ()
  "Return to NORMAL state; the selection is dropped, a list of completions
beside the cursor closes, and a line begun and left blank loses the blanks it
was indented with."
  (close-popup *editor*)
  (forget-typed-indentation (current-buffer))
  (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) nil)
  (line-selection-clear)
  (set-modal-state :normal))

(define-command tuatara-quit ()
  "\\[tuatara-quit]: in INSERT state return to NORMAL; in NORMAL drop the selection."
  (if (eq (modal-state) :insert)
      (tuatara-normal)
      (progn (line-selection-clear) (keyboard-quit))))

(define-command tuatara-insert-after ()
  "Insert after the character at point, and enter INSERT state. Any selection
is dropped first, as by \\<normal>\\[tuatara-insert]."
  (drop-selection) (char-motion 1) (tuatara-insert))

(define-command tuatara-insert-line-start ()
  "Enter INSERT state at the start of the line. Any selection is dropped
first, as by \\<normal>\\[tuatara-insert]."
  (drop-selection) (move-to-line-start) (tuatara-insert))

(define-command tuatara-insert-line-end ()
  "Enter INSERT state at the end of the line. Any selection is dropped first,
as by \\<normal>\\[tuatara-insert]."
  (drop-selection) (move-to-line-end) (tuatara-insert))

(define-command tuatara-open-below ()
  "Open a line below the current one and enter INSERT state on it, indented as
the language's mode indents it. With a count, that many lines: the cursor on
the first, the rest left blank below it."
  (let ((extra (1- (max 1 (prefix)))) (b (current-buffer)))
    ;; the count is the lines', not the line break's
    (setf (editor-prefix-argument *editor*) nil)
    (drop-selection)
    (move-to-line-end)
    (newline)
    (when (plusp extra)
      ;; the blank lines go after the cursor, which stays where it is
      (let ((p (point)))
        (tuatara.buffer:buffer-insert b p (with-output-to-string (out)
                                            (dotimes (i extra) (write-string (tuatara.buffer:buffer-newline b) out)))
                                      :group t)
        (goto p)))
    (tuatara-insert)))

(define-command tuatara-open-above ()
  "Open a line above the current one and enter INSERT state on it, indented as
the language's mode indents it (see *INDENT-NEW-LINES*). With a count, that
many lines: the cursor on the one beside the line it came from, the rest left
blank above it."
  (let ((extra (1- (max 1 (prefix)))) (b (current-buffer)))
    (setf (editor-prefix-argument *editor*) nil)
    (drop-selection)
    (move-to-line-start)
    (let ((*indent-new-lines* nil)) (newline))
    (char-motion -1)
    (indent-typed-line b :group t)
    (when (plusp extra)
      (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-line-start b (point))
                                    (with-output-to-string (out)
                                      (dotimes (i extra) (write-string (tuatara.buffer:buffer-newline b) out)))
                                    :group t)
      (move-to-line-end))
    (tuatara-insert)))

(define-command tuatara-change ()
  "Delete the selection (into the kill ring) or the character at point (not
saved) and enter INSERT state. A line selection keeps its final newline.
A rectangle goes to the rectangle store, where \\<normal>\\[yank-rectangle] pastes it from: one row high,
its block is emptied for INSERT state; taller, what to write on every row in
its place is read from the minibuffer."
  (if (rectangle-live-p (current-buffer))
      (change-selected-rectangle)
      (progn (kill-selection-or-chars)
             (tuatara-insert))))

(defvar *enter-dwim-functions* '()
  "Functions of a buffer asked in turn what \\`RET' in NORMAL state does there:
the first that did something returns true (Org follows a link).")

(define-command tuatara-normal-return ()
  "\\<normal>\\[tuatara-normal-return] in NORMAL state: what the buffer's mode does with it
(*enter-dwim-functions*), as following the Org link point is on; else, and
always in the hex view, nothing, so that it changes no text."
  (let ((buffer (current-buffer)))
    (unless (hex-view-here-p)
      (some (lambda (function) (funcall function buffer)) *enter-dwim-functions*))))

(define-command tuatara-normal-inert ()
  "Do nothing: NORMAL state keeps BACKSPACE and DELETE inert in editing buffers."
  nil)

(defvar *tuatara-normal-tab-modes* '(org-mode)
  "The major modes, by name, where \\<normal>\\[tuatara-normal-tab] in NORMAL state does what \\`TAB' does in
the mode, the modes derived from them too; in the others it does nothing, as
the keys that would type do nothing there. Org's alone unless set:
(setf *tuatara-normal-tab-modes* '()) makes \\[tuatara-normal-tab] do nothing in NORMAL
state everywhere.")

(define-command tuatara-normal-tab ()
  "\\<normal>\\[tuatara-normal-tab] in NORMAL state: in a buffer whose major mode is one of
*tuatara-normal-tab-modes* (Org), or derives from one, what \\`TAB' does in that
mode, as in INSERT state; in the hex view, from its hex digits to its text
or back (hex-switch-column); elsewhere nothing, so that it changes no text."
  (when (hex-view-here-p)
    ;; the hex view: from the hex digits to the text, or back
    (return-from tuatara-normal-tab (hex-switch-column)))
  (let ((b (current-buffer)))
    (when (mode-setting (buffer-major-mode b) (setting-list '*tuatara-normal-tab-modes* *tuatara-normal-tab-modes*))
      ;; the binding under NORMAL state's: the mode's own, else the global one
      (let ((command (loop for map in (list (tuatara.buffer:buffer-local b :keymap) (editor-global-keymap *editor*))
                           for binding = (and map (lookup-key map '((:tab))))
                           when (and binding (not (keymap-p binding)) (not (eq binding 'tuatara-normal-tab)))
                             return binding)))
        (when command
          (funcall (find-command command)))))))

;;; Mode line, cursor, keys

(sb-ext:defglobal **modal-mode-lines** (make-hash-table :test 'eq :weakness :key :synchronized t)
  "For each window, the modal mode line it showed last and what it was made
from: (LIGHTER LINE SPANS TEXT TEXT-SPANS).")

(defun modal-mode-line (editor &optional (window (editor-window editor)))
  "The modal lighter of WINDOW's buffer, then the default mode line:
N or I in editing buffers, S in support buffers, E where modal editing is off,
after TUATARA while *MODE-LINE-TUATARA*. The same text and spans as last time,
nothing made, while neither changed."
  (let* ((buffer (tuatara.display:window-buffer window))
         (lighter (cond ((not *modal-editing*) "")
                        (*mode-line-tuatara*
                         (case (buffer-modal-class buffer)
                           (:support " TUATARA[S]") (:excluded " TUATARA[E]")
                           (t (if (eq (modal-state buffer) :insert) " TUATARA[I]" " TUATARA[N]"))))
                        (t (case (buffer-modal-class buffer)
                             (:support " [S]") (:excluded " [E]")
                             (t (if (eq (modal-state buffer) :insert) " [I]" " [N]")))))))
    (multiple-value-bind (line spans) (default-mode-line editor window)
      (let ((last (gethash window **modal-mode-lines**)))
        (if (and last (eq (first last) lighter) (string= (second last) line) (equal (third last) spans))
            (values (fourth last) (fifth last))
            (let ((text (concatenate 'string lighter line))
                  (text-spans (loop for (start end face) in spans
                                    collect (list (+ start (length lighter)) (+ end (length lighter)) face))))
              (setf (gethash window **modal-mode-lines**) (list lighter line spans text text-spans))
              (values text text-spans)))))))

(define-command toggle-mode-line-tuatara ()
  "Write TUATARA before the modal state in the mode line, TUATARA[N], or the
state alone, [N]; *mode-line-tuatara* says which as Tuatara starts."
  (setf *mode-line-tuatara* (not *mode-line-tuatara*))
  (message "The mode line's state ~:[alone~;after TUATARA~]" *mode-line-tuatara*))

(defvar *tuatara-normal-cursor-shape* nil
  "The cursor's shape in NORMAL state: :block, :bar or :underline; NIL (as
standard) for *CURSOR-SHAPE*.")

(defvar *tuatara-normal-cursor-colour* nil
  "The cursor's colour in NORMAL state, as faces take colours; NIL (as
standard) for *CURSOR-COLOUR*.")

(defvar *tuatara-insert-cursor-shape* nil
  "The cursor's shape in INSERT state, while typing: :block, :bar or
:underline; NIL (as standard) for *CURSOR-SHAPE*.")

(defvar *tuatara-insert-cursor-colour* nil
  "The cursor's colour in INSERT state, as faces take colours; NIL (as
standard) for *CURSOR-COLOUR*.")

(defvar *tuatara-support-cursor-shape* nil
  "The cursor's shape in support buffers (lists, help, *Messages*): :block,
:bar or :underline; NIL (as standard) for *CURSOR-SHAPE*.")

(defvar *tuatara-support-cursor-colour* nil
  "The cursor's colour in support buffers, as faces take colours; NIL (as
standard) for *CURSOR-COLOUR*.")

(defun modal-cursor-shape (editor)
  "The cursor's shape and colour in the current buffer's modal state, as set
for it (*TUATARA-NORMAL-CURSOR-SHAPE* and the rest; NIL leaves the look for
all, *CURSOR-SHAPE* and *CURSOR-COLOUR*), and as a third value true in INSERT
state, while typing. Nothing in a buffer modal editing stays out of."
  (declare (ignore editor))
  (case (buffer-modal-class)
    (:excluded nil)
    (:support (values *tuatara-support-cursor-shape* *tuatara-support-cursor-colour* nil))
    (t (if (eq (modal-state) :insert)
           (values *tuatara-insert-cursor-shape* *tuatara-insert-cursor-colour* t)
           (values *tuatara-normal-cursor-shape* *tuatara-normal-cursor-colour* nil)))))

(defparameter *tuatara-normal-bindings*
  '(("h" backward-char) ("j" next-line) ("k" previous-line) ("l" forward-char)
    ("w" forward-word) ("b" backward-word) ("W" forward-sexp) ("B" backward-sexp)
    ("g g" beginning-of-buffer) ("g e" end-of-buffer)
    ("g h" beginning-of-line) ("g l" end-of-line) ("g j" join-selected-lines)
    ("i" tuatara-insert) ("a" tuatara-insert-after) ("I" tuatara-insert-line-start)
    ("A" tuatara-insert-line-end) ("o" tuatara-open-below) ("O" tuatara-open-above)
    ("d" delete-selection-or-char) ("D" kill-line) ("c" tuatara-change)
    ("y" copy-selection-or-char) ("p" yank-over-selection) ("P" yank-rectangle) ("u" undo) ("U" redo) ("." repeat)
    ("v" start-selection) ("V" select-lines) ("%" select-whole-buffer) ("M" mark-run-toggle)
    ("m v" rectangle-mark-mode)
    ("f" split-selection) ("t" add-cursor-below) ("T" add-cursor-above)
    ("m l" bank-selected-lines) ("m u" unbank-line) ("m U" unbank-section)
    ("m DEL" clear-banked-lines) ("m <delete>" clear-banked-lines)
    ("m I" select-sexp-inside) ("m A" select-sexp) ("C" toggle-comment-lines)
    ("m w" select-word) ("m W" select-symbol) ("m b" select-word-backward)
    ("m B" select-symbol-backward) ("m s" select-sentence) ("m S" select-sentence-backward)
    ("m p" select-paragraph) ("m P" select-paragraph-backward)
    ("m i" select-inside-pair) ("m a" select-pair)
    ("g q" fill-region) ("g Q" fill-paragraph)
    (">" indent-selection-or-line)
    ("J" select-next-line) ("K" select-previous-line) ("F" delete-other-windows)
    ("S" jump-back) ("z z" recenter) ("z =" spellcheck-correct) ("z g" spellcheck-add-word) ("z f" spellcheck-add-word-to-file) ("z n" spellcheck-next) ("z p" spellcheck-previous) (":" goto-line)
    ("r q" query-replace-regexp) ("r r" replace-regexp) ("?" describe-bindings)
    ("RET" tuatara-normal-return) ("DEL" tuatara-normal-inert) ("<delete>" tuatara-normal-inert) ("TAB" tuatara-normal-tab))
  "The NORMAL state bindings, each (KEYS COMMAND), but for those that select by
the syntax tree (*TUATARA-SYNTAX-BINDINGS*).")

(defparameter *tuatara-syntax-bindings*
  '(("m f" select-function-inside) ("m F" select-function)
    ("m t" select-type-inside) ("m T" select-type)
    ("m r" select-parameter-inside) ("m R" select-parameter)
    ("m c" select-comment-inside) ("m C" select-comment)
    ("m y" select-test-inside) ("m Y" select-test)
    ("m e" select-entry-inside) ("m E" select-entry)
    ("m x" select-element-inside) ("m X" select-element)
    ("M-o" expand-selection) ("M-i" shrink-selection)
    ("M-n" select-next-sibling) ("M-p" select-previous-sibling))
  "The NORMAL state bindings that select by the syntax tree: the function,
type, parameter, comment, test, entry or element around point, and the piece
of syntax around the selection, inside it or beside it.")

(defparameter *tuatara-prose-keys*
  '(;; move
    "h" "j" "k" "l" "w" "W" "b" "B" "J" "K"
    "g g" "g e" "g h" "g l" "S" ":" "z z"
    ;; select
    "v" "V" "M" "%"
    "m w" "m W" "m b" "m B" "m i" "m a" "m I" "m A"
    "m s" "m S" "m p" "m P" "m v"
    "m l" "m u" "m U" "m DEL" "m <delete>"
    ;; copy, enter, and the chart of keys
    "y" "RET" "?"
    ;; the window alone on the screen
    "F")
  "The keys of NORMAL state that support buffers (lists, help, *Messages*)
have too, its reading half: moving, selecting, marking, jumping and copying,
\\`RET' and \\`?' (every binding), and \\`F' (this window alone on the screen),
none of which changes the text. Each does what it does in NORMAL state, as
bound there when modal editing is installed, unless the buffer's major mode has its
own use for the key: \\`RET' opens and \\`q' quits in a list; \\`g' begins
motions, so a list is made again with \\`r' or \\`g r' (\\<buffers-mode>\\[revert-buffer]).")

(defparameter *tuatara-list-keys*
  '("j" "k" "V" "J" "K" "y" "z z" "?" "F")
  "The keys of NORMAL state that lists of lines have (*TUATARA-LIST-KEYMAP*),
as the agenda has: up and down a line, whole lines selected and grown a line
at a time and copied, the line in the middle of the window, every binding,
this window alone on the screen; not the keys that move by words or select
parts of lines, which have nothing to act on there.")

(defparameter *tuatara-leader-bindings* '()
  "The leader's keys (after \\`SPC' in NORMAL state) as standard: none, the
leader being yours alone for keys of your own (bind-key with :map :leader;
the starter init file lists, commented out, the keys it had once). What it
had has keys under C-x and C-c (\\[describe-bindings] lists them).")

(defparameter *tuatara-leader-labels*
  '(("i" "input"))
  "The labels of the leader's prefix keys, which the key hints show after
\\`SPC' in place of the key (\\`t' +toggle); the init file relabels them with
bind-key's :label.")

(defvar *leader-key* "SPC"
  "The key that opens the leader's keys (bind-key's :map :leader) in NORMAL and
support states, in lists and in the calendar: SPC, or another written as
bind-key writes keys (\",\", \"C-c SPC\"). The key it was on is freed. Read
once the init file is.")

(defvar *leader-key-in-force* nil
  "The key the leader's keys are on now (*LEADER-KEY* once the init file is read).")

(defvar *mode-layers* (make-hash-table :test 'equal)
  "The keys bound for one major mode and those made from it (bind-key's :mode):
\((MODE-NAME . MAP) . KEYMAP), MAP being :normal, :insert, :support or :leader;
each consulted in its state before that state's keys.")

(defun leader-hosts ()
  "The keymaps the leader key opens the leader's keys in, each (KEYMAP .
LEADER): NORMAL and support states, lists and the calendar, and the modes'
own NORMAL and support keys for their own leader keys."
  (append (mapcar (lambda (map) (cons map *tuatara-leader-keymap*))
                  (list *tuatara-normal-keymap* *tuatara-support-keymap* *tuatara-list-keymap* *tuatara-calendar-keymap*))
          (loop for (name . map) being the hash-keys of *mode-layers* using (hash-value leader)
                when (eq map :leader)
                  append (loop for state in '(:normal :support)
                               for host = (gethash (cons name state) *mode-layers*)
                               when host collect (cons host leader)))))

(defvar *leader-displaced* '()
  "What the leader key took the place of, each ((KEYMAP . KEY) . BINDING), given
back when the leader moves to another key.")

(defun give-back-leader-key (host leader key)
  "Give KEY of the keymap HOST, where it opens LEADER, the leader's keys, back
what it had there before them (*LEADER-DISPLACED*), or nothing."
  (when (eq (lookup-key host (tuatara.frontend:parse-key-sequence key) :parents nil) leader)
    (let ((displaced (assoc (cons host key) *leader-displaced* :test #'equal)))
      (define-key host key (cdr displaced))
      (setf *leader-displaced* (remove displaced *leader-displaced*)))))

(defun put-leader-on (key)
  "Have KEY (as bind-key writes keys) open the leader's keys wherever they are
opened (LEADER-HOSTS): the key they were on before given back what it had
there before them, what KEY had kept to give back."
  (let ((old *leader-key-in-force*))
    (unless (equal old key)
      (loop for (host . leader) in (leader-hosts)
            do (when old (give-back-leader-key host leader old))
               (let ((had (lookup-key host (tuatara.frontend:parse-key-sequence key) :parents nil)))
                 (when (and had (not (eq had leader)))
                   (push (cons (cons host key) had) *leader-displaced*)))))
    (loop for (host . leader) in (leader-hosts)
          do (define-key host key leader))
    (setf *leader-key-in-force* key)))

(defun link-mode-layers ()
  "Give each mode's own keys (*MODE-LAYERS*) those of the nearest mode it is
made from that has some of the same kind as the keys looked in after them; for
the leader's, the leader's keys last."
  (maphash (lambda (key map)
             (destructuring-bind (name . kind) key
               (setf (keymap-parent map)
                     (or (loop for ancestor in (rest (mode-precedence name))
                               thereis (gethash (cons ancestor kind) *mode-layers*))
                         (and (eq kind :leader) *tuatara-leader-keymap*)))))
           *mode-layers*))

(defun mode-layer (name kind)
  "The keys bound for the major mode called NAME of KIND (:normal, :insert,
:support or :leader), made when there are none: the leader's opened by the
leader key in the mode's NORMAL and support keys."
  (or (gethash (cons name kind) *mode-layers*)
      (let ((map (make-keymap :name (format nil "~(~a~) ~(~a~)" name kind))))
        (setf (gethash (cons name kind) *mode-layers*) map)
        (when (eq kind :leader)
          (dolist (state '(:normal :support))
            (define-key (mode-layer name state) (or *leader-key-in-force* *leader-key*) map)))
        (link-mode-layers)
        map)))

(defun mode-state-keys (buffer state-keymap)
  "The keys bound for BUFFER's major mode, or the nearest it is made from, in
the state whose keys are STATE-KEYMAP: a list of one keymap, or none."
  (when (plusp (hash-table-count *mode-layers*))
    (let ((kind (cond ((eq state-keymap *tuatara-normal-keymap*) :normal)
                      ((eq state-keymap *tuatara-insert-keymap*) :insert)
                      ((eq state-keymap *tuatara-support-keymap*) :support))))
      (when kind
        (loop for name in (mode-precedence (buffer-major-mode buffer))
              for map = (gethash (cons name kind) *mode-layers*)
              when map return (list map))))))

(defun copy-normal-keys (keys keymap)
  "Bind each of KEYS, keys as written, in KEYMAP to the command NORMAL state
binds it to, when it binds one."
  (dolist (key keys)
    (let ((command (lookup-key *tuatara-normal-keymap* (tuatara.frontend:parse-key-sequence key))))
      (when (and command (not (keymap-p command)))
        (define-key keymap key command)))))

(defun install-tuatara-modal-keys ()
  "Bind the NORMAL, INSERT and support keys, the calendar's, and the leader."
  (define-keys *tuatara-normal-keymap* (append *tuatara-normal-bindings* *tuatara-syntax-bindings*))
  (define-key *tuatara-insert-keymap* "C-g" 'tuatara-normal)
  ;; support buffers read and select as NORMAL state does
  (copy-normal-keys *tuatara-prose-keys* *tuatara-support-keymap*)
  (copy-normal-keys *tuatara-list-keys* *tuatara-list-keymap*)
  ;; the calendar: moving, scrolling, today and back, % and y as NORMAL has them, the leader
  (define-keys *tuatara-calendar-keymap* '(("h" calendar-backward-day) ("l" calendar-forward-day)
                                           ("j" calendar-forward-week) ("k" calendar-backward-week)
                                           ("J" calendar-backward-month) ("K" calendar-forward-month)
                                           ("H" calendar-scroll-right) ("L" calendar-scroll-left)
                                           ("g h" calendar-goto-today) ("g l" calendar-jump-back)
                                           ("n" calendar-journal-entry)))
  (copy-normal-keys '("%" "y") *tuatara-calendar-keymap*)
  (put-leader-on *leader-key*)
  (setf tuatara.command:*state-keys-function* 'mode-state-keys)
  (define-keys *tuatara-leader-keymap* *tuatara-leader-bindings*)
  (label-keys *tuatara-leader-keymap* *tuatara-leader-labels*)
  (tuatara-normal-claim-wrap-keys))


(defun modal-sync-after-background (editor)
  "After work from the background, which can show a buffer of its own (a
commit's message), bring EDITOR's modal state in line with the buffer shown,
while modal editing is on (MODAL-SYNC)."
  (when *modal-editing* (modal-sync editor)))

(defun install-tuatara-modal (editor)
  "Install Tuatara's modal editing into EDITOR: keys, mode line, cursor shape, the state sync and
position tracking after every command, the state sync after work from the background too, and
a file read again while typing kept a step of its own to undo, and a file open already, visited
from another buffer, shown in NORMAL state."
  (install-tuatara-modal-keys)
  (setf (editor-mode-line-function editor) #'modal-mode-line
        (editor-cursor-shape-function editor) #'modal-cursor-shape)
  (install-mark-run editor)
  (install-banks)
  (install-split editor)
  (install-cursors editor)
  (pushnew 'selection-remind (editor-post-command-functions editor))
  (pushnew 'note-selection-before (editor-pre-command-functions editor))
  (pushnew 'note-selection-after (editor-post-command-functions editor))
  (pushnew 'modal-sync (editor-post-command-functions editor))
  (pushnew 'modal-sync-after-background *after-tick-functions*)
  (pushnew 'insert-undo-break *before-reload-functions*)
  (pushnew 'insert-undo-restart *after-reload-functions*)
  (pushnew 'end-insert-state *shown-again-functions*)
  (pushnew 'track-position (editor-post-command-functions editor))
  (define-key (editor-global-keymap editor) "C-g" 'tuatara-quit)
  (modal-sync editor))

;;; Keys of the layer

(defun tuatara-normal-claim-wrap-keys ()
  "Bind every delimiter of *PAIR-DELIMITERS*, both halves, to
wrap-selection-with-key in NORMAL state, where no key of its own is: a key that
already does something (\\<normal>\\[indent-selection-or-line] indents,
\\[goto-line] goes to a line) is not taken."
  (bind-free-delimiter-keys *tuatara-normal-keymap* 'wrap-selection-with-key))

(defvar *input-method-keys* '()
  "Input methods of your own under the leader's i, each (KEY LABEL METHOD):
KEY a key, LABEL what it is called, METHOD an input method's name
\\(\\[set-input-method] lists them), such as '((\"s\" \"swedish\" \"swedish-postfix\")).
Each becomes a command, input-method-LABEL, on its key; & . and - are taken.")

(defparameter *input-method-own-keys* '("&" "." "-")
  "The keys under the leader's i that are Tuatara's own.")

(defun input-method-key-command (label method)
  "The command, named input-method-LABEL, that turns METHOD on as
\\[set-input-method] does, calling it LABEL; made when it is not there."
  (let ((name (intern (string-upcase (format nil "input-method-~a"
                                             (string-trim "-" (cl-ppcre:regex-replace-all "[^A-Za-z0-9]+" (string-downcase label) "-"))))
                      :tuatara.defaults)))
    (setf (symbol-function name) (lambda () (turn-on-input-method method label))
          (documentation name 'function) (format nil "Turn on the ~a input method, ~a (*input-method-keys*)." label method)
          (gethash name *commands*) (symbol-function name))
    name))

(defun install-input-method-keys ()
  "Bind the leader's i keys *INPUT-METHOD-KEYS* names, once the init file has
set it: each to a command turning its method on. An entry that is not three
strings, names no input method, or takes one of Tuatara's own keys is left
out, and said."
  (dolist (entry (setting-list '*input-method-keys* *input-method-keys*))
    (if (and (listp entry) (= (length entry) 3) (every #'stringp entry)
             (find-input-method (third entry))
             (not (member (first entry) *input-method-own-keys* :test #'string=)))
        (define-key *tuatara-leader-keymap* (format nil "i ~a" (first entry))
          (input-method-key-command (second entry) (third entry)))
        (message "*input-method-keys*: ~s is left out" entry))))
