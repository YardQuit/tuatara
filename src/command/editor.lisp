;;;; src/command/editor.lisp --- The editor's state, the current buffer, and messages in the echo area.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The command loop is in parts: the panel under the prompt (panel.lisp),
;;;; reading keys and running commands (dispatch.lisp), painting frames through
;;;; the frontend (redisplay.lisp), the minibuffer (minibuffer.lisp), and
;;;; making and running the editor (session.lisp); windows side by side and one
;;;; above another are in windows.lisp.

(in-package #:tuatara.command)

(define-condition quit (error) ()
  (:documentation "Signalled by keyboard-quit (\\`C-g') to abandon the current command or minibuffer.")
  (:report (lambda (c s) (declare (ignore c)) (format s "Quit"))))

(defstruct (frame (:constructor make-frame (frontend)) (:copier nil))
  "One screen of the editor. FRONTEND paints it and reads its events; WINDOW
is its selected window, the one commands act on, in a layout of one or more;
MINIBUFFER-PROMPT, MINIBUFFER-TEXT and MINIBUFFER-ACTIVE describe its echo
area; MESSAGE is transient text shown there; PENDING are the key specs of an
unfinished key sequence typed in it; UNREAD holds events read while the
editor waited on work of its own, to be handled next, in order; DEFERRED
holds the window closed or files dropped while a command was asking for
something, to be handled once it is done."
  (frontend nil :read-only t)
  (window nil)
  (minibuffer-prompt nil)
  (minibuffer-text nil)
  (minibuffer-point 0 :type fixnum)
  (minibuffer-count nil)
  (minibuffer-active nil)
  (completion nil)
  (popup nil)
  (note nil)
  (minibuffer-completer nil)
  (height 24 :type fixnum)
  (width 80 :type fixnum)
  (message nil)
  (pending '())
  (prefix-argument nil)
  (unread '())
  (collecting-prefix nil)
  (this-key nil)
  (transient-map nil)
  (transient-keep nil)
  (transient-exit nil)
  (transient-fresh nil)
  (menu nil)
  (title-shown nil)
  (deferred '()))

(setf (documentation 'make-frame 'function) "A FRAME painted through FRONTEND, with no window yet."
      (documentation 'frame-p 'function) "True when OBJECT is a FRAME.")

(defstruct (editor (:constructor %make-editor (frontend &aux (frame (make-frame frontend)) (frames (list frame))))
                   (:copier nil))
  "The editor state. FRAMES are its screens, and FRAME the one being worked in:
EDITOR-WINDOW, EDITOR-MESSAGE, EDITOR-MINIBUFFER-TEXT and the other names of a
screen's state read and set that frame's; SELECTIONS counts the
times a window was selected; KILL-RING holds killed text, newest first;
STATE-KEYMAP is the modal state's map consulted before the others (the modal
layer's); RUNNING is false once the editor should exit."
  (frames '() :type list)
  (frame nil :type frame)
  (selections 0 :type fixnum)
  (global-keymap (make-keymap :name "global"))
  (state-keymap nil :type (or null keymap))
  (fallback-keymap nil :type (or null keymap))
  (minibuffer-keymap (make-keymap :name "minibuffer"))
  (key-hints t)
  (kill-ring '())
  (last-command nil)
  (this-command nil)
  (running t)
  (self-insert-command nil)
  (mode-line-function nil)
  (cursor-shape-function nil)
  (post-command-functions '())
  (repeatable nil)
  (repeat-map nil)
  (pre-command-functions '()))

(defmacro define-frame-accessor (name slot documentation &key read-only)
  "Define NAME, a function of an editor, as SLOT of the editor's current frame,
documented by DOCUMENTATION; SETF, INCF and PUSH on it change that frame's,
unless READ-ONLY. The frame's own reader of SLOT is documented as NAME's."
  (let ((reader (symbolicate 'frame- slot)))
    `(progn
       (setf (documentation ',reader 'function)
             ,(format nil "The ~(~a~) of FRAME, which `~(~a~)' gives of an editor's current frame." slot name))
       (declaim (inline ,name ,@(unless read-only `((setf ,name)))))
       (defun ,name (editor)
         ,documentation
         (,reader (editor-frame editor)))
       ,@(unless read-only
           `((defun (setf ,name) (value editor)
               ,documentation
               (setf (,reader (editor-frame editor)) value)))))))

(define-frame-accessor editor-frontend frontend
  "The frontend of EDITOR's current frame." :read-only t)
(define-frame-accessor editor-window window
  "The selected window of EDITOR's current frame, the one commands act on.")
(define-frame-accessor editor-minibuffer-prompt minibuffer-prompt
  "The prompt shown while the minibuffer of EDITOR's current frame is active.")
(define-frame-accessor editor-minibuffer-text minibuffer-text
  "The text typed so far in the minibuffer of EDITOR's current frame.")
(define-frame-accessor editor-minibuffer-point minibuffer-point
  "The cursor position within the minibuffer text of EDITOR's current frame.")
(define-frame-accessor editor-minibuffer-count minibuffer-count
  "While a question that is no completing read counts something, what it shows
before its prompt in EDITOR's current frame (3/12, or \"\" for its place kept
empty); else NIL.")
(define-frame-accessor editor-minibuffer-active minibuffer-active
  "True while the minibuffer of EDITOR's current frame reads input.")
(define-frame-accessor editor-completion completion
  "The COMPLETION state while a completing read is active in EDITOR's current frame, or NIL.")
(define-frame-accessor editor-popup popup
  "The list of completions shown beside the cursor in EDITOR's current frame (COMPLETE-IN-TEXT), or NIL.")
(define-frame-accessor editor-note note
  "The box of words shown under the cursor in EDITOR's current frame (SHOW-NOTE),
as (BUFFER POSITION PARAGRAPHS), or NIL.")
(define-frame-accessor editor-minibuffer-completer minibuffer-completer
  "While the minibuffer of EDITOR's current frame reads, the function
\\<minibuffer>\\[minibuffer-complete] calls to complete the word at its cursor, or NIL.")
(define-frame-accessor editor-height height
  "Rows of the whole display of EDITOR's current frame.")
(define-frame-accessor editor-width width
  "Columns of the whole display of EDITOR's current frame.")
(define-frame-accessor editor-message message
  "Transient text shown in the echo area of EDITOR's current frame, or NIL.")
(define-frame-accessor editor-pending pending
  "Key specs of an unfinished key sequence typed in EDITOR's current frame.")
(define-frame-accessor editor-prefix-argument prefix-argument
  "The numeric prefix argument for the current command, given in EDITOR's current frame, or NIL.")
(define-frame-accessor editor-unread unread
  "Events of EDITOR's current frame read while the editor waited on work of its
own, to be handled next, in order.")
(define-frame-accessor editor-collecting-prefix collecting-prefix
  "True while \\[universal-argument] collects digits for the prefix argument in EDITOR's current frame:
:DIGITS once a digit has come since the last \\[universal-argument].")
(define-frame-accessor editor-this-key this-key
  "The key that completed the current command's key sequence in EDITOR's current frame.")
(define-frame-accessor editor-transient-map transient-map
  "A keymap consulted before all others in EDITOR's current frame while its
commands keep it (SET-TRANSIENT-MAP), or NIL.")
(define-frame-accessor editor-transient-keep transient-keep
  "The function of a command that says whether the transient map of EDITOR's current frame stays after it.")
(define-frame-accessor editor-transient-exit transient-exit
  "The function called when the transient map of EDITOR's current frame goes, or NIL.")
(define-frame-accessor editor-transient-fresh transient-fresh
  "True while the command that set the transient map of EDITOR's current frame
runs: that one is not asked whether it keeps it.")
(define-frame-accessor editor-menu menu
  "The menu open now in EDITOR's current frame, the arguments it has on and
those it was opened with, as (MENU ARGUMENTS OPENED), or NIL (OPEN-MENU).")
(define-frame-accessor editor-title-shown title-shown
  "The buffer name and *GUI-TITLE* the title of EDITOR's current frame was last made from, as a list.")
(define-frame-accessor editor-deferred deferred
  "Events that ended a command's question in EDITOR's current frame, the window
closed or files dropped, to be handled once it is done, oldest first.")

(setf (documentation 'editor-p 'function) "True when OBJECT is an EDITOR."
      (documentation 'editor-frames 'function) "The frames of EDITOR, its screens."
      (documentation 'editor-frame 'function) "The current frame of EDITOR, the screen being worked in."
      (documentation 'editor-selections 'function) "How many times a window of EDITOR has been selected: the clock of WINDOW-USE-TIME."
      (documentation 'editor-global-keymap 'function) "EDITOR's global keymap, the last one consulted."
      (documentation 'editor-state-keymap 'function) "The modal state keymap consulted first, or NIL."
      (documentation 'editor-fallback-keymap 'function) "A keymap consulted after the buffer's own and before the global one (the modal support keys), or NIL."
      (documentation 'editor-minibuffer-keymap 'function) "The keymap active while the minibuffer reads input."
      (documentation 'editor-key-hints 'function) "True to show the bindings under a pending prefix key in the panel."
      (documentation 'editor-kill-ring 'function) "Kills, newest first: strings, or ropes for kills too large for the system clipboard."
      (documentation 'editor-last-command 'function) "The command run before the current one."
      (documentation 'editor-this-command 'function) "The command being run."
      (documentation 'editor-running 'function) "False once the editor should exit."
      (documentation 'editor-self-insert-command 'function) "Command run for an unbound plain character, or NIL to treat it as undefined."
      (documentation 'editor-mode-line-function 'function) "Function of (EDITOR WINDOW) returning the text of WINDOW's mode line and the spans of it in faces of their own, or NIL for the default."
      (documentation 'editor-cursor-shape-function 'function) "Function of (EDITOR) returning the cursor's shape and colour, as *CURSOR-SHAPE* and *CURSOR-COLOUR* take them, NIL for those; and as a third value, true while text is typed. NIL for *CURSOR-SHAPE* and *CURSOR-COLOUR* always."
      (documentation 'editor-post-command-functions 'function) "Functions called with (EDITOR) after every command."
      (documentation 'editor-repeatable 'function) "The last command run other than repeat, the prefix argument it had and the key that ran it, as (COMMAND ARGUMENT . KEY)."
      (documentation 'editor-repeat-map 'function) "The keymap of keys that repeat the command just run by themselves (DEFINE-REPEAT-KEYS), while it is in force, or NIL."
      (documentation 'editor-pre-command-functions 'function) "Functions called with (EDITOR COMMAND) before every command runs.")

(defun wake-editor (editor)
  "Wake EDITOR's frames, from any thread, to show what work in the background
has for them; an error waking one is ignored."
  (dolist (frame (editor-frames editor))
    (ignore-errors (tuatara.frontend:frontend-wake (frame-frontend frame)))))

(defvar *editor* nil
  "The editor whose command is running.")

(defun prefix-argument ()
  "The prefix argument given the command running now, a number: 4 for
\\[universal-argument] alone, 16 for it twice, the digits typed after it, -1 for a
lone minus; NIL for none, or with no editor."
  (and *editor* (editor-prefix-argument *editor*)))

(defstruct (completion (:constructor make-completion (table &key annotate require-match history (separator #\/) (filter t)
                                                             select reselect preview source default keep-order none
                                                             (prompt "")))
                       (:copier nil))
  "State of a completing read. TABLE is a list of CANDIDATE or a function of
the input returning one; ANNOTATE is a function of a candidate returning an
annotation string or NIL; REQUIRE-MATCH true means \\<minibuffer>\\[minibuffer-done] must pick a candidate;
HISTORY is a list of previous inputs, most recent first (or a function of no
arguments returning one, for a list still growing); SEPARATOR, for a
TABLE that is a function, the character after whose last occurrence the input
is matched and completed (/ for file names, a comma for a list of words);
FILTER false shows a function TABLE's candidates as it gives them, matched and
ordered already (a search of files as you type). SELECT, when given, is a
function of the candidates first shown (a vector) and the input, returning the
index of the one selected at first, or :INPUT for the line typed itself; with
RESELECT, of those shown after each change of the input too. Where the read
takes what is typed (REQUIRE-MATCH false), the line typed can be selected as a
candidate is (INDEX -1): it is when nothing matches, or SELECT says so, and
one step above the first candidate; PREVIEW a function called with the selected candidate,
or NIL when none is, each time the selection changes, and PREVIEWED the
candidate it was last called with. SOURCE, for candidates that arrive from
work in the background, is a function the minibuffer calls on the editor's
thread whenever it is woken while the read is open, returning three values:
true when the candidates may have changed (STALE is set then, and they are
worked out again, the selected one kept), true while more may come
(WORKING), and how many things it has listed so far, or NIL for nothing to
tell (LISTED), which the prompt tells while more come (SHOWN-PROMPT).
DEFAULT, when not NIL, is what \\[minibuffer-done] takes while nothing is
typed, its candidate offered first then, and named in the prompt; KEEP-ORDER
true keeps the candidates matching in the order TABLE gives them, not sorted
best first; NONE, words the prompt says once all have come and there are
none; PROMPT, the question as asked, before those words are added to it (or
a function of no arguments giving it).
DONE-PENDING is true when \\[minibuffer-done] came while nothing could be
chosen yet and more was coming, to be done once it has come; MOVED
is true once a candidate was selected by hand for the input typed, which
stays selected as more come (else the best match is); TALLEST, the most rows
of candidates shown so far, the panel keeping that height while the question
is open, so that the windows above do not jump as typing narrows the list.
CANDIDATES holds the matches for the current input and INDEX the selected
one, -1 for the line typed; NARROWED, the last filtering, (ALL QUERY
MATCHES), so that a query typed on from that one is matched against MATCHES
alone."
  (table nil :read-only t)
  (annotate nil :read-only t)
  (require-match nil :read-only t)
  (history '() :read-only t)
  (separator #\/ :read-only t)
  (filter t :read-only t)
  (select nil :read-only t)
  (reselect nil :read-only t)
  (preview nil :read-only t)
  (source nil :read-only t)
  (default nil :read-only t)
  (keep-order nil :read-only t)
  (none nil :read-only t)
  (prompt "" :type (or string function) :read-only t)
  (working nil)
  (listed nil)
  (stale nil)
  (done-pending nil)
  (moved nil)
  (tallest 0)
  (tall nil)
  (previewed :none)
  (candidates #() :type simple-vector)
  (index 0 :type fixnum)
  (input-seen nil)
  (narrowed nil))

(setf (documentation 'make-completion 'function) "Return the state of a completing read over TABLE."
      (documentation 'completion-p 'function) "True when OBJECT is a COMPLETION."
      (documentation 'completion-table 'function) "The candidate list or function of a COMPLETION."
      (documentation 'completion-annotate 'function) "The annotation function of a COMPLETION, or NIL."
      (documentation 'completion-require-match 'function) "True when the completing read only accepts a candidate."
      (documentation 'completion-history 'function) "Previous inputs of a COMPLETION, most recent first."
      (documentation 'completion-separator 'function) "The character after which a COMPLETION with a function table matches and completes the input."
      (documentation 'completion-filter 'function) "False when a COMPLETION shows its function table's candidates as they come."
      (documentation 'completion-select 'function) "The function naming the candidate a COMPLETION selects at first, or NIL."
      (documentation 'completion-reselect 'function) "True when a COMPLETION's SELECT names the candidate selected after each change of the input too."
      (documentation 'completion-preview 'function) "The function a COMPLETION calls with the selected candidate as the selection changes, or NIL."
      (documentation 'completion-source 'function) "The function a COMPLETION calls to take in candidates found in the background, or NIL."
      (documentation 'completion-default 'function) "What a COMPLETION takes while nothing is typed, or NIL."
      (documentation 'completion-keep-order 'function) "True when a COMPLETION keeps its candidates in the order given."
      (documentation 'completion-none 'function) "The words a COMPLETION's prompt says when all have come and there are none, or NIL."
      (documentation 'completion-prompt 'function) "The question a COMPLETION asks, as asked."
      (documentation 'completion-working 'function) "True while a COMPLETION's SOURCE said more candidates may come."
      (documentation 'completion-listed 'function) "How many things a COMPLETION's SOURCE said it has listed so far, or NIL."
      (documentation 'completion-stale 'function) "True when a COMPLETION's candidates are to be worked out again, its input unchanged."
      (documentation 'completion-tallest 'function) "The most rows of candidates a COMPLETION's panel has shown, which it keeps while the question is open."
      (documentation 'completion-tall 'function) "True while a COMPLETION's candidates take as many rows as the frame has room for (\\[minibuffer-toggle-tall])."
      (documentation 'completion-moved 'function) "True when a COMPLETION's selection was moved by hand since the input last changed."
      (documentation 'completion-done-pending 'function) "True when a choice was asked of a COMPLETION before anything could be chosen, more coming."
      (documentation 'completion-previewed 'function) "The candidate a COMPLETION's preview function was last called with, or :NONE."
      (documentation 'completion-candidates 'function) "The candidates matching the current input."
      (documentation 'completion-index 'function) "The index of the selected candidate, -1 when it is the line typed."
      (documentation 'completion-input-seen 'function) "The input the candidates were computed for."
      (documentation 'completion-narrowed 'function) "The last filtering of a COMPLETION: (ALL QUERY MATCHES).")

(defstruct (popup (:constructor %make-popup (target start end table annotate document constituent &optional keep-order auto))
                  (:copier nil))
  "Completion in the text (COMPLETE-IN-TEXT). TARGET is the buffer the word is
in, or :MINIBUFFER. In a buffer, START and END are anchors at the word's ends,
END moving on with what is typed at it; in the minibuffer, START is the index
of the word's first character and END the text after the word, which stays as
it is. TABLE is the candidates, a list of CANDIDATE; ANNOTATE a function of a
candidate returning what is said beside it, or NIL; DOCUMENT one returning its
documentation, shown in the box beside the list, or NIL; CONSTITUENT a
function of a character, true for those a word is made of; KEEP-ORDER true to
list the matches in TABLE's order, not the best first; AUTO true for a list
that opened by itself as the word was typed, with none chosen at first.
CANDIDATES holds the matches of the word, INDEX the chosen one (-1 for none)
and SCROLL the first shown;
SEPARATOR is true once \\<popup>\\[popup-insert-separator] put a space in the word, which
then goes on past spaces; INPUT-SEEN is the word the candidates were matched
against; SHOWN the documentation last shown, as (CANDIDATE . LINES)."
  (target nil :read-only t)
  (start nil :read-only t)
  (end nil :read-only t)
  (table '() :type list :read-only t)
  (annotate nil :read-only t)
  (document nil :read-only t)
  (constituent nil :read-only t)
  (keep-order nil :read-only t)
  (auto nil :read-only t)
  (candidates #() :type simple-vector)
  (index 0 :type fixnum)
  (scroll 0 :type fixnum)
  (separator nil)
  (input-seen nil)
  (shown nil))

(setf (documentation 'popup-p 'function) "True when OBJECT is a POPUP."
      (documentation 'popup-target 'function) "The buffer the completed word is in, or :MINIBUFFER."
      (documentation 'popup-start 'function) "Where the completed word starts: an anchor in a buffer, an index in the minibuffer."
      (documentation 'popup-end 'function) "Where the completed word ends: an anchor in a buffer, the text after it in the minibuffer."
      (documentation 'popup-table 'function) "Every candidate of a completion in the text."
      (documentation 'popup-annotate 'function) "The function giving what is said beside a candidate, or NIL."
      (documentation 'popup-document 'function) "The function giving a candidate's documentation, or NIL."
      (documentation 'popup-constituent 'function) "The function saying which characters a word is made of."
      (documentation 'popup-keep-order 'function) "True when the matches are listed in the table's order."
      (documentation 'popup-candidates 'function) "The candidates matching the word, best first."
      (documentation 'popup-auto 'function) "True for a list that opened by itself as the word was typed."
      (documentation 'popup-index 'function) "The index of the chosen candidate, -1 while none is."
      (documentation 'popup-scroll 'function) "The index of the first candidate shown."
      (documentation 'popup-separator 'function) "True once \\<popup>\\[popup-insert-separator] put a space in the word."
      (documentation 'popup-input-seen 'function) "The word the candidates were matched against."
      (documentation 'popup-shown 'function) "The documentation last shown, as (CANDIDATE ROOM LINES WIDTH): its lines broken for ROOM columns, the box WIDTH wide (DOCUMENTATION-LINES).")

(defun resize (editor width height)
  "Record the display size and lay the windows out on the rows the panel leaves."
  (setf (editor-width editor) (max 1 width) (editor-height editor) (max 3 height))
  (fit-windows editor))

(defvar *current-buffer* nil
  "The buffer commands and the Lisp one types act on when it is not the selected
window's: made so by SET-BUFFER until the command ends, or by
WITH-CURRENT-BUFFER for its body.")

(defun current-buffer ()
  "Return the current buffer: the one commands act on, the selected window's
unless SET-BUFFER or WITH-CURRENT-BUFFER made another current."
  (or *current-buffer* (tuatara.display:window-buffer (editor-window *editor*))))

(defun current-window ()
  "The selected window, the one commands act on."
  (editor-window *editor*))

(defvar *message-log-max* 1000
  "Lines the *Messages* buffer keeps of the messages shown, the oldest going
first; NIL keeps none.")

(defun message-repeats (line)
  "LINE, a line of *Messages*, as the message it shows and how many times:
\"Saved [3 times]\" is \"Saved\" three times."
  (let ((open (search " [" line :from-end t)))
    (if (and open (> (length line) (+ open 8)) (string= " times]" line :start2 (- (length line) 7)))
        (let ((count (parse-integer line :start (+ open 2) :end (- (length line) 7) :junk-allowed t)))
          (if count (values (subseq line 0 open) count) (values line 1)))
        (values line 1))))

(defvar *messages-made-function* nil
  "A function of the *Messages* buffer, called when it has just been made,
giving it what more it has (its major mode); NIL for nothing more.")

(defun messages-buffer ()
  "The *Messages* buffer, made when there is none: read-only, and given what
*MESSAGES-MADE-FUNCTION* gives it."
  (or (tuatara.buffer:find-buffer "*Messages*")
      (let ((b (tuatara.buffer:make-buffer "*Messages*")))
        (setf (tuatara.buffer:buffer-local b :read-only) t
              (tuatara.buffer:buffer-local b :kind) :messages)
        (when *messages-made-function* (funcall *messages-made-function* b))
        b)))

(defun add-to-log (buffer max change)
  "Call CHANGE with BUFFER, a log such as *Messages*, its read-only lifted for
it, then keep BUFFER's last MAX lines, the oldest going first (all of them
when MAX is NIL). A log never counts as modified and has no undo; a window
showing its end goes on showing it."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (following (and *editor*
                         (remove-if-not (lambda (window) (= (tuatara.display:window-point window) size))
                                        (buffer-windows buffer)))))
    (unwind-protect
         (tuatara.buffer:with-read-only-lifted (buffer :after t)
           (funcall change buffer)
           (when max
             (let ((lines (1- (tuatara.buffer:buffer-line-count buffer))))
               (when (> lines max)
                 (tuatara.buffer:buffer-delete buffer 0 (tuatara.buffer:buffer-goto-line buffer (- lines max)))))))
      (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '())
      (tuatara.buffer:buffer-mark-saved buffer))
    (dolist (window following)
      (setf (tuatara.display:window-point window) (tuatara.buffer:buffer-size buffer)))))

(defun log-message (text)
  "Add TEXT, a message just shown, to the *Messages* buffer (made when there is
none): a line of its own, or the last line counted up when it says the same
(\"Saved [2 times]\"). The buffer keeps *MESSAGE-LOG-MAX* lines (ADD-TO-LOG)."
  (when (and *message-log-max* (plusp (length text)))
    (add-to-log (messages-buffer) *message-log-max*
                (lambda (buffer)
                  (let* ((size (tuatara.buffer:buffer-size buffer))
                         (last-start (if (zerop size) 0 (tuatara.buffer:buffer-line-start buffer (1- size))))
                         (last (and (plusp size) (tuatara.buffer:buffer-string buffer :start last-start :end (1- size)))))
                    (multiple-value-bind (said count) (if last (message-repeats last) (values nil 0))
                      (if (and last (string= said text))
                          (tuatara.buffer:buffer-replace buffer last-start (1- size) (format nil "~a [~d times]" text (1+ count)))
                          (tuatara.buffer:buffer-insert buffer size (format nil "~a~%" text)))))))))

(defvar *messages-kept* nil
  "While work runs on a thread away from the editor's: a list cell whose first
element gathers the messages it says, the newest first, for the editor to
show once the work is taken in; NIL, as on the editor's own thread, for
messages to be shown at once.")

(defvar *running-editor* nil
  "The editor whose session runs, and the thread it runs on, as a cons; NIL
when none does. Work on other threads finds it here.")

(defvar *messages-from-threads* '()
  "The messages said on other threads while an editor runs, the newest first,
for the editor's thread to show and keep (TAKE-IN-THREAD-MESSAGES).")

(defvar *actions-from-threads* '()
  "The functions given to DEFER-ACTION on other threads while an editor runs,
the newest first, for the editor's thread to run (TAKE-IN-THREAD-MESSAGES).")

(defvar *messages-from-threads-lock* (sb-thread:make-mutex :name "messages from threads")
  "Held while *MESSAGES-FROM-THREADS* or *ACTIONS-FROM-THREADS* is changed.")

(defun away-from-editor-p ()
  "True on a thread other than the one the running editor's session runs on."
  (let ((running *running-editor*))
    (and running (not (eq (cdr running) sb-thread:*current-thread*)))))

(defmacro hand-to-editor-thread (item place)
  "Push ITEM onto PLACE, *MESSAGES-FROM-THREADS* or *ACTIONS-FROM-THREADS*,
under *MESSAGES-FROM-THREADS-LOCK*, and wake the running editor to take it in."
  `(progn (sb-thread:with-mutex (*messages-from-threads-lock*)
            (push ,item ,place))
          (let ((running *running-editor*))
            (when running
              (wake-editor (car running))))))

(defun take-in-thread-messages (editor)
  "Show and keep in *Messages*, on EDITOR's thread, the messages said on other
threads since last time, in the order they were said, and have the functions
given to DEFER-ACTION there run as EDITOR's next commands, in that order too."
  (when (or *messages-from-threads* *actions-from-threads*)
    (let (texts actions)
      (sb-thread:with-mutex (*messages-from-threads-lock*)
        (setf texts (shiftf *messages-from-threads* '())
              actions (shiftf *actions-from-threads* '())))
      (dolist (text (reverse texts))
        (setf (editor-message editor) text)
        (ignore-errors (log-message text)))
      (when actions
        (setf (editor-deferred editor) (append (editor-deferred editor) (reverse actions)))))))

(defun message (control &rest arguments)
  "Show a formatted message in the echo area until the next command, and keep
it in the *Messages* buffer (LOG-MESSAGE). The keys CONTROL names as
\\=\\[COMMAND] and the rest (SUBSTITUTE-COMMAND-KEYS) are those bound now, shown
in :help-key-binding. Said by work away from the editor's thread, while
*MESSAGES-KEPT* holds a cell, it is kept there to be shown later; said there
otherwise, it is handed to the editor's thread, which shows it as soon as it
is woken."
  (let ((text (unmark-keys (apply #'format nil (substitute-command-keys control :marks t :for-format t) arguments))))
    (cond (*messages-kept*
           (push text (car *messages-kept*)))
          ((away-from-editor-p)
           (hand-to-editor-thread text *messages-from-threads*))
          (t
           (when *editor*
             (setf (editor-message *editor*) text))
           (ignore-errors (log-message text))))
    text))

(defun echo (control &rest arguments)
  "Show a formatted text in the echo area, as MESSAGE does, without keeping it
in *Messages*: the keys of a sequence being typed, prompts."
  (setf (editor-message *editor*)
        (unmark-keys (apply #'format nil (substitute-command-keys control :marks t :for-format t) arguments))))

(defun keyboard-quit-signal ()
  "Signal QUIT: abandon the current command or minibuffer read."
  (error 'quit))

(defvar *key-question* nil
  "True while a question in the echo area waits for one key (READ-KEY): the
cursor stands after the question, the window's point drawn as a window's
the typing does not go to.")

(defvar *minibuffer-question-keymap* nil
  "The keys of the question the minibuffer asks now, consulted before the
minibuffer's own (READ-STRING's KEYMAP); NIL when it has none.")

(defvar *minibuffer-hidden* nil
  "True while the minibuffer reads a secret (READ-PASSWORD): what is typed is
shown as stars, and the keys of the commands run meanwhile are not noted in
debug mode's log.")

(defun minibuffer-shown-text (editor)
  "The text typed in EDITOR's minibuffer as it is shown: as stars while it reads a secret."
  (let ((text (editor-minibuffer-text editor)))
    (if *minibuffer-hidden* (make-string (length text) :initial-element #\*) text)))
