;;;; src/command/dispatch.lisp --- Reading keys and running the commands they are bound to.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defvar *state-keys-function* nil
  "A function of a buffer and the modal state's keymap giving the keymaps
consulted just before that state's: the keys bound for the buffer's major mode
in that state; NIL for none.")

(defun mouse-key-window (editor key)
  "The window of EDITOR whose text KEY, a mouse key, happened over, or NIL for
a key of the keyboard or one elsewhere (WINDOW-AT-CELL)."
  (and (tuatara.frontend:key-column key) (tuatara.frontend:key-row key)
       (window-at-cell (tuatara.frontend:key-column key) (tuatara.frontend:key-row key) editor)))

(defun active-keymaps (editor &optional key)
  "The keymaps consulted for a key, in precedence order: the keys of the list
of completions beside the cursor while one is shown (COMPLETE-IN-TEXT); then
the question's own and the minibuffer's while it reads (READ-STRING); otherwise a transient map in force
(SET-TRANSIENT-MAP), the current buffer's own keys for a passing use of it
(its :OVERRIDE-KEYMAP local, as a quick edit's from a rimu list), the keys
bound for its major mode in the modal state (*STATE-KEYS-FUNCTION*), the modal
state map, the current buffer's local keymap (its major mode's, in the :KEYMAP
local, whose parents are the parent modes' keymaps), the fallback map, and the
global map. For KEY, when given and a mouse key over a window's text, the
buffer is that window's rather than the current one, so that a click acts
as that buffer's keys say at once."
  (if (editor-minibuffer-active editor)
      (remove nil (list (popup-keys editor) *minibuffer-question-keymap* (editor-minibuffer-keymap editor)))
      (let* ((window (or (and key (mouse-key-window editor key)) (editor-window editor)))
             (buffer (and window (tuatara.display:window-buffer window))))
        (remove nil (list* (popup-keys editor)
                           (editor-transient-map editor)
                           (and buffer (tuatara.buffer:buffer-local buffer :override-keymap))
                           (append (and buffer *state-keys-function* (editor-state-keymap editor)
                                        (funcall *state-keys-function* buffer (editor-state-keymap editor)))
                                   (list (editor-state-keymap editor)
                                         (and buffer (tuatara.buffer:buffer-local buffer :keymap))
                                         (editor-fallback-keymap editor)
                                         (editor-global-keymap editor))))))))

(defvar *prefix-help-function* nil
  "A function of the editor and a prefix key's specs, called when \\`C-h'
\(*PREFIX-HELP-KEY*) follows the prefix and nothing is bound to it there: one
listing every key under the prefix.")

(defun resolve-keys (editor specs &optional key)
  "Look SPECS up in the active keymaps in precedence order, KEY the last of
them as typed when known (ACTIVE-KEYMAPS). Returns a command, a keymap
\(prefix), or NIL. A key bound to :UNDEFINED in a keymap is unbound there and
in the keymaps after it: NIL, as a key bound nowhere."
  (keymaps-lookup (active-keymaps editor key) specs))

(defvar tuatara-user:*escape-quits* nil
  "True to have Escape do what \\`C-g' does everywhere: leave INSERT state,
close a question, a list or a popup, stop a wait or work in the background,
drop a selection; NIL (the standard) for Escape as a key of its own, which
does what it is bound to and nothing where it is bound to nothing. In a
terminal, Escape quickly followed by another key arrives as Meta with that
key, as terminals send the two alike. \\[quoted-insert] still takes Escape
as itself.")

(defvar *keys-as-typed* nil
  "True while a key is read as it was typed, Escape as Escape whatever
*ESCAPE-QUITS* says (\\[quoted-insert]).")

(defun keys-as-typed-here-p ()
  "True when the current buffer wants keys as they were typed, Escape as
Escape: its :KEYS-AS-TYPED local, a function of the buffer saying so (a
terminal while it is typed into)."
  (let* ((window (and *editor* (editor-window *editor*)))
         (buffer (and window (tuatara.display:window-buffer window)))
         (wants (and buffer (tuatara.buffer:buffer-local buffer :keys-as-typed))))
    (and wants (funcall wants buffer))))

(defun escape-as-quit (event)
  "EVENT, a key of Escape made \\`C-g' while *ESCAPE-QUITS* says so (and keys
are not read as typed, nor wanted so where they are typed); any other event as it is."
  (if (and tuatara-user:*escape-quits* (not *keys-as-typed*) (not (keys-as-typed-here-p))
           (eq (tuatara.frontend:event-kind event) :key)
           (equal (tuatara.frontend:key-spec (tuatara.frontend:event-key event)) '(:escape)))
      (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\g :ctrl t))
      event))

;;; Keys recorded and played back

(defstruct (recording (:constructor make-recording ()) (:copier nil))
  "Keys being recorded: EVENTS, the key events read so far, in order, and
COMMAND-START, how many of them came before the keys of the command running
now."
  (events (make-array 64 :adjustable t :fill-pointer 0) :read-only t)
  (command-start 0 :type fixnum))

(setf (documentation 'make-recording 'function) "A RECORDING with no keys in it yet."
      (documentation 'recording-p 'function) "True when OBJECT is a RECORDING.")

(defvar *keys-recording* nil
  "The keys being recorded, a RECORDING, or NIL while none are.")

(defstruct (playing (:constructor make-playing (events)) (:copier nil))
  "Recorded keys being played once: EVENTS, a vector of key events; NEXT, the
index of the next one to give; FAILED, once a command among them failed, the
words it left in the echo area (or T); RAN-OUT, when the keys had all been
given while a question still waited on work in the background, the time
they had."
  (events #() :type simple-vector :read-only t)
  (next 0 :type fixnum)
  (failed nil)
  (ran-out nil))

(setf (documentation 'make-playing 'function) "A PLAYING of EVENTS from their first."
      (documentation 'playing-p 'function) "True when OBJECT is a PLAYING.")

(defvar *keys-playing* nil
  "While recorded keys are played, the PLAYING they are read from: READ-EVENT
gives its keys before any typed, and nothing is drawn; NIL otherwise.")

(defparameter *played-keys-wait* 10
  "The most seconds a question may go on waiting for work in the background
once the recorded keys played into it have all been given; after that the
play fails, as the keys ended before the question did.")

(defun keys-playing-p ()
  "True while recorded keys are being played."
  (and *keys-playing* t))

(defun keys-recording-p ()
  "True while keys are being recorded."
  (and *keys-recording* t))

(defun note-command-failed ()
  "Say that the command running now did not do what it was asked, as a search
that found nothing: while recorded keys are played, the play stops after it,
the words in the echo area saying why."
  (let ((playing *keys-playing*))
    (when (and playing (not (playing-failed playing)))
      (setf (playing-failed playing) (or (and *editor* (editor-message *editor*)) t)))))

(defun record-event (event)
  "Add EVENT, a key or text pasted just read as typed, to the keys being
recorded, the paste to be pasted again as it was; the mouse is not
recorded."
  (unless (and (eq (tuatara.frontend:event-kind event) :key)
               (tuatara.frontend:key-row (tuatara.frontend:event-key event)))
    (vector-push-extend event (recording-events *keys-recording*))))

(defun give-up-keys-recording ()
  "End the recording of keys going on, with nothing kept: what \\`C-g' does
when it gives up a command or a question while keys are recorded (but not
while recorded keys play)."
  (unless *keys-playing*
    (setf *keys-recording* nil)))

(defun played-event (editor playing timeout ticks)
  "The next key event of PLAYING; after \\`C-g' stopped the play
\(QUIT-WHILE-PLAYING-P), a quit instead. Once all have been given, a
question that holds back what it was given while its candidates still come
has a :TICK after at most TIMEOUT seconds (a twentieth at most), for up to
*PLAYED-KEYS-WAIT* seconds, keys typed meanwhile kept for after the play and
\\`C-g' stopping it; otherwise an error: the keys ended before what they were
answering did."
  (let ((events (playing-events playing))
        (next (playing-next playing))
        (completion (editor-completion editor)))
    (cond ((eq (playing-failed playing) :quit) (keyboard-quit-signal))
          ((< next (length events))
           (setf (playing-next playing) (1+ next))
           (svref events next))
          ((and ticks completion (completion-done-pending completion) (completion-working completion)
                (< (tuatara.runtime:milliseconds-since (or (playing-ran-out playing)
                                                             (setf (playing-ran-out playing) (get-internal-real-time))))
                   (* 1000 *played-keys-wait*)))
           (let ((frontend (frame-frontend (editor-frame editor))))
             (if (tuatara.frontend:frontend-input-pending-p frontend)
                 (when (keep-or-quit-event editor (tuatara.frontend:frontend-read-event-timeout frontend 1/100))
                   (keyboard-quit-signal))
                 (sleep (min (or timeout 1/20) 1/20))))
           (tuatara.frontend:make-event :tick))
          (t (error "The recorded keys ended before the question they were answering")))))

(defun put-back-events (editor events)
  "Have EVENTS, keys read before they could be handled, read again next, in
order, before any other: back among the recorded keys being played when they
are the last given from them, else first among EDITOR's unread events."
  (let* ((playing *keys-playing*)
         (recording *keys-recording*)
         (count (length events))
         (from (and playing (- (playing-next playing) count))))
    (if (and playing (>= from 0)
             (loop for event in events
                   for i from from
                   always (eq event (svref (playing-events playing) i))))
        (setf (playing-next playing) from)
        (progn
          ;; recorded as they were read, they are recorded again when read once more
          (when (and recording (not playing))
            (let* ((recorded (recording-events recording))
                   (start (- (fill-pointer recorded) count)))
              (when (and (>= start 0)
                         (loop for event in events
                               for i from start
                               always (eq event (aref recorded i))))
                (setf (fill-pointer recorded) start))))
          (setf (editor-unread editor) (append events (editor-unread editor)))))))

(defun keep-or-quit-event (editor event)
  "Deal with EVENT, read from the frontend while EDITOR is busy with work of
its own: true when it is \\`C-g' (QUIT-KEY-P), which is to stop that work.
Any other key is kept to be handled after it, as typed (EDITOR-UNREAD); a
window resized is drawn again; a window closed, files dropped or text pasted
are dealt with after it."
  (case (tuatara.frontend:event-kind event)
    (:key (or (quit-key-p (tuatara.frontend:event-key event))
              (progn (setf (editor-unread editor) (append (editor-unread editor) (list event)))
                     nil)))
    (:resize (resize editor (tuatara.frontend:event-width event) (tuatara.frontend:event-height event))
             (ignore-errors (redisplay editor))
             nil)
    ((:close :drop :paste) (setf (editor-deferred editor) (append (editor-deferred editor) (list event)))
     nil)
    (t nil)))

(defun read-event (editor &key timeout ticks pastes)
  "Read the next event from the frontend, handling a resize here: the screen is
painted again at the new size at once. With TICKS (the command loop), an event
of kind :TICK comes back when another thread wakes the editor, after a resize,
and when no key arrives within TIMEOUT seconds; without, only keys and :QUIT
come back, for the loops reading keys inside a command (the minibuffer, search):
the window closed, files dropped or text pasted there end the command's
question as \\`C-g' does, and are handled once the command is done
\(EDITOR-DEFERRED). With PASTES (the minibuffer), text pasted comes back too.
While it waits the editor counts as idle (TUATARA.RUNTIME:NOTE-EDITOR-BUSY).
While recorded keys are played their keys come first (PLAYED-EVENT); while
keys are recorded each key and paste read as typed is added (RECORD-EVENT)."
  (loop
    (let* ((frame (editor-frame editor))
           (playing *keys-playing*)
           (event (cond (playing (played-event editor playing timeout ticks))
                        ((frame-unread frame) (pop (frame-unread frame)))
                        (t (tuatara.runtime:note-editor-busy nil)
                           (prog1 (if timeout
                                      (tuatara.frontend:frontend-read-event-timeout (frame-frontend frame) timeout)
                                      (tuatara.frontend:frontend-read-event (frame-frontend frame)))
                             (tuatara.runtime:note-editor-busy t))))))
      (case (tuatara.frontend:event-kind event)
        (:resize (resize editor (tuatara.frontend:event-width event) (tuatara.frontend:event-height event))
                 (if ticks
                     (return (tuatara.frontend:make-event :tick))
                     (redisplay-or-say editor)))
        (:tick (when ticks (return event)))
        ((:close :drop :paste) (if (or ticks (and pastes (eq (tuatara.frontend:event-kind event) :paste)))
                            (progn (when (and *keys-recording* (not playing) (eq (tuatara.frontend:event-kind event) :paste))
                                     (record-event event))
                                   (return event))
                            (progn (setf (editor-deferred editor) (append (editor-deferred editor) (list event)))
                                   (keyboard-quit-signal))))
        (t (return (if playing
                       event
                       (let ((event (escape-as-quit event)))
                         (when *keys-recording* (record-event event))
                         event))))))))

(defvar *repeat-maps* (make-hash-table :test 'eq)
  "For each command that can be repeated by a key alone, the keymap of the keys
that then repeat it and its kin, and those keys as written, as (KEYMAP .
KEYS) (DEFINE-REPEAT-KEYS).")

(defvar *not-repeatable* '(universal-argument)
  "Commands the repeat command never repeats: the one that starts a count, and
repeat itself, which the defaults add.")

(defun define-repeat-keys (bindings)
  "Make the commands of BINDINGS, a list of (KEY COMMAND), repeatable by their
keys alone: once any of them has run from its key sequence, each of those KEYS
runs its command straight away, until another key is pressed, which then does
what it always does. Returns the keymap."
  (let* ((map (make-keymap :name "repeat"))
         (entry (cons map (mapcar #'first bindings))))
    (loop for (key command) in bindings
          do (define-key map key command)
             (setf (gethash command *repeat-maps*) entry))
    map))

(defun set-transient-map (editor keymap keep &optional exit)
  "Put KEYMAP before every other keymap of EDITOR for as long as each command
run passes KEEP, a function of the command; once one does not, it goes after
that command, and EXIT, when given, is called. A transient map in force is
taken down first."
  (clear-transient-map editor)
  (setf (editor-transient-map editor) keymap
        (editor-transient-keep editor) keep
        (editor-transient-exit editor) exit
        (editor-transient-fresh editor) t))

(defun clear-transient-map (editor)
  "Take EDITOR's transient map down, calling its exit function."
  (let ((exit (editor-transient-exit editor)))
    (setf (editor-transient-map editor) nil
          (editor-transient-keep editor) nil
          (editor-transient-exit editor) nil)
    (when exit (funcall exit))))

(defvar *debug-on-error* nil
  "True to show the calls under way when a command or an evaluation meets an
error, in *Backtrace*; \\[toggle-debug-on-error] switches it.")

(defvar *backtrace-function* nil
  "A function of a condition and the calls under way when it was signalled (a
list of strings, innermost first, see CAPTURE-BACKTRACE) that shows them; the
defaults show *Backtrace*.")

(defparameter *backtrace-frames* 200
  "The most calls a backtrace keeps, the innermost.")

(defun capture-backtrace (&key stop)
  "The calls under way now, innermost first, as a list of strings, as
BACKTRACE-LINES gives them: names in TUATARA-USER as the Lisp one types sees
them, at most *BACKTRACE-FRAMES* of them, those from the first whose call is to
the function named STOP, a symbol, on left out."
  (tuatara.runtime:backtrace-lines :count *backtrace-frames* :stop stop
                                   :package (or (find-package :tuatara-user) *package*)))

(defvar *this-command-keys* '()
  "The key specs of the sequence that ran the command running now.")

(defvar *command-undo-since* nil
  "The buffer the command running now began in and its undo stack as it was
when the command began, as (BUFFER . STACK): what one undo of the command goes
back to. A command that makes several steps to undo of its own, as playing
recorded keys does, one step a play, sets STACK to where its last step ends.")

(defvar *command-notes* nil
  "While a command run from a key runs (RUN-COMMAND), a list cell whose car is
a property list of what is noted for that command alone: an answer given once
for all its edits. NIL when no command runs: a tick, work in the background,
a buffer edited from Lisp outside the command loop.")

(defvar *command-refusal-function* nil
  "A function of a command's name returning NIL when the command may run now,
or what to say instead of running it: the defaults refuse a command made for
one kind of buffer, such as a list's own keys, anywhere else.")

(defun call-with-guard (thunk what stop &key (quit "Quit") said naming trace-wanted shown)
  "Call THUNK, a function of no arguments, for WHAT, a name for messages, so
that the editor carries on whatever it meets: C-g in it says QUIT (NIL to say
nothing); an error is said, Error in WHAT: and its words, or as SAID, a
function of the words, has it, and reported in debug mode's log with the
calls under way within THUNK, made in the function named STOP, under the
name NAMING, a function of no arguments, gives then, when given;
running out of memory or stack says Stopped and what ran out. The calls are
captured in debug mode, or when TRACE-WANTED, a function of no arguments,
says so, and SHOWN, when given, is called with the error and them. NIL then,
else what THUNK returns."
  (let ((trace nil))
    (flet ((what () (if naming (funcall naming) what)))
      (handler-case
          (handler-bind ((error (lambda (e)
                                  (when (and (or tuatara.runtime:*debug-on* (and trace-wanted (funcall trace-wanted)))
                                             (not (typep e 'quit)))
                                    (setf trace (capture-backtrace :stop stop))))))
            (funcall thunk))
        (quit () (when quit (message "~a" quit)) nil)
        (error (e)
          (let ((words (tuatara.runtime:condition-words e)))
            (message "~a" (if said (funcall said words) (format nil "Error in ~(~a~): ~a" what words)))
            (tuatara.runtime:debug-report :error "~(~a~): ~a~{~%    ~a~}" (what) words trace))
          (when shown (funcall shown e trace))
          nil)
        (storage-condition (e)
          (let ((headline (tuatara.runtime:condition-words e :headline t)))
            (message "Stopped: ~a" headline)
            (tuatara.runtime:debug-report :error "~(~a~): ~a" (what) headline))
          nil)))))

(defun call-guarded (what function &rest arguments)
  "Call FUNCTION with ARGUMENTS for WHAT (a name for messages), guarded
\(CALL-WITH-GUARD): C-g in it says Quit, an error or running out of memory is
said and reported in debug mode, and the editor carries on; NIL then, else
what FUNCTION returns."
  (call-with-guard (lambda () (apply function arguments)) what 'call-guarded))

(defun run-command (editor command)
  "Run COMMAND (a symbol) with the editor context, catching errors into messages.
The windows are laid out first as they are with the key typed, so that a
panel of key hints shown before it does not shorten them for the command.
Running out of stack or heap in a command also becomes a message where SBCL can
recover, so the editor keeps running; running out of heap in the middle of a
collection cannot be recovered, which is why large work keeps within a budget.
A command that does not finish (an error, \\`C-g', a refusal) stops recorded
keys that are playing."
  (setf (editor-this-command editor) command
        (editor-repeat-map editor) nil)
  (unless (member command *not-repeatable*)
    (setf (editor-repeatable editor) (list* command (editor-prefix-argument editor) (editor-this-key editor))))
  (call-guarded 'fit-windows #'fit-windows editor)
  ;; the commands run and their keys, typing aside, for debug mode
  (unless (eq command (editor-self-insert-command editor))
    (tuatara.runtime:debug-report :command "~(~a~)~@[ (~a)~]" command
                                  (cond (*minibuffer-hidden* "keys of a secret not noted")
                                        (*this-command-keys* (tuatara.frontend:key-sequence-description *this-command-keys*)))))
  (dolist (f (editor-pre-command-functions editor)) (call-guarded f f editor command))
  (let* ((function (find-command command))
         (refusal (and *command-refusal-function*
                       ;; a mouse key's command is one of the buffer under the mouse
                       (let* ((key (editor-this-key editor))
                              (window (and (tuatara.frontend:key-p key) (mouse-key-window editor key)))
                              (*current-buffer* (if window (tuatara.display:window-buffer window) *current-buffer*)))
                         (funcall *command-refusal-function* command))))
         ;; one command, one step to undo: what it changed in the buffer it began in, taken back at
         ;; once; a command making steps of its own (playing recorded keys) moves the cell's stack on
         (undo-buffer (current-buffer))
         (undo-since (cons undo-buffer (tuatara.buffer:buffer-undo-stack undo-buffer)))
         (finished nil))
    (cond
      ((null function) (message "No command ~a" command))
      (refusal (message "~a" refusal))
      (t
        (flet ((backtrace-shown-p () (and *debug-on-error* *backtrace-function*)))
          (call-with-guard (lambda ()
                             (let ((*current-buffer* nil) ; SET-BUFFER lasts until the command ends
                                   (*command-notes* (list '()))
                                   (*command-undo-since* undo-since))
                               (funcall function)
                               (setf finished t)))
                           command 'run-command
                           :naming (lambda () (or (editor-this-command editor) command))
                           :said #'identity :trace-wanted #'backtrace-shown-p
                           :shown (lambda (e trace)
                                    (when (and trace (backtrace-shown-p)) (funcall *backtrace-function* e trace)))))))
    (unless finished (note-command-failed))
    (when (live-buffer-p undo-buffer)
      (tuatara.buffer:merge-undo-groups undo-buffer (cdr undo-since))))
  (call-guarded 'popup-follow #'popup-follow editor)
  ;; a command may name itself after what it did (EDITOR-THIS-COMMAND), so
  ;; that the next command sees that as the last one; C-u, which only starts
  ;; a count, leaves the last one as it was
  (unless (member command *not-repeatable*)
    (setf (editor-last-command editor) (or (editor-this-command editor) command)))
  (cond ((editor-transient-fresh editor) (setf (editor-transient-fresh editor) nil))
        ((and (editor-transient-map editor)
              (not (funcall (editor-transient-keep editor) command)))
         (clear-transient-map editor)))
  (let ((entry (gethash command *repeat-maps*)))
    (when entry
      (setf (editor-repeat-map editor) (car entry))
      (unless (editor-message editor)
        (echo "Repeat with ~{~a~^, ~}" (mapcar #'mark-key (cdr entry))))))
  (note-selected-buffer (tuatara.display:window-buffer (editor-window editor)))
  (dolist (f (editor-post-command-functions editor)) (call-guarded f f editor))
  ;; a command may have set what the frontend reads (M-: (setf *gui-padding* 12))
  (tuatara.frontend:frontend-settings-changed (editor-frontend editor)))

(defun collect-prefix-key (editor key)
  "While \\[universal-argument] is collecting a numeric argument, absorb KEY if
it is a digit, a minus sign or another \\[universal-argument]; return true
when absorbed."
  (let ((base (tuatara.frontend:key-base key))
        (arg (editor-prefix-argument editor)))
    (cond ((and (characterp base) (digit-char-p base)
                (not (tuatara.frontend:key-ctrl key)) (not (tuatara.frontend:key-meta key)))
           (let ((digit (digit-char-p base)))
             ;; digits after a C-u that gave 16 begin a number of their own
             (setf (editor-prefix-argument editor)
                   (cond ((eq arg :minus) (- digit))
                         ((and (integerp arg) (eq (editor-collecting-prefix editor) :digits))
                          (if (minusp arg) (- (* arg 10) digit) (+ (* arg 10) digit)))
                         (t digit))
                   (editor-collecting-prefix editor) :digits)
             (echo "\\[universal-argument] ~a-" (editor-prefix-argument editor))
             t))
          ((and (eql base #\-) (not (tuatara.frontend:key-ctrl key)) (eq arg :fresh))
           (setf (editor-prefix-argument editor) :minus) (echo "\\[universal-argument] --") t)
          ((and (eql base #\u) (tuatara.frontend:key-ctrl key) (not (tuatara.frontend:key-meta key)))
           (setf (editor-prefix-argument editor) (if (integerp arg) (* arg 4) 16)
                 (editor-collecting-prefix editor) t)
           (echo "\\[universal-argument] ~a-" (editor-prefix-argument editor)) t)
          (t nil))))

(defun repeat-binding (editor key)
  "The command KEY runs by itself right after a command it repeats
\(DEFINE-REPEAT-KEYS), while EDITOR's repeat map is in force and no sequence
or count is begun; NIL otherwise."
  (let ((map (editor-repeat-map editor)))
    (when (and map (null (editor-pending editor)) (not (editor-collecting-prefix editor)))
      (let ((binding (lookup-key map (list (tuatara.frontend:key-spec key)))))
        (and binding (not (keymap-p binding)) binding)))))

(defun key-handler-takes (editor key)
  "True when the current buffer's :KEY-HANDLER local, a function of the editor
and a key, took KEY: asked first for a key that begins no sequence, while
the minibuffer is not reading and no list of completions is shown, before
any keymap (a terminal's keys go to the program in it so), but for a key
that repeats the command just run (REPEAT-BINDING)."
  (let* ((window (editor-window editor))
         (buffer (and window (tuatara.display:window-buffer window)))
         (handler (and buffer (tuatara.buffer:buffer-local buffer :key-handler))))
    (and handler
         (null (editor-pending editor))
         (not (editor-collecting-prefix editor))
         (not (editor-minibuffer-active editor))
         (null (popup-keys editor))
         (null (repeat-binding editor key))
         (funcall handler editor key))))

(defun handle-key (editor key)
  "Add KEY to the pending sequence and run the command it completes, if any;
a key bound to nothing fails a play of recorded keys (NOTE-COMMAND-FAILED).
Right after a command that can be repeated by a key alone, KEY is first
looked up among those keys (DEFINE-REPEAT-KEYS). The current buffer's key
handler sees it before any keymap (KEY-HANDLER-TAKES)."
  (let ((recording *keys-recording*))
    ;; where the keys of a command begin, for the recording to end without those that end it
    (when (and recording (not *keys-playing*) (null (editor-pending editor)) (not (editor-collecting-prefix editor)))
      (setf (recording-command-start recording) (max 0 (1- (fill-pointer (recording-events recording)))))))
  (when (and (editor-collecting-prefix editor) (collect-prefix-key editor key))
    (return-from handle-key nil))
  (when (key-handler-takes editor key)
    (setf (editor-this-key editor) key
          (editor-repeat-map editor) nil)
    (return-from handle-key nil))
  (let ((binding (repeat-binding editor key)))
    (when (and (editor-repeat-map editor) (null (editor-pending editor)) (not (editor-collecting-prefix editor)))
      (setf (editor-repeat-map editor) nil))
    (when binding
      (setf (editor-this-key editor) key
            (editor-message editor) nil)
      (run-command editor binding)
      (setf (editor-prefix-argument editor) nil)
      (return-from handle-key nil)))
  (when (editor-collecting-prefix editor)
    ;; the first key that is not part of the count ends the collection; a bare
    ;; C-u means 4, a lone minus means -1
    (setf (editor-collecting-prefix editor) nil)
    (case (editor-prefix-argument editor)
      (:fresh (setf (editor-prefix-argument editor) 4))
      (:minus (setf (editor-prefix-argument editor) -1))))
  (let* ((spec (tuatara.frontend:key-spec key))
         (specs (append (editor-pending editor) (list spec)))
         (binding (or (resolve-keys editor specs key)
                      (and (null (editor-pending editor))
                           (characterp (tuatara.frontend:key-base key))
                           (not (tuatara.frontend:key-ctrl key))
                           (not (tuatara.frontend:key-meta key))
                           (editor-self-insert-command editor)))))
    (setf (editor-this-key editor) key)
    (cond ((keymap-p binding)
           (setf (editor-pending editor) specs)
           (echo "~a-" (tuatara.frontend:key-sequence-description specs)))
          ((and (null binding) (editor-pending editor) *prefix-help-function*
                (equal (tuatara.frontend:key-description spec) *prefix-help-key*))
           (let ((prefix (editor-pending editor)))
             (setf (editor-pending editor) '()
                   (editor-message editor) nil)
             (funcall *prefix-help-function* editor prefix)))
          ((and (null binding) (eq (tuatara.frontend:key-base key) :up-mouse-1))
           ;; a button let go of where nothing is bound to it says nothing, leaving what its press said
           (setf (editor-pending editor) '()))
          ((null binding)
           (setf (editor-pending editor) '())
           (message "~a is undefined" (tuatara.frontend:key-sequence-description specs))
           (note-command-failed))
          (t
           (setf (editor-pending editor) '())
           (setf (editor-message editor) nil)
           (let ((*this-command-keys* specs))
             (run-command editor binding))
           (unless (editor-collecting-prefix editor)
             (setf (editor-prefix-argument editor) nil))))))
