;;;; src/command/dispatch.lisp --- Reading keys and running the commands they are bound to.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defgeneric host-state-keymaps (host buffer state-keymap)
  (:documentation "The keymaps consulted just before STATE-KEYMAP, the modal state's: the keys
bound for BUFFER's major mode in that state; on the core's own host, none.")
  (:method ((host tuatara.runtime:host) buffer state-keymap)
    (declare (ignore buffer state-keymap))
    nil))

(defun buffer-override-keymap (buffer)
  "The keymap BUFFER has for itself above its modes' keys while something
there wants keys of its own (a snippet being filled in, a file waiting for its
passphrase, a viewer's page keys), or NIL."
  (tuatara.buffer:buffer-local buffer :override-keymap))

(defun (setf buffer-override-keymap) (keymap buffer)
  "Give BUFFER KEYMAP above its modes' keys (BUFFER-OVERRIDE-KEYMAP); NIL for
none."
  (setf (tuatara.buffer:buffer-local buffer :override-keymap) keymap))

(defun mouse-key-window (editor key)
  "The window of EDITOR whose text KEY, a mouse key, happened over, or NIL for
a key of the keyboard or one elsewhere (WINDOW-AT-CELL)."
  (and (tuatara.frontend:key-column key) (tuatara.frontend:key-row key)
       (window-at-cell (tuatara.frontend:key-column key) (tuatara.frontend:key-row key) editor)))

(defun active-keymaps (editor &optional key)
  "The keymaps consulted for a key, in precedence order.
First, the keys of the list of completions beside the cursor while one is
shown (COMPLETE-IN-TEXT). Then the question's own and the minibuffer's while it
reads (READ-STRING). Otherwise come a transient map in force
\(SET-TRANSIENT-MAP) and the current buffer's own keys for a passing use of it
\(BUFFER-OVERRIDE-KEYMAP, as a quick edit's from a rimu list). After them come
the keys bound for its major mode in the modal state (HOST-STATE-KEYMAPS) and
the modal state map. Then come the current buffer's local keymap (its major
mode's, in the :KEYMAP local, whose parents are the parent modes' keymaps), the
fallback map, and the global map.
For KEY, when given and a mouse key over a window's text, the buffer is that
window's rather than the current one, so that a click acts as that buffer's
keys say at once."
  (let ((maps '()))
    (flet ((add (map) (when map (push map maps))))
      (add (popup-keys editor))
      (if (editor-minibuffer-active editor)
          (progn (add *minibuffer-question-keymap*)
                 (add (editor-minibuffer-keymap editor)))
          (let* ((window (or (and key (mouse-key-window editor key)) (editor-window editor)))
                 (buffer (and window (tuatara.display:window-buffer window)))
                 (state (editor-state-keymap editor)))
            (add (editor-transient-map editor))
            (when buffer
              (add (buffer-override-keymap buffer))
              (when state
                (dolist (map (host-state-keymaps tuatara.runtime:*host* buffer state)) (add map))))
            (add state)
            (when buffer (add (tuatara.buffer:buffer-local buffer :keymap)))
            (add (editor-fallback-keymap editor))
            (add (editor-global-keymap editor)))))
    (nreverse maps)))

(defgeneric host-show-prefix-help (host editor prefix)
  (:documentation "List every key under PREFIX, a prefix key's specs, when \\`C-h'
\(*PREFIX-HELP-KEY*) follows it in EDITOR and nothing is bound to it there; on
the core's own host, say the keys are undefined.")
  (:method ((host tuatara.runtime:host) editor prefix)
    (declare (ignore editor))
    (message "~a ~a is undefined" (tuatara.frontend:key-sequence-description prefix) *prefix-help-key*)
    (note-command-failed)))

(defun resolve-keys (editor specs &optional key)
  "Look SPECS up in the active keymaps in precedence order, KEY the last of
them as typed when known (ACTIVE-KEYMAPS). Returns a command, a keymap
\(prefix), or NIL. A key bound to :UNDEFINED in a keymap is unbound there and
in the keymaps after it: NIL, as a key bound nowhere. Escape alone, bound
nowhere, runs what \\`C-g' runs there: it leaves INSERT state, drops a
selection, closes a list beside the cursor."
  (let ((keymaps (active-keymaps editor key)))
    (or (keymaps-lookup keymaps specs)
        (and (equal specs '((:escape))) (keymaps-lookup keymaps '((:ctrl #\g)))))))

(define-setting tuatara-user:*escape-quits* nil
  "Whether Escape does what \\`C-g' does everywhere.
In a terminal, Escape quickly followed by another key arrives as Meta with
that key, as terminals send the two alike. \\[quoted-insert] still takes
Escape as itself.
- t: Escape leaves INSERT state, closes a question, a list or a popup, stops
  a wait or work in the background, and drops a selection.
- nil: Escape is a key of its own, which does what it is bound to (it closes
  a question typed in the minibuffer). It does what \\`C-g' does where it is
  bound to nothing (it leaves INSERT state, drops a selection), but does not
  end a question one key answers, a wait or work in the background (the
  standard)."
  :values boolean)

(defvar *keys-as-typed* nil
  "True while a key is read as it was typed, Escape as Escape whatever
*ESCAPE-QUITS* says (\\[quoted-insert]).")

(defun keys-as-typed-here-p ()
  "True when the current buffer wants keys as they were typed, Escape as
Escape: its :KEYS-AS-TYPED local, a function of the buffer saying so (a
terminal while it is typed into)."
  (let* ((buffer (selected-window-buffer))
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

(define-record (recording (:constructor make-recording ()) (:copier nil))
  "Keys being recorded: EVENTS, the key events read so far, in order, and
COMMAND-START, how many of them came before the keys of the command running
now."
  (events (make-array 64 :adjustable t :fill-pointer 0) :read-only t
          :documentation "The key events RECORDING has read so far, in order.")
  (command-start 0 :type fixnum
                 :documentation "How many of RECORDING's events came before the keys of the command running now."))

(setf (documentation 'make-recording 'function) "A RECORDING with no keys in it yet.")

(define-record (playing (:constructor make-playing (events)) (:copier nil))
  "Recorded keys being played once: EVENTS, a vector of key events; NEXT, the
index of the next one to give; FAILED, once a command among them failed, the
words it left in the echo area (or T); RAN-OUT, when the keys had all been
given while a question still waited on work in the background, the time
they had."
  (events #() :type simple-vector :read-only t :documentation "The key events PLAYING gives, a vector.")
  (next 0 :type fixnum :documentation "The index in PLAYING's events of the next one to give.")
  (failed nil
          :documentation "The words a failed command left in the echo area, or T, once one of PLAYING's commands failed;
:QUIT when it was quit; else NIL.")
  (ran-out nil
           :documentation "The time, in internal time units, at which PLAYING's keys had all been given while a question
still waited on work in the background, or NIL."))

(setf (documentation 'make-playing 'function) "A PLAYING of EVENTS from their first.")

(defparameter *played-keys-wait* 10
  "The most seconds a question may go on waiting for work in the background
once the recorded keys played into it have all been given; after that the
play fails, as the keys ended before the question did.")

(defun keys-playing-p ()
  "True while recorded keys are being played into the frame you are in."
  (let ((editor *editor*))
    (and editor (editor-keys-playing editor) t)))

(defun keys-recording-p ()
  "True while keys are being recorded in the frame you are in."
  (let ((editor *editor*))
    (and editor (editor-keys-recording editor) t)))

(defun note-command-failed ()
  "Say that the command running now did not do what it was asked, as a search
that found nothing: while recorded keys are played, the play stops after it,
the words in the echo area saying why."
  (let ((playing (and *editor* (editor-keys-playing *editor*))))
    (when (and playing (not (playing-failed playing)))
      (setf (playing-failed playing) (or (editor-message *editor*) t)))))

(defun event-of-no-frame (event)
  "EVENT as recorded keys keep it: without the frame it happened in, so that
it holds on to no frame and plays in any."
  (if (tuatara.frontend:event-frame event)
      (tuatara.frontend:make-event (tuatara.frontend:event-kind event)
                                   :key (tuatara.frontend:event-key event) :text (tuatara.frontend:event-text event)
                                   :width (tuatara.frontend:event-width event)
                                   :height (tuatara.frontend:event-height event)
                                   :files (tuatara.frontend:event-files event)
                                   :column (tuatara.frontend:event-column event)
                                   :row (tuatara.frontend:event-row event))
      event))

(defun recorded-as-p (recorded event)
  "True when RECORDED, among the keys being recorded, is EVENT as it was
recorded (EVENT-OF-NO-FRAME)."
  (or (eq recorded event)
      (and (eq (tuatara.frontend:event-kind recorded) (tuatara.frontend:event-kind event))
           (eq (tuatara.frontend:event-key recorded) (tuatara.frontend:event-key event))
           (eq (tuatara.frontend:event-text recorded) (tuatara.frontend:event-text event)))))

(defun record-event (recording event)
  "Add EVENT, a key or text pasted just read as typed, to RECORDING, the keys
being recorded, the paste to be pasted again as it was, without the frame it
happened in (EVENT-OF-NO-FRAME); the mouse is not recorded. What is typed
while a secret is asked for or at hand (a passphrase) is never recorded: one
event of kind :SECRET stands in for all of it, where a play stops and leaves
the question to you (PLAYED-EVENT)."
  (let ((events (recording-events recording)))
    (cond ((or *minibuffer-hidden* tuatara.runtime:*secret-at-hand*)
           (unless (and (plusp (fill-pointer events))
                        (eq (tuatara.frontend:event-kind (aref events (1- (fill-pointer events)))) :secret))
             (vector-push-extend (tuatara.frontend:make-event :secret) events)))
          ((not (and (eq (tuatara.frontend:event-kind event) :key)
                     (tuatara.frontend:key-row (tuatara.frontend:event-key event))))
           (vector-push-extend (event-of-no-frame event) events)))))

(defun give-up-keys-recording ()
  "End the recording of keys going on in the frame you are in, with nothing
kept: what \\`C-g' does when it gives up a command or a question while keys
are recorded (but not while recorded keys play)."
  (let ((editor *editor*))
    (when (and editor (not (editor-keys-playing editor)))
      (setf (editor-keys-recording editor) nil))))

(defun played-event (editor playing timeout ticks)
  "The next key event of PLAYING; after \\`C-g' stopped the play
\(QUIT-WHILE-PLAYING-P), a quit instead. Once all have been given, a
question that holds back what it was given while its candidates still come
has a :TICK after at most TIMEOUT seconds (a twentieth at most), for up to
*PLAYED-KEYS-WAIT* seconds, keys typed meanwhile kept for after the play and
\\`C-g' stopping it; otherwise an error: the keys ended before what they were
answering did. Where a secret was typed (RECORD-EVENT) the play stops: the
rest of its keys are left unplayed, the plays after it too, and NIL comes
back, the question asking then shown and answered by you."
  (let ((events (playing-events playing))
        (next (playing-next playing))
        (completion (editor-completion editor)))
    (cond ((eq (playing-failed playing) :quit) (keyboard-quit-signal))
          ((and (< next (length events)) (eq (tuatara.frontend:event-kind (svref events next)) :secret))
           (setf (playing-next playing) (length events)
                 (playing-failed playing) "a secret was typed there, which is never recorded"
                 (frame-keys-playing (editor-frame editor)) nil)
           (redisplay-or-say editor)
           nil)
          ((< next (length events))
           (setf (playing-next playing) (1+ next))
           (svref events next))
          ((and ticks completion (completion-done-pending completion) (completion-working completion)
                (< (tuatara.runtime:milliseconds-since (or (playing-ran-out playing)
                                                             (setf (playing-ran-out playing) (get-internal-real-time))))
                   (* 1000 *played-keys-wait*)))
           (if (tuatara.frontend:frontend-input-pending-p (frame-frontend (editor-frame editor)))
               (when (pending-quit-p editor) (keyboard-quit-signal))
               (sleep (min (or timeout 1/20) 1/20)))
           (tuatara.frontend:make-event :tick))
          (t (refuse "The recorded keys ended before the question they were answering")))))

(defun put-back-events (editor events)
  "Have EVENTS, keys read before they could be handled, read again next, in
order, before any other: back among the recorded keys being played when they
are the last given from them, else first among EDITOR's unread events."
  (let* ((playing (editor-keys-playing editor))
         (recording (editor-keys-recording editor))
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
                               always (recorded-as-p (aref recorded i) event)))
                (setf (fill-pointer recorded) start))))
          (setf (editor-unread editor) (append events (editor-unread editor)))))))

(defparameter *frames-question-depth* 8
  "The most questions open at once, each in a frame of its own, one asked
inside another's: past it, what is done in another frame waits until the
innermost question ends.")

(defun frame-answers-now-p (editor frame)
  "True when what is done in FRAME, one of EDITOR's frames other than the one
worked in, which asks a question, runs now, inside that question
\(RUN-INSIDE-QUESTION): FRAME has no question of its own waiting
\(FRAME-HELD), and fewer than *FRAMES-QUESTION-DEPTH* questions are open."
  (and (not (frame-held frame))
       (< (1+ (count-if #'frame-held (editor-frames editor))) *frames-question-depth*)))

(defun other-frames-unread (editor)
  "The next event left unread in another frame of EDITOR than the one worked
in, which has none, taken from that frame's; NIL when there is none."
  (let ((frames (editor-frames editor)))
    (when (rest frames)
      (let ((frame (find-if #'frame-unread frames)))
        (and frame (pop (frame-unread frame)))))))

(defun run-other-frames-unread (editor)
  "While the frame of EDITOR worked in asks a question, run inside it the next
event left unread in another frame whose events run now (FRAME-ANSWERS-NOW-P),
as done there (RUN-INSIDE-QUESTION): true when there was one. A window given
the keyboard is let go: the frame you go on in becomes the one worked in as
you do something there."
  (let ((frames (editor-frames editor)))
    (when (rest frames)
      (let ((frame (find-if (lambda (frame)
                              (and (frame-unread frame) (not (eq frame (editor-frame editor)))
                                   (frame-answers-now-p editor frame)))
                            frames)))
        (when frame
          (let ((event (pop (frame-unread frame))))
            (when (tuatara.frontend:input-event-p event)
              (run-inside-question editor frame event)))
          t)))))

(defun tell-frame-waits (editor frame)
  "Say in FRAME, one of EDITOR's frames, that what is typed there waits for
the question asked in another frame, and paint it so."
  (let ((words (substitute-command-keys "Waiting for the question in another frame (\\`C-g' there ends it)")))
    (unless (equal (frame-message frame) words)
      (setf (frame-message frame) words)
      (paint-other-frame editor frame))))

(defun take-to-its-frame (editor event reading)
  "Deal with EVENT, just read, as the frame of EDITOR it happened in wants.
True when it is to be handled now, in the frame worked in. One of no frame, or
of the frame worked in, is. One of a frame that has gone is dropped. READING
says what read it.
While a command works (NIL), what happened in another frame waits among that
frame's unread events for its turn.
Otherwise a window of another frame resized is drawn again there. Between
commands (:BETWEEN-COMMANDS, the command loop's own read), what you do in
another frame, and its window given the keyboard, makes it the one worked in.
While the frame worked in asks a question (:QUESTION), what you do in another
frame runs there at once, inside the question (RUN-INSIDE-QUESTION). The
exception is when that frame has a question of its own waiting or as many
questions are open as may be (FRAME-ANSWERS-NOW-P). Then it waits among the
frame's unread events, the frame saying so (TELL-FRAME-WAITS), but for its
window closed, which closes it at once, its question given up once it is
reached again.
A window given the keyboard meanwhile is let go. A frame whose screen went
away (:DETACHED) is closed at once. Words its frontend has (:SAID) wait for
that frame's turn."
  (let ((frame (tuatara.frontend:event-frame event)))
    (flet ((wait-there ()
             (setf (frame-unread frame) (append (frame-unread frame) (list event)))
             nil))
      (cond ((or (null frame) (eq frame (editor-frame editor))) t)
            ((frame-deleted frame) nil)
            ((null reading) (wait-there))
            ((eq (tuatara.frontend:event-kind event) :resize)
             (call-in-frame editor frame (lambda ()
                                           (call-with-question-of frame
                                                                  (lambda ()
                                                                    (resize editor (tuatara.frontend:event-width event)
                                                                            (tuatara.frontend:event-height event))))))
             (paint-other-frame editor frame)
             nil)
            ((eq reading :between-commands)
             (when (or (tuatara.frontend:input-event-p event) (eq (tuatara.frontend:event-kind event) :focus))
               (select-frame editor frame))
             t)
            ;; a client gone: its frame closed now, its question given up once reached; its words for later
            ((eq (tuatara.frontend:event-kind event) :detached) (close-screen-gone editor frame) nil)
            ((eq (tuatara.frontend:event-kind event) :said)
             (setf (frame-deferred frame) (append (frame-deferred frame) (list event)))
             nil)
            ((not (tuatara.frontend:input-event-p event)) nil)
            ((and (frame-held frame) (eq (tuatara.frontend:event-kind event) :close))
             (remove-frame editor frame)
             nil)
            ((not (frame-answers-now-p editor frame))
             (wait-there)
             (tell-frame-waits editor frame)
             nil)
            ;; what was done there before waits no more: it runs first, in turn (RUN-OTHER-FRAMES-UNREAD)
            ((frame-unread frame) (wait-there))
            (t (run-inside-question editor frame event)
               nil)))))

(defun keep-or-quit-event (editor event)
  "Deal with EVENT, read from the frontend while EDITOR is busy with work of
its own: true when it is \\`C-g' (QUIT-KEY-P), which is to stop that work.
Any other key is kept to be handled after it, as typed (EDITOR-UNREAD); a
window resized is drawn again; a window closed or gone, files dropped, text
pasted or words to say are dealt with after it. What happened in another
frame waits for its turn there (TAKE-TO-ITS-FRAME): \\`C-g' there stops nothing here."
  (case (and (take-to-its-frame editor event nil) (tuatara.frontend:event-kind event))
    (:key (or (quit-key-p (tuatara.frontend:event-key event))
              (progn (setf (editor-unread editor) (append (editor-unread editor) (list event)))
                     nil)))
    (:resize (resize editor (tuatara.frontend:event-width event) (tuatara.frontend:event-height event))
             (tuatara.runtime:ignoring-errors-reported ("drawing the resized window") (redisplay editor))
             nil)
    ((:close :drop :paste :detached :said)
     (setf (editor-deferred editor) (append (editor-deferred editor) (list event)))
     nil)
    (t nil)))

(defun pending-quit-p (editor)
  "Read what is waiting to be read from EDITOR's frontend while EDITOR is busy
with work of its own: true when \\`C-g' is among it, the other events dealt
with as KEEP-OR-QUIT-EVENT deals with them. NIL at once when nothing waits."
  (let ((frontend (frame-frontend (editor-frame editor))))
    (loop repeat 64
          while (tuatara.frontend:frontend-input-pending-p frontend)
            thereis (keep-or-quit-event editor (tuatara.frontend:frontend-read-event-timeout frontend 1/100)))))

(defun read-event (editor &key timeout ticks pastes between-commands)
  "Read the next event from the frontend, handling a resize here: the screen is
painted again at the new size at once.
With TICKS (the command loop), an event of kind :TICK comes back when another
thread wakes the editor, after a resize, and when no key arrives within
TIMEOUT seconds. :FOCUS comes back when a frame's window is given the
keyboard. Without TICKS, only keys and :QUIT come back, for the loops reading
keys inside a command (the minibuffer, search). The window closed, files
dropped or text pasted there end the command's question as \\`C-g' does, and
are handled once the command is done (EDITOR-DEFERRED). A question whose frame
was closed meanwhile is given up so too. With PASTES (the minibuffer), text
pasted comes back too.
While it waits the editor counts as idle (TUATARA.RUNTIME:NOTE-EDITOR-BUSY).
While recorded keys are played their keys come first (PLAYED-EVENT). While
keys are recorded each key and paste read as typed is added (RECORD-EVENT).
An event of another frame goes to that frame (TAKE-TO-ITS-FRAME). With
BETWEEN-COMMANDS, for the command loop's read alone, it may make that frame
the one worked in, and what was left unread in another frame comes once the
frame worked in has none left. Without it, what is done in another frame runs
there inside the question (RUN-INSIDE-QUESTION), what was left unread there
first (RUN-OTHER-FRAMES-UNREAD). With several frames, or in an editor running
in the background, the editor woken takes in what work in the background has
for the others."
  (let ((asking (editor-frame editor))
        (reading (if between-commands :between-commands :question)))
    (flet ((from-frontend (frame)
             (tuatara.runtime:note-editor-busy nil)
             (prog1 (if timeout
                        (tuatara.frontend:frontend-read-event-timeout (frame-frontend frame) timeout)
                        (tuatara.frontend:frontend-read-event (frame-frontend frame)))
               (tuatara.runtime:note-editor-busy t))))
      (loop
        ;; the frame asking was closed by what ran inside its question: the question is given up there
        (when (and (not between-commands) (frame-deleted asking))
          (setf (editor-frame editor) asking)
          (keyboard-quit-signal))
        (let* ((frame (editor-frame editor))
               (playing (frame-keys-playing frame))
               (event (cond (playing (played-event editor playing timeout ticks))
                            ((frame-unread frame) (pop (frame-unread frame)))
                            (between-commands (or (other-frames-unread editor) (from-frontend frame)))
                            ((run-other-frames-unread editor) nil)
                            (t (from-frontend frame)))))
          ;; keys played are the frame's that plays them, wherever they were recorded
          (when (and event (or playing (take-to-its-frame editor event reading)))
            (let ((recording (editor-keys-recording editor)))
              (case (tuatara.frontend:event-kind event)
                (:resize (resize editor (tuatara.frontend:event-width event) (tuatara.frontend:event-height event))
                         (if ticks
                             (return (tuatara.frontend:make-event :tick))
                             (redisplay-or-say editor)))
                ((:tick :focus)
                 ;; a wake while a question is open: what other frames' work brought is taken in now, and
                 ;; in an editor in the background, the clients that come and its ending
                 (when (and (not between-commands) (not playing) (eq (tuatara.frontend:event-kind event) :tick)
                            (or (rest (editor-frames editor)) (editor-idle-frontend editor)))
                   (run-inside-question editor nil nil))
                 (when ticks (return event)))
                ;; words from the frontend: said once the question is done, which goes on
                (:said (if ticks
                           (return event)
                           (setf (editor-deferred editor) (append (editor-deferred editor) (list event)))))
                ((:close :drop :paste :detached)
                 (if (or ticks (and pastes (eq (tuatara.frontend:event-kind event) :paste)))
                     (progn (when (and recording (not playing) (eq (tuatara.frontend:event-kind event) :paste))
                              (record-event recording event))
                            (return event))
                     (progn (setf (editor-deferred editor) (append (editor-deferred editor) (list event)))
                            (keyboard-quit-signal))))
                (t (return (if playing
                               event
                               (let ((event (escape-as-quit event)))
                                 (when recording (record-event recording event))
                                 event))))))))))))

(defvar *repeat-maps* (make-hash-table :test 'eq)
  "For each command that can be repeated by a key alone, the keymap of the keys
that then repeat it and its kin, and those keys as written, as (KEYMAP .
KEYS) (DEFINE-REPEAT-KEYS).")

(defvar *not-repeatable* '(universal-argument)
  "Commands the repeat command never repeats: the one that starts a count, and
repeat itself, which the defaults add.")

(defvar *repeat-after* (make-hash-table :test 'eq)
  "For the keymap of a group of repeat keys made with an AFTER key
(DEFINE-REPEAT-KEYS), the key spec its commands must have been run from.")

(defvar *repeat-key-run* nil
  "True while a command runs from one of the keys of a repeat keymap, so that
its repeat keys stay on.")

(defun define-repeat-keys (bindings &key after)
  "Make the commands of BINDINGS, a list of (KEY COMMAND), repeatable by their
keys alone: once any of them has run from its key sequence, each of those KEYS
runs its command straight away, until another key is pressed, which then does
what it always does. With AFTER, a key such as \"C-z\", this holds only for a
command run by a key sequence that began with it, or by one of those keys
themselves. Returns the keymap."
  (let* ((map (make-keymap :name "repeat"))
         (entry (cons map (mapcar #'first bindings))))
    (when after
      (setf (gethash map *repeat-after*) (first (tuatara.frontend:parse-key-sequence after))))
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

(define-setting *debug-on-error* nil
  "Whether a command or an evaluation that meets an error shows the calls under
way, in *Backtrace*.
\\[toggle-debug-on-error] switches it.
- t: the calls under way are shown.
- nil: only the error is said (the standard)."
  :values boolean)

(defgeneric host-show-backtrace (host condition calls)
  (:documentation "Show CALLS, those under way when CONDITION was signalled (a list of strings,
innermost first, see CAPTURE-BACKTRACE): the defaults show *Backtrace*; the
core's own host, nothing.")
  (:method ((host tuatara.runtime:host) condition calls)
    (declare (ignore condition calls))
    nil))

(defparameter *backtrace-frames* 200
  "The most calls a backtrace keeps, the innermost.")

(defun repeat-hint (command)
  "The words naming the keys that repeat COMMAND by themselves right after it
\(DEFINE-REPEAT-KEYS), or NIL when none do."
  (let ((entry (gethash command *repeat-maps*)))
    (and entry (format nil "Repeat with ~{~a~^, ~}" (mapcar #'mark-key (cdr entry))))))

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

(defgeneric host-command-refusal (host command)
  (:documentation "What to say instead of running COMMAND, a command's name, now, or NIL when it
may run: the defaults refuse a command made for one kind of buffer, such as a
list's own keys, anywhere else; the core's own host, none.")
  (:method ((host tuatara.runtime:host) command)
    (declare (ignore command))
    nil))

(defun call-with-guard (thunk what stop &key (quit "Quit") said naming trace-wanted shown)
  "Call THUNK, a function of no arguments, for WHAT, a name for messages, so
that the editor carries on whatever it meets. C-g in it says QUIT (NIL to say
nothing). An error is said, Error in WHAT: and its words, or as SAID, a
function of the words, has it. It is reported in debug mode's log with the
calls under way within THUNK. These are made in the function named STOP, under
the name NAMING, a function of no arguments, gives then, when given. Running
out of memory or stack says Stopped and what ran out. The calls are captured
in debug mode, or when TRACE-WANTED, a function of no arguments, says so, and
SHOWN, when given, is called with the error and them. The value is NIL then,
else what THUNK returns.
Work in a buffer whose text is secret (TUATARA.RUNTIME:SECRET-BUFFER-P) counts
as a secret at hand: the debug log gets no strings of it."
  (let ((trace nil)
        (secret-then nil)
        (tuatara.runtime:*secret-at-hand* (or tuatara.runtime:*secret-at-hand*
                                              (tuatara.runtime:secret-buffer-p
                                               (or *current-buffer* (selected-window-buffer))))))
    (flet ((what () (if naming (funcall naming) what)))
      (handler-case
          (handler-bind ((error (lambda (e)
                                  ;; a secret at hand where it was signalled stays so as it is said
                                  (setf secret-then tuatara.runtime:*secret-at-hand*)
                                  (when (and (or tuatara.runtime:*debug-on* (and trace-wanted (funcall trace-wanted)))
                                             (not (typep e 'quit)))
                                    (setf trace (capture-backtrace :stop stop))))))
            (funcall thunk))
        (quit (c) (when quit (message "~a" (or (quit-words c) quit))) nil)
        (error (e)
          (let* ((tuatara.runtime:*secret-at-hand* (or tuatara.runtime:*secret-at-hand* secret-then))
                 (words (tuatara.runtime:failure-words e :said t)))
            (message "~a" (if said (funcall said words) (format nil "Error in ~(~a~): ~a" what words)))
            (tuatara.runtime:debug-report :error "~(~a~): ~a~{~%    ~a~}"
                                          (what) (tuatara.runtime:failure-words e) trace)
            (when shown (funcall shown e trace)))
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
  (declare (dynamic-extent arguments))
  (flet ((call () (apply function arguments)))
    (declare (dynamic-extent #'call))
    (call-with-guard #'call what 'call-guarded)))

(defun repeat-keys-on-p (map)
  "True when the repeat keys of the keymap MAP go on after the command that ran
now: it has no AFTER key (DEFINE-REPEAT-KEYS), or was run from a key sequence
beginning with it, or from a repeat key."
  (let ((after (gethash map *repeat-after*)))
    (or (null after) *repeat-key-run* (equal (first *this-command-keys*) after))))

(defun run-command (editor command)
  "Run COMMAND (a symbol) with the editor context, catching errors into messages.
An edit it makes in the buffer it runs in, reaching outside the part of it
shown when that buffer is narrowed, is refused.
The windows are laid out first as they are with the key typed, so that a
panel of key hints shown before it does not shorten them for the command.
Running out of stack or heap in a command also becomes a message where Steel
Bank Common Lisp (SBCL) can recover, so the editor keeps running; running out
of heap in the middle of a collection cannot be recovered, which is why large
work keeps within a budget. A command that does not finish (an error, \\`C-g',
a refusal) stops recorded keys that are playing."
  (setf (editor-this-command editor) command
        (editor-repeat-map editor) nil)
  (unless (member command *not-repeatable*)
    (setf (editor-repeatable editor) (list* command (editor-prefix-argument editor) (editor-this-key editor))))
  (call-guarded 'fit-windows #'fit-windows editor)
  ;; the commands run and their keys, typing aside, for debug mode
  (unless (eq command (editor-self-insert-command editor))
    (tuatara.runtime:debug-report :command "~(~a~)~@[ (~a)~]" command
                                  (cond (*minibuffer-hidden* "keys of a secret not noted")
                                        (*this-command-keys*
                                         (tuatara.frontend:key-sequence-description *this-command-keys*)))))
  (dolist (f (editor-pre-command-functions editor)) (call-guarded f f editor command))
  (let* ((function (find-command command))
         ;; a mouse key's command is one of the buffer under the mouse
         (refusal (let* ((key (editor-this-key editor))
                         (window (and (tuatara.frontend:key-p key) (mouse-key-window editor key)))
                         (*current-buffer* (if window (tuatara.display:window-buffer window) *current-buffer*)))
                    (host-command-refusal tuatara.runtime:*host* command)))
         ;; one command, one step to undo: what it changed in the buffer it began in, taken back at
         ;; once; a command making steps of its own (playing recorded keys) moves the cell's stack on
         (undo-buffer (current-buffer))
         (undo-since (cons undo-buffer (tuatara.buffer:buffer-undo-stack undo-buffer)))
         (finished nil))
    (cond
      ((null function) (message "No command ~a" command))
      (refusal (message "~a" refusal))
      (t
        (flet ((backtrace-shown-p () *debug-on-error*)
               (call ()
                 (let ((*current-buffer* nil) ; SET-BUFFER lasts until the command ends
                       (tuatara.buffer:*narrowing-in-force* undo-buffer)
                       (*command-notes* (list '()))
                       (*command-undo-since* undo-since))
                   (funcall function)
                   (setf finished t)))
               (naming () (or (editor-this-command editor) command)))
          (flet ((shown (e trace)
                   (when (and trace (backtrace-shown-p))
                     (host-show-backtrace tuatara.runtime:*host* e trace))))
            (declare (dynamic-extent #'backtrace-shown-p #'call #'naming #'shown))
            (call-with-guard #'call command 'run-command
                             :naming #'naming :said #'identity :trace-wanted #'backtrace-shown-p
                             :shown #'shown)))))
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
    (when (and entry (repeat-keys-on-p (car entry)))
      (setf (editor-repeat-map editor) (car entry))
      (unless (editor-message editor)
        (echo "~a" (repeat-hint command)))))
  (note-selected-buffer (selected-window-buffer editor))
  (dolist (f (editor-post-command-functions editor)) (call-guarded f f editor))
  ;; a command may have set what the frontends read (M-: (setf *gui-padding* 12))
  (dolist (frame (editor-frames editor))
    (tuatara.frontend:frontend-settings-changed (frame-frontend frame))))

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

(defun key-handler-takes (editor key repeat)
  "True when the current buffer's :KEY-HANDLER local, a function of the editor
and a key, took KEY. It is asked first for a key that begins no sequence,
while the minibuffer is not reading and no list of completions is shown,
before any keymap (a terminal's keys go to the program in it so). It is not
asked for a key that repeats the command just run, REPEAT being the command
it repeats or NIL (REPEAT-BINDING)."
  (let* ((buffer (selected-window-buffer editor))
         (handler (and buffer (tuatara.buffer:buffer-local buffer :key-handler))))
    (and handler
         (null (editor-pending editor))
         (not (editor-collecting-prefix editor))
         (not (editor-minibuffer-active editor))
         (null (popup-keys editor))
         (null repeat)
         (funcall handler editor key))))

(defun handle-key (editor key)
  "Add KEY to the pending sequence and run the command it completes, if any;
a key bound to nothing fails a play of recorded keys (NOTE-COMMAND-FAILED).
Right after a command that can be repeated by a key alone, KEY is first
looked up among those keys (DEFINE-REPEAT-KEYS). The current buffer's key
handler sees it before any keymap (KEY-HANDLER-TAKES). \\`C-g' after a
prefix key, bound nowhere after it, gives up the keys typed so far and the
count, and does nothing else."
  (let ((recording (editor-keys-recording editor)))
    ;; where the keys of a command begin, for the recording to end without those that end it
    (when (and recording (not (editor-keys-playing editor)) (null (editor-pending editor))
               (not (editor-collecting-prefix editor)))
      (setf (recording-command-start recording) (max 0 (1- (fill-pointer (recording-events recording)))))))
  (when (and (editor-collecting-prefix editor) (collect-prefix-key editor key))
    (return-from handle-key nil))
  (let ((binding (repeat-binding editor key)))
    (when (key-handler-takes editor key binding)
      (setf (editor-this-key editor) key
            (editor-repeat-map editor) nil)
      (return-from handle-key nil))
    (when (and (editor-repeat-map editor) (null (editor-pending editor)) (not (editor-collecting-prefix editor)))
      (setf (editor-repeat-map editor) nil))
    (when binding
      (setf (editor-this-key editor) key
            (editor-message editor) nil)
      (let ((*repeat-key-run* t))
        (run-command editor binding))
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
          ((and (null binding) (editor-pending editor)
                (equal (tuatara.frontend:key-description spec) *prefix-help-key*))
           (let ((prefix (editor-pending editor)))
             (setf (editor-pending editor) '()
                   (editor-message editor) nil)
             (host-show-prefix-help tuatara.runtime:*host* editor prefix)))
          ((and (null binding) (editor-pending editor) (equal spec '(:ctrl #\g)))
           ;; C-g after a prefix key gives up the keys typed so far, and nothing else
           (setf (editor-pending editor) '()
                 (editor-prefix-argument editor) nil)
           (message "Quit"))
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
