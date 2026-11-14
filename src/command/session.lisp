;;;; src/command/session.lisp --- The editor itself: making it, its command loop, and running it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defun make-editor (frontend)
  "Return an editor painting through FRONTEND with the minibuffer keys
installed, its one who started it its first client, on its own screen."
  (let ((editor (%make-editor frontend))
        (client (make-client)))
    (setf (frame-client (editor-frame editor)) client
          (editor-clients editor) (list client))
    (install-minibuffer-keys (editor-minibuffer-keymap editor))
    editor))

(defun show-buffer (buffer)
  "Show BUFFER in the selected window of the editor, where that window last showed it if it
did. A window kept for other buffers (PLACE-BUFFERS with :keep) is left as it
is: BUFFER is shown in another window, which is selected. A buffer that has
been killed is not shown: that is an error. BUFFER becomes the buffer of the
client who works in the frame too (JOIN-CLIENT)."
  (unless (live-buffer-p buffer)
    (refuse "~a has been killed" (tuatara.buffer:buffer-name buffer)))
  (join-client buffer (current-client))
  (let* ((editor *editor*)
         (window (editor-window editor)))
    (if (window-kept-elsewhere-p window buffer editor)
        (pop-to-buffer buffer :other t)
        (progn
          (when (one-window-p editor)
            (setf (tuatara.display:window-kept window) nil))
          (tuatara.display:set-window-buffer window buffer)))
    buffer))

(defvar *tick-functions* '()
  "Functions of the editor for work going on in the background. Each is called
before every redisplay of the command loop and returns when it wants to be
called again. NIL means not until the next key. T means work of its own runs
in the background, to be called soon (*TICK-INTERVAL*). A number of seconds
means nothing is running, to look again then: keys typed meanwhile do not
call it again before, only the editor woken (as a job ending wakes it). The
loop waits no longer than the soonest for a key. This is how a replacement or
a save running in the background shows its progress and is taken in, and how
a file changed on disk is noticed.")

(defvar *last-input-time* (get-internal-real-time)
  "When the command loop last read something you did (a key, a paste, the
mouse), in internal time units: how long you have left the editor alone.")

(defvar *tick-interval* 1
  "Seconds the command loop waits for a key while a tick function asks for
more: how often the progress of work in the background is shown, and at most
how long a finished job waits to be taken in. Once a second is enough to see
it move, and calm to look at.")

(defun call-tick-function (function editor)
  "Call the tick FUNCTION with EDITOR and return what it wants. An error in it
is said and reported in debug mode with the calls under way, and counts as
wanting nothing, so that work taken in from the background cannot take the
editor down. An edit of a buffer a waiting question is about, refused
\(BUFFER-HELD-BY-QUESTION), is put off: said nothing of, it wants another
call soon, as the question will be answered."
  (flet ((call ()
           (handler-case (funcall function editor)
             (buffer-held-by-question () t))))
    (declare (dynamic-extent #'call))
    (call-with-guard #'call function 'call-tick-function :quit nil)))

(defvar *after-tick-functions* '()
  "Functions of the editor called after *TICK-FUNCTIONS*, each time, and when
another frame becomes the one worked in (SELECT-FRAME). Work taken in from the
background may have shown another buffer meanwhile, as another frame shows
its own. These functions bring what follows the buffer shown (the modal
state's keys) in line before the next key is read, as after a command.")

(defvar *tick-due* (make-hash-table :test 'eq)
  "When each tick function that last asked for a number of seconds wants its
next call, an internal real time, by function.")

(defun tick-wanted (function editor now woken)
  "What the tick FUNCTION wants, as *TICK-FUNCTIONS* says, NOW an internal
real time: called with EDITOR, unless it asked for a number of seconds that
have not gone and the editor was not WOKEN, the seconds left then. A wait of
more than a day is kept as a day: the function is asked again then."
  (let ((due (gethash function *tick-due*)))
    (if (and due (not woken) (< now due))
        ;; a single float, which makes nothing
        (/ (float (- due now) 1f0) internal-time-units-per-second)
        (let ((wanted (call-tick-function function editor)))
          (if (realp wanted)
              (setf (gethash function *tick-due*)
                    (+ now (round (* (min wanted 86400) internal-time-units-per-second))))
              (remhash function *tick-due*))
          wanted))))

(defun run-tick-functions (editor &key (woken t))
  "Call *TICK-FUNCTIONS* with EDITOR (CALL-TICK-FUNCTION), then
*AFTER-TICK-FUNCTIONS*; the seconds until the soonest tick function wants
another call, or NIL when none does. Sets *BACKGROUND-WORK*. Unless the
editor was WOKEN, after a key, a function that asked for a number of seconds
not gone yet is not called (TICK-WANTED)."
  (let ((soonest nil) (working nil) (now (get-internal-real-time)))
    (dolist (f *tick-functions*)
      (let ((wanted (tick-wanted f editor now woken)))
        (when wanted
          (when (eq wanted t) (setf working t))
          (let ((seconds (if (realp wanted) wanted *tick-interval*)))
            (setf soonest (if soonest (min soonest seconds) seconds))))))
    (tuatara.runtime:run-hook *after-tick-functions* editor)
    (setf *background-work* working)
    soonest))

(defun paint-trouble (editor condition)
  "Paint a frame saying the window could not be shown because of CONDITION: the
text of a mapped file that another program rewrote in place, for one. The
editor stays usable: the way out is said, and the minibuffer is shown as it
reads (\\[execute-extended-command], the questions \\[save-buffers-kill-editor] asks), its prompt
and what is typed on the last row, else the message there."
  (let ((frontend (editor-frontend editor)))
    (tuatara.runtime:ignoring-errors-reported ("painting a frame saying why the window cannot be shown")
     (tuatara.frontend:frontend-begin-frame frontend)
     (tuatara.frontend:frontend-put-row
      frontend 0 (format nil "This window cannot be shown: ~a."
                         (if tuatara.runtime:*secret-at-hand*
                             (tuatara.runtime:failure-words condition)
                             (tuatara.runtime:condition-words condition :headline t))))
     (tuatara.frontend:frontend-put-row
      frontend 1 (substitute-command-keys (concatenate 'string "\\[switch-to-buffer] shows another buffer, "
                                                       "\\[kill-buffer] kills this one, "
                                                       "\\[save-buffers-kill-editor] quits; if its file changed on "
                                                       "disk, \\[revert-buffer] reads it again.")))
     (tuatara.frontend:frontend-put-row
      frontend 2 (substitute-command-keys
                  "Debug mode (\\[debug-mode]) keeps what went wrong, and where, in *Debug* and its log."))
     (let ((last (max 3 (1- (editor-height editor)))))
       (if (editor-minibuffer-active editor)
           (let ((row (concatenate 'string (editor-minibuffer-prompt editor) (minibuffer-shown-text editor))))
             (tuatara.frontend:frontend-put-row frontend last row)
             (tuatara.frontend:frontend-set-cursor frontend last
                                                   (min (length row) (+ (length (editor-minibuffer-prompt editor))
                                                                        (editor-minibuffer-point editor)))
                                                   :bar))
           (tuatara.frontend:frontend-put-row frontend last (or (editor-message editor) ""))))
     (tuatara.frontend:frontend-end-frame frontend))))

(defun redisplay-or-say (editor)
  "Redisplay EDITOR's frame; when that fails, paint one saying why and the way
out, the minibuffer shown as it reads (PAINT-TROUBLE), and report it, with the
calls under way in debug mode: so that \\[execute-extended-command] and the questions of
\\[save-buffers-kill-editor] still work whatever a window's drawing does. While a window shows a
secret buffer (FRAME-SECRET-P, looked at once by REDISPLAY), only the kind of
failure is said. A frame closed is not drawn."
  (let ((trace nil) (secret-then nil))
    (when (frame-deleted (editor-frame editor))
      (return-from redisplay-or-say nil))
    (handler-case
        (handler-bind ((error (lambda (e)
                                (declare (ignore e))
                                ;; the secret REDISPLAY had at hand, still so once its calls are left
                                (setf secret-then tuatara.runtime:*secret-at-hand*)
                                (when tuatara.runtime:*debug-on*
                                  (setf trace (capture-backtrace :stop 'redisplay-or-say))))))
          (redisplay editor))
      (error (e)
        (let ((tuatara.runtime:*secret-at-hand* (or tuatara.runtime:*secret-at-hand* secret-then)))
          (tuatara.runtime:debug-report :redisplay "the screen could not be drawn: ~a~{~%    ~a~}"
                                        (tuatara.runtime:failure-words e) trace)
          (paint-trouble editor e))))))

(defvar *dropped* nil
  "While the command that opens files dropped on the window runs, the :drop
event it is for; NIL otherwise.")

(defvar *pasted* nil
  "While \\[insert-pasted-text] runs for text pasted into the terminal Tuatara
runs in, the :paste event it is for; NIL otherwise.")

(define-command insert-pasted-text ()
  "Put in the text just pasted into the terminal Tuatara runs in, as text:
none of it is taken for keys or commands, whatever it holds, in any editing
state. It goes in at point, its lines ending as the buffer's do, and one undo
takes it all back; a buffer that cannot be changed says so. In a buffer
whose :PASTE-HANDLER local is a function of the buffer and the text (a
terminal, which hands it to the program in it), that function takes it."
  (let ((text (and *pasted* (tuatara.frontend:event-text *pasted*)))
        (buffer (current-buffer)))
    (unless text (refuse "Nothing has been pasted"))
    (let ((handler (tuatara.buffer:buffer-local buffer :paste-handler)))
      (cond (handler (funcall handler buffer text))
            ((plusp (length text))
             (let ((at (tuatara.buffer:buffer-point buffer)))
               (setf (tuatara.buffer:buffer-point buffer)
                     (tuatara.buffer:buffer-insert buffer at (tuatara.buffer:text-for-buffer text buffer at)))))))))

(defparameter *slow-frame* 30
  "The milliseconds drawing a frame may take before debug mode reports it.")

(defparameter *frame-at-least-every* 100
  "While keys wait to be handled (a key held down), the milliseconds the
editor may go without drawing a frame: the keys are handled first, so that
none is left over to act after the key is let go, and a frame drawn at least
this often shows where they have got to.")

(defun take-in-background (editor &key (woken t))
  "Take in, on EDITOR's thread, what work in the background has for it: what
other threads said and handed on (TAKE-IN-THREAD-MESSAGES), then what the
tick functions take in (RUN-TICK-FUNCTIONS, WOKEN unless it comes after a
key), moving the display generation on when anything may show differently.
The seconds until a tick function wants another call, or NIL."
  (when (take-in-thread-messages editor)
    (incf *display-generation*))
  (let ((working *background-work*))
    (prog1 (run-tick-functions editor :woken woken)
      ;; work going on in the background, or just ended: what it brought may show anywhere
      (when (or working *background-work*)
        (incf *display-generation*)))))

(defun command-loop (editor)
  "Run EDITOR until it stops: take in background work (TAKE-IN-BACKGROUND),
redisplay, read an event, dispatch it. While more keys wait, the frame is
drawn only every *FRAME-AT-LEAST-EVERY* milliseconds, so that typing
is never held up by drawing; the other frames are painted when what they
show may have changed (PAINT-OTHER-FRAMES). Each event is handled guarded
\(HANDLE-EVENT-GUARDED)."
  (let ((*editor* editor)
        (drawn (get-internal-real-time))
        (woken t))
    (loop while (editor-running editor)
          do (let ((wait (take-in-background editor :woken woken))
                   (began (get-internal-real-time)))
               (unless (and (tuatara.frontend:frontend-input-pending-p (frame-frontend (editor-frame editor)))
                            (< (tuatara.runtime:milliseconds-since drawn) *frame-at-least-every*))
                 (redisplay-or-say editor)
                 (setf drawn (get-internal-real-time))
                 (when tuatara.runtime:*debug-on*
                   (let ((spent (tuatara.runtime:milliseconds-since began)))
                     (when (>= spent *slow-frame*)
                       (tuatara.runtime:debug-report :redisplay "a frame drawn in ~,1f ms" spent)))))
               (let ((later (paint-other-frames editor)))
                 (when later (setf wait (if wait (min wait later) later))))
               ;; what waits to be done in the frame kept with no screen waits for a client to do it in
               (let ((event (or (if (idle-frame-p editor (editor-frame editor))
                                    (take-deferred-without-screen editor)
                                    (pop (editor-deferred editor)))
                                (other-frames-deferred editor)
                                (read-event editor :timeout wait :ticks t :between-commands t))))
                 ;; work handed on from the background, about to run: what any frame shows may change
                 (when (functionp event)
                   (incf *display-generation*))
                 (when (and event (not (functionp event)))
                   (setf *last-input-time* (get-internal-real-time)))
                 ;; after a key, the tick functions that asked for a while are left until it has gone
                 (setf woken (not (tuatara.frontend:input-event-p event)))
                 (handle-event-guarded editor event)
                 ;; the frame worked in closed by what ran: another is worked in from now
                 (when (frame-deleted (editor-frame editor))
                   (select-frame editor (first (editor-frames editor)))))))))

(defun handle-event-guarded (editor event)
  "Handle EVENT, read by the command loop (HANDLE-EVENT), so that whatever
escapes the command's own guards is said, never the end of the editor. A
command held up longer than TUATARA.RUNTIME:*SLOW-CALL-SECONDS* by one
stoppable call (a regular expression stuck on a line), where nothing goes on
with it in the background, is stopped and says so."
  (call-guarded
   'command-loop
   (lambda ()
     (when (eq :slow (nth-value 1 (tuatara.runtime:stoppable-work ((list nil) :slow tuatara.runtime:*slow-call-seconds*)
                                    (handle-event editor event)
                                    nil)))
       ;; a regular expression stuck on a line, where nothing carries it on in the background
       (message "Stopped: the pattern is too slow on this text to match here")))))

(defun handle-event (editor event)
  "Do what EVENT, read by the command loop, asks of EDITOR: run the command its
key is bound to, or the work handed on as a function (DEFER-ACTION), and the
like. A window's close button closes its frame (delete-frame), the last one
quitting as \\[save-buffers-kill-editor] does. A frame whose screen went away
\(its client left) is closed, never the editor: the last frame stays."
  (if (functionp event)
      ;; work handed on to run as a command of its own (DEFER-ACTION): it may ask
      (funcall event)
      (case (tuatara.frontend:event-kind event)
        (:quit (setf (editor-running editor) nil))
        ;; a window's close button: closing its frame, or for the last, as C-x C-c, asking about what is unsaved
        (:close (let ((command (command-symbol (if (rest (editor-frames editor))
                                                   "delete-frame"
                                                   "save-buffers-kill-editor"))))
                  (if command
                      (run-command editor command)
                      (setf (editor-running editor) nil))))
        ;; files dropped on the window: opened by the command for it
        (:drop (let ((command (command-symbol "open-dropped-files")))
                 (when command
                   (let ((*dropped* event)) (run-command editor command)))))
        ;; text pasted into the terminal: put in as text, never as keys
        (:paste (let ((*pasted* event)) (run-command editor 'insert-pasted-text)))
        (:said (message "~a" (escape-controls (tuatara.frontend:event-text event))))
        ;; a client gone: its frame closed, and nothing else
        (:detached (let ((frame (or (tuatara.frontend:event-frame event) (editor-frame editor))))
                     (if (and (rest (editor-frames editor)) (member frame (editor-frames editor)))
                         (close-screen-gone editor frame)
                         (tuatara.runtime:debug-report :remote "the last frame's screen went away; the frame stays"))))
        (:key (handle-key editor (tuatara.frontend:event-key event))))))

(defun running-editor ()
  "The editor whose session runs, or NIL; for work on another thread to wake or reach."
  (car *running-editor*))

(defun key-waiting-p ()
  "True when a key has been pressed, in any of the editor's frames, that the
editor has not read yet."
  (let ((editor (or *editor* (running-editor))))
    (and editor (some (lambda (frame) (tuatara.frontend:frontend-input-pending-p (frame-frontend frame)))
                      (editor-frames editor)))))

(defvar *actions-without-screen* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "The functions given to DEFER-ACTION that run though no client shows the
editor (an editor running in the background with none attached): they ask
nothing. The others wait for a client, to ask in its frame.")

(defun take-deferred-without-screen (editor)
  "Take out of what waits to be done in EDITOR's frame the first function
that runs though no client shows the editor (*ACTIONS-WITHOUT-SCREEN*), and
return it; NIL when there is none."
  (let ((action (find-if (lambda (item) (and (functionp item) (gethash item *actions-without-screen*)))
                         (editor-deferred editor))))
    (when action
      (setf (editor-deferred editor) (remove action (editor-deferred editor) :count 1))
      action)))

(defun defer-action (function &key without-screen)
  "Have FUNCTION run as the editor's next command, before the next key: work
taken in from the background (on a tick) that asks a question, which it may
then, as a command does. Given on another thread, FUNCTION is handed to the
editor's thread, which is woken for it (TAKE-IN-THREAD-MESSAGES). In an
editor running in the background, it waits for a client to be shown in,
unless WITHOUT-SCREEN says it asks nothing (*ACTIONS-WITHOUT-SCREEN*)."
  (when without-screen
    (setf (gethash function *actions-without-screen*) t))
  (if (away-from-editor-p)
      (hand-to-editor-thread function *actions-from-threads*)
      (let ((editor (or *editor* (car *running-editor*))))
        (when editor
          (setf (editor-deferred editor) (append (editor-deferred editor) (list function)))
          (wake-editor editor)))))

(define-record (editor-hold (:constructor make-editor-hold (editor words stop)) (:copier nil))
  "The running EDITOR's thread asked to wait between two commands for work on
another thread (CALL-WITH-EDITOR-HELD), WORDS saying what it is, STOP a
function asking it to stop where it safely can, or NIL. STATE is :ASKED
until it waits, :HELD while it does, :RELEASED once the work is done,
:GIVEN-UP when the work stopped asking first; WAITING is signalled once it
waits, GO-ON lets it go on."
  (editor nil :documentation "The editor an EDITOR-HOLD keeps waiting.")
  (words nil :documentation "What the work an EDITOR-HOLD waits for is, in words.")
  (stop nil :documentation "The function asking the work an EDITOR-HOLD waits
for to stop, or NIL.")
  (state :asked :documentation "Whether the editor of an EDITOR-HOLD has been
asked to wait, waits, or was given up.")
  (waiting (sb-thread:make-semaphore :name "tuatara editor waiting")
           :documentation "Signalled once the editor of an EDITOR-HOLD waits.")
  (go-on (sb-thread:make-semaphore :name "tuatara editor goes on")
         :documentation "Signalled to let the editor of an EDITOR-HOLD go on."))

(defparameter *editor-held-seconds* 3
  "The seconds the editor may wait for work holding it (CALL-WITH-EDITOR-HELD)
before that is taken for a fault: said, in the debug log too, and \\`C-g'
then asks the work to stop where it safely can.")

(defun wait-while-held (hold)
  "On the editor's thread: wait until the work HOLD waits for lets it go on,
counted as idle meanwhile (no hang is reported), however long it takes, for
the editor going on before would let commands run with that work half done.
Once that has taken *EDITOR-HELD-SECONDS*, it is said, in the debug log too,
and \\`C-g' asks the work to stop where it safely can (HOLD's STOP), the
waiting going on, said; other keys are kept for after."
  (let* ((editor *editor*)
         (frontend (and editor (frame-frontend (editor-frame editor))))
         (go-on (editor-hold-go-on hold)))
    (tuatara.runtime:note-editor-busy nil)
    (unwind-protect
         (unless (sb-thread:wait-on-semaphore go-on :timeout *editor-held-seconds*)
           (flet ((say (stopping)
                    (when editor
                      (echo "~a" (if stopping
                                     (format nil
                                             "~a: stopping where it safely can, and waiting for that"
                                             (editor-hold-words hold))
                                     (format nil "~a is taking long~:[~*~;; ~a stops it where it safely can~]"
                                             (editor-hold-words hold) (editor-hold-stop hold)
                                             (substitute-command-keys "\\`C-g'"))))
                      (tuatara.runtime:ignoring-errors-reported ("drawing while the editor is held")
                        (redisplay editor)))))
             (tuatara.runtime:debug-report :hang "~a has held the editor for more than ~d seconds"
                                           (editor-hold-words hold) *editor-held-seconds*)
             (say nil)
             (loop (when (sb-thread:wait-on-semaphore go-on :timeout 1/20) (return))
                   (when (and frontend (pending-quit-p editor) (editor-hold-stop hold))
                     (funcall (editor-hold-stop hold))
                     (say t)))))
      (tuatara.runtime:note-editor-busy t))))

(defun hold-editor (words stop)
  "Ask the running editor's thread to wait between two commands, as a command
of its own (DEFER-ACTION), for the work of this thread, WORDS saying what it
is, STOP a function asking it to stop where it safely can (WAIT-WHILE-HELD),
and return the EDITOR-HOLD once it waits; NIL when the
editor stops running first. A job asked to stop meanwhile ends here; the
editor is never left waiting for work that gave up."
  (let* ((editor (running-editor))
         (hold (and editor (make-editor-hold editor words stop)))
         (got nil))
    (when hold
      (defer-action (lambda ()
                      (when (eq :asked (sb-ext:compare-and-swap (editor-hold-state hold) :asked :held))
                        (sb-thread:signal-semaphore (editor-hold-waiting hold))
                        (wait-while-held hold)))
                    :without-screen t)
      (unwind-protect
           (loop
             (when (sb-thread:wait-on-semaphore (editor-hold-waiting hold) :timeout 1/10)
               (return (setf got hold)))
             (tuatara.runtime:job-check-stop)
             (unless (eq (running-editor) editor)
               (return nil)))
        (unless got
          ;; once it waits after all, it is let go at once
          (unless (eq :asked (sb-ext:compare-and-swap (editor-hold-state hold) :asked :given-up))
            (sb-thread:signal-semaphore (editor-hold-go-on hold))))))))

(defun call-with-editor-held (function &key (words "Work in the background") stop)
  "Call FUNCTION, on a thread other than the editor's, while the editor's
thread waits between two commands all the while, and return what it
returns: for work in the background that changes what commands use (a
package's definitions loaded), so that no command runs meanwhile. Keys
pressed meanwhile wait and are handled after. *EDITOR* is the running
editor's in FUNCTION, as in a command. The work is meant to take
milliseconds: one that takes more than *EDITOR-HELD-SECONDS* is a fault,
said, WORDS saying what it is, and \\`C-g' then calls STOP, a function
asking it to stop where it safely can, the editor waiting on until it has
ended (WAIT-WHILE-HELD).
On the editor's own thread, or with no editor running, FUNCTION is called at
once. An error when the editor stops before it waits."
  (if (not (away-from-editor-p))
      (funcall function)
      (let ((hold (or (hold-editor words stop) (refuse "The editor stopped before the work could be done"))))
        (unwind-protect (let ((*editor* (editor-hold-editor hold))) (funcall function))
          (setf (editor-hold-state hold) :released)
          (sb-thread:signal-semaphore (editor-hold-go-on hold))))))

(defvar *exit-functions* '()
  "Functions called with the editor once its command loop has stopped, however
it stopped (\\[save-buffers-kill-editor], the window closed), before the frontend
goes: what is kept between sessions is written then. An error in one is said
in the debug log and the others still run.")

(defgeneric host-startup-buffer (host)
  (:documentation "The buffer to show when the editor starts without files and without a BUFFER
argument: the defaults choose a scratch buffer. NIL, as on the core's own host,
for one called *scratch*.")
  (:method ((host tuatara.runtime:host))
    nil))

(defun survive-stack-exhaustion ()
  "Have Steel Bank Common Lisp (SBCL) report running out of stack as a
condition the editor can catch, rather than end the process as it does when
started with --script (which implies --lose-on-corruption): code evaluated
in the editor may recurse without end, and must not take the editor with
it."
  (tuatara.runtime:ignoring-errors-reported ("having running out of stack reported as a condition")
    (setf (sb-alien:extern-alien "lose_on_corruption_p" (sb-alien:unsigned 8)) 0)))

(defgeneric host-visit-file (host path &key safe hex)
  (:documentation "Visit the file at PATH, one the editor is started with, as the editor's
commands do (a PDF as its pages, a binary file safely in the hex view), SAFE
to visit it safely and HEX to show it in the hex view, never first as text,
and return its buffer. The core's own host visits its text, saying why it
opened safely when it did.")
  (:method ((host tuatara.runtime:host) path &key safe hex)
    (declare (ignore hex))
    (let* ((buffer (tuatara.io:call-with-file-heads (lambda () (tuatara.io:visit-file path :safe safe))))
           (why (tuatara.io:safe-visit-reason buffer)))
      (when why
        (message "~a opened safely: ~a; \\[end-safe-visit] opens it as usual" (tuatara.buffer:buffer-name buffer) why))
      buffer)))

(defgeneric host-visit-standard-input (host descriptor)
  (:documentation "Make a buffer of what is read from DESCRIPTOR, open, and return the buffer,
for a lone - among the files the editor is started with: called *stdin*, read only
and opened safely. The defaults read it in the background; the core's own
host reads it all first, into a file with no name in the cache directory.")
  (:method ((host tuatara.runtime:host) descriptor)
    (multiple-value-bind (scratch size)
        (unwind-protect (tuatara.io:copy-descriptor-to-scratch descriptor)
          (sb-posix:close descriptor))
      (multiple-value-bind (rope mapping) (tuatara.io:scratch-file-text scratch size "standard input")
        (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*stdin*") :text rope)))
          (setf (tuatara.buffer:buffer-mapping buffer) mapping
                (tuatara.buffer:buffer-read-only-p buffer) t
                (tuatara.io:safe-visit-p buffer) "it came from standard input")
          buffer)))))

(defun visit-startup-file (entry)
  "Visit ENTRY, a file the editor was started with: a path, or a list of it
and :SAFE and :HEX as asked, or (:STANDARD-INPUT DESCRIPTOR) (HOST-VISIT-FILE,
HOST-VISIT-STANDARD-INPUT)."
  (if (and (consp entry) (eq (first entry) :standard-input))
      (host-visit-standard-input tuatara.runtime:*host* (second entry))
      (destructuring-bind (path &key safe hex) (alexandria:ensure-list entry)
        (host-visit-file tuatara.runtime:*host* path :safe safe :hex hex))))

(defvar *exit-code* 0
  "The code the editor ended with: that of the one who started it (its first
client, CLIENT-EXIT-CODE), 0, or 1 for a program that runs it as its editor to
give up (git's commit not made).")

(defun visit-startup-files (entries)
  "Visit each of ENTRIES, the files the editor was started with, as
VISIT-STARTUP-FILE does, and return their buffers in order. A file that
cannot be opened is said in the echo area and left out; the others open."
  (loop for entry in entries
        for buffer = (call-with-guard (lambda () (visit-startup-file entry)) 'visit-startup-file 'visit-startup-files
                                      :said (lambda (words)
                                              (format nil "~a: ~a" (if (consp entry) (first entry) entry) words)))
        when buffer collect buffer))

(defun run-editor (frontend &key files (setup nil) buffer start)
  "Start FRONTEND, call SETUP with the editor if given (this is where the
defaults install keymaps, modes, themes and the init file), then show the
first of FILES that opens, or BUFFER, or what HOST-STARTUP-BUFFER gives. Call
START, if given, with the editor once that is shown. Run the command loop, and
stop the frontends of its frames when it ends. Setup runs before any buffer is
shown, so what it installs (major modes, the startup buffers) applies to them.
Returns the editor."
  (let* ((editor (make-editor frontend))
         ;; the editor is current for the whole session, including SETUP and the
         ;; init file it loads, so MESSAGE and friends work there
         (*editor* editor))
    (survive-stack-exhaustion)
    (setf (tuatara.frontend:frontend-frame frontend) (editor-frame editor))
    (tuatara.frontend:frontend-start frontend)
    ;; what other threads say from now on is handed to this one
    (sb-thread:with-mutex (*messages-from-threads-lock*) (setf *messages-from-threads* '() *actions-from-threads* '()))
    (setf *running-editor* (cons editor sb-thread:*current-thread*))
    (unwind-protect
         (multiple-value-bind (width height) (tuatara.frontend:frontend-size frontend)
           (setf (editor-width editor) (max 1 width) (editor-height editor) (max 3 height))
           (when setup (funcall setup editor))
           ;; the init file has run: the settings the frontend reads may be new
           (tuatara.frontend:frontend-settings-changed frontend)
           (setf *exit-code* 0
                 (client-startup-buffers (current-client editor)) (visit-startup-files files))
           (let ((first (or (first (client-startup-buffers (current-client editor)))
                            buffer
                            (host-startup-buffer tuatara.runtime:*host*)
                            (tuatara.buffer:find-buffer "*scratch*")
                            (tuatara.buffer:make-buffer "*scratch*"))))
             (select-window (tuatara.display:make-window first :width (max 1
                                                                           width) :height (max 1 (- height 2))) editor)
             (fit-windows editor)
             (note-selected-buffer first)
             ;; let per-command bookkeeping (modal state, gutters) see the buffer now shown
             (dolist (f (editor-post-command-functions editor)) (call-guarded f f editor))
             (when start (call-guarded start start editor))
             (let ((end-watch (tuatara.runtime:start-hang-watch sb-thread:*current-thread*
                                                                :while (lambda () (editor-running editor)))))
               (unwind-protect (command-loop editor)
                 (funcall end-watch)))
             ;; stopped, however: each client ends, the one who started it last, then what is kept
             ;; between sessions is written
             (let ((clients (editor-clients editor)))
               (dolist (client (append (rest clients) (list (first clients))))
                 (end-client editor client))
               (setf *exit-code* (client-exit-code (first clients))))
             (dolist (f *exit-functions*) (call-guarded f f editor))))
      (when (and *running-editor* (eq (car *running-editor*) editor))
        (setf *running-editor* nil))
      (take-in-thread-messages editor)
      ;; once each: frames taking turns on one display share its frontend
      (dolist (frontend (remove-duplicates (mapcar #'frame-frontend (editor-frames editor))))
        (tuatara.frontend:frontend-stop frontend)))
    editor))
