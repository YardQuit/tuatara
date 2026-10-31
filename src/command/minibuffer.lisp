;;;; src/command/minibuffer.lisp --- The minibuffer: reading a string, a key or an answer, with completion.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defun read-password (prompt &key confirm)
  "Read a secret, a passphrase, with PROMPT in the minibuffer, what is typed
shown as stars and kept nowhere else (no history, no debug log); with CONFIRM
asked twice, an error when the two differ. Returns the string, which the
caller should wipe (WIPE-STRING) once it has handed it on. Every copy not
returned is wiped, the question given up (\\[keyboard-quit]) too."
  (let* ((*minibuffer-hidden* t)
         (first (read-string prompt))
         (returned nil))
    (if (not confirm)
        first
        (unwind-protect
             (let ((second (read-string "Again: ")))
               (unwind-protect
                    (if (string= first second)
                        (setf returned first)
                        (error "The two passphrases differ: nothing done"))
                 (wipe-string second)))
          (unless returned (wipe-string first))))))

(defun wipe-string (string)
  "Overwrite STRING, a secret no longer needed, so that it lies in memory no longer than it must."
  (when (stringp string) (fill string #\Nul))
  nil)

(defun replace-minibuffer-text (start end string &optional (editor *editor*))
  "Put STRING in place of the minibuffer text of EDITOR from START below END,
the minibuffer cursor just after it. The new text is made in one piece, and
while a secret is read (*MINIBUFFER-HIDDEN*) the text it replaces is wiped
\(WIPE-STRING), so that no part of a passphrase is left behind in memory as
it is typed and corrected."
  (let* ((text (editor-minibuffer-text editor))
         (new (make-string (+ (length text) (length string) (- start end)))))
    (replace new text :end2 start)
    (replace new string :start1 start)
    (replace new text :start1 (+ start (length string)) :start2 end)
    (when *minibuffer-hidden* (wipe-string text))
    (setf (editor-minibuffer-text editor) new
          (editor-minibuffer-point editor) (+ start (length string)))))

(defvar *minibuffer-finish* nil
  "While the minibuffer reads, a function of no arguments that ends the
question with the text typed (FINISH-MINIBUFFER); NIL otherwise.")

(defvar *minibuffer-history* nil
  "While the minibuffer reads a question with earlier answers (READ-STRING's
HISTORY), a list (ANSWERS PLACE TYPED): the answers, newest first, the index
of the one shown, -1 for none, and what was typed before going back through
them; NIL otherwise.")

(defun finish-minibuffer ()
  "End the question the minibuffer asks: READ-STRING returns the text typed.
For the commands of a question's own keys (READ-STRING's KEYMAP) that end it."
  (if *minibuffer-finish*
      (funcall *minibuffer-finish*)
      (error "The minibuffer is not reading.")))

(defun read-string (prompt &key (initial "") complete select-initial changed update keymap history)
  "Read a line of text with PROMPT in the echo area. Returns the
string; signals QUIT when it is given up with \\<minibuffer>\\[minibuffer-abort]. Runs a
nested command loop with the minibuffer keymap. A message a key says while it
reads is shown after the text typed, in brackets, until the next key; one
said before is not shown. COMPLETE, when given, is a
function of no arguments that \\[minibuffer-complete] and \\`C-M-i' call to complete the
word at the minibuffer's cursor (COMPLETE-IN-TEXT). With SELECT-INITIAL, the
text INITIAL offers is selected, to be taken with \\[minibuffer-done] or typed over.
While a choice waits for candidates still coming in the background, the keys
typed meanwhile are kept to be handled once it is made, as typed; \\`C-g'
stops the question at once.
A question that acts as it is typed (a search, a list narrowed) gives
CHANGED, a function of one argument, the text typed: it is called on the
editor's thread each time that text changes, by a key, a paste, an earlier
answer brought back, before the screen is drawn again. It must be quick, a
few hundredths of a second at most: what takes longer it starts in the
background (START-JOB-THREAD) and takes in with UPDATE. UPDATE, a function of
no arguments, is called on the editor's thread before the question is drawn
each time and whenever the editor is woken (as a job ending wakes it): it
takes in what the work in the background has done, and returns three values:
true while such work goes on, the question woken for it, or a number of
seconds after which to call UPDATE again (work put off while typing goes
on); the prompt to show
now (how far the work has got), or NIL to keep the prompt shown; and what the
question counts, shown before the prompt where a completing read shows its
count (3/12, \"\" keeping that place empty, NIL for none). \\`C-g' ends the question by signalling QUIT,
as ever: the caller undoes there what its CHANGED did (with HANDLER-CASE or
UNWIND-PROTECT). KEYMAP holds the question's own keys, consulted before the
minibuffer's; a command of it may end the question with FINISH-MINIBUFFER.
HISTORY, a list of earlier answers, newest first, is gone back through with
\\[minibuffer-previous-history] and forward with \\[minibuffer-next-history]."
  (let ((editor *editor*)
        (held '())
        (answered nil)
        (working nil)
        (*minibuffer-initial-selected* (and select-initial (plusp (length initial))))
        (*minibuffer-question-keymap* keymap)
        (*minibuffer-history* (and history (list history -1 nil))))
    (when (editor-minibuffer-active editor) (error "The minibuffer is already in use."))
    ;; a list of completions in a buffer closes when the minibuffer asks something
    (close-popup editor)
    (setf (editor-minibuffer-prompt editor) prompt
          (editor-minibuffer-text editor) (copy-seq initial)
          (editor-minibuffer-point editor) (length initial)
          (editor-minibuffer-count editor) nil
          (editor-minibuffer-active editor) :reading
          (editor-minibuffer-completer editor) complete
          ;; what was said before the question is not shown in it
          (editor-message editor) nil)
    (unwind-protect
         (prog1 (block reading
                  (let ((*minibuffer-finish* (lambda () (return-from reading (editor-minibuffer-text editor)))))
                    (flet ((choose ()
                             ;; RET: the selected candidate, or the line typed where it can be taken,
                             ;; among the candidates as they are now (more may have come since drawn)
                             (update-candidates editor)
                             (let ((completion (editor-completion editor)))
                               (cond ((null completion) (return-from reading (editor-minibuffer-text editor)))
                                     ;; a name taken though not offered, typed whole
                                     ((and (not (completion-moved completion))
                                           (member (editor-minibuffer-text editor) (completion-taken completion) :test #'string=))
                                      (return-from reading (editor-minibuffer-text editor)))
                                     ;; nothing typed: the default, at once
                                     ((and (completion-default completion) (zerop (length (editor-minibuffer-text editor)))
                                           (not (completion-moved completion)))
                                      (return-from reading
                                        (let ((selected (selected-candidate completion)))
                                          (if (and selected (default-candidate-p completion selected))
                                              (candidate-result selected)
                                              (completion-default completion)))))
                                     ;; more coming and nothing chosen by hand: the best match once all has come
                                     ((and (completion-working completion) (not (completion-moved completion)))
                                      (setf (completion-done-pending completion) t))
                                     ((selected-candidate completion)
                                      (return-from reading (candidate-result (selected-candidate completion))))
                                     ;; the line typed selected: taken as it is
                                     ((and (= (completion-index completion) -1) (input-selectable-p completion))
                                      (return-from reading (editor-minibuffer-text editor)))
                                     ((completion-require-match completion) (message "No match"))
                                     (t (return-from reading (editor-minibuffer-text editor)))))))
                  (loop
                    (when update
                      (multiple-value-bind (going prompt count) (funcall update)
                        (setf working going
                              (editor-minibuffer-count editor) count)
                        (when prompt (setf (editor-minibuffer-prompt editor) prompt))))
                    (redisplay-or-say editor)
                    (let* ((completion (editor-completion editor))
                           (source (and completion (completion-source completion))))
                      (flet ((take-in ()
                               ;; what came in the background; a choice held back is made once all has come
                               (take-in-source editor)
                               (when (and (completion-done-pending completion) (not (completion-working completion)))
                                 (setf (completion-done-pending completion) nil)
                                 ;; the keys typed while it waited: handled after it, as typed
                                 (put-back-events editor held)
                                 (setf held '())
                                 (choose))))
                        ;; what was asked for while the candidates were worked out may have started
                        ;; work: whether more is coming, asked again before waiting; all come, shown so first
                        (when source
                          (let ((was (completion-working completion)))
                            (take-in)
                            (when (and was (not (completion-working completion)))
                              (redisplay-or-say editor))))
                      (let ((event (if source
                                       ;; candidates come from the background: woken for them, and
                                       ;; looking again shortly while more may come
                                       (let ((*background-work* (or *background-work* (completion-working completion))))
                                         (read-event editor :ticks t :pastes t :timeout (and (completion-working completion) 1/20)))
                                       (read-event editor :ticks working :pastes t :timeout (and (realp working) working))))
                            (before (editor-minibuffer-text editor)))
                      ;; what a key said is shown until the next one is typed
                      (when (member (tuatara.frontend:event-kind event) '(:key :paste))
                        (setf (editor-message editor) nil))
                      (case (tuatara.frontend:event-kind event)
                        (:quit (keyboard-quit-signal))
                        (:tick (when completion (take-in)))
                        ((:close :drop)
                         ;; the window closed or files dropped: the question ends as C-g ends it
                         (setf (editor-deferred editor) (append (editor-deferred editor) (list event)))
                         (keyboard-quit-signal))
                        ;; text pasted: typed into the answer, none of it taken for keys
                        (:paste
                         (when *minibuffer-initial-selected*
                           (setf *minibuffer-initial-selected* nil)
                           (minibuffer-clear))
                         (minibuffer-insert (tuatara.frontend:event-text event))
                         ;; a secret pasted is left in the answer alone
                         (when *minibuffer-hidden* (wipe-string (tuatara.frontend:event-text event))))
                        (:key
                         (if (and completion (completion-done-pending completion)
                                  (not (quit-key-p (tuatara.frontend:event-key event))))
                             ;; a choice waits for all to come: what is typed meanwhile is for after it
                             (setf held (append held (list event)))
                         (let* ((key (tuatara.frontend:event-key event))
                                (specs (append (editor-pending editor) (list (tuatara.frontend:key-spec key))))
                                (binding (resolve-keys editor specs)))
                           (cond ((keymap-p binding) (setf (editor-pending editor) specs))
                                 ((and binding *minibuffer-initial-selected*
                                       (member binding '(minibuffer-backward-delete-char minibuffer-kill-line)))
                                  ;; the text offered, selected, cleared
                                  (setf (editor-pending editor) '()
                                        *minibuffer-initial-selected* nil)
                                  (minibuffer-clear))
                                 (binding
                                  (setf (editor-pending editor) '())
                                  (unless (member binding '(minibuffer-done minibuffer-submit-input minibuffer-abort))
                                    (setf *minibuffer-initial-selected* nil))
                                  (case binding
                                    (minibuffer-done (choose))
                                    (minibuffer-submit-input (return-from reading (editor-minibuffer-text editor)))
                                    (minibuffer-abort (keyboard-quit-signal))
                                    (t (funcall (find-command binding)))))
                                 ((and (null (editor-pending editor))
                                       (characterp (tuatara.frontend:key-base key))
                                       (not (tuatara.frontend:key-ctrl key)) (not (tuatara.frontend:key-meta key)))
                                  (when *minibuffer-initial-selected*
                                    ;; what is typed takes the place of the text offered
                                    (setf *minibuffer-initial-selected* nil)
                                    (minibuffer-clear))
                                  (let ((typed (string (tuatara.frontend:key-base key))))
                                    (minibuffer-insert typed)
                                    (when *minibuffer-hidden* (wipe-string typed))))
                                 (t (setf (editor-pending editor) '())))))))
                      (when (and changed (string/= before (editor-minibuffer-text editor)))
                        (funcall changed (editor-minibuffer-text editor))))))))))
                (setf answered t))
      (let ((popup (editor-popup editor)))
        (when (and popup (eq (popup-target popup) :minibuffer))
          (close-popup editor)))
      ;; a secret given up: wiped here, as the one taking it wipes it once it is handed on
      (when (and *minibuffer-hidden* (not answered))
        (wipe-string (editor-minibuffer-text editor)))
      (setf (editor-minibuffer-active editor) nil
            (editor-minibuffer-prompt editor) nil
            (editor-minibuffer-text editor) nil
            (editor-minibuffer-count editor) nil
            (editor-minibuffer-completer editor) nil
            (editor-completion editor) nil)
      ;; the rows the candidates took go back to the windows before the command goes on
      (fit-windows editor))))

(defun quit-key-p (key)
  "True when KEY is \\`C-g', the key that stops what the editor is doing, or
Escape while *ESCAPE-QUITS* says it does the same."
  (let ((spec (tuatara.frontend:key-spec key)))
    (or (equal spec '(:ctrl #\g))
        (and tuatara-user:*escape-quits* (equal spec '(:escape))))))

(defun wait-interruptibly (thread &key (message "Working...") (grace 1/10) stop seconds)
  "Wait for THREAD, work done for a command, to end, while the editor keeps
reading keys, as WAIT-WHILE-INTERRUPTIBLY waits: MESSAGE shown once it has run
GRACE seconds; \\`C-g' stops the waiting, and with STOP, a function of THREAD
that should make it end soon, the work too; with SECONDS, the waiting stops
after that long. True when THREAD ended by itself; else NIL and, as a second
value, :RUNNING when it had not ended: never ended outright (which could cut
it short inside a library, holding what others need), but left to end on its
own."
  (let ((deadline (and seconds (tuatara.runtime:deadline-in seconds))))
    (multiple-value-bind (ended running)
        (wait-while-interruptibly (lambda (wait)
                                     (let ((wait (if deadline
                                                     (max 0 (min wait (tuatara.runtime:seconds-left deadline)))
                                                     wait)))
                                       (and (eq :running (sb-thread:join-thread thread :timeout wait :default :running))
                                            (sb-thread:thread-alive-p thread)
                                            (or (null deadline) (< (get-internal-real-time) deadline)))))
                                  :message message :grace grace :stop (and stop (lambda () (funcall stop thread))) :thread thread)
      (cond ((not ended) (if running (values nil running) nil))
            ((sb-thread:thread-alive-p thread) (values nil :running))
            (t t)))))

(defun wait-until-interruptibly (ready-p &key (message "Working...") (grace 1/10))
  "Wait until READY-P, a function of no arguments, is true, as
WAIT-WHILE-INTERRUPTIBLY waits: the editor reading keys, MESSAGE shown after
GRACE seconds, \\`C-g' stopping the waiting, the work going on. True once
ready; else NIL, and :RUNNING as a second value."
  (wait-while-interruptibly (lambda (seconds)
                              (let ((deadline (tuatara.runtime:deadline-in seconds)))
                                (loop (when (funcall ready-p) (return nil))
                                      (when (>= (get-internal-real-time) deadline) (return t))
                                      (sleep 1/50))))
                            :message message :grace grace))

(defun wait-while-interruptibly (running-p &key (message "Working...") (grace 1/10) stop thread)
  "Wait while RUNNING-P, a function of the seconds to wait at most, answers
true, the work waited for going on, the editor reading keys all the while.
Once the wait has gone GRACE seconds the echo area shows MESSAGE, a string or
a function of no arguments giving one (asked again every fifth of a second,
so that it can show how far the work has got), and after it what \\`C-g'
does. With STOP, a function of no arguments that should make the work end
soon, \\`C-g' calls it and gives the work two seconds more to end, the echo
area saying \"(\\`C-g' stops it)\"; without, \\`C-g' stops only the waiting,
the work going on, the echo area saying \"(\\`C-g' stops waiting)\". Any
other key is kept to be handled after the command (EDITOR-UNREAD), as typed;
a window resized is redrawn, and one closed or files dropped on it are dealt
with after the command. True when the work ended; else NIL and, as a second
value, :RUNNING when it had not. Work told to stop that does not end in the
two seconds is left to end on its own, the echo area saying so, and THREAD,
when given, the debug log. With no editor showing, it simply waits."
  (let* ((editor *editor*)
         (frontend (and editor (frame-frontend (editor-frame editor))))
         (shown nil)
         (next 0))
    (unless frontend
      (loop while (funcall running-p 1/20))
      (return-from wait-while-interruptibly t))
    (flet ((running-p (seconds) (funcall running-p seconds))
           (show ()
             (let ((text (format nil "~a ~a" (if (functionp message) (funcall message) message)
                                 (substitute-command-keys (if stop "(\\`C-g' stops it)" "(\\`C-g' stops waiting)") :marks t))))
               (setf next (tuatara.runtime:deadline-in 1/5))
               (unless (equal text shown)
                 (setf shown text)
                 (echo "~a" text)
                 (ignore-errors (redisplay editor))))))
      (when (running-p grace)
        (show)
        (loop
          (unless (running-p 1/20) (return))
          (when (and (functionp message) (>= (get-internal-real-time) next)) (show))
          (when (and (tuatara.frontend:frontend-input-pending-p frontend)
                     (keep-or-quit-event editor (tuatara.frontend:frontend-read-event-timeout frontend 1/100)))
            (cond ((null stop) (return-from wait-while-interruptibly (values nil :running)))
                  ((progn (funcall stop) (running-p 2))
                   (when thread (tuatara.runtime:left-running thread "Work stopped with C-g"))
                   (message "Stopped; what was under way did not end at once and ends on its own")
                   (return-from wait-while-interruptibly (values nil :running)))
                  (t (return-from wait-while-interruptibly nil)))))))
    t))

(defvar *read-key-replay* nil
  "While one command runs at many places in turn, a cons: the keys READ-KEY
gives before reading any, and those read so far, newest first. So a key the
command asks for is asked once and given at every place.")

(defun discard-typed-ahead (editor)
  "Throw away the keys typed to EDITOR, waiting in its frontend and not read
yet, so that a question asked now is answered only by a key pressed once it
is shown. Reading stops at a wake-up (a tick), which is kept; a window
resized, closed, files dropped or text pasted are kept to be dealt with, as
are keys a command left unread on purpose."
  (let ((frontend (editor-frontend editor)) (kept '()))
    (loop while (tuatara.frontend:frontend-input-pending-p frontend)
          do (let ((event (tuatara.frontend:frontend-read-event-timeout frontend 0)))
               (case (and event (tuatara.frontend:event-kind event))
                 ((nil) (return))
                 (:tick (push event kept) (return))
                 (:key)
                 (t (push event kept)))))
    (setf (editor-unread editor) (append (editor-unread editor) (nreverse kept)))))

(defun read-key (prompt &key fresh)
  "Show PROMPT in the echo area, redisplay, and return the next key pressed,
the cursor standing after PROMPT meanwhile (*KEY-QUESTION*). With FRESH, the
keys typed until PROMPT is shown are thrown away (DISCARD-TYPED-AHEAD):
for a question no key typed ahead may answer. Signals QUIT when the frontend
has nothing more to deliver."
  (let ((replay *read-key-replay*))
    (if (and replay (car replay))
        (pop (car replay))
        (let ((editor *editor*) (*key-question* t))
          (echo "~a" prompt)
          (redisplay-or-say editor)
          (when fresh (discard-typed-ahead editor))
          (let* ((event (read-event editor))
                 (key (case (tuatara.frontend:event-kind event)
                        (:quit (keyboard-quit-signal))
                        (t (tuatara.frontend:event-key event)))))
            (when replay (push key (cdr replay)))
            key)))))

(defun answer-keys-words (answers)
  "The keys ANSWERS, a list of characters, in words: y, n or v."
  (format nil "~{~a~#[~; or ~:;, ~]~}" answers))

(defun read-answer-key (prompt answers)
  "Ask PROMPT in the echo area, answered by one key: one of ANSWERS, a list
of characters such as (#\\y #\\n), pressed once the question is shown (READ-KEY
with FRESH: keys typed before are thrown away). Returns the character, in
lower case, whatever the case typed. Any other key says which keys answer
and asks again; \\`C-g' signals QUIT."
  (let ((asked prompt))
    (loop
      (let* ((key (read-key asked :fresh t))
             (base (tuatara.frontend:key-base key))
             (char (and (not (tuatara.frontend:key-ctrl key)) (not (tuatara.frontend:key-meta key))
                        (characterp base) (char-downcase base))))
        (cond ((quit-key-p key) (keyboard-quit-signal))
              ((and char (member char answers)) (return char))
              (t (setf asked (format nil "Answer ~a. ~a" (answer-keys-words answers) prompt))))))))

(defun completing-read (prompt table &key annotate require-match history (initial "") (separator #\/) (filter t)
                                         select reselect preview source default keep-order none taken keymap)
  "Read with completion: PROMPT is shown (a string, or a function of no
arguments giving it, for a question whose words change while it is open), TABLE is a list of candidates
(CANDIDATE objects or strings) or a function of the input returning one, and
the panel shows the matches sorted best first with the current one marked.
\\<minibuffer>\\[minibuffer-done] returns the current candidate's value (or the input when
nothing matches and REQUIRE-MATCH is false); \\[minibuffer-submit-input] returns the
input as typed; \\[minibuffer-complete] puts the current candidate into the input;
\\[minibuffer-next-candidate], \\[minibuffer-previous-candidate] and the arrows move; \\[minibuffer-abort] aborts.
HISTORY orders recent inputs first. With a function TABLE, the input is
matched and completed after its last SEPARATOR (a slash by default, for file
names), and with FILTER false its candidates are shown as it gives them.
SELECT, a function of the candidates first shown (a vector), returns the
index of the one selected at first, and with RESELECT the one selected after
each change of the input too; PREVIEW is called with the selected
candidate (NIL when there is none) each time the selection changes, to show
what choosing it would do. SOURCE, for candidates found in the background
while the question is open, is called on the editor's thread each time the
editor is woken, and returns three values: true when TABLE's candidates may
have changed, true while more may come, and how many there are so far (see
COMPLETION); the prompt says (listing, 12 so far) meanwhile, and NONE, when
given, once all have come and there are none. \\[minibuffer-done]
before anything can be chosen, more coming, chooses once it has come.
DEFAULT, when given, is what \\[minibuffer-done] takes while nothing is
typed: its candidate comes first then, selected, and the prompt names it,
Describe function (default car): . With KEEP-ORDER, the candidates that
match stay in the order TABLE gives them, not sorted best first; with
KEEP-ORDER :UNTIL-TYPED, only while nothing is typed to match them.
TAKEN, a list of names not among the candidates, are taken when typed
whole and \\[minibuffer-done] is pressed with no candidate chosen by hand.
KEYMAP holds the question's own keys, as READ-STRING takes it.
Signals QUIT on abort."
  (let* ((editor *editor*)
         (normalise (lambda (items) (if (every #'candidate-p items) items (mapcar (lambda (c) (if (stringp c) (make-candidate c) c)) items))))
         (table (if (functionp table)
                    (lambda (input) (funcall normalise (funcall table input)))
                    (funcall normalise table))))
    (let ((completion (make-completion table :annotate annotate :require-match require-match :history history
                                             :separator separator :filter filter :select select :reselect reselect
                                             :preview preview :source source :default default :keep-order keep-order
                                             :none none :taken taken :prompt prompt)))
      (setf (editor-completion editor) completion)
      (if source
          (take-in-source editor)
          (setf (completion-listed completion) (and (listp table) (length table))))
      (read-string (shown-prompt completion) :initial initial :keymap keymap))))

(defun completing-read-value (prompt candidates &rest keys)
  "Read with completion as COMPLETING-READ does, KEYS given to it, and return
the value of the candidate chosen among CANDIDATES, a list of CANDIDATE
objects: also when its name is typed and taken as typed; else what was typed."
  (let ((chosen (apply #'completing-read prompt candidates keys)))
    (if (stringp chosen)
        (let ((candidate (find chosen candidates :key #'candidate-string :test #'string=)))
          (if candidate (candidate-result candidate) chosen))
        chosen)))

(define-command minibuffer-toggle-tall ()
  "Show as many candidates as the frame has room for, the windows above giving
up their rows while the question is open; again, as many as usual
\(*PANEL-MAX-ROWS*)."
  (let ((c (editor-completion *editor*)))
    (when c
      (setf (completion-tall c) (not (completion-tall c))
            (completion-tallest c) 0))))

(define-command minibuffer-next-candidate ()
  "Select the next completion candidate."
  (let ((c (editor-completion *editor*)))
    (when (and c (plusp (length (completion-candidates c))))
      (setf (completion-index c) (step-selection c 1)
            (completion-moved c) t))))

(define-command minibuffer-previous-candidate ()
  "Select the previous completion candidate; above the first, the line typed,
where what is typed can be taken."
  (let ((c (editor-completion *editor*)))
    (when (and c (plusp (length (completion-candidates c))))
      (setf (completion-index c) (step-selection c -1)
            (completion-moved c) t))))

(defun step-selection (completion step)
  "The index STEP (1 or -1) from COMPLETION's selected one, going round: the
line typed (-1) one step above the first candidate, where it can be taken."
  (let* ((count (length (completion-candidates completion)))
         (low (if (input-selectable-p completion) -1 0)))
    (+ low (mod (+ (- (completion-index completion) low) step) (- count low)))))

(define-command minibuffer-complete ()
  "Put the selected candidate into the minibuffer input: for a file name, in
place of the name being typed after the last slash (a directory candidate
descends into it); for a list of words, in place of the word after the last
comma. Where the minibuffer reads no choice from a list but can complete the
word at its cursor (\\[eval-expression] completes Lisp's names), list the completions of that
word beside it."
  (let ((c (editor-completion *editor*)))
    (when (and (null c) (editor-minibuffer-completer *editor*))
      (funcall (editor-minibuffer-completer *editor*)))
    (when (and c (selected-candidate c))
      (let* ((candidate (selected-candidate c))
             (input (editor-minibuffer-text *editor*))
             (at (position (completion-separator c) input :from-end t))
             (text (if (and at (functionp (completion-table c)))
                       (concatenate 'string (subseq input 0 (1+ at)) (candidate-string candidate))
                       (candidate-string candidate))))
        (setf (editor-minibuffer-text *editor*) text
              (editor-minibuffer-point *editor*) (length text))))))

(define-command minibuffer-submit-input ()
  "Return the minibuffer input as typed, ignoring the candidates."
  nil)

(defun minibuffer-insert (string)
  "Insert STRING at the minibuffer cursor."
  (let ((p (editor-minibuffer-point *editor*)))
    (replace-minibuffer-text p p string)))

(define-command universal-argument ()
  "Begin a numeric prefix argument: \\[universal-argument] alone means 4,
\\[universal-argument] \\[universal-argument] 16, digits after it give their number,
a leading minus negates."
  (setf (editor-collecting-prefix *editor*) t
        (editor-prefix-argument *editor*) :fresh)
  (echo "\\[universal-argument]-"))

(define-command minibuffer-done ()
  "Finish minibuffer input and return the text to the command that asked."
  nil)

(define-command minibuffer-abort ()
  "Abandon the minibuffer input."
  nil)

(define-command minibuffer-backward-delete-char ()
  "Delete the character before the minibuffer cursor."
  (let ((p (editor-minibuffer-point *editor*)))
    (when (plusp p)
      (replace-minibuffer-text (1- p) p ""))))

(define-command minibuffer-backward-char ()
  "Move the minibuffer cursor left a character, a letter with its accents or
an emoji with its skin tone one."
  (setf (editor-minibuffer-point *editor*)
        (tuatara.text:string-cluster-start (editor-minibuffer-text *editor*) (editor-minibuffer-point *editor*))))

(define-command minibuffer-forward-char ()
  "Move the minibuffer cursor right a character, a letter with its accents or
an emoji with its skin tone one."
  (setf (editor-minibuffer-point *editor*)
        (tuatara.text:string-cluster-end (editor-minibuffer-text *editor*) (editor-minibuffer-point *editor*))))

(define-command minibuffer-beginning ()
  "Move the minibuffer cursor to the start."
  (setf (editor-minibuffer-point *editor*) 0))

(define-command minibuffer-end ()
  "Move the minibuffer cursor to the end."
  (setf (editor-minibuffer-point *editor*) (length (editor-minibuffer-text *editor*))))

(define-command minibuffer-kill-line ()
  "Delete the minibuffer text after the cursor."
  (let ((p (editor-minibuffer-point *editor*)))
    (replace-minibuffer-text p (length (editor-minibuffer-text *editor*)) "")))

(define-command minibuffer-clear ()
  "Clear the minibuffer: the text typed goes, the prompt stays, as \\`C-u' clears a shell's line."
  (replace-minibuffer-text 0 (length (editor-minibuffer-text *editor*)) ""))

(defun minibuffer-history-step (step)
  "Show in the minibuffer the answer STEP (1 back, -1 forward) from the one
shown among the question's earlier answers (*MINIBUFFER-HISTORY*), forward
of the newest what was typed before; say so when there is none that way."
  (let ((history *minibuffer-history*) (editor *editor*))
    (destructuring-bind (answers place typed) (or history (list '() -1 nil))
      (let ((next (+ place step)))
        (cond ((null history) (message "This question keeps no earlier answers"))
              ((>= next (length answers)) (message "No earlier answer"))
              ((< next -1) (message "No later answer"))
              (t (when (= place -1)
                   (setf (third history) (editor-minibuffer-text editor)))
                 (setf (second history) next)
                 (let ((text (copy-seq (if (= next -1) (or typed "") (nth next answers)))))
                   (setf *minibuffer-initial-selected* nil
                         (editor-minibuffer-text editor) text
                         (editor-minibuffer-point editor) (length text)))))))))

(define-command minibuffer-previous-history ()
  "Put the answer given to this question before the one shown in the
minibuffer in place of the text typed, for a question that keeps its
answers (the patterns typed, the commands run by name)."
  (minibuffer-history-step 1))

(define-command minibuffer-next-history ()
  "Put the answer given to this question after the one shown in the
minibuffer in place of the text typed; past the newest, what was typed."
  (minibuffer-history-step -1))

(define-command minibuffer-backward-kill-word ()
  "Delete the word before the minibuffer cursor: back over what is not a
letter or a digit, then over the letters and digits before it."
  (let* ((editor *editor*) (text (editor-minibuffer-text editor)) (p (editor-minibuffer-point editor)) (q p))
    (loop while (and (plusp q) (not (tuatara.text:word-char-p (char text (1- q))))) do (decf q))
    (loop while (and (plusp q) (tuatara.text:word-char-p (char text (1- q)))) do (decf q))
    (replace-minibuffer-text q p "" editor)))

(defun install-minibuffer-keys (keymap)
  "Bind the editing keys of the minibuffer in KEYMAP."
  (define-key keymap "RET" 'minibuffer-done)
  (define-key keymap "C-g" 'minibuffer-abort)
  (define-key keymap "ESC" 'minibuffer-abort)
  (define-key keymap "DEL" 'minibuffer-backward-delete-char)
  (define-key keymap "C-h" 'minibuffer-backward-delete-char)
  (define-key keymap "<left>" 'minibuffer-backward-char)
  (define-key keymap "C-b" 'minibuffer-backward-char)
  (define-key keymap "<right>" 'minibuffer-forward-char)
  (define-key keymap "C-f" 'minibuffer-forward-char)
  (define-key keymap "C-a" 'minibuffer-beginning)
  (define-key keymap "<home>" 'minibuffer-beginning)
  (define-key keymap "C-e" 'minibuffer-end)
  (define-key keymap "<end>" 'minibuffer-end)
  (define-key keymap "C-k" 'minibuffer-kill-line)
  (define-key keymap "C-u" 'minibuffer-clear)
  (define-key keymap "C-<backspace>" 'minibuffer-backward-kill-word)
  (define-key keymap "M-DEL" 'minibuffer-backward-kill-word)
  (define-key keymap *taller-panel-key* 'minibuffer-toggle-tall)
  (define-key keymap "C-n" 'minibuffer-next-candidate)
  (define-key keymap "<down>" 'minibuffer-next-candidate)
  (define-key keymap "C-p" 'minibuffer-previous-candidate)
  (define-key keymap "<up>" 'minibuffer-previous-candidate)
  (define-key keymap "TAB" 'minibuffer-complete)
  (define-key keymap "C-M-i" 'minibuffer-complete)
  (define-key keymap "M-TAB" 'minibuffer-complete)
  (define-key keymap "M-p" 'minibuffer-previous-history)
  (define-key keymap "M-n" 'minibuffer-next-history)
  (define-key keymap "C-j" 'minibuffer-submit-input)
  (define-key keymap "M-RET" 'minibuffer-submit-input))

(defun yes-or-no-p* (prompt)
  "Ask PROMPT in the minibuffer: true for the answer y or yes, in any case and
with spaces around; false for any other answer, n, no, none at all (\`RET')
or anything else, never asked again."
  (let ((answer (string-trim '(#\Space #\Tab) (read-string (concatenate 'string prompt " (y or n) ")))))
    (and (member answer '("y" "yes") :test #'string-equal) t)))
