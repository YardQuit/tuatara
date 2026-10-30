;;;; lisp/editing/recorded-keys.lisp --- Commands recording the keys typed and playing them back.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The recording and the playing themselves are the command loop's
;;;; (src/command/recorded-keys.lisp); here are the commands, the counter the
;;;; keys can insert, and the names that keep recorded keys as commands.

(in-package #:tuatara.defaults)

(defvar *last-recorded-keys* nil
  "The keys recorded last, as a vector of key events, or NIL before any are:
what \\[play-recorded-keys] plays.")

(defvar *last-recorded-keys-name* nil
  "The name \\[name-recorded-keys] last gave the keys recorded last, or NIL.")

(defvar *keys-counter* 1
  "The number \\[insert-keys-counter] inserts next.")

(defvar *keys-counter-format* "%d"
  "How \\[insert-keys-counter] writes the counter: %d stands for the number,
the rest as it is (\\[set-keys-counter-format]).")

(defvar *keys-counter-set* nil
  "The number \\[set-keys-counter] set while no keys were recorded, which the
next recording begins with, whatever plays have inserted since; NIL when it
has not been set since the last recording began.")

;;; Recording and playing

(defun recorded-keys-text (events)
  "The keys of EVENTS, key events, written as key sequences are (C-a M-f
C-k), text pasted among them as <pasted text>."
  (format nil "~{~a~^ ~}"
          (map 'list (lambda (event)
                       (if (eq (tuatara.frontend:event-kind event) :paste)
                           "<pasted text>"
                           (tuatara.frontend:key-description (tuatara.frontend:key-spec (tuatara.frontend:event-key event)))))
               events)))

(define-command start-recording-keys ()
  "Begin recording the keys you type, the answers typed to questions too,
until \\[stop-recording-keys]; \\[play-recorded-keys] then plays them again,
as if typed once more: \\[start-recording-keys] \\[regex-search-forward] date RET \\[kill-line] \\[stop-recording-keys]
records a search for date and a kill there, and each play searches for date
again and kills from where it found it. The mode line shows Rec meanwhile.
The quit key, \\`C-g' as standard, gives the recording up (in INSERT state
the first \\`C-g' only leaves INSERT), the keys recorded before kept as they
were. Text pasted is recorded too, and pasted again as it was; the mouse is
not recorded. The counter \\[insert-keys-counter]
inserts starts again at 1, or at the number \\[set-keys-counter] set since
the last recording began, however far plays took it meanwhile."
  (cond ((keys-playing-p) (error "Keys are not recorded while recorded keys play"))
        ((keys-recording-p)
         (drop-command-keys)
         (message "Keys are being recorded already: \\[stop-recording-keys] ends the recording"))
        (t (setf *keys-counter* (or *keys-counter-set* 1)
                 *keys-counter-set* nil)
           (start-keys-recording)
           (message "Recording keys: \\[stop-recording-keys] ends the recording, ~a gives it up" (quit-key-text)))))

(define-command stop-recording-keys ()
  "End the recording \\[start-recording-keys] began: the keys typed since,
but those of this command, become the keys \\[play-recorded-keys] plays. A
recording of no keys leaves the keys recorded before as they were."
  (if (not (keys-recording-p))
      (message "No keys are being recorded: \\[start-recording-keys] begins a recording")
      (let ((events (stop-keys-recording)))
        (if (null events)
            (message "No keys were recorded; the keys recorded before are kept")
            (progn
              (setf *last-recorded-keys* events
                    *last-recorded-keys-name* nil)
              (message "Recorded ~:d key~:p: ~a" (length events)
                       (tuatara.display:shorten-string (recorded-keys-text events) 40)))))))

(defun play-count ()
  "How many times the command running now plays recorded keys: its prefix
argument, 0 for until a play fails, 1 without one (or with one below 0)."
  (let ((n (prefix-argument)))
    (if (and n (>= n 0)) n 1)))

(defun play-keys-saying (events times)
  "Play EVENTS, recorded keys, TIMES times, 0 for until a play fails
\(PLAY-KEYS), and say how it went: how many plays were done, and where they
stopped, why. Keys played inside a play of others that stop before all their
plays were done fail that one too."
  (multiple-value-bind (done stopped words) (play-keys events times)
    (let ((why (or words "a command among the keys failed")))
      (case stopped
        (:failed (cond ((zerop times) (message "Played ~:d time~:p, until: ~a" done why))
                       ((= times 1) (message "The play of the recorded keys stopped: ~a" why))
                       (t (message "Stopped after ~:d of ~:d plays: ~a" done times why)))
                 (unless (zerop times) (note-command-failed)))
        (:quit (if (zerop times)
                   (message "Quit after ~:d play~:p" done)
                   (message "Quit after ~:d of ~:d plays" done times))
               (note-command-failed))
        (t (unless (= times 1) (message "Played ~:d times" done)))))))

(define-command play-recorded-keys ()
  "Play the keys recorded last (\\[start-recording-keys] ... \\[stop-recording-keys]) as if
typed again, answers to questions too. With a prefix argument N, N times
\(\\[universal-argument] 300 \\[play-recorded-keys]); with 0, again and again until a play fails. A
command among the keys that fails, a search that finds nothing or a key bound
to nothing stops all the plays there, saying how many were done. Each play is
one step to undo. Nothing is drawn till the plays end; plays going on for more
than a second show in the mode line how many are done, and the quit key,
\\`C-g', stops them between two plays. Right after, e plays the keys once
more, and again, till another key."
  (cond ((keys-recording-p)
         (drop-command-keys)
         (message "Keys are being recorded: \\[stop-recording-keys] ends the recording first"))
        ((null *last-recorded-keys*)
         (message "No keys have been recorded: \\[start-recording-keys] begins a recording"))
        (t (play-keys-saying *last-recorded-keys* (play-count)))))

(defun recorded-keys-mode-line (editor buffer)
  "Mode line function, before [-]: Rec while keys are recorded; while plays of
recorded keys have gone on for more than a second, how many are done, Played
1,200/10,000 (or Played 1,200 when they go on until one fails); in
:mode-line-recording."
  (declare (ignore editor buffer))
  (let ((progress *keys-play-progress*))
    (cond (progress (list (if (plusp (cdr progress))
                              (format nil "Played ~:d/~:d" (car progress) (cdr progress))
                              (format nil "Played ~:d" (car progress)))
                          :mode-line-recording))
          ((keys-recording-p) '("Rec" :mode-line-recording)))))

;;; The counter

(define-command insert-keys-counter ()
  "Insert the counter at point, written as \\[set-keys-counter-format] set
\(the number alone as standard), and add 1 to it, or with a prefix argument N,
add N. Recorded with other keys, each play inserts the next number: 1, 2, 3
and on. It starts at 1 with each recording, or where \\[set-keys-counter] set
it."
  (let ((buffer (current-buffer))
        (text (funcall (number-formatter *keys-counter-format*) *keys-counter*)))
    (check-writable buffer)
    (goto (tuatara.buffer:buffer-insert buffer (point) text))
    (incf *keys-counter* (prefix 1))))

(define-command set-keys-counter ()
  "Set the number \\[insert-keys-counter] inserts next, asked for, or the
prefix argument when one is given. Set while no keys are recorded, the next
recording begins with it, even when plays insert the counter before that
recording begins."
  (setf *keys-counter* (or (prefix-argument)
                           (read-whole-number "Counter (~d): " *keys-counter*
                                              :valid (constantly t) :refusal "Not a whole number."))
        *keys-counter-set* (and (not (keys-recording-p)) *keys-counter*))
  (message "The counter is ~d" *keys-counter*))

(define-command set-keys-counter-format ()
  "Set how \\[insert-keys-counter] writes the counter: %d for the number,
%03d for it in three digits with zeros before (001), %3d with spaces before,
%-3d with spaces after, %% a per cent sign, the rest as it is (Item %d.
writes Item 7.)."
  (let* ((format (read-string "Counter format (%d the number, %03d in three digits): " :initial *keys-counter-format*))
         (formatter (handler-case (number-formatter format)
                      (error (e) (message "~a" (tuatara.runtime:condition-words e)) nil))))
    (when formatter
      (setf *keys-counter-format* format)
      (message "The counter is written ~a" (funcall formatter *keys-counter*)))))

;;; Keeping recorded keys as commands

(defun tuatara-user:define-recorded-keys (name keys &optional documentation)
  "Make NAME, a symbol, a command that plays KEYS, a key sequence written as
BIND-KEY takes one (\"C-a M-f C-k\", \"C-s date RET\"), as
\\[play-recorded-keys] plays the keys recorded last: M-x NAME, or a key bound
to it, plays them, a prefix argument as with that command. DOCUMENTATION,
when given, is what help says of it. A name that is a command or function
already is refused, but one given to recorded keys before, which then plays
these. \\[insert-recorded-keys-definition] writes this for an init file:
\(define-recorded-keys 'kill-after-date \"C-s date RET C-k\")."
  (let* ((events (if (stringp keys)
                     (map 'simple-vector (lambda (spec) (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec)))
                          (tuatara.frontend:parse-key-sequence keys))
                     (coerce keys 'simple-vector)))
         (text (recorded-keys-text events))
         (other (command-symbol name)))
    (when (zerop (length events)) (error "No keys to play"))
    (when (or (not (symbolp name)) (null name) (eq (symbol-package name) (find-package '#:common-lisp)))
      (error "~s cannot name recorded keys: it is not a name, or is one of Lisp's own" name))
    (when (or (and (fboundp name) (not (get name 'recorded-keys)))
              (and other (not (eq other name))))
      (error "~(~a~) is the name of a command or function already" name))
    (setf (fdefinition name) (lambda () (play-keys-saying events (play-count)))
          (documentation name 'function)
          (or documentation
              (format nil "Play the recorded keys ~a, as \\[play-recorded-keys] plays the keys recorded last:
with a prefix argument N, N times, with 0 until a play fails." text))
          (get name 'recorded-keys) text
          (gethash name *commands*) (fdefinition name))
    name))

(defun recorded-keys-name-p (name)
  "True when NAME, a string, can name a command of recorded keys: letters,
digits and - _ + * / < > = ! ? %, not all digits."
  (and (plusp (length name))
       (every (lambda (c) (or (alphanumericp c) (find c "-_+*/<>=!?%"))) name)
       (notevery #'digit-char-p name)))

(defun read-recorded-keys-name (prompt default)
  "Ask with PROMPT for a name for recorded keys (RECORDED-KEYS-NAME-P),
DEFAULT, when given, taken when nothing is typed; its symbol in tuatara-user,
or NIL, said why, for a name that cannot be one: one that reads back as a
number or another thing than that symbol (-5, 1e3), or a name of Lisp's own
\(t, nil, pi)."
  (let ((name (string-trim " " (read-string (if default (format nil "~a (default ~(~a~)): " prompt default) (format nil "~a: " prompt))))))
    (cond ((and (zerop (length name)) default) default)
          ((not (recorded-keys-name-p name))
           (message "A name is letters, digits and - _ + * / < > = ! ? %, not digits alone") nil)
          (t (let ((symbol (intern (string-upcase name) '#:tuatara-user))
                   (read (let ((*package* (find-package '#:tuatara-user)) (*read-eval* nil))
                           (ignore-errors (read-from-string name)))))
               (cond ((not (eq read symbol))
                      (message "~a reads as ~:[something else~;~:*~s~], not as a name" name read) nil)
                     ((eq (symbol-package symbol) (find-package '#:common-lisp))
                      (message "~a is a name of Lisp's own" name) nil)
                     (t symbol)))))))

(define-command name-recorded-keys ()
  "Give the keys recorded last a name, asked for, which then plays them as a
command: M-x NAME, or a key bound to it, as \\[play-recorded-keys] does, a
prefix argument as with it. The name lasts while the editor runs;
\\[insert-recorded-keys-definition] writes the text that keeps it in an init
file. A name that is a command already is refused, but one given to
recorded keys before, which then plays these."
  (if (null *last-recorded-keys*)
      (message "No keys have been recorded: \\[start-recording-keys] begins a recording")
      (let ((name (read-recorded-keys-name "Name for the recorded keys" nil)))
        (when name
          (tuatara-user:define-recorded-keys name *last-recorded-keys*)
          (setf *last-recorded-keys-name* name)
          (message "M-x ~(~a~) plays the recorded keys" name)))))

(define-command insert-recorded-keys-definition ()
  "Insert at point the Lisp text that makes the keys recorded last a command
again and binds it to a key, both asked for, for an init file: a name (the
one \\[name-recorded-keys] gave, as standard) and a key (nothing typed for
none). The text is
\(define-recorded-keys 'kill-after-date \"C-s date RET C-k\")
\(bind-key \"C-c d\" 'kill-after-date)
and it is only inserted: putting it in the init file is left to you."
  (let ((events *last-recorded-keys*))
    (if (null events)
        (message "No keys have been recorded: \\[start-recording-keys] begins a recording")
        (let ((text (recorded-keys-text events)))
          (check-writable (current-buffer))
          (if (or (find :paste events :key #'tuatara.frontend:event-kind)
                  (not (equal (map 'list (lambda (event) (tuatara.frontend:key-spec (tuatara.frontend:event-key event))) events)
                              (ignore-errors (tuatara.frontend:parse-key-sequence text)))))
              (message "These keys cannot all be written as text: ~a" text)
              (let ((name (read-recorded-keys-name "Name of the command" *last-recorded-keys-name*)))
                (when name
                  (let ((key (string-trim " " (read-string "Key to bind it to (nothing for none): "))))
                    (if (and (plusp (length key))
                             (not (and (valid-key-text-p key) (not (string= "M-x " key :end2 (min 4 (length key)))))))
                        (message "~a is not a key" key)
                        (let ((lisp (format nil "(define-recorded-keys '~(~a~) ~s)~%~@[(bind-key ~s '~(~a~))~%~]"
                                            name text (and (plusp (length key)) key) name)))
                          (goto (tuatara.buffer:buffer-insert (current-buffer) (point)
                                                              (text-for-buffer lisp (current-buffer))))))))))))))

(defun install-recorded-keys ()
  "Show in the mode line while keys are recorded and played, and have e
play the recorded keys once more right after \\[play-recorded-keys]."
  (pushnew 'recorded-keys-mode-line *mode-line-lead-functions*)
  (define-repeat-keys '(("e" play-recorded-keys))))
