;;;; src/command/recorded-keys.lisp --- Recording the keys typed and playing them back.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The keys are taken where every key is read (READ-EVENT, dispatch.lisp),
;;;; those typed into questions too, and played back through the same place:
;;;; a play hands them to the command loop's own HANDLE-EVENT one by one, and
;;;; a question asked meanwhile reads its answer from them as from the
;;;; keyboard.

(in-package #:tuatara.command)

(defun start-keys-recording ()
  "Begin recording the keys typed from now on, every one read as typed
\(RECORD-EVENT), until END-KEY-RECORDING."
  (setf *keys-recording* (make-recording)))

(defun drop-command-keys ()
  "Leave the keys of the command running now out of the recording: those of
a command refused while keys are recorded."
  (let ((recording *keys-recording*))
    (when recording
      (setf (fill-pointer (recording-events recording)) (recording-command-start recording)))))

(defun stop-keys-recording ()
  "End the recording, and return the keys recorded, without those of the
command running now (the one that ends it), as a vector of key events;
NIL when none were recorded."
  (let ((recording *keys-recording*))
    (setf *keys-recording* nil)
    (when recording
      (let ((events (recording-events recording)))
        (and (plusp (recording-command-start recording))
             (coerce (subseq events 0 (recording-command-start recording)) 'simple-vector))))))

(defvar *keys-play-progress* nil
  "While recorded keys have been played over and over for more than a second,
how far they have got, as (DONE . TIMES), TIMES 0 when they play until one
play fails; NIL otherwise.")

(defparameter *keys-plays-within-most* 16
  "How many plays of recorded keys may run one inside another (keys that play
named keys, which play others): past that, the innermost fails.")

(defvar *keys-plays-within* 0
  "How many plays of recorded keys run now, one inside another.")

(defun waiting-quit-p (editor)
  "Read what is waiting to be read from EDITOR's frontend: true when \\`C-g'
is among it, the other keys kept for later (KEEP-OR-QUIT-EVENT)."
  (let ((frontend (frame-frontend (editor-frame editor))))
    (loop repeat 64
          while (tuatara.frontend:frontend-input-pending-p frontend)
            thereis (keep-or-quit-event editor (tuatara.frontend:frontend-read-event-timeout frontend 1/100)))))

(defun quit-while-playing-p ()
  "While recorded keys play, read what is waiting to be read: true when
\\`C-g' is among it, which stops the play, the next key it gives then
quitting what reads it; the other keys are kept for after the play. NIL
when no keys play."
  (let ((playing *keys-playing*))
    (when (and playing (waiting-quit-p *editor*))
      (setf (playing-failed playing) :quit)
      t)))

(defun play-once (editor events)
  "Play EVENTS, recorded keys, once: each handed to the command loop as if
typed, the windows laid out before each as the command loop lays them out
\(nothing painted), a question asked meanwhile answered from them. What the
play changed in each buffer is one step to undo there. The words of the
command that failed, or T, or :QUIT after \\`C-g', when one did
\(NOTE-COMMAND-FAILED); else NIL."
  (let ((stacks (mapcar (lambda (buffer) (cons buffer (tuatara.buffer:buffer-undo-stack buffer)))
                        tuatara.buffer:*buffers*))
        (playing (make-playing events)))
    (setf (editor-repeat-map editor) nil
          (editor-pending editor) '()
          (editor-prefix-argument editor) nil)
    (unwind-protect
         (let ((*keys-playing* playing))
           (loop while (and (< (playing-next playing) (length events)) (not (playing-failed playing)))
                 do (lay-out-windows editor)
                    (handle-event editor (read-event editor :pastes t))))
      ;; keys that leave a sequence or a count begun are left at that
      (setf (editor-pending editor) '()
            (editor-prefix-argument editor) nil
            (editor-collecting-prefix editor) nil)
      (dolist (buffer tuatara.buffer:*buffers*)
        (let ((since (cdr (assoc buffer stacks))))
          (tuatara.buffer:merge-undo-groups buffer since)
          ;; the play's step stands as it is, never joined with the next by the command playing it
          (let ((cell *command-undo-since*))
            (when (and cell (eq (car cell) buffer))
              (setf (cdr cell) (tuatara.buffer:buffer-undo-stack buffer)))))))
    (playing-failed playing)))

(defun play-keys (events times)
  "Play EVENTS, a vector of recorded key events, TIMES times, or with TIMES 0
again and again until a play fails. Each play is one step to undo; nothing is
drawn till the end, but for the mode line once a second after the first
second (*KEYS-PLAY-PROGRESS*); between plays keys typed are looked at,
\\`C-g' stopping the plays, the others kept for after them. Returns how many
plays were done, then why they stopped: NIL when all were done, :FAILED when a
command among the keys failed, its words the third value, or :QUIT."
  (when (>= *keys-plays-within* *keys-plays-within-most*)
    (error "Recorded keys play other keys more than ~d deep" *keys-plays-within-most*))
  (let* ((editor *editor*)
         (*keys-plays-within* (1+ *keys-plays-within*))
         (outermost (= *keys-plays-within* 1))
         ;; the playing command stays the last command, as the key that ran it
         (this-command (editor-this-command editor))
         (repeatable (editor-repeatable editor))
         (this-key (editor-this-key editor))
         (next-shown (tuatara.runtime:deadline-in 1))
         (done 0) (stopped nil) (words nil))
    (unwind-protect
         (loop until (and (plusp times) (>= done times))
               do (let ((failed (play-once editor events)))
                    (when failed
                      (setf stopped (if (eq failed :quit) :quit :failed)
                            words (and (stringp failed) failed))
                      (return))
                    (incf done)
                    (when (waiting-quit-p editor)
                      (setf stopped :quit)
                      (return))
                    (when (and outermost (tuatara.runtime:deadline-passed-p next-shown))
                      (setf *keys-play-progress* (cons done times)
                            next-shown (tuatara.runtime:deadline-in 1))
                      (redisplay-or-say editor))))
      (when outermost (setf *keys-play-progress* nil))
      (setf (editor-this-command editor) this-command
            (editor-repeatable editor) repeatable
            (editor-this-key editor) this-key))
    (values done stopped words)))
