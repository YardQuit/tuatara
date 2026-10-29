;;;; test/recorded-keys.lisp --- Keys recorded and played back: C-x ( C-x ) C-x e, the counter, names.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test recorded-keys
  :description "Keys recorded with C-x ( and C-x ), answers typed to questions among them, and
played again with C-x e: as many times as a prefix argument says, or until a play fails; a
failing command or search stops every play there; each play one step to undo; nothing drawn
till the end; C-g between plays; the counter they insert; names that keep them as commands.")

(defvar *plays-noted* '()
  "What TEST-NOTE-PLAY noted at each run, latest first.")

(defvar *play-note* nil
  "A function of no arguments TEST-NOTE-PLAY calls and notes what it returns.")

(tuatara.command:define-command test-note-play ()
  "Note what *PLAY-NOTE* gives, for the tests of recorded keys."
  (push (funcall *play-note*) *plays-noted*))

(defun recorded-keys-session (keys &key (text "") (width 60) (height 8) note pending)
  "Run KEYS (as RUN-KEYS takes them) on a scratch buffer holding TEXT, with no
keys recorded before and the counter as it starts, C-c y running
TEST-NOTE-PLAY with NOTE, a function of the frontend; with PENDING, a function
of the frontend, keys count as typed while it answers true. A part of KEYS
\(:PASTE TEXT) is TEXT pasted. Returns the frontend and the editor."
  (setf tuatara.defaults::*last-recorded-keys* nil tuatara.defaults::*last-recorded-keys-name* nil
        tuatara.defaults::*keys-counter* 1 tuatara.defaults::*keys-counter-format* "%d"
        tuatara.defaults::*keys-counter-set* nil tuatara.command::*keys-recording* nil
        *plays-noted* '())
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (setf *play-note* (and note (lambda () (funcall note f))))
    (when pending (setf (tuatara.frontend.fake:fake-pending f) pending))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text text)
    (setf tuatara.defaults::*command-history* '() tuatara.defaults::*pattern-history* '()
          tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
    (queue-test-keys f (list (lambda () (tuatara-user:bind-key "C-c y" 'test-note-play))))
    (dolist (part (if (listp keys) keys (list keys)))
      (if (and (consp part) (eq (first part) :paste))
          (tuatara.frontend.fake:fake-queue-event f (tuatara.frontend:make-event :paste :text (second part)))
          (queue-test-keys f (list part))))
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(defun session-message (editor)
  "What the echo area of EDITOR says last, or the empty string."
  (or (tuatara.command:editor-message editor) ""))

(defparameter *five-lines* (format nil "one~%two~%three~%four~%five~%"))

(define-test (recorded-keys played-as-often-as-asked)
  ;; C-d C-n recorded deletes a line's first character and goes down: played once, then twice
  (is string= (format nil "ne~%wo~%hree~%our~%five~%")
      (session-text (nth-value 1 (recorded-keys-session "C-x ( C-d C-n C-x ) C-x e C-u 2 C-x e" :text *five-lines*))))
  ;; e right after plays them once more, again and again
  (is string= (format nil "ne~%wo~%hree~%our~%five~%")
      (session-text (nth-value 1 (recorded-keys-session "C-x ( C-d C-n C-x ) C-x e e e" :text *five-lines*))))
  ;; C-x e while recording is left out of the recording, which goes on
  (multiple-value-bind (f editor) (recorded-keys-session "C-x ( C-d C-x e C-n C-x ) C-x e" :text *five-lines*)
    (declare (ignore f))
    (is string= (format nil "ne~%wo~%three~%four~%five~%") (session-text editor))))

(define-test (recorded-keys answers-to-questions-played)
  ;; the search's text is recorded with its keys: each play searches for date again
  (let ((text "x date1 date2 date3 date4"))
    (is string= "x date date date3 date4"
        (session-text (nth-value 1 (recorded-keys-session "C-x ( C-s d a t e RET C-d C-x ) C-x e" :text text))))
    ;; with 0, until a play fails: the search finding nothing more ends it
    (multiple-value-bind (f editor) (recorded-keys-session "C-x ( C-s d a t e RET C-d C-x ) C-u 0 C-x e" :text text)
      (declare (ignore f))
      (is string= "x date date date date" (session-text editor))
      (true (search "Played 3 times, until: No match for date" (session-message editor))))))

(define-test (recorded-keys failing-step-stops-every-play)
  ;; a failed search stops the plays there, saying how many were done
  (multiple-value-bind (f editor)
      (recorded-keys-session "C-x ( C-s d a t e RET C-d C-x ) C-u 5 C-x e" :text "x date1 date2 y")
    (declare (ignore f))
    (is string= "x date date y" (session-text editor))
    (true (search "Stopped after 1 of 5 plays: No match for date" (session-message editor))))
  ;; a key bound to nothing stops them too, before the keys after it
  (multiple-value-bind (f editor)
      (recorded-keys-session (list (lambda () (tuatara-user:define-recorded-keys 'tuatara-user::test-recorded-undefined "C-d C-c q C-d"))
                                   "C-u 3 M-x t e s t - r e c o r d e d - u n d e f i n e d RET")
                             :text "abcdef")
    (declare (ignore f))
    (unwind-protect
         (progn (is string= "bcdef" (session-text editor))
                (true (search "Stopped after 0 of 3 plays: C-c q is undefined" (session-message editor))))
      (tuatara.command:forget-command 'tuatara-user::test-recorded-undefined)
      (fmakunbound 'tuatara-user::test-recorded-undefined)))
  ;; keys that end before the question they answer fail, never waiting for more
  (multiple-value-bind (f editor)
      (recorded-keys-session (list (lambda () (tuatara-user:define-recorded-keys 'tuatara-user::test-recorded-unfinished "C-s x y"))
                                   "M-x t e s t - r e c o r d e d - u n f i n i s h e d RET")
                             :text "abc xy")
    (declare (ignore f))
    (unwind-protect (true (search "ended before the question" (session-message editor)))
      (tuatara.command:forget-command 'tuatara-user::test-recorded-unfinished)
      (fmakunbound 'tuatara-user::test-recorded-unfinished))))

(define-test (recorded-keys each-play-one-undo)
  ;; three plays, three steps: one undo takes back the last play alone
  (is string= (format nil "ne~%wo~%hree~%four~%five~%")
      (session-text (nth-value 1 (recorded-keys-session "C-x ( C-d C-n C-x ) C-u 3 C-x e C-x u" :text *five-lines*))))
  (is string= (format nil "ne~%wo~%three~%four~%five~%")
      (session-text (nth-value 1 (recorded-keys-session "C-x ( C-d C-n C-x ) C-u 3 C-x e C-x u C-x u" :text *five-lines*)))))

(define-test (recorded-keys quit-keeps-nothing)
  ;; C-g while recording gives the recording up: the keys recorded before play
  (multiple-value-bind (f editor)
      (recorded-keys-session "C-x ( C-d C-n C-x ) C-x ( C-a C-g C-x ) C-x e" :text *five-lines*)
    (declare (ignore f))
    (is string= (format nil "ne~%wo~%three~%four~%five~%") (session-text editor))
    (false (tuatara.command:keys-recording-p))))

(define-test (recorded-keys nothing-drawn-while-playing)
  ;; nothing is drawn while the keys play, their question's keys included: each play notes
  ;; the same count of frames drawn; the screen shows the end once they are done
  (multiple-value-bind (f editor)
      (recorded-keys-session "C-x ( C-s d a t e RET C-c y C-x ) C-u 3 C-x e" :text "date date date date"
                                                                         :note #'tuatara.frontend.fake:fake-frames)
    (declare (ignore editor))
    (is = 4 (length *plays-noted*))
    (is = 1 (length (remove-duplicates (subseq *plays-noted* 0 3))))
    (true (> (tuatara.frontend.fake:fake-frames f) (first *plays-noted*)))))

(define-test (recorded-keys mode-line-sign)
  ;; Rec, in its face, while recording; gone after
  (multiple-value-bind (f editor) (recorded-keys-session "C-x ( C-n C-x )" :text *five-lines*)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f " Rec "))
    (true (loop for (spans) in (tuatara.frontend.fake:fake-face-history f)
                  thereis (loop for row-spans across spans
                                  thereis (find :mode-line-recording row-spans :key #'third))))
    (false (loop for row below 8
                   thereis (search " Rec " (tuatara.frontend.fake:fake-screen-line f row))))))

(define-test (recorded-keys quit-between-plays)
  ;; C-g typed while they play stops them between two plays; a key typed before it acts after
  (multiple-value-bind (f editor)
      (recorded-keys-session (list "C-x ( C-c y C-x ) C-u 0 C-x e" "C-n" "C-g")
                             :text *five-lines*
                             :note (lambda (f) (declare (ignore f)) (when (> (length *plays-noted*) 1000) (error "Not stopped")) t)
                             :pending (lambda (f) (and (>= (length *plays-noted*) 6) (tuatara.frontend.fake::fake-events f))))
    (is = 6 (length *plays-noted*))
    (true (tuatara.frontend.fake:fake-frame-showing f "Quit after 5 plays"))
    (is = 4 (session-point editor))))

(define-test (recorded-keys progress-on-long-plays)
  ;; plays going on for more than a second say in the mode line how many are done
  (multiple-value-bind (f editor)
      (recorded-keys-session "C-x ( C-c y C-x ) C-u 1 5 0 C-x e" :text *five-lines* :width 80
                                                                 :note (lambda (f) (declare (ignore f)) (sleep 1/100) t))
    (declare (ignore editor))
    (true (some (lambda (screen) (some (lambda (row) (and (search "Played " row) (search "/150" row))) screen))
                (tuatara.frontend.fake:fake-history f)))))

(define-test (recorded-keys counter)
  ;; the counter steps each time it is inserted, in the format set
  ;; a format that cannot be used is refused, the one before kept
  (multiple-value-bind (f editor)
      (recorded-keys-session "M-x s e t - k e y s - c o u n t e r - f o r m a t RET DEL DEL % q RET" :text "")
    (declare (ignore f))
    (true (search "not understood" (session-message editor)))
    (is string= "%d" tuatara.defaults::*keys-counter-format*))
  (is string= (format nil "1~%2~%3~%~%")
      (session-text (nth-value 1 (recorded-keys-session "C-x ( M-x i n s e r t - k e y s - c o u n t e r RET C-n C-x ) C-u 2 C-x e"
                                                        :text (format nil "~%~%~%~%")))))
  (is string= (format nil "005~%006~%~%")
      (session-text (nth-value 1 (recorded-keys-session
                                  (list "M-x s e t - k e y s - c o u n t e r - f o r m a t RET DEL DEL % 0 3 d RET"
                                        "M-x s e t - k e y s - c o u n t e r RET 5 RET"
                                        "C-x ( M-x i n s e r t - k e y s - c o u n t e r RET C-n C-x ) C-x e")
                                  :text (format nil "~%~%~%"))))))

(define-test (recorded-keys kept-by-name)
  (unwind-protect
       (progn
         ;; named, the keys are a command of that name
         (is string= (format nil "ne~%wo~%three~%four~%five~%")
             (session-text (nth-value 1 (recorded-keys-session
                                         "C-x ( C-d C-n C-x ) M-x n a m e - r e c o r d e d - k e y s RET t e s t - k i l l - f i r s t RET M-x t e s t - k i l l - f i r s t RET"
                                         :text *five-lines*))))
         (true (tuatara.command:find-command 'tuatara-user::test-kill-first))
         ;; a command's name is refused
         (multiple-value-bind (f editor)
             (recorded-keys-session "C-x ( C-d C-x ) M-x n a m e - r e c o r d e d - k e y s RET f o r w a r d - c h a r RET" :text "abc")
           (declare (ignore f))
           (true (search "forward-char is the name of a command" (session-message editor))))
         ;; the definition inserted for an init file makes the same command again, bound to the key
         (let ((written nil))
           (multiple-value-bind (f editor)
               (recorded-keys-session "C-x ( C-d C-n C-x ) M-x i n s e r t - r e c o r d e d - k e y s - d e f i n i t i o n RET t e s t - k i l l - a g a i n RET C - c SPC m RET"
                                      :text "x")
             (declare (ignore f))
             (setf written (session-text editor)))
           (is string= (format nil "(define-recorded-keys 'test-kill-again \"C-d C-n\")~%(bind-key \"C-c m\" 'test-kill-again)~%") written)
           (let ((*package* (find-package :tuatara-user)))
             (eval (read-from-string (subseq written 0 (position #\Newline written)))))
           (is string= (format nil "ne~%two~%three~%four~%five~%")
               (session-text (nth-value 1 (recorded-keys-session "M-x t e s t - k i l l - a g a i n RET" :text *five-lines*))))))
    (dolist (name '(tuatara-user::test-kill-first tuatara-user::test-kill-again))
      (tuatara.command:forget-command name)
      (fmakunbound name))))

(defun pending-while-playing (f)
  "True while recorded keys play, between plays too, and F has events
queued: keys typed meanwhile."
  (and (plusp tuatara.command::*keys-plays-within*) (tuatara.frontend.fake::fake-events f)))

(define-test (recorded-keys played-search-ends-whatever-is-typed)
  ;; a key typed while a played search runs waits till the plays are over; the searches end
  (multiple-value-bind (f editor)
      (recorded-keys-session (list "C-x ( C-s d a t e RET C-d C-x ) C-u 2 C-x e" "C-a")
                             :text "x date1 date2 date3" :pending #'pending-while-playing)
    (declare (ignore f))
    (is string= "x date date date" (session-text editor))
    (is = 0 (session-point editor)))
  ;; C-g typed while they play stops them, whatever they are at
  (multiple-value-bind (f editor)
      (recorded-keys-session (list "C-x ( C-s d a t e RET C-d C-x ) C-u 5 C-x e" "C-g")
                             :text "x date1 date2 date3 date4 date5 date6" :pending #'pending-while-playing)
    (is string= "x date date date3 date4 date5 date6" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Quit after 1 of 5 plays")))
  ;; in a played search, only C-g stops it, and the play with it; other keys wait for after
  (let* ((f (tuatara.frontend.fake:make-fake-frontend))
         (tuatara.command:*editor* (tuatara.command:make-editor f))
         (tuatara.command::*keys-playing* (tuatara.command::make-playing (vector)))
         (stop (tuatara.defaults::search-stopper nil nil)))
    (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
    (tuatara.frontend.fake:fake-queue-keys f "C-n")
    (false (funcall stop))
    (is = 1 (length (tuatara.command:editor-unread tuatara.command:*editor*)))
    (tuatara.frontend.fake:fake-queue-keys f "C-g")
    (true (funcall stop))
    (is eq :quit (tuatara.command::playing-failed tuatara.command::*keys-playing*))))

(define-test (recorded-keys held-keys-recorded-once)
  ;; keys a question held back and put back are recorded once, when they are read again
  (let* ((f (tuatara.frontend.fake:make-fake-frontend))
         (editor (tuatara.command:make-editor f))
         (tuatara.command::*keys-recording* (tuatara.command::make-recording))
         (a (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\a)))
         (b (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\b))))
    (tuatara.command::record-event a)
    (tuatara.command::record-event b)
    (tuatara.command::put-back-events editor (list a b))
    (tuatara.command:read-event editor)
    (tuatara.command:read-event editor)
    (is equal (list a b) (coerce (tuatara.command::recording-events tuatara.command::*keys-recording*) 'list))))

(define-test (recorded-keys pastes-recorded)
  ;; text pasted while recording is pasted again by each play
  (is string= "hi hi hi "
      (session-text (nth-value 1 (recorded-keys-session (list "C-x (" '(:paste "hi ") "C-x ) C-u 2 C-x e"))))))

(define-test (recorded-keys each-buffer-one-undo)
  ;; a play that edits two buffers is one step to undo in each: one undo there after two
  ;; plays leaves what one play left
  (flet ((other-after (keys)
           (unwind-protect
                (progn
                  (recorded-keys-session (list (lambda () (tuatara.buffer:make-buffer "other" :text "abcdefghijkl"))
                                               "C-x ( C-d C-x b o t h e r RET C-d C-f C-d C-x b RET C-x )"
                                               keys)
                                         :text "123456")
                  (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "other")))
             (let ((other (tuatara.buffer:find-buffer "other")))
               (when other (tuatara.buffer:kill-buffer other))))))
    (let ((once (other-after "C-x e")))
      (isnt string= once (other-after "C-u 2 C-x e"))
      (is string= once (other-after "C-u 2 C-x e C-x b o t h e r RET C-x u")))))

(define-test (recorded-keys windows-laid-out-while-playing)
  ;; C-v played after moving lands where typing it does: the window is laid out between the keys
  (let ((text (format nil "~{line ~d~%~}" (loop for i below 200 collect i))))
    (is = (session-point (nth-value 1 (recorded-keys-session "C-u 1 0 C-n C-v C-u 1 0 C-n C-v C-u 1 0 C-n C-v" :text text)))
        (session-point (nth-value 1 (recorded-keys-session "C-x ( C-u 1 0 C-n C-v C-x ) C-u 2 C-x e" :text text))))))

(define-test (recorded-keys unfinished-question-not-waited-on)
  ;; keys ending inside a question fail at once, work in the background going on or not,
  ;; one whose candidates come in the background too
  (let ((busy (lambda (editor) (declare (ignore editor)) t))
        (seconds nil) (started nil))
    (unwind-protect
         (multiple-value-bind (f editor)
             (recorded-keys-session
              (list (lambda ()
                      (tuatara-user:define-recorded-keys 'tuatara-user::test-recorded-unfinished "C-x C-f")
                      (push busy tuatara.command:*tick-functions*))
                    :tick
                    (lambda () (setf started (get-internal-real-time)))
                    "M-x t e s t - r e c o r d e d - u n f i n i s h e d RET"
                    (lambda ()
                      (setf seconds (/ (- (get-internal-real-time) started) internal-time-units-per-second)
                            tuatara.command:*tick-functions* (remove busy tuatara.command:*tick-functions*))))
              :text "abc xy")
           (declare (ignore f))
           (true (search "ended before the question" (session-message editor)))
           (true (< seconds 2)))
      (setf tuatara.command:*tick-functions* (remove busy tuatara.command:*tick-functions*))
      (tuatara.command:forget-command 'tuatara-user::test-recorded-unfinished)
      (fmakunbound 'tuatara-user::test-recorded-unfinished))))

(define-test (recorded-keys names-that-read-back)
  ;; a name that reads as a number, or is one of Lisp's own, is refused
  (multiple-value-bind (f editor)
      (recorded-keys-session "C-x ( C-n C-x ) M-x n a m e - r e c o r d e d - k e y s RET - 5 RET" :text *five-lines*)
    (declare (ignore f))
    (true (search "-5 reads as -5, not as a name" (session-message editor)))
    (false (fboundp 'tuatara-user::|-5|)))
  (multiple-value-bind (f editor)
      (recorded-keys-session "C-x ( C-n C-x ) M-x n a m e - r e c o r d e d - k e y s RET p i RET" :text *five-lines*)
    (declare (ignore f))
    (true (search "pi is a name of Lisp's own" (session-message editor)))))

(define-test (recorded-keys counter-width-bounded)
  ;; a width that would take more memory than any counter needs is refused
  (multiple-value-bind (f editor)
      (recorded-keys-session "M-x s e t - k e y s - c o u n t e r - f o r m a t RET DEL DEL % 2 0 0 0 0 d RET" :text "")
    (declare (ignore f))
    (true (search "a width of more than 1,000 is not taken" (session-message editor)))
    (is string= "%d" tuatara.defaults::*keys-counter-format*)))

(define-test (recorded-keys refused-step-fails-the-play)
  ;; a recorded key refused when played (the buffer made read-only) stops every play there
  (multiple-value-bind (f editor)
      (recorded-keys-session "C-x ( C-d C-n C-x ) C-x C-q C-u 3 C-x e" :text *five-lines*)
    (declare (ignore f))
    (is string= (format nil "ne~%two~%three~%four~%five~%") (session-text editor))
    (true (search "Stopped after 0 of 3 plays" (session-message editor)))
    (true (search "read-only" (session-message editor)))))
