;;;; test/frames.lisp --- Several frames in one editor: keys, repeats, recordings, messages and windows.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test frames
  :description "An editor with several frames sharing one queue of events: each event runs in its frame, which holds its own last command, repeat keys and keys recorded. Work begun in a frame says its messages there. Killing a buffer and keeping windows in place hold in every frame. The list of frames is replaced, never changed. The frames of one terminal take turns on it, the one shown named in the mode line, the others never painted.")

(defun run-frames (typing &key (count (1+ (reduce #'max typing :key #'first))) (text "") (width 40) (height 8))
  "Run a session of an editor with COUNT frames, each painted by a fake
frontend, all reading one queue of events, the first frame on a scratch buffer
holding TEXT and each other showing it too, each buffer in the modal layer's
NORMAL state at first. TYPING is a list of (INDEX KEYS):
KEYS, a key sequence typed in the INDEXth frame (0 the first), or a function
called in its turn. Returns the editor, its frames and their frontends."
  (let* ((first (tuatara.frontend.fake:make-fake-frontend :width width :height height))
         (frontends (cons first (loop repeat (1- count)
                                      collect (tuatara.frontend.fake:make-fake-frontend :width width :height height
                                                                                        :joining first))))
         (frames (make-array count)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (dolist (name '("kiwi"))
      (let ((left (tuatara.buffer:find-buffer name)))
        (when left (tuatara.buffer:kill-buffer left))))
    (tuatara.buffer:make-buffer "*scratch*" :text text)
    (setf tuatara.defaults::*command-history* '() tuatara.defaults::*pattern-history* '())
    (let ((editor (without-grammar-questions
                    (tuatara.command:run-editor
                     first :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*")
                     :start (lambda (editor)
                              (setf (aref frames 0) (tuatara.command::editor-frame editor))
                              (loop for i from 1 below count
                                    for f in (rest frontends)
                                    do (setf (aref frames i) (tuatara.command::add-frame editor f)))
                              (loop for (index keys) in typing
                                    do (if (functionp keys)
                                           (tuatara.frontend.fake:fake-queue-action first keys)
                                           (tuatara.frontend.fake:fake-queue-keys first keys
                                                                                  :frame (aref frames index)))))))))
      (values editor (coerce frames 'list) frontends))))

(defmacro with-frames ((editor frames &rest options) typing &body body)
  "Run TYPING in an editor of several frames (RUN-FRAMES, with OPTIONS), then
BODY with EDITOR bound to the editor and each of FRAMES, a list of names, to
its frame in turn."
  (let ((all (gensym "FRAMES")) (others (gensym "OTHERS")))
    `(multiple-value-bind (,editor ,all) (run-frames ,typing ,@options)
       (declare (ignorable ,editor))
       (destructuring-bind (&optional ,@frames &rest ,others) ,all
         (declare (ignorable ,@frames) (ignore ,others))
         ,@body))))

(defun frame-buffer-text (frame)
  "The name of the buffer FRAME's selected window shows, and its text, as a list."
  (let ((buffer (tuatara.display:window-buffer (tuatara.command::frame-window frame))))
    (list (tuatara.buffer:buffer-name buffer) (tuatara.buffer:buffer-string buffer))))

(defstruct (frames-test-job (:include tuatara.runtime:job) (:constructor make-frames-test-job ()))
  "A job of the frames' tests, with no buffer.")

(defmethod tuatara.runtime:job-doing ((job frames-test-job))
  "Testing")

(defun start-frames-test-job (work)
  "Start a FRAMES-TEST-JOB on the editor running, which runs WORK, a function
of no arguments; returns the job."
  (tuatara.runtime:start-job-thread (make-frames-test-job) (lambda (job) (declare (ignore job)) (funcall work))))

(defun wait-for-job (job)
  "Wait, five seconds at most, for JOB to end."
  (loop repeat 500 while (tuatara.runtime:job-running-p job) do (sleep 0.01)))

(define-modal-test (frames keys-run-in-their-frame)
  ;; each key runs in the frame it was typed in: its buffer, its window, its question
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i k i w i") (0 "i t u i"))
    (is equal '("kiwi" "kiwi") (frame-buffer-text second))
    (is equal '("*scratch*" "tui") (frame-buffer-text first))
    (is eq first (tuatara.command::editor-frame editor))))

(define-modal-test (frames repeat-keys-of-their-frame)
  ;; a key repeating the command just run repeats it only in the frame it ran in: typed in
  ;; another frame, it does what it does there, and the first frame can still repeat
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i") (0 "i C-x ( a C-x ) C-x e") (1 "e") (0 "e"))
    (is equal '("kiwi" "e") (frame-buffer-text second))
    (is equal '("*scratch*" "aaa") (frame-buffer-text first))))

(define-modal-test (frames last-command-of-their-frame)
  ;; a kill following a kill in another frame starts a kill of its own, never joined to that one
  (with-frames (editor (first second) :text "kea") '((1 "C-x b k i w i C-j i t u i C-a") (0 "C-k") (1 "C-k"))
    (is equal '("tui" "kea") (mapcar #'princ-to-string (subseq (tuatara.command:editor-kill-ring editor) 0 2)))))

(define-modal-test (frames keys-recorded-in-their-frame)
  ;; keys recorded in one frame are its own: those typed in another meanwhile are not taken
  (with-frames (editor (first second)) '((0 "i C-x (") (1 "C-x b k i w i C-j i x y") (0 "a C-x ) C-x e"))
    (is equal '("kiwi" "xy") (frame-buffer-text second))
    (is equal '("*scratch*" "aa") (frame-buffer-text first))))

(defun frames-probe (index function)
  "A turn of RUN-FRAMES's typing calling FUNCTION with the editor's frames,
in INDEX's turn."
  (list index (lambda () (funcall function (tuatara.command:editor-frames tuatara.command:*editor*)))))

(define-modal-test (frames keys-run-in-their-frame-while-another-asks)
  ;; a key of one frame typed while a question is open in another runs at once, in its own frame,
  ;; the question staying open with its text
  (let ((seen nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j i") '(0 "M-x k e a") '(1 "x")
              (frames-probe 0 (lambda (frames)
                                (setf seen (list (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "kiwi"))
                                                 (tuatara.command::frame-minibuffer-text (first frames))
                                                 (tuatara.command::editor-frame tuatara.command:*editor*)))))
              '(0 "C-g"))
      (is equal (list "x" "kea" first) seen)
      (is equal '("kiwi" "x") (frame-buffer-text second))
      (is equal '("*scratch*" "") (frame-buffer-text first)))))

(define-test (frames job-messages-shown-where-begun)
  ;; a message said by work in the background shows in the frame the work was begun in, though
  ;; another frame is the one worked in when it comes; the frame worked in shows none of it
  (let ((go (sb-thread:make-semaphore)) (job nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (start-frames-test-job (lambda ()
                                                           (sb-thread:wait-on-semaphore go :timeout 5)
                                                           (tuatara.command:message "Pīwakawaka"))))))
              '(0 "C-n")
              (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job))))
      (is eq second (tuatara.runtime::job-frame job))
      (is equal "Pīwakawaka" (tuatara.command::frame-message second))
      (false (equal "Pīwakawaka" (tuatara.command::frame-message first))))))

(define-test (frames job-messages-shown-here-once-their-frame-is-gone)
  ;; a message of work begun in a frame since taken away shows in the frame worked in
  (let ((go (sb-thread:make-semaphore)) (job nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (start-frames-test-job (lambda ()
                                                           (sb-thread:wait-on-semaphore go :timeout 5)
                                                           (tuatara.command:message "Pīwakawaka"))))))
              (list 0 (lambda ()
                        (let ((editor tuatara.command:*editor*))
                          (tuatara.command::remove-frame editor (second (tuatara.command::editor-frames editor))))
                        (sb-thread:signal-semaphore go)
                        (wait-for-job job))))
      (true (tuatara.command::frame-deleted second))
      (is equal (list first) (tuatara.command::editor-frames editor))
      (is equal "Pīwakawaka" (tuatara.command::frame-message first)))))

(define-test (frames deferred-actions-run-where-begun)
  ;; work handed on from the background by work begun in a frame runs with that frame the one
  ;; worked in, and the frame worked in before is so again after
  (let ((go (sb-thread:make-semaphore)) (job nil) (seen '()))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (start-frames-test-job
                                   (lambda ()
                                     (sb-thread:wait-on-semaphore go :timeout 5)
                                     (tuatara.command:defer-action
                                      (lambda ()
                                        (push (tuatara.command::editor-frame tuatara.command:*editor*) seen))))))))
              '(0 "C-n")
              (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job))))
      (is equal (list second) seen)
      (is eq first (tuatara.command::editor-frame editor)))))

(define-test (frames killed-buffer-left-in-no-frame)
  ;; a buffer killed in one frame is shown in no window of any frame
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j") (0 "C-x b k i w i C-j C-x k RET"))
    (false (tuatara.buffer:find-buffer "kiwi"))
    (dolist (frame (list first second))
      (true (tuatara.buffer:live-buffer-p (tuatara.display:window-buffer (tuatara.command::frame-window frame)))))))

(define-test (frames windows-of-every-frame)
  ;; the windows showing a buffer are those of every frame, each with a point of its own
  (with-frames (editor (first second) :text "kea tūī") '((1 "C-e") (0 "C-f"))
    (let* ((buffer (tuatara.buffer:find-buffer "*scratch*"))
           (windows (tuatara.command:buffer-windows buffer editor)))
      (is = 2 (length windows))
      (is equal (list (tuatara.command::frame-window first) (tuatara.command::frame-window second)) windows)
      (is = 1 (tuatara.display:window-point (tuatara.command::frame-window first)))
      (is = (length (sb-ext:string-to-octets "kea tūī" :external-format :utf-8))
          (tuatara.display:window-point (tuatara.command::frame-window second))))))

(define-test (frames list-replaced-never-changed)
  ;; adding and taking away frames replaces the list of frames: a list read before is as it was,
  ;; and threads reading it all the while, as waking the editor and asking for keys do, meet no harm
  (let ((before nil) (with-two nil) (errors '()) (reads 0))
    (with-frames (editor (first))
        (list (list 0 (lambda ()
                        (let* ((editor tuatara.command:*editor*)
                               (buffer (tuatara.buffer:find-buffer "*scratch*"))
                               (done nil)
                               (reader (sb-thread:make-thread
                                        (lambda ()
                                          (loop until done
                                                do (handler-case
                                                       (progn (tuatara.command:key-waiting-p)
                                                              (tuatara.command:wake-editor editor)
                                                              (tuatara.command:buffer-windows buffer editor)
                                                              (dolist (frame (tuatara.command::editor-frames editor))
                                                                (unless (tuatara.command::frame-p frame)
                                                                  (error "Not a frame: ~s" frame)))
                                                              (incf reads))
                                                     (error (e) (push e errors))))))))
                          (setf before (tuatara.command::editor-frames editor))
                          (let ((added (tuatara.command::add-frame
                                        editor (tuatara.frontend.fake:make-fake-frontend
                                                :joining (tuatara.command:editor-frontend editor)))))
                            (setf with-two (tuatara.command::editor-frames editor))
                            (tuatara.command::remove-frame editor added))
                          (dotimes (i 2000)
                            (tuatara.command::remove-frame
                             editor (tuatara.command::add-frame
                                     editor (tuatara.frontend.fake:make-fake-frontend
                                             :joining (tuatara.command:editor-frontend editor)))))
                          (setf done t)
                          (sb-thread:join-thread reader)))))
      (is equal (list first) before)
      (is = 2 (length with-two))
      (is eq first (first with-two))
      (is equal (list first) (tuatara.command::editor-frames editor))
      (true (plusp reads))
      (is equal '() errors))))

(define-test (frames last-frame-stays)
  ;; the only frame is never taken away
  (with-frames (editor (first)) '((0 "C-n"))
    (fail (tuatara.command::remove-frame editor first))
    (is equal (list first) (tuatara.command::editor-frames editor))
    (false (tuatara.command::frame-deleted first))))

(define-modal-test (frames keys-run-in-their-frame-while-a-question-takes-in-work)
  ;; a question whose candidates come from the background (finding a file) reads keys as wakes
  ;; come; a key typed in another frame meanwhile runs there at once, the question keeping its own
  ;; frame and text, its frame the one worked in again after
  (let ((seen nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j i") '(0 "C-x C-f") '(1 "x")
              (frames-probe 0 (lambda (frames)
                                (setf seen (list (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "kiwi"))
                                                 (tuatara.command::frame-minibuffer-active (first frames))
                                                 (tuatara.command::editor-frame tuatara.command:*editor*)))))
              '(0 "C-g"))
      (is equal "x" (first seen))
      (true (second seen))
      (is eq first (third seen))
      (is equal '("kiwi" "x") (frame-buffer-text second))
      (is equal '("*scratch*" "") (frame-buffer-text first))
      (false (tuatara.command::frame-minibuffer-active first))
      (false (tuatara.command::frame-minibuffer-active second))
      (is eq first (tuatara.command::editor-frame editor)))))

(define-modal-test (frames recorded-keys-hold-no-frame)
  ;; keys recorded in a frame are kept without it, and play in another frame
  (with-frames (editor (first second)) '((1 "C-x b k i w i C-j i C-x ( a b C-x )") (0 "i C-x e"))
    (true (plusp (length tuatara.defaults::*last-recorded-keys*)))
    (true (every (lambda (event) (null (tuatara.frontend:event-frame event))) tuatara.defaults::*last-recorded-keys*))
    (is equal '("kiwi" "ab") (frame-buffer-text second))
    (is equal '("*scratch*" "ab") (frame-buffer-text first))))

(defstruct (frames-taken-job (:include frames-test-job) (:constructor make-frames-taken-job ()))
  "A job of the frames' tests that notes the frame it is taken in with, and says so.")

(defvar *frames-taken-in* '()
  "The frames the FRAMES-TAKEN-JOBs were taken in with, the latest first.")

(defmethod tuatara.runtime:job-take-in ((job frames-taken-job))
  "Note the frame worked in as JOB is taken in, and say Kōkako."
  (push (tuatara.command::editor-frame tuatara.command:*editor*) *frames-taken-in*)
  (tuatara.command:message "Kōkako"))

(define-test (frames jobs-taken-in-where-begun)
  ;; a job begun in a frame is taken in with that frame the one worked in, what it says shown
  ;; there, though another frame is worked in when it ends; that one is worked in again after
  (let ((go (sb-thread:make-semaphore)) (job nil) (*frames-taken-in* '()))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (tuatara.runtime:start-job-thread
                                   (make-frames-taken-job)
                                   (lambda (job)
                                     (declare (ignore job))
                                     (sb-thread:wait-on-semaphore go :timeout 5))))))
              '(0 "C-n")
              (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job)))
              '(0 "C-n"))
      (is equal (list second) *frames-taken-in*)
      (is equal "Kōkako" (tuatara.command::frame-message second))
      (is eq first (tuatara.command::editor-frame editor)))))

(defstruct (frames-deferring-job (:include frames-test-job) (:constructor make-frames-deferring-job (taken)))
  "A job of the frames' tests whose taking in calls TAKEN, a function of no arguments."
  (taken nil))

(defmethod tuatara.runtime:job-take-in ((job frames-deferring-job))
  "Call JOB's TAKEN."
  (funcall (frames-deferring-job-taken job)))

(defun run-deferring-job (taken)
  "Run a session of two frames: a FRAMES-DEFERRING-JOB begun in the second
frame, calling TAKEN as it is taken in, ends while the first is worked in.
Returns the editor and its frames, as RUN-FRAMES does."
  (let ((go (sb-thread:make-semaphore)) (job nil))
    (run-frames (list '(1 "C-x b k i w i C-j")
                      (list 1 (lambda ()
                                (setf job (tuatara.runtime:start-job-thread
                                           (make-frames-deferring-job taken)
                                           (lambda (job)
                                             (declare (ignore job))
                                             (sb-thread:wait-on-semaphore go :timeout 5))))))
                      '(0 "C-n")
                      (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job)))
                      '(0 "C-n")))))

(define-test (frames work-deferred-at-taking-in-runs-in-its-frame)
  ;; work a job's taking in hands on runs at once, with the job's frame the one worked in, though
  ;; nothing more is typed there
  (let ((seen '()))
    (multiple-value-bind (editor frames)
        (run-deferring-job (lambda ()
                             (tuatara.command:defer-action
                              (lambda () (push (tuatara.command::editor-frame tuatara.command:*editor*) seen)))))
      (is equal (list (second frames)) seen)
      (is eq (first frames) (tuatara.command::editor-frame editor)))))

(define-test (frames work-deferred-in-a-closed-frame-still-runs)
  ;; work handed on to be done in a frame that closes before it runs is done in the frame left
  (let ((seen '()))
    (multiple-value-bind (editor frames)
        (run-deferring-job (lambda ()
                             (let ((editor tuatara.command:*editor*))
                               (tuatara.command:defer-action
                                (lambda () (push (tuatara.command::editor-frame tuatara.command:*editor*) seen)))
                               (tuatara.command::remove-frame editor (tuatara.command::editor-frame editor)))))
      (is equal (list (first frames)) seen)
      (is equal (list (first frames)) (tuatara.command::editor-frames editor)))))

;;; Frames opened and closed by their commands, in a window

(defun run-window-keys (typing &key (text "") (width 40) (height 8) files start)
  "Run a session of an editor in a fake window, which can open other frames,
on a scratch buffer holding TEXT (or FILES), START called once they are shown.
TYPING is a list of keys, as a string, or (FRAME-INDEX KEYS) for keys typed
in the INDEXth frame open then (0 the first), or a function called in its
turn. Returns the editor, its frames' frontends as they were opened, and the
first frontend."
  (let ((first (tuatara.frontend.fake:make-fake-frontend :width width :height height :font-steps 0)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (dolist (name '("kiwi"))
      (let ((left (tuatara.buffer:find-buffer name)))
        (when left (tuatara.buffer:kill-buffer left))))
    (tuatara.buffer:make-buffer "*scratch*" :text text)
    (setf tuatara.defaults::*command-history* '() tuatara.defaults::*pattern-history* '())
    (dolist (item typing)
      (cond ((stringp item) (tuatara.frontend.fake:fake-queue-keys first item))
            ((functionp item) (tuatara.frontend.fake:fake-queue-action first item))
            (t (destructuring-bind (index keys) item
                 (tuatara.frontend.fake:fake-queue-action
                  first (lambda ()
                          (let ((frame (nth index (tuatara.command:editor-frames tuatara.command:*editor*))))
                            (dolist (spec (reverse (tuatara.frontend:parse-key-sequence keys)))
                              (push (tuatara.frontend:make-event :key
                                                                 :key (tuatara.frontend:spec-key spec)
                                                                 :frame frame)
                                    (car (tuatara.frontend.fake::fake-source first)))))))))))
    (let ((editor (without-grammar-questions
                    (tuatara.command:run-editor first :files files :setup #'tuatara.defaults:install-defaults
                                                      :buffer (tuatara.buffer:find-buffer "*scratch*")
                                                      :start start))))
      (values editor (mapcar #'tuatara.command::frame-frontend (tuatara.command:editor-frames editor)) first))))

(defun frame-shows (frame)
  "The name of the buffer FRAME's selected window shows."
  (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:frame-window frame))))

(define-modal-test (frames make-frame-opens-another)
  ;; C-x 5 2 opens a frame showing the current buffer, brought before the others and worked in:
  ;; what is typed then goes there, into the same buffer
  (multiple-value-bind (editor frontends) (run-window-keys '("C-x 5 2 i k e a"))
    (destructuring-bind (first second) (tuatara.command:editor-frames editor)
      (is eq second (tuatara.command:editor-frame editor))
      (is equal "*scratch*" (frame-shows first))
      (is equal "*scratch*" (frame-shows second))
      (is equal "kea" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*")))
      (is = 1 (tuatara.frontend.fake:fake-raised (second frontends)))
      (true (tuatara.frontend.fake:fake-frame-showing (second frontends) "kea")))))

(define-test (frames leader-on-c-x-spc-in-another-frame)
  ;; a frame opened by C-x 5 2 reaches the leader's keys by C-x SPC, in INSERT state and with modal
  ;; editing off, the key hints shown in that frame alone
  (unwind-protect
       (progn
         (tuatara-user:bind-key "o z" 'end-of-buffer :map :leader)
         (tuatara-user:bind-key "o" nil :map :leader :label "own")
         (loop for (modal state) in '((t "i ") (nil ""))
               do (let ((tuatara.defaults::*modal-editing* modal))
                    (multiple-value-bind (editor frontends)
                        (run-window-keys (list (format nil "C-x 5 2 ~aC-x SPC" state)) :text "abc" :width 80 :height 14)
                      (declare (ignore editor))
                      (true (tuatara.frontend.fake:fake-frame-showing (second frontends) "o +own"))
                      (false (tuatara.frontend.fake:fake-frame-showing (first frontends) "o +own")))
                    (let ((editor (run-window-keys (list (format nil "C-x 5 2 ~aC-x SPC o z" state)) :text "abc")))
                      (destructuring-bind (first second) (tuatara.command:editor-frames editor)
                        (is = 0 (tuatara.display:window-point (tuatara.command::frame-window first)))
                        (is = 3 (tuatara.display:window-point (tuatara.command::frame-window second))))))))
    (tuatara-user:unbind-key "o z" :map :leader)
    (tuatara-user:unbind-key "o" :map :leader)))

;;; Frames taking turns on one terminal

(defun shown-frame-name (f)
  "The frame's name the mode line of F's last frame shows, F1 to F9, or NIL."
  (loop for n from 1 to 9
        for name = (format nil "F~d" n)
        when (screen-showing f (format nil "] ~a " name))
          return name))

(defun note-shown (seen)
  "A function for a test's keys, pushing onto the list in the cell SEEN the
frame's name the terminal's last frame shows in its mode line, and the
position of the frame worked in among the editor's frames."
  (lambda ()
    (let ((editor tuatara.command:*editor*))
      (push (list (shown-frame-name (tuatara.command:editor-frontend editor))
                  (position (tuatara.command:editor-frame editor) (tuatara.command:editor-frames editor)))
            (car seen)))))

(define-test (frames terminal-frames-take-turns)
  ;; in a terminal C-x 5 2 opens a frame shown in place of the one before, on the same terminal,
  ;; the mode line naming the frame shown while there are two or more; C-x 5 o shows the next,
  ;; painted whole; C-x 5 0 shows the one left, and with one frame no name is shown
  (let ((seen (list '())))
    (multiple-value-bind (f editor)
        (run-keys (list (note-shown seen) "C-x 5 2" (note-shown seen) "C-x b k i w i C-j" (note-shown seen)
                        "C-x 5 o" (note-shown seen) "C-x 5 o" (note-shown seen) "C-x 5 0" (note-shown seen)))
      (is equal '((nil 0) ("F2" 1) ("F2" 1) ("F1" 0) ("F2" 1) (nil 0)) (reverse (car seen)))
      (is = 1 (length (tuatara.command:editor-frames editor)))
      (is eq f (tuatara.command:editor-frontend editor))
      ;; F1 shown again showed its own buffer, F2 the one it went to
      (true (tuatara.frontend.fake:fake-frame-showing f "] F1 *scratch*"))
      (true (tuatara.frontend.fake:fake-frame-showing f "] F2 kiwi"))
      (true (tuatara.buffer:find-buffer "kiwi")))))

(defclass naming-fake-frontend (tuatara.frontend.fake:fake-frontend)
  ((frame :initform nil :accessor naming-frame
          :documentation "The frame its events name (FRONTEND-FRAME)."))
  (:documentation "A fake terminal that names a frame in the events it delivers, as a terminal
reached from elsewhere does."))

(defmethod tuatara.frontend:frontend-frame ((f naming-fake-frontend))
  (naming-frame f))

(defmethod (setf tuatara.frontend:frontend-frame) (frame (f naming-fake-frontend))
  (setf (naming-frame f) frame))

(defun type-in-named-frame (keys)
  "A function for a test's turn: KEYS typed on the display of the frame worked
in, each event naming the frame that display names."
  (lambda ()
    (let* ((f (tuatara.command:editor-frontend tuatara.command:*editor*))
           (frame (tuatara.frontend:frontend-frame f)))
      (dolist (spec (reverse (tuatara.frontend:parse-key-sequence keys)))
        (push (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key spec) :frame frame)
              (car (tuatara.frontend.fake::fake-source f)))))))

(define-test (frames terminal-display-names-the-frame-shown)
  ;; a display whose events name their frame names the one shown: keys typed after C-x 5 o run
  ;; there, and after the frame it named is closed, in the one shown in its place
  (let ((f (make-instance 'naming-fake-frontend)) (named '()))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (let ((left (tuatara.buffer:find-buffer "kiwi")))
      (when left (tuatara.buffer:kill-buffer left)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (queue-test-keys f (list "C-x 5 2 C-x b k i w i C-j"
                             (type-in-named-frame "i k e a ESC")
                             "C-x 5 o C-x 5 o C-x 5 o C-x 5 0"
                             (lambda () (push (tuatara.frontend:frontend-frame f) named))
                             (type-in-named-frame "i t u i ESC")))
    (let ((editor (without-grammar-questions
                    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                  :buffer (tuatara.buffer:find-buffer "*scratch*")))))
      (is = 1 (length (tuatara.command:editor-frames editor)))
      (is eq (tuatara.command:editor-frame editor) (first named))
      (false (tuatara.command::frame-deleted (first named)))
      (let ((kiwi (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "kiwi"))))
        (true (search "kea" kiwi) "~s" kiwi)
        (true (search "tui" kiwi) "~s" kiwi))
      (is equal "" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*"))))))

(define-test (frames terminal-frames-share-the-one-display)
  ;; every frame of a terminal paints through its one frontend: none is started for them, none
  ;; stopped as one closes, and the terminal is given back once as the editor ends
  (let ((frames '()) (starts 0) (stops 0) (stops-before-the-end nil))
    (sb-int:encapsulate 'tuatara.frontend:frontend-start 'one-display
                        (lambda (function &rest arguments) (incf starts) (apply function arguments)))
    (sb-int:encapsulate 'tuatara.frontend:frontend-stop 'one-display
                        (lambda (function &rest arguments) (incf stops) (apply function arguments)))
    (unwind-protect
         (multiple-value-bind (f editor)
             (run-keys (list "C-x 5 2 C-x 5 2"
                             (lambda () (setf frames (tuatara.command:editor-frames tuatara.command:*editor*)))
                             "C-x 5 o C-x 5 o C-x 5 0"
                             (lambda () (setf stops-before-the-end stops))))
           (is = 3 (length frames))
           (true (every (lambda (frame) (eq f (tuatara.command::frame-frontend frame))) frames))
           (is equal '("F1" "F3") (mapcar #'tuatara.command::frame-name (tuatara.command:editor-frames editor)))
           (is equal '("F1" "F2" "F3") (mapcar #'tuatara.command::frame-name frames))
           ;; the name of the frame closed is given to the next opened
           (is equal "F2" (tuatara.command::free-frame-name editor))
           (is = 1 starts)
           (is = 0 stops-before-the-end)
           (is = 1 stops))
      (sb-int:unencapsulate 'tuatara.frontend:frontend-start 'one-display)
      (sb-int:unencapsulate 'tuatara.frontend:frontend-stop 'one-display))))

(define-test (frames terminal-hidden-frame-never-painted)
  ;; a frame waiting its turn is never painted, though the buffer it shows changes: every frame
  ;; painted while F2 is shown names F2, and each key paints one frame, the settings never looked
  ;; at for others
  (let ((tuatara.command::*frame-at-least-every* 0) (paints 0) (looks 0) (counting nil) (screens '()))
    (sb-int:encapsulate 'tuatara.command:redisplay 'hidden-frames
                        (lambda (function &rest arguments) (when counting (incf paints)) (apply function arguments)))
    (sb-int:encapsulate 'tuatara.command::look-at-settings 'hidden-frames
                        (lambda (function &rest arguments) (when counting (incf looks)) (apply function arguments)))
    (unwind-protect
         (multiple-value-bind (f editor)
             (run-keys (list "C-x 5 2 i" (lambda () (setf counting t)) "a b c d e f"
                             (lambda ()
                               (setf counting nil
                                     screens (copy-list (tuatara.frontend.fake:fake-history
                                                         (tuatara.command:editor-frontend tuatara.command:*editor*)))))
                             "ESC C-x 5 o"))
           (let ((hidden (first (tuatara.command:editor-frames editor))))
             ;; one for each key, and one for the wake that began the count
             (is = 7 paints)
             (is = 0 looks)
             (false (tuatara.command::frame-painted hidden))
             ;; painted before C-x 5 2 had no name; none showed F1 until it was shown again
             (false (some (lambda (screen) (some (lambda (row) (search "] F1 " row)) screen)) screens))
             (true (tuatara.frontend.fake:fake-frame-showing f "] F1 *scratch*"))
             (true (tuatara.frontend.fake:fake-frame-showing f "abcdef"))))
      (sb-int:unencapsulate 'tuatara.command:redisplay 'hidden-frames)
      (sb-int:unencapsulate 'tuatara.command::look-at-settings 'hidden-frames))))

(defun ticks-garbage-per-key (keys &key hidden)
  "The octets the work done between keys makes (RUN-TICK-FUNCTIONS and
PAINT-OTHER-FRAMES) for each of KEYS typed in a fake terminal, with HIDDEN a
second frame waiting its turn on it."
  (let ((counting nil) (octets 0) (frames 0))
    (dolist (name '(tuatara.command::run-tick-functions tuatara.command::paint-other-frames))
      (sb-int:encapsulate name 'ticks-garbage
                          (lambda (function &rest arguments)
                            (if counting
                                (let ((before (tuatara.bench::bytes-consed)))
                                  (multiple-value-prog1 (apply function arguments)
                                    (incf octets (- (tuatara.bench::bytes-consed) before))))
                                (apply function arguments)))))
    (unwind-protect
         (run-keys (list (if hidden "C-x 5 2 C-x 5 o i" "i")
                         (lambda () (setf frames (length (tuatara.command:editor-frames tuatara.command:*editor*))
                                          counting t))
                         keys
                         (lambda () (setf counting nil))))
      (dolist (name '(tuatara.command::run-tick-functions tuatara.command::paint-other-frames))
        (sb-int:unencapsulate name 'ticks-garbage)))
    (values (/ octets (1+ (count #\Space keys))) frames)))

(define-test (frames terminal-hidden-frame-makes-no-garbage)
  ;; the work between keys makes nothing, and a frame waiting its turn adds nothing to it: the work
  ;; that goes through every frame's windows makes as much with it as without
  (let ((keys "a b c d DEL DEL DEL DEL a b c d DEL DEL DEL DEL"))
    (flet ((least (hidden)
             ;; the least of five: work timed by the clock may make something once in a while, and
             ;; the count of octets takes in what other threads make meanwhile
             (loop repeat 5 minimize (ticks-garbage-per-key keys :hidden hidden))))
      (is = 2 (nth-value 1 (ticks-garbage-per-key keys :hidden t)))
      (let ((alone (least nil)) (hidden (least t)))
        ;; with nothing going on, the tick functions and the guard around each make nothing
        (true (<= alone 8) "~,1f octets a key between keys" alone)
        (true (<= hidden (+ alone 8)) "~,1f octets a key with a frame hidden, ~,1f without" hidden alone)))))

(define-test (frames terminal-frame-shown-at-the-size-now)
  ;; a frame waiting its turn while the terminal is resized is laid out at the new size when shown
  (multiple-value-bind (f editor)
      (run-keys (list "C-x 5 2"
                      (lambda ()
                        (let ((f (tuatara.command:editor-frontend tuatara.command:*editor*)))
                          (setf (tuatara.frontend.fake::fake-width f) 60 (tuatara.frontend.fake::fake-height f) 12)
                          (push (tuatara.frontend:make-event :resize :width 60 :height 12)
                                (car (tuatara.frontend.fake::fake-source f)))))
                      "C-x 5 o"))
    (declare (ignore f))
    (is = 2 (length (tuatara.command:editor-frames editor)))
    (dolist (frame (tuatara.command:editor-frames editor))
      (is = 60 (tuatara.command::frame-width frame))
      (is = 12 (tuatara.command::frame-height frame))
      (is = 60 (tuatara.display:area-width (tuatara.display:area-root (tuatara.command:frame-window frame)))))))

(define-test (frames terminal-start-frames)
  ;; --frames N in a terminal opens N frames taking turns on it, the files one to each, the
  ;; first shown
  (with-test-directory (dir)
    (let ((paths (loop for name in '("a.txt" "b.txt")
                       collect (namestring (put-file (merge-pathnames name dir) name))))
          (f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
      (tuatara.frontend.fake:fake-queue-keys f "C-g")
      (let ((editor (tuatara.command:run-editor f :files paths :setup #'tuatara.defaults:install-defaults
                                                  :start (tuatara.terminal::start-function :frames 3))))
        (is equal '("a.txt" "b.txt" "a.txt") (mapcar #'frame-shows (tuatara.command:editor-frames editor)))
        (is eq (first (tuatara.command:editor-frames editor)) (tuatara.command:editor-frame editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "] F1 a.txt"))
        (false (tuatara.frontend.fake:fake-frame-showing f "b.txt"))))))

(define-test (frames delete-frame-keeps-the-last)
  ;; C-x 5 0 closes the frame worked in, its buffer staying; the last frame stays, said in plain words
  (multiple-value-bind (editor frontends first-frontend)
      (run-window-keys '("C-x 5 2 C-x b k i w i C-j C-x 5 0 C-x 5 0"))
    (declare (ignore frontends))
    (is = 1 (length (tuatara.command:editor-frames editor)))
    (is equal "*scratch*" (frame-shows (tuatara.command:editor-frame editor)))
    (true (tuatara.buffer:find-buffer "kiwi"))
    (true (tuatara.frontend.fake:fake-frame-showing first-frontend "This is the only frame; C-x C-c quits"))))

(define-test (frames delete-other-frames-keeps-this-one)
  ;; C-x 5 1 closes every frame but the one worked in
  (multiple-value-bind (editor frontends) (run-window-keys '("C-x 5 2 C-x 5 2 C-x b k i w i C-j C-x 5 1"))
    (declare (ignore frontends))
    (is = 1 (length (tuatara.command:editor-frames editor)))
    (is equal "kiwi" (frame-shows (tuatara.command:editor-frame editor)))))

(define-modal-test (frames close-button-closes-its-frame)
  ;; a frame's close button closes that frame only, the editor going on in the others; the last
  ;; one's quits, as C-x C-c does
  (let ((closed nil) (running nil))
    (multiple-value-bind (editor frontends first-frontend)
        (run-window-keys (list "C-x 5 2"
                               (lambda ()
                                 (let ((editor tuatara.command:*editor*))
                                   (setf closed (second (tuatara.command:editor-frames editor)))
                                   (push (tuatara.frontend:make-event :close :frame closed)
                                         (car (tuatara.frontend.fake::fake-source
                                               (tuatara.command:editor-frontend editor))))))
                               "i t u i"
                               (lambda () (setf running (tuatara.command:editor-running tuatara.command:*editor*)))
                               (lambda ()
                                 (push (tuatara.frontend:make-event
                                        :close :frame (first (tuatara.command:editor-frames tuatara.command:*editor*)))
                                       (car (tuatara.frontend.fake::fake-source
                                             (tuatara.command:editor-frontend tuatara.command:*editor*)))))
                               "i k e a"))
      (declare (ignore frontends first-frontend))
      (true (tuatara.command::frame-deleted closed))
      (true running)
      (is = 1 (length (tuatara.command:editor-frames editor)))
      ;; the last frame's close button ended the editor before what came after it
      (false (tuatara.command:editor-running editor))
      (is equal "tui" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*"))))))

(define-test (frames other-frame-goes-round)
  ;; C-x 5 o goes to the next frame, the first after the last, and brings its window before the others
  (let ((seen '()))
    (flet ((note () (push (position (tuatara.command:editor-frame tuatara.command:*editor*)
                                    (tuatara.command:editor-frames tuatara.command:*editor*))
                          seen)))
      (multiple-value-bind (editor frontends first-frontend)
          (run-window-keys (list "C-x 5 o" "C-x 5 2" #'note "C-x 5 o" #'note "C-x 5 o" #'note "C-u - 1 C-x 5 o" #'note))
        (declare (ignore editor))
        (is equal '(1 0 1 0) (reverse seen))
        (is = 2 (tuatara.frontend.fake:fake-raised first-frontend))
        (is = 2 (tuatara.frontend.fake:fake-raised (second frontends)))
        (true (tuatara.frontend.fake:fake-frame-showing first-frontend "There is no other frame"))))))

(define-test (frames focus-selects-its-frame)
  ;; the frame whose window is given the keyboard is the one worked in, where a message said by
  ;; no frame's work shows
  (with-frames (editor (first second))
      (list '(1 "C-x b k i w i C-j")
            '(0 "C-n")
            (list 0 (lambda ()
                      (push (tuatara.frontend:make-event :focus :frame (second (tuatara.command:editor-frames
                                                                                 tuatara.command:*editor*)))
                            (car (tuatara.frontend.fake::fake-source
                                  (tuatara.command:editor-frontend tuatara.command:*editor*))))))
            (list 0 (lambda ()
                      (sb-thread:join-thread (sb-thread:make-thread (lambda () (tuatara.command:message "Ruru")))))))
    (is eq second (tuatara.command:editor-frame editor))
    (is equal "Ruru" (tuatara.command::frame-message second))
    (false (equal "Ruru" (tuatara.command::frame-message first)))))

(define-test (frames others-painted-only-on-change)
  ;; a frame not worked in is painted again only when what it shows changed: typing in another
  ;; buffer leaves it alone, typing in its buffer paints it, the text shown
  (let ((tuatara.command::*frame-at-least-every* 0) (counts '()))
    (flet ((note-paints (editor)
             (push (tuatara.frontend.fake:fake-frames
                    (tuatara.command::frame-frontend (second (tuatara.command:editor-frames editor))))
                   counts)))
      (with-frames (editor (first second))
          (list '(1 "C-x b k i w i C-j")
                '(0 "i")
                (list 0 (lambda ()
                          (push #'note-paints
                                (tuatara.command:editor-post-command-functions tuatara.command:*editor*))))
                '(0 "a b c d e f")
                (list 0 (lambda ()
                          (setf (tuatara.command:editor-post-command-functions tuatara.command:*editor*)
                                (remove #'note-paints (tuatara.command:editor-post-command-functions
                                                       tuatara.command:*editor*)))))
                '(0 "C-x b k i w i C-j i t i e k e"))
        ;; noted after each of a to f: the second frame painted no more meanwhile
        (is = 6 (length counts))
        (is = 1 (length (remove-duplicates counts)) "~s" counts)
        (true (> (tuatara.frontend.fake:fake-frames (tuatara.command::frame-frontend second)) (first counts)))
        (true (tuatara.frontend.fake:fake-frame-showing (tuatara.command::frame-frontend second) "tieke"))))))

(define-test (frames others-left-alone-by-idle-wakes)
  ;; the editor woken with nothing taken in paints no other frame again
  (let ((tuatara.command::*frame-at-least-every* 0) (counts '()))
    (flet ((note-paints (editor)
             (push (tuatara.frontend.fake:fake-frames
                    (tuatara.command::frame-frontend (second (tuatara.command:editor-frames editor))))
                   counts)))
      (with-frames (editor (first second))
          (list '(1 "C-x b k i w i C-j")
                '(0 "i")
                (list 0 (lambda ()
                          (push #'note-paints
                                (tuatara.command:editor-post-command-functions tuatara.command:*editor*))))
                '(0 "a")
                ;; each of these wakes the editor, as work in the background does, with nothing to take in
                (list 0 (lambda ()))
                (list 0 (lambda ()))
                (list 0 (lambda ()))
                '(0 "b"))
        (is = 2 (length counts))
        (is = (first counts) (second counts) "~s" counts)))))

(define-test (frames others-painted-when-a-setting-changes)
  ;; a setting changed by a command, however it sets it, paints a frame not worked in again,
  ;; though no buffer it shows changed
  (let ((tuatara.command::*frame-at-least-every* 0) (counts '()) (was tuatara-user:*debug-on-error*))
    (flet ((note-paints (editor)
             (push (tuatara.frontend.fake:fake-frames
                    (tuatara.command::frame-frontend (second (tuatara.command:editor-frames editor))))
                   counts)))
      (unwind-protect
           (with-frames (editor (first second))
               (list '(1 "C-x b k i w i C-j")
                     '(0 "i")
                     (list 0 (lambda ()
                               (push #'note-paints
                                     (tuatara.command:editor-post-command-functions tuatara.command:*editor*))))
                     '(0 "a b")
                     '(0 "M-x t o g g l e - d e b u g - o n - e r r o r RET")
                     '(0 "c"))
             (is eq (not was) (not (not tuatara-user:*debug-on-error*)))
             ;; noted after a, b, the toggle and c: no paint of the second frame until the toggle;
             ;; after c, it had been painted for the setting
             (destructuring-bind (after-c . before) counts
               (is = 1 (length (remove-duplicates before)) "~s" counts)
               (true (> after-c (first before)) "~s" counts)))
        (setf tuatara-user:*debug-on-error* was)))))

(define-test (frames find-file-in-another)
  ;; C-x 5 f, C-x 5 b and C-x 5 d open a file, a buffer and a directory in a new frame, worked in;
  ;; the frame they were asked in shows what it showed
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "kea.txt" dir) "kea")))
      (multiple-value-bind (editor frontends)
          (run-window-keys (list (format nil "C-x 5 f C-a C-k ~a RET" (rimu-typed (namestring path)))
                                 '(0 "C-x 5 b k i w i C-j")
                                 (format nil "C-x 5 d C-a C-k ~a RET" (rimu-typed (namestring dir)))))
        (declare (ignore frontends))
        (destructuring-bind (first second third fourth) (tuatara.command:editor-frames editor)
          (is equal "*scratch*" (frame-shows first))
          (is equal "kea.txt" (frame-shows second))
          (is equal "kiwi" (frame-shows third))
          (true (tuatara.defaults::rimu-list-p (tuatara.display:window-buffer (tuatara.command:frame-window fourth))))
          (is eq fourth (tuatara.command:editor-frame editor)))))))

(define-test (frames start-frames-spread-files)
  ;; --frames N opens N frames as the editor starts, the files one to each in turn, those left
  ;; over behind the first; the first frame is the one worked in
  (with-test-directory (dir)
    (let ((paths (loop for name in '("a.txt" "b.txt" "c.txt" "d.txt")
                       collect (namestring (put-file (merge-pathnames name dir) name)))))
      (let ((editor (run-window-keys '() :files paths :start (tuatara.terminal::start-function :frames 3))))
        (is equal '("a.txt" "b.txt" "c.txt") (mapcar #'frame-shows (tuatara.command:editor-frames editor)))
        (is eq (first (tuatara.command:editor-frames editor)) (tuatara.command:editor-frame editor))
        (true (tuatara.buffer:find-buffer "d.txt")))
      (let ((editor (run-window-keys '() :files (list (first paths))
                                         :start (tuatara.terminal::start-function :frames 3))))
        (is equal '("a.txt" "a.txt" "a.txt") (mapcar #'frame-shows (tuatara.command:editor-frames editor)))))))

;;; A question open in one frame while the others work

(define-modal-test (frames another-frame-asks-inside-a-question)
  ;; a frame works and asks while a question is open in another: C-g there ends only its own
  ;; question, and the first question's answer, given after, still works, finding what the other made
  (let ((during nil) (after-quit nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j") '(0 "C-x b") '(1 "i x y")
              (frames-probe 0 (lambda (frames)
                                (setf during (list (tuatara.command::frame-minibuffer-active (first frames))
                                                   (tuatara.command::frame-minibuffer-active (second frames))))))
              '(1 "C-x b")
              (frames-probe 0 (lambda (frames)
                                (setf during
                                      (append during
                                              (list (tuatara.command::frame-minibuffer-active (second frames)))))))
              '(1 "C-g")
              (frames-probe 0 (lambda (frames)
                                (setf after-quit (list (tuatara.command::frame-minibuffer-active (first frames))
                                                       (tuatara.command::frame-minibuffer-active (second frames))))))
              '(0 "k i w i C-j"))
      (is equal '(:reading nil :reading) (mapcar (lambda (x) (and x :reading)) during))
      (is equal '(:reading nil) (mapcar (lambda (x) (and x :reading)) after-quit))
      (is equal '("kiwi" "xy") (frame-buffer-text second))
      (is equal '("kiwi" "xy") (frame-buffer-text first))
      (is eq first (tuatara.command::editor-frame editor)))))

(define-test (frames keys-of-a-waiting-question-kept)
  ;; while a frame's question waits for another frame's question, what is typed in it waits, the
  ;; frame saying so; its C-g is kept too, and ends its question once the other's has ended
  (let ((waiting nil) (kept nil) (ended nil))
    (multiple-value-bind (editor frames frontends)
        (run-frames (list '(1 "C-x b k i w i C-j") '(0 "C-x b") '(1 "M-x") '(0 "k i w i")
                          (frames-probe 0 (lambda (frames)
                                            (let ((first (first frames)))
                                              (setf waiting (list (tuatara.command::frame-minibuffer-text first)
                                                                  (length (tuatara.command::frame-unread first))
                                                                  (tuatara.command::frame-minibuffer-active
                                                                   (second frames)))))))
                          '(0 "C-g")
                          (frames-probe 0 (lambda (frames)
                                            (setf kept (tuatara.command::frame-minibuffer-active (first frames)))))
                          '(1 "C-g")
                          (frames-probe 0 (lambda (frames)
                                            (setf ended (list (tuatara.command::frame-minibuffer-active (first frames))
                                                              (tuatara.command::frame-minibuffer-active
                                                               (second frames))
                                                              (tuatara.command::editor-frame
                                                               tuatara.command:*editor*)))))))
      (declare (ignore editor))
      (destructuring-bind (text unread inner) waiting
        (is equal "" text)
        (is = 4 unread)
        (true inner))
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows
             (first frontends) "Waiting for the question in another frame (C-g there ends it)"))
      (true kept)
      (is equal (list nil nil (first frames)) ended)
      (is equal '("*scratch*" "") (frame-buffer-text (first frames))))))

(define-test (frames waiting-question-answered-after)
  ;; what was typed in a frame whose question waited answers that question once the other ends
  (with-frames (editor (first second))
      '((1 "C-x b k i w i C-j") (0 "C-x b") (1 "M-x") (0 "k i w i C-j") (1 "C-g"))
    (is equal "kiwi" (frame-shows first))
    (false (tuatara.command::frame-minibuffer-active first))))

(define-modal-test (frames closed-frame-question-given-up)
  ;; a frame closed by what runs in another while it asks has its question given up; the frame
  ;; left is worked in after
  (let ((seen nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j") '(0 "C-x b") '(1 "C-x 5 1")
              (frames-probe 0 (lambda (frames)
                                (setf seen (list frames (tuatara.command::editor-frame tuatara.command:*editor*)))))
              '(1 "i z"))
      (true (tuatara.command::frame-deleted first))
      (false (tuatara.command::frame-minibuffer-active first))
      (is equal (list (list second) second) seen)
      (is equal '("kiwi" "z") (frame-buffer-text second)))))

(define-modal-test (frames window-closed-while-its-question-waits)
  ;; a frame's window closed while its question waits for another frame's closes it at once; its
  ;; question is given up as soon as the other's ends
  (let ((seen nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j") '(0 "C-x b") '(1 "M-x")
              (list 0 (lambda ()
                        (let* ((editor tuatara.command:*editor*)
                               (closed (first (tuatara.command:editor-frames editor))))
                          (push (tuatara.frontend:make-event :close :frame closed)
                                (car (tuatara.frontend.fake::fake-source (tuatara.command:editor-frontend editor)))))))
              (frames-probe 1 (lambda (frames)
                                (setf seen (list frames (tuatara.command::frame-minibuffer-active (first frames))))))
              '(1 "C-g i z"))
      (is equal (list second) (first seen))
      (true (second seen))
      (true (tuatara.command::frame-deleted first))
      (false (tuatara.command::frame-minibuffer-active first))
      (is equal (list second) (tuatara.command:editor-frames editor))
      (is eq second (tuatara.command::editor-frame editor))
      (is equal '("kiwi" "z") (frame-buffer-text second)))))

(define-test (frames question-given-up-when-its-buffer-killed)
  ;; a question whose buffer is killed in another frame meanwhile is given up, saying why
  (with-frames (editor (first second))
      '((0 "C-x b k i w i C-j") (0 "C-x b") (1 "C-x b k i w i C-j C-x k RET") (1 "C-n"))
    (false (tuatara.buffer:find-buffer "kiwi"))
    (false (tuatara.command::frame-minibuffer-active first))
    (is equal "kiwi was killed in another frame: the question about it is given up"
        (tuatara.command::frame-message first))))

(define-test (frames quitting-inside-a-question-ends-every-level)
  ;; quitting the editor from a frame while another asks gives up that question and ends the
  ;; editor: nothing typed after it runs
  (with-frames (editor (first second))
      '((1 "C-x b k i w i C-j") (0 "C-x b") (1 "C-x C-c") (1 "i z"))
    (false (tuatara.command:editor-running editor))
    (false (tuatara.command::frame-minibuffer-active first))
    (is equal '("kiwi" "") (frame-buffer-text second))))

(define-modal-test (frames questions-open-at-once-bounded)
  ;; past the most questions open at once, what is done in another frame waits, the frame saying
  ;; so, and runs once a question has ended (the first question about a buffer of its own)
  (let ((tuatara.command::*frames-question-depth* 2) (waiting nil))
    (multiple-value-bind (editor frames frontends)
        (run-frames (list '(0 "C-x b k i w i C-j C-x b") '(1 "M-x") '(2 "i k")
                          (frames-probe 0 (lambda (frames)
                                            (setf waiting (list (length (tuatara.command::frame-unread (third frames)))
                                                                (tuatara.buffer:buffer-string
                                                                 (tuatara.buffer:find-buffer "*scratch*"))))))
                          '(1 "C-g") '(0 "C-g")))
      (declare (ignore editor))
      (is equal '(2 "") waiting)
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows (third frontends) "Waiting for the question"))
      (is equal '("*scratch*" "k") (frame-buffer-text (third frames))))))

(define-test (frames waiting-question-painted-as-it-asks)
  ;; a frame asking for a secret, painted while its question waits for another frame's, shows the
  ;; secret as stars: never as typed
  (let ((answer nil))
    (multiple-value-bind (editor frames frontends)
        (run-frames (list '(0 "C-n")
                          (list 0 (lambda ()
                                    (tuatara.command:defer-action
                                     (lambda () (setf answer (tuatara.command:read-password "Passphrase: "))))))
                          '(0 "t u i") '(1 "M-x") '(0 "x") '(1 "C-g") '(0 "RET")))
      (declare (ignore editor frames))
      (is equal "tuix" answer)
      (true (tuatara.frontend.fake:fake-frame-showing-across-rows (first frontends) "Waiting for the question"))
      (true (tuatara.frontend.fake:fake-frame-showing (first frontends) "Passphrase: ***"))
      (false (tuatara.frontend.fake:fake-frame-showing (first frontends) "tui")))))

(define-test (frames work-of-other-frames-taken-in-during-a-question)
  ;; while a frame asks, a job begun in another frame that ends is taken in at once, there; one
  ;; begun in the frame asking waits until its question is answered
  (let ((go (sb-thread:make-semaphore)) (jobs '()) (*frames-taken-in* '()) (during nil))
    (flet ((start ()
             (push (tuatara.runtime:start-job-thread (make-frames-taken-job)
                                                     (lambda (job)
                                                       (declare (ignore job))
                                                       (sb-thread:wait-on-semaphore go :timeout 5)))
                   jobs)))
      (with-frames (editor (first second))
          (list '(1 "C-x b k i w i C-j") (list 1 #'start) '(0 "C-n") (list 0 #'start) '(0 "M-x")
                (list 0 (lambda ()
                          (sb-thread:signal-semaphore go 2)
                          (dolist (job jobs) (wait-for-job job))))
                (frames-probe 0 (lambda (frames)
                                  (setf during (list (copy-list *frames-taken-in*)
                                                     (tuatara.command::frame-minibuffer-active (first frames))
                                                     (tuatara.command::frame-message (second frames))))))
                '(0 "C-g"))
        (destructuring-bind (taken asking said) during
          (is equal (list second) taken)
          (true asking)
          (is equal "Kōkako" said))
        (is equal (list first second) *frames-taken-in*)))))

(define-test (frames work-handed-on-to-another-frame-runs-during-a-question)
  ;; work handed on from the background for a frame runs there while another frame asks
  (let ((go (sb-thread:make-semaphore)) (job nil) (seen '()) (during nil))
    (with-frames (editor (first second))
        (list '(1 "C-x b k i w i C-j")
              (list 1 (lambda ()
                        (setf job (start-frames-test-job
                                   (lambda ()
                                     (sb-thread:wait-on-semaphore go :timeout 5)
                                     (tuatara.command:defer-action
                                      (lambda ()
                                        (push (tuatara.command::editor-frame tuatara.command:*editor*) seen))))))))
              '(0 "M-x")
              (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job)))
              (frames-probe 0 (lambda (frames)
                                (setf during (list (copy-list seen)
                                                   (tuatara.command::frame-minibuffer-active (first frames))))))
              '(0 "C-g"))
      (is equal (list second) (first during))
      (true (second during))
      (is equal (list second) seen))))

(define-test (frames terminal-hidden-frame-question-waits-for-the-shown-one)
  ;; on a terminal, a question work begun in a frame not shown would ask, brought in while the
  ;; frame shown asks, waits for that question to end: the display stays on the frame asking
  ;; until then; then the other frame is shown with its question, and once it is answered the
  ;; first frame is shown again
  (let ((go (sb-thread:make-semaphore)) (job nil) (answer nil) (seen (list '())) (during nil))
    (run-keys (list "C-x 5 2"
                    (lambda ()
                      (setf job (make-frames-deferring-job
                                 (lambda ()
                                   (tuatara.command:defer-action
                                    (lambda () (setf answer (tuatara.command:read-string "Name: ")))))))
                      (tuatara.runtime:start-job-thread job (lambda (job)
                                                              (declare (ignore job))
                                                              (sb-thread:wait-on-semaphore go :timeout 5))))
                    "C-x 5 o M-x"
                    (lambda () (sb-thread:signal-semaphore go) (wait-for-job job))
                    (note-shown seen)
                    (lambda ()
                      (setf during
                            (list answer
                                  (and (tuatara.command::editor-minibuffer-active tuatara.command:*editor*) t))))
                    "C-g k e a" (note-shown seen) "RET" (note-shown seen)))
    (is equal '(nil t) during)
    (is equal "kea" answer)
    (is equal '(("F1" 0) ("F2" 1) ("F1" 0)) (reverse (car seen)))))

(define-test (frames terminal-question-keeps-its-frame-shown)
  ;; on a terminal, the frame keys for going to another frame do not leave a question: the frame
  ;; asking stays shown, its question reached by what is typed
  (let ((seen (list '())) (open nil))
    (run-keys (list "C-x 5 2 C-x 5 o M-x C-x 5 o C-x 5 2 k e a" (note-shown seen)
                    (lambda ()
                      (let ((editor tuatara.command:*editor*))
                        (setf open (list (length (tuatara.command:editor-frames editor))
                                         (and (tuatara.command::editor-minibuffer-active editor) t)
                                         (search "kea" (tuatara.command::editor-minibuffer-text editor))))))
                    "C-g"))
    (is equal '(("F1" 0)) (car seen))
    (destructuring-bind (count active typed) open
      (is = 2 count)
      (true active)
      (true typed))))

(define-test (frames buffer-of-a-waiting-question-kept-as-asked)
  ;; while a replacement asks at a match in one frame, an edit of its buffer typed in another frame
  ;; is refused, saying where the question is, and the replacement then replaces the text it asked at
  (with-frames (editor (first second) :text "kea kea")
      '((0 "M-% k e a RET t u i RET") (1 "i x") (0 "y q"))
    (is equal "tui kea" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*scratch*")))
    (true (tuatara.frontend.fake:fake-frame-showing-across-rows
           (tuatara.command::frame-frontend second) "*scratch* has a question open in F1"))))

(define-test (frames save-question-gives-up-for-a-killed-buffer)
  ;; a question about other buffers than its own (which files to save) is given up when one of them
  ;; is killed in another frame meanwhile
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "kea.txt" dir) "kea")))
      (with-frames (editor (first second))
          (list (list 1 (format nil "C-x C-f C-a C-k ~a RET i x C-x b k i w i C-j" (rimu-typed (namestring path))))
                '(0 "C-x s") '(1 "C-x b k e a . t x t RET C-x k y RET"))
        (false (tuatara.buffer:find-buffer "kea.txt"))
        (false (tuatara.command::frame-minibuffer-active first))
        (true (search "was killed in another frame" (or (tuatara.command::frame-message first) "")))))))

(define-test (frames file-change-put-off-while-a-question-is-about-it)
  ;; a file followed that grows on disk while a question in a frame is about its buffer takes in
  ;; what was added only once the question is answered, and then all of it
  (with-slow-polling
    (with-test-directory (dir)
      (let ((path (put-file (merge-pathnames "lines.txt" dir) (format nil "l1~%")))
            (during nil))
        (with-frames (editor (first second) :count 2)
            (list (list 0 (format nil "C-x C-f C-a C-k ~a RET M-x f o l l o w - f i l e RET"
                                  (rimu-typed (namestring path))))
                  '(0 "M-x")
                  (list 0 (lambda () (append-on-disk path (format nil "l2~%")) (sleep 1)))
                  (list 0 (lambda () (sleep 0.2)))
                  (frames-probe 0 (lambda (frames)
                                    (declare (ignore frames))
                                    (setf during
                                          (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "lines.txt")))))
                  '(0 "C-g")
                  (list 0 (lambda () (sleep 0.5))))
          (is equal (format nil "l1~%") during)
          (is equal (format nil "l1~%l2~%") (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "lines.txt")))
          (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "lines.txt")))))))

(define-test (frames question-kept-over-job-messages)
  ;; work in the background saying something while a question of one key waits: the question stays in
  ;; the echo area, what was said kept in *Messages*
  (let ((go (sb-thread:make-semaphore)) (job nil) (shown :unset) (answer :unset))
    (with-messages-said (said)
      (with-frames (editor (first second))
          (list '(1 "C-x b k i w i C-j") '(0 "C-n")
                (list 0 (lambda ()
                          (setf job (start-frames-test-job (lambda ()
                                                             (sb-thread:wait-on-semaphore go :timeout 5)
                                                             (tuatara.command:message "Compiling in the background..."))))
                          (setf answer (tuatara.command:y-or-n-p* "Carry on?"))))
                (list 0 (lambda () (sb-thread:signal-semaphore go) (wait-for-job job)))
                (frames-probe 0 (lambda (frames) (setf shown (tuatara.command::frame-message (first frames)))))
                '(0 "y"))
        (true (search "Carry on?" shown) "~s" shown)
        (true answer)
        (true (search "Compiling in the background..." (said)))))))
