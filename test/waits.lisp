;;;; test/waits.lisp --- Waiting for work in the background: keys read, C-g stops the waiting.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test waits
  :description "A command that waits for work in the background (a thread, a large file's lines
being counted, a parse, the characters' names) keeps reading keys: it goes on when the work ends,
showing what it waits for and how far the work has got; C-g gives the waiting up at once, the work
going on unless the wait stops it too; other keys typed meanwhile are handled after it, in order.")

(defun waiting-session (wait keys &key (text "") file (width 100) (height 10))
  "Start a session on a scratch buffer holding TEXT, or visiting FILE, call WAIT,
a function of the editor, in its turn, then type KEYS, keys queued counting as
typed while the editor waits, as a terminal has them. Returns the frontend, the
editor, what WAIT returned (:QUIT when it gave up the command) and the seconds
it took."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height))
        (result :not-called) (seconds nil))
    (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (unless file (tuatara.buffer:make-buffer "*scratch*" :text text))
    (tuatara.frontend.fake:fake-queue-action
     f (lambda ()
         (let ((start (get-internal-real-time)))
           (setf result (handler-case (funcall wait tuatara.command:*editor*)
                          (tuatara.command:quit () :quit))
                 seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
    (queue-test-keys f keys)
    (let ((editor (tuatara.command:run-editor f :files (and file (list file))
                                                :setup #'tuatara.defaults:install-defaults
                                                :buffer (tuatara.buffer:find-buffer "*scratch*"))))
      (values f editor result seconds))))

(defun sleeping-thread (seconds &optional then)
  "A thread that sleeps SECONDS, then calls THEN when given."
  (sb-thread:make-thread (lambda () (sleep seconds) (when then (funcall then))) :name "test work"))

(defun end-thread (thread)
  "End THREAD, work a test left going, and wait for it."
  (ignore-errors (sb-thread:terminate-thread thread))
  (ignore-errors (sb-thread:join-thread thread :default nil)))

(define-test (waits waiting-ends-stops-and-keeps-keys)
  (let ((tuatara-user:*modal-editing* nil))
    ;; the work ends: true, what is waited for shown with the key that stops the waiting
    (let ((thread (sleeping-thread 0.4)))
      (multiple-value-bind (f editor result)
          (waiting-session (lambda (e) (declare (ignore e)) (tuatara.command:wait-interruptibly thread :message "Waiting a while...")) "")
        (declare (ignore editor))
        (is eq t result)
        (true (tuatara.frontend.fake:fake-frame-showing f "Waiting a while... (C-g stops waiting)"))))
    ;; work that ends within the grace shows nothing
    (let ((thread (sleeping-thread 0.01)))
      (multiple-value-bind (f editor result)
          (waiting-session (lambda (e) (declare (ignore e)) (tuatara.command:wait-interruptibly thread :message "Quick work...")) "")
        (declare (ignore editor))
        (is eq t result)
        (false (tuatara.frontend.fake:fake-frame-showing f "Quick work"))))
    ;; C-g gives the waiting up at once, the work going on, nothing more said
    (let ((thread (sleeping-thread 5)))
      (unwind-protect
           (multiple-value-bind (f editor result seconds)
               (waiting-session (lambda (e) (declare (ignore e))
                                  (prog1 (multiple-value-list (tuatara.command:wait-interruptibly thread))
                                    (true (sb-thread:thread-alive-p thread))))
                                "C-g")
             (declare (ignore editor))
             (is equal '(nil :running) result)
             (true (< seconds 1/2) "~,2f s" seconds)
             (true (tuatara.frontend.fake:fake-frame-showing f "Working... (C-g stops waiting)"))
             (false (tuatara.frontend.fake:fake-frame-showing f "did not end at once")))
        (end-thread thread)))
    ;; keys typed meanwhile are kept, and handled after the wait, in order
    (let ((thread (sleeping-thread 0.4)))
      (multiple-value-bind (f editor result)
          (waiting-session (lambda (e)
                             (list (tuatara.command:wait-interruptibly thread)
                                   (length (tuatara.command:editor-unread e))))
                           "a b")
        (declare (ignore f))
        (is equal '(t 2) result)
        (is string= "ab" (session-text editor))))
    ;; the message asked again as the wait goes, showing how far the work has got
    (let ((until (+ (get-internal-real-time) (round (* 0.9 internal-time-units-per-second))))
          (asked 0))
      (multiple-value-bind (f editor result)
          (waiting-session (lambda (e) (declare (ignore e))
                             (tuatara.command:wait-until-interruptibly
                              (lambda () (>= (get-internal-real-time) until))
                              :message (lambda () (format nil "Step ~d" (incf asked)))))
                           "")
        (declare (ignore editor))
        (is eq t result)
        (true (tuatara.frontend.fake:fake-frame-showing f "Step 1 (C-g stops waiting)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Step 3 (C-g stops waiting)"))))
    ;; with STOP, C-g stops the work too: work that then ends is simply stopped
    (let* ((told nil)
           (thread (sb-thread:make-thread (lambda () (loop until told do (sleep 0.01))) :name "test work")))
      (multiple-value-bind (f editor result seconds)
          (waiting-session (lambda (e) (declare (ignore e))
                             (multiple-value-list
                              (tuatara.command:wait-interruptibly thread :message "Busy..."
                                                                         :stop (lambda (thread) (declare (ignore thread)) (setf told t)))))
                           "C-g")
        (declare (ignore editor))
        (is equal '(nil) result)
        (true (< seconds 1/2) "~,2f s" seconds)
        (false (sb-thread:thread-alive-p thread))
        (true (tuatara.frontend.fake:fake-frame-showing f "Busy... (C-g stops it)"))
        (false (tuatara.frontend.fake:fake-frame-showing f "did not end at once"))))
    ;; work that does not end when told is left to end on its own, and said to be
    (let ((thread (sleeping-thread 5)))
      (unwind-protect
           (multiple-value-bind (f editor result)
               (waiting-session (lambda (e) (declare (ignore e))
                                  (multiple-value-list
                                   (tuatara.command:wait-interruptibly thread :stop (lambda (thread) (declare (ignore thread))))))
                                "C-g")
             (declare (ignore editor))
             (is equal '(nil :running) result)
             (true (tuatara.frontend.fake:fake-frame-showing f "Stopped; what was under way did not end at once")))
        (end-thread thread)))))

(define-test (waits waiting-for-lines-counted)
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "big.txt" dir) (sample-lines 5000))))
          (tuatara-user:*modal-editing* nil))
      (flet ((buffer-of (editor) (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (unwind-protect
             (progn
               ;; all the lines: the wait ends when they are counted, showing how far the count has got
               (with-lines-counted-slowly (:pause 0.5)
                 (multiple-value-bind (f editor result)
                     (waiting-session (lambda (e) (let ((b (buffer-of e)))
                                                    (tuatara.defaults::wait-for-counting b)
                                                    (tuatara.buffer:buffer-indexed-p b)))
                                      "" :file path)
                   (is eq t result)
                   (true (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt: " "% (C-g stops waiting)"))
                   (tuatara.buffer:kill-buffer (buffer-of editor))))
               ;; a line: where it starts, the message saying which
               (with-lines-counted-slowly (:pause 0.5)
                 (multiple-value-bind (f editor result)
                     (waiting-session (lambda (e) (tuatara.defaults::wait-for-counting (buffer-of e) 41)) "" :file path)
                   ;; NIL when the count had ended by the time it was looked at
                   (if result
                       (is = (length (to-octets (subseq (sample-lines 5000) 0 (search "41 é" (sample-lines 5000))))) result)
                       (true (tuatara.buffer:buffer-indexed-p (buffer-of editor))))
                   (true (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt up to line 42: "))
                   (tuatara.buffer:kill-buffer (buffer-of editor))))
               ;; C-g gives up the command at once, the counting going on
               (with-lines-counted-slowly (:pause 1.5)
                 (multiple-value-bind (f editor result seconds)
                     (waiting-session (lambda (e)
                                        (let ((b (buffer-of e)))
                                          (handler-case (tuatara.defaults::wait-for-counting b)
                                            (tuatara.command:quit ()
                                              (let ((job (tuatara.io:index-job-of b)))
                                                (true (and job (eq :running (tuatara.io::index-job-status job))))
                                                :quit)))))
                                      "C-g" :file path)
                   (declare (ignore f))
                   (is eq :quit result)
                   (true (< seconds 1/2) "~,2f s" seconds)
                   (tuatara.buffer:kill-buffer (buffer-of editor))))
               ;; a key typed while M-g g waits for the line is handled once it is there
               (with-lines-counted-slowly (:pause 0.5)
                 (let ((tuatara.buffer:*line-scan-limit* 100)
                       (f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 10)))
                   (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
                   (tuatara.frontend.fake:fake-queue-keys f "M-g g 4 2 RET x")
                   (let* ((editor (tuatara.command:run-editor f :files (list path) :setup #'tuatara.defaults:install-defaults))
                          (b (buffer-of editor))
                          (text (tuatara.buffer:buffer-string b)))
                     (true (tuatara.frontend.fake:fake-frame-showing f "Counting the lines of big.txt up to line 42"))
                     (true (search (format nil "~%40 é 中 😀 line~%x41 é") text))
                     (tuatara.buffer:kill-buffer b)))))
          (kill-buffers-visiting dir))))))

(defun thread-under-way (thread)
  "A stand-in for PARSE-UNDER-WAY-P: true while THREAD runs, after waiting at
most the seconds given for it to end."
  (lambda (syntax &optional (seconds 0))
    (declare (ignore syntax))
    (if (plusp seconds)
        (eq :running (sb-thread:join-thread thread :timeout seconds :default :running))
        (sb-thread:thread-alive-p thread))))

(define-test (waits waiting-for-the-outline-parse)
  ;; the outline waits on the parse itself, no thread of its own polling it
  (let* ((done nil)
         (parse (sleeping-thread 0.4 (lambda () (setf done t)))))
    (with-functions-replaced ((tuatara.syntax:syntax-ready-tree (lambda (syntax) (declare (ignore syntax)) (and done :tree)))
                              (tuatara.syntax:parse-under-way-p (thread-under-way parse)))
      (multiple-value-bind (f editor result)
          (waiting-session (lambda (e) (declare (ignore e)) (tuatara.defaults::outline-tree :syntax)) "")
        (declare (ignore editor))
        (is eq :tree result)
        (true (tuatara.frontend.fake:fake-frame-showing f "Reading the text... (C-g stops waiting)")))))
  ;; C-g gives up the command at once, the parse going on
  (let ((parse (sleeping-thread 3)))
    (unwind-protect
         (with-functions-replaced ((tuatara.syntax:syntax-ready-tree (lambda (syntax) (declare (ignore syntax)) nil))
                                   (tuatara.syntax:parse-under-way-p (thread-under-way parse)))
           (multiple-value-bind (f editor result seconds)
               (waiting-session (lambda (e) (declare (ignore e))
                                  (handler-case (tuatara.defaults::outline-tree :syntax)
                                    (tuatara.command:quit () (true (sb-thread:thread-alive-p parse)) :quit)))
                                "C-g")
             (declare (ignore f editor))
             (is eq :quit result)
             (true (< seconds 1/2) "~,2f s" seconds)))
      (end-thread parse))))

(define-test (waits waiting-for-the-org-parse)
  (let* ((parse (fdefinition 'tuatara.defaults::org-parse))
         (text (format nil "* One~%a~%** Two~%b~%* Three~%"))
         (pause 0.4)
         (worker nil))
    (flet ((org-session (keys seconds wait)
             (waiting-session (lambda (e)
                                (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window e))))
                                  (setf pause 0)
                                  (tuatara.defaults::set-major-mode b 'tuatara.defaults::org-mode)
                                  (setf pause seconds)
                                  ;; a change, so that the parse is behind the text
                                  (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-size b) (format nil "* Four~%"))
                                  (funcall wait b)))
                              keys :text text)))
      (with-functions-replaced ((tuatara.defaults::org-parse (lambda (text) (sleep pause) (funcall parse text))))
        (let ((tuatara.defaults::*org-parse-at-once* 0))
          ;; the tree of the text as it is now, once it is parsed
          (multiple-value-bind (f editor result)
              (org-session "" 0.4 (lambda (b) (length (tuatara.defaults::org-headlines (tuatara.defaults::org-tree-now b)))))
            (declare (ignore editor))
            (is = 4 result)
            (true (tuatara.frontend.fake:fake-frame-showing f "Reading the file... (C-g stops waiting)")))
          ;; C-g gives up the command at once, the parse going on
          (multiple-value-bind (f editor result seconds)
              (org-session "C-g" 2 (lambda (b)
                                   (handler-case (tuatara.defaults::org-tree-now b)
                                     (tuatara.command:quit ()
                                       (setf worker (tuatara.defaults::org-state-worker (tuatara.defaults::org-buffer-state b)))
                                       (true (and worker (sb-thread:thread-alive-p worker)))
                                       :quit))))
            (declare (ignore f editor))
            (is eq :quit result)
            (true (< seconds 1/2) "~,2f s" seconds)
            (when worker (sb-thread:join-thread worker :default nil))))))))

(define-test (waits typing-never-parses-a-large-note)
  ;; a note past *org-parse-at-once* is parsed in the background after a change, never by the
  ;; one colouring it; that waits a moment for the parse, and takes it when it is done in time
  (let* ((parse #'tuatara.defaults::org-parse)
         (threads '())
         (text (with-output-to-string (out)
                 (loop while (<= (file-position out) tuatara.defaults::*org-parse-at-once*)
                       for i from 0 do (format out "* Heading ~d~%Some body text.~%" i))))
         (buffer (tuatara.buffer:make-buffer "large.org" :text text)))
    (with-functions-replaced ((tuatara.defaults::org-parse (lambda (text) (push sb-thread:*current-thread* threads) (funcall parse text))))
      (let ((tuatara.defaults::*org-parse-wait* 5))
        (tuatara.buffer:buffer-insert buffer 0 "x")
        (tuatara.defaults::org-buffer-tree buffer)
        (is eql (tuatara.buffer:buffer-version buffer) (tuatara.defaults::org-state-version (tuatara.defaults::org-buffer-state buffer)))
        (is = 1 (length threads))
        (false (member sb-thread:*current-thread* threads))))
    (tuatara.buffer:buffer-mark-saved buffer)
    (tuatara.buffer:kill-buffer buffer)))

(define-test (waits waiting-for-the-character-names)
  (let ((kept tuatara.defaults::*character-names*))
    (unwind-protect
         (flet ((listing (seconds)
                  (setf tuatara.defaults::*character-names* nil
                        tuatara.defaults::*character-names-thread*
                        (sleeping-thread seconds (lambda ()
                                                   (setf tuatara.defaults::*character-names* (list :names)
                                                         tuatara.defaults::*character-names-thread* nil))))))
           ;; the names once listed
           (listing 0.4)
           (multiple-value-bind (f editor result)
               (waiting-session (lambda (e) (declare (ignore e)) (tuatara.defaults::character-names)) "")
             (declare (ignore editor))
             (is equal '(:names) result)
             (true (tuatara.frontend.fake:fake-frame-showing f "Listing the characters' names... (C-g stops waiting)")))
           ;; C-g gives up the command at once, the listing going on, for next time
           (let ((thread (listing 2)))
             (multiple-value-bind (f editor result seconds)
                 (waiting-session (lambda (e) (declare (ignore e))
                                    (handler-case (tuatara.defaults::character-names)
                                      (tuatara.command:quit () (true (sb-thread:thread-alive-p thread)) :quit)))
                                  "C-g")
               (declare (ignore f editor))
               (is eq :quit result)
               (true (< seconds 1/2) "~,2f s" seconds))
             (sb-thread:join-thread thread :default nil)
             (is equal '(:names) tuatara.defaults::*character-names*)))
      (setf tuatara.defaults::*character-names* kept
            tuatara.defaults::*character-names-thread* nil))))

(define-test (waits held-key-frames)
  ;; a key held down: the keys waiting are handled before a frame is drawn, the last one shown
  (flet ((frames (pending)
           (let ((f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
             (when pending
               (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f))))
             (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
               (when scratch (tuatara.buffer:kill-buffer scratch)))
             (tuatara.buffer:make-buffer "*scratch*" :text (format nil "~{line ~d~%~}" (loop for i from 1 to 60 collect i)))
             (queue-test-keys f (format nil "~{~a~^ ~}" (loop repeat 40 collect "j")))
             (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                         :buffer (tuatara.buffer:find-buffer "*scratch*"))))
               (values (length (tuatara.frontend.fake::fake-history f))
                       (tuatara.frontend.fake:fake-frame-showing f "line 41")
                       (session-point editor))))))
    (multiple-value-bind (typed-ahead shown point) (frames t)
      (multiple-value-bind (one-by-one shown-too point-too) (frames nil)
        (true (< typed-ahead 10) "~d frames for 40 keys waiting" typed-ahead)
        (true (>= one-by-one 40))
        (true shown) (true shown-too)
        (is = point point-too)))))

(define-test (waits slow-parses-not-waited-for)
  ;; a note whose last parse took longer than the colouring would wait is not waited for:
  ;; typing in it never stops for its parse
  (let ((state (tuatara.defaults::make-org-state)))
    (setf (tuatara.defaults::org-state-parse-seconds state) 1)
    (false (tuatara.defaults::org-await-parse state (list 7)))
    ;; no wait was begun for that version
    (false (tuatara.defaults::org-state-waited state))))