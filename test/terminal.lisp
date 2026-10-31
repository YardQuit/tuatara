;;;; test/terminal.lisp --- M-x terminal: a terminal inside a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test terminal
  :description "M-x terminal: a shell in a buffer, what is typed going to it (Escape and C-c too) but for the keys kept for the editor, its colours as faces, its size the window's, NORMAL state showing its screen as text, and killing the buffer ending the shell.")

(defun terminal-session (steps &key (width 80) (height 12) (program '("bash" "--norc" "--noprofile" "-i")) (place :full) cell-pixels)
  "Run an editor session on STEPS, each a string of keys typed, (:until
FUNCTION) waiting (ten seconds at most, failing the test past them) until
FUNCTION, of the current buffer, is true, (:do FUNCTION) calling FUNCTION of
the current buffer, or (:mouse BASE COLUMN ROW . MODIFIERS) a mouse key at
that cell of the screen (MODIFIERS :ctrl t, say), the terminal's program
PROGRAM, shown at PLACE. Every terminal still running is ended afterwards,
its buffer killed. CELL-PIXELS, a cell's width and height in pixels as a
list, has the frontend show pictures. Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels))
        (tuatara-user:*terminal-program* program)
        (tuatara-user:*terminal-place* place)
        (tuatara.command::*buffer-places* '()))
    (setf tuatara.defaults::*command-history* '())
    (labels ((buffer () (tuatara.display:window-buffer (tuatara.command:editor-window tuatara.command:*editor*)))
             (take (steps start)
               (let ((step (first steps)))
                 (cond ((null steps))
                       ((and (consp step) (eq (first step) :mouse))
                        (destructuring-bind (base column row &rest modifiers) (rest step)
                          (tuatara.frontend.fake:fake-queue-event
                           f (tuatara.frontend:make-event :key :key (apply #'tuatara.frontend:make-key base :column column :row row modifiers))))
                        (tuatara.frontend.fake:fake-queue-action f (lambda () (take (rest steps) (get-internal-real-time)))))
                       ((and (consp step) (eq (first step) :do))
                        (funcall (second step) (buffer))
                        (take (rest steps) (get-internal-real-time)))
                       ((and (consp step) (eq (first step) :until))
                        (cond ((funcall (second step) (buffer))
                               (take (rest steps) (get-internal-real-time)))
                              ((> (- (get-internal-real-time) start) (* 10 internal-time-units-per-second))
                               (waited-in-vain (second step) 10)
                               (take (rest steps) (get-internal-real-time)))
                              (t (sleep 0.02)
                                 (tuatara.frontend.fake:fake-queue-action f (lambda () (take steps start))))))
                       (t (queue-test-keys f (list step))
                          (tuatara.frontend.fake:fake-queue-action
                           f (lambda () (take (rest steps) (get-internal-real-time)))))))))
      (tuatara.frontend.fake:fake-queue-action f (lambda () (take steps (get-internal-real-time))))
      (unwind-protect
           (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))
        (loop for emulator being the hash-values of tuatara.defaults::*emulators*
              do (tuatara.defaults::stop-emulator emulator))
        ;; and their buffers, so that the next session's first terminal is *terminal* again
        (dolist (buffer (copy-list tuatara.buffer:*buffers*))
          (when (uiop:string-prefix-p "*terminal*" (tuatara.buffer:buffer-name buffer))
            (tuatara.buffer:kill-buffer buffer)))
        (setf tuatara.defaults::*terminal-buffers* '())))))

(defun buffer-holding (text)
  "A function of a buffer: true when its text holds TEXT."
  (lambda (buffer) (search text (tuatara.buffer:buffer-string buffer))))

(define-test (terminal a-shell-typed-into)
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              "e c h o SPC h i - $ ( ( 6 * 7 ) ) RET" (list :until (buffer-holding "hi-42"))))
    (declare (ignore f))
    (let ((buffer (session-buffer editor)))
      (true (uiop:string-prefix-p "*terminal*" (tuatara.buffer:buffer-name buffer)))
      (true (search "hi-42" (tuatara.buffer:buffer-string buffer)))
      ;; the shell's own session, the terminal its controlling one: job control
      (false (search "no job control" (tuatara.buffer:buffer-string buffer)))
      (is eq :insert (tuatara.defaults::modal-state buffer))
      ;; the shell's own lines, not the editor's: nothing typed into the text
      (true (tuatara.buffer:buffer-local buffer :read-only)))))

(define-test (terminal typed-line-shown)
  ;; what is typed shows as it is typed, before RET (the shell's line editor echoing it)
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              "e c h o SPC z z z" (list :until (buffer-holding "echo zzz"))))
    (declare (ignore f))
    (true (search "echo zzz" (tuatara.buffer:buffer-string (session-buffer editor))))))

(define-test (terminal control-keys-reach-the-program)
  ;; C-c stops what runs, the shell asking again at once: what is typed then runs
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                                "s l e e p SPC 3 0 RET" (list :until (buffer-holding "sleep 30"))
                                "C-c" "e c h o SPC a f t e r RET" (list :until (buffer-holding "after"))))
      (declare (ignore f))
      (true (search "after" (tuatara.buffer:buffer-string (session-buffer editor))))
      (true (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second))))))

(define-test (terminal editor-keys-kept)
  ;; C-x stays the editor's: C-x b asks which buffer, nothing sent to the shell
  (let ((f (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                                   "C-x b"))))
    (true (tuatara.frontend.fake:fake-frame-showing f "Switch to buffer"))))

(define-test (terminal colours-as-faces)
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              "p r i n t f SPC ' \\ 0 3 3 [ 3 1 m r e d \\ 0 3 3 [ 0 m - t e x t \\ n ' RET"
                              (list :until (buffer-holding "red-text"))))
    (declare (ignore f))
    (let* ((buffer (session-buffer editor))
           (text (tuatara.buffer:buffer-string buffer))
           (at (search "red-text" text :from-end t))
           (spans (tuatara.defaults::made-spans-between buffer at (+ at 8))))
      (true spans)
      (true (find-if (lambda (span) (and (<= (first span) at) (member :terminal-fg-1 (third span)))) spans)))))

(define-test (terminal colours-follow-the-theme)
  ;; the sixteen basic colours are the theme's; with no theme, the terminal's own (the number)
  (let ((theme tuatara.display:*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'tuatara.defaults::ao-dark)
           (let ((face (tuatara.defaults::terminal-colour-face :fg 1)))
             (is equal (tuatara.display:theme-role-colour (tuatara.display:find-theme 'tuatara.defaults::ao-dark) 'tuatara.defaults::red)
                 (tuatara.display:face-attribute face :fg))
             ;; another theme put in force: bare, then coloured again for it
             (tuatara.display:apply-theme 'tuatara.defaults::ao-light)
             (tuatara.defaults::recolour-terminal-faces)
             (is equal (tuatara.display:theme-role-colour (tuatara.display:find-theme 'tuatara.defaults::ao-light) 'tuatara.defaults::red)
                 (tuatara.display:face-attribute face :fg))
             (tuatara.display:apply-theme nil)
             (tuatara.defaults::recolour-terminal-faces)
             (is eql 1 (tuatara.display:face-attribute face :fg))
             ;; a colour beyond the sixteen, and one of 24 bits, as the program chose them
             (is eql 202 (tuatara.display:face-attribute (tuatara.defaults::terminal-colour-face :fg 202) :fg))
             (tuatara.display:apply-theme 'tuatara.defaults::ao-dark)
             (is equal "#ff0000" (tuatara.display:face-attribute (tuatara.defaults::terminal-colour-face :bg "#ff0000") :bg))))
      (tuatara.display:apply-theme theme)
      (tuatara.defaults::recolour-terminal-faces))))

(define-test (terminal normal-state-as-text)
  ;; C-\ stops typing: the screen stands still as text, keys move point there
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              "e c h o SPC s t i l l RET" (list :until (buffer-holding "still"))
                              "C-\\" "k k"))
    (declare (ignore f))
    (let ((buffer (session-buffer editor)))
      (is eq :support (tuatara.defaults::buffer-modal-class buffer))
      (true (tuatara.buffer:buffer-local buffer 'tuatara.defaults::terminal-history-view))
      (false (search "kk" (tuatara.buffer:buffer-string buffer))))))

(define-test (terminal history-in-normal-state)
  ;; stopping typing shows the whole history as text, colours and all, point on the cursor's line
  (let ((history nil))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                                "p r i n t f SPC ' \\ 0 3 3 [ 3 1 m r e d - l i n e \\ 0 3 3 [ 0 m \\ n ' ; SPC s e q SPC 1 SPC 3 0 0 RET"
                                (list :until (buffer-holding (format nil "~%300~%")))
                                "C-\\"
                                (list :until (buffer-holding "red-line"))
                                (list :until (lambda (b)
                                               (setf history (list (tuatara.buffer:buffer-string b)
                                                                   (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b))
                                                                   (tuatara.buffer:buffer-line-count b)
                                                                   (let ((at (search "red-line" (tuatara.buffer:buffer-string b) :from-end t)))
                                                                     (and at (tuatara.defaults::made-spans-between b at (+ at 8))))))
                                               t))
                                "i" (list :until (lambda (b) (not (search "red-line" (tuatara.buffer:buffer-string b))))))
                          :height 12)
      (declare (ignore f))
      (destructuring-bind (text line lines spans) history
        ;; every line, those gone up off the screen too
        (true (search (format nil "~%1~%2~%3~%") text))
        (true (search (format nil "~%150~%") text))
        (true (>= lines 300))
        ;; the colours kept
        (true (find-if (lambda (span) (member :terminal-fg-1 (third span))) spans))
        ;; point where the program's cursor was: on the prompt after 300
        (true (> line 300)))
      ;; typing again: the screen as it is, not the history
      (let ((buffer (session-buffer editor)))
        (is eq :insert (tuatara.defaults::modal-state buffer))
        (true (< (tuatara.buffer:buffer-line-count buffer) 20))))))

(define-test (terminal history-view-without-modal-editing)
  ;; with modal editing off, the history view all the same: Emacs's keys move there, nothing
  ;; goes to the shell; RET copies what is selected and types into it again
  (let ((tuatara-user:*modal-editing* nil) (copied nil))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                                "e c h o SPC h i s t o r y - 4 2 RET" (list :until (buffer-holding "history-42"))
                                "C-\\" (list :until (lambda (b) (declare (ignore b))
                                                       (tuatara.frontend.fake:fake-frame-showing
                                                        (tuatara.command:editor-frontend tuatara.command:*editor*) "its history as text")))
                                "C-p C-p C-a C-SPC C-e RET"
                                (list :until (lambda (b) (declare (ignore b))
                                               (setf copied (princ-to-string (first (tuatara.command:editor-kill-ring tuatara.command:*editor*))))
                                               t))))
      (true (tuatara.frontend.fake:fake-frame-showing f "Terminal history"))
      (let ((buffer (session-buffer editor)))
        (false (tuatara.buffer:buffer-local buffer 'tuatara.defaults::terminal-history-view))
        (true (search "history-42" copied))))))

(define-test (terminal c-g-stops-typing)
  ;; C-g leaves typing as C-\ does, as it leaves INSERT state anywhere
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$")) "C-g"))
    (declare (ignore f))
    (true (tuatara.buffer:buffer-local (session-buffer editor) 'tuatara.defaults::terminal-history-view))))

(define-test (terminal c-g-to-the-program-when-not-kept)
  ;; C-g left out of the kept keys goes to the program, C-\ still stopping typing
  (let ((tuatara-user:*terminal-editor-keys* (remove "C-g" tuatara-user:*terminal-editor-keys* :test #'string=)))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                                "c a t SPC - v RET" (list :until (buffer-holding "cat -v"))
                                "C-g RET" (list :until (buffer-holding "^G"))))
      (declare (ignore f))
      (let ((buffer (session-buffer editor)))
        (true (search "^G" (tuatara.buffer:buffer-string buffer)))
        (is eq :insert (tuatara.defaults::modal-state buffer))))))

(define-test (terminal escape-follows-escape-quits)
  ;; with *escape-quits* Escape stops typing as C-g does; without, it goes to the program
  (let ((tuatara-user:*escape-quits* t))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$")) "ESC"))
      (declare (ignore f))
      (true (tuatara.buffer:buffer-local (session-buffer editor) 'tuatara.defaults::terminal-history-view))))
  (let ((tuatara-user:*escape-quits* nil))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$")) "ESC"))
      (declare (ignore f))
      (is eq :insert (tuatara.defaults::modal-state (session-buffer editor)))))
  ;; and C-q ESC gives the program its Escape all the same: cat -v shows it as ^[
  (let ((tuatara-user:*escape-quits* t))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                                "c a t SPC - v RET" (list :until (buffer-holding "cat -v"))
                                "C-q ESC RET" (list :until (buffer-holding "^["))))
      (declare (ignore f))
      (true (search "^[" (tuatara.buffer:buffer-string (session-buffer editor))))
      (is eq :insert (tuatara.defaults::modal-state (session-buffer editor))))))

(define-test (terminal kept-keys-sent-with-c-q)
  ;; C-q sends the next key whatever it is: cat -v shows C-g and C-x as it got them
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              "c a t SPC - v RET" (list :until (buffer-holding "cat -v"))
                              "C-q C-g C-q C-x RET" (list :until (buffer-holding "^G^X"))))
    (declare (ignore f))
    (let ((buffer (session-buffer editor)))
      (true (search "^G^X" (tuatara.buffer:buffer-string buffer)))
      (is eq :insert (tuatara.defaults::modal-state buffer)))))

(define-test (terminal fits-its-window)
  ;; the shell is told the window's size, and the new one when it changes
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              "s t t y SPC s i z e RET" (list :until (buffer-holding "12 80"))
                              "C-x 2" "s t t y SPC s i z e RET" (list :until (buffer-holding "6 80")))
                        :width 80 :height 14)
    (declare (ignore f))
    (let ((text (tuatara.buffer:buffer-string (session-buffer editor))))
      (true (search "12 80" text))
      (true (search "6 80" text)))))

(define-test (terminal exit-kills-the-buffer)
  ;; the shell ending kills its buffer, saying how it ended
  (let ((f (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                                   "e x i t SPC 3 RET"
                                   (list :until (lambda (b) (not (uiop:string-prefix-p "*terminal*" (tuatara.buffer:buffer-name b)))))))))
    (true (tuatara.frontend.fake:fake-frame-showing f "*terminal* ended (exit 3)"))
    (false (tuatara.buffer:find-buffer "*terminal*"))
    (is = 0 (hash-table-count tuatara.defaults::*emulators*))))

(defun terminal-windows (editor)
  "EDITOR's windows showing a terminal."
  (remove-if-not (lambda (w) (uiop:string-prefix-p "*terminal*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer w))))
                 (tuatara.command:editor-windows editor)))

(define-test (terminal shown-where-said)
  ;; at the bottom, a third of the screen; on the left, beside the rest
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))) :place :bottom :height 30)
    (declare (ignore f))
    (let* ((windows (tuatara.command:editor-windows editor))
           (terminal (first (terminal-windows editor)))
           (other (find terminal windows :test-not #'eq)))
      (is = 2 (length windows))
      (true (> (tuatara.display:area-top terminal) (tuatara.display:area-top other)))
      (true (<= 6 (tuatara.display:area-height terminal) 12))
      (is eq terminal (tuatara.command:editor-window editor))))
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))) :place :left :width 100)
    (declare (ignore f))
    (let* ((terminal (first (terminal-windows editor)))
           (other (find terminal (tuatara.command:editor-windows editor) :test-not #'eq)))
      (true (< (tuatara.display:area-left terminal) (tuatara.display:area-left other))))))

(defun typed-command (name)
  "The keys typing M-x NAME RET."
  (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string name)))

(define-test (terminal placed-where-asked)
  ;; terminal-left, -right, -top, -bottom and -full each open a new terminal there, whatever *terminal-place* says
  (multiple-value-bind (f editor)
      (terminal-session (list (typed-command "terminal-right") (list :until (buffer-holding "$"))
                              (typed-command "terminal-top") (list :until (buffer-holding "$"))
                              (typed-command "terminal-left") (list :until (buffer-holding "$")))
                        :place :bottom :width 120 :height 40)
    (declare (ignore f))
    (let* ((terminals (terminal-windows editor))
           (left (find "*terminal*<3>" terminals :key (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w))) :test #'string=))
           (right (find "*terminal*" terminals :key (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w))) :test #'string=))
           (top (find "*terminal*<2>" terminals :key (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w))) :test #'string=)))
      (is = 3 (length terminals))
      (is = 4 (length (tuatara.command:editor-windows editor)))
      (true (and left right top))
      (when (and left right top)
        (is = 0 (tuatara.display:area-left left))
        ;; at the side half the screen wide (*terminal-width*), at the top 0.3 of its height (*terminal-size*)
        (true (<= 55 (tuatara.display:area-width left) 62) "~d" (tuatara.display:area-width left))
        (true (<= 9 (tuatara.display:area-height top) 13) "~d" (tuatara.display:area-height top))
        (true (> (tuatara.display:area-left right) (tuatara.display:area-left left)))
        (is = 0 (tuatara.display:area-top top))
        ;; the one opened last selected, typing there
        (is eq left (tuatara.command:editor-window editor)))))
  ;; -full in the selected window, -bottom below everything; neither moves *terminal-place*'s window
  (multiple-value-bind (f editor)
      (terminal-session (list (typed-command "terminal-full") (list :until (buffer-holding "$"))
                              (typed-command "terminal-bottom") (list :until (buffer-holding "$")))
                        :place :right :width 120 :height 40)
    (declare (ignore f))
    (let ((windows (tuatara.command:editor-windows editor)))
      (is = 2 (length windows))
      (is = 2 (length (terminal-windows editor)))
      (true (> (tuatara.display:area-top (tuatara.command:editor-window editor))
               (tuatara.display:area-top (find (tuatara.command:editor-window editor) windows :test-not #'eq)))))))

(define-test (terminal toggled)
  ;; made when there is none, hidden from inside it, shown again: the same terminal, still running
  (let ((emulators '()))
    (multiple-value-bind (f editor)
        (terminal-session (list "M-x t e r m i n a l - t o g g l e RET" (list :until (buffer-holding "$"))
                                "e c h o SPC k e p t - $ ( ( 4 0 + 2 ) ) RET" (list :until (buffer-holding "kept-42"))
                                (list :until (lambda (b) (declare (ignore b))
                                               (push (copy-list (alexandria:hash-table-values tuatara.defaults::*emulators*)) emulators)
                                               t))
                                "M-x t e r m i n a l - t o g g l e RET"
                                (list :until (lambda (b) (declare (ignore b))
                                               (push (length (terminal-windows tuatara.command:*editor*)) emulators)
                                               t))
                                "M-x t e r m i n a l - t o g g l e RET" (list :until (buffer-holding "kept-42"))
                                (list :until (lambda (b) (declare (ignore b))
                                               (push (copy-list (alexandria:hash-table-values tuatara.defaults::*emulators*)) emulators)
                                               t)))
                          :place :bottom :height 30)
      (declare (ignore f))
      (destructuring-bind (shown-again hidden-windows running) emulators
        ;; hidden: no window shows it, its program going on
        (is = 0 hidden-windows)
        (is = 1 (length running))
        ;; shown again, selected, typed into, the same program and screen
        (is equal running shown-again)
        (true (uiop:string-prefix-p "*terminal*" (tuatara.buffer:buffer-name (session-buffer editor))))
        (is eq :insert (tuatara.defaults::modal-state (session-buffer editor)))))))

(defun copied (text)
  "A step of TERMINAL-SESSION: TEXT copied, first on the kill ring."
  (list :do (lambda (buffer) (declare (ignore buffer)) (tuatara.defaults::push-kill text))))

(define-test (terminal pasted-marked-and-safe)
  ;; a program that asks for pastes marked gets them so, the paste's Escape made a space
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (copied (format nil "one~ctwo~%" #\Escape))
                              (list :until (buffer-holding "ready"))
                              "C-V" (list :until (buffer-holding "two"))
                              "RET" (list :until (buffer-holding "201~")))
                        :program '("sh" "-c" "printf '\\033[?2004hready\\n'; stty -echo; cat -v"))
    (declare (ignore f))
    (let ((text (tuatara.buffer:buffer-string (session-buffer editor))))
      (true (search "^[[200~one two" text))
      (true (search "^[[201~" text)))))

(define-test (terminal pasted-into-the-outer-terminal)
  ;; text pasted into the terminal Tuatara runs in goes to the program, marked and made safe
  ;; as any paste there, never typed as keys
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                              (list :do (lambda (buffer)
                                          (declare (ignore buffer))
                                          (tuatara.frontend.fake:fake-queue-event
                                           (tuatara.command:editor-frontend tuatara.command:*editor*)
                                           (tuatara.frontend:make-event :paste :text (format nil "one~c:two~%" #\Escape)))))
                              (list :until (buffer-holding "two"))
                              "RET" (list :until (buffer-holding "201~")))
                        :program '("sh" "-c" "printf '\\033[?2004hready\\n'; stty -echo; cat -v"))
    (declare (ignore f))
    (let ((text (tuatara.buffer:buffer-string (session-buffer editor))))
      (true (search "^[[200~one :two" text))
      (true (search "^[[201~" text)))))

(define-test (terminal pasted-lines-wait-for-ret)
  ;; pasted into the shell, its lines are run only when RET is pressed
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (copied (format nil "echo pasted-~a~%echo pasted-~a" "one" "two"))
                              (list :until (buffer-holding "$"))
                              "C-V" (list :until (buffer-holding "echo pasted-two"))
                              (list :do (lambda (buffer)
                                          (false (search (format nil "~%pasted-one") (tuatara.buffer:buffer-string buffer)))))
                              "RET" (list :until (buffer-holding (format nil "~%pasted-two")))))
    (declare (ignore f))
    (let ((text (tuatara.buffer:buffer-string (session-buffer editor))))
      (true (search (format nil "~%pasted-one") text))
      (true (search (format nil "~%pasted-two") text)))))

(define-test (terminal pasted-from-the-history-view)
  ;; C-y there pastes and types into the terminal again
  (multiple-value-bind (f editor)
      (let ((tuatara-user:*modal-editing* nil))
        (terminal-session (list "M-x t e r m i n a l RET" (copied "echo from-history")
                                (list :until (buffer-holding "$"))
                                "C-g" "C-y" (list :until (buffer-holding "echo from-history"))
                                "RET" (list :until (buffer-holding (format nil "~%from-history"))))))
    (declare (ignore f))
    (let ((buffer (session-buffer editor)))
      (true (search (format nil "~%from-history") (tuatara.buffer:buffer-string buffer)))
      (true (tuatara.defaults::terminal-typing-p buffer)))))

(define-test (terminal follows-directory-and-title)
  ;; the program's cd and its title, in the buffer's directory and the mode line
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                              (list :until (lambda (buffer) (equal "/usr/" (tuatara.buffer:buffer-local buffer :directory)))))
                        :width 120
                        :program '("sh" "-c" "printf '\\033]2;a-title\\007ready\\n'; cd /usr; exec cat"))
    (let ((buffer (session-buffer editor)))
      (is equal "/usr/" (tuatara.buffer:buffer-local buffer :directory))
      (is equal "/usr/" (tuatara.defaults::buffer-directory buffer))
      (true (tuatara.frontend.fake:fake-frame-showing f "*terminal*" "a-title" "/usr/")))))

(define-test (terminal mouse-to-a-program-that-tracks-it)
  ;; a click at the screen's fourth column, third row, as the program asked to hear of it
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                              '(:mouse :down-mouse-1 3 2) '(:mouse :up-mouse-1 3 2)
                              (list :until (buffer-holding "3m")))
                        :program '("sh" "-c" "printf '\\033[?1000h\\033[?1006hready\\n'; stty -echo -icanon; exec cat -v"))
    (declare (ignore f))
    (let ((text (tuatara.buffer:buffer-string (session-buffer editor))))
      (true (search "^[[<0;4;3M^[[<0;4;3m" text) "the program read ~s" text))))

(define-test (terminal wheel-as-keys-in-full-screen)
  ;; a full-screen program that does not track the mouse: the wheel is the up key, three times
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                              '(:mouse :wheel-up 3 2) (list :until (buffer-holding "^[[A^[[A^[[A")))
                        :program '("sh" "-c" "printf '\\033[?1049hready\\n'; stty -echo -icanon; exec cat -v"))
    (declare (ignore f))
    (true (search "^[[A^[[A^[[A" (tuatara.buffer:buffer-string (session-buffer editor))))))

(define-test (terminal wheel-up-shows-the-history)
  ;; the shell, which does not track the mouse: the wheel up goes into the history view
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              '(:mouse :wheel-up 3 2)
                              (list :until (lambda (buffer) (not (tuatara.defaults::terminal-typing-p buffer))))))
    (declare (ignore f))
    (false (tuatara.defaults::terminal-typing-p (session-buffer editor)))))

(define-test (terminal addresses-found)
  (is equal '("https://example.org/a_(b)" "http://x.y/z" "file:///tmp/f")
      (mapcar (lambda (text) (apply #'subseq "see https://example.org/a_(b). and (http://x.y/z), file:///tmp/f'" text))
              (tuatara.defaults::text-urls "see https://example.org/a_(b). and (http://x.y/z), file:///tmp/f'"))))

(define-test (terminal links-underlined-and-opened)
  ;; a link a program wrote (OSC 8) and an address written: underlined; C-click on the
  ;; first, to a file here, opens it in Tuatara
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "linked.txt" dir))))
      (with-open-file (out file :direction :output :if-exists :supersede) (write-line "linked file" out))
      (multiple-value-bind (f editor)
          (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                                  (list :do (lambda (buffer)
                                              (let ((emulator (tuatara.buffer:buffer-local buffer :emulator)))
                                                (is equal (format nil "file://~a" file)
                                                    (tuatara.defaults::terminal-link-at buffer emulator 1 0))
                                                (is equal "https://example.org/x"
                                                    (tuatara.defaults::terminal-link-at buffer emulator 12 1))
                                                (false (tuatara.defaults::terminal-link-at buffer emulator 3 1))
                                                ;; underlined: the link's text, at the start, and the address on the next row
                                                (multiple-value-bind (octets spans)
                                                    (tuatara.defaults::terminal-shown-text (tuatara.buffer:buffer-local buffer 'tuatara.defaults::terminal-rows)
                                                                                           (tuatara.buffer:buffer-local buffer 'tuatara.defaults::terminal-looks) nil 0)
                                                  (let ((text (tuatara.text:octets-string octets)))
                                                  (let ((links (remove-if-not (lambda (span) (member :link (third span))) spans)))
                                                    (true (find 0 links :key #'first))
                                                    (true (find (search "https://example.org/x" text) links :key #'first))))))))
                                  '(:mouse :down-mouse-1 1 0 :ctrl t)
                                  ;; asked first, the file it leads to named
                                  (list :until #'question-asked) "y RET"
                                  (list :until (lambda (buffer) (equal "linked.txt" (tuatara.buffer:buffer-name buffer)))))
                            :width 250
                            :program (list "sh" "-c" (format nil "printf '\\033]8;;file://~a\\033\\\\name\\033]8;;\\033\\\\\\nsee: https://example.org/x.\\nready\\n'; exec cat" file)))
        (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Open ~a in Tuatara?" file)))
        (is equal "linked.txt" (tuatara.buffer:buffer-name (session-buffer editor)))))))

(defun question-asked (buffer)
  "A function for TERMINAL-SESSION's :until: true once the minibuffer asks something."
  (declare (ignore buffer))
  (tuatara.command:editor-minibuffer-active tuatara.command:*editor*))

(defun file-link-program (file)
  "A program for TERMINAL-SESSION writing a link (OSC 8) to FILE that shows a
web address, then ready, and reading what is typed."
  (list "sh" "-c" (format nil "printf '\\033]8;;file://~a\\033\\\\https://example.org/\\033]8;;\\033\\\\\\nready\\n'; exec cat" file)))

(define-test (terminal link-message-stays-after-the-release)
  ;; Control held as the button is let go after a click on a link: the link's answer stays, nothing said of the release
  (let ((f (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                                   '(:mouse :down-mouse-1 1 0 :ctrl t) '(:mouse :up-mouse-1 1 0 :ctrl t))
                             :program (list "sh" "-c" "printf '\\033]8;;file://elsewhere.example/x\\033\\\\name\\033]8;;\\033\\\\\\nready\\n'; exec cat"))))
    (true (search "is no file of this computer" (tuatara.frontend.fake:fake-screen-line f 11)))
    (false (tuatara.frontend.fake:fake-frame-showing f "is undefined"))))

(define-test (terminal file-link-asks-first)
  ;; a link showing a web address but leading to a file: the file named, nothing opened on no
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "other.txt" dir))))
      (with-open-file (out file :direction :output :if-exists :supersede) (write-line "other file" out))
      (multiple-value-bind (f editor)
          (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                                  '(:mouse :down-mouse-1 1 0 :ctrl t)
                                  (list :until #'question-asked) "n RET"
                                  (list :until (lambda (buffer) (declare (ignore buffer)) (not (question-asked nil)))))
                            :width 250 :program (file-link-program file))
        (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Open ~a in Tuatara?" file)))
        (true (uiop:string-prefix-p "*terminal*" (tuatara.buffer:buffer-name (session-buffer editor))))
        (false (find "other.txt" tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-name :test #'equal))))))

(define-test (terminal file-link-in-a-safe-directory-opened-safely)
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "other.txt" dir)))
          (tuatara-user:*safe-directories* (list (namestring dir))))
      (with-open-file (out file :direction :output :if-exists :supersede) (write-line "other file" out))
      (multiple-value-bind (f editor)
          (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                                  '(:mouse :down-mouse-1 1 0 :ctrl t)
                                  (list :until #'question-asked) "y RET"
                                  (list :until (lambda (buffer) (equal "other.txt" (tuatara.buffer:buffer-name buffer)))))
                            :width 250 :program (file-link-program file))
        (declare (ignore f))
        (let ((buffer (session-buffer editor)))
          (is equal "other.txt" (tuatara.buffer:buffer-name buffer))
          (true (tuatara.io:safe-visit-p buffer)))))))

(define-test (terminal starts-at-home-from-a-safe-visit)
  ;; from a file opened safely, or a folder of *safe-directories*: the shell starts at home
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "evidence.txt" dir)))
          (home (tuatara.io:home-directory)))
      (with-open-file (out file :direction :output :if-exists :supersede) (write-line "evidence" out))
      (with-open-file (out (merge-pathnames "plain.txt" dir) :direction :output :if-exists :supersede) (write-line "plain" out))
      (flet ((started (steps)
               (multiple-value-bind (f editor)
                   (terminal-session (append steps (list "M-x t e r m i n a l RET" (list :until (buffer-holding "at:"))))
                                     :width 120 :program '("sh" "-c" "echo at:$(pwd)/; exec cat"))
                 (let ((buffer (session-buffer editor)))
                   (true (search (format nil "at:~a" home) (tuatara.buffer:buffer-string buffer)))
                   (is equal home (tuatara.buffer:buffer-local buffer :directory))
                   (true (tuatara.frontend.fake:fake-frame-showing f "home directory"))))))
        (started (list (list :do (lambda (buffer) (declare (ignore buffer)) (tuatara.defaults::visit-path file :safe t)))))
        ;; a buffer standing in a folder of *safe-directories*
        (let ((tuatara-user:*safe-directories* (list (namestring dir)))
              (place (tuatara.buffer:make-buffer "*in-the-evidence*")))
          (setf (tuatara.buffer:buffer-local place :directory) (namestring dir))
          (unwind-protect
               (started (list (list :do (lambda (buffer) (declare (ignore buffer))
                                          (tuatara.command:show-buffer place)))))
            (tuatara.buffer:kill-buffer place)))
        ;; elsewhere, the buffer's own directory
        (multiple-value-bind (f editor)
            (terminal-session (list (list :do (lambda (buffer) (declare (ignore buffer))
                                                (tuatara.defaults::visit-path (namestring (merge-pathnames "plain.txt" dir)))))
                                    "M-x t e r m i n a l RET" (list :until (buffer-holding "at:")))
                              :width 120 :program '("sh" "-c" "echo at:$(pwd)/; exec cat"))
          (declare (ignore f))
          (true (search (format nil "/~a/" (car (last (pathname-directory dir))))
                        (tuatara.buffer:buffer-local (session-buffer editor) :directory))))))))

(define-test (terminal multi-line-paste-asks-when-not-marked)
  ;; a program that does not ask for pastes marked: lines pasted only after asking
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (copied (format nil "first-pasted~%second-pasted~%"))
                              (list :until (buffer-holding "ready"))
                              "C-V" (list :until #'question-asked) "n RET"
                              (list :until (lambda (buffer) (declare (ignore buffer)) (not (question-asked nil))))
                              (list :do (lambda (buffer) (false (search "first-pasted" (tuatara.buffer:buffer-string buffer)))))
                              "C-V" (list :until #'question-asked) "y RET"
                              (list :until (buffer-holding "second-pasted")))
                        :program '("sh" "-c" "stty -echo; printf 'ready\\n'; exec cat"))
    (true (tuatara.frontend.fake:fake-frame-showing f "Paste 2 lines; the program runs each as it comes; go on?"))
    (true (search (format nil "first-pasted~%second-pasted") (tuatara.buffer:buffer-string (session-buffer editor))))))

(define-test (terminal search-cancelled-is-forgotten)
  ;; C-s, then typing again before the history is in: viewing the history later asks nothing
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "last-line"))
                              "C-s" "i"
                              (list :until #'tuatara.defaults::terminal-typing-p)
                              "C-g"
                              (list :until (lambda (buffer)
                                             (and (not (tuatara.defaults::terminal-typing-p buffer))
                                                  ;; the history in, not the screen alone
                                                  (> (tuatara.buffer:buffer-line-count buffer) 1000))))
                              ;; a few ticks more, in which a search left over would begin
                              (list :do (lambda (buffer) (declare (ignore buffer)) (sleep 0.3))))
                        :program '("sh" "-c" "seq 1 300000; echo last-line; exec cat"))
    (declare (ignore f))
    (false (tuatara.command:editor-minibuffer-active editor))
    (false (tuatara.defaults::terminal-typing-p (session-buffer editor)))))

(define-test (terminal refused-place-leaves-no-buffer)
  ;; a size that cannot be: refused, and no terminal buffer left behind
  (let ((tuatara-user:*terminal-size* 0))
    (true (tuatara.frontend.fake:fake-frame-showing (terminal-session (list "M-x t e r m i n a l RET") :place :bottom)
                                                    "is no size")))
  (false (find-if (lambda (buffer) (uiop:string-prefix-p "*terminal*" (tuatara.buffer:buffer-name buffer)))
                  tuatara.buffer:*buffers*))
  (is = 0 (hash-table-count tuatara.defaults::*emulators*)))

(define-test (terminal link-found-in-the-history)
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                              "C-g" (list :until (lambda (buffer) (not (tuatara.defaults::terminal-typing-p buffer))))
                              (list :do (lambda (buffer)
                                          (let ((emulator (tuatara.buffer:buffer-local buffer :emulator)))
                                            (is equal "https://example.org/y"
                                                (tuatara.defaults::terminal-link-at buffer emulator 8 1 :history t))))))
                        :program '("sh" "-c" "printf 'a line\\nsee at https://example.org/y\\nready\\n'; exec cat"))
    (declare (ignore f editor))))

(defun picture-program (file format)
  "A program, for TERMINAL-SESSION, that sends the picture in FILE, in kitty's
FORMAT (32 for 2 by 2 pixels of red, green, blue and alpha; 100 for PNG), to
be shown 4 columns by 2 rows, then prints ready and waits."
  (list "sh" "-c" (format nil "printf '\\033_Ga=T,f=~d,~@[~a,~]c=4,r=2,q=2;'; base64 -w0 '~a'; printf '\\033\\\\'; echo; echo ready; exec cat"
                          format (and (= format 32) "s=2,v=2") file)))

(defun shown-pictures (buffer)
  "The pictures BUFFER, a terminal, places over its text."
  (tuatara.buffer:buffer-local buffer :pictures))

(define-test (terminal pictures-shown)
  ;; a picture a program sends, shown over the cells it placed it in
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "pixels" dir))) (seen nil))
      (with-open-file (out file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (coerce '(255 0 0 255  0 255 0 255  0 0 255 255  255 255 255 128) '(vector (unsigned-byte 8))) out))
      (multiple-value-bind (f editor)
          (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                                  (list :until (lambda (buffer) (shown-pictures buffer)))
                                  (list :do (lambda (buffer) (setf seen (shown-pictures buffer)))))
                            :cell-pixels '(10 20) :program (picture-program file 32))
        (declare (ignore editor))
        (is = 1 (length seen))
        (destructuring-bind (image row column columns rows) (first seen)
          (is equal '(2 2 4) (list (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)
                                   (tuatara.frontend:image-channels image)))
          (is equal '(255 0 0 255) (coerce (subseq (tuatara.frontend:image-pixels image) 0 4) 'list))
          (is equal '(0 0 4 2) (list row column columns rows)))
        ;; and drawn by the frontend there
        (true (find-if (lambda (put) (and (= 4 (fourth put)) (= 2 (fifth put))))
                       (reduce #'append (tuatara.frontend.fake:fake-image-history f))))))))

(define-test (terminal pictures-in-text)
  ;; kitty's pictures in text (Unicode placeholders): picture 7 placed as 4 by 2 cells
  ;; written as U+10EEEE in its colour, each row's first cell marked with its row and
  ;; column, the rest following on; drawn over those cells, the characters shown as spaces
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "pixels" dir))) (seen nil) (text nil))
      (with-open-file (out file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (coerce '(255 0 0 255  0 255 0 255  0 0 255 255  255 255 255 255) '(vector (unsigned-byte 8))) out))
      (terminal-session
       (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
             (list :until (lambda (buffer) (shown-pictures buffer)))
             (list :do (lambda (buffer) (setf seen (shown-pictures buffer) text (tuatara.buffer:buffer-string buffer)))))
       :cell-pixels '(10 20)
       :program (list "sh" "-c"
                      (format nil "printf '\\033_Ga=T,U=1,i=7,f=32,s=2,v=2,c=4,r=2,q=2;'; base64 -w0 '~a'; printf '\\033\\\\'; ~
                                   p='\\364\\216\\273\\256'; printf \"\\033[38;5;7m$p\\314\\205\\314\\205$p$p$p\\033[39m|\\n\\033[38;5;7m$p\\314\\215\\314\\205$p$p$p\\033[39m|\\nready\\n\"; exec cat"
                              file)))
      (is = 1 (length seen) "pictures shown: ~s" (mapcar #'rest seen))
      (destructuring-bind (image row column columns rows) (first seen)
        (is equal '(0 0 4 2) (list row column columns rows))
        ;; drawn at its cells' size: 4 by 2 cells of 10 by 20 pixels
        (is equal '(40 40) (list (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)))
        ;; the 2 by 2 picture made as large as fits: red top left, blue bottom left
        (is equal '(255 0 0 255) (coerce (subseq (tuatara.frontend:image-pixels image) 0 4) 'list))
        (is equal '(0 0 255 255) (coerce (subseq (tuatara.frontend:image-pixels image) (* 4 (* 39 40)) (+ 4 (* 4 (* 39 40)))) 'list)))
      ;; the cells' characters shown as spaces
      (true (search (format nil "    |~%    |") text)))))

(define-test (terminal pictures-from-kitty-icat)
  ;; kitty's own icat, placing a picture where the cursor is and in text: shown both ways,
  ;; 7 by 4 cells for a 64 by 64 picture in cells of 10 by 20 pixels
  (if (not (and (tuatara.io:find-program "kitten") (eq :ready (tuatara.io:sandbox-status))))
      (skip "kitty's kitten or the sandbox is not here" (true nil))
      (dolist (flags '("" "--unicode-placeholder"))
        (let ((seen nil))
          (terminal-session
           (list "M-x t e r m i n a l RET" (list :until (buffer-holding "done-icat"))
                 (list :until (lambda (buffer) (shown-pictures buffer)))
                 (list :do (lambda (buffer) (setf seen (shown-pictures buffer)))))
           :cell-pixels '(10 20)
           :program (list "sh" "-c" (format nil "kitten icat --transfer-mode=stream --align left ~a '~a'; echo done-icat; exec cat"
                                            flags (namestring (merge-pathnames "data/icons/hicolor/64x64/apps/tuatara.png"
                                                                               (asdf:system-source-directory :tuatara))))))
          (is = 1 (length seen) "~a: ~s" flags (mapcar #'rest seen))
          (is equal '(7 4) (list (fourth (first seen)) (fifth (first seen))) "~a" flags)))))

(define-test (terminal png-picture-drawn-in-the-sandbox)
  ;; a PNG picture is drawn by the program the sandbox shuts in
  (if (not (eq :ready (tuatara.io:sandbox-status)))
      (skip "The sandbox is not ready here" (true nil))
      (with-test-directory (dir)
        (let ((file (namestring (merge-pathnames "picture.png" dir))) (seen nil))
          (with-open-file (out file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
            (write-sequence (tuatara.defaults::png-octets
                             (tuatara.frontend:make-image 3 2 3 (coerce '(255 0 0 0 255 0 0 0 255  9 9 9 9 9 9 9 9 9)
                                                                        '(simple-array (unsigned-byte 8) (*)))))
                            out))
          (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "ready"))
                                  (list :until (lambda (buffer) (shown-pictures buffer)))
                                  (list :do (lambda (buffer) (setf seen (shown-pictures buffer)))))
                            :cell-pixels '(10 20) :program (picture-program file 100))
          (is = 1 (length seen))
          (let ((image (first (first seen))))
            (is equal '(3 2) (list (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)))
            (is equal '(255 0 0) (coerce (subseq (tuatara.frontend:image-pixels image) 0 3) 'list)))))))

(defun selected-elsewhere (text)
  "A step of TERMINAL-SESSION: TEXT selected in another program, the primary selection."
  (list :do (lambda (buffer) (declare (ignore buffer))
              (setf (tuatara.frontend.fake:fake-primary (tuatara.command:editor-frontend tuatara.command:*editor*)) text))))

(define-test (terminal middle-button-pastes-into-the-program)
  ;; the middle button pastes what is selected anywhere, marked as pasted: run on RET
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (selected-elsewhere "echo from-primary")
                              (list :until (buffer-holding "$"))
                              '(:mouse :mouse-2 3 0) (list :until (buffer-holding "echo from-primary"))
                              "RET" (list :until (buffer-holding (format nil "~%from-primary")))))
    (declare (ignore f))
    (true (search (format nil "~%from-primary") (tuatara.buffer:buffer-string (session-buffer editor))))))

(define-test (terminal searched-from-typing)
  ;; C-s while typing: the history view, and a search back from the cursor once it is in
  (multiple-value-bind (f editor)
      (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "last-line"))
                              "C-s" (list :until (lambda (buffer) (declare (ignore buffer))
                                                   (tuatara.command:editor-minibuffer-active tuatara.command:*editor*)))
                              "w a n t e d RET")
                        :program '("sh" "-c" "echo first; echo the-wanted-one; echo other; echo last-line; exec cat"))
    (declare (ignore f))
    (let* ((buffer (session-buffer editor)) (point (tuatara.buffer:buffer-point buffer)))
      (false (tuatara.defaults::terminal-typing-p buffer))
      (true (search "the-wanted-one" (tuatara.buffer:buffer-string buffer
                                                                   :start (tuatara.buffer:buffer-line-start buffer point)
                                                                   :end (tuatara.buffer:buffer-line-end buffer point)))))))

(define-test (terminal killed-ends-the-shell)
  (terminal-session (list "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                          (list :until (lambda (b) (declare (ignore b))
                                         (= 1 (hash-table-count tuatara.defaults::*emulators*))))
                          "C-x k RET"))
  (is = 0 (hash-table-count tuatara.defaults::*emulators*)))

(define-test (terminal title-cut-to-columns)
  ;; a title of wide characters is cut to the columns it may take, not to as many characters
  (let ((b (tuatara.buffer:make-buffer "*terminal title test*")))
    (unwind-protect
         (let ((tuatara-user:*terminal-title-max* 5))
           (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::terminal-title) "漢字漢字")
           (is string= "漢字…" (tuatara.defaults::terminal-mode-line-title nil b))
           (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::terminal-title) "short")
           (is string= "short" (tuatara.defaults::terminal-mode-line-title nil b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (terminal font-size-repeat-keys)
  ;; right after C-x C-=, = makes the font larger again rather than going to the
  ;; shell; once another key has gone to the shell, = goes there too
  (multiple-value-bind (f editor)
      (terminal-session (list (list :do (lambda (buffer)
                                          (declare (ignore buffer))
                                          (setf (tuatara.frontend.fake:fake-font-steps
                                                 (tuatara.command::editor-frontend tuatara.command:*editor*))
                                                0)))
                              "M-x t e r m i n a l RET" (list :until (buffer-holding "$"))
                              "C-x C-= =" "e c h o SPC z q =" (list :until (buffer-holding "echo zq="))))
    (is = 2 (tuatara.frontend.fake:fake-font-steps f))
    (let ((text (tuatara.buffer:buffer-string (session-buffer editor))))
      (true (search "echo zq=" text))
      (false (search "=echo" text)))))

(define-test (terminal history-marks-read-in-one-pass)
  ;; a history whose prompts a shell marks (OSC 133, ended by ESC \) costs the same for
  ;; each more prompt, however long the history: the end of each mark is found where it is
  (flet ((history (prompts)
           (with-output-to-string (out)
             (dotimes (k prompts)
               (format out "~c]133;A~c\\$ ls~%a.txt  b.txt~%" #\Escape #\Escape))))
         (seconds (text)
           (let ((start (thread-cpu-time)))
             (tuatara.defaults::ansi-pieces text)
             (/ (- (thread-cpu-time) start) internal-time-units-per-second))))
    (is equal (list (format nil "$ ls~%a.txt  b.txt~%")) (tuatara.defaults::ansi-pieces (history 1)))
    (is equal (list (format nil "a~%b")) (tuatara.defaults::ansi-pieces (format nil "a~c]0;title~c~%b" #\Escape (code-char 7))))
    ;; about 0.2 µs a prompt; 50 µs when each mark's end was looked for to the end of the history
    (let* ((small (history 8000)) (large (history 32000))
           (each (/ (- (seconds large) (seconds small)) 24000)))
      (true (< each 0.000005) "~,2f µs for each more prompt" (* each 1000000)))))

(define-test (terminal one-character-lines-shown)
  ;; a row holding one character, or one before spaces, is shown, the spaces after it left out
  (let ((rows (vector "a   " "bb  " "c   " " x  " "y   " "    "))
        (looks (make-array 6 :initial-element '())))
    (is string= (format nil "a~%bb~%c~% x~%y~%")
        (tuatara.text:octets-string (tuatara.defaults::terminal-shown-text rows looks nil 0)))
    ;; the spaces before the cursor kept
    (is string= (format nil "a~%bb~%c~% x~%y~%  ")
        (tuatara.text:octets-string (tuatara.defaults::terminal-shown-text rows looks 5 2)))))

(define-test (terminal refresh-makes-changed-rows-only)
  ;; a screen of 200 by 50 in colours with addresses, one row changing: the other rows' text,
  ;; addresses and faces are not made again, as they were for every refresh
  (true (tuatara.defaults::terminal-library-p))
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "screen.txt" dir))))
      (with-open-file (out file :direction :output :if-exists :supersede)
        (dotimes (row 49)
          (dotimes (word 12)
            (format out "~c[38;5;~dmword~2,'0d~c[0m " #\Escape (+ row word) word #\Escape))
          (format out "see https://example.org/~d and more text to the row's end~%" row)))
      (let ((e (tuatara.defaults::start-emulator "sh" (list "-c" (format nil "cat '~a'; exec cat" file)) :columns 200 :rows 50))
            (buffer (tuatara.buffer:make-buffer "*terminal refresh test*")))
        (unwind-protect
             (let ((deadline (tuatara.runtime:deadline-in 10)))
               (loop until (or (search "example.org/48" (tuatara.buffer:buffer-string buffer))
                               (<= (tuatara.runtime:seconds-left deadline) 0))
                     do (tuatara.defaults::terminal-refresh buffer e :all t)
                        (sleep 1/50))
               (true (search "example.org/48" (tuatara.buffer:buffer-string buffer)))
               (flet ((typed (all)
                        ;; a character typed, echoed on the cursor's row, then the screen refreshed
                        (tuatara.defaults::emulator-input e "x")
                        (let ((deadline (tuatara.runtime:deadline-in 5)))
                          (loop until (or (tuatara.defaults::emulator-changed e) (<= (tuatara.runtime:seconds-left deadline) 0))
                                do (sleep 1/100)))
                        (let ((before (sb-ext:get-bytes-consed)))
                          (tuatara.defaults::terminal-refresh buffer e :all all)
                          (- (sb-ext:get-bytes-consed) before))))
                 (let ((every (loop repeat 3 minimize (typed t)))
                       (one (loop repeat 3 minimize (typed nil))))
                   (true (< (* 4 one) every) "~:d octets for one row changed, ~:d for the whole screen" one every)
                   (true (search "xxxxxx" (tuatara.buffer:buffer-string buffer))))))
          (tuatara.defaults::stop-emulator e)
          (tuatara.buffer:kill-buffer buffer))))))
