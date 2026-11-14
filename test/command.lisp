;;;; test/command.lisp --- The command loop driven through the fake frontend.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test command
  :description "Keys through the fake frontend: modal states, editing commands, kill ring, undo, minibuffer, files, screen contents.")

;; The tests read the colours of rows exactly; the line point is on is
;; highlighted (*global-hl-line-mode*, on for users) only in the tests about it.
(setf tuatara-user:*global-hl-line-mode* nil)
;; Files open at their top in the tests, wherever an earlier test left them;
;; the places test (test/places.lisp) turns remembering on for itself.
(setf tuatara-user:*save-place-mode* nil)
;; Notes logged right under a task's planning line, as the tests were written (on for users: into
;; :LOGBOOK:); the tests of drawers bind it for themselves.
(setf tuatara-user:*org-log-into-drawer* nil)
;; Org files open with their tasks' drawers and notes shown, the lines the tests move over as
;; written; the test of folding them (org metadata-folded-on-opening) turns it on for itself.
(setf tuatara-user:*org-fold-metadata* nil)
;; Dates typed at the prompt at once, as the tests were written; the tests of choosing them in the
;; calendar (org dates-chosen-in-the-calendar) turn it on for themselves.
(setf tuatara-user:*org-read-date-in-calendar* nil)

(defun wait-for-minibuffer-source ()
  "Wait, on the editor's thread, until the question open has taken in all its
candidates found in the background (a completing read's SOURCE)."
  (loop while (tuatara.command::take-in-source tuatara.command:*editor*)
        do (sleep 0.01)))

(defvar *buffers-before-test* '()
  "The buffers there were when the test running began: those it did not make.")

(defun test-buffers ()
  "The buffers the test running made, the newest first."
  (remove-if (lambda (b) (member b *buffers-before-test*)) tuatara.buffer:*buffers*))

(defparameter *buffers-kept-between-tests* '("*Messages*" "*scratch*")
  "The buffers a test may leave for the next: made again when missing, never
looked for as made by a test.")

(defmethod parachute:eval-in-context :around ((report parachute:report) (test parachute:test))
  "Run TEST, then kill every buffer it made and left (but those of
*BUFFERS-KEPT-BETWEEN-TESTS*): a test starts with the buffers there were
before the test before it, and finds its own (TEST-BUFFERS)."
  (let ((*buffers-before-test* (copy-list tuatara.buffer:*buffers*)))
    (unwind-protect (call-next-method)
      (dolist (buffer (copy-list tuatara.buffer:*buffers*))
        (unless (or (member buffer *buffers-before-test*)
                    (member (tuatara.buffer:buffer-name buffer) *buffers-kept-between-tests* :test #'string=))
          (ignore-errors (tuatara.buffer:kill-buffer buffer)))))))

(defun waited-in-vain (until seconds)
  "Fail the test running: a session waited SECONDS for UNTIL, a function, to
be true, and it never was. What the editor asked and said then is told."
  (let ((editor tuatara.command:*editor*))
    (true nil "Waited ~a s in vain for ~a; asking ~s, saying ~s" seconds until
          (and editor
               (tuatara.command:editor-minibuffer-active editor)
               (tuatara.command:editor-minibuffer-prompt editor))
          (and editor (tuatara.command:editor-message editor)))))

(defun queue-test-keys (f keys)
  "Queue KEYS on the fake frontend F: a key sequence as a string, or a list of
them, :WAIT among them for WAIT-FOR-MINIBUFFER-SOURCE at that point, :TICK
for the editor woken there once, a function called there."
  (if (listp keys)
      (dolist (part keys)
        (case part
          (:wait (tuatara.frontend.fake:fake-queue-action f #'wait-for-minibuffer-source))
          (:tick (tuatara.frontend.fake:fake-queue-action f (lambda () nil)))
          (t (if (functionp part)
                 (tuatara.frontend.fake:fake-queue-action f part)
                 (tuatara.frontend.fake:fake-queue-keys f part)))))
      (tuatara.frontend.fake:fake-queue-keys f keys)))

(defmacro with-messages-said ((said) &body body)
  "Run BODY with *Messages* keeping every line said, and SAID, a function of
none, giving the text said since BODY began; a log that keeps only its last
lines would move the text under an offset taken before."
  (let ((log (gensym "LOG")) (start (gensym "START")))
    `(let* ((tuatara.command:*message-log-max* most-positive-fixnum)
            (,log (tuatara.command::messages-buffer))
            (,start (tuatara.buffer:buffer-size ,log)))
       (flet ((,said () (tuatara.buffer:buffer-string ,log :start (min ,start (tuatara.buffer:buffer-size ,log)))))
         ,@body))))

(defmacro counting-calls ((function &key (stub nil stubbed)) &body body)
  "Run BODY with calls of FUNCTION counted, the count following BODY's value as
a second value. With STUB, a call gives that value and does nothing else."
  (let ((calls (gensym "CALLS")))
    `(let ((,calls 0))
       (sb-int:encapsulate ',function 'counting-calls
                           (lambda (original &rest arguments)
                             (incf ,calls)
                             ,(if stubbed `(progn original arguments ,stub) '(apply original arguments))))
       (values (unwind-protect (progn ,@body) (sb-int:unencapsulate ',function 'counting-calls)) ,calls))))

(defvar *grammar-questions-wanted* nil
  "True while a test looks at the question whether to turn a grammar on
\(WITH-GRAMMAR-ANSWERS-FORGOTTEN); otherwise a session asks none, as a key typed
meanwhile would answer it.")

(defun call-without-grammar-questions (function)
  "Call FUNCTION, a session in it asked no grammar question (every language
given up for this session), unless a test wants it asked."
  (if *grammar-questions-wanted*
      (funcall function)
      (let ((tuatara.defaults::*grammars-not-now*
              (mapcar (lambda (language) (string-downcase (symbol-name (tuatara.defaults::language-name language))))
                      tuatara.defaults::*languages*)))
        (funcall function))))

(defmacro without-grammar-questions (&body body)
  "Run BODY with no grammar question asked in it (CALL-WITHOUT-GRAMMAR-QUESTIONS)."
  `(call-without-grammar-questions (lambda () ,@body)))

(defun row-showing (frontend text)
  "The first row of the last frame FRONTEND, a fake screen, painted that shows TEXT, or NIL."
  (position-if (lambda (row) (search text row)) (tuatara.frontend.fake:fake-screen frontend)))

(defun faces-at (frontend row column)
  "The faces the last frame FRONTEND, a fake screen, painted over COLUMN of ROW, a list."
  (loop for (start end face) in (aref (tuatara.frontend.fake:fake-spans frontend) row)
        when (and (<= start column) (< column end))
          append (if (listp face) face (list face))))

(defun run-keys (keys &key (text "") (width 40) (height 8) file cell-pixels)
  "Run an editor session on a scratch buffer holding TEXT (or FILE) with KEYS
typed (a string, or a list of strings and :WAIT, QUEUE-TEST-KEYS); return the
frontend and the editor. CELL-PIXELS, a cell's width and height in pixels as
a list, has the frontend show pictures."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (unless file (tuatara.buffer:make-buffer "*scratch*" :text text))
    ;; each session starts afresh: no command history, no search lit from one before
    (setf tuatara.defaults::*command-history* '() tuatara.defaults::*pattern-history* '()
          tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
    (queue-test-keys f keys)
    (let ((editor (without-grammar-questions
                    (tuatara.command:run-editor f :files (and file (list file))
                                                  :setup #'tuatara.defaults:install-defaults
                                                  :buffer (tuatara.buffer:find-buffer "*scratch*")))))
      (values f editor))))

(defparameter *colouring-functions* '(tuatara.command::window-highlights-for tuatara.display:apply-highlights)
  "The functions that work out and paint the colours of a window's text.")

(defun typing-garbage-per-key (text type height measured &key less)
  "The octets the functions named in MEASURED make, less those the functions
named in LESS make, for each key typed in a window HEIGHT rows tall on a file
of TYPE holding TEXT, its colours worked out afresh after each key; and how
many times its colours were worked out while those keys were typed, as a
second value."
  (let ((path (records-file text :type type))
        (keys "a b c d DEL DEL DEL DEL a b c d DEL DEL DEL DEL")
        (counting nil) (octets 0) (worked-out 0) (buffer nil))
    (flet ((count-octets (names sign)
             (dolist (name names)
               (sb-int:encapsulate name 'garbage-per-key
                                   (lambda (function &rest arguments)
                                     (if counting
                                         (let ((before (tuatara.bench::bytes-consed)))
                                           (multiple-value-prog1 (apply function arguments)
                                             (incf octets (* sign (- (tuatara.bench::bytes-consed) before)))))
                                         (apply function arguments)))))))
      (count-octets measured 1)
      (count-octets less -1))
    (sb-int:encapsulate 'tuatara.syntax:rich-highlights 'garbage-per-key
                        (lambda (function &rest arguments)
                          (when counting (incf worked-out))
                          (apply function arguments)))
    (unwind-protect
         ;; each key's colours from the text parsed after it
         (let ((tuatara.syntax:*syntax-wait* 30) (tuatara.defaults::*rules-wait* 30))
           (run-keys (list (lambda ()
                             (setf buffer (tuatara.command:current-buffer))
                             ;; its grammar loaded first, as it is a moment after a start
                             (loop repeat 600
                                   for language = (tuatara.buffer:buffer-local buffer :syntax-pending)
                                   until (or (null language) (tuatara.syntax:buffer-syntax buffer))
                                   do (tuatara.defaults::start-parsing buffer language)
                                      (sleep 0.05)))
                           "C-v i" (lambda () (setf counting t)) keys (lambda () (setf counting nil)))
                     :file path :width 100 :height height))
      (dolist (name (list* 'tuatara.syntax:rich-highlights (append measured less)))
        (sb-int:unencapsulate name 'garbage-per-key))
      (kill-records buffer path))
    (values (/ octets (1+ (count #\Space keys))) worked-out)))

(defmacro with-functions-replaced ((&rest bindings) &body body)
  "Run BODY with each (NAME FUNCTION) of BINDINGS standing in for the global
function NAME, which is put back afterwards."
  (let ((saved (loop repeat (length bindings) collect (gensym "SAVED"))))
    `(let ,(loop for (name) in bindings for s in saved collect `(,s (fdefinition ',name)))
       (unwind-protect
            (progn ,@(loop for (name function) in bindings collect `(setf (fdefinition ',name) ,function))
                   ,@body)
         ,@(loop for (name) in bindings for s in saved collect `(setf (fdefinition ',name) ,s))))))

(defclass answering-host (tuatara.defaults::defaults-host)
  ((answers :initarg :answers :reader answering-host-answers))
  (:documentation "The defaults' host with some of its answers given by a test: a plist of a
question's name, a HOST-... generic function, and the function answering it in
its place, given the question's arguments after the host."))

(defmacro define-test-answer (question &rest parameters)
  "Have an ANSWERING-HOST answer QUESTION, of PARAMETERS after the host (a
method's lambda list), with the function the test gave for it, else as the
defaults do."
  (let ((call (loop with keys = nil
                    for parameter in parameters
                    do (when (member parameter '(&optional &key)) (setf keys (eq parameter '&key)))
                    unless (member parameter lambda-list-keywords)
                      append (if keys (list (intern (symbol-name parameter) :keyword) parameter) (list parameter)))))
    `(defmethod ,question ((host answering-host) ,@parameters)
       (let ((answer (getf (answering-host-answers host) ',question)))
         (if answer (funcall answer ,@call) (call-next-method))))))

(define-test-answer tuatara.runtime:host-wake)
(define-test-answer tuatara.runtime:host-say-hook-failure text)
(define-test-answer tuatara.runtime:host-secret-buffer-p buffer)
(define-test-answer tuatara.io:host-writing-refusal path &key dirfd fd)
(define-test-answer tuatara.io:host-say-writing words)
(define-test-answer tuatara.io:host-save-transform buffer path)
(define-test-answer tuatara.display:host-tab-width buffer)
(define-test-answer tuatara.syntax:host-trusted-text-p buffer)
(define-test-answer tuatara.syntax:host-say-helper-refused trusted)
(define-test-answer tuatara.syntax:host-say-helper-ended helper)
(define-test-answer tuatara.syntax:host-must-not-wait-p)
(define-test-answer tuatara.syntax:host-buffer-shown-p buffer)
(define-test-answer tuatara.syntax:host-say-syntax-fault syntax)
(define-test-answer tuatara.syntax:host-say-syntax-slow syntax)

(defun make-answering-host (&rest answers)
  "A host answering each question of ANSWERS, a HOST-... generic function's
name followed by the function answering it in its place (DEFINE-TEST-ANSWER),
and the others as the defaults do: bound to TUATARA.RUNTIME:*HOST* for a test."
  (make-instance 'answering-host :answers answers))

(defun this-thread-only ()
  "A function of no arguments true on the thread that made it only, as the
editor's thread is for a test that has no editor running."
  (let ((thread sb-thread:*current-thread*))
    (lambda () (eq sb-thread:*current-thread* thread))))

(defun processing-shown (frontend &rest texts)
  "True when some frame FRONTEND painted had a row holding every string in
TEXTS with the mode line's sign of work, P, lit in :mode-line-sign."
  (loop for screen in (tuatara.frontend.fake:fake-history frontend)
        for (spans) in (tuatara.frontend.fake:fake-face-history frontend)
          thereis (loop for row across screen
                        for row-spans across spans
                          thereis (and (every (lambda (text) (search text row)) texts)
                                       (loop for (start end face) in row-spans
                                               thereis (and (member :mode-line-sign (if (listp face) face (list face)))
                                                            (string= "P" row :start2 start :end2 end)))))))

(defun session-text (editor)
  "The text of the editor's current buffer."
  (tuatara.buffer:buffer-string (tuatara.display:window-buffer (tuatara.command:editor-window editor))))

(defun session-point (editor)
  "Point of the editor's current buffer."
  (tuatara.buffer:buffer-point (tuatara.display:window-buffer (tuatara.command:editor-window editor))))

(defun session-buffer (editor)
  "The buffer of the editor's selected window."
  (tuatara.display:window-buffer (tuatara.command:editor-window editor)))

(define-modal-test (command insert-and-normal)
  (multiple-value-bind (f editor) (run-keys "i h i SPC t h e r e C-g h h d")
    (is string= "hi thee" (session-text editor))
    (is = 6 (session-point editor))
    (is eq :block (tuatara.frontend.fake:fake-cursor-shape f))
    (is string= "hi thee" (tuatara.frontend.fake:fake-screen-line f 0))
    (true (search "TUATARA[N]" (tuatara.frontend.fake:fake-screen-line f 6)))
    (true (search "[*]" (tuatara.frontend.fake:fake-screen-line f 6)))
    ;; letters in NORMAL never insert
    (multiple-value-bind (f2 e2) (run-keys "q z" :text "abc")
      (declare (ignore f2))
      (is string= "abc" (session-text e2)))))

(define-modal-test (command repeat-with-its-selection)
  ;; . repeats a change with the selection it took, made again as large from point:
  ;; whole lines, characters, a text object as many times, a rectangle
  (flet ((after (keys text) (substitute #\| #\Newline (session-text (nth-value 1 (run-keys keys :text text))))))
    (let ((lines (format nil "one~%two~%three~%four~%five~%six~%")))
      (is string= "three|four|five|six|" (after "V d ." lines))
      (is string= "five|six|" (after "V J d ." lines))
      (is string= "two|three|four|five|six|" (after "v l l d ." lines))
      (is string= "||five|six|" (after "m w m w d ." lines))
      ;; only the change right before: a move after it is what . repeats
      (is string= "three|four|five|six|" (after "V J d j ." lines)))
    (is string= "cdef|ef|cdef|abcdef|" (after "m v j l d ." (format nil "abcdef~%abcdef~%abcdef~%abcdef~%")))))

(define-test (command text-objects-follow-normal-keys)
  ;; the text objects . makes again are the SELECT- commands NORMAL state binds under m, as its keymap
  ;; stands: a key added there counts, and stops counting when it goes
  (flet ((counted-p (command) (member command (tuatara.defaults::selection-object-commands))))
    (true (counted-p 'tuatara.defaults::select-word))
    (false (counted-p 'tuatara.defaults::select-next-sibling))
    (unwind-protect
         (progn (tuatara.command:define-key tuatara.defaults::*tuatara-normal-keymap* "m z"
                  'tuatara.defaults::select-next-sibling)
                (true (counted-p 'tuatara.defaults::select-next-sibling)))
      (tuatara.command:define-key tuatara.defaults::*tuatara-normal-keymap* "m z" nil))
    (false (counted-p 'tuatara.defaults::select-next-sibling))))

(define-modal-test (command insert-undoes-at-once)
  ;; one INSERT session is one step to undo, line breaks and all, with what the
  ;; key that began it did: c's deletion, o's new line
  (flet ((after (keys) (session-text (nth-value 1 (run-keys keys :text (format nil "The tui sings.~%"))))))
    (is string= (format nil "The tui sings.~%") (after "g e i F i r s t RET S e c o n d C-g u"))
    (is string= (format nil "The tui sings.~%") (after "w v w c b e l l b i r d C-g u"))
    (is string= (format nil "The tui sings.~%") (after "o n e w C-g u"))
    ;; two sessions, two steps
    (is string= (format nil "aThe tui sings.~%") (after "i a C-g i b C-g u"))))

(define-modal-test (command undo-redo-kill-yank)
  (multiple-value-bind (f editor) (run-keys "i a b c C-g u")
    (declare (ignore f))
    (is string= "" (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "i a b c C-g u U")
    (declare (ignore f))
    (is string= "abc" (session-text editor)))
  ;; select two chars with v, delete them (killed), paste at end
  (multiple-value-bind (f editor) (run-keys "v l l d g e p" :text "abcd")
    (declare (ignore f))
    (is string= "cdab" (session-text editor)))
  ;; x without a selection deletes but does not kill: p still pastes the earlier kill "a"
  (multiple-value-bind (f editor) (run-keys "v l d d p" :text "abcd")
    (declare (ignore f))
    (is string= "acd" (session-text editor)))
  ;; y without a selection copies the character at point
  (multiple-value-bind (f editor) (run-keys "y g e p" :text "abc")
    (declare (ignore f))
    (is string= "abca" (session-text editor)))
  ;; y copies the character at point; p replaces the active selection "ab" with it
  (multiple-value-bind (f editor) (run-keys "y v l l p" :text "abcd")
    (declare (ignore f))
    (is string= "acd" (session-text editor)))
  ;; C-u 3 x deletes three characters
  (multiple-value-bind (f editor) (run-keys "C-u 3 d" :text "abcdef")
    (declare (ignore f))
    (is string= "def" (session-text editor)))
  ;; C-k C-k joins two kills; C-y yanks both
  (multiple-value-bind (f editor) (run-keys "C-k C-k g e C-y" :text (format nil "one~%two"))
    (declare (ignore f))
    (is string= (format nil "twoone~%") (session-text editor)))
  ;; the ring keeps the *KILL-RING-MAX* newest kills
  (let ((tuatara-user:*kill-ring-max* 3))
    (multiple-value-bind (f editor) (run-keys "C-k C-n C-k C-n C-k C-n C-k C-n C-k" :text (format nil "a~%b~%c~%d~%e"))
      (declare (ignore f))
      (is equal '("e" "d" "c") (tuatara.command:editor-kill-ring editor)))))

(define-modal-test (command inserting-drops-selection)
  ;; every key that enters INSERT state drops the selection, a rectangle too: V i then
  ;; RET leaves nothing selected
  (flet ((region-shown-p (f)
           (loop for row across (tuatara.frontend.fake:fake-spans f)
                 thereis (find-if (lambda (span)
                                    (let ((face (third span)))
                                      (if (listp face) (member :region face) (eq face :region))))
                                  row))))
    (dolist (keys '("V i" "V i RET" "V j a" "v l I" "v l A" "V o" "V O" "m v j i"))
      (multiple-value-bind (f editor) (run-keys keys :text (format nil "abc~%def~%ghi"))
        (false (tuatara.buffer:buffer-mark-active-p (session-buffer editor)) "~a leaves a selection" keys)
        (false (region-shown-p f) "~a shows a selection" keys)
        (true (tuatara.frontend.fake:fake-frame-showing f "TUATARA[I]") "~a does not insert" keys))))
  ;; a moves one character on from point, the selection gone first rather than stretched:
  ;; V leaves point at the end of its line, so a types on the next
  (multiple-value-bind (f editor) (run-keys "V a X C-g" :text (format nil "abc~%def"))
    (declare (ignore f))
    (is string= (format nil "abc~%Xdef") (session-text editor))))

(define-modal-test (command delete-and-change-alike)
  (flet ((text (keys text) (session-text (nth-value 1 (run-keys keys :text text)))))
    ;; without a selection, COUNT characters at point
    (is string= "def" (text "C-u 3 d" "abcdef"))
    (is string= "Zdef" (text "C-u 3 c Z C-g" "abcdef"))
    ;; a selection is killed, whole lines for d, the highlighted text for c
    (is string= (format nil "twoone~%") (text "V d g e p" (format nil "one~%two")))
    (is string= (format nil "~%twoone") (text "V c C-g g e p" (format nil "one~%two")))))

(define-modal-test (command visual-lines)
  ;; V d removes whole lines including the newline
  (multiple-value-bind (f editor) (run-keys "j V J d" :text (format nil "one~%two~%three~%four"))
    (declare (ignore f))
    (is string= (format nil "one~%four") (session-text editor)))
  ;; V y then p pastes back as complete lines
  (multiple-value-bind (f editor) (run-keys "V y g e p" :text (format nil "one~%two"))
    (declare (ignore f))
    (is string= (format nil "one~%twoone~%") (session-text editor)))
  ;; V c keeps the newline and enters INSERT on the emptied line
  (multiple-value-bind (f editor) (run-keys "V c x C-g" :text (format nil "one~%two"))
    (declare (ignore f))
    (is string= (format nil "x~%two") (session-text editor)))
  ;; V toggles off; point stays where V left it (the line end), so go back first
  (multiple-value-bind (f editor) (run-keys "V V g h d" :text "abc")
    (declare (ignore f))
    (is string= "bc" (session-text editor)))
  ;; g j joins the next line up; with a selection, joins the touched lines
  (multiple-value-bind (f editor) (run-keys "g j" :text (format nil "one~%  two~%three"))
    (declare (ignore f))
    (is string= (format nil "one two~%three") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "V J J g j" :text (format nil "a~%b~%c~%d"))
    (declare (ignore f))
    (is string= (format nil "a b c~%d") (session-text editor))))

(define-modal-test (command jump-back-and-repeat)
  ;; G records the position before the jump; S returns there
  (multiple-value-bind (f editor) (run-keys "l l g e S" :text (format nil "abc~%def"))
    (declare (ignore f))
    (is = 2 (session-point editor)))
  ;; . repeats the previous command
  (multiple-value-bind (f editor) (run-keys "d ." :text "abcd")
    (declare (ignore f))
    (is string= "cd" (session-text editor)))
  ;; W moves over a bracketed group, % selects everything
  (multiple-value-bind (f editor) (run-keys "W" :text "(a (b) c) d")
    (declare (ignore f))
    (is = 9 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "% d" :text "everything")
    (declare (ignore f))
    (is string= "" (session-text editor))))

(define-modal-test (command minibuffer-and-files)
  (let ((path (merge-pathnames (format nil "tuatara-cmd-~a.txt" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           ;; find-file through the minibuffer, type, save
           (multiple-value-bind (f editor)
               (run-keys (format nil "C-x C-f ~{~a ~}RET i s a v e d C-g C-x C-s"
                                 (map 'list (lambda (c) (if (char= c #\/) "/" (string c))) (namestring path))))
             (declare (ignore f))
             (is string= "saved" (session-text editor))
             (is string= "saved" (sb-ext:octets-to-string (alexandria:read-file-into-byte-vector path))))
           ;; M-x runs a command by name; goto-line reads a number
           (multiple-value-bind (f editor) (run-keys "M-x e n d - o f - b u f f e r RET" :text "abc")
             (declare (ignore f))
             (is = 3 (session-point editor)))
           (multiple-value-bind (f editor) (run-keys "M-g g 2 RET" :text (format nil "a~%b~%c"))
             (declare (ignore f))
             (is = 2 (session-point editor)))
           ;; C-g in the minibuffer aborts
           (multiple-value-bind (f editor) (run-keys "M-x f o o C-g i z C-g" :text "")
             (declare (ignore f))
             (is string= "z" (session-text editor))))
      (ignore-errors (delete-file path))
      (let ((b (find path tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-file :test #'equal)))
        (when b (tuatara.buffer:kill-buffer b))))))

(define-modal-test (command hooks-cannot-end-the-editor)
  ;; an error or C-g in what runs after each command is said, and the editor carries on with the keys after it
  (dolist (trouble (list (lambda (editor) (declare (ignore editor)) (error "boom"))
                         (lambda (editor) (declare (ignore editor)) (tuatara.command:keyboard-quit-signal))))
    (let ((f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 8))
          (once nil))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
        (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text "")
      (tuatara.frontend.fake:fake-queue-keys f "i a b c")
      (let ((editor (tuatara.command:run-editor
                     f :setup (lambda (editor)
                                (tuatara.defaults:install-defaults editor)
                                (push (lambda (editor) (unless once (setf once t) (funcall trouble editor)))
                                      (tuatara.command:editor-post-command-functions editor)))
                       :buffer (tuatara.buffer:find-buffer "*scratch*"))))
        (is string= "abc" (session-text editor))
        (true (or (tuatara.frontend.fake:fake-frame-showing f "boom")
                  (tuatara.frontend.fake:fake-frame-showing f "Quit")))))))

(define-test (command defaults-installed-twice-run-once)
  ;; installing the defaults again leaves one of each thing run after a command
  (multiple-value-bind (f editor) (run-keys "" :text "")
    (declare (ignore f))
    (let ((tuatara.command:*editor* editor))
      (tuatara.defaults:install-defaults editor))
    (let ((after (tuatara.command:editor-post-command-functions editor)))
      (is equal after (remove-duplicates after :test #'equal :from-end t)))))

(define-test (command offered-commands-without-refusals)
  ;; M-x leaves out the commands made for other buffers without writing what it would say of each
  (let ((said 0) (offered nil))
    (multiple-value-bind (f editor) (run-keys "" :text "")
      (declare (ignore f))
      (let ((tuatara.command:*editor* editor))
        (sb-int:encapsulate 'tuatara.defaults::mode-command-refusal 'counted
                            (lambda (function &rest arguments) (incf said) (apply function arguments)))
        (unwind-protect (setf offered (tuatara.defaults::command-candidates))
          (sb-int:unencapsulate 'tuatara.defaults::mode-command-refusal 'counted))
        (true (tuatara.defaults::mode-command-refusal 'tuatara.git::git-stage))
        (false (find 'tuatara.git::git-stage offered :key #'tuatara.command:candidate-result))
        (true (find 'tuatara.defaults::find-file offered :key #'tuatara.command:candidate-result))))
    (is = 0 said)))

(define-test (command key-only-commands-not-offered)
  ;; commands made for keys alone are not offered by M-x, but a key still runs them
  (multiple-value-bind (f editor) (run-keys "" :text "")
    (declare (ignore f))
    (let* ((tuatara.command:*editor* editor)
           (offered (mapcar #'tuatara.command:candidate-result (tuatara.defaults::command-candidates)))
           (everywhere (mapcar #'tuatara.command:candidate-result
                               (tuatara.defaults::command-candidates :everywhere t))))
      (dolist (name (mapcar #'tuatara.command:command-symbol
                            '("minibuffer-finish" "popup-next" "mark-run-refuse" "tuatara-normal-inert")))
        (true (tuatara.command:key-only-command-p name) "~a" name)
        (false (member name offered) "~a" name)
        (true (member name everywhere) "~a" name)
        (true (tuatara.command:find-command name) "~a" name))
      (false (tuatara.command:key-only-command-p (tuatara.command:command-symbol "find-file")))
      (true (member (tuatara.command:command-symbol "find-file") offered)))))

(tuatara.command:define-command test-placed-nowhere ()
  "Do nothing, for the tests: a command made for places that are not there."
  nil)

(defun test-failing-place (buffer)
  "Fail, for the tests: a place of commands whose test fails for BUFFER."
  (declare (ignore buffer))
  (error "The place could not be told."))

(define-test (command broken-places-contained)
  ;; a command made for a mode that is gone (its name left in no Lisp package), for a test
  ;; that is not there or for one that fails runs nowhere, each said once in the debug log,
  ;; and M-x still offers every other command
  (let ((places (list (make-symbol "GONE-MODE") 'test-no-such-place 'test-failing-place)))
    (dolist (place places)
      (tuatara.defaults::commands-for-mode place "a place that is not there" 'test-placed-nowhere))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys "" :text "")
           (declare (ignore f))
           (loop while (sb-concurrency:dequeue tuatara.runtime:*debug-reports*))
           (let ((tuatara.command:*editor* editor)
                 (tuatara.runtime:*debug-on* t))
             (dotimes (i 2)
               (let ((offered (mapcar #'tuatara.command:candidate-result (tuatara.defaults::command-candidates))))
                 (true (member 'tuatara.defaults::find-file offered))
                 (false (member 'test-placed-nowhere offered))))
             (is eq :elsewhere (tuatara.defaults::command-refusal-reason 'test-placed-nowhere))
             (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                                while report collect (princ-to-string (fourth report)))))
               (dolist (name '("gone-mode failed" "test-no-such-place failed" "test-failing-place failed"))
                 (is = 1 (count-if (lambda (text) (search name text)) texts) "~a in ~s" name texts)))))
      (remhash 'test-placed-nowhere tuatara.defaults::*mode-commands*)
      (dolist (place places)
        (remhash place tuatara.defaults::*broken-command-places*)))))

(define-modal-test (command odd-places)
  ;; the minibuffer's commands outside it say where they work; . repeats a command run with M-x
  (let ((f (run-keys "M-x m i n i b u f f e r - k i l l - l i n e RET" :text "abc" :width 120 :height 8)))
    (false (tuatara.frontend.fake:fake-frame-showing f "bounding indices"))
    (false (tuatara.frontend.fake:fake-frame-showing f "is not of type")))
  (multiple-value-bind (f editor) (run-keys "M-x u p c a s e - w o r d RET ." :text "one two" :width 80 :height 8)
    (declare (ignore f))
    (is string= "ONE TWO" (session-text editor))))

(define-test (command quitting-asks-then-acts)
  ;; the questions before exiting change nothing: what they stop is stopped only once the exit is certain
  (let* ((job (tuatara.defaults::%make-hash-job :what "a sum"))
         (tuatara.runtime:*jobs* (list job)))
    (sb-int:encapsulate 'tuatara.command:y-or-n-p* 'test (lambda (f prompt &rest how) (declare (ignore f prompt how)) t))
    (unwind-protect
         (let ((answer (tuatara.defaults::jobs-quit-query)))
           (true (functionp answer))
           (false (car (tuatara.runtime:job-cancel job)))
           (funcall answer)
           (true (car (tuatara.runtime:job-cancel job))))
      (sb-int:unencapsulate 'tuatara.command:y-or-n-p* 'test))))

(defvar *yes-or-no-answer* :none
  "What the question of the yes-or-no test answered.")

(define-test (command yes-or-no-answers)
  ;; y or yes, in any case and with spaces around, is yes; n or no is no; anything else, an
  ;; empty answer too, says which answers it takes and asks again
  (let ((ask (key-string "(setf tuatara.tests::*yes-or-no-answer* (tuatara.command:yes-or-no-p* \"Go on?\"))")))
    (dolist (case '(("y" t) ("SPC Y e s SPC" t) ("n" nil) ("SPC N o SPC" nil)
                    ("RET n" nil) ("y i k e s RET y" t) ("y y RET n o" nil)))
      (setf *yes-or-no-answer* :none)
      (let ((f (run-keys (format nil "M-: ~a RET ~a RET" ask (first case)) :width 120)))
        (is eq (second case) *yes-or-no-answer* "~s" (first case))
        (is eq (and (search "RET " (first case)) t)
            (and (tuatara.frontend.fake:fake-frame-showing f "Answer y or n. Go on?") t)
            "~s" (first case))))))

(define-test (command kill-ring-bounded)
  ;; the kills together held to *kill-ring-max-size*: the oldest forgotten, the latest always kept
  (let ((tuatara.defaults::*kill-ring-max-size* 10))
    (multiple-value-bind (f editor)
        (run-keys "M-x k i l l - l i n e RET M-x k i l l - l i n e RET M-x k i l l - l i n e RET"
                  :text (format nil "abcdef~%ghijkl~%") :width 80)
      (declare (ignore f))
      (true (<= (reduce #'+ (tuatara.command:editor-kill-ring editor) :key #'length) 13))
      (true (plusp (length (tuatara.command:editor-kill-ring editor)))))))

(define-test (command one-command-one-undo)
  ;; what one command changed is taken back by one undo, however many changes it made
  (let ((tuatara-user:*modal-editing* nil))
    (is string= (format nil "abc~%  def~%")
        (session-text (nth-value 1 (run-keys "C-n M-^ C-/" :text (format nil "abc~%  def~%")))))
    (is string= (format nil "a b c")
        (session-text (nth-value 1 (run-keys "C-x h M-x d e l e t e - t r a i l i n g - w h i t e s p a c e RET C-/"
                                             :text (format nil "a b c")))))))

(define-modal-test (command mode-line-groups-digits)
  (let ((text (with-output-to-string (o) (dotimes (i 1234) (format o "x~%")))))
    ;; G to the last line: 1,235 in the mode line; goto-line accepts grouped digits
    (multiple-value-bind (f editor) (run-keys "g e" :text text)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "L1,235 C0")))
    (multiple-value-bind (f editor) (run-keys "M-g g 1 , 0 0 0 RET" :text text)
      (declare (ignore f))
      (is = 999
          (tuatara.buffer:buffer-line-number (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                             (session-point editor))))
    (multiple-value-bind (f editor) (run-keys "M-g g 1 _ 0 0 1 RET" :text text)
      (declare (ignore f))
      (is = 1000
          (tuatara.buffer:buffer-line-number (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                             (session-point editor))))
    ;; the minibuffer's line keys: C-a and C-e to its start and end, C-u clears it, C-<backspace> and M-DEL delete a word back
    (loop for (keys line) in '(("M-g g 2 C-a 1 C-e 0 RET" 120) ("M-g g 9 9 C-u 3 RET" 3)
                               ("M-g g 1 2 . 3 4 C-<backspace> RET" 12) ("M-g g 1 2 . 3 4 M-DEL RET" 12)
                               ("M-g g 1 2 . 3 4 C-<backspace> C-<backspace> 7 RET" 7))
          do (multiple-value-bind (f editor) (run-keys keys :text text)
               (declare (ignore f))
               (is = (1- line) (tuatara.buffer:buffer-line-number (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                                                  (session-point editor))
                   "~a" keys)))))

(define-modal-test (command screen-and-scrolling)
  (multiple-value-bind (f editor)
      (run-keys "g e" :text (format nil "~{l~a~%~}" (loop for i below 50 collect i)) :height 6)
    (declare (ignore editor))
    ;; window has 4 text rows; the last (empty) line is visible at the bottom
    (is string= "l49" (tuatara.frontend.fake:fake-screen-line f 2))
    (is equal '(3 . 0) (tuatara.frontend.fake:fake-cursor f)))
  (multiple-value-bind (f editor) (run-keys "i" :text "x")
    (declare (ignore editor))
    ;; a block in every state unless set (COMMAND CURSOR-LOOKS)
    (is eq :block (tuatara.frontend.fake:fake-cursor-shape f))
    (true (search "TUATARA[I]" (tuatara.frontend.fake:fake-screen-line f 6)))))

(define-modal-test (command cursor-looks)
  ;; one shape and colour for all, a block in the text's colour unless set;
  ;; each of the modal states can have its own
  (flet ((look (keys)
           (let ((f (run-keys keys :text "x")))
             (list (tuatara.frontend.fake:fake-cursor-shape f) (tuatara.frontend.fake:fake-cursor-colour f)))))
    (is equal '(:block nil) (look ""))
    (is equal '(:block nil) (look "i"))
    (let ((tuatara-user:*cursor-shape* :underline) (tuatara-user:*cursor-colour* "#ff0000"))
      (is equal '(:underline "#ff0000") (look ""))
      (is equal '(:underline "#ff0000") (look "i"))
      (let ((tuatara-user:*tuatara-insert-cursor-shape* :bar) (tuatara-user:*tuatara-insert-cursor-colour* :red)
            (tuatara-user:*tuatara-normal-cursor-colour* 33))
        (is equal '(:bar :red) (look "i"))
        (is equal '(:underline 33) (look ""))
        (is equal '(:underline 33) (look "i C-g")))))
  ;; support buffers their own
  (let ((tuatara-user:*tuatara-support-cursor-shape* :bar))
    (let ((f (run-keys "C-h k C-f C-x o" :text "x" :height 20)))
      (is eq :bar (tuatara.frontend.fake:fake-cursor-shape f))))
  ;; colours as faces take them, written for the terminal's OSC 12
  (is string= "#cd0000" (tuatara.display:colour-hex :red))
  (is string= "#ff0000" (tuatara.display:colour-hex 196))
  (is string= "#45b1e8" (tuatara.display:colour-hex "#45B1E8"))
  (false (tuatara.display:colour-hex :no-such-colour))
  (false (tuatara.display:colour-hex "blue"))
  ;; and the terminal's shapes, DECSCUSR's steady ones
  (is equal '(2 6 4) (mapcar #'tuatara.frontend.tui::cursor-shape-code '(:block :bar :underline)))
  (is string= "#ff0000" (tuatara.frontend.tui::cursor-colour-code "#ff0000")))

(define-modal-test (command region-is-highlighted)
  ;; v then two steps right: the two selected characters are painted with the region face
  (multiple-value-bind (f editor) (run-keys "v l l" :text "abcdef")
    (declare (ignore editor))
    (is equal '((0 2 :region)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
  ;; a visual-line selection over two lines highlights both rows
  (multiple-value-bind (f editor) (run-keys "V J" :text (format nil "one~%two~%three"))
    (declare (ignore editor))
    (is equal '((0 3 :region)) (aref (tuatara.frontend.fake:fake-spans f) 0))
    (is equal '((0 3 :region)) (aref (tuatara.frontend.fake:fake-spans f) 1))
    (false (aref (tuatara.frontend.fake:fake-spans f) 2)))
  ;; C-g drops the highlight
  (multiple-value-bind (f editor) (run-keys "v l l C-g" :text "abcdef")
    (declare (ignore editor))
    (false (aref (tuatara.frontend.fake:fake-spans f) 0))))

(define-test (command mode-line-directory)
  ;; the buffer's directory ends the mode line, in a face of its own; home is ~
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "tuatara-mode-line-~a/" (random 1000000))
                                      (uiop:temporary-directory))))
         (path (merge-pathnames "notes.txt" directory)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output) (write-string "text" o))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys "" :file path :width 100)
           (let* ((line (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 6)))
                  (name (tuatara.io:native directory))
                  (span (find :mode-line-directory (aref (tuatara.frontend.fake:fake-spans f) 6) :key #'third)))
             (true (search name line :from-end t))
             (is = (length line) (+ (search name line :from-end t) (length name)))
             (true span)
             (is string= name (subseq line (first span) (second span))))
           (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))
  (is string= "~/" (tuatara.command::buffer-directory-name
                    (tuatara.buffer:make-buffer "*home*"
                                                :text ""
                                                :file (merge-pathnames "x.txt" (user-homedir-pathname)))))
  (let ((buffer (tuatara.buffer:find-buffer "*home*"))) (when buffer (tuatara.buffer:kill-buffer buffer))))

(define-modal-test (command modified-indicator)
  ;; the modal state first, the sign of the buffer's jobs, then [-], [*] or [%], then the name
  (multiple-value-bind (f editor) (run-keys "" :text "abc")
    (declare (ignore editor))
    (true (search " TUATARA[N] PD [-] *scratch*" (tuatara.frontend.fake:fake-screen-line f 6))))
  (multiple-value-bind (f editor) (run-keys "i x C-g" :text "abc")
    (declare (ignore editor))
    (true (search " TUATARA[N] PD [*] *scratch*" (tuatara.frontend.fake:fake-screen-line f 6))))
  ;; undoing back to the text as it was: [-] again
  (multiple-value-bind (f editor) (run-keys "i x C-g C-/" :text "abc")
    (is string= "abc" (session-text editor))
    (true (search " TUATARA[N] PD [-] *scratch*" (tuatara.frontend.fake:fake-screen-line f 6))))
  ;; read-only wins
  (multiple-value-bind (f editor) (run-keys "C-x C-q" :text "abc")
    (declare (ignore editor))
    (true (search "[%] *scratch*" (tuatara.frontend.fake:fake-screen-line f 6)))))

(define-test (command messages-buffer)
  ;; every message is kept in *Messages*, a repeat counted; the keys of a sequence being typed are not
  (let ((old (tuatara.buffer:find-buffer "*Messages*"))) (when old (tuatara.buffer:kill-buffer old)))
  (multiple-value-bind (f editor) (run-keys "C-c C-c C-h e" :text "abc" :height 10)
    (let ((messages (tuatara.buffer:find-buffer "*Messages*")))
      (true messages)
      (true (search "C-c is undefined [2 times]" (tuatara.buffer:buffer-string messages)))
      (false (search "C-h-" (tuatara.buffer:buffer-string messages)))
      (false (tuatara.buffer:buffer-modified-p messages))
      (true (tuatara.buffer:buffer-local messages :read-only))
      ;; C-h e shows it
      (is string= "*Messages*" (tuatara.buffer:buffer-name (session-buffer editor)))
      (true (tuatara.frontend.fake:fake-frame-showing f "C-c is undefined [2 times]"))))
  ;; it keeps *MESSAGE-LOG-MAX* lines, the oldest going first
  (let ((tuatara.command:*message-log-max* 3))
    (let ((old (tuatara.buffer:find-buffer "*Messages*"))) (when old (tuatara.buffer:kill-buffer old)))
    (dotimes (i 5) (tuatara.command:log-message (format nil "line ~d" i)))
    (is string= (format nil "line 2~%line 3~%line 4~%")
        (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))))

(define-modal-test (command system-clipboard)
  ;; what is copied goes on the system clipboard as well
  (multiple-value-bind (f editor) (run-keys "v l l y" :text "abcdef")
    (declare (ignore editor))
    (is string= "ab" (tuatara.frontend.fake:fake-clipboard f)))
  ;; what another program copied is pasted
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "xyz")
    (setf (tuatara.frontend.fake:fake-clipboard f) "from outside ")
    (tuatara.frontend.fake:fake-queue-keys f "p")
    (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                :buffer (tuatara.buffer:find-buffer "*scratch*"))))
      (is string= "from outside xyz" (session-text editor))))
  ;; with it off, nothing goes there
  (let ((tuatara-user:*use-system-clipboard* nil))
    (multiple-value-bind (f editor) (run-keys "v l y" :text "abcdef")
      (declare (ignore editor))
      (false (tuatara.frontend.fake:fake-clipboard f)))))

(define-modal-test (command unbalanced-brackets)
  ;; W and B on a bracket without a partner say so and stay, as over a grammar's text
  (multiple-value-bind (f editor) (run-keys "W" :text "(a (b c d")
    (is = 0 (session-point editor))
    (true (search "The brackets do not balance" (tuatara.frontend.fake:fake-screen-line f 7))))
  (multiple-value-bind (f editor) (run-keys "g e B" :text "a b) c)")
    (declare (ignore f))
    (true (plusp (session-point editor))))
  (multiple-value-bind (f editor) (run-keys "g l B" :text "x a b)")
    (is = 6 (session-point editor))
    (true (search "The brackets do not balance" (tuatara.frontend.fake:fake-screen-line f 7))))
  ;; balanced groups still move as before, backward too
  (is = 0 (session-point (nth-value 1 (run-keys "g l B" :text "(a (b) c)"))))
  ;; a partner past the limit counts as none
  (let ((tuatara.defaults::*text-scan-limit* 4))
    (multiple-value-bind (f editor) (run-keys "W" :text "(abcdef)")
      (is = 0 (session-point editor))
      (true (search "The brackets do not balance" (tuatara.frontend.fake:fake-screen-line f 7))))))

(define-modal-test (command large-kills)
  ;; a kill too large for the system clipboard is kept as a rope, pasted by a
  ;; splice; a large deletion is a splice that undo swaps back: with the limits
  ;; small, a few words do what gigabytes would
  (let ((tuatara-user:*clipboard-size-limit* 8)
        (tuatara.buffer:*large-edit-octets* 10)
        (text (format nil "alpha beta gamma~%second line here~%third~%")))
    ;; V j y copies two lines: a rope, not on the clipboard; G p pastes them at the end
    (multiple-value-bind (f editor) (run-keys "V j y g e p" :text text)
      (let ((kill (first (tuatara.command:editor-kill-ring editor))))
        (true (tuatara.text:rope-p kill))
        (is string= (format nil "alpha beta gamma~%second line here~%") (tuatara.text:rope-string kill))
        (false (tuatara.frontend.fake:fake-clipboard f))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied 34 B, kept where it is"))
        (is string= (format nil "~aalpha beta gamma~%second line here~%" text) (session-text editor))))
    ;; V j d cuts them as one splice; u puts them back
    (multiple-value-bind (f editor) (run-keys "V j d" :text text)
      (declare (ignore f))
      (is string= (format nil "third~%") (session-text editor))
      (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (true (tuatara.text:batch-swap (first (first (tuatara.buffer:buffer-undo-stack b)))))
        (tuatara.buffer:buffer-undo b)
        (is string= text (tuatara.buffer:buffer-string b))))
    ;; kills in a row join, ropes and strings alike; C-y pastes the whole
    (multiple-value-bind (f editor) (run-keys "C-k C-k C-k C-k M-> C-y" :text text)
      (declare (ignore f))
      (is string= (format nil "third~%alpha beta gamma~%second line here~%") (session-text editor)))
    ;; a count pastes that many copies
    (multiple-value-bind (f editor) (run-keys "V j y g e C-u 5 p" :text text)
      (declare (ignore f))
      (is string= (format nil
                          "~{~a~}"
                          (cons text (loop repeat 5 collect (format nil "alpha beta gamma~%second line here~%"))))
          (session-text editor)))
    ;; pasting at every cursor refuses a kill kept as a rope
    (multiple-value-bind (f editor) (run-keys "V j y g g t p" :text text)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "too large to paste at every cursor")))))

(define-modal-test (command tick-function-errors)
  ;; an error in work taken in between keys is said, and the editor goes on
  (let ((failing (lambda (editor) (declare (ignore editor)) (error "broken tick"))))
    (push failing tuatara.command:*tick-functions*)
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys "i a b c C-g" :text "")
           (true (tuatara.frontend.fake:fake-frame-showing f "broken tick"))
           (is string= "abc" (session-text editor)))
      (setf tuatara.command:*tick-functions* (remove failing tuatara.command:*tick-functions*)))))

(defun typed-keys (text)
  "TEXT as the keys that type it, a space as SPC."
  (format nil "~{~a~^ ~}" (map 'list (lambda (c) (if (char= c #\Space) "SPC" (string c))) text)))

(define-test (command mode-line-long-names)
  (let ((long "a-very-long-buffer-name-that-goes-on-and-on-and-on.txt"))
    (flet ((mode-line (keys width)
             (multiple-value-bind (f editor)
                 (run-keys (format nil "C-x b ~a RET ~a" (typed-keys long) keys) :width width)
               (declare (ignore editor))
               (tuatara.frontend.fake:fake-screen-line f 6))))
      (let ((old (tuatara.buffer:find-buffer long))) (when old (tuatara.buffer:kill-buffer old)))
      (let ((b (tuatara.buffer:make-buffer long :text "x")))
        (unwind-protect
             (progn
               ;; too long for the window: its start and its end, the middle as ..., the line and column showing
               (let ((line (mode-line "" 60)))
                 (true (search "[-] a-very" line))
                 (true (search "…" line))
                 (true (search "on.txt  L1 C0" line)))
               ;; room enough: whole
               (let ((line (mode-line "" 150)))
                 (true (search long line))
                 (false (search "…" line)))
               ;; *mode-line-name-limit* cuts it anyway, to no fewer than 12 columns
               (let ((tuatara-user:*mode-line-name-limit* 20))
                 (let ((line (mode-line "" 150)))
                   (false (search long line))
                   (true (search "…" line))
                   (is = 20 (- (search "  L1" line) (+ (search "[-] " line) 4))))))
          (tuatara.buffer:kill-buffer b))))))

(define-modal-test (command modal-editing-off)
  ;; with *modal-editing* off, typing inserts and Emacs's keys work, with no state in the mode line;
  ;; the commands the modal keys run are still there
  (let ((tuatara-user:*modal-editing* nil))
    (multiple-value-bind (f editor) (run-keys "a b c C-a C-k x" :text "" :width 80 :height 8)
      (is string= "x" (session-text editor))
      (false (tuatara.frontend.fake:fake-frame-showing f "TUATARA[")))
    (multiple-value-bind (f editor)
        (run-keys "a b C-SPC C-a M-x s e l e c t - w h o l e - b u f f e r RET" :text "" :width 80 :height 8)
      (declare (ignore f))
      (true (tuatara.buffer:buffer-mark-active-p (session-buffer editor)))))
  ;; on, as it is unless turned off: NORMAL state, its label
  (let ((f (run-keys "j k" :text "" :width 80 :height 8)))
    (true (tuatara.frontend.fake:fake-frame-showing f "TUATARA[N]")))
  ;; the names the settings had as DONKEY's set the new ones
  (let ((tuatara-user:*tuatara-normal-cursor-shape* nil))
    (setf tuatara-user:*tuatara-normal-cursor-shape* :bar)
    (is eq :bar tuatara-user:*tuatara-normal-cursor-shape*)))

(define-test (command drawing-fails-usable)
  ;; a window whose drawing fails: what went wrong and the way out said, the minibuffer still shown as it reads
  (let* ((f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 10))
         (b (let ((old (tuatara.buffer:find-buffer "*broken*"))) (when old (tuatara.buffer:kill-buffer old))
              (tuatara.buffer:make-buffer "*broken*" :text (format nil "one~%two~%")))))
    (setf (tuatara.buffer:buffer-local b :line-prefix-function)
          (lambda (&rest args) (declare (ignore args)) (error 'type-error :datum 10105 :expected-type 'list)))
    (tuatara.frontend.fake:fake-queue-keys f "M-x")
    (tuatara.frontend.fake:fake-queue-action f (lambda () nil))
    (tuatara.frontend.fake:fake-queue-keys f "C-g")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer b)
    (true (tuatara.frontend.fake:fake-frame-showing f "This window cannot be shown" "10105"))
    (true (tuatara.frontend.fake:fake-frame-showing f "kills this one" "quits"))
    (true (loop for screen in (tuatara.frontend.fake::fake-history f)
                thereis (some (lambda (row) (uiop:string-prefix-p "M-x" row)) screen)))
    (tuatara.buffer:kill-buffer b)))

(defvar *growing-batches* '()
  "What the test command TEST-GROWING-READ's source gives, a batch each time it is called.")

(defvar *growing-chosen* nil
  "What TEST-GROWING-READ read.")

(tuatara.command:define-command test-growing-read ()
  "Read among candidates that come in batches (*GROWING-BATCHES*), as from work in the background."
  (let ((items '()))
    (setf *growing-chosen*
          (tuatara.command:completing-read
           "Growing: " (lambda (input) (declare (ignore input)) items)
           :require-match t :separator (code-char 0)
           :source (lambda ()
                     (let ((batch (pop *growing-batches*)))
                       (setf items (append items batch))
                       (values (and batch t) (and *growing-batches* t))))))))

(define-test (command growing-candidates)
  ;; a candidate moved to by hand stays selected as more come (a, sorted first, came after)
  (let ((*growing-batches* (list '("b" "c") '("a") '("d"))) (*growing-chosen* nil))
    (run-keys '("M-x t e s t - g r o w i n g - r e a d RET C-n" :tick "RET") :width 80)
    (is equal "c" *growing-chosen*))
  ;; moved to by hand: taken at once, more still coming
  (let ((*growing-batches* (list '("b" "c") '("a") '("d"))) (*growing-chosen* nil))
    (run-keys "M-x t e s t - g r o w i n g - r e a d RET C-n RET" :width 80)
    (is equal "c" *growing-chosen*))
  ;; RET before all have come, nothing moved to: the best match once all have
  (let ((*growing-batches* (list '("b" "c") '("a") '("d"))) (*growing-chosen* nil))
    (run-keys "M-x t e s t - g r o w i n g - r e a d RET RET" :width 80)
    (is equal "a" *growing-chosen*)
    (is equal '() *growing-batches*))
  ;; what is typed narrows what comes later too
  (let ((*growing-batches* (list '("b" "c") '("dd" "ad"))) (*growing-chosen* nil))
    (run-keys "M-x t e s t - g r o w i n g - r e a d RET a RET" :width 80)
    (is equal "ad" *growing-chosen*)))

(define-test (command messages-from-threads)
  ;; said on another thread: handed to the editor's, which shows it and keeps it
  (let ((handed nil))
    (let* ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8)))
      (tuatara.frontend.fake:fake-queue-action
       f (lambda ()
           (sb-thread:join-thread
            (sb-thread:make-thread
             (lambda ()
               (tuatara.command:message "said on a thread of its own")
               (setf handed (copy-list tuatara.command::*messages-from-threads*)))))))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
      (is equal '((nil . "said on a thread of its own")) handed)
      (true (tuatara.frontend.fake:fake-frame-showing f "said on a thread of its own"))
      (true (search "said on a thread of its own"
                    (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
      (is eq nil tuatara.command::*running-editor*)))
  ;; with no editor running, said at once
  (tuatara.command:message "said with no editor")
  (true (search "said with no editor" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))))


(define-test (command failing-highlight-left-out)
  ;; a way of marking text that fails is left out, said once; the screen is still drawn
  (let* ((calls 0)
         (broken (lambda (editor window rows)
                   (declare (ignore editor window rows))
                   (incf calls)
                   (error "broken on purpose")))
         (tuatara.command:*highlight-functions* (cons broken tuatara.command:*highlight-functions*)))
    (multiple-value-bind (f editor) (run-keys "j k j" :text (format nil "one~%two~%") :width 60 :height 6)
      (declare (ignore editor))
      (true (> calls 1))
      (true (search "one" (tuatara.frontend.fake:fake-screen-line f 0)))
      (true (search "failed and was left out: broken on purpose"
                    (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
      (is = 1
          (count-matches "broken on purpose"
                         (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))))))

(define-test (command help-window-selected)
  ;; *help-window-select*: the window help shows in is selected, q closes it and comes back
  (let ((tuatara-user:*help-window-select* t))
    (multiple-value-bind (f editor) (run-keys "C-h f f o r w a r d - c h a r RET" :text "abc" :width 80 :height 20)
      (declare (ignore f))
      (is string= "*Help*" (tuatara.buffer:buffer-name (session-buffer editor))))
    (multiple-value-bind (f editor) (run-keys "C-h f f o r w a r d - c h a r RET q" :text "abc" :width 80 :height 20)
      (declare (ignore f))
      (is string= "*scratch*" (tuatara.buffer:buffer-name (session-buffer editor)))))
  ;; as standard, you stay where you were
  (let ((tuatara-user:*help-window-select* nil))
    (multiple-value-bind (f editor) (run-keys "C-h f f o r w a r d - c h a r RET" :text "abc" :width 80 :height 20)
      (declare (ignore f))
      (is string= "*scratch*" (tuatara.buffer:buffer-name (session-buffer editor))))))

(define-modal-test (command escape-quits)
  ;; *escape-quits*: Escape is C-g everywhere, but C-q ESC still an escape character
  (let ((tuatara-user:*escape-quits* t))
    ;; INSERT state left as C-g leaves it
    (multiple-value-bind (f editor) (run-keys "i a b ESC h x" :text "" :width 60 :height 8)
      (declare (ignore f))
      (is string= (run-keys-text "i a b C-g h x") (session-text editor)))
    ;; a question abandoned, nothing run
    (multiple-value-bind (f editor)
        (run-keys "M-x e n d - o f - b u f ESC" :text (format nil "a~%b") :width 60 :height 8)
      (declare (ignore f))
      (is = 0 (session-point editor)))
    ;; quoted insert takes Escape as itself
    (multiple-value-bind (f editor) (run-keys "i C-q ESC C-g" :text "" :width 60 :height 8)
      (declare (ignore f))
      (is string= (string (code-char 27)) (session-text editor))))
  ;; as standard, Escape is a key of its own: it neither stops work nor ends a question one key answers
  (let ((tuatara-user:*escape-quits* nil))
    (true (tuatara.command:quit-key-p (tuatara.frontend:make-key #\g :ctrl t)))
    (false (tuatara.command:quit-key-p (tuatara.frontend:make-key :escape)))
    (let ((tuatara-user:*escape-quits* t))
      (true (tuatara.command:quit-key-p (tuatara.frontend:make-key :escape))))))

(define-test (command escape-where-nothing-binds-it)
  ;; as standard, Escape bound nowhere does what C-g does there: INSERT state left, never "ESC is undefined"
  (let ((tuatara-user:*escape-quits* nil))
    (multiple-value-bind (f editor) (run-keys "i a b ESC h c" :text "" :width 60 :height 8)
      (is string= (run-keys-text "i a b C-g h c") (session-text editor))
      (false (tuatara.frontend.fake:fake-frame-showing f "ESC is undefined")))
    (let ((editor (nth-value 1 (run-keys "i a b ESC" :text "" :width 60 :height 8))))
      (is eq :normal (tuatara.defaults:modal-state (session-buffer editor))))
    ;; in NORMAL state too, as C-g: the selection dropped
    (multiple-value-bind (f editor) (run-keys "v l ESC" :text (format nil "ab~%cd") :width 60 :height 8)
      (false (tuatara.frontend.fake:fake-frame-showing f "ESC is undefined"))
      (false (tuatara.buffer:buffer-mark-active-p (session-buffer editor))))
    ;; where Escape is bound it does its own: the minibuffer's question closed, nothing run
    (multiple-value-bind (f editor)
        (run-keys "M-x e n d - o f - b u f ESC" :text (format nil "a~%b") :width 60 :height 8)
      (declare (ignore f))
      (is = 0 (session-point editor)))
    ;; and after a prefix key it is no key of the sequence's
    (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x ESC" :text "" :width 60 :height 8)
                                                    "C-x ESC is undefined"))))

(define-test (command escape-alone-or-as-meta-from-a-terminal)
  ;; in a terminal, ESC alone is Escape; ESC with a key sent at once is that key with Meta
  ;; (Alt and the key, or ESC x typed as M-x); with the rest coming later, Escape alone
  (flet ((spec (script &optional kitty)
           (tuatara.frontend:key-spec (tuatara.frontend:event-key (scripted-terminal-events script :kitty kitty)))))
    (is equal '(:escape) (spec '()))
    (is equal '(:meta #\x) (spec '(#\x)))
    (is equal '(:escape) (spec '(:gap #\x)))
    ;; with kitty's keyboard on, Escape alone too, and Alt and a key still Meta
    (is equal '(:escape) (spec '() t))
    (is equal '(:meta #\x) (spec '(#\x) t))))

(defun run-keys-text (keys)
  "The text an empty scratch buffer holds after KEYS."
  (session-text (nth-value 1 (run-keys keys :text "" :width 60 :height 8))))

(define-test (command kitty-keyboard)
  ;; kitty's keyboard protocol: CSI u sequences read as the keys the window frontend gives
  (flet ((key (parameters)
           (let ((k (tuatara.frontend.tui::kitty-key parameters))) (and k (tuatara.frontend:key-spec k)))))
    (is equal '(:escape) (key "27"))
    (is equal '(:meta #\d) (key "100;3"))
    (is equal '(:ctrl #\i) (key "105;5"))
    (is equal '(:ctrl #\m) (key "109;5"))
    (is equal '(:tab) (key "9"))
    (is equal '(:enter) (key "13"))
    (is equal '(:ctrl :meta #\x) (key "120;7"))
    ;; Shift and a letter: the capital, as the window has it
    (is equal '(:ctrl #\A) (key "97;6"))
    (is equal '(:ctrl #\Space) (key "32;5"))
    (is equal '(:shift :tab) (key "9;2"))
    ;; Shift with a key that types something: what it types (kitty reports it with flag 4)
    (is equal '(:meta #\>) (key "46:62;4"))
    (is equal '(:meta #\%) (key "53:37;4"))
    (is equal '(:ctrl #\_) (key "45:95;6"))
    (is equal '(:meta #\A) (key "97:65;4"))
    ;; without what it types: as on a US keyboard
    (is equal '(:meta #\>) (key "46;4"))
    (is equal '(:ctrl #\_) (key "45;6"))
    ;; the keypad: its keys as the others, its digits as digits
    (is equal '(:left) (key "57417"))
    (is equal '(:ctrl :page-down) (key "57422;5"))
    (is equal '(#\7) (key "57406"))
    (is equal '(:enter) (key "57414"))
    ;; a lock key alone, and what is not one: no key
    (false (key "57358"))
    (false (key "")))
  ;; xterm's special keys with modifiers, and a terminal's answer: no key
  (flet ((key (parameters final)
           (let ((k (tuatara.frontend.tui::csi-key parameters final))) (and k (tuatara.frontend:key-spec k)))))
    (is equal '(:meta :up) (key "1;3" #\A))
    (is equal '(:ctrl :shift :left) (key "1;6" #\D))
    (is equal '(:delete) (key "3" #\~))
    (is equal '(:meta :page-down) (key "6;3" #\~))
    (is equal '(:meta #\d) (key "100;3" #\u))
    (false (key "?0" #\u))
    (false (key "?62;22" #\c))
    (false (key "1;2" #\R)))
  ;; ncurses' codes: Shift's, F13 and on, and names with modifiers; unknown ones no key
  (flet ((spec (k) (and k (tuatara.frontend:key-spec k))))
    (is equal '(:shift :left) (spec (tuatara.frontend.tui::ncurses-key 393)))
    (is equal '(:shift :f1) (spec (tuatara.frontend.tui::ncurses-key 277)))
    (is equal '(:ctrl :f2) (spec (tuatara.frontend.tui::ncurses-key 290)))
    (is equal '(:meta :f12) (spec (tuatara.frontend.tui::ncurses-key 324)))
    (is equal '(:meta :up) (spec (tuatara.frontend.tui::key-name-key "kUP3")))
    (is equal '(:ctrl :shift :right) (spec (tuatara.frontend.tui::key-name-key "kRIT6")))
    (false (tuatara.frontend.tui::key-name-key "kfoo")))
  ;; every key as a sequence (flag 8) with what it types (16): the text, a key let go nothing
  (flet ((key (parameters)
           (let ((k (tuatara.frontend.tui::kitty-key parameters))) (and k (tuatara.frontend:key-spec k)))))
    (is equal '(#\x) (key "120;;120"))
    (is equal '(#\A) (key "97:65;2;65"))
    (is equal (list (code-char 229)) (key "229;;229"))
    (false (key "120;1:3"))
    (false (key "57441;2"))
    (false (key "57441;1:3")))
  ;; text of more than one character: all of it
  (is string= "ab" (tuatara.frontend.tui::kitty-key "97;;97:98"))
  ;; a special key let go is nothing either
  (false (tuatara.frontend.tui::csi-key "1;1:3" #\A))
  (false (tuatara.frontend.tui::csi-key "3;1:3" #\~))
  ;; the terminal's answer: with a CSI ? flags u before its device attributes, it speaks the protocol
  (true (tuatara.frontend.tui::kitty-keyboard-reply-p (format nil "~c[?0u~c[?62;c" #\Escape #\Escape)))
  (false (tuatara.frontend.tui::kitty-keyboard-reply-p (format nil "~c[?62;22c" #\Escape))))

(define-test (command kitty-altgr-as-meta)
  ;; AltGr held, as the terminal reports it pressed and let go: a letter key that types a sign
  ;; is Meta with the letter, by the key's code or else its place on a US keyboard (kitty
  ;; gives the sign as the code, », and x as that place); digits and signs type as they do
  (flet ((key (parameters)
           (let ((k (tuatara.frontend.tui::kitty-key parameters))) (and k (tuatara.frontend:key-spec k)))))
    (let ((tuatara-user:*altgr-as-meta* t) (tuatara.frontend.tui::*altgr-held* nil))
      (false (key "57453"))
      (true tuatara.frontend.tui::*altgr-held*)
      (is equal '(:meta #\x) (key "187::120;;187"))
      (is equal '(:meta #\x) (key "120;;187"))
      (is equal '(:meta #\X) (key "62::120;2;62"))
      (is equal '(:meta #\e) (key "8364::101;;8364"))
      ;; a Cyrillic key with AltGr held keeps its letter, as in the window: never M-a
      (is equal (list (code-char #x444)) (key "1092::97;;1092"))
      ;; with Control the terminal sends no text: C-M-x, as the window gives
      (is equal '(:ctrl :meta #\x) (key "187::120;5"))
      (false (key "187::120;1:3"))
      (is equal '(#\{) (key "123::55;;123"))
      (is equal '(#\@) (key "64::50;;64"))
      (is equal '(#\\) (key "92::45;;92"))
      (false (key "57453;1:3"))
      (false tuatara.frontend.tui::*altgr-held*)
      ;; AltGr not held: a letter composed after an accent's dead key types, as without a base
      (is equal (list (code-char 233)) (key "233::101;;233"))
      (is equal (list (code-char 187)) (key "187;;187"))
      ;; the left Alt is Meta as always
      (is equal '(:meta #\x) (key "120;3"))
      ;; AltGr held on a letter key with no third sign: the letter, and AltGr still held for the next
      (setf tuatara.frontend.tui::*altgr-held* t)
      (is equal '(#\q) (key "113;;113"))
      (true tuatara.frontend.tui::*altgr-held*)
      (is equal '(:meta #\x) (key "187::120;;187")))
    (let ((tuatara-user:*altgr-as-meta* nil) (tuatara.frontend.tui::*altgr-held* t))
      (is equal (list (code-char 187)) (key "187::120;;187"))))
  ;; AltGr let go unseen: not held once the terminal loses the focus, or the protocol is asked again
  (let ((tuatara.frontend.tui::*altgr-held* t))
    (false (tuatara.frontend.tui::csi-key "" #\O))
    (false tuatara.frontend.tui::*altgr-held*))
  (let ((tuatara.frontend.tui::*altgr-held* t) (tuatara.frontend.tui::*kitty-flags* nil))
    (with-output-to-string (*standard-output*) (tuatara.frontend.tui::kitty-keyboard t))
    (false tuatara.frontend.tui::*altgr-held*))
  ;; the setting changed while the terminal runs: the flags asked again, on and off
  (flet ((changed (on flags)
           (let ((tuatara-user:*altgr-as-meta* on) (tuatara.frontend.tui::*kitty-flags* flags)
                 (tuatara.frontend.tui::*kitty-keyboard-on* t))
             (with-output-to-string (*standard-output*) (tuatara.frontend.tui::kitty-flags-follow-setting)))))
    (let ((esc (string #\Escape)))
      (is string= (concatenate 'string esc "[<u" esc "[>31u" esc "[?1004h") (changed t 5))
      (is string= (concatenate 'string esc "[<u" esc "[?1004l" esc "[>5u") (changed nil 31))
      (is string= "" (changed t 31))
      (is string= "" (changed nil 5))))
  ;; text of more than one character from one key: a key each, the first now
  (let* ((f (tuatara.frontend.tui:make-tui-frontend))
         (event (tuatara.frontend.tui::key-event f "ab")))
    (is equal '(#\a) (tuatara.frontend:key-spec (tuatara.frontend:event-key event)))
    (is equal '((#\b)) (mapcar (lambda (e) (tuatara.frontend:key-spec (tuatara.frontend:event-key e)))
                               (tuatara.frontend.tui::tui-pending f))))
  ;; the terminal asked for every key pressed and let go only under the setting
  (flet ((asked (on)
           (let ((tuatara-user:*altgr-as-meta* on) (tuatara.frontend.tui::*kitty-flags* nil)
                 (tuatara.frontend.tui::*altgr-held* nil))
             (with-output-to-string (*standard-output*) (tuatara.frontend.tui::kitty-keyboard t)))))
    (true (search "[>31u" (asked t)))
    (true (search "[?1004h" (asked t)))
    (true (search "[>5u" (asked nil)))
    (false (search "[?1004h" (asked nil)))))

(define-test (command tui-mouse-reports)
  ;; the terminal's reports of the mouse (SGR's form) as the editor's mouse keys, at the
  ;; editor's cells: the padding taken off
  (let ((f (tuatara.frontend.tui:make-tui-frontend)))
    (setf (tuatara.frontend.tui::tui-padding f) '(1 0 0 2))
    (flet ((key (parameters final)
             (let ((k (tuatara.frontend.tui::mouse-key f parameters final)))
               (and k
                    (list (tuatara.frontend:key-spec k)
                          (tuatara.frontend:key-column k)
                          (tuatara.frontend:key-row k))))))
      (is equal '((:down-mouse-1) 7 3) (key "<0;10;5" #\M))
      ;; again on that cell at once: a double and a triple click; elsewhere a press again
      (is equal '((:double-mouse-1) 7 3) (key "<0;10;5" #\M))
      (is equal '((:triple-mouse-1) 7 3) (key "<0;10;5" #\M))
      (is equal '((:down-mouse-1) 8 3) (key "<0;11;5" #\M))
      ;; moved while held, let go, the wheel, with keys held
      (is equal '((:drag-mouse-1) 9 4) (key "<32;12;6" #\M))
      (is equal '((:up-mouse-1) 9 4) (key "<0;12;6" #\m))
      (is equal '((:wheel-up) 0 0) (key "<64;3;2" #\M))
      (is equal '((:ctrl :wheel-down) 0 0) (key "<81;3;2" #\M))
      (is equal '((:ctrl :shift :down-mouse-1) 0 0) (key "<20;3;2" #\M))
      ;; the middle button pressed; let go, the right button, and nonsense: no key
      (is equal '((:mouse-2) 0 0) (key "<1;3;2" #\M))
      (false (key "<1;3;2" #\m))
      (false (key "<2;3;2" #\M))
      (false (key "<x" #\M)))))

(define-test (command tui-bracketed-paste)
  ;; the start of a paste is told from keys; what follows is read as text up to its end,
  ;; an escape and letters in it kept as they are, carriage returns made newlines
  (true (tuatara.frontend.tui::paste-start-p "200" #\~))
  (false (tuatara.frontend.tui::paste-start-p "201" #\~))
  (false (tuatara.frontend.tui::paste-start-p "3" #\~))
  (flet ((take (text &optional (limit 1000))
           ;; TEXT with ^[ for an escape, ^M a carriage return, ^J a newline
           (let ((text (cl-ppcre:regex-replace-all "\\^\\[" (cl-ppcre:regex-replace-all "\\^M" (cl-ppcre:regex-replace-all "\\^J" text (string #\Newline)) (string #\Return)) (string #\Escape)))
                 (paste (tuatara.frontend.tui::make-paste limit)) (ended nil) (taken 0))
             (loop for c across text
                   until ended
                   do (setf ended (tuatara.frontend.tui::paste-take paste (char-code c)))
                      (incf taken))
             (list (tuatara.frontend.tui::paste-string paste) ended taken (tuatara.frontend.tui::paste-cut paste)))))
    (is equal (list (format nil "~c:(+ 1 2)~%dd" #\Escape) t 18 nil) (take "^[:(+ 1 2)^Mdd^[[201~more"))
    ;; CR LF one newline; a start of the end that was not the end stays text
    (is equal (list (format nil "a~%b~c[20x~c[A" #\Escape #\Escape) t 18 nil) (take "a^M^Jb^[[20x^[[A^[[201~"))
    ;; the end after an escape: the first escape is text
    (is equal (list (string #\Escape) t 7 nil) (take "^[^[[201~"))
    ;; past the limit nothing more is kept, and the end is still found
    (is equal '("abc" t 12 t) (take "abcdef^[[201~" 3))
    ;; no end: all of it, not ended
    (is equal '("xyz" nil 3 nil) (take "xyz"))))

;;; The terminal's input given in parts, for the tests

(defvar *terminal-script* '()
  "What the stand-in terminal gives next: characters, and :GAP where the
rest comes only after a wait.")

(defun scripted-terminal-events (script &key kitty)
  "The event the TUI makes of an ESC followed by SCRIPT (see *TERMINAL-SCRIPT*),
the terminal's reads and waits stood in for, with kitty's keyboard on when KITTY."
  (let ((*terminal-script* script)
        (tuatara.frontend.tui::*kitty-keyboard-on* kitty))
    (sb-int:encapsulate 'tuatara.frontend.tui::read-wide-char 'scripted
                        (lambda (original window)
                          (declare (ignore original window))
                          (if (characterp (first *terminal-script*))
                              (values 0 (char-code (pop *terminal-script*)))
                              (values -1 0))))
    (sb-int:encapsulate 'tuatara.io:wait-for-fds 'scripted
                        (lambda (original fds seconds)
                          (if (equal fds '(0))
                              (when *terminal-script*
                                (when (eq (first *terminal-script*) :gap) (pop *terminal-script*))
                                (list 0))
                              (funcall original fds seconds))))
    (sb-int:encapsulate 'tuatara.frontend.tui::%nodelay 'scripted (lambda (original &rest arguments) (declare (ignore original arguments)) 0))
    (sb-int:encapsulate 'tuatara.frontend.tui::%keypad 'scripted (lambda (original &rest arguments) (declare (ignore original arguments)) 0))
    (unwind-protect
         (tuatara.frontend.tui::wide-char-event (tuatara.frontend.tui:make-tui-frontend) nil 0 27)
      (dolist (name '(tuatara.frontend.tui::read-wide-char tuatara.io:wait-for-fds
                      tuatara.frontend.tui::%nodelay tuatara.frontend.tui::%keypad))
        (sb-int:unencapsulate name 'scripted)))))

(defvar *read-script* ""
  "What the stand-in terminal of OCTETS-PER-READ gives, after the ESC already read.")

(defvar *read-at* 0
  "How much of *READ-SCRIPT* the stand-in terminal has given.")

(defun octets-per-read (sequence &optional (runs 200))
  "The octets made in reading SEQUENCE, what a terminal sends after an ESC, as
the terminal frontend's event, by kitty's keyboard protocol; with SEQUENCE NIL,
in reading the plain character x. The least of five tries, as the count takes
in every thread."
  (let ((f (tuatara.frontend.tui:make-tui-frontend)) (tuatara.frontend.tui::*kitty-keyboard-on* t) (least nil))
    (with-functions-replaced ((tuatara.frontend.tui::read-wide-char
                               (lambda (window)
                                 (declare (ignore window))
                                 (if (< *read-at* (length *read-script*))
                                     (values 0 (prog1 (char-code (char *read-script* *read-at*)) (incf *read-at*)))
                                     (values -1 0))))
                              (tuatara.frontend.tui::%nodelay (lambda (window on) (declare (ignore window on)) 0)))
      (loop repeat 5
            do (let ((before (tuatara.bench::bytes-consed)))
                 (loop repeat runs
                       do (setf *read-script* (or sequence "") *read-at* 0)
                          (if sequence
                              (tuatara.frontend.tui::wide-char-event f nil 0 27)
                              (tuatara.frontend.tui::wide-char-event f nil 0 120)))
                 (let ((made (floor (- (tuatara.bench::bytes-consed) before) runs)))
                   (setf least (if least (min least made) made))))))
    least))

(define-test (command kitty-keys-made-without-garbage)
  ;; every key comes as a CSI u sequence, pressed and let go, when AltGr is asked for: an
  ;; ordinary key makes no more than its event, as a plain character does, and a key let go nothing
  (let ((plain (octets-per-read nil))
        (ordinary (octets-per-read "[120;;120u"))
        (shifted (octets-per-read "[104:72;2;72u"))
        (released (octets-per-read "[120;1:3u")))
    (true (<= ordinary (+ plain 16)))
    (true (<= shifted (+ plain 16)))
    (true (<= released 16))))

(define-test (command tui-sequences-in-parts)
  ;; a paste whose start comes in two parts (over a network) is still taken in as
  ;; text, never typed as keys
  (let ((event (scripted-terminal-events (append (list #\[ :gap) (coerce "200~dd:q" 'list)
                                                 (list #\Escape) (coerce "[201~" 'list)))))
    (is eq :paste (tuatara.frontend:event-kind event))
    (is string= "dd:q" (tuatara.frontend:event-text event)))
  ;; a key's sequence in two parts: the key
  (let ((event (scripted-terminal-events (list #\[ :gap #\A))))
    (is equal '(:up) (tuatara.frontend:key-spec (tuatara.frontend:event-key event))))
  ;; ESC [ and nothing more: M-[
  (let ((event (scripted-terminal-events (list #\[))))
    (is equal '(:meta #\[) (tuatara.frontend:key-spec (tuatara.frontend:event-key event)))))

(defun paste-then (text &optional keys)
  "A step of RUN-KEYS: TEXT pasted into the terminal Tuatara runs in, then KEYS typed."
  (lambda ()
    (let ((f (tuatara.command:editor-frontend tuatara.command:*editor*)))
      (tuatara.frontend.fake:fake-queue-event f (tuatara.frontend:make-event :paste :text text))
      (when keys (tuatara.frontend.fake:fake-queue-keys f keys)))))

(define-modal-test (command paste-goes-in-as-text)
  ;; an escape, a colon and letters that are commands in NORMAL: all go in as text, point after them
  (let ((pasted (format nil "~c:(+ 1 2)~%dd x" #\Escape)))
    (multiple-value-bind (f editor) (run-keys (list (paste-then pasted)) :text "abc")
      (declare (ignore f))
      (is string= (concatenate 'string pasted "abc") (session-text editor))
      (is = (length pasted) (session-point editor)))
    ;; one undo takes it all back
    (is string= "abc" (session-text (nth-value 1 (run-keys (list (paste-then pasted "u")) :text "abc"))))
    ;; a buffer that cannot be changed says so
    (multiple-value-bind (f editor) (run-keys (list "C-x C-q" (paste-then pasted)) :text "abc")
      (declare (ignore f))
      (is string= "abc" (session-text editor))
      (true (search "read-only" (or (tuatara.command:editor-message editor) ""))))
    ;; in the minibuffer: into the answer
    (multiple-value-bind (f editor) (run-keys (list "M-:" (paste-then "(+ 40 2)" "RET")) :text "abc")
      (declare (ignore f))
      (is string= "abc" (session-text editor))
      (true (search "42" (or (tuatara.command:editor-message editor) ""))))))

(defvar *recorded-keys* '()
  "The keys TEST-RECORD-KEY was run as, latest first.")

(tuatara.command:define-command test-record-key ()
  "Note the key this command was run as, for the tests."
  (push (tuatara.frontend:key-description (tuatara.frontend:key-spec (tuatara.command:editor-this-key tuatara.command:*editor*)))
        *recorded-keys*))

(define-modal-test (command repeat-as-its-key)
  ;; . runs the last command again as the key that ran it, not as .
  (setf *recorded-keys* '())
  (run-keys (list (lambda () (tuatara-user:bind-key "C-c z" 'test-record-key)) "C-c z . .") :text "x")
  (is equal '("z" "z" "z") *recorded-keys*))

(define-test (command long-question-read-whole)
  ;; a question wider than its row shows its start on rows above, the answer typed after its end
  (let ((answer nil))
    (multiple-value-bind (f editor)
        (run-keys (list (lambda ()
                          (setf answer (tuatara.defaults::read-string
                                        "Trust /tmp/a-long-directory-name/that-matters/? Its language servers run its code, from now on (y or n) ")))
                        "y RET")
                  :text "" :width 60 :height 12)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Trust /tmp/a-long-directory-name/that-matters/?"))
      (true (tuatara.frontend.fake:fake-frame-showing f "(y or n)"))
      (is string= "y" answer))))

(define-test (command message-and-prompt-write-out-controls)
  ;; a name with a line end or a direction mark in it is written out in a message and a question
  (let ((name (format nil "a~cb~cexe.txt" #\Newline (code-char #x202E))) (said nil))
    (multiple-value-bind (f editor)
        (run-keys (list (lambda ()
                          (setf said (tuatara.command:message "Wrote ~a" name))
                          (tuatara.defaults::read-string (format nil "Delete ~a? " name)))
                        "y RET")
                  :text "" :width 80 :height 12)
      (declare (ignore editor))
      (is string= "Wrote a^Jb<202E>exe.txt" said)
      (true (tuatara.frontend.fake:fake-frame-showing f "Delete a^Jb<202E>exe.txt? ")))))

(define-test (command long-key-question-read-whole)
  ;; a question of one key wider than its row shows its start on rows above while it
  ;; waits, as the minibuffer's does, the cursor after its end
  (let ((answer nil) (screen nil))
    (run-keys (list (lambda ()
                      (setf answer (tuatara.command:read-key
                                    "Compile and load the package pkg from /tmp/a-long-directory-name/that-matters/? (y, n, or v to view its source) ")))
                    (lambda ()
                      (let ((f (tuatara.command::frame-frontend (tuatara.command::editor-frame tuatara.command:*editor*))))
                        (setf screen (loop for row below 12 collect (tuatara.frontend.fake:fake-screen-line f row)))))
                    "y")
              :text "" :width 60 :height 12)
    ;; the rows it is broken into, between words, read as one
    (true (search "Compile and load the package pkg from /tmp/a-long-directory-name/that-matters/? (y, n, or v to view its source)"
                  (format nil "~{~a~^ ~}" (mapcar (lambda (line) (string-trim " " line)) screen))))
    (is equal '(#\y) (tuatara.frontend:key-spec answer))))

(define-test (command long-key-question-keys-coloured)
  ;; the keys named in the start of a long question of one key, on the rows above the
  ;; echo row, are coloured as on the echo row
  (let ((faces nil))
    (run-keys (list (lambda ()
                      (tuatara.command:read-key
                       (tuatara.command:substitute-command-keys
                        "\\`C-g' leaves the package pkg from /tmp/a-long-directory-name/that-matters/ alone; load it? (y or n) "
                        :marks t)))
                    (lambda ()
                      (let ((f (tuatara.command::frame-frontend (tuatara.command::editor-frame tuatara.command:*editor*))))
                        (setf faces (loop for row below 12
                                          when (search "C-g leaves" (tuatara.frontend.fake:fake-screen-line f row))
                                            append (mapcar #'third (aref (tuatara.frontend.fake:fake-spans f) row))))))
                    "y")
              :text "" :width 60 :height 12)
    (true (member :help-key-binding faces))))

(define-modal-test (command moving-by-clusters)
  ;; a letter with its accent and an emoji with its skin tone are one character to move
  ;; over, to type beside and to delete; backspace takes off the last mark alone
  (let ((text (format nil "e~cx ~c~cy" (code-char #x301) (code-char #x1F44D) (code-char #x1F3FD))))
    (flet ((point-after (keys) (session-point (nth-value 1 (run-keys keys :text text))))
           (text-after (keys) (session-text (nth-value 1 (run-keys keys :text text)))))
      (is = 3 (point-after "l"))
      (is = 13 (point-after "l l l l"))
      (is = 5 (point-after "l l l l h"))
      (is = 0 (point-after "l h"))
      (is = 3 (point-after "<right>"))
      (is = 3 (point-after "C-f"))
      (is = 5 (point-after "C-f C-f C-f C-f C-b"))
      (is string= (format nil "e~cQx ~c~cy" (code-char #x301) (code-char #x1F44D) (code-char #x1F3FD))
          (text-after "l i Q C-g"))
      (is string= (format nil "x ~c~cy" (code-char #x1F44D) (code-char #x1F3FD)) (text-after "d"))
      (is string= (format nil "e~cx y" (code-char #x301)) (text-after "l l l d"))
      (is string= (format nil "e~cx ~cy" (code-char #x301) (code-char #x1F44D)) (text-after "l l l l i DEL C-g"))
      (is string= (format nil "x ~c~cy" (code-char #x1F44D) (code-char #x1F3FD)) (text-after "i <delete> C-g")))))

(define-test (command minibuffer-cursor-in-columns)
  ;; the minibuffer's cursor stands after what is typed, in columns: a wide
  ;; character two, an accent none, as the row is drawn
  (flet ((cursor (keys &key (width 40))
           ;; the cursor's column and its row's text while the question is open
           (let ((seen nil))
             (run-keys (list keys (lambda ()
                                    (let* ((f (tuatara.command::editor-frontend tuatara.command:*editor*))
                                           (at (tuatara.frontend.fake:fake-cursor f)))
                                      (setf seen (list (cdr at) (tuatara.frontend.fake:fake-screen-line f (car at)))))))
                       :width width)
             (values-list seen))))
    (let ((start (cursor "M-x")))
      (is = (+ start 4) (cursor "M-x 漢 字"))
      (is = (+ start 4) (cursor "M-x a b 漢 C-b C-f"))
      (is = (+ start 2) (cursor "M-x 漢 字 C-b"))
      (is = (+ start 1) (cursor (format nil "M-x x e ~c C-b" (code-char #x301))))
      ;; past the row's width the row scrolls, the cursor still in it, after the text
      (multiple-value-bind (column line) (cursor "M-x 漢 字 漢 字 漢 字 漢 字 漢 字 漢 字 漢 字 漢 字 a" :width 20)
        (true (< column 20))
        ;; as much of the text before the cursor as the row has room for
        (is = 19 column)
        (is char= #\a (char line (1- (tuatara.display:chars-in-columns line column))))))))

(defun face-at-column-p (spans column face)
  "True when one of SPANS, a row's (START END FACE-OR-FACES), covers COLUMN with FACE."
  (some (lambda (span)
          (destructuring-bind (start end faces) span
            (and (<= start column) (< column end)
                 (if (listp faces) (member face faces) (eq faces face)))))
        spans))

(defun question-look ()
  "What the frame painted last shows, as a question waits: the cursor,
\(ROW . COLUMN), its rows and their spans."
  (let ((f (tuatara.command:editor-frontend tuatara.command:*editor*)))
    (list (tuatara.frontend.fake:fake-cursor f)
          (copy-seq (tuatara.frontend.fake:fake-screen f))
          (copy-seq (tuatara.frontend.fake:fake-spans f)))))

(defun cursor-at-question (keys &rest options)
  "Run a session of KEYS (a list for RUN-KEYS, given OPTIONS), :LOOK among
them noting the frame painted last as a question waits there. Values: the
cursor then, (ROW . COLUMN), the rows of that frame and their spans."
  (let ((seen nil))
    (apply #'run-keys (substitute (lambda () (setf seen (question-look))) :look keys) options)
    (values-list seen)))

(define-test (command prompt-cursor)
  ;; a question of one key: the cursor after it in the echo row, the window's point a cursor the typing does not go to
  (multiple-value-bind (cursor screen spans) (cursor-at-question '("l C-q" :look "a") :text "foo boo" :width 60)
    (is equal (cons 7 4) cursor)
    (is string= "C-q-" (string-right-trim " " (aref screen 7)))
    (true (face-at-column-p (aref spans 0) 1 :cursor-inactive)))
  ;; a key sequence half typed, the key hints shown: after it, on the echo row
  (multiple-value-bind (cursor screen) (cursor-at-question '("C-x" :look "C-g") :text "foo" :width 60 :height 20)
    (true (search "C-x-" (aref screen (car cursor))))
    (is = 4 (cdr cursor)))
  ;; a question in the minibuffer is as it was: the cursor after the prompt there
  (multiple-value-bind (cursor screen spans) (cursor-at-question '("M-x" :look "C-g") :text "foo" :width 60)
    (true (search "M-x" (aref screen (car cursor))))
    (true (face-at-column-p (aref spans 0) 0 :cursor-inactive)))
  ;; once answered, the cursor is back in the window, point drawn by it alone
  (multiple-value-bind (f editor) (run-keys "l C-q a" :text "foo boo" :width 60)
    (declare (ignore editor))
    (is equal (cons 0 2) (tuatara.frontend.fake:fake-cursor f))
    (false (some (lambda (spans) (face-at-column-p spans 2 :cursor-inactive))
                 (coerce (tuatara.frontend.fake:fake-spans f) 'list))))
  ;; a long question scrolls off the left, wide characters counted as two columns
  (is = 2 (tuatara.command::scrolled-off "abcdef" 6 4))
  (is = 1 (tuatara.command::scrolled-off (coerce '(#\U65E5 #\U672C #\U8A9E) 'string) 3 4))
  (is = 0 (tuatara.command::scrolled-off "abc" 3 4)))

(defparameter *live-question-package-code*
  "(progn
     (defvar *words* '(\"apple\" \"apricot\" \"banana\" \"cherry\"))
     (defvar *shown* *words*)
     (define-command live-narrow-words ()
       \"Narrow the words to those holding what is typed, as it is typed.\"
       (handler-case
           (message \"Kept ~{~a~^ ~}\"
                    (progn (read-string \"Words: \"
                                        :changed (lambda (text)
                                                   (setf *shown* (remove-if-not (lambda (word) (search text word)) *words*))))
                           *shown*))
         (quit () (setf *shown* *words*)))))"
  "A stand-in package's code, read in a package that uses only CL and
TUATARA: a command narrowing a list of words live through READ-STRING's
CHANGED, every word back on \\`C-g'.")

(define-test (command live-question-from-a-package)
  ;; a package narrows live through the minibuffer's question, with the API's names alone
  (let ((package (make-package "TUATARA-TEST-LIVE-QUESTION" :use '(#:cl #:tuatara))))
    (unwind-protect
         (let ((*package* package) (seen '()))
           (eval (read-from-string *live-question-package-code*))
           (flet ((typed (keys) (format nil "M-x ~{~a~^ ~} RET ~a" (coerce "live-narrow-words" 'list) keys))
                  (shown () (symbol-value (find-symbol "*SHOWN*" package)))
                  (look () (push (symbol-value (find-symbol "*SHOWN*" package)) seen)))
             ;; each change of the text narrows at once; RET keeps what is shown
             (let ((f (run-keys (list (typed "a p") #'look "r" #'look "DEL DEL" #'look "RET") :width 60)))
               (is equal '(("apple" "apricot" "banana") ("apricot") ("apple" "apricot")) seen)
               (true (tuatara.frontend.fake:fake-frame-showing f "Kept apple apricot banana")))
             ;; C-g: the package puts every word back
             (setf seen '())
             (run-keys (list (typed "c h") #'look "C-g") :width 60)
             (is equal '(("cherry")) seen)
             (is = 4 (length (shown)))))
      (tuatara.command:forget-command (find-symbol "LIVE-NARROW-WORDS" package))
      (delete-package package))))

(defparameter *option-completion-package-code*
  "(define-command read-some-options ()
     \"Ask for options, TAB completing the one at the cursor.\"
     (message \"Options: ~a\"
              (read-string \"Options: \"
                           :complete (lambda ()
                                       (complete-in-minibuffer
                                        (lambda (text point)
                                          (let ((start (1+ (or (position #\\Space text :end point :from-end t) -1))))
                                            (and (< start point)
                                                 (values start point '(\"--has-header\" \"--print-header\" \"--utc\")))))
                                        :keep-order t)))))"
  "A stand-in package's code, read in a package that uses only CL and
TUATARA: a command whose question completes the word at the cursor from a
list of options.")

(define-test (command completion-in-the-minibuffer-from-a-package)
  ;; a package completes the word at the minibuffer's cursor from a list of its own
  (let ((package (make-package "TUATARA-TEST-OPTION-COMPLETION" :use '(#:cl #:tuatara))))
    (unwind-protect
         (let ((*package* package))
           (eval (read-from-string *option-completion-package-code*))
           (flet ((typed (keys) (format nil "M-x ~{~a~^ ~} RET ~a" (coerce "read-some-options" 'list) keys)))
             ;; a lone match goes in at once, the word before it left as it is
             (true (tuatara.frontend.fake:fake-frame-showing (run-keys (typed "- H SPC - - u TAB RET") :width 60)
                                                             "Options: -H --utc"))
             ;; more than one match is listed beside the cursor, the first taken with RET
             (true (tuatara.frontend.fake:fake-frame-showing (run-keys (typed "- - h TAB RET RET") :width 60)
                                                             "Options: --has-header"))
             ;; after a space there is no word: nothing to complete, said in *Messages*
             (run-keys (typed "- H SPC TAB") :width 60)
             (true (search "Nothing to complete here"
                           (let ((log (tuatara.command:messages-buffer)))
                             (tuatara.buffer:buffer-string log))))))
      (tuatara.command:forget-command (find-symbol "READ-SOME-OPTIONS" package))
      (delete-package package))))

(define-test (command message-in-an-open-question)
  ;; what a key in a question says is shown after the text typed, in brackets, until the next key
  (true (tuatara.frontend.fake:fake-frame-showing (run-keys "M-x q q z z q RET" :width 60) "qqzzq [No match]"))
  (false (screen-showing (run-keys "M-x q q z z q RET DEL" :width 60) "No match"))
  ;; a message said before the question opens is not carried into it
  (false (screen-showing (run-keys (list (lambda () (tuatara.command:message "Said before the question")) "M-x q")
                                   :width 60)
                         "Said before")))

(defparameter *text-snapshot-package-code*
  "(defun text-as-taken (buffer)
     \"BUFFER's banked lines and its text, as they are now: the text read
from a rope later changes leave as it is.\"
     (let ((rope (buffer-text buffer)))
       (list (banked-spans buffer)
             (lambda ()
               (let ((octets (make-array (rope-octet-length rope) :element-type '(unsigned-byte 8))))
                 (rope-copy-into rope 0 (rope-octet-length rope) octets)
                 (lenient-string octets)))
             (rope-find-octet rope 10)
             (rope-line-start rope 5)
             (rope-line-end rope 5)
             (rope-octet rope 0))))"
  "A stand-in package's code, read in a package that uses only CL and
TUATARA: what a job of its own would take from a buffer before it starts.")

(define-test (command text-snapshot-from-a-package)
  ;; a package reads a buffer's text as it was taken, its banked lines and its selection
  (let ((package (make-package "TUATARA-TEST-TEXT-SNAPSHOT" :use '(#:cl #:tuatara))))
    (unwind-protect
         (let ((*package* package))
           (eval (read-from-string *text-snapshot-package-code*))
           (let ((b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*snapshot*")
                                                :text (format nil "abc~%def~%ghi~%"))))
             (unwind-protect
                  (progn
                    (tuatara.defaults::bank-span b (cons 4 8))
                    (destructuring-bind (banked text newline start end first)
                        (funcall (find-symbol "TEXT-AS-TAKEN" package) b)
                      (is equal '((4 . 8)) banked)
                      (is = 3 newline)
                      (is = 4 start)
                      (is = 7 end)
                      (is = 97 first)
                      ;; the buffer changed after the text was taken: the text taken stays
                      (tuatara.buffer:buffer-replace b 0 3 "xyz")
                      (is string= (format nil "abc~%def~%ghi~%") (funcall text))
                      ;; letting go of the banks
                      (setf (tuatara.defaults::buffer-banks b) '())
                      (is eq nil (tuatara.defaults::banked-spans b))))
               (tuatara.buffer:kill-buffer b))))
      (delete-package package))))

(define-test (command names-for-work-on-text-exported)
  ;; what a package running a program over a buffer's text needs, in the API
  (dolist (name '("BUFFER-TEXT" "ROPE-OCTET-LENGTH" "ROPE-OCTET" "ROPE-COPY-INTO" "ROPE-FIND-OCTET" "ROPE-LINE-START"
                  "ROPE-LINE-END" "FIND-OR-MAKE-BUFFER" "COMPLETE-IN-MINIBUFFER" "JOB-PROGRAM-STARTED" "RUNNING-JOB"
                  "TAKE-IN-SOON" "WRITE-OCTETS" "FILE-SYSTEM-ERROR" "LENIENT-STRING" "MERGE-RANGES"
                  "SELECTION-RANGE" "BANKED-SPANS" "SPAN-LINE-COUNT" "BUFFER-BANKS"))
    (is eq :external (nth-value 1 (find-symbol name :tuatara)) "~a is not exported" name)))

(define-test (command start-up-garbage-collected-when-idle)
  ;; the garbage of starting is collected once, in the first second nothing is
  ;; typed, rather than between keys later on
  (run-keys "")
  (true (member 'tuatara.defaults::start-up-garbage-tick tuatara.command:*tick-functions*))
  (let ((tuatara.defaults::*start-up-garbage-collected* nil)
        (tuatara.defaults::*start-up-tick-time* nil)
        (before sb-ext:*gc-run-time*))
    ;; just started, then a key straight after: called again in a second, nothing collected
    (is eql 1 (tuatara.defaults::start-up-garbage-tick nil))
    (is eql 1 (tuatara.defaults::start-up-garbage-tick nil))
    (false tuatara.defaults::*start-up-garbage-collected*)
    ;; a second with nothing typed: collected, and never again
    (sleep 1)
    (false (tuatara.defaults::start-up-garbage-tick nil))
    (true tuatara.defaults::*start-up-garbage-collected*)
    (true (> sb-ext:*gc-run-time* before))
    (false (tuatara.defaults::start-up-garbage-tick nil))))

(define-modal-test (command count-digits-after-c-u-c-u)
  ;; digits after C-u C-u give their own number, not one after 16
  (flet ((text (keys text) (session-text (nth-value 1 (run-keys keys :text text)))))
    (is string= "defghij" (text "C-u C-u 3 d" "abcdefghij"))
    (is string= "ij" (text "C-u C-u 8 d" "abcdefghij"))
    (is string= "abcdefghij" (text "C-u C-u d" (format nil "~a~a" (make-string 16 :initial-element #\x) "abcdefghij")))
    ;; digits typed together still make one number
    (is string= "mn" (text "C-u 1 2 d" "abcdefghijklmn"))))

(define-test (command cursor-beside-a-terminal-picture)
  ;; a picture placed over a buffer's text at its first cell leaves the cursor shown
  (let ((f (run-keys (list (lambda ()
                             (setf (tuatara.buffer:buffer-local (tuatara.buffer:find-buffer "*scratch*") :pictures)
                                   (list (list :picture 0 0 2 2))))
                           "C-g")
                     :text "abc" :cell-pixels '(10 20))))
    (true (tuatara.frontend.fake:fake-images f) "the picture is placed")
    (isnt eq :hidden (tuatara.frontend.fake:fake-cursor-shape f))))

(define-test (command box-over-wide-candidates)
  ;; a list of wide characters covers two columns for each
  (true (tuatara.command::box-over-area-p (list 0 0 (list (list (coerce (loop repeat 6 collect (code-char #x65E5)) 'string) nil)))
                                          0 8 2 1)))

(define-test (command passphrases-wiped)
  ;; a passphrase given up at its second asking is left nowhere
  (let ((answers '()))
    (sb-int:encapsulate 'tuatara.command:read-string 'keeping-answers
                        (lambda (function &rest arguments)
                          (let ((answer (apply function arguments))) (push answer answers) answer)))
    (unwind-protect
         (run-keys (list (lambda () (ignore-errors (tuatara.command:read-password "Passphrase: " :confirm t)))
                         "s e c r e t RET o t h e r C-g"))
      (sb-int:unencapsulate 'tuatara.command:read-string 'keeping-answers))
    (is = 1 (length answers))
    (true (every (lambda (c) (char= c (code-char 0))) (first answers)) "the first answer wiped"))
  ;; a passphrase pasted is kept in the answer alone
  (let ((pasted (copy-seq "pasted secret"))
        (answer nil)
        (f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
    (tuatara.frontend.fake:fake-queue-action f (lambda () (setf answer (tuatara.command:read-password "Passphrase: "))))
    (tuatara.frontend.fake:fake-queue-event f (tuatara.frontend:make-event :paste :text pasted))
    (tuatara.frontend.fake:fake-queue-keys f "RET")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
    (is string= "pasted secret" answer)
    (true (every (lambda (c) (char= c (code-char 0))) pasted) "the pasted text wiped")))

(define-test (command paste-into-a-prompt)
  ;; C-y brings in what a password manager copied, without its line ending, and it stays out of the kill ring
  (let ((answer nil)
        (f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
    (setf (tuatara.frontend.fake:fake-clipboard f) (format nil "long secret~%"))
    (tuatara.frontend.fake:fake-queue-action f (lambda () (setf answer (tuatara.command:read-password "Passphrase: "))))
    (tuatara.frontend.fake:fake-queue-keys f "C-y RET")
    (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))
      (is string= "long secret" answer)
      (false (member "long secret" (tuatara.command:editor-kill-ring editor) :test #'equal))))
  ;; a secret never comes from the kill ring
  (let ((answer nil)
        (f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
    (tuatara.frontend.fake:fake-queue-action
     f (lambda ()
         (push "killed" (tuatara.command:editor-kill-ring tuatara.command:*editor*))
         (setf answer (tuatara.command:read-password "Passphrase: "))))
    (tuatara.frontend.fake:fake-queue-keys f "C-y RET")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
    (is string= "" answer))
  ;; any other prompt takes the clipboard, or the text killed last when there is none
  (let ((answer nil)
        (f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
    (setf (tuatara.frontend.fake:fake-clipboard f) "from the desk")
    (tuatara.frontend.fake:fake-queue-action f (lambda () (setf answer (tuatara.command:read-string "Name: "))))
    (tuatara.frontend.fake:fake-queue-keys f "C-y RET")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
    (is string= "from the desk" answer)))

(define-test (command pasted-passphrase-copy-wiped)
  ;; the copy C-y makes of a secret without its line ending is wiped once in the answer; the
  ;; clipboard's own text is the frontend's and stays
  (let ((given '())
        (clipboard (format nil "long secret~%"))
        (f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
    (setf (tuatara.frontend.fake:fake-clipboard f) clipboard)
    (sb-int:encapsulate 'tuatara.command::minibuffer-take-pasted 'keeping-pasted
                        (lambda (function text) (push text given) (funcall function text)))
    (unwind-protect
         (progn
           (tuatara.frontend.fake:fake-queue-action f (lambda () (tuatara.command:read-password "Passphrase: ")))
           (tuatara.frontend.fake:fake-queue-keys f "C-y RET")
           (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))
      (sb-int:unencapsulate 'tuatara.command::minibuffer-take-pasted 'keeping-pasted))
    (is = 1 (length given))
    (true (every (lambda (c) (char= c (code-char 0))) (first given)) "the trimmed copy wiped")
    (is string= (format nil "long secret~%") clipboard)))

(define-test (command tick-functions-called-when-due)
  ;; a tick function that asked to look again in a minute is not called for the keys typed meanwhile, only
  ;; when the editor is woken (as each action queued here wakes it)
  (let* ((calls 0)
         (tick (lambda (editor) (declare (ignore editor)) (incf calls) 60))
         (seen '()))
    (flet ((note () (push calls seen)))
      (push tick tuatara.command:*tick-functions*)
      (unwind-protect (run-keys (list "a b c d" #'note #'note "e f" #'note))
        (setf tuatara.command:*tick-functions* (remove tick tuatara.command:*tick-functions*))))
    (destructuring-bind (after-more-keys after-wake after-keys) seen
      ;; once as the editor starts, then once for each wake, never for a key
      (is = 1 after-keys)
      (is = 2 after-wake)
      (is = 3 after-more-keys))))

(define-test (command typed-passphrase-characters-wiped)
  ;; each character of a passphrase typed is left in the answer alone
  (let ((inserted '()))
    (sb-int:encapsulate 'tuatara.command::minibuffer-insert 'keeping-inserted
                        (lambda (function string) (push string inserted) (funcall function string)))
    (unwind-protect
         (run-keys (list (lambda () (tuatara.command:read-password "Passphrase: ")) "k e a RET"))
      (sb-int:unencapsulate 'tuatara.command::minibuffer-insert 'keeping-inserted))
    (is = 3 (length inserted))
    (true (every (lambda (s) (every (lambda (c) (char= c (code-char 0))) s)) inserted) "each typed character wiped")))

(define-test (command actions-handed-over-from-threads)
  ;; every action given on other threads at once runs on the editor's thread
  (let ((ran 0) (editor-thread nil) (elsewhere 0))
    (labels ((action ()
               (unless (eq sb-thread:*current-thread* editor-thread) (incf elsewhere))
               (incf ran))
             (give-many ()
               (loop repeat 500 do (tuatara.command:defer-action #'action)))
             (start ()
               (setf editor-thread sb-thread:*current-thread*)
               (mapc #'sb-thread:join-thread (loop repeat 4 collect (sb-thread:make-thread #'give-many)))))
      (run-keys (list #'start)))
    (is = 2000 ran)
    (is = 0 elsewhere)))

(define-test (command mode-line-asks-no-directory-each-frame)
  ;; the mode line of a buffer with no file looks for its directory at most once a second
  (let ((looked 0))
    (sb-int:encapsulate 'tuatara.io:default-directory 'counting
                        (lambda (function &rest arguments) (incf looked) (apply function arguments)))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys "C-g")
           (declare (ignore f))
           (setf looked 0)
           (dotimes (i 50) (tuatara.command:default-mode-line editor))
           (true (<= looked 1) "looked ~d times for 50 mode lines" looked))
      (sb-int:unencapsulate 'tuatara.io:default-directory 'counting))))

(define-test (command title-writes-raw-bytes)
  ;; an octet that is not UTF-8 in a buffer's name is written \xNN in the title, as in the mode line
  (let* ((name (format nil "bad~cname.txt" (code-char #xDCFF)))
         (f (run-keys (list (lambda ()
                              (tuatara.command::show-title tuatara.command:*editor*
                                                           (or (tuatara.buffer:find-buffer name)
                                                               (tuatara.buffer:make-buffer name))))))))
    (true (find-if (lambda (title) (search "bad\\xFFname.txt" title :test #'char-equal))
                   (tuatara.frontend.fake:fake-titles f))
          "titles ~s" (tuatara.frontend.fake:fake-titles f))
    (let ((buffer (tuatara.buffer:find-buffer name))) (when buffer (tuatara.buffer:kill-buffer buffer)))))

(define-test (command prefix-keys-made-alike)
  ;; binding a key and labelling one reach and make their prefix keys the same way
  (let ((map (tuatara.command:make-keymap :name "test")))
    (tuatara.command:set-key-label map "C-c x y" "why")
    (let ((prefix (tuatara.command:lookup-key map (tuatara.frontend:parse-key-sequence "C-c x"))))
      (true (tuatara.command:keymap-p prefix))
      (tuatara.command:define-key map "C-c x z" 'tuatara.defaults::end-of-buffer)
      (is eq prefix (tuatara.command:lookup-key map (tuatara.frontend:parse-key-sequence "C-c x"))))
    (is equal "why"
        (tuatara.command:key-label (tuatara.command:lookup-key map (tuatara.frontend:parse-key-sequence "C-c x"))
                                   (first (tuatara.frontend:parse-key-sequence "y"))))
    ;; a key bound to a command: binding under it makes it a prefix, labelling under it is refused
    (tuatara.command:define-key map "C-c q" 'tuatara.defaults::end-of-buffer)
    (fail (tuatara.command:set-key-label map "C-c q r" "no"))
    (tuatara.command:define-key map "C-c q r" 'tuatara.defaults::end-of-buffer)
    (true (tuatara.command:keymap-p (tuatara.command:lookup-key map (tuatara.frontend:parse-key-sequence "C-c q"))))))

(define-test (command a-test-leaves-a-buffer)
  ;; with BUFFERS-OF-A-TEST-KILLED: a buffer a test made, not killed by it
  (true (tuatara.buffer:make-buffer "left by a test")))

(define-test (command buffers-of-a-test-killed)
  :depends-on (a-test-leaves-a-buffer)
  ;; the buffer the test before made and left is gone
  (false (tuatara.buffer:find-buffer "left by a test")))

(define-modal-test (command insert-escape-keys)
  (let ((tuatara-user:*insert-escape-keys* "jk"))
    (flet ((run (keys &optional (text ""))
             (let ((editor (nth-value 1 (run-keys keys :text text :width 60 :height 8))))
               (values (session-text editor) (tuatara.defaults:modal-state (session-buffer editor)) editor))))
      ;; the pair leaves INSERT state and neither letter stays; what came before stays
      (multiple-value-bind (text state) (run "i a b j k")
        (is string= "ab" text)
        (is eq :normal state))
      ;; the first letter goes in at once; another key after it is no pair
      (multiple-value-bind (text state) (run "i j a k")
        (is string= "jak" text)
        (is eq :insert state))
      ;; the second letter alone, or a different order, is text
      (is string= "kj" (run "i k j"))
      ;; one undo takes back the session, with no step for the stray letter
      (is string= "" (run "i a b j k u"))
      (is string= "a" (run "i a C-g i b j k u"))
      (is string= "" (run "i a C-g i j k u"))
      (is string= "x" (run "i j k u" "x"))
      ;; point is where the first letter was typed
      (is = 1 (session-point (nth-value 2 (run "i a j k"))))
      ;; too slow: both stay
      (let ((tuatara.defaults::*insert-escape-window* 0))
        (multiple-value-bind (text state) (run "i j k")
          (is string= "jk" text)
          (is eq :insert state)))
      ;; the same letters twice
      (let ((tuatara-user:*insert-escape-keys* "jj"))
        (is string= "" (run "i j j")))
      ;; recorded keys play back the same
      (multiple-value-bind (text state) (run "C-x ( i a j k C-x ) C-x e")
        (is string= "aa" text)
        (is eq :normal state))
      ;; a typed answer in the minibuffer takes both letters
      (multiple-value-bind (f editor) (run-keys "M-x j k" :text "" :width 60 :height 8)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "M-x jk")))
      ;; in NORMAL state j and k are moves
      (is string= "abab" (run "j k i a b C-g" "ab"))))
  ;; off by default
  (let ((tuatara-user:*insert-escape-keys* nil))
    (multiple-value-bind (f editor) (run-keys "i j k" :text "" :width 60 :height 8)
      (declare (ignore f))
      (is string= "jk" (session-text editor))
      (is eq :insert (tuatara.defaults:modal-state (session-buffer editor))))))

(define-test (command quit-after-a-prefix-key)
  ;; C-g after a prefix key gives the sequence up, as it does alone, in every prefix map
  (dolist (keys '("C-x C-g" "C-x 5 C-g" "C-x w C-g"))
    (let ((f (run-keys keys :text "" :width 60 :height 8)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Quit") "~a" keys)
      (false (tuatara.frontend.fake:fake-frame-showing f "is undefined") "~a" keys))))

(define-modal-test (command prefix-key-quit-touches-nothing-else)
  ;; C-g after a prefix key gives up the keys typed and leaves the rest as it was
  (let ((editor (nth-value 1 (run-keys "i C-SPC M-f C-x C-g" :text "one two" :width 60 :height 8))))
    (true (tuatara.buffer:buffer-mark-active-p (session-buffer editor)))
    (is eq :insert (tuatara.defaults:modal-state (session-buffer editor))))
  (let ((editor (nth-value 1 (run-keys "i C-x ( a C-x C-g b C-x ) C-x e" :text "" :width 60 :height 8))))
    (is string= "abab" (session-text editor))))

(define-test (command fresh-question-gone-once-answered)
  ;; a question that comes unasked leaves the echo area once answered, as one a command asks does
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 60 :height 8))
        (said :unset))
    (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (declare (ignore f)) nil))
    (queue-test-keys f (list (lambda ()
                               (tuatara.command:y-or-n-p* "Carry on?" :fresh t)
                               (setf said (tuatara.command:editor-message tuatara.command:*editor*)))
                             "y"))
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
    (false said)))

(define-test (command view-change-functions-called-when-the-view-moves)
  ;; a function of *VIEW-CHANGE-FUNCTIONS* is called as a window comes to show other lines, not on every frame
  (flet ((calls (keys)
           (let* ((calls 0)
                  (tuatara.command:*view-change-functions*
                    (list (lambda (editor window rows) (declare (ignore editor window rows)) (incf calls)))))
             (run-keys keys :text (format nil "~{line ~d~%~}" (loop for i below 50 collect i)) :height 8)
             calls)))
    (let ((still (calls "C-f")))
      (true (plusp still))
      (is = still (calls "C-f C-f C-n C-f"))
      (true (> (calls "C-v") still)))))

(define-test (command later-answer-taken-while-the-question-stands)
  ;; an answer given now is taken at once; one to come, when it comes, unless the buffer changed meanwhile
  (with-test-buffer (b "*later*" :text "abc")
    (let ((tuatara.command::*current-buffer* b) (taken '()) (then nil))
      (flet ((take (&rest values) (push values taken))
             (ask () (tuatara.command:later (lambda (answer) (setf then answer)))))
        (tuatara.command:take-answer b #'take 1 2)
        (is equal '((1 2)) taken)
        (tuatara.command:take-answer b #'take (ask))
        (is equal '((1 2)) taken)
        (funcall then 3 4)
        (is equal '((3 4) (1 2)) taken)
        (tuatara.command:take-answer b #'take (ask))
        (tuatara.buffer:buffer-insert b 0 "x")
        (funcall then 5)
        (is equal '((3 4) (1 2)) taken)))))
