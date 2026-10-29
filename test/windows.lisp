;;;; test/windows.lisp --- Windows: splitting, closing, the focus, swaps, rotations, sizes, where buffers go.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test windows
  :description "Windows one above another and side by side, each with its point, start and mode line; closing them; moving the focus; swapping and rotating; sizes; where help and *Messages* are shown and how q puts them away.")

(defparameter *twenty-lines* (format nil "~{line ~a~%~}" (loop for i from 1 to 20 collect i))
  "Twenty numbered lines of text.")

(defun window-buffers (editor)
  "The names of the buffers EDITOR's windows show, from the top left."
  (mapcar (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w)))
          (tuatara.command:editor-windows editor)))

(defun window-boxes (editor)
  "Each window of EDITOR as (LEFT TOP WIDTH HEIGHT), from the top left."
  (mapcar (lambda (w) (list (tuatara.display:area-left w) (tuatara.display:area-top w)
                            (tuatara.display:area-width w) (tuatara.display:area-height w)))
          (tuatara.command:editor-windows editor)))

(defun selected-index (editor)
  "The place of EDITOR's selected window among its windows, from the top left."
  (position (tuatara.command:editor-window editor) (tuatara.command:editor-windows editor)))

(defun run-keys-placing (keys rules &key (text "") (width 80) (height 20))
  "RUN-KEYS with the rules of where buffers go RULES, each the arguments of a
PLACE-BUFFERS call, made after the defaults."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text text)
    (tuatara.frontend.fake:fake-queue-keys f keys)
    (let ((editor (tuatara.command:run-editor
                   f :setup (lambda (editor)
                              (tuatara.defaults:install-defaults editor)
                              (dolist (rule rules) (apply #'tuatara-user:place-buffers rule)))
                     :buffer (tuatara.buffer:find-buffer "*scratch*"))))
      (values f editor))))

(define-test (windows split-below)
  (multiple-value-bind (f editor) (run-keys "C-x 2" :text *twenty-lines* :width 40 :height 12)
    ;; eleven rows above the echo area: the upper window keeps the larger half
    (is equal '((0 0 40 6) (0 6 40 5)) (window-boxes editor))
    (is = 0 (selected-index editor))
    (is equal '("*scratch*" "*scratch*") (window-buffers editor))
    (is string= "line 1" (tuatara.frontend.fake:fake-screen-line f 0))
    (is string= "line 1" (tuatara.frontend.fake:fake-screen-line f 6))
    ;; each window its mode line: the selected one's in :mode-line
    (true (search "*scratch*" (tuatara.frontend.fake:fake-screen-line f 5)))
    (is eq :mode-line (aref (tuatara.frontend.fake:fake-row-faces f) 5))
    (is eq :mode-line-inactive (aref (tuatara.frontend.fake:fake-row-faces f) 10))
    (is equal '(0 . 0) (tuatara.frontend.fake:fake-cursor f)))
  ;; a prefix argument gives the upper window that many rows
  (multiple-value-bind (f editor) (run-keys "C-u 7 C-x 2" :text *twenty-lines* :width 40 :height 12)
    (declare (ignore f))
    (is equal '((0 0 40 7) (0 7 40 4)) (window-boxes editor)))
  ;; no room for two windows of four rows: said, and nothing split
  (multiple-value-bind (f editor) (run-keys "C-x 2" :width 40 :height 8)
    (is = 1 (length (tuatara.command:editor-windows editor)))
    (true (search "Not enough room" (tuatara.frontend.fake:fake-screen-line f 7)))))

(define-test (windows split-right)
  (multiple-value-bind (f editor) (run-keys "C-x 3" :text *twenty-lines* :width 40 :height 8)
    (is equal '((0 0 20 7) (20 0 20 7)) (window-boxes editor))
    ;; the left window's last column is the divider; the mode lines share the row
    (is string= "line 1             │line 1" (tuatara.frontend.fake:fake-screen-line f 0))
    ;; the right window, not selected, shows its point as a cursor the typing does not go to
    (is equal '((19 20 :vertical-border) (20 21 :cursor-inactive)) (aref (tuatara.frontend.fake:fake-spans f) 0))
    ;; each window's mode line in its face, the sign of work, P, faint in each
    (is equal '((0 12 :mode-line) (12 13 (:mode-line-sign-idle :mode-line)) (13 20 :mode-line)
                (20 32 :mode-line-inactive) (32 33 (:mode-line-sign-idle :mode-line-inactive)) (33 40 :mode-line-inactive))
        (aref (tuatara.frontend.fake:fake-spans f) 6))
    (is string= " TUATARA[N] P [-] *s TUATARA[N] P [-] *s" (tuatara.frontend.fake:fake-screen-line f 6))))

(define-test (windows own-point)
  ;; the upper window moves; the lower one keeps its point, line and mode line
  (multiple-value-bind (f editor) (run-keys "C-x 2 j j l" :text *twenty-lines* :width 40 :height 12)
    (declare (ignore editor))
    (true (search "L3 C1" (tuatara.frontend.fake:fake-screen-line f 5)))
    (true (search "L1 C0" (tuatara.frontend.fake:fake-screen-line f 10)))
    (is equal '(2 . 1) (tuatara.frontend.fake:fake-cursor f)))
  ;; the focus moves down: the lower window's point is where it was, and moves alone
  (multiple-value-bind (f editor) (run-keys "C-x 2 j j l C-x o j" :text *twenty-lines* :width 40 :height 12)
    (is = 1 (selected-index editor))
    (true (search "L3 C1" (tuatara.frontend.fake:fake-screen-line f 5)))
    (true (search "L2 C0" (tuatara.frontend.fake:fake-screen-line f 10)))
    (is equal '(7 . 0) (tuatara.frontend.fake:fake-cursor f))
    (is = 7 (session-point editor)))
  ;; text typed in one window shows in the other, whose point follows its own text
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-x o j j C-x o i X X C-g" :text *twenty-lines* :width 40 :height 12)
    (declare (ignore editor))
    (is string= "XXline 1" (tuatara.frontend.fake:fake-screen-line f 0))
    (is string= "XXline 1" (tuatara.frontend.fake:fake-screen-line f 6))
    (true (search "L3 C0" (tuatara.frontend.fake:fake-screen-line f 10)))))

(define-test (windows other-window)
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-x 2" :width 40 :height 20)
    (declare (ignore f))
    (is equal '((0 0 40 5) (0 5 40 5) (0 10 40 9)) (window-boxes editor)))
  (dolist (case '(("C-x 2 C-x 2 C-x o" 1) ("C-x 2 C-x 2 C-x o C-x o" 2) ("C-x 2 C-x 2 C-x o C-x o C-x o" 0)
                  ("C-x 2 C-x 2 C-u - 1 C-x o" 2) ("C-x 2 C-x 2 SPC SPC" 1) ("C-x 2 C-x 2 SPC g SPC g" 2)
                  ("C-x 2 C-x 2 SPC w w" 1)))
    (destructuring-bind (keys index) case
      (multiple-value-bind (f editor) (run-keys keys :width 40 :height 20)
        (declare (ignore f))
        (is = index (selected-index editor) "~a" keys)))))

(define-test (windows delete)
  (dolist (keys '("C-x 2 C-x 0" "C-x 2 C-x 3 C-x 1" "C-x 2 C-x 3 F" "C-x 3 C-x o SPC k K" "C-x 2 SPC w o" "C-x 2 SPC w q"))
    (multiple-value-bind (f editor) (run-keys keys :text *twenty-lines* :width 40 :height 12)
      (declare (ignore f))
      (is equal '((0 0 40 11)) (window-boxes editor) "~a" keys)))
  ;; the room of a window closed goes to the one above it; the focus to the one used before
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-x 2 C-x o C-x o C-x o C-x o C-x 0" :width 40 :height 20)
    (declare (ignore f))
    (is equal '((0 0 40 10) (0 10 40 9)) (window-boxes editor))
    (is = 0 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 0" :width 40 :height 8)
    (is = 1 (length (tuatara.command:editor-windows editor)))
    (true (search "only window" (tuatara.frontend.fake:fake-screen-line f 7)))))

(define-test (windows directions)
  (dolist (case '(("C-x 3 C-M-l" 1) ("C-x 3 C-M-l C-M-h" 0) ("C-x 2 C-M-j" 1) ("C-x 2 C-M-j C-M-k" 0)
                  ("C-x 3 SPC L" 1) ("C-x 3 SPC L SPC H" 0) ("C-x 2 SPC J" 1) ("C-x 2 SPC J SPC K" 0)))
    (destructuring-bind (keys index) case
      (multiple-value-bind (f editor) (run-keys keys :width 40 :height 12)
        (declare (ignore f))
        (is = index (selected-index editor) "~a" keys))))
  ;; of the two windows to the right, the one beside point's line
  (multiple-value-bind (f editor) (run-keys "C-x 3 C-M-l C-x 2 C-M-h j j j j j j j C-M-l"
                                            :text *twenty-lines* :width 40 :height 12)
    (declare (ignore f))
    (is equal '((0 0 20 11) (20 0 20 6) (20 6 20 5)) (window-boxes editor))
    (is = 2 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-M-h" :width 40 :height 8)
    (declare (ignore editor))
    (true (search "No window to the left" (tuatara.frontend.fake:fake-screen-line f 7)))))

(define-test (windows swapping-windows)
  ;; the left window shows other, the right *scratch*: SPC w l moves other right, and you with it
  (multiple-value-bind (f editor) (run-keys "C-x 3 C-x b o t h e r C-j SPC w l" :width 40 :height 8)
    (declare (ignore f))
    (is equal '("*scratch*" "other") (window-buffers editor))
    (is = 1 (selected-index editor))
    (is string= "other" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
  (multiple-value-bind (f editor) (run-keys "C-x 3 C-x b o t h e r C-j SPC w l SPC w h" :width 40 :height 8)
    (declare (ignore f))
    (is equal '("other" "*scratch*") (window-buffers editor))
    (is = 0 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-x b o t h e r C-j SPC w j" :width 40 :height 12)
    (declare (ignore f))
    (is equal '("*scratch*" "other") (window-buffers editor))
    (is = 1 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-x b o t h e r C-j SPC w x" :width 40 :height 12)
    (declare (ignore f))
    (is equal '("*scratch*" "other") (window-buffers editor)))
  ;; each window keeps its point as the buffers trade places
  (multiple-value-bind (f editor) (run-keys "C-x 3 j j C-x o C-x b o t h e r C-j C-x o SPC w l"
                                            :text *twenty-lines* :width 80 :height 8)
    (is equal '("other" "*scratch*") (window-buffers editor))
    (true (search "*scratch*  L3" (tuatara.frontend.fake:fake-screen-line f 6)))
    (is = 14 (session-point editor))))

(define-test (windows kill-buffer-and-window)
  ;; C-x 4 0 asks before killing a buffer with changes: no keeps the buffer and its window,
  ;; yes kills it and closes the window
  (let ((tuatara.defaults::*modal-editing* nil))
    (dolist (case '(("n" 2 t) ("y" 1 nil)))
      (destructuring-bind (answer windows kept) case
        (multiple-value-bind (f editor) (run-keys (format nil "C-x 2 C-x b z z k RET a C-x 4 0 ~a RET" answer) :width 40 :height 12)
          (true (tuatara.frontend.fake:fake-frame-showing f "Buffer zzk modified; kill anyway?"))
          (is = windows (length (tuatara.command:editor-windows editor)) "~a" answer)
          (is eq kept (and (tuatara.buffer:find-buffer "zzk") t) "~a" answer)
          (let ((left (tuatara.buffer:find-buffer "zzk")))
            (when left (tuatara.buffer:kill-buffer left))))))))

(define-test (windows rotate)
  (let ((setup "C-x 2 C-x 2 C-x b z z 1 RET C-x o C-x b z z 2 RET C-x o C-x b z z 3 RET"))
    (multiple-value-bind (f editor) (run-keys setup :width 40 :height 20)
      (declare (ignore f))
      (is equal '("zz1" "zz2" "zz3") (window-buffers editor))
      (is = 2 (selected-index editor)))
    ;; the buffers move one window on, and you with yours
    (multiple-value-bind (f editor) (run-keys (format nil "~a SPC w r" setup) :width 40 :height 20)
      (declare (ignore f))
      (is equal '("zz3" "zz1" "zz2") (window-buffers editor))
      (is = 0 (selected-index editor)))
    (multiple-value-bind (f editor) (run-keys (format nil "~a SPC w R" setup) :width 40 :height 20)
      (declare (ignore f))
      (is equal '("zz2" "zz3" "zz1") (window-buffers editor))
      (is = 1 (selected-index editor))))
  ;; rows become columns and back, each window keeping its share: six rows of
  ;; eleven and five, then 22 columns of 40 and 18
  (multiple-value-bind (f editor) (run-keys "C-x 2 SPC w t" :width 40 :height 12)
    (declare (ignore f))
    (is equal '((0 0 22 11) (22 0 18 11)) (window-boxes editor))
    (is = 0 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 2 SPC w t SPC w t" :width 40 :height 12)
    (declare (ignore f))
    (is equal '((0 0 40 6) (0 6 40 5)) (window-boxes editor)))
  ;; a quarter turn clockwise: the upper window goes to the right, the left one to the top
  (multiple-value-bind (f editor) (run-keys "C-x 2 SPC w ." :width 40 :height 12)
    (declare (ignore f))
    (is = 1 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 3 SPC w ." :width 40 :height 12)
    (declare (ignore f))
    (is equal '((0 0 40 6) (0 6 40 5)) (window-boxes editor))
    (is = 0 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 3 SPC w ," :width 40 :height 12)
    (declare (ignore f))
    (is = 1 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 3 M-x f l i p - w i n d o w - l a y o u t - h o r i z o n t a l l y RET"
                                            :width 40 :height 12)
    (declare (ignore f))
    (is = 1 (selected-index editor))))

(define-test (windows sizes)
  (dolist (case '(("C-x 2 C-x ^" ((0 0 40 7) (0 7 40 4)))
                  ;; the lower window has no more than its four rows to give
                  ("C-x 2 C-u 3 C-x ^" ((0 0 40 7) (0 7 40 4)))
                  ("C-x 2 C-x o C-x ^" ((0 0 40 5) (0 5 40 6)))
                  ("C-x 2 SPC w -" ((0 0 40 5) (0 5 40 6)))
                  ("C-x 2 C-x ^ C-x +" ((0 0 40 6) (0 6 40 5)))
                  ("C-x 3 C-x }" ((0 0 21 11) (21 0 19 11)))
                  ("C-x 3 C-x {" ((0 0 19 11) (19 0 21 11)))
                  ("C-x 3 C-u 5 SPC w >" ((0 0 25 11) (25 0 15 11)))))
    (destructuring-bind (keys boxes) case
      (multiple-value-bind (f editor) (run-keys keys :width 40 :height 12)
        (declare (ignore f))
        (is equal boxes (window-boxes editor) "~a" keys))))
  ;; as short as the buffer's lines need (seven and the mode line), but never under four rows
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-x o C-x -" :text (format nil "a~%b~%c~%d~%e~%f~%g") :width 40 :height 20)
    (declare (ignore f))
    (is equal '((0 0 40 11) (0 11 40 8)) (window-boxes editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-x -" :text (format nil "a~%b") :width 40 :height 20)
    (declare (ignore f))
    (is equal '((0 0 40 4) (0 4 40 15)) (window-boxes editor)))
  ;; the screen grows: the windows keep their shares, 6/11 of 23 rows being 12.5
  (multiple-value-bind (f editor) (run-keys "C-x 2" :width 40 :height 12)
    (declare (ignore f))
    (tuatara.command:resize editor 40 24)
    (is equal '((0 0 40 13) (0 13 40 10)) (window-boxes editor))))

(define-test (windows panel)
  ;; a taller panel takes its rows from the window at the bottom only
  (multiple-value-bind (f editor) (run-keys "C-x 2" :width 40 :height 20)
    (declare (ignore f))
    (is = 16 (tuatara.command:fit-windows editor 4))
    (is equal '((0 0 40 10) (0 10 40 6)) (window-boxes editor))
    (is = 12 (tuatara.command:fit-windows editor 8))
    (is equal '((0 0 40 10) (0 10 40 2)) (window-boxes editor))
    ;; when the bottom one has nothing more to give, the one above gives
    (is = 10 (tuatara.command:fit-windows editor 10))
    (is equal '((0 0 40 8) (0 8 40 2)) (window-boxes editor))
    (is = 19 (tuatara.command:fit-windows editor 1))
    (is equal '((0 0 40 10) (0 10 40 9)) (window-boxes editor)))
  ;; side by side, both give
  (multiple-value-bind (f editor) (run-keys "C-x 3" :width 40 :height 20)
    (declare (ignore f))
    (tuatara.command:fit-windows editor 5)
    (is equal '((0 0 20 15) (20 0 20 15)) (window-boxes editor))))

(define-test (windows help-and-messages)
  ;; help: in a window split off below, not selected; q there closes it
  (multiple-value-bind (f editor) (run-keys "C-h f u n d o RET" :text *twenty-lines* :width 80 :height 20)
    (is equal '("*scratch*" "*Help*") (window-buffers editor))
    (is = 0 (selected-index editor))
    (is equal '((0 0 80 10) (0 10 80 9)) (window-boxes editor))
    (is string= "undo is a command." (tuatara.frontend.fake:fake-screen-line f 10)))
  (multiple-value-bind (f editor) (run-keys "C-h f u n d o RET C-x o q" :text *twenty-lines* :width 80 :height 20)
    (declare (ignore f))
    (is equal '("*scratch*") (window-buffers editor)))
  ;; help again: the same window
  (multiple-value-bind (f editor) (run-keys "C-h f u n d o RET C-h k C-x C-f" :width 80 :height 20)
    (is equal '("*scratch*" "*Help*") (window-buffers editor))
    (true (search "find-file" (tuatara.frontend.fake:fake-screen-line f 10))))
  ;; a wide screen: beside
  (multiple-value-bind (f editor) (run-keys "C-h f u n d o RET" :width 200 :height 20)
    (declare (ignore f))
    (is equal '((0 0 100 19) (100 0 100 19)) (window-boxes editor)))
  ;; *Messages*: across the bottom, 30% of the rows, selected, kept for it; q closes it
  (multiple-value-bind (f editor) (run-keys "C-h e" :width 80 :height 20)
    (declare (ignore f))
    (is equal '("*scratch*" "*Messages*") (window-buffers editor))
    (is equal '((0 0 80 13) (0 13 80 6)) (window-boxes editor))
    (is = 1 (selected-index editor)))
  (multiple-value-bind (f editor) (run-keys "C-h e q" :width 80 :height 20)
    (declare (ignore f))
    (is equal '("*scratch*") (window-buffers editor))
    (is = 0 (selected-index editor)))
  ;; the window kept for *Messages* is not given another buffer: C-x b there shows it elsewhere
  (multiple-value-bind (f editor) (run-keys "C-h e C-x b o t h e r C-j" :width 80 :height 20)
    (declare (ignore f))
    (is equal '("other" "*Messages*") (window-buffers editor))
    (is = 0 (selected-index editor))))

(define-test (windows placing-buffers)
  (dolist (case '(((:help :here) ((0 0 80 19)) ("*Help*"))
                  ((:help :right :size 0.4) ((0 0 48 19) (48 0 32 19)) ("*scratch*" "*Help*"))
                  ((:help :beside) ((0 0 40 19) (40 0 40 19)) ("*scratch*" "*Help*"))
                  ((:help :below :size 5) ((0 0 80 13) (0 13 80 6)) ("*scratch*" "*Help*"))
                  ((:help :nowhere) ((0 0 80 19)) ("*scratch*"))
                  (("*Help*" :bottom :size :fit) ((0 0 80 11) (0 11 80 8)) ("*scratch*" "*Help*"))
                  ((help-mode :bottom :size 0.5 :select t) ((0 0 80 9) (0 9 80 10)) ("*scratch*" "*Help*"))))
    (destructuring-bind (rule boxes buffers) case
      (multiple-value-bind (f editor) (run-keys-placing "C-h f u n d o RET" (list rule))
        (declare (ignore f))
        (is equal boxes (window-boxes editor) "~s" rule)
        (is equal buffers (window-buffers editor) "~s" rule))))
  ;; the rule made last counts
  (multiple-value-bind (f editor) (run-keys-placing "C-h f u n d o RET" '((:help :here) ((:help) :beside)))
    (declare (ignore f))
    (is = 2 (length (tuatara.command:editor-windows editor))))
  (fail (tuatara-user:place-buffers :help :somewhere))
  (fail (tuatara-user:place-buffers :help :here :size -1))
  (fail (tuatara-user:place-buffers 42 :here)))

(define-test (windows killing-buffers)
  ;; both windows show other: killing it shows in each the buffer it showed before
  (multiple-value-bind (f editor) (run-keys "C-x 3 C-x b o t h e r C-j C-x o C-x b o t h e r RET C-x k"
                                            :width 40 :height 8)
    (declare (ignore f))
    (is equal '("*scratch*" "*scratch*") (window-buffers editor))
    (false (tuatara.buffer:find-buffer "other")))
  ;; a window made for help closes with its buffer
  (multiple-value-bind (f editor) (run-keys "C-h f u n d o RET C-x o C-x k" :width 80 :height 20)
    (declare (ignore f))
    (is equal '("*scratch*") (window-buffers editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 2 SPC k k" :width 40 :height 12)
    (declare (ignore f))
    (is = 1 (length (tuatara.command:editor-windows editor)))
    (false (tuatara.buffer:find-buffer "*scratch*"))))

(define-test (windows other-window-commands)
  ;; C-x 4 b: another window, made below a lone one, and you there
  (multiple-value-bind (f editor) (run-keys "C-x 4 b z z 9 RET" :width 80 :height 20)
    (declare (ignore f))
    (is equal '("*scratch*" "zz9") (window-buffers editor))
    (is = 1 (selected-index editor)))
  ;; C-x 4 f: a file in another window
  (with-test-directory (directory)
    (let ((path (put-file (merge-pathnames "other-window.txt" directory) (format nil "from the file~%"))))
      (multiple-value-bind (f editor) (run-keys (format nil "C-x 4 f ~a RET" (key-string (namestring path))) :width 80 :height 20)
        (is equal '("*scratch*" "other-window.txt") (window-buffers editor))
        (is = 1 (selected-index editor))
        (is string= "from the file" (tuatara.frontend.fake:fake-screen-line f 10))
        (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
  ;; C-M-v scrolls the window below by its rows less two; yours stays
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-M-v" :text *twenty-lines* :width 40 :height 12)
    (declare (ignore editor))
    (is string= "line 1" (tuatara.frontend.fake:fake-screen-line f 0))
    (is string= "line 3" (tuatara.frontend.fake:fake-screen-line f 6)))
  ;; run by name, the window below scrolls by its whole height, not by what
  ;; the candidates of M-x left it
  (let ((text (format nil "~{line ~a~%~}" (loop for i from 1 to 100 collect i))))
    (flet ((top-below (keys)
             (tuatara.frontend.fake:fake-screen-line (run-keys keys :text text :width 80 :height 30) 15)))
      (is string= "line 34" (top-below "C-x 2 C-M-v C-M-v C-M-v"))
      (is string= "line 23" (top-below "C-x 2 C-M-v C-M-v C-M-v M-x s c r o l l - o t h e r - w i n d o w - d o w n RET"))
      ;; C-M-S-v scrolls it back, as the keyboard gives it: C-M-V
      (is string= "line 23" (top-below "C-x 2 C-M-v C-M-v C-M-v C-M-S-v"))
      (is string= "line 23" (top-below "C-x 2 C-M-v C-M-v C-M-v C-M-V"))
      (is string= "line 45" (top-below "C-x 2 C-M-v C-M-v C-M-v M-x s c r o l l - o t h e r - w i n d o w RET"))
      ;; with a count, that many rows, the other way when it is negative
      (is string= "line 15" (top-below "C-x 2 C-M-v C-u 3 C-M-v"))
      (is string= "line 9" (top-below "C-x 2 C-M-v C-u - 3 C-M-v"))
      (is string= "line 10" (top-below "C-x 2 C-M-v C-u 2 C-M-S-v")))
    (is string= "line 4" (tuatara.frontend.fake:fake-screen-line (run-keys "C-u 3 C-v" :text text :width 80 :height 30) 0))
    (is string= "line 2" (tuatara.frontend.fake:fake-screen-line (run-keys "C-u 3 C-v C-u 2 M-v" :text text :width 80 :height 30) 0))))

(define-test (windows history)
  ;; a buffer shown again in a window comes back from the same line, not from its top
  (multiple-value-bind (f editor) (run-keys "C-u 1 4 j z z C-x b o t h e r C-j C-x b * s c r a t c h * RET"
                                            :text *twenty-lines* :width 40 :height 8)
    (is string= "line 12" (tuatara.frontend.fake:fake-screen-line f 0))
    (is = (length (format nil "~{line ~a~%~}" (loop for i from 1 to 14 collect i))) (session-point editor))))

(define-test (windows layout)
  (let* ((buffer (tuatara.buffer:make-buffer "*window-layout-test*" :text *twenty-lines*))
         (a (tuatara.display:make-window buffer)))
    (unwind-protect
         (progn
           (is equalp #(20 20) (tuatara.display::distribute 40 '(1/2 1/2) '(2 2)))
           (is equalp #(3 4 3) (tuatara.display::distribute 10 '(1/3 1/3 1/3) '(2 2 2)))
           ;; a share too small for the least is raised, taken from the biggest
           (is equalp #(4 16) (tuatara.display::distribute 20 '(1/20 19/20) '(4 2)))
           (tuatara.display:layout-area a 0 0 80 24 80)
           (let ((b (tuatara.display:make-window buffer))
                 (c (tuatara.display:make-window buffer)))
             (tuatara.display:split-area a :right b)
             (tuatara.display:layout-area (tuatara.display:area-root a) 0 0 80 24 80)
             (tuatara.display:split-area b :below c)
             (let ((root (tuatara.display:area-root a)))
               (tuatara.display:layout-area root 0 0 80 24 80)
               (is equal (list a b c) (tuatara.display:area-windows root))
               (is equal '(0 40 40) (mapcar #'tuatara.display:area-left (list a b c)))
               (is equal '(0 0 12) (mapcar #'tuatara.display:area-top (list a b c)))
               ;; a window beside another gives its last column to the divider
               (is = 39 (tuatara.display:window-width a))
               (is = 40 (tuatara.display:window-width b))
               (is eq b (tuatara.display:window-in-direction a :right :row 3))
               (is eq c (tuatara.display:window-in-direction a :right :row 20))
               (is eq a (tuatara.display:window-in-direction c :left))
               (is eq b (tuatara.display:window-in-direction c :above))
               (false (tuatara.display:window-in-direction c :below))
               ;; closing b: c takes its room, the split of one collapses
               (let ((root (tuatara.display:delete-area b)))
                 (tuatara.display:layout-area root 0 0 80 24 80)
                 (is equal (list a c) (tuatara.display:area-windows root))
                 (is equal '(40 0 40 24) (list (tuatara.display:area-left c) (tuatara.display:area-top c)
                                              (tuatara.display:area-width c) (tuatara.display:area-height c)))))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (windows return-and-control-j)
  ;; in the terminal the return key and C-j are two keys
  (is equal :enter (tuatara.frontend:key-base (tuatara.frontend.tui::char-key 13)))
  (is equal '(:ctrl #\j) (tuatara.frontend:key-spec (tuatara.frontend.tui::char-key 10)))
  (is equal '(:ctrl :meta #\j) (tuatara.frontend:key-spec (tuatara.frontend.tui::char-key 10 :meta t)))
  ;; 28 to 31 are C-\ C-] C-^ C-_, the last what C-/ sends
  (is equal '((:ctrl #\\) (:ctrl #\]) (:ctrl #\^) (:ctrl #\_))
      (loop for code from 28 to 31 collect (tuatara.frontend:key-spec (tuatara.frontend.tui::char-key code)))))

(define-test (windows repeating-sizes)
  ;; after C-x {, { alone narrows again, and again
  (dolist (case '(("C-x 3 C-x { { {" ((0 0 17 11) (17 0 23 11)))
                  ("C-x 3 C-x { . ." ((0 0 17 11) (17 0 23 11)))
                  ("C-x 3 C-u 2 C-x { ." ((0 0 16 11) (16 0 24 11)))
                  ("C-x 3 C-x } } { }" ((0 0 22 11) (22 0 18 11)))
                  ;; any other key ends it, and does what it always does
                  ("C-x 3 C-x { l {" ((0 0 19 11) (19 0 21 11)))))
    (destructuring-bind (keys boxes) case
      (multiple-value-bind (f editor) (run-keys keys :text "abc" :width 40 :height 12)
        (declare (ignore f))
        (is equal boxes (window-boxes editor) "~a" keys))))
  (multiple-value-bind (f editor) (run-keys "C-x 2 SPC w + + +" :width 40 :height 20)
    (declare (ignore f))
    (is equal '((0 0 40 13) (0 13 40 6)) (window-boxes editor)))
  (multiple-value-bind (f editor) (run-keys "C-x 3 C-x {" :width 40 :height 12)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Repeat with {, }, ^, v"))))

(define-test (windows repeat-again)
  ;; . repeats the last command as often as it is pressed
  (multiple-value-bind (f editor) (run-keys "j . . ." :text *twenty-lines*)
    (declare (ignore f))
    (is = (length (format nil "~{line ~a~%~}" '(1 2 3 4))) (session-point editor))))

(define-test (windows one-window-from-a-support-buffer)
  ;; F in a support buffer, as in NORMAL state: its window alone on the screen
  (multiple-value-bind (f editor) (run-keys "C-x 2 C-h e F" :text *twenty-lines* :width 40 :height 20)
    (declare (ignore f))
    (is string= "*Messages*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
    (is = 1 (length (tuatara.command:editor-windows editor)))))

(define-test (windows centred-text)
  ;; the text in a column of *centred-text-width* columns in the middle of the window, blank either side
  (let ((tuatara-user:*centred-text-width* 20))
    (multiple-value-bind (f editor) (run-keys "M-x c e n t r e d - t e x t - m o d e RET" :text (format nil "hello world~%second line~%") :width 60 :height 8)
      (declare (ignore editor))
      ;; 60 columns less the column's 20 and the one between the margin and the text: 19 each side
      (is string= (make-string 20 :initial-element #\Space) (subseq (tuatara.frontend.fake:fake-screen-line f 0) 0 20))
      (is string= "hello world" (string-right-trim " " (subseq (tuatara.frontend.fake:fake-screen-line f 0) 20)))
      (is string= "second line" (string-right-trim " " (subseq (tuatara.frontend.fake:fake-screen-line f 1) 20)))
      (is equal '(0 . 20) (tuatara.frontend.fake:fake-cursor f))
      (true (tuatara.frontend.fake:fake-frame-showing f "Centred text on"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Centred")))
    ;; a long line is cut at the column's edge, and wraps there with soft wrap
    (let ((long (format nil "~a~%" (make-string 50 :initial-element #\x))))
      (let ((f (run-keys "M-x c e n t r e d - t e x t - m o d e RET" :text long :width 60 :height 8)))
        (is string= (make-string 20 :initial-element #\x) (string-trim " " (tuatara.frontend.fake:fake-screen-line f 0))))
      (let ((f (run-keys "M-x c e n t r e d - t e x t - m o d e RET M-x s o f t - w r a p - m o d e RET" :text long :width 60 :height 8)))
        (is string= (make-string 20 :initial-element #\x) (string-trim " " (tuatara.frontend.fake:fake-screen-line f 0)))
        (is string= (make-string 20 :initial-element #\x) (string-trim " ↪" (tuatara.frontend.fake:fake-screen-line f 1)))
        (is string= (make-string 10 :initial-element #\x) (string-trim " ↪" (tuatara.frontend.fake:fake-screen-line f 2)))))
    ;; point at the end of a long line scrolls the column sideways, not the whole window away
    (let ((f (run-keys "M-x c e n t r e d - t e x t - m o d e RET C-e" :text (format nil "~a~%next~%" (make-string 50 :initial-element #\x)) :width 60 :height 8)))
      (is = 0 (car (tuatara.frontend.fake:fake-cursor f)))
      (true (< (cdr (tuatara.frontend.fake:fake-cursor f)) 41)))
    ;; the mouse: a click in the column lands on its character
    (is = 5 (session-point (nth-value 1 (run-events '("M-x c e n t r e d - t e x t - m o d e RET" (:down-mouse-1 25 0)) :width 60 :height 8))))
    ;; off again; and on by major mode; never in a window too narrow for the column
    (let ((f (run-keys "M-x c e n t r e d - t e x t - m o d e RET M-x c e n t r e d - t e x t - m o d e RET" :text "hello" :width 60 :height 8)))
      (is string= "hello" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Centred text off")))
    (let ((tuatara-user:*centred-text-modes* '((tuatara.defaults::text-mode . t))))
      (let ((f (run-keys "M-x t e x t - m o d e RET" :text "hello" :width 60 :height 8)))
        (is string= "hello" (string-right-trim " " (subseq (tuatara.frontend.fake:fake-screen-line f 0) 20))))
      (let ((f (run-keys "M-x t e x t - m o d e RET" :text "hello" :width 18 :height 8)))
        (is string= "hello" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0)))))))


(define-test (windows font-refused-unless-fixed-width)
  ;; a family whose letters are not all as wide is refused, though the window could show it
  (flet ((chosen (keys)
           (let ((f (tuatara.frontend.fake:make-fake-frontend
                     :width 120 :font-steps 0 :fonts '("DejaVu Sans Mono")
                     :font-faces '(("DejaVu Sans Mono" t (("Regular" "DejaVu Sans Mono 11")))
                                   ("DejaVu Serif" nil (("Regular" "DejaVu Serif 11"))))))
                 (asked '()))
             (sb-int:encapsulate 'tuatara.frontend:frontend-set-font 'any-family
                                 (lambda (function frontend name)
                                   (declare (ignore function))
                                   (push name asked)
                                   (setf (tuatara.frontend.fake:fake-font frontend) name)))
             (unwind-protect
                  (progn
                    (tuatara.frontend.fake:fake-queue-keys f keys)
                    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
                      (when scratch (tuatara.buffer:kill-buffer scratch)))
                    (tuatara.buffer:make-buffer "*scratch*" :text "hello")
                    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                  :buffer (tuatara.buffer:find-buffer "*scratch*"))
                    (values asked (tuatara.frontend.fake:fake-screen-line f 7)))
               (sb-int:unencapsulate 'tuatara.frontend:frontend-set-font 'any-family)))))
    (multiple-value-bind (asked echo) (chosen (format nil "M-x s e t - f r a m e - f o n t RET ~a RET" (key-string "DejaVu Serif 12")))
      (is equal '() asked)
      (true (search "DejaVu Serif is no fixed-width font" echo)))
    (is equal '("dejavu sans mono 12")
        (chosen (format nil "M-x s e t - f r a m e - f o n t RET ~a RET" (key-string "dejavu sans mono 12"))))
    (multiple-value-bind (asked echo) (chosen (format nil "M-x s e t - f r a m e - f o n t RET ~a RET" (key-string "DejaVu Sans Monolith 12")))
      (is equal '() asked)
      (true (search "No font family DejaVu Sans Monolith is installed" echo)))))

(define-test (windows shown-for-a-package)
  ;; a buffer shown down the right in a window of its own, once however often asked; found by its
  ;; buffer; its own point moved, the selected window's left; closed again as q closes it
  (let ((side (tuatara.buffer:make-buffer "*side*" :text *twenty-lines*))
        first again found point counts)
    (unwind-protect
         (multiple-value-bind (f editor)
             (run-keys (list :tick
                             (lambda ()
                               (setf first (tuatara.command:display-buffer-at side :right)
                                     again (tuatara.command:display-buffer-at side :right)
                                     found (tuatara.command:buffer-window side))
                               (push (length (tuatara.command:editor-windows tuatara.command:*editor*)) counts)
                               (setf (tuatara.display:window-point first) 7
                                     point (tuatara.display:window-point first))
                               (tuatara.command:leave-window first)
                               (push (length (tuatara.command:editor-windows tuatara.command:*editor*)) counts)))
                       :text *twenty-lines* :width 80 :height 20)
           (declare (ignore f))
           (true first)
           (is eq first again)
           (is eq first found)
           (is = 7 point)
           (is = 0 (session-point editor))
           (is equal '(1 2) counts))
      (tuatara.buffer:kill-buffer side))))

(define-test (windows editor-made-with-one-frame)
  ;; the editor has one frame, made from its frontend, and that frame is current
  (let* ((f (tuatara.frontend.fake:make-fake-frontend))
         (editor (tuatara.command:make-editor f))
         (frame (tuatara.command::editor-frame editor)))
    (true (tuatara.command::frame-p frame))
    (is equal (list frame) (tuatara.command::editor-frames editor))
    (is eq f (tuatara.command::frame-frontend frame))
    (is eq f (tuatara.command:editor-frontend editor))))

(define-test (windows frames-keep-their-own-screen)
  ;; the editor's names of the screen read and set its current frame's state, and
  ;; another frame made current has its own, the first's kept; the kill ring is the editor's
  (let* ((editor (tuatara.command:make-editor (tuatara.frontend.fake:make-fake-frontend)))
         (first (tuatara.command::editor-frame editor))
         (second (tuatara.command::make-frame (tuatara.frontend.fake:make-fake-frontend))))
    (setf (tuatara.command:editor-message editor) "Kererū"
          (tuatara.command:editor-minibuffer-text editor) "Ōhope")
    (incf (tuatara.command::editor-minibuffer-point editor) 3)
    (push '(:ctrl #\x) (tuatara.command:editor-pending editor))
    (push "tītī" (tuatara.command:editor-kill-ring editor))
    (is string= "Kererū" (tuatara.command::frame-message first))
    (is string= "Ōhope" (tuatara.command::frame-minibuffer-text first))
    (is = 3 (tuatara.command::frame-minibuffer-point first))
    (is equal '((:ctrl #\x)) (tuatara.command::frame-pending first))
    (setf (tuatara.command::editor-frames editor) (list first second)
          (tuatara.command::editor-frame editor) second)
    (false (tuatara.command:editor-message editor))
    (false (tuatara.command:editor-minibuffer-text editor))
    (is = 0 (tuatara.command::editor-minibuffer-point editor))
    (is equal '() (tuatara.command:editor-pending editor))
    (is equal '("tītī") (tuatara.command:editor-kill-ring editor))
    (setf (tuatara.command:editor-message editor) "Tūī")
    (is string= "Kererū" (tuatara.command::frame-message first))
    (setf (tuatara.command::editor-frame editor) first)
    (is string= "Kererū" (tuatara.command:editor-message editor))
    (is string= "Ōhope" (tuatara.command:editor-minibuffer-text editor))
    (is equal '((:ctrl #\x)) (tuatara.command:editor-pending editor))
    (is string= "Tūī" (tuatara.command::frame-message second))))

(define-test (windows session-paints-its-frame)
  ;; the screen is painted through the current frame's frontend, with its window:
  ;; another frame made current is painted, the first frame's frontend left alone
  (multiple-value-bind (f editor) (run-keys "" :text "kea")
    (let* ((first (tuatara.command::editor-frame editor))
           (g (tuatara.frontend.fake:make-fake-frontend))
           (second (tuatara.command::make-frame g))
           (painted (tuatara.frontend.fake:fake-frames f)))
      (is eq f (tuatara.command::frame-frontend first))
      (tuatara.frontend:frontend-start g)
      (setf (tuatara.command::frame-window second) (tuatara.command::frame-window first)
            (tuatara.command::frame-width second) (tuatara.command::frame-width first)
            (tuatara.command::frame-height second) (tuatara.command::frame-height first)
            (tuatara.command::editor-frames editor) (list first second)
            (tuatara.command::editor-frame editor) second)
      (let ((tuatara.command:*editor* editor)) (tuatara.command::redisplay editor))
      (is = painted (tuatara.frontend.fake:fake-frames f))
      (is = 1 (tuatara.frontend.fake:fake-frames g))
      (true (loop for row below (tuatara.frontend.fake::fake-height g)
                  thereis (search "kea" (tuatara.frontend.fake:fake-screen-line g row)))))))
