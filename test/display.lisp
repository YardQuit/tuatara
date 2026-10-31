;;;; test/display.lisp --- Layout engine and windows.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test display
  :description "Layout of rows from a snapshot: truncation, wrapping, tabs, wide and control characters, cursor mapping, windows keeping point visible.")

(defun row-strings (rows) "The strings of ROWS as a list." (map 'list #'tuatara.display:row-string rows))

(define-test (display basic-rows)
  (let* ((rope (tuatara.text:string->rope (format nil "abc~%de~%~%last")))
         (rows (tuatara.display:layout rope 0 10 10)))
    (is equal '("abc" "de" "" "last") (row-strings rows))
    (is = 0 (tuatara.display:row-start (aref rows 0)))
    (is = 4 (tuatara.display:row-end (aref rows 0)))
    (is = 4 (tuatara.display:row-start (aref rows 1)))
    (true (tuatara.display:row-last-p (aref rows 3)))
    (false (tuatara.display:row-last-p (aref rows 0)))
    ;; cursor mapping: on a char, on the newline, at the very end
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 1) (is = 0 r) (is = 1 c))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 3) (is = 0 r) (is = 3 c))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 7) (is = 2 r) (is = 0 c))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 12) (is = 3 r) (is = 4 c))
    ;; height limits rows
    (is = 2 (length (tuatara.display:layout rope 0 10 2)))
    ;; a trailing newline shows an empty last row
    (let ((rows (tuatara.display:layout (tuatara.text:string->rope (format nil "x~%")) 0 10 10)))
      (is equal '("x" "") (row-strings rows))
      (true (tuatara.display:row-last-p (aref rows 1))))
    ;; empty text: one empty last row
    (let ((rows (tuatara.display:layout (tuatara.text:make-rope) 0 10 10)))
      (is = 1 (length rows))
      (true (tuatara.display:row-last-p (aref rows 0))))))

(define-test (display truncate-and-wrap)
  (let ((rope (tuatara.text:string->rope (format nil "0123456789abcdef~%short"))))
    (let ((rows (tuatara.display:layout rope 0 5 10)))
      (is equal '("01234" "short") (row-strings rows))
      (is = 17 (tuatara.display:row-end (aref rows 0)))
      (false (tuatara.display:point-row-column rows 8)))   ; truncated text is not visible
    (let ((rows (tuatara.display:layout rope 0 5 10 :wrap t)))
      (is equal '("01234" "56789" "abcde" "f" "short") (row-strings rows))
      (false (tuatara.display:row-continuation-p (aref rows 0)))
      (true (tuatara.display:row-continuation-p (aref rows 1)))
      (false (tuatara.display:row-continuation-p (aref rows 4)))
      (multiple-value-bind (r c) (tuatara.display:point-row-column rows 8) (is = 1 r) (is = 3 c))
      (multiple-value-bind (r c) (tuatara.display:point-row-column rows 16) (is = 3 r) (is = 1 c)))
    ;; horizontal scroll in truncate mode
    (let ((rows (tuatara.display:layout rope 0 5 10 :hscroll 12)))
      (is equal '("cdef" "") (row-strings rows)))))

(define-test (display tabs-wide-and-control)
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope (format nil "a~cb" #\Tab)) 0 20 5)))
    (is string= "a       b" (tuatara.display:row-string (aref rows 0)))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 2) (is = 0 r) (is = 8 c)))
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope "中x") 0 20 5)))
    (is string= "中x" (tuatara.display:row-string (aref rows 0)))
    (is = 3 (tuatara.display:row-columns (aref rows 0)))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 3) (is = 0 r) (is = 2 c)))
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope (format nil "~cx" (code-char 1))) 0 20 5)))
    (is string= "^Ax" (tuatara.display:row-string (aref rows 0)))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 1) (is = 0 r) (is = 2 c)))
  ;; a wide character that does not fit wraps as a whole
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope "ab中") 0 3 5 :wrap t)))
    (is equal '("ab" "中") (row-strings rows)))
  ;; combining mark attaches to the previous glyph without a column
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope (format nil "e~cx" (code-char #x301))) 0 20 5)))
    (is = 2 (tuatara.display:row-columns (aref rows 0)))
    (is = 3 (length (tuatara.display:row-string (aref rows 0))))))

(define-test (display displayable-stand-ins)
  ;; controls, C1 controls among them, and the characters that turn text's direction
  ;; are drawn as ?, a picture's cell character as U+FFFD; the length kept
  (flet ((shown (&rest codes)
           (map 'list #'char-code (tuatara.display:displayable (map 'string #'code-char codes)))))
    (is equal '(97 63 63 98) (shown 97 #x1B #x9B 98))
    (is equal '(63 63 63 63 63 63 63 63 63 63 63 63)
        (shown #x202A #x202B #x202C #x202D #x202E #x2066 #x2067 #x2068 #x2069 #x200E #x200F #x061C))
    (is equal '(120 #xFFFD 121) (shown 120 #x10EEEE 121))
    (is equal '(32 #xFFFD) (shown 9 #xDC80))
    ;; ordinary text, joiners and combining marks pass as they are
    (let ((text (format nil "abc ~c~c中" (code-char #x200D) (code-char #x301))))
      (is eq text (tuatara.display:displayable text)))
    ;; without CONTROLS only what cannot be sent is changed
    (is equal (list #x202E #x10EEEE #x9B) (map 'list #'char-code (tuatara.display:displayable (map 'string #'code-char '(#x202E #x10EEEE #x9B)) :controls nil))))
  ;; a direction character takes a column of its own, where its stand-in is drawn
  (is = 1 (tuatara.display:char-columns (code-char #x200F)))
  (is = 1 (tuatara.display:char-columns (code-char #x202E)))
  (is = 0 (tuatara.display:char-columns (code-char #x200D)))
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope (format nil "a~cb" (code-char #x202E))) 0 20 5)))
    (is = 3 (tuatara.display:row-columns (aref rows 0)))
    (is string= "a?b" (tuatara.display:displayable (tuatara.display:row-string (aref rows 0))))))

(define-test (display window-keeps-point-visible)
  (with-test-buffer (b "*w*" :text (format nil "~{line ~a~%~}" (loop for i from 0 below 100 collect i)))
    (let ((w (tuatara.display:make-window b :width 20 :height 5)))
      (tuatara.display:window-ensure-point-visible w)
      (is = 0 (tuatara.display:window-start w))
      (is equal '("line 0" "line 1" "line 2" "line 3" "line 4") (row-strings (tuatara.display:window-rows w)))
      ;; move point far down: window scrolls so point is on the last row
      (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b 50))
      (tuatara.display:window-ensure-point-visible w)
      (multiple-value-bind (r c) (tuatara.display:window-point-position w) (is = 4 r) (is = 0 c))
      (is = 46 (tuatara.buffer:buffer-line-number b (tuatara.display:window-start w)))
      ;; move point up above the window: point becomes the top row
      (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b 10))
      (tuatara.display:window-ensure-point-visible w)
      (multiple-value-bind (r c) (tuatara.display:window-point-position w) (is = 0 r) (is = 0 c))
      ;; scrolling moves point into the window
      (tuatara.display:window-scroll w 20)
      (is = 30 (tuatara.buffer:buffer-line-number b (tuatara.display:window-start w)))
      (is = 30 (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b)))
      (tuatara.display:window-scroll w -100)
      (is = 0 (tuatara.display:window-start w))
      ;; recenter
      (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b 60))
      (tuatara.display:window-recenter w)
      (is = 58 (tuatara.buffer:buffer-line-number b (tuatara.display:window-start w)))
      ;; horizontal scrolling for a long line in truncate mode
      (tuatara.buffer:buffer-insert b 0 (make-string 50 :initial-element #\z))
      (setf (tuatara.buffer:buffer-point b) 45)
      (tuatara.display:window-ensure-point-visible w)
      (true (plusp (window-hscroll-of w)))
      (true (tuatara.display:window-point-position w)))))

(defun window-hscroll-of (w) "Horizontal scroll of W." (tuatara.display:window-hscroll w))

(define-test (display window-keeps-a-scroll-margin)
  ;; *scroll-margin* rows between point and the edges, recentring as well when *scroll-recenter*
  (with-test-buffer (b "*w*" :text (format nil "~{line ~a~%~}" (loop for i from 0 below 100 collect i)))
    (let ((w (tuatara.display:make-window b :width 20 :height 11))
          (tuatara-user:*scroll-margin* 3))
      (flet ((to-line (n)
               (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b n))
               (tuatara.display:window-ensure-point-visible w)
               (tuatara.buffer:buffer-line-number b (tuatara.display:window-start w)))
             (point-line () (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b))))
        ;; no text above: point may reach the top
        (is = 0 (to-line 0))
        ;; three rows below point: left as it is; two: scrolled one
        (is = 0 (to-line 7))
        (is = 1 (to-line 8))
        ;; far below: three rows below point
        (is = 43 (to-line 50))
        (is = 7 (tuatara.display:window-point-position w))
        ;; within three rows of the top: three rows above point
        (is = 42 (to-line 45))
        ;; at the end of the text: its last row no further up than the window's last
        (is = 90 (to-line 99))
        (true (tuatara.display:row-last-p (let ((rows (tuatara.display:window-rows w))) (aref rows (1- (length rows))))))
        ;; scrolling takes point three rows in, and showing it leaves the window where it was
        (to-line 45)
        (tuatara.display:window-scroll w 20)
        (is = 62 (tuatara.buffer:buffer-line-number b (tuatara.display:window-start w)))
        (is = 65 (point-line))
        (is = 62 (to-line (point-line)))
        (tuatara.display:window-scroll w -10)
        (is = 52 (tuatara.buffer:buffer-line-number b (tuatara.display:window-start w)))
        (is = 59 (point-line))
        (is = 52 (to-line (point-line)))
        ;; recentring: point's line to the middle once it comes within the margin
        (let ((tuatara-user:*scroll-recenter* t))
          (is = 52 (to-line 56))
          (is = 55 (to-line 60))
          (is = 25 (to-line 30))
          (is = 5 (tuatara.display:window-point-position w))
          ;; the margin nothing: only when point leaves the window
          (let ((tuatara-user:*scroll-margin* 0))
            (is = 25 (to-line 35))
            (is = 31 (to-line 36))))))))

(define-test (display scrolling-lays-out-the-new-rows)
  ;; a window following point down a line at a time keeps the rows it has and
  ;; lays out those it moved by: what it shows is what laying it out whole
  ;; shows, and a line moved costs a layout of the window, not two
  (with-test-buffer (b "*w*" :text (format nil "~{~a~%~}" (loop for i below 400
                                                                collect (format nil "line ~d ~a" i (make-string (mod (* i 7) 41) :initial-element #\x)))))
    (flet ((walk (height lines &key wrap check)
             (let ((w (tuatara.display:make-window b :width 30 :height height))
                   (tuatara-user:*scroll-margin* 3))
               ;; from far enough down that every line moved scrolls
               (setf (tuatara.display:window-wrap w) wrap
                     (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b 60))
               (tuatara.display:window-ensure-point-visible w)
               (flet ((step-down ()
                        (setf (tuatara.buffer:buffer-point b)
                              (tuatara.buffer:buffer-goto-line b (1+ (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b)))))
                        (tuatara.display:window-ensure-point-visible w)))
                 (if check
                     (loop repeat lines
                           do (step-down)
                              (let ((kept (tuatara.display:window-rows w))
                                    (whole (tuatara.display:window-layout-rows w (tuatara.display:window-start w) height)))
                                (is = (length whole) (length kept) "rows at ~d" (tuatara.display:window-start w))
                                (loop for k across kept for n across whole
                                      do (is equal (list (tuatara.display:row-start n) (tuatara.display:row-end n)
                                                         (coerce (tuatara.display:row-string n) 'list)
                                                         (coerce (tuatara.display:row-bytes n) 'list)
                                                         (tuatara.display:row-continuation-p n) (tuatara.display:row-last-p n))
                                             (list (tuatara.display:row-start k) (tuatara.display:row-end k)
                                                   (coerce (tuatara.display:row-string k) 'list)
                                                   (coerce (tuatara.display:row-bytes k) 'list)
                                                   (tuatara.display:row-continuation-p k) (tuatara.display:row-last-p k))))))
                     (nth-value 2 (measuring (loop repeat lines do (step-down)))))))))
      (walk 12 300 :check t)
      (walk 12 300 :wrap t :check t)
      ;; each row more of the window, for each line moved, against a layout's for each row more
      (let ((moved (/ (- (walk 40 150) (walk 20 150)) 150 20))
            (laid (/ (- (nth-value 2 (measuring (tuatara.display:layout (tuatara.buffer:buffer-text b) 0 30 40)))
                        (nth-value 2 (measuring (tuatara.display:layout (tuatara.buffer:buffer-text b) 0 30 20))))
                     20)))
        ;; twice a layout's before
        (true (< (/ moved laid) 3/2) "~,2f layouts a line moved" (/ moved laid))))))

(define-test (display frames-keep-their-layout)
  ;; one frame after another, a window's rows are used again: following point a
  ;; line at a time, down or up, lays out the line that came in, not the
  ;; window; what it shows is what laying it out whole shows, and highlights
  ;; painted on its rows are gone from them when they are used again
  (with-test-buffer (b "*w*" :text (format nil "~{~a~%~}" (loop for i below 400 collect (format nil "line ~d ~a" i (make-string (mod (* i 7) 23) :initial-element #\x)))))
    (flet ((walk (height lines direction &key check)
             (let ((w (tuatara.display:make-window b :width 40 :height height))
                   (tuatara-user:*scroll-margin* 3))
               (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b 200))
               (tuatara.display:window-ensure-point-visible w)
               (flet ((step-line ()
                        (setf (tuatara.buffer:buffer-point b)
                              (tuatara.buffer:buffer-goto-line b (+ direction (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b)))))
                        (tuatara.display:window-ensure-point-visible w)
                        ;; highlights on every row, as a frame paints them
                        (tuatara.display:apply-highlights (tuatara.display:window-rows w)
                                                          (list (list 0 (tuatara.buffer:buffer-size b) :region)))))
                 (if check
                     (loop repeat lines
                           do (step-line)
                              (let ((kept (tuatara.display:window-rows w)))
                                (tuatara.display:window-ensure-point-visible w)
                                (let ((whole (tuatara.display:window-layout-rows w (tuatara.display:window-start w) height))
                                      (again (tuatara.display:window-rows w)))
                                  (is eq kept again)
                                  (is equal (map 'list #'tuatara.display:row-start whole) (map 'list #'tuatara.display:row-start again))
                                  (is equal (map 'list (lambda (row) (coerce (tuatara.display:row-string row) 'list)) whole)
                                      (map 'list (lambda (row) (coerce (tuatara.display:row-string row) 'list)) again))
                                  (true (every (lambda (row) (every #'null (tuatara.display:row-faces row))) again)
                                        "highlights left on rows used again"))))
                     (nth-value 2 (measuring (loop repeat lines do (step-line)))))))))
      (walk 12 120 1 :check t)
      (walk 12 120 -1 :check t)
      ;; each row more of the window, for each line moved, against a layout's for each row more
      (let ((moved (/ (- (walk 40 100 1) (walk 20 100 1)) 100 20))
            (laid (/ (- (nth-value 2 (measuring (tuatara.display:layout (tuatara.buffer:buffer-text b) 0 40 40)))
                        (nth-value 2 (measuring (tuatara.display:layout (tuatara.buffer:buffer-text b) 0 40 20))))
                     20)))
        ;; a layout of the window for each line before
        (true (< (/ moved laid) 1/4) "~,2f layouts a line moved" (/ moved laid))))))

(define-test (display wrapped-rows-keep-a-scroll-margin)
  ;; where long lines wrap, the margin is counted in rows
  (with-test-buffer (b "*w*" :text (format nil "~{~a~%~}" (loop repeat 20 collect (make-string 15 :initial-element #\a))))
    (let ((w (tuatara.display:make-window b :width 10 :height 6))
          (tuatara-user:*scroll-margin* 2))
      (setf (tuatara.display:window-wrap w) t)
      (tuatara.display:window-ensure-point-visible w)
      ;; the fourth line's first row is two rows from the bottom: the second line's second row on top
      (setf (tuatara.buffer:buffer-point b) 48)
      (tuatara.display:window-ensure-point-visible w)
      (is = 26 (tuatara.display:window-start w))
      (is = 3 (tuatara.display:window-point-position w)))))

(define-test (display highlights)
  (let* ((rope (tuatara.text:string->rope (format nil "abcdef~%gh")))
         (rows (tuatara.display:layout rope 0 20 5 :highlights '((2 5 :region) (7 8 :match)))))
    (is equal '((2 5 :region)) (tuatara.display:row-spans (aref rows 0)))
    (is equal '((0 1 :match)) (tuatara.display:row-spans (aref rows 1)))
    (is eq :region (aref (tuatara.display:row-faces (aref rows 0)) 3))
    (false (aref (tuatara.display:row-faces (aref rows 0)) 5)))
  ;; later ranges paint over earlier ones; overlaps become a list, highest priority first
  (let* ((rope (tuatara.text:string->rope "abcdef"))
         (rows (tuatara.display:layout rope 0 20 5 :highlights '((0 6 :string) (2 4 :region)))))
    (is equal '((0 2 :string) (2 4 (:region :string)) (4 6 :string)) (tuatara.display:row-spans (aref rows 0))))
  ;; ranges outside the rows, and empty ranges, change nothing
  (let* ((rope (tuatara.text:string->rope (format nil "ab~%cd")))
         (rows (tuatara.display:layout rope 3 20 5 :highlights '((0 2 :string) (4 4 :region)))))
    (false (tuatara.display:row-spans (aref rows 0))))
  ;; a wide character inside a highlight keeps one face entry per printed character
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope "a中b") 0 20 5 :highlights '((0 4 :region)))))
    (is equal '((0 2 :region)) (tuatara.display:row-spans (aref rows 0))))
  ;; a control character shows two characters with the same face
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope (format nil "~cx" (code-char 1))) 0 20 5 :highlights '((0 1 :region)))))
    (is equal '((0 2 :region)) (tuatara.display:row-spans (aref rows 0)))))

(define-test (display rows-drawn-large)
  ;; a line drawn twice as large holds half the columns; the height is the rows' heights added up
  (let* ((rope (tuatara.text:string->rope (format nil "big line~%small~%")))
         (scale (lambda (line-start) (if (zerop line-start) 2 1)))
         (rows (tuatara.display:layout rope 0 10 10 :scale scale :wrap t)))
    (is equal '("big l" "ine" "small" "") (row-strings rows))
    (is equal '(2 2 1 1) (map 'list #'tuatara.display:row-scale rows))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 8) (is = 1 r) (is = 3 c))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 9) (is = 2 r) (is = 0 c))
    ;; a large row that does not fit is left out, but for the first, made to fit
    (is equal '("small" "") (row-strings (tuatara.display:layout rope 9 10 2 :scale scale)))
    (is equal '("big line") (row-strings (tuatara.display:layout rope 0 10 1 :scale scale)))
    (is = 1 (tuatara.display:row-scale (aref (tuatara.display:layout rope 0 10 1 :scale scale) 0)))
    (is equal '("big l") (row-strings (tuatara.display:layout rope 0 10 3 :scale scale :wrap t)))
    ;; half as large again: two thirds of the columns, two such rows in three
    (let ((rows (tuatara.display:layout rope 0 10 3 :scale (lambda (line-start) (if (zerop line-start) 3/2 1)) :wrap t)))
      (is equal '("big li" "ne") (row-strings rows))
      (is equal '(3/2 3/2) (map 'list #'tuatara.display:row-scale rows)))
    ;; a column of a large row in the window's columns, and back
    (is equal '(0 2 3 4 5) (mapcar (lambda (c) (tuatara.display:large-column c 5/4)) '(0 1 2 3 4)))
    (true (loop for scale in '(5/4 3/2 2 3 6/5)
                always (loop for c below 50 always (= c (floor (tuatara.display:large-column c scale) scale)))))))

(define-test (display hl-line)
  ;; the line point is on, in the selected window alone, out to its edge,
  ;; under the selection: on for every buffer unless the init file says not
  (flet ((spans (keys &key (global t))
           (let ((tuatara-user:*global-hl-line-mode* global))
             (let ((f (run-keys keys :text (format nil "one~%two~%three") :width 20 :height 10)))
               (loop for r below 9 collect (aref (tuatara.frontend.fake:fake-spans f) r))))))
    (let ((rows (spans "j")))
      (is equal '((0 20 :hl-line)) (second rows))
      (false (first rows))
      (false (third rows)))
    ;; the last line, with no newline after it, and the empty line after a last newline
    (is equal '((0 20 :hl-line)) (third (spans "j j")))
    (let ((tuatara-user:*global-hl-line-mode* t))
      (let ((f (run-keys "M->" :text (format nil "one~%two~%") :width 20 :height 10)))
        (is equal '((0 20 :hl-line)) (aref (tuatara.frontend.fake:fake-spans f) 2))
        (false (aref (tuatara.frontend.fake:fake-spans f) 1))))
    ;; the selection over it
    (is equal '((0 1 (:region :hl-line)) (1 20 :hl-line)) (second (spans "j v l")))
    ;; a window not selected shows none, only its point: the lower one after C-x 2
    (let ((rows (spans "C-x 2 j")))
      (is equal '((0 20 :hl-line)) (second rows))
      (is equal '((0 1 :cursor-inactive)) (nth 5 rows)))
    ;; off for all, and a buffer that asks for it with hl-line-mode, and one that does not
    (false (second (spans "j" :global nil)))
    (is equal '((0 20 :hl-line)) (second (spans "M-x h l - l i n e - m o d e RET j" :global nil)))
    (false (second (spans "M-x h l - l i n e - m o d e RET j")))
    ;; the leader's SPC t h does the same
    (false (second (spans "SPC t h j"))))
  ;; the commands say what they did; global-hl-line-mode switches it for every buffer
  (let ((tuatara-user:*global-hl-line-mode* t))
    (multiple-value-bind (f editor) (run-keys "M-x h l - l i n e - m o d e RET" :text "abc" :width 80)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Hl-line off")))
    (multiple-value-bind (f editor) (run-keys "M-x g l o b a l - h l - l i n e - m o d e RET" :text "abc" :width 80)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Hl-line off in every buffer"))
      (false tuatara-user:*global-hl-line-mode*))))


(define-test (display wrapping-at-words)
  ;; wrapped at a word's edge, at a column short of the window's width
  (flet ((rows (text width column) (row-strings (tuatara.display:layout (tuatara.text:string->rope text) 0 width 10
                                                                        :wrap t :wrap-column column :word-wrap t))))
    (is equal '("aaa bbb " "ccc") (rows "aaa bbb ccc" 40 8))
    ;; a word too long for a row is cut where it must be
    (is equal '("abcd" "efgh" "ij") (rows "abcdefghij" 40 4))
    ;; a blank that does not fit ends the row unseen
    (is equal '("abcd" "efg") (rows "abcd efg" 40 4))
    ;; the window's width when that is nearer
    (is equal '("aaa " "bbb " "ccc") (rows "aaa bbb ccc" 4 8))
    ;; each line wraps by itself
    (is equal '("aa bb " "cc" "dd ee") (rows (format nil "aa bb cc~%dd ee") 40 6)))
  ;; the rows after the first go on from it; point is found on them
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope "aaa bbb ccc") 0 40 10 :wrap t :wrap-column 8 :word-wrap t)))
    (false (tuatara.display:row-continuation-p (aref rows 0)))
    (true (tuatara.display:row-continuation-p (aref rows 1)))
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows 9) (is = 1 r) (is = 1 c))))

(defun finishes-within (seconds function)
  "True when FUNCTION returns within SECONDS; otherwise its thread is stopped and NIL returned."
  (let* ((done nil)
         (thread (sb-thread:make-thread (lambda () (funcall function) (setf done t)) :name "finishes-within")))
    (loop repeat (* seconds 20) until done do (sleep 0.05))
    (unless done
      (sb-thread:terminate-thread thread)
      (sb-thread:join-thread thread :default nil :timeout 5))
    done))

(define-test (display wrapped-rows-always-move-on)
  ;; a character too wide for a whole row is shown cut to it, never left for a row that never comes
  (flet ((rows (text width &rest keys)
           (apply #'tuatara.display:layout (tuatara.text:string->rope text) 0 width 20 :wrap t keys)))
    (let ((rows (rows "あいう" 1)))
      (is equal (list tuatara.display::*cut-glyph* tuatara.display::*cut-glyph* tuatara.display::*cut-glyph* "")
          (row-strings rows))
      (is equal '(0 3 6 9) (map 'list #'tuatara.display:row-start rows))
      (multiple-value-bind (r c) (tuatara.display:point-row-column rows 3) (is = 1 r) (is = 0 c)))
    ;; a tab wider than the row: the spaces that fit
    (is equal '("   " "x") (row-strings (rows (format nil "~ax" #\Tab) 3)))
    ;; a prefix leaving no room: the character still takes the row
    (let ((rows (rows "ab" 2 :prefix (lambda (start continuation) (declare (ignore start continuation)) (values "↪ " nil)))))
      (is equal '(0 1 2) (map 'list #'tuatara.display:row-start rows))))
  ;; the session that froze: soft wrap in a window narrower than a wide character and its mark
  (dolist (case (list (list "あいうえお" 3) (list "あ" 1) (list (format nil "~ax" #\Tab) 5)))
    (destructuring-bind (text width) case
      (true (finishes-within 20 (lambda () (run-keys "SPC t l l l j g e" :text text :width width :height 6)))))))

(define-test (display zero-width-characters-show-the-cursor)
  (flet ((at (text byte &rest keys)
           (multiple-value-list (tuatara.display:point-row-column
                                 (apply #'tuatara.display:layout (tuatara.text:string->rope text) 0 20 5 keys) byte))))
    ;; a combining mark: at the column of the letter it joins
    (let ((text (format nil "ae~cb" (code-char #x301))))
      (is equal '(0 1) (at text 2))
      (is equal '(0 2) (at text 4)))
    ;; a joiner between two emoji, and the one joined: at the first of them, the pair two columns
    (let ((text (format nil "a~c~c~cb" (code-char #x1F468) (code-char #x200D) (code-char #x1F469))))
      (is equal '(0 1) (at text 5))
      (is equal '(0 1) (at text 8))
      (is equal '(0 3) (at text 12)))
    ;; with nothing before it on its row: where the next character stands, past a prefix too
    (is equal '(0 0) (at (format nil "~cx" (code-char #x301)) 0))
    (is equal '(0 2) (at (format nil "~cx" (code-char #x301)) 0
                         :prefix (lambda (start continuation) (declare (ignore start continuation)) (values "  " nil))))
    ;; a byte-order mark takes no column
    (is = 0 (tuatara.display:char-columns (code-char #xFEFF)))
    (is equal '(0 0) (at (format nil "~cbom" (code-char #xFEFF)) 0)))
  ;; in a session, point on the mark shows the cursor on the letter rather than on the bottom row
  (let ((f (run-keys (list (lambda () (setf (tuatara.buffer:buffer-point (tuatara.buffer:find-buffer "*scratch*")) 2)) :tick)
                     :text (format nil "ae~cb" (code-char #x301)) :width 20 :height 6)))
    (is equal '(0 . 1) (tuatara.frontend.fake:fake-cursor f))))

(defun mixed-line (repeats)
  "A line of REPEATS times letters, a tab, a wide character with a combining mark and a joiner."
  (with-output-to-string (out)
    (dotimes (i repeats)
      (format out "ab~cあ~cc~cd" #\Tab (code-char #x301) (code-char #x200D)))
    (format out "~%short~%")))

(define-test (display columns-past-the-reach)
  ;; past *screen-column-reach* octets a character counts one column: a row scrolled sideways
  ;; starts where that count says, whether the characters are counted yet or not
  (let ((tuatara.display::*screen-column-reach* 64)
        (text (mixed-line 60))
        (path (merge-pathnames (format nil "reach-~36r.txt" (random (expt 36 8))) (tuatara.io:cache-directory))))
    (alexandria:write-byte-vector-into-file (sb-ext:string-to-octets text :external-format :utf-8) path :if-exists :supersede)
    (multiple-value-bind (unindexed mapping) (tuatara.text:open-file-rope path)
      (unwind-protect
           (dolist (rope (list (tuatara.text:string->rope text) unindexed))
             (let ((last -1))
               (loop for hscroll from 0 below 700 by 7
                     do (let* ((row (aref (tuatara.display:layout rope 0 10 1 :hscroll hscroll) 0))
                               (first (find-if-not #'minusp (tuatara.display:row-char-bytes row))))
                          (when first
                            ;; on along the line as the scroll grows, near the column asked for
                            (true (>= first last))
                            (setf last first)
                            (true (<= (- hscroll 3) (tuatara.display::line-screen-column rope 0 first) (+ hscroll 8))))))
               (true (plusp last))))
        (when mapping (tuatara.text:close-mapping mapping))
        (delete-file path))))
  ;; the window keeps point shown at every character of such a line, a tab, a wide character, a mark
  (let ((tuatara.display::*screen-column-reach* 64))
    (with-test-buffer (b "*reach*" :text (mixed-line 60))
      (let ((w (tuatara.display:make-window b :width 12 :height 3))
            (end (tuatara.buffer:buffer-line-end b 0)))
        (loop for p = 0 then (tuatara.text:rope-next-boundary (tuatara.buffer:buffer-text b) p)
              while (<= p end)
              do (setf (tuatara.buffer:buffer-point b) p)
                 (tuatara.display:window-ensure-point-visible w)
                 (multiple-value-bind (row column) (tuatara.display:window-point-position w)
                   (true (eql row 0))
                   (true (and column (< column 12))))
              until (= p end))))))

(define-test (display long-line-keys-cost-no-wait)
  ;; on a line of 20 MB, keeping point shown costs no read of the line: a few milliseconds a key
  (with-test-buffer (b "*long*" :text (let ((piece (format nil "abc def ghi~cjkl " #\Tab)))
                                        (with-output-to-string (out)
                                          (dotimes (i (floor (* 20 1024 1024) (length piece))) (write-string piece out)))))
    (let* ((w (tuatara.display:make-window b :width 60 :height 6))
           (size (tuatara.buffer:buffer-size b))
           (times '()))
      (flet ((show (p)
               (setf (tuatara.buffer:buffer-point b) p)
               (let ((start (thread-cpu-time)))
                 (tuatara.display:window-ensure-point-visible w)
                 (push (/ (- (thread-cpu-time) start) (/ internal-time-units-per-second 1000)) times))
               (multiple-value-bind (row column) (tuatara.display:window-point-position w)
                 (true (eql row 0))
                 (true (and column (< column 60)))
                 ;; the character before point is shown just before the cursor
                 (when (and column (plusp column))
                   (let ((shown (tuatara.display:row-string (aref (tuatara.display:window-rows w) 0)))
                         (before (char (tuatara.buffer:buffer-string b :start (1- p) :end p) 0)))
                     (is char= (if (char= before #\Tab) #\Space before) (char shown (1- column))))))))
        ;; the end, then back a character at a time, a jump to the middle, near the reach
        (show size)
        (loop repeat 10 for p downfrom (1- size) do (show p))
        (show (floor size 2))
        (show (+ tuatara.display::*screen-column-reach* 5))
        (show (- tuatara.display::*screen-column-reach* 5))
        (show size)
        (let ((sorted (sort (copy-list times) #'<)))
          (true (< (nth (floor (length sorted) 2) sorted) 50))
          (true (< (first (last sorted)) 400)))))))

(define-test (display cursor-colour-follows-its-face)
  ;; a colour of its own, else the :cursor face's, which both frontends take
  (is string= "#00ff00" (tuatara.display:cursor-colour "#00ff00"))
  (let ((face (tuatara.display:find-face :cursor)))
    (let ((old (tuatara.display::face-fg face)))
      (unwind-protect
           (progn (tuatara.display:set-face-attributes :cursor :fg "#ff0000")
                  (is string= "#ff0000" (tuatara.display:cursor-colour nil))
                  (is string= "#ff0000" (tuatara.frontend.tui::cursor-colour-code nil)))
        (setf (tuatara.display::face-fg face) old)))))

(define-test (display overlapping-faces-merged-once)
  ;; a face painted over a run of characters of one face makes one merged face for the run
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope "abcdef") 0 20 1)))
    (tuatara.display::apply-highlights rows '((0 6 :comment) (1 5 :region)))
    (let ((faces (tuatara.display::row-faces (aref rows 0))))
      (is eq :comment (aref faces 0))
      (is equal '(:region :comment) (aref faces 1))
      (is eq (aref faces 1) (aref faces 4))
      (is eq :comment (aref faces 5)))))

(define-test (display point-hidden-in-a-fold-below)
  ;; point hidden in a fold below the window: keeping it in sight ends, the window shown from
  ;; the fold's line, however the window starts snap back to it
  (with-test-buffer (b "*fold*" :text (format nil "~{line ~a~%~}" (loop for i from 0 below 100 collect i)))
    (let ((w (tuatara.display:make-window b :width 20 :height 5)))
      (tuatara.display:fold-region b (tuatara.buffer:buffer-goto-line b 3) (tuatara.buffer:buffer-goto-line b 80))
      (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-goto-line b 50))
      (let ((thread (sb-thread:make-thread (lambda () (tuatara.display:window-ensure-point-visible w) :done))))
        (is eq :done (sb-thread:join-thread thread :timeout 5 :default :hung))
        (when (sb-thread:thread-alive-p thread) (sb-thread:terminate-thread thread))))))

(define-test (display replacements-asked-between-folds)
  ;; what a buffer shows in place of its text is asked for the stretches shown between its folds
  ;; alone, never the text a fold hides, however much that is; those after the fold still shown
  (with-test-buffer (b "*fold*" :text (format nil "~{line ~a~%~}" (loop for i from 0 below 100 collect i)))
    (let* ((asked '())
           (fold-start (tuatara.buffer:buffer-goto-line b 3))
           (fold-end (tuatara.buffer:buffer-goto-line b 90))
           (after (tuatara.buffer:buffer-goto-line b 91)))
      (tuatara.display:fold-region b fold-start fold-end)
      (setf (tuatara.buffer:buffer-local b :display-replacements-function)
            (lambda (buffer from to)
              (declare (ignore buffer))
              (push (cons from to) asked)
              (when (and (<= from after) (< after to)) (list (list after (+ after 4) "LINE" nil)))))
      (let ((hidden (tuatara.display::window-hidden b 0 20 5)))
        (true asked)
        (loop for (from . to) in asked
              do (false (and (< from fold-end) (> to fold-start))))
        (true (find after hidden :key #'first))))))

(define-test (display long-wrapped-line-rows)
  ;; a wrapped line of more than a megabyte: every row begins at a character, and the
  ;; rows above any place in it are found a row at a time, as they are laid out
  (let ((piece (format nil "abc漢字😀e~c~c" (code-char #x301) #\Tab)))
    (with-test-buffer (b "*long-wrap*" :text (format nil "~a~%second~%"
                                                     (with-output-to-string (out)
                                                       (dotimes (i 70000) (write-string piece out)))))
      (let* ((w (tuatara.display:make-window b :width 40 :height 6))
             (rope (tuatara.buffer:buffer-text b))
             (end (tuatara.buffer:buffer-line-end b 0)))
        (setf (tuatara.display:window-wrap w) t)
        (dolist (p (list (- end 1) (- end 7) (- end 300000) (+ 1048576 3) 70000))
          (let ((start (tuatara.display:window-row-start w p)))
            (true (tuatara.text:char-boundary-p rope start))
            (true (<= start p))
            (let ((row (aref (tuatara.display:window-layout-rows w start 1) 0)))
              (true (< p (tuatara.display:row-end row))))
            ;; the rows above: each ends where the one below it begins
            (let ((at start))
              (loop repeat 5
                    do (let* ((above (tuatara.display:rows-back w at 1))
                              (row (aref (tuatara.display:window-layout-rows w above 1) 0)))
                         (true (tuatara.text:char-boundary-p rope above))
                         (true (< above at))
                         (is = at (tuatara.display:row-end row))
                         (setf at above))))
            ;; and three rows up at once comes to the same place as one at a time
            (is = (tuatara.display:rows-back w (tuatara.display:rows-back w (tuatara.display:rows-back w start 1) 1) 1)
                (tuatara.display:rows-back w start 3))))))))

(define-test (display sideways-scroll-cuts-a-wide-character)
  ;; a wide character the left edge cuts leaves its column blank: what follows stays in its column
  (let* ((rope (tuatara.text:string->rope (format nil "a~a~%~ab~%" (make-string 20 :initial-element #\漢)
                                                  (make-string 20 :initial-element #\漢))))
         (rows (tuatara.display:layout rope 0 10 2 :hscroll 31)))
    ;; line 1: column 31 is the second half of the 16th 漢, so it starts at a whole one
    (is string= "漢漢漢漢漢" (tuatara.display:row-string (aref rows 0)))
    ;; line 2: column 31 is the right half of a 漢, left blank
    (is string= " 漢漢漢漢b" (tuatara.display:row-string (aref rows 1)))
    (is = 10 (tuatara.display:row-columns (aref rows 1)))
    ;; the b is in the column it has unscrolled, less the scroll
    (multiple-value-bind (r c) (tuatara.display:point-row-column rows (- (tuatara.text:rope-bytes rope) 2))
      (is = 1 r) (is = 9 c))))

(define-test (display marks-take-no-column)
  ;; every mark that combines with the character before it takes no column; jamo after the
  ;; first of a syllable neither
  (dolist (code '(#x301 #x308 #x323 #x94D #x947 #xE48 #x0670 #x05B4 #x1161 #x11A8 #x1F3FD #xFE0F))
    (is = 0 (tuatara.display:char-columns (code-char code))))
  (dolist (code '(#x41 #x928 #x627 #xE2A #x1100))
    (true (plusp (tuatara.display:char-columns (code-char code))))))

(define-test (display hl-line-on-wrapped-rows)
  ;; every row of the line point is on, where it wraps, is lit out to the window's edge
  (let ((tuatara-user:*global-hl-line-mode* t)
        (tuatara-user:*fill-column* 8))
    (let ((f (run-keys "SPC t l" :text (format nil "aaa bbbbb ccc dd eee~%next") :width 30 :height 8)))
      (dotimes (r 4)
        (let ((last (car (last (aref (tuatara.frontend.fake:fake-spans f) r)))))
          (is = 30 (second last) "Row ~d is lit to column ~a" r (second last))
          (true (intersection '(:hl-line :soft-wrap-row) (alexandria:ensure-list (third last))))))
      (false (aref (tuatara.frontend.fake:fake-spans f) 4)))))

(define-test (display cursor-colour-from-its-background)
  ;; the :cursor face's background colours the cursor, in both frontends, its text
  ;; colour the character under the block
  (let* ((face (tuatara.display:find-face :cursor))
         (fg (tuatara.display::face-fg face)) (bg (tuatara.display::face-bg face))
         (theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (tuatara.display:set-face-attributes :cursor :fg nil :bg "#ff0000")
           (is string= "#ff0000" (tuatara.display:cursor-colour nil))
           (is string= "#ff0000" (tuatara.frontend.tui::cursor-colour-code nil))
           (tuatara.display:set-face-attributes :cursor :fg "#00ff00")
           (tuatara.frontend.gtk:call-with-painted-frame
            (lambda (surface width height cell-width cell-height)
              (declare (ignore width height cell-width cell-height))
              (is string= "#ff0000" (tuatara.frontend.gtk:pixel-colour surface 1 1))
              (is equal '(0d0 1d0 0d0) (tuatara.frontend.gtk::cursor-colours nil)))
            (tuatara.frontend.gtk:make-frame (vector (list "M" nil '()) nil))
            :columns 4 :rows 2 :font "Adwaita Mono 11"))
      (setf (tuatara.display::face-fg face) fg (tuatara.display::face-bg face) bg)
      (tuatara.display:apply-theme theme))))

(define-test (display face-cache-emptied-on-change)
  ;; what is worked out from face specs is kept until a face, or what else it rests on, changes
  (let ((cache (tuatara.display:make-face-cache)))
    (setf (gethash :text (tuatara.display:face-cache-table cache)) 1)
    (is = 1 (gethash :text (tuatara.display:face-cache-table cache)))
    (false (nth-value 1 (tuatara.display:face-cache-table cache)))
    (tuatara.display:set-face-attributes :face-cache-test :bold t)
    (true (nth-value 1 (tuatara.display:face-cache-table cache)))
    (false (gethash :text (tuatara.display:face-cache-table cache)))
    (setf (gethash :text (tuatara.display:face-cache-table cache)) 2)
    (true (nth-value 1 (tuatara.display:face-cache-table cache '(:light))))
    (false (nth-value 1 (tuatara.display:face-cache-table cache '(:light))))
    (false (gethash :text (tuatara.display:face-cache-table cache '(:light))))))

(define-test (display columns-kept-as-worked-out)
  ;; the columns of each code of the first two planes, asked for twice, are those its ranges give
  (dotimes (pass 2)
    (true (loop for code from 160 below #x20000
                always (= (tuatara.display:code-columns code) (tuatara.display::ranges-columns code))))))

(define-test (display wrapped-start-found-again)
  ;; where lines wrap, the window's start is a row's start, found again when the
  ;; window's width, the text or the start changes
  (with-test-buffer (b "*wrapped start*" :text (make-string 300 :initial-element #\a))
    (let ((w (tuatara.display:make-window b :width 80 :height 5)))
      (setf (tuatara.display:window-wrap w) t
            (tuatara.buffer:buffer-point b) 200
            (tuatara.display:window-start w) 165)
      (flet ((start-after ()
               (tuatara.display:window-ensure-point-visible w)
               (tuatara.display:window-start w)))
        (let ((first (start-after)))
          (is = first (tuatara.display::window-row-start w 165))
          (is = first (start-after))
          (setf (tuatara.display:window-width w) 70)
          (is = (tuatara.display::window-row-start w first) (start-after))
          (true (/= first (tuatara.display:window-start w))))
        (let ((start (tuatara.display:window-start w)))
          (tuatara.buffer:buffer-insert b 0 "bb")
          (is = start (start-after))
          (setf (tuatara.display:window-start w) (+ start 5))
          (is = start (start-after)))))))

(defun codes (&rest codes)
  "A string of the characters of CODES."
  (map 'string #'code-char codes))

(define-test (display emoji-columns)
  ;; every character drawn two wide counts two, a picture and a regional indicator among them
  (dolist (code '(#x1F680 #x1FAE0 #x2615 #x2705 #x231A #x1F6D1 #x1F004 #x1F1F3 #x4E2D #xFF21))
    (is = 2 (tuatara.display:code-columns code) "U+~x is two columns" code))
  ;; a symbol shown as text unless asked otherwise is one
  (dolist (code '(#x2764 #x2600 #x1F321 #x263A))
    (is = 1 (tuatara.display:code-columns code) "U+~x is one column" code))
  ;; a grapheme cluster takes the columns of its first character, made two by U+FE0F
  (loop for (columns . string)
          in (list (cons 2 (codes #x2764 #xFE0F))                    ; a red heart
                   (cons 2 (codes #x2600 #xFE0F))                    ; the sun
                   (cons 2 (codes #x31 #xFE0F #x20E3))               ; the keycap 1
                   (cons 2 (codes #x1F680 #xFE0F))                   ; a rocket asked for twice
                   (cons 2 (codes #x1F468 #x200D #x1F469 #x200D #x1F467 #x200D #x1F466)) ; a family
                   (cons 2 (codes #x1F469 #x1F3FD #x200D #x1F4BB))   ; a coder with a skin tone
                   (cons 2 (codes #x1F3F3 #xFE0F #x200D #x1F308))    ; a rainbow flag
                   (cons 2 (codes #x1F469 #x200D #x2764 #xFE0F #x200D #x1F468)) ; a couple
                   (cons 2 (codes #x1F1F3 #x1F1FF))                  ; a flag
                   (cons 4 (codes #x1F1F3 #x1F1FF #x1F1F3))          ; a flag and half of one
                   (cons 2 (codes #x1F44D #x1F3FD))                  ; a thumb with a skin tone
                   (cons 3 (codes #x61 #x2764 #xFE0F))
                   (cons 1 (codes #x61 #xFE0F))                      ; a letter has no picture
                   (cons 2 (codes #x65 #x301 #x62)))
        do (is = columns (tuatara.display:string-columns string) "~a takes ~a columns" string columns))
  ;; text is cut to fit a width before a cluster, not inside it
  (is = 1 (tuatara.display:chars-in-columns (codes #x61 #x2764 #xFE0F #x62) 2))
  (is = 3 (tuatara.display:chars-in-columns (codes #x61 #x2764 #xFE0F #x62) 3))
  ;; a window lays them out so: the x after each in the column after its two
  (loop for string in (list (codes #x1F680) (codes #x2764 #xFE0F) (codes #x31 #xFE0F #x20E3)
                            (codes #x1F468 #x200D #x1F469 #x200D #x1F467 #x200D #x1F466) (codes #x1F1F3 #x1F1FF))
        do (let* ((text (concatenate 'string string "x"))
                  (rope (tuatara.text:string->rope text))
                  (rows (tuatara.display:layout rope 0 20 2)))
             (is = 3 (tuatara.display:row-columns (aref rows 0)) "~a lays out in three columns" text)
             (is equal '(0 2) (multiple-value-list (tuatara.display:point-row-column rows (1- (tuatara.text:rope-bytes rope)))))
             (is = 2 (tuatara.display:display-column rope 0 (1- (tuatara.text:rope-bytes rope))))))
  ;; both columns of a heart made a picture stand for the heart, and the selector is shown on it
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope (codes #x2764 #xFE0F #x78)) 0 20 2)))
    (is equal '(0 0 6) (coerce (tuatara.display:row-bytes (aref rows 0)) 'list))
    (is equal '(0 0) (multiple-value-list (tuatara.display:point-row-column rows 3))))
  ;; a heart with U+FE0F at a wrapped row's last column stays whole on that row
  (let ((rows (tuatara.display:layout (tuatara.text:string->rope (codes #x61 #x2764 #xFE0F #x62)) 0 2 3 :wrap t)))
    (is string= (codes #x61 #x2764 #xFE0F) (tuatara.display:row-string (aref rows 0)))))

(define-test (display mark-beginning-a-line)
  ;; a mark with nothing before it is shown on a dotted circle, in a column of its own
  (let* ((text (format nil "first~%~csecond" (code-char #x301)))
         (rope (tuatara.text:string->rope text))
         (rows (tuatara.display:layout rope 0 20 3)))
    (is string= (format nil "~c~csecond" (code-char #x25CC) (code-char #x301)) (tuatara.display:row-string (aref rows 1)))
    (is = 7 (tuatara.display:row-columns (aref rows 1)))
    (is = 1 (tuatara.display:display-column rope 6 8))
    (is = 7 (tuatara.display:string-columns (format nil "~csecond" (code-char #x301))))))

(define-test (display vertical-motion-keeps-clusters-whole)
  ;; to a column inside a grapheme cluster: after it, so that nothing then cuts it
  (with-test-buffer (b "*clusters*" :text (concatenate 'string "abcd" (string #\Newline)
                                                       (codes #x2764 #xFE0F #x78) (string #\Newline)
                                                       (codes #x915 #x93F #x78) (string #\Newline)
                                                       (codes #x1F468 #x200D #x1F469 #x200D #x1F467 #x64)))
    (let ((heart (tuatara.buffer:buffer-goto-line b 1))
          (letter (tuatara.buffer:buffer-goto-line b 2))
          (family (tuatara.buffer:buffer-goto-line b 3)))
      (is = (+ heart 6) (tuatara.display:buffer-move-to-screen-column b heart 1))
      (is = (+ heart 6) (tuatara.display:buffer-move-to-screen-column b heart 2))
      (is = heart (tuatara.display:buffer-move-to-screen-column b heart 0))
      (is = (+ letter 6) (tuatara.display:buffer-move-to-screen-column b letter 1))
      (is = (+ family 18) (tuatara.display:buffer-move-to-screen-column b family 1))
      (is = (+ family 18) (tuatara.display:buffer-move-to-screen-column b family 2))))
  ;; in a session, C-n to the family's second column lands after it, and C-d there takes the d
  (let ((text (format nil "ab~%~a" (codes #x1F468 #x200D #x1F469 #x200D #x1F467 #x64)))
        (left (format nil "ab~%~a" (codes #x1F468 #x200D #x1F469 #x200D #x1F467))))
    (is string= left (session-text (nth-value 1 (run-keys "C-f C-f C-n C-d" :text text))))
    (is string= left (session-text (nth-value 1 (run-keys "C-f C-n C-d" :text text))))))

(define-test (display window-starting-inside-a-line)
  ;; where lines wrap, a window beginning inside a line shows its top row as going on from the one above
  (let* ((rope (tuatara.text:string->rope (format nil "~a~%next" (make-string 50 :initial-element #\a))))
         (rows (tuatara.display:layout rope 20 10 4 :wrap t)))
    (true (tuatara.display:row-continuation-p (aref rows 0)))
    (true (tuatara.display:row-continuation-p (aref rows 2)))
    (false (tuatara.display:row-continuation-p (aref rows 3))))
  (false (tuatara.display:row-continuation-p
          (aref (tuatara.display:layout (tuatara.text:string->rope "abc") 0 10 2 :wrap t) 0))))

(define-test (display few-columns-read-few-octets)
  ;; counting a few columns of a long text reads about as much of it, not a block of 64 KB
  (let ((rope (tuatara.text:string->rope (make-string 200000 :initial-element #\a))))
    (flet ((consed (columns)
             (tuatara.display::scan-columns rope 0 (tuatara.text:rope-bytes rope) 8 :limit columns)
             (nth-value 2 (tuatara.bench::measuring
                            (tuatara.display::scan-columns rope 0 (tuatara.text:rope-bytes rope) 8 :limit columns)))))
      (true (< (consed 10) 1000))
      (true (< (consed 100) 1000))
      (is = 100 (nth-value 1 (tuatara.display::scan-columns rope 0 (tuatara.text:rope-bytes rope) 8 :limit 100)))
      (is = 150000 (nth-value 1 (tuatara.display::scan-columns rope 0 150000 8))))))

(define-test (display padded-to-columns)
  ;; text padded with spaces to a number of columns, wide characters two
  (is string= "ab   " (tuatara.display:pad-columns "ab" 5))
  (is string= "   ab" (tuatara.display:pad-columns "ab" 5 :left t))
  (let ((wide (string (code-char #x65E5))))
    (is string= (format nil "~a " wide) (tuatara.display:pad-columns wide 3)))
  (is string= "abcdef" (tuatara.display:pad-columns "abcdef" 3)))

(define-test (display scrolling-past-a-fold)
  ;; a window that does not wrap scrolls by the rows it shows, a fold at its top one row
  (with-test-buffer (b "*fold-scroll*" :text (format nil "~{line ~a~%~}" (loop for i from 0 below 100 collect i)))
    (let ((w (tuatara.display:make-window b :width 20 :height 5)))
      (tuatara.display:fold-region b (tuatara.buffer:buffer-line-end b 0)
                                   (tuatara.buffer:buffer-line-end b (tuatara.buffer:buffer-goto-line b 6)))
      (setf (tuatara.display:window-start w) 0)
      (tuatara.display:window-layout w)
      (is = (tuatara.buffer:buffer-goto-line b 7) (tuatara.display:window-scroll w 1))
      (is = (tuatara.buffer:buffer-goto-line b 8) (tuatara.display:window-scroll w 1))
      (is = 0 (tuatara.display:window-scroll w -2)))))

(define-test (display rows-filled-again)
  ;; a window's layout after a key fills the rows it no longer shows again, rather than
  ;; making rows the window's width, reads the text into a kept block, and shows what
  ;; a layout made anew shows
  (with-test-buffer (b "*rows*" :text (format nil "~{~a~%~}" (loop for i below 400 collect (format nil "line ~d ~a~c~a" i (make-string (mod (* i 37) 150) :initial-element #\x) #\Tab "é中"))))
    (let ((w (tuatara.display:make-window b :width 200 :height 50)))
      (flet ((same-as-anew ()
               (let ((anew (tuatara.display:window-layout-rows w (tuatara.display:window-start w) 50))
                     (shown (tuatara.display:window-rows w)))
                 (is equal (map 'list (lambda (row) (list (tuatara.display:row-start row) (tuatara.display:row-end row)
                                                          (coerce (tuatara.display:row-string row) 'list)
                                                          (coerce (tuatara.display:row-bytes row) 'list)
                                                          (tuatara.display:row-last-p row)))
                                anew)
                     (map 'list (lambda (row) (list (tuatara.display:row-start row) (tuatara.display:row-end row)
                                                    (coerce (tuatara.display:row-string row) 'list)
                                                    (coerce (tuatara.display:row-bytes row) 'list)
                                                    (tuatara.display:row-last-p row)))
                          shown)))))
        (tuatara.display:window-layout w)
        (let ((consed (loop repeat 3
                            minimize (progn (tuatara.buffer:buffer-insert b 10 "z")
                                            (nth-value 2 (tuatara.bench::measuring (tuatara.display:window-layout w)))))))
          (true (< consed (* 6 1024)) "a layout after a key consed ~:d octets" consed))
        (same-as-anew)
        ;; scrolled, then the end of the text, then edited again
        (setf (tuatara.display:window-start w) (tuatara.buffer:buffer-goto-line b 100))
        (tuatara.display:window-layout w)
        (same-as-anew)
        (setf (tuatara.display:window-start w) (tuatara.buffer:buffer-goto-line b 390))
        (tuatara.display:window-layout w)
        (same-as-anew)
        (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-goto-line b 395) "new line")
        (tuatara.display:window-layout w)
        (same-as-anew)))))

(define-test (display columns-agree-with-what-is-shown)
  ;; the columns a character is counted as taking are those of the text it is shown as
  (flet ((agree (code &optional invisible)
           (let ((c (code-char code)))
             (multiple-value-bind (text own) (tuatara.display:char-display c 0 8 invisible)
               (when (stringp text)
                 (is = (length text) own "~x shown as ~s" code text)
                 (is = (length text) (tuatara.display:code-columns code invisible) "~x counted" code)
                 (is = (length text) (tuatara.display:advance-column code 0 0 8 invisible) "~x advanced" code))))))
    (loop for code from 0 below #x300 unless (= code 9) do (agree code))
    (loop for code from #xDC80 to #xDCFF do (agree code))
    (loop for (low . high) in tuatara.display:*invisible-characters*
          do (loop for code from low to high do (agree code t)))
    ;; a character that draws nothing past the planes of four digits and one more
    (let ((tuatara.display:*invisible-characters* (list (cons #x10FFFD #x10FFFD))))
      (agree #x10FFFD t)))
  ;; a tab reaches its stop, wherever it stands
  (is = 3 (tuatara.display:advance-column 9 5 0 8 nil))
  (is = 8 (tuatara.display:advance-column 9 8 0 8 nil))
  ;; a control character takes one column in the panel, as the sign it is shown as there
  (loop for code from 0 below 160
        do (is = (length (tuatara.display:displayable (string (code-char code))))
               (tuatara.command::panel-char-columns (code-char code)))))

(define-test (display stand-ins-made-once)
  ;; a tab, a control character and a raw byte are shown as text made once, not for each
  ;; character of each frame; octets written in hex take one string
  (flet ((consed (function)
           (funcall function)
           (loop repeat 3
                 minimize (let ((before (tuatara.bench::bytes-consed)))
                            (funcall function)
                            (- (tuatara.bench::bytes-consed) before)))))
    (is = 0 (consed (lambda () (dotimes (i 100)
                                 (tuatara.display:char-display #\Tab (mod i 8) 8)
                                 (tuatara.display:char-display (code-char 1) 0 8)
                                 (tuatara.display:char-display (code-char #x85) 0 8)
                                 (tuatara.display:char-display (code-char #xDCA9) 0 8)))))
    (let ((octets (coerce (loop for i below 32 collect (* i 8)) 'tuatara.text:octets)))
      (true (< (consed (lambda () (tuatara.text:octets-hex octets))) 512))
      (is string= "0008101820" (tuatara.text:octets-hex octets :end 5))
      (is string= "F0:F8" (tuatara.text:octets-hex octets :start 30 :upper-case t :separator ":"))
      (is string= "" (tuatara.text:octets-hex octets :start 3 :end 3))))
  (is string= "^A" (tuatara.display:control-char-display (code-char 1)))
  (is string= "^?" (tuatara.display:control-char-display (code-char 127)))
  (is string= "\\x85" (tuatara.display:control-char-display (code-char #x85)))
  (is string= "\\xA9" (tuatara.display:raw-byte-display #xDCA9))
  (is string= "        " (tuatara.display:char-display #\Tab 0 8)))

(define-test (display rows-fitted-in-one-pass)
  ;; a piece of a screen row is cut or padded to its columns as before, looking at each
  ;; character once: a row of plain text in less time than counting its columns takes
  (dolist (string (list "" "plain" "wide 中文 text" (format nil "e~cx" (code-char #x301)) "👍🏽 ok" (make-string 130 :initial-element #\x)))
    (dolist (columns '(0 1 3 5 10 120))
      (let ((kept (tuatara.display:chars-in-columns string columns)))
        (is equal (list (tuatara.display:pad-columns (subseq string 0 kept) columns) kept)
            (multiple-value-list (tuatara.command::fit-to-columns string columns))
            "~s in ~d" string columns))))
  (let ((row (make-string 120 :initial-element #\k)))
    (flet ((fastest (function)
             (loop repeat 5
                   minimize (let ((start (tuatara.bench::monotonic-seconds)))
                              (dotimes (i 2000) (funcall function))
                              (- (tuatara.bench::monotonic-seconds) start)))))
      (let ((fit (fastest (lambda () (tuatara.command::fit-to-columns row 130))))
            (count (fastest (lambda () (tuatara.display:string-columns row)))))
        (true (< fit count) "fitting took ~,1f µs, counting ~,1f µs" (* 500 fit) (* 500 count))))))
