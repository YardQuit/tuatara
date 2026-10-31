;;;; test/many-places.lisp --- Editing at many places: split, cursors, rectangles, banks, a paste over a selection.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test many-places
  :description "The kinds of editing at many places, f's matches, t's cursors, a rectangle's rows, banked lines and a paste over a selection, delete, ask, report, find, kill, wrap and undo alike.")

(defun many-text (keys text)
  "The text after KEYS are typed on TEXT, each turn of KEYS (a list) once the
work in the background the one before began is done."
  (session-text (nth-value 1 (run-keys-in-turns (if (listp keys) keys (list keys)) :text text :width 100 :height 10))))

(defun many-frame (keys text)
  "The frontend after KEYS are typed on TEXT, as MANY-TEXT types them."
  (run-keys-in-turns (if (listp keys) keys (list keys)) :text text :width 100 :height 10))

(define-test (many-places one-question-past-the-limit)
  ;; f, t and c on a rectangle ask the one question past their limits, with one setting
  (let ((tuatara-user:*split-limit-ask* t)
        (tuatara-user:*split-limit* 2)
        (tuatara-user:*cursors-limit* 2)
        (tuatara.defaults::*rectangle-cursors-limit* 2)
        (lines (format nil "abc~%abc~%abc~%abc~%")))
    (true (tuatara.frontend.fake:fake-frame-showing
           (many-frame "% f b RET" lines) "Hold all 4 matches, with every key slow at that many (else those in sight)?"))
    (true (tuatara.frontend.fake:fake-frame-showing
           (many-frame "V j j j t" lines) "Hold all 4 cursors, with every key slow at that many (else those in sight)?"))
    (true (tuatara.frontend.fake:fake-frame-showing
           (many-frame "m v j j j c" lines) "Hold all 4 cursors, with every key slow at that many (else those in sight)?"))
    ;; yes holds them all, no those in sight, and every one is written either way
    (true (tuatara.frontend.fake:fake-frame-showing (many-frame "% f b RET y" lines) "Split: 4 places in the selection"))
    (true (tuatara.frontend.fake:fake-frame-showing (many-frame "% f b RET n" lines) "Split: 2 places of 4 in the selection"))
    (dolist (answer '("y" "n"))
      (is string= (format nil "aXc~%aXc~%aXc~%aXc~%")
          (many-text (list (format nil "% f b RET ~a RET c X C-g" answer) "") lines) "f, ~a" answer)
      (is string= (format nil "Xabc~%Xabc~%Xabc~%Xabc~%")
          (many-text (list (format nil "V j j j t ~a i X C-g" answer) "") lines) "t, ~a" answer)
      (is string= (format nil "Xbc~%Xbc~%Xbc~%Xbc~%")
          (many-text (list (format nil "m v j j j c ~a X C-g" answer) "") lines) "c on a rectangle, ~a" answer))))

(define-test (many-places change-is-one-undo)
  ;; c empties and types at many places as one change: one u takes both back
  (let ((two (format nil "foo bar~%foo baz~%")))
    (is string= two (many-text "% f f o o RET c X C-g u" two))
    (is string= two (many-text "t c X C-g u" two))
    (is string= two (many-text "t m w c X C-g u" two))
    (is string= two (many-text "m v j l c X C-g C-g u" two))
    (let ((tuatara-user:*split-limit* 1))
      (is string= two (many-text '("% f f o o RET c X C-g" "u") two)))))

(define-test (many-places one-kill-of-parts)
  ;; a rectangle's kill goes in as a block by C-y and by p, and a row to each of as many cursors
  (let ((two (format nil "ab~%cd~%")) (block (format nil "bab~%dcd~%")))
    (is string= block (many-text "l m v j M-w g g C-y" two))
    (is string= block (many-text "l m v j M-w g g p" two))
    (is string= block (many-text "l m v j M-w g g t p" two))
    (multiple-value-bind (parts shape)
        (tuatara.defaults::kill-parts (first (tuatara.command:editor-kill-ring (nth-value 1 (run-keys "l m v j M-w" :text two)))))
      (is equal '("b" "d") parts)
      (is eq :block shape)))
  ;; a kill made at as many cursors gives each its own part, and is one text at one place
  (let ((three (format nil "ab~%cd~%ef~%")))
    (is string= (format nil "abab~%cdcd~%efef~%") (many-text "t t m w y g l p" three))
    (is string= (format nil "ab~%cd~%ef~%ab~%cd~%ef") (many-text "t t m w y C-g C-g g e C-y" three))
    (is string= (format nil "ab~%cd~%ef~%ab~%cd~%ef") (many-text "t t m w y C-g C-g g e p" three))
    (multiple-value-bind (parts shape)
        (tuatara.defaults::kill-parts (first (tuatara.command:editor-kill-ring (nth-value 1 (run-keys "t t m w y" :text three)))))
      (is equal '("ab" "cd" "ef") parts)
      (is eq :pieces shape))
    ;; the kill's text is one copy where the parts agree, whole lines together, else one a line
    (is string= "x" (tuatara.defaults::parts-kill-text '("x" "x")))
    (is string= (format nil "a~%b~%") (tuatara.defaults::parts-kill-text (list (format nil "a~%") (format nil "b~%"))))
    (is string= (format nil "a~%b") (tuatara.defaults::parts-kill-text '("a" "b")))
    (false (tuatara.defaults::parts-kill-text '("" "")))))

(define-test (many-places wrap-and-unwrap-alike)
  ;; a wrap key wraps, and again takes the pair off, at every kind of place
  (let ((two (format nil "ab~%ab~%")) (wrapped (format nil "(ab)~%(ab)~%")))
    (is string= (format nil "(ab)~%ab~%") (many-text "m w (" two))
    (is string= wrapped (many-text "% f a b RET ( C-g" two))
    (is string= wrapped (many-text "t m w (" two))
    (is string= wrapped (many-text "m v j l (" two))
    (is string= two (many-text "l m w (" (format nil "(ab)~%ab~%")))
    (is string= two (many-text "% f a b RET ( C-g" wrapped))
    (is string= two (many-text "l t m w (" wrapped))
    (is string= two (many-text "l m v j l (" wrapped))
    ;; a rectangle over much text unwraps in the background, decided by the rows in sight
    (let ((tuatara.defaults::*rewrite-threshold* 0))
      (is string= two (many-text '("l m v j l (" "") wrapped))
      (is string= wrapped (many-text '("m v j l (" "") two)))
    ;; a row not wrapped: every row is wrapped again
    (is string= (format nil "((ab))~%x(ab)~%") (many-text "l m v j l (" (format nil "(ab)~%xab~%")))))

(define-test (many-places one-deletion)
  ;; every kind refuses a buffer that may not be written, one way, and changes nothing
  (let ((two (format nil "foo~%foo~%")))
    (dolist (keys '("% f f o o RET d" "t d" "t m w d" "m v j l d" "m l j m l d" "y m w p" "m l y j m l p"))
      (multiple-value-bind (f editor) (run-keys (format nil "M-x r e a d - o n l y - m o d e RET ~a" keys) :text two :width 80 :height 10)
        (is string= two (session-text editor) "~a" keys)
        (true (tuatara.frontend.fake:fake-frame-showing f "read-only") "~a says so" keys))))
  ;; the deletion itself: in order and apart, one change, positions after each
  (let ((b (tuatara.buffer:make-buffer "*many-places*" :text "abcdef")))
    (unwind-protect
         (progn
           (is equalp #(1 2) (tuatara.defaults::delete-spans b '((0 . 1) (2 . 4)) :replacements '("X" "")))
           (is string= "Xbef" (tuatara.buffer:buffer-string b))
           (tuatara.buffer:buffer-undo b)
           (is string= "abcdef" (tuatara.buffer:buffer-string b))
           (false (tuatara.defaults::delete-spans b '((1 . 1)))))
      (tuatara.buffer:kill-buffer b))))

(define-test (many-places one-matcher)
  ;; f, r r and :s find a pattern one way
  (flet ((kind (pattern &rest keys) (tuatara.defaults::matcher-kind (apply #'tuatara.defaults::make-pattern-matcher pattern keys))))
    (is eq :literal (kind "foo" :regex t))
    (is eq :regex (kind "f.o" :regex t))
    (is eq :regex (kind "foo" :regex t :compiled t))
    (is eq :literal (kind "f.o"))
    ;; literal text with letters beyond ASCII, case ignored: only the regular expression knows their other case
    (is eq :regex (kind "ä"))
    (is eq :literal (kind "Ä"))
    (multiple-value-bind (matcher problem) (tuatara.defaults::make-pattern-matcher "(" :regex t)
      (false matcher)
      (is string= "incomplete" problem)))
  (let ((text (format nil "Äpfel äpfel~%")))
    (true (tuatara.frontend.fake:fake-frame-showing (many-frame "% f ä RET" text) "Split: 2 places"))
    (is string= (format nil "Xpfel Xpfel~%") (many-text "C-x h M-x r e p l a c e - r e g e x p RET % ä RET X RET" text))
    (is string= (format nil "Xpfel Xpfel~%") (many-text "r r % ä / X RET" text))))

(define-test (many-places one-words-for-jobs)
  ;; every job at many places says what it does from one table
  (is string= "Pasting the rectangle's 2 rows" (tuatara.defaults::job-words :paste :doing "the rectangle's 2 rows"))
  (is string= "Writing at the other lines" (tuatara.defaults::job-words :type :doing "the other lines"))
  (is string= "Unwrapping" (tuatara.defaults::job-words :unwrap :doing))
  (is string= "cut the rectangle" (tuatara.defaults::job-words :cut :verb "the rectangle"))
  ;; and as they start, in the background
  (let ((text (format nil "one two~%one two~%")))
    (let ((tuatara-user:*cursors-limit* 1))
      (true (tuatara.frontend.fake:fake-frame-showing (many-frame '("V j t d" "") text)
                                                      "Deleting the other lines in the background")))
    (let ((tuatara.defaults::*rewrite-threshold* 0))
      (true (tuatara.frontend.fake:fake-frame-showing (many-frame '("l m v j y g g P" "") text)
                                                      "Pasting the rectangle's 2 rows in the background"))
      (true (tuatara.frontend.fake:fake-frame-showing (many-frame '("l m v j y g g m v j P" "") text)
                                                      "Pasting the rectangle's 2 rows in the background")))
    ;; and how they ended, one way
    (let ((tuatara-user:*split-limit* 1))
      (true (tuatara.frontend.fake:fake-frame-showing (many-frame '("% f o RET d" "") text) "Deleted 3 more matches of o, out of sight, in"))
      (true (tuatara.frontend.fake:fake-frame-showing (many-frame '("% f z RET" "") text) "Nothing matched z")))
    (let ((tuatara.defaults::*rewrite-threshold* 0))
      (true (tuatara.frontend.fake:fake-frame-showing (many-frame '("l m v j d" "") text) "Cut the rectangle's 2 rows in")))))

(define-test (many-places last-line-copied-as-a-line)
  ;; V y of a last line without a line end copies it as a whole line: p puts it in as one
  (is string= (format nil "three~%one~%two~%three") (many-text "g e V y g g p" (format nil "one~%two~%three")))
  (is string= (format nil "two~%one~%two~%three") (many-text "j V y g g p" (format nil "one~%two~%three"))))

(define-test (many-places position-kept-without-anchors)
  ;; a command that leaves point where it was makes no new anchor for the place it was
  (flet ((anchors (keys)
           (let ((made 0))
             (sb-int:encapsulate 'tuatara.text:make-anchor 'count-anchors
                                 (lambda (real &rest arguments) (incf made) (apply real arguments)))
             (unwind-protect (run-keys keys :text "kia ora")
               (sb-int:unencapsulate 'tuatara.text:make-anchor 'count-anchors))
             made)))
    (let ((few (anchors "C-l")) (many (anchors (format nil "~{~a~^ ~}" (make-list 41 :initial-element "C-l")))))
      (true (< (- many few) 5) "~d anchors more for 40 commands" (- many few)))))
