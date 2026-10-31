;;;; test/objects.lisp --- Selecting words, sentences, paragraphs and pairs, and filling paragraphs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test objects
  :description "m w W b B s S p P select words, symbols, sentences and paragraphs and grow them pressed again, mixing freely; m i and m a select within pairs of delimiters, a level out each press; g q, g Q and M-q fill paragraphs to the fill column.")

(defun selection-after (keys text &key (width 80))
  "The text selected after KEYS are typed on TEXT, or NIL when nothing is."
  (multiple-value-bind (f editor) (run-keys keys :text text :width width :height 10)
    (declare (ignore f))
    (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
      (multiple-value-bind (s e) (tuatara.buffer:buffer-region b)
        (and s (tuatara.buffer:buffer-mark-active-p b) (tuatara.buffer:buffer-string b :start s :end e))))))

(defparameter *paragraphs* (format nil "A1~%A2~%~%B1~%B2~%~%C1~%")
  "Three paragraphs.")

(define-test (objects words-and-symbols)
  (let ((text "This buffer is for text that is not saved"))
    ;; on "that": two words forward, one back
    (is string= "text that is" (selection-after "w w w w w l m w m w m b" text))
    ;; from the gap between two words: m w the one ahead, m b the one behind
    (is string= "text" (selection-after "w w w w m w" text))
    (is string= "for" (selection-after "w w w w m b" text)))
  ;; the same on the first line of *lisp-scratch*
  (is string= "text and serves"
      (selection-after "w w w w w l m w m w m b" "This buffer holds unsaved text and serves as a space for Lisp evaluation."))
  (is string= "alpha beta" (selection-after "C-u 2 m w" "alpha beta gamma"))
  ;; at the end of the text, the last word
  (is string= "gamma" (selection-after "g l m w" "alpha beta gamma  "))
  ;; a symbol leaves the prose punctuation around it
  (is string= "bar" (selection-after "w w w m W" "see `foo', bar."))
  (is string= "foo-bar_baz" (selection-after "l m W" "(foo-bar_baz qux)"))
  ;; pressed again, a symbol grows by a balanced expression
  (is string= "x (y z)" (selection-after "w m W m W" "(setq x (y z))"))
  (is string= "No word at or before point"
      (multiple-value-bind (f editor) (run-keys "m w" :text "  ... ")
        (declare (ignore editor))
        (tuatara.frontend.fake:fake-screen-line f 7))))

(define-test (objects words-by-octets)
  ;; letters past ASCII make words, a dash past ASCII and a stray octet do not
  (flet ((point-after (keys text) (session-point (nth-value 1 (run-keys keys :text text :width 80 :height 10)))))
    (is = 6 (point-after "M-f" "héllo wörld"))
    (is = 13 (point-after "M-f M-f" "héllo wörld"))
    (is = 7 (point-after "g l M-b" "héllo wörld"))
    (is = 1 (point-after "M-f" "a—b"))
    (is = 5 (point-after "M-f M-f" "a—b"))
    (is = 4 (point-after "g l M-b" "a—b"))
    (let ((stray (format nil "ab~acd" (code-char (+ #xDC00 #xE9)))))
      (is = 2 (point-after "M-f" stray))
      (is = 5 (point-after "M-f M-f" stray)))
    (is string= "wörld" (selection-after "w w m w" "héllo wörld"))
    ;; across millions of octets that make no word, at once, both ways
    (let ((text (concatenate 'string (make-string 8000000 :initial-element (code-char 0)) "word")))
      (let ((start (get-internal-real-time)))
        (is = 8000004 (point-after "M-f" text))
        (is = 8000000 (point-after "g l M-b" text))
        (is = 0 (point-after "g l M-b M-b" text))
        (true (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 3))))
    ;; no further than *text-scan-limit*, said at once
    (let ((tuatara.defaults::*text-scan-limit* 1000)
          (text (concatenate 'string (make-string 2000 :initial-element #\Space) "word")))
      (multiple-value-bind (f editor) (run-keys "M-f" :text text :width 80 :height 10)
        (true (tuatara.frontend.fake:fake-frame-showing f "No word within 1000 bytes"))
        (is = 0 (session-point editor))))
    ;; a symbol, and the gap before one, no further than *text-scan-limit* either
    (let ((tuatara.defaults::*text-scan-limit* 1000))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "m W" :text (make-string 3000 :initial-element #\a) :width 80 :height 10)
             "No end of the symbol within 1000 bytes"))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "g l m W" :text (make-string 3000 :initial-element #\a) :width 80 :height 10)
             "No start of the symbol within 1000 bytes"))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys "m W" :text (concatenate 'string (make-string 3000 :initial-element #\() "word") :width 80 :height 10)
             "No symbol at or before point")))))

(define-test (objects sentences-and-paragraphs)
  (let ((text "One thing. Two three. Four five."))
    (is string= "Two three." (selection-after "w w w w m s" text))
    (is string= "Two three. Four five." (selection-after "w w w w m s m s" text))
    (is string= "One thing. Two three." (selection-after "w w w w m S m S" text))
    ;; objects mix: a sentence and the word before it
    (is string= "thing. Two three." (selection-after "w w w w m s m b" text)))
  ;; a paragraph comes with one blank line, the one before it, the first the one after
  (is string= (format nil "~%B1~%B2~%") (selection-after "j j j m p" *paragraphs*))
  (is string= (format nil "A1~%A2~%~%") (selection-after "m p" *paragraphs*))
  ;; from the blank line between two: m p the one below, m P the one above
  (is string= (format nil "~%B1~%B2~%") (selection-after "j j m p" *paragraphs*))
  (is string= (format nil "A1~%A2~%~%") (selection-after "j j m P" *paragraphs*))
  (is string= (format nil "~%B1~%B2~%~%C1~%") (selection-after "j j j m p m p" *paragraphs*)))

(define-test (objects pairs)
  (let ((text "(a (b) c)"))
    (is string= "b" (selection-after "l l l l m i (" text))
    ;; either half of the pair; pressed again, the pair around it
    (is string= "b" (selection-after "l l l l m i )" text))
    (is string= "a (b) c" (selection-after "l l l l m i ( m i" text))
    (is string= "(b)" (selection-after "l l l l m a )" text))
    ;; on a delimiter, that one, without asking
    (is string= "a (b) c" (selection-after "m i" text))
    (is string= "a (b) c" (selection-after "l l l l C-u 2 m i (" text)))
  (is string= "word" (selection-after "w w m i" "say \"word\" now"))
  ;; quotes do not nest: a level out is one more on either side
  (is string= "writing on paper." (selection-after "w w w w l m i \"" "\"No use \"writing on paper.\" That\""))
  (is string= "No use \"writing on paper.\" That" (selection-after "w w w w l m i \" m i" "\"No use \"writing on paper.\" That\""))
  (multiple-value-bind (f editor) (run-keys "m i x" :text "abc")
    (declare (ignore editor))
    (true (search "Unsupported delimiter `x'" (tuatara.frontend.fake:fake-screen-line f 7))))
  (multiple-value-bind (f editor) (run-keys "l m i (" :text "abc")
    (declare (ignore editor))
    (true (search "No '(' found near cursor" (tuatara.frontend.fake:fake-screen-line f 7)))))

(define-test (objects filling)
  (flet ((filled (keys text) (session-text (nth-value 1 (run-keys keys :text text)))))
    (is string= (format nil "The quick brown fox~%jumps over the lazy~%dog and keeps~%running far away.~%~%Second paragraph stays.~%")
        (filled "C-u 2 0 C-x f g Q RET" (format nil "The quick brown fox jumps over the lazy dog and keeps running far away.~%~%Second paragraph stays.~%")))
    ;; the text before the words is kept: an indentation, a quotation's >
    (is string= (format nil "  Indented text that~%  is long enough to~%  need a break~%  somewhere.~%")
        (filled "C-u 2 0 C-x f M-q RET" (format nil "  Indented text that is long enough to need a break somewhere.~%")))
    (is string= (format nil "> quoted line one~%> quoted line two~%> that is longer~%> than the rest~%")
        (filled "C-u 2 0 C-x f j M-q RET" (format nil "> quoted line one~%> quoted line two that is longer than the rest~%")))
    ;; a list item continues under its words
    (is string= (format nil "- a list item that~%  goes on for quite~%  a while~%")
        (filled "C-u 2 0 C-x f M-q RET" (format nil "- a list item that goes on for quite a while~%")))
    ;; in Org's indented view, counted from the window's margin: under a second-level headline, 4 columns in
    (is string= (format nil "* A~%** B~%- one two three~%  four five six~%  seven~%")
        (filled "M-x o r g - m o d e RET C-u 2 0 C-x f j j M-q RET" (format nil "* A~%** B~%- one two three four five six seven~%")))
    ;; what is no prose in Org is never filled: tables, planning lines, drawers, blocks, fixed-width lines
    (let ((org (format nil "* A~%SCHEDULED: <2026-10-20 Tue>~%:PROPERTIES:~%:ID: abc~%:END:~%Some text here.~%~%| a_b | 10 | 50% |~%|-----+----+-----|~%| c   | 2  | 3%  |~%~%#+begin_src python~%def f(x):~%    return x + 1~%print(f(2))~%#+end_src~%~%: fixed one~%: fixed two~%")))
      (dolist (line '(1 2 3 7 8 12 13 17 18))
        (is string= org (filled (format nil "M-x o r g - m o d e RET g g ~{~a ~}C-u 2 0 0 M-q" (make-list line :initial-element "j")) org)
            "line ~d" line))
      ;; a selection over all of it fills only the prose
      (is string= org (filled "M-x o r g - m o d e RET C-u 2 0 0 C-x f % g q RET" org)))
    ;; nor in Markdown: code fences and tables
    (let ((md (format nil "```python~%def f():~%    return 1~%```~%~%| a | b |~%|---|---|~%| 1 | 2 |~%")))
      (dolist (line '(1 2 5 6))
        (is string= md (filled (format nil "M-x m a r k d o w n - m o d e RET g g ~{~a ~}C-u 2 0 0 M-q" (make-list line :initial-element "j")) md)
            "line ~d" line)))
    ;; each item of a list is a paragraph of its own: V on one, g q, the others left as they are
    (is string= (format nil "- first item that~%  goes on for a~%  while~%- second item that goes on for a while too~%1. numbered item that goes on and on~%")
        (filled "C-u 2 0 C-x f V g q RET"
                (format nil "- first item that goes on for a while~%- second item that goes on for a while too~%1. numbered item that goes on and on~%")))
    (is string= (format nil "- first one~%- second item that~%  goes on for a~%  while more~%- third~%")
        (filled "C-u 2 0 C-x f j M-q RET" (format nil "- first one~%- second item that goes on~%  for a while more~%- third~%")))
    (is string= (format nil ";; - one item that~%;;   is long enough~%;; - two~%")
        (filled "M-x l i s p - m o d e RET C-u 2 0 C-x f M-q RET" (format nil ";; - one item that is long enough~%;; - two~%")))
    ;; a numbered item goes on under its words; several items selected, each filled alone
    (is string= (format nil "1. numbered item~%   that goes on~%2. short~%")
        (filled "C-u 1 6 C-x f M-q RET" (format nil "1. numbered item that goes on~%2. short~%")))
    (is string= (format nil "- one item that~%  goes on~%- two items that~%  go on~%~%Text after that is not selected at all.~%")
        (filled "C-u 1 6 C-x f V j g q RET"
                (format nil "- one item that goes on~%- two items that go on~%~%Text after that is not selected at all.~%")))
    ;; g q fills only the lines selected, the rest of their paragraph left as it is
    (is string= (format nil "first line stays as it is written~%second line that is~%long and filled~%third line stays as it is written too~%")
        (filled "C-u 2 0 C-x f j V g q RET"
                (format nil "first line stays as it is written~%second line that is long and filled~%third line stays as it is written too~%")))
    (is string= (format nil "first line stays as it is written~%second one and the~%third one, filled~%together~%fourth stays as it is written~%")
        (filled "C-u 2 0 C-x f j V j g q RET"
                (format nil "first line stays as it is written~%second one and the third~%one, filled together~%fourth stays as it is written~%")))
    ;; a few characters of one line selected: that line alone
    (is string= (format nil "first line stays as it is written~%second line that is~%long and filled~%third line stays as it is written too~%")
        (filled "C-u 2 0 C-x f j w v l l g q RET"
                (format nil "first line stays as it is written~%second line that is long and filled~%third line stays as it is written too~%")))
    ;; the first and the last line of a block, each alone
    (is string= (format nil "first line that is~%long and filled~%second line stays as it is written~%")
        (filled "C-u 2 0 C-x f V g q RET"
                (format nil "first line that is long and filled~%second line stays as it is written~%")))
    (is string= (format nil "first line stays as it is written~%last line that is~%long and filled~%")
        (filled "C-u 2 0 C-x f j V g q RET"
                (format nil "first line stays as it is written~%last line that is long and filled~%")))
    ;; g q fills every paragraph selected
    (is string= (format nil "one two three four~%five six seven eight~%~%nine ten eleven~%twelve thirteen~%fourteen~%")
        (filled "C-u 2 0 C-x f % g q RET" (format nil "one two three four five six seven eight~%~%nine ten eleven twelve thirteen fourteen~%")))
    ;; in Lisp, the comment at point, its ;; on every line; the code is left alone
    (is string= (format nil ";; A comment in Lisp that is~%;; long enough to be filled at~%;; thirty columns.~%(defun f () nil)~%")
        (filled "M-x l i s p - m o d e RET C-u 3 0 C-x f M-q RET"
                (format nil ";; A comment in Lisp that is long enough to be filled at thirty columns.~%(defun f () nil)~%")))
    (is string= (format nil "(defun f () nil)~%")
        (filled "M-x l i s p - m o d e RET M-q" (format nil "(defun f () nil)~%"))))
  ;; the column is 80 unless set: a line of 75 stays whole
  (let ((line (format nil "~{~a~^ ~}~%" (loop repeat 15 collect "word"))))
    (is string= line (session-text (nth-value 1 (run-keys "M-q RET" :text line))))
    (let ((tuatara-user:*fill-column* 40))
      (is string= (format nil "~{~a~^ ~}~%~{~a~^ ~}~%" (loop repeat 8 collect "word") (loop repeat 7 collect "word"))
          (session-text (nth-value 1 (run-keys "M-q RET" :text line))))))
  ;; a count fills to it, and the buffer keeps it: the next paragraph too
  (multiple-value-bind (f editor)
      (run-keys "C-u 2 0 g Q g e k M-q RET"
                :text (format nil "one two three four five six seven~%~%eight nine ten eleven twelve thirteen~%"))
    (is string= (format nil "one two three four~%five six seven~%~%eight nine ten~%eleven twelve~%thirteen~%")
        (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Fill column 20 for this buffer")))
  ;; a git commit message is filled to 72
  (with-test-directory (dir)
    (let ((path (merge-pathnames "COMMIT_EDITMSG" dir)))
      (put-file path (format nil "Summary~%~%~{~a~^ ~}~%# Please enter the commit message.~%"
                             (loop repeat 16 collect "word")))
      (multiple-value-bind (f editor) (run-keys "j j M-q RET" :file (namestring path))
        (declare (ignore f))
        (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= "Git commit" (tuatara.buffer:buffer-local b :mode-name))
          (is = 72 (tuatara.defaults::buffer-fill-column b))
          (is string= (format nil "Summary~%~%~{~a~^ ~}~%~{~a~^ ~}~%# Please enter the commit message.~%"
                              (loop repeat 14 collect "word") (loop repeat 2 collect "word"))
              (tuatara.buffer:buffer-string b))
          (tuatara.buffer:kill-buffer b))))))

(define-test (objects mark-run)
  (let ((text "This buffer is for text that is not saved"))
    (flet ((after (keys) (selection-after (format nil "w w w w w l ~a" keys) text :width 100)))
      ;; M is a word selected, and each letter one more m-prefixed press
      (is string= "that" (after "M"))
      (is string= "that is" (after "M w"))
      (is string= "text that" (after "M b"))
      (is string= "that is not saved" (after "M C-u 3 w"))
      (is string= "that is not" (after "M w ."))
      ;; u and U step back and forward through the run
      (is string= "that is" (after "M w w u"))
      (is string= "that is" (after "M w w u u U"))
      ;; a key that does nothing, or v, leaves the run standing
      (is string= "that is" (after "M q w"))
      (is string= "that is" (after "M v w"))
      ;; the line's and the text's edges, and whole lines
      (is string= text (after "M g h g l"))
      (is string= "that is not saved" (after "M J"))
      ;; m p and m s still reach the run
      (false (after "M M"))))
  ;; any other key ends the run and does its job on the selection
  (multiple-value-bind (f editor) (run-keys "w w w w w l M w d" :text "This buffer is for text that is not saved")
    (declare (ignore f))
    ;; the words go; the spaces either side of them stay
    (is string= "This buffer is for text  not saved" (session-text editor))
    (false (tuatara.command:editor-transient-map editor)))
  ;; a selection made already is taken into the run and grown from its end
  (is string= "This" (selection-after "v l l l M w" "This buffer is for text"))
  (multiple-value-bind (f editor) (run-keys "M" :text "alpha beta" :width 100)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "Mark run: w/b words"))))

(define-test (objects wrapping)
  ;; a delimiter on a selection puts its pair around it, either half
  (is string= "say \"word\" now" (session-text (nth-value 1 (run-keys "w m w \"" :text "say word now"))))
  (is string= "(foo) bar" (session-text (nth-value 1 (run-keys "m w )" :text "foo bar"))))
  (is string= "[foo] bar" (session-text (nth-value 1 (run-keys "m w [" :text "foo bar"))))
  ;; and takes it off when it stands just outside the selection
  (is string= "say word now" (session-text (nth-value 1 (run-keys "w w m i \" \"" :text "say \"word\" now"))))
  ;; the pair and all: another pair around it
  (is string= "say \"\"word\"\" now" (session-text (nth-value 1 (run-keys "w w m a \" \"" :text "say \"word\" now"))))
  ;; one undo takes a wrap back
  (is string= "foo bar" (session-text (nth-value 1 (run-keys "m w ( u" :text "foo bar"))))
  ;; no selection: nothing
  (multiple-value-bind (f editor) (run-keys "(" :text "foo")
    (is string= "foo" (session-text editor))
    (true (search "is undefined" (tuatara.frontend.fake:fake-screen-line f 7))))
  ;; a key that does something keeps doing it: > indents rather than wraps
  (is string= "foo" (session-text (nth-value 1 (run-keys "m w >" :text "foo")))))

(define-test (objects rectangles)
  (flet ((text-of (keys text) (session-text (nth-value 1 (run-keys keys :text text))))
         (store () tuatara.defaults::*killed-rectangle*))
    ;; m v holds the character at point; j sizes the block; y copies it to its own store
    (is string= "babc
edef
ghi" (text-of "l m v j y g g P" "abc
def
ghi"))
    (is equal '("b" "e") (store))
    ;; d cuts it; point stays on its row, at the block's left column
    (multiple-value-bind (f editor) (run-keys "l m v j d" :text "abc
def
ghi")
      (declare (ignore f))
      (is string= "ac
df
ghi" (session-text editor))
      (is = 4 (session-point editor))
      (is equal '("b" "e") (store)))
    ;; one undo puts a cut rectangle back
    (is string= "abc
def" (text-of "l m v j d u" "abc
def"))
    ;; a count pastes each row that many times across
    (is string= "bbabc
eedef" (text-of "l m v j y g g C-u 2 P" "abc
def"))
    ;; over a rectangle of as many rows it takes the block's place, one undo
    (is string= "bbc
eef
ghi" (text-of "l m v j y g g m v j P" "abc
def
ghi"))
    (is string= "abc
def
ghi" (text-of "l m v j y g g m v j P u" "abc
def
ghi"))
    ;; of a different number of rows it is refused
    (multiple-value-bind (f editor) (run-keys "l m v j y g g m v P" :text "abc
def")
      (is string= "abc
def" (session-text editor))
      (true (search "Rectangle row mismatch" (tuatara.frontend.fake:fake-screen-line f 7))))
    ;; p does not paste over a rectangle
    (multiple-value-bind (f editor) (run-keys "m v p" :text "abc")
      (is string= "abc" (session-text editor))
      (true (search "p is undefined on a rectangle" (tuatara.frontend.fake:fake-screen-line f 7))))
    ;; a short line: the corner keeps its column past its end, the row padded in the store
    (is string= "abd
x
abd" (text-of "l l m v j j d" "abcd
x
abcd"))
    (is equal '("c" " " "c") (store))
    ;; pasted below the last line, the rows beyond make lines of their own
    (is string= "abcd
x
abcdc
     
    c" (text-of "l l m v j j y g e P" "abcd
x
abcd"))
    ;; l goes on past the end of a line, and j keeps that column
    (is string= "
" (text-of "m v l l l j d" "ab
abcd"))
    (is equal '("ab  " "abcd") (store))
    ;; h stops at the start of the line
    (is string= "bc
abc" (text-of "l m v h h h d" "abc
abc"))
    ;; c on one row: its block emptied for INSERT state
    (is string= "XYc" (text-of "m v l c X Y C-g" "abc"))
    (is equal '("ab") (store))
    ;; c on more: a cursor on every row, typing at them all
    (multiple-value-bind (f editor) (run-keys "m v j c Z C-g C-g" :text "abc
abc")
      (declare (ignore f))
      (is string= "Zbc
Zbc" (session-text editor))
      (is = 5 (session-point editor)))
    ;; past the limit, the rows in sight hold the cursors, the others written in the background
    (let ((tuatara.defaults::*rectangle-cursors-limit* 1))
      (is string= "Zbc
Zbc" (session-text (nth-value 1 (run-keys-in-turns '("m v j c Z C-g" "") :text "abc
abc")))))
    ;; a wrap key wraps each row, a short one padded out to the block
    (is string= "(a)b
(a)b" (text-of "m v j (" "ab
ab"))
    (is string= "a[bc]
a[  ]" (text-of "l m v l j [" "abc
a"))
    ;; the highlight is the block on each row, and the cursor stands past a short line
    (let ((f (run-keys "l m v j" :text "abc
abc
abc")))
      (is equal '((1 2 :region)) (aref (tuatara.frontend.fake:fake-spans f) 0))
      (is equal '((1 2 :region)) (aref (tuatara.frontend.fake:fake-spans f) 1))
      (is equal '() (aref (tuatara.frontend.fake:fake-spans f) 2))
      (true (search "Rectangle: any motion" (tuatara.frontend.fake:fake-screen-line f 7))))
    (is equal '(1 . 3) (tuatara.frontend.fake:fake-cursor (run-keys "l l m v j" :text "abcd
x")))
    ;; m v again cancels, and so does C-g; v then starts a plain selection
    (multiple-value-bind (f editor) (run-keys "m v m v" :text "abc")
      (true (search "Rectangle: cancelled" (tuatara.frontend.fake:fake-screen-line f 7)))
      (false (tuatara.buffer:buffer-mark-active-p (session-buffer editor))))
    (false (tuatara.buffer:buffer-local (session-buffer (nth-value 1 (run-keys "m v j C-g" :text "abc
abc")))
                                        :rectangle))
    (multiple-value-bind (f editor) (run-keys "m v j v l" :text "abc
abc")
      (false (tuatara.command:rectangle-live-p (session-buffer editor)))
      (true (search "Linear selection:" (tuatara.frontend.fake:fake-screen-line f 7))))
    (true (search "Lines:" (tuatara.frontend.fake:fake-screen-line (run-keys "V" :text "abc") 7)))))

(define-test (objects banking)
  (flet ((kill (editor) (first (tuatara.command:editor-kill-ring editor)))
         (echo (f) (tuatara.frontend.fake:fake-screen-line f 7)))
    (let ((five (format nil "a~%b~%c~%d~%e")))
      ;; lines banked apart are copied as one kill, and are spent by it
      (multiple-value-bind (f editor) (run-keys "m l j j m l y" :text five)
        (is string= (format nil "a~%c~%") (kill editor))
        (true (search "Copied 2 lines" (echo f)))
        (false (tuatara.defaults::banked-p (session-buffer editor))))
      ;; and deleted as one, one undo bringing them back
      (multiple-value-bind (f editor) (run-keys "m l j j m l d" :text five)
        (declare (ignore f))
        (is string= (format nil "b~%d~%e") (session-text editor))
        (is string= (format nil "a~%c~%") (kill editor)))
      (is string= five (session-text (nth-value 1 (run-keys "m l j j m l d u" :text five))))
      ;; the selection made at the time counts with them
      (is string= (format nil "a~%c~%") (kill (nth-value 1 (run-keys "m l j j v l y" :text five))))
      ;; pressed again on a banked line, it comes out
      (true (search "Unbanked this line (0 total)" (echo (run-keys "m l m l" :text five))))
      (true (search "Banked this line (1 total): navigate, then y/d/p" (echo (run-keys "m l" :text five))))
      ;; a count banks that many lines
      (true (search "Banked 3 lines (3 total)" (echo (run-keys "C-u 3 m l" :text five))))
      ;; a selection all banked comes out; one partly banked has the rest banked
      (true (search "Unbanked 2 lines (0 total)" (echo (run-keys "V j m l g g V j m l" :text five))))
      (true (search "Banked 2 lines (2 total)" (echo (run-keys "m l g g V j m l" :text five))))
      ;; m u takes one line out, m U the run point is on, m DEL the lot
      (multiple-value-bind (f editor) (run-keys "V j j m l k m u y" :text five)
        (declare (ignore f))
        (is string= (format nil "a~%c~%") (kill editor)))
      (true (search "Unbanked 2 lines (1 total)" (echo (run-keys "m l j m l j j m l k k m U" :text five))))
      (true (search "No banked section at point" (echo (run-keys "j j m U" :text five))))
      (true (search "Discarded 2 banked lines" (echo (run-keys "m l j m l m DEL" :text five))))
      (true (search "No banked lines" (echo (run-keys "m DEL" :text five))))
      ;; C-g lets go of a selection, not of the bank
      (is string= (format nil "a~%") (kill (nth-value 1 (run-keys "m l C-g j y" :text five))))
      ;; banked lines follow the text through edits
      (is string= (format nil "c~%") (kill (nth-value 1 (run-keys "j j m l g g O x C-g y" :text five))))
      ;; a rectangle is the selection seen, and wins; the bank stays for later
      (multiple-value-bind (f editor) (run-keys "m l j m v y" :text five)
        (declare (ignore f))
        (is equal '("b") tuatara.defaults::*killed-rectangle*)
        (true (tuatara.defaults::banked-p (session-buffer editor))))
      ;; the empty line after the last newline has nothing to bank
      (true (search "Nothing to bank: empty final line" (echo (run-keys "j m l" :text (format nil "a~%")))))
      ;; shown in their face, to the window's edge, so an empty one shows too
      (is equal '((0 40 :banked-line)) (aref (tuatara.frontend.fake:fake-spans (run-keys "m l" :text five)) 0))
      (is equal '((0 40 :banked-line)) (aref (tuatara.frontend.fake:fake-spans (run-keys "j m l" :text (format nil "a~%~%c"))) 1))
      ;; the selection is shown over them
      (is equal '((0 1 (:region :banked-line)) (1 40 :banked-line))
          (aref (tuatara.frontend.fake:fake-spans (run-keys "m l v l" :text five)) 0)))
    ;; p puts the last kill in their place, the lines below kept as lines
    (multiple-value-bind (f editor) (run-keys "y j m l j j m l p" :text (format nil "X~%a~%b~%c"))
      (is string= (format nil "X~%X~%b~%") (session-text editor))
      (is = 3 (session-point editor))
      (true (search "Replaced 2 lines" (echo f))))))

(define-test (objects banks-keep-their-lines)
  (let ((four (format nil "aaa~%bbb~%ccc~%ddd~%")))
    (flet ((kill (keys &optional (text four))
             (first (tuatara.command:editor-kill-ring (nth-value 1 (run-keys keys :text text))))))
      ;; lines typed at a bank's edges stay out of it
      (is string= (format nil "bbb~%") (kill "j m l O N E W C-g g e y"))
      (is string= (format nil "bbb~%") (kill "j m l o N E W C-g g e y"))
      (is string= (format nil "bbb~%") (kill "j m l A RET N E W C-g g e y"))
      (is string= (format nil "bbb~%") (kill "j m l k A RET N E W C-g g e y"))
      (is string= (format nil "bbb~%") (kill "j m l j i X Y C-g g e y"))
      ;; typed on the banked line itself, it is part of it, as is a line joined to it
      (is string= (format nil "Xbbb~%") (kill "j m l i X C-g g e y"))
      (is string= (format nil "bbbccc~%") (kill "j m l j i DEL C-g g e y"))
      ;; an empty line banked stays banked, typed on or with a line opened above it
      (is string= (format nil "~%") (kill "j m l O X C-g g e y" (format nil "aaa~%~%ccc~%")))
      (is string= (format nil "X~%") (kill "j m l i X C-g g e y" (format nil "aaa~%~%ccc~%"))))
    ;; d takes only the banked line
    (multiple-value-bind (f editor) (run-keys "j m l o N E W C-g g g d" :text four)
      (declare (ignore f))
      (is string= (format nil "aaa~%NEW~%ccc~%ddd~%") (session-text editor))
      (is string= (format nil "bbb~%") (first (tuatara.command:editor-kill-ring editor))))))

(define-test (objects digraphs)
  (flet ((text-of (keys text) (session-text (nth-value 1 (run-keys keys :text text))))
         (echo (f) (tuatara.frontend.fake:fake-screen-line f 7)))
    ;; SPC i & types one character from its digraph, in NORMAL state
    (is string= "éabc" (text-of "SPC i & e '" "abc"))
    (is string= "→abc" (text-of "SPC i & - >" "abc"))
    ;; three keys where two start a longer one
    (is string= "↘" (text-of "SPC i & ! ! >" ""))
    ;; a count types it that many times
    (is string= "ééé" (text-of "C-u 3 SPC i & e '" ""))
    ;; keys that make none are named
    (true (search "No digraph e" (echo (run-keys "SPC i & e RET" :text "abc"))))
    ;; with a selection, the character wraps it with its partner
    (is string= "«foo» bar" (text-of "m w SPC i & < <" "foo bar"))
    ;; SPC i . types digraphs after & in INSERT state: the rfc1345 input method, m in the mode line
    (is string= "éx" (text-of "SPC i . i & e ' x C-g" ""))
    (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC i ." :text "") "Digraphs (rfc1345) on when you enter INSERT state"))
    (is string= "m" (let ((b (tuatara.buffer:make-buffer "*method*")))
                      (unwind-protect (progn (tuatara.defaults::activate-input-method "rfc1345" b)
                                             (tuatara.defaults::input-method-mode-line nil b))
                        (tuatara.buffer:kill-buffer b))))
    ;; keys that start none are typed as they are
    (is string= "&#" (text-of "SPC i . i & # C-g" ""))
    ;; SPC i - turns it off
    (is string= "&e" (text-of "SPC i . SPC i - i & e C-g" ""))
    (true (search "No input method is on" (echo (run-keys "SPC i -" :text ""))))
    ;; the chart
    (let ((f (run-keys "M-x l i s t - d i g r a p h s RET" :text "" :height 20)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Digraphs")))
    (is = 1893 (length tuatara.defaults::*digraph-table*))))

(define-test (objects unicode-names-as-the-standard-writes-them)
  ;; one name for a character wherever it is shown: none for a control character or a code
  ;; given no character, the standard's capitals for the rest
  (is equal '("LATIN SMALL LETTER E WITH ACUTE" "SPACE" "NO-BREAK SPACE" "" "" "" "")
      (mapcar (lambda (code) (tuatara.defaults::unicode-name (code-char code))) '(#xE9 #x20 #xA0 #x7F #x9 #xE000 #x378)))
  (false (find "UE000" (tuatara.defaults::list-character-names) :key #'tuatara.command::candidate-string :test #'string=)))

(define-test (objects input-methods-at-many-places)
  ;; a letter made of several keys (postfix, prefix, digraph) reaches every split place and every cursor
  (flet ((typed (method keys text)
           (substitute #\| #\Newline
                       (session-text (nth-value 1 (run-keys (format nil "M-x s e t - i n p u t - m e t h o d RET ~{~a ~}RET ~a"
                                                                    (map 'list #'string method) keys)
                                                            :text text))))))
    (is string= "fåoo x fåoo" (typed "swedish-postfix" "% f o o RET i a a C-g" "foo x foo"))
    (is string= "fooår x fooår" (typed "swedish-postfix" "% f o o RET a a a r C-g" "foo x foo"))
    (is string= "fä x fä" (typed "swedish-postfix" "% f o o RET c a e C-g" "foo x foo"))
    (is string= "fąoo x fąoo" (typed "polish-slash" "% f o o RET i / a C-g" "foo x foo"))
    (is string= "féoo x féoo" (typed "rfc1345" "% f o o RET i & e ' C-g" "foo x foo"))
    (is string= "åfoo|åfoo|" (typed "swedish-postfix" "t i a a C-g" (format nil "foo~%foo~%")))
    (is string= "ąfoo|ąfoo|" (typed "polish-slash" "t i / a C-g" (format nil "foo~%foo~%")))))

(define-test (objects input-methods)
  (let ((tuatara-user:*modal-editing* nil)
        (tuatara-user:*default-input-method* nil))
    (flet ((typed (keys &optional (method "swedish-postfix"))
             (session-text (nth-value 1 (run-keys (format nil "M-x s e t - i n p u t - m e t h o d RET ~{~a ~}RET ~a"
                                                          (map 'list #'string method) keys)
                                                  :text "")))))
      ;; swedish-postfix, as Emacs's: aa å, ae ä, oe ö, e' é; aaa gives back aa; a key no rule goes on with settles
      (is string= "hår" (typed "h a a r"))
      (is string= "äö" (typed "a e o e"))
      (is string= "é!" (typed "e ' !"))
      (is string= "aa" (typed "a a a"))
      (is string= "ax" (typed "a x"))
      ;; keys waiting stay as typed when another command comes
      (is string= "a" (typed "a C-b"))
      ;; polish-slash, a prefix method: /a ą, // a slash
      (is string= "ą/" (typed "/ a / /" "polish-slash"))
      ;; the digraphs: &e' é, & alone and keys that start none as typed
      (is string= "é&#" (typed "& e ' & #" "rfc1345"))
      ;; one undo step with the typing around it
      (is string= "" (typed "h a a r C-/"))
      ;; a count types the key itself
      (is string= "aaa" (typed "C-u 3 a"))
      ;; C-\ turns it off and on again
      (is string= "aaå" (typed "C-\\ a a C-\\ a a")))
    ;; C-\ with a default turns it on at once
    (let ((tuatara-user:*default-input-method* "danish-postfix"))
      (is string= "æ" (session-text (nth-value 1 (run-keys "C-\\ a e" :text "")))))
    ;; the keys waiting are underlined, and the mode line shows the method's title
    (let ((f (run-keys "M-x s e t - i n p u t - m e t h o d RET s w e d i s h - p o s t f i x RET x a" :text "" :width 100)))
      (true (tuatara.frontend.fake:fake-frame-showing f "SV<"))))
  ;; your own under SPC i, made into commands
  (let ((tuatara-user:*input-method-keys* '(("s" "swedish" "swedish-postfix") ("&" "taken" "rfc1345") ("x" "none" "no-such-method"))))
    (is string= "å" (session-text (nth-value 1 (run-keys "SPC i s i a a C-g" :text ""))))
    (true (tuatara.command:find-command 'tuatara.defaults::input-method-swedish)))
  ;; its rules listed
  (true (tuatara.frontend.fake:fake-frame-showing
         (run-keys "M-x d e s c r i b e - i n p u t - m e t h o d RET s w e d i s h - p o s t f i x RET" :text "" :height 30)
         "AA          Å"))
  ;; the step the rules take, as Quail takes them
  (let ((m (tuatara.defaults::find-input-method "swedish-postfix")))
    (is equal '("" "a") (multiple-value-list (tuatara.defaults::input-method-step m "a")))
    (is equal '("" "aa") (multiple-value-list (tuatara.defaults::input-method-step m "aa")))
    (is equal '("aa" "") (multiple-value-list (tuatara.defaults::input-method-step m "aaa")))
    (is equal '("åx" "") (multiple-value-list (tuatara.defaults::input-method-step m "aax")))))

(define-test (objects insert-char)
  (let ((tuatara-user:*modal-editing* nil))
    (flet ((typed (keys) (session-text (nth-value 1 (run-keys keys :text "" :width 100 :height 12)))))
      (is string= "é" (typed "C-x 8 RET L A T I N SPC S M A L L SPC L E T T E R SPC E SPC W I T H SPC A C U T E RET"))
      (is string= (string (code-char #x1F600)) (typed "C-x 8 RET U + 1 F 6 0 0 RET"))
      (is string= "ééé" (typed "C-u 3 C-x 8 RET # x e 9 RET"))
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x 8 RET # x z z RET" :text "" :width 100) "No character is called #xzz"))))
  (is = #xE9 (tuatara.defaults::character-code-from "latin small letter e with acute"))
  (is = #xE9 (tuatara.defaults::character-code-from "e9"))
  (is = #xE9 (tuatara.defaults::character-code-from "#o351"))
  (is = 233 (tuatara.defaults::character-code-from "#10r233"))
  (false (tuatara.defaults::character-code-from "#x110000"))
  (false (tuatara.defaults::character-code-from "no such name")))

(define-test (objects mark-ring)
  ;; set-mark-command and its ring, against Emacs 30's results for the same keys (point, mark, active, ring)
  (let ((tuatara-user:*modal-editing* nil))
    (flet ((after (keys &optional repeat-pop)
             (let ((tuatara-user:*set-mark-command-repeat-pop* repeat-pop))
               (let* ((editor (nth-value 1 (run-keys keys :text (format nil "abc~%def~%ghi~%jkl~%"))))
                      (b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (list (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-mark b)
                       (and (tuatara.buffer:buffer-mark-active-p b) t)
                       (mapcar (lambda (a) (tuatara.defaults::anchor-in b a)) (tuatara.defaults::mark-ring b)))))))
      (is equal '(0 0 t ()) (after "C-SPC"))
      (is equal '(0 0 nil ()) (after "C-SPC C-SPC"))
      (is equal '(0 0 t ()) (after "C-SPC C-SPC C-SPC"))
      (is equal '(8 4 nil (8)) (after "C-n C-SPC C-n C-SPC C-n C-u C-SPC"))
      (is equal '(4 12 nil (8 4)) (after "C-n C-SPC C-n C-SPC C-n C-SPC C-u C-SPC C-u C-SPC C-u C-SPC"))
      (is equal '(16 0 nil (16)) (after "M-> M-< C-u C-SPC"))
      (is equal '(0 16 nil (0)) (after "M-> M-< C-u C-SPC C-u C-SPC"))
      (is equal '(4 12 nil (8 4)) (after "C-n C-SPC C-n C-SPC C-n C-SPC C-u C-SPC C-SPC C-SPC" t))
      (is equal '(12 12 nil (8 4 12)) (after "C-n C-SPC C-n C-SPC C-n C-SPC C-u C-SPC C-SPC C-SPC"))
      (is equal '(0 16 t (0)) (after "C-x h"))
      (is equal '(0 16 t (4 4)) (after "C-n C-SPC C-x h"))
      (is equal '(0 0 t ()) (after "C-SPC C-u C-u C-SPC"))
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-u C-SPC" :text "abc") "No mark set in this buffer"))
      ;; a search that moved leaves the mark where it began, the mark before on the ring
      (let* ((editor (nth-value 1 (run-keys "C-SPC C-SPC C-n C-s g h i RET" :text (format nil "abc~%def~%ghi~%"))))
             (b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is = 4 (tuatara.buffer:buffer-mark b))
        (is equal '(0) (mapcar (lambda (a) (tuatara.defaults::anchor-in b a)) (tuatara.defaults::mark-ring b))))
      ;; the ring follows the text through edits
      ;; (M-< leaves no mark while the region is active, as in Emacs)
      (is equal '(11 7 nil (11)) (after "C-n C-n C-SPC C-n C-SPC M-< C-d C-u C-SPC"))
      ;; C-x C-SPC goes to the newest global mark (this buffer's), then back to the buffer before
      (let* ((editor (nth-value 1 (run-keys "M-> C-x b o t h e r C-j C-SPC C-x C-SPC C-x C-SPC" :text "abc")))
             (b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is string= "*scratch*" (tuatara.buffer:buffer-name b))
        (is = 0 (tuatara.buffer:buffer-point b))))))

(define-test (objects goal-column-with-J)
  ;; J and j keep to one column across a short line, as the mark run's j does
  (is = 10 (session-point (nth-value 1 (run-keys "l l l J J" :text "abcd
x
abcd"))))
  (is = 10 (session-point (nth-value 1 (run-keys "l l l j J" :text "abcd
x
abcd")))))

(define-test (objects pairing-as-typed)
  (let ((tuatara-user:*electric-pair-mode* t))
    (flet ((typed (keys text) (session-text (nth-value 1 (run-keys keys :text text)))))
      (is string= "(a)" (typed "i ( a C-g" ""))
      ;; the closing half typed where it stands steps over it
      (is string= "(a)b" (typed "i ( a ) b C-g" ""))
      (is string= "\"x\"" (typed "i \" x \" C-g" ""))
      ;; DEL between an empty pair takes both; elsewhere one character
      (is string= "" (typed "i ( DEL C-g" ""))
      (is string= "" (typed "i ( a DEL DEL C-g" ""))
      (is string= "a" (typed "i a b DEL C-g" ""))
      ;; the apostrophe and other everyday punctuation are left alone
      (is string= "don't" (typed "i d o n ' t C-g" ""))
      (let ((tuatara-user:*electric-pair-typing* :all))
        (is string= "''" (typed "i ' C-g" "")))))
  ;; off, as it is unless asked for
  (is string= "(" (session-text (nth-value 1 (run-keys "i ( C-g" :text "")))))

(define-test (objects pairs-from-a-slip)
  ;; pairs written as one dotted pair, or a list with a dotted tail, still pair
  (let ((tuatara-user:*electric-pair-mode* t))
    (flet ((typed (keys) (session-text (nth-value 1 (run-keys keys :text "")))))
      (let ((tuatara-user:*pair-delimiters* '(#\( . #\))))
        (is string= "(a)" (typed "i ( a C-g"))
        (is string= "[a" (typed "i [ a C-g")))
      (let ((tuatara-user:*pair-delimiters* '((#\( . #\)) (#\[ . #\]) . #\x)))
        (is string= "[a]" (typed "i [ a C-g")))
      (let ((tuatara-user:*electric-pair-safe-exclusions* '(#\< . #\()))
        (is string= "(a" (typed "i ( a C-g"))
        (is string= "[a]" (typed "i [ a C-g")))
      (let ((tuatara-user:*electric-pair-typing* '(#\[ . #\()))
        (is string= "(a)" (typed "i ( a C-g"))
        (is string= "{a" (typed "i { a C-g")))
      (let ((tuatara-user:*electric-pair-typing* #\())
        (is string= "(a)" (typed "i ( a C-g"))))))

(define-test (objects pairs-far-and-limited)
  ;; the pair found however far, fast; with none, said at once, even in a large text
  (let ((far (concatenate 'string "(" (make-string 300000 :initial-element #\x) ")")))
    (is = 300000 (length (selection-after "l l m i (" far))))
  (let ((text (make-string 2000000 :initial-element #\y)))
    (multiple-value-bind (seconds result)
        (let ((start (get-internal-real-time)))
          (let ((f (run-keys "g e m i (" :text text)))
            (values (/ (- (get-internal-real-time) start) internal-time-units-per-second) f)))
      (true (search "No '(' found near cursor" (tuatara.frontend.fake:fake-screen-line result 7)))
      (true (< seconds 2))))
  ;; past the limit, a partner counts as none
  (let ((tuatara.defaults::*text-scan-limit* 3))
    (multiple-value-bind (f editor) (run-keys "l m i (" :text "(abcdef)")
      (declare (ignore editor))
      (true (search "No matching ')' found after cursor" (tuatara.frontend.fake:fake-screen-line f 7))))))

(define-test (objects fill-column-asked)
  ;; M-q, g Q and g q ask for the column, the buffer's offered: RET takes it
  (let ((line (format nil "~{~a~^ ~}~%" (loop repeat 20 collect "word"))))
    (multiple-value-bind (f editor) (run-keys "M-q" :text line)
      (true (tuatara.frontend.fake:fake-frame-showing f "Fill column (default 80): "))
      (is string= line (session-text editor)))
    (is string= (format nil "~{~a~^ ~}~%~{~a~^ ~}~%" (loop repeat 16 collect "word") (loop repeat 4 collect "word"))
        (session-text (nth-value 1 (run-keys "M-q RET" :text line))))
    ;; a number typed fills to it and becomes the buffer's, offered next time
    (multiple-value-bind (f editor)
        (run-keys "g Q 4 0 RET g e k M-q" :text (format nil "~a~%~a" line line))
      (true (tuatara.frontend.fake:fake-frame-showing f "Fill column 40 for this buffer"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Fill column (default 40): "))
      (is = 40 (tuatara.defaults::buffer-fill-column (session-buffer editor))))
    (is string= (format nil "~{~a~^ ~}~%~{~a~^ ~}~%~{~a~^ ~}~%" (loop repeat 8 collect "word")
                        (loop repeat 8 collect "word") (loop repeat 4 collect "word"))
        (session-text (nth-value 1 (run-keys "M-q 4 0 RET" :text line))))
    ;; what is no column is asked again; C-g leaves the text as it was
    (multiple-value-bind (f editor) (run-keys "M-q x y RET" :text line :width 80)
      (true (tuatara.frontend.fake:fake-frame-showing f "Not a column. Fill column (default 80): "))
      (is string= line (session-text editor)))
    (is string= line (session-text (nth-value 1 (run-keys "M-q C-g" :text line))))
    ;; a selection: asked once, and said to what column
    (multiple-value-bind (f editor) (run-keys "% g q 4 0 RET" :text (format nil "~a~%~a" line line))
      (true (tuatara.frontend.fake:fake-frame-showing f "Filled 2 paragraphs to column 40"))
      (is = 40 (tuatara.defaults::buffer-fill-column (session-buffer editor))))
    ;; nothing to fill: not asked, and in code said why
    (multiple-value-bind (f editor) (run-keys "M-x l i s p - m o d e RET M-q" :text "(defun f () nil)")
      (declare (ignore editor))
      (false (tuatara.frontend.fake:fake-frame-showing f "Fill column (default"))
      (true (tuatara.frontend.fake:fake-frame-showing f "Nothing to fill here: Lisp mode fills only comments and strings")))
    ;; *fill-ask-column* NIL fills at once
    (let ((tuatara-user:*fill-ask-column* nil))
      (is string= (format nil "~{~a~^ ~}~%~{~a~^ ~}~%" (loop repeat 16 collect "word") (loop repeat 4 collect "word"))
          (session-text (nth-value 1 (run-keys "M-q" :text line)))))
    ;; C-x f asks as well, the buffer's column offered: a number typed becomes
    ;; the buffer's, RET alone keeps it, 0 is no column, a count answers at once
    (multiple-value-bind (f editor) (run-keys "C-x f 7 2 RET" :text line :width 80)
      (true (tuatara.frontend.fake:fake-frame-showing f "Set fill column (now 80): "))
      (true (tuatara.frontend.fake:fake-frame-showing f "Fill column set to 72 (was 80)"))
      (is = 72 (tuatara.defaults::buffer-fill-column (session-buffer editor)))
      (is string= line (session-text editor)))
    (multiple-value-bind (f editor) (run-keys "C-x f RET" :text line :width 80)
      (true (tuatara.frontend.fake:fake-frame-showing f "Fill column stays 80"))
      (is = 80 (tuatara.defaults::buffer-fill-column (session-buffer editor))))
    (multiple-value-bind (f editor) (run-keys "C-x f 0 RET 6 0 RET" :text line :width 80)
      (true (tuatara.frontend.fake:fake-frame-showing f "Not a column. Set fill column (now 80): "))
      (is = 60 (tuatara.defaults::buffer-fill-column (session-buffer editor))))
    (multiple-value-bind (f editor) (run-keys "C-u 6 6 C-x f" :text line :width 80)
      (false (tuatara.frontend.fake:fake-frame-showing f "Set fill column"))
      (is = 66 (tuatara.defaults::buffer-fill-column (session-buffer editor))))
    (is = 80 (tuatara.defaults::buffer-fill-column (session-buffer (nth-value 1 (run-keys "C-x f C-g" :text line)))))))

(define-test (objects paragraphs-and-sentences-far)
  ;; two megabytes of lines without a blank one: the paragraph is all of it, found at once
  (let* ((line (format nil "2026-09-28,12.5,note~%"))
         (text (with-output-to-string (o) (dotimes (i 100000) (write-string line o)))))
    (multiple-value-bind (seconds f editor)
        (let ((start (get-internal-real-time)))
          (multiple-value-bind (f editor) (run-keys "j j m p" :text text)
            (values (/ (- (get-internal-real-time) start) internal-time-units-per-second) f editor)))
      (declare (ignore f))
      (true (< seconds 2))
      (is equal (list 0 (length text))
          (let ((b (session-buffer editor)))
            (list (min (tuatara.buffer:buffer-mark b) (tuatara.buffer:buffer-point b))
                  (max (tuatara.buffer:buffer-mark b) (tuatara.buffer:buffer-point b))))))
    ;; no sentence ends in it either (12.5 is no full stop): the sentence is the paragraph
    (multiple-value-bind (seconds f)
        (let ((start (get-internal-real-time)))
          (let ((f (run-keys "j j m s" :text text)))
            (values (/ (- (get-internal-real-time) start) internal-time-units-per-second) f)))
      (true (< seconds 2))
      (true (search "Sentence marked" (tuatara.frontend.fake:fake-screen-line f 7))))
    ;; farther than the limit: said, and nothing selected
    (let ((tuatara.defaults::*text-scan-limit* 1000))
      (multiple-value-bind (f editor) (run-keys "j j m p" :text text)
        (true (search "No paragraph break within 1000 bytes" (tuatara.frontend.fake:fake-screen-line f 7)))
        (false (tuatara.buffer:buffer-mark-active-p (session-buffer editor)))))
    ;; a paragraph larger than filling takes by itself: said, and left as it was
    (let ((tuatara.defaults::*fill-paragraph-limit* 1000))
      (multiple-value-bind (f editor) (run-keys "j j M-q" :text text)
        (true (search "The paragraph is larger than 0 KB, *fill-paragraph-limit*" (tuatara.frontend.fake:fake-screen-line f 7)))
        (is string= text (session-text editor)))
      ;; and in a selection, filled with g q: the same limit, said the same way
      (multiple-value-bind (f editor) (run-keys "% g q RET" :text text)
        (true (search "The paragraph is larger than 0 KB" (tuatara.frontend.fake:fake-screen-line f 7)))
        (is string= text (session-text editor))))))

(defun width-for (path &optional (more 120))
  "A frame width with room for PATH, as a name, and MORE columns beside it:
the test directories' paths are as long as where the cache is."
  (+ more (length (if (stringp path) path (namestring path)))))

(defun run-keys-in-turns (turns &key (text "") (width 120) (height 8) crlf mode folds point mark)
  "Run an editor session on a scratch buffer holding TEXT with the key
sequences TURNS typed one after another, each once the work the one before
started in the background (a replacement, dynamic blocks made) is done; return the frontend and the editor. CRLF
has the buffer end its lines with a carriage return and a newline; MODE names
the buffer's major mode; the octets of FOLDS, each (START . END), are folded
first; point starts at POINT, and with MARK the mark is there and active."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (let ((b (tuatara.buffer:make-buffer "*scratch*" :text text)))
      (when crlf (setf (tuatara.buffer:buffer-local b :line-ending) :crlf))
      (when mode (tuatara.defaults::set-major-mode b mode))
      (dolist (fold folds) (tuatara.display:fold-region b (car fold) (cdr fold)))
      (when point (setf (tuatara.buffer:buffer-point b) point))
      (when mark (setf (tuatara.buffer:buffer-mark b) mark (tuatara.buffer:buffer-mark-active-p b) t)))
    (loop for (keys . more) on turns
          do (tuatara.frontend.fake:fake-queue-keys f keys)
             (when more
               (tuatara.frontend.fake:fake-queue-action
                f (lambda ()
                    (loop while (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*)
                          do (sleep 0.01))))))
    (values f (without-grammar-questions
                (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                              :buffer (tuatara.buffer:find-buffer "*scratch*"))))))

(defparameter *emacs-rectangle-cases*
  '(("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-x r k" "abdef~%ghjkl~%mn<POINT>pqr~%" ("c" "i" "o"))
    ("ab<POINT>cdef~%ghijkl~%mno<MARK>pqr~%" "C-x r k" "ab<POINT>def~%ghjkl~%mnpqr~%" ("c" "i" "o"))
    ("abc<MARK>def~%ghijkl~%mn<POINT>opqr~%" "C-x r k" "abdef~%ghjkl~%mn<POINT>pqr~%" ("c" "i" "o"))
    ("ab<MARK>cdef~%g~%mno<POINT>pqr~%" "C-x r k" "abdef~%g~%mn<POINT>pqr~%" ("c" " " "o"))
    ("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-x r M-w" "abcdef~%ghijkl~%mno<POINT>pqr~%" ("c" "i" "o"))
    ("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-x r d" "abdef~%ghjkl~%mn<POINT>pqr~%" ("Q"))
    ("abcdef~%gh<POINT>ijkl~%m~%" "C-x r y" "abcdef~%ghXYijkl~%m Z<POINT>~%" ("XY" "Z"))
    ("abcdef~%gh<POINT>ijkl~%" "C-x r y" "abcdef~%ghXYijkl~%  Z<POINT>" ("XY" "Z"))
    ("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-x r t X Y RET" "abXYdef~%ghXYjkl~%mnXY<POINT>pqr~%" ("Q"))
    ("ab<POINT>cdef~%ghijkl~%mno<MARK>pqr~%" "C-x r t X Y RET" "abXYdef~%ghXYjkl~%mnXY<POINT>pqr~%" ("Q"))
    ("ab<POINT>cdef~%ghijkl~%mnopqr~%" "C-x SPC C-n C-f C-w" "abdef~%gh<POINT>jkl~%mnopqr~%" ("c" "i"))
    ("ab<POINT>cdef~%ghijkl~%mnopqr~%" "C-x SPC C-n C-f C-w C-a C-y" "abdef~%cghjkl~%i<POINT>mnopqr~%" ("c" "i"))
    ("ab<POINT>cdef~%ghijkl~%mnopqr~%" "C-x SPC C-n C-f M-w M-< C-y" "cabcdef~%i<POINT>ghijkl~%mnopqr~%" ("c" "i"))
    ("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-x r c" "ab def~%gh jkl~%mn<POINT> pqr~%" ("Q"))
    ("ab<POINT>cdef~%ghijkl~%mno<MARK>pqr~%" "C-x r c" "ab<POINT> def~%gh jkl~%mn pqr~%" ("Q"))
    ("ab<MARK>cdef~%g~%ghi~%mno<POINT>pqr~%" "C-x r c" "ab def~%g~%gh~%mn<POINT> pqr~%" ("Q"))
    ("ab<MARK>cdef~%ghi~%mnopqr<POINT>~%" "C-x r c" "ab~%gh~%mn<POINT>~%" ("Q"))
    ("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-x r N" "ab1 cdef~%gh2 ijkl~%mn3 o<POINT>pqr~%" ("Q"))
    ("ab<POINT>cdef~%ghijkl~%mno<MARK>pqr~%" "C-x r N" "ab<POINT>1 cdef~%gh2 ijkl~%mn3 opqr~%" ("Q"))
    ("ab<MARK>cdef~%g~%~%mno<POINT>pqr~%" "C-x r N" "ab1 cdef~%g 2 ~%  3 ~%mn4 o<POINT>pqr~%" ("Q"))
    ("<MARK>a~%b~%c~%d~%e~%f~%g~%h~%i~%j~%<POINT>k~%" "C-x r N"
     " 1 a~% 2 b~% 3 c~% 4 d~% 5 e~% 6 f~% 7 g~% 8 h~% 9 i~%10 j~%<POINT>11 k~%" ("Q"))
    ("<MARK>a~%b~%c~%d~%e~%f~%g~%h~%<POINT>i~%" "C-x r N"
     "1 a~%2 b~%3 c~%4 d~%5 e~%6 f~%7 g~%8 h~%<POINT>9 i~%" ("Q"))
    ("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-u C-x r N 9 RET C-a C-k % d . SPC RET"
     "ab9. cdef~%gh10. ijkl~%mn11. o<POINT>pqr~%" ("Q"))
    ("ab<MARK>cdef~%ghijkl~%mno<POINT>pqr~%" "C-u C-x r N RET C-a C-k % 0 3 d | % % SPC RET"
     "ab001|% cdef~%gh002|% ijkl~%mn003|% o<POINT>pqr~%" ("Q"))
    ("<MARK>- one~%- two~%<POINT>- three~%" "C-f M-x n u m b e r - r e c t a n g l e - l i n e s - r e p l a c e RET"
     "1 one~%2 two~%3<POINT> three~%" ("Q"))
    ("<MARK>7. one~%8. two~%<POINT>9. three~%" "C-f M-x n u m b e r - r e c t a n g l e - l i n e s - r e p l a c e RET"
     "1. one~%2. two~%3<POINT>. three~%" ("Q"))
    ("<MARK>a~%b~%c~%d~%e~%f~%g~%h~%i~%<POINT>j~%" "M-x n u m b e r - r e c t a n g l e - l i n e s - r e p l a c e RET"
     " 1a~% 2b~% 3c~% 4d~% 5e~% 6f~% 7g~% 8h~% 9i~%10<POINT>j~%" ("Q")))
  "Emacs's rectangle keys, each as (TEXT KEYS EXPECTED STORE), ~% a newline:
the text after, and the rectangle store, which starts as (\"XY\" \"Z\") for
C-x r y and (\"Q\") otherwise; as Emacs 30 does.")

(define-test (objects emacs-rectangles)
  (let ((tuatara-user:*modal-editing* nil))
    (loop for (text keys expected store) in *emacs-rectangle-cases*
          do (setf tuatara.defaults::*killed-rectangle* (if (search "r y" keys) (list "XY" "Z") (list "Q")))
             (multiple-value-bind (plain point mark) (point-and-mark-text (format nil text))
               (multiple-value-bind (f editor) (session-in nil plain keys :point point :mark mark :width 100)
                 (declare (ignore f))
                 (is string= (format nil expected) (text-with-marks (session-buffer editor)) "~s then ~a" text keys)
                 (is equal store tuatara.defaults::*killed-rectangle* "the store after ~a" keys)))))
  ;; with the mark never set, the commands say so
  (let ((tuatara-user:*modal-editing* nil))
    (multiple-value-bind (f editor) (run-keys "C-x r k" :text "abc")
      (is string= "abc" (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "The mark is not set now")))
    ;; a format it does not understand changes nothing and says so
    (multiple-value-bind (f editor) (session-in nil (format nil "ab~%cd~%") "C-u C-x r N RET C-a C-k % x RET" :point 4 :mark 0)
      (is string= (format nil "ab~%cd~%") (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "%x is not understood")))))

(defparameter *emacs-screen-rectangle-cases*
  '(("ab<MARK>cdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r k" "ab~%x ~%ab<POINT>gh~%" ("cdef" "  yz" "cdef"))
    ("ab<MARK>cdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r M-w" "abcdef~%x<TAB>yz~%abcdef<POINT>gh~%" ("cdef" "  yz" "cdef"))
    ("ab<MARK>cdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r c" "ab~%x~%ab<POINT>    gh~%" ("Q"))
    ("ab<MARK>cdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r t X Y RET" "abXY~%x XY~%abXY<POINT>gh~%" ("Q"))
    ("ab<MARK>cdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r N" "ab1 cdef~%x 2   yz~%ab3 cdef<POINT>gh~%" ("Q"))
    ("a<MARK>bcdef~%xy<TAB>z~%abc<POINT>defgh~%" "C-x r k" "adef~%x z~%a<POINT>defgh~%" ("bc" "y " "bc"))
    ("a<MARK>bcdef~%xy<TAB>z~%abc<POINT>defgh~%" "C-x r M-w" "abcdef~%xy<TAB>z~%abc<POINT>defgh~%" ("bc" "y " "bc"))
    ("a<MARK>bcdef~%xy<TAB>z~%abc<POINT>defgh~%" "C-x r c" "a  def~%x   z~%a<POINT>  defgh~%" ("Q"))
    ("a<MARK>bcdef~%xy<TAB>z~%abc<POINT>defgh~%" "C-x r t X Y RET" "aXYdef~%xXY z~%aXY<POINT>defgh~%" ("Q"))
    ("a<MARK>bcdef~%xy<TAB>z~%abc<POINT>defgh~%" "C-x r N" "a1 bcdef~%x2 y<TAB>z~%a3 bc<POINT>defgh~%" ("Q"))
    ("ab<MARK>cdef~%x<TAB>z~%abc<POINT>defgh~%" "C-x r k" "abdef~%x  z~%ab<POINT>defgh~%" ("c" " " "c"))
    ("ab<MARK>cdef~%x<TAB>z~%abc<POINT>defgh~%" "C-x r M-w" "abcdef~%x<TAB>z~%abc<POINT>defgh~%" ("c" " " "c"))
    ("ab<MARK>cdef~%x<TAB>z~%abc<POINT>defgh~%" "C-x r c" "ab def~%x   z~%ab<POINT> defgh~%" ("Q"))
    ("ab<MARK>cdef~%x<TAB>z~%abc<POINT>defgh~%" "C-x r t X Y RET" "abXYdef~%x XY z~%abXY<POINT>defgh~%" ("Q"))
    ("ab<MARK>cdef~%x<TAB>z~%abc<POINT>defgh~%" "C-x r N" "ab1 cdef~%x 2   z~%ab3 c<POINT>defgh~%" ("Q"))
    ("ab<MARK>cdef~%x漢yz~%abcd<POINT>ef~%" "C-x r k" "abef~%x漢z~%ab<POINT>ef~%" ("cd" " y" "cd"))
    ("ab<MARK>cdef~%x漢yz~%abcd<POINT>ef~%" "C-x r M-w" "abcdef~%x漢yz~%abcd<POINT>ef~%" ("cd" " y" "cd"))
    ("ab<MARK>cdef~%x漢yz~%abcd<POINT>ef~%" "C-x r c" "ab  ef~%x漢 z~%ab<POINT>  ef~%" ("Q"))
    ("ab<MARK>cdef~%x漢yz~%abcd<POINT>ef~%" "C-x r t X Y RET" "abXYef~%x漢XYz~%abXY<POINT>ef~%" ("Q"))
    ("ab<MARK>cdef~%x漢yz~%abcd<POINT>ef~%" "C-x r N" "ab1 cdef~%x漢2 yz~%ab3 cd<POINT>ef~%" ("Q"))
    ("a<MARK>bcdef~%xy漢z~%abc<POINT>def~%" "C-x r k" "adef~%x漢z~%a<POINT>def~%" ("bc" "y " "bc"))
    ("a<MARK>bcdef~%xy漢z~%abc<POINT>def~%" "C-x r M-w" "abcdef~%xy漢z~%abc<POINT>def~%" ("bc" "y " "bc"))
    ("a<MARK>bcdef~%xy漢z~%abc<POINT>def~%" "C-x r c" "a  def~%x 漢z~%a<POINT>  def~%" ("Q"))
    ("a<MARK>bcdef~%xy漢z~%abc<POINT>def~%" "C-x r t X Y RET" "aXYdef~%xXY漢z~%aXY<POINT>def~%" ("Q"))
    ("a<MARK>bcdef~%xy漢z~%abc<POINT>def~%" "C-x r N" "a1 bcdef~%x2 y漢z~%a3 bc<POINT>def~%" ("Q"))
    ("<MARK>abcdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r k" "~%~%<POINT>gh~%" ("abcdef" "x   yz" "abcdef"))
    ("<MARK>abcdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r M-w" "abcdef~%x<TAB>yz~%abcdef<POINT>gh~%" ("abcdef" "x   yz" "abcdef"))
    ("<MARK>abcdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r c" "~%~%<POINT>      gh~%" ("Q"))
    ("<MARK>abcdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r t X Y RET" "XY~%XY~%XY<POINT>gh~%" ("Q"))
    ("<MARK>abcdef~%x<TAB>yz~%abcdef<POINT>gh~%" "C-x r N" "1 abcdef~%2 x<TAB>yz~%3 abcdef<POINT>gh~%" ("Q"))
    ("ab<TAB><MARK>cd~%abcdefgh<POINT>ij~%" "C-x r k" "ab<TAB>~%abcd<POINT>ij~%" ("cd  " "efgh"))
    ("ab<TAB><MARK>cd~%abcdefgh<POINT>ij~%" "C-x r M-w" "ab<TAB>cd~%abcdefgh<POINT>ij~%" ("cd  " "efgh"))
    ("ab<TAB><MARK>cd~%abcdefgh<POINT>ij~%" "C-x r c" "ab<TAB>~%abcd<POINT>    ij~%" ("Q"))
    ("ab<TAB><MARK>cd~%abcdefgh<POINT>ij~%" "C-x r t X Y RET" "ab<TAB>XY~%abcdXY<POINT>ij~%" ("Q"))
    ("ab<TAB><MARK>cd~%abcdefgh<POINT>ij~%" "C-x r N" "ab<TAB>1 cd~%abcd2 efgh<POINT>ij~%" ("Q"))
    ("ab<POINT>cdef~%x<TAB>yz~%" "C-x r y" "abXYcdef~%x Z<POINT>  yz~%" ("XY" "Z"))
    ("abc<POINT>def~%x漢yz~%" "C-x r y" "abcXYdef~%x漢Z<POINT>yz~%" ("XY" "Z"))
    ("<POINT>abcdef~%x<TAB>yz~%" "C-x r y" "XYabcdef~%Z<POINT>x<TAB>yz~%" ("XY" "Z")))
  "Emacs's rectangle keys over tabs (<TAB>, four columns apart) and wide
characters, each as *EMACS-RECTANGLE-CASES* has it: as Emacs 30 does, but
where Emacs disagrees with itself. Here a wide character across an edge stays
out of the block (Emacs takes it in at the right edge, so a row is wider than
the block); a row kept is the block's width, its tabs made spaces, whether
cut or copied (Emacs's C-x r k and C-x r M-w keep different rows); C-x r c
keeps what follows the block where it was, a tab across its edge as spaces,
and cuts a line that ends inside it at its left column (Emacs moves the text
after a tab, and leaves a row after a wide character as it was).")

(define-test (objects rectangles-in-screen-columns)
  (let ((tuatara-user:*modal-editing* nil)
        (tuatara-user:*tab-width* 4))
    (flet ((tabs (text) (cl-ppcre:regex-replace-all "<TAB>" (format nil text) (string #\Tab))))
      (loop for (text keys expected store) in *emacs-screen-rectangle-cases*
            do (setf tuatara.defaults::*killed-rectangle* (if (search "r y" keys) (list "XY" "Z") (list "Q")))
               (multiple-value-bind (plain point mark) (point-and-mark-text (tabs text))
                 (multiple-value-bind (f editor) (session-in nil plain keys :point point :mark mark :width 100)
                   (declare (ignore f))
                   (is string= (tabs expected) (text-with-marks (session-buffer editor)) "~s then ~a" text keys)
                   (is equal (mapcar #'tabs store) tuatara.defaults::*killed-rectangle* "the store after ~a on ~s" keys text))))
      ;; in the background, the same text and the same rows kept
      (loop for (text keys expected store) in *emacs-screen-rectangle-cases*
            unless (search "r y" keys)
              do (setf tuatara.defaults::*killed-rectangle* (list "Q"))
                 (multiple-value-bind (plain point mark) (point-and-mark-text (tabs text))
                   (with-background ()
                     (let ((editor (nth-value 1 (run-keys-in-turns (list keys "") :text plain :point point :mark mark))))
                       (is string= (point-and-mark-text (tabs expected)) (session-text editor) "~s then ~a in the background" text keys)
                       (let ((kept tuatara.defaults::*killed-rectangle*))
                         (is equal (mapcar #'tabs store)
                             (if (tuatara.defaults::rectangle-source-p kept) (tuatara.defaults::source-texts kept) kept)
                             "the store after ~a on ~s in the background" keys text))))))))
  ;; NORMAL state: the block sized by moving down through a tab keeps its columns on the screen
  (let ((tuatara-user:*tab-width* 4))
    (flet ((tabs (text) (cl-ppcre:regex-replace-all "<TAB>" (format nil text) (string #\Tab))))
      (multiple-value-bind (f editor) (run-keys "l l m v j j l d" :text (tabs "abcdefgh~%x<TAB>yz~%abcdefgh"))
        (declare (ignore f))
        (is string= (tabs "abefgh~%x yz~%abefgh") (session-text editor))
        (is equal '("cd" "  " "cd") tuatara.defaults::*killed-rectangle*))
      ;; down and up keep the column on the screen: past a tab or a wide character it spans, then back
      (dolist (case '(("l l j" "abcd~%x<TAB>yz~%abcd" 7) ("l l j j" "abcd~%x<TAB>yz~%abcd" 12)
                      ("l j" "abcd~%漢字x~%abcd" 8) ("l j j" "abcd~%漢字x~%abcd" 14)))
        (destructuring-bind (keys text point) case
          (is = point (session-point (nth-value 1 (run-keys keys :text (tabs text)))) "~a on ~s" keys text))))))

(define-test (objects rectangles-in-read-only-buffers)
  ;; C-x r d and C-x r k in a buffer that may not be written: nothing cut, the store as it was
  (let ((tuatara-user:*modal-editing* nil))
    (dolist (keys '("M-x r e a d - o n l y - m o d e RET C-x r d" "M-x r e a d - o n l y - m o d e RET C-x r k"))
      (setf tuatara.defaults::*killed-rectangle* (list "Q"))
      (multiple-value-bind (f editor) (session-in nil (format nil "abc~%def~%") keys :point 5 :mark 0)
        (is string= (format nil "abc~%def~%") (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "read-only"))
        (is equal '("Q") tuatara.defaults::*killed-rectangle* "~a" keys)))))

(define-test (objects rectangle-job-shading)
  ;; a rectangle's job in the background shades its columns only, row by row
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.1))))
    (with-background ()
      (let ((spans (painted-job-spans (run-keys "l m v j j d" :text (format nil "abcd~%efgh~%ijkl~%mnop") :width 60 :height 10))))
        (true spans)
        (true (every (lambda (span) (and (<= (first span) 2) (= 1 (second span)) (= 2 (third span)))) spans)))))
  ;; a sort's: its region, until an edit touches it
  (let ((job (tuatara.defaults::%make-sort-job :region-start 2 :region-end 8)))
    (is equal '((2 5 :job-queued)) (tuatara.defaults::sort-shading job 0 5))
    (false (tuatara.defaults::sort-shading job 8 20))
    (setf (tuatara.defaults::sort-job-changed job) t)
    (false (tuatara.defaults::sort-shading job 0 5))))

(define-test (objects large-rectangles)
  ;; a rectangle over more text than *rewrite-threshold* is kept by where it is
  ;; and changed in the background, with what the small one does as its result
  (flet ((both (keys text)
           (let* ((turns (uiop:split-string keys :separator "|"))
                  (small (session-text (nth-value 1 (run-keys-in-turns turns :text text))))
                  (large (with-background ()
                           (session-text (nth-value 1 (run-keys-in-turns turns :text text))))))
             (is string= small large "~a on ~s" keys text)
             large))
         (store () tuatara.defaults::*killed-rectangle*))
    (is string= (format nil "babc~%edef~%ghi") (both "l m v j y g g P" (format nil "abc~%def~%ghi")))
    (with-background ()
      (run-keys "l m v j y" :text (format nil "abc~%def~%ghi"))
      (true (tuatara.defaults::rectangle-source-p (store)))
      (is equal '("b" "e") (tuatara.defaults::source-texts (store))))
    ;; cut, with one undo putting it back
    (is string= (format nil "ac~%df~%ghi") (both "l m v j d" (format nil "abc~%def~%ghi")))
    (is equal '("b" "e") (tuatara.defaults::source-texts (store)))
    (is string= (format nil "abc~%def") (both "l m v j d|u" (format nil "abc~%def")))
    ;; pasted with a count, and over a rectangle of as many rows
    (is string= (format nil "bbabc~%eedef") (both "l m v j y g g C-u 2 P" (format nil "abc~%def")))
    (is string= (format nil "bbc~%eef~%ghi") (both "l m v j y g g m v j P" (format nil "abc~%def~%ghi")))
    (is string= (format nil "abc~%def~%ghi") (both "l m v j y g g m v j P|u" (format nil "abc~%def~%ghi")))
    ;; of a different number of rows it is refused
    (with-background ()
      (multiple-value-bind (f editor) (run-keys "l m v j y g g m v P" :text (format nil "abc~%def"))
        (is string= (format nil "abc~%def") (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Rectangle row mismatch"))))
    ;; more rows than are changed at once, however little text: in the background too
    (let ((tuatara.defaults::*rectangle-rows-in-memory* 2))
      (true (tuatara.defaults::large-rectangle-p
             (progn (run-keys "l m v j j" :text (format nil "abc~%def~%ghi~%")) (tuatara.buffer:find-buffer "*scratch*"))))
      (is string= (format nil "ac~%df~%gi~%")
          (session-text (nth-value 1 (run-keys-in-turns '("l m v j j d" "") :text (format nil "abc~%def~%ghi~%"))))))
    ;; lines ending with a carriage return and a newline: the last row's ending kept once
    (with-background ()
      (is string= (format nil "ac~c~%df~c~%" #\Return #\Return)
          (session-text (nth-value 1 (run-keys-in-turns '("l m v j d" "") :text (format nil "abc~c~%def~c~%" #\Return #\Return)
                                                                          :crlf t)))))
    ;; short lines padded, rows past the last line on lines of their own
    (is string= (format nil "abd~%x~%abd") (both "l l m v j j d" (format nil "abcd~%x~%abcd")))
    (is equal '("c" " " "c") (tuatara.defaults::source-texts (store)))
    (is string= (format nil "abcd~%x~%abcdc~%     ~%    c") (both "l l m v j j y g e P" (format nil "abcd~%x~%abcd")))
    (is string= (format nil "~%") (both "m v l l l j d" (format nil "ab~%abcd")))
    ;; c: past the limit, typed at the rows in sight and the others in the background; a wrap key wraps each row
    (let ((tuatara.defaults::*rectangle-cursors-limit* 1))
      (is string= (format nil "Zbc~%Zbc") (both "m v j c Z C-g|" (format nil "abc~%abc")))
      (is string= (format nil "Zc~%Zc~%Zc") (both "l m v j j c DEL Z C-g|" (format nil "abc~%abc~%abc")))
      (is string= (format nil "aZ~% Z~%aZ") (both "l m v l j j c Z C-g|" (format nil "abc~%~%abc"))))
    (with-background ()
      (is string= (format nil "Zbc~%Zbc") (session-text (nth-value 1 (run-keys "m v j c Z C-g" :text (format nil "abc~%abc"))))))
    ;; the emptying and the typing are one change: one u puts the block back, past the limit too
    (is string= (format nil "abc~%abc") (session-text (nth-value 1 (run-keys-in-turns '("m v j c Z C-g C-g" "u") :text (format nil "abc~%abc")))))
    (let ((tuatara.defaults::*rectangle-cursors-limit* 1))
      (is string= (format nil "abc~%abc~%abc")
          (session-text (nth-value 1 (run-keys-in-turns '("m v j j c Z C-g" "u") :text (format nil "abc~%abc~%abc"))))))
    ;; cleared, and numbered in place of the block, one undo putting it back
    (is string= (format nil "a  d~%x~%a  d~%") (both "l m v j j l C-x r c" (format nil "abcd~%x~%abcd~%")))
    (is string= (format nil "a~%x~%a~%") (both "l m v j j l l l C-x r c" (format nil "abcd~%x~%abcd~%")))
    (is string= (format nil "a1 bcd~%x2 ~%a3 bcd~%") (both "l m v j j C-x r N" (format nil "abcd~%x~%abcd~%")))
    (is string= (format nil "a1cd~%x2~%a3cd~%")
        (both "l m v j j M-x n u m b e r - r e c t a n g l e - l i n e s - r e p l a c e RET" (format nil "abcd~%x~%abcd~%")))
    (is string= (format nil "abcd~%x~%abcd~%") (both "l m v j j C-x r N|u" (format nil "abcd~%x~%abcd~%")))
    (is string= (format nil "(a)b~%(a)b") (both "m v j (" (format nil "ab~%ab")))
    (is string= (format nil "a[bc]~%a[  ]") (both "l m v l j [" (format nil "abc~%a")))
    ;; a block with no width says so at once
    (with-background ()
      (multiple-value-bind (f editor) (run-keys "m v j h y" :text (format nil "abc~%def"))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Nothing to copy: the rectangle has no width"))))
    ;; the work shows in the mode line while it runs, and the editor goes on
    (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
      (with-background ()
        (multiple-value-bind (f editor) (run-keys "l m v j d g e i X C-g" :text (format nil "abc~%def~%ghi"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Cutting"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Cut the rectangle's 2 rows"))
          (is string= (format nil "ac~%df~%ghiX") (session-text editor)))))))

(defun run-keys-folded (keys text folds)
  "RUN-KEYS on TEXT in a plain buffer whose octets FOLDS, each (START . END), are folded first."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (let ((b (tuatara.buffer:make-buffer "*scratch*" :text text)))
      (dolist (fold folds) (tuatara.display:fold-region b (car fold) (cdr fold))))
    (queue-test-keys f keys)
    (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                :buffer (tuatara.buffer:find-buffer "*scratch*"))))
      (values f editor))))

(define-test (objects lines-shown-past-folds)
  ;; any buffer's folds, not Org's alone: "a" hides the line "b" after it
  (let ((text (format nil "a~%b~%c~%d~%")) (folds '((1 . 3))))
    (flet ((after (keys) (session-text (nth-value 1 (run-keys-folded keys text folds)))))
      (is string= (format nil "a...") (string-right-trim " " (tuatara.frontend.fake:fake-screen-line (run-keys-folded "" text folds) 0)))
      ;; a line and what its fold hides are one line to V, J, K and a count
      (is string= (format nil "c~%d~%") (after "V d"))
      (is string= (format nil "d~%") (after "V J d"))
      (is string= (format nil "d~%") (after "j V K d"))
      (is string= (format nil "d~%") (after "C-u 2 V d"))
      ;; t puts its cursor on the line shown below
      (is string= (format nil "Xa~%b~%Xc~%d~%") (after "t i X C-g"))
      ;; deleting into the hidden text is refused, the fold shown; the same key then deletes
      (multiple-value-bind (f editor) (run-keys-folded "g l d" text folds)
        (is string= text (session-text editor))
        (is string= "b" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 1)))
        (true (tuatara.frontend.fake:fake-frame-showing f "That would delete hidden text")))
      (is string= (format nil "ab~%c~%d~%") (after "g l d d")))))

(define-test (objects folds-kept-at-many-places)
  ;; "a" hides the line "b" after it: what acts at many places at once does not
  ;; delete hidden text unseen either
  (let ((text (format nil "a~%b~%c~%d~%")) (folds '((1 . 3))))
    (flet ((after (keys) (session-text (nth-value 1 (run-keys-folded keys text folds))))
           (refused (keys)
             (multiple-value-bind (f editor) (run-keys-folded keys text folds)
               (is string= text (session-text editor) "~a deletes nothing" keys)
               (is string= "b" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 1)) "~a shows the fold" keys)
               (true (tuatara.frontend.fake:fake-frame-showing f "That would delete hidden text") "~a says so" keys))))
      ;; a banked line is the line shown, with all its fold hides
      (is string= (format nil "c~%d~%") (after "m l d"))
      (is string= (format nil "d~%") (after "m l j m l d"))
      (is string= (format nil "d~%") (after "C-u 2 m l d"))
      (is string= (format nil "c~%c~%d~%") (after "j y k m l p"))
      ;; every cursor's selection, a rectangle
      (refused "t V d")
      (refused "m v j l d")
      ;; a match the fold hides is no place at all, over the whole buffer too
      (dolist (keys '("% f b RET" "V f b RET"))
        (multiple-value-bind (f editor) (run-keys-folded keys text folds)
          (is string= text (session-text editor) "~a leaves it alone" keys)
          (is string= "a..." (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0)) "~a keeps the fold" keys)
          (true (tuatara.frontend.fake:fake-frame-showing f "Nothing matched b; 1 hidden in folds left alone") "~a says so" keys)))
      ;; shown, the same key deletes what can now be seen
      (is string= (format nil "b~%d~%") (after "t V d d"))
      (is string= (format nil "~%~%~%d~%") (after "m v j l d d"))
      ;; what does not reach into a fold goes ahead, the fold kept
      (multiple-value-bind (f editor) (run-keys-folded "% f [ c d ] RET d" text folds)
        (is string= (format nil "a~%b~%~%~%") (session-text editor))
        (is string= "a..." (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 0))))))
  ;; out of sight, in the background: the hidden match is left alone, over the
  ;; whole buffer too, and the report says so
  (let ((tuatara-user:*split-limit* 1))
    (multiple-value-bind (f editor)
        (run-keys-in-turns '("% f b RET d" "") :text (format nil "b~%a~%b~%c~%b~%") :folds '((3 . 5)))
      (is string= (format nil "~%a~%b~%c~%~%") (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 hidden in folds left alone")))
    (multiple-value-bind (f editor)
        (run-keys-in-turns '("l v g e f b RET d" "") :text (format nil "xb~%a~%b~%c~%b~%") :folds '((4 . 6)))
      (is string= (format nil "x~%a~%b~%c~%~%") (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 hidden in folds left alone"))))
  (let ((tuatara.defaults::*rewrite-threshold* 0))
    (multiple-value-bind (f editor)
        (run-keys-in-turns '("m v j l d" "") :text (format nil "a~%b~%c~%d~%") :folds '((1 . 3)))
      (is string= (format nil "~%b~%~%d~%") (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "1 left as they were, partly hidden in a fold")))))

(define-test (objects rectangle-paste-counts)
  ;; a count pastes each row that many times across, over a rectangle too; 0 pastes nothing, keeping the selection
  (let ((tuatara-user:*modal-editing* nil))
    (setf tuatara.defaults::*killed-rectangle* (list "XY" "Z"))
    (multiple-value-bind (f editor) (session-in nil (format nil "abc~%def~%") "C-u 2 C-x r y" :point 1)
      (declare (ignore f))
      (is string= (format nil "aXYXYbc~%dZZef~%") (session-text editor)))
    (setf tuatara.defaults::*killed-rectangle* (list "XY" "Z"))
    (multiple-value-bind (f editor) (session-in nil (format nil "abc~%def~%") "C-f C-f C-x SPC C-n C-b C-u 2 C-x r y" :point 0)
      (declare (ignore f))
      (is string= (format nil "aXYXYc~%dZZf~%") (session-text editor)))
    (setf tuatara.defaults::*killed-rectangle* (list "XY" "Z"))
    (multiple-value-bind (f editor) (session-in nil (format nil "abc~%def~%") "C-SPC C-f C-u 0 C-x r y" :point 0)
      (is string= (format nil "abc~%def~%") (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "A count of 0 pastes nothing")))))

(define-test (objects symbol-characters-alike)
  ;; one rule for a symbol's characters: in Lisp *foo* is one symbol for selecting and for searching alike
  (let ((b (tuatara.buffer:make-buffer "sym-test.lisp" :text "(setf *foo-bar* 1+)")))
    (unwind-protect
         (progn
           (tuatara.defaults::set-major-mode b 'tuatara.defaults::lisp-mode)
           (is equal '(6 15) (multiple-value-list (tuatara.defaults::symbol-at (tuatara.buffer:buffer-text b) 8 b))))
      (tuatara.buffer:kill-buffer b)))
  (let ((b (tuatara.buffer:make-buffer "sym-test.txt" :text "a *foo* b")))
    (unwind-protect
         (is equal '(3 6) (multiple-value-list (tuatara.defaults::symbol-at (tuatara.buffer:buffer-text b) 4 b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (objects two-spaces-between-sentences)
  ;; filled with two spaces after a sentence's end where it had two or a line end
  (flet ((filled (keys text) (session-text (nth-value 1 (run-keys keys :text text)))))
    (let ((tuatara-user:*sentence-end-double-space* t))
      (is string= (format nil "One two.  Three e.g. four~%five.  Six.~%")
          (filled "C-u 2 6 C-x f M-q RET" (format nil "One two.  Three e.g. four five.~%Six.~%"))))
    (let ((tuatara-user:*sentence-end-double-space* nil))
      (is string= (format nil "One two. Three e.g. four~%five. Six.~%")
          (filled "C-u 2 6 C-x f M-q RET" (format nil "One two.  Three e.g. four five.~%Six.~%")))))
  ;; a sentence ends at two spaces, a tab or a line end, never at one space
  (let ((b (tuatara.buffer:make-buffer "*two-spaces*" :text (format nil "See e.g. this.  Next one!~%Last"))))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local b :sentence-end-double-space) t)
           (is = 14 (tuatara.defaults::sentences-forward b 0 1))
           (is = 25 (tuatara.defaults::sentences-forward b 14 1))
           (is = 16 (tuatara.defaults::sentences-backward b 25 1))
           (setf (tuatara.buffer:buffer-local b :sentence-end-double-space) nil)
           ;; one space apart, still not after an abbreviation's full stop
           (is = 14 (tuatara.defaults::sentences-forward b 0 1))
           (is = 16 (tuatara.defaults::sentences-backward b 25 1)))
      (tuatara.buffer:kill-buffer b))))

(define-test (objects abbreviations)
  ;; an abbreviation's full stop ends no sentence: Tuatara's, capitalised, after a bracket; your own added, one taken out
  (let ((b (tuatara.buffer:make-buffer "*abbreviations*" :text "")))
    (unwind-protect
         (flet ((abbreviation-p (text)
                  (tuatara.buffer:buffer-edit b (vector (tuatara.text:make-edit 0 (tuatara.buffer:buffer-size b) (tuatara.text:string-octets text))))
                  (tuatara.defaults::abbreviation-before-p b (tuatara.buffer:buffer-size b))))
           (true (abbreviation-p "see e.g."))
           (true (abbreviation-p "E.g."))
           (true (abbreviation-p "(cf."))
           (true (abbreviation-p "Ask Dr."))
           (false (abbreviation-p "ask dr."))
           (false (abbreviation-p "He sat."))
           (false (abbreviation-p "Xe.g."))
           (false (abbreviation-p "The end."))
           (false (abbreviation-p (format nil "Dr~%.")))
           (let ((tuatara-user:*extra-sentence-abbreviations* '("Inc."))
                 (tuatara-user:*removed-sentence-abbreviations* '("St.")))
             (true (abbreviation-p "Acme Inc."))
             (false (abbreviation-p "Main St."))
             (true (tuatara.defaults::abbreviation-ending-p "Inc."))
             (false (tuatara.defaults::abbreviation-ending-p "St."))))
      (tuatara.buffer:kill-buffer b)))
  ;; filling with two spaces between sentences: none after an abbreviation, whatever followed it
  (let ((tuatara-user:*sentence-end-double-space* t))
    (is string= (format nil "Ask Dr. Who (e.g.) now.  Then~%")
        (session-text (nth-value 1 (run-keys "C-u 4 0 C-x f M-q RET" :text (format nil "Ask Dr.~%Who (e.g.)  now.~%Then~%")))))))

(define-test (objects selection-remade-counts-when-used)
  ;; what makes a selection again is kept before every command but counts nothing until
  ;; it is used: no cost per key while text is selected
  (let ((b (tuatara.buffer:make-buffer "*remade*" :text (make-string 60000 :initial-element #\a)))
        (counted 0))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-mark b) 50000 (tuatara.buffer:buffer-mark-active-p b) t
                 (tuatara.buffer:buffer-point b) 0)
           (dolist (name '(tuatara.text:rope-count-chars tuatara.text:byte->char))
             (sb-int:encapsulate name 'counted (lambda (function &rest arguments) (incf counted) (apply function arguments))))
           (unwind-protect (true (functionp (tuatara.defaults::selection-remaker b)))
             (dolist (name '(tuatara.text:rope-count-chars tuatara.text:byte->char))
               (sb-int:unencapsulate name 'counted)))
           (is = 0 counted))
      (tuatara.buffer:kill-buffer b))))

(define-test (objects selected-region)
  ;; the selection as (START END) while text is selected; NIL with none, an empty one or a mark let go of
  (let ((b (tuatara.buffer:make-buffer "*selected-region*" :text "one two")))
    (unwind-protect
         (progn
           (false (tuatara.defaults::selected-region b))
           (setf (tuatara.buffer:buffer-mark b) 4 (tuatara.buffer:buffer-mark-active-p b) t
                 (tuatara.buffer:buffer-point b) 0)
           (is equal '(0 4) (tuatara.defaults::selected-region b))
           (setf (tuatara.buffer:buffer-point b) 4)
           (false (tuatara.defaults::selected-region b))
           (setf (tuatara.buffer:buffer-point b) 7 (tuatara.buffer:buffer-mark-active-p b) nil)
           (false (tuatara.defaults::selected-region b)))
      (tuatara.buffer:kill-buffer b))))
