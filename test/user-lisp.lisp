;;;; test/user-lisp.lisp --- Emacs's names for the basics, eldoc, C-h f.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test user-lisp
  :description "The Lisp one types has functions for the basics of buffers and their text, positions counted in characters from 1; a Lisp buffer shows the parameters of the call point is in; C-h f describes any function.")

(defmacro with-lisp-buffer ((text) &body body)
  "Run BODY with a new buffer holding TEXT current for the Lisp one types, point
at its start, killed afterwards."
  (let ((b (gensym)))
    `(let* ((,b (tuatara.buffer:make-buffer (format nil "*user-lisp-~d*" (random 1000000)) :text ,text))
            (tuatara.command:*current-buffer* ,b))
       (unwind-protect (progn ,@body)
         (tuatara.buffer:kill-buffer ,b)))))

(define-test (user-lisp positions-and-text)
  (with-lisp-buffer ((format nil "héllo~%world~%"))
    ;; positions are characters from 1, é one of them
    (is = 1 (tuatara-user:point))
    (is = 1 (tuatara-user:point-min))
    (is = 13 (tuatara-user:point-max))
    (is = 12 (tuatara-user:buffer-size))
    (is = 4 (tuatara-user:goto-char 4))
    (is char= #\l (tuatara-user:char-after))
    (is char= #\l (tuatara-user:char-before))
    (is char= #\é (tuatara-user:char-before 3))
    (is = 1 (tuatara-user:line-beginning-position))
    (is = 6 (tuatara-user:line-end-position))
    (is = 7 (tuatara-user:line-beginning-position 2))
    (is string= "héllo" (tuatara-user:buffer-substring 1 6))
    (is string= "héllo" (tuatara-user:buffer-substring 6 1))
    ;; words and line numbers: count-words and line-number-at-pos
    (is = 2 (tuatara-user:count-words 1 13))
    (is = 2 (tuatara-user:count-words 2 8))
    (is = 1 (tuatara-user:line-number-at-pos))
    (is = 2 (tuatara-user:line-number-at-pos 7))
    (is = 3 (tuatara-user:line-number-at-pos 13 t))
    (fail (tuatara-user:buffer-substring 1 99))
    ;; insert at point, point after it
    (tuatara-user:goto-char 6)
    (tuatara-user:insert "!" #\?)
    (is = 8 (tuatara-user:point))
    (is string= (format nil "héllo!?~%world~%") (tuatara-user:buffer-string))
    (tuatara-user:delete-region 6 8)
    (is string= (format nil "héllo~%world~%") (tuatara-user:buffer-string))
    ;; going too far is clamped
    (tuatara-user:goto-char 999)
    (true (tuatara-user:eobp))
    (tuatara-user:goto-char -5)
    (true (tuatara-user:bobp))
    (true (tuatara-user:bolp))
    ;; forward-line: the lines not gone; the last line without a newline counts when point moves
    (is = 0 (tuatara-user:forward-line 1))
    (is = 7 (tuatara-user:point))
    (is = 1 (tuatara-user:forward-line 2))
    (is = 13 (tuatara-user:point))
    (is = -1 (tuatara-user:forward-line -3))
    (is = 1 (tuatara-user:point))
    (is = 2 (tuatara-user:count-lines 1 13))
    (is = 2 (tuatara-user:count-lines 1 10))
    (tuatara-user:goto-char 12)
    (tuatara-user:forward-char)
    (fail (tuatara-user:forward-char))
    (true (tuatara-user:eobp))
    (tuatara-user:erase-buffer)
    (is string= "" (tuatara-user:buffer-string)))
  ;; a last line without a newline
  (with-lisp-buffer ("abc")
    (is = 0 (tuatara-user:forward-line 1))
    (is = 4 (tuatara-user:point))
    (is = 1 (tuatara-user:forward-line 1))))

(define-test (user-lisp buffer-functions)
  (with-lisp-buffer ("first")
    (let ((other (tuatara-user:get-buffer-create "*user-lisp-other*")))
      (unwind-protect
           (progn
             (is eq other (tuatara-user:get-buffer "*user-lisp-other*"))
             (false (tuatara-user:get-buffer "*no such buffer*"))
             (true (member other (tuatara-user:buffer-list)))
             ;; with-current-buffer: the body's buffer, then back
             (tuatara-user:with-current-buffer other
               (tuatara-user:insert "in other")
               (is string= "*user-lisp-other*" (tuatara-user:buffer-name)))
             (is string= "first" (tuatara-user:buffer-string))
             (is string= "in other" (tuatara.buffer:buffer-string other))
             ;; save-excursion: point back where it was, moved along by what went in before it
             (tuatara-user:goto-char 3)
             (tuatara-user:save-excursion
               (tuatara-user:goto-char 1)
               (tuatara-user:insert "XY"))
             (is = 5 (tuatara-user:point))
             (false (tuatara-user:buffer-file-name))
             (fail (tuatara-user:buffer-name "*no such buffer*")))
        (tuatara.buffer:kill-buffer other)))))

(define-test (user-lisp narrowed-lisp-sees-the-part)
  ;; "one two three four" on four lines: narrow to the second and third
  (with-lisp-buffer ((format nil "one~%two~%three~%four~%"))
    (tuatara-user:narrow-to-region 5 15)
    (is = 5 (tuatara-user:point-min))
    (is = 15 (tuatara-user:point-max))
    (is string= (format nil "two~%three~%") (tuatara-user:buffer-string))
    (is = 19 (tuatara-user:buffer-size) "size is of the whole text")
    ;; moving: goto-char is taken to the part's edges
    (tuatara-user:goto-char 1)
    (is = 5 (tuatara-user:point))
    (tuatara-user:goto-char 99)
    (is = 15 (tuatara-user:point))
    (true (tuatara-user:eobp))
    (false (tuatara-user:char-after))
    (tuatara-user:goto-char 5)
    (true (tuatara-user:bobp))
    (fail (tuatara-user:forward-char -1))
    (is = 5 (tuatara-user:point))
    (is = -1 (tuatara-user:forward-line -1))
    (is = 5 (tuatara-user:line-beginning-position))
    ;; reading outside is refused, inside is not
    (is string= "two" (tuatara-user:buffer-substring 5 8))
    (fail (tuatara-user:buffer-substring 1 8))
    (fail (tuatara-user:buffer-substring 5 20))
    (fail (tuatara-user:count-words 1 8))
    (fail (tuatara-user:count-lines 1 8))
    (fail (tuatara-user:line-number-at-pos 2))
    ;; deleting outside is refused and changes nothing
    (fail (tuatara-user:delete-region 1 8))
    (fail (tuatara-user:delete-region 10 20))
    (is string= (format nil "two~%three~%") (tuatara-user:buffer-string))
    ;; inserting: point can not be outside, an insertion at the part's edge stays in it
    (tuatara-user:goto-char 5)
    (tuatara-user:insert "X")
    (is string= (format nil "Xtwo~%three~%") (tuatara-user:buffer-string))
    (is string= (format nil "one~%Xtwo~%three~%four~%")
        (tuatara.buffer:buffer-string (tuatara-user:current-buffer)))
    ;; widen shows it all
    (is eq nil (tuatara-user:widen))
    (is = 1 (tuatara-user:point-min))
    (is = 21 (tuatara-user:point-max))))

(define-test (user-lisp save-restriction)
  (with-lisp-buffer ((format nil "one~%two~%three~%four~%"))
    (tuatara-user:narrow-to-region 5 15)
    ;; the body can widen and read outside; the narrowing is back after
    (is string= "one"
        (tuatara-user:save-restriction
          (tuatara-user:widen)
          (tuatara-user:buffer-substring 1 4)))
    (is = 5 (tuatara-user:point-min))
    (is = 15 (tuatara-user:point-max))
    ;; and after an error inside
    (fail (tuatara-user:save-restriction
            (tuatara-user:widen)
            (error "stop")))
    (is = 5 (tuatara-user:point-min))
    (is = 15 (tuatara-user:point-max))
    ;; and after a non-local exit
    (block out
      (tuatara-user:save-restriction
        (tuatara-user:widen)
        (return-from out nil)))
    (is = 15 (tuatara-user:point-max))
    ;; text put in meanwhile before the part moves it along; in it, grows it
    (tuatara-user:save-restriction
      (tuatara-user:widen)
      (tuatara-user:goto-char 1)
      (tuatara-user:insert "AA")
      (tuatara-user:goto-char 7)
      (tuatara-user:insert "-"))
    (is = 7 (tuatara-user:point-min))
    (is = 18 (tuatara-user:point-max))
    (is string= (format nil "-two~%three~%") (tuatara-user:buffer-string))
    ;; a buffer that was not narrowed is widened again
    (tuatara-user:widen)
    (tuatara-user:save-restriction
      (tuatara-user:narrow-to-region 1 4))
    (is = 1 (tuatara-user:point-min))
    (is = 23 (tuatara-user:point-max))
    ;; nested: each puts back its own
    (tuatara-user:narrow-to-region 3 10)
    (tuatara-user:save-restriction
      (tuatara-user:narrow-to-region 4 6)
      (tuatara-user:save-restriction (tuatara-user:widen))
      (is = 4 (tuatara-user:point-min)))
    (is = 3 (tuatara-user:point-min))
    (is = 10 (tuatara-user:point-max))
    ;; narrowing inside a narrowing can not reach outside it
    (fail (tuatara-user:narrow-to-region 1 6))))

(define-test (user-lisp package-api-keeps-the-whole-buffer)
  ;; the package API works on octet offsets of the whole text, narrowed or not
  (with-lisp-buffer ((format nil "one~%two~%three~%"))
    (tuatara-user:narrow-to-region 5 8)
    (let ((b (tuatara-user:current-buffer)))
      (tuatara.buffer:buffer-insert b 0 "API ")
      (is string= (format nil "API one~%two~%three~%") (tuatara.buffer:buffer-string b))
      (is string= "two" (tuatara-user:buffer-string)))))

(define-test (user-lisp narrowed-lisp-at-the-keyboard)
  (flet ((run (lisp)
           (multiple-value-bind (f editor)
               (narrowing-keys (format nil "C-n C-SPC C-n C-n C-x x n M-: ~a RET" (key-string lisp))
                         :text (format nil "one~%two~%three~%four~%five~%"))
             (declare (ignore f))
             (session-text editor))))
    ;; M-: (goto-char 1) lands at the part's start
    (is string= (format nil "one~%Xtwo~%three~%four~%five~%")
        (run "(progn (goto-char 1) (insert \"X\"))"))
    ;; outside the part is refused and nothing changes
    (is string= (format nil "one~%two~%three~%four~%five~%")
        (run "(delete-region 1 3)"))
    ;; save-restriction and widen reach outside, and the narrowing is back after
    (is string= (format nil "ABC~%two~%three~%four~%five~%")
        (run "(progn (save-restriction (widen) (goto-char 1) (delete-region 1 4) (insert \"ABC\")) (goto-char (point-min)))"))
    (is string= (format nil "one~%Ztwo~%three~%four~%five~%")
        (run "(progn (save-restriction (widen) (buffer-substring 1 4)) (goto-char 1) (insert \"Z\"))"))))

(define-test (user-lisp at-the-keyboard)
  ;; M-: sees the same functions: (insert "x") goes in the buffer M-: was typed in
  (multiple-value-bind (f editor) (run-keys (format nil "M-: ~a RET" (key-string "(insert \"x\")")) :text "abc")
    (declare (ignore f))
    (is string= "xabc" (session-text editor)))
  ;; a Lisp buffer shows the parameters of the call point is in
  (let ((f (fresh-start (format nil "g e o ~a" (key-string "(insert ")) :width 160)))
    (true (showing f "(insert &rest arguments)")))
  ;; C-h f describes any function: its parameters, what it does, where it is defined
  (let ((f (fresh-start (format nil "C-h f ~a RET" (key-string "insert")) :width 160)))
    (true (tuatara.frontend.fake:fake-frame-showing f "insert is a function in TUATARA-USER"))
    (true (tuatara.frontend.fake:fake-frame-showing f "(insert &rest arguments)"))
    (true (tuatara.frontend.fake:fake-frame-showing f "user-lisp.lisp")))
  ;; and commands still, with their keys
  (let ((f (fresh-start (format nil "C-h f ~a RET" (key-string "goto-line")) :width 160)))
    (true (tuatara.frontend.fake:fake-frame-showing f "goto-line is a command."))
    (true (tuatara.frontend.fake:fake-frame-showing f "Keys:"))))

(define-test (user-lisp call-shown-without-copying-the-form)
  ;; the operator of the call point is in is found in the form's octets read into the
  ;; same place each time, nothing made for each octet before point
  (flet ((operator (text &optional (at (length text)))
           (let ((b (tuatara.buffer:make-buffer "*operator*" :text text)))
             (unwind-protect (tuatara.defaults::operator-at b at)
               (tuatara.buffer:kill-buffer b)))))
    (is string= "mapcar" (operator "(defun f ()
  (mapcar #'car "))
    (is string= "list" (operator "(a \"(b\" ; (c
 (list ;|
"))
    (is eq nil (operator "'(a b "))
    (is eq nil (operator "(f) x"))
    (is string= "f" (operator "(f (g) |a (b| "))
    (flet ((consed (count)
             (let ((b (tuatara.buffer:make-buffer "*operator*"
                                                  :text (format nil "(defparameter *data*~%  (vector ~{~d~^ ~}" (loop for i below count collect i)))))
               (unwind-protect
                    (progn (tuatara.defaults::operator-at b (tuatara.buffer:buffer-size b))
                           (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring
                                                                  (tuatara.defaults::operator-at b (tuatara.buffer:buffer-size b))))))
                 (tuatara.buffer:kill-buffer b)))))
      (let ((per-octet (/ (- (consed 9000) (consed 1500)) (* 7500 4.9))))
        (true (< per-octet 0.05) "~,3f octets consed for each octet before point" per-octet)))))

(define-test (user-lisp switch-to-buffer-makes-as-get-buffer-create)
  ;; a buffer switched to by a name is the one get-buffer-create gives, made once
  (let ((name (format nil "*switched ~36r*" (random (expt 36 6)))))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys "" :text "")
           (declare (ignore f))
           (let ((tuatara.command:*editor* editor))
             (let ((made (tuatara-user:switch-to-buffer name)))
               (is eq made (tuatara-user:get-buffer-create name))
               (is eq made (tuatara-user:switch-to-buffer name))
               (is = 1 (count name tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-name :test #'string=)))))
      (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))))

(define-test (user-lisp name-characters-alike)
  ;; a name's characters are the same for completion, indentation and reading expressions: all
  ;; but a bar, which the name at point leaves out
  (dotimes (code 256)
    (let ((char (code-char code)))
      (is eq (and (tuatara.defaults::lisp-symbol-octet-p code) (/= code 124))
          (tuatara.defaults::lisp-name-part-p char) "~s" char)))
  (true (tuatara.defaults::lisp-name-part-p (code-char #x101)))
  ;; reading expressions cuts an atom where the octets say
  (is equal '((0 3 0) (4 7 0)) (tuatara.defaults::lisp-elements (tuatara.text:string-octets (format nil "abc~cdef" #\Page)))))
