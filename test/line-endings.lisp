;;;; test/line-endings.lisp --- Lines ended by a carriage return and a newline, or by either alone.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test line-endings
  :description "A Windows file (lines ending CR LF) is shown and edited without a ^M on every line, and saved exactly as it was apart from the edits: RET makes CR LF there, and a CR LF is one step. A carriage return no newline follows ends a line in every buffer: counted, found, shown, searched and sorted so, and RET makes one in a file whose lines end so.")

(define-test (line-endings detection)
  (flet ((of (string) (tuatara.buffer:detect-line-ending (tuatara.text:string->rope string))))
    (is eq :crlf (of (format nil "a~c~%b~c~%" #\Return #\Return)))
    (is eq :lf (of (format nil "a~%b~c~%" #\Return)))           ; the first line end decides
    (is eq :lf (of "no line end"))
    (is eq :lf (of "")))
  ;; of a part of the text, as the files searched together are each judged
  (let ((rope (tuatara.text:string->rope (format nil "a~%b~c~%c~%~c~%" #\Return #\Return))))
    (is eq :lf (tuatara.buffer:detect-line-ending rope :start 0 :end 2))
    (is eq :crlf (tuatara.buffer:detect-line-ending rope :start 2 :end 5))
    (is eq :lf (tuatara.buffer:detect-line-ending rope :start 5 :end 7))
    (is eq :lf (tuatara.buffer:detect-line-ending rope :start 4 :end 5))
    (is eq :crlf (tuatara.buffer:detect-line-ending rope :start 7))
    ;; the text of a line: before its carriage return with CRLF, a carriage return at the end too
    (is = 3 (tuatara.text:rope-line-text-end rope 2 4 t))
    (is = 4 (tuatara.text:rope-line-text-end rope 2 4 nil))
    (is = 7 (tuatara.text:rope-line-text-end rope 7 7 t))
    (is = 7 (tuatara.text:rope-line-text-end rope 7 8 t))))

(define-test (line-endings showing-line-endings)
  (let ((rope (tuatara.text:string->rope (format nil "ab~c~%cd~c~%lone~cCR" #\Return #\Return #\Return))))
    ;; shown as lines, the carriage returns before newlines unseen; a lone one ends a line, unseen too
    (is equal '("ab" "cd" "lone" "CR")
        (map 'list (lambda (row) (coerce (tuatara.display::row-string row) 'string))
             (tuatara.display:layout rope 0 40 4 :crlf t)))
    ;; in a buffer that ends lines with a newline alone, every carriage return shows
    (is string= "ab^M" (coerce (tuatara.display::row-string (aref (tuatara.display:layout rope 0 40 1) 0)) 'string))))

(define-test (line-endings motion-and-editing)
  (let ((b (tuatara.buffer:make-buffer "*crlf-test*" :text (format nil "ab~c~%cd~c~%" #\Return #\Return))))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local b :line-ending) :crlf)
           (is = 2 (tuatara.buffer:buffer-line-end b 0))          ; before the carriage return
           (is = 4 (tuatara.buffer:buffer-forward-char b 2))      ; over both at once
           (is = 2 (tuatara.buffer:buffer-forward-char b 4 -1))
           (is = 6 (tuatara.buffer:buffer-line-end b 4))
           (is string= (coerce '(#\Return #\Newline) 'string) (tuatara.buffer:buffer-newline b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (line-endings files)
  (with-test-directory (dir)
    (let ((path (merge-pathnames "windows.txt" dir))
          (crlf (coerce '(#\Return #\Newline) 'string)))
      (put-file path (format nil "ab~acd~a" crlf crlf))
      ;; C-e stops before the carriage return; RET there makes a CR LF; the mode line says CRLF
      (multiple-value-bind (f editor) (run-keys "C-e i RET Z C-g C-x C-s" :file path :width 100)
        (true (tuatara.frontend.fake:fake-frame-showing f "CRLF"))
        (false (tuatara.frontend.fake:fake-frame-showing f "^M"))
        (is string= (format nil "ab~aZ~acd~a" crlf crlf crlf) (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; C-d at the end of a line joins it with the next, taking both octets
      (multiple-value-bind (f editor) (run-keys "C-e C-d C-x C-s" :file path :width 100)
        (declare (ignore f))
        (is string= (format nil "abZ~acd~a" crlf crlf) (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; o opens a line below with a CR LF of its own
      (multiple-value-bind (f editor) (run-keys "o Y C-g C-x C-s" :file path :width 100)
        (declare (ignore f))
        (is string= (format nil "abZ~aY~acd~a" crlf crlf crlf) (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor))))))

(define-test (line-endings searching-and-pasting)
  (let ((crlf (format nil "ab~c~%cb~c~%" #\Return #\Return)))
    (flet ((after (keys &optional (text crlf))
             (session-text (nth-value 1 (run-keys-in-turns (list keys "") :text text :crlf t)))))
      ;; $ matches before the carriage return; . takes neither it nor the newline
      (is string= (format nil "aX~c~%cX~c~%" #\Return #\Return) (after (format nil "r r ~a RET" (key-string "%b$/X"))))
      (is string= (format nil "aY~c~%cY~c~%" #\Return #\Return) (after (format nil "r r ~a RET" (key-string "%.$/Y"))))
      (is string= crlf (after (format nil "r r ~a RET" (key-string "%b./Z"))))
      ;; f the same
      (is string= (format nil "aW~c~%cW~c~%" #\Return #\Return) (after "% f b $ RET c W C-g"))
      ;; a kill takes the line ends of the buffer it is pasted in: here, into a
      ;; buffer whose lines end with a newline alone
      (let ((other (after "V j y C-x b o t h e r C-j p")))
        (false (find #\Return other))
        (true (search (format nil "ab~%cb~%") other))
        ;; the buffer pasted in, changed, is left to no test after
        (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "other")))
      ;; and the other way, into this one
      (let ((b (session-buffer (nth-value 1 (run-keys-in-turns '("") :text crlf :crlf t)))))
        (is string= (format nil "x~c~%y~c~%" #\Return #\Return)
            (tuatara.defaults::text-for-buffer (format nil "x~%y~c~%" #\Return) b)))
      ;; lines pasted one to a cursor carry no carriage return with them
      (let ((pasted (after "V j y g e k k t p" (format nil "1~c~%2~c~%ab~c~%cd~c~%" #\Return #\Return #\Return #\Return))))
        (is = 4 (count #\Return pasted))
        (is = 2 (count #\2 pasted))))))

;;; A carriage return no newline follows ends a line too

(defun cr (text)
  "TEXT, a format control, made with each | a carriage return and each ~% a newline."
  (substitute #\Return #\| (format nil text)))

(define-test (line-endings lone-returns-end-lines)
  ;; a, b, c, d and an empty last line: ended by CR, CR LF, LF and CR
  (let ((rope (tuatara.text:string->rope (cr "a|b|~%c~%d|"))))
    (is = 4 (tuatara.text:rope-lines rope))
    (is equal '(0 2 5 7 9) (loop for line from 0 to 4 collect (tuatara.text:line->byte rope line)))
    (is equal '(0 0 1 1 1 2 2 3 3 4) (loop for byte from 0 to 9 collect (tuatara.text:byte->line rope byte)))
    (is equal '(0 0 2 2 2 5 5 7 7 9) (loop for byte from 0 to 9 collect (tuatara.text:rope-line-start rope byte)))
    (is equal '(1 1 4 4 4 6 6 8 8 9) (loop for byte from 0 to 9 collect (tuatara.text:rope-line-end rope byte)))
    (is equal '(t nil t nil nil t nil t nil t) (loop for byte from 0 to 9 collect (tuatara.text:rope-line-start-p rope byte)))
    (is = 3 (tuatara.text:rope-count-newlines rope 0 8))
    ;; the carriage return before a newline ends no line of its own, counted to there or from there
    (is = 1 (tuatara.text:rope-count-newlines rope 0 4))
    (is = 2 (tuatara.text:rope-count-newlines rope 3 7))
    (is = 3 (tuatara.text:rope-line-text-end rope 2 4 t))
    (is = 8 (tuatara.text:rope-line-text-end rope 7 8 t)))
  ;; a newline put after a carriage return joins them in one line end; a letter put between
  ;; parts them into two
  (flet ((lines-after (text start end insert)
           (let ((new (tuatara.text:apply-edits (tuatara.text:string->rope text)
                                                (vector (tuatara.text:make-edit start end (and insert (tuatara.text:string-octets insert)))))))
             (true (tuatara.text:check-rope new))
             (tuatara.text:rope-lines new))))
    (is = 1 (lines-after (cr "x|y~%z") 2 3 nil))
    (is = 2 (lines-after (cr "x|~%z") 2 2 "y"))
    (is = 1 (lines-after (cr "x|z") 2 2 (string #\Newline)))
    (is = 1 (lines-after (cr "xy~%z") 2 2 (string #\Return))))
  ;; at the edges of leaves: a carriage return and its newline are never parted
  (let* ((text (with-output-to-string (o)
                 (dotimes (i 40) (write-string (make-string (- 4095 (mod i 3)) :initial-element #\a) o)
                   (write-string (cr (if (evenp i) "|~%" "|")) o))))
         (rope (tuatara.text:string->rope text)))
    (true (tuatara.text:check-rope rope))
    (is = (string-lines text) (tuatara.text:rope-lines rope))
    (loop for at in '(4095 4096 8191 8192 12287)
          do (let ((new (tuatara.text:rope-delete rope at (1+ at))))
               (true (tuatara.text:check-rope new))
               (is = (string-lines (tuatara.text:rope-string new)) (tuatara.text:rope-lines new)))))
  ;; a file read in spans counted in parallel: a carriage return at the end of one, its
  ;; newline at the start of the next, is one line end, counted before and after
  (with-test-directory (dir)
    (let* ((text (concatenate 'string (make-string 65535 :initial-element #\a) (cr "|~%b|c~%")
                              (make-string 70000 :initial-element #\b) (cr "|d")))
           (path (put-file (merge-pathnames "spans.txt" dir) text))
           (rope (tuatara.text:open-file-rope (namestring path)))
           (tally (tuatara.text:make-tally))
           (counted (tuatara.text:index-rope rope :threads 2 :chunk 4096 :tally tally)))
      (is = 2 (tuatara.text:rope-count-newlines rope 65530 65540))
      (is = (string-lines text) (tuatara.text:rope-count-newlines rope 0 (length text)))
      (true (tuatara.text:check-rope counted))
      (is = (string-lines text) (tuatara.text:rope-lines counted))
      (is = 65537 (tuatara.text:line->byte counted 1))
      (is = 65539 (tuatara.text:tally-line-start tally rope 2))
      (is = 2 (tuatara.text:tally-line-number tally rope 65540)))))

(define-test (line-endings lone-return-files)
  (with-test-directory (dir)
    (let ((path (merge-pathnames "mac.txt" dir)))
      (put-file path (cr "one|two|three|"))
      (is eq :cr (tuatara.buffer:detect-line-ending (tuatara.text:string->rope (cr "one|two"))))
      ;; shown as lines, the line's number in the mode line, CR there; RET makes a carriage return
      (multiple-value-bind (f editor) (run-keys "C-n C-n C-e i RET Z C-g C-x C-s" :file path :width 100)
        (true (tuatara.frontend.fake:fake-frame-showing f " CR "))
        (true (tuatara.frontend.fake:fake-frame-showing f "L4"))
        (false (tuatara.frontend.fake:fake-frame-showing f "^M"))
        (is string= "two" (string-right-trim " " (tuatara.frontend.fake:fake-screen-line f 1)))
        (is string= (cr "one|two|three|Z|") (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor))))
    ;; a file of every kind of line end is saved as it was but for the edit
    (let ((path (merge-pathnames "mixed.txt" dir)))
      (put-file path (cr "a|~%b~%c|d|~%e"))
      (multiple-value-bind (f editor) (run-keys "C-n C-n C-n C-e i ! C-g C-x C-s" :file path :width 100)
        (declare (ignore f))
        (is string= (cr "a|~%b~%c|d!|~%e") (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor))))))

(define-test (line-endings lone-returns-searched-and-sorted)
  ;; ^ and $ at every line end, . at none of them, the same in a buffer of each kind
  (let ((rope (tuatara.text:string->rope (cr "ab|cd|~%ef~%gh"))))
    (flet ((starts (pattern) (map 'list #'first (tuatara.syntax:regex-search-all rope pattern))))
      (is equal '(0 3 7 10) (starts "^\\w"))
      (is equal '(1 4 8 11) (starts "\\w$"))
      (is equal '() (starts "b.c"))
      (is equal '() (starts "d.\\n"))
      (is equal '(1) (starts "b\\rc"))))
  ;; lines sorted with the line ends staying where they were
  (is string= (cr "a|b~%c|") (session-text (nth-value 1 (run-keys "C-x h M-x s o r t - l i n e s RET" :text (cr "c|b~%a|"))))))

(defun ending-session (text ending parts &key (point 0) mode mark modal)
  "Run an editor session on a scratch buffer holding TEXT, its lines ending as
ENDING says (:LF, :CRLF or :CR), point at POINT, the mark active at MARK
when given, in MODE when given, in the modal states with MODAL, with PARTS
typed: each a key string, or (:PASTE TEXT) for text pasted into the
terminal. Returns the buffer's text."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 10)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (let ((b (tuatara.buffer:make-buffer "*scratch*" :text text)))
      (setf (tuatara.buffer:buffer-local b :line-ending) ending
            (tuatara.buffer:buffer-point b) point)
      (when mark (setf (tuatara.buffer:buffer-mark b) mark (tuatara.buffer:buffer-mark-active-p b) t))
      (when mode (tuatara.defaults::set-major-mode b mode)))
    (dolist (part parts)
      (if (consp part)
          (tuatara.frontend.fake:fake-queue-event f (tuatara.frontend:make-event :paste :text (second part)))
          (queue-test-keys f (list part))))
    (session-text (without-grammar-questions
                    (let ((tuatara.defaults::*modal-editing* modal))
                      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                    :buffer (tuatara.buffer:find-buffer "*scratch*")))))))

(define-test (line-endings commands-join-no-line-ends)
  (flet ((lines (text) (tuatara.text:rope-lines (tuatara.text:string->rope text))))
    ;; RET and C-o at a line's start after a carriage return alone: a line more, never a CR LF made
    (is string= (cr "a||b") (ending-session (cr "a|b") :lf '("RET") :point 2))
    (is string= (cr "a||b") (ending-session (cr "a|b") :lf '("C-o") :point 2))
    ;; RET at the end of a line ended by a newline, in a buffer of carriage returns: a newline
    (is = 2 (lines (ending-session (format nil "a~%b") :cr '("RET") :point 1)))
    ;; text yanked or pasted that would join a line end there takes its kind
    (let ((b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "join") :text (cr "a|b~%c"))))
      (unwind-protect
           (progn (is string= (cr "|x") (tuatara.buffer:text-for-buffer (format nil "~%x") b 2))
                  (is string= (format nil "x~%") (tuatara.buffer:text-for-buffer (cr "x|") b 3))
                  (is string= (format nil "~%x") (tuatara.buffer:text-for-buffer (format nil "~%x") b 1)))
        (tuatara.buffer:kill-buffer b)))
    ;; text pasted into the terminal takes the buffer's line ends as a yank does: CR LF into a
    ;; buffer of carriage returns is one carriage return
    (is string= (cr "ax|y|b") (ending-session "ab" :cr (list (list :paste (format nil "x~c~%y~c~%" #\Return #\Return))) :point 1))
    ;; DEL after a carriage return and a newline in a buffer of newlines takes both: the lines join
    (is string= "ab" (ending-session (cr "a|~%b") :lf '("DEL") :point 3))))

(define-test (line-endings line-ends-of-each-line-kept)
  ;; text put back by lines tells a line ended by a carriage return from one ended by a newline
  (let ((b (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "lines") :text (cr "a|b~%"))))
    (unwind-protect
         (progn (true (plusp (tuatara.defaults::put-text-by-lines b (tuatara.buffer:buffer-text b) (format nil "a~%b~%"))))
                (is string= (format nil "a~%b~%") (tuatara.buffer:buffer-string b)))
      (tuatara.buffer:kill-buffer b)))
  ;; a subtree moved keeps each of its lines' ends, in a buffer of each kind
  (loop for (ending text moved) in (list (list :lf (cr "* A|body~%* B~%") (cr "* B~%* A|body~%"))
                                         (list :crlf (cr "* A|~%x|y|~%* B|~%") (cr "* B|~%* A|~%x|y|~%"))
                                         (list :cr (cr "* A~%body|* B|") (cr "* B|* A~%body|")))
        do (is string= moved (ending-session text ending '("M-x o r g - m o v e - s u b t r e e - d o w n RET")
                                             :mode 'tuatara.defaults::org-mode)
               "~a" ending))
  ;; a check of a rope sees a carriage return and its newline parted by an empty leaf
  (let ((rope (tuatara.text::%make-rope (tuatara.text::make-branch
                                         (vector (tuatara.text::make-leaf (tuatara.text:string-octets (cr "a|")))
                                                 (tuatara.text::make-leaf (tuatara.text:make-octets 0))
                                                 (tuatara.text::make-leaf (tuatara.text:string-octets (format nil "~%b"))))))))
    (fail (tuatara.text:check-rope rope))))

(define-test (line-endings tree-sitter-rows-by-newlines)
  ;; tree-sitter counts rows by newlines alone: the points of its edits too, a carriage
  ;; return before a newline in the text or at an insert's end ending no row
  (let ((rope (tuatara.text:string->rope (cr "x = 1|y = 2~%z = 3"))))
    (is equal '(0 . 6) (tuatara.syntax::point-at rope 6))
    (is equal '(1 . 1) (tuatara.syntax::point-at rope 13))
    (is equal '(0 . 8) (tuatara.syntax::point-after '(0 . 6) (tuatara.text:string-octets (cr "a|"))))
    (is equal '(1 . 0) (tuatara.syntax::point-after '(0 . 6) (tuatara.text:string-octets (cr "a|~%"))))))

(define-test (line-endings plain-reading-without-returns)
  ;; a text without a carriage return is searched with the pattern as written, quicker and the
  ;; same there; one with a carriage return, by the line rule; its nodes say which in O(1)
  (let* ((scanner (tuatara.syntax:compile-regex "^\\d+$"))
         (plain-rope (tuatara.text:string->rope (format nil "12~%ab~%34~%")))
         (cr-rope (tuatara.text:string->rope (cr "12|ab~%34~%"))))
    (false (tuatara.text:rope-holds-return-p plain-rope))
    (true (tuatara.text:rope-holds-return-p cr-rope))
    (false (tuatara.text:rope-holds-return-p (tuatara.text:rope-delete cr-rope 2 3)))
    (isnt eq scanner (tuatara.syntax::scanner-for-rope scanner plain-rope))
    (is eq scanner (tuatara.syntax::scanner-for-rope scanner cr-rope))
    (is equal '(0 6) (map 'list #'first (tuatara.syntax:regex-search-all plain-rope scanner)))
    (is equal '(0 6) (map 'list #'first (tuatara.syntax:regex-search-all cr-rope scanner)))))

(define-test (line-endings deletions-take-a-line-end-whole)
  ;; in a buffer of newlines, a carriage return before a newline shows, but the two end one
  ;; line: a deletion or kill that would part them takes both, so the lines join
  (dolist (case (list (list "C-d" 2) (list "C-k" 2) (list "DEL" 3) (list "C-w" 3 2) (list "d" 2 nil t)))
    (destructuring-bind (keys point &optional mark modal) case
      (is string= "ab" (ending-session (cr "a|~%b") :lf (list keys) :point point :mark mark :modal modal) "~a" keys)))
  ;; joining lines takes both too
  (is string= "a b" (ending-session (cr "a|~%b") :lf '("M-^") :point 3))
  ;; a selection ending between the two leaves the line end: in a buffer of newlines the
  ;; newline, in one of CR LF line ends the pair, taken back before the carriage return
  (is string= (format nil "~%cd~%") (ending-session (cr "ab|~%cd~%") :lf '("C-w") :point 3 :mark 0))
  (is string= (cr "a|~%cd|~%") (ending-session (cr "ab|~%cd|~%") :crlf '("C-w") :point 3 :mark 1)))
