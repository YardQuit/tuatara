;;;; lisp/editing/simple.lisp --- Emacs's simple.el: joining lines, spaces, opening lines.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Editing commands of Emacs's simple.el, to Emacs's rules as its
;;;; test/lisp/simple-tests.el pins them down: joining a line to the one before
;;;; (delete-indentation, M-^), the spaces around point (M-\, M-SPC), opening a
;;;; line (C-o), going to a line's text (M-m), the blank lines about point
;;;; (C-x C-o) and trailing whitespace.

(in-package #:tuatara.defaults)

;;; Spaces around point

(defun space-bounds (buffer position &key newlines)
  "The start and end of the spaces and tabs around POSITION in BUFFER, as two
values; with NEWLINES, of the line ends among them too."
  (let ((octets (if newlines tuatara.text:*blank-octets* '(32 9))))
    (values (skip-blanks buffer position :backward t :octets octets) (skip-blanks buffer position :octets octets))))

(define-command delete-horizontal-space ()
  "Delete the spaces and tabs around point; with \\[universal-argument], only
those before it."
  (let ((b (current-buffer)) (p (point)))
    (multiple-value-bind (start end) (space-bounds b p)
      (let ((end (if (prefix-argument) p end)))
        (when (< start end) (tuatara.buffer:buffer-delete b start end))
        (goto start)))))

(define-command just-one-space ()
  "Leave one space in place of the spaces and tabs around point, point after
it; with a count N, N spaces; a negative count takes the line ends among them
too, leaving -N spaces."
  (let ((b (current-buffer)) (n (prefix)))
    (multiple-value-bind (start end) (space-bounds b (point) :newlines (minusp n))
      (goto (tuatara.buffer:buffer-replace b start end (make-string (abs n) :initial-element #\Space))))))

(define-command cycle-spacing ()
  "Change the spaces around point in turn: once, one space (\\[just-one-space],
a count as there); pressed again, none; a third time, the spaces as they were."
  (let* ((b (current-buffer))
         (state (tuatara.buffer:buffer-local b 'cycle-spacing))
         (again (and state (eq (editor-last-command *editor*) 'cycle-spacing))))
    (if (not again)
        (let ((n (prefix)))
          (multiple-value-bind (start end) (space-bounds b (point) :newlines (minusp n))
            (setf (tuatara.buffer:buffer-local b 'cycle-spacing)
                  (list :step 1 :newlines (minusp n)
                        :original (tuatara.buffer:buffer-string b :start start :end end)
                        :point-offset (- (point) start)))
            (goto (tuatara.buffer:buffer-replace b start end (make-string (abs n) :initial-element #\Space)))))
        (destructuring-bind (&key step newlines original point-offset) state
          (multiple-value-bind (start end) (space-bounds b (point) :newlines newlines)
            (if (= step 1)
                (progn (setf (getf (tuatara.buffer:buffer-local b 'cycle-spacing) :step) 2)
                       (goto (tuatara.buffer:buffer-replace b start end "")))
                (progn (setf (tuatara.buffer:buffer-local b 'cycle-spacing) nil)
                       (tuatara.buffer:buffer-replace b start end original)
                       (goto (+ start point-offset)))))))))

(defun lisp-prefix-char-p (buffer char)
  "True when CHAR goes before an expression in BUFFER's language as a quote
mark does in Lisp (' ` , #), so that no space follows it when lines are joined."
  (let ((lisp (find-class 'lisp-mode nil)))
    (and lisp (typep (buffer-major-mode buffer) lisp) (find char "'`,#"))))

(defun fixup-whitespace-at (buffer position)
  "Leave one space or none at POSITION of BUFFER in place of the spaces and
tabs around it: none at a line's start or end, before a closing bracket, or
after an opening one or a quote mark of Lisp. Returns where the spaces began,
the space left after it."
  (multiple-value-bind (start end) (space-bounds buffer position)
    (when (< start end) (tuatara.buffer:buffer-delete buffer start end))
    (let ((after (tuatara.buffer:buffer-char-at buffer start))
          (before (and (plusp start) (tuatara.buffer:buffer-char-at buffer (1- start)))))
      (unless (or (null after) (member after '(#\Newline #\Return)) (find after tuatara.text:*closers*)
                  (null before) (char= before #\Newline) (find before tuatara.text:*openers*)
                  (lisp-prefix-char-p buffer before))
        (tuatara.buffer:buffer-insert buffer start " "))
      start)))

;;; Joining lines

(defun join-lines-above (buffer bound)
  "Join the line point is on to the one above it, and so on upwards while the
line's start is after BOUND and after the start of the part shown while
the buffer is narrowed: each line end goes, and the spaces about it become
one or none (FIXUP-WHITESPACE-AT), point left at the last join."
  (loop
    (let ((bol (tuatara.buffer:buffer-line-start buffer (point))))
      ;; past the bound, point stays at the last join
      (unless (> bol bound) (return))
      (goto bol)
      ;; at the start of the text, or of the part shown: nothing above to join
      (unless (and (> bol (tuatara.buffer:buffer-start buffer)) (eql (tuatara.buffer:buffer-char-at buffer (1- bol)) #\Newline))
        (return))
      (let ((start (if (and (> bol 1) (eql (tuatara.buffer:buffer-char-at buffer (- bol 2)) #\Return))
                       (- bol 2)
                       (1- bol))))
        (tuatara.buffer:buffer-delete buffer start bol)
        (goto (fixup-whitespace-at buffer start))))))

(defun join-report ()
  "A job's REPORT for joining lines in the background."
  (lambda (job status changes seconds kept)
    (declare (ignore job changes))
    (case status
      (:done (format nil "Joined the lines ~a" (job-done-words seconds kept)))
      (:none "No line to join"))))

(declaim (inline block-octet))
(defun block-octet (rope block block-start block-end p)
  "The octet at offset P of ROPE: from BLOCK, the octets from BLOCK-START below
BLOCK-END, when it holds it."
  (if (and (>= p block-start) (< p block-end))
      (aref block (- p block-start))
      (tuatara.text:rope-octet rope p)))

(defun joining-line (top bottom prefixes)
  "A function changing each line for START-LINE-JOB, so that the lines from the
one starting at TOP to the one starting at BOTTOM become one, as
JOIN-LINES-ABOVE joins them: every line but the first loses the blanks it
begins with, every line but the last the blanks it ends with and its line end,
blank lines go, and between two lines' texts goes a space unless
FIXUP-WHITESPACE-AT would leave none (PREFIXES are the quote marks of Lisp)."
  (let ((space (tuatara.text:string-octets " "))
        (none (make-array 0 :element-type '(unsigned-byte 8)))
        (bound nil))
    (lambda (function line line-end next rope block block-start block-end)
      (flet ((octet (p) (block-octet rope block block-start block-end p))
             (blank-p (b) (or (= b 32) (= b 9))))
        (let* ((text (loop for p from line below line-end unless (blank-p (octet p)) return p finally (return line-end)))
               (last (= line bottom)))
          (if (= text line-end)
              ;; a blank line goes, the last one's line end kept
              (let ((to (if last line-end next)))
                (when (< line to) (funcall function line to nil none)))
              (let ((end (loop for p from (1- line-end) downto text unless (blank-p (octet p)) return (1+ p))))
                (when (and (/= line top) (< line text))
                  (funcall function line text nil none))
                (unless last
                  (let* ((bound (or bound (setf bound (tuatara.text:rope-line-end rope bottom))))
                         (after (loop for p from next below bound
                                      for b = (octet p)
                                      unless (member b tuatara.text:*blank-octets*) return b))
                         (before (octet (1- end))))
                    (funcall function end next nil
                             (if (and after
                                      (not (member after tuatara.text:*closing-octets*))
                                      (not (member before tuatara.text:*opening-octets*))
                                      (not (find (code-char before) prefixes)))
                                 space
                                 none)))))))))))

(defun join-lines-in-background (buffer start last)
  "Join the lines of BUFFER from the one holding START to the one holding LAST
into one, as JOIN-LINES-ABOVE joins them, in the background on all but two
cores, a line at a time (JOINING-LINE); one change for undo."
  (check-writable buffer)
  (let ((top (tuatara.buffer:buffer-line-start buffer start))
        (bottom (tuatara.buffer:buffer-line-start buffer last)))
    (start-line-job buffer top (tuatara.buffer:buffer-line-end buffer bottom)
                    (joining-line top bottom (if (lisp-prefix-char-p buffer #\') "'`,#" ""))
                    :doing "Joining" :what "Joining the lines" :report (join-report)
                    :verb "join the lines" :leaves "the lines are as they were"
                    ;; point at the first join, after the first line's text, as joining here leaves it
                    :goal (lambda (from)
                            (let ((end (loop with p = (tuatara.buffer:buffer-line-end buffer from)
                                             while (and (> p from) (member (tuatara.buffer:buffer-char-at buffer (1- p)) '(#\Space #\Tab)))
                                             do (decf p)
                                             finally (return p))))
                              (lambda (b) (declare (ignore b)) end)))
                    :announce "Joining the lines")))

(defun join-following-line (buffer)
  "Join the line after point's line to it, as \\[delete-indentation] does with
\\[universal-argument]."
  (let ((bound (1- (next-line-start buffer (point)))))
    (goto (next-line-start buffer (point)))
    (join-lines-above buffer bound)))

(define-command delete-indentation ()
  "Join this line to the one before it: the line end between them goes, and
the spaces about it become one space, or none where a line begins or ends,
before a closing bracket or after an opening one. With
\\[universal-argument], join the next line to this one; with a selection, join
every line it takes in, in the background when it holds more than
*REWRITE-THRESHOLD* octets."
  (let* ((b (current-buffer))
         (arg (prefix-argument))
         (region (and (not arg) (tuatara.buffer:buffer-mark-active-p b))))
    (cond (region
           (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
             (setf (tuatara.buffer:buffer-mark-active-p b) nil)
             (if (large-for-here-p start end)
                 (join-lines-in-background b start end)
                 (progn (goto (if (> end (tuatara.buffer:buffer-line-end b start)) end start))
                        (join-lines-above b start)))))
          (arg (join-following-line b))
          (t (join-lines-above b (1- (tuatara.buffer:buffer-line-start b (point))))))))

(define-command join-line ()
  "Join this line to the one before it: \\[delete-indentation] by another name."
  (delete-indentation))

;;; Opening and going to lines

(define-command open-line ()
  "Insert a line end after point, point staying before it; with a count, that
many line ends. The new line is not indented."
  (let ((n (insert-count)) (b (current-buffer)) (p (point)))
    (tuatara.buffer:buffer-insert b p (repeated-text (tuatara.buffer:buffer-newline b) n))
    (goto p)))

(define-command back-to-indentation ()
  "Move point to the first character of its line that is not a space or a tab."
  (let* ((b (current-buffer)) (line (tuatara.buffer:buffer-line-start b (point))))
    (goto (+ line (line-indentation b line)))))

(define-command delete-blank-lines ()
  "Delete the blank lines about point: on a blank line, all the blank lines
around it but one; on a blank line alone, that one; on a line with text, the
blank lines after it."
  (let* ((b (current-buffer))
         (size (tuatara.buffer:buffer-end b))
         (line (tuatara.buffer:buffer-line-start b (point)))
         (this-blank (blank-line-at-p b line))
         (next (next-line-start b line))
         (single (and this-blank
                      (not (and (< next size) (blank-line-at-p b next)))
                      (or (zerop line)
                          (not (blank-line-at-p b (tuatara.buffer:buffer-line-start b (1- line))))))))
    (flet ((text-at-or-before (p)
             ;; the start of the line after the last character before P that is no blank
             (let ((q (skip-blanks b p :backward t)))
               (if (zerop q) 0 (next-line-start b (1- q)))))
           (text-at-or-after (p)
             ;; the start of the line of the first character at or after P that is no blank
             (let ((q (skip-blanks b p)))
               (if (>= q (tuatara.buffer:buffer-end b))
                   (tuatara.buffer:buffer-end b)
                   (tuatara.buffer:buffer-line-start b q)))))
      ;; the blank lines before, and this one when it is alone
      (when this-blank
        (let* ((from (if single (next-line-start b line) line))
               (to (text-at-or-before line)))
          (when (< to from) (refuse-hidden-deletion b to from) (tuatara.buffer:buffer-delete b to from))
          (goto (min to from))))
      ;; the blank lines after, unless this is a blank line alone
      (unless (and this-blank single)
        (let* ((after (next-line-start b (point)))
               (to (text-at-or-after after)))
          (when (< after to) (refuse-hidden-deletion b after to) (tuatara.buffer:buffer-delete b after to))))
      ;; a blank line with nothing after it but its line end goes, point at the end
      (let ((p (point)))
        (when (and (= p (tuatara.buffer:buffer-line-start b p))
                   (blank-line-at-p b p)
                   (= (next-line-start b p) (tuatara.buffer:buffer-end b))
                   (< p (tuatara.buffer:buffer-end b)))
          (tuatara.buffer:buffer-delete b p (tuatara.buffer:buffer-end b)))))))

;;; Trailing whitespace

(defvar *delete-trailing-lines* t
  "True for \\[delete-trailing-whitespace] on the whole buffer to delete the
blank lines at its end too.")

(define-command delete-trailing-whitespace ()
  "Delete the spaces and tabs at the end of every line, of the selection when
there is one, else of the whole buffer (of the part shown of a narrowed one),
and then the blank lines at its end too (*DELETE-TRAILING-LINES*). A form
feed is not whitespace. A
large buffer is done in the background, as \\[replace-regexp] does it."
  (let* ((b (current-buffer))
         (region (tuatara.buffer:buffer-mark-active-p b))
         (origin (point)))
    (multiple-value-bind (low high) (if region (tuatara.buffer:buffer-region b) (tuatara.buffer:buffer-bounds b))
      (setf (tuatara.buffer:buffer-mark-active-p b) nil)
      ;; the blank lines at the end first: their text is gone before the lines are looked at
      (let ((blank-lines 0))
        (when (and (not region) *delete-trailing-lines*)
          (let* ((size (tuatara.buffer:buffer-end b)) (q (skip-blanks b size :backward t)))
            ;; keep the line end that ends the last line of text
            (let ((keep (next-line-start b q)))
              (when (and (< keep size) (< q size))
                (setf blank-lines (count #\Newline (tuatara.buffer:buffer-string b :start keep :end size)))
                (tuatara.buffer:buffer-delete b keep size)
                (setf high (min high keep))))))
        (let ((lines (let ((*last-search-pattern* *last-search-pattern*))
                       ;; the last search stays the one typed
                       (run-substitution b (make-search-line :pattern "[ \\t]+(\\r?)$" :replacement "\\1" :flags "g")
                                         t low (min high (tuatara.buffer:buffer-end b)) "trailing whitespace"
                                         (min origin (tuatara.buffer:buffer-end b)) :flags "g"))))
          ;; in the background the job says what it does; here, what was done
          (unless (rewrite-of b)
            (cond ((and (zerop lines) (zerop blank-lines)) (message "No trailing whitespace"))
                  ((zerop blank-lines) (message "Deleted trailing whitespace on ~:d line~:p" lines))
                  ((zerop lines) (message "Deleted ~:d blank line~:p at the end" blank-lines))
                  (t (message "Deleted trailing whitespace on ~:d line~:p, and ~:d blank line~:p at the end"
                              lines blank-lines)))))))))

;;; Transposing

(defun char-mover (buffer position count)
  "Where COUNT characters on from POSITION of BUFFER is, back when COUNT is
negative; past either end, an error: \"End of buffer\" or
\"Beginning of buffer\"."
  (let ((p position))
    (dotimes (i (abs count) p)
      (cond ((and (plusp count) (>= p (tuatara.buffer:buffer-size buffer))) (error "End of buffer"))
            ((and (minusp count) (<= p 0)) (error "Beginning of buffer")))
      (setf p (tuatara.buffer:buffer-forward-char buffer p (if (plusp count) 1 -1))))))

(defun word-mover (buffer position count)
  "Where COUNT words on from POSITION of BUFFER end, or where -COUNT words back
start, as \\[forward-word] and \\[backward-word] go (WORDS-FORWARD)."
  (if (minusp count)
      (words-backward buffer position (- count))
      (words-forward buffer position count)))

(defun line-mover (buffer position count)
  "Where the start of the line COUNT lines on from POSITION's is, back when
COUNT is negative, for \\[transpose-lines]: going forward past the last line
makes the line ends it lacks; of a narrowed buffer, the lines of the part
shown alone."
  (if (plusp count)
      (let ((p position) (left count))
        (loop while (and (plusp left) (< (next-line-start buffer p) (tuatara.buffer:buffer-end buffer)))
              do (setf p (next-line-start buffer p)) (decf left))
        ;; at the last line: to its end, and a line end for each line still wanted
        (when (plusp left)
          (let ((size (tuatara.buffer:buffer-end buffer)))
            (setf p size)
            (unless (and (plusp size) (eql (tuatara.buffer:buffer-char-at buffer (1- size)) #\Newline))
              (tuatara.buffer:buffer-insert buffer size (tuatara.buffer:buffer-newline buffer))
              (setf p (tuatara.buffer:buffer-end buffer))
              (decf left))
            (when (plusp left)
              (tuatara.buffer:buffer-insert buffer p (repeated-text (tuatara.buffer:buffer-newline buffer) left))
              (setf p (tuatara.buffer:buffer-end buffer)))))
        p)
      (let ((p (tuatara.buffer:buffer-line-start buffer position)))
        (dotimes (i (- count) p)
          (when (<= p (tuatara.buffer:buffer-start buffer)) (return p))
          (setf p (tuatara.buffer:buffer-line-start buffer (1- p)))))))

(defun mover-aux (mover)
  "The objects MOVER moves over, as TRANSPOSE-OBJECTS takes them: a function
of a buffer, a position and a count, giving the two ends of the object COUNT
on (the far end first) and where it leaves point."
  (lambda (buffer position count)
    (let* ((far (funcall mover buffer position count))
           (near (funcall mover buffer far (- count))))
      (values (cons far near) near))))

(defun swapped-marker (position a1 l1 a2 l2)
  "Where a marker at POSITION ends when the text of length L1 at A1 and the
text of length L2 at A2 (after it) change places by four edits: an insertion
before the markers at A1's end, the first text deleted, an insertion at the
second's place, and the second text deleted."
  (let ((p position) (b a2))
    ;; the second text is put after the first, before markers there
    (when (>= p (+ a1 l1)) (incf p l2))
    (incf b l2)
    ;; the first text goes
    (cond ((> p (+ a1 l1)) (decf p l1)) ((> p a1) (setf p a1)))
    (decf b l1)
    ;; the first text is put at the second's place
    (when (> p b) (incf p l1))
    ;; and the second text goes from there
    (let ((from (+ b l1)))
      (cond ((> p (+ from l2)) (decf p l2)) ((> p from) (setf p from))))
    p))

(defun transpose-objects (buffer aux arg)
  "Interchange objects of BUFFER, AUX giving an object's ends (see MOVER-AUX):
with ARG positive, the object before point goes past ARG objects after it,
point after it; negative, back past -ARG objects; zero, the objects at point
and at the mark change places."
  (flet ((swap (pos1 pos2)
           ;; the two objects, each (START . END) either way round, change places; their starts
           (let ((pos1 (if (> (car pos1) (cdr pos1)) (cons (cdr pos1) (car pos1)) pos1))
                 (pos2 (if (> (car pos2) (cdr pos2)) (cons (cdr pos2) (car pos2)) pos2)))
             (when (> (car pos1) (car pos2)) (rotatef pos1 pos2))
             (when (> (cdr pos1) (car pos2)) (error "Don't have two things to transpose"))
             (if (> (- (cdr pos2) (car pos1)) tuatara.buffer:*large-edit-octets*)
                 ;; far apart or large: the three texts moved as they are, nothing copied
                 (let ((second (tuatara.buffer:buffer-slice buffer (car pos2) (cdr pos2))))
                   (tuatara.buffer:buffer-splice
                    buffer (car pos1) (cdr pos2)
                    (tuatara.text:splice-rope second (tuatara.text:rope-bytes second) (tuatara.text:rope-bytes second)
                                              (tuatara.buffer:buffer-slice buffer (cdr pos1) (car pos2))
                                              (tuatara.buffer:buffer-slice buffer (car pos1) (cdr pos1)))))
                 (let ((first (tuatara.buffer:buffer-string buffer :start (car pos1) :end (cdr pos1)))
                       (between (tuatara.buffer:buffer-string buffer :start (cdr pos1) :end (car pos2)))
                       (second (tuatara.buffer:buffer-string buffer :start (car pos2) :end (cdr pos2))))
                   (tuatara.buffer:buffer-replace buffer (car pos1) (cdr pos2) (concatenate 'string second between first))))
             (values (car pos1) (- (cdr pos1) (car pos1)) (car pos2) (- (cdr pos2) (car pos2))))))
    (cond ((zerop arg)
           (let ((mark (tuatara.buffer:buffer-mark buffer)) (point (point)))
             (unless mark (error "No mark set in this buffer"))
             (let ((pos1 (funcall aux buffer point 1))
                   (pos2 (funcall aux buffer mark 1)))
               (multiple-value-bind (a1 l1 a2 l2) (swap pos1 pos2)
                 ;; point and the mark follow the edits, then change places
                 (setf (tuatara.buffer:buffer-mark buffer) (swapped-marker point a1 l1 a2 l2))
                 (goto (swapped-marker mark a1 l1 a2 l2))))))
          ((plusp arg)
           (multiple-value-bind (pos1 p) (funcall aux buffer (point) -1)
             (let ((pos2 (funcall aux buffer p arg)))
               (swap pos1 pos2)
               (goto (car pos2)))))
          (t
           (let* ((pos1 (funcall aux buffer (point) -1))
                  (pos2 (funcall aux buffer (car pos1) arg)))
             (swap pos1 pos2)
             (goto (+ (car pos2) (- (cdr pos1) (car pos1)))))))))

(define-command transpose-chars ()
  "Interchange the characters about point, moving on past them; at the end of
a line, the two before point. With a count N, carry the character before point
forward past N characters (back when N is negative); with 0, interchange the
characters at point and at the mark."
  (let ((b (current-buffer)) (arg (prefix-argument)))
    (when (and (null arg) (plusp (point)) (= (point) (tuatara.buffer:buffer-line-end b (point))))
      (goto (tuatara.buffer:buffer-forward-char b (point) -1)))
    (transpose-objects b (mover-aux #'char-mover) (prefix))))

(define-command transpose-words ()
  "Interchange the words about point, point after them; with a count N, carry
the word before or around point past N words (back when N is negative); with
0, interchange the words at point and at the mark."
  (transpose-objects (current-buffer) (mover-aux #'word-mover) (prefix)))

(define-command transpose-lines ()
  "Interchange this line and the one before it, point after them; with a count
N, carry the line before point past N lines (back when N is negative); with 0,
interchange the lines of point and the mark."
  (transpose-objects (current-buffer) (mover-aux #'line-mover) (prefix)))

(defun sexp-symbol-char-p (buffer char)
  "True for a character of a symbol or a word, as balanced expressions take
them in BUFFER's language."
  (and char (not (sexp-boundary-p char)) (not (find char "'`,\"#;"))
       (or (lisp-prefix-char-p buffer #\') (not (find char ".,:!?")))))

(defun sexp-aux (buffer position count)
  "The balanced expression COUNT on from POSITION of BUFFER, as
\\[transpose-sexps] takes it: from inside or beside a symbol, the whole
symbol; between expressions, the one after the blanks and comments."
  (flet ((at (p) (tuatara.buffer:buffer-char-at buffer p))
         (motion (p n)
           (or (sexp-motion buffer p n) (error "Not between two complete sexps"))))
    (if (if (plusp count)
            (sexp-symbol-char-p buffer (at position))
            (and (plusp position) (sexp-symbol-char-p buffer (at (1- position)))))
        ;; over a symbol, from its start going forward or its end going back
        (let ((p position))
          (if (plusp count)
              (loop while (and (plusp p) (sexp-symbol-char-p buffer (at (1- p)))) do (decf p))
              (loop while (sexp-symbol-char-p buffer (at p)) do (incf p)))
          (values (cons (motion p count) p) p))
        ;; between expressions: back over blanks before going on, so both ways see the same ones
        (let ((p (skip-blanks buffer position :backward (plusp count))))
          (let ((far (motion p count))
                (near p))
            ;; to the expression's own edge, past blanks and comments
            (if (plusp count)
                (loop (setf near (skip-blanks buffer near))
                      (if (eql (at near) #\;)
                          (setf near (next-line-start buffer near))
                          (return)))
                (setf near (skip-blanks buffer near :backward t)))
            (values (cons far near) near))))))

(define-command transpose-sexps ()
  "Interchange the balanced expressions about point, point after them; with a
count N, carry the expression before point past N expressions (back when N is
negative); with 0, interchange the expressions at point and at the mark."
  (transpose-objects (current-buffer) #'sexp-aux (prefix)))

;;; Killing lines and zapping

(define-command kill-whole-line ()
  "Kill the current line and its line end; with a count N, N lines from this
one; with a negative count, -N lines back, with the line end before them
rather than after; with 0, the line without its line end. After another kill,
the text before point goes at the front of that kill and the rest after it, so
that the kills read in order. Lines that would take part of the text a fold
hides, but not all of it, are not killed: the fold is shown instead. In a
narrowed buffer, the lines of the part shown."
  (let* ((b (current-buffer)) (n (prefix)) (p (point))
         (narrowed (tuatara.buffer:buffer-narrowed-p b))
         (low (tuatara.buffer:buffer-start b))
         (size (tuatara.buffer:buffer-end b))
         (bol (tuatara.buffer:within-narrowing b (tuatara.buffer:buffer-line-start b p)))
         (eol (tuatara.buffer:within-narrowing b (tuatara.buffer:buffer-line-end b p))))
    (when (and (plusp n) (= p size) (= bol size)) (error (if narrowed "End of the part shown" "End of buffer")))
    (when (and (minusp n) (= p low) (= eol low)) (error (if narrowed "Beginning of the part shown" "Beginning of buffer")))
    (let* ((other (cond ((zerop n) eol)
                        ((minusp n)
                         ;; the start of the line -N-1 back, then back over the line end before it
                         (let ((q bol))
                           (dotimes (i (- (1+ n))) (unless (<= q low) (setf q (tuatara.buffer:within-narrowing b (tuatara.buffer:buffer-line-start b (1- q))))))
                           (cond ((<= q low) low)
                                 ((and (> q (1+ low)) (eql (tuatara.buffer:buffer-char-at b (- q 2)) #\Return)) (- q 2))
                                 (t (1- q)))))
                        (t (let ((q p)) (dotimes (i n q) (setf q (min size (next-line-start b q))))))))
           (first-end (if (minusp n) eol bol))
           (start (min p other first-end)) (end (max p other first-end))
           (appending (kill-command-p (editor-last-command *editor*))))
      (refuse-hidden-deletion b start end)
      (if appending
          (progn (when (< start p) (kill-text (region-text b start p) :append t :prepend t))
                 (when (< p end) (kill-text (region-text b p end) :append t)))
          (kill-text (region-text b start end) :append nil))
      (tuatara.buffer:buffer-delete b start end)
      (goto start))))

(defun read-typed-char (prompt &optional (for "to use here"))
  "A character typed at PROMPT: a key without Control or Meta, RET for a line
end and TAB for a tab; \\`C-g' (and Escape when it quits) gives up; any other
key is said to be no character FOR that."
  (let ((key (read-key prompt)))
    (let ((base (tuatara.frontend:key-base key)))
      (cond ((quit-key-p key) (keyboard-quit-signal))
            ((and (characterp base) (not (tuatara.frontend:key-ctrl key)) (not (tuatara.frontend:key-meta key))) base)
            ((eq base :enter) #\Newline)
            ((eq base :tab) #\Tab)
            (t (error "~a is no character ~a" (tuatara.frontend:key-description (tuatara.frontend:key-spec key)) for))))))

(defun find-char-occurrence (buffer position char count fold)
  "Where the COUNTth CHAR after POSITION of BUFFER ends, or where the -COUNTth
before it begins when COUNT is negative, in either case when FOLD; NIL when
there is none, :FAR when none was found within *TEXT-SCAN-LIMIT* octets."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (chars (if (and fold (both-case-p char)) (list (char-downcase char) (char-upcase char)) (list char)))
         (codes (mapcar (lambda (c) (tuatara.text:string-octets (string c))) chars))
         (firsts (remove-duplicates (mapcar (lambda (o) (aref o 0)) codes)))
         (size (tuatara.text:rope-bytes rope))
         (p position) (left (abs count)))
    (flet ((match-length (q)
             (loop for code in codes
                   when (and (<= (+ q (length code)) size)
                             (loop for i below (length code) always (= (tuatara.text:rope-octet rope (+ q i)) (aref code i))))
                     return (length code))))
      (if (plusp count)
          (let ((limit (min size (+ position *text-scan-limit*))))
            (loop (let ((q (tuatara.text:rope-find-any-octet rope firsts :start p :end limit)))
                    (unless q (return (and (< limit size) :far)))
                    (let ((length (match-length q)))
                      (setf p (+ q (or length 1)))
                      (when (and length (zerop (decf left))) (return p))))))
          (let ((limit (max 0 (- position *text-scan-limit*))))
            (loop (let ((q (tuatara.text:rope-find-any-octet rope firsts :start limit :end p :from-end t)))
                    (unless q (return (and (plusp limit) :far)))
                    (setf p q)
                    (when (and (match-length q) (zerop (decf left))) (return q)))))))))

(defun zap (char count &key up-to)
  "Kill from point to the COUNTth CHAR on (back when COUNT is negative), it
included or, UP-TO, not: an upper-case CHAR is looked for in that case alone,
any other in either case."
  (unless (zerop count)
    (let* ((b (current-buffer)) (p (point)) (forward (plusp count))
           (fold (not (upper-case-p char)))
           ;; up to the character: it is not looked for at point itself
           (from (if up-to
                     (if forward
                         (min (tuatara.buffer:buffer-size b) (tuatara.buffer:buffer-forward-char b p))
                         (max 0 (tuatara.buffer:buffer-forward-char b p -1)))
                     p))
           (found (find-char-occurrence b from char count fold)))
      (cond ((eq found :far)
             (error "~s not found within ~d MB" (string char) (floor *text-scan-limit* (* 1024 1024))))
            ((null found) (error "Search failed: ~s" (string char))))
      (let ((end (if up-to
                     (if forward
                         (tuatara.buffer:buffer-forward-char b found -1)
                         (tuatara.buffer:buffer-forward-char b found))
                     found)))
        (let ((start (min p end)) (stop (max p end)))
          (when (< start stop) (kill-span b start stop :prepend (< end p)))
          (goto start))))))

(define-command zap-to-char ()
  "Kill up to and including the next character typed after it; with a count N,
to the Nth, back when N is negative. An upper-case character is looked for in
that case only, any other in either case."
  (let ((n (prefix)))
    (zap (read-typed-char "Zap to char: " "to zap to") n)))

(define-command zap-up-to-char ()
  "Kill up to, but not including, the next character typed after it; with a
count N, to the Nth, back when N is negative. An upper-case character is
looked for in that case only."
  (let ((n (prefix)))
    (zap (read-typed-char "Zap up to char: " "to zap to") n :up-to t)))

;;; Counting words

(defvar *count-wait* 1/2
  "Seconds \\[count-words] waits for its count before letting it go on in the background.")

(defvar *count-piece* (* 4 1024 1024)
  "Octets counted at a time: the step of the progress shown and of stopping.")

(defparameter *count-carried-settings* '(*count-piece*)
  "The settings a count reads as it works, carried to the thread it works on as
they are on the editor's when it starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

(defstruct (count-job (:include tuatara.runtime:job) (:constructor %make-count-job) (:copier nil))
  "A count for BUFFER, a job (TUATARA.RUNTIME:JOB), of the text WHAT names
\(\"Region\", \"Buffer\"): TOTAL octets, the car of DONE counted so far;
RESULT, once done, a list of lines, sentences, words and characters; LEFT-OUT
the lines search filters hide, not counted."
  what (total 0) (done (list 0)) result (left-out 0))

(setf (documentation 'count-job-p 'function) "True when OBJECT is a COUNT-JOB.")

(tuatara.runtime:define-job-words count-job
  :at-exit :quiet
  :doing "Counting"
  :description (format nil "Counting the words of the ~(~a~)" (count-job-what job))
  :verb "count the words"
  :left-words "nothing counted")

(defun count-text (rope start end &key done cancel (parts (list (cons start end))) double-space)
  "Four values: the lines, sentences, words and characters of ROPE's octets
from START below END: lines as count-lines (a last line without a line end
counts), words as runs of letters and digits, a sentence ending where
sentence motion ends it (SENTENCE-GAP: at . ? ! … and the closing marks after
them followed by a blank or a line end, two blanks where sentences are two
spaces apart, DOUBLE-SPACE), not at the full stop of an abbreviation
\(ABBREVIATION-ENDING-P), at a blank line, or at the end; an octet that is no
part of a character in UTF-8 counts as one character. PARTS,
a list of (FROM . TO) in order, gives the stretches of it counted, one after
another as one text: all of it unless given. DONE, a cons, has its car set to
the octets counted as the count goes; CANCEL, a cons, stops it (NIL) when its
car turns true."
  (let* ((lines 0) (sentences 0) (words 0) (chars 0)
         (in-word nil) (in-sentence nil) (last-octet nil)
         ;; after a sentence's end: :end (its closing marks may follow), :blank (one blank seen)
         (ending nil)
         ;; since the last line end, only blanks: a blank line ends a sentence
         (line-blank t)
         ;; the octets read, three more for those of a character a piece cut, carried to the next
         (piece (make-array (+ 3 (min *count-piece* (max 1 (- end start)))) :element-type '(unsigned-byte 8)))
         (carried 0) (counted 0)
         (left (loop for (from . to) in parts sum (max 0 (- to from))))
         ;; the characters last seen, in a ring, to find an abbreviation's full stop
         (ring (make-string (max 1 (nth-value 1 (abbreviations-read-back)))))
         (char-at (lambda (i)
                    (declare (type fixnum i))
                    (and (>= i 0) (>= i (- chars (length ring))) (schar ring (mod i (length ring)))))))
    (declare (type fixnum lines sentences words chars carried counted left))
    (labels ((sentence-over () (when in-sentence (incf sentences)) (setf in-sentence nil ending nil))
             (see (char)
               (setf (schar ring (mod chars (length ring))) char)
               (incf chars)
               (let ((word (word-char-p char)))
                 (when (and word (not in-word)) (incf words))
                 (setf in-word word))
               (let ((blank (member char '(#\Space #\Tab #\Newline #\Return))))
                 ;; after a sentence's end, what follows says whether it ends there
                 (case ending
                   (:end (unless (sentence-closing-char-p char)
                           (let ((gap (sentence-gap char double-space)))
                             (case gap
                               ((t) (sentence-over))
                               (:blank (setf ending :blank))
                               (t (setf ending nil))))))
                   (:blank (if blank (sentence-over) (setf ending nil))))
                 (cond ((char= char #\Newline)
                        (when line-blank (sentence-over))
                        (setf line-blank t))
                       (blank)
                       (t
                        (setf line-blank nil in-sentence t)
                        (when (and (null ending) (sentence-end-char-p char)
                                   (not (and (char= char #\.) (abbreviation-ends-p char-at chars))))
                          (setf ending :end)))))))
      (loop for (from . to) in parts
            do (loop with at = from
                     while (< at to)
                     do (when (and cancel (car cancel)) (return-from count-text nil))
                        (let* ((n (min (- (length piece) carried) (- to at)))
                               (filled (+ carried n))
                               (i 0))
                          (declare (type fixnum n filled i))
                          (tuatara.text:rope-copy-into rope at (+ at n) piece carried)
                          (loop for k from carried below filled
                                do (let ((octet (aref piece k)))
                                     (setf last-octet octet)
                                     (when (= octet 10) (incf lines))))
                          ;; a character begun at the end may go on in the next piece
                          (loop with stop of-type fixnum = (if (= left n) filled (- filled (tuatara.text:octets-pending piece 0 filled)))
                                while (< i stop)
                                do (if (< (aref piece i) #x80)
                                       (progn (see (code-char (aref piece i))) (incf i))
                                       (multiple-value-bind (code length) (tuatara.text:octets-decode piece i filled)
                                         (see (code-char code))
                                         (incf i length))))
                          (replace piece piece :start2 i :end2 filled)
                          (setf carried (- filled i))
                          (decf left n)
                          (incf at n) (incf counted n)
                          (when done (setf (car done) counted)))))
      ;; the end of the text ends the sentence under way
      (sentence-over)
      (when (and (< start end) last-octet (/= last-octet 10)) (incf lines))
      (values lines sentences words chars))))

(defun count-progress (job)
  "How much of JOB is counted, from 0 to 99 while it runs."
  (tuatara.runtime:running-percent (car (count-job-done job)) (count-job-total job)))

(defmethod tuatara.runtime:job-percent ((job count-job))
  "How much of JOB is counted, from 0 to 99 while it runs (COUNT-PROGRESS)."
  (count-progress job))

(defmethod tuatara.runtime:job-take-in ((job count-job))
  "Say what JOB, which has ended, counted, or why it did not; nothing once its
buffer was killed."
  (unless (killed-job-buffer-p job)
    (ecase (count-job-status job)
      (:done (destructuring-bind (lines sentences words chars) (count-job-result job)
               (message "~a has ~:d line~:p, ~:d sentence~:p, ~:d word~:p, and ~:d character~:p~a"
                        (count-job-what job) lines sentences words chars (left-out-note (count-job-left-out job)))))
      ((:stopped :failed) (message "~a" (tuatara.runtime:job-ending-words job))))))

(define-command count-words ()
  "Say how many lines, sentences, words and characters the selection has, or
the whole buffer without one (the part shown of a narrowed one): \"Region
has 2 lines, 2 sentences, 9 words, and 48 characters\". A sentence ends where \\[forward-sentence] finds its end: at
. ? ! or … and a space or a line end after it (two spaces, a tab or a line
end with *SENTENCE-END-DOUBLE-SPACE*), or at a blank line. The lines search filters hide
\(\\[search-filter-regex]) are not counted. A large text is counted in the background, its
progress in the mode line; \\[stop-job] stops it. While another job runs in
the buffer it waits its turn (\\[list-jobs])."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end what)
        (if (tuatara.buffer:buffer-mark-active-p b)
            (multiple-value-bind (s e) (tuatara.buffer:buffer-region b) (values s e "Region"))
            (multiple-value-call #'values (tuatara.buffer:buffer-bounds b)
              (if (tuatara.buffer:buffer-narrowed-p b) "The part shown" "Buffer")))
      (unless (queue-if-busy b (format nil "Counting the words of the ~(~a~)" what)
                             (list (cons start :before) (cons end :after))
                             (lambda (start end) (start-word-count b start end what)))
        (start-word-count b start end what)))))

(defun start-word-count (b start end what)
  "Count the lines, sentences, words and characters of B from START to END,
WHAT they are (Region, Buffer), as \\[count-words] does: the lines search
filters hide left out (TUATARA.DISPLAY:SHOWN-PARTS), in the background when
it takes time."
  (let* ((rope (tuatara.buffer:buffer-text b))
         (parts (tuatara.display:shown-parts b start end))
         (job (%make-count-job :buffer b :what what :total (loop for (s . e) in parts sum (- e s))
                               :left-out (if (equal parts (list (cons start end)))
                                             0
                                             (tuatara.display:lines-left-out b start end parts))))
         (done (count-job-done job)) (cancel (count-job-cancel job)))
    (tuatara.runtime:start-job-thread
     job (tuatara.runtime:carry-settings
          *count-carried-settings*
          (lambda (job)
            (let ((result (multiple-value-list (count-text rope start end :parts parts :done done :cancel cancel
                                                                          :double-space (sentence-end-double-space-p b)))))
              (if (first result)
                  (setf (count-job-result job) result (count-job-status job) :done)
                  (setf (count-job-status job) :stopped)))))
     :name "tuatara count" :details (format nil "octets ~:d to ~:d" start end))
    (take-in-soon job *count-wait* (format nil "Counting the ~(~a~)" what))))

(define-command count-words-region ()
  "Count the selection's lines, sentences, words and characters, or the whole
buffer's without one: \\[count-words] by another name."
  (count-words))
