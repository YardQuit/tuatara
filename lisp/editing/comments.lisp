;;;; lisp/editing/comments.lisp --- Lines commented and uncommented, and comments at the ends of lines.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; comment-line (C-x C-;) comments the lines of the region, or the line
;;;; point is on and then goes to the next, or uncomments them when they all
;;;; are, with the language's marker at their smallest indentation, as
;;;; Emacs's comment-line. comment-dwim (M-;) does that with a region; on a
;;;; line with a comment it goes into it, on a blank line it begins one, and
;;;; after code it puts one at the line's end, at *comment-column*.

(in-package #:tuatara.defaults)

(defvar *comment-column* 40
  "The column \\[comment-dwim] puts a comment at the end of a line of code at.")


(defun line-bounds (buffer start end)
  "The starts of the lines from the one holding START to the one ending the
range START below END (a range ending at a line's start leaves that line
out), and the offset after the last one's text, as two values."
  (let* ((first (tuatara.buffer:buffer-line-start buffer start))
         (last (tuatara.buffer:buffer-line-start buffer (range-last-place buffer start end))))
    (values (loop for line = first then (1+ (tuatara.buffer:buffer-line-end buffer line))
                  while (<= line last)
                  collect line
                  until (>= (tuatara.buffer:buffer-line-end buffer line) (tuatara.buffer:buffer-size buffer)))
            (tuatara.buffer:buffer-line-end buffer last))))

(defun lines-with-text (buffer start end)
  "The lines of BUFFER from the one holding START to the one ending the range
below END that are not blank, each (LINE . INDENT): the octet the line starts
at and the octets of blanks it begins with."
  (let ((texts '()))
    (dolist (line (line-bounds buffer start end) (nreverse texts))
      (multiple-value-bind (indent blank) (line-indentation buffer line)
        (unless blank (push (cons line indent) texts))))))

(defun block-comment-lines (buffer start end opener closer)
  "Comment the whole lines of BUFFER from the one holding START to the one
ending the range below END each between OPENER and CLOSER (<!-- and --> in
HTML), a space inside each, or uncomment them when every line with text is so
commented; the openers line up at the smallest indentation among them, blank
lines are left alone. One undoable edit. Returns :COMMENTED or :UNCOMMENTED,
NIL when there is no text."
  (let ((texts (lines-with-text buffer start end))
        (rope (tuatara.buffer:buffer-text buffer)))
    (flet ((text-of (entry)
             ;; the line's text from its indentation, trailing blanks left out
             (string-right-trim '(#\Space #\Tab #\Return)
                                (tuatara.text:rope-string rope :start (+ (car entry) (cdr entry))
                                                               :end (tuatara.buffer:buffer-line-end buffer (car entry)))))
           (octets (string) (tuatara.text:string-octet-length string)))
      (when texts
        (if (every (lambda (entry)
                     (let ((text (text-of entry)))
                       (and (>= (length text) (+ (length opener) (length closer)))
                            (string= opener text :end2 (length opener))
                            (string= closer text :start2 (- (length text) (length closer))))))
                   texts)
            (let ((edits '()))
              (dolist (entry texts)
                (let* ((text (text-of entry))
                       (at (+ (car entry) (cdr entry)))
                       (inner-start (+ (length opener) (if (and (> (length text) (length opener))
                                                                (char= (char text (length opener)) #\Space))
                                                           1 0)))
                       (inner-end (- (length text) (length closer)
                                     (if (and (> (- (length text) (length closer)) inner-start)
                                              (char= (char text (- (length text) (length closer) 1)) #\Space))
                                         1 0))))
                  (push (tuatara.text:make-edit at (+ at (octets (subseq text 0 inner-start)))) edits)
                  (push (tuatara.text:make-edit (+ at (octets (subseq text 0 inner-end))) (+ at (octets text))) edits)))
              (tuatara.buffer:buffer-edit buffer (coerce (nreverse edits) 'simple-vector))
              :uncommented)
            (let ((column (reduce #'min texts :key #'cdr))
                  (before (tuatara.text:string-octets (concatenate 'string opener " ")))
                  (after (tuatara.text:string-octets (concatenate 'string " " closer)))
                  (edits '()))
              (dolist (entry texts)
                (let ((at (+ (car entry) column))
                      (stop (+ (car entry) (cdr entry) (octets (text-of entry)))))
                  (push (tuatara.text:make-edit at at before) edits)
                  (push (tuatara.text:make-edit stop stop after) edits)))
              (tuatara.buffer:buffer-edit buffer (coerce (nreverse edits) 'simple-vector))
              :commented))))))

(defun comment-line-facts (octets start end marker)
  "Two values for the line in OCTETS from START below END: how many octets of
blanks it begins with, or NIL when it has nothing else; and whether MARKER
(octets) follows them."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets marker) (type fixnum start end))
  (let ((i start))
    (declare (type fixnum i))
    (loop while (and (< i end) (let ((b (aref octets i))) (or (= b 32) (= b 9)))) do (incf i))
    (if (= i end)
        (values nil nil)
        (values (- i start)
                (and (<= (+ i (length marker)) end)
                     (loop for k of-type fixnum from 0 below (length marker)
                           always (= (aref marker k) (aref octets (+ i k)))))))))

(defun comment-marker-end (octets start end marker)
  "Where uncommenting a line takes the comment away to: from START, the index
in OCTETS of a line's comment marker, past the run of MARKER's characters (a
string) and one space after them, short of END."
  (let ((p start))
    (loop while (and (< p end) (find (code-char (aref octets p)) marker)) do (incf p))
    (when (and (< p end) (= 32 (aref octets p))) (incf p))
    p))

(defun comment-job-report (commenting)
  "A job's REPORT for commenting lines in the background, or uncommenting them unless COMMENTING."
  (lambda (job status changes seconds kept)
    (declare (ignore job))
    (case status
      (:done (format nil "~:[Uncommented~;Commented~] ~:d line~:p ~a"
                     commenting changes (job-done-words seconds kept)))
      (:none "No line with text to comment"))))

(defun start-comment-job (buffer from to marker insert)
  "Comment the lines of BUFFER from the one starting at FROM to the one ending
at TO with MARKER (INSERT and a space put in), or uncomment them when every
line with text is commented, as COMMENT-OR-UNCOMMENT-LINES does, in the
background on all but two cores: the lines are read once to learn which, and
their smallest indentation, then written. One change for undo. Returns the
job, or :QUEUED when another job runs or waits in BUFFER."
  (let* ((marker-octets (tuatara.text:string-octets marker))
         (insert-octets (tuatara.text:string-octets (concatenate 'string insert " ")))
         (none (make-array 0 :element-type '(unsigned-byte 8)))
         (crlf (buffer-crlf-p buffer))
         (column nil) (commenting nil))
    (flet ((prepare (job)
             (declare (notinline rewrite-pieces rewrite-rope rewrite-piece-start rewrite-piece-end rewrite-cancel))
             ;; every piece's lines on the pool: their smallest indentation, and whether all are commented
             (let* ((pieces (rewrite-pieces job))
                    (found (make-array (length pieces))))
               (tuatara.runtime:run-tasks
                (map 'simple-vector
                     (lambda (piece)
                       (let ((i (position piece pieces)))
                         (lambda ()
                           (let ((least nil) (all t))
                             (map-piece-lines
                              (lambda (line line-end rope block block-start block-end)
                                (multiple-value-bind (octets at) (line-octets rope line line-end block block-start block-end)
                                  (multiple-value-bind (indent marked)
                                      (comment-line-facts octets at (+ at (- line-end line)) marker-octets)
                                    (when indent
                                      (setf least (if least (min least indent) indent))
                                      (unless marked (setf all nil))))))
                              (rewrite-rope job) (rewrite-piece-start piece) (rewrite-piece-end piece) crlf
                              (lambda () (car (rewrite-cancel job))))
                             (setf (svref found i) (cons least all))))))
                     pieces)
                :parallelism (max 1 (- (tuatara.runtime:cpu-count) 2)))
               (setf column (loop for (least . nil) across found when least minimize least)
                     commenting (and column (notevery (lambda (entry) (or (null (car entry)) (cdr entry))) found))))))
      (start-line-job
       buffer from to
       (lambda (function line line-end next rope block block-start block-end)
         (declare (ignore next))
         (when column
           (multiple-value-bind (octets at) (line-octets rope line line-end block block-start block-end)
             (let ((end (+ at (- line-end line))))
               (multiple-value-bind (indent) (comment-line-facts octets at end marker-octets)
                 (when indent
                   (if commenting
                       (let ((p (+ line column))) (funcall function p p nil insert-octets))
                       (funcall function (+ line indent) (+ line (- (comment-marker-end octets (+ at indent) end marker) at))
                                nil none))))))))
       :doing "Commenting" :what "Commenting or uncommenting the lines"
       :verb "comment the lines" :leaves "the lines are as they were"
       :prepare #'prepare
       :report (lambda (job status changes seconds kept)
                 (funcall (comment-job-report commenting) job status changes seconds kept))
       :announce "Commenting or uncommenting the lines"))))

(defun comment-or-uncomment-lines (buffer start end)
  "Comment the whole lines of BUFFER from the one holding START to the one
ending the range below END, or uncomment them when every line with text is
commented: commenting puts the language's marker and a space at the smallest
indentation among them, lining the markers up, and leaves blank lines alone;
uncommenting takes away the run of marker characters and the space after it.
A language whose comments have an end (HTML's <!-- -->) has each line so
enclosed. One undoable edit. Returns :COMMENTED, :UNCOMMENTED, or NIL when the
language has no comments."
  (multiple-value-bind (marker insert) (mode-comment-syntax (buffer-major-mode buffer))
    (unless marker
      (multiple-value-bind (opener closer) (mode-block-comment-syntax (buffer-major-mode buffer))
        (return-from comment-or-uncomment-lines
          (and opener
               (if (too-large-here-p start end "Commenting")
                   :too-large
                   (block-comment-lines buffer start end opener closer))))))
    (when (and marker (large-for-here-p start end))
      (check-writable buffer)
      (let ((from (tuatara.buffer:buffer-line-start buffer start))
            (to (tuatara.buffer:buffer-line-end buffer (range-last-place buffer start end))))
        (return-from comment-or-uncomment-lines
          (if (eq :queued (start-comment-job buffer from to marker insert)) :queued :background))))
    (when marker
      (let* ((texts (lines-with-text buffer start end))
             (marker-octets (tuatara.text:string-octets marker)))
        (flet ((commented-p (entry)
                 (let ((at (+ (car entry) (cdr entry))))
                   (and (<= (+ at (length marker-octets)) (tuatara.buffer:buffer-size buffer))
                        (equalp marker-octets (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer)
                                                                        :start at :end (+ at (length marker-octets))))))))
          (when texts
            (if (every #'commented-p texts)
                (let ((edits '()))
                  (dolist (entry texts)
                    (let* ((at (+ (car entry) (cdr entry)))
                           (octets (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer)
                                                             :start at :end (tuatara.buffer:buffer-line-end buffer (car entry)))))
                      (push (tuatara.text:make-edit at (+ at (comment-marker-end octets 0 (length octets) marker))) edits)))
                  (tuatara.buffer:buffer-edit buffer (coerce (nreverse edits) 'simple-vector))
                  :uncommented)
                (let ((column (reduce #'min texts :key #'cdr))
                      (octets (tuatara.text:string-octets (concatenate 'string insert " "))))
                  (tuatara.buffer:buffer-edit
                   buffer (map 'simple-vector (lambda (entry)
                                                (let ((at (+ (car entry) column)))
                                                  (tuatara.text:make-edit at at octets)))
                               texts))
                  :commented))))))))

(define-command comment-line ()
  "Comment the lines the region touches, or the line point is on, then going
to the next line, or uncomment them when they are all commented
\(\\`C-x C-;'). It puts the language's marker (;; in Lisp, // in Rust, # in
shell scripts) at their smallest indentation or, where a comment has an end
\(/* */ in CSS), each line between the two. A count comments as many lines,
no further than the end of the buffer, or of the part shown when it is
narrowed."
  (let* ((b (current-buffer))
         (active (and (tuatara.buffer:buffer-mark-active-p b)
                      (multiple-value-bind (s e) (tuatara.buffer:buffer-region b) (< s e)))))
    (if active
        (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
          ;; the line the region ends on is one of them, though it ends at its start
          (unless (comment-or-uncomment-lines b start (tuatara.buffer:buffer-line-end b end))
            (message "No comment syntax is defined"))
          (setf (tuatara.buffer:buffer-mark-active-p b) nil))
        (let* ((n (max 1 (prefix)))
               (end (tuatara.buffer:buffer-end b))
               (start (tuatara.buffer:buffer-line-start b (point)))
               ;; as many lines as there are, of the part shown of a narrowed buffer
               (last (loop with p = start repeat (1- n)
                           do (let ((next (1+ (tuatara.buffer:buffer-line-end b p))))
                                (if (< next end) (setf p next) (return p)))
                           finally (return p))))
          ;; to the end of the last line, so that the line is one of them
          (if (comment-or-uncomment-lines b start (tuatara.buffer:buffer-line-end b last))
              ;; the end as the change leaves it
              (let* ((end (tuatara.buffer:buffer-end b))
                     (after (1+ (tuatara.buffer:buffer-line-end b (min end last)))))
                (goto (min after end)))
              (message "No comment syntax is defined"))))))

(defun end-of-line-comment-at (buffer line-start line-end marker)
  "Where the comment of BUFFER's line from LINE-START to LINE-END that MARKER
begins starts, or NIL: MARKER at the line's start or after white space."
  (let* ((line (tuatara.buffer:buffer-string buffer :start line-start :end line-end))
         (at (loop for i = (search marker line) then (search marker line :start2 (1+ i))
                   while i
                   when (or (zerop i) (member (char line (1- i)) '(#\Space #\Tab))) return i)))
    (and at (+ line-start (tuatara.text:string-octet-length line :end at)))))

(define-command comment-dwim ()
  "Comment what suits where point is (\\`M-;'): with a region, its lines, or
uncomment them when they all are (as \\[comment-line]); on a line with a
comment, go into it; on a blank line, begin a comment; after code, begin one at
the line's end, at *COMMENT-COLUMN*."
  (let* ((b (current-buffer))
         (mode (buffer-major-mode b)))
    (if (and (tuatara.buffer:buffer-mark-active-p b)
             (multiple-value-bind (s e) (tuatara.buffer:buffer-region b) (< s e)))
        (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
          (unless (comment-or-uncomment-lines b start end)
            (message "No comment syntax is defined"))
          (setf (tuatara.buffer:buffer-mark-active-p b) nil))
        (multiple-value-bind (marker insert) (mode-comment-syntax mode)
          (multiple-value-bind (opener closer) (if marker (values nil nil) (mode-block-comment-syntax mode))
            (let* ((p (point))
                   (line-start (tuatara.buffer:buffer-line-start b p))
                   (line-end (tuatara.buffer:buffer-line-end b p))
                   (line (tuatara.buffer:buffer-string b :start line-start :end line-end))
                   (start-marker (or marker opener))
                   (at (and start-marker (end-of-line-comment-at b line-start line-end start-marker))))
              (cond
                ((null start-marker) (message "No comment syntax is defined"))
                (at
                 ;; into the comment there, after its marker and a space
                 (let* ((rest (tuatara.buffer:buffer-string b :start at :end line-end))
                        (skip (length (string-right-trim " " (subseq rest 0 (or (position-if-not (lambda (c) (find c start-marker)) rest) (length rest)))))))
                   (goto (min line-end (+ at skip (if (and (< (+ at skip) line-end) (eql (char rest skip) #\Space)) 1 0))))))
                (t
                 (check-writable b)
                 (let* ((blank (string= (string-trim " 	" line) ""))
                        (begin (if blank (or insert opener) (or marker opener)))
                        (code-end (+ line-start (tuatara.text:string-octet-length (string-right-trim " 	" line))))
                        (column (tuatara.display:buffer-screen-column b code-end))
                        (gap (cond (blank "")
                                   ((< column *comment-column*)
                                    (let ((tabs (buffer-indent-tabs-p b)) (width (buffer-tab-width b)) (col column) (out '()))
                                      (when tabs
                                        (loop while (<= (next-tab-stop col width) *comment-column*)
                                              do (push #\Tab out) (setf col (next-tab-stop col width))))
                                      (loop while (< col *comment-column*) do (push #\Space out) (incf col))
                                      (coerce (nreverse out) 'string)))
                                   (t " ")))
                        (at (if blank (+ line-start (tuatara.text:string-octet-length line)) code-end))
                        (text (format nil "~a~a " gap begin))
                        (after (if closer (format nil " ~a" closer) "")))
                   (tuatara.buffer:buffer-replace b at line-end (concatenate 'string text after))
                   (goto (+ at (tuatara.text:string-octet-length text))))))))))))
