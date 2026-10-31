;;;; lisp/org/edit.lisp --- Org's structure edited: subtrees moved, promoted and demoted, TODO states, new headlines and items.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands that edit an Org document by its structure, as Org's do:
;;;; a subtree moved over its sibling (org-move-subtree-up and -down),
;;;; a headline promoted or demoted alone or with its subtree, the TODO
;;;; keyword cycled through the document's keywords (a task done noted with
;;;; CLOSED: and when, as org-log-done 'time does), a new headline after the
;;;; subtree (M-RET, as Emacs Org's org-insert-heading-respect-content) or a new item in a list.
;;;; Each is one step to undo.

(in-package #:tuatara.defaults)

;;; Where point is in the structure


(defun org-point-char (buffer)
  "BUFFER's point as a character of its parse."
  (org-byte->char buffer (tuatara.buffer:buffer-point buffer)))

(defun org-current-headline (tree position)
  "The innermost headline of TREE whose subtree holds the character POSITION
(the end of the text the last one's), or NIL: down the tree, never through
every headline."
  (let ((found nil))
    (labels ((holds-p (node)
               (and (<= (org-node-begin node) position)
                    (or (< position (org-node-end node))
                        (and (= position (org-node-end node)) (= position (length *org-current-text*))))))
             (walk (node)
               (when (eq (org-node-type node) 'headline) (setf found node))
               (let ((child (find-if (lambda (c) (and (eq (org-node-type c) 'headline) (holds-p c))) (org-node-children node))))
                 (when child (walk child)))))
      (walk tree))
    found))

(defmethod mode-section-bounds ((mode org-mode) buffer position)
  "The subtree of the headline POSITION of BUFFER is under, its headline
with it; NIL before the first headline."
  (declare (ignore mode))
  (multiple-value-bind (tree text) (org-tree-now buffer)
    (let* ((*org-current-text* (or text ""))
           (headline (and tree (org-current-headline tree (org-byte->char buffer position)))))
      (when headline
        (values (org-char->byte buffer (org-node-begin headline))
                (org-char->byte buffer (org-node-end headline)))))))

(defun org-siblings (headline tree)
  "The headlines beside HEADLINE, under the same parent (TREE's top for the first level), in order."
  (org-child-headlines (or (org-node-parent headline) tree)))

(defun org-line-ends-kept (buffer start end text)
  "TEXT, Org's text for BUFFER's octets START below END as an edit leaves it,
each newline ending a line, with the line ends BUFFER had there: a line of
TEXT that was a line there ends as it ended (a newline, a carriage return
and a newline, or a carriage return alone), however the lines moved; a line
changed ends as the line in its place ended, when there are as many lines as
before; a new one as BUFFER makes lines (BUFFER-NEWLINE), a carriage return
it came with before its newline left out. So no line end the
edit did not make changes, in a file of one kind of line end or of several."
  (let* ((old (tuatara.buffer:buffer-string buffer :start start :end end))
         (crlf (buffer-crlf-p buffer))
         ;; the lines there as Org reads them, each (TEXT . END): in a CRLF buffer a
         ;; carriage return before a newline belongs to the end, elsewhere to the text
         (old-lines (let ((lines '()) (from 0) (n (length old)))
                      (loop for i from 0 below n
                            do (let ((c (char old i)))
                                 (cond ((char= c #\Newline)
                                        (if (and crlf (> i from) (char= (char old (1- i)) #\Return))
                                            (push (cons (subseq old from (1- i)) (coerce '(#\Return #\Newline) 'string)) lines)
                                            (push (cons (subseq old from i) (string #\Newline)) lines))
                                        (setf from (1+ i)))
                                       ((and (char= c #\Return) (not (and (< (1+ i) n) (char= (char old (1+ i)) #\Newline))))
                                        (push (cons (subseq old from i) (string #\Return)) lines)
                                        (setf from (1+ i))))))
                      (nreverse lines)))
         ;; an empty TEXT is one empty line
         (new-lines (or (uiop:split-string text :separator '(#\Newline)) (list "")))
         (ended (butlast new-lines))
         (same-count (= (length ended) (length old-lines)))
         (by-text (make-hash-table :test 'equal))
         (newline (tuatara.buffer:buffer-newline buffer)))
    (loop for (line . ending) in (reverse old-lines) do (push ending (gethash line by-text)))
    ;; lines found again first, each taking an end of its own text in order
    (let ((endings (loop for line in ended
                         collect (let ((queue (gethash line by-text)))
                                   (when queue
                                     (setf (gethash line by-text) (rest queue))
                                     (first queue))))))
      (with-output-to-string (out)
        (loop for line in ended
              for ending in endings
              for (nil . there) in (if same-count old-lines (make-list (length ended)))
              do (if (or ending there)
                     (progn (write-string line out) (write-string (or ending there) out))
                     ;; a new line ends as the buffer's do, a carriage return it came with left out
                     (progn (write-string (string-right-trim '(#\Return) line) out) (write-string newline out))))
        (write-string (car (last new-lines)) out)))))

(defun org-replace-chars (buffer from to text &key point)
  "Put TEXT in place of BUFFER's characters FROM below TO (of its parse), one
step to undo, and point at the character POINT (of the text after) when given.
Each line keeps the line end it had (ORG-LINE-ENDS-KEPT), a new one taking
the buffer's own."
  (let* ((returns (org-returns-of buffer))
         (start (org-char->byte buffer from returns))
         (end (org-char->byte buffer to returns)))
    (tuatara.buffer:buffer-replace buffer start end (org-line-ends-kept buffer start end text))
    (when point
      (setf (tuatara.buffer:buffer-point buffer) (org-char->byte buffer point)))))

(defmacro with-org-structure ((buffer tree text position) &body body)
  "Run BODY with BUFFER the current Org buffer, TREE and TEXT its parse as it
is now and POSITION its point as a character of it."
  `(let ((,buffer (current-buffer)))
     (multiple-value-bind (,tree ,text) (org-tree-now ,buffer)
       (let ((*org-current-text* (or ,text ""))
             (,position (org-point-char ,buffer)))
         (declare (ignorable ,position))
         (if (null ,tree)
             (message "No Org structure here")
             (progn ,@body))))))

(defun org-ends-with-newline-p (string)
  "True when STRING ends with a newline."
  (and (plusp (length string)) (char= (char string (1- (length string))) #\Newline)))

(defun org-ensure-newline (string)
  "STRING ending with a newline."
  (if (org-ends-with-newline-p string)
      string
      (concatenate 'string string (string #\Newline))))

(defun org-text-lines (text)
  "The lines of TEXT, the newline it ends with not making one more; none for an empty TEXT."
  (and (plusp (length text))
       (uiop:split-string (if (org-ends-with-newline-p text) (subseq text 0 (1- (length text))) text)
                          :separator '(#\Newline))))

(defun org-entry-from-lines (headline lines final-newline)
  "The entry made of HEADLINE, its headline's line, and LINES under it, ending with a newline when FINAL-NEWLINE is true."
  (format nil "~a~{~%~a~}~:[~;~%~]" headline lines final-newline))

;;; Moving subtrees

(defun org-move-subtree (direction)
  "Move the subtree point is in over its sibling in DIRECTION, :up or :down,
point going with it, each kept folded when it was."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not in a subtree")
          (let* ((siblings (org-siblings headline tree))
                 (index (position headline siblings))
                 (other (and index (let ((at (if (eq direction :down) (1+ index) (1- index))))
                                     (and (>= at 0) (nth at siblings))))))
            (if (or (null other) (and (eq direction :up) (zerop index)))
                (message "Cannot move past superior level or buffer limit")
                (let* ((first (if (eq direction :down) headline other))
                       (second (if (eq direction :down) other headline))
                       (from (org-node-begin first))
                       (to (org-node-end second))
                       (first-text (subseq text from (org-node-end first)))
                       (second-text (subseq text (org-node-begin second) to))
                       (ended (and (plusp to) (char= (char text (1- to)) #\Newline)))
                       (moved (concatenate 'string (org-ensure-newline second-text) (org-ensure-newline first-text)))
                       (moved (if ended moved (subseq moved 0 (1- (length moved)))))
                       ;; folded before: folded after, where each went
                       (first-folded (let ((r (org-fold-range first text))) (and r (org-folded-p buffer r))))
                       (second-folded (let ((r (org-fold-range second text))) (and r (org-folded-p buffer r))))
                       (offset (- position (org-node-begin headline)))
                       ;; after the move, SECOND stands at FROM and FIRST after it
                       (second-at from)
                       (first-at (+ from (length (org-ensure-newline second-text))))
                       (new-begin (if (eq direction :down) first-at second-at)))
                  (org-replace-chars buffer from to moved :point (+ new-begin offset))
                  (when (or first-folded second-folded)
                    (multiple-value-bind (tree text) (org-tree-now buffer)
                      (let ((*org-current-text* text))
                        (flet ((refold (begin)
                                 (let ((h (find begin (org-headlines tree) :key #'org-node-begin)))
                                   (when h
                                     (let ((r (org-fold-range h text)))
                                       (when r (org-fold-chars buffer (car r) (cdr r))))))))
                          (when first-folded (refold first-at))
                          (when second-folded (refold second-at)))))))))))))

(define-command org-move-subtree-down ()
  "Move the subtree point is in down, past its next sibling."
  (org-move-subtree :down))

(define-command org-move-subtree-up ()
  "Move the subtree point is in up, before its sibling above."
  (org-move-subtree :up))

;;; Promoting and demoting

(defun org-change-levels (buffer headlines text by)
  "Change the levels of HEADLINES of BUFFER (whose parse's text is TEXT) by BY
stars, one step to undo, point staying on its character."
  (let* ((position (org-point-char buffer))
         (from (reduce #'min headlines :key #'org-node-begin))
         (to (reduce #'max headlines :key (lambda (h) (+ (org-node-begin h) (org-property h :level)))))
         (shift 0)
         (pieces '())
         (at from))
    (dolist (headline (sort (copy-list headlines) #'< :key #'org-node-begin))
      (let ((begin (org-node-begin headline)) (level (org-property headline :level)))
        (push (subseq text at begin) pieces)
        (push (make-string (+ level by) :initial-element #\*) pieces)
        ;; stars wholly before point move it; point among them stays
        (when (<= (+ begin level) position) (incf shift by))
        (setf at (+ begin level))))
    (push (subseq text at to) pieces)
    (org-replace-chars buffer from to (apply #'concatenate 'string (nreverse pieces))
                       :point (max from (+ position shift)))))

(defun org-promote (subtree by)
  "Change the level of the headline point is in by BY (-1 promotes, 1
demotes), with SUBTREE all the headlines under it too."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (cond ((null headline) (message "Not in a subtree"))
            ((and (minusp by) (= 1 (org-property headline :level))) (message "Cannot promote to level 0"))
            (t (org-change-levels buffer (if subtree (org-headlines headline) (list headline)) text by))))))

(define-command org-do-promote ()
  "Promote the headline point is in a level, its subtree's other headlines as
they are."
  (org-promote nil -1))

(define-command org-do-demote ()
  "Demote the headline point is in a level, its subtree's other headlines as
they are."
  (org-promote nil 1))

(define-command org-promote-subtree ()
  "Promote the headline point is in a level, and every headline under it."
  (org-promote t -1))

(define-command org-demote-subtree ()
  "Demote the headline point is in a level, and every headline under it."
  (org-promote t 1))

;;; TODO states

(defvar *org-todo-keywords-not-cycled* '("CANCELLED" "CANCELED")
  "The TODO keywords passed over as a task's keyword goes round to the next:
by \\<org-mode>\\[org-todo], \\[org-shiftright] and \\[org-shiftleft], by RET on a task's
headline and by the agenda's \\<org-agenda-mode>\\[org-agenda-todo]. With the standard
keywords a task goes TODO, WAIT, DONE and none, never CANCELLED, which
\\<org-mode>\\[org-cancel-task] sets. The keywords of a file's own #+TODO: line are
passed over the same way; NIL for every keyword to take its turn.")

(defun org-keyword-sequence (text)
  "The TODO keywords of TEXT in the order they cycle: those not done, then
those done, but for those of *ORG-TODO-KEYWORDS-NOT-CYCLED*."
  (destructuring-bind (not-done . done) (org-todo-keywords-of text)
    (remove-if (lambda (keyword) (member keyword *org-todo-keywords-not-cycled* :test #'string=))
               (append not-done done))))

(defun org-state-note (new old now)
  "The note Org logs of a change of state from OLD to NEW at the universal time NOW, as
  - State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]."
  (format nil "- State ~12a from ~12a ~a" (format nil "\"~a\"" new) (format nil "\"~a\"" (or old "")) (org-timestamp-text now)))

(defun org-entry-lines-with (lines property value)
  "LINES, those of an entry after its headline, with PROPERTY set to VALUE in
its property drawer, made after its planning line when it has none."
  (let* ((planning (and lines (org-planning-line-p (first lines))))
         (head (if planning (list (first lines)) '()))
         (rest (if planning (rest lines) lines))
         (drawer '()))
    (if (and rest (string-equal (string-trim " 	" (first rest)) ":PROPERTIES:"))
        (let ((end (position-if (lambda (l) (string-equal (string-trim " 	" l) ":END:")) rest)))
          (setf drawer (subseq rest 0 (if end (1+ end) (length rest)))
                rest (if end (subseq rest (1+ end)) '())))
        (setf drawer (list ":PROPERTIES:" ":END:")))
    (let* ((key (format nil ":~a:" property))
           (line (format nil "~10a ~a" key value))
           (at (position-if (lambda (l)
                              (let ((from (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) l) (length l))))
                                (and (<= (+ from (length key)) (length l))
                                     (string-equal key l :start2 from :end2 (+ from (length key))))))
                            drawer)))
      (if at
          (setf (nth at drawer) line)
          (setf drawer (append (butlast drawer) (list line) (last drawer)))))
    (append head drawer rest)))

(defun org-line-after (text position)
  "Where the line after the one ending at or after POSITION in TEXT begins."
  (let ((newline (position #\Newline text :start (max 0 (min position (length text))))))
    (if newline (1+ newline) (length text))))

(defun org-log-place (owner text)
  "Where what OWNER logs goes in TEXT, OWNER a headline or a list item with a
checkbox, as two values: where a :LOGBOOK: drawer is made for it, or a line
goes with none (the line after a headline's own line, its planning line and
its properties; after an item's first line); and where the line after the
first line of its :LOGBOOK: drawer begins, the first drawer of that name in its
own text (a sub-headline's or a list item's left to them), or NIL when it has none."
  (let* ((*org-text* text)
         (item-p (eq (org-node-type owner) 'item))
         (children (if item-p
                       (org-node-children owner)
                       (let ((section (find 'section (org-node-children owner) :key #'org-node-type)))
                         (and section (org-node-children section)))))
         (place (org-line-after text (org-node-begin owner)))
         (drawer (find-if (lambda (child) (and (eq (org-node-type child) 'drawer)
                                               (string-equal (org-property child :drawer-name) "LOGBOOK")))
                          children)))
    (unless item-p
      (loop for child in children
            while (member (org-node-type child) '(planning property-drawer))
            do (setf place (org-line-after text (max (org-node-begin child) (1- (org-content-end child)))))))
    (values place (and drawer (org-line-after text (org-node-begin drawer))))))

(defun org-log-edit (owner text lines &key (drawer t))
  "The edit, (FROM TO NEW) of TEXT's characters, logging LINES for OWNER, a
headline or a list item with a checkbox, newest first: at the top of its
:LOGBOOK: drawer, one made where ORG-LOG-PLACE says when it has none; with
DRAWER NIL, there with no drawer. An item's lines are set in under it, as far
as the text after its bullet."
  (multiple-value-bind (place logbook) (org-log-place owner text)
    (let* ((indent (if (eq (org-node-type owner) 'item) (org-item-indent owner text) ""))
           (body (format nil "~{~a~a~%~}" (loop for line in lines collect indent collect line))))
      (if (and drawer logbook)
          (list logbook logbook body)
          (let ((new (if drawer (format nil "~a:LOGBOOK:~%~a~a:END:~%" indent body indent) body)))
            (list place place
                  ;; after a last line with no newline
                  (if (and (= place (length text)) (plusp place) (char/= (char text (1- place)) #\Newline))
                      (concatenate 'string (string #\Newline) new)
                      new)))))))

(defun org-entry-with-log (headline text lines drawer)
  "The text of HEADLINE's entry in TEXT (from its line to its first
sub-headline), with LINES logged as ORG-LOG-EDIT logs them (DRAWER as it
takes it); none when LINES is NIL."
  (let ((begin (org-node-begin headline))
        (end (org-entry-end headline text)))
    (if (null lines)
        (subseq text begin end)
        (destructuring-bind (from to new) (org-log-edit headline text lines :drawer drawer)
          (concatenate 'string (subseq text begin from) new (subseq text to end))))))

(defun org-entry-end (headline text)
  "Where the entry of HEADLINE ends in TEXT: at its first sub-headline, else at its end."
  (let ((child (find 'headline (org-node-children headline) :key #'org-node-type)))
    (if child (org-node-begin child) (min (length text) (org-node-end headline)))))

(defun org-keyword-place (text begin level line-end)
  "Where in TEXT the TODO keyword of the headline at BEGIN, LEVEL stars deep,
is or would be put: after its stars and the spaces and tabs after them, at
LINE-END, its line's end, at the latest."
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) text :start (min line-end (+ begin level)) :end line-end)
      line-end))

(defun org-boxes-cleared (lines)
  "LINES, an entry's, with every list item's checkbox cleared ([X] and [-]
made [ ]) and the counts on the lines of items with checkboxes made nought
\([0/3], [0%]), and whether a checkbox was cleared, as two values."
  (let ((changed nil))
    (values (mapcar (lambda (line)
                      (multiple-value-bind (start end) (cl-ppcre:scan "^[ \\t]*(?:[-+*]|[0-9]+[.)])[ \\t]+\\[[ Xx-]\\]" line)
                        (declare (ignore start))
                        (cond ((null end) line)
                              (t (unless (char= (char line (- end 2)) #\Space) (setf changed t))
                                 (org-cookies-nought (concatenate 'string (subseq line 0 (- end 3)) "[ ]" (subseq line end)))))))
                    lines)
            changed)))

(defun org-cookies-nought (text)
  "TEXT with its statistics cookies made nought: [2/3] [0/3], [66%] [0%]."
  (cl-ppcre:regex-replace-all "\\[[0-9]+%\\]" (cl-ppcre:regex-replace-all "\\[[0-9]+/([0-9]+)\\]" text "[0/\\1]") "[0%]"))

(defun org-cookies-saying (text done total)
  "TEXT with its statistics cookies saying DONE of TOTAL: [2/3], [66%]."
  (cl-ppcre:regex-replace-all "\\[[0-9]*(?:%|/[0-9]*)\\]" text
                              (lambda (cookie) (org-cookie-text cookie done total))
                              :simple-calls t))

(defun org-boxes-ticked (lines)
  "How many of the checkboxes of LINES, an entry's, that are least indented
\(those its headline's counts count) are ticked, and how many there are, as
two values."
  (let ((boxes (loop for line in lines
                     for (start end) = (multiple-value-list (cl-ppcre:scan "^[ \\t]*(?:[-+*]|[0-9]+[.)])[ \\t]+\\[[ Xx-]\\]" line))
                     when start collect (cons (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line)
                                              (char-equal (char line (- end 2)) #\X)))))
    (let ((least (loop for box in boxes minimize (car box))))
      (loop for (indent . ticked) in boxes
            when (= indent least) count ticked into done and count t into total
            finally (return (values done total))))))

(defparameter *org-boxes-ticked-note* "  ~d of ~d ~:[checkboxes ticked~;sub-tasks done~]"
  "The line under the note of a repeating task done that says how many of its
checkboxes were ticked then, or of its sub-tasks done, before they were
opened again for the next time.")

(defun org-subtasks-reopened (lines done-keywords first-todo)
  "LINES, those of the sub-headlines of a repeating task and what is under
them, opened again for its next time: each sub-task with a keyword of
DONE-KEYWORDS made FIRST-TODO, the CLOSED: of its planning line taken out,
checkboxes cleared and counts made nought (ORG-BOXES-CLEARED); and how many
sub-tasks were opened, as a second value."
  (let ((scanner (cl-ppcre:create-scanner (format nil "^(\\*+[ \\t]+)(~{~a~^|~})(?=[ \\t]|$)"
                                                  (mapcar #'cl-ppcre:quote-meta-chars done-keywords))))
        (opened 0) (after-opened nil) (out '()))
    (dolist (line (org-boxes-cleared lines))
      (multiple-value-bind (start end starts ends) (cl-ppcre:scan scanner line)
        (declare (ignore start starts))
        (cond (end
               (incf opened)
               (setf after-opened t)
               (push (org-cookies-nought (concatenate 'string (subseq line 0 (aref ends 0)) first-todo (subseq line (aref ends 1))))
                     out))
              ((and after-opened (cl-ppcre:scan "^[ \\t]*CLOSED: \\[[^]\\n]*\\]" line))
               (setf after-opened nil)
               (let ((rest (cl-ppcre:regex-replace "CLOSED: \\[[^]\\n]*\\][ \\t]*" line "")))
                 (unless (string= (string-trim " 	" rest) "") (push rest out))))
              (t (setf after-opened nil)
                 (push (if (cl-ppcre:scan "^\\*+[ \\t]" line) (org-cookies-nought line) line) out)))))
    (values (nreverse out) opened)))

(defun org-repeat-entry (buffer headline text position old done now &key cancelling-time)
  "Repeat the task of HEADLINE (in BUFFER, TEXT its text), with timestamps
that repeat, as it goes from OLD to the done keyword DONE at the universal
time NOW: those timestamps moved to their next time, its keyword the first of
its sequence again, and with *ORG-LOG-REPEAT* LAST_REPEAT set and the change
of state logged; in one step to undo. A task by rule (ORG-RULE-TIMESTAMP-IN)
keeps its rule, LAST_REPEAT, set whatever *ORG-LOG-REPEAT* says, saying the
day it is done for (ORG-RULE-DONE-TEXT). Its checkboxes are cleared for the
next time (ORG-BOXES-CLEARED), and the counts of its headline with them when
they count checkboxes; its sub-tasks done are opened again
\(ORG-SUBTASKS-REOPENED), and its counts with them when they count those. How
many were ticked or done is said under the note of DONE when its headline
counts them (*ORG-BOXES-TICKED-NOTE*), for the agenda's line of the day it
was done.
The times cancelled one at a time
\(ORG-OCCURRENCE-NOTES) are passed over. CANCELLING-TIME, an occurrence, is
the time this one is cancelled, not done: noted as such
\(ORG-OCCURRENCE-NOTE), whatever *ORG-LOG-REPEAT* says."
  (let* ((begin (org-node-begin headline))
         (line-end (string-line-end text begin))
         (end (org-entry-end headline text))
         (cancelled (org-entry-notes headline text))
         (level (org-property headline :level))
         (keyword-at (org-keyword-place text begin level line-end))
         (old-end (if old (min line-end (+ keyword-at (length old) 1)) keyword-at))
         (first-todo (first (car (org-todo-keywords-of text))))
         (subtree-end (max end (min (length text) (org-node-end headline))))
         ;; what its counts count, as (DONE ALL SUB-TASKS-P): its TODO sub-headlines, or with none its checkboxes
         (ticked (multiple-value-bind (all finished) (org-headline-todo-counts headline)
                   (if (plusp all)
                       (list finished all t)
                       (append (multiple-value-list (org-boxes-ticked (org-text-lines (subseq text line-end end)))) (list nil)))))
         (entry (org-entry-with-log headline text
                                    (cond (cancelling-time (list (org-occurrence-note :cancelled cancelling-time nil now)))
                                          ((and *org-log-repeat* (plusp (second ticked)) (org-cookies-of headline))
                                           ;; a note's own words go on the lines after its \\, as Org writes them
                                           (list (concatenate 'string (org-state-note done old now) " \\\\")
                                                 (format nil *org-boxes-ticked-note* (first ticked) (second ticked) (third ticked))))
                                          (*org-log-repeat* (list (org-state-note done old now))))
                                    *org-log-into-drawer*))
         (entry-line-end (or (position #\Newline entry) (length entry)))
         (headline-text (concatenate 'string (subseq entry 0 (- keyword-at begin)) first-todo " " (subseq entry (- old-end begin) entry-line-end)))
         (body (if (< entry-line-end (length entry)) (subseq entry (1+ entry-line-end)) ""))
         (moved (org-repeat-timestamps body now (if cancelling-time (cons cancelling-time cancelled) cancelled)))
         (rule (org-rule-timestamp-in body 0 (length body)))
         (done-for (and rule (org-rule-done-text rule (org-headline-property headline "LAST_REPEAT") now cancelled)))
         (lines (org-text-lines moved))
         (boxes-cleared (nth-value 1 (org-boxes-cleared lines)))
         (lines (org-boxes-cleared lines))
         (subtree (subseq text end subtree-end))
         (reopened (multiple-value-list (org-subtasks-reopened (org-text-lines subtree) (cdr (org-todo-keywords-of text)) first-todo)))
         (headline-text (if (if (third ticked) (plusp (second reopened)) boxes-cleared)
                            (org-cookies-nought headline-text)
                            headline-text))
         (lines (cond (done-for (org-entry-lines-with lines "LAST_REPEAT" done-for))
                      (*org-log-repeat* (org-entry-lines-with lines "LAST_REPEAT" (org-timestamp-text now)))
                      (t lines)))
         (new (org-entry-from-lines headline-text lines (or (org-ends-with-newline-p moved)
                                                            (and (null lines) (< entry-line-end (length entry)))))))
    (when (plusp (second reopened))
      (setf new (concatenate 'string (org-ensure-newline new)
                             (format nil "~{~a~^~%~}~:[~;~%~]" (first reopened) (org-ends-with-newline-p subtree)))))
    (org-replace-chars buffer begin (if (plusp (second reopened)) subtree-end end) new
                       :point (if (<= position line-end) (min position (+ begin (length headline-text))) begin))
    ;; said: what it was done or cancelled for, and a rule's next day
    (let* ((for-day (cond (cancelling-time (car cancelling-time))
                          (done-for (org-ts-day-number (org-read-ts done-for)))))
           (next (and rule for-day (org-rule-next-open rule (1+ for-day) (if cancelling-time (cons cancelling-time cancelled) cancelled)))))
      (message "~a~@[; next ~a~]: ~a~:[~;, its checkboxes cleared~]~[~:;, ~:*~d sub-task~:p opened again~]"
               (cond (cancelling-time (format nil "Cancelled for ~a" (org-occurrence-words cancelling-time)))
                     (done-for (format nil "Done for ~a" (subseq done-for 1 (1- (length done-for)))))
                     (t "Entry repeats"))
               (and next (format-day "%F %a" next)) first-todo boxes-cleared (second reopened)))))

(defun org-set-todo (step)
  "Set the TODO keyword of the headline point is in to the one STEP (1 or -1)
from it in the document's sequence (ORG-KEYWORD-SEQUENCE), none past either
end; from none, or from a keyword the sequence passes over, the first with
STEP 1 and the last with -1. As ORG-PUT-TODO sets it."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not in a subtree")
          (let* ((sequence (org-keyword-sequence text))
                 (old (org-property headline :todo-keyword))
                 (index (and old (position old sequence :test #'string=))))
            (org-put-todo buffer text position headline
                          (if index
                              (let ((next (+ index step))) (and (< -1 next (length sequence)) (nth next sequence)))
                              (if (plusp step) (first sequence) (car (last sequence))))))))))

(defun org-set-todo-to (keyword)
  "Set the TODO keyword of the headline point is in to KEYWORD, NIL for none,
as ORG-PUT-TODO sets it."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not in a subtree")
          (org-put-todo buffer text position headline keyword)))))

(defun org-put-todo (buffer text position headline new &key (repeat t))
  "Make NEW, a TODO keyword or NIL for none, the keyword of HEADLINE in BUFFER,
TEXT its text and POSITION point as a character of it: reaching a done
keyword from one not done notes when (CLOSED:), leaving it takes that away.
A task whose date repeats, made done, is done this time and moves on to its
next date instead, unless REPEAT is NIL."
  (let* ((done-keywords (cdr (org-todo-keywords-of text)))
         (old (org-property headline :todo-keyword))
         (begin (org-node-begin headline))
         (level (org-property headline :level))
         (line-end (string-line-end text begin))
         (keyword-at (org-keyword-place text begin level line-end))
         (old-end (if old (min line-end (+ keyword-at (length old) 1)) keyword-at))
         (inserted (if new (format nil "~a " new) ""))
         (was-done (and old (member old done-keywords :test #'string=)))
         (now-done (and new (member new done-keywords :test #'string=)))
         (position (if (>= position old-end) (+ position (- (length inserted) (- old-end keyword-at))) position))
         (now (org-now)))
    (when (and repeat now-done (not was-done)
               (or (org-repeating-timestamp-in-p text line-end (org-entry-end headline text))
                   ;; a task by rule with no day after this one is done for good
                   (let ((rule (org-rule-timestamp-in text line-end (org-entry-end headline text))))
                     (and rule (org-rule-goes-on-p rule (org-headline-property headline "LAST_REPEAT") now
                                                   (org-entry-notes headline text))))))
      (return-from org-put-todo (org-repeat-entry buffer headline text (org-point-char buffer) old new now)))
    ;; the planning line under the headline: CLOSED: added or taken away
    (let* ((next-line (min (length text) (1+ line-end)))
           (planning (and (< next-line (length text))
                          (org-planning-line-p text :start next-line :end (string-line-end text next-line))))
           (planning-end (and planning (string-line-end text next-line)))
           (planning-text (and planning (subseq text next-line planning-end)))
           (new-planning
             (cond ((and now-done (not was-done) (not *org-log-done*)) nil)
                   ((and now-done (not was-done))
                    (if planning
                        (cl-ppcre:regex-replace "^([ \\t]*)" planning-text
                                                (format nil "\\1CLOSED: ~a " (org-timestamp-text now)))
                        :new))
                   ((and was-done (not now-done) planning)
                    (string-right-trim " " (cl-ppcre:regex-replace "CLOSED:[ \\t]*\\[[^]]*\\][ \\t]*" planning-text "")))
                   (t nil))))
      (let* ((headline-text (concatenate 'string (subseq text begin keyword-at) inserted (subseq text old-end line-end)))
             (region-end (cond ((null new-planning) line-end)
                               ((eq new-planning :new) line-end)
                               (t planning-end)))
             (replacement
               (cond ((null new-planning) headline-text)
                     ((eq new-planning :new)
                      (format nil "~a~%CLOSED: ~a" headline-text (org-timestamp-text now)))
                     ((string= (string-trim " 	" new-planning) "")
                      ;; the planning line emptied: it goes, with its newline
                      (setf region-end (min (length text) (1+ planning-end)))
                      (format nil "~a~%" headline-text))
                     (t (format nil "~a~%~a" headline-text new-planning)))))
        (org-replace-chars buffer begin region-end replacement :point position)
        (message "~:[No keyword~;~:*~a~]" new)))))

(define-command org-todo ()
  "Change the TODO keyword of the headline point is in to the next of the
document's (#+TODO:, else TODO, WAIT and DONE), none after the last, those of
*org-todo-keywords-not-cycled* passed over (CANCELLED, which
\\[org-cancel-task] sets); a task done is noted with CLOSED: and when. On a
list item's line with a checkbox, tick it, or clear it, as a task done or
not, logged (*org-log-checkboxes*)."
  (unless (call-on-checkbox-item (lambda (buffer tree text position item)
                                   (org-toggle-checkbox buffer tree text position item)))
    (org-set-todo 1)
    (org-todo-changed (current-buffer))))

(defvar *org-return-cycles-todo* t
  "True for \\<normal>\\[tuatara-normal-return] in NORMAL state on a headline's line with a
TODO keyword to change it to the next, as \\[org-todo] does, but never to
none: TODO to WAIT to DONE and round, or round the document's #+TODO:
keywords, those of *org-todo-keywords-not-cycled* passed over (a link
point is on is followed first). A task whose date repeats, made done, is
done this time and goes on to its next. While a checkbox right under it is
not ticked or a TODO under it not done, the keywords that are done are passed
over: TODO to NEXT to WAIT and round with #+TODO: TODO NEXT WAIT | DONE; with
none other to go to, it says what is open, and \\[org-todo] makes it done all
the same. NIL to leave it to \\[org-todo].")

(defun org-open-under (headline)
  "What keeps HEADLINE from being done: how many of the checkboxes right under
it are not ticked, and how many of its TODO sub-headlines are not done, as
two values."
  (multiple-value-bind (boxes ticked) (org-checkbox-counts headline)
    (multiple-value-bind (tasks done) (org-headline-todo-counts headline)
      (values (- boxes ticked) (- tasks done)))))

(defun org-todo-return (buffer)
  "\\<normal>\\[tuatara-normal-return] in an Org buffer on a headline's line with a TODO
keyword: change it to the next, round to the first after the last
\(*ORG-RETURN-CYCLES-TODO*), the done ones passed over while a checkbox under
it is not ticked or a TODO under it not done; with none other to go to, say
what is open. True when it handled the line."
  ;; the file parsed only for a line that is a headline
  (when (and *org-return-cycles-todo* (typep (buffer-major-mode buffer) 'org-mode)
             (cl-ppcre:scan "^\\*+ " (org-line-head buffer (tuatara.buffer:buffer-point buffer))))
    (multiple-value-bind (tree text) (org-tree-now buffer)
      (let* ((*org-current-text* text)
             (headline (and tree (org-headline-on-line tree text (org-point-char buffer))))
             (keyword (and headline (org-property headline :todo-keyword))))
        (when keyword
          (let* ((sequence (org-keyword-sequence text))
                 (done-keywords (cdr (org-todo-keywords-of text)))
                 (count (length sequence))
                 (index (or (position keyword sequence :test #'string=) -1)))
            (multiple-value-bind (boxes tasks) (org-open-under headline)
              (let* ((open (plusp (+ boxes tasks)))
                     (target (loop for k from 1 to count
                                   for candidate = (nth (mod (+ index k) count) sequence)
                                   unless (and open (member candidate done-keywords :test #'string=))
                                     return candidate)))
                (check-writable buffer)
                (if (or (null target) (string= target keyword))
                    (let ((done (find-if (lambda (k) (member k done-keywords :test #'string=)) sequence)))
                      (message "Not ~a yet: ~@[~a~]~:[~; and ~]~@[~a~] still open; ~a makes it ~a all the same"
                               done
                               (and (plusp boxes) (format nil "~d checkbox~:*~[es~;~:;es~]" boxes))
                               (and (plusp boxes) (plusp tasks))
                               (and (plusp tasks) (format nil "~d task~:p" tasks))
                               (substitute-command-keys "\\<org-mode>\\[org-todo]") done))
                    (progn
                      ;; straight to it: round from the last to the first without passing through none
                      (org-set-todo-to target)
                      (org-todo-changed buffer))))
              t)))))))

(define-command org-shiftright ()
  "On a headline, its next TODO keyword, as \\[org-todo] does."
  (org-set-todo 1)
  (org-todo-changed (current-buffer)))

(define-command org-shiftleft ()
  "On a headline, its TODO keyword before, none before the first."
  (org-set-todo -1)
  (org-todo-changed (current-buffer)))

(defun org-cancel-keyword (text)
  "The done keyword of TEXT that cancels a task: CANCELLED, else CANCELED;
NIL when its #+TODO: lines have neither."
  (let ((done (cdr (org-todo-keywords-of text))))
    (find-if (lambda (name) (member name done :test #'string=)) *org-cancel-keywords*)))

(defvar *org-agenda-occurrence* nil
  "While the agenda cancels or schedules the entry on its line, the time of
that line, an occurrence (ORG-OCCURRENCE-NOTES); NIL for the entry's time
open now.")

(defun org-repeats-in-p (text start end)
  "True when the entry in TEXT from START below END has a date that repeats:
a timestamp with a repeater, or one by rule."
  (or (org-repeating-timestamp-in-p text start end) (org-rule-timestamp-in text start end)))

(defun org-entry-notes (headline text)
  "The times of HEADLINE's entry in TEXT cancelled and moved one at a time,
as two values (ORG-OCCURRENCE-NOTES)."
  (org-occurrence-notes text (string-line-end text (org-node-begin headline)) (org-entry-end headline text)))

(defun org-task-keyword-p (keyword text)
  "True when KEYWORD is one of TEXT's TODO keywords not done: the entry it
heads is a task still to do."
  (and keyword (not (member keyword (cdr (org-todo-keywords-of text)) :test #'string=))))

(defun org-occurrence< (a b)
  "True when the occurrence A comes before B: by day, then by time of day,
one of no time first."
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (or (cdr a) -1) (or (cdr b) -1)))))

(defun org-open-occurrence (headline text now task)
  "The time of the repeating entry of HEADLINE in TEXT open at the universal
time NOW, an occurrence, or NIL. Of a TASK, the time it is done for when
marked done: by rule, the day ORG-RULE-DONE-DAY gives; else the earliest of
its timestamps that repeat. Of an entry that is no task, the first of its
times from today on."
  (let* ((start (string-line-end text (org-node-begin headline)))
         (end (org-entry-end headline text))
         (cancelled (org-entry-notes headline text))
         (today (org-today now))
         (rule (org-rule-timestamp-in text start end)))
    (if rule
        (let ((day (if task
                       (let ((last (org-read-ts (or (org-headline-property headline "LAST_REPEAT") ""))))
                         (org-rule-done-day rule (and last (org-ts-day-number last)) today cancelled))
                       (org-rule-next-open rule today cancelled))))
          (and day (cons day (org-rule-time-of-day rule))))
        (let ((open '()))
          (cl-ppcre:do-matches (s e *org-active-timestamp-scanner* text nil :start start :end end)
            (let ((ts (org-read-ts (subseq text s e))))
              (when (org-repeating-ts-p ts)
                (let* ((time (org-ts-time-of-day ts))
                       (repeater (list ts (org-ts-repeat-value ts) (org-ts-repeat-unit ts) (org-ts-day-number ts)))
                       (day (if task
                                (org-ts-day-number ts)
                                (org-step-past (org-repeat-day repeater today :future)
                                               (lambda (day) (org-repeat-day repeater (1+ day) :future))
                                               (lambda (day) (org-cancelled-p cancelled day time))))))
                  (when day (push (cons day time) open))))))
          (first (sort open #'org-occurrence<))))))

(defun org-log-note (buffer headline text position note)
  "Note NOTE in the log of HEADLINE's entry in BUFFER (TEXT its text, POSITION
point as a character of it), point kept on the headline's line, else put at
its start."
  (let ((begin (org-node-begin headline)))
    (org-replace-chars buffer begin (org-entry-end headline text)
                       (org-entry-with-log headline text (list note) *org-log-into-drawer*)
                       :point (if (<= position (string-line-end text begin)) position begin))))

(defun org-cancel-occurrence (buffer headline text position occurrence)
  "Cancel one time of the repeating entry of HEADLINE in BUFFER (TEXT its
text, POSITION point as a character of it), the rest going on: OCCURRENCE,
or the one open now (ORG-OPEN-OCCURRENCE) when it is NIL, where it was moved
to when it was. Noted in its log (ORG-OCCURRENCE-NOTE), which the agenda and
the repeats read; a task's open time cancelled moves it on to its next, as
done does."
  (let* ((now (org-now))
         (keyword (org-property headline :todo-keyword))
         (task (org-task-keyword-p keyword text))
         (open (org-open-occurrence headline text now task)))
    (multiple-value-bind (cancelled moved) (org-entry-notes headline text)
      (let ((wanted (let ((at (or occurrence open))) (and at (org-moved-target moved at)))))
        (cond ((null wanted) (message "No time of it is left to cancel"))
              ((org-cancelled-p cancelled (car wanted) (cdr wanted))
               (message "~a is cancelled already" (org-occurrence-words wanted)))
              ;; the time open now, where it is or where it was moved to
              ((and task open (or (= (car wanted) (car open)) (equal wanted (org-moved-target moved open))))
               (org-repeat-entry buffer headline text position keyword "CANCELLED" now :cancelling-time wanted))
              (t (org-log-note buffer headline text position (org-occurrence-note :cancelled wanted nil now))
                 (message "Cancelled for ~a; the rest of it goes on" (org-occurrence-words wanted))))))))

(defun org-move-occurrence (buffer headline text position occurrence to)
  "Move one time of the repeating entry of HEADLINE in BUFFER (TEXT its text,
POSITION point as a character of it) to the occurrence TO, its own time of
day kept when TO has none, the rest as it was: OCCURRENCE, or the one open
now (ORG-OPEN-OCCURRENCE) when it is NIL, where it was moved to when it was.
Noted in its log (ORG-OCCURRENCE-NOTE), which the agenda reads: that time
listed on TO's day, and a task done there done for the time it was."
  (let* ((now (org-now))
         (task (org-task-keyword-p (org-property headline :todo-keyword) text)))
    (multiple-value-bind (cancelled moved) (org-entry-notes headline text)
      (let* ((from (let ((at (or occurrence (org-open-occurrence headline text now task))))
                     (and at (org-moved-target moved at))))
             (to (and from (cons (car to) (or (cdr to) (cdr from))))))
        (cond ((null from) (message "No time of it is left to move"))
              ((org-cancelled-p cancelled (car from) (cdr from))
               (message "~a is cancelled already" (org-occurrence-words from)))
              ((equal from to) (message "~a is where it is already" (org-occurrence-words from)))
              (t (org-log-note buffer headline text position (org-occurrence-note :moved from to now))
                 (message "Moved ~a to ~a; the rest of it goes on" (org-occurrence-words from) (org-occurrence-words to))))))))

(defun org-occurrence-choice (verb)
  "For the headline point is under, whose date repeats, ask as VERB (Cancel,
Schedule) whether to act on one time of it, the agenda's line's
\(*ORG-AGENDA-OCCURRENCE*) or the one open now, or on the whole of it:
:THIS for \\`t', :SERIES for \\`s', :NONE for any other key, said to do
nothing; \\`C-g' quits. :SERIES unasked when it does not repeat."
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (not (and headline
                    (org-repeats-in-p text (string-line-end text (org-node-begin headline)) (org-entry-end headline text))))
          :series
          (let ((key (read-key (format nil "~a: t this time~@[ (~a)~], s the series" verb
                                       (and *org-agenda-occurrence* (org-occurrence-words *org-agenda-occurrence*))))))
            (when (quit-key-p key) (keyboard-quit-signal))
            (let ((words (tuatara.frontend:key-description (tuatara.frontend:key-spec key))))
              (cond ((equal words "t") :this)
                    ((equal words "s") :series)
                    (t (message "~a is neither: t this time, s the series; nothing done" words)
                       :none))))))))

(defun org-move-asked ()
  "Ask for the day to move one time of the repeating entry point is under to,
its time too if it changes (READ-ORG-DATE), and move it (ORG-MOVE-OCCURRENCE)."
  (let ((ts (read-org-date "Move this time to: ")))
    (if (org-ts-p ts)
        (with-org-structure (buffer tree text position)
          (org-move-occurrence buffer (org-current-headline tree position) text position *org-agenda-occurrence*
                               (cons (org-ts-day-number ts) (org-ts-time-of-day ts))))
        (org-no-date))))

(define-command org-cancel-task ()
  "Cancel the task of the headline point is in: its keyword made CANCELLED, a
done keyword that \\[org-todo] and the other keys that go round the keywords
pass over, noted with CLOSED: and when as a task done is. A file with its
own #+TODO: line uses its done keyword CANCELLED or CANCELED, and says so
when it has neither. An entry whose date repeats asks first: \\`t' cancels
this time alone, the rest going on, noted in its log as cancelled for that
day and passed over by the agenda and its repeats; a task's time open now
cancelled moves on to its next, as done does. \\`s' cancels the series,
ended, not moved on."
  (if (org-checkbox-line-p (org-line-head (current-buffer) (point)))
      (message "A list item's checkbox is ticked or cleared, not cancelled: \\<org-mode>\\[org-todo] ticks it")
      (with-org-structure (buffer tree text position)
        (let* ((headline (org-current-headline tree position))
               (keyword (org-cancel-keyword text))
               (choice (org-occurrence-choice "Cancel")))
          (cond ((null headline) (message "Not in a subtree"))
                ((eq choice :none))
                ((eq choice :this) (org-cancel-occurrence buffer headline text position *org-agenda-occurrence*))
                ((null keyword) (message "No CANCELLED among this file's #+TODO: keywords"))
                (t (org-put-todo buffer text position headline keyword :repeat nil)
                   (org-todo-changed buffer)))))))

;;; Tags and notes

(defvar *org-tags-column* 0
  "Where \\[org-set-tags-command] and \\[org-align-tags] put a headline's
tags: 0, one space after the headline's text; negative, so that they end at
that column (-77 to end at the 77th); positive, so that they start there.")

(defun org-tags-from-text (text)
  "The tags TEXT names, :work:urgent: or work urgent or work, urgent: each
once, in order."
  (remove-duplicates (remove "" (cl-ppcre:split "[:,\\s]+" text) :test #'string=) :test #'string= :from-end t))

(defun read-org-tags (buffer &optional old)
  "Ask for tags, OLD, a list, written to begin with: the tags used in BUFFER
and the agenda files (ORG-KNOWN-TAGS) offered for the one being typed after
the last colon, those already written left out; work urgent and
:work:urgent: both taken. Returns the tags, a list."
  (let ((known (org-known-tags buffer)))
    (org-tags-from-text
     (completing-read "Tags: "
                      (lambda (input)
                        (let* ((cut (position #\: input :from-end t))
                               (before (if cut (subseq input 0 (1+ cut)) ""))
                               (written (org-tags-from-text before)))
                          (append
                           ;; nothing begun after the colon: what is written, as it is
                           (when (zerop (length (string-trim " " (subseq input (length before)))))
                             (list (make-candidate "" :value input)))
                           (loop for tag in known
                                 unless (member tag written :test #'string=)
                                   collect (make-candidate tag :value (concatenate 'string before tag ":"))))))
                      :separator #\: :initial (if old (format nil ":~{~a~^:~}:" old) "")))))

(defun org-headline-with-tags (line tags)
  "The headline LINE with its tags TAGS (a list), put at *ORG-TAGS-COLUMN*,
or none."
  (let* ((stripped (string-right-trim " 	" (cl-ppcre:regex-replace "[ \\t]+:[\\w@#%:]+:[ \\t]*$" line "")))
         ;; a headline of stars alone keeps the space that makes it one
         (bare (if (every (lambda (c) (char= c #\*)) stripped) (concatenate 'string stripped " ") stripped)))
    (if (null tags)
        bare
        (let* ((text (format nil ":~{~a~^:~}:" tags))
               (used (text-columns bare))
               (column (if (minusp *org-tags-column*) (- (abs *org-tags-column*) (text-columns text)) *org-tags-column*)))
          (concatenate 'string bare (make-string (max 1 (- column used)) :initial-element #\Space) text)))))

(define-command org-set-tags-command ()
  "Set the tags of the headline point is under
(\\`C-c C-q'): asked for with its tags offered, work urgent or :work:urgent:,
nothing for none; they go at the end of its line, where *ORG-TAGS-COLUMN*
says, by default one space after its text. With \\[universal-argument], the
tags of every headline of the buffer put there instead (\\[org-align-all-tags])."
  (when (prefix-argument)
    (return-from org-set-tags-command (org-align-all-tags)))
  (with-org-structure (buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not under a headline")
          (let* ((begin (org-node-begin headline))
                 (line-end (string-line-end text begin))
                 (line (subseq text begin line-end))
                 (old (org-property headline :tags))
                 (tags (read-org-tags buffer old))
                 (new (org-headline-with-tags line tags)))
            (if (string= new line)
                (message "Tags as they were")
                (progn (org-replace-chars buffer begin line-end new
                                          :point (if (> position line-end)
                                                     (+ position (- (length new) (- line-end begin)))
                                                     (min position (+ begin (length new)))))
                       (message "~:[No tags~;~:*Tags: ~{~a~^ ~}~]" tags))))))))

(defun org-tags-alignment-edits (headlines text)
  "The edits, (FROM TO NEW) of TEXT's characters, putting the tags of each of
HEADLINES that has any where *ORG-TAGS-COLUMN* says; none for those already there."
  (loop for headline in headlines
        for tags = (org-property headline :tags)
        for begin = (org-node-begin headline)
        for line-end = (string-line-end text begin)
        for line = (subseq text begin line-end)
        for new = (and tags (org-headline-with-tags line tags))
        when (and new (string/= new line))
          collect (list begin line-end new)))

(defun org-align-tags-in (buffer headlines text position)
  "Put the tags of HEADLINES of BUFFER (TEXT its parse, point at the character
POSITION) where *ORG-TAGS-COLUMN* says, in one step to undo; point kept by the
text it is in, or by the tags it is in; how many lines changed."
  (let* ((edits (org-tags-alignment-edits headlines text))
         ;; on a line that changes, as far from its end as it was, past the edits before the line
         (inside (find-if (lambda (edit) (<= (first edit) position (second edit))) edits))
         (point (if inside
                    (destructuring-bind (begin line-end new) inside
                      (let ((title-end (length (string-right-trim " 	" (cl-ppcre:regex-replace "[ \\t]+:[\\w@#%:]+:[ \\t]*$" new ""))))
                            (before (loop for (start end other) in edits
                                          when (<= end begin) sum (- (length other) (- end start)))))
                        (+ before
                           (if (<= position (+ begin title-end))
                               position
                               (max (+ begin title-end) (- (+ begin (length new)) (- line-end position)))))))
                    position)))
    (when edits
      (check-writable buffer)
      (org-apply-edits buffer edits :point point :final (and inside t)))
    (length edits)))

(define-command org-align-tags ()
  "Put the tags of the headline point is under where *ORG-TAGS-COLUMN* says:
by default one space after its text. With
\\[universal-argument], the tags of every headline of the buffer
\(\\[org-align-all-tags])."
  (if (prefix-argument)
      (org-align-all-tags)
      (with-org-structure (buffer tree text position)
        (let ((headline (org-current-headline tree position)))
          (cond ((null headline) (message "Not under a headline"))
                ((null (org-property headline :tags)) (message "No tags on this headline"))
                ((zerop (org-align-tags-in buffer (list headline) text position)) (message "Tags in place already"))
                (t (message "Tags put in place")))))))

(define-command org-align-all-tags ()
  "Put the tags of every headline of the buffer where *ORG-TAGS-COLUMN* says:
by default one space after each one's text, in
one step to undo."
  (with-org-structure (buffer tree text position)
    (let ((count (org-align-tags-in buffer (org-headlines tree) text position)))
      (message (if (zerop count) "Tags in place already" (format nil "Tags put in place on ~d headline~:p" count))))))

(define-command org-add-note ()
  "Add a note to the entry point is under (\\`C-c C-z'):
the note is asked for, and logged where Org logs notes, at the top of its
:LOGBOOK: drawer with *ORG-LOG-INTO-DRAWER*, else after its planning line and
properties: - Note taken on [2026-10-01 Thu 18:00] \\\\ and the note under it.
On a list item's line with a checkbox, on that item, in a :LOGBOOK: drawer
under it. An empty note is not kept."
  (with-org-structure (buffer tree text position)
    (let* ((item (org-checkbox-item-at tree text position))
           (owner (or item (org-current-headline tree position))))
      (if (null owner)
          (message "Not under a headline")
          (let ((note (string-trim " 	" (read-string "Note: "))))
            (if (zerop (length note))
                (message "No note taken: it was empty")
                (progn
                  (org-apply-edits buffer (list (org-log-edit owner text (list (format nil "- Note taken on ~a \\\\" (org-timestamp-text (org-now)))
                                                                               (format nil "  ~a" note))
                                                              :drawer (or item *org-log-into-drawer*)))
                                   :point position)
                  (message "Note taken"))))))))

;;; New headlines and items

(defun org-current-item (tree text position)
  "The innermost list item of TREE holding the character POSITION of TEXT, or
NIL; at the very end of a text not ending with a newline, the item of its
last line."
  (org-node-at tree (if (and (= position (length text)) (plusp position) (char/= (char text (1- position)) #\Newline))
                        (1- position)
                        position)
               (lambda (node) (eq (org-node-type node) 'item)) :innermost t))

(defun org-insert-heading (todo)
  "Insert a new headline or item for what point is in: in a list, a new item
after the one point is in and its sub-items, of the same kind (its bullet, the
next number, a checkbox [ ] when it has one); else a headline of the level
point is under after its subtree, with
TODO a task."
  (with-org-structure (buffer tree text position)
    (let ((item (and (not todo) (org-current-item tree text position)))
          (headline (org-current-headline tree position)))
      (cond
        ((and item (not (and headline (= (org-node-begin headline) (string-line-start text position)))))
         (let* ((end (org-last-content-end text (org-node-begin item) (org-node-end item)))
                (bullet (org-property item :bullet))
                (indent (subseq text (org-node-begin item)
                                (+ (org-node-begin item) (- (length (org-line-at text (org-node-begin item)))
                                                            (length (string-left-trim " 	" (org-line-at text (org-node-begin item))))))))
                (next (cl-ppcre:regex-replace "^(\\d+)" (string-trim " " bullet)
                                              (lambda (match number) (declare (ignore match)) (princ-to-string (1+ (parse-integer number))))
                                              :simple-calls t))
                (new (format nil "~%~a~a ~:[~;[ ] ~]" indent next (org-property item :checkbox)))
                (after (rest (member item (org-node-children (org-node-parent item))))))
           (org-apply-edits buffer
                            (cons (list end end new)
                                  (when (digit-char-p (char next 0))
                                    (loop for each in after
                                          for n from (1+ (parse-integer next :junk-allowed t))
                                          when (eq (org-node-type each) 'item)
                                            append (org-renumber-edit text each n))))
                            :point end)))
        (headline
         (let* ((level (org-property headline :level))
                (content-end (org-last-content-end text (org-node-begin headline) (org-node-end headline)))
                (blank (< (1+ content-end) (org-node-end headline)))
                (new (format nil "~%~:[~;~%~]~a ~:[~;TODO ~]" blank (make-string level :initial-element #\*) todo)))
           (org-replace-chars buffer content-end content-end new :point (+ content-end (length new)))))
        (t
         (let* ((line-end (string-line-end text position))
                (new (format nil "~%* ~:[~;TODO ~]" todo)))
           (org-replace-chars buffer line-end line-end new :point (+ line-end (length new)))))))))

(defun org-renumber-edit (text item number)
  "The edit, a list of one (FROM TO NEW), giving the numbered ITEM of TEXT the
NUMBER, its . or ) kept; NIL for an item with no number."
  (let* ((line (org-line-at text (org-node-begin item)))
         (at (+ (org-node-begin item) (- (length line) (length (string-left-trim " 	" line)))))
         (digits (or (position-if-not #'digit-char-p text :start at) (length text))))
    (when (> digits at)
      (list (list at digits (princ-to-string number))))))

(define-command org-meta-return ()
  "A new item or headline after what point is in and all under it: in a list,
an item of the same kind after the one point is in and all under it (its
bullet, the next number, [ ] when it has a checkbox); past the list, in a
section's text or on a headline, a headline of the section's level after its
subtree."
  (org-insert-heading nil))

(define-command org-insert-todo-heading ()
  "A new TODO headline of this level after this subtree."
  (org-insert-heading t))

(defun install-org-editing ()
  "Bind Org's keys for TODO states and new headlines (M-h M-j M-k M-l, shared with tables, are bound by INSTALL-ORG-TABLES)."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-keys keymap '(("C-c C-t" org-todo) ("S-<right>" org-shiftright) ("S-<left>" org-shiftleft)
                          ("C-c C-x C" org-cancel-task)
                          ("C-c C-q" org-set-tags-command) ("C-c C-z" org-add-note) ("C-c C-w" org-refile)
                          ("M-RET" org-meta-return) ("M-S-RET" org-insert-todo-heading)
                          ("C-c C-x C-u" org-dblock-update)
                          ("C-c C-x x" org-dynamic-block-insert-dblock)))))

;;; Saving a note changed for you

(defun org-save-soon (buffer)
  "Save BUFFER now, or, while a save of it is under way, once that is done
(ORG-SAVE-AGAIN), so that the change just made is saved too."
  (if (tuatara.io:save-of buffer)
      (setf (tuatara.buffer:buffer-local buffer 'org-save-again) t)
      (save-buffer-with-care buffer)))

(defun org-save-again (buffer)
  "After BUFFER was saved: saved once more when a change came while it was
being saved (ORG-SAVE-SOON)."
  (when (tuatara.buffer:buffer-local buffer 'org-save-again)
    (setf (tuatara.buffer:buffer-local buffer 'org-save-again) nil)
    (when (tuatara.buffer:buffer-modified-p buffer) (org-save-soon buffer))))

(pushnew 'org-save-again *after-save-functions*)

(defun org-save-changed (buffer was-modified &key existing)
  "Save BUFFER after a command changed it for you (an entry filed into it or
taken out, a clock's line): an agenda file as the agenda saves one
\(*ORG-AGENDA-AUTOSAVE-FILES*), any other file when it had no changes of its
own before (WAS-MODIFIED false, TUATARA.IO:CHANGES-UNSAVED-P: a change a save
under way writes is not your own); once that save is done (ORG-SAVE-SOON). With EXISTING, only while its file is still there: one
taken away meanwhile is not made again."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (cond ((null file))
          ((and existing (not (eq (tuatara.io:path-kind file) :regular))))
          ((org-agenda-autosave-file-p buffer) (org-agenda-autosave buffer))
          ((not was-modified) (org-save-soon buffer)))))
