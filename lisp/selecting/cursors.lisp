;;;; lisp/selecting/cursors.lisp --- Cursors in a column, and commands run at every one of them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A split of cursors (split.lisp) holds a place for each cursor: where it is
;;;; and what it selects. Motions and selections run once at every cursor, each
;;;; kept on its own line; deleting, copying and pasting are one change at them
;;;; all; typing is written at every cursor as at the places of f.

(in-package #:tuatara.defaults)

;;; A cursor

(defun cursor-selecting-p (place)
  "True when the cursor of PLACE has a selection."
  (or (place-line place) (place-anchored place)))

(defun cursor-position (buffer place)
  "Where the cursor of PLACE in BUFFER is."
  (multiple-value-bind (s e) (place-bounds buffer place)
    (cond ((place-line place) (min e (+ s (place-offset place))))
          ((place-at-end place) e)
          (t s))))

(defun cursor-anchor (buffer place)
  "Where the selection of PLACE's cursor in BUFFER is anchored, or NIL."
  (when (place-anchored place)
    (multiple-value-bind (s e) (place-bounds buffer place)
      (if (place-at-end place) s e))))

(defun cursor-line-bounds (buffer place)
  "Two values: the start and end of the line of PLACE's cursor in BUFFER, its newline left out."
  (let ((p (cursor-position buffer place)))
    (values (tuatara.buffer:buffer-line-start buffer p) (tuatara.buffer:buffer-line-end buffer p))))

(defun set-cursor (buffer place cursor anchor line memory)
  "Put PLACE's cursor at CURSOR in BUFFER, selecting from ANCHOR, or its whole
LINE, or nothing; MEMORY is what the selecting keys left there."
  (cond (line
         (let ((start (tuatara.buffer:buffer-line-start buffer cursor)))
           (move-place buffer place start (tuatara.buffer:buffer-line-end buffer cursor))
           (setf (place-offset place) (- cursor start))))
        (anchor (move-place buffer place (min cursor anchor) (max cursor anchor)))
        (t (move-place buffer place cursor cursor)))
  (setf (place-anchored place) (and anchor (not line) t)
        (place-at-end place) (and anchor (not line) (> cursor anchor))
        (place-line place) (and line t)
        (place-memory place) memory)
  place)

(defun cursor-state (buffer place)
  "PLACE's cursor in BUFFER as SET-CURSOR takes it back: (PLACE CURSOR ANCHOR LINE MEMORY)."
  (list place (cursor-position buffer place) (cursor-anchor buffer place) (place-line place) (place-memory place)))

(defparameter *cursor-memory* '(select-sexp-state select-pair-state)
  "The buffer-local state the selecting keys leave to grow on the next
press, which each cursor keeps for itself.")

(defun cursor-memory (buffer)
  "What the selecting keys left in BUFFER, for a cursor to keep."
  (mapcar (lambda (name) (tuatara.buffer:buffer-local buffer name)) *cursor-memory*))

(defun set-cursor-memory (buffer memory)
  "Give BUFFER the selecting keys' state a cursor kept, MEMORY."
  (loop for name in *cursor-memory*
        for i from 0
        do (setf (tuatara.buffer:buffer-local buffer name) (nth i memory))))

(defun settle-cursors (split)
  "Fit SPLIT's cursors after they moved: in order, those come to share a line
made one, each place fitted to what its cursor selects, point at the real one."
  (let* ((b (split-buffer split))
         (ordered (sort (map 'list (lambda (place) (cons (cursor-position b place) place)) (split-places split))
                        #'< :key #'car))
         (kept '()) (line nil))
    (loop for (position . place) in ordered
          do (let ((this (tuatara.buffer:buffer-line-start b position)))
               (if (eql this line)
                   (when (eq place (split-primary split)) (setf (split-primary split) (first kept)))
                   (progn (setf line this) (push place kept)))))
    (setf (split-places split) (coerce (nreverse kept) 'simple-vector))
    (loop for place across (split-places split)
          do (cond ((place-line place)
                    (set-cursor b place (cursor-position b place) nil t (place-memory place)))
                   ((not (place-anchored place))
                    (let ((s (place-bounds b place))) (move-place b place s s)))))
    (unless (find (split-primary split) (split-places split))
      (setf (split-primary split) (aref (split-places split) 0)))
    (goto (cursor-position b (split-primary split)))
    (setf (split-text split) (place-text b (split-primary split)))))

(defun deselect-cursors (split)
  "Let go of every cursor's selection, keeping the cursors."
  (let ((b (split-buffer split)))
    (loop for place across (split-places split)
          do (set-cursor b place (cursor-position b place) nil nil nil))
    (settle-cursors split)))

(defun adopt-cursor-places (split)
  "Put every cursor of SPLIT back on its place after the places moved: a
selection keeps its direction, and an empty place is its cursor."
  (let ((b (split-buffer split)))
    (loop for place across (split-places split)
          do (multiple-value-bind (s e) (place-bounds b place)
               (if (= s e)
                   (set-cursor b place s nil nil nil)
                   (let ((back (and (place-anchored place) (not (place-at-end place)))))
                     (set-cursor b place (if back s e) (if back e s) nil nil)))))
    (settle-cursors split)))

(defun resume-cursors (split)
  "Take a split of cursors back to its cursors once the typing at them is
over: each as far into its place as point is into its own."
  (let* ((b (split-buffer split))
         (primary (split-primary split))
         (offset (- (tuatara.buffer:buffer-point b) (place-bounds b primary))))
    (loop for place across (split-places split)
          do (multiple-value-bind (s e) (place-bounds b place)
               (set-cursor b place (max s (min e (+ s offset))) nil nil nil)))
    (setf (split-running split) nil (split-last split) nil)
    (arm-cursors split (if (split-virtual split) *virtual-cursor-keymap* *cursor-keymap*))
    ;; armed after the command's keys were weighed: the next key is weighed as ever
    (setf (editor-transient-fresh *editor*) nil)
    (settle-cursors split)
    (virtual-resume split)
    (echo "~a" (split-hint split))))

;;; Making cursors

(defvar *cursor-keymap* (make-keymap :name "cursors")
  "The keys a split of cursors answers, made from NORMAL state's as it starts.")

(defvar *cursor-run-keymap* (make-keymap :name "cursors' mark run")
  "The keys of a mark run at the cursors, before the cursors' own.")

(defun arm-cursors (split map)
  "Put MAP before every other keymap for the cursors of SPLIT: a key it does
not have ends them, doing its own job."
  (declare (ignore split))
  (let ((*split-keeping* t))
    (set-transient-map *editor* map #'split-keep-p #'split-exit)))

(defun refuse-cursors (n)
  "Refuse N cursors, more than there may be (PLACE-LIMIT of *CURSORS-LIMIT*),
saying why."
  (multiple-value-bind (limit why) (place-limit *cursors-limit*)
    (if (eq why :memory)
        (error "~:d cursors would be more than the memory has room for (~:d)" n limit)
        (error "~:d cursors would pass *cursors-limit* (~:d)" n limit))))

(defun cursors-in-sight-p (n)
  "True when only those in sight of N cursors are to be made, past the most
there may be (PAST-LIMIT-CHOICE of *CURSORS-LIMIT*, which may ask)."
  (eq (past-limit-choice n t "cursors" *cursors-limit*) :in-sight))

(defun start-cursors (column)
  "A split of cursors with one cursor, at point; new ones go to COLUMN."
  (when *split* (dissolve-split :quiet t))
  (build-cursor-keymaps)
  (let* ((b (current-buffer))
         (split (make-split b)))
    (setf (tuatara.buffer:buffer-mark-active-p b) nil)
    (fresh-selection b)
    (let ((first (make-place b (point) (point))))
      (setf (split-places split) (vector first)
            (split-primary split) first
            (split-cursors split) t
            (split-column split) column
            (split-scope split) "a column"
            (split-tick split) (tuatara.buffer:buffer-content-number b)
            *split* split))
    (arm-cursors split *cursor-keymap*)
    split))

(defun add-cursors (split spots)
  "Add a cursor to SPLIT at each of SPOTS, noting them for
\\<*cursor-keymap*>\\[drop-last-cursor]."
  (let* ((b (split-buffer split))
         (added (mapcar (lambda (spot) (make-place b spot spot)) spots)))
    (setf (split-places split) (concatenate 'simple-vector (split-places split) added)
          (split-order split) (append (reverse added) (split-order split)))
    (settle-cursors split)
    (message "~a" (split-hint split))))

(defun cursor-spots (buffer from column n above)
  "The N positions at COLUMN on the screen on the lines after FROM's, or
before it when ABOVE, nearest first (TUATARA.DISPLAY:BUFFER-MOVE-TO-SCREEN-COLUMN,
as moving a line down finds them); a shorter line gives its end. The lines are those a
window shows: the text a fold hides is passed over with the line whose end it
hides. Refuses when there are not N lines that way; the empty line after the
last newline is not one."
  (let ((size (tuatara.buffer:buffer-size buffer))
        (line (tuatara.display:visible-line-start buffer from))
        (way (if above "above" "below"))
        (spots '()))
    (dotimes (i n (nreverse spots))
      (let ((next (if above
                      (and (plusp line) (tuatara.display:visible-line-start buffer (tuatara.buffer:buffer-forward-char buffer line -1)))
                      (let ((end (tuatara.display:visible-line-end buffer line)))
                        (and (< end size) (tuatara.buffer:buffer-forward-char buffer end))))))
        (when (and next (not above) (= next size)) (setf next nil))
        (unless next
          (error (if spots (format nil "Only ~d line~:p ~a" (length spots) way) (format nil "No line ~a" way))))
        (setf line next)
        (push (tuatara.display:buffer-move-to-screen-column buffer line column) spots)))))

(defun selected-lines-span (buffer)
  "The lines the selection of BUFFER covers, as (START . END): from the start
of the first to after the newline of the last. A visual-line selection, or
a rectangle, takes every line it touches; another one ending at a line's
start does not take that line. NIL without a selection."
  (when (tuatara.buffer:buffer-mark-active-p buffer)
    (multiple-value-bind (s e) (tuatara.buffer:buffer-region buffer)
      (if (or (line-selection-anchor buffer) (rectangle-live-p buffer))
          (cons (tuatara.buffer:buffer-line-start buffer s)
                (let ((end (tuatara.buffer:buffer-line-end buffer e)))
                  (if (< end (tuatara.buffer:buffer-size buffer)) (tuatara.buffer:buffer-forward-char buffer end) end)))
          (whole-lines-span buffer s e)))))

(defun span-lines (buffer start end cap)
  "Two values: how many lines of BUFFER start from START, a line start, below
END, a line start or the text's end; and true when that is the exact count.
While the lines of a large file are still being counted, they are read here,
and the reading stops once there are more than CAP: the count is then more
than CAP, not exact."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (tail (if (and (> end start) (/= 10 (tuatara.text:rope-octet rope (1- end)))) 1 0)))
    (if (tuatara.text:rope-indexed-p rope)
        (values (+ (- (tuatara.text:byte->line rope end) (tuatara.text:byte->line rope start)) tail) t)
        (let ((lines 0) (pos start) (block (make-array 65536 :element-type '(unsigned-byte 8))))
          (loop while (and (< pos end) (<= lines cap))
                do (let ((to (min end (+ pos 65536))))
                     (tuatara.text:rope-copy-into rope pos to block 0)
                     (incf lines (count 10 block :end (- to pos)))
                     (setf pos to)))
          (values (+ lines tail) (>= pos end))))))

(defun line-spots (buffer start end column here rows)
  "Where a cursor goes on each of the ROWS lines of BUFFER from START, a line
start, below END, but the line starting at HERE: at COLUMN on the screen
\(TUATARA.DISPLAY:BUFFER-MOVE-TO-SCREEN-COLUMN), or at the line's end where it
is shorter; top first. The lines are read a block at a
time, or, when they are long, each found by its number."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (if (and (tuatara.text:rope-indexed-p rope) (> (- end start) (* 4096 (max 1 rows))))
        (loop with first = (tuatara.text:byte->line rope start)
              for i from 0 below rows
              for line = (tuatara.text:line->byte rope (+ first i))
              unless (= line here) collect (tuatara.display:buffer-move-to-screen-column buffer line column))
        (let ((spots '()))
          (apply #'tuatara.text:map-line-slices
                 (lambda (line s e start-column end-column line-end)
                   (declare (ignore s start-column end-column line-end))
                   (unless (= line here) (push e spots)))
                 rope start end column column (tuatara.display:buffer-slice-keys buffer))
          (nreverse spots)))))

(defun cursors-on-selected-lines (buffer)
  "With more than one line selected in BUFFER, put a cursor on every one of
them, at point's column (at each line's start for a visual-line selection), the
real one on point's line; past the most cursors there may be (PLACE-LIMIT),
those in sight, the others written in the background once a verb is done
(HOLD-VIRTUAL-CURSORS), or all of them when the question past the limit is
answered yes (PAST-LIMIT-CHOICE). The lines are counted before a cursor is
made. NIL, doing nothing, when no more than a line is selected."
  (let ((span (selected-lines-span buffer)))
    (when span
      (destructuring-bind (start . end) span
        (multiple-value-bind (rows exact) (span-lines buffer start end (place-count-cap *cursors-limit*))
          (when (> rows 1)
            (let ((column (if (line-selection-anchor buffer) 0 (tuatara.display:buffer-screen-column buffer (point))))
                  (here (point-line buffer)))
              (if (eq (past-limit-choice rows exact "cursors" *cursors-limit*) :all)
                  (line-cursors buffer start end column here rows)
                  (message "~a" (hold-virtual-cursors buffer start end column here rows exact)))
              t)))))))

(defun point-line (buffer)
  "The start of the line point is on in BUFFER, the line before when point is
on the empty line after the last newline."
  (let ((p (tuatara.buffer:buffer-point buffer)) (size (tuatara.buffer:buffer-size buffer)))
    (tuatara.buffer:buffer-line-start
     buffer (if (and (= p size) (plusp p) (= p (tuatara.buffer:buffer-line-start buffer p)))
                (tuatara.buffer:buffer-forward-char buffer p -1)
                p))))

(defun line-cursors (buffer start end column here rows)
  "Put a cursor on each of the ROWS lines of BUFFER from START, a line start,
below END, at COLUMN on the screen or at a shorter line's end, the real one on
the line starting at HERE."
  (let ((spots (line-spots buffer start end column here rows)))
    (goto (tuatara.display:buffer-move-to-screen-column buffer here column))
    (add-cursors (start-cursors column) spots)))

(defun cursors-here ()
  "The split of cursors in force in this buffer, or NIL."
  (and (split-here-p) (split-cursors *split*) *split*))

(define-command add-cursor-below ()
  "Add a cursor on the line below, at the same column: the first press makes
cursors of point and the line below, each one after adds one below the last,
and a count adds that many (negative, above). A shorter line gets its cursor
at its end. With lines selected (\\<normal>\\[select-lines],
\\[start-selection], or a rectangle), the first press puts a cursor on every one
of them instead: at each line's start for \\[select-lines], else at
point's column; the lines are counted first. Past *CURSORS-LIMIT* (or
what the memory has room for), lines selected or a count such as
\\[universal-argument] 50000 \\[add-cursor-below], the cursors in sight
stand, and the others are written in the background as each verb is done at
those: then \\<normal>\\[backward-char] \\[forward-char] \\[beginning-of-line]
\\[end-of-line] move them all alike, those out of sight keeping one column,
\\<*virtual-cursor-keymap*>\\[cursors-insert]
\\[cursors-append] \\[cursors-insert-line]
\\[cursors-append-line] \\[cursors-change] and
\\[cursors-delete] act at them, their other keys are refused, saying what
works, and undo ends them.
Every cursor then does what the real one does: \\<normal>\\[backward-char]
\\[forward-char] \\[forward-word] \\[backward-word] \\[forward-sexp]
\\[backward-sexp] \\[beginning-of-line] \\[end-of-line] move,
\\[start-selection] \\[select-lines] \\[select-word] and the
other \\`m' keys select, \\[tuatara-insert] \\[tuatara-insert-after]
\\[tuatara-insert-line-start] \\[tuatara-insert-line-end]
\\[tuatara-change] \\[tuatara-open-below] \\[tuatara-open-above] type at all of
them, \\[delete-selection-or-char] \\[kill-line] \\[copy-selection-or-char] \\[yank-over-selection]
delete, kill, copy and paste, a delimiter wraps, \\[mark-run-toggle]
starts a mark run, \\[undo] \\[redo] undo, \\[repeat] repeats, \\[split-selection]
searches. \\[add-cursor-above] adds above,
\\<*cursor-keymap*>\\[drop-last-cursor] takes back the cursor added
last, \\[align-cursors] lines them up. A cursor never leaves its
line, and cursors that come to share one become one; any other key ends them and
does its own job. \\[split-quit] lets go of the selections, and then ends
them."
  (let ((count (prefix-argument)))
    (unless (and (not (cursors-here)) (cursors-on-selected-lines (current-buffer)))
      (add-cursors-beside (max 1 (abs (or count 1))) (and count (minusp count))))))

(defun add-cursors-beside (n above)
  "Add N cursors below the last, or above the first with ABOVE, at the
cursors' column, from point's line when there are no cursors yet; past the
most there may be, those in sight (COUNTED-CURSORS-IN-SIGHT)."
  (let* ((b (current-buffer))
         (split (cursors-here))
         (column (if split (split-column split) (tuatara.display:buffer-screen-column b (point)))))
    ;; counted before a line is looked at
    (if (cursors-in-sight-p (+ n (if split (length (split-places split)) 1)))
        (counted-cursors-in-sight b split n above)
        (let* ((places (and split (split-places split)))
               (from (if split
                         (cursor-position b (aref places (if above 0 (1- (length places)))))
                         (point))))
          (add-cursors (or split (start-cursors column)) (cursor-spots b from column n above))))))

(defun counted-cursors-in-sight (buffer split n above)
  "Past the most cursors there may be (PLACE-LIMIT), the N cursors a count asks
for below the last of SPLIT's cursors, or above the first with ABOVE, or below
or above point's line with no cursors yet: held as the cursors of selected lines
are (HOLD-VIRTUAL-CURSORS), those in sight standing and the others written in
the background once a verb is done, SPLIT's cursors taken in. Refused when
there are not N lines that way, the empty line after the last newline not one,
and when SPLIT's cursors do not stand alone on lines one after another. Counts
BUFFER's lines first, as goto-line does."
  (let* ((places (and split (sort (copy-seq (split-places split)) #'< :key (lambda (p) (cursor-position buffer p)))))
         (lines (and split (map 'list (lambda (p) (tuatara.buffer:buffer-line-number buffer (cursor-position buffer p))) places)))
         (total (+ n (if split (length places) 1))))
    (when (and split (not (and (every (lambda (p) (multiple-value-bind (s e) (place-bounds buffer p) (= s e))) places)
                               (loop for (a b) on lines while b always (= b (1+ a))))))
      (refuse-cursors total))
    (let* ((rope (tuatara.buffer:buffer-text buffer))
           (size (tuatara.buffer:buffer-size buffer))
           (here (tuatara.buffer:buffer-line-start
                  buffer (if split (cursor-position buffer (split-primary split)) (point-line buffer))))
           (top (if split (first lines) (tuatara.buffer:buffer-line-number buffer here)))
           (bottom (if split (car (last lines)) top))
           (final (tuatara.buffer:buffer-line-number
                   buffer (if (and (plusp size) (= (tuatara.text:rope-octet rope (1- size)) 10)) (1- size) size)))
           (room (if above top (- final bottom))))
      (when (< room n)
        (if (plusp room)
            (error "Only ~:d line~:p ~a" room (if above "above" "below"))
            (error "No line ~a" (if above "above" "below"))))
      (let* ((from (if above (- top n) top))
             (to (if above bottom (+ bottom n)))
             (start (tuatara.buffer:buffer-goto-line buffer from))
             (end (if (< to (tuatara.text:rope-lines rope)) (tuatara.buffer:buffer-goto-line buffer (1+ to)) size)))
        (message "~a" (hold-virtual-cursors buffer start end
                                            (if split (split-column split) (tuatara.display:buffer-screen-column buffer (point)))
                                            here (1+ (- to from)) t))))))

(define-command add-cursor-above ()
  "Add a cursor on the line above, at the same column: the first press makes
cursors of point and the line above, each one after adds one above the
first, and a count adds that many (negative, below). With lines selected, the
first press puts a cursor on every one of them, as
\\<normal>\\[add-cursor-below] does. See add-cursor-below."
  (let ((count (prefix-argument)))
    (cond ((and (not (cursors-here)) (cursors-on-selected-lines (current-buffer))))
          ((and count (minusp count))
           (setf (editor-prefix-argument *editor*) (- count))
           (add-cursor-below))
          (t (add-cursors-beside (max 1 (or count 1)) t)))))

(define-command drop-last-cursor ()
  "Take back the cursor added last, keeping the others; the real cursor is
never taken back, and \\<*cursor-keymap*>\\[split-quit] ends the cursors."
  (let* ((split (live-split))
         (standing (let ((set (make-hash-table :test 'eq)))
                     (loop for place across (split-places split) do (setf (gethash place set) t))
                     set))
         (candidates (remove-if-not (lambda (place) (and (gethash place standing) (not (eq place (split-primary split)))))
                                    (split-order split)))
         (place (or (first candidates)
                    (find-if (lambda (place) (not (eq place (split-primary split))))
                             (reverse (coerce (split-places split) 'list))))))
    (if (null place)
        (message "Only the real cursor is left; \\<*cursor-keymap*>\\[split-quit] ends the split")
        (progn (setf (split-order split) (remove place (split-order split))
                     (split-places split) (remove place (split-places split)))
               (settle-cursors split)
               (echo "~a" (split-hint split))))))

(define-command align-cursors ()
  "Put every cursor at the real cursor's column, or its line's end where the
line is shorter; with cursors selecting, wrap the selections in = as any
delimiter does."
  (let* ((split (live-split)) (b (split-buffer split)))
    (if (some #'cursor-selecting-p (split-places split))
        (wrap-places split #\=)
        (let ((column (tuatara.display:buffer-screen-column b (point))))
          (loop for place across (split-places split)
                unless (eq place (split-primary split))
                  do (multiple-value-bind (start) (cursor-line-bounds b place)
                       (set-cursor b place (tuatara.display:buffer-move-to-screen-column b start column) nil nil nil)))
          (setf (split-column split) column)
          (settle-cursors split)))))

;;; Cursors past the limit: those in sight held, the others written in the background

(defvar *virtual-cursor-keymap* (make-keymap :name "cursors past the limit")
  "The keys cursors past their limit answer, made from the cursors' own:
\\<*virtual-cursor-keymap*>\\[cursors-insert] \\[cursors-append]
\\[cursors-insert-line] \\[cursors-append-line] type,
\\[cursors-change] changes and \\[cursors-delete]
deletes, and \\[split-quit] ends them.")

(defparameter *virtual-cursor-verbs*
  '(cursors-insert cursors-append cursors-insert-line
    cursors-append-line cursors-change cursors-delete)
  "The cursors' verbs that cursors past their limit answer too, the cursors out
of sight written in the background.")

(defparameter *virtual-cursor-motions* '(backward-char forward-char beginning-of-line end-of-line)
  "The NORMAL state motions cursors past their limit follow, those out of sight
too: each moves every cursor alike, so that the rows out of sight keep one
column (\\<*virtual-cursor-keymap*>\\[cursors-move-past-limit] and the rest).")

(define-command cursors-move-past-limit ()
  "Move cursors past their limit, those out of sight with those in sight, by
the NORMAL state motion of the keys pressed: a character right or left (with
a count, that many), or to the start or the end of the line. Those in sight
move as cursors do; those out of sight keep one column, moved as theirs, and
what is done there on a line shorter than that column is done at its end."
  (let* ((split (live-split))
         (b (split-buffer split))
         (virtual (split-virtual split))
         (command (lookup-key *tuatara-normal-keymap* *this-command-keys*))
         ;; a negative count moves the other way, in sight and out of it alike
         (n (prefix)))
    (unless (member command *virtual-cursor-motions*)
      (error "~a does not move cursors past their limit"
             (tuatara.frontend:key-sequence-description *this-command-keys*)))
    (if (eq command 'end-of-line)
        (cursors-line-end)
        (run-at-cursors split command (prefix-argument)))
    (when virtual
      (let* ((column (virtual-column virtual))
             (new (ecase command
                    (end-of-line :end)
                    (beginning-of-line 0)
                    ;; right by N (left when negative); from a line's end, leftwards from the
                    ;; column the cursor here has come to, rightwards staying at the end
                    ((forward-char backward-char)
                     (let ((right (if (eq command 'forward-char) n (- n))))
                       (cond ((not (eq column :end)) (max 0 (+ column right)))
                             ((minusp right) (max 0 (tuatara.display:buffer-screen-column b (tuatara.buffer:buffer-point b))))
                             (t :end)))))))
        (setf (virtual-column virtual) new
              (virtual-at-end virtual) (eq new :end)
              (virtual-mode virtual) :start
              (virtual-pad virtual) nil
              (virtual-tab-width virtual) (tuatara.display:tab-width-of b))
        (unless (eq new :end)
          (setf (virtual-left virtual) new (virtual-right virtual) new))))))

(define-command cursors-past-limit-refuse ()
  "Say what cursors past their limit answer, leaving them as they are: those out
of sight move only alike (\\<*virtual-cursor-keymap*>\\[cursors-move-past-limit]
and the line's start and end), and a motion that would take each its own way,
or a verb they have not, is refused."
  (message "Cursors past the limit move only alike: \\<*tuatara-normal-keymap*>\\[backward-char], \\[forward-char], \\[beginning-of-line], \\[end-of-line]; \\<*virtual-cursor-keymap*>\\[cursors-insert] \\[cursors-append] \\[cursors-insert-line] \\[cursors-append-line] \\[cursors-change] \\[cursors-delete] edit; \\[split-quit] ends them"))

(defun start-lines-job (buffer virtual kind &key (text "") (back 0) (forward 0) skip joins)
  "Write at the rows of VIRTUAL in BUFFER, but those the places of SKIP (a
vector of (START . END)) reach into, in the background on all but two cores:
KIND :TYPE puts TEXT at each row's place (TYPED-ROW, BACK and FORWARD
characters beside it deleted too), :DELETE deletes the place; a row where that
would take part of the text a fold hides, but not all of it, is left as it is.
One change for undo, joining JOINS's (REWRITE), said as every job at many
places says it (JOB-WORDS, JOB-REPORT). Returns the job, or :QUEUED when
another job runs or waits in BUFFER, or its lines are still being counted: it
then waits its turn, its rows and SKIP's places followed."
  (let ((top (virtual-now buffer (virtual-top virtual)))
        (bottom (virtual-now buffer (virtual-bottom virtual)))
        (left (virtual-left virtual)) (right (virtual-right virtual))
        (at-end (virtual-at-end virtual)) (pad (virtual-pad virtual))
        (what (job-words kind :doing "the lines out of sight"))
        (refused (list 0))
        (hidden (list 0)))
    (flet ((start (top bottom skip)
             (start-rows-job buffer kind top bottom left right (rows-between buffer top bottom)
                             :text text :pad pad :at-end at-end :back back :forward forward
                             :skip (held-lines buffer skip) :refused refused :hidden hidden :joins joins
                             :tab-width (virtual-tab-width virtual)
                             :what what :verb (job-words kind :verb "the lines out of sight")
                             :leaves "the lines out of sight are as they were"
                             :report (job-report kind (lambda (n) (format nil "~:d more line~:p, out of sight," n))
                                                 "the lines out of sight" :refused refused :hidden hidden)
                             :announce :none)))
      (if (or (job-busy-p buffer) (queued-in buffer) (lines-uncounted-p buffer))
          (let ((followed (spans-followed buffer skip)))
            (queue-if-busy buffer what (list (cons top :before) (cons bottom :before))
                           (lambda (top bottom) (start top bottom (funcall followed)))
                           :shade (block-shade buffer left (max right (1+ left)) :wide at-end
                                                            :tab-width (virtual-tab-width virtual))
                           :counted t)
            :queued)
          (start top bottom skip)))))

(defun hold-virtual-cursors (buffer start end column here rows exact)
  "Put a cursor on each of the ROWS lines of BUFFER from START, a line start,
below END (more, unless EXACT), at COLUMN, the real one on the line starting at
HERE: more than there may be (PLACE-LIMIT of *CURSORS-LIMIT*), so those in
sight are held, as many as there may be, and the others are written in the
background once a verb is done at those held. Returns what to say."
  (multiple-value-bind (limit why) (place-limit *cursors-limit*)
    (let ((version (tuatara.buffer:buffer-version buffer))
          (bottom (tuatara.buffer:buffer-line-start buffer (max start (1- end)))))
      (goto (tuatara.display:buffer-move-to-screen-column buffer here column))
      (let ((split (start-cursors column)))
        (setf (split-virtual split)
              (make-virtual :kind :rows
                            :top (tuatara.text:make-anchor version start :bias :before)
                            :bottom (tuatara.text:make-anchor version bottom :bias :before)
                            :column column :left column :right column :tab-width (tuatara.display:tab-width-of buffer)
                            :count (if exact rows limit) :exact exact :limit limit :setting why))
        (virtual-fill split)
        (settle-cursors split)
        (arm-cursors split *virtual-cursor-keymap*)
        (split-hint split)))))

(defun virtual-verb-place (split &key (shift 0) line-start at-end (width 0) (mode :start))
  "For cursors past their limit, where the verb about to run acts on each row
out of sight: the cursors' column moved by SHIFT, or the line's start, or with
AT-END its end, WIDTH columns from there emptied first when MODE is :CLEAR."
  (let ((virtual (split-virtual split)))
    (when virtual
      (let ((column (virtual-column virtual)))
        (cond ((or at-end (eq column :end))
               (setf (virtual-at-end virtual) t (virtual-left virtual) 0 (virtual-right virtual) 0))
              (t (let ((left (if line-start 0 (+ column shift))))
                   (setf (virtual-at-end virtual) nil (virtual-left virtual) left
                         (virtual-right virtual) (+ left width)))))
        (setf (virtual-pad virtual) nil (virtual-mode virtual) mode)))))

(defun virtual-resume (split)
  "After the typing at the cursors of SPLIT past their limit: the column the
cursors stand at now, for the next verb."
  (let ((virtual (split-virtual split)) (b (split-buffer split)))
    (when virtual
      (let ((column (if (virtual-at-end virtual)
                        :end
                        (tuatara.display:buffer-screen-column b (tuatara.buffer:buffer-point b)))))
        (setf (virtual-column virtual) column (virtual-mode virtual) :start (virtual-pad virtual) nil
              (virtual-at-end virtual) (eq column :end) (virtual-tab-width virtual) (tuatara.display:tab-width-of b))
        (unless (eq column :end)
          (setf (virtual-left virtual) column (virtual-right virtual) column))))))

(defun hold-virtual-block (buffer top bottom left right rows exact)
  "\\<normal>\\[tuatara-change] on a rectangle of BUFFER taller than there may be
cursors (*RECTANGLE-CURSORS-LIMIT*): the lines from the one starting at TOP to the
one starting at BOTTOM, ROWS of them (more, unless EXACT), between the columns
LEFT and RIGHT. The block goes to the rectangle store kept by where it is; the
rows in sight are emptied and hold a cursor each, the real one on point's, and
typing begins; the others are emptied and written in the background when it
ends."
  (multiple-value-bind (limit why) (place-limit *rectangle-cursors-limit*)
    (let ((version (tuatara.buffer:buffer-version buffer))
          (here (point-line buffer))
          (split (make-split buffer))
          (before (tuatara.buffer:buffer-undo-stack buffer)))
      (setf *killed-rectangle* (rectangle-source-of buffer)
            (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (when *split* (dissolve-split :quiet t))
      (build-cursor-keymaps)
      (fresh-selection buffer)
      (setf (split-cursors split) t (split-column split) left (split-scope split) "a column"
            (split-tick split) (tuatara.buffer:buffer-content-number buffer)
            (split-virtual split)
            (make-virtual :kind :rows
                          :top (tuatara.text:make-anchor version top :bias :before)
                          :bottom (tuatara.text:make-anchor version bottom :bias :before)
                          :column left :left left :right right :pad t :mode :clear
                          :tab-width (tuatara.display:tab-width-of buffer)
                          :count (if exact rows limit) :exact exact :limit limit :setting why))
      ;; point's row first, so that the real cursor is where it was
      (let ((span (first (virtual-spans buffer (split-virtual split) here here))))
        (when span
          (let ((place (make-place buffer (car span) (cdr span))))
            (setf (split-places split) (vector place))
            (catch-up-places split (list place)))))
      (virtual-fill split :prepare t)
      (let ((primary (or (find-if (lambda (place)
                                    (= here (tuatara.buffer:buffer-line-start buffer (place-bounds buffer place))))
                                  (split-places split))
                         (aref (split-places split) 0))))
        (setf (split-primary split) primary
              (split-did split) :changed
              *split* split)
        (goto (place-bounds buffer primary))
        (setf (split-text split) "")
        (enter-split-edit split nil :start :since before)))))

;;; Running a key at every cursor

(defun replay-at (split place command arg edit)
  "Run COMMAND with prefix ARG at PLACE's cursor, as the real cursor would:
point, the selection and the selecting keys' state are the cursor's while it
runs, and what it leaves is kept on the cursor's line. EDIT says COMMAND
changes text: the cursor is left where COMMAND leaves it, its selection let go."
  (let* ((b (split-buffer split))
         (cursor (cursor-position b place)))
    (multiple-value-bind (start end) (cursor-line-bounds b place)
      (let ((anchor (cursor-anchor b place)))
        (goto cursor)
        (cond ((and edit (place-line place))
               (setf (tuatara.buffer:buffer-mark b) start (tuatara.buffer:buffer-mark-active-p b) t)
               (goto end))
              (anchor (setf (tuatara.buffer:buffer-mark b) anchor (tuatara.buffer:buffer-mark-active-p b) t))
              (t (setf (tuatara.buffer:buffer-mark-active-p b) nil)))
        (set-cursor-memory b (place-memory place))
        (setf (editor-last-command *editor*) (split-last split)
              (editor-this-command *editor*) command
              (editor-prefix-argument *editor*) arg)
        (funcall (find-command command))
        (let* ((p (point))
               (mark (and (tuatara.buffer:buffer-mark-active-p b) (not edit) (tuatara.buffer:buffer-mark b)))
               (beyond (and mark (or (and (< p start) (< mark start)) (and (> p end) (> mark end))))))
          (set-cursor b place
                      (cond ((and edit (place-line place)) (cursor-position b place))
                            (edit p)
                            (beyond cursor)
                            (t (max start (min end p))))
                      (cond (beyond cursor) (mark (max start (min end mark))))
                      (and (not mark) (not edit) (place-line place))
                      (cursor-memory b)))))))

(defun run-at-cursors (split command arg &optional edit)
  "Run COMMAND with prefix ARG at every cursor of SPLIT. Every cursor runs it or
none does: when it fails at one, all are put back, and an EDIT (a command
that changes text) is taken back everywhere. The changes are one change for
undo. A key the command asks for is asked once. Only the last cursor's
messages are kept. Refused while a cursor is outside the part of the buffer
shown, as when cursors were made before it was narrowed."
  (let* ((b (split-buffer split))
         (places (split-places split))
         (states (map 'list (lambda (place) (cursor-state b place)) places))
         (since (tuatara.buffer:buffer-undo-stack b))
         (last (aref places (1- (length places))))
         (replay (cons nil nil))
         (this (editor-this-command *editor*))
         (done nil))
    (when edit (check-writable (split-buffer split)))
    (when (some (lambda (place) (let ((p (cursor-position b place))) (/= p (tuatara.buffer:within-narrowing b p)))) places)
      (error "Some cursors are outside the part of ~a shown: widen shows them again" (tuatara.buffer:buffer-name b)))
    (setf (split-repeat split) (list :run command arg edit))
    (unwind-protect
         (let ((*read-key-replay* replay))
           (loop for place across places
                 do (setf (car replay) (reverse (cdr replay)))
                    (let ((*message-log-max* (and (eq place last) *message-log-max*)))
                      (replay-at split place command arg edit)))
           (setf done t))
      (setf (editor-this-command *editor*) this)
      (unless done
        (when edit (tuatara.buffer:undo-back-to b since))
        (dolist (state states) (apply #'set-cursor b state))))
    (when edit
      (tuatara.buffer:merge-undo-groups b since)
      (setf (split-did split) :edited))
    (when (and (split-running split) (not edit))
      (push states (split-history split))
      (setf (split-redo split) '()))
    (setf (tuatara.buffer:buffer-mark-active-p b) nil
          (split-last split) command)
    (settle-cursors split)))

(defparameter *cursor-replayed-commands*
  '(backward-char forward-char backward-word forward-word backward-sexp forward-sexp beginning-of-line
    select-word select-word-backward select-symbol select-symbol-backward
    select-sentence select-sentence-backward select-inside-pair select-pair
    select-sexp-inside select-sexp)
  "The NORMAL state commands every cursor runs at its own place.")

(defparameter *cursor-run-replayed-commands*
  '(select-word select-word-backward select-symbol select-symbol-backward
    select-sentence select-sentence-backward mark-run-left mark-run-right
    mark-run-line-start mark-run-line-end mark-run-exchange)
  "The mark run's commands every cursor runs while the cursors are in a run.")

(define-command cursors-replay ()
  "Run the command of the keys just pressed at every cursor: the NORMAL state
motion or selection they are on, or in a mark run, the run's."
  (let* ((split (live-split))
         (keys *this-command-keys*)
         (run (and (split-running split) (lookup-key *mark-run-keymap* keys)))
         (command (if (member run *cursor-run-replayed-commands*) run (lookup-key *tuatara-normal-keymap* keys))))
    (unless (and command (or (eq command run) (member command *cursor-replayed-commands*)))
      (error "~a is not run at every cursor" (tuatara.frontend:key-sequence-description keys)))
    (run-at-cursors split command (prefix-argument))))

;;; What the cursors do

(define-command cursors-select ()
  "Start a selection at every cursor, where each is: the motions then grow them."
  (let* ((split (live-split)) (b (split-buffer split)))
    (loop for place across (split-places split)
          do (let ((cursor (cursor-position b place))) (set-cursor b place cursor cursor nil nil)))
    (settle-cursors split)))

(define-command cursors-select-lines ()
  "Select every cursor's whole line, or let go where every cursor has."
  (let* ((split (live-split)) (b (split-buffer split))
         (all (every #'place-line (split-places split))))
    (loop for place across (split-places split)
          do (set-cursor b place (cursor-position b place) nil (not all) nil))
    (settle-cursors split)))

(defun type-at-cursors (split where)
  "Put every cursor of SPLIT at WHERE on its line (:START or :END) and type at them all."
  (let ((b (split-buffer split)))
    (if (eq where :end) (virtual-verb-place split :at-end t) (virtual-verb-place split :line-start t))
    (loop for place across (split-places split)
          do (multiple-value-bind (start end) (cursor-line-bounds b place)
               (set-cursor b place (if (eq where :end) end start) nil nil nil)))
    (settle-cursors split)
    (enter-split-edit split nil :start)))

(define-command cursors-insert ()
  "Type before every cursor, or its selection: what is typed at one appears
at all. \\<insert>\\[tuatara-normal] comes back to the cursors."
  (let ((split (live-split)))
    (virtual-verb-place split)
    (enter-split-edit split nil :start)))

(define-command cursors-append ()
  "Type after the character under every cursor, or after its selection; a
cursor at its line's end types there."
  (let* ((split (live-split)) (b (split-buffer split)))
    (virtual-verb-place split :shift 1)
    (loop for place across (split-places split)
          do (multiple-value-bind (start end) (cursor-line-bounds b place)
               (declare (ignore start))
               (multiple-value-bind (s e) (place-bounds b place)
                 (declare (ignore s))
                 (set-cursor b place
                             (if (cursor-selecting-p place)
                                 e
                                 (min end (tuatara.buffer:buffer-forward-char b (cursor-position b place))))
                             nil nil nil))))
    (settle-cursors split)
    (enter-split-edit split nil :start)))

(define-command cursors-insert-line ()
  "Type at the start of every cursor's line."
  (type-at-cursors (live-split) :start))

(define-command cursors-append-line ()
  "Type at the end of every cursor's line."
  (type-at-cursors (live-split) :end))

(define-command cursors-line-end ()
  "Put every cursor at the end of its line, keeping its selection."
  (let* ((split (live-split)) (b (split-buffer split)))
    (loop for place across (split-places split)
          do (multiple-value-bind (start end) (cursor-line-bounds b place)
               (declare (ignore start))
               (set-cursor b place end (and (not (place-line place)) (cursor-anchor b place))
                           (place-line place) (place-memory place))))
    (setf (split-last split) 'end-of-line)
    (settle-cursors split)))

(defun cursor-span (buffer place count whole-lines)
  "Three values for what PLACE's cursor acts on: the start and end, and
whether it is a selection: the selection, with WHOLE-LINES a selected line
with its newline, or else COUNT characters from the cursor within its line."
  (let ((cursor (cursor-position buffer place)))
    (multiple-value-bind (start end) (cursor-line-bounds buffer place)
      (cond ((and whole-lines (place-line place))
             (values start (if (< end (tuatara.buffer:buffer-size buffer)) (tuatara.buffer:buffer-forward-char buffer end) end) t))
            ((cursor-selecting-p place)
             (multiple-value-bind (s e) (place-bounds buffer place) (values s e t)))
            (t (let ((far (max start (min end (tuatara.buffer:buffer-forward-char buffer cursor count)))))
                 (values (min cursor far) (max cursor far) nil)))))))

(define-command cursors-change ()
  "Empty every cursor's selection, or the count's characters at every cursor,
then type there; a selection goes on the kill ring as one kill. The emptying
and the typing are one change for undo. When that would take part of the text
a fold hides, but not all of it, nothing is emptied: the fold is shown
instead, and the key pressed again empties."
  (let* ((split (live-split)) (b (split-buffer split)) (since (tuatara.buffer:buffer-undo-stack b)))
    (refuse-negative-past-limit split)
    (if (some #'cursor-selecting-p (split-places split))
        (enter-split-edit split t :start)
        (progn
          (delete-spans b (map 'list (lambda (place) (multiple-value-list (cursor-span b place (prefix) nil)))
                               (split-places split)))
          (virtual-verb-place split :width (max 0 (prefix)) :mode :clear)
          (settle-cursors split)
          (enter-split-edit split nil :start :since since)))))

(defun refuse-negative-past-limit (split)
  "Refuse a negative count when SPLIT has cursors past their limit: those out
of sight change only forward of their column, so the two would differ."
  (when (and (split-virtual split) (minusp (prefix)))
    (error "A negative count does not reach the cursors out of sight: nothing changed")))

(define-command cursors-delete ()
  "Delete every cursor's selection, a selected line with its newline, or the
count's characters at every cursor; one change, the cursors kept. What was
selected goes on the kill ring as one kill. When that would take part of the
text a fold hides, but not all of it, nothing is deleted: the fold is shown
instead, and the key pressed again deletes; a line out of sight where that
would be is left as it is."
  (let* ((split (live-split)) (b (split-buffer split))
         (spans (map 'list (lambda (place) (multiple-value-list (cursor-span b place (prefix) t))) (split-places split)))
         (kills (loop for (s e selected) in spans when selected collect (tuatara.buffer:buffer-string b :start s :end e))))
    (refuse-negative-past-limit split)
    (check-writable (split-buffer split))
    (setf (split-repeat split) (list :verb 'cursors-delete (prefix-argument)))
    (if (and (every (lambda (span) (= (first span) (second span))) spans) (not (split-virtual split)))
        (message "Nothing to delete")
        (progn (delete-spans b spans)
               (when (and kills (not (split-virtual split))) (kill-in-parts kills))
               (setf (split-did split) :deleted)
               (when (and (split-virtual split) (not (eq (virtual-column (split-virtual split)) :end)))
                 (virtual-verb-place split :width (max 0 (prefix)) :mode :clear)
                 (start-virtual-job split :kind :delete))
               (deselect-cursors split)))))

(define-command cursors-copy ()
  "Copy every cursor's selection, or the count's characters at every cursor,
as one kill, one line a cursor; the selections are let go."
  (let* ((split (live-split)) (b (split-buffer split))
         (texts (map 'list (lambda (place)
                             (multiple-value-bind (s e) (cursor-span b place (prefix) t)
                               (region-text b s e :string t)))
                     (split-places split))))
    (if (every (lambda (text) (zerop (length text))) texts)
        (message "Nothing to copy")
        (progn (kill-in-parts texts)
               (message "Copied at ~a" (places-phrase split (length texts)))
               (deselect-cursors split)))))

(define-command cursors-yank ()
  "Paste at every cursor, over its selection: a kill made of as many parts
\(KILL-PARTS), at as many cursors or a rectangle of as many rows, gives each
cursor its own part back, top to bottom; another kill with as many lines as
there are cursors gives each its own line; else each gets the whole kill;
a count pastes that many copies. A selection of lines is replaced as whole
lines: a paste that ends with a newline slots in as complete lines, one that
does not keeps the line's newline. One change. When a selection replaced
would take part of the text a fold hides, but not all of it, nothing is
pasted: the fold is shown instead, and the key pressed again pastes."
  (take-in-clipboard)
  (let* ((split (live-split)) (b (split-buffer split))
         (text (first (editor-kill-ring *editor*)))
         (places (split-places split)))
    (check-writable (split-buffer split))
    (setf (split-repeat split) (list :verb 'cursors-yank (prefix-argument)))
    (cond
      ((or (null text) (zerop (kill-size text)))
        (message "Nothing to paste"))
      ((not (stringp text))
       (message "The last kill (~a) is too large to paste at every cursor" (octets-words (kill-size text))))
      ((> (* (length places) (length text) (max 1 (prefix))) *rewrite-threshold*)
       (message "Pasting the last kill (~a) ~:[~;~:*~:d times ~]at ~a would write ~a, more than is written at once (~a)"
                (octets-words (kill-size text)) (and (> (prefix) 1) (prefix)) (places-phrase split (length places))
                (octets-words (* (length places) (length text) (max 1 (prefix)))) (octets-words *rewrite-threshold*)))
      (t
        (let* ((pieces (or (kill-parts text (length places))
                           ;; another kill with as many lines as there are cursors: a line to each
                           (let ((lines (mapcar (lambda (line) (string-right-trim '(#\Return) line))
                                                (uiop:split-string (if (and (plusp (length text))
                                                                            (char= (char text (1- (length text))) #\Newline))
                                                                       (subseq text 0 (1- (length text)))
                                                                       text)
                                                                   :separator '(#\Newline)))))
                             (and (> (length places) 1) (= (length lines) (length places)) lines))
                           (make-list (length places) :initial-element text)))
               (n (max 0 (prefix)))
               (newline (tuatara.buffer:buffer-newline b))
               (spans (map 'list (lambda (place) (multiple-value-list (cursor-paste-span b place))) places))
               (positions (delete-spans
                           b spans
                           :replacements (loop for place across places
                                               for piece in pieces
                                               for (nil e) in spans
                                               collect (let ((copies (let ((piece (text-for-buffer piece b)))
                                                                       (with-output-to-string (out)
                                                                         (dotimes (i n) (write-string piece out))))))
                                                         (if (and (place-line place) (plusp (length copies))
                                                                  (not (kill-ends-line-p copies))
                                                                  (> e (nth-value 1 (cursor-line-bounds b place))))
                                                             (concatenate 'string copies newline)
                                                             copies))))))
          (when positions
            (loop for place across places
                  for i from 0
                  do (set-cursor b place (aref positions i) nil nil nil)))
          (setf (split-did split) :pasted)
          (settle-cursors split))))))

(defun cursor-paste-span (buffer place)
  "Two values, the start and end of what a paste at PLACE's cursor replaces:
a selected line with its newline, the selection, or nothing at the cursor."
  (cond ((place-line place) (cursor-span buffer place 1 t))
        ((cursor-selecting-p place) (place-bounds buffer place))
        (t (let ((c (cursor-position buffer place))) (values c c)))))

(define-command cursors-kill-line ()
  "Kill from every cursor to the end of its line, as one kill and one change;
a cursor at its line's end kills nothing. When that would take part of the
text a fold hides, but not all of it, nothing is killed: the fold is shown
instead, and the key pressed again kills."
  (let* ((split (live-split)) (b (split-buffer split))
         (spans (map 'list (lambda (place) (list (cursor-position b place) (nth-value 1 (cursor-line-bounds b place))))
                     (split-places split)))
         (texts (loop for (s e) in spans collect (tuatara.buffer:buffer-string b :start s :end e))))
    (check-writable (split-buffer split))
    (setf (split-repeat split) (list :verb 'cursors-kill-line nil))
    (if (every (lambda (text) (zerop (length text))) texts)
        (message "Nothing to kill")
        (progn (delete-spans b spans)
               (kill-in-parts texts)
               (setf (split-did split) :deleted)
               (deselect-cursors split)))))

(defun cursor-line-spans (split)
  "Every cursor's whole line of SPLIT, its newline included, as (START . END)."
  (let ((b (split-buffer split)))
    (map 'list (lambda (place) (let ((c (cursor-position b place))) (whole-lines-span b c c))) (split-places split))))

(define-command cursors-bank ()
  "Bank every cursor's line, or take them all out where every one is banked;
the bank is there for \\<normal>\\[copy-selection-or-char], \\[delete-selection-or-char] and
\\[split-selection] once the cursors end."
  (let* ((split (live-split)) (b (split-buffer split))
         (spans (cursor-line-spans split))
         (unbanking (every (lambda (span) (lines-banked-p b span)) spans)))
    (setf (split-repeat split) (list :verb 'cursors-bank nil))
    (dolist (span spans) (if unbanking (unbank-lines b span) (bank-lines b span)))
    (message "~:[Banked~;Unbanked~] ~a (~d banked in all)" unbanking (places-phrase split (length spans))
             (banked-line-count b))))

(define-command cursors-unbank ()
  "Take every cursor's line out of the bank, leaving every other bank alone."
  (let* ((split (live-split)) (b (split-buffer split)))
    (setf (split-repeat split) (list :verb 'cursors-unbank nil))
    (dolist (span (cursor-line-spans split)) (unbank-lines b span))
    (message "Unbanked at ~a (~d banked in all)" (places-phrase split (length (split-places split)))
             (banked-line-count b))))

(defun open-at-cursors (split above)
  "Open a line below every cursor's line of SPLIT, or ABOVE it, indented as
the mode indents it, and type at them all; the opening is one change."
  (check-writable (split-buffer split))
  (let* ((b (split-buffer split))
         (since (tuatara.buffer:buffer-undo-stack b)))
    (loop for place across (split-places split)
          do (goto (cursor-position b place))
             (if above
                 (progn (goto (tuatara.buffer:buffer-line-start b (point)))
                        (let ((*indent-new-lines* nil)) (newline))
                        (char-motion -1)
                        (indent-typed-line b :group t))
                 (progn (goto (tuatara.buffer:buffer-line-end b (point)))
                        (newline)))
             (set-cursor b place (point) nil nil nil))
    (tuatara.buffer:merge-undo-groups b since)
    (settle-cursors split)
    (enter-split-edit split nil :start)))

(define-command cursors-open-below ()
  "Open a line below every cursor's line and type on all of them."
  (open-at-cursors (live-split) nil))

(define-command cursors-open-above ()
  "Open a line above every cursor's line and type on all of them."
  (open-at-cursors (live-split) t))

(define-command cursors-indent ()
  "Indent every cursor's line as the mode indents it."
  (run-at-cursors (live-split) 'indent-selection-or-line nil t))

(define-command cursors-comment ()
  "Comment or uncomment every cursor's line."
  (run-at-cursors (live-split) 'toggle-comment-lines nil t))

(defun case-at-cursors (split how)
  "Change the case of every cursor's selection of SPLIT, or of the count's
words from every cursor, never past its line, to HOW (see CASE-CHANGED); one
change. A cursor with nothing selected moves over the words."
  (check-writable (split-buffer split))
  (let ((b (split-buffer split)) (n (max 1 (prefix))) (edits '()))
    (loop for place across (split-places split)
          do (multiple-value-bind (s e)
                 (cond ((place-line place) (cursor-line-bounds b place))
                       ((cursor-selecting-p place) (place-bounds b place))
                       (t (let ((c (cursor-position b place)))
                            (values c (words-forward b c n (nth-value 1 (cursor-line-bounds b place)))))))
               (let* ((text (tuatara.buffer:buffer-string b :start s :end e))
                      (new (case-changed text how)))
                 (unless (string= text new)
                   (push (tuatara.text:make-edit s e (tuatara.text:string-octets new)) edits))
                 (unless (cursor-selecting-p place)
                   ;; the cursor goes over the words, as M-u goes over them
                   (set-cursor b place s nil nil nil)
                   (move-place b place s e)))))
    (when edits (tuatara.buffer:buffer-edit b (coerce (nreverse edits) 'simple-vector)))
    (loop for place across (split-places split)
          unless (cursor-selecting-p place)
            do (set-cursor b place (nth-value 1 (place-bounds b place)) nil nil nil))
    (setf (split-did split) :edited)
    (settle-cursors split)))

(define-command cursors-upcase ()
  "Put every cursor's selection in upper case, or the count's words from every cursor."
  (let ((split (live-split)))
    (setf (split-repeat split) (list :verb 'cursors-upcase (prefix-argument)))
    (case-at-cursors split :up)))

(define-command cursors-downcase ()
  "Put every cursor's selection in lower case, or the count's words from every cursor."
  (let ((split (live-split)))
    (setf (split-repeat split) (list :verb 'cursors-downcase (prefix-argument)))
    (case-at-cursors split :down)))

(define-command cursors-capitalise ()
  "Capitalise every cursor's selection, or the count's words from every cursor."
  (let ((split (live-split)))
    (setf (split-repeat split) (list :verb 'cursors-capitalise (prefix-argument)))
    (case-at-cursors split :capital)))

(define-command cursors-undo ()
  "Undo, keeping the cursors: a change made at them all is taken back at all."
  (let ((split (live-split)))
    (undo)
    (settle-cursors split)))

(define-command cursors-redo ()
  "Redo, keeping the cursors."
  (let ((split (live-split)))
    (redo)
    (settle-cursors split)))

(define-command cursors-repeat ()
  "Run the cursors' last command again at every cursor, with the count it had:
a motion, a selection, or deleting, pasting, killing and banking."
  (let* ((split (live-split)) (repeat (split-repeat split)))
    (case (first repeat)
      (:run (destructuring-bind (command arg edit) (rest repeat) (run-at-cursors split command arg edit)))
      (:verb (destructuring-bind (command arg) (rest repeat)
               (setf (editor-prefix-argument *editor*) arg)
               (funcall (find-command command))))
      (t (message "Nothing to repeat at the cursors")))))

;;; A mark run at the cursors

(define-command cursors-run-toggle ()
  "Start a mark run at every cursor, or end it and let go of the selections:
each cursor keeps what it selected, or selects its word, and the run's keys
\\<*mark-run-keymap*>\\[select-word] \\[select-symbol]
\\[select-word-backward] \\[select-symbol-backward]
\\[select-sentence] \\[select-sentence-backward] grow and
\\[mark-run-left] \\[mark-run-right]
\\[mark-run-line-start] \\[mark-run-line-end]
\\[mark-run-exchange] adjust every selection at once;
\\[mark-run-step-back] and \\[mark-run-step-forward] step them
back and forward. The run's keys that would leave the line refuse."
  (let* ((split (live-split)) (b (split-buffer split)))
    (if (split-running split)
        (progn (end-cursors-run split)
               (deselect-cursors split))
        (progn
          (if (some (lambda (place) (multiple-value-bind (s e) (place-bounds b place) (< s e))) (split-places split))
              (progn (loop for place across (split-places split)
                           when (place-line place)
                             do (multiple-value-bind (start end) (cursor-line-bounds b place)
                                  (set-cursor b place start end nil nil)))
                     (setf (split-last split) 'mark-run-adopt))
              (handler-case (run-at-cursors split 'select-word nil)
                (error () (setf (split-last split) nil))))
          (setf (split-running split) t (split-history split) '() (split-redo split) '())
          (arm-cursors split *cursor-run-keymap*)
          (settle-cursors split)))))

(define-command cursors-run-refuse ()
  "Refuse a mark run key the cursors do not run, and keep the run: a cursor stays on its line."
  (message "~a is not run at the cursors: each stays on its line"
           (tuatara.frontend:key-sequence-description *this-command-keys*)))

(defun restore-cursors (split snapshot)
  "Put SPLIT's cursors back as SNAPSHOT had them."
  (let ((b (split-buffer split)))
    (dolist (state snapshot)
      (when (find (first state) (split-places split)) (apply #'set-cursor b state)))
    (setf (split-last split) 'mark-run-step-back)
    (settle-cursors split)))

(define-command cursors-run-step-back ()
  "Put every cursor's selection back where the run's last press found it."
  (let* ((split (live-split)) (b (split-buffer split)))
    (if (null (split-history split))
        (message "No earlier step in this run")
        (progn (push (map 'list (lambda (place) (cursor-state b place)) (split-places split)) (split-redo split))
               (restore-cursors split (pop (split-history split)))))))

(define-command cursors-run-step-forward ()
  "Put every cursor's selection forward again, a step
\\<*mark-run-keymap*>\\[mark-run-step-back] took back."
  (let* ((split (live-split)) (b (split-buffer split)))
    (if (null (split-redo split))
        (message "No later step in this run")
        (progn (push (map 'list (lambda (place) (cursor-state b place)) (split-places split)) (split-history split))
               (restore-cursors split (pop (split-redo split)))))))

(defparameter *cursor-run-keeps*
  '(cursors-replay cursors-run-toggle cursors-repeat
    cursors-run-refuse cursors-run-step-back cursors-run-step-forward
    universal-argument)
  "The commands after which a mark run at the cursors goes on.")

(defun end-cursors-run (split)
  "End the mark run at SPLIT's cursors, keeping them: the cursors' own keys
answer again."
  (setf (split-running split) nil)
  (arm-cursors split *cursor-keymap*))

(defun cursors-run-end (editor)
  "After a command that is not the run's, end a mark run at the cursors, keeping them."
  (declare (ignore editor))
  (let ((split *split*))
    (when (and split (split-cursors split) (split-running split)
               (not (member (editor-this-command *editor*) *cursor-run-keeps*))
               (eq (editor-transient-map *editor*) *cursor-run-keymap*))
      (end-cursors-run split)
      (setf (editor-transient-fresh *editor*) nil))))

;;; The cursors' keys

(defparameter *cursor-verbs*
  '((start-selection . cursors-select)
    (select-lines . cursors-select-lines)
    (tuatara-insert . cursors-insert)
    (tuatara-insert-after . cursors-append)
    (tuatara-insert-line-start . cursors-insert-line)
    (tuatara-insert-line-end . cursors-append-line)
    (end-of-line . cursors-line-end)
    (tuatara-change . cursors-change)
    (delete-selection-or-char . cursors-delete)
    (copy-selection-or-char . cursors-copy)
    (yank-over-selection . cursors-yank)
    (kill-line . cursors-kill-line)
    (bank-selected-lines . cursors-bank)
    (unbank-line . cursors-unbank)
    (tuatara-open-below . cursors-open-below)
    (tuatara-open-above . cursors-open-above)
    (indent-selection-or-line . cursors-indent)
    (toggle-comment-lines . cursors-comment)
    (repeat . cursors-repeat)
    (undo . cursors-undo)
    (redo . cursors-redo)
    (wrap-selection-with-key . split-wrap-key)
    (add-cursor-below . add-cursor-below)
    (add-cursor-above . add-cursor-above)
    (split-selection . split-selection)
    (mark-run-toggle . cursors-run-toggle))
  "Each NORMAL state command the cursors answer, and what answers it on the
same keys.")

(defparameter *cursor-global-verbs*
  '((upcase-word . cursors-upcase) (upcase-dwim . cursors-upcase)
    (downcase-word . cursors-downcase) (downcase-dwim . cursors-downcase)
    (capitalize-word . cursors-capitalise) (capitalize-dwim . cursors-capitalise))
  "Each command of the global keys the cursors answer, on its one-key bindings
(\\[upcase-word] \\[downcase-word] \\[capitalize-word]), and what answers it.")

(defun short-keys-of (command keymap)
  "The key sequences of one or two keys KEYMAP runs COMMAND on, as strings."
  (remove-if (lambda (keys) (> (length (tuatara.frontend:parse-key-sequence keys)) 2))
             (where-is command (list keymap))))

(defun build-cursor-keymaps ()
  "Make the cursors' keymaps from the keys NORMAL state and the mark run have now."
  (setf *cursor-keymap* (make-keymap :name "cursors")
        *cursor-run-keymap* (make-keymap :name "cursors' mark run" :parent *cursor-keymap*))
  (define-key *cursor-keymap* "=" 'align-cursors)
  (flet ((bind (map keys command)
           ;; the first binding a map is given for a key stays; the run's keys
           ;; shadow the cursors' own, the run map's parent
           (unless (lookup-key map (tuatara.frontend:parse-key-sequence keys) :parents nil)
             (define-key map keys command))))
    (dolist (command *cursor-replayed-commands*)
      (dolist (keys (short-keys-of command *tuatara-normal-keymap*))
        (bind *cursor-keymap* keys 'cursors-replay)))
    (loop for (command . verb) in *cursor-verbs*
          do (dolist (keys (short-keys-of command *tuatara-normal-keymap*))
               (bind *cursor-keymap* keys verb)))
    (loop for (command . verb) in *cursor-global-verbs*
          do (dolist (keys (where-is command (list *global-keymap*)))
               (when (= 1 (length (tuatara.frontend:parse-key-sequence keys)))
                 (bind *cursor-keymap* keys verb))))
    (define-key *cursor-keymap* "DEL" 'drop-last-cursor)
    (define-key *cursor-keymap* "C-g" 'split-quit)
    ;; past the limit, the verbs that write at the rows out of sight too
    (setf *virtual-cursor-keymap* (make-keymap :name "cursors past the limit"))
    (loop for (command . verb) in *cursor-verbs*
          when (member verb *virtual-cursor-verbs*)
            do (dolist (keys (short-keys-of command *tuatara-normal-keymap*))
                 (bind *virtual-cursor-keymap* keys verb)))
    (define-key *virtual-cursor-keymap* "C-g" 'split-quit)
    ;; the motions that move every cursor alike, those out of sight too; the cursors'
    ;; other keys refused, saying what works, rather than ending them all
    (dolist (command (append *cursor-replayed-commands* *virtual-cursor-motions*))
      (dolist (keys (short-keys-of command *tuatara-normal-keymap*))
        (bind *virtual-cursor-keymap* keys
              (if (member command *virtual-cursor-motions*) 'cursors-move-past-limit 'cursors-past-limit-refuse))))
    ;; undo, redo, repeating and making cursors end them and do their own job, as before
    (loop for (command . verb) in *cursor-verbs*
          unless (or (member verb *virtual-cursor-verbs*)
                     (member verb '(cursors-undo cursors-redo cursors-repeat add-cursor-below add-cursor-above split-selection)))
            do (dolist (keys (short-keys-of command *tuatara-normal-keymap*))
                 (bind *virtual-cursor-keymap* keys 'cursors-past-limit-refuse)))
    (loop for (command) in *cursor-global-verbs*
          do (dolist (keys (where-is command (list *global-keymap*)))
               (when (= 1 (length (tuatara.frontend:parse-key-sequence keys)))
                 (bind *virtual-cursor-keymap* keys 'cursors-past-limit-refuse))))
    (loop for (keys command) in *mark-run-bindings*
          do (bind *cursor-run-keymap* keys
                   (cond ((member command *cursor-run-replayed-commands*) 'cursors-replay)
                         ((eq command 'mark-run-cancel) 'cursors-run-toggle)
                         ((eq command 'repeat) 'cursors-repeat)
                         ((eq command 'mark-run-step-back) 'cursors-run-step-back)
                         ((eq command 'mark-run-step-forward) 'cursors-run-step-forward)
                         (t 'cursors-run-refuse))))
    (dolist (keys '("m p" "m P")) (bind *cursor-run-keymap* keys 'cursors-run-refuse))
    (define-key *cursor-run-keymap* "C-g" 'split-quit)))

(defun install-cursors (editor)
  "Let the split's keys keep the cursors, and have EDITOR end a mark run at
them when another key is pressed."
  (dolist (command (append '(cursors-replay drop-last-cursor align-cursors
                             cursors-move-past-limit cursors-past-limit-refuse
                             cursors-run-refuse cursors-run-step-back
                             cursors-run-step-forward add-cursor-below
                             add-cursor-above)
                           (mapcar #'cdr *cursor-verbs*)
                           (mapcar #'cdr *cursor-global-verbs*)))
    (pushnew command *split-commands*))
  (setf *split-commands* (remove 'split-selection *split-commands*))
  (build-cursor-keymaps)
  (pushnew 'cursors-run-end (editor-post-command-functions editor)))
