;;;; lisp/selecting/selecting.lisp --- Selecting and acting on the selection: the verbs that take it, or a character without one.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A selection is started at point and grown by any motion (start-selection),
;;;; or by whole lines (select-lines, as Vim's visual line); delete-selection-or-char,
;;;; copy-selection-or-char and yank-over-selection act on it, or on the
;;;; character at point without one. The selections of a word, a sentence, a
;;;; paragraph and a pair are in selection-objects.lisp; of an expression and of
;;;; the pieces of syntax tree-sitter names (functions, types, parameters ...)
;;;; here. Banking, the mark run, split, cursors, rectangles, wrapping and
;;;; digraphs are in files of their own. The modal layer (lisp/modal/) binds
;;;; them to its keys; every one is a command of its own, to bind or run with M-x.

(in-package #:tuatara.defaults)

(defparameter *position-ring-max* 10
  "How many previous positions the jump-back ring keeps per buffer.")

(defun drop-selection (&optional (buffer (current-buffer)))
  "Drop the selection of BUFFER, the current buffer unless given, as the keys
that enter INSERT state do: the mark goes inactive, and a line selection or a
rectangle ends with it. Banked lines stay; \\<normal>\\[tuatara-change] is the
key that acts on them."
  (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
  (end-rectangle buffer)
  (line-selection-clear buffer))

(defvar *cursors-limit* 10000
  "The most cursors \\<normal>\\[add-cursor-below] and
\\[add-cursor-above] make, or NIL for as many as the memory has room
for: every key but typing runs once at every cursor, so a great many make every
key slow; what is typed is written as it is typed only at the cursors in sight,
the others when the typing ends. With more lines selected than this, the
cursors in sight stand, as many as this, and the others are written in the
background as each verb is done at those.")

(defvar *rectangle-cursors-limit* 10000
  "The most rows \\<normal>\\[tuatara-change] on a rectangle
(\\[rectangle-mark-mode]) puts a cursor on, or NIL for as many as the
memory has room for: the block is emptied and what is typed appears on every
row, as at the cursors of \\[add-cursor-below]. On a taller block the
rows in sight hold the cursors, as many as this, and the others are emptied and
written in the background as the typing ends.")

(defvar *split-limit-ask* nil
  "True to be asked, when the places would pass their limit, whether to hold
them all the same, the memory having room, rather than those in sight only:
the matches of \\<normal>\\[split-selection] past *SPLIT-LIMIT*, the cursors of
\\[add-cursor-below] past *CURSORS-LIMIT* (a count past it too), and those of
\\[tuatara-change] on a rectangle past *RECTANGLE-CURSORS-LIMIT*: one
question, the same for them all. NIL, the default, holds those in sight.")

;;; Selections: the mark, and visual-line sessions started with V

(defun line-selection-anchor (&optional (buffer (current-buffer)))
  "The byte offset of the line where the visual-line session started, or NIL."
  (tuatara.buffer:buffer-local buffer 'line-selection-anchor))

(defun line-selection-clear (&optional (buffer (current-buffer)))
  "End the visual-line session of BUFFER, if any."
  (setf (tuatara.buffer:buffer-local buffer 'line-selection-anchor) nil))

(defun line-selection-active-p ()
  "True while a visual-line session is live: it was started and the region is still active."
  (and (line-selection-anchor) (tuatara.buffer:buffer-mark-active-p (current-buffer))))

(defparameter *linear-selection-hint* "Linear selection: any motion extends, \\<normal>\\[start-selection] re-anchors, \\[tuatara-quit] to cancel"
  "What the echo area says while a selection started with v lasts.")

(defparameter *line-selection-hint* "Lines: \\<normal>\\[select-next-line]/\\[select-previous-line] whole lines, \\[next-line]/\\[previous-line] by char, \\[select-lines] to cancel"
  "What the echo area says while a visual-line session lasts.")

(defparameter *rectangle-hint* "Rectangle: any motion sizes the block, \\<normal>\\[rectangle-mark-mode] or \\[tuatara-quit] to cancel"
  "What the echo area says while a rectangle lasts.")

(defparameter *selection-hint-motions*
  '(next-line previous-line forward-char backward-char forward-word backward-word
    forward-sexp backward-sexp beginning-of-line end-of-line beginning-of-buffer end-of-buffer)
  "The motions after which the reminder of the selection being made is shown again.")

(defun fresh-selection (&optional (buffer (current-buffer)))
  "Forget the kind of selection BUFFER had, a new one being made: a rectangle,
a visual-line session, a selection started with \\<normal>\\[start-selection]."
  (end-rectangle buffer)
  (line-selection-clear buffer)
  (setf (tuatara.buffer:buffer-local buffer 'linear-selection) nil))

(defun selection-remind (editor)
  "After a motion, show the reminder of the selection being made again: a
rectangle's, a visual-line session's or one started with \\<normal>\\[start-selection]."
  (when (and (null (editor-message editor))
             (not (editor-minibuffer-active editor))
             (member (editor-this-command editor) *selection-hint-motions*))
    (let ((b (tuatara.display:window-buffer (editor-window editor))))
      (when (tuatara.buffer:buffer-mark-active-p b)
        (cond ((rectangle-live-p b) (echo "~a" (substitute-command-keys *rectangle-hint* :marks t)))
              ((line-selection-anchor b) (echo "~a" (substitute-command-keys *line-selection-hint* :marks t)))
              ((tuatara.buffer:buffer-local b 'linear-selection)
               (echo "~a" (substitute-command-keys *linear-selection-hint* :marks t))))))))

(defun pressed-key-name ()
  "The name of the key that ran the current command, as the echo area writes keys."
  (let ((key (editor-this-key *editor*)))
    (if key (tuatara.frontend:key-description (tuatara.frontend:key-spec key)) "That key")))

(defun line-selection-update ()
  "Set mark and point to show the line selection from the anchor line
to the current line: whole lines, the final newline not highlighted. A line
whose end a fold hides is taken with all the fold hides; the lines search
filters hide are not taken."
  (let* ((b (current-buffer)) (anchor (line-selection-anchor)) (p (point)))
    (cond ((>= p anchor)
           (setf (tuatara.buffer:buffer-mark b) (tuatara.display:visible-line-start b anchor :hidden nil))
           (goto (tuatara.display:visible-line-end b p :hidden nil)))
          (t
           (setf (tuatara.buffer:buffer-mark b) (tuatara.display:visible-line-end b anchor :hidden nil))
           (goto (tuatara.display:visible-line-start b p :hidden nil))))
    (setf (tuatara.buffer:buffer-mark-active-p b) t)))

(defun line-after (buffer position)
  "Where the line a window shows after POSITION's line of BUFFER starts, or
the text's end: past a line whose end a fold hides, after all the fold hides,
but never past the lines search filters hide: the line after it is then one
of those."
  (let ((line-end (tuatara.display:visible-line-end buffer position :hidden nil)))
    (if (< line-end (tuatara.buffer:buffer-size buffer))
        (tuatara.buffer:buffer-forward-char buffer line-end)
        line-end)))

(defun whole-lines (buffer start end &key visible columns)
  "Two values, the start and end of the whole lines from START to END of
BUFFER: from the start of the first to after the newline of the last, or to
the text's end. END at the start of a line past START belongs to the line
before it (RANGE-LAST-PLACE), unless COLUMNS, when that line is taken too.
With VISIBLE, lines are those a window shows: a line whose end a fold hides
is whole with all the fold hides, as a folded headline with its body; a line
shown is whole without the lines search filters hide after it. Of a
narrowed buffer, the lines of the part shown, cut at its edges."
  (let* ((low (min start end)) (high (max start end))
         (last (if columns high (range-last-place buffer low high))))
    (multiple-value-bind (from to)
        (if visible
            (values (tuatara.display:visible-line-start buffer low :hidden nil) (line-after buffer last))
            (let* ((rope (tuatara.buffer:buffer-text buffer))
                   (newline (tuatara.text:rope-find-line-end rope :start last)))
              (values (tuatara.text:rope-line-start rope low) (if newline (1+ newline) (tuatara.text:rope-bytes rope)))))
      (values (tuatara.buffer:within-narrowing buffer from) (tuatara.buffer:within-narrowing buffer to)))))

(defun whole-lines-span (buffer start end)
  "The whole lines a window shows from START to END of BUFFER (WHOLE-LINES,
VISIBLE) as (START . END)."
  (multiple-value-call #'cons (whole-lines buffer start end :visible t)))

(defun selection-range (&key widen)
  "Two values, the start and end of the active selection, or NIL. With WIDEN
true a visual-line session covers whole lines including the final newline,
as \\<normal>\\[copy-selection-or-char], \\[delete-selection-or-char] and \\[yank-over-selection] take it; without, it is the highlighted text, as
\\[tuatara-change] takes it. A line whose end a fold hides is whole with all
the fold hides."
  (let ((b (current-buffer)))
    (when (tuatara.buffer:buffer-mark-active-p b)
      (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
        (if (and widen (line-selection-anchor))
            (let ((span (whole-lines-span b start end)))
              (values (car span) (cdr span)))
            (values start end))))))

(define-command start-selection ()
  "Start a selection at point: any motion extends it. Pressed again it starts
afresh from where point is; \\[tuatara-quit] lets go."
  (let ((b (current-buffer)))
    (fresh-selection b)
    (setf (tuatara.buffer:buffer-mark b) (point)
          (tuatara.buffer:buffer-mark-active-p b) t
          (tuatara.buffer:buffer-local b 'linear-selection) t)
    (message "~a" (substitute-command-keys *linear-selection-hint* :marks t))))

(define-command select-whole-buffer ()
  "Select the whole buffer: point at the start, the mark at the end."
  (fresh-selection)
  (mark-whole-buffer))

(define-command select-lines ()
  "Start a line selection anchored on the current line, or cancel a live one.
With a count, select that many rows from the current line (negative goes up)."
  (let ((count (prefix-argument)))
    (cond ((and (null count) (line-selection-active-p))
           (line-selection-clear)
           (setf (tuatara.buffer:buffer-mark-active-p (current-buffer)) nil)
           (message "Line selection cancelled"))
          (t
           (fresh-selection)
           (setf (tuatara.buffer:buffer-local (current-buffer) 'line-selection-anchor) (point))
           (when (and count (/= count 0))
             (vertical-motion (if (plusp count) (1- count) (1+ count))))
           (line-selection-update)
           (message "~a" (substitute-command-keys *line-selection-hint* :marks t))))))

(define-command select-next-line ()
  "Move down, extending the line selection if one is active."
  (if (line-selection-active-p)
      (progn
        (goto (line-start-away (current-buffer) (point) (prefix)))
        (line-selection-update))
      (progn (next-line)
             ;; as next-line, so that j and J keep to one column together
             (setf (editor-this-command *editor*) 'next-line))))

(define-command select-previous-line ()
  "Move up, extending the line selection if one is active."
  (if (line-selection-active-p)
      (progn
        (goto (line-start-away (current-buffer) (point) (- (prefix))))
        (line-selection-update))
      (progn (previous-line)
             (setf (editor-this-command *editor*) 'previous-line))))

;;; Delete, change, copy, paste

(defun char-span (count)
  "Two values: the byte range of COUNT characters after point (before it when
COUNT is negative), a letter with its accents or an emoji with its skin tone
one (CLUSTER-FORWARD), clipped to the text, or NIL for a zero count."
  (unless (zerop count)
    (let* ((b (current-buffer)) (p (point)) (q (cluster-forward b p count)))
      (values (min p q) (max p q)))))

(defun kill-selection-or-chars (&key widen)
  "Kill the selection into the kill ring, a line selection widened to whole
lines when WIDEN is true (SELECTION-RANGE); without one, delete COUNT
characters at point, which are not saved. What would take part of the text a
fold hides, but not all of it, is not deleted: the fold is shown instead."
  (multiple-value-bind (start end) (selection-range :widen widen)
    (if start
        (progn (setf (tuatara.buffer:buffer-mark (current-buffer)) start) (goto end)
               (kill-region) (line-selection-clear))
        (multiple-value-bind (s e) (char-span (prefix))
          (when s (multiple-value-setq (s e) (tuatara.buffer:whole-line-ends (current-buffer) s e)))
          (when (and s (< s e))
            (refuse-hidden-deletion (current-buffer) s e)
            (tuatara.buffer:buffer-delete (current-buffer) s e) (goto s))))))

(define-command delete-selection-or-char ()
  "Delete the selection into the kill ring; without one, delete COUNT
characters at point, which are not saved: a character under the cursor is a
typo being fixed, not a cut. A line selection is widened to whole lines.
A rectangle goes to the rectangle store instead, where \\<normal>\\[yank-rectangle] pastes it from.
With lines banked (\\[bank-selected-lines]), they are killed, with the lines of any selection, as one.
What would take part of the text a fold hides, but not all of it, is not
deleted: the fold is shown instead, and the key pressed again deletes."
  (cond ((rectangle-live-p (current-buffer)) (delete-selected-rectangle))
        ((banked-p) (delete-banked))
        (t (kill-selection-or-chars :widen t))))

(define-command copy-selection-or-char ()
  "Copy the selection, or COUNT characters at point when nothing is selected.
A line selection is widened to whole lines, the last line given its line
end when the text ends without one, so that it pastes as a line of its own. At the end of the buffer
there is nothing to copy. A rectangle is copied to the rectangle store, where
\\<normal>\\[yank-rectangle] pastes it from; the kill ring and the system clipboard are left alone.
With lines banked (\\[bank-selected-lines]), they are copied, with the lines of any selection, as
one. The lines search filters hide (\\[search-filter-regex]) are left out, in
a rectangle too: only the lines shown are copied."
  (when (rectangle-live-p (current-buffer))
    (return-from copy-selection-or-char (copy-selected-rectangle)))
  (when (banked-p)
    (return-from copy-selection-or-char (copy-banked)))
  (multiple-value-bind (start end) (selection-range :widen t)
    (let ((b (current-buffer)) (lines (and start (line-selection-anchor))))
      (unless start
        (multiple-value-setq (start end) (char-span (prefix))))
      (cond ((and start (< start end))
             (copy-region-text b start end)
             ;; whole lines, the last with no line end: copied with one, to be pasted as lines
             (when (and lines (= end (tuatara.buffer:buffer-size b))
                        (not (tuatara.text:rope-line-start-p (tuatara.buffer:buffer-text b) end)))
               (kill-text (string #\Newline) :append t))
             (setf (tuatara.buffer:buffer-mark-active-p b) nil)
             (line-selection-clear))
            (t (message "Nothing to copy"))))))

(define-command yank-over-selection ()
  "Paste the last kill COUNT times, replacing the selection if there is one,
its lines ending as the buffer's do; what another program copied to the system
clipboard since counts as the last kill. A line selection is replaced as whole lines: a kill that ends
with a newline slots in as complete lines, one that does not keeps the taken
newline. Not over a rectangle, whose shape a kill does not have: \\<normal>\\[yank-rectangle]
pastes the rectangle store. Lines banked (\\[bank-selected-lines]) are a selection too: the paste
replaces them, with the lines of any selection made. A rectangle killed or
copied with \\[kill-region] or \\[kill-ring-save] goes in as a block, as
\\[yank] puts it, each row COUNT times across. What would replace part of the
text a fold hides, but not all of it, is not pasted over: the fold is shown
instead, and the key pressed again pastes."
  (when (rectangle-live-p (current-buffer))
    (return-from yank-over-selection (message "~a is undefined on a rectangle" (pressed-key-name))))
  (take-in-clipboard)
  (let* ((kill (first (editor-kill-ring *editor*)))
         (text (and kill (text-for-buffer kill (current-buffer)))))
    (if (null text)
        (message "The kill ring is empty")
        (let ((b (current-buffer))
              (copies (kill-copies text (max 1 (prefix)))))
          (multiple-value-bind (start end) (selection-range :widen t)
            (cond ((and (eq (nth-value 1 (kill-parts kill)) :block) (not (banked-p b)))
                   (when start (delete-spans b (list (cons start end))))
                   (setf (tuatara.buffer:buffer-mark-active-p b) nil)
                   (line-selection-clear)
                   (when start (goto start))
                   (paste-rows-at-point (mapcar (lambda (row) (with-output-to-string (out)
                                                                (dotimes (i (max 1 (prefix))) (write-string row out))))
                                                (kill-parts kill))))
                  ((banked-p b)
                   (if (stringp copies)
                       (yank-over-banked copies)
                       (message "The last kill (~a) is too large to paste over banked lines" (octets-words (kill-size copies)))))
                  (start
                   (let ((restore (and (line-selection-anchor)
                                       (plusp (kill-size copies))
                                       (not (kill-ends-line-p copies))
                                       (< end (tuatara.buffer:buffer-size b)))))
                     (let ((after (delete-spans b (list (cons start end))
                                                :replacements (list (if restore (kill-join copies (tuatara.buffer:buffer-newline b)) copies)))))
                       (setf (tuatara.buffer:buffer-mark-active-p b) nil)
                       (line-selection-clear)
                       (goto (if after (aref after 0) start)))))
                  (t (goto (tuatara.buffer:buffer-insert b (point) (if (stringp copies)
                                                                         (tuatara.buffer:unjoined-line-ends b (point) copies)
                                                                         copies))))))))))

;;; Lines

(define-command join-selected-lines ()
  "Pull the following line up onto this one, COUNT times; with a selection,
join the lines it touches into one, in the background when it holds more than
*REWRITE-THRESHOLD* octets."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (selection-range)
      (when (and start (large-for-here-p start end))
        (setf (tuatara.buffer:buffer-mark-active-p b) nil)
        (line-selection-clear)
        (return-from join-selected-lines
          (join-lines-in-background b start (max start (tuatara.buffer:buffer-forward-char b end -1)))))
      (let ((n (if start
                   (max 1 (- (tuatara.buffer:buffer-line-number b (max start (tuatara.buffer:buffer-forward-char b end -1)))
                             (tuatara.buffer:buffer-line-number b start)))
                   (prefix))))
        (when start
          (goto start)
          (setf (tuatara.buffer:buffer-mark-active-p b) nil)
          (line-selection-clear))
        (dotimes (i (max 0 n)) (join-following-line b))))))

;;; The position ring and jump-back

(defun track-position (editor)
  "After every command, record the previous point in the buffer's position ring
when point moved. The previous point is kept as an anchor made while it was
valid, so it follows the text through whatever the command changed; one not
put in the ring is moved along, not made again. Runs from the post-command
functions."
  (let* ((b (tuatara.display:window-buffer (editor-window editor)))
         (version (tuatara.buffer:buffer-version b))
         (now (tuatara.buffer:buffer-point b))
         (last (tuatara.buffer:buffer-local b 'selection-last-anchor)))
    (when (and last
               (/= (tuatara.text:anchor-position last version) now)
               (not (eq (editor-this-command editor) 'jump-back)))
      (let ((ring (tuatara.buffer:buffer-local b 'position-ring '())))
        (push last ring)
        (when (> (length ring) *position-ring-max*)
          (setf ring (subseq ring 0 *position-ring-max*)))
        (setf (tuatara.buffer:buffer-local b 'position-ring) ring
              (tuatara.buffer:buffer-local b 'position-index) 0
              last nil)))
    (if last
        (tuatara.text:anchor-move last version now)
        (setf (tuatara.buffer:buffer-local b 'selection-last-anchor) (tuatara.text:make-anchor version now)))))

(define-command jump-back ()
  "Rotate to the next stored position in the ring and jump there; press again
to cycle. Meant for taking back a mis-keyed jump."
  (let* ((b (current-buffer))
         (ring (tuatara.buffer:buffer-local b 'position-ring '())))
    (if (null ring)
        (message "No positions recorded yet")
        (let* ((index (mod (+ (tuatara.buffer:buffer-local b 'position-index 0) (1- (max 1 (prefix)))) (length ring)))
               (anchor (nth index ring)))
          (goto (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version b)))
          (setf (tuatara.buffer:buffer-local b 'position-index) (mod (1+ index) (length ring)))
          (message "Position ~a of ~a" (1+ index) (length ring))))))

;;; Marking the expression around point

(defun expression-pairs (buffer position count)
  "The COUNT bracketed expressions around POSITION in BUFFER, innermost first,
as (START . END) from an opening bracket to after its closing one: by the
grammar where BUFFER has one, so brackets in strings, comments and character
literals do not count, else by the brackets in the text."
  (let ((syntax (ready-syntax buffer)))
    (if syntax
        (tuatara.syntax:enclosing-pairs syntax position count)
        (tuatara.syntax:text-enclosing-pairs (tuatara.buffer:buffer-text buffer) position count))))

(defun select-expression-at (inner)
  "Select the bracketed expression around point, without its brackets when
INNER; see select-sexp-inside."
  (let* ((b (current-buffer))
         (state (and (eq (editor-last-command *editor*) (editor-this-command *editor*))
                     (tuatara.buffer:buffer-mark-active-p b)
                     (tuatara.buffer:buffer-local b 'select-sexp-state)))
         (anchor (if state (car state) (point)))
         (levels (+ (if state (cdr state) 0) (max 1 (prefix))))
         (pairs (expression-pairs b anchor levels)))
    (if (< (length pairs) levels)
        (message "Not inside a balanced expression")
        (destructuring-bind (open . close) (car (last pairs))
          (let ((start (if inner (1+ open) open))
                (end (if inner (1- close) close)))
            (if (and inner (>= start end))
                (message "Empty expression")
                (progn
                  (fresh-selection b)
                  (setf (tuatara.buffer:buffer-mark b) end
                        (tuatara.buffer:buffer-mark-active-p b) t
                        (tuatara.buffer:buffer-local b 'select-sexp-state) (cons anchor levels))
                  (goto start)
                  (message (if inner "Marked inner expression" "Marked outer expression")))))))))

(define-command select-sexp-inside ()
  "Select what is inside the bracketed expression around point, its brackets
left out: point at the start, the mark at the end. Point on a bracket means
that bracket's pair. In a language with a grammar, brackets in strings,
comments and character literals do not count. With \\[universal-argument] N, the Nth pair out;
pressed again at once, the next pair out."
  (select-expression-at t))

(define-command select-sexp ()
  "Select the bracketed expression around point, its brackets included: point
at the start, the mark at the end. As select-sexp-inside for which pair."
  (select-expression-at nil))

;;; Selecting functions, types, parameters, comments, tests, entries and elements

(define-command select-function-inside ()
  "Select the body of the function around point: the block of a Rust fn or
closure, the body of a shell function, the forms of a Lisp defun, defmacro,
defgeneric, defmethod or lambda after its lambda list. Point at the start,
the mark at the end. Pressed again at once, or with \\[universal-argument] N,
the next function out."
  (mark-text-object "function" t))

(define-command select-function ()
  "Select all of the function around point. Pressed again at once, or with
\\[universal-argument] N, the next function out."
  (mark-text-object "function" nil))

(define-command select-type-inside ()
  "Select the body of the type around point: the fields, variants or items of
a Rust struct, enum, union, trait or impl; in Lisp, what follows the name in a
defclass, defstruct, define-condition or deftype. Pressed again at once, or
with \\[universal-argument] N, the next type out."
  (mark-text-object "class" t))

(define-command select-type ()
  "Select all of the type around point. Pressed again at once, or with
\\[universal-argument] N, the next type out."
  (mark-text-object "class" nil))

(define-command select-parameter-inside ()
  "Select the parameter or argument around point: of a Rust function's
parameters, a call's arguments or a generic's parameters; of a shell command's
arguments; of a Lisp lambda list or a call's arguments. Pressed again at once,
or with \\[universal-argument] N, the next one out."
  (mark-text-object "parameter" t))

(define-command select-parameter ()
  "Select the parameter or argument around point with the comma after it.
Pressed again at once, or with \\[universal-argument] N, the next one out."
  (mark-text-object "parameter" nil))

(define-command select-comment-inside ()
  "Select the comment around point."
  (mark-text-object "comment" t))

(define-command select-comment ()
  "Select the comment around point with the comment lines right before and
after it."
  (mark-text-object "comment" nil))

(define-command select-test-inside ()
  "Select the body of the test around point: a Rust function marked #[test], a
Python function whose name begins with test, a Go TestX or BenchmarkX, and the
tests of Zig, Elixir, Dart, Julia and others, as the language's rules know
them. Pressed again at once, or with \\[universal-argument] N, the next test
out."
  (mark-text-object "test" t))

(define-command select-test ()
  "Select all of the test around point. Pressed again at once, or with
\\[universal-argument] N, the next test out."
  (mark-text-object "test" nil))

(define-command select-entry-inside ()
  "Select the key or the value at point of the entry around it: of a key and
its value in JSON, YAML or TOML, a field of a struct, an item of a list, as
the language's rules know them. Pressed again at once, or with
\\[universal-argument] N, the next one out."
  (mark-text-object "entry" t))

(define-command select-entry ()
  "Select all of the entry around point, its key and value or the list's
item. Pressed again at once, or with \\[universal-argument] N, the next entry
out."
  (mark-text-object "entry" nil))

(define-command select-element-inside ()
  "Select what is between the tags of the element around point: in HTML, XML,
JSX, Svelte and Vue. Pressed again at once, or with \\[universal-argument] N,
the next element out."
  (mark-text-object "xml-element" t))

(define-command select-element ()
  "Select all of the element around point, its tags too. Pressed again at
once, or with \\[universal-argument] N, the next element out."
  (mark-text-object "xml-element" nil))

;;; Commenting lines

(define-command toggle-comment-lines ()
  "Comment the lines the selection touches, or the current line, or uncomment
them when they are all commented: with the language's marker (;; in Lisp, //
in Rust, # in shell scripts and Org) at their smallest indentation, or each
line between the two a comment has where it has an end (<!-- --> in HTML,
/* */ in CSS). The selection ends."
  (let* ((b (current-buffer))
         (active (tuatara.buffer:buffer-mark-active-p b)))
    (multiple-value-bind (start end) (if active (tuatara.buffer:buffer-region b) (values (point) (point)))
      (let ((done (comment-or-uncomment-lines b start end)))
        (setf (tuatara.buffer:buffer-mark-active-p b) nil)
        (unless done
          (message "No comment syntax is defined"))))))

;;; Indenting lines

(define-command indent-selection-or-line ()
  "Indent the lines the selection touches, or the current line, as the
language's mode indents them. The selection ends."
  (let* ((b (current-buffer))
         (active (tuatara.buffer:buffer-mark-active-p b)))
    (multiple-value-bind (start end) (if active (tuatara.buffer:buffer-region b) (values (point) (point)))
      (let ((done (indent-lines b start end)))
        (setf (tuatara.buffer:buffer-mark-active-p b) nil)
        (when (eq done :none)
          (message "No indentation rules for ~a" (tuatara.buffer:buffer-local b :mode-name "this buffer")))))))

;;; Repeating a change with its selection

(defvar *text-object-commands-found* nil
  "SELECTION-OBJECT-COMMANDS as last found, with the lists of keys it was found
in: (NORMAL-BINDINGS SYNTAX-BINDINGS COMMANDS).")

(defun selection-object-commands ()
  "The commands NORMAL state selects a text object with: those of its \\`m'
keys that make a selection of a kind (a word, a sentence, a pair ...). Found
again only when NORMAL state's keys have changed."
  (let ((normal (symbol-value '*tuatara-normal-bindings*))
        (syntax (symbol-value '*tuatara-syntax-bindings*))
        (kept *text-object-commands-found*))
    (if (and kept (eq (first kept) normal) (eq (second kept) syntax))
        (third kept)
        (third (setf *text-object-commands-found*
                     (list normal syntax
                           (loop for (keys command) in (append normal syntax)
                                 when (and (uiop:string-prefix-p "m " keys) (uiop:string-prefix-p "SELECT-" (symbol-name command)))
                                   collect command)))))))

(defparameter *repeat-selection-largest* (* 64 1024)
  "The most octets a selection may hold to be made again when the command
that took it is repeated (\\[repeat]); a larger one is not measured.")

(defun selection-remaker (buffer)
  "A function of no arguments that makes again, from point, a selection like
BUFFER's active one: the text object that made it, as many times; else the
same number of whole lines, a rectangle as many rows by as many columns, or
the same number of characters, counted only when it is called. NIL when
BUFFER has none, or one larger than *REPEAT-SELECTION-LARGEST*."
  (when (and (tuatara.buffer:buffer-mark-active-p buffer)
             (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
               (<= (- end start) *repeat-selection-largest*)))
    (let ((maker (tuatara.buffer:buffer-local buffer 'selection-maker))
          (rope (tuatara.buffer:buffer-text buffer)))
      (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
        (cond ((consp maker)
               (destructuring-bind (command . count) maker
                 (lambda ()
                   (setf (editor-prefix-argument *editor*) (and (> count 1) count))
                   (funcall (find-command command)))))
              ((rectangle-live-p buffer)
               (multiple-value-bind (left right) (rectangle-columns buffer)
                 (lambda ()
                   (rectangle-mark-mode)
                   (vertical-motion (tuatara.text:rope-count-newlines rope start end))
                   (char-motion (- right left 1)))))
              ((line-selection-anchor buffer)
               (let ((last (max start (tuatara.buffer:buffer-forward-char buffer end -1))))
                 (lambda ()
                   (setf (editor-prefix-argument *editor*) (1+ (tuatara.text:rope-count-newlines rope start last)))
                   (select-lines))))
              (t
               (lambda ()
                 (let ((characters (if (tuatara.text:rope-indexed-p rope)
                                       (- (tuatara.text:byte->char rope end) (tuatara.text:byte->char rope start))
                                       (tuatara.text:rope-count-chars rope start end))))
                   (start-selection)
                   (char-motion characters)))))))))

(defun note-selection-before (editor command)
  "Before COMMAND runs, keep how to make the active selection again, should
COMMAND act on it (NOTE-SELECTION-AFTER)."
  (declare (ignore command))
  (setf (tuatara.buffer:buffer-local (current-buffer) 'selection-before)
        (and (editor-window editor) (selection-remaker (current-buffer)))))

(defun note-selection-after (editor)
  "After a command: one that took the selection it found (no selection after
it) is repeated with that selection made again (*REPEAT-SELECTION*); one that
made or grew a selection is noted as what made it, a text object's command
with how many it took, anything else as a selection to measure."
  (let* ((buffer (current-buffer))
         (before (tuatara.buffer:buffer-local buffer 'selection-before))
         (command (editor-this-command editor))
         (entry (editor-repeatable editor)))
    (setf (tuatara.buffer:buffer-local buffer 'selection-before) nil)
    (cond ((not (tuatara.buffer:buffer-mark-active-p buffer))
           (setf (tuatara.buffer:buffer-local buffer 'selection-maker) nil)
           (when (and before entry (eq (car entry) command))
             (setf *repeat-selection* (cons entry before))))
          ((member command (selection-object-commands))
           (let ((maker (tuatara.buffer:buffer-local buffer 'selection-maker)))
             (setf (tuatara.buffer:buffer-local buffer 'selection-maker)
                   (cons command (+ (if (and (consp maker) (eq (car maker) command) before) (cdr maker) 0)
                                    (max 1 (or (second entry) 1)))))))
          (t (setf (tuatara.buffer:buffer-local buffer 'selection-maker) :measured)))))

