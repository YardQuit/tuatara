;;;; src/display/window.lisp --- Windows: a view of a buffer that keeps point visible.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.display)

(defclass area ()
  ((parent :initform nil :accessor area-parent
           :documentation "The split holding the area, or NIL for the area that is the whole layout.")
   (share :initform 1 :accessor area-share
          :documentation "The area's part of its split: of the split's height when the areas in it are one above another, else of its width. The parts of a split add up to 1.")
   (left :initform 0 :accessor area-left
         :documentation "The screen column of the area's left edge.")
   (top :initform 0 :accessor area-top
        :documentation "The screen row of the area's top edge.")
   (total-width :initform 0 :accessor area-width
                :documentation "Columns the area covers, the divider of a window beside another included.")
   (total-height :initform 0 :accessor area-height
                 :documentation "Rows the area covers, a window's mode line included."))
  (:documentation "A rectangle of the screen in the layout of windows: a window, or a split
holding windows one above another or side by side."))

(setf (documentation 'area-parent 'function) "The split holding AREA, or NIL when AREA is the whole layout."
      (documentation 'area-share 'function) "AREA's part of its split's height or width, a fraction."
      (documentation 'area-left 'function) "The screen column of AREA's left edge."
      (documentation 'area-top 'function) "The screen row of AREA's top edge."
      (documentation 'area-width 'function) "Columns AREA covers, a window's divider included."
      (documentation 'area-height 'function) "Rows AREA covers, a window's mode line included.")

(defclass window (area)
  ((buffer :initarg :buffer :reader window-buffer
           :documentation "The buffer shown in the window.")
   (start :accessor window-start-anchor
          :documentation "An anchor holding the first byte shown: always a line start or a wrap point.")
   (point :initform nil :accessor window-point-anchor
          :documentation "An anchor holding the window's point while another window is selected; NIL for the selected window, whose point is its buffer's.")
   (width :initarg :width :initform 80 :accessor window-width
          :documentation "Columns available for text.")
   (height :initarg :height :initform 24 :accessor window-height
           :documentation "Rows available for text (excluding the mode line).")
   (full-height :initform nil :accessor window-full-height
                :documentation "Rows of text the window has while the panel below the windows is one row, or NIL for its height: point is kept visible by these, so that a taller panel for a while hides rows without scrolling.")
   (wrap :initform nil :accessor window-wrap
         :documentation "True to wrap long lines, NIL to truncate them with horizontal scrolling.")
   (hscroll :initform 0 :accessor window-hscroll
            :documentation "Columns scrolled horizontally when not wrapping.")
   (goal-column :initform nil :accessor window-goal-column
                :documentation "Column that vertical motion tries to keep, or NIL.")
   (margin :initform 0 :accessor window-margin
           :documentation "Columns reserved at the left for gutters (line numbers, version-control marks); text starts after them.")
   (rows :initform #() :accessor window-rows
         :documentation "The rows of the last layout, for painting and cursor mapping.")
   (spare-rows :initform (make-array 0 :adjustable t :fill-pointer 0) :reader window-spare-rows
               :documentation "Rows the window showed before and shows no more, for its next layout to fill again.")
   (row-start-seen :initform nil :accessor window-row-start-seen
                   :documentation "What the window's start was last found a row's start for (ROW-START-STATE), or NIL.")
   (laid-out :initform nil :accessor window-laid-out
             :documentation "The window's last layout, kept to be used again (KEPT-LAYOUT): (STATE START ROWS BASES), or NIL.")
   (divider :initform nil :accessor window-divider
            :documentation "True when the window's last column is the line between it and the window beside it.")
   (use-time :initform 0 :accessor window-use-time
             :documentation "When the window was last selected, as a count of selections: the highest was used most recently.")
   (kept :initform nil :accessor window-kept
         :documentation "What the window is kept for, a rule of where buffers go, or NIL: other buffers are shown elsewhere.")
   (quit-restore :initform nil :accessor window-quit-restore
                 :documentation "What quitting the window does: :DELETE for a window made to show its buffer, a buffer to show again, or NIL for the one shown before.")
   (history :initform '() :accessor window-history
            :documentation "The buffers the window showed before, the latest first, each (BUFFER START POINT) with anchors where it was."))
  (:documentation "A window: a rectangle showing part of a buffer, with a start position that
the redisplay keeps adjusted so that point is visible. The selected window's
point is its buffer's point; every other window keeps a point of its own."))

(setf (documentation 'window-buffer 'function) "The buffer WINDOW shows."
      (documentation 'window-start-anchor 'function) "The anchor holding the first byte WINDOW shows."
      (documentation 'window-point-anchor 'function) "The anchor of WINDOW's own point, or NIL while its point is its buffer's (the selected window)."
      (documentation 'window-width 'function) "Columns of WINDOW available for text."
      (documentation 'window-height 'function) "Rows of WINDOW available for text."
      (documentation 'window-full-height 'function) "Rows of text WINDOW has under a panel of one row, or NIL for its height."
      (documentation 'window-wrap 'function) "True when WINDOW wraps long lines."
      (documentation 'window-hscroll 'function) "Columns WINDOW is scrolled horizontally."
      (documentation 'window-goal-column 'function) "Column vertical motion in WINDOW tries to keep, or NIL."
      (documentation 'window-rows 'function) "Rows of WINDOW's last layout."
      (documentation 'window-spare-rows 'function) "Rows WINDOW showed before and shows no more, for its next layout to fill again."
      (documentation 'window-row-start-seen 'function) "What WINDOW's start was last found a row's start for (ROW-START-STATE), or NIL."
      (documentation 'window-laid-out 'function) "WINDOW's last layout, kept to be used again (KEPT-LAYOUT), or NIL."
      (documentation 'window-margin 'function) "Columns of WINDOW reserved at the left for gutters."
      (documentation 'window-divider 'function) "True when WINDOW's last column divides it from the window beside it."
      (documentation 'window-use-time 'function) "The count of selections when WINDOW was last selected."
      (documentation 'window-kept 'function) "The rule WINDOW is kept for, or NIL."
      (documentation 'window-quit-restore 'function) "What quitting WINDOW does: :DELETE, a buffer to show, or NIL."
      (documentation 'window-history 'function) "The buffers WINDOW showed before, latest first, as (BUFFER START POINT).")

(defun make-window (buffer &key (width 80) (height 24))
  "Return a window showing BUFFER with WIDTH columns and HEIGHT rows of text."
  (let ((window (make-instance 'window :buffer buffer :width width :height height)))
    (setf (window-start-anchor window)
          (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) 0 :bias :before))
    window))

(defun window-header-rows (window)
  "The rows at the top of WINDOW its buffer's header line takes: one while the
buffer has a header line (its :header-line, a function of the window giving
the line's text and spans), is not shown as a hex dump unless the line is for
the dump too (its :header-line-over-hex local), and the window has room for
it, a row of text and its mode line; else none."
  (let ((buffer (window-buffer window)))
    (if (and (tuatara.buffer:buffer-local buffer :header-line)
             (or (not (hex-view-p buffer)) (tuatara.buffer:buffer-local buffer :header-line-over-hex))
             (>= (area-height window) 3))
        1
        0)))

(defun window-text-top (window)
  "The screen row of WINDOW's first row of text: its top, or the row under its
header line (WINDOW-HEADER-ROWS)."
  (+ (area-top window) (window-header-rows window)))

(defun window-start (window)
  "Byte offset of the first character WINDOW shows; it follows the text through edits."
  (tuatara.text:anchor-position (window-start-anchor window)
                                (tuatara.buffer:buffer-version (window-buffer window))))

(defun (setf window-start) (byte window)
  "Make WINDOW show its buffer from byte offset BYTE; while the buffer is
narrowed, from within the part shown."
  (let* ((buffer (window-buffer window))
         (byte (tuatara.buffer:within-narrowing buffer byte)))
    (tuatara.text:anchor-move (window-start-anchor window) (tuatara.buffer:buffer-version buffer) byte)
    byte))

(defun call-keeping-window-starts (windows function)
  "Call FUNCTION, which puts a new text in place of a buffer's whole text and
returns the function mapping a position of the old text onto the new one (of
the position and :BEFORE), or NIL; then show each of WINDOWS from the start of
the line its first character went to, so that each stays on the same text."
  (let* ((tops (mapcar #'window-start windows))
         (forward (funcall function)))
    (when forward
      (loop for window in windows
            for top in tops
            do (let ((rope (tuatara.buffer:buffer-text (window-buffer window))))
                 (setf (window-start window)
                       (tuatara.text:rope-line-start rope (min (funcall forward top :before) (tuatara.text:rope-bytes rope)))))))))

(defun call-keeping-window-lines (windows start end function)
  "Call FUNCTION, which puts other lines in place of the lines of a buffer from
byte START below END (sorted or reordered), WINDOWS showing it; then keep each
window's first line, and its own point when it has one, on the same line of
those lines and at the same column as before, and anything after them as far
past them as it was: each window stays where it was, however the lines moved."
  (when (null windows) (return-from call-keeping-window-lines (funcall function)))
  (let* ((old (tuatara.buffer:buffer-text (window-buffer (first windows))))
         (old-bytes (tuatara.text:rope-bytes old)))
    (flet ((place (p)
             (cond ((< p start) (list :before p))
                   ((>= p end) (list :after (- old-bytes p)))
                   (t (let ((line-start (tuatara.text:rope-line-start old p)))
                        (list :inside (tuatara.text:rope-count-newlines old start line-start) (- p line-start)))))))
      (let ((spots (mapcar (lambda (window)
                             (list window (place (window-start window))
                                   (and (window-point-anchor window) (place (window-point window)))))
                           windows)))
        (multiple-value-prog1 (funcall function)
          (let* ((new (tuatara.buffer:buffer-text (window-buffer (first windows))))
                 (new-bytes (tuatara.text:rope-bytes new))
                 (new-end (+ end (- new-bytes old-bytes))))
            (flet ((back (spot)
                     (destructuring-bind (kind a &optional b) spot
                       (ecase kind
                         (:before a)
                         (:after (max 0 (- new-bytes a)))
                         (:inside (let* ((line (if (zerop a) start
                                                   (let ((newline (tuatara.text:rope-nth-newline new start new-end a)))
                                                     (if newline (1+ newline) start))))
                                         (line-end (min new-end (tuatara.text:rope-line-end new line))))
                                    (tuatara.text:rope-char-start new (min (+ line b) line-end))))))))
              (loop for (window top point) in spots
                    do (setf (window-start window) (tuatara.text:rope-line-start new (min (back top) new-bytes)))
                       (when point (setf (window-point window) (min (back point) new-bytes)))))))))))

(defun window-point (window)
  "WINDOW's point: its buffer's point while it is the selected window, else the
point it keeps of its own, which follows the text through edits."
  (let ((anchor (window-point-anchor window))
        (buffer (window-buffer window)))
    (if anchor
        (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version buffer))
        (tuatara.buffer:buffer-point buffer))))

(defun (setf window-point) (byte window)
  "Move WINDOW's point to byte offset BYTE."
  (let ((anchor (window-point-anchor window))
        (buffer (window-buffer window)))
    (if anchor
        (tuatara.text:anchor-move anchor (tuatara.buffer:buffer-version buffer) byte)
        (setf (tuatara.buffer:buffer-point buffer) byte))
    byte))

(defvar *window-history-length* 20
  "How many buffers a window remembers having shown, to show them again where they were.")

(defun remember-window-buffer (window)
  "Put WINDOW's buffer first in the window's history, with where its start and
point are now."
  (let* ((buffer (window-buffer window))
         (version (tuatara.buffer:buffer-version buffer))
         (entry (list buffer
                      (tuatara.text:make-anchor version (window-start window) :bias :before)
                      (tuatara.text:make-anchor version (window-point window) :bias :after)))
         (history (cons entry (remove buffer (window-history window) :key #'first))))
    (setf (window-history window)
          (if (> (length history) *window-history-length*)
              (subseq history 0 *window-history-length*)
              history))))

(defun set-window-buffer (window buffer &key (remember t))
  "Show BUFFER in WINDOW: where the window last showed it, when it did, else
from its first line with the buffer's point. The buffer shown before goes first
in the window's history, unless REMEMBER is false. Returns BUFFER."
  (unless (eq buffer (window-buffer window))
    (when remember (remember-window-buffer window))
    (let* ((entry (find buffer (window-history window) :key #'first))
           (version (tuatara.buffer:buffer-version buffer))
           (point (if entry
                      (tuatara.text:anchor-position (third entry) version)
                      (tuatara.buffer:buffer-point buffer))))
      (setf (slot-value window 'buffer) buffer
            (window-start-anchor window)
            (tuatara.text:make-anchor version (if entry (tuatara.text:anchor-position (second entry) version) 0)
                                      :bias :before)
            (window-hscroll window) 0
            (window-goal-column window) nil
            (window-history window) (remove entry (window-history window)))
      (if (window-point-anchor window)
          (setf (window-point-anchor window) (tuatara.text:make-anchor version point :bias :after))
          (setf (tuatara.buffer:buffer-point buffer) point))))
  buffer)

(defun (setf window-buffer) (buffer window)
  "Show BUFFER in WINDOW (SET-WINDOW-BUFFER)."
  (set-window-buffer window buffer))

(defun forget-window-buffer (window buffer)
  "Take BUFFER out of WINDOW's history."
  (setf (window-history window) (remove buffer (window-history window) :key #'first)))

(defun window-state (window)
  "What WINDOW shows and how, as a list for SET-WINDOW-STATE: its buffer, start,
point, scroll, wrapping, goal column, what it is kept for, what quitting it
does and its history."
  (list (window-buffer window) (window-start-anchor window) (window-point-anchor window)
        (window-hscroll window) (window-wrap window) (window-goal-column window)
        (window-kept window) (window-quit-restore window) (window-history window)))

(defun set-window-state (window state)
  "Make WINDOW show what STATE, from WINDOW-STATE, describes."
  (destructuring-bind (buffer start point hscroll wrap goal kept quit history) state
    (setf (slot-value window 'buffer) buffer
          (window-start-anchor window) start
          (window-point-anchor window) point
          (window-hscroll window) hscroll
          (window-wrap window) wrap
          (window-goal-column window) goal
          (window-kept window) kept
          (window-quit-restore window) quit
          (window-history window) history)))

(defvar *tab-width-function* nil
  "A function of a buffer returning the columns a tab takes in it, from one
tab stop to the next; NIL for 8 in every buffer.")

(defun sensible-tab-width (width default)
  "WIDTH when it is a tab width the screen can show, a whole number from 1 to
64; else DEFAULT."
  (if (and (integerp width) (<= 1 width 64)) width default))

(defun tab-width-of (buffer)
  "The columns a tab takes in BUFFER, as *TAB-WIDTH-FUNCTION* says: a whole
number from 1 to 64, 8 when it says nothing sensible."
  (sensible-tab-width (and *tab-width-function* (funcall *tab-width-function* buffer)) 8))

(defvar *soft-wrap-function* nil
  "A function of a buffer saying how its lines longer than the window wrap: NIL
for not at all (cut off at the window's edge and scrolled sideways), or at a
word's edge, T at the window's edge or a column at that column; NIL for no
wrapping in any buffer.")

(defun soft-wrap-of (buffer)
  "How BUFFER's long lines wrap at a word's edge, as *SOFT-WRAP-FUNCTION*
says: NIL, T for the window's edge, or a column."
  (and *soft-wrap-function* (funcall *soft-wrap-function* buffer)))

(defun window-wraps-p (window)
  "True when WINDOW wraps long lines: set for the window (WINDOW-WRAP), or at a
word's edge as its buffer wants (SOFT-WRAP-OF)."
  (or (window-wrap window) (and (soft-wrap-of (window-buffer window)) t)))

(defvar *invisible-characters-function* nil
  "A function of a buffer, true when the characters that draw nothing are
shown in its text as their codes (<FEFF>, `*invisible-characters*'); NIL for
not in any buffer.")

(defun invisible-characters-of (buffer)
  "True when BUFFER shows the characters that draw nothing as their codes, as
*INVISIBLE-CHARACTERS-FUNCTION* says."
  (and *invisible-characters-function* (funcall *invisible-characters-function* buffer) t))

(defvar *layout-probe* nil
  "True while a window's buffer is laid out only to find where a row starts,
one line at a time (WINDOW-LAYOUT-ROWS with END): what the buffer shows
otherwise may be looked up then, but what is kept for the window as it is
shown is left as it is.")

(defun window-layout-rows (window start height &key end spares)
  "WINDOW's buffer laid out from START into at most HEIGHT rows as WINDOW shows
it: its width, its wrapping (at a word's edge and a column when its buffer
wants), its tab width and its line ends; while the frontend draws rows large
\(*ROW-SCALING*), its lines as many rows tall as its :line-scale-function says.
With END, only the rows up to that octet are wanted: none is laid out that
begins past it, and what the buffer hides or shows otherwise is looked for no
further. With SPARES, the rows are filled from WINDOW's rows no longer shown
\(WINDOW-SPARE-ROWS) while it has any. Of a narrowed buffer, only the part
shown is laid out."
  (let* ((buffer (window-buffer window))
         (visual (soft-wrap-of buffer))
         (wrap (or (window-wrap window) (and visual t))))
    (layout (tuatara.buffer:buffer-text buffer) start
            (window-text-width window) height
            :wrap wrap :wrap-column (and (integerp visual) visual) :word-wrap (and visual t)
            :hscroll (if wrap 0 (window-hscroll window))
            :tab-width (tab-width-of buffer)
            :crlf (tuatara.buffer:buffer-crlf-p buffer)
            :until end
            :text-end (and (tuatara.buffer:buffer-narrowing buffer) (tuatara.buffer:buffer-end buffer))
            :invisible (invisible-characters-of buffer)
            :spares (and spares (window-spare-rows window))
            :hidden (let ((*layout-probe* (and end t))) (window-hidden buffer start (window-width window) height end))
            :prefix (let ((function (tuatara.buffer:buffer-local buffer :line-prefix-function)))
                      (and function (lambda (line-start continuation) (funcall function buffer line-start continuation))))
            :scale (let ((function (and *row-scaling* (tuatara.buffer:buffer-local buffer :line-scale-function))))
                     (and function (lambda (line-start) (funcall function buffer line-start)))))))

(defun window-line-scale (window line-start)
  "How many rows tall WINDOW draws the line of its buffer beginning at
LINE-START, as the buffer's :line-scale-function says while the frontend
draws rows large (*ROW-SCALING*); 1 otherwise."
  (let* ((buffer (window-buffer window))
         (function (and *row-scaling* (tuatara.buffer:buffer-local buffer :line-scale-function))))
    (if function (max 1 (or (funcall function buffer line-start) 1)) 1)))

(defun window-text-width (window)
  "The columns WINDOW's text rows hold: its width less its margin, and no more
than its buffer's :text-width when that is set (the text shown centred in a
column); one at least."
  (let ((limit (tuatara.buffer:buffer-local (window-buffer window) :text-width))
        (room (- (window-width window) (window-margin window))))
    (max 1 (if limit (min limit room) room))))

(defun window-scaling-p (window)
  "True while WINDOW may draw rows large: the frontend does (*ROW-SCALING*)
and its buffer says how tall (:line-scale-function)."
  (and *row-scaling* (tuatara.buffer:buffer-local (window-buffer window) :line-scale-function) t))

(defun line-prefix-columns (buffer line-start)
  "How many columns the prefix BUFFER shows before its line beginning at
LINE-START takes (its :line-prefix-function, an indentation, as Org's
indented view shows); 0 with none."
  (let* ((function (tuatara.buffer:buffer-local buffer :line-prefix-function))
         (text (and function (funcall function buffer line-start nil))))
    (if text (string-columns text) 0)))

(defun hidden-in-reach (bounds start height)
  "The stretches of BOUNDS (HIDE-RANGES) a window HEIGHT rows high showing
text from START could reach, each (START . END), in order: those ending
after START, as many as there are rows and two more, since a line shown
stands between each two. O(log n + HEIGHT)."
  (loop for k from (hidden-index bounds (lambda (s e) (declare (ignore s)) (> e start)))
        for stretch = (hidden-stretch bounds k)
        repeat (+ 2 (max 1 height))
        while stretch
        collect stretch))

(defun window-hidden (buffer start width height &optional end)
  "What a window showing BUFFER from START, WIDTH by HEIGHT, does not show as
it is, for LAYOUT's HIDDEN: BUFFER's folds, the lines search filters hide as
far as the window could reach (HIDDEN-IN-REACH), and what its
:display-replacements-function shows otherwise (each (START END TEXT FACE))
from START on, as far as the window could reach or to END, asked for each
stretch shown between the folds alone, never the text they hide; one inside
a fold left out."
  (let* ((bounds (hidden-bounds buffer))
         (folds (and (tuatara.buffer:buffer-local buffer :folds) (buffer-folds buffer)))
         (folds (if bounds (merge 'list (copy-list folds) (hidden-in-reach bounds start height) #'< :key #'car) folds))
         (function (tuatara.buffer:buffer-local buffer :display-replacements-function)))
    (if (null function)
        folds
        (let* ((budget (* 8 (max 1 width) (max 1 height)))
               (limit (or end (tuatara.buffer:buffer-size buffer)))
               ;; the stretches shown between the folds, as far as the window could reach:
               ;; that many shown octets, never the text the folds and filters hide
               (shown (let ((at start) (out '()))
                        (dolist (fold folds)
                          (when (>= at limit) (return))
                          (when (> (cdr fold) at)
                            (when (> (car fold) at)
                              (let ((to (min (car fold) (+ at budget) limit)))
                                (push (cons at to) out)
                                (decf budget (- to at))
                                (when (<= budget 0) (return))))
                            (setf at (cdr fold))))
                        (when (and (plusp budget) (< at limit))
                          (push (cons at (min limit (+ at budget))) out))
                        (nreverse out)))
               (replacements (loop for (from . to) in shown
                                   when (< from to)
                                     nconc (copy-list (funcall function buffer from to))))
               ;; those inside a fold left out, both lists being in order
               (kept (let ((rest folds) (out '()))
                       (dolist (r replacements (nreverse out))
                         (loop while (and rest (<= (cdr (first rest)) (first r))) do (pop rest))
                         (unless (and rest (< (car (first rest)) (second r)))
                           (push r out))))))
          ;; lists of their own: the folds are kept, and what the function gave may be kept by it
          (merge 'list (copy-list folds) kept #'< :key #'car)))))

(defun row-starts (window from position)
  "The starts of the rows WINDOW lays out from FROM, where a row begins, as far
as the one holding POSITION, on the same line, in order; POSITION itself last
when no row holds it (the rows make no headway)."
  (let ((rope (tuatara.buffer:buffer-text (window-buffer window)))
        (starts (make-array 16 :element-type 'fixnum :adjustable t :fill-pointer 0)))
    (loop with start = from
          do (let* ((rows (window-layout-rows window start 256 :end (tuatara.text:rope-line-end rope position)))
                    (last (and (plusp (length rows)) (aref rows (1- (length rows))))))
               (loop for row across rows
                     do (vector-push-extend (row-start row) starts)
                        (when (< position (row-end row)) (return-from row-starts starts)))
               (when (or (null last) (row-last-p last) (<= (row-end last) start))
                 (vector-push-extend position starts)
                 (return starts))
               (setf start (row-end last))))))

(defun window-row-start (window position)
  "Where the row of WINDOW holding POSITION starts, as WINDOW lays out its
buffer: the line's start, or where its lines wrap, the wrap point at or before
POSITION, read from the line's start or, on a long line, from the last place
before POSITION its rows are worked out from (WRAP-ANCHOR). Complexity:
O(log n + *WRAP-STRETCH*)."
  (let* ((rope (tuatara.buffer:buffer-text (window-buffer window)))
         (line (tuatara.text:rope-line-start rope position)))
    (if (or (not (window-wraps-p window)) (= line position))
        line
        (let ((starts (row-starts window (wrap-anchor rope line position) position)))
          (aref starts (1- (length starts)))))))

(defun layout-state (window)
  "What decides how WINDOW lays out its buffer, but for where from: the
buffer's text, the part of it shown when it is narrowed, and its settings
for laying it out, the window's width, height, wrapping and sideways
scroll."
  (let ((buffer (window-buffer window)))
    (list (tuatara.buffer:buffer-version buffer) (window-text-width window) (window-height window)
          (window-hscroll window) (window-wrap window) (soft-wrap-of buffer) (tab-width-of buffer)
          (tuatara.buffer:buffer-line-ending buffer) (invisible-characters-of buffer)
          (tuatara.buffer:buffer-local buffer :folds) (hidden-bounds buffer) (tuatara.buffer:buffer-local buffer :line-prefix-function)
          (and *row-scaling* (tuatara.buffer:buffer-local buffer :line-scale-function))
          (tuatara.buffer:buffer-narrowing buffer))))

(defun row-start-state (window)
  "What decides whether WINDOW's start is where one of its rows starts: its
start, and how it lays out its buffer (LAYOUT-STATE)."
  (cons (window-start window) (layout-state window)))

(defun rows-back (window position count)
  "Where the row starts that is above the one of WINDOW holding POSITION by
COUNT rows' height, a row drawn large counting for as much as it is tall, as
WINDOW lays out its buffer (WINDOW-ROW-START); the start of the text shown
at most.
Complexity: O(COUNT × (log n + *WRAP-STRETCH*)) at most, the rows of a stretch
of a line laid out once."
  (let ((rope (tuatara.buffer:buffer-text (window-buffer window)))
        (start (window-row-start window position))
        (left count))
    (loop with top = (tuatara.buffer:buffer-start (window-buffer window))
          while (and (plusp left) (> start top))
          do (let* ((before (1- start))
                    ;; past the lines hidden above
                    (line (if (buffer-hides-p (window-buffer window))
                              (visible-line-start (window-buffer window) before)
                              (tuatara.text:rope-line-start rope before)))
                    (scale (window-line-scale window line))
                    (starts (if (window-wraps-p window)
                                (row-starts window (wrap-anchor rope line before) before)
                                (vector line))))
               ;; the rows above, nearest first; a row taller than the height left stays out
               (loop for i from (1- (length starts)) downto 0
                     do (when (or (> scale left) (>= (aref starts i) start)) (return-from rows-back start))
                        (setf start (aref starts i))
                        (decf left scale)
                        (unless (plusp left) (return)))))
    start))

(defun window-layout (window)
  "Lay out WINDOW's buffer from its start, store and return the rows: as text,
or as a hex dump (HEX-LAYOUT) when the buffer is in the hex view."
  (let ((buffer (window-buffer window)))
    ;; never from outside the part a narrowed buffer shows
    (when (tuatara.buffer:buffer-narrowing buffer)
      (setf (window-start window) (window-start window)))
    ;; never from inside a fold: from the line it begins on
    (let ((fold (and (buffer-hides-p buffer) (not (hex-view-p buffer))
                     (or (fold-at buffer (window-start window)) (hidden-around buffer (window-start window))))))
      (when fold
        (setf (window-start window) (tuatara.text:rope-line-start (tuatara.buffer:buffer-text buffer) (car fold)))))
    (let ((old (window-rows window))
          (rows (if (hex-view-p buffer)
                    (progn
                      ;; the rows kept for the text view may be filled with the dump's
                      (setf (window-laid-out window) nil)
                      (hex-layout (tuatara.buffer:buffer-text buffer) (window-start window) (window-height window)
                                  (window-spare-rows window)))
                    (kept-layout window))))
      ;; the rows shown before and not now: filled again by the next layout
      (loop for row across old
            unless (find row rows :test #'eq)
              do (vector-push-extend row (window-spare-rows window)))
      (setf (window-rows window) rows))))

(defun layout-kept-p (window)
  "True when WINDOW's last layout may be used again (KEPT-LAYOUT): its buffer
has no folds, nothing its display replacements show otherwise (as Org's
links, which depend on where point is), no prefix before its lines and no
lines drawn large, all of which can change while LAYOUT-STATE does not."
  (let ((buffer (window-buffer window)))
    (not (or (tuatara.buffer:buffer-local buffer :folds)
             (tuatara.buffer:buffer-local buffer :display-replacements-function)
             (tuatara.buffer:buffer-local buffer :line-prefix-function)
             (window-scaling-p window)))))

(defun row-base (row)
  "What ROW's faces were as laid out, before highlights were painted on them
\(APPLY-HIGHLIGHTS): NIL for none, else (FACES . EXTEND), a copy."
  (if (and (every #'null (row-faces row)) (null (row-extend row)))
      nil
      (cons (copy-seq (row-faces row)) (row-extend row))))

(defun restore-row (row base)
  "Give ROW back the faces it was laid out with, BASE (ROW-BASE)."
  (if base
      (setf (row-faces row) (replace (row-faces row) (car base)) (row-extend row) (cdr base))
      (setf (row-faces row) (fill (row-faces row) nil) (row-extend row) nil)))

(defun rows-height (rows)
  "How many rows' height ROWS take, each as tall as it is drawn (ROW-SCALE)."
  (loop for row across rows sum (row-scale row)))

(defun kept-layout (window)
  "WINDOW's buffer laid out from its start, as WINDOW-LAYOUT-ROWS does, with
the rows of its last layout used again while it is laid out alike
\(LAYOUT-STATE) and may be (LAYOUT-KEPT-P), their faces as laid out: all of them when the start is where
it was; when the start moved forward to where one of them begins, the rows
from there, and those after them laid out, where they begin a line; when it
moved back, the rows before them laid out, where those end where they begin.
Kept for the next time (WINDOW-LAID-OUT). Complexity: that of laying out the
rows the window moved by, where rows are used again."
  (let* ((state (layout-state window))
         (start (window-start window))
         (height (window-height window))
         (keep (layout-kept-p window))
         (kept (window-laid-out window))
         (old (and kept keep (equal state (first kept)) (third kept)))
         (old-start (second kept))
         (rope (tuatara.buffer:buffer-text (window-buffer window)))
         ;; the rows used again: from old row FROM, COUNT of them, at row AT of the new layout
         (from 0) (count 0) (at 0))
    (flet ((line-start-p (position) (= position (tuatara.text:rope-line-start rope position))))
      (let* ((rows
               (cond ((or (null old) (zerop (length old))) nil)
                     ((= start old-start)
                      (setf count (length old))
                      old)
                     ((< old-start start)
                      (let ((k (position start old :key #'row-start :start 1))
                            (last (aref old (1- (length old)))))
                        (when (and k (or (row-last-p last) (line-start-p (row-end last))))
                          (setf from k count (- (length old) k))
                          (let ((used (subseq old k)))
                            (if (row-last-p last)
                                used
                                (concatenate 'simple-vector used
                                             (window-layout-rows window (row-end last) (- height (rows-height used)) :spares t)))))))
                     (t
                      (let* ((before (window-layout-rows window start height :end (1- old-start) :spares t))
                             (last (and (plusp (length before)) (aref before (1- (length before))))))
                        (when (and last (= (row-end last) old-start) (not (row-last-p last)))
                          (let ((room (- height (rows-height before))))
                            (setf at (length before)
                                  count (loop for row across old
                                              while (<= (row-scale row) room)
                                              do (decf room (row-scale row))
                                              count t))
                            (concatenate 'simple-vector before (subseq old 0 count))))))))
             (rows (or rows (progn (setf count 0) (window-layout-rows window start height :spares t))))
             (bases (make-array (length rows) :initial-element nil)))
        (dotimes (i (length rows))
          (setf (aref bases i)
                (if (and (<= at i) (< i (+ at count)))
                    (let ((base (aref (fourth kept) (+ from (- i at)))))
                      (restore-row (aref rows i) base)
                      base)
                    (row-base (aref rows i)))))
        ;; a layout that may not be used again is not kept either: what made it so may be gone next time
        (setf (window-laid-out window) (and keep (list state start rows bases)))
        rows))))

(defun window-point-position (window)
  "Two values: row and column of point in WINDOW's last layout, or NIL when
point is not visible. In the hex view, the hex point's: on its hex digits, the
second when the first was typed (:HEX-NIBBLE), or on its character in the text
beside them when typing goes there (:HEX-COLUMN :TEXT)."
  (let ((buffer (window-buffer window)))
    (if (hex-view-p buffer)
        (let ((p (hex-point buffer)))
          (multiple-value-bind (row column) (point-row-column (window-rows window) p)
            (cond ((null row) nil)
                  ((eq (tuatara.buffer:buffer-local buffer :hex-column) :text)
                   (values row (or (position p (row-bytes (aref (window-rows window) row)) :from-end t) column)))
                  ((let ((half (tuatara.buffer:buffer-local buffer :hex-nibble)))
                     (and half (eql p (tuatara.text:anchor-position half (tuatara.buffer:buffer-version buffer)))))
                   (values row (1+ column)))
                  (t (values row column)))))
        ;; on a row drawn large, its column counted in the window's own (LARGE-COLUMN)
        (multiple-value-bind (row column) (point-row-column (window-rows window) (window-point window))
          (if row
              (values row (large-column column (row-scale (aref (window-rows window) row))))
              nil)))))

(defun window-rows-drawn (window)
  "How many of WINDOW's rows (its last layout) it draws, from the first, each
as tall as it is drawn (ROW-SCALE), as far as its height holds them; the rest
are left out (a panel may have taken rows the layout had)."
  (let ((rows (window-rows window)) (height (window-height window)) (shift 0))
    (loop for row across rows
          for r from 0
          do (when (> (+ r shift (row-scale row)) height) (return r))
             (incf shift (1- (row-scale row)))
          finally (return (length rows)))))

(defun large-column (column scale)
  "The window's column standing for COLUMN of a row drawn SCALE times as
wide: the first whose cell begins within that character, so that dividing it
by SCALE, rounding down, gives COLUMN again."
  (ceiling (* column scale)))

(defun hex-ensure-point-visible (window)
  "WINDOW-ENSURE-POINT-VISIBLE for the hex view: rows of +HEX-ROW+ octets, the
hex point's row brought into the window."
  (let* ((buffer (window-buffer window))
         (point (hex-point buffer))
         (row (hex-row-start point))
         (height (window-height window)))
    (setf (window-start window) (hex-row-start (min (window-start window) (tuatara.buffer:buffer-size buffer)))
          (window-hscroll window) 0)
    (cond ((< row (window-start window)) (setf (window-start window) row))
          ((>= row (+ (window-start window) (* height +hex-row+)))
           (setf (window-start window) (max 0 (- row (* (1- height) +hex-row+))))))
    (window-layout window)))

(defun scroll-lines (rope start n)
  "The start of the line N lines after (or before, if negative) the line
containing START: by the line counts, or while they are being made a line at
a time."
  (if (tuatara.text:rope-indexed-p rope)
      (let* ((line (tuatara.text:byte->line rope start))
             (target (max 0 (min (+ line n) (tuatara.text:rope-lines rope)))))
        (tuatara.text:line->byte rope target))
      (step-lines rope start n)))

(defun step-lines (rope start n)
  "The start of the line N lines after (or before, if negative) the line
containing START in ROPE, found a line at a time: the first or last line when
there are fewer."
  (let ((line (tuatara.text:rope-line-start rope start)) (size (tuatara.text:rope-bytes rope)))
    (if (minusp n)
        (loop repeat (- n)
              while (plusp line)
              do (setf line (tuatara.text:rope-line-start rope (1- line))))
        (loop repeat n
              do (let ((end (tuatara.text:rope-line-end rope line)))
                   (if (< end size) (setf line (1+ end)) (return)))))
    line))

(defvar *scroll-margin* 0
  "How many rows a window keeps between point and its top and bottom edges:
moving point nearer an edge scrolls the window, as long as there is text
beyond that edge. At most what leaves a row for point between the two
margins.")

(defvar *scroll-recenter* nil
  "True to put point's line in the middle of the window whenever point leaves
the window or comes within *SCROLL-MARGIN* rows of its edge, as \\[recenter]
does; NIL to scroll only as far as keeps point in view, that many rows from
the edge.")

(defun window-scroll-margin (window)
  "How many rows WINDOW keeps between point and its top and bottom edges:
*SCROLL-MARGIN*, at most what leaves a row for point between them."
  (max 0 (min *scroll-margin* (floor (1- (window-height window)) 2))))

(defun centred-rows (window point)
  "How many rows of WINDOW are above point's row when that row is in the
middle of it: half its height, less what more a row drawn large takes."
  (- (floor (window-height window) 2)
     (1- (window-line-scale window (tuatara.text:rope-line-start (tuatara.buffer:buffer-text (window-buffer window)) point)))))

(defun rows-above-when-below (window point margin)
  "How many rows of WINDOW are above point's row once it has scrolled forward
to show point: half of them with *SCROLL-RECENTER*; else all but point's and
MARGIN more, fewer where the text ends before MARGIN rows follow point's."
  (if *scroll-recenter*
      (centred-rows window point)
      (let ((after (if (plusp margin)
                       (max 0 (1- (length (window-layout-rows window (window-row-start window point) (1+ margin)))))
                       0)))
        (- (window-height window)
           (window-line-scale window (tuatara.text:rope-line-start (tuatara.buffer:buffer-text (window-buffer window)) point))
           (min margin after)))))

(defun margin-start (window rows row margin)
  "Where WINDOW's start goes when point, on ROW of its layout ROWS, is within
MARGIN rows of the top or the bottom of the window with text beyond that edge
(WINDOW-ENSURE-POINT-VISIBLE); NIL when it is not."
  (let* ((point (window-point window))
         (above (loop for i below row sum (row-scale (aref rows i))))
         (below (- (window-height window) above (row-scale (aref rows row)))))
    (cond ((zerop margin) nil)
          ((and (< above margin) (plusp (window-start window)))
           (rows-back window point (if *scroll-recenter* (centred-rows window point) margin)))
          ((and (< below margin) (not (row-last-p (aref rows (1- (length rows))))))
           (rows-back window point (rows-above-when-below window point margin))))))

(defun keep-point-column-visible (window)
  "Scroll WINDOW sideways, its lines being cut off at the edge, so that the
character at point shows whole: point's column inside the width, less what
the line's prefix takes, and as many times fewer columns as the line is
drawn large."
  (let* ((buffer (window-buffer window))
         (rope (tuatara.buffer:buffer-text buffer))
         (point (window-point window))
         (line-start (tuatara.text:rope-line-start rope point))
         (tab-width (tab-width-of buffer))
         (crlf (tuatara.buffer:buffer-crlf-p buffer))
         (invisible (invisible-characters-of buffer))
         (column (line-screen-column rope line-start point :tab-width tab-width :crlf crlf :invisible invisible))
         (width (window-text-width window))
         (room (max 1 (- (floor width (window-line-scale window line-start))
                         (line-prefix-columns buffer line-start)))))
    (cond ((and (plusp (window-hscroll window)) (< column (1- room)))
           (setf (window-hscroll window) 0))
          ((< column (window-hscroll window))
           (setf (window-hscroll window) (max 0 (- column (floor room 2)))))
          ((>= (- column (window-hscroll window)) room)
           (setf (window-hscroll window) (- column (floor room 2)))))
    ;; scrolled on until the character at point fits whole in the room as the row draws
    ;; it: a tab or a wide character at the edge, or past the line's reach, where a
    ;; character counts one column though a wide one or a tab is drawn wider
    (let* ((after (if (tuatara.text:rope-line-text-ends-p rope point crlf)
                      point
                      (tuatara.text:rope-next-boundary rope point))))
      (loop for back = (- column (window-hscroll window)) then (floor back 2)
            do (setf (window-hscroll window) (- column back))
               (multiple-value-bind (start at)
                   (skip-columns rope line-start line-start 0 (window-hscroll window) point
                                 :tab-width tab-width :crlf crlf :invisible invisible)
                 (let ((shown (- (nth-value 1 (scan-columns rope start after tab-width
                                                            :column at :limit (+ at room 1) :crlf crlf
                                                            :invisible invisible))
                                 at)))
                   (when (or (zerop back) (if (= after point) (< shown room) (<= shown room)))
                     (return))))))))

(defun window-ensure-point-visible (window)
  "Adjust WINDOW's start and horizontal scroll so that point is shown, then lay
out. When point is above the window, or within *SCROLL-MARGIN* rows of its
top, the window scrolls back so point's row is that many rows from the top;
when below it, or within that many rows of its bottom, forward so point's row
is that many rows from the bottom, the end of the text no further up than the
last row. With *SCROLL-RECENTER*, point's row goes to the middle instead.
Where lines are cut off at the edge, it scrolls sideways until the character
at point shows whole. Returns the rows. Complexity: that of a layout, and
O(*SCREEN-COLUMN-REACH* + log n) for the columns of point's line, however
long it is."
  (let* ((buffer (window-buffer window))
         (rope (tuatara.buffer:buffer-text buffer))
         (point (window-point window))
         (margin (window-scroll-margin window)))
    (when (hex-view-p buffer)
      (return-from window-ensure-point-visible (hex-ensure-point-visible window)))
    ;; keep start valid after edits
    (setf (window-start window) (min (window-start window) (tuatara.text:rope-bytes rope)))
    ;; a line's start, or where lines wrap, a row's, unless nothing has changed since it was one
    (if (window-wraps-p window)
        (unless (equal (window-row-start-seen window) (row-start-state window))
          (setf (window-start window) (window-row-start window (window-start window))
                (window-row-start-seen window) (row-start-state window)))
        (setf (window-start window) (window-row-start window (window-start window))))
    (when (< point (window-start window))
      (setf (window-start window) (if (or *scroll-recenter* (plusp margin))
                                      (rows-back window point (if *scroll-recenter* (centred-rows window point) margin))
                                      (window-row-start window point))))
    (unless (window-wraps-p window)
      (keep-point-column-visible window))
    (loop do (window-layout window)
             (let ((rows (window-rows window)))
               (multiple-value-bind (row column) (window-point-position window)
                 (declare (ignore column))
                 (cond (row
                        (let ((start (margin-start window rows row margin)))
                          (when (and start (/= start (window-start window)))
                            (setf (window-start window) start)
                            (window-layout window)))
                        (return (window-rows window)))
                       ((zerop (length rows)) (return rows))
                       ;; point's row is in the window, point itself past where the row is cut: no further
                       ((find-if (lambda (row) (and (<= (row-start row) point) (< point (row-end row)))) rows)
                        (return rows))
                       (t
                        ;; point is below: scroll forward. Without wrapping every row is a
                        ;; line, so jump straight; with wrapping, step by rows.
                        (let ((last (aref rows (1- (length rows))))
                              (above (rows-above-when-below window point margin)))
                          (cond ((row-last-p last) (return rows)) ; cannot happen: point would be visible
                                ((window-wraps-p window)
                                 ;; straight to the row that puts point's where it goes, rows being read back from it
                                 (let ((start (rows-back window point above)))
                                   (let ((next (if (> start (window-start window)) start (row-end (aref rows 0)))))
                                     ;; a start that would not move on: no further
                                     (when (<= next (window-start window)) (return rows))
                                     (setf (window-start window) next))))
                                (t
                                 (let ((start (cond ((or (window-scaling-p window) (buffer-hides-p (window-buffer window)))
                                                     ;; lines drawn large count for their heights, hidden ones for none
                                                     (rows-back window point above))
                                                    ((tuatara.text:rope-indexed-p rope)
                                                     (let* ((point-line (tuatara.text:byte->line rope point))
                                                            (top-line (max 0 (- point-line above))))
                                                       (tuatara.text:line->byte rope top-line)))
                                                    ;; while the lines are counted: back a line at a time
                                                    (t (step-lines rope point (- above))))))
                                   ;; lines drawn large take more rows: on a line at a time from there
                                   (let ((next (if (> start (window-start window)) start (row-end (aref rows 0)))))
                                     (when (<= next (window-start window)) (return rows))
                                     (setf (window-start window) next)))))))))))))

(defun screen-slice-keys (crlf tab-width &optional invisible)
  "The keys TUATARA.TEXT:MAP-LINE-SLICES takes to cut lines at columns on the
screen: CRLF when a carriage return before a newline ends them, a tab TAB-WIDTH
columns at most, each other character as wide as it is shown, with INVISIBLE
one that draws nothing as its code (CODE-COLUMNS), a grapheme cluster as its
first character (CLUSTER-COLUMNS)."
  (list :crlf crlf
        :width (lambda (code column before) (advance-column code column before tab-width invisible))))

(defun buffer-slice-keys (buffer)
  "The keys TUATARA.TEXT:MAP-LINE-SLICES takes to cut BUFFER's lines at
columns on the screen (SCREEN-SLICE-KEYS): its line ends, its tab width,
whether it shows the characters that draw nothing as their codes."
  (screen-slice-keys (tuatara.buffer:buffer-crlf-p buffer) (tab-width-of buffer)
                     (invisible-characters-of buffer)))

(defun buffer-screen-column (buffer position)
  "The column on the screen of POSITION in BUFFER: a tab to its stop (BUFFER's
tab width), a wide character two, a combining mark none; past the first
*SCREEN-COLUMN-REACH* octets of a line, a character one (LINE-SCREEN-COLUMN);
a character that draws nothing as its code while BUFFER shows it so."
  (line-screen-column (tuatara.buffer:buffer-text buffer) (tuatara.buffer:buffer-line-start buffer position) position
                      :tab-width (tab-width-of buffer)
                      :crlf (tuatara.buffer:buffer-crlf-p buffer)
                      :invisible (invisible-characters-of buffer)))

(defun buffer-screen-slice (buffer line-start left right)
  "The part of BUFFER's line starting at LINE-START between the columns LEFT
and RIGHT on the screen, as four values: where it starts and ends, and the
columns it starts and ends at (TUATARA.TEXT:MAP-LINE-SLICES): a tab across an
edge in it, a wide character across one out of it, the line's end when the
line is shorter."
  (let ((slice (list line-start line-start 0 0)))
    (apply #'tuatara.text:map-line-slices
           (lambda (line s e sc ec line-end) (declare (ignore line line-end)) (setf slice (list s e sc ec)))
           (tuatara.buffer:buffer-text buffer) line-start (1+ line-start) left right
           (buffer-slice-keys buffer))
    (values-list slice)))

(defun buffer-move-to-screen-column (buffer line-start column)
  "Where COLUMN on the screen is on BUFFER's line starting at LINE-START:
before the character there, after a tab, wide character or grapheme cluster
\(a letter with its accents, an emoji sequence) that spans it, the line's end
when it is shorter."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (at (nth-value 1 (buffer-screen-slice buffer line-start column column)))
         (cluster (if (or (= at line-start) (>= at (tuatara.text:rope-bytes rope)))
                      at
                      (tuatara.text:rope-previous-boundary rope (tuatara.text:rope-next-boundary rope at) t))))
    (if (< cluster at) (tuatara.text:rope-next-boundary rope cluster t) at)))

(defun scroll-rows (window start count)
  "Where WINDOW's start goes, scrolled COUNT rows from START as it lays out its
buffer (back when negative): the text's first row at least, its last at most."
  (if (plusp count)
      (let ((rows (window-layout-rows window start (1+ count))))
        (if (plusp (length rows)) (row-start (aref rows (1- (length rows)))) start))
      (rows-back window start (- count))))

(defun window-scroll (window lines)
  "Scroll WINDOW by LINES (negative scrolls up), by the rows it shows where it
wraps long lines, and move point into the window if it left it. Returns the
new start."
  (let* ((buffer (window-buffer window))
         (rope (tuatara.buffer:buffer-text buffer)))
    (when (hex-view-p buffer)
      ;; rows of octets: the hex point moves with the window
      (let* ((size (tuatara.buffer:buffer-size buffer))
             (start (max 0 (min (hex-row-start size) (+ (window-start window) (* lines +hex-row+))))))
        (setf (window-start window) start
              (hex-point buffer) (min size (max start (+ (hex-point buffer) (* lines +hex-row+)))))
        (window-layout window)
        (return-from window-scroll start)))
    (setf (window-start window)
          ;; by rows where lines wrap or some are hidden (folds, search filters)
          (if (or (window-wraps-p window) (buffer-hides-p buffer))
              (scroll-rows window (window-start window) lines)
              (scroll-lines rope (window-start window) lines)))
    (window-layout window)
    ;; point kept *SCROLL-MARGIN* rows from the edges, where there is text beyond them
    (let* ((rows (window-rows window)) (point (window-point window))
           (margin (min (window-scroll-margin window) (max 0 (1- (length rows))))))
      (when (plusp (length rows))
        (let* ((end (row-last-p (aref rows (1- (length rows)))))
               (first (aref rows (if (plusp (window-start window)) margin 0)))
               (last (aref rows (- (length rows) 1 (if end 0 margin)))))
          (cond ((< point (row-start first))
                 (setf (window-point window) (row-start first)))
                ((and (>= point (row-end last)) (not (row-last-p last)))
                 (setf (window-point window) (row-start last)))))))
    (window-start window)))

(defun window-recenter (window)
  "Scroll WINDOW so that point's line is in the middle; where it wraps long
lines, point's row."
  (let* ((buffer (window-buffer window))
         (rope (tuatara.buffer:buffer-text buffer))
         (point (window-point window)))
    (setf (window-start window)
          (cond ((hex-view-p buffer)
                 (max 0 (- (hex-row-start (hex-point buffer)) (* +hex-row+ (floor (window-height window) 2)))))
                ((or (window-wraps-p window) (window-scaling-p window) (buffer-hides-p buffer))
                 (rows-back window point (centred-rows window point)))
                (t (scroll-lines rope (tuatara.text:rope-line-start rope point) (- (centred-rows window point))))))
    (window-layout window)))
