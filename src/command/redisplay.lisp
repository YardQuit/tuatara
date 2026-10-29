;;;; src/command/redisplay.lisp --- Painting a frame: the windows, their mode lines, the panel and the cursor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.command)

(defvar *cursor-shape* :block
  "The cursor's shape: :block, :bar (a line before the character) or
:underline, in the window and the terminal alike, unless the modal state has
its own (*TUATARA-INSERT-CURSOR-SHAPE* and the rest). :block unless set.")

(defvar *cursor-colour* nil
  "The cursor's colour, as faces take colours: a name such as :red, a number
0-255, or \"#rrggbb\"; NIL (as standard) for the colour of the text, the
character under a block then in the colour of the ground. The modal states can
have their own (*TUATARA-INSERT-CURSOR-COLOUR* and the rest).")

(defvar *cursor-colour-functions* '()
  "Functions of the editor that may colour the cursor for the moment, the
first to give a colour (as faces take colours) winning over the others: the
current match of a search, the cursor on it, in the match's colour.")

(defun cursor-look (editor)
  "The cursor's shape and colour, and whether text is being typed, as three
values: from EDITOR's cursor function (EDITOR-CURSOR-SHAPE-FUNCTION) where it
says, else *CURSOR-SHAPE* and *CURSOR-COLOUR*; a colour of
*CURSOR-COLOUR-FUNCTIONS* before either."
  (multiple-value-bind (shape colour typing)
      (and (editor-cursor-shape-function editor) (funcall (editor-cursor-shape-function editor) editor))
    (values (or shape *cursor-shape* :block)
            (or (some (lambda (function) (funcall function editor)) *cursor-colour-functions*) colour *cursor-colour*)
            typing)))

(defvar *gutter-providers* '()
  "Gutter providers, each a plist (:name NAME :width WIDTH-FN :cell CELL-FN),
with :prepare PREPARE-FN too when it has one: WIDTH-FN is called with (EDITOR
WINDOW) and returns the columns the provider needs (0 to stay hidden);
PREPARE-FN, once a frame, with (EDITOR WINDOW ROWS), the rows shown, and
returns what its cells are worked out from; CELL-FN with (EDITOR WINDOW ROW
LINE), and that as a fifth argument when it has a PREPARE-FN, where LINE is
the 0-based line number of ROW's start, or NIL for a continuation row, and
returns two values, the text for that row (padded to the width) and its face.")

(defun window-margin-for (editor window)
  "The margin WINDOW needs: the widths of the gutter providers plus one
separating column when any is visible; none in the hex view, whose offsets
stand where line numbers would."
  (if (tuatara.display:hex-view-p (tuatara.display:window-buffer window))
      0
      (let ((total (loop for p in *gutter-providers* sum (funcall (getf p :width) editor window))))
        (if (plusp total) (1+ total) 0))))

(defparameter *gutter-count-reach* 65536
  "The most octets between one row's start and the next that the gutter counts
the next row's line number on across, rather than looking it up afresh: a
fold hiding more is looked past.")

(defun gutter-cells (editor window rows)
  "For each row of ROWS, a list of (STRING . FACE) gutter pieces, in provider order."
  (when *gutter-providers*
    (let* ((buffer (tuatara.display:window-buffer window))
           (widths (mapcar (lambda (p) (funcall (getf p :width) editor window)) *gutter-providers*))
           (prepared (loop for p in *gutter-providers*
                           for width in widths
                           collect (and (plusp width) (getf p :prepare) (funcall (getf p :prepare) editor window rows))))
           ;; not waited for while the lines are counted: known where the count has passed
           (line (and (plusp (length rows))
                      (tuatara.buffer:buffer-known-line-number buffer (tuatara.display:row-start (aref rows 0))))))
      (loop for row across rows
            for index from 0
            collect (let ((row-line (if (tuatara.display:row-continuation-p row) nil line)))
                      (prog1 (loop for p in *gutter-providers*
                                   for width in widths
                                   for ready in prepared
                                   when (plusp width)
                                     collect (multiple-value-bind (text face)
                                                 (if (getf p :prepare)
                                                     (funcall (getf p :cell) editor window row row-line ready)
                                                     (funcall (getf p :cell) editor window row row-line))
                                               (cons (let ((s (or text "")))
                                                       (if (< (length s) width)
                                                           (concatenate 'string (make-string (- width (length s)) :initial-element #\Space) s)
                                                           (subseq s 0 width)))
                                                     face)))
                        ;; the next line begins where the next row does not go on from this one:
                        ;; its own number where it is known, as lines hidden by a fold are skipped,
                        ;; counted on from this row when it is near
                        (when (and line (< (1+ index) (length rows))
                                   (not (tuatara.display:row-continuation-p (aref rows (1+ index)))))
                          (let ((from (tuatara.display:row-start row))
                                (to (tuatara.display:row-start (aref rows (1+ index)))))
                            (setf line (if (and (tuatara.buffer:buffer-indexed-p buffer)
                                                (<= 0 (- to from) *gutter-count-reach*))
                                           (+ line (tuatara.text:rope-count-newlines (tuatara.buffer:buffer-text buffer) from to))
                                           (or (tuatara.buffer:buffer-known-line-number buffer to)
                                               (1+ line))))))))))))

(defvar *syntax-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning lists of (START END FACE) byte
ranges for the text in ROWS: syntax colours from the major mode. Painted first,
so everything else shows over them.")

(defvar *job-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning lists of (START END FACE) byte
ranges for the text in ROWS that work in the background is changing (a
replacement, a sort): painted over the syntax colours and under everything
else, so that search matches, selections and the region show over them.")

(defvar *line-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning lists of (START END FACE) byte
ranges for the text in ROWS painted under all others, the syntax colours
included: the line point is on (hl-line-mode).")

(defvar *text-highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS SYNTAX) returning lists of (START END
FACE) byte ranges for the text in ROWS, painted over the syntax colours,
SYNTAX, the ranges those are painted in, in the same form: what tells
apart the kinds of text, a comment from code, is taken from them (the words
spelt wrong).")

(defvar *highlight-functions* '()
  "Functions of (EDITOR WINDOW ROWS) returning lists of (START END FACE) byte
ranges for the text in ROWS, painted over the syntax colours: search matches
and the like. The region is painted last.")

(defun rectangle-live-p (buffer)
  "True while BUFFER's selection is a rectangle: the block of columns between
point and the mark on each line from one to the other."
  (and (tuatara.buffer:buffer-local buffer :rectangle) (tuatara.buffer:buffer-mark-active-p buffer)))

(defun end-rectangle (buffer)
  "Make BUFFER's selection, if it has one, plain again."
  (setf (tuatara.buffer:buffer-local buffer :rectangle) nil
        (tuatara.buffer:buffer-local buffer :rectangle-point) nil))

(defun rectangle-point-column (buffer)
  "The column on the screen of the corner of BUFFER's rectangle at point:
point's own, or one further right when a motion took the corner past the end
of a short line. With no rectangle, point's column on the screen."
  (let* ((p (tuatara.buffer:buffer-point buffer))
         (column (tuatara.display:buffer-screen-column buffer p))
         (kept (tuatara.buffer:buffer-local buffer :rectangle-point)))
    (if (and kept (rectangle-live-p buffer) (= (car kept) p) (> (cdr kept) column))
        (cdr kept)
        column)))

(defun keep-rectangle-column (buffer column)
  "While BUFFER's selection is a rectangle, stand its corner at point in
COLUMN, past the end of point's line when that is shorter."
  (when (rectangle-live-p buffer)
    (setf (tuatara.buffer:buffer-local buffer :rectangle-point)
          (cons (tuatara.buffer:buffer-point buffer) column))))

(defun rectangle-rows (buffer point mark &key (from 0) (to most-positive-fixnum)
                                             (point-column (tuatara.display:buffer-screen-column buffer point)))
  "The rows of the rectangle with corners POINT and MARK in BUFFER, top
first, each (START END START-COLUMN END-COLUMN): the byte span of its columns
on the screen on that line, cut short where the line is, a tab across an edge
in it, and the columns that span starts and ends at (outside the rectangle's
where a tab crosses its edge, inside where a wide character does). Only the rows
of lines between FROM and TO. POINT-COLUMN is the column of the corner at
POINT, when that is past the end of its line. The text is read once
\(TUATARA.TEXT:MAP-LINE-SLICES)."
  (let ((low (min point mark)) (high (max point mark)))
    (when (and (<= from high) (>= to low))
      (let* ((c1 point-column)
             (c2 (tuatara.display:buffer-screen-column buffer mark))
             (left (min c1 c2)) (right (max c1 c2))
             (top (tuatara.buffer:buffer-line-start buffer (max low from)))
             (bottom (tuatara.buffer:buffer-line-start buffer (min high to)))
             (rows '()))
        (apply #'tuatara.text:map-line-slices (lambda (line start end start-column end-column line-end)
                                                (declare (ignore line line-end))
                                                (push (list start end start-column end-column) rows))
               (tuatara.buffer:buffer-text buffer) top (1+ bottom) left right
               (tuatara.display:buffer-slice-keys buffer))
        (nreverse rows)))))

(defun rectangle-columns (buffer)
  "Two values: the left and right columns on the screen of BUFFER's rectangle."
  (let ((a (rectangle-point-column buffer))
        (b (tuatara.display:buffer-screen-column buffer (tuatara.buffer:buffer-mark buffer))))
    (values (min a b) (max a b))))

(defun region-rectangle-rows (buffer &key (from 0) (to most-positive-fixnum))
  "The rows of BUFFER's rectangle, as RECTANGLE-ROWS gives them."
  (rectangle-rows buffer (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-mark buffer)
                  :from from :to to :point-column (rectangle-point-column buffer)))

(defvar *failed-highlights* (make-hash-table :test 'eq :synchronized t)
  "The highlight functions that have failed, each said once.")

(defun highlights-of (function &rest arguments)
  "What the highlight FUNCTION gives for ARGUMENTS, a copy; NIL when it fails,
which is said once (in the echo area and the debug log), so that one way of
marking text that goes wrong leaves the rest of the screen drawn."
  (handler-case (copy-list (apply function arguments))
    (error (e)
      (unless (gethash function *failed-highlights*)
        (setf (gethash function *failed-highlights*) t)
        (tuatara.runtime:debug-report :redisplay "~(~a~) failed and is left out: ~a" function e)
        (message "~(~a~) failed and is left out of the screen: ~a" function e))
      nil)))

(defun window-syntax (editor window rows)
  "The ranges the text in ROWS of WINDOW is coloured in by its syntax,
(START END FACE): *SYNTAX-HIGHLIGHT-FUNCTIONS*, one that fails left out
\(HIGHLIGHTS-OF)."
  (loop for f in *syntax-highlight-functions*
        nconc (highlights-of f editor window rows)))

(defun window-highlights-for (editor window rows)
  "The highlight ranges for the text in ROWS of WINDOW, lowest priority first:
*LINE-HIGHLIGHT-FUNCTIONS*, syntax, *TEXT-HIGHLIGHT-FUNCTIONS*, then
*JOB-HIGHLIGHT-FUNCTIONS*, then *HIGHLIGHT-FUNCTIONS*, then the active region,
which only the selected window shows."
  (let* ((buffer (tuatara.display:window-buffer window))
         (ranges (list nil))
         (tail ranges))
    (flet ((add (list)
             (setf (cdr tail) list
                   tail (last tail))))
      ;; a mode's colours, and the line point is on, are for text, not for the hex view's digits
      (unless (tuatara.display:hex-view-p buffer)
        (dolist (f *line-highlight-functions*)
          (add (highlights-of f editor window rows)))
        (let ((syntax (window-syntax editor window rows)))
          (add (copy-list syntax))
          (dolist (f *text-highlight-functions*)
            (add (highlights-of f editor window rows syntax)))))
      ;; the hex view's octet at its cursor, in its hex digits and its character both
      (when (tuatara.display:hex-view-p buffer)
        (let ((p (tuatara.display:hex-point buffer)))
          (when (< p (tuatara.buffer:buffer-size buffer))
            (add (list (list p (1+ p) :hex-mirror))))))
      (dolist (f *job-highlight-functions*)
        (add (highlights-of f editor window rows)))
      (dolist (f *highlight-functions*)
        (add (highlights-of f editor window rows)))
      (when (and (eq window (editor-window editor)) (tuatara.buffer:buffer-mark-active-p buffer))
        (if (and (tuatara.buffer:buffer-local buffer :rectangle) (plusp (length rows)))
            ;; a rectangle: its columns on each line shown, and only those
            (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
                  ;; the line a range ends at the start of is not in it
                  do (loop for (start end) in (region-rectangle-rows buffer :from from :to (max from (1- to)))
                           when (< start end)
                             do (add (list (list start end :region)))))
            (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
              (when (and start (< start end))
                (add (list (list start end :region)))))))
      (rest ranges))))

(defvar *mode-line-functions* '()
  "Functions of (EDITOR BUFFER) returning what to add to the mode line: a string,
a list (STRING FACE) for text shown in a face of its own, or NIL.")

(defvar *mode-line-lead-functions* '()
  "Functions of (EDITOR BUFFER) returning what goes first in the mode line,
before [-], [*] or [%], as *MODE-LINE-FUNCTIONS* return their parts, or a list
of such parts written together with no space between them: signs that keep one
place whatever else the mode line shows (the signs of directories kept read).")

(defvar *mode-line-name-functions* '()
  "Functions of (EDITOR BUFFER) returning what goes just before the buffer's
name in the mode line, after [-], [*] or [%], as *MODE-LINE-FUNCTIONS* return
their parts: what is said of the buffer itself (Safe, for a file opened
safely).")

(defun modified-indicator (buffer)
  "BUFFER's state as the mode line shows it: [%] when it is read-only, else
[*] when it has changes not saved, else [-]."
  (cond ((tuatara.buffer:buffer-local buffer :read-only) "[%]")
        ((tuatara.buffer:buffer-modified-p buffer) "[*]")
        (t "[-]")))

(defun abbreviated-path (path)
  "PATH, a file's or a directory's (a pathname or a native name), as a
string with the home directory written ~, as messages and the mode line show
it, whichever name of the home directory it is by
\(TUATARA.IO:HOME-DIRECTORY-NAMES): ~/notes.org, ~/ for the home itself."
  (let ((name (tuatara.io:native path)))
    (dolist (home (tuatara.io:home-directory-names) name)
      (cond ((string= name home) (return "~/"))
            ((and (> (length name) (length home)) (string= home name :end2 (length home)))
             (return (concatenate 'string "~/" (subseq name (length home)))))))))

(defun buffer-directory (buffer)
  "The native name, ending in /, of the directory BUFFER stands in: the one it
lists, for a list of a directory (\\[rimu]); else that of its file; else the one
*DEFAULT-DIRECTORY* names. Files asked for from BUFFER are looked for there."
  (let ((listed (tuatara.buffer:buffer-local buffer :directory))
        (file (tuatara.buffer:buffer-file buffer)))
    (cond (listed)
          (file (tuatara.io:parent-path file))
          (t (tuatara.io:native (tuatara.io:default-directory))))))

(defvar *shown-default-directory* nil
  "The directory *DEFAULT-DIRECTORY* named when the mode line last looked, as
\(SETTING HOME TIME NAME): the setting and the HOME variable then, the
internal real time, and the name as the mode line shows it.")

(defun shown-default-directory ()
  "The directory *DEFAULT-DIRECTORY* names, as the mode line shows it
\(ABBREVIATED-PATH): looked up again when the setting or the home directory
changes, else at most once a second, so that drawing makes no call to the
file system."
  (let ((kept *shown-default-directory*)
        (home (sb-posix:getenv "HOME"))
        (now (get-internal-real-time)))
    (if (and kept (eq (first kept) tuatara-user:*default-directory*) (equal (second kept) home)
             (< (- now (third kept)) internal-time-units-per-second))
        (fourth kept)
        (fourth (setf *shown-default-directory*
                      (list tuatara-user:*default-directory* home now
                            (abbreviated-path (tuatara.io:native (tuatara.io:default-directory)))))))))

(defun buffer-directory-name (buffer)
  "The directory BUFFER stands in (BUFFER-DIRECTORY) as the mode line shows
it: the home directory written ~, a / at the end."
  (if (or (tuatara.buffer:buffer-local buffer :directory) (tuatara.buffer:buffer-file buffer))
      (abbreviated-path (buffer-directory buffer))
      (shown-default-directory)))

(defvar *mode-line-name-max* nil
  "The most columns a buffer's name takes in the mode line, or NIL for as many
as it has: a longer name keeps its start and its end, … standing for its
middle. Whatever this says, a name is cut so that the mode line shows whole in
its window, all but the directory at its end, down to 12 columns.")

(defun mode-line-name (name room)
  "NAME as the mode line shows it with ROOM columns for it: whole when it fits
and is no longer than *MODE-LINE-NAME-MAX*; else cut in its middle to the
smaller of the two, … standing for what is cut, but to no fewer than 12
columns."
  (let ((limit (max 12 (min room (or *mode-line-name-max* room)))))
    (if (<= (panel-columns name) limit)
        name
        (values (tuatara.display:fit-columns name '() limit :middle t :width #'panel-char-columns)))))

(defun default-mode-line (editor &optional (window (editor-window editor)))
  "The mode line of WINDOW, the selected window by default: [-], [*] or [%] as
the buffer is saved, changed or read-only (MODIFIED-INDICATOR), the buffer's
name (cut in its middle when long, MODE-LINE-NAME), the line and column of the
window's point, digits grouped with commas (L100,000,001) so large numbers read
at a glance, the major mode's name when the buffer has one (the :mode-name
local), what *MODE-LINE-FUNCTIONS* add, and last the buffer's directory in the
:mode-line-directory face; what *MODE-LINE-LEAD-FUNCTIONS* give goes first,
before [-], and what *MODE-LINE-NAME-FUNCTIONS* give just before the name. Returns the text and, as a second value, the spans of it in faces
of their own, each (START END FACE)."
  (let* ((buffer (tuatara.display:window-buffer window))
         (point (tuatara.display:window-point window))
         (spans '())
         (before (with-output-to-string (out)
                   (flet ((parts (functions)
                            (loop for f in functions
                                  for lead = (funcall f editor buffer)
                                  ;; one part, or several written together
                                  for pieces = (cond ((stringp lead) (list (list lead nil)))
                                                     ((and (consp lead) (consp (first lead))) lead)
                                                     ((consp lead) (list lead)))
                                  when pieces
                                    do (loop for (part face) in pieces
                                             do (when face
                                                  (push (list (file-position out) (+ (file-position out) (length part)) face) spans))
                                                (write-string part out))
                                       (write-char #\Space out))))
                     (write-char #\Space out)
                     (parts *mode-line-lead-functions*)
                     (format out "~a " (modified-indicator buffer))
                     (parts *mode-line-name-functions*))))
         (after (format nil "  L~:[…~;~:*~:d~] C~:[…~;~:*~:d~]~@[  ~a~]"
                        ;; not waited for: while a large file's lines are counted, the line
                        ;; where the count has passed it, else L…
                        (let ((line (tuatara.buffer:buffer-known-line-number buffer point)))
                          (and line (1+ line)))
                        ;; on a line too long to read back over while its lines are counted, C…
                        (tuatara.buffer:buffer-column buffer point :most tuatara.display:*screen-column-reach*)
                        (tuatara.buffer:buffer-local buffer :mode-name)))
         (extras (loop for f in *mode-line-functions*
                       for extra = (funcall f editor buffer)
                       when (stringp extra) collect (list extra nil)
                       when (consp extra) collect (list (first extra) (second extra))))
         (text (concatenate 'string before
                            (mode-line-name (escape-controls (tuatara.buffer:buffer-name buffer))
                                            (- (tuatara.display:window-width window) 1
                                               (panel-columns before) (panel-columns after)
                                               (loop for (part) in extras sum (+ 2 (panel-columns part)))))
                            after)))
    (flet ((add (part face)
             (setf text (concatenate 'string text "  "))
             (when face (push (list (length text) (+ (length text) (length part)) face) spans))
             (setf text (concatenate 'string text part))))
      (loop for (part face) in extras do (add part face))
      (add (buffer-directory-name buffer) :mode-line-directory))
    (values (concatenate 'string text " ") (nreverse spans))))

(defparameter *window-divider* "│"
  "The character drawn down the line between windows side by side, in the
:vertical-border face.")

(defun fit-to-columns (string columns)
  "STRING cut or padded with spaces to fill COLUMNS columns of the screen, a
wide character counting two. Two values: that string, and how many characters
of STRING it keeps. Each character is looked at once; one of printable ASCII
alone takes a column each."
  (declare (type fixnum columns))
  (let ((n (length string)))
    (multiple-value-bind (kept used)
        (if (every (lambda (c) (char<= #\Space c #\~)) string)
            (let ((k (min n columns))) (values k k))
            (tuatara.display:chars-in-columns string columns))
      (declare (type fixnum kept used))
      (let ((cut (if (= kept n) string (subseq string 0 kept))))
        (values (if (< used columns)
                    (concatenate 'string cut (make-string (- columns used) :initial-element #\Space))
                    cut)
                kept)))))

(defun row-parts (pieces)
  "The columns where a part of a screen row made of PIECES begins after the
first (see PUT-SCREEN-ROW): each piece but one from column 0, and a window's
text after its gutter, in order."
  (sort (remove-duplicates
         (loop for (left nil nil nil nil nil text-from) in pieces
               when (plusp left) collect left
               when (and text-from (plusp text-from)) collect (+ left text-from)))
        #'<))

(defun put-screen-row (frontend row pieces width &optional boxes)
  "Paint screen ROW, WIDTH columns, from PIECES, each (LEFT COLUMNS STRING
SPANS FACE DRAW TEXT-FROM): STRING in the COLUMNS columns from LEFT, SPANS over its
characters, FACE under the whole piece (a mode line) or NIL, DRAW how it is
drawn otherwise (PIECE-SCALES), TEXT-FROM the column of the piece where a
window's text begins after its gutter, with the parts
of BOXES (POPUP-BOXES) on the row laid over them. The frontend is told where
the row's parts begin (ROW-PARTS), so that it may keep each painted apart. A raw byte's character is
written \\xNN (ESCAPE-CONTROLS). On a row with parts drawn large or moved
down, the boxes' parts are given apart, to be drawn over all (OVER), not laid
into the text."
  (multiple-value-bind (string spans face scales) (screen-row-text pieces width)
    (multiple-value-bind (string spans) (escape-controls string spans :raw-bytes)
      (if scales
          (tuatara.frontend:frontend-put-row frontend row string :face face :spans spans :scales scales
                                                                 :over (boxes-on-row row boxes))
          (multiple-value-bind (string spans face) (lay-boxes-over string spans face row boxes width)
            (tuatara.frontend:frontend-put-row frontend row string :face face :spans spans
                                                                   :parts (row-parts pieces)))))))

(defun screen-row-text (pieces width)
  "The text of a screen row WIDTH columns wide made of PIECES (see
PUT-SCREEN-ROW), as four values: the string, its spans, the face under the
whole row and the parts drawn otherwise (PIECE-SCALES). A piece alone across
the row is taken as it is, FACE the row's."
  (if (and (null (rest pieces)) (zerop (first (first pieces))) (= width (second (first pieces))))
      (destructuring-bind (left columns string spans face &optional draw text-from) (first pieces)
        (declare (ignore text-from))
        (values string spans face (piece-scales left columns draw)))
      (let ((text '()) (spans '()) (at 0) (column 0) (scales '()))
        (dolist (piece (sort (copy-list pieces) #'< :key #'first))
          (destructuring-bind (left columns string piece-spans face &optional draw text-from) piece
            (declare (ignore text-from))
            (setf scales (append scales (piece-scales left columns draw)))
            (when (> left column)
              (push (make-string (- left column) :initial-element #\Space) text)
              (incf at (- left column)))
            (multiple-value-bind (fitted kept) (fit-to-columns string columns)
              (let ((inner (loop for (start end span-face) in piece-spans
                                 when (and span-face (< start kept))
                                   collect (list start (min end kept) span-face))))
                (dolist (span (if face (flatten-spans (cons (list 0 (length fitted) face) inner) (length fitted)) inner))
                  (push (list (+ at (first span)) (+ at (second span)) (third span)) spans)))
              (push fitted text)
              (incf at (length fitted))
              (setf column (+ left columns)))))
        (values (apply #'concatenate 'string (nreverse text)) (nreverse spans) nil scales))))

(defun piece-scales (left columns draw)
  "The parts of a piece from column LEFT, COLUMNS wide, drawn otherwise than
in its own cells, as the frontend takes them (TUATARA.FRONTEND:FRONTEND-PUT-ROW's
SCALES), from DRAW, (OFFSET SCALE SHIFT): from OFFSET columns in, SCALE
times as large, the whole piece SHIFT rows lower, its gutter before OFFSET as
it is but as tall as the band; SCALE 0 for a piece left to the rows moved over
it. NIL for a piece drawn as it is."
  (when draw
    (destructuring-bind (offset scale shift) draw
      (cond ((zerop scale) (list (list left (+ left columns) 0 0 0)))
            ((plusp offset)
             (list (list left (+ left offset) 1 shift scale) (list (+ left offset) (+ left columns) scale shift scale)))
            (t (list (list (+ left offset) (+ left columns) scale shift scale)))))))

(defvar *other-cursor-functions* '()
  "Functions of (EDITOR WINDOW ROWS) giving the byte offsets of cursors to draw
in ROWS of WINDOW besides point, in any order: the cursors of a split, for one.
Each is drawn in :split-cursor, or :split-cursor-insert while the real cursor
is a bar.")

(defvar *point-marked-functions* '()
  "Functions of a buffer, true when it shows where point is in a look of its
own (the calendar its day): no cursor is then drawn over it, in its window
selected or not.")

(defun point-marked-p (buffer)
  "True when BUFFER shows where point is in a look of its own
\(*POINT-MARKED-FUNCTIONS*), so that no cursor is drawn over it."
  (some (lambda (function) (funcall function buffer)) *point-marked-functions*))

(defun row-at (rows position)
  "The index of the row of ROWS showing POSITION, or at whose end it stands;
NIL when POSITION is outside them."
  (let ((lo (tuatara.runtime:bisect (mid 0 (length rows)) (<= (tuatara.display:row-start (aref rows mid)) position))))
    (let ((r (1- lo)))
      (and (>= r 0)
           (let ((row (aref rows r)))
             (and (or (< position (tuatara.display:row-end row))
                      (and (tuatara.display:row-last-p row) (= position (tuatara.display:row-end row))))
                  r))))))

(defun cursor-elsewhere-p (editor window)
  "True when the cursor, where typing goes, is not in WINDOW: WINDOW is not
the selected one, or the minibuffer or a question in the echo area waits
for typing (ECHO-QUESTION-P). WINDOW's point is then drawn in :cursor-inactive."
  (or (not (eq window (editor-window editor)))
      (and (editor-minibuffer-active editor) t)
      (echo-question-p editor)))

(defun other-cursors-in (editor window rows)
  "The cursors to draw in ROWS of WINDOW besides the real one, as two
values: the highlight ranges of those on a character, and for each row with
one after the end of its text, (ROW . FACE). Those of *OTHER-CURSOR-FUNCTIONS*
are in :split-cursor (:split-cursor-insert while text is typed); WINDOW's
point, while the cursor is elsewhere (CURSOR-ELSEWHERE-P), in :cursor-inactive,
but in the hex view, which marks its octet itself, and in a buffer that marks
point in its own way (*POINT-MARKED-FUNCTIONS*)."
  (let* ((buffer (tuatara.display:window-buffer window))
         (point (and (cursor-elsewhere-p editor window)
                     (not (tuatara.display:hex-view-p buffer))
                     (not (point-marked-p buffer))
                     (tuatara.display:window-point window))))
    (when (and (or point *other-cursor-functions*) (plusp (length rows)))
      (let* ((from (tuatara.display:row-start (aref rows 0)))
             (to (tuatara.display:row-end (aref rows (1- (length rows)))))
             (split-face (if (nth-value 2 (cursor-look editor)) :split-cursor-insert :split-cursor))
             (ranges '()) (ends '()))
        (flet ((place (p face)
                 (let ((r (and (<= from p to) (row-at rows p))))
                   (when r
                     (let* ((row (aref rows r)) (shown (tuatara.display:row-shown-end row)))
                       (cond ((< p shown)
                              (push (list p (tuatara.buffer:buffer-forward-char buffer p) face) ranges))
                             ((and (= p shown) (not (tuatara.display:row-truncated-p row))
                                   (or (tuatara.display:row-last-p row) (< shown (tuatara.display:row-end row)))
                                   (not (assoc r ends)))
                              (push (cons r face) ends))))))))
          (dolist (f *other-cursor-functions*)
            (dolist (p (funcall f editor window rows))
              (place p split-face)))
          (when point (place point :cursor-inactive)))
        (values (nreverse ranges) ends)))))

(defvar *ruler-functions* '()
  "Functions of a window giving the columns of its text marked by a line down
the window, over the text's backgrounds but under *FACES-OVER-RULERS*: each a
list of (COLUMN FACE),
COLUMN counted from 0 at the line's start (the fill column's in
:visual-line, point's in :cursor-column), or NIL for none.")

(defparameter *faces-over-rulers* '(:hl-line :soft-wrap-row :split-cursor :split-cursor-insert)
  "The faces a line down the window (*RULER-FUNCTIONS*) is painted under: the
line point is on, and the other cursors. Every other face of a character, a
source block's background or the selection's, is painted under the line.")

(defun ruler-cell (text spans at ruler-face)
  "TEXT and SPANS, (START END FACE) over its characters, as two values, with the
character at screen column AT painted in RULER-FACE over the faces it has but
those of *FACES-OVER-RULERS*; TEXT padded with spaces to reach AT when it is
shorter."
  (let* ((starts (tuatara.display:string-column-starts text))
         (column (aref starts (length text)))
         (index (loop for i from 0 below (length text)
                      when (> (aref starts (1+ i)) at) return i)))
    (unless index
      (setf text (concatenate 'string text (make-string (- (1+ at) column) :initial-element #\Space))
            index (1- (length text))))
    (let ((cell (list index (1+ index) ruler-face)) (out '()))
      (dolist (span spans)
        (destructuring-bind (start end face) span
          (cond ((or (<= end index) (> start index))
                 (when (and cell (> start index)) (push cell out) (setf cell nil))
                 (push span out))
                (t
                 (when (< start index) (push (list start index face) out))
                 (push (list index (1+ index)
                             (let ((faces (if (listp face) face (list face))))
                               (flet ((over (f) (member f *faces-over-rulers*)))
                                 (append (remove-if-not #'over faces) (list ruler-face) (remove-if #'over faces)))))
                       out)
                 (when (< (1+ index) end) (push (list (1+ index) end face) out))
                 (setf cell nil)))))
      (when cell (push cell out))
      (values text (nreverse out)))))

(defun ruler-cells (text spans columns)
  "TEXT and SPANS as RULER-CELL leaves them after painting each of COLUMNS, each (AT FACE)."
  (loop for (at face) in columns
        do (multiple-value-setq (text spans) (ruler-cell text spans at face)))
  (values text spans))

(defvar *frame-images* '()
  "The pictures of the frame being painted, each (IMAGE ROW COLUMN COLUMNS
ROWS OFFSET): where WINDOW-PIECES left a window's text blank for its buffer's
picture, and the buffer's :image-offset (see TUATARA.FRONTEND:FRONTEND-PUT-IMAGE).")

(defun picture-in-place (frontend buffer)
  "The picture BUFFER shows in place of its text on FRONTEND: its :image local
where the frontend shows pictures and its :image-hidden local is false; NIL
when it shows its text."
  (and (tuatara.frontend:frontend-images-p frontend)
       (not (tuatara.buffer:buffer-local buffer :image-hidden))
       (tuatara.buffer:buffer-local buffer :image)))

(defun shown-as-picture-p (frontend buffer)
  "True when BUFFER's windows show a picture in place of its text on FRONTEND
\(PICTURE-IN-PLACE), or it is shown as pictures (:picture-view) and its
picture is shown or on its way: its text is not there to put a cursor in."
  (or (picture-in-place frontend buffer)
      (and (tuatara.frontend:frontend-images-p frontend)
           (not (tuatara.buffer:buffer-local buffer :image-hidden))
           (tuatara.buffer:buffer-local buffer :picture-view))))

(defun window-pieces (editor window selected screen)
  "Lay WINDOW out with its point visible and add what it shows to SCREEN, a
vector of the pieces of each screen row (PUT-SCREEN-ROW): its buffer's header
line when it has one, in :header-line, its text rows with their gutter, the
divider beside a window to its right, and its mode line, in :mode-line when it
is SELECTED, else in :mode-line-inactive. Point is kept visible in the rows the
window has under a panel of one row, so that a taller panel, while it lasts,
only hides rows. A buffer with a picture (its :image local) has it shown in
place of its text where the frontend shows pictures, unless its :image-hidden
local is true, at its own size from its :image-offset when it has one: the
text rows are left blank, and the picture noted in *FRAME-IMAGES*. A buffer
shown as pictures (:picture-view) whose picture is on its way (:picture-pending)
shows blank rows meanwhile, not its text. Pictures a buffer places over its
text (its :pictures local, each (IMAGE ROW COLUMN COLUMNS ROWS) of its text's
cells, a terminal's) are noted too, its text shown as it is."
  (let ((top (tuatara.display:window-text-top window))
        (left (tuatara.display:area-left window))
        (width (tuatara.display:window-width window))
        (height (tuatara.display:window-height window)))
    (when (plusp (tuatara.display:area-height window))
      (setf (tuatara.display:window-margin window) (window-margin-for editor window))
      (let* ((rows (let ((shown (tuatara.display:window-height window)))
                     (setf (tuatara.display:window-height window)
                           (or (tuatara.display:window-full-height window) shown))
                     (unwind-protect (tuatara.display:window-ensure-point-visible window)
                       (setf (tuatara.display:window-height window) shown))))
             (cursor-ends (multiple-value-bind (ranges ends) (other-cursors-in editor window rows)
                            ;; other cursors last, over everything else
                            (tuatara.display:apply-highlights rows (append (window-highlights-for editor window rows) ranges))
                            ends))
             (margin (tuatara.display:window-margin window))
             (cells (gutter-cells editor window rows))
             ;; the screen columns marked down the window
             (rulers (let ((shift (if (tuatara.display:window-wraps-p window) 0 (tuatara.display:window-hscroll window))))
                       (loop for (column face) in (loop for f in *ruler-functions* append (funcall f window))
                             for at = (+ margin (- column shift))
                             when (and (<= margin at) (< at width)) collect (list at face)))))
        (when (plusp (tuatara.display:window-header-rows window))
          (let ((header-top (tuatara.display:area-top window)))
            (multiple-value-bind (text spans)
                (ignore-errors (funcall (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) :header-line) window))
              (push (list left width (or text "") (remove nil spans :key #'third) :header-line) (aref screen header-top)))
            (when (tuatara.display:window-divider window)
              (push (list (+ left width) 1 *window-divider* '((0 1 :vertical-border)) nil) (aref screen header-top)))))
        (let ((image (picture-in-place (editor-frontend editor) (tuatara.display:window-buffer window))))
          (when image
            (push (list image top left width height
                        (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) :image-offset))
                  *frame-images*)
            (setf rows #()))
          (when (tuatara.frontend:frontend-images-p (editor-frontend editor))
            (loop with text-width = (tuatara.display:window-text-width window)
                  for (picture row column columns rows) in (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) :pictures)
                  for shown-rows = (min rows (- height row))
                  for shown-columns = (min columns (- text-width column))
                  when (and (>= row 0) (>= column 0) (plusp shown-rows) (plusp shown-columns))
                    do (push (list picture (+ top row) (+ left (tuatara.display:window-margin window) column) shown-columns shown-rows nil)
                             *frame-images*)))
          ;; a buffer shown as pictures (:picture-view) whose picture is on its way: blank, not its text
          (when (and (null image)
                     (tuatara.frontend:frontend-images-p (editor-frontend editor))
                     (not (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) :image-hidden))
                     (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) :picture-view)
                     (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) :picture-pending))
            (setf rows #())))
        ;; SHIFT: how far the rows drawn large above have moved this one down, in rows; the rows
        ;; past those the window draws (WINDOW-ROWS-DRAWN) left out
        ;; (a picture in place of the text leaves no rows)
        (let ((shift 0) (drawn (min (length rows) (tuatara.display:window-rows-drawn window))))
         (dotimes (r height)
          (push (if (< r drawn)
                    (let* ((row (aref rows r))
                           (pieces (pop cells))
                           (gutter (if (plusp margin)
                                       (concatenate 'string (apply #'concatenate 'string (mapcar #'car pieces)) " ")
                                       ""))
                           ;; the gutter's colours only where it is shown (not in the hex view, whose offsets stand there)
                           (spans (append (and (plusp margin)
                                               (let ((pos 0))
                                                 (loop for (text . face) in pieces
                                                       collect (prog1 (list pos (+ pos (length text)) face) (incf pos (length text))))))
                                          (mapcar (lambda (span) (list (+ (first span) (length gutter)) (+ (second span) (length gutter)) (third span)))
                                                  (tuatara.display:row-spans row))))
                           (text (concatenate 'string gutter (tuatara.display:row-string row)))
                           (cursor-cell (and (< (+ (length gutter) (tuatara.display:row-columns row)) width)
                                             (cdr (assoc r cursor-ends))))
                           (extend (tuatara.display:row-extend row))
                           ;; a face carried past the line's end fills the row to the window's edge
                           (fill (if extend
                                     (max 0 (- width (length gutter) (tuatara.display:row-columns row) (if cursor-cell 1 0)))
                                     0)))
                      ;; another cursor after the end of the line's text
                      (when cursor-cell
                        (setf spans (append spans (list (list (length text) (1+ (length text)) cursor-cell)))
                              text (concatenate 'string text " ")))
                      (when (plusp fill)
                        (let ((last (car (last spans))) (end (+ (length text) fill)))
                          (setf spans (if (and last (= (second last) (length text)) (equal (third last) extend))
                                          (append (butlast spans) (list (list (first last) end extend)))
                                          (append spans (list (list (length text) end extend))))
                                text (concatenate 'string text (make-string fill :initial-element #\Space)))))
                      (multiple-value-bind (text spans) (ruler-cells text (remove nil spans :key #'third) rulers)
                        ;; a row drawn large, from after its gutter, or moved down by those above
                        (let ((scale (tuatara.display:row-scale row)))
                          (prog1 (list left width text spans nil
                                       (and (or (/= scale 1) (plusp shift)) (list (length gutter) scale shift))
                                       (tuatara.display:string-columns gutter))
                            (incf shift (1- scale))))))
                    (multiple-value-bind (text spans) (ruler-cells "" '() rulers)
                      ;; below rows moved down: left to them
                      (list left width text spans nil (and (plusp shift) (list 0 0 0)))))
                (aref screen (+ top r)))
          (when (tuatara.display:window-divider window)
            (push (list (+ left width) 1 *window-divider* '((0 1 :vertical-border)) nil) (aref screen (+ top r))))))
        (multiple-value-bind (line spans)
            (funcall (or (editor-mode-line-function editor) #'default-mode-line) editor window)
          (push (list left (tuatara.display:area-width window) line spans (if selected :mode-line :mode-line-inactive))
                (aref screen (+ top height))))))))

(defun cursor-column-past-line (window column &optional (scale 1))
  "COLUMN, where WINDOW's point is drawn, moved on to where the corner of a
rectangle stands past the end of point's line, each column beyond it SCALE
columns wide on a row drawn large; kept inside the window."
  (let ((buffer (tuatara.display:window-buffer window)))
    (if (rectangle-live-p buffer)
        (let ((beyond (- (rectangle-point-column buffer)
                         (tuatara.display:buffer-screen-column buffer (tuatara.buffer:buffer-point buffer)))))
          (min (+ column (round (* beyond scale)))
               (max column (1- (tuatara.display:window-text-width window)))))
        column)))

(defvar *gui-title* "~a - Tuatara"
  "The title of the window tuatara --gui opens, where the desktop names it: a
format control given the name of the buffer the selected window shows. \"~a\"
shows the name alone.")

(defvar *title-functions* '()
  "Functions of the editor returning text to show before the window's title
(the clock running on an Org task), or NIL.")

(defparameter *escaped-invisibles* '((#x200B . #x200B) (#x2060 . #x2064) (#xFEFF . #xFEFF))
  "Code point ranges of the invisible characters, besides those changing the
direction text is drawn in, that ESCAPE-CONTROLS writes as their code: the
zero-width space, the word joiner and its kin and the byte-order mark, which
take no part in drawing any language's text.")

(defun escape-controls (text &optional spans (which :controls))
  "TEXT with each control character in it written as the editor shows one in
text (^J for a newline, ^I for a tab, \\x85 for code 133), and each invisible
character that changes the direction text is drawn in, or draws nothing at
all (a zero-width space, *ESCAPED-INVISIBLES*), as its code (<202E>, <200B>),
so that it takes one line, reads in the order it is written and hides no
character: how a name from elsewhere is shown in a list, a heading, the
panel under the prompt or the window's title. With WHICH :RAW-BYTES, only
each raw byte's character (U+DC80..U+DCFF, an octet that is not UTF-8) is
written, as \\xNN, in the four columns its width counts: a row of the screen,
laid out already, as it is painted. TEXT itself when it has none. The second
value is SPANS, lists (START END FACE) over TEXT's characters, moved to
match."
  (flet ((shown (c)
           ;; the text C is written as, or NIL to leave it as it is
           (let ((code (char-code c)))
             (if (eq which :raw-bytes)
                 (and (tuatara.text:raw-byte-code-p code) (tuatara.display:raw-byte-display code))
                 (cond ((tuatara.text:control-code-p code) (tuatara.display:control-char-display c))
                       ((and (>= code #x61C)
                             (or (tuatara.display:in-ranges-p code tuatara.display:*direction-controls*)
                                 (tuatara.display:in-ranges-p code *escaped-invisibles*)))
                        (tuatara.display:invisible-code-display code)))))))
    (if (find-if #'shown text)
        (let ((moved (make-array (1+ (length text)))) (at 0))
          (values (with-output-to-string (out)
                    (loop for c across text
                          for i from 0
                          do (setf (aref moved i) at)
                             (let ((shown (shown c)))
                               (if shown
                                   (progn (write-string shown out) (incf at (length shown)))
                                   (progn (write-char c out) (incf at)))))
                    (setf (aref moved (length text)) at))
                  (loop for (start end face) in spans
                        collect (list (aref moved (min start (length text))) (aref moved (min end (length text))) face))))
        (values text spans))))

(defun shown-name (text)
  "TEXT, a name from elsewhere, as a list, a menu or the window's title shows
it: its control and invisible characters written out (ESCAPE-CONTROLS), and
each octet that is not UTF-8 as \\xNN, in the four columns it takes."
  (values (escape-controls (escape-controls text) nil :raw-bytes)))

(defun show-title (editor buffer)
  "Give EDITOR's frontend the title *GUI-TITLE* makes of BUFFER's name, with
what *TITLE-FUNCTIONS* add before it, when that has changed since it was last
given; control and direction characters in it are written out, and octets
that are not UTF-8 as \\xNN, as the mode line writes them (ESCAPE-CONTROLS)."
  (let* ((name (tuatara.buffer:buffer-name buffer))
         (extra (format nil "~{~a ~}" (remove nil (mapcar (lambda (f) (ignore-errors (funcall f editor))) *title-functions*))))
         (shown (editor-title-shown editor)))
    (unless (and shown (eq name (first shown)) (eq *gui-title* (second shown)) (equal extra (third shown)))
      (setf (editor-title-shown editor) (list name *gui-title* extra))
      (tuatara.frontend:frontend-set-title
       (editor-frontend editor)
       (shown-name (concatenate 'string extra (or (ignore-errors (format nil *gui-title* name)) name)))))))

(defun redisplay (editor)
  "Lay out every window with its point visible and paint a frame: each
window's text rows and its mode line, the selected window's in :mode-line and
the others' in :mode-line-inactive, a divider between windows side by side,
the bottom panel (echo area, or the minibuffer with its candidates, or the key
hints), and the cursor. Nothing here waits for a large file's lines to be
counted: what needs them shows without them meanwhile."
  (update-candidates editor)
  (preview-selection editor)
  (popup-follow editor)
  (multiple-value-bind (panel minibuffer-row) (panel-rows editor)
    (let* ((frame (editor-frame editor))
           (frontend (frame-frontend frame))
           (tree-rows (fit-windows editor (length panel)))
           (selected (frame-window frame))
           (screen (make-array tree-rows :initial-element '()))
           (width (frame-width frame))
           (*frame-images* '()))
      (dolist (window (editor-windows editor))
        (window-pieces editor window (eq window selected) screen))
      (let ((boxes (popup-boxes editor tree-rows minibuffer-row)))
        (tuatara.frontend:frontend-begin-frame frontend)
        (dotimes (r tree-rows)
          (put-screen-row frontend r (or (aref screen r) (list (list 0 width "" nil nil))) width boxes))
        (loop for (string face spans) in panel
              for r from tree-rows
              do (multiple-value-bind (string spans) (escape-controls string spans :raw-bytes)
                   (multiple-value-bind (string spans face) (lay-boxes-over string spans face r boxes width)
                     (tuatara.frontend:frontend-put-row frontend r string :face face :spans spans))))
        ;; a picture a completion list lies over is left out while it is there: the list stays readable;
        ;; one in a window with no room for it, too
        (loop for (image row column columns rows offset) in *frame-images*
              unless (or (not (plusp rows)) (not (plusp columns))
                         (some (lambda (box) (box-over-area-p box row column columns rows)) boxes))
                do (tuatara.frontend:frontend-put-image frontend image row column columns rows offset)))
      (cond
        ((editor-minibuffer-active editor)
         ;; the minibuffer's cursor: the look for all, *CURSOR-SHAPE* and *CURSOR-COLOUR*
         (tuatara.frontend:frontend-set-cursor
          frontend (+ tree-rows minibuffer-row)
          (minibuffer-column editor (editor-minibuffer-point editor))
          (or *cursor-shape* :block) *cursor-colour*))
        ((echo-question-p editor)
         ;; a question in the echo area: the cursor after it, as the minibuffer's
         (tuatara.frontend:frontend-set-cursor
          frontend (+ tree-rows minibuffer-row) (echo-cursor-column editor)
          (or *cursor-shape* :block) *cursor-colour*))
        (t
         (multiple-value-bind (row column) (tuatara.display:window-point-position selected)
           (multiple-value-bind (shape colour) (cursor-look editor)
             (if (and row (< row (tuatara.display:window-rows-drawn selected)))
                 (tuatara.frontend:frontend-set-cursor
                  frontend
                  (+ (tuatara.display:window-text-top selected) row)
                  (+ (tuatara.display:area-left selected) (tuatara.display:window-margin selected)
                     (cursor-column-past-line selected column (tuatara.display:row-scale (aref (tuatara.display:window-rows selected) row))))
                  ;; no cursor over a picture shown in place of the text, nor while one is on its
                  ;; way, nor where the buffer marks point itself (the calendar's day)
                  (let ((buffer (tuatara.display:window-buffer selected)))
                    (if (or (shown-as-picture-p frontend buffer) (point-marked-p buffer)) :hidden shape))
                  colour)
                 ;; point is under the panel for now: the cursor waits after the echo row's text
                 (tuatara.frontend:frontend-set-cursor
                  frontend (+ tree-rows minibuffer-row)
                  (min (panel-columns (first (nth minibuffer-row panel))) (1- (editor-width editor))) shape colour))))))
      (tuatara.frontend:frontend-end-frame frontend)
      (show-title editor (tuatara.display:window-buffer selected)))))
