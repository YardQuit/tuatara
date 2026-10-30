;;;; frontends/gtk/paint.lisp --- The window painted: colours, fonts, the rows, pictures and the cursor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.frontend.gtk)

;;; Colours and fonts

(defun colour-rgb (colour)
  "COLOUR, a face's colour (a name such as :red, an index into the 256
colours, or \"#rrggbb\"), as a list of three numbers from 0 to 1; NIL when there
is none."
  (mapcar (lambda (c) (/ c 255d0)) (tuatara.display:colour-components colour)))

(defun blend (a b amount)
  "The colour AMOUNT of the way from A to B."
  (mapcar (lambda (x y) (+ x (* amount (- y x)))) a b))

(defun window-paper (part)
  "The colour of PART, :fg or :bg, where no face gives one: the desktop's with
*WINDOW-COLOURS-FROM-DESKTOP* when GTK's theme gives them, else black text on
a white ground; three numbers from 0 to 1."
  (let ((desktop (and *window-colours-from-desktop* *desktop-colours*)))
    (ecase part
      (:fg (if desktop (first desktop) '(0d0 0d0 0d0)))
      (:bg (if desktop (second desktop) '(1d0 1d0 1d0))))))

(defvar *face-colours* (tuatara.display:make-face-cache)
  "What FACE-COLOURS gives for each face spec, as a list, kept until a face or
the desktop's colours change.")

(defun face-colours (spec)
  "Five values for the face SPEC (a face name, a list of them, or NIL) as the
window paints it: the text's colour and the ground's, each three numbers from
0 to 1, its flags (:bold :italic :underline and the rest), the name of the
font it asks for, or NIL for the window's, and its underline's colour, or NIL
for the text's."
  (let ((table (tuatara.display:face-cache-table *face-colours*
                                                 (and *window-colours-from-desktop* *desktop-colours*))))
    (values-list
     (or (gethash spec table)
         (setf (gethash spec table)
               (multiple-value-bind (fg bg flags) (tuatara.display:resolve-face-spec spec)
                 (let ((fg (or (colour-rgb fg) (window-paper :fg)))
                       (bg (or (colour-rgb bg) (window-paper :bg))))
                   (when (member :reverse flags) (rotatef fg bg))
                   (when (member :dim flags) (setf fg (blend fg bg 0.45d0)))
                   (list fg bg flags (and spec (tuatara.display:resolve-face-font spec))
                         (and (member :underline flags)
                              (colour-rgb (tuatara.display:resolve-face-underline-colour spec)))))))))))

(defun draw-underline (f cr column columns y flags colour)
  "Underline COLUMNS cells from COLUMN on the row whose top is at Y, in
COLOUR (three numbers from 0 to 1, NIL for the colour in use): wavy with
:wave in FLAGS, dotted with :dotted, a box round the cells with :box, else a
straight line."
  (let* ((cw (cell-width f))
         (left (float (* column cw) 1d0))
         (right (float (* (+ column columns) cw) 1d0))
         (line (+ y (cell-baseline f) 1.5d0)))
    (when colour (set-colour cr colour))
    (cairo-set-line-width cr 1d0)
    (cond ((member :wave flags)
           ;; up and down every two pixels, a wave two pixels high
           (cairo-move-to cr left line)
           (loop for x from (+ left 2d0) by 2d0
                 for up = t then (not up)
                 while (<= x right)
                 do (cairo-line-to cr x (if up (- line 1d0) (+ line 1d0))))
           (cairo-stroke cr))
          ((member :box flags)
           (cairo-rectangle cr (+ left 0.5d0) (+ y 0.5d0) (- right left 1d0) (float (1- (cell-height f)) 1d0))
           (cairo-stroke cr))
          ((member :dotted flags)
           (loop for x from left by 3d0
                 while (< x right)
                 do (cairo-rectangle cr x (- line 0.5d0) 1d0 1d0))
           (cairo-fill cr))
          (t (cairo-move-to cr left line)
             (cairo-line-to cr right line)
             (cairo-stroke cr)))))

(defun set-colour (cr colour)
  "Draw on CR in COLOUR, three numbers from 0 to 1."
  (destructuring-bind (r g b) colour
    (cairo-set-source-rgb cr (float r 1d0) (float g 1d0) (float b 1d0))))

(defun fill-ground (cr face x y width height)
  "Fill on CR the box at X, Y, WIDTH by HEIGHT pixels, with the ground colour
of FACE (the editor's own for NIL)."
  (set-colour cr (nth-value 1 (face-colours face)))
  (cairo-rectangle cr (float x 1d0) (float y 1d0) (float width 1d0) (float height 1d0))
  (cairo-fill cr))

(defun font-name ()
  "The font the window is written in: *GUI-FONT*, or the desktop's monospace
font, or Monospace 11."
  (or *gui-font*
      (ignore-errors (g-settings-string "org.gnome.desktop.interface" "monospace-font-name"))
      "Monospace 11"))

(defun make-fonts (name &optional (scale 1))
  "The four font descriptions of the font NAME, its size times SCALE: upright,
bold, italic, bold italic. A NAME without a size is taken at 11 points."
  (let ((fonts (make-array 4)))
    (dotimes (i 4 fonts)
      (let ((description (pango-font-description-from-string name)))
        (when (or (/= scale 1) (zerop (pango-font-description-get-size description)))
          (pango-font-description-set-size
           description (max (* 2 +pango-scale+) (round (* scale (font-description-size description))))))
        (when (logtest i 1) (pango-font-description-set-weight description 700))
        (when (logtest i 2) (pango-font-description-set-style description 2))
        (setf (aref fonts i) description)))))

(defun measure-cells (f cr)
  "Set the cell size of F from its font as CR lays it out: the width and height of M."
  (let ((layout (pango-cairo-create-layout cr)))
    (unwind-protect
         (progn
           (pango-layout-set-font-description layout (aref (gtk-fonts f) 0))
           (pango-layout-set-text layout "M")
           (multiple-value-bind (width height) (pango-layout-get-pixel-size layout)
             (sb-thread:with-mutex ((gtk-lock f))
               (setf (cell-width f) (max 1 width)
                     (cell-height f) (max 1 height)
                     (cell-baseline f) (round (pango-layout-get-baseline layout) +pango-scale+)))))
      (g-object-unref layout))))

;;; Painting

(defun font-index (flags)
  "Which of the four fonts FLAGS ask for."
  (+ (if (member :bold flags) 1 0) (if (member :italic flags) 2 0)))

(defun face-font-description (f font)
  "The description of the font named FONT (a face's own) at the size of F's
font, made once and kept."
  (or (gethash font (gtk-face-fonts f))
      (setf (gethash font (gtk-face-fonts f))
            (let ((description (pango-font-description-from-string font)))
              (pango-font-description-set-size description
                                               (pango-font-description-get-size (aref (gtk-fonts f) 0)))
              description))))

(defun forget-face-fonts (f)
  "Let go of F's descriptions of the fonts faces name, made at a size no longer the window's."
  (loop for description being the hash-values of (gtk-face-fonts f)
        do (pango-font-description-free description))
  (clrhash (gtk-face-fonts f)))

(defparameter *most-kept-layouts* 4096
  "The most layouts of the text drawn kept from one frame to the next, about:
once half of them have been made, those not drawn since the last time are
let go of, and made again when they are drawn again.")

(defun make-layouts ()
  "Room for the layouts painting makes as needed, kept from one frame to the
next: a table of the layouts of the text drawn, each set out once however
often it is drawn (KEPT-LAYOUT), the table of those of the frames before it
\(AGE-LAYOUTS), and a list of (SCALE . LAYOUTS) for the parts drawn SCALE times
as large, whose layouts Pango sets out for that scale (LAYOUTS-AT-SCALE)."
  (vector (make-hash-table :test 'equal) (make-hash-table :test 'equal) '()))

(defun free-layouts (layouts)
  "Let go of the layouts in LAYOUTS (MAKE-LAYOUTS), those of its scales too."
  (loop for i below 2
        do (loop for layout being the hash-values of (aref layouts i) do (g-object-unref layout)))
  (loop for (nil . own) in (aref layouts 2) do (free-layouts own)))

(defun age-layouts (layouts)
  "Before a frame: once LAYOUTS (MAKE-LAYOUTS) has made half of
*MOST-KEPT-LAYOUTS* since it last did this, let go of the layouts not drawn
since then and start again, the ones drawn since kept; for its scales too."
  (when (> (hash-table-count (aref layouts 0)) (floor *most-kept-layouts* 2))
    (loop for layout being the hash-values of (aref layouts 1) do (g-object-unref layout))
    (setf (aref layouts 1) (aref layouts 0)
          (aref layouts 0) (make-hash-table :test 'equal)))
  (loop for (nil . own) in (aref layouts 2) do (age-layouts own)))

(defun frame-layouts (f)
  "The layouts F paints a frame with (MAKE-LAYOUTS): those kept from the frames
before (AGE-LAYOUTS), made when there are none."
  (let ((layouts (or (gtk-layouts f) (setf (gtk-layouts f) (make-layouts)))))
    (age-layouts layouts)
    layouts))

(defun forget-layouts (f)
  "Let go of the layouts F keeps from one frame to the next, set out in fonts
no longer the window's, and of the rows painted with them (FORGET-KEPT-ROWS)."
  (forget-kept-rows f)
  (when (gtk-layouts f)
    (free-layouts (gtk-layouts f))
    (setf (gtk-layouts f) nil)))

(defun layouts-at-scale (layouts scale)
  "The layouts of LAYOUTS (MAKE-LAYOUTS) for text drawn SCALE times as large,
made when first asked, under that scale's transform: LAYOUTS itself for 1."
  (if (= scale 1)
      layouts
      (or (cdr (assoc scale (aref layouts 2)))
          (let ((own (make-layouts)))
            (push (cons scale own) (aref layouts 2))
            own))))

(defun font-layout (f cr font index)
  "A new layout on CR in the font named FONT (a face's own) when given, else
in F's font of INDEX (FONT-INDEX)."
  (let ((layout (pango-cairo-create-layout cr)))
    (pango-layout-set-font-description layout (if font (face-font-description f font) (aref (gtk-fonts f) index)))
    layout))

(defun kept-layout (f cr layouts text font index)
  "The layout of LAYOUTS (MAKE-LAYOUTS) holding TEXT in the font named FONT or
else F's font of INDEX: made and set out the first time it is asked for, the
same one after, from a frame before too (AGE-LAYOUTS), so that text drawn
again, as when a window scrolls, is not set out again. A layout keeps what
it was made for (the screen's scale, the font's settings): the layouts are
let go of when those change (FORGET-LAYOUTS)."
  (let ((key (list text font index)))
    (or (gethash key (aref layouts 0))
        (let ((old (gethash key (aref layouts 1))))
          (when old
            (remhash key (aref layouts 1))
            (setf (gethash key (aref layouts 0)) old)))
        (let ((layout (font-layout f cr font index)))
          (pango-layout-set-text layout text)
          (setf (gethash key (aref layouts 0)) layout)))))

(defun draw-text (f cr layouts text column y flags &optional font shape)
  "Draw TEXT from COLUMN on the row whose top is at Y, in the font FLAGS ask for,
or in FONT (a face's own, by name) when given, its baseline on the cells'.
LAYOUTS (MAKE-LAYOUTS) keeps the layout of each text drawn (KEPT-LAYOUT).
With SHAPE, the glyphs' outlines are filled as one shape rather than drawn
one by one, so that where glyphs overlap (the pieces of a ligature) no edge
is drawn twice."
  (let ((layout (kept-layout f cr layouts text font (font-index flags))))
    (cairo-move-to cr (float (* column (cell-width f)) 1d0)
                   (float (+ y (- (cell-baseline f) (round (pango-layout-get-baseline layout) +pango-scale+))) 1d0))
    (if shape
        (progn (pango-cairo-layout-path cr layout)
               (cairo-fill cr))
        (pango-cairo-show-layout cr layout))))

(defparameter *joining-characters* "-=_~<>|+*#:"
  "Characters a font may join into one stroke when two or more come together:
a line of dashes, an arrow, a double line. A run of signs with two of them
together is drawn as one filled shape (see DRAW-TEXT), so that the pieces meet
without the seams glyphs drawn one by one leave where they overlap.")

(defun joining-runs (text)
  "The runs of signs in TEXT (characters neither letters, digits nor spaces)
with two or more *JOINING-CHARACTERS* together in them, each (START . END), in
order: a line of dashes, an arrow, !== or <!-- whole, as the font joins them."
  (let ((runs '()) (i 0) (n (length text)))
    (flet ((sign-p (c) (and (graphic-char-p c) (not (alphanumericp c)) (char/= c #\Space)))
           (joining-p (c) (find c *joining-characters*)))
      (loop while (< i n)
            do (if (sign-p (char text i))
                   (let ((j (or (position-if-not #'sign-p text :start i) n)))
                     (when (loop for k from i below (1- j)
                                 thereis (and (joining-p (char text k)) (joining-p (char text (1+ k)))))
                       (push (cons i j) runs))
                     (setf i j))
                   (incf i))))
    (nreverse runs)))

(defun draw-ascii-text (f cr layouts text column y flags)
  "Draw TEXT, all ASCII, from COLUMN on the row whose top is at Y, as DRAW-TEXT
does, each run JOINING-RUNS finds in it as one shape."
  (let ((at 0))
    (dolist (run (joining-runs text))
      (when (< at (car run))
        (draw-text f cr layouts (subseq text at (car run)) (+ column at) y flags))
      (draw-text f cr layouts (subseq text (car run) (cdr run)) (+ column (car run)) y flags nil t)
      (setf at (cdr run)))
    (when (< at (length text))
      (draw-text f cr layouts (subseq text at) (+ column at) y flags))))

(defun paint-run (f cr layouts string start end column y spec &key each)
  "Paint the characters of STRING from START below END from COLUMN on the row
whose top is at Y, in the face SPEC; the column after them. Plain ASCII goes
in one piece; any other character on its own, at its column, so that a glyph
from another font or a wide one cannot push the rest out of the grid, a
letter with its accents, an emoji with its skin tone (a grapheme cluster,
TUATARA.TEXT:STRING-CLUSTER-END) together in the cells they take. With EACH,
every character, or cluster, on its own, at its column: drawn larger, a
font's own spacing would not keep to the grid, and a run's letters would
stand otherwise than the same letters split among colours."
  (multiple-value-bind (fg bg flags font underline) (face-colours spec)
    (let* ((columns (tuatara.display:string-columns string :start start :end end))
           (cw (cell-width f)) (ch (cell-height f)))
      (when spec
        (set-colour cr bg)
        (cairo-rectangle cr (float (* column cw) 1d0) (float y 1d0) (float (* columns cw) 1d0) (float ch 1d0))
        (cairo-fill cr))
      (set-colour cr fg)
      (flet ((alone-p (i)
               ;; an ASCII character no mark joins
               (and (< (char-code (char string i)) 128)
                    (= (tuatara.text:string-cluster-end string i end) (1+ i)))))
        (cond
          ((and font (not each))
           ;; a face's own font: the run in one piece, as that font spaces it
           (draw-text f cr layouts (subseq string start end) column y flags font))
          (t
           (let ((at column) (i start))
             (loop while (< i end)
                   do (if (and (not each) (char/= (char string i) #\Space) (alone-p i))
                          ;; a run of ASCII
                          (let ((j i))
                            (loop while (and (< j end) (alone-p j)) do (incf j))
                            (let ((text (string-right-trim " " (subseq string i j))))
                              (when (plusp (length text)) (draw-ascii-text f cr layouts text at y flags)))
                            (incf at (- j i))
                            (setf i j))
                          (let ((next (tuatara.text:string-cluster-end string i end)))
                            (unless (char= (char string i) #\Space)
                              (draw-text f cr layouts (subseq string i next) at y flags font))
                            (incf at (tuatara.display:string-columns string :start i :end next))
                            (setf i next))))))))
      (when (member :underline flags)
        (draw-underline f cr column columns y flags underline))
      (+ column columns))))

(defun char-index-at (string column)
  "The index of the character of STRING at COLUMN, the first of its grapheme
cluster (TUATARA.TEXT:STRING-CLUSTER-END), or its length when the characters
end before it. An ASCII character no mark follows takes a column of its own
and is passed at once."
  (let ((at 0) (i 0) (n (length string)))
    (loop while (< i n)
          do (when (>= at column) (return-from char-index-at i))
             (if (and (<= 32 (char-code (char string i)) 126)
                      (or (= (1+ i) n) (< (char-code (char string (1+ i))) #x300)))
                 (setf at (1+ at) i (1+ i))
                 (let ((next (tuatara.text:string-cluster-end string i)))
                   (incf at (tuatara.display:string-columns string :start i :end next))
                   (setf i next))))
    n))

(defun paint-row (f cr layouts row y width &key (pass :grid) (from 0) (to most-positive-fixnum))
  "Paint ROW, (STRING FACE SPANS SCALES OVER), on the row whose top is at Y,
WIDTH pixels wide, in three passes over all rows, as PASS says. :grid, the row
in its own cells but for the parts SCALES names, each (START END SCALE SHIFT
HEIGHT) in columns and rows, only its runs of characters that reach the columns
from FROM below TO; :large, those parts alone: the text from START on
drawn SCALE times as tall and wide, SHIFT rows lower, over the cells from
START below END of the HEIGHT rows it reaches (SCALE 0: nothing); :over, the
parts of OVER, each (LEFT TEXT SPANS), in the row's own cells over everything,
the first on top."
  (destructuring-bind (string face spans &optional scales over parts) row
    (declare (ignore parts))
    (let* ((string (tuatara.display:displayable string))
           (n (length string))
           (faces (make-array n :initial-element nil))
           (cw (cell-width f)) (ch (cell-height f)))
      (loop for (start end span-face) in spans
            do (loop with combined = (tuatara.display:combine-faces span-face face)
                     for i from (max 0 start) below (min n end)
                     do (setf (aref faces i) combined)))
      (labels ((paint-chars (string faces face start below column y &optional each (layouts layouts))
                 (let ((i start))
                   ;; no further than the columns asked for
                   (loop while (and (< i below) (< column to))
                         do (let* ((spec (or (aref faces i) face))
                                   ;; a mark or a joined emoji goes with the letter it is drawn on, whatever its own face
                                   (j (tuatara.text:string-cluster-end string i below)))
                              (loop while (and (< j below) (equal (or (aref faces j) face) spec))
                                    do (setf j (tuatara.text:string-cluster-end string j below)))
                              (setf column (if (or (>= column to)
                                                   (<= (+ column (tuatara.display:string-columns string :start i :end j)) from))
                                               (+ column (tuatara.display:string-columns string :start i :end j))
                                               (paint-run f cr layouts string i j column y spec :each each)))
                              (setf i j))))))
        (ecase pass
          (:grid
           ;; the row's face covers the whole row, past the text too
           (when face (fill-ground cr face 0 y width ch))
           (let ((i 0))
             (loop for (from below) in (append (sort (loop for (start end) in scales
                                                           collect (list (char-index-at string start) (char-index-at string end)))
                                                     #'< :key #'first)
                                               (list (list n n)))
                   do (when (< i from)
                        (paint-chars string faces face i from
                                     (tuatara.display:string-columns string :end i) y))
                      (setf i (max i below)))))
          (:large
           (loop for (start end scale shift height) in scales
                 when (plusp scale)
                   do (let ((top (+ y (* shift ch))) (band (or height scale)))
                        (cairo-save cr)
                        ;; grounds meeting between pixels, with no seam where they meet
                        (cairo-set-antialias cr 1)
                        (cairo-rectangle cr (float (* start cw) 1d0) (float top 1d0)
                                         (float (* (- end start) cw) 1d0) (float (* band ch) 1d0))
                        (cairo-clip cr)
                        (fill-ground cr face (* start cw) top (* (- end start) cw) (* band ch))
                        (cairo-translate cr (float (* start cw) 1d0) (float top 1d0))
                        (cairo-scale cr (float scale 1d0) (float scale 1d0))
                        ;; scaled, each letter at its own cell, wherever the colours change, with
                        ;; the layouts of its scale, which Pango sets out for the scale in force when made
                        (paint-chars string faces face (char-index-at string start) (char-index-at string end) 0 0
                                     (/= scale 1) (layouts-at-scale layouts scale))
                        (cairo-restore cr))))
          (:over
           (dolist (part (reverse over))
             (destructuring-bind (left text text-spans) part
               (let* ((text (tuatara.display:displayable text))
                      (text-faces (make-array (length text) :initial-element nil)))
                 (loop for (start end span-face) in text-spans
                       do (loop for i from (max 0 start) below (min (length text) end)
                                do (setf (aref text-faces i) span-face)))
                 (fill-ground cr nil (* left cw) y (* (tuatara.display:string-columns text) cw) ch)
                 (paint-chars text text-faces nil 0 (length text) left y))))))))))

(defun cursor-character (row column)
  "What ROW, (STRING FACE SPANS), shows at COLUMN, as two values: the
character there with the marks drawn on it (a grapheme cluster,
TUATARA.TEXT:STRING-CLUSTER-END) as a string, and the columns it takes; a
space and one column when nothing is there."
  (let ((string (and row (tuatara.display:displayable (first row)))) (at 0) (i 0))
    (when string
      (loop while (< i (length string))
            do (let ((next (tuatara.text:string-cluster-end string i)))
                 (when (= at column)
                   (return-from cursor-character
                     (values (subseq string i next)
                             (max 1 (tuatara.display:string-columns string :start i :end next)))))
                 (incf at (tuatara.display:string-columns string :start i :end next))
                 (when (> at column) (return))
                 (setf i next))))
    (values " " 1)))

(defun cursor-part (frame row column)
  "The part of FRAME's ROW drawn large or moved down that holds COLUMN, (START
END SCALE SHIFT HEIGHT), or NIL when the cell is drawn where it is."
  (let ((entry (and (< -1 row (length (frame-rows frame))) (aref (frame-rows frame) row))))
    (find-if (lambda (part) (and (plusp (third part)) (<= (first part) column) (< column (second part))))
             (fourth entry))))

(defun part-cell (part column)
  "The cell of PART, (START END SCALE SHIFT HEIGHT), a part of a row drawn
large or moved down, that holds COLUMN, counted from the part's start in its
own large cells."
  (floor (- column (first part)) (third part)))

(defun enter-part (f cr part row)
  "Have CR draw in the cells of PART, (START END SCALE SHIFT HEIGHT), a part of
ROW drawn large or moved down: from its first cell, SHIFT rows lower, SCALE
times as large."
  (destructuring-bind (start end scale shift &optional height) part
    (declare (ignore end height))
    (cairo-translate cr (float (* start (cell-width f)) 1d0) (float (* (+ row shift) (cell-height f)) 1d0))
    (cairo-scale cr (float scale 1d0) (float scale 1d0))))

(defun frame-cursor-pixels (f frame)
  "Where FRAME's cursor is drawn on F's grid, as four values: its x, y, width
and height in pixels, the room round the grid left out; on a part of its row
drawn large or moved down, the cell of that part."
  (let* ((row (frame-cursor-row frame)) (column (frame-cursor-column frame))
         (part (cursor-part frame row column))
         (cw (cell-width f)) (ch (cell-height f)))
    (if part
        (destructuring-bind (start end scale shift &optional height) part
          (declare (ignore end height))
          (values (* (+ start (* (part-cell part column) scale)) cw) (* (+ row shift) ch) (* scale cw) (* scale ch)))
        (values (* column cw) (* row ch) cw ch))))

(defun paint-preedit (f cr layouts frame text)
  "Paint TEXT, what the input method is composing, over the cells at FRAME's
cursor, underlined, on a part of its row drawn large or moved down where that
part's cells are; the column after it, in the window's own columns."
  (let* ((row (frame-cursor-row frame)) (column (frame-cursor-column frame))
         (part (cursor-part frame row column))
         (cw (cell-width f)) (ch (cell-height f))
         (columns (tuatara.display:string-columns text)))
    (cairo-save cr)
    (multiple-value-bind (cell top layouts scale)
        (if part
            (progn (enter-part f cr part row)
                   (values (part-cell part column) 0 (layouts-at-scale layouts (third part)) (third part)))
            (values column (* row ch) layouts 1))
      (let ((x (float (* cell cw) 1d0)) (y (float top 1d0)))
        (fill-ground cr nil x y (* columns cw) ch)
        (paint-run f cr layouts text 0 (length text) cell top nil :each (/= scale 1))
        (set-colour cr (face-colours nil))
        (cairo-set-line-width cr 1d0)
        (let ((line (+ y (cell-baseline f) 1.5d0)))
          (cairo-move-to cr x line)
          (cairo-line-to cr (+ x (* columns cw)) line)
          (cairo-stroke cr)))
      (cairo-restore cr)
      (if part
          (+ (first part) (tuatara.display:large-column (+ cell columns) scale))
          (+ column columns)))))

(defun cursor-colours (colour)
  "The cursor's colours for COLOUR, as faces take colours, as two values, each
three numbers from 0 to 1: the character's under a block and the cursor's own;
that colour (TUATARA.DISPLAY:CURSOR-COLOUR) with the character in the
ground's, or in the :cursor face's text colour when the face gives the
cursor's; the :cursor face's when it gives none, the window's own."
  (let ((rgb (colour-rgb (tuatara.display:cursor-colour colour))))
    (if rgb
        (values (or (and (null colour) (tuatara.display:face-attribute :cursor :bg)
                         (colour-rgb (tuatara.display:face-attribute :cursor :fg)))
                    (nth-value 1 (face-colours :default)))
                rgb)
        (face-colours :cursor))))

(defun paint-cursor (f cr layouts frame focused &key (column (frame-cursor-column frame))
                                                     (shape (frame-cursor-shape frame)))
  "Paint FRAME's cursor, at COLUMN of its row, in its colour: a block with the
character under it, as wide as that character, a bar before the character or
a line under it, as SHAPE says; an outline when the window is not FOCUSED; none for :hidden. On a part
of the row drawn large or moved down, where its characters are and as large."
  (when (eq shape :hidden)
    (return-from paint-cursor nil))
  (let* ((row (frame-cursor-row frame))
         (entry (and (< -1 row (length (frame-rows frame))) (aref (frame-rows frame) row)))
         (part (cursor-part frame row column))
         (cw (cell-width f)) (ch (cell-height f)))
    (cairo-save cr)
    ;; on a part drawn large or moved down: drawn where its own cells are, its column the character's under it
    (multiple-value-bind (cell text-column top)
        (if part
            (let ((inside (part-cell part column)))
              (enter-part f cr part row)
              (values inside (+ (first part) inside) 0))
            (values column column (* row ch)))
      (multiple-value-bind (text columns) (cursor-character entry text-column)
        (let ((x (float (* cell cw) 1d0)) (y (float top 1d0))
              (width (float (* columns cw) 1d0)))
          (multiple-value-bind (fg bg) (cursor-colours (frame-cursor-colour frame))
            (set-colour cr bg)
            (cond ((not focused)
                   (cairo-set-line-width cr 1d0)
                   (cairo-rectangle cr (+ x 0.5d0) (+ y 0.5d0) (- width 1d0) (float (- ch 1) 1d0))
                   (cairo-stroke cr))
                  ((eq shape :bar)
                   (cairo-rectangle cr x y 2d0 (float ch 1d0))
                   (cairo-fill cr))
                  ((eq shape :underline)
                   (cairo-rectangle cr x (float (- (+ y ch) 2) 1d0) width 2d0)
                   (cairo-fill cr))
                  (t
                   (cairo-rectangle cr x y width (float ch 1d0))
                   (cairo-fill cr)
                   (unless (string= text " ")
                     (set-colour cr fg)
                     ;; on a part, the layouts of its scale (see PAINT-ROW)
                     (draw-text f cr (if part (layouts-at-scale layouts (third part)) layouts) text cell top nil))))))))
    (cairo-restore cr)))

(defun image-surface (image)
  "A cairo surface holding IMAGE's pixels, its alpha applied as cairo keeps it:
copied by GDK from IMAGE's texture (IMAGE-TEXTURE), which writes them as
cairo does, 32 bits a pixel, blue first, each colour multiplied by the alpha."
  (let ((surface (cairo-image-surface-create 0 (tuatara.frontend:image-width image) (tuatara.frontend:image-height image)))
        (texture (image-texture image)))
    (cairo-surface-flush surface)
    (gdk-texture-download texture (cairo-image-surface-get-data surface) (cairo-image-surface-get-stride surface))
    (g-object-unref texture)
    (cairo-surface-mark-dirty surface)
    surface))

(defun paint-images (f cr frame)
  "Paint FRAME's pictures on CR, each in its cells, as large as fits, its
proportions kept and never larger than it is, from the top left; one with an
offset at its own size, from that row and column of cells of it on. The
surfaces of pictures no longer shown are let go of."
  (let ((surfaces (gtk-surfaces f)) (shown '()))
    (loop for (image row column columns rows offset) in (frame-images frame)
          ;; no room: nothing painted (a scale of 0 would spoil all painted after on CR)
          when (and (plusp rows) (plusp columns))
          do (let* ((surface (or (gethash image surfaces) (setf (gethash image surfaces) (image-surface image))))
                    ;; the picture's own size is in the display's pixels: as large as that in the window's, no larger
                    (display (gtk-display-scale f))
                    (scale (if offset
                               (/ 1d0 display)
                               (tuatara.frontend:image-fit-scale image (* columns (cell-width f)) (* rows (cell-height f))
                                                                 (/ 1d0 display)))))
               (push image shown)
               (cairo-save cr)
               (cairo-translate cr (float (* column (cell-width f)) 1d0) (float (* row (cell-height f)) 1d0))
               (cairo-rectangle cr 0d0 0d0 (float (* columns (cell-width f)) 1d0) (float (* rows (cell-height f)) 1d0))
               (cairo-clip cr)
               (when offset
                 (cairo-translate cr (float (- (* (second offset) (cell-width f))) 1d0)
                                  (float (- (* (first offset) (cell-height f))) 1d0)))
               (cairo-scale cr (float scale 1d0) (float scale 1d0))
               (cairo-set-source-surface cr surface 0d0 0d0)
               (cairo-paint cr)
               (cairo-restore cr)))
    (loop for image being the hash-keys of surfaces using (hash-value surface)
          unless (member image shown)
            do (cairo-surface-destroy surface)
               (remhash image surfaces))))

(defmacro with-grid ((f cr width height grid-width) &body body)
  "Run BODY with CR moved inside the room round F's grid (GTK-PADDING) of a
window WIDTH by HEIGHT pixels and held to its whole cells, GRID-WIDTH bound to
their width in pixels."
  (let ((top (gensym "TOP")) (right (gensym "RIGHT")) (bottom (gensym "BOTTOM")) (left (gensym "LEFT"))
        (grid-height (gensym "GRID-HEIGHT")))
    `(destructuring-bind (,top ,right ,bottom ,left) (gtk-padding ,f)
       (let ((,grid-width (* (cell-width ,f) (max 1 (floor (- ,width ,left ,right) (cell-width ,f)))))
             (,grid-height (* (cell-height ,f) (max 1 (floor (- ,height ,top ,bottom) (cell-height ,f))))))
         (declare (ignorable ,grid-width))
         (cairo-save ,cr)
         (cairo-translate ,cr (float ,left 1d0) (float ,top 1d0))
         ;; nothing past the grid's whole cells: a row longer than the window stays out of the room round it
         (cairo-rectangle ,cr 0d0 0d0 (float ,grid-width 1d0) (float ,grid-height 1d0))
         (cairo-clip ,cr)
         (unwind-protect (progn ,@body)
           (cairo-restore ,cr))))))

(defstruct (kept-rows (:constructor make-kept-rows (key)) (:copier nil))
  "The rows of the window as last painted, kept from one frame to the next so
that only what changed is painted again (KEEP-ROWS): PIECES, for each row
down the window, its parts as painted, each (FROM BELOW STRIP SHOWS
TEXTURE): a picture in memory, STRIP, of the row's cells from column FROM below
BELOW, without what is drawn over the rows (the cursor, pictures, what the
input method composes), showing SHOWS (PIECE-CONTENT), NIL for a strip yet to
be painted, and TEXTURE, the strip as the graphics card is given it
\(STRIP-TEXTURE), or NIL until it is; KEY what they were painted for
\(ROWS-PAINTED-FOR)."
  key (pieces #()))

(defun forget-kept-rows (f)
  "Let go of the rows F keeps painted (KEEP-ROWS): the next frame is painted whole."
  (let ((kept (gtk-kept-rows f)))
    (when kept
      (loop for pieces across (kept-rows-pieces kept)
            do (loop for piece in pieces
                     do (cairo-surface-destroy (third piece))
                        (when (fifth piece) (g-object-unref (fifth piece)))))
      (setf (gtk-kept-rows f) nil))))

(defun rows-painted-for (f width count)
  "What F's rows are painted for, COUNT rows of a grid WIDTH pixels wide: when
any of it changes (the window's size, the faces or the theme, the desktop's
colours, the cells, the font, the screen's scale), the rows kept painted are
of no use."
  (list width count tuatara.display:*face-generation* (and *window-colours-from-desktop* *desktop-colours*)
        (cell-width f) (cell-height f) (gtk-font-in-use f) (gtk-display-scale f)))

(defun drawn-across-rows-p (row)
  "True when ROW, as a frame holds it, has parts drawn large or over others,
which reach into the rows round it."
  (and row (or (fourth row) (fifth row)) t))

(defparameter *most-row-shift* 16
  "The most rows a window's text is looked for moved by from one frame to the
next (ROW-SHIFT): moved further, as by a page, it is painted whole.")

(defun same-line-p (old new)
  "True when NEW and OLD, rows as frames hold them, show the same line, but
perhaps for its first columns (a line number counted from the cursor's): the
same from their end over three quarters of the longer at least."
  (and old new
       (let* ((a (first old)) (b (first new))
              (longer (max (length a) (length b)))
              (from-end (mismatch a b :from-end t)))
         (or (null from-end)
             (>= (- (length a) from-end) (* 3/4 longer))))))

(defun row-shift (old-rows new-rows)
  "How many rows further down than now the rows of the new frame, NEW-ROWS,
stood in the frame before, OLD-ROWS, as when a window scrolls: the shift, of
*MOST-ROW-SHIFT* at most either way, with which most rows show the same
lines as before (SAME-LINE-P); 0 when none does better than staying."
  (let ((best 0) (most -1) (n (length new-rows)))
    ;; the nearest shifts first: one that shows all the lines but two or fewer ends the search
    (loop for shift in (cons 0 (loop for k from 1 to *most-row-shift* collect k collect (- k)))
          do (let ((count (loop for i from (max 0 (- shift)) below (min n (- (length old-rows) shift))
                                count (same-line-p (aref old-rows (+ i shift)) (aref new-rows i)))))
               (when (> count most) (setf best shift most count))
               (when (>= count (- n (abs shift) 2)) (return))))
    best))

(defun paint-rows-whole (f cr layouts rows width height)
  "Paint on CR, WIDTH by HEIGHT pixels, its ground painted already, inside the
room round the grid (GTK-PADDING), no further than its whole cells, ROWS: each
in its cells, then what is drawn large or moved down, over the rows below it,
and what lies over all."
  (with-grid (f cr width height grid-width)
    (loop for row across rows
          for y from 0 by (cell-height f)
          when row do (paint-row f cr layouts row y grid-width))
    (dolist (pass '(:large :over))
      (loop for row across rows
            for y from 0 by (cell-height f)
            when (and row (if (eq pass :large) (fourth row) (fifth row)))
              do (paint-row f cr layouts row y grid-width :pass pass)))))

(defun changed-columns (old new)
  "The columns where NEW, a row as frames hold it, shows otherwise than OLD, the
row painted there before, as two values, from and below: from its first
character changed to its last, or to its end when what follows moved to
other columns; NIL when the whole row is to be painted again (its face or
the faces of its characters changed, or there was none). A column more each
side, for a letter leaning into the next cell."
  (when (and old new (equal (second old) (second new)) (equal (third old) (third new)))
    (let* ((a (first old)) (b (first new))
           (start (mismatch a b)))
      (if (null start)
          (values 0 0)
          (let* ((end-old (max start (or (mismatch a b :from-end t) 0)))
                 (end-new (max start (or (mismatch b a :from-end t) 0)))
                 (old-columns (tuatara.display:string-columns a :end end-old))
                 (new-columns (tuatara.display:string-columns b :end end-new)))
            (values (max 0 (1- (tuatara.display:string-columns b :end (min start (length b)))))
                    ;; the rest as it was only where it stays in its columns; else to the end of the longer
                    (1+ (if (= old-columns new-columns)
                            new-columns
                            (max (tuatara.display:string-columns a) (tuatara.display:string-columns b))))))))))

(defun row-strip (f width)
  "A picture in memory of a row's cells, WIDTH pixels wide and a cell tall,
in the screen's own pixels."
  (let* ((scale (gtk-display-scale f))
         (strip (cairo-image-surface-create 1 (max 1 (* scale width)) (* scale (cell-height f)))))
    (cairo-surface-set-device-scale strip (float scale 1d0) (float scale 1d0))
    strip))

(defun row-bounds (row columns)
  "Where the parts of ROW, as frames hold rows, begin and end on a grid COLUMNS
wide: a list of (FROM . BELOW), its parts (the row's PARTS) in order."
  (let ((starts (cons 0 (remove-if-not (lambda (c) (< 0 c columns)) (and row (sixth row))))))
    (loop for (from below) on starts
          collect (cons from (or below columns)))))

(defun piece-content (row from below)
  "What ROW, as frames hold rows, shows in its columns from FROM below BELOW:
its characters there, the row's face and the faces of those characters,
counted from FROM, to be compared with what a strip shows."
  (when row
    (destructuring-bind (string face spans &rest more) row
      (declare (ignore more))
      (let ((start (char-index-at string from)) (end (char-index-at string below)))
        (list (subseq string start end) face
              (loop for (s e span-face) in spans
                    when (and (< s end) (> e start))
                      collect (list (- (max s start) start) (- (min e end) start) span-face)))))))

(defun paint-piece (f strip layouts shows row from below)
  "Paint on STRIP, the cells of a row from column FROM below BELOW, which shows
SHOWS (PIECE-CONTENT, or NIL for nothing yet), what ROW shows there: of a part
whose characters alone changed, the columns they are in (CHANGED-COLUMNS),
else all of it, its cells in the ground's colour first."
  (let ((cw (cell-width f)))
    (multiple-value-bind (left right) (changed-columns shows (piece-content row from below))
      (let ((left (+ from (or left 0)))
            (right (if left (min below (+ from right)) below))
            (cr (cairo-create strip)))
        (unwind-protect
             (when (< left right)
               (cairo-translate cr (float (- (* from cw)) 1d0) 0d0)
               (cairo-rectangle cr (float (* left cw) 1d0) 0d0 (float (* (- right left) cw) 1d0) (float (cell-height f) 1d0))
               (cairo-clip cr)
               (set-colour cr (nth-value 1 (face-colours nil)))
               (cairo-paint cr)
               (when row (paint-row f cr layouts row 0 (* below cw) :from left :to right)))
          (cairo-destroy cr))))))

(defun keep-rows (f width rows layouts)
  "The parts of ROWS, the rows of a frame, painted for F's window, whose grid
is WIDTH pixels wide (KEPT-ROWS), as a vector with each row's list of (FROM
BELOW STRIP SHOWS TEXTURE): the last part of each row, a window's text, taken from
the row that showed its line in the frame before, as the window scrolled
\(ROW-SHIFT), the others from the same row, as a gutter's numbers stay there;
only what changed in each painted again; all painted the first time, and when
what they are painted for changes (ROWS-PAINTED-FOR)."
  (let* ((columns (max 1 (floor width (cell-width f))))
         (key (rows-painted-for f width (length rows)))
         (kept (gtk-kept-rows f)))
    (unless (and kept (equal key (kept-rows-key kept)))
      (forget-kept-rows f)
      (setf kept (setf (gtk-kept-rows f) (make-kept-rows key))
            (kept-rows-pieces kept) (make-array (length rows) :initial-element '())))
    (let* ((old (kept-rows-pieces kept))
           (n (length rows))
           (cw (cell-width f))
           (bounds (map 'vector (lambda (row) (row-bounds row columns)) rows))
           ;; what each row's last part shows now and showed before, for the shift
           (lasts (map 'vector (lambda (row b) (let ((last (car (last b)))) (and row (piece-content row (car last) (cdr last)))))
                       rows bounds))
           (old-lasts (map 'vector (lambda (pieces) (fourth (car (last pieces)))) old))
           (shift (row-shift old-lasts lasts))
           (free '())
           (new (make-array n :initial-element '())))
      ;; first every piece is matched to the strip it can be painted over, the strips taken out of the old rows
      (let ((claimed (make-hash-table :test 'eq)))
        (dotimes (i n)
          (let ((b (aref bounds i)))
            (setf (aref new i)
                  (loop for (from . below) in b
                        for last = (eq (car (last b)) (assoc from b))
                        collect (let* ((j (if last (+ i shift) i))
                                       (pieces (and (< -1 j n) (aref old j)))
                                       (piece (find-if (lambda (p) (and (= (first p) from) (= (second p) below)
                                                                        (not (gethash (third p) claimed))))
                                                       pieces)))
                                  (if piece
                                      (progn (setf (gethash (third piece) claimed) t)
                                             (list from below (third piece) (fourth piece) (fifth piece)))
                                      (list from below nil nil nil)))))))
        ;; strips no row took: kept for the pieces that need one of their width
        (loop for pieces across old
              do (loop for piece in pieces
                       unless (gethash (third piece) claimed)
                         do (push piece free)))
        (dotimes (i n)
          (let ((row (aref rows i)))
            (dolist (piece (aref new i))
              (destructuring-bind (from below strip shows &optional texture) piece
                (declare (ignore texture))
                (unless strip
                  (let ((spare (find-if (lambda (p) (and (= (- (second p) (first p)) (- below from)))) free)))
                    (if spare
                        (progn (setf free (remove spare free) (third piece) (third spare))
                               (when (fifth spare) (g-object-unref (fifth spare))))
                        (setf (third piece) (row-strip f (* (- below from) cw))))
                    (setf shows nil (fourth piece) nil)))
                (let ((content (and row (piece-content row from below))))
                  (unless (and shows (equal content shows))
                    (paint-piece f (third piece) layouts shows row from below)
                    ;; its texture, of what it showed, made again when next shown (STRIP-TEXTURE)
                    (when (fifth piece) (g-object-unref (fifth piece)) (setf (fifth piece) nil))
                    (setf (fourth piece) content)))))))
        (dolist (piece free)
          (cairo-surface-destroy (third piece))
          (when (fifth piece) (g-object-unref (fifth piece)))))
      (setf (kept-rows-pieces kept) new)
      new)))

(defun strip-texture (f strip width)
  "A texture of STRIP, a part of a row WIDTH pixels wide (ROW-STRIP), for the
graphics card to draw: made once each time the strip is painted, and moved
from row to row as it is, never sent to the card again while it stands."
  (let* ((scale (gtk-display-scale f))
         (pixels-wide (max 1 (* scale width)))
         (pixels-high (* scale (cell-height f)))
         (stride (cairo-image-surface-get-stride strip)))
    (cairo-surface-flush strip)
    (let* ((bytes (g-bytes-new (cairo-image-surface-get-data strip) (* stride pixels-high)))
           (texture (gdk-memory-texture-new pixels-wide pixels-high 29 bytes stride)))
      (g-bytes-unref bytes)
      texture)))

(defun place-picture (f slot texture x y width height)
  "Show TEXTURE at X Y in the window, WIDTH by HEIGHT of its pixels, in the
picture SLOT of F's grid: made the first time, only what changed since given
again, so that a texture moved from one row to another costs nothing but its
place. The texture, in the screen's own pixels, fills the picture's size."
  (let ((pictures (gtk-pictures f)))
    (when (>= slot (length pictures))
      (let ((picture (gtk-picture-new)))
        (gtk-picture-set-content-fit picture 0)
        (gtk-widget-set-can-target picture nil)
        (gtk-fixed-put (gtk-grid f) picture 0d0 0d0)
        (vector-push-extend (list picture nil nil nil nil nil) pictures)))
    (let ((entry (aref pictures slot)))
      (destructuring-bind (picture at-x at-y shown at-width at-height) entry
        (unless (eq texture shown)
          (when (null shown) (gtk-widget-set-visible picture t))
          (gtk-picture-set-paintable picture texture))
        (unless (and (eql width at-width) (eql height at-height))
          (gtk-widget-set-size-request picture width height))
        (unless (and (eql x at-x) (eql y at-y))
          (let ((transform (gsk-transform-move x y)))
            (gtk-fixed-set-child-transform (gtk-grid f) picture transform)
            (gsk-transform-unref transform)))
        (setf (aref pictures slot) (list picture x y texture width height))))))

(defun hide-pictures-from (f slot)
  "Hide the pictures of F's grid from SLOT on, which the frame does not use."
  (let ((pictures (gtk-pictures f)))
    (loop for i from slot below (length pictures)
          for entry = (aref pictures i)
          when (fourth entry)
            do (gtk-picture-set-paintable (first entry) (null-pointer))
               (gtk-widget-set-visible (first entry) nil)
               (setf (fourth entry) nil))))

(defun show-ground (f width height)
  "Show the window's ground colour across it, WIDTH by HEIGHT pixels, under
the rows: a picture of one pixel, stretched to that size."
  (let ((colour (nth-value 1 (face-colours nil))) (ground (gtk-ground f)))
    (unless (equal colour (gtk-ground-colour f))
      (sb-alien:with-alien ((pixel (array (sb-alien:unsigned 8) 4)))
        (destructuring-bind (r g b) colour
          (setf (sb-alien:deref pixel 0) (round (* 255 b))
                (sb-alien:deref pixel 1) (round (* 255 g))
                (sb-alien:deref pixel 2) (round (* 255 r))
                (sb-alien:deref pixel 3) 255))
        (let* ((bytes (g-bytes-new (sb-alien:alien-sap pixel) 4))
               (texture (gdk-memory-texture-new 1 1 29 bytes 4)))
          (g-bytes-unref bytes)
          (gtk-picture-set-paintable ground texture)
          (g-object-unref texture)))
      (setf (gtk-ground-colour f) colour))
    (gtk-widget-set-size-request ground width height)))

(defun image-texture (image)
  "A texture of IMAGE's pixels as they are, for the graphics card to draw:
red, green and blue, with alpha when it has four channels."
  (let* ((width (tuatara.frontend:image-width image))
         (channels (tuatara.frontend:image-channels image))
         (pixels (tuatara.frontend:image-pixels image))
         (stride (* width channels)))
    (tuatara.io:with-octets-sap (data pixels)
      (let* ((bytes (g-bytes-new data (length pixels)))
             ;; 5: R8G8B8A8, 7: R8G8B8 (GdkMemoryFormat)
             (texture (gdk-memory-texture-new width (tuatara.frontend:image-height image)
                                              (if (= channels 4) 5 7) bytes stride)))
        (g-bytes-unref bytes)
        texture))))

(defun layer-picture (layer texture)
  "A new picture of TEXTURE put on LAYER, sized as it is asked for and taking
no pointer events."
  (let ((picture (gtk-picture-new)))
    (gtk-widget-set-can-target picture nil)
    (gtk-picture-set-content-fit picture 0)
    (gtk-picture-set-paintable picture texture)
    (gtk-fixed-put layer picture 0d0 0d0)
    picture))

(defun place-images (f frame left top)
  "Show FRAME's pictures on F's pictures layer as the graphics card draws them,
the grid's top left at LEFT TOP: each in the cells it takes, as large as
fits, its proportions kept and never larger than it is, from their top left,
as PAINT-IMAGES paints them. A picture is given to the card once while it is
shown, however many windows show it, and drawn in each; those no longer
shown are let go of. Pictures shown from an offset, larger than their cells,
are painted whole (REDRAW)."
  (let ((layer (gtk-pictures-layer f)) (shown (gtk-shown-images f))
        (cw (cell-width f)) (ch (cell-height f)) (display (gtk-display-scale f)))
    (loop for entry being the hash-values of shown do (setf (second entry) 0))
    (loop for (image row column columns rows) in (frame-images frame)
          when (and (plusp rows) (plusp columns))
            do (let* ((entry (or (gethash image shown)
                                 (setf (gethash image shown) (list (image-texture image) 0))))
                      (placing (or (nth (second entry) (cddr entry))
                                   (car (last (setf (cddr entry)
                                                    (append (cddr entry)
                                                            (list (list (layer-picture layer (first entry)) nil)))))))))
                 (incf (second entry))
                 (destructuring-bind (picture place) placing
                   (let* ((scale (tuatara.frontend:image-fit-scale image (* columns cw) (* rows ch) (/ 1d0 display)))
                          (wanted (list row column columns rows scale left top cw ch)))
                     (unless (equal wanted place)
                       (gtk-widget-set-size-request picture
                                                    (max 1 (round (* scale (tuatara.frontend:image-width image))))
                                                    (max 1 (round (* scale (tuatara.frontend:image-height image)))))
                       (let ((at (gsk-transform-move (+ left (* column cw)) (+ top (* row ch)))))
                         (gtk-fixed-set-child-transform layer picture at)
                         (gsk-transform-unref at))
                       (setf (second placing) wanted))))))
    (loop for image being the hash-keys of shown using (hash-value entry)
          do (let ((used (second entry)))
               (when (nthcdr used (cddr entry))
                 (dolist (placing (nthcdr used (cddr entry))) (gtk-fixed-remove layer (first placing)))
                 (if (zerop used)
                     (progn (g-object-unref (first entry)) (remhash image shown))
                     (setf (cdr (nthcdr (1- used) (cddr entry))) nil)))))))

(defun place-cursor (f frame left top)
  "Put F's cursor's drawing area where FRAME's cursor is, as large as the
cursor and what the input method is composing, the grid's top left at LEFT
TOP, and have it painted again; tell the input method where it is."
  (multiple-value-bind (x y w h) (frame-cursor-pixels f frame)
    (let* ((preedit (gtk-preedit f))
           (w (if preedit (max w (+ 2 (* (cell-width f) (tuatara.display:string-columns preedit)))) w))
           (area (gtk-cursor-area f)))
      (setf (gtk-cursor-at f) (cons x y))
      (gtk-widget-set-size-request area (ceiling w) (ceiling h))
      (let ((transform (gsk-transform-move (+ left x) (+ top y))))
        (gtk-fixed-set-child-transform (gtk-above f) area transform)
        (gsk-transform-unref transform))
      (gtk-widget-queue-draw area)
      (gtk-im-context-set-cursor-location (gtk-im f) (round (+ left x)) (round (+ top y)) (round w) (round h)))))

(define-callback on-draw-cursor :void ((area :pointer) (cr :pointer) (width :int) (height :int) (data :pointer))
  (declare (ignore area width height data))
  (let* ((f *frontend*)
         (frame (and f (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f)))))
    (when frame
      (let ((layouts (frame-layouts f)) (at (gtk-cursor-at f)))
        (cairo-translate cr (float (- (car at)) 1d0) (float (- (cdr at)) 1d0))
        (let ((preedit (gtk-preedit f)))
          (if preedit
              (paint-cursor f cr layouts frame (gtk-focused f)
                            :column (paint-preedit f cr layouts frame preedit) :shape :bar)
              (paint-cursor f cr layouts frame (gtk-focused f))))))))

(defun follow-display-scale (f widget)
  "Have F's text set out for the scale of the screen WIDGET is on, as layouts
are set out for the scale in force when made; and the desktop's colours read
when the window takes them (*WINDOW-COLOURS-FROM-DESKTOP*) and they are not
read yet."
  (let ((scale (max 1 (gtk-widget-get-scale-factor widget))))
    (unless (= scale (gtk-display-scale f))
      (forget-layouts f)
      (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-display-scale f) scale))))
  (when (and *window-colours-from-desktop* (null *desktop-colours*))
    (read-desktop-colours widget)))

(defun place-pieces (f frame)
  "Show FRAME as pictures the graphics card draws: the ground, each part of
each row as kept painted (KEEP-ROWS) and given to the card once
\(STRIP-TEXTURE), the frame's own pictures over them (PLACE-IMAGES), the
cursor over those."
  (let* ((area (gtk-area f))
         (width (gtk-widget-get-width area))
         (height (gtk-widget-get-height area)))
    (follow-display-scale f area)
    (destructuring-bind (top right bottom left) (gtk-padding f)
      (declare (ignore bottom))
      (let* ((cw (cell-width f)) (ch (cell-height f))
             (grid-width (* cw (max 1 (floor (- width left right) cw))))
             (slot 0))
        (show-ground f width height)
        (loop for row-pieces across (keep-rows f grid-width (frame-rows frame) (frame-layouts f))
              for y from top by ch
              do (dolist (piece row-pieces)
                   (destructuring-bind (from below strip shows texture) piece
                     (declare (ignore shows))
                     (place-picture f slot (or texture (setf (fifth piece) (strip-texture f strip (* (- below from) cw))))
                                    (+ left (* from cw)) y (* (- below from) cw) ch)
                     (incf slot))))
        (hide-pictures-from f slot)
        (place-images f frame left top)
        (place-cursor f frame left top)))))

(defun let-go-of-pictures (f on-card)
  "Let go of the pictures F keeps for the way of drawing it no longer uses:
drawn by the graphics card when ON-CARD, the cairo surfaces painting whole
uses (PAINT-IMAGES); else the pictures and textures on the card's layer
\(PLACE-IMAGES)."
  (if on-card
      (let ((surfaces (gtk-surfaces f)))
        (loop for surface being the hash-values of surfaces do (cairo-surface-destroy surface))
        (clrhash surfaces))
      (let ((shown (gtk-shown-images f)))
        (loop for (texture nil . placings) being the hash-values of shown
              do (dolist (placing placings) (gtk-fixed-remove (gtk-pictures-layer f) (first placing)))
                 (g-object-unref texture))
        (clrhash shown))))

(defun drawn-on-card (f on)
  "Show F's rows as pictures the graphics card draws (PLACE-PIECES) when ON,
else painted whole by the drawing area (PAINT-FRAME), as a frame with rows
drawn large or a picture shown from an offset is; the pictures kept for the
other way are let go of (LET-GO-OF-PICTURES)."
  (unless (eq on (gtk-on-card f))
    (setf (gtk-on-card f) on)
    (let-go-of-pictures f on)
    (gtk-drawing-area-set-draw-func (gtk-area f) (if on (null-pointer) (callback on-draw))
                                    (null-pointer) (null-pointer))
    (gtk-widget-set-visible (gtk-grid f) on)
    (gtk-widget-set-visible (gtk-pictures-layer f) on)
    (gtk-widget-set-visible (gtk-above f) on)))

(defun redraw (f)
  "Show F's window again as the frame it was last given is, on GTK's thread:
its rows and pictures as the graphics card draws them, or, for a frame with
rows drawn large, which reach into the rows round them, or a picture shown
from an offset, cut to its cells, painted whole by the drawing area."
  (when (gtk-area f)
    (let ((frame (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f))))
      (if (and frame (gtk-grid f)
               (notany #'drawn-across-rows-p (frame-rows frame))
               ;; a picture shown from an offset is cut to its cells: painted whole
               (notany #'sixth (frame-images frame)))
          (progn
            (drawn-on-card f t)
            (handler-case (place-pieces f frame)
              (error (e) (format *error-output* "~&Tuatara could not show the rows: ~a~%" e))))
          (progn
            (drawn-on-card f nil)
            (gtk-widget-queue-draw (gtk-area f)))))))

(defun paint-frame (f cr width height frame focused)
  "Paint FRAME on CR, WIDTH by HEIGHT pixels: the ground, all of it, then inside
the room round the grid (GTK-PADDING), and no further than its whole cells,
every row (as kept painted from the frame before, KEEP-ROWS, unless one is
drawn large or over others, which reaches into the rows round it), the
pictures, what the input method is composing, the cursor."
  (let ((layouts (frame-layouts f)))
    (fill-ground cr nil 0 0 width height)
    (when frame
      (let ((rows (frame-rows frame)))
        (if (some #'drawn-across-rows-p rows)
            (progn (forget-kept-rows f)
                   (paint-rows-whole f cr layouts rows width height))
            (with-grid (f cr width height grid-width)
              (loop with cw = (cell-width f) and ch = (cell-height f)
                    for pieces across (keep-rows f grid-width rows layouts)
                    for y from 0 by ch
                    do (loop for (from below strip) in pieces
                             do (cairo-set-source-surface cr strip (float (* from cw) 1d0) (float y 1d0))
                                (cairo-rectangle cr (float (* from cw) 1d0) (float y 1d0)
                                                 (float (* (- below from) cw) 1d0) (float ch 1d0))
                                (cairo-fill cr)))))
        (with-grid (f cr width height grid-width)
          (paint-images f cr frame)
          (let ((preedit (gtk-preedit f)))
            (if preedit
                (paint-cursor f cr layouts frame focused
                              :column (paint-preedit f cr layouts frame preedit) :shape :bar)
                (paint-cursor f cr layouts frame focused))))))))

(define-callback on-draw :void ((area :pointer) (cr :pointer) (width :int) (height :int) (data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when f
      (follow-display-scale f area)
      (let ((frame (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f))))
        (handler-case (paint-frame f cr width height frame (gtk-focused f))
          (error (e) (format *error-output* "~&Tuatara could not paint: ~a~%" e)))
        ;; where the cursor is, for the input method's window
        (when frame
          (multiple-value-bind (x y w h) (frame-cursor-pixels f frame)
            (gtk-im-context-set-cursor-location (gtk-im f)
                                                (round (+ (fourth (gtk-padding f)) x)) (round (+ (first (gtk-padding f)) y))
                                                (round w) (round h))))))))

(defun call-with-painted-frame (function frame &key (columns 80) (rows 24) (font (font-name)) (focused t) preedit
                                                    (padding 0))
  "Paint FRAME as the window would, COLUMNS by ROWS cells in FONT with PADDING
pixels round them, in memory, without a display, with PREEDIT as what the input
method is composing, and call FUNCTION with the image surface, its width and
height in pixels, and the width and height of a cell; its value is returned."
  (load-gtk-libraries)
  (with-gtk-float-traps
    (let ((f (make-gtk-frontend)))
      (setf (gtk-fonts f) (make-fonts font)
            (gtk-preedit f) preedit
            (gtk-padding f) (multiple-value-list (tuatara.frontend:padding-sides padding)))
      (measure-font f)
      (let* ((width (+ (* columns (cell-width f)) (second (gtk-padding f)) (fourth (gtk-padding f))))
             (height (+ (* rows (cell-height f)) (first (gtk-padding f)) (third (gtk-padding f))))
             (surface (cairo-image-surface-create 0 width height))
             (cr (cairo-create surface)))
        (unwind-protect
             (progn (paint-frame f cr width height frame focused)
                    (cairo-surface-flush surface)
                    (funcall function surface width height (cell-width f) (cell-height f)))
          (cairo-destroy cr)
          (cairo-surface-destroy surface)
          (forget-layouts f)
          (loop for font across (gtk-fonts f) do (pango-font-description-free font))
          (forget-face-fonts f))))))

(defun pixel-colour (surface x y)
  "The colour of the pixel at X Y of the image SURFACE, as \"#rrggbb\"."
  (let* ((data (cairo-image-surface-get-data surface))
         (offset (+ (* y (cairo-image-surface-get-stride surface)) (* 4 x))))
    ;; 32 bits a pixel, blue first in memory on this machine
    (tuatara.display:colour-hex
     (list (sb-sys:sap-ref-8 data (+ offset 2)) (sb-sys:sap-ref-8 data (+ offset 1))
           (sb-sys:sap-ref-8 data offset)))))

(defun paint-frame-to-png (frame path &rest options)
  "Paint FRAME as the window would into a PNG file at PATH, without a display;
OPTIONS as for CALL-WITH-PAINTED-FRAME. Returns the width and height in pixels."
  (apply #'call-with-painted-frame
         (lambda (surface width height cell-width cell-height)
           (declare (ignore cell-width cell-height))
           (cairo-surface-write-to-png surface (namestring path))
           (values width height))
         frame options))
