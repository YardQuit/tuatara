;;;; src/frontend/interface.lisp --- Keys, events and the painter interface.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every frontend (ncurses, GTK4, SDL, the fake one for tests) implements the
;;;; generic functions below and nothing else.  The editor never talks to a
;;;; toolkit directly.

(in-package #:tuatara.frontend)

;;; Keys

(defstruct (key (:constructor make-key (base &key ctrl meta shift column row)) (:copier nil))
  "A key press. BASE is a character or a keyword naming a special key (:up,
:down, :left, :right, :home, :end, :page-up, :page-down, :backspace, :delete,
:insert, :enter, :tab, :escape, :f1 ... :f12), a turn of the mouse wheel
(:wheel-up, :wheel-down), or what the first mouse button did (:down-mouse-1
pressed, :drag-mouse-1 moved while held, :double-mouse-1 and :triple-mouse-1
pressed twice or three times at once). CTRL, META and SHIFT are the
modifiers; COLUMN and ROW, the cell of the screen a mouse key happened at."
  (base #\Nul :type (or character keyword) :read-only t)
  (ctrl nil :type boolean :read-only t)
  (meta nil :type boolean :read-only t)
  (shift nil :type boolean :read-only t)
  (column nil :read-only t)
  (row nil :read-only t))

(setf (documentation 'make-key 'function) "Return a KEY for BASE with the modifiers CTRL, META and SHIFT."
      (documentation 'key-p 'function) "True when OBJECT is a KEY."
      (documentation 'key-base 'function) "The character or special-key keyword of KEY."
      (documentation 'key-ctrl 'function) "True when Control is held in KEY."
      (documentation 'key-meta 'function) "True when Meta (Alt) is held in KEY."
      (documentation 'key-shift 'function) "True when Shift is held in KEY (only reported for special keys)."
      (documentation 'key-column 'function) "The screen column a mouse KEY happened at, or NIL."
      (documentation 'key-row 'function) "The screen row a mouse KEY happened at, or NIL.")

(defun key-spec (key)
  "The canonical, EQUAL-comparable form of KEY: a list of modifier keywords in
the order :ctrl :meta :shift followed by the base."
  (append (and (key-ctrl key) '(:ctrl))
          (and (key-meta key) '(:meta))
          (and (key-shift key) '(:shift))
          (list (key-base key))))

(defun spec-key (spec)
  "The KEY that SPEC, a key spec as KEY-SPEC gives one, stands for."
  (make-key (car (last spec)) :ctrl (and (member :ctrl spec) t) :meta (and (member :meta spec) t)
                              :shift (and (member :shift spec) t)))

(defparameter *key-names*
  '(("RET" . :enter) ("SPC" . #\Space) ("TAB" . :tab) ("DEL" . :backspace) ("ESC" . :escape)
    ("<up>" . :up) ("<down>" . :down) ("<left>" . :left) ("<right>" . :right)
    ("<home>" . :home) ("<end>" . :end) ("<prior>" . :page-up) ("<next>" . :page-down)
    ("<delete>" . :delete) ("<deletechar>" . :delete) ("<insert>" . :insert) ("<backspace>" . :backspace)
    ("<return>" . :enter) ("<tab>" . :tab) ("<escape>" . :escape)
    ("<f1>" . :f1) ("<f2>" . :f2) ("<f3>" . :f3) ("<f4>" . :f4) ("<f5>" . :f5) ("<f6>" . :f6)
    ("<f7>" . :f7) ("<f8>" . :f8) ("<f9>" . :f9) ("<f10>" . :f10) ("<f11>" . :f11) ("<f12>" . :f12)
    ("<wheel-up>" . :wheel-up) ("<wheel-down>" . :wheel-down)
    ("<down-mouse-1>" . :down-mouse-1) ("<drag-mouse-1>" . :drag-mouse-1) ("<up-mouse-1>" . :up-mouse-1) ("<mouse-2>" . :mouse-2)
    ("<double-mouse-1>" . :double-mouse-1) ("<triple-mouse-1>" . :triple-mouse-1))
  "The key names of key sequences (RET, SPC, TAB, <f5> and the rest) and the
base each stands for.")

(defun parse-key (token)
  "Parse one key into a key spec: a character or a key name (*KEY-NAMES*),
after any of C- for Control, M- for Meta and S- for Shift, as in \"C-x\",
\"M-RET\", \"<f5>\" and \"a\". Shift on a letter is its capital, as the
keyboard gives it: \"C-M-S-v\" and \"C-M-V\" are one key."
  (let ((ctrl nil) (meta nil) (shift nil) (rest token))
    (loop while (and (> (length rest) 2) (char= (char rest 1) #\-) (find (char rest 0) "CMS"))
          do (case (char rest 0)
               (#\C (setf ctrl t)) (#\M (setf meta t)) (#\S (setf shift t)))
             (setf rest (subseq rest 2)))
    (let ((base (cond ((cdr (assoc rest *key-names* :test #'string=)))
                      ((= (length rest) 1) (char rest 0))
                      (t (error "Unknown key ~s in ~s." rest token)))))
      (when (and shift (characterp base) (alpha-char-p base))
        (setf base (char-upcase base) shift nil))
      (key-spec (make-key base :ctrl ctrl :meta meta :shift shift)))))

(defun parse-key-sequence (string)
  "Parse a key sequence, keys as PARSE-KEY reads them with spaces between
(\"C-x C-f\"), into a list of key specs."
  (mapcar #'parse-key (tuatara.text:split-words string " ")))

(defun key-description (spec)
  "Key spec SPEC as messages and help write it: C- for Control, M- for Meta and
S- for Shift before the key's character or name (C-x, M-x, S-TAB, RET, <f5>)."
  (let ((base (car (last spec))))
    (format nil "~:[~;C-~]~:[~;M-~]~:[~;S-~]~a"
            (member :ctrl spec) (member :meta spec) (member :shift spec)
            (typecase base
              (character (case base (#\Space "SPC") (t (string base))))
              (t (or (car (rassoc base *key-names*)) (string-downcase base)))))))

(defun key-sequence-description (specs)
  "The key specs SPECS as messages and help write a key sequence: each as
KEY-DESCRIPTION writes it, with a space between (C-x C-f)."
  (format nil "~{~a~^ ~}" (mapcar #'key-description specs)))

;;; Events

(defstruct (event (:constructor make-event (kind &key key width height files column row text)) (:copier nil))
  "Something the frontend reports: KIND is :key (with KEY), :resize (with WIDTH
and HEIGHT in cells), :drop when files were dropped on the window (FILES,
their paths, on the cell COLUMN, ROW), :paste when text was pasted into the
terminal Tuatara runs in (TEXT, its lines ending in newlines), to go in as
text and never as keys, :close when the window was asked to close, or :quit
when the frontend can deliver nothing more."
  (kind :key :type keyword :read-only t)
  (key nil :type (or null key) :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t)
  (files '() :type list :read-only t)
  (column nil :read-only t)
  (row nil :read-only t)
  (text nil :type (or null string) :read-only t))

(setf (documentation 'make-event 'function) "Return an EVENT of KIND with the given KEY, WIDTH, HEIGHT, FILES, COLUMN, ROW and TEXT."
      (documentation 'event-p 'function) "True when OBJECT is an EVENT."
      (documentation 'event-kind 'function) "The kind of EVENT: :key, :resize, :drop, :paste, :close, :quit, or :tick when a timed read found no key."
      (documentation 'event-key 'function) "The KEY of a :key EVENT."
      (documentation 'event-width 'function) "Width in cells of a :resize EVENT."
      (documentation 'event-height 'function) "Height in cells of a :resize EVENT."
      (documentation 'event-files 'function) "The paths of the files of a :drop EVENT."
      (documentation 'event-column 'function) "The column of the cell a :drop EVENT happened on."
      (documentation 'event-row 'function) "The row of the cell a :drop EVENT happened on."
      (documentation 'event-text 'function) "The text of a :paste EVENT.")

;;; The painter interface

(defgeneric frontend-start (frontend)
  (:documentation "Take over the display: initialise the toolkit or terminal."))

(defgeneric frontend-stop (frontend)
  (:documentation "Release the display, restoring the terminal or closing the window."))

(defgeneric frontend-size (frontend)
  (:documentation "Two values: the width and height of the display in cells."))

(defgeneric frontend-read-event (frontend)
  (:documentation "Block until the next EVENT and return it."))

(defgeneric frontend-read-event-timeout (frontend seconds)
  (:documentation "Like FRONTEND-READ-EVENT, but after SECONDS without input return
an event of kind :TICK, so that the editor can show the progress of work going
on in the background and take in its result. The default waits as long as it
takes and never ticks.")
  (:method (frontend seconds) (declare (ignore seconds)) (frontend-read-event frontend)))

(defgeneric frontend-wake (frontend)
  (:documentation "Make a FRONTEND-READ-EVENT or FRONTEND-READ-EVENT-TIMEOUT
waiting on another thread return an event of kind :TICK soon, so that the
editor takes in news from other threads (a file changed on disk) without
looking for it at intervals. Any thread may call it. The default does nothing.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-input-pending-p (frontend)
  (:documentation "True when input is waiting to be read, so that long work on the
editor thread, such as a search through a file of many gigabytes, can give way
to the next key. Must not block. The default answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-colour-count (frontend)
  (:documentation "How many colours FRONTEND can show: 0 for none (a monochrome terminal),
8, 16 or 256 for terminals, 16777216 for a GUI. The defaults choose the
colourless theme below 8.")
  (:method (frontend) (declare (ignore frontend)) 16777216))

(defgeneric frontend-description (frontend)
  (:documentation "Where FRONTEND shows the editor, as words that follow \"Tuatara
... on SBCL ...,\": \"in a GTK 4.22 window\", \"in the terminal (kitty)\"; NIL
for none.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-health (frontend)
  (:documentation "What \\[check-health] says of FRONTEND, as a list of (LABEL . TEXT):
how it shows the editor, and how it reaches the system clipboard. The default
says nothing.")
  (:method (frontend) (declare (ignore frontend)) '()))

(defgeneric frontend-renderers (frontend)
  (:documentation "The ways FRONTEND can be drawn that \\[check-health] measures, as
*GUI-RENDERER* names them (:gl, :vulkan), the one standard first, and as a
second value the way the settings ask for; NIL for a frontend drawn one way
only, as a terminal is.")
  (:method (frontend) (declare (ignore frontend)) '()))

(defgeneric frontend-measure-drawing (frontend renderer)
  (:documentation "Measure how FRONTEND draws the editor when drawn as RENDERER says
\(one of FRONTEND-RENDERERS), as a list: :RENDERER, the name of what drew it;
:KEYS given and :FRAMES drawn while a key was held; :MEDIAN and :SLOWEST
\(the slowest of every twenty) milliseconds from a key to the frame showing
it; NIL when it could not be measured. Takes a few seconds; called on a
thread of its own.")
  (:method (frontend renderer) (declare (ignore frontend renderer)) nil))

(defgeneric frontend-terminal-device (frontend)
  (:documentation "The device of the terminal FRONTEND shows the editor in
\(/dev/pts/3), which a program can be lent (FRONTEND-LEND-TERMINAL); NIL for
a frontend that is no terminal.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-lend-terminal (frontend function)
  (:documentation "Call FUNCTION with the terminal FRONTEND shows the editor
in given back to it as the terminal was before the editor took it, so that a
program FUNCTION runs (gpg-agent's pinentry asking for a passphrase) can use
it; then take it again and paint it whole. Returns what FUNCTION returns. A
frontend that is no terminal just calls FUNCTION.")
  (:method (frontend function) (declare (ignore frontend)) (funcall function)))

(defgeneric frontend-set-clipboard (frontend text)
  (:documentation "Put TEXT on the system clipboard, where other programs paste it
from. True when it is there. Must not hang. The default has no clipboard.")
  (:method (frontend text) (declare (ignore frontend text)) nil))

(defgeneric frontend-clipboard (frontend)
  (:documentation "The text on the system clipboard, or NIL when there is none or it
cannot be read. Must not hang: a clipboard that does not answer soon counts as
none. The default has no clipboard.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-set-primary-selection (frontend text)
  (:documentation "Make TEXT the desktop's primary selection, the text the
middle button pastes in other programs. Must not hang. The default has none.")
  (:method (frontend text) (declare (ignore frontend text)) nil))

(defgeneric frontend-primary-selection (frontend)
  (:documentation "The text selected last in any program, the desktop's primary
selection (what the middle button pastes), or NIL when there is none or it
cannot be read. Must not hang, as FRONTEND-CLIPBOARD. The default has none.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-font-steps (frontend)
  (:documentation "How many steps FRONTEND's font is scaled from the size it
started with, each step 1.2 times the last (negative: smaller); NIL for a
frontend whose font is not its own to change, as a terminal's. The default
answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric (setf frontend-font-steps) (steps frontend)
  (:documentation "Scale FRONTEND's font to STEPS steps from the size it started
with. The window keeps its size and shows more or fewer rows and columns, the
editor told as for any resize; fewer steps are taken when more would leave the
window too few rows. Returns the steps taken. The default does nothing.")
  (:method (steps frontend) (declare (ignore frontend)) steps))

(defgeneric frontend-fonts (frontend)
  (:documentation "The names of the fixed-width font families FRONTEND can show,
sorted; NIL for a frontend whose font is not its own, as a terminal's. The
default answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-font-faces (frontend)
  (:documentation "Every font family FRONTEND can show, as a list of (FAMILY
FIXED-WIDTH-P VARIANTS), the fixed-width families first and each group sorted
by name; VARIANTS is a list of (NAME FONT), one for each of the family's
variants (Regular, Bold, Light Italic...), FONT the full name that chooses it at
the size in use (\"DejaVu Sans Mono 12\"). NIL for a frontend whose font
is not its own. The default answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-set-font (frontend name)
  (:documentation "Show FRONTEND's text in the font NAME, a family with any
style and size after it (\"DejaVu Sans Mono Bold 12\"); without a size, the
size of the font in use. Returns the font's full name, or NIL when there is no
such family or the font is not FRONTEND's own. The default returns NIL.")
  (:method (frontend name) (declare (ignore frontend name)) nil))

(defun padding-sides (value)
  "Four values, the room at the top, right, bottom and left, from VALUE as the
padding options take it: one number for all four sides; two for the top and
bottom, then the right and left; three for the top, the right and left, the
bottom; four for the top, right, bottom and left, clockwise. What is not a
number, or is less than 0, counts as 0."
  (flet ((side (x) (if (realp x) (max 0 (round x)) 0)))
    (destructuring-bind (&optional (top 0) (right top) (bottom top) (left right) &rest ignore)
        (if (listp value) value (list value))
      (declare (ignore ignore))
      (values (side top) (side right) (side bottom) (side left)))))

(defgeneric frontend-settings-changed (frontend)
  (:documentation "Take in again the settings FRONTEND reads from the init file
and the session (the window's font and padding, the terminal's padding), which
may have changed: the editor calls it once the init file has run and after
every command, so it must be cheap when nothing changed. A frontend whose size
in cells changes because of them reports a resize. The default does nothing.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-desktop-style (frontend)
  (:documentation "The desktop's style as FRONTEND knows it, :dark or :light, and
NIL when it does not know it, as a terminal does not. The frontend wakes the
editor (FRONTEND-WAKE) when the desktop switches. The default answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-toggle-state (frontend state)
  (:documentation "Turn FRONTEND's window to STATE, :fullscreen (filling the
screen, no title bar) or :maximised (as large as the screen allows), or back
from it when it is so already. True when FRONTEND has a window of its own to
do it with; the default answers NIL.")
  (:method (frontend state) (declare (ignore frontend state)) nil))

(defgeneric frontend-set-title (frontend title)
  (:documentation "Show TITLE as the title of FRONTEND's window, where the desktop
names it. Called when it changes. The default does nothing: a terminal's title
is its own.")
  (:method (frontend title) (declare (ignore frontend title)) nil))

(defgeneric frontend-begin-frame (frontend)
  (:documentation "Start painting a frame; everything is cleared."))

(defgeneric frontend-put-row (frontend row string &key face spans scales over parts)
  (:documentation "Paint STRING on ROW (0-based) from column 0 and clear the rest of
the row. FACE (a face name or a list of names) paints the whole row, the cleared
rest included, as for the mode line or a selected candidate. SPANS is a list of
(START END FACE) over character indices of STRING whose characters get that
face on top of the row's. Text with no face shows the :default face.
Where the frontend draws rows large (*ROW-SCALING*), SCALES lists parts of the
row drawn otherwise, each (START END SCALE SHIFT HEIGHT): from column START
below END, the text from that column on drawn SCALE times as tall and wide (a
rational of 1 or more) and SHIFT rows lower, over a band HEIGHT rows tall
\(the row's gutter, drawn as it is, is as tall as the band of its text), each
of its characters taking SCALE columns; SCALE 0 leaves those cells to the
parts moved over them. OVER lists (LEFT TEXT SPANS) drawn over everything
at the row's own cells, the first on top: a list of candidates. A frontend
that draws all rows alike is given neither. PARTS lists the columns where
the row's parts begin after the first (a window's text after its gutter, the
window beside it), in order: a frontend that keeps what it painted may keep
each part apart, as the gutter's numbers stay on a row while the text
scrolls past them."))

(defgeneric frontend-set-cursor (frontend row column shape &optional colour)
  (:documentation "Place the cursor at ROW and COLUMN with SHAPE, :block, :bar or
:underline, in COLOUR, as faces take colours (see TUATARA.DISPLAY:COLOUR-HEX),
or the text's when NIL; SHAPE :hidden shows no cursor (over a picture)."))

(defgeneric frontend-end-frame (frontend)
  (:documentation "Finish the frame and show it."))

;;; Pictures

(defstruct (image (:constructor %make-image (width height channels pixels id)) (:copier nil))
  "A picture to show: WIDTH by HEIGHT pixels, each CHANNELS octets (3, red,
green and blue; 4, with alpha), row after row from the top left, in PIXELS;
ID, a number no other picture of this session has, by which a frontend tells
pictures apart."
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t)
  (channels 4 :type fixnum :read-only t)
  (pixels nil :read-only t)
  (id 0 :type fixnum :read-only t))

(setf (documentation 'image-p 'function) "True when OBJECT is an IMAGE."
      (documentation 'image-width 'function) "The picture's width in pixels."
      (documentation 'image-height 'function) "The picture's height in pixels."
      (documentation 'image-channels 'function) "The octets of each pixel of the picture: 3 (RGB) or 4 (RGBA)."
      (documentation 'image-pixels 'function) "The picture's pixels, row after row from the top left."
      (documentation 'image-id 'function) "The number that tells the picture apart from every other of the session.")

(defun image-fit-scale (image width height &optional (most 1))
  "The scale IMAGE is drawn at to fit in WIDTH by HEIGHT pixels, its
proportions kept: as large as fits, never more than MOST (1, its own size,
unless given)."
  (min most (/ width (max 1 (image-width image))) (/ height (max 1 (image-height image)))))

(sb-ext:defglobal **image-count** (list 0)
  "How many pictures have been made this session: the last one's ID.")

(defun make-image (width height channels pixels)
  "A picture of WIDTH by HEIGHT pixels of CHANNELS octets each (3 or 4), from
PIXELS, row after row from the top left, with an ID of its own."
  (%make-image width height channels pixels (sb-ext:atomic-incf (car **image-count**))))

(defgeneric frontend-images-p (frontend)
  (:documentation "True when FRONTEND can show pictures (FRONTEND-PUT-IMAGE). The
default answers NIL: a window showing a picture then shows its buffer's text.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-cell-pixels (frontend)
  (:documentation "The width and height of one of FRONTEND's cells in pixels, as
two values, for sizing pictures; NIL when it does not know. The default answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-put-image (frontend image row column columns rows &optional offset)
  (:documentation "Show IMAGE over the frame being painted, in the COLUMNS by ROWS
cells from ROW and COLUMN (0-based), which the rows painted before it left
blank: as large as it fits there, its proportions kept, never larger than its
own size, from the top left. With OFFSET, a list of a row and a column of
cells, at its own size instead (FRONTEND-CELL-PIXELS), the part from that row
and column of it on (a page scrolled). A picture put in no frame for a while
may be let go of. Only called when FRONTEND-IMAGES-P; the default does nothing.")
  (:method (frontend image row column columns rows &optional offset)
    (declare (ignore frontend image row column columns rows offset))
    nil))

;;; kitty's pictures in text: each cell of a picture is U+10EEEE, the picture's
;;; number its colour, marks after it giving the cell's row and column of the
;;; picture (as kitty's graphics protocol has them, its Unicode placeholders)

(defparameter *kitty-marks*
  #(#x0305 #x030D #x030E #x0310 #x0312 #x033D #x033E #x033F #x0346 #x034A #x034B #x034C #x0350 #x0351 #x0352
    #x0357 #x035B #x0363 #x0364 #x0365 #x0366 #x0367 #x0368 #x0369 #x036A #x036B #x036C #x036D #x036E #x036F
    #x0483 #x0484 #x0485 #x0486 #x0487 #x0592 #x0593 #x0594 #x0595 #x0597 #x0598 #x0599 #x059C #x059D #x059E
    #x059F #x05A0 #x05A1 #x05A8 #x05A9 #x05AB #x05AC #x05AF #x05C4 #x0610 #x0611 #x0612 #x0613 #x0614 #x0615
    #x0616 #x0617 #x0657 #x0658 #x0659 #x065A #x065B #x065D #x065E #x06D6 #x06D7 #x06D8 #x06D9 #x06DA #x06DB
    #x06DC #x06DF #x06E0 #x06E1 #x06E2 #x06E4 #x06E7 #x06E8 #x06EB #x06EC #x0730 #x0732 #x0733 #x0735 #x0736
    #x073A #x073D #x073F #x0740 #x0741 #x0743 #x0745 #x0747 #x0749 #x074A #x07EB #x07EC #x07ED #x07EE #x07EF
    #x07F0 #x07F1 #x07F3 #x0816 #x0817 #x0818 #x0819 #x081B #x081C #x081D #x081E #x081F #x0820 #x0821 #x0822
    #x0823 #x0825 #x0826 #x0827 #x0829 #x082A #x082B #x082C #x082D #x0951 #x0953 #x0954 #x0F82 #x0F83 #x0F86
    #x0F87 #x135D #x135E #x135F #x17DD #x193A #x1A17 #x1A75 #x1A76 #x1A77 #x1A78 #x1A79 #x1A7A #x1A7B #x1A7C
    #x1B6B #x1B6D #x1B6E #x1B6F #x1B70 #x1B71 #x1B72 #x1B73 #x1CD0 #x1CD1 #x1CD2 #x1CDA #x1CDB #x1CE0 #x1DC0
    #x1DC1 #x1DC3 #x1DC4 #x1DC5 #x1DC6 #x1DC7 #x1DC8 #x1DC9 #x1DCB #x1DCC #x1DD1 #x1DD2 #x1DD3 #x1DD4 #x1DD5
    #x1DD6 #x1DD7 #x1DD8 #x1DD9 #x1DDA #x1DDB #x1DDC #x1DDD #x1DDE #x1DDF #x1DE0 #x1DE1 #x1DE2 #x1DE3 #x1DE4
    #x1DE5 #x1DE6 #x1DFE #x20D0 #x20D1 #x20D4 #x20D5 #x20D6 #x20D7 #x20DB #x20DC #x20E1 #x20E7 #x20E9 #x20F0
    #x2CEF #x2CF0 #x2CF1 #x2DE0 #x2DE1 #x2DE2 #x2DE3 #x2DE4 #x2DE5 #x2DE6 #x2DE7 #x2DE8 #x2DE9 #x2DEA #x2DEB
    #x2DEC #x2DED #x2DEE #x2DEF #x2DF0 #x2DF1 #x2DF2 #x2DF3 #x2DF4 #x2DF5 #x2DF6 #x2DF7 #x2DF8 #x2DF9 #x2DFA
    #x2DFB #x2DFC #x2DFD #x2DFE #x2DFF #xA66F #xA67C #xA67D #xA6F0 #xA6F1 #xA8E0 #xA8E1 #xA8E2 #xA8E3 #xA8E4
    #xA8E5 #xA8E6 #xA8E7 #xA8E8 #xA8E9 #xA8EA #xA8EB #xA8EC #xA8ED #xA8EE #xA8EF #xA8F0 #xA8F1 #xAAB0 #xAAB2
    #xAAB3 #xAAB7 #xAAB8 #xAABE #xAABF #xAAC1 #xFE20 #xFE21 #xFE22 #xFE23 #xFE24 #xFE25 #xFE26 #x10A0F #x10A38
    #x1D185 #x1D186 #x1D187 #x1D188 #x1D189 #x1D1AA #x1D1AB #x1D1AC #x1D1AD #x1D242 #x1D243 #x1D244)
  "The marks that give the row or column of a cell of a picture, the Nth for N:
297 of them, so a picture takes at most 297 rows and columns.")

(defconstant +kitty-placeholder+ #x10EEEE "The character of a cell of a picture, in kitty's pictures in text.")
