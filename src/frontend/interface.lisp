;;;; src/frontend/interface.lisp --- Keys, events and the painter interface.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every frontend (the terminal's, GTK4's, a client's at the other end of a
;;;; connection and the fake one for tests) implements the generic functions
;;;; below and nothing else. The editor never talks to a toolkit directly.

(in-package #:tuatara.frontend)

;;; Keys

(define-record (key (:constructor make-key (base &key ctrl meta shift column row)) (:copier nil))
  "A key press. BASE is a character or a keyword naming a special key (:up,
:down, :left, :right, :home, :end, :page-up, :page-down, :backspace, :delete,
:insert, :enter, :tab, :escape, :f1 ... :f12), a turn of the mouse wheel
(:wheel-up, :wheel-down), or what the first mouse button did (:down-mouse-1
pressed, :drag-mouse-1 moved while held, :double-mouse-1 and :triple-mouse-1
pressed twice or three times at once). CTRL, META and SHIFT are the
modifiers; COLUMN and ROW, the cell of the screen a mouse key happened at."
  (base #\Nul :type (or character keyword) :read-only t :documentation "The character or special-key keyword of KEY.")
  (ctrl nil :type boolean :read-only t :documentation "True when Control is held in KEY.")
  (meta nil :type boolean :read-only t :documentation "True when Meta (Alt) is held in KEY.")
  (shift nil :type boolean :read-only t
         :documentation "True when Shift is held in KEY (only reported for special keys).")
  (column nil :read-only t :documentation "The screen column a mouse KEY happened at, or NIL.")
  (row nil :read-only t :documentation "The screen row a mouse KEY happened at, or NIL."))

(setf (documentation 'make-key 'function) "Return a KEY for BASE with the modifiers CTRL, META and SHIFT.")

(define-setting tuatara-user:*altgr-as-meta* t
  "Whether AltGr, the right Alt key of most European keyboards, works as
Meta with a letter. On such a keyboard AltGr picks a third sign of a key
\(AltGr-x types » on a Swedish layout), so AltGr-x is no command.
- t: AltGr held with a letter key is Meta with the letter the key types
  without AltGr, so AltGr-x is M-x and AltGr-Shift-x is M-X. AltGr with
  digits and other signs types as before, so @, $, {, [, ], }, \\ and |
  still come. AltGr-e is M-e, not €: type € by \\[insert-digraph] E u, or
  by \\[insert-char] and its name, euro sign. In a terminal this works where
  the terminal reports every key pressed and let go by the kitty keyboard
  protocol, AltGr among them, as kitty and foot do; elsewhere nothing
  changes. This is the standard.
- nil: AltGr types what the keyboard layout gives. Set it so where AltGr
  with a letter types letters of the language: Polish (ą ę ł ś ż), Czech
  and Slovak, Hungarian, Romanian (ă ș ț), Croatian and Slovenian,
  Lithuanian, and Neo or bone, whose third level is on the letter keys."
  :values boolean)

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
    ("<down-mouse-1>" . :down-mouse-1) ("<drag-mouse-1>" . :drag-mouse-1) ("<up-mouse-1>" . :up-mouse-1)
    ("<mouse-2>" . :mouse-2)
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

(define-record (event (:constructor make-event (kind &key key width height files column row text frame)) (:copier nil))
  "Something the frontend reports. KIND is :key (with KEY), :resize (with WIDTH
and HEIGHT in cells), :drop when files were dropped on the window (FILES,
their paths, on the cell COLUMN, ROW), or :paste when text was pasted into the
terminal Tuatara runs in (TEXT, its lines ending in newlines), to go in as
text and never as keys. It is :close when the window was asked to close,
:focus when it was given the keyboard, and :detached when the frame's screen
went away (its client left). It is :said with TEXT, words for the echo area
from the frontend itself, or :quit when the frontend can deliver nothing
more. :secret stands among recorded keys where a secret was typed, which is
never recorded. FRAME is the editor's frame the event happened in, where
frames share one source of events; NIL for the frame being worked in."
  (kind :key :type keyword :read-only t
        :documentation "The kind of EVENT: :key, :resize, :drop, :paste, :close, :focus, :detached, :said,
:quit, :secret among recorded keys, or :tick when a timed read found no key.")
  (key nil :type (or null key) :read-only t :documentation "The KEY of a :key EVENT.")
  (width 0 :type fixnum :read-only t :documentation "Width in cells of a :resize EVENT.")
  (height 0 :type fixnum :read-only t :documentation "Height in cells of a :resize EVENT.")
  (files '() :type list :read-only t :documentation "The paths of the files of a :drop EVENT.")
  (column nil :read-only t :documentation "The column of the cell a :drop EVENT happened on.")
  (row nil :read-only t :documentation "The row of the cell a :drop EVENT happened on.")
  (text nil :type (or null string) :read-only t :documentation "The text of a :paste EVENT.")
  (frame nil :read-only t :documentation "The frame EVENT happened in, or NIL for the frame being worked in."))

(setf (documentation 'make-event 'function) "Return an EVENT of KIND with the given KEY, WIDTH,
HEIGHT, FILES, COLUMN, ROW and TEXT.")

(defun dropped-name-p (name)
  "True when NAME, the path of a file dropped on a window, may be opened: a
string, not empty, with no control character in it (a NUL, a newline, an
escape), which no file name typed or shown would hold."
  (and (stringp name) (plusp (length name))
       (notany (lambda (c) (tuatara.text:control-code-p (char-code c))) name)))

(defun input-event-p (item)
  "True when ITEM, an event a frontend has queued, is something you did and
wait to see answered: a key or a mouse button, text pasted, files dropped, the
window asked to close; not a wake, nor a change of the window's size."
  (and (event-p item) (member (event-kind item) '(:key :paste :drop :close)) t))

;;; A queue of events, for frontends that deliver them from threads of their
;;; own; the frames that share a source of events share one

(define-record (event-queue (:constructor make-event-queue ()) (:copier nil))
  "Events on their way to the editor from the threads that read them, oldest
first, with how many wait of each frame."
  (items '() :documentation "The events in QUEUE, oldest first.")
  (tail '() :documentation "The last cons of QUEUE's items.")
  (counts (make-hash-table :test 'eq) :documentation "How many events wait in QUEUE, by the frame they name.")
  (lock (sb-thread:make-mutex :name "events") :documentation "The mutex guarding QUEUE.")
  (waiting (sb-thread:make-waitqueue :name "events") :documentation "Where the editor waits for QUEUE's next event.")
  (taken (sb-thread:make-waitqueue :name "events taken")
         :documentation "Where a thread waits for the editor to take events from QUEUE."))

(setf (documentation 'make-event-queue 'function) "An empty EVENT-QUEUE.")

(defun tick-p (event)
  "True when EVENT is a :TICK, a wake with nothing done by you."
  (and (event-p event) (eq (event-kind event) :tick)))

(defgeneric events-held (frame)
  (:documentation "How many events of FRAME the editor has taken from their queue and holds
still to be handled (keys typed in a frame while another asks a question). The default
holds none.")
  (:method (frame) (declare (ignore frame)) 0))

(defun queue-push (queue event &key most stop)
  "Add EVENT to QUEUE from any thread, waking the editor if it waits; a :TICK
after another waiting is one wake, not two. With MOST, first wait while
MOST events or more of EVENT's frame wait to be handled, in QUEUE or held
by the editor (EVENTS-HELD), until STOP, a function of no arguments asked
every half second, is true: a thread reading events from elsewhere so reads
no faster than the editor handles them."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (let ((counts (event-queue-counts queue)) (frame (and (event-p event) (event-frame event))))
      (when most
        (loop while (and (>= (+ (gethash frame counts 0) (if frame (events-held frame) 0)) most)
                         (not (funcall stop)))
              do (tuatara.runtime:wait-on (event-queue-taken queue) (event-queue-lock queue) 0.5)))
      (unless (and (tick-p event)
                   (event-queue-items queue)
                   (tick-p (first (event-queue-tail queue))))
        (let ((cell (list event)))
          (if (event-queue-items queue)
              (setf (cdr (event-queue-tail queue)) cell)
              (setf (event-queue-items queue) cell))
          (setf (event-queue-tail queue) cell)
          (incf (gethash frame counts 0)))))
    (sb-thread:condition-notify (event-queue-waiting queue))))

(defun queue-empty-p (queue)
  "True when QUEUE holds no events."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (null (event-queue-items queue))))

(defun queue-input-p (queue)
  "True when QUEUE holds something you did (INPUT-EVENT-P): the wakes of work
in the background, and a window's new size, are no input."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (and (find-if #'input-event-p (event-queue-items queue)) t)))

(defun queue-pop (queue seconds)
  "The oldest event in QUEUE, waiting up to SECONDS for one (NIL: as long as it
takes); NIL when none came."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (tuatara.runtime:wait-on-until (event-queue-waiting queue) (event-queue-lock queue)
                                   (lambda () (event-queue-items queue)) seconds)
    (let ((event (pop (event-queue-items queue))))
      (when event
        (let* ((counts (event-queue-counts queue)) (frame (and (event-p event) (event-frame event)))
               (left (1- (gethash frame counts 1))))
          (if (plusp left) (setf (gethash frame counts) left) (remhash frame counts)))
        (sb-thread:condition-broadcast (event-queue-taken queue)))
      event)))

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

(defgeneric frontend-another (frontend)
  (:documentation "A new frontend for another frame of the editor FRONTEND paints
for, showing it beside FRONTEND's (a window of its own), shown only once it
is started (FRONTEND-START), after it is given its frame (FRONTEND-FRAME): it
delivers its events into the source FRONTEND's come from, each naming that
frame. NIL, the default, when FRONTEND shows one frame at a time, as a
terminal does: the frames then take turns on FRONTEND itself.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-ask-another (frontend what windows)
  (:documentation "Ask FRONTEND's screen for another window of its own, a
screen that comes later, as a new connection, for another frame of the
editor FRONTEND paints for, its client showing WINDOWS windows now; WHAT is
kept with the request and given back when the window comes. T when it was
asked; words saying why when it was not, its client showing as many windows
as it may or having as many asked for still to come; NIL, the default, when
FRONTEND opens no window that way (FRONTEND-ANOTHER).")
  (:method (frontend what windows) (declare (ignore frontend what windows)) nil))

(defgeneric frontend-frame (frontend)
  (:documentation "The editor's frame FRONTEND paints, which it names in the events
it delivers (EVENT-FRAME) once the editor has several; NIL while it paints the
only one. The default keeps none.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric (setf frontend-frame) (frame frontend)
  (:documentation "Have FRONTEND name FRAME in the events it delivers (FRONTEND-FRAME).")
  (:method (frame frontend) (declare (ignore frontend)) frame))

(defgeneric frontend-raise (frontend)
  (:documentation "Bring FRONTEND's window before the others on the desktop, to be
typed in. The default does nothing: a terminal is where it is.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-scales-rows-p (frontend)
  (:documentation "True when FRONTEND draws a row of more than one row's height where
a face asks for it (a window of its own, not a terminal): the rows of its frames
are laid out so (TUATARA.DISPLAY:*ROW-SCALING*). The default answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-input-pending-p (frontend)
  (:documentation "True when input is waiting to be read, so that long work on the
editor thread, such as a search through a file of many gigabytes, can give way
to the next key. Must not block. The default answers NIL.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-colour-count (frontend)
  (:documentation "How many colours FRONTEND can show: 0 for none (a monochrome terminal),
8, 16 or 256 for terminals, 16777216 for a graphical user interface (GUI). The
defaults choose the colourless theme below 8.")
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

(defgeneric frontend-lend-terminal (frontend function &key words)
  (:documentation "Call FUNCTION with the terminal FRONTEND shows the editor
in given back to it as the terminal was before the editor took it, WORDS
shown on it first, so that a program FUNCTION runs (gpg-agent's pinentry
asking for a passphrase) can use it; then take it again and paint it whole.
Returns what FUNCTION returns. A frontend that is no terminal just calls
FUNCTION.")
  (:method (frontend function &key words) (declare (ignore frontend words)) (funcall function)))

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
row drawn otherwise, each (START END SCALE SHIFT HEIGHT). The part runs from
column START below END. The text from that column on is drawn SCALE times as
tall and wide (a rational of 1 or more) and SHIFT rows lower, over a band
HEIGHT rows tall (the row's gutter, drawn as it is, is as tall as the band of
its text), each of its characters taking SCALE columns. SCALE 0 leaves those
cells to the parts moved over them. OVER lists (LEFT TEXT SPANS) drawn over
everything at the row's own cells, the first on top: a list of candidates. A
frontend that draws all rows alike is given neither. PARTS lists the columns
where the row's parts begin after the first (a window's text after its gutter,
the window beside it), in order. A frontend that keeps what it painted may keep
each part apart, as the gutter's numbers stay on a row while the text scrolls
past them."))

(defgeneric frontend-set-cursor (frontend row column shape &optional colour)
  (:documentation "Place the cursor at ROW and COLUMN with SHAPE, :block, :bar or
:underline, in COLOUR, as faces take colours (see TUATARA.DISPLAY:COLOUR-HEX),
or the text's when NIL; SHAPE :hidden shows no cursor (over a picture)."))

(defgeneric frontend-end-frame (frontend)
  (:documentation "Finish the frame and show it."))

;;; Pictures

(define-record (image (:constructor %make-image (width height channels pixels id)) (:copier nil))
  "A picture to show: WIDTH by HEIGHT pixels, each CHANNELS octets (3, red,
green and blue; 4, with alpha), row after row from the top left, in PIXELS;
ID, a number no other picture of this session has, by which a frontend tells
pictures apart."
  (width 0 :type fixnum :read-only t :documentation "The picture's width in pixels.")
  (height 0 :type fixnum :read-only t :documentation "The picture's height in pixels.")
  (channels 4 :type fixnum :read-only t :documentation "The octets of each pixel of the picture: 3 (RGB) or 4 (RGBA).")
  (pixels nil :read-only t :documentation "The picture's pixels, row after row from the top left.")
  (id 0 :type fixnum :read-only t
      :documentation "The number that tells the picture apart from every other of the session."))

(defun image-fit-scale (image width height &optional (most 1))
  "The scale IMAGE is drawn at to fit in WIDTH by HEIGHT pixels, its
proportions kept: as large as fits, never more than MOST (1, its own size,
unless given)."
  (min most (/ width (max 1 (image-width image))) (/ height (max 1 (image-height image)))))

(sb-ext:defglobal **image-count** (list 0)
  "How many pictures have been made this session: the last one's ID.")

(defun make-image (width height channels pixels)
  "A picture of WIDTH by HEIGHT pixels of CHANNELS octets each (3 or 4), from
PIXELS, row after row from the top left, with an ID of its own. Refused when
PIXELS holds fewer octets than that, so that no frontend reads past them."
  (unless (and (typep width '(integer 0)) (typep height '(integer 0)) (typep channels '(integer 1))
               (vectorp pixels) (>= (length pixels) (* width height channels)))
    (tuatara.runtime:refuse "A picture of ~a by ~a pixels came with too few octets for them: not shown" width height))
  (%make-image width height channels pixels (sb-ext:atomic-incf (car **image-count**))))

(defgeneric frontend-focused-p (frontend)
  (:documentation "True while FRONTEND's window has the keyboard, so that what is copied
or read from the clipboard there is yours to give. The default answers T: a
terminal does not say when it is left.")
  (:method (frontend) (declare (ignore frontend)) t))

(defgeneric frontend-images-p (frontend)
  (:documentation "True when FRONTEND can show pictures (FRONTEND-PUT-IMAGE). The
default answers NIL: a window showing a picture then shows its buffer's text.")
  (:method (frontend) (declare (ignore frontend)) nil))

(defgeneric frontend-reorders-text-p (frontend)
  (:documentation "True when FRONTEND turns right-to-left text round itself, whatever it
is asked (a terminal that does and cannot be told not to): the text is then
best sent in the order it is stored. The default answers NIL.")
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
