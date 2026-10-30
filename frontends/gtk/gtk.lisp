;;;; frontends/gtk/gtk.lisp --- Tuatara in a window of its own: GTK4, painted with Pango and Cairo.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; GTK runs on the main thread and the editor on a thread of its own. GTK's
;;;; callbacks put keys, resizes and the close button in a queue the editor
;;;; reads; each frame the editor finishes is handed to the main thread, which
;;;; paints it on one drawing area: the rows as the editor laid them out, in the
;;;; faces' colours, on a grid of the font's cells.
;;;;
;;;; This file holds the frontend and what passes between the threads;
;;;; input.lisp the keys and the mouse, paint.lisp the painting, window.lisp
;;;; the window itself.

(defpackage #:tuatara.frontend.gtk
  (:use #:cl #:tuatara.gtk)
  (:import-from #:tuatara.io #:null-pointer #:null-pointer-p #:sap-string)
  (:import-from #:tuatara-user #:*gui-font* #:*gui-renderer* #:*gui-padding* #:*window-colours-from-desktop*)
  (:documentation "The GTK4 frontend: Tuatara in a window of its own, one drawing area painted with Pango and Cairo.")
  (:export #:gtk-frontend #:make-gtk-frontend #:run-in-window #:gdk-key #:paint-frame-to-png
           #:call-with-painted-frame #:pixel-colour #:queue-push #:queue-pop #:make-event-queue
           #:make-frame #:frame-rows #:colour-rgb))

(in-package #:tuatara.frontend.gtk)

(defvar *gui-font* nil
  "The font of Tuatara's window, as Pango reads it, \"Adwaita Mono 12\" for one;
NIL for the desktop's monospace font. Set it in the init file.")

(defvar *gui-padding* 8
  "The room, in pixels, between the edges of Tuatara's window and its text, in
the ground's colour: one number for all four sides, or a list: of two, the
top and bottom, then the right and left ((4 12) is 4 above and below, 12 at
the sides); of three, the top, the right and left, the bottom; of four, the
top, right, bottom and left, clockwise. Set it in the init file, or try values
with \\[eval-expression] (setf *gui-padding* 12), which the window follows at once.")

(defvar *window-colours-from-desktop* nil
  "True to paint text and ground the theme leaves without a colour (the
terminal and plain themes in a window) in the colours of the desktop's GTK
theme, following its light or dark style as it changes; NIL, as standard, for
black text on a white ground, as a window with no theme shows it. Themes that
give their own colours (ao-dark, ao-light) are not changed. Set it in the init
file.")

(defvar *gui-renderer* :gl
  "How Tuatara's window is drawn: :gl, by the graphics card through OpenGL, as
standard; :vulkan, by the graphics card through Vulkan; :cairo, by the
processor alone; NIL, as GTK chooses. Which is faster depends on the machine:
\\[check-health], in the window, measures both and says which to use. Set it in the
init file: when it asks for another way than the window opened with, Tuatara
starts again at once, drawn that way; set later, from the next start.
GSK_RENDERER in the environment, GTK's own name for it (gl, vulkan, cairo),
goes before it.")

(defvar *window-renderer* nil
  "The way the window was opened to be drawn, as GSK_RENDERER names it (gl,
vulkan, cairo), NIL for GTK's own choice, :ENVIRONMENT when GSK_RENDERER was
set before Tuatara started.")

(defun renderer-name (renderer)
  "GTK's name (GSK_RENDERER) for RENDERER, a value of *GUI-RENDERER*; NIL for GTK's own choice."
  (case renderer (:gl "gl") (:vulkan "vulkan") (:cairo "cairo") (t nil)))

(defun window-renderer-name (window)
  "The renderer drawing WINDOW, realised, as GTK names its type (GskGLRenderer,
GskVulkanRenderer, GskCairoRenderer), or NIL when it has none."
  (let ((renderer (gtk-native-get-renderer window)))
    (unless (null-pointer-p renderer) (g-type-name-from-instance renderer))))

(defvar *desktop-colours* nil
  "The desktop's text and ground colours as GTK's theme last gave them, a list
of two lists of three numbers from 0 to 1, or NIL; kept by GTK's thread.")

;;; Events from GTK to the editor

(defstruct (event-queue (:constructor make-event-queue ()) (:copier nil))
  "Events on their way from GTK's thread to the editor's, oldest first."
  (items '()) (tail '())
  (lock (sb-thread:make-mutex :name "events"))
  (waiting (sb-thread:make-waitqueue :name "events")))

(setf (documentation 'make-event-queue 'function) "An empty EVENT-QUEUE."
      (documentation 'event-queue-p 'function) "True when OBJECT is an EVENT-QUEUE."
      (documentation 'event-queue-items 'function) "The events in QUEUE, oldest first."
      (documentation 'event-queue-tail 'function) "The last cons of QUEUE's items."
      (documentation 'event-queue-lock 'function) "The mutex guarding QUEUE."
      (documentation 'event-queue-waiting 'function) "Where the editor waits for QUEUE's next event.")

(defun tick-p (event)
  "True when EVENT is a :TICK, a wake with nothing done by you."
  (and (typep event 'tuatara.frontend:event) (eq (tuatara.frontend:event-kind event) :tick)))

(defun queue-push (queue event)
  "Add EVENT to QUEUE, waking the editor if it waits; a :TICK after another
waiting is one wake, not two."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (unless (and (tick-p event)
                 (event-queue-items queue)
                 (tick-p (first (event-queue-tail queue))))
      (let ((cell (list event)))
        (if (event-queue-items queue)
            (setf (cdr (event-queue-tail queue)) cell)
            (setf (event-queue-items queue) cell))
        (setf (event-queue-tail queue) cell)))
    (sb-thread:condition-notify (event-queue-waiting queue))))

(defun queue-empty-p (queue)
  "True when QUEUE holds no events."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (null (event-queue-items queue))))

(defun queue-pop (queue seconds)
  "The oldest event in QUEUE, waiting up to SECONDS for one (NIL: as long as it
takes); NIL when none came."
  (sb-thread:with-mutex ((event-queue-lock queue))
    (tuatara.runtime:wait-on-until (event-queue-waiting queue) (event-queue-lock queue)
                                   (lambda () (event-queue-items queue)) seconds)
    (pop (event-queue-items queue))))

;;; Frames from the editor to GTK

(defstruct (frame (:constructor make-frame (rows)) (:copier nil))
  "What the window shows: ROWS, a vector with each row's (STRING FACE SPANS
SCALES OVER PARTS) or NIL, the cursor at CURSOR-ROW and CURSOR-COLUMN, CURSOR-SHAPE :block, :bar
or :underline, in CURSOR-COLOUR (as faces take colours; NIL for the text's),
and IMAGES, the pictures over the rows, each (IMAGE ROW COLUMN COLUMNS ROWS
OFFSET), as TUATARA.FRONTEND:FRONTEND-PUT-IMAGE takes them."
  (rows #() :type simple-vector)
  (cursor-row 0) (cursor-column 0) (cursor-shape :block) (cursor-colour nil)
  (images '()))

(setf (documentation 'make-frame 'function) "A FRAME showing ROWS, its cursor at the top left."
      (documentation 'frame-p 'function) "True when OBJECT is a FRAME."
      (documentation 'frame-rows 'function) "The rows of FRAME, each (STRING FACE SPANS SCALES OVER PARTS) or NIL."
      (documentation 'frame-cursor-row 'function) "The row FRAME's cursor is on."
      (documentation 'frame-cursor-column 'function) "The column FRAME's cursor is at."
      (documentation 'frame-cursor-shape 'function) "The shape of FRAME's cursor, :block, :bar or :underline."
      (documentation 'frame-cursor-colour 'function) "The colour of FRAME's cursor, as faces take colours, or NIL for the text's."
      (documentation 'frame-images 'function) "The pictures over FRAME's rows, each (IMAGE ROW COLUMN COLUMNS ROWS OFFSET).")

;;; The frontend

(defclass gtk-frontend ()
  ((window :initform nil :accessor gtk-window :documentation "The GTK window.")
   (area :initform nil :accessor gtk-area :documentation "The drawing area the window holds.")
   (im :initform nil :accessor gtk-im :documentation "The input method context keys pass through.")
   (fonts :initform nil :accessor gtk-fonts :documentation "The font descriptions: upright, bold, italic, bold italic.")
   (cell-width :initform 8 :accessor cell-width :documentation "The width of a cell in pixels.")
   (cell-height :initform 16 :accessor cell-height :documentation "The height of a cell in pixels.")
   (baseline :initform 12 :accessor cell-baseline :documentation "How far below a cell's top its text's baseline is, in pixels.")
   (columns :initform 80 :accessor gtk-columns :documentation "The width of the window in cells.")
   (rows :initform 24 :accessor gtk-rows :documentation "The height of the window in cells.")
   (lock :initform (sb-thread:make-mutex :name "window") :reader gtk-lock :documentation "Guards what both threads read.")
   (changed :initform (sb-thread:make-waitqueue :name "window") :reader gtk-changed :documentation "Where the editor waits for the window's size or the clipboard.")
   (sized :initform nil :accessor gtk-sized :documentation "True once the window knows its size.")
   (queue :initform (make-event-queue) :reader gtk-queue :documentation "Events for the editor.")
   (frame :initform nil :accessor gtk-frame :documentation "The frame the editor is painting.")
   (shown :initform nil :accessor gtk-shown :documentation "The frame the window shows.")
   (draw-pending :initform nil :accessor gtk-draw-pending :documentation "True while a redraw is asked for and not yet made.")
   (done :initform nil :accessor gtk-done :documentation "True once the editor has ended.")
   (focused :initform t :accessor gtk-focused :documentation "True while the window has the keyboard focus.")
   (clipboard-out :initform nil :accessor gtk-clipboard-out :documentation "Text on its way to the clipboard.")
   (primary-out :initform nil :accessor gtk-primary-out :documentation "Text on its way to the primary selection.")
   (clipboard-in :initform nil :accessor gtk-clipboard-in :documentation "The clipboard's text as last read, or :waiting.")
   (clipboard-kept :initform nil :accessor gtk-clipboard-kept
                   :documentation "The clipboard's text, read in the background each time it changes; NIL until it is watched.")
   (clipboard-reads :initform nil :accessor gtk-clipboard-reads
                    :documentation "How many reads of the changed clipboard are under way, or NIL while it is not watched.")
   (scroll :initform 0d0 :accessor gtk-scroll :documentation "Wheel movement not yet turned into whole turns.")
   (pointer :initform nil :accessor gtk-pointer :documentation "Where the pointer last was over the window, (X . Y) in pixels, or NIL.")
   (drag :initform nil :accessor gtk-drag :documentation "Where the first mouse button went down, (X . Y) in pixels, while it is held.")
   (drag-cell :initform nil :accessor gtk-drag-cell :documentation "The cell the editor was last told a drag reached, (COLUMN . ROW).")
   (drag-repeating :initform nil :accessor gtk-drag-repeating :documentation "True while a drag held past the text is told again and again, to scroll.")
   (title :initform nil :accessor gtk-title :documentation "The title the editor last gave the window.")
   (preedit :initform nil :accessor gtk-preedit :documentation "What the input method is composing, shown at the cursor until it is typed, or NIL.")
   (font-steps :initform 0 :accessor gtk-font-steps :documentation "The steps the font is scaled by from its size, each 1.2 times the last.")
   (pointer-shape :initform "text" :accessor gtk-pointer-shape :documentation "The name of the pointer's shape over the window now.")
   (toggles :initform '() :accessor gtk-toggles :documentation "The window states the editor asked to turn to or from, not yet done, oldest first.")
   (face-fonts :initform (make-hash-table :test 'equal) :reader gtk-face-fonts :documentation "The font descriptions of faces that name a font of their own (FACE-FONT), by name, at the window's size.")
   (layouts :initform nil :accessor gtk-layouts :documentation "The layouts painting keeps from one frame to the next (MAKE-LAYOUTS), or NIL; let go of when the fonts change.")
   (room-height :initform nil :accessor gtk-room-height :documentation "The height in pixels the grid of cells has inside the padding, as last fitted, or NIL before then.")
   (padding :initform '(0 0 0 0) :accessor gtk-padding :documentation "The room in pixels round the grid of cells, as the grid was last fitted: top, right, bottom and left.")
   (font-in-use :initform nil :accessor gtk-font-in-use :documentation "The name of the font the window's fonts were last made from.")
   (renderer-checked :initform nil :accessor gtk-renderer-checked :documentation "True once *GUI-RENDERER* has been looked at as the editor started (FOLLOW-RENDERER-SETTING).")
   (renderer-said :initform nil :accessor gtk-renderer-said :documentation "The way of drawing last said to be taken from the next start.")
   (measured :initform nil :accessor gtk-measured :documentation "While the drawing is measured (MEASURE-DRAWING): when each key went in, each frame was finished and each was painted, three vectors of microseconds (MEASURE-CLOCK).")
   (drawn-by :initform nil :accessor gtk-drawn-by :documentation "The renderer drawing the window, as GTK names its type (GskGLRenderer), once it is realised.")
   (settings-pending :initform nil :accessor gtk-settings-pending :documentation "True while the window is yet to take in changed settings.")
   (desktop-style :initform nil :accessor gtk-desktop-style :documentation "The desktop's style as GTK last told it, :dark or :light, or NIL.")
   (display-scale :initform 1 :accessor gtk-display-scale :documentation "How many of the display's own pixels each of the window's pixels takes across, as the window last painted: 2 on a screen scaled to 200 per cent.")
   (kept-rows :initform nil :accessor gtk-kept-rows :documentation "The rows as last painted, kept from one frame to the next (KEEP-ROWS), or NIL.")
   (grid :initform nil :accessor gtk-grid :documentation "The widget over the drawing area that holds the pictures of the rows' parts, drawn by the graphics card (PLACE-PIECES).")
   (pictures-layer :initform nil :accessor gtk-pictures-layer :documentation "The widget over the grid, under the cursor, that holds the frame's pictures (PLACE-IMAGES).")
   (shown-images :initform (make-hash-table :test 'eq) :reader gtk-shown-images :documentation "For each image shown on the pictures layer, by the IMAGE: (TEXTURE COUNT (PICTURE PLACE)...), one picture for each place it is shown, PLACE how that picture was last placed, COUNT how many are in use.")
   (above :initform nil :accessor gtk-above :documentation "The widget over the grid that holds the cursor's own drawing area.")
   (cursor-area :initform nil :accessor gtk-cursor-area :documentation "The drawing area the cursor and what the input method composes are painted in, as large as they are.")
   (cursor-at :initform '(0 . 0) :accessor gtk-cursor-at :documentation "Where the cursor's drawing area stands on the grid, (X . Y) in pixels.")
   (ground :initform nil :accessor gtk-ground :documentation "The picture of the window's ground colour, stretched across it, under the rows.")
   (ground-colour :initform nil :accessor gtk-ground-colour :documentation "The colour GTK-GROUND shows, three numbers from 0 to 1, or NIL before it shows one.")
   (pictures :initform (make-array 0 :adjustable t :fill-pointer 0) :reader gtk-pictures :documentation "The pictures on the grid, each (PICTURE X Y TEXTURE WIDTH HEIGHT) as it was last placed (PLACE-PICTURE).")
   (on-card :initform nil :accessor gtk-on-card :documentation "True while the rows are shown as pictures the graphics card draws, NIL while the drawing area paints the whole frame.")
   (surfaces :initform (make-hash-table :test 'eq) :reader gtk-surfaces :documentation "The cairo surfaces of the pictures the window shows, by their IMAGE; used and let go of on the window's own thread."))
  (:documentation "Tuatara's window: a GTK4 window with one drawing area."))

(defun make-gtk-frontend ()
  "A frontend for a window of Tuatara's own."
  (make-instance 'gtk-frontend))

(defvar *frontend* nil
  "The frontend of the window open, for GTK's callbacks.")

(defun callback-failed (name condition)
  "Report on the error output that the window's callback NAME failed with CONDITION."
  (ignore-errors
   (format *error-output* "~&Tuatara: the window's ~(~a~) failed: ~a~%" name condition)
   (finish-output *error-output*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-ignored (declarations)
    "DECLARATIONS, DECLARE forms, without what they say of variables not used."
    (loop for (nil . specifiers) in declarations
          for kept = (remove-if (lambda (s) (member (first s) '(ignore ignorable))) specifiers)
          when kept collect `(declare ,@kept))))

(defmacro define-callback (name-and-options return-type (&rest arguments) &body body)
  "Define a function for GTK to call, whose C function pointer (CALLBACK NAME)
is given to GTK. NAME-AND-OPTIONS is its name, or a list of its name and
:FAILED FORM. RETURN-TYPE and each of ARGUMENTS, (ARGUMENT TYPE), are of the
types TUATARA.IO:C-TYPE knows: a :string argument is read as text, a :boolean
result given as GLib's truth value. An error in BODY is reported on the error
output and answered with the value of FORM (NIL by default) rather than
taking the editor down with GTK's thread, never unwinding through GTK."
  (destructuring-bind (name &key failed) (if (listp name-and-options) name-and-options (list name-and-options))
    (multiple-value-bind (forms declarations) (alexandria:parse-body body)
      (let ((names (mapcar #'first arguments))
            (texts (loop for (argument type) in arguments when (eq type :string) collect argument))
            (result (gensym "RESULT")))
        `(sb-alien:define-alien-callable ,name ,(tuatara.io:c-type (if (eq return-type :boolean) :int return-type))
             ,(loop for (argument type) in arguments
                    collect (list argument (tuatara.io:c-type (if (eq type :string) :pointer type))))
           ;; each argument named, used or not: a callable's body takes no declarations
           ,@names
           (let ((,result (handler-case
                              (let (,@(loop for text in texts collect `(,text (sap-string ,text))))
                                (locally ,@(remove-ignored declarations) ,@forms))
                            (serious-condition (condition)
                              (callback-failed ',name condition)
                              ,failed))))
             ,(if (eq return-type :boolean) `(if ,result 1 0) result)))))))

(defun request-draw (f)
  "Have the window painted again soon, from GTK's thread; any thread may ask."
  (unless (sb-thread:with-mutex ((gtk-lock f)) (shiftf (gtk-draw-pending f) t))
    (g-idle-add (callback on-draw-request) (null-pointer))))

(defmethod tuatara.frontend:frontend-start ((f gtk-frontend))
  ;; the window was opened on GTK's thread; wait for it to know its size
  (sb-thread:with-mutex ((gtk-lock f))
    (tuatara.runtime:wait-on-until (gtk-changed f) (gtk-lock f) (lambda () (gtk-sized f)) nil))
  ;; rows drawn large where a face asks for it (:height)
  (setf tuatara.display:*row-scaling* t))

(defmethod tuatara.frontend:frontend-stop ((f gtk-frontend))
  (setf tuatara.display:*row-scaling* nil
        (gtk-done f) t)
  (g-idle-add (callback on-close-window) (null-pointer)))

(defmethod tuatara.frontend:frontend-size ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f)) (values (gtk-columns f) (gtk-rows f))))

(defmethod tuatara.frontend:frontend-read-event ((f gtk-frontend))
  (queue-pop (gtk-queue f) nil))

(defmethod tuatara.frontend:frontend-read-event-timeout ((f gtk-frontend) seconds)
  (or (queue-pop (gtk-queue f) seconds) (tuatara.frontend:make-event :tick)))

(defmethod tuatara.frontend:frontend-wake ((f gtk-frontend))
  (queue-push (gtk-queue f) (tuatara.frontend:make-event :tick)))

(defmethod tuatara.frontend:frontend-input-pending-p ((f gtk-frontend))
  ;; what you did waits; the wakes of work in the background are no input
  (let ((queue (gtk-queue f)))
    (sb-thread:with-mutex ((event-queue-lock queue))
      (and (find-if-not #'tick-p (event-queue-items queue)) t))))

(defmethod tuatara.frontend:frontend-description ((f gtk-frontend))
  (format nil "in a GTK ~d.~d window"
          (tuatara.runtime:foreign-call "gtk_get_major_version" sb-alien:unsigned-int)
          (tuatara.runtime:foreign-call "gtk_get_minor_version" sb-alien:unsigned-int)))

(defmethod tuatara.frontend:frontend-health ((f gtk-frontend))
  (list (cons "Display" "a window of its own (GTK 4), 24-bit colour")
        (cons "Drawing" (format nil "~a~:[~;, as GSK_RENDERER in the environment says~]"
                                (or (gtk-drawn-by f) "not yet") (eq *window-renderer* :environment)))
        (cons "Clipboard" "the desktop's, through GTK")))

(defmethod tuatara.frontend:frontend-set-clipboard ((f gtk-frontend) text)
  (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-clipboard-out f) text))
  (g-idle-add (callback on-set-clipboard) (null-pointer))
  t)

(defmethod tuatara.frontend:frontend-set-primary-selection ((f gtk-frontend) text)
  (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-primary-out f) text))
  (g-idle-add (callback on-set-clipboard) (null-pointer))
  t)

(defmethod tuatara.frontend:frontend-font-steps ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f)) (gtk-font-steps f)))

(defmethod (setf tuatara.frontend:frontend-font-steps) (steps (f gtk-frontend))
  (let ((steps (sb-thread:with-mutex ((gtk-lock f))
                 (setf (gtk-font-steps f) (font-steps-fitting f steps)))))
    (g-idle-add (callback on-rescale-font) (null-pointer))
    steps))

(defun call-with-font-families (function)
  "Call FUNCTION with each font family installed, a Pango font family."
  (load-gtk-libraries)
  (with-gtk-float-traps
    (sb-alien:with-alien ((families sb-sys:system-area-pointer (null-pointer)) (found sb-alien:int 0))
      (pango-font-map-list-families (pango-cairo-font-map-get-default)
                                    (sb-alien:alien-sap (sb-alien:addr families)) (sb-alien:alien-sap (sb-alien:addr found)))
      (unwind-protect
           (dotimes (i found)
             (funcall function (sb-sys:sap-ref-sap families (* sb-vm:n-word-bytes i))))
        (g-free families)))))

(defun font-families (&key monospace)
  "The names of the font families installed, sorted; with MONOSPACE, only those
whose characters all have one width."
  (let ((names '()))
    (call-with-font-families
     (lambda (family)
       (when (or (not monospace) (pango-font-family-is-monospace family))
         (push (pango-font-family-get-name family) names))))
    (sort names #'string-lessp)))

(defmethod tuatara.frontend:frontend-fonts ((f gtk-frontend))
  (font-families :monospace t))

(defun font-description-size (description)
  "The size of the Pango font DESCRIPTION, in Pango units; 11 points when it
names none."
  (let ((size (pango-font-description-get-size description)))
    (if (zerop size) (* 11 +pango-scale+) size)))

(defun font-size-in-use ()
  "The size of the window's font as it started, in Pango units: *GUI-FONT*'s, or
the desktop's monospace font's; 11 points when neither names one."
  (let ((description (pango-font-description-from-string (font-name))))
    (unwind-protect (font-description-size description)
      (pango-font-description-free description))))

(defun family-variants (family size)
  "The variants of the Pango font FAMILY drawn as themselves, as (NAME FONT)
with FONT the full name that chooses each at SIZE (Pango units), sorted by name."
  (sb-alien:with-alien ((faces sb-sys:system-area-pointer (null-pointer)) (found sb-alien:int 0))
    (pango-font-family-list-faces family (sb-alien:alien-sap (sb-alien:addr faces)) (sb-alien:alien-sap (sb-alien:addr found)))
    (let ((array faces))
      (unwind-protect
           (sort (loop for i below found
                       for face = (sb-sys:sap-ref-sap array (* sb-vm:n-word-bytes i))
                       unless (pango-font-face-is-synthesized face)
                         collect (let ((description (pango-font-face-describe face)))
                                   (unwind-protect
                                        (progn (pango-font-description-set-size description size)
                                               (list (pango-font-face-get-face-name face)
                                                     (pango-font-description-to-string description)))
                                     (pango-font-description-free description))))
                 #'string-lessp :key #'first)
        (g-free array)))))

(defmethod tuatara.frontend:frontend-font-faces ((f gtk-frontend))
  (let ((size (with-gtk-float-traps (load-gtk-libraries) (font-size-in-use))) (all '()))
    (call-with-font-families
     (lambda (family)
       (let ((variants (family-variants family size)))
         (when variants
           (push (list (pango-font-family-get-name family) (pango-font-family-is-monospace family) variants)
                 all)))))
    (sort all (lambda (a b)
                (if (eq (second a) (second b))
                    (string-lessp (first a) (first b))
                    (second a))))))

(defmethod tuatara.frontend:frontend-set-font ((f gtk-frontend) name)
  (with-gtk-float-traps
    (let* ((description (pango-font-description-from-string name))
           (family (pango-font-description-get-family description)))
      (unwind-protect
           (when (and family (member family (font-families) :test #'string-equal))
             (when (zerop (pango-font-description-get-size description))
               (pango-font-description-set-size description (font-size-in-use)))
             (let ((full (pango-font-description-to-string description)))
               (setf *gui-font* full)
               (g-idle-add (callback on-rescale-font) (null-pointer))
               full))
        (pango-font-description-free description)))))

(define-callback on-toggle-state :boolean ((data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*)
         (states (and f (sb-thread:with-mutex ((gtk-lock f)) (shiftf (gtk-toggles f) '()))))
         (window (and f (gtk-window f))))
    (when window
      (dolist (state states)
        (ecase state
          (:fullscreen (if (gtk-window-is-fullscreen window)
                           (gtk-window-unfullscreen window)
                           (gtk-window-fullscreen window)))
          (:maximised (if (gtk-window-is-maximized window)
                          (gtk-window-unmaximize window)
                          (gtk-window-maximize window)))))))
  nil)

(defmethod tuatara.frontend:frontend-toggle-state ((f gtk-frontend) state)
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-toggles f) (append (gtk-toggles f) (list state))))
  (g-idle-add (callback on-toggle-state) (null-pointer))
  t)

(defun settings-desktop-style ()
  "The desktop's style as GTK's settings have it (GTK 4.20 on), :dark or
:light; NIL when GTK cannot tell. On GTK's thread."
  (let ((settings (gtk-settings-get-default)))
    (when (and (not (null-pointer-p settings))
               (not (null-pointer-p
                     ;; the settings' class, which the object's first field points at
                     (g-object-class-find-property (sb-sys:sap-ref-sap settings 0) "gtk-interface-color-scheme"))))
      ;; 1: no preference, which desktops show light; 2: dark; 3: light
      (case (g-object-get-int settings "gtk-interface-color-scheme")
        (2 :dark)
        ((1 3) :light)
        (t nil)))))

(defun read-desktop-colours (widget)
  "Keep the desktop's text and ground colours for WIDGET in *DESKTOP-COLOURS*:
those the desktop's theme names for windows, else GTK's own. On GTK's thread."
  (let ((fg (widget-theme-colour widget "window_fg_color" "theme_fg_color"))
        (bg (widget-theme-colour widget "window_bg_color" "theme_bg_color")))
    (setf *desktop-colours* (and fg bg (list fg bg)))))

(define-callback on-style-changed :void ((settings :pointer) (spec :pointer) (data :pointer))
  (declare (ignore settings spec data))
  (let ((f *frontend*))
    (when f
      (let ((style (settings-desktop-style)))
        (sb-thread:with-mutex ((gtk-lock f)) (setf (gtk-desktop-style f) style))
        (when (gtk-area f)
          (read-desktop-colours (gtk-area f))
          (redraw f))
        (queue-push (gtk-queue f) (tuatara.frontend:make-event :tick))))))

(defmethod tuatara.frontend:frontend-settings-changed ((f gtk-frontend))
  (follow-renderer-setting f)
  ;; the font or the padding asked for not the ones in use: made again on GTK's thread
  (let ((font (font-name))
        (padding (multiple-value-list (tuatara.frontend:padding-sides *gui-padding*))))
    (when (sb-thread:with-mutex ((gtk-lock f))
            (and (not (gtk-settings-pending f))
                 (not (and (equal font (gtk-font-in-use f)) (equal padding (gtk-padding f))))
                 (setf (gtk-settings-pending f) t)))
      (g-idle-add (callback on-rescale-font) (null-pointer)))))

(defmethod tuatara.frontend:frontend-desktop-style ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f)) (gtk-desktop-style f)))

(defmethod tuatara.frontend:frontend-set-title ((f gtk-frontend) title)
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-title f) title))
  (g-idle-add (callback on-set-title) (null-pointer)))

(defmethod tuatara.frontend:frontend-clipboard ((f gtk-frontend))
  (read-clipboard-now f nil))

(defmethod tuatara.frontend:frontend-primary-selection ((f gtk-frontend))
  (read-clipboard-now f t))

(defun read-clipboard-now (f primary)
  "The text of F's clipboard, or with PRIMARY its primary selection: as kept
when it is watched (the clipboard), once a change being read is in, or, a
second gone, as it was before it; else read on GTK's thread, a second at
most; NIL when there is none."
  (sb-thread:with-mutex ((gtk-lock f))
    ;; watched: the text kept, once a change being read is in (a second at most)
    (when (and (not primary) (gtk-clipboard-reads f))
      (tuatara.runtime:wait-on-until (gtk-changed f) (gtk-lock f) (lambda () (zerop (gtk-clipboard-reads f))) 1)
      ;; or, the change still not in, the text kept before it
      (return-from read-clipboard-now (gtk-clipboard-kept f)))
    (setf (gtk-clipboard-in f) :waiting)
    (g-idle-add (if primary (callback on-read-primary) (callback on-read-clipboard)) (null-pointer))
    ;; a clipboard that does not answer within a second counts as none
    (tuatara.runtime:wait-on-until (gtk-changed f) (gtk-lock f) (lambda () (not (eq (gtk-clipboard-in f) :waiting))) 1)
    (let ((text (gtk-clipboard-in f)))
      (setf (gtk-clipboard-in f) nil)
      (and (stringp text) text))))

(defmethod tuatara.frontend:frontend-begin-frame ((f gtk-frontend))
  (setf (gtk-frame f) (make-frame (make-array (sb-thread:with-mutex ((gtk-lock f)) (gtk-rows f)) :initial-element nil))))

(defmethod tuatara.frontend:frontend-put-row ((f gtk-frontend) row string &key face spans scales over parts)
  (let ((rows (frame-rows (gtk-frame f))))
    (when (< -1 row (length rows))
      (setf (aref rows row) (list (copy-seq string) face (copy-list spans) (copy-list scales) (copy-tree over) (copy-list parts))))))

(defmethod tuatara.frontend:frontend-images-p ((f gtk-frontend))
  t)

(defmethod tuatara.frontend:frontend-cell-pixels ((f gtk-frontend))
  ;; in the display's own pixels: a picture made that large is drawn sharp on a scaled screen
  (sb-thread:with-mutex ((gtk-lock f))
    (let ((scale (gtk-display-scale f)))
      (values (* scale (cell-width f)) (* scale (cell-height f))))))

(defmethod tuatara.frontend:frontend-put-image ((f gtk-frontend) image row column columns rows &optional offset)
  (push (list image row column columns rows offset) (frame-images (gtk-frame f))))

(defmethod tuatara.frontend:frontend-set-cursor ((f gtk-frontend) row column shape &optional colour)
  (let ((frame (gtk-frame f)))
    (setf (frame-cursor-row frame) row (frame-cursor-column frame) column
          (frame-cursor-shape frame) shape (frame-cursor-colour frame) colour)))

(defmethod tuatara.frontend:frontend-end-frame ((f gtk-frontend))
  (sb-thread:with-mutex ((gtk-lock f))
    (setf (gtk-shown f) (gtk-frame f))
    (when (gtk-measured f) (vector-push-extend (measure-clock) (aref (gtk-measured f) 1))))
  (request-draw f))
