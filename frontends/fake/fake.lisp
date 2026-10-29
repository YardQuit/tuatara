;;;; frontends/fake/fake.lisp --- An in-memory frontend for tests.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.frontend.fake
  (:use #:cl)
  (:documentation "A frontend that takes its events from a queue and paints into a vector of strings.")
  (:export #:fake-frontend #:make-fake-frontend #:fake-queue-keys #:fake-queue-event #:fake-screen #:fake-screen-line
           #:fake-cursor #:fake-cursor-shape #:fake-cursor-colour #:fake-frames #:fake-history #:fake-frame-showing #:fake-frame-showing-across-rows #:fake-spans
           #:fake-row-faces #:fake-row-scales #:fake-face-history #:fake-frame-with #:fake-pending #:fake-clipboard #:fake-primary #:fake-titles
           #:fake-font-steps #:fake-fonts #:fake-font #:fake-toggles #:fake-font-faces #:fake-desktop-style
           #:fake-queue-action #:fake-images #:fake-image-history))

(in-package #:tuatara.frontend.fake)

(defclass fake-frontend ()
  ((width :initarg :width :initform 40 :accessor fake-width
          :documentation "Columns of the fake screen.")
   (height :initarg :height :initform 8 :accessor fake-height
           :documentation "Rows of the fake screen.")
   (colours :initarg :colours :initform 256 :accessor fake-colours
            :documentation "How many colours the fake screen claims to show; 0 for a monochrome terminal.")
   (events :initform '() :accessor fake-events
           :documentation "Queued events, delivered in order; then :quit.")
   (pending :initform nil :accessor fake-pending
            :documentation "What FRONTEND-INPUT-PENDING-P answers: NIL, as though every key
arrived after the work before it finished; true, as though keys were typed
during it; or a function of the frontend asked each time.")
   (screen :initform nil :accessor fake-screen
           :documentation "The last painted frame as a vector of strings, one per row.")
   (spans :initform nil :accessor fake-spans
          :documentation "The face spans of each row of the last frame, a vector of lists of (START END FACE).")
   (row-faces :initform nil :accessor fake-row-faces
              :documentation "The whole-row face of each row of the last frame, or NIL.")
   (row-scales :initform nil :accessor fake-row-scales
               :documentation "The parts of each row of the last frame drawn large or moved down, lists of (START END SCALE SHIFT HEIGHT).")
   (cursor :initform nil :accessor fake-cursor
           :documentation "The cursor position of the last frame as (ROW . COLUMN).")
   (cursor-shape :initform :block :accessor fake-cursor-shape
                 :documentation "The cursor shape of the last frame.")
   (cursor-colour :initform nil :accessor fake-cursor-colour
                  :documentation "The cursor colour of the last frame, NIL for the text's.")
   (frames :initform 0 :accessor fake-frames
           :documentation "Number of frames painted.")
   (history :initform '() :accessor fake-history
            :documentation "Every frame painted, newest first, each a vector of strings.")
   (face-history :initform '() :accessor fake-face-history
                 :documentation "The faces of every frame painted, newest first, each a cons (SPANS . ROW-FACES) of vectors.")
   (primary :initform nil :accessor fake-primary
            :documentation "The text of the fake primary selection, or NIL: what the editor made it, or what a test selected as another program would.")
   (clipboard :initform nil :accessor fake-clipboard
              :documentation "The text on the fake system clipboard, or NIL: what the editor put there, or what a test did as another program would.")
   (titles :initform '() :accessor fake-titles
           :documentation "Every title the editor gave the fake window, newest first.")
   (font-steps :initarg :font-steps :initform nil :accessor fake-font-steps
               :documentation "The steps the fake window's font is scaled by, or NIL for a fake terminal, whose font is not its own.")
   (fonts :initarg :fonts :initform '() :accessor fake-fonts
          :documentation "The font families the fake window offers; none for a fake terminal.")
   (font-faces :initarg :font-faces :initform '() :accessor fake-font-faces
               :documentation "What the fake window answers FRONTEND-FONT-FACES with.")
   (desktop-style :initarg :desktop-style :initform nil :accessor fake-desktop-style
                  :documentation "The desktop's style the fake frontend reports, :dark, :light or NIL.")
   (font :initform nil :accessor fake-font
         :documentation "The font the editor last chose for the fake window, or NIL.")
   (cell-pixels :initarg :cell-pixels :initform nil :accessor fake-cell-pixels
                :documentation "The width and height in pixels of a cell of the fake screen, as a list, when it shows pictures; NIL when it shows none.")
   (images :initform '() :accessor fake-images
           :documentation "The pictures put in the last frame, each (IMAGE ROW COLUMN COLUMNS ROWS OFFSET), in the order put.")
   (image-history :initform '() :accessor fake-image-history
                  :documentation "The pictures of every frame painted, newest first, as FAKE-IMAGES has them.")
   (toggles :initform '() :accessor fake-toggles
            :documentation "The window states the editor turned the fake window to or from, newest first; only for a fake window (FONT-STEPS set)."))
  (:documentation "A fake frontend for driving the editor from tests."))

(setf (documentation 'fake-screen 'function) "The last frame painted by the fake frontend, a vector of strings."
      (documentation 'fake-cursor 'function) "Cursor of the last frame as (ROW . COLUMN)."
      (documentation 'fake-spans 'function) "Face spans per row of the last frame."
      (documentation 'fake-row-faces 'function) "Whole-row faces of the last frame, one per row."
      (documentation 'fake-row-scales 'function) "The parts of each row of the last frame drawn large or moved down, (START END SCALE SHIFT HEIGHT) in columns and rows."
      (documentation 'fake-cursor-shape 'function) "Cursor shape of the last frame, :block, :bar or :underline."
      (documentation 'fake-cursor-colour 'function) "Cursor colour of the last frame, as faces take colours, or NIL for the text's."
      (documentation 'fake-frames 'function) "Number of frames the fake frontend has painted."
      (documentation 'fake-history 'function) "All frames painted, newest first."
      (documentation 'fake-face-history 'function) "The faces of all frames painted, newest first, as (SPANS . ROW-FACES)."
      (documentation 'fake-pending 'function) "True when the fake frontend claims input is waiting."
      (documentation 'fake-clipboard 'function) "The text on the fake frontend's system clipboard, or NIL."
      (documentation 'fake-primary 'function) "The text of the fake frontend's primary selection, or NIL."
      (documentation 'fake-titles 'function) "The titles the fake frontend's window was given, newest first."
      (documentation 'fake-font-steps 'function) "The steps the fake frontend's font is scaled by, or NIL when it has no font of its own."
      (documentation 'fake-fonts 'function) "The font families the fake frontend offers."
      (documentation 'fake-font 'function) "The font the editor last chose for the fake frontend, or NIL."
      (documentation 'fake-toggles 'function) "The window states the fake frontend was turned to or from, newest first."
      (documentation 'fake-font-faces 'function) "The font families and variants the fake frontend offers, as FRONTEND-FONT-FACES gives them."
      (documentation 'fake-desktop-style 'function) "The desktop's style the fake frontend reports: :dark, :light or NIL."
      (documentation 'fake-images 'function) "The pictures put in the fake frontend's last frame, each (IMAGE ROW COLUMN COLUMNS ROWS OFFSET)."
      (documentation 'fake-image-history 'function) "The pictures of every frame the fake frontend painted, newest first.")

(defun make-fake-frontend (&key (width 40) (height 8) (colours 256) font-steps fonts font-faces desktop-style cell-pixels)
  "Return a fake frontend with a WIDTH by HEIGHT screen showing COLOURS colours;
FONT-STEPS, a number, makes it a window whose font can be scaled, FONTS the
font families it offers, FONT-FACES what it lists of them with their variants,
DESKTOP-STYLE the desktop's style it reports; CELL-PIXELS, a list of a cell's
width and height in pixels, makes it show pictures."
  (make-instance 'fake-frontend :width width :height height :colours colours :font-steps font-steps
                                :fonts fonts :font-faces font-faces :desktop-style desktop-style
                                :cell-pixels cell-pixels))

(defmethod tuatara.frontend:frontend-images-p ((f fake-frontend))
  (and (fake-cell-pixels f) t))

(defmethod tuatara.frontend:frontend-cell-pixels ((f fake-frontend))
  (values-list (fake-cell-pixels f)))

(defmethod tuatara.frontend:frontend-put-image ((f fake-frontend) image row column columns rows &optional offset)
  (setf (fake-images f) (append (fake-images f) (list (list image row column columns rows offset)))))

(defmethod tuatara.frontend:frontend-colour-count ((f fake-frontend))
  (fake-colours f))

(defmethod tuatara.frontend:frontend-read-event-timeout ((f fake-frontend) seconds)
  ;; with nothing queued, wait SECONDS and tick instead of ending the session
  ;; while work runs in the background, so that it finishes first; a tick
  ;; function that only wants to look again later does not keep the session up
  (cond ((fake-events f) (tuatara.frontend:frontend-read-event f))
        (tuatara.command:*background-work* (sleep seconds) (tuatara.frontend:make-event :tick))
        (t (tuatara.frontend:make-event :quit))))

(defmethod tuatara.frontend:frontend-set-clipboard ((f fake-frontend) text)
  (setf (fake-clipboard f) (copy-seq text))
  t)

(defmethod tuatara.frontend:frontend-set-primary-selection ((f fake-frontend) text)
  (setf (fake-primary f) (copy-seq text))
  t)

(defmethod tuatara.frontend:frontend-set-title ((f fake-frontend) title)
  (push title (fake-titles f)))

(defmethod tuatara.frontend:frontend-fonts ((f fake-frontend))
  (fake-fonts f))

(defmethod tuatara.frontend:frontend-font-faces ((f fake-frontend))
  (fake-font-faces f))

(defmethod tuatara.frontend:frontend-set-font ((f fake-frontend) name)
  ;; the family is the name without a size at its end; the size kept is 11
  (let* ((words (uiop:split-string name :separator " "))
         (sized (and (rest words) (every #'digit-char-p (car (last words)))))
         (family-words (if sized (butlast words) words)))
    (when (some (lambda (family)
                  (let ((family-words-n (length (uiop:split-string family :separator " "))))
                    (and (<= family-words-n (length family-words))
                         (string-equal family (format nil "~{~a~^ ~}" (subseq family-words 0 family-words-n))))))
                (fake-fonts f))
      (setf (fake-font f) (if sized name (format nil "~a 11" name))))))

(defmethod tuatara.frontend:frontend-toggle-state ((f fake-frontend) state)
  (when (fake-font-steps f)
    (push state (fake-toggles f))
    t))

(defmethod tuatara.frontend:frontend-font-steps ((f fake-frontend))
  (fake-font-steps f))

(defmethod (setf tuatara.frontend:frontend-font-steps) (steps (f fake-frontend))
  (when (fake-font-steps f)
    (setf (fake-font-steps f) steps)))

(defmethod tuatara.frontend:frontend-clipboard ((f fake-frontend))
  (fake-clipboard f))

(defmethod tuatara.frontend:frontend-primary-selection ((f fake-frontend))
  (fake-primary f))

(defmethod tuatara.frontend:frontend-input-pending-p ((f fake-frontend))
  (let ((pending (fake-pending f)))
    (if (functionp pending) (and (funcall pending f) t) pending)))

(defun fake-queue-event (frontend event)
  "Append EVENT to FRONTEND's queue."
  (setf (fake-events frontend) (append (fake-events frontend) (list event))))

(defun fake-queue-keys (frontend keys)
  "Queue key events for KEYS, a key sequence as a string (\"C-x C-f a b\")."
  (dolist (spec (tuatara.frontend:parse-key-sequence keys))
    (fake-queue-event frontend
                      (tuatara.frontend:make-event
                       :key :key (tuatara.frontend:make-key (car (last spec))
                                                            :ctrl (and (member :ctrl spec) t)
                                                            :meta (and (member :meta spec) t)
                                                            :shift (and (member :shift spec) t))))))

(defun fake-screen-line (frontend row)
  "Row ROW of the last frame, right-trimmed."
  (string-right-trim " " (aref (fake-screen frontend) row)))

(defmethod tuatara.frontend:frontend-start ((f fake-frontend))
  (setf (fake-screen f) (make-array (fake-height f) :initial-element "")))

(defmethod tuatara.frontend:frontend-stop ((f fake-frontend)) nil)

(defmethod tuatara.frontend:frontend-size ((f fake-frontend))
  (values (fake-width f) (fake-height f)))

(defmethod tuatara.frontend:frontend-read-event ((f fake-frontend))
  (let ((event (pop (fake-events f))))
    (cond ((functionp event)
           ;; an action queued in its turn (FAKE-QUEUE-ACTION): done, and the editor woken
           (funcall event)
           (tuatara.frontend:make-event :tick))
          (event)
          (t (tuatara.frontend:make-event :quit)))))

(defun fake-queue-action (frontend function)
  "Queue FUNCTION, called when its turn comes among FRONTEND's events, which then
wakes the editor as FRONTEND-WAKE does: for what changes outside the editor, such
as the desktop's style."
  (setf (fake-events frontend) (append (fake-events frontend) (list function))))

(defmethod tuatara.frontend:frontend-desktop-style ((f fake-frontend))
  (fake-desktop-style f))

(defmethod tuatara.frontend:frontend-begin-frame ((f fake-frontend))
  (setf (fake-images f) '()
        (fake-screen f) (make-array (fake-height f) :initial-element "")
        (fake-spans f) (make-array (fake-height f) :initial-element nil)
        (fake-row-faces f) (make-array (fake-height f) :initial-element nil)
        (fake-row-scales f) (make-array (fake-height f) :initial-element nil)))

(defmethod tuatara.frontend:frontend-put-row ((f fake-frontend) row string &key face spans scales over parts)
  (declare (ignore parts))
  (when (< row (fake-height f))
    ;; what is drawn over the row shown in its text, as a terminal would show it
    (dolist (part (reverse over))
      (destructuring-bind (left text spans) part
        (declare (ignore spans))
        (let ((padded (if (< (length string) (+ left (length text)))
                          (concatenate 'string string (make-string (- (+ left (length text)) (length string)) :initial-element #\Space))
                          (copy-seq string))))
          (setf string (replace padded text :start1 left)))))
    (setf (aref (fake-screen f) row) (copy-seq string)
          (aref (fake-spans f) row) spans
          (aref (fake-row-faces f) row) face
          (aref (fake-row-scales f) row) scales)))

(defmethod tuatara.frontend:frontend-set-cursor ((f fake-frontend) row column shape &optional colour)
  (setf (fake-cursor f) (cons row column) (fake-cursor-shape f) shape (fake-cursor-colour f) colour))

(defmethod tuatara.frontend:frontend-end-frame ((f fake-frontend))
  (incf (fake-frames f))
  (push (copy-seq (fake-screen f)) (fake-history f))
  (push (fake-images f) (fake-image-history f))
  (push (cons (copy-seq (fake-spans f)) (copy-seq (fake-row-faces f))) (fake-face-history f)))

(defun fake-frame-with (frontend text)
  "The most recent frame with a row containing TEXT, as three values: its screen
(a vector of strings), its spans and its row faces (vectors by row); NIL when none."
  (loop for screen in (fake-history frontend)
        for faces in (fake-face-history frontend)
        when (some (lambda (row) (search text row)) screen)
          return (values screen (car faces) (cdr faces))))

(defun fake-frame-showing (frontend &rest texts)
  "True when some frame painted so far had a row containing every string in TEXTS."
  (some (lambda (screen)
          (some (lambda (row) (every (lambda (text) (search text row)) texts)) screen))
        (fake-history frontend)))

(defun fake-frame-showing-across-rows (frontend text)
  "True when some frame painted so far showed TEXT, its rows read as one line
with a space where each row ends: for text that may be wrapped onto the next
row, as a long question in the echo area is."
  (some (lambda (screen)
          (search text (format nil "~{~a~^ ~}" (map 'list (lambda (row) (string-right-trim " " row)) screen))))
        (fake-history frontend)))
