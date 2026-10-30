;;;; frontends/gtk/input.lisp --- The window's keys, mouse, dropped files and input method, handed to the editor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.frontend.gtk)

;;; Keys

(defparameter *special-keyvals*
  '((#xff08 . :backspace) (#xff09 . :tab) (#xfe20 . :tab) (#xff0d . :enter) (#xff8d . :enter)
    (#xff1b . :escape) (#xffff . :delete) (#xff9f . :delete) (#xff63 . :insert) (#xff9e . :insert)
    (#xff50 . :home) (#xff95 . :home) (#xff57 . :end) (#xff9c . :end)
    (#xff51 . :left) (#xff96 . :left) (#xff52 . :up) (#xff97 . :up)
    (#xff53 . :right) (#xff98 . :right) (#xff54 . :down) (#xff99 . :down)
    (#xff55 . :page-up) (#xff9a . :page-up) (#xff56 . :page-down) (#xff9b . :page-down)
    (#xffbe . :f1) (#xffbf . :f2) (#xffc0 . :f3) (#xffc1 . :f4) (#xffc2 . :f5) (#xffc3 . :f6)
    (#xffc4 . :f7) (#xffc5 . :f8) (#xffc6 . :f9) (#xffc7 . :f10) (#xffc8 . :f11) (#xffc9 . :f12))
  "GDK's key values of the keys that type nothing, and the keys they are.")

(defun gdk-modifiers (state)
  "Three values, whether GDK's modifier STATE holds Control, Meta (Alt, or
the Meta key itself) and Shift."
  (values (logtest state 4) (logtest state (logior 8 (ash 1 28))) (logtest state 1)))

(defun typable-code-p (code)
  "True when CODE is that of a character typing puts in text: not a control
character (TUATARA.TEXT:CONTROL-CODE-P), nor half of a UTF-16 pair."
  (not (or (tuatara.text:control-code-p code) (<= #xD800 code #xDFFF))))

(defun typable-text (text)
  "TEXT, from the input method, without the characters typing does not put in
text (TYPABLE-CODE-P)."
  (remove-if-not (lambda (c) (typable-code-p (char-code c))) text))

(defun gdk-key (keyval state &optional (unicode (and (< keyval #x1000000) (gdk-keyval-to-unicode keyval))))
  "The key GDK's KEYVAL with the modifiers of STATE stands for, or NIL for one
that is not a key of its own (a modifier pressed alone). UNICODE is the
character KEYVAL types, as GDK says."
  (multiple-value-bind (ctrl meta shift) (gdk-modifiers state)
    (let ((special (cdr (assoc keyval *special-keyvals*))))
      (cond (special
             (tuatara.frontend:make-key special :ctrl ctrl :meta meta :shift (or shift (= keyval #xfe20))))
            ((and unicode (typable-code-p unicode))
             (tuatara.frontend:make-key (code-char unicode) :ctrl ctrl :meta meta))
            (t nil)))))

(defun push-key (f key)
  "Hand KEY to the editor."
  (queue-push (gtk-queue f) (tuatara.frontend:make-event :key :key key)))

(define-callback on-key-pressed :boolean ((controller :pointer) (keyval :uint) (keycode :uint) (state :uint) (data :pointer))
  (declare (ignore controller keycode data))
  (let ((f *frontend*) (key (gdk-key keyval state)))
    (when (and f key)
      (push-key f key)
      t)))

(defun command-key-p (keyval state)
  "True when the key GDK's KEYVAL with the modifiers of STATE is a command key,
one held with Control or Meta, handed to the editor at once, never to the
input method: but Control+Shift+U, which starts typing a character by its code."
  (multiple-value-bind (ctrl meta shift) (gdk-modifiers state)
    (and (or ctrl meta)
         (not (and ctrl shift (member keyval '(#x55 #x75)))))))

(define-callback on-command-key-pressed :boolean ((controller :pointer) (keyval :uint) (keycode :uint) (state :uint) (data :pointer))
  ;; before the input method: a command key gives up what it is composing (an accent
  ;; typed first, a dead key's) and runs as it always does
  (declare (ignore controller keycode data))
  (let ((f *frontend*) (key (and (command-key-p keyval state) (gdk-key keyval state))))
    (when (and f key)
      (when (gtk-preedit f)
        (gtk-im-context-reset (gtk-im f))
        (setf (gtk-preedit f) nil)
        (redraw f))
      (push-key f key)
      t)))

(define-callback on-commit :void ((context :pointer) (text :string) (data :pointer))
  (declare (ignore context data))
  (let ((f *frontend*))
    (when f
      (loop for c across (typable-text text)
            do (push-key f (tuatara.frontend:make-key c))))))

(defun cell-at (f x y)
  "The cell of F's window at pixel X, Y: two values, its column and row, the
room round the grid (GTK-PADDING) left out. Over a part of a row drawn large
or moved down, that row's, and the column that stands for the character
drawn there (TUATARA.DISPLAY:LARGE-COLUMN)."
  (destructuring-bind (top right bottom left) (gtk-padding f)
    (declare (ignore right bottom))
    (let* ((cw (cell-width f)) (ch (cell-height f))
           (gx (/ (- x left) cw)) (gy (/ (- y top) ch))
           (frame (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f))))
      (when frame
        (loop for entry across (frame-rows frame)
              for row from 0
              do (loop for (start end scale shift height) in (fourth entry)
                       when (and (plusp scale) (<= start gx) (< gx end)
                                 (<= (+ row shift) gy) (< gy (+ row shift (or height scale))))
                         do (return-from cell-at
                              (values (+ start (tuatara.display:large-column (floor (- gx start) scale) scale)) row)))))
      (values (floor gx) (floor gy)))))

(defun modifier-key (base state column row)
  "A mouse key BASE with the modifiers of GDK's STATE, at cell COLUMN, ROW."
  (multiple-value-bind (ctrl meta shift) (gdk-modifiers state)
    (tuatara.frontend:make-key base :ctrl ctrl :meta meta :shift shift :column column :row row)))

(define-callback on-preedit-changed :void ((context :pointer) (data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when f
      (sb-alien:with-alien ((text sb-sys:system-area-pointer (null-pointer))
                            (attributes sb-sys:system-area-pointer (null-pointer))
                            (cursor sb-alien:int 0))
        (gtk-im-context-get-preedit-string context (sb-alien:alien-sap (sb-alien:addr text))
                                           (sb-alien:alien-sap (sb-alien:addr attributes)) (sb-alien:alien-sap (sb-alien:addr cursor)))
        (let ((pointer text) (list attributes))
          (setf (gtk-preedit f)
                (unless (null-pointer-p pointer)
                  (let ((string (sap-string pointer)))
                    (let ((shown (typable-text string))) (and (plusp (length shown)) shown)))))
          (unless (null-pointer-p pointer) (g-free pointer))
          (unless (null-pointer-p list) (pango-attr-list-unref list))))
      (redraw f))))

(defun wheel-turns (f dy)
  "The turns of the wheel the scroll DY, in GDK's units, makes in F's window
with what was left over from the scrolls before: a whole number, below zero
for up; what is left of a turn is kept for the next. One scroll turns the
wheel a screenful of times at most, the rest of it dropped."
  (let* ((limit (max 1 (gtk-rows f)))
         (scroll (max (- limit) (min limit (+ (gtk-scroll f) dy))))
         (turns (truncate scroll)))
    (setf (gtk-scroll f) (- scroll turns))
    turns))

(define-callback on-scroll :boolean ((controller :pointer) (dx :double) (dy :double) (data :pointer))
  (declare (ignore dx data))
  (let ((f *frontend*))
    (when f
      (let* ((turns (wheel-turns f dy))
             (pointer (gtk-pointer f))
             (state (gtk-event-controller-get-current-event-state controller)))
        (multiple-value-bind (column row) (if pointer (cell-at f (car pointer) (cdr pointer)) (values nil nil))
          (loop repeat (abs turns)
                do (push-key f (modifier-key (if (plusp turns) :wheel-down :wheel-up) state column row)))))
      t)))

(defun pointer-shape-at (frame column row)
  "The name of the pointer's shape over cell COLUMN, ROW of FRAME: an arrow on a
mode line and on the rows below the windows, a sideways arrow on the divider
between windows side by side, the text beam elsewhere."
  (let* ((rows (and frame (frame-rows frame)))
         (entry (and rows (< -1 row (length rows)) (aref rows row))))
    (flet ((face-at (face)
             (or (and (eq (second entry) face) t)
                 (some (lambda (span) (and (eq (third span) face) (<= (first span) column) (< column (second span))))
                       (third entry)))))
      (cond ((null entry) "default")
            ((face-at :vertical-border) "col-resize")
            ((or (face-at :mode-line) (face-at :mode-line-inactive)) "default")
            ((= row (1- (length rows))) "default")
            (t "text")))))

(define-callback on-motion :void ((controller :pointer) (x :double) (y :double) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when (and f (gtk-area f))
      (setf (gtk-pointer f) (cons x y))
      ;; the pointer's shape follows what it is over, but not while a drag goes on
      (unless (gtk-drag f)
        (multiple-value-bind (column row) (cell-at f x y)
          (let ((shape (pointer-shape-at (sb-thread:with-mutex ((gtk-lock f)) (gtk-shown f)) column row)))
            (unless (string= shape (gtk-pointer-shape f))
              (setf (gtk-pointer-shape f) shape)
              (gtk-widget-set-cursor-from-name (gtk-area f) shape))))))))

(define-callback on-pointer-leave :void ((controller :pointer) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when f (setf (gtk-pointer f) nil))))

(define-callback on-pressed :void ((gesture :pointer) (presses :int) (x :double) (y :double) (data :pointer))
  (declare (ignore data))
  (let ((f *frontend*))
    (when f
      (gtk-widget-grab-focus (gtk-area f))
      (gtk-im-context-reset (gtk-im f))
      (multiple-value-bind (column row) (cell-at f x y)
        (push-key f (modifier-key (case presses (1 :down-mouse-1) (2 :double-mouse-1) (t :triple-mouse-1))
                                  (gtk-event-controller-get-current-event-state gesture)
                                  column row))))))

(define-callback on-middle-pressed :void ((gesture :pointer) (presses :int) (x :double) (y :double) (data :pointer))
  (declare (ignore presses data))
  (let ((f *frontend*))
    (when f
      (multiple-value-bind (column row) (cell-at f x y)
        (push-key f (modifier-key :mouse-2 (gtk-event-controller-get-current-event-state gesture) column row))))))

(define-callback on-released :void ((gesture :pointer) (presses :int) (x :double) (y :double) (data :pointer))
  (declare (ignore presses data))
  (let ((f *frontend*))
    (when f
      (multiple-value-bind (column row) (cell-at f x y)
        (push-key f (modifier-key :up-mouse-1 (gtk-event-controller-get-current-event-state gesture) column row))))))

(defun drag-past-text-p (f row)
  "True when a drag at ROW is above F's window or on its last two rows, the
bottom mode line and the echo row, where holding it still keeps scrolling."
  (or (< row 0) (>= row (- (gtk-rows f) 2))))

(define-callback on-drag-repeat :boolean ((data :pointer))
  (declare (ignore data))
  (let* ((f *frontend*) (cell (and f (gtk-drag f) (gtk-drag-cell f))))
    (cond ((and cell (drag-past-text-p f (cdr cell)))
           (when (queue-empty-p (gtk-queue f))
             (push-key f (tuatara.frontend:make-key :drag-mouse-1 :column (car cell) :row (cdr cell))))
           t)
          (t (when f (setf (gtk-drag-repeating f) nil))
             nil))))

(define-callback on-drag-begin :void ((gesture :pointer) (x :double) (y :double) (data :pointer))
  (declare (ignore gesture data))
  (let ((f *frontend*))
    (when f
      (setf (gtk-drag f) (cons x y)
            (gtk-drag-cell f) (multiple-value-call #'cons (cell-at f x y))))))

(define-callback on-drag-update :void ((gesture :pointer) (dx :double) (dy :double) (data :pointer))
  (declare (ignore gesture data))
  (let* ((f *frontend*) (start (and f (gtk-drag f))))
    (when start
      (multiple-value-bind (column row) (cell-at f (+ (car start) dx) (+ (cdr start) dy))
        (let ((cell (gtk-drag-cell f)))
          (unless (and cell (= column (car cell)) (= row (cdr cell)))
            (setf (gtk-drag-cell f) (cons column row))
            (push-key f (tuatara.frontend:make-key :drag-mouse-1 :column column :row row))))
        (when (and (drag-past-text-p f row) (not (gtk-drag-repeating f)))
          (setf (gtk-drag-repeating f) t)
          (g-timeout-add 50 (callback on-drag-repeat) (null-pointer)))))))

(define-callback on-drag-end :void ((gesture :pointer) (dx :double) (dy :double) (data :pointer))
  (declare (ignore gesture dx dy data))
  (let ((f *frontend*))
    (when f (setf (gtk-drag f) nil))))

(defun dropped-paths (value)
  "The local paths of the files in VALUE, a GValue holding a GdkFileList, as
the editor names files: a name's octets that are not UTF-8 kept as they are."
  (let* ((list (g-value-get-boxed value))
         (files (if (null-pointer-p list) (null-pointer) (gdk-file-list-get-files list)))
         (paths '()))
    (unwind-protect
         ;; a GSList's cells: what each holds, then the next
         (do ((node files (sb-sys:sap-ref-sap node sb-vm:n-word-bytes)))
             ((null-pointer-p node))
           (let ((path (g-file-get-path (sb-sys:sap-ref-sap node 0))))
             (unless (null-pointer-p path)
               (push (sap-string path) paths)
               (g-free path))))
      (unless (null-pointer-p files) (g-slist-free files)))
    (nreverse paths)))

(define-callback on-drop :boolean ((target :pointer) (value :pointer) (x :double) (y :double) (data :pointer))
  (declare (ignore target data))
  (let ((f *frontend*) (paths (dropped-paths value)))
    (when (and f paths)
      (multiple-value-bind (column row) (cell-at f x y)
        (queue-push (gtk-queue f) (tuatara.frontend:make-event :drop :files paths :column column :row row)))
      t)))

(define-callback on-focus-enter :void ((controller :pointer) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when f
      (setf (gtk-focused f) t)
      (gtk-im-context-focus-in (gtk-im f))
      (redraw f))))

(define-callback on-focus-leave :void ((controller :pointer) (data :pointer))
  (declare (ignore controller data))
  (let ((f *frontend*))
    (when f
      (setf (gtk-focused f) nil)
      (gtk-im-context-focus-out (gtk-im f))
      (redraw f))))

(define-callback (on-close-request :failed t) :boolean ((window :pointer) (data :pointer))
  (declare (ignore window data))
  (let ((f *frontend*))
    (if (and f (not (gtk-done f)))
        (progn (queue-push (gtk-queue f) (tuatara.frontend:make-event :close)) t)
        nil)))
