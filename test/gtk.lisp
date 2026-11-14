;;;; test/gtk.lisp --- The window frontend: keys from GDK, colours, frames painted without a display, the mouse, the title, the icon.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test gtk
  :description "The GTK4 frontend: GDK's keys as the editor's, face colours as RGB, events passed between threads, frames painted as the window paints them, in memory, what the input method composes, the mouse's cells, the window's title, and the program's icon.")

(defun gtk-frontend-source ()
  "The text of the window frontend's source files but its calls into GTK's
libraries (ffi.lisp), one after another."
  (format nil "~{~a~}" (mapcar #'uiop:read-file-string
                               (remove "ffi"
                                       (directory (merge-pathnames "frontends/gtk/*.lisp"
                                                                   (asdf:system-source-directory :tuatara)))
                                       :key #'pathname-name :test #'string=))))

(define-test (gtk keys-from-gdk)
  (flet ((spec (keyval state unicode)
           (let ((key (tuatara.frontend.gtk:gdk-key keyval state unicode)))
             (and key (tuatara.frontend:key-spec key)))))
    (is equal '(:enter) (spec #xff0d 0 0))
    (is equal '(:ctrl #\a) (spec #x61 4 97))
    (is equal '(:meta #\x) (spec #x78 8 120))
    (is equal '(:ctrl #\Space) (spec #x20 4 32))
    (is equal '(:shift :tab) (spec #xfe20 1 0))
    (is equal '(:f5) (spec #xffc2 0 0))
    (is equal '(:ctrl :meta :backspace) (spec #xff08 12 8))
    ;; a modifier pressed alone is no key
    (false (spec #xffe1 1 0))
    ;; a letter with Shift is its capital, and no shift is reported
    (is equal '(#\A) (spec #x41 1 65))
    ;; a control character, Delete or one of 128 to 159, is no key that types
    (false (spec #x1000085 0 #x85))
    (false (spec #x100007f 0 127)))
  ;; what the input method commits or composes keeps only what typing puts in text
  (is string= (format nil "a~cb" (code-char #xe9))
      (tuatara.frontend.gtk::typable-text (format nil "a~c~c~c~cb" (code-char 127) (code-char #x85) #\Tab (code-char #xe9)))))

(define-test (gtk colours)
  (is equal '(1d0 0d0 0d0) (tuatara.frontend.gtk:colour-rgb "#ff0000"))
  (is equal (list (/ 205 255d0) 0d0 0d0) (tuatara.frontend.gtk:colour-rgb :red))
  (is equal '(0d0 0d0 0d0) (tuatara.frontend.gtk:colour-rgb 16))
  (false (tuatara.frontend.gtk:colour-rgb nil)))

(define-test (gtk paper-from-the-desktop)
  ;; where no face gives a colour: black on white, or the desktop's when asked for
  (let ((tuatara.frontend.gtk::*desktop-colours* '((0.8d0 0.8d0 0.8d0) (0.1d0 0.1d0 0.1d0))))
    (let ((tuatara-user:*window-colours-from-desktop* nil))
      (is equal '((0d0 0d0 0d0) (1d0 1d0 1d0))
          (list (tuatara.frontend.gtk::window-paper :fg) (tuatara.frontend.gtk::window-paper :bg))))
    (let ((tuatara-user:*window-colours-from-desktop* t))
      (is equal '((0.8d0 0.8d0 0.8d0) (0.1d0 0.1d0 0.1d0))
          (list (tuatara.frontend.gtk::window-paper :fg) (tuatara.frontend.gtk::window-paper :bg)))
      ;; a face's own colour is not changed by it
      (tuatara.display:set-face-attributes :gtk-test-paper :fg "#ff0000")
      (is equal '(1d0 0d0 0d0) (tuatara.frontend.gtk::face-colours :gtk-test-paper))))
  ;; nothing known of the desktop: black on white even when asked for
  (let ((tuatara.frontend.gtk::*desktop-colours* nil) (tuatara-user:*window-colours-from-desktop* t))
    (is equal '((0d0 0d0 0d0) (1d0 1d0 1d0))
        (list (tuatara.frontend.gtk::window-paper :fg) (tuatara.frontend.gtk::window-paper :bg)))))

(define-test (gtk events-between-threads)
  (let ((queue (tuatara.frontend:make-event-queue)))
    ;; nothing within the time: NIL
    (false (tuatara.frontend:queue-pop queue 0.01))
    ;; one pushed from another thread arrives, and in order
    (sb-thread:join-thread (sb-thread:make-thread (lambda ()
                                                    (tuatara.frontend:queue-push queue 1)
                                                    (tuatara.frontend:queue-push queue 2))))
    (is = 1 (tuatara.frontend:queue-pop queue 1))
    (is = 2 (tuatara.frontend:queue-pop queue 1)))
  ;; wakes in a row are one, and no input waiting; a key is
  (let* ((f (tuatara.frontend.gtk::make-gtk-frontend))
         (queue (tuatara.frontend.gtk::gtk-queue f)))
    (dotimes (i 5) (tuatara.frontend:queue-push queue (tuatara.frontend:make-event :tick)))
    (is = 1 (length (tuatara.frontend::event-queue-items queue)))
    (false (tuatara.frontend:frontend-input-pending-p f))
    (tuatara.frontend:queue-push queue (tuatara.frontend:make-event :key :key (tuatara.frontend:make-key #\a)))
    (true (tuatara.frontend:frontend-input-pending-p f)))
  ;; the window's new size is no input waiting either: work giving way to input does not stop for it
  (let* ((f (tuatara.frontend.gtk::make-gtk-frontend))
         (queue (tuatara.frontend.gtk::gtk-queue f)))
    (tuatara.frontend:queue-push queue (tuatara.frontend:make-event :resize :width 80 :height 24))
    (false (tuatara.frontend:frontend-input-pending-p f))
    (tuatara.frontend:queue-push queue (tuatara.frontend:make-event :drop :files '("/tmp/a")))
    (true (tuatara.frontend:frontend-input-pending-p f))))

(define-test (gtk pictures)
  ;; a picture over the rows, in its cells from their top left, at its own size, its alpha applied
  (let* ((image (tuatara.frontend:make-image 2 2 4 (coerce '(255 0 0 255  0 255 0 255
                                                             0 0 255 255  0 0 0 0)
                                                           '(simple-array (unsigned-byte 8) (*)))))
         (frame (tuatara.frontend.gtk:make-frame (vector nil nil nil nil))))
    (true (tuatara.frontend:frontend-images-p (tuatara.frontend.gtk:make-gtk-frontend)))
    ;; pictures are made in the display's own pixels: twice the cell's on a screen scaled to 200 per cent
    (let ((f (tuatara.frontend.gtk:make-gtk-frontend)))
      (setf (tuatara.frontend.gtk::gtk-display-scale f) 2)
      (is equal (list (* 2 (tuatara.frontend.gtk::cell-width f)) (* 2 (tuatara.frontend.gtk::cell-height f)))
          (multiple-value-list (tuatara.frontend:frontend-cell-pixels f))))
    (setf (tuatara.frontend.gtk::frame-images frame) (list (list image 1 2 5 2)))
    (tuatara.frontend.gtk:call-with-painted-frame
     (lambda (surface width height cell-width cell-height)
       (declare (ignore width height))
       (let ((x (* 2 cell-width)) (y cell-height))
         (flet ((at (dx dy) (tuatara.frontend.gtk:pixel-colour surface (+ x dx) (+ y dy))))
           (is string= "#ff0000" (at 0 0))
           (is string= "#00ff00" (at 1 0))
           (is string= "#0000ff" (at 0 1))
           ;; a pixel with no alpha shows the ground, and so does all past the picture
           (is string= (at 5 5) (at 1 1))
           (false (string= "#ff0000" (at 5 5))))))
     frame :columns 10 :rows 4)))

(define-test (gtk painting-without-a-display)
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (let ((frame (tuatara.frontend.gtk:make-frame
                         (vector (list "hello" nil '())
                                 (list "world" nil '((0 5 :region)))
                                 (list " status" :mode-line '())
                                 nil))))
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (true (plusp cell-width))
                (true (> cell-height cell-width))
                (is = (* 10 cell-width) width)
                (is = (* 4 cell-height) height)
                (flet ((at (column row)
                         (tuatara.frontend.gtk:pixel-colour surface
                                                            (1+ (* column cell-width))
                                                            (1+ (* row cell-height)))))
                  ;; the ground, where nothing is written
                  (is string= "#080d15" (at 8 0))
                  ;; the cursor, a block at the top left, in the colour of the text
                  (is string= "#dadada" (at 0 0))
                  ;; the selection behind the second row's text, and only there
                  (is string= "#7533bd" (at 0 1))
                  (is string= "#080d15" (at 7 1))
                  ;; the mode line's colour across the whole row
                  (is string= (tuatara.frontend.gtk:pixel-colour surface (- width 2) (1+ (* 2 cell-height)))
                      (at 1 2))
                  (true (string/= "#080d15" (at 9 2)))))
              frame :columns 10 :rows 4 :font "Adwaita Mono 11")
             ;; a colour of its own (*CURSOR-COLOUR*), and a line under the character
             (setf (tuatara.frontend.gtk::frame-cursor-colour frame) "#45b1e8")
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore width height cell-height))
                (is string= "#45b1e8" (tuatara.frontend.gtk:pixel-colour surface 1 1))
                (is string= "#45b1e8" (tuatara.frontend.gtk:pixel-colour surface (1- cell-width) 1)))
              frame :columns 10 :rows 4 :font "Adwaita Mono 11")
             (setf (tuatara.frontend.gtk::frame-cursor-shape frame) :underline)
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore width height))
                (is string= "#45b1e8" (tuatara.frontend.gtk:pixel-colour surface (1- cell-width) (1- cell-height)))
                (is string= "#080d15"
                    (tuatara.frontend.gtk:pixel-colour surface (1- cell-width) (floor cell-height 2))))
              frame :columns 10 :rows 4 :font "Adwaita Mono 11")))
      (tuatara.display:apply-theme theme))))

(define-test (gtk text-set-out-once)
  ;; text painted again in a later frame, as when a window scrolls, keeps the
  ;; layout Pango set it out in: what each row more costs in text set out when
  ;; the same frame is painted a second time, on a new drawing as the window's
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (tuatara.frontend.gtk::load-gtk-libraries)
           (flet ((set-out-again (rows)
                    (let ((frame (tuatara.frontend.gtk:make-frame
                                  (coerce (loop for i below rows
                                                collect (list (format nil "~d,plain,~d,é" i i) nil
                                                              '((0 2 :csv-column-0) (3 8 :csv-column-1))))
                                          'vector)))
                          (f (tuatara.frontend.gtk::make-gtk-frontend))
                          (count 0))
                      (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
                        (setf (tuatara.frontend.gtk::gtk-fonts f) (tuatara.frontend.gtk::make-fonts "Adwaita Mono 11")
                              (tuatara.frontend.gtk::gtk-padding f) (list 0 0 0 0))
                        (tuatara.frontend.gtk::measure-font f)
                        (flet ((paint ()
                                 (let* ((width (* 20 (tuatara.frontend.gtk::cell-width f)))
                                        (height (* rows (tuatara.frontend.gtk::cell-height f)))
                                        (surface (tuatara.frontend.gtk::cairo-image-surface-create 0 width height))
                                        (cr (tuatara.frontend.gtk::cairo-create surface)))
                                   (unwind-protect (tuatara.frontend.gtk::paint-frame f cr width height frame t)
                                     (tuatara.frontend.gtk::cairo-destroy cr)
                                     (tuatara.frontend.gtk::cairo-surface-destroy surface)))))
                          (unwind-protect
                               (progn
                                 (paint)
                                 ;; painted whole again, not from the rows kept painted
                                 (tuatara.frontend.gtk::forget-kept-rows f)
                                 (sb-int:encapsulate 'tuatara.gtk:pango-layout-set-text 'counted
                                                     (lambda (function &rest arguments)
                                                       (incf count)
                                                       (apply function arguments)))
                                 (unwind-protect (paint)
                                   (sb-int:unencapsulate 'tuatara.gtk:pango-layout-set-text 'counted)))
                            (tuatara.frontend.gtk::forget-layouts f)
                            (loop for font across (tuatara.frontend.gtk::gtk-fonts f) do (tuatara.gtk:pango-font-description-free font))
                            (tuatara.frontend.gtk::forget-face-fonts f))))
                      count)))
             ;; three or more for each row before: every run of each row set out again
             (let ((each (/ (- (set-out-again 40) (set-out-again 20)) 20)))
               (true (< each 1/10) "~,2f texts set out again for each row" each))))
      (tuatara.display:apply-theme theme))))

(define-test (gtk drawing-measured)
  ;; how the window drew while a key was held: from each key to the painting
  ;; of the first frame finished after it, and the frames painted meanwhile
  (let* ((unit 1000)
         (keys (map 'vector (lambda (ms) (* ms unit)) '(0 17 33 50)))
         (ends (map 'vector (lambda (ms) (* ms unit)) '(4 21 37 54)))
         (paints (map 'vector (lambda (ms) (* ms unit)) '(2 12 30 48 64 2000)))
         (figures (tuatara.frontend.gtk::drawing-figures keys ends paints)))
    (is = 4 (getf figures :keys))
    ;; a paint before the key's frame is finished does not show it: 12, 13, 15 and 14 ms
    (is = 14.0 (getf figures :median))
    (is = 15.0 (getf figures :slowest))
    ;; those from the first key to half a second after the last
    (is = 5 (getf figures :frames)))
  (is string= "gl" (tuatara.frontend.gtk::renderer-name :gl))
  (is string= "vulkan" (tuatara.frontend.gtk::renderer-name :vulkan))
  (false (tuatara.frontend.gtk::renderer-name nil)))

(defun painted-pixels (f frame width height)
  "The pixels of FRAME painted by F's window as the window paints it, WIDTH by
HEIGHT pixels, on a new picture, as a vector of octets."
  (let* ((surface (tuatara.frontend.gtk::cairo-image-surface-create 0 width height))
         (cr (tuatara.frontend.gtk::cairo-create surface)))
    (unwind-protect
         (progn (tuatara.frontend.gtk::paint-frame f cr width height frame t)
                (tuatara.frontend.gtk::cairo-surface-flush surface)
                (let* ((data (tuatara.frontend.gtk::cairo-image-surface-get-data surface))
                       (size (* height (tuatara.frontend.gtk::cairo-image-surface-get-stride surface)))
                       (pixels (make-array size :element-type '(unsigned-byte 8))))
                  (dotimes (i size pixels) (setf (aref pixels i) (sb-sys:sap-ref-8 data i)))))
      (tuatara.frontend.gtk::cairo-destroy cr)
      (tuatara.frontend.gtk::cairo-surface-destroy surface))))

(defmacro with-painting-window ((f) &body body)
  "Run BODY with F a window's frontend that paints without a display, in
Adwaita Mono 11 on ao-dark."
  `(let ((theme tuatara.display::*current-theme*))
     (unwind-protect
          (progn
            (tuatara.display:apply-theme 'ao-dark)
            (tuatara.frontend.gtk::load-gtk-libraries)
            (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
              (let ((,f (tuatara.frontend.gtk::make-gtk-frontend)))
                (setf (tuatara.frontend.gtk::gtk-fonts ,f) (tuatara.frontend.gtk::make-fonts "Adwaita Mono 11")
                      (tuatara.frontend.gtk::gtk-padding ,f) (list 4 6 4 6))
                (tuatara.frontend.gtk::measure-font ,f)
                (unwind-protect (progn ,@body)
                  (tuatara.frontend.gtk::forget-layouts ,f)
                  (loop for font across (tuatara.frontend.gtk::gtk-fonts ,f) do (tuatara.gtk:pango-font-description-free font))
                  (tuatara.frontend.gtk::forget-face-fonts ,f)))))
       (tuatara.display:apply-theme theme))))

(define-test (gtk rows-kept-painted)
  ;; a window scrolled a line paints the line that came in and the mode line,
  ;; the rest moved as painted before, and shows what painting it whole shows:
  ;; what each row more of the window costs in rows painted
  (flet ((frame (from rows)
           (let ((frame (tuatara.frontend.gtk:make-frame
                         (coerce (append (loop for i from from below (+ from rows -1)
                                               collect (list (format nil "~d,plain,~d,é" i (* i i)) nil
                                                             '((0 2 :csv-column-0) (3 8 :csv-column-1))))
                                         (list (list (format nil " L~d  CSV" from) :mode-line '())))
                                 'vector))))
             (setf (tuatara.frontend.gtk::frame-cursor-row frame) (- rows 4))
             frame)))
    (flet ((scrolled (rows)
             (let ((painted 0))
               (with-painting-window (f)
                 (let ((width (+ 12 (* 30 (tuatara.frontend.gtk::cell-width f))))
                       (height (+ 8 (* rows (tuatara.frontend.gtk::cell-height f)))))
                   (painted-pixels f (frame 0 rows) width height)
                   (sb-int:encapsulate 'tuatara.frontend.gtk::paint-row 'counted
                                       (lambda (function &rest arguments) (incf painted) (apply function arguments)))
                   (let ((kept (unwind-protect (painted-pixels f (frame 1 rows) width height)
                                 (sb-int:unencapsulate 'tuatara.frontend.gtk::paint-row 'counted)))
                         (whole (with-painting-window (g) (painted-pixels g (frame 1 rows) width height))))
                     (true (equalp kept whole) "~d rows: not as painted whole" rows))))
               painted)))
      ;; every row painted again before
      (let ((each (/ (- (scrolled 40) (scrolled 20)) 20)))
        (true (< each 1/10) "~,2f rows painted for each row of the window" each))))
  ;; relative line numbers, point kept on its row as the text scrolls: every
  ;; row's line a line on, its number as it was; of each row, the runs of
  ;; characters at the number are painted, not the rest of the row
  (flet ((frame (from rows)
           (let ((frame (tuatara.frontend.gtk:make-frame
                         (coerce (loop for i from 0 below rows
                                       collect (list (format nil
                                                             "~3d ~d,plain,~d,é"
                                                             (abs (- i 10))
                                                             (+ from i)
                                                             (* (+ from i) (+ from i))) nil
                                                     '((0 4 :line-number) (4 6 :csv-column-0) (7 12 :csv-column-1))))
                                 'vector))))
             (setf (tuatara.frontend.gtk::frame-cursor-row frame) 10)
             frame)))
    (flet ((scrolled (rows)
             (let ((painted 0))
               (with-painting-window (f)
                 (let ((width (+ 12 (* 30 (tuatara.frontend.gtk::cell-width f))))
                       (height (+ 8 (* rows (tuatara.frontend.gtk::cell-height f))))
                       (next (frame 1 rows)))
                   (painted-pixels f (frame 0 rows) width height)
                   ;; point a line down and the text with it: each row's number as it was, its line the next
                   (sb-int:encapsulate 'tuatara.frontend.gtk::paint-run 'counted
                                       (lambda (function &rest arguments) (incf painted) (apply function arguments)))
                   (let ((kept (unwind-protect (painted-pixels f next width height)
                                 (sb-int:unencapsulate 'tuatara.frontend.gtk::paint-run 'counted)))
                         (whole (with-painting-window (g) (painted-pixels g next width height))))
                     (true (equalp kept whole) "~d rows numbered: not as painted whole" rows))))
               painted)))
      ;; six runs a row before, the whole row painted
      (let ((each (/ (- (scrolled 40) (scrolled 20)) 20)))
        (true (< each 3) "~,2f runs painted for each row of the window" each))))
  ;; the gutter told apart from the text (the row's PARTS): the numbers stay
  ;; where they are, the text moves, and only the line that came in is painted
  (flet ((frame (from rows)
           (tuatara.frontend.gtk:make-frame
            (coerce (loop for i from 0 below rows
                          collect (list (format nil
                                                "~3d ~d,plain,~d,é"
                                                (abs (- i 10))
                                                (+ from i)
                                                (* (+ from i) (+ from i))) nil
                                        '((0 4 :line-number) (4 6 :csv-column-0) (7 12 :csv-column-1))
                                        nil nil '(4)))
                    'vector))))
    (flet ((scrolled (rows)
             (let ((painted 0))
               (with-painting-window (f)
                 (let ((width (+ 12 (* 30 (tuatara.frontend.gtk::cell-width f))))
                       (height (+ 8 (* rows (tuatara.frontend.gtk::cell-height f)))))
                   (painted-pixels f (frame 0 rows) width height)
                   (sb-int:encapsulate 'tuatara.frontend.gtk::paint-run 'counted
                                       (lambda (function &rest arguments) (incf painted) (apply function arguments)))
                   (let ((kept (unwind-protect (painted-pixels f (frame 1 rows) width height)
                                 (sb-int:unencapsulate 'tuatara.frontend.gtk::paint-run 'counted)))
                         (whole (with-painting-window (g) (painted-pixels g (frame 1 rows) width height))))
                     (true (equalp kept whole) "~d rows in parts: not as painted whole" rows))))
               painted)))
      ;; two runs a row before, the numbers painted again
      (let ((each (/ (- (scrolled 40) (scrolled 20)) 20)))
        (true (< each 1/5) "~,2f runs painted for each row of the window" each))))
  ;; rows changed in place, some made longer, so that what follows the change
  ;; stands a column further on: painted as when painted whole
  (with-painting-window (f)
    (flet ((frame (field)
             (tuatara.frontend.gtk:make-frame
              (coerce (loop for i from 0 below 12
                            collect (list (format nil
                                                  "~3d ~d,plain,~d,é"
                                                  (abs (- i (if (= field 0) 6 5)))
                                                  (+ field i)
                                                  (* i i)) nil
                                          '((0 4 :line-number) (4 6 :csv-column-0) (7 12 :csv-column-1))))
                      'vector))))
      (let ((width (+ 12 (* 30 (tuatara.frontend.gtk::cell-width f))))
            (height (+ 8 (* 12 (tuatara.frontend.gtk::cell-height f)))))
        (painted-pixels f (frame 0) width height)
        (is equalp (with-painting-window (g) (painted-pixels g (frame 5) width height))
            (painted-pixels f (frame 5) width height))))))

(define-test (gtk row-textures-hold-the-rows)
  ;; the textures the graphics card is given hold what the rows' parts show:
  ;; read back from GTK, each the same, pixel for pixel, as those cells of the
  ;; frame painted whole (the cursor, drawn over the rows, left out)
  (flet ((frame ()
           (let ((frame (tuatara.frontend.gtk:make-frame
                         (coerce (loop for i below 6
                                       collect (list (format nil "~3d ~d,plain,~d,é" i (* 7 i) (* i i)) nil
                                                     '((0 4 :line-number) (4 6 :csv-column-0) (7 12 :csv-column-1))
                                                     nil nil '(4)))
                                 'vector))))
             (setf (tuatara.frontend.gtk::frame-cursor-shape frame) :hidden)
             frame)))
    (with-painting-window (f)
      (let* ((cw (tuatara.frontend.gtk::cell-width f)) (ch (tuatara.frontend.gtk::cell-height f))
             (width (+ 12 (* 30 cw))) (height (+ 8 (* 6 ch)))
             (whole (painted-pixels f (frame) width height))
             (stride (/ (length whole) height))
             (pieces (tuatara.frontend.gtk::keep-rows f (* 30 cw) (tuatara.frontend.gtk:frame-rows (frame))
                                                      (tuatara.frontend.gtk::frame-layouts f)))
             (wrong 0) (seen 0))
        (loop for row-pieces across pieces
              for row from 0
              do (loop for (from below strip) in row-pieces
                       do (let* ((w (* (- below from) cw))
                                 (texture (tuatara.frontend.gtk::strip-texture f strip w)))
                            (tuatara.io:with-foreign-memory (data (* 4 w ch))
                              (tuatara.gtk:gdk-texture-download texture data (* 4 w))
                              (dotimes (y ch)
                                (dotimes (x w)
                                  (dotimes (c 3)
                                    (incf seen)
                                    (unless (= (sb-sys:sap-ref-8 data (+ (* 4 (+ x (* y w))) c))
                                               (aref whole
                                                     (+ (* (+ 4 (* row ch) y) stride) (* 4 (+ 6 (* from cw) x)) c)))
                                      (incf wrong))))))
                            (tuatara.gtk:g-object-unref texture))))
        (true (plusp seen))
        (is = 0 wrong "~d of ~d octets not as painted whole" wrong seen)))))

(defun run-on-broadway (dir script &rest arguments)
  "Run SCRIPT, a file of test/gtk/, with sbcl --script and ARGUMENTS, strings
and :OUT for the file it writes what it found to, on a broadway display of its
own, its state in DIR, and what it wrote there, read as data; NIL when it
wrote nothing."
  (let* ((display (+ 40 (random 40)))
         (out (merge-pathnames "drawn" dir))
         (server (sb-ext:run-program (tuatara.io:find-program "gtk4-broadwayd")
                                     (list (format nil ":~d" display) "--port" (princ-to-string (+ 18000 display))
                                           "--address" "127.0.0.1")
                                     :wait nil :output nil :error nil)))
    (unwind-protect
         (progn
           ;; the server ready once its socket, named for the display after its own, takes a connection
           (wait-until (lambda ()
                         (let ((fd (tuatara.io:connect-to (format nil "~a/broadway~d.socket"
                                                                  (uiop:getenv "XDG_RUNTIME_DIR") (1+ display)))))
                           (when fd (sb-posix:close fd) t)))
                       10)
           (ignore-errors (delete-file out))
           (sb-ext:run-program sb-ext:*runtime-pathname*
                               (list* "--dynamic-space-size"
                                      "4GB"
                                      "--noinform"
                                      "--disable-ldb"
                                      "--end-runtime-options"
                                      "--script"
                                      (namestring (asdf:system-relative-pathname :tuatara
                                                                                 (format nil "test/gtk/~a" script)))
                                      (substitute (namestring out) :out arguments))
                               :wait t :output nil :error nil
                               :environment (append (list "GDK_BACKEND=broadway"
                                                          (format nil "BROADWAY_DISPLAY=:~d" display)
                                                          (format nil
                                                                  "XDG_CONFIG_HOME=~a"
                                                                  (namestring (merge-pathnames "config/" dir)))
                                                          (format nil
                                                                  "XDG_STATE_HOME=~a"
                                                                  (namestring (merge-pathnames "state/" dir)))
                                                          (format nil
                                                                  "XDG_CACHE_HOME=~a"
                                                                  (or (uiop:getenv "XDG_CACHE_HOME")
                                                                      (namestring (merge-pathnames "cache/" dir))))
                                                          (format nil
                                                                  "XDG_DATA_HOME=~a"
                                                                  (namestring (merge-pathnames "data/" dir))))
                                                    (remove-if (lambda (pair)
                                                                 (some (lambda (key) (uiop:string-prefix-p key pair))
                                                                       '("GDK_BACKEND="
                                                                         "BROADWAY_DISPLAY="
                                                                         "WAYLAND_DISPLAY="
                                                                         "DISPLAY="
                                                                         "XDG_CONFIG_HOME="
                                                                         "XDG_STATE_HOME="
                                                                         "XDG_DATA_HOME="
                                                                         "GSK_RENDERER=")))
                                                               (sb-ext:posix-environ))))
           (ignore-errors (with-open-file (in out) (with-standard-io-syntax (let ((*read-eval* nil)) (read in))))))
      (ignore-errors (sb-ext:process-kill server 15))
      (ignore-errors (sb-ext:process-close server)))))

(defun window-drawn (dir file &optional pictures)
  "Open FILE in a real window from these sources on a broadway display of its
own (test/gtk/window-drawn.lisp, with its state in DIR), and what it found:
the textures drawn and the octets the window drawn by the graphics card and
painted whole differ in; NIL when it wrote nothing. PICTURES is \"picture\"
to wait for the file's picture, \"pictures-beside\" for it shown in two
windows side by side, \"floated\" for an Org note's two pictures."
  (apply #'run-on-broadway dir "window-drawn.lisp" (namestring file) :out (and pictures (list pictures))))

(define-test (gtk window-drawn-as-painted)
  ;; a real window, on a display of its own: drawn as pictures the graphics
  ;; card is given, it shows what it shows painted whole, octet for octet; a
  ;; picture in it too, but for how the card and cairo blend a scaled picture's
  ;; pixels
  (if (null (tuatara.io:find-program "gtk4-broadwayd"))
      (skip "gtk4-broadwayd is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((sample (merge-pathnames "sample.csv" dir)))
          (with-open-file (o sample :direction :output :if-exists :supersede)
            (dotimes (i 200) (format o "2026-01-01T00:00:00Z,~d,The quick brown fox jumps over the lazy dog.,abc~d~%" (+ 1700000000 i) i)))
          (let ((drawn (window-drawn dir sample)))
            (true drawn "the window wrote nothing")
            (false (getf drawn :error) "~a" (getf drawn :error))
            (false (getf drawn :timed-out) "the window gave up waiting")
            (when (getf drawn :octets)
              ;; a texture for each row's part at least
              (true (> (getf drawn :textures) 20) "~d textures" (getf drawn :textures))
              (true (plusp (getf drawn :octets)))
              (is = 0 (getf drawn :differing) "~:d of ~:d octets not as painted whole"
                  (getf drawn :differing) (getf drawn :octets)))))
        ;; a picture larger than the window, shown fitted
        (let ((picture (merge-pathnames "gradient.png" dir)))
          (tuatara.frontend.gtk::load-gtk-libraries)
          (let* ((surface (tuatara.frontend.gtk::cairo-image-surface-create 0 1600 1200))
                 (cr (tuatara.frontend.gtk::cairo-create surface)))
            (dotimes (y 12)
              (dotimes (x 16)
                (tuatara.frontend.gtk::cairo-set-source-rgb cr (/ x 16d0) (/ y 12d0) 0.5d0)
                (tuatara.frontend.gtk::cairo-rectangle cr (* 100d0 x) (* 100d0 y) 100d0 100d0)
                (tuatara.frontend.gtk::cairo-fill cr)))
            (tuatara.frontend.gtk::cairo-surface-write-to-png surface (namestring picture))
            (tuatara.frontend.gtk::cairo-destroy cr)
            (tuatara.frontend.gtk::cairo-surface-destroy surface))
          (let ((drawn (window-drawn dir picture "picture")))
            (true drawn "the window with a picture wrote nothing")
            (false (getf drawn :error) "~a" (getf drawn :error))
            (false (getf drawn :timed-out) "the window gave up waiting")
            (when (getf drawn :octets)
              (true (> (getf drawn :textures) 1) "~d textures" (getf drawn :textures))
              (true (< (getf drawn :differing-much) (/ (getf drawn :octets) 200))
                    "~:d of ~:d octets far from painted whole"
                    (getf drawn :differing-much) (getf drawn :octets))))
          ;; the same picture in two windows side by side: drawn in both
          (let ((drawn (window-drawn dir picture "pictures-beside")))
            (true drawn "the window with a picture beside itself wrote nothing")
            (false (getf drawn :error) "~a" (getf drawn :error))
            (false (getf drawn :timed-out) "the window gave up waiting")
            (when (getf drawn :octets)
              (true (< (getf drawn :differing-much) (/ (getf drawn :octets) 200))
                    "~:d of ~:d octets far from painted whole"
                    (getf drawn :differing-much) (getf drawn :octets))))))))

(define-test (gtk floated-picture-drawn)
  ;; a real window, on a display of its own, showing an Org note with a red picture floated left and a blue one on
  ;; rows of its own below: each at its own size, the text beside the red one from its top row, the blue one
  ;; beginning right below the red one, at the same left edge
  (if (null (tuatara.io:find-program "gtk4-broadwayd"))
      (skip "gtk4-broadwayd is not installed here" (true nil))
      (with-test-directory (dir)
        (uiop:run-program (list "magick" "-size" "200x100" "xc:#ff0000" (namestring (merge-pathnames "red.png" dir))))
        (uiop:run-program (list "magick" "-size" "300x100" "xc:#0000ff" (namestring (merge-pathnames "blue.png" dir))))
        (let* ((note (put-file (merge-pathnames "note.org" dir)
                               (format nil "#+STARTUP: inlineimages~%#+ATTR_ORG: :float left~%[[file:red.png]]~%~
                                            one two three~%~%[[file:blue.png]]~%~%after~%")))
               (drawn (window-drawn dir note "floated")))
          (true drawn "the window wrote nothing")
          (false (getf drawn :error) "~a" (getf drawn :error))
          (false (getf drawn :timed-out) "the window gave up waiting")
          (destructuring-bind (&optional red-left red-top red-right red-bottom) (getf drawn :red)
            (destructuring-bind (&optional blue-left blue-top blue-right blue-bottom) (getf drawn :blue)
              (true red-left "no red shown")
              (true blue-left "no blue shown")
              (when (and red-left blue-left)
                (is equal '(200 100) (list (- red-right red-left -1) (- red-bottom red-top -1)))
                (is equal '(300 100) (list (- blue-right blue-left -1) (- blue-bottom blue-top -1)))
                (is = red-left blue-left)
                (true (<= 1 (- blue-top red-bottom) 2) "blue from ~d, red to ~d" blue-top red-bottom)
                (true (> (getf drawn :beside-top 0) 1) "the row beside the red picture's top shows nothing"))))
          (when (getf drawn :octets)
            (true (< (getf drawn :differing-much) (/ (getf drawn :octets) 200))
                  "~:d of ~:d octets far from painted whole"
                  (getf drawn :differing-much) (getf drawn :octets)))))))

(define-test (gtk two-windows)
  ;; two frames in two real windows on a display of their own: each shows its file, typing in one
  ;; does not paint the other for each key, a close button closes its window alone, C-x 5 2 opens another,
  ;; and the window given the keyboard is the frame worked in
  (if (null (tuatara.io:find-program "gtk4-broadwayd"))
      (skip "gtk4-broadwayd is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((a (put-file (merge-pathnames "a.txt" dir) (format nil "alpha~%")))
               (b (put-file (merge-pathnames "b.txt" dir) (format nil "beta~%")))
               (seen (run-on-broadway dir "two-windows.lisp" (namestring a) (namestring b) :out)))
          (true seen "the windows wrote nothing")
          (false (getf seen :error) "~a" (getf seen :error))
          (is = 2 (getf seen :windows))
          (dolist (step '(:first-shows-a :second-shows-b :typed-in-second :closed-one
                          :first-still-typed :opened-by-key :new-worked-in :focus-selects :frames-measured))
            (true (getf seen step) "~(~a~) in ~s" step seen))
          ;; twenty keys typed in the second window: the first painted again for work taken in alone
          (true (<= (getf seen :first-painted-while-typing 99) 4) "~d paints" (getf seen :first-painted-while-typing))
          (is = 1 (getf seen :frames-left))
          ;; every event a window handed on named its frame, a new window's from its first
          (is equal '() (getf seen :unnamed))))))

(define-test (gtk large-parts-painted)
  ;; a part drawn twice as large covers the row under it, its cursor as large, the text before it as it is
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (let ((frame (tuatara.frontend.gtk:make-frame
                         (vector (list "12* One" nil '((0 2 :region)) '((2 10 2 0)))
                                 (list "12" nil '((0 2 :region)) '((2 10 0 0)))
                                 (list "next" nil '())
                                 nil))))
             (setf (tuatara.frontend.gtk::frame-cursor-column frame) 2)
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore width height))
                (flet ((at (x y)
                         (tuatara.frontend.gtk:pixel-colour surface
                                                            (floor (* x cell-width))
                                                            (floor (* y cell-height)))))
                  ;; the columns before it as they are, on both rows
                  (is string= "#7533bd" (at 0.1 0.1))
                  (is string= "#7533bd" (at 0.1 1.1))
                  ;; the cursor two columns wide and two rows tall
                  (is string= "#dadada" (at 2.1 0.1))
                  (is string= "#dadada" (at 3.9 1.9))
                  (false (string= "#dadada" (at 4.1 1.5)))))
              frame :columns 10 :rows 4 :font "Adwaita Mono 11"))
           ;; half as large again, the row under it moved down half a row, a list of candidates over both
           (let ((frame (tuatara.frontend.gtk:make-frame
                         (vector (list "* One" nil '() '((0 10 3/2 0)) '((6 "ab" ((0 2 :region)))))
                                 (list "next" nil '() '((0 10 1 1/2)))
                                 (list "" nil '() '((0 10 0 0)))
                                 nil))))
             (setf (tuatara.frontend.gtk::frame-cursor-row frame) 1
                   (tuatara.frontend.gtk::frame-cursor-column frame) 0)
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore width height))
                (flet ((at (x y)
                         (tuatara.frontend.gtk:pixel-colour surface
                                                            (floor (* x cell-width))
                                                            (floor (* y cell-height)))))
                  ;; the cursor on the moved row: from a row and a half down, a row tall
                  (is string= "#dadada" (at 0.1 1.6))
                  (is string= "#dadada" (at 0.1 2.4))
                  (false (string= "#dadada" (at 0.1 1.4)))
                  (false (string= "#dadada" (at 0.1 2.6)))
                  ;; the list over the large row, in its own cells, only there
                  (is string= "#7533bd" (at 6.1 0.1))
                  (is string= "#7533bd" (at 7.9 0.9))
                  (false (string= "#7533bd" (at 8.5 0.5)))))
              frame :columns 10 :rows 4 :font "Adwaita Mono 11")))
      (tuatara.display:apply-theme theme))))

(define-test (gtk large-letters-keep-their-places)
  ;; letters drawn larger stand where they stood when a selection or the cursor's line splits them among colours
  ;; and so do letters of their own size moved down under a larger row
  (flet ((pixels (spans scale shift)
           (tuatara.frontend.gtk:call-with-painted-frame
            (lambda (surface width height cell-width cell-height)
              (declare (ignore height cell-width))
              (loop for y below (* 3 cell-height)
                    collect (loop for x below width collect (tuatara.frontend.gtk:pixel-colour surface x y))))
            (tuatara.frontend.gtk:make-frame
             (vector (list "** Second level here" nil spans `((0 30 ,scale ,shift))) nil nil nil))
            :columns 30 :rows 4 :font "Adwaita Mono 11")))
    (loop for (scale shift) in '((5/4 0) (3/2 0) (2 0) (1 1/4) (1 1/3) (11/10 1/4) (5/4 3/4))
          do (is equal (pixels '() scale shift) (pixels '((5 9 :default) (12 13 :default)) scale shift)
                 "Split letters moved at scale ~a, shift ~a" scale shift)))
  ;; a row of its own size under a larger one in the same face: its letters where they stand without it
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (flet ((after-region (scale)
                    (tuatara.frontend.gtk:call-with-painted-frame
                     (lambda (surface width height cell-width cell-height)
                       (declare (ignore width height))
                       (loop for y from (floor (* 3/2 cell-height)) below (floor (* 5/2 cell-height))
                             collect (loop for x from (* 10 cell-width) below (* 20 cell-width)
                                           collect (tuatara.frontend.gtk:pixel-colour surface x y))))
                     (tuatara.frontend.gtk:make-frame
                      (vector (list "* Big" nil '((0 5 :org-level-1)) `((0 30 ,scale 0)))
                              (list "** Second level here" nil '((0 20 :org-level-1) (5 9 :region)) '((0 30 1 1/2)))
                              nil nil))
                     :columns 30 :rows 4 :font "Adwaita Mono 11")))
             (is equal (after-region 3/2) (after-region 1))))
      (tuatara.display:apply-theme theme))))

(define-test (gtk joined-strokes)
  ;; a run of dashes a font joins into one line is painted as one: the same on
  ;; every pixel along it, no brighter seam where one piece meets the next; and
  ;; what the font joins with other signs (!==, <!--) stays as the font joins it
  (if (not (member "Cascadia Code NF" (tuatara.frontend.gtk::font-families :monospace t) :test #'string=))
      (skip "Cascadia Code NF, a font that joins dashes, is not installed" (true t))
      (flet ((painted (text)
               ;; the brightness of each pixel of the row TEXT is painted on
               (tuatara.frontend.gtk:call-with-painted-frame
                (lambda (surface width height cell-width cell-height)
                  (declare (ignore height))
                  (let ((pixels (make-array (list cell-height width))))
                    (dotimes (y cell-height (values pixels cell-width))
                      (dotimes (x width)
                        (setf (aref pixels y x)
                              (parse-integer (tuatara.frontend.gtk:pixel-colour surface x y)
                                             :start 1
                                             :end 3
                                             :radix 16))))))
                (tuatara.frontend.gtk:make-frame (vector (list text nil '()) nil))
                :columns 24 :rows 2 :font "Cascadia Code NF 12")))
        (multiple-value-bind (pixels cell-width) (painted "x ------------------ y")
          ;; the cells well inside the run, and the rows drawn on there
          (let* ((from (* 4 cell-width)) (to (* 18 cell-width))
                 (ground (aref pixels (1- (array-dimension pixels 0)) from))
                 (rows (loop for y below (array-dimension pixels 0)
                             when (loop for x from from below to thereis (/= ground (aref pixels y x)))
                               collect y)))
            (true rows)
            (dolist (y rows)
              (true (loop for x from from below to always (= (aref pixels y from) (aref pixels y x)))
                    "Row ~d of the line is not the same all along it" y))))
        (let ((joined (painted "a !== b <!-- c"))
              (as-shaped (let ((tuatara.frontend.gtk::*joining-characters* "")) (painted "a !== b <!-- c"))))
          (true (< (loop for i below (array-total-size joined)
                         count (> (abs (- (row-major-aref joined i) (row-major-aref as-shaped i))) 60))
                   5)
                "!== and <!-- are not joined as the font joins them")))))

(define-test (gtk mouse-cells)
  (let* ((f (tuatara.frontend.gtk:make-gtk-frontend))
         (data (tuatara.frontend.gtk::window-data f)))
    (setf (tuatara.frontend.gtk::cell-width f) 9 (tuatara.frontend.gtk::cell-height f) 20
          (tuatara.frontend.gtk::gtk-rows f) 30)
    ;; pixels to cells, above and left of the window too
    (is equal '(2 1) (multiple-value-list (tuatara.frontend.gtk::cell-at f 20d0 39.5d0)))
    (is equal '(-1 -1) (multiple-value-list (tuatara.frontend.gtk::cell-at f -0.5d0 -3d0)))
    ;; the modifiers held go with a mouse key
    (is equal '(:ctrl :shift :down-mouse-1)
        (tuatara.frontend:key-spec (tuatara.frontend.gtk::modifier-key :down-mouse-1 5 3 4)))
    ;; held past the text: above the window, or on its last two rows
    (true (tuatara.frontend.gtk::drag-past-text-p f -1))
    (false (tuatara.frontend.gtk::drag-past-text-p f 27))
    (true (tuatara.frontend.gtk::drag-past-text-p f 28))
    ;; a drag tells the editor each cell it reaches once, as GTK's gesture calls it;
    ;; the button let go is the click's to tell, once
    (flet ((call (callback x y)
             (sb-alien:alien-funcall (sb-alien:sap-alien callback
                                                         (function sb-alien:void
                                                                   sb-sys:system-area-pointer
                                                                   sb-alien:double
                                                                   sb-alien:double sb-sys:system-area-pointer))
                                     (tuatara.io:null-pointer) x y data)))
      (call (tuatara.gtk:callback tuatara.frontend.gtk::on-drag-begin) 20d0 45d0)
      (call (tuatara.gtk:callback tuatara.frontend.gtk::on-drag-update) 1d0 1d0)
      (call (tuatara.gtk:callback tuatara.frontend.gtk::on-drag-update) 30d0 0d0)
      (call (tuatara.gtk:callback tuatara.frontend.gtk::on-drag-update) 31d0 0d0)
      (call (tuatara.gtk:callback tuatara.frontend.gtk::on-drag-update) 30d0 20d0)
      (call (tuatara.gtk:callback tuatara.frontend.gtk::on-drag-end) 30d0 20d0))
    (is equal '((:drag-mouse-1 5 2) (:drag-mouse-1 5 3))
        (loop for event = (tuatara.frontend:queue-pop (tuatara.frontend.gtk::gtk-queue f) 0)
              while event
              collect (let ((key (tuatara.frontend:event-key event)))
                        (list (tuatara.frontend:key-base key) (tuatara.frontend:key-column key)
                              (tuatara.frontend:key-row key)))))
    (false (tuatara.frontend.gtk::gtk-drag f))
    (tuatara.frontend.gtk::forget-window f)))

(define-test (gtk window-title)
  ;; the name of the buffer the selected window shows, given each time it changes
  (is equal '("other - Tuatara" "*scratch* - Tuatara")
      (tuatara.frontend.fake:fake-titles (run-keys "l l C-x b o t h e r C-j l")))
  ;; *gui-title* makes it; one that fails gives the name alone
  (let ((tuatara-user:*gui-title* "~a"))
    (is equal '("*scratch*") (tuatara.frontend.fake:fake-titles (run-keys "l"))))
  (let ((tuatara-user:*gui-title* "~a ~a"))
    (is equal '("*scratch*") (tuatara.frontend.fake:fake-titles (run-keys "l"))))
  ;; a name's control characters and direction overrides are written out, as lists write them
  (let ((odd (list (tuatara.buffer:make-buffer (format nil "rtl~cgnp.exe" (code-char #x202E)) :text "")
                   (tuatara.buffer:make-buffer (format nil "esc~c[31mred~%x" (code-char 27)) :text ""))))
    (unwind-protect
         (is equal '("esc^[[31mred^Jx - Tuatara" "rtl<202E>gnp.exe - Tuatara" "*scratch* - Tuatara")
             (tuatara.frontend.fake:fake-titles
              (run-keys (mapcar (lambda (b) (lambda () (tuatara.command:show-buffer b))) odd))))
      (mapc #'tuatara.buffer:kill-buffer odd))))

(define-test (gtk composing)
  ;; what the input method is composing shows at the cursor, underlined, a bar after it
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (let ((frame (tuatara.frontend.gtk:make-frame (vector (list "" nil '()) nil))))
             (setf (tuatara.frontend.gtk::frame-cursor-column frame) 2)
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore width height))
                (flet ((pixel (x y) (tuatara.frontend.gtk:pixel-colour surface x y)))
                  (is string= "#dadada" (pixel (* 3 cell-width) 1))
                  (true (loop for y from (floor cell-height 2) below cell-height
                              thereis (string/= "#080d15" (pixel (+ (* 2 cell-width) 2) y))))
                  (true (loop for y from 0 below cell-height
                              always (string= "#080d15" (pixel (+ (* 4 cell-width) 2) y))))))
              frame :columns 6 :rows 2 :font "Adwaita Mono 11" :preedit (string (code-char #xb4)))
             ;; nothing composed: the block cursor on its cell
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore width height cell-height))
                (is string= "#dadada" (tuatara.frontend.gtk:pixel-colour surface (1+ (* 2 cell-width)) 1)))
              frame :columns 6 :rows 2 :font "Adwaita Mono 11")))
      (tuatara.display:apply-theme theme))))

(define-modal-test (gtk font-size)
  ;; C-x C-+ and C-x C-= a step larger, C-x C-- smaller, C-x C-0 back; + = - 0 go on alone
  (flet ((steps (keys &optional (font-steps 0) leader-key)
           (let ((f (tuatara.frontend.fake:make-fake-frontend :font-steps font-steps)))
             (tuatara.frontend.fake:fake-queue-keys f keys)
             (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
               (when scratch (tuatara.buffer:kill-buffer scratch)))
             (tuatara.buffer:make-buffer "*scratch*" :text "hello")
             (tuatara.command:run-editor
              f :setup (lambda (editor)
                         (tuatara.defaults:install-defaults editor)
                         (when leader-key
                           (tuatara-user:bind-key leader-key 'tuatara.defaults::text-scale-adjust :map :leader)))
                :buffer (tuatara.buffer:find-buffer "*scratch*"))
             f)))
    (is = 1 (tuatara.frontend.fake:fake-font-steps (steps "C-x C-+")))
    (is = 3 (tuatara.frontend.fake:fake-font-steps (steps "C-x C-= + =")))
    (is = 1 (tuatara.frontend.fake:fake-font-steps (steps "C-x C-+ + -")))
    (is = -2 (tuatara.frontend.fake:fake-font-steps (steps "C-x C-- -")))
    (is = 0 (tuatara.frontend.fake:fake-font-steps (steps "C-x C-+ + 0")))
    (is = 0 (tuatara.frontend.fake:fake-font-steps (steps "C-x C-0" 4)))
    ;; a prefix argument takes that many steps; the steps stay within their limits
    (is = 3 (tuatara.frontend.fake:fake-font-steps (steps "C-u 3 C-x C-+")))
    (is = 15 (tuatara.frontend.fake:fake-font-steps (steps "C-u 5 0 C-x C-+")))
    (is = -5 (tuatara.frontend.fake:fake-font-steps (steps "C-u 9 C-x C--")))
    ;; after another key, + types again (in INSERT state here)
    (let ((f (steps "C-x C-+ i +")))
      (is = 1 (tuatara.frontend.fake:fake-font-steps f))
      (true (search "+hello" (tuatara.frontend.fake:fake-screen-line f 0))))
    ;; bound to another key (SPC S) it goes larger, and + - 0 go on after it
    (is = 1 (tuatara.frontend.fake:fake-font-steps (steps "SPC S" 0 "S")))
    (is = 3 (tuatara.frontend.fake:fake-font-steps (steps "SPC S + +" 0 "S")))
    ;; the wheel with Ctrl held
    (is = 1 (tuatara.frontend.fake:fake-font-steps (steps "C-<wheel-up> C-<wheel-up> C-<wheel-down>")))
    (true (search "Font size +2" (tuatara.frontend.fake:fake-screen-line (steps "C-x C-+ +") 7)))
    ;; a terminal's font is its own
    (let ((f (steps "C-x C-+" nil)))
      (false (tuatara.frontend.fake:fake-font-steps f))
      (true (search "the terminal's to change" (tuatara.frontend.fake:fake-screen-line f 7)))))
  ;; the window's fonts at a scale: larger cells
  (tuatara.frontend.gtk::load-gtk-libraries)
  (flet ((cell (scale)
           (let ((f (tuatara.frontend.gtk:make-gtk-frontend)))
             (setf (tuatara.frontend.gtk::gtk-fonts f) (tuatara.frontend.gtk::make-fonts "Adwaita Mono 11" scale))
             (tuatara.frontend.gtk::measure-font f)
             (prog1 (list (tuatara.frontend.gtk::cell-width f) (tuatara.frontend.gtk::cell-height f))
               (loop for font across (tuatara.frontend.gtk::gtk-fonts f)
                     do (tuatara.gtk:pango-font-description-free font))))))
    (let ((plain (cell 1)) (larger (cell (expt 1.2d0 3))) (smaller (cell (expt 1.2d0 -2))))
      (true (every #'< plain larger))
      (true (every #'> plain smaller)))))

(define-test (gtk font-grows-while-the-rows-fit)
  ;; made larger, the font stops where the window still shows a row of text, its mode line and the echo row
  (tuatara.frontend.gtk::load-gtk-libraries)
  (let ((f (tuatara.frontend.gtk:make-gtk-frontend)))
    (setf (tuatara.frontend.gtk::cell-height f) 20 (tuatara.frontend.gtk::gtk-room-height f) 700)
    (is = 13 (setf (tuatara.frontend:frontend-font-steps f) 15))
    (is = 13 (tuatara.frontend:frontend-font-steps f))
    (is = 5 (setf (tuatara.frontend:frontend-font-steps f) 5))
    (is = -3 (setf (tuatara.frontend:frontend-font-steps f) -3))))

(define-test (gtk choosing-a-font)
  (flet ((chosen (keys &key (fonts '("Adwaita Mono" "Cascadia Code NF")))
           (let ((f (tuatara.frontend.fake:make-fake-frontend :font-steps (and fonts 0) :fonts fonts)))
             (tuatara.frontend.fake:fake-queue-keys f keys)
             (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
               (when scratch (tuatara.buffer:kill-buffer scratch)))
             (tuatara.buffer:make-buffer "*scratch*" :text "hello")
             (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                           :buffer (tuatara.buffer:find-buffer "*scratch*"))
             (values (tuatara.frontend.fake:fake-font f) (tuatara.frontend.fake:fake-screen-line f 7)))))
    ;; a family chosen from the list keeps the size in use
    (multiple-value-bind (font echo) (chosen "M-x s e t - f r a m e - f o n t RET c a s c RET")
      (is string= "Cascadia Code NF 11" font)
      (true (search "Font Cascadia Code NF 11" echo)))
    ;; a style and size typed after the family
    (is string= "Cascadia Code NF Medium 12"
        (chosen "M-x s e t - f r a m e - f o n t RET C a s c a d i a SPC C o d e SPC N F SPC M e d i u m SPC 1 2 RET"))
    ;; a family not installed changes nothing
    (multiple-value-bind (font echo) (chosen "M-x s e t - f r a m e - f o n t RET N o s u c h SPC 1 2 RET")
      (false font)
      (true (search "No font family Nosuch is installed" echo)))
    ;; a terminal's font is its own
    (multiple-value-bind (font echo) (chosen "M-x s e t - f r a m e - f o n t RET" :fonts nil)
      (false font)
      (true (search "the terminal's to change" echo))))
  ;; the window lists the fixed-width families installed
  (let ((families (tuatara.frontend.gtk::font-families :monospace t)))
    (true (plusp (length families)))
    (true (every #'stringp families))))

(defclass slow-fonts-frontend (tuatara.frontend.fake:fake-frontend)
  ((listed-on :initform nil :accessor fonts-listed-on
              :documentation "The thread FRONTEND-FONTS was called on."))
  (:documentation "A fake window that takes a while to list its fonts."))

(defmethod tuatara.frontend:frontend-fonts ((f slow-fonts-frontend))
  (setf (fonts-listed-on f) sb-thread:*current-thread*)
  (sleep 1)
  (call-next-method))

(define-test (gtk fonts-listed-off-the-editor-thread)
  ;; the window's fonts are listed on a thread of their own: the editor reads \`C-g' meanwhile and gives up
  (let ((f (make-instance 'slow-fonts-frontend :font-steps 0 :fonts '("Adwaita Mono"))))
    (tuatara.frontend.fake:fake-queue-keys f "M-x s e t - f r a m e - f o n t RET C-g")
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "hello")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                  :buffer (tuatara.buffer:find-buffer "*scratch*"))
    (true (fonts-listed-on f))
    (false (eq sb-thread:*current-thread* (fonts-listed-on f)))
    (false (tuatara.frontend.fake:fake-font f))))

(define-test (gtk dropped-files)
  ;; files as a file manager drops them: a GdkFileList in a GValue
  (tuatara.gtk:load-gtk-libraries)
  (let* ((paths (list "/tmp/a file.txt" "/tmp/b [2].txt" (format nil "/tmp/caf~a.txt" (code-char #xe9))))
         (files (let ((list (tuatara.io:null-pointer)))
                  (dolist (path (reverse paths) list)
                    (setf list (tuatara.runtime:foreign-call "g_slist_prepend" sb-sys:system-area-pointer
                                                             (sb-sys:system-area-pointer list)
                                                             (sb-sys:system-area-pointer
                                                              (tuatara.runtime:foreign-call "g_file_new_for_path"
                                                                                            sb-sys:system-area-pointer
                                                                                            ((sb-alien:c-string :external-format :utf-8) path))))))))
         (file-list (tuatara.runtime:foreign-call "gdk_file_list_new_from_list" sb-sys:system-area-pointer
                                                  (sb-sys:system-area-pointer files))))
    (tuatara.runtime:foreign-call "g_slist_free_full" sb-alien:void (sb-sys:system-area-pointer files)
                                  (sb-sys:system-area-pointer (sb-sys:int-sap (sb-sys:find-dynamic-foreign-symbol-address "g_object_unref"))))
    ;; a GValue: 24 octets, all zero before it is set up
    (tuatara.io:with-foreign-memory (value 24 :zero t)
      (tuatara.runtime:foreign-call "g_value_init" sb-sys:system-area-pointer (sb-sys:system-area-pointer value)
                                    (sb-alien:unsigned-long (tuatara.gtk:gdk-file-list-get-type)))
      (tuatara.runtime:foreign-call "g_value_take_boxed" sb-alien:void (sb-sys:system-area-pointer value)
                                    (sb-sys:system-area-pointer file-list))
      (unwind-protect
           (progn
             (is equal paths (tuatara.frontend.gtk::dropped-paths value))
             ;; the drop reaches the editor as an event with the cell it was on
             (let* ((f (tuatara.frontend.gtk:make-gtk-frontend))
                    (data (tuatara.frontend.gtk::window-data f)))
               (setf (tuatara.frontend.gtk::cell-width f) 9 (tuatara.frontend.gtk::cell-height f) 20)
               (true (sb-alien:alien-funcall
                      (sb-alien:sap-alien (tuatara.gtk:callback tuatara.frontend.gtk::on-drop)
                                          (function (sb-alien:boolean 32)
                                                    sb-sys:system-area-pointer
                                                    sb-sys:system-area-pointer
                                                    sb-alien:double sb-alien:double sb-sys:system-area-pointer))
                      (tuatara.io:null-pointer) value 20d0 45d0 data))
               (tuatara.frontend.gtk::forget-window f)
               (let ((event (tuatara.frontend:queue-pop (tuatara.frontend.gtk::gtk-queue f) 0)))
                 (is eq :drop (tuatara.frontend:event-kind event))
                 (is equal paths (tuatara.frontend:event-files event))
                 (is = 2 (tuatara.frontend:event-column event))
                 (is = 2 (tuatara.frontend:event-row event)))))
        (tuatara.runtime:foreign-call "g_value_unset" sb-alien:void (sb-sys:system-area-pointer value))))))

(define-test (gtk pointer-shapes)
  ;; the text beam over text, an arrow on mode lines and the echo row, a sideways arrow on a divider
  (let ((frame (tuatara.frontend.gtk:make-frame
                (vector (list "left text          │right" nil '((19 20 :vertical-border)))
                        (list " TUATARA[N] a.txt    TUATARA[N] b.txt" nil '((0 20 :mode-line)
                                                                            (20 40 :mode-line-inactive)))
                        (list "whole text" nil '())
                        (list " TUATARA[N] c.txt" :mode-line '())
                        (list "Wrote c.txt" nil '())))))
    (flet ((shape (column row) (tuatara.frontend.gtk::pointer-shape-at frame column row)))
      (is string= "text" (shape 3 0))
      (is string= "col-resize" (shape 19 0))
      (is string= "text" (shape 25 0))
      (is string= "default" (shape 5 1))
      (is string= "default" (shape 25 1))
      (is string= "text" (shape 2 2))
      (is string= "default" (shape 2 3))
      (is string= "default" (shape 2 4))
      (is string= "default" (shape 2 9)))))

(define-test (gtk filling-the-screen)
  (flet ((toggled (keys &optional (window t))
           (let ((f (tuatara.frontend.fake:make-fake-frontend :font-steps (and window 0))))
             (tuatara.frontend.fake:fake-queue-keys f keys)
             (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
               (when scratch (tuatara.buffer:kill-buffer scratch)))
             (tuatara.buffer:make-buffer "*scratch*" :text "hello")
             (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                           :buffer (tuatara.buffer:find-buffer "*scratch*"))
             (values (tuatara.frontend.fake:fake-toggles f) (tuatara.frontend.fake:fake-screen-line f 7)))))
    ;; F11 fills the screen and back, M-F10 maximises
    (is equal '(:fullscreen) (toggled "<f11>"))
    (is equal '(:fullscreen :fullscreen) (toggled "<f11> <f11>"))
    (is equal '(:maximised) (toggled "M-<f10>"))
    ;; a terminal's window is the terminal's
    (multiple-value-bind (toggles echo) (toggled "<f11>" nil)
      (false toggles)
      (true (search "the terminal's to change" echo))))
  ;; the window turns states on GTK's thread, as asked, in order
  (let* ((f (tuatara.frontend.gtk:make-gtk-frontend))
         (data (tuatara.frontend.gtk::window-data f)))
    (tuatara.gtk:load-gtk-libraries)
    (setf (tuatara.frontend.gtk::gtk-toggles f) '())
    (true (tuatara.frontend:frontend-toggle-state f :fullscreen))
    (true (tuatara.frontend:frontend-toggle-state f :fullscreen))
    (is equal '(:fullscreen :fullscreen) (tuatara.frontend.gtk::gtk-toggles f))
    ;; with no window open yet, the requests are taken and nothing is done
    (false (sb-alien:alien-funcall
            (sb-alien:sap-alien (tuatara.gtk:callback tuatara.frontend.gtk::on-toggle-state)
                                (function (sb-alien:boolean 32) sb-sys:system-area-pointer))
            data))
    (false (tuatara.frontend.gtk::gtk-toggles f))
    (tuatara.frontend.gtk::forget-window f)))

(define-test (gtk short-waits)
  ;; reading the event queue with a time limit, again and again, as the editor does between keys
  (let ((queue (tuatara.frontend:make-event-queue)))
    (true (loop repeat 500 always (null (tuatara.frontend:queue-pop queue 0.0001))))
    (tuatara.frontend:queue-push queue (tuatara.frontend:make-event :close))
    (is eq :close (tuatara.frontend:event-kind (tuatara.frontend:queue-pop queue 0.0001)))))

(define-test (gtk window-events-mid-question)
  (let* ((path (format nil "~amid-~d.txt" (namestring (uiop:temporary-directory)) (random 1000000)))
         (other (concatenate 'string path ".other")))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string "abc" o))
    (with-open-file (o other :direction :output :if-exists :supersede) (write-string "dropped" o))
    (unwind-protect
         (flet ((session (keys event)
                  ;; KEYS on a changed file's buffer, then EVENT, from the window
                  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 60)))
                    (dolist (name (list (file-namestring path) (file-namestring other)))
                      (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))
                    (tuatara.frontend.fake:fake-queue-keys f (format nil "i x C-g ~a" keys))
                    (tuatara.frontend.fake:fake-queue-event f event)
                    (values f
                            (tuatara.command:run-editor f
                                                        :files (list path)
                                                        :setup #'tuatara.defaults:install-defaults)))))
           ;; the window closed while the minibuffer asks: the question ends, and closing asks its own
           (dolist (keys '("C-x C-f" "C-s a" "C-x k RET"))
             (let ((f (session keys (tuatara.frontend:make-event :close))))
               (true (tuatara.frontend.fake:fake-frame-showing f "Modified buffers exist; exit anyway?"))))
           ;; files dropped while it asks: the question ends, the file is opened
           (multiple-value-bind (f editor)
               (session "C-x C-f" (tuatara.frontend:make-event :drop :files (list other) :column 1 :row 1))
             (declare (ignore f))
             (is string= "dropped" (session-text editor))))
      (dolist (name (list (file-namestring path) (file-namestring other)))
        (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file other)))))

(define-modal-test (gtk list-of-fonts)
  (let ((faces '(("Adwaita Mono" t (("Bold" "Adwaita Mono Bold 11") ("Regular" "Adwaita Mono 11")))
                 ("Cascadia Code NF" t (("Light" "Cascadia Code NF Light 11")))
                 ("Cantarell" nil (("Regular" "Cantarell 11"))))))
    (flet ((session (keys &key (font-faces faces))
             (let ((f (tuatara.frontend.fake:make-fake-frontend :width 120 :height 20 :font-steps (and font-faces 0)
                                                                :fonts (mapcar #'first font-faces)
                                                                :font-faces font-faces)))
               (tuatara.frontend.fake:fake-queue-keys f keys)
               (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
                 (when scratch (tuatara.buffer:kill-buffer scratch)))
               (let ((list (tuatara.buffer:find-buffer "*Fonts*"))) (when list (tuatara.buffer:kill-buffer list)))
               (tuatara.buffer:make-buffer "*scratch*" :text "hello")
               (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                       :buffer (tuatara.buffer:find-buffer "*scratch*"))))))
      ;; the fixed-width families first, each variant's line for the init file and a sample in its font
      (multiple-value-bind (f editor) (session "M-x f o n t - l i s t RET")
        (let ((text (session-text editor)))
          (true (search "Fonts with every character the same width, for the window: 2 families" text))
          (true (search "(setf *gui-font* \"Cascadia Code NF Light 11\")" text))
          (true (< (search "Adwaita Mono" text) (search "Cascadia Code NF" text) (search "Cantarell" text)))
          (true (search "Other fonts" text)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Adwaita Mono Bold 11" "The quick brown fox"))
        ;; the samples are in faces that name their fonts
        (true (some (lambda (spans) (some (lambda (span) (let ((face (third span)))
                                                             (and (keywordp face)
                                                                  (equal "Adwaita Mono Bold 11"
                                                                         (tuatara.display:face-attribute face :font)))))
                                          spans))
                    (coerce (tuatara.frontend.fake:fake-spans f) 'list))))
      ;; RET on a variant shows the window in it; w copies its line
      (let ((f (session "M-x f o n t - l i s t RET j j j RET")))
        (is string= "Adwaita Mono Bold 11" (tuatara.frontend.fake:fake-font f)))
      (multiple-value-bind (f editor) (session "M-x f o n t - l i s t RET j j j j j j w")
        (declare (ignore f))
        (is string= "(setf *gui-font* \"Cascadia Code NF Light 11\")"
            (first (tuatara.command:editor-kill-ring editor))))
      ;; the modal keys: V copies a whole line, v and g h / g l a part, m i " the font's name
      (multiple-value-bind (f editor) (session "M-x f o n t - l i s t RET j j j V y")
        (declare (ignore f))
        (let ((kill (first (tuatara.command:editor-kill-ring editor))))
          (is eql 0 (search "  (setf *gui-font* \"Adwaita Mono Bold 11\")" kill))
          (is eql #\Newline (char kill (1- (length kill))))))
      (multiple-value-bind (f editor) (session "M-x f o n t - l i s t RET j j j g l v g h y")
        (declare (ignore f))
        (true (search "(setf *gui-font* \"Adwaita Mono Bold 11\")" (first (tuatara.command:editor-kill-ring editor)))))
      (multiple-value-bind (f editor)
          (session "M-x f o n t - l i s t RET j j j l l l l l l l l l l l l l l l l l l l l l l m i \" y")
        (declare (ignore f))
        (is string= "Adwaita Mono Bold 11" (first (tuatara.command:editor-kill-ring editor))))
      ;; g e, G and g g move; g begins those motions, so g r lists the fonts again
      (multiple-value-bind (f editor) (session "M-x f o n t - l i s t RET g e")
        (declare (ignore f))
        (is = (length (tuatara.text:string-octets (session-text editor))) (session-point editor)))
      (multiple-value-bind (f editor) (session "M-x f o n t - l i s t RET g e g g")
        (declare (ignore f))
        (is = 0 (session-point editor)))
      ;; C-x / s narrows it as every list: the fonts whose lines match, each under its family; g r keeps the filter
      (dolist (keys '("M-x f o n t - l i s t RET C-x / s c a s c RET"
                      "M-x f o n t - l i s t RET C-x / s c a s c RET g r"))
        (multiple-value-bind (f editor) (session keys)
          (declare (ignore f))
          (let ((text (session-text editor)))
            (true (search "Filtered by casc: 1 of 4 fonts" text))
            (true (search "Cascadia Code NF" text))
            (false (search "Adwaita" text))
            (false (search "Cantarell" text)))))
      (false (gethash 'tuatara.defaults::font-list-filter tuatara.command:*commands*))
      ;; a terminal lists none
      (true (search "a terminal's font is its own"
                    (tuatara.frontend.fake:fake-screen-line (session "M-x f o n t - l i s t RET"
                                                                     :font-faces nil) 19))))))

(define-test (gtk faces-with-fonts)
  ;; a face may name a font; a theme changed after keeps it; the window paints its text in it
  (tuatara.display:register-face :test-sample "A test face with a font of its own." (list :font "Adwaita Mono Bold"))
  (is string= "Adwaita Mono Bold" (tuatara.display:face-attribute :test-sample :font))
  (is string= "Adwaita Mono Bold" (tuatara.display:resolve-face-font '(:region :test-sample)))
  (false (tuatara.display:resolve-face-font :region))
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (is string= "Adwaita Mono Bold" (tuatara.display:face-attribute :test-sample :font))
           ;; bold by its font, not by a flag: the sample's strokes are wider than the plain text's
           (flet ((ink (spec)
                    (let ((frame (tuatara.frontend.gtk:make-frame (vector (list "IIIIIIII" nil (and spec '((0 8 :test-sample)))) nil))))
                      (setf (tuatara.frontend.gtk::frame-cursor-row frame) 1)
                      (tuatara.frontend.gtk:call-with-painted-frame
                       (lambda (surface width height cell-width cell-height)
                         (declare (ignore height))
                         (loop for x below (min width (* 8 cell-width))
                               sum (loop for y below cell-height
                                         count (string/= "#080d15" (tuatara.frontend.gtk:pixel-colour surface x y)))))
                       frame :columns 10 :rows 2 :font "Adwaita Mono 11"))))
             (true (> (ink t) (ink nil)))))
      (tuatara.display:apply-theme theme))))

(define-test (gtk padding)
  ;; the ground all round the grid, the grid moved in by the padding
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (let ((frame (tuatara.frontend.gtk:make-frame (vector (list "hello" nil '()) nil))))
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (is = (+ (* 6 cell-width) 16) width)
                (is = (+ (* 2 cell-height) 16) height)
                (flet ((at (x y) (tuatara.frontend.gtk:pixel-colour surface x y)))
                  ;; the room round it: the ground
                  (is string= "#080d15" (at 3 3))
                  (is string= "#080d15" (at (- width 3) (- height 3)))
                  ;; the block cursor at the top left of the grid, 8 pixels in
                  (is string= "#dadada" (at 9 9))))
              frame :columns 6 :rows 2 :font "Adwaita Mono 11" :padding 8)))
      (tuatara.display:apply-theme theme)))
  ;; the mouse's cells count from inside the padding
  (let ((f (tuatara.frontend.gtk:make-gtk-frontend)))
    (setf (tuatara.frontend.gtk::cell-width f) 9 (tuatara.frontend.gtk::cell-height f) 20
          (tuatara.frontend.gtk::gtk-padding f) '(8 8 8 8))
    (is equal '(2 1) (multiple-value-list (tuatara.frontend.gtk::cell-at f 28d0 47.5d0)))
    (is equal '(-1 -1) (multiple-value-list (tuatara.frontend.gtk::cell-at f 3d0 3d0)))
    ;; each side its own: 4 above, 12 at the left
    (setf (tuatara.frontend.gtk::gtk-padding f) '(4 6 4 12))
    (is equal '(0 0) (multiple-value-list (tuatara.frontend.gtk::cell-at f 12d0 4d0))))
  ;; the padding options: one number for all four sides; two for the top and bottom,
  ;; then the right and left; three for the top, the right and left, the bottom; four
  ;; clockwise from the top
  (flet ((sides (value) (multiple-value-list (tuatara.frontend:padding-sides value))))
    (is equal '(8 8 8 8) (sides 8))
    (is equal '(4 12 4 12) (sides '(4 12)))
    (is equal '(1 2 3 2) (sides '(1 2 3)))
    (is equal '(1 2 3 4) (sides '(1 2 3 4)))
    (is equal '(0 0 0 0) (sides nil))
    (is equal '(0 0 0 0) (sides -3))
    (is equal '(2 2 2 2) (sides 2.4)))
  ;; a window painted with its sides unlike: the grid 4 down and 12 across
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (let ((frame (tuatara.frontend.gtk:make-frame (vector (list "hi" nil '()) nil))))
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore height))
                (is = (+ (* 4 cell-width) 12 6) width)
                (is string= "#080d15" (tuatara.frontend.gtk:pixel-colour surface 10 6))
                (is string= "#dadada" (tuatara.frontend.gtk:pixel-colour surface 13 5))
                (true (plusp cell-height)))
              frame :columns 4 :rows 2 :font "Adwaita Mono 11" :padding '(4 6 4 12))))
      (tuatara.display:apply-theme theme))))

(define-test (gtk icon)
  ;; the program's icon, in the source tree, laid out as an icon theme: every size, as pictures
  (let ((icons (merge-pathnames "data/icons/" tuatara.runtime:*source-tree*)))
    (true (member icons (tuatara.frontend.gtk::icon-directories) :test #'equal))
    ;; no drawing beside them, which a desktop would take before them at any size
    (false (probe-file (merge-pathnames "hicolor/scalable/apps/tuatara.svg" icons)))
    (dolist (size '(16 24 32 48 64 128 256 512))
      (let ((png (merge-pathnames (format nil "hicolor/~dx~d/apps/tuatara.png" size size) icons)))
        (true (probe-file png) "~dx~d" size size)
        ;; its size as it says, and nothing in it but the picture: no text or other chunks
        (with-open-file (in png :element-type '(unsigned-byte 8))
          (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
            (read-sequence octets in)
            (flet ((u32 (at) (tuatara.io:octets-integer octets at 4 :big-endian t)))
              (is = size (u32 16))
              (is = size (u32 20))
              (let ((kinds (loop with at = 8
                                 while (< at (length octets))
                                 collect (tuatara.text:octets-latin-1 octets :start (+ at 4) :end (+ at 8))
                                 do (incf at (+ 12 (u32 at))))))
                (true (subsetp kinds '("IHDR" "PLTE" "tRNS" "IDAT" "IEND") :test #'string=) "~a" kinds)))))))))


(define-test (gtk marks-drawn-on-their-letters)
  ;; a letter and its accent are drawn together in the letter's cell, as the letter
  ;; written with its accent is; the cursor block on a wide character covers both its cells
  (flet ((pixels (text &key (column 0))
           (let ((frame (tuatara.frontend.gtk:make-frame (vector (list text nil '()) nil nil))))
             (setf (tuatara.frontend.gtk::frame-cursor-column frame) column)
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore height cell-height))
                (values (loop for y below 20
                              collect (loop for x below width collect (tuatara.frontend.gtk:pixel-colour surface x y)))
                        surface cell-width))
              frame :columns 10 :rows 3 :font "Adwaita Mono 11"))))
    (is equal (pixels (format nil "ae~cx" (code-char #x301)) :column 9)
        (pixels (format nil "a~cx" (code-char #xE9)) :column 9))
    (is equal (pixels (format nil "e~c~cx" (code-char #x323) (code-char #x302)) :column 9)
        (pixels (format nil "~cx" (code-char #x1EC7)) :column 9)
        "Two marks on one letter")
    (let ((theme tuatara.display::*current-theme*))
      (unwind-protect
           (progn
             (tuatara.display:apply-theme 'ao-dark)
             (tuatara.frontend.gtk:call-with-painted-frame
              (lambda (surface width height cell-width cell-height)
                (declare (ignore width height cell-height))
                (is string= "#dadada" (tuatara.frontend.gtk:pixel-colour surface 1 1))
                (is string= "#dadada" (tuatara.frontend.gtk:pixel-colour surface (- (* 2 cell-width) 2) 1))
                (is string= "#080d15" (tuatara.frontend.gtk:pixel-colour surface (+ (* 2 cell-width) 1) 1)))
              (tuatara.frontend.gtk:make-frame (vector (list (format nil "~cx" (code-char #x6F22)) nil '()) nil nil))
              :columns 10 :rows 3 :font "Adwaita Mono 11"))
        (tuatara.display:apply-theme theme)))))

(define-test (gtk measuring-directory-private-and-new)
  ;; the directory a measuring copy keeps its init file and state in is made new, for you
  ;; alone; one already there under its name, made by someone else, is never taken
  (let* ((name (format nil "test-~a" (tuatara.io:random-name)))
         (directory (tuatara.frontend.gtk::measuring-directory name)))
    (unwind-protect
         (progn
           (is = #o700 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring directory)))))
           (fail (tuatara.frontend.gtk::measuring-directory name) 'tuatara.io:file-system-error))
      (tuatara.io:delete-tree (uiop:native-namestring directory))))
  (let ((one (tuatara.frontend.gtk::measuring-directory))
        (two (tuatara.frontend.gtk::measuring-directory)))
    (unwind-protect (isnt equal one two)
      (tuatara.io:delete-tree (uiop:native-namestring one))
      (tuatara.io:delete-tree (uiop:native-namestring two)))))

(define-test (gtk renderer-setting-leaves-the-environment-alone)
  ;; run from the sources, which cannot start again, a renderer set in the init file
  ;; changes nothing of the environment GTK's thread reads meanwhile
  (let ((f (tuatara.frontend.gtk::make-gtk-frontend))
        (before (uiop:getenv "GSK_RENDERER")))
    (unwind-protect
         (let ((tuatara.io:*saved-program* nil)
               (tuatara.frontend.gtk::*window-renderer* "gl")
               (tuatara-user:*gui-renderer* :vulkan))
           (sb-posix:setenv "GSK_RENDERER" "made-up" 1)
           (tuatara.frontend.gtk::follow-renderer-setting f)
           (is equal "made-up" (uiop:getenv "GSK_RENDERER"))
           (true (tuatara.frontend.gtk::gtk-renderer-checked f)))
      (if before (sb-posix:setenv "GSK_RENDERER" before 1) (sb-posix:unsetenv "GSK_RENDERER")))))

(define-test (gtk modifiers-read-one-way)
  ;; keys and the mouse take Control, Meta and Shift from GDK's state alike
  (is equal '(t nil t) (multiple-value-list (tuatara.frontend.gtk::gdk-modifiers (logior 4 1))))
  (is equal '(nil t nil) (multiple-value-list (tuatara.frontend.gtk::gdk-modifiers (ash 1 28))))
  (let ((key (tuatara.frontend.gtk::modifier-key :mouse-1 (logior 4 8) 3 4)))
    (true (tuatara.frontend:key-ctrl key))
    (true (tuatara.frontend:key-meta key))
    (false (tuatara.frontend:key-shift key)))
  (false (search "with-float-traps-masked"
                 (gtk-frontend-source))))

(define-test (gtk huge-scrolls-turn-a-screenful)
  ;; a device that says it scrolled a million lines turns the wheel a screenful of times at most
  (let ((f (tuatara.frontend.gtk::make-gtk-frontend)))
    (setf (tuatara.frontend.gtk::gtk-rows f) 40)
    (is = 40 (tuatara.frontend.gtk::wheel-turns f 1d6))
    (is = -40 (tuatara.frontend.gtk::wheel-turns f -1d6))
    ;; parts of a turn add up
    (is = 0 (tuatara.frontend.gtk::wheel-turns f 0.5d0))
    (is = 1 (tuatara.frontend.gtk::wheel-turns f 0.75d0))
    (is = -1 (tuatara.frontend.gtk::wheel-turns f -1.25d0))))

(define-test (gtk paste-waits-a-second-at-most)
  ;; a watched clipboard whose change is not in after a second: the text it had, at once
  (with-painting-window (f)
    (setf (tuatara.frontend.gtk::gtk-clipboard-reads f) 1
          (tuatara.frontend.gtk::gtk-clipboard-kept f) "kept before")
    (let ((start (get-internal-real-time)))
      (is equal "kept before" (tuatara.frontend.gtk::read-clipboard-now f nil))
      (true (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 5)))))

(define-test (gtk pictures-let-go-of-when-drawn-the-other-way)
  ;; the surfaces painting whole kept are let go of once the card draws the rows
  (with-painting-window (f)
    (let ((surfaces (tuatara.frontend.gtk::gtk-surfaces f)))
      (setf (gethash :a-picture surfaces) (tuatara.frontend.gtk::cairo-image-surface-create 0 16 16))
      (tuatara.frontend.gtk::let-go-of-pictures f t)
      (is = 0 (hash-table-count surfaces)))))

(define-test (gtk every-call-documented)
  ;; the calls into GTK and the libraries under it say what they do, the private ones too
  (do-symbols (symbol :tuatara.gtk)
    (when (and (eq (symbol-package symbol) (find-package :tuatara.gtk)) (fboundp symbol))
      (true (documentation symbol 'function) "~a has no docstring" symbol))))

(define-test (gtk font-sizes-and-families-one-way)
  ;; a font named without a size is taken at 11 points, by one rule, and the families
  ;; installed are walked one way for the list of fonts and for their variants
  (tuatara.frontend.gtk::load-gtk-libraries)
  (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
    (flet ((size (name)
             (let ((description (tuatara.gtk:pango-font-description-from-string name)))
               (unwind-protect (tuatara.frontend.gtk::font-description-size description)
                 (tuatara.gtk:pango-font-description-free description)))))
      (is = (* 11 1024) (size "Monospace"))
      (is = (* 14 1024) (size "Monospace 14")))
    (let ((count 0))
      (tuatara.frontend.gtk::call-with-font-families (lambda (family) (declare (ignore family)) (incf count)))
      (is = count (length (tuatara.frontend.gtk::font-families)))))
  (is = 1 (cl-ppcre:count-matches "11 \\+pango-scale\\+"
                         (gtk-frontend-source))))

(define-test (gtk pictures-made-ready-quickly)
  ;; a picture painted whole is made ready for cairo at a few milliseconds a million
  ;; pixels, whatever its size, its colours multiplied by its alpha
  (tuatara.frontend.gtk::load-gtk-libraries)
  (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
    (flet ((picture (side)
             (let ((pixels (make-array (* side side 4) :element-type '(unsigned-byte 8))))
               (dotimes (i (length pixels)) (setf (aref pixels i) (mod (* i 7) 256)))
               (tuatara.frontend:make-image side side 4 pixels)))
           (ms-per-megapixel (image)
             (loop repeat 3
                   minimize (let ((start (thread-cpu-time))
                                  (surface (tuatara.frontend.gtk::image-surface image)))
                              (prog1 (/ (* 1000 (- (thread-cpu-time) start))
                                        internal-time-units-per-second
                                        (/ (* (tuatara.frontend:image-width image)
                                              (tuatara.frontend:image-height image)) 1d6))
                                (tuatara.frontend.gtk::cairo-surface-destroy surface))))))
      (dolist (side '(1000 2000))
        (let ((ms (ms-per-megapixel (picture side))))
          (true (< ms 10) "~d by ~d: ~,1f ms a million pixels" side side ms)))
      (let* ((image (picture 2))
             (surface (tuatara.frontend.gtk::image-surface image))
             (data (tuatara.frontend.gtk::cairo-image-surface-get-data surface))
             (pixels (tuatara.frontend:image-pixels image)))
        (unwind-protect
             (dotimes (p 4)
               (let ((alpha (aref pixels (+ (* 4 p) 3))))
                 (is = alpha (sb-sys:sap-ref-8 data (+ (* 4 p) 3)))
                 (loop for (to from) in '((0 2) (1 1) (2 0))
                       do (true (<= (abs (- (sb-sys:sap-ref-8 data (+ (* 4 p) to))
                                            (floor (* alpha (aref pixels (+ (* 4 p) from))) 255)))
                                    1)))))
          (tuatara.frontend.gtk::cairo-surface-destroy surface))))))

(define-test (gtk title-writes-octets-not-utf-8)
  ;; a name that is not UTF-8 is titled as the mode line writes it, each octet \xNN
  (is string= "bad\\xFF\\xFEname.txt - Tuatara"
      (tuatara.frontend.gtk::shown-title (format nil
                                                 "bad~c~cname.txt - Tuatara"
                                                 (code-char #xDCFF)
                                                 (code-char #xDCFE))))
  (is string= "plain - Tuatara" (tuatara.frontend.gtk::shown-title "plain - Tuatara")))

(define-test (gtk command-keys-before-the-input-method)
  ;; keys held with Control or Meta go to the editor before the input method, so that
  ;; a dead key's accent being composed never eats C-x C-f or C-g; Control+Shift+U,
  ;; typing a character by its code, and plain keys are the input method's
  (flet ((command (keyval state) (tuatara.frontend.gtk::command-key-p keyval state)))
    (true (command (char-code #\x) 4))
    (true (command (char-code #\g) 4))
    (true (command (char-code #\x) 8))
    (false (command (char-code #\x) 0))
    (false (command (char-code #\/) 1))
    (false (command #xfe51 0))
    (false (command #x55 (logior 4 1)))
    (true (command #x55 4))))

(define-test (gtk altgr-letters-as-meta)
  ;; AltGr with a letter key is Meta with the letter the key gives alone (on a Swedish layout
  ;; AltGr-x gives », AltGr-e €); AltGr with a digit or sign is the input method's (AltGr-7 {,
  ;; AltGr-2 @); the left Alt is Meta as always
  (flet ((key (keyval state &optional first-level)
           (let ((k (tuatara.frontend.gtk::gdk-command-key keyval state first-level)))
             (and k (tuatara.frontend:key-spec k)))))
    (let ((tuatara-user:*altgr-as-meta* t))
      (is equal '(:meta #\x) (key #xbb 0 #x78))
      (is equal '(:meta #\X) (key #x3e 1 #x78))
      (is equal '(:meta #\e) (key #x20ac 0 #x65))
      (is equal '(:ctrl :meta #\x) (key #xbb 4 #x78))
      (false (key #x7b 0 #x37))
      (false (key #x40 0 #x32))
      (false (key #x5c 0 #x2b))
      (is equal '(:meta #\x) (key #x78 8))
      (is equal '(:ctrl #\x) (key #x78 4))
      (false (key #x78 0))
      (false (key #x58 1)))
    (let ((tuatara-user:*altgr-as-meta* nil))
      (false (key #xbb 0 #x78))
      (false (key #x20ac 0 #x65))
      (is equal '(:meta #\x) (key #x78 8))))
  ;; the setting names the layouts that type their own letters with AltGr, which turn it off
  (let ((words (documentation 'tuatara-user:*altgr-as-meta* 'variable)))
    (dolist (layout '("Polish" "Czech" "Hungarian" "Romanian" "Croatian" "Lithuanian" "Neo"))
      (true (search layout words)))
    (false (search "no third sign" words))))

(define-test (gtk altgr-level-read-from-the-event)
  ;; GDK stood in for: the key event's level says AltGr (2 and up), and the keycode's key value
  ;; with no modifier, in the event's layout, is the letter that goes with Meta
  (let ((level 2) (asked '()))
    (with-functions-replaced ((tuatara.gtk:gtk-event-controller-get-current-event
                               (lambda (controller) (declare (ignore controller)) (sb-sys:int-sap 16)))
                              (tuatara.gtk:gdk-key-event-get-level (lambda (event) (declare (ignore event)) level))
                              (tuatara.gtk:gdk-event-get-display (lambda (event) (declare (ignore event)) (sb-sys:int-sap 32)))
                              (tuatara.gtk:gdk-key-event-get-layout (lambda (event) (declare (ignore event)) 1))
                              (tuatara.gtk:gdk-first-level-keyval
                               (lambda (display keycode layout)
                                 (declare (ignore display))
                                 (push (list keycode layout) asked)
                                 (case keycode (53 #x78) (16 #x37)))))
      (flet ((key (keyval state keycode)
               (let ((k (tuatara.frontend.gtk::gdk-command-key
                         keyval state (tuatara.frontend.gtk::third-level-keyval (sb-sys:int-sap 8) keycode))))
                 (and k (tuatara.frontend:key-spec k)))))
        (let ((tuatara-user:*altgr-as-meta* t))
          (is equal '(:meta #\x) (key #xbb 0 53))
          (is equal '(53 1) (first asked))
          (is equal '(:meta #\X) (key #x3e 1 53))
          (false (key #x7b 0 16))
          (setf level 0 asked '())
          (false (key #x78 0 53))
          (false asked))
        (let ((tuatara-user:*altgr-as-meta* nil))
          (setf level 2)
          (false (key #xbb 0 53))
          (false asked))))))

(define-test (gtk picture-written-to-a-name-not-utf-8)
  ;; a file name the window gives GTK or Cairo reaches the system as it was read, raw bytes and all
  (tuatara.gtk:load-gtk-libraries)
  (let* ((directory (tuatara.io:path-join (uiop:native-namestring (uiop:temporary-directory))
                                       (tuatara.io:slash-ended (format nil
                                                                       "tuatara-test-~a"
                                                                       (tuatara.io:random-name)))))
         (path (format nil "~apicture-~c.png" directory (code-char #xDCFF))))
    (tuatara.io:make-directory directory #o700)
    (unwind-protect
         (sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
           (let ((surface (tuatara.gtk:cairo-image-surface-create 0 4 4)))
             (unwind-protect (is = 0 (tuatara.gtk:cairo-surface-write-to-png surface path))
               (tuatara.gtk:cairo-surface-destroy surface)))
           (is eq :regular (tuatara.io:path-kind path)))
      (tuatara.io:delete-tree directory))))

(define-test (gtk callback-pointers-kept)
  ;; a callback's C function pointer is found once and kept: nothing made each time it is given to GTK
  (flet ((pointer () (tuatara.gtk:callback tuatara.frontend.gtk::on-draw-request)))
    (is = (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:alien-callable-function 'tuatara.frontend.gtk::on-draw-request)))
        (sb-sys:sap-int (pointer)))
    (pointer)
    (let* ((before (tuatara.bench::bytes-consed))
           (consed (progn (dotimes (i 1000) (pointer)) (- (tuatara.bench::bytes-consed) before))))
      (true (< consed 1000) "~d octets for 1000 pointers" consed))))

;; right-to-left text is put in the order it is read by the layout; Pango is handed Hebrew a letter at a time,
;; so that it does not turn it round again, and an Arabic word whole and in the order written, so that it joins
(define-test (gtk right-to-left-text-painted-in-row-order)
  (tuatara.display:register-face :test-sample "A test face with a font of its own." (list :font "Adwaita Mono Bold"))
  (let ((theme tuatara.display::*current-theme*)
        (hebrew (format nil "x ~{~c~} y" (mapcar #'code-char '(#x5D2 #x5D1 #x5D0))))
        ;; كتب (k t b) as the row shows it, b t k
        (arabic (format nil "x ~{~c~} y" (mapcar #'code-char '(#x628 #x62A #x643)))))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (flet ((texts (row spans)
                    (let ((frame (tuatara.frontend.gtk:make-frame (vector (list row nil spans) nil)))
                          (texts '()))
                      (setf (tuatara.frontend.gtk::frame-cursor-row frame) 1)
                      (sb-int:encapsulate 'tuatara.gtk:pango-layout-set-text 'seen
                                          (lambda (function layout text)
                                            (push text texts)
                                            (funcall function layout text)))
                      (unwind-protect
                           (tuatara.frontend.gtk:call-with-painted-frame
                            (lambda (&rest arguments) (declare (ignore arguments)))
                            frame :columns 10 :rows 2 :font "Adwaita Mono 11")
                        (sb-int:unencapsulate 'tuatara.gtk:pango-layout-set-text 'seen))
                      texts)))
             (flet ((count-of (text low high) (count-if (lambda (c) (<= low (char-code c) high)) text)))
               (dolist (spans '(((2 5 :right-to-left)) ((0 7 (:test-sample :right-to-left)))))
                 (false (find-if (lambda (text) (> (count-of text #x5D0 #x5EA) 1)) (texts hebrew spans))))
               (dolist (spans '(((2 5 :right-to-left)) ((0 2 :test-sample) (2 5 (:test-sample :right-to-left)))))
                 (true (member (format nil "~{~c~}" (mapcar #'code-char '(#x202B #x643 #x62A #x628 #x202C)))
                               (texts arabic spans) :test #'string=))))))
      (tuatara.display:apply-theme theme))))

(define-test (gtk joined-arabic-kept-to-its-cells)
  ;; a joined Arabic run in a font of another width than the cells' (narrower, wider) is drawn as wide as its
  ;; cells, from end to end: none of its letters cut off by them, nothing of it drawn outside them
  (let ((theme tuatara.display::*current-theme*))
    (unwind-protect
         (progn
           (tuatara.display:apply-theme 'ao-dark)
           (dolist (font '("Adwaita Sans 5" "Adwaita Sans 22"))
             (tuatara.display:register-face :test-run-sample "A test face with a font of its own." (list :font font))
             (let ((frame (tuatara.frontend.gtk:make-frame
                           (vector (list (format nil "  ~{~c~}  " (mapcar #'code-char '(#x628 #x62A #x643))) nil
                                         '((2 5 (:test-run-sample :right-to-left))))
                                   nil)))
                   (scales '()))
               (setf (tuatara.frontend.gtk::frame-cursor-row frame) 1)
               (sb-int:encapsulate 'tuatara.gtk:cairo-scale 'seen
                                   (lambda (function cr x y) (push x scales) (funcall function cr x y)))
               (unwind-protect
                    (tuatara.frontend.gtk:call-with-painted-frame
                     (lambda (surface width height cell-width cell-height)
                       (declare (ignore width height))
                       ;; the font's width is not the cells': the letters drawn narrower or wider to fit
                       (true (find-if (lambda (x) (/= x 1d0)) scales) "drawn as Pango sets them, in ~a" font)
                       (flet ((inked-p (x)
                                (loop for y below cell-height
                                      thereis (string/= "#080d15" (tuatara.frontend.gtk:pixel-colour surface x y)))))
                         (let ((left (loop for x from 0 below (* 7 cell-width) when (inked-p x) return x))
                               (right (loop for x downfrom (1- (* 7 cell-width)) to 0 when (inked-p x) return x)))
                           ;; the letters reach from the run's first cell to its last, and no further
                           (true (and left (<= (* 2 cell-width) left (+ (* 2 cell-width) (floor cell-width 2))))
                                 "the letters begin at ~a, the run at ~a, in ~a" left (* 2 cell-width) font)
                           (true (and right (<= (- (* 5 cell-width) (floor cell-width 2)) right (1- (* 5 cell-width))))
                                 "the letters end at ~a, the run at ~a, in ~a" right (* 5 cell-width) font))))
                     frame :columns 7 :rows 2 :font "Adwaita Mono 11")
                 (sb-int:unencapsulate 'tuatara.gtk:cairo-scale 'seen)))))
      (tuatara.display:apply-theme theme))))

(defclass failing-fake-frontend (tuatara.frontend.fake:fake-frontend)
  ((fails :initarg :fails :reader failing-fails
          :documentation "Which of :START and :STOP signals an error."))
  (:documentation "A fake window that fails to start or to stop, for the tests of a client's windows."))

(defmethod tuatara.frontend:frontend-start ((f failing-fake-frontend))
  (if (eq (failing-fails f) :start) (error "the window cannot start") (call-next-method)))

(defmethod tuatara.frontend:frontend-stop ((f failing-fake-frontend))
  (if (eq (failing-fails f) :stop) (error "the window cannot close") (call-next-method)))

(define-test (gtk client-window-gone-whatever-fails)
  ;; a window that fails to start or to close is still counted gone, its connection closed
  (dolist (fails '(:start :stop))
    (multiple-value-bind (mine theirs) (tuatara.io:socket-pair)
      (let ((gone 0) (octets (make-array 16 :element-type '(unsigned-byte 8))))
        ;; the editor at the other end says nothing: no client is made of the connection
        (when (eq fails :stop) (tuatara.io:shut-socket theirs))
        (ignore-errors
         (tuatara.frontend.gtk::show-client-window (make-instance 'failing-fake-frontend :fails fails) mine nil
                                                   (tuatara.frontend:make-request) nil
                                                   :painted (constantly nil) :gone (lambda () (incf gone))))
        (is = 1 gone "~(~a~) failing" fails)
        (is = 0 (tuatara.io:read-fd-into theirs octets) "~(~a~) failing: the connection closed" fails)
        (sb-posix:close theirs)))))

(define-test (gtk layouts-found-without-garbage)
  ;; a text already set out is found again without consing a key for it: what each lookup costs in bytes, at two counts
  (with-painting-window (f)
    (let* ((surface (tuatara.gtk:cairo-image-surface-create 0 64 64))
           (cr (tuatara.gtk:cairo-create surface))
           (layouts (tuatara.frontend.gtk::make-layouts)))
      (unwind-protect
           (flet ((cost (lookups)
                    (tuatara.frontend.gtk::kept-layout f cr layouts "hello" nil 0)
                    (let ((before (sb-ext:get-bytes-consed)))
                      (dotimes (i lookups) (tuatara.frontend.gtk::kept-layout f cr layouts "hello" nil 0))
                      (- (sb-ext:get-bytes-consed) before))))
             (cost 10)
             (true (< (cost 100000) 10000) "~d bytes for 100000 lookups" (cost 100000))
             ;; the same text in another font is another layout
             (false (eq (tuatara.frontend.gtk::kept-layout f cr layouts "hello" nil 0)
                        (tuatara.frontend.gtk::kept-layout f cr layouts "hello" nil 1))))
        (tuatara.frontend.gtk::free-layouts layouts)
        (tuatara.gtk:cairo-destroy cr)
        (tuatara.gtk:cairo-surface-destroy surface)))))

(define-test (gtk last-piece-of-a-row-read-once)
  ;; the content of a row's last piece is made once for a frame, for the shift and for the painting
  (with-painting-window (f)
    (let ((count 0)
          (frame (tuatara.frontend.gtk:make-frame (vector (list "hello" nil '()) (list "world" nil '())))))
      (sb-int:encapsulate 'tuatara.frontend.gtk::piece-content 'counted
                          (lambda (function &rest arguments) (incf count) (apply function arguments)))
      (unwind-protect
           (let* ((width (* 20 (tuatara.frontend.gtk::cell-width f)))
                  (surface (tuatara.gtk:cairo-image-surface-create 0 width (* 2 (tuatara.frontend.gtk::cell-height f))))
                  (cr (tuatara.gtk:cairo-create surface)))
             (unwind-protect (tuatara.frontend.gtk::paint-frame f cr width (* 2 (tuatara.frontend.gtk::cell-height f))
                                                                frame t)
               (tuatara.gtk:cairo-destroy cr)
               (tuatara.gtk:cairo-surface-destroy surface)))
        (sb-int:unencapsulate 'tuatara.frontend.gtk::piece-content 'counted))
      (is = 2 count "one content made for each of the two rows"))))

(define-test (gtk font-families-asked-of-the-font-map)
  ;; a font is looked up by its family's name, without listing every family installed
  (with-painting-window (f)
    (true (tuatara.frontend.gtk::font-family-installed-p "Adwaita Mono"))
    (true (tuatara.frontend.gtk::font-family-installed-p "adwaita mono"))
    (false (tuatara.frontend.gtk::font-family-installed-p "No Such Family 12345"))
    (let ((listed nil))
      (sb-int:encapsulate 'tuatara.frontend.gtk::call-with-font-families 'counted
                          (lambda (function &rest arguments) (setf listed t) (apply function arguments)))
      (unwind-protect (let ((was tuatara-user:*gui-font*))
                        (unwind-protect (tuatara.frontend:frontend-set-font f "Adwaita Mono 11")
                          (setf tuatara-user:*gui-font* was)))
        (sb-int:unencapsulate 'tuatara.frontend.gtk::call-with-font-families 'counted))
      (false listed "setting a font listed the families"))))

(define-test (gtk clipboard-watched-from-the-first-paste)
  ;; a window keeps no copy of the clipboard until a paste asks for it: opening it starts no watch
  (with-painting-window (f)
    (false (tuatara.frontend.gtk::gtk-clipboard-reads f))
    (false (search "(read-changed-clipboard f)"
                   (subseq (gtk-frontend-source)
                           (search "(defun open-window (f)" (gtk-frontend-source))
                           (search "(defun log-stderr-unless-terminal" (gtk-frontend-source)))))))
