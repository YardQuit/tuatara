;;;; test/gtk/window-drawn.lisp --- A window drawn both ways, for the test gtk window-drawn-as-painted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Run by that test with sbcl --script on a display of its own (broadway):
;;;; FILE OUT [picture | pictures-beside]. Opens a Tuatara window from these
;;;; sources on FILE, and once it shows the file (with picture, once the file's
;;;; picture shows; with pictures-beside, once it shows in two windows side by side),
;;;; takes a picture of the whole window as GTK renders it, drawn as the
;;;; graphics card is given it, then another of the same window painted whole,
;;;; and writes to OUT how many textures the first held, how many octets the
;;;; two differ in, and in how many by more than 16.

(load (merge-pathnames "../../tools/quicklisp.lisp" *load-truename*))
(push (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)) asdf:*central-registry*)
(ql:quickload :tuatara/terminal :silent t)

(defmacro c (name result &rest arguments)
  "Call the C function NAME, which returns RESULT, with ARGUMENTS, alternately
a type and a value, of the types TUATARA.IO:C-TYPE knows."
  `(tuatara.runtime:foreign-call ,name ,(tuatara.io:c-type result)
                                 ,@(loop for (type value) on arguments by #'cddr
                                         collect (list (tuatara.io:c-type type) value))))

(defun window-picture (window)
  "The pixels of WINDOW as GTK renders it now, as a vector of octets, 32 bits a
pixel, and the texture nodes it was drawn with, by count."
  (let* ((width (c "gtk_widget_get_width" :int :pointer window))
         (height (c "gtk_widget_get_height" :int :pointer window))
         (paintable (c "gtk_widget_paintable_new" :pointer :pointer window))
         (snapshot (c "gtk_snapshot_new" :pointer)))
    (c "gdk_paintable_snapshot" :void :pointer paintable :pointer snapshot
       :double (float width 1d0) :double (float height 1d0))
    (let* ((node (let ((node (c "gtk_snapshot_free_to_node" :pointer :pointer snapshot)))
                   (if (tuatara.io:null-pointer-p node) (error "the window drew nothing") node)))
           (renderer (c "gsk_cairo_renderer_new" :pointer))
           (textures (let ((bytes (c "gsk_render_node_serialize" :pointer :pointer node)))
                       (sb-alien:with-alien ((size sb-alien:unsigned-long 0))
                         (let* ((data (c "g_bytes_get_data" :pointer :pointer bytes
                                         :pointer (sb-alien:alien-sap (sb-alien:addr size))))
                                (text (tuatara.io:sap-string data size)))
                           (loop with start = 0
                                 for at = (search "texture {" text :start2 start)
                                 while at count t do (setf start (1+ at))))))))
      (c "gsk_renderer_realize" :boolean :pointer renderer :pointer (tuatara.io:null-pointer)
         :pointer (tuatara.io:null-pointer))
      (let* ((texture (c "gsk_renderer_render_texture" :pointer :pointer renderer :pointer node
                         :pointer (tuatara.io:null-pointer)))
             (pixels (make-array (* 4 width height) :element-type '(unsigned-byte 8))))
        (tuatara.io:with-octets-sap (data pixels)
          (c "gdk_texture_download" :void :pointer texture :pointer data :size (* 4 width)))
        (values pixels textures)))))

(defvar *on-card* nil
  "The picture of the window drawn as pictures the graphics card is given, and its textures by count.")

(tuatara.frontend.gtk::define-callback show-on-card :boolean ((data :pointer))
  (tuatara.frontend.gtk::redraw tuatara.frontend.gtk::*frontend*)
  nil)

(tuatara.frontend.gtk::define-callback picture-on-card :boolean ((data :pointer))
  (let ((f tuatara.frontend.gtk::*frontend*))
    (setf *on-card* (multiple-value-list (window-picture (tuatara.frontend.gtk::gtk-window f))))
    ;; then the same window painted whole
    (tuatara.frontend.gtk::drawn-on-card f nil))
  nil)

(tuatara.frontend.gtk::define-callback picture-painted-whole :boolean ((data :pointer))
  (let ((whole (window-picture (tuatara.frontend.gtk::gtk-window tuatara.frontend.gtk::*frontend*))))
    (destructuring-bind (on-card textures) *on-card*
      (with-open-file (o (second (uiop:command-line-arguments)) :direction :output :if-exists :supersede)
        (with-standard-io-syntax
          (prin1 (list :textures textures
                       :octets (length whole)
                       :differing (if (= (length on-card) (length whole))
                                      (count nil (map 'list #'= on-card whole))
                                      (length whole))
                       :differing-much (if (= (length on-card) (length whole))
                                           (count-if (lambda (d) (> (abs d) 16)) (map 'list #'- on-card whole))
                                           (length whole)))
                 o)))))
  (sb-ext:exit :code 0 :abort t))

(sb-thread:make-thread
 (lambda ()
   (loop until (and tuatara.frontend.gtk::*frontend* (tuatara.frontend.gtk::gtk-shown tuatara.frontend.gtk::*frontend*))
         do (sleep 0.1))
   ;; a picture: once it shows, in as many windows as asked for
   (flet ((pictures-shown ()
            (let ((frame (sb-thread:with-mutex ((tuatara.frontend.gtk::gtk-lock tuatara.frontend.gtk::*frontend*))
                           (tuatara.frontend.gtk::gtk-shown tuatara.frontend.gtk::*frontend*))))
              (if frame (length (tuatara.frontend.gtk::frame-images frame)) 0))))
     (when (member (third (uiop:command-line-arguments)) '("picture" "pictures-beside") :test #'equal)
       (loop repeat 600 until (plusp (pictures-shown)) do (sleep 0.1)))
     (when (equal (third (uiop:command-line-arguments)) "pictures-beside")
       (dolist (key (list (tuatara.frontend:make-key #\x :ctrl t) (tuatara.frontend:make-key #\3)))
         (tuatara.frontend.gtk::push-key tuatara.frontend.gtk::*frontend* key))
       (loop repeat 600 until (= 2 (pictures-shown)) do (sleep 0.1))))
   ;; the file's text laid out and coloured, then each step once GTK has laid out the one before
   (sleep 2)
   (dolist (step (list (tuatara.gtk:callback show-on-card) (tuatara.gtk:callback picture-on-card)
                       (tuatara.gtk:callback picture-painted-whole)))
     (tuatara.frontend.gtk::g-idle-add step (tuatara.io:null-pointer))
     (sleep 1))))

(tuatara.terminal:main (list "--gui" (first (uiop:command-line-arguments))))
