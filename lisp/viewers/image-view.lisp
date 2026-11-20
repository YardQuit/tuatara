;;;; lisp/viewers/image-view.lisp --- An image file shown as its picture.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Visiting an image (PNG, JPEG, GIF, WebP, BMP, TIFF, ICO, HEIF, AVIF, told
;;;; by its octets) shows its picture, drawn by a program shut in by the
;;;; sandbox (decode.lisp) in the background, as large as the window allows,
;;;; never larger than it is: a buffer of its own, read only, with nothing to
;;;; save. hex-mode opens its octets, safely; O opens it in the desktop's
;;;; program. An SVG image is a text: it is visited as one, its picture in
;;;; rimu's preview. Where pictures are not shown, the buffer says what the
;;;; image is. A file keeping more pictures than the one shown (a TIFF's
;;;; pages, inner-pictures.lisp) shows the others with n and p.

(in-package #:tuatara.defaults)

(define-major-mode image-view-mode special-mode
  "An image file shown as its picture, drawn in the sandbox: read only,
nothing to save. \\<image-view-mode>\\[image-view-next-picture] and
\\[image-view-previous-picture] show the next and previous picture the file
keeps inside it (a TIFF's pages); \\[hex-mode] opens its octets, safely;
\\[open-externally] opens it in the desktop's program; \\`q' closes."
  :lighter "Image" :interactive nil)

(defmethod mode-wraps-p ((mode image-view-mode))
  "A picture is drawn, not laid out in lines: its lines never wrap."
  nil)

(defvar *image-view-keymap* (mode-keymap 'image-view-mode)
  "Keys of an image file shown as its picture (image-view-mode's keymap).")

(defparameter *image-view-largest* 4096
  "The most pixels across or down an image is drawn with: a larger one is drawn
this large, its proportions kept, and shown smaller to fit its window.")

(defvar *image-views* '()
  "The buffers showing an image file as its picture.")

(define-record (image-view (:constructor make-image-view (path kind words)) (:copier nil))
  "What an image's buffer knows: the image file's PATH, its KIND (as
TUATARA.IO:OCTETS-KIND tells it) and that in WORDS; the PICTURES it keeps
inside it, once read, and the INDEX of the one shown; its drawing JOB; the
picture WANTED by a link before the pictures were read."
  (path nil :documentation "The path of the image file of IMAGE-VIEW.")
  (kind nil :documentation "The kind of IMAGE-VIEW's file, as TUATARA.IO:OCTETS-KIND tells it.")
  (words nil :documentation "The kind of IMAGE-VIEW's file in words.")
  (pictures :unread :documentation "The INNER-PICTURE records of IMAGE-VIEW's file, NIL for one picture only, or
:UNREAD before its first drawing.")
  (index 0 :documentation "The index from 0 among its pictures of the picture IMAGE-VIEW shows.")
  (job nil :documentation "The IMAGE-DRAW-JOB drawing IMAGE-VIEW's picture now, or NIL.")
  (wanted nil :documentation "The index of the picture a link asked IMAGE-VIEW for before its pictures were read."))

(defun image-view-of (buffer)
  "The IMAGE-VIEW of BUFFER, or NIL when it shows no image file."
  (and (live-buffer-p buffer) (tuatara.buffer:buffer-local buffer 'image-view)))

(define-record (image-draw-job (:include drawing-job) (:constructor make-image-draw-job (buffer view path))
                               (:copier nil))
  "VIEW's image, the file at PATH, being drawn for BUFFER, a drawing job:
RESULT, once done, (IMAGE . NOTES) as DECODE-IMAGE gives them; PICTURES,
those the file keeps, when they were read with it."
  (view nil :documentation "The IMAGE-VIEW whose image IMAGE-DRAW-JOB draws.")
  (pictures :unread :documentation "The pictures IMAGE-DRAW-JOB read the file to keep, or :UNREAD."))

(defmethod tuatara.runtime:job-take-in ((job image-draw-job))
  "Give the image's buffer its picture (IMAGE-VIEW-SHOW), or say why there is
none; a drawing given up for another picture is let go."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (view (image-draw-job-view job)))
    (when (and (image-view-of buffer) (eq job (image-view-job view)))
      (setf (image-view-job view) nil)
      (unless (eq (image-draw-job-pictures job) :unread)
        (setf (image-view-pictures view) (image-draw-job-pictures job)))
      (let ((wanted (shiftf (image-view-wanted view) nil)))
        (when (and wanted (plusp wanted))
          (if (< wanted (length (image-view-pictures view)))
              (return-from tuatara.runtime:job-take-in (image-view-start buffer view wanted))
              (message "~a keeps no picture ~d" (tuatara.io:native-file-name (image-view-path view)) (1+ wanted)))))
      (image-view-show buffer view
                       (or (case (tuatara.runtime:job-status job)
                             (:done (drawing-job-result job))
                             (:stopped (cons nil (list (cons ", not shown: stopped" :comment)))))
                           (cons nil (list (cons (format nil ", not shown: ~a"
                                                         (or (drawing-job-failure job) "it could not be read"))
                                                 :comment))))))))

(defun image-file-kind (native)
  "What the file at NATIVE is by its octets, as two values, a keyword and
words, when it is an image this view draws (one of *IMAGE-FORMATS*, not
SVG); else NIL."
  (multiple-value-bind (words kind) (ignoring-errors-reported ("finding what ~a is" native) (file-kind native))
    (and kind (not (eq kind :svg)) (assoc kind *image-formats*)
         (values kind words))))

(defun image-view-start (buffer view &optional (index (image-view-index view)))
  "Start drawing picture INDEX (from 0) of VIEW's image, BUFFER's, in the
background, its drawing of another stopped; the pictures it keeps inside it
read with the first."
  (let ((old (image-view-job view))
        (job (make-image-draw-job buffer view (image-view-path view)))
        (kind (image-view-kind view))
        (pictures (image-view-pictures view))
        (largest *image-view-largest*))
    (when old (setf (car (tuatara.runtime:job-cancel old)) t))
    (setf (image-view-index view) index
          (image-view-job view) job)
    (start-drawing-job job
                       (lambda (fd cancel)
                         (when (eq pictures :unread)
                           (setf pictures (inner-pictures fd (tuatara.io:read-fd-start fd "the picture"
                                                                                       (tuatara.io:fd-size fd) 4096)
                                                          kind)
                                 (image-draw-job-pictures job) pictures))
                         (multiple-value-bind (image notes)
                             (if pictures
                                 (inner-picture-image fd kind (nth index pictures) largest largest :cancel cancel)
                                 (decode-image fd kind largest largest :cancel cancel))
                           (cons image notes)))
                       :name "tuatara image view")))

(defun image-view-header-words (view)
  "What the header line of VIEW's buffer says: the file's kind, and which of
the pictures inside it is shown, when it keeps more than one."
  (let ((pictures (image-view-pictures view))
        (index (image-view-index view)))
    (if (and (consp pictures) (< index (length pictures)))
        (format nil "~a; picture ~d of ~d: ~a" (image-view-words view) (1+ index) (length pictures)
                (inner-picture-words (nth index pictures)))
        (image-view-words view))))

(defun image-view-step (count)
  "Show the picture COUNT on from the one shown in the image's view of the
current buffer, among those its file keeps inside it, round from the last to
the first."
  (let* ((buffer (current-buffer))
         (view (image-view-of buffer))
         (pictures (and view (image-view-pictures view))))
    (cond ((null view) (message "Not an image's view"))
          ((eq pictures :unread) (message "The pictures inside it are still being read"))
          ((null pictures) (message "~a keeps no other picture" (tuatara.io:native-file-name (image-view-path view))))
          (t (image-view-start buffer view (mod (+ (image-view-index view) count) (length pictures)))))))

(define-command image-view-next-picture ()
  "Show the next picture the image file shown keeps inside it (a TIFF's next
page), after the last the first: the header line names which is shown."
  (image-view-step 1))

(define-command image-view-previous-picture ()
  "Show the previous picture the image file shown keeps inside it (a TIFF's
page before), before the first the last: the header line names which is
shown."
  (image-view-step -1))

(defmethod mode-link-search ((mode image-view-mode) buffer search)
  "Show the picture inside the image that SEARCH names (picture 2, as
\\[show-file-metadata] links to it); once its pictures are read, when they
are being read."
  (let* ((view (image-view-of buffer))
         (pictures (and view (image-view-pictures view)))
         (index (and view (inner-picture-search-index search (if (listp pictures)
                                                                 (max 1 (length pictures))
                                                                 most-positive-fixnum)))))
    (cond ((null index) nil)
          ((eq pictures :unread) (setf (image-view-wanted view) index) t)
          (t (image-view-start buffer view index) t))))

(defun image-view-visit (path show)
  "Show the image file at PATH as its picture, with SHOW, a function of the
buffer, when it is an image by its octets and a program shut in by the
sandbox can draw it; the buffer made for it already, when there is one.
Returns the buffer, or NIL for a file visited as usual (not an image, or
nothing to draw it with, which is said)."
  (let ((native (file-view-path path)))
    (when native
      (multiple-value-bind (kind words) (image-file-kind native)
        (when (and kind
                   (file-view-can-draw-p native (find-image-decoder) (no-decoder-words "pictures" *image-decoders*)))
          (file-view-show native *image-views* (lambda (b) (let ((v (image-view-of b))) (and v (image-view-path v))))
                          (lambda () (image-view-make native kind words)) show))))))

(defun image-view-make (native kind words)
  "A new buffer showing the image file at NATIVE, of KIND (WORDS), its picture being drawn."
  (let ((buffer (make-file-view-buffer native 'image-view-mode
                                       (substitute-command-keys
                                        (format nil "~a: ~a~%~%Pictures are not shown here: \\[hex-mode] shows its octets.~%"
                                                (tuatara.io:native-file-name native) words))
                                       "its picture"))
        (view (make-image-view native kind words)))
    (setf (tuatara.display:buffer-header-line buffer)
          (static-header-line (format nil " ~a" words))
          (tuatara.buffer:buffer-local buffer 'image-view) view)
    (push buffer *image-views*)
    (image-view-start buffer view)
    buffer))

(defun image-view-show (buffer view result)
  "Give BUFFER, VIEW's, the picture in RESULT, (IMAGE . NOTES), what is said of
it in its header. With no picture, or where pictures are not shown, the
file's octets in its place, and why (FILE-VIEW-FALL-TO-OCTETS)."
  (destructuring-bind (image . notes) result
    (let ((words (format nil "~a~{~a~}" (image-view-header-words view) (mapcar #'car notes))))
      (cond ((null image)
             ;; its text shown, no picture on its way, while the octets are opened
             (setf (tuatara.buffer:buffer-local buffer :picture-pending) nil
                   (tuatara.buffer:buffer-local buffer :picture-view) nil)
             (file-view-fall-to-octets buffer (format nil "The picture could not be drawn (~a)" words)))
            ((not (and *editor* (tuatara.frontend:frontend-images-p (editor-frontend *editor*))))
             (file-view-fall-to-octets buffer "Pictures are not shown here"))
            (t (setf (tuatara.display:buffer-header-line buffer) (static-header-line (format nil " ~a" words))
                     (tuatara.buffer:buffer-local buffer :image) image))))))

(defun image-view-forget (buffer)
  "Forget BUFFER, killed, among the images shown; its drawing, a job of its, is stopped with it."
  (setf *image-views* (remove buffer *image-views*)))

(defun install-image-view ()
  "Have image files visited shown as their pictures, and bind O there, n and
p to step through the pictures inside one."
  (define-key *image-view-keymap* "O" 'open-externally)
  (define-key *image-view-keymap* "n" 'image-view-next-picture)
  (define-key *image-view-keymap* "p" 'image-view-previous-picture)
  (pushnew 'image-view-visit *file-views*)
  (pushnew 'image-view-visit *sandboxed-file-views*)
  (pushnew 'stop-decoding *keyboard-quit-functions*)
  (pushnew 'image-view-forget tuatara.buffer:*kill-buffer-functions*))
