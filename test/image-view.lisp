;;;; test/image-view.lisp --- An image file visited, shown as its picture.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test image-view
  :description "Visiting an image file (PNG, JPEG and the like, by its octets) shows its picture,
drawn in the sandbox, read only; M-x hex-mode opens its octets, safely; where pictures are not shown
the buffer says what it is. An SVG image is visited as its text; a safe visit opens the octets.")

(defun image-session (file keys &key cell-pixels (width 120) (height 30) settle)
  "Visit FILE with C-x C-f in an editor session and, once its picture is drawn
in the background, type KEYS; CELL-PIXELS has the frontend show pictures.
With SETTLE, a function, it is called with the buffer shown after the keys,
and the session waits again for what is drawn. Returns the frontend and the
editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels))
        (start (get-internal-real-time)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file))))
    (labels ((busy ()
               (some #'tuatara.defaults::image-draw-job-p tuatara.runtime:*jobs*))
             (wait (then)
               (if (and (busy) (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f (lambda () (wait then))))
                   (funcall then)))
             (check ()
               (wait (lambda ()
                       (when keys (tuatara.frontend.fake:fake-queue-keys f keys))
                       (when settle
                         (tuatara.frontend.fake:fake-queue-action
                          f (lambda ()
                              (funcall settle (tuatara.display:window-buffer
                                               (tuatara.command:editor-window tuatara.command:*editor*)))
                              (setf start (get-internal-real-time))
                              (wait (constantly nil)))))))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(defun image-forget ()
  "Kill the image buffers earlier sessions left, and the octets shown in place of a picture."
  (dolist (b (copy-list tuatara.defaults::*image-views*))
    (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b)))
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (when (tuatara.display:hex-view-p b) (tuatara.buffer:kill-buffer b))))

(define-test (image-view shown)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "photo.png" dir)))
      (alexandria:write-byte-vector-into-file *tiny-png* file)
      ;; its picture, read only, with what it is over it; no octets, nothing to save
      (image-forget)
      (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= "photo.png" (tuatara.buffer:buffer-name buffer))
          (true (tuatara.defaults::image-view-of buffer))
          (true (tuatara.frontend.fake:fake-images f))
          (true (tuatara.frontend.fake:fake-frame-showing f "PNG image, 2 × 2"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Image"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Sandboxed"))
          (false (tuatara.frontend.fake:fake-frame-showing f "Pictures are not shown here"))
          (false (tuatara.buffer:buffer-modified-p buffer))
          (false (tuatara.buffer:buffer-file buffer))))
      ;; M-x hex-mode: its own octets, opened safely
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "M-x h e x - m o d e RET" :cell-pixels '(10 20))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (true (tuatara.display:hex-view-p buffer))
          (true (tuatara.io:safe-visit-p buffer))
          (true (tuatara.frontend.fake:fake-frame-showing f "00000000  89 50 4e 47"))
          (tuatara.buffer:kill-buffer buffer)))
      ;; where pictures are not shown: its octets, and why
      (image-forget)
      (multiple-value-bind (f editor) (image-session file nil)
        (true (tuatara.frontend.fake:fake-frame-showing f "Pictures are not shown here"))
        (true (tuatara.display:hex-view-p (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      ;; a safe visit: its octets
      (image-forget)
      (multiple-value-bind (f editor)
          (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))
                    :width 120
                    :height 20
                    :cell-pixels '(10 20))
        (declare (ignore f))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (false (tuatara.defaults::image-view-of buffer))
          (tuatara.buffer:kill-buffer buffer)))
      ;; an SVG image is a text: visited as one
      (alexandria:write-string-into-file "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"4\"></svg>"
                                         (merge-pathnames "drawing.svg" dir))
      (image-forget)
      (multiple-value-bind (f editor) (image-session (merge-pathnames "drawing.svg" dir) nil :cell-pixels '(10 20))
        (declare (ignore f))
        (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (false (tuatara.defaults::image-view-of buffer))
          (true (search "<svg" (tuatara.buffer:buffer-string buffer)))
          (tuatara.buffer:kill-buffer buffer)))
      (image-forget))))

(define-test (image-view sizes-checked-before-use)
  ;; a picture with too few octets for its size is refused; a JPEG of height 0 has no size
  (true (tuatara.frontend:make-image 2 1 3 (make-array 6 :element-type '(unsigned-byte 8))))
  (fail (tuatara.frontend:make-image 2 2 3 (make-array 6 :element-type '(unsigned-byte 8))) 'tuatara.runtime:refusal)
  (flet ((jpeg (height width)
           (coerce (list #xFF #xD8 #xFF #xC0 0 11 8 (ash height -8) (logand height 255) (ash width -8)
                         (logand width 255) 1 1 #x11 0 #xFF #xD9)
                   'tuatara.text:octets)))
    (is equal '(16 8) (multiple-value-list (tuatara.io::jpeg-size (jpeg 8 16))))
    (false (tuatara.io::jpeg-size (jpeg 0 16)))))

(define-test (image-view large-photo)
  ;; a photo of 108 megapixels (12000 x 9000, a phone's at full size) drawn as any: decoded
  ;; at a fraction of its size from the start, never refused for want of memory in the sandbox
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "large.jpg" dir))))
      (uiop:run-program (list "magick" "-size" "12000x9000" "xc:#808080" "-quality" "70" file))
      (image-forget)
      (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-images f))
        (true (tuatara.frontend.fake:fake-frame-showing f "12000 × 9000"))
        (false (tuatara.frontend.fake:fake-frame-showing f "failed"))))
    ;; a PNG, a TIFF with its directory after its pixels, a WebP and an AVIF of 36 megapixels, which no
    ;; fraction shortens: given the memory their size calls for, read from the file
    (dolist (name '("large.png" "large.tif" "large.webp" "large.avif"))
      (let ((file (namestring (merge-pathnames name dir))))
        (uiop:run-program (list "magick" "-size" "6000x6000" "gradient:#203040-#c0d0e0" file))
        (image-forget)
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-images f) "~a" name)
          (true (tuatara.frontend.fake:fake-frame-showing f "6000 × 6000") "~a" name)
          (false (tuatara.frontend.fake:fake-frame-showing f "failed") "~a" name))))
    ;; one needing more than *picture-memory*: not drawn, and why said
    (let ((tuatara.defaults::*picture-memory* (* 512 1024 1024)))
      (image-forget)
      (let ((f (image-session (namestring (merge-pathnames "large.png" dir)) nil :cell-pixels '(10 20))))
        (true (tuatara.frontend.fake:fake-frame-showing f "more than *picture-memory* allows"))))))

(define-test (image-view camera-raw-preview)
  ;; a Fujifilm raw LibRaw cannot develop (here one with no raw data at all): the camera's JPEG
  ;; it carries, where its header says, drawn in its place
  (with-test-directory (dir)
    (let ((jpeg (namestring (merge-pathnames "preview.jpg" dir)))
          (raf (namestring (merge-pathnames "photo.raf" dir))))
      (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" jpeg))
      (let* ((octets (alexandria:read-file-into-byte-vector jpeg))
             (header (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
        (replace header (map 'vector #'char-code "FUJIFILMCCD-RAW 0201FF383501"))
        (loop for (at value) in (list (list 84 256) (list 88 (length octets)))
              do (loop for i below 4 do (setf (aref header (+ at i)) (ldb (byte 8 (* 8 (- 3 i))) value))))
        (with-open-file (out raf :direction :output :element-type '(unsigned-byte 8))
          (write-sequence header out)
          (write-sequence octets out)))
      (image-forget)
      (multiple-value-bind (f editor) (image-session raf nil :cell-pixels '(10 20))
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-images f))
        (true (tuatara.frontend.fake:fake-frame-showing f "Camera raw, Fujifilm RAF"))))))

(define-test (image-view why-not)
  ;; nothing to draw it with: its octets, and why
  (with-test-directory (dir)
    (let ((file (merge-pathnames "photo.png" dir)))
      (alexandria:write-byte-vector-into-file *tiny-png* file)
      (image-forget)
      (let ((tuatara-user:*sandbox* nil))
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "photo.png opens as octets: decoding is off"))
          (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
            (false (tuatara.defaults::image-view-of buffer))
            (tuatara.buffer:kill-buffer buffer))))
      (image-forget)
      (let ((tuatara.defaults::*image-decoders* '((:name "NoSuchMagick" :program ("no-such-magick-tuatara")))))
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "photo.png opens as octets: no program for pictures (install NoSuchMagick: no-such-magick-tuatara)"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (tuatara.io:sandbox-status :again t)
      ;; a picture that cannot be drawn says so and shows its octets
      (let ((fake (merge-pathnames "fake.png" dir)))
        (alexandria:write-byte-vector-into-file
         (concatenate '(vector (unsigned-byte 8))
                      #(#x89 80 78 71 13 10 26 10)
                      (map 'vector #'char-code "garbage garbage garbage"))
         fake)
        (image-forget)
        (multiple-value-bind (f editor) (image-session fake nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-frame-showing f "damaged PNG image"))
          (true (tuatara.frontend.fake:fake-frame-showing f "The picture could not be drawn"))
          (true (tuatara.display:hex-view-p (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (false (tuatara.frontend.fake:fake-frame-showing f "Pictures are not shown here"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (image-forget))
      ;; a JPEG cut short: what could be drawn is shown, and said to be damaged
      (let ((whole (namestring (merge-pathnames "whole.jpg" dir)))
            (cut (merge-pathnames "cut.jpg" dir)))
        (uiop:run-program (list "magick" "-size" "400x300" "gradient:red-blue" whole))
        (let ((octets (alexandria:read-file-into-byte-vector whole)))
          (alexandria:write-byte-vector-into-file (subseq octets 0 (floor (length octets) 2)) cut))
        (image-forget)
        (multiple-value-bind (f editor) (image-session cut nil :cell-pixels '(10 20))
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-images f))
          (true (tuatara.frontend.fake:fake-frame-showing f ", damaged: Premature end of JPEG file")))
        (image-forget)))))

(define-test (image-view listed-with-its-file)
  ;; the buffer list names the picture's file, counted among the files
  (with-test-directory (dir)
    (let ((file (merge-pathnames "photo.png" dir)))
      (alexandria:write-byte-vector-into-file *tiny-png* file)
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "C-x C-b" :cell-pixels '(10 20) :width 200)
        (declare (ignore f))
        (let ((row (find-if (lambda (l) (search "photo.png" l :end2 (min 30 (length l))))
                            (uiop:split-string (session-text editor) :separator '(#\Newline)))))
          (true row)
          (true (search (tuatara.command::abbreviated-path (tuatara.io:native file)) row))))
      (image-forget)
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (image-view picture-octets-decoded)
  ;; a picture's octets, as a package finds them in another file, turned into pixels in the sandbox
  (with-test-directory (dir)
    (let ((jpeg (namestring (merge-pathnames "patch.jpg" dir))))
      (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" "-quality" "95" jpeg))
      (let ((octets (alexandria:read-file-into-byte-vector jpeg)))
        (let ((image (tuatara:decode-picture octets :jpeg 64 48)))
          (true image)
          (when image
            (is = 64 (tuatara:image-width image))
            (is = 48 (tuatara:image-height image))
            (let ((pixels (tuatara:image-pixels image)) (at (* 10 (tuatara:image-channels image))))
              (true (<= 48 (aref pixels at) 54) "red ~d" (aref pixels at))
              (true (<= 99 (aref pixels (+ at 1)) 105) "green ~d" (aref pixels (+ at 1)))
              (true (<= 150 (aref pixels (+ at 2)) 156) "blue ~d" (aref pixels (+ at 2))))))
        ;; nothing to decode it with: no picture, and why
        (unwind-protect
             (let ((tuatara-user:*sandbox* nil))
               (tuatara.io:sandbox-status :again t)
               (multiple-value-bind (image notes) (tuatara:decode-picture octets :jpeg 64 48)
                 (false image)
                 (true notes)))
          (tuatara.io:sandbox-status :again t))))))

(defun j2k-octets (width height &key (components 3) (tile-width width) (tile-height height) (precision 8))
  "The start of a JPEG 2000 codestream for a picture WIDTH by HEIGHT: its
start marker and size marker, with COMPONENTS of PRECISION bits, cut in tiles
TILE-WIDTH by TILE-HEIGHT, and nothing after them."
  (flet ((be (n k) (loop for i from (1- k) downto 0 collect (ldb (byte 8 (* 8 i)) n))))
    (kind-octets (list #xFF #x4F #xFF #x51) (be (+ 38 (* 3 components)) 2) (be 0 2)
                 (be width 4) (be height 4) (be 0 4) (be 0 4) (be tile-width 4) (be tile-height 4) (be 0 4) (be 0 4)
                 (be components 2)
                 (loop repeat components append (list (1- precision) 1 1)))))

(defun picture-box (type octets &key long)
  "OCTETS in a box of TYPE, as JPEG 2000 and JPEG XL files keep their parts;
LONG writes its size in the long form (size 1, then eight octets)."
  (if long
      (let ((size (+ 16 (length octets))))
        (kind-octets (list 0 0 0 1) type (loop for i from 7 downto 0 collect (ldb (byte 8
                                                                                        (* 8 i)) size))
                     (coerce octets 'list)))
      (let ((size (+ 8 (length octets))))
        (kind-octets (loop for i from 3 downto 0 collect (ldb (byte 8 (* 8 i)) size)) type (coerce octets 'list)))))

(defun jp2-octets (codestream &key (before #()))
  "CODESTREAM kept in a JP2 file: its signature, its file type box, BEFORE
\(octets of boxes) and the codestream's box."
  (concatenate '(vector (unsigned-byte 8))
               (kind-octets tuatara.io::*jp2-signature*) (picture-box "ftyp" (kind-octets "jp2 " (list 0 0 0 0) "jp2 "))
               before (picture-box "jp2c" codestream)))

(defun jxl-octets (width height)
  "The start of a bare JPEG XL codestream for a picture WIDTH by HEIGHT (each
at most 2^30), its size written in its longest form, all else as standard,
and nothing after."
  (let ((bits '()))
    (flet ((put (value count) (dotimes (k count) (push (ldb (byte 1 k) value) bits))))
      (put 0 1) (put 3 2) (put (1- height) 30) (put 0 3) (put 3 2) (put (1- width) 30) (put 1 1))
    (setf bits (reverse bits))
    (kind-octets (list #xFF #x0A)
                 (loop for rest on bits by (lambda (l) (nthcdr 8 l))
                       collect (loop for k below 8 for b in rest sum (ash b k))))))

(defun jxl-boxed (codestream &key parts long (before #()))
  "CODESTREAM kept in a JPEG XL file of boxes: its signature, its file type
box, BEFORE (octets of boxes) and a jxlc box, or with PARTS, in that many
jxlp boxes, their sizes in the long form with LONG."
  (concatenate '(vector (unsigned-byte 8))
               (kind-octets tuatara.io::*jxl-signature*) (picture-box "ftyp" (kind-octets "jxl " (list 0 0 0 0) "jxl "))
               before
               (if parts
                   (let ((step (ceiling (length codestream) parts)))
                     (apply #'concatenate '(vector (unsigned-byte 8))
                            (loop for i below parts
                                  for start = (* i step)
                                  collect (picture-box "jxlp" (concatenate '(vector (unsigned-byte 8))
                                                                           (vector (if (= i (1- parts)) #x80 0) 0 0 i)
                                                                           (subseq codestream
                                                                                   (min start (length codestream))
                                                                                   (min (+ start step)
                                                                                        (length codestream))))
                                                       :long long))))
                   (picture-box "jxlc" codestream))))

(define-test (image-view jpeg-2000-and-xl-told)
  ;; JPEG 2000 files and codestreams, JPEG XL codestreams and files of boxes: what they are, and their size
  (flet ((kind (octets) (multiple-value-list (tuatara.io:octets-kind octets))))
    (is equal '("JPEG 2000 codestream, 640 × 480" :j2k) (kind (j2k-octets 640 480)))
    (is equal '("JPEG 2000 image, 640 × 480" :jp2) (kind (jp2-octets (j2k-octets 640 480))))
    (is equal '(640 480) (multiple-value-list (tuatara.io:octets-picture-size (jp2-octets (j2k-octets 640 480)))))
    (is equal '("JPEG 2000 image" :jp2) (kind (kind-octets tuatara.io::*jp2-signature*)))
    (is equal '("JPEG XL image, 5000 × 3000" :jxl) (kind (jxl-octets 5000 3000)))
    (is equal '("JPEG XL image, 5000 × 3000" :jxl) (kind (jxl-boxed (jxl-octets 5000 3000))))
    (is equal '("JPEG XL image, 5000 × 3000" :jxl) (kind (jxl-boxed (jxl-octets 5000 3000) :parts 2)))
    (is equal '(:width 300 :height 200 :components 4 :precision 16 :tiles 247)
        (tuatara.io:j2k-header (j2k-octets 300 200 :components 4 :precision 16 :tile-width 16 :tile-height 16)))
    (false (tuatara.io:kind-name-disagrees-p :jp2 "scan.jpx"))
    (false (tuatara.io:kind-name-disagrees-p :j2k "scan.j2c"))
    (true (tuatara.io:kind-name-disagrees-p :jxl "photo.txt"))))

(defun coded-picture-files (dir)
  "Pictures made in DIR by ImageMagick, each 64 by 48 pixels of #336699, as
a list of (NAME KIND OCTETS): a JP2 file, a JPEG 2000 codestream, a JPEG XL
codestream, the same kept in boxes, a JP2 file and a JPEG XL one of 16 bits,
and a JP2 file with alpha at half."
  (let ((png (namestring (merge-pathnames "flat.png" dir)))
        (png16 (namestring (merge-pathnames "flat16.png" dir)))
        (alpha (namestring (merge-pathnames "alpha.png" dir))))
    (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" png))
    (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" "-depth" "16" png16))
    (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" "-alpha" "set" "-channel" "A" "-evaluate" "set" "50%" alpha))
    (flet ((made (from name &rest options)
             (let ((path (namestring (merge-pathnames name dir))))
               (uiop:run-program (append (list "magick" from) options (list path)))
               (alexandria:read-file-into-byte-vector path))))
      (let ((jxl (made png "flat.jxl" "-quality" "100")))
        (list (list "flat.jp2" :jp2 (made png "flat.jp2" "-depth" "8"))
              (list "flat.j2k" :j2k (made png "flat.j2k" "-depth" "8"))
              (list "flat.jxl" :jxl jxl)
              (list "boxed.jxl" :jxl (jxl-boxed jxl))
              (list "parts.jxl" :jxl (jxl-boxed jxl :parts 3))
              (list "flat16.jp2" :jp2 (made png16 "flat16.jp2" "-depth" "16"))
              (list "flat16.jxl" :jxl (made png16 "flat16.jxl" "-depth" "16" "-quality" "100"))
              (list "alpha.jp2" :jp2 (made alpha "alpha.jp2" "-depth" "8")))))))

(define-test (image-view jpeg-2000-and-xl-decoded)
  ;; each turned into pixels in the sandbox, as a package calls for them, and shown when visited
  (with-test-directory (dir)
    (let ((files (coded-picture-files dir)))
      (loop for (name kind octets) in files
            do (is eq kind (nth-value 1 (tuatara.io:octets-kind octets)) "~a told" name)
               (multiple-value-bind (image notes) (tuatara:decode-picture octets kind 64 48)
                 (true image "~a decoded: ~s" name notes)
                 (when image
                   (is = 64 (tuatara:image-width image))
                   (is = 48 (tuatara:image-height image))
                   (let ((pixels (tuatara:image-pixels image)) (at (* 100 (tuatara:image-channels image))))
                     (true (<= 48 (aref pixels at) 54) "~a red ~d" name (aref pixels at))
                     (true (<= 99 (aref pixels (+ at 1)) 105) "~a green ~d" name (aref pixels (+ at 1)))
                     (true (<= 150 (aref pixels (+ at 2)) 156) "~a blue ~d" name (aref pixels (+ at 2)))
                     (when (= 4 (tuatara:image-channels image))
                       (if (search "alpha" name)
                           (true (<= 120 (aref pixels (+ at 3)) 135) "~a alpha ~d" name (aref pixels (+ at 3)))
                           (is = 255 (aref pixels (+ at 3)))))))))
      ;; a PDF's /JPXDecode picture may be either: :jp2 and :j2k each read both
      (let ((jp2 (third (assoc "flat.jp2" files :test #'string=)))
            (j2k (third (assoc "flat.j2k" files :test #'string=))))
        (true (tuatara:decode-picture j2k :jp2 64 48))
        (true (tuatara:decode-picture jp2 :j2k 64 48)))
      ;; visited: its picture, with what it is over it
      (let ((file (merge-pathnames "flat.jp2" dir)))
        (image-forget)
        (multiple-value-bind (f editor) (image-session file nil :cell-pixels '(10 20))
          (true (tuatara.frontend.fake:fake-images f))
          (true (tuatara.frontend.fake:fake-frame-showing f "JPEG 2000 image, 64 × 48"))
          (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (image-forget)))))

(define-test (image-view huge-declared-picture-refused)
  ;; a small file declaring a huge picture is refused from its header, the decoding program never run
  ;; (here one that would say it gave no picture)
  (let ((tuatara.defaults::*image-decoders* '((:name "stand-in" :program ("true")))))
    (flet ((refusal (octets kind)
             (multiple-value-bind (image notes) (tuatara:decode-picture octets kind 640 480)
               (false image)
               (let ((said (format nil "~{~a~}" (mapcar #'car notes))))
                 (false (search "gave no picture" said) "~a" said)
                 said))))
      (let ((garbage (make-array 4096 :element-type '(unsigned-byte 8) :initial-element #x5A)))
        (dolist (case (list (list (concatenate '(vector (unsigned-byte 8)) (j2k-octets 100000 100000) garbage) :j2k)
                            (list (jp2-octets (concatenate '(vector (unsigned-byte 8)) (j2k-octets 100000 100000) garbage)) :jp2)
                            ;; its codestream past the first 64 KB, after a long box of XML
                            (list (jp2-octets (concatenate '(vector
                                                             (unsigned-byte 8)) (j2k-octets 100000 100000) garbage)
                                              :before (picture-box "xml "
                                                                   (make-array 70000 :element-type '(unsigned-byte 8)
                                                                                     :initial-element 32)))
                                  :jp2)
                            (list (concatenate '(vector
                                                 (unsigned-byte 8)) (jxl-octets 1000000000 1000000000) garbage) :jxl)
                            (list (jxl-boxed (concatenate '(vector (unsigned-byte 8)) (jxl-octets 200000 200000) garbage)) :jxl)))
          (true (cl-ppcre:scan "not shown: [0-9,.]+ megapixels would take .* more than \\*picture-memory\\* allows"
                               (refusal (first case) (second case)))
                "~a" (second case))))
      ;; no size in its header, or one no JPEG 2000 picture has
      (true (search "its header gives no size" (refusal (kind-octets tuatara.io::*jp2-signature*) :jp2)))
      (true (search "its header gives no size" (refusal (kind-octets (list #xFF #x0A)) :jxl)))
      (true (search "more than JPEG 2000 allows"
                    (refusal (j2k-octets 100000 100000 :tile-width 1 :tile-height 1 :components 1) :j2k))))))

(define-test (image-view corrupt-coded-picture-fails-cleanly)
  ;; a damaged codestream fails in the sandbox and says so, never an error here
  (with-test-directory (dir)
    (loop for (name kind octets) in (coded-picture-files dir)
          when (member name '("flat.jp2" "flat.j2k" "flat.jxl") :test #'string=)
            do (let ((damaged (copy-seq octets))
                     (start (get-internal-real-time)))
                 ;; the header kept, the coded pixels after it made noise, then cut short
                 (loop for i from (min (length damaged) (if (eq kind :jxl) 12 140)) below (length damaged)
                       do (setf (aref damaged i) (logxor (aref damaged i) (logand (* i 37) 255))))
                 (setf damaged (subseq damaged 0 (min (length damaged) (max 40 (floor (* 3 (length damaged)) 4)))))
                 (multiple-value-bind (image notes) (tuatara:decode-picture damaged kind 64 48)
                   (let ((said (format nil "~{~a~}" (mapcar #'car notes))))
                     (false image "~a drawn" name)
                     (true (search "not shown: " said) "~a: ~s" name said)))
                 (true (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))))))

(define-test (image-view format-reader-missing-said)
  ;; a decoding program built without a format's reader: said plainly, the library named
  (let ((tuatara.defaults::*image-decoders*
          '((:name "stand-in"
             :program ("sh" "-c" "echo \"magick: no decode delegate for this image format \\`JP2' @ error/constitute.c/ReadImage/746.\" >&2; exit 1")))))
    (multiple-value-bind (image notes)
        (tuatara:decode-picture (jp2-octets (concatenate '(vector (unsigned-byte 8))
                                                         (j2k-octets 64 48)
                                                         (make-array 64 :element-type '(unsigned-byte 8))))
                                :jp2 64 48)
      (false image)
      (is equal ", not shown: sh here cannot read JP2 pictures: it was built or installed without OpenJPEG"
          (car (first notes))))))

(define-test (image-view coded-picture-given-time)
  ;; a large JPEG 2000 or JPEG XL picture is given time for its size, as far as *picture-memory* lets
  ;; one be drawn, and says plainly when that was not enough (a stand-in decoder taking two seconds, then printing one pixel)
  (let ((tuatara.defaults::*image-decoders*
          '((:name "stand-in"
             :program ("sh" "-c" "sleep 2; printf 'P7\\nWIDTH 1\\nHEIGHT 1\\nDEPTH 3\\nMAXVAL 255\\nTUPLTYPE RGB\\nENDHDR\\n\\001\\002\\003'"))))
        (tuatara-user:*sandbox-seconds* 1)
        (octets (concatenate '(vector (unsigned-byte 8))
                             (j2k-octets 5472 3648)
                             (make-array 64 :element-type '(unsigned-byte 8)))))
    (is = 31 (tuatara.defaults::picture-seconds (* 5472 3648)))
    (multiple-value-bind (image notes) (tuatara:decode-picture octets :j2k 64 48)
      (true image "~s" notes)
      (when image (is equalp #(1 2 3) (tuatara:image-pixels image))))
    (let ((tuatara.defaults::*picture-seconds-per-megapixel* 0))
      (multiple-value-bind (image notes) (tuatara:decode-picture octets :j2k 64 48)
        (false image)
        (is equal ", not shown: sh was stopped after 1 second, all a picture of 20.0 megapixels is given (*sandbox-seconds* and more for each megapixel)"
            (car (first notes)))))))

(define-test (image-view imagemagick-6-driven)
  ;; where ImageMagick 6 is all there is (convert-im6.q16, as Ubuntu 24.04 has it): driven as magick
  ;; is, with the same arguments and the same policy of its own
  (let ((seven (find "magick"
                     tuatara.defaults::*image-decoders*
                     :key (lambda (d) (first (getf d :program)))
                     :test #'string=)))
    (dolist (name '("convert-im6.q16" "convert-im6.q16hdri"))
      (let ((six (find name
                       tuatara.defaults::*image-decoders*
                       :key (lambda (d) (first (getf d :program)))
                       :test #'string=)))
        (true six "~a" name)
        (when six
          (is equal (rest (getf seven :program)) (rest (getf six :program)))
          (is equal (getf seven :jpeg-hint) (getf six :jpeg-hint))
          (is eq 'tuatara.defaults::magick-sandbox (getf six :sandbox))
          ;; after magick, before GraphicsMagick
          (true (< (position seven tuatara.defaults::*image-decoders*) (position six tuatara.defaults::*image-decoders*)
                   (position "gm"
                             tuatara.defaults::*image-decoders*
                             :key (lambda (d) (first (getf d :program)))
                             :test #'string=))))))))

(define-test (image-view readers-missing-said)
  ;; M-x check-health names the kinds of picture the program here has no reader for, and the library
  (let ((tuatara.defaults::*image-decoders*
          (list (list :name "stand-in"
                      :program (list "sh"
                                     "-c"
                                     (format nil "for f in ~{~a~^ ~}; do echo \"   $f* $f rw-  a format\"; done"
                                             (remove "JXL"
                                                     (mapcar #'cdr tuatara.defaults::*image-formats*)
                                                     :test #'string=)))))))
    (is equal "pictures by sh (stand-in), without readers for JXL (libjxl)" (tuatara.defaults::image-decoder-words)))
  ;; the program here, when it reads every kind, has nothing said
  (let ((words (tuatara.defaults::image-decoder-words)))
    (when (search "magick (ImageMagick)" words)
      (false (search "without readers" words) "~a" words))))

(define-test (image-view jpeg-never-decoded-larger)
  ;; a JPEG smaller than the room it is shown in is decoded at its own size, never more (ImageMagick 6
  ;; enlarges it to the size it is told); a larger one at no more than the room
  (with-test-directory (dir)
    (let ((small (namestring (merge-pathnames "small.jpg" dir)))
          (large (namestring (merge-pathnames "large.jpg" dir)))
          (tuatara.defaults::*image-decoders*
            '((:name "stand-in" :program ("sh" "-c" "echo \"$*\" >&2; exit 1" "sh" :jpeg-hint :input)
               :jpeg-hint ("-define" "jpeg:size=~dx~d")))))
      (uiop:run-program (list "magick" "-size" "40x30" "xc:#336699" small))
      (uiop:run-program (list "magick" "-size" "400x300" "xc:#336699" large))
      (flet ((told (path)
               (car (first (nth-value 1 (tuatara:decode-picture (alexandria:read-file-into-byte-vector path) :jpeg 64 48))))))
        (true (search "-define jpeg:size=40x30 JPEG:-[0]" (told small)) "~a" (told small))
        (true (search "-define jpeg:size=64x48 JPEG:-[0]" (told large)) "~a" (told large))))))

(define-test (image-view magick-policy-in-place-of-others)
  ;; ImageMagick reads every policy file on its path: each one the program here lists besides
  ;; Tuatara's is given Tuatara's policy in its place, shut in; one that cannot be is refused
  (clrhash tuatara.defaults::*magick-checked*)
  (let ((octets (concatenate '(vector (unsigned-byte 8))
                             (j2k-octets 64 48)
                             (make-array 64 :element-type '(unsigned-byte 8))))
        (pixel "printf 'P7\\nWIDTH 1\\nHEIGHT 1\\nDEPTH 3\\nMAXVAL 255\\nTUPLTYPE RGB\\nENDHDR\\n\\001\\002\\003'"))
    ;; a stand-in listing the policy files it finds, with "pattern: PAM" for Tuatara's; drawing only
    ;; when the other one, /etc/ld.so.cache here, holds Tuatara's policy
    (let ((tuatara.defaults::*image-decoders*
            (list (list :name "stand-in" :sandbox 'tuatara.defaults::magick-sandbox
                        :program (list "sh" "-c"
                                       (format nil "if [ \"$1\" = -list ]; then
  for f in /etc/tuatara/ImageMagick/policy.xml /etc/ld.so.cache; do
    [ -f \"$f\" ] || continue; echo \"Path: $f\"; grep -q 'pattern=\"PAM\"' \"$f\" && echo '  pattern: PAM'
  done; exit 0
else grep -q 'pattern=\"PAM\"' /etc/ld.so.cache && ~a; fi" pixel)
                                       "sh" :input)))))
      (multiple-value-bind (image notes) (tuatara:decode-picture octets :j2k 64 48)
        (true image "~s" notes)
        (when image (is equalp #(1 2 3) (tuatara:image-pixels image))))
      (is equal '("/etc/ld.so.cache")
          (tuatara.defaults::magick-policy-overlays (first tuatara.defaults::*image-decoders*))))
    ;; a policy file that still holds another policy, Tuatara's given in its place: not run
    (let ((tuatara.defaults::*image-decoders*
            (list (list :name "stand-in" :sandbox 'tuatara.defaults::magick-sandbox
                        :program (list "sh" "-c"
                                       (format nil "if [ \"$1\" = -list ]; then
  printf 'Path: /etc/tuatara/ImageMagick/policy.xml\\n  pattern: PAM\\nPath: /etc/ld.so.cache\\n  pattern: *\\n'
else ~a; fi" pixel)
                                       "sh" :input)))))
      (multiple-value-bind (image notes) (tuatara:decode-picture octets :j2k 64 48)
        (false image)
        (is equal ", not shown: sh: it reads a policy that cannot be replaced (/etc/ld.so.cache)"
            (car (first notes))))))
  ;; the policy holds ImageMagick to the memory the sandbox gives it
  (let ((policy (tuatara.defaults::magick-policy (* 1536 1024 1024))))
    (true (search "name=\"memory\" value=\"1536MiB\"" policy))
    (true (search "name=\"map\" value=\"1536MiB\"" policy))
    (true (search "name=\"disk\" value=\"1536MiB\"" policy)))
  (clrhash tuatara.defaults::*magick-checked*))

(define-test (image-view jpeg-xl-boxes-read-wherever)
  ;; a JPEG XL kept in boxes is read wherever its codestream begins: past the first 64 KB (after a
  ;; long box of XML, as a large Exif or C2PA box puts it), and in parts whose sizes are in the long
  ;; form; two octets FF 0A alone are no JPEG XL
  (with-test-directory (dir)
    (let* ((png (namestring (merge-pathnames "flat.png" dir)))
           (jxl (namestring (merge-pathnames "flat.jxl" dir)))
           (codestream (progn (uiop:run-program (list "magick" "-size" "64x48" "xc:#336699" png))
                              (uiop:run-program (list "magick" png "-quality" "100" jxl))
                              (alexandria:read-file-into-byte-vector jxl)))
           (far (jxl-boxed codestream :before (picture-box "xml " (make-array 70000 :element-type '(unsigned-byte 8)
                                                                                    :initial-element 32)))))
      (is eq :jxl (nth-value 1 (tuatara.io:octets-kind far)))
      (multiple-value-bind (image notes) (tuatara:decode-picture far :jxl 64 48)
        (true image "~s" notes)
        (when image (is = 64 (tuatara:image-width image))))
      (let ((long (jxl-boxed codestream :parts 3 :long t)))
        (is equal '("JPEG XL image, 64 × 48" :jxl) (multiple-value-list (tuatara.io:octets-kind long)))
        (true (tuatara:decode-picture long :jxl 64 48)))))
  (is equal '("JPEG XL image, 5000 × 3000" :jxl)
      (multiple-value-list (tuatara.io:octets-kind (jxl-boxed (jxl-octets 5000 3000) :parts 2 :long t))))
  (false (eq :jxl (nth-value 1 (tuatara.io:octets-kind (kind-octets (list #xFF #x0A 0))))))
  (is eq :jxl (nth-value 1 (tuatara.io:octets-kind (jxl-octets 64 48)))))

(define-test (image-view decoder-said-escaped)
  ;; what a decoding program says is shown with its control and direction characters written out:
  ;; a damaged file cannot have it clear the screen or turn text around
  (let ((octets (concatenate '(vector (unsigned-byte 8))
                             (j2k-octets 64 48)
                             (make-array 64 :element-type '(unsigned-byte 8)))))
    (flet ((said (script)
             (let ((tuatara.defaults::*image-decoders* (list (list :name
                                                                   "stand-in"
                                                                   :program (list "sh" "-c" script "sh" :input)))))
               (format nil "~{~a~}" (mapcar #'car (nth-value 1 (tuatara:decode-picture octets :j2k 64 48)))))))
      (let ((failed (said "printf 'bad \\033[2J\\342\\200\\256evil\\n' >&2; exit 1")))
        (false (find (code-char 27) failed) "~s" failed)
        (false (find (code-char #x202E) failed) "~s" failed)
        (true (search "bad ^[[2J<202E>evil" failed) "~s" failed))
      (let ((damaged (said "printf 'corrupt \\033[31mdata\\n' >&2; printf 'P7\\nWIDTH 1\\nHEIGHT 1\\nDEPTH 3\\nMAXVAL 255\\nTUPLTYPE RGB\\nENDHDR\\n\\001\\002\\003'")))
        (false (find (code-char 27) damaged) "~s" damaged)
        (true (search "damaged: corrupt ^[[31mdata" damaged) "~s" damaged)))))

(define-test (image-view pictures-inside)
  ;; a TIFF's pages: n and p step through them, the header naming the one shown; a link's picture 3 shows the third
  (with-test-directory (dir)
    (let ((file (merge-pathnames "pages.tif" dir)))
      (three-page-tiff file)
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "n" :cell-pixels '(10 20) :width 160 :settle #'identity)
        (let ((view (tuatara.defaults::image-view-of
                     (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
          (true (tuatara.frontend.fake:fake-frame-showing f "picture 1 of 3: page 1 of 3, 64 × 48"))
          (true (tuatara.frontend.fake:fake-frame-showing f "picture 2 of 3: page 2 of 3, 32 × 24"))
          (is = 3 (length (tuatara.defaults::image-view-pictures view)))))
      ;; before the first, the last
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "p" :cell-pixels '(10 20) :width 160 :settle #'identity)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "picture 3 of 3: page 3 of 3, 64 × 48")))
      ;; a link to the third, as M-x show-file-metadata writes it
      (image-forget)
      (let ((found '()))
        (multiple-value-bind (f editor)
            (image-session file nil :cell-pixels '(10 20) :width 160
                                    :settle (lambda (buffer)
                                              (dolist (search '("picture 3" "a heading" "picture 4"))
                                                (push (tuatara.defaults::mode-link-search
                                                       (tuatara.defaults::buffer-major-mode buffer) buffer search)
                                                      found))))
          (declare (ignore editor))
          (is equal '(nil nil t) found)
          (true (tuatara.frontend.fake:fake-frame-showing f "picture 3 of 3")))))))

(define-test (image-view pictures-inside-bounded)
  ;; a TIFF whose directories lead round to one already read is read once round; one picture is none inside
  (flet ((tiff (&rest directories)
           ;; little-endian, each directory (offset width next) of one entry, the file 64 octets
           (let ((o (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)))
             (replace o (list 73 73 42 0 8 0 0 0))
             (loop for (at width next) in directories
                   do (replace o (list 1 0 0 1 3 0 1 0 0 0 width 0 0 0) :start1 at)
                      (replace o (list (ldb (byte 8 0) next) (ldb (byte 8 8) next) 0 0) :start1 (+ at 14)))
             o)))
    (flet ((pictures (octets)
             (tuatara.io:call-with-memory-file "tuatara-test" octets
                                               (lambda (fd) (tuatara.defaults::inner-pictures fd octets :tiff)))))
      (is equal '(5 7) (mapcar #'tuatara.defaults::inner-picture-width (pictures (tiff '(8 5 28) '(28 7 8)))))
      (false (pictures (tiff '(8 5 8))))
      (false (pictures (tiff '(8 5 4000))))
      (is = 2 (length (pictures (tiff '(8 5 28) '(28 7 60)))))
      (is equal 2 (tuatara.defaults::inner-picture-search-index "picture 3" 3))
      (false (tuatara.defaults::inner-picture-search-index "picture 4" 3))
      (false (tuatara.defaults::inner-picture-search-index "picture 0" 3)))))

(defun photoshop-test-data (records &key big)
  "Photoshop's data as a TIFF keeps it (tag 37724), holding the layer
RECORDS, each a plist of :BOX (TOP LEFT BOTTOM RIGHT), :BLEND, :OPACITY,
:FLAGS, :NAME, :UNICODE and :SECTION, numbers and four-letter keys written
BIG-endian or, as a little-endian TIFF writes them, backwards."
  (labels ((int (value n) (loop for i below n collect (ldb (byte 8 (* 8 (if big (- n 1 i) i))) value)))
           (key (k) (map 'list #'char-code (if big k (reverse k))))
           (keyed (k data) (append (key "8BIM") (key k) (int (length data) 4) data (if (oddp (length data)) '(0) '()))))
    (let ((layers
            (loop for r in records
                  append (destructuring-bind (&key box (blend "norm") (opacity 255) (flags 0) (name "") unicode section) r
                           (let* ((pascal (append (list (length name)) (map 'list #'char-code name)))
                                  (padded (append pascal (make-list (mod (- (length pascal)) 4) :initial-element 0)))
                                  (extra (append (int 0 4) (int 0 4) padded
                                                 (and unicode
                                                      (keyed "luni" (append (int (length unicode) 4)
                                                                            (loop for c across unicode
                                                                                  append (int (char-code c) 2)))))
                                                 (and section (keyed "lsct" (int section 4))))))
                             (append (loop for v in box append (int v 4)) (int 0 2)
                                     (key "8BIM") (key blend) (list opacity 0 flags 0) (int (length extra) 4) extra))))))
      (coerce (append (map 'list #'char-code "Adobe Photoshop Document Data Block") '(0)
                      (keyed "Layr" (append (int (- (length records)) 2) layers)))
              '(vector (unsigned-byte 8))))))

(define-test (image-view photoshop-layers-read)
  ;; names (the Unicode one first), places, blend, opacity, hidden; a group's closing record left out
  (dolist (big '(nil t))
    (let ((layers (tuatara.defaults::photoshop-layers
                   (photoshop-test-data (list (list :box '(0 0 40 60) :name "Background")
                                              (list :box '(5 10 25 40) :name "Red box" :blend "mul " :opacity 128)
                                              (list :box '(0 0 0 0) :name "Group" :section 1)
                                              (list :box '(30 50 50 70) :name "old" :unicode "Hōiho" :flags 2)
                                              (list :box '(0 0 0 0) :name "</Layer group>" :section 3))
                                        :big big)
                   big)))
      (is equal '("Background" "Red box" "Group" "Hōiho") (mapcar (lambda (l) (getf l :name)) layers))
      (is equal '(5 10 25 40) (let ((l (second layers))) (list (getf l :top) (getf l :left) (getf l :bottom)
                                                             (getf l :right))))
      (is equal "multiply" (getf (second layers) :blend))
      (is = 128 (getf (second layers) :opacity))
      (true (getf (third layers) :group))
      (true (getf (fourth layers) :hidden))
      (false (getf (second layers) :hidden))
      ;; as pictures: the group listed, not drawn; frames count only those with pixels; what each covers
      (let ((pictures (tuatara.defaults::photoshop-layer-pictures layers 1000 1)))
        (is equal '((:frame 1) (:frame 2) nil (:frame 3)) (mapcar #'tuatara.defaults::inner-picture-source pictures))
        (true (search "Photoshop group \"Group\"" (tuatara.defaults::inner-picture-label (third pictures))))
        (true (search "at 10, 5 on the picture; multiply, 50% opaque; over \"Background\""
                      (tuatara.defaults::inner-picture-place (second pictures))))
        (true (search "hidden" (tuatara.defaults::inner-picture-place (fourth pictures))))
        (is equal '(10 5) (tuatara.defaults::inner-picture-region (second pictures))))))
  ;; a record cut short, a block of no layers, other octets: what fits, or nothing
  (let ((data (photoshop-test-data (list (list :box '(0 0 4 4) :name "One") (list :box '(0 0 4 4) :name "Two")))))
    (is = 1 (length (tuatara.defaults::photoshop-layers (subseq data 0 (- (length data) 3)) nil)))
    (false (tuatara.defaults::photoshop-layers (photoshop-test-data '()) nil))
    (false (tuatara.defaults::photoshop-layers (subseq data 10) nil))))

(define-test (image-view photoshop-layers-in-tiff)
  ;; a TIFF with Photoshop's layers, as ImageMagick writes one: each layer a picture, drawn and compared with
  ;; the part of the picture under it
  (with-test-directory (dir)
    (let ((psd (namestring (merge-pathnames "layers.psd" dir)))
          (file (namestring (merge-pathnames "layers.tif" dir))))
      (uiop:run-program (list "magick" "-size" "80x60" "xc:#336699"
                              "(" "-size" "30x20" "xc:#ff0000" "-set" "label" "Red box" "-repage" "+10+5" ")"
                              "(" "-size" "20x20" "xc:#00ff00" "-set" "label" "Green" "-repage" "+50+30" ")"
                              "(" "-clone" "0-2" "-flatten" ")" "-insert" "0" "-delete" "1" psd))
      (uiop:run-program (list "magick" psd "-define" "tiff:write-layers=true" file))
      (let ((note (metadata-of file)))
        (note-has note "- Picture 2 :: Photoshop layer \"Red box\", 30 × 20; at 10, 5 on the picture; normal, 100% opaque")
        (note-has note "- Picture 3 :: Photoshop layer \"Green\", 20 × 20; at 50, 30 on the picture")
        (note-has note "looks the same as the part of the picture shown under it"))
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "n n" :cell-pixels '(10 20) :width 200 :settle #'identity)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "picture 3 of 3: Photoshop layer \"Green\", 20 × 20")))
      ;; a picture shown that leaves the green layer out: that layer differs from what lies under it
      (uiop:run-program (list "magick" "-size" "80x60" "xc:#336699"
                              "(" "-size" "30x20" "xc:#ff0000" "-set" "label" "Red box" "-repage" "+10+5" ")"
                              "(" "-size" "20x20" "xc:#00ff00" "-set" "label" "Green" "-repage" "+50+30" ")"
                              "(" "-clone" "0-1" "-flatten" ")" "-insert" "0" "-delete" "1" psd))
      (uiop:run-program (list "magick" psd "-define" "tiff:write-layers=true" file))
      (let ((note (metadata-of file)))
        (note-has note "\"Red box\", 30 × 20; at 10, 5 on the picture; normal, 100% opaque; its record at octet")
        (true (cl-ppcre:scan "\"Green\".*differs from the part of the picture shown under it in 100% of it" note)
              "~a" note)))))

(defun jpeg-with-pictures-inside (dir)
  "The octets of a JPEG made in DIR, 64 × 48 blue, keeping an EXIF thumbnail
\(16 × 12 blue), Photoshop's thumbnail (16 × 12 red) and, listed in its MPF
index, a larger preview (32 × 24 green) after its picture."
  (flet ((jpeg (size colour)
           (let ((path (namestring (merge-pathnames (format nil "~a.jpg" (subseq colour 1)) dir))))
             (uiop:run-program (list "magick" "-size" size (format nil "xc:~a" colour) path))
             (alexandria:read-file-into-byte-vector path)))
         (be (value n) (loop for i from (1- n) downto 0 collect (ldb (byte 8 (* 8 i)) value)))
         (le (value n) (loop for i below n collect (ldb (byte 8 (* 8 i)) value)))
         (segment (marker data) (append (list #xFF marker) (loop for i from 1 downto 0 collect (ldb (byte 8 (* 8 i)) (+ 2 (length data)))) data)))
    (let* ((picture (coerce (jpeg "64x48" "#336699") 'list))
           (thumbnail (coerce (jpeg "16x12" "#336698") 'list))
           (photoshop (coerce (jpeg "16x12" "#ff0000") 'list))
           (preview (coerce (jpeg "32x24" "#00ff00") 'list))
           (exif (segment #xE1 (append (map 'list #'char-code "Exif") '(0 0)
                                       '(73 73 42 0) (le 8 4) (le 0 2) (le 14 4)
                                       (le 2 2) (le #x201 2) (le 4 2) (le 1 4) (le 44 4)
                                       (le #x202 2) (le 4 2) (le 1 4) (le (length thumbnail) 4) (le 0 4)
                                       thumbnail)))
           (resources (append (map 'list #'char-code "Photoshop 3.0") '(0)
                              (map 'list #'char-code "8BIM") (be 1036 2) '(0 0) (be (+ 28 (length photoshop)) 4)
                              (be 1 4) (be 16 4) (be 12 4) (make-list 16 :initial-element 0)
                              photoshop (and (oddp (length photoshop)) '(0))))
           (app13 (segment #xED resources))
           (mpf-length (+ 4 8 2 12 4 32))
           (base (+ 2 (length exif) (length app13) 4 4))
           (preview-at (+ 2 (length exif) (length app13) 4 mpf-length (- (length picture) 2)))
           (app2 (segment #xE2 (append (map 'list #'char-code "MPF") '(0)
                                       (map 'list #'char-code "MM") '(0 42) (be 8 4)
                                       (be 1 2) (be #xB002 2) (be 7 2) (be 32 4) (be 26 4) (be 0 4)
                                       (be #x20030000 4) (be (- preview-at 0) 4) (be 0 4) (be 0 4)
                                       (be #x00010001 4) (be (length preview) 4) (be (- preview-at base) 4) (be 0 4)))))
      (assert (= (length app2) (+ 4 mpf-length)))
      (coerce (append '(#xFF #xD8) exif app13 app2 (nthcdr 2 picture) preview) '(vector (unsigned-byte 8))))))

(define-test (image-view jpeg-pictures-inside)
  ;; a JPEG's EXIF and Photoshop thumbnails and the pictures its MPF index lists: listed, compared, shown with n
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "kept.jpg" dir))))
      (alexandria:write-byte-vector-into-file (jpeg-with-pictures-inside dir) file :if-exists :supersede)
      (let ((note (metadata-of file)))
        (note-has note "- Picture 1 :: the picture itself, 64 × 48; from octet 0; the picture a viewer shows")
        (true (cl-ppcre:scan "- Picture 2 :: its EXIF thumbnail, 16 × 12; in its APP1 segment, from octet [0-9,]+, [0-9.]+ [kM]?B; looks the same as the picture shown" note) "~a" note)
        (true (cl-ppcre:scan "- Picture 3 :: Photoshop's thumbnail, 16 × 12; in its APP13 segment \\(resource 1036\\).*differs from the picture shown in 100% of it" note) "~a" note)
        (true (cl-ppcre:scan "- Picture 4 :: a larger preview \\(VGA\\), 32 × 24; listed in its MPF index \\(APP2\\), number 2.*differs from the picture shown in 100% of it" note) "~a" note))
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "p" :cell-pixels '(10 20) :width 200 :settle #'identity)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "picture 4 of 4: a larger preview (VGA), 32 × 24")))))
  ;; an index pointing past the file's end, or at what is no JPEG: left out
  (with-test-directory (dir)
    (let* ((octets (jpeg-with-pictures-inside dir))
           (cut (subseq octets 0 (- (length octets) 10))))
      (flet ((labels-of (o)
               (tuatara.io:call-with-memory-file
                "tuatara-test" o
                (lambda (fd) (mapcar #'tuatara.defaults::inner-picture-label
                                     (tuatara.defaults::inner-pictures fd (subseq o 0 (min 4096 (length o))) :jpeg))))))
        (is equal '("the picture itself" "its EXIF thumbnail" "Photoshop's thumbnail") (labels-of cut))))))

(defun png-test-chunks (octets)
  "The chunks of the PNG OCTETS as a list of (TYPE . DATA), DATA a list of octets."
  (let ((chunks '()))
    (tuatara.defaults::map-png-chunks (lambda (type start end) (push (cons type (coerce (subseq octets start end) 'list)) chunks))
                                      octets)
    (nreverse chunks)))

(defun apng-test-octets (dir frames &key hidden)
  "An animated PNG made in DIR of FRAMES, each (COLOUR SIZE X Y DELAY), DELAY
in hundredths of a second, each drawn by ImageMagick as a PNG and its picture
data taken: the first frame is the file's picture data, or with HIDDEN,
\(COLOUR SIZE) of a picture of its own that is no frame comes first."
  (labels ((png (colour size)
             (let ((path (namestring (merge-pathnames (format nil "~a-~a.png" (subseq colour 1) size) dir))))
               ;; true colour, as every frame shares the file's header
               (uiop:run-program (list "magick" "-size" size (format nil "xc:~a" colour) (format nil "PNG24:~a" path)))
               (png-test-chunks (alexandria:read-file-into-byte-vector path))))
           (be (value n) (loop for i from (1- n) downto 0 collect (ldb (byte 8 (* 8 i)) value)))
           (data (chunks) (remove-if-not (lambda (c) (string= (car c) "IDAT")) chunks)))
    (let* ((sequence -1)
           (pictures (mapcar (lambda (f) (png (first f) (second f))) frames))
           (base (if hidden (png (first hidden) (second hidden)) (first pictures)))
           (chunks
             (append (list (assoc "IHDR" base :test #'string=) (cons "acTL" (append (be (length frames) 4) (be 0 4))))
                     (and hidden (data base))
                     (loop for (colour size x y delay) in frames
                           for picture in pictures
                           for first = t then nil
                           for (width height) = (mapcar #'parse-integer (uiop:split-string size :separator "x"))
                           collect (cons "fcTL" (append (be (incf sequence) 4) (be width 4) (be height 4) (be x 4) (be y 4)
                                                        (be delay 2) (be 100 2) (list 0 0)))
                           append (if (and first (not hidden))
                                      (data picture)
                                      (mapcar (lambda (c) (cons "fdAT" (append (be (incf sequence) 4) (cdr c))))
                                              (data picture))))
                     (list (cons "IEND" '())))))
      (apply #'concatenate '(vector (unsigned-byte 8)) '(137 80 78 71 13 10 26 10)
             (mapcar (lambda (c) (tuatara.defaults::png-chunk-octets (car c) (coerce (cdr c) '(vector (unsigned-byte 8)))))
                     chunks)))))

(define-test (image-view apng-frames-inside)
  ;; an animated PNG's frames past the first: listed with their place and timing, compared, shown with n
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "anim.png" dir))))
      (alexandria:write-byte-vector-into-file
       (apng-test-octets dir '(("#336699" "32x24" 0 0 50) ("#993366" "32x24" 0 0 50) ("#336699" "16x12" 8 6 50)))
       file :if-exists :supersede)
      (let ((note (metadata-of file)))
        (note-has note "- Picture 1 :: the picture itself, the animation's first frame, 32 × 24")
        (true (cl-ppcre:scan "- Picture 2 :: frame 2 of 3, 32 × 24; at 0, 0 on the picture; shown 0.50 s;.*differs from the part of the picture shown under it in 100% of it" note) "~a" note)
        (true (cl-ppcre:scan "- Picture 3 :: frame 3 of 3, 16 × 12; at 8, 6 on the picture;.*looks the same as the part of the picture shown under it" note) "~a" note))
      (image-forget)
      (multiple-value-bind (f editor) (image-session file "n" :cell-pixels '(10 20) :width 200 :settle #'identity)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "picture 2 of 3: frame 2 of 3, 32 × 24")))))
  ;; picture data that is no frame (no fcTL before it): said to be left out by programs showing the animation
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "hidden.png" dir))))
      (alexandria:write-byte-vector-into-file (apng-test-octets dir '(("#00ff00" "16x12" 8 6 10)) :hidden '("#ff0000" "32x24"))
                                              file :if-exists :supersede)
      (let ((note (metadata-of file)))
        (note-has note "- Picture 1 :: the picture itself, which programs showing the animation leave out, 32 × 24")
        (true (cl-ppcre:scan "- Picture 2 :: frame 1 of 1, 16 × 12; at 8, 6 on the picture; shown 0.10 s; replacing what is under it; then left as it is.*differs from the part of the picture shown under it in 100% of it" note) "~a" note)))))
