;;;; lisp/viewers/inner-pictures.lisp --- The pictures a picture file keeps besides the one shown.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A picture file may keep more pictures than the one a viewer shows: a
;;;; TIFF's further pages (a scanner's other sheets, GIMP's layers, a smaller
;;;; copy, a mask) and the layers Photoshop keeps in it; a JPEG's thumbnails
;;;; (EXIF's, Photoshop's) and the pictures its MPF index lists (larger
;;;; previews, a phone's gain and depth maps); an animated PNG's or WebP's
;;;; frames; a HEIF's items (thumbnails, depth and gain maps, tiles). Their
;;;; structure is read here, by Tuatara alone, as the metadata is: small
;;;; reads, every offset checked against the file, every structure held to a
;;;; count. Their pixels are drawn in the sandbox (decode.lisp). M-x
;;;; show-file-metadata lists them in a section "Pictures inside", each
;;;; compared with the picture shown; in an image's view n and p step through
;;;; them. It lists the editing data a PDF keeps (Illustrator's own file,
;;;; Photoshop's and others' private data) as well.

(in-package #:tuatara.defaults)

(defparameter *inner-pictures-most* 256
  "The most pictures read of one file's structure: past them the rest are left out.")

(defparameter *inner-pictures-compared* 16
  "The most pictures inside one file that M-x show-file-metadata compares with
the picture shown: the rest are listed, not compared.")

(defparameter *inner-picture-compared-size* 256
  "The pixels across and down, at most, both pictures are drawn at to be
compared: enough to see a change, little to draw.")

(defparameter *inner-picture-difference-step* 24
  "How far, of 255, a colour of a pixel may stray between two pictures before
the pixel counts as differing: less is taken for the loss of saving or
drawing them small.")

(define-record (inner-picture (:constructor make-inner-picture (label place width height source &optional region))
                              (:copier nil))
  "One picture a file keeps: LABEL, what it is in words (page 2 of 3, a
smaller copy, its name); PLACE, where it lies in the file in words; its
WIDTH and HEIGHT in pixels (NIL when not said); SOURCE, how it is drawn,
\(:FRAME N), the file's own picture N, from 0, (:OCTETS START LENGTH KIND),
a picture of KIND kept in the file's octets from START, (:APNG-FRAME N), an
animated PNG's frame N from 0 (or :PICTURE, its picture data) made a PNG of
its own, or NIL when it has no pixels;
REGION, (X Y), where it lies on the picture shown, for a layer."
  (label nil :documentation "What INNER-PICTURE is, in words.")
  (place nil :documentation "Where INNER-PICTURE lies in its file, in words.")
  (width nil :documentation "The width of INNER-PICTURE in pixels, or NIL.")
  (height nil :documentation "The height of INNER-PICTURE in pixels, or NIL.")
  (source nil :documentation "How INNER-PICTURE is drawn: (:FRAME N), its file's picture N from 0, (:OCTETS START
LENGTH KIND), a picture kept in its file's octets, (:APNG-FRAME N), an animated PNG's frame, or NIL.")
  (region nil :documentation "Where INNER-PICTURE lies on the picture shown, (X Y) in pixels, or NIL."))

;;; Photoshop's layers, kept in a TIFF (tag 37724)

(defparameter *photoshop-layers-read* (* 4 1024 1024)
  "The most octets of the Photoshop data a TIFF keeps read for its layers'
records: their pixels, which follow, are drawn in the sandbox, not read.")

(defparameter *photoshop-blend-modes*
  '(("norm" . "normal") ("diss" . "dissolve") ("dark" . "darken") ("mul " . "multiply") ("idiv" . "colour burn")
    ("lbrn" . "linear burn") ("dkCl" . "darker colour") ("lite" . "lighten") ("scrn" . "screen")
    ("div " . "colour dodge") ("lddg" . "linear dodge") ("lgCl" . "lighter colour") ("over" . "overlay")
    ("sLit" . "soft light") ("hLit" . "hard light") ("vLit" . "vivid light") ("lLit" . "linear light")
    ("pLit" . "pin light") ("hMix" . "hard mix") ("diff" . "difference") ("smud" . "exclusion")
    ("fsub" . "subtract") ("fdiv" . "divide") ("hue " . "hue") ("sat " . "saturation") ("colr" . "colour")
    ("lum " . "luminosity") ("pass" . "pass through"))
  "Photoshop's blend modes by the four letters a layer record names them with.")

(defun photoshop-layers (octets big)
  "The layers of the Photoshop data in OCTETS, as a TIFF keeps it (tag 37724,
\"Adobe Photoshop Document Data Block\"), numbers read BIG-endian or not as
the TIFF's are: a list, bottom layer first, of plists with :NAME, :TOP,
:LEFT, :BOTTOM, :RIGHT (pixels on the picture), :BLEND (words), :OPACITY
\(0 to 255), :HIDDEN, :GROUP (true for a group's own record) and :AT (the
record's offset in OCTETS). A group's closing record is left out. At most
*INNER-PICTURES-MOST* layers; reading stops where a record does not fit in
OCTETS."
  (let ((length (length octets)) (layers '()))
    (labels ((int (at n) (and (<= (+ at n) length) (tuatara.io:octets-integer octets at n :big-endian big)))
             (signed (at n) (octets-signed (int at n) (* 8 n)))
             (key (at)
               ;; four letters, as the file writes them backwards when its numbers are
               (and (<= (+ at 4) length)
                    (let ((k (tuatara.text:octets-latin-1 octets :start at :end (+ at 4))))
                      (if big k (reverse k)))))
             (extra (at end)
               ;; the keyed blocks after a layer's name: its Unicode name and whether it opens a group
               (let ((name nil) (section nil))
                 (loop while (<= (+ at 12) end)
                       for size = (int (+ at 8) 4)
                       for data = (+ at 12)
                       while (and size (<= (+ data size) end) (member (key at) '("8BIM" "8B64") :test #'string=))
                       do (let ((k (key (+ at 4))))
                            (cond ((and (string= k "luni") (>= size 4))
                                   (let ((count (int data 4)))
                                     (when (and count (<= (+ data 4 (* 2 count)) (+ data size)))
                                       (setf name (utf-16-text octets :start (+ data 4) :end (+ data 4 (* 2 count))
                                                                      :big-endian big)))))
                                  ((and (member k '("lsct" "lsdk") :test #'string=) (>= size 4))
                                   (setf section (int data 4)))))
                          (setf at (+ data size (mod size 2))))
                 (values name section))))
      (let* ((header "Adobe Photoshop Document Data Block")
             (at (and (tuatara.io:octets-at-p octets header) (1+ (length header)))))
        ;; the blocks of the data, to the one of the layers
        (loop while (and at (<= (+ at 12) length))
              for size = (int (+ at 8) 4)
              until (or (null size) (member (key (+ at 4)) '("Layr" "Lr16" "Lr32") :test #'string=))
              do (setf at (+ at 12 size (mod (- size) 4))))
        (when (and at (<= (+ at 14) length))
          (let ((count (min (abs (or (signed (+ at 12) 2) 0)) *inner-pictures-most*))
                (record (+ at 14)))
            (loop repeat count
                  do (let* ((channels (int (+ record 16) 2))
                            (blend-at (and channels (+ record 18 (* 6 channels))))
                            (extra-length (and blend-at (int (+ blend-at 12) 4)))
                            (extra-at (and extra-length (+ blend-at 16)))
                            (end (and extra-at (+ extra-at extra-length))))
                       (unless (and end (<= end length) (member (key blend-at) '("8BIM" "8B64") :test #'string=))
                         (return))
                       (let* ((mask (int extra-at 4))
                              (ranges-at (and mask (+ extra-at 4 mask)))
                              (ranges (and ranges-at (int ranges-at 4)))
                              (name-at (and ranges (+ ranges-at 4 ranges)))
                              (name-length (and name-at (< name-at end) (aref octets name-at)))
                              (name (and name-length (<= (+ name-at 1 name-length) end)
                                         (octets-text octets (1+ name-at) (+ name-at 1 name-length)))))
                         (multiple-value-bind (unicode section)
                             (if name-length
                                 (extra (+ name-at (* 4 (ceiling (1+ name-length) 4))) end)
                                 (values nil nil))
                           (unless (eql section 3)
                             (push (list :name (let ((text (or unicode name "")))
                                                 (escape-controls (subseq text 0 (min 200 (length text)))))
                                         :top (signed record 4) :left (signed (+ record 4) 4)
                                         :bottom (signed (+ record 8) 4) :right (signed (+ record 12) 4)
                                         :blend (let ((k (key (+ blend-at 4))))
                                                  ;; ImageMagick writes it forwards in a little-endian TIFF
                                                  (or (cdr (assoc k *photoshop-blend-modes* :test #'string=))
                                                      (cdr (assoc (reverse k) *photoshop-blend-modes*
                                                                  :test #'string=))
                                                      (escape-controls k)))
                                         :opacity (aref octets (+ blend-at 8))
                                         :hidden (logtest 2 (aref octets (+ blend-at 10)))
                                         :group (member section '(1 2))
                                         :at record)
                                   layers))))
                       (setf record end)))))))
    (nreverse layers)))

(defun photoshop-layer-pictures (layers base frame)
  "INNER-PICTURE records of LAYERS (PHOTOSHOP-LAYERS), the first that has
pixels its file's picture FRAME, as two values: the records and the frame
after the last; BASE, the octet of the file the layers' offsets count from.
Each says where it lies on the picture, how it is blended, whether it is
hidden, and the layers under it that it covers. A layer of no pixels (a
group's record, one left empty) is listed, not drawn."
  (let ((pictures '()) (below '()))
    (dolist (layer layers)
      (destructuring-bind (&key name top left bottom right blend opacity hidden group at) layer
        (let* ((width (and left right (max 0 (- right left))))
               (height (and top bottom (max 0 (- bottom top))))
               (empty (or (null width) (null height) (zerop width) (zerop height)))
               (under (loop for (other-name o-top o-left o-bottom o-right) in below
                            when (and (not empty) (< left o-right) (< o-left right) (< top o-bottom) (< o-top bottom))
                              collect other-name)))
          (push (make-inner-picture
                 (format nil "~:[Photoshop layer~;Photoshop group~]~@[ \"~a\"~]" group (and (plusp (length name)) name))
                 (format nil "~{~a~^; ~}"
                         (remove nil (list (and (not empty) (format nil "at ~d, ~d on the picture" left top))
                                           (format nil "~a, ~d% opaque" blend (round (* 100 opacity) 255))
                                           (and hidden "hidden")
                                           (and under (format nil "over ~{\"~a\"~^, ~}"
                                                              (subseq under 0 (min 5 (length under)))))
                                           (format nil "its record at octet ~:d" (+ base at)))))
                 (and (not empty) width) (and (not empty) height)
                 (and (not empty) (list :frame (prog1 frame (incf frame))))
                 (and (not empty) (list left top)))
                pictures)
          (unless (or empty hidden)
            (push (list (if (plusp (length name)) name "unnamed") top left bottom right) below)))))
    (values (nreverse pictures) frame)))

;;; TIFF: a chain of directories, one a picture

(defun tiff-inner-pictures (fd head size)
  "The pictures of the TIFF open as file descriptor (FD), whose first octets
are HEAD, of SIZE octets: one for each directory of its chain, in order,
each followed by the Photoshop layers it keeps, at most
*INNER-PICTURES-MOST* directories, each read where it stands and visited
once. Frames are counted as ImageMagick draws them: a directory's layers
of pixels right after it."
  (let ((big (= (aref head 0) 77)) (seen '()) (pictures '()) (frame 0))
    (labels ((int (octets at n) (tuatara.io:octets-integer octets at n :big-endian big))
             (directory-at (at)
               ;; the octets of the directory at AT, its count, entries and the offset of the next
               (let* ((count-octets (and (< 7 at size) (tuatara.io:read-fd-exactly fd 2 at)))
                      (count (and count-octets (min 512 (int count-octets 0 2)))))
                 (and count (plusp count) (<= (+ at 6 (* 12 count)) size)
                      (tuatara.io:read-fd-exactly fd (+ 6 (* 12 count)) at))))
             (value-octets (octets entry)
               ;; the octets of ENTRY's value: in the entry, or where it points, at most 4 KB
               (destructuring-bind (tag type count value where) entry
                 (declare (ignore tag value))
                 (let* ((unit (exif-type-size type))
                        (total (and unit (* unit count))))
                   (cond ((null total) nil)
                         ((<= total 4) (subseq octets where (+ where total)))
                         (t (let ((at (int octets where 4)))
                              (and at (< 7 at size)
                                   (tuatara.io:read-fd-exactly fd (min total 4096 (- size at)) at))))))))
             (fraction (octets entry)
               (let ((v (and entry (value-octets octets entry))))
                 (and v (= (length v) 8) (plusp (int v 4 4)) (/ (int v 0 4) (int v 4 4)))))
             (place (octets entries)
               ;; where its pixels go on the page, when it says (GIMP's layers do), in pixels
               (let ((x (fraction octets (assoc 286 entries))) (y (fraction octets (assoc 287 entries)))
                     (x-dots (fraction octets (assoc 282 entries))) (y-dots (fraction octets (assoc 283 entries))))
                 (and x y (if (and x-dots y-dots)
                              (list (round (* x x-dots)) (round (* y y-dots)))
                              (list (round x) (round y))))))
             (label (octets entries index)
               (let* ((name-entry (assoc 285 entries))
                      (name (let ((v (and name-entry (value-octets octets name-entry))))
                              (and v (escape-controls (string-trim '(#\Nul #\Space)
                                                                   (octets-text v 0 (min (length v) 200)))))))
                      (subfile (let ((e (assoc 254 entries))) (if e (fourth e) 0)))
                      (old (fourth (assoc 255 entries)))
                      (page (assoc 297 entries))
                      (page-octets (and page (= (third page) 2) (value-octets octets page)))
                      (what (cond ((or (logtest subfile 1) (eql old 2)) "a smaller copy")
                                  ((logtest subfile 4) "a transparency mask")
                                  ((and page-octets (plusp (int page-octets 2 2)))
                                   (format nil "page ~d of ~d" (1+ (int page-octets 0 2)) (int page-octets 2 2)))
                                  (t (format nil "page ~d" (1+ index))))))
                 (if (plusp (length name)) (format nil "~a \"~a\"" what name) what)))
             (layers (octets entries)
               ;; the Photoshop layers of the directory, as pictures from the frame after its own
               (let* ((entry (assoc 37724 entries))
                      (at (and entry (member (second entry) '(1 7)) (> (third entry) 4) (int octets (fifth entry) 4)))
                      (data (and at (< 7 at size)
                                 (tuatara.io:read-fd-exactly fd (min (third entry) *photoshop-layers-read* (- size at))
                                                             at))))
                 (when data
                   (multiple-value-bind (layer-pictures next) (photoshop-layer-pictures (photoshop-layers data big)
                                                                                        at frame)
                     (setf frame next)
                     layer-pictures)))))
      (loop with at = (int head 4 4)
            for index from 0 below *inner-pictures-most*
            while (and at (plusp at) (not (member at seen)))
            do (push at seen)
               (let ((octets (directory-at at)))
                 (unless octets (return))
                 (multiple-value-bind (entries reader next) (tuatara.io:tiff-entries octets :ifd 0 :most 512
                                                                                           :little (not big))
                   (declare (ignore reader))
                   (let ((place (place octets entries)))
                     (push (make-inner-picture (label octets entries index)
                                               (format nil "its directory at octet ~:d~@[, placed at ~{~d, ~d~}~]"
                                                       at place)
                                               (fourth (assoc 256 entries)) (fourth (assoc 257 entries))
                                               (list :frame (prog1 frame (incf frame))))
                           pictures))
                   (setf pictures (revappend (layers octets entries) pictures))
                   (setf at next)))))
    (nreverse pictures)))

;;; JPEG: thumbnails and the MPF index

(defparameter *jpeg-segments-read* (* 1024 1024)
  "The most octets at the start of a JPEG read for the segments before its
picture, where its thumbnails and its MPF index are kept.")

(defparameter *jpeg-inner-largest* (* 64 1024 1024)
  "The most octets one picture kept inside a JPEG may take to be listed and
drawn: one claiming more is left out.")

(defparameter *mpf-types*
  '((#x010001 . "a larger preview (VGA)") (#x010002 . "a larger preview (full HD)")
    (#x020001 . "a frame of a panorama") (#x020002 . "a view for 3-D (disparity)")
    (#x020003 . "a view from another angle") (#x030000 . "a picture of its own")
    (#x000000 . "a picture of no stated kind (phones keep gain and depth maps so)"))
  "What a picture of a JPEG's MPF index is, by its type, as the Multi-Picture
Format standard (CIPA DC-007) names them.")

(defun jpeg-inner-pictures (fd head size)
  "The pictures of the JPEG open as file descriptor (FD), whose first octets
are HEAD, of SIZE octets: the picture itself, then its EXIF thumbnail,
Photoshop's thumbnail (resource 1036, or 1033) and the pictures its MPF
index lists, each found in the segments before its picture data
\(*JPEG-SEGMENTS-READ* octets at most), checked to lie within the file and
to begin as a JPEG does, at most *INNER-PICTURES-MOST* of them."
  (let* ((octets (tuatara.io:read-fd-exactly fd (min size *jpeg-segments-read*) 0))
         (pictures (list (multiple-value-bind (width height) (tuatara.io:octets-picture-size head)
                           (make-inner-picture "the picture itself" "from octet 0" width height (list :frame 0))))))
    (labels ((kept (label place at length)
               ;; a JPEG kept at AT, LENGTH octets, when it is there
               (when (and at length (< (length pictures) *inner-pictures-most*) (< 1 length *jpeg-inner-largest*)
                          (<= (+ at length) size))
                 (let ((start (tuatara.io:read-fd-exactly fd (min length 65536) at)))
                   (when (and start (tuatara.io:octets-at-p start '(#xFF #xD8 #xFF)))
                     (multiple-value-bind (width height) (tuatara.io:octets-picture-size start)
                       (push (make-inner-picture label
                                                 (format nil "~a, from octet ~:d, ~a" place at (octets-words length))
                                                 width height (list :octets at length :jpeg))
                             pictures))))))
             (int (at n big) (tuatara.io:octets-integer octets at n :big-endian big))
             (thumbnail-tag (fields tag)
               (third (find-if (lambda (f) (and (eq (first f) :thumbnail) (= (second f) tag))) fields)))
             (exif (start end)
               (let* ((base (+ start 6))
                      (fields (exif-read (subseq octets base end) 0))
                      (offset (thumbnail-tag fields #x0201))
                      (length (thumbnail-tag fields #x0202)))
                 (when (and (consp offset) (consp length) (integerp (first offset)) (integerp (first length))
                            (<= (+ base (first offset) (first length)) end))
                   (kept "its EXIF thumbnail" "in its APP1 segment" (+ base (first offset)) (first length)))))
             (photoshop (start end)
               ;; the image resources after "Photoshop 3.0": 8BIM, an id, a name, a size, the data
               (let ((at (+ start 14)) (count 0))
                 (loop while (and (<= (+ at 12) end) (< (incf count) 1000) (tuatara.io:octets-at-p octets "8BIM" at))
                       do (let* ((id (int (+ at 4) 2 t))
                                 (name (aref octets (+ at 6)))
                                 (size-at (+ at 6 (* 2 (ceiling (1+ name) 2))))
                                 (size (and (<= (+ size-at 4) end) (int size-at 4 t)))
                                 (data (+ size-at 4)))
                            (unless (and size (<= (+ data size) end)) (return))
                            (when (and (member id '(1033 1036)) (> size 28) (eql (int data 4 t) 1))
                              (kept "Photoshop's thumbnail" (format nil "in its APP13 segment (resource ~d)" id)
                                    (+ data 28) (- size 28)))
                            (setf at (+ data size (mod size 2)))))))
             (mpf (start end)
               ;; the MP index: a TIFF structure after "MPF", its offsets from that structure's start
               (let* ((base (+ start 4))
                      (index (subseq octets base end))
                      (big (tuatara.io:octets-at-p index "MM"))
                      (entry (and (or big (tuatara.io:octets-at-p index "II"))
                                  (assoc #xB002 (tuatara.io:tiff-entries index :most 64 :little (not big)))))
                      (count (and entry (floor (third entry) 16)))
                      (at (and entry (> (third entry) 4) (tuatara.io:octets-integer index (fifth entry) 4
                                                                                    :big-endian big))))
                 (when at
                   (loop for i below (min count 64)
                         for e = (+ at (* 16 i))
                         for attribute = (tuatara.io:octets-integer index e 4 :big-endian big)
                         for length = (tuatara.io:octets-integer index (+ e 4) 4 :big-endian big)
                         for offset = (tuatara.io:octets-integer index (+ e 8) 4 :big-endian big)
                         while (and attribute length offset)
                         ;; the first, at 0, is the picture itself
                         when (plusp offset)
                           do (let ((type (logand attribute #xFFFFFF)))
                                (kept (or (cdr (assoc type *mpf-types*))
                                          (format nil "a picture of MPF type 0x~6,'0x" type))
                                      (format nil "listed in its MPF index (APP2), number ~d" (1+ i))
                                      (+ base offset) length)))))))
      (when octets
        (tuatara.io:jpeg-segments
         octets (lambda (marker start end)
                  (cond ((and (= marker #xE1) (tuatara.io:octets-at-p octets (format nil "Exif~c~c" #\Nul #\Nul) start))
                         (exif start end))
                        ((and (= marker #xED)
                              (tuatara.io:octets-at-p octets (format nil "Photoshop 3.0~c" #\Nul) start))
                         (photoshop start end))
                        ((and (= marker #xE2) (tuatara.io:octets-at-p octets (format nil "MPF~c" #\Nul) start))
                         (mpf start end)))
                  nil))))
    (nreverse pictures)))

;;; Animated PNG: frames in fcTL and fdAT chunks

(defparameter *apng-largest* (* 64 1024 1024)
  "The most octets an animated PNG may have for its frames to be read: a
larger one is shown and listed as its first picture alone.")

(defparameter *png-chunks-kept-for-frames* '("PLTE" "tRNS" "gAMA" "cHRM" "sRGB" "iCCP" "sBIT" "cICP")
  "The chunks of an animated PNG before its picture data that a frame made a
PNG of its own keeps: those that say how its pixels are read.")

(defun map-png-chunks (function octets)
  "Call FUNCTION with the type (four letters), where the data begins and where
it ends of each chunk of the PNG in OCTETS, in order, to IEND: at most
100,000, each checked to lie within OCTETS."
  (let ((at 8))
    (loop repeat 100000
          while (<= (+ at 12) (length octets))
          do (let* ((length (tuatara.io:octets-integer octets at 4 :big-endian t))
                    (data (+ at 8))
                    (end (+ data length)))
               (when (> (+ end 4) (length octets)) (return))
               (let ((type (tuatara.text:octets-latin-1 octets :start (+ at 4) :end data)))
                 (funcall function type data end)
                 (when (string= type "IEND") (return)))
               (setf at (+ end 4))))))

(defun apng-frames (octets)
  "The frames of the animated PNG in OCTETS, in order, as plists of :WIDTH,
:HEIGHT, :X, :Y, :DELAY (seconds, a ratio), :DISPOSE and :BLEND (their
numbers), :AT (the octet of the frame's fcTL chunk), :DEFAULT (true for the
frame that is the picture data, IDAT, itself) and :DATA (a list of (START .
END) of its compressed pixels); and as a second value true when the picture
data (IDAT) is no frame, a picture that programs showing the animation leave
out. At most *INNER-PICTURES-MOST* frames."
  (let ((frames '()) (idat-seen nil) (idat-framed nil))
    (map-png-chunks
     (lambda (type data end)
       (flet ((int (at n) (tuatara.io:octets-integer octets at n :big-endian t)))
         (cond ((and (string= type "fcTL") (= (- end data) 26) (< (length frames) *inner-pictures-most*))
                (let ((denominator (int (+ data 22) 2)))
                  (push (list :width (int (+ data 4) 4) :height (int (+ data 8) 4)
                              :x (int (+ data 12) 4) :y (int (+ data 16) 4)
                              :delay (/ (int (+ data 20) 2) (if (zerop denominator) 100 denominator))
                              :dispose (aref octets (+ data 24)) :blend (aref octets (+ data 25))
                              :at (- data 8) :default (not idat-seen) :data '())
                        frames)))
               ((string= type "IDAT")
                (unless idat-seen
                  (setf idat-seen t idat-framed (and frames t)))
                (when (and frames (getf (first frames) :default))
                  (push (cons data end) (getf (first frames) :data))))
               ((and (string= type "fdAT") frames (> (- end data) 4))
                (push (cons (+ data 4) end) (getf (first frames) :data))))))
     octets)
    (values (mapcar (lambda (f) (setf (getf f :data) (reverse (getf f :data))) f) (nreverse frames))
            (and idat-seen (not idat-framed)))))

(defun png-chunk-octets (type data)
  "The octets of a PNG chunk of TYPE holding DATA: its length, type, data and CRC."
  (let ((out (make-array (+ 12 (length data)) :element-type '(unsigned-byte 8))))
    (tuatara.io:octets-put-integer out (length data) 4 :at 0 :big-endian t)
    (loop for c across type for i from 4 do (setf (aref out i) (char-code c)))
    (replace out data :start1 8)
    (tuatara.io:octets-put-integer out (tuatara.io:crc32 out 4 (+ 8 (length data))) 4 :at (+ 8 (length data))
                                   :big-endian t)
    out))

(defun apng-frame-octets (octets index)
  "Frame INDEX (from 0) of the animated PNG in OCTETS, or with :PICTURE its
picture data (IDAT), made a PNG of its own, as a program shut in can draw it
without reading animations: the PNG's header with the frame's size, the
chunks that say how its pixels are read (*PNG-CHUNKS-KEPT-FOR-FRAMES*), its
compressed pixels as picture data, and the end. NIL when there is no such
frame."
  (let ((frame (if (eq index :picture) (list :data '()) (nth index (apng-frames octets))))
        (header nil) (kept '()) (data-seen nil))
    (when frame
      (map-png-chunks (lambda (type data end)
                        (cond ((string= type "IHDR") (setf header (subseq octets data end)))
                              ((member type '("IDAT" "fdAT") :test #'string=)
                               (setf data-seen t)
                               (when (and (eq index :picture) (string= type "IDAT"))
                                 (push (cons data end) (getf frame :data))))
                              ((and (not data-seen) (member type *png-chunks-kept-for-frames* :test #'string=))
                               (push (png-chunk-octets type (subseq octets data end)) kept))))
                      octets)
      (when (and header (= (length header) 13) (getf frame :data))
        (unless (eq index :picture)
          (tuatara.io:octets-put-integer header (getf frame :width) 4 :at 0 :big-endian t)
          (tuatara.io:octets-put-integer header (getf frame :height) 4 :at 4 :big-endian t))
        (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
               (coerce #(137 80 78 71 13 10 26 10) '(simple-array (unsigned-byte 8) (*)))
               (png-chunk-octets "IHDR" header)
               (append (nreverse kept)
                       (loop for (start . end) in (if (eq index :picture)
                                                      (reverse (getf frame :data))
                                                      (getf frame :data))
                             collect (png-chunk-octets "IDAT" (subseq octets start end)))
                       (list (png-chunk-octets "IEND" #()))))))))

(defun apng-inner-pictures (fd head size)
  "The pictures of the animated PNG open as file descriptor (FD), whose first
octets are HEAD, of SIZE octets: the picture itself, then each frame of the
animation that is not it, with where it lies on the picture, how long it is
shown and how it is laid over the frame before. Read only when its acTL
chunk is among HEAD, as the format puts it before the picture data, and the
file is no larger than *APNG-LARGEST*."
  (when (and (search "acTL" (tuatara.text:octets-latin-1 head)) (<= size *apng-largest*))
    (let ((octets (tuatara.io:read-fd-exactly fd size 0)))
      (when octets
        (multiple-value-bind (frames hidden) (apng-frames octets)
          (multiple-value-bind (width height) (tuatara.io:octets-picture-size head)
            (cons (make-inner-picture (if hidden
                                          "the picture itself, which programs showing the animation leave out"
                                          "the picture itself, the animation's first frame")
                                      "its picture data (IDAT)" width height (list :apng-frame :picture))
                  (loop for frame in frames
                        for n from 0
                        unless (getf frame :default)
                          collect (destructuring-bind (&key width height x y delay dispose blend at &allow-other-keys)
                                      frame
                                    (make-inner-picture
                                     (format nil "frame ~d of ~d" (1+ n) (length frames))
                                     (format nil "at ~d, ~d on the picture; shown ~,2f s; ~a; then ~a; its fcTL ~
                                                  chunk at octet ~:d"
                                             x y (float delay)
                                             (if (eql blend 1)
                                                 "laid over the frame before"
                                                 "replacing what is under it")
                                             (case dispose (1 "cleared") (2 "the frame before put back")
                                                   (t "left as it is"))
                                             at)
                                     width height (list :apng-frame n) (list x y)))))))))))

;;; WebP: an animation's frames in ANMF chunks

(defun webp-inner-pictures (fd head size)
  "The pictures of the animated WebP open as file descriptor (FD), whose
first octets are HEAD, of SIZE octets: the picture itself (its first
frame), then each frame after it, with where it lies on the picture, how
long it is shown and how it is laid over the frame before. Each chunk's
header is read where it stands, at most 100,000 chunks and
*INNER-PICTURES-MOST* frames; NIL for a WebP that is no animation."
  (when (and (tuatara.io:octets-at-p head "RIFF") (tuatara.io:octets-at-p head "WEBPVP8X" 8))
    (let ((frames '()) (at 12))
      (loop repeat 100000
            while (and (<= (+ at 8) size) (< (length frames) *inner-pictures-most*))
            do (let* ((header (tuatara.io:read-fd-exactly fd 8 at))
                      (type (and header (tuatara.text:octets-latin-1 header :end 4)))
                      (length (and header (tuatara.io:octets-integer header 4 4))))
                 (unless (and type length (<= (+ at 8 length) size)) (return))
                 (when (and (string= type "ANMF") (>= length 16))
                   (let ((frame (tuatara.io:read-fd-exactly fd 16 (+ at 8))))
                     (flet ((int (from) (tuatara.io:octets-integer frame from 3)))
                       (push (list :x (* 2 (int 0)) :y (* 2 (int 3)) :width (1+ (int 6)) :height (1+ (int 9))
                                   :duration (int 12) :blend (not (logtest 2 (aref frame 15)))
                                   :dispose (logtest 1 (aref frame 15)) :at at)
                             frames))))
                 (setf at (+ at 8 length (mod length 2)))))
      (setf frames (nreverse frames))
      (when (rest frames)
        (multiple-value-bind (width height) (tuatara.io:octets-picture-size head)
          (cons (make-inner-picture "the picture itself, the animation's first frame"
                                    (format nil "its first frame at octet ~:d" (getf (first frames) :at))
                                    width height (list :frame 0))
                (loop for frame in (rest frames)
                      for n from 1
                      collect (destructuring-bind (&key x y width height duration blend dispose at) frame
                                (make-inner-picture
                                 (format nil "frame ~d of ~d" (1+ n) (length frames))
                                 (format nil "at ~d, ~d on the picture; shown ~,2f s; ~a; then ~a; its ANMF chunk at ~
                                              octet ~:d"
                                         x y (/ duration 1000.0)
                                         (if blend "laid over the frame before" "replacing what is under it")
                                         (if dispose "cleared" "left as it is") at)
                                 width height (list :frame n) (list x y))))))))))

;;; HEIF (HEIC, AVIF): the items of its meta box

(defparameter *heif-read* (* 4 1024 1024)
  "The most octets at the start of a HEIF read for its meta box, where its
items, their places and their roles are kept.")

(defparameter *heif-picture-kinds* '("hvc1" "av01" "jpeg" "grid" "iden" "iovl" "avc1" "j2k1")
  "The kinds of HEIF item that are pictures: the others (Exif, mime for XMP)
hold what the file says of itself.")

(defun heif-structure (octets)
  "The items of the HEIF in OCTETS, from its meta box, as two values: a list
of (ID . PLIST), one an item, PLIST of :ID, :KIND (four letters), :NAME, :HIDDEN, :AT
and :LENGTH (where its data lies in OCTETS' file, the first extent, NIL when
not said or kept otherwise), :WIDTH and :HEIGHT (its ispe property), :AUX
\(its auxC property's name), :REFS (a list of (TYPE . TO-IDS) from its iref
entries); and the primary item's id. At most 10,000 items, each structure
checked to lie within OCTETS; NIL when there is no meta box."
  (let* ((n (length octets))
         (meta (tuatara.io:box-find octets 0 n "meta")))
    (when meta
      (let* ((children (tuatara.io:box-children octets (+ 4 (second meta)) (third meta)))
             (items (make-hash-table)) (order '()) (primary nil))
        (labels ((box (type) (find type children :key #'first :test #'string=))
                 (int (at size) (and (< (+ at size -1) n) (octets-integer octets at size :big-endian t)))
                 (item (id) (or (gethash id items)
                                (progn (push id order) (setf (gethash id items) (cons id (list :id id)))))))
          (let ((pitm (box "pitm")))
            (when pitm (setf primary (int (+ (second pitm) 4) (if (zerop (aref octets (second pitm))) 2 4)))))
          (let ((iinf (box "iinf")))
            (when (and iinf (< (second iinf) n))
              (let ((at (+ (second iinf) (if (zerop (aref octets (second iinf))) 6 8))))
                (loop for (type start end) in (tuatara.io:box-children octets at (third iinf))
                      when (and (string= type "infe") (< (+ start 12) n) (>= (aref octets start) 2))
                        do (let* ((wide (>= (aref octets start) 3))
                                  (id (int (+ start 4) (if wide 4 2)))
                                  (kind-at (+ start (if wide 10 8)))
                                  (name-end (or (position 0 octets :start (+ kind-at 4) :end (min end n)) (min end n)))
                                  (entry (item id)))
                             (setf (getf (cdr entry) :kind) (octets-text octets kind-at (+ kind-at 4))
                                   (getf (cdr entry) :name) (escape-controls
                                                             (octets-text octets (+ kind-at 4)
                                                                          (min name-end (+ kind-at 4 200))))
                                   (getf (cdr entry) :hidden) (logtest 1 (or (int (+ start 1) 3) 0))
                                   (gethash id items) entry))))))
          (let ((iloc (box "iloc")) (idat (box "idat")))
            (when (and iloc (int (+ (second iloc) 4) 2))
              (let* ((at (second iloc)) (version (aref octets at))
                     (sizes (int (+ at 4) 2))
                     (offset-size (ldb (byte 4 12) sizes)) (length-size (ldb (byte 4 8) sizes))
                     (base-size (ldb (byte 4 4) sizes))
                     (index-size (if (>= version 1) (ldb (byte 4 0) sizes) 0))
                     (count (int (+ at 6) (if (< version 2) 2 4)))
                     (p (+ at (if (< version 2) 8 10))))
                (dotimes (i (min (or count 0) 10000))
                  (let* ((id (int p (if (< version 2) 2 4)))
                         (method (progn (incf p (if (< version 2) 2 4))
                                        (if (>= version 1) (prog1 (logand 15 (or (int p 2) 0)) (incf p 2)) 0))))
                    (incf p 2)
                    (let ((base (if (zerop base-size) 0 (int p base-size)))
                          (extents (progn (incf p base-size) (int p 2))))
                      (incf p 2)
                      (unless (and id base extents) (return))
                      (let ((first nil) (total 0))
                        (dotimes (e (min extents 1000))
                          (incf p index-size)
                          (let ((offset (if (zerop offset-size) 0 (int p offset-size)))
                                (length (if (zerop length-size) 0 (int (+ p offset-size) length-size))))
                            (unless (and offset length) (return))
                            (when (zerop e) (setf first offset))
                            (incf total length))
                          (incf p (+ offset-size length-size)))
                        (when first
                          (let ((entry (item id)))
                            ;; construction method 1: within the idat box; 2 (from other items): not followed
                            (setf (getf (cdr entry) :at) (case method
                                                           (0 (+ base first))
                                                           (1 (and idat (+ (second idat) base first))))
                                  (getf (cdr entry) :length) (and (plusp total) total)
                                  (gethash id items) entry))))))))))
          (let ((iref (box "iref")))
            (when (and iref (< (second iref) n))
              (let ((wide (plusp (aref octets (second iref)))))
                (loop for (type start end) in (tuatara.io:box-children octets (+ 4 (second iref)) (third iref))
                      do (let* ((from (int start (if wide 4 2)))
                                (count (int (+ start (if wide 4 2)) 2))
                                (to (loop for k below (min (or count 0) 1000)
                                          for at = (+ start (if wide 6 4) (* k (if wide 4 2)))
                                          while (<= (+ at (if wide 4 2)) end)
                                          collect (int at (if wide 4 2)))))
                           (when from
                             (let ((entry (item from)))
                               (push (cons type to) (getf (cdr entry) :refs))
                               (setf (gethash from items) entry))))))))
          (let* ((iprp (box "iprp"))
                 (parts (and iprp (tuatara.io:box-children octets (second iprp) (third iprp))))
                 (ipco (find "ipco" parts :key #'first :test #'string=))
                 (ipma (find "ipma" parts :key #'first :test #'string=))
                 (properties (and ipco (coerce (tuatara.io:box-children octets (second ipco) (third ipco)) 'vector))))
            (when (and ipma properties (< (+ (second ipma) 8) n))
              (let* ((at (second ipma)) (version (aref octets at)) (big-index (logtest 1 (aref octets (+ at 3))))
                     (count (int (+ at 4) 4)) (p (+ at 8)))
                (dotimes (i (min (or count 0) 10000))
                  (let ((id (int p (if (< version 1) 2 4))))
                    (incf p (if (< version 1) 2 4))
                    (let ((associations (and (< p n) (aref octets p))))
                      (incf p)
                      (unless (and id associations) (return))
                      (dotimes (k associations)
                        (let* ((raw (int p (if big-index 2 1)))
                               (index (and raw (logand raw (if big-index #x7FFF #x7F))))
                               (property (and index (plusp index) (<= index (length properties))
                                              (aref properties (1- index)))))
                          (incf p (if big-index 2 1))
                          (when property
                            (destructuring-bind (type start end) property
                              (let ((entry (item id)))
                                (cond ((and (string= type "ispe") (<= (+ start 12) end))
                                       (setf (getf (cdr entry) :width) (int (+ start 4) 4)
                                             (getf (cdr entry) :height) (int (+ start 8) 4)))
                                      ((and (string= type "auxC") (< (+ start 4) end))
                                       (setf (getf (cdr entry) :aux)
                                             (escape-controls
                                              (octets-text octets (+ start 4)
                                                           (or (position 0 octets :start (+ start 4) :end end)
                                                               end))))))
                                (setf (gethash id items) entry))))))))))))
          (values (mapcar (lambda (id) (gethash id items)) (nreverse order)) primary))))))

(defun heif-aux-words (name)
  "What an auxiliary picture of a HEIF is, by the NAME its auxC property gives."
  (cond ((search "gainmap" name) "an HDR gain map")
        ((search "matte" name) "a portrait matte")
        ((or (search "depth" name) (search "auxid:2" name)) "a depth map")
        ((or (search "alpha" name) (search "auxid:1" name)) "its transparency (alpha)")
        (t (format nil "an auxiliary picture (~a)" name))))

(defun heif-inner-pictures (fd head size)
  "The pictures of the HEIF (HEIC, AVIF) open as file descriptor (FD), whose
first octets are HEAD, of SIZE octets: the picture itself (its primary
item), then each other item that is a picture, named by what it is to the
others (a thumbnail, a depth map, an HDR gain map, a tile of a picture made
of tiles), with its size and where its data lies. Only an item kept as a
JPEG is drawn; the others are listed. NIL when it keeps one picture only."
  (declare (ignore head))
  (let ((octets (tuatara.io:read-fd-exactly fd (min size *heif-read*) 0)))
    (multiple-value-bind (items primary) (and octets (heif-structure octets))
      (let* ((pictures (remove-if-not (lambda (i) (member (getf (cdr i) :kind) *heif-picture-kinds* :test #'string=))
                                      items))
             (main (find primary pictures :key (lambda (i) (getf (cdr i) :id))))
             (tiles (loop for i in pictures append (cdr (assoc "dimg" (getf (cdr i) :refs) :test #'string=)))))
        (when (and main (rest pictures))
          (flet ((place (entry)
                   (destructuring-bind (&key id kind at length hidden &allow-other-keys) (cdr entry)
                     (format nil "item ~d (~a)~:[~;, hidden~]~@[, its data from octet ~:d~]~@[, ~a~]"
                             id kind hidden at (and length (octets-words length))))))
            (cons (make-inner-picture "the picture itself (its primary item)" (place main)
                                      (getf (cdr main) :width) (getf (cdr main) :height) (list :frame 0))
                  (loop for entry in pictures
                        for plist = (cdr entry)
                        for count from 1
                        while (< count *inner-pictures-most*)
                        unless (or (eq entry main) (member (getf plist :id) tiles))
                          collect (let* ((thumbnail (assoc "thmb" (getf plist :refs) :test #'string=))
                                         (aux (assoc "auxl" (getf plist :refs) :test #'string=))
                                         (grid (assoc "dimg" (getf plist :refs) :test #'string=))
                                         (of (cdr (or thumbnail aux))))
                                    (make-inner-picture
                                     (format nil "~a~@[ of item ~{~d~^, ~}~]"
                                             (cond (thumbnail "a thumbnail")
                                                   (aux (heif-aux-words (or (getf plist :aux) "")))
                                                   (grid (format nil "a picture made of ~d tiles" (length (cdr grid))))
                                                   ((getf plist :aux) (heif-aux-words (getf plist :aux)))
                                                   (t "another picture"))
                                             of)
                                     (place entry) (getf plist :width) (getf plist :height)
                                     (and (string= (getf plist :kind) "jpeg") (getf plist :at) (getf plist :length)
                                          (<= (+ (getf plist :at) (getf plist :length)) size)
                                          (list :octets (getf plist :at) (getf plist :length) :jpeg))))))))))))

(defun inner-pictures (fd head kind &key (size (tuatara.io:fd-size fd)))
  "The pictures the file open as file descriptor (FD), whose first octets are
HEAD, of KIND (TUATARA.IO:OCTETS-KIND's keyword) and SIZE octets, keeps, the
one a viewer shows first, as INNER-PICTURE records; NIL for a kind not read
so or a file keeping one picture only. Read by Tuatara alone, a few small
reads, nothing in the file run."
  (let ((pictures (and head size
                       (ignoring-errors-reported ("reading the pictures inside a file")
                         (case kind
                           (:tiff (and (tuatara.io:tiff-octets-p head) (tiff-inner-pictures fd head size)))
                           (:jpeg (jpeg-inner-pictures fd head size))
                           (:png (apng-inner-pictures fd head size))
                           (:webp (webp-inner-pictures fd head size))
                           ((:heif :avif) (heif-inner-pictures fd head size)))))))
    (and (rest pictures) pictures)))

;;; Drawing and comparing

(defun inner-picture-image (fd kind picture width height &key cancel enlarge)
  "PICTURE, one of the file open as file descriptor (FD), of KIND, drawn in
the sandbox no larger than WIDTH by HEIGHT pixels (with ENLARGE, as large as
fits): two values, as DECODE-IMAGE gives them. A picture of no pixels (an
empty layer) is said not to be drawn."
  (if (null (inner-picture-source picture))
      (values nil (list (cons ", not drawn: it has no pixels" :comment)))
      (destructuring-bind (how &optional n &rest more) (inner-picture-source picture)
        (declare (ignore more))
        (ecase how
          (:frame (decode-image fd kind width height :cancel cancel :enlarge enlarge :frame n))
          (:apng-frame (let* ((size (tuatara.io:fd-size fd))
                              (octets (and size (<= size *apng-largest*) (tuatara.io:read-fd-exactly fd size 0)))
                              (frame (and octets (apng-frame-octets octets n))))
                         (if frame
                             (decode-picture frame :png width height :cancel cancel :enlarge enlarge)
                             (values nil (list (cons ", not drawn: the frame could not be read" :comment))))))
          (:octets (destructuring-bind (start length kind) (rest (inner-picture-source picture))
                     (let ((octets (tuatara.io:read-fd-exactly fd length start)))
                       (if octets
                           (decode-picture octets kind width height :cancel cancel :enlarge enlarge)
                           (values nil (list (cons ", not drawn: it could not be read" :comment)))))))))))

(defun images-difference (a b &optional at)
  "The share, from 0 to 1, of the pixels of the picture B whose colour
differs from A's by more than *INNER-PICTURE-DIFFERENCE-STEP* in red, green,
blue or (both having it) alpha. With AT, (X Y), B is a part of A's size laid
on it there, and only B's pixels more opaque than not are counted, those
outside A and those of B's edge left out (drawn small, A blends what lies
on both sides of it); NIL when none is left. Without, A and B are the same
picture drawn twice: NIL when their sizes differ by more than a pixel, which
comes of other proportions; a pixel more one way is a rounding, and they are
compared over the part both have."
  (let* ((wa (tuatara.frontend:image-width a)) (ha (tuatara.frontend:image-height a))
         (wb (tuatara.frontend:image-width b)) (hb (tuatara.frontend:image-height b))
         (pa (tuatara.frontend:image-pixels a)) (pb (tuatara.frontend:image-pixels b))
         (ca (tuatara.frontend:image-channels a)) (cb (tuatara.frontend:image-channels b))
         (dx (if at (first at) 0)) (dy (if at (second at) 0))
         (channels (if at 3 (min ca cb)))
         (step *inner-picture-difference-step*)
         (counted 0) (differing 0))
    (unless (or at (and (<= (abs (- wa wb)) 1) (<= (abs (- ha hb)) 1)))
      (return-from images-difference nil))
    (dotimes (y hb)
      (dotimes (x wb)
        (let ((ax (+ x dx)) (ay (+ y dy)))
          (when (and (< -1 ax wa) (< -1 ay ha)
                     (or (null at) (< wb 3) (< hb 3) (and (< 0 x (1- wb)) (< 0 y (1- hb)))))
            (let ((ia (* ca (+ ax (* ay wa)))) (ib (* cb (+ x (* y wb)))))
              (unless (and at (= cb 4) (< (aref pb (+ ib 3)) 128))
                (incf counted)
                (when (loop for c below channels
                            thereis (> (abs (- (aref pa (+ ia c)) (aref pb (+ ib c)))) step))
                  (incf differing))))))))
    (and (plusp counted) (/ differing counted))))

(defun difference-words (share &optional part)
  "SHARE, of the pixels of a picture differing from the one shown (from 0 to
1), in words; with PART, of a layer from the part of it the layer lies on."
  (let ((shown (if part "the part of the picture shown under it" "the picture shown")))
    (cond ((zerop share) (format nil "looks the same as ~a" shown))
          ((< share 1/100) (format nil "differs from ~a in less than 1% of it" shown))
          (t (format nil "differs from ~a in ~d% of it" shown (round (* 100 share)))))))

(defun inner-pictures-compared (fd kind pictures &key cancel)
  "Each of PICTURES, of the file open as file descriptor (FD), of KIND, past
the first (the one shown), compared with the first, as words, one for each:
both drawn no larger than *INNER-PICTURE-COMPARED-SIZE* in the sandbox. At
most *INNER-PICTURES-COMPARED* are compared, those past them said to be not."
  (let* ((size *inner-picture-compared-size*)
         (shown (inner-picture-image fd kind (first pictures) size size :cancel cancel :enlarge t)))
    (loop for picture in (rest pictures)
          for n from 1
          collect (cond ((> n *inner-pictures-compared*)
                         (format nil "not compared: only the first ~d are (*inner-pictures-compared*)"
                                 *inner-pictures-compared*))
                        ((null (inner-picture-source picture))
                         (if (inner-picture-width picture)
                             "not compared: not drawn here"
                             "not compared: it has no pixels"))
                        ((null shown) "not compared: the picture shown could not be drawn")
                        ((and cancel (car cancel)) "not compared: stopped")
                        (t (let* ((region (inner-picture-region picture))
                                  ;; a layer drawn at the scale the picture shown was, to lie on it
                                  (scale (and region (inner-picture-width (first pictures))
                                              (/ (tuatara.frontend:image-width shown)
                                                 (max 1 (inner-picture-width (first pictures))))))
                                  (width (if scale (max 1 (round (* scale (inner-picture-width picture)))) size))
                                  (height (if scale (max 1 (round (* scale (inner-picture-height picture)))) size)))
                             (multiple-value-bind (image notes)
                                 (inner-picture-image fd kind picture width height :cancel cancel :enlarge t)
                               (let ((share (and image (images-difference
                                                        shown image
                                                        (and scale (mapcar (lambda (v) (round (* scale v))) region))))))
                                 (cond ((null image)
                                        (format nil "not compared: it could not be drawn~@[ (~a)~]"
                                                (and notes (string-left-trim ", " (car (first notes))))))
                                       ((and (null share) region) "not compared: it lies outside the picture shown")
                                       ((null share) "not compared: of other proportions than the picture shown")
                                       (t (difference-words share region)))))))))))

;;; M-x show-file-metadata

(defun inner-picture-words (picture)
  "What PICTURE is and its size, in words: page 2 of 3, 32 × 24."
  (format nil "~a~@[, ~a~]" (inner-picture-label picture)
          (and (inner-picture-width picture) (inner-picture-height picture)
               (tuatara.io:size-words (inner-picture-width picture) (inner-picture-height picture)))))

(defun inner-pictures-metadata (path kind &key fd size)
  "A list of the one section \"Pictures inside\" of the note of the file at
PATH, of KIND, open as file descriptor (FD), of SIZE octets, when it keeps
more pictures than the one shown: each with what it is, its size, where it
lies, how it compares with the picture shown, and a link that shows it in
the image's view (\\`RET' on it). NIL when it keeps one picture only."
  (let* ((head (and (member kind '(:tiff :jpeg :png :webp :heif :avif)) (tuatara.io:read-fd-start fd path size 4096)))
         (pictures (and head (inner-pictures fd head kind :size size))))
    (when pictures
      (let ((compared (inner-pictures-compared fd kind pictures
                                               :cancel (and tuatara.runtime:*job*
                                                            (tuatara.runtime:job-cancel tuatara.runtime:*job*)))))
        (list (with-output-to-string (s)
                (format s "* Pictures inside~%")
                (loop for picture in pictures
                      for n from 1
                      for words in (cons "the picture a viewer shows" compared)
                      do (format s "- Picture ~d :: ~a~@[ ~a~]~%" n
                                 (metadata-value (format nil "~a; ~a; ~a" (inner-picture-words picture)
                                                         (inner-picture-place picture) words))
                                 (and (inner-picture-source picture)
                                      (org-link-text (format nil "file:~a::picture ~d" path n) "show it"))))
                (terpri s)))))))

(defun inner-picture-search-index (search count)
  "The index from 0 of the picture SEARCH names, the part after :: of a link
to a file (picture 2 names the second), when it is among COUNT pictures; NIL
otherwise."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^picture (\\d{1,6})$" search)
    (let ((n (and match (parse-integer (aref groups 0)))))
      (and n (<= 1 n count) (1- n)))))

;;; The editing data a PDF keeps: Illustrator's own file, Photoshop's and others' private data

(defparameter *pdf-editing-applications*
  '(("/Illustrator" . "Illustrator") ("/AdobePhotoshop" . "Photoshop") ("/Photoshop" . "Photoshop")
    ("/InDesign" . "InDesign"))
  "The programs a PDF's page-piece dictionaries (/PieceInfo) are known to name,
by the name they keep their data under.")

(defparameter *illustrator-data-read* (* 16 1024 1024)
  "The most octets of Illustrator's own file, kept in a PDF, read to find its
creator and layers, its parts decoded and put together: the rest is not
read.")

(defun pdf-private-streams (octets private)
  "The streams the private data PRIVATE, a dictionary of a PDF in OCTETS,
names directly, as a list of (KEY . OBJECT), OBJECT a PDF-INDIRECT."
  (loop for (key . value) in (and (pdf-dictionary-p private) private)
        for object = (and (pdf-ref-p value) (pdf-find-object octets (second value) (third value)))
        when (and object (pdf-indirect-get object :stream-start)) collect (cons key object)))

(defun pdf-stream-size (object)
  "The octets OBJECT's stream takes in its PDF, as written."
  (- (pdf-indirect-get object :stream-end) (pdf-indirect-get object :stream-start)))

(defun illustrator-file-facts (text)
  "What TEXT, the start of Illustrator's own file as a PDF keeps it, says of
itself, as a plist: :CREATOR and :TITLE from its header, :PACKED, the words
for how the rest is compressed (zlib, Zstandard) or NIL, :PACKED-AT, where
the compressed part starts, and :LAYERS, the layer names found in what is
plain, before any compressed part (each \"(NAME) Ln\", at most 50)."
  (flet ((header (name)
           (multiple-value-bind (match groups)
               (cl-ppcre:scan-to-strings (format nil "(?:^|[\\r\\n])%%~a:[ \\t]*([^\\r\\n]{1,200})" name)
                                         text :end (min (length text) 65536))
             (and match (escape-controls (string-trim " " (aref groups 0)))))))
    (let* ((zstd (search "%AI24_ZStandard_Data" text))
           (zlib (search "%AI12_CompressedData" text)))
      (list :creator (header "Creator") :title (header "Title")
            :packed (cond (zstd "Zstandard") (zlib "zlib"))
            :packed-at (cond (zstd (+ zstd 20)) (zlib (+ zlib 20)))
            :layers (illustrator-layer-names (if (or zstd zlib) (subseq text 0 (or zstd zlib)) text))))))

(defun illustrator-layer-names (text &optional (start 0))
  "The names of the layers in TEXT, Illustrator's own file, from START: each
line \"(NAME) Ln\" names one; at most 50, each once, cut to 200 characters."
  (let ((names '()))
    (cl-ppcre:do-register-groups (name) ("\\(([^\\r\\n]{0,200}?)\\) Ln[\\r\\n]" text nil :start start)
      (pushnew (escape-controls name) names :test #'string=)
      (when (>= (length names) 50) (return)))
    (nreverse names)))

(defun illustrator-data-fields (octets private where changed)
  "The fields of the note for Illustrator's own file kept in the private data
PRIVATE of the PDF in OCTETS, found WHERE (Page 1, Document), last CHANGED
\(words, or NIL): how many parts (AIPrivateData1, 2...) and octets, who wrote
it, how it is compressed, and its layers' names when they could be read
\(plain, or packed with zlib). NIL when it holds no such parts."
  (let* ((parts (sort (loop for (key . object) in (pdf-private-streams octets private)
                            for n = (and (uiop:string-prefix-p "/AIPrivateData" key)
                                         (parse-integer key :start 14 :junk-allowed t))
                            when n collect (cons n object))
                      #'< :key #'car))
         (data (let ((decoded (loop with total = 0
                                    for (nil . object) in parts
                                    while (< total *illustrator-data-read*)
                                    for d = (pdf-stream-octets (pdf-indirect-get object :value)
                                                               (subseq octets (pdf-indirect-get object :stream-start)
                                                                       (pdf-indirect-get object :stream-end))
                                                               :limit *illustrator-data-read*)
                                    when d collect d and do (incf total (length d)))))
                 ;; the parts one after another, as Illustrator wrote its file
                 (let ((out (make-array (min *illustrator-data-read* (reduce #'+ decoded :key #'length))
                                        :element-type '(unsigned-byte 8)))
                       (at 0))
                   (dolist (d decoded out)
                     (replace out d :start1 at)
                     (incf at (length d))))))
         (text (tuatara.text:octets-latin-1 data))
         (facts (illustrator-file-facts text))
         (packed (getf facts :packed))
         (unpacked (and (equal packed "zlib")
                        (let ((at (position #x78 data :start (getf facts :packed-at)
                                                      :end (min (length data) (+ 4 (getf facts :packed-at))))))
                          (and at (zlib-octets data at (length data) :limit *illustrator-data-read*)))))
         (layers (remove-duplicates
                  (append (getf facts :layers)
                          (and unpacked (illustrator-layer-names (tuatara.text:octets-latin-1 unpacked))))
                  :test #'string= :from-end t))
         (version (pdf-resolve octets (pdf-get private "/CreatorVersion"))))
    (when parts
      (list (cons where
                  (format nil "Illustrator's own file, kept for editing: ~d part~:p, ~a~@[, written by ~a~]~
                               ~:[~*~;, version ~a~]~@[, titled ~a~]~@[; ~a~]~@[; last changed ~a~]"
                          (length parts) (octets-words (reduce #'+ parts :key (lambda (p) (pdf-stream-size (cdr p)))))
                          (getf facts :creator) (integerp version) version (getf facts :title)
                          (cond ((equal packed "Zstandard") "compressed with Zstandard: its layers are not read")
                                ((and (equal packed "zlib") (null unpacked))
                                 "compressed with zlib, which could not be undone: its layers are not read"))
                          changed))
            (cons (format nil "~a, Illustrator's layers" where)
                  (and layers (format nil "~{\"~a\"~^, ~}" layers)))))))

(defun pdf-editing-data-fields (octets)
  "The fields of the note for the editing data the PDF in OCTETS keeps in its
page-piece dictionaries (/PieceInfo), of the document and of each page: for
each program, its name, when it last changed the data, and how many streams
and octets its private data takes; Illustrator's own file read further
\(ILLUSTRATOR-DATA-FIELDS). Nothing of it is run."
  (let ((fields '()))
    (labels ((piece (dictionary where)
               (let ((info (pdf-resolve octets (pdf-get dictionary "/PieceInfo"))))
                 (loop for (key . value) in (and (pdf-dictionary-p info) info)
                       for data = (pdf-resolve octets value)
                       for private = (pdf-resolve octets (pdf-get data "/Private"))
                       for program = (or (cdr (assoc key *pdf-editing-applications* :test #'string=))
                                         (escape-controls (subseq key 1 (min (length key) 64))))
                       for changed = (let ((d (pdf-resolve octets (pdf-get data "/LastModified"))))
                                       (and (stringp d) (pdf-date-words d)))
                       for streams = (pdf-private-streams octets private)
                       for illustrator = (and (string= key "/Illustrator")
                                              (illustrator-data-fields octets private where changed))
                       do (if illustrator
                              (setf fields (revappend illustrator fields))
                              (push (cons where
                                          (format nil "~a's private data: ~d stream~:p, ~a, not read~
                                                       ~@[; last changed ~a~]"
                                                  program (length streams)
                                                  (octets-words (reduce #'+ streams
                                                                        :key (lambda (s) (pdf-stream-size (cdr s)))))
                                                  changed))
                                    fields))))))
      (piece (pdf-resolve octets (pdf-get (pdf-trailer octets) "/Root")) "Document")
      (let ((n 0))
        (map-pdf-pages (lambda (page media crop turn)
                         (declare (ignore media crop turn))
                         (piece page (format nil "Page ~d" (incf n))))
                       octets)))
    (nreverse fields)))
