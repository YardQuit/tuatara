;;;; lisp/viewers/inner-pictures.lisp --- The pictures a picture file keeps besides the one shown.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A picture file may keep more pictures than the one a viewer shows: a
;;;; TIFF's further pages (a scanner's other sheets, GIMP's layers, a smaller
;;;; copy, a mask) and the layers Photoshop keeps in it; a JPEG's thumbnails
;;;; (EXIF's, Photoshop's) and the pictures its MPF index lists (larger
;;;; previews, a phone's gain and depth maps). Their structure is read here, by Tuatara alone, as the
;;;; metadata is: small reads, every offset checked against the file, every
;;;; structure held to a count. Their pixels are drawn in the sandbox
;;;; (decode.lisp). M-x show-file-metadata lists them in a section "Pictures
;;;; inside", each compared with the picture shown; in an image's view n and
;;;; p step through them.

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
a picture of KIND kept in the file's octets from START, or NIL when it has
no pixels;
REGION, (X Y), where it lies on the picture shown, for a layer."
  (label nil :documentation "What INNER-PICTURE is, in words.")
  (place nil :documentation "Where INNER-PICTURE lies in its file, in words.")
  (width nil :documentation "The width of INNER-PICTURE in pixels, or NIL.")
  (height nil :documentation "The height of INNER-PICTURE in pixels, or NIL.")
  (source nil :documentation "How INNER-PICTURE is drawn: (:FRAME N), its file's picture N from 0, (:OCTETS START
LENGTH KIND), a picture kept in its file's octets, or NIL.")
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
                           (:jpeg (jpeg-inner-pictures fd head size)))))))
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
                        ((null shown) "not compared: the picture shown could not be drawn")
                        ((and cancel (car cancel)) "not compared: stopped")
                        ((null (inner-picture-source picture)) "not compared: it has no pixels")
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
  (let* ((head (and (member kind '(:tiff :jpeg)) (tuatara.io:read-fd-start fd path size 4096)))
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
