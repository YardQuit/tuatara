;;;; lisp/viewers/metadata.lisp --- What a file is and what it says of itself, read by Tuatara alone: M-x file-metadata.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x file-metadata shows, in a buffer of its own, an Org note of what the
;;;; system knows of a file (its kind by its contents, size, permissions,
;;;; owner, its four times, its SHA-256 sum and extended attributes) and what
;;;; the file says of itself: a photograph's EXIF (camera, times, settings,
;;;; GPS position, the thumbnail kept inside it) in JPEG, TIFF and raw, PNG,
;;;; WebP and HEIF; a PNG's text; XMP, wherever a file keeps it (its edit
;;;; history too); a PDF's document information, how many times it was saved,
;;;; and whether it holds JavaScript, actions or files; an MP3's ID3 tags, a
;;;; FLAC's comments; a video's or sound's times, maker, model and place
;;;; (MP4, QuickTime, M4A); an Office or OpenDocument file's own account of
;;;; itself, as its text view gives it; an OpenPGP message's keys and
;;;; passphrases, from the packets before its encrypted data.
;;;;
;;;; Everything is read here, by Tuatara: no program is run (but gpg
;;;; --list-keys, to name the owners of an encrypted file's keys from your
;;;; keyring, when *METADATA-LOOK-UP-KEY-OWNERS* allows it), nothing in the
;;;; file is run, nothing it names is fetched. The file is read without
;;;; changing its access time, at most *METADATA-LARGEST-READ* octets of it
;;;; at once, every offset and length it gives checked against what was read,
;;;; every structure held to a count; what does not make sense is left out.
;;;; A PDF's values, objects and streams are read by the PDF reader
;;;; (pdf-reader.lisp).

(in-package #:tuatara.defaults)

(defparameter *metadata-largest-read* (* 64 1024 1024)
  "The most octets of a file read at once to find what it says of itself:
a larger one is read at its start and its end, which is where its metadata
is kept.")

(defparameter *metadata-most-fields* 2000
  "The most fields read of one kind of metadata in a file: past them the
rest is left out, and that is said.")

(defvar *file-metadata-functions* '()
  "Functions of a file's PATH, its kind (TUATARA.IO:OCTETS-KIND's keyword), the
open descriptor FD and its SIZE, each returning a section of an Org note of
what the file says of itself, or NIL: the readers M-x file-metadata asks.")

;;; The note's sections

(defun metadata-section (heading fields)
  "A section of the note headed HEADING listing FIELDS, each (LABEL . VALUE),
those with no value left out; an empty string when none has one."
  (let ((fields (remove-if (lambda (f) (or (null (cdr f)) (zerop (length (cdr f))))) fields)))
    (if (null fields)
        ""
        (with-output-to-string (s)
          (format s "* ~a~%" heading)
          (metadata-lines fields s)
          (terpri s)))))

;;; Reading octets safely

(defun file-octets-for-metadata (fd size)
  "The octets of the file open as FD, of SIZE octets, read to find what it
says of itself: all of it, or its first and last halves of
*METADATA-LARGEST-READ* octets when it is larger (two values: the octets, and
true when they are cut so)."
  (let ((limit *metadata-largest-read*))
    (if (<= size limit)
        (let ((octets (make-array size :element-type '(unsigned-byte 8))))
          (values (subseq octets 0 (tuatara.io:read-fd-at fd octets 0)) nil))
        (let* ((half (floor limit 2))
               (octets (make-array (* 2 half) :element-type '(unsigned-byte 8))))
          (tuatara.io:read-fd-at fd octets 0 :end half)
          (let ((tail (make-array half :element-type '(unsigned-byte 8))))
            (tuatara.io:read-fd-at fd tail (- size half))
            (replace octets tail :start1 half))
          (values octets t)))))

;;; EXIF: a TIFF structure of tags

(defparameter *exif-tags*
  '((#x0100 . "ImageWidth") (#x0101 . "ImageLength") (#x010E . "ImageDescription") (#x010F . "Make") (#x0110 . "Model")
    (#x0112 . "Orientation") (#x011A . "XResolution") (#x011B . "YResolution") (#x0128 . "ResolutionUnit")
    (#x0131 . "Software") (#x0132 . "DateTime") (#x013B . "Artist") (#x013C . "HostComputer") (#x0201 . "ThumbnailOffset")
    (#x0202 . "ThumbnailLength") (#x0213 . "YCbCrPositioning") (#x8298 . "Copyright") (#x829A . "ExposureTime")
    (#x829D . "FNumber") (#x8822 . "ExposureProgram") (#x8827 . "ISOSpeedRatings") (#x8830 . "SensitivityType")
    (#x9000 . "ExifVersion") (#x9003 . "DateTimeOriginal") (#x9004 . "DateTimeDigitized") (#x9010 . "OffsetTime")
    (#x9011 . "OffsetTimeOriginal") (#x9012 . "OffsetTimeDigitized") (#x9101 . "ComponentsConfiguration")
    (#x9201 . "ShutterSpeedValue") (#x9202 . "ApertureValue") (#x9203 . "BrightnessValue") (#x9204 . "ExposureBiasValue")
    (#x9205 . "MaxApertureValue") (#x9206 . "SubjectDistance") (#x9207 . "MeteringMode") (#x9208 . "LightSource")
    (#x9209 . "Flash") (#x920A . "FocalLength") (#x9214 . "SubjectArea") (#x927C . "MakerNote") (#x9286 . "UserComment")
    (#x9290 . "SubSecTime") (#x9291 . "SubSecTimeOriginal") (#x9292 . "SubSecTimeDigitized") (#x9C9B . "XPTitle")
    (#x9C9C . "XPComment") (#x9C9D . "XPAuthor") (#x9C9E . "XPKeywords") (#x9C9F . "XPSubject") (#xA000 . "FlashpixVersion")
    (#xA001 . "ColorSpace") (#xA002 . "PixelXDimension") (#xA003 . "PixelYDimension") (#xA217 . "SensingMethod")
    (#xA300 . "FileSource") (#xA301 . "SceneType") (#xA401 . "CustomRendered") (#xA402 . "ExposureMode")
    (#xA403 . "WhiteBalance") (#xA404 . "DigitalZoomRatio") (#xA405 . "FocalLengthIn35mmFilm") (#xA406 . "SceneCaptureType")
    (#xA420 . "ImageUniqueID") (#xA430 . "CameraOwnerName") (#xA431 . "BodySerialNumber") (#xA432 . "LensSpecification")
    (#xA433 . "LensMake") (#xA434 . "LensModel") (#xA435 . "LensSerialNumber") (#xC4A5 . "PrintImageMatching"))
  "The names of the EXIF tags of a picture's main and EXIF parts, by number.")

(defparameter *exif-gps-tags*
  '((0 . "GPSVersionID") (1 . "GPSLatitudeRef") (2 . "GPSLatitude") (3 . "GPSLongitudeRef") (4 . "GPSLongitude")
    (5 . "GPSAltitudeRef") (6 . "GPSAltitude") (7 . "GPSTimeStamp") (8 . "GPSSatellites") (9 . "GPSStatus")
    (10 . "GPSMeasureMode") (11 . "GPSDOP") (12 . "GPSSpeedRef") (13 . "GPSSpeed") (14 . "GPSTrackRef") (15 . "GPSTrack")
    (16 . "GPSImgDirectionRef") (17 . "GPSImgDirection") (18 . "GPSMapDatum") (27 . "GPSProcessingMethod")
    (29 . "GPSDateStamp") (31 . "GPSHPositioningError"))
  "The names of the EXIF tags of a picture's GPS part, by number.")

(defun exif-type-size (type)
  "The octets one item of an EXIF (TIFF) field of TYPE takes, or NIL for a type not known."
  (case type ((1 2 6 7) 1) ((3 8) 2) ((4 9 11) 4) ((5 10 12) 8) (t nil)))

(defun exif-read (octets start)
  "The tags of the TIFF structure (EXIF) in OCTETS from START, as a list of
\(PART TAG-NAME VALUE) in order, PART one of :IMAGE, :EXIF, :GPS and
:THUMBNAIL; each offset checked, each part read once, at most
*METADATA-MOST-FIELDS* tags. Values are read as their type says: text,
numbers, fractions as written (1/125), dates as written; text and octets
to their first 4 KB."
  (let* ((big (cond ((tuatara.io:octets-at-p octets "MM" start) t) ((tuatara.io:octets-at-p octets "II" start) nil) (t (return-from exif-read '()))))
         (fields '()) (seen '()) (count 0))
    (labels ((int (at n) (octets-integer octets (+ start at) n :big-endian big))
             (value (type number at)
               ;; the value of NUMBER items of TYPE whose octets begin at AT of the structure
               (let* ((size (exif-type-size type))
                      (total (and size (* size number))))
                 (cond ((or (null size) (> (+ start at (or total 0)) (length octets)) (minusp at)) nil)
                       ;; text and octets kept to their first 4 KB: a field may claim the whole file, a thousand times
                       ((= type 2) (string-right-trim '(#\Space) (octets-text octets (+ start at) (+ start at (min total 4096)))))
                       ((member type '(1 6 7)) (subseq octets (+ start at) (+ start at (min total 4096))))
                       (t (loop for i from 0 below (min number 64)
                                for p = (+ at (* i size))
                                collect (case type
                                          (3 (int p 2)) (8 (octets-signed (int p 2) 16))
                                          (4 (int p 4)) (9 (octets-signed (int p 4) 32))
                                          (5 (cons (int p 4) (int (+ p 4) 4)))
                                          (10 (cons (octets-signed (int p 4) 32) (octets-signed (int (+ p 4) 4) 32)))
                                          (t nil)))))))
             (walk (at part)
               (loop while (and at (plusp at) (not (member at seen)) (< count *metadata-most-fields*))
                     do (push at seen)
                        (multiple-value-bind (entries reader next)
                            (tuatara.io:tiff-entries octets :start start :ifd at :most 1000 :little (not big))
                          (declare (ignore reader))
                          (loop for (tag type number nil inline) in entries
                                do (let* ((where (if (<= (* (or (exif-type-size type) 1) number) 4) inline (int inline 4)))
                                          (v (and where (< number (* 16 1024 1024)) (value type number where))))
                                     (cond ((and (= tag #x8769) (eq part :image)) (walk (and (consp v) (first v)) :exif))
                                           ((and (= tag #x8825) (eq part :image)) (walk (and (consp v) (first v)) :gps))
                                           ((= tag #xA005))
                                           (t (incf count)
                                              (push (list part tag v) fields)))))
                          ;; the part after the picture's own: its thumbnail
                          (if (eq part :image) (setf at next part :thumbnail) (return))))))
      (walk (int 4 4) :image))
    (nreverse fields)))

(defun exif-rational-words (r)
  "R, a fraction (NUMERATOR . DENOMINATOR) as EXIF keeps it, in words: 1/125 below 1, else a decimal."
  (destructuring-bind (n . d) r
    (cond ((or (null n) (null d) (zerop d)) (format nil "~a/~a" n d))
          ((integerp (/ n d)) (format nil "~d" (/ n d)))
          ((and (< (abs n) d) (= 1 (abs n))) (format nil "~d/~d" n d))
          (t (string-right-trim "." (string-right-trim "0" (format nil "~,4f" (/ n d))))))))

(defun exif-value-words (tag value)
  "VALUE of the EXIF TAG (its number) as words."
  (cond ((stringp value) value)
        ((and (vectorp value) (member tag '(#x9C9B #x9C9C #x9C9D #x9C9E #x9C9F))) (utf-16-text value))
        ((and (vectorp value) (= tag #x9286) (>= (length value) 8))
         ;; a comment: its first eight octets name its characters
         (if (tuatara.io:octets-at-p value "UNICODE") (utf-16-text value :start 8) (octets-text value 8 (length value))))
        ((and (vectorp value) (member tag '(#x9000 #xA000 0)) (<= (length value) 8))
         (if (= tag 0) (format nil "~{~d~^.~}" (coerce value 'list)) (octets-text value 0 (length value))))
        ((and (vectorp value) (= tag #x927C)) (format nil "~:d octets of the maker's own notes (not read)" (length value)))
        ((vectorp value) (if (<= (length value) 32) (tuatara.text:octets-hex value :upper-case t) (format nil "~:d octets" (length value))))
        ((listp value)
         (format nil "~{~a~^ ~}" (mapcar (lambda (v) (if (consp v) (exif-rational-words v) v)) value)))
        (t (princ-to-string value))))

(defun exif-degrees (value reference)
  "The degrees, minutes and seconds VALUE (three fractions) of a GPS position,
south or west by REFERENCE (S, W), as a signed decimal, or NIL."
  (when (and (listp value) (= (length value) 3) (every (lambda (r) (and (consp r) (car r) (cdr r) (plusp (cdr r)))) value))
    (let ((degrees (+ (/ (car (first value)) (cdr (first value)))
                      (/ (/ (car (second value)) (cdr (second value))) 60)
                      (/ (/ (car (third value)) (cdr (third value))) 3600))))
      (* (if (member reference '("S" "W") :test #'equal) -1 1) degrees))))

(defun exif-section (octets start where)
  "The section of the note giving the EXIF of the TIFF structure in OCTETS from
START, found WHERE (in its APP1 segment): its position as decimal degrees
first when it has one, then each tag, the thumbnail's size; an empty string
when it has none."
  (let ((fields (exif-read octets start)))
    (if (null fields)
        ""
        (flet ((tag (part number) (third (find-if (lambda (f) (and (eq (first f) part) (= (second f) number))) fields))))
          (let* ((latitude (exif-degrees (tag :gps 2) (tag :gps 1)))
                 (longitude (exif-degrees (tag :gps 4) (tag :gps 3)))
                 (altitude (let ((a (first (tag :gps 6)))) (and (consp a) a (cdr a) (plusp (cdr a)) (/ (car a) (cdr a)))))
                 (below (let ((r (tag :gps 5))) (and (vectorp r) (plusp (length r)) (= 1 (aref r 0)))))
                 (thumbnail (first (tag :thumbnail #x0202))))
            (metadata-section
             (format nil "EXIF (~a)" where)
             (append
              (and latitude longitude
                   (list (cons "Position" (format nil "~,6f, ~,6f~@[ (~a m ~:[above~;below~] sea level)~]"
                                                  (float latitude 1d0) (float longitude 1d0)
                                                  (and altitude (string-right-trim "." (string-right-trim "0" (format nil "~,1f" (float altitude))))) below))))
              (and thumbnail (integerp thumbnail)
                   (list (cons "Thumbnail" (format nil "~:d octets, a picture of its own kept inside it (it may show it as it was before a change)" thumbnail))))
              (loop for (part number value) in fields
                    unless (and (eq part :thumbnail) (member number '(#x0201 #x0202)))
                      collect (cons (format nil "~a~@[ (~a)~]"
                                            (or (cdr (assoc number (if (eq part :gps) *exif-gps-tags* *exif-tags*)))
                                                (format nil "Tag 0x~4,'0x" number))
                                            (case part (:gps nil) (:exif nil) (:thumbnail "thumbnail") (t nil)))
                                    (exif-value-words number value))))))))))

;;; Pictures: where each kind keeps its EXIF and text

(defun jpeg-metadata (octets)
  "The sections of the note for the JPEG picture in OCTETS: its EXIF and its comments."
  (let ((sections '()) (comments '()))
    (tuatara.io:jpeg-segments
     octets (lambda (marker start end)
              (cond ((and (= marker #xE1) (tuatara.io:octets-at-p octets (format nil "Exif~c~c" #\Nul #\Nul) start)
                          (< (+ start 6) end))
                     (push (exif-section (subseq octets (+ start 6) end) 0 "in its APP1 segment") sections))
                    ((= marker #xFE)
                     (push (octets-text octets start end) comments)))
              nil))
    (append (nreverse sections)
            (list (metadata-section "JPEG comments" (mapcar (lambda (c) (cons "Comment" c)) (nreverse comments)))))))

(defun png-metadata (octets)
  "The sections of the note for the PNG picture in OCTETS: its EXIF, its text and when it says it was last changed."
  (let ((at 8) (sections '()) (texts '()) (count 0))
    (loop while (< (+ at 8) (length octets))
          do (let* ((length (octets-integer octets at 4 :big-endian t))
                    (type (octets-text octets (+ at 4) (+ at 8)))
                    (data (+ at 8)) (end (and length (+ data length))))
               (when (or (null length) (> end (length octets)) (> (incf count) 100000)) (return))
               (cond ((string= type "IEND") (return))
                     ((string= type "eXIf") (push (exif-section (subseq octets data end) 0 "in its eXIf chunk") sections))
                     ((string= type "tIME")
                      (when (= length 7)
                        (destructuring-bind (year month day hour minute second)
                            (cons (octets-integer octets data 2 :big-endian t) (coerce (subseq octets (+ data 2) (+ data 7)) 'list))
                          (push (cons "Last changed (tIME)"
                                      (if (and (valid-date-p year month day) (< hour 24) (< minute 60) (< second 61))
                                          (utc-time-words (+ (* 86400 (day-number year month day)) (* 3600 hour) (* 60 minute) second))
                                          (format nil "no such time: year ~d, month ~d, day ~d, ~d:~d:~d" year month day hour minute second)))
                                texts))))
                     ((member type '("tEXt" "zTXt" "iTXt") :test #'string=)
                      (let ((nul (position 0 octets :start data :end end)))
                        (when nul
                          (let ((key (octets-text octets data nul)))
                            (push (cons key
                                        (cond ((string= type "tEXt") (tuatara.text:octets-latin-1 octets :start (1+ nul) :end end))
                                              ((string= type "zTXt")
                                               (let ((text (zlib-octets octets (+ nul 2) end))) (and text (tuatara.text:octets-latin-1 text))))
                                              (t (let* ((compressed (and (< (1+ nul) end) (= 1 (aref octets (1+ nul)))))
                                                        (language-end (and (< (+ nul 3) end) (position 0 octets :start (+ nul 3) :end end)))
                                                        (translated-end (and language-end (position 0 octets :start (1+ language-end) :end end))))
                                                   (when translated-end
                                                     (let ((text (if compressed
                                                                     (zlib-octets octets (1+ translated-end) end)
                                                                     (subseq octets (1+ translated-end) end))))
                                                       (and text (octets-text text 0 (length text)))))))))
                                  texts))))))
               (setf at (+ end 4))))
    (append (nreverse sections)
            (list (metadata-section "PNG text" (remove-if (lambda (f) (search "xmpmeta" (or (cdr f) ""))) (nreverse texts)))))))

(defun webp-metadata (octets)
  "The sections of the note for the WebP picture in OCTETS: its EXIF."
  (let ((at 12) (sections '()) (count 0))
    (loop while (< (+ at 8) (length octets))
          do (let* ((type (octets-text octets at (+ at 4)))
                    (length (octets-integer octets (+ at 4) 4))
                    (data (+ at 8)) (end (and length (+ data length))))
               (when (or (null length) (> end (length octets)) (> (incf count) 10000)) (return))
               (when (string= type "EXIF")
                 (let ((start (min end (if (tuatara.io:octets-at-p octets "Exif" data) (+ data 6) data))))
                   (push (exif-section (subseq octets start end) 0 "in its EXIF chunk") sections)))
               (setf at (+ end (mod length 2)))))
    (nreverse sections)))

;;; Boxes: MP4, QuickTime, HEIF

(defparameter *mp4-items*
  '(("©nam" . "Title") ("©ART" . "Artist") ("©alb" . "Album") ("©day" . "Date") ("©cmt" . "Comment") ("©too" . "Encoder")
    ("©gen" . "Genre") ("©wrt" . "Composer") ("©xyz" . "Position") ("©mak" . "Make") ("©mod" . "Model") ("©swr" . "Software")
    ("©des" . "Description") ("cprt" . "Copyright") ("desc" . "Description") ("aART" . "Album artist"))
  "The names of an MP4 or QuickTime file's own items, by type.")

(defun mp4-data-text (octets start end)
  "The value of a data box (from START below END of OCTETS) of an MP4 item: text as its type says, a number, or its size."
  (let ((type (octets-integer octets start 4 :big-endian t)) (from (+ start 8)))
    (cond ((or (null type) (> from end)) nil)
          ((member type '(1 4)) (octets-text octets from end))
          ((member type '(2 5)) (utf-16-text octets :start from :end end :big-endian t))
          ((and (member type '(0 21 22)) (<= 1 (- end from) 8)) (princ-to-string (octets-integer octets from (- end from) :big-endian t)))
          (t (format nil "~:d octets" (- end from))))))

(defun mp4-metadata (octets)
  "The sections of the note for the MP4, QuickTime or M4A file in OCTETS:
when it was made and changed (as its movie header says), its length, its
brand, and its own items (title, maker, model, place, software)."
  (let* ((n (length octets))
         (fields '())
         (brand (let ((ftyp (tuatara.io:box-find octets 0 n "ftyp"))) (and ftyp (octets-text octets (second ftyp) (+ 4 (second ftyp))))))
         (mvhd (tuatara.io:box-find octets 0 n "moov" "mvhd")))
    (when brand (push (cons "Brand" brand) fields))
    (when (and mvhd (< (second mvhd) (third mvhd)))
      (let* ((at (second mvhd)) (version (aref octets at)) (wide (= version 1))
             (made (octets-integer octets (+ at 4) (if wide 8 4) :big-endian t))
             (changed (octets-integer octets (+ at (if wide 12 8)) (if wide 8 4) :big-endian t))
             (scale (octets-integer octets (+ at (if wide 20 12)) 4 :big-endian t))
             (duration (octets-integer octets (+ at (if wide 24 16)) (if wide 8 4) :big-endian t)))
        ;; seconds since 1904, in universal time
        (flet ((when-words (s) (and s (plusp s) (time-words (+ s (* 1460 86400))))))
          (push (cons "Made (its movie header)" (when-words made)) fields)
          (push (cons "Changed (its movie header)" (when-words changed)) fields)
          (when (and scale (plusp scale) duration)
            (push (cons "Length" (format nil "~,1f seconds" (/ duration scale))) fields)))))
    ;; QuickTime's items, each in udta with a length and a language before its text
    (let ((udta (tuatara.io:box-find octets 0 n "moov" "udta")))
      (when udta
        (dolist (box (tuatara.io:box-children octets (second udta) (third udta)))
          (let ((name (cdr (assoc (first box) *mp4-items* :test #'string=))))
            (when (and name (char= (char (first box) 0) #\©) (not (tuatara.io:box-find octets (second box) (third box) "data")))
              (let ((length (octets-integer octets (second box) 2 :big-endian t)))
                (when (and length (<= (+ (second box) 4 length) (third box)))
                  (push (cons name (octets-text octets (+ (second box) 4) (+ (second box) 4 length))) fields))))))))
    ;; MP4's items (in udta's meta, or moov's), and QuickTime's keyed items (moov's meta with keys)
    (dolist (path '(("moov" "udta" "meta") ("moov" "meta")))
      (let ((meta (apply #'tuatara.io:box-find octets 0 n path)))
        (when meta
          (let* ((inner (if (tuatara.io:octets-at-p octets "hdlr" (+ (second meta) 4)) (second meta) (+ (second meta) 4)))
                 (keys (let ((box (find "keys" (tuatara.io:box-children octets inner (third meta)) :key #'first :test #'string=)))
                         (when box
                           (let ((count (octets-integer octets (+ (second box) 4) 4 :big-endian t)) (at (+ (second box) 8)) (names '()))
                             (dotimes (i (min (or count 0) 1000) (coerce (nreverse names) 'vector))
                               (let ((size (octets-integer octets at 4 :big-endian t)))
                                 (unless (and size (>= size 8) (<= (+ at size) (third box))) (return (coerce (nreverse names) 'vector)))
                                 (push (octets-text octets (+ at 8) (+ at size)) names)
                                 (incf at size)))))))
                 (ilst (find "ilst" (tuatara.io:box-children octets inner (third meta)) :key #'first :test #'string=)))
            (when ilst
              (dolist (item (tuatara.io:box-children octets (second ilst) (third ilst)))
                (let* ((index (octets-integer octets (- (second item) 4) 4 :big-endian t))
                       (name (or (cdr (assoc (first item) *mp4-items* :test #'string=))
                                 (and keys index (<= 1 index (length keys)) (aref keys (1- index)))
                                 (first item)))
                       (data (find "data" (tuatara.io:box-children octets (second item) (third item)) :key #'first :test #'string=)))
                  (when data (push (cons name (mp4-data-text octets (second data) (third data))) fields)))))))))
    (list (metadata-section "What the video or sound says of itself" (nreverse fields)))))

(defun heif-metadata (octets)
  "The sections of the note for the HEIF picture (HEIC, AVIF) in OCTETS: the
EXIF its item of that kind holds."
  (let* ((n (length octets))
         (meta (tuatara.io:box-find octets 0 n "meta")))
    (when meta
      (let* ((children (tuatara.io:box-children octets (+ 4 (second meta)) (third meta)))
             (iinf (find "iinf" children :key #'first :test #'string=))
             (iloc (find "iloc" children :key #'first :test #'string=))
             (exif-id (when iinf
                        (let ((at (+ (second iinf) (if (zerop (aref octets (second iinf))) 6 8))))
                          (loop for (type start nil) in (tuatara.io:box-children octets at (third iinf))
                                when (string= type "infe")
                                  do (let ((version (aref octets start)))
                                       (when (>= version 2)
                                         (let* ((wide (>= version 3))
                                                (id (octets-integer octets (+ start 4) (if wide 4 2) :big-endian t))
                                                (kind (octets-text octets (+ start (if wide 10 8)) (+ start (if wide 14 12)))))
                                           (when (string= kind "Exif") (return id)))))))))
             (place (when (and iloc exif-id)
                      (let* ((at (second iloc)) (version (aref octets at))
                             (sizes (octets-integer octets (+ at 4) 2 :big-endian t))
                             (offset-size (ldb (byte 4 12) sizes)) (length-size (ldb (byte 4 8) sizes))
                             (base-size (ldb (byte 4 4) sizes)) (index-size (if (>= version 1) (ldb (byte 4 0) sizes) 0))
                             (count (octets-integer octets (+ at 6) (if (< version 2) 2 4) :big-endian t))
                             (p (+ at (if (< version 2) 8 10))))
                        (dotimes (i (min (or count 0) 10000))
                          (let ((id (octets-integer octets p (if (< version 2) 2 4) :big-endian t)))
                            (incf p (if (< version 2) 2 4))
                            (when (>= version 1) (incf p 2))
                            (incf p 2)
                            (let ((base (if (zerop base-size) 0 (octets-integer octets p base-size :big-endian t)))
                                  (extents (progn (incf p base-size) (octets-integer octets p 2 :big-endian t))))
                              (incf p 2)
                              (unless (and base extents) (return))
                              (let ((first-offset nil))
                                (dotimes (e (min extents 1000))
                                  (incf p index-size)
                                  (let ((offset (if (zerop offset-size) 0 (octets-integer octets p offset-size :big-endian t))))
                                    (when (zerop e) (setf first-offset offset)))
                                  (incf p (+ offset-size length-size)))
                                (when (and (eql id exif-id) first-offset) (return (+ base first-offset))))))))))
             (start (and place (let ((skip (octets-integer octets place 4 :big-endian t))) (and skip (+ place 4 skip))))))
        (and start (< start n) (list (exif-section octets start "in its Exif item")))))))

;;; XMP, wherever a file keeps it

(defun xmp-fields (tree)
  "The fields of TREE, an XMP packet read as XML, each (NAME . VALUE): the
values of each description's attributes and elements, lists joined, a list
of structures (an edit history) one field per item."
  (let ((fields '()))
    (labels ((text (node)
               ;; an element's value: its list's items joined, a structure's fields joined, or its text
               (let ((list (find-if (lambda (c) (member (xml-name c) '("rdf:Seq" "rdf:Bag" "rdf:Alt") :test #'equal)) (xml-children node))))
                 (if list
                     (format nil "~{~a~^; ~}" (mapcar #'text (xml-local-children list "li")))
                     (let ((attributes (remove-if (lambda (a) (or (uiop:string-prefix-p "xmlns" (car a)) (member (car a) '("rdf:about" "rdf:parseType" "xml:lang") :test #'string=)))
                                                  (second node)))
                           (elements (remove-if-not #'consp (xml-children node))))
                       (if (or attributes elements)
                           (format nil "~{~a~^, ~}" (append (mapcar (lambda (a) (format nil "~a ~a" (car a) (cdr a))) attributes)
                                                            (mapcar (lambda (e) (format nil "~a ~a" (xml-name e) (text e))) elements)))
                           (string-trim '(#\Space #\Newline #\Tab) (xml-all-text node 2000)))))))
             (walk (node)
               (when (consp node)
                 (if (equal (xml-name node) "rdf:Description")
                     (progn
                       (dolist (a (second node))
                         (unless (or (uiop:string-prefix-p "xmlns" (car a)) (member (car a) '("rdf:about") :test #'string=))
                           (push (cons (car a) (cdr a)) fields)))
                       (dolist (child (xml-children node))
                         (when (consp child) (push (cons (xml-name child) (text child)) fields))))
                     (mapc #'walk (xml-children node))))))
      (walk tree))
    (let ((fields (nreverse fields)))
      (subseq fields 0 (min (length fields) *metadata-most-fields*)))))

(defun xmp-sections (octets)
  "The sections of the note giving each XMP packet in OCTETS (at most four),
the text of edits and of who made the file as programs keep it."
  (let ((start-mark (map '(vector (unsigned-byte 8)) #'char-code "<x:xmpmeta"))
        (end-mark (map '(vector (unsigned-byte 8)) #'char-code "</x:xmpmeta>"))
        (sections '()) (at 0))
    (loop repeat 4
          for start = (search start-mark octets :start2 at)
          for end = (and start (search end-mark octets :start2 start))
          while end
          do (let ((tree (ignore-errors (xml-parse (octets-text octets start (+ end (length end-mark)))))))
               (when tree
                 (push (metadata-section (format nil "XMP~@[ (packet ~d)~]" (and (or sections (search start-mark octets :start2 (1+ end))) (1+ (length sections))))
                                         (xmp-fields tree))
                       sections)))
             (setf at (1+ end)))
    (nreverse sections)))

;;; PDF

(defun pdf-metadata (octets)
  "The sections of the note for the PDF in OCTETS: its version, how many times
it was saved, its document information (title, author, the programs that
made it, its dates), whether it is encrypted, and what in it could act
\(JavaScript, actions, files kept inside it), none of it run. Its objects
are found once for all (WITH-PDF-OBJECTS-FOUND), so that a file naming a
great many it does not have is read in a bounded time."
  (with-pdf-objects-found
    (pdf-metadata-sections octets)))

(defun pdf-metadata-sections (octets)
  "The sections of PDF-METADATA's note for the PDF in OCTETS."
  (let* ((version (let ((header (tuatara.io:pdf-header-at octets)))
                    (and header (octets-text octets (+ header 5) (min (length octets) (+ header 8))))))
         (saves (loop with mark = (map '(vector (unsigned-byte 8)) #'char-code "%%EOF")
                      for at = (search mark octets) then (search mark octets :start2 (1+ at))
                      while at count t))
         (info-mark (map '(vector (unsigned-byte 8)) #'char-code "/Info"))
         (info-at (search info-mark octets :from-end t))
         (info (and info-at (pdf-resolve octets (pdf-object-at octets (+ info-at (length info-mark))))))
         (encrypted (search (map '(vector (unsigned-byte 8)) #'char-code "/Encrypt") octets))
         (acts (loop for (mark . words) in '(("/JavaScript" . "JavaScript") ("/JS" . "JavaScript") ("/OpenAction" . "an action on opening")
                                              ("/AA" . "actions on events") ("/Launch" . "an action starting a program")
                                              ("/EmbeddedFile" . "files kept inside it") ("/RichMedia" . "media")
                                              ("/XFA" . "an XFA form") ("/SubmitForm" . "a form sent somewhere"))
                     when (search (map '(vector (unsigned-byte 8)) #'char-code mark) octets)
                       collect words)))
    (list (metadata-section
           "What the PDF says of itself"
           (append (list (cons "PDF version" version)
                         (cons "Saved" (and (plusp saves) (format nil "~d time~:p~:[~; (changed after it was made: each later save is kept beside the first)~]" saves (> saves 1))))
                         (cons "Encrypted" (and encrypted "yes: what it says of itself may be scrambled")))
                   (loop for (key . value) in (and (pdf-dictionary-p info) info)
                         for v = (pdf-resolve octets value)
                         collect (cons (subseq key 1)
                                       (cond ((and (stringp v) (member key '("/CreationDate" "/ModDate") :test #'string=)) (pdf-date-words v))
                                             ((stringp v) v)
                                             (t (princ-to-string v)))))
                   (list (cons "Holds" (and acts (format nil "~{~a~^, ~} (none run here)" (remove-duplicates acts :test #'string= :from-end t))))))))))

;;; Sound: MP3's ID3 tags, FLAC's comments

(defparameter *id3-frames*
  '(("TIT2" . "Title") ("TPE1" . "Artist") ("TALB" . "Album") ("TYER" . "Year") ("TDRC" . "Recorded") ("TCON" . "Genre")
    ("TRCK" . "Track") ("TENC" . "Encoded by") ("TSSE" . "Encoder settings") ("TCOP" . "Copyright") ("TPE2" . "Album artist")
    ("TCOM" . "Composer") ("TDEN" . "Encoded") ("TDTG" . "Tagged") ("TLEN" . "Length (ms)") ("COMM" . "Comment")
    ("USLT" . "Lyrics") ("APIC" . "Picture") ("TT2" . "Title") ("TP1" . "Artist") ("TAL" . "Album") ("TYE" . "Year")
    ("COM" . "Comment") ("PIC" . "Picture") ("TEN" . "Encoded by") ("TSS" . "Encoder settings"))
  "The names of an MP3's ID3 frames, by their ids.")

(defun id3-text (octets start end encoding)
  "OCTETS from START below END as ID3 text of ENCODING (0 Latin-1, 1 UTF-16 with its mark, 2 UTF-16 big-endian, 3 UTF-8)."
  (case encoding
    ((1 2) (utf-16-text octets :start start :end end :big-endian (= encoding 2)))
    (3 (octets-text octets start end))
    (t (tuatara.text:octets-latin-1 octets :start start :end (or (position 0 octets :start start :end end) end)))))

(defun id3-split (octets start end encoding)
  "Where the first string of ENCODING in OCTETS from START below END ends,
and where the next begins, past its NUL (two octets wide in UTF-16)."
  (if (member encoding '(1 2))
      (loop for i from start below (1- end) by 2
            when (and (zerop (aref octets i)) (zerop (aref octets (1+ i)))) return (values i (+ i 2))
            finally (return (values end end)))
      (let ((nul (position 0 octets :start start :end end)))
        (if nul (values nul (1+ nul)) (values end end)))))

(defun id3-metadata (octets)
  "The sections of the note for the MP3 in OCTETS: its ID3 tags, version 2 at its start and version 1 at its end."
  (let ((fields '()))
    (when (and (tuatara.io:octets-at-p octets "ID3") (> (length octets) 10))
      (let* ((major (aref octets 3)) (flags (aref octets 5))
             (size (loop for i from 6 below 10 sum (ash (logand (aref octets i) #x7F) (* 7 (- 9 i)))))
             (end (min (length octets) (+ 10 size)))
             (tag (if (logtest flags #x80)
                      ;; unsynchronised: each FF 00 stands for FF
                      (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
                        (loop for i from 10 below end
                              do (unless (and (zerop (aref octets i)) (> i 10) (= #xFF (aref octets (1- i)))) (vector-push-extend (aref octets i) out)))
                        (coerce out '(simple-array (unsigned-byte 8) (*))))
                      (subseq octets 10 end)))
             (at (if (and (logtest flags #x40) (>= major 3)) (+ 4 (or (octets-integer tag 0 4 :big-endian t) 0)) 0))
             (id-size (if (= major 2) 3 4)) (header (if (= major 2) 6 10)) (count 0))
        (loop while (and (<= (+ at header) (length tag)) (< (incf count) *metadata-most-fields*))
              do (let* ((id (octets-text tag at (+ at id-size)))
                        (length (cond ((= major 2) (octets-integer tag (+ at 3) 3 :big-endian t))
                                      ((= major 4) (loop for i from 4 below 8 sum (ash (logand (aref tag (+ at i)) #x7F) (* 7 (- 7 i)))))
                                      (t (octets-integer tag (+ at 4) 4 :big-endian t))))
                        (data (+ at header)) (data-end (and length (+ data length))))
                   (when (or (zerop (length id)) (not (alphanumericp (char id 0))) (null length) (> data-end (length tag))) (return))
                   (let ((name (or (cdr (assoc id *id3-frames* :test #'string=)) id))
                         (encoding (if (< data data-end) (aref tag data) 0)))
                     (push (cons name
                                 (cond ((zerop length) "")
                                       ((member id '("TXXX" "WXXX" "TXX" "WXX") :test #'string=)
                                        (multiple-value-bind (stop next) (id3-split tag (1+ data) data-end encoding)
                                          (format nil "~a: ~a" (id3-text tag (1+ data) stop encoding)
                                                  (if (char= (char id 0) #\W) (octets-text tag next data-end) (id3-text tag next data-end encoding)))))
                                       ((char= (char id 0) #\T) (substitute #\/ (code-char 0) (id3-text tag (1+ data) data-end encoding)))
                                       ((char= (char id 0) #\W) (octets-text tag data data-end))
                                       ((member id '("COMM" "USLT" "COM" "ULT") :test #'string=)
                                        (multiple-value-bind (stop next) (id3-split tag (+ data 4) data-end encoding)
                                          (declare (ignore stop))
                                          (id3-text tag next data-end encoding)))
                                       ((member id '("APIC" "PIC") :test #'string=) (format nil "~:d octets of a picture kept inside it" length))
                                       ((string= id "PRIV")
                                        (multiple-value-bind (stop) (id3-split tag data data-end 0)
                                          (format nil "~a: ~:d octets" (octets-text tag data stop) (- data-end stop))))
                                       (t (format nil "~:d octets" length))))
                           fields))
                   (setf at data-end)))))
    (let ((v1 (- (length octets) 128)))
      (when (and (>= v1 0) (tuatara.io:octets-at-p octets "TAG" v1))
        (loop for (label start width) in '(("Title (ID3v1)" 3 30) ("Artist (ID3v1)" 33 30) ("Album (ID3v1)" 63 30)
                                           ("Year (ID3v1)" 93 4) ("Comment (ID3v1)" 97 30))
              do (push (cons label (tuatara.text:octets-latin-1 octets :start (+ v1 start) :end (or (position 0 octets :start (+ v1 start) :end (+ v1 start width)) (+ v1 start width))))
                       fields))))
    (list (metadata-section "ID3 tags" (nreverse fields)))))

(defun flac-metadata (octets)
  "The sections of the note for the FLAC sound in OCTETS: its length and its comments."
  (let ((at 4) (fields '()) (count 0))
    (loop while (and (< (+ at 4) (length octets)) (< (incf count) 1000))
          do (let* ((header (aref octets at)) (type (logand header #x7F))
                    (length (octets-integer octets (1+ at) 3 :big-endian t))
                    (data (+ at 4)) (end (+ data length)))
               (when (> end (length octets)) (return))
               (case type
                 (0 (let* ((rate (ldb (byte 20 44) (octets-integer octets (+ data 10) 8 :big-endian t)))
                           (samples (ldb (byte 36 0) (octets-integer octets (+ data 10) 8 :big-endian t))))
                      (when (plusp rate) (push (cons "Length" (format nil "~,1f seconds, ~:d samples a second" (/ samples rate) rate)) fields))))
                 (4 (let* ((vendor (octets-integer octets data 4)) (p (+ data 4 (or vendor 0))))
                      (when vendor (push (cons "Made by" (octets-text octets (+ data 4) (min end p))) fields))
                      (let ((items (octets-integer octets p 4)))
                        (incf p 4)
                        (dotimes (i (min (or items 0) *metadata-most-fields*))
                          (let ((size (octets-integer octets p 4)))
                            (unless (and size (<= (+ p 4 size) end)) (return))
                            (let* ((text (octets-text octets (+ p 4) (+ p 4 size))) (equals (position #\= text)))
                              (push (if equals (cons (subseq text 0 equals) (subseq text (1+ equals))) (cons "Comment" text)) fields))
                            (incf p (+ 4 size)))))))
                 (6 (push (cons "Picture" (format nil "~:d octets of a picture kept inside it" length)) fields)))
               (when (logbitp 7 header) (return))
               (setf at end)))
    (list (metadata-section "FLAC comments" (nreverse fields)))))

;;; Office and OpenDocument files: their text view's own account

(defun office-metadata (path kind)
  "The section of the note giving what the Office or OpenDocument file at PATH,
of KIND, says of itself: the first section of the note its reader makes."
  (let* ((office (office-preview-format kind (tuatara.io:native-file-name path)))
         (read (and office (getf office :read))))
    (when read
      (let* ((note (handler-case (funcall read path) (error (e) (format nil "* About this file~%It could not be read: ~a~%" e))))
             (start (search (format nil "~%* About this") note))
             (start (if start (1+ start) (and (uiop:string-prefix-p "* About this" note) 0)))
             (end (and start (search (format nil "~%* ") note :start2 (1+ start)))))
        (and start (list (subseq note start (if end (1+ end) (length note)))))))))

;;; The note

(defun mode-words (mode)
  "The permissions of MODE as ls shows them: -rw-r--r--, with the octal number after."
  (format nil "~a (~4,'0o)" (first (mode-field mode)) (logand mode #o7777)))

(defun file-facts-section (path stamp kind words sum)
  "The section of the note giving what the system knows of the file at PATH:
its STAMP, KIND and WORDS (by its contents) and its SHA-256 SUM."
  (let* ((name (tuatara.io:native-file-name path))
         (link (ignore-errors (sb-posix:readlink (tuatara.io:native path))))
         (times (tuatara.io:path-times path))
         (owner (tuatara.io:owner-name (tuatara.io:stamp-uid stamp)))
         (group (tuatara.io:owner-group-name (tuatara.io:stamp-gid stamp))))
    (flet ((time-of (key) (let ((n (getf times key))) (and n (time-words (mtime-universal-time n))))))
      (metadata-section
       "The file"
       (list (cons "Path" (tuatara.io:native path))
             (cons "Link to" link)
             (cons "Kind" (format nil "~a~:[~; (its name says otherwise: ~a)~]" (or words "text, or a kind not told by its contents")
                                  (and kind (tuatara.io:kind-name-disagrees-p kind name)) name))
             (cons "MIME type" (and kind (file-mime-type kind name)))
             (cons "Size" (format nil "~:d octets (~a)" (tuatara.io:stamp-size stamp) (octets-words (tuatara.io:stamp-size stamp))))
             (cons "Permissions" (mode-words (tuatara.io:stamp-mode stamp)))
             (cons "Owner" (format nil "~a (~d), group ~a (~d)" (or owner "unknown") (tuatara.io:stamp-uid stamp)
                                   (or group "unknown") (tuatara.io:stamp-gid stamp)))
             (cons "Modified" (time-of :modified))
             (cons "Changed" (let ((w (time-of :changed))) (and w (format nil "~a (its contents, name, owner or permissions)" w))))
             (cons "Accessed" (let ((w (time-of :accessed))) (and w (format nil "~a (as it was before this look, which leaves it alone)" w))))
             (cons "Born" (or (time-of :born) "not kept by its file system"))
             (cons "Inode" (format nil "~d on device ~d, ~d name~:p" (tuatara.io:stamp-ino stamp) (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-nlink stamp)))
             (cons "SHA-256" (or sum "not summed")))))))

(defun xattr-section (fd)
  "The section of the note giving the extended attributes of the file open as
FD (where a browser keeps the address it was downloaded from, the SELinux
label, ACLs)."
  (metadata-section
   "Extended attributes"
   (loop for name in (tuatara.io:fd-xattr-names fd)
         for value = (tuatara.io:fd-xattr-value fd name)
         collect (cons name (cond ((null value) "(cannot be read)")
                                  ((and (every (lambda (o) (or (>= o 32) (member o '(0 9 10 13)))) value)
                                        (ignore-errors (sb-ext:octets-to-string value :external-format :utf-8)))
                                   (string-right-trim (string (code-char 0)) (sb-ext:octets-to-string value :external-format :utf-8)))
                                  (t (format nil "~a (~:d octets)" (tuatara.text:octets-hex value :end (min 64 (length value)) :upper-case t) (length value))))))))

;;; OpenPGP: who an encrypted file is for, read from its packets

(defvar *metadata-look-up-key-owners* :ask
  "Whether M-x file-metadata looks up, in your own GnuPG keyring, whose the
keys an encrypted file is for are, running gpg --list-keys (the file itself is
never given to gpg): true looks them up, :ask asks each time, false (nil)
never does, the keys then shown by their IDs alone.")

(defvar *metadata-looking-up-keys* nil
  "True while the note being made looks up the owners of the keys an
encrypted file is for (*METADATA-LOOK-UP-KEY-OWNERS*).")

(defparameter *openpgp-key-algorithms*
  '((1 . "RSA") (2 . "RSA") (3 . "RSA") (16 . "ElGamal") (17 . "DSA") (18 . "ECDH") (19 . "ECDSA") (22 . "EdDSA")
    (25 . "X25519") (26 . "X448") (27 . "Ed25519") (28 . "Ed448") (35 . "ML-KEM-768 with X25519") (36 . "ML-KEM-1024 with X448"))
  "The names of OpenPGP's public-key algorithms, by number.")

(defparameter *openpgp-ciphers*
  '((1 . "IDEA") (2 . "TripleDES") (3 . "CAST5") (4 . "Blowfish") (7 . "AES-128") (8 . "AES-192") (9 . "AES-256")
    (10 . "Twofish") (11 . "Camellia-128") (12 . "Camellia-192") (13 . "Camellia-256"))
  "The names of OpenPGP's ciphers, by number.")

(defparameter *openpgp-hashes*
  '((1 . "MD5") (2 . "SHA-1") (3 . "RIPEMD-160") (8 . "SHA-256") (9 . "SHA-384") (10 . "SHA-512") (11 . "SHA-224")
    (12 . "SHA3-256") (14 . "SHA3-512"))
  "The names of OpenPGP's hashes, by number.")

(defparameter *openpgp-aead-modes* '((1 . "EAX") (2 . "OCB") (3 . "GCM"))
  "The names of OpenPGP's modes of authenticated encryption, by number.")

(defun openpgp-name (table number)
  "NUMBER's name in TABLE, an alist, or words saying it is unknown."
  (or (cdr (assoc number table)) (format nil "unknown (~a)" number)))

(defun openpgp-armour-octets (octets)
  "The packets armoured in OCTETS (-----BEGIN PGP MESSAGE-----), decoded from
its first 256 KB, and its headers (Version, Comment) as (NAME . VALUE); NIL
when OCTETS are not armoured."
  (when (tuatara.io:octets-at-p octets "-----BEGIN PGP MESSAGE-----")
    (let* ((first-end (or (position 10 octets) (length octets)))
           (headers '())
           (at (1+ first-end)))
      ;; the headers run to the first empty line
      (loop for line-end = (or (position 10 octets :start (min at (length octets))) (length octets))
            for line = (string-right-trim '(#\Return) (tuatara.text:octets-latin-1 octets :start (min at line-end) :end line-end))
            while (and (< at (length octets)) (plusp (length line)) (< (length headers) 50))
            do (let ((colon (search ": " line)))
                 (if colon
                     (push (cons (subseq line 0 colon) (octets-text octets (+ at colon 2) line-end)) headers)
                     (return)))
               (setf at (1+ line-end)))
      (setf at (min at (length octets)))
      (let ((end (min (length octets) (+ at (* 256 1024)) (or (search (tuatara.text:string-octets "-----END") octets :start2 at) (length octets)))))
        (values (tuatara.io:base64-octets octets :start (min at end) :end end) (nreverse headers))))))

(defun openpgp-octets-hex (octets start end)
  "OCTETS from START below END in hexadecimal capitals; NIL when they are cut short."
  (and (<= end (length octets)) (tuatara.text:octets-hex octets :start start :end end :upper-case t)))

(defun openpgp-recipient (octets body)
  "The key the session key packet in OCTETS at BODY is for, as a plist: :ID
\(its 16 hexadecimal digits, NIL when the sender left it out),
:FINGERPRINT (a version 6 packet's) and :ALGORITHM; NIL for a packet not read."
  (case (aref octets body)
    (3 (let ((id (openpgp-octets-hex octets (+ body 1) (+ body 9)))
             (algorithm (tuatara.io:octet-at octets (+ body 9))))
         (and id algorithm
              (list :id (and (find-if (lambda (c) (char/= c #\0)) id) id) :algorithm (openpgp-name *openpgp-key-algorithms* algorithm)))))
    (6 (let ((n (tuatara.io:octet-at octets (+ body 1))))
         (cond ((null n) nil)
               ((zerop n) (let ((algorithm (tuatara.io:octet-at octets (+ body 2))))
                            (and algorithm (list :id nil :algorithm (openpgp-name *openpgp-key-algorithms* algorithm)))))
               (t (let ((version (tuatara.io:octet-at octets (+ body 2)))
                        (fingerprint (openpgp-octets-hex octets (+ body 3) (+ body 2 n)))
                        (algorithm (tuatara.io:octet-at octets (+ body 2 n))))
                    (and version fingerprint algorithm (>= (length fingerprint) 16)
                         (list :id (if (= version 4) (subseq fingerprint (- (length fingerprint) 16)) (subseq fingerprint 0 16))
                               :fingerprint fingerprint :algorithm (openpgp-name *openpgp-key-algorithms* algorithm))))))))))

(defun openpgp-s2k-words (octets at)
  "How the passphrase is made a key by the S2K in OCTETS at AT, as words."
  (let ((type (tuatara.io:octet-at octets at))
        (hash (let ((h (tuatara.io:octet-at octets (1+ at)))) (and h (openpgp-name *openpgp-hashes* h)))))
    (case type
      (0 (format nil "~a of the passphrase alone, no salt: weak" hash))
      (1 (format nil "~a, salted, once: weak" hash))
      (3 (let ((c (tuatara.io:octet-at octets (+ at 10))))
           (format nil "~a, salted and repeated~@[, ~:d octets hashed~]" hash
                   (and c (ash (+ 16 (logand c 15)) (+ (ash c -4) 6))))))
      (4 (let ((passes (tuatara.io:octet-at octets (+ at 17))) (lanes (tuatara.io:octet-at octets (+ at 18))) (memory (tuatara.io:octet-at octets (+ at 19))))
           (if (and passes lanes memory (< memory 64))
               (format nil "Argon2, ~d pass~:*~[es~;~:;es~], ~d lane~:p, ~d ~a of memory" passes lanes
                       (ash 1 (mod memory 10)) (case (floor memory 10) (0 "KiB") (1 "MiB") (2 "GiB") (t "TiB")))
               "Argon2")))
      (101 "GnuPG's own (the key kept elsewhere, on a card)")
      ((nil) "not read: cut short")
      (t (format nil "unknown (~d)" type)))))

(defun openpgp-passphrase-words (octets body)
  "What the passphrase packet in OCTETS at BODY says of how it is made a key, as words."
  (let ((version (aref octets body)))
    (flet ((words (cipher at) (format nil "~a, the passphrase made a key by ~a"
                                     (if cipher (openpgp-name *openpgp-ciphers* cipher) "a cipher not read")
                                     (openpgp-s2k-words octets at))))
      (case version
        (4 (words (tuatara.io:octet-at octets (+ body 1)) (+ body 2)))
        (5 (words (tuatara.io:octet-at octets (+ body 1)) (+ body 3)))
        (6 (words (tuatara.io:octet-at octets (+ body 2)) (+ body 5)))
        (t (format nil "a passphrase packet of version ~d, not read" version))))))

(defun openpgp-protection-words (tag octets body)
  "How the data of the packet of TAG in OCTETS at BODY is kept from being changed, as words."
  (flet ((aead-words ()
           ;; the cipher and the mode, after the version
           (let ((c (tuatara.io:octet-at octets (+ body 1))) (m (tuatara.io:octet-at octets (+ body 2))))
             (format nil "~a, ~a" (if c (openpgp-name *openpgp-ciphers* c) "?") (if m (openpgp-name *openpgp-aead-modes* m) "?")))))
    (case tag
      (9 "none: an old form without a modification check; what it holds could have been changed unnoticed")
      (18 (case (tuatara.io:octet-at octets body)
            (1 "a modification check (SHA-1), as gpg writes it")
            (2 (format nil "authenticated encryption: ~a" (aead-words)))
            (t "an encrypted data packet of a version not known")))
      (20 (format nil "authenticated encryption, GnuPG's own packet: ~a" (aead-words))))))

(defun openpgp-packets (octets)
  "What the packets before an OpenPGP message's encrypted data say, from
OCTETS (armoured or not), as a plist: :RECIPIENTS (OPENPGP-RECIPIENT's),
:PASSPHRASES (words for each passphrase that opens it), :PROTECTION (words, NIL
when the data was not reached), :HEADERS (the armour's) and :CUT (true when
the packets end before the data)."
  (multiple-value-bind (decoded headers) (openpgp-armour-octets octets)
    (let ((octets (or decoded octets)) (recipients '()) (passphrases '()) (protection nil) (at 0))
      (loop repeat 1000
            do (multiple-value-bind (tag body length) (tuatara.io:openpgp-packet-header octets at)
                 (when (or (null tag) (>= body (length octets))) (return))
                 (case tag
                   (1 (let ((r (openpgp-recipient octets body))) (when r (push r recipients))))
                   (3 (push (openpgp-passphrase-words octets body) passphrases))
                   ((9 18 20) (setf protection (openpgp-protection-words tag octets body)) (return))
                   (10)
                   (t (return)))
                 (if length (setf at (+ body length)) (return))))
      (list :recipients (nreverse recipients) :passphrases (nreverse passphrases) :protection protection
            :headers headers :cut (null protection)))))

(defun openpgp-recipient-ids (recipients)
  "The IDs of the keys of RECIPIENTS (OPENPGP-RECIPIENT's), those their sender left out not among them."
  (remove nil (mapcar (lambda (r) (getf r :id)) recipients)))

(defun openpgp-key-ids (octets)
  "The IDs of the keys the OpenPGP message in OCTETS is for, those its sender left out not among them."
  (openpgp-recipient-ids (getf (openpgp-packets octets) :recipients)))

(defun openpgp-metadata (octets)
  "The section of the note saying who the OpenPGP message in OCTETS is for:
the keys it is encrypted to (their owners as your keyring has them, when
*METADATA-LOOKING-UP-KEYS*), whether a passphrase opens it and how that is
made a key, and how its data is kept from being changed; read from its
packets, nothing decrypted."
  (let* ((packets (openpgp-packets octets))
         (recipients (getf packets :recipients))
         (ids (openpgp-recipient-ids recipients))
         (owners (cond ((null ids) nil)
                       ((not *metadata-looking-up-keys*) :not-asked)
                       ((null (gpg-path)) :no-gpg)
                       (t (key-owners ids)))))
    (list (metadata-section
           "Encryption (OpenPGP)"
           (append
            (list (cons "Encrypted to" (format nil "~[no key~;1 key~:;~:*~d keys~]" (length recipients)))
                  (cons "Passphrase" (if (getf packets :passphrases)
                                         (format nil "yes~[~;~:;~:*, any of ~d~]" (length (getf packets :passphrases)))
                                         "no")))
            (loop for r in recipients
                  for k from 1
                  collect (cons (format nil "Key ~d" k)
                                (format nil "~:[hidden: the sender left its ID out~;~:*~a~]~@[ (fingerprint ~a)~], ~a~@[: ~a~]"
                                        (getf r :id) (getf r :fingerprint) (getf r :algorithm)
                                        (and (getf r :id)
                                             (case owners
                                               (:not-asked "owner not looked up (*metadata-look-up-key-owners*)")
                                               (:no-gpg "owner not looked up: gpg is not installed")
                                               (t (or (cdr (assoc (getf r :id) owners :test #'string-equal)) "not in your keyring")))))))
            (loop for words in (getf packets :passphrases)
                  for k from 1
                  collect (cons (format nil "Passphrase ~d" k) words))
            (list (cons "Modification check" (or (getf packets :protection) "not reached: the file is cut short or not read")))
            (loop for (name . value) in (getf packets :headers)
                  collect (cons (format nil "Armour header ~a" name) value)))))))

(defmacro metadata-part (&body body)
  "The sections BODY gives, or, when it fails (a file made to break what reads
it, say), one saying what could not be read, so that the rest is still shown."
  `(handler-case (progn ,@body)
     (error (e) (list (format nil "* Not read~%Some of what it says of itself could not be read: ~a~%~%"
                              (metadata-value (princ-to-string e)))))))

(defun built-in-metadata (path kind fd size)
  "The sections of the note giving what the file at PATH, of KIND, open as FD,
of SIZE octets, says of itself, by the readers of this file: its pictures',
PDF's, sound's and video's own fields, and XMP wherever it is kept."
  (declare (ignore path))
  (multiple-value-bind (octets cut) (file-octets-for-metadata fd size)
    (append (metadata-part
             (case kind
               (:jpeg (jpeg-metadata octets))
               ;; a camera's raw of TIFF's shape keeps its tags as a TIFF does
               ((:tiff :raw) (and (tuatara.io:tiff-octets-p octets)
                                  (list (exif-section octets 0 "its own tags"))))
               (:png (png-metadata octets))
               (:webp (webp-metadata octets))
               ((:heif :avif) (heif-metadata octets))
               (:pdf (pdf-metadata octets))
               (:mp3 (id3-metadata octets))
               (:flac (flac-metadata octets))
               (:openpgp (openpgp-metadata octets))
               (:mp4 (unless cut (mp4-metadata octets)))))
            (when (and (eq kind :mp4) cut)
              (list (format nil "* What the video or sound says of itself~%Not read: it is larger than ~a (*metadata-largest-read*), and its parts can be anywhere in it.~%~%"
                            (octets-words *metadata-largest-read*))))
            ;; an MP3's ID3 tag may come before what the contents are told by
            (when (and (not (eq kind :mp3)) (tuatara.io:octets-at-p octets "ID3")) (metadata-part (id3-metadata octets)))
            (metadata-part (xmp-sections octets)))))

(defun office-metadata-function (path kind fd size)
  "What the Office or OpenDocument file at PATH, of KIND, says of itself (OFFICE-METADATA); FD and SIZE unused."
  (declare (ignore fd size))
  (office-metadata path kind))

(defun file-metadata-note (path &key (cancel (list nil)) (done (list 0)) (look-up-keys (eq *metadata-look-up-key-owners* t)))
  "The Org note of what the file at PATH is and says of itself, as
\\[file-metadata] shows it; its SHA-256 summed with CANCEL and DONE as
FILE-SHA256 takes them; with LOOK-UP-KEYS, the owners of the keys an
encrypted file is for looked up in your keyring. Signals an error when it
cannot be read."
  (let ((*metadata-looking-up-keys* look-up-keys))
    (tuatara.io:call-with-file
     path
     (lambda (fd native size)
       (let* ((stamp (tuatara.io:fd-stamp fd native))
              (head (tuatara.io:read-fd-start fd native size 4096)))
         (multiple-value-bind (words kind) (tuatara.io:octets-kind head)
           (let ((sections (loop for function in *file-metadata-functions*
                                 append (metadata-part (funcall function native kind fd size)))))
             (concatenate 'string
                          (format nil "#+title: Metadata of ~a~%~%" (org-safe-line (tuatara.io:native-file-name native)))
                          (file-facts-section native stamp kind words (file-sha256 native :cancel cancel :done done))
                          (xattr-section fd)
                          (format nil "~{~a~}" (remove "" sections :test #'string=))))))))))

;;; The command

(defstruct (metadata-job (:include tuatara.runtime:job) (:constructor make-metadata-job (buffer path size)) (:copier nil))
  "The metadata of the file at PATH, of SIZE octets, being read for BUFFER, a
job; DONE, the octets summed so far; TEXT, the note made."
  path size (done (list 0)) text)

(setf (documentation 'metadata-job-p 'function) "True when OBJECT is a METADATA-JOB.")

(tuatara.runtime:define-job-words metadata-job
  :doing "Reading the file's metadata"
  :description (format nil "Reading the metadata of ~a" (tuatara.io:native-file-name (metadata-job-path job)))
  :verb "read the metadata"
  :left-words "nothing read")

(defmethod tuatara.runtime:job-percent ((job metadata-job))
  "How far summing the file has got."
  (tuatara.runtime:running-percent (car (metadata-job-done job)) (max 1 (metadata-job-size job))))

(defmethod tuatara.runtime:job-take-in ((job metadata-job))
  "Show the metadata read, or say why it could not be."
  (show-reading-result job (metadata-job-path job) (metadata-job-text job) "The metadata of ~a could not be read: ~a~%"))

(defun look-up-key-owners-p (path)
  "True when M-x file-metadata is to look up in your keyring whose the keys the
file at PATH is encrypted to are (*METADATA-LOOK-UP-KEY-OWNERS*): asked, when
it says :ask, only of a file encrypted to keys it names."
  (case *metadata-look-up-key-owners*
    ((nil) nil)
    (:ask (let ((head (tuatara.io:file-head path)))
            (and head
                 (eq (nth-value 1 (tuatara.io:octets-kind head)) :openpgp)
                 (openpgp-key-ids head)
                 (y-or-n-p* "It is encrypted to keys: look up whose in your keyring (runs gpg --list-keys)?"))))
    (t t)))

(define-command file-metadata ()
  "Show what the file shown is and what it says of itself, in a buffer of its
own, which \\`q' closes: the file the buffer visits, the entry on the line in a list of a
directory, the PDF or picture in its view; with \\[universal-argument], or
where there is none, asked for. What the system knows of it (its kind by its
contents, size, permissions, owner, when it was modified, changed, accessed
and born, its SHA-256 sum, its extended attributes) and what it keeps of
itself: a photograph's EXIF (camera, times, settings, GPS position, its
thumbnail), XMP and its edit history, a PNG's text, a PDF's document
information and saves, an MP3's tags, a video's times and place, an Office
document's account of itself, an encrypted file's keys and passphrases (whose
the keys are looked up in your keyring as *metadata-look-up-key-owners* says).
Read by Tuatara alone in the background, nothing run and its access time left
alone; gpg, when it looks up keys, is given your keyring alone, not the file."
  (let* ((path (shown-file-argument "Metadata of file: "))
         (stamp (tuatara.io:path-stamp path)))
    (cond ((null stamp) (message "~a is not there" (abbreviated-path path)))
          ((not (eq (tuatara.io:stamp-kind stamp) :regular)) (message "~a is not a regular file: no metadata read" (abbreviated-path path)))
          (t (let ((look-up-keys (look-up-key-owners-p path))
                   (buffer (buffer-in-mode (format nil "*Metadata: ~a*" (tuatara.io:native-file-name path)) 'org-mode)))
               (setf (tuatara.buffer:buffer-local buffer :read-only) t
                     (tuatara.buffer:buffer-local buffer :directory) (tuatara.io:parent-path path))
               (view-made-text buffer)
               (set-made-text buffer (format nil "Reading the metadata of ~a...~%" (abbreviated-path path)))
               (show-buffer buffer)
               (let ((job (make-metadata-job buffer path (tuatara.io:stamp-size stamp))))
                 (tuatara.runtime:start-job-thread
                  job (lambda (job) (setf (metadata-job-text job)
                                          (file-metadata-note (metadata-job-path job) :cancel (tuatara.runtime:job-cancel job)
                                                                                      :done (metadata-job-done job)
                                                                                      :look-up-keys look-up-keys)))
                  :name "tuatara metadata")))))))

(defun install-metadata ()
  "Have M-x file-metadata read pictures, PDFs, sound and video, and Office and
OpenDocument files, and bind \\`g i' to it in lists of directories."
  (define-key (symbol-value '*rimu-keymap*) "g i" 'file-metadata)
  (pushnew 'office-metadata-function *file-metadata-functions*)
  (pushnew 'built-in-metadata *file-metadata-functions*))
