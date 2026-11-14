;;;; test/metadata.lisp --- M-x show-file-metadata: what a file is and says of itself, read by Tuatara alone.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test metadata
  :description "M-x show-file-metadata shows what the system knows of a file (kind by contents, size,
permissions, owner, its four times, SHA-256, extended attributes) and what it says of itself, read
by Tuatara alone. These are a photograph's EXIF and GPS position and thumbnail in JPEG, PNG, WebP,
TIFF and HEIF; PNG text; XMP and its edit history; a PDF's information, saves and what could act in
it, from an object stream too; MP3 and FLAC tags; an MP4's times and place; an Office file's
account of itself. Files made to mislead or loop are read as far as they make sense, never hanging;
the file's access time is left alone.")

;;; Test files, built here

(defun octets-of (&rest parts)
  "PARTS joined as octets: strings as Latin-1, integers as one octet each, vectors as they are."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (part parts (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase part
        (string (loop for c across part do (vector-push-extend (char-code c) out)))
        (integer (vector-push-extend part out))
        (vector (loop for o across part do (vector-push-extend o out)))))))

(defun le-octets (n count)
  "N as COUNT octets, least significant first."
  (tuatara.io:octets-put-integer (make-array count :element-type '(unsigned-byte 8)) n count :at 0))
(defun be-octets (n count)
  "N as COUNT octets, most significant first."
  (tuatara.io:octets-put-integer (make-array count :element-type '(unsigned-byte 8)) n count :at 0 :big-endian t))

(defun tiff-test-octets (image exif gps thumbnail)
  "A little-endian TIFF structure (EXIF) of the IMAGE, EXIF, GPS and THUMBNAIL
parts, each a list of (TAG TYPE VALUE): VALUE a string (ASCII), a list of
integers (SHORT, LONG) or of fractions (N . D) (RATIONAL), or octets."
  (let* ((image (append image (and exif '((#x8769 4 (:exif)))) (and gps '((#x8825 4 (:gps))))))
         (parts (remove nil
                        (list (cons :image image)
                              (and exif (cons :exif exif))
                              (and gps (cons :gps gps))
                              (and thumbnail (cons :thumbnail thumbnail)))))
         (places (let ((at 8))
                   (loop for (name . entries) in parts collect (cons name at) do (incf at
                                                                                       (+ 2 (* 12
                                                                                               (length entries)) 4)))))
         (data-at (+ 8 (loop for (nil . entries) in parts sum (+ 2 (* 12 (length entries)) 4))))
         (head (octets-of "II" 42 0 (le-octets 8 4)))
         (ifds (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((value-octets (type value)
             (case type
               (2 (octets-of value 0))
               (3 (apply #'octets-of (mapcar (lambda (v) (le-octets v 2)) value)))
               (4
                (apply #'octets-of
                       (mapcar (lambda (v) (le-octets (if (keywordp v) (cdr (assoc v places)) v) 4)) value)))
               (5
                (apply #'octets-of (mapcar (lambda (r) (octets-of (le-octets (car r) 4) (le-octets (cdr r) 4))) value)))
               (t (coerce value '(vector (unsigned-byte 8)))))))
      (loop for (name . entries) in parts
            do (loop for o across (le-octets (length entries) 2) do (vector-push-extend o ifds))
               (dolist (entry entries)
                 (destructuring-bind (tag type value) entry
                   (let* ((octets (value-octets type value))
                          (number (case type
                                    (2 (length octets))
                                    (3 (/ (length octets) 2))
                                    (4 (/ (length octets) 4))
                                    (5 (/ (length octets) 8))
                                    (t (length octets)))))
                     (loop for o across (octets-of (le-octets tag 2)
                                                   (le-octets type 2)
                                                   (le-octets number 4)) do (vector-push-extend o ifds))
                     (if (<= (length octets) 4)
                         (loop for i below 4 do (vector-push-extend (if (< i (length octets)) (aref octets i) 0) ifds))
                         (progn
                           (loop for o across (le-octets (+ data-at (length data)) 4) do (vector-push-extend o ifds))
                           (loop for o across octets do (vector-push-extend o data)))))))
               ;; the picture's own part leads to its thumbnail's
               (loop for o across (le-octets (if (and (eq name :image)
                                                      (assoc :thumbnail places)) (cdr (assoc :thumbnail places)) 0) 4)
                     do (vector-push-extend o ifds))))
    (octets-of head ifds data)))

(defparameter *exif-test-tiff*
  (tiff-test-octets '((#x010F 2 "Kea Cameras")
                      (#x0110 2 "K1")
                      (#x0131 2 "Darkroom 4.2")
                      (#x0132 2 "2026:03:02 10:00:00")
                      (#x9C9C 1 #(72 0 105 0 0 0)))
                    '((#x9003 2 "2026:03:01 09:30:00") (#x829A 5 ((1 . 125))) (#x829D 5 ((28 . 10))) (#x8827 3 (200))
                      (#x9286 7 #(65 83 67 73 73 0 0 0 79 110 32 116 104 101 32 114 105 100 103 101))
                      (#x927C 7 #(1 2 3 4 5 6 7 8 9)))
                    '((1 2 "S")
                      (2 5 ((41 . 1) (17 . 1) (12 . 1)))
                      (3 2 "E")
                      (4 5 ((174 . 1) (46 . 1) (30 . 1)))
                      (5 1 #(0))
                      (6 5 ((123 . 10))))
                    '((#x0201 4 (0)) (#x0202 4 (4321))))
  "The EXIF of a photograph made for the tests: its camera, times, settings, a GPS position in Wellington, a thumbnail.")

(defparameter *xmp-test-packet*
  "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?><x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description rdf:about='' xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmlns:dc='http://purl.org/dc/elements/1.1/' xmlns:xmpMM='http://ns.adobe.com/xap/1.0/mm/' xmlns:stEvt='http://ns.adobe.com/xap/1.0/sType/ResourceEvent#' xmp:CreatorTool='Kea Hamilton 3'><dc:creator><rdf:Seq><rdf:li>Ana Timaru</rdf:li></rdf:Seq></dc:creator><xmpMM:History><rdf:Seq><rdf:li stEvt:action='saved' stEvt:when='2026-03-02T10:00:00' stEvt:softwareAgent='Kea Hamilton 3'/></rdf:Seq></xmpMM:History></rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end='w'?>"
  "An XMP packet made for the tests: the program that made the file, who, and an edit.")

(defun jpeg-test-octets ()
  "A JPEG made for the tests: its EXIF, an XMP packet, a comment."
  (let ((exif (octets-of "Exif" 0 0 *exif-test-tiff*))
        (xmp (octets-of "http://ns.adobe.com/xap/1.0/" 0 *xmp-test-packet*))
        (comment (octets-of "Shot from the ridge")))
    (octets-of #xFF #xD8
               #xFF #xE1 (be-octets (+ 2 (length exif)) 2) exif
               #xFF #xE1 (be-octets (+ 2 (length xmp)) 2) xmp
               #xFF #xFE (be-octets (+ 2 (length comment)) 2) comment
               #xFF #xDA 0 2 #xFF #xD9)))

(defun png-chunk (type data)
  "A PNG chunk of TYPE holding DATA (its check left 0)."
  (octets-of (be-octets (length data) 4) type data 0 0 0 0))

(defun box (type &rest contents)
  "An MP4 box of TYPE holding CONTENTS."
  (let ((data (apply #'octets-of contents)))
    (octets-of (be-octets (+ 8 (length data)) 4) type data)))

(defun write-test-octets-file (path octets)
  "Write OCTETS to PATH; return PATH as a string."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence octets out))
  (namestring path))

(defun metadata-of (path)
  "The note M-x show-file-metadata makes of the file at PATH."
  (tuatara.defaults::install-metadata)
  (tuatara.defaults::file-metadata-note path))

;;; Pictures

(define-test (metadata photograph-read)
  (with-test-directory (dir)
    (let* ((path (write-test-octets-file (merge-pathnames "kea.jpg" dir) (jpeg-test-octets)))
           (note (metadata-of path)))
      (note-has note "#+title: Metadata of kea.jpg")
      (note-has note "- Kind :: JPEG image")
      (note-has note "- MIME type :: image/jpeg")
      (note-has note "- Permissions :: -rw-")
      (note-has note "- SHA-256 :: ")
      (note-has note "* EXIF (in its APP1 segment)")
      (note-has note "- Position :: -41.286667, 174.775000 (12.3 m above sea level)")
      (note-has note "- Thumbnail :: 4,321 octets, a picture of its own kept inside it")
      (note-has note "- Make :: Kea Cameras")
      (note-has note "- Model :: K1")
      (note-has note "- Software :: Darkroom 4.2")
      (note-has note "- DateTime :: 2026:03:02 10:00:00")
      (note-has note "- DateTimeOriginal :: 2026:03:01 09:30:00")
      (note-has note "- ExposureTime :: 1/125")
      (note-has note "- FNumber :: 2.8")
      (note-has note "- ISOSpeedRatings :: 200")
      (note-has note "- UserComment :: On the ridge")
      (note-has note "- XPComment :: Hi")
      (note-has note "- MakerNote :: 9 octets of the maker's own notes (not read)")
      (note-has note "- GPSLatitudeRef :: S")
      (note-has note "- Comment :: Shot from the ridge")
      (note-has note "* XMP")
      (note-has note "- xmp:CreatorTool :: Kea Hamilton 3")
      (note-has note "- dc:creator :: Ana Timaru")
      (note-has note "- xmpMM:History :: stEvt:action saved, stEvt:when 2026-03-02T10:00:00, stEvt:softwareAgent Kea Hamilton 3"))))

(define-test (metadata read-from-octets-alone)
  ;; what a package reads from a picture it holds, with no file to open: the same sections the note gives
  (let ((note (format nil "~{~a~}" (tuatara:octets-metadata (jpeg-test-octets) :jpeg))))
    (note-has note "- Software :: Darkroom 4.2")
    (note-has note "- Comment :: Shot from the ridge")
    (note-has note "- xmp:CreatorTool :: Kea Hamilton 3"))
  (let ((note (format nil "~{~a~}" (tuatara:octets-metadata
                                    (octets-of #x89 "PNG" 13 10 26 10
                                               (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                                               (png-chunk "tEXt" (octets-of "Software" 0 "GIMP 2.10.38"))
                                               (png-chunk "tIME" (octets-of (be-octets 2026 2) 3 2 10 0 0))
                                               (png-chunk "IEND" #()))
                                    :png))))
    (note-has note "- Software :: GIMP 2.10.38")
    (note-has note "- Last changed (tIME) :: 2026-03-02 10:00:00 UTC"))
  (note-has (format nil "~{~a~}" (tuatara:octets-metadata *exif-test-tiff* :tiff)) "- Make :: Kea Cameras")
  ;; the segments of a JPEG before its picture data, each marker with where its data lies
  (let ((markers '()))
    (tuatara:jpeg-segments (jpeg-test-octets) (lambda (marker start end) (push (list marker start end) markers) nil))
    (true (find #xE1 markers :key #'first))
    (true (find #xFE markers :key #'first))))

(define-test (metadata large-texts-read-in-part)
  ;; a text of megabytes is cut before it is made a string; an XMP packet past the most read is named, not
  ;; parsed; a chunk claiming more than the file holds is left
  (let* ((png (octets-of #x89 "PNG" 13 10 26 10
                         (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                         (png-chunk "tEXt" (octets-of "Comment" 0 (make-array (* 4 1024 1024) :initial-element 65)))
                         (png-chunk "IEND" #())))
         (before (sb-ext:get-bytes-consed))
         (note (format nil "~{~a~}" (tuatara:octets-metadata png :png))))
    (true (< (- (sb-ext:get-bytes-consed) before) (* 4 1024 1024)) "~:d octets made"
          (- (sb-ext:get-bytes-consed) before))
    (note-has note "- Comment :: AAAA"))
  (let* ((xmp (octets-of #xFF #xD8 "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">"
                         (make-array (* 5 1024 1024) :initial-element 32) "</x:xmpmeta>"))
         (before (sb-ext:get-bytes-consed))
         (note (format nil "~{~a~}" (tuatara:octets-metadata xmp :jpeg))))
    (true (< (- (sb-ext:get-bytes-consed) before) (* 4 1024 1024)) "~:d octets made"
          (- (sb-ext:get-bytes-consed) before))
    (note-has note "- Not read :: ")
    (note-has note "(*metadata-xmp-most*)"))
  (let ((png (octets-of #x89 "PNG" 13 10 26 10
                        (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                        (be-octets 400000000 4) "tEXt" "Comment" 0 "short")))
    (true (listp (tuatara:octets-metadata png :png)))))

(define-test (metadata many-texts-read-within-bounds)
  ;; thousands of PNG texts of 4 KB each: the first *metadata-most-fields* read, each cut as it is read, the
  ;; rest counted
  (let* ((text (png-chunk "tEXt" (octets-of "Comment" 0 (make-array 4096 :initial-element #xE9))))
         (png (apply #'octets-of #x89 "PNG" 13 10 26 10
                     (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                     (append (loop repeat 3000 collect text) (list (png-chunk "IEND" #())))))
         (before (sb-ext:get-bytes-consed))
         (note (format nil "~{~a~}" (tuatara:octets-metadata png :png)))
         (made (- (sb-ext:get-bytes-consed) before)))
    (true (< made (* 64 1024 1024)) "~:d octets made" made)
    (true (< (length note) (* 1200 1024)) "a note of ~:d characters" (length note))
    (note-has note "- Not read :: 1,000 texts more")))

(define-test (metadata labels-end-at-their-colons)
  ;; a PNG text whose keyword holds :: cannot make its line read as another field
  (note-has (format nil "~{~a~}" (tuatara:octets-metadata
                                  (octets-of #x89 "PNG" 13 10 26 10
                                             (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                                             (png-chunk "tEXt" (octets-of "DateTime :: 2020:01:01 00:00:00" 0 "x"))
                                             (png-chunk "IEND" #()))
                                  :png))
            "- DateTime : 2020:01:01 00:00:00 :: x"))

(define-test (metadata other-pictures)
  (with-test-directory (dir)
    ;; PNG: its EXIF, its text (plain, packed, international) and when it says it was changed
    (let* ((png (write-test-octets-file
                 (merge-pathnames "kea.png" dir)
                 (octets-of #x89 "PNG" 13 10 26 10
                            (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                            (png-chunk "tEXt" (octets-of "Author" 0 "Ana Timaru"))
                            (png-chunk "iTXt" (octets-of "Description" 0 0 0 "en" 0 0 "Kea on a rock"))
                            (png-chunk "tIME" (octets-of (be-octets 2026 2) 3 2 10 0 0))
                            (png-chunk "eXIf" *exif-test-tiff*)
                            (png-chunk "IEND" #()))))
           (note (metadata-of png)))
      (note-has note "* EXIF (in its eXIf chunk)")
      (note-has note "- Make :: Kea Cameras")
      (note-has note "- Author :: Ana Timaru")
      (note-has note "- Description :: Kea on a rock")
      (note-has note "- Last changed (tIME) :: 2026-03-02 10:00:00 UTC"))
    ;; a time no calendar has is said to be none, its numbers as written
    (let ((png (write-test-octets-file
                (merge-pathnames "bad-time.png" dir)
                (octets-of #x89 "PNG" 13 10 26 10
                           (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                           (png-chunk "tIME" (octets-of (be-octets 2026 2) 13 40 25 0 0))
                           (png-chunk "IEND" #())))))
      (note-has (metadata-of png) "- Last changed (tIME) :: no such time: year 2026, month 13, day 40, 25:0:0"))
    ;; WebP and TIFF
    (let ((webp (write-test-octets-file (merge-pathnames "kea.webp" dir)
                                        (let ((exif (octets-of "Exif" 0 0 *exif-test-tiff*)))
                                          (octets-of "RIFF"
                                                     (le-octets (+ 4 8 (length exif) (mod (length exif) 2)) 4)
                                                     "WEBP"
                                                     "EXIF" (le-octets (length exif) 4) exif (if (oddp (length exif))
                                                                                                 (octets-of 0)
                                                                                               #())))))
          (tiff (write-test-octets-file (merge-pathnames "kea.tif" dir) *exif-test-tiff*)))
      (note-has (metadata-of webp) "- Model :: K1")
      (note-has (metadata-of tiff) "* EXIF (its own tags)")
      (note-has (metadata-of tiff) "- Position :: -41.286667, 174.775000"))
    ;; HEIF, as a phone keeps its photographs: the EXIF in an item the boxes point to
    (let* ((exif-data (octets-of (be-octets 0 4) *exif-test-tiff*))
           (ftyp (box "ftyp" "heic" (be-octets 0 4) "mif1heic"))
           (infe (box "infe" 2 0 0 0 (be-octets 2 2) (be-octets 0 2) "Exif"))
           (iinf (box "iinf" 0 0 0 0 (be-octets 1 2) infe))
           (hdlr (box "hdlr" 0 0 0 0 (be-octets 0 4) "pict" (be-octets 0 12) 0))
           (iloc-size (+ 8 4 2 2 (+ 2 2 2 4 4)))
           (meta-size (+ 8 4 (length hdlr) (length iinf) iloc-size))
           (data-at (+ (length ftyp) meta-size))
           (iloc (box "iloc" 0 0 0 0 #x44 #x00 (be-octets 1 2)
                      (be-octets 2 2)
                      (be-octets 0 2)
                      (be-octets 1 2)
                      (be-octets data-at 4)
                      (be-octets (length exif-data) 4)))
           (meta (box "meta" 0 0 0 0 hdlr iinf iloc))
           (heic (write-test-octets-file (merge-pathnames "kea.heic" dir) (octets-of ftyp meta exif-data))))
      (is = meta-size (length meta))
      (let ((note (metadata-of heic)))
        (note-has note "* EXIF (in its Exif item)")
        (note-has note "- Position :: -41.286667, 174.775000")))))

;;; PDF

(define-test (metadata png-packed-text-bounded)
  ;; many small packed texts standing for a great deal each: each inflated to a bound, all of them to another
  (let* ((bomb (tuatara.io::deflate (make-array (* 1024 1024) :element-type '(unsigned-byte 8) :initial-element 0)))
         (octets (apply #'octets-of #x89 "PNG" 13 10 26 10
                        (png-chunk "IHDR" (octets-of (be-octets 1 4) (be-octets 1 4) 8 6 0 0 0))
                        (png-chunk "zTXt" (octets-of "Kept" 0 0 (tuatara.io::deflate (octets-of "Ana Timaru"))))
                        (append (loop repeat 300 collect (png-chunk "zTXt" (octets-of "Bomb" 0 0 bomb)))
                                (list (png-chunk "IEND" (octets-of))))))
         (limits '()))
    (sb-int:encapsulate 'tuatara.defaults::zlib-octets 'limits
                        (lambda (function octets start end &rest keys)
                          (push (getf keys :limit) limits)
                          (apply function octets start end keys)))
    (unwind-protect
         (let ((note (format nil "~{~a~}" (tuatara.defaults::png-metadata octets))))
           (true (search "- Kept :: Ana Timaru" note))
           (true (every (lambda (limit) (and limit (<= limit (* 64 1024)))) limits))
           (true (<= (reduce #'+ limits) (* 16 1024 1024))))
      (sb-int:unencapsulate 'tuatara.defaults::zlib-octets 'limits))))

(define-test (metadata pdf-read)
  (with-test-directory (dir)
    (let* ((pdf (write-test-octets-file
                 (merge-pathnames "report.pdf" dir)
                 (octets-of "%PDF-1.7" 10
                            "1 0 obj << /Type /Catalog /Pages 2 0 R /OpenAction << /S /JavaScript /JS (app.alert\\(1\\)) >> >> endobj" 10
                            "2 0 obj << /Type /Pages /Kids [] /Count 0 >> endobj" 10
                            "3 0 obj << /Title <FEFF004B00650061002000720065007000f60072007400> /Author (Ana \\(field\\) Writer) /Creator (Kea Writer)"
                            " /Producer (Kea PDF 1.0) /CreationDate (D:20260301093000+13'00') /ModDate (D:20260302100000Z) /Custom#20Field (x) >> endobj" 10
                            "trailer << /Root 1 0 R /Info 3 0 R >>" 10 "%%EOF" 10
                            ;; saved again: the information changed, the first kept beside it
                            "3 0 obj << /Title (Kea report, changed) /Author 4 0 R >> endobj" 10
                            "4 0 obj (Bo Westland) endobj" 10
                            "trailer << /Root 1 0 R /Info 3 0 R /Prev 9 >>" 10 "%%EOF" 10)))
           (note (metadata-of pdf)))
      (note-has note "* What the PDF says of itself")
      (note-has note "- PDF version :: 1.7")
      (note-has note "- Saved :: 2 times (changed after it was made")
      (note-has note "- Title :: Kea report, changed")
      (note-has note "- Author :: Bo Westland")
      (note-has note "- Holds :: JavaScript, an action on opening (none run here)")
      (false (search "Ana (field) Writer" note)))
    ;; the first save's information, and one kept in a packed object stream
    (let* ((first (write-test-octets-file
                   (merge-pathnames "first.pdf" dir)
                   (octets-of "%PDF-1.4" 10
                              "3 0 obj << /Title <FEFF004B00650061002000720065007000f60072007400> /Author (Ana \\(field\\) Writer)"
                              " /CreationDate (D:20260301093000+13'00') /Custom#20Field (x) >> endobj" 10
                              "trailer << /Info 3 0 R >>" 10 "%%EOF" 10)))
           (note (metadata-of first)))
      (note-has note "- Title :: Kea repört")
      (note-has note "- Author :: Ana (field) Writer")
      (note-has note "- CreationDate :: 2026-03-01 09:30:00 +13:00")
      (note-has note "- Custom Field :: x"))
    (let* ((packed (coerce (loop for i from 0 below (length "789c335530503057303750b0b151d00fc92cc94955d008484cce4e4d512801f13415f41d4b4b32f28b14341cf3123515f4038af2534a93538b1434bc531315025cdc3415ecec1460006c4a6541aa827e40627a6ab182be737e695e898281829d1d00214b1d42") by 2
                                 collect (parse-integer "789c335530503057303750b0b151d00fc92cc94955d008484cce4e4d512801f13415f41d4b4b32f28b14341cf3123515f4038af2534a93538b1434bc531315025cdc3415ecec1460006c4a6541aa827e40627a6ab182be737e695e898281829d1d00214b1d42"
                                                        :start i :end (+ i 2) :radix 16))
                           '(vector (unsigned-byte 8))))
           (pdf (write-test-octets-file
                 (merge-pathnames "packed.pdf" dir)
                 (octets-of "%PDF-1.5" 10
                            "6 0 obj << /Type /ObjStm /N 2 /First 9 /Filter /FlateDecode /Length " (princ-to-string (length packed)) " >>" 10
                            "stream" 13 10 packed 10 "endstream endobj" 10
                            "8 0 obj << /Type /XRef /Root 1 0 R /Info 5 0 R /Size 9 >> stream" 10 "endstream endobj" 10 "%%EOF" 10)))
           (note (metadata-of pdf)))
      (note-has note "- Title :: Packed title")
      (note-has note "- Producer :: Kea PDF"))))

(define-test (metadata pdf-naming-missing-objects)
  ;; a PDF whose information names a great many objects it does not have, among many object
  ;; streams that are none: read in a moment, every object looked for once, a bounded number of them
  (let* ((octets (octets-of "%PDF-1.7" 10
                            (format nil
                                    "3 0 obj << /Title (Kept) ~{/K~d ~:*~d 0 R ~}>> endobj"
                                    (loop for i from 100 below 1100 collect i)) 10
                            (with-output-to-string (out)
                              (loop repeat 400 do (format out "9 0 obj << /Type /ObjStm /N 1 /First 4 >> endobj~%")))
                            (make-string (* 4 1024 1024) :initial-element #\Space)
                            "trailer << /Info 3 0 R >>" 10 "%%EOF" 10))
         (start (get-internal-real-time))
         (sections (tuatara.defaults::pdf-metadata octets))
         (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (true (search "- Title :: Kept" (first sections)))
    (true (< seconds 10) "read in ~,1f seconds" seconds)))

(define-test (metadata pdf-objects-in-streams)
  ;; an object written only as octets inside a stream is no object: the one the file really writes is read
  (let* ((planted "3 0 obj << /Title (Planted) >> endobj")
         (octets (octets-of "%PDF-1.7" 10 "3 0 obj << /Title (Real) >> endobj" 10
                            (format nil "4 0 obj << /Length ~d >> stream~%~a~%endstream endobj~%"
                                    (length planted) planted)
                            "trailer << /Info 3 0 R >>" 10 "%%EOF" 10))
         (note (first (tuatara.defaults::pdf-metadata octets))))
    (true (search "- Title :: Real" note))
    (false (search "Planted" note))))

(define-test (metadata pdf-eof-in-stream)
  ;; a %%EOF among a stream's octets ends no save: the PDF was saved once
  (let* ((data (format nil "%%EOF~%1 0 obj~%"))
         (octets (octets-of "%PDF-1.7" 10
                            (format nil "4 0 obj << /Length ~d >> stream~%~aendstream endobj~%" (length data) data)
                            "trailer << >>" 10 "%%EOF" 10))
         (note (first (tuatara.defaults::pdf-metadata octets))))
    (is = 1 (length (tuatara:pdf-eof-lines octets)))
    (is = 2 (length (tuatara:pdf-eof-lines octets #())))
    (true (search "- Saved :: 1 time" note))
    (false (search "2 times" note))))

(defun pdf-with-table (objects &key (trailer "/Root 1 0 R /Info 6 0 R") (before "") (after ""))
  "A PDF of OBJECTS, each (NUMBER . BODY), with BEFORE (text) after its
header, a cross-reference table, TRAILER's entries, startxref and %%EOF, and
AFTER (text) after that."
  (let ((out (make-string-output-stream)) (at 0) (offsets '()))
    (flet ((put (text) (write-string text out) (incf at (length text))))
      (put (format nil "%PDF-1.7~%~a" before))
      (loop for (number . body) in objects
            do (push (cons number at) offsets)
               (put (format nil "~d 0 obj~%~a~%endobj~%" number body)))
      (let ((xref at) (size (1+ (reduce #'max objects :key #'car))))
        (put (format nil "xref~%0 ~d~%0000000000 65535 f ~%" size))
        (loop for k from 1 below size
              do (put (let ((offset (cdr (assoc k offsets))))
                        (if offset (format nil "~10,'0d 00000 n ~%" offset) (format nil "0000000000 65535 f ~%")))))
        (put (format nil "trailer << /Size ~d ~a >>~%startxref~%~d~%%%EOF~%~a" size trailer xref after))))
    (octets-of (get-output-stream-string out))))

(define-test (metadata pdf-planted-objects)
  ;; what the cross-reference says is read: an object written again after the last save, or in a comment, is not
  (flet ((note (octets) (first (tuatara.defaults::pdf-metadata octets))))
    (let ((objects '((1 . "<< /Type /Catalog >>") (6 . "<< /Title (Real) >>"))))
      (let ((after (note (pdf-with-table objects :after (format nil "6 0 obj~%<< /Title (Planted) >>~%endobj~%")))))
        (true (search "- Title :: Real" after))
        (false (search "Planted" after))
        (false (search "not from the cross-reference" after)))
      (let* ((octets (pdf-with-table objects :before (format nil "% 99 0 obj << >> stream~%")
                                             :after (format nil "% 6 0 obj << /Title (Planted) >> endobj~%")))
             (commented (note octets))
             (numbers '()))
        (true (search "- Title :: Real" commented))
        (true (search "- Saved :: 1 time" commented))
        (is = 1 (length (tuatara:pdf-eof-lines octets)))
        (tuatara:map-pdf-written-objects (lambda (o) (push (tuatara:pdf-indirect-get o :number) numbers) nil) octets)
        (is equal '(1 6) (reverse numbers))))
    ;; with no cross-reference to go by, the objects are found by scanning, and the note says so
    (let ((scanned (note (octets-of "%PDF-1.7" 10 "6 0 obj << /Title (Scanned) >> endobj" 10
                                    "trailer << /Info 6 0 R >>" 10 "%%EOF" 10))))
      (true (search "- Title :: Scanned" scanned))
      (true (search "not from the cross-reference" scanned)))))

(define-test (metadata pdf-packed-info)
  ;; the information an object stream holds, as the cross-reference stream says, not one of its number
  ;; written outside
  (let* ((packed "<< /Title (Real) >>")
         (header "6 0 ")
         (out (make-string-output-stream)) (at 0) (offsets '()))
    (flet ((put (text) (write-string text out) (incf at (length text)))
           (object (number text) (push (cons number at) offsets) (format nil "~d 0 obj~%~a~%endobj~%" number text)))
      (put (format nil "%PDF-1.7~%"))
      (put (object 1 "<< /Type /Catalog >>"))
      (put (object 2 (format nil "<< /Type /ObjStm /N 1 /First ~d /Length ~d >>~%stream~%~a~a~%endstream"
                             (length header) (+ (length header) (length packed)) header packed)))
      (put (object 6 "<< /Title (Planted) >>"))
      (let* ((xref at)
             (rows (octets-of 0 0 0 255
                              1 (ash (cdr (assoc 1 offsets)) -8) (logand (cdr (assoc 1 offsets)) 255) 0
                              1 (ash (cdr (assoc 2 offsets)) -8) (logand (cdr (assoc 2 offsets)) 255) 0
                              2 0 2 0)))
        (put (format nil "7 0 obj~%<< /Type /XRef /Size 8 /Index [0 3 6 1] /W [1 2 1] /Root 1 0 R /Info 6 0 R ~
                          /Length ~d >>~%stream~%"
                     (length rows)))
        (let ((octets (octets-of (get-output-stream-string out) rows
                                 (format nil "~%endstream~%endobj~%startxref~%~d~%%%EOF~%" xref))))
          (let ((note (first (tuatara.defaults::pdf-metadata octets))))
            (true (search "- Title :: Real" note))
            (false (search "Planted" note))))))))

(define-test (metadata pdf-actions-in-object-streams)
  ;; an action kept in a packed object stream, its name written with #xx escapes, is still told of
  (let* ((packed "<< /Type /Catalog /OpenAction << /S /Java#53cript /J#53 (app.alert\\(1\\)) >> >>")
         (header "1 0 ")
         (data (tuatara.io::deflate (octets-of header packed)))
         (octets (octets-of "%PDF-1.7" 10
                            (format nil "2 0 obj~%<< /Type /ObjStm /N 1 /First ~d /Filter /FlateDecode /Length ~d >>~%stream~%"
                                    (length header) (length data))
                            data (format nil "~%endstream~%endobj~%%%EOF~%"))))
    (false (search "JavaScript" (tuatara.text:octets-latin-1 octets)))
    (true (search "- Holds :: JavaScript, an action on opening (none run here)"
                  (first (tuatara.defaults::pdf-metadata octets))))))

(define-test (metadata large-file-read-in-part-said)
  ;; a file read only at its start and end says so: its counts and searches cover those parts only
  (with-test-directory (dir)
    (let* ((tuatara.defaults::*metadata-largest-read* 4096)
           (path (write-test-octets-file (merge-pathnames "big.pdf" dir)
                                         (octets-of "%PDF-1.7" 10 (make-string 20000 :initial-element #\Space)
                                                    "%%EOF" 10)))
           (note (metadata-of path)))
      (true (search "- Read :: its first and last " note) "~a" note))))

(define-test (metadata read-from-one-opening)
  ;; the sum and the facts come from the same opening of the file, never a second one that may be another file
  (with-test-directory (dir)
    (let ((path (write-test-octets-file (merge-pathnames "plain.pdf" dir) (octets-of "%PDF-1.7" 10 "%%EOF" 10)))
          (opened 0))
      (tuatara.defaults::install-metadata)
      (sb-int:encapsulate 'tuatara.io:open-for-reading 'count
                          (lambda (function &rest arguments) (incf opened) (apply function arguments)))
      (unwind-protect (tuatara.defaults::file-metadata-note path)
        (sb-int:unencapsulate 'tuatara.io:open-for-reading 'count))
      (is = 1 opened))))

(define-test (metadata armour-end-looked-for-within-bound)
  ;; armour with no end line: looked for only as far as it is decoded, whatever the size of the file
  (flet ((seconds (size)
           (let ((octets (octets-of (format nil "-----BEGIN PGP MESSAGE-----~%~%")
                                    (make-array size :element-type '(unsigned-byte 8) :initial-element 65)))
                 (start (get-internal-real-time)))
             (dotimes (k 4) (tuatara.defaults::openpgp-armour-octets octets))
             (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    (let ((small (seconds (* 1024 1024))) (large (seconds (* 32 1024 1024))))
      (true (< large (+ 0.1 (* 4 small))) "1 MB in ~,3f s, 32 MB in ~,3f s" small large))))

(define-test (metadata pdf-cross-reference-stream-read)
  ;; a cross-reference stream's entries: types, fields, defaults, and one cut short
  (let ((rows '()))
    (is eq :end (tuatara:map-pdf-cross-reference-stream (lambda (&rest row) (push row rows) nil)
                                                        '(("/W" 1 2 0) ("/Index" 4 2))
                                                        (octets-of 1 0 9 2 0 3)))
    (is equal '((4 1 9 0) (5 2 3 0)) (reverse rows)))
  (is eq :short (tuatara:map-pdf-cross-reference-stream (constantly nil) '(("/W" 1 2 1) ("/Size" . 3))
                                                         (octets-of 1 0 9 0)))
  (is eq :stopped (tuatara:map-pdf-cross-reference-stream (constantly t) '(("/W" 0 1 0) ("/Size" . 1)) (octets-of 7)))
  (false (tuatara:map-pdf-cross-reference-stream (constantly nil) '(("/W" 1 9 1) ("/Size" . 1)) (octets-of 1 2 3))))

(define-test (metadata pdf-unconfirmed-stream-keeps-saves)
  ;; a stream whose end is not confirmed hides no save: the %%EOF in its span ends one, and is named
  (let* ((first-save (pdf-with-table '((1 . "<< /Type /Catalog >>")
                                       (4 . "<< /Length 5 >>
stream
hello
         
endobj"))))
         (octets (octets-of first-save "4 0 obj << /Length 3 >> stream" 10 "new" 10 "endstream endobj" 10 "%%EOF" 10)))
    (multiple-value-bind (lines doubtful) (tuatara:pdf-eof-lines octets)
      (is = 2 (length lines))
      (is = 1 (length doubtful))
      (is = 4 (cdr (first doubtful))))
    (is = 2 (count 4 (let ((numbers '()))
                       (tuatara:map-pdf-written-objects (lambda (o) (push (tuatara:pdf-indirect-get o :number) numbers) nil)
                                                        octets)
                       numbers)))))

(defun pdf-sample (name)
  "The octets of the sample PDF NAME of test/pdf-reader/."
  (alexandria:read-file-into-byte-vector
   (asdf:system-relative-pathname :tuatara (format nil "test/pdf-reader/~a" name))))

(define-test (metadata pdf-saves-by-chain)
  ;; a save the chain of trailers names, or one written whole within a stream as the file's own (its startxref or
  ;; its entries naming the file's places), is a save, though a /Length runs over it: and that length is named a lie
  (dolist (name '("length-over-save.pdf" "indirect-length-over-save.pdf"
                  "length-over-whole-save.pdf" "length-over-whole-save-unnamed.pdf"))
    (let ((octets (pdf-sample name)))
      (multiple-value-bind (lines doubtful lying) (tuatara:pdf-eof-lines octets)
        (declare (ignore doubtful))
        (is = 2 (length lines) "~a" name)
        (is equal '(4) (mapcar #'cdr lying) "~a" name))
      (true (search "- Saved :: 2 times" (first (tuatara.defaults::pdf-metadata octets))) "~a" name)))
  ;; an attached PDF, its own saves within a stream no chain names, ends nothing
  (dolist (name '("attached-pdf.pdf" "attached-pdf-mutool.pdf"))
    (let ((octets (pdf-sample name)))
      (multiple-value-bind (lines doubtful lying) (tuatara:pdf-eof-lines octets)
        (is = 1 (length lines) "~a" name)
        (false doubtful)
        (false lying))
      (true (search "- Saved :: 1 time" (first (tuatara.defaults::pdf-metadata octets))) "~a" name))))

(define-test (metadata pdf-attachments-and-save-signs)
  ;; a PDF attached to itself, or its original to its later save, uncompressed: the attachment's own sections
  ;; count from its own start, and end no save of the file; its file specification linked from nothing, it
  ;; stays a stream a save may hide in
  (loop for (name saves) in '(("self-attached.pdf" 2) ("original-attached-to-later.pdf" 3))
        do (multiple-value-bind (lines doubtful lying signs) (tuatara:pdf-eof-lines (pdf-sample name))
             (declare (ignore doubtful))
             (is = saves (length lines) "~a" name)
             (false lying "~a" name)
             (true signs "~a" name)))
  ;; one the document's embedded files name is an attached file, no sign
  (false (nth-value 3 (tuatara:pdf-eof-lines (pdf-sample "attached-pdf.pdf"))))
  ;; a save written within another stream's data, nothing of the file naming it: a sign, not a save
  ;; a stream merely beginning with %PDF- or calling itself an /EmbeddedFile, no file specification keeping it,
  ;; is no attached file
  (dolist (name '("save-in-stream-xref-stream.pdf" "save-in-stream-offsets.pdf" "save-in-stream-bare.pdf"
                  "save-in-stream-said-pdf.pdf" "save-in-stream-said-embedded.pdf"
                  "save-in-stream-own-file-reached.pdf" "save-in-stream-own-file-unreached.pdf"))
    (multiple-value-bind (lines doubtful lying signs) (tuatara:pdf-eof-lines (pdf-sample name))
      (declare (ignore doubtful))
      (is = 1 (length lines) "~a" name)
      (false lying "~a" name)
      (is = 4 (car (first signs)) "~a" name)
      (true (integerp (cdr (first signs))) "~a" name))))

(define-test (metadata pdf-eof-lines-bounded)
  ;; a stream quoting more %%EOF lines than are read hides neither the save nor what the file says of itself
  (let* ((quoted (with-output-to-string (out) (loop repeat 100001 do (format out "%%EOF~%"))))
         (octets (pdf-with-table `((1 . "<< /Type /Catalog >>") (6 . "<< /Title (Real) >>")
                                   (9 . ,(format nil "<< /Length ~d >>~%stream~%~aendstream" (length quoted) quoted))))))
    (is = 1 (length (tuatara:pdf-eof-lines octets)))
    (true (search "- Saved :: 1 time" (first (tuatara.defaults::pdf-metadata octets))))))

(define-test (metadata pdf-entries-capped)
  ;; a small file listing millions of entries is read to a bound, quickly, and the note says so
  (let* ((start (get-internal-real-time))
         (note (first (tuatara.defaults::pdf-metadata (pdf-sample "many-entries.pdf"))))
         (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (true (search "lists more than 1,000,000 objects" note))
    (true (< seconds 30) "read in ~,1f seconds" seconds))
  ;; sections chained by /Prev, each listing a share: all of them count toward the bound
  (let ((tuatara.defaults::*pdf-most-entries* 1000) (out (make-string-output-stream)) (at 0) (prev nil))
    (flet ((put (text) (write-string text out) (incf at (length text))))
      (put (format nil "%PDF-1.7~%"))
      (dotimes (k 8)
        (let ((here at))
          (put (format nil "~d 0 obj~%<< /Type /XRef /W [1 0 0] /Index [0 200] /Size 200~@[ /Prev ~d~] ~
                            /Length 200 >>~%stream~%"
                       (+ 10 k) prev))
          (put (make-string 200 :initial-element (code-char 1)))
          (put (format nil "~%endstream~%endobj~%"))
          (setf prev here)))
      (put (format nil "startxref~%~d~%%%EOF~%" prev))
      (let ((octets (octets-of (get-output-stream-string out))))
        (multiple-value-bind (sections too-many) (tuatara.defaults::pdf-sections octets (constantly nil))
          (true too-many)
          (is = 6 (length sections)))))))

(define-test (metadata pdf-junk-before-header)
  ;; octets before %PDF-: the offsets the file gives count from its header, as readers take them
  (dolist (junk (list 50 3000))
    (let* ((plain (pdf-with-table '((1 . "<< /Type /Catalog >>") (6 . "<< /Title (Real) >>"))))
           (octets (octets-of (make-string junk :initial-element #\J) plain))
           (note (first (tuatara.defaults::pdf-metadata octets))))
      (is = junk (tuatara:pdf-offset-base octets))
      (is = (+ junk (tuatara:pdf-startxref plain)) (tuatara:pdf-startxref octets))
      (true (search "- Title :: Real" note))
      (false (search "not from the cross-reference" note))))
  (is = 0 (tuatara:pdf-offset-base (pdf-with-table '((1 . "<< /Type /Catalog >>"))))))

(define-test (metadata pdf-object-stream-unplaced)
  ;; an object stream that does not say where its objects are holds none: the information is read as far as it goes
  (let* ((octets (octets-of "%PDF-1.7" 10 "3 0 obj << /Type /ObjStm /N 2 /Length 5 >> stream" 10 "10 0 " 10
                            "endstream endobj" 10 "trailer << /Info 10 0 R >>" 10 "%%EOF" 10))
         (note (first (tuatara.defaults::pdf-metadata octets))))
    (true (search "PDF version" note))))

(define-test (metadata pdf-large-file-read-quickly)
  ;; a PDF of 48 MB, its pictures' octets naming nothing: what it says of itself read in a moment
  (let* ((filler (make-array (* 48 1024 1024) :element-type '(unsigned-byte 8) :initial-element 255))
         (octets (octets-of "%PDF-1.7" 10 "1 0 obj << /Title (Kept) >> endobj" 10
                            (format nil "2 0 obj << /Length ~d >> stream~%" (length filler)) filler
                            (format nil "~%endstream endobj~%trailer << /Info 1 0 R >>~%%%EOF~%")))
         (start (get-internal-real-time))
         (sections (tuatara.defaults::pdf-metadata octets))
         (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (true (search "- Title :: Kept" (first sections)))
    (true (< seconds 2) "read in ~,2f seconds" seconds)))

(define-test (metadata pdf-long-literal-string-read-whole)
  ;; a string longer than what is kept ends at its own closing bracket: nothing in it is read as keys
  (let* ((inside (format nil "~a /Author (Planted)" (make-string 70000 :initial-element #\a)))
         (octets (octets-of "(" inside ") /Next" 10)))
    (multiple-value-bind (text end) (tuatara.defaults::pdf-literal-string-at octets 0)
      (is = 65536 (length text))
      (is = (+ 2 (length inside)) end)))
  ;; a backslash before a CR LF line end passes over both
  (is string= "ab" (tuatara.defaults::pdf-literal-string-at (octets-of "(a\\" 13 10 "b)") 0)))

(define-test (metadata pdf-streams-without-end-read-in-step)
  ;; streams with no endstream after them: the file is read in a time in step with its size, not its square
  (flet ((seconds (count)
           (let ((octets (octets-of (with-output-to-string (out)
                                      (format out "%PDF-1.7~%")
                                      (dotimes (k count) (format out "~d 0 obj<<>>stream~%" (1+ k))))))
                 (start (get-internal-real-time)))
             (tuatara.defaults::pdf-objects-written octets)
             (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    (let ((small (seconds 5000)) (large (seconds 20000)))
      (true (< large (+ 0.5 (* 8 small))) "5,000 in ~,3f s, 20,000 in ~,3f s" small large))))

(define-test (metadata octets-search-agrees)
  ;; the quick search finds what SEARCH finds, from the start or the end, within bounds, in any vector
  (let ((random (sb-ext:seed-random-state 7)) (disagreeing '()))
    (dotimes (k 2000)
      (let* ((octets (coerce (loop repeat (random 60 random) collect (random 3 random)) 'tuatara.text:octets))
             (needle (coerce (loop repeat (random 4 random) collect (random 3 random)) 'tuatara.text:octets))
             (start (random (1+ (length octets)) random))
             (end (+ start (random (1+ (- (length octets) start)) random)))
             (from-end (zerop (random 2 random))))
        (unless (eql (search needle octets :start2 start :end2 end :from-end from-end)
                     (tuatara.io:octets-search needle octets :start start :end end :from-end from-end))
          (push (list needle octets start end from-end) disagreeing))))
    (is equal '() disagreeing))
  (is = 2 (tuatara.io:octets-search #(1 2) (make-array 4 :initial-contents '(0 0 1 2) :adjustable t)))
  (false (tuatara.io:octets-search (octets-of "obj") (octets-of "ob")))
  ;; crossed or outside bounds answer NIL in any vector, never an error
  (let ((adjustable (make-array 4 :initial-contents '(0 0 1 2) :adjustable t)))
    (dolist (octets (list (octets-of 0 0 1 2) adjustable))
      (false (tuatara.io:octets-search #(1) octets :start 3 :end 1))
      (false (tuatara.io:octets-search #(1) octets :start 9))
      (is = 2 (tuatara.io:octets-search #(1) octets :start -5 :end 99))
      (false (tuatara.io:octets-search #() octets :start 3 :end 1)))))

(defun zlib-stored-octets (octets)
  "OCTETS as zlib data of one stored deflate block, as /FlateDecode takes them."
  (let ((n (length octets)))
    (octets-of #x78 #x01 1 (logand n 255) (ash n -8) (logand (lognot n) 255) (logand (ash (lognot n) -8) 255) octets 0 0 0 0)))

(define-test (metadata pdf-objects)
  ;; numbers, from their octets; N G R a reference, RG a word
  (is equal '(42 2) (multiple-value-list (tuatara.defaults::pdf-number-at (octets-of "42 ") 0)))
  (is = -1.5d0 (tuatara.defaults::pdf-number-at (octets-of "-1.5]") 0))
  (is = 0.25d0 (tuatara.defaults::pdf-number-at (octets-of ".25") 0))
  (is equal '((:ref 12 0) 6) (multiple-value-list (tuatara.defaults::pdf-number-at (octets-of "12 0 R>>") 0)))
  (is = 1 (tuatara.defaults::pdf-number-at (octets-of "1 0 RG") 0))
  (false (tuatara.defaults::pdf-number-at (octets-of "-") 0))
  (false (tuatara.defaults::pdf-number-at (octets-of (make-string 40 :initial-element #\7)) 0))
  (is equal '(12 4) (multiple-value-list (tuatara.defaults::pdf-integer-at (octets-of "  12x") 0)))
  (false (tuatara.defaults::pdf-integer-at (octets-of "1234567890123456") 0))
  ;; values: a dictionary's entries, a reference's number, an array's items
  (let ((dictionary (tuatara.defaults::pdf-object-at (octets-of "<< /Type /Page /Kids [1 0 R 2 0 R] /Parent 3 0 R >>") 0)))
    (true (tuatara.defaults::pdf-dictionary-p dictionary))
    (is string= "/Page" (tuatara.defaults::pdf-get dictionary "/Type"))
    (is = 3 (tuatara.defaults::pdf-ref-p (tuatara.defaults::pdf-get dictionary "/Parent")))
    (is equal '((:ref 1 0) (:ref 2 0))
        (tuatara.defaults::pdf-array-items (tuatara.defaults::pdf-get dictionary "/Kids")))
    (is equal '("/Page") (tuatara.defaults::pdf-array-items "/Page"))
    (false (tuatara.defaults::pdf-get '(1 2) "/Type")))
  ;; an object as written: its stream as long as /Length says, else as far as endstream
  (let* ((octets (octets-of "%PDF-1.4" 10 "4 0 obj << /Length 5 >> stream" 13 10 "hello" 13 10 "endstream endobj" 10
                            "5 0 obj << /Length 99 >> stream" 10 "world" 13 10 "endstream" 10 "endobj"))
         (four (tuatara.defaults::pdf-indirect-object-at octets 9))
         (five-at (search (octets-of "5 0 obj") octets))
         (five (tuatara.defaults::pdf-indirect-object-at octets five-at)))
    (is = 4 (tuatara:pdf-indirect-get four :number))
    (is equalp (octets-of "hello")
        (subseq octets (tuatara:pdf-indirect-get four :stream-start) (tuatara:pdf-indirect-get four :stream-end)))
    (is equalp (octets-of "world")
        (subseq octets (tuatara:pdf-indirect-get five :stream-start) (tuatara:pdf-indirect-get five :stream-end)))
    (is = (length octets) (tuatara:pdf-indirect-get five :end))
    (is equal (list five-at 5 0)
        (multiple-value-list (tuatara.defaults::pdf-object-header-before octets (+ five-at 4))))
    (false (tuatara.defaults::pdf-indirect-object-at octets 0)))
  ;; streams decoded: each filter, filters in a row, PNG's predictor, a picture's data left as it is, the limit kept
  (is equalp (octets-of "Hello") (tuatara.defaults::pdf-ascii-hex-decoded (octets-of "48 65 6C6c6F>") 100))
  (is equalp (octets-of "Man ") (tuatara.defaults::pdf-ascii-85-decoded (octets-of "9jqo^~>") 100))
  (is equalp (octets-of "abcxxx") (tuatara.defaults::pdf-run-length-decoded (octets-of 2 "abc" 254 "x" 128) 100))
  (is equalp (octets-of 45 45 45 45 45 65 45 45 45 66)
      (tuatara.defaults::pdf-lzw-decoded (octets-of #x80 #x0B #x60 #x50 #x22 #x0C #x0C #x85 #x01) 100 1))
  (is equalp (octets-of "abcxxx")
      (tuatara.defaults::pdf-stream-octets '(("/Filter" "/AHx" "/RL")) (octets-of "02616263FE7880>")))
  (is equalp (octets-of 1 2 2 3)
      (tuatara.defaults::pdf-stream-octets '(("/Filter" . "/FlateDecode")
                                             ("/DecodeParms" ("/Predictor" . 12) ("/Columns" . 2)))
                                           (zlib-stored-octets (octets-of 2 1 2 2 1 1))))
  (is equalp '(#(255 216) "/DCTDecode")
      (multiple-value-list (tuatara.defaults::pdf-stream-octets '(("/Filter" . "/DCTDecode")) (octets-of 255 216))))
  (multiple-value-bind (data why) (tuatara.defaults::pdf-stream-octets '(("/Filter" . "/FlateDecode")) (zlib-stored-octets (make-array 2000 :element-type '(unsigned-byte 8) :initial-element 0)) :limit 1000)
    (false data)
    (true (search "more than" why)))
  (multiple-value-bind (data why) (tuatara.defaults::pdf-stream-octets '(("/F" . "elsewhere.dat")) (octets-of "x"))
    (false data)
    (true (search "another file" why)))
  ;; an object stream's objects
  (let* ((data (octets-of "7 0 8 8 (seven) (eight)"))
         (offsets (tuatara.defaults::pdf-object-stream-offsets '(("/N" . 2) ("/First" . 8)) data))
         (eight (tuatara.defaults::pdf-packed-object data offsets 8 :index 1 :in 6)))
    (is equalp #((7 . 8) (8 . 16)) offsets)
    (is string= "eight" (tuatara:pdf-indirect-get eight :value))
    (is = 6 (tuatara:pdf-indirect-get eight :packed-in))
    (is string= "seven" (tuatara:pdf-indirect-get (tuatara.defaults::pdf-packed-object data offsets 7 :index 1) :value))
    (false (tuatara.defaults::pdf-packed-object data offsets 9))
    (false (tuatara.defaults::pdf-object-stream-offsets '(("/N" . 2) ("/First" . 99)) data)))
  ;; a header after other octets, as a PDF may have; a text naming %PDF- is still text
  (is = 10 (tuatara.io:pdf-header-at (octets-of "junk junk" 10 "%PDF-1.4" 10)))
  (false (tuatara.io:pdf-header-at (octets-of "say %PDF- here")))
  (false (tuatara.io:pdf-header-at (octets-of (make-string 1100 :initial-element #\x) "%PDF-1.4")))
  (is eq :pdf (nth-value 1 (tuatara.io:octets-kind (octets-of "MIME junk" 10 "%PDF-1.4" 10 "%" 0 226 10))))
  (is string= "PDF document, version 1.4"
      (tuatara.io:octets-kind (octets-of "MIME junk" 10 "%PDF-1.4" 10 "%" 0 226 10)))
  (false (eq :pdf (nth-value 1 (tuatara.io:octets-kind (octets-of "Notes: a PDF begins %PDF-1.7 as a rule" 10))))))

(defun two-saves-pdf ()
  "A PDF saved twice, with each part's offset: the octets, and an alist of
:CATALOG, :STREAM, :XREF, :STARTXREF, :EOF, :CHANGED, :XREF-2 and :EOF-2 to
where each begins."
  (let ((parts '()) (out (make-string-output-stream)) (at 0))
    (flet ((put (key text)
             (when key (push (cons key at) parts))
             (write-string text out)
             (incf at (length text))))
      (put nil (format nil "%PDF-1.4~%"))
      (put :catalog (format nil "1 0 obj << /Type /Catalog >> endobj~%"))
      (put :stream (format nil "2 0 obj << /Length 16 >> stream~%9 0 obj 1 endobj~%endstream endobj~%"))
      (put :xref (format nil "xref~%0 3~%0000000000 65535 f ~%~10,'0d 00000 n ~%~10,'0d 00000 n ~%"
                         (cdr (assoc :catalog parts)) (cdr (assoc :stream parts))))
      (put nil (format nil "trailer << /Size 3 /Root 1 0 R >>~%"))
      (put :startxref (format nil "startxref~%~d~%" (cdr (assoc :xref parts))))
      (put :eof (format nil "%%EOF~%"))
      (put :changed (format nil "1 0 obj << /Type /Catalog /Lang (mi) >> endobj~%"))
      (put :xref-2 (format nil "xref~%0 1~%0000000000 65535 f ~%1 1~%~10,'0d 00000 n ~%" (cdr (assoc :changed parts))))
      (put nil (format nil "trailer << /Size 3 /Prev ~d >>~%startxref~%" (cdr (assoc :xref parts))))
      (put nil (format nil "~d~%" (cdr (assoc :xref-2 parts))))
      (put :eof-2 (format nil "%%EOF~%"))
      (values (octets-of (get-output-stream-string out)) parts))))

(define-test (metadata pdf-saves-read)
  ;; a PDF saved twice: its objects in the order written, none read from inside a stream's octets
  (multiple-value-bind (octets parts) (two-saves-pdf)
    (flet ((at (key) (cdr (assoc key parts))))
      (let ((written '()))
        (is eq nil (tuatara:map-pdf-written-objects (lambda (object) (push object written) nil) octets))
        (is equal '(1 2 1) (mapcar (lambda (o) (tuatara:pdf-indirect-get o :number)) (reverse written)))
        (is equal (list (at :catalog) (at :stream) (at :changed))
            (mapcar (lambda (o) (tuatara:pdf-indirect-get o :at)) (reverse written)))
        (is eq :first (tuatara:map-pdf-written-objects (constantly :first) octets)))
      ;; each save's %%EOF and where its line ends, and the section its startxref names
      (is equal (list (cons (at :eof) (+ (at :eof) 6)) (cons (at :eof-2) (length octets)))
          (tuatara:pdf-eof-lines octets))
      (is equal (list (at :xref) (at :startxref)) (multiple-value-list (tuatara:pdf-startxref octets :end (at :eof))))
      (is = (at :xref-2) (tuatara:pdf-startxref octets))
      (false (tuatara:pdf-startxref octets :end (at :startxref)))
      ;; the cross-reference tables: their entries, how they ended, the trailer after them
      (let ((rows '()))
        (multiple-value-bind (ending trailer)
            (tuatara:map-pdf-cross-reference-table (lambda (&rest row) (push row rows) nil) octets (at :xref))
          (is eq :end ending)
          (is = 3 (tuatara:pdf-get trailer "/Size"))
          (is equal `((0 0 65535 :free) (1 ,(at :catalog) 0 :used) (2 ,(at :stream) 0 :used)) (reverse rows))))
      (let ((rows '()))
        (is eq :end
            (tuatara:map-pdf-cross-reference-table (lambda (&rest row) (push row rows) nil) octets (at :xref-2)))
        (is equal `((0 0 65535 :free) (1 ,(at :changed) 0 :used)) (reverse rows)))
      (is eq :stopped (tuatara:map-pdf-cross-reference-table (constantly t) octets (at :xref)))
      (false (tuatara:map-pdf-cross-reference-table (constantly nil) octets (at :catalog)))))
  ;; tables that do not make sense: cut short, an entry that is none, a count no file holds
  (is eq :cut-short (tuatara:map-pdf-cross-reference-table (constantly nil) (octets-of "xref 0 x") 0))
  (let ((octets (octets-of "xref" 10 "0 2" 10 "0000000000 65535 f " 10 "trailer << >>")))
    (is equal (list :unreadable nil (search (octets-of " " 10 "trailer") octets))
        (multiple-value-list (tuatara:map-pdf-cross-reference-table (constantly nil) octets 0))))
  (is eq :unreadable (tuatara:map-pdf-cross-reference-table (constantly nil) (octets-of "xref 0 999999999999999 ") 0))
  ;; a dictionary's entries, with where each key and value end
  (let ((octets (octets-of "  << /A 1 /B (two) >> x")) (entries '()))
    (is equal '(2 19)
        (multiple-value-list (tuatara:map-pdf-dictionary (lambda (&rest entry) (push entry entries)) octets 0)))
    (is equal '(("/A" 1 7 9) ("/B" "two" 12 18)) (reverse entries)))
  (false (tuatara:map-pdf-dictionary (constantly nil) (octets-of "<< /A 1") 0))
  (false (tuatara:map-pdf-dictionary (constantly nil) (octets-of "<< 1 2 >>") 0))
  (false (tuatara:map-pdf-dictionary (constantly nil) (octets-of "[ 1 2 ]") 0)))

(defun content-operators (text &rest keys)
  "The operators MAP-PDF-CONTENT reads in TEXT, with KEYS: each (OPERATOR
OPERANDS FROM END), and how many it counted, as two values."
  (let ((out '()))
    (let ((count (apply #'tuatara:map-pdf-content (lambda (&rest operator) (push operator out)) (octets-of text) keys)))
      (values (reverse out) count))))

(define-test (metadata pdf-content-read)
  ;; a page's operators with their operands, where they begin and end; nothing run
  (is equalp '(("q" () 0 1) ("cm" (1 0 0 1 72 720.5d0) 2 21) ("Tf" ("/F1" 12) 22 31)
               ("Tj" (#(72 105 41)) 32 43) ("TJ" ((#(97) -20 #(98))) 44 60) ("d0" (:true :false :null) 61 79))
      (content-operators "q 1 0 0 1 72 720.5 cm /F1 12 Tf (Hi\\051) Tj [(a) -20 (b)] TJ true false null d0"))
  (is equalp '(("Tj" (#(171 205)) 0 10)) (content-operators "<AB CD> Tj"))
  (is equalp '(("gs" ((("/CA" . 0.5d0))) 0 15)) (content-operators "<< /CA .5 >> gs"))
  ;; an inline picture: its dictionary, its data passed over to an EI standing alone
  (is equalp '(("BI" ((("/W" . 2) ("/H" . 1))) 0 23) ("Q" () 24 25)) (content-operators "BI /W 2 /H 1 ID xEIx EI Q"))
  ;; held to a count, and files made to mislead read to their end
  (is = 5 (nth-value 1 (content-operators (format nil "~v@{~a ~:*~}" 100 "0 0 m") :limit 5)))
  (is = 1 (nth-value 1 (content-operators (concatenate 'string (make-string 5000 :initial-element #\[)
                                                       (make-string 5000 :initial-element #\]) " x"))))
  (is = 0 (nth-value 1 (content-operators "1 2 (never closed Tj")))
  (is = 0 (nth-value 1 (content-operators "1 2 <never closed Tj")))
  (is = 1 (nth-value 1 (content-operators "BI /W 2 ID no end")))
  (is = 1 (nth-value 1 (content-operators "BI /W")))
  (is = 1 (nth-value 1 (content-operators "BI /W  "))))

;;; Sound and video

(define-test (metadata id3-v2.4-extended-header)
  ;; version 2.4: the extended header's synchsafe size takes in its own four octets
  (let* ((frame (octets-of "TIT2" 0 0 0 9 0 0 3 "Kea call"))
         (tag (octets-of 0 0 0 6 1 0 frame))
         (size (length tag))
         (octets (octets-of "ID3" 4 0 #x40 0 0 (ldb (byte 7 7) size) (ldb (byte 7 0) size) tag)))
    (true (search "- Title :: Kea call" (format nil "~{~a~}" (tuatara.defaults::id3-metadata octets))))))

(define-test (metadata sound-and-video-read)
  (with-test-directory (dir)
    ;; MP3: ID3 version 2.3 at its start, version 1 at its end
    (flet ((frame (id data) (octets-of id (be-octets (length data) 4) 0 0 data)))
      (let* ((frames (octets-of (frame "TIT2" (octets-of 0 "Kea call"))
                                (frame "TPE1" (octets-of 1 #xFF #xFE "A" 0 "n" 0 "a" 0))
                                (frame "COMM" (octets-of 0 "eng" 0 "Recorded at dawn"))
                                (frame "TXXX" (octets-of 0 "Place" 0 "Arthur's Pass"))
                                (frame "APIC" (octets-of 0 "image/png" 0 3 0 1 2 3 4))))
             (size (length frames))
             (mp3 (write-test-octets-file
                   (merge-pathnames "kea.mp3" dir)
                   (octets-of "ID3" 3 0 0 (ldb (byte 7 21) size) (ldb (byte 7 14) size) (ldb (byte 7 7) size) (ldb (byte 7 0) size)
                              frames #xFF #xFB #x90 0 (make-array 200
                                                                  :element-type '(unsigned-byte 8)
                                                                  :initial-element 0)
                              "TAG"
                              (octets-of "Old title"
                                         (make-array 21 :element-type '(unsigned-byte 8) :initial-element 0))
                              (make-array 95 :element-type '(unsigned-byte 8) :initial-element 0))))
             (note (metadata-of mp3)))
        (note-has note "* ID3 tags")
        (note-has note "- Title :: Kea call")
        (note-has note "- Artist :: Ana")
        (note-has note "- Comment :: Recorded at dawn")
        (note-has note "- TXXX :: Place: Arthur's Pass")
        (note-has note "- Picture :: 17 octets of a picture kept inside it")
        (note-has note "- Title (ID3v1) :: Old title")))
    ;; FLAC: its length and comments
    (let* ((comments (octets-of (le-octets 7 4)
                                "Kea rec"
                                (le-octets 2 4)
                                (le-octets 10 4)
                                "TITLE=Dawn"
                                (le-octets 13 4)
                                "ARTIST=Ana W."))
           (info (octets-of (be-octets 0 4) (be-octets 0 3) (be-octets 0 3)
                            (be-octets (logior (ash 44100 44) (ash 1 41) (ash 15 36) 441000) 8)
                            (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
           (flac (write-test-octets-file (merge-pathnames "kea.flac" dir)
                                         (octets-of "fLaC" 0 (be-octets (length info) 3) info #x84 (be-octets (length comments) 3) comments)))
           (note (metadata-of flac)))
      (note-has note "- Length :: 10.0 seconds, 44,100 samples a second")
      (note-has note "- Made by :: Kea rec")
      (note-has note "- TITLE :: Dawn")
      (note-has note "- ARTIST :: Ana W."))
    ;; a phone's video: its movie header's times and length, its place, maker and model as keyed items
    (let* ((mvhd (box "mvhd" 0 0 0 0 (be-octets 3855888000 4) (be-octets 3855888060 4) (be-octets 600 4) (be-octets 7200 4)
                      (make-array 80 :element-type '(unsigned-byte 8) :initial-element 0)))
           (udta (box "udta" (box "©xyz" (be-octets 18 2) (be-octets #x15c7 2) "-41.2865+174.7762/")))
           (keys (box "keys" 0 0 0 0 (be-octets 2 4)
                      (be-octets 44 4) "mdta" "com.apple.quicktime.location.ISO6709"
                      (be-octets 33 4) "mdta" "com.apple.quicktime.model"))
           (ilst (box "ilst" (box (be-octets 2 4) (box "data" (be-octets 1 4) (be-octets 0 4) "Kea Phone 9"))))
           (meta (box "meta" (box "hdlr" 0 0 0 0 (be-octets 0 4) "mdta" (be-octets 0 12) 0) keys ilst))
           (mov (write-test-octets-file (merge-pathnames "kea.mov" dir)
                                        (octets-of (box "ftyp" "qt  " (be-octets 0 4) "qt  ")
                                                   (box "moov" mvhd udta meta))))
           (note (metadata-of mov)))
      (note-has note "* What the video or sound says of itself")
      (note-has note "- Brand :: qt")
      (note-has note "- Made (its movie header) :: 2026-03-09")
      (note-has note "- Length :: 12.0 seconds")
      (note-has note "- Position :: -41.2865+174.7762/")
      (note-has note "- com.apple.quicktime.model :: Kea Phone 9"))))

;;; Office files, the command, files made to mislead

(define-test (metadata office-and-command)
  (with-test-directory (dir)
    (let* ((xlsx (write-office-test-file (merge-pathnames "birds.xlsx" dir) *xlsx-test-parts*))
           (note (metadata-of xlsx)))
      (note-has note "* About this workbook")
      (note-has note "- Made by :: Bo Kaikoura")
      (false (search "* Sheet 1" note)))
    ;; M-x show-file-metadata in a buffer visiting the file: its own buffer, read in the background, the file's
    ;; access time as it was
    (let ((path (write-test-octets-file (merge-pathnames "kea.jpg" dir) (jpeg-test-octets))))
      (sb-posix:utimes path 946681200 946681200)
      (docx-session path (list (format nil "M-x ~a RET" (rimu-typed "show-file-metadata")) ""))
      (let ((visited (tuatara.buffer:find-buffer "kea.jpg")))
        (when visited (tuatara.buffer:kill-buffer visited)))
      (let ((shown (tuatara.buffer:find-buffer "*Metadata: kea.jpg*")))
        (true shown)
        (when shown
          (note-has (tuatara.buffer:buffer-string shown) "- Accessed :: 2000-01-01")
          (note-has (tuatara.buffer:buffer-string shown) "- Make :: Kea Cameras")
          (true (tuatara.buffer:buffer-local shown :read-only))
          (tuatara.buffer:kill-buffer shown)))
      (is = 946681200 (sb-posix:stat-atime (sb-posix:stat path))))))

(define-test (metadata misleading-files)
  ;; loops, sizes past the end, counts in the millions: read as far as they make sense, never hanging
  (with-test-directory (dir)
    (let ((start (get-internal-real-time)))
      (flet ((note-of (name octets) (metadata-of (write-test-octets-file (merge-pathnames name dir) octets))))
        ;; EXIF whose parts lead to themselves, entries in their millions, offsets past the end
        (let ((loop-tiff (octets-of "II" 42 0 (le-octets 8 4) (le-octets 2 2)
                                    (le-octets #x8769 2) (le-octets 4 2) (le-octets 1 4) (le-octets 8 4)
                                    (le-octets #x010F 2) (le-octets 2 2) (le-octets 4000000 4) (le-octets 99999 4)
                                    (le-octets 8 4))))
          (note-has (note-of "loop.tif" loop-tiff) "* The file")
          (note-has (note-of "many.jpg" (octets-of #xFF #xD8 #xFF #xE1 #xFF #xFF "Exif" 0 0 "II" 42 0 (le-octets 8 4) (le-octets 65535 2))) "* The file"))
        ;; a PDF whose information names itself, and whose strings never close
        (note-has (note-of "self.pdf" (octets-of "%PDF-1.4" 10 "3 0 obj 3 0 R endobj trailer << /Info 3 0 R >> %%EOF"))
             "- PDF version :: 1.4")
        (note-has (note-of "open.pdf"
                    (octets-of "%PDF-1.4" 10 "3 0 obj << /Title (((((never closed" 10 "trailer << /Info 3 0 R >>"))
             "* What the PDF says of itself")
        ;; boxes of no size, of sizes past the end; an ID3 tag longer than the file
        (note-has (note-of "boxes.mp4"
                    (octets-of (box "ftyp" "isom" (be-octets 0 4)) (be-octets 0 4) "moov" (be-octets 999999 4) "mvhd"))
                  "- Brand :: isom")
        (note-has (note-of "tag.mp3" (octets-of "ID3" 4 0 0 127 127 127 127 "TIT2" (be-octets 999999 4) 0 0 0 "x"))
                  "* The file")
        ;; a PNG whose chunk says it is larger than the file
        (note-has (note-of "big.png" (octets-of #x89 "PNG" 13 10 26 10 (be-octets #x7FFFFFFF 4) "tEXt" "x"))
                  "* The file"))
      (true (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second))))))

(define-test (metadata exif-fields-bounded)
  ;; a thousand fields each claiming most of the file are kept to their first 4 KB, not copied whole each time
  (let* ((size (* 256 1024))
         (octets (make-array size :element-type '(unsigned-byte 8) :initial-element 65)))
    (replace octets (octets-of "II" 42 0 (le-octets 8 4) (le-octets 1000 2)))
    (dotimes (i 1000)
      (replace octets
               (octets-of (le-octets #x9286 2) (le-octets 7 2) (le-octets (- size 16) 4) (le-octets 0 4))
               :start1 (+ 10 (* 12 i))))
    (let ((fields (tuatara.defaults::exif-read octets 0)))
      (is = 1000 (length fields))
      (true (every (lambda (field) (<= (length (third field)) 4096)) fields)))))

;;; OpenPGP

(defun openpgp-test-packet (tag body)
  "An OpenPGP packet of TAG holding BODY (octets), in the new form, its length in one octet."
  (octets-of (logior #xC0 tag) (length body) body))

(define-test (metadata openpgp-read)
  (with-test-directory (dir)
    (let* ((zeros (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
           (salt (make-array 16 :element-type '(unsigned-byte 8) :initial-element 7))
           (fingerprint (octets-of (be-octets #xA1B2C3D4E5F60718 8)
                                   (make-array 24 :element-type '(unsigned-byte 8) :initial-element 9)))
           (packets (octets-of
                     ;; to an RSA key, the old form of header; to a key its sender left out; to a version 6 key
                     (octets-of #x84 13 3 (be-octets #x0123456789ABCDEF 8) 1 0 8 #xAB)
                     (openpgp-test-packet 1 (octets-of 3 zeros 18 0 8 #xCD))
                     (openpgp-test-packet 1 (octets-of 6 33 6 fingerprint 25 1 2 3))
                     ;; passphrases: iterated and salted SHA-512; Argon2
                     (openpgp-test-packet 3 (octets-of 4 9 3 10 (subseq salt 0 8) #xFF))
                     (openpgp-test-packet 3 (octets-of 6 23 9 2 20 4 salt 1 4 21 1 2))
                     ;; the data, authenticated
                     (openpgp-test-packet 18 (octets-of 2 9 2 6 1 2 3))))
           (path (write-test-octets-file (merge-pathnames "secret.gpg" dir) packets))
           (note (metadata-of path)))
      (true (eq :openpgp (nth-value 1 (tuatara.io:octets-kind packets))))
      (note-has note "OpenPGP encrypted message")
      (note-has note "* Encryption (OpenPGP)")
      (note-has note "- Encrypted to :: 3 keys")
      (note-has note "- Passphrase :: yes, any of 2")
      (note-has note "- Key 1 :: 0123456789ABCDEF, RSA: owner not looked up (*metadata-look-up-key-owners*)")
      (note-has note "- Key 2 :: hidden: the sender left its ID out, ECDH")
      (note-has note "- Key 3 :: A1B2C3D4E5F60718 (fingerprint A1B2C3D4E5F60718")
      (note-has note ", X25519: owner not looked up")
      (note-has note "- Passphrase 1 :: AES-256, the passphrase made a key by SHA-512, salted and repeated, 65,011,712 octets hashed")
      (note-has note "- Passphrase 2 :: AES-256, the passphrase made a key by Argon2, 1 pass, 4 lanes, 2 GiB of memory")
      (note-has note "- Modification check :: authenticated encryption: AES-256, OCB")
      ;; armoured, with its headers; the same packets read
      (let* ((armoured (format nil "-----BEGIN PGP MESSAGE-----~%Comment: from the field~%~%~a~%=abcd~%-----END PGP MESSAGE-----~%"
                               (base64-of packets)))
             (note (metadata-of (write-test-octets-file (merge-pathnames "secret.asc" dir) (octets-of armoured)))))
        (note-has note "OpenPGP encrypted message, armoured")
        (note-has note "- Encrypted to :: 3 keys")
        (note-has note "- Armour header Comment :: from the field"))
      ;; cut short, or an old packet without a modification check
      (note-has (metadata-of (write-test-octets-file (merge-pathnames "cut.gpg" dir) (subseq packets 0 20)))
                "- Modification check :: not reached")
      (let ((old (write-test-octets-file (merge-pathnames "old.gpg" dir)
                                         (octets-of (openpgp-test-packet 3 (octets-of 4 3 0 1))
                                                    (openpgp-test-packet 9 (octets-of 1 2 3))))))
        (note-has (metadata-of old) "the passphrase made a key by MD5 of the passphrase alone, no salt: weak")
        (note-has (metadata-of old) "- Modification check :: none: an old form")
        ;; nothing asked of a file that names no key
        (let ((tuatara-user:*metadata-look-up-key-owners* :ask))
          (false (tuatara.defaults::look-up-key-owners-p old))))
      ;; armour with nothing after its first line
      (note-has (metadata-of (write-test-octets-file (merge-pathnames "bare.asc" dir)
                                                     (octets-of "-----BEGIN PGP MESSAGE-----")))
                "- Modification check :: not reached")
      ;; other files are not taken for one
      (false (eq :openpgp (nth-value 1 (tuatara.io:octets-kind (octets-of #x84 2 9 9))))))))

(defun base64-of (octets)
  "OCTETS in base64, without line breaks."
  (let ((letters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (s)
      (loop for i from 0 below (length octets) by 3
            for n = (min 3 (- (length octets) i))
            for bits = (loop for k below 3 sum (ash (if (< k n) (aref octets (+ i k)) 0) (* 8 (- 2 k))))
            do (loop for k below 4
                     do (write-char (if (<= k n) (char letters (ldb (byte 6 (* 6 (- 3 k))) bits)) #\=) s))))))

(define-test (metadata openpgp-key-owners)
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-test-directory (dir)
        (let* ((key (gpg-test-key))
               (path (namestring (gpg-test-file (merge-pathnames "plans.gpg" dir) "plans" :key key :passphrase "kea"))))
          (tuatara.defaults::install-metadata)
          (let ((note (tuatara.defaults::file-metadata-note path :look-up-keys t)))
            (note-has note "- Passphrase :: yes")
            (note-has note ": Kea Taranaki <kea@example.org>"))
          (let ((tuatara-user:*metadata-look-up-key-owners* nil))
            (false (tuatara.defaults::look-up-key-owners-p path)))
          (let ((tuatara-user:*metadata-look-up-key-owners* t))
            (true (tuatara.defaults::look-up-key-owners-p path))))
        ;; asked by M-x show-file-metadata, the answer y: a key not in the keyring said to be so
        (let ((path (write-test-octets-file (merge-pathnames "message.dat" dir)
                                            (octets-of (openpgp-test-packet 1 (octets-of 3 (be-octets #x0123456789ABCDEF 8) 1 0 8 #xAB))
                                                       (openpgp-test-packet 18 (octets-of 1 2 3)))))
              (tuatara-user:*metadata-look-up-key-owners* :ask))
          (let ((f (docx-session path (list (format nil "M-x ~a RET" (rimu-typed "show-file-metadata")) "y RET" ""))))
            (true (tuatara.frontend.fake:fake-frame-showing f "look up whose in your keyring")))
          (let ((shown (tuatara.buffer:find-buffer "*Metadata: message.dat*")))
            (true shown)
            (when shown
              (note-has (tuatara.buffer:buffer-string shown) "- Key 1 :: 0123456789ABCDEF, RSA: not in your keyring")
              (tuatara.buffer:kill-buffer shown)))
          (let ((visited (tuatara.buffer:find-buffer "message.dat")))
            (when visited (tuatara.buffer:kill-buffer visited)))))))

(define-test (metadata part-failure-reported)
  ;; a reader that breaks gives a section saying what could not be read, and the failure is in
  ;; the debug log while debug mode is on
  (drain-debug-reports)
  (unwind-protect
       (progn
         (tuatara.runtime:set-debug t)
         (let ((sections (tuatara.defaults::metadata-part (error "a reader broke"))))
           (true (search "could not be read" (first sections)) "~s" sections)
           (true (search "a reader broke" (first sections)) "~s" sections))
         (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                            while report collect (princ-to-string (fourth report)))))
           (true (find-if (lambda (text) (search "reading what a file says of itself failed: a reader broke" text))
                          texts)
                 "~s" texts)))
    (drain-debug-reports)))
