;;;; lisp/viewers/inner-pictures.lisp --- The pictures a picture file keeps besides the one shown.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A picture file may keep more pictures than the one a viewer shows: a
;;;; TIFF's further pages (a scanner's other sheets, GIMP's layers, a smaller
;;;; copy, a mask). Their structure is read here, by Tuatara alone, as the
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

(define-record (inner-picture (:constructor make-inner-picture (label place width height source)) (:copier nil))
  "One picture a file keeps: LABEL, what it is in words (page 2 of 3, a
smaller copy, its name); PLACE, where it lies in the file in words; its
WIDTH and HEIGHT in pixels (NIL when not said); SOURCE, how it is drawn,
\(:FRAME N), the file's own picture N, from 0."
  (label nil :documentation "What INNER-PICTURE is, in words.")
  (place nil :documentation "Where INNER-PICTURE lies in its file, in words.")
  (width nil :documentation "The width of INNER-PICTURE in pixels, or NIL.")
  (height nil :documentation "The height of INNER-PICTURE in pixels, or NIL.")
  (source nil :documentation "How INNER-PICTURE is drawn: (:FRAME N), its file's picture N from 0."))

;;; TIFF: a chain of directories, one a picture

(defun tiff-inner-pictures (fd head size)
  "The pictures of the TIFF open as file descriptor (FD), whose first octets
are HEAD, of SIZE octets: one for each directory of its chain, in order, at
most *INNER-PICTURES-MOST*, each directory read where it stands and visited
once."
  (let ((big (= (aref head 0) 77)) (seen '()) (pictures '()))
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
                 (and x y (if (and x-dots y-dots) (list (round (* x x-dots)) (round (* y y-dots))) (list (round x) (round y))))))
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
                 (if (plusp (length name)) (format nil "~a \"~a\"" what name) what))))
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
                                               (list :frame index))
                           pictures))
                   (setf at next)))))
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
                           (:tiff (and (tuatara.io:tiff-octets-p head) (tiff-inner-pictures fd head size))))))))
    (and (rest pictures) pictures)))

;;; Drawing and comparing

(defun inner-picture-image (fd kind picture width height &key cancel enlarge)
  "PICTURE, one of the file open as file descriptor (FD), of KIND, drawn in
the sandbox no larger than WIDTH by HEIGHT pixels (with ENLARGE, as large as
fits): two values, as DECODE-IMAGE gives them."
  (destructuring-bind (how n) (inner-picture-source picture)
    (ecase how
      (:frame (decode-image fd kind width height :cancel cancel :enlarge enlarge :frame n)))))

(defun images-difference (a b)
  "The share, from 0 to 1, of the pixels of the pictures A and B whose colour
differs by more than *INNER-PICTURE-DIFFERENCE-STEP* in red, green, blue or
\(both having it) alpha; NIL when their sizes differ by more than a pixel,
which comes of other proportions. A pixel more one way is a rounding: the
pictures are compared over the part both have."
  (let ((width (min (tuatara.frontend:image-width a) (tuatara.frontend:image-width b)))
        (height (min (tuatara.frontend:image-height a) (tuatara.frontend:image-height b))))
    (when (and (<= (abs (- (tuatara.frontend:image-width a) (tuatara.frontend:image-width b))) 1)
               (<= (abs (- (tuatara.frontend:image-height a) (tuatara.frontend:image-height b))) 1)
               (plusp width) (plusp height))
      (let* ((pa (tuatara.frontend:image-pixels a)) (pb (tuatara.frontend:image-pixels b))
             (ca (tuatara.frontend:image-channels a)) (cb (tuatara.frontend:image-channels b))
             (wa (tuatara.frontend:image-width a)) (wb (tuatara.frontend:image-width b))
             (channels (min ca cb))
             (step *inner-picture-difference-step*)
             (differing 0))
        (dotimes (y height)
          (dotimes (x width)
            (let ((ia (* ca (+ x (* y wa)))) (ib (* cb (+ x (* y wb)))))
              (when (loop for c below channels
                          thereis (> (abs (- (aref pa (+ ia c)) (aref pb (+ ib c)))) step))
                (incf differing)))))
        (/ differing (* width height))))))

(defun difference-words (share)
  "SHARE, of the pixels of a picture differing from the one shown (from 0 to
1), in words."
  (cond ((zerop share) "looks the same as the picture shown")
        ((< share 1/100) "differs from the picture shown in less than 1% of it")
        (t (format nil "differs from the picture shown in ~d% of it" (round (* 100 share))))))

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
                        (t (multiple-value-bind (image notes)
                               (inner-picture-image fd kind picture size size :cancel cancel :enlarge t)
                             (let ((share (and image (images-difference shown image))))
                               (cond ((null image)
                                      (format nil "not compared: it could not be drawn~@[ (~a)~]"
                                              (and notes (string-left-trim ", " (car (first notes))))))
                                     ((null share) "not compared: of other proportions than the picture shown")
                                     (t (difference-words share))))))))))

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
  (let* ((head (and (member kind '(:tiff)) (tuatara.io:read-fd-start fd path size 4096)))
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
                      do (format s "- Picture ~d :: ~a ~a~%" n
                                 (metadata-value (format nil "~a; ~a; ~a" (inner-picture-words picture)
                                                         (inner-picture-place picture) words))
                                 (org-link-text (format nil "file:~a::picture ~d" path n) "show it")))
                (terpri s)))))))

(defun inner-picture-search-index (search count)
  "The index from 0 of the picture SEARCH names, the part after :: of a link
to a file (picture 2 names the second), when it is among COUNT pictures; NIL
otherwise."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^picture (\\d{1,6})$" search)
    (let ((n (and match (parse-integer (aref groups 0)))))
      (and n (<= 1 n count) (1- n)))))
