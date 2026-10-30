;;;; src/text/strings.lisp --- Helpers for strings and octets that are not the rope's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

(defun octets-latin-1 (octets &key (start 0) end)
  "OCTETS from START below END (or their end) as Latin-1 text: each octet the
character of its code, as ASCII and ISO 8859-1 read it."
  (map 'string #'code-char (subseq octets start end)))

(defun utf-16-text (octets &key (start 0) (end (length octets)) big-endian)
  "OCTETS from START below END as UTF-16 text, least significant octet first
unless BIG-ENDIAN or a byte-order mark says otherwise; the mark, a NUL at its
end and what follows left out."
  (let* ((end (- end (mod (- end start) 2)))
         (big (cond ((and (< (1+ start) end) (= (aref octets start) #xFE) (= (aref octets (1+ start)) #xFF)) (incf start 2) t)
                    ((and (< (1+ start) end) (= (aref octets start) #xFF) (= (aref octets (1+ start)) #xFE)) (incf start 2) nil)
                    (t big-endian)))
         (text (or (ignore-errors (sb-ext:octets-to-string (coerce octets '(simple-array (unsigned-byte 8) (*)))
                                                          :start start :end end
                                                          :external-format (list (if big :utf-16be :utf-16le) :replacement #\?)))
                   "")))
    (subseq text 0 (or (position (code-char 0) text) (length text)))))

(defparameter *charset-aliases*
  '(("us-ascii" . :latin-1) ("ascii" . :latin-1) ("ansi_x3.4-1968" . :latin-1) ("utf8" . :utf-8) ("latin1" . :latin-1)
    ("iso-8859-8-i" . :iso-8859-8) ("gb2312" . :gbk) ("x-gbk" . :gbk) ("cp936" . :gbk) ("cp932" . :shift_jis)
    ("windows-31j" . :shift_jis) ("x-sjis" . :shift_jis) ("unicode" . :utf-16le) ("utf-16" . :utf-16le)
    ("unicode-1-1-utf-7" . nil) ("utf-7" . nil))
  "Charsets that mail and web pages name otherwise than the system's
encodings are named, each (NAME . ENCODING), ENCODING NIL for one read as
UTF-8.")

(defun charset-encoding (charset)
  "The encoding the charset named CHARSET (as mail and web pages name one:
utf-8, iso-8859-1, windows-1252, shift_jis) stands for, a keyword the system
decodes; NIL for UTF-8 or a charset not known."
  (when (and charset (plusp (length charset)) (< (length charset) 40))
    (let* ((name (string-downcase (string-trim " \"'" charset)))
           (alias (assoc name *charset-aliases* :test #'string=))
           (encoding (if alias (cdr alias) (find-symbol (string-upcase name) :keyword))))
      (and encoding (not (eq encoding :utf-8))
           (ignore-errors (sb-ext:octets-to-string (make-array 0 :element-type '(unsigned-byte 8)) :external-format encoding))
           encoding))))

(defun charset-text (octets charset &key (start 0) (end (length octets)))
  "OCTETS from START below END as text in the charset named CHARSET
\(CHARSET-ENCODING): an octet the charset has no character for as U+FFFD; in
UTF-8, or a charset not known, as OCTETS-STRING reads them, an octet that is
not UTF-8 kept as its raw byte."
  (let ((octets (coerce octets '(simple-array (unsigned-byte 8) (*))))
        (encoding (charset-encoding charset)))
    (or (and encoding
             (ignore-errors (sb-ext:octets-to-string octets :start start :end end
                                                            :external-format (list encoding :replacement (code-char #xFFFD)))))
        (octets-string octets :start start :end end))))

(defun word-char-p (char)
  "True for characters that make up words: letters and digits."
  (and char (or (alpha-char-p char) (digit-char-p char))))

(defun name-char-p (char)
  "True for characters that make up a name, a word with _ in it as well:
letters, digits and _."
  (or (word-char-p char) (eql char #\_)))

(defun split-words (string &optional (separators '(#\Space #\Tab)))
  "The parts of STRING between SEPARATORS (characters, or a string of them),
the empty ones left out; NIL for NIL."
  (and string (remove "" (uiop:split-string string :separator separators) :test #'string=)))

(defun parse-decimal (text &key (most-digits 12))
  "TEXT, digits with perhaps a point and more digits after (as 595.276), as
an exact number; NIL when it is anything else or has more than MOST-DIGITS
digits. Only the digits and the point are read, never anything else."
  (let ((point (position #\. text)))
    (and (plusp (length text))
         (<= (length text) (1+ most-digits))
         (every (lambda (c) (or (digit-char-p c) (char= c #\.))) text)
         (= (count #\. text) (if point 1 0))
         (let ((whole (subseq text 0 (or point (length text))))
               (part (if point (subseq text (1+ point)) "")))
           (and (plusp (+ (length whole) (length part)))
                (+ (if (plusp (length whole)) (parse-integer whole) 0)
                   (if (plusp (length part)) (/ (parse-integer part) (expt 10 (length part))) 0)))))))

(defparameter *hex-pairs*
  (let ((pairs (make-array 256)))
    (dotimes (i 256 pairs)
      (setf (aref pairs i) (coerce (format nil "~(~2,'0x~)" i) 'simple-base-string))))
  "The two lower-case hex digits of each octet, by its value.")

(defun octets-hex (octets &key (start 0) end upper-case separator)
  "OCTETS from START below END (or their end) written in hexadecimal, two
digits each, in lower case unless UPPER-CASE, SEPARATOR (a string) between
each two when given."
  (let* ((end (or end (length octets)))
         (count (max 0 (- end start)))
         (gap (length (or separator "")))
         (out (make-string (max 0 (- (* count (+ 2 gap)) gap))))
         (at 0))
    (declare (type fixnum end count gap at))
    (loop for i from start below end
          do (when (and separator (> i start))
               (replace out separator :start1 at)
               (incf at gap))
             (let ((pair (svref *hex-pairs* (aref octets i))))
               (setf (char out at) (if upper-case (char-upcase (char pair 0)) (char pair 0))
                     (char out (1+ at)) (if upper-case (char-upcase (char pair 1)) (char pair 1)))
               (incf at 2)))
    out))

;;; Hashes of text

(defconstant +fnv-offset+ #xcbf29ce484222325 "FNV-1a's starting value for 64-bit hashes.")

(defconstant +fnv-prime+ 1099511628211 "FNV-1a's multiplier for 64-bit hashes.")

(declaim (inline octets-hash))
(defun octets-hash (octets &key (start 0) (end (length octets)) (hash +fnv-offset+))
  "The 64-bit FNV-1a hash of OCTETS from START below END, carried on from HASH:
the hash of a text read in pieces is that of each piece in turn, each begun
from the hash of the pieces before it."
  (declare (type octets octets) (type fixnum start end) (type (unsigned-byte 64) hash)
           (optimize speed))
  (loop for i of-type fixnum from start below end
        do (setf hash (ldb (byte 64 0) (* (logxor hash (aref octets i)) +fnv-prime+))))
  hash)

(defun string-hash (text)
  "A hash of the string TEXT: FNV-1a over its octets in UTF-8, as a fixnum,
the same in every version of Tuatara and of the Lisp it runs on."
  (ldb (byte 62 0) (octets-hash (string-octets text))))

;;; Text on one line

(defun one-line (text)
  "TEXT on one line: each run of line ends, tabs and other control
characters in it one space, and the spaces at its ends left out, so that it
can be shown or written where a line is expected (a name, a value, a reason
given by a program). NIL for NIL."
  (and text
       (let ((out (make-string-output-stream)) (blank nil))
         (loop for c across text
               do (if (control-code-p (char-code c))
                      (setf blank t)
                      (progn (when blank (write-char #\Space out) (setf blank nil))
                             (write-char c out))))
         (string-trim " " (get-output-stream-string out)))))

;;; Lines of a string

(defun string-line-start (text position)
  "Where the line of the string TEXT holding POSITION begins: just after the
newline before POSITION, or 0."
  (let ((newline (and (plusp position) (position #\Newline text :end (min position (length text)) :from-end t))))
    (if newline (1+ newline) 0)))

(defun string-line-end (text position)
  "Where the line of the string TEXT holding POSITION ends: the newline at or
after POSITION, or the end of TEXT."
  (or (position #\Newline text :start (min position (length text))) (length text)))

;;; Ranges of octets

(defun merge-ranges (ranges &key (touching t))
  "RANGES, each (START . END), in any order, put in order of their starts, with
those that overlap made one, and with TOUCHING (the default) those that touch
too, one ending where the next starts. RANGES itself is left as it was."
  (let ((merged '()))
    (dolist (range (sort (copy-list ranges) #'< :key #'car) (nreverse merged))
      (if (and merged (if touching
                          (<= (car range) (cdr (first merged)))
                          (< (car range) (cdr (first merged)))))
          (setf (cdr (first merged)) (max (cdr (first merged)) (cdr range)))
          (push (cons (car range) (cdr range)) merged)))))

(defparameter *number-width-most* 1000
  "The widest a number may be padded to by NUMBER-FORMATTER's %d.")

(defun number-formatter (control)
  "A function of a number giving the text CONTROL makes of it: CONTROL's
characters as they are, %d (or %s) standing for the number, with - before
the width to put it on the left, 0 to pad with zeros and a width (%3d, %-3d,
%03d), and %% for a percent sign. An error for any other %, and for a width
over *NUMBER-WIDTH-MOST*."
  (let ((parts '()) (i 0) (n (length control)))
    (loop while (< i n)
          do (let ((c (char control i)))
               (if (char/= c #\%)
                   (progn (push (string c) parts) (incf i))
                   (let ((j (1+ i)) (left nil) (zeros nil))
                     (loop while (and (< j n) (find (char control j) "-0"))
                           do (if (char= (char control j) #\-) (setf left t) (setf zeros t))
                              (incf j))
                     (let ((width-end (or (position-if-not #'digit-char-p control :start j) n)))
                       (cond ((and (= j (1+ i)) (< j n) (char= (char control j) #\%))
                              (push "%" parts) (setf i (1+ j)))
                             ((and (< width-end n) (find (char control width-end) "ds") (> width-end j)
                                   (or (> (- width-end j) 4)
                                       (> (parse-integer control :start j :end width-end) *number-width-most*)))
                              (error "Format string: a width of more than ~:d is not taken" *number-width-most*))
                             ((and (< width-end n) (find (char control width-end) "ds"))
                              (push (list (if (> width-end j) (parse-integer control :start j :end width-end) 0) left zeros)
                                    parts)
                              (setf i (1+ width-end)))
                             (t (error "Format string: ~a is not understood; %d stands for the number"
                                       (subseq control i (min n (1+ width-end)))))))))))
    (setf parts (nreverse parts))
    (lambda (number)
      (with-output-to-string (out)
        (dolist (part parts)
          (if (stringp part)
              (write-string part out)
              (destructuring-bind (width left zeros) part
                (let* ((digits (princ-to-string number))
                       (pad (max 0 (- width (length digits)))))
                  (cond (left (write-string digits out) (write-string (make-string pad :initial-element #\Space) out))
                        (zeros (when (minusp number) (write-char #\- out) (setf digits (subseq digits 1)))
                               (write-string (make-string pad :initial-element #\0) out)
                               (write-string digits out))
                        (t (write-string (make-string pad :initial-element #\Space) out) (write-string digits out)))))))))))

(defun natural-string-lessp (a b)
  "True when the string A comes before B in the order people sort names in:
letters alphabetically whatever their case, and a run of digits by the number
it writes: file2 before file10, Notes before notes.txt; two
names alike but for case, or for zeros before a number, in the order their
characters' codes give. Complexity: O(length of the shorter), nothing made."
  (declare (type string a b) (optimize speed))
  (let ((la (length a)) (lb (length b)) (i 0) (j 0))
    (declare (type fixnum la lb i j))
    (flet ((digit-p (c) (char<= #\0 c #\9)))
      (declare (inline digit-p))
      (loop
        (cond ((>= i la) (return (if (< j lb) t (and (string< a b) t))))
              ((>= j lb) (return nil)))
        (let ((x (char a i)) (y (char b j)))
          (if (and (digit-p x) (digit-p y))
              ;; a number each: the one with fewer digits, its leading zeros left out, the smaller
              (let ((sa i) (sb j) (ea i) (eb j))
                (declare (type fixnum sa sb ea eb))
                (loop while (and (< sa la) (char= #\0 (char a sa))) do (incf sa))
                (loop while (and (< sb lb) (char= #\0 (char b sb))) do (incf sb))
                (setf ea sa eb sb)
                (loop while (and (< ea la) (digit-p (char a ea))) do (incf ea))
                (loop while (and (< eb lb) (digit-p (char b eb))) do (incf eb))
                (unless (= (- ea sa) (- eb sb))
                  (return (< (- ea sa) (- eb sb))))
                (loop for p of-type fixnum from sa below ea
                      for q of-type fixnum from sb
                      do (let ((d (char a p)) (e (char b q)))
                           (unless (char= d e) (return-from natural-string-lessp (char< d e)))))
                (setf i ea j eb))
              (let ((fx (char-downcase x)) (fy (char-downcase y)))
                (cond ((char< fx fy) (return t))
                      ((char> fx fy) (return nil))
                      (t (incf i) (incf j))))))))))
