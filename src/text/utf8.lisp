;;;; src/text/utf8.lisp --- Octets, and UTF-8 read and written strictly, every octet kept.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

;;; Octets

(deftype octets ()
  "A simple vector of octets: the storage type of all text in Tuatara (UTF-8)."
  '(simple-array (unsigned-byte 8) (*)))

(deftype fixvec ()
  "A simple vector of fixnums, used for batches of positions."
  '(simple-array fixnum (*)))

(declaim (inline continuation-byte-p make-octets))

(defun continuation-byte-p (octet)
  "True when OCTET is a UTF-8 continuation byte (10xxxxxx), i.e. not the start of a character."
  (declare (type (unsigned-byte 8) octet))
  (= (logand octet #xC0) #x80))

(defun make-octets (length)
  "Return a fresh octet vector of LENGTH."
  (declare (type fixnum length))
  (make-array length :element-type '(unsigned-byte 8)))

(defconstant +block-slack+ 256
  "Octets a kept block has past its size, for a needle or a character read on past it.")

(sb-ext:defglobal **octet-blocks**
    (map 'simple-vector
         (lambda (size)
           (tuatara.runtime:make-spares (lambda () (make-octets (+ size +block-slack+)))))
         '(4096 65536 1048576))
  "Blocks of octets text is read into, kept to be read into again: of 4 KB,
64 KB and a megabyte, each with +BLOCK-SLACK+ octets more (WITH-OCTET-BLOCK).")

(defun octet-block-spares (size)
  "The kept blocks of the least size holding SIZE octets, or NIL when none is that large."
  (declare (type fixnum size))
  (cond ((<= size (+ 4096 +block-slack+)) (svref **octet-blocks** 0))
        ((<= size (+ 65536 +block-slack+)) (svref **octet-blocks** 1))
        ((<= size (+ 1048576 +block-slack+)) (svref **octet-blocks** 2))
        (t nil)))

(defmacro with-octet-block ((var size) &body body)
  "Run BODY with VAR bound to octets at least SIZE long: a block kept for
reading text into (**OCTET-BLOCKS**), given back after however BODY ends, or
for a size larger than a megabyte, new octets. BODY must not keep it."
  (let ((spares (gensym "SPARES")) (body-function (gensym "BODY")) (n (gensym "SIZE")))
    `(flet ((,body-function (,var)
              (declare (type octets ,var))
              ,@body))
       (declare (dynamic-extent #',body-function))
       (let* ((,n ,size) (,spares (octet-block-spares ,n)))
         (if ,spares
             (tuatara.runtime:with-spare (,var ,spares) (,body-function ,var))
             (,body-function (make-octets ,n)))))))

;;; UTF-8, strictly
;;;
;;; Every sequence of octets is text. A well-formed UTF-8 sequence (Unicode's
;;; table 3-7: no overlong forms, no surrogates, nothing past U+10FFFF) is one
;;; character; every other octet is a character of its own, a raw byte, which
;;; decodes to U+DC80..U+DCFF (the octet plus #xDC00, as Python's
;;; surrogateescape has it) and encodes back to that octet, so that a file of
;;; invalid UTF-8 is shown, edited and saved without losing an octet. Whether an
;;; octet starts a character depends on at most three octets before it and
;;; three after; every leaf and span starts where a character starts.

(defconstant +raw-byte-base+ #xDC00
  "A raw byte B decodes to the character with code B plus this.")

(declaim (inline raw-byte-code-p))
(defun raw-byte-code-p (code)
  "True when CODE is the code of a raw byte's character, U+DC80..U+DCFF."
  (<= #xDC80 code #xDCFF))

(declaim (inline control-code-p))
(defun control-code-p (code)
  "True when CODE is a control character's: 0 to 31, 127 to 159."
  (or (< code 32) (<= 127 code 159)))

;;; Line ends
;;;
;;; A line ends at a newline (10), or at a carriage return (13) that no
;;; newline follows; a carriage return and the newline after it end one line,
;;; at the newline. LINE-END-OCTET-P says so for one octet, and everything that
;;; counts or finds lines asks it, or WORD-LINE-ENDS-FORM for eight at once.

(declaim (inline line-end-octet-p))
(defun line-end-octet-p (octet next)
  "True when OCTET ends a line, NEXT being the octet after it, or NIL at the
end of the text: a newline does, and so does a carriage return that no
newline follows."
  (declare (type (unsigned-byte 8) octet))
  (or (= octet 10) (and (= octet 13) (not (eql next 10)))))

(declaim (inline octets-line-end-p))
(defun octets-line-end-p (octets i avail)
  "True when the octet at index I of OCTETS, whose text ends at AVAIL, ends a
line (LINE-END-OCTET-P); a carriage return at AVAIL - 1 is taken to end one."
  (declare (type octets octets) (type fixnum i avail))
  (line-end-octet-p (aref octets i) (and (< (1+ i) avail) (aref octets (1+ i)))))

(defun octets-line-end-position (octets start end &optional (avail end))
  "The index of the first octet of OCTETS from START below END that ends a
line (OCTETS-LINE-END-P, the text ending at AVAIL), or NIL. AVAIL NIL says
the text goes on past END unread: a carriage return at END - 1, whose next
octet is not known, is then taken for none. Complexity: O(END - START)."
  (declare (type octets octets) (type fixnum start end) (type (or null fixnum) avail) (optimize speed))
  (loop for i of-type fixnum from start below end
        do (let ((b (aref octets i)))
             (when (or (= b 10)
                       (and (= b 13)
                            (if (< (1+ i) end)
                                (/= 10 (aref octets (1+ i)))
                                (and avail (octets-line-end-p octets i avail)))))
               (return i)))))

(defun octets-line-end-position-backward (octets start end &optional (avail end))
  "The index of the last octet of OCTETS from START below END that ends a
line (OCTETS-LINE-END-P, the text ending at AVAIL), or NIL. AVAIL NIL says
the text goes on past END unread, as OCTETS-LINE-END-POSITION has it.
Complexity: O(END - START)."
  (declare (type octets octets) (type fixnum start end) (type (or null fixnum) avail) (optimize speed))
  (loop for i of-type fixnum from (1- end) downto start
        do (let ((b (aref octets i)))
             (when (or (= b 10)
                       (and (= b 13)
                            (if (< (1+ i) end)
                                (/= 10 (aref octets (1+ i)))
                                (and avail (octets-line-end-p octets i avail)))))
               (return i)))))

(declaim (inline line-text-ends-p))
(defun line-text-ends-p (octets i avail crlf)
  "True when the text of a line ends before the octet at index I of OCTETS,
whose text ends at AVAIL: a newline or a carriage return that ends the line
\(OCTETS-LINE-END-P), or with CRLF a carriage return before a newline."
  (declare (type octets octets) (type fixnum i avail))
  (let ((b (aref octets i)))
    (or (= b 10) (and (= b 13) (or crlf (octets-line-end-p octets i avail))))))

(declaim (inline crlf-octets-p))
(defun crlf-octets-p (octets i avail)
  "True when OCTETS hold a carriage return at index I and a newline after it,
below AVAIL: a line end of two octets."
  (declare (type octets octets) (type fixnum i avail))
  (and (= (aref octets i) 13) (< (1+ i) avail) (= (aref octets (1+ i)) 10)))

(declaim (inline line-text-end))
(defun line-text-end (octets start newline crlf)
  "Where the text of a line ends in OCTETS, whose newline is at index NEWLINE
and which starts at index START or before: with CRLF, at the carriage return
just before the newline, when there is one after START; else at the newline."
  (declare (type octets octets) (type fixnum start newline))
  (if (and crlf (> newline start) (= (aref octets (1- newline)) 13)) (1- newline) newline))

(sb-ext:defglobal **utf8-classes**
    (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
      (dotimes (b 256 table)
        (setf (aref table b)
              (cond ((< b #x80) 0) ((< b #x90) 1) ((< b #xA0) 2) ((< b #xC0) 3) ((< b #xC2) 4)
                    ((< b #xE0) 5) ((= b #xE0) 6) ((= b #xED) 8) ((< b #xF0) 7)
                    ((= b #xF0) 9) ((< b #xF4) 10) ((= b #xF4) 11) (t 4)))))
  "The class of each octet in decoding UTF-8: 0 ASCII; 1, 2 and 3 continuation
octets 80-8F, 90-9F and A0-BF; 4 never valid (C0, C1, F5-FF); 5 the leads of two
octets; 6 E0; 7 the other leads of three; 8 ED; 9 F0; 10 F1-F3; 11 F4.")
(declaim (type (simple-array (unsigned-byte 8) (256)) **utf8-classes**))

(sb-ext:defglobal **utf8-next**
    (let ((table (make-array 96 :element-type '(unsigned-byte 8) :initial-element 8)))
      (flet ((to (state classes next)
               (dolist (class classes) (setf (aref table (+ (* state 12) class)) next))))
        ;; at a character start: ASCII, a raw byte, or a lead
        (to 0 '(0) 0) (to 0 '(1 2 3 4) 9)
        (to 0 '(5) 1) (to 0 '(6) 4) (to 0 '(7) 2) (to 0 '(8) 5) (to 0 '(9) 6) (to 0 '(10) 3) (to 0 '(11) 7)
        ;; one, two or three more continuation octets of any value
        (to 1 '(1 2 3) 0) (to 2 '(1 2 3) 1) (to 3 '(1 2 3) 2)
        ;; after E0, ED, F0 and F4 the next octet has a narrower range
        (to 4 '(3) 1) (to 5 '(1 2) 1) (to 6 '(2 3) 2) (to 7 '(1) 2))
      table)
  "The UTF-8 decoder's transitions, at (+ (* STATE 12) CLASS): 0 a character is
complete; 1 to 7 more continuation octets are needed; 8 the sequence begun is
not well formed (its octets are raw bytes, and this one starts afresh); 9 this
octet, at a character start, is a raw byte.")
(declaim (type (simple-array (unsigned-byte 8) (96)) **utf8-next**))

(defmacro utf8-decode-form (byte-at i end)
  "A form giving the code of the character at I and its length in octets, the
octets read by (BYTE-AT index), none at or after END: a raw byte's character and
length 1 for an octet that starts no well-formed sequence."
  (let ((b0 (gensym "B0")) (state (gensym "STATE")) (code (gensym "CODE")) (k (gensym "K"))
        (p (gensym "P")) (b (gensym "B")) (next (gensym "NEXT")))
    `(let* ((,b0 (,byte-at ,i))
            (,state (aref **utf8-next** (aref **utf8-classes** ,b0))))
       (declare (type (unsigned-byte 8) ,b0 ,state))
       (cond ((zerop ,state) (values ,b0 1))
             ((= ,state 9) (values (+ +raw-byte-base+ ,b0) 1))
             (t (let ((,code (logand ,b0 (case ,state (1 #x1F) ((2 4 5) #x0F) (t #x07))))
                      (,k 1))
                  (declare (type fixnum ,code ,k))
                  (loop
                    (let ((,p (+ ,i ,k)))
                      (declare (type fixnum ,p))
                      (when (>= ,p ,end) (return (values (+ +raw-byte-base+ ,b0) 1)))
                      (let* ((,b (,byte-at ,p))
                             (,next (aref **utf8-next** (+ (* ,state 12) (aref **utf8-classes** ,b)))))
                        (declare (type (unsigned-byte 8) ,b ,next))
                        (when (= ,next 8) (return (values (+ +raw-byte-base+ ,b0) 1)))
                        (setf ,code (logior (ash ,code 6) (logand ,b #x3F)))
                        (incf ,k)
                        (when (zerop ,next) (return (values ,code ,k)))
                        (setf ,state ,next))))))))))

(defmacro utf8-char-start-form (byte-at i start end)
  "A form, true when the octet at I starts a character of the text running from
START below END, its octets read by (BYTE-AT index): any octet but a
continuation octet does; a continuation octet does unless a well-formed
sequence begun at most three octets before it takes it in."
  (let ((k (gensym "K")) (j (gensym "J")) (bj (gensym "BJ")))
    `(if (/= (logand (,byte-at ,i) #xC0) #x80)
         t
         (loop for ,k of-type fixnum from 1 to 3
               for ,j of-type fixnum = (- ,i ,k)
               do (when (< ,j ,start) (return t))
                  (let ((,bj (,byte-at ,j)))
                    (declare (type (unsigned-byte 8) ,bj))
                    (when (/= (logand ,bj #xC0) #x80)
                      (return (<= (the fixnum (nth-value 1 (utf8-decode-form ,byte-at ,j ,end))) ,k))))
               finally (return t)))))

(defmacro utf8-pending-form (byte-at start end)
  "A form giving how many octets at the end of the text from START below END
begin a well-formed sequence that is not finished there, which octets following
them could finish: 0, 1, 2 or 3."
  (let ((k (gensym "K")) (j (gensym "J")) (state (gensym "STATE")) (p (gensym "P")))
    `(loop for ,k of-type fixnum from 1 to 3
           for ,j of-type fixnum = (- ,end ,k)
           do (when (< ,j ,start) (return 0))
              (when (/= (logand (,byte-at ,j) #xC0) #x80)
                (let ((,state (aref **utf8-next** (aref **utf8-classes** (,byte-at ,j)))))
                  (declare (type (unsigned-byte 8) ,state))
                  (when (or (zerop ,state) (= ,state 9)) (return 0))
                  (loop for ,p of-type fixnum from (1+ ,j) below ,end
                        do (setf ,state (aref **utf8-next** (+ (* ,state 12) (aref **utf8-classes** (,byte-at ,p)))))
                           (when (or (zerop ,state) (= ,state 8)) (return-from nil)))
                  (return (if (or (zerop ,state) (= ,state 8)) 0 ,k))))
           finally (return 0))))

(defun octets-decode (octets i end)
  "The code of the character at index I of OCTETS, whose text ends at END, and
its length in octets (see this section's header: an octet that starts no
well-formed sequence is a raw byte of length 1)."
  (declare (type octets octets) (type fixnum i end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(aref octets ,p)))
    (utf8-decode-form byte-at i end)))

(defun octets-char-start-p (octets i start end)
  "True when index I of OCTETS, whose text runs from START below END, starts a character."
  (declare (type octets octets) (type fixnum i start end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(aref octets ,p)))
    (utf8-char-start-form byte-at i start end)))

(defun octets-pending (octets start end)
  "How many octets at the end of OCTETS' text from START below END begin a
sequence that following octets could finish (UTF8-PENDING-FORM)."
  (declare (type octets octets) (type fixnum start end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(aref octets ,p)))
    (utf8-pending-form byte-at start end)))

(defun sap-char-start-p (sap i start end)
  "True when offset I of the memory at SAP, whose text runs from START below END, starts a character."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum i start end) (optimize speed (safety 0)))
  (macrolet ((byte-at (p) `(sb-sys:sap-ref-8 sap ,p)))
    (utf8-char-start-form byte-at i start end)))

(defun octets-cut-p (octets i start end)
  "True when the text of OCTETS from START below END may be cut into pieces
before index I, above START: a character starts there, and a carriage return
is not parted from the newline after it, so that each piece counts its own
line ends (LINE-END-OCTET-P)."
  (declare (type octets octets) (type fixnum i start end))
  (and (octets-char-start-p octets i start end)
       (not (and (= (aref octets i) 10) (= (aref octets (1- i)) 13)))))

(defun sap-cut-p (sap i start end)
  "True when the text at SAP from START below END may be cut into pieces
before offset I, above START (OCTETS-CUT-P)."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum i start end))
  (and (sap-char-start-p sap i start end)
       (not (and (= (sb-sys:sap-ref-8 sap i) 10) (= (sb-sys:sap-ref-8 sap (1- i)) 13)))))

(defconstant +ascii-mask+ #x8080808080808080 "The top bit of each octet of a word.")
(defconstant +low-seven+ #x7F7F7F7F7F7F7F7F "The other seven bits of each octet of a word.")
(defconstant +newlines+ #x0A0A0A0A0A0A0A0A "A word of eight newline octets.")
(defconstant +returns+ #x0D0D0D0D0D0D0D0D "A word of eight carriage return octets.")
(defconstant +last-octet-top+ #x8000000000000000 "The top bit of the last octet of a word.")

(declaim (inline word-octets-of))
(defun word-octets-of (word pattern)
  "The top bit of each octet of WORD that equals the one in its place in
PATTERN, the other bits clear."
  (declare (type (unsigned-byte 64) word pattern) (optimize speed (safety 0)))
  (let* ((x (logxor word pattern))
         (nonzero (logior (logand (+ (logand x +low-seven+) +low-seven+) #xFFFFFFFFFFFFFFFF) x)))
    (declare (type (unsigned-byte 64) x nonzero))
    (logandc1 nonzero +ascii-mask+)))

(declaim (inline word-holds-p))
(defun word-holds-p (word pattern)
  "True when one of the eight octets of WORD equals the one in its place in
PATTERN: a quicker test than WORD-OCTETS-OF, which says which."
  (declare (type (unsigned-byte 64) word pattern) (optimize speed (safety 0)))
  (let ((x (logxor word pattern)))
    (declare (type (unsigned-byte 64) x))
    (not (zerop (logand (logandc2 (logand (- x #x0101010101010101) #xFFFFFFFFFFFFFFFF) x) +ascii-mask+)))))

(defmacro word-line-ends-form (word next)
  "A form giving how many of the eight octets of WORD (read in memory order,
the first the lowest) end a line (LINE-END-OCTET-P), NEXT a form giving the
octet after them, or NIL at the end of the text: evaluated only when WORD
holds a carriage return."
  (let ((w (gensym "WORD")) (returns (gensym "RETURNS")) (newlines (gensym "NEWLINES")))
    `(let* ((,w ,word)
            (,newlines (word-octets-of ,w +newlines+)))
       (declare (type (unsigned-byte 64) ,w ,newlines))
       (if (not (word-holds-p ,w +returns+))
           (logcount ,newlines)
           ;; a carriage return ends a line unless a newline comes next
           (let ((,returns (word-octets-of ,w +returns+)))
             (declare (type (unsigned-byte 64) ,returns))
             (+ (logcount ,newlines)
                (logcount (logandc2 ,returns (logior (ash ,newlines -8)
                                                     (if (eql ,next 10) +last-octet-top+ 0))))))))))

(declaim (inline sap-holds-return-p))
(defun sap-holds-return-p (sap start end)
  "True when a carriage return is among the octets at SAP from offset START
below END, found by the C library's memchr: the line ends of text without one
are its newlines alone."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum start end))
  (and (< start end)
       (/= 0 (sb-sys:sap-int
              (sb-alien:alien-funcall
               (sb-alien:extern-alien "memchr" (function sb-sys:system-area-pointer sb-sys:system-area-pointer
                                                         sb-alien:int sb-alien:unsigned-long))
               (sb-sys:sap+ sap start) 13 (- end start))))))

(defun summarise-sap (sap start end)
  "Scan the octets at SAP from offset START, which must start a character,
below END, and return four values: characters (a raw byte is one), UTF-16 code
units (a raw byte is one), line ends (LINE-END-OCTET-P, a carriage return at
END - 1 ending one), and raw bytes. Words of eight ASCII octets go at once,
two at a time where no carriage return is. Complexity: O(END - START)."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum start end) (optimize speed (safety 0)))
  (let ((chars 0) (utf16 0) (lines 0) (invalid 0) (state 0) (pending 0) (i start)
        (classes **utf8-classes**) (next **utf8-next**)
        (returns (sap-holds-return-p sap start end)))
    (declare (type fixnum chars utf16 lines invalid state pending i)
             (type (simple-array (unsigned-byte 8) (256)) classes)
             (type (simple-array (unsigned-byte 8) (96)) next))
    (loop
      (when (zerop state)
        (if returns
            (loop while (<= (+ i 8) end)
                  do (let ((word (sb-sys:sap-ref-64 sap i)))
                       (declare (type (unsigned-byte 64) word))
                       (unless (zerop (logand word +ascii-mask+)) (return))
                       (incf lines (the fixnum (word-line-ends-form word (and (< (+ i 8) end) (sb-sys:sap-ref-8 sap (+ i 8))))))
                       (incf chars 8) (incf utf16 8) (incf i 8)))
            ;; no carriage return: the newlines of two words counted at once
            (progn
              (loop while (<= (+ i 16) end)
                    do (let ((a (sb-sys:sap-ref-64 sap i)) (b (sb-sys:sap-ref-64 sap (+ i 8))))
                         (declare (type (unsigned-byte 64) a b))
                         (unless (zerop (logand (logior a b) +ascii-mask+)) (return))
                         (incf lines (logcount (logior (word-octets-of a +newlines+)
                                                       (ash (word-octets-of b +newlines+) -1))))
                         (incf chars 16) (incf utf16 16) (incf i 16)))
              (loop while (<= (+ i 8) end)
                    do (let ((word (sb-sys:sap-ref-64 sap i)))
                         (declare (type (unsigned-byte 64) word))
                         (unless (zerop (logand word +ascii-mask+)) (return))
                         (incf lines (logcount (word-octets-of word +newlines+)))
                         (incf chars 8) (incf utf16 8) (incf i 8))))))
      (when (>= i end) (return))
      (let* ((b (sb-sys:sap-ref-8 sap i))
             (n (aref next (+ (* state 12) (aref classes b)))))
        (declare (type (unsigned-byte 8) b n))
        (case n
          (0 (incf chars) (incf utf16 (if (= pending 3) 2 1))
             (when (or (= b 10)
                       (and returns (= b 13) (line-end-octet-p b (and (< (1+ i) end) (sb-sys:sap-ref-8 sap (1+ i))))))
               (incf lines))
             (setf state 0 pending 0) (incf i))
          (9 (incf chars) (incf utf16) (incf invalid) (incf i))
          ;; the octets of the sequence begun are raw bytes; this octet starts afresh
          (8 (incf chars pending) (incf utf16 pending) (incf invalid pending)
             (setf state 0 pending 0))
          (t (setf state n) (incf pending) (incf i)))))
    (values (+ chars pending) (+ utf16 pending) lines (+ invalid pending))))

(defun newlines-sap (sap start end &optional (limit end))
  "Count the line ends (LINE-END-OCTET-P) at SAP from offset START below END,
eight octets at a time, the text going on to LIMIT: a carriage return at
END - 1 ends a line unless the octet at END, below LIMIT, is a newline. Line
ends are never part of another character in UTF-8. Complexity: O(END - START)."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum start end limit) (optimize speed (safety 0)))
  (let ((lines 0) (i start))
    (declare (type fixnum lines i))
    (if (sap-holds-return-p sap start end)
        (progn
          (loop while (<= (+ i 8) end)
                do (incf lines (the fixnum (word-line-ends-form (sb-sys:sap-ref-64 sap i)
                                                               (and (< (+ i 8) limit) (sb-sys:sap-ref-8 sap (+ i 8))))))
                   (incf i 8))
          (loop while (< i end)
                do (let ((b (sb-sys:sap-ref-8 sap i)))
                     (when (and (or (= b 10) (= b 13))
                                (line-end-octet-p b (and (< (1+ i) limit) (sb-sys:sap-ref-8 sap (1+ i)))))
                       (incf lines)))
                   (incf i)))
        (progn
          (loop while (<= (+ i 16) end)
                do (incf lines (logcount (logior (word-octets-of (sb-sys:sap-ref-64 sap i) +newlines+)
                                                 (ash (word-octets-of (sb-sys:sap-ref-64 sap (+ i 8)) +newlines+) -1))))
                   (incf i 16))
          (loop while (<= (+ i 8) end)
                do (incf lines (logcount (word-octets-of (sb-sys:sap-ref-64 sap i) +newlines+)))
                   (incf i 8))
          (loop while (< i end)
                do (when (= (sb-sys:sap-ref-8 sap i) 10) (incf lines))
                   (incf i))))
    lines))

(defun octets-line-ends (octets start end &optional (avail end))
  "How many line ends (LINE-END-OCTET-P) OCTETS holds from START below END,
the text ending at AVAIL (NEWLINES-SAP): a carriage return at END - 1 ends a
line unless the octet at END, below AVAIL, is a newline."
  (declare (type octets octets) (type fixnum start end avail))
  (sb-sys:with-pinned-objects (octets)
    (newlines-sap (sb-sys:vector-sap octets) start end avail)))

(defun summarise (octets start end)
  "SUMMARISE-SAP over the octet vector OCTETS from START below END."
  (declare (type octets octets) (type fixnum start end))
  (sb-sys:with-pinned-objects (octets)
    (summarise-sap (sb-sys:vector-sap octets) start end)))

(defun unfinished-sequence-length (octets i end)
  "How many octets at index I of OCTETS, whose text ends at END, make one
piece of what is not UTF-8: a sequence well begun and cut short, its octets
up to where it goes wrong; else the one octet at I."
  (declare (type octets octets) (type fixnum i end) (optimize speed (safety 0)))
  (let ((state (aref **utf8-next** (aref **utf8-classes** (aref octets i)))) (k 1))
    (declare (type (unsigned-byte 8) state) (type fixnum k))
    (if (or (zerop state) (= state 9))
        1
        (loop (when (>= (+ i k) end) (return k))
              (let ((next (aref **utf8-next** (+ (* state 12) (aref **utf8-classes** (aref octets (+ i k)))))))
                (declare (type (unsigned-byte 8) next))
                (when (= next 8) (return k))
                (incf k)
                (setf state next))))))

(defun octets-string (octets &key (start 0) (end (length octets)) replacement)
  "OCTETS from START below END as a string: well-formed UTF-8 decoded, every
other octet as its raw byte's character (U+DC80..U+DCFF); with REPLACEMENT, a
character, each piece of what is not UTF-8 as that character instead: a
sequence cut short as one, any other octet as one each."
  (declare (type octets octets) (type fixnum start end) (type (or null character) replacement))
  (let ((out (make-string (- end start))) (n 0) (i start))
    (declare (type fixnum n i))
    (loop while (< i end)
          do (let ((b (aref octets i)))
               (if (< b #x80)
                   (progn (setf (schar out n) (code-char b)) (incf n) (incf i))
                   (multiple-value-bind (code length) (octets-decode octets i end)
                     (cond ((not (and replacement (raw-byte-code-p code)))
                            (setf (schar out n) (code-char code)) (incf n) (incf i length))
                           (t (setf (schar out n) replacement) (incf n)
                              (incf i (unfinished-sequence-length octets i end))))))))
    (if (= n (length out)) out (subseq out 0 n))))

(defun lenient-string (octets &key (start 0) (end (length octets)) (replacement #\?))
  "OCTETS from START below END read as UTF-8 text, what is not UTF-8 as
REPLACEMENT, a character or string: a program's output, a part of a file read
to be shown."
  (if (and (typep octets 'octets) (characterp replacement))
      (octets-string octets :start start :end end :replacement replacement)
      (sb-ext:octets-to-string octets :start start :end end :external-format (list :utf-8 :replacement replacement))))

(declaim (inline char-octet-length))
(defun char-octet-length (char)
  "How many octets CHAR takes in the text: as STRING-OCTETS writes it, a raw
byte's character as its one octet, any other lone surrogate as U+FFFD's three."
  (let ((code (char-code char)))
    (cond ((< code #x80) 1)
          ((raw-byte-code-p code) 1)
          ((< code #x800) 2)
          ((< code #x10000) 3)
          (t 4))))

(defun string-octet-length (string &key (start 0) (end (length string)))
  "How many octets STRING from START below END takes in the text, counted
without making them (CHAR-OCTET-LENGTH)."
  (loop for i from start below end sum (char-octet-length (char string i))))

(defun put-string-octets (string out at &key (start 0) (end (length string)))
  "Put STRING from START below END into OUT, octets, from AT, as
STRING-OCTETS writes it, a larger copy of OUT made first when it has no room:
that vector and where the next octet goes."
  (declare (type string string) (type octets out) (type fixnum at start end) (optimize speed))
  (let ((room (+ at (* 4 (- end start)))))
    (declare (type fixnum room))
    (when (> room (length out))
      (let ((larger (make-array (max room (* 2 (length out))) :element-type '(unsigned-byte 8))))
        (replace larger out :end2 at)
        (setf out larger))))
  (macrolet ((put (octet) `(progn (setf (aref out at) ,octet) (incf at)))
             (each-code ((code) &body body)
               ;; the loop written out for the usual kind of string, so that its characters are read directly
               `(flet ((body (,code) (declare (type (integer 0 #x10FFFF) ,code)) ,@body))
                  (declare (inline body))
                  (if (typep string '(simple-array character (*)))
                      (loop for i of-type fixnum from start below end do (body (char-code (schar string i))))
                      (loop for i of-type fixnum from start below end do (body (char-code (char string i))))))))
    (each-code (code)
       (cond ((< code #x80) (put code))
             ((raw-byte-code-p code) (put (- code +raw-byte-base+)))
             ((<= #xD800 code #xDFFF) (put #xEF) (put #xBF) (put #xBD))
             ((< code #x800)
              (put (logior #xC0 (ash code -6)))
              (put (logior #x80 (logand code #x3F))))
             ((< code #x10000)
              (put (logior #xE0 (ash code -12)))
              (put (logior #x80 (logand (ash code -6) #x3F)))
              (put (logior #x80 (logand code #x3F))))
             (t
              (put (logior #xF0 (ash code -18)))
              (put (logior #x80 (logand (ash code -12) #x3F)))
              (put (logior #x80 (logand (ash code -6) #x3F)))
              (put (logior #x80 (logand code #x3F)))))))
  (values out at))

(defun string-octets (string &key (start 0) (end (length string)))
  "STRING from START below END as UTF-8 octets, a raw byte's character
(U+DC80..U+DCFF) as its octet; any other lone surrogate, which no text decodes
to, as U+FFFD."
  (if (not (find-if (lambda (c) (<= #xD800 (char-code c) #xDFFF)) string :start start :end end))
      (sb-ext:string-to-octets string :external-format :utf-8 :start start :end end)
      (multiple-value-bind (out at)
          (put-string-octets string (make-array (* 4 (- end start)) :element-type '(unsigned-byte 8)) 0 :start start :end end)
        (subseq out 0 at))))
