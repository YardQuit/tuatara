;;;; test/raw-bytes.lisp --- Text that is not all UTF-8: every octet kept, each invalid one a character.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test raw-bytes
  :description "Any octets are text. Well-formed UTF-8 is characters, every other octet a raw byte of its own, shown as \\xNN and saved as it was; the rope's counts, boundaries and edits agree with a reference decoder, whatever the octets.")

(defun reference-decode (octets)
  "The characters of OCTETS by Unicode's table of well-formed UTF-8 sequences,
written out plainly as the oracle: a list of (START LENGTH CODE), a raw byte
being (START 1 (+ #xDC00 OCTET))."
  (let ((n (length octets)) (i 0) (out '()))
    (flet ((in (k lo hi) (and (< (+ i k) n) (<= lo (aref octets (+ i k)) hi))))
      (loop while (< i n)
            do (let* ((b (aref octets i))
                      (length (cond ((<= b #x7F) 1)
                                    ((<= #xC2 b #xDF) (if (in 1 #x80 #xBF) 2 0))
                                    ((= b #xE0) (if (and (in 1 #xA0 #xBF) (in 2 #x80 #xBF)) 3 0))
                                    ((or (<= #xE1 b #xEC) (<= #xEE b #xEF)) (if (and (in 1 #x80 #xBF) (in 2 #x80 #xBF)) 3 0))
                                    ((= b #xED) (if (and (in 1 #x80 #x9F) (in 2 #x80 #xBF)) 3 0))
                                    ((= b #xF0) (if (and (in 1 #x90 #xBF) (in 2 #x80 #xBF) (in 3 #x80 #xBF)) 4 0))
                                    ((<= #xF1 b #xF3) (if (and (in 1 #x80 #xBF) (in 2 #x80 #xBF) (in 3 #x80 #xBF)) 4 0))
                                    ((= b #xF4) (if (and (in 1 #x80 #x8F) (in 2 #x80 #xBF) (in 3 #x80 #xBF)) 4 0))
                                    (t 0))))
                 (if (zerop length)
                     (progn (push (list i 1 (+ #xDC00 b)) out) (incf i))
                     (let ((code (if (= length 1) b
                                     (sb-ext:octets-to-string (subseq octets i (+ i length)) :external-format :utf-8))))
                       (push (list i length (if (stringp code) (char-code (char code 0)) code)) out)
                       (incf i length))))))
    (nreverse out)))

(defparameter *raw-pieces*
  (append (map 'list #'to-octets '("a" "b" "é" "€" "😀" "中" " "))
          (list #(10) #(#x80) #(#xBF) #(#xC0) #(#xC1) #(#xF5) #(#xFF) #(#xE2) #(#xE2 #x82) #(#xF0 #x9F) #(#xF0 #x9F #x98)
                #(#x82) #(#xAC) #(#x9F) #(#x98) #(#x80 #x80 #x80 #x80) #(#xE0 #x80 #x80) #(#xED #xA0 #x80)
                #(#xF4 #x90 #x80 #x80) #(#xC3) #(#xA9) #(#xF0) #(#x90) #(#xED) #(#xE0)))
  "Pieces random octet strings are made of: characters, newlines, lone leads and
continuations, overlong forms, surrogates, and parts that join into characters.")

(defun random-octets (pieces)
  "A random octet vector of PIECES random pieces from *RAW-PIECES*."
  (coerce (apply #'concatenate '(vector (unsigned-byte 8))
                 (loop repeat pieces collect (nth (random (length *raw-pieces*)) *raw-pieces*)))
          'tuatara.text:octets))

(defun check-against-reference (rope octets)
  "Check ROPE, whose octets should be OCTETS, against REFERENCE-DECODE: counts,
every boundary, characters, conversions and the string both ways."
  (let* ((chars (reference-decode octets))
         (starts (make-array (1+ (length octets)) :initial-element nil)))
    (dolist (c chars) (setf (aref starts (first c)) t))
    (setf (aref starts (length octets)) t)
    (true (tuatara.text:check-rope rope))
    (is equalp octets (tuatara.text:rope-octets rope))
    (is = (length chars) (tuatara.text:rope-chars rope))
    (is = (count-if (lambda (c) (<= #xDC80 (third c) #xDCFF)) chars) (tuatara.text:rope-invalid rope))
    (is = (loop for c in chars sum (if (= (second c) 4) 2 1)) (tuatara.text:rope-utf16 rope))
    (is = (count 10 octets) (tuatara.text:rope-lines rope))
    (true (loop for i from 0 to (length octets)
                always (eq (aref starts i) (tuatara.text:char-boundary-p rope i))))
    (true (loop for (start length code) in chars
                for k from 0
                always (and (= code (char-code (tuatara.text:rope-char-at rope start)))
                            (= (+ start length) (tuatara.text:rope-next-boundary rope start))
                            (= start (tuatara.text:rope-previous-boundary rope (+ start length)))
                            (= k (tuatara.text:byte->char rope start))
                            (= start (tuatara.text:char->byte rope k)))))
    (let ((string (tuatara.text:rope-string rope)))
      (is equal (mapcar #'third chars) (map 'list #'char-code string))
      ;; nothing lost: the string encodes back to the very octets
      (is equalp octets (tuatara.text:string-octets string)))))

(define-test (raw-bytes decoding-matches-reference)
  (dotimes (round 30)
    (let ((octets (random-octets (if (< round 25) 200 8000))))
      (check-against-reference (tuatara.text:octets->rope octets) octets)))
  ;; the classic cases, one by one
  (flet ((chars-of (&rest bytes) (map 'list #'char-code (tuatara.text:rope-string (tuatara.text:octets->rope (coerce bytes 'tuatara.text:octets))))))
    (is equal '(#x20AC) (chars-of #xE2 #x82 #xAC))
    (is equal '(#xDCE2 #xDC82 #x41) (chars-of #xE2 #x82 #x41))          ; cut short
    (is equal '(#xDCC0 #xDC80) (chars-of #xC0 #x80))                    ; overlong NUL
    (is equal '(#xDCED #xDCA0 #xDC80) (chars-of #xED #xA0 #x80))        ; a surrogate
    (is equal '(#xDCF4 #xDC90 #xDC80 #xDC80) (chars-of #xF4 #x90 #x80 #x80)) ; past U+10FFFF
    (is equal '(#x41 #xDC80 #x42) (chars-of #x41 #x80 #x42))            ; a stray continuation
    (is equal '(#x1F600) (chars-of #xF0 #x9F #x98 #x80))
    (is equal '(#xDCF7 #xDCBF #xDCBF #xDCBF) (chars-of #xF7 #xBF #xBF #xBF))
    (is equal '(#x41 #xDCC3) (chars-of #x41 #xC3)))                     ; a lead at the end
  ;; text in a charset named as mail and web pages name one; an unknown one, and UTF-8, as the editor reads files
  (flet ((in (charset &rest bytes) (map 'list #'char-code (tuatara.text:charset-text (coerce bytes 'tuatara.text:octets) charset))))
    (is equal '(#x20AC #xE9) (in "windows-1252" #x80 #xE9))
    (is equal '(#xE9) (in "\"ISO-8859-1\"" #xE9))
    (is equal '(#x41) (in "us-ascii" #x41))
    (is equal '(#x20AC #xDC80) (in "utf-8" #xE2 #x82 #xAC #x80))
    (is equal '(#xDCE9) (in "no-such-charset" #xE9))
    (is equal '(#xDCE9) (in "make-hash-table" #xE9))
    (is equal '(#xDCE9) (in nil #xE9))))

(define-test (raw-bytes edits-keep-agreeing)
  ;; random edits of random octets, joins of leads and continuations included,
  ;; against a flat vector: every leaf's counts stay right (edits widen when needed)
  (dotimes (round 8)
    (let* ((octets (random-octets (if (< round 6) 400 12000)))
           (rope (tuatara.text:octets->rope octets)))
      (dotimes (step 30)
        (let* ((chars (reference-decode octets))
               (starts (cons 0 (mapcar (lambda (c) (+ (first c) (second c))) chars)))
               (count (random 4))
               (points (sort (loop repeat (* 2 count) collect (nth (random (length starts)) starts)) #'<))
               (edits (loop for (a b) on points by #'cddr
                            collect (tuatara.text:make-edit a b (let ((r (random 3)))
                                                                  (case r (0 nil) (t (random-octets (1+ (random 3)))))))))
               (expected (let ((out '()) (at 0))
                           (dolist (e edits)
                             (push (subseq octets at (tuatara.text:edit-start e)) out)
                             (when (tuatara.text:edit-insert e) (push (tuatara.text:edit-insert e) out))
                             (setf at (tuatara.text:edit-end e)))
                           (push (subseq octets at) out)
                           (coerce (apply #'concatenate '(vector (unsigned-byte 8)) (nreverse out)) 'tuatara.text:octets))))
          (multiple-value-bind (new positions) (tuatara.text:apply-edits rope (coerce edits 'simple-vector))
            (check-against-reference new expected)
            ;; undo brings the old octets back
            (is equalp octets (tuatara.text:rope-octets
                               (tuatara.text:apply-edits new (tuatara.text:inverse-edits (coerce edits 'simple-vector) positions))))
            (setf rope new octets expected)))))))

(define-test (raw-bytes inserts-meeting-across-a-deletion)
  ;; two inserts with a deletion of all between them meet: their octets one character
  (let* ((rope (tuatara.text:octets->rope (coerce #(195 191) 'tuatara.text:octets)))
         (new (tuatara.text:apply-edits rope (vector (tuatara.text:make-edit 0 0 (coerce #(10 226 130) 'tuatara.text:octets))
                                                    (tuatara.text:make-edit 0 2)
                                                    (tuatara.text:make-edit 2 2 (coerce #(130) 'tuatara.text:octets))))))
    (true (tuatara.text:check-rope new))
    (is = 2 (tuatara.text:rope-chars new))
    (is equal (list 10 #x2082) (map 'list #'char-code (tuatara.text:rope-string new)))))

(define-test (raw-bytes joins)
  ;; deleting what stood between a lead and its continuations makes one character of them
  (let* ((rope (tuatara.text:octets->rope (coerce #(#xE2 #x41 #x82 #xAC) 'tuatara.text:octets)))
         (new (tuatara.text:rope-delete rope 1 2)))
    (is equal (list #x20AC) (map 'list #'char-code (tuatara.text:rope-string new)))
    (true (tuatara.text:check-rope new)))
  ;; the same across leaves: a lead at the end of one, continuations at the start of the next
  (let* ((a (make-array tuatara.text:+target-leaf+ :element-type '(unsigned-byte 8) :initial-element 97))
         (octets (concatenate '(vector (unsigned-byte 8)) a #(#xE2 #x41) a #(#x82 #xAC) a))
         (octets (coerce octets 'tuatara.text:octets))
         (rope (tuatara.text:octets->rope octets))
         (lead (length a))
         (new (tuatara.text:rope-delete rope (1+ lead) (+ lead 2 (length a)))))
    (check-against-reference new (coerce (concatenate '(vector (unsigned-byte 8)) a #(#xE2 #x82 #xAC) a) 'tuatara.text:octets))
    (is = #x20AC (char-code (tuatara.text:rope-char-at new lead))))
  ;; ropes spliced so that a lead meets its continuations
  (let* ((left (tuatara.text:octets->rope (coerce #(#x61 #xE2) 'tuatara.text:octets)))
         (middle (tuatara.text:octets->rope (coerce #(#x82 #xAC #x62) 'tuatara.text:octets)))
         (spliced (tuatara.text:splice-rope left 2 2 middle)))
    (check-against-reference spliced (coerce #(#x61 #xE2 #x82 #xAC #x62) 'tuatara.text:octets))))

(define-test (raw-bytes mapped-files)
  ;; a mapped file of random octets, indexed in parallel pieces, agrees too
  (let ((octets (random-octets 60000)))
    (with-temp-file (path octets)
      (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope path)
        (unwind-protect
             (let ((indexed (tuatara.text:index-rope rope :chunk 4096)))
               (check-against-reference indexed octets))
          (when mapping (tuatara.text:close-mapping mapping)))))))

(define-test (raw-bytes shown-and-edited)
  ;; the display shows each raw byte as \xNN, four columns, in its own face
  (let* ((rope (tuatara.text:octets->rope (coerce (concatenate '(vector (unsigned-byte 8)) (to-octets "a") #(#xFF) (to-octets "b"))
                                                  'tuatara.text:octets)))
         (rows (tuatara.display:layout rope 0 20 1)))
    (is string= "a\\xFFb" (coerce (tuatara.display::row-string (aref rows 0)) 'string))
    (is = 6 (tuatara.display::row-columns (aref rows 0)))
    (is eq :raw-byte (aref (tuatara.display::row-faces (aref rows 0)) 1)))
  ;; in a buffer, a raw byte is one character: moved over, deleted, copied and put back whole
  (let ((b (tuatara.buffer:make-buffer "*raw-test*" :text (coerce (concatenate '(vector (unsigned-byte 8)) (to-octets "é") #(#x80) (to-octets "x"))
                                                                  'tuatara.text:octets))))
    (unwind-protect
         (progn
           (is = 2 (tuatara.buffer:buffer-forward-char b 0))
           (is = 3 (tuatara.buffer:buffer-forward-char b 2))
           (let ((raw (tuatara.buffer:buffer-string b :start 2 :end 3)))
             (is = #xDC80 (char-code (char raw 0)))
             (tuatara.buffer:buffer-delete b 2 3)
             (is string= "éx" (tuatara.buffer:buffer-string b))
             (tuatara.buffer:buffer-insert b 2 raw)
             (is equalp (coerce #(#xC3 #xA9 #x80 #x78) 'tuatara.text:octets)
                 (tuatara.text:rope-octets (tuatara.buffer:buffer-text b)))))
      (tuatara.buffer:kill-buffer b))))

(define-test (raw-bytes in-names-and-typing)
  ;; a file name that is not UTF-8 is watched by its own octets, as inotify names it
  (is equalp #(97 255) (cdr (tuatara.io::watch-key (format nil "/somewhere/a~c" (code-char #xDCFF)))))
  ;; an extended attribute whose name is not UTF-8 is listed, read and copied
  (with-temp-file (from (to-octets "a"))
    (with-temp-file (to (to-octets "b"))
      (let ((name (format nil "user.t~c" (code-char #xDCFF))))
        (with-open-file (in from :element-type '(unsigned-byte 8))
          (with-open-file (out to :element-type '(unsigned-byte 8) :direction :output :if-exists :append)
            (let ((in-fd (sb-sys:fd-stream-fd in)) (out-fd (sb-sys:fd-stream-fd out))
                  (key (tuatara.io::path-octets name)) (value (coerce #(1 2) 'tuatara.text:octets)))
              (is = 0 (sb-sys:with-pinned-objects (key value)
                        (sb-alien:alien-funcall
                         (sb-alien:extern-alien "fsetxattr" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                                      sb-sys:system-area-pointer sb-alien:unsigned-long sb-alien:int))
                         in-fd (sb-sys:vector-sap key) (sb-sys:vector-sap value) 2 0)))
              (true (member name (tuatara.io:fd-xattr-names in-fd) :test #'string=))
              (tuatara.io:copy-xattrs in-fd out-fd)
              (is equalp #(1 2) (tuatara.io:fd-xattr-value out-fd name))))))))
  ;; typed into a terminal, a raw byte's character reaches the program as its octet
  (let ((emulator (tuatara.defaults::%make-emulator)))
    (tuatara.defaults::emulator-input emulator (format nil "a~cb" (code-char #xDCFF)))
    (is equalp #(97 255 98) (sb-concurrency:receive-message (tuatara.defaults::emulator-outbox emulator) :timeout 1))))

(define-test (raw-bytes written-to-json-as-replacements)
  ;; a raw byte in text sent as JSON (to a language server) is U+FFFD: what is sent is UTF-8
  (let ((written (tuatara.text:json-write (format nil "a~cb" (code-char #xDCFF)))))
    (is string= (format nil "\"a~cb\"" (code-char #xFFFD)) written)
    (is equalp #(34 97 239 191 189 98 34) (tuatara.text:string-octets written))))

(define-test (raw-bytes given-to-c-as-they-were-read)
  ;; text given to a C library: UTF-8, a raw byte as the octet it was, any other
  ;; lone surrogate as U+FFFD rather than an error; read back as it was given
  (let ((text (format nil "a~c~cb~c" (code-char #xDCFF) (code-char #xE9) (code-char #xD800))))
    (tuatara.io:with-c-string (sap text :length length)
      (is = 8 length)
      (is equalp #(97 255 195 169 98 239 191 189 0) (tuatara.io:sap-octets sap 9))
      (is string= (format nil "a~c~cb~c" (code-char #xDCFF) (code-char #xE9) (code-char #xFFFD))
          (tuatara.io:sap-string sap)))
    (tuatara.io:with-c-string (sap text :start 2 :end 4 :length length)
      (is = 3 length)
      (is equalp #(195 169 98 0) (tuatara.io:sap-octets sap 4)))
    ;; nothing made for each string given, short or as long as a clipboard's
    (flet ((consed (string)
             (let ((before (tuatara.bench::bytes-consed)))
               (dotimes (i 100) (tuatara.io:with-c-string (sap string) (sb-sys:sap-ref-8 sap 0)))
               (/ (- (tuatara.bench::bytes-consed) before) 100))))
      (consed text)
      (true (< (consed text) 1) "~,1f octets of garbage for each short string" (consed text))
      (let ((long (make-string 100000 :initial-element #\x)))
        (consed long)
        (true (< (consed long) 64) "~,1f octets of garbage for each long string" (consed long))))))

(tuatara.io:define-c-function ("strchr" text-from-character) :string
  "The text of TEXT from the first CHARACTER in it (a code), or NIL when there is none."
  (text :string) (character :int))

(define-test (raw-bytes c-functions-take-any-string)
  ;; a C function's text argument may be any string: one with a fill pointer, one
  ;; with a raw byte; its text result keeps a raw byte as it was
  (let ((growing (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c across (format nil "caf~cs and more" (code-char #xE9)) do (vector-push-extend c growing))
    (is string= "s and more" (text-from-character growing (char-code #\s))))
  (let ((raw (format nil "x~cy" (code-char #xDCFF))))
    (is string= (format nil "~cy" (code-char #xDCFF)) (text-from-character raw 255))
    (is string= raw (text-from-character raw (char-code #\x))))
  (false (text-from-character "abc" (char-code #\z))))
