;;;; test/rope.lisp --- Property tests for the rope against a naive string model.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

;;; The oracle: plain strings, with byte-offset tables computed from code points.

(defun utf8-length (char)
  "Number of UTF-8 octets CHAR occupies."
  (let ((c (char-code char)))
    (cond ((< c #x80) 1) ((< c #x800) 2) ((< c #x10000) 3) (t 4))))

(defun byte-offsets (string)
  "Vector of length (1+ (length STRING)): the UTF-8 byte offset of each character index."
  (let ((v (make-array (1+ (length string)) :element-type 'fixnum)) (b 0))
    (setf (aref v 0) 0)
    (loop for i from 0 below (length string)
          do (incf b (utf8-length (char string i))) (setf (aref v (1+ i)) b))
    v))

(defun utf16-offsets (string)
  "Vector of length (1+ (length STRING)): the UTF-16 offset of each character index."
  (let ((v (make-array (1+ (length string)) :element-type 'fixnum)) (u 0))
    (setf (aref v 0) 0)
    (loop for i from 0 below (length string)
          do (incf u (if (>= (char-code (char string i)) #x10000) 2 1)) (setf (aref v (1+ i)) u))
    v))

(defun string-utf16 (string) "UTF-16 code units STRING occupies." (aref (utf16-offsets string) (length string)))
(defun string-line-end-p (string i)
  "True when character I of STRING ends a line: a newline, or a carriage return no newline follows."
  (let ((c (char string i)))
    (or (char= c #\Newline)
        (and (char= c #\Return) (not (and (< (1+ i) (length string)) (char= (char string (1+ i)) #\Newline)))))))
(defun string-lines (string &key (end (length string)))
  "Number of line ends in STRING before index END, as the whole of STRING has them."
  (loop for i from 0 below end count (string-line-end-p string i)))
(defun to-octets (string) "STRING as UTF-8 octets." (sb-ext:string-to-octets string :external-format :utf-8))

(defparameter *alphabet*
  (vector "a" "b" "c" "d" " " "x" "é" "ö" "€" "中" "😀" "🎉" (string #\Newline) (string #\Newline) (string #\Return))
  "Pieces random texts are built from: 1-, 2-, 3- and 4-octet characters, newlines
and carriage returns, alone and before newlines.")

(defun random-text (max-chars)
  "A random string of up to MAX-CHARS pieces from *ALPHABET*."
  (with-output-to-string (out)
    (loop repeat (random (1+ max-chars))
          do (write-string (aref *alphabet* (random (length *alphabet*))) out))))

(defun random-batch (string)
  "Return three values for a random batch of edits against STRING: the edits as
a simple-vector of tuatara.text:edit (byte offsets), the string after applying
them, and the fixnum vector of byte positions APPLY-EDITS should return."
  (let* ((len (length string))
         (offsets (byte-offsets string))
         (count (random (min 12 (1+ len))))
         (starts (sort (remove-duplicates (loop repeat count collect (random (1+ len)))) #'<))
         (edits '()) (pieces '()) (positions '())
         (pos 0) (delta 0))
    (loop for (start . rest) on starts
          do (let* ((limit (if rest (- (car rest) start) (- len start)))
                    (del (random (1+ (min 4 limit))))
                    (ins (if (zerop (random 3)) "" (random-text 4)))
                    (ins-octets (to-octets ins))
                    (start-byte (aref offsets start))
                    (end-byte (aref offsets (+ start del))))
               (push (tuatara.text:make-edit start-byte end-byte (if (plusp (length ins-octets)) ins-octets nil)) edits)
               (push (subseq string pos start) pieces)
               (push ins pieces)
               (push (+ start-byte delta (length ins-octets)) positions)
               (incf delta (- (length ins-octets) (- end-byte start-byte)))
               (setf pos (+ start del))))
    (push (subseq string pos) pieces)
    (values (coerce (nreverse edits) 'simple-vector)
            (apply #'concatenate 'string (nreverse pieces))
            (coerce (nreverse positions) '(simple-array fixnum (*))))))

;;; Tests

(define-test rope
  :description "The persistent rope: building, reading, index conversions, batch edits, undo, invariants.")

(defun check-rope-matches (rope string)
  "Assert that ROPE holds exactly STRING with correct counts and a valid structure."
  (true (tuatara.text:check-rope rope))
  (is string= string (tuatara.text:rope-string rope))
  (is = (length (to-octets string)) (tuatara.text:rope-bytes rope))
  (is = (length string) (tuatara.text:rope-chars rope))
  (is = (string-utf16 string) (tuatara.text:rope-utf16 rope))
  (is = (string-lines string) (tuatara.text:rope-lines rope)))

(define-test (rope build-and-read)
  (check-rope-matches (tuatara.text:make-rope) "")
  (check-rope-matches (tuatara.text:make-rope "hello") "hello")
  (dotimes (i 20)
    (let ((s (random-text 3000)))
      (check-rope-matches (tuatara.text:string->rope s) s)))
  ;; big enough for two levels of branches
  (let ((s (random-text 80000)))
    (check-rope-matches (tuatara.text:string->rope s) s)))

(define-test (rope index-conversions)
  (dotimes (i 15)
    (let* ((s (random-text (if (< i 12) 2000 40000)))
           (rope (tuatara.text:string->rope s))
           (offsets (byte-offsets s))
           (u16 (utf16-offsets s))
           (line-starts (cons 0 (loop for i from 0 below (length s)
                                      when (string-line-end-p s i) collect (aref offsets (1+ i))))))
      (loop repeat 60
            for c = (random (1+ (length s)))
            do (is = (aref offsets c) (tuatara.text:char->byte rope c))
               (is = c (tuatara.text:byte->char rope (aref offsets c)))
               (is = (aref u16 c) (tuatara.text:byte->utf16 rope (aref offsets c)))
               (is = (aref offsets c) (tuatara.text:utf16->byte rope (aref u16 c)))
               (is = (string-lines s :end c) (tuatara.text:byte->line rope (aref offsets c)))
               (true (tuatara.text:char-boundary-p rope (aref offsets c))))
      ;; inner bytes of multi-octet characters are not boundaries
      (loop for c from 0 below (length s)
            when (> (utf8-length (char s c)) 1)
              do (false (tuatara.text:char-boundary-p rope (1+ (aref offsets c)))))
      (loop for l from 0 below (length line-starts)
            do (is = (nth l line-starts) (tuatara.text:line->byte rope l))))))

(define-test (rope random-edits-match-oracle)
  (dotimes (round 12)
    (let* ((s (random-text (if (< round 9) 3000 60000)))
           (rope (tuatara.text:string->rope s)))
      (dotimes (step 25)
        (multiple-value-bind (edits expected expected-positions) (random-batch s)
          (multiple-value-bind (new positions) (tuatara.text:apply-edits rope edits)
            (check-rope-matches new expected)
            (is equalp expected-positions positions)
            ;; the old rope is untouched
            (is string= s (tuatara.text:rope-string rope))
            ;; undo restores the old text
            (let ((inverse (tuatara.text:inverse-edits edits positions)))
              (is string= s (tuatara.text:rope-string (tuatara.text:apply-edits new inverse))))
            (setf rope new s expected)))))))

(define-test (rope edge-cases)
  (let ((r (tuatara.text:make-rope)))
    (check-rope-matches (tuatara.text:rope-insert r 0 "abc") "abc")
    (check-rope-matches (tuatara.text:rope-insert (tuatara.text:rope-insert r 0 "abc") 3 "😀") "abc😀")
    (check-rope-matches (tuatara.text:rope-delete (tuatara.text:rope-insert r 0 "abc") 0 3) "")
    ;; an insertion larger than a leaf into a small rope
    (let ((big (make-string (* 3 tuatara.text:+max-leaf+) :initial-element #\z)))
      (check-rope-matches (tuatara.text:rope-insert (tuatara.text:make-rope "ab") 1 big)
                          (concatenate 'string "a" big "b")))
    ;; delete almost everything from a many-leaf rope: leaves merge, tree collapses
    (let* ((s (random-text 50000)) (rope (tuatara.text:string->rope s))
           (offsets (byte-offsets s))
           (keep (min 5 (length s))))
      (check-rope-matches (tuatara.text:rope-delete rope (aref offsets keep) (aref offsets (length s)))
                          (subseq s 0 keep)))
    ;; a deletion spanning many leaves plus an insertion at its start
    (let* ((s (random-text 50000)) (rope (tuatara.text:string->rope s))
           (offsets (byte-offsets s)) (a (floor (length s) 4)) (b (floor (* 3 (length s)) 4)))
      (multiple-value-bind (new positions)
          (tuatara.text:apply-edits rope (vector (tuatara.text:make-edit (aref offsets a) (aref offsets b) (to-octets "MID"))))
        (check-rope-matches new (concatenate 'string (subseq s 0 a) "MID" (subseq s b)))
        (is = (+ (aref offsets a) 3) (aref positions 0))))
    ;; empty batch
    (multiple-value-bind (new positions) (tuatara.text:apply-edits (tuatara.text:make-rope "x") #())
      (is string= "x" (tuatara.text:rope-string new))
      (is = 0 (length positions)))
    ;; invalid batches are rejected
    (fail (tuatara.text:apply-edits (tuatara.text:make-rope "abc") (vector (tuatara.text:make-edit 2 1))))
    (fail (tuatara.text:apply-edits (tuatara.text:make-rope "abc")
                                    (vector (tuatara.text:make-edit 0 2) (tuatara.text:make-edit 1 3))))
    (fail (tuatara.text:apply-edits (tuatara.text:make-rope "abc") (vector (tuatara.text:make-edit 0 9))))))

(define-test (rope large-batch-matches-flat-oracle)
  ;; 100,000 lines, one insertion per line, compared with a flat one-pass oracle.
  (let* ((line (to-octets (format nil "The quick brown fox jumps over the lazy dog 0123456789~%")))
         (lines 100000)
         (octets (let ((v (make-array (* lines (length line)) :element-type '(unsigned-byte 8))))
                   (dotimes (i lines v) (replace v line :start1 (* i (length line))))))
         (rope (tuatara.text:octets->rope octets))
         (ins (to-octets "x"))
         (edits (coerce (loop for i from 0 below lines
                              collect (tuatara.text:make-edit (+ (* i (length line)) 4) (+ (* i (length line)) 4) ins))
                        'simple-vector)))
    (multiple-value-bind (new positions) (tuatara.text:apply-edits rope edits)
      (true (tuatara.text:check-rope new))
      (is = (+ (length octets) lines) (tuatara.text:rope-bytes new))
      (is = (rope-lines-oracle octets) (tuatara.text:rope-lines new))
      (let ((expected (make-array (+ (length octets) lines) :element-type '(unsigned-byte 8)))
            (src 0) (dst 0))
        (loop for e across edits
              do (replace expected octets :start1 dst :start2 src :end2 (tuatara.text:edit-start e))
                 (incf dst (- (tuatara.text:edit-start e) src))
                 (setf src (tuatara.text:edit-start e))
                 (replace expected ins :start1 dst)
                 (incf dst 1))
        (replace expected octets :start1 dst :start2 src)
        (is equalp expected (tuatara.text:rope-octets new))
        (is = (+ 4 1) (aref positions 0))
        (is = (+ (* 99999 (length line)) 4 99999 1) (aref positions 99999))))))

(defun rope-lines-oracle (octets) "Newline count of OCTETS." (count 10 octets))

(define-test (rope bracket-matching)
  ;; f(a [b] {c}) é(x) (open: offsets are octets, é taking two
  (let ((rope (tuatara.text:string->rope (format nil "f(a [b] {c}) ~a(x) (open" (code-char #xe9)))))
    (flet ((forward (p &rest keys) (apply #'tuatara.text:rope-bracket-match rope p keys))
           (backward (p &rest keys) (apply #'tuatara.text:rope-bracket-match rope p :backward t keys)))
      ;; forward: the offset just after the partner
      (is = 12 (forward 1))
      (is = 7 (forward 4))
      (is = 18 (forward 15))
      ;; backward: the offset of the partner
      (is = 1 (backward 12))
      (is = 4 (backward 7))
      (is = 15 (backward 18))
      ;; no partner before the text ends, not a bracket, or past the limit
      (false (forward 19))
      (false (forward 0))
      (false (backward 3))
      (false (forward 1 :limit 5))
      (is = 12 (forward 1 :limit 11))))
  ;; kinds count together, as the text alone cannot tell them apart; the second value says whether they pair
  (is equal '(2 nil) (multiple-value-list (tuatara.text:rope-bracket-match (tuatara.text:string->rope "(]") 0)))
  (is equal '(2 t) (multiple-value-list (tuatara.text:rope-bracket-match (tuatara.text:string->rope "[]") 0)))
  (is equal '(0 nil) (multiple-value-list (tuatara.text:rope-bracket-match (tuatara.text:string->rope "{)") 2 :backward t)))
  ;; brackets in the spans given, or escaped when asked, count for nothing; nor do those past START or END
  (let ((rope (tuatara.text:string->rope "(a \")\" \\) b)")))
    (is = 5 (tuatara.text:rope-bracket-match rope 0))
    (is = 9 (tuatara.text:rope-bracket-match rope 0 :spans (vector (cons 3 6))))
    (is = 12 (tuatara.text:rope-bracket-match rope 0 :spans (vector (cons 3 6)) :escapes t))
    (is = 0 (tuatara.text:rope-bracket-match rope 12 :backward t :spans (vector (cons 3 6)) :escapes t))
    (false (tuatara.text:rope-bracket-match rope 0 :spans (vector (cons 3 6)) :escapes t :end 11))
    (false (tuatara.text:rope-bracket-match rope 12 :backward t :spans (vector (cons 3 6)) :escapes t :start 1)))
  ;; blanks passed over, either way, as octets
  (let ((rope (tuatara.text:string->rope (format nil "a ~c~% ~cb  " #\Tab #\Return))))
    (is = 6 (tuatara.text:rope-skip-octets rope 1 tuatara.text:*blank-octets*))
    (is = 1 (tuatara.text:rope-skip-octets rope 6 tuatara.text:*blank-octets* :backward t))
    (is = 3 (tuatara.text:rope-skip-octets rope 1 '(32 9)))
    (is = 9 (tuatara.text:rope-skip-octets rope 7 tuatara.text:*blank-octets*))
    (is = 0 (tuatara.text:rope-skip-octets rope 0 '(97) :backward t))
    (is = 4 (tuatara.text:rope-skip-octets rope 1 tuatara.text:*blank-octets* :bound 4)))
  ;; far apart, across many blocks read
  (let* ((text (concatenate 'string "(" (make-string 200000 :initial-element #\x) ")"))
         (rope (tuatara.text:string->rope text)))
    (is = 200002 (tuatara.text:rope-bracket-match rope 0))
    (is = 0 (tuatara.text:rope-bracket-match rope 200002 :backward t)))
  ;; in a file's text, read where it lies on disk
  (let ((path (format nil "~abrackets-~d.txt" (namestring (uiop:temporary-directory)) (random 1000000))))
    (with-open-file (o path :direction :output :if-exists :supersede)
      (write-string "x (" o) (dotimes (i 70000) (write-string "ab" o)) (write-string ") y" o))
    (unwind-protect
         (let ((rope (tuatara.text:open-file-rope path)))
           (is = 140004 (tuatara.text:rope-bracket-match rope 2))
           (is = 2 (tuatara.text:rope-bracket-match rope 140004 :backward t)))
      (delete-file path))))

(define-test (rope pairs-around-and-octets-back)
  ;; a(b(c)d)e: from inside, the pair around
  (let ((rope (tuatara.text:string->rope "a(b(c)d)e")))
    (is = 8 (tuatara.text:rope-bracket-match rope 3 :inside t))
    (is = 1 (tuatara.text:rope-bracket-match rope 3 :inside t :backward t))
    (is = 6 (tuatara.text:rope-bracket-match rope 5 :inside t))
    (is = 3 (tuatara.text:rope-bracket-match rope 5 :inside t :backward t))
    (false (tuatara.text:rope-bracket-match rope 9 :inside t))
    (false (tuatara.text:rope-bracket-match rope 0 :inside t :backward t)))
  ;; the last octet before a place, across blocks
  (let ((rope (tuatara.text:string->rope (concatenate 'string "q" (make-string 150000 :initial-element #\x) "q!"))))
    (is = 150001 (tuatara.text:rope-find-octet-backward rope (char-code #\q)))
    (is = 0 (tuatara.text:rope-find-octet-backward rope (char-code #\q) :end 150001))
    (false (tuatara.text:rope-find-octet-backward rope (char-code #\z)))
    (false (tuatara.text:rope-find-octet-backward rope (char-code #\q) :end 150001 :start 1))))

(define-test (rope blank-lines-and-octet-sets)
  ;; lines: 0 "a", 2 "", 3 "  ", 6 "b", 8 "c", 10 "", 11 "d"; 12 octets
  (let ((rope (tuatara.text:string->rope (format nil "a~%~%  ~%b~%c~%~%d"))))
    (flet ((down (start &rest keys) (apply #'tuatara.text:rope-line-downward rope start keys))
           (up (start &rest keys) (apply #'tuatara.text:rope-line-upward rope start keys)))
      (is = 2 (down 0 :blank t))
      (is = 6 (down 2))
      (is = 10 (down 6 :blank t))
      (is = 12 (down 11 :blank t))
      (is = 11 (down 11))
      (is = 10 (up 11 :blank t))
      (is = 3 (up 8 :blank t))
      (is = 6 (up 6))
      (is = 0 (up 3))
      (is = 2 (up 2 :blank t))
      (false (up 0 :blank t))
      ;; past the limit: no answer, and said so going up
      (false (down 0 :blank t :limit 1))
      (is equal '(nil :limit) (multiple-value-list (up 11 :blank t :limit 1)))))
  ;; a text without a blank line, read to its end
  (let* ((line (format nil "2026-09-28,12.5,note~%"))
         (rope (tuatara.text:string->rope (with-output-to-string (o) (dotimes (i 300000) (write-string line o))))))
    (is = (* 300000 (length line)) (tuatara.text:rope-line-downward rope 0 :blank t))
    (false (tuatara.text:rope-line-upward rope (* 299999 (length line)) :blank t)))
  ;; any of a set of octets, forward and back
  (let ((rope (tuatara.text:string->rope "ab.cd?ef")))
    (is = 2 (tuatara.text:rope-find-any-octet rope (list (char-code #\.) (char-code #\?))))
    (is = 5 (tuatara.text:rope-find-any-octet rope (list (char-code #\.) (char-code #\?)) :from-end t))
    (is = 5 (tuatara.text:rope-find-any-octet rope (list (char-code #\?)) :start 3))
    (false (tuatara.text:rope-find-any-octet rope (list (char-code #\!))))))

(defun column-by-steps (buffer line column)
  "The position COLUMN characters into BUFFER's line starting at LINE, or the
line's end where it is shorter, found a character at a time."
  (let ((end (tuatara.buffer:buffer-line-end buffer line)) (p line))
    (loop repeat column
          while (< p end)
          do (setf p (tuatara.text:rope-next-boundary (tuatara.buffer:buffer-text buffer) p)))
    p))

(defun line-slices-by-steps (buffer left right)
  "Each line's rectangle slice of BUFFER between the columns LEFT and RIGHT,
found a line at a time by the buffer's own steps, as the rows of a rectangle
were before MAP-LINE-SLICES: (LINE START END START-COLUMN END-COLUMN LINE-END)."
  (let ((size (tuatara.buffer:buffer-size buffer)) (rows '()))
    (loop for line = 0 then (let ((end (tuatara.buffer:buffer-line-end buffer line)))
                              (if (>= end size) nil (tuatara.buffer:buffer-forward-char buffer end)))
          while line
          do (let ((s (column-by-steps buffer line left))
                   (e (column-by-steps buffer line right)))
               (push (list line s e (tuatara.buffer:buffer-column buffer s) (tuatara.buffer:buffer-column buffer e)
                           (tuatara.buffer:buffer-line-end buffer line))
                     rows)))
    (nreverse rows)))

(define-test (rope line-slices)
  ;; the same slices as the buffer's steps, over text of every kind: tabs,
  ;; UTF-8, octets that are not UTF-8, CRLF lines, empty and unfinished lines
  (let ((texts (list (vector)
                     (map 'vector #'char-code "abc")
                     (map 'vector #'char-code (format nil "abc~%"))
                     (map 'vector #'char-code (format nil "a~cb~%xyz~%~%q" #\Tab))
                     (concatenate 'vector #(195 169 228 184 173 120 10 121) (map 'vector #'char-code (format nil "~%longer line here~%")))
                     #(97 255 98 10 128 99 100 10 228 184 10 240 159 152 128 101 10)
                     (map 'vector #'char-code (format nil "a~c~%bc~c~%~c~%d" #\Return #\Return #\Return))
                     (map 'vector #'char-code (format nil "a~cb~%~c" #\Return #\Return)))))
    (dolist (octets texts)
      (dolist (crlf '(nil t))
        (let ((b (tuatara.buffer:make-buffer (format nil " line-slices-~d" (random 100000))
                                             :text (tuatara.text:octets->rope (coerce octets '(simple-array (unsigned-byte 8) (*)))))))
          (unwind-protect
               (progn
                 (when crlf (setf (tuatara.buffer:buffer-local b :line-ending) :crlf))
                 (loop for left from 0 to 5
                       do (loop for right from left to 7
                                do (let ((found '()))
                                     (tuatara.text:map-line-slices
                                      (lambda (&rest row) (push row found))
                                      (tuatara.buffer:buffer-text b) 0 (1+ (tuatara.buffer:buffer-size b)) left right :crlf crlf)
                                     (is equal (line-slices-by-steps b left right) (nreverse found)
                                         "~s ~:[LF~;CRLF~] ~d-~d" octets crlf left right)))))
            (tuatara.buffer:kill-buffer b)))))))

(define-test (rope line-slices-read-what-is-asked)
  ;; the columns of one line are read through a block about as large as the
  ;; line, not a megabyte for every line asked about (each key moving point
  ;; down a line asks once): what one more line asked about makes
  (let* ((line (format nil "~{~d~^,~}" (loop for i below 30 collect i)))
         (rope (tuatara.text:string->rope (with-output-to-string (o) (dotimes (i 40000) (write-line line o)))))
         (stride (1+ (length line))))
    (flet ((made (lines)
             (nth-value 2 (measuring
                            (dotimes (i lines)
                              (tuatara.text:map-line-slices (lambda (&rest slice) (declare (ignore slice)))
                                                            rope (* i stride) (1+ (* i stride)) 5 10))))))
      ;; a megabyte a line before; 4 KB now
      (let ((each (/ (- (made 400) (made 100)) 300)))
        (true (< each 20000) "~:d octets a line" (round each)))))
  ;; a line longer than the block: read on to its end, the same as the buffer's own steps find
  (let ((b (tuatara.buffer:make-buffer " long-line-slices" :text (format nil "ab~%~a~%cd" (make-string 50000 :initial-element #\x)))))
    (unwind-protect
         (dolist (columns '((0 3) (4090 4100) (49990 50010)))
           (destructuring-bind (left right) columns
             (let ((found '()))
               (tuatara.text:map-line-slices (lambda (&rest slice) (push slice found))
                                             (tuatara.buffer:buffer-text b) 0 (1+ (tuatara.buffer:buffer-size b)) left right)
               (is equal (line-slices-by-steps b left right) (nreverse found) "~d-~d" left right))))
      (tuatara.buffer:kill-buffer b))))

(define-test (rope line-slices-past-characters-of-no-width)
  ;; a run of characters of no width longer than the block read does not cut
  ;; the line short: what follows it is in the slice, the line's end its end
  (let* ((text (format nil "x~a~a" (make-string 1400 :initial-element (code-char #x200B)) "SECRET"))
         (rope (tuatara.text:string->rope text))
         (total (tuatara.text:rope-bytes rope))
         (found '()))
    (tuatara.text:map-line-slices (lambda (&rest slice) (push slice found)) rope 0 1 0 10
                                  :width (lambda (code column before) (declare (ignore column before)) (values (if (= code #x200B) 0 1) code)))
    (destructuring-bind (line start end start-column end-column line-end) (first found)
      (is = 0 line)
      (is = 0 start)
      (is = total end)
      (is = 0 start-column)
      (is = 7 end-column)
      (is = total line-end))))

(define-test (rope conversions-read-leaves-in-place)
  ;; a character's, a UTF-16 unit's or a line's offset is found where its leaf
  ;; lies, nothing copied: what one more call makes is next to nothing (a
  ;; whole leaf, kilobytes, before)
  (let* ((rope (tuatara.text:index-rope
                (tuatara.text:string->rope (with-output-to-string (o) (dotimes (i 20000) (format o "line é😀 ~d~%" i))))))
         (chars (tuatara.text:rope-chars rope))
         (lines (tuatara.text:rope-lines rope)))
    (flet ((made (calls)
             (nth-value 2 (measuring
                            (dotimes (i calls)
                              (tuatara.text:char->byte rope (mod (* i 7919) chars))
                              (tuatara.text:utf16->byte rope (mod (* i 7919) chars))
                              (tuatara.text:line->byte rope (mod (* i 31) lines)))))))
      (let ((each (/ (- (made 2000) (made 500)) 1500)))
        (true (< each 64) "~,1f octets a call" each))))
  ;; and they agree with counting from the start
  (let* ((text "aé😀b
cd")
         (rope (tuatara.text:index-rope (tuatara.text:string->rope text))))
    (is equal '(0 1 3 7 8 9 10 11) (loop for c from 0 to 7 collect (tuatara.text:char->byte rope c)))
    (is equal '(0 1 3 3 7 8) (loop for u from 0 to 5 collect (tuatara.text:utf16->byte rope u)))
    (is = 9 (tuatara.text:line->byte rope 1))))

(define-test (rope searching-back-makes-nothing)
  ;; looking back for an octet (a line's start, every key) reads through a block
  ;; on the stack: however far it looks, it makes next to nothing (twice the
  ;; distance, in new blocks, before)
  (let ((rope (tuatara.text:string->rope (format nil "x~%~a" (make-string 200000 :initial-element #\a)))))
    (flet ((made (back)
             (nth-value 2 (measuring
                            (dotimes (i 20)
                              (tuatara.text:rope-find-octet-backward rope 10 :start (- 200002 back) :end 200002))))))
      (let ((each (/ (- (made 64000) (made 4000)) 20)))
        (true (< each 1024) "~:d octets a search" (round each))))
    (is = 2 (tuatara.text:rope-line-start rope 150000))
    (is = 1 (tuatara.text:rope-find-octet-backward rope 10 :end 200002))))

(define-test (rope character-steps-make-nothing)
  ;; a step to the next or the previous character, or a look at one, reads the
  ;; few octets around it on the stack: nothing made for each (a new vector before)
  (let* ((rope (tuatara.text:string->rope (with-output-to-string (o) (dotimes (i 5000) (write-string "aé😀🇳🇿" o)))))
         (total (tuatara.text:rope-bytes rope)))
    (flet ((made (steps)
             (nth-value 2 (measuring
                            (let ((p 0))
                              (dotimes (i steps)
                                (setf p (tuatara.text:rope-next-boundary rope p))
                                (tuatara.text:char-boundary-p rope p)
                                (tuatara.text:rope-char-at rope p)
                                (tuatara.text:rope-previous-boundary rope p)
                                (tuatara.text:rope-next-boundary rope p t)))))))
      (let ((each (/ (- (made 8000) (made 2000)) 6000)))
        (true (< each 16) "~,1f octets a step" each)))
    (is = 1 (tuatara.text:rope-next-boundary rope 0))
    (is = 3 (tuatara.text:rope-next-boundary rope 1))
    (is = 7 (tuatara.text:rope-next-boundary rope 3))
    (is = 15 (tuatara.text:rope-next-boundary rope 7 t))
    (is = 7 (tuatara.text:rope-previous-boundary rope 15 t))
    (is = 3 (tuatara.text:rope-previous-boundary rope 7))
    (false (tuatara.text:char-boundary-p rope 2))
    (is = total (tuatara.text:rope-next-boundary rope total))))

(define-test (rope long-line-ends)
  ;; a line longer than 64 KB: its ends found by its number once the lines are
  ;; counted, by reading while they are not, the same either way
  (let* ((text (format nil "x~%~a~%y" (make-string 200000 :initial-element #\a)))
         (plain (tuatara.text:string->rope text))
         (counted (tuatara.text:index-rope plain)))
    (true (tuatara.text:rope-indexed-p counted))
    (dolist (rope (list plain counted))
      (is = 200002 (tuatara.text:rope-line-end rope 2))
      (is = 200002 (tuatara.text:rope-line-end rope 150000))
      (is = 2 (tuatara.text:rope-line-start rope 150000))
      (is = 2 (tuatara.text:rope-line-start rope 200002))
      (is = 1 (tuatara.text:rope-line-end rope 0))
      (is = 200003 (tuatara.text:rope-line-start rope (1- (length text))))
      (is = (length text) (tuatara.text:rope-line-end rope (1- (length text)))))))

(define-test (rope octet-lengths-counted)
  ;; counted without making the octets, the same as making them: raw bytes one, other lone surrogates three
  (let ((pieces (list "a" "é" "€" "😀" (string (code-char #xDC80)) (string (code-char #xDCFF)) (string (code-char #xD800)) (string #\Newline))))
    (dotimes (i 200)
      (let ((s (format nil "~{~a~}" (loop repeat (random 12) collect (nth (random (length pieces)) pieces)))))
        (is = (length (tuatara.text:string-octets s)) (tuatara.text:string-octet-length s))
        (when (> (length s) 2)
          (is = (length (tuatara.text:string-octets s :start 1 :end (1- (length s))))
              (tuatara.text:string-octet-length s :start 1 :end (1- (length s)))))))
    (is = 1 (tuatara.text:char-octet-length (code-char #xDC80)))
    (is = 3 (tuatara.text:char-octet-length (code-char #xD800)))))

(define-test (rope grapheme-clusters)
  ;; with CLUSTERS the boundaries step over what a reader takes for one character
  (flet ((starts (string)
           ;; the cluster starts of STRING, forwards, and backwards from its end
           (let* ((rope (tuatara.text:string->rope string))
                  (size (tuatara.text:rope-bytes rope))
                  (forward (loop for p = 0 then (tuatara.text:rope-next-boundary rope p t)
                                 while (< p size) collect p))
                  (backward (loop for p = size then (tuatara.text:rope-previous-boundary rope p t)
                                  while (plusp p) collect (tuatara.text:rope-previous-boundary rope p t))))
             (is equal forward (reverse backward))
             forward))
         (text (&rest parts)
           (apply #'concatenate 'string
                  (mapcar (lambda (part) (if (integerp part) (string (code-char part)) part)) parts))))
    ;; a letter and its accents
    (is equal '(0 1 6) (starts (text "a" "e" #x301 #x323 "x")))
    ;; an emoji and its skin tone, a family joined by U+200D
    (is equal '(0 8 9) (starts (text #x1F44D #x1F3FD " " "x")))
    (is equal '(0 18) (starts (text #x1F468 #x200D #x1F469 #x200D #x1F467 "y")))
    ;; flags: regional indicators two by two
    (is equal '(0 8 16) (starts (text #x1F1F3 #x1F1FF #x1F1EF #x1F1F5 #x1F1F3)))
    ;; Hangul jamo making one syllable; Devanagari vowel signs with their letter
    (is equal '(0 9) (starts (text #x1100 #x1161 #x11A8 "x")))
    (is equal '(0 3 6 12) (starts (text #x928 #x92E #x938 #x94D #x924 #x947)))
    ;; a line's end, a tab and a raw byte join nothing, and nothing joins them
    (is equal '(0 3 4 6 7) (starts (text "a" #x301 (string #\Newline) #x301 (string #\Tab) "b")))
    (let ((rope (tuatara.text:octets->rope (coerce #(97 #xFF #xCC #x81 98) 'tuatara.text:octets))))
      (is = 1 (tuatara.text:rope-next-boundary rope 0 t))
      (is = 2 (tuatara.text:rope-next-boundary rope 1 t))
      (is = 1 (tuatara.text:rope-previous-boundary rope 2 t)))
    ;; a run of marks longer than a cluster's reach is cut, never read whole
    (let* ((rope (tuatara.text:string->rope (text "a" (make-string 1000 :initial-element (code-char #x301)))))
           (next (tuatara.text:rope-next-boundary rope 0 t)))
      (true (< 1 next 300)))))

(define-test (rope plain-strings)
  ;; octets in hex, with a separator or none
  (let ((octets (coerce #(0 171 16 255) 'tuatara.text:octets)))
    (is string= "00ab10ff" (tuatara.text:octets-hex octets))
    (is string= "00 AB 10 FF" (tuatara.text:octets-hex octets :upper-case t :separator " "))
    (is string= "AB:10" (tuatara.text:octets-hex octets :start 1 :end 3 :upper-case t :separator ":")))
  ;; a string shortened after a word, or where it must; on one line when asked
  (is string= "short" (tuatara.display:shorten-string "short" 10))
  (is string= "the quick…" (tuatara.display:shorten-string "the quick brown fox" 14))
  (is string= "abcdefghi…" (tuatara.display:shorten-string "abcdefghijklmnop" 10))
  (is string= "one two" (tuatara.display:shorten-string (format nil "one~%two") 20 :one-line t))
  ;; in columns: a wide character takes two
  (is string= "日本語…" (tuatara.display:shorten-string "日本語のテキスト" 7))
  ;; on one line: runs of line ends and other control characters one space, the ends trimmed
  (is string= "C:\\photos\\x.jpg: $HOME no face" (tuatara.text:one-line (format nil " C:\\photos\\x.jpg:~c$HOME~c~%no face~%" #\Tab #\Return)))
  (false (tuatara.text:one-line nil))
  ;; a name without its namespace's prefix
  (is string= "Description" (tuatara.text:xml-unprefixed "rdf:Description"))
  (is string= "title" (tuatara.text:xml-unprefixed "title"))
  (false (tuatara.text:xml-local-name "text")))

(define-test (rope shared-character-sets)
  ;; the sets of octets every part of the editor goes by, each written once
  (is equal '(32 9 13) (remove-if-not #'tuatara.text:blank-line-octet-p '(32 9 10 13 65 12)))
  (is equal '(32 9 13) tuatara.text:*blank-line-octets*)
  (is = (- 256 (length tuatara.text:*blank-octets*)) (length tuatara.text:*text-octets*))
  (true (notany (lambda (o) (member o tuatara.text:*blank-octets*)) tuatara.text:*text-octets*))
  (is equal '(40 91 123 41 93 125) tuatara.text:*bracket-octets*)
  (is equal '(40 91 123) tuatara.text:*opening-octets*)
  (is equal '(41 93 125) tuatara.text:*closing-octets*)
  (is equal '((40 . 41) (91 . 93) (123 . 125)) tuatara.syntax::*bracket-pairs*)
  (is equal '(32 9 10 13 40 91 123 41 93 125) tuatara.defaults::*sexp-boundary-octets*)
  ;; a line end of two octets, in a block and in a rope, and the text before it
  (let ((octets (tuatara.text:string-octets (format nil "ab~c~cc~c" #\Return #\Newline #\Return))))
    (true (tuatara.text:crlf-octets-p octets 2 (length octets)))
    (false (tuatara.text:crlf-octets-p octets 5 (length octets)))
    (is = 2 (tuatara.text:line-text-end octets 0 3 t))
    (is = 3 (tuatara.text:line-text-end octets 0 3 nil))
    (let ((rope (tuatara.text:octets->rope octets)))
      (true (tuatara.text:rope-crlf-p rope 2))
      (false (tuatara.text:rope-crlf-p rope 5))
      (false (tuatara.text:rope-crlf-p rope -1)))))

(define-test (rope bracket-match-makes-nothing)
  ;; matching a bracket, as each move of point shows its partner, makes no table of its own
  (let ((rope (tuatara.text:string->rope "(defun f (x) [x {y}])")))
    (is = 21 (tuatara.text:rope-bracket-match rope 0))
    (is = 13 (tuatara.text:rope-bracket-match rope 20 :backward t))
    (is = 20 (tuatara.text:rope-bracket-match rope 13 :openers "[" :closers "]"))
    (let ((consed (loop repeat 3
                        minimize (let ((before (tuatara.bench::bytes-consed)))
                                   (tuatara.text:rope-bracket-match rope 0)
                                   (- (tuatara.bench::bytes-consed) before)))))
      (true (< consed 256) "a match consed ~:d octets" consed))))

(define-test (rope lenient-string-as-decoder-replaces)
  ;; what is not UTF-8 becomes the replacement as the external format's decoder has it: a
  ;; sequence cut short as one, any other octet as one each; a text's decoding makes its string alone
  (let ((state (sb-ext:seed-random-state 27)) (differ 0))
    (dotimes (n 20000)
      (let ((octets (make-array (random 14 state) :element-type '(unsigned-byte 8))))
        (dotimes (i (length octets))
          (setf (aref octets i) (case (random 4 state)
                                  (0 (random 128 state))
                                  (1 (+ 128 (random 64 state)))
                                  (t (random 256 state)))))
        (let ((start (random (1+ (length octets)) state)))
          (unless (string= (sb-ext:octets-to-string octets :start start :external-format '(:utf-8 :replacement #\?))
                           (tuatara.text:lenient-string octets :start start))
            (incf differ)))))
    (is = 0 differ))
  (let* ((text (tuatara.text:string-octets
                (with-output-to-string (o) (dotimes (i 20000) (write-string "kia ora tūī ✓ " o)))))
         (per-octet (/ (least-bytes (lambda () (tuatara.text:lenient-string text)) 3) (length text))))
    (true (< per-octet 10) "~,1f octets made for each octet decoded" per-octet)))
