;;;; lisp/editing/sort.lisp --- Sorting lines and other records; deleting duplicate and matching lines.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Emacs's commands, each doing one thing: sort-lines, reverse-region,
;;;; sort-fields, sort-numeric-fields, sort-columns, sort-regexp-fields,
;;;; sort-paragraphs, sort-pages, delete-duplicate-lines, flush-lines and
;;;; keep-lines; and a -reverse command for each sort, in descending order. They act on what the selection touches, the lines commands on
;;;; its whole lines, and without a selection do nothing and say so. Records
;;;; are sorted stably, the text between them staying where it was; a prefix
;;;; argument sorts in descending order, ties reversed with the rest, as Emacs
;;;; has it.
;;;;
;;;; Every one runs on a thread of its own and is waited for briefly: a small
;;;; region looks done at once, a large one carries on in the background with
;;;; its progress in the mode line. A region under *SORT-IN-MEMORY-LIMIT* is
;;;; handled in memory. A larger region of lines is cut into pieces at line
;;;; starts, and the pool's workers handle a piece each: for a sort, into an
;;;; unnamed run file, the runs then merged into the result; for duplicates,
;;;; into a file of the lines kept, the files then joined. So a hundred million
;;;; lines sort without their text in memory. The result arrives as one undoable
;;;; step; an edit to the region meanwhile abandons it, edits elsewhere are kept.
;;;;
;;;; flush-lines and keep-lines give each line a fate through REWRITE-LINES,
;;;; as packages do: a job over lines (START-LINE-JOB), which keeps the edits
;;;; made to them meanwhile.

(in-package #:tuatara.defaults)

(defvar *sort-fold-case* nil
  "True to ignore the case of ASCII letters when sorting: \"b\" then sorts
before \"C\".")

(defvar *sort-numeric-base* 10
  "The base sort-numeric-fields reads numbers in, unless they begin with 0x.")

(defvar *sort-in-memory-limit* (* 32 1024 1024)
  "Regions of this many octets or more are sorted or cleared of duplicate
lines on disk, in pieces, in the background.")

(defvar *sort-piece* (* 64 1024 1024)
  "Octets of a large region each worker sorts at a time.")

(defvar *filter-piece* (* 8 1024 1024)
  "Octets of a large region each worker clears of duplicate lines at a time
\(delete-duplicate-lines), or of a large file's records it matches search
filters against (search-filter-regex).")

(defvar *sort-wait* 1/2
  "Seconds a sort or filter is waited for before it carries on in the background.")

(defparameter *sort-carried-settings* '(*sort-piece* *filter-piece* *sort-numeric-base* *sort-in-memory-limit*)
  "The settings a sort or filter reads as it works, carried to the thread it
works on as they are on the editor's when it starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

;;; What is selected

(defun selected-lines (buffer &key columns)
  "The start and end of the whole lines BUFFER's selection touches, or NIL
without a selection: from the start of its first line to the start of the line
after its last, or the text's end. Lines are those a window shows, as a
line selection takes them (WHOLE-LINES): a folded headline's line is whole
with all the fold hides. The last line is the one its end is in, except that
an end at the start of a line leaves that line out, unless COLUMNS is true
\(sort-columns counts it)."
  (when (tuatara.buffer:buffer-mark-active-p buffer)
    (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
      (when (and start (< start end))
        (whole-lines buffer start end :visible t :columns columns)))))

(defvar *sort-descending* nil
  "True while one of the reverse sort commands runs (\\[sort-lines-reverse] and
the others): the sort is in descending order, whatever the prefix argument.")

(defvar *sort-command* nil
  "The reverse sort command running, to name in what is said, or NIL.")

(defun sort-command-name (command)
  "The name to say for COMMAND: the reverse sort command running it, if any."
  (or *sort-command* command))

(defun sort-descending-p ()
  "True to sort in descending order: in a reverse sort command, or with a
prefix argument."
  (or *sort-descending* (and (prefix-argument) t)))

(defmacro with-selected-lines ((buffer start end command &key columns) &body body)
  "Run BODY with START and END bound to the whole lines the current buffer's
selection touches and BUFFER to the buffer; without a selection, say that
COMMAND was not run. Refused when search filters hide any of those lines
\(CHECK-HIDDEN-LINES)."
  `(let ((,buffer (current-buffer)))
     (multiple-value-bind (,start ,end) (selected-lines ,buffer :columns ,columns)
       (if (null ,start)
           (message "No selection: ~(~a~) was not run" (sort-command-name ,command))
           (progn (check-hidden-lines ,buffer ,start ,end)
                  ,@body)))))

;;; Lines of octets

(defun count-newlines (octets start end)
  "How many newline octets OCTETS holds from START below END."
  (declare (type tuatara.text:octets octets) (type fixnum start end) (optimize speed))
  (loop for i of-type fixnum from start below end count (= (aref octets i) 10)))

(defun scan-lines (octets crlf)
  "The lines of OCTETS, which hold whole lines: three fixnum vectors of each
line's start, the end of its text (before its newline, and in a CRLF buffer
before a carriage return ahead of that) and the start of the line after."
  (declare (type tuatara.text:octets octets))
  (let* ((size (length octets))
         (n (+ (count-newlines octets 0 size) (if (and (plusp size) (/= (aref octets (1- size)) 10)) 1 0)))
         (starts (make-array n :element-type 'fixnum))
         (ends (make-array n :element-type 'fixnum))
         (nexts (make-array n :element-type 'fixnum))
         (p 0) (k 0))
    (declare (type fixnum size p k))
    (loop while (< p size)
          do (let* ((newline (or (loop for i of-type fixnum from p below size when (= (aref octets i) 10) return i) size))
                    (text-end (if (< newline size) (tuatara.text:line-text-end octets p newline crlf) newline))
                    (next (min size (1+ newline))))
               (declare (type fixnum newline text-end next))
               (setf (aref starts k) p (aref ends k) text-end (aref nexts k) next)
               (incf k)
               (setf p next)))
    (values starts ends nexts)))

(defun rope-line-before (rope at crlf)
  "The text of ROPE's line that ends just before AT, a line's start, as octets,
without its line end (a carriage return too with CRLF)."
  (let* ((end (tuatara.text:rope-line-text-end rope 0 (1- at) crlf))
         (start (tuatara.text:rope-line-start rope (max 0 end))))
    (tuatara.text:rope-octets rope :start start :end end)))

(defun line-preview (octets start end)
  "The line of OCTETS from START below END as text to show in a message, cut to 60 columns."
  (let ((text (tuatara.text:octets-string octets :start start :end (min end (+ start 240)))))
    (tuatara.display:shorten-string text 60)))

;;; Keys

(declaim (inline space-or-tab-octet-p fold-octet))
(defun space-or-tab-octet-p (octet)
  "True for a space or a tab: the octets between a line's fields."
  (or (= octet 32) (= octet 9)))

(defun fold-octet (octet)
  "OCTET with an ASCII capital letter made small."
  (if (<= 65 octet 90) (+ octet 32) octet))

(declaim (inline space-or-tab-char-p))
(defun space-or-tab-char-p (char)
  "True for a space or a tab: the characters between a line's fields."
  (or (char= char #\Space) (char= char #\Tab)))

(defmacro define-field-bounds (name type blank-p)
  "Define NAME, a function finding a field of a line held in a vector of TYPE,
BLANK-P telling apart what separates its fields."
  `(defun ,name (text start end n)
     ,(format nil "Where field N of the line from START below END of TEXT~@
                   (~(~a~)) begins and ends: fields are separated by spaces and~@
                   tabs and counted from 1, or from the end of the line when N is~@
                   negative. NIL when the line has too few." type)
     (declare (type ,type text) (type fixnum start end n))
     (if (plusp n)
         (let ((p start))
           (declare (type fixnum p))
           (loop repeat (1- n)
                 do (loop while (and (< p end) (,blank-p (aref text p))) do (incf p))
                    (loop while (and (< p end) (not (,blank-p (aref text p)))) do (incf p)))
           (loop while (and (< p end) (,blank-p (aref text p))) do (incf p))
           (when (< p end)
             (let ((e p))
               (declare (type fixnum e))
               (loop while (and (< e end) (not (,blank-p (aref text e)))) do (incf e))
               (values p e))))
         (let ((p end))
           (declare (type fixnum p))
           (loop repeat (1- (- n))
                 do (loop while (and (> p start) (,blank-p (aref text (1- p)))) do (decf p))
                    (loop while (and (> p start) (not (,blank-p (aref text (1- p))))) do (decf p)))
           (loop while (and (> p start) (,blank-p (aref text (1- p)))) do (decf p))
           (when (> p start)
             (let ((s p))
               (declare (type fixnum s))
               (loop while (and (> s start) (not (,blank-p (aref text (1- s))))) do (decf s))
               (values s p)))))))

(define-field-bounds field-bounds tuatara.text:octets space-or-tab-octet-p)
(define-field-bounds text-field-bounds simple-string space-or-tab-char-p)

(defun digit-value (octet)
  "The value of OCTET as a digit of up to base 36, or NIL."
  (cond ((<= 48 octet 57) (- octet 48))
        ((<= 97 (fold-octet octet) 122) (+ 10 (- (fold-octet octet) 97)))
        (t nil)))

(defun field-number (octets start end)
  "The number written at START of OCTETS, read no further than END, as a
double-float: 0x and hexadecimal digits, else an optional sign and digits in
*SORT-NUMERIC-BASE*, in base 10 with a fraction and an exponent too; 0 when
none is there. A leading zero does not make a number octal, so zero-padded
numbers sort as the numbers they are."
  (declare (type tuatara.text:octets octets) (type fixnum start end))
  (let ((p start) (sign 1) (base *sort-numeric-base*))
    (declare (type fixnum p))
    (when (and (< p end) (member (aref octets p) '(43 45)))
      (when (= (aref octets p) 45) (setf sign -1))
      (incf p))
    (when (and (< (1+ p) end) (= (aref octets p) 48) (member (aref octets (1+ p)) '(120 88)))
      (setf base 16)
      (incf p 2))
    (let ((whole 0) (digits 0))
      (loop while (< p end)
            do (let ((d (digit-value (aref octets p))))
                 (if (and d (< d base))
                     (setf whole (+ (* whole base) d) digits (1+ digits) p (1+ p))
                     (return))))
      (let ((value whole))
        (when (= base 10)
          (when (and (< p end) (= (aref octets p) 46))
            (incf p)
            (let ((scale 1))
              (loop while (and (< p end) (<= 48 (aref octets p) 57))
                    do (setf scale (* scale 10) value (+ value (/ (- (aref octets p) 48) scale)) digits (1+ digits))
                       (incf p))))
          (when (and (plusp digits) (< (1+ p) end) (member (aref octets p) '(101 69)))
            (let ((q (1+ p)) (exponent-sign 1) (exponent 0) (exponent-digits 0))
              (declare (type fixnum q))
              (when (and (< q end) (member (aref octets q) '(43 45)))
                (when (= (aref octets q) 45) (setf exponent-sign -1))
                (incf q))
              (loop while (and (< q end) (<= 48 (aref octets q) 57))
                    do (setf exponent (+ (* exponent 10) (- (aref octets q) 48)) exponent-digits (1+ exponent-digits))
                       (incf q))
              (when (plusp exponent-digits)
                (setf value (* value (expt 10 (* exponent-sign (min exponent 400)))))))))
        (cond ((zerop digits) 0d0)
              ;; past what a double holds: the largest there is, of the sign
              (t (handler-case (coerce (* sign value) 'double-float)
                   (error () (if (minusp sign) most-negative-double-float most-positive-double-float)))))))))

(defun column-bounds (octets start end from to &optional (tab-width 8))
  "Where the text in display columns FROM below TO of the line from START below
END of OCTETS begins and ends: a tab reaches the next multiple of TAB-WIDTH,
other characters take the columns the display gives them."
  (declare (type tuatara.text:octets octets) (type fixnum start end from to tab-width))
  (let ((column 0) (p start) (key-start nil) (before 0))
    (declare (type fixnum column p before))
    (loop while (< p end)
          do (when (and (null key-start) (>= column from)) (setf key-start p))
             (when (>= column to) (return))
             (multiple-value-bind (code length) (tuatara.text:octets-decode octets p end)
               (multiple-value-bind (columns next) (tuatara.display:advance-column code column before tab-width nil)
                 (incf column columns)
                 (setf before next))
               (incf p length)))
    (values (or key-start p) p)))

(defun line-keys (octets starts ends key)
  "The sort keys of the lines of OCTETS (as SCAN-LINES gives them) for KEY, one
of (:LINE), (:INDEX), (:FIELD N), (:NUMERIC N) and (:COLUMNS FROM TO TAB-WIDTH): two fixnum
vectors of where each key starts and ends, and for :NUMERIC a double-float
vector of the numbers. A line with too few fields stops the sort, naming it."
  (let* ((n (length starts))
         (key-starts (make-array n :element-type 'fixnum))
         (key-ends (make-array n :element-type 'fixnum))
         (numbers (and (eq (first key) :numeric) (make-array n :element-type 'double-float))))
    (dotimes (i n)
      (let ((s (aref starts i)) (e (aref ends i)))
        (ecase (first key)
          ((:line :index) (setf (aref key-starts i) s (aref key-ends i) e))
          ((:field :numeric)
           (multiple-value-bind (fs fe) (field-bounds octets s e (second key))
             (unless fs
               (error "A line has too few fields for field ~d: ~a" (second key) (line-preview octets s e)))
             (setf (aref key-starts i) fs (aref key-ends i) fe)
             (when numbers (setf (aref numbers i) (field-number octets fs fe)))))
          (:columns
           (multiple-value-bind (cs ce) (column-bounds octets s e (second key) (third key) (or (fourth key) 8))
             (setf (aref key-starts i) cs (aref key-ends i) ce))))))
    (values key-starts key-ends numbers)))

(defun octet-runs-compare (octets a1 e1 a2 e2 fold)
  "-1, 0 or 1 as OCTETS from A1 below E1 come before, are the same as, or come
after those from A2 below E2: octet by octet, which for UTF-8 is the order of the
characters' codes, a shorter run before a longer one it begins; the case of
ASCII letters ignored with FOLD."
  (declare (type tuatara.text:octets octets) (type fixnum a1 e1 a2 e2) (optimize speed (safety 0)))
  (loop
    (cond ((= a1 e1) (return (if (= a2 e2) 0 -1)))
          ((= a2 e2) (return 1)))
    (let ((x (aref octets a1)) (y (aref octets a2)))
      (when fold (setf x (fold-octet x) y (fold-octet y)))
      (cond ((< x y) (return -1))
            ((> x y) (return 1))))
    (incf a1) (incf a2)))

(defun sap-compare (sap1 a1 e1 sap2 a2 e2 fold)
  "OCTET-RUNS-COMPARE over memory: SAP1 from A1 below E1 against SAP2 from A2 below E2."
  (declare (type sb-sys:system-area-pointer sap1 sap2) (type fixnum a1 e1 a2 e2) (optimize speed (safety 0)))
  (loop
    (cond ((= a1 e1) (return (if (= a2 e2) 0 -1)))
          ((= a2 e2) (return 1)))
    (let ((x (sb-sys:sap-ref-8 sap1 a1)) (y (sb-sys:sap-ref-8 sap2 a2)))
      (when fold (setf x (fold-octet x) y (fold-octet y)))
      (cond ((< x y) (return -1))
            ((> x y) (return 1))))
    (incf a1) (incf a2)))

(defun key-order (octets key-starts key-ends numbers key reverse fold)
  "The order of the lines by their keys: a fixnum vector of line indices,
sorted stably; for the :INDEX key the lines as they are. REVERSE reverses the
whole order, so that descending order reverses ties too."
  (let* ((n (length key-starts)) (order (make-array n :element-type 'fixnum)))
    (dotimes (i n) (setf (aref order i) i))
    (unless (eq (first key) :index)
      (setf order
            (if numbers
                (stable-sort order (lambda (i j) (declare (type fixnum i j)) (< (aref numbers i) (aref numbers j))))
                (stable-sort order (lambda (i j)
                                     (declare (type fixnum i j))
                                     (minusp (octet-runs-compare octets (aref key-starts i) (aref key-ends i)
                                                             (aref key-starts j) (aref key-ends j) fold)))))))
    (if reverse (nreverse order) order)))

(defun arrange-lines (octets starts ends nexts order)
  "Fresh octets holding the lines of OCTETS in ORDER, each line end staying
where it was: place I holds the text of line (ORDER I), then the end of line I."
  (let ((out (make-array (length octets) :element-type '(unsigned-byte 8))) (dst 0))
    (declare (type fixnum dst))
    (dotimes (slot (length order) out)
      (let ((r (aref order slot)))
        (replace out octets :start1 dst :start2 (aref starts r) :end2 (aref ends r))
        (incf dst (- (aref ends r) (aref starts r)))
        (replace out octets :start1 dst :start2 (aref ends slot) :end2 (aref nexts slot))
        (incf dst (- (aref nexts slot) (aref ends slot)))))))

(defun each-kept-line (octets starts nexts marks emit)
  "Call EMIT with OCTETS, a start and an end for each line of OCTETS that
MARKS, a bit vector, does not mark (1 for a line to delete), its line end with it."
  (dotimes (i (length starts))
    (when (zerop (sbit marks i))
      (funcall emit octets (aref starts i) (aref nexts i)))))

(defun put-octets (out from start end)
  "Add FROM's octets from START below END at the end of OUT, an adjustable
octet vector with a fill pointer, made larger when it has no room; OUT."
  (let ((dst (fill-pointer out)))
    (when (> (+ dst (- end start)) (array-dimension out 0))
      (setf out (adjust-array out (max (* 2 (array-dimension out 0)) (+ dst (- end start) 64)))))
    (setf (fill-pointer out) (+ dst (- end start)))
    (replace out from :start1 dst :start2 start :end2 end)
    out))

(defun kept-lines (octets starts nexts marks)
  "Fresh octets holding the lines of OCTETS that MARKS leaves (EACH-KEPT-LINE), each with its line end."
  (let ((out (make-array (length octets) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t)))
    (each-kept-line octets starts nexts marks (lambda (from start end) (setf out (put-octets out from start end))))
    (coerce out 'tuatara.text:octets)))

;;; Marking lines to delete

(defun octet-run-hash (octets start end)
  "A hash of OCTETS from START below END, the same for the same octets."
  (ldb (byte 62 0) (tuatara.text:octets-hash octets :start start :end end)))

(defun duplicate-marks (octets starts ends reverse adjacent keep-blanks)
  "A bit vector marking the lines of OCTETS that delete-duplicate-lines deletes:
all but the first of identical lines (the last with REVERSE); with ADJACENT only
a line identical to the one before it; never an empty line with KEEP-BLANKS.
Returns it and how many are marked. Lines are compared, and kept to compare,
as the octets they are: nothing is made for a line."
  (declare (type tuatara.text:octets octets) (type (simple-array fixnum (*)) starts ends))
  (let* ((n (length starts)) (marks (make-array n :element-type 'bit :initial-element 0)) (count 0)
         (seen (unless adjacent
                 ;; a line by its index, compared and hashed by its octets
                 (make-hash-table :test (lambda (i j)
                                          (declare (type fixnum i j))
                                          (zerop (octet-runs-compare octets (aref starts i) (aref ends i)
                                                                     (aref starts j) (aref ends j) nil)))
                                  :hash-function (lambda (i)
                                                   (declare (type fixnum i))
                                                   (octet-run-hash octets (aref starts i) (aref ends i)))
                                  :size (max 16 (min n 100000)))))
         (previous-start -1) (previous-end -1))
    (declare (type fixnum n count previous-start previous-end))
    (flet ((visit (i)
             (let ((s (aref starts i)) (e (aref ends i)))
               (unless (and keep-blanks (= s e))
                 (if adjacent
                     (progn
                       (when (and (>= previous-start 0) (zerop (octet-runs-compare octets s e previous-start previous-end nil)))
                         (setf (sbit marks i) 1)
                         (incf count))
                       (setf previous-start s previous-end e))
                     (if (gethash i seen)
                         (progn (setf (sbit marks i) 1) (incf count))
                         (setf (gethash i seen) t)))))))
      (if reverse
          (loop for i from (1- n) downto 0 do (visit i))
          (dotimes (i n) (visit i))))
    (values marks count)))

;;; Other records, in memory: paragraphs, pages, regexp matches

(defun blank-line-p (octets start end)
  "True when the line of OCTETS from START below END holds only spaces, tabs and carriage returns."
  (loop for i from start below end always (tuatara.text:blank-line-octet-p (aref octets i))))

(defun paragraph-records (octets starts ends nexts)
  "The paragraphs of OCTETS (lines as SCAN-LINES gives them): a list of (START
END) running from the first line of each paragraph to the start of the line
after its last; blank lines between them are not in any."
  (let ((records '()) (open nil))
    (dotimes (i (length starts))
      (if (blank-line-p octets (aref starts i) (aref ends i))
          (when open (push (list open (aref starts i)) records) (setf open nil))
          (unless open (setf open (aref starts i))))
      (when (and open (= i (1- (length starts))))
        (push (list open (aref nexts i)) records)))
    (nreverse records)))

(defun page-records (octets)
  "The pages of OCTETS: a list of (START END), each starting after the
newlines that begin it and ending just after the form feed at the start of a
line that ends it, or at the end."
  (let ((records '()) (p 0) (size (length octets)))
    (loop while (< p size)
          do (loop while (and (< p size) (= (aref octets p) 10)) do (incf p))
             (when (< p size)
               (let ((end (loop for i from p below size
                                when (and (= (aref octets i) 12) (or (= i 0) (= (aref octets (1- i)) 10)) (> i p))
                                  return (1+ i)
                                finally (return size))))
                 (push (list p end) records)
                 (setf p end))))
    (nreverse records)))

(defun arrange-records (octets records order extra)
  "Fresh octets holding OCTETS with its RECORDS, (START END) in order, put in
ORDER, the text between them staying where it was; EXTRA, when not NIL, is
added to a record that ends the text without a line end, so that records moved
do not run together."
  (let ((pieces '()) (at 0) (records (coerce records 'simple-vector)))
    (dotimes (slot (length records))
      (destructuring-bind (s e) (aref records slot)
        (push (subseq octets at s) pieces)
        (destructuring-bind (rs re) (aref records (aref order slot))
          (push (subseq octets rs re) pieces)
          (when (and extra (plusp (- re rs)) (/= (aref octets (1- re)) 10) (/= (aref order slot) slot))
            (push extra pieces)))
        (setf at e)))
    (push (subseq octets at) pieces)
    (let ((out (tuatara.text:make-octets (reduce #'+ pieces :key #'length))) (at 0))
      (dolist (piece (nreverse pieces) out)
        (replace out piece :start1 at)
        (incf at (length piece))))))

(defun record-order (octets records reverse fold)
  "The order of RECORDS, (START END) of OCTETS, by their text, stably; REVERSE as for KEY-ORDER."
  (key-order octets (map '(simple-array fixnum (*)) #'first records) (map '(simple-array fixnum (*)) #'second records)
             nil (list :line) reverse fold))

(defun regexp-records (text record-regexp key)
  "The matches of RECORD-REGEXP in TEXT as a list of (START END KEY), character
indices: KEY is \\& for the whole match, \\N for group N, else a regular
expression whose first match in the record is its key (an empty key when it has
none). Empty matches are passed over."
  (let* ((records '())
         (group (cond ((string= key "\\&") 0)
                      ((and (= (length key) 2) (char= (char key 0) #\\) (digit-char-p (char key 1)))
                       (digit-char-p (char key 1)))))
         (key-scanner (and (null group) (tuatara.syntax:compile-regex key)))
         (scanner (tuatara.syntax:compile-regex record-regexp))
         (pos 0))
    (loop while (<= pos (length text))
          do (multiple-value-bind (ms me rs re) (tuatara.runtime:stoppable (cl-ppcre:scan scanner text :start pos))
               (unless ms (return))
               (when (< ms me)
                 (push (list ms me
                             (cond ((eql group 0) (subseq text ms me))
                                   (group (let ((a (and (< (1- group) (length rs)) (aref rs (1- group)))))
                                            (if a (subseq text a (aref re (1- group))) "")))
                                   (t (multiple-value-bind (ks ke)
                                          (tuatara.runtime:stoppable (cl-ppcre:scan key-scanner text :start ms :end me))
                                        (if ks (subseq text ks ke) "")))))
                       records))
               (setf pos (if (= ms me) (1+ me) me))))
    (nreverse records)))

(defun sort-regexp-text (text record-regexp key reverse fold)
  "TEXT with the matches of RECORD-REGEXP sorted stably by KEY (REGEXP-RECORDS),
the text between them staying where it was; with REVERSE descending. Returns the
new text and the number of records."
  (let* ((records (coerce (regexp-records text record-regexp key) 'simple-vector))
         (n (length records))
         (order (make-array n :element-type 'fixnum)))
    (dotimes (i n) (setf (aref order i) i))
    (setf order (stable-sort order (lambda (i j)
                                     (let ((a (third (aref records i))) (b (third (aref records j))))
                                       (if fold (string-lessp a b) (string< a b))))))
    (when reverse (setf order (nreverse order)))
    (values (with-output-to-string (out)
              (let ((at 0))
                (dotimes (slot n)
                  (destructuring-bind (s e key) (aref records slot)
                    (declare (ignore key))
                    (write-string text out :start at :end s)
                    (destructuring-bind (rs re key) (aref records (aref order slot))
                      (declare (ignore key))
                      (write-string text out :start rs :end re))
                    (setf at e)))
                (write-string text out :start at)))
            n)))

;;; Writing files a piece at a time

(defun file-rope (fd length)
  "A rope over the first LENGTH octets of the open unnamed file FD, which it
takes: mapped, its lines counted later with the rest of the result, all at once
(START-SORT)."
  (if (zerop length)
      (progn (sb-posix:close fd) (tuatara.text:make-rope))
      (tuatara.text:mapping-rope (tuatara.text:map-fd fd length "a sorted region"))))

;;; Large regions: pieces, runs and the merge

(defstruct (sort-job (:include tuatara.runtime:job) (:constructor %make-sort-job) (:copier nil))
  "A sort or filter of BUFFER's lines, a job (TUATARA.RUNTIME:JOB), WORDS
saying what it does (\"Sorting\") and WHAT in full: PROGRESS a cons whose car
counts the octets handled, of TOTAL; COUNTING, once the result is written, a
cons whose car counts the octets of it whose lines are counted, of its cdr;
RESULT the octets or rope that replace the region once done, and MESSAGE what
to say then. REGION-START and REGION-END follow the region through edits made
meanwhile; CHANGED turns true when one touches it, which abandons the result.
BASE is the text the job began on (JOB-BEGINS-WHERE-GIVEN), the region
BASE-START below BASE-END in it, and SINCE the buffer's undo stack there: the
result stands in the undo history there, before the changes made meanwhile
\(TUATARA.BUFFER:BUFFER-CHANGE-WHERE-BEGUN)."
  words what (progress (list 0)) (total 1) (counting nil) result message
  (region-start 0) (region-end 0) changed change-function
  base (base-start 0) (base-end 0) since)

(setf (documentation 'sort-job-p 'function) "True when OBJECT is a SORT-JOB.")

(defstruct (run (:constructor make-run (fd length lines ends)) (:copier nil))
  "A piece of a region sorted into an unnamed file FD of LENGTH octets: its
LINES as records in their sorted order, each a header (the line's index in the
piece, where its key starts and ends in its text, the text's length, and a
number as two halves) and the line's text; and ENDS, the length of each line's
line end in the lines' first order."
  fd length lines ends mapping)

(setf (documentation 'make-run 'function) "A run of LINES records in the file FD of LENGTH octets."
      (documentation 'run-p 'function) "True when OBJECT is a RUN.")

(defconstant +record-header+ 24 "Octets of a run record's header.")

(defmacro with-scratch-file ((fd directory) &body body)
  "Run BODY with FD a new unnamed file in DIRECTORY (OPEN-SCRATCH-FILE), closed,
its octets freed, unless BODY returns a result: when it is stopped, fails (the
disk full) or returns NIL. Otherwise what it returns holds the file open."
  (let ((result (gensym "RESULT")))
    `(let ((,fd (tuatara.io:open-scratch-file ,directory)) (,result nil))
       (unwind-protect (setf ,result (multiple-value-list (progn ,@body)))
         (unless (first ,result) (ignore-errors (sb-posix:close ,fd))))
       (values-list ,result))))

(defun discard-run (run)
  "Let go of RUN's file, a run or a piece's output (FD ...)."
  (let ((fd (if (consp run) (first run) (run-fd run))))
    (when fd (ignore-errors (sb-posix:close fd)))))

(defun sort-piece (job rope piece key reverse crlf fold directory)
  "Sort the lines of PIECE, (START END) of ROPE, into an unnamed run file in
DIRECTORY and return the RUN; NIL when JOB was stopped first."
  (unless (car (sort-job-cancel job))
    (let* ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece)))
           (third (floor (length octets) 3)))
      ;; the piece counts in three steps, read, sorted and written, so that progress moves
      (sb-ext:atomic-incf (car (sort-job-progress job)) third)
      (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
        (multiple-value-bind (key-starts key-ends numbers) (line-keys octets starts ends key)
          (let ((order (prog1 (key-order octets key-starts key-ends numbers key reverse fold)
                         (sb-ext:atomic-incf (car (sort-job-progress job)) third))))
            (with-scratch-file (fd directory)
              (let ((writer (tuatara.io:make-writer fd))
                    (header (tuatara.text:make-octets +record-header+))
                    (line-ends (make-array (length starts) :element-type '(unsigned-byte 2))))
                (dotimes (i (length starts))
                  (setf (aref line-ends i) (- (aref nexts i) (aref ends i))))
                (loop for r across order
                      do (let* ((s (aref starts r)) (e (aref ends r))
                                (number (if numbers (aref numbers r) 0d0)))
                           (flet ((put (at value) (tuatara.io:octets-put-integer header value 4 :at at)))
                             (put 0 r)
                             (put 4 (- (aref key-starts r) s))
                             (put 8 (- (aref key-ends r) s))
                             (put 12 (- e s))
                             (put 16 (ldb (byte 32 0) (sb-kernel:double-float-low-bits number)))
                             (put 20 (ldb (byte 32 0) (sb-kernel:double-float-high-bits number))))
                           (tuatara.io:writer-octets writer header)
                           (tuatara.io:writer-octets writer octets s e)))
                (tuatara.io:writer-flush writer)
                (sb-ext:atomic-incf (car (sort-job-progress job)) (- (length octets) third third))
                (make-run fd (tuatara.io:writer-length writer) (length starts) line-ends)))))))))

(defstruct (cursor (:constructor make-cursor (run index sap end)) (:copier nil))
  "Where the merge is in a run: the RUN's INDEX among the runs, its memory SAP
and END, the record at POS, and that record's header decoded."
  run (index 0 :type fixnum) (sap (sb-sys:int-sap 0) :type sb-sys:system-area-pointer) (end 0 :type fixnum)
  (pos 0 :type fixnum) (line 0 :type fixnum) (key-start 0 :type fixnum) (key-end 0 :type fixnum)
  (text 0 :type fixnum) (length 0 :type fixnum) (number 0d0 :type double-float))

(setf (documentation 'make-cursor 'function) "A cursor at the start of the run RUN, the INDEXth, mapped at SAP up to END."
      (documentation 'cursor-p 'function) "True when OBJECT is a CURSOR.")

(defun cursor-read (cursor)
  "Decode the header of the record CURSOR is at; NIL at the end of its run."
  (let ((sap (cursor-sap cursor)) (pos (cursor-pos cursor)))
    (when (< pos (cursor-end cursor))
      (let ((low (sb-sys:sap-ref-32 sap (+ pos 16))) (high (sb-sys:sap-ref-32 sap (+ pos 20))))
        (setf (cursor-line cursor) (sb-sys:sap-ref-32 sap pos)
              (cursor-text cursor) (+ pos +record-header+)
              (cursor-key-start cursor) (+ pos +record-header+ (sb-sys:sap-ref-32 sap (+ pos 4)))
              (cursor-key-end cursor) (+ pos +record-header+ (sb-sys:sap-ref-32 sap (+ pos 8)))
              (cursor-length cursor) (sb-sys:sap-ref-32 sap (+ pos 12))
              (cursor-number cursor) (sb-kernel:make-double-float (if (>= high #x80000000) (- high #x100000000) high) low)))
      t)))

(defun cursor-advance (cursor)
  "Move CURSOR to its run's next record; NIL at the end."
  (setf (cursor-pos cursor) (+ (cursor-text cursor) (cursor-length cursor)))
  (cursor-read cursor))

(defun cursor-before-p (a b numeric reverse fold)
  "True when the record under cursor A comes before the one under B in the
merge: by key, descending with REVERSE, and between equal keys by run, the
later run first with REVERSE so that ties reverse as the pieces' did."
  (let ((c (if numeric
               (let ((x (cursor-number a)) (y (cursor-number b))) (cond ((< x y) -1) ((> x y) 1) (t 0)))
               (sap-compare (cursor-sap a) (cursor-key-start a) (cursor-key-end a)
                            (cursor-sap b) (cursor-key-start b) (cursor-key-end b) fold))))
    (cond ((/= c 0) (if reverse (> c 0) (< c 0)))
          (reverse (> (cursor-index a) (cursor-index b)))
          (t (< (cursor-index a) (cursor-index b))))))

(defun merge-runs (job runs function numeric reverse fold)
  "Merge RUNS, calling FUNCTION with the cursor of each record in merged order
(its record is under the cursor only during the call); stops when JOB is
stopped. The runs' files are mapped for the merge and let go after."
  (let ((heap (make-array (length runs))) (size 0))
    (flet ((before (i j) (cursor-before-p (aref heap i) (aref heap j) numeric reverse fold))
           (swap (i j) (rotatef (aref heap i) (aref heap j))))
      (labels ((up (i)
                 (loop while (and (plusp i) (before i (floor (1- i) 2)))
                       do (swap i (floor (1- i) 2)) (setf i (floor (1- i) 2))))
               (down (i)
                 (loop (let* ((l (1+ (* 2 i))) (r (1+ l)) (m i))
                         (when (and (< l size) (before l m)) (setf m l))
                         (when (and (< r size) (before r m)) (setf m r))
                         (when (= m i) (return))
                         (swap i m) (setf i m)))))
        (unwind-protect
             (progn
               (loop for run in runs for index from 0
                     when (plusp (run-length run))
                       do (let* ((mapping (tuatara.text:map-fd (run-fd run) (run-length run) "a sort run"))
                                 (cursor (make-cursor run index (tuatara.text:mapping-sap mapping) (run-length run))))
                            (setf (run-mapping run) mapping (run-fd run) nil)
                            (when (cursor-read cursor)
                              (setf (aref heap size) cursor)
                              (incf size)
                              (up (1- size)))))
               (let ((count 0))
                 (loop while (plusp size)
                       do (let ((cursor (aref heap 0)))
                            (when (and (zerop (mod (incf count) 65536)) (car (sort-job-cancel job))) (return))
                            (funcall function cursor)
                            (if (cursor-advance cursor)
                                (down 0)
                                (progn (setf (aref heap 0) (aref heap (1- size))) (decf size) (down 0)))))))
          (dolist (run runs)
            (when (run-mapping run) (tuatara.text:close-mapping (run-mapping run)))
            (when (run-fd run) (ignore-errors (sb-posix:close (run-fd run))))))))))

(defun write-kept-line-end (writer code crlf)
  "Write to WRITER the end a line had, by CODE, the octets between its text's
end and the next line's start (SCAN-LINES): none for 0, a newline for 1, and
for 2 a carriage return and a newline when CRLF is true, else a newline."
  (let ((newline (if crlf
                     (load-time-value (coerce #(13 10) 'tuatara.text:octets) t)
                     (load-time-value (coerce #(10) 'tuatara.text:octets) t))))
    (case code
      (0 nil)
      (1 (tuatara.io:writer-octets writer newline (1- (length newline)) (length newline)))
      (t (tuatara.io:writer-octets writer newline 0 (length newline))))))

(defun sort-on-disk (job rope start end key reverse crlf fold directory)
  "Sort the lines of ROPE from START below END as the pool's workers can, a
piece at a time into run files, then merge them into an unnamed file: the rope
over it, and how many lines there are. NIL when JOB was stopped."
  (let* ((pieces (tuatara.text:rope-line-pieces rope start end *sort-piece*))
         (runs (coerce (tuatara.runtime:run-tasks
                        (map 'simple-vector (lambda (piece) (lambda () (sort-piece job rope piece key reverse crlf fold directory)))
                             pieces)
                        :parallelism (max 1 (min 8 (- (tuatara.runtime:cpu-count) 2))) :discard #'discard-run)
                       'list)))
    (if (or (car (sort-job-cancel job)) (some #'null runs))
        (progn (dolist (run runs) (when run (discard-run run))) nil)
        (with-scratch-file (fd directory)
          (let* ((writer (tuatara.io:make-writer fd))
                 ;; the line ends stay where they were: place I takes the end of line I
                 (ends (let ((all (make-array (reduce #'+ runs :key #'run-lines) :element-type '(unsigned-byte 2))) (at 0))
                         (dolist (run runs all) (replace all (run-ends run) :start1 at) (incf at (run-lines run)))))
                 (slot 0))
            (merge-runs job runs
                        (lambda (cursor)
                          (tuatara.io:writer-sap writer (cursor-sap cursor) (cursor-text cursor) (+ (cursor-text cursor) (cursor-length cursor)))
                          (write-kept-line-end writer (aref ends slot) crlf)
                          (sb-ext:atomic-incf (car (sort-job-progress job)) (+ 1 (cursor-length cursor)))
                          (incf slot))
                        (eq (first key) :numeric) reverse fold)
            (tuatara.io:writer-flush writer)
            (unless (car (sort-job-cancel job))
              (values (file-rope fd (tuatara.io:writer-length writer)) (length ends))))))))

(defun keep-piece (job rope piece crlf files marks-for)
  "Write the lines of PIECE, (START END) of ROPE, that MARKS-FOR (a function of
the piece's octets, starts and ends, returning a bit vector of the lines to
delete and how many go) leaves, at the end of this thread's file among FILES
\(PIECE-WRITER). Returns the writer, where in its file the piece's lines begin
and end, the lines deleted and the piece's lines; NIL when JOB was stopped.
The piece counts twice in the job's progress: read and marked, then written."
  (unless (car (sort-job-cancel job))
    (let ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece))))
      (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
        (multiple-value-bind (marks count) (funcall marks-for octets starts ends)
          (sb-ext:atomic-incf (car (sort-job-progress job)) (length octets))
          (let* ((writer (piece-writer files))
                 (from (tuatara.io:writer-length writer)))
            (each-kept-line octets starts nexts marks
                            (lambda (octets start end) (tuatara.io:writer-octets writer octets start end)))
            (sb-ext:atomic-incf (car (sort-job-progress job)) (length octets))
            (list writer from (tuatara.io:writer-length writer) count (length starts))))))))

(defun keep-pieces (job rope pieces crlf directory marks-for-piece)
  "Write the lines of each of PIECES of ROPE that the function
MARKS-FOR-PIECE gives for its index leaves (KEEP-PIECE), the pool's workers
taking a piece each, into a file a worker in DIRECTORY (PIECE-FILES).
Returns the rope of what is kept and how many lines were deleted; NIL when
JOB was stopped."
  (let ((files (make-piece-files directory)))
    (unwind-protect
         (let* ((index -1)
                (outputs (coerce (tuatara.runtime:run-tasks
                                  (map 'simple-vector
                                       (lambda (piece)
                                         (let ((marks-for (funcall marks-for-piece (incf index))))
                                           (lambda () (keep-piece job rope piece crlf files marks-for))))
                                       pieces)
                                  :parallelism (max 1 (- (tuatara.runtime:cpu-count) 2)))
                                 'list)))
           (unless (or (car (sort-job-cancel job)) (some #'null outputs))
             (values (apply #'tuatara.text:splice-rope (tuatara.text:make-rope) 0 0 (piece-files-ropes files outputs))
                     (reduce #'+ outputs :key #'fourth))))
      ;; the files not mapped by then: stopped, or failed
      (discard-piece-files files))))

(defun dedupe-on-disk (job rope start end reverse keep-blanks crlf directory)
  "Delete all but the first (the last with REVERSE) of identical lines of ROPE
from START below END: the lines are sorted a piece at a time into runs, merged
to find the lines that repeat, and the region written again without them.
Returns the rope and how many lines were deleted; NIL when JOB was stopped.
The region counts four times in the job's progress: sorted, merged, and twice
as it is written again (KEEP-PIECE)."
  (setf (sort-job-total job) (* 4 (max 1 (- end start))))
  (let* ((pieces (tuatara.text:rope-line-pieces rope start end *sort-piece*))
         (runs (coerce (tuatara.runtime:run-tasks
                        (map 'simple-vector (lambda (piece) (lambda () (sort-piece job rope piece (list :line) nil crlf nil directory)))
                             pieces)
                        :parallelism (max 1 (min 8 (- (tuatara.runtime:cpu-count) 2))) :discard #'discard-run)
                       'list)))
    (when (or (car (sort-job-cancel job)) (some #'null runs))
      (dolist (run runs) (when run (discard-run run)))
      (return-from dedupe-on-disk nil))
    (let* ((firsts (let ((at 0)) (map 'vector (lambda (run) (prog1 at (incf at (run-lines run)))) runs)))
           (marks (make-array (reduce #'+ runs :key #'run-lines) :element-type 'bit :initial-element 0))
           (count 0)
           (previous nil))
      ;; equal lines come together, in their first order: all but the first (or last) are marked
      (merge-runs job runs
                  (lambda (cursor)
                    (let ((line (+ (aref firsts (cursor-index cursor)) (cursor-line cursor)))
                          (length (cursor-length cursor)))
                      (unless (and keep-blanks (zerop length))
                        (if (and previous
                                 (zerop (sap-compare (cursor-sap cursor) (cursor-text cursor) (+ (cursor-text cursor) length)
                                                     (first previous) (second previous) (third previous) nil)))
                            (progn (incf count)
                                   (if reverse
                                       (progn (setf (sbit marks (fourth previous)) 1)
                                              (setf previous (list (cursor-sap cursor) (cursor-text cursor)
                                                                   (+ (cursor-text cursor) length) line)))
                                       (setf (sbit marks line) 1)))
                            (setf previous (list (cursor-sap cursor) (cursor-text cursor)
                                                 (+ (cursor-text cursor) length) line))))
                      (sb-ext:atomic-incf (car (sort-job-progress job)) (1+ length))))
                  nil nil nil)
      (when (car (sort-job-cancel job)) (return-from dedupe-on-disk nil))
      ;; write the region again without the marked lines, a piece at a time
      (let ((firsts (coerce firsts 'simple-vector)))
        (multiple-value-bind (kept gone)
            (keep-pieces job rope pieces crlf directory
                         (lambda (index)
                           (let ((first (svref firsts index)))
                             (lambda (octets starts ends)
                               (declare (ignore octets ends))
                               (let ((local (make-array (length starts) :element-type 'bit)))
                                 (dotimes (i (length starts)) (setf (sbit local i) (sbit marks (+ first i))))
                                 (values local (count 1 local)))))))
          (declare (ignore gone))
          (and kept (values kept count)))))))

(defun reverse-on-disk (job rope start end crlf directory)
  "Reverse the order of the lines of ROPE from START below END into an unnamed
file, the pieces from the last, each piece's lines from its last, the line ends
staying where they were. The rope over it and how many lines there are; NIL
when JOB was stopped."
  (let* ((pieces (tuatara.text:rope-line-pieces rope start end *sort-piece*))
         ;; the line ends in order, first: place I keeps the end of line I
         (ends (let ((all '()))
                 (dolist (piece pieces (apply #'concatenate '(vector (unsigned-byte 2)) (nreverse all)))
                   (let ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece))))
                     (multiple-value-bind (starts line-ends nexts) (scan-lines octets crlf)
                       (declare (ignore starts))
                       (push (map '(vector (unsigned-byte 2)) #'- nexts line-ends) all))))))
         (slot 0))
    (with-scratch-file (fd directory)
      (let ((writer (tuatara.io:make-writer fd)))
        (dolist (piece (reverse pieces))
          (when (car (sort-job-cancel job)) (return-from reverse-on-disk nil))
          (let ((octets (tuatara.text:rope-octets rope :start (first piece) :end (second piece))))
            (multiple-value-bind (starts line-ends) (scan-lines octets crlf)
              (loop for i from (1- (length starts)) downto 0
                    do (tuatara.io:writer-octets writer octets (aref starts i) (aref line-ends i))
                       (write-kept-line-end writer (aref ends slot) crlf)
                       (incf slot)))
            (sb-ext:atomic-incf (car (sort-job-progress job)) (* 2 (length octets)))))
        (tuatara.io:writer-flush writer)
        (values (file-rope fd (tuatara.io:writer-length writer)) (length ends))))))

;;; Jobs

(defun sort-job-of (buffer)
  "The sort or filter running in BUFFER, or NIL."
  (tuatara.runtime:running-job buffer 'sort-job))

(defun sort-progress (job)
  "How much of JOB is done, from 0 to 99 while it runs."
  (tuatara.runtime:running-percent (car (sort-job-progress job)) (sort-job-total job)))

(tuatara.runtime:define-job-words sort-job
  :doing (sort-job-words job)
  :description (or (sort-job-what job) (sort-job-words job))
  :verb (let ((words (sort-job-words job)))
    (cond ((string= words "Reversing") "reverse the lines")
          ((string= words "Deleting duplicates") "delete the duplicate lines")
          (t "sort"))))


(defmethod tuatara.runtime:job-percent ((job sort-job))
  "A region of *SORT-IN-MEMORY-LIMIT* octets or more has its result's lines
counted once written: sorting is the first half, counting the second."
  (tuatara.runtime:two-phase-percent (sort-progress job) (sort-job-counting job)
                                     :halves (>= (floor (sort-job-total job) 2) *sort-in-memory-limit*)))

(defmethod tuatara.runtime:job-state-words ((job sort-job))
  "What JOB is doing, reading or sorting, and how far it has gone, in words."
  (tuatara.runtime:two-phase-words (sort-job-words job) (sort-progress job) (sort-job-counting job)))


(defmethod tuatara.runtime:job-guarded-spans ((job sort-job))
  "The region a sort or filter works on, until an edit has touched it."
  (unless (sort-job-changed job)
    (list (cons (sort-job-region-start job) (sort-job-region-end job)))))

(defun sort-note-batch (job batch)
  "Change function while JOB runs: follow the region through BATCH, and note
when BATCH touches it (TUATARA.RUNTIME:SPAN-TOUCHED-P): an insertion at its
start or inside it, a deletion or replacement overlapping it, the whole text
swapped."
  (unless (sort-job-changed job)
    (let ((region (tuatara.buffer:span-after-batch (cons (sort-job-region-start job) (sort-job-region-end job)) batch)))
      (if region
          (setf (sort-job-region-start job) (car region) (sort-job-region-end job) (cdr region))
          (setf (sort-job-changed job) t)))))

(defun lines-done-words (words buffer start end)
  "WORDS (\"Sorting\") followed by what they are done to: the whole buffer
when START and END take in all of BUFFER, else the selection's lines."
  (format nil "~a ~:[the selection's lines~;the whole buffer~]" words
          (and (zerop start) (= end (tuatara.buffer:buffer-size buffer)))))

(defun start-sort (buffer start end words work)
  "Run WORK, a function of the job, BUFFER's rope, START and END returning the
octets or rope to replace START below END and what to say then, as a job of
BUFFER (TUATARA.RUNTIME:START-JOB-THREAD), with *SORT-CARRIED-SETTINGS* bound
there as they are here. Waits up to *SORT-WAIT* seconds and takes it in if it
is done by then, else leaves it running with its progress in the mode line,
the editor woken when it ends. Returns the job, or
:QUEUED when another job runs or waits in BUFFER, or its lines are still
being counted: it then waits its turn, START and END followed meanwhile."
  (let ((what (lines-done-words words buffer start end)))
    (when (queue-if-busy buffer what (list (cons start :before) (cons end :after))
                         (lambda (start end) (start-sort buffer start end words work))
                         :counted t)
      (return-from start-sort :queued))
    (tuatara.buffer:ensure-indexed buffer)
    (multiple-value-bind (rope base-start base-end since) (job-begins-where-given buffer start end :untouched t)
    (let ((job (%make-sort-job :buffer buffer :words words :what what :region-start start :region-end end
                               :base rope :base-start base-start :base-end base-end :since since
                               :total (* 2 (max 1 (- end start))))))
      (let ((function (lambda (b batch) (declare (ignore b)) (sort-note-batch job batch))))
        (setf (sort-job-change-function job) function)
        (push function (tuatara.buffer:buffer-change-functions buffer)))
      (tuatara.runtime:start-job-thread
       job (tuatara.runtime:carry-settings
            *sort-carried-settings*
            (lambda (job)
              (multiple-value-bind (result message) (funcall work job rope base-start base-end)
                ;; a result written to files has its lines counted here,
                ;; all of it at once on nine cores in ten, its progress shown
                (when (and (typep result 'tuatara.text:rope) (not (tuatara.text:rope-indexed-p result))
                           (not (car (sort-job-cancel job))))
                  (let ((counting (cons 0 0)))
                    (setf (sort-job-counting job) counting
                          result (tuatara.text:index-rope result :progress counting :cancel (sort-job-cancel job)))))
                (if (or (null result) (car (sort-job-cancel job)))
                    (setf (sort-job-status job) :stopped)
                    (setf (sort-job-result job) result (sort-job-message job) message
                          (sort-job-status job) :done)))))
       :name "tuatara sort" :details (format nil "octets ~:d to ~:d" start end))
      (take-in-soon job *sort-wait* words)
      job))))

(defmethod tuatara.runtime:job-take-in ((job sort-job))
  "Put JOB's result in its buffer as one undoable step, on the editor's thread,
unless the region changed meanwhile or the buffer may no longer take it
\(RESULT-REFUSED-P); or say why there is none."
  (let ((buffer (sort-job-buffer job)))
    (setf (tuatara.buffer:buffer-change-functions buffer)
          (remove (sort-job-change-function job) (tuatara.buffer:buffer-change-functions buffer)))
    (when (live-buffer-p buffer)
      (ecase (sort-job-status job)
        ((:stopped :failed) (message "~a" (tuatara.runtime:job-ending-words job)))
        (:done
         (cond
           ((sort-job-changed job)
            (message "The lines changed while ~(~a~) them; nothing was changed" (sort-job-words job)))
           ((result-refused-p buffer (format nil "the result of ~(~a~)" (sort-job-words job))
                              (- (let ((result (sort-job-result job)))
                                   (if (tuatara.text:rope-p result) (tuatara.text:rope-bytes result) (length result)))
                                 (- (sort-job-region-end job) (sort-job-region-start job)))))
           (t
             (let* ((start (sort-job-region-start job)) (end (sort-job-region-end job))
                    (result (sort-job-result job))
                    (point (tuatara.buffer:buffer-point buffer)))
               (tuatara.display:call-keeping-window-lines
                (buffer-windows buffer) start end
                (lambda ()
                  (let ((same (and (vectorp result)
                                   (equalp result (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end))))
                        (result (if (vectorp result) (tuatara.text:make-rope result) result)))
                    (flet ((spliced (rope from to)
                             ;; ROPE with the region FROM below TO in it sorted, and maps to and from it
                             (let ((delta (- (tuatara.text:rope-bytes result) (- to from))))
                               (values (tuatara.text:splice-rope rope from to result)
                                       (lambda (p bias) (declare (ignore bias)) (cond ((< p from) p) ((>= p to) (+ p delta)) (t from)))
                                       (lambda (p bias) (declare (ignore bias)) (cond ((< p from) p) ((>= p (+ to delta)) (- p delta)) (t from)))))))
                      (unless same
                        ;; standing where the job began, before the changes made meanwhile elsewhere
                        (multiple-value-bind (final final-forward final-backward)
                            (spliced (tuatara.buffer:buffer-text buffer) start end)
                          (unless (multiple-value-call #'tuatara.buffer:buffer-change-where-begun
                                    buffer (sort-job-since job) (sort-job-base job)
                                    (spliced (sort-job-base job) (sort-job-base-start job) (sort-job-base-end job))
                                    final final-forward final-backward)
                            (tuatara.buffer:buffer-swap-text buffer final final-forward final-backward))))))))
               (when (<= start point end)
                 (setf (tuatara.buffer:buffer-point buffer) start))
               (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
               (message "~a" (sort-job-message job))))))))))

;;; The commands

(defun sort-lines-by (buffer start end key reverse words done)
  "Sort BUFFER's lines from START below END by KEY (see LINE-KEYS), in reverse
with REVERSE, in memory or on disk by size; DONE is the past tense for the
message (\"Sorted\")."
  (let ((crlf (buffer-crlf-p buffer)) (fold *sort-fold-case*)
        (directory (tuatara.io:buffer-scratch-directory buffer))
        (limit *sort-in-memory-limit*))
    (start-sort buffer start end words
                (lambda (job rope start end)
                  (if (< (- end start) limit)
                      (let ((octets (tuatara.text:rope-octets rope :start start :end end)))
                        (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
                          (multiple-value-bind (key-starts key-ends numbers) (line-keys octets starts ends key)
                            (values (arrange-lines octets starts ends nexts
                                                   (key-order octets key-starts key-ends numbers key reverse fold))
                                    (format nil "~a ~:d line~:p" done (length starts))))))
                      (multiple-value-bind (rope lines)
                          (if (eq (first key) :index)
                              (reverse-on-disk job rope start end crlf directory)
                              (sort-on-disk job rope start end key reverse crlf fold directory))
                        (and rope (values rope (format nil "~a ~:d line~:p" done lines)))))))))

(defun field-argument ()
  "The field the prefix argument names for sort-fields: the first by default."
  (let ((arg (prefix-argument)))
    (if (or (null arg) (not (integerp arg)) (zerop arg)) 1 arg)))

(define-command sort-lines ()
  "Sort the lines the selection touches by their text: by the codes of their
characters, the case of ASCII letters ignored when *SORT-FOLD-CASE* is true.
With a prefix argument, in descending order. Lines that compare equal keep
their order (reversed with the rest in descending order). Without a selection,
does nothing and says so; a large selection is sorted in the background."
  (with-selected-lines (b start end 'sort-lines)
    (sort-lines-by b start end (list :line) (sort-descending-p) "Sorting" "Sorted")))

(define-command reverse-region ()
  "Reverse the order of the lines the selection touches. Without a selection,
does nothing and says so."
  (with-selected-lines (b start end 'reverse-region)
    (sort-lines-by b start end (list :index) t "Reversing" "Reversed")))

(defun record-field-order (octets ends crlf each field numeric reverse fold)
  "The records of OCTETS, whole records ending as ENDS says (RECORD-ENDS), and
their order by FIELD (EACH: a RECORD-FIELDS-EACH; NIL for the whole record),
as text, or with NUMERIC by the number its value begins with (FIELD-NUMBER),
a record without the field taking an empty value: two values, the records as
\(START END) and the order, as KEY-ORDER gives it."
  (let* ((rope (tuatara.text:octets->rope octets))
         (size (length octets))
         (holder (list (make-string 256)))
         (keys (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (records '()) (starts '()) (finishes '()) (numbers '()))
    (loop with at = 0
          while (< at size)
          do (multiple-value-bind (text-end after) (record-bounds rope at ends)
               (let* ((length (decode-into octets at (record-text-end rope text-end crlf) holder))
                      (value (if field
                                 (or (funcall each (car holder) length field (lambda (string from to) (subseq string from to))) "")
                                 (subseq (car holder) 0 length)))
                      (key (tuatara.text:string-octets value)))
                 (push (list at after) records)
                 (push (fill-pointer keys) starts)
                 (loop for octet across key do (vector-push-extend octet keys))
                 (push (fill-pointer keys) finishes)
                 (when numeric (push (field-number key 0 (length key)) numbers))
                 (setf at after))))
    (let ((key-octets (coerce keys 'tuatara.text:octets)))
      (values (nreverse records)
              (key-order key-octets (coerce (nreverse starts) '(simple-array fixnum (*))) (coerce (nreverse finishes) '(simple-array fixnum (*)))
                         (and numeric (coerce (nreverse numbers) '(simple-array double-float (*))))
                         (list :field) reverse fold)))))

(defun sort-records-by-field (buffer start end command numeric)
  "Sort the records of BUFFER, a file of records, that the lines from START
below END touch, its header aside, by a field asked for (READ-RECORD-FIELD),
as text, or with NUMERIC by number: each record whole, though it holds line
breaks (RECORD-ENDS). Worked in memory (SORT-IN-MEMORY-ONLY). COMMAND names
the command in messages."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (ends (record-ends buffer))
         (crlf (buffer-crlf-p buffer))
         (fields (mode-record-fields (buffer-major-mode buffer) buffer))
         (field (and fields (read-record-field buffer fields "Sort by the column: ")))
         (each (and fields (record-fields-each fields)))
         (reverse (sort-descending-p)) (fold *sort-fold-case*)
         (newline (tuatara.text:string-octets (tuatara.buffer:buffer-newline buffer)))
         (memo (make-record-memo))
         (first (if (list-header-p buffer) (records-start rope 1 ends) 0))
         ;; the whole records the lines touch
         (low (max first (line-record memo rope start (min (tuatara.text:rope-bytes rope) (1+ (tuatara.text:rope-line-end rope start))) ends)))
         (high (max low (nth-value 2 (line-record memo rope (tuatara.text:rope-line-start rope (max start (1- end))) end ends)))))
    (if (>= low high)
        (message "No records to sort but the header: ~(~a~) was not run" (sort-command-name command))
        (sort-in-memory-only buffer low high command "Sorting"
                             (lambda (octets)
                               (multiple-value-bind (records order)
                                   (record-field-order octets ends crlf each field numeric reverse fold)
                                 (values (arrange-records octets records order newline)
                                         (format nil "Sorted ~:d record~:p" (length records)))))))))

(define-command sort-fields ()
  "Sort the lines the selection touches by one field of each, as text: fields
are separated by spaces and tabs, and the prefix argument names the field (the
first by default; a negative one counts from the end of the line). A line with
too few fields stops the sort. In a file of records, a CSV, a log, JSON Lines
and the others \\[list-show-records] names, the records are sorted instead by
the column asked for, as \\[list-show-records] asks for it, each record whole
and the header line staying first; a record without the column sorts as
empty. Without a selection, does nothing and says so."
  (let ((field (field-argument)))
    (with-selected-lines (b start end 'sort-fields)
      (if (records-file-p b)
          (sort-records-by-field b start end 'sort-fields nil)
          (sort-lines-by b start end (list :field field) *sort-descending* "Sorting" "Sorted")))))

(define-command sort-numeric-fields ()
  "Sort the lines the selection touches by the number at the start of one
field of each (the prefix argument's, as for sort-fields): 0x and hexadecimal
digits, or a number in *SORT-NUMERIC-BASE*, decimal by default and then with a
fraction and exponent; a field without a number counts as 0. A leading zero
does not make a number octal. In a file of records, the column is asked for
and the records sorted whole, as sort-fields sorts them. Without a
selection, does nothing and says so."
  (let ((field (field-argument)))
    (with-selected-lines (b start end 'sort-numeric-fields)
      (if (records-file-p b)
          (sort-records-by-field b start end 'sort-numeric-fields t)
          (sort-lines-by b start end (list :numeric field) *sort-descending* "Sorting" "Sorted")))))

(define-command sort-columns ()
  "Sort the lines from the one where the selection starts to the one where it
ends by the text between the columns of its two ends, as text; with a prefix
argument, in descending order. A tab counts to the next tab stop (every
*tab-width* columns, or as set for the buffer's mode), a wide character two
columns. Without a selection, does nothing and says so."
  (with-selected-lines (b start end 'sort-columns :columns t)
    (multiple-value-bind (a z) (tuatara.buffer:buffer-region b)
      (let ((c1 (tuatara.display:buffer-screen-column b a))
            (c2 (tuatara.display:buffer-screen-column b z)))
        (sort-lines-by b start end (list :columns (min c1 c2) (max c1 c2) (buffer-tab-width b))
                       (sort-descending-p) "Sorting" "Sorted")))))

(defun sort-in-memory-only (buffer start end command words work)
  "Run WORK, a function of the region's octets giving the octets that replace
it and a message, as a sort job of BUFFER from START below END, when the region
is small enough to handle in memory; else say that COMMAND is not done on so
much yet."
  (if (>= (- end start) *sort-in-memory-limit*)
      (message "~(~a~) works in memory, up to ~:d MB; the selection is ~:d MB" (sort-command-name command)
               (floor *sort-in-memory-limit* (* 1024 1024)) (floor (- end start) (* 1024 1024)))
      (start-sort buffer start end words
                  (lambda (job rope start end)
                    (declare (ignore job))
                    (funcall work (tuatara.text:rope-octets rope :start start :end end))))))

(define-command sort-paragraphs ()
  "Sort the paragraphs the selection touches by their text: paragraphs are
separated by blank lines, which stay where they were. With a prefix argument, in
descending order. Without a selection, does nothing and says so."
  (let ((reverse (sort-descending-p)) (fold *sort-fold-case*))
    (with-selected-lines (b start end 'sort-paragraphs)
      (let ((newline (tuatara.text:string-octets (tuatara.buffer:buffer-newline b))) (crlf (buffer-crlf-p b)))
        (sort-in-memory-only b start end 'sort-paragraphs "Sorting"
                             (lambda (octets)
                               (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
                                 (let ((records (paragraph-records octets starts ends nexts)))
                                   (values (arrange-records octets records (record-order octets records reverse fold) newline)
                                           (format nil "Sorted ~:d paragraph~:p" (length records)))))))))))

(define-command sort-pages ()
  "Sort the pages the selection touches by their text, pages ending with a form
feed at the start of a line. With a prefix argument, in descending order.
Without a selection, does nothing and says so."
  (let ((reverse (sort-descending-p)) (fold *sort-fold-case*))
    (with-selected-lines (b start end 'sort-pages)
      (sort-in-memory-only b start end 'sort-pages "Sorting"
                           (lambda (octets)
                             (let ((records (page-records octets)))
                               (values (arrange-records octets records (record-order octets records reverse fold) nil)
                                       (format nil "Sorted ~:d page~:p" (length records)))))))))

(define-command sort-regexp-fields ()
  "Sort the matches of a regular expression within the selection: asks for the
pattern of the records, then for their key: \\& for the whole record, \\1 to \\9
for a group of the record's pattern, or a pattern whose first match in the
record is its key (an empty key when there is none). Keys compare as text; with
a prefix argument, in descending order. The text between the records stays
where it was. Without a selection, does nothing and says so."
  (let ((b (current-buffer)) (reverse (sort-descending-p)) (fold *sort-fold-case*))
    (multiple-value-bind (start end) (and (tuatara.buffer:buffer-mark-active-p b) (tuatara.buffer:buffer-region b))
      (if (not (and start (< start end)))
          (message "No selection: ~(~a~) was not run" (sort-command-name 'sort-regexp-fields))
          (let* ((record (multiple-value-call #'pattern-regex (read-pattern "sort the records")))
                 (key (multiple-value-bind (text regex) (read-pattern "sort them by the key" :initial "\\&")
                        ;; \& and \1 to \9 name the record or its group, whichever the kind
                        (if (cl-ppcre:scan "^\\\\[&1-9]$" text) text (pattern-regex text regex)))))
            (if (zerop (length record))
                (message "No pattern: ~(~a~) was not run" (sort-command-name 'sort-regexp-fields))
                (sort-in-memory-only b start end 'sort-regexp-fields "Sorting"
                                     (lambda (octets)
                                       (multiple-value-bind (text count)
                                           (sort-regexp-text (tuatara.text:octets-string octets) record key reverse fold)
                                         (values (tuatara.text:string-octets text)
                                                 (format nil "Sorted ~:d record~:p" count)))))))))))

(defmacro define-reverse-sort (name command doc)
  "Define NAME, a command running COMMAND in descending order, with DOC."
  `(define-command ,name ()
     ,doc
     (let ((*sort-descending* t) (*sort-command* ',name))
       (,command))))

(define-reverse-sort sort-lines-reverse sort-lines
  "Sort the lines the selection touches by their text in descending order, as
\\[sort-lines] does with a prefix argument: lines that compare equal come in
reverse order too. Without a selection, does nothing and says so.")

(define-reverse-sort sort-fields-reverse sort-fields
  "Sort the lines the selection touches by one field of each, as text, in
descending order: as \\[sort-fields], the prefix argument naming the field (a
negative one counting from the end of the line).")

(define-reverse-sort sort-numeric-fields-reverse sort-numeric-fields
  "Sort the lines the selection touches by the number at the start of one
field of each, largest first: as \\[sort-numeric-fields], the prefix argument
naming the field.")

(define-reverse-sort sort-columns-reverse sort-columns
  "Sort the lines from the one where the selection starts to the one where it
ends by the text between the columns of its two ends, in descending order: as
\\[sort-columns] does with a prefix argument.")

(define-reverse-sort sort-paragraphs-reverse sort-paragraphs
  "Sort the paragraphs the selection touches by their text in descending
order: as \\[sort-paragraphs] does with a prefix argument.")

(define-reverse-sort sort-pages-reverse sort-pages
  "Sort the pages the selection touches by their text in descending order: as
\\[sort-pages] does with a prefix argument.")

(define-reverse-sort sort-regexp-fields-reverse sort-regexp-fields
  "Sort the matches of a regular expression within the selection by their
keys in descending order: as \\[sort-regexp-fields] does with a prefix
argument, asking for the records' pattern and their key.")

(define-command delete-duplicate-lines ()
  "Delete all but one of each set of identical lines the selection touches:
the first of them is kept, with \\[universal-argument] the last; with
\\[universal-argument] \\[universal-argument] only a line identical to the one
before it goes, which is quicker for sorted text; with
\\[universal-argument] \\[universal-argument] \\[universal-argument] blank lines
are all kept. Says how many were deleted. Without a selection, does nothing and
says so; a large selection is handled in the background."
  (let* ((arg (prefix-argument))
         (reverse (eql arg 4)) (adjacent (eql arg 16)) (keep-blanks (eql arg 64)))
    (with-selected-lines (b start end 'delete-duplicate-lines)
      (let ((crlf (buffer-crlf-p b)) (limit *sort-in-memory-limit*)
            (directory (tuatara.io:buffer-scratch-directory b)))
        (flet ((report (count)
                 (format nil "Deleted ~:d ~:[~;adjacent ~]duplicate line~p~:[~; backward~]" count adjacent count reverse)))
          (start-sort b start end "Deleting duplicates"
                      (lambda (job rope start end)
                        (cond ((< (- end start) limit)
                               (let ((octets (tuatara.text:rope-octets rope :start start :end end)))
                                 (multiple-value-bind (starts ends nexts) (scan-lines octets crlf)
                                   (multiple-value-bind (marks count) (duplicate-marks octets starts ends reverse adjacent keep-blanks)
                                     (values (kept-lines octets starts nexts marks) (report count))))))
                              (adjacent
                               ;; lines repeat only next to each other: each piece alone, and the
                               ;; first line of a piece against the last of the one before
                               (let ((pieces (tuatara.text:rope-line-pieces rope start end *filter-piece*)))
                                 (multiple-value-bind (kept count)
                                     (keep-pieces job rope pieces crlf directory
                                                  (lambda (index)
                                                    (let ((before (and (plusp index) (rope-line-before rope (first (elt pieces index)) crlf))))
                                                      (lambda (octets starts ends)
                                                        (multiple-value-bind (marks count) (duplicate-marks octets starts ends nil t keep-blanks)
                                                          (when (and before (plusp (length starts))
                                                                     (not (and keep-blanks (= (aref starts 0) (aref ends 0))))
                                                                     (equalp before (subseq octets (aref starts 0) (aref ends 0))))
                                                            (setf (sbit marks 0) 1)
                                                            (incf count))
                                                          (values marks count))))))
                                   (and kept (values kept (report count))))))
                              (t (multiple-value-bind (kept count)
                                     (dedupe-on-disk job rope start end reverse keep-blanks crlf directory)
                                   (and kept (values kept (report count)))))))))))))

;;; Lines given a new text or none

(defvar *rewrite-line-limit* (* 16 1024 1024)
  "Octets of a line a filter trying a column reads (LINES-DECODED): a longer
line is tried on its first that many, so that a line of gigabytes is never
held in memory whole.")

(defun decode-into (octets start end holder)
  "Decode OCTETS from START below END into the string in the car of HOLDER,
a larger one put there first when it may not hold them; the number of
characters."
  (declare (type tuatara.text:octets octets) (type fixnum start end) (optimize speed))
  (let ((needed (- end start)))
    (declare (type fixnum needed))
    (when (< (length (the (simple-array character (*)) (car holder))) needed)
      (setf (car holder) (make-string (min (* 2 needed) (+ needed 1048576)))))
    (let ((string (car holder)) (n 0) (i start))
      (declare (type (simple-array character (*)) string) (type fixnum n i))
      (loop while (< i end)
            do (let ((b (aref octets i)))
                 (if (< b #x80)
                     (setf (schar string n) (code-char b) i (1+ i))
                     (multiple-value-bind (code length) (tuatara.text:octets-decode octets i end)
                       (declare (type fixnum code length))
                       (setf (schar string n) (code-char code) i (+ i length))))
                 (incf n)))
      n)))

(defun thread-own (owned make)
  "This thread's own thing among OWNED, a cons whose car lists (THREAD .
THING): made by calling MAKE the first time a thread asks."
  (let ((thread sb-thread:*current-thread*))
    (or (cdr (assoc thread (car owned) :test #'eq))
        (let ((thing (funcall make)))
          (sb-ext:atomic-push (cons thread thing) (car owned))
          thing))))

(defun filtered-line (filter keep gone &key ends crlf)
  "Two values: a function deleting each line for START-LINE-JOB that FILTER
leaves, or with KEEP each it does not (LINES-MATCHING), and a function of
\(ROPE LOW HIGH HALT) to call first, working out which lines those are from
LOW below HIGH of ROPE, on all but two cores, as one scan of that text finds
them (LINES-MATCHING-EVERYWHERE), until HALT, a function of none, answers
true. The car of GONE counts the lines deleted, but not while a job is taken
in, when its lines are read again to follow positions onto its result. With
ENDS (RECORD-ENDS), the lines are those of records that may hold line breaks,
each kept or deleted with its record, the record counted once."
  (let ((runs (make-array 0 :element-type 'fixnum))
        (memos (list '()))
        (none (tuatara.text:make-octets 0)))
    (declare (type (simple-array fixnum (*)) runs))
    (values
     (lambda (function line line-end next rope block block-start block-end)
       (declare (ignore line-end block block-start block-end) (type fixnum line next))
       ;; the run ending after the line's start, found by halving
       (let* ((k (* 2 (tuatara.runtime:bisect (k 0 (ash (length runs) -1)) (<= (aref runs (1+ (* 2 k))) line))))
              (left (and (< k (length runs)) (<= (aref runs k) line))))
         (unless (eq keep left)
           (when (and (not tuatara.runtime:*job-taken-in*)
                      (or (null ends) (= line (line-record (thread-own memos #'make-record-memo) rope line next ends))))
             (sb-ext:atomic-incf (car gone)))
           (funcall function line next nil none))))
     (lambda (rope low high halt)
       (setf runs (lines-matching-everywhere filter rope (make-array 2 :element-type 'fixnum :initial-contents (list low high))
                                             crlf nil halt nil ends))))))

(defun fate-line (fate limit gone changed long &key ends crlf)
  "A function changing each line for START-LINE-JOB as FATE decides
\(REWRITE-LINES): the line's text decoded into a string each thread keeps,
FATE called on it, and the line deleted with its line end, given its new
text, or left as it is. A line of more than LIMIT octets is left as it is,
unread. The cars of GONE, CHANGED and LONG count the lines deleted, changed
and too long, but not while a job is taken in, when its lines are read again
to follow positions onto its result. With ENDS (RECORD-ENDS), the lines are
those of records that may hold line breaks: FATE, which then keeps or deletes
and changes nothing, is called on each record's whole text (RECORD-VERDICT),
and each of its lines is kept or deleted with it, the record counted once."
  (let ((holders (list '()))
        (memos (list '()))
        (none (tuatara.text:make-octets 0)))
    (lambda (function line line-end next rope block block-start block-end)
      (declare (type fixnum line line-end next))
      (let ((counting (not tuatara.runtime:*job-taken-in*))
            (holder (thread-own holders (lambda () (list (make-string 256)))))
            (memo (thread-own memos #'make-record-memo)))
        (multiple-value-bind (start text-end after)
            (if ends
                (line-record memo rope line next ends)
                (values line line-end next))
          (if (and (= start line) (= after next))
              (if (> (- line-end line) limit)
                  (when counting (sb-ext:atomic-incf (car long)))
                  (multiple-value-bind (octets at) (line-octets rope line line-end block block-start block-end)
                    (let* ((length (decode-into octets at (+ at (- line-end line)) holder))
                           (new (funcall fate (car holder) length)))
                      ;; a string grown for a long line is not kept for the short ones after it
                      (when (> (length (car holder)) 1048576)
                        (setf (car holder) (make-string 256)))
                      (cond ((null new)
                             (when counting (sb-ext:atomic-incf (car gone)))
                             (funcall function line next nil none))
                            ((stringp new)
                             (when counting (sb-ext:atomic-incf (car changed)))
                             (funcall function line line-end nil (tuatara.text:string-octets new)))))))
              ;; a line of a record over several lines: the record decided once, whole
              (unless (record-verdict memo rope start text-end crlf limit holder fate)
                (when (and counting (= start line)) (sb-ext:atomic-incf (car gone)))
                (funcall function line next nil none))))))))

(defun too-long-words (count)
  "What is said of COUNT lines too long to read, after what was done to the
others: \"; N lines too long to look at left as they are\", or \"\" for none."
  (format nil "~[~:;; ~:*~:d line~:p too long to look at left as ~:*~[~;it is~:;they are~]~]" count))

(defun rewrite-lines-here (buffer start end per-line)
  "Change the lines of BUFFER from START below END as PER-LINE says
\(START-LINE-JOB), at once on this thread, as one change to undo."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (octets (tuatara.text:rope-octets rope :start start :end end))
         (out (make-array (length octets) :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
         (pos 0))
    (multiple-value-bind (starts ends nexts) (scan-lines octets (buffer-crlf-p buffer))
      (dotimes (i (length starts))
        (funcall per-line
                 (lambda (a z groups new)
                   (declare (ignore groups))
                   (setf out (put-octets (put-octets out octets pos (- a start)) new 0 (length new))
                         pos (- z start)))
                 (+ start (aref starts i)) (+ start (aref ends i)) (+ start (aref nexts i))
                 rope octets start end)))
    (setf out (put-octets out octets pos (length octets)))
    (let ((result (coerce out 'tuatara.text:octets)))
      (unless (equalp result octets)
        (tuatara.buffer:buffer-replace buffer start end result)))))

(defun change-lines (buffer start end words per-line report &key prepare)
  "Change the lines of BUFFER from START below END, octets at line starts,
as PER-LINE says (START-LINE-JOB), PREPARE, when given, called first with
the text, where the lines start and end, and a function of none answering
true once the work is stopped: in the background on all but two cores, waited
for *SORT-WAIT* seconds so that quick work looks done at once, edits made to
them meanwhile kept; a buffer whose text never goes to disk (an encrypted
file's) at once instead, up to *REWRITE-THRESHOLD* octets. One step to undo
either way. REPORT, a function of the words saying how long the job took
\(NIL when done at once), gives what is said once done. WORDS names the work
\(\"Filtering\"). Returns the job, :QUEUED (QUEUE-IF-BUSY), or NIL. Refused
when search filters hide any of those lines (CHECK-HIDDEN-LINES)."
  (check-writable buffer)
  (check-hidden-lines buffer start end)
  (when (< start end)
    (let ((what (lines-done-words words buffer start end)))
      (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
      (cond ((tuatara.io:buffer-scratch-directory buffer)
             ;; TO is on the last line: END is that line's end, or the next one's start
             (let ((job (start-line-job buffer start (1- end) per-line
                                        :prepare (and prepare
                                                      (lambda (job)
                                                        (declare (notinline rewrite-rope rewrite-low rewrite-high rewrite-cancel))
                                                        (funcall prepare (rewrite-rope job) (rewrite-low job) (rewrite-high job)
                                                                 (lambda () (car (rewrite-cancel job))))))
                                        :doing words :what what
                                        :verb "change the lines" :leaves "the lines are as they were"
                                        :report (lambda (job status changes seconds kept)
                                                  (declare (ignore job changes))
                                                  (unless (eq status :stopped)
                                                    (funcall report (job-done-words seconds kept)))))))
               (unless (eq job :queued) (take-in-soon job *sort-wait* what))
               job))
            ((not (too-large-here-p start end words))
             (when prepare (funcall prepare (tuatara.buffer:buffer-text buffer) start end nil))
             (rewrite-lines-here buffer start end per-line)
             (message "~a" (funcall report nil))
             nil)))))

(defun rewrite-lines (buffer start end words fate report &key ends)
  "Give each line of BUFFER from START below END, octets at line starts, what
FATE gives it: FATE, a function of a string and how many of its first
characters are the line's text (without its line end), gives NIL to delete
the line, T to keep it, or a string, its new text; the string it is given is
used again for the next line. A line of more than *REWRITE-LINE-LIMIT* octets
is left as it is, FATE never asked. The lines are done as CHANGE-LINES does
them, in the background when large, one step to undo. FATE runs on other
threads, several at once, so it changes nothing outside itself. REPORT, a
function of how many lines went and how many changed, gives what is said
once done, followed by how many were too long to look at. WORDS names the
work (\"Rearranging\"). Returns the job, :QUEUED (QUEUE-IF-BUSY), or NIL. With
ENDS (RECORD-ENDS), FATE keeps or deletes records that may hold line breaks,
each whole (FATE-LINE)."
  (let ((gone (list 0)) (changed (list 0)) (long (list 0)))
    (change-lines buffer start end words
                  (fate-line fate *rewrite-line-limit* gone changed long :ends ends :crlf (buffer-crlf-p buffer))
                  (lambda (done)
                    (format nil "~a~@[ ~a~]~a" (funcall report (car gone) (car changed)) done (too-long-words (car long)))))))

(defun delete-filtered-lines (buffer start end words filter keep report &key ends)
  "Delete each line of BUFFER from START below END, octets at line starts,
that FILTER leaves, or with KEEP each it does not (FILTERED-LINE), as
CHANGE-LINES does it, in the background when large, one step to undo.
REPORT, a function of how many lines went, gives what is said once done.
WORDS names the work (\"Filtering\"). Returns the job, :QUEUED
\(QUEUE-IF-BUSY), or NIL. With ENDS (RECORD-ENDS), records that may hold
line breaks are kept or deleted whole."
  (let ((gone (list 0)))
    (multiple-value-bind (per-line prepare) (filtered-line filter keep gone :ends ends :crlf (buffer-crlf-p buffer))
      (change-lines buffer start end words per-line
                    (lambda (done) (format nil "~a~@[ ~a~]" (funcall report (car gone)) done))
                    :prepare prepare))))

(defun read-line-filter (buffer words start end)
  "Ask how to filter the lines of BUFFER from START below END, WORDS (keep
lines) heading the question: what of a line the pattern is tried on
\(FILTER-LINE-MATCHER, a column asked first in a file of records cut into
fields), then the pattern, asked as a search asks it, with how many lines it matches counted before the
prompt when it is tried on the whole line. Five values: the pattern, true
when it is a regular expression, the matcher, words naming what of a line it
tries or NIL, and the first line that may change, the one after a file of
records' header (LIST-HEADER-P), or NIL."
  (multiple-value-bind (matcher what) (filter-line-matcher buffer)
    (let ((first (and (records-file-p buffer) (list-header-p buffer) (records-start (tuatara.buffer:buffer-text buffer) 1 (record-ends buffer)))))
      (multiple-value-bind (pattern regex)
          (let ((counter (make-pattern-count))
                (lines (list (cons (max start (or first 0)) end))))
            (unwind-protect
                 (read-pattern (format nil "~a~@[ whose ~a holds~]" words what)
                               ;; how many lines the pattern matches, when it is tried on the whole line
                               :count (and (null what)
                                           (lambda (text regex)
                                             (stretch-pattern-count counter buffer lines text regex
                                                                    :ends (and (records-file-p buffer) (record-ends buffer))))))
              (stop-pattern-count counter)))
        (values pattern regex matcher what first)))))

(defun filter-lines (command keep &key whole (noun "line") (words (if keep "keep lines" "flush lines")))
  "Ask for a pattern and delete the lines the selection touches that it
matches, or with KEEP those it does not, in the background when large; what
of a line the pattern is tried on is asked first in a file of records cut
into fields (READ-LINE-FILTER, WORDS heading the question). With WHOLE,
every line when nothing is selected (of the part shown, in a narrowed
buffer). COMMAND names the command in messages,
NOUN what a line is called in them."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (selected-lines b)
      (when (and (null start) whole)
        (multiple-value-setq (start end) (tuatara.buffer:buffer-bounds b)))
      (if (null start)
          (message "No selection: ~(~a~) was not run" command)
          (multiple-value-bind (pattern regex matcher what first) (read-line-filter b words start end)
            (multiple-value-bind (filter why)
                (and (plusp (length pattern))
                     (make-search-filter pattern :regex regex :test (and what matcher) :crlf (buffer-crlf-p b)))
              (let ((start (max start (or first 0))))
                (cond ((zerop (length pattern)) (message "No pattern: ~(~a~) was not run" command))
                      ((null filter) (message "~a: ~(~a~) was not run" (string-capitalize why :end 1) command))
                      ((>= start end) (message "No lines to filter but the header: ~(~a~) was not run" command))
                      (t (delete-filtered-lines b start end "Filtering" filter keep
                                                (lambda (gone)
                                                  (format nil "Deleted ~:d ~:[matching~;non-matching~] ~a~p" gone keep noun gone))
                                                :ends (and (records-file-p b) (record-ends b))))))))))))

(define-command flush-lines ()
  "Delete the lines the selection touches in which a regular expression, asked
for in the minibuffer, finds a match: every line any part of a match lies
on, as one search of the text finds the matches, a match running over
several lines deleting them all; ^ and $ are a line's start and end, so that
^$ deletes the empty lines. Smart case, as in
search: a pattern with a capital letter, or naming a case ([[:upper:]],
\\p{Lu}), heeds case. Says how many were deleted.
In a file of records cut into columns, a CSV, a log, JSON Lines and the
others \\[list-show-records] names, a column is asked for first (nothing for
the whole line), the pattern tried on its value alone, and a header line
always stays. Without a selection, does nothing
and says so; a large selection is handled in the background."
  (filter-lines 'flush-lines nil))

(define-command keep-lines ()
  "Delete the lines the selection touches in which a regular expression, asked
for in the minibuffer, finds no match, keeping those it matches. The pattern is
found as for flush-lines, in a file of records on the
column asked for. Says how many were deleted. Without a selection, does nothing and says
so; a large selection is handled in the background."
  (filter-lines 'keep-lines t))

(defun records-file-p (buffer)
  "True when BUFFER is a file of records, a line each: a CSV or TSV, a log,
a JSON Lines file and the others *LINE-RECORD-MODES* names
\(FILTERS-HIDE-LINES-P)."
  (filters-hide-lines-p (buffer-major-mode buffer) buffer))

(define-command list-keep-records ()
  "Keep the records whose line a regular expression matches and delete the
rest, in a file of records, a line each: a CSV or TSV, a log, a JSON Lines
file, todo.txt and the others *LINE-RECORD-MODES* names, the column asked
for first as \\[list-show-records] asks. The lines the selection touches, or
every record; a header line always stays (\\[toggle-list-header]). Smart case, as in search. One step to undo; a large file
is filtered in the background. \\[list-show-records] hides the others
instead, deleting nothing."
  (if (records-file-p (current-buffer))
      (filter-lines 'list-keep-records t :whole t :noun "record" :words "keep records")
      (message "Not a file of records (a CSV, a log): list-keep-records was not run; keep-lines filters any lines")))

(define-command list-delete-records ()
  "Delete the records whose line a regular expression matches, keeping the
rest: asked for as \\[list-keep-records] asks, in the same files. A header
line always stays. \\[list-hide-records] hides them instead,
deleting nothing."
  (if (records-file-p (current-buffer))
      (filter-lines 'list-delete-records nil :whole t :noun "record" :words "delete records")
      (message "Not a file of records (a CSV, a log): list-delete-records was not run; flush-lines filters any lines")))

(defun sort-shading (job from to)
  "The region JOB, a sort or filter, is changing, from FROM below TO only, as
a range to shade in :JOB-QUEUED; none once an edit has touched it, which
gives up the result."
  (let ((start (max (sort-job-region-start job) from))
        (end (min (sort-job-region-end job) to)))
    (when (and (not (sort-job-changed job)) (< start end))
      (list (list start end :job-queued)))))

(defun sort-highlights (editor window rows)
  "Job highlight function: the region a sort or filter running in WINDOW's
buffer is changing, in ROWS only (SORT-SHADING)."
  (declare (ignore editor))
  (let ((job (sort-job-of (tuatara.display:window-buffer window))))
    (when (and job (plusp (length rows)))
      (sort-shading job (tuatara.display:row-start (aref rows 0))
                    (tuatara.display:row-end (aref rows (1- (length rows))))))))

(defun install-sorting ()
  "Show the region sorts and filters are changing."
  (pushnew 'sort-highlights *job-highlight-functions*))
