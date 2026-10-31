;;;; lisp/modes/csv.lisp --- Delimited data: a colour per column, the column at point.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Colouring reads only the rows on screen (and, for a wrapped line whose start
;;;; is above the window, that line's beginning), so a 100-million-line file costs
;;;; the same as a small one. Quoted fields may contain the delimiter; a quote
;;;; inside a quoted field is written twice, which toggles twice and so needs no
;;;; special case. The header lock shows the first line in the window's header
;;;; line, laid out and coloured the same way, a row of work a frame. Coloured
;;;; sparsely, as Emacs's csv-mode colours, only the separators, the quoted
;;;; fields and the lines that start with # are coloured.

(in-package #:tuatara.defaults)

(define-major-mode csv-mode text-mode
  "Delimited data such as comma-separated values: each column in its own colour,
cycling through eight (\\[rotate-colouring] colours fewer things, or none), and
the column at point with its header in the mode line, counted from 0 (col 0
the first).
Fields are separated by *CSV-SEPARATOR*, a comma unless set;
\\<csv-mode>\\[csv-define-separator] sets the separator for the buffer.
\\[toggle-list-header-lock] keeps the first line, the header, at the top of the
window as the rest scrolls under it. \\[search-filter-regex] hides the records
a pattern does not match, the mode line saying how many it leaves."
  :lighter "CSV" :extensions ("csv"))

(define-major-mode tsv-mode csv-mode
  "Tab-separated values: CSV mode with the tab as the delimiter."
  :lighter "TSV" :extensions ("tsv" "tab"))

(defvar *csv-colouring* nil
  "How CSV and TSV buffers are set to be coloured: :RICH each column in its own
colour, cycling through eight; :SPARSE only the
separators, the quoted fields and the lines that start with #; :NONE nothing; NIL, the default, as set for text-mode
or every buffer, else rich. Set it in the init file with
(set-colouring 'csv-mode :sparse); \\[rotate-colouring] in a CSV buffer goes on
to the next.")

(defun csv-colouring (mode)
  "The style a CSV or TSV buffer of major mode MODE (an instance) is coloured
in: as set for CSV, else for the nearest mode it is made from or every buffer
(COLOURING-RULE), else :RICH."
  (or *csv-colouring* (colouring-rule mode) :rich))

(defparameter *csv-faces*
  #(:csv-column-0 :csv-column-1 :csv-column-2 :csv-column-3
    :csv-column-4 :csv-column-5 :csv-column-6 :csv-column-7)
  "The faces columns cycle through.")

(defconstant +csv-scan-limit+ (* 1024 1024)
  "Most octets of one line scanned for colouring or for the column at point;
a longer line is coloured up to here.")

(defconstant +quote-octet+ 34 "The octet of the double quote that encloses fields.")

(defmacro do-csv-octets ((octet position quoted) (rope start end) &body body)
  "Run BODY for each octet of ROPE from START below END, read where the text is
kept and nothing copied: OCTET bound to it and POSITION to where it is.
QUOTED names a variable of the caller's, true inside double quotes: BODY sees
it as it was before the octet, and a double quote turns it over after. BODY
leaves the whole with RETURN, whose value the form then has; else NIL."
  (let ((node (gensym "NODE")) (from (gensym "FROM")) (to (gensym "TO")) (at (gensym "AT"))
        (i (gensym "I")) (visit (gensym "VISIT")))
    `(block nil
       (let ((,at ,start))
         (declare (type fixnum ,at))
         (flet ((,visit (,node ,from ,to)
                  (declare (type fixnum ,from ,to))
                  (loop for ,i of-type fixnum from ,from below ,to
                        do (let ((,octet (tuatara.text:piece-octet ,node ,i))
                                 (,position (+ ,at (- ,i ,from))))
                             (declare (type (unsigned-byte 8) ,octet) (type fixnum ,position) (ignorable ,position))
                             ,@body
                             (when (= ,octet +quote-octet+) (setf ,quoted (not ,quoted)))))
                  (incf ,at (- ,to ,from))))
           (declare (dynamic-extent #',visit))
           (when (< ,at ,end)
             (tuatara.text:map-leaf-pieces #',visit ,rope :start ,at :end ,end))))
       nil)))

(defun detect-delimiter (rope)
  "The octet that separates fields in ROPE: whichever of comma, semicolon, tab
and pipe occurs most often outside quotes in the first line (up to 64 KB);
comma when none does. True as a second value when one does."
  (let* ((reach (min (tuatara.text:rope-bytes rope) 65536))
         (end (or (tuatara.text:rope-find-line-end rope :end reach) reach))
         (counts (list (cons 44 0) (cons 59 0) (cons 9 0) (cons 124 0)))
         (quoted nil))
    (do-csv-octets (b at quoted) (rope 0 end)
      (unless quoted
        (let ((entry (assoc b counts))) (when entry (incf (cdr entry))))))
    (let ((best (reduce (lambda (a b) (if (> (cdr b) (cdr a)) b a)) counts)))
      (if (plusp (cdr best)) (values (car best) t) 44))))

(defvar *csv-separator* :detect
  "What separates the fields of the files CSV mode shows: :DETECT, the
default, takes whichever of comma, semicolon, tab and pipe the first line of
each file has most of (a comma while it has none, until one is typed there:
in an empty file, or one made empty); a character, such as #\\,
#\\; or #\\Tab, is taken always, whatever the file holds. Files ending in .tsv
or .tab are separated by tabs. \\<csv-mode>\\[csv-define-separator] sets the
separator for one buffer.")

(defvar *csv-header* t
  "True when the first line of the files CSV mode shows is their header,
naming the columns: always shown by search filters, never deleted by
\\[list-keep-records], its names offered for a column. NIL takes it as a
record like the rest, the columns offered by their numbers.
\\[toggle-list-header] says so for one buffer.")

(defparameter *csv-separator-names*
  '(("comma" . #\,) ("semicolon" . #\;) ("tab" . #\Tab) ("pipe" . #\|) ("colon" . #\:) ("space" . #\Space))
  "The separators \\<csv-mode>\\[csv-define-separator] offers by name.")

(defun separator-char (separator)
  "The character SEPARATOR stands for as a separator of fields: a character, a
string of one, or one of the names of *CSV-SEPARATOR-NAMES*; NIL for anything
else, and for a character that cannot separate fields: one outside ASCII, a
line end or the double quote."
  (let ((char (typecase separator
                (character separator)
                (string (if (= (length separator) 1)
                            (char separator 0)
                            (cdr (assoc separator *csv-separator-names* :test #'string-equal)))))))
    (and char (< (char-code char) 128) (not (member char '(#\Newline #\Return #\"))) char)))

(defun separator-octet (separator rope)
  "The octet that separates fields in ROPE by SEPARATOR, a value
*CSV-SEPARATOR* may have: its character; for :DETECT, what the first line has
most of (DETECT-DELIMITER), true as a second value when it has any; a comma for
anything else."
  (if (eq separator :detect)
      (detect-delimiter rope)
      (char-code (or (separator-char separator) #\,))))

(defun separator-name (octet)
  "The separator OCTET as it is named: comma, tab and the others of
*CSV-SEPARATOR-NAMES* by their names, another character as itself."
  (let ((named (rassoc (code-char octet) *csv-separator-names*)))
    (if named (car named) (string (code-char octet)))))

(defun csv-delimiter (buffer)
  "The code of the character that separates the fields of BUFFER, a CSV or TSV
buffer: as detected or set (\\<csv-mode>\\[csv-define-separator]), a comma when
it has none."
  (tuatara.buffer:buffer-local buffer :csv-delimiter 44))

(defun first-line-touched-p (rope batch)
  "True when BATCH, the change that made ROPE, touched its first line."
  (or (tuatara.text:batch-swap batch)
      (let ((first-end (or (tuatara.text:rope-find-line-end rope) (tuatara.text:rope-bytes rope))))
        (some (lambda (edit) (<= (tuatara.text:edit-start edit) first-end)) (tuatara.text:batch-edits batch)))))

(defun csv-note-change (buffer batch)
  "After an edit of BUFFER, whose separator is detected: while its first line
has none yet (an empty file, or one made empty), detect it again from what is
typed there."
  (let ((detecting (tuatara.buffer:buffer-local buffer :csv-detecting)))
    (when detecting
      (if (zerop (tuatara.buffer:buffer-size buffer))
          (setf (tuatara.buffer:buffer-local buffer :csv-detecting) :open)
          (when (and (eq detecting :open) (first-line-touched-p (tuatara.buffer:buffer-text buffer) batch))
            (multiple-value-bind (octet found) (detect-delimiter (tuatara.buffer:buffer-text buffer))
              (when found
                (setf (tuatara.buffer:buffer-local buffer :csv-delimiter) octet
                      (tuatara.buffer:buffer-local buffer :csv-detecting) :done))))))))

(defmethod mode-enter ((mode csv-mode) buffer)
  "Find BUFFER's separator (*CSV-SEPARATOR*) and follow its edits to find it
again while it is open."
  (call-next-method)
  (multiple-value-bind (octet found) (separator-octet *csv-separator* (tuatara.buffer:buffer-text buffer))
    (setf (tuatara.buffer:buffer-local buffer :csv-delimiter) octet
          (tuatara.buffer:buffer-local buffer :csv-detecting) (and (eq *csv-separator* :detect) (if found :done :open))))
  (pushnew 'csv-note-change (tuatara.buffer:buffer-change-functions buffer))
  (set-header-lock buffer (and *list-header-lock* (list-header-p buffer))))

(defmethod mode-exit ((mode csv-mode) buffer)
  "Stop following BUFFER's edits for its separator."
  (setf (tuatara.buffer:buffer-change-functions buffer) (remove 'csv-note-change (tuatara.buffer:buffer-change-functions buffer))
        (tuatara.buffer:buffer-local buffer :csv-detecting) nil)
  (call-next-method))

(defmethod mode-enter ((mode tsv-mode) buffer)
  "Take the tab as BUFFER's separator, whatever its text holds."
  (call-next-method)
  (setf (tuatara.buffer:buffer-local buffer :csv-delimiter) 9
        (tuatara.buffer:buffer-local buffer :csv-detecting) nil))

(defun csv-count-fields (rope start end delimiter field quoted)
  "The column index and quoting at the end of ROPE's octets from START below
END (at most +CSV-SCAN-LIMIT+ of them), FIELD and QUOTED being those at START,
as CSV-SCAN gives them: read where the text is kept, nothing copied."
  (declare (type fixnum start end field))
  (do-csv-octets (b at quoted) (rope start (min end (+ start +csv-scan-limit+)))
    (when (and (= b delimiter) (not quoted)) (incf field)))
  (values field quoted))

(defun csv-scan (rope start end delimiter field quoted &key (ranges t))
  "Scan ROPE from byte START below END (at most +CSV-SCAN-LIMIT+ octets) as
delimited data, FIELD and QUOTED being the column index and quoting at START.
Returns three values: the column ranges (START END FACE) when RANGES is true,
and the column index and quoting at the end of the scan."
  (declare (type fixnum start end field))
  (unless ranges
    (multiple-value-bind (field quoted) (csv-count-fields rope start end delimiter field quoted)
      (return-from csv-scan (values '() field quoted))))
  (let* ((stop (min end (+ start +csv-scan-limit+)))
         (out '())
         (field-start start))
    (declare (type fixnum stop field-start))
    (do-csv-octets (b here quoted) (rope start stop)
      (when (and (= b delimiter) (not quoted))
        (when (> here field-start)
          (push (list field-start here (aref *csv-faces* (mod field 8))) out))
        (incf field)
        (setf field-start (1+ here))))
    (when (> stop field-start)
      (push (list field-start stop (aref *csv-faces* (mod field 8))) out))
    (values (nreverse out) field quoted)))

(defun csv-sparse-scan (rope start end delimiter quoted)
  "Scan ROPE from byte START below END (at most +CSV-SCAN-LIMIT+ octets) as
delimited data coloured sparsely, QUOTED being the quoting at START. Returns
two values: the ranges (START END FACE) of its quoted fields, in :string, and of
its separators, in :csv-separator; and the quoting at the end of the scan."
  (declare (type fixnum start end))
  (let* ((stop (min end (+ start +csv-scan-limit+)))
         (out '())
         (opened (and quoted start)))
    (do-csv-octets (b at quoted) (rope start stop)
      (cond ((= b +quote-octet+)
             (if quoted
                 (progn (push (list opened (1+ at) :string) out)
                        (setf opened nil))
                 (setf opened at)))
            ((and (= b delimiter) (not quoted))
             (push (list at (1+ at) :csv-separator) out))))
    (when (and opened (< opened stop))
      (push (list opened stop :string) out))
    (values (nreverse out) quoted)))

(defconstant +csv-record-checkpoint+ 16384
  "Octets between the record starts kept along a long scan for the start of a
record (CSV-RECORD-START), so that a line above the one asked about is found
again by reading no more than this.")

(defun record-scan-anchor (rope line-start known)
  "Where to read ROPE from to find the start of the record holding the line
starting at LINE-START: KNOWN, a record start before it, when given; else the
start of the text when that is within +CSV-SCAN-LIMIT+ octets, or the start
of the first line within them, taken to begin a record."
  (cond (known known)
        ((<= line-start +csv-scan-limit+) 0)
        (t (let ((newline (tuatara.text:rope-find-line-end rope :start (- line-start +csv-scan-limit+) :end line-start)))
             (if newline (1+ newline) line-start)))))

(defun record-starts-between (rope anchor line-start)
  "Read ROPE from ANCHOR, a record's start, below LINE-START, a line's: two
values, the last line start at or before LINE-START outside quotes, which
begins the record holding that line, and the record starts met every
+CSV-RECORD-CHECKPOINT+ octets on the way, the last first."
  (declare (type fixnum anchor line-start))
  (let ((start anchor) (quoted nil) (found '()) (checkpoint (+ anchor +csv-record-checkpoint+))
        ;; a carriage return outside quotes just read: a line end unless a newline comes next
        (return-before nil))
    (declare (type fixnum start checkpoint))
    (flet ((line-ended (next)
             (setf start next)
             (when (>= start checkpoint)
               (push start found)
               (setf checkpoint (+ start +csv-record-checkpoint+)))))
      (do-csv-octets (b at quoted) (rope anchor line-start)
        (when (and return-before (/= b 10)) (line-ended at))
        (setf return-before (and (= b 13) (not quoted)))
        (when (and (= b 10) (not quoted)) (line-ended (1+ at))))
      (when return-before (line-ended line-start)))
    (values start found)))

(defun unchanged-before (version now)
  "How far into the text the changes from VERSION, a version of a text, to
NOW, a later one, leave it as it was: the start of the first change (the
text's end when there is none, MOST-POSITIVE-FIXNUM); NIL when that is not
known, VERSION not before NOW or the text swapped whole."
  (let ((v version) (low most-positive-fixnum))
    (loop
      (when (eq v now) (return low))
      (let ((next (tuatara.text:version-next v)))
        (unless next (return nil))
        (let ((batch (tuatara.text:version-batch next)))
          (when (tuatara.text:batch-swap batch) (return nil))
          (loop for edit across (tuatara.text:batch-edits batch)
                do (setf low (min low (tuatara.text:edit-start edit)))))
        (setf v next)))))

(defun csv-record-start (buffer line-start)
  "Where the record holding the line of BUFFER starting at LINE-START begins: a
quoted field may hold line breaks, so the last line start at or before it
outside quotes. Read from the nearest record start found before, for the
buffer's text, or before the first change made since, at most
+CSV-SCAN-LIMIT+ octets back; else from where
RECORD-SCAN-ANCHOR says. The record starts met every +CSV-RECORD-CHECKPOINT+
octets on the way are kept too (the last 512 found), so that moving a line at
a time reads a line or so going down and no more than that many octets going
up. Complexity: O(the octets read)."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (kept (let ((kept (tuatara.buffer:buffer-local buffer 'csv-record-start)))
                 (and kept
                      (let ((unchanged (unchanged-before (car kept) version)))
                        ;; the starts before the first change since stand
                        (and unchanged (remove-if (lambda (k) (>= k unchanged)) (cdr kept)))))))
         (known (let ((best nil))
                  (dolist (k kept best)
                    (when (and (<= k line-start) (<= (- line-start k) +csv-scan-limit+)
                               (or (null best) (> k best)))
                      (setf best k)))))
         (rope (tuatara.buffer:buffer-text buffer))
         (anchor (record-scan-anchor rope line-start known)))
    (multiple-value-bind (start found) (record-starts-between rope anchor line-start)
      (dolist (k (list* anchor start found))
        (unless (member k kept) (push k kept)))
      (setf (tuatara.buffer:buffer-local buffer 'csv-record-start)
            (cons version (if (> (length kept) 512) (subseq kept 0 512) kept)))
      start)))

(defun rope-record-start (rope line-start)
  "Where the record of ROPE, CSV text, holding the line starting at
LINE-START begins (as CSV-RECORD-START finds it, nothing kept): for work on a
copy of a text, on any thread."
  (values (record-starts-between rope (record-scan-anchor rope line-start nil) line-start)))

(defun csv-record-end (rope start)
  "Where the record of ROPE, CSV text, beginning at START ends: a quoted field
may hold line breaks, so the first line end after START outside quotes. Two
values: that line end (the end of the text when there is none), and the start
of the line after it. No more than +CSV-SCAN-LIMIT+ octets are read: a record
not ended within them ends with its first line."
  (declare (type fixnum start))
  (let* ((size (tuatara.text:rope-bytes rope))
         (stop (min size (+ start +csv-scan-limit+)))
         (quoted nil)
         ;; a carriage return outside quotes just read: a line end unless a newline comes next
         (return-before nil))
    (do-csv-octets (b at quoted) (rope start stop)
      (when (and return-before (/= b 10))
        (return-from csv-record-end (values (1- at) at)))
      (setf return-before (and (= b 13) (not quoted)))
      (when (and (= b 10) (not quoted))
        (return-from csv-record-end (values at (1+ at)))))
    (when (and return-before (= stop size))
      (return-from csv-record-end (values (1- size) size)))
    (if (= stop size)
        (values size size)
        (let ((line-end (tuatara.text:rope-line-end rope start)))
          (values line-end (min size (1+ line-end)))))))

(defun csv-comment-line-p (rope line-start)
  "True when the line of ROPE starting at LINE-START starts with #."
  (and (< line-start (tuatara.text:rope-bytes rope))
       (= (tuatara.text:rope-octet rope line-start) 35)))

(defmethod mode-syntax-highlights ((mode csv-mode) buffer rows)
  "Colour the fields ROWS of BUFFER show, each column its own colour, as *CSV-
COLOURING* says."
  (unless (eq (csv-colouring mode) :none)
    (let* ((rope (tuatara.buffer:buffer-text buffer))
           (delimiter (csv-delimiter buffer))
           (sparse (eq (csv-colouring mode) :sparse))
           (ranges (list nil))
           (tail ranges)
           (field 0) (quoted nil) (carried nil) (comment nil))
      (flet ((add (list)
               (setf (cdr tail) list
                     tail (last tail))))
        (loop for row across rows
              do (let* ((start (tuatara.display:row-start row))
                        ;; a row goes on past the lines search filters hide after it: those are not read
                        (end (min (tuatara.display:row-shown-end row) (tuatara.text:rope-line-end rope start))))
                   (cond ((not carried)
                          ;; the first row shown: the state at its start, read from its record's start
                          (let* ((line-start (if (tuatara.display:row-continuation-p row)
                                                 (tuatara.text:rope-line-start rope start)
                                                 start))
                                 (record (csv-record-start buffer line-start)))
                            (setf comment (and sparse (= record line-start) (csv-comment-line-p rope line-start)))
                            (multiple-value-bind (r f q) (csv-scan rope record start delimiter 0 nil :ranges nil)
                              (declare (ignore r))
                              (setf field f quoted q))))
                         ;; a new line starts a record, unless a quoted field goes on over the line break
                         ((and (not (tuatara.display:row-continuation-p row)) (not quoted))
                          (setf field 0
                                comment (and sparse (csv-comment-line-p rope start)))))
                   (cond (comment
                          (when (< start end)
                            (add (list (list start end :comment))))
                          (setf carried t))
                         (sparse
                          (multiple-value-bind (row-ranges q) (csv-sparse-scan rope start end delimiter quoted)
                            (add row-ranges)
                            (setf quoted q carried t)))
                         (t
                          (multiple-value-bind (row-ranges f q) (csv-scan rope start end delimiter field quoted)
                            (add row-ranges)
                            (setf field f quoted q carried t)))))))
      (cdr ranges))))

(defun csv-field-spans (text start end delimiter spans &optional (first 0) last)
  "Put where the fields of the record in TEXT, a string, from START below END
begin and end, as written, quotes kept, into SPANS, a vector of fixnums with
two places to a field: from field FIRST (from 0) on, as many as it has room
for. A DELIMITER (a character, or its code) inside double quotes is no
field's end. How many fields the record has; with LAST, a field's number,
the record is read no further than that field's end, and one more than LAST
is the answer when the record has it. Makes nothing, for work on every line
of a large file."
  (declare (type simple-string text) (type fixnum start end first)
           (type (or null fixnum) last)
           (type (simple-array fixnum (*)) spans) (optimize speed))
  (let ((delimiter (if (characterp delimiter) delimiter (code-char delimiter)))
        (room (length spans)) (field 0) (field-start start) (quoted nil))
    (declare (type character delimiter) (type fixnum room field field-start))
    (flet ((put (field-end)
             (let ((at (* 2 (- field first))))
               (declare (type fixnum at))
               (when (and (>= field first) (< (1+ at) room))
                 (setf (aref spans at) field-start (aref spans (1+ at)) field-end)))))
      (loop for i of-type fixnum from start below end
            for c = (schar text i)
            do (cond ((char= c #\") (setf quoted (not quoted)))
                     ((and (char= c delimiter) (not quoted))
                      (put i)
                      (when (eql field last) (return-from csv-field-spans (1+ field)))
                      (incf field)
                      (setf field-start (1+ i)))))
      (put end)
      (1+ field))))

(defun csv-field-bounds (text start end delimiter index)
  "Where field INDEX (from 0) of the record in TEXT, a string, from START
below END, begins and ends, as written, quotes kept: two values; when the
record has fewer fields, NIL and how many it has (CSV-FIELD-SPANS). Reads
no further than that field, and makes nothing."
  (let ((spans (make-array 2 :element-type 'fixnum :initial-element 0)))
    (declare (dynamic-extent spans))
    (let ((count (csv-field-spans text start end delimiter spans index index)))
      (if (< index count) (values (aref spans 0) (aref spans 1)) (values nil count)))))

(defun csv-fields (text start end delimiter)
  "The fields of the record in TEXT, a string, from START below END, as a
vector of (START . END) each, as written: the record read twice, once to
count its fields and once to place them (CSV-FIELD-SPANS)."
  (let* ((count (csv-field-spans text start end delimiter (make-array 0 :element-type 'fixnum)))
         (spans (make-array (* 2 count) :element-type 'fixnum :initial-element 0))
         (fields (make-array count)))
    (csv-field-spans text start end delimiter spans)
    (dotimes (i count fields)
      (setf (svref fields i) (cons (aref spans (* 2 i)) (aref spans (1+ (* 2 i))))))))

(defun csv-value-bounds (text start end)
  "Where the value of the field of TEXT from START below END stands: spaces
and carriage returns about it left out, and true as a third value when it is
quoted, its quotes left out too (a doubled quote inside then stands for one).
Makes nothing."
  (declare (type simple-string text) (type fixnum start end) (optimize speed))
  (flet ((blank-p (c) (or (char= c #\Space) (char= c #\Return))))
    (loop while (and (< start end) (blank-p (schar text start))) do (incf start))
    (loop while (and (> end start) (blank-p (schar text (1- end)))) do (decf end)))
  (if (and (>= (- end start) 2) (char= (schar text start) #\") (char= (schar text (1- end)) #\"))
      (values (1+ start) (1- end) t)
      (values start end nil)))

(defun csv-field-value (text start end)
  "The value of the field of TEXT from START below END (CSV-VALUE-BOUNDS), as a
new string, a doubled quote inside a quoted value read as one."
  (multiple-value-bind (start end quoted) (csv-value-bounds text start end)
    (if quoted
        (cl-ppcre:regex-replace-all "\"\"" (subseq text start end) "\"")
        (subseq text start end))))

(defun csv-header-names (buffer)
  "The names of the columns of BUFFER, a CSV or TSV buffer, as its first line,
the header, gives them, in order, without enclosing quotes (CSV-FIELD-VALUE);
of a first line longer than a megabyte, only the fields of its first
megabyte. A quoted name may hold a line break, the header then going on over
it (CSV-RECORD-END). A byte-order mark the file begins with is no part of
the first name."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (end (record-text-end rope (min (csv-record-end rope 0) +csv-scan-limit+) (buffer-crlf-p buffer)))
         (text (tuatara.text:lenient-string (tuatara.text:rope-octets rope :start 0 :end end)))
         (start (if (and (plusp (length text)) (char= (char text 0) (code-char #xfeff))) 1 0)))
    (map 'list (lambda (field) (csv-field-value text (car field) (cdr field)))
         (csv-fields text start (length text) (csv-delimiter buffer)))))

(defun csv-header-name (buffer index)
  "The name of column INDEX (from 0) of BUFFER, as CSV-HEADER-NAMES gives
them, or NIL; the names kept for BUFFER's text as it is."
  (let ((version (tuatara.buffer:buffer-version buffer))
        (kept (tuatara.buffer:buffer-local buffer 'csv-header-names)))
    (unless (and kept (eq (car kept) version))
      (setf kept (cons version (coerce (csv-header-names buffer) 'simple-vector))
            (tuatara.buffer:buffer-local buffer 'csv-header-names) kept))
    (and (< index (length (cdr kept))) (svref (cdr kept) index))))

(defmethod filters-hide-lines-p ((mode csv-mode) buffer)
  "A CSV's records, a line each under its header line."
  (declare (ignore buffer))
  t)

(defmethod record-header-lines ((mode csv-mode) buffer)
  "A CSV's first line, naming its columns, unless *CSV-HEADER* is NIL."
  (declare (ignore buffer))
  (if *csv-header* 1 0))

(defmethod mode-record-fields ((mode csv-mode) buffer)
  "A CSV's fields, apart by its separator, each value without its quotes (a
doubled quote in it read as one); named by the header when the first line is
one."
  (make-record-fields
   ;; the separator as it is when a record is tried, after csv-define-separator too
   :each (lambda (text length field fn)
           (multiple-value-bind (field-start field-end) (csv-field-bounds text 0 length (csv-delimiter buffer) field)
             (when field-start
               (multiple-value-bind (start end quoted) (csv-value-bounds text field-start field-end)
                 (if (and quoted (search "\"\"" text :start2 start :end2 end))
                     (let ((value (csv-field-value text field-start field-end)))
                       (funcall fn value 0 (length value)))
                     (funcall fn text start end))))))))

(defmethod mode-line-extra ((mode csv-mode) buffer)
  "What the mode line says of the column point is in (CSV-COLUMN-WORDS),
worked out once for the text and point as they are."
  (let ((version (tuatara.buffer:buffer-version buffer))
        (point (tuatara.buffer:buffer-point buffer))
        (kept (tuatara.buffer:buffer-local buffer 'csv-mode-line)))
    (if (and kept (eq (first kept) version) (eql (second kept) point))
        (third kept)
        (let ((words (csv-column-words buffer point)))
          (setf (tuatara.buffer:buffer-local buffer 'csv-mode-line) (list version point words))
          words))))

(defun csv-column-words (buffer point)
  "What the mode line of BUFFER says of the column POINT is in: col 2 price,
the fields counted from the start of its record (CSV-RECORD-START); NIL on a
record too long to read at every key."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (line-start (tuatara.text:rope-line-start rope point))
         (record (csv-record-start buffer line-start)))
    (when (<= (- point record) +csv-scan-limit+)
      (multiple-value-bind (r field) (csv-scan rope record point (csv-delimiter buffer) 0 nil :ranges nil)
        (declare (ignore r))
        (let ((header (and (list-header-p buffer) (csv-header-name buffer field))))
          (format nil "col ~:d~@[ ~a~]" field (and header (plusp (length header)) (tuatara.display:shorten-string header 24))))))))

(define-command csv-define-separator ()
  "Ask for the character that separates the fields of this buffer and show
them separated by it: comma, semicolon, tab, pipe, colon or space by name, or
any other character of ASCII typed as itself (\\<minibuffer>\\[minibuffer-submit-input]
takes what is typed as it is); detect takes whichever of comma, semicolon, tab
and pipe the first line has most of, or has once one is typed there.
*CSV-SEPARATOR* sets it for every CSV buffer."
  (let* ((buffer (current-buffer))
         (current (csv-delimiter buffer))
         (candidates (append (loop for (name . char) in *csv-separator-names*
                                   collect (make-candidate name :annotation (if (member char '(#\Tab #\Space)) "" (string char))))
                             (list (make-candidate "detect" :annotation "what the first line has most of"))))
         (answer (completing-read "Separator: " candidates :keep-order t :default (separator-name current)))
         (detect (string-equal answer "detect")))
    (multiple-value-bind (octet found)
        (if detect
            (detect-delimiter (tuatara.buffer:buffer-text buffer))
            (let ((char (separator-char answer)))
              (unless char
                (error "Not a separator: ~a (one character of ASCII, not a line end or a double quote)" answer))
              (char-code char)))
      (setf (tuatara.buffer:buffer-local buffer :csv-delimiter) octet
            (tuatara.buffer:buffer-local buffer :csv-detecting) (and detect (if found :done :open)))
      (message "Fields separated by ~a" (separator-name octet)))))

(defun install-csv ()
  "Give CSV buffers their key: \\`C-c C-s' sets the separator; its command works
there alone."
  (commands-for-mode 'csv-mode "CSV and TSV buffers" 'csv-define-separator)
  (define-key (mode-keymap 'csv-mode) "C-c C-s" 'csv-define-separator))
