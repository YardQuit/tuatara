;;;; lisp/org/parts.lisp --- An Org buffer in parts: each headline with its text, parsed by itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A headline and the text under it, up to the next headline, read the same
;;;; wherever they stand in a file: nothing before or after them changes their
;;;; parse but the TODO keywords a #+TODO: line sets. So an Org buffer is
;;;; kept here as such parts, found by the stars that begin their lines alone,
;;;; the text before the first headline a part too. A change to the text marks
;;;; the parts it touched and only that stretch is split into parts again; a
;;;; part is parsed when it is first shown, and again only after a change
;;;; within it. What is drawn at each key (the colours, the hidden parts of
;;;; links, the signs, the indented view) is read from the parts in sight, so
;;;; that typing costs what the part typed in costs, whatever the size of the
;;;; file. Commands that work on the whole file take its whole parse
;;;; (ORG-BUFFER-TREE), made of the parts' parses (ORG-ASSEMBLE), only the
;;;; parts changed parsed again. A part too large to parse at once is parsed
;;;; in the background, once typing in it pauses.

(in-package #:tuatara.defaults)

(defparameter *org-parse-at-once* (* 8 1024)
  "The characters an Org buffer, or a part of one (ORG-PART), may have to be
parsed again at once, after a change, before its colours are shown; a larger
one is parsed in the background, waited for at most *ORG-PARSE-WAIT*, the
parse before kept meanwhile.")

(defparameter *org-parse-wait* 1/50
  "The seconds the colouring of an Org buffer larger than *ORG-PARSE-AT-ONCE*
waits at most, once after each change, for its parse in the background, so
that what is typed rarely shows in the colours of the parse before.")

(defstruct (org-state (:constructor make-org-state ()) (:copier nil))
  "What the parse of an Org buffer, or of a part of one (ORG-PART), is: TREE,
the parse of TEXT, the text at VERSION; RESULT, the latest parse a worker
made, (TREE TEXT VERSION RETURNS LANGUAGES PLACES), put there in one step,
PLACES where the parts are in a whole buffer's parse with the snapshot it
was made of, let go once taken (ORG-KEEP-WHOLE-PLACES); WORKER,
the thread parsing in the background, one at a time, and WANTED, the
\(VERSION SOURCE CRLF KEYWORDS) it parses next, the latest asked for
\(ORG-WANT-PARSE), guarded by LOCK, DONE woken as each parse is put in
RESULT, WAITED the last version colouring waited for. RETURNS, where TEXT,
with the carriage returns before its newlines left out, lost one
\(ORG-STRIP-RETURNS). PARSE-SECONDS, how long the worker's last parse took."
  (version -1) tree text returns result worker wanted (lock (sb-thread:make-mutex :name "org parse"))
  (done (sb-thread:make-waitqueue)) waited parse-seconds)

(setf (documentation 'org-state-p 'function) "True when OBJECT is an ORG-STATE.")

(defstruct (org-part (:constructor make-org-part (level &optional todo-lines)) (:copier nil))
  "A part of an Org buffer: the headline of LEVEL stars beginning it with the
text under it before the next headline, or for LEVEL 0 the text before the
first headline; LEVEL NIL while a change has left it to be split again.
TODO-LINES, where its #+TODO: lines (#+SEQ_TODO:, #+TYP_TODO:) begin, and
CODE-LINES, where the lines that may say where its source blocks are and
what they are begin (ORG-LINE-KIND's :CODE), octets from its start;
COMMENTED, true when its headline's line has the word COMMENT. PARSE, its
parse once made, (TREE TEXT RETURNS), the characters of TEXT counted from
the part's start; STATE, for a part larger than *ORG-PARSE-AT-ONCE*, the
ORG-STATE parsing it in the background instead; RETURNS, where its text
lost a carriage return before a newline once that is asked for (:UNKNOWN
before). CHANGED, the number of the buffer's version its text last changed
in, and CHANGED-AT, the internal real time it did. TOUCHED, while changes
within this one part alone wait to be taken in, what was known of it
before and the octets they touched: (LEVEL COMMENTED LOW HIGH TODO-LINES
CODE-LINES), the lines moved where the changes moved them. WHOLE, where it
is in the buffer's whole parse (ORG-BUFFER-TREE) as last made, (TREE NODE
START LENGTH RETURNS-BEFORE CURRENT): NODE its headline there (the whole
TREE for the text before the first headline), START and LENGTH its
characters in that parse's text, RETURNS-BEFORE the carriage returns that
text lost before START; CURRENT false once the part changed, the parse then
only shown until its own is made. BLOCKS, what ORG-BLOCKS-KEY takes of it,
once worked out."
  level todo-lines code-lines commented parse state (returns :unknown) (changed 0)
  (changed-at (get-internal-real-time)) touched whole blocks)

(setf (documentation 'org-part-p 'function) "True when OBJECT is an ORG-PART.")

(defstruct (org-parts (:constructor make-org-parts ()) (:copier nil))
  "An Org buffer as its parts, in order: COUNT of them, STARTS, where each
begins in octets, and PARTS, each an ORG-PART, vectors with room for more.
VERSION, the buffer's version they are of; BYTES, its size then;
TODO-KEYWORDS, the TODO keywords its #+TODO: lines set, (NOT-DONE . DONE) as
ORG-PARSE takes them. SUMS and ADDS, room ORG-PARTS-TAKE-EDITS keeps
between changes for the moves of the parts."
  version (bytes 0 :type fixnum) (count 0 :type fixnum)
  (starts (make-array 16 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (parts (make-array 16 :initial-element nil) :type simple-vector)
  todo-keywords
  (sums (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (adds (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*))))

(setf (documentation 'org-parts-p 'function) "True when OBJECT is an ORG-PARTS.")

;;; Finding the parts in the text

(defun org-line-kind (octet-at line end)
  "What the line beginning at the octet LINE, before END, is to the parts: a
headline's number of stars (stars, then a space or a tab), and as a second
value true when the word COMMENT is on its line; :TODO for a #+TODO:,
#+SEQ_TODO: or #+TYP_TODO: line; :CODE for one that may say where source
blocks are and what they are, a block's first or last line (#+begin_src,
#+end_src and the rest), #+PROPERTY: or :header-args; these in any case
and set in by spaces or tabs; else NIL. OCTET-AT gives the octet at a
position."
  (declare (type function octet-at) (type fixnum line end))
  (labels ((at (p) (and (< p end) (funcall octet-at p)))
           (word-at (p word)
             (loop for c across word
                   for q from p
                   always (let ((octet (at q))) (and octet (char-equal c (code-char octet))))))
           (comment-p (from)
             (loop for p from from
                   for octet = (at p)
                   until (or (null octet) (= octet 10) (= octet 13))
                   thereis (and (= octet 67) (word-at p "COMMENT")))))
    (case (at line)
      (42 (let ((after (loop for p from line while (eql (at p) 42) finally (return p))))
            (and (member (at after) '(32 9)) (values (- after line) (comment-p after)))))
      ((32 9 35 58)
       (let ((p (loop for p from line while (member (at p) '(32 9)) finally (return p))))
         (cond ((and (eql (at p) 35) (eql (at (1+ p)) 43))
                (cond ((some (lambda (word) (word-at (+ p 2) word)) '("TODO:" "SEQ_TODO:" "TYP_TODO:")) :todo)
                      ((some (lambda (word) (word-at (+ p 2) word)) '("BEGIN" "END" "PROPERTY:")) :code)))
               ((word-at p ":HEADER-ARGS") :code)))))))

(defun org-scan-parts (rope start end)
  "The headlines and the lines ORG-LINE-KIND names of ROPE's text from the
octet START, where a line begins, below END, where one begins or the text
ends: three lists in order, of each headline's (START LEVEL COMMENTED), of
each #+TODO: line's start and of each :CODE line's start. The text is read a
window at a time, on the stack. Complexity: O(END - START)."
  (declare (type fixnum start end))
  (let ((window (make-array 65536 :element-type '(unsigned-byte 8)))
        (headlines '()) (todo-lines '()) (code-lines '())
        (low start) (high start))
    (declare (dynamic-extent window) (type (simple-array (unsigned-byte 8) (65536)) window) (type fixnum low high))
    (flet ((octet-at (p)
             (declare (type fixnum p))
             (if (< p high) (aref window (- p low)) (tuatara.text:rope-octet rope p))))
      (declare (dynamic-extent #'octet-at))
      (loop while (< low end)
            do (setf high (min end (+ low (length window))))
               (tuatara.text:rope-copy-into rope low high window)
               (let ((line low))
                 (declare (type fixnum line))
                 (loop
                   (multiple-value-bind (kind commented)
                       ;; only a line beginning with a star, a blank, # or : may be one
                       (and (member (aref window (- line low)) '(42 32 9 35 58)) (org-line-kind #'octet-at line end))
                     (case kind
                       ((nil))
                       (:todo (push line todo-lines))
                       (:code (push line code-lines))
                       (t (push (list line kind commented) headlines))))
                   (let ((newline (let ((at (tuatara.text:octets-line-end-position window (- line low) (- high low))))
                                    ;; a carriage return ending the window ends no line a newline follows
                                    (and at (not (and (= at (- high low 1)) (= 13 (aref window at)) (< high end)
                                                      (= 10 (tuatara.text:rope-octet rope high))))
                                         at))))
                     (unless newline
                       ;; the line goes on past the window: the next begins after it
                       (let ((next (and (< high end) (tuatara.text:rope-find-line-end rope :start high :end end))))
                         (setf low (if next (1+ next) end))
                         (return)))
                     (setf line (+ low newline 1))
                     (when (>= line high)
                       (setf low line)
                       (return)))))))
    (values (nreverse headlines) (nreverse todo-lines) (nreverse code-lines))))

;;; Keeping the parts up with the text

(defun org-part-index (parts byte)
  "The index of the part of PARTS holding the octet BYTE: the last that begins
at or before it."
  (let ((starts (org-parts-starts parts)))
    (max 0 (1- (tuatara.runtime:bisect (mid 0 (org-parts-count parts)) (<= (aref starts mid) byte))))))

(defun org-part-start (parts index)
  "Where the part of PARTS at INDEX begins."
  (aref (org-parts-starts parts) index))

(defun org-part-at (parts index)
  "The part of PARTS at INDEX, an ORG-PART."
  (svref (org-parts-parts parts) index))

(defun org-part-end (parts index)
  "Where the part of PARTS at INDEX ends: where the next begins, or the text's end."
  (if (< (1+ index) (org-parts-count parts)) (org-part-start parts (1+ index)) (org-parts-bytes parts)))

(defun org-moved-offsets (offsets from to delta)
  "OFFSETS, positions in order, after an edit replaced those from FROM below
TO: those before FROM as they were, those in it gone, the rest DELTA on."
  (loop for offset in offsets
        if (< offset from) collect offset
        else if (>= offset to) collect (+ offset delta)))

(defun org-part-touched-by (old from to inserted)
  "What is known of the part OLD (an ORG-PART, split or touched before) after
an edit within it replaced its octets FROM below TO by INSERTED octets, as
ORG-PART's TOUCHED has it; NIL when nothing is known of it."
  (let ((touched (org-part-touched old))
        (delta (- inserted (- to from))))
    (when (or (org-part-level old) touched)
      (destructuring-bind (level commented low high todo code)
          (or touched (list (org-part-level old) (org-part-commented old) from from
                            (org-part-todo-lines old) (org-part-code-lines old)))
        (list level commented (min low from)
              (max (+ from inserted) (cond ((<= high from) high) ((< high to) (+ from inserted)) (t (+ high delta))))
              (org-moved-offsets todo from to delta) (org-moved-offsets code from to delta))))))

(defun org-split-range (rope start end touched changed)
  "The parts of ROPE's text from the octet START, where a part begins, below
END, by the headlines in it: a list of (START . PART). Only the lines a
change touched are read when TOUCHED says what is known of the rest
\(ORG-PART's TOUCHED), else all of it; CHANGED, the number of the version
that changed it. As a second value, true when it holds a #+TODO: line."
  (let* ((from (if touched
                   (let ((at (+ start (third touched))))
                     (if (> at start) (1+ (or (tuatara.text:rope-find-line-end-backward rope :start start :end at) (1- start))) start))
                   start))
         (to (if touched
                 (let ((newline (tuatara.text:rope-find-line-end rope :start (min end (+ start (fourth touched))) :end end)))
                   (if newline (1+ newline) end))
                 end)))
    (multiple-value-bind (headlines todo-lines code-lines) (org-scan-parts rope from to)
      (when touched
        ;; the lines kept from before, those outside the lines read again
        (flet ((kept (offsets found)
                 (merge 'list (loop for offset in offsets
                                    for line = (+ start offset)
                                    unless (and (<= from line) (< line to)) collect line)
                        found #'<)))
          (setf todo-lines (kept (fifth touched) todo-lines)
                code-lines (kept (sixth touched) code-lines))))
      (let ((held (and todo-lines t))
            (begins (cond ((and headlines (= (car (first headlines)) start)) headlines)
                          ((and touched (> from start)) (cons (list start (first touched) (second touched)) headlines))
                          (t (cons (list start 0 nil) headlines)))))
        (values (loop for ((begin level commented) next) on begins
                      for part-end = (if next (car next) end)
                      ;; the lines in order, each taken by the part it is in
                      collect (let ((part (make-org-part level (loop while (and todo-lines (< (first todo-lines) part-end))
                                                                     collect (- (pop todo-lines) begin)))))
                                (setf (org-part-code-lines part) (loop while (and code-lines (< (first code-lines) part-end))
                                                                       collect (- (pop code-lines) begin))
                                      (org-part-commented part) commented
                                      (org-part-changed part) changed)
                                (cons begin part)))
                held)))))

(defun org-parts-take-edits (parts batches rope changed)
  "Bring PARTS up to ROPE, the text after BATCHES (the edit batches since the
version PARTS are of, oldest first), in one pass whatever the number of
edits: the parts each edit may have touched, from the one before its start's
to the one holding its end, become one part to be split again (its first
line, or the line a part begins on, may no longer begin a headline); those
after it move. Where an edit is within one part, what is known of the rest
of it is kept and only the lines touched are read again. CHANGED, the number
of the version they are of. True when a part changed held a #+TODO: line or
now holds one. Complexity: O(edits x log^2 parts + parts), and the lines
read again."
  (let* ((n (org-parts-count parts))
         (starts (org-parts-starts parts))
         (all (org-parts-parts parts))
         ;; the moves of the parts after each edit, a sum by index (a Fenwick tree) and each by itself
         (sums (let ((v (org-parts-sums parts)))
                 (if (> (length v) n) (fill v 0 :end (1+ n)) (setf (org-parts-sums parts) (make-array (* 2 (1+ n)) :element-type 'fixnum :initial-element 0)))))
         (adds (let ((v (org-parts-adds parts)))
                 (if (> (length v) n) (fill v 0 :end (1+ n)) (setf (org-parts-adds parts) (make-array (* 2 (1+ n)) :element-type 'fixnum :initial-element 0)))))
         ;; a part taken into the one before it, by index: that one; the last index a part's run covers
         (owner nil) (run-ends nil)
         (dirty (make-hash-table)) (todo nil) (lowest n))
    (declare (type (simple-array fixnum (*)) starts sums adds) (type fixnum n lowest))
    (labels ((prefix (i)
               (let ((sum 0)) (declare (type fixnum sum))
                 (loop for k of-type fixnum = (1+ i) then (logandc2 k (logand k (- k)))
                       while (plusp k) do (incf sum (aref sums k)))
                 sum))
             (add (i delta)
               (when (< i n)
                 (setf lowest (min lowest i))
                 (incf (aref adds i) delta)
                 (loop for k of-type fixnum = (1+ i) then (+ k (logand k (- k)))
                       while (<= k n) do (incf (aref sums k) delta))))
             (resolve (i)
               (let ((up (and owner (gethash i owner))))
                 (if up (setf (gethash i owner) (resolve up)) i)))
             (run-end (i) (or (and run-ends (gethash i run-ends)) i))
             (now (i) (let ((r (resolve i))) (+ (aref starts r) (prefix r))))
             (locate (byte) (resolve (max 0 (1- (tuatara.runtime:bisect (mid 0 n) (<= (now mid) byte))))))
             (take-in (first last)
               ;; the runs of parts after FIRST up to LAST taken into FIRST's
               (loop for r = (1+ (run-end first)) then (1+ (run-end r))
                     while (<= r last)
                     do (setf (gethash r (or owner (setf owner (make-hash-table)))) first
                              todo (or todo (org-part-todo-lines (svref all r))))
                     finally (setf (gethash first (or run-ends (setf run-ends (make-hash-table)))) (run-end last)))))
      (dolist (batch batches)
        (let ((edits (tuatara.text:batch-edits batch)))
          ;; from the last back, so that each is where the batch has it
          (loop for k from (1- (length edits)) downto 0
                do (let* ((edit (svref edits k))
                          (start (tuatara.text:edit-start edit))
                          (end (tuatara.text:edit-end edit))
                          (inserted (tuatara.text:edit-insert-length edit))
                          (first (locate (max 0 (1- start))))
                          (last (locate end))
                          (old (svref all first))
                          (part (make-org-part nil (or (org-part-todo-lines old)
                                                       (loop for r = (1+ (run-end first)) then (1+ (run-end r))
                                                             while (<= r last) thereis (org-part-todo-lines (svref all r)))))))
                     (if (= first last)
                         (let ((base (now first)))
                           ;; a part parsed in the background keeps its last parse while it is parsed again
                           (setf (org-part-state part) (shiftf (org-part-state old) nil)
                                 ;; its place in the whole parse, shown until it is parsed again
                                 (org-part-whole part) (let ((whole (org-part-whole old))) (and whole (append (butlast whole) (list nil))))
                                 (org-part-touched part) (org-part-touched-by old (- start base) (- end base) inserted)))
                         (take-in first last))
                     (setf (gethash first dirty) t
                           (svref all first) part)
                     (add (1+ (run-end first)) (- inserted (- end start)))
                     (incf (org-parts-bytes parts) (- inserted (- end start)))))))
      ;; every start where the edits moved it
      (let ((sum 0)) (declare (type fixnum sum))
        (loop for i of-type fixnum from lowest below n
              do (incf sum (aref adds i))
                 (incf (aref starts i) sum)))
      (flet ((live-p (i) (not (and owner (gethash i owner))))
             (end-of (r) (let ((next (1+ (run-end r)))) (if (< next n) (aref starts next) (org-parts-bytes parts)))))
        ;; a part whose first line no longer begins a headline joins the one before
        (dolist (r (sort (loop for r being the hash-keys of dirty when (live-p r) collect r) #'>))
          (when (and (plusp r) (live-p r)
                     (not (integerp (org-line-kind (lambda (p) (tuatara.text:rope-octet rope p)) (aref starts r) (end-of r)))))
            (let ((before (resolve (1- r))))
              (setf todo (or todo (org-part-todo-lines (svref all r)) (org-part-todo-lines (svref all before))))
              (setf (gethash before dirty) t
                    (svref all before) (make-org-part nil (org-part-todo-lines (svref all before))))
              (take-in before r))))
        ;; each part left to split, split, in place when no part came or went
        (let* ((split (loop for r in (sort (loop for r being the hash-keys of dirty when (live-p r) collect r) #'<)
                            collect (multiple-value-bind (new held)
                                        (org-split-range rope (aref starts r) (end-of r) (org-part-touched (svref all r)) changed)
                                      (when (or held (org-part-todo-lines (svref all r))) (setf todo t))
                                      ;; the background parse of the part typed in goes on with the part that begins where it did
                                      (setf (org-part-state (cdr (first new))) (shiftf (org-part-state (svref all r)) nil)
                                            (org-part-whole (cdr (first new))) (org-part-whole (svref all r)))
                                      (cons r new)))))
          (if (and (null owner) (every (lambda (entry) (null (cddr entry))) split))
              (loop for (r (nil . part)) in split do (setf (svref all r) part))
              (let ((new-starts '()) (new-parts '()) (count 0)
                    (by-index (make-hash-table)))
                (loop for entry in split do (setf (gethash (car entry) by-index) entry))
                (loop for r = 0 then (1+ (run-end r))
                      while (< r n)
                      do (let ((entry (gethash r by-index)))
                           (if entry
                               (loop for (start . part) in (cdr entry)
                                     do (push start new-starts) (push part new-parts) (incf count))
                               (progn (push (aref starts r) new-starts) (push (svref all r) new-parts) (incf count)))))
                (let ((room (max 16 (* 2 count))))
                  (setf (org-parts-starts parts) (replace (make-array room :element-type 'fixnum) (nreverse new-starts))
                        (org-parts-parts parts) (replace (make-array room :initial-element nil) (nreverse new-parts))
                        (org-parts-count parts) count)))))))
    todo))

(defun org-parts-keywords (parts rope)
  "The TODO keywords the #+TODO: lines of the parts of PARTS set, in ROPE their text."
  (let ((lines (with-output-to-string (out)
                 (loop for i below (org-parts-count parts)
                       for part = (org-part-at parts i)
                       for start = (org-part-start parts i)
                       do (dolist (offset (org-part-todo-lines part))
                            (let* ((line (+ start offset))
                                   (end (or (tuatara.text:rope-find-line-end rope :start line) (tuatara.text:rope-bytes rope))))
                              ;; a CR LF line's return not one of its words
                              (write-line (string-right-trim '(#\Return) (tuatara.text:rope-string rope :start line :end end)) out)))))))
    (org-todo-keywords-of lines)))

(defun org-buffer-parts (buffer)
  "BUFFER's text as its parts (ORG-PARTS), as it is now: made the first time,
then kept up with the changes since, taken in one pass (ORG-PARTS-TAKE-EDITS);
the TODO keywords read again only when a change touched a #+TODO: line, and
every parse let go when they changed. Complexity: O(the text) the first time
or after a change of the whole text; then O(edits x log^2 parts + parts),
and the lines changed."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (rope (tuatara.text:version-rope version))
         (parts (tuatara.buffer:buffer-local buffer 'org-parts)))
    (unless (and parts (eq (org-parts-version parts) version))
      (let* ((batches (and parts
                           (loop for at = (org-parts-version parts) then next
                                 for next = (tuatara.text:version-next at)
                                 until (eq at version)
                                 collect (let ((batch (and next (tuatara.text:version-batch next))))
                                           (if (or (null batch) (tuatara.text:batch-swap batch))
                                               (return :whole)
                                               batch)))))
             (changed (tuatara.text:version-number version))
             (todo nil))
        (if (or (null parts) (eq batches :whole))
            (let ((fresh (make-org-parts)))
              (setf (tuatara.buffer:buffer-local buffer 'org-parts) fresh
                    (org-parts-bytes fresh) (tuatara.text:rope-bytes rope))
              (multiple-value-bind (new held) (org-split-range rope 0 (tuatara.text:rope-bytes rope) nil changed)
                (setf (org-parts-starts fresh) (coerce (mapcar #'car new) '(simple-array fixnum (*)))
                      (org-parts-parts fresh) (coerce (mapcar #'cdr new) 'simple-vector)
                      (org-parts-count fresh) (length new)
                      todo (or held t)))
              (setf parts fresh))
            (setf todo (org-parts-take-edits parts batches rope changed)))
        (setf (org-parts-version parts) version)
        (when todo
          (let ((keywords (org-parts-keywords parts rope)))
            (unless (equal keywords (org-parts-todo-keywords parts))
              (setf (org-parts-todo-keywords parts) keywords)
              (loop for i below (org-parts-count parts)
                    for part = (org-part-at parts i)
                    do (setf (org-part-parse part) nil
                             (org-part-state part) nil
                             (org-part-whole part) nil
                             (org-part-changed part) changed)))))))
    parts))

;;; What is known of a stretch of the text

(defstruct (org-view (:constructor make-org-view (tree text from to returns current &optional base root
                                                   &aux (plain (and (null base) current (null returns) (= (length text) (- to from))))))
                    (:copier nil))
  "What is known of a part of an Org buffer, the octets FROM below TO: TREE,
the parse of TEXT, or the part's node of the whole buffer's parse ROOT with
TEXT that parse's text; RETURNS, where TEXT lost a carriage return before a
newline (ORG-STRIP-RETURNS); CURRENT, true when the parse is of the part as
it is now, false while a parse of it is behind it. PLAIN, true when each
character of TEXT is one octet of the part, so that the one is found from
the other at once; else BASE, the buffer's character less TEXT's character
there (given for the whole buffer's parse, else found when first asked for,
ORG-VIEW-BYTE)."
  tree text from to returns current plain base root)

(setf (documentation 'org-view-p 'function) "True when OBJECT is an ORG-VIEW.")

(defun org-view-base-char (buffer view)
  "The character of BUFFER that VIEW's text begins at, found the first time."
  (or (org-view-base view)
      (setf (org-view-base view) (tuatara.text:byte->char (tuatara.buffer:buffer-text buffer) (org-view-from view)))))

(defun org-view-byte (buffer view char)
  "The octet of BUFFER where the character CHAR of VIEW's text is."
  (if (org-view-plain view)
      (+ (org-view-from view) char)
      (org-char->byte buffer char (org-view-returns view) (org-view-base-char buffer view))))

(defun org-view-char (buffer view byte)
  "The character of VIEW's text at the octet BYTE of BUFFER, BYTE within it."
  (if (org-view-plain view)
      (- byte (org-view-from view))
      (org-byte->char buffer byte (org-view-returns view) (org-view-base-char buffer view))))

(defun org-parse-part (buffer from to keywords)
  "The text of BUFFER from the octet FROM below TO, a part of it, parsed with
the TODO keywords KEYWORDS: (TREE TEXT RETURNS), as ORG-PART's PARSE has it."
  (let ((raw (tuatara.buffer:buffer-string buffer :start from :end to)))
    (multiple-value-bind (text returns) (if (buffer-crlf-p buffer) (org-strip-returns raw) (values (lone-returns-as-newlines raw) nil))
      (let ((tree (org-parse text :todo-keywords keywords)))
        (org-warm-src-languages (org-src-language-names tree))
        (list tree text returns)))))

(defparameter *org-parse-pause-most* 1
  "The most seconds typing in a large part of an Org buffer (ORG-PART) must
pause before the part is parsed again in the background.")

(defvar *org-parse-due* nil
  "The internal real time a parse of a large part put off while typing goes
on is due (ORG-PART-PARSE-LATER), the soonest of them; NIL when none is.")

(defun org-part-parse-later (part state)
  "True when PART, a large part of an Org buffer whose parse STATE (an
ORG-STATE) is behind its text, is to be parsed again only later: its last
parse took longer than *ORG-PARSE-WAIT*, so that a parse for each key would
be garbage made faster than it is shown, and it changed less than that long
ago (at most *ORG-PARSE-PAUSE-MOST* seconds). Then *ORG-PARSE-DUE* says when
\(ORG-PARSE-TICK), so that the part is parsed once typing pauses, a parse
at a time whatever is typed meanwhile."
  (let ((took (org-state-parse-seconds state)))
    (when (and took (> took *org-parse-wait*))
      (let ((due (+ (org-part-changed-at part)
                    (round (* (min took *org-parse-pause-most*) internal-time-units-per-second)))))
        (when (> due (get-internal-real-time))
          (setf *org-parse-due* (if *org-parse-due* (min *org-parse-due* due) due))
          t)))))

(defun org-parse-tick (editor)
  "Tick function: when a large part's parse put off while typing went on is
due (*ORG-PARSE-DUE*), have the editor draw again then, which asks for it."
  (declare (ignore editor))
  (when *org-parse-due*
    (let ((left (/ (- *org-parse-due* (get-internal-real-time)) internal-time-units-per-second)))
      (if (plusp left)
          left
          (progn (setf *org-parse-due* nil) 1/100)))))

(defun install-org-parts ()
  "Have large parts of Org buffers parsed once typing in them pauses (ORG-PARSE-TICK)."
  (pushnew 'org-parse-tick *tick-functions*))

(defun org-part-whole-now (buffer part)
  "PART's place in BUFFER's whole parse (ORG-PART's WHOLE) while that parse
is the one kept, else NIL."
  (let ((whole (org-part-whole part)))
    (and whole (eq (first whole) (org-state-tree (org-buffer-state buffer))) whole)))

(defun org-whole-view (buffer part from to)
  "The ORG-VIEW of PART, a part of BUFFER from the octet FROM below TO, read
from BUFFER's whole parse (ORG-PART's WHOLE), or NIL when it is not there:
current while the part is as it was then."
  (let ((whole (org-part-whole-now buffer part)))
    (when whole
      (destructuring-bind (tree node start length returns-before current) whole
        (declare (ignore length))
        (let ((state (org-buffer-state buffer)))
          (make-org-view node (org-state-text state) from to (org-state-returns state) current
                         (- (tuatara.text:byte->char (tuatara.buffer:buffer-text buffer) from) start returns-before)
                         tree))))))

(defun org-part-view (buffer parts index)
  "The ORG-VIEW of the part of PARTS at INDEX, a part of BUFFER: its own
parse; else, while it is as it was then, its place in the whole buffer's
parse (ORG-WHOLE-VIEW), which nothing holds twice; else parsed now when it
is no larger than *ORG-PARSE-AT-ONCE*, its parse kept until it changes; a
larger one parsed in the background, waited for a moment (*ORG-PARSE-WAIT*),
the parse before given until that one is done, or NIL while it has none."
  (let* ((part (org-part-at parts index))
         (from (org-part-start parts index))
         (to (org-part-end parts index))
         (keywords (org-parts-todo-keywords parts))
         (whole (org-part-whole-now buffer part)))
    (cond ((org-part-parse part)
           (destructuring-bind (tree text returns) (org-part-parse part)
             (make-org-view tree text from to returns t)))
          ((and whole (sixth whole))
           (org-whole-view buffer part from to))
          ((<= (- to from) *org-parse-at-once*)
           (destructuring-bind (tree text returns) (setf (org-part-parse part) (org-parse-part buffer from to keywords))
             (make-org-view tree text from to returns t)))
          (t
           (let ((state (or (org-part-state part) (setf (org-part-state part) (make-org-state))))
                 (version (tuatara.buffer:buffer-version buffer)))
             (flet ((current-p () (and (org-state-tree state) (>= (org-version-number (org-state-version state)) (org-part-changed part)))))
               (org-take-parse state)
               (unless (or (current-p) (org-part-parse-later part state))
                 (org-want-parse buffer state version (lambda () (cons from to)) :todo-keywords keywords)
                 (when (org-await-parse state version)
                   (org-take-parse state)))
               (if (org-state-tree state)
                   (make-org-view (org-state-tree state) (org-state-text state) from to (org-state-returns state) (current-p))
                   ;; its place in the whole parse, as it was before it changed
                   (org-whole-view buffer part from to))))))))

(defun org-views (buffer start end)
  "The ORG-VIEWs of the parts of BUFFER holding an octet from START below END
\(the part at START when they are one), in order; none for a part not
parsed yet, nor while the text's characters are not counted yet."
  (when (tuatara.text:rope-indexed-p (tuatara.buffer:buffer-text buffer))
    (let* ((parts (org-buffer-parts buffer))
           (first (org-part-index parts start)))
      (loop for i from first below (org-parts-count parts)
            while (or (= i first) (< (org-part-start parts i) end))
            nconc (let ((view (org-part-view buffer parts i)))
                    (and view (list view)))))))

(defun org-view-at (buffer position)
  "The ORG-VIEW of the part of BUFFER holding the octet POSITION, or NIL
while that part is not parsed yet."
  (first (org-views buffer position position)))

;;; The headlines, as the parts begin

(defun org-headline-at-or-before (buffer position)
  "The last headline of BUFFER that begins at or before the octet POSITION:
where it begins and its level, as two values; NIL above the first."
  (let* ((parts (org-buffer-parts buffer))
         (index (org-part-index parts position))
         (level (org-part-level (org-part-at parts index))))
    (and (plusp level) (values (org-part-start parts index) level))))

(defun map-org-headlines (buffer start end function)
  "Call FUNCTION with where each headline of BUFFER beginning from the octet
START below END begins and its level, in order. Complexity: O(log parts +
the headlines there)."
  (let ((parts (org-buffer-parts buffer)))
    (loop for i from (org-part-index parts start) below (org-parts-count parts)
          for begin = (org-part-start parts i)
          for level = (org-part-level (org-part-at parts i))
          while (< begin end)
          do (when (and (>= begin start) (plusp level))
               (funcall function begin level)))))

;;; Carriage returns

(defun org-part-returns-now (buffer parts index)
  "Where the text Org reads of the part of PARTS at INDEX, a part of BUFFER,
lost a carriage return before a newline: its parse's, those of the whole
buffer's parse in it while it is as it was then, or found in its octets
once and kept with it."
  (let* ((part (org-part-at parts index))
         (whole (org-part-whole-now buffer part)))
    (cond ((org-part-parse part) (third (org-part-parse part)))
          ((and whole (sixth whole))
           (let ((start (third whole)) (length (fourth whole)))
             (org-returns-within (org-state-returns (org-buffer-state buffer)) start (+ start length))))
          ((not (eq (org-part-returns part) :unknown)) (org-part-returns part))
          (t (setf (org-part-returns part)
                   (org-returns-in-rope (tuatara.buffer:buffer-text buffer)
                                        :start (org-part-start parts index) :end (org-part-end parts index)))))))

(defun org-parts-returns (buffer)
  "Where the text Org reads of BUFFER, a buffer whose lines end with CR LF,
lost a carriage return before a newline, as ORG-STRIP-RETURNS gives them:
put together from its parts', each found once and kept until the part
changes. Complexity: O(parts x log of the text + the returns), and the
octets of the parts changed."
  (let* ((parts (org-buffer-parts buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (returns (make-array 64 :element-type 'fixnum :adjustable t :fill-pointer 0))
         (offset 0)
         (from-char 0))
    (dotimes (i (org-parts-count parts))
      (let ((part-returns (org-part-returns-now buffer parts i))
            (to-char (tuatara.text:byte->char rope (org-part-end parts i))))
        (loop for r across (or part-returns #()) do (vector-push-extend (+ offset r) returns))
        (incf offset (- to-char from-char (length part-returns)))
        (setf from-char to-char)))
    (and (plusp (length returns)) (coerce returns '(simple-array fixnum (*))))))

;;; The whole buffer's parse, made of its parts' parses

(defun org-returns-within (returns from to)
  "Those of RETURNS (ORG-STRIP-RETURNS) from FROM below TO, counted from FROM;
NIL when there are none."
  (when returns
    (let* ((low (org-returns-before returns from))
           (high (org-returns-before returns to)))
      (and (< low high)
           (let ((within (make-array (- high low) :element-type 'fixnum)))
             (loop for i from low below high
                   for j from 0
                   do (setf (aref within j) (- (aref returns i) from)))
             within)))))

(defstruct (org-snapshot (:constructor make-org-snapshot (parts keywords entries crlf whole version)) (:copier nil))
  "What the parse of a whole Org buffer is made from, taken from its PARTS
\(ORG-PARTS) at one version: ENTRIES, a vector of (PART LEVEL SOURCE) in
order, SOURCE the part's own parse (TREE TEXT RETURNS), or (:WHOLE NODE
START LENGTH TEXT RETURNS) where the whole parse made before holds it as it
is, or the part's text, a string or (ROPE START END); or, for a whole parse
made afresh, WHOLE, the whole text, a string or a rope. The TODO KEYWORDS
the parts are parsed with; CRLF, true when its lines end with CR LF;
VERSION, the number of the buffer's version it was taken at."
  parts keywords entries crlf whole version)

(setf (documentation 'org-snapshot-p 'function) "True when OBJECT is an ORG-SNAPSHOT.")

(defun org-parts-snapshot (buffer parts)
  "An ORG-SNAPSHOT of PARTS, BUFFER's parts as they are now: for each, its own
parse, its place in the whole parse made before while it is as it was, or
where its text is, a copy of it when BUFFER's file is mapped from disk (the
mapping may go with the buffer). Made afresh, the whole text read once, when
there is no whole parse to take parts from. Complexity: O(parts), and the
octets copied of a mapped file."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (mapped (tuatara.buffer:buffer-mapping buffer))
         (state (org-buffer-state buffer))
         (count (org-parts-count parts))
         (kept (loop for i below count
                     thereis (let ((whole (org-part-whole-now buffer (org-part-at parts i)))) (and whole (sixth whole)))))
         (entries (make-array count)))
    (dotimes (i count)
      (let* ((part (org-part-at parts i))
             (whole (org-part-whole-now buffer part))
             (part-state (org-part-state part))
             (from (org-part-start parts i))
             (to (org-part-end parts i)))
        (setf (svref entries i)
              (list part (org-part-level part)
                    (cond ((org-part-parse part))
                          ((and whole (sixth whole))
                           (destructuring-bind (node start length) (subseq whole 1 4)
                             (list :whole node start length (org-state-text state) (org-state-returns state))))
                          ((and part-state (org-state-tree part-state)
                                (>= (org-version-number (org-state-version part-state)) (org-part-changed part)))
                           (list (org-state-tree part-state) (org-state-text part-state) (org-state-returns part-state)))
                          ((not kept) nil)
                          (mapped (tuatara.buffer:buffer-string buffer :start from :end to))
                          (t (list rope from to)))))))
    (make-org-snapshot parts (org-parts-todo-keywords parts) entries (buffer-crlf-p buffer)
                       (and (not kept) (if mapped (tuatara.buffer:buffer-string buffer) rope))
                       (tuatara.text:version-number (tuatara.buffer:buffer-version buffer)))))

(defun org-copy-node (node offset parent &key headline-end)
  "A copy of NODE and of what is in it, every position OFFSET characters on,
its parent PARENT; as a second value, how many nodes deep it goes, NODE
itself one. With HEADLINE-END, NODE is a headline of a whole parse copied
as the parse of its part alone would have it, the part ending at the
character HEADLINE-END of that parse's text (*ORG-TEXT*): without the
headlines under it, its contents and blank lines counted to there."
  (let ((deepest 0)
        (copy (flet ((moved (position) (and position (+ position offset))))
                (make-org-node (org-node-type node)
                               :begin (moved (org-node-begin node)) :end (moved (org-node-end node))
                               :contents-begin (moved (org-node-contents-begin node))
                               :contents-end (moved (org-node-contents-end node))
                               :post-blank (org-node-post-blank node)
                               :post-affiliated (moved (org-node-post-affiliated node))))))
    (flet ((moved (position) (and position (+ position offset)))
           (copied (child)
             (multiple-value-bind (child-copy depth) (org-copy-node child offset copy)
               (setf deepest (max deepest depth))
               child-copy)))
      (setf (org-node-parent copy) parent
            (org-node-properties copy)
            (loop for (key value) on (org-node-properties node) by #'cddr
                  unless (eq key :children-vector)
                    nconc (list key (case key
                                      ((:title-begin :title-end) (moved value))
                                      (:title (mapcar #'copied value))
                                      (t value))))
            (org-node-children copy)
            (loop for child in (org-node-children node)
                  unless (and headline-end (eq (org-node-type child) 'headline))
                    collect (copied child)))
      (when headline-end
        (let ((contents (let ((begin (org-node-contents-begin node))) (and begin (< begin headline-end) begin))))
          (setf (org-node-end copy) (moved headline-end)
                (org-node-contents-begin copy) (moved contents)
                (org-node-contents-end copy) (and contents (moved headline-end))
                (org-node-post-blank copy) (if contents
                                               0
                                               (org-count-blank-lines (min headline-end (org-next-line (org-node-begin node))) headline-end))))))
    (values copy (1+ deepest))))

(defun org-whole-places (tree text returns entries)
  "Where the parts of ENTRIES (as an ORG-SNAPSHOT has them) are in TREE, the
whole parse of TEXT, which lost RETURNS (ORG-STRIP-RETURNS): for each,
\(PART START LENGTH RETURNS-BEFORE NODE), NODE its headline (TREE for the
text before the first); NIL when TREE's headlines are not the parts'."
  (let ((headlines (org-headlines tree))
        (total (length text)))
    (when (= (length entries) (+ (length headlines) (if (and (plusp (length entries)) (zerop (second (svref entries 0)))) 1 0)))
      (let ((starts (loop for (nil level) across entries
                          collect (if (zerop level) 0 (org-node-begin (pop headlines))))))
        (setf headlines (org-headlines tree))
        (loop for (part level) across entries
              for (start next) on starts
              collect (list part start (- (or next total) start)
                            (if returns (org-returns-before returns start) 0)
                            (if (zerop level) tree (pop headlines))))))))

(defun org-assemble (snapshot)
  "The parse of a whole Org buffer made of its parts' (SNAPSHOT, an
ORG-SNAPSHOT), as ORG-PARSE makes it of the whole text: four values, the
tree, the text, where the text lost a carriage return before a newline
\(ORG-STRIP-RETURNS) and where each part is in it (ORG-WHOLE-PLACES). A
part as it was in the whole parse made before is copied from there, the
others parsed. A headline holds the headlines after it up to the next of as
many stars or fewer (a space after them), as its subtree, and its contents
and blank lines are counted as far as that. Made afresh by parsing the whole
text when SNAPSHOT says so (its WHOLE). Complexity: O(the nodes of the
parse and the characters of the text), and the parts parsed here."
  (let ((keywords (org-snapshot-keywords snapshot)))
    (when (org-snapshot-whole snapshot)
      (let ((raw (let ((whole (org-snapshot-whole snapshot))) (if (stringp whole) whole (tuatara.text:rope-string whole)))))
        (multiple-value-bind (text returns) (if (org-snapshot-crlf snapshot) (org-strip-returns raw) (values (lone-returns-as-newlines raw) nil))
          (let ((tree (org-parse text :todo-keywords keywords)))
            (return-from org-assemble
              (values tree text returns (org-whole-places tree text returns (org-snapshot-entries snapshot))))))))
    (let ((pieces '()) (returns '()) (places '()) (offset 0) (too-deep nil))
      ;; each part's tree, made or copied, where its text begins in the whole and the headlines it closes
      (let ((placed
              (loop for (part level source) across (org-snapshot-entries snapshot)
                    collect (multiple-value-bind (node text from to part-returns)
                                (cond ((eq (first source) :whole)
                                       (destructuring-bind (node start length text whole-returns) (rest source)
                                         (values (list node start (+ start length)) text start (+ start length)
                                                 (org-returns-within whole-returns start (+ start length)))))
                                      ((org-node-p (first source))
                                       (destructuring-bind (tree text part-returns) source
                                         (values tree text 0 (length text) part-returns)))
                                      (t
                                       (let ((raw (if (stringp source)
                                                      source
                                                      (destructuring-bind (rope from to) source
                                                        (tuatara.text:rope-string rope :start from :end to)))))
                                         (multiple-value-bind (text part-returns)
                                             (if (org-snapshot-crlf snapshot) (org-strip-returns raw) (values (lone-returns-as-newlines raw) nil))
                                           (values (org-parse text :todo-keywords keywords) text 0 (length text) part-returns)))))
                              (push (list text from to) pieces)
                              (loop for r across (or part-returns #()) do (push (+ offset r) returns))
                              (prog1 (list level node text from offset (- to from) part)
                                (incf offset (- to from)))))))
        (let* ((total offset)
               (root (make-org-node 'org-data :begin 0 :end total :contents-begin 0 :contents-end total :post-affiliated 0))
               (root-children '())
               ;; the headlines whose subtrees are open, innermost first: (NODE LEVEL PART-END . CHILDREN reversed)
               (open '()))
          (labels ((close-subtree (entry at)
                     (destructuring-bind (node level part-end . children) entry
                       (declare (ignore level))
                       (let ((children (nreverse children)))
                         (setf (org-node-end node) at
                               (org-node-children node) (append (org-node-children node) children))
                         ;; blank under its line, its contents begin with its first child
                         (when (and (null (org-node-contents-begin node)) children (> at part-end))
                           (setf (org-node-contents-begin node) (org-node-begin (first children))
                                 (org-node-post-blank node) 0))
                         (when (org-node-contents-begin node)
                           (setf (org-node-contents-end node) at)))))
                   (adopt (node)
                     (if open
                         (push node (cdddr (first open)))
                         (push node root-children))))
            (loop for (level source text from start length part) in placed
                  for part-end = (+ start length)
                  do (if (zerop level)
                         (let ((children (if (consp source)
                                             ;; the text before the first headline, from the whole parse
                                             (remove 'headline (org-node-children (first source)) :key #'org-node-type)
                                             (org-node-children source))))
                           (dolist (child children)
                             (adopt (org-copy-node child (- start from) root)))
                           (push (list part start length root) places))
                         (let* ((closes (char= #\Space (char text (+ from level))))
                                (outermost (and closes (position-if (lambda (entry) (>= (second entry) level)) open :from-end t)))
                                (parent (progn (when outermost
                                                 (loop repeat (1+ outermost) do (close-subtree (pop open) start)))
                                               (if open (car (first open)) root)))
                                (copy (multiple-value-bind (copy depth)
                                          (if (consp source)
                                              (let ((*org-text* text))
                                                (org-copy-node (first source) (- start from) parent :headline-end (third source)))
                                              (org-copy-node (first (org-node-children source)) start parent))
                                        ;; parsed alone it went deeper than the parse of the whole text may, under its headlines
                                        (when (> (+ depth (length open)) *org-deepest*)
                                          (setf too-deep t))
                                        copy)))
                           (adopt copy)
                           (push (list* copy level part-end '()) open)
                           (push (list part start length copy) places))))
            (loop while open do (close-subtree (pop open) total))
            (setf (org-node-children root) (nreverse root-children))
            (let ((returns (and returns (coerce (nreverse returns) '(simple-array fixnum (*)))))
                  (text (let ((whole (make-string total)) (at 0))
                          (loop for (text from to) in (nreverse pieces)
                                do (replace whole text :start1 at :start2 from :end2 to)
                                   (incf at (- to from)))
                          whole)))
              (if too-deep
                  ;; headlines nested past what a parse reads: the whole text parsed, as deep as that goes
                  (let ((tree (org-parse text :todo-keywords keywords)))
                    (values tree text returns (org-whole-places tree text returns (org-snapshot-entries snapshot))))
                  (values root text returns
                          (loop for (part start length node) in (nreverse places)
                                collect (list part start length (if returns (org-returns-before returns start) 0)
                                              (if (eq node root) root node))))))))))))

(defun org-keep-whole-places (snapshot tree places)
  "Note in each part of SNAPSHOT where it is in TREE, the whole parse made of
it (ORG-ASSEMBLE's PLACES), and let go of the parses it holds of its own,
the whole one now holding them: only while the TODO keywords the parse was
made with are those of the parts still, and only for a part not changed
since SNAPSHOT was taken, a parse of it in the background newer than that
kept."
  (when (eq (org-parts-todo-keywords (org-snapshot-parts snapshot)) (org-snapshot-keywords snapshot))
    (let ((version (org-snapshot-version snapshot)))
      (loop for (part start length returns-before node) in places
            when (and (org-part-level part) (<= (org-part-changed part) version))
              do (setf (org-part-whole part) (list tree node start length returns-before t)
                       (org-part-parse part) nil)
                 (let ((state (org-part-state part)))
                   ;; a parse in the background of the text as it is in TREE, let go; how long it took kept
                   (when (and state (<= (org-version-number (org-state-version state)) version))
                     (setf (org-state-tree state) nil (org-state-text state) nil (org-state-returns state) nil
                           (org-state-version state) -1)))))))

;;; What says which source blocks a buffer has

(defun org-blocks-key-entry (parts index rope)
  "What ORG-BLOCKS-KEY takes of the part of PARTS at INDEX, ROPE the text:
NIL for a part with no lines that may say where blocks are and what they are
\(ORG-PART's CODE-LINES) nor a COMMENT headline; else its level and those
lines, or all of its text when it gives :header-args, after true when every
part's level is wanted too. Worked out once for each part: a part changed is
a new one."
  (let ((part (org-part-at parts index)))
    (when (or (org-part-code-lines part) (org-part-commented part))
      (or (org-part-blocks part)
          (setf (org-part-blocks part)
                (let* ((start (org-part-start parts index))
                       (lines (loop for offset in (org-part-code-lines part)
                                    collect (let* ((line (+ start offset))
                                                   (end (or (tuatara.text:rope-find-line-end rope :start line)
                                                            (tuatara.text:rope-bytes rope))))
                                              (string-trim '(#\Space #\Tab #\Return)
                                                           (tuatara.text:rope-string rope :start line :end end)))))
                       (arguments (find-if (lambda (line) (and (plusp (length line)) (char= (char line 0) #\:))) lines)))
                  (cons (or (org-part-commented part) (and arguments t))
                        (list (org-part-level part) (org-part-commented part)
                              (if arguments
                                  (tuatara.text:rope-string rope :start start :end (org-part-end parts index))
                                  lines)))))))))

(defun org-blocks-key (buffer)
  "What the source blocks of BUFFER, an Org buffer, and what they are depend
on, as data to compare, read from its parts (ORG-BUFFER-PARTS): for each
part with lines that may say where blocks are and what they are (ORG-PART's
CODE-LINES) or a COMMENT headline, its level and those lines, or all of its
text when it gives :header-args (ORG-BLOCKS-KEY-ENTRY); and every part's level
when some part has those or such a headline, as they decide which headlines
a block is under. Equal for two texts whose blocks, their languages, header
arguments and commented subtrees are the same. Complexity: O(parts), and the
lines of the parts changed since it was last asked."
  (let* ((parts (org-buffer-parts buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (nesting nil)
         (entries (loop for i below (org-parts-count parts)
                        for blocks = (org-blocks-key-entry parts i rope)
                        when blocks
                          collect (progn (when (car blocks) (setf nesting t))
                                         (cdr blocks)))))
    (cons entries (and nesting (loop for i below (org-parts-count parts) collect (org-part-level (org-part-at parts i)))))))
