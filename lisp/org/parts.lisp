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
;;;; (ORG-BUFFER-TREE).

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
made, (TREE TEXT VERSION RETURNS LANGUAGES), put there in one step; WORKER,
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
TODO-LINES, where its #+TODO: lines (#+SEQ_TODO:, #+TYP_TODO:) begin,
octets from its start. PARSE, its parse once made, (TREE TEXT RETURNS), the
characters of TEXT counted from the part's start; STATE, for a part larger
than *ORG-PARSE-AT-ONCE*, the ORG-STATE parsing it in the background
instead; CHANGED, the number of the buffer's version its text last changed
in."
  level todo-lines parse state (changed 0))

(setf (documentation 'org-part-p 'function) "True when OBJECT is an ORG-PART.")

(defstruct (org-parts (:constructor make-org-parts ()) (:copier nil))
  "An Org buffer as its parts, in order: COUNT of them, STARTS, where each
begins in octets, and PARTS, each an ORG-PART, vectors with room for more.
VERSION, the buffer's version they are of; BYTES, its size then;
TODO-KEYWORDS, the TODO keywords its #+TODO: lines set, (NOT-DONE . DONE) as
ORG-PARSE takes them. While changes are taken in, UNSPLIT counts the parts
left to be split again, none before the index UNSPLIT-FROM."
  version (bytes 0 :type fixnum) (unsplit 0 :type fixnum) (unsplit-from 0 :type fixnum) (count 0 :type fixnum)
  (starts (make-array 16 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (parts (make-array 16 :initial-element nil) :type simple-vector)
  todo-keywords)

(setf (documentation 'org-parts-p 'function) "True when OBJECT is an ORG-PARTS.")

;;; Finding the parts in the text

(defun org-line-kind (octet-at line end)
  "What the line beginning at the octet LINE, before END, is to the parts: a
headline's number of stars (stars, then a space or a tab), :TODO for a
#+TODO:, #+SEQ_TODO: or #+TYP_TODO: line, in any case and set in by spaces
or tabs, else NIL. OCTET-AT gives the octet at a position."
  (declare (type function octet-at) (type fixnum line end))
  (flet ((at (p) (and (< p end) (funcall octet-at p))))
    (case (at line)
      (42 (let ((after (loop for p from line while (eql (at p) 42) finally (return p))))
            (and (member (at after) '(32 9)) (- after line))))
      ((32 9 35)
       (let ((p (loop for p from line while (member (at p) '(32 9)) finally (return p))))
         (and (eql (at p) 35) (eql (at (1+ p)) 43)
              (some (lambda (word)
                      (loop for c across word
                            for q from (+ p 2)
                            always (let ((octet (at q))) (and octet (char-equal c (code-char octet))))))
                    '("TODO:" "SEQ_TODO:" "TYP_TODO:"))
              :todo))))))

(defun org-scan-parts (rope start end)
  "The headlines and #+TODO: lines of ROPE's text from the octet START, where
a line begins, below END, where one begins or the text ends: two lists in
order, of each headline's (START . LEVEL) and of each #+TODO: line's start.
The text is read a window at a time, on the stack.
Complexity: O(END - START)."
  (declare (type fixnum start end))
  (let ((window (make-array 65536 :element-type '(unsigned-byte 8)))
        (headlines '()) (todo-lines '())
        (low start) (high start))
    (declare (dynamic-extent window) (type fixnum low high))
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
                   (let ((kind (org-line-kind #'octet-at line end)))
                     (cond ((eq kind :todo) (push line todo-lines))
                           (kind (push (cons line kind) headlines))))
                   (let ((newline (position 10 window :start (- line low) :end (- high low))))
                     (unless newline
                       ;; the line goes on past the window: the next begins after it
                       (let ((next (and (< high end) (tuatara.text:rope-find-octet rope 10 :start high :end end))))
                         (setf low (if next (1+ next) end))
                         (return)))
                     (setf line (+ low newline 1))
                     (when (>= line high)
                       (setf low line)
                       (return)))))))
    (values (nreverse headlines) (nreverse todo-lines))))

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

(defun org-parts-replace (parts first last starts new)
  "Put the parts NEW, beginning at STARTS (lists of one length), in place of
those of PARTS from the index FIRST to LAST, both in."
  (let* ((count (org-parts-count parts))
         (after (1+ last))
         (added (length new))
         (new-count (+ count added (- first after))))
    (when (> new-count (length (org-parts-starts parts)))
      (let ((room (max new-count (* 2 (length (org-parts-starts parts))))))
        (setf (org-parts-starts parts) (replace (make-array room :element-type 'fixnum) (org-parts-starts parts))
              (org-parts-parts parts) (replace (make-array room :initial-element nil) (org-parts-parts parts)))))
    (let ((old-starts (org-parts-starts parts))
          (old-parts (org-parts-parts parts)))
      ;; the parts after them moved up or down to their place
      (replace old-starts old-starts :start1 (+ first added) :start2 after :end2 count)
      (replace old-parts old-parts :start1 (+ first added) :start2 after :end2 count)
      (loop for i from first
            for start in starts
            for part in new
            do (setf (aref old-starts i) start
                     (svref old-parts i) part))
      (fill old-parts nil :start new-count :end (max new-count count))
      (setf (org-parts-count parts) new-count))
    parts))

(defun org-parts-edit (parts start end inserted)
  "Note in PARTS an edit of their text: its octets from START below END
replaced by INSERTED octets. The parts it may have touched, from the one
before START's to the one holding END, become one part to be split again
\(its first line, or the line a part begins on, may no longer begin a
headline); those after it move. Complexity: O(parts after it)."
  (let* ((first (org-part-index parts (max 0 (1- start))))
         (last (org-part-index parts end))
         (delta (- inserted (- end start)))
         (old (org-part-at parts first))
         (dirty (make-org-part nil (loop for i from first to last thereis (org-part-todo-lines (org-part-at parts i))))))
    ;; a part parsed in the background keeps its last parse while it is parsed again
    (when (= first last)
      (setf (org-part-state dirty) (org-part-state old)))
    (setf (org-parts-unsplit parts) (1+ (- (org-parts-unsplit parts)
                                           (loop for i from first to last count (null (org-part-level (org-part-at parts i))))))
          (org-parts-unsplit-from parts) (min first (org-parts-unsplit-from parts)))
    (org-parts-replace parts first last (list (org-part-start parts first)) (list dirty))
    (let ((starts (org-parts-starts parts)))
      (loop for i of-type fixnum from (1+ first) below (org-parts-count parts)
            do (incf (aref starts i) delta)))
    (incf (org-parts-bytes parts) delta)))

(defun org-parts-split (parts index rope changed)
  "Split again the part of PARTS at INDEX, which a change left unsplit, by
the headlines in it now, joined to the part before when its first line no
longer begins one; CHANGED, the number of the version that changed it. Two
values: the index of the part after those it became, and true when it held
a #+TODO: line or now holds one."
  (let ((todo (org-part-todo-lines (org-part-at parts index))))
    (when (and (plusp index)
               (not (integerp (org-line-kind (lambda (p) (tuatara.text:rope-octet rope p))
                                             (org-part-start parts index) (org-part-end parts index)))))
      (decf index)
      (setf todo (or todo (org-part-todo-lines (org-part-at parts index))))
      (org-parts-replace parts index (1+ index) (list (org-part-start parts index)) (list (make-org-part nil))))
    (let ((start (org-part-start parts index))
          (end (org-part-end parts index))
          (state (org-part-state (org-part-at parts index))))
      (multiple-value-bind (headlines todo-lines) (org-scan-parts rope start end)
        (let* ((begins (if (and headlines (= (car (first headlines)) start))
                           headlines
                           (cons (cons start 0) headlines)))
               (new (loop for ((begin . level) next) on begins
                          for part-end = (if next (car next) end)
                          collect (let ((part (make-org-part level
                                                             (loop for line in todo-lines
                                                                   when (and (<= begin line) (< line part-end))
                                                                     collect (- line begin)))))
                                    (setf (org-part-changed part) changed)
                                    part))))
          ;; the background parse of the part typed in goes on with the part that begins where it did
          (setf (org-part-state (first new)) state)
          (org-parts-replace parts index index (mapcar #'car begins) new)
          (decf (org-parts-unsplit parts))
          (values (+ index (length new)) (or todo (and todo-lines t))))))))

(defun org-parts-keywords (parts rope)
  "The TODO keywords the #+TODO: lines of the parts of PARTS set, in ROPE their text."
  (let ((lines (with-output-to-string (out)
                 (loop for i below (org-parts-count parts)
                       for part = (org-part-at parts i)
                       for start = (org-part-start parts i)
                       do (dolist (offset (org-part-todo-lines part))
                            (let* ((line (+ start offset))
                                   (end (or (tuatara.text:rope-find-octet rope 10 :start line) (tuatara.text:rope-bytes rope))))
                              ;; a CR LF line's return not one of its words
                              (write-line (string-right-trim '(#\Return) (tuatara.text:rope-string rope :start line :end end)) out)))))))
    (org-todo-keywords-of lines)))

(defun org-buffer-parts (buffer)
  "BUFFER's text as its parts (ORG-PARTS), as it is now: made the first time,
then kept up with each change, the parts each change touched split again and
the rest moved; the TODO keywords read again, and every parse let go, only
when a change touched a #+TODO: line. Complexity: O(the text) the first time
or after a change of the whole text; then O(the parts changed and their
size, and the parts after them moved)."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (rope (tuatara.text:version-rope version))
         (parts (tuatara.buffer:buffer-local buffer 'org-parts)))
    (unless (and parts (eq (org-parts-version parts) version))
      (let ((whole (or (null parts)
                       ;; the edits from the version the parts are of, one batch at a time
                       (loop for at = (org-parts-version parts) then next
                             for next = (tuatara.text:version-next at)
                             until (eq at version)
                             do (let ((batch (and next (tuatara.text:version-batch next))))
                                  (when (or (null batch) (tuatara.text:batch-swap batch))
                                    (return t))
                                  ;; from the last back, so that each is where the batch has it
                                  (let ((edits (tuatara.text:batch-edits batch)))
                                    (loop for i from (1- (length edits)) downto 0
                                          do (let ((edit (svref edits i)))
                                               (org-parts-edit parts (tuatara.text:edit-start edit) (tuatara.text:edit-end edit)
                                                               (tuatara.text:edit-insert-length edit))))))
                             finally (return nil)))))
        (when whole
          (setf parts (make-org-parts)
                (tuatara.buffer:buffer-local buffer 'org-parts) parts)
          (org-parts-replace parts 0 -1 (list 0) (list (make-org-part nil t)))
          (setf (org-parts-bytes parts) (tuatara.text:rope-bytes rope)
                (org-parts-unsplit parts) 1))
        (let ((changed (tuatara.text:version-number version))
              (todo nil))
          (loop with i = (org-parts-unsplit-from parts)
                while (plusp (org-parts-unsplit parts))
                do (if (org-part-level (org-part-at parts i))
                       (incf i)
                       (multiple-value-bind (next held) (org-parts-split parts i rope changed)
                         (setf i next)
                         (when held (setf todo t)))))
          (setf (org-parts-unsplit-from parts) (org-parts-count parts))
          (setf (org-parts-version parts) version)
          (when todo
            (let ((keywords (org-parts-keywords parts rope)))
              (unless (equal keywords (org-parts-todo-keywords parts))
                (setf (org-parts-todo-keywords parts) keywords)
                (loop for i below (org-parts-count parts)
                      for part = (org-part-at parts i)
                      do (setf (org-part-parse part) nil
                               (org-part-state part) nil
                               (org-part-changed part) changed))))))))
    parts))

;;; What is known of a stretch of the text

(defstruct (org-view (:constructor make-org-view (tree text from to returns current
                                                   &aux (plain (and current (null returns) (= (length text) (- to from))))))
                    (:copier nil))
  "What is known of a part of an Org buffer, the octets FROM below TO: TREE,
the parse of TEXT; RETURNS, where TEXT lost a carriage return before a
newline (ORG-STRIP-RETURNS); CURRENT, true when the parse is of the part as
it is now, false while a large part's parse in the background is behind it.
PLAIN, true when each character of TEXT is one octet of the part, so that
the one is found from the other at once; else BASE, the buffer's character
TEXT's first is, once it is asked for (ORG-VIEW-BYTE)."
  tree text from to returns current plain base)

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
    (multiple-value-bind (text returns) (if (buffer-crlf-p buffer) (org-strip-returns raw) (values raw nil))
      (let ((tree (org-parse text :todo-keywords keywords)))
        (org-warm-src-languages (org-src-language-names tree))
        (list tree text returns)))))

(defun org-part-view (buffer parts index)
  "The ORG-VIEW of the part of PARTS at INDEX, a part of BUFFER: parsed now
when it is no larger than *ORG-PARSE-AT-ONCE*, its parse kept until it
changes; a larger one parsed in the background, waited for a moment
\(*ORG-PARSE-WAIT*), its parse before given until that one is done, or NIL
while it has none."
  (let* ((part (org-part-at parts index))
         (from (org-part-start parts index))
         (to (org-part-end parts index))
         (keywords (org-parts-todo-keywords parts)))
    (if (<= (- to from) *org-parse-at-once*)
        (destructuring-bind (tree text returns)
            (or (org-part-parse part) (setf (org-part-parse part) (org-parse-part buffer from to keywords)))
          (make-org-view tree text from to returns t))
        (let ((state (or (org-part-state part) (setf (org-part-state part) (make-org-state))))
              (version (tuatara.buffer:buffer-version buffer)))
          (flet ((current-p () (>= (org-version-number (org-state-version state)) (org-part-changed part))))
            (org-take-parse state)
            (unless (current-p)
              (org-want-parse buffer state version :start from :end to :todo-keywords keywords)
              (when (org-await-parse state version)
                (org-take-parse state)))
            (and (org-state-tree state)
                 (make-org-view (org-state-tree state) (org-state-text state) from to (org-state-returns state)
                                (current-p))))))))

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
