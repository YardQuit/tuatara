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
CODE-LINES), the lines moved where the changes moved them."
  level todo-lines code-lines commented parse state (returns :unknown) (changed 0)
  (changed-at (get-internal-real-time)) touched)

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
                   until (or (null octet) (= octet 10))
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
                   (multiple-value-bind (kind commented) (org-line-kind #'octet-at line end)
                     (case kind
                       ((nil))
                       (:todo (push line todo-lines))
                       (:code (push line code-lines))
                       (t (push (list line kind commented) headlines))))
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

(defun org-moved-offsets (offsets from to delta)
  "OFFSETS, positions in order, after an edit replaced those from FROM below
TO: those before FROM as they were, those in it gone, the rest DELTA on."
  (loop for offset in offsets
        if (< offset from) collect offset
        else if (>= offset to) collect (+ offset delta)))

(defun org-parts-edit (parts start end inserted)
  "Note in PARTS an edit of their text: its octets from START below END
replaced by INSERTED octets. The parts it may have touched, from the one
before START's to the one holding END, become one part to be split again
\(its first line, or the line a part begins on, may no longer begin a
headline); those after it move. An edit within one part keeps what is known
of it (its level, its lines), only the lines it touched to be read again
\(ORG-PART's TOUCHED). Complexity: O(parts after it)."
  (let* ((first (org-part-index parts (max 0 (1- start))))
         (last (org-part-index parts end))
         (delta (- inserted (- end start)))
         (old (org-part-at parts first))
         (dirty (make-org-part nil (loop for i from first to last thereis (org-part-todo-lines (org-part-at parts i))))))
    (when (= first last)
      (let* ((base (org-part-start parts first))
             (from (- start base)) (to (- end base))
             (touched (org-part-touched old)))
        ;; a part parsed in the background keeps its last parse while it is parsed again
        (setf (org-part-state dirty) (org-part-state old))
        (when (or (org-part-level old) touched)
          (setf (org-part-touched dirty)
                (destructuring-bind (level commented low high todo code)
                    (or touched (list (org-part-level old) (org-part-commented old) from from
                                      (org-part-todo-lines old) (org-part-code-lines old)))
                  (list level commented (min low from)
                        (max (+ from inserted) (cond ((<= high from) high) ((< high to) (+ from inserted)) (t (+ high delta))))
                        (org-moved-offsets todo from to delta) (org-moved-offsets code from to delta)))))))
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
longer begins one; CHANGED, the number of the version that changed it. Only
the lines a change touched are read when what is known of the rest was kept
\(ORG-PART's TOUCHED), else all of it. Two values: the index of the part
after those it became, and true when it held a #+TODO: line or now holds
one."
  (let* ((dirty (org-part-at parts index))
         (todo (org-part-todo-lines dirty))
         (touched (org-part-touched dirty)))
    (when (and (plusp index)
               (not (integerp (org-line-kind (lambda (p) (tuatara.text:rope-octet rope p))
                                             (org-part-start parts index) (org-part-end parts index)))))
      (decf index)
      (setf todo (or todo (org-part-todo-lines (org-part-at parts index)))
            touched nil)
      (org-parts-replace parts index (1+ index) (list (org-part-start parts index)) (list (make-org-part nil))))
    (let* ((start (org-part-start parts index))
           (end (org-part-end parts index))
           (state (org-part-state (org-part-at parts index)))
           ;; the lines to read: those the changes touched, from the start of the first to the end of the last
           (from (if touched
                     (let ((at (+ start (third touched))))
                       (if (> at start) (1+ (or (tuatara.text:rope-find-octet-backward rope 10 :start start :end at) (1- start))) start))
                     start))
           (to (if touched
                   (let ((newline (tuatara.text:rope-find-octet rope 10 :start (min end (+ start (fourth touched))) :end end)))
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
        (let* ((begins (cond ((and headlines (= (car (first headlines)) start)) headlines)
                             ((and touched (> from start)) (cons (list start (first touched) (second touched)) headlines))
                             (t (cons (list start 0 nil) headlines))))
               (new (loop for ((begin level commented) next) on begins
                          for part-end = (if next (car next) end)
                          collect (flet ((within (lines)
                                           (loop for line in lines
                                                 when (and (<= begin line) (< line part-end))
                                                   collect (- line begin))))
                                    (let ((part (make-org-part level (within todo-lines))))
                                      (setf (org-part-code-lines part) (within code-lines)
                                            (org-part-commented part) commented
                                            (org-part-changed part) changed)
                                      part)))))
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
            (unless (or (current-p) (org-part-parse-later part state))
              (org-want-parse buffer state version (lambda () (cons from to)) :todo-keywords keywords)
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

;;; Carriage returns

(defun org-part-returns-now (buffer parts index)
  "Where the text Org reads of the part of PARTS at INDEX, a part of BUFFER,
lost a carriage return before a newline: its parse's, or found in its
octets once and kept with it."
  (let ((part (org-part-at parts index)))
    (cond ((org-part-parse part) (third (org-part-parse part)))
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

(defstruct (org-snapshot (:constructor make-org-snapshot (parts keywords entries crlf)) (:copier nil))
  "What the parse of a whole Org buffer is made from, taken from its PARTS
\(ORG-PARTS) at one version: ENTRIES, a vector of (PART LEVEL SOURCE) in
order, SOURCE the part's parse (TREE TEXT RETURNS) or, for one not parsed
yet, its text, a string or (ROPE START END); the TODO KEYWORDS the parts
are parsed with; CRLF, true when its lines end with CR LF."
  parts keywords entries crlf)

(setf (documentation 'org-snapshot-p 'function) "True when OBJECT is an ORG-SNAPSHOT.")

(defun org-part-parse-now (part)
  "The parse of PART, a part of an Org buffer, as it is now, (TREE TEXT
RETURNS), or NIL while there is none: its own, or for a large one, its parse
in the background when that is of its text as it is now."
  (or (org-part-parse part)
      (let ((state (org-part-state part)))
        (and state (org-state-tree state)
             (>= (org-version-number (org-state-version state)) (org-part-changed part))
             (list (org-state-tree state) (org-state-text state) (org-state-returns state))))))

(defun org-parts-snapshot (buffer parts)
  "An ORG-SNAPSHOT of PARTS, BUFFER's parts as they are now: the parses made
so far, and for the others where their text is, a copy of it when BUFFER's
file is mapped from disk (the mapping may go with the buffer).
Complexity: O(parts), and the octets of the parts not parsed of a mapped file."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (mapped (tuatara.buffer:buffer-mapping buffer))
        (entries (make-array (org-parts-count parts))))
    (dotimes (i (org-parts-count parts))
      (let ((part (org-part-at parts i))
            (from (org-part-start parts i))
            (to (org-part-end parts i)))
        (setf (svref entries i)
              (list part (org-part-level part)
                    (or (org-part-parse-now part)
                        (if mapped
                            (tuatara.buffer:buffer-string buffer :start from :end to)
                            (list rope from to)))))))
    (make-org-snapshot parts (org-parts-todo-keywords parts) entries (buffer-crlf-p buffer))))

(defun org-copy-node (node offset parent)
  "A copy of NODE and of what is in it, every position OFFSET characters on,
its parent PARENT."
  (flet ((moved (position) (and position (+ position offset))))
    (let ((copy (make-org-node (org-node-type node)
                               :begin (moved (org-node-begin node)) :end (moved (org-node-end node))
                               :contents-begin (moved (org-node-contents-begin node))
                               :contents-end (moved (org-node-contents-end node))
                               :post-blank (org-node-post-blank node)
                               :post-affiliated (moved (org-node-post-affiliated node)))))
      (setf (org-node-parent copy) parent
            (org-node-properties copy)
            (loop for (key value) on (org-node-properties node) by #'cddr
                  unless (eq key :children-vector)
                    nconc (list key (case key
                                      ((:title-begin :title-end) (moved value))
                                      (:title (mapcar (lambda (object) (org-copy-node object offset copy)) value))
                                      (t value))))
            (org-node-children copy)
            (mapcar (lambda (child) (org-copy-node child offset copy)) (org-node-children node)))
      copy)))

(defun org-assemble (snapshot)
  "The parse of a whole Org buffer made of its parts' (SNAPSHOT, an
ORG-SNAPSHOT), as ORG-PARSE makes it of the whole text: four values, the
tree, the text, where the text lost a carriage return before a newline
\(ORG-STRIP-RETURNS) and the parses made here of the parts not parsed
before, each (PART . PARSE). A headline holds the headlines after it up to
the next of as many stars or fewer (a space after them), as its subtree,
and its contents and blank lines are counted as far as that.
Complexity: O(the nodes of the parse and the characters of the text), and the
parts parsed here."
  (let ((keywords (org-snapshot-keywords snapshot))
        (made '()) (pieces '()) (returns '()) (offset 0))
    ;; each part's parse and where its text begins in the whole
    (let ((placed (loop for (part level source) across (org-snapshot-entries snapshot)
                        collect (destructuring-bind (tree text part-returns)
                                    (if (and (consp source) (org-node-p (first source)))
                                        source
                                        (let ((raw (if (stringp source)
                                                       source
                                                       (destructuring-bind (rope from to) source
                                                         (tuatara.text:rope-string rope :start from :end to)))))
                                          (multiple-value-bind (text part-returns)
                                              (if (org-snapshot-crlf snapshot) (org-strip-returns raw) (values raw nil))
                                            (let ((parse (list (org-parse text :todo-keywords keywords) text part-returns)))
                                              (push (cons part parse) made)
                                              parse))))
                                  (push text pieces)
                                  (loop for r across (or part-returns #()) do (push (+ offset r) returns))
                                  (prog1 (list level tree text offset)
                                    (incf offset (length text)))))))
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
          (loop for (level tree text start) in placed
                for part-end = (+ start (length text))
                do (if (zerop level)
                       (dolist (child (org-node-children tree))
                         (adopt (org-copy-node child start root)))
                       (let* ((headline (first (org-node-children tree)))
                              ;; stars and a space end the subtrees of as many stars or more; a tab ends none
                              (closes (char= #\Space (char text level)))
                              (outermost (and closes (position-if (lambda (entry) (>= (second entry) level)) open :from-end t))))
                         (when outermost
                           (loop repeat (1+ outermost) do (close-subtree (pop open) start)))
                         (let ((copy (org-copy-node headline start (if open (car (first open)) root))))
                           (adopt copy)
                           (push (list* copy level part-end '()) open)))))
          (loop while open do (close-subtree (pop open) total))
          (setf (org-node-children root) (nreverse root-children))
          (values root
                  (let ((text (make-string total)) (at 0))
                    (dolist (piece (nreverse pieces) text)
                      (replace text piece :start1 at)
                      (incf at (length piece))))
                  (and returns (coerce (nreverse returns) '(simple-array fixnum (*))))
                  (nreverse made)))))))

(defun org-keep-part-parses (snapshot made)
  "Keep MADE, the parses ORG-ASSEMBLE made of parts of SNAPSHOT not parsed
before, each (PART . PARSE), as those parts' own, while the TODO keywords
they were made with are those of the parts still; a part changed since is
no longer among them, and a large one keeps its own parse."
  (when (eq (org-parts-todo-keywords (org-snapshot-parts snapshot)) (org-snapshot-keywords snapshot))
    (loop for (part . parse) in made
          when (and (org-part-level part) (null (org-part-parse part)) (null (org-part-state part)))
            do (setf (org-part-parse part) parse))))
;;; What says which source blocks a buffer has

(defun org-blocks-key (buffer)
  "What the source blocks of BUFFER, an Org buffer, and what they are depend
on, as data to compare, read from its parts (ORG-BUFFER-PARTS): for each
part with lines that may say where blocks are and what they are (ORG-PART's
CODE-LINES) or a COMMENT headline, its level and those lines, or all of its
text when it gives :header-args; and every part's level when some part has
those or such a headline, as they decide which headlines a block is under.
Equal for two texts whose blocks, their languages, header arguments and
commented subtrees are the same. Complexity: O(parts + those lines)."
  (let* ((parts (org-buffer-parts buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (nesting nil)
         (entries
           (loop for i below (org-parts-count parts)
                 for part = (org-part-at parts i)
                 when (or (org-part-code-lines part) (org-part-commented part))
                   collect (let* ((start (org-part-start parts i))
                                  (lines (loop for offset in (org-part-code-lines part)
                                               collect (let* ((line (+ start offset))
                                                              (end (or (tuatara.text:rope-find-octet rope 10 :start line)
                                                                       (tuatara.text:rope-bytes rope))))
                                                         (string-trim '(#\Space #\Tab #\Return)
                                                                      (tuatara.text:rope-string rope :start line :end end))))))
                             (when (or (org-part-commented part)
                                       (find-if (lambda (line) (and (plusp (length line)) (char= (char line 0) #\:))) lines))
                               (setf nesting t))
                             (list (org-part-level part) (org-part-commented part)
                                   (if (find-if (lambda (line) (and (plusp (length line)) (char= (char line 0) #\:))) lines)
                                       (tuatara.text:rope-string rope :start start :end (org-part-end parts i))
                                       lines))))))
    (cons entries (and nesting (loop for i below (org-parts-count parts) collect (org-part-level (org-part-at parts i)))))))
