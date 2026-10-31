;;;; src/buffer/buffer.lisp --- Buffers: a version chain, point and mark, undo, a file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.buffer)

(defclass buffer ()
  ((name :initarg :name :accessor buffer-name
         :documentation "The buffer's name, unique among live buffers.")
   (killed :initform nil :accessor buffer-killed-p
           :documentation "True once the buffer has been killed (KILL-BUFFER).")
   (version :initarg :version :accessor buffer-version
            :documentation "The current text version (tuatara.text:version).")
   (point :accessor buffer-point-anchor
          :documentation "An anchor (bias :after) holding point.")
   (mark :initform nil :accessor buffer-mark-anchor
         :documentation "An anchor holding the mark, or NIL when no mark is set.")
   (mark-active :initform nil :accessor buffer-mark-active-p
                :documentation "True while the region between point and mark is active.")
   (narrowing :initform nil :accessor buffer-narrowing
              :documentation "While the buffer is narrowed (NARROW-BUFFER), the anchors its part shown starts and ends at, (START . END); NIL while all of it is.")
   (file :initform nil :initarg :file :accessor buffer-file
         :documentation "The pathname the buffer visits, or NIL.")
   (mapping :initform nil :accessor buffer-mapping
            :documentation "The tuatara.text:mapping of the visited file while it is memory-mapped, or NIL.")
   (lent :initform nil :accessor buffer-lent-p
         :documentation "True once text of the buffer is kept elsewhere sharing its octets (BUFFER-SLICE), so that its file must stay readable after the buffer is killed.")
   (saved-number :initform 0 :accessor buffer-saved-number
                 :documentation "The content number last saved or loaded; the buffer is modified when its content number differs.")
   (content-number :initform 0 :accessor buffer-content-number
                   :documentation "A number for what the text holds now: a new one with every edit, and the one it had before (or after) when undo (or redo) brings a text back, so that undoing back to the text last saved leaves the buffer unmodified.")
   (batch-contents :initform (make-hash-table :test 'eq :weakness :key) :accessor buffer-batch-contents
                   :documentation "For each batch on the undo and redo stacks, the content numbers before and after it, as a cons.")
   (undo :initform '() :accessor buffer-undo-stack
         :documentation "Undo groups, newest first; each group is a list of batches, newest first, undone together.")
   (redo :initform '() :accessor buffer-redo-stack
         :documentation "Groups undone and available for redo, newest first.")
   (undo-held :initform 0 :accessor buffer-undo-held
              :documentation "The octets of memory the undo stack holds (BATCH-HELD-OCTETS of each batch on it).")
   (redo-held :initform 0 :accessor buffer-redo-held
              :documentation "The octets of memory the redo stack holds, counted as for the undo stack.")
   (swap-held :initform (make-hash-table :test 'eq :weakness :key) :accessor buffer-swap-held
              :documentation "For each batch on the undo and redo stacks that swapped the whole text, the octets it holds, worked out once.")
   (locals :initform (make-hash-table :test 'eq :synchronized t) :accessor buffer-locals
           :documentation "Buffer-local variable values, keyed by symbol; safe to read and write from
several threads at once (redisplay reads them while an evaluation may write them).")
   (change-functions :initform '() :accessor buffer-change-functions
                     :documentation "Functions called with (BUFFER BATCH) after every edit batch."))
  (:documentation "A buffer: an editable text with a history, point and mark, and optionally a file.
Every edit commits a new version; snapshots are versions and stay valid."))

(setf (documentation 'buffer-name 'function) "The name of BUFFER, unique among live buffers."
      (documentation 'buffer-version 'function) "The current text version of BUFFER; setting it is for the buffer's own editing functions."
      (documentation 'buffer-file 'function) "The pathname BUFFER visits, or NIL."
      (documentation 'buffer-mark-active-p 'function) "True while BUFFER's region between point and mark is active."
      (documentation 'buffer-narrowing 'function) "While BUFFER is narrowed, the anchors its part shown starts and ends at, (START . END); else NIL."
      (documentation 'buffer-undo-stack 'function) "Undo groups of BUFFER, newest first; each a list of batches undone together."
      (documentation 'buffer-redo-stack 'function) "Groups of BUFFER undone and available for redo, newest first."
      (documentation 'buffer-undo-held 'function) "The octets of memory BUFFER's undo stack holds."
      (documentation 'buffer-redo-held 'function) "The octets of memory BUFFER's redo stack holds."
      (documentation 'buffer-swap-held 'function) "BUFFER's table of the octets each swap of its whole text on its undo and redo stacks holds."
      (documentation 'buffer-change-functions 'function) "Functions called with (BUFFER BATCH) after every edit batch of BUFFER."
      (documentation 'buffer-content-number 'function) "The number for what BUFFER's text holds now (see the slot)."
      (documentation 'buffer-batch-contents 'function) "BUFFER's table of the content numbers before and after each batch."
      (documentation 'buffer-mapping 'function) "The mapping of the file BUFFER's text reads, while it reads one, or NIL."
      (documentation 'buffer-lent-p 'function) "True once text of BUFFER is kept elsewhere sharing its octets (BUFFER-SLICE)."
      (documentation 'buffer-saved-number 'function) "The content number of BUFFER last saved or read; it is modified when its content number differs.")

;; the jobs in the background name their buffers in their reports
(setf tuatara.runtime:*job-buffer-name* (lambda (buffer) (if buffer (buffer-name buffer) "no buffer")))

(defvar *buffers* '()
  "All live buffers, most recently created first.")

(defvar *find-file-functions* '()
  "Functions called with a buffer that FIND-FILE-BUFFER has just created for a
file: this is where the defaults choose its major mode.")

(defvar *kill-buffer-functions* '()
  "Functions called with a buffer just before KILL-BUFFER removes it, to release
what was attached to it (parsers, processes).")

(defvar *before-edit-functions* '()
  "Functions called with (BUFFER EDITS) before EDITS change BUFFER's text: a
vector of TUATARA.TEXT:EDIT in the text as it is now, sorted, a whole text
swapped given as one edit from 0 to its end. One may signal an error to refuse
them: nothing is changed then. The defaults ask here before an edit spoils the
work of a job running over the text it touches.")

(defun run-before-edit (buffer edits)
  "Call *BEFORE-EDIT-FUNCTIONS* with BUFFER and EDITS, a vector of
TUATARA.TEXT:EDIT about to change its text, or :ALL for the whole text
swapped. O(functions) when they have nothing to say of EDITS."
  (when *before-edit-functions*
    (let ((edits (if (eq edits :all) (vector (tuatara.text:make-edit 0 (buffer-size buffer))) edits)))
      (dolist (f *before-edit-functions*) (funcall f buffer edits)))))

(defun text-rope (text)
  "TEXT, a string, an octet vector or a rope, as a rope."
  (etypecase text
    (tuatara.text:rope text)
    (string (tuatara.text:string->rope text))
    ((array (unsigned-byte 8) (*)) (tuatara.text:octets->rope (coerce text 'tuatara.text:octets)))))

(defun text-octets (text)
  "TEXT, a string, an octet vector or a rope, as octets in UTF-8."
  (etypecase text
    (string (tuatara.text:string-octets text))
    ((array (unsigned-byte 8) (*)) (coerce text 'tuatara.text:octets))
    (tuatara.text:rope (tuatara.text:rope-octets text))))

(defun make-buffer (name &key (text "") file)
  "Create and return a buffer called NAME holding TEXT (a string, an octet vector or
a rope), visiting FILE if given. NAME must not be in use."
  (when (find-buffer name)
    (error "A buffer named ~s already exists." name))
  (let* ((rope (text-rope text))
         (version (tuatara.text:make-version rope))
         (buffer (make-instance 'buffer :name name :version version :file file)))
    (setf (buffer-point-anchor buffer) (tuatara.text:make-anchor version 0 :bias :after)
          (buffer-content-number buffer) (tuatara.text:version-number version)
          (buffer-saved-number buffer) (tuatara.text:version-number version))
    (push buffer *buffers*)
    buffer))

(defun find-buffer (name)
  "Return the live buffer called NAME, or NIL."
  (find name *buffers* :key #'buffer-name :test #'string=))

(defun live-buffer-p (buffer)
  "True when BUFFER is a buffer that has not been killed."
  (and (typep buffer 'buffer) (not (buffer-killed-p buffer))))

(defun kill-buffer (buffer)
  "Remove BUFFER from the live buffers and release its file mapping, after
running *KILL-BUFFER-FUNCTIONS*. The mapping of a buffer whose text is kept
elsewhere (BUFFER-LENT-P) is released once nothing reads it any more. A
function that fails does not stop the others or the removal: the first
failure is signalled once the buffer is gone."
  (let ((failure nil))
    (dolist (f *kill-buffer-functions*)
      (handler-case (tuatara.runtime:reporting-errors ("~(~a~) for ~a" f (buffer-name buffer))
                      (funcall f buffer))
        (error (condition) (unless failure (setf failure condition)))))
    (setf *buffers* (remove buffer *buffers*)
          (buffer-killed-p buffer) t)
    (when (buffer-mapping buffer)
      (unless (buffer-lent-p buffer)
        (tuatara.text:close-mapping (buffer-mapping buffer)))
      (setf (buffer-mapping buffer) nil))
    (when failure (error failure)))
  nil)

;;; Text access

(declaim (inline buffer-text))
(defun buffer-text (buffer)
  "BUFFER's text as it is now, a rope. A change to BUFFER makes a new rope
and leaves this one as it is, so work in the background may read it
\(ROPE-OCTET, ROPE-COPY-INTO and the rest) while BUFFER is edited, the text
it reads being the text as it was when it was taken."
  (tuatara.text:version-rope (buffer-version buffer)))

(defun buffer-size (buffer)
  "Number of octets in BUFFER."
  (tuatara.text:rope-bytes (buffer-text buffer)))

(defun buffer-string (buffer &key (start 0) (end (buffer-size buffer)))
  "BUFFER's text from byte START below END as a string. The whole text by
default, whether or not BUFFER is narrowed (BUFFER-START and BUFFER-END give
the part shown)."
  (tuatara.text:rope-string (buffer-text buffer) :start start :end end))

(defun buffer-slice (buffer start end)
  "BUFFER's text from byte START below END as a rope sharing the buffer's
octets rather than copying them (TUATARA.TEXT:ROPE-SLICE), made at once
whatever its size, for text kept elsewhere, such as a large kill; while the
buffer's lines are still being counted too, the slice then holding text not
yet counted, which a buffer it goes into counts in the background as it does
a large file just opened. The buffer is then lent (BUFFER-LENT-P): once
killed, its file stays mapped, and guarded against other programs, until
nothing holds text read from it."
  (check-position buffer start) (check-position buffer end)
  (when (< end start) (rotatef start end))
  (setf (buffer-lent-p buffer) t)
  (tuatara.text:rope-slice (buffer-text buffer) start end))

(defun buffer-char-at (buffer byte)
  "The character starting at byte offset BYTE of BUFFER, or NIL at the end."
  (tuatara.text:rope-char-at (buffer-text buffer) byte))

(defun buffer-modified-p (buffer)
  "True when BUFFER's text differs from what was last saved or loaded: its
content number is not the saved one."
  (/= (buffer-content-number buffer) (buffer-saved-number buffer)))

(defun buffer-mark-saved (buffer &optional (content (buffer-content-number buffer)))
  "Take CONTENT, a content number of BUFFER (its present one by default), as the
text saved: BUFFER is unmodified when it holds that text. For buffers whose text
the editor makes (help, lists, logs), and for saving."
  (setf (buffer-saved-number buffer) content))

(defun note-new-content (buffer batch)
  "BUFFER's text changed by BATCH into something new: give it a new content
number, and remember the numbers before and after BATCH for undo and redo."
  (let ((before (buffer-content-number buffer))
        (after (tuatara.text:version-number (buffer-version buffer))))
    (setf (buffer-content-number buffer) after
          (gethash batch (buffer-batch-contents buffer)) (cons before after))))

;;; Point and mark

(defmethod (setf buffer-mark-active-p) :after ((value null) (buffer buffer))
  "Letting go of the region ends a rectangle made of it: the next is plain."
  (let ((locals (buffer-locals buffer)))
    (remhash :rectangle locals)
    (remhash :rectangle-point locals)))

(defun buffer-point (buffer)
  "Point of BUFFER as a byte offset in its current version."
  (tuatara.text:anchor-position (buffer-point-anchor buffer) (buffer-version buffer)))

(defun (setf buffer-point) (byte buffer)
  "Move point of BUFFER to byte offset BYTE, which must be a character
boundary; while BUFFER is narrowed, no further than the edges of the part
shown (BUFFER-START, BUFFER-END)."
  (check-position buffer byte)
  (let ((byte (within-narrowing buffer byte)))
    (tuatara.text:anchor-move (buffer-point-anchor buffer) (buffer-version buffer) byte)
    byte))

(defun buffer-mark (buffer)
  "The mark of BUFFER as a byte offset, or NIL when no mark is set."
  (let ((anchor (buffer-mark-anchor buffer)))
    (and anchor (tuatara.text:anchor-position anchor (buffer-version buffer)))))

(defun (setf buffer-mark) (byte buffer)
  "Set the mark of BUFFER to byte offset BYTE, or remove it with NIL; while
BUFFER is narrowed, no further than the edges of the part shown."
  (if (null byte)
      (setf (buffer-mark-anchor buffer) nil (buffer-mark-active-p buffer) nil)
      (progn (check-position buffer byte)
             (setf byte (within-narrowing buffer byte))
             (if (buffer-mark-anchor buffer)
                 (tuatara.text:anchor-move (buffer-mark-anchor buffer) (buffer-version buffer) byte)
                 (setf (buffer-mark-anchor buffer)
                       (tuatara.text:make-anchor (buffer-version buffer) byte :bias :before)))))
  byte)

(defun buffer-region (buffer)
  "Two values: the start and end byte offsets of the region between point and
mark, ordered, or NIL when no mark is set."
  (let ((mark (buffer-mark buffer)))
    (when mark
      (let ((point (buffer-point buffer)))
        (values (min point mark) (max point mark))))))

(defun check-position (buffer byte)
  "Signal an error unless BYTE is a character boundary inside BUFFER."
  (let ((rope (buffer-text buffer)))
    (unless (and (typep byte 'fixnum) (<= 0 byte (tuatara.text:rope-bytes rope)))
      (error "Position ~a is outside buffer ~s (0..~a)." byte (buffer-name buffer) (tuatara.text:rope-bytes rope)))
    (unless (tuatara.text:char-boundary-p rope byte)
      (error "Position ~a in buffer ~s is inside a character." byte (buffer-name buffer)))))

;;; Narrowing

(defvar *narrowing-in-force* nil
  "The buffer a command runs in, while it runs: an edit of that buffer
reaching outside the part it is narrowed to is refused then. Edits of other
buffers, the editor's own writing (WITH-READ-ONLY-LIFTED), and work in the
background, taken in at once or later, are not held to it.")

(defun buffer-start (buffer)
  "Where the part of BUFFER that commands reach starts: the start of the part
it is narrowed to (NARROW-BUFFER), else 0, the start of its text. O(1)."
  (let ((narrowing (buffer-narrowing buffer)))
    (if narrowing
        (tuatara.text:anchor-position (car narrowing) (buffer-version buffer))
        0)))

(defun buffer-end (buffer)
  "Where the part of BUFFER that commands reach ends: the end of the part it
is narrowed to (NARROW-BUFFER), else BUFFER-SIZE, the end of its text. O(1)."
  (let ((narrowing (buffer-narrowing buffer)))
    (if narrowing
        (let ((version (buffer-version buffer)))
          (max (tuatara.text:anchor-position (car narrowing) version)
               (tuatara.text:anchor-position (cdr narrowing) version)))
        (buffer-size buffer))))

(defun buffer-bounds (buffer)
  "Two values: the start and end of the part of BUFFER that commands reach,
BUFFER-START and BUFFER-END; all of its text while it is not narrowed."
  (values (buffer-start buffer) (buffer-end buffer)))

(defun buffer-narrowed-p (buffer)
  "True while BUFFER is narrowed to a part of its text (NARROW-BUFFER)."
  (and (buffer-narrowing buffer) t))

(defun within-narrowing (buffer byte)
  "BYTE, an offset of BUFFER's text, moved to the nearer edge of the part
BUFFER is narrowed to when it is outside it; BYTE itself while BUFFER is not
narrowed."
  (if (buffer-narrowing buffer)
      (max (buffer-start buffer) (min byte (buffer-end buffer)))
      byte))

(defun keep-within-narrowing (buffer)
  "Move BUFFER's point and mark into the part it is narrowed to, where a
change made outside it (undone, or taken in from work in the background)
left them outside."
  (when (buffer-narrowing buffer)
    (let ((point (buffer-point buffer)) (mark (buffer-mark buffer)))
      (unless (= point (within-narrowing buffer point))
        (setf (buffer-point buffer) point))
      (when (and mark (/= mark (within-narrowing buffer mark)))
        (setf (buffer-mark buffer) mark)))))

(defun narrow-buffer (buffer start end)
  "Narrow BUFFER to its text from octet START below END: commands then reach
only that part of it (BUFFER-START, BUFFER-END), and windows show only that.
Its edges move with the text as it is edited, what is typed at either going
inside. Point and the mark are moved inside. Saving, and work in the
background begun before, still reach the whole text."
  (check-position buffer start) (check-position buffer end)
  (when (< end start) (rotatef start end))
  (let ((version (buffer-version buffer)))
    (setf (buffer-narrowing buffer)
          (cons (tuatara.text:make-anchor version start :bias :before)
                (tuatara.text:make-anchor version end :bias :after))))
  (keep-within-narrowing buffer)
  buffer)

(defun widen-buffer (buffer)
  "Make all of BUFFER's text reachable and shown again (NARROW-BUFFER)."
  (setf (buffer-narrowing buffer) nil)
  buffer)

(defvar *narrowings-lifted* '()
  "The buffers whose narrowing WITH-NARROWING-LIFTED lifts now, each (BUFFER
. NARROWING), NARROWING the anchors it puts back, or NIL once a jump outside
them (LET-GO-OF-LIFTED-NARROWING) has the buffer stay widened.")

(defun lifted-narrowing-outside-p (buffer byte)
  "True when BUFFER's narrowing is lifted now (WITH-NARROWING-LIFTED) and BYTE
is outside the part it is to be narrowed to again."
  (let ((narrowing (cdr (assoc buffer *narrowings-lifted*))))
    (and narrowing
         (let* ((version (buffer-version buffer))
                (start (tuatara.text:anchor-position (car narrowing) version))
                (end (max start (tuatara.text:anchor-position (cdr narrowing) version))))
           (not (<= start byte end))))))

(defun let-go-of-lifted-narrowing (buffer)
  "Have BUFFER, whose narrowing is lifted now, stay widened once the change
lifting it is done, point where that leaves it: a jump went outside it."
  (let ((entry (assoc buffer *narrowings-lifted*)))
    (when entry (setf (cdr entry) nil))))

(defun call-with-narrowing-lifted (buffer function)
  "Call FUNCTION with all of BUFFER's text reachable, as WITH-NARROWING-LIFTED
says, and return what it returns."
  (let ((narrowing (buffer-narrowing buffer)))
    (if (null narrowing)
        (funcall function)
        ;; where point is, held before what goes in at it, as it stood
        (let* ((here (tuatara.text:make-anchor (buffer-version buffer) (buffer-point buffer) :bias :before))
               (entry (cons buffer narrowing))
               (*narrowings-lifted* (cons entry *narrowings-lifted*)))
          (setf (buffer-narrowing buffer) nil)
          (unwind-protect (let ((*narrowing-in-force* nil)) (funcall function))
            (when (cdr entry)
              (setf (buffer-narrowing buffer) narrowing
                    (buffer-point buffer) (tuatara.text:anchor-position here (buffer-version buffer)))
              (keep-within-narrowing buffer)))))))

(defmacro with-narrowing-lifted ((buffer) &body body)
  "Run BODY with all of BUFFER's text reachable though it is narrowed, for a
change asked for on something outside the part shown (a task clocked out,
an entry of the agenda, a name renamed in every file); then narrow it again
as it was, its edges moved by the changes, point back where it was, before
what went in there. A jump BODY makes outside the part (to a subtree just
refiled) leaves the buffer widened instead, point where the jump took it."
  `(call-with-narrowing-lifted ,buffer (lambda () ,@body)))

(defvar *widen-key-function* nil
  "A function of no arguments giving the key that widens a buffer as messages
name it (C-x x w), for OUTSIDE-NARROWING-WORDS; NIL names the command, widen.")

(defun outside-narrowing-words (buffer)
  "The words saying a change would reach outside the part of BUFFER shown,
and that widening (the key *WIDEN-KEY-FUNCTION* gives) shows the rest."
  (format nil "That would change text outside the part of ~a shown: ~a shows all of it again"
          (buffer-name buffer) (if *widen-key-function* (funcall *widen-key-function*) "widen")))

(defun refuse-outside-narrowing (buffer)
  "Signal the error that says a change would reach outside the part of
BUFFER shown (OUTSIDE-NARROWING-WORDS)."
  (error "~a" (outside-narrowing-words buffer)))

(defun check-within-narrowing (buffer start end)
  "Signal an error when an edit of BUFFER from octet START to END reaches
outside the part it is narrowed to while a command runs in BUFFER. A job
taken in is not refused, whenever it is taken in: one begun while BUFFER
was narrowed works within the bounds it was given."
  (when (and (eq *narrowing-in-force* buffer) (null tuatara.runtime:*job-taken-in*) (buffer-narrowing buffer)
             (or (< start (buffer-start buffer)) (> end (buffer-end buffer))))
    (refuse-outside-narrowing buffer)))

;;; How much the undo history holds

(defvar *undo-limit* nil
  "The most memory, in octets, the undo history of one buffer may hold; NIL,
the default, for no limit.

With NIL every change is kept, so that each can be undone, back to the text
as it was when the buffer was opened. Nothing is let go unless you ask for
it: in forensic work, a change silently made impossible to take back would
be evidence lost.

With a number, a buffer's undo and redo together keep at most that many
octets: the text each change took away and put in, about a hundred octets
more for each change, and, for a large change made in one piece (cutting or
replacing across a whole file), the part of the old text no longer in the
new. Text still read from the file on disk costs nothing and is not counted,
so cutting a gigabyte out of a file just opened holds little.

When a change takes a buffer past the limit, its oldest changes are let go
until about three quarters of the limit is held, so that this does not
happen at every key. Those oldest changes can no longer be undone; every
newer one still can, and redo works as before. The change just made is
always kept, however large. While work in the background (a large
replacement, a sort) runs on a buffer, nothing of its history is let go
until the work is done. The first time a buffer lets changes go, the editor
says so once, naming the buffer.

Some values and what they give:
  (* 64 1024 1024)    64 MB: room for most editing, memory kept small.
  (* 256 1024 1024)   256 MB: long sessions on large files stay undoable.
  (* 1024 1024 1024)  1 GB: a replacement of millions of matches in a large
                      file can still be undone.

The limit applies to every buffer alike.")

(defvar *undo-let-go-functions* '()
  "Functions called with a buffer the first time its oldest changes are let go
because its undo history reached *UNDO-LIMIT*: this is where the editor says
so.")

(defconstant +edit-octets+ 96
  "The octets of memory each edit kept for undo takes besides its text: its
record and its place in the change it belongs to.")

(defmethod (setf buffer-undo-stack) :after ((value null) (buffer buffer))
  "An emptied undo stack holds nothing."
  (setf (buffer-undo-held buffer) 0))

(defmethod (setf buffer-redo-stack) :after ((value null) (buffer buffer))
  "An emptied redo stack holds nothing."
  (setf (buffer-redo-held buffer) 0))

(defun swap-held-octets (swap)
  "The octets of memory SWAP, a swap of a whole text, keeps for undo: those of
the old text held in memory that the new text does not share. Text read from
a mapped file costs nothing."
  (let ((only-before nil))
    (tuatara.text:map-rope-sources
     (lambda (kind source start end)
       (declare (ignore start end))
       (when (eq kind :memory)
         (setf (gethash source (or only-before (setf only-before (make-hash-table :test 'eq)))) t)))
     (tuatara.text:swap-before swap))
    (if (null only-before)
        0
        (progn
          (tuatara.text:map-rope-sources
           (lambda (kind source start end)
             (declare (ignore start end))
             (when (eq kind :memory) (remhash source only-before)))
           (tuatara.text:swap-after swap))
          (loop for octets being the hash-keys of only-before sum (length octets))))))

(defun batch-held-octets (buffer batch)
  "The octets of memory BATCH, a change on BUFFER's undo or redo stack, holds:
for edits their text taken away and put in, and +EDIT-OCTETS+ each; for a swap
of the whole text SWAP-HELD-OCTETS, worked out once."
  (let ((swap (tuatara.text:batch-swap batch)))
    (if swap
        (let ((table (buffer-swap-held buffer)))
          (or (gethash batch table) (setf (gethash batch table) (swap-held-octets swap))))
        (loop for edit across (tuatara.text:batch-edits batch)
              sum (+ +edit-octets+ (tuatara.text:edit-insert-length edit) (tuatara.text:edit-delete-length edit))))))

(defun group-held-octets (buffer group)
  "The octets of memory GROUP, an undo group of BUFFER, holds."
  (loop for batch in group sum (batch-held-octets buffer batch)))

(defun buffer-undo-octets (buffer)
  "The octets of memory BUFFER's undo and redo history holds, as *UNDO-LIMIT*
counts them."
  (+ (buffer-undo-held buffer) (buffer-redo-held buffer)))

(defun add-held (buffer undo redo)
  "Add UNDO and REDO octets to what BUFFER's undo and redo stacks hold; an
empty stack holds nothing."
  (setf (buffer-undo-held buffer) (if (buffer-undo-stack buffer) (+ (buffer-undo-held buffer) undo) 0)
        (buffer-redo-held buffer) (if (buffer-redo-stack buffer) (+ (buffer-redo-held buffer) redo) 0)))

(defun let-go-of-oldest-changes (buffer)
  "When BUFFER's undo history holds more than *UNDO-LIMIT*, let go of its
oldest undo groups, whole, until about three quarters of the limit is held:
never its newest group, never one a place in its history (MAKE-UNDO-PLACE)
stands on or above, and nothing while a job of BUFFER is running or waiting
to be taken in. The first time, *UNDO-LET-GO-FUNCTIONS* are told."
  (let ((limit *undo-limit*))
    (when (and limit (> (buffer-undo-octets buffer) limit) (null (tuatara.runtime:jobs-of buffer)))
      (let* ((stack (buffer-undo-stack buffer))
             (places (loop for place in (buffer-local buffer 'undo-places)
                           when (tailp (car place) stack) collect (car place))))
        ;; a place at the very start of the history stands on every group
        (unless (member nil places)
          (let ((aim (- (floor (* 3 limit) 4) (buffer-redo-held buffer)))
                (kept (group-held-octets buffer (first stack))))
            (loop for cell on stack
                  for older = (cdr cell)
                  do (setf places (delete cell places))
                  while older
                  do (let ((held (group-held-octets buffer (car older))))
                       (when (and (null places) (> (+ kept held) aim))
                         (setf (cdr cell) nil
                               (buffer-undo-held buffer) kept)
                         (unless (buffer-local buffer 'undo-let-go)
                           (setf (buffer-local buffer 'undo-let-go) t)
                           (dolist (f *undo-let-go-functions*) (funcall f buffer)))
                         (return))
                       (incf kept held)))))))))

(defun record-batch (buffer batch &optional join)
  "Put BATCH, a change just made on BUFFER, on its undo stack as a group of its
own, or with JOIN in the newest group; forget what there was to redo, and let
go of the oldest changes past *UNDO-LIMIT*."
  (if (and join (buffer-undo-stack buffer))
      (push batch (first (buffer-undo-stack buffer)))
      (push (list batch) (buffer-undo-stack buffer)))
  (add-held buffer (batch-held-octets buffer batch) 0)
  (setf (buffer-redo-stack buffer) '())
  (let-go-of-oldest-changes buffer))

;;; Editing

(defvar *hex-view-resizing* nil
  "True while an edit may change the size of a buffer shown in the hex view:
typing there past the end of its text, or reading its file again.")

(defun hex-view-refuses-p (buffer change)
  "True when BUFFER is shown in the hex view and an edit would change its size
by CHANGE octets (not zero): there octets are typed over, never inserted or
deleted, unless *HEX-VIEW-RESIZING* is true."
  (and (/= change 0) (buffer-local buffer :hex-view) (not *hex-view-resizing*)))

(defun check-hex-view-size (buffer change)
  "Signal an error when an edit changing BUFFER's size by CHANGE octets is
refused there because it is shown in the hex view (HEX-VIEW-REFUSES-P)."
  (when (hex-view-refuses-p buffer change)
    (error "The hex view types over octets: nothing is inserted or deleted here (M-x hex-mode leaves it)")))

(defun buffer-edit (buffer edits &key group)
  "Apply EDITS (a sequence of TUATARA.TEXT:EDIT, sorted, not overlapping, at
byte offsets on character boundaries) to BUFFER as one undoable batch, and
return the positions vector (where each insertion ends). With GROUP true, the
batch joins the newest undo group instead of starting one, so that characters
typed one after another undo together. *BEFORE-EDIT-FUNCTIONS* see EDITS
first, and may refuse them; the change functions run after. An error when the
buffer-local :READ-ONLY is set, in the hex view (the buffer-local
:HEX-VIEW) for edits that would change the buffer's size
\(CHECK-HEX-VIEW-SIZE), and, while a command runs in BUFFER, for edits
outside the part of it shown when it is narrowed. A text whose lines are still being counted is edited
at once: the count is made again around the edits once it ends
\(TUATARA.IO:FINISH-INDEXING)."
  (when (buffer-local buffer :read-only)
    (error "Buffer ~a is read-only." (buffer-name buffer)))
  (when (buffer-local buffer :hex-view)
    (check-hex-view-size buffer (reduce #'+ edits :key (lambda (e) (- (tuatara.text:edit-insert-length e)
                                                                      (tuatara.text:edit-delete-length e))))))
  (when (and (buffer-narrowing buffer) (plusp (length edits)))
    (check-within-narrowing buffer (tuatara.text:edit-start (elt edits 0)) (tuatara.text:edit-end (elt edits (1- (length edits))))))
  (run-before-edit buffer edits)
  (multiple-value-bind (version batch) (tuatara.text:version-commit (buffer-version buffer) edits)
    (setf (buffer-version buffer) version)
    (note-new-content buffer batch)
    (record-batch buffer batch group)
    (dolist (f (buffer-change-functions buffer)) (funcall f buffer batch))
    (tuatara.text:batch-positions batch)))

(defvar *large-edit-octets* (* 64 1024 1024)
  "Octets from which a deletion, or an insertion of a rope, is made as one
splice of the text (BUFFER-SPLICE) rather than as an edit: undo keeps the old
text instead of a copy of what went, so that cutting or pasting gigabytes
copies none of them.")

(defun buffer-insert (buffer byte text &key group)
  "Insert TEXT (a string, an octet vector or a rope) at byte offset BYTE of
BUFFER and return the byte offset just after it. GROUP is passed to
BUFFER-EDIT. A rope of more than *LARGE-EDIT-OCTETS* goes in as one splice
(BUFFER-SPLICE), sharing its leaves. Offsets are of the whole text, narrowed
or not;
while a command runs in a narrowed BUFFER an edit reaching outside the part shown
(BUFFER-START, BUFFER-END) is refused, unless inside WITH-NARROWING-LIFTED."
  (check-position buffer byte)
  (if (and (tuatara.text:rope-p text) (> (tuatara.text:rope-bytes text) *large-edit-octets*))
      (buffer-splice buffer byte byte text)
      (let ((octets (text-octets text)))
        (if (zerop (length octets))
            byte
            (aref (buffer-edit buffer (vector (tuatara.text:make-edit byte byte octets)) :group group) 0)))))

(defun buffer-splice (buffer start end rope)
  "Put ROPE's text (NIL for none) in place of the octets from START below END
of BUFFER as one undoable step that copies nothing: the text is spliced
(TUATARA.TEXT:SPLICE-ROPE), and undo swaps the old one back. Positions before
START stay, those after END move with the text, those between go to START.
Refused in the hex view when it would change the size (CHECK-HEX-VIEW-SIZE),
and, while a command runs in BUFFER, outside the part of it shown when it is
narrowed.
Returns the offset just after the text put in."
  (check-position buffer start) (check-position buffer end)
  (when (< end start) (rotatef start end))
  (check-hex-view-size buffer (- (if rope (tuatara.text:rope-bytes rope) 0) (- end start)))
  (check-within-narrowing buffer start end)
  (ensure-indexed buffer)
  (let* ((old (buffer-text buffer))
         (length (if rope (tuatara.text:rope-bytes rope) 0))
         (new (if rope (tuatara.text:splice-rope old start end rope) (tuatara.text:splice-rope old start end)))
         (shift (- length (- end start))))
    (buffer-swap-text buffer new
                      (lambda (p bias)
                        (cond ((< p start) p)
                              ((>= p end) (+ p shift))
                              ((eq bias :after) (+ start length))
                              (t start)))
                      (lambda (p bias)
                        (declare (ignore bias))
                        (cond ((< p start) p)
                              ((>= p (+ start length)) (- p shift))
                              (t start))))
    (+ start length)))

(defun buffer-splice-spans (buffer spans rope)
  "Put ROPE's text (NIL for none) in place of the first of SPANS of BUFFER,
each (START . END), in order and apart, and take the others away, as one
undoable step that copies nothing, as BUFFER-SPLICE does for one stretch.
Positions in a span go to its start (in the first, after ROPE when their bias
is :AFTER), those after it move with the text. Refused in the hex view when
it would change the size (CHECK-HEX-VIEW-SIZE), while a command runs in
BUFFER outside the part of it shown when it is narrowed, and when a span's edge is not
a character's (CHECK-POSITION) or the spans are out of order. Returns the
offset just after ROPE; NIL, nothing done, for no SPANS."
  (when (null spans) (return-from buffer-splice-spans nil))
  (let ((before 0))
    (map nil (lambda (span)
               (check-position buffer (car span)) (check-position buffer (cdr span))
               (unless (<= before (car span) (cdr span))
                 (error "The spans of a splice must be in order and apart: ~a." span))
               (setf before (cdr span)))
         spans))
  (check-hex-view-size buffer (- (if rope (tuatara.text:rope-bytes rope) 0)
                                 (reduce #'+ spans :key (lambda (span) (- (cdr span) (car span))))))
  (check-within-narrowing buffer (car (elt spans 0)) (cdr (elt spans (1- (length spans)))))
  (ensure-indexed buffer)
  (let* ((spans (coerce spans 'simple-vector))
         (n (length spans))
         (length (if rope (tuatara.text:rope-bytes rope) 0))
         (shift (make-array (1+ n) :initial-element 0))   ; how far the text after span I-1 moves
         (new (buffer-text buffer)))
    (loop for i from 0 below n
          for (start . end) = (svref spans i)
          do (setf (svref shift (1+ i)) (+ (svref shift i) (if (zerop i) length 0) (- start end))))
    ;; from the last back, so that the offsets of those before stay good
    (loop for i from (1- n) downto 0
          for (start . end) = (svref spans i)
          do (setf new (if (and (zerop i) rope)
                           (tuatara.text:splice-rope new start end rope)
                           (tuatara.text:splice-rope new start end))))
    ;; the span a position is in or after, found by halves: the spans' starts, and where
    ;; they stand in the new text, both rise
    (flet ((forward (p bias)
             (let ((i (1- (tuatara.runtime:bisect (k 0 n) (<= (car (svref spans k)) p)))))
               (cond ((minusp i) p)
                     ((>= p (cdr (svref spans i))) (+ p (svref shift (1+ i))))
                     (t (+ (car (svref spans i)) (svref shift i)
                           (if (and (zerop i) (eq bias :after)) length 0))))))
           (backward (q bias)
             (declare (ignore bias))
             (let ((i (1- (tuatara.runtime:bisect (k 0 n) (<= (+ (car (svref spans k)) (svref shift k)) q)))))
               (if (minusp i)
                   q
                   (let ((here (+ (car (svref spans i)) (svref shift i))))
                     (if (< q (+ here (if (zerop i) length 0)))
                         (car (svref spans i))
                         (- q (svref shift (1+ i)))))))))
      (buffer-swap-text buffer new #'forward #'backward))
    (+ (car (svref spans 0)) length)))

(defun buffer-delete (buffer start end &key group)
  "Delete the octets from START below END of BUFFER. Returns START. More than
*LARGE-EDIT-OCTETS* go as one splice (BUFFER-SPLICE). With GROUP true, the
deletion joins the newest undo group, as BUFFER-EDIT says. Offsets are of the
whole text, narrowed or not;
while a command runs in a narrowed BUFFER an edit reaching outside the part shown
(BUFFER-START, BUFFER-END) is refused, unless inside WITH-NARROWING-LIFTED."
  (check-position buffer start) (check-position buffer end)
  (when (< end start) (rotatef start end))
  (cond ((> (- end start) *large-edit-octets*) (buffer-splice buffer start end nil))
        ((< start end) (buffer-edit buffer (vector (tuatara.text:make-edit start end)) :group group)))
  start)

(defun buffer-replace (buffer start end text)
  "Replace the octets from START below END of BUFFER with TEXT (a string, an
octet vector or a rope) and return the byte offset just after the new text.
More than *LARGE-EDIT-OCTETS* going or coming as a rope make one splice
(BUFFER-SPLICE). Offsets are of the whole text, narrowed or not;
while a command runs in a narrowed BUFFER an edit reaching outside the part shown
(BUFFER-START, BUFFER-END) is refused, unless inside WITH-NARROWING-LIFTED."
  (check-position buffer start) (check-position buffer end)
  (when (< end start) (rotatef start end))
  (if (or (> (- end start) *large-edit-octets*)
          (and (tuatara.text:rope-p text) (> (tuatara.text:rope-bytes text) *large-edit-octets*)))
      (buffer-splice buffer start end (text-rope text))
      (let ((octets (text-octets text)))
        (aref (buffer-edit buffer (vector (tuatara.text:make-edit start end (if (plusp (length octets)) octets nil)))) 0))))

(defun commit-without-record (buffer edits)
  "Commit EDITS to BUFFER without touching the undo stacks; return the batch.
A text whose lines are still being counted is edited at once, as BUFFER-EDIT
does."
  (multiple-value-bind (version batch) (tuatara.text:version-commit (buffer-version buffer) edits)
    (setf (buffer-version buffer) version)
    (dolist (f (buffer-change-functions buffer)) (funcall f buffer batch))
    batch))

(defun commit-swap-without-record (buffer rope forward backward)
  "Swap BUFFER's whole text for ROPE without touching the undo stacks; return the batch."
  (multiple-value-bind (version batch) (tuatara.text:version-swap (buffer-version buffer) rope forward backward)
    (setf (buffer-version buffer) version)
    (dolist (f (buffer-change-functions buffer)) (funcall f buffer batch))
    batch))

(defun buffer-swap-text (buffer rope forward backward)
  "Make ROPE the whole text of BUFFER as one undoable step, for a change too
large to make edit by edit (a replacement across a file of many gigabytes).
FORWARD maps a byte position and a bias from the old text to ROPE, BACKWARD
from ROPE back; point, the mark and every other anchor follow through them,
and undo swaps the old text back. Signals an error when the buffer is
read-only, and in the hex view when ROPE is not the size of the text it
takes the place of (CHECK-HEX-VIEW-SIZE). *BEFORE-EDIT-FUNCTIONS* see the
whole text changed first, and may refuse it. Returns the batch."
  (let ((batch (swap-whole-text buffer rope forward backward)))
    (record-batch buffer batch)
    batch))

(defun swap-whole-text (buffer rope forward backward)
  "Make ROPE the whole text of BUFFER, as BUFFER-SWAP-TEXT does, without
putting the change on the undo stack; return its batch."
  (when (buffer-local buffer :read-only)
    (error "Buffer ~a is read-only." (buffer-name buffer)))
  (check-hex-view-size buffer (- (tuatara.text:rope-bytes rope) (buffer-size buffer)))
  (run-before-edit buffer :all)
  (let ((batch (commit-swap-without-record buffer rope forward backward)))
    (note-new-content buffer batch)
    batch))

(defun take-back-batch (buffer batch &optional check)
  "Take back BATCH, the newest change made to BUFFER, without touching the
undo stacks: the whole text it swapped swapped back, or its edits inverted;
with CHECK, *BEFORE-EDIT-FUNCTIONS* see that change first, and may refuse it.
Returns where the first edit taken back starts, or NIL."
  (let ((swap (tuatara.text:batch-swap batch)))
    (if swap
        (progn (when check (run-before-edit buffer :all))
               (commit-swap-without-record buffer (tuatara.text:swap-before swap)
                                           (tuatara.text:swap-backward swap) (tuatara.text:swap-forward swap))
               nil)
        (let ((inverse (tuatara.text:inverse-edits (tuatara.text:batch-edits batch) (tuatara.text:batch-positions batch))))
          (when check (run-before-edit buffer inverse))
          (commit-without-record buffer inverse)
          (and (plusp (length inverse)) (tuatara.text:edit-start (aref inverse 0)))))))

(defun make-batch-again (buffer batch &optional check)
  "Make BATCH, a change taken back from BUFFER, again, without touching the
undo stacks; with CHECK, *BEFORE-EDIT-FUNCTIONS* see it first, and may refuse
it. Returns the new batch."
  (let ((swap (tuatara.text:batch-swap batch)))
    (if swap
        (progn (when check (run-before-edit buffer :all))
               (commit-swap-without-record buffer (tuatara.text:swap-after swap)
                                           (tuatara.text:swap-forward swap) (tuatara.text:swap-backward swap)))
        (let ((edits (map 'simple-vector
                          (lambda (e) (tuatara.text:make-edit (tuatara.text:edit-start e) (tuatara.text:edit-end e)
                                                              (tuatara.text:edit-insert e)))
                          (tuatara.text:batch-edits batch))))
          (when check (run-before-edit buffer edits))
          (commit-without-record buffer edits)))))

(defun changed-outside-p (buffer before start end)
  "True when the changes BUFFER's text went through since its version BEFORE
reached outside the stretch from START to END of the text of BEFORE, the
stretch followed through each change. A swap of the whole text ends the
looking."
  (loop with version = before
        for next = (tuatara.text:version-next version)
        while next
        do (let ((batch (tuatara.text:version-batch next)))
             (when (tuatara.text:batch-swap batch) (return nil))
             (when (some (lambda (edit) (or (< (tuatara.text:edit-start edit) start) (> (tuatara.text:edit-end edit) end)))
                         (tuatara.text:batch-edits batch))
               (return t))
             (setf start (tuatara.text:map-position start batch :before)
                   end (tuatara.text:map-position end batch :after)
                   version next))
        until (eq version (buffer-version buffer))))

(defun buffer-undo (buffer)
  "Undo the newest undo group of BUFFER. Returns the group undone (a list of
batches), or NIL when there is nothing to undo, and as a second value true
when, BUFFER being narrowed, what was taken back reached outside the part
shown: undo reaches every change, wherever it is. Point moves to the start of the
undone change, or, when the change taken back last swapped the whole text (a
job's change), stays where that moves it. Signals an error when the buffer is
read-only.
*BEFORE-EDIT-FUNCTIONS* see each change taken back first; when one refuses,
the text is left as it was and the group stays to undo."
  (when (buffer-local buffer :read-only)
    (error "Buffer ~a is read-only." (buffer-name buffer)))
  (let ((part (and (buffer-narrowing buffer) (multiple-value-list (buffer-bounds buffer))))
        (before (buffer-version buffer))
        (group (pop (buffer-undo-stack buffer))))
    (when group
      (let ((first-start nil) (undone '()) (done nil))
        ;; undo the group's batches newest first
        (unwind-protect
             (progn
               (dolist (batch group)
                 (setf first-start (take-back-batch buffer batch t))
                 (push batch undone))
               (setf done t))
          (unless done
            (dolist (batch undone) (make-batch-again buffer batch))
            (push group (buffer-undo-stack buffer))))
        (push group (buffer-redo-stack buffer))
        (let ((held (group-held-octets buffer group))) (add-held buffer (- held) held))
        ;; the text is what it was before the group's first batch
        (let ((contents (gethash (car (last group)) (buffer-batch-contents buffer))))
          (setf (buffer-content-number buffer)
                (if contents (car contents) (tuatara.text:version-number (buffer-version buffer)))))
        (when first-start (setf (buffer-point buffer) first-start))
        (keep-within-narrowing buffer)
        (values group (and part (changed-outside-p buffer before (first part) (second part))))))))

(defun merge-undo-groups (buffer since)
  "Make the undo groups BUFFER gained since its undo stack was SINCE one group,
so that one undo takes them all back. Nothing changes when SINCE is no longer
under the stack (something was undone past it). A place in the history among
them (MAKE-UNDO-PLACE) goes on top of the one group."
  (let ((stack (buffer-undo-stack buffer)))
    (when (and (not (eq stack since)) (tailp since stack))
      (let ((groups (ldiff stack since)))
        (when (rest groups)
          (let ((merged (cons (loop for group in groups append group) since)))
            (dolist (place (buffer-local buffer 'undo-places))
              (when (and (tailp (car place) stack) (not (tailp (car place) since)))
                (setf (car place) merged)))
            (setf (buffer-undo-stack buffer) merged)))))))

(defun make-undo-place (buffer)
  "A place in BUFFER's undo history: where it stands now, for work given now
that begins later to stand there (BUFFER-TEXT-AT). A list of one, whose car is
the undo stack there, kept where it is as the history is laid out anew under
it (BUFFER-CHANGE-WHERE-BEGUN, MERGE-UNDO-GROUPS). Let go of it with
FORGET-UNDO-PLACE."
  (let ((place (list (buffer-undo-stack buffer))))
    (push place (buffer-local buffer 'undo-places))
    place))

(defun forget-undo-place (buffer place)
  "Stop keeping PLACE, made by MAKE-UNDO-PLACE, in BUFFER's undo history."
  (setf (buffer-local buffer 'undo-places) (remove place (buffer-local buffer 'undo-places))))

(defun span-after-batch (span batch)
  "SPAN, a stretch (FROM . TO) of a text, where it stands after BATCH, a change
of that text, as a new cons; NIL when BATCH swapped the whole text or one of
its edits touches SPAN (TUATARA.RUNTIME:SPAN-TOUCHED-P)."
  (unless (or (tuatara.text:batch-swap batch)
              (some (lambda (e) (tuatara.runtime:span-touched-p span (tuatara.text:edit-start e) (tuatara.text:edit-end e)))
                    (tuatara.text:batch-edits batch)))
    (cons (tuatara.text:map-position (car span) batch :after)
          (tuatara.text:map-position (cdr span) batch :before))))

(defun buffer-text-at (buffer since)
  "BUFFER's text as it stood when its undo stack was SINCE, made without
touching the buffer: the changes made after it taken back from the text now.
Second value, those changes, the batches as made, the oldest first; third, a
function of (POSITION BIAS) taking a position in the text now back to that
text. NIL when the history no longer passes through SINCE, or one of those
changes swapped the whole text."
  (let ((stack (buffer-undo-stack buffer)))
    (when (tailp since stack)
      (let ((made (loop for group in (ldiff stack since) append group)))
        (unless (some #'tuatara.text:batch-swap made)
          (let ((text (buffer-text buffer)) (back '()))
            ;; taken back the newest first, each batch's inverse on the text it made
            (dolist (batch made)
              (multiple-value-bind (before batch)
                  (tuatara.text:edits-batch text (tuatara.text:inverse-edits (tuatara.text:batch-edits batch)
                                                                             (tuatara.text:batch-positions batch)))
                (setf text before)
                (push batch back)))
            (setf back (nreverse back))
            (values text (reverse made)
                    (lambda (position bias)
                      (dolist (batch back position)
                        (setf position (tuatara.text:map-position position batch bias)))))))))))

(defun undo-back-to (buffer since)
  "Take back every undo group BUFFER gained since its undo stack was SINCE, and
forget them: undone, with nothing left to redo."
  (when (tailp since (buffer-undo-stack buffer))
    (loop until (eq (buffer-undo-stack buffer) since)
          do (buffer-undo buffer)
             (add-held buffer 0 (- (group-held-octets buffer (pop (buffer-redo-stack buffer))))))))

(defstruct (carry-piece (:constructor make-carry-piece (start linear move shift)) (:copier nil))
  "A stretch of a text edited while work in the background ran, from START up
to the next piece's START, and where it stands in the work's result with the
edits made there again (CARRIED-POSITION): with LINEAR, text the edits put,
START + MOVE on, at that offset; else the work's map at the position less
MOVE, plus SHIFT."
  (start 0 :type fixnum)
  (linear nil)
  (move 0 :type fixnum)
  (shift 0 :type fixnum))

(setf (documentation 'carry-piece-p 'function) "True when OBJECT is a CARRY-PIECE.")

(defun carry-pieces ()
  "The pieces (CARRY-PIECE) of a text no edit has touched yet: one, from 0 on,
where the work's map takes it."
  (make-array 1 :adjustable t :fill-pointer 1 :initial-element (make-carry-piece 0 nil 0 0)))

(defun carry-piece-at (pieces position)
  "The index in PIECES of the piece POSITION stands in: the last one starting
at or before it. O(log pieces)."
  (let ((lo 0) (hi (fill-pointer pieces)))
    (loop while (> (- hi lo) 1)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (<= (carry-piece-start (aref pieces mid)) position) (setf lo mid) (setf hi mid))))
    lo))

(defun carried-position (pieces map position bias)
  "Where POSITION in the text PIECES cover stands in the work's result, MAP
taking the text no edit put there, with BIAS, as it takes the text before the
edits."
  (let ((piece (aref pieces (carry-piece-at pieces position))))
    (if (carry-piece-linear piece)
        (+ position (carry-piece-move piece))
        (+ (funcall map (- position (carry-piece-move piece)) bias) (carry-piece-shift piece)))))

(defun carry-pieces-past (pieces map edits)
  "Carry PIECES, a text's pieces as CARRIED-POSITION reads them with MAP, past
EDITS (an applied batch's, sorted, in the text before them), made on both:
the pieces of the text after EDITS, an edit's inserted text going to where it
is made again, the rest moved by what the edits before it took and put, in
either text. Only the pieces the edits touch are made again; those after are
moved where they stand, so carrying a batch past costs its edits and the
pieces after them, not the batches carried before."
  (let* ((theres (map 'simple-vector (lambda (e) (carried-position pieces map (tuatara.text:edit-start e) :before)) edits))
         (ends (map 'simple-vector (lambda (e) (carried-position pieces map (tuatara.text:edit-end e) :after)) edits))
         (count (fill-pointer pieces))
         (first (carry-piece-at pieces (tuatara.text:edit-start (aref edits 0))))
         (last (carry-piece-at pieces (tuatara.text:edit-end (aref edits (1- (length edits))))))
         (made '())
         ;; DELTA, how far the edits passed moved this text; SHIFT, the text made elsewhere
         (delta 0) (shift 0))
    (labels ((piece-end (i)
               (if (< (1+ i) count) (carry-piece-start (aref pieces (1+ i))) most-positive-fixnum))
             (moved (piece from)
               ;; PIECE from FROM on, after the edits passed so far
               (if (carry-piece-linear piece)
                   (make-carry-piece (+ from delta) t (+ (carry-piece-move piece) (- delta) shift) 0)
                   (make-carry-piece (+ from delta) nil (+ (carry-piece-move piece) delta) (+ (carry-piece-shift piece) shift))))
             (keep (from below)
               ;; the old pieces over FROM up to BELOW, in the text before the edits
               (loop for i from first to last
                     for piece = (aref pieces i)
                     for a = (max from (carry-piece-start piece))
                     when (< a (min below (piece-end i)))
                       do (push (moved piece a) made))))
      (let ((from (carry-piece-start (aref pieces first))))
        (loop for e across edits
              for there across theres
              for end across ends
              for inserted = (length (or (tuatara.text:edit-insert e) #()))
              do (keep from (tuatara.text:edit-start e))
                 (when (plusp inserted)
                   (let ((start (+ (tuatara.text:edit-start e) delta)))
                     (push (make-carry-piece start t (- (+ there shift) start) 0) made)))
                 (incf delta (- inserted (- (tuatara.text:edit-end e) (tuatara.text:edit-start e))))
                 (incf shift (- inserted (- end there)))
                 (setf from (tuatara.text:edit-end e)))
        (keep from (piece-end last)))
      ;; the pieces after the last one touched: moved where they stand
      (loop for i from (1+ last) below count
            for piece = (aref pieces i)
            do (incf (carry-piece-start piece) delta)
               (if (carry-piece-linear piece)
                   (incf (carry-piece-move piece) (- shift delta))
                   (progn (incf (carry-piece-move piece) delta)
                          (incf (carry-piece-shift piece) shift))))
      ;; the pieces FIRST to LAST give way to those made
      (let* ((made (coerce (nreverse made) 'simple-vector))
             (removed (1+ (- last first)))
             (new-count (+ (- count removed) (length made))))
        (when (> new-count (array-dimension pieces 0))
          (adjust-array pieces (max new-count (* 2 (array-dimension pieces 0)))))
        (setf (fill-pointer pieces) (max count new-count))
        (replace pieces pieces :start1 (+ first (length made)) :start2 (1+ last) :end2 count)
        (replace pieces made :start1 first)
        (setf (fill-pointer pieces) new-count)
        pieces))))

(defvar *made-content-number* 0
  "The last content number given to a text an undo step was made to lead to
without the buffer ever holding it (BUFFER-CHANGE-WHERE-BEGUN): counted down
from 0, so that it is never a version's number nor a file's saved content.")

(defun buffer-change-where-begun (buffer since begun rope forward backward final final-forward final-backward &key join)
  "Make FINAL BUFFER's text, as one swap (FINAL-FORWARD and FINAL-BACKWARD map
the text now to it and back), for work in the background begun when BUFFER's
undo stack was SINCE on the text BEGUN, whose result is ROPE, BEGUN with the
work done (FORWARD and BACKWARD map between them); FINAL is ROPE with the
changes made to BUFFER meanwhile made on it too. The undo history then stands
as if the work had been done the moment it began: the work one undo step,
where it began, and each change made meanwhile its own step after it, its
places carried over by FORWARD, so that undo takes back those changes first
and the work after them. Only the history is laid out so; the text is
swapped once, everything that follows it (point, marks, places) moved once,
to where it now belongs. With JOIN, the work's step and the newest one under
SINCE, a change the same work made first (in sight, at once), are one. NIL,
and nothing done, when the undo history no longer passes through SINCE
\(undone past it), a change made meanwhile swapped the whole text, or the
changes carried over do not lead to FINAL; true otherwise."
  (let* ((stack (buffer-undo-stack buffer))
         (later (and (not (eq stack since)) (tailp since stack) (ldiff stack since))))
    (when (and (or (eq stack since) later)
               (notany (lambda (group) (some #'tuatara.text:batch-swap group)) later))
      ;; the changes made meanwhile, carried over onto ROPE, made one after another without the buffer
      (let ((text rope) (pieces (carry-pieces)) (groups '()))
        (dolist (group (reverse later))
          (let ((batches '()))
            (dolist (batch (reverse group))
              (let ((edits (tuatara.text:batch-edits batch)))
                (multiple-value-bind (next made)
                    (tuatara.text:edits-batch
                     text (map 'simple-vector
                               (lambda (e)
                                 (tuatara.text:make-edit (carried-position pieces forward (tuatara.text:edit-start e) :before)
                                                         (carried-position pieces forward (tuatara.text:edit-end e) :after)
                                                         (tuatara.text:edit-insert e)))
                               edits))
                  (setf text next)
                  (when (plusp (length edits)) (carry-pieces-past pieces forward edits))
                  (push made batches))))
            (push batches groups)))
        (when (= (tuatara.text:rope-bytes text) (tuatara.text:rope-bytes final))
          (let* ((table (buffer-batch-contents buffer))
                 (began (let ((oldest (and later (car (last (car (last later)))))))
                          (and oldest (car (gethash oldest table)))))
                 (swapped (swap-whole-text buffer final final-forward final-backward))
                 (now (buffer-content-number buffer))
                 (before (or began (car (gethash swapped table))))
                 (work (tuatara.text:swap-batch begun rope forward backward)))
            (remhash swapped table)
            ;; the content numbers before and after each step, for undo to know the text saved
            (setf (gethash work table) (cons before (if groups (decf *made-content-number*) now)))
            (loop for (batches . more) on (reverse groups)
                  for from = (cdr (gethash work table)) then to
                  for to = (if more (decf *made-content-number*) now)
                  do (dolist (batch batches) (setf (gethash batch table) (cons from to))))
            (setf (buffer-undo-stack buffer)
                  (append groups
                          (if (and join since)
                              (cons (cons work (car since)) (cdr since))
                              (cons (list work) since)))
                  (buffer-redo-stack buffer) '())
            (add-held buffer (- (+ (batch-held-octets buffer work)
                                   (loop for group in groups sum (group-held-octets buffer group)))
                                (loop for group in later sum (group-held-octets buffer group)))
                      0)
            ;; a place in the history at SINCE or above it stays above the same changes, the work under them
            (dolist (place (buffer-local buffer 'undo-places))
              (when (and (tailp (car place) stack) (tailp since (car place)))
                (setf (car place) (nthcdr (- (length groups) (length (ldiff (car place) since)))
                                          (buffer-undo-stack buffer)))))
            (let-go-of-oldest-changes buffer)
            (keep-within-narrowing buffer)
            t))))))

(defun buffer-redo (buffer)
  "Redo the most recently undone group of BUFFER, point after the last change it
makes again, or, when it swaps the whole text (a job's change), where it is,
moved as the text is. Returns the new group, or NIL when there is nothing to
redo. Signals an error when the buffer is read-only.
*BEFORE-EDIT-FUNCTIONS* see each change made again first; when one refuses,
the text is left as it was and the group stays to redo."
  (when (buffer-local buffer :read-only)
    (error "Buffer ~a is read-only." (buffer-name buffer)))
  (let ((group (pop (buffer-redo-stack buffer))))
    (when group
      (let ((new-group '()) (last-position nil) (table (buffer-batch-contents buffer)) (done nil))
        ;; redo the group's batches oldest first; each new batch takes its original's content numbers
        (unwind-protect
             (progn
               (dolist (batch (reverse group))
                 (let ((new (make-batch-again buffer batch t)))
                   (setf (gethash new table) (gethash batch table))
                   (multiple-value-bind (held found) (gethash batch (buffer-swap-held buffer))
                     (when found (setf (gethash new (buffer-swap-held buffer)) held)))
                   (push new new-group)
                   (let ((positions (tuatara.text:batch-positions new)))
                     (when (plusp (length positions))
                       (setf last-position (aref positions (1- (length positions))))))))
               (setf done t))
          (unless done
            (dolist (batch new-group) (take-back-batch buffer batch))
            (push group (buffer-redo-stack buffer))))
        (push new-group (buffer-undo-stack buffer))
        (let ((held (group-held-octets buffer new-group))) (add-held buffer held (- held)))
        ;; the text is what it was after the group's last batch
        (let ((contents (gethash (first group) table)))
          (setf (buffer-content-number buffer)
                (if contents (cdr contents) (tuatara.text:version-number (buffer-version buffer)))))
        ;; a job's change, a swap of the whole text, leaves point where it is, as the text moved it
        (when (and last-position (notany #'tuatara.text:batch-swap group))
          (setf (buffer-point buffer) last-position))
        (keep-within-narrowing buffer)
        new-group))))

;;; Navigation helpers (byte offsets, character aware)

(defun buffer-line-ending (buffer)
  "How BUFFER's lines end: :LF; :CRLF for text whose lines end with a carriage
return and a newline (Windows files), which is then shown and edited as
though each pair were one line end, the octets staying as they are; or :CR
for text whose lines end with a carriage return alone (old Macintosh files).
What a new line is made with (BUFFER-NEWLINE); in any buffer a carriage
return that no newline follows ends a line too (TUATARA.TEXT:LINE-END-OCTET-P).
The :LINE-ENDING local; visiting a file sets it from the file."
  (buffer-local buffer :line-ending :lf))

(defun buffer-crlf-p (buffer)
  "True when BUFFER's lines end with a carriage return and a newline."
  (eq (buffer-line-ending buffer) :crlf))

(defun buffer-newline (buffer)
  "The text that ends a line in BUFFER: a newline, in a :CRLF buffer a
carriage return and a newline, in a :CR buffer a carriage return. The string
is shared: it must not be changed."
  (case (buffer-line-ending buffer)
    (:crlf (load-time-value (coerce '(#\Return #\Newline) 'simple-string) t))
    (:cr (load-time-value (string #\Return) t))
    (t (load-time-value (string #\Newline) t))))

(defun detect-line-ending (rope &key (start 0) (end (tuatara.text:rope-bytes rope)))
  "How the lines of ROPE's text from START below END end, judged by its first
line end in its first 64 KB: :CRLF when it is a newline a carriage return
comes before, :CR when it is a carriage return alone, else :LF."
  (let ((line-end (tuatara.text:rope-find-line-end rope :start start :end (min end (+ start 65536)))))
    (cond ((null line-end) :lf)
          ((= 13 (tuatara.text:rope-octet rope line-end)) :cr)
          ((/= line-end (tuatara.text:rope-line-text-end rope start line-end t)) :crlf)
          (t :lf))))

(defun buffer-forward-char (buffer byte &optional (n 1))
  "Byte offset N characters after BYTE in BUFFER (before, for negative N),
clamped to the text, stopping at once at its end. In a :CRLF buffer a
carriage return and the newline after it are one step."
  (let ((rope (buffer-text buffer)) (crlf (buffer-crlf-p buffer)))
    ;; at an end of the text it stops at once, whatever is left of N
    (if (>= n 0)
        (loop repeat n
              for next = (if (and crlf (tuatara.text:rope-crlf-p rope byte))
                             (+ byte 2)
                             (tuatara.text:rope-next-boundary rope byte))
              until (= next byte)
              do (setf byte next))
        (loop repeat (- n)
              for next = (if (and crlf (tuatara.text:rope-crlf-p rope (- byte 2)))
                             (- byte 2)
                             (tuatara.text:rope-previous-boundary rope byte))
              until (= next byte)
              do (setf byte next)))
    byte))

(defun buffer-line-start (buffer byte)
  "Byte offset of the start of the line containing BYTE."
  (tuatara.text:rope-line-start (buffer-text buffer) byte))

(defun buffer-line-end (buffer byte)
  "Byte offset of the end of the line containing BYTE: the octet that ends it
\(a newline, in a :CRLF buffer the carriage return before one, or a carriage
return alone), or the text end."
  (let* ((rope (buffer-text buffer))
         (end (tuatara.text:rope-line-end rope byte)))
    (if (and (buffer-crlf-p buffer) (tuatara.text:rope-crlf-p rope (1- end)))
        (1- end)
        end)))

(defun buffer-line-text (buffer start)
  "The text of BUFFER's line from START, a line start, to its end
\(BUFFER-LINE-END), its line end left out."
  (buffer-string buffer :start start :end (buffer-line-end buffer start)))

(defvar *index-wait-function* nil
  "A function of a buffer, called when the line counts of its text are needed
while they are still being made in the background: it returns once the text is
indexed (the editor waits, showing how far the counting has got), or signals
to give up what needed them. NIL counts them there and then.")

(defun buffer-indexed-p (buffer)
  "True when the line and character counts of BUFFER's text are known: a large
file's are made in the background after it opens."
  (tuatara.text:rope-indexed-p (buffer-text buffer)))

(defun buffer-adopt-text (buffer rope)
  "Make ROPE, BUFFER's own text with its counts known, the text BUFFER holds: a
version of its own that changes nothing, so point, the mark and undo stay as
they are and the buffer is no more modified than it was. Returns the batch."
  (multiple-value-bind (version batch) (tuatara.text:version-adopt (buffer-version buffer) rope)
    (setf (buffer-version buffer) version)
    (dolist (f (buffer-change-functions buffer)) (funcall f buffer batch))
    batch))

(defun ensure-indexed (buffer)
  "Make sure the line and character counts of BUFFER's text are known: wait for
the background counting (*INDEX-WAIT-FUNCTION*), or count them now."
  (unless (buffer-indexed-p buffer)
    (if *index-wait-function*
        (funcall *index-wait-function* buffer)
        (buffer-adopt-text buffer (tuatara.text:index-rope (buffer-text buffer))))
    (unless (buffer-indexed-p buffer)
      (error "The lines of ~a are not counted yet." (buffer-name buffer)))))

(defvar *counted-line-function* nil
  "A function of a buffer and a byte offset returning the line, from 0, there
while the buffer's lines are still being counted, when the count has passed
it, else NIL: the files layer's TUATARA.IO:COUNTED-LINE-NUMBER.")

(defun buffer-known-line-number (buffer byte)
  "The line number, from 0, of BYTE in BUFFER when it is known without waiting:
always once the lines are counted; while they are, where the count has passed
(*COUNTED-LINE-FUNCTION*); else NIL."
  (if (buffer-indexed-p buffer)
      (buffer-line-number buffer byte)
      (and *counted-line-function* (funcall *counted-line-function* buffer byte))))

(defvar *line-scan-limit* (* 64 1024 1024)
  "Octets from its start that a text whose lines are still being counted is
read through for a line (BUFFER-SCAN-LINE), rather than waiting for the count.")

(defun buffer-scan-line (buffer line &key (limit *line-scan-limit*))
  "The start of line LINE (from 0) of BUFFER, the last line when there are
fewer, found by reading the text from its start and counting its lines, the
line numbers not needed; NIL when LINE starts more than LIMIT octets in. For a
text whose lines are still being counted: a line near the start is found at
once. The text is read a block of a megabyte at a time."
  (let* ((rope (buffer-text buffer))
         (size (tuatara.text:rope-bytes rope))
         (last 0) (n 0))
    (block found
      (tuatara.text:map-line-slices (lambda (start &rest others)
                                      (declare (ignore others))
                                      (when (= n line) (return-from found start))
                                      (setf last start)
                                      (incf n))
                                    rope 0 (1+ (min size limit)) 0 0
                                    :crlf (buffer-crlf-p buffer))
      ;; fewer lines than LINE: the last one, when the whole text was read
      (and (<= size limit) last))))

(defun buffer-line-number (buffer byte)
  "0-based line number of byte offset BYTE. While the text's lines are still
being counted in the background, this waits for them (ENSURE-INDEXED)."
  (ensure-indexed buffer)
  (tuatara.text:byte->line (buffer-text buffer) byte))

(defun buffer-line-count (buffer)
  "Number of lines in BUFFER (line ends plus one), waiting as BUFFER-LINE-NUMBER does."
  (ensure-indexed buffer)
  (1+ (tuatara.text:rope-lines (buffer-text buffer))))

(defun buffer-goto-line (buffer line)
  "Byte offset where 0-based LINE starts, clamped to the last line; waits as
BUFFER-LINE-NUMBER does."
  (ensure-indexed buffer)
  (let ((rope (buffer-text buffer)))
    (tuatara.text:line->byte rope (max 0 (min line (tuatara.text:rope-lines rope))))))

(defun buffer-column (buffer byte &key most)
  "Number of characters between the start of BYTE's line and BYTE: from the
counts, or, while they are being made, by reading the line. With MOST, NIL
when the counts are not made and the line starts more than MOST octets
before BYTE, so that no more than that is read."
  (let ((rope (buffer-text buffer)))
    (cond ((tuatara.text:rope-indexed-p rope)
           (- (tuatara.text:byte->char rope byte) (tuatara.text:byte->char rope (buffer-line-start buffer byte))))
          ((null most) (tuatara.text:rope-count-chars rope (buffer-line-start buffer byte) byte))
          (t (let* ((near (max 0 (- byte most)))
                    (newline (tuatara.text:rope-find-line-end-backward rope :start near :end byte)))
               (cond (newline (tuatara.text:rope-count-chars rope (1+ newline) byte))
                     ((zerop near) (tuatara.text:rope-count-chars rope 0 byte))))))))

;;; Buffer-local variables

(defun buffer-local (buffer name &optional default)
  "The value of buffer-local variable NAME in BUFFER, or DEFAULT when unset.
A package reads Tuatara's own only by keywords meant for it: :READ-ONLY (true
when the buffer may not be edited) and :DIRECTORY (the folder it stands in);
its own it keeps under symbols of its own package."
  (multiple-value-bind (value found) (gethash name (buffer-locals buffer))
    (if found value default)))

(defun (setf buffer-local) (value buffer name)
  "Set buffer-local variable NAME in BUFFER to VALUE."
  (setf (gethash name (buffer-locals buffer)) value))

(defmacro with-read-only-lifted ((buffer &key (after nil after-given)) &body body)
  "Run BODY with BUFFER writable for the change it makes, then make BUFFER read
only again as it was before; or, with AFTER, as AFTER (evaluated once BODY is
done or left) says. BUFFER is evaluated once. The change may reach outside
the part of BUFFER shown when it is narrowed, as the editor's own writing
does."
  (let ((place (gensym "BUFFER")) (before (gensym "BEFORE")))
    `(let* ((,place ,buffer) (,before (buffer-local ,place :read-only)) (*narrowing-in-force* nil))
       (declare (ignorable ,before))
       (setf (buffer-local ,place :read-only) nil)
       (unwind-protect (progn ,@body)
         (setf (buffer-local ,place :read-only) ,(if after-given after before))))))

;;; Files

(defun unique-buffer-name (base)
  "BASE, or BASE<2>, BASE<3>... whichever is not in use."
  (if (find-buffer base)
      (loop for i from 2
            for name = (format nil "~a<~a>" base i)
            unless (find-buffer name) return name)
      base))

