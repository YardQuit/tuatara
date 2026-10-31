;;;; lisp/editing/conflicts.lisp --- Conflicts a merge left in a file, shown and put right.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; When git merges, rebases or applies a stash and two changes meet, it
;;;; writes both into the file between marker lines: <<<<<<< and the first
;;;; side's name, the first side, ||||||| and the base where it writes one,
;;;; =======, the second side, >>>>>>> and its name. A file shown is looked
;;;; through for them in the background, once (LOOK-AT-SHOWN-FILES); where
;;;; they make whole conflicts, each part is painted in a face of its own and
;;;; the keys under C-c ^ go from one to the next and keep a side, as Emacs's
;;;; smerge-mode does. The conflicts are kept as anchors, which follow the
;;;; edits: each edit has only the conflicts it touches checked again and only
;;;; its lines looked through for markers; the whole text is looked through
;;;; again only where an edit left a marker no conflict holds, once the edits
;;;; pause.

(in-package #:tuatara.defaults)

(defvar *conflict-markers-max-bytes* (* 256 1024 1024)
  "The most octets of a file that are looked through for conflicts by
themselves when it is shown; past it the file is looked through only when
asked, with \\[conflict-markers-mode].")

(defparameter *conflict-bindings*
  '(("C-c ^ n" conflict-next) ("C-c ^ p" conflict-previous)
    ("C-c ^ u" conflict-keep-first-side) ("C-c ^ l" conflict-keep-second-side)
    ("C-c ^ a" conflict-keep-both-sides) ("C-c ^ b" conflict-keep-base)
    ("C-c ^ =" conflict-show-sides-diff))
  "The keys of a buffer holding conflicts, under \\`C-c ^', each (KEYS COMMAND).")

(defun conflict-keys (&optional parent)
  "A keymap of *CONFLICT-BINDINGS*, labelled, whose keys not bound fall through to PARENT."
  (let ((keymap (make-keymap :name "conflicts" :parent parent)))
    (define-keys keymap *conflict-bindings*)
    (label-keys keymap '(("C-c ^" "conflicts")))))

(defvar *conflict-keymap* (conflict-keys)
  "The keys of a buffer holding conflicts (\\[conflict-markers-mode]), under
\\`C-c ^', before its mode's.")

(defconstant +conflict-marker-length+ 7
  "How many of its character a marker line begins with: <<<<<<<, |||||||, ======= or >>>>>>>.")

(defparameter *conflict-pause* 1/2
  "Seconds without an edit after which a buffer whose edits left a marker no
conflict holds is looked through again whole.")

(defstruct (conflict (:constructor make-conflict (start base separator end)) (:copier nil))
  "A conflict in a buffer: anchors at the starts of its marker lines, START
\(<<<<<<<), BASE (|||||||, or NIL where there is none), SEPARATOR (=======)
and END (>>>>>>>)."
  start base separator end)

(setf (documentation 'conflict-p 'function) "True when OBJECT is a CONFLICT."
      (documentation 'conflict-start 'function) "The anchor at the start of a conflict's <<<<<<< line."
      (documentation 'conflict-base 'function) "The anchor at the start of a conflict's ||||||| line, or NIL."
      (documentation 'conflict-separator 'function) "The anchor at the start of a conflict's ======= line."
      (documentation 'conflict-end 'function) "The anchor at the start of a conflict's >>>>>>> line.")

;;; Finding them

(defun conflict-map-markers (rope function &key (start 0) (end (tuatara.text:rope-bytes rope)) cancel)
  "Call FUNCTION with the character of each marker line of ROPE (the
character <, |, = or >) from octet START, a line's start, below END, a
line's start or the end of ROPE, and the octet it starts at, in order: a
line of seven of one of them, ended there or, but for =======, followed by
a space and a name. Read 64 KB at a time into a block on the stack, so that
none of it is garbage; stopped, returning NIL, once CANCEL's car turns true,
else T.
Complexity: O(END - START), little more than finding the newlines."
  (let ((size (tuatara.text:rope-bytes rope))
        (block (make-array 65536 :element-type '(unsigned-byte 8)))
        ;; octets of the marker so far on the line, -1 for a line that is none, 8 after a carriage
        ;; return after a marker, -2 after one on a line that is none
        (column 0) (kind 0) (line start))
    (declare (type fixnum size column kind line start end) (type tuatara.text:octets block)
             (dynamic-extent block) (optimize speed))
    (flet ((found () (funcall function (code-char kind) line)))
      (loop for low of-type fixnum from start below end by 65536
            do (when (and cancel (car cancel)) (return-from conflict-map-markers nil))
               (let ((n (tuatara.text:rope-copy-into rope low (min end (+ low 65536)) block))
                     (i 0))
                 (declare (type fixnum n i))
                 (loop while (< i n)
                       do (let ((octet (aref block i)))
                            (cond ((= octet 10)
                                   (when (>= column +conflict-marker-length+) (found))
                                   (setf column 0 line (+ low i 1)))
                                  ((or (= column -2) (= column 8))
                                   ;; the carriage return before, no newline after it, ended the line:
                                   ;; this octet is looked at again as the next line's first
                                   (when (= column 8) (found))
                                   (setf column 0 line (+ low i))
                                   (decf i))
                                  ((minusp column)
                                   ;; no marker on this line: on to its end
                                   (if (= octet 13)
                                       (setf column -2)
                                       (let ((newline (position-if (lambda (b) (or (= b 10) (= b 13))) block :start i :end n)))
                                         (setf i (1- (or newline n))))))
                                  ((zerop column)
                                   (if (member octet '(60 61 62 124))
                                       (setf kind octet column 1)
                                       (setf column -1)))
                                  ((< column +conflict-marker-length+)
                                   (setf column (if (= octet kind) (1+ column) -1)))
                                  ((and (= column +conflict-marker-length+) (= octet 13)) (setf column 8))
                                  ((and (= column +conflict-marker-length+) (= octet 32) (/= kind 61))
                                   (found)
                                   (setf column -1))
                                  (t (setf column -1))))
                          (incf i))))
      (when (and (= end size) (>= column +conflict-marker-length+)) (found))
      t)))

(defun conflict-find-all (rope &key (start 0) (end (tuatara.text:rope-bytes rope)) cancel)
  "The whole conflicts in ROPE from octet START below END, both lines'
starts, in order, each (START BASE SEPARATOR END), the octets their marker
lines start at, BASE NIL where there is none: a <<<<<<< line, then a
||||||| line or none, then =======, then >>>>>>>. A marker out of that order
belongs to no conflict: a ======= line alone, a >>>>>>> line with no =======
before it; a <<<<<<< line begins again. NIL too when stopped by CANCEL
\(CONFLICT-MAP-MARKERS)."
  (let ((found '()) (first nil) (base nil) (separator nil))
    (and (conflict-map-markers
          rope
          (lambda (kind at)
            (case kind
              (#\< (setf first at base nil separator nil))
              (#\| (when (and first (not base) (not separator)) (setf base at)))
              (#\= (when (and first (not separator)) (setf separator at)))
              (#\> (when separator
                     (push (list first base separator at) found)
                     (setf first nil base nil separator nil)))))
          :start start :end end :cancel cancel)
         (nreverse found))))

(defun conflict-marker-line-p (rope at kind)
  "True when the line of ROPE starting at octet AT is a marker line of KIND,
the character it is made of (CONFLICT-MAP-MARKERS)."
  (let ((size (tuatara.text:rope-bytes rope))
        (octet (char-code kind)))
    (flet ((octet (i) (and (< i size) (tuatara.text:rope-octet rope i))))
      (and (or (zerop at) (= (1- at) (tuatara.text:rope-line-end rope (1- at))))
           (loop for i from at below (+ at +conflict-marker-length+) always (eql octet (octet i)))
           (let ((after (octet (+ at +conflict-marker-length+))))
             (or (null after) (= after 10) (= after 13)
                 (and (= after 32) (char/= kind #\=))))))))

(defun conflict-places (buffer conflict)
  "Where CONFLICT of BUFFER is now, as five values, the octets its marker
lines start at (START, BASE or NIL, SEPARATOR, END) and where the line after
its last starts; NIL when an edit has broken it, a marker line changed."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (start (tuatara.text:anchor-position (conflict-start conflict) version))
         (base (and (conflict-base conflict) (tuatara.text:anchor-position (conflict-base conflict) version)))
         (separator (tuatara.text:anchor-position (conflict-separator conflict) version))
         (end (tuatara.text:anchor-position (conflict-end conflict) version)))
    (when (and (< start (or base separator)) (< separator end)
               (conflict-marker-line-p rope start #\<) (or (null base) (conflict-marker-line-p rope base #\|))
               (conflict-marker-line-p rope separator #\=) (conflict-marker-line-p rope end #\>))
      (values start base separator end (next-line-start buffer end)))))

(defun conflict-list (buffer)
  "BUFFER's conflicts that stand, in order, a vector of CONFLICTs: those an
edit breaks are taken out as it is made (CONFLICT-FOLLOW-EDIT)."
  (or (tuatara.buffer:buffer-local buffer 'conflict-list) #()))

(defun conflict-markers-on-p (&optional (buffer (current-buffer)))
  "True when BUFFER (the current buffer) holds conflicts shown and put right
with the keys under \\`C-c ^': some stand in it, and it is not switched off
there (\\[conflict-markers-mode])."
  (and (plusp (length (conflict-list buffer)))
       (not (tuatara.buffer:buffer-local buffer 'conflict-off))))

(defun conflict-start-at (buffer conflict)
  "Where CONFLICT of BUFFER's <<<<<<< line starts now."
  (tuatara.text:anchor-position (conflict-start conflict) (tuatara.buffer:buffer-version buffer)))

(defun conflict-index-from (buffer from)
  "The index of the first of BUFFER's conflicts whose >>>>>>> line starts at
FROM or after. Complexity: O(log conflicts)."
  (let ((conflicts (conflict-list buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (tuatara.runtime:bisect (i 0 (length conflicts))
      (< (tuatara.text:anchor-position (conflict-end (svref conflicts i)) version) from))))

(defun conflict-index-range (buffer start end)
  "The indices of BUFFER's conflicts that have lines from octet START below END, two values, from and below."
  (let* ((conflicts (conflict-list buffer))
         (from (conflict-index-from buffer start)))
    (values from (loop for i from from below (length conflicts)
                       while (< (conflict-start-at buffer (svref conflicts i)) end)
                       finally (return i)))))

(defun conflict-put (buffer version found)
  "Take FOUND, conflicts as CONFLICT-FIND-ALL gives them for BUFFER's text at
VERSION, among those BUFFER holds: each followed through the edits made
since and kept when it still stands and none held starts where it does (one
put right meanwhile does not), the conflicts kept in order. Complexity:
O(found × log conflicts), and O(conflicts) more when some are added."
  (flet ((anchor (at) (and at (tuatara.text:make-anchor version at :bias :after))))
    (let ((added (loop for places in found
                       for conflict = (apply #'make-conflict (mapcar #'anchor places))
                       for start = (conflict-places buffer conflict)
                       when (and start
                                 (let* ((held (conflict-list buffer)) (i (conflict-index-from buffer start)))
                                   (not (and (< i (length held)) (= start (conflict-start-at buffer (svref held i)))))))
                         collect conflict)))
      (when added
        (setf (tuatara.buffer:buffer-local buffer 'conflict-list)
              (sort (concatenate 'simple-vector (conflict-list buffer) added) #'<
                    :key (lambda (c) (conflict-start-at buffer c))))))))

;;; Following the edits

(defvar *conflict-rescans* '()
  "The buffers whose edits left a marker line no conflict holds, to be
looked through again whole once their edits pause (*CONFLICT-PAUSE*).")

(defun conflict-touched-spans (buffer batch)
  "The whole lines of BUFFER's text, as it is after BATCH, that BATCH's edits
touched: a list of (START . END), octets from a line's start to the next
line's start (or the end of the text), in order, spans that meet joined.
Complexity: O(edits × the lines' length)."
  (let ((rope (tuatara.buffer:buffer-text buffer)) (spans '()))
    (loop for e across (tuatara.text:batch-edits batch)
          for p of-type fixnum across (tuatara.text:batch-positions batch)
          do (let ((start (tuatara.text:rope-line-start rope (max 0 (- p (tuatara.text:edit-insert-length e)))))
                   (end (next-line-start buffer p)))
               (if (and spans (<= start (cdar spans)))
                   (setf (cdar spans) (max end (cdar spans)))
                   (push (cons start end) spans))))
    (nreverse spans)))

(defun conflict-follow-edit (buffer batch)
  "Change function of a buffer holding conflicts, or that held some: after
BATCH, take out the conflicts it touched that no longer stand, take in the
whole conflicts in the lines it touched (undo bringing one back), and when a
marker line is left there that no conflict holds, look the buffer through
again whole once its edits pause. A batch taking in a whole text at once
(the file read again, or such a change undone) takes out the conflicts that
no longer stand and has the buffer looked through again whole once its
edits pause, up to *CONFLICT-MARKERS-MAX-BYTES*. Complexity: O(log conflicts + the lines
touched + the conflicts among them); for a whole text, O(conflicts)."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (version (tuatara.buffer:buffer-version buffer))
        (left-over nil))
    (if (tuatara.text:batch-swap batch)
        ;; a whole text: those still standing kept, the rest found by a look through it all
        (progn
          (setf (tuatara.buffer:buffer-local buffer 'conflict-list)
                (remove-if-not (lambda (conflict) (conflict-places buffer conflict))
                               (conflict-list buffer)))
          (setf left-over (<= (tuatara.buffer:buffer-size buffer) *conflict-markers-max-bytes*)))
        (dolist (span (conflict-touched-spans buffer batch))
          (destructuring-bind (start . end) span
            (multiple-value-bind (from below) (conflict-index-range buffer start end)
              (let* ((conflicts (conflict-list buffer))
                     (broken (loop for i from from below below
                                   unless (conflict-places buffer (svref conflicts i)) collect (svref conflicts i))))
                (when broken
                  (setf (tuatara.buffer:buffer-local buffer 'conflict-list)
                        (remove-if (lambda (c) (member c broken)) conflicts)))))
            (let ((markers '()))
              (conflict-map-markers rope (lambda (kind at) (declare (ignore kind)) (push at markers)) :start start :end end)
              (when markers
                (conflict-put buffer version (conflict-find-all rope :start start :end end))
                (multiple-value-bind (from below) (conflict-index-range buffer start end)
                  (let ((held (loop for i from from below below
                                    nconc (multiple-value-bind (a b c d) (conflict-places buffer (svref (conflict-list buffer) i))
                                            (remove nil (list a b c d))))))
                    (when (set-difference markers held)
                      (setf left-over t)))))))))
    (when left-over (pushnew buffer *conflict-rescans*))
    (when (member buffer *conflict-rescans*)
      (setf (tuatara.buffer:buffer-local buffer 'conflict-edited-at) (get-internal-real-time)))
    (conflict-follow-keymap buffer)))

(defun conflict-watch (buffer)
  "Follow BUFFER's edits for its conflicts (CONFLICT-FOLLOW-EDIT)."
  (pushnew 'conflict-follow-edit (tuatara.buffer:buffer-change-functions buffer)))

(defun conflict-watched-p (buffer)
  "True while BUFFER's edits are followed for its conflicts."
  (and (member 'conflict-follow-edit (tuatara.buffer:buffer-change-functions buffer)) t))

(defun conflict-stop-watching (buffer)
  "Follow BUFFER's edits for conflicts no more."
  (setf (tuatara.buffer:buffer-change-functions buffer)
        (remove 'conflict-follow-edit (tuatara.buffer:buffer-change-functions buffer))))

(defun conflict-pause-tick (editor)
  "Tick function: look through again whole each buffer of *CONFLICT-RESCANS*
not edited for *CONFLICT-PAUSE* seconds and not being looked through
already; while one is edited, look again when its time is up."
  (declare (ignore editor))
  (when *conflict-rescans*
    (let ((now (get-internal-real-time)) (soonest nil))
      (setf *conflict-rescans*
            (remove-if (lambda (buffer)
                         (let ((quiet (/ (- now (tuatara.buffer:buffer-local buffer 'conflict-edited-at 0))
                                         internal-time-units-per-second)))
                           (cond ((not (tuatara.buffer:live-buffer-p buffer)) t)
                                 ((or (< quiet *conflict-pause*) (tuatara.buffer:buffer-local buffer 'conflict-job))
                                  (setf soonest (min (or soonest *conflict-pause*) (max 0.05 (- *conflict-pause* quiet))))
                                  nil)
                                 (t (conflict-look-through buffer) t))))
                       *conflict-rescans*))
      soonest)))

;;; Looking through a buffer in the background

(defstruct (conflict-job (:include shown-text-job)
                         (:constructor make-conflict-job (buffer tell go)) (:copier nil))
  "BUFFER looked through for conflicts in the background, a look at a file
shown (SHOWN-TEXT-JOB); TELL as CONFLICT-LOOK-THROUGH takes it, GO a place:
once found, point goes to the first conflict if it is still there. RESULT,
once done, the conflicts as CONFLICT-FIND-ALL gives them."
  tell go)

(setf (documentation 'conflict-job-p 'function) "True when OBJECT is a CONFLICT-JOB."
      (documentation 'conflict-job-tell 'function) "Whether a look for conflicts says what it found: T, :FOUND only when some are, NIL."
      (documentation 'conflict-job-go 'function) "Where point was when the conflicts were asked for, to go to the first, or NIL.")

(tuatara.runtime:define-job-words conflict-job
  :doing "Looking for conflicts"
  :left-words "the conflicts are as they were")

(defun conflict-look-through (buffer &key tell go)
  "Look through BUFFER whole for conflicts in the background
\(CONFLICT-FIND-ALL), from its text as it is now; TELL T to say what is
found, :FOUND to say it only when some are; GO true to go to the first
conflict found unless point has moved meanwhile. A look under way is
started again, keeping what it was to say."
  (let ((running (tuatara.buffer:buffer-local buffer 'conflict-job)))
    (when running
      (setf tell (if (eq tell t) t (or (conflict-job-tell running) tell))
            go (or go (conflict-job-go running)))))
  (setf (tuatara.buffer:buffer-local buffer 'conflict-text-seen) (tuatara.buffer:buffer-content-number buffer))
  (start-shown-text-job (make-conflict-job buffer tell (and go (tuatara.buffer:buffer-point buffer))) 'conflict-job
                        (lambda (job)
                          (conflict-find-all (conflict-job-rope job) :cancel (tuatara.runtime:job-cancel job)))
                        :name "tuatara conflicts"))

(defmethod tuatara.runtime:job-take-in ((job conflict-job))
  "Take the conflicts found in among those the buffer holds (CONFLICT-PUT):
each followed through the edits made since, one put right meanwhile left
out; say how many when asked, go to the first. A buffer holding some has
its edits followed (CONFLICT-WATCH); one looked through by itself that never
held any does not."
  (let ((buffer (conflict-job-buffer job)))
    (when (shown-text-job-taken-p job 'conflict-job)
      (conflict-put buffer (conflict-job-version job) (conflict-job-result job))
      (let ((count (length (conflict-list buffer)))
            (tell (conflict-job-tell job))
            (go (conflict-job-go job)))
        (cond ((or (plusp count) (eq tell t))
               (when (plusp count) (setf (tuatara.buffer:buffer-local buffer 'conflict-seen) t))
               (conflict-watch buffer))
              ((not (tuatara.buffer:buffer-local buffer 'conflict-seen))
               (conflict-stop-watching buffer)))
        (conflict-follow-keymap buffer)
        (when (and go (plusp count) (eql go (tuatara.buffer:buffer-point buffer)))
          (setf (tuatara.buffer:buffer-point buffer) (conflict-start-at buffer (svref (conflict-list buffer) 0))))
        (when (and tell (or (plusp count) (eq tell t)))
          (if (plusp count)
              (message (substitute-command-keys "~:d conflict~:p in ~a: \\<*conflict-keymap*>\\[conflict-next] goes to the next, \\[conflict-keep-first-side] and \\[conflict-keep-second-side] keep a side"
                                                :for-format t)
                       count (escape-controls (tuatara.buffer:buffer-name buffer)))
              (message "No conflicts in ~a" (escape-controls (tuatara.buffer:buffer-name buffer)))))))))

(defun conflict-keymap-follows (buffer)
  "True when the keys BUFFER has for itself (its :OVERRIDE-KEYMAP) are those CONFLICT-FOLLOW-KEYMAP put there."
  (let ((own (tuatara.buffer:buffer-local buffer :override-keymap)))
    (and own (eq own (tuatara.buffer:buffer-local buffer 'conflict-keymap)))))

(defun conflict-follow-keymap (buffer)
  "Give BUFFER the keys under \\`C-c ^' while it holds conflicts
\(CONFLICT-MARKERS-ON-P), and take them away once it holds none. Where the
buffer has keys of its own already (its :OVERRIDE-KEYMAP, as a quick edit
or a snippet gives it), the conflicts' keys go in front of them, in a keymap
whose parent they are, and taking them away puts those keys back as they
were."
  (let ((own (tuatara.buffer:buffer-local buffer :override-keymap)))
    (cond ((and (conflict-markers-on-p buffer) (not (conflict-keymap-follows buffer)))
           (let ((keymap (if own (conflict-keys own) *conflict-keymap*)))
             (setf (tuatara.buffer:buffer-local buffer 'conflict-keymap) keymap
                   (tuatara.buffer:buffer-local buffer :override-keymap) keymap)))
          ((and (not (conflict-markers-on-p buffer)) (conflict-keymap-follows buffer))
           (setf (tuatara.buffer:buffer-local buffer :override-keymap) (keymap-parent own)
                 (tuatara.buffer:buffer-local buffer 'conflict-keymap) nil)))))

(defun conflict-look-again-p (buffer)
  "True when BUFFER, a file looked through once whose edits are not followed,
holds another text as it stands on disk than when it was: read again, or
filled after it was shown."
  (and (not (tuatara.buffer:buffer-modified-p buffer))
       (not (eql (tuatara.buffer:buffer-local buffer 'conflict-text-seen) (tuatara.buffer:buffer-content-number buffer)))
       (not (tuatara.buffer:buffer-local buffer 'conflict-job))
       (<= (tuatara.buffer:buffer-size buffer) *conflict-markers-max-bytes*)))

(defun conflict-look-at-shown (editor)
  "After each command: look through each file EDITOR's windows show for
conflicts, once, in the background (LOOK-AT-SHOWN-FILES), up to
*CONFLICT-MARKERS-MAX-BYTES*; again once its text is another as it stands on
disk, when its edits are not followed (CONFLICT-LOOK-AGAIN-P); and keep the
keys of one holding conflicts in front of keys it was given since."
  (look-at-shown-files editor 'conflict-checked (lambda (b) (conflict-look-through b :tell :found))
                       :most *conflict-markers-max-bytes*
                       :again (lambda (b)
                                (cond ((conflict-watched-p b) (conflict-follow-keymap b))
                                      ((conflict-look-again-p b) (conflict-look-through b :tell :found))))))

;;; Showing them

(defun conflict-ranges (buffer conflict)
  "The ranges CONFLICT of BUFFER is painted in, each (START END FACE): its
marker lines in :conflict-marker, the first side in :conflict-first, the base
in :conflict-base, the second side in :conflict-second; NIL when it is
broken (CONFLICT-PLACES)."
  (multiple-value-bind (start base separator end after) (conflict-places buffer conflict)
    (when start
      (flet ((marker (at) (list at (next-line-start buffer at) :conflict-marker))
             (side (from to face) (list (next-line-start buffer from) to face)))
        (remove-if (lambda (range) (>= (first range) (second range)))
                   (append (list (marker start) (side start (or base separator) :conflict-first))
                           (and base (list (marker base) (side base separator :conflict-base)))
                           (list (marker separator) (side separator end :conflict-second)
                                 (list end after :conflict-marker))))))))

(defun conflict-highlights (editor window rows syntax)
  "The ranges the conflicts shown in ROWS of WINDOW are painted in (CONFLICT-RANGES)."
  (declare (ignore editor syntax))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (plusp (length rows)) (conflict-markers-on-p buffer))
      (multiple-value-bind (from below)
          (conflict-index-range buffer (tuatara.display:row-start (aref rows 0))
                                (1+ (tuatara.display:row-end (aref rows (1- (length rows))))))
        (loop for i from from below below
              append (conflict-ranges buffer (svref (conflict-list buffer) i)))))))

(defun conflict-mode-line (editor buffer)
  "Mode line function: Conflicts and how many stand while BUFFER shows its conflicts (CONFLICT-MARKERS-ON-P)."
  (declare (ignore editor))
  (when (conflict-markers-on-p buffer)
    (format nil "Conflicts ~:d" (length (conflict-list buffer)))))

;;; Going to them and putting them right

(defun conflict-at (buffer at)
  "The conflict of BUFFER whose lines hold octet AT (its end too, when its
last line ends the text without a newline), as (CONFLICT START BASE
SEPARATOR END AFTER) after CONFLICT-PLACES, or NIL."
  (let* ((conflicts (conflict-list buffer))
         (i (conflict-index-from buffer (tuatara.buffer:buffer-line-start buffer at))))
    (when (< i (length conflicts))
      (let ((places (multiple-value-list (conflict-places buffer (svref conflicts i)))))
        (destructuring-bind (start &optional base separator end after) places
          (declare (ignore base separator end))
          (when (and start (<= start at)
                     (or (< at after) (= at after (tuatara.buffer:buffer-size buffer))))
            (cons (svref conflicts i) places)))))))

(defun conflict-go (direction)
  "Put point at the start of the next conflict after point (DIRECTION 1), or
the one before (-1), and say which of how many it is."
  (let* ((buffer (current-buffer))
         (starts (map 'list (lambda (c) (conflict-start-at buffer c)) (conflict-list buffer)))
         (point (point))
         (target (if (plusp direction)
                     (find-if (lambda (start) (> start point)) starts)
                     (find-if (lambda (start) (< start point)) starts :from-end t))))
    (if target
        (progn (goto target)
               (message "Conflict ~:d of ~:d" (1+ (position target starts)) (length starts)))
        (message "No conflict ~:[before~;after~] this one" (plusp direction)))))

(define-command conflict-next ()
  "Go to the start of the next conflict, its <<<<<<< line, and say which of how many it is."
  (conflict-go 1))

(define-command conflict-previous ()
  "Go to the start of the conflict before point, its <<<<<<< line, and say which of how many it is."
  (conflict-go -1))

(defun conflict-done-words (buffer)
  "What is said once the last conflict of BUFFER is put right: that none is
left, and, for a file in git, how to stage it."
  (format nil (substitute-command-keys "No conflicts left in ~a~:[~;: save it (\\[save-buffer]), then \\[git] and \\<git-status-mode>\\[git-stage] on its line stage it~]"
                                       :for-format t)
          (escape-controls (tuatara.buffer:buffer-name buffer))
          (let ((file (tuatara.buffer:buffer-file buffer)))
            (and file (repository-top (tuatara.io:parent-path file))))))

(defun conflict-outside-words ()
  "What is said when point is in no conflict."
  (substitute-command-keys "Point is in no conflict: \\<*conflict-keymap*>\\[conflict-next] goes to the next"))

(defun conflict-keep-parts (parts what)
  "Put the conflict point is in right, keeping PARTS of it, a list of :FIRST,
:BASE and :SECOND, in that order, its marker lines and the rest taken away,
one change to undo; WHAT says what was kept. Then go to the next conflict,
or the first left, saying how many are left; when none is, say so and how
to stage the file, point where the conflict was."
  (let* ((buffer (current-buffer))
         (entry (conflict-at buffer (point))))
    (check-writable buffer)
    (cond ((null entry) (message "~a" (conflict-outside-words)))
          ((and (member :base parts) (null (third entry)))
           (message "This conflict has no base"))
          (t
           (destructuring-bind (conflict start base separator end after) entry
             (let ((text (with-output-to-string (out)
                           (dolist (part parts)
                             (multiple-value-bind (from to)
                                 (ecase part
                                   (:first (values (next-line-start buffer start) (or base separator)))
                                   (:base (values (next-line-start buffer base) separator))
                                   (:second (values (next-line-start buffer separator) end)))
                               (when (< from to)
                                 (write-string (tuatara.buffer:buffer-string buffer :start from :end to) out)))))))
               (tuatara.buffer:buffer-replace buffer start after text)
               (setf (tuatara.buffer:buffer-local buffer 'conflict-list) (remove conflict (conflict-list buffer)))
               (conflict-follow-keymap buffer)
               (let* ((starts (map 'list (lambda (c) (conflict-start-at buffer c)) (conflict-list buffer)))
                      (next (or (find-if (lambda (s) (>= s start)) starts) (first starts))))
                 (goto (or next start))
                 (if next
                     (message "Kept ~a; ~:d conflict~:p left" what (length starts))
                     (message "~a" (conflict-done-words buffer))))))))))

(define-command conflict-keep-first-side ()
  "Put the conflict point is in right by keeping its first side, the lines
after its <<<<<<< line, and taking away the rest and its marker lines; then
go to the next conflict. One change to undo. Which side is which:
\\[conflict-markers-mode]."
  (conflict-keep-parts '(:first) "the first side"))

(define-command conflict-keep-second-side ()
  "Put the conflict point is in right by keeping its second side, the lines
after its ======= line, and taking away the rest and its marker lines; then
go to the next conflict. One change to undo. Which side is which:
\\[conflict-markers-mode]."
  (conflict-keep-parts '(:second) "the second side"))

(define-command conflict-keep-both-sides ()
  "Put the conflict point is in right by keeping both its sides, the first
then the second, the base and the marker lines taken away; then go to the
next conflict. One change to undo."
  (conflict-keep-parts '(:first :second) "both sides"))

(define-command conflict-keep-base ()
  "Put the conflict point is in right by keeping its base, the lines after
its ||||||| line, as they were before either side changed them, the sides
and the marker lines taken away; then go to the next conflict. One change to
undo. git writes the base only when it is set to show it (merge.conflictStyle
diff3); a conflict without one is said to have none."
  (conflict-keep-parts '(:base) "the base"))

(defun conflict-marker-label (buffer at)
  "The name after the marker line of BUFFER starting at octet AT, or NIL when it has none."
  (let* ((line (tuatara.buffer:buffer-string buffer :start at :end (tuatara.buffer:buffer-line-end buffer at)))
         (name (string-trim '(#\Space #\Return) (subseq line (min (length line) +conflict-marker-length+)))))
    (and (plusp (length name)) name)))

(defun conflict-side-lines (text)
  "The lines of TEXT, a side of a conflict as it stands, each line ended by a
newline, as strings without their newlines, exactly as they are: empty
lines kept."
  (if (zerop (length text))
      '()
      (uiop:split-string (if (char= (char text (1- (length text))) #\Newline) (subseq text 0 (1- (length text))) text)
                         :separator '(#\Newline))))

(defstruct (conflict-sides-job (:include tuatara.runtime:job)
                               (:constructor make-conflict-sides-job (buffer rope first second names)) (:copier nil))
  "How the two sides of a conflict differ, worked out in the background for
BUFFER, the buffer showing it, a job: ROPE the text, FIRST and SECOND each
\(FROM . TO), the octets of a side, NAMES the sides' names; TEXT once done."
  rope first second names text)

(setf (documentation 'conflict-sides-job-p 'function) "True when OBJECT is a CONFLICT-SIDES-JOB.")

(tuatara.runtime:define-job-words conflict-sides-job
  :doing "Comparing the two sides"
  :left-words "nothing compared"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-take-in ((job conflict-sides-job))
  "Show how the sides differ, that they are the same, or why they were not compared."
  (show-reading-result job (tuatara.buffer:buffer-name (conflict-sides-job-buffer job))
                       (let ((text (conflict-sides-job-text job)))
                         (if (equal text "") (format nil "The two sides are the same.~%") text))
                       "~a: the sides could not be compared: ~a~%"))

(define-command conflict-show-sides-diff ()
  "Show how the two sides of the conflict point is in differ, in a buffer of
their own in another window, which \\`q' closes: the lines only the first
side has after -, those only the second has after +, three lines around
each change, the sides named by their marker lines. Worked out in the
background, the editor free meanwhile."
  (let* ((buffer (current-buffer))
         (entry (conflict-at buffer (point))))
    (if (null entry)
        (message "~a" (conflict-outside-words))
        (destructuring-bind (conflict start base separator end after) entry
          (declare (ignore conflict after))
          (let ((shown (fresh-made-buffer (format nil "*Conflict: ~a*" (tuatara.buffer:buffer-name buffer)) 'diff-mode
                                          (format nil "Comparing the two sides...~%")))
                (rope (if (tuatara.buffer:buffer-mapping buffer)
                          (tuatara.buffer:buffer-slice buffer 0 (tuatara.buffer:buffer-size buffer))
                          (tuatara.buffer:buffer-text buffer))))
            ;; files asked for from it are looked for beside the conflicted file
            (setf (tuatara.buffer:buffer-local shown :directory) (buffer-directory buffer))
            (tuatara.runtime:start-job-thread
             (make-conflict-sides-job shown rope (cons (next-line-start buffer start) (or base separator))
                                      (cons (next-line-start buffer separator) end)
                                      (list (or (conflict-marker-label buffer start) "first side")
                                            (or (conflict-marker-label buffer end) "second side")))
             (lambda (job)
               (flet ((side (span) (conflict-side-lines (tuatara.text:rope-string (conflict-sides-job-rope job)
                                                                                   :start (car span) :end (cdr span)))))
                 (setf (conflict-sides-job-text job)
                       (unified-diff (side (conflict-sides-job-first job)) (side (conflict-sides-job-second job))
                                     :old-name (first (conflict-sides-job-names job))
                                     :new-name (second (conflict-sides-job-names job))))))
             :name "tuatara conflict sides")
            (pop-to-buffer shown :other t))))))

(commands-for-buffers 'conflict-markers-on-p "a file with conflicts in it (\\[conflict-markers-mode])"
                      'conflict-next 'conflict-previous 'conflict-keep-first-side 'conflict-keep-second-side
                      'conflict-keep-both-sides 'conflict-keep-base 'conflict-show-sides-diff)

(define-command conflict-markers-mode ()
  "Show the conflicts a merge left in the current buffer, and give it the
keys that put them right, or stop. A conflict is the lines git writes where
two changes to the same lines meet: <<<<<<< and the first side's name, the
first side, ||||||| and the base where it writes one (the lines as they were
before either side changed them), =======, the second side, and >>>>>>> and
the second side's name. In a merge the first side is the branch you are on
and the second the one merged into it, what git status calls us and them;
in a rebase it is the other way round: the first side is the branch being
rebased onto, the second your own commit put on it. Each part is painted in
a face of its own (:conflict-first, :conflict-base, :conflict-second, the
marker lines in :conflict-marker). It is on by itself in a file that holds
whole conflicts, looked through in the background when it is shown (up to
*CONFLICT-MARKERS-MAX-BYTES*); edits are followed, so that undo brings a
conflict back; a lone ======= line is none.
\\<*conflict-keymap*>\\[conflict-next] and \\[conflict-previous] go to the next conflict and the one
before; \\[conflict-keep-first-side] keeps the first side, \\[conflict-keep-second-side] the second, \\[conflict-keep-both-sides]
both, \\[conflict-keep-base] the base, each one change to undo, point going on
to the next; \\[conflict-show-sides-diff] shows how the sides differ. Once none is left, it
says how to stage the file in git. With a count, on when it is positive,
off when it is zero or less."
  (let* ((buffer (current-buffer))
         (on (switched-by-count (conflict-markers-on-p buffer))))
    (setf (tuatara.buffer:buffer-local buffer 'conflict-off) (not on))
    (conflict-follow-keymap buffer)
    (if on
        (progn (conflict-look-through buffer :tell t)
               (message "Looking for conflicts in ~a..." (escape-controls (tuatara.buffer:buffer-name buffer))))
        (message "Conflict markers off"))))

(defun install-conflicts (editor)
  "Look through the files EDITOR's windows show for conflicts, follow the
edits of those holding some, paint them, and name them in the mode line."
  (pushnew 'conflict-look-at-shown (editor-post-command-functions editor))
  (pushnew 'conflict-pause-tick *tick-functions*)
  (pushnew 'conflict-highlights tuatara.command:*text-highlight-functions*)
  (pushnew 'conflict-mode-line *mode-line-functions*))
