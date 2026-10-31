;;;; src/text/version.lisp --- Text versions, edit batches and anchors.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A VERSION is a rope plus the batch of edits that produced it, linked to the
;;;; version that replaced it.  Anyone holding an old version (a worker with a
;;;; snapshot) can map byte positions forward to the current version by walking
;;;; the NEXT links and mapping through each batch in O(log edits).  An anchor
;;;; holds no version, only the version's LINK: the same chain without the
;;;; texts, so that an anchor resolved long ago keeps the edits since alive but
;;;; never a rope.  Versions no longer referenced are garbage collected, so the
;;;; history is exactly as long as something needs it.

(in-package #:tuatara.text)

(defstruct (swap (:constructor make-swap (before after forward backward)) (:copier nil))
  "A change of a whole text too large to describe edit by edit: the rope BEFORE
and the rope AFTER, with FORWARD mapping a byte position and a bias from the
one to the other and BACKWARD mapping back. A replacement across a file of many
gigabytes arrives this way."
  (before nil :type rope :read-only t)
  (after nil :type rope :read-only t)
  (forward nil :type function :read-only t)
  (backward nil :type function :read-only t))

(setf (documentation 'make-swap 'function) "Return a swap from rope BEFORE to rope AFTER with the two position maps."
      (documentation 'swap-p 'function) "True when OBJECT is a SWAP."
      (documentation 'swap-before 'function) "The rope a SWAP replaced."
      (documentation 'swap-after 'function) "The rope a SWAP put in its place."
      (documentation 'swap-forward 'function) "SWAP's map of (POSITION BIAS) from the rope before to the rope after."
      (documentation 'swap-backward 'function) "SWAP's map of (POSITION BIAS) from the rope after to the rope before.")

(defstruct (batch (:constructor %make-batch (edits deltas positions))
                  (:constructor %make-swap-batch (swap))
                  (:copier nil))
  "An applied edit batch: EDITS (a sorted simple-vector of EDIT), DELTAS (a
fixnum vector where DELTAS[k] is the total change in length after edits 0..k),
and POSITIONS (the vector APPLY-EDITS returned). A batch that swapped the whole
text has no edits and its SWAP instead."
  (edits #() :type simple-vector :read-only t)
  (deltas (make-array 0 :element-type 'fixnum) :type fixvec :read-only t)
  (positions (make-array 0 :element-type 'fixnum) :type fixvec :read-only t)
  (swap nil :type (or null swap) :read-only t))

(setf (documentation 'batch-p 'function) "True when OBJECT is a BATCH."
      (documentation 'batch-edits 'function) "The sorted edits of BATCH."
      (documentation 'batch-deltas 'function) "Prefix sums of length change per edit of BATCH."
      (documentation 'batch-positions 'function) "The positions APPLY-EDITS returned for BATCH."
      (documentation 'batch-swap 'function) "The SWAP of a batch that replaced the whole text, or NIL.")

(defstruct (link (:constructor make-link (map)) (:copier nil))
  "A version's place in its text's history, without its text: MAP takes a
position of the version before to this one (NIL for the first: a batch of
edits, or a function of a position and a bias for a swap of the whole text);
NEXT is the link of the version after, or NIL while it is current."
  (map nil :type (or null batch function) :read-only t)
  (next nil :type (or null link)))

(setf (documentation 'link-p 'function) "True when OBJECT is a LINK."
      (documentation 'make-link 'function) "Return a link whose position map from the version before is MAP."
      (documentation 'link-map 'function) "How a LINK's version maps positions of the one before: a batch, a function, or NIL."
      (documentation 'link-next 'function) "The link of the version after, or NIL.")

(defstruct (version (:constructor %make-version (rope number batch &aux (link (make-link (batch-link-map batch)))))
                    (:copier nil))
  "One version of a text. ROPE is its content; NUMBER counts versions from 0;
BATCH is the batch that produced it from the previous version (NIL for the
first); NEXT is the version that replaced it, or NIL while it is current; LINK
its place in the history without the text (anchors hold it)."
  (rope (make-rope) :type rope :read-only t)
  (number 0 :type fixnum :read-only t)
  (batch nil :type (or null batch) :read-only t)
  (next nil :type (or null version))
  (link nil :type link :read-only t))

(setf (documentation 'version-p 'function) "True when OBJECT is a VERSION."
      (documentation 'version-rope 'function) "The rope holding VERSION's text."
      (documentation 'version-number 'function) "VERSION's sequence number, 0 for the first."
      (documentation 'version-batch 'function) "The batch that produced VERSION, or NIL for the first."
      (documentation 'version-next 'function) "The version that replaced VERSION, or NIL while it is current."
      (documentation 'version-link 'function) "VERSION's place in the history, without its text (a LINK).")

(defun batch-link-map (batch)
  "What a link keeps of BATCH to map positions through it: the batch, or for a
swap of the whole text its function forward alone, which holds no rope."
  (and batch (if (batch-swap batch) (swap-forward (batch-swap batch)) batch)))

(defun make-version (&optional (rope (make-rope)))
  "Return a first version holding ROPE."
  (%make-version rope 0 nil))

(defun version-current-p (version)
  "True when nothing has replaced VERSION yet."
  (null (version-next version)))

(defun edits-batch (rope edits)
  "Apply EDITS (a sequence of EDIT, sorted and non-overlapping) to ROPE, which is
left unchanged: the new rope, and the batch that made it, as an undo step
takes it back. Complexity: that of APPLY-EDITS plus O(edits)."
  (multiple-value-bind (new positions) (apply-edits rope edits)
    (let* ((edits (coerce edits 'simple-vector))
           (n (length edits))
           (deltas (make-array n :element-type 'fixnum))
           (delta 0))
      (declare (type fixnum delta))
      (loop for k of-type fixnum from 0 below n
            for e of-type edit = (aref edits k)
            do (incf delta (- (edit-insert-length e) (edit-delete-length e)))
               (setf (aref deltas k) delta))
      (values new (%make-batch edits deltas positions)))))

(defun swap-batch (before after forward backward)
  "The batch of a swap of the whole text BEFORE for AFTER, FORWARD mapping a
byte position and a bias from the one to the other and BACKWARD back, as an
undo step takes it back."
  (%make-swap-batch (make-swap before after forward backward)))

(defun advance-version (version rope batch what)
  "The version after VERSION, which must be current, holding ROPE, made by
BATCH and linked as VERSION's NEXT; returned with BATCH. When VERSION is not
current, an error saying WHAT applies to the newest version only."
  (unless (version-current-p version)
    (error "Version ~a is not current; ~a applies to the newest version only." (version-number version) what))
  (let ((new (%make-version rope (1+ (version-number version)) batch)))
    (setf (version-next version) new
          (link-next (version-link version)) (version-link new))
    (values new batch)))

(defun version-commit (version edits)
  "Apply EDITS (a sequence of EDIT, sorted and non-overlapping) to VERSION, which
must be current, and return the new version, linked as VERSION's NEXT. Also
returns the batch as a second value (EDITS-BATCH). Complexity: that of
APPLY-EDITS plus O(edits)."
  (multiple-value-bind (rope batch) (edits-batch (version-rope version) edits)
    (advance-version version rope batch "an edit")))

(defun version-swap (version rope forward backward)
  "Replace the whole text of VERSION, which must be current, with ROPE: return
the new version, linked as VERSION's NEXT, and its batch, whose SWAP maps
positions with FORWARD and back with BACKWARD (functions of a byte position and
a bias). Complexity: O(1)."
  (advance-version version rope (%make-swap-batch (make-swap (version-rope version) rope forward backward)) "a swap"))

(defun version-adopt (version rope)
  "Make ROPE, the same octets as VERSION's text with more known of them (their
counts), the text of a new version linked as VERSION's NEXT; VERSION must be
current. The new version's batch has no edits: every position stays where it
is. Returns the new version and its batch. Complexity: O(1)."
  (advance-version version rope (%make-batch #() (make-array 0 :element-type 'fixnum) (make-array 0 :element-type 'fixnum))
                   "adopting a text"))

(defun version-undo-edits (version)
  "Return the edits that undo the batch that produced VERSION, applicable to
VERSION's text, or NIL for a first version."
  (let ((batch (version-batch version)))
    (and batch (inverse-edits (batch-edits batch) (batch-positions batch)))))

(defun map-position (position batch bias)
  "Map byte POSITION in the text before BATCH to the text after it. BIAS is
:BEFORE or :AFTER: a position at an insertion point, or inside deleted text,
lands before or after the inserted text respectively. Complexity: O(log edits)."
  (declare (type fixnum position))
  (let ((swap (batch-swap batch)))
    (when swap
      (return-from map-position (funcall (swap-forward swap) position bias))))
  (let* ((edits (batch-edits batch))
         (deltas (batch-deltas batch))
         (n (length edits)))
    (declare (type simple-vector edits) (type fixvec deltas))
    (if (zerop n)
        position
        ;; k: the last edit whose start is < POSITION (bias :before) or <= POSITION
        ;; (bias :after); -1 when there is none.
        (let* ((strict (eq bias :before))
               (k (1- (tuatara.runtime:bisect (mid 0 n)
                        (let ((start (edit-start (the edit (aref edits mid)))))
                          (declare (type fixnum start))
                          (if strict (< start position) (<= start position)))))))
          (declare (type fixnum k))
          (if (< k 0)
              position
              (let* ((e (aref edits k))
                     (before (if (zerop k) 0 (aref deltas (1- k)))))
                (declare (type edit e) (type fixnum before))
                (if (> position (edit-end e))
                    (+ position (aref deltas k))
                    (+ (edit-start e) before
                       (if (eq bias :after) (edit-insert-length e) 0)))))))))

(defun map-forward (position from to bias)
  "Map byte POSITION in version FROM to version TO, which must be FROM or one of
its successors, with BIAS as in MAP-POSITION; FROM and TO may be versions or
their links. Complexity: O(versions between × log edits)."
  (declare (type fixnum position))
  (let ((link (if (version-p from) (version-link from) from))
        (to (if (version-p to) (version-link to) to)))
    (loop until (eq link to)
          do (let ((next (link-next link)))
               (unless next
                 (error "A version is not a successor of the one a position was given in."))
               (let ((map (link-map next)))
                 (setf position (if (functionp map) (funcall map position bias) (map-position position map bias))
                       link next))))
    position))

;;; What changed between two versions

(defun stretches-through (stretches batch)
  "STRETCHES, as CHANGED-STRETCHES gives them up to the text before BATCH,
carried through BATCH: each edit of it joined to the stretches it meets or
touches, those after it moved. Complexity: O(edits + stretches)."
  (let* ((edits (batch-edits batch))
         (n (length edits))
         (k 0) (out '())
         ;; between stretches, the text before BATCH less the first version's; after it less before
         (shift 0) (delta 0))
    (declare (type fixnum n k shift delta))
    (loop
      (let ((stretch (first stretches))
            (edit (and (< k n) (svref edits k))))
        (unless (or stretch edit) (return))
        (let* ((start (min (if stretch (third stretch) most-positive-fixnum)
                           (if edit (edit-start edit) most-positive-fixnum)))
               (end start) (old-start (- start shift)) (grown 0))
          (declare (type fixnum start end old-start grown))
          ;; everything that meets what is gathered so far, gathered
          (loop
            (let ((stretch (first stretches))
                  (edit (and (< k n) (svref edits k))))
              (cond ((and stretch (<= (the fixnum (third stretch)) end))
                     (setf end (max end (the fixnum (fourth stretch)))
                           shift (- (the fixnum (fourth stretch)) (the fixnum (second stretch))))
                     (pop stretches))
                    ((and edit (<= (edit-start edit) end))
                     (setf end (max end (edit-end edit)))
                     (incf grown (- (edit-insert-length edit) (edit-delete-length edit)))
                     (incf k))
                    (t (return)))))
          (push (list old-start (- end shift) (+ start delta) (+ end delta grown)) out)
          (incf delta grown))))
    (nreverse out)))

(defun changed-stretches (from to)
  "The stretches of a text that differ between its versions FROM and TO, TO
the later: a list in order of (OLD-START OLD-END NEW-START NEW-END), byte
offsets of each in FROM's text and in TO's, the text between them the same in
both; :WHOLE when that is not known, the whole text swapped between them or TO
not after FROM. The edits of every version between are taken in one pass.
Complexity: O(edits + stretches) for each version between."
  (let ((stretches '()) (at from))
    (loop until (eq at to)
          do (let* ((next (version-next at))
                    (batch (and next (version-batch next))))
               (when (or (null batch) (batch-swap batch))
                 (return-from changed-stretches :whole))
               (setf stretches (stretches-through stretches batch)
                     at next)))
    stretches))

;;; Anchors

(defstruct (anchor (:constructor %make-anchor (link cached bias)) (:copier nil))
  "A byte position that follows the text through edits. CACHED is the position
in the version of LINK; ANCHOR-POSITION maps it forward on demand and caches
the result. BIAS is :BEFORE (stays before text inserted at it) or :AFTER
\(moves after it)."
  (link nil :type link)
  (cached 0 :type fixnum)
  (bias :before :type (member :before :after)))

(setf (documentation 'anchor-p 'function) "True when OBJECT is an ANCHOR."
      (documentation 'anchor-link 'function) "The link of the version ANCHOR's cached position is valid in."
      (documentation 'anchor-bias 'function) "ANCHOR's bias, :BEFORE or :AFTER."
      (documentation 'anchor-cached 'function) "ANCHOR's position in ANCHOR-VERSION, possibly stale; use ANCHOR-POSITION.")

(defun make-anchor (version position &key (bias :before))
  "Return an anchor at byte POSITION of VERSION with BIAS (:BEFORE by default)."
  (declare (type fixnum position))
  (unless (<= 0 position (rope-bytes (version-rope version)))
    (error "Anchor position ~a is outside the text (0..~a)." position (rope-bytes (version-rope version))))
  (%make-anchor (version-link version) position bias))

(defun anchor-position (anchor version)
  "Return ANCHOR's byte position in VERSION, mapping it forward from the version
it was last resolved in and caching the result. VERSION must be that version or
a successor of it."
  (let ((link (version-link version)))
    (unless (eq (anchor-link anchor) link)
      (setf (anchor-cached anchor)
            (map-forward (anchor-cached anchor) (anchor-link anchor) link (anchor-bias anchor))
            (anchor-link anchor) link)))
  (anchor-cached anchor))

(defun anchor-move (anchor version position)
  "Set ANCHOR to byte POSITION of VERSION."
  (declare (type fixnum position))
  (unless (<= 0 position (rope-bytes (version-rope version)))
    (error "Anchor position ~a is outside the text (0..~a)." position (rope-bytes (version-rope version))))
  (setf (anchor-cached anchor) position
        (anchor-link anchor) (version-link version))
  anchor)
