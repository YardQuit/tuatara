;;;; src/text/edits.lisp --- Batches of edits applied to a rope in one descent.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.text)

;;; Edits

(defstruct (edit (:constructor make-edit (start end &optional insert)) (:copier nil))
  "One edit against a text: delete the octets from START below END (byte offsets
in the text the edit applies to) and put INSERT, an octet vector or NIL, in their
place. A pure insertion has START = END; a pure deletion has no INSERT. After
APPLY-EDITS, DELETED holds the octets that were removed, for undo."
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  (insert nil :type (or null octets))
  (deleted nil))

(setf (documentation 'make-edit 'function)
      "Return an EDIT deleting byte range START below END and inserting INSERT (an octet vector or NIL)."
      (documentation 'edit-p 'function) "True when OBJECT is an EDIT."
      (documentation 'edit-start 'function) "Byte offset where EDIT's deleted range starts."
      (documentation 'edit-end 'function) "Byte offset where EDIT's deleted range ends (exclusive)."
      (documentation 'edit-insert 'function) "The octets EDIT inserts, or NIL."
      (documentation 'edit-deleted 'function)
      "The octets EDIT removed, filled in by APPLY-EDITS; NIL before that or when nothing was deleted.")

(declaim (inline edit-insert-length edit-delete-length))
(defun edit-insert-length (edit)
  "Number of octets EDIT inserts."
  (let ((insert (edit-insert edit))) (if insert (length (the octets insert)) 0)))
(defun edit-delete-length (edit)
  "Number of octets EDIT deletes."
  (- (edit-end edit) (edit-start edit)))

(defun check-edits (rope edits)
  "Signal an error unless EDITS (a simple-vector of EDIT) are within ROPE, sorted
by START and non-overlapping. Also clears each edit's DELETED slot, so the same
edit objects can be applied again. Returns true when two edits that both insert
meet (one ends where the next starts, or only deletions right after the one
stand between), and as a second value true when an edit inserts nothing or
octets beginning with a newline or ending with a carriage return, which could
meet a carriage return or a newline beside it (JOINING-RETURNS-P).
Complexity: O(number of edits)."
  (declare (type simple-vector edits))
  (let ((total (rope-bytes rope)) (previous-end -1) (previous-inserts nil) (meeting nil) (edges nil)
        ;; edits made at many places at once share their insert: looked at once
        (looked-at nil))
    (declare (type fixnum total previous-end))
    (loop for e of-type edit across edits
          do (when (edit-deleted e) (setf (edit-deleted e) nil))
             (let ((insert (edit-insert e)))
               (unless (or edges (and insert (eq insert looked-at)))
                 (setf edges (or (null insert) (zerop (length (the octets insert)))
                                 (= 10 (aref (the octets insert) 0))
                                 (= 13 (aref (the octets insert) (1- (length (the octets insert))))))
                       looked-at insert)))
             (unless (and (<= 0 (edit-start e)) (<= (edit-start e) (edit-end e)) (<= (edit-end e) total))
               (error "Edit ~a..~a is outside the text (0..~a) or reversed." (edit-start e) (edit-end e) total))
             (unless (<= previous-end (edit-start e))
               (error "Edits overlap or are not sorted: an edit ends at ~a, the next starts at ~a."
                      previous-end (edit-start e)))
             (when (and previous-inserts (edit-insert e) (= previous-end (edit-start e)))
               (setf meeting t))
             ;; an insert's reach goes on over deletions right after it
             (setf previous-inserts (or (and (edit-insert e) t)
                                        (and previous-inserts (= previous-end (edit-start e))))
                   previous-end (edit-end e)))
    (values meeting edges)))

(defun rebuild-uncounted-span (node leaf-start edits i j scratch)
  "Apply EDITS[I..J) to NODE, a span whose counts are not known yet (its edges
may fall inside a character), whose text starts at byte LEAF-START, and return
the simple-vector of nodes that replace it: the octets before the first edit
and after the last stay spans of the file, still uncounted, and only the
stretch between, whose edges are the edits' own and so fall between
characters, becomes in-memory leaves with their counts (REBUILD-LEAF).
Complexity: O(the edited stretch + edit sizes)."
  (declare (type span node) (type fixnum leaf-start i j) (type simple-vector edits))
  (let* ((size (node-bytes node))
         (lo (loop for k from i below j minimize (max 0 (- (edit-start (aref edits k)) leaf-start))))
         (hi (loop for k from i below j maximize (min size (- (edit-end (aref edits k)) leaf-start))))
         (hi (max lo hi))
         (middle (let ((octets (make-octets (- hi lo))))
                   (piece-copy node lo hi octets 0)
                   (%make-leaf octets 0 (- hi lo) -1 -1 -1 -1 -1 -1)))
         (mapping (span-mapping node))
         (offset (span-offset node)))
    (concatenate 'simple-vector
                 (if (plusp lo) (vector (%make-span mapping offset lo -1 -1 -1 -1 -1 -1)) #())
                 (rebuild-leaf middle (+ leaf-start lo) edits i j scratch)
                 (if (< hi size) (vector (%make-span mapping (+ offset hi) (- size hi) -1 -1 -1 -1 -1 -1)) #()))))

(defun rebuild-leaf (node leaf-start edits i j scratch)
  "Apply EDITS[I..J) to NODE (a leaf or span), whose text starts at byte
LEAF-START of the whole rope, and return a simple-vector of the in-memory leaves
that replace it (empty when all of its text is deleted). Edits that begin before
LEAF-START are deletions continuing from an earlier leaf; their insertions
belong to the leaf where they began. SCRATCH is a cons whose car is an octet
vector of +SPAN-SIZE+ octets that a span's content is copied into, made when
the first span is edited and reused across the batch.
Complexity: O(leaf size + edit sizes)."
  (declare (type fixnum leaf-start i j) (type simple-vector edits) (type cons scratch)
           (optimize speed (safety 1)))
  (when (and (span-p node) (not (node-indexed-p node)))
    (return-from rebuild-leaf (rebuild-uncounted-span node leaf-start edits i j scratch)))
  (let* ((old (if (leaf-p node)
                  (leaf-octets node)
                  (piece-copy node 0 (node-bytes node) (or (car scratch) (setf (car scratch) (make-octets +span-size+))) 0)))
         (base (if (leaf-p node) (leaf-start node) 0))
         (old-len (node-bytes node))
         (new-len old-len)
         (indexed (node-indexed-p node))
         (dchars 0) (dutf16 0) (dlines 0) (dinvalid 0) (dreturns 0) (ins-returns 0) (dnewlines 0) (ins-newlines 0)
         ;; Multi-cursor commands share one insertion vector across all edits:
         ;; summarise it once, not once per edit.
         (last-ins nil) (ins-chars 0) (ins-utf16 0) (ins-lines 0) (ins-invalid 0) (same 0)
         ;; a carriage return deleted, inserted or just before an edit may end a line
         ;; or not by what comes to follow it: the line ends are counted again
         (recount nil))
    (declare (type octets old)
             (type fixnum base old-len new-len dchars dutf16 dlines dinvalid dreturns dnewlines ins-chars ins-utf16 ins-lines ins-invalid ins-returns ins-newlines same)
             (type (or null octets) last-ins))
    (flet ((take-inserts ()
             ;; the counts of the insert used SAME times since it was summarised
             (incf dchars (* same ins-chars)) (incf dutf16 (* same ins-utf16)) (incf dlines (* same ins-lines))
             (incf dinvalid (* same ins-invalid)) (incf dreturns (* same ins-returns)) (incf dnewlines (* same ins-newlines))
             (setf same 0)))
    ;; Pass 1: the new length, the count deltas, and the deleted octets for undo.
    (loop for k of-type fixnum from i below j
          for e of-type edit = (aref edits k)
          do (let ((s (max 0 (- (edit-start e) leaf-start)))
                   (en (min old-len (- (edit-end e) leaf-start))))
               (declare (type fixnum s en))
               (when (and indexed (> s 0) (= 13 (aref old (+ base s -1))))
                 (setf recount t))
               (when (> en s)
                 (decf new-len (- en s))
                 (when indexed
                   (multiple-value-bind (c u l v r n) (summarise old (+ base s) (+ base en))
                     (declare (type fixnum c u l v r n))
                     (decf dchars c) (decf dutf16 u) (decf dlines l) (decf dinvalid v) (decf dreturns r) (decf dnewlines n)
                     (when (plusp r) (setf recount t))))
                 (push (subseq old (+ base s) (+ base en)) (edit-deleted e)))
               (when (and (edit-insert e) (>= (edit-start e) leaf-start))
                 (let ((ins (edit-insert e)))
                   (declare (type octets ins))
                   (when indexed
                     (unless (eq ins last-ins)
                       (take-inserts)
                       (multiple-value-setq (ins-chars ins-utf16 ins-lines ins-invalid ins-returns ins-newlines) (summarise ins 0 (length ins)))
                       (setf last-ins ins)
                       (when (plusp ins-returns) (setf recount t)))
                     (incf same))
                   (incf new-len (length ins))))))
    (take-inserts))
    (if (zerop new-len)
        #()
        ;; Pass 2: build the new octets in one pass.
        (let ((new (make-octets new-len)) (src 0) (dst 0))
          (declare (type octets new) (type fixnum src dst))
          (loop for k of-type fixnum from i below j
                for e of-type edit = (aref edits k)
                do (let ((s (max 0 (- (edit-start e) leaf-start)))
                         (en (min old-len (- (edit-end e) leaf-start))))
                     (declare (type fixnum s en))
                     (replace new old :start1 dst :start2 (+ base src) :end2 (+ base s))
                     (incf dst (- s src))
                     (setf src en)
                     (when (and (edit-insert e) (>= (edit-start e) leaf-start))
                       (let ((ins (edit-insert e)))
                         (declare (type octets ins))
                         (replace new ins :start1 dst)
                         (incf dst (length ins))))))
          (replace new old :start1 dst :start2 (+ base src) :end2 (+ base old-len))
          (cond ((> new-len +max-leaf+) (chunk-octets new 0 new-len))
                (indexed (vector (%make-leaf new 0 new-len
                                             (+ (node-chars node) dchars)
                                             (+ (node-utf16 node) dutf16)
                                             (if recount
                                                 (nth-value 2 (summarise new 0 new-len))
                                                 (+ (node-lines node) dlines))
                                             (+ (node-invalid node) dinvalid)
                                             (+ (node-returns node) dreturns)
                                             (+ (node-newlines node) dnewlines))))
                (t (vector (make-leaf new))))))))

(defun merge-small-leaves (leaves)
  "Return LEAVES (a simple-vector of leaves and spans) with adjacent ones merged
into in-memory leaves wherever one of them is smaller than +MIN-LEAF+ and the
pair fits in +MAX-LEAF+. Unindexed spans are left alone."
  (declare (type simple-vector leaves))
  (if (< (length leaves) 2)
      leaves
      (let ((out (make-array 0 :adjustable t :fill-pointer 0))
            (current (aref leaves 0)))
        (loop for k from 1 below (length leaves)
              for next = (aref leaves k)
              do (if (and (node-indexed-p current) (node-indexed-p next)
                          (or (< (node-bytes current) +min-leaf+) (< (node-bytes next) +min-leaf+))
                          (<= (+ (node-bytes current) (node-bytes next)) +max-leaf+))
                     (let* ((a (node-bytes current)) (b (node-bytes next))
                            (merged (make-octets (+ a b))))
                       (piece-copy current 0 a merged 0)
                       (piece-copy next 0 b merged a)
                       (setf current (%make-leaf merged 0 (+ a b)
                                                 (+ (node-chars current) (node-chars next))
                                                 (+ (node-utf16 current) (node-utf16 next))
                                                 (+ (node-lines current) (node-lines next))
                                                 (+ (node-invalid current) (node-invalid next))
                                                 (+ (node-returns current) (node-returns next))
                                                 (+ (node-newlines current) (node-newlines next)))))
                     (progn (vector-push-extend current out) (setf current next))))
        (vector-push-extend current out)
        (coerce out 'simple-vector))))

(defun rebuild-branch (branch branch-start edits i j scratch)
  "Apply EDITS[I..J) to BRANCH, whose text starts at byte BRANCH-START, and return
a simple-vector of branches of the same height that replace it (empty when all
text below it is deleted). Untouched children are shared with the old tree.
SCRATCH is passed down to REBUILD-LEAF."
  (declare (type branch branch) (type fixnum branch-start i j) (type simple-vector edits)
           (optimize speed (safety 1)))
  (let* ((children (branch-children branch))
         (last (1- (length children)))
         (new (make-array 0 :adjustable t :fill-pointer 0))
         (child-start branch-start)
         (k i))
    (declare (type fixnum child-start k last))
    (loop for ci from 0 to last
          for child = (aref children ci)
          do (let* ((child-end (+ child-start (node-bytes child)))
                    ;; m: the first edit at or beyond K that starts at or after CHILD-END
                    ;; (binary search, the edits are sorted by start), except that the
                    ;; last child takes everything that is left.
                    (m (if (= ci last)
                           j
                           (tuatara.runtime:bisect (mid k j) (< (edit-start (the edit (aref edits mid))) child-end)))))
               (declare (type fixnum child-end m))
               (if (= k m)
                   (vector-push-extend child new)
                   (loop for node across (if (branch-p child)
                                             (rebuild-branch child child-start edits k m scratch)
                                             (rebuild-leaf child child-start edits k m scratch))
                         do (vector-push-extend node new)))
               ;; Only the last edit of a child can delete past CHILD-END (the edits
               ;; are non-overlapping); if it does, it continues into the next child.
               (setf k (if (and (> m k) (> (edit-end (the edit (aref edits (1- m)))) child-end))
                           (1- m)
                           m))
               (setf child-start child-end)))
    (let ((nodes (coerce new 'simple-vector)))
      (cond ((zerop (length nodes)) #())
            (t (unless (branch-p (aref nodes 0))
                 (setf nodes (merge-small-leaves nodes)))
               (group-into-branches nodes))))))

(defun insert-shape (insert)
  "Two values for INSERT, an octet vector or NIL: whether it begins with a
continuation octet, and how many octets at its end begin a sequence it does
not finish."
  (if (or (null insert) (zerop (length insert)))
      (values nil 0)
      (values (continuation-byte-p (aref insert 0)) (octets-pending insert 0 (length insert)))))

(defun continuation-at-p (rope byte)
  "True when the octet at BYTE of ROPE is a continuation octet; NIL at the end."
  (and (< byte (rope-bytes rope)) (continuation-byte-p (rope-octet rope byte))))

(defun rope-pending-before (rope byte)
  "How many octets just before BYTE of ROPE begin a sequence not finished there (0 to 3)."
  (with-rope-window (window base end) (rope (- byte 3) byte 3)
    (octets-pending window 0 end)))

(defun past-continuations (rope byte)
  "BYTE of ROPE moved past the continuation octets there, at most three, and
on to where a character starts."
  (let ((total (rope-bytes rope)) (end byte))
    (loop while (and (< end total) (< (- end byte) 3) (continuation-at-p rope end)) do (incf end))
    (loop while (and (< end total) (not (char-boundary-p rope end))) do (incf end))
    end))

(defun group-replacement (rope edits members start end)
  "The octets that replace START below END of ROPE for the edits of EDITS whose
indices, in order, are MEMBERS: the old octets around and between their ranges
and their inserts."
  (let ((parts '()) (at start))
    (dolist (k members)
      (let ((e (aref edits k)))
        (when (< at (edit-start e)) (push (rope-octets rope :start at :end (edit-start e)) parts))
        (when (edit-insert e) (push (edit-insert e) parts))
        (setf at (edit-end e))))
    (when (< at end) (push (rope-octets rope :start at :end end) parts))
    (apply #'concatenate 'octets (nreverse parts))))

(defun widen-group (rope edits group)
  "Widen GROUP, a list (START END MEMBERS), in place until neither of its edges
joins the unfinished start of a sequence to octets that continue it, nor a
carriage return to a newline: over the unfinished octets before it and the
continuation octets after it, and over the carriage return before it or the
newline after it. True when it widened."
  (let ((widened nil) (total (rope-bytes rope)))
    (loop
      (destructuring-bind (start end members) group
        (let* ((octets (group-replacement rope edits members start end))
               (next-continues (if (plusp (length octets))
                                   (continuation-byte-p (aref octets 0))
                                   (continuation-at-p rope end)))
               (before (if next-continues (rope-pending-before rope start) 0)))
          (when (plusp before)
            (setf (first group) (- start before) widened t
                  octets (group-replacement rope edits members (first group) end)))
          (let ((after (and (plusp (length octets))
                            (plusp (octets-pending octets 0 (length octets)))
                            (continuation-at-p rope end))))
            (when after
              (setf (second group) (past-continuations rope end) widened t))
            (unless (or (plusp before) after)
              ;; a carriage return before the group and a newline it begins with, or
              ;; one it ends with and a newline after it: the pair kept in one piece
              (let* ((start (first group)) (end (second group))
                     (size (length octets))
                     (first-new (cond ((plusp size) (aref octets 0)) ((< end total) (rope-octet rope end))))
                     (last-new (cond ((plusp size) (aref octets (1- size))) ((plusp start) (rope-octet rope (1- start))))))
                (cond ((and (eql first-new 10) (plusp start) (= 13 (rope-octet rope (1- start))))
                       (setf (first group) (1- start) widened t))
                      ((and (eql last-new 13) (< end total) (= 10 (rope-octet rope end)))
                       (setf (second group) (1+ end) widened t))
                      (t (return widened)))))))))))

(defun joining-returns-p (rope edits)
  "True when EDITS (sorted) may bring a carriage return and a newline
together across an edge: one that inserts a newline first or a carriage
return last, or deletes alone, next to a carriage return before it or a
newline after it in ROPE, or next to another edit."
  (declare (type simple-vector edits) (optimize speed))
  (let ((n (length edits)) (total (rope-bytes rope)))
    (declare (type fixnum n total))
    ;; the octets around read only for an edit that could join them
    (flet ((return-before (k s)
             (declare (type fixnum k s))
             (and (plusp s)
                  (or (and (plusp k) (= s (edit-end (svref edits (1- k)))))
                      (= 13 (the fixnum (rope-octet rope (1- s)))))))
           (newline-after (k end)
             (declare (type fixnum k end))
             (and (< end total)
                  (or (and (< (1+ k) n) (= end (edit-start (svref edits (1+ k)))))
                      (= 10 (the fixnum (rope-octet rope end)))))))
      (dotimes (k n nil)
        (let* ((e (svref edits k)) (insert (edit-insert e)))
          (declare (type edit e))
          (when (if insert
                    (let ((size (length (the octets insert))))
                      (if (zerop size)
                          (and (return-before k (edit-start e)) (newline-after k (edit-end e)))
                          (or (and (= 10 (aref (the octets insert) 0)) (return-before k (edit-start e)))
                              (and (= 13 (aref (the octets insert) (1- size))) (newline-after k (edit-end e))))))
                    (and (return-before k (edit-start e)) (newline-after k (edit-end e))))
            (return t)))))))

(defun joining-inserts-p (edits)
  "True when two edits of EDITS that insert meet, the first's insert ending
with the unfinished start of a sequence and the second's beginning with a
continuation octet: one right after the other, or with only deletions between
them that take away all that stood between."
  (let ((last nil) (reach -1))
    (loop for e across edits
          thereis (and last (edit-insert e) (= reach (edit-start e))
                       (plusp (nth-value 1 (insert-shape (edit-insert last))))
                       (insert-shape (edit-insert e)))
          do (cond ((edit-insert e) (setf last e))
                   ((/= reach (edit-start e)) (setf last nil)))
             (setf reach (edit-end e)))))

(defun widen-edits (rope edits &optional (meeting t) (edges t))
  "EDITS (a sorted simple-vector) as they are; or, where octets they bring
together could form one character across their edges (possible only where ROPE
holds raw bytes, or where inserts of raw bytes meet), or a carriage return and
a newline (JOINING-RETURNS-P), edits grouped and widened so that each group's
octets are counted as the one run they become: edits within three octets of
each other go together, and a group grows over the unfinished octets before
it and the continuation octets after it, and over a carriage return before
it or a newline after it that would meet one it brings. Returns
the edits to apply and, when they differ, a vector giving for each of EDITS the
index of the one that holds it and where its own range starts within that one's.
MEETING false says no two edits that insert meet, and EDGES false that none
inserts nothing or octets beginning with a newline or ending with a carriage
return (CHECK-EDITS finds out both)."
  (let ((n (length edits)))
    (if (or (zerop n)
            (and (zerop (node-invalid (rope-root rope))) (not (and meeting (joining-inserts-p edits)))
                 (not (and edges (joining-returns-p rope edits)))))
        (values edits nil)
        (let ((groups '()) (done '()) (changed nil))
          (dotimes (k n)
            (let ((e (aref edits k)) (last (first groups)))
              (if (and last (<= (edit-start e) (+ (second last) 3)))
                  (setf (second last) (max (second last) (edit-end e))
                        (third last) (cons k (third last))
                        changed t)
                  (push (list (edit-start e) (edit-end e) (list k)) groups))))
          ;; each group's edits gathered newest first: in their order again
          (dolist (group groups) (setf (third group) (nreverse (third group))))
          (dolist (group (nreverse groups))
            (when (widen-group rope edits group) (setf changed t))
            (loop while (and done (<= (first group) (+ (second (first done)) 3)))
                  do (let ((previous (pop done)))
                       (setf group (list (first previous) (max (second previous) (second group))
                                         (append (third previous) (third group)))
                             changed t)
                       (widen-group rope edits group)))
            (push group done))
          (if (not changed)
              (values edits nil)
              (let* ((groups (nreverse done))
                     (widened (make-array (length groups)))
                     (owners (make-array n)))
                (loop for (start end members) in groups
                      for g from 0
                      do (let ((octets (group-replacement rope edits members start end)))
                           (dolist (k members)
                             (setf (aref owners k) (cons g (- (edit-start (aref edits k)) start))))
                           (setf (aref widened g) (make-edit start end (and (plusp (length octets)) octets)))))
                (values widened owners)))))))

(defun apply-edits (rope edits)
  "Apply EDITS to ROPE and return two values: the new rope, and a fixnum vector
with, for each edit, the byte offset in the new text just after its insertion
(or at its deletion point). EDITS is a sequence of EDIT objects sorted by START
and non-overlapping; positions are byte offsets in ROPE at character boundaries.
Each edit's DELETED slot is filled with the removed octets for undo. ROPE is
unchanged. Edited file-backed spans become in-memory leaves; the rest of the
file stays mapped. ROPE may be still uncounted, a file freshly opened: a span
whose counts are unknown keeps the octets on either side of its edits as
uncounted spans (REBUILD-UNCOUNTED-SPAN), and the new rope is uncounted too,
for INDEX-ROPE to count, given ROPE counted as donor. Complexity: O(number of
edits + touched leaves + log n)."
  (let* ((edits (coerce edits 'simple-vector))
         (n (length edits)))
    (multiple-value-bind (applied owners) (multiple-value-call #'widen-edits rope edits (check-edits rope edits))
      (let* ((m (length applied))
             (root (rope-root rope))
             (scratch (list nil))
             (nodes (cond ((zerop m) (vector root))
                          ((branch-p root) (rebuild-branch root 0 applied 0 m scratch))
                          (t (rebuild-leaf root 0 applied 0 m scratch))))
             (new-root (if (zerop (length nodes))
                           (make-leaf (make-octets 0))
                           (build-tree nodes)))
             (positions (make-array n :element-type 'fixnum))
             (delta 0))
        (declare (type fixnum m delta))
        ;; Collapse a root with a single child; finish the deleted-octets records.
        (loop while (and (branch-p new-root) (= 1 (length (branch-children new-root))))
              do (setf new-root (aref (branch-children new-root) 0)))
        (loop for e across applied
              do (let ((pieces (edit-deleted e)))
                   (when pieces
                     (setf (edit-deleted e)
                           (if (null (cdr pieces))
                               (car pieces)
                               (apply #'concatenate 'octets (nreverse pieces)))))))
        ;; widened edits hand each edit its own deleted octets
        (when owners
          (loop for k from 0 below n
                for e = (aref edits k)
                do (destructuring-bind (g . offset) (aref owners k)
                     (setf (edit-deleted e)
                           (and (> (edit-end e) (edit-start e))
                                (subseq (edit-deleted (aref applied g)) offset (+ offset (edit-delete-length e))))))))
        (loop for k of-type fixnum from 0 below n
              for e of-type edit = (aref edits k)
              do (setf (aref positions k) (+ (edit-start e) delta (edit-insert-length e)))
                 (incf delta (- (edit-insert-length e) (edit-delete-length e))))
        (values (%make-rope new-root) positions)))))

(defun inverse-edits (edits positions)
  "Return a simple-vector of edits that undo EDITS after they were applied,
given the POSITIONS vector APPLY-EDITS returned. The inverse edits are sorted and
apply to the new text. Complexity: O(number of edits)."
  (let* ((edits (coerce edits 'simple-vector))
         (out (make-array (length edits))))
    (loop for k from 0 below (length edits)
          for e = (aref edits k)
          for after = (aref positions k)
          do (setf (aref out k)
                   (make-edit (- after (edit-insert-length e)) after (edit-deleted e))))
    out))

(defun rope-insert (rope byte text)
  "Return a new rope with TEXT (a string or octet vector) inserted at byte offset
BYTE of ROPE, which must be a character boundary."
  (multiple-value-bind (new positions)
      (apply-edits rope (vector (make-edit byte byte (etypecase text
                                                       (string (string-octets text))
                                                       (octets text)))))
    (declare (ignore positions))
    new))

(defun rope-delete (rope start end)
  "Return a new rope with the octets from START below END of ROPE removed."
  (multiple-value-bind (new positions) (apply-edits rope (vector (make-edit start end)))
    (declare (ignore positions))
    new))
