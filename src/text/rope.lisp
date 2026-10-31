;;;; src/text/rope.lisp --- Persistent rope of UTF-8 leaves with cached counts.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The rope is a persistent B+tree.  Leaves are either in-memory octet vectors
;;;; (LEAF) of at most +MAX-LEAF+ octets, or ranges of at most +SPAN-SIZE+
;;;; octets of a memory-mapped file that never enters the Lisp heap (SPAN).
;;;; Branches hold up to +MAX-FANOUT+ children of equal height.  Every node caches its byte,
;;;; character, UTF-16 unit and line end counts; for spans of a freshly opened
;;;; file the counts are unknown (-1) until INDEX-ROPE has scanned the file, in
;;;; parallel, which is how a file of gigabytes opens at once.  Nodes are never
;;;; mutated after construction, so a snapshot is the root pointer and an edit
;;;; copies only the path to the leaves it touches.  A batch of sorted edits is
;;;; applied in one descent (APPLY-EDITS).  No two counted leaves part a carriage
;;;; return from the newline after it, so that each counts its own line ends.

(in-package #:tuatara.text)

;;; Tunables

(defconstant +max-leaf+ 32768
  "Maximum number of octets in an in-memory leaf. An indexed leaf never splits
a UTF-8 sequence. Every scan inside an in-memory leaf is bounded by this;
one inside a span of a file by +SPAN-SIZE+.")

(defconstant +target-leaf+ 4096
  "Size of the in-memory leaves built from fresh text, so that edits have
headroom before a leaf has to split.")

(defconstant +min-leaf+ 1024
  "Leaves smaller than this are merged with a neighbour when a batch rebuilds their parent.")

(defconstant +max-fanout+ 32
  "Maximum number of children of a branch.")

(defconstant +span-size+ 65536
  "Size of the file-backed spans a mapped file is cut into on open, and the
most octets a span holds. Every span is an object the garbage collector copies,
so that larger spans make a large file's rope fewer objects; an edited span
becomes in-memory leaves of +TARGET-LEAF+.")

;;; Nodes

(defstruct (node (:constructor nil) (:copier nil) (:predicate nil))
  "Summary slots shared by all nodes: the counts of the text below the node.
BYTES is always known; CHARS, UTF16, LINES and INVALID (raw bytes) are -1 while
unknown (unindexed spans)."
  (bytes 0 :type fixnum :read-only t)
  (chars -1 :type fixnum :read-only t)
  (utf16 -1 :type fixnum :read-only t)
  (lines -1 :type fixnum :read-only t)
  (invalid -1 :type fixnum :read-only t))

(declaim (inline node-indexed-p))
(defun node-indexed-p (node)
  "True when NODE's character, UTF-16 and line counts are known."
  (>= (node-chars node) 0))

(defstruct (leaf (:include node)
                 (:constructor %make-leaf (octets start bytes chars utf16 lines invalid))
                 (:copier nil))
  "An in-memory leaf: BYTES octets of the immutable vector OCTETS starting at START."
  (octets (make-octets 0) :type octets :read-only t)
  (start 0 :type fixnum :read-only t))

(defstruct (span (:include node)
                 (:constructor %make-span (mapping offset bytes chars utf16 lines invalid))
                 (:copier nil))
  "A file-backed leaf: BYTES octets of MAPPING starting at OFFSET."
  (mapping nil :type mapping :read-only t)
  (offset 0 :type fixnum :read-only t))

(defstruct (branch (:include node)
                   (:constructor %make-branch (children height bytes chars utf16 lines invalid))
                   (:copier nil))
  "A branch: an immutable vector of 1 to +MAX-FANOUT+ children, all of HEIGHT - 1."
  (children #() :type simple-vector :read-only t)
  (height 1 :type fixnum :read-only t))

(declaim (inline node-height))
(defun node-height (node)
  "Return the height of NODE: 0 for a leaf or span, 1 + child height for a branch."
  (if (branch-p node) (branch-height node) 0))

(defun make-leaf (octets &optional (start 0) (end (length octets)))
  "Return a leaf viewing OCTETS from START below END, without copying. Counts
are computed by one scan."
  (declare (type octets octets) (type fixnum start end))
  (multiple-value-bind (chars utf16 lines invalid) (summarise octets start end)
    (%make-leaf octets start (- end start) chars utf16 lines invalid)))

(defun make-branch (children)
  "Return a branch over CHILDREN, a simple-vector of nodes of equal height,
summing their counts; unknown counts in any child make the branch's unknown."
  (declare (type simple-vector children))
  (let ((bytes 0) (chars 0) (utf16 0) (lines 0) (invalid 0))
    (declare (type fixnum bytes chars utf16 lines invalid))
    (loop for c across children
          do (incf bytes (node-bytes c))
             (if (and (>= chars 0) (node-indexed-p c))
                 (progn (incf chars (node-chars c)) (incf utf16 (node-utf16 c)) (incf lines (node-lines c))
                        (incf invalid (node-invalid c)))
                 (setf chars -1 utf16 -1 lines -1 invalid -1)))
    (%make-branch children (1+ (node-height (aref children 0))) bytes chars utf16 lines invalid)))

;;; Piece access: the content of a leaf or span

(defun piece-copy (node start end out out-start)
  "Copy octets START below END of NODE's content (a leaf or span) into OUT at OUT-START."
  (declare (type fixnum start end out-start) (type octets out))
  (etypecase node
    (leaf (replace out (leaf-octets node) :start1 out-start
                                          :start2 (+ (leaf-start node) start)
                                          :end2 (+ (leaf-start node) end)))
    (span (let ((mapping (span-mapping node)))
            (check-mapping mapping)
            (sb-sys:with-pinned-objects (out)
              (sb-kernel:system-area-ub8-copy (mapping-sap mapping) (+ (span-offset node) start)
                                              (sb-sys:vector-sap out) out-start
                                              (- end start))))))
  out)

(defmacro with-piece-sap ((sap base) node &body body)
  "Run BODY with SAP pointing at memory holding NODE's content (a leaf's or a
span's) from offset BASE, nothing copied: a leaf's octets pinned meanwhile, a
span's mapping checked first."
  (let ((n (gensym "NODE")) (octets (gensym "OCTETS")) (mapping (gensym "MAPPING")))
    `(let ((,n ,node))
       (etypecase ,n
         (leaf (let ((,octets (leaf-octets ,n)))
                 (sb-sys:with-pinned-objects (,octets)
                   (let ((,sap (sb-sys:vector-sap ,octets)) (,base (leaf-start ,n)))
                     (declare (type sb-sys:system-area-pointer ,sap) (type fixnum ,base))
                     ,@body))))
         (span (let ((,mapping (span-mapping ,n)))
                 (check-mapping ,mapping)
                 (let ((,sap (mapping-sap ,mapping)) (,base (span-offset ,n)))
                   (declare (type sb-sys:system-area-pointer ,sap) (type fixnum ,base))
                   ,@body)))))))

(defun piece-units-octets (node count utf16)
  "How many octets of NODE's content its first COUNT characters take, or with
UTF16 its first COUNT UTF-16 units (a character of four octets two of them;
one cut in two counted out); all of it when it has fewer. Read in place,
words of eight ASCII octets passed at once."
  (declare (type fixnum count) (optimize speed (safety 1)))
  (let ((n (node-bytes node)))
    (declare (type fixnum n))
    (with-piece-sap (sap base) node
      (macrolet ((byte-at (p) `(sb-sys:sap-ref-8 sap ,p)))
        (let ((i base) (end (+ base n)) (remaining count))
          (declare (type fixnum i end remaining))
          (loop while (and (>= remaining 8) (<= (+ i 8) end)
                           (zerop (logand (sb-sys:sap-ref-64 sap i) +ascii-mask+)))
                do (decf remaining 8) (incf i 8))
          (loop while (and (> remaining 0) (< i end))
                do (let ((length (nth-value 1 (utf8-decode-form byte-at i end))))
                     (declare (type fixnum length))
                     (decf remaining (if (and utf16 (= length 4)) 2 1))
                     (when (< remaining 0) (return))
                     (incf i length)))
          (- i base))))))

(defun piece-line-octets (node lines)
  "How many octets of NODE's content come before the start of its LINES-th
line from 1, just after its LINES-th line end (LINE-END-OCTET-P); NIL when it
has fewer. Read in place, words of eight octets with fewer line ends than are
left passed at once."
  (declare (type fixnum lines) (optimize speed (safety 1)))
  (let ((n (node-bytes node)) (remaining lines))
    (declare (type fixnum n remaining))
    (with-piece-sap (sap base) node
      (let ((i base) (end (+ base n)) (returns (sap-holds-return-p sap base (+ base n))))
        (declare (type fixnum i end))
        (loop while (<= (+ i 8) end)
              do (let ((count (if returns
                                  (word-line-ends-form (sb-sys:sap-ref-64 sap i)
                                                       (and (< (+ i 8) end) (sb-sys:sap-ref-8 sap (+ i 8))))
                                  (logcount (word-octets-of (sb-sys:sap-ref-64 sap i) +newlines+)))))
                   (declare (type fixnum count))
                   (when (>= count remaining) (return))
                   (decf remaining count)
                   (incf i 8)))
        (loop while (< i end)
              do (let ((b (sb-sys:sap-ref-8 sap i)))
                   (when (and (or (= b 10) (= b 13))
                              (line-end-octet-p b (and (< (1+ i) end) (sb-sys:sap-ref-8 sap (1+ i)))))
                     (decf remaining)
                     (when (zerop remaining) (return (- (1+ i) base)))))
                 (incf i))))))

(defun piece-summarise (node start end)
  "SUMMARISE octets START below END of NODE's content: characters, UTF-16 units,
newlines and raw bytes."
  (declare (type fixnum start end))
  (etypecase node
    (leaf (summarise (leaf-octets node) (+ (leaf-start node) start) (+ (leaf-start node) end)))
    (span (let ((mapping (span-mapping node)))
            (check-mapping mapping)
            (summarise-sap (mapping-sap mapping) (+ (span-offset node) start) (+ (span-offset node) end))))))

(defun piece-newlines (node start end)
  "The line ends (LINE-END-OCTET-P) from START below END of NODE's content,
counted where they lie, eight octets at a time (NEWLINES-SAP), a carriage
return at END - 1 ending a line unless NODE's next octet is a newline; a node
counted already gives its count when the range is all of it."
  (declare (type fixnum start end))
  (if (and (zerop start) (= end (node-bytes node)) (node-indexed-p node))
      (node-lines node)
      (etypecase node
        (leaf (let ((octets (leaf-octets node)) (base (leaf-start node)))
                (sb-sys:with-pinned-objects (octets)
                  (newlines-sap (sb-sys:vector-sap octets) (+ base start) (+ base end) (+ base (node-bytes node))))))
        (span (let ((mapping (span-mapping node)) (base (span-offset node)))
                (check-mapping mapping)
                (newlines-sap (mapping-sap mapping) (+ base start) (+ base end) (+ base (node-bytes node))))))))

(defun piece-octet (node i)
  "Return octet I of NODE's content."
  (declare (type fixnum i))
  (etypecase node
    (leaf (aref (leaf-octets node) (+ (leaf-start node) i)))
    (span (let ((mapping (span-mapping node)))
            (check-mapping mapping)
            (sb-sys:sap-ref-8 (mapping-sap mapping) (+ (span-offset node) i))))))

;;; Building trees

(defun chunk-octets (octets start end)
  "Return a simple-vector of leaves viewing OCTETS from START below END (text
starting at a character start), each about +TARGET-LEAF+ octets and every one
starting where a character starts. Complexity: O(END - START)."
  (declare (type octets octets) (type fixnum start end))
  (let ((leaves (make-array 0 :adjustable t :fill-pointer 0)) (text-start start))
    (if (= start end)
        (vector-push-extend (make-leaf (make-octets 0)) leaves)
        (loop while (< start end)
              do (let ((cut (min end (+ start +target-leaf+))))
                   (declare (type fixnum cut))
                   (when (< cut end)
                     ;; at most four steps back: no character is longer than four octets,
                     ;; and a carriage return stays with the newline after it
                     (loop while (and (> cut start) (not (octets-cut-p octets cut text-start end)))
                           do (decf cut)))
                   (vector-push-extend (make-leaf octets start cut) leaves)
                   (setf start cut))))
    (coerce leaves 'simple-vector)))

(defun group-into-branches (nodes)
  "Group NODES, a simple-vector of equal-height nodes, into branches of at most
+MAX-FANOUT+ children with sizes as even as possible. Returns a simple-vector."
  (declare (type simple-vector nodes))
  (let* ((n (length nodes))
         (groups (ceiling n +max-fanout+))
         (base (floor n groups))
         (extra (mod n groups))
         (result (make-array groups))
         (pos 0))
    (dotimes (g groups result)
      (let ((size (if (< g extra) (1+ base) base)))
        (setf (aref result g) (make-branch (subseq nodes pos (+ pos size))))
        (incf pos size)))))

(defun build-tree (nodes)
  "Return a single node over NODES (a non-empty simple-vector of equal-height nodes)."
  (declare (type simple-vector nodes))
  (loop while (> (length nodes) 1)
        do (setf nodes (group-into-branches nodes)))
  (aref nodes 0))

;;; Ropes

(defstruct (rope (:constructor %make-rope (root)) (:copier nil))
  "An immutable text. ROOT is a leaf, a span or a branch. Every operation returns
a new rope sharing structure with the old one; the old one stays valid, which is
what makes snapshots free."
  (root (make-leaf (make-octets 0)) :type node :read-only t))

(setf (documentation 'rope-p 'function) "True when OBJECT is a ROPE.")

(declaim (inline rope-bytes rope-indexed-p))
(defun rope-bytes (rope) "Number of octets in ROPE. O(1)." (node-bytes (rope-root rope)))
(defun rope-indexed-p (rope)
  "True when ROPE's character, UTF-16 and line counts are known everywhere."
  (node-indexed-p (rope-root rope)))

(defun octets->rope (octets &key (start 0) (end (length octets)))
  "Return a rope viewing OCTETS from START below END, any octets at all,
which must not be modified afterwards. Complexity: O(END - START), one scan, no copy."
  (declare (type octets octets))
  (%make-rope (build-tree (chunk-octets octets start end))))

(defun string->rope (string)
  "Return a rope holding STRING, encoded as UTF-8 (STRING-OCTETS). Complexity: O(length of STRING)."
  (octets->rope (string-octets string)))

(defun make-rope (&optional (text ""))
  "Return a rope holding TEXT, a string or an octet vector of UTF-8; empty by default."
  (etypecase text
    (string (string->rope text))
    ((array (unsigned-byte 8) (*)) (octets->rope (coerce text 'octets)))))

(defun slice-piece (node start end)
  "A leaf or span over octets START below END of NODE's content, sharing its
storage; the counts of the part are computed for an indexed NODE. Complexity:
O(END - START)."
  (declare (type fixnum start end))
  (etypecase node
    (leaf (make-leaf (leaf-octets node) (+ (leaf-start node) start) (+ (leaf-start node) end)))
    (span (if (node-indexed-p node)
              (multiple-value-bind (chars utf16 lines invalid) (piece-summarise node start end)
                (%make-span (span-mapping node) (+ (span-offset node) start) (- end start) chars utf16 lines invalid))
              (%make-span (span-mapping node) (+ (span-offset node) start) (- end start) -1 -1 -1 -1)))))

(defun rope-pieces (rope start end)
  "The leaves and spans of ROPE holding octets START below END, in order, as a
list; the one or two the range cuts are replaced by slices (SLICE-PIECE).
START and END should be character boundaries. Complexity: O(log n + leaves)."
  (let ((out '()))
    (map-leaf-pieces (lambda (node s e)
                       (push (if (and (zerop s) (= e (node-bytes node))) node (slice-piece node s e)) out))
                     rope :start start :end end)
    (nreverse out)))

(defun rope-slice (rope start end)
  "A rope holding ROPE's text from byte START below END, sharing its leaves and
spans rather than copying them, so that a slice of many gigabytes is made in
the time it takes to list its spans. START and END are character boundaries.
Complexity: O(log n + leaves in the range)."
  (let ((nodes (clean-junctions (remove-if (lambda (n) (zerop (node-bytes n))) (rope-pieces rope start end)))))
    (%make-rope (if nodes (build-tree (coerce nodes 'simple-vector)) (make-leaf (make-octets 0))))))

(defun splice-rope (rope start end &rest middles)
  "A rope holding ROPE's text before START, then the text of each of MIDDLES
(ropes) in turn, then ROPE's text from END. Leaves and spans are shared, not
copied, so ropes over files of many gigabytes splice in the time it takes to
list their spans; only the two leaves the cuts fall in are sliced. START and
END are character boundaries of ROPE with START <= END."
  (let ((nodes (append (rope-pieces rope 0 start)
                       (loop for middle in middles
                             append (let ((acc '())) (map-leaves (lambda (n) (push n acc)) middle) (nreverse acc)))
                       (rope-pieces rope end (rope-bytes rope)))))
    (setf nodes (clean-junctions (remove-if (lambda (n) (zerop (node-bytes n))) nodes)))
    (%make-rope (if nodes (build-tree (coerce nodes 'simple-vector)) (make-leaf (make-octets 0))))))

(defun clean-junction-p (left right)
  "True unless the octets of LEFT, a leaf or span, end with the unfinished
start of a sequence that the first octet of RIGHT continues, or with a
carriage return that a newline beginning RIGHT follows."
  (or (zerop (node-bytes left)) (zerop (node-bytes right))
      (let ((first (piece-octet right 0)))
        (cond ((= first 10) (/= 13 (piece-octet left (1- (node-bytes left)))))
              ((not (continuation-byte-p first)))
              (t (let* ((k (min 3 (node-bytes left))) (tail (make-octets k)))
                   (piece-copy left (- (node-bytes left) k) (node-bytes left) tail 0)
                   (zerop (octets-pending tail 0 k))))))))

(defun clean-junctions (nodes)
  "NODES, a list of leaves and spans in order, with every pair whose junction
would split a character (possible only around raw bytes) or part a carriage
return from the newline after it remade as leaves cut where they may be
\(CHUNK-OCTETS)."
  (let ((out '()))
    (dolist (node nodes (nreverse out))
      (let ((left (first out)))
        (if (and left (not (clean-junction-p left node)))
            (let* ((a (node-bytes left)) (b (node-bytes node)) (octets (make-octets (+ a b))))
              (piece-copy left 0 a octets 0)
              (piece-copy node 0 b octets a)
              (pop out)
              (loop for leaf across (chunk-octets octets 0 (+ a b)) do (push leaf out)))
            (push node out))))))

(defun unindexed-spans (mapping start end)
  "Return a simple-vector of spans of +SPAN-SIZE+ covering MAPPING from START
below END, with unknown counts. Cuts may fall inside UTF-8 sequences; INDEX-ROPE
re-cuts them. Nothing is read from the file. Complexity: O(number of spans)."
  (declare (type fixnum start end))
  (let ((spans (make-array (ceiling (- end start) +span-size+))))
    (loop for i from 0 below (length spans)
          for s = (+ start (* i +span-size+))
          do (setf (aref spans i) (%make-span mapping s (min +span-size+ (- end s)) -1 -1 -1 -1)))
    spans))

(defun mapping-rope (mapping &optional (start 0) (end (mapping-length mapping)))
  "A rope over MAPPING's octets from START below END, all of them unless
given, with unknown character and line counts until INDEX-ROPE. Nothing is
read. Complexity: O(octets / +SPAN-SIZE+)."
  (%make-rope (build-tree (unindexed-spans mapping start end))))

(defun open-file-rope (path)
  "Return a rope over the file at PATH, memory-mapped, with unknown character
and line counts, and the mapping as a second value (NIL for an empty file).
Byte-based operations work immediately; character and line operations need
INDEX-ROPE first. Complexity: O(file size / +SPAN-SIZE+), nothing is read."
  (let ((mapping (map-file path)))
    (if mapping
        (values (%make-rope (build-tree (unindexed-spans mapping 0 (mapping-length mapping)))) mapping)
        (values (make-rope) nil))))

(defun check-indexed (rope)
  "Signal an error unless ROPE is indexed."
  (unless (rope-indexed-p rope)
    (error "The text is not indexed yet; character and line counts are unknown until INDEX-ROPE completes.")))

(defun rope-chars (rope)
  "Number of characters in ROPE. O(1). Signals an error while ROPE is unindexed."
  (check-indexed rope) (node-chars (rope-root rope)))
(defun rope-utf16 (rope)
  "Number of UTF-16 code units ROPE would occupy. O(1). Signals an error while unindexed."
  (check-indexed rope) (node-utf16 (rope-root rope)))
(defun rope-invalid (rope)
  "Number of raw bytes in ROPE: octets that are not part of well-formed UTF-8.
O(1). Signals an error while ROPE is unindexed."
  (check-indexed rope) (node-invalid (rope-root rope)))
(defun rope-lines (rope)
  "Number of line ends in ROPE (LINE-END-OCTET-P: newlines, and carriage
returns no newline follows); the number of lines is one more. O(1).
Signals an error while ROPE is unindexed."
  (check-indexed rope) (node-lines (rope-root rope)))

(defun map-leaves (function rope)
  "Call FUNCTION with each leaf or span of ROPE in order."
  (declare (type function function))
  (labels ((walk (node)
             (if (branch-p node)
                 (loop for c across (branch-children node) do (walk c))
                 (funcall function node))))
    (walk (rope-root rope))))

(defun rope-statistics (rope)
  "Return a plist describing ROPE's structure: :height, :leaves (in-memory),
:spans (file-backed), :unindexed-spans, :memory-bytes (octets held by in-memory
leaves), :file-bytes (octets served from mapped files). For diagnostics and tests."
  (let ((leaves 0) (spans 0) (unindexed 0) (memory 0) (file 0))
    (map-leaves (lambda (node)
                  (if (leaf-p node)
                      (progn (incf leaves) (incf memory (node-bytes node)))
                      (progn (incf spans) (incf file (node-bytes node))
                             (unless (node-indexed-p node) (incf unindexed)))))
                rope)
    (list :height (node-height (rope-root rope)) :leaves leaves :spans spans
          :unindexed-spans unindexed :memory-bytes memory :file-bytes file)))

;;; Indexing: computing the counts of a mapped file in parallel

(defun piece-boundary-at (sap offset end)
  "Return the first offset at or after OFFSET, above 0 (but not past END),
where the mapped text at SAP, which ends at END, may be cut into pieces
\(SAP-CUT-P): at most four octets on."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum offset end))
  (loop while (and (< offset end) (not (sap-cut-p sap offset 0 end)))
        do (incf offset))
  offset)

(defun index-range (mapping start end)
  "Cut MAPPING's octets from START below END into indexed spans of at most
+SPAN-SIZE+, each starting where a character starts and none parting a
carriage return from the newline after it, and return them as a list.
START and END must be character boundaries. Complexity: O(END - START)."
  (declare (type fixnum start end))
  (let ((sap (mapping-sap mapping)) (length (mapping-length mapping)) (spans '()))
    (loop while (< start end)
          do (let ((cut (min end (+ start +span-size+))))
               (declare (type fixnum cut))
               (when (< cut end)
                 (loop while (and (> cut start) (not (sap-cut-p sap cut 0 length)))
                       do (decf cut)))
               (multiple-value-bind (chars utf16 lines invalid) (summarise-sap sap start cut)
                 (push (%make-span mapping start (- cut start) chars utf16 lines invalid) spans))
               (setf start cut)))
    (nreverse spans)))

(defun donor-spans (donor)
  "The indexed spans of DONOR by mapping: a hash table from each mapping to a
simple-vector of its spans in DONOR, in order of their offsets."
  (let ((table (make-hash-table :test 'eq)))
    (map-leaves (lambda (node)
                  (when (and (span-p node) (node-indexed-p node))
                    (push node (gethash (span-mapping node) table))))
                donor)
    (maphash (lambda (mapping spans)
               (setf (gethash mapping table)
                     (sort (coerce spans 'simple-vector) #'< :key #'span-offset)))
             table)
    table))

(defun run-pieces (start end spans)
  "The pieces a run of a mapping's octets from START below END, both character
boundaries, is made of when SPANS (a donor's indexed spans of the mapping, by
offset) are to be reused: a list of spans taken whole and (START . END) ranges
to read, in order."
  (let ((pieces '()) (pos start)
        ;; the first span that ends after START
        (i (tuatara.runtime:bisect (mid 0 (length spans))
                                   (<= (+ (span-offset (aref spans mid)) (node-bytes (aref spans mid))) start))))
    (flet ((read-to (to) (when (< pos to) (push (cons pos to) pieces) (setf pos to))))
      (loop while (and (< i (length spans)) (< pos end))
            do (let* ((span (aref spans i))
                      (s (span-offset span))
                      (e (+ s (node-bytes span))))
                 (cond ((>= s end) (return))
                       ((and (= s pos) (<= e end)) (push span pieces) (setf pos e))
                       ;; a span cut by the run's start or end, or after a gap: read what the run has of it
                       (t (read-to (min e end))))
                 (incf i)))
      (read-to end))
    (nreverse pieces)))

;;; What is counted so far, while a text is being counted

(defstruct (tally (:constructor make-tally ()) (:copier nil))
  "What INDEX-ROPE has counted so far of a text, for the line numbers wanted
before all of it is counted: the text cut into segments, in order, each done
once its newlines are counted. STARTS and ENDS are where each segment begins
and ends in the text, LINES its newlines, -1 until counted; READY turns true
once INDEX-ROPE has filled them in. DONE, CUMULATIVE, COUNTED and REACHED are
kept by TALLY-ADVANCE on the one thread that reads the tally: how many
segments from the first are counted, the newlines before each of them, the
newlines in all of them, and where they end. LAST, on that thread too, is the
line TALLY-LINE-NUMBER found last, as (BYTE LINE), the next counted from it."
  (starts (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (ends (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (lines (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (ready nil)
  (done 0 :type fixnum)
  (cumulative (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (counted 0 :type fixnum)
  (reached 0 :type fixnum)
  (last nil))

(setf (documentation 'make-tally 'function) "Return a TALLY, for INDEX-ROPE to fill in."
      (documentation 'tally-p 'function) "True when OBJECT is a TALLY.")

(defun tally-fill (tally starts ends lines)
  "Give TALLY the segments INDEX-ROPE cut the text into (STARTS, ENDS and
LINES, fixnum vectors), before any of them is counted."
  (setf (tally-starts tally) (coerce starts '(simple-array fixnum (*)))
        (tally-ends tally) (coerce ends '(simple-array fixnum (*)))
        (tally-lines tally) (coerce lines '(simple-array fixnum (*)))
        (tally-cumulative tally) (make-array (length lines) :element-type 'fixnum :initial-element 0))
  (sb-thread:barrier (:write))
  (setf (tally-ready tally) t))

(defun tally-note (tally segment start end lines)
  "Tell TALLY that SEGMENT, from START below END of the text, holds LINES
newlines: its range first, its count after, so that a reader seeing the count
sees the range."
  (setf (aref (tally-starts tally) segment) start
        (aref (tally-ends tally) segment) end)
  (sb-thread:barrier (:write))
  (setf (aref (tally-lines tally) segment) lines))

(defun tally-advance (tally)
  "Take in the segments of TALLY counted since last time, from the first not
yet taken in, while they come in order: the text counted from its start grows."
  (when (tally-ready tally)
    (let ((lines (tally-lines tally)))
      (loop while (< (tally-done tally) (length lines))
            do (let ((n (aref lines (tally-done tally))))
                 (when (minusp n) (return))
                 (sb-thread:barrier (:read))
                 (setf (aref (tally-cumulative tally) (tally-done tally)) (tally-counted tally))
                 (incf (tally-counted tally) n)
                 (setf (tally-reached tally) (aref (tally-ends tally) (tally-done tally)))
                 (incf (tally-done tally)))))
    tally))

(defun tally-segment (tally key value)
  "The last of TALLY's counted segments whose KEY (TALLY-STARTS or
TALLY-CUMULATIVE) is at most VALUE, found by halving; NIL when none is."
  (let* ((vector (funcall key tally))
         (lo (tuatara.runtime:bisect (mid 0 (tally-done tally)) (<= (aref vector mid) value))))
    (and (plusp lo) (1- lo))))

(defun tally-line-number (tally rope byte)
  "The line, from 0, holding offset BYTE of ROPE, the text TALLY counts, when
the count has passed it from the start; NIL otherwise. Counts from the line
found last when that is nearer (the rows of a window, one after another, each
read only from the one before), else from the start of BYTE's segment: reads
at most one segment of the text."
  (tally-advance tally)
  (cond ((zerop byte) 0)
        ((> byte (tally-reached tally)) nil)
        (t (let ((i (tally-segment tally #'tally-starts byte)))
             (when i
               (let* ((start (aref (tally-starts tally) i))
                      (last (tally-last tally))
                      (line (if (and last (<= start (first last)) (< (abs (- byte (first last))) (- byte start)))
                                (if (<= (first last) byte)
                                    (+ (second last) (rope-count-newlines rope (first last) byte))
                                    (- (second last) (rope-count-newlines rope byte (first last))))
                                (+ (aref (tally-cumulative tally) i) (rope-count-newlines rope start byte)))))
                 (setf (tally-last tally) (list byte line))
                 line))))))

(defun tally-line-start (tally rope line)
  "The offset where line LINE, from 0, of ROPE, the text TALLY counts, starts,
when the count has passed it from the start; NIL otherwise. Reads at most one
segment of the text."
  (tally-advance tally)
  (cond ((zerop line) 0)
        ((> line (tally-counted tally)) nil)
        (t (let ((i (tally-segment tally #'tally-cumulative (1- line))))
             (and i (let ((newline (rope-nth-newline rope (aref (tally-starts tally) i) (aref (tally-ends tally) i)
                                                     (- line (aref (tally-cumulative tally) i)))))
                      (and newline (1+ newline))))))))

(defun tally-counted-lines (tally)
  "Two values: the newlines TALLY has counted from the text's start, and the
offset the count has reached."
  (tally-advance tally)
  (values (tally-counted tally) (tally-reached tally)))

(defun rope-count-newlines (rope start end)
  "The line ends (LINE-END-OCTET-P) of ROPE from byte START below END, a
carriage return at END - 1 ending a line unless a newline follows it in ROPE:
those of pieces counted already taken as they are, the rest counted where
they lie (PIECE-NEWLINES), nothing copied. Complexity: O(log n + the octets
of pieces not counted yet)."
  (declare (type fixnum start end))
  (let ((count 0) (last-return nil))
    (declare (type fixnum count))
    ;; a carriage return ending one piece and a newline beginning the next end one line
    (flet ((tally (node piece-start piece-end)
             (declare (type fixnum piece-start piece-end))
             (when (and last-return (= 10 (piece-octet node piece-start))) (decf count))
             (incf count (the fixnum (piece-newlines node piece-start piece-end)))
             (setf last-return (and (= piece-end (node-bytes node)) (= 13 (piece-octet node (1- piece-end)))))))
      (declare (dynamic-extent #'tally))
      (map-leaf-pieces #'tally rope :start start :end end))
    (when (and last-return (< end (rope-bytes rope)) (= 10 (rope-octet rope end)))
      (decf count))
    count))

(defun rope-nth-newline (rope start end n)
  "The offset of the Nth line end (from 1; LINE-END-OCTET-P) of ROPE from byte
START below END, found by reading 64 KB at a time; NIL when there are fewer."
  (declare (type fixnum start end n))
  (let* ((total (rope-bytes rope))
         (block (make-octets (1+ (min 65536 (max 0 (- end start))))))
         (pos start) (left n))
    (declare (type fixnum total pos left))
    (loop while (< pos end)
          do (let* ((stop (min end (+ pos 65536)))
                    (have (min total (1+ stop)))
                    (avail (- have pos)))
               (declare (type fixnum stop have avail))
               (rope-copy-into rope pos have block)
               (loop for i of-type fixnum from 0 below (- stop pos)
                     do (let ((b (aref block i)))
                          (when (and (or (= b 10) (= b 13)) (octets-line-end-p block i avail))
                            (decf left)
                            (when (zerop left) (return-from rope-nth-newline (+ pos i))))))
               (setf pos stop)))
    nil))

(defun index-rope (rope &key (threads (tuatara.runtime:cpu-count)) (chunk (* 2 1024 1024)) donor progress cancel tally)
  "Return ROPE with every unindexed span replaced by indexed spans cut at
character boundaries. Runs of unindexed spans are scanned in parallel on up to
THREADS threads in pieces of about CHUNK octets, taken in the order of the
text. In-memory leaves and already indexed spans are kept as they are. DONOR,
an indexed rope over the same mappings (as it was before edits made ROPE),
gives its spans where a run covers them whole, so that only the pieces at the
ends of runs are read. PROGRESS, a cons, has its cdr set to the octets to read
and its car to those read so far, as the work goes. TALLY, a TALLY, is told
each piece's newlines as it is counted, for line numbers wanted before all of
the text is counted (TALLY-LINE-NUMBER, TALLY-LINE-START). When CANCEL's car
turns true the reading stops and NIL is returned. Returns ROPE itself when
nothing is unindexed.
Complexity: O(unindexed octets / THREADS + number of leaves); with a donor,
O(leaves + the octets of the pieces read)."
  (if (rope-indexed-p rope)
      rope
      (let ((items '())
            (donated (and donor (donor-spans donor))))
        ;; Collect leaves in order, coalescing runs of contiguous unindexed spans
        ;; of the same mapping into (mapping start end) entries.
        (map-leaves (lambda (node)
                      (if (and (span-p node) (not (node-indexed-p node)))
                          (let ((run (first items)))
                            (if (and (consp run) (eq (first run) (span-mapping node))
                                     (= (third run) (span-offset node)))
                                (setf (third run) (+ (span-offset node) (node-bytes node)))
                                (push (list (span-mapping node) (span-offset node)
                                            (+ (span-offset node) (node-bytes node)))
                                      items)))
                          (push node items)))
                    rope)
        (setf items (nreverse items))
        ;; Each run becomes spans taken from the donor and ranges to read; the
        ;; ranges are cut into tasks of about CHUNK octets, interior cut points
        ;; moved to character boundaries so that every task and both of its
        ;; neighbours agree on where the boundary is.
        (let ((tasks (make-array 0 :adjustable t :fill-pointer 0))
              (placement (make-array 0 :adjustable t :fill-pointer 0))
              (sizes (make-array 0 :adjustable t :fill-pointer 0))
              (to-read 0)
              ;; the text cut into segments, in order, for TALLY: a node counted
              ;; already, or a piece to read
              (segment-starts (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
              (segment-ends (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
              (segment-lines (make-array 0 :element-type 'fixnum :adjustable t :fill-pointer 0))
              (at 0))
          (declare (type fixnum at))
          (setf items
                (loop for item in items
                      append (if (and (consp item) donated (gethash (first item) donated))
                                 (destructuring-bind (mapping start end) item
                                   (loop for piece in (run-pieces start end (gethash mapping donated))
                                         collect (if (consp piece) (list mapping (car piece) (cdr piece)) piece)))
                                 (list item))))
          (loop for item in items
                for index from 0
                do (if (consp item)
                       (destructuring-bind (mapping start end) item
                         (check-mapping mapping)
                         (let ((sap (mapping-sap mapping)) (length (mapping-length mapping)) (pos start)
                               (base (- at start)))
                           (loop while (< pos end)
                                 do (let* ((a pos) (b (min end (+ pos chunk)))
                                           (segment (fill-pointer segment-starts)))
                                      (vector-push-extend (+ a base) segment-starts)
                                      (vector-push-extend (+ b base) segment-ends)
                                      (vector-push-extend -1 segment-lines)
                                      ;; each task moves its own cuts to where characters start, reading
                                      ;; the octets there itself, so that nothing is read before the
                                      ;; tasks run in parallel; both neighbours of a cut agree on it
                                      (vector-push-extend (lambda ()
                                                            (if (and cancel (car cancel))
                                                                :cancelled
                                                                (let* ((from (if (= a start) a (min end (piece-boundary-at sap a length))))
                                                                       (to (if (= b end) b (min end (piece-boundary-at sap b length))))
                                                                       (spans (index-range mapping from to)))
                                                                  (when tally
                                                                    (tally-note tally segment (+ from base) (+ to base)
                                                                                (loop for span in spans sum (node-lines span))))
                                                                  (when progress (sb-ext:atomic-incf (car progress) (- b a)))
                                                                  spans)))
                                                          tasks)
                                      (vector-push-extend index placement)
                                      (vector-push-extend (- b a) sizes)
                                      (incf to-read (- b a))
                                      (setf pos b)))
                           (incf at (- end start))))
                       (progn
                         (vector-push-extend at segment-starts)
                         (vector-push-extend (+ at (node-bytes item)) segment-ends)
                         (vector-push-extend (node-lines item) segment-lines)
                         (incf at (node-bytes item)))))
          (when tally
            (tally-fill tally segment-starts segment-ends segment-lines))
          (when progress
            (setf (car progress) 0 (cdr progress) to-read))
          (let* ((results (tuatara.runtime:run-tasks (coerce tasks 'simple-vector) :parallelism threads))
                 (leaves (make-array 0 :adjustable t :fill-pointer 0))
                 (r 0))
            (when (or (and cancel (car cancel)) (find :cancelled results))
              (return-from index-rope nil))
            (loop for item in items
                  for index from 0
                  do (if (consp item)
                         (loop while (and (< r (length placement)) (= (aref placement r) index))
                               do (dolist (span (aref results r)) (vector-push-extend span leaves))
                                  (incf r))
                         (vector-push-extend item leaves)))
            (%make-rope (if (zerop (length leaves))
                            (make-leaf (make-octets 0))
                            (build-tree (coerce leaves 'simple-vector)))))))))

(defun rope-count-chars (rope start end)
  "The characters of ROPE from byte START below END, both character
boundaries, counted by reading them (a raw byte is one, as the index counts),
for a rope not indexed yet: 64 KB at a time. Complexity: O(END - START)."
  (declare (type fixnum start end))
  (let ((count 0) (block (make-octets (+ (min 65536 (max 0 (- end start))) 4))) (pos start) (total (rope-bytes rope)))
    (declare (type fixnum count pos total))
    (loop while (< pos end)
          do (let* ((stop (min end (+ pos 65536)))
                    (have (min total (+ stop 4))))
               (rope-copy-into rope pos have block)
               (let ((cut (- stop pos)) (avail (- have pos)))
                 (declare (type fixnum cut avail))
                 (when (< stop end)
                   ;; end the block where a character starts, so that none is cut in two
                   (loop while (and (> cut 0) (not (octets-char-start-p block cut 0 avail)))
                         do (decf cut))
                   (when (zerop cut) (setf cut (- stop pos))))
                 (incf count (the fixnum (nth-value 0 (summarise block 0 cut))))
                 (incf pos cut))))
    count))

;;; Locating leaves

(defmacro define-locator (name measure doc)
  "Define NAME (ROPE VALUE) returning six values: the leaf whose MEASURE range
contains VALUE (the last leaf when VALUE equals the total), the leaf's starting
byte, character, UTF-16 and line counts, and the leaf's starting MEASURE. MEASURE
is one of the symbols BYTES, CHARS, UTF16 or LINES; DOC is the docstring."
  (let ((accessor (symbolicate 'node- measure)))
    `(defun ,name (rope value)
       ,doc
       (declare (type fixnum value))
       (let ((node (rope-root rope)) (bytes 0) (chars 0) (utf16 0) (lines 0))
         (declare (type fixnum bytes chars utf16 lines))
         (loop while (branch-p node)
               do (let* ((children (branch-children node)) (last (1- (length children))))
                    (loop for i fixnum from 0 to last
                          for child = (aref children i)
                          do (when (or (= i last)
                                       (< value (+ ,measure (,accessor child))))
                               (setf node child)
                               (return))
                             (incf bytes (node-bytes child)) (incf chars (node-chars child))
                             (incf utf16 (node-utf16 child)) (incf lines (node-lines child)))))
         (values node bytes chars utf16 lines ,measure)))))

(define-locator locate-byte bytes "Locate the leaf containing byte offset VALUE of ROPE.")
(define-locator locate-char chars "Locate the leaf containing character offset VALUE of ROPE (ROPE must be indexed).")
(define-locator locate-utf16 utf16 "Locate the leaf containing UTF-16 offset VALUE of ROPE (ROPE must be indexed).")

(defun locate-newline (rope n)
  "Locate the leaf containing the Nth line end of ROPE (1-based; LINE-END-OCTET-P). Returns the leaf
and its starting byte and line counts. N must be between 1 and (rope-lines ROPE)."
  (declare (type fixnum n))
  (let ((node (rope-root rope)) (bytes 0) (lines 0))
    (declare (type fixnum bytes lines))
    (loop while (branch-p node)
          do (let* ((children (branch-children node)) (last (1- (length children))))
               (loop for i fixnum from 0 to last
                     for child = (aref children i)
                     do (when (or (= i last) (<= n (+ lines (node-lines child))))
                          (setf node child)
                          (return))
                        (incf bytes (node-bytes child)) (incf lines (node-lines child)))))
    (values node bytes lines)))

;;; Index conversions

(defun check-byte-offset (rope byte)
  "Signal an error unless BYTE is a byte offset within ROPE (0 to its length inclusive)."
  (unless (<= 0 byte (rope-bytes rope))
    (error "Byte offset ~a is outside the text (0..~a)." byte (rope-bytes rope))))

(defun byte->char (rope byte)
  "Return the character offset of byte offset BYTE in ROPE. BYTE must be a
character boundary. Complexity: O(log n + leaf size)."
  (declare (type fixnum byte))
  (check-indexed rope) (check-byte-offset rope byte)
  (multiple-value-bind (leaf lbytes lchars) (locate-byte rope byte)
    (multiple-value-bind (chars) (piece-summarise leaf 0 (- byte lbytes))
      (+ lchars chars))))

(defun char->byte (rope char)
  "Return the byte offset of character offset CHAR in ROPE (0 to rope-chars inclusive).
Complexity: O(log n + leaf size)."
  (declare (type fixnum char))
  (check-indexed rope)
  (unless (<= 0 char (rope-chars rope))
    (error "Character offset ~a is outside the text (0..~a)." char (rope-chars rope)))
  (multiple-value-bind (leaf lbytes lchars) (locate-char rope char)
    (+ lbytes (piece-units-octets leaf (- char lchars) nil))))

(defun byte->utf16 (rope byte)
  "Return the UTF-16 offset of byte offset BYTE in ROPE. BYTE must be a character boundary."
  (declare (type fixnum byte))
  (check-indexed rope) (check-byte-offset rope byte)
  (multiple-value-bind (leaf lbytes lchars lutf16) (locate-byte rope byte)
    (declare (ignore lchars))
    (multiple-value-bind (chars utf16) (piece-summarise leaf 0 (- byte lbytes))
      (declare (ignore chars))
      (+ lutf16 utf16))))

(defun utf16->byte (rope unit)
  "Return the byte offset of UTF-16 offset UNIT in ROPE. An offset inside a
surrogate pair maps to the start of that character."
  (declare (type fixnum unit))
  (check-indexed rope)
  (unless (<= 0 unit (rope-utf16 rope))
    (error "UTF-16 offset ~a is outside the text (0..~a)." unit (rope-utf16 rope)))
  (multiple-value-bind (leaf lbytes lchars lutf16) (locate-utf16 rope unit)
    (declare (ignore lchars))
    (+ lbytes (piece-units-octets leaf (- unit lutf16) t))))

(defun byte->line (rope byte)
  "Return the 0-based line number containing byte offset BYTE in ROPE."
  (declare (type fixnum byte))
  (check-indexed rope) (check-byte-offset rope byte)
  (multiple-value-bind (leaf lbytes lchars lutf16 llines) (locate-byte rope byte)
    (declare (ignore lchars lutf16))
    (+ llines (piece-newlines leaf 0 (- byte lbytes)))))

(defun line->byte (rope line)
  "Return the byte offset where 0-based LINE starts in ROPE. LINE may be at most
the number of line ends, in which case the last line's start is returned."
  (declare (type fixnum line))
  (check-indexed rope)
  (unless (<= 0 line (rope-lines rope))
    (error "Line ~a is outside the text (0..~a)." line (rope-lines rope)))
  (if (zerop line)
      0
      (multiple-value-bind (leaf lbytes llines) (locate-newline rope line)
        (+ lbytes (or (piece-line-octets leaf (- line llines))
                      (error "Newline ~a not found in its leaf; the rope is corrupt." line))))))

(defmacro with-rope-window ((window base end) (rope from to size) &body body)
  "Run BODY with WINDOW holding ROPE's octets from FROM below TO, both clipped
to the text, from its index 0, BASE bound to the offset they start at and END
to how many there are: a few octets around a place, to decode there. SIZE, a
constant, is the most TO - FROM can be. WINDOW is on the stack, so that
nothing is made; BODY must not keep it."
  (let ((r (gensym "ROPE")) (to-var (gensym "TO")))
    `(let* ((,r ,rope)
            (,base (max 0 ,from))
            (,to-var (max ,base (min (rope-bytes ,r) ,to)))
            (,end (- ,to-var ,base))
            (,window (make-array ,size :element-type '(unsigned-byte 8))))
       (declare (type fixnum ,base ,to-var ,end) (dynamic-extent ,window) (ignorable ,base))
       (rope-copy-into ,r ,base ,to-var ,window)
       ,@body)))

(defun char-boundary-p (rope byte)
  "True when byte offset BYTE of ROPE starts a character or is the end of the text.
Works on unindexed ropes too."
  (declare (type fixnum byte))
  (check-byte-offset rope byte)
  (or (= byte (rope-bytes rope))
      (with-rope-window (window base end) (rope (- byte 3) (+ byte 4) 7)
        (octets-char-start-p window (- byte base) 0 end))))

;;; Reading

(defun map-leaf-pieces (function rope &key (start 0) (end (rope-bytes rope)))
  "Call FUNCTION with (NODE PIECE-START PIECE-END) for each leaf or span of ROPE
that overlaps byte range START below END, clipped to the range, in order. Read
the content with PIECE-COPY, PIECE-OCTETS or PIECE-OCTET.
Complexity: O(log n + number of leaves in the range)."
  (declare (type fixnum start end) (type function function))
  (check-byte-offset rope start) (check-byte-offset rope end)
  (when (< start end)
    (labels ((walk (node node-start)
               (declare (type fixnum node-start))
               (if (branch-p node)
                   (let ((pos node-start))
                     (declare (type fixnum pos))
                     (loop for child across (branch-children node)
                           do (let ((child-end (+ pos (node-bytes child))))
                                (when (and (< pos end) (> child-end start))
                                  (walk child pos))
                                (setf pos child-end))))
                   (let ((s (max 0 (- start node-start)))
                         (e (min (node-bytes node) (- end node-start))))
                     (when (< s e) (funcall function node s e))))))
      (walk (rope-root rope) 0))))

(defun map-rope-sources (function rope)
  "Call FUNCTION for each stretch of ROPE's octets in order: (:MEMORY OCTETS START
END) for octets held in memory, (:FILE MAPPING OFFSET LENGTH) for octets of a
mapped file, neighbouring spans of one mapping joined into one stretch. Saving
uses it to let the kernel copy what comes from files."
  (declare (type function function))
  (let ((pending nil))
    (flet ((flush () (when pending (apply function :file pending) (setf pending nil))))
      (map-leaf-pieces (lambda (node s e)
                         (declare (type fixnum s e))
                         (etypecase node
                           (leaf (flush)
                                 (funcall function :memory (leaf-octets node) (+ (leaf-start node) s) (+ (leaf-start node) e)))
                           (span (let ((mapping (span-mapping node)) (offset (+ (span-offset node) s)))
                                   (if (and pending (eq (first pending) mapping)
                                            (= (+ (second pending) (third pending)) offset))
                                       (incf (third pending) (- e s))
                                       (progn (flush) (setf pending (list mapping offset (- e s)))))))))
                       rope)
      (flush))))

(defun rope-copy-into (rope start end out &optional (out-start 0))
  "Copy ROPE's octets from byte START below END into OUT at OUT-START, which must
have room; return the number of octets copied. Allocates nothing."
  (declare (type fixnum start end out-start) (type octets out))
  (let ((pos out-start))
    (declare (type fixnum pos))
    ;; on the stack, as is POS
    (flet ((copy (node s e)
             (declare (type fixnum s e))
             (piece-copy node s e out pos)
             (incf pos (- e s))))
      (declare (dynamic-extent #'copy))
      (map-leaf-pieces #'copy rope :start start :end end))
    (- pos out-start)))

(defun rope-copy-to-sap (rope start end sap)
  "Copy ROPE's octets from byte START below END to foreign memory at SAP, which
must have room; return the number of octets copied. Allocates nothing."
  (declare (type fixnum start end) (type sb-sys:system-area-pointer sap))
  (let ((pos 0))
    (declare (type fixnum pos))
    ;; on the stack, as is POS
    (flet ((copy (node s e)
             (declare (type fixnum s e))
             (with-piece-sap (from base) node
               (sb-kernel:system-area-ub8-copy from (+ base s) sap pos (- e s)))
             (incf pos (- e s))))
      (declare (dynamic-extent #'copy))
      (map-leaf-pieces #'copy rope :start start :end end))
    pos))

(defun rope-octets (rope &key (start 0) (end (rope-bytes rope)))
  "Return a fresh octet vector holding ROPE's text from byte START below END."
  (declare (type fixnum start end))
  (let ((out (make-octets (max 0 (- end start)))))
    (rope-copy-into rope start end out)
    out))

(defun rope-string (rope &key (start 0) (end (rope-bytes rope)))
  "Return ROPE's text from byte START below END as a string, raw bytes as their
characters (U+DC80..U+DCFF). Both offsets must be character boundaries."
  (unless (and (char-boundary-p rope start) (char-boundary-p rope end))
    (error "Byte range ~a..~a does not fall on character boundaries." start end))
  (octets-string (rope-octets rope :start start :end end)))

(defun rope-octet (rope byte)
  "Return the octet at byte offset BYTE of ROPE. Complexity: O(log n)."
  (declare (type fixnum byte))
  (unless (< -1 byte (rope-bytes rope))
    (error "Byte offset ~a is outside the text (0..~a)." byte (1- (rope-bytes rope))))
  (multiple-value-bind (leaf lbytes) (locate-byte rope byte)
    (piece-octet leaf (- byte lbytes))))

;;; Characters and scanning

;;; Grapheme clusters: what a reader takes for one character, a letter with
;;; its accents, an emoji with its skin tone or a family of emoji joined by
;;; U+200D, a Hangul syllable written as jamo, a flag of two regional
;;; indicators. Unicode's rules (UAX #29), but for the conjuncts of Indic
;;; scripts, and with a pictographic symbol standing for an extended
;;; pictographic one.

(defconstant +cluster-reach+ 256
  "The most octets a grapheme cluster is taken to have: a longer run of marks
is cut into clusters of that many, so that stepping over one costs no more.")

(declaim (inline regional-indicator-code-p))
(defun regional-indicator-code-p (code)
  "True when CODE is a regional indicator's, one of the letters two of which make a flag."
  (<= #x1F1E6 code #x1F1FF))

(defun cluster-joins-p (before after regional)
  "True when the characters of codes BEFORE and AFTER, the one following the
other, belong to one grapheme cluster: AFTER a mark, a joiner or a skin tone,
BEFORE a prefixed sign, jamo making one Hangul syllable, an emoji after a
joiner (U+200D), or the second regional indicator of a flag, REGIONAL being
how many run up to BEFORE, BEFORE among them. A control character, a line's
end and a raw byte join nothing. Complexity: O(1)."
  (declare (type fixnum before after regional))
  (if (and (< before #x300) (< after #x300))
      nil
      (let ((b (sb-unicode:grapheme-break-class (code-char before)))
            (a (sb-unicode:grapheme-break-class (code-char after))))
        (cond ((or (member b '(:control :cr :lf)) (member a '(:control :cr :lf))) nil)
              ((member a '(:extend :zwj :spacing-mark)) t)
              ((eq b :prepend) t)
              ((eq b :l) (and (member a '(:l :v :lv :lvt)) t))
              ((member b '(:lv :v)) (and (member a '(:v :t)) t))
              ((member b '(:lvt :t)) (eq a :t))
              ((eq b :zwj) (eq (sb-unicode:general-category (code-char after)) :so))
              ((eq b :regional-indicator) (and (eq a :regional-indicator) (oddp regional)))
              (t nil)))))

(defun octets-cluster-end (octets i end)
  "The index after the grapheme cluster starting at index I of OCTETS, whose
text ends at END (CLUSTER-JOINS-P): no more than +CLUSTER-REACH+ octets
on, but for the last character's own."
  (declare (type octets octets) (type fixnum i end))
  (multiple-value-bind (before length) (octets-decode octets i end)
    (let ((p (+ i length))
          (limit (min end (+ i +cluster-reach+)))
          (regional (if (regional-indicator-code-p before) 1 0)))
      (declare (type fixnum p limit regional before))
      (loop while (< p limit)
            do (multiple-value-bind (after length) (octets-decode octets p end)
                 (unless (cluster-joins-p before after regional) (return))
                 (setf regional (if (regional-indicator-code-p after) (1+ regional) 0)
                       before after)
                 (incf p length)))
      p)))

(defun string-cluster-end (string start &optional (end (length string)))
  "The index after the grapheme cluster of STRING starting at index START, no
further than END (CLUSTER-JOINS-P): past a letter's accents, an emoji's skin
tone, a flag's second half; no more than +CLUSTER-REACH+ characters on."
  (declare (type string string) (type fixnum start end))
  (if (>= start end)
      end
      (let* ((before (char-code (char string start)))
             (regional (if (regional-indicator-code-p before) 1 0))
             (limit (min end (+ start +cluster-reach+)))
             (p (1+ start)))
        (declare (type fixnum before regional limit p))
        (loop while (< p limit)
              do (let ((after (char-code (char string p))))
                   (unless (cluster-joins-p before after regional) (return))
                   (setf regional (if (regional-indicator-code-p after) (1+ regional) 0)
                         before after)
                   (incf p)))
        p)))

(defun string-cluster-start (string end)
  "The index where the grapheme cluster of STRING before index END begins, or
the one holding the character before END (STRING-CLUSTER-END); 0 at most."
  (declare (type string string) (type fixnum end))
  (if (<= end 0)
      0
      (let ((q (1- end)))
        ;; back while characters join, a flag's halves however many run; then on by
        ;; whole clusters from there, which pairs regional indicators as they come
        (loop while (and (plusp q) (> q (- end +cluster-reach+))
                         (cluster-joins-p (char-code (char string (1- q))) (char-code (char string q)) 1))
              do (decf q))
        (loop for p = q then next
              for next = (string-cluster-end string p (length string))
              when (>= next end) return p))))

(defun rope-next-boundary (rope byte &optional clusters)
  "Return the byte offset of the character after the one starting at BYTE, or
the text length when BYTE is at or past the last character. With CLUSTERS,
of the grapheme cluster after the one starting at BYTE: past a letter's
accents, an emoji's skin tone, a flag's second half (CLUSTER-JOINS-P).
Complexity: O(log n)."
  (declare (type fixnum byte))
  (let ((total (rope-bytes rope)))
    (cond ((>= byte total) total)
          (clusters
           (with-rope-window (window base end) (rope byte (+ byte +cluster-reach+ 4) (+ +cluster-reach+ 4))
             (+ byte (octets-cluster-end window 0 end))))
          (t
           (with-rope-window (window base end) (rope byte (+ byte 4) 4)
             (+ byte (nth-value 1 (octets-decode window 0 end))))))))

(defun rope-previous-boundary (rope byte &optional clusters)
  "Return the byte offset of the character before byte offset BYTE, or 0 when
BYTE is 0. With CLUSTERS, the start of the grapheme cluster before BYTE, or
holding the character before it (CLUSTER-JOINS-P). Complexity: O(log n)."
  (declare (type fixnum byte))
  (cond ((<= byte 0) 0)
        (clusters
         (with-rope-window (window base end) (rope (- byte +cluster-reach+ 3) (+ byte 4) (+ +cluster-reach+ 7))
           (let* ((at (- byte base))
                  ;; the first octets of a window cut from the text may be taken for characters' starts
                  (low (if (zerop base) 0 3)))
             (flet ((start-before (i)
                      (loop for p from (1- i) downto low
                            when (octets-char-start-p window p 0 end) return p
                            finally (return low))))
               ;; back while characters join, a flag's halves however many run; then on by
               ;; whole clusters from there, which pairs regional indicators as they come
               (let* ((q (start-before at))
                      (after (octets-decode window q end)))
                 (loop while (> q low)
                       do (let* ((r (start-before q))
                                 (code (octets-decode window r end)))
                            (unless (cluster-joins-p code after 1) (return))
                            (setf q r after code)))
                 (loop for p = q then next
                       for next = (octets-cluster-end window p end)
                       when (>= next at) return (+ base p)))))))
        (t
         (with-rope-window (window base end) (rope (- byte 7) (+ byte 4) 11)
           (loop for p from (1- byte) downto (max 0 (- byte 4))
                 when (octets-char-start-p window (- p base) 0 end) return p
                 finally (return (max 0 (- byte 4))))))))

(defun rope-char-start (rope byte)
  "The start of the character of ROPE holding byte offset BYTE: BYTE itself
when a character starts there."
  (if (char-boundary-p rope byte) byte (rope-previous-boundary rope byte)))

(defun rope-char-at (rope byte)
  "Return the character starting at byte offset BYTE of ROPE, or NIL at the end
of the text: a raw byte's character (U+DC80..U+DCFF) for an octet that starts no
well-formed sequence. Decodes at most four octets. Complexity: O(log n)."
  (declare (type fixnum byte))
  (when (< byte (rope-bytes rope))
    (with-rope-window (window base end) (rope byte (+ byte 4) 4)
      (code-char (octets-decode window 0 end)))))

(defun rope-crlf-p (rope byte)
  "True when ROPE holds a carriage return at BYTE and a newline after it (CRLF-OCTETS-P)."
  (declare (type fixnum byte))
  (and (>= byte 0) (< (1+ byte) (rope-bytes rope))
       (let ((pair (make-array 2 :element-type '(unsigned-byte 8))))
         (declare (dynamic-extent pair))
         (rope-copy-into rope byte (+ byte 2) pair)
         (crlf-octets-p pair 0 2))))

(defun rope-line-start-p (rope byte)
  "True when a line of ROPE starts at BYTE: at the start of the text, or after
an octet that ends a line (LINE-END-OCTET-P). Complexity: O(log n)."
  (declare (type fixnum byte))
  (or (<= byte 0)
      (with-rope-window (window base end) (rope (1- byte) (1+ byte) 2)
        (octets-line-end-p window 0 end))))

(defun rope-line-text-ends-p (rope byte crlf)
  "True when the text of a line of ROPE ends at BYTE: at the end of the text,
or before an octet that ends a line or, with CRLF, before a carriage return
and a newline (LINE-TEXT-ENDS-P)."
  (declare (type fixnum byte))
  (or (>= byte (rope-bytes rope))
      (with-rope-window (window base end) (rope byte (+ byte 2) 2)
        (line-text-ends-p window 0 end crlf))))

(defun rope-line-text-end (rope start end crlf)
  "Where the text of a line of ROPE ends, the line ending at octet END (the
octet that ends it, LINE-END-OCTET-P, or the end of the text) and starting at
START or before: with CRLF, at the carriage return just before END when END
is a newline and the carriage return is after START; else at END. As
LINE-TEXT-END has it for octets."
  (declare (type fixnum start end))
  (if (and crlf (> end start) (< end (rope-bytes rope))
           (= 10 (rope-octet rope end)) (= 13 (rope-octet rope (1- end))))
      (1- end)
      end))

(defun rope-find-octet (rope value &key (start 0) (end (rope-bytes rope)))
  "Return the byte offset of the first octet equal to VALUE in ROPE from START
below END, or NIL. Complexity: O(log n + distance scanned)."
  (declare (type (unsigned-byte 8) value) (type fixnum start end))
  (let ((pos start))
    (declare (type fixnum pos))
    (block scan
      ;; on the stack, as is POS: a search for the next newline makes nothing
      (flet ((look (node s e)
               (declare (type fixnum s e))
               (let ((hit (etypecase node
                            (leaf (let ((octets (leaf-octets node)) (base (leaf-start node)))
                                    (declare (type octets octets) (type fixnum base))
                                    (loop for i of-type fixnum from (+ base s) below (+ base e)
                                          when (= (aref octets i) value) return (- i base))))
                            (span (let ((mapping (span-mapping node)))
                                    (check-mapping mapping)
                                    (let ((sap (mapping-sap mapping)) (base (span-offset node)))
                                      (declare (type sb-sys:system-area-pointer sap) (type fixnum base))
                                      (loop for i of-type fixnum from (+ base s) below (+ base e)
                                            when (= (sb-sys:sap-ref-8 sap i) value) return (- i base))))))))
                 (when hit
                   (return-from scan (+ pos (- hit s))))
                 (incf pos (- e s)))))
        (declare (dynamic-extent #'look))
        (map-leaf-pieces #'look rope :start start :end end))
      nil)))

(defun rope-find-line-end (rope &key (start 0) (end (rope-bytes rope)))
  "Return the byte offset of the first octet of ROPE from START below END
that ends a line (LINE-END-OCTET-P: a newline, or a carriage return that no
newline follows in ROPE), or NIL. Complexity: O(log n + distance scanned)."
  (declare (type fixnum start end))
  (let ((pos start) (total (rope-bytes rope)))
    (declare (type fixnum pos total))
    (block scan
      ;; on the stack, as is POS: a search for the next line end makes nothing
      (flet ((look (node s e)
               (declare (type fixnum s e))
               (let* ((size (node-bytes node))
                      (hit (with-piece-sap (sap base) node
                             (loop for i of-type fixnum from (+ base s) below (+ base e)
                                   do (let ((b (sb-sys:sap-ref-8 sap i)))
                                        (when (and (or (= b 10) (= b 13))
                                                   (line-end-octet-p
                                                    b (let ((at (+ pos (- i base s) 1)))
                                                        (cond ((< (- i base) (1- size)) (sb-sys:sap-ref-8 sap (1+ i)))
                                                              ((< at total) (rope-octet rope at))))))
                                          (return (- i base))))))))
                 (when hit
                   (return-from scan (+ pos (- hit s))))
                 (incf pos (- e s)))))
        (declare (dynamic-extent #'look))
        (map-leaf-pieces #'look rope :start start :end end))
      nil)))

(defmacro do-rope-windows ((block low high) (rope start end &key from-end) &body body)
  "Run BODY on each window of ROPE's octets from START below END, in order, or
with FROM-END (read when the macro is expanded) from the end back: LOW and
HIGH bound the window, BLOCK holds its octets from index 0. Windows start at
256 octets and double up to 64 KB, so that a search that ends soon reads
little and a long one reads in large blocks; BLOCK is on the stack, so that
none of it is garbage. BODY must not keep BLOCK, and leaves with RETURN-FROM."
  (let ((r (gensym "ROPE")) (s (gensym "START")) (e (gensym "END")) (size (gensym "SIZE")))
    `(let ((,r ,rope) (,s ,start) (,e ,end) (,size 256))
       (declare (type fixnum ,s ,e ,size))
       (let ((,block (make-array 65536 :element-type '(unsigned-byte 8))))
         (declare (type octets ,block) (dynamic-extent ,block))
         ,(if from-end
              `(let ((,high ,e))
                 (declare (type fixnum ,high))
                 (loop while (> ,high ,s)
                       do (let ((,low (max ,s (- ,high ,size))))
                            (declare (type fixnum ,low))
                            (rope-copy-into ,r ,low ,high ,block)
                            ,@body
                            (setf ,high ,low)
                            (when (< ,size 65536)
                              (setf ,size (* 2 ,size))))))
              `(let ((,low ,s))
                 (declare (type fixnum ,low))
                 (loop while (< ,low ,e)
                       do (let ((,high (min ,e (+ ,low ,size))))
                            (declare (type fixnum ,high))
                            (rope-copy-into ,r ,low ,high ,block)
                            ,@body
                            (setf ,low ,high)
                            (when (< ,size 65536)
                              (setf ,size (* 2 ,size)))))))))))

(defun rope-escaped-p (rope position)
  "True when the octet at POSITION of ROPE follows an odd number of
backslashes: escaped, since a backslash escapes the character after it.
Complexity: O(log n) for each backslash before it."
  (let ((count 0) (q position))
    (declare (type fixnum count q))
    (loop while (and (plusp q) (= 92 (rope-octet rope (1- q)))) do (incf count) (decf q))
    (oddp count)))

(defparameter *openers* "([{"
  "The brackets that open a pair, each in the place of the one that closes it in *CLOSERS*.")

(defparameter *closers* ")]}"
  "The brackets that close a pair, each in the place of the one that opens it in *OPENERS*.")

(defparameter *opening-octets* (map 'list #'char-code *openers*)
  "The octets of the brackets that open a pair (*OPENERS*).")

(defparameter *closing-octets* (map 'list #'char-code *closers*)
  "The octets of the brackets that close a pair (*CLOSERS*), each in the place of the one that opens it.")

(defparameter *bracket-octets* (append *opening-octets* *closing-octets*)
  "The octets of the brackets (*OPENERS* and *CLOSERS*).")

(defun bracket-weights (openers closers)
  "A table of 256 by octet: 1 for each bracket in OPENERS, -1 for each in CLOSERS, 0 for the rest."
  (let ((weight (make-array 256 :element-type '(signed-byte 8) :initial-element 0)))
    (loop for c across openers do (setf (aref weight (char-code c)) 1))
    (loop for c across closers do (setf (aref weight (char-code c)) -1))
    weight))

(sb-ext:define-load-time-global **bracket-weights**
    (list *openers* *closers* (bracket-weights *openers* *closers*))
  "*OPENERS*, *CLOSERS* and their BRACKET-WEIGHTS, made once.")

(defun rope-bracket-match (rope position &key backward inside (openers *openers*) (closers *closers*)
                                              (limit most-positive-fixnum) (start 0) (end (rope-bytes rope))
                                              (spans #()) escapes)
  "The partner of the opening bracket at byte POSITION of ROPE, or with BACKWARD
of the closing one just before it, found by the octets alone (brackets are
ASCII, and no other character's octets can be taken for one): each opening
bracket in OPENERS goes a level deeper and each closing one in CLOSERS a level
back, whatever their kind. Forward, the offset just after the bracket that
comes back to the first level; backward, the offset of the one that does. A
second value is true when the two are of one kind, paired by their places in
OPENERS and CLOSERS (( with ), [ with ], { with }).
With INSIDE, POSITION is taken to be within a pair already, no bracket there:
the end or the start of that pair is found, and the second value is true.
SPANS, a simple-vector of ranges (FROM . TO) in order, none within another,
are text whose brackets count for nothing (strings, comments); with ESCAPES,
nor do brackets after an odd number of backslashes (ROPE-ESCAPED-P). Only the
octets from START below END are looked at, and at most LIMIT of them. NIL when
there is no such bracket at POSITION, or no partner there.
Complexity: O(log n + distance scanned + spans before POSITION)."
  (declare (type fixnum position start end) (type simple-vector spans))
  (check-byte-offset rope position)
  (let ((weight (destructuring-bind (kept-openers kept-closers table) **bracket-weights**
                  (if (and (string= openers kept-openers) (string= closers kept-closers))
                      table
                      (bracket-weights openers closers))))
        (depth (if inside 1 0))
        (total (rope-bytes rope))
        (count (length spans))
        (own nil))
    (declare (type fixnum depth total count) (type (simple-array (signed-byte 8) (256)) weight))
    (labels ((kind (octet)
               (let ((c (code-char octet))) (or (position c openers) (position c closers))))
             (same-kind-p (octet) (or inside (eql (kind octet) own))))
      (if backward
          (let ((k (1- count)))
            (declare (type fixnum k))
            (flet ((counts-p (q)
                     ;; the last span starting at or before Q holds it, or none does
                     (loop while (and (>= k 0) (> (the fixnum (car (svref spans k))) q)) do (decf k))
                     (not (or (and (>= k 0) (< q (the fixnum (cdr (svref spans k)))))
                              (and escapes (rope-escaped-p rope q))))))
              (when (or inside (and (plusp position) (= -1 (aref weight (rope-octet rope (1- position))))))
                (unless inside (setf own (kind (rope-octet rope (1- position)))))
                (do-rope-windows (block low high) (rope (max start 0 (- position limit)) position :from-end t)
                  (loop for i of-type fixnum from (- high low 1) downto 0
                        do (let ((w (aref weight (aref block i))) (q (+ low i)))
                             (declare (type fixnum q))
                             (when (and (/= w 0) (or (and (not inside) (= q (1- position))) (counts-p q)))
                               (decf depth w)
                               (when (zerop depth)
                                 (return-from rope-bracket-match (values q (same-kind-p (aref block i)))))))))
                nil)))
          (let ((k 0))
            (declare (type fixnum k))
            (flet ((counts-p (q)
                     ;; the first span ending after Q holds it, or none does
                     (loop while (and (< k count) (<= (the fixnum (cdr (svref spans k))) q)) do (incf k))
                     (not (or (and (< k count) (<= (the fixnum (car (svref spans k))) q))
                              (and escapes (rope-escaped-p rope q))))))
              (when (or inside (and (< position total) (= 1 (aref weight (rope-octet rope position)))))
                (unless inside (setf own (kind (rope-octet rope position))))
                (do-rope-windows (block low high) (rope position (min end total (+ position limit)))
                  (loop for i of-type fixnum from 0 below (- high low)
                        do (let ((w (aref weight (aref block i))) (q (+ low i)))
                             (declare (type fixnum q))
                             (when (and (/= w 0) (or (and (not inside) (= q position)) (counts-p q)))
                               (incf depth w)
                               (when (zerop depth)
                                 (return-from rope-bracket-match (values (1+ q) (same-kind-p (aref block i)))))))))
                nil)))))))

(defun rope-find-octet-backward (rope value &key (end (rope-bytes rope)) (start 0))
  "Return the byte offset of the last octet equal to VALUE in ROPE from START
below END, or NIL. Complexity: O(log n + distance scanned)."
  (declare (type (unsigned-byte 8) value) (type fixnum start end))
  (check-byte-offset rope start) (check-byte-offset rope end)
  (do-rope-windows (block low high) (rope start end :from-end t)
    (let ((hit (position value block :end (- high low) :from-end t)))
      (when hit (return-from rope-find-octet-backward (+ low hit)))))
  nil)

(defun rope-find-line-end-backward (rope &key (end (rope-bytes rope)) (start 0))
  "Return the byte offset of the last octet of ROPE from START below END that
ends a line (ROPE-FIND-LINE-END), or NIL. Complexity: O(log n + distance
scanned)."
  (declare (type fixnum start end))
  (check-byte-offset rope start) (check-byte-offset rope end)
  ;; the octet just after the window, for a carriage return at its end
  (let ((after (and (< end (rope-bytes rope)) (rope-octet rope end))))
    (do-rope-windows (block low high) (rope start end :from-end t)
      (loop for i of-type fixnum from (- high low 1) downto 0
            do (let ((b (aref block i)))
                 (when (and (or (= b 10) (= b 13))
                            (line-end-octet-p b (if (< (1+ i) (- high low)) (aref block (1+ i)) after)))
                   (return-from rope-find-line-end-backward (+ low i)))))
      (setf after (aref block 0))))
  nil)

(defun rope-find-any-octet (rope octets &key (start 0) (end (rope-bytes rope)) from-end)
  "The byte offset of the first octet of ROPE from START below END that is one
of OCTETS (a list), or with FROM-END the last; NIL when there is none.
Complexity: O(log n + distance scanned)."
  (declare (type fixnum start end))
  (check-byte-offset rope start) (check-byte-offset rope end)
  (let ((wanted (make-array 256 :element-type 'bit :initial-element 0)))
    (dolist (octet octets) (setf (sbit wanted octet) 1))
    (if from-end
        (do-rope-windows (block low high) (rope start end :from-end t)
          (loop for i of-type fixnum from (- high low 1) downto 0
                when (= 1 (sbit wanted (aref block i))) do (return-from rope-find-any-octet (+ low i))))
        (do-rope-windows (block low high) (rope start end)
          (loop for i of-type fixnum from 0 below (- high low)
                when (= 1 (sbit wanted (aref block i))) do (return-from rope-find-any-octet (+ low i)))))
    nil))

(defparameter *blank-octets* '(32 9 10 13)
  "The octets of the blanks between words and lines: a space, a tab, a newline
and a carriage return.")

(defparameter *text-octets* (loop for octet below 256 unless (member octet *blank-octets*) collect octet)
  "The octets that make a line more than blank: all but the blanks (*BLANK-OCTETS*).")

(defun rope-skip-octets (rope position octets &key backward (bound (if backward 0 (rope-bytes rope))))
  "POSITION of ROPE moved over the octets in OCTETS, a list of them (such as
*BLANK-OCTETS*): forward to the first octet at or after it that is not one of
them, or BOUND (the end of ROPE) when there is none before it; with BACKWARD,
back to just after the last octet before it that is not one of them, or BOUND
\(0). Complexity: O(log n + distance moved)."
  (declare (type fixnum position bound))
  (check-byte-offset rope position)
  (let ((skipped (make-array 256 :element-type 'bit :initial-element 0)))
    (dolist (octet octets) (setf (sbit skipped octet) 1))
    (if backward
        (do-rope-windows (block low high) (rope (min bound position) position :from-end t)
          (loop for i of-type fixnum from (- high low 1) downto 0
                when (zerop (sbit skipped (aref block i))) do (return-from rope-skip-octets (+ low i 1))))
        (do-rope-windows (block low high) (rope position (max bound position))
          (loop for i of-type fixnum from 0 below (- high low)
                when (zerop (sbit skipped (aref block i))) do (return-from rope-skip-octets (+ low i)))))
    bound))

(defparameter *blank-line-octets* (remove 10 *blank-octets*)
  "The octets a blank line may hold: the blanks (*BLANK-OCTETS*) but the newline.")

(sb-ext:define-load-time-global **blank-line-octets**
    (let ((table (make-array 256 :element-type 'bit :initial-element 0)))
      (dolist (octet *blank-line-octets* table)
        (setf (sbit table octet) 1)))
  "One bit for each octet, 1 for those a blank line may hold (*BLANK-LINE-OCTETS*).")

(declaim (inline blank-line-octet-p))
(defun blank-line-octet-p (octet)
  "True for an octet a blank line may hold (*BLANK-LINE-OCTETS*)."
  (= 1 (sbit (the simple-bit-vector **blank-line-octets**) octet)))

(defun map-line-slices (function rope start end left right &key crlf octets width block)
  "Call FUNCTION for each line of ROPE that starts at a byte offset in [START,
END), START being a line start: with the line's start, the byte offsets of its
LEFT-th and RIGHT-th characters (its end, where it is shorter), the columns
those stand at (the line's length in characters, where it is shorter), and the
line's end: the octet that ends it (LINE-END-OCTET-P), with CRLF the carriage
return before a newline, or the text's end. With OCTETS, three more: the
block of octets read, where in the text it starts and where it ends; a line
shorter than a block is in it whole, its line end too. Columns count characters, as TUATARA.BUFFER:BUFFER-COLUMN
does: a raw byte is one. With WIDTH, they are columns on the screen
instead: a printable ASCII character takes one, any other what WIDTH says, a
function of its code, the column it stands at and what it gave as its second
value for the character before (that character's code for a printable ASCII
one, 0 at the line's start) returning the columns and that second value, so
that a tab can reach its stop and a grapheme cluster take the columns of its
first character; a tab across LEFT or
RIGHT is in the slice (its start or end column then outside them), another
character across one out of it, and a character of no width (a combining
mark) goes with the one before it. The text is read once, a block at a time:
as large as the lines asked for, from 4 KB to a megabyte, never more than
the text left, into BLOCK when given octets as large, else into one kept
for reading into (WITH-OCTET-BLOCK), so that nothing is made for it: FUNCTION
must not keep the block. An ASCII octet is a character by itself, so most lines cost a look at
each octet. Complexity: O(END - START + the length of the last line)."
  (declare (type function function) (type fixnum start end left right)
           (optimize speed))
  (let* ((total (rope-bytes rope))
         (size (max (+ 16 (* 4 right))
                    (min (* 1024 1024) (- total start) (max 4096 (* 2 (- (min end total) start))))))
         (block-start 0) (block-end 0)
         (pos start))
    (declare (type fixnum total size block-start block-end pos))
    (unless (and block (>= (length (the octets block)) size))
      (return-from map-line-slices
        (with-octet-block (kept size)
          (map-line-slices function rope start end left right
                           :crlf crlf :octets octets :width width :block kept))))
    (let ((block block))
      (declare (type octets block))
      (labels ((fill-from (at)
                 (declare (type fixnum at))
                 (setf block-start at
                       block-end (min total (+ at size)))
                 (rope-copy-into rope block-start block-end block))
               (ends-line-p (i avail stop-at-pair)
                 ;; the octet at I of the block, a newline or a carriage return, ends the
                 ;; line's text: as a line end, or with STOP-AT-PAIR before a newline
                 (declare (type fixnum i avail))
                 (or (= (aref block i) 10)
                     stop-at-pair
                     (let ((at (+ block-start i 1)))
                       (declare (type fixnum at))
                       (/= 10 (cond ((< (1+ i) avail) (aref block (1+ i)))
                                    ((< at total) (rope-octet rope at))
                                    (t 0)))))))
        (declare (inline ends-line-p))
        (loop while (and (< pos end) (<= pos total))
              do ;; a slice takes at most four octets a character, and a line end two
                 (when (or (< pos block-start) (> (min total (+ pos (* 4 right) 5)) block-end))
                   (fill-from pos))
                 (let ((i (- pos block-start))
                       (avail (- block-end block-start))
                       (column 0) (slice-start -1) (start-column 0) (before 0))
                   (declare (type fixnum i avail column slice-start start-column before))
                   (if (null width)
                       (loop
                         (when (and (= column left) (< slice-start 0))
                           (setf slice-start (+ block-start i) start-column column))
                         (when (or (= column right) (>= i avail)) (return))
                         (let ((b (aref block i)))
                           (when (and (or (= b 10) (= b 13)) (ends-line-p i avail crlf))
                             (return))
                           (if (< b 128)
                               (incf i)
                               (incf i (the fixnum (nth-value 1 (octets-decode block i avail))))))
                         (incf column))
                       ;; columns on the screen: a tab to its stop, others as WIDTH says; a tab
                       ;; across an edge taken in (its columns made spaces by the caller), another
                       ;; character across one left out, one of no width kept with the one before
                       (loop
                         ;; characters of no width can fill the block before RIGHT: read on, the
                         ;; line from its start where the block began before it
                         (when (and (> (+ i 4) avail) (< block-end total))
                           (let ((at (+ block-start i)))
                             (declare (type fixnum at))
                             (fill-from (if (> pos block-start) pos at))
                             (setf i (- at block-start) avail (- block-end block-start))))
                         (when (>= i avail) (return))
                         (let* ((b (aref block i))
                                (tab (= b 9)))
                           (when (and (or (= b 10) (= b 13)) (ends-line-p i avail crlf))
                             (return))
                           (multiple-value-bind (w len next)
                               (if (< 31 b 127)
                                   (values 1 1 b)
                                   (multiple-value-bind (code length) (if (< b 128) (values b 1) (octets-decode block i avail))
                                     (multiple-value-bind (w next) (funcall (the function width) code column before)
                                       (values w length next))))
                             (declare (type fixnum w len next))
                             (setf before next)
                             (when (and (< slice-start 0) (plusp w)
                                        (or (>= column left)
                                            (and tab (< column left (+ column w)))))
                               (setf slice-start (+ block-start i) start-column column))
                             (when (and (>= slice-start 0) (plusp w)
                                        (or (>= column right)
                                            (and (not tab) (< column right (+ column w)))))
                               (return))
                             (incf i len)
                             (incf column w)))))
                   (let ((slice-end (+ block-start i)))
                     (declare (type fixnum slice-end))
                     (when (< slice-start 0) (setf slice-start slice-end start-column column))
                     (let* ((newline (let ((from slice-end))
                                       (declare (type fixnum from))
                                       (loop
                                         (let ((hit (loop with avail of-type fixnum = (- block-end block-start)
                                                          for k of-type fixnum from (- from block-start) below avail
                                                          do (let ((b (aref block k)))
                                                               (when (and (or (= b 10) (= b 13)) (ends-line-p k avail nil))
                                                                 (return k))))))
                                           (when hit (return (+ block-start (the fixnum hit))))
                                           (when (>= block-end total) (return total))
                                           ;; read on from the line's start, so that a line shorter than a
                                           ;; block is in the block whole; a longer one, from where the search is
                                           (if (> pos block-start)
                                               (fill-from pos)
                                               (progn (setf from block-end) (fill-from from)))))))
                            (line-end (cond ((or (not crlf) (>= newline total) (<= newline slice-end)) newline)
                                            ((> newline block-start)
                                             (+ block-start (line-text-end block 0 (- newline block-start) crlf)))
                                            ((rope-crlf-p rope (1- newline)) (1- newline))
                                            (t newline))))
                       (declare (type fixnum newline line-end))
                       (if octets
                           (funcall function pos slice-start slice-end start-column column line-end
                                    block block-start block-end)
                           (funcall function pos slice-start slice-end start-column column line-end))
                       (setf pos (1+ newline))))))))
    nil))

(defun rope-line-downward (rope start &key blank (limit most-positive-fixnum))
  "The start of the first line of ROPE, from the one starting at byte START
down, that is blank (nothing but spaces, tabs and carriage returns) with
BLANK, or has text without; the end of the text when none is. NIL once LIMIT
octets have been read without an answer. Complexity: O(log n + distance
scanned)."
  (declare (type fixnum start))
  (check-byte-offset rope start)
  (let* ((total (rope-bytes rope))
         (top (min total (+ start limit)))
         (line start)
         (empty t))
    (declare (type fixnum total top line))
    (do-rope-windows (block low high) (rope start top)
      (loop for i of-type fixnum from 0 below (- high low)
            for octet of-type (unsigned-byte 8) = (aref block i)
            do (cond ((and (or (= octet 10) (= octet 13))
                           (line-end-octet-p octet (cond ((< (1+ i) (- high low)) (aref block (1+ i)))
                                                         ((< high total) (rope-octet rope high)))))
                      (when (and blank empty) (return-from rope-line-downward line))
                      (setf line (+ low i 1) empty t))
                     ((blank-line-octet-p octet))
                     (t (unless blank (return-from rope-line-downward line))
                        (setf empty nil)))))
    (cond ((< top total) nil)
          ((and blank empty (< line total)) line)
          (t total))))

(defun rope-line-upward (rope start &key blank (limit most-positive-fixnum))
  "The start of the first line of ROPE, from the one starting at byte START
up, that is blank (nothing but spaces, tabs and carriage returns) with BLANK,
or has text without. NIL when none is before the start of the text; NIL and
:LIMIT once LIMIT octets have been read without an answer. Complexity:
O(log n + distance scanned)."
  (declare (type fixnum start))
  (check-byte-offset rope start)
  (let* ((total (rope-bytes rope))
         (own-end (or (rope-find-line-end rope :start start :end (min total (+ start limit)))
                      (if (< (+ start limit) total) (return-from rope-line-upward (values nil :limit)) total)))
         (wanted-empty (and blank t)))
    (declare (type fixnum total own-end))
    ;; the line at START first
    (when (eq (null (rope-find-any-octet rope *text-octets* :start start :end own-end)) wanted-empty)
      (return-from rope-line-upward start))
    ;; then each line above, read backward from the line end that ends it
    (when (zerop start) (return-from rope-line-upward nil))
    (let ((bottom (max 0 (- start limit))) (empty t) (after (rope-octet rope (1- start))))
      (do-rope-windows (block low high) (rope bottom (1- start) :from-end t)
        (loop for i of-type fixnum from (- high low 1) downto 0
              for octet of-type (unsigned-byte 8) = (aref block i)
              do (cond ((and (or (= octet 10) (= octet 13))
                             (line-end-octet-p octet (if (< (1+ i) (- high low)) (aref block (1+ i)) after)))
                        (when (eq empty wanted-empty) (return-from rope-line-upward (+ low i 1)))
                        (setf empty t))
                       ((blank-line-octet-p octet))
                       (t (setf empty nil))))
        (setf after (aref block 0)))
      (cond ((plusp bottom) (values nil :limit))
            ((eq empty wanted-empty) 0)
            (t nil)))))

(defun rope-line-start (rope byte)
  "Return the byte offset of the start of the line containing byte offset BYTE:
the position after the line end before it (LINE-END-OCTET-P), or 0. Complexity: O(log n + the part
of the line before BYTE), and once ROPE's line counts are made no more than
64 KB of it, however long the line: a longer one is found by its number."
  (declare (type fixnum byte))
  (check-byte-offset rope byte)
  (let* ((near (max 0 (- byte 65536)))
         (newline (or (rope-find-line-end-backward rope :start near :end byte)
                      (cond ((zerop near) nil)
                            ((rope-indexed-p rope) (1- (line->byte rope (byte->line rope byte))))
                            (t (rope-find-line-end-backward rope :end near))))))
    (if newline (1+ newline) 0)))

(defun rope-line-end (rope byte)
  "Return the byte offset of the octet ending the line containing BYTE (a
newline, or a carriage return no newline follows), or the text length when
the last line has none. Complexity: O(log n + the part of the
line after BYTE), and once ROPE's line counts are made no more than 64 KB of
it, however long the line: a longer one is found by its number."
  (declare (type fixnum byte))
  (check-byte-offset rope byte)
  (let* ((total (rope-bytes rope))
         (near (min total (+ byte 65536))))
    (declare (type fixnum total near))
    (or (rope-find-line-end rope :start byte :end near)
        (cond ((= near total) total)
              ((rope-indexed-p rope)
               (let ((line (byte->line rope byte)))
                 (if (< line (rope-lines rope)) (1- (line->byte rope (1+ line))) total)))
              (t (or (rope-find-line-end rope :start near) total))))))

;;; Pieces of a text that end at line starts

(defun rope-line-pieces (rope start end size)
  "Pieces of ROPE from START below END, each about SIZE octets and ending at a
line start, or at END: a list of (START END)."
  (declare (type fixnum start end size))
  (let ((pieces '()) (p start))
    (declare (type fixnum p))
    (loop while (< p end)
          do (let* ((nominal (+ p size))
                    (cut (if (>= nominal end)
                             end
                             (let ((newline (rope-find-line-end rope :start nominal :end end)))
                               (if newline (1+ newline) end)))))
               (push (list p cut) pieces)
               (setf p cut)))
    (nreverse pieces)))

;;; Invariants, for tests

(defun check-rope (rope)
  "Verify the structural invariants of ROPE and return T, or signal an error
naming the first violation: uniform height, fanout bounds, leaf sizes, counts
matching a rescan, no indexed leaf starting inside a character, and no two
parting a carriage return from the newline after it."
  (labels ((check (node expected-height)
             (unless (= (node-height node) expected-height)
               (error "Node height ~a, expected ~a." (node-height node) expected-height))
             (if (branch-p node)
                 (let ((children (branch-children node)))
                   (unless (<= 1 (length children) +max-fanout+)
                     (error "Branch with ~a children." (length children)))
                   (loop for c across children do (check c (1- expected-height)))
                   (let ((indexed (every #'node-indexed-p children)))
                     (unless (= (node-bytes node) (reduce #'+ children :key #'node-bytes))
                       (error "Branch byte count disagrees with its children."))
                     (if indexed
                         (unless (and (= (node-chars node) (reduce #'+ children :key #'node-chars))
                                      (= (node-utf16 node) (reduce #'+ children :key #'node-utf16))
                                      (= (node-lines node) (reduce #'+ children :key #'node-lines)))
                           (error "Branch counts disagree with its children."))
                         (when (node-indexed-p node)
                           (error "Branch claims counts but has an unindexed child.")))))
                 (progn
                   (let ((most (if (span-p node) +span-size+ +max-leaf+)))
                     (when (> (node-bytes node) most)
                       (error "Leaf of ~a octets exceeds ~a." (node-bytes node) most)))
                   (when (node-indexed-p node)
                     (multiple-value-bind (c u l v) (piece-summarise node 0 (node-bytes node))
                       (unless (and (= (node-chars node) c) (= (node-utf16 node) u) (= (node-lines node) l)
                                    (= (node-invalid node) v))
                         (error "Leaf counts ~a/~a/~a/~a disagree with rescan ~a/~a/~a/~a."
                                (node-chars node) (node-utf16 node) (node-lines node) (node-invalid node) c u l v))))))))
    (check (rope-root rope) (node-height (rope-root rope)))
    ;; every indexed leaf starts where a character of the whole text starts, and
    ;; never with the newline of a carriage return that ends an indexed leaf before it
    (let ((at 0) (previous nil))
      (map-leaves (lambda (node)
                    (when (and (node-indexed-p node) (plusp at) (not (char-boundary-p rope at)))
                      (error "An indexed leaf starts at ~a, inside a character." at))
                    (when (and previous (node-indexed-p previous) (node-indexed-p node)
                               (plusp (node-bytes previous)) (plusp (node-bytes node))
                               (= 13 (piece-octet previous (1- (node-bytes previous))))
                               (= 10 (piece-octet node 0)))
                      (error "A leaf starts at ~a with the newline of a carriage return before it." at))
                    (setf previous node)
                    (incf at (node-bytes node)))
                  rope))
    t))
