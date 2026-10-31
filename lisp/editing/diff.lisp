;;;; lisp/editing/diff.lisp --- Differences between two texts, line by line.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Lines are compared by hashes (FNV-1a), and the differences found by
;;;; Myers's algorithm after the lines both texts begin and end with are set
;;;; aside, reported as git diff -U0 reports hunks. The version-control gutter
;;;; compares the buffer with the committed file this way; reading a changed
;;;; file again compares the old text with the new, so that point and the window
;;;; stay on the same text. Two lists of lines are compared the same way
;;;; (LINE-DIFF).

(in-package #:tuatara.defaults)

(defun hash-lines-into (octets end hash open hashes &optional (avail end))
  "Go on hashing lines (FNV-1a, the octet that ends each left out:
TUATARA.TEXT:LINE-END-OCTET-P) through OCTETS below END from HASH, pushing the
hash of each line ended on HASHES; the text goes on to AVAIL, so that a
carriage return at END - 1 ends a line unless the octet at END is a newline.
Returns the hash and whether a line is open, to go on with in the next octets."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum end avail)
           (type (unsigned-byte 64) hash))
  (loop with start of-type fixnum = 0
        for newline = (tuatara.text:octets-line-end-position octets start end avail)
        do (setf hash (tuatara.text:octets-hash octets :start start :end (or newline end) :hash hash))
           (unless newline
             (return (values hash (or open (< start end)))))
           (vector-push-extend (ldb (byte 62 0) hash) hashes)
           (setf hash +fnv-offset+ open nil start (1+ newline))))

(defun unended-line-hash (hash)
  "The hash of a last line that has no line end after it, HASH its octets':
another than the same line's with one, so that a line end added or taken
away at the end is a line changed."
  (ldb (byte 62 0) (logxor hash #x2545F4914F6CDD1D)))

(defun line-hashes (octets)
  "A hash of each line of OCTETS (FNV-1a over its octets, the octet that ends
it left out), as a fixnum vector; a last line without a line end counts."
  (declare (type tuatara.text:octets octets))
  (let ((hashes (make-array 1024 :element-type 'fixnum :adjustable t :fill-pointer 0)))
    (multiple-value-bind (hash open) (hash-lines-into octets (length octets) +fnv-offset+ nil hashes)
      (when open (vector-push-extend (unended-line-hash hash) hashes)))
    (coerce hashes '(simple-array fixnum (*)))))

(defun rope-line-hashes (rope)
  "LINE-HASHES of ROPE's text, read a megabyte at a time rather than copied whole."
  (let ((hashes (make-array 1024 :element-type 'fixnum :adjustable t :fill-pointer 0))
        (chunk (make-array (* 1024 1024) :element-type '(unsigned-byte 8)))
        (size (tuatara.text:rope-bytes rope))
        (hash +fnv-offset+) (open nil))
    ;; an octet more than is hashed, read for a carriage return at a chunk's end
    (loop for at from 0 below size by (1- (length chunk))
          do (let* ((n (min (- size at) (1- (length chunk))))
                    (avail (tuatara.text:rope-copy-into rope at (min size (+ at n 1)) chunk)))
               (multiple-value-setq (hash open) (hash-lines-into chunk n hash open hashes avail))))
    (when open (vector-push-extend (unended-line-hash hash) hashes))
    (coerce hashes '(simple-array fixnum (*)))))

(defun myers-moves (a a0 a1 b b0 b1 limit)
  "The moves of a shortest edit script from A[A0..A1) to B[B0..B1), vectors of
line hashes, by Myers's algorithm: a list of :EQUAL, :DELETE and :INSERT, one a
line. NIL when more than LIMIT lines differ."
  (let* ((n (- a1 a0)) (m (- b1 b0)) (reach (min (+ n m) limit))
         (v (make-array (+ (* 2 reach) 3) :element-type 'fixnum :initial-element 0))
         (trace (make-array 16 :adjustable t :fill-pointer 0))
         (end nil))
    (flet ((v (k) (aref v (+ k reach 1))))
      (loop for d from 0 to reach
            do (loop for k from (- d) to d by 2
                     do (let* ((x (if (or (= k (- d)) (and (/= k d) (< (v (1- k)) (v (1+ k)))))
                                      (v (1+ k))
                                      (1+ (v (1- k)))))
                               (y (- x k)))
                          (loop while (and (< x n) (< y m) (= (aref a (+ a0 x)) (aref b (+ b0 y))))
                                do (incf x) (incf y))
                          (setf (aref v (+ k reach 1)) x)
                          (when (and (>= x n) (>= y m)) (setf end d) (return))))
               (when end (return))
               ;; V after step D, for k from -D to D
               (vector-push-extend (subseq v (+ (- d) reach 1) (+ d reach 2)) trace)))
    (when end
      (let ((moves '()) (x n) (y m))
        (loop for d from end downto 1
              do (let* ((previous (aref trace (1- d)))
                        (k (- x y))
                        (down (or (= k (- d))
                                  (and (/= k d) (< (aref previous (+ (1- k) (1- d))) (aref previous (+ (1+ k) (1- d)))))))
                        (pk (if down (1+ k) (1- k)))
                        (px (aref previous (+ pk (1- d))))
                        (py (- px pk))
                        (mx (if down px (1+ px)))
                        (my (if down (1+ py) py)))
                   (loop while (and (> x mx) (> y my)) do (push :equal moves) (decf x) (decf y))
                   (push (if down :insert :delete) moves)
                   (setf x px y py)))
        (loop while (and (> x 0) (> y 0)) do (push :equal moves) (decf x) (decf y))
        moves))))

(defun diff-hunks (old new &key (limit 2000))
  "The hunks that turn the lines OLD into NEW (vectors of line hashes) as git
diff -U0 reports them: a list of (OLD-START OLD-COUNT NEW-START NEW-COUNT),
1-based, a side without lines giving the line after which the other side's go.
The lines both begin and end with are set aside first; past LIMIT differences,
all that lies between is one hunk."
  (let* ((n (length old)) (m (length new)) (prefix 0) (suffix 0))
    (loop while (and (< prefix n) (< prefix m) (= (aref old prefix) (aref new prefix))) do (incf prefix))
    (loop while (and (< suffix (- n prefix)) (< suffix (- m prefix))
                     (= (aref old (- n 1 suffix)) (aref new (- m 1 suffix))))
          do (incf suffix))
    (let ((a0 prefix) (a1 (- n suffix)) (b0 prefix) (b1 (- m suffix)))
      (flet ((hunk (x y old-count new-count)
               (list (if (zerop old-count) x (1+ x)) old-count (if (zerop new-count) y (1+ y)) new-count)))
        (if (and (= a0 a1) (= b0 b1))
            '()
            (let ((moves (myers-moves old a0 a1 new b0 b1 limit)))
              (if (null moves)
                  (list (hunk a0 b0 (- a1 a0) (- b1 b0)))
                  (let ((x a0) (y b0) (hunks '()) (open nil))
                    (dolist (move moves)
                      (ecase move
                        (:equal (when open (push (apply #'hunk open) hunks) (setf open nil)) (incf x) (incf y))
                        (:delete (unless open (setf open (list x y 0 0))) (incf (third open)) (incf x))
                        (:insert (unless open (setf open (list x y 0 0))) (incf (fourth open)) (incf y))))
                    (when open (push (apply #'hunk open) hunks))
                    (nreverse hunks)))))))))

(defun line-diff (old new &key (limit 2000))
  "The lines that differ between OLD and NEW, sequences of strings, each
\(:REMOVED LINE) or (:ADDED LINE), in order, the lines both share left out:
each hunk DIFF-HUNKS finds, its lines removed, then those added. Past LIMIT
differences, all that lies between the lines both begin and end with is
removed and added."
  (let* ((old (coerce old 'simple-vector)) (new (coerce new 'simple-vector))
         (hunks (diff-hunks (map '(simple-array fixnum (*)) #'tuatara.text:string-hash old)
                            (map '(simple-array fixnum (*)) #'tuatara.text:string-hash new)
                            :limit limit)))
    (loop for (old-start old-count new-start new-count) in hunks
          append (loop for i from (1- old-start) repeat old-count collect (list :removed (svref old i)))
          append (loop for i from (1- new-start) repeat new-count collect (list :added (svref new i))))))

(defun unified-diff (old new &key (old-name "old") (new-name "new") (context 3) (limit 2000))
  "The differences between OLD and NEW, lists of lines (strings without their
newlines), written as diff --unified and git write them: a --- line naming
OLD-NAME and a +++ line naming NEW-NAME, then each hunk, its @@ line, the
lines only OLD has after -, those only NEW has after +, and CONTEXT lines
both have around them after a space, hunks whose context would meet made
one. Empty when they are the same. LIMIT is as for DIFF-HUNKS."
  (let* ((old (coerce old 'simple-vector)) (new (coerce new 'simple-vector))
         (n (length old))
         ;; each hunk as 0-based (OLD-FROM OLD-TO NEW-FROM NEW-TO)
         (hunks (loop for (os oc ns nc) in (diff-hunks (map '(simple-array fixnum (*)) #'tuatara.text:string-hash old)
                                                         (map '(simple-array fixnum (*)) #'tuatara.text:string-hash new)
                                                         :limit limit)
                      for o = (if (zerop oc) os (1- os))
                      for s = (if (zerop nc) ns (1- ns))
                      collect (list o (+ o oc) s (+ s nc))))
         (groups '()))
    ;; hunks whose context meets, together
    (dolist (hunk hunks)
      (if (and groups (<= (- (first hunk) (second (first (first groups)))) (* 2 context)))
          (push hunk (first groups))
          (push (list hunk) groups)))
    (if (null hunks)
        ""
        (with-output-to-string (out)
          (format out "--- ~a~%+++ ~a~%" old-name new-name)
          (dolist (group (nreverse (mapcar #'reverse groups)))
            (let* ((first (first group)) (last (car (last group)))
                   (old-from (max 0 (- (first first) context)))
                   (old-to (min n (+ (second last) context)))
                   (new-from (- (third first) (- (first first) old-from)))
                   (new-to (+ (fourth last) (- old-to (second last)))))
              (flet ((range (from to) (format nil "~d,~d" (if (= from to) from (1+ from)) (- to from))))
                (format out "@@ -~a +~a @@~%" (range old-from old-to) (range new-from new-to)))
              (let ((at old-from))
                (dolist (hunk group)
                  (destructuring-bind (o0 o1 n0 n1) hunk
                    (loop for i from at below o0 do (format out " ~a~%" (svref old i)))
                    (loop for i from o0 below o1 do (format out "-~a~%" (svref old i)))
                    (loop for i from n0 below n1 do (format out "+~a~%" (svref new i)))
                    (setf at o1)))
                (loop for i from at below old-to do (format out " ~a~%" (svref old i))))))))))

(defun map-line (hunks line)
  "The line of the new text that 0-based LINE of the old one becomes through
HUNKS (DIFF-HUNKS): moved by the lines added and removed before it; a line
that changed goes to the same place among the lines that replace it, or the
last of them, or where removed lines were."
  (let ((delta 0))
    (dolist (hunk hunks (+ line delta))
      (destructuring-bind (old-start old-count new-start new-count) hunk
        ;; 0-based first lines: a side without lines names the line after which the other's go
        (let ((old-first (if (zerop old-count) old-start (1- old-start)))
              (new-first (if (zerop new-count) new-start (1- new-start))))
          (cond ((< line old-first) (return (+ line delta)))
                ((< line (+ old-first old-count))
                 (return (if (zerop new-count) new-first (+ new-first (min (- line old-first) (1- new-count))))))
                (t (setf delta (- (+ new-first new-count) (+ old-first old-count))))))))))

(defun map-by-lines (from to hunks position)
  "POSITION in the rope FROM taken to the same line and column of the rope TO,
through HUNKS (DIFF-HUNKS from FROM's lines to TO's); the column is cut to the
line's length and kept off the middle of a character, and a line past TO's
last goes to its end."
  (let* ((line (tuatara.text:byte->line from position))
         (column (- position (tuatara.text:line->byte from line)))
         (target (map-line hunks line)))
    (if (> target (tuatara.text:rope-lines to))
        (tuatara.text:rope-bytes to)
        (let* ((start (tuatara.text:line->byte to target))
               (at (min (+ start column) (tuatara.text:rope-line-end to start))))
          (tuatara.text:rope-char-start to at)))))

(defvar *reload-diff-limit* (* 8 1024 1024)
  "The largest text, in octets, compared line by line with the file read again
in its place, so that point and the window stay on the same text; larger ones
keep their offsets instead.")

(defun text-mapping (old new)
  "Two functions for BUFFER-SWAP-TEXT, taking positions in the rope OLD to the
same text in the rope NEW and back: through the lines that differ, when both
are at most *RELOAD-DIFF-LIMIT* octets and indexed; else a position stays
where it is, cut to the other text's size."
  (let ((old-size (tuatara.text:rope-bytes old)) (new-size (tuatara.text:rope-bytes new)))
    (if (and (<= old-size *reload-diff-limit*) (<= new-size *reload-diff-limit*)
             (tuatara.text:rope-indexed-p old) (tuatara.text:rope-indexed-p new))
        (let* ((hunks (diff-hunks (rope-line-hashes old) (rope-line-hashes new)))
               (back (mapcar (lambda (hunk) (destructuring-bind (os oc ns nc) hunk (list ns nc os oc))) hunks)))
          (values (lambda (position bias) (declare (ignore bias)) (map-by-lines old new hunks position))
                  (lambda (position bias) (declare (ignore bias)) (map-by-lines new old back position))))
        (values (lambda (position bias) (declare (ignore bias)) (min position new-size))
                (lambda (position bias) (declare (ignore bias)) (min position old-size))))))

(defun line-edges (rope from count)
  "The byte offsets in ROPE where its COUNT lines from the 0-based line FROM
begin and end; past its last line, its end."
  (flet ((at (line) (if (> line (tuatara.text:rope-lines rope)) (tuatara.text:rope-bytes rope) (tuatara.text:line->byte rope line))))
    (values (at from) (at (+ from count)))))

(defun put-text-by-lines (buffer old new)
  "Make BUFFER's text, OLD (a rope, as BUFFER-TEXT gave it), NEW (a rope or a
string) as one change to undo, only the lines that differ replaced, so that
the cursor and the window stay on the text they were on; returns how many
places changed."
  (let* ((new (if (stringp new) (tuatara.text:string->rope new) new))
         (edits (loop for (os oc ns nc) in (diff-hunks (rope-line-hashes old) (rope-line-hashes new))
                     collect (multiple-value-bind (start end) (if (zerop oc) (line-edges old os 0) (line-edges old (1- os) oc))
                               (multiple-value-bind (from to) (if (zerop nc) (values 0 0) (line-edges new (1- ns) nc))
                                 (tuatara.text:make-edit start end (and (< from to) (tuatara.text:rope-octets new :start from :end to))))))))
    (when edits
      (tuatara.buffer:buffer-edit buffer (coerce edits 'vector)))
    (length edits)))
