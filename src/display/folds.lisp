;;;; src/display/folds.lisp --- Folds: parts of a buffer hidden from its windows, an ellipsis in their place.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A buffer's folds are ranges of its text that its windows do not show: a
;;;; row reaching one shows an ellipsis and goes on after it. Each fold's ends
;;;; are anchors, so that it stays where it was through edits elsewhere; a
;;;; fold an edit empties goes. The text itself is never changed by folding,
;;;; and point is kept out of folds: moved into one, it goes to its start or,
;;;; going forward, its end. Org's subtrees fold with them.
;;;;
;;;; The lines search filters hide are folds too, but of a kind of their own
;;;; (HIDE-RANGES): there may be millions of them, so they are kept in one
;;;; vector of offsets, carried through each change at once, rather than as
;;;; anchors; and the text they hide is never changed by an edit of what is
;;;; shown (HIDDEN-EDIT-P says which edits would).

(in-package #:tuatara.display)

(defun buffer-folds (buffer)
  "BUFFER's folds as ranges of octets, each (START . END), in order, those
that overlap made one, those emptied by edits left out; worked out once for
the buffer's version and folds, and kept until either changes. Never to be
changed in place."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (folds (tuatara.buffer:buffer-local buffer :folds))
         (kept (tuatara.buffer:buffer-local buffer 'folds-merged)))
    (if (and kept (eql (first kept) version) (eq (second kept) folds))
        (cddr kept)
        (let* ((ranges (loop for (start . end) in folds
                             for s = (tuatara.text:anchor-position start version)
                             for e = (tuatara.text:anchor-position end version)
                             when (< s e) collect (cons s e)))
               (merged (tuatara.text:merge-ranges ranges)))
          (setf (tuatara.buffer:buffer-local buffer 'folds-merged) (list* version folds merged))
          merged))))

(defun fold-region (buffer start end)
  "Hide the octets of BUFFER from START below END from its windows, an
ellipsis in their place."
  (when (< start end)
    (let ((version (tuatara.buffer:buffer-version buffer)))
      (push (cons (tuatara.text:make-anchor version start :bias :after)
                  (tuatara.text:make-anchor version end :bias :before))
            (tuatara.buffer:buffer-local buffer :folds)))))

(defun unfold-region (buffer start end)
  "Show again what BUFFER's folds hide from START below END: the folds
overlapping it go."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (setf (tuatara.buffer:buffer-local buffer :folds)
          (remove-if (lambda (fold)
                       (let ((s (tuatara.text:anchor-position (car fold) version))
                             (e (tuatara.text:anchor-position (cdr fold) version)))
                         (or (>= s e) (and (< s end) (> e start)) (and (= s e start)))))
                     (tuatara.buffer:buffer-local buffer :folds)))))

(defun unfold-ranges (buffer ranges)
  "Show again what BUFFER's folds hide in RANGES, a vector of (START . END) in
order, none overlapping another: the folds overlapping any of them go, all in
one pass over the folds, each range found by halving."
  (when (plusp (length ranges))
    (let ((version (tuatara.buffer:buffer-version buffer)))
      (setf (tuatara.buffer:buffer-local buffer :folds)
            (remove-if (lambda (fold)
                         (let* ((s (tuatara.text:anchor-position (car fold) version))
                                (e (tuatara.text:anchor-position (cdr fold) version))
                                (i (fold-index ranges (lambda (range) (> (cdr range) s)))))
                           (or (>= s e) (and (< i (length ranges)) (< (car (svref ranges i)) e)))))
                       (tuatara.buffer:buffer-local buffer :folds))))))

(defun unfold-all (buffer)
  "Show all of BUFFER again: its folds go."
  (setf (tuatara.buffer:buffer-local buffer :folds) '()))

;;; Lines hidden by search filters

(defstruct (hidden-ranges (:constructor make-hidden-ranges (version bounds)) (:copier nil))
  "The stretches of a buffer's text that search filters hide: BOUNDS, a
vector of fixnums holding the start and end of each in turn, in order and
apart, as offsets into the text of VERSION. Each starts at the newline ending
the line shown before it and ends at the newline ending its last line, or at
the end of the text. TOUCHED is true once a change reached into one of them,
or replaced the whole text, which shows everything again."
  version (bounds nil :type (simple-array fixnum (*))) (touched nil))

(setf (documentation 'hidden-ranges-p 'function) "True when OBJECT is a HIDDEN-RANGES."
      (documentation 'hidden-ranges-version 'function) "The version of a text HIDDEN-RANGES' bounds are offsets into."
      (documentation 'hidden-ranges-bounds 'function) "The start and end of each stretch hidden, in turn, a vector of fixnums."
      (documentation 'hidden-ranges-touched 'function) "True once a change reached into what HIDDEN-RANGES hides.")

(defun hide-ranges (buffer bounds &key version)
  "Hide from BUFFER's windows the stretches of its text BOUNDS gives, a vector
of fixnums holding the start and end of each in turn, in order and apart, as
offsets into the text of VERSION (BUFFER's version now unless given), those
hidden before shown again: the lines search filters leave out, each stretch
from the newline ending a line shown to the newline ending the last line it
hides. NIL or an empty vector hides nothing."
  (setf (tuatara.buffer:buffer-local buffer :hidden-ranges)
        (and bounds (plusp (length bounds))
             (make-hidden-ranges (or version (tuatara.buffer:buffer-version buffer))
                                 (coerce bounds '(simple-array fixnum (*)))))))

(defun hidden-edit-p (edit start end rope)
  "True when EDIT, to be made to ROPE, would change a line hidden by the
stretch from START to END (HIDE-RANGES): an insertion inside it, or at its
end unless what goes in begins a line of its own; a deletion or replacement
reaching into it or taking the newline after it. The newline it starts at
may go with whole lines shown before it, deleted from a line's start or put
in place by whole lines, as when a line shown is killed whole or the lines
shown are sorted."
  (let* ((a (tuatara.text:edit-start edit))
         (z (tuatara.text:edit-end edit))
         (insert (tuatara.text:edit-insert edit))
         (inserts (and insert (plusp (length insert)))))
    (if (= a z)
        (and inserts
             (or (< start a end)
                 (and (= a end) (not (member (aref insert 0) '(10 13))))))
        (let ((reach (if (< end (tuatara.text:rope-bytes rope)) (1+ end) end)))
          (and (< a reach) (> z start)
               (not (and (= z (1+ start)) (< 0 a start)
                         (= (1- a) (tuatara.text:rope-line-end rope (1- a)))
                         (or (not inserts) (member (aref insert (1- (length insert))) '(10 13))))))))))

(defun map-hidden-bounds (bounds batch rope)
  "BOUNDS, as HIDE-RANGES takes them, in ROPE, carried through BATCH, the
edits made to ROPE: a new vector, BOUNDS left as they were, and true as a
second value when an edit reached into one of them (HIDDEN-EDIT-P). A
stretch whose first newline was deleted with the whole lines shown before it
starts at the newline before those lines; stretches meeting are made one,
those emptied go. Only the stretches about the edits are mapped one by one:
O(stretches + edits × log edits)."
  (declare (type (simple-array fixnum (*)) bounds))
  (let* ((edits (tuatara.text:batch-edits batch))
         (n (length edits)))
    (if (zerop n)
        (values bounds nil)
        (let* ((low (tuatara.text:edit-start (svref edits 0)))
               (high (tuatara.text:edit-end (svref edits (1- n))))
               (deltas (tuatara.text:batch-deltas batch))
               (delta (aref deltas (1- (length deltas))))
               (out (make-array (length bounds) :element-type 'fixnum))
               (w 0) (k 0) (touched nil))
          (declare (type fixnum low high delta w k))
          (flet ((put (s e)
                   (declare (type fixnum s e))
                   (cond ((>= s e))
                         ((and (plusp w) (<= s (aref out (1- w))))
                          (setf (aref out (1- w)) (max e (aref out (1- w)))))
                         (t (setf (aref out w) s (aref out (1+ w)) e)
                            (incf w 2)))))
            (loop for i of-type fixnum from 0 below (length bounds) by 2
                  for s of-type fixnum = (aref bounds i)
                  for e of-type fixnum = (aref bounds (1+ i))
                  do (cond ((< e low) (put s e))
                           ((> s high) (put (+ s delta) (+ e delta)))
                           (t (loop while (and (< k n) (< (tuatara.text:edit-end (svref edits k)) s)) do (incf k))
                              (let ((ns (tuatara.text:map-position s batch :after))
                                    (ne (tuatara.text:map-position e batch :before)))
                                (loop for j from k below n
                                      for edit = (svref edits j)
                                      while (<= (tuatara.text:edit-start edit) e)
                                      do (cond ((hidden-edit-p edit s e rope) (setf touched t))
                                               ;; the newline it starts at went with the lines before it
                                               ((and (< (tuatara.text:edit-start edit) s)
                                                     (= (tuatara.text:edit-end edit) (1+ s)))
                                                (setf ns (1- ns)))))
                                (put ns ne))))))
          (values (subseq out 0 w) touched)))))

(defun hidden-bounds (buffer)
  "The stretches search filters hide of BUFFER's text (HIDE-RANGES), as
offsets into its text as it is now: a vector of fixnums, the start and end of
each in turn; NIL when there are none. Carried through each change once,
when next asked for (MAP-HIDDEN-BOUNDS); a change of the whole text shows
everything again."
  (let ((hidden (tuatara.buffer:buffer-local buffer :hidden-ranges)))
    (when hidden
      (let ((now (tuatara.buffer:buffer-version buffer)))
        (loop until (eq (hidden-ranges-version hidden) now)
              do (let* ((before (hidden-ranges-version hidden))
                        (next (tuatara.text:version-next before))
                        (batch (tuatara.text:version-batch next)))
                   (if (tuatara.text:batch-swap batch)
                       (setf (hidden-ranges-bounds hidden) (make-array 0 :element-type 'fixnum)
                             (hidden-ranges-touched hidden) t)
                       (multiple-value-bind (bounds touched)
                           (map-hidden-bounds (hidden-ranges-bounds hidden) batch (tuatara.text:version-rope before))
                         (setf (hidden-ranges-bounds hidden) bounds)
                         (when touched (setf (hidden-ranges-touched hidden) t))))
                   (setf (hidden-ranges-version hidden) next)))
        (let ((bounds (hidden-ranges-bounds hidden)))
          (and (plusp (length bounds)) bounds))))))

(defun hidden-touched-p (buffer)
  "True when a change reached into what search filters hide of BUFFER
(HIDE-RANGES) since they were last set, or replaced its whole text."
  (let ((hidden (tuatara.buffer:buffer-local buffer :hidden-ranges)))
    (and hidden (progn (hidden-bounds buffer) (hidden-ranges-touched hidden)))))

(defun shown-parts (buffer start end)
  "The parts of BUFFER's text from START below END that search filters do not
hide (HIDE-RANGES), a list of (START . END) in order: all of it when they
hide none of it. The lines they hide are left out whole, each with its line
end, so that a part ending with a line shown ends after that line's newline,
and the parts one after another are the lines shown. O(log n) in the
stretches hidden, and the stretches inside."
  (let ((bounds (hidden-bounds buffer)))
    (if (null bounds)
        (list (cons start end))
        (let ((size (tuatara.buffer:buffer-size buffer)) (parts '()) (at start))
          (loop for k from (hidden-index bounds (lambda (s e) (declare (ignore s)) (>= e start)))
                for stretch = (hidden-stretch bounds k)
                ;; the lines hidden: from after the newline it starts at, to after the one it ends at
                for from = (and stretch (1+ (car stretch)))
                while (and stretch (< from end))
                do (when (< at from) (push (cons at from) parts))
                   (setf at (max at (min size (1+ (cdr stretch))))))
          (when (< at end) (push (cons at end) parts))
          (nreverse parts)))))

(defun shown-ranges (rows &key (join 4096) shown buffer)
  "The octets ROWS show, as a list of (START . END) in order, rows less than
JOIN octets apart joined: a screen of lines is one range, a line of megabytes
cut short on screen (a minified script) only what of it shows. With SHOWN, a
row's range ends where what it shows does, never at its line's end. With
BUFFER, the buffer the rows show, what its search filters and its folds hide
in the rows and between them is left out (SHOWN-PARTS, UNFOLDED-PARTS)."
  (let ((ranges '()))
    (loop for row across rows
          for bytes = (row-bytes row)
          ;; the first character shown, past what a line scrolled sideways hides and
          ;; what stands for no character before it (Org's indented view)
          for start = (or (find-if-not #'minusp bytes) (row-start row))
          for end = (max start (row-shown-end row) (if (or shown (row-truncated-p row))
                                                                      0
                                                                      (row-end row)))
          do (cond ((and ranges (< (- start (cdr (first ranges))) join))
                    (setf (cdr (first ranges)) (max end (cdr (first ranges)))))
                   ;; a row showing nothing, past the end of the text, adds nothing
                   ((< start end) (push (cons start end) ranges))))
    (if buffer
        (loop for (start . end) in (nreverse ranges)
              nconc (loop for (s . e) in (shown-parts buffer start end) nconc (unfolded-parts buffer s e)))
        (nreverse ranges))))

(defun unfolded-parts (buffer start end)
  "The parts of BUFFER's text from START below END that its folds do not
hide, a list of (START . END) in order. O(log n) in the folds, and the folds
inside."
  (let ((folds (if (tuatara.buffer:buffer-local buffer :folds) (fold-vector buffer) #()))
        (parts '()) (at start))
    (loop for k from (fold-index folds (lambda (fold) (> (cdr fold) start))) below (length folds)
          for (s . e) = (svref folds k)
          while (< s end)
          do (when (< at s) (push (cons at s) parts))
             (setf at (max at e)))
    (when (< at end) (push (cons at end) parts))
    (nreverse parts)))

(defun lines-left-out (buffer start end parts)
  "How many lines of BUFFER from START below END are not in PARTS, the parts
of it shown as SHOWN-PARTS gives them: the lines search filters hide there."
  (let ((count 0) (at start))
    (flet ((gap (a b)
             (when (< a b)
               (incf count (1+ (- (tuatara.buffer:buffer-line-number buffer (1- b))
                                  (tuatara.buffer:buffer-line-number buffer a)))))))
      (loop for (s . e) in parts do (gap at s) (setf at e))
      (gap at end))
    count))

(defun buffer-hides-p (buffer)
  "True when any of BUFFER's text is hidden from its windows: by folds, or
by search filters."
  (and (or (tuatara.buffer:buffer-local buffer :folds) (hidden-bounds buffer)) t))

(defun hidden-index (bounds test)
  "The index of the first stretch of BOUNDS (as HIDE-RANGES takes them) for
which TEST, a function of its start and end, holds, TEST holding for every
one after one it holds for; the number of stretches when none. O(log n)."
  (declare (type (simple-array fixnum (*)) bounds) (type function test))
  (tuatara.runtime:bisect (k 0 (ash (length bounds) -1))
                          (not (funcall test (aref bounds (* 2 k)) (aref bounds (1+ (* 2 k)))))))

(defun hidden-stretch (bounds k)
  "Stretch K of BOUNDS as (START . END), or NIL past the last."
  (and (< (* 2 k) (length bounds)) (cons (aref bounds (* 2 k)) (aref bounds (1+ (* 2 k))))))

(defun hidden-around (buffer position)
  "The stretch hidden by search filters (HIDE-RANGES) that POSITION of
BUFFER is inside of, as (START . END): after its start, up to its end and
at it, since what is typed there would go on the last line it hides; NIL
when there is none."
  (let ((bounds (hidden-bounds buffer)))
    (when bounds
      (let ((stretch (hidden-stretch bounds (hidden-index bounds (lambda (s e) (declare (ignore s)) (>= e position))))))
        (and stretch (< (car stretch) position) stretch)))))

(defun fold-at (buffer position)
  "The fold of BUFFER holding POSITION, inside it (not at its start), as
\(START . END), or NIL (FOLD-HIDING). O(log n) in the folds."
  (let ((fold (fold-hiding buffer position)))
    (and fold (< (car fold) position) fold)))

(defun point-out-of-folds (buffer previous)
  "Move BUFFER's point out of the fold it is inside, if any: when it came from
before the fold (PREVIOUS, where it was), on past it, to the start of the line
after it; else back to its start, at the end of the line it hides the rest of.
Out of the lines search filters hide (HIDDEN-AROUND) the same way: on to the
line after them, or back to the end of the line shown before them, which is
also where point goes when no line follows them."
  (let ((hidden (hidden-around buffer (tuatara.buffer:buffer-point buffer))))
    (when hidden
      (setf (tuatara.buffer:buffer-point buffer)
            (if (and previous (<= previous (car hidden)) (< (cdr hidden) (tuatara.buffer:buffer-size buffer)))
                (1+ (cdr hidden))
                (car hidden)))))
  (let* ((point (tuatara.buffer:buffer-point buffer))
         (fold (and (tuatara.buffer:buffer-local buffer :folds) (fold-at buffer point))))
    (when fold
      (setf (tuatara.buffer:buffer-point buffer)
            (if (and previous (<= previous (car fold)))
                (let* ((rope (tuatara.buffer:buffer-text buffer))
                       (size (tuatara.text:rope-bytes rope))
                       (end (cdr fold)))
                  (cond ((and (< end size) (= end (tuatara.text:rope-line-end rope end))) (1+ end))
                        ((tuatara.text:rope-crlf-p rope end)
                         (+ end 2))
                        (t end)))
                (car fold))))))

;;; Finding folds by halving, and the lines a window shows

(defun fold-vector (buffer)
  "BUFFER's folds (BUFFER-FOLDS) as a vector, in order, to find one by halving;
kept until they change."
  (let ((folds (buffer-folds buffer))
        (kept (tuatara.buffer:buffer-local buffer 'folds-vector)))
    (if (and kept (eq (car kept) folds))
        (cdr kept)
        (let ((vector (coerce folds 'simple-vector)))
          (setf (tuatara.buffer:buffer-local buffer 'folds-vector) (cons folds vector))
          vector))))

(defun fold-index (folds test)
  "The index of the first of FOLDS, a vector of them in order, for which TEST
holds, TEST holding for every fold after one it holds for; the length of FOLDS
when it holds for none. Halving: O(log n)."
  (tuatara.runtime:bisect (mid 0 (length folds)) (not (funcall test (svref folds mid)))))

(defun fold-hiding (buffer position &optional (hidden t))
  "The fold of BUFFER that hides the octet at POSITION, as (START . END), or
NIL: its start is hidden too, its end is not. Unless HIDDEN is NIL, the lines
search filters hide are such folds too (HIDE-RANGES), true as a second value
then. O(log n) in the folds."
  (or (when (tuatara.buffer:buffer-local buffer :folds)
        (let* ((folds (fold-vector buffer))
               (i (fold-index folds (lambda (fold) (> (cdr fold) position)))))
          (and (< i (length folds)) (<= (car (svref folds i)) position) (svref folds i))))
      (let ((bounds (and hidden (hidden-bounds buffer))))
        (when bounds
          (let ((stretch (hidden-stretch bounds (hidden-index bounds (lambda (s e) (declare (ignore s)) (> e position))))))
            (and stretch (<= (car stretch) position) (values stretch t)))))))

(defun fold-from (buffer position)
  "The first fold of BUFFER starting at POSITION or after it, as (START .
END), or NIL; the lines search filters hide count as folds."
  (let ((fold (when (tuatara.buffer:buffer-local buffer :folds)
                (let* ((folds (fold-vector buffer))
                       (i (fold-index folds (lambda (fold) (>= (car fold) position)))))
                  (and (< i (length folds)) (svref folds i)))))
        (stretch (let ((bounds (hidden-bounds buffer)))
                   (and bounds (hidden-stretch bounds (hidden-index bounds (lambda (s e) (declare (ignore e)) (>= s position))))))))
    (if (and fold stretch) (if (< (car stretch) (car fold)) stretch fold) (or fold stretch))))

(defun fold-before (buffer position)
  "The last fold of BUFFER starting before POSITION, as (START . END), or
NIL; the lines search filters hide count as folds."
  (let ((fold (when (tuatara.buffer:buffer-local buffer :folds)
                (let* ((folds (fold-vector buffer))
                       (i (fold-index folds (lambda (fold) (>= (car fold) position)))))
                  (and (plusp i) (svref folds (1- i))))))
        (stretch (let ((bounds (hidden-bounds buffer)))
                   (when bounds
                     (let ((k (hidden-index bounds (lambda (s e) (declare (ignore e)) (>= s position)))))
                       (and (plusp k) (hidden-stretch bounds (1- k))))))))
    (if (and fold stretch) (if (> (car stretch) (car fold)) stretch fold) (or fold stretch))))

(defstruct (fold-set (:constructor make-fold-set (folds bounds rope)) (:copier nil))
  "A buffer's folds as they stand at a moment (FOLDS-NOW): FOLDS, a vector of (START
. END) in order; BOUNDS, the stretches search filters hide, as HIDE-RANGES
takes them, or NIL; ROPE, the text they are in."
  (folds #() :type simple-vector) bounds rope)

(setf (documentation 'fold-set-p 'function) "True when OBJECT is a FOLD-SET."
      (documentation 'fold-set-folds 'function) "The folds of a FOLD-SET, a vector of (START . END) in order."
      (documentation 'fold-set-bounds 'function) "The stretches search filters hide in a FOLD-SET, or NIL."
      (documentation 'fold-set-rope 'function) "The text a FOLD-SET's folds are in.")

(defun folds-cut (now start end)
  "The first fold of NOW, a FOLD-SET as FOLDS-NOW gives it, that deleting the
octets from START below END would cut into, taking some of the text it hides
but not all of it, as (START . END), or NIL when that deletion takes every fold it touches
whole or touches none. A stretch search filters hide is cut by any deletion
that would change what it hides (HIDDEN-EDIT-P), whole or not, :HIDDEN as a
second value then. Only the folds in that stretch are looked at: O(log n)
in the folds."
  (when (< start end)
    (let* ((folds (fold-set-folds now))
           (i (fold-index folds (lambda (fold) (> (cdr fold) start)))))
      (or (loop for k from i below (length folds)
                for fold = (svref folds k)
                while (< (car fold) end)
                unless (and (<= start (car fold)) (<= (cdr fold) end)) return fold)
          (let ((bounds (fold-set-bounds now)))
            (when bounds
              (let ((edit (tuatara.text:make-edit start end)))
                (loop for k from (hidden-index bounds (lambda (s e) (declare (ignore s)) (>= (1+ e) start)))
                      for stretch = (hidden-stretch bounds k)
                      while (and stretch (< (car stretch) end))
                      when (hidden-edit-p edit (car stretch) (cdr stretch) (fold-set-rope now))
                        return (values stretch :hidden)))))))))

(defun folds-hiding (now start end)
  "The first fold of NOW, a FOLD-SET as FOLDS-NOW gives it, that hides any of
the octets from START below END, as (START . END), the lines search filters hide counted too; for
an empty stretch, START equal to END, the fold START is inside of (not at its
start). NIL when all of the stretch is shown. O(log n) in the folds."
  (let* ((folds (fold-set-folds now))
         (i (fold-index folds (lambda (fold) (> (cdr fold) start)))))
    (or (and (< i (length folds)) (< (car (svref folds i)) end) (svref folds i))
        (let ((bounds (fold-set-bounds now)))
          (when bounds
            (let ((stretch (hidden-stretch bounds (hidden-index bounds (lambda (s e) (declare (ignore s)) (> e start))))))
              (and stretch (< (car stretch) end) stretch)))))))

(defun hold-folds (buffer spans)
  "Have BUFFER's folds keep hidden what is written at the edges of those of
SPANS (a sequence, each (START . END) or (START END)) that a fold hides any of
(FOLDS-HIDING): a fold starting where such a stretch starts stays before text
put in there, and one ending where it ends takes text put in there in. Returns
the folds' ends so changed, for RELEASE-FOLDS to put back once the writing is
done; NIL when BUFFER has no folds. O(f + s) in the folds and the stretches."
  (let ((folds (folds-now buffer)))
    (when folds
      (let ((starts (make-hash-table)) (ends (make-hash-table))
            (version (tuatara.buffer:buffer-version buffer))
            (changed '()))
        (map nil (lambda (span)
                   (let ((s (car span)) (e (if (consp (cdr span)) (cadr span) (cdr span))))
                     (when (folds-hiding folds s e)
                       (setf (gethash s starts) t (gethash e ends) t))))
             spans)
        (when (plusp (+ (hash-table-count starts) (hash-table-count ends)))
          (dolist (fold (tuatara.buffer:buffer-local buffer :folds))
            (destructuring-bind (start . end) fold
              (when (and (gethash (tuatara.text:anchor-position start version) starts)
                         (eq (tuatara.text:anchor-bias start) :after))
                (setf (tuatara.text:anchor-bias start) :before)
                (push start changed))
              (when (and (gethash (tuatara.text:anchor-position end version) ends)
                         (eq (tuatara.text:anchor-bias end) :before))
                (setf (tuatara.text:anchor-bias end) :after)
                (push end changed)))))
        changed))))

(defun release-folds (buffer held)
  "Put back the ends of BUFFER's folds that HOLD-FOLDS changed, HELD, now that
what was written at them is in BUFFER's text: a fold's start goes after text
put in at it again, its end stays before it."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (dolist (anchor held)
      (tuatara.text:anchor-position anchor version)
      (setf (tuatara.text:anchor-bias anchor) (if (eq (tuatara.text:anchor-bias anchor) :before) :after :before)))))

(defun folds-now (buffer)
  "BUFFER's folds as they stand now, the lines search filters hide among
them, a FOLD-SET for FOLDS-CUT and FOLDS-HIDING, and for a job reading
BUFFER's text as it is now to check what it would delete against; NIL when
BUFFER has none. Never to be changed in place."
  (let ((folds (if (tuatara.buffer:buffer-local buffer :folds) (fold-vector buffer) #()))
        (bounds (hidden-bounds buffer)))
    (when (or bounds (plusp (length folds)))
      (make-fold-set folds bounds (tuatara.buffer:buffer-text buffer)))))

(defun visible-line-start (buffer position &key (hidden t))
  "The start of the line a window shows POSITION of BUFFER on: the start of
its line, or where the folds that hide that line's start leave off, the line
whose end they hide. With HIDDEN NIL the lines search filters hide are not
taken with the line before them, as when that line is taken by itself."
  (let ((start (tuatara.buffer:buffer-line-start buffer position)))
    (loop for fold = (and (plusp start) (fold-hiding buffer (1- start) hidden))
          while fold
          do (setf start (tuatara.buffer:buffer-line-start buffer (car fold))))
    start))

(defun visible-line-end (buffer position &key (hidden t))
  "The end of the line a window shows POSITION of BUFFER on, before its
newline: the end of its line, or, when a fold hides that, the end of the line
the fold ends on, and so on past each fold that hides one. With HIDDEN NIL
the lines search filters hide are not taken with the line before them."
  (let ((end (tuatara.buffer:buffer-line-end buffer position)))
    (loop for fold = (fold-hiding buffer end hidden)
          while fold
          do (setf end (tuatara.buffer:buffer-line-end buffer (cdr fold))))
    end))
