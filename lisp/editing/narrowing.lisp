;;;; lisp/editing/narrowing.lisp --- Narrowing a buffer to a part of it, and widening it again.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A narrowed buffer shows only a part of its text, and the commands run in
;;;; it reach only that part: motion, search and replacement, editing, the
;;;; selection. Its edges are anchors, kept by the buffer
;;;; (TUATARA.BUFFER:NARROW-BUFFER), so that they move with the text as it is
;;;; edited. Saving still writes the whole file. The commands here narrow to
;;;; the selection or to the section around point (an Org subtree, the
;;;; definition at point in code) and widen again; the mode line says Narrow
;;;; meanwhile.

(in-package #:tuatara.defaults)

(defgeneric mode-section-bounds (mode buffer position)
  (:documentation "The section of BUFFER, in MODE, that POSITION is in, for
\\[narrow-to-section]: its start and end as two values, octets; NIL when
there is none there. In code, the definition point is in, as
\\[beginning-of-defun] and \\[end-of-defun] find it, or the one after it
when point is between two; in Org, the subtree of the headline point is
under."))

(defun definition-bounds (buffer position)
  "The start and end of the definition of BUFFER that POSITION is in, or of
the one after it when POSITION is between two, as \\[beginning-of-defun] and
\\[end-of-defun] find them; NIL when there is none."
  (multiple-value-bind (start found) (beginning-of-defun-position buffer (tuatara.buffer:buffer-line-end buffer position) 1)
    (let ((end (and (eq found t) (end-of-defun-position buffer start 1))))
      (when (or (null end) (<= end position))
        ;; between two definitions, or before the first: the one after
        (multiple-value-bind (next found) (end-of-defun-position buffer position 1)
          (setf end (and found next)
                start (and end (beginning-of-defun-position buffer end 1)))))
      (and start end (< start end) (values start end)))))

(defmethod mode-section-bounds (mode buffer position)
  "The definition POSITION of BUFFER is in (DEFINITION-BOUNDS)."
  (declare (ignore mode))
  (definition-bounds buffer position))

(defun narrowed-words (buffer)
  "Words for the part of BUFFER shown while it is narrowed: its lines, by
number from 1."
  (let ((first (tuatara.buffer:buffer-known-line-number buffer (tuatara.buffer:buffer-start buffer)))
        (last (tuatara.buffer:buffer-known-line-number buffer (max (tuatara.buffer:buffer-start buffer)
                                                                   (1- (tuatara.buffer:buffer-end buffer))))))
    (if (and first last)
        (if (= first last) (format nil "line ~:d" (1+ first)) (format nil "lines ~:d to ~:d" (1+ first) (1+ last)))
        "a part of it")))

(defun narrow-to (buffer start end)
  "Narrow BUFFER to its text from START below END, within the part it is
narrowed to already, its selection let go, and say so; refused in the hex
view, which shows every octet."
  (if (tuatara.display:hex-view-p buffer)
      (message "The hex view shows every octet: \\[hex-mode] leaves it, to narrow the text")
      (let ((start (tuatara.buffer:within-narrowing buffer start))
            (end (tuatara.buffer:within-narrowing buffer end)))
        (tuatara.buffer:narrow-buffer buffer start end)
        (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
        (message "Narrowed to ~a: \\[widen] shows all of it again" (narrowed-words buffer)))))

(define-command narrow-to-region ()
  "Show only the selected text, and keep every command to it: motion, search
and replacement, editing and selecting stop at its edges, and the rest of
the buffer is out of sight and out of reach until \\[widen]. Text typed at
either edge goes inside. The mode line says Narrow meanwhile. Saving writes
the whole file still. Without a selection, does nothing and says so."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (values-list (selected-region b))
      (if (null start)
          (message "No selection: narrow-to-region was not run")
          (narrow-to b start end)))))

(define-command narrow-to-section ()
  "Show only the section point is in, and keep every command to it, as
\\[narrow-to-region] does for the selection: in Org, the subtree of the
headline point is under, the headline with it; in code, the definition
point is in (or the one after it), as \\[beginning-of-defun] and
\\[end-of-defun] find it. \\[widen] shows all of the buffer again."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (mode-section-bounds (buffer-major-mode b) b (point))
      (if (null start)
          (message "No section here to narrow to")
          (narrow-to b start end)))))

(define-command widen ()
  "Show all of the buffer again after \\[narrow-to-region] or
\\[narrow-to-section], every command reaching all of it, point staying where
it is."
  (let ((b (current-buffer)))
    (if (tuatara.buffer:buffer-narrowed-p b)
        (progn (tuatara.buffer:widen-buffer b)
               (message "Showing all of ~a" (tuatara.buffer:buffer-name b)))
        (message "~a is not narrowed" (tuatara.buffer:buffer-name b)))))

(defun narrowed-mode-line (editor buffer)
  "Mode line function, just before the buffer's name: Narrow, in
:mode-line-narrowed, while the buffer is narrowed (\\[narrow-to-region])."
  (declare (ignore editor))
  (and (tuatara.buffer:buffer-narrowed-p buffer) (list " Narrow " :mode-line-narrowed)))

(defun install-narrowing ()
  "Have the mode line say Narrow before the name of a narrowed buffer."
  (pushnew 'narrowed-mode-line *mode-line-name-functions*))
