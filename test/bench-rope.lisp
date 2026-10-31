;;;; test/bench-rope.lisp --- Rope benchmarks against their budgets.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every budget is twice the slowest time seen in the speed reference
;;;; environment (.project/tools/speed/speed-env.sh: 16 GB, the 358H's 16
;;;; threads pinned) over five runs of each benchmark in one process and four
;;;; fresh processes, rounded up; the margin leaves room for other work running
;;;; on the same machine.  The single-position lookups take microseconds, below
;;;; what the clock tells apart from a context switch, so they keep 0.05 ms.
;;;; Set a budget again from such runs, never by raising it to make a failure
;;;; pass.

(in-package #:tuatara.tests)

(defvar *big-octets* nil "27.5 MB, 500,000-line reference text, built on first use.")
(defvar *big-rope* nil "The reference text as a rope.")
(defvar *big-edits* nil "One single-octet insertion per line of the reference text, 500,000 edits.")

(defun big-fixture ()
  "Build the reference text, rope and edit batch once; return the rope."
  (unless *big-rope*
    (let* ((line (to-octets (format nil "The quick brown fox jumps over the lazy dog 0123456789~%")))
           (lines 500000)
           (octets (make-array (* lines (length line)) :element-type '(unsigned-byte 8))))
      (dotimes (i lines) (replace octets line :start1 (* i (length line))))
      (setf *big-octets* octets
            *big-rope* (tuatara.text:octets->rope octets)
            *big-edits* (let ((ins (to-octets "x")))
                          (coerce (loop for i from 0 below lines
                                        for p = (+ (* i (length line)) 4)
                                        collect (tuatara.text:make-edit p p ins))
                                  'simple-vector)))))
  *big-rope*)

(define-benchmark rope-batch-insert-500k (:budget-ms 50 :reps 9)
  (tuatara.text:apply-edits (big-fixture) *big-edits*))

(define-benchmark rope-single-insert-in-27mb (:budget-ms 0.05 :reps 20)
  (tuatara.text:rope-insert (big-fixture) 13750000 "x"))

(define-benchmark rope-char->byte-in-27mb (:budget-ms 0.05 :reps 20)
  (tuatara.text:char->byte (big-fixture) 13000000))

(define-benchmark rope-line->byte-in-27mb (:budget-ms 0.05 :reps 20)
  (tuatara.text:line->byte (big-fixture) 250000))

(define-benchmark octets->rope-27mb (:budget-ms 25 :reps 3)
  (progn (big-fixture) (tuatara.text:octets->rope *big-octets*)))

(define-benchmark rope-octets-27mb (:reps 3)
  (tuatara.text:rope-octets (big-fixture)))

(defvar *big-file* nil "The reference text written to a temporary file, for file-backed benchmarks.")
(defvar *big-file-rope* nil "The reference file opened and indexed.")
(defvar *big-file-mapping* nil "The mapping behind *BIG-FILE-ROPE*.")

(defun big-file-fixture ()
  "Write the reference text to a temporary file once, open and index it; return the indexed rope."
  (unless *big-file-rope*
    (big-fixture)
    (setf *big-file* (write-temp-file *big-octets*))
    (multiple-value-bind (rope mapping) (tuatara.text:open-file-rope *big-file*)
      (setf *big-file-mapping* mapping
            *big-file-rope* (tuatara.text:index-rope rope))))
  *big-file-rope*)

(define-benchmark open-file-rope-27mb (:budget-ms 0.15 :reps 5)
  (progn (big-file-fixture) (tuatara.text:open-file-rope *big-file*)))

(define-benchmark index-rope-27mb-all-cores (:budget-ms 3 :reps 5)
  (progn (big-file-fixture) (tuatara.text:index-rope (tuatara.text:open-file-rope *big-file*))))

(define-benchmark index-rope-27mb-one-thread (:reps 3)
  (progn (big-file-fixture) (tuatara.text:index-rope (tuatara.text:open-file-rope *big-file*) :threads 1)))

(define-benchmark file-rope-single-insert (:budget-ms 0.15 :reps 20)
  (tuatara.text:rope-insert (big-file-fixture) 13750000 "x"))

(define-benchmark file-rope-batch-insert-500k (:budget-ms 75 :reps 9)
  (tuatara.text:apply-edits (big-file-fixture) *big-edits*))

(define-benchmark file-rope-octets-27mb (:reps 3)
  (tuatara.text:rope-octets (big-file-fixture)))

(define-test (rope benchmarks-within-budget)
  (true (run-benchmarks) "a rope benchmark exceeded its budget; see the table above"))

(define-benchmark search-forward-rare-27mb (:budget-ms 25 :reps 3)
  (tuatara.syntax:search-forward (big-fixture) "zzz-not-there"))

(define-benchmark search-all-common-27mb-all-cores (:budget-ms 20 :reps 7)
  (tuatara.syntax:search-all (big-fixture) "fox"))

(define-benchmark search-all-common-27mb-one-thread (:reps 3)
  (tuatara.syntax:search-all (big-fixture) "fox" :threads 1))

(define-benchmark regex-search-forward-rare-27mb (:budget-ms 13 :reps 3)
  ;; the prefilter skips to "zzz" at literal speed: as fast as a literal search
  (tuatara.syntax:regex-search-forward (big-fixture) "zzz-\\d+-not-there"))

(define-benchmark regex-search-forward-class-27mb (:reps 3)
  ;; no text every match holds: every position is tried, in parallel
  (tuatara.syntax:regex-search-forward (big-fixture) "[qz]{3}\\d"))

(define-benchmark regex-search-all-27mb (:reps 1)
  (tuatara.syntax:regex-search-all (big-fixture) "f[a-z]x" :limit 1000000))

(defvar *bracketed-rope* nil "The reference text after an opening bracket that has no partner.")

(define-benchmark rope-bracket-scan-27mb (:budget-ms 60 :reps 5)
  ;; a stray bracket: the whole 27.5 MB looked through for its partner
  (tuatara.text:rope-bracket-match (or *bracketed-rope*
                                       (setf *bracketed-rope* (tuatara.text:rope-insert (big-fixture) 0 "(")))
                                   0))
