;;;; test/jobs.lisp --- Jobs in the background one after another: the queue, M-x list-jobs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test jobs
  :description "A job asked for while another runs in its buffer waits its turn instead of being
refused, first in first out, on the text as the one before leaves it; the mode line says how far
the one running has got and counts those waiting, their text is painted, and M-x list-jobs lists
the jobs and drops those marked.")

(defun waiting-spans (frontend)
  "The (START END) spans in :job-waiting of every frame FRONTEND painted."
  (loop for (spans) in (reverse (tuatara.frontend.fake::fake-face-history frontend))
        append (loop for row across spans
                     append (loop for (start end face) in row
                                  when (member :job-waiting (if (listp face) face (list face)))
                                    collect (list start end)))))

(define-test (jobs one-after-another)
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2))))
    (with-background ()
      ;; a second replacement waits for the first, and works on its result
      (multiple-value-bind (f editor) (run-keys "r r % a / b RET r r % b / c RET" :text "a a a" :width 150)
        (true (tuatara.frontend.fake:fake-frame-showing f "Queued, first in line: Replacing"))
        ;; the mode line: P lit, not what either does, nor how far or how many
        (true (processing-shown f "TUATARA["))
        (false (tuatara.frontend.fake:fake-frame-showing f "TUATARA[" "queued"))
        (false (tuatara.frontend.fake:fake-frame-showing f "TUATARA[" "Replacing"))
        (is string= "c c c" (session-text editor)))
      ;; what it will change is painted while it waits: its matches alone
      (multiple-value-bind (f editor) (run-keys "r r % a / b RET r r % x / y RET" :text "a x a x" :width 150)
        (let ((spans (waiting-spans f)))
          (true spans)
          (true (every (lambda (span) (= 1 (- (second span) (first span)))) spans)))
        (is string= "b y b y" (session-text editor)))
      ;; a sort waits for a replacement too, and sorts what it leaves
      (multiple-value-bind (f editor)
          (run-keys "r r % 1 / 9 RET % M-x s o r t - l i n e s RET" :text (format nil "b1~%a2~%c1~%") :width 150)
        (true (tuatara.frontend.fake:fake-frame-showing f "Queued, first in line: Sorting"))
        (is string= (format nil "a2~%b9~%c9~%") (session-text editor))))))

(define-test (jobs mode-line-of-its-buffer)
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.3))))
    (with-background ()
      ;; a job says nothing in the mode line of another buffer
      (let ((f (run-keys "r r % a / b RET C-x b o t h e r C-j" :text "a a a" :width 150)))
        (true (processing-shown f "TUATARA[" "*scratch*"))
        ;; faint in the other, in the same place
        (true (tuatara.frontend.fake:fake-frame-showing f "TUATARA[" " P [-] other"))
        (false (processing-shown f "TUATARA[" "other"))))))

(define-test (jobs sign-and-keys)
  ;; P stands first, faint while nothing runs, with or without directories kept read;
  ;; NIL shows nothing; C-x j lists the jobs, C-x J stops the buffer's
  (let ((f (run-keys "" :width 80)))
    (true (tuatara.frontend.fake:fake-frame-showing f " P [-] *scratch*"))
    (false (processing-shown f)))
  (let ((tuatara-user:*processing-indicator* nil))
    (let ((f (run-keys "" :width 80)))
      (true (tuatara.frontend.fake:fake-frame-showing f " [-] *scratch*"))))
  (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x j" :width 80) "No jobs in the background"))
  (true (tuatara.frontend.fake:fake-frame-showing (run-keys "C-x J" :width 80) "Nothing is running in this buffer")))

(define-test (jobs list-and-drop)
  ;; a list left by an earlier test would keep its point
  (let ((list (tuatara.buffer:find-buffer "*Jobs*"))) (when list (tuatara.buffer:kill-buffer list)))
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.3))))
    (with-background ()
      ;; the list shows the job running and the one waiting; d then x drops it, after asking
      (multiple-value-bind (f editor)
          (run-keys "r r % a / b RET r r % b / c RET M-x l i s t - j o b s RET j d x y RET q"
                    :text "a a a" :width 150 :height 12)
        (true (tuatara.frontend.fake:fake-frame-showing f "Buffer" "State" "Job"))
        (true (tuatara.frontend.fake:fake-frame-showing f "*scratch*" "Replacing" "Replacing a with b in the buffer"))
        (true (tuatara.frontend.fake:fake-frame-showing f "waiting, first in line" "Replacing b with c in the buffer"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Dropped 1 waiting job"))
        (is string= "b b b" (session-text editor)))
      ;; x acts on those marked for deletion only; D on those marked (m), as in every list
      (let ((list (tuatara.buffer:find-buffer "*Jobs*"))) (when list (tuatara.buffer:kill-buffer list)))
      (multiple-value-bind (f editor)
          (run-keys "r r % a / b RET r r % b / c RET M-x l i s t - j o b s RET j m x D y RET q"
                    :text "a a a" :width 150 :height 12)
        (true (tuatara.frontend.fake:fake-frame-showing f "No jobs marked for deletion"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Dropped 1 waiting job"))
        (is string= "b b b" (session-text editor)))
      ;; nothing in the background: the list says so
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "M-x l i s t - j o b s RET" :text "x" :width 150 :height 12)
                                                      "No jobs in the background")))))

(define-test (jobs reports-say-alike)
  (is string= "in 1.5 s" (tuatara.defaults::job-done-words 1.5 0))
  (is string= "in 2.0 s; 1,203 kept as you edited them" (tuatara.defaults::job-done-words 2 1203))
  (is string= "in 2.0 s: 5 lines changed; 1 kept as you edited them"
      (tuatara.defaults::job-done-words 2 1 "5 lines changed"))
  ;; each kind of job says it so
  (is string= "Put the region in upper case in 2.0 s: 5 lines changed; 1 kept as you edited them"
      (funcall (tuatara.defaults::case-report :up) nil :done 5 2 1))
  (is string= "Commented 3 lines in 2.0 s" (funcall (tuatara.defaults::comment-job-report t) nil :done 3 2 0))
  (is string= "Uncommented 1 line in 2.0 s; 2 kept as you edited them"
      (funcall (tuatara.defaults::comment-job-report nil) nil :done 1 2 2))
  (is string= "Joined the lines in 0.5 s" (funcall (tuatara.defaults::join-report) nil :done 9 0.5 0))
  ;; counts of matches, a refused pattern, the lines a job is done to
  (is string= "1 match on 1 line" (tuatara.defaults::matches-on-lines-words 1 1))
  (is string= "12,345 matches on 2 lines" (tuatara.defaults::matches-on-lines-words 12345 2))
  (is string= "0 matches on 0 lines" (tuatara.defaults::matches-on-lines-words 0 0))
  (is string= "incomplete; finish it, or C-g gives up" (tuatara.defaults::refused-pattern-words "incomplete" "C-g"))
  (is string= "unmatched ); mend it, or C-g gives up" (tuatara.defaults::refused-pattern-words "unmatched )" "C-g"))
  (let ((b (tuatara.buffer:make-buffer "*lines done*" :text (format nil "a~%b~%"))))
    (unwind-protect
         (progn (is string= "Sorting the whole buffer" (tuatara.defaults::lines-done-words "Sorting" b 0 4))
                (is string= "Sorting the selection's lines" (tuatara.defaults::lines-done-words "Sorting" b 0 2))
                (is string= "Sorting the selection's lines" (tuatara.defaults::lines-done-words "Sorting" b 2 4)))
      (tuatara.buffer:kill-buffer b))))
