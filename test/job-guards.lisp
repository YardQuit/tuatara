;;;; test/job-guards.lisp --- An edit that would spoil a running job's work asks first.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test job-guards
  :description "A command's edit of the text a sort or dynamic blocks being made work on
asks once whether to stop the job: no leaves the text as it was and the job goes on to its result,
yes stops it and makes the edit. Edits elsewhere, and edits while a replacement, a filter or a sum
runs, ask nothing; undo and redo ask alike; edits made outside a command leave the job to give up by itself.")

(defvar *held-go* nil
  "True once the work held by WITH-HELD-SORTS, or the dynamic block writer \"held\", may go on.")

(defmacro with-held-sorts (() &body body)
  "Run BODY with every sort or filter running in the background held before it
reads its lines, until *HELD-GO* turns true or it is asked to stop, ten seconds
at most; a command starting one does not wait for it."
  `(progn
     (setf *held-go* nil)
     (sb-int:encapsulate 'tuatara.defaults::scan-lines 'job-guards
                         (lambda (f &rest arguments)
                           (let ((job tuatara.runtime:*job*))
                             (when job
                               (loop repeat 1000
                                     until (or *held-go* (car (tuatara.runtime:job-cancel job)))
                                     do (sleep 0.01))))
                           (apply f arguments)))
     (unwind-protect (let ((tuatara.defaults::*sort-wait* 0)) ,@body)
       (setf *held-go* t)
       (sb-int:unencapsulate 'tuatara.defaults::scan-lines 'job-guards))))

(defun go-on ()
  "A turn letting the held work go on (*HELD-GO*)."
  (lambda () (setf *held-go* t)))

(defun run-held (turns &rest arguments)
  "RUN-KEYS with TURNS, then the held work let go on (GO-ON), held until then;
ARGUMENTS are RUN-KEYS's."
  (setf *held-go* nil)
  (apply #'run-keys (append turns (list (go-on))) arguments))

(defparameter *guard-lines* (lines-text "c" "b" "a" "end")
  "Three lines to sort, then a line left out of the selection.")

(defparameter *select-three* "C-SPC C-n C-n C-n"
  "Keys selecting the first three lines, from the top.")

(define-test (job-guards guarding-sorted-lines)
  (with-held-sorts ()
    ;; typed inside the lines being sorted, answered no: nothing typed, the sort goes on to its result
    (with-questions (prompts nil)
      (multiple-value-bind (f editor)
          (run-held (list (format nil "~a M-x s o r t - l i n e s RET C-g C-p C-p i x C-g" *select-three*))
                    :text *guard-lines* :width 120)
        (is = 1 (length prompts) "~s" prompts)
        (true (search "Sorting works on this text (" (first prompts)) "~s" prompts)
        (true (search "); stop sorting and change it?" (first prompts)) "~s" prompts)
        (true (tuatara.frontend.fake:fake-frame-showing f "Not changed: sorting goes on"))
        (is string= (lines-text "a" "b" "c" "end") (session-text editor))))
    ;; answered yes, at the question itself: the sort stops, the text typed is in
    (multiple-value-bind (f editor)
        (run-held (list (format nil "~a M-x s o r t - l i n e s RET C-g C-p C-p i x" *select-three*) :tick
                        "y C-g")
                  :text *guard-lines* :width 120)
      (true (tuatara.frontend.fake:fake-frame-showing f "Sorting works on this text ("))
      (true (tuatara.frontend.fake:fake-frame-showing f "Stopped: nothing was changed"))
      (is string= (lines-text "c" "xb" "a" "end") (session-text editor)))
    ;; typed on the line left out: nothing asked, the text typed kept, the lines sorted
    (with-questions (prompts t)
      (multiple-value-bind (f editor)
          (run-held (list (format nil "~a M-x s o r t - l i n e s RET C-g C-e i x C-g" *select-three*))
                    :text *guard-lines* :width 120)
        (declare (ignore f))
        (is equal '() prompts)
        (is string= (lines-text "a" "b" "c" "endx") (session-text editor))))
    ;; typed at the start of the first line: asked, as it would join a line sorted
    (with-questions (prompts nil)
      (run-held (list (format nil "~a M-x s o r t - l i n e s RET C-g C-p C-p C-p i x C-g" *select-three*))
                :text *guard-lines* :width 120)
      (is = 1 (length prompts) "~s" prompts))))

(define-test (job-guards asking-once-a-command)
  (with-held-sorts ()
    ;; a command editing twice inside the lines: one question; refused, nothing changed
    (with-questions (prompts nil)
      (multiple-value-bind (f editor)
          (run-held (list (format nil "~a M-x s o r t - l i n e s RET C-g" *select-three*)
                          (lambda ()
                            (let ((tuatara.command:*command-notes* (list '()))
                                  (b (tuatara.buffer:find-buffer "*scratch*")))
                              (ignore-errors (tuatara.buffer:buffer-insert b 2 "x"))
                              (ignore-errors (tuatara.buffer:buffer-insert b 4 "y"))))
                         )
                    :text *guard-lines* :width 120)
        (declare (ignore f))
        (is = 1 (length prompts) "~s" prompts)
        (is string= (lines-text "a" "b" "c" "end") (session-text editor))))))

(define-test (job-guards filtered-lines-carry-edits)
  ;; flush-lines, a job over lines, carries an edit inside its lines onto its result: nothing asked
  (let ((text (lines-text "keep" "drop" "keep too" "end"))
        (tuatara.defaults::*sort-wait* 0)
        (tuatara.defaults::*rewrite-piece-hook* (lambda () (loop repeat 1000 until *held-go* do (sleep 0.01)))))
    (with-questions (prompts t)
      (multiple-value-bind (f editor)
          (run-held (list (format nil "~a M-x f l u s h - l i n e s RET d r o p RET C-g C-p i x C-g" *select-three*))
                    :text text :width 120)
        (is equal '() prompts)
        (false (tuatara.frontend.fake:fake-frame-showing f "Filtering works on this text ("))
        (is string= (lines-text "keep" "xkeep too" "end") (session-text editor))))))

(define-test (job-guards guarding-undo)
  (with-held-sorts ()
    ;; undoing a change inside the lines being sorted asks too
    (with-questions (prompts nil)
      (multiple-value-bind (f editor)
          (run-held (list (format nil "C-n i x C-g C-p C-a ~a M-x s o r t - l i n e s RET C-g C-/" *select-three*)
                         )
                    :text *guard-lines* :width 120)
        (is = 1 (length prompts) "~s" prompts)
        (true (tuatara.frontend.fake:fake-frame-showing f "Not changed: sorting goes on"))
        (is string= (lines-text "a" "c" "xb" "end") (session-text editor)))))
  ;; an undo or redo refused part of the way leaves the text, and what there is to undo or redo, as they were
  (let ((b (tuatara.buffer:make-buffer "job-guards undoing" :text "abc")))
    (unwind-protect
         (flet ((refusing-at-zero (buffer edits)
                  (declare (ignore buffer))
                  (when (find 0 edits :key #'tuatara.text:edit-start)
                    (error "Not changed: refused"))))
           (tuatara.buffer:buffer-insert b 0 "x")
           (tuatara.buffer:buffer-insert b 4 "y" :group t)
           (is string= "xabcy" (tuatara.buffer:buffer-string b))
           (let ((tuatara.buffer:*before-edit-functions* (list #'refusing-at-zero)))
             (fail (tuatara.buffer:buffer-undo b))
             (is string= "xabcy" (tuatara.buffer:buffer-string b)))
           (tuatara.buffer:buffer-undo b)
           (is string= "abc" (tuatara.buffer:buffer-string b))
           (let ((tuatara.buffer:*before-edit-functions* (list #'refusing-at-zero)))
             (fail (tuatara.buffer:buffer-redo b))
             (is string= "abc" (tuatara.buffer:buffer-string b)))
           (tuatara.buffer:buffer-redo b)
           (is string= "xabcy" (tuatara.buffer:buffer-string b)))
      (tuatara.buffer:kill-buffer b))))

(define-test (job-guards edits-outside-commands)
  ;; an edit made outside a command asks nothing: the sort gives up its result by itself, and says so
  (with-held-sorts ()
    (with-questions (prompts t)
      (multiple-value-bind (f editor)
          (run-held (list (format nil "~a M-x s o r t - l i n e s RET C-g" *select-three*)
                          (lambda () (tuatara.buffer:buffer-insert (tuatara.buffer:find-buffer "*scratch*") 2 "x"))
                         )
                    :text *guard-lines* :width 120)
        (is equal '() prompts)
        (true (tuatara.frontend.fake:fake-frame-showing f "The lines changed while sorting them; nothing was changed"))
        (is string= (lines-text "c" "xb" "a" "end") (session-text editor))))))

(define-test (job-guards guarding-dynamic-blocks)
  (tuatara-user:define-dblock-writer "held" (params)
    (declare (ignore params))
    (loop repeat 1000 until *held-go* do (tuatara.defaults::dblock-progress 1/2) (sleep 0.01))
    (format nil "made~%"))
  (flet ((session (keys)
           (let ((tuatara.defaults::*dblock-wait* 0))
             (run-held (list (format nil "M-x o r g - m o d e RET j C-c C-c ~a" keys))
                       :text (format nil "top~%#+BEGIN: held~%old~%#+END:~%end~%") :width 120 :height 12))))
    ;; typed inside the block being made, answered no: nothing typed, the block made
    (with-questions (prompts nil)
      (multiple-value-bind (f editor) (session "j i x C-g")
        (is = 1 (length prompts) "~s" prompts)
        (true (search "Making blocks works on this text (" (first prompts)) "~s" prompts)
        (true (tuatara.frontend.fake:fake-frame-showing f "Not changed: making blocks goes on"))
        (is string= (format nil "top~%#+BEGIN: held~%made~%#+END:~%end~%") (session-text editor))))
    ;; answered yes: stopped, the block as it was with the text typed
    (with-questions (prompts t)
      (multiple-value-bind (f editor) (session "j i x C-g")
        (is = 1 (length prompts) "~s" prompts)
        (true (tuatara.frontend.fake:fake-frame-showing f "Stopped: the dynamic blocks are as they were"))
        (is string= (format nil "top~%#+BEGIN: held~%xold~%#+END:~%end~%") (session-text editor))))
    ;; its text changed outside a command: nothing asked, the block left as it is, and said
    (with-questions (prompts t)
      (setf *held-go* nil)
      (multiple-value-bind (f editor)
          (let ((tuatara.defaults::*dblock-wait* 0))
            (run-keys (list "M-x o r g - m o d e RET j C-c C-c"
                            (lambda () (tuatara.buffer:buffer-insert (tuatara.buffer:find-buffer "*scratch*") 18 "x"))
                            (go-on))
                      :text (format nil "top~%#+BEGIN: held~%old~%#+END:~%end~%") :width 120 :height 12))
        (is equal '() prompts)
        (true (tuatara.frontend.fake:fake-frame-showing f "changed while it was made"))
        (is string= (format nil "top~%#+BEGIN: held~%xold~%#+END:~%end~%") (session-text editor))))
    ;; typed below the block: nothing asked, the block made
    (with-questions (prompts t)
      (multiple-value-bind (f editor) (session "j j j i x C-g")
        (declare (ignore f))
        (is equal '() prompts)
        (is string= (format nil "top~%#+BEGIN: held~%made~%#+END:~%xend~%") (session-text editor))))))

(define-test (job-guards edits-carried-or-harmless)
  ;; a replacement running carries an edit inside its scope onto its result: nothing asked
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.3))))
    (with-background ()
      (with-questions (prompts t)
        (multiple-value-bind (f editor) (run-keys "r r % a / b RET C-g i x C-g" :text "a a a" :width 150)
          (declare (ignore f))
          (is equal '() prompts)
          (let ((text (session-text editor)))
            (true (find #\x text) "~s" text)
            (is = 3 (count #\b text) "~s" text))))))
  ;; a sum running reads the text as it was: nothing asked, the edit made
  (with-questions (prompts t)
    (setf *held-go* nil)
    (multiple-value-bind (f editor job)
        (job-session (lambda (b) (tuatara.defaults::%make-hash-job :buffer b :what "the text" :total 10)) "i x C-g"
                     :work (lambda (job) (declare (ignore job)) (loop repeat 1000 until *held-go* do (sleep 0.01)))
                     :after (lambda (job) (declare (ignore job)) (setf *held-go* t)))
      (declare (ignore f job))
      (is equal '() prompts)
      (is string= "xabc" (session-text editor)))))

(define-test (job-guards result-refused-meanwhile)
  ;; a buffer made read-only, or turned to the hex view, while a sort or a filter
  ;; runs: its result is left out, said calmly, as for a replacement
  (with-held-sorts ()
    (let ((text (lines-text "b" "a" "b")))
      (loop for (command toggle said) in '(("s o r t - l i n e s" "r e a d - o n l y - m o d e"
                                            "*scratch* became read-only meanwhile; the result of sorting was not applied: run it again")
                                           ("d e l e t e - d u p l i c a t e - l i n e s" "r e a d - o n l y - m o d e"
                                            "the result of deleting duplicates was not applied")
                                           ("d e l e t e - d u p l i c a t e - l i n e s" "h e x - m o d e"
                                            "*scratch* turned to the hex view, which only types over octets, meanwhile"))
            do (multiple-value-bind (f editor)
                   (run-held (list (format nil "C-x h M-x ~a RET M-x ~a RET" command toggle)) :text text :width 160)
                 (is string= text (session-text editor) "~a then ~a" command toggle)
                 (true (tuatara.frontend.fake:fake-frame-showing f said) "~a then ~a" command toggle)
                 (false (tuatara.frontend.fake:fake-frame-showing f "Error in") "~a then ~a" command toggle)))
      ;; the hex view takes a result that keeps the text's size: sorted lines are as many octets
      (multiple-value-bind (f editor)
          (run-held (list "C-x h M-x s o r t - l i n e s RET M-x h e x - m o d e RET") :text text :width 160)
        (is string= (lines-text "a" "b" "b") (session-text editor))
        (false (tuatara.frontend.fake:fake-frame-showing f "Error in"))))))
