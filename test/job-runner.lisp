;;;; test/job-runner.lisp --- Every kind of job in the background runs, stops, ends and is asked about alike.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test job-runner
  :description "Every kind of job runs on one runner: M-x stop-job stops each with one question, each
says how it stopped or failed in one form, one question before a kill or an exit names every job,
a list's files copied light P in the mode line and are listed with the agenda and the
calendar's marks, and a job whose work runs out of memory ends as failed.")

(defun job-kinds ()
  "Each kind of job, as (NAME MAKE STOP-QUESTION STOPPED): MAKE a function of a
buffer giving a new job of that kind for it, nothing done yet; what stop-job
asks before stopping it, and what is said once it has stopped."
  (list (list "rewrite" (lambda (b) (tuatara.defaults::%make-rewrite :buffer b :words "in the buffer"
                                                                      :pieces (vector (tuatara.defaults::make-rewrite-piece 0 3))))
              "Stop replacing at 0%?" "Stopped: nothing replaced")
        (list "sort" (lambda (b) (tuatara.defaults::%make-sort-job :buffer b :words "Sorting"))
              "Stop sorting at 0%?" "Stopped: nothing was changed")
        (list "sum" (lambda (b) (tuatara.defaults::%make-hash-job :buffer b :what "the text" :total 10))
              "Stop summing at 0%?" "Stopped: nothing summed")
        (list "count" (lambda (b) (tuatara.defaults::%make-count-job :buffer b :what "Buffer" :total 10))
              "Stop counting at 0%?" "Stopped: nothing counted")
        (list "dynamic blocks" (lambda (b) (tuatara.defaults::make-dblock-job b '() "Making the dynamic blocks again" nil))
              "Stop making blocks at 0%?" "Stopped: the dynamic blocks are as they were")
        (list "copy" (lambda (b) (tuatara.defaults::make-rimu-operation :copy (list (cons "/nonexistent/a" "/nonexistent/b")) b nil))
              "Stop copying at 0%? The file under way is taken away" "Stopped: copied 0 files")
        (list "save" (lambda (b) (tuatara.io::%make-save-job :buffer b :path "/nonexistent/saved.txt" :total 10))
              "Stop saving at 0%? The file stays as it was" "Stopped: the file is as it was")
        (list "agenda" (lambda (b)
                         (let ((job (tuatara.defaults::make-agenda-refresh b nil nil)))
                           (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::org-agenda-refresh) job)
                           job))
              "Stop working out the agenda?" "Stopped: the agenda is as it was")
        (list "calendar marks" (lambda (b)
                                 (let ((job (tuatara.defaults::make-calendar-marks b '())))
                                   (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::calendar-marks-job) job)
                                   job))
              "Stop marking the days?" "Stopped: the days are not marked")))

(defun until-stopped (job)
  "A job's work that does nothing until JOB is asked to stop, ten seconds at most."
  (loop repeat 1000 until (car (tuatara.runtime:job-cancel job)) do (sleep 0.01)))

(defun job-session (make keys &key (work #'until-stopped) (width 120) (height 10) after)
  "A session on a scratch buffer whose first event starts the job MAKE, a
function of the buffer, gives, doing WORK, then types KEYS, then calls AFTER,
when given, with the job. Returns the frontend, the editor and the job."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height))
        (job nil))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "abc")
    (tuatara.frontend.fake:fake-queue-action
     f (lambda ()
         (setf job (funcall make (tuatara.command:current-buffer)))
         (tuatara.runtime:start-job-thread job work :name "tuatara test job")))
    (queue-test-keys f keys)
    (when after
      (tuatara.frontend.fake:fake-queue-action f (lambda () (funcall after job))))
    (let ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                :buffer (tuatara.buffer:find-buffer "*scratch*"))))
      (values f editor job))))

(defmacro with-questions ((prompts &optional answer) &body body)
  "Run BODY with every yes-or-no question, typed or of one key, answered
ANSWER (NIL by default) at once, and PROMPTS bound to the list of the
questions asked, the first first."
  `(let ((,prompts '()))
     (dolist (asking '(tuatara.command:yes-or-no-p* tuatara.command:y-or-n-p*))
       (sb-int:encapsulate asking 'job-runner
                           (lambda (f prompt &rest how) (declare (ignore f how)) (setf ,prompts (append ,prompts (list prompt))) ,answer)))
     (unwind-protect (progn ,@body)
       (dolist (asking '(tuatara.command:yes-or-no-p* tuatara.command:y-or-n-p*))
         (sb-int:unencapsulate asking 'job-runner)))))

(define-test (job-runner stopping-every-kind)
  ;; M-x stop-job stops each kind, after one question of one form; how it stopped is said in one form
  (loop for (name make question stopped) in (job-kinds)
        do (multiple-value-bind (f editor job) (job-session make "M-x s t o p - j o b RET y")
             (declare (ignore editor))
             (true (tuatara.frontend.fake:fake-frame-showing f question) "~a asked ~s" name question)
             (true (tuatara.frontend.fake:fake-frame-showing f stopped) "~a said ~s" name stopped)
             (is eq :stopped (tuatara.runtime:job-status job) "~a" name)
             (false (member job tuatara.runtime:*jobs*) "~a taken in" name))))

(define-test (job-runner failing-alike)
  ;; a lack of memory in any kind's work: the job ends failed, never left running, and says so in one form
  (let ((b (tuatara.buffer:make-buffer "job-runner failing" :text "abc")))
    (unwind-protect
         (loop for (name make) in (job-kinds)
               do (let ((job (funcall make b)))
                    (tuatara.runtime:start-job-thread job (lambda (job) (declare (ignore job)) (error 'storage-condition)))
                    (is eq :failed (wait-for-status (lambda () (tuatara.runtime:job-status job))) "~a" name)
                    (true (search "Could not " (tuatara.runtime:job-ending-words job)) "~a: ~a"
                          name (tuatara.runtime:job-ending-words job))
                    (tuatara.runtime:forget-job job)))
      (tuatara.buffer:kill-buffer b)))
  ;; and the editor says it as it takes the job in
  (let ((f (job-session (second (find "count" (job-kinds) :key #'first :test #'string=)) ""
                        :work (lambda (job) (declare (ignore job)) (error 'storage-condition)))))
    (true (tuatara.frontend.fake:fake-frame-showing f "Could not count the words: "))))

(define-test (job-runner wordings)
  ;; the words of the end, the question before stopping, and the state in the list
  (let ((b (tuatara.buffer:make-buffer "job-runner wordings" :text "abc")))
    (unwind-protect
         (let ((sort (tuatara.defaults::%make-sort-job :buffer b :words "Sorting" :total 200)))
           (setf (car (tuatara.defaults::sort-job-progress sort)) 84)
           (is string= "Stop sorting at 42%?" (tuatara.runtime:job-stop-question sort))
           (is string= "Sorting 42%" (tuatara.runtime:job-state-words sort))
           (setf (tuatara.defaults::sort-job-counting sort) (cons 21 50))
           (is string= "Sorting: counting lines 42%" (tuatara.runtime:job-state-words sort))
           (setf (tuatara.runtime:job-status sort) :stopped)
           (is string= "Stopped: nothing was changed" (tuatara.runtime:job-ending-words sort))
           (setf (tuatara.runtime:job-status sort) :failed (tuatara.runtime:job-error sort) "no room")
           (is string= "Could not sort: no room" (tuatara.runtime:job-ending-words sort)))
      (tuatara.buffer:kill-buffer b)))
  ;; two phases: writing the first half, counting the lines of what was written the second
  (is = 21 (tuatara.runtime:two-phase-percent 42 nil))
  (is = 42 (tuatara.runtime:two-phase-percent 42 nil :halves nil))
  (is = 71 (tuatara.runtime:two-phase-percent 100 (cons 42 100)))
  (is = 99 (tuatara.runtime:two-phase-percent 100 (cons 100 100)))
  (is = 50 (tuatara.runtime:two-phase-percent 100 (cons 0 0))))

(define-test (job-runner one-question-to-kill)
  ;; a job running and one waiting its turn: one question names both
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.3))))
    (with-background ()
      (with-questions (prompts)
        (multiple-value-bind (f editor) (run-keys "r r % a / b RET r r % b / c RET C-x k" :text "a a a" :width 150)
          (declare (ignore f))
          (is = 1 (length prompts) "~s" prompts)
          (true (search "In the background: *scratch*: Replacing" (first prompts)) "~s" prompts)
          (true (search ", 1 queued; stop them, then kill the buffer?" (first prompts)) "~s" prompts)
          ;; answered no: the buffer and both jobs go on
          (is string= "c c c" (session-text editor)))))))

(define-test (job-runner one-question-to-exit)
  ;; running and waiting jobs: one question before exiting; a save is waited for, the rest stopped
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.3))))
    (with-background ()
      (with-questions (prompts)
        (multiple-value-bind (f editor) (run-keys "r r % a / b RET r r % b / c RET C-x C-c" :text "a a a" :width 150)
          (declare (ignore f))
          (is = 1 (length prompts) "~s" prompts)
          (true (search "In the background: *scratch*: Replacing" (first prompts)) "~s" prompts)
          (true (search ", 1 queued; stop them, then exit?" (first prompts)) "~s" prompts)
          (is string= "c c c" (session-text editor))))))
  (let ((b (tuatara.buffer:make-buffer "job-runner exit" :text "abc")))
    (unwind-protect
         (let ((tuatara.runtime:*jobs* (list (tuatara.io::%make-save-job :buffer b :path "/nonexistent/x" :total 10)
                                             (tuatara.defaults::%make-sort-job :buffer b :words "Sorting")))
               (tuatara.defaults::*job-queue* (list :one :two)))
           (with-questions (prompts)
             (false (tuatara.defaults::jobs-quit-query))
             (is equal '("In the background: job-runner exit: Saving 0%, job-runner exit: Sorting 0%, 2 queued; wait for the saving, stop the rest, then exit?")
                 prompts)))
      (tuatara.buffer:kill-buffer b))))

(define-test (job-runner quiet-jobs-not-asked)
  ;; what only shows something is stopped at a kill or an exit without a question
  (let ((b (tuatara.buffer:make-buffer "job-runner quiet" :text "abc")))
    (unwind-protect
         (let ((tuatara.runtime:*jobs* (list (tuatara.defaults::%make-hash-job :buffer b :what "abc" :total 3)
                                             (tuatara.defaults::%make-count-job :buffer b :what "Buffer" :total 3)))
               (tuatara.defaults::*job-queue* '()))
           (with-questions (prompts)
             (true (tuatara.defaults::jobs-kill-query b))
             (let ((then (tuatara.defaults::jobs-quit-query)))
               (true (functionp then))
               (funcall then))
             (is equal '() prompts)
             (true (every (lambda (job) (car (tuatara.runtime:job-cancel job))) tuatara.runtime:*jobs*)))
           ;; with a job that is asked about beside them, the question names only that one
           (let ((tuatara.runtime:*jobs* (list (tuatara.defaults::%make-hash-job :buffer b :what "abc" :total 3)
                                               (tuatara.defaults::%make-sort-job :buffer b :words "Sorting"))))
             (with-questions (prompts)
               (false (tuatara.defaults::jobs-kill-query b))
               (is equal '("In the background: job-runner quiet: Sorting 0%; stop it, then kill the buffer?") prompts))))
      (tuatara.buffer:kill-buffer b))))

(define-test (job-runner exports-asked)
  ;; an export makes a file and takes long to do again: killing its buffer, or leaving,
  ;; asks first, and no keeps the buffer and the export going
  (let ((b (tuatara.buffer:make-buffer "job-runner export" :text "* Note")))
    (unwind-protect
         (let ((tuatara.runtime:*jobs* (list (tuatara.defaults::make-pdf-export b "/nonexistent/note.org" :org :file nil nil t)
                                             (tuatara.defaults::make-docx-export b "" "/nonexistent/note.org" :file)))
               (tuatara.defaults::*job-queue* '()))
           (with-questions (prompts)
             (false (tuatara.defaults::jobs-kill-query b))
             (false (tuatara.defaults::jobs-quit-query))
             (is = 2 (length prompts))
             (true (search "stop them, then kill the buffer?" (first prompts)) "~s" prompts)
             (true (search "stop them, then exit?" (second prompts)) "~s" prompts)
             (true (notany (lambda (job) (car (tuatara.runtime:job-cancel job))) tuatara.runtime:*jobs*))))
      (tuatara.buffer:kill-buffer b))))

(define-test (job-runner files-copied-processing)
  ;; a list's files copied: P lit in the mode line, never the copying's own words;
  ;; listed with the other jobs, and stopped there
  (multiple-value-bind (f editor job)
      (job-session (second (find "copy" (job-kinds) :key #'first :test #'string=))
                   "M-x l i s t - j o b s RET" :height 12
                   :after (lambda (job) (tuatara.runtime:job-stop job)))
    (declare (ignore editor))
    (true (processing-shown f "TUATARA["))
    (false (tuatara.frontend.fake:fake-frame-showing f "Copying 0/1"))
    (true (tuatara.frontend.fake:fake-frame-showing f "*scratch*" "Copying 0%" "Copying 1 file to"))
    (is eq :stopped (tuatara.runtime:job-status job))))

(define-test (job-runner agenda-and-marks-listed)
  ;; an agenda worked out and a calendar's days marked are jobs like the others: listed, and stoppable
  (multiple-value-bind (f editor job)
      (job-session (second (find "agenda" (job-kinds) :key #'first :test #'string=))
                   "M-x l i s t - j o b s RET" :height 12
                   :after (lambda (job) (tuatara.runtime:job-stop job)))
    (declare (ignore editor job))
    (true (tuatara.frontend.fake:fake-frame-showing f "*scratch*" "Working out the agenda"))
    ;; P lit, as for any job
    (true (processing-shown f "TUATARA[")))
  ;; the calendar's marks, as the calendar starts them
  (let ((tuatara.defaults::*calendar-mark-functions*
          (list (lambda (first last)
                  (declare (ignore first last))
                  (lambda () (loop repeat 50 do (sleep 0.01) (tuatara.runtime:job-check-stop)) '())))))
    (let ((f (run-keys "M-x c a l e n d a r RET M-x l i s t - j o b s RET" :width 120 :height 24)))
      (true (tuatara.frontend.fake:fake-frame-showing f "*Calendar*" "Marking the days")))))

(define-test (job-runner no-thread-to-be-had)
  ;; a job whose thread cannot be made ends at once, failed, never left running
  (let ((b (tuatara.buffer:make-buffer "job-runner no thread" :text "abc")))
    (unwind-protect
         (let ((job (tuatara.defaults::%make-sort-job :buffer b :words "Sorting")))
           (sb-int:encapsulate 'tuatara.runtime:make-guarded-thread 'none
                               (lambda (original &rest arguments)
                                 (declare (ignore original arguments))
                                 (error "Could not create new OS thread.")))
           (unwind-protect (tuatara.runtime:start-job-thread job (lambda (job) (declare (ignore job))))
             (sb-int:unencapsulate 'tuatara.runtime:make-guarded-thread 'none))
           (is eq :failed (tuatara.runtime:job-status job))
           (true (search "Could not create" (tuatara.runtime:job-error job)))
           (true (tuatara.runtime:stop-and-settle job))
           (tuatara.runtime:forget-job job))
      (tuatara.buffer:kill-buffer b))))

(define-test (job-runner background-result-out-of-stack)
  ;; a result worked out in the background that runs out of stack is delivered, as an error
  (let* ((woken nil)
         (cell (tuatara.runtime:start-background-result
                (lambda () (labels ((deep (n) (if (< n 0) 0 (1+ (deep (1+ n)))))) (deep 0)))
                "tuatara test deep" (lambda () (setf woken t)))))
    (loop repeat 1000 until (car cell) do (sleep 0.01))
    (is eq :error (first (car cell)))
    (true (search "stack" (second (car cell))))
    (true woken)))
