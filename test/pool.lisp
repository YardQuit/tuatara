;;;; test/pool.lisp --- The worker pool.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test pool
  :description "The worker pool runs tasks in parallel, keeps result order, re-signals errors, and allows nesting.")

(define-test (pool basics)
  (is equalp #() (tuatara.runtime:run-tasks #()))
  (is equalp #(1) (tuatara.runtime:run-tasks (vector (lambda () 1))))
  (let ((tasks (coerce (loop for i below 200 collect (let ((i i)) (lambda () (* i i)))) 'simple-vector)))
    (is equalp (coerce (loop for i below 200 collect (* i i)) 'simple-vector) (tuatara.runtime:run-tasks tasks))
    (is equalp (coerce (loop for i below 200 collect (* i i)) 'simple-vector) (tuatara.runtime:run-tasks tasks :parallelism 1)))
  ;; tasks really run on more than one thread
  (let* ((threads (tuatara.runtime:run-tasks (coerce (loop repeat 64 collect (lambda () (sleep 0.005) sb-thread:*current-thread*)) 'simple-vector))))
    (true (> (length (remove-duplicates (coerce threads 'list))) 1)))
  ;; an error in one task is signalled once all have run
  (fail (tuatara.runtime:run-tasks (vector (lambda () 1) (lambda () (error "boom")) (lambda () 3))))
  ;; and the others' results let go first (a run's open file, say)
  (let ((discarded '()))
    (fail (tuatara.runtime:run-tasks (vector (lambda () 1) (lambda () (error "boom")) (lambda () 3))
                                     :discard (lambda (result) (push result discarded))))
    (is equal '(1 3) (sort discarded #'<)))
  ;; nesting: tasks that run their own parallel batches finish even when every worker is busy
  (let ((outer (coerce (loop for i below (* 2 (tuatara.runtime:cpu-count))
                             collect (let ((i i))
                                       (lambda ()
                                         (reduce #'+ (tuatara.runtime:run-tasks
                                                      (coerce (loop for j below 8 collect (let ((j j)) (lambda () (+ i j)))) 'simple-vector))))))
                       'simple-vector)))
    (is equalp (coerce (loop for i below (* 2 (tuatara.runtime:cpu-count)) collect (+ (* 8 i) 28)) 'simple-vector)
        (tuatara.runtime:run-tasks outer))))

(define-test (pool waiting-with-a-limit)
  (let ((mutex (sb-thread:make-mutex :name "test")) (queue (sb-thread:make-waitqueue :name "test")))
    ;; the time runs out: false, and the mutex held again
    (sb-thread:with-mutex (mutex)
      (false (tuatara.runtime:wait-on queue mutex 0.001))
      (true (sb-thread:holding-mutex-p mutex)))
    ;; many short waits in a row never lose it
    (sb-thread:with-mutex (mutex)
      (true (loop repeat 500
                  always (progn (tuatara.runtime:wait-on queue mutex 0.0001)
                                (sb-thread:holding-mutex-p mutex)))))
    ;; woken by another thread: true, the mutex held
    (let ((flag nil))
      (sb-thread:with-mutex (mutex)
        (sb-thread:make-thread (lambda ()
                                 (sleep 0.05)
                                 (sb-thread:with-mutex (mutex)
                                   (setf flag t)
                                   (sb-thread:condition-notify queue))))
        (loop until flag do (tuatara.runtime:wait-on queue mutex 2))
        (true flag)
        (true (sb-thread:holding-mutex-p mutex))))))

(define-test (pool serious-conditions-end-the-batch)
  ;; a task failing with more than an error (a lack of memory, say) still ends its batch: the
  ;; condition is told where the tasks are waited for, and nothing waits for ever
  (is eq :caught (handler-case (tuatara.runtime:run-tasks (vector (lambda () 1) (lambda () (error 'storage-condition)) (lambda () 3)))
                   (storage-condition () :caught))))

(define-test (pool leaves-room)
  ;; the workers are nine in ten of the processors, at least one: work in the
  ;; background leaves the editor's thread room
  (is = (max 1 (floor (* 9 (tuatara.runtime:cpu-count)) 10)) (tuatara.runtime:pool-size (tuatara.runtime:ensure-pool))))

(define-test (pool stoppable-work)
  ;; a call that never looks whether to stop is cut short once its work is stopped, on the
  ;; pool's workers as on the thread that runs the work
  (let ((cancel (list nil)) (start (get-internal-real-time)))
    (sb-thread:make-thread (lambda () (sleep 0.1) (tuatara.runtime:stop-work cancel)) :name "tuatara test stopper")
    (multiple-value-bind (value reason)
        (tuatara.runtime:stoppable-work (cancel)
          (tuatara.runtime:run-tasks (vector (lambda () (tuatara.runtime:stoppable (loop)))
                                             (lambda () (tuatara.runtime:stoppable (loop))))
                                     :parallelism 2)
          :finished)
      (false value)
      (is eq t reason))
    (true (< (- (get-internal-real-time) start) (* internal-time-units-per-second (machine-slowdown)))))
  ;; work given seconds is cut short as slow when one call runs longer
  (multiple-value-bind (value reason)
      (tuatara.runtime:stoppable-work ((list nil) :slow 1/20)
        (tuatara.runtime:stoppable (loop))
        :finished)
    (false value)
    (is eq :slow reason))
  ;; and else gives what it made, however long it took between its calls
  (is eq :finished (tuatara.runtime:stoppable-work ((list nil) :slow 1/20)
                     (dotimes (i 4) (tuatara.runtime:stoppable (sleep 0.01)))
                     :finished)))

(defun thread-niceness ()
  "The calling thread's standing for the processor, as nice counts it."
  (let ((stat (uiop:read-file-string "/proc/thread-self/stat")))
    (parse-integer (nth 17 (uiop:split-string (subseq stat (1+ (position #\) stat :from-end t))) :separator " ")))))

(define-test (pool stopped-work-passes-over-the-rest)
  ;; work stopped by its first task passes over the tasks left at once, well under a
  ;; microsecond each: a stopped search of a large text gives the editor back however many
  ;; pieces it was cut into
  (flet ((seconds (n)
           (let* ((cancel (list nil))
                  (ran (list 0))
                  (tasks (coerce (loop for i below n
                                       collect (let ((i i))
                                                 (lambda ()
                                                   (sb-ext:atomic-incf (car ran))
                                                   (when (zerop i) (tuatara.runtime:stop-work cancel))
                                                   i)))
                                 'simple-vector))
                  (start (get-internal-real-time)))
             (multiple-value-bind (results stopped)
                 (tuatara.runtime:stoppable-work (cancel) (tuatara.runtime:run-tasks tasks))
               (false results)
               (true stopped))
             (true (< (car ran) n))
             (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    (seconds 1000)
    (let ((many (seconds 80000)))
      (true (< many (* 80000 1/1000000)) "80,000 tasks ~,4f s" many))))

(define-test (pool background-stands-below)
  ;; work in the background stands below the thread that starts it for the
  ;; processor, the programs it starts too, so that the editor's thread is
  ;; given the processor first; a thread whose work is looked at as it comes
  ;; (a terminal's output) stands as high
  (let* ((here (thread-niceness))
         (below (min 19 (+ here tuatara.runtime::*background-niceness*))))
    (flet ((in-thread (function &rest keys)
             (sb-thread:join-thread (apply #'tuatara.runtime:make-guarded-thread function :name "niceness" keys))))
      (is = below (in-thread #'thread-niceness))
      (is = here (in-thread #'thread-niceness :niceness 0))
      ;; a program a background thread starts
      (is = below (in-thread (lambda ()
                               (parse-integer (uiop:run-program '("sh" "-c" "cut -d' ' -f19 /proc/self/stat")
                                                                :output '(:string :stripped t)))))))
    ;; the pool's workers; the calling thread, which works too, as it was
    (let ((seen (remove-duplicates
                 (coerce (tuatara.runtime:run-tasks (coerce (loop repeat 16 collect (lambda () (sleep 0.05) (thread-niceness))) 'vector)
                                                    :parallelism 8)
                         'list))))
      (true (member below seen) "no worker below: ~a" seen)
      (true (subsetp seen (list here below)) "~a" seen))))

(define-test (pool background-stands-below-editor-alone)
  ;; a thread made by a background thread stands as far below the editor as
  ;; one the editor makes, not further: shared threads (the pool's workers)
  ;; stand the same whoever makes them first
  (let* ((here (thread-niceness))
         (below (min 19 (+ here tuatara.runtime::*background-niceness*))))
    (flet ((in-thread (function)
             (sb-thread:join-thread (tuatara.runtime:make-guarded-thread function :name "niceness"))))
      (is = below (in-thread (lambda () (in-thread #'thread-niceness)))))))

(define-test (pool source-tree-not-kept)
  ;; a saved program keeps no source tree of the machine it was built on, and
  ;; finds its own from where it stands: the tree whose build/ it is in, else
  ;; its own directory
  (true (member 'tuatara.runtime::forget-source-tree sb-ext:*save-hooks*))
  (true (member 'tuatara.runtime::find-source-tree sb-ext:*init-hooks*))
  (let ((tuatara.runtime:*source-tree* tuatara.runtime:*source-tree*))
    (tuatara.runtime::forget-source-tree)
    (false tuatara.runtime:*source-tree*)
    (true (tuatara.runtime:resource-directories "grammars/" "build/grammars/") "beside the program alone"))
  (let* ((dir (uiop:ensure-directory-pathname (format nil "/tmp/tuatara-source-tree-~d/" (sb-posix:getpid))))
         (tree (ensure-directories-exist (merge-pathnames "tree/build/" dir)))
         (release (ensure-directories-exist (merge-pathnames "release/" dir))))
    (unwind-protect
         (progn
           (alexandria:write-string-into-file "" (merge-pathnames "tree/tuatara.asd" dir))
           (is equal (namestring (merge-pathnames "tree/" dir))
               (namestring (tuatara.runtime::program-source-tree (merge-pathnames "tuatara" tree))))
           (is equal (namestring release)
               (namestring (tuatara.runtime::program-source-tree (merge-pathnames "tuatara" release)))))
      (uiop:delete-directory-tree dir :validate (lambda (p) (search "/tuatara-source-tree-" (namestring p)))))))

(define-test (pool processors-allowed)
  ;; the processors counted are those the thread may run on, as a container or taskset leaves
  ;; them: a thread let run on one processor alone counts one
  (let ((counted nil))
    (sb-thread:join-thread
     (sb-thread:make-thread
      (lambda ()
        (sb-alien:with-alien ((mask (array (sb-alien:unsigned 8) 128)))
          (dotimes (i 128) (setf (sb-alien:deref mask i) 0))
          (setf (sb-alien:deref mask 0) 1)
          (when (zerop (sb-alien:alien-funcall
                        (sb-alien:extern-alien "sched_setaffinity"
                                               (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                         (* (array (sb-alien:unsigned 8) 128))))
                        0 128 (sb-alien:addr mask)))
            (setf counted (tuatara.runtime:cpu-count)))))))
    (is eql 1 counted)))
