;;;; lisp/editing/shown-files.lisp --- Each file shown looked at once, in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The version-control gutter and the conflicts a merge leaves both look at
;;;; a file as soon as a window shows it: after each command, every file in a
;;;; window not looked at yet is, once, never one opened safely nor one whose
;;;; text no program may see (decrypted). The look runs as a job on the text
;;;; as it was then, which never changes, a file mapped from disk read as a
;;;; slice that keeps the mapping while the job reads it; it holds no other
;;;; work back, is not shown as work under way, and is stopped without asking
;;;; when the editor is left. Taken in, a look started again since, or
;;;; stopped, counts for nothing.

(in-package #:tuatara.defaults)

(defstruct (shown-text-job (:include tuatara.runtime:job) (:constructor nil) (:copier nil))
  "A look in the background at the text of a file shown in a window
\(LOOK-AT-SHOWN-FILES), a job: the text at VERSION, ROPE, and RESULT, what
the look found once done. Each kind of look is a structure including this
one."
  version rope result)

(setf (documentation 'shown-text-job-p 'function) "True when OBJECT is a SHOWN-TEXT-JOB, of any kind."
      (documentation 'shown-text-job-version 'function) "The version of its buffer's text a look in the background was started on."
      (documentation 'shown-text-job-rope 'function) "The text a look in the background reads."
      (documentation 'shown-text-job-result 'function) "What a look in the background found, once done.")

(tuatara.runtime:define-job-words shown-text-job
  :at-exit :quiet)

(defmethod tuatara.runtime:job-holds-queue-p ((job shown-text-job))
  "False: other work on the buffer need not wait while a file shown is looked at."
  nil)

(defmethod tuatara.runtime:job-shows-processing-p ((job shown-text-job))
  "False: looking at a file shown is not shown as work under way in the mode line."
  nil)

(defun start-shown-text-job (job key work &key (name "tuatara job") (slice t))
  "Start JOB, a SHOWN-TEXT-JOB of its buffer, on the buffer's text as it is
now, kept under the buffer's local KEY, a look kept there before stopped:
WORK, a function of JOB, gives its RESULT. With SLICE, the text of a file
mapped from disk is read as a slice (BUFFER-SLICE), which keeps the mapping
while the job reads it. Returns JOB."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (before (tuatara.buffer:buffer-local buffer key)))
    (when before (tuatara.runtime:job-stop before))
    (setf (shown-text-job-version job) (tuatara.buffer:buffer-version buffer)
          (shown-text-job-rope job) (if (and slice (tuatara.buffer:buffer-mapping buffer))
                                        (tuatara.buffer:buffer-slice buffer 0 (tuatara.buffer:buffer-size buffer))
                                        (tuatara.buffer:buffer-text buffer)))
    (setf (tuatara.buffer:buffer-local buffer key)
          (tuatara.runtime:start-job-thread job (lambda (job) (setf (shown-text-job-result job) (funcall work job)))
                                            :name name))))

(defun shown-text-job-taken-p (job key)
  "On taking JOB in: true when its buffer is still there and JOB is the look
it keeps under the local KEY, which is then cleared, and JOB was not
stopped; NIL for a look started again or stopped since."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (when (and (live-buffer-p buffer) (eq (tuatara.buffer:buffer-local buffer key) job))
      (setf (tuatara.buffer:buffer-local buffer key) nil)
      (not (eq (tuatara.runtime:job-status job) :stopped)))))

(defun look-at-shown-files (editor key look &key again (most most-positive-fixnum))
  "Look at each file EDITOR's windows show: LOOK, a function of its buffer,
called once, the buffer's local KEY marking it done, for a file of at most
MOST octets; AGAIN, a function of the buffer, called each time after, when
given. Never a file opened safely, whose surroundings may not be read, nor
one whose text no program may see (decrypted: :PLAINTEXT)."
  (dolist (window (editor-windows editor))
    (let ((b (tuatara.display:window-buffer window)))
      (when (and (tuatara.buffer:buffer-file b)
                 (not (tuatara.io:safe-visit-p b))
                 (not (tuatara.buffer:buffer-local b :plaintext)))
        (cond ((null (tuatara.buffer:buffer-local b key))
               (setf (tuatara.buffer:buffer-local b key) t)
               (when (<= (tuatara.buffer:buffer-size b) most)
                 (funcall look b)))
              (again (funcall again b)))))))
