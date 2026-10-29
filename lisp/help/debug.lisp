;;;; lisp/help/debug.lisp --- Debug mode: what the machinery says, in *Debug*.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x debug-mode switches it on and off. While it is on, the reports the
;;;; editor's parts make (TUATARA.RUNTIME:DEBUG-REPORT) go to *Debug*, a line
;;;; each, stamped with the seconds since the start and the thread that made
;;;; it: rules compiled and texts parsed, jobs in the background starting and
;;;; ending, what the file watcher found, frames slow to draw, collections of
;;;; garbage, the commands run and the keys that ran them, and errors, with the
;;;; calls under way. *Messages* keeps to what you were told. The mode line says
;;;; Debug; C-h E shows the buffer. The same lines go at once to debug.log in
;;;; the cache directory, where they outlast a crash.

(in-package #:tuatara.defaults)

(defvar *debug-mode* nil
  "True to start the editor with debug mode on, as \\[debug-mode] switches it:
what the editor's parts do is reported in *Debug*. Set it in the init file.")

(defvar *debug-log-max* 10000
  "The lines *Debug* keeps, the oldest going first; NIL keeps them all.")

(defun debug-buffer ()
  "The *Debug* buffer, made when there is none: read-only, shown where
*Messages* is."
  (find-or-make-buffer "*Debug*" :mode 'special-mode
                                 :made (lambda (b)
                                         (setf (tuatara.buffer:buffer-local b :read-only) t
                                               (tuatara.buffer:buffer-local b :kind) :messages))))

(defun show-debug-reports (editor)
  "Tick function: put the reports made since it was last called at the end of
*Debug*, keeping *DEBUG-LOG-MAX* lines; a window showing the end of *Debug*
goes on showing it."
  (declare (ignore editor))
  (let ((first (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)))
    (when first
      (add-to-log (debug-buffer) *debug-log-max*
                  (lambda (buffer)
                    (tuatara.buffer:buffer-insert
                     buffer (tuatara.buffer:buffer-size buffer)
                     (with-output-to-string (out)
                       (loop for report = first then (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                             while report
                             do (write-line (tuatara.runtime:debug-report-line report) out))))))))
  nil)

(defun debug-mode-line (editor buffer)
  "Mode line function: Debug while debug mode is on."
  (declare (ignore editor buffer))
  (and tuatara.runtime:*debug-on* "Debug"))

(defun switch-debug-mode (on)
  "Switch debug mode on when ON is true, else off, and say so in *Debug* and
the debug log: switched on, with which Tuatara this is, the process and the
time of day."
  (when on (debug-buffer))
  (tuatara.runtime:set-debug t)
  (if on
      (tuatara.runtime:debug-report :debug "debug mode on: ~a, process ~d, ~a"
                                    (tuatara:version-line) (sb-posix:getpid) (format-time "%F %T" (get-universal-time)))
      (tuatara.runtime:debug-report :debug "debug mode off"))
  (tuatara.runtime:set-debug on)
  (setf *debug-mode* (and on t)))

(define-command debug-mode ()
  "Switch debug mode on or off. While it is on, what the editor's parts do goes
to *Debug*, a line each, stamped with the seconds since the start and the
thread that did it: rules compiled and texts parsed, jobs in the background
starting and ending, what the file watcher found, frames slow to draw,
collections of garbage, the commands run and the keys that ran them, and
errors, with the calls under way, in commands, in jobs in the background and
in Lisp evaluated. *Messages* keeps to what you were told. The same lines go
at once to debug.log in the cache directory, readable by you alone (the logs
of the three sessions before kept as debug.log.1 to debug.log.3; while another
session writes to it, the lines follow its own), where they outlast a crash. The
mode line says Debug while it is on; \\[view-debug-buffer] shows *Debug*. While
it is off nothing is kept, and it costs nothing. *DEBUG-MODE* in the init
file starts the editor with it on."
  (let ((on (not tuatara.runtime:*debug-on*)))
    (switch-debug-mode on)
    (message "Debug mode ~:[off~;on: what the editor does goes to *Debug* (\\[view-debug-buffer])~]" on)))

(define-command view-debug-buffer ()
  "Show *Debug*, the reports of debug mode (\\[debug-mode]), where *Messages* is
shown, its end in sight."
  (display-buffer (debug-buffer) :at :end))

(defun install-debug (editor)
  "Bind \\[view-debug-buffer] to \\`C-h E', show the reports of debug mode as they
come, say Debug in the mode line while it is on, count the reports' times from
now, and have a report made in another thread wake EDITOR to show it. The
debug log's place, in the cache folder, is set once the init file is read,
which may choose that folder (*cache-directory*)."
  (define-key *global-keymap* "C-h E" 'view-debug-buffer)
  (pushnew 'show-debug-reports *tick-functions*)
  (pushnew 'debug-mode-line *mode-line-functions*)
  (setf tuatara.runtime:*debug-start* (get-internal-real-time))
  (let ((thread sb-thread:*current-thread*))
    (setf tuatara.runtime:*debug-wake-function*
          (lambda ()
            (unless (eq sb-thread:*current-thread* thread)
              (wake-editor editor))))))
