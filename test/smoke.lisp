;;;; test/smoke.lisp --- Smoke tests: the image loads and the harness works.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test smoke
  :description "The core loads, reports a version, and the benchmark harness measures.")

(define-test (smoke version-is-a-string)
  (of-type string (tuatara:version))
  (true (plusp (length (tuatara:version)))))

(define-test (smoke harness-measures-time-and-bytes)
  (multiple-value-bind (result seconds bytes) (measuring (make-array 100000 :element-type '(unsigned-byte 8)))
    (of-type (simple-array (unsigned-byte 8) (*)) result)
    (true (>= seconds 0d0))
    (true (>= bytes 100000) "expected at least 100000 bytes consed, got ~a" bytes)))

(define-test (smoke harness-reports-budget)
  (let ((tuatara.bench:*benchmarks* '()))
    (define-benchmark within (:budget-ms 1000 :reps 1) (+ 1 1))
    (true (run-benchmarks :stream (make-broadcast-stream)))
    (define-benchmark over (:budget-ms 0 :reps 1) (sleep 0.002))
    (false (run-benchmarks :stream (make-broadcast-stream)))))

(define-test (smoke program-options)
  ;; the program's own options, as build/tuatara and tools/tui.lisp take them
  (let ((out (with-output-to-string (*standard-output*)
               (is = 0 (tuatara.terminal:main '("--version"))))))
    ;; the line M-x version says, without where it shows
    (is string= (format nil "~a~%" (tuatara:version-line)) out)
    (true (search (format nil "Tuatara ~a (" (tuatara:version)) out)))
  (let ((out (with-output-to-string (*standard-output*)
               (is = 0 (tuatara.terminal:main '("--help"))))))
    (true (search "Usage: tuatara [--gui] [-c INIT-FILE] [FILE...]" out))
    (true (search "~/.config/tuatara/init.lisp" out))
    (true (search "--generate-config" out))
    (true (search "--dynamic-space-size SIZE" out))
    (true (search "Michael A Jones" out))))

(define-test (smoke help-lists-real-options)
  ;; every option --help lists is one the program takes, or one the AppImage's launcher takes
  (let ((out (with-output-to-string (*standard-output*)
               (tuatara.terminal:main '("--help"))))
        (launcher '("--install-desktop" "--remove-desktop" "--dark" "--light" "--dynamic-space-size")))
    (loop with start = 0
          for at = (search "  -" out :start2 start)
          while at
          do (let* ((end (or (position-if (lambda (c) (member c '(#\Space #\, #\Newline))) out :start (+ at 2)) (length out)))
                    (option (subseq out (+ at 2) end)))
               (true (or (tuatara.terminal::option-p option) (member option launcher :test #'string=))
                     "~a is listed but not taken" option)
               (setf start end)))))

(define-test (smoke init-file-options)
  ;; -c names the init file, anywhere among the options; -- ends them
  (flet ((options (&rest arguments) (tuatara.terminal::read-options arguments)))
    (let ((read (options "--gui" "-c" "other.lisp" "notes.org")))
      (is equal "other.lisp" (getf read :config))
      (true (getf read :gui))
      (is equal '("notes.org") (getf read :files)))
    (is equal "other.lisp" (getf (options "--config=other.lisp") :config))
    (is equal "other.lisp" (getf (options "--config" "other.lisp" "--gui") :config))
    (is equal '("-c" "x") (getf (options "--" "-c" "x") :files))
    (true (getf (options "-c") :error))
    (true (getf (options "--generate-config") :generate-config)))
  ;; a file named as an option, as * gives one in a folder holding it, is never read as one
  (let ((folder (merge-pathnames (format nil "tuatara-options-~d/" (sb-posix:getpid)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist folder)
           (dolist (name '("-c" "evidence.txt")) (with-open-file (out (merge-pathnames name folder) :direction :output) (write-string "x" out)))
           (let ((read (tuatara.terminal::read-options '("-c" "evidence.txt") :directory (namestring folder))))
             (true (search "-c is a file here too" (getf read :error)))
             (false (getf read :config)))
           ;; written with ./ or after --, it is a file to edit
           (is equal '("./-c") (getf (tuatara.terminal::read-options '("./-c") :directory (namestring folder)) :files))
           (is equal '("-c") (getf (tuatara.terminal::read-options '("--" "-c") :directory (namestring folder)) :files)))
      (uiop:delete-directory-tree folder :validate t :if-does-not-exist :ignore)))
  ;; an init file named with characters a pattern would take, or a folder: said as not there
  (let ((said (make-string-output-stream)))
    (let ((*error-output* said))
      (is = 2 (tuatara.terminal:main '("-c" "/nonexistent/init[*].lisp")))
      (is = 2 (tuatara.terminal:main '("-c" "/tmp/"))))
    (is = 2 (count-matches "no init file at" (get-output-stream-string said))))
  ;; an init file named that is not there: said, and Tuatara does not start
  (let ((said (make-string-output-stream)))
    (let ((*error-output* said))
      (is = 2 (tuatara.terminal:main '("-c" "/nonexistent/tuatara-init.lisp"))))
    (true (search "no init file at /nonexistent/tuatara-init.lisp" (get-output-stream-string said))))
  ;; --generate-config writes the starter as generated-init.lisp, never over a file there,
  ;; in a configuration folder of the test's own
  (let ((home (merge-pathnames (format nil "tuatara-config-~d/" (sb-posix:getpid)) (uiop:temporary-directory)))
        (was (uiop:getenv "XDG_CONFIG_HOME")))
    (unwind-protect
         (progn
           (sb-posix:setenv "XDG_CONFIG_HOME" (namestring home) 1)
           (let ((file (merge-pathnames "tuatara/generated-init.lisp" home)))
             (with-output-to-string (*standard-output*)
               (is = 0 (tuatara.terminal:main '("--generate-config")))
               (is string= tuatara.defaults::*starter-config* (uiop:read-file-string file))
               (with-open-file (out file :direction :output :if-exists :supersede) (write-string "mine" out))
               (let ((*error-output* (make-broadcast-stream)))
                 (is = 1 (tuatara.terminal:main '("--generate-config"))))
               (is string= "mine" (uiop:read-file-string file)))
             (false (probe-file (merge-pathnames "tuatara/init.lisp" home)))
             ;; a link there, even one leading nowhere, is never written through
             (delete-file file)
             (sb-posix:symlink (namestring (merge-pathnames "elsewhere.lisp" home)) (namestring file))
             (let ((*error-output* (make-broadcast-stream)))
               (with-output-to-string (*standard-output*)
                 (is = 1 (tuatara.terminal:main '("--generate-config")))))
             (false (probe-file (merge-pathnames "elsewhere.lisp" home)))))
      (if was (sb-posix:setenv "XDG_CONFIG_HOME" was 1) (sb-posix:unsetenv "XDG_CONFIG_HOME"))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(define-test (smoke heap-decided-at-start)
  ;; half the machine's memory, from 2 GB to 16 GB, or TUATARA_HEAP_MB megabytes
  (flet ((wanted (setting memory-kb) (tuatara.terminal::wanted-heap-mb :setting setting :memory memory-kb)))
    (is = 3906 (wanted nil 8000000))
    (is = 16384 (wanted nil 128000000))
    (is = 2048 (wanted nil 2000000))
    (is = 3000 (wanted "3000" 128000000))
    (is = 3906 (wanted "lots" 8000000))
    (is = 3906 (wanted "0" 8000000))
    (false (wanted nil nil)))
  (true (plusp (tuatara.terminal::memory-kb)))
  ;; a memory limit on the process's control group, or one above it, counts too
  (let ((root (merge-pathnames (format nil "tuatara-cgroup-~d/" (sb-posix:getpid)) (uiop:temporary-directory))))
    (flet ((limit (group &rest limits)
             (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
             (loop for (path max) on limits by #'cddr
                   do (let ((file (merge-pathnames "memory.max" (merge-pathnames path root))))
                        (ensure-directories-exist file)
                        (with-open-file (out file :direction :output) (format out "~a~%" max))))
             (tuatara.terminal::memory-limit-kb :root (string-right-trim "/" (namestring root)) :group group)))
      (unwind-protect
           (progn
             (is = 8388608 (limit "/user.slice/app.scope" "user.slice/app.scope/" "8589934592" "user.slice/" "max"))
             (is = 4194304 (limit "/user.slice/app.scope" "user.slice/app.scope/" "max" "user.slice/" "4294967296"))
             (is = 2097152 (limit "/a/b" "a/b/" "4294967296" "a/" "2147483648"))
             (false (limit "/user.slice/app.scope" "user.slice/app.scope/" "max"))
             (false (limit "/nowhere/at/all"))
             (false (limit "no-slash"))
             (false (limit nil)))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
  ;; a restart only for the heap saved in the executable, when the machine wants one
  ;; more than a tenth larger or smaller, and never twice
  (flet ((restart (now built wanted &optional restarted)
           (tuatara.terminal::heap-to-restart-with :now (* now 1024 1024) :built (and built (* built 1024 1024))
                                                   :wanted wanted :restarted restarted)))
    (is = 3906 (restart 16384 16384 3906))
    (is = 16384 (restart 2048 2048 16384))
    (false (restart 16384 16384 15000))
    (false (restart 6144 16384 3906))
    (false (restart 16384 16384 3906 "1"))
    (false (restart 16384 nil 3906))
    (false (restart 16384 16384 nil))))
