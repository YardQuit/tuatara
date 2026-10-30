;;;; frontends/tui/main.lisp --- Tuatara in the terminal: the program's entry point.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The defaults in the terminal frontend, started by MAIN: from tools/tui.lisp
;;;; while developing, and from build/tuatara, the whole editor saved as one
;;;; executable by tools/build-image.sh, which starts in a fraction of a second.

(defpackage #:tuatara.terminal
  (:use #:cl)
  (:documentation "Tuatara in the terminal: the entry point shared by tools/tui.lisp and the saved executable.")
  (:export #:main #:image-toplevel #:*built-heap*))

(in-package #:tuatara.terminal)

(defparameter *usage*
  "Usage: tuatara [--gui] [-c INIT-FILE] [FILE...]
Edit the FILEs in this terminal, or start in the scratch buffers without any.
Written by Michael A Jones; free software under the GNU General Public
License, version 3 or later.
  --gui       open a window of its own instead (as when started with no terminal)
  -c INIT-FILE, --config INIT-FILE (or --config=INIT-FILE)
              read INIT-FILE as the init file instead of
              ~/.config/tuatara/init.lisp
  --generate-config
              write the init file Tuatara writes when there is none, every
              setting in it commented out, as
              ~/.config/tuatara/generated-init.lisp, and exit; a file there
              already is left as it is
  --help, -h  show this and exit
  --version, -V
              show the version and exit
  --          the arguments after it are all files, even when one starts with a
              dash
  --install-desktop
              (the AppImage) copy it to ~/.local/bin/tuatara and put Tuatara
              in the desktop's applications, its icon too
  --remove-desktop
              (the AppImage) take them away again
  --dark, --light
              (the AppImage) show the window's title bar in the dark or the
              light style; with --install-desktop, for the desktop's
              applications
  --dynamic-space-size SIZE
              the most memory Tuatara's own data may take, such as 6GB; by
              default half the machine's memory, from 2GB to 16GB, or
              $TUATARA_HEAP_MB megabytes
The init file is ~/.config/tuatara/init.lisp, written the first time
Tuatara starts without one.
"
  "What --help shows, as it is (it is no format control: ~/ starts one).")

;;; The heap, decided when the executable starts

(defvar *built-heap* nil
  "The heap, in octets, the executable was saved with (tools/build-image.lisp
sets it): a heap of another size was asked for with --dynamic-space-size.")

(defun file-first-line (path)
  "The first line of the file at PATH, or NIL when it cannot be read."
  (ignore-errors (with-open-file (in path) (read-line in nil))))

(defun memory-limit-kb (&key (root "/sys/fs/cgroup")
                             (group (let ((line (ignore-errors
                                                 (with-open-file (in "/proc/self/cgroup")
                                                   (loop for line = (read-line in nil)
                                                         while line
                                                         when (uiop:string-prefix-p "0::" line)
                                                           return line)))))
                                      (and line (subseq line 3)))))
  "The lowest memory limit, in kilobytes, set on GROUP (this process's control
group, as /proc/self/cgroup names it) or a group above it: memory.max under
ROOT. NIL when none is set, or they cannot be read."
  (let ((lowest nil))
    (when group
      (loop for path = (string-right-trim "/" group) then (subseq path 0 (or (position #\/ path :from-end t) 0))
            for max = (file-first-line (concatenate 'string root path "/memory.max"))
            do (when (and max (plusp (length max)) (every #'digit-char-p max))
                 (let ((kb (floor (parse-integer max) 1024)))
                   (setf lowest (if lowest (min lowest kb) kb))))
            while (plusp (length path))))
    lowest))

(defun memory-kb ()
  "The memory Tuatara may use, in kilobytes: the machine's, as /proc/meminfo
says, or less under a memory limit (MEMORY-LIMIT-KB). NIL when unknown."
  (let ((total (ignore-errors
                (with-open-file (in "/proc/meminfo")
                  (loop for line = (read-line in nil)
                        while line
                        when (uiop:string-prefix-p "MemTotal:" line)
                          return (parse-integer line :start 9 :junk-allowed t)))))
        (limit (memory-limit-kb)))
    (if (and total limit) (min total limit) (or total limit))))

(defun wanted-heap-mb (&key (setting (uiop:getenv "TUATARA_HEAP_MB")) (memory (memory-kb)))
  "The heap Tuatara wants, in megabytes: SETTING ($TUATARA_HEAP_MB) when it is
a number, else half of MEMORY (kilobytes, the machine's), from 2 GB to 16 GB,
so that a replacement across a file of many gigabytes has room while the heap
cannot outgrow the memory. NIL when neither is known."
  (or (ignore-errors (let ((mb (parse-integer setting))) (and (plusp mb) mb)))
      (and memory (max 2048 (min 16384 (floor memory 2048))))))

(defun heap-to-restart-with (&key (now (sb-ext:dynamic-space-size)) (built *built-heap*)
                                  (wanted (wanted-heap-mb)) (restarted (uiop:getenv "TUATARA_HEAP_DECIDED")))
  "The heap, in megabytes, to start again with, or NIL to go on: when the heap
NOW (octets) is the one the executable was BUILT with, not one asked for, and
the WANTED one differs from it by more than a tenth, unless this start is
already the second (RESTARTED)."
  (when (and wanted built (= now built) (not restarted)
             (> (abs (- (* wanted 1024 1024) now)) (floor now 10)))
    wanted))

(defun restart-with-heap (mb)
  "Run this executable again in place of this process, with the same arguments
and a heap of MB megabytes. Returns only when that fails."
  (sb-posix:setenv "TUATARA_HEAP_DECIDED" "1" 1)
  (tuatara.io:run-program-again (list* "--dynamic-space-size" (format nil "~dMB" mb) (rest sb-ext:*posix-argv*))))

(defparameter *runtime-options-with-values* '("--dynamic-space-size" "--control-stack-size" "--tls-limit")
  "The options SBCL's runtime takes out of the command line, each with the value after it.")

(defun command-line-arguments (&key (argv sb-ext:*posix-argv*) (from "/proc/self/cmdline"))
  "The program's command line, its name first, as SB-EXT:*POSIX-ARGV* has it:
ARGV itself, or, when it is NIL because an argument is not UTF-8, read again
as octets FROM the system's record of it, each octet that is not UTF-8 kept
as its raw byte's character, so that such a file name still opens, the
runtime's options (*RUNTIME-OPTIONS-WITH-VALUES*) left out as it leaves them out."
  (or argv
      (let* ((octets (ignore-errors (tuatara.io:file-octets from)))
             (parts (and octets
                         (loop with start = 0
                               for end = (position 0 octets :start start)
                               while end
                               collect (tuatara.text:octets-string octets :start start :end end)
                               do (setf start (1+ end))))))
        (loop while parts
              for part = (pop parts)
              if (member part *runtime-options-with-values* :test #'string=)
                do (pop parts)
              else collect part))))

(defun terminal-p (fd)
  "True when the file descriptor FD is a terminal."
  (eql 1 (ignore-errors (sb-unix:unix-isatty fd))))

(defun window-wanted-p (&key (input (terminal-p 0)) (output (terminal-p 1))
                             (desktop (or (uiop:getenv "WAYLAND_DISPLAY") (uiop:getenv "DISPLAY"))))
  "True when Tuatara was started with no terminal to run in, on a desktop: from
a launcher rather than a shell. INPUT and OUTPUT say whether its input and
its output are a terminal, DESKTOP whether there is a desktop to show a window on."
  (and (not input) (not output) desktop t))

(defun read-keys-from-terminal ()
  "When Tuatara's input is not a terminal but its output is, as in
`find . -name \"*.org\" | xargs tuatara', read the keys from the terminal it
was started in (/dev/tty) instead; what came in on the input is not read."
  (when (and (not (terminal-p 0)) (terminal-p 1))
    (let ((fd (ignore-errors (sb-posix:open "/dev/tty" sb-posix:o-rdwr))))
      (when fd
        (sb-posix:dup2 fd 0)
        (sb-posix:close fd)))))

(defun option-p (argument)
  "True when ARGUMENT is one of the program's options."
  (or (member argument '("--" "--version" "-V" "--help" "-h" "--gui" "--generate-config" "-c" "--config") :test #'string=)
      (uiop:string-prefix-p "--config=" argument)))

(defun read-options (arguments &key (directory (uiop:getcwd)))
  "The options of ARGUMENTS, the program's (strings), as a plist: :GUI,
:CONFIG (the init file -c names), :GENERATE-CONFIG, :VERSION, :HELP and
:FILES, the arguments after the options (all after --); or :ERROR, what is
wrong with them, among them an option that is also a file's name in
DIRECTORY, where the program was started."
  (let ((options '()))
    (loop
      (let ((argument (pop arguments)))
        (flet ((given (key) (setf (getf options key) t)))
          (cond ((null argument) (return))
                ;; a file named as an option, as * gives one in a folder holding it: never read as
                ;; one, so that no file there chooses the init file or what is done
                ((and (option-p argument) (ignore-errors (tuatara.io:path-kind (tuatara.io:expand-path argument :directory directory) :follow nil)))
                 (return-from read-options
                   (list :error (format nil "~a is a file here too: write ./~a, or -- before the files" argument argument))))
                ((string= argument "--") (return))
                ((member argument '("--version" "-V") :test #'string=) (given :version))
                ((member argument '("--help" "-h") :test #'string=) (given :help))
                ((string= argument "--gui") (given :gui))
                ((string= argument "--generate-config") (given :generate-config))
                ((member argument '("-c" "--config") :test #'string=)
                 (let ((path (pop arguments)))
                   (if (and path (plusp (length path)))
                       (setf (getf options :config) path)
                       (return-from read-options (list :error (format nil "~a needs the init file after it" argument))))))
                ((uiop:string-prefix-p "--config=" argument)
                 (setf (getf options :config) (subseq argument (length "--config="))))
                (t (push argument arguments) (return))))))
    (list* :files arguments options)))

(defun generate-config ()
  "Write the starter init file as generated-init.lisp in the configuration
folder, never over a file there; said on the standard output, or why not on
the error output. The exit code: 0 when written, 1 when not."
  (let ((path (tuatara.defaults::config-path "generated-init.lisp")))
    (multiple-value-bind (written why) (tuatara.defaults::write-starter-config path)
      (cond (written (format t "Wrote ~a~%" path) 0)
            (why (format *error-output* "tuatara: no init file written at ~a: ~a~%" path why) 1)
            (t (format *error-output* "tuatara: ~a is there already; move it away to write a new one~%" path) 1)))))

(defun main (arguments)
  "Run Tuatara in this terminal on the files named in ARGUMENTS (strings), or
in the scratch buffers when there are none, as its options say
\(READ-OPTIONS): --version, --help and --generate-config do what they say
instead; -c names the init file; -- before the files lets a file's name
start with a dash. With --gui, or when started with no terminal on a
desktop, Tuatara opens a window of its own. Returns the exit code: 0, or
what the session left (*EXIT-CODE*); 2 for options it cannot take."
  (let* ((options (read-options arguments))
         (config (getf options :config))
         ;; named from where Tuatara was started, as the files are
         (config-path (and config (tuatara.io:expand-path config :directory (uiop:getcwd)))))
    (cond ((getf options :error)
           (format *error-output* "tuatara: ~a~%~a" (getf options :error) *usage*)
           2)
          ((getf options :version)
           (format t "~a~%" (tuatara:version-line))
           0)
          ((getf options :help)
           (write-string *usage*)
           0)
          ((getf options :generate-config)
           (generate-config))
          ((and config (not (eq (ignore-errors (tuatara.io:path-kind config-path)) :regular)))
           (format *error-output* "tuatara: no init file at ~a (tuatara --generate-config writes one to start from)~%" config)
           2)
          (t
           (tuatara.defaults::set-collection-policy)
           (when config
             (setf tuatara.defaults::*config-file* (pathname config-path)))
           (let* ((gui (or (getf options :gui) (window-wanted-p)))
                  ;; named from where Tuatara was started, not *default-directory*
                  (files (mapcar (lambda (file) (tuatara.io:native (tuatara.io:expand-path file :directory (uiop:getcwd))))
                                 (getf options :files))))
             ;; a copy started to measure how the window draws (check-health): no init
             ;; file, the window's font as the one measuring has it
             (let ((measure (uiop:getenv "TUATARA_MEASURE_DRAWING")))
               (when measure
                 (setf tuatara.defaults::*config-file* (merge-pathnames "no-init-file.lisp" measure)
                       tuatara-user:*gui-font* (uiop:getenv "TUATARA_MEASURE_FONT"))))
             (if gui
                 (tuatara.frontend.gtk:run-in-window files #'tuatara.defaults:install-defaults)
                 (progn
                   (read-keys-from-terminal)
                   (tuatara.command:run-editor (tuatara.frontend.tui:make-tui-frontend)
                                               :files files
                                               :setup #'tuatara.defaults:install-defaults)))
             tuatara.command:*exit-code*)))))

(defun leave-appimage-environment ()
  "Run from Tuatara's AppImage (its AppRun says so in TUATARA_APPIMAGE), give
LD_LIBRARY_PATH back as it was before AppRun added the libraries Tuatara
brings: the system looks there for Tuatara's own all the same, as it read the
path when Tuatara started, while the programs Tuatara starts (a terminal's
shell, the desktop's programs, those the sandbox shuts in) use their own."
  (when (uiop:getenv "TUATARA_APPIMAGE")
    (let ((before (uiop:getenv "TUATARA_LD_LIBRARY_PATH")))
      (if (and before (plusp (length before)))
          (sb-posix:setenv "LD_LIBRARY_PATH" before 1)
          (sb-posix:unsetenv "LD_LIBRARY_PATH")))
    (sb-posix:unsetenv "TUATARA_LD_LIBRARY_PATH")
    (sb-posix:unsetenv "TUATARA_APPIMAGE")))

(defun fresh-random-state ()
  "Give *RANDOM-STATE* a state of its own for this start, so that the random
numbers Tuatara draws differ from one start to the next rather than repeat
the state the executable was saved with."
  (setf *random-state* (make-random-state t)))

(defun image-toplevel ()
  "What the saved executable runs: MAIN on its command-line arguments, then exit
with its code at once, the terminal already given back. First, when this
machine wants a heap of another size than the executable was saved with, it
starts again with that one (HEAP-TO-RESTART-WITH), and its random numbers are
new (FRESH-RANDOM-STATE), its arguments read even when one is not UTF-8
\(COMMAND-LINE-ARGUMENTS); run from the AppImage, the
programs it starts are given the environment as it was before
\(LEAVE-APPIMAGE-ENVIRONMENT)."
  (sb-ext:disable-debugger)
  (setf sb-ext:*posix-argv* (command-line-arguments))
  (fresh-random-state)
  (setf tuatara.io:*saved-program* (ignore-errors (namestring (truename "/proc/self/exe"))))
  (let ((mb (heap-to-restart-with)))
    (when mb (restart-with-heap mb)))
  (sb-posix:unsetenv "TUATARA_HEAP_DECIDED")
  ;; from now on the AppImage, which brings its libraries along, is what starts Tuatara again
  (let ((appimage (uiop:getenv "APPIMAGE")))
    (when (and appimage (uiop:getenv "TUATARA_APPIMAGE"))
      (setf tuatara.io:*saved-program* appimage)))
  (leave-appimage-environment)
  (uiop:call-image-restore-hook)
  (let ((code (main (rest sb-ext:*posix-argv*))))
    (finish-output)
    (sb-ext:exit :code code :abort t)))
