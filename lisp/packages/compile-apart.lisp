;;;; lisp/packages/compile-apart.lisp --- Compiling a package in a process of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package's files are compiled by a child process started from a core made for
;;;; the purpose, with a request file saying what to compile, so that nothing a
;;;; package does while it compiles can reach the editor.

(in-package #:tuatara.packages)

(defparameter *package-compiling-line* ";;; tuatara: compiling"
  "What a process compiling a package apart prints on a line of its own as it
begins compiling a file of it (COMPILE-PACKAGE-CHILD).")

(defparameter *package-child-said-lines* 40
  "The most lines kept of what a process compiling a package apart said, the
last ones, to say why it failed.")

(defun note-package-compiling (file)
  "As FILE, of a package, is about to be compiled: an error while the editor
waits for a package to load (*PACKAGE-COMPILING* :REFUSED), for FILE changed
after the package was compiled apart; said on the output the first time in
a process compiling a package apart (:APART)."
  (case *package-compiling*
    (:refused (refuse "~a changed while the package was being compiled; it loads as changed the next time it is loaded"
                      (abbreviated-path (tuatara.io:native file))))
    (:apart (format t "~&~a~%" *package-compiling-line*)
            (finish-output)
            (setf *package-compiling* :apart-said))))

(defun package-request-text (directory name file token)
  "What a process compiling the package NAME in DIRECTORY apart is given to do
\(COMPILE-PACKAGE-CHILD), as text read back without evaluating anything: the
package, FILE its entry when named; the packages loaded so far, to load
first, oldest first; where this Tuatara keeps compiled packages; and TOKEN,
which the process is given apart too (PACKAGE-REQUEST-ALLOWED-P)."
  (with-standard-io-syntax
    (let ((*print-readably* t))
      (prin1-to-string
       (list :token token :name name :directory (tuatara.io:native directory) :file file
             :prelude (reverse (remove name *loaded-package-directories* :key #'first :test #'string=))
             :cache (tuatara.io:native (tuatara.io:cache-directory))
             :packages (packages-directory)
             :identity (tuatara-identity))))))

(defparameter *package-child-environment*
  '("PATH" "HOME" "USER" "LOGNAME" "LANG" "LC_ALL" "LC_CTYPE" "TZ" "TMPDIR"
    "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_STATE_HOME"
    "SBCL_HOME" "QUICKLISP_SETUP")
  "The variables of the editor's environment a process compiling a package
apart is given, and no others. This cuts down what it reaches by chance (the
desktop, the keyring's agent, the session's message bus, whose sockets
XDG_RUNTIME_DIR names); it is no sandbox: a package's code runs there with
your full rights, as in the editor.")

(defun package-child-environment (request token)
  "The environment of a process compiling a package apart, as REQUEST, a
file, and TOKEN say (COMPILE-PACKAGE-CHILD): those of
*PACKAGE-CHILD-ENVIRONMENT* the editor has, and, with REQUEST, what tells the
process what to do."
  (append (loop for name in *package-child-environment*
                for value = (tuatara.io:own-variable name)
                when value collect (format nil "~a=~a" name value))
          (list "TUATARA_HEAP_DECIDED=1")
          (and request
               (list (format nil "TUATARA_COMPILE_PACKAGE=~a" request)
                     (format nil "TUATARA_COMPILE_TOKEN=~a" token)
                     (format nil "TUATARA_COMPILE_CACHE=~a" (package-cache))))))

(defvar *compiler-core-lock* (sb-thread:make-mutex :name "tuatara compiler core")
  "Held while the core packages are compiled apart from, run from Tuatara's
sources, is made (COMPILER-CORE), so that it is made once.")

(defun compiler-core-toplevel ()
  "What the core COMPILER-CORE makes runs: compile a package as the editor
asks (COMPILE-PACKAGE-CHILD), then exit with its code."
  (sb-ext:disable-debugger)
  (uiop:call-image-restore-hook)
  (let ((code (compile-package-child)))
    (finish-output)
    (sb-ext:exit :code code :abort t)))

(defun compiler-core ()
  "Run from Tuatara's sources, the Steel Bank Common Lisp (SBCL) core a process
compiling a package apart starts from, so that it starts at once. It is SBCL
with Quicklisp and these sources loaded, saved once into the directory of compiled
packages. That directory is this Tuatara's alone (PACKAGE-CACHE: another source
tree, or the same one changed, has another). The core is made again only when it
is not there. It is made in a process of its own (save-lisp-and-die cannot run
where threads do), under a name of its own and renamed into place whole. Returns
its path, or NIL when it could not be made, why in *Messages*."
  (let ((core (format nil "~acompiler-~36r.core" (package-cache)
                      (sxhash (uiop:native-namestring (asdf:system-source-directory :tuatara))))))
    (sb-thread:with-mutex (*compiler-core-lock*)
      (or (and (probe-file core) core)
          (let ((temporary (format nil "~a.~d.~a.new" core (sb-posix:getpid) (tuatara.io:random-name))))
            (multiple-value-bind (said ok)
                (tuatara.io:program-output
                 (list (uiop:native-namestring sb-ext:*runtime-pathname*)
                       "--non-interactive"
                       "--no-sysinit"
                       "--no-userinit"
                       "--load" (uiop:native-namestring (asdf:system-relative-pathname :tuatara "tools/quicklisp.lisp"))
                       "--eval" (format nil
                                        "(progn (push ~s asdf:*central-registry*) (funcall (find-symbol \"QUICKLOAD\" \"QL\") :tuatara/defaults :silent t))"
                                        (uiop:native-namestring (asdf:system-source-directory :tuatara)))
                       "--eval" (format nil
                                        "(sb-ext:save-lisp-and-die ~s :toplevel (find-symbol \"COMPILER-CORE-TOPLEVEL\" \"TUATARA.PACKAGES\"))"
                                        temporary))
                 :seconds 600 :environment (package-child-environment nil nil))
              (cond ((and ok (probe-file temporary))
                     (sb-posix:chmod temporary #o600)
                     (sb-posix:rename temporary core)
                     core)
                    (t (handler-case (delete-file temporary) (file-error () nil))
                       (let ((words (format nil "No core to compile packages apart from could be made: ~a"
                                            (tuatara.text:one-line (or said "it failed")))))
                         (defer-action (lambda () (log-message words))))
                       nil))))))))

(defun package-compiler-command ()
  "The command that runs this Tuatara again to compile a package apart, told
what to do by its environment (PACKAGE-CHILD-ENVIRONMENT): the saved
executable, or, run from its sources, Steel Bank Common Lisp (SBCL) started
from the core made for that (COMPILER-CORE); NIL when there is none."
  (if tuatara.io:*saved-program*
      (list tuatara.io:*saved-program*)
      (let ((core (compiler-core)))
        (and core (list (uiop:native-namestring sb-ext:*runtime-pathname*) "--core" core)))))

(defparameter *package-child-line-limit* 1024
  "The most characters kept of a line a process compiling a package apart
says; the rest of it is read and let go.")

(defparameter *package-child-said-limit* (* 64 1024)
  "The most characters read and kept in all of what a process compiling a
package apart says; the rest is read and let go, so that it never waits.")

(defun compile-package-apart (job directory name file)
  "Compile the package NAME in DIRECTORY, FILE its entry when named, in a process
of its own, this Tuatara run again (PACKAGE-COMPILER-COMMAND): it loads the
packages loaded here so far, then this one, compiling what changed, as a
load here would, so that its compile-time code never runs in the editor.
JOB is the loading's: stopping it ends that process; its COMPILED turns true
as a file of it begins compiling, said. True when it was all compiled so,
with the structures the package defines as a second value
\(STRUCTURES-MADE-SINCE), or :UNKNOWN when the process did not say them;
NIL when it failed there, as code of the package's that needs what only the
editor has does, the last of what the process said kept in *Messages*, or
when there is nothing to run it from (COMPILER-CORE). It
is refused for names this Tuatara lacks only as it loads in the editor
\(REFUSE-PACKAGE). What the process says is read a line of at most
*PACKAGE-CHILD-LINE-LIMIT* at a time, *PACKAGE-CHILD-SAID-LIMIT* in all."
  (let ((request (format nil "~acompile-~a.request" (package-cache) (tuatara.io:random-name)))
        (token (tuatara.io:random-name))
        (command (package-compiler-command))
        (said '()) (kept 0) (read 0))
    (unless command
      (return-from compile-package-apart nil))
    (tuatara.io:ensure-private-directory (package-cache))
    (tuatara.io:write-private-file request (package-request-text directory name file token))
    (unwind-protect
         (let ((code (tuatara.io:call-with-program
                      command
                      (lambda (process)
                        (loop for line = (tuatara.io:read-line-bounded (sb-ext:process-output process)
                                                                       *package-child-line-limit*)
                              while line
                              do (incf read (length line))
                              (cond ((> read *package-child-said-limit*))
                                    ((string= line *package-compiling-line*)
                                     (unless (package-fetch-compiled job)
                                       (setf (package-fetch-compiled job) t)
                                       (message "Compiling the package ~a in the background..." name)))
                                    (t (push line said)
                                       (when (> (incf kept) *package-child-said-lines*)
                                         (setf said (subseq said 0 *package-child-said-lines*) kept
                                               *package-child-said-lines*)))))
                        (sb-ext:process-wait process)
                        (sb-ext:process-exit-code process))
                      :job job :output :stream :error :output
                      :environment (package-child-environment request token))))
           (tuatara.runtime:job-check-stop)
           (cond ((eql code 0) (values t (read-structures-file (structures-file request))))
                 (t (let ((words (format nil
                                         "The package ~a could not be compiled apart from the editor, and is compiled as it loads~@[: ~{~a~^ ~}~]"
                                         name
                                         (reverse (mapcar (lambda (l) (string-trim " ;" l))
                                                          (remove "" said :test #'string=))))))
                      ;; *Messages* is the editor's
                      (defer-action (lambda () (log-message words))))
                    nil)))
      (dolist (made (list request (structures-file request)))
        (handler-case (delete-file made) (file-error () nil))))))

(defun structure-description (name)
  "How the structure NAME is laid out, as strings, integers and lists of them:
its length, how deep it stands among the structures it includes, which of its
words hold Lisp objects, the names of the classes it stands on and, for each
slot, its name, its place, the kind of value kept in it and its type. A
structure can be defined again in a running Tuatara only as it was laid out
before; one laid out otherwise needs a new start. NIL when NAME names no
structure."
  (let ((definition (sb-kernel:find-defstruct-description name nil))
        (layout (sb-kernel:find-layout name)))
    (when (and definition layout)
      (with-standard-io-syntax
        (let ((*package* (find-package :keyword)) (*print-readably* nil) (*print-pretty* nil))
          (list (sb-kernel:layout-length layout)
                (sb-kernel:layout-depthoid layout)
                (sb-kernel:layout-bitmap layout)
                (map 'list (lambda (above)
                             (prin1-to-string (sb-kernel:classoid-name (sb-kernel:layout-classoid above))))
                     (sb-kernel:layout-inherits layout))
                (mapcar (lambda (slot)
                          (list (symbol-name (sb-kernel:dsd-name slot)) (sb-kernel:dsd-index slot)
                                (prin1-to-string (sb-kernel:dsd-raw-type slot))
                                (prin1-to-string (sb-kernel:dsd-type slot))))
                        (sb-kernel:dd-slots definition))))))))

(defun structure-definitions ()
  "A table from each symbol that names a structure in this Lisp to its definition."
  (let ((table (make-hash-table :test 'eq)))
    (do-all-symbols (symbol table)
      (let ((definition (sb-kernel:find-defstruct-description symbol nil)))
        (when definition (setf (gethash symbol table) definition))))))

(defun structures-made-since (before)
  "The structures defined, or defined again, since BEFORE (STRUCTURE-DEFINITIONS),
each (PACKAGE NAME DESCRIPTION): the names of the symbol's package and of
the symbol, and how it is laid out (STRUCTURE-DESCRIPTION)."
  (let ((made '()))
    (do-all-symbols (symbol made)
      (let ((definition (sb-kernel:find-defstruct-description symbol nil)))
        (when (and definition (symbol-package symbol) (not (eq definition (gethash symbol before))))
          (pushnew (list (package-name (symbol-package symbol)) (symbol-name symbol) (structure-description symbol))
                   made :test #'equal))))))

(defun structures-file (request)
  "The file a process compiling a package apart writes, beside its REQUEST, the
structures the package defines in (STRUCTURES-MADE-SINCE), as JSON."
  (concatenate 'string request ".structures"))

(defun write-structures-file (file structures)
  "Write STRUCTURES (STRUCTURES-MADE-SINCE), lists of strings, integers and
lists, to FILE as JSON arrays, readable by you alone."
  (labels ((arrays (value) (if (listp value) (map 'vector #'arrays value) value)))
    (tuatara.io:write-private-file file (tuatara.text:json-write (arrays structures)))))

(defun read-structures-file (file)
  "The structures FILE says a package compiled apart defines
\(WRITE-STRUCTURES-FILE), each (PACKAGE NAME DESCRIPTION), or :UNKNOWN when
FILE is not there or holds anything but arrays of strings and integers. Only
JSON is read from it, never Lisp: nothing is evaluated or interned."
  (labels ((plain (value)
             (typecase value
               ((or string integer) value)
               (simple-vector (map 'list #'plain value))
               (t (return-from read-structures-file :unknown)))))
    (let* ((text (ignoring-errors-reported ("reading ~a" file) (and (probe-file file) (tuatara.io:file-text file))))
           (read (and text (handler-case (plain (tuatara.text:json-read text))
                             (tuatara.text:json-error () :unknown)))))
      (if (and (listp read)
               (every (lambda (entry) (and (listp entry) (= (length entry) 3)
                                           (stringp (first entry)) (stringp (second entry)) (listp (third entry))))
                      read))
          read
          :unknown))))

(defun structures-in-the-way (name structures)
  "The names of the structures that keep a new version of the package NAME
from loading in this running Tuatara, in lower case: those of STRUCTURES,
what the new version defines (STRUCTURES-MADE-SINCE), that are laid out
otherwise here (STRUCTURE-DESCRIPTION). When STRUCTURES is :UNKNOWN and NAME
is loaded, every structure its own Lisp packages define, since any of them
may have changed."
  (if (eq structures :unknown)
      (and (package-loaded-p name)
           (let ((found '()))
             (dolist (package (cdr (assoc name *package-code* :test #'string=)) found)
               (when (package-name package)
                 (do-symbols (symbol package)
                   (when (and (eq (symbol-package symbol) package) (sb-kernel:find-defstruct-description symbol nil))
                     (pushnew (string-downcase (symbol-name symbol)) found :test #'string=)))))))
      (loop for (package-name symbol-name description) in structures
            for package = (find-package package-name)
            for symbol = (and package (find-symbol symbol-name package))
            for here = (and symbol (structure-description symbol))
            when (and here (not (equal here description)))
              collect (string-downcase symbol-name))))

(defun package-request-allowed-p (request token cache)
  "True when REQUEST, a file, is one the editor wrote for a process compiling
a package apart (COMPILE-PACKAGE-APART): a regular file, not a link, in CACHE,
the directory of compiled packages the editor named, called compile-*.request,
yours and read and written by you alone; and TOKEN, given in the
environment, is the one it holds (checked by COMPILE-PACKAGE-CHILD)."
  (let ((stat (and request (plusp (length request)) token (plusp (length token)) cache
                   (ignoring-errors-reported ("examining ~a" request) (sb-posix:lstat request))))
        (name (and request (tuatara.io:native-file-name request))))
    (and stat
         (= (logand (sb-posix:stat-mode stat) #o170000) #o100000)
         (= (logand (sb-posix:stat-mode stat) #o777) #o600)
         (= (sb-posix:stat-uid stat) (sb-posix:getuid))
         (uiop:string-prefix-p "compile-" name)
         (uiop:string-suffix-p name ".request")
         (equal (tuatara.io:real-path (tuatara.io:parent-path request))
                (tuatara.io:real-path cache)))))

(defun compile-package-child ()
  "Compile a package as the editor asked in a process of its own: the
request it wrote (PACKAGE-REQUEST-TEXT), named in the environment as
TUATARA_COMPILE_PACKAGE, taken only when PACKAGE-REQUEST-ALLOWED-P and
holding the token TUATARA_COMPILE_TOKEN gives; never from the command line.
The packages loaded in the editor are loaded first, one that fails left out,
then the package, each file compiled that changed since, as a load in the
editor would, into the same place (PACKAGE-FASL), nothing refused
\(REFUSE-PACKAGE). Returns the exit code: 0 when it loaded, 1 when it
failed, why printed, 2 when the request was not taken."
  (let ((request (tuatara.io:own-variable "TUATARA_COMPILE_PACKAGE"))
        (token (tuatara.io:own-variable "TUATARA_COMPILE_TOKEN"))
        (cache (tuatara.io:own-variable "TUATARA_COMPILE_CACHE")))
    (dolist (name '("TUATARA_COMPILE_PACKAGE" "TUATARA_COMPILE_TOKEN" "TUATARA_COMPILE_CACHE"))
      (sb-posix:unsetenv name))
    (unless (package-request-allowed-p request token cache)
      (format t "~&Not a request of the editor's: nothing compiled~%")
      (return-from compile-package-child 2))
    (handler-case
        (reporting-errors ("compiling as ~a asks" request)
                          (let ((plist (with-standard-io-syntax
                                         (let ((*read-eval* nil)) (read-from-string (tuatara.io:file-text request))))))
                            (unless (and (listp plist) (equal (getf plist :token) token))
                              (format t "~&Not a request of the editor's: nothing compiled~%")
                              (return-from compile-package-child 2))
                            (setf *tuatara-identity* (getf plist :identity)
                                  tuatara-user:*cache-directory* (getf plist :cache)
                                  tuatara-user:*packages-directory* (getf plist :packages))
                            (let ((*package-compiling* :apart))
                              (loop for (name directory file) in (getf plist :prelude)
                                    do (handler-case (reporting-errors ("loading the package ~a" name)
                                                                       (load-package-entry (pathname directory) name
                                                                                           file))
                                         (error (e) (format t "~&~a was left out: ~a~%" name e))))
                              (let ((before (structure-definitions)))
                                (load-package-entry (pathname (getf plist :directory)) (getf plist :name)
                                                    (getf plist :file))
                                (write-structures-file (structures-file request) (structures-made-since before))))
                            0))
      (serious-condition (e)
        (format t "~&~a~%" e)
        1))))
