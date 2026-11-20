;;;; test/fences.lisp --- What keeps the tests away from the developer's own files and settings.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded first with the tests, however they are run (tools/test.sh,
;;;; asdf:test-system, one test from a Lisp session): the tests never read the
;;;; developer's init file, note templates, capture templates, snippets or git
;;;; settings, never write their state (the recent files), their cache (compiled
;;;; packages, indexes, the debug log), their trashes (the home one or a drive's),
;;;; or their spelling word lists. The tests have a cache of their own, and each
;;;; run directories of its own for the rest, which REMOVE-TEST-DIRECTORIES takes
;;;; away. In a Lisp session that also runs the editor, loading the tests sets
;;;; these for that session.

(in-package #:tuatara.tests)

;; The config test binds the init file it reads, as the tests do not read the developer's.
;; Nor their own note templates: the formatted test binds its own.
(setf tuatara.defaults::*note-templates-directory* "/nonexistent/tuatara-test-note-templates/")
;; Nor their own capture templates, which file into their real files, nor their
;; snippets: the capture and snippets tests bind their own.
(setf tuatara-user:*capture-template-directories* (list "/nonexistent/tuatara-test-capture-templates/"))
(setf tuatara-user:*snippet-directories* (list "/nonexistent/tuatara-test-snippets/"))
;; The tests' own files lie in the temporary directory, which is untrusted as standard: none is, so they open as
;; the developer's files do; the tests of trust bind their own.
(setf tuatara-user:*untrusted-directories* '())
;; Nor write the developer's state (the list of recent files): a directory of
;; their own, removed when they are done.
(defvar *test-state-directory*
  (merge-pathnames (format nil "tuatara-test-state-~d/" (sb-posix:getpid)) (uiop:temporary-directory)))
;; Nor write in the developer's cache (their compiled packages, indexes, debug log):
;; the tests' own, beside it so on the same drive, kept from one run to the next for
;; what is slow to make again (the core packages are compiled apart in).
(defvar *test-cache-directory* (uiop:xdg-cache-home "tuatara-tests/"))
(ensure-directories-exist *test-cache-directory*)
(sb-posix:chmod (namestring *test-cache-directory*) #o700)
(setf tuatara-user:*cache-directory* (namestring *test-cache-directory*))
;; Nor list or open anything else in the developer's home: the tests have a made-up
;; home of their own, in their state directory, and their files and data home in it,
;; so that a path typed to a question never passes through the developer's home.
(defvar *real-home-names* (tuatara.io:home-directory-names)
  "The names of the developer's own home directory, as HOME gave it before the
tests' made-up home took its place and as the file system has it.")
(defvar *test-home-directory* (merge-pathnames "home/" *test-state-directory*)
  "The tests' made-up home directory: HOME and the XDG directories while the
tests run, and where WITH-TEST-DIRECTORY makes their files.")
(defvar *test-data-directory* (merge-pathnames ".local/share/" *test-home-directory*)
  "The tests' data home ($XDG_DATA_HOME), with their home trash, in their made-up home.")
;; Programs the tests start find Quicklisp where this Lisp found it, not in the made-up home.
(defvar *quicklisp-home*
  (let* ((package (find-package "QL"))
         (symbol (and package (find-symbol "*QUICKLISP-HOME*" package))))
    (and symbol (boundp symbol) (symbol-value symbol)))
  "The directory this Lisp loaded Quicklisp from; NIL when it has no Quicklisp.")
(unless (sb-posix:getenv "QUICKLISP_SETUP")
  (unless *quicklisp-home*
    (error "The tests need Quicklisp, which the programs they start load the editor with: load it before the ~
            tests, or name its setup.lisp in QUICKLISP_SETUP."))
  (sb-posix:setenv "QUICKLISP_SETUP" (sb-ext:native-namestring (merge-pathnames "setup.lisp" *quicklisp-home*)) 1))
(defun tree-git-directory ()
  "The git directory of the tree being tested when it lies elsewhere, as a
worktree's has it (its .git a file naming the directory); NIL otherwise."
  (let* ((tree (asdf:system-source-directory :tuatara))
         (file (merge-pathnames ".git" tree))
         (line (and (uiop:file-exists-p file) (not (uiop:directory-exists-p file))
                    (ignore-errors (first (uiop:read-file-lines file))))))
    (when (and line (uiop:string-prefix-p "gitdir: " line))
      (let ((named (string-trim " " (subseq line 8))))
        (uiop:ensure-directory-pathname
         (if (uiop:absolute-pathname-p named) named (merge-pathnames named tree)))))))
(defun root-mount-places ()
  "The other places the drive shown as the root is shown at, the same directory
of it, each ending with a slash: in a container, its root as the host keeps it,
which may lie in the developer's home."
  (let* ((mounts (ignore-errors (tuatara.defaults::mounts-shown)))
         (root (find "/" mounts :key #'third :test #'string= :from-end t)))
    (when root
      (loop for (device directory place) in mounts
            when (and (string= device (first root)) (string= directory (second root)) (string/= place "/"))
              collect (tuatara.io:slash-ended place)))))
(defvar *real-home-allowed*
  (let ((places (list* (asdf:system-source-directory :tuatara) (tree-git-directory) *test-cache-directory*
                       *quicklisp-home* (uiop:xdg-cache-home "common-lisp/") (root-mount-places))))
    (remove-duplicates (loop for place in (remove nil places)
                             for true = (ignore-errors (truename place))
                             collect (sb-ext:native-namestring place)
                             when true collect (sb-ext:native-namestring true))
                       :test #'string=))
  "The directories in the developer's home the tests may list and open, each a
native name ending with a slash: the tree being tested (but *REAL-HOME-KEPT-OUT*)
and its git directory, the tests' own cache, Quicklisp, the Lisp's compiled
files, the root's other places (ROOT-MOUNT-PLACES) and the package repositories
beside the tree (test/source-scan.lisp).")
(defvar *real-home-kept-out*
  (let ((notes (merge-pathnames ".project/" (asdf:system-source-directory :tuatara))))
    (remove-duplicates (remove nil (list (sb-ext:native-namestring notes)
                                         (ignore-errors (sb-ext:native-namestring (truename notes)))))
                       :test #'string=))
  "The directories in *REAL-HOME-ALLOWED* the tests may not reach all the same:
the untracked private notes of the tree being tested (.project).")
;; The developer's init file, configuration directory (their Word and LaTeX templates), state, trashes, GnuPG
;; keys and agent (a keyring of the tests' own, made empty), temporary directory (so whatever a test forgets goes
;; with the run) and directory of daemons' sockets (so no test reaches a running daemon) are the run's own, and the
;; variables naming their agents, display and session bus unset, from tools/fence.lisp, which the tools use too.
;; The home is made up, its directories where a home has them. Its cache directory leads to the tests' cache, kept
;; from one run to the next for the packages compiled apart there.
(load (asdf:system-relative-pathname :tuatara "tools/fence.lisp"))
(let ((cache (merge-pathnames ".cache" *test-home-directory*)))
  (ensure-directories-exist *test-home-directory*)
  (ensure-directories-exist (merge-pathnames "cache/" *test-cache-directory*))
  ;; made once: loaded again in the same Lisp, the link is there already
  (unless (ignore-errors (sb-posix:lstat (sb-ext:native-namestring cache)))
    (sb-posix:symlink (sb-ext:native-namestring (merge-pathnames "cache/" *test-cache-directory*))
                      (sb-ext:native-namestring cache))))
(funcall (find-symbol "FENCE-PERSON-DATA" "TUATARA.FENCE") *test-state-directory*
         :places (list (cons "HOME" "home/")
                       (cons "XDG_CONFIG_HOME" "home/.config/")
                       (cons "XDG_DATA_HOME" "home/.local/share/")
                       (cons "XDG_STATE_HOME" "home/.local/state/")
                       (cons "XDG_CACHE_HOME" "home/.cache/")
                       (cons "GNUPGHOME" "home/.gnupg/")))
(setf tuatara.defaults::*runtime-directory* (sb-posix:getenv "XDG_RUNTIME_DIR"))
(let ((no-drives (merge-pathnames "no-drives" *test-state-directory*)))
  (ensure-directories-exist no-drives)
  (with-open-file (out no-drives :direction :output :if-exists :supersede))
  (setf tuatara.defaults::*mounts-file* (namestring no-drives)))
;; Nor add to the developer's spelling word lists: enchant keeps the tests' own there.
(sb-posix:setenv "ENCHANT_CONFIG_DIR" (namestring (merge-pathnames "enchant/" *test-state-directory*)) 1)
;; Nor read the developer's git settings (their name, their signing, their
;; aliases): a global settings file of the tests' own, and none of the system's.
(let ((settings (merge-pathnames "gitconfig" *test-state-directory*)))
  (ensure-directories-exist settings)
  (with-open-file (out settings :direction :output :if-exists :supersede)
    (format out "[user]~%	name = Test~%	email = test@example.invalid~%[commit]~%	gpgsign = false~%[init]~%	defaultBranch = main~%"))
  (sb-posix:setenv "GIT_CONFIG_GLOBAL" (namestring settings) 1)
  (sb-posix:setenv "GIT_CONFIG_NOSYSTEM" "1" 1))
;; Nor read the developer's own agenda files (the clocks left open are looked for in them as
;; a session starts), and no tick works out notifications from any agenda files meanwhile
;; (the notification tests start theirs): the next is never due.
(setf tuatara.defaults::*org-agenda-files* (list "/nonexistent/tuatara-test-agenda/agenda.org"))
(setf (second tuatara.defaults::**org-notify-state**) most-positive-fixnum)
;; Nor copy changes not saved anywhere: the auto-save tests turn it on for themselves.
(setf tuatara-user:*auto-save* nil)
;; The fake frontend has no screen to flicker: work in the background is looked
;; at often, so that the tests do not wait a second for each tick.
(setf tuatara.command:*tick-interval* 1/20)
;; the file questions list their entries in plain order, not by what earlier tests chose
(setf tuatara-user:*last-used-first* nil)
;; a language's rules are compiled before its colours are checked, however long it takes
(setf tuatara.defaults::*rules-wait* 60
      tuatara.defaults::*rules-editor-wait* 60)
;; and the program parsing a file's text started before its first frame is
;; drawn, however busy the machine: a test reads the colours of that frame
;; (the tests of starting it on time bind their own wait)
(setf tuatara.io::*at-once-wait* 30)

(defun gnupg-homes-under (directory &optional (depth 4))
  "The GnuPG home directories within DIRECTORY, DEPTH levels down at most: those
holding a keyring, a trust database or secret keys, as gpg leaves where it ran."
  (when (and (plusp depth) (uiop:directory-exists-p directory))
    (let ((subdirectories (ignore-errors (uiop:subdirectories directory))))
      (append (and (or (probe-file (merge-pathnames "pubring.kbx" directory))
                       (probe-file (merge-pathnames "trustdb.gpg" directory))
                       (find "private-keys-v1.d" subdirectories
                             :key (lambda (d) (car (last (pathname-directory d)))) :test #'string=))
                   (list directory))
              (loop for d in subdirectories append (gnupg-homes-under d (1- depth)))))))

(defun stop-gnupg-homes-under (directory)
  "Stop the gpg-agent of each GnuPG home within DIRECTORY (GNUPG-HOMES-UNDER)
and take away the directory of its sockets that gpg made under /run/user, which
would otherwise outlast the home: one for each made-up home of every run."
  (dolist (home (gnupg-homes-under directory))
    (dolist (arguments '(("--kill" "gpg-agent") ("--remove-socketdir")))
      (ignore-errors (uiop:run-program (list* "gpgconf" "--homedir" (namestring home) arguments)
                                       :ignore-error-status t :output nil :error-output nil)))))

(defun remove-test-directories ()
  "Take away the directory this test run kept its state, made-up home and data
in, the gpg-agents of the keyrings in it stopped first (STOP-GNUPG-HOMES-UNDER)."
  (stop-gnupg-homes-under *test-state-directory*)
  (uiop:delete-directory-tree *test-state-directory* :validate t :if-does-not-exist :ignore))
;; however the tests are run, their directories go when Lisp exits
(pushnew 'remove-test-directories sb-ext:*exit-hooks*)

(defun remove-stale-test-directories ()
  "Take away the state, data and test directories of earlier test runs whose
Lisp has ended without taking them away (one killed, say): those named for a
process no longer running."
  (flet ((sweep (parent prefix)
           (dolist (dir (uiop:subdirectories parent))
             (let* ((name (car (last (pathname-directory dir))))
                    (pid (and (uiop:string-prefix-p prefix name)
                              (parse-integer name :start (length prefix) :junk-allowed t))))
               (when (and pid (not (probe-file (format nil "/proc/~d/" pid))))
                 (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))))
    (sweep (uiop:temporary-directory) "tuatara-test-state-")
    (sweep *test-cache-directory* "test-data-")
    (sweep *test-cache-directory* "test-")))
(remove-stale-test-directories)

;; The tests take the leader keys the starter init file offers (commented out
;; there, SPC being yours), as one who takes them does: the tests that
;; reach commands through SPC keep doing so, and the offered lines are seen to work.
(let ((*package* (find-package :tuatara-user)))
  (dolist (line (uiop:read-file-lines (asdf:system-relative-pathname :tuatara "data/init.lisp")))
    (let ((line (string-left-trim " " line)))
      (when (cl-ppcre:scan "^;; \\(bind-key \"[^\"]+\" (nil|'[a-z-]+) :map :leader( :mode '(?:[a-z-]+|\\([a-z -]+\\)))?( :label \"[^\"]*\")?\\)\\s*$" line)
        (eval (read-from-string (subseq line 3)))))))

(defun tests-own-path-p (path)
  "True when PATH is in a directory of the tests' own: their state directory,
with their made-up home, or their cache. Only such a directory is removed."
  (or (uiop:subpathp path *test-state-directory*) (uiop:subpathp path *test-cache-directory*)))

(defmacro with-environment-variable ((name value) &body body)
  "Run BODY with the environment variable NAME set to VALUE (NIL: unset), as it was after."
  (let ((before (gensym "BEFORE")))
    `(let ((,before (sb-posix:getenv ,name)))
       (if ,value (sb-posix:setenv ,name ,value 1) (sb-posix:unsetenv ,name))
       (unwind-protect (progn ,@body)
         (if ,before (sb-posix:setenv ,name ,before 1) (sb-posix:unsetenv ,name))))))

(defvar *real-home-reached* '()
  "Each time a test reached something in the developer's own home outside
*REAL-HOME-ALLOWED*, newest first, as (WHAT PATH TEST): WHAT is \"list\",
\"open\", \"examine\" or \"change\", TEST the name of the test running then
\(NIL in a thread of its own). The run fails when there is any.")

(defvar *real-home-lock* (sb-thread:make-mutex :name "real home reached")
  "Held while *REAL-HOME-REACHED* is added to.")

(defvar *test-running* nil
  "The name of the test running now, for every thread to see.")

(defmethod parachute:eval-in-context :around (context (result parachute:test-result))
  (let ((before *test-running*))
    (setf *test-running* (ignore-errors (parachute:name (parachute:expression result))))
    (unwind-protect (call-next-method)
      (setf *test-running* before))))

(defun real-home-path-p (path &optional what)
  "True when PATH, a pathname or a native name, is the developer's own home or
in it, and not in one of the places the tests may reach there. When WHAT is
\"examine\", a directory on the way to one of those places is not, nor what is
in such a directory, as making or finding one examines each directory above it
and what a project's root is told by there."
  (let* ((name (if (pathnamep path) (ignore-errors (sb-ext:native-namestring path)) path))
         (name (and (stringp name) (plusp (length name))
                    (if (char= #\/ (char name 0)) name (concatenate 'string (sb-posix:getcwd) "/" name))))
         (directory (and name (tuatara.io:slash-ended name))))
    (and directory
         (some (lambda (home) (uiop:string-prefix-p home directory)) *real-home-names*)
         (or (notany (lambda (allowed) (uiop:string-prefix-p allowed directory)) *real-home-allowed*)
             (some (lambda (kept) (uiop:string-prefix-p kept directory)) *real-home-kept-out*))
         (not (and (equal what "examine")
                   (let ((above (subseq directory 0 (1+ (position #\/ directory :end (1- (length directory))
                                                                               :from-end t)))))
                     (some (lambda (allowed) (uiop:string-prefix-p above allowed)) *real-home-allowed*)))))))

(defun note-real-home (what path)
  "Add PATH to *REAL-HOME-REACHED* as WHAT (\"list\", \"open\", \"examine\",
\"change\") when it is in the developer's own home (REAL-HOME-PATH-P), and
then return true."
  (when (real-home-path-p path what)
    (sb-thread:with-mutex (*real-home-lock*)
      (push (list what (if (pathnamep path) (namestring path) path) *test-running*) *real-home-reached*))
    t))

(defun guard-real-home (function what paths refused)
  "Watch FUNCTION, a function of the editor or the system that reaches files
by name. PATHS is a function of its arguments giving the paths that call is
to reach (the others it takes being descriptors or flags). When one of them is
in the developer's own home it is noted (NOTE-REAL-HOME) as WHAT, and the call
is not made: REFUSED, a function of the name of FUNCTION and that path,
gives what is returned in its place, as the system refuses one who may not
look there."
  (let ((name (string-downcase (symbol-name function))))
    (sb-int:encapsulate function 'real-home
                        (lambda (real &rest arguments)
                          (let ((reached (find-if (lambda (path) (note-real-home what path))
                                                  (apply paths arguments))))
                            (if reached
                                (funcall refused name reached)
                                (apply real arguments)))))))

(defun at-name (dirfd name)
  "As a list, the name NAME (a string, or octets) taken relative to the
directory open as DIRFD when it is a name from the current directory or a
whole path; none when it is relative to another directory, which was opened
by name, and watched, first."
  (let ((name (if (stringp name) name (and (plusp (length name)) (tuatara.text:octets-string name)))))
    (when (and name (or (eql dirfd tuatara.io::+at-fdcwd+) (and (plusp (length name)) (char= #\/ (char name 0)))))
      (list name))))

(defun syscall-refused (name path)
  "Signal what SB-POSIX signals when the system call NAME on PATH is refused leave (EACCES)."
  (declare (ignore path))
  (error 'sb-posix:syscall-error :name name :errno sb-posix:eacces))

(defun errno-refused (name path)
  "NIL and the errno of leave refused (EACCES), as a call NAME on PATH that gives them on failure returns."
  (declare (ignore name path))
  (values nil sb-posix:eacces))

(defun nothing-refused (name path)
  "NIL, as a call NAME on PATH that gives NIL on failure returns."
  (declare (ignore name path))
  nil)

(defun file-system-refused (operation)
  "A function that signals the editor's FILE-SYSTEM-ERROR for OPERATION (a
verb phrase) on the path it is given, refused leave (EACCES), as a call of
the editor that signals on failure does."
  (lambda (name path)
    (declare (ignore name))
    (tuatara.io::fail operation path sb-posix:eacces)))

;; every way the tests and the editor reach a file by name is watched: listing a directory (CL:DIRECTORY and UIOP,
;; SB-POSIX, the editor's own reading of one), opening (OPEN, SB-POSIX, the editor's openat), examining (PROBE-FILE,
;; SB-POSIX, statx, realpath, readlink, attributes, statvfs, inotify) and changing (Lisp's own, SB-POSIX, the
;; editor's links and modes). Lisp's file functions reach the system through SB-UNIX; the editor calls those of
;; SB-POSIX that take a name, not copied into it (src/package.lisp). Its other calls on names (renameat2, unlinkat,
;; mkdirat, utimensat, symlinkat) take a descriptor of a directory it opened by name, and so watched, first.
(flet ((first-one (path &rest more) (declare (ignore more)) (list path))
       (first-two (one two &rest more) (declare (ignore more)) (list one two))
       (second-one (one two &rest more) (declare (ignore one more)) (list two))
       (at (dirfd name &rest more) (declare (ignore more)) (at-name dirfd name)))
  (guard-real-home 'sb-impl::call-with-native-directory-iterator "list"
                   (lambda (iterate namestring &rest more) (declare (ignore iterate more)) (list namestring))
                   #'nothing-refused)
  (guard-real-home 'tuatara.io::map-directory-fd "list"
                   (lambda (each fd path) (declare (ignore each fd)) (list path))
                   (file-system-refused "read"))
  (guard-real-home 'sb-posix:opendir "list" #'first-one #'syscall-refused)
  (guard-real-home 'sb-unix:unix-open "open" #'first-one #'errno-refused)
  (dolist (function '(sb-unix:unix-stat sb-unix:unix-lstat sb-unix:unix-access sb-unix:unix-realpath
                      sb-unix:unix-readlink))
    (guard-real-home function "examine" #'first-one #'errno-refused))
  (dolist (function '(sb-unix:unix-mkdir sb-unix:unix-unlink))
    (guard-real-home function "change" #'first-one #'errno-refused))
  (guard-real-home 'sb-unix:unix-rename "change" #'first-two #'errno-refused)
  (dolist (function '(sb-posix:open sb-posix:creat))
    (guard-real-home function "open" #'first-one #'syscall-refused))
  (dolist (function '(sb-posix:stat sb-posix:lstat sb-posix:access sb-posix:readlink sb-posix:chdir))
    (guard-real-home function "examine" #'first-one #'syscall-refused))
  (dolist (function '(sb-posix:unlink sb-posix:rmdir sb-posix:mkdir sb-posix:mkfifo sb-posix:chmod sb-posix:chown
                      sb-posix:lchown sb-posix:truncate sb-posix:utime sb-posix:utimes))
    (guard-real-home function "change" #'first-one #'syscall-refused))
  (guard-real-home 'sb-posix:rename "change" #'first-two #'syscall-refused)
  (guard-real-home 'sb-posix:link "change" #'first-two #'syscall-refused)
  (guard-real-home 'sb-posix:symlink "change" #'second-one #'syscall-refused)
  (guard-real-home 'tuatara.io::openat-octets "open" #'at #'errno-refused)
  (guard-real-home 'tuatara.io::statx-fields "examine" #'at #'errno-refused)
  (guard-real-home 'tuatara.io::link-octets "examine" #'at #'nothing-refused)
  (dolist (function '(tuatara.io::real-path tuatara.io::statvfs-fields tuatara.io::path-xattr-p))
    (guard-real-home function "examine" #'first-one #'nothing-refused))
  (guard-real-home 'tuatara.io::inotify-watch "list" #'second-one #'nothing-refused)
  (guard-real-home 'tuatara.io::set-path-mode "change" #'first-one #'nothing-refused)
  (guard-real-home 'tuatara.io::make-hard-link "change" #'first-one (file-system-refused "give another name to")))

(defmacro with-real-home-unreached (&body body)
  "Run BODY, then check that nothing it did reached anything in the
developer's own home (*REAL-HOME-REACHED*), naming what it reached if so."
  (let ((before (gensym "BEFORE")))
    `(let ((,before *real-home-reached*))
       (multiple-value-prog1 (progn ,@body)
         (is equal '() (ldiff *real-home-reached* ,before) "the developer's own home was reached")))))
