;;;; lisp/packages/fetch.lisp --- Fetching a package in the background, and deciding what to do with it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package is fetched from its repository with git, only the commit and the
;;;; directory it needs, within a size limit; who signed it is checked, the version
;;;; taken and the one before it kept, and the package asked about and loaded once
;;;; the work in the background is done.

(in-package #:tuatara.packages)

;;; Fetching

(defun git-for-packages ()
  "The git program packages are fetched with, or an error saying it is missing."
  (or (tuatara.io:find-program "git")
      (tuatara.io:program-missing "git"
                                  :why (format nil
                                               "packages are fetched with it: ~a"
                                               (outside-program-install-words :git)))))

(defvar *package-plain-transport* nil
  "True while a package whose load-package allows http:// or git:// (:plain
t) is fetched; else git is not let use them, even when a server sends it
there.")

(defparameter *package-git-seconds* 900
  "The most seconds one git command fetching or checking a package may take
before it is ended.")

(defun package-git-environment ()
  "What git is given when it fetches and checks packages, what it is given to
push and pull now (GIT-NETWORK-ENVIRONMENT), and besides this. There are no
hooks, no submodules, no attributes file of yours, and none of the system's,
so that none of your own git's programs runs on a package. Symbolic links in a
package are checked out as small files of their target's name, never leading
outside it. The filters of git-lfs, which would ask other servers, are left
out. ftp is never used, and http:// and git:// are not unless
*PACKAGE-PLAIN-TRANSPORT*, however a server redirects."
  (append (apply #'git-network-environment
                 '("core.symlinks" "false") '("filter.lfs.smudge" "") '("filter.lfs.process" "")
                 '("filter.lfs.required" "false") '("core.hooksPath" "/dev/null") '("submodule.recurse" "false")
                 '("core.attributesFile" "/dev/null") '("protocol.ftp.allow" "never") '("protocol.ftps.allow" "never")
                 (unless *package-plain-transport*
                   '(("protocol.http.allow" "never") ("protocol.git.allow" "never"))))
          (list "GIT_LFS_SKIP_SMUDGE=1" "GIT_ATTR_NOSYSTEM=1")))

(defparameter *package-attributes*
  (format nil "* -filter -text -eol -ident -working-tree-encoding~%")
  "The attributes every file of a package is checked out with, before the
repository's own: no filter program, no change of line ends, encoding or
$Id$, so that its files are written as they are.")

(defun package-git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY for a package's fetching: what it
printed, as text; an error with what it said when it fails, or when it takes
more than *PACKAGE-GIT-SECONDS*. A package's own scratch repository may fetch
the files a sparse checkout needs from its address as it goes. A repository
made by init is given *PACKAGE-ATTRIBUTES* as its own."
  (multiple-value-bind (octets ok too-large said)
      (let ((*git-safety* (remove "--no-lazy-fetch" *git-safety* :test #'string=)))
        (run-git-program (git-for-packages) directory arguments :environment (package-git-environment) :errors t
                         :seconds *package-git-seconds*))
    (declare (ignore too-large))
    (unless ok
      (refuse "~a" (git-failure-words (first arguments) (or said ""))))
    (when (equal (first arguments) "init")
      (let ((info (format nil "~a.git/info/" (tuatara.io:native directory))))
        (tuatara.io:ensure-private-directory info)
        (tuatara.io:write-private-file (concatenate 'string info "attributes") *package-attributes*)))
    (if octets (tuatara.text:octets-string octets) "")))

(defun package-commit-name (text where)
  "TEXT, a commit's name git printed for the repository at WHERE, checked to be
one (GIT-HASH-P, 7 to 64 hexadecimal digits); an error otherwise."
  (let ((name (string-trim '(#\Newline #\Space #\Tab) text)))
    (unless (git-hash-p name :short t)
      (refuse "~a gave no commit's name: ~s" where (subseq name 0 (min 80 (length name)))))
    name))

(defun package-commit (directory where version)
  "The commit, and its tag or NIL, VERSION of the repository at WHERE is,
asking it from DIRECTORY, a repository of no commit made for it. \"latest\" is
its main branch's newest commit. \"release\" is its newest tag by date, its
name never read. NIL is the same, or its newest commit when it has no tag.
Anything else is that tag, branch or commit as written. Every commit's name is
checked to be one (PACKAGE-COMMIT-NAME)."
  (labels ((first-word (text)
             (first (uiop:split-string (string-trim '(#\Newline #\Space) text) :separator '(#\Space #\Tab))))
           (newest-commit ()
             (values (package-commit-name (or (first-word (package-git directory "ls-remote" where "HEAD"))
                                              (refuse "~a has no main branch" where))
                                          where)
                     nil))
           (newest-tag ()
             ;; the tags' commits alone, to find the newest by date
             (package-git directory "fetch" "--quiet" "--depth=1" "--filter=tree:0" where "+refs/tags/*:refs/tags/*")
             (let* ((line (string-trim '(#\Newline #\Space)
                                       (package-git directory "for-each-ref" "--sort=-creatordate" "--count=1"
                                                    "--format=%(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end) %(refname:strip=2)"
                                                    "refs/tags")))
                    (space (position #\Space line)))
               (values (package-commit-name (subseq line 0 space) where) (subseq line (1+ space))))))
    (cond ((equal version "latest") (newest-commit))
          ((member version '(nil "release") :test #'equal)
           (cond ((plusp (length (string-trim '(#\Newline #\Space) (package-git directory "ls-remote" "--tags" where))))
                  (newest-tag))
                 ((null version) (newest-commit))
                 (t (refuse "~a has no tagged version; :version \"latest\" takes its newest commit" where))))
          (t
           (let* ((listed (package-git directory "ls-remote" where
                                       (format nil "refs/tags/~a" version) (format nil "refs/tags/~a^{}" version)
                                       (format nil "refs/heads/~a" version)))
                  (lines (tuatara.text:split-words listed '(#\Newline)))
                  (peeled (find-if (lambda (line) (search "^{}" line)) lines)))
             (cond (lines (values (package-commit-name (first-word (or peeled (first lines))) where)
                                  (and (search "refs/tags/" (or peeled (first lines))) version)))
                   ((git-hash-p version) (values version nil))
                   ((git-hash-p version :short t)
                    (refuse "~a has no tag or branch called ~a, and a commit is fetched by its whole name, ~
                            40 hexadecimal digits (git log shows it)" where version))
                   (t (refuse "~a has no tag, branch or commit called ~a" where version))))))))

(defun delete-package-directory (directory)
  "Delete DIRECTORY, with all in it, or a file, when it is inside the directories
packages are kept or compiled in, nothing it holds followed when it is a link (DELETE-TREE);
an error for any other directory."
  (let ((native (tuatara.io:native directory)))
    (unless (or (tuatara.io:path-within-p native (tuatara.io:native (packages-directory)))
                (tuatara.io:path-within-p native (packages-compiled-directory)))
      (refuse "~a is no directory of the packages': not deleted" (abbreviated-path native)))
    (tuatara.io:delete-tree native)))

(define-setting *package-size-limit* (* 512 1024 1024)
  "The most octets a package may take on disk while it is fetched, what git
keeps of its history and its files together: a fetch that grows past it is
ended and its files deleted. Raise it for a package with a long history;
lower it to refuse large fetches, as on a metered link.
- a whole number: that many octets, 512 MB as standard."
  :values (integer 0 *))

(define-condition package-too-large (error)
  ((where :initarg :where :reader package-too-large-where))
  (:report (lambda (condition stream)
             (format stream "~a takes more than ~a: the package was not fetched"
                     (package-too-large-where condition) (tuatara.io:octets-words *package-size-limit*))))
  (:documentation "Signalled when a package fetched from WHERE grows past *PACKAGE-SIZE-LIMIT*."))

(setf (documentation 'package-too-large-where 'function) "Where the package too large was fetched from.")

(defun fetch-packs-size (directory)
  "The octets the packs of the repositories directly in DIRECTORY take, where
git puts what it fetches as it comes."
  (loop for (name kind) in (tuatara.io:directory-entries directory)
        when (eq kind :directory)
          sum (or (tuatara.io:tree-size (format nil "~a~a/.git/objects/pack/" directory name)) 0)))

(defun call-within-package-size (directory where thunk)
  "Call THUNK, which fetches into DIRECTORY on the thread of a job, and look at
how much DIRECTORY takes meanwhile: what git has fetched every second, all
its files every five seconds and at the end. Past *PACKAGE-SIZE-LIMIT*, the
program the job runs is ended and PACKAGE-TOO-LARGE signalled, also when
THUNK has ended by then."
  (let* ((job tuatara.runtime:*job*) (most *package-size-limit*)
         (too-large nil)
         (looks 0)
         (timer (sb-ext:make-timer (lambda ()
                                     (when (and (not too-large)
                                                (> (if (zerop (mod (incf looks) 5))
                                                       (or (tuatara.io:tree-size directory) 0)
                                                       (fetch-packs-size directory))
                                                   most))
                                       (setf too-large t)
                                       (let ((process (and job (tuatara.runtime:job-process job))))
                                         (when process
                                           (ignoring-errors-reported ("ending the fetching of ~a" where)
                                                                     (sb-ext:process-kill process 15))))))
                                   :thread t)))
    (flet ((too-large () (error 'package-too-large :where where)))
      (sb-ext:schedule-timer timer 1 :repeat-interval 1)
      (multiple-value-prog1
          (unwind-protect (handler-bind ((error (lambda (e) (declare (ignore e)) (when too-large (too-large)))))
                            (funcall thunk))
            (sb-ext:unschedule-timer timer))
        (when (or too-large (> (or (tuatara.io:tree-size directory) 0) most)) (too-large))))))

(defparameter *package-versions-tried* 10
  "How many older versions of a package are tried, the newest first, when the
one asked for needs a newer Tuatara than this one: the first that fits is
taken.")

(defparameter *package-history-depth* 100
  "How many commits of a repository's main branch are looked through for an
older version of a package when its newest commit needs a newer Tuatara.")

(defun package-entry-paths (name file)
  "The paths in its repository the package NAME may be entered by: FILE,
NAME.lisp or load.lisp, in its directory (PACKAGE-REPOSITORY-DIRECTORY) or at the
top."
  (let ((inside (package-repository-directory name)))
    (loop for top in (list (format nil "~a/" inside) "")
          append (loop for entry in (list file (format nil "~a.lisp" name) "load.lisp")
                       when entry collect (concatenate 'string top entry)))))

(defun package-older-versions (probe where version tag name file)
  "The versions of the repository at WHERE older than the one VERSION found,
the newest first, each (COMMIT . TAG), at most *PACKAGE-VERSIONS-TRIED*,
asked from PROBE, the repository it was found in (PACKAGE-COMMIT). For TAG,
the newest tag, the tags made before it; for the newest commit, the newest
commit of each earlier state of the package NAME's entry file
\(PACKAGE-ENTRY-PATHS) on the main branch, within *PACKAGE-HISTORY-DEPTH*
commits; for a tag, branch or commit named, none."
  (flet ((lines (text) (tuatara.text:split-words text '(#\Newline))))
    (cond ((not (member version '(nil "release" "latest") :test #'equal)) '())
          (tag
           (loop for line in (lines (package-git probe "for-each-ref" "--sort=-creatordate"
                                                 (format nil "--count=~d" (1+ *package-versions-tried*))
                                                 "--format=%(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end) %(refname:strip=2)"
                                                 "refs/tags"))
                 for space = (position #\Space line)
                 when (and space (git-hash-p (subseq line 0 space) :short t) (string/= (subseq line (1+ space)) tag))
                 collect (cons (subseq line 0 space) (subseq line (1+ space)))))
          (t
           (package-git probe "fetch" "--quiet" (format nil "--depth=~d" *package-history-depth*) "--filter=blob:none"
                        where "HEAD")
           (let ((commits (remove-if-not #'git-hash-p
                                         (lines (package-git probe "rev-list" "--first-parent"
                                                             (format nil "--max-count=~d" *package-history-depth*)
                                                             "FETCH_HEAD"))))
                 (found '()))
             (dolist (change (lines (apply #'package-git probe "log" "--first-parent" "--format=%H"
                                           (format nil "--max-count=~d" *package-history-depth*) "FETCH_HEAD" "--"
                                           (package-entry-paths name file))))
               (let ((older (second (member change commits :test #'string=))))
                 (when (and older (< (length found) *package-versions-tried*))
                   (pushnew (cons older nil) found :key #'car :test #'string=))))
             (nreverse found))))))

(defun built-on (probe where floor versions)
  "Those of VERSIONS, each (COMMIT . TAG) of the repository at WHERE, that are
FLOOR or built on it (git merge-base --is-ancestor), asked from PROBE, the
repository they were found in, their history fetched without files first;
none when FLOOR is not in that history."
  (package-git probe "fetch" "--quiet" "--filter=tree:0" "--depth=2147483647" where "HEAD" "+refs/tags/*:refs/tags/*")
  (remove-if-not (lambda (version)
                   (or (string= (car version) floor)
                       (ignoring-errors-reported ("asking whether ~a is built on ~a" (car version) floor)
                                                 (package-git probe "merge-base" "--is-ancestor" floor (car version))
                                                 t)))
                 versions))

(define-record (package-fetching (:constructor make-package-fetching (name where scratch)) (:copier nil))
  "A fetch of the package NAME from the repository at WHERE: SCRATCH, the
directory it works in until it is done, which is deleted after."
  (name "" :type string :read-only t :documentation "The name of the package FETCHING fetches.")
  (where "" :type string :read-only t :documentation "The repository FETCHING fetches from.")
  (scratch "" :type string :read-only t :documentation "The directory FETCHING works in."))

(defun check-out-package (fetching files commit tag filter)
  "Check COMMIT of the package FETCHING fetches out in the directory FILES, by
its TAG when it has one: the package's own directory only when the repository
has it, as FILTER true asks for a partial clone, within the size watched."
  (call-within-package-size
   (package-fetching-scratch fetching) (package-fetching-where fetching)
   (lambda ()
     (tuatara.io:ensure-private-directory files)
     (package-git files "init" "--quiet")
     (apply #'package-git files "fetch" "--quiet" "--depth=1"
            (append (and filter '("--filter=blob:none"))
                    (list (package-fetching-where fetching) (if tag (format nil "refs/tags/~a" tag) commit))))
     (let* ((inside-name (package-repository-directory (package-fetching-name fetching)))
            (inside (string-trim '(#\Newline #\Space)
                                 (package-git files "ls-tree" "-d" "--name-only" commit inside-name))))
       (when (string= inside inside-name)
         (package-git files "sparse-checkout" "set" "--no-cone" (format nil "/~a/" inside-name))))
     (package-git files "checkout" "--quiet" commit))))

(defun partial-clone-failure-p (condition)
  "True when CONDITION, a failure of git, is one a server or git that cannot
make a partial clone gives: the words say so."
  (let ((words (princ-to-string condition)))
    (and (some (lambda (word) (search word words)) '("filter" "promisor" "partial clone")) t)))

(defun check-out-package-fully (fetching files commit tag)
  "Check COMMIT out as CHECK-OUT-PACKAGE does, a partial clone first, the whole
repository when a failure of that kind says the partial one cannot be had.
The directory the files are in, FILES or another beside it."
  (handler-case (reporting-errors ("checking out ~a" commit) (check-out-package fetching files commit tag t) files)
    (refusal (e)
      (unless (partial-clone-failure-p e) (error e))
      (delete-package-directory files)
      (let ((whole (format nil "~awhole-~a/" (package-fetching-scratch fetching) commit)))
        (check-out-package fetching whole commit tag nil)
        whole))))

(defun package-tag-repository (fetching commit tag)
  "A repository of its own holding TAG as fetched now, apart from the directory
kept for COMMIT, within the size watched."
  (let ((repository (format nil "~atag-~a/" (package-fetching-scratch fetching) commit)))
    (call-within-package-size
     (package-fetching-scratch fetching) (package-fetching-where fetching)
     (lambda ()
       (tuatara.io:ensure-private-directory repository)
       (package-git repository "init" "--quiet")
       (package-git repository "fetch" "--quiet" "--depth=1" "--filter=blob:none"
                    (package-fetching-where fetching) (format nil "refs/tags/~a" tag))))
    repository))

(defun kept-package-directory (fetching commit tag file)
  "The directory COMMIT of the package FETCHING fetches is kept in, fetched
now when it is not yet, as three values: that directory, or NIL when the
version needs a newer Tuatara (UNFITTING-VERSION, FILE its entry when given),
that Tuatara, and the repository to check its signature in."
  (let* ((name (package-fetching-name fetching))
         (where (package-fetching-where fetching))
         (directory (package-directory-for where name commit)))
    (if (probe-file directory)
        (let ((needed (unfitting-version (package-directory-in directory name) name file))
              (looked (if tag (package-tag-repository fetching commit tag) directory)))
          (if needed (values nil needed looked) (values directory nil looked)))
        (let* ((files (check-out-package-fully fetching (format nil "~afiles-~a/" (package-fetching-scratch fetching)
                                                                commit)
                                               commit tag))
               (needed (unfitting-version (package-directory-in files name) name file)))
          (cond (needed (values nil needed files))
                (t (tuatara.io:ensure-private-directory (directory-namestring (string-right-trim "/" directory)))
                   ;; a fetch of the same commit beside this one may have kept it first
                   (handler-case (sb-posix:rename (string-right-trim "/" files) (string-right-trim "/" directory))
                     (sb-posix:syscall-error (e) (unless (probe-file directory) (error e))))
                   (values directory nil directory)))))))

(defun fetch-package (name where version &key file floor walk-p signature-of)
  "Fetch the package called NAME, VERSION of the repository at WHERE: that
commit alone, and its directory (PACKAGE-REPOSITORY-DIRECTORY) alone when it has
one, else the whole of it; a repository that gives no file by itself after
its directories, that commit whole; never more than *PACKAGE-SIZE-LIMIT*
\(CALL-WITHIN-PACKAGE-SIZE), finding the commit as well as checking it out.
When that version needs a newer Tuatara than this one (UNFITTING-VERSION,
FILE its entry when given), older ones are tried, the newest first
\(PACKAGE-OLDER-VERSIONS), and the first that fits is kept; one that does not
fit is not. None older than FLOOR, the commit taken now, is tried, nor any
not built on it; and none at all when WALK-P, a function of the directory the
version is in, its commit and its tag, says no (its signer not trusted).
Returns the directory it is kept in, its commit, its tag, and, when the version
asked for was passed over, (VERSION NEEDED UNTRUSTED): its tag or commit, the
Tuatara it needs, and true when WALK-P said no; the directory NIL when no version
tried fits. SIGNATURE-OF, a function of a repository holding the version
taken, its commit and its tag (as fetched now: for a commit kept before, the
tag is fetched apart, into a repository of its own), gives the fifth value."
  (let* ((fetching (make-package-fetching name where (format nil "~a.fetching-~a/" (packages-directory)
                                                             (tuatara.io:random-name))))
         (scratch (package-fetching-scratch fetching))
         (probe (format nil "~aprobe/" scratch)))
    (tuatara.io:ensure-private-directory probe)
    (flet ((taken (directory commit tag looked passed)
             (values directory commit tag passed (and signature-of (funcall signature-of looked commit tag)))))
      (unwind-protect
           (multiple-value-bind (commit tag)
               (call-within-package-size scratch where
                                         (lambda ()
                                           (package-git probe "init" "--quiet")
                                           (package-commit probe where version)))
             (multiple-value-bind (directory needed looked) (kept-package-directory fetching commit tag file)
               (cond
                 (directory (taken directory commit tag looked nil))
                 ((and walk-p (not (funcall walk-p looked commit tag)))
                  (values nil nil nil (list (or tag (subseq commit 0 7)) needed t)))
                 (t
                  (let ((passed (list (or tag (subseq commit 0 7)) needed nil)))
                    (dolist (older (call-within-package-size
                                    scratch where
                                    (lambda ()
                                      (let ((older (package-older-versions probe where version tag name file)))
                                        (if floor (built-on probe where floor older) older))))
                             (values nil nil nil passed))
                      (multiple-value-bind (directory needed looked)
                          (kept-package-directory fetching (car older) (cdr older) file)
                        (declare (ignore needed))
                        (when directory
                          (return (taken directory (car older) (cdr older) looked passed))))))))))
        (when (probe-file scratch)
          (delete-package-directory scratch))))))

(defun package-repository-directory (name)
  "The directory the package NAME is in within its repository or source directory:
its load-package's :directory, else NAME."
  (or (fifth (declared-package name)) name))

(defun package-directory-in (directory name)
  "The directory of the package NAME within DIRECTORY, a repository's or a source
directory: its own directory there (PACKAGE-REPOSITORY-DIRECTORY) when it has one, else
DIRECTORY itself."
  (let ((inside (format nil "~a~a/" directory (package-repository-directory name))))
    (if (probe-file inside) inside directory)))

(defun package-directory-of (name where entry)
  "The directory the package NAME from WHERE is loaded from (PACKAGE-DIRECTORY-IN):
within WHERE when it is a directory of your disk, else within the commit ENTRY,
what is remembered of it, says was fetched; NIL when none was."
  (cond ((local-source-p where)
         (package-directory-in (directory-name where) name))
        ((and entry (string/= (second entry) "-"))
         (package-directory-in (package-directory-for where name (second entry)) name))))

;;; Keeping the version taken and the one before it

(defun prune-package-versions (name where)
  "Delete what is kept and compiled of the versions of the package NAME from
WHERE that are no longer wanted: all but the one taken and the one before it
of each version asked for (*FETCHED-PACKAGES*), and the one loaded this
session, never deleted under the editor. Another editor running beside this
one may have loaded a version deleted here: it goes on running what it
loaded, and fetches that version again when it next needs it."
  (let* ((prefix (format nil "~a ~a " name where))
         (keep (loop for entry in (state-list-entries *fetched-packages*)
                     when (uiop:string-prefix-p prefix (first entry))
                     collect (second entry)
                     and when (fifth entry) collect (fifth entry)))
         (loaded (second (assoc name *loaded-package-directories* :test #'string=))))
    (dolist (directory (ignoring-errors-reported ("reading the directories of ~a" name)
                                                 (uiop:subdirectories (package-kept-directory where name))))
      (let ((native (tuatara.io:native directory))
            (commit (car (last (pathname-directory directory)))))
        (unless (or (not (git-hash-p commit :short t))
                    (member commit keep :test #'string=)
                    (and loaded (tuatara.io:path-within-p loaded native)))
          (let ((compiled (directory-namestring (package-fasl (concatenate 'string native "x")))))
            (when (probe-file compiled) (delete-package-directory compiled)))
          (delete-package-directory native))))))

(defvar *packages-in-hand* '()
  "The packages being fetched or checked, whose version waits to be decided
on (asked about), or being compiled and loaded in the background
\(START-PACKAGE-LOAD), each (NAME . JOB): no other fetch or check of one
starts meanwhile.")

(defvar *package-job-deciding* nil
  "The fetch or check whose version is being decided on (PACKAGE-FETCHED), or NIL.")

(defun package-version-words (commit tag)
  "A version of a package as it is said: TAG, or the first seven characters of COMMIT."
  (or tag (subseq commit 0 (min 7 (length commit)))))

(defun package-awaits-restart-p (name)
  "True when the package NAME runs in this session from another version than
the one taken for it (*FETCHED-PACKAGES*), which loads at the next start, or
runs parts of two versions (*PACKAGES-MIXED*), which only a new start mends."
  (let ((declared (declared-package name))
        (loaded (second (assoc name *loaded-package-directories* :test #'string=))))
    (and (package-loaded-p name)
         (or (and (member name *packages-mixed* :test #'string=) t)
             (and declared loaded
                  (let* ((where (package-where (second declared)))
                         (entry (fetched-package (package-key name where (third declared))))
                         (directory (and entry (fourth entry) (package-directory-of name where entry))))
                    (and directory (string/= loaded (tuatara.io:native directory)))))))))

(defun load-version (name where directory file before &optional then)
  "Load the package NAME from DIRECTORY, a version of WHERE. When it fails and
BEFORE, what was remembered of the version taken before it (as
*FETCHED-PACKAGES* has it), names one that is kept, that one is remembered
again as the one taken, and said. It is loaded in its place once who signed it
is checked against the trust in force now (START-PACKAGE-CHECK). A version
that cannot be loaded over the one running (PACKAGE-NEEDS-RESTART) is no
failure: it stays the one taken, loaded at the next start. One that failed
part of the way over the version running (*PACKAGES-MIXED*) leaves parts of
both running: the version before it is remembered again as the one taken,
loaded at the next start, and you are told to restart. Gives :TAKEN and the
file loaded, :RESTART for one loaded at the next start, said when THEN is
not given, :MIXED for one that failed part of the way, :PREVIOUS when the
version before it is being checked, or NIL. When THEN is given, a function of them, it is called with them once the
package has loaded, which may be later, in the background
\(LOAD-PACKAGE-DIRECTORY). A loading stopped on purpose falls back on nothing.
It returns what THEN returns. Without THEN it returns them, or NIL
when it loads later."
  (load-package-directory
   directory name file
   (lambda (loaded &optional how)
     (multiple-value-call (or then #'values)
       (cond
         (loaded (values :taken loaded))
         ((eq how :restart)
          (unless then (say-package-needs-restart name))
          :restart)
         ((member name *packages-mixed* :test #'string=)
          (when before (state-list-put *fetched-packages* (list before)))
          (message "The new version of the package ~a failed part of the way as it loaded (~a): this session now ~
                    runs parts of two versions, so restart Tuatara~@[; the version before it, ~a, loads then~]"
                   name (cdr (assoc name *package-failures* :test #'string=))
                   (and before (package-version-words (second before) (third before))))
          :mixed)
         (t
          (let ((back (and before (not how) (package-directory-of name where before))))
            (when (and back (probe-file back))
              (state-list-put *fetched-packages* (list before))
              (message "The package ~a could not be loaded (~a): the version before it, ~a, is loaded in its place ~
                        once who signed it is checked"
                       name (cdr (assoc name *package-failures* :test #'string=))
                       (package-version-words (second before) (third before)))
              (when *package-job-deciding*
                (let-package-go *package-job-deciding*))
              (start-package-check name where nil file (first before) before :quiet t)
              :previous))))))))

(defun refuse-package-version (name where words &optional remember)
  "Refuse the version of the package NAME fetched from WHERE, WORDS saying
why: said, with how to take it all the same; the version loaded now kept,
the one refused deleted; REMEMBER, an entry of *PACKAGE-TRUST*, remembered
when given."
  (when remember
    (state-list-put *package-trust* (list remember)))
  (let* ((loaded (package-loaded-p name))
         (text (format nil "The package ~a from ~a is refused: ~a" name where words)))
    (unless loaded (note-package-failure name text))
    (message "~a. ~:[It is not loaded~;The version loaded now is kept~]; ~a" text loaded
             (if (package-signers-named where)
                 "its :signers in the init file say whose versions are taken"
                 (format nil
                         "~a trusts its source anew, as the first time"
                         (substitute-command-keys "\\[package-trust-again]"))))
    (prune-package-versions name where)))

;;; Fetching in the background

(define-record (package-fetch (:include tuatara.runtime:job)
                              (:constructor make-package-fetch (name where version file key)) (:copier nil))
  "The package NAME being fetched from WHERE, VERSION asked for, FILE its
entry when given, KEY what it is remembered by; once done, its DIRECTORY,
COMMIT and TAG, PASSED the version passed over for a newer Tuatara and the
one it needs (FETCH-PACKAGE), and SIGNATURE who signed it
\(PACKAGE-SIGNATURE). A package compiled and loaded in the background
\(START-PACKAGE-LOAD) has LOAD, the function loading it, and THEN, called
with the file it was loaded by and the error that ended it once it has
loaded; meanwhile LOADED and FAILURE keep those, and COMPILED is true once a
file of it is compiled. CHECKING is true for a check of who signed a version
kept (START-PACKAGE-CHECK), which fetches nothing."
  (name nil :documentation "The name of the package PACKAGE-FETCH fetches.")
  (where nil :documentation "Where the package of PACKAGE-FETCH is fetched from.")
  (version nil :documentation "The version of the package asked for.")
  (file nil :documentation "The entry of the package, when one was given.")
  (key nil :documentation "What the package is remembered by.")
  (directory nil :documentation "The directory of the package once fetched.")
  (commit nil :documentation "The commit of the package once fetched.")
  (tag nil :documentation "The tag of the package once fetched.")
  (passed nil
          :documentation "The version passed over for a newer Tuatara and the one the package needs (FETCH-PACKAGE).")
  (signature nil :documentation "Who signed the package (PACKAGE-SIGNATURE).")
  (load nil
        :documentation "The function loading the package, for one compiled and loaded in the background
\(START-PACKAGE-LOAD).")
  (then nil
        :documentation "The function called once the package has loaded, given the file it was loaded by and the error
that ended it.")
  (loaded nil :documentation "The file the package was loaded by, kept for THEN.")
  (failure nil :documentation "The error that ended the load of the package, kept for THEN.")
  (compiled nil :documentation "True once a file of the package is compiled.")
  (checking nil
            :documentation "True for a check of who signed a version kept, with nothing fetched
\(START-PACKAGE-CHECK)."))

(tuatara.runtime:define-job-words package-fetch
    :doing (cond ((package-fetch-load job) "Compiling and loading a package")
                 ((package-fetch-checking job) "Checking who signed a package")
                 (t "Fetching a package"))
    :verb (cond ((package-fetch-load job) "load the package")
                ((package-fetch-checking job) "check the package")
                (t "fetch the package"))
    :description (format nil
                         "~a the package ~a"
                         (cond ((package-fetch-load job) "Compiling and loading")
                               ((package-fetch-checking job) "Checking who signed")
                               (t "Fetching"))
                         (package-fetch-name job))
    :left-words (cond ((package-fetch-load job) "the package not loaded")
                      ((package-fetch-checking job) "the package not checked")
                      (t "the package not fetched"))
    :at-exit :quiet)

(defmethod tuatara.runtime:job-holds-queue-p ((job package-fetch))
  "NIL: the jobs of a buffer queued behind it need not wait for a package's fetching."
  nil)

(defun take-package-in-hand (job &key quiet)
  "Hold the package of JOB in hand (*PACKAGES-IN-HAND*) and return true.
Return NIL when a fetch or check of it runs already, its question waits for an
answer or it is being loaded. That is said on the echo row unless QUIET (a
load at the start, or by one of its commands or modes, which the version being
decided on serves), else in *Messages* alone."
  (let ((name (package-fetch-name job)))
    (if (assoc name *packages-in-hand* :test #'string=)
        (let ((words (format nil
                             "~a: a fetch or check of it is already running, or waits for an answer, or it is being loaded" name)))
          (if quiet (log-message words) (message "~a" words))
          nil)
        (progn (push (cons name job) *packages-in-hand*)
               t))))

(defvar *package-update-round* '()
  "The names of the packages \\[package-update] fetches again now, more than one,
for the echo area to say when none of them is in hand any more; NIL else.")

(defun let-package-go (job)
  "Let the package JOB holds in hand go (TAKE-PACKAGE-IN-HAND), when JOB still
holds it. When that was the last of *PACKAGE-UPDATE-ROUND* in hand, the update
is said to be done once what is doing now has said its own (SAY-PACKAGE-UPDATE-DONE)."
  (setf *packages-in-hand* (remove job *packages-in-hand* :key #'cdr))
  (let ((round *package-update-round*))
    (when (and round (notany (lambda (name) (assoc name *packages-in-hand* :test #'string=)) round))
      (setf *package-update-round* '())
      (defer-action (lambda () (say-package-update-done round))))))

(defun say-package-update-done (names)
  "Say that the update of the packages called NAMES is done, which of them
are not loaded, *Messages* holding why, and which load the new version taken
at the next start (PACKAGE-AWAITS-RESTART-P)."
  (let ((missing (remove-if #'package-loaded-p names))
        (restart (remove-if-not #'package-awaits-restart-p names)))
    (if missing
        (message "The package update is done: ~d of ~d loaded; not loaded: ~{~a~^, ~} (\\[view-echo-area-messages] ~
                  says why)~@[; restart Tuatara to use the new ~{~a~^, ~}~]"
                 (- (length names) (length missing)) (length names) missing restart)
        (message "The package update is done: the ~d packages are loaded~@[; restart Tuatara to use the new ~
                  ~{~a~^, ~}~]"
                 (length names) restart))))

(defun start-package-job (job work thread &key quiet)
  "Start JOB, a fetch or check of a package, in the background on a thread
called THREAD, WORK its function: its package held in hand until its version
is decided on, and counted as being fetched until the job ends. NIL, nothing
started, when the package is in hand already (said unless QUIET:
TAKE-PACKAGE-IN-HAND); let go again when the job cannot be started."
  (when (take-package-in-hand job :quiet quiet)
    (let ((name (package-fetch-name job)) (started nil))
      (pushnew name *fetching-packages* :test #'string=)
      (unwind-protect
           (progn (tuatara.runtime:start-job-thread job work :name thread)
                  (setf started t)
                  job)
        (unless started
          (let-package-go job)
          (setf *fetching-packages* (remove name *fetching-packages* :test #'string=)))))))

(defun start-package-load (name where load compile then)
  "Load the package NAME, of WHERE, in the background. A job calls COMPILE, a
function of it, which compiles the package apart from the editor
\(COMPILE-PACKAGE-APART), the editor going on with your keys meanwhile. Then
LOAD is run, the editor waiting between two commands all the while it runs
\(CALL-WITH-EDITOR-HELD), so that no command runs with the package part loaded,
nor compiles any of it there (*PACKAGE-COMPILING* :REFUSED). When it could not
be compiled apart, what is left to compile is compiled as it loads, the editor
waiting for that too. A package loaded already whose new version lays out a
structure otherwise (STRUCTURES-IN-THE-WAY) is not loaded at all: the error
that ends it is a PACKAGE-NEEDS-RESTART. The package is held in hand meanwhile
\(*PACKAGES-IN-HAND*), taken over from the fetch or check deciding on it. While
another fetch, check or loading holds it, nothing starts, said
\(TAKE-PACKAGE-IN-HAND). Once it has loaded, THEN is called on the editor's
thread with the file it was loaded by, or NIL, the error that ended it, or
NIL, true when a file of it was compiled, and true when it was stopped on
purpose. Returns the job, or NIL."
  (let ((job (make-package-fetch name where nil nil nil))
        (deciding *package-job-deciding*))
    (setf (package-fetch-load job) load
          (package-fetch-then job) then)
    (when (if (and deciding (eq (cdr (assoc name *packages-in-hand* :test #'string=)) deciding))
              (progn (setf (cdr (assoc name *packages-in-hand* :test #'string=)) job) t)
              (take-package-in-hand job))
      (tuatara.runtime:start-job-thread
       job
       (lambda (job)
         (multiple-value-bind (loaded failure)
             (handler-case
                 (reporting-errors ("loading the package ~a" name)
                                   (multiple-value-bind (apart structures) (and compile (funcall compile job))
                                     (tuatara.runtime:job-check-stop)
                                     (let ((stop (list nil))
                                           (in-the-way (structures-in-the-way name (if apart structures :unknown))))
                                       (if in-the-way
                                           (values nil (make-condition 'package-needs-restart
                                                                       :name name :structures in-the-way))
                                           (call-with-editor-held (lambda () (let ((*package-compiling*
                                                                                    (if apart :refused nil))
                                                                                   (*package-load-stop* stop))
                                                                               (funcall load)))
                                                                  :words (format nil "Loading the package ~a" name)
                                                                  :stop (lambda () (setf (car stop) t)))))))
               (error (e) (values nil e)))
           (setf (package-fetch-loaded job) loaded
                 (package-fetch-failure job) failure)))
       :name "tuatara package load"))))

(defun package-load-taken-in (job)
  "Finish the loading JOB did in the background (START-PACKAGE-LOAD): its
package let go, and its THEN called with what it loaded, the error that
ended it or why it stopped, whether a file of it was compiled, and whether
it was stopped on purpose."
  (let-package-go job)
  (let ((status (package-fetch-status job)))
    (funcall (package-fetch-then job)
             (and (eq status :done) (package-fetch-loaded job))
             (case status
               (:done (package-fetch-failure job))
               (:stopped "it was stopped")
               (t (or (tuatara.runtime:job-error job) "it ended unexpectedly")))
             (package-fetch-compiled job)
             (or (eq status :stopped) (typep (package-fetch-failure job) 'package-load-stopped)))))

(defun start-package-fetch (name where version file key &key quiet)
  "Fetch the package NAME (FETCH-PACKAGE) in the background, and see who
signed it (PACKAGE-SIGNATURE), then ask and load it (PACKAGE-FETCHED). An
address of http:// or git:// not allowed (PLAIN-SOURCE-REFUSAL) is not
fetched from, said; nor is a package in hand already (START-PACKAGE-JOB),
said too unless QUIET. One whose directory package-remove-unused is deleting
waits for it (WAIT-FOR-SWEEP). True when the fetch began now."
  (let ((refusal (plain-source-refusal name where)))
    (when refusal
      (note-package-failure name refusal)
      (message "~a" refusal)
      (return-from start-package-fetch nil)))
  (when (wait-for-sweep (package-kept-directory where name)
                        (lambda () (start-package-fetch name where version file key :quiet quiet)))
    (return-from start-package-fetch nil))
  (let ((plain (and (plain-transport-p where) t))
        (floor (let ((had (fetched-package key))) (and had (fourth had) (git-hash-p (second had) :short t)
                                                       (second had)))))
    (when (start-package-job
           (make-package-fetch name where version file key)
           (lambda (job)
             (let ((*package-plain-transport* plain))
               (multiple-value-bind (directory commit tag passed signature)
                   (fetch-package name where version
                                  :file file :floor floor
                                  :walk-p (lambda (looked commit tag)
                                            (not (eq :refuse (package-trust-verdict where (package-signature looked
                                                                                                             commit tag where)))))
                                  :signature-of (lambda (looked commit tag) (package-signature looked commit tag
                                                                                               where)))
                 (setf (package-fetch-directory job) directory (package-fetch-commit job) commit (package-fetch-tag job)
                       tag (package-fetch-passed job) passed (package-fetch-signature job) signature))))
           "tuatara package fetch" :quiet quiet)
      (clear-package-failure name)
      (log-message (format nil "Fetching the package ~a from ~a" name where))
      t)))

(defun start-package-check (name where version file key entry &key quiet)
  "Check in the background who signed the version of the package NAME from
WHERE kept as ENTRY remembers it (PACKAGE-SIGNATURE), then take it as one
just fetched (PACKAGE-FETCHED): asked about with who signed it, loaded, or
refused, as the trust in force says. Nothing is fetched; nothing is done
while the package is in hand already (START-PACKAGE-JOB, QUIET as it takes
it)."
  (let ((job (make-package-fetch name where version file key)))
    (setf (package-fetch-directory job) (package-directory-for where name (second entry))
          (package-fetch-commit job) (second entry)
          (package-fetch-tag job) (third entry)
          (package-fetch-checking job) t)
    (when (start-package-job
           job
           (lambda (job)
             (setf (package-fetch-signature job)
                   (package-signature (package-fetch-directory job) (package-fetch-commit job) (package-fetch-tag job)
                                      where)))
           "tuatara package check" :quiet quiet)
      (clear-package-failure name))))

(defun signature-signer (signature)
  "Who SIGNATURE (PACKAGE-SIGNATURE) says signed a version, as an entry of
*FETCHED-PACKAGES* keeps it: the key's fingerprint, \"unsigned\", or NIL when
it could not be told."
  (case (getf signature :status)
    (:good (getf signature :fingerprint))
    (:none "unsigned")))

(defun kept-version-check-needed-p (where entry)
  "True when the version of the source WHERE that ENTRY of *FETCHED-PACKAGES*
remembers is to be checked again before it loads at a start. That is so when who
signed it, as git checked it when it was taken, is not known. It is also so when
the trust in force now would not take it (PACKAGE-TRUST-VERDICT: the source
trusted anew, its signer changed since, the init file's :signers naming others).
It is never so while *PACKAGE-CHECK-SIGNATURES* is NIL."
  (let ((signer (seventh entry)))
    (and *package-check-signatures*
         (or (null signer)
             (not (eq :take (package-trust-verdict where (if (string= signer "unsigned")
                                                             (list :status :none)
                                                             (list :status :good :fingerprint signer)))))))))

(defmethod tuatara.runtime:job-take-in ((job package-fetch))
  "Ask about and load the package fetched, or say why it was not; its package
held in hand until that is decided. For a package compiled and loaded in the
background, what follows its loading (PACKAGE-LOAD-TAKEN-IN)."
  (if (package-fetch-load job)
      (defer-action (lambda () (package-load-taken-in job)))
      (progn
        (setf *fetching-packages* (remove (package-fetch-name job) *fetching-packages* :test #'string=))
        (case (package-fetch-status job)
          (:done (defer-action (lambda ()
                                 (let ((*package-job-deciding* job))
                                   (unwind-protect (package-fetched job)
                                     (let-package-go job))))))
          (:failed (let-package-go job)
                   (let ((why (or (tuatara.runtime:job-error job) "it ended unexpectedly")))
                     (note-package-failure (package-fetch-name job) why)
                     (message "The package ~a was not fetched: ~a" (package-fetch-name job) why)))
          (t (let-package-go job))))))

(defun package-passed-over (job)
  "Say that JOB, a fetch of a package, found no version that fits this Tuatara:
the package is not loaded, or the version loaded now is kept."
  (let* ((name (package-fetch-name job)) (passed (package-fetch-passed job))
         (words (format nil "The package ~a ~a needs Tuatara ~a or later, and this is ~a; ~:[no older version tried ~
                             fits~;~
                             it is not signed as the versions taken before were, so no older version is looked for~]"
                        name (first passed) (second passed) (tuatara:version) (third passed)))
         (loaded (package-loaded-p name)))
    (unless loaded (note-package-failure name words))
    (message "~a: ~:[it is not loaded~;the version loaded now is kept~]" words loaded)))

(defun package-fetched-refused (job had words remember)
  "Refuse the version JOB fetched of a package, for WORDS (REFUSE-PACKAGE-VERSION).
HAD is what was remembered of the package before."
  (let ((key (package-fetch-key job)) (commit (package-fetch-commit job)))
    ;; a version taken before and refused now is asked about again, never loaded at a start unchecked
    (when (and had (fourth had) (string= (second had) commit))
      (state-list-put *fetched-packages*
                      (list (list key commit (package-fetch-tag job) nil (fifth had) (sixth had) (seventh had)))))
    (refuse-package-version (package-fetch-name job) (package-fetch-where job) words remember)))

(defun load-fetched-package (job directory had accepted-other previous previous-tag signer trust)
  "Remember the version JOB fetched of a package as let load and load it from
DIRECTORY, the version taken before it, ACCEPTED-OTHER when that was another
commit let load, else PREVIOUS, kept to fall back on; TRUST is called once it
is loaded, and older versions are then deleted (PRUNE-PACKAGE-VERSIONS)."
  (let ((name (package-fetch-name job)) (where (package-fetch-where job))
        (commit (package-fetch-commit job)) (tag (package-fetch-tag job)) (passed (package-fetch-passed job)))
    (state-list-put *fetched-packages* (list (list (package-fetch-key job) commit tag t previous previous-tag signer)))
    (load-version name where directory (package-fetch-file job)
                  (cond (accepted-other had)
                        (previous (list (package-fetch-key job) previous previous-tag t)))
                  (lambda (outcome &optional loaded)
                    (declare (ignore loaded))
                    (case outcome
                      (:taken
                       (funcall trust)
                       (when (package-loaded-p name)
                         (message "The package ~a~@[ ~a~] is loaded~@[ (~a needs Tuatara ~a or later)~]"
                                  name (or tag (and passed (subseq commit 0 7))) (first passed) (second passed))))
                      (:restart
                       (funcall trust)
                       (say-package-needs-restart name (package-version-words commit tag))))
                    (prune-package-versions name where)))))

(defun package-fetched-accepted (job had verdict words remember)
  "Load the version JOB fetched of a package, or ask about it, as PACKAGE-FETCHED
says. HAD is what was remembered of the package before; VERDICT, WORDS and
REMEMBER are what PACKAGE-TRUST-VERDICT said of its signer."
  (let* ((name (package-fetch-name job)) (key (package-fetch-key job)) (where (package-fetch-where job))
         (commit (package-fetch-commit job)) (tag (package-fetch-tag job))
         (directory (package-directory-in (package-fetch-directory job) name))
         (accepted-same (and had (fourth had) (string= (second had) commit)))
         (accepted-other (and had (fourth had) (not accepted-same)))
         (previous (if accepted-other (second had) (fifth had)))
         (previous-tag (if accepted-other (third had) (sixth had)))
         (signer (signature-signer (package-fetch-signature job)))
         (trust (lambda () (when remember (state-list-put *package-trust* (list remember)))))
         (load (lambda ()
                 (load-fetched-package job directory had accepted-other previous previous-tag signer trust))))
    (cond ((and accepted-same (package-loaded-p name))
           (funcall trust)
           (unless (equal (seventh had) signer)
             (state-list-put *fetched-packages* (list (list key commit tag t (fifth had) (sixth had) signer))))
           (if (package-awaits-restart-p name)
               (say-package-needs-restart name (package-version-words commit tag))
               (message "The package ~a is up to date~@[ (~a)~]" name tag)))
          ((and accepted-same (not (eq verdict :ask)))
           (funcall load))
          (accepted-same
           ;; taken before its source's signer was remembered: asked again, who signed it shown
           (package-question name where directory load words))
          (t (state-list-put *fetched-packages* (list (list key commit tag nil previous previous-tag signer)))
             (package-question name where directory load words)))))

(defun package-fetched (job)
  "Take the version of a package JOB fetched, or refuse it. One that needs a
newer Tuatara than this was passed over for the newest that fits, said; when
none fits, the version loaded now is kept. One whose signer is not the one
trusted is refused (PACKAGE-TRUST-VERDICT). Else it is loaded: at once when
that commit was let load before and its source's signer is known, said to
be up to date when it is loaded already; else asked about first, who signed
it shown, and that signer remembered once it is let load. The version taken
before is kept to fall back on (LOAD-VERSION), older ones deleted
\(PRUNE-PACKAGE-VERSIONS)."
  (let ((name (package-fetch-name job)) (passed (package-fetch-passed job))
        (had (fetched-package (package-fetch-key job))))
    (cond
      ((null (package-fetch-directory job)) (package-passed-over job))
      (t
       (when passed
         (message "The package ~a ~a needs Tuatara ~a or later, and this is ~a: ~a, the newest version that fits, is ~
                   taken"
                  name (first passed) (second passed) (tuatara:version)
                  (or (package-fetch-tag job) (subseq (package-fetch-commit job) 0 7))))
       (multiple-value-bind (verdict words remember)
           (package-trust-verdict (package-fetch-where job) (package-fetch-signature job))
         (if (eq verdict :refuse)
             (package-fetched-refused job had words remember)
             (package-fetched-accepted job had verdict words remember)))))))
