;;;; test/packages.lisp --- Packages from a git repository or a directory: fetched, asked about, compiled, listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test packages
  :description "load-package with :source fetches the version asked for (the newest tag by date, latest, or one named) of a repository, the package's directory alone, asks before compiling it unless told not to, keeps it for the next start; a directory of your disk is a source too; the package manager lists, previews, updates and deletes them.")

(defun package-repository (dir)
  "Make DIR a git repository of two packages, pkgone/ (with a README) and
pkgtwo/: each version sets *PACKAGE-TEST-VERSION* to its name. Tag zeta is made first and
alpha later, then a commit no tag names, \"newest\"."
  (labels ((git (date &rest arguments)
             (uiop:run-program (list* "env"
                                      (format nil "GIT_COMMITTER_DATE=~a" date)
                                      (format nil "GIT_AUTHOR_DATE=~a" date)
                                      "git" "-C" (namestring dir) arguments)
                               :ignore-error-status t))
           (version (name date)
             (alexandria:write-string-into-file (format nil "(defparameter *package-test-version* ~s)~%" name)
                                                (ensure-directories-exist (merge-pathnames "pkgone/pkgone.lisp" dir))
                                                :if-exists :supersede)
             (alexandria:write-string-into-file (format nil "# pkgone~%~%Reads ~a.~%" name)
                                                (merge-pathnames "pkgone/README.md" dir) :if-exists :supersede)
             (alexandria:write-string-into-file (format nil "(defparameter *package-test-two* ~s)~%" name)
                                                (ensure-directories-exist (merge-pathnames "pkgtwo/load.lisp" dir))
                                                :if-exists :supersede)
             (git date "add" ".")
             (git date "commit" "-q" "-m" name)))
    (git "2026-01-01T00:00:00" "init" "-q" "-b" "main")
    (git "2026-01-01T00:00:00" "config" "user.name" "Test")
    (git "2026-01-01T00:00:00" "config" "user.email" "test@example.invalid")
    (git "2026-01-01T00:00:00" "config" "commit.gpgsign" "false")
    (git "2026-01-01T00:00:00" "config" "tag.gpgsign" "false")
    (git "2026-01-01T00:00:00" "config" "uploadpack.allowFilter" "true")
    (version "zeta" "2026-01-01T00:00:00")
    (git "2026-01-01T00:00:00" "tag" "-a" "zeta" "-m" "zeta")
    (version "alpha" "2026-02-01T00:00:00")
    (git "2026-02-01T00:00:00" "tag" "-a" "alpha" "-m" "alpha")
    (version "newest" "2026-03-01T00:00:00")))

(defun forget-test-packages (dir)
  "Forget the packages from DIR: what was remembered of them, what was kept of
them and compiled, the session's lists of them, those removed, and their
versions' setting."
  (let ((where (format nil "file://~a" (string-right-trim "/" (namestring dir)))))
    (tuatara.defaults::state-list-drop tuatara.packages::*fetched-packages*
                                       (remove-if-not (lambda (key) (search (namestring dir) key))
                                                      (mapcar #'first (tuatara.defaults::state-list-entries tuatara.packages::*fetched-packages*))))
    (let ((kept (format nil "~a~a/"
                        (tuatara.packages:packages-directory)
                        (tuatara.io:percent-encoded where :safe "._-"))))
      (dolist (directory (list kept (directory-namestring (tuatara.packages::package-fasl (merge-pathnames "x" kept)))))
        (when (probe-file directory)
          (uiop:delete-directory-tree (pathname directory)
                                      :validate (lambda (p) (uiop:subpathp p *test-cache-directory*))))))
    (let ((fasls (directory-namestring (tuatara.packages::package-fasl (merge-pathnames "x" dir)))))
      (when (probe-file fasls)
        (uiop:delete-directory-tree (pathname fasls)
                                    :validate (lambda (p) (uiop:subpathp p *test-cache-directory*))))))
  (tuatara.defaults::state-list-drop tuatara.packages::*removed-packages*
                                     (tuatara.defaults::state-list-entries tuatara.packages::*removed-packages*))
  (setf tuatara.packages::*declared-packages* '()
        tuatara.packages::*loaded-packages* '()
        tuatara.packages::*unloaded-packages* '()
        tuatara.packages::*package-code* '()
        tuatara.packages::*package-failures* '()
        tuatara.packages::*refused-packages* '())
  (makunbound 'tuatara-user::*package-test-version*)
  (makunbound 'tuatara-user::*package-test-two*)
  (makunbound 'tuatara-user::*package-test-config*))

(defparameter *package-until-bound* 120
  "The bound on a wait of the package tests: loading a package fetches,
checks and compiles it apart in the background, and the first one compiled
makes the core it is compiled from; each test waits for that to happen, this
only ends a wait that never would.")

(defmacro with-package-repository ((dir where) &body body)
  "Run BODY with DIR a new repository of packages (PACKAGE-REPOSITORY) and
WHERE its file:// address, all of it forgotten afterwards."
  `(with-test-directory (,dir)
     (package-repository ,dir)
     (let ((,where (format nil "file://~a" (string-right-trim "/" (namestring ,dir))))
           (tuatara.defaults::*package-sources* '())
           (*until-bound* *package-until-bound*))
       (unwind-protect (progn ,@body)
         (forget-test-packages ,dir)))))

(defun package-test-version ()
  "The version of pkgone loaded, or NIL."
  (and (boundp 'tuatara-user::*package-test-version*) (symbol-value 'tuatara-user::*package-test-version*)))

(defvar *package-question-details* nil
  "What the buffer below the last question whether to load a package showed
(PACKAGE-QUESTION-SHOWN).")

(defun package-question-shown ()
  "True while the question whether to compile and load a package waits for its
key, what is shown below it then kept in *PACKAGE-QUESTION-DETAILS*."
  (and tuatara.command::*key-question*
       (search "Compile and load the package" (or (tuatara.command:editor-message tuatara.command:*editor*) ""))
       (let ((buffer (tuatara.buffer:find-buffer "*Question*")))
         (setf *package-question-details* (and buffer (tuatara.buffer:buffer-string buffer)))
         t)))

(define-test (packages newest-tag-by-date)
  ;; no version asked for: the tag made last, though its name sorts first; only its directory kept
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) directory)
      (auto-save-session (list (list :until (lambda () (tuatara-user:package-source "test" where) t))
                               (list :until (lambda () (tuatara-user:load-package "pkgone" :source "test"
                                                                                  :config
                                                                                  (setf tuatara-user::*package-test-config* (list tuatara-user::*package-test-version*)))
                                              t))
                               (list :until (lambda () (package-test-version)))
                               (list :until (lambda () (setf directory (nth-value 1 (tuatara.packages::package-state "pkgone" "test" nil)))))))
      (is equal "alpha" (package-test-version))
      ;; its :config ran once it had loaded, in the background
      (is equal '("alpha") (symbol-value 'tuatara-user::*package-test-config*))
      (true (search "/pkgone/" directory))
      (true (probe-file (merge-pathnames "pkgone.lisp" directory)))
      (false (probe-file (merge-pathnames "../pkgtwo/" directory)))
      (is equal "alpha" (third (tuatara.packages::package-state "pkgone" "test" nil)))
      ;; the next start: loaded at once from what was kept, nothing fetched
      (makunbound 'tuatara-user::*package-test-version*)
      (true (tuatara-user:load-package "pkgone" :source "test"))
      (is equal "alpha" (package-test-version)))))

(define-test (packages versions-named)
  ;; a tag named, the newest commit, a package entered by load.lisp, and one not there; a host
  ;; giving files by themselves, as the usual hosts do
  (with-package-repository (dir where)
    (git-output dir "config" "uploadpack.allowAnySHA1InWant" "true")
    (let ((tuatara-user:*package-ask-before-loading* nil) (versions '()))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where :version "zeta") t))
                      (list :until (lambda () (package-test-version)))
                      (list :until (lambda () (push (package-test-version) versions)))
                      (list :until (lambda () (tuatara-user:load-package "pkgone" :source where :version "latest") t))
                      (list :until (lambda () (equal "newest" (package-test-version))))
                      (list :until (lambda () (push (package-test-version) versions)))
                      (list :until (lambda () (tuatara-user:load-package "pkgtwo" :source where) t))
                      (list :until (lambda () (boundp 'tuatara-user::*package-test-two*)))
                      (list :until (lambda () (tuatara-user:load-package "pkgone" :source where :version "omega") t))
                      (list :until (lambda () (search "omega" (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                :width 160)))
        (is equal '("newest" "zeta") versions)
        (is equal "alpha" (symbol-value 'tuatara-user::*package-test-two*))
        (true (tuatara.frontend.fake:fake-frame-showing f "has no tag, branch or commit called omega"))))))

(define-test (packages untagged-newest-commit)
  ;; a repository of no tag yet: no version asked for takes its newest commit; "release" says it has none
  (with-package-repository (dir where)
    (git-output dir "tag" "-d" "zeta" "alpha")
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
                      (list :until (lambda () (package-test-version)))
                      (list :until (lambda () (tuatara-user:load-package "pkgtwo" :source where :version "release") t))
                      (list :until (lambda () (search "tagged" (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                :width 200)))
        (is equal "newest" (package-test-version))
        (true (tuatara.frontend.fake:fake-frame-showing f "has no tagged version"))))))

(define-test (packages asked-first)
  ;; asked before compiling: n leaves it, and y at the next start loads it and remembers so
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* t))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
                      (list :until #'package-question-shown)
                      "n" (list :wait 0.2))
                :width 160)))
        (true (search "The package pkgone, from" *package-question-details*))
        (true (tuatara.frontend.fake:fake-frame-showing f "The package pkgone is not loaded"))
        (false (package-test-version))
        (false (fourth (tuatara.packages::package-state "pkgone" where nil)))
        (auto-save-session
         (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
               (list :until #'package-question-shown)
               "y" (list :until (lambda () (package-test-version)))))
        (is equal "alpha" (package-test-version))
        (true (fourth (tuatara.packages::package-state "pkgone" where nil)))))))

(define-test (packages up-to-date-and-managed)
  ;; two packages of one commit each kept apart; package-update finds the same tag; the
  ;; package manager lists them, the README of the one on the line beside it, and unloads,
  ;; removes and deletes the one on the line
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil)
          (commit (string-trim '(#\Newline) (git-output dir "rev-parse" "alpha^{commit}")))
          (screens '()))
      (flet ((screen (f)
               (push (format nil "~{~a~%~}"
                             (loop for row below 24 collect (tuatara.frontend.fake:fake-screen-line f row))) screens)))
        (let ((f (auto-save-session
                  (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
                        (list :until (lambda () (tuatara-user:load-package "pkgtwo" :source where) t))
                        (list :until (lambda () (and (package-test-version) (boundp 'tuatara-user::*package-test-two*))))
                        "M-x p a c k a g e - u p d a t e RET"
                        (list :until (lambda () (null tuatara.packages::*fetching-packages*)))
                        "M-x p a c k a g e - m a n a g e r RET" (list :wait 0.2)
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t))
                        "C-n" (list :wait 0.2)
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t))
                        "C-p D"
                        (list :until (lambda () (tuatara.frontend.fake:fake-frame-showing
                                                 (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                 "Unload, remove and delete")))
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t))
                        "y e s RET"
                        (list :until (lambda ()
                                       (search "deleted"
                                               (or (tuatara.command:editor-message tuatara.command:*editor*) ""))))
                        (list :wait 0.2)
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t)))
                  :width 200 :height 24)))
          (declare (ignore f))
          (destructuring-bind (after question second first) screens
            ;; the README beside the list goes with what was deleted, the entry on the line shown anew
            (false (search "Reads alpha." after))
            (false (search "deleted on disk" after))
            (true (search "pkgone is not fetched yet" after) "~a" after)
            ;; what is done asked plainly, with the size of what is deleted
            (true (search "Unload, remove and delete the package pkgone, " question) "~a" question)
            (true (search "pkgone" first))
            (true (search "alpha" first))
            (true (search (subseq commit 0 7) first))
            (true (search "loaded" first))
            (true (search "2 packages, 2 loaded" first))
            (true (search "Reads alpha." first))
            (true (search "has no README" second)))
          (is equal "alpha" (symbol-value 'tuatara-user::*package-test-two*))
          (false (probe-file (tuatara.packages::package-kept-directory where "pkgone")))
          (true (probe-file (tuatara.packages::package-kept-directory where "pkgtwo")))
          (false (tuatara.packages::package-state "pkgone" where nil))
          (true (tuatara.packages::package-removed-p "pkgone")))))))

(define-test (packages compile-failure-said)
  ;; a file that cannot be read says why, not only that it could not be compiled
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "(defun broken ()~%  (list 1 2~%")
                                       (merge-pathnames "broken.lisp" dir))
    (let* ((tuatara.packages::*package-directory* (pathname dir))
           (said (handler-case (progn (tuatara-user:load-package-file "broken.lisp") nil)
                   (error (e) (princ-to-string e)))))
      (true (search "could not be compiled: " said))
      (true (search "end of file" (string-downcase said))))))

(define-test (packages directory-source)
  ;; a directory of your disk: asked about once, then loaded at once; :file names its entry
  (with-test-directory (dir)
    (alexandria:write-string-into-file "(defparameter *package-test-version* \"mine\")"
                                       (merge-pathnames "start.lisp" dir))
    (let ((tuatara-user:*package-ask-before-loading* t)
          (tuatara.defaults::*package-sources* '()))
      (unwind-protect
           (progn
             ;; v lists its directory in rimu, and nothing is loaded
             (let ((f (auto-save-session
              (list (list :until (lambda () (tuatara-user:package-source "mine" (namestring dir)) t))
                    (list :until (lambda () (tuatara-user:load-package "mypkg" :source "mine" :file "start.lisp") t))
                    (list :until #'package-question-shown)
                    "v" (list :until (lambda () (search "is shown" (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                       :width 120)))
               (true (tuatara.frontend.fake:fake-frame-showing f "start.lisp")))
             (false (package-test-version))
             (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "mypkg" :source "mine" :file "start.lisp") t))
                    (list :until #'package-question-shown)
                    "y" (list :until (lambda () (package-test-version)))))
             (is equal "mine" (package-test-version))
             (makunbound 'tuatara-user::*package-test-version*)
             (true (tuatara-user:load-package "mypkg" :source "mine" :file "start.lisp"))
             (is equal "mine" (package-test-version)))
        (forget-test-packages dir)))))

(defmacro with-directory-package ((dir name text) &body body)
  "Run BODY with DIR a new directory holding the package NAME, its NAME.lisp
TEXT, as the source \"mine\"; all of it forgotten afterwards."
  `(with-test-directory (,dir)
     (alexandria:write-string-into-file ,text (merge-pathnames (format nil "~a.lisp" ,name) ,dir))
     (let ((tuatara.defaults::*package-sources* '())
           (*until-bound* *package-until-bound*))
       (unwind-protect (progn (tuatara-user:package-source "mine" (namestring ,dir)) ,@body)
         (forget-test-packages ,dir)))))

(define-test (packages question-answered-by-one-key)
  ;; y alone answers, no minibuffer opened; another key says which keys answer and asks
  ;; again; C-g is not now, nothing loaded
  (with-directory-package (dir "onekey" "(defparameter *package-test-version* \"one key\")")
    (let ((tuatara-user:*package-ask-before-loading* t) (minibuffer :unseen))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "onekey" :source "mine") t))
                      (list :until #'package-question-shown)
                      (list :until (lambda ()
                                     (setf minibuffer
                                           (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)) t))
                      "x" (list :until (lambda () (search "Answer y, n or v. Compile and load the package onekey"
                                                          (or (tuatara.command:editor-message tuatara.command:*editor*) ""))))
                      "y" (list :until (lambda () (package-test-version))))
                :width 160)))
        (false minibuffer)
        (true (tuatara.frontend.fake:fake-frame-showing f "Answer y, n or v. Compile and load the package onekey"))
        (is equal "one key" (package-test-version))))
    ;; forgotten, so that it is asked about again
    (forget-test-packages dir)
    (let ((tuatara-user:*package-ask-before-loading* t))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "onekey" :source "mine") t))
                      (list :until #'package-question-shown)
                      "C-g"
                      (list :until (lambda ()
                                     (search "is not loaded now"
                                             (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                :width 160)))
        (true (tuatara.frontend.fake:fake-frame-showing f "The package onekey is not loaded now"))
        (false (package-test-version))))))

(define-test (packages question-not-answered-by-keys-typed-ahead)
  ;; keys typed before the question shows never answer it, wake-ups among them or not,
  ;; nor keys left unread: the n pressed after it does
  (with-directory-package (dir "ahead" "(defparameter *package-test-version* \"ahead\")")
    (let ((tuatara-user:*package-ask-before-loading* t)
          (f (tuatara.frontend.fake:make-fake-frontend :width 160 :height 8))
          (waiting t))
      (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (and waiting (tuatara.frontend.fake::fake-events f))))
      (queue-test-keys f (list (lambda ()
                                 (tuatara-user:load-package "ahead" :source "mine")
                                 ;; a key a command left unread, as a wait for work keeps one
                                 (setf (tuatara.command:editor-unread tuatara.command:*editor*)
                                       (list (tuatara.frontend:make-event :key :key (tuatara.frontend:spec-key '(#\y))))))
                               :tick "y RET" :tick "y" (lambda () (setf waiting nil)) "n"))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)
      (true (tuatara.frontend.fake:fake-frame-showing f "The package ahead is not loaded"))
      (false (tuatara.frontend.fake:fake-frame-showing f "is not loaded now"))
      (false (package-test-version)))))

(define-test (packages compiled-while-the-editor-goes-on)
  ;; a package taking a second and a half to compile: the command loading it returns at
  ;; once, the editor answers all the while, and the package is loaded once, said
  (with-directory-package (dir "slowpkg" (format nil "(eval-when (:compile-toplevel) (sleep 1.5))~%~
                                                   (defvar *package-test-loads* 0)~%~
                                                   (incf *package-test-loads*)~%~
                                                   (defparameter *package-test-version* \"slow\")~%"))
    (let ((tuatara-user:*package-ask-before-loading* nil)
          (call 0) (last nil) (longest 0) (polled 0))
      (unwind-protect
           (let ((f (auto-save-session
                     (list (list :until (lambda ()
                                          (let ((start (get-internal-real-time)))
                                            (tuatara-user:load-package "slowpkg" :source "mine")
                                            (setf call (tuatara.runtime:milliseconds-since start)
                                                  last (get-internal-real-time)))
                                          t))
                           ;; looked at on the editor's thread every 20 ms until it has loaded
                           (list :until (lambda ()
                                          (setf longest (max longest (tuatara.runtime:milliseconds-since last))
                                                last (get-internal-real-time))
                                          (incf polled)
                                          (package-test-version))))
                     :width 160)))
             (is equal "slow" (package-test-version))
             (is = 1 (symbol-value 'tuatara-user::*package-test-loads*) "loaded once")
             (true (< call 200) "the command returned in ~d ms" call)
             (true (< longest 500) "the editor answered every ~d ms at most" longest)
             (true (> polled 20) "looked at ~d times while it compiled" polled)
             (true (tuatara.frontend.fake:fake-frame-showing f "Compiling the package slowpkg in the background"))
             (true (tuatara.frontend.fake:fake-frame-showing f "The package slowpkg is compiled and loaded")))
        (makunbound 'tuatara-user::*package-test-loads*)))))

(define-test (packages never-half-loaded-to-commands)
  ;; an entry loading a second file slow to compile: no command runs while the entry is
  ;; part loaded, and the package's compile-time code runs in another process
  (with-test-directory (dir)
    (alexandria:write-string-into-file
     (format nil "(defparameter *package-test-half-a* t)~%(load-package-file \"b.lisp\")~%(defparameter *package-test-version* \"half\")~%")
     (merge-pathnames "halfpkg.lisp" dir))
    (alexandria:write-string-into-file
     (format nil "(eval-when (:compile-toplevel)~%  (sleep 1.5)~%  (with-open-file (o ~s :direction :output :if-exists :supersede) (print (sb-posix:getpid) o)))~%(defvar *package-test-half-b* t)~%"
             (namestring (merge-pathnames "compiled-by" dir)))
     (merge-pathnames "b.lisp" dir))
    (let ((tuatara.defaults::*package-sources* '())
          (tuatara-user:*package-ask-before-loading* nil)
          (seen-half 0)
          (polled 0)
          (*until-bound* *package-until-bound*))
      (unwind-protect
           (progn (tuatara-user:package-source "mine" (namestring dir))
                  (auto-save-session
                   (list (list :until (lambda () (tuatara-user:load-package "halfpkg" :source "mine") t))
                         (list :until (lambda ()
                                        (incf polled)
                                        (when (and (boundp 'tuatara-user::*package-test-half-a*)
                                                   (not (package-test-version)))
                                          (incf seen-half))
                                        (package-test-version))))
                   :width 160)
                  (is equal "half" (package-test-version))
                  (is = 0 seen-half "commands ran ~d times with the package part loaded" seen-half)
                  (true (> polled 20) "the editor went on while it compiled: ~d" polled)
                  (let ((pid (with-open-file (i (merge-pathnames "compiled-by" dir)) (read i))))
                    (true (integerp pid))
                    (isnt = (sb-posix:getpid) pid "compiled in the editor's own process")))
        (makunbound 'tuatara-user::*package-test-half-a*)
        (makunbound 'tuatara-user::*package-test-half-b*)
        (forget-test-packages dir)))))

(define-test (packages stopped-while-compiling)
  ;; stopping the loading while the package compiles ends the compiling at once, and
  ;; nothing is loaded
  (with-directory-package (dir "stopped" (format nil "(eval-when (:compile-toplevel) (sleep 8))~%(defparameter *package-test-version* \"stopped\")~%"))
    (let ((tuatara-user:*package-ask-before-loading* nil) (began nil) (took nil))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "stopped" :source "mine") t))
                      (list :until (lambda ()
                                     (search "Compiling the package stopped"
                                             (or (tuatara.command:editor-message tuatara.command:*editor*) ""))))
                      (list :until (lambda ()
                                     (setf began (get-internal-real-time))
                                     (dolist (job tuatara.runtime:*jobs*)
                                       (when (tuatara.packages::package-fetch-p job) (tuatara.runtime:job-stop job)))
                                     t))
                      (list :until (lambda ()
                                     (when (search "it was stopped"
                                                   (or (tuatara.command:editor-message tuatara.command:*editor*) ""))
                                       (setf took (tuatara.runtime:milliseconds-since began))))))
                :width 160)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Loading package stopped...failed: it was stopped"))
        (true (and took (< took 3000)) "ended ~a ms after it was stopped" took)
        (false (package-test-version))))))

(define-test (packages long-load-said)
  ;; a load that holds the editor longer than it should is said, and then loads
  (with-directory-package (dir "longload" (format nil "(sleep 2)~%(defparameter *package-test-version* \"long\")~%"))
    (let ((tuatara-user:*package-ask-before-loading* nil) (tuatara.command::*editor-held-seconds* 1))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "longload" :source "mine") t))
                      (list :until (lambda () (package-test-version))))
                :width 160)))
        (is equal "long" (package-test-version))
        (true (tuatara.frontend.fake:fake-frame-showing f "Loading the package longload is taking long"))))))

(define-test (packages question-out-of-recorded-keys)
  ;; keys being recorded are no part of the question: C-g there leaves the recording going,
  ;; and neither the question nor its answer is recorded
  (with-directory-package (dir "recorded" "(defparameter *package-test-version* \"recorded\")")
    (let ((tuatara-user:*package-ask-before-loading* t))
      (recorded-keys-session (list "C-x (" (lambda () (tuatara-user:load-package "recorded" :source "mine"))
                                   "C-g" "C-f" "C-x )")
                             :text "abc")
      (false (package-test-version))
      (true tuatara.defaults::*last-recorded-keys* "the recording went on after C-g")
      (is equal
          '("C-f")
          (map 'list
               (lambda (event)
                 (tuatara.frontend:key-description (tuatara.frontend:key-spec (tuatara.frontend:event-key event))))
               tuatara.defaults::*last-recorded-keys*)))))

(defun compile-child-with (request token)
  "What COMPILE-PACKAGE-CHILD answers with REQUEST and TOKEN in the
environment as the editor gives them (NIL leaves one out), the directory of
compiled packages named as the editor names it; the environment as before
after."
  (let ((names '("TUATARA_COMPILE_PACKAGE" "TUATARA_COMPILE_TOKEN" "TUATARA_COMPILE_CACHE")))
    (unwind-protect
         (progn (loop for name in names
                      for value in (list request token (tuatara.packages::package-cache))
                      do (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name)))
                (let ((*standard-output* (make-broadcast-stream))) (tuatara.packages:compile-package-child)))
      (dolist (name names) (sb-posix:unsetenv name)))))

(define-test (packages compiling-apart-never-from-file-names)
  ;; files named --compile-package and a in a directory, as `tuatara *' there passes them: they
  ;; are files to open, and nothing they name is ever compiled or loaded; only a request the
  ;; editor wrote in its directory of compiled packages, its own and private, with the token
  ;; it gave in the environment, is taken
  (with-test-directory (dir)
    (let* ((marker (merge-pathnames "ran" dir))
           (evil (ensure-directories-exist (merge-pathnames "evil/" dir)))
           (plist (lambda (token)
                    (with-standard-io-syntax
                      (prin1-to-string (list :token token :name "evil" :directory (namestring evil) :file nil
                                             :prelude '()
                                             :cache (tuatara.io:native (tuatara.io:cache-directory))
                                             :packages (tuatara.packages:packages-directory)
                                             :identity (tuatara.packages::tuatara-identity))))))
           (in-cache (format nil "~acompile-test-~a.request" (tuatara.packages::package-cache) (tuatara.io:random-name))))
      (alexandria:write-string-into-file (format nil "(with-open-file (o ~s :direction :output :if-exists :supersede) (print :ran o))~%"
                                                 (namestring marker))
                                         (merge-pathnames "evil.lisp" evil))
      (alexandria:write-string-into-file "" (merge-pathnames "--compile-package" dir))
      (alexandria:write-string-into-file (funcall plist "guessed") (merge-pathnames "a" dir))
      (unwind-protect
           (progn
             ;; the command line names files, never the mode
             (is equal '("--compile-package" "a")
                 (getf (tuatara.terminal::read-options '("--" "--compile-package" "a") :directory (namestring dir))
                       :files))
             ;; no request in the environment, as when started by hand
             (is = 2 (compile-child-with nil nil))
             ;; a request outside the directory of compiled packages, its token right or not
             (is = 2 (compile-child-with (namestring (merge-pathnames "a" dir)) "guessed"))
             ;; in that directory, but readable by others
             (tuatara.io:ensure-private-directory (tuatara.packages::package-cache))
             (tuatara.io:write-private-file in-cache (funcall plist "right"))
             (sb-posix:chmod in-cache #o644)
             (is = 2 (compile-child-with in-cache "right"))
             ;; private, but another token
             (sb-posix:chmod in-cache #o600)
             (is = 2 (compile-child-with in-cache "wrong"))
             ;; a link to a private request
             (let ((link (format nil
                                 "~acompile-link-~a.request"
                                 (tuatara.packages::package-cache)
                                 (tuatara.io:random-name))))
               (sb-posix:symlink in-cache link)
               (unwind-protect (is = 2 (compile-child-with link "right"))
                 (ignore-errors (sb-posix:unlink link))))
             (false (probe-file marker) "nothing of the directory was loaded"))
        (ignore-errors (delete-file in-cache))))))

(define-test (packages stopped-between-files-while-held)
  ;; C-g once a held load takes long stops it before its next file, the editor waiting
  ;; until then; what of it loaded is taken out, and it is said to be half loaded
  (with-test-directory (dir)
    ;; a Lisp package of its own, as a package has
    (alexandria:write-string-into-file (format nil "(defpackage #:stopper (:use #:cl #:tuatara))~%(load-package-file \"one.lisp\")~%(load-package-file \"two.lisp\")~%")
                                       (merge-pathnames "stopper.lisp" dir))
    (alexandria:write-string-into-file (format nil "(in-package #:stopper)~%(define-command stopper-hello () \"Say hello.\" nil)~%(sleep 2)~%")
                                       (merge-pathnames "one.lisp" dir))
    (alexandria:write-string-into-file (format nil "(defparameter *package-test-version* \"stopper\")~%")
                                       (merge-pathnames "two.lisp" dir))
    (let ((tuatara.defaults::*package-sources* '()) (tuatara-user:*package-ask-before-loading* nil)
          (tuatara.command::*editor-held-seconds* 1) (*until-bound* *package-until-bound*))
      (unwind-protect
           (let ((f (auto-save-session
                     (list (list :until (lambda () (tuatara-user:package-source "mine" (namestring dir))
                                          (tuatara-user:load-package "stopper" :source "mine") t))
                           (list :until (lambda () (search "is taking long" (or (tuatara.command:editor-message tuatara.command:*editor*) ""))) 60)
                           "C-g"
                           (list :until (lambda () (search "half loaded" (or (tuatara.command:editor-message tuatara.command:*editor*) ""))) 60))
                     :width 200 :pending t)))
             (true (tuatara.frontend.fake:fake-frame-showing f "Loading the package stopper: stopping where it safely can"))
             (true (tuatara.frontend.fake:fake-frame-showing f "The package stopper is half loaded; restart Tuatara before using it"))
             (false (package-test-version) "its second file never loaded")
             (false (tuatara.command:find-command "stopper-hello") "its command taken out")
             (false (tuatara.packages::package-loaded-p "stopper")))
        (forget-test-packages dir)))))

(define-test (packages compiled-files-written-whole)
  ;; a compiled file is renamed into place whole: another inode once compiled again, and
  ;; nothing left beside it
  (with-test-directory (dir)
    (let ((file (merge-pathnames "whole.lisp" dir)))
      (alexandria:write-string-into-file "(defparameter *package-test-whole* 1)" file)
      (let* ((fasl (tuatara.packages::package-fasl file)))
        (tuatara.io:ensure-private-directory (directory-namestring fasl))
        (tuatara.packages::compile-package-file file fasl)
        (let ((first (sb-posix:stat-ino (sb-posix:stat fasl))))
          (tuatara.packages::compile-package-file file fasl)
          (isnt = first (sb-posix:stat-ino (sb-posix:stat fasl)) "written in place"))
        (is equal '() (remove-if-not (lambda (p) (search ".new" (namestring p)))
                                     (uiop:directory-files (directory-namestring fasl))))
        (uiop:delete-directory-tree (pathname (directory-namestring fasl))
                                    :validate (lambda (p) (uiop:subpathp p *test-cache-directory*)))))))

(define-test (packages private-directories-kept-private)
  ;; a directory of yours there already, open to others, is made yours alone; one not yours
  ;; is refused, never used
  (with-test-directory (dir)
    (let ((open (namestring (merge-pathnames "open/" dir))))
      (ensure-directories-exist open)
      (sb-posix:chmod open #o777)
      (tuatara.io:ensure-private-directory open)
      (is = #o700 (logand (sb-posix:stat-mode (sb-posix:stat open)) #o777))))
  (fail (tuatara.io:ensure-private-directory "/usr/") 'tuatara.io:file-system-error))

(define-test (packages compiled-apart-from-one-core)
  ;; run from the sources, a package is compiled apart by SBCL started from one core
  ;; saved for these sources, made once and used by every compiling after
  (unless tuatara.io:*saved-program*
    (with-directory-package (dir "corea" "(defparameter *package-test-version* \"a\")")
      (alexandria:write-string-into-file "(defparameter *package-test-two* \"b\")" (merge-pathnames "coreb.lisp" dir))
      (let ((tuatara-user:*package-ask-before-loading* nil) (cores '()))
        (flet ((note () (let ((core (tuatara.packages::compiler-core)))
                          (push (and core (sb-posix:stat-ino (sb-posix:stat core))) cores))))
          (auto-save-session
           (list (list :until (lambda () (tuatara-user:load-package "corea" :source "mine") t))
                 (list :until (lambda () (package-test-version)))
                 (list :until (lambda () (note) t))
                 (list :until (lambda () (tuatara-user:load-package "coreb" :source "mine") t))
                 (list :until (lambda () (boundp 'tuatara-user::*package-test-two*)))
                 (list :until (lambda () (note) t)))))
        (is equal "a" (package-test-version))
        (true (first cores) "a core was made")
        (is equal (first cores) (second cores) "and kept")
        (true (member "--core" (tuatara.packages::package-compiler-command) :test #'equal))))))

(define-test (packages scanned-from-the-init-file)
  ;; package-scan: each load-package with :source loaded by itself, one left inside another
  ;; by an open parenthesis too; a group of settings left alone; where reading stopped said
  (with-test-directory (dir)
    (dolist (name '("pkga" "pkgb"))
      (alexandria:write-string-into-file (format nil "(defparameter *package-test-~a* t)" name)
                                         (ensure-directories-exist (merge-pathnames (format nil "~a/~:*~a.lisp" name) dir))))
    (let ((init (merge-pathnames "init.lisp" dir))
          (tuatara-user:*package-ask-before-loading* nil)
          (tuatara.defaults::*package-sources* '()))
      (alexandria:write-string-into-file
       (format nil ";; settings~%(load-package \"tuatara\" :config (setf *package-test-config* t))~%~%(load-package \"pkga\" :source ~s~%~%(load-package \"pkgb\" :source ~s)~%~%)~%)~%(load-package \"pkgc\" :source ~s)~%"
               (namestring dir) (namestring dir) (namestring dir))
       init)
      (unwind-protect
           (multiple-value-bind (results stopped) (tuatara.packages::scan-init-file-packages init)
             ;; pkga swallowed pkgb: pkga refused saying so, pkgb found inside it and loaded by itself
             (is equal '("pkga" "pkgb") (mapcar #'first results))
             (true (search "pkgb\" is inside this one" (fourth (first results))))
             (is equal '("pkgb" 4 t "loaded") (second results))
             (true (boundp 'tuatara-user::*package-test-pkgb*))
             ;; the group of settings was not run, nor anything else of the file
             (false (boundp 'tuatara-user::*package-test-config*))
             ;; the stray ) stops the reading: pkgc, after it, is not seen
             (is = 9 (car stopped))
             (false (tuatara.packages::declared-package "pkgc")))
        (forget-test-packages dir))
      ;; what decides whether a load-package runs is kept to: a guarded one, or one a group loads, is not looked at
      (alexandria:write-string-into-file
       (format nil "(when nil (load-package \"pkga\" :source ~s))~%(load-package \"tuatara\" :config (load-package \"pkgb\" :source ~s))~%"
               (namestring dir) (namestring dir))
       init :if-exists :supersede)
      (unwind-protect
           (is equal '() (tuatara.packages::scan-init-file-packages init))
        (forget-test-packages dir)
        (makunbound 'tuatara-user::*package-test-pkga*)
        (makunbound 'tuatara-user::*package-test-pkgb*)))))

(define-test (packages directory-not-there)
  ;; a directory of your disk that is not there: said at once, nothing asked
  (with-test-directory (dir)
    (let ((tuatara-user:*package-ask-before-loading* t)
          (tuatara.defaults::*package-sources* '())
          (gone (namestring (merge-pathnames "gone/" dir))))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "ghost" :source gone) t))
                      (list :until (lambda ()
                                     (search "is not there"
                                             (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                :width 120)))
        (true (tuatara.frontend.fake:fake-frame-showing f "The package ghost is not loaded: its directory"))
        (false (tuatara.frontend.fake:fake-frame-showing f "Compile and load"))))))

(define-test (packages fetch-size-bounded)
  ;; a repository growing past the most a package may take is ended and deleted, not loaded
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil)
          (most tuatara.defaults::*package-size-limit*))
      (setf tuatara.defaults::*package-size-limit* 1000)
      (unwind-protect
           (let ((f (auto-save-session
                     (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
                           (list :until (lambda ()
                                          (search "takes more than"
                                                  (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                     :width 200)))
             (true (tuatara.frontend.fake:fake-frame-showing f "takes more than 1.0 kB: the package was not fetched"))
             (false (package-test-version))
             (false (directory (merge-pathnames ".fetching-*/" (tuatara.packages:packages-directory)))))
        (setf tuatara.defaults::*package-size-limit* most)))))

(define-test (packages fetch-size-looked-at-lightly)
  ;; while a fetch goes on, what git receives is looked at every second and the whole of what
  ;; it has fetched only every five seconds, not walked through each second
  (with-test-directory (dir)
    (let* ((scratch (namestring dir))
           (pack (format nil "~aprobe/.git/objects/pack/" scratch))
           (walks 0))
      (dotimes (i 50)
        (alexandria:write-string-into-file "x" (ensure-directories-exist (format nil "~afiles/d~d/f" scratch i))))
      (ensure-directories-exist pack)
      (sb-int:encapsulate 'tuatara.io:tree-size 'counted
                          (lambda (function directory &rest more)
                            (when (equal directory scratch) (incf walks))
                            (apply function directory more)))
      (unwind-protect
           (progn
             (tuatara.packages::call-within-package-size scratch "here" (lambda () (sleep 3.5)))
             (is = 1 walks "the whole walked through at the end alone")
             (let ((tuatara.defaults::*package-size-limit* 1000))
               (alexandria:write-string-into-file (make-string 2000 :initial-element #\p)
                                                  (format nil "~apack-1.pack" pack))
               (fail (tuatara.packages::call-within-package-size scratch "here" (lambda () (sleep 1.5)))
                   'tuatara.packages::package-too-large)))
        (sb-int:unencapsulate 'tuatara.io:tree-size 'counted)))))

(define-test (packages arguments-checked)
  ;; what could be taken for one of git's options, or a file outside the package's directory
  (fail (tuatara-user:load-package "pkgone" :source "--upload-pack=touch"))
  (fail (tuatara-user:package-source "bad" "--upload-pack=touch"))
  (fail (tuatara-user:load-package "pkgone" :source "/tmp/" :file "../x.lisp"))
  (fail (tuatara-user:load-package "pkgone" :source "/tmp/" :version "a b"))
  (fail (tuatara-user:load-package "../pkgone" :source "/tmp/"))
  ;; a parenthesis left open: the load-packages after it are no values of :file, said, never dropped
  (flet ((why (options) (handler-case (progn (tuatara.packages::load-package-options options) nil)
                          (error (e) (princ-to-string e)))))
    (true (search "load-package \"kitty\" is inside this one, after :file: a parenthesis is left open"
                  (why '(:source "/tmp/" :file "a.lisp" (tuatara-user:load-package "kitty" :source "/tmp/")))))
    (true (search ":version takes one value" (why '(:version "latest" "release"))))
    ;; :config and the others take many
    (false (why '(:source "/tmp/" :config (setf a 1) (setf b 2))))))

(define-test (packages a-mode-of-tuatara)
  ;; a mode of Tuatara's own by name, tuatara, or a group of your own: nothing loaded, its
  ;; :config run at once
  (unwind-protect
       (progn
         (false (tuatara-user:load-package "org" :config (setf tuatara-user::*package-test-config* :org)))
         (is eq :org (symbol-value 'tuatara-user::*package-test-config*))
         ;; and Tuatara itself, for settings of no one mode
         (false (tuatara-user:load-package "tuatara" :config (setf tuatara-user::*package-test-config* :tuatara)))
         (is eq :tuatara (symbol-value 'tuatara-user::*package-test-config*))
         ;; a name of your own is a group of settings, said so; a near miss of a package's or
         ;; a mode's name is taken for a mistyping
         (false (tuatara-user:load-package "writing" :config (setf tuatara-user::*package-test-config* :writing)))
         (is eq :writing (symbol-value 'tuatara-user::*package-test-config*))
         (true (search "Settings writing (your own group)"
                       (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
         (is string= "No package or mode called calendr: did you mean calendar?"
             (handler-case (progn (tuatara-user:load-package "calendr") nil) (error (e) (princ-to-string e))))
         (fail (tuatara-user:load-package "ogr"))
         ;; :disabled leaves a block out, and :if when its form gives nil
         (setf (symbol-value 'tuatara-user::*package-test-config*) :before)
         (false (tuatara-user:load-package "writing"
                                           :disabled t
                                           :config (setf tuatara-user::*package-test-config* :disabled)))
         (false (tuatara-user:load-package "writing"
                                           :if (= 1 2)
                                           :config (setf tuatara-user::*package-test-config* :if-false)))
         (is eq :before (symbol-value 'tuatara-user::*package-test-config*))
         (true (search "writing is left out (:disabled)"
                       (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
         (tuatara-user:load-package "writing" :if (= 1 1) :config (setf tuatara-user::*package-test-config* :if-true))
         (is eq :if-true (symbol-value 'tuatara-user::*package-test-config*)))
    (makunbound 'tuatara-user::*package-test-config*)))

(define-test (packages directory-of-another-name)
  ;; :directory names the package's directory in the repository when it has another name
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) directory)
      (auto-save-session (list (list :until (lambda ()
                                              (tuatara-user:load-package "second" :source where :directory "pkgtwo") t))
                               (list :until (lambda () (boundp 'tuatara-user::*package-test-two*)))
                               (list :until (lambda () (setf directory (nth-value 1 (tuatara.packages::package-state "second" where nil)))))))
      (is equal "alpha" (symbol-value 'tuatara-user::*package-test-two*))
      (true (search "/pkgtwo/" directory))
      (fail (tuatara-user:load-package "second" :source where :directory "../pkgtwo")))))

(defun directory-packages (dir &rest packages)
  "Make in DIR a directory of each of PACKAGES, (NAME TEXT), NAME/NAME.lisp
holding TEXT, for a source that is a directory of the disk."
  (loop for (name text) in packages
        do (alexandria:write-string-into-file text (ensure-directories-exist (merge-pathnames (format nil "~a/~a.lisp" name name) dir))
                                              :if-exists :supersede)))

(defmacro with-directory-packages ((dir) &body body)
  "Run BODY with DIR a new directory for DIRECTORY-PACKAGES, nothing asked before
loading, and what the packages left forgotten afterwards."
  `(with-test-directory (,dir)
     (let ((tuatara-user:*package-ask-before-loading* nil)
           (tuatara.defaults::*package-sources* '())
           (*until-bound* *package-until-bound*))
       (unwind-protect (progn ,@body)
         (forget-test-packages ,dir)
         (setf tuatara.packages::*deferred-packages* '() tuatara.packages::*packages-waiting* '())
         (dolist (symbol '(tuatara-user::*package-test-order* tuatara-user::*package-test-said*))
           (makunbound symbol))))))

(define-test (packages init-after-and-failures)
  ;; :init before the package loads; :after waits for the other one; a package that fails is
  ;; said and the next still loads; a :config form that fails leaves the others to run
  (with-directory-packages (dir)
    (directory-packages dir
                     '("early" "(defparameter *package-test-said* (list :saw *package-test-config*))")
                     '("first" "(push :first *package-test-order*)")
                     '("second" "(push :second *package-test-order*)")
                     '("broken" "(error \"made to fail\")"))
    (setf (symbol-value 'tuatara-user::*package-test-order*) '())
    (let ((where (namestring dir)))
      (tuatara-user:load-package "early" :source where :init (setf tuatara-user::*package-test-config* :early))
      (is equal '(:saw :early) (symbol-value 'tuatara-user::*package-test-said*))
      (tuatara-user:load-package "second" :source where :after "first")
      (is equal '() (symbol-value 'tuatara-user::*package-test-order*))
      (true (find "second" tuatara.packages::*packages-waiting* :key #'second :test #'string=))
      (tuatara-user:load-package "first" :source where)
      (is equal '(:second :first) (symbol-value 'tuatara-user::*package-test-order*))
      ;; a group waits for a package, and for another group, as a package does
      (setf (symbol-value 'tuatara-user::*package-test-order*) '())
      (tuatara-user:load-package "late-group" :after "early-group"
                                 :config (push :late-group tuatara-user::*package-test-order*))
      (tuatara-user:load-package "writing" :after "third"
                                 :config (push :writing tuatara-user::*package-test-order*))
      (is equal '() (symbol-value 'tuatara-user::*package-test-order*))
      (tuatara-user:load-package "early-group" :config (push :early-group tuatara-user::*package-test-order*))
      (directory-packages dir '("third" "(push :third *package-test-order*)"))
      (tuatara-user:load-package "third" :source where)
      (is equal '(:writing :third :late-group :early-group) (symbol-value 'tuatara-user::*package-test-order*))
      (false (tuatara-user:load-package "broken" :source where))
      (false (tuatara.packages::package-loaded-p "broken"))
      (true (assoc "broken" tuatara.packages::*package-failures* :test #'string=))
      (tuatara-user:load-package "tuatara"
                                 :config (error "one bad line")
                                 (setf tuatara-user::*package-test-config* :went-on))
      (is eq :went-on (symbol-value 'tuatara-user::*package-test-config*))
      (fail (macroexpand '(tuatara-user:load-package "x" :no-such-keyword 1))))))

(define-test (packages loaded-later)
  ;; :defer t loads once the editor has started; :commands when one of them is first run;
  ;; :modes when a buffer is first in one of them
  (with-directory-packages (dir)
    (directory-packages dir
                     '("lazy" "(push :lazy *package-test-order*)")
                     '("bycommand" "(tuatara.command:define-command package-test-hello () \"Say hello.\" (push :hello *package-test-order*))")
                     '("bymode" "(push :bymode *package-test-order*)"))
    (setf (symbol-value 'tuatara-user::*package-test-order*) '())
    (let ((where (namestring dir)) (at-once nil))
      (auto-save-session
       (list (list :until (lambda ()
                            (tuatara-user:load-package "lazy" :source where :defer t)
                            (tuatara-user:load-package "bycommand" :source where :commands package-test-hello)
                            (tuatara-user:load-package "bymode" :source where :modes csv)
                            (setf at-once (copy-list (symbol-value 'tuatara-user::*package-test-order*)))
                            t))
             (list :until (lambda () (member :lazy (symbol-value 'tuatara-user::*package-test-order*))))
             "M-x p a c k a g e - t e s t - h e l l o RET"
             (list :until (lambda () (member :hello (symbol-value 'tuatara-user::*package-test-order*))))
             "M-x c s v - m o d e RET"
             (list :until (lambda () (member :bymode (symbol-value 'tuatara-user::*package-test-order*))))))
      (is equal '() at-once)
      (is equal '(:bymode :hello :lazy) (symbol-value 'tuatara-user::*package-test-order*))
      (true (tuatara.packages::package-loaded-p "bycommand"))
      ;; the command's name once in M-x, though the package's and the stub's are two
      (is = 1 (count "package-test-hello" (tuatara.command:command-names) :test #'string=)))))

(define-test (packages said-at-start)
  ;; the init file's packages said in *Messages* with their times, and how many at the end;
  ;; each listed in the package manager with its directory, and compiled into the cache
  (with-directory-packages (dir)
    (directory-packages dir '("quick" "(defparameter *package-test-said* :quick)")
                     '("second" "(defparameter *package-test-said-second* :second)"))
    (let ((init (merge-pathnames "init.lisp" dir)))
      (alexandria:write-string-into-file
       (format nil
               "(setf *package-ask-before-loading* nil)~%(load-package ~s :source ~s)~%(load-package ~s :source ~s)~%"
               "quick" (namestring dir) "second" (namestring dir))
       init)
      (let ((tuatara.defaults:*config-file* init))
        (multiple-value-bind (f editor) (run-keys "M-x p a c k a g e - m a n a g e r RET"
                                                  :text ""
                                                  :width 160
                                                  :height 20)
          (declare (ignore editor))
          (let ((messages (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
            (true (cl-ppcre:scan "Loading package quick from .*\\.\\.\\.done \\(\\d+ ms\\)" messages))
            (true (cl-ppcre:scan "Loading package second from .*\\.\\.\\.done \\(\\d+ ms\\)" messages))
            (true (cl-ppcre:scan "Packages: 2 loaded in \\d+ ms" messages)))
          (true (probe-file (tuatara.packages::package-fasl (merge-pathnames "second/second.lisp" dir))))
          (true (tuatara.frontend.fake:fake-frame-showing f "second")))))))

(define-test (packages directory-in-rimu)
  ;; RET lists a package's directory in rimu, opened safely, the directory its top: h there
  ;; comes back to the package manager, and its files are previewed
  (with-directory-packages (dir)
    (directory-packages dir '("quick" "(defparameter *package-test-said* :quick)"))
    (let ((init (merge-pathnames "init.lisp" dir))
          (seen '()))
      (alexandria:write-string-into-file
       (format nil "(setf *package-ask-before-loading* nil)~%(load-package ~s :source ~s)~%" "quick" (namestring dir))
       init)
      (let ((tuatara.defaults:*config-file* init)
            (tuatara-user:*rimu-preview* t)
            (tuatara-user:*rimu-preview-delay* 0))
        (flet ((note (what) (lambda () (push (cons what (tuatara.display:window-buffer (tuatara.command:editor-window tuatara.command:*editor*))) seen))))
          (multiple-value-bind (f editor)
              (run-keys (list "M-x p a c k a g e - m a n a g e r RET C-s q u i c k RET RET"
                              (note :directory)
                              :tick :tick "h"
                              (note :back))
                        :text "" :width 160 :height 20)
            (declare (ignore f editor))
            (let ((directory (cdr (assoc :directory seen))) (back (cdr (assoc :back seen))))
              (true (tuatara.defaults::rimu-list-p directory))
              (true (tuatara.io:safe-visit-p directory))
              (true (tuatara.defaults::rimu-previewing-p directory))
              (is equal (tuatara.buffer:buffer-local directory 'tuatara.defaults::rimu-directory)
                  (tuatara.buffer:buffer-local directory 'tuatara.defaults::rimu-root))
              (true (typep (tuatara.defaults::buffer-major-mode back) 'tuatara.packages::packages-mode)))))))))

(define-test (packages compiled-for-one-tuatara)
  ;; compiled packages are kept for the Tuatara that compiled them: another one's are deleted
  (let* ((other (format nil "~aother-tuatara-~36r/"
                       (directory-namestring (string-right-trim "/" (tuatara.packages::package-cache)))
                       (random (expt 36 6))))
         (tuatara.packages::*tuatara-identity* (format nil "test-~36r" (random (expt 36 8) (make-random-state t)))))
    (alexandria:write-string-into-file "x" (ensure-directories-exist (merge-pathnames "kept.fasl" other)))
    (let ((here (tuatara.packages::package-cache)))
      (true (search tuatara.packages::*tuatara-identity* here))
      (false (probe-file other)))))

(define-test (packages readme-below-a-narrow-list)
  ;; the README goes beside the package manager only when the list keeps its columns
  (flet ((list-width (width)
           (let ((seen nil) (readme (tuatara.buffer:make-buffer "readme-placing" :text "Reads.")))
             (unwind-protect
                  (run-keys (list (lambda ()
                                    (let* ((window (tuatara.command:editor-window tuatara.command:*editor*))
                                           (preview (tuatara.defaults::make-side-preview
                                                     window (tuatara.defaults::table-columns-width
                                                             tuatara.packages::*packages-columns*
                                                             tuatara.packages::*packages-source-width*))))
                                      (tuatara.defaults::side-preview-show preview readme nil)
                                      (setf seen (tuatara.display:window-width window)))))
                            :width width :height 30)
               (tuatara.buffer:kill-buffer readme))
             seen)))
    (is = 100 (list-width 100))
    (true (< (list-width 200) 200))))

(defun aged-package (dir name text)
  "Make the directory package NAME in DIR holding TEXT (DIRECTORY-PACKAGES), its
file dated a minute ago, so that what is kept of it later is newer."
  (directory-packages dir (list name text))
  (let ((file (namestring (merge-pathnames (format nil "~a/~a.lisp" name name) dir)))
        (then (- (sb-posix:time) 60)))
    (sb-posix:utimes file then then)))

(defun package-failure (name)
  "Why the package NAME was not loaded this session, or NIL."
  (cdr (assoc name tuatara.packages::*package-failures* :test #'string=)))

(defun forget-failure (name)
  "Forget that the package NAME failed or was refused this session, as package-scan does
before loading it again."
  (setf tuatara.packages::*package-failures*
        (remove name tuatara.packages::*package-failures* :key #'car :test #'string=)
        tuatara.packages::*refused-packages* (remove name tuatara.packages::*refused-packages* :test #'string=)))

(define-test (packages names-tuatara-lacks-refused)
  ;; a package calling functions Tuatara lacks is refused before its file runs, naming them,
  ;; and nothing of it is left: not its Lisp package, not its command
  (with-directory-packages (dir)
    (aged-package dir "lacker"
                  "(defpackage #:lacker (:use #:cl #:tuatara))
(in-package #:lacker)
(setf tuatara-user::*package-test-said* :ran)
(define-command lacker-run () \"Run.\" (tuatara-user::no-such-function))
(defun lacker-other () (tuatara.defaults::no-such-thing))")
    (setf (symbol-value 'tuatara-user::*package-test-said*) nil)
    (unwind-protect
         (progn
           (let ((tuatara.runtime:*debug-on* t))
             (false (tuatara-user:load-package "lacker" :source (namestring dir))))
           ;; the debug log has them too
           (true (find-if (lambda (report) (search "lacker refused, its code using names this Tuatara lacks: tuatara-user::no-such-function"
                                                   (fourth report)))
                          (sb-concurrency:list-queue-contents tuatara.runtime:*debug-reports*)))
           (false (tuatara.packages::package-loaded-p "lacker"))
           (is string= "lacker needs names this Tuatara lacks: tuatara-user::no-such-function, tuatara.defaults::no-such-thing; update the package (M-x package-update) or Tuatara"
               (package-failure "lacker"))
           (is equal "refused" (tuatara.packages::package-condition "lacker" nil))
           (false (symbol-value 'tuatara-user::*package-test-said*))
           (false (find-package "LACKER"))
           (false (tuatara.command:find-command "lacker-run")))
      (dolist (name '("NO-SUCH-FUNCTION" "NO-SUCH-THING"))
        (dolist (home '(:tuatara-user :tuatara.defaults))
          (let ((symbol (find-symbol name home)))
            (when (and symbol (eq (symbol-package symbol) (find-package home))) (unintern symbol home))))))))

(define-test (packages names-of-its-own-lacking-refused)
  ;; a name the package calls as its own and defines nowhere, as an API name renamed since:
  ;; known once its files have loaded, what it put in place is taken out again; loaded again
  ;; from what was compiled, it is refused before any of it runs, till its file changes
  (with-directory-packages (dir)
    (let ((text "(defpackage #:renamer (:use #:cl #:tuatara))
(in-package #:renamer)
(push :ran tuatara-user::*package-test-order*)
(define-command renamer-run () \"Run.\" (docx-file-octets \"x\"))
(defun renamer-view (path) (declare (ignore path)) nil)
(pushnew 'renamer-view *file-views*)
(define-major-mode renamer-mode special-mode \"A list.\" :extensions (\"renamer-test\"))"))
      (aged-package dir "renamer" text)
      (setf (symbol-value 'tuatara-user::*package-test-order*) '())
      (flet ((left-p ()
               (or (find-package "RENAMER")
                   (tuatara.command:find-command "renamer-run")
                   (tuatara.command:find-command "renamer-mode")
                   (string= "RENAMER-MODE" (symbol-name (tuatara.modes:mode-for-file "x.renamer-test")))
                   (find "RENAMER-VIEW"
                         tuatara:*file-views*
                         :key (lambda (view) (and (symbolp view) (symbol-name view)))
                                                             :test #'equal))))
        (false (tuatara-user:load-package "renamer" :source (namestring dir)))
        (is string= "renamer needs names this Tuatara lacks: docx-file-octets; update the package (M-x package-update) or Tuatara"
            (package-failure "renamer"))
        (is equal '(:ran) (symbol-value 'tuatara-user::*package-test-order*))
        (false (left-p))
        (dotimes (i 2)
          (forget-failure "renamer")
          (false (tuatara-user:load-package "renamer" :source (namestring dir)))
          (is equal '(:ran) (symbol-value 'tuatara-user::*package-test-order*))
          (true (search "docx-file-octets" (package-failure "renamer")))
          (false (left-p)))
        ;; mended: it loads
        (forget-failure "renamer")
        (directory-packages dir (list "renamer" (concatenate 'string text "
(defun docx-file-octets (path) path)")))
        (true (tuatara-user:load-package "renamer" :source (namestring dir)))
        (true (tuatara.packages::package-loaded-p "renamer"))
        (is equal '(:ran :ran) (symbol-value 'tuatara-user::*package-test-order*))
        (tuatara.packages::forget-package-code (list (find-package "RENAMER")))
        (false (left-p))))))

(define-test (packages variable-lacking-refused)
  ;; a variable it reads that is nowhere is refused as a function is, Tuatara's or its own
  (with-directory-packages (dir)
    (aged-package dir "reader"
                  "(defpackage #:reader-pkg (:use #:cl #:tuatara))
(in-package #:reader-pkg)
(defun reader-value () tuatara-user::*no-such-setting*)")
    (aged-package dir "ownreader"
                  "(defpackage #:ownreader (:use #:cl #:tuatara))
(in-package #:ownreader)
(defun ownreader-value () *missing-own*)")
    (unwind-protect
         (progn
           (false (tuatara-user:load-package "reader" :source (namestring dir)))
           (is string= "reader needs names this Tuatara lacks: tuatara-user::*no-such-setting*; update the package (M-x package-update) or Tuatara"
               (package-failure "reader"))
           (false (find-package "READER-PKG"))
           (false (tuatara-user:load-package "ownreader" :source (namestring dir)))
           (is string= "ownreader needs names this Tuatara lacks: *missing-own*; update the package (M-x package-update) or Tuatara"
               (package-failure "ownreader"))
           (false (find-package "OWNREADER")))
      (let ((symbol (find-symbol "*NO-SUCH-SETTING*" :tuatara-user)))
        (when symbol (unintern symbol :tuatara-user))))))

(defun nameless-command-places ()
  "The places commands are made for (COMMANDS-FOR-MODE) whose names belong to
no Lisp package any more, each (COMMAND . PLACE)."
  (loop for command being the hash-keys of tuatara.defaults::*mode-commands* using (hash-value entries)
        append (loop for (place) in entries unless (symbol-package place) collect (cons command place))))

(defun offered-commands ()
  "The commands M-x offers in a new session on an empty scratch buffer."
  (multiple-value-bind (f editor) (run-keys "" :text "")
    (declare (ignore f))
    (let ((tuatara.command:*editor* editor))
      (mapcar #'tuatara.command:candidate-result (tuatara.defaults::command-candidates)))))

(define-test (packages refused-takes-back-places)
  ;; a package refused once its files have loaded takes back the places it gave Tuatara's
  ;; own commands (a list's keys for its mode); its mended version loads after it in the
  ;; same session, and M-x offers every command as before
  (with-directory-packages (dir)
    (let ((text "(defpackage #:placer (:use #:cl #:tuatara))
(in-package #:placer)
(define-major-mode placer-mode special-mode \"A list of places.\")
(entry-list-keys 'placer-mode \"a list of places\")
(define-command placer-run () \"Run.\" (placer-missing))"))
      (aged-package dir "placer" text)
      (unwind-protect
           (progn
             (false (tuatara-user:load-package "placer" :source (namestring dir)))
             (true (search "placer-missing" (package-failure "placer")))
             (false (find-package "PLACER"))
             (false (nameless-command-places))
             (forget-failure "placer")
             (directory-packages dir (list "placer" (concatenate 'string text "
(defun placer-missing () nil)")))
             (true (tuatara-user:load-package "placer" :source (namestring dir)))
             (is equal '("a list of places")
                 (loop for (place . where) in (gethash 'tuatara.defaults::list-visit tuatara.defaults::*mode-commands*)
                       when (string= "PLACER-MODE" (symbol-name place)) collect where))
             (false (nameless-command-places))
             (let ((offered (offered-commands)))
               (true (member 'tuatara.defaults::find-file offered))
               (false (member 'tuatara.defaults::list-visit offered))))
        (when (find-package "PLACER")
          (tuatara.packages::forget-package-code (list (find-package "PLACER"))))))))

(define-test (packages failed-takes-back-what-it-loaded)
  ;; a package whose file fails part of the way, its command and its list's keys already in
  ;; place, leaves nothing of it behind: not its Lisp package, its command or its places
  (with-directory-packages (dir)
    (aged-package dir "failer"
                  "(defpackage #:failer (:use #:cl #:tuatara))
(in-package #:failer)
(define-major-mode failer-mode special-mode \"A list that fails.\")
(entry-list-keys 'failer-mode \"a list that fails\")
(define-command failer-run () \"Run.\" nil)
(error \"The failer package fails as it loads.\")")
    (unwind-protect
         (progn
           (false (tuatara-user:load-package "failer" :source (namestring dir)))
           (true (search "fails as it loads" (package-failure "failer")))
           (false (find-package "FAILER"))
           (false (tuatara.command:find-command "failer-run"))
           (false (find "FAILER-MODE" (tuatara.modes:mode-names) :key #'symbol-name :test #'string=))
           (false (nameless-command-places)))
      (when (find-package "FAILER")
        (tuatara.packages::forget-package-code (list (find-package "FAILER")))))))

(define-test (packages forgotten-keys-labels-and-toggles)
  ;; a package taken out takes back its keys in Tuatara's keymaps with their labels, the
  ;; prefix keys left with nothing under them, and its ways of showing text
  (let ((package (make-package "FORGOTTEN" :use '("CL" "TUATARA")))
        (keymap (tuatara.modes:mode-keymap 'tuatara.modes:fundamental-mode))
        (prefix (tuatara.frontend:parse-key-sequence "C-c +")))
    (unwind-protect
         (progn
           (let ((*package* package))
             (eval (read-from-string "(progn
  (define-command forgotten-run () \"Run.\" nil)
  (define-keys (mode-keymap 'fundamental-mode) '((\"C-c + r\" forgotten-run)))
  (label-keys (mode-keymap 'fundamental-mode) '((\"C-c +\" \"forgotten\")))
  (define-display-toggle forgotten-ruler \"Ruler\" \"Show a ruler in the current buffer, or stop.\"
    :command forgotten-ruler-mode :lookup forgotten-ruler-p
    :global *forgotten-ruler* :what \"show a ruler\" :example \"'((text-mode . t))\"))")))
           (let ((labelled (tuatara.command:lookup-key keymap (list (first prefix)) :parents nil)))
             (is string= "forgotten" (tuatara.command:key-label labelled (second prefix)))
             (true (tuatara.command:lookup-key keymap prefix :parents nil))
             (true (tuatara.defaults::find-display-toggle (find-symbol "FORGOTTEN-RULER" package)))
             (tuatara.packages::forget-package-code (list package))
             (false (tuatara.command:lookup-key keymap prefix :parents nil))
             (false (tuatara.command:key-label labelled (second prefix)))
             (false (find "FORGOTTEN-RULER" tuatara.defaults::*display-toggles*
                          :key (lambda (toggle) (symbol-name (tuatara.defaults::display-toggle-name toggle)))
                          :test #'string=))))
      (tuatara.command:define-key keymap prefix nil)
      (let ((c-c (tuatara.command:lookup-key keymap (list (first prefix)) :parents nil)))
        (when (and (tuatara.command:keymap-p c-c) (zerop (hash-table-count (tuatara.command::keymap-table c-c))))
          (tuatara.command:define-key keymap (list (first prefix)) nil)))
      (dolist (name '("*FORGOTTEN-RULER*" "*FORGOTTEN-RULER-MODES*"))
        (let ((symbol (find-symbol name :tuatara-user)))
          (when symbol
            (unexport symbol :tuatara-user)
            (unintern symbol :tuatara-user))))
      (when (package-name package)
        (tuatara.packages::forget-package-code (list package))))))

(define-test (packages api-names-loaded)
  ;; a package using only the API, and a name of another library, loads; loaded again from
  ;; what was compiled, what it needs is looked for again: gone, it is refused before it runs
  (with-directory-packages (dir)
    (aged-package dir "apiuser"
                  "(defpackage #:apiuser (:use #:cl #:tuatara))
(in-package #:apiuser)
(push :ran tuatara-user::*package-test-order*)
(define-command apiuser-run () \"Say so.\" (message \"~a ~a\" (buffer-name (current-buffer)) (cl-ppcre:scan \"a\" \"a\")))
(defun apiuser-said () tuatara-user::*apiuser-test-value*)")
    (setf (symbol-value 'tuatara-user::*package-test-order*) '()
          (symbol-value 'tuatara-user::*apiuser-test-value*) :there)
    (unwind-protect
         (progn
           (true (tuatara-user:load-package "apiuser" :source (namestring dir)))
           (true (tuatara.packages::package-loaded-p "apiuser"))
           (false (package-failure "apiuser"))
           (true (tuatara.command:find-command "apiuser-run"))
           (is equal '(:ran) (symbol-value 'tuatara-user::*package-test-order*))
           (flet ((unload ()
                    (setf tuatara.packages::*loaded-packages*
                          (remove "apiuser" tuatara.packages::*loaded-packages* :test #'string=))
                    (tuatara.packages::forget-package-code (list (find-package "APIUSER")))))
             (let ((fasl (tuatara.packages::package-fasl (merge-pathnames "apiuser/apiuser.lisp" dir)))
                   (then (- (sb-posix:time) 30)))
               ;; loaded again from what was compiled, not compiled again
               (sb-posix:utimes fasl then then)
               (unload)
               (true (tuatara-user:load-package "apiuser" :source (namestring dir)))
               (is = then (sb-posix:stat-mtime (sb-posix:stat fasl))))
             (is equal '(:ran :ran) (symbol-value 'tuatara-user::*package-test-order*))
             (makunbound 'tuatara-user::*apiuser-test-value*)
             (unload))
           (false (tuatara-user:load-package "apiuser" :source (namestring dir)))
           (is equal '(:ran :ran) (symbol-value 'tuatara-user::*package-test-order*))
           (true (search "tuatara-user::*apiuser-test-value*" (package-failure "apiuser"))))
      (when (find-package "APIUSER")
        (tuatara.packages::forget-package-code (list (find-package "APIUSER"))))
      (makunbound 'tuatara-user::*apiuser-test-value*))))

(define-test (packages probe-size-bounded)
  ;; finding which commit to fetch is held to the most a package may take as well: past it,
  ;; nothing is checked out
  (with-package-repository (dir where)
    (let ((most tuatara.defaults::*package-size-limit*)
          (directories '()))
      (setf tuatara.defaults::*package-size-limit* 1)
      (sb-int:encapsulate 'tuatara.packages::package-git 'probe-size-bounded
                          (lambda (function directory &rest arguments)
                            (push directory directories)
                            (apply function directory arguments)))
      (unwind-protect
           (fail (tuatara.packages::fetch-package "pkgone" where nil) 'tuatara.packages::package-too-large)
        (sb-int:unencapsulate 'tuatara.packages::package-git 'probe-size-bounded)
        (setf tuatara.defaults::*package-size-limit* most))
      (true (find-if (lambda (directory) (search "/probe/" directory)) directories))
      (false (find-if-not (lambda (directory) (search "/probe/" directory)) directories))
      (false (directory (merge-pathnames ".fetching-*/" (tuatara.packages:packages-directory)))))))

(define-test (packages manager-help-shows-the-marks-shown)
  ;; the package manager's help gives the signs its list shows for a mark
  (let ((help (documentation 'tuatara.packages::package-manager 'function)))
    (true (search (format nil "(~a marked, ~a marked for deletion)"
                          (tuatara.defaults::list-mark-sign :marked) (tuatara.defaults::list-mark-sign :deletion))
                  help))))

(define-test (packages api-runs-a-program-on-text)
  ;; a package splits a command line and runs a program of its own over text through a pipe,
  ;; with the API's names alone
  (with-directory-packages (dir)
    (aged-package dir "filteruser"
                  "(defpackage #:filteruser (:use #:cl #:tuatara))
(in-package #:filteruser)
(defun filteruser-run (line text)
  (let ((words (command-words line :backslash :escapes))
        (octets (string-octets text)))
    (multiple-value-bind (output said exit)
        (filter-through-program (cons (find-program (first words)) (rest words))
                                (lambda (fd) (write-octets fd octets :path \"filteruser\")))
      (list (lenient-string output) said exit))))")
    (unwind-protect
         (progn
           (true (tuatara-user:load-package "filteruser" :source (namestring dir)))
           (false (package-failure "filteruser"))
           (is equal '("A,B C" "" 0)
               (funcall (find-symbol "FILTERUSER-RUN" "FILTERUSER") "tr 'a-z,' \"A-Z,\"" "a,b c")))
      (when (find-package "FILTERUSER")
        (tuatara.packages::forget-package-code (list (find-package "FILTERUSER")))))))

(define-test (packages api-names-used)
  ;; informative: the names of the API no package of ours uses, said in the test's output, so
  ;; that they can be weighed; none is taken away for it, other packages may use them
  (let ((sources (sibling-packages-directory)))
    (if (null sources)
        (skip "the packages' sources are not beside the editor's" (true nil))
        (let* ((words (make-hash-table :test 'equal))
               (unused '()))
          ;; every word of the packages' code that could name a symbol, without its package
          (dolist (file (directory (merge-pathnames "**/*.lisp" sources)))
            (dolist (word (tuatara.text:split-words (string-downcase (uiop:read-file-string file))
                                                    (coerce (format nil " ~c~c~c()'`,\";#|" #\Tab #\Newline #\Return) 'list)))
              (setf (gethash (subseq word (1+ (or (position #\: word :from-end t) -1))) words) t)))
          (setf unused (loop for symbol in tuatara.defaults::*api*
                             for name = (string-downcase (symbol-name symbol))
                             unless (gethash name words)
                               collect name))
          (format t "~&~d of the API's ~d names are used by no package of ours:~%~{  ~a~%~}"
                  (length unused) (length tuatara.defaults::*api*) unused)
          (true (listp unused))))))

(define-test (packages unused-sources-removed)
  ;; package-remove-unused, in a packages directory of the test's own: a source nothing names goes,
  ;; asked once with sizes; a package named, one left out (:disabled), a fetch's scratch directory
  ;; and what a link points to stay; n deletes nothing
  (with-test-directory (dir)
    (let* ((top (namestring (ensure-directories-exist (merge-pathnames "packages/" dir))))
           (outside (ensure-directories-exist (merge-pathnames "outside/" dir)))
           (tuatara-user:*packages-directory* top)
           (tuatara.packages::*declared-packages* (list (list "used" "https://example.org/used" nil nil nil)))
           (tuatara.packages::*left-out-packages* '())
           (tuatara.defaults::*package-sources* '())
           (commit "0123456789abcdef0123456789abcdef01234567"))
      (flet ((kept (where name)
               (let ((file (merge-pathnames (format nil "~a/~a/~a/~a.lisp"
                                                    (tuatara.io:path-file-name where) name commit name) top)))
                 (alexandria:write-string-into-file (make-string 2000 :initial-element #\;)
                                                    (ensure-directories-exist file))
                 file)))
        (let ((gone (kept "https://example.org/gone" "gone"))
              (used (kept "https://example.org/used" "used"))
              (other (kept "https://example.org/used" "other"))
              (off (kept "https://example.org/off" "off"))
              (scratch (ensure-directories-exist (merge-pathnames ".fetching-abc/probe/" top))))
          (alexandria:write-string-into-file "theirs" (merge-pathnames "keep.txt" outside))
          (sb-posix:symlink (string-right-trim "/" (namestring outside))
                            (concatenate 'string top "https%3A%2F%2Fexample.org%2Flink"))
          (tuatara-user:load-package "off" :source "https://example.org/off" :disabled t)
          ;; n: nothing deleted
          (let ((f (auto-save-session
                    (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
                          (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                          "n RET" (list :until (lambda () (echo-says "Nothing deleted"))))
                    :width 160 :height 30)))
            (true (tuatara.frontend.fake:fake-frame-showing f "https://example.org/gone, all of it"))
            (true (tuatara.frontend.fake:fake-frame-showing f "other from https://example.org/used"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Delete 2 package sources no longer named"))
            (false (tuatara.frontend.fake:fake-frame-showing f "example.org/off"))
            (false (tuatara.frontend.fake:fake-frame-showing f "example.org/link")))
          (true (probe-file gone))
          (true (probe-file other))
          ;; yes: those two go, the rest stays
          (let ((pushed (push "used" tuatara.packages::*fetching-packages*)))
            (declare (ignore pushed))
            (unwind-protect
                 (auto-save-session
                  (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
                        (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                        "y RET" (list :until (lambda () (echo-says "freed")))))
              (setf tuatara.packages::*fetching-packages*
                    (remove "used" tuatara.packages::*fetching-packages* :test #'string=))))
          (false (probe-file gone))
          (true (probe-file used))
          (true (probe-file off))
          (true (probe-file scratch))
          (true (probe-file (merge-pathnames "keep.txt" outside)))
          ;; "other" sat beside a package being fetched: left alone while it ran
          (true (probe-file other)))))))

(define-test (packages unused-sources-none)
  ;; nothing unused: said, nothing asked
  (with-test-directory (dir)
    (let ((tuatara-user:*packages-directory* (namestring (ensure-directories-exist (merge-pathnames "packages/" dir))))
          (tuatara.packages::*declared-packages* '())
          (tuatara.defaults::*package-sources* '()))
      (let* ((said nil)
             (f (auto-save-session
                 (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
                       (list :until (lambda () (setf said (echo-says "No package source kept is unused")))))
                 :width 160)))
        (true said)
        (false (tuatara.frontend.fake:fake-frame-showing f "no longer named,"))))))

(defmacro with-unused-source ((top gone) &body body)
  "Run BODY with TOP a packages directory of a test directory's own and GONE the
entry file of a package of a source nothing names, kept in it."
  (let ((dir (gensym "DIR")))
    `(with-test-directory (,dir)
       (let* ((,top (namestring (ensure-directories-exist (merge-pathnames "packages/" ,dir))))
              (tuatara-user:*packages-directory* ,top)
              (tuatara.packages::*declared-packages* '())
              (tuatara.packages::*left-out-packages* '())
              (tuatara.defaults::*package-sources* '())
              (,gone (merge-pathnames (format nil "~a/gone/0123456789abcdef0123456789abcdef01234567/gone.lisp"
                                              (tuatara.io:path-file-name "https://example.org/gone"))
                                      ,top)))
         (alexandria:write-string-into-file "(defvar *gone* t)" (ensure-directories-exist ,gone))
         ,@body))))

(define-test (packages unused-sources-looked-at-again)
  ;; a source named while the question waits is not deleted: each is looked at again, as things are then
  (with-unused-source (top gone)
    (auto-save-session
     (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
           (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
           (list :until (lambda ()
                          (push (list "gone" "https://example.org/gone" nil nil nil)
                                tuatara.packages::*declared-packages*)))
           "y RET" (list :until (lambda () (echo-says "no longer named deleted: 0")))))
    (true (probe-file gone))))

(define-test (packages unused-sources-init-file-read-through)
  ;; an init file not read to its end: nothing looked for, nothing deleted, said why
  (with-unused-source (top gone)
    (let ((tuatara.defaults::*init-file-stop* (cons 12 "end of file")) (said nil))
      (auto-save-session
       (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
             (list :until (lambda () (setf said (echo-says "The init file was not read to its end (12: end of file); nothing is deleted until it is")))))
       :width 200)
      (true said)
      (true (probe-file gone)))))

(define-test (packages unused-sources-failure-said)
  ;; looking failing is said, in plain words
  (with-unused-source (top gone)
    (let ((said nil))
      (sb-int:encapsulate 'tuatara.packages::unused-package-directories 'unused-sources-failure-said
                          (lambda (function) (declare (ignore function)) (error "the directory could not be read")))
      (unwind-protect
           (auto-save-session
            (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
                  (list :until (lambda () (setf said (echo-says "Looking for package sources no longer named failed")))))
            :width 200)
        (sb-int:unencapsulate 'tuatara.packages::unused-package-directories 'unused-sources-failure-said))
      (true said)
      (true (probe-file gone)))))

(define-test (packages unused-sources-parent-link-left)
  ;; a source's directory swapped for a link before the deleting: what the link leads to stays
  (with-test-directory (dir)
    (let* ((top (namestring (ensure-directories-exist (merge-pathnames "packages/" dir))))
           (outside (namestring (ensure-directories-exist (merge-pathnames "outside/" dir))))
           (tuatara-user:*packages-directory* top)
           (tuatara.packages::*declared-packages* (list (list "used" "https://example.org/mixed" nil nil nil)))
           (tuatara.packages::*left-out-packages* '())
           (tuatara.defaults::*package-sources* '())
           (source (concatenate 'string top (tuatara.io:path-file-name "https://example.org/mixed")))
           (theirs (merge-pathnames "other/c0ffee0123456789abcdef0123456789abcdef01/other.lisp" outside)))
      (alexandria:write-string-into-file "x" (ensure-directories-exist (format nil "~a/other/c0ffee0123456789abcdef0123456789abcdef01/other.lisp" source)))
      (alexandria:write-string-into-file "theirs" (ensure-directories-exist theirs))
      (auto-save-session
       (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
             (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
             (list :until (lambda ()
                            (uiop:delete-directory-tree (pathname (format nil "~a/" source)) :validate t)
                            (sb-posix:symlink (string-right-trim "/" outside) source)
                            t))
             "y RET" (list :until (lambda () (echo-says "no longer named deleted")))))
      (true (probe-file theirs)))))

(define-test (packages start-waits-quietly)
  ;; a load at the start, or by a command or mode, of a package in hand waits without a word
  (let ((tuatara.packages::*packages-in-hand* (list (cons "pkgq" :deciding)))
        (tuatara.packages::*declared-packages* '())
        (tuatara.defaults::*package-sources* '()))
    (let ((f (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "pkgq" :source "https://example.org/pkgq") t))
                    (list :wait 0.2))
              :width 200)))
      (false (tuatara.frontend.fake:fake-frame-showing f "already running")))))

(define-test (packages kept-list-unreadable-reported)
  ;; a kept list that cannot be read is none, as before; debug mode keeps why in its log
  (with-test-directory (dir)
    (let ((good (tuatara.io:native (merge-pathnames "good.needs" dir)))
          (broken (tuatara.io:native (put-file (merge-pathnames "broken.needs" dir) "(:needs ("))))
      (tuatara.packages::write-kept-list good '(:needs ()))
      (drain-debug-reports)
      (unwind-protect
           (progn
             (is equal '((:needs ()) t) (multiple-value-list (tuatara.packages::read-kept-list good)))
             (is equal '(nil) (multiple-value-list (tuatara.packages::read-kept-list broken)))
             (true (sb-concurrency:queue-empty-p tuatara.runtime:*debug-reports*))
             (tuatara.runtime:set-debug t)
             (is equal '(nil) (multiple-value-list (tuatara.packages::read-kept-list broken)))
             (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                                while report collect (princ-to-string (fourth report)))))
               (true (find-if (lambda (text) (search (format nil "reading ~a failed" broken) text)) texts)
                     "~s" texts)))
        (drain-debug-reports)))))

(defparameter *unloader-text* "(defpackage #:unloader (:use #:cl #:tuatara))
(in-package #:unloader)
(define-package-setting *unloader-size* 3 \"How many things a list of things unloaded shows.\")
(define-major-mode unloader-mode special-mode \"A list of things unloaded.\")
(entry-list-keys 'unloader-mode \"a list of things unloaded\")
(defmethod list-entry-noun ((mode unloader-mode)) \"thing\")
(define-command unloader-run () \"Run.\" nil)
(define-keys (mode-keymap 'fundamental-mode) '((\"C-c + r\" unloader-run)))
(label-keys (mode-keymap 'fundamental-mode) '((\"C-c +\" \"unloader\")))
(defun unloader-tick () nil)
(pushnew 'unloader-tick *tick-functions*)
(push (lambda (path show) (declare (ignore path show)) nil) *file-views*)
(defstruct (unloader-job (:include job)))
(defun unloader-start ()
  (start-job-thread (make-unloader-job) (lambda (job) (declare (ignore job)) (loop (job-check-stop) (sleep 0.01)))))"
  "A package of a directory that puts in Tuatara one of each thing unloading takes out.")

(define-test (packages unloaded-taken-out)
  ;; K in the package manager unloads a package: its command, mode, key and label, hooks,
  ;; tick function, setting, methods and Lisp package go, its job is stopped, a buffer in its
  ;; mode is put in fundamental-mode; its load-package stays, and package-load brings it back
  ;; with the value given to its setting
  (with-directory-packages (dir)
    (aged-package dir "unloader" *unloader-text*)
    (let* ((init (merge-pathnames "init.lisp" dir))
           (keymap (tuatara.modes:mode-keymap 'tuatara.modes:fundamental-mode))
           (prefix (tuatara.frontend:parse-key-sequence "C-c +"))
           (views (length tuatara.defaults::*file-views*))
           (buffer nil) (job nil))
      (alexandria:write-string-into-file (format nil "(load-package \"unloader\" :source ~s)~%" (namestring dir)) init)
      (unwind-protect
           (progn
             (true (tuatara-user:load-package "unloader" :source (namestring dir)))
             (let* ((package (find-package "UNLOADER"))
                    (mode (find-symbol "UNLOADER-MODE" package))
                    (class (find-class mode))
                    (setting (find-symbol "*UNLOADER-SIZE*" :tuatara-user))
                    (labelled (tuatara.command:lookup-key keymap (list (first prefix)) :parents nil)))
               (setf job (funcall (find-symbol "UNLOADER-START" package))
                     buffer (tuatara.buffer:make-buffer "*unloader test*"))
               (tuatara.modes:set-major-mode buffer mode)
               (setf (symbol-value setting) 7)
               (is string= "unloader" (tuatara.command:key-label labelled (second prefix)))
               (is = (1+ views) (length tuatara.defaults::*file-views*))
               (true (sb-mop:specializer-direct-methods class))
               (auto-save-session
                (list "M-x p a c k a g e - m a n a g e r RET K"
                      (list :until (lambda () (not (tuatara.packages::package-loaded-p "unloader"))))))
               (false (package-name package))
               (false (tuatara.command:find-command "unloader-run"))
               (false (member mode (tuatara.modes:mode-names)))
               (false (tuatara.command:lookup-key keymap prefix :parents nil))
               (false (tuatara.command:key-label labelled (second prefix)))
               (is = views (length tuatara.defaults::*file-views*))
               (false (find "UNLOADER-TICK" tuatara.command:*tick-functions*
                            :key (lambda (f) (and (symbolp f) (symbol-name f))) :test #'equal))
               (false (sb-mop:specializer-direct-methods class))
               (wait-for-job job)
               (false (tuatara.runtime:job-running-p job))
               (is eq :internal (nth-value 1 (find-symbol "*UNLOADER-SIZE*" :tuatara-user)))
               (false (get setting 'tuatara.packages::package-setting))
               (true (tuatara.buffer:live-buffer-p buffer))
               (is eq 'tuatara.modes:fundamental-mode (type-of (tuatara.modes:buffer-major-mode buffer)))
               (is string= "unloaded" (tuatara.packages::package-condition "unloader" nil))
               (true (tuatara.packages::declared-package "unloader"))
               ;; loaded again by its load-package, the value given to its setting kept
               (let ((tuatara.defaults:*config-file* init))
                 (tuatara.packages::load-packages-again '("unloader")))
               (true (tuatara.packages::package-loaded-p "unloader"))
               (true (tuatara.command:find-command "unloader-run"))
               (is eq :external (nth-value 1 (find-symbol "*UNLOADER-SIZE*" :tuatara-user)))
               (is eql 7 (symbol-value setting))))
        (when buffer (tuatara.buffer:kill-buffer buffer))
        (when job
          (tuatara.runtime:job-stop job)
          (wait-for-job job)
          (tuatara.runtime::forget-job job))
        (when (find-package "UNLOADER")
          (tuatara.packages::forget-package-code (list (find-package "UNLOADER"))))
        (tuatara.command:define-key keymap prefix nil)
        (let ((c-c (tuatara.command:lookup-key keymap (list (first prefix)) :parents nil)))
          (when (and (tuatara.command:keymap-p c-c) (zerop (hash-table-count (tuatara.command::keymap-table c-c))))
            (tuatara.command:define-key keymap (list (first prefix)) nil)))
        (let ((symbol (find-symbol "*UNLOADER-SIZE*" :tuatara-user)))
          (when symbol
            (unexport symbol :tuatara-user)
            (unintern symbol :tuatara-user)))))))

(define-test (packages removed-passed-over-at-start)
  ;; a package removed is not loaded at the start though the init file names it, said once in
  ;; *Messages*, listed as removed; L in the package manager loads it, no longer removed, and R
  ;; removes it again; the init file is left as it was
  (with-directory-packages (dir)
    (directory-packages dir '("remover" "(defparameter *package-test-said* :remover)"))
    (let* ((init (merge-pathnames "init.lisp" dir))
           (text (format nil "(load-package \"remover\" :source ~s)~%(load-package \"remover\" :source ~s)~%"
                         (namestring dir) (namestring dir))))
      (alexandria:write-string-into-file text init)
      (tuatara.packages::remove-package "remover")
      (let ((tuatara.defaults:*config-file* init)
            (tuatara.packages::*removed-packages-said* '())
            (said nil) (loaded-at-start t))
        (auto-save-session
         (list "M-x p a c k a g e - m a n a g e r RET"
               (list :until (lambda () (tuatara.frontend.fake:fake-frame-showing
                                        (tuatara.command:editor-frontend tuatara.command:*editor*) "removed")))
               (list :until (lambda ()
                              (setf said (count-matches "The package remover is removed" (messages-text))
                                    loaded-at-start (boundp 'tuatara-user::*package-test-said*))
                              t))
               "L" (list :until (lambda () (tuatara.packages::package-loaded-p "remover")))
               (list :until (lambda () (boundp 'tuatara-user::*package-test-said*)))
               "R" (list :until (lambda () (tuatara.packages::package-removed-p "remover"))))
         :width 160 :height 20)
        ;; named twice at the start, said once
        (is = 1 said)
        (false loaded-at-start))
      (false (tuatara.packages::package-loaded-p "remover"))
      (is string= "removed" (tuatara.packages::package-condition "remover" nil))
      (is string= text (tuatara.io:file-text init)))))

(define-test (packages deleted-with-its-files)
  ;; package-delete unloads a package of a directory of yours, removes it, and deletes its
  ;; compiled files after asking with their size; the directory of yours stays as it is
  (with-directory-packages (dir)
    (aged-package dir "deleter" "(defpackage #:deleter (:use #:cl #:tuatara))
(in-package #:deleter)
(define-command deleter-run () \"Run.\" nil)")
    (let ((source (merge-pathnames "deleter/deleter.lisp" dir)))
      (true (tuatara-user:load-package "deleter" :source (namestring dir)))
      (let ((fasl (tuatara.packages::package-fasl source)))
        (true (probe-file fasl))
        (auto-save-session
         (list "M-x p a c k a g e - d e l e t e RET d e l e t e r RET"
               (list :until (lambda () (tuatara.frontend.fake:fake-frame-showing
                                        (tuatara.command:editor-frontend tuatara.command:*editor*)
                                        "Unload, remove and delete the package deleter")))
               "y e s RET" (list :until (lambda () (echo-says "deleter deleted"))))
         :width 160 :height 20)
        (false (probe-file fasl))
        (true (probe-file source))
        (false (find-package "DELETER"))
        (false (tuatara.command:find-command "deleter-run"))
        (false (tuatara.packages::package-loaded-p "deleter"))
        (true (tuatara.packages::package-removed-p "deleter"))
        (false (tuatara.packages::package-state "deleter" (namestring dir) nil))))))

(define-test (packages setting-values-declared)
  ;; a package's setting says the values it takes as Tuatara's own do, shown in words and
  ;; checked; one that says none still loads
  (let ((package (make-package "VALUER" :use '(:cl :tuatara))))
    (unwind-protect
         (let ((*package* package))
           (eval (read-from-string "(define-package-setting *valuer-side* :left \"Where the valuer goes.
- :left: at the left.
- :right: at the right.\" :values (member :left :right))"))
           (eval (read-from-string "(define-package-setting *valuer-size* 3 \"How big the valuer is.\")"))
           (let ((side (find-symbol "*VALUER-SIDE*" :tuatara-user))
                 (size (find-symbol "*VALUER-SIZE*" :tuatara-user)))
             (is string= ":left, :right" (tuatara.runtime::setting-values-words side))
             (false (tuatara.runtime::setting-value-fault side))
             (setf (symbol-value side) :middle)
             (is string= "*valuer-side* is :middle, but it takes :left, :right"
                 (tuatara.runtime::setting-value-fault side))
             (is = 3 (symbol-value size))
             (false (tuatara.runtime::setting-values-words size))))
      (dolist (name '("*VALUER-SIDE*" "*VALUER-SIZE*"))
        (let ((symbol (find-symbol name :tuatara-user)))
          (when symbol
            (remprop symbol 'tuatara.packages::package-setting)
            (unexport symbol :tuatara-user)
            (makunbound symbol)
            (unintern symbol :tuatara-user))))
      (delete-package package))))

(define-test (packages fingerprint-and-name-forms)
  ;; a key's fingerprint and a package's name are told by one reader each
  (let ((sha (format nil "SHA256:~a" (make-string 43 :initial-element #\a))))
    (true (tuatara.packages::signer-fingerprint-p sha))
    (true (tuatara.packages::signer-fingerprint-p (format nil "~{~a~}" (make-list 10 :initial-element "abcd"))))
    (false (tuatara.packages::signer-fingerprint-p "SHA256:abc"))
    (false (tuatara.packages::signer-fingerprint-p (make-string 41 :initial-element #\a)))
    (true (tuatara.packages::signer-fingerprint-p "unsigned" :stored t))
    (false (tuatara.packages::signer-fingerprint-p "unsigned"))
    (true (tuatara.packages::signer-fingerprint-p (format nil "key:~a" (make-string 40 :initial-element #\A)) :stored t))
    (false (tuatara.packages::signer-fingerprint-p (make-string 40 :initial-element #\a) :stored t))
    ;; the state files of earlier releases are read as those releases wrote them
    (true (tuatara.packages::signer-fingerprint-p "SHA256:abcDEF0123456789+/=" :stored t))
    (true (tuatara.packages::signer-fingerprint-p (make-string 50 :initial-element #\A) :stored t)))
  (true (tuatara.packages::package-name-p "my-pkg2"))
  (false (tuatara.packages::package-name-p "-pkg"))
  (false (tuatara.packages::package-name-p "Pkg"))
  (false (tuatara.packages::package-name-p nil)))

(define-test (packages failure-kept-once-for-a-name)
  ;; a package that failed twice has one reason kept, the latest, and none after it is cleared
  (let ((tuatara.packages::*package-failures* '()))
    (tuatara.packages::note-package-failure "one" "first")
    (tuatara.packages::note-package-failure "two" "other")
    (tuatara.packages::note-package-failure "one" "second")
    (is equal '(("one" . "second") ("two" . "other")) tuatara.packages::*package-failures*)
    (tuatara.packages::clear-package-failure "one")
    (is equal '(("two" . "other")) tuatara.packages::*package-failures*)))

(define-test (packages whole-clone-only-when-partial-fails)
  ;; a fetch tries the whole repository again only when git's words say a partial clone cannot be had
  (with-package-repository (dir where)
    (flet ((fetching (words)
             (sb-int:encapsulate 'tuatara.packages::package-git 'partial-test
                                 (lambda (function directory &rest arguments)
                                   (if (and words (member "--filter=blob:none" arguments :test #'string=)
                                            (equal (first arguments) "fetch") (search "/files-" directory))
                                       (tuatara.runtime:refuse "~a" words)
                                       (apply function directory arguments))))
             (unwind-protect
                  (handler-case (list :kept (tuatara.packages::fetch-package "pkgone" where nil))
                    (error (condition) (list :failed (princ-to-string condition))))
               (sb-int:unencapsulate 'tuatara.packages::package-git 'partial-test))))
      (let ((refused (fetching "git fetch: fatal: No space left on device")))
        (is eq :failed (first refused))
        (true (search "No space left" (second refused))))
      (dolist (words '("git fetch: fatal: filtering not recognized by server"
                       "git fetch: fatal: filter blob:none not supported"))
        (let ((kept (fetching words)))
          (is eq :kept (first kept) "~a" words)
          (true (probe-file (second kept))))))))

(defun structure-package-version (dir version code)
  "Commit to the git repository DIR, made when it is not one yet, a version of
the package structpkg, VERSION its name, which sets *PACKAGE-TEST-VERSION* to
VERSION and then runs CODE, text such as \"(defstruct box a b)\", in its own
Lisp package structpkg. Returns the commit."
  (flet ((git (&rest arguments) (apply #'git-output dir arguments)))
    (unless (probe-file (merge-pathnames ".git/" dir))
      (git "init" "-q" "-b" "main")
      (git "config" "user.name" "Test")
      (git "config" "user.email" "test@example.invalid")
      (git "config" "commit.gpgsign" "false")
      (git "config" "uploadpack.allowFilter" "true"))
    (alexandria:write-string-into-file
     (format nil "(defpackage #:structpkg (:use #:cl))~%(in-package #:structpkg)~%~
                  (defparameter tuatara-user::*package-test-version* ~s)~%~a~%"
             version code)
     (ensure-directories-exist (merge-pathnames "structpkg/structpkg.lisp" dir)) :if-exists :supersede)
    (git "add" ".")
    (git "commit" "-q" "-m" version)
    (string-trim '(#\Newline) (git "rev-parse" "HEAD"))))

(defmacro with-structure-package ((dir where) &body body)
  "Run BODY with DIR a new directory for STRUCTURE-PACKAGE-VERSION and WHERE its
file:// address, nothing asked before loading; the package structpkg and all
that was kept of it forgotten afterwards."
  `(with-test-directory (,dir)
     (let ((,where (format nil "file://~a" (string-right-trim "/" (namestring ,dir))))
           (tuatara.defaults::*package-sources* '())
           (tuatara-user:*package-ask-before-loading* nil)
           (*until-bound* *package-until-bound*))
       (unwind-protect (progn ,@body)
         (when (find-package "STRUCTPKG")
           (tuatara.packages::forget-package-code (list (find-package "STRUCTPKG"))))
         (setf tuatara.packages::*loaded-package-directories*
               (remove "structpkg" tuatara.packages::*loaded-package-directories* :key #'first :test #'string=))
         (setf tuatara.packages::*packages-mixed* '())
         (tuatara.defaults::state-list-drop tuatara.packages::*package-trust* (list ,where))
         (forget-test-packages ,dir)))))

(defun structpkg-call (name &rest arguments)
  "Call the function NAME of the Lisp package structpkg with ARGUMENTS."
  (apply (find-symbol name "STRUCTPKG") arguments))

(defun forget-structpkg-as-at-a-start ()
  "Take the package structpkg out of this session as a new start would have
it: its code, its version's setting and that it was loaded."
  (when (find-package "STRUCTPKG")
    (tuatara.packages::forget-package-code (list (find-package "STRUCTPKG"))))
  (makunbound 'tuatara-user::*package-test-version*)
  (setf tuatara.packages::*loaded-packages* (remove "structpkg" tuatara.packages::*loaded-packages* :test #'string=)
        tuatara.packages::*package-failures* '()
        tuatara.packages::*packages-mixed* '()
        tuatara.packages::*loaded-package-directories*
        (remove "structpkg" tuatara.packages::*loaded-package-directories* :key #'first :test #'string=)))

(defun package-messages-said (text)
  "True once no package is being fetched, checked or loaded any more and a
message said TEXT since the step before began (SAID-IN-SESSION)."
  (and (packages-settled-p) (said-in-session text)))

(define-test (packages new-structures-load-at-the-next-start)
  ;; a new version laying out a structure otherwise is not loaded over the one running: it is
  ;; kept as the version taken, said to need a restart, said so again by the next update rather
  ;; than "up to date", and loaded at the next start
  (with-structure-package (dir where)
    (let ((one (structure-package-version dir "one" "(defstruct box a b)"))
          (two nil) (box nil))
      (let ((f (auto-save-session
                (list (list :until (lambda ()
                                     (tuatara-user:load-package "structpkg" :source where :version "latest")
                                     t))
                      (list :until (lambda () (equal "one" (package-test-version))))
                      (list :until (lambda ()
                                     (setf box (structpkg-call "MAKE-BOX" :a 1)
                                           two (structure-package-version dir "two" "(defstruct box a b c)"))))
                      "M-x p a c k a g e - u p d a t e RET"
                      (list :until (lambda () (package-messages-said "restart Tuatara")))
                      "M-x p a c k a g e - u p d a t e RET"
                      (list :until (lambda () (package-messages-said "restart Tuatara"))))
                :width 200)))
        (is equal "one" (package-test-version))
        (is = 1 (structpkg-call "BOX-A" box) "what the running version made still works")
        (true (tuatara.frontend.fake:fake-frame-showing
               f (format nil "The package structpkg ~a is fetched; restart Tuatara to use it" (subseq two 0 7))))
        (false (tuatara.frontend.fake:fake-frame-showing f "could not be loaded"))
        (false (tuatara.frontend.fake:fake-frame-showing f "is up to date"))
        (let ((entry (tuatara.packages::package-state "structpkg" where "latest")))
          (is equal two (second entry) "the version taken is remembered")
          (true (fourth entry))
          (is equal one (fifth entry)))
        (true (probe-file (tuatara.packages::package-directory-for where "structpkg" two)) "its files are kept")
        (true (probe-file (tuatara.packages::package-directory-for where "structpkg" one)))
        (is string= "restart" (tuatara.packages::declared-package-condition "structpkg")))
      ;; the next start: nothing of it loaded yet, it loads the version taken
      (forget-structpkg-as-at-a-start)
      (tuatara-user:load-package "structpkg" :source where :version "latest")
      (is equal "two" (package-test-version))
      (is = 3 (structpkg-call "BOX-C" (structpkg-call "MAKE-BOX" :c 3))))))

(define-test (packages same-structures-loaded-at-once)
  ;; a new version whose structures are laid out as before is loaded over the one running
  (with-structure-package (dir where)
    (structure-package-version dir "one" "(defstruct box a b)")
    (let ((f (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "structpkg" :source where :version "latest") t))
                    (list :until (lambda () (equal "one" (package-test-version))))
                    (list :until (lambda () (structure-package-version dir "two" "(defstruct box a b)")))
                    "M-x p a c k a g e - u p d a t e RET"
                    (list :until (lambda () (equal "two" (package-test-version)))))
              :width 200)))
      (false (tuatara.frontend.fake:fake-frame-showing f "restart Tuatara"))
      (is string= "loaded" (tuatara.packages::declared-package-condition "structpkg")))))

(define-test (packages check-named-as-a-check)
  ;; a check of who signed a version kept says so in the list of jobs and the log, never "Fetching"
  (let ((check (tuatara.packages::make-package-fetch "kept" "https://example.invalid/kept" nil nil nil))
        (fetch (tuatara.packages::make-package-fetch "new" "https://example.invalid/new" nil nil nil)))
    (setf (tuatara.packages::package-fetch-checking check) t)
    (is string= "Checking who signed the package kept" (tuatara.runtime:job-description check))
    (is string= "Fetching the package new" (tuatara.runtime:job-description fetch))))

(defun structpkg-updated (dir where one two)
  "Run a session that loads structpkg from WHERE, ONE the code of its first
version, then commits TWO, the code of the next, to DIR and runs
\\[package-update], until the echo row says the update ended: loaded, to
restart, or fallen back. Returns the fake frame and the two commits."
  (let* ((first (structure-package-version dir "one" one)) (second nil)
         (f (auto-save-session
             (list (list :until (lambda ()
                                  (tuatara-user:load-package "structpkg" :source where :version "latest")
                                  t))
                   (list :until (lambda () (equal "one" (package-test-version))))
                   (list :until (lambda () (setf second (structure-package-version dir "two" two))))
                   "M-x p a c k a g e - u p d a t e RET"
                   (list :until (lambda () (or (package-messages-said "restart Tuatara")
                                               (package-messages-said "is loaded")
                                               (package-messages-said "could not be loaded")))))
             :width 200)))
    (values f first second)))

(define-test (packages retyped-structure-loads-at-the-next-start)
  ;; a slot's type changed alone, the slots and their kinds as before, is a new layout too
  (with-structure-package (dir where)
    (multiple-value-bind (f one two)
        (structpkg-updated dir where "(defstruct box a (b 0 :type fixnum))" "(defstruct box a (b 0 :type integer))")
      (declare (ignore one))
      (is equal "one" (package-test-version))
      (true (tuatara.frontend.fake:fake-frame-showing
             f (format nil "The package structpkg ~a is fetched; restart Tuatara to use it" (subseq two 0 7))))
      (false (tuatara.frontend.fake:fake-frame-showing f "could not be loaded")))))

(define-test (packages broken-version-falls-back)
  ;; a new version that cannot be compiled, defining no structure, gives way to the version before it
  (with-structure-package (dir where)
    (multiple-value-bind (f one two)
        (structpkg-updated dir where "(defvar *works* t)" "(defun broken (")
      (declare (ignore two))
      (declare (ignore f))
      (true (said-in-session (format nil "the version before it, ~a, is loaded in its place" (subseq one 0 7))))
      (is equal one (second (tuatara.packages::package-state "structpkg" where "latest"))))))

(define-test (packages broken-structures-fall-back-at-the-next-start)
  ;; a new version that cannot even be compiled apart, of a package defining structures, waits for
  ;; a restart, as any of them may have changed; failing then, it gives way to the version before it
  (with-structure-package (dir where)
    (multiple-value-bind (f one two)
        (structpkg-updated dir where "(defstruct box a b)"
                           "(defstruct box a b) (error \"The second version is broken.\")")
      (true (tuatara.frontend.fake:fake-frame-showing
             f (format nil "The package structpkg ~a is fetched; restart Tuatara to use it" (subseq two 0 7))))
      (is equal two (second (tuatara.packages::package-state "structpkg" where "latest")))
      (let ((f (auto-save-session
                (list (list :until (lambda ()
                                     (forget-structpkg-as-at-a-start)
                                     (tuatara-user:load-package "structpkg" :source where :version "latest")
                                     t))
                      (list :until (lambda () (equal "one" (package-test-version))))))))
        (declare (ignore f))
        (true (said-in-session "could not be loaded"))
        (is equal one (second (tuatara.packages::package-state "structpkg" where "latest")))))))

(define-test (packages restart-version-refused-at-the-next-start)
  ;; a version waiting for a restart whose signer the trust in force no longer takes is refused
  ;; at the next start, not loaded unchecked
  (with-structure-package (dir where)
    (multiple-value-bind (f one two) (structpkg-updated dir where "(defstruct box a b)" "(defstruct box a b c)")
      (declare (ignore one two))
      (true (tuatara.frontend.fake:fake-frame-showing f "restart Tuatara"))
      (tuatara.defaults::state-list-put tuatara.packages::*package-trust*
                                        (list (list where "0123456789ABCDEF0123456789ABCDEF01234567" "Someone" nil)))
      (let ((f (auto-save-session
                (list (list :until (lambda ()
                                     (forget-structpkg-as-at-a-start)
                                     (tuatara-user:load-package "structpkg" :source where :version "latest")
                                     t))
                      (list :until (lambda () (package-messages-said "is refused")))))))
        (declare (ignore f))
        (true (said-in-session "is refused"))
        (false (package-test-version))))))

(define-test (packages structures-file-read-as-data)
  ;; what a package's code may write beside its compiling is read as JSON of strings and integers
  ;; alone: Lisp in it is no list of structures, and no symbol of it is interned
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "compile-x.request.structures" dir))))
      (alexandria:write-string-into-file "((\"STRUCTPKG\" \"BOX\" (tuatara-sneaky-name)))" file)
      (is eq :unknown (tuatara.packages::read-structures-file file))
      (false (find-symbol "TUATARA-SNEAKY-NAME" :cl-user))
      (alexandria:write-string-into-file "[[\"STRUCTPKG\", \"BOX\", [2, 1, -1, [\"T\"], []]]]" file
                                         :if-exists :supersede)
      (is equal '(("STRUCTPKG" "BOX" (2 1 -1 ("T") ()))) (tuatara.packages::read-structures-file file))
      (alexandria:write-string-into-file "[[\"STRUCTPKG\", \"BOX\", [2.5, true]]]" file :if-exists :supersede)
      (is eq :unknown (tuatara.packages::read-structures-file file)))))

(define-test (packages failed-part-way-keeps-the-running-code)
  ;; a new version failing part of the way over the one running takes none of the running code
  ;; out: the session runs parts of both, you are told to restart, the package manager says
  ;; restart, the version before is the one taken, and a later update never says up to date
  (with-structure-package (dir where)
    (multiple-value-bind (f one two)
        (structpkg-updated dir where "(defun answer () 1)"
                           "(defun answer () 2) (error \"The second version fails part of the way.\")")
      (declare (ignore f two))
      (true (said-in-session "runs parts of two versions, so restart Tuatara"))
      (is = 2 (structpkg-call "ANSWER") "the running code is still there")
      (is string= "restart" (tuatara.packages::declared-package-condition "structpkg"))
      (is equal one (second (tuatara.packages::package-state "structpkg" where "latest")))
      (auto-save-session
       (list "M-x p a c k a g e - u p d a t e RET"
             (list :until (lambda () (package-messages-said "restart Tuatara"))))
       :width 200)
      (false (said-in-session "up to date")))))
