;;;; test/packages.lisp --- Packages from a git repository or a folder: fetched, asked about, compiled, listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test packages
  :description "load-package with :source fetches the version asked for (the newest tag by date, latest, or one named) of a repository, the package's folder alone, asks before compiling it unless told not to, keeps it for the next start; a folder of your disk is a source too; the package manager lists, previews, updates and deletes them.")

(defun package-repository (dir)
  "Make DIR a git repository of two packages, pkgone/ (with a README) and
pkgtwo/: each version sets *PACKAGE-TEST-VERSION* to its name. Tag zeta is made first and
alpha later, then a commit no tag names, \"newest\"."
  (labels ((git (date &rest arguments)
             (uiop:run-program (list* "env" (format nil "GIT_COMMITTER_DATE=~a" date) (format nil "GIT_AUTHOR_DATE=~a" date)
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
them and compiled, the session's lists of them and their versions' setting."
  (let ((where (format nil "file://~a" (string-right-trim "/" (namestring dir)))))
    (tuatara.defaults::state-list-drop tuatara.defaults::*fetched-packages*
                                       (remove-if-not (lambda (key) (search (namestring dir) key))
                                                      (mapcar #'first (tuatara.defaults::state-list-entries tuatara.defaults::*fetched-packages*))))
    (let ((kept (format nil "~a~a/" (tuatara.defaults::packages-directory) (tuatara.io:percent-encoded where :safe "._-"))))
      (dolist (folder (list kept (directory-namestring (tuatara.defaults::package-fasl (merge-pathnames "x" kept)))))
        (when (probe-file folder)
          (uiop:delete-directory-tree (pathname folder) :validate (lambda (p) (search "/tuatara/packages/" (namestring p)))))))
    (let ((fasls (directory-namestring (tuatara.defaults::package-fasl (merge-pathnames "x" dir)))))
      (when (probe-file fasls)
        (uiop:delete-directory-tree (pathname fasls) :validate (lambda (p) (search "/tuatara/packages/" (namestring p)))))))
  (setf tuatara.defaults::*declared-packages* '()
        tuatara.defaults::*loaded-packages* '()
        tuatara.defaults::*package-failures* '()
        tuatara.defaults::*refused-packages* '())
  (makunbound 'tuatara-user::*package-test-version*)
  (makunbound 'tuatara-user::*package-test-two*)
  (makunbound 'tuatara-user::*package-test-config*))

(defmacro with-package-repository ((dir where) &body body)
  "Run BODY with DIR a new repository of packages (PACKAGE-REPOSITORY) and
WHERE its file:// address, all of it forgotten afterwards."
  `(with-test-directory (,dir)
     (package-repository ,dir)
     (let ((,where (format nil "file://~a" (string-right-trim "/" (namestring ,dir))))
           (tuatara.defaults::*package-sources* '()))
       (unwind-protect (progn ,@body)
         (forget-test-packages ,dir)))))

(defun package-test-version ()
  "The version of pkgone loaded, or NIL."
  (and (boundp 'tuatara-user::*package-test-version*) (symbol-value 'tuatara-user::*package-test-version*)))

(define-test (packages newest-tag-by-date)
  ;; no version asked for: the tag made last, though its name sorts first; only its folder kept
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) folder)
      (auto-save-session (list (list :until (lambda () (tuatara-user:package-source "test" where) t))
                               (list :until (lambda () (tuatara-user:load-package "pkgone" :source "test"
                                                                                  :config
                                                                                  (setf tuatara-user::*package-test-config* (list tuatara-user::*package-test-version*)))
                                              t))
                               (list :until (lambda () (package-test-version)))
                               (list :until (lambda () (setf folder (nth-value 1 (tuatara.defaults::package-state "pkgone" "test" nil)))))))
      (is equal "alpha" (package-test-version))
      ;; its :config ran once it had loaded, in the background
      (is equal '("alpha") (symbol-value 'tuatara-user::*package-test-config*))
      (true (search "/pkgone/" folder))
      (true (probe-file (merge-pathnames "pkgone.lisp" folder)))
      (false (probe-file (merge-pathnames "../pkgtwo/" folder)))
      (is equal "alpha" (third (tuatara.defaults::package-state "pkgone" "test" nil)))
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
                      (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                      "n RET" (list :wait 0.2))
                :width 160)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Compile and load the package pkgone from"))
        (true (tuatara.frontend.fake:fake-frame-showing f "The package pkgone is not loaded"))
        (false (package-test-version))
        (false (fourth (tuatara.defaults::package-state "pkgone" where nil)))
        (auto-save-session
         (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
               (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
               "y RET" (list :until (lambda () (package-test-version)))))
        (is equal "alpha" (package-test-version))
        (true (fourth (tuatara.defaults::package-state "pkgone" where nil)))))))

(define-test (packages up-to-date-and-managed)
  ;; two packages of one commit each kept apart; package-update finds the same tag; the
  ;; package manager lists them, the README of the one on the line beside it, and deletes
  ;; what is kept of the one on the line
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil)
          (commit (string-trim '(#\Newline) (git-output dir "rev-parse" "alpha^{commit}")))
          (screens '()))
      (flet ((screen (f) (push (format nil "~{~a~%~}" (loop for row below 24 collect (tuatara.frontend.fake:fake-screen-line f row))) screens)))
        (let ((f (auto-save-session
                  (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
                        (list :until (lambda () (tuatara-user:load-package "pkgtwo" :source where) t))
                        (list :until (lambda () (and (package-test-version) (boundp 'tuatara-user::*package-test-two*))))
                        "M-x p a c k a g e - u p d a t e RET"
                        (list :until (lambda () (null tuatara.defaults::*fetching-packages*)))
                        "M-x p a c k a g e - m a n a g e r RET" (list :wait 0.2)
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t))
                        "C-n" (list :wait 0.2)
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t))
                        "C-p D" (list :wait 0.2)
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t))
                        "y e s RET"
                        (list :until (lambda () (search "deleted" (or (tuatara.command:editor-message tuatara.command:*editor*) ""))))
                        (list :wait 0.2)
                        (list :until (lambda () (screen (tuatara.command:editor-frontend tuatara.command:*editor*)) t)))
                  :width 200 :height 24)))
          (declare (ignore f))
          (destructuring-bind (after question second first) screens
            ;; the README beside the list goes with what was deleted, the entry on the line shown anew
            (false (search "Reads alpha." after))
            (false (search "deleted on disk" after))
            (true (search "pkgone is not fetched yet" after) "~a" after)
            ;; what is deleted asked plainly, Tuatara's copy of it
            (true (search "Delete what Tuatara keeps of the package pkgone?" question))
            (true (search "pkgone" first))
            (true (search "alpha" first))
            (true (search (subseq commit 0 7) first))
            (true (search "loaded" first))
            (true (search "2 packages, 2 loaded" first))
            (true (search "Reads alpha." first))
            (true (search "has no README" second)))
          (is equal "alpha" (symbol-value 'tuatara-user::*package-test-two*))
          (false (probe-file (tuatara.defaults::package-kept-folder where "pkgone")))
          (true (probe-file (tuatara.defaults::package-kept-folder where "pkgtwo")))
          (false (tuatara.defaults::package-state "pkgone" where nil)))))))

(define-test (packages compile-failure-said)
  ;; a file that cannot be read says why, not only that it could not be compiled
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "(defun broken ()~%  (list 1 2~%") (merge-pathnames "broken.lisp" dir))
    (let* ((tuatara.defaults::*package-folder* (pathname dir))
           (said (handler-case (progn (tuatara-user:load-package-file "broken.lisp") nil)
                   (error (e) (princ-to-string e)))))
      (true (search "could not be compiled: " said))
      (true (search "end of file" (string-downcase said))))))

(define-test (packages folder-source)
  ;; a folder of your disk: asked about once, then loaded at once; :file names its entry
  (with-test-directory (dir)
    (alexandria:write-string-into-file "(defparameter *package-test-version* \"mine\")" (merge-pathnames "start.lisp" dir))
    (let ((tuatara-user:*package-ask-before-loading* t)
          (tuatara.defaults::*package-sources* '()))
      (unwind-protect
           (progn
             ;; v lists its folder in rimu, and nothing is loaded
             (let ((f (auto-save-session
              (list (list :until (lambda () (tuatara-user:package-source "mine" (namestring dir)) t))
                    (list :until (lambda () (tuatara-user:load-package "mypkg" :source "mine" :file "start.lisp") t))
                    (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                    "v RET" (list :until (lambda () (search "is shown" (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                       :width 120)))
               (true (tuatara.frontend.fake:fake-frame-showing f "start.lisp")))
             (false (package-test-version))
             (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "mypkg" :source "mine" :file "start.lisp") t))
                    (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                    "y RET" (list :until (lambda () (package-test-version)))))
             (is equal "mine" (package-test-version))
             (makunbound 'tuatara-user::*package-test-version*)
             (true (tuatara-user:load-package "mypkg" :source "mine" :file "start.lisp"))
             (is equal "mine" (package-test-version))
             ;; deleted: its files compiled go too, the folder of yours stays as it is
             (let ((fasl (tuatara.defaults::package-fasl (merge-pathnames "start.lisp" dir))))
               (true (probe-file fasl))
               (tuatara.defaults::forget-package "mypkg" "mine" nil)
               (false (probe-file fasl))
               (true (probe-file (merge-pathnames "start.lisp" dir)))))
        (forget-test-packages dir)))))

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
           (multiple-value-bind (results stopped) (tuatara.defaults::scan-init-file-packages init)
             ;; pkga swallowed pkgb: pkga refused saying so, pkgb found inside it and loaded by itself
             (is equal '("pkga" "pkgb") (mapcar #'first results))
             (true (search "pkgb\" is inside this one" (fourth (first results))))
             (is equal '("pkgb" 4 t "loaded") (second results))
             (true (boundp 'tuatara-user::*package-test-pkgb*))
             ;; the group of settings was not run, nor anything else of the file
             (false (boundp 'tuatara-user::*package-test-config*))
             ;; the stray ) stops the reading: pkgc, after it, is not seen
             (is = 9 (car stopped))
             (false (tuatara.defaults::declared-package "pkgc")))
        (forget-test-packages dir))
      ;; what decides whether a load-package runs is kept to: a guarded one, or one a group loads, is not looked at
      (alexandria:write-string-into-file
       (format nil "(when nil (load-package \"pkga\" :source ~s))~%(load-package \"tuatara\" :config (load-package \"pkgb\" :source ~s))~%"
               (namestring dir) (namestring dir))
       init :if-exists :supersede)
      (unwind-protect
           (is equal '() (tuatara.defaults::scan-init-file-packages init))
        (forget-test-packages dir)
        (makunbound 'tuatara-user::*package-test-pkga*)
        (makunbound 'tuatara-user::*package-test-pkgb*)))))

(define-test (packages folder-not-there)
  ;; a folder of your disk that is not there: said at once, nothing asked
  (with-test-directory (dir)
    (let ((tuatara-user:*package-ask-before-loading* t)
          (tuatara.defaults::*package-sources* '())
          (gone (namestring (merge-pathnames "gone/" dir))))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "ghost" :source gone) t))
                      (list :until (lambda () (search "is not there" (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                :width 120)))
        (true (tuatara.frontend.fake:fake-frame-showing f "The package ghost is not loaded: its folder"))
        (false (tuatara.frontend.fake:fake-frame-showing f "Compile and load"))))))

(define-test (packages fetch-size-bounded)
  ;; a repository growing past the most a package may take is ended and deleted, not loaded
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil)
          (most tuatara.defaults::*package-most-octets*))
      (setf tuatara.defaults::*package-most-octets* 1000)
      (unwind-protect
           (let ((f (auto-save-session
                     (list (list :until (lambda () (tuatara-user:load-package "pkgone" :source where) t))
                           (list :until (lambda () (search "takes more than" (or (tuatara.command:editor-message tuatara.command:*editor*) "")))))
                     :width 200)))
             (true (tuatara.frontend.fake:fake-frame-showing f "takes more than 1.0 kB: the package was not fetched"))
             (false (package-test-version))
             (false (directory (merge-pathnames ".fetching-*/" (tuatara.defaults::packages-directory)))))
        (setf tuatara.defaults::*package-most-octets* most)))))

(define-test (packages arguments-checked)
  ;; what could be taken for one of git's options, or a file outside the package's folder
  (fail (tuatara-user:load-package "pkgone" :source "--upload-pack=touch"))
  (fail (tuatara-user:package-source "bad" "--upload-pack=touch"))
  (fail (tuatara-user:load-package "pkgone" :source "/tmp/" :file "../x.lisp"))
  (fail (tuatara-user:load-package "pkgone" :source "/tmp/" :version "a b"))
  (fail (tuatara-user:load-package "../pkgone" :source "/tmp/"))
  ;; a parenthesis left open: the load-packages after it are no values of :file, said, never dropped
  (flet ((why (options) (handler-case (progn (tuatara.defaults::load-package-options options) nil)
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
         (false (tuatara-user:load-package "writing" :disabled t :config (setf tuatara-user::*package-test-config* :disabled)))
         (false (tuatara-user:load-package "writing" :if (= 1 2) :config (setf tuatara-user::*package-test-config* :if-false)))
         (is eq :before (symbol-value 'tuatara-user::*package-test-config*))
         (true (search "writing is left out (:disabled)" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
         (tuatara-user:load-package "writing" :if (= 1 1) :config (setf tuatara-user::*package-test-config* :if-true))
         (is eq :if-true (symbol-value 'tuatara-user::*package-test-config*)))
    (makunbound 'tuatara-user::*package-test-config*)))

(define-test (packages folder-of-another-name)
  ;; :folder names the package's folder in the repository when it has another name
  (with-package-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) folder)
      (auto-save-session (list (list :until (lambda () (tuatara-user:load-package "second" :source where :folder "pkgtwo") t))
                               (list :until (lambda () (boundp 'tuatara-user::*package-test-two*)))
                               (list :until (lambda () (setf folder (nth-value 1 (tuatara.defaults::package-state "second" where nil)))))))
      (is equal "alpha" (symbol-value 'tuatara-user::*package-test-two*))
      (true (search "/pkgtwo/" folder))
      (fail (tuatara-user:load-package "second" :source where :folder "../pkgtwo")))))

(defun folder-packages (dir &rest packages)
  "Make in DIR a folder of each of PACKAGES, (NAME TEXT), NAME/NAME.lisp
holding TEXT, for a source that is a folder of the disk."
  (loop for (name text) in packages
        do (alexandria:write-string-into-file text (ensure-directories-exist (merge-pathnames (format nil "~a/~a.lisp" name name) dir))
                                              :if-exists :supersede)))

(defmacro with-folder-packages ((dir) &body body)
  "Run BODY with DIR a new folder for FOLDER-PACKAGES, nothing asked before
loading, and what the packages left forgotten afterwards."
  `(with-test-directory (,dir)
     (let ((tuatara-user:*package-ask-before-loading* nil)
           (tuatara.defaults::*package-sources* '()))
       (unwind-protect (progn ,@body)
         (forget-test-packages ,dir)
         (setf tuatara.defaults::*deferred-packages* '() tuatara.defaults::*packages-waiting* '())
         (dolist (symbol '(tuatara-user::*package-test-order* tuatara-user::*package-test-said*))
           (makunbound symbol))))))

(define-test (packages init-after-and-failures)
  ;; :init before the package loads; :after waits for the other one; a package that fails is
  ;; said and the next still loads; a :config form that fails leaves the others to run
  (with-folder-packages (dir)
    (folder-packages dir
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
      (true (find "second" tuatara.defaults::*packages-waiting* :key #'second :test #'string=))
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
      (folder-packages dir '("third" "(push :third *package-test-order*)"))
      (tuatara-user:load-package "third" :source where)
      (is equal '(:writing :third :late-group :early-group) (symbol-value 'tuatara-user::*package-test-order*))
      (false (tuatara-user:load-package "broken" :source where))
      (false (tuatara.defaults::package-loaded-p "broken"))
      (true (assoc "broken" tuatara.defaults::*package-failures* :test #'string=))
      (tuatara-user:load-package "tuatara" :config (error "one bad line") (setf tuatara-user::*package-test-config* :went-on))
      (is eq :went-on (symbol-value 'tuatara-user::*package-test-config*))
      (fail (macroexpand '(tuatara-user:load-package "x" :no-such-keyword 1))))))

(define-test (packages loaded-later)
  ;; :defer t loads once the editor has started; :commands when one of them is first run;
  ;; :modes when a buffer is first in one of them
  (with-folder-packages (dir)
    (folder-packages dir
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
      (true (tuatara.defaults::package-loaded-p "bycommand"))
      ;; the command's name once in M-x, though the package's and the stub's are two
      (is = 1 (count "package-test-hello" (tuatara.command:command-names) :test #'string=)))))

(define-test (packages said-at-start)
  ;; the init file's packages said in *Messages* with their times, and how many at the end;
  ;; each listed in the package manager with its folder, and compiled into the cache
  (with-folder-packages (dir)
    (folder-packages dir '("quick" "(defparameter *package-test-said* :quick)")
                     '("second" "(defparameter *package-test-said-second* :second)"))
    (let ((init (merge-pathnames "init.lisp" dir)))
      (alexandria:write-string-into-file
       (format nil "(setf *package-ask-before-loading* nil)~%(load-package ~s :source ~s)~%(load-package ~s :source ~s)~%"
               "quick" (namestring dir) "second" (namestring dir))
       init)
      (let ((tuatara.defaults:*config-file* init))
        (multiple-value-bind (f editor) (run-keys "M-x p a c k a g e - m a n a g e r RET" :text "" :width 160 :height 20)
          (declare (ignore editor))
          (let ((messages (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
            (true (cl-ppcre:scan "Loading package quick from .*\\.\\.\\.done \\(\\d+ ms\\)" messages))
            (true (cl-ppcre:scan "Loading package second from .*\\.\\.\\.done \\(\\d+ ms\\)" messages))
            (true (cl-ppcre:scan "Packages: 2 loaded in \\d+ ms" messages)))
          (true (probe-file (tuatara.defaults::package-fasl (merge-pathnames "second/second.lisp" dir))))
          (true (tuatara.frontend.fake:fake-frame-showing f "second")))))))

(define-test (packages folder-in-rimu)
  ;; RET lists a package's folder in rimu, opened safely, the folder its top: h there
  ;; comes back to the package manager, and its files are previewed
  (with-folder-packages (dir)
    (folder-packages dir '("quick" "(defparameter *package-test-said* :quick)"))
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
              (run-keys (list "M-x p a c k a g e - m a n a g e r RET C-s q u i c k RET RET" (note :folder) :tick :tick "h" (note :back))
                        :text "" :width 160 :height 20)
            (declare (ignore f editor))
            (let ((folder (cdr (assoc :folder seen))) (back (cdr (assoc :back seen))))
              (true (tuatara.defaults::rimu-list-p folder))
              (true (tuatara.io:safe-visit-p folder))
              (true (tuatara.defaults::rimu-previewing-p folder))
              (is equal (tuatara.buffer:buffer-local folder 'tuatara.defaults::rimu-directory)
                  (tuatara.buffer:buffer-local folder 'tuatara.defaults::rimu-root))
              (true (typep (tuatara.defaults::buffer-major-mode back) 'tuatara.defaults::packages-mode)))))))))

(define-test (packages compiled-for-one-tuatara)
  ;; compiled packages are kept for the Tuatara that compiled them: another one's are deleted
  (let* ((other (format nil "~aother-tuatara-~36r/"
                       (directory-namestring (string-right-trim "/" (tuatara.defaults::package-cache))) (random (expt 36 6))))
         (tuatara.defaults::*tuatara-identity* (format nil "test-~36r" (random (expt 36 8) (make-random-state t)))))
    (alexandria:write-string-into-file "x" (ensure-directories-exist (merge-pathnames "kept.fasl" other)))
    (let ((here (tuatara.defaults::package-cache)))
      (true (search tuatara.defaults::*tuatara-identity* here))
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
                                                             tuatara.defaults::*packages-columns* tuatara.defaults::*packages-source-width*))))
                                      (tuatara.defaults::side-preview-show preview readme nil)
                                      (setf seen (tuatara.display:window-width window)))))
                            :width width :height 30)
               (tuatara.buffer:kill-buffer readme))
             seen)))
    (is = 100 (list-width 100))
    (true (< (list-width 200) 200))))

(defun aged-package (dir name text)
  "Make the folder package NAME in DIR holding TEXT (FOLDER-PACKAGES), its
file dated a minute ago, so that what is kept of it later is newer."
  (folder-packages dir (list name text))
  (let ((file (namestring (merge-pathnames (format nil "~a/~a.lisp" name name) dir)))
        (then (- (sb-posix:time) 60)))
    (sb-posix:utimes file then then)))

(defun package-failure (name)
  "Why the package NAME was not loaded this session, or NIL."
  (cdr (assoc name tuatara.defaults::*package-failures* :test #'string=)))

(defun forget-failure (name)
  "Forget that the package NAME failed or was refused this session, as package-scan does
before loading it again."
  (setf tuatara.defaults::*package-failures* (remove name tuatara.defaults::*package-failures* :key #'car :test #'string=)
        tuatara.defaults::*refused-packages* (remove name tuatara.defaults::*refused-packages* :test #'string=)))

(define-test (packages names-tuatara-lacks-refused)
  ;; a package calling functions Tuatara lacks is refused before its file runs, naming them,
  ;; and nothing of it is left: not its Lisp package, not its command
  (with-folder-packages (dir)
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
           (false (tuatara.defaults::package-loaded-p "lacker"))
           (is string= "lacker needs names this Tuatara lacks: tuatara-user::no-such-function, tuatara.defaults::no-such-thing; update the package (M-x package-update) or Tuatara"
               (package-failure "lacker"))
           (is equal "refused" (tuatara.defaults::package-condition "lacker" nil))
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
  (with-folder-packages (dir)
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
                   (find "RENAMER-VIEW" tuatara:*file-views* :key (lambda (view) (and (symbolp view) (symbol-name view)))
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
        (folder-packages dir (list "renamer" (concatenate 'string text "
(defun docx-file-octets (path) path)")))
        (true (tuatara-user:load-package "renamer" :source (namestring dir)))
        (true (tuatara.defaults::package-loaded-p "renamer"))
        (is equal '(:ran :ran) (symbol-value 'tuatara-user::*package-test-order*))
        (tuatara.defaults::forget-package-code (list (find-package "RENAMER")))
        (false (left-p))))))

(define-test (packages variable-lacking-refused)
  ;; a variable it reads that is nowhere is refused as a function is, Tuatara's or its own
  (with-folder-packages (dir)
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

(define-test (packages api-names-loaded)
  ;; a package using only the API, and a name of another library, loads; loaded again from
  ;; what was compiled, what it needs is looked for again: gone, it is refused before it runs
  (with-folder-packages (dir)
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
           (true (tuatara.defaults::package-loaded-p "apiuser"))
           (false (package-failure "apiuser"))
           (true (tuatara.command:find-command "apiuser-run"))
           (is equal '(:ran) (symbol-value 'tuatara-user::*package-test-order*))
           (flet ((unload ()
                    (setf tuatara.defaults::*loaded-packages* (remove "apiuser" tuatara.defaults::*loaded-packages* :test #'string=))
                    (tuatara.defaults::forget-package-code (list (find-package "APIUSER")))))
             (let ((fasl (tuatara.defaults::package-fasl (merge-pathnames "apiuser/apiuser.lisp" dir)))
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
        (tuatara.defaults::forget-package-code (list (find-package "APIUSER"))))
      (makunbound 'tuatara-user::*apiuser-test-value*))))

(define-test (packages probe-size-bounded)
  ;; finding which commit to fetch is held to the most a package may take as well: past it,
  ;; nothing is checked out
  (with-package-repository (dir where)
    (let ((most tuatara.defaults::*package-most-octets*)
          (folders '()))
      (setf tuatara.defaults::*package-most-octets* 1)
      (sb-int:encapsulate 'tuatara.defaults::package-git 'probe-size-bounded
                          (lambda (function directory &rest arguments)
                            (push directory folders)
                            (apply function directory arguments)))
      (unwind-protect
           (fail (tuatara.defaults::fetch-package "pkgone" where nil) 'tuatara.defaults::package-too-large)
        (sb-int:unencapsulate 'tuatara.defaults::package-git 'probe-size-bounded)
        (setf tuatara.defaults::*package-most-octets* most))
      (true (find-if (lambda (folder) (search "/probe/" folder)) folders))
      (false (find-if-not (lambda (folder) (search "/probe/" folder)) folders))
      (false (directory (merge-pathnames ".fetching-*/" (tuatara.defaults::packages-directory)))))))

(define-test (packages manager-help-shows-the-marks-shown)
  ;; the package manager's help gives the signs its list shows for a mark
  (let ((help (documentation 'tuatara.defaults::package-manager 'function)))
    (true (search (format nil "(~a marked, ~a marked for deletion)"
                          (tuatara.defaults::list-mark-sign :marked) (tuatara.defaults::list-mark-sign :deletion))
                  help))))

(define-test (packages api-runs-a-program-on-text)
  ;; a package splits a command line and runs a program of its own over text through a pipe,
  ;; with the API's names alone
  (with-folder-packages (dir)
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
        (tuatara.defaults::forget-package-code (list (find-package "FILTERUSER")))))))

(define-test (packages api-names-used)
  ;; informative: the names of the API no package of ours uses, said in the test's output, so
  ;; that they can be weighed; none is taken away for it, other packages may use them
  (let ((sources (find-if #'uiop:directory-exists-p
                          (list (merge-pathnames "../tuatara-packages/" (asdf:system-source-directory :tuatara))
                                (merge-pathnames "../../../../tuatara-packages/" (asdf:system-source-directory :tuatara))))))
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
  ;; package-remove-unused, in a packages folder of the test's own: a source nothing names goes,
  ;; asked once with sizes; a package named, one left out (:disabled), a fetch's scratch folder
  ;; and what a link points to stay; n deletes nothing
  (with-test-directory (dir)
    (let* ((top (namestring (ensure-directories-exist (merge-pathnames "packages/" dir))))
           (outside (ensure-directories-exist (merge-pathnames "outside/" dir)))
           (tuatara-user:*packages-directory* top)
           (tuatara.defaults::*declared-packages* (list (list "used" "https://example.org/used" nil nil nil)))
           (tuatara.defaults::*left-out-packages* '())
           (tuatara.defaults::*package-sources* '())
           (commit "0123456789abcdef0123456789abcdef01234567"))
      (flet ((kept (where name)
               (let ((file (merge-pathnames (format nil "~a/~a/~a/~a.lisp" (tuatara.io:path-file-name where) name commit name) top)))
                 (alexandria:write-string-into-file (make-string 2000 :initial-element #\;) (ensure-directories-exist file))
                 file)))
        (let ((gone (kept "https://example.org/gone" "gone"))
              (used (kept "https://example.org/used" "used"))
              (other (kept "https://example.org/used" "other"))
              (off (kept "https://example.org/off" "off"))
              (scratch (ensure-directories-exist (merge-pathnames ".fetching-abc/probe/" top))))
          (alexandria:write-string-into-file "theirs" (merge-pathnames "keep.txt" outside))
          (sb-posix:symlink (string-right-trim "/" (namestring outside)) (concatenate 'string top "https%3A%2F%2Fexample.org%2Flink"))
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
          (let ((pushed (push "used" tuatara.defaults::*fetching-packages*)))
            (declare (ignore pushed))
            (unwind-protect
                 (auto-save-session
                  (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
                        (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                        "y RET" (list :until (lambda () (echo-says "freed")))))
              (setf tuatara.defaults::*fetching-packages* (remove "used" tuatara.defaults::*fetching-packages* :test #'string=))))
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
          (tuatara.defaults::*declared-packages* '())
          (tuatara.defaults::*package-sources* '()))
      (let* ((said nil)
             (f (auto-save-session
                 (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
                       (list :until (lambda () (setf said (echo-says "No package source kept is unused")))))
                 :width 160)))
        (true said)
        (false (tuatara.frontend.fake:fake-frame-showing f "no longer named,"))))))

(defmacro with-unused-source ((top gone) &body body)
  "Run BODY with TOP a packages folder of a test directory's own and GONE the
entry file of a package of a source nothing names, kept in it."
  (let ((dir (gensym "DIR")))
    `(with-test-directory (,dir)
       (let* ((,top (namestring (ensure-directories-exist (merge-pathnames "packages/" ,dir))))
              (tuatara-user:*packages-directory* ,top)
              (tuatara.defaults::*declared-packages* '())
              (tuatara.defaults::*left-out-packages* '())
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
           (list :until (lambda () (push (list "gone" "https://example.org/gone" nil nil nil) tuatara.defaults::*declared-packages*)))
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
      (sb-int:encapsulate 'tuatara.defaults::unused-package-folders 'unused-sources-failure-said
                          (lambda (function) (declare (ignore function)) (error "the folder could not be read")))
      (unwind-protect
           (auto-save-session
            (list "M-x p a c k a g e - r e m o v e - u n u s e d RET"
                  (list :until (lambda () (setf said (echo-says "Looking for package sources no longer named failed")))))
            :width 200)
        (sb-int:unencapsulate 'tuatara.defaults::unused-package-folders 'unused-sources-failure-said))
      (true said)
      (true (probe-file gone)))))

(define-test (packages unused-sources-parent-link-left)
  ;; a source's folder swapped for a link before the deleting: what the link leads to stays
  (with-test-directory (dir)
    (let* ((top (namestring (ensure-directories-exist (merge-pathnames "packages/" dir))))
           (outside (namestring (ensure-directories-exist (merge-pathnames "outside/" dir))))
           (tuatara-user:*packages-directory* top)
           (tuatara.defaults::*declared-packages* (list (list "used" "https://example.org/mixed" nil nil nil)))
           (tuatara.defaults::*left-out-packages* '())
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
  (let ((tuatara.defaults::*packages-in-hand* (list (cons "pkgq" :deciding)))
        (tuatara.defaults::*declared-packages* '())
        (tuatara.defaults::*package-sources* '()))
    (let ((f (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "pkgq" :source "https://example.org/pkgq") t))
                    (list :wait 0.2))
              :width 200)))
      (false (tuatara.frontend.fake:fake-frame-showing f "already running")))))
