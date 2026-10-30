;;;; test/package-trust.lisp --- Packages kept to what this Tuatara supports, and to whom their source is trusted by.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test package-trust
  :description "A package's version that needs a newer Tuatara (;;;; Needs: Tuatara X in its entry's header) is never taken, the newest that fits in its place; one that fails to load gives way to the version before it; the version taken and the one before are all that is kept. Who signed a source's first version (SSH or GPG) is remembered: later versions by that key are taken, others refused until package-trust-again; unsigned sources stay usable; :signers names the only keys taken; http:// and git:// only with :plain t.")

(defvar *trust-gnupg-home* nil
  "The GnuPG folder TRUST-GIT signs with, when not the tests' own keyring.")

(defun trust-git (dir &rest arguments)
  "Run git with ARGUMENTS in DIR, a test repository, signing with the keys of
*TRUST-GNUPG-HOME* when it is set; what it printed."
  (uiop:run-program (append (and *trust-gnupg-home* (list "env" (format nil "GNUPGHOME=~a" *trust-gnupg-home*)))
                            (list* "git" "-C" (namestring dir) arguments))
                    :output :string :error-output :string :ignore-error-status t))

(defun trust-repository (dir)
  "Make DIR an empty git repository packages can be fetched from."
  (trust-git dir "init" "-q" "-b" "main")
  (trust-git dir "config" "uploadpack.allowFilter" "true")
  (trust-git dir "config" "tag.gpgsign" "false"))

(defun trust-version (dir name &key needs signer gpg tag (body (format nil "(defparameter *package-trust-version* ~s)~%" name)))
  "Commit to the repository DIR a version NAME of the package pkgv, which
sets *PACKAGE-TRUST-VERSION* to NAME (or holds BODY): NEEDS the Tuatara its
header says it needs; SIGNER an SSH key's file to sign it with, or GPG a GPG
key's ID; TAG a tag made for it, signed as it is. Returns its commit."
  (alexandria:write-string-into-file
   (format nil ";;;; pkgv.lisp --- A package for the tests.~%~@[;;;; Needs: Tuatara ~a~%~]~%~a" needs body)
   (ensure-directories-exist (merge-pathnames "pkgv/pkgv.lisp" dir)) :if-exists :supersede)
  (trust-git dir "add" ".")
  (let ((signing (cond (signer (list "-c" "gpg.format=ssh" "-c" (format nil "user.signingkey=~a" signer)))
                       (gpg (list "-c" (format nil "user.signingkey=~a" gpg))))))
    (apply #'trust-git dir (append signing (list "commit" "-q" "-m" name)
                                   (if (or signer gpg) (list "-S") (list "--no-gpg-sign"))))
    (when tag
      (apply #'trust-git dir (append signing (list "tag" (if (or signer gpg) "-s" "-a") tag "-m" tag)))))
  (string-trim '(#\Newline) (trust-git dir "rev-parse" "HEAD")))

(defun trust-ssh-key (dir name)
  "Make a throwaway SSH key NAME in DIR, with no passphrase: its private key's
file and its fingerprint (SHA256:...)."
  (let ((file (namestring (merge-pathnames name dir))))
    (uiop:run-program (list "ssh-keygen" "-q" "-t" "ed25519" "-N" "" "-C" name "-f" file))
    (values file (second (uiop:split-string (uiop:run-program (list "ssh-keygen" "-lf" (concatenate 'string file ".pub")) :output :string)
                                            :separator " ")))))

(defmacro with-trust-repository ((dir where) &body body)
  "Run BODY with DIR a new empty repository (TRUST-REPOSITORY) and WHERE its
file:// address, everything remembered of it forgotten afterwards."
  `(with-test-directory (,dir)
     (trust-repository ,dir)
     (let ((,where (format nil "file://~a" (string-right-trim "/" (namestring ,dir))))
           (tuatara.defaults::*package-sources* '())
           (tuatara.defaults::*package-signers* '())
           (tuatara.defaults::*package-plain-allowed* '())
           (tuatara.defaults::*loaded-package-folders* '()))
       (unwind-protect (progn ,@body)
         (tuatara.defaults::state-list-drop tuatara.defaults::*package-trust* (list ,where))
         (forget-test-packages ,dir)
         (makunbound 'tuatara-user::*package-trust-version*)))))

(defun trust-version-loaded ()
  "The version of pkgv loaded, or NIL."
  (and (boundp 'tuatara-user::*package-trust-version*) (symbol-value 'tuatara-user::*package-trust-version*)))

(defun echo-says (text)
  "True when the echo row says TEXT now."
  (search text (or (tuatara.command:editor-message tuatara.command:*editor*) "")))

(defun kept-versions (where)
  "The commits of pkgv from WHERE kept on disk."
  (remove-if-not (lambda (folder) (tuatara.defaults::git-hash-p (car (last (pathname-directory folder))) :short t))
                 (ignore-errors (uiop:subdirectories (tuatara.defaults::package-kept-folder where "pkgv")))))

(define-test (package-trust needs-read-from-the-header)
  (is equal "0.91.0" (tuatara.defaults::needed-version-in (format nil ";;;; a.lisp --- A.~%;;;; Needs: Tuatara 0.91.0~%(defpackage x)")))
  (is equal "1.2" (tuatara.defaults::needed-version-in (format nil ";;; Needs: tuatara 1.2 or later.~%")))
  ;; only the comments at the top: a line of code ends them
  (false (tuatara.defaults::needed-version-in (format nil "(defpackage x)~%;;;; Needs: Tuatara 9.0~%")))
  (false (tuatara.defaults::needed-version-in (format nil ";;;; a.lisp --- A.~%")))
  (true (tuatara.defaults::tuatara-fits-p "0.1"))
  (true (tuatara.defaults::tuatara-fits-p (tuatara:version)))
  (false (tuatara.defaults::tuatara-fits-p "9.0"))
  (false (tuatara.defaults::tuatara-fits-p (format nil "~a.1" (tuatara:version)))))

(define-test (package-trust newer-tuatara-passed-over)
  ;; the newest commits need a newer Tuatara: the newest that fits is taken; saying nothing fits
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (trust-version dir "one")
      (let ((two (trust-version dir "two" :needs "0.1.0")))
        (trust-version dir "three" :needs "9.0")
        (trust-version dir "four" :needs "9.0")
        (let ((f (auto-save-session
                  (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                        (list :until (lambda () (trust-version-loaded))))
                  :width 240)))
          (is equal "two" (trust-version-loaded))
          (true (tuatara.frontend.fake:fake-frame-showing f "needs Tuatara 9.0 or later"))
          (is equal two (second (tuatara.defaults::package-state "pkgv" where "latest")))
          (is = 1 (length (kept-versions where)))))))
  ;; the newest tag needs a newer one: the tag before it is taken
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (trust-version dir "old" :tag "v1")
      (sleep 1.1)
      (trust-version dir "new" :needs "9.0" :tag "v2")
      (auto-save-session
       (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where) t))
             (list :until (lambda () (trust-version-loaded)))))
      (is equal "old" (trust-version-loaded))
      (is equal "v1" (third (tuatara.defaults::package-state "pkgv" where nil))))))

(define-test (package-trust nothing-fits-keeps-what-is-loaded)
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      ;; nothing fits at all: not loaded, said
      (trust-version dir "far" :needs "9.0")
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                      (list :until (lambda () (echo-says "no older version tried fits"))))
                :width 240)))
        (true (tuatara.frontend.fake:fake-frame-showing f "it is not loaded"))
        (false (trust-version-loaded)))))
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      ;; loaded, then updated to a repository whose new versions all need more: kept
      (let ((one (trust-version dir "one")))
        (auto-save-session
         (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
               (list :until (lambda () (trust-version-loaded)))
               (list :until (lambda () (trust-version dir "two" :needs "9.0") t))
               (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
               (list :until (lambda () (echo-says "up to date")))))
        (is equal "one" (trust-version-loaded))
        (is equal one (second (tuatara.defaults::package-state "pkgv" where "latest")))))))

(define-test (package-trust failed-version-falls-back)
  ;; a new version that cannot be compiled, or uses a name Tuatara lacks: the one before is loaded again
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (let ((one (trust-version dir "one")) (screens '()))
        (dolist (body (list (format nil "(defparameter *package-trust-version* \"broken\")~%(defun broken (~%")
                            (format nil "(defparameter *package-trust-version* \"lacking\")~%(tuatara::no-such-function-anywhere-at-all)~%")))
          (let ((f (auto-save-session
                    (list (list :until (lambda () (unless (trust-version-loaded)
                                                    (tuatara-user:load-package "pkgv" :source where :version "latest"))
                                         t))
                          (list :until (lambda () (trust-version-loaded)))
                          (list :until (lambda () (trust-version dir "next" :body body) t))
                          (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                          (list :until (lambda () (echo-says "is loaded in its place")))
                          (list :until (lambda () (or (echo-says "up to date") (echo-says "pkgv is loaded")))))
                    :width 240)))
            (push (tuatara.frontend.fake:fake-frame-showing f "the version before it") screens))
          (is equal "one" (trust-version-loaded))
          (is equal one (second (tuatara.defaults::package-state "pkgv" where "latest")))
          ;; what failed is not kept
          (is = 1 (length (kept-versions where))))
        (true (every #'identity screens))))))

(define-test (package-trust two-versions-kept)
  ;; three updates in a row leave the version taken and the one before; a failed fourth the same two
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) (commits '()))
      (push (trust-version dir "v0") commits)
      (auto-save-session
       (append (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                     (list :until (lambda () (equal "v0" (trust-version-loaded)))))
               (loop for n from 1 to 3
                     for name = (format nil "v~d" n)
                     append (let ((name name))
                              (list (list :until (lambda () (push (trust-version dir name) commits) t))
                                    (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                                    (list :until (lambda () (equal name (trust-version-loaded)))))))))
      (flet ((kept () (sort (mapcar (lambda (f) (car (last (pathname-directory f)))) (kept-versions where)) #'string<)))
        (is equal (sort (list (first commits) (second commits)) #'string<) (kept))
        (let ((compiled (directory (merge-pathnames "**/*.fasl" (directory-namestring
                                                                (tuatara.defaults::package-fasl
                                                                 (merge-pathnames "x" (tuatara.defaults::package-kept-folder where "pkgv"))))))))
          (is = 2 (length compiled)))
        (auto-save-session
         (list (list :until (lambda () (trust-version dir "v4" :body (format nil "(defun broken (~%")) t))
               (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
               (list :until (lambda () (echo-says "is loaded in its place")))
               (list :until (lambda () (or (echo-says "up to date") (echo-says "pkgv is loaded"))))))
        (is equal "v3" (trust-version-loaded))
        (is equal (sort (list (first commits) (second commits)) #'string<) (kept))))))

(define-test (package-trust first-signer-remembered)
  ;; trust on first use: the signer asked about with the version, remembered; the same key taken
  ;; later; another key refused, the version loaded kept, until package-trust-again
  (with-trust-repository (dir where)
    (multiple-value-bind (alice alice-print) (trust-ssh-key dir "alice")
      (multiple-value-bind (bob bob-print) (trust-ssh-key dir "bob")
        (trust-version dir "one" :signer alice)
        (let ((tuatara-user:*package-ask-before-loading* t))
          (let ((f (auto-save-session
                    (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                          (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                          "y RET"
                          (list :until (lambda () (trust-version-loaded))))
                    :width 240)))
            (true (tuatara.frontend.fake:fake-frame-showing-across-rows f (format nil "signed by the key ~a" alice-print)))
            (is equal alice-print (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))))
        (let ((tuatara-user:*package-ask-before-loading* nil))
          ;; the same key: taken
          (auto-save-session
           (list (list :until (lambda () (trust-version dir "two" :signer alice) t))
                 (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                 (list :until (lambda () (equal "two" (trust-version-loaded))))))
          (is equal "two" (trust-version-loaded))
          ;; another key: refused, saying how to take it
          (let ((f (auto-save-session
                    (list (list :until (lambda () (trust-version dir "three" :signer bob) t))
                          (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                          (list :until (lambda () (echo-says "is refused"))))
                    :width 400)))
            (true (tuatara.frontend.fake:fake-frame-showing f "the versions taken before were signed by"))
            (true (tuatara.frontend.fake:fake-frame-showing f "The version loaded now is kept"))
            (true (tuatara.frontend.fake:fake-frame-showing f "package-trust-again")))
          (is equal "two" (trust-version-loaded))
          (is = 2 (length (kept-versions where)))
          ;; trusted anew: bob's version taken and bob remembered
          (auto-save-session
           (list "M-x p a c k a g e - t r u s t - a g a i n RET" "p k g v RET"
                 (list :until (lambda () (equal "three" (trust-version-loaded))))))
          (is equal "three" (trust-version-loaded))
          (is equal bob-print (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where))))))))

(define-test (package-trust unsigned-stays-usable)
  ;; an unsigned source: its updates taken, the list saying unsigned; a signed one later is
  ;; taken and its signer remembered, and unsigned after that is refused
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) (screen nil))
      (multiple-value-bind (alice alice-print) (trust-ssh-key dir "alice")
        (trust-version dir "one")
        (auto-save-session
         (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
               (list :until (lambda () (trust-version-loaded)))
               (list :until (lambda () (trust-version dir "two") t))
               (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
               (list :until (lambda () (equal "two" (trust-version-loaded))))
               "M-x p a c k a g e - m a n a g e r RET" (list :wait 0.2)
               (list :until (lambda () (setf screen (format nil "~{~a~%~}"
                                                            (loop with f = (tuatara.command:editor-frontend tuatara.command:*editor*)
                                                                  for row below 20 collect (tuatara.frontend.fake:fake-screen-line f row))))))
               (list :until (lambda () (trust-version dir "three" :signer alice) t))
               (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
               (list :until (lambda () (equal "three" (trust-version-loaded))))
               (list :until (lambda () (trust-version dir "four") t))
               (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
               (list :until (lambda () (echo-says "is refused"))))
         :width 240)
        (true (search "unsigned" screen) "~a" screen)
        (is equal alice-print (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))
        (is equal "three" (trust-version-loaded))))))

(define-test (package-trust signers-named-strict)
  ;; :signers: only those keys, from the first version
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (multiple-value-bind (alice) (trust-ssh-key dir "alice")
        (multiple-value-bind (bob bob-print) (trust-ssh-key dir "bob")
          (declare (ignore bob))
          (trust-version dir "one" :signer alice)
          (tuatara-user:package-source "strict" where :signers (list bob-print))
          (let ((f (auto-save-session
                    (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source "strict" :version "latest") t))
                          (list :until (lambda () (echo-says "is refused"))))
                    :width 400)))
            (true (tuatara.frontend.fake:fake-frame-showing f "by none of the signers its :signers name"))
            (false (trust-version-loaded))
            (is = 0 (length (kept-versions where)))))))
    (fail (tuatara-user:package-source "bad" where :signers (list "not a key")))))

(define-test (package-trust gpg-signed-tag)
  ;; a tag signed with GPG (the tests' own keyring): its signer's name and fingerprint remembered
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-trust-repository (dir where)
        (let ((tuatara-user:*package-ask-before-loading* nil)
              (key (gpg-test-key)))
          (trust-version dir "one" :gpg key :tag "v1")
          (auto-save-session
           (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where) t))
                 (list :until (lambda () (trust-version-loaded)))))
          (is equal "one" (trust-version-loaded))
          (let ((trusted (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))
            (true (search "Kea Taranaki" (or (third trusted) "")))
            (true (cl-ppcre:scan "^[0-9A-F]{40}$" (second trusted))))))))

(define-test (package-trust plain-transport-allowed-by-name)
  ;; http:// and git:// refused, saying how to allow it; :plain t on its load-package allows it
  (let ((tuatara-user:*package-ask-before-loading* nil)
        (tuatara.defaults::*package-plain-allowed* '())
        (tuatara.defaults::*declared-packages* '())
        (tuatara.defaults::*package-failures* '()))
    (dolist (where '("http://127.0.0.1:9/pkgv.git" "git://127.0.0.1:9/pkgv.git"))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where) t))
                      (list :until (lambda () (echo-says "is not fetched from"))))
                :width 400)))
        (true (tuatara.frontend.fake:fake-frame-showing f "adding :plain t to its load-package"))))
    (let ((f (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source "http://127.0.0.1:9/pkgv.git" :plain t) t))
                    (list :until (lambda () (echo-says "was not fetched"))))
              :width 400)))
      ;; allowed: fetched, and failing only for there being no server
      (false (tuatara.frontend.fake:fake-frame-showing f "adding :plain t")))
    ;; and the package list says it is unencrypted
    (is equal "unencrypted" (tuatara.defaults::package-signed-words "http://127.0.0.1:9/pkgv.git"))
    (setf tuatara.defaults::*declared-packages* '())))

(defun other-gnupg-key (dir)
  "Make in DIR a GnuPG folder of its own with a key, Tasman Kaikoura's, that
the tests' own keyring lacks: the folder and the key's fingerprint."
  (let ((home (namestring (ensure-directories-exist (merge-pathnames "other-gnupg/" dir)))))
    (sb-posix:chmod home #o700)
    (uiop:run-program (list "gpg" "--homedir" home "--batch" "--passphrase" "" "--quick-gen-key"
                            "Tasman Kaikoura <tasman@example.org>" "ed25519" "sign" "never")
                      :ignore-error-status t :error-output nil)
    (values home
            (let ((line (find-if (lambda (l) (uiop:string-prefix-p "fpr:" l))
                                 (uiop:split-string (uiop:run-program (list "gpg" "--homedir" home "--list-keys" "--with-colons")
                                                                      :output :string)
                                                    :separator '(#\Newline)))))
              (tenth (uiop:split-string line :separator ":"))))))

(defun stop-gnupg (home)
  "Stop the gpg-agent of the GnuPG folder HOME."
  (uiop:run-program (list "gpgconf" "--homedir" home "--kill" "gpg-agent") :ignore-error-status t))

(define-test (package-trust asked-again-with-its-signer)
  ;; a version left unanswered (v) is asked about again at the next start, who signed it shown;
  ;; y then remembers that signer and keeps the version before it to fall back on
  (with-trust-repository (dir where)
    (multiple-value-bind (alice alice-print) (trust-ssh-key dir "alice")
      (let ((one (trust-version dir "one" :signer alice)))
        (let ((tuatara-user:*package-ask-before-loading* nil))
          (auto-save-session
           (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                 (list :until (lambda () (trust-version-loaded))))))
        (trust-version dir "two" :signer alice)
        (tuatara.defaults::state-list-drop tuatara.defaults::*package-trust* (list where))
        (let ((tuatara-user:*package-ask-before-loading* t))
          (auto-save-session
           (list (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                 (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                 "v RET" (list :until (lambda () (echo-says "is shown")))))
          (false (fourth (tuatara.defaults::package-state "pkgv" where "latest")))
          ;; the next start
          (setf tuatara.defaults::*loaded-packages* '())
          (makunbound 'tuatara-user::*package-trust-version*)
          (let ((f (auto-save-session
                    (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                          (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                          "y RET" (list :until (lambda () (trust-version-loaded))))
                    :width 240)))
            (true (tuatara.frontend.fake:fake-frame-showing-across-rows f (format nil "signed by the key ~a" alice-print))))
          (is equal "two" (trust-version-loaded))
          (is equal alice-print (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))
          (let ((entry (tuatara.defaults::package-state "pkgv" where "latest")))
            (true (fourth entry))
            (is equal one (fifth entry))))))))

(define-test (package-trust never-walks-below-what-is-taken)
  ;; an unsigned newest version needing a newer Tuatara, from a source signed before: nothing
  ;; older is looked for
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (multiple-value-bind (alice) (trust-ssh-key dir "alice")
        (trust-version dir "one" :signer alice)
        (let ((two (trust-version dir "two" :signer alice)))
          (let ((f (auto-save-session
                    (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                          (list :until (lambda () (equal "two" (trust-version-loaded))))
                          (list :until (lambda () (trust-version dir "three" :needs "99") t))
                          (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                          (list :until (lambda () (echo-says "no older version is looked for"))))
                    :width 300)))
            (true (tuatara.frontend.fake:fake-frame-showing f "not signed as the versions taken before were")))
          (is equal two (second (tuatara.defaults::package-state "pkgv" where "latest")))))))
  ;; history rewritten under the version taken: an older version is never taken in its place
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (let ((one (trust-version dir "one")))
        (let ((two (trust-version dir "two")))
          (auto-save-session
           (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                 (list :until (lambda () (equal "two" (trust-version-loaded))))
                 (list :until (lambda () (trust-git dir "reset" "-q" "--hard" one) (trust-version dir "three" :needs "99") t))
                 (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                 (list :until (lambda () (echo-says "no older version tried fits")))))
          (is equal "two" (trust-version-loaded))
          (is equal two (second (tuatara.defaults::package-state "pkgv" where "latest"))))))))

(define-test (package-trust unchecked-key-kept-until-imported)
  ;; a first version signed by a GPG key the keyring lacks is refused, its key's fingerprint remembered,
  ;; never taken as unsigned; once the key is imported, that key's versions are taken
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-trust-repository (dir where)
        (multiple-value-bind (home print) (other-gnupg-key dir)
          (unwind-protect
               (let ((tuatara-user:*package-ask-before-loading* nil))
                 (let ((*trust-gnupg-home* home))
                   (trust-version dir "one" :gpg print))
                 (let ((f (auto-save-session
                           (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                                 (list :until (lambda () (echo-says "is refused"))))
                           :width 400)))
                   (true (tuatara.frontend.fake:fake-frame-showing f "which your keyring lacks")))
                 (false (trust-version-loaded))
                 (is equal (format nil "key:~a" print)
                     (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))
                 ;; an unsigned version is not taken in its place
                 (trust-version dir "unsigned")
                 (auto-save-session
                  (list (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                        (list :until (lambda () (echo-says "is refused")))))
                 (false (trust-version-loaded))
                 ;; the key imported: its version is taken, and its whole fingerprint remembered
                 (let ((*trust-gnupg-home* home))
                   (trust-version dir "two" :gpg print))
                 (uiop:run-program (format nil "gpg --homedir ~a --export ~a | gpg --batch --import" home print) :ignore-error-status t
                                                                                                                :error-output nil)
                 (auto-save-session
                  (list (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                        (list :until (lambda () (trust-version-loaded)))))
                 (is equal "two" (trust-version-loaded))
                 (is equal print (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where))))
            (stop-gnupg home))))))

(define-test (package-trust start-checks-current-rules)
  ;; at a start, a version kept from a source whose :signers do not name its signer is refused,
  ;; said, not loaded; one whose source is trusted anew is asked about again
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (multiple-value-bind (bob bob-print) (trust-ssh-key dir "bob")
        (declare (ignore bob))
        (trust-version dir "one")
        (auto-save-session
         (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
               (list :until (lambda () (trust-version-loaded)))))
        (setf tuatara.defaults::*loaded-packages* '())
        (makunbound 'tuatara-user::*package-trust-version*)
        (tuatara-user:package-source "strict" where :signers (list bob-print))
        (let ((f (auto-save-session
                  (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source "strict" :version "latest") t))
                        (list :until (lambda () (echo-says "is refused"))))
                  :width 400)))
          (true (tuatara.frontend.fake:fake-frame-showing f "by none of the signers its :signers name")))
        (false (trust-version-loaded))
        ;; trusted anew, no :signers: asked again, its signer shown
        (setf tuatara.defaults::*package-signers* '())
        (tuatara.defaults::state-list-drop tuatara.defaults::*package-trust* (list where))
        (let ((tuatara-user:*package-ask-before-loading* t))
          (let ((f (auto-save-session
                    (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                          (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                          "y RET" (list :until (lambda () (trust-version-loaded))))
                    :width 240)))
            (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "from"))
            (true (tuatara.frontend.fake:fake-frame-showing-across-rows f ", unsigned?"))))
        (is equal "unsigned" (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))))))

(define-test (package-trust plain-and-ftp-kept-out)
  ;; git is told never to follow a server to http:// or git:// unless :plain t allows it; ftp never
  (flet ((setting (name)
           (let* ((environment (tuatara.defaults::package-git-environment))
                  (key (find-if (lambda (e) (search (format nil "=~a" name) e)) environment)))
             (and key (let ((n (subseq key (length "GIT_CONFIG_KEY_") (position #\= key))))
                        (let ((value (find-if (lambda (e) (uiop:string-prefix-p (format nil "GIT_CONFIG_VALUE_~a=" n) e)) environment)))
                          (subseq value (1+ (position #\= value)))))))))
    (let ((tuatara.defaults::*package-plain-transport* nil))
      (is equal "never" (setting "protocol.http.allow"))
      (is equal "never" (setting "protocol.git.allow"))
      (is equal "never" (setting "protocol.ftp.allow"))
      (is equal "/dev/null" (setting "core.hooksPath")))
    (let ((tuatara.defaults::*package-plain-transport* t))
      (false (setting "protocol.http.allow"))
      (is equal "never" (setting "protocol.ftp.allow"))))
  (fail (tuatara-user:package-source "ftp" "ftp://example.org/pkgv.git"))
  (fail (tuatara-user:load-package "pkgv" :source "ftps://example.org/pkgv.git")))

(define-test (package-trust own-git-programs-never-run)
  ;; a filter program and a hook of the person's own git settings never run on a package
  (with-trust-repository (dir where)
    (let* ((marker (merge-pathnames "filtered" dir))
           (hooked (merge-pathnames "hooked" dir))
           (hooks (ensure-directories-exist (merge-pathnames "hooks/" dir)))
           (settings (merge-pathnames "own-gitconfig" dir))
           (before (uiop:getenv "GIT_CONFIG_GLOBAL"))
           (tuatara-user:*package-ask-before-loading* nil))
      (dolist (hook '("post-checkout" "reference-transaction"))
        (alexandria:write-string-into-file (format nil "#!/bin/sh~%touch ~a~%" (namestring hooked)) (merge-pathnames hook hooks))
        (sb-posix:chmod (namestring (merge-pathnames hook hooks)) #o755))
      (alexandria:write-string-into-file
       (format nil "[user]~%	name = Test~%	email = test@example.invalid~%[commit]~%	gpgsign = false~%[filter \"own\"]~%	smudge = sh -c 'touch ~a; cat'~%	required = true~%[core]~%	hooksPath = ~a~%"
               (namestring marker) (namestring hooks))
       settings)
      (dolist (place '(".gitattributes" "pkgv/.gitattributes"))
        (alexandria:write-string-into-file (format nil "* filter=own~%") (ensure-directories-exist (merge-pathnames place dir))))
      (trust-version dir "one")
      (ignore-errors (delete-file marker))
      (ignore-errors (delete-file hooked))
      (sb-posix:setenv "GIT_CONFIG_GLOBAL" (namestring settings) 1)
      (unwind-protect
           (auto-save-session
            (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                  (list :until (lambda () (or (trust-version-loaded) (echo-says "not"))))))
        (sb-posix:setenv "GIT_CONFIG_GLOBAL" before 1))
      (is equal "one" (trust-version-loaded))
      (false (probe-file marker))
      (false (probe-file hooked)))))

(define-test (package-trust tag-checked-is-the-one-fetched)
  ;; a commit kept before, tagged anew by a signed tag: the new tag's signature is checked
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (multiple-value-bind (bob bob-print) (trust-ssh-key dir "bob")
        (trust-version dir "one" :tag "v1")
        (auto-save-session
         (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where) t))
               (list :until (lambda () (trust-version-loaded)))))
        (let* ((kept (first (kept-versions where)))
               (fetch-head (merge-pathnames ".git/FETCH_HEAD" kept))
               (before (alexandria:read-file-into-string fetch-head)))
        (is equal "unsigned" (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))
        (sleep 1.1)
        (trust-git dir "-c" "gpg.format=ssh" "-c" (format nil "user.signingkey=~a" bob) "tag" "-s" "v2" "-m" "v2")
        (auto-save-session
         (list (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
               (list :until (lambda () (echo-says "up to date")))))
        (is equal bob-print (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))
        ;; the tag was fetched apart: nothing written into the folder kept
        (is equal before (alexandria:read-file-into-string fetch-head)))))))

(define-test (package-trust signatures-read-strictly)
  ;; a tag's signature is its last block, not one written in its message
  (let ((object (format nil "object 0123~%type commit~%tag v1~%~%v1~%-----BEGIN SSH SIGNATURE-----~%AAAA~%-----END SSH SIGNATURE-----~%~
                             -----BEGIN PGP SIGNATURE-----~%BBBB~%-----END PGP SIGNATURE-----~%")))
    (false (tuatara.defaults::ssh-signature-p object :tag))
    (is = (search "-----BEGIN PGP" object) (tuatara.defaults::signature-block object :tag)))
  ;; a commit's is in its header only
  (false (tuatara.defaults::signature-block (format nil "tree 0~%author A~%~%-----BEGIN PGP SIGNATURE-----~%x~%-----END PGP SIGNATURE-----~%") :commit))
  ;; gpg's status lines read only at the start of a line
  (false (tuatara.defaults::gnupg-status (format nil "Name [GNUPG:] GOODSIG 1 x~%") "GOODSIG"))
  (true (tuatara.defaults::gnupg-status (format nil "[GNUPG:] NEWSIG~%[GNUPG:] GOODSIG 1 x~%") "GOODSIG"))
  ;; an expired signature counts as good, as for a commit
  (is eq :good (getf (tuatara.defaults::tag-signature
                      (format nil "[GNUPG:] EXPSIG 42C0A0291C34DD19 Ana~%[GNUPG:] VALIDSIG D647C771761F736B31ED8A2E42C0A0291C34DD19 x~%")
                      (format nil "tag v1~%~%-----BEGIN PGP SIGNATURE-----~%x~%-----END PGP SIGNATURE-----~%"))
                     :status))
  ;; a signed commit git could not check is no unsigned one
  (is eq :error (getf (tuatara.defaults::commit-signature (list "N" "" "" "" "")
                                                          (format nil "tree 0~%gpgsig -----BEGIN PGP SIGNATURE-----~% x~% -----END PGP SIGNATURE-----~%~%m~%"))
                      :status))
  (is eq :none (getf (tuatara.defaults::commit-signature (list "N" "" "" "" "") (format nil "tree 0~%~%m~%")) :status))
  ;; key IDs refused in :signers, saying to name the whole fingerprint
  (true (search "whole fingerprint"
                (handler-case (progn (tuatara.defaults::note-package-source-terms "https://example.org/x" (list "42C0A0291C34DD19")) "")
                  (error (e) (princ-to-string e)))))
  ;; names shown as they are, no right-to-left override
  (is equal "Ana evil" (tuatara.defaults::plain-name (format nil "Ana ~cevil" (code-char #x202E)))))

(define-test (package-trust gpg-subkey-kept-until-imported)
  ;; the usual GPG key, a primary key and a subkey that signs: the subkey's whole fingerprint is
  ;; remembered while the keyring lacks it, and the key is taken once imported
  (if (null (tuatara.defaults::gpg-path))
      (skip "gpg is not installed here" (true nil))
      (with-trust-repository (dir where)
        (let ((home (namestring (ensure-directories-exist (merge-pathnames "subkey-gnupg/" dir)))))
          (sb-posix:chmod home #o700)
          (unwind-protect
               (flet ((gpg (&rest arguments)
                        (uiop:run-program (list* "gpg" "--homedir" home "--batch" "--passphrase" "" arguments)
                                          :output :string :error-output nil :ignore-error-status t))
                      (fingerprints (text)
                        (loop for line in (uiop:split-string text :separator '(#\Newline))
                              when (uiop:string-prefix-p "fpr:" line) collect (tenth (uiop:split-string line :separator ":")))))
                 (gpg "--quick-gen-key" "Rangi Waitaki <rangi@example.org>" "ed25519" "cert" "never")
                 (let ((primary (first (fingerprints (gpg "--list-keys" "--with-colons")))))
                   (gpg "--quick-add-key" primary "ed25519" "sign" "never")
                   (let ((subkey (second (fingerprints (gpg "--list-keys" "--with-colons"))))
                         (tuatara-user:*package-ask-before-loading* nil))
                     (let ((*trust-gnupg-home* home))
                       (trust-version dir "one" :gpg primary))
                     (auto-save-session
                      (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                            (list :until (lambda () (echo-says "is refused")))))
                     (is equal (format nil "key:~a" subkey)
                         (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where)))
                     (uiop:run-program (format nil "gpg --homedir ~a --export ~a | gpg --batch --import" home primary)
                                       :ignore-error-status t :error-output nil)
                     (let ((*trust-gnupg-home* home))
                       (trust-version dir "two" :gpg primary))
                     (auto-save-session
                      (list (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                            (list :until (lambda () (trust-version-loaded)))))
                     (is equal "two" (trust-version-loaded))
                     (is equal primary (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where))))))
            (stop-gnupg home))))))

(define-test (package-trust signature-headers-read-strictly)
  ;; a merged tag's signature in a commit's header is not the commit's own
  (false (tuatara.defaults::signature-block
          (format nil "tree 0~%parent 1~%mergetag object 2~% type commit~% tag v1~% -----BEGIN PGP SIGNATURE-----~% x~% -----END PGP SIGNATURE-----~%author A~%~%m~%")
          :commit))
  (true (tuatara.defaults::signature-block
         (format nil "tree 0~%author A~%gpgsig -----BEGIN SSH SIGNATURE-----~% x~% -----END SSH SIGNATURE-----~%~%m~%")
         :commit))
  ;; a tag's name read from its header, not from its message
  (false (tuatara.defaults::tag-object-names-p (format nil "object 0~%type commit~%tag v1~%~%tag v2~%") "v2"))
  (true (tuatara.defaults::tag-object-names-p (format nil "object 0~%type commit~%tag v2~%~%notes~%") "v2")))

(define-test (package-trust key-check-files-apart)
  ;; each check of an SSH key writes a file of its own, so that two at once never share one
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) (names '()))
      (multiple-value-bind (alice) (trust-ssh-key dir "alice")
        (trust-version dir "one" :signer alice)
        (sb-int:encapsulate 'tuatara.io:write-private-file 'key-check-files-apart
                            (lambda (function path text)
                              (when (search ".allowed-signers-check" (namestring path)) (push (namestring path) names))
                              (funcall function path text)))
        (unwind-protect
             (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                    (list :until (lambda () (trust-version-loaded)))
                    (list :until (lambda () (trust-version dir "two" :signer alice) t))
                    (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                    (list :until (lambda () (equal "two" (trust-version-loaded))))))
          (sb-int:unencapsulate 'tuatara.io:write-private-file 'key-check-files-apart))
        (true (>= (length names) 2))
        (is = (length names) (length (remove-duplicates names :test #'string=)))))))

(define-test (package-trust fallback-checked-and-one-job)
  ;; the version before one that failed is checked against the rules in force: refused when the
  ;; init file's :signers no longer name its signer, and not loaded at a start
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil))
      (multiple-value-bind (alice alice-print) (trust-ssh-key dir "alice")
        (multiple-value-bind (bob bob-print) (trust-ssh-key dir "bob")
          (tuatara-user:package-source "signed" where :signers (list alice-print))
          (trust-version dir "one" :signer alice)
          (auto-save-session
           (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source "signed" :version "latest") t))
                 (list :until (lambda () (equal "one" (trust-version-loaded))))
                 (list :until (lambda () (trust-version dir "two" :signer alice) t))
                 (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                 (list :until (lambda () (equal "two" (trust-version-loaded))))))
          (tuatara-user:package-source "signed" where :signers (list bob-print))
          (let ((f (auto-save-session
                    (list (list :until (lambda () (trust-version dir "three" :signer bob :body (format nil "(defun broken (~%")) t))
                          (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
                          (list :until (lambda () (echo-says "is refused"))))
                    :width 400)))
            (true (tuatara.frontend.fake:fake-frame-showing f "the version before it"))
            (true (tuatara.frontend.fake:fake-frame-showing f "by none of the signers its :signers name")))
          (false (fourth (tuatara.defaults::package-state "pkgv" where "latest")))))))
  ;; one fetch at a time for a package, however often it is asked for
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) (jobs nil))
      (trust-version dir "one")
      (auto-save-session
       (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
             (list :until (lambda () (trust-version-loaded)))
             (list :until (lambda () (trust-version dir "two") t))
             (list :until (lambda ()
                            (tuatara.defaults::update-packages (list "pkgv"))
                            (tuatara.defaults::update-packages (list "pkgv"))
                            (setf jobs (count-if (lambda (job) (and (tuatara.defaults::package-fetch-p job)
                                                                    (equal "pkgv" (tuatara.defaults::package-fetch-name job))))
                                                 tuatara.runtime:*jobs*))
                            t))
             (list :until (lambda () (equal "two" (trust-version-loaded))))))
      (is = 1 jobs))))

(define-test (package-trust guard-let-go-and-said)
  ;; a fetch that cannot be started leaves its package free to be fetched later
  (with-trust-repository (dir where)
    (trust-version dir "one")
    (sb-int:encapsulate 'tuatara.runtime:start-job-thread 'guard-let-go-and-said
                        (lambda (function &rest arguments) (declare (ignore function arguments)) (error "no thread")))
    (unwind-protect
         (handler-case (tuatara.defaults::start-package-fetch "pkgv" where "latest" nil (format nil "pkgv ~a latest" where))
           (error () nil))
      (sb-int:unencapsulate 'tuatara.runtime:start-job-thread 'guard-let-go-and-said))
    (false (member "pkgv" tuatara.defaults::*fetching-packages* :test #'string=))
    (false (assoc "pkgv" (ignore-errors (symbol-value 'tuatara.defaults::*packages-in-hand*)) :test #'string=))
    ;; asked for again while its question waits: said, and no second fetch started
    (let ((tuatara-user:*package-ask-before-loading* t) (jobs nil) (said nil))
      (let ((f (auto-save-session
                (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                      (list :until (lambda () (tuatara.command::editor-minibuffer-active tuatara.command:*editor*)))
                      (list :until (lambda ()
                                     (tuatara.defaults::update-packages (list "pkgv"))
                                     (setf jobs (count-if #'tuatara.defaults::package-fetch-p tuatara.runtime:*jobs*))
                                     t))
                      (list :until (lambda () (setf said (echo-says "pkgv: a fetch or check of it is already running"))))
                      "n RET" (list :until (lambda () (echo-says "is not loaded"))))
                :width 240)))
        (true said)
        (true (tuatara.frontend.fake:fake-frame-showing f "is not loaded"))
        (is = 0 jobs)
        ;; answered: free again
        (false (assoc "pkgv" tuatara.defaults::*packages-in-hand* :test #'string=))))))

(define-test (package-trust key-check-file-gone-on-error)
  ;; the file a key is checked against goes even when the check ends in an error
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) (calls 0))
      (multiple-value-bind (alice) (trust-ssh-key dir "alice")
        (trust-version dir "one" :signer alice)
        (sb-int:encapsulate 'tuatara.defaults::commit-signature 'key-check-file-gone-on-error
                            (lambda (function &rest arguments)
                              (if (= 2 (incf calls)) (error "checking failed") (apply function arguments))))
        (unwind-protect
             (auto-save-session
              (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                    (list :until (lambda () (echo-says "was not fetched")))))
          (sb-int:unencapsulate 'tuatara.defaults::commit-signature 'key-check-file-gone-on-error))
        (is = 2 calls)
        (false (directory (merge-pathnames "*.allowed-signers-check-*"
                                           (directory-namestring (tuatara.defaults::package-signers-file where)))))
        (false (directory (merge-pathnames ".allowed-signers-check-*"
                                           (directory-namestring (tuatara.defaults::package-signers-file where)))))))))

(define-test (package-trust start-judges-the-kept-version)
  ;; the version kept is judged by who signed it, not by the source's signer: an unsigned one put
  ;; back (a fallback ended by quitting) is refused at the next start once the source is signed
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) (key (format nil "pkgv ~a latest" where)) (first-entry nil))
      (multiple-value-bind (alice) (trust-ssh-key dir "alice")
        (trust-version dir "one")
        (auto-save-session
         (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
               (list :until (lambda () (trust-version-loaded)))
               (list :until (lambda () (setf first-entry (tuatara.defaults::fetched-package key))))
               (list :until (lambda () (trust-version dir "two" :signer alice) t))
               (list :until (lambda () (tuatara.defaults::update-packages (list "pkgv")) t))
               (list :until (lambda () (equal "two" (trust-version-loaded))))))
        (is equal "unsigned" (seventh first-entry))
        (tuatara.defaults::state-list-put tuatara.defaults::*fetched-packages* (list first-entry))
        (setf tuatara.defaults::*loaded-packages* '())
        (makunbound 'tuatara-user::*package-trust-version*)
        (let ((f (auto-save-session
                  (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source where :version "latest") t))
                        (list :until (lambda () (echo-says "is refused"))))
                  :width 400)))
          (true (tuatara.frontend.fake:fake-frame-showing f "the versions taken before were signed by")))
        (false (trust-version-loaded))))))

(define-test (package-trust signer-remembered-after-loading)
  ;; a version taken before whose load fails does not make its signer the one remembered
  (with-trust-repository (dir where)
    (let ((tuatara-user:*package-ask-before-loading* nil) (key (format nil "pkgv ~a latest" where)))
      (multiple-value-bind (alice alice-print) (trust-ssh-key dir "alice")
        (tuatara-user:package-source "signed" where :signers (list alice-print))
        (let ((one (trust-version dir "one" :signer alice)))
          (auto-save-session
           (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source "signed" :version "latest") t))
                 (list :until (lambda () (trust-version-loaded)))))
          ;; the source remembered as unsigned (its :signers decide), the version kept broken, who signed it not known
          (tuatara.defaults::state-list-put tuatara.defaults::*package-trust* (list (list where "unsigned" nil nil)))
          (tuatara.defaults::state-list-put tuatara.defaults::*fetched-packages* (list (list key one nil t)))
          (alexandria:write-string-into-file (format nil "(defun broken (~%")
                                             (merge-pathnames "pkgv/pkgv.lisp" (tuatara.defaults::package-folder-for where "pkgv" one))
                                             :if-exists :supersede)
          (setf tuatara.defaults::*loaded-packages* '())
          (makunbound 'tuatara-user::*package-trust-version*)
          (auto-save-session
           (list (list :until (lambda () (tuatara-user:load-package "pkgv" :source "signed" :version "latest") t))
                 (list :until (lambda () (echo-says "failed")))))
          (false (trust-version-loaded))
          (is equal "unsigned" (second (tuatara.defaults::state-list-find tuatara.defaults::*package-trust* where))))))))
