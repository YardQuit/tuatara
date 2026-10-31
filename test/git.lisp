;;;; test/git.lisp --- Git from the editor (C-x g): a repository's state, staging, commits.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test git
  :description "C-x g shows the state of the repository a file is in: its branch, files not tracked, changed and staged, its latest commits; s stages, u unstages, x reverses changes (files, hunks or lines selected) or moves untracked files to the trash, TAB shows a file's changes, n and p go by files and hunks, c commits, L lists the log; only for a project trusted.")

(defun scratch-repository (dir)
  "Make DIR a git repository of its own, signing nothing, holding a.txt committed."
  (flet ((git (&rest arguments)
           (uiop:run-program (list* "git" "-C" (namestring dir) arguments) :ignore-error-status t)))
    (git "init" "-q" "-b" "main")
    (git "config" "user.name" "Test")
    (git "config" "user.email" "test@example.invalid")
    (git "config" "commit.gpgsign" "false")
    (alexandria:write-string-into-file (format nil "one~%") (merge-pathnames "a.txt" dir))
    (git "add" "a.txt")
    (git "commit" "-q" "-m" "First")))

(defun git-output (dir &rest arguments)
  "What git prints with ARGUMENTS in DIR."
  (uiop:run-program (list* "git" "-C" (namestring dir) arguments) :output :string :ignore-error-status t))

(defun git-buffer-named (prefix)
  "The buffer whose name begins with PREFIX, when there is one."
  (find-if (lambda (b) (uiop:string-prefix-p prefix (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*))

(define-test (git look-ended-in-time)
  ;; a repository whose settings take in a named pipe leaves git waiting: the gutter's and rimu's looks end it
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((fifo (namestring (merge-pathnames ".git/fifo" dir))))
      (sb-posix:mkfifo fifo #o600)
      (with-open-file (out (merge-pathnames ".git/config" dir) :direction :output :if-exists :append)
        (format out "[include]~%	path = fifo~%"))
      (let ((start (get-internal-real-time)))
        (false (nth-value 1 (tuatara.defaults::run-git (namestring dir) (list "cat-file" "-e" "HEAD:./a.txt") :seconds 1)))
        (true (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))))))

(define-test (git status-stage-commit)
  (with-test-directory (dir)
    (scratch-repository dir)
    (alexandria:write-string-into-file (format nil "one~%two~%") (merge-pathnames "a.txt" dir) :if-exists :supersede)
    (alexandria:write-string-into-file (format nil "new~%") (merge-pathnames "b.txt" dir))
    (let ((tuatara.defaults::*modal-editing* nil)
          (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
          (staged nil))
      (flet ((status () (find-if (lambda (b) (typep (tuatara.modes:buffer-major-mode b) 'tuatara.defaults::git-status-mode))
                                 tuatara.buffer:*buffers*)))
        (multiple-value-bind (f editor)
            (auto-save-session
             (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.txt" dir))))
                   "C-x g"
                   (list :until (lambda () (and (status) (tuatara.buffer:buffer-local (status) 'tuatara.defaults::git-state))))
                   ;; a.txt's changes shown under it
                   "C-s a . t x t RET TAB" (list :until (lambda () (search "+two" (tuatara.buffer:buffer-string (status)))))
                   ;; both staged, b.txt from its section's title
                   "M-< C-s U n t r a c k e d RET s" (list :until (lambda () (null (tuatara.defaults::git-state-untracked
                                                                                      (tuatara.buffer:buffer-local (status) 'tuatara.defaults::git-state)))))
                   "M-< C-s m o d i f i e d RET s"
                   (list :until (lambda () (= 2 (length (tuatara.defaults::git-state-staged
                                                         (tuatara.buffer:buffer-local (status) 'tuatara.defaults::git-state))))))
                   (list :until (lambda () (setf staged (mapcar #'first (tuatara.defaults::git-state-staged
                                                                          (tuatara.buffer:buffer-local (status) 'tuatara.defaults::git-state))))))
                   ;; committed, its message written in a buffer of its own
                   "c c" (list :until (lambda () (tuatara.defaults::git-buffer-of (tuatara.defaults::repository-top (namestring dir)) 'tuatara.defaults::git-commit-mode))) "S e c o n d C-c C-c"
                   (list :until (lambda () (search "Second" (git-output dir "log" "--format=%s"))))
                   (list :until (lambda () (null (tuatara.defaults::git-state-staged
                                                  (tuatara.buffer:buffer-local (status) 'tuatara.defaults::git-state))))))
             :width 120 :height 30)
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "Head:     main  First"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Untracked files (1)"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Unstaged changes (1)"))
          (true (tuatara.frontend.fake:fake-frame-showing f "modified   a.txt"))
          (true (tuatara.frontend.fake:fake-frame-showing f "+two"))
          (is equal '("a.txt" "b.txt") (sort staged #'string<))
          (is equal (format nil "Second~%First~%") (git-output dir "log" "--format=%s"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Head:     main  Second")))))))

(define-test (git trusted-only)
  ;; a project not trusted: asked, and with no, git never runs
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((tuatara.defaults::*modal-editing* nil)
          (tuatara-user:*lsp-trusted-projects* '()))
      (let ((f (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "a.txt" dir))))
                                        "C-x g" (list :wait 0.1) "n o RET" (list :wait 0.1))
                                  :width 160)))
        (true (tuatara.frontend.fake:fake-frame-showing-across-rows f "git runs the programs it names"))
        (false (tuatara.defaults::git-buffer-of (tuatara.defaults::repository-top (namestring dir)) 'tuatara.defaults::git-status-mode))))))

(define-test (git status-read)
  ;; what git status --porcelain=v2 says, read: branch, upstream, files of each kind, a rename
  (let ((state (tuatara.defaults::parse-git-status
                (format nil "# branch.oid abc~a# branch.head main~a# branch.upstream origin/main~a# branch.ab +2 -1~a1 .M N... 100644 100644 100644 1 2 src/a b.lisp~a1 A. N... 0 100644 100644 0 2 new.txt~a2 R. N... 100644 100644 100644 1 2 R100 to.txt~afrom.txt~a? loose.txt~a"
                        #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul))))
    (is equal "main" (tuatara.defaults::git-state-branch state))
    (is equal "origin/main" (tuatara.defaults::git-state-upstream state))
    (is = 2 (tuatara.defaults::git-state-ahead state))
    (is = 1 (tuatara.defaults::git-state-behind state))
    (is equal '(("src/a b.lisp" "modified")) (tuatara.defaults::git-state-unstaged state))
    (is equal '(("new.txt" "new file") ("to.txt" "renamed" "from.txt")) (tuatara.defaults::git-state-staged state))
    (is equal '(("loose.txt" "untracked")) (tuatara.defaults::git-state-untracked state))))

(define-test (git unmerged-files-read)
  ;; git status's u lines are files in conflict, a section of their own
  (let ((state (tuatara.defaults::parse-git-status
                (format nil "u UU N... 100644 100644 100644 100644 a1 b2 c3 both.txt~au UD N... 100644 100644 0 100644 a1 b2 c3 gone.txt~a1 .M N... 100644 100644 100644 1 2 plain.txt~a"
                        #\Nul #\Nul #\Nul))))
    (is equal '(("both.txt" "both changed") ("gone.txt" "deleted by them")) (tuatara.defaults::git-state-unmerged state))
    (is equal '(("plain.txt" "modified")) (tuatara.defaults::git-state-unstaged state))))

(define-test (git trusted-where-it-really-is)
  ;; a project is trusted by where it really is: a link from a trusted folder lends none,
  ;; nor does .. out of one; one trusted when asked is kept by its real place
  (with-test-directory (trusted)
    (with-test-directory (elsewhere)
      (let ((tuatara-user:*lsp-trusted-projects* (list (namestring trusted)))
            (inside (merge-pathnames "inside/" trusted))
            (link (concatenate 'string (string-right-trim "/" (namestring trusted)) "/link")))
        (ensure-directories-exist inside)
        (sb-posix:symlink (string-right-trim "/" (namestring elsewhere)) link)
        (true (tuatara.defaults::project-trusted-p (namestring inside)))
        (false (tuatara.defaults::project-trusted-p link))
        (false (tuatara.defaults::project-trusted-p (concatenate 'string (namestring trusted) "../" (car (last (pathname-directory elsewhere))) "/")))
        (false (tuatara.defaults::project-trusted-p (concatenate 'string (namestring trusted) "not-there/")))))))

(define-test (git trust-looked-up-once)
  ;; whether a project is trusted asks where it really is, and not again where each trusted folder is
  (with-test-directory (dir)
    (let* ((folders (loop for k below 50
                          collect (namestring (ensure-directories-exist (merge-pathnames (format nil "t~d/" k) dir)))))
           (tuatara-user:*lsp-trusted-projects* folders)
           (calls 0))
      (tuatara.defaults::project-trusted-p (first folders))
      (sb-int:encapsulate 'tuatara.io:real-path 'count-calls
                          (lambda (f path) (incf calls) (funcall f path)))
      (unwind-protect
           (progn (true (tuatara.defaults::project-trusted-p (car (last folders))))
                  (false (tuatara.defaults::project-trusted-p (namestring dir))))
        (sb-int:unencapsulate 'tuatara.io:real-path 'count-calls))
      (is = 2 calls))))

(defun native-in (dir name)
  "The native name of NAME in the directory DIR, NAME taken as it is (a * no pattern)."
  (concatenate 'string (tuatara.io:native dir) name))

(define-test (git unpushed-commits)
  ;; the commits not pushed have a section of their own: those the upstream lacks,
  ;; else, a branch with no upstream, those on no remote at all
  (with-test-directory (dir)
    (let ((repo (merge-pathnames "unpushed/" dir))
          (remote (native-in dir "remote.git")))
      (ensure-directories-exist repo)
      (scratch-repository repo)
      (git-output dir "init" "-q" "--bare" "-b" "main" remote)
      (git-output repo "remote" "add" "origin" remote)
      (git-output repo "push" "-q" "-u" "origin" "main")
      (put-file (merge-pathnames "a.txt" repo) (format nil "one~%two~%"))
      (git-output repo "commit" "-q" "-am" "Second")
      (put-file (merge-pathnames "a.txt" repo) (format nil "one~%two~%three~%"))
      (git-output repo "commit" "-q" "-am" "Third")
      (let ((f (git-session repo "a.txt" (list (list :wait 0.2)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Unpushed to origin/main (2)"))
        ;; the branch you are on lit up at the top
        (let ((row (loop for r below 30 when (search "Head:     main" (tuatara.frontend.fake:fake-screen-line f r)) return r)))
          (true row)
          (when row
            (true (covered-by-face (aref (tuatara.frontend.fake:fake-spans f) row) :git-status-branch 10 14))))
        (let ((state (git-state-here)))
          (is equal '("Third" "Second") (mapcar #'second (tuatara.defaults::git-state-unpushed state)))))
      ;; a branch of its own, not pushed: on no remote
      (git-output repo "checkout" "-q" "-b" "idea")
      (put-file (merge-pathnames "b.txt" repo) "b")
      (git-output repo "add" "b.txt")
      (git-output repo "commit" "-q" "-m" "Idea")
      (let ((f (git-session repo "a.txt" (list (list :wait 0.2)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Unpushed, on no remote (3)")))
      ;; all pushed: no section
      (git-output repo "checkout" "-q" "main")
      (git-output repo "push" "-q")
      (let ((f (git-session repo "a.txt" (list (list :until (lambda () (let ((state (git-state-here)))
                                                                        (and state (equal "main" (tuatara.defaults::git-state-branch state))
                                                                             (zerop (tuatara.defaults::git-state-ahead state))))))
                                                (list :wait 0.2)))))
        ;; on the screen now, as the frames before showed the section
        (false (loop for row below 30 thereis (search "Unpushed" (tuatara.frontend.fake:fake-screen-line f row))))
        (false (tuatara.defaults::git-state-unpushed (git-state-here))))
      ;; the buffers the sessions made go, for the tests after
      (dolist (buffer (copy-list tuatara.buffer:*buffers*))
        (when (search "unpushed" (tuatara.buffer:buffer-name buffer))
          (tuatara.buffer:kill-buffer buffer)))
      (kill-buffers-visiting dir))))

(defun git-status-here ()
  "The status buffer of the session, when there is one."
  (find-if (lambda (b) (typep (tuatara.modes:buffer-major-mode b) 'tuatara.defaults::git-status-mode))
           tuatara.buffer:*buffers*))

(defun git-state-here ()
  "The state of the repository the session's status last read, or NIL."
  (let ((status (git-status-here)))
    (and status (tuatara.buffer:buffer-local status 'tuatara.defaults::git-state))))

(defun git-session (dir file steps &key (width 120) (height 30))
  "Run STEPS (as AUTO-SAVE-SESSION takes them) once FILE in the repository
DIR is visited and its status read, the project trusted. Returns the frontend."
  (let ((tuatara.defaults::*modal-editing* nil)
        (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
    (values (auto-save-session (list* (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir file)))
                                      "C-x g" (list :until #'git-state-here)
                                      steps)
                               :width width :height height))))

(define-test (git names-one-line-each-and-moved-to-the-trash)
  ;; a name with a newline is one line of the status, \n in it; a file called * is
  ;; staged alone, never as a pattern; k moves a file not tracked to the trash
  (with-test-directory (dir)
    (scratch-repository dir)
    (dolist (name (list "*" "loose.txt" (format nil "two~%lines.txt")))
      (put-file (sb-ext:parse-native-namestring (native-in dir name)) "text"))
    (assert (search (namestring *test-data-directory*) (tuatara.io:home-trash)))
    (flet ((staged () (mapcar #'first (tuatara.defaults::git-state-staged (git-state-here))))
           (gone (name) (lambda () (null (tuatara.io:path-kind (native-in dir name) :follow nil)))))
      (let ((f (git-session dir "a.txt"
                            (list "M-< C-s U n t r a c k e d RET C-n s"
                                  (list :until (lambda () (staged)))
                                  "M-< C-s l o o s e RET x y e s RET" (list :until (gone "loose.txt"))
                                  "M-< C-s l i n e s RET x y e s RET" (list :until (gone (format nil "two~%lines.txt")))
                                  (list :until (lambda () (null (tuatara.defaults::git-state-untracked (git-state-here)))))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "untracked  two^Jlines.txt"))
        (is equal '("*") (staged))
        (true (tuatara.io:path-kind (native-in dir "*")))
        (true (tuatara.io:path-kind (concatenate 'string (tuatara.io:home-trash) "files/loose.txt") :follow nil))
        (true (tuatara.frontend.fake:fake-frame-showing f "Moved 1 file to the trash"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Move to the trash loose.txt?"))))))

(define-test (git trashed-never-through-a-link)
  ;; a file is moved to the trash only where it really is in the repository
  (with-test-directory (dir)
    (let ((top (tuatara.io:native dir)))
      (ensure-directories-exist (merge-pathnames "real/" dir))
      (put-file (merge-pathnames "real/x.txt" dir) "x")
      (sb-posix:symlink (native-in dir "real") (native-in dir "link"))
      (true (tuatara.defaults::git-file-inside-p top "real/x.txt"))
      (true (tuatara.defaults::git-file-inside-p top "link"))
      (false (tuatara.defaults::git-file-inside-p top "link/x.txt")))))

(define-test (git no-commit-yet)
  ;; in a repository with no commit, a new file is unstaged, and moved to the trash
  (with-test-directory (dir)
    (flet ((git (&rest arguments) (apply #'git-output dir arguments)))
      (git "init" "-q" "-b" "main")
      (put-file (merge-pathnames "new.txt" dir) "new")
      (git "add" "new.txt")
      (flet ((untracked () (tuatara.defaults::git-state-untracked (git-state-here)))
             (staged () (tuatara.defaults::git-state-staged (git-state-here))))
        (let ((f (git-session dir "new.txt"
                              (list "M-< C-s n e w RET u" (list :until (lambda () (untracked)))
                                    "M-< C-s n e w RET s" (list :until (lambda () (staged)))
                                    "M-< C-s n e w RET x y e s RET"
                                    (list :until (lambda () (null (probe-file (merge-pathnames "new.txt" dir)))))
                                    (list :until (lambda () (and (null (staged)) (null (untracked)))))))))
          (true (tuatara.frontend.fake:fake-frame-showing f "(no commit yet)"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Unstaged 1 file"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Moved 1 file to the trash"))
          (false (tuatara.frontend.fake:fake-frame-showing f "fatal"))
          (is equal "" (git "ls-files")))))))

(define-test (git deleted-file-not-visited)
  ;; RET on a file deleted says so: no empty buffer that saving would make the file again
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-output dir "rm" "-q" "a.txt")
    (put-file (merge-pathnames "b.txt" dir) "b")
    (let ((f (git-session dir "b.txt" (list "M-< C-s d e l e t e d RET RET" (list :wait 0.2)))))
      (true (tuatara.frontend.fake:fake-frame-showing f "a.txt is deleted"))
      (false (tuatara.buffer:find-buffer "a.txt")))))

(define-test (git a-status-for-each-repository)
  ;; two repositories whose folders share a name have a status each, found by their top
  (with-test-directory (dir)
    (let* ((one (native-in dir "one/repo/"))
           (two (native-in dir "two/repo/"))
           (a (tuatara.defaults::git-status-buffer one))
           (b (tuatara.defaults::git-status-buffer two)))
      (unwind-protect
           (progn
             (false (eq a b))
             (is equal "*git: repo*" (tuatara.buffer:buffer-name a))
             (is equal "*git: repo*<2>" (tuatara.buffer:buffer-name b))
             (is eq a (tuatara.defaults::git-status-buffer one))
             (is equal two (tuatara.buffer:buffer-local b 'tuatara.defaults::git-top)))
        (tuatara.buffer:kill-buffer a)
        (tuatara.buffer:kill-buffer b)))))

(define-test (git passphrase-window)
  ;; on a desktop with an askpass program, ssh asks a key's passphrase in its window; else it asks nothing
  (let ((saved (mapcar (lambda (name) (cons name (uiop:getenv name))) '("SSH_ASKPASS" "DISPLAY" "WAYLAND_DISPLAY"))))
    (flet ((with-env (askpass display)
             (if askpass (sb-posix:setenv "SSH_ASKPASS" askpass 1) (sb-posix:unsetenv "SSH_ASKPASS"))
             (if display (sb-posix:setenv "DISPLAY" display 1) (sb-posix:unsetenv "DISPLAY"))
             (sb-posix:unsetenv "WAYLAND_DISPLAY")
             (tuatara.defaults::git-network-environment)))
      (unwind-protect
           (progn
             (let ((env (with-env "/bin/true" ":0")))
               (true (member "GIT_SSH_COMMAND=ssh" env :test #'string=))
               (true (member "SSH_ASKPASS_REQUIRE=force" env :test #'string=))
               ;; still no program an address names
               (true (member "GIT_CONFIG_VALUE_0=never" env :test #'string=)))
             (dolist (env (list (with-env "/bin/true" nil) (with-env "/nonexistent/askpass" ":0") (with-env nil ":0")))
               (true (member "GIT_SSH_COMMAND=ssh -o BatchMode=yes" env :test #'string=))
               (false (member "SSH_ASKPASS_REQUIRE=force" env :test #'string=))))
        (loop for (name . value) in saved
              do (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name)))))))

(define-test (git what-went-wrong)
  ;; what git said went wrong: its fatal: or error: line, else its last
  (is equal "git push: fatal: You are not currently on a branch."
      (tuatara.defaults::git-failure-words "push" (format nil "fatal: You are not currently on a branch.~%To push the history leading to the current (detached HEAD)~%state now, use~%~%    git push origin HEAD:<name-of-remote-branch>~%")))
  (is equal "git commit: nothing added to commit"
      (tuatara.defaults::git-failure-words "commit" (format nil "On branch main~%nothing added to commit~%~%")))
  (is equal "git status: it failed" (tuatara.defaults::git-failure-words "status" ""))
  ;; no key the server takes: said plainly, with how to give ssh yours
  (is equal "git push: the server took none of the ssh keys offered (Permission denied (publickey)): ssh-add puts yours in your ssh agent"
      (tuatara.defaults::git-failure-words "push" (format nil "git@github.com: Permission denied (publickey).~%fatal: Could not read from remote repository.~%~%Please make sure you have the correct access rights~%")))
  ;; a submodule's changes are its own: said so, and nothing claimed reversed
  (is equal "Left sub as it is: a submodule, whose changes are its own repository's"
      (tuatara.defaults::git-reverse-words '() '() '(("sub" "a submodule, whose changes are its own repository's"))))
  (is equal "Reversed the changes of 2 files; moved 1 file to the trash"
      (tuatara.defaults::git-reverse-words '("a" "b") '("c") '()))
  (is equal '(("sub" "submodule"))
      (tuatara.defaults::git-state-unstaged
       (tuatara.defaults::parse-git-status (format nil "1 .M S.M. 160000 160000 160000 abc abc sub~a" #\Nul)))))

(define-test (git work-is-a-job)
  ;; git's work is a job: listed, and stopped by M-x stop-job, a commit not made
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%"))
    (git-output dir "add" "a.txt")
    (let ((hook (merge-pathnames ".git/hooks/pre-commit" dir))
          (described '()))
      (put-file hook (format nil "#!/bin/sh~%sleep 30~%"))
      (sb-posix:chmod (namestring hook) #o755)
      (flet ((git-jobs () (remove-if-not #'tuatara.defaults::git-job-p tuatara.runtime:*jobs*)))
        (let ((f (git-session dir "a.txt"
                              (list "c c" (list :until (lambda () (tuatara.defaults::git-buffer-of (tuatara.defaults::repository-top (namestring dir)) 'tuatara.defaults::git-commit-mode))) "S e c o n d C-c C-c"
                                    (list :until (lambda () (setf described (mapcar #'tuatara.runtime:job-description (git-jobs)))))
                                    "M-x s t o p - j o b RET y e s RET"
                                    (list :until (lambda () (null (git-jobs))))))))
          (true (member "git: committing" described :test #'equal))
          (false (git-jobs))
          (true (tuatara.frontend.fake:fake-frame-showing f "Stopped"))
          (is equal (format nil "First~%") (git-output dir "log" "--format=%s")))))))

(define-test (git found-where-path-names-it-from-the-root)
  ;; git is run from a folder PATH names from the root, never from one relative to wherever the editor is
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((path (sb-posix:getenv "PATH")))
      (unwind-protect
           (progn (sb-posix:setenv "PATH" "bin:." 1)
                  (false (nth-value 1 (tuatara.defaults::run-git (tuatara.io:native dir) '("rev-parse")))))
        (sb-posix:setenv "PATH" path 1)))
    (true (nth-value 1 (tuatara.defaults::run-git (tuatara.io:native dir) '("rev-parse"))))))

(define-test (git hunk-patches)
  ;; a hunk's lines chosen: a change not chosen stays as it is, a line taken away
  ;; kept and one added left out, or the other way round for a patch applied backwards
  (let ((hunk '("@@ -3,4 +3,5 @@ (defun f ()" " a" "-b" "+B" "+C" " d" "\\ No newline at end of file")))
    (multiple-value-bind (old-start old-count start count) (tuatara.defaults::git-hunk-range hunk)
      (is equal '(3 4 3 5) (list old-start old-count start count)))
    (is equal hunk (tuatara.defaults::git-hunk-patch hunk))
    (is equal '("@@ -3,3 +3,4 @@" " a" " b" "+B" " d" "\\ No newline at end of file")
        (tuatara.defaults::git-hunk-patch hunk :lines '(3)))
    (is equal '("@@ -3,3 +3,2 @@" " a" "-b" " d" "\\ No newline at end of file")
        (tuatara.defaults::git-hunk-patch hunk :lines '(2)))
    (is equal '("@@ -3,3 +3,4 @@" " a" "+B" " C" " d" "\\ No newline at end of file")
        (tuatara.defaults::git-hunk-patch hunk :lines '(3) :reverse t))
    (false (tuatara.defaults::git-hunk-patch hunk :lines '(1 5)))
    ;; where a hunk's lines are in the file, and which lines it covers
    (is = 3 (tuatara.defaults::git-hunk-line hunk 1))
    (is = 4 (tuatara.defaults::git-hunk-line hunk 3))
    (is = 6 (tuatara.defaults::git-hunk-line hunk 5))
    (true (tuatara.defaults::git-hunk-covers-p hunk 5))
    (true (tuatara.defaults::git-hunk-covers-p hunk 4))
    (false (tuatara.defaults::git-hunk-covers-p hunk 6))
    (true (tuatara.defaults::git-hunk-covers-p '("@@ -4,3 +4,2 @@" " a" "-gone" " b") 5))
    (false (tuatara.defaults::git-hunk-covers-p '("@@ -4,3 +4,2 @@" " a" "-gone" " b") 3)))
  (is equal (list (format nil "diff --git a/a b/a~%") '("@@ -1 +1 @@" "-x" "+y") '("@@ -9 +9 @@" "-p" "+q"))
      (tuatara.defaults::git-parse-diff (format nil "diff --git a/a b/a~%@@ -1 +1 @@~%-x~%+y~%@@ -9 +9 @@~%-p~%+q~%")))
  (is equal '("") (tuatara.defaults::git-parse-diff "")))

(defun numbered-lines (count &rest changed)
  "COUNT lines, l1, l2 ..., but for those whose numbers CHANGED names, given in upper case, L2."
  (format nil "~{~a~%~}" (loop for i from 1 to count
                               collect (format nil "~:[l~;L~]~d" (member i changed) i))))

(define-test (git stage-hunks-and-lines)
  ;; s on a hunk stages it alone, u unstages it, x reverses it; lines selected
  ;; in a hunk are staged alone; l visits the file at the hunk's line
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (numbered-lines 20))
    (git-output dir "commit" "-q" "-am" "Twenty")
    (put-file (merge-pathnames "a.txt" dir) (numbered-lines 20 2 15))
    (flet ((shows (text) (lambda () (search text (tuatara.buffer:buffer-string (git-status-here)))))
           (staged () (git-output dir "diff" "--cached"))
           (unstaged () (git-output dir "diff")))
      (let ((f (git-session dir "a.txt"
                            (list ;; title, file, first hunk: staged alone
                                  "M-< n n TAB" (list :until (shows "+L15"))
                                  "n s" (list :until (lambda () (search "+L2" (staged))))
                                  ;; the hunk staged leaves the unstaged changes
                                  (list :until (shows "Staged changes"))
                                  ;; unstaged again from the staged section
                                  "M-< C-s S t a g e d RET n TAB" (list :until (lambda () (let ((text (tuatara.buffer:buffer-string (git-status-here))))
                                                                                    (search "+L2" text :start2 (search "Staged changes" text)))))
                                  "n u" (list :until (lambda () (zerop (length (staged)))))
                                  ;; the second hunk reversed, asked, which says it cannot be undone
                                  "M-< C-s L 1 5 RET x y e s RET" (list :until (lambda () (not (search "L15" (unstaged)))))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Reverse the hunk of a.txt? It cannot be undone"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Reversed the hunk of a.txt"))
        (is equal (numbered-lines 20 2) (alexandria:read-file-into-string (merge-pathnames "a.txt" dir)))))
    ;; two lines added, one of them selected and staged
    (put-file (merge-pathnames "a.txt" dir) (format nil "l1~%new one~%new two~%~a" (subseq (numbered-lines 20) 3)))
    (let ((f (git-session dir "a.txt"
                          (list "M-< n n TAB" (list :until (lambda () (search "+new two" (tuatara.buffer:buffer-string (git-status-here)))))
                                "M-< C-s n e w SPC o RET C-a C-SPC C-e s"
                                (list :until (lambda () (search "new one" (git-output dir "diff" "--cached"))))
                                ;; l on a hunk's line visits the file there
                                "M-< C-s n e w SPC t RET l" (list :wait 0.3)))))
      (declare (ignore f))
      (false (search "new two" (git-output dir "diff" "--cached")))
      (true (search "new two" (git-output dir "diff")))
      (let ((visited (tuatara.buffer:find-buffer "a.txt")))
        (is = 2 (tuatara.buffer:buffer-line-number visited (tuatara.buffer:buffer-point visited)))))))

(define-test (git stage-hunk-in-the-file)
  ;; M-x git-stage-hunk in a file's own buffer stages the change at point alone
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (numbered-lines 20))
    (git-output dir "commit" "-q" "-am" "Twenty")
    (put-file (merge-pathnames "a.txt" dir) (numbered-lines 20 2 15))
    (let ((tuatara.defaults::*modal-editing* nil)
          (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
      (let ((f (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "a.txt")))
                                        "M-< C-s L 1 5 RET M-x g i t - s t a g e - h u n k RET"
                                        (list :until (lambda () (search "L15" (git-output dir "diff" "--cached"))))
                                        "M-< M-x g i t - s t a g e - h u n k RET" (list :wait 0.3))
                                  :width 120)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Staged the hunk at line 15"))
        (true (tuatara.frontend.fake:fake-frame-showing f "No change at line 1 left to stage"))
        (false (search "L2" (git-output dir "diff" "--cached")))
        (true (search "L2" (git-output dir "diff")))))))

(define-test (git stage-hunk-in-a-file-of-lone-returns)
  ;; in a file whose carriage returns end lines alone, git counts lines by newlines: the
  ;; hunk staged is git's at point's line, never the one at the editor's line of that number
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((text (first last)
             (format nil "~a~%~{~a~%~}" (substitute #\Return #\| first) (append (loop for i from 2 to 11 collect (format nil "l~d" i)) (list last)))))
      (put-file (merge-pathnames "a.txt" dir) (text "h|h|h|h|h|h|h|h" "l12"))
      (git-output dir "commit" "-q" "-am" "Lone returns")
      (put-file (merge-pathnames "a.txt" dir) (text "H|h|h|h|h|h|h|h" "L12")))
    (let ((tuatara.defaults::*modal-editing* nil)
          (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
          (visit (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "a.txt")))))
      ;; l5, the editor's 12th line and git's 5th, is no change
      (let ((f (auto-save-session (list visit "M-< C-s l 5 RET M-x g i t - s t a g e - h u n k RET"
                                        (list :until (asking-said "left to stage")))
                                  :width 120)))
        (true (tuatara.frontend.fake:fake-frame-showing f "No change at line 5 left to stage"))
        (is string= "" (git-output dir "diff" "--cached")))
      (tuatara.buffer:kill-buffer (git-buffer-named "a.txt"))
      ;; L12, git's 12th line, stages its hunk alone
      (let ((f (auto-save-session (list visit "M-< C-s L 1 2 RET M-x g i t - s t a g e - h u n k RET"
                                        (list :until (lambda () (search "L12" (git-output dir "diff" "--cached")))))
                                  :width 120)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Staged the hunk at line 12"))
        (false (search "H" (git-output dir "diff" "--cached")))
        (true (search (format nil "+H~ch" #\Return) (git-output dir "diff")))))))

(defun asking-said (words)
  "A function true once the echo area says something holding WORDS."
  (lambda () (let ((message (tuatara.command:editor-message tuatara.command:*editor*)))
               (and message (search words message)))))

(defun asking (prompt)
  "A function true while the editor asks a question whose prompt holds PROMPT."
  (lambda ()
    (let ((editor tuatara.command:*editor*))
      (and editor (tuatara.command:editor-minibuffer-active editor)
           (search prompt (or (tuatara.command:editor-minibuffer-prompt editor) ""))))))

(defun git-branch-names (dir)
  "The branches of the repository DIR, as git lists them."
  (sort (remove "" (uiop:split-string (git-output dir "for-each-ref" "--format=%(refname:short)" "refs/heads") :separator '(#\Newline))
                :test #'string=)
        #'string<))

(define-test (git branch-menu)
  ;; b opens the branch menu: a new branch checked out, another checked out,
  ;; renamed, deleted; ~ shows the actions the slim menu leaves out
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((on () (string-trim '(#\Newline) (git-output dir "branch" "--show-current"))))
      (let ((f (git-session dir "a.txt"
                            (list "b" (list :wait 0.1)
                                  "c" (list :until (asking "New branch:")) "t o p i c RET"
                                  (list :until (asking "Start topic at")) "RET"
                                  (list :until (lambda () (equal "topic" (on))))
                                  "b b" (list :until (asking "Check out:")) "m a i n RET"
                                  (list :until (lambda () (equal "main" (on))))
                                  "b m" (list :until (asking "Rename branch")) "t o p i c RET"
                                  (list :until (asking "Rename topic to:")) "f e a t RET"
                                  (list :until (lambda () (equal '("feat" "main") (git-branch-names dir))))
                                  "b d" (list :until (asking "Delete branch:")) "f e a t RET"
                                  (list :until (asking "Delete the branch feat?")) "y e s RET"
                                  (list :until (lambda () (equal '("main") (git-branch-names dir))))
                                  "~ b" (list :wait 0.2) "C-g")
                            :width 120 :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Branch"))
        (true (tuatara.frontend.fake:fake-frame-showing f "b Branch or revision"))
        (true (tuatara.frontend.fake:fake-frame-showing f "c New branch"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Checked out main"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Deleted feat"))
        (true (tuatara.frontend.fake:fake-frame-showing f "s Spin off the work"))))
    ;; a branch with commits no other has is deleted only when asked again
    (git-output dir "checkout" "-q" "-b" "side")
    (put-file (merge-pathnames "side.txt" dir) "side")
    (git-output dir "add" "side.txt")
    (git-output dir "commit" "-q" "-m" "Side")
    (git-output dir "checkout" "-q" "main")
    (git-session dir "a.txt"
                 (list "b d" (list :until (asking "Delete branch:")) "s i d e RET"
                       (list :until (asking "Delete the branch side?")) "y e s RET"
                       (list :until (asking "has commits no other branch has")) "y e s RET"
                       (list :until (lambda () (equal '("main") (git-branch-names dir))))))
    (is equal '("main") (git-branch-names dir))
    ;; a name git would read as an option is never given to it
    ;; an action pushed to the slim menus by an init file, its symbol its own, is shown
    (let ((tuatara-user:*git-slim-actions* (list (intern "GIT-BRANCH-RESET" :tuatara-user)))
          (tuatara.command::*current-buffer* (tuatara.buffer:make-buffer "*slim test*")))
      (true (tuatara.defaults::git-menu-shows-p
             (find 'tuatara.defaults::git-branch-reset (tuatara.command::menu-entries (gethash 'tuatara.defaults::git-branch-menu tuatara.command::*menus*) :all t)
                   :key #'tuatara.command:menu-entry-command))))
    (false (tuatara.defaults::git-name-p "--force"))
    (false (tuatara.defaults::git-name-p "a b"))
    (true (tuatara.defaults::git-name-p "origin/main"))))

(define-test (git worktree-menu)
  ;; Z makes a worktree for a new branch, shows its status, and deletes it, asked
  (with-test-directory (dir)
    (let ((repo (merge-pathnames "repo/" dir))
          (tree (native-in dir "wt")))
      (ensure-directories-exist repo)
      (scratch-repository repo)
      (let ((tuatara.defaults::*modal-editing* nil)
            (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
        (let ((f (auto-save-session
                  (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in repo "a.txt")))
                        "C-x g" (list :until #'git-state-here)
                        "~ Z c" (list :until (asking "New branch:")) "w o r k RET"
                        (list :until (asking "Start work at")) "RET"
                        (list :until (asking "Worktree for work at:")) (format nil "C-a C-k ~a RET" (rimu-typed tree))
                        (list :until (lambda () (search "[work]" (git-output repo "worktree" "list"))))
                        "Z g" (list :until (asking "Status of worktree:")) "w t RET"
                        (list :until (lambda () (tuatara.buffer:find-buffer "*git: wt*")))
                        "Z d" (list :until (asking "Delete worktree:")) "w t RET"
                        (list :until (asking "Delete the worktree")) "y e s RET"
                        (list :until (lambda () (not (search "[work]" (git-output repo "worktree" "list"))))))
                  :width 120 :height 30)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Worktree"))
          (true (tuatara.frontend.fake:fake-frame-showing f "The new branch work checked out in"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Deleted the worktree"))
          (false (probe-file (concatenate 'string tree "/")))
          (true (member "work" (git-branch-names repo) :test #'string=)))))))

(defun git-log-subjects (dir)
  "The subjects of the commits of the repository DIR, the latest first."
  (remove "" (uiop:split-string (git-output dir "log" "--format=%s") :separator '(#\Newline)) :test #'string=))

(define-test (git commit-menu)
  ;; c c commits what is staged; -a every change; c a amends with the message
  ;; written again; c w rewords alone; c e extends; c f and c s fix up and squash
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((change (name text) (put-file (merge-pathnames name dir) text))
           (committed-p (subject) (lambda () (equal subject (first (git-log-subjects dir)))))
           (commit-buffer () (tuatara.defaults::git-buffer-of (tuatara.defaults::repository-top (namestring dir)) 'tuatara.defaults::git-commit-mode)))
      (change "a.txt" (format nil "one~%two~%"))
      (let* ((first (string-trim '(#\Newline) (git-output dir "rev-parse" "--short" "HEAD")))
             (reworded nil)
             (f (git-session dir "a.txt"
                            (list ;; nothing staged: refused, then committed with -a
                                  "c c" (list :wait 0.4)
                                  "c - a c" (list :until #'commit-buffer) "S e c o n d C-c C-c" (list :until (committed-p "Second"))
                                  ;; amended: the message written again, the new change in it
                                  (lambda () (change "a.txt" (format nil "one~%two~%three~%")) (git-output dir "add" "a.txt"))
                                  "c a" (list :until #'commit-buffer) "C-k T w o RET" (list :wait 0.1) "C-c C-c" (list :until (committed-p "Two"))
                                  ;; reworded: what is staged left out
                                  (lambda () (change "b.txt" "b") (git-output dir "add" "b.txt"))
                                  "c w" (list :until #'commit-buffer) "C-k D u e RET" (list :wait 0.1) "C-c C-c" (list :until (committed-p "Due"))
                                  (lambda () (setf reworded (git-output dir "show" "--stat" "HEAD")))
                                  ;; extended with what is staged, the message kept
                                  "c e" (list :until (lambda () (search "b.txt" (git-output dir "show" "--stat" "HEAD"))))
                                  ;; a fixup of the first commit and a squash into it
                                  (lambda () (change "c.txt" "c") (git-output dir "add" "c.txt"))
                                  ;; left out of the slim menu: said so, the menu kept open, nothing else run
                                  "c f" (list :wait 0.2) "C-g"
                                  "~ c f" (list :until (asking "Fix up commit")) (format nil "~a RET" (key-string first)) (list :until (committed-p "fixup! First"))
                                  (lambda () (change "d.txt" "d") (git-output dir "add" "d.txt"))
                                  "c s" (list :until (asking "Squash into commit")) (format nil "~a RET" (key-string first)) (list :until (committed-p "squash! First"))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Nothing staged: s stages a file"))
        (true (tuatara.frontend.fake:fake-frame-showing f "- a Every change (--all)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Fix up (f) is left out of this menu now; Slim menus:"))
        (is equal '("squash! First" "fixup! First" "Due" "First") (git-log-subjects dir))
        (is equal (format nil "one~%two~%three~%") (git-output dir "show" "HEAD~2:a.txt"))
        (false (search "b.txt" reworded))
        (true (search "b.txt" (git-output dir "show" "--stat" "HEAD~2")))))))

(define-test (git commit-message-typed-at-once)
  ;; with modal editing, a new commit's message, shown when git has read what is staged (work
  ;; from the background), begins in INSERT state, its very first key typed; amending, the
  ;; message there already, begins in NORMAL
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%"))
    (git-output dir "add" "a.txt")
    (let ((tuatara.defaults::*modal-editing* t)
          (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
          (states '()))
      (flet ((commit-buffer () (tuatara.defaults::git-buffer-of (tuatara.defaults::repository-top (namestring dir)) 'tuatara.defaults::git-commit-mode))
             (committed-p (subject) (lambda () (equal subject (first (git-log-subjects dir))))))
        (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "a.txt")))
                                 "C-x g" (list :until #'git-state-here)
                                 "c c" (list :until #'commit-buffer)
                                 (list :until (lambda () (push (tuatara.defaults::modal-state (commit-buffer)) states)))
                                 "S e c o n d C-c C-c" (list :until (committed-p "Second"))
                                 "c a" (list :until #'commit-buffer)
                                 (list :until (lambda () (push (tuatara.defaults::modal-state (commit-buffer)) states)))
                                 "C-c C-k")
                           :width 120 :height 30)
        (is equal '(:normal :insert) states)
        (is equal '("Second" "First") (git-log-subjects dir)))))
  ;; the message git in a terminal asks for, opened as a file: typed at once too
  (with-test-directory (dir)
    (let ((path (merge-pathnames "COMMIT_EDITMSG" dir))
          (tuatara.defaults::*modal-editing* t))
      (put-file path (format nil "~%# Please enter the commit message.~%"))
      (let* ((editor (nth-value 1 (run-keys "T h i r d" :file (namestring path))))
             (b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
        (is string= (format nil "Third~%# Please enter the commit message.~%") (tuatara.buffer:buffer-string b))
        (tuatara.buffer:kill-buffer b)))))

(define-test (git file-left-typing-opened-in-normal-state)
  ;; with modal editing, a file left in INSERT state for the status (C-x g works while typing)
  ;; and opened again from it with RET comes back in NORMAL state: j moves, typing nothing
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%"))
    (let ((tuatara.defaults::*modal-editing* t)
          (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
          (state nil))
      (flet ((file () (tuatara.buffer:find-buffer "a.txt")))
        (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "a.txt")))
                                 "i x" "C-x g" (list :until #'git-state-here)
                                 "M-< C-s a . t x t RET RET"
                                 (list :until (lambda () (eq (file) (tuatara.display:window-buffer
                                                                     (tuatara.command:editor-window tuatara.command:*editor*)))))
                                 (lambda () (setf state (tuatara.defaults::modal-state (file))))
                                 "j")
                           :width 120 :height 30)
        (is eq :normal state)
        (is string= (format nil "xone~%two~%") (tuatara.buffer:buffer-string (file)))))))

(define-test (git stash-menu)
  ;; z z puts changes aside with a message; the status lists the stash; RET
  ;; shows it; z p brings it back and drops it; z d drops one, asked
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((change (text) (put-file (merge-pathnames "a.txt" dir) text))
           (stashes () (git-output dir "stash" "list")))
      (change (format nil "one~%two~%"))
      (let ((f (git-session dir "a.txt"
                            (list "z z" (list :until (asking "Stash message")) "P a r k e d RET"
                                  (list :until (lambda () (search "Parked" (stashes))))
                                  (list :until (lambda () (search "Stashes (1)" (tuatara.buffer:buffer-string (git-status-here)))))
                                  "M-< C-s s t a s h @ RET RET" (list :until (lambda () (git-buffer-named "*git stash@{0}*")))
                                  (list :wait 0.1) "C-x o"
                                  "z p" (list :until (asking "Pop stash")) "RET"
                                  (list :until (lambda () (zerop (length (stashes)))))
                                  (lambda () (change (format nil "one~%three~%")))
                                  "z z" (list :until (asking "Stash message")) "RET"
                                  (list :until (lambda () (plusp (length (stashes)))))
                                  "z d" (list :until (asking "Drop stash")) "RET"
                                  (list :until (asking "It cannot be undone")) "y e s RET"
                                  (list :until (lambda () (zerop (length (stashes)))))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Put the changes aside"))
        (true (search "+two" (tuatara.buffer:buffer-string (git-buffer-named "*git stash@{0}*"))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Popped stash@{0}"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Dropped stash@{0}"))
        (is equal (format nil "one~%") (alexandria:read-file-into-string (merge-pathnames "a.txt" dir)))))))

(define-test (git log-and-diff-menus)
  ;; L L lists the commits; -g draws the graph; L f a file's; L r the reflog;
  ;; RET shows a commit; d d every change, d s what is staged, -s the counts
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%"))
    (git-output dir "commit" "-q" "-am" "Second")
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%three~%"))
    (flet ((log-text () (let ((b (tuatara.buffer:find-buffer "*git log*"))) (and b (tuatara.buffer:buffer-string b)))))
      (let ((f (git-session dir "a.txt"
                            (list "~ L L" (list :until (lambda () (search "Second" (or (log-text) ""))))
                                  (lambda () (setf (getf (symbol-plist 'log-and-diff) :plain) (log-text)))
                                  "h C-x g" (list :wait 0.2)
                                  "L - g L" (list :until (lambda () (search "* " (or (log-text) ""))))
                                  "h C-x g" (list :wait 0.2)
                                  "L r" (list :until (lambda () (search "commit (initial): First" (or (log-text) ""))))
                                  "M-< C-s i n i t i a l RET RET" (list :until (lambda () (git-buffer-named "*git commit ")))
                                  "C-x 1 C-x g" (list :wait 0.2)
                                  "d d" (list :until (lambda () (tuatara.buffer:find-buffer "*git diff*")))
                                  "C-x 1 C-x g" (list :wait 0.2)
                                  "d s" (list :wait 0.4)
                                  "d - s u" (list :until (lambda () (tuatara.buffer:find-buffer "*git diff unstaged*")))))))
        (let ((plain (getf (symbol-plist 'log-and-diff) :plain)))
          (true (search "Log of HEAD" plain))
          (true (search "First" plain))
          (true (search "(HEAD -> main) Second" plain)))
        (true (search "commit (initial): First" (log-text)))
        (true (search "+three" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*git diff*"))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Nothing staged"))
        (true (search "a.txt | 1 +" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*git diff unstaged*"))))))))

(define-test (git fetch-pull-push-menus)
  ;; a remote in a folder: P p pushes, f u fetches what another clone pushed,
  ;; F u brings it in; a forced push is asked first, and no leaves it
  (with-test-directory (dir)
    (let ((repo (merge-pathnames "repo/" dir))
          (remote (native-in dir "remote.git"))
          (other (merge-pathnames "other/" dir)))
      (ensure-directories-exist repo)
      (scratch-repository repo)
      (git-output dir "init" "-q" "--bare" "-b" "main" remote)
      (git-output repo "remote" "add" "origin" remote)
      (git-output repo "push" "-q" "-u" "origin" "main")
      (git-output dir "clone" "-q" remote (namestring other))
      (git-output other "config" "user.name" "Other")
      (git-output other "config" "user.email" "o@example.invalid")
      (git-output other "config" "commit.gpgsign" "false")
      (put-file (merge-pathnames "a.txt" repo) (format nil "one~%two~%"))
      (git-output repo "commit" "-q" "-am" "Mine")
      (flet ((remote-subjects () (git-log-subjects remote)))
        (let ((tuatara.defaults::*modal-editing* nil)
              (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
          (let ((f (auto-save-session
                    (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in repo "a.txt")))
                          "C-x g" (list :until #'git-state-here)
                          "P p" (list :until (lambda () (equal "Mine" (first (remote-subjects)))))
                          (lambda ()
                            (git-output other "pull" "-q")
                            (put-file (merge-pathnames "b.txt" other) "b")
                            (git-output other "add" "b.txt")
                            (git-output other "commit" "-q" "-m" "Theirs")
                            (git-output other "push" "-q"))
                          "f u" (list :until (lambda () (search "Theirs" (git-output repo "log" "--format=%s" "origin/main"))))
                          (list :until (lambda () (notany #'tuatara.defaults::git-job-p tuatara.runtime:*jobs*)))
                          (list :wait 0.1)
                          "F u" (list :until (lambda () (equal "Theirs" (first (git-log-subjects repo)))))
                          "P - f p" (list :until (asking "Force the push")) "n o RET" (list :wait 0.3))
                    :width 120 :height 30)))
            (true (tuatara.frontend.fake:fake-frame-showing f "Pushed"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Fetched"))
            (true (tuatara.frontend.fake:fake-frame-showing f "Pulled"))
            (is equal '("Theirs" "Mine" "First") (git-log-subjects repo))))))))

(defun git-commit-file (dir name text subject)
  "Write TEXT to the file NAME of the repository DIR and commit it with SUBJECT."
  (put-file (merge-pathnames name dir) text)
  (git-output dir "add" name)
  (git-output dir "commit" "-q" "-m" subject))

(define-test (git merge-rebase-pick-revert-reset)
  ;; m m merges a branch; one changing a line both sides stops half way, the
  ;; status says so, m a stops it; R e rebases; A A picks; _ _ reverts; X m
  ;; and X H reset, the hard one asked
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-output dir "checkout" "-q" "-b" "side")
    (git-commit-file dir "side.txt" "side" "Side file")
    (git-output dir "checkout" "-q" "main")
    (git-commit-file dir "main.txt" "main" "Main file")
    (flet ((subjects () (git-log-subjects dir))
           (status-text () (tuatara.buffer:buffer-string (git-status-here))))
      (let ((f (git-session dir "a.txt"
                            (list "m m" (list :until (asking "Merge:")) "s i d e RET"
                                  (list :until (lambda () (probe-file (merge-pathnames "side.txt" dir)))))
                            :width 120 :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Merged side"))
        (true (search "Merge branch 'side'" (first (subjects)))))
      ;; a merge stopped half way, then stopped
      (git-output dir "checkout" "-q" "-b" "clash" "HEAD~1")
      (git-commit-file dir "a.txt" (format nil "theirs~%") "Theirs")
      (git-output dir "checkout" "-q" "main")
      (git-commit-file dir "a.txt" (format nil "ours~%") "Ours")
      (let ((f (git-session dir "a.txt"
                            (list "m m" (list :until (asking "Merge:")) "c l a s h RET"
                                  (list :until (lambda () (search "Doing:    merging" (status-text))))
                                  "m a" (list :until (asking "Stop the merge?")) "y e s RET"
                                  (list :until (lambda () (not (search "Doing:" (status-text))))))
                            :width 160 :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Doing:    merging (c c commits once the files are put right and staged, m a stops it)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Stopped the merge"))
        (is equal (format nil "ours~%") (alexandria:read-file-into-string (merge-pathnames "a.txt" dir))))
      ;; a branch rebased on main, a commit picked, one reverted
      (git-output dir "checkout" "-q" "-b" "topic" "HEAD~2")
      (git-commit-file dir "topic.txt" "topic" "Topic file")
      (let ((pick (string-trim '(#\Newline) (git-output dir "rev-parse" "--short" "clash"))))
        (git-session dir "a.txt"
                     (list "R e" (list :until (asking "Rebase on")) "m a i n RET"
                           (list :until (lambda () (search "Ours" (git-output dir "log" "--format=%s"))))
                           "A A" (list :until (asking "Cherry-pick commit")) (format nil "~a RET" (key-string pick))
                           (list :until (lambda () (or (equal "Theirs" (first (subjects))) (search "Doing:    cherry" (status-text)))))))
        (true (search "Topic file" (git-output dir "log" "--format=%s")))
        ;; the pick clashed with Ours: stopped from its menu
        (git-session dir "a.txt"
                     (list (list :until (lambda () (search "Doing:    cherry-picking" (status-text))))
                           "A a" (list :until (asking "Stop the cherry-pick?")) "y e s RET"
                           (list :until (lambda () (not (search "Doing:" (status-text)))))))
        (is equal "Topic file" (first (subjects))))
      (let ((topic (string-trim '(#\Newline) (git-output dir "rev-parse" "--short" "HEAD"))))
        (git-session dir "a.txt"
                     (list "_ _" (list :until (asking "Revert commit")) (format nil "~a RET" (key-string topic))
                           (list :until (lambda () (search "Revert \"Topic file\"" (first (subjects)))))))
        (false (probe-file (merge-pathnames "topic.txt" dir)))
        ;; reset: mixed to before the revert, then hard with a change, asked
        (put-file (merge-pathnames "a.txt" dir) (format nil "changed~%"))
        (let ((f (git-session dir "a.txt"
                              (list "X m" (list :until (asking "Reset to")) "H E A D ~ 1 RET"
                                    (list :until (lambda () (equal "Topic file" (first (subjects)))))
                                    "X H" (list :until (asking "Reset to (hard)")) "RET"
                                    (list :until (asking "Changes not committed are lost")) "y e s RET"
                                    (list :until (lambda () (zerop (length (git-output dir "status" "--porcelain"))))))
                              :width 120 :height 30)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Reset to HEAD~1, the files as they were"))
          (true (probe-file (merge-pathnames "topic.txt" dir)))
          (is equal (format nil "ours~%") (alexandria:read-file-into-string (merge-pathnames "a.txt" dir))))))))

(define-test (git rebase-as-a-list-says)
  ;; R i shows git's own list of the commits after a revision; a line dropped
  ;; leaves its commit out, a reword line asks the new subject
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-commit-file dir "a.txt" (format nil "one~%A~%") "Add A")
    (git-commit-file dir "b.txt" "b" "Add B")
    (git-commit-file dir "c.txt" "c" "Add C")
    (flet ((todo () (git-buffer-named "*git rebase:"))
           (set-word (subject word)
             ;; the first word of the list's line for SUBJECT becomes WORD
             (let* ((buffer (git-buffer-named "*git rebase:"))
                    (text (tuatara.buffer:buffer-string buffer))
                    (at (search subject text))
                    (start (1+ (or (position #\Newline text :end at :from-end t) -1)))
                    (end (position #\Space text :start start)))
               (tuatara.buffer:buffer-replace buffer start end word))))
      (let ((f (git-session dir "a.txt"
                            (list "~ R i" (list :until (asking "Rebase the commits after")) "F i r s t RET"
                                  (list :until #'todo)
                                  (lambda () (set-word "Add B" "drop") (set-word "Add C" "reword"))
                                  "C-c C-c" (list :until (asking "New subject of")) "C-a C-k C SPC a d d e d RET"
                                  (list :until (lambda () (equal "C added" (first (git-log-subjects dir))))))
                            :width 120 :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "# C-c C-c rebases as these lines say"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Rebased as the list said"))
        (is equal '("C added" "Add A" "First") (git-log-subjects dir))
        (false (probe-file (merge-pathnames "b.txt" dir)))
        (true (probe-file (merge-pathnames "c.txt" dir)))))))

(define-test (git rebase-leaves-no-folder)
  ;; the folder a rebase's list keeps its files in goes when the list is given
  ;; up, and when git has rebased as it says
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-commit-file dir "b.txt" "b" "Add B")
    (let ((folders '()))
      (flet ((todo () (git-buffer-named "*git rebase:"))
             (note-folder ()
               (push (tuatara.defaults::buffer-private-folder (git-buffer-named "*git rebase:")) folders))
             (gone () (notany #'probe-file (remove nil folders))))
        (git-session dir "a.txt"
                     (list "~ R i" (list :until (asking "Rebase the commits after")) "F i r s t RET"
                           (list :until #'todo) #'note-folder
                           "C-c C-k" (list :until #'gone)
                           "~ R i" (list :until (asking "Rebase the commits after")) "F i r s t RET"
                           (list :until #'todo) #'note-folder
                           "C-c C-c" (list :until (lambda () (and (not (todo)) (gone)))))
                     :width 120 :height 30)
        (is = 2 (count-if #'identity folders))
        (true (gone))))))

(define-test (git renamed-file-followed)
  ;; a file renamed from git's file menu is followed by the buffer visiting it,
  ;; and the lists showing it are told, as they are of one deleted
  (with-test-directory (dir)
    (scratch-repository dir)
    (let* ((told '())
           (tuatara.defaults::*paths-changed-functions*
             (cons (lambda (paths) (setf told (append paths told))) tuatara.defaults::*paths-changed-functions*))
           (renamed (native-in dir "c.txt"))
           (visits nil))
      (flet ((visiting () (find-if (lambda (b) (equal (tuatara.buffer:buffer-file b) (tuatara.io:expand-path renamed)))
                                   tuatara.buffer:*buffers*)))
        (git-session dir "a.txt"
                     (list "C-x b a . t x t RET"
                           "M-x g i t - r e n a m e - f i l e RET" (list :until (asking "Rename a.txt to:"))
                           "C-a C-k c . t x t RET" (list :until (lambda () (member renamed told :test #'string=)))
                           (lambda () (setf visits (and (visiting) (tuatara.buffer:buffer-name (visiting)))))
                           "C-x b c . t x t RET"
                           "M-x g i t - d e l e t e - f i l e RET" (list :until (asking "Delete c.txt"))
                           "y e s RET" (list :until (lambda () (not (probe-file renamed))))
                           (list :until (lambda () (= 2 (count renamed told :test #'string=))))))
        (is equal "c.txt" visits)
        (is = 2 (count renamed told :test #'string=))))))

(define-test (git tags-and-remotes)
  :time-limit 900
  ;; t t tags, -a with a message; t d deletes; M a adds a remote, M r renames
  ;; it, M u gives it a new address, M d removes it; ext:: is never taken
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((tags () (git-output dir "tag" "--list" "-n1"))
           (remotes () (git-output dir "remote" "-v")))
      (let ((f (git-session dir "a.txt"
                            (list "t t" (list :until (asking "Tag name:")) "v 1 RET" (list :until (asking "Tag v1 at")) "RET"
                                  (list :until (lambda () (search "v1" (tags))))
                                  "t - a t" (list :until (asking "Tag name:")) "v 2 RET" (list :until (asking "Tag v2 at")) "RET"
                                  (list :until (asking "Message of v2:")) "S e c o n d SPC r e l e a s e RET"
                                  (list :until (lambda () (search "Second release" (tags))))
                                  "~ t d" (list :until (asking "Delete tag:")) "v 1 RET" (list :until (asking "Delete the tag v1?")) "y e s RET"
                                  (list :until (lambda () (not (search "v1 " (tags)))))
                                  "M a" (list :until (asking "Remote name")) "RET" (list :until (asking "Address of origin:"))
                                  "e x t : : s h SPC - c SPC t o u c h SPC x RET" (list :wait 0.2)
                                  "M a" (list :until (asking "Remote name")) "RET" (list :until (asking "Address of origin:"))
                                  "/ t m p / o n e . g i t RET" (list :until (lambda () (search "/tmp/one.git" (remotes))))
                                  "M r" (list :until (asking "Rename remote")) "RET" (list :until (asking "Rename origin to:")) "u p RET"
                                  (list :until (lambda () (search "up	" (remotes))))
                                  "M u" (list :until (asking "Address of remote")) "RET" (list :until (asking "Address of up:"))
                                  "C-a C-k / t m p / t w o . g i t RET" (list :until (lambda () (search "/tmp/two.git" (remotes))))
                                  ;; a folder written from ~/ is made whole
                                  "M a" (list :until (asking "Remote name")) "h o m e RET" (list :until (asking "Address of home:"))
                                  "~ / x . g i t RET" (list :until (lambda () (search "x.git" (remotes))))
                                  "M d" (list :until (asking "Remove remote")) "u p RET" (list :until (asking "Remove the remote up")) "y e s RET"
                                  (list :until (lambda () (not (search "up	" (remotes))))))
                            :width 120 :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Not an address git is given: ext::sh -c touch x"))
        (let ((url (string-trim '(#\Newline) (git-output dir "remote" "get-url" "home"))))
          (true (char= #\/ (char url 0)))
          (true (uiop:string-suffix-p url "/x.git"))
          (is equal (truename (user-homedir-pathname)) (truename (tuatara.io:parent-path url))))
        (is equal '("v2") (remove "" (uiop:split-string (git-output dir "tag" "--list") :separator '(#\Newline)) :test #'string=))
        (is equal "tag" (string-trim '(#\Newline) (git-output dir "cat-file" "-t" "v2")))
        (false (probe-file (merge-pathnames "x" dir)))))))

(define-test (git bisect-menu)
  ;; B B begins with a bad and a good revision; B b and B g, said of each
  ;; commit tried, narrow it to the first bad commit, named; B r stops
  (with-test-directory (dir)
    (scratch-repository dir)
    (dolist (n '(2 3 4 5))
      (git-commit-file dir "a.txt" (format nil "~:[good~;bad~] ~d~%" (>= n 4) n) (format nil "Change ~d" n)))
    (flet ((bad-p () (search "bad" (alexandria:read-file-into-string (merge-pathnames "a.txt" dir))))
           (status-text () (tuatara.buffer:buffer-string (git-status-here)))
           (messages () (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
      (let ((f (git-session dir "a.txt"
                            (list "~ B B" (list :until (asking "Bad revision")) "RET"
                                  (list :until (asking "Good revision")) "F i r s t RET"
                                  (list :until (lambda () (search "Doing:    bisecting" (status-text)))))
                            :width 160 :height 30)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Doing:    bisecting (B g, B b and B s say good, bad, skip; B r stops it)")))
      (loop repeat 4
            until (search "is the first" (messages))
            do (git-session dir "a.txt" (list (if (bad-p) "B b" "B g") (list :until (lambda () (search "Bisecting" (messages)))) (list :wait 0.4))))
      (true (search "is the first" (messages)))
      (true (search "first 'bad' commit: [" (git-output dir "bisect" "log")))
      (true (search "Change 4" (subseq (git-output dir "bisect" "log") (search "first 'bad' commit" (git-output dir "bisect" "log")))))
      (git-session dir "a.txt" (list "B r" (list :until (lambda () (not (search "Doing:" (status-text)))))))
      (true (search "bad 5" (alexandria:read-file-into-string (merge-pathnames "a.txt" dir)))))))

(define-test (git submodules-ignore-blame-file-menu)
  ;; o a adds a submodule, the status lists it; i t and i p ignore a file;
  ;; M-x git-blame lists who changed each line; the file menu stages the file
  (with-test-directory (dir)
    (let ((repo (merge-pathnames "repo/" dir))
          (sub (merge-pathnames "sub/" dir)))
      (ensure-directories-exist repo)
      (ensure-directories-exist sub)
      (scratch-repository repo)
      (scratch-repository sub)
      ;; a submodule from a folder, which git refuses by default: allowed by a
      ;; global settings file of the test's own, never yours
      (put-file (merge-pathnames "gitconfig" dir) (format nil "[protocol \"file\"]~%	allow = always~%[commit]~%	gpgsign = false~%"))
      (put-file (merge-pathnames "loose.log" repo) "x")
      (put-file (merge-pathnames "secret.txt" repo) "x")
      (let ((tuatara.defaults::*modal-editing* nil)
            (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
            (global (sb-posix:getenv "GIT_CONFIG_GLOBAL")))
        (sb-posix:setenv "GIT_CONFIG_GLOBAL" (native-in dir "gitconfig") 1)
        (unwind-protect
             (flet ((status-text () (tuatara.buffer:buffer-string (git-status-here))))
               (let ((f (auto-save-session
                         (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in repo "a.txt")))
                               "C-x g" (list :until #'git-state-here)
                               "o a" (list :until (asking "Address of the submodule:")) (format nil "~a RET" (rimu-typed (namestring sub)))
                               (list :until (asking "Put")) "C-a C-k l i b RET"
                               (list :until (lambda () (search "Submodules (1)" (status-text))))
                               "M-< C-s l o o s e RET i t" (list :until (asking "Ignore:")) "RET"
                               (list :until (lambda () (not (search "loose.log" (status-text)))))
                               "M-< C-s s e c r e t RET i p" (list :until (asking "Ignore:")) "RET"
                               (list :until (lambda () (not (search "secret.txt" (status-text)))))
                               "C-x b a . t x t RET M-x g i t - b l a m e RET" (list :until (lambda () (git-buffer-named "*git blame: a.txt*")))
                               "C-x b a . t x t RET" (lambda () (put-file (merge-pathnames "a.txt" repo) (format nil "one~%two~%")))
                               "M-x r e v e r t - b u f f e r RET" (list :wait 0.2)
                               "M-x g i t - f i l e - m e n u RET s" (list :until (lambda () (search "a.txt" (git-output repo "diff" "--cached" "--name-only")))))
                         :width 120 :height 30)))
                 (true (tuatara.frontend.fake:fake-frame-showing f "as recorded"))
                 (true (search "/loose.log" (alexandria:read-file-into-string (merge-pathnames ".gitignore" repo))))
                 (true (search "/secret.txt" (alexandria:read-file-into-string (merge-pathnames ".git/info/exclude" repo))))
                 (true (search "Commit   Author" (tuatara.buffer:buffer-string (git-buffer-named "*git blame: a.txt*"))))
                 (true (search "one" (tuatara.buffer:buffer-string (git-buffer-named "*git blame: a.txt*"))))
                 (true (tuatara.frontend.fake:fake-frame-showing f "Staged a.txt"))
                 (true (probe-file (merge-pathnames "lib/a.txt" repo)))))
          (if global (sb-posix:setenv "GIT_CONFIG_GLOBAL" global 1) (sb-posix:unsetenv "GIT_CONFIG_GLOBAL")))))))

(define-test (git patches-notes-sparse-refs-run)
  ;; W c writes commits as patches; w w takes one in on another branch; W s
  ;; saves the changes as a patch, W a applies it; T T notes a commit; > s
  ;; checks out one folder, > d all again; y lists the branches and tags; !
  ;; runs a git command; W b writes a bundle, W v checks it
  (with-test-directory (dir)
    (let ((repo (merge-pathnames "repo/" dir)))
      (ensure-directories-exist repo)
      (scratch-repository repo)
      (git-output repo "tag" "v1")
      (git-output repo "checkout" "-q" "-b" "work")
      (ensure-directories-exist (merge-pathnames "docs/" repo))
      (git-commit-file repo "docs/guide.txt" "guide" "Add the guide")
      (git-output repo "checkout" "-q" "main")
      (put-file (merge-pathnames "a.txt" repo) (format nil "one~%two~%"))
      (flet ((messages () (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))))
        (let ((tuatara.defaults::*modal-editing* nil)
              (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
              (tuatara-user:*git-menus* :full))
          (let ((f (auto-save-session
                    (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in repo "a.txt")))
                          "C-x g" (list :until #'git-state-here)
                          ;; the changes saved as a patch, reversed, then applied again
                          "W s" (list :until (asking "Save the changes as:")) (format nil "C-a C-k ~a RET" (rimu-typed (native-in dir "changes.patch")))
                          (list :until (lambda () (probe-file (merge-pathnames "changes.patch" dir))))
                          (lambda () (git-output repo "checkout" "--" "a.txt"))
                          "W a" (list :until (asking "Apply patch:")) (format nil "C-a C-k ~a RET" (rimu-typed (native-in dir "changes.patch")))
                          (list :until (lambda () (search "two" (alexandria:read-file-into-string (merge-pathnames "a.txt" repo)))))
                          (lambda () (git-output repo "checkout" "--" "a.txt")))
                    :width 120 :height 30)))
            (declare (ignore f)))
          ;; work's commit, as a patch, taken in on main; then written out again
          (git-output repo "format-patch" "-q" "-o" (native-in dir "out2/") "main..work")
          (let* ((patch (namestring (first (directory (merge-pathnames "out2/*.patch" dir)))))
                 (f (auto-save-session
                     (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in repo "a.txt")))
                           "C-x g" (list :until #'git-state-here)
                           "w w" (list :until (asking "Take in patch:")) (format nil "C-a C-k ~a RET" (rimu-typed patch))
                           (list :until (lambda () (equal "Add the guide" (first (git-log-subjects repo)))))
                           "W c" (list :until (asking "Commits after")) "v 1 RET"
                           (list :until (asking "Write the patches in:")) (format nil "C-a C-k ~a RET" (rimu-typed (native-in dir "out/")))
                           (list :until (lambda () (directory (merge-pathnames "out/*.patch" dir))))
                           "T T" (list :until (asking "Note on")) "RET" (list :until (asking "Note on HEAD:")) "R e v i e w e d RET"
                           (list :until (lambda () (search "Reviewed" (git-output repo "notes" "show" "HEAD"))))
                           "> s" (list :until (asking "Check out only the folders:")) "d o c s RET"
                           (list :until (lambda () (search "true" (git-output repo "config" "core.sparseCheckout"))))
                           "> d" (list :until (lambda () (not (search "true" (git-output repo "config" "core.sparseCheckout")))))
                           "y" (list :until (lambda () (git-buffer-named "*git refs:")))
                           "C-x 1 C-x g" (list :wait 0.2)
                           "!" (list :until (asking "Run git:")) "r e v - p a r s e SPC - - a b b r e v - r e f SPC H E A D RET"
                           (list :until (lambda () (git-buffer-named "*git output*")))
                           "C-x 1 C-x g" (list :wait 0.2)
                           "W b" (list :until (asking "Write the bundle as:")) (format nil "C-a C-k ~a RET" (rimu-typed (native-in dir "all.bundle")))
                           (list :until (lambda () (probe-file (merge-pathnames "all.bundle" dir))))
                           "W v" (list :until (asking "Check bundle:")) (format nil "C-a C-k ~a RET" (rimu-typed (native-in dir "all.bundle")))
                           (list :until (lambda () (search "is good" (messages))))
                           "O s" (list :until (asking "Subtree folder to split:")) "d o c s RET" (list :wait 0.5))
                     :width 120 :height 30)))
            (true (probe-file (merge-pathnames "docs/guide.txt" repo)))
            (let ((refs (tuatara.buffer:buffer-string (git-buffer-named "*git refs:"))))
              (true (search "Branches (2)" refs))
              (true (search "* main" refs))
              (true (search "Tags (1)" refs))
              (true (search "v1" refs)))
            (is equal (format nil "main~%") (tuatara.buffer:buffer-string (git-buffer-named "*git output*")))
            (true (search "all.bundle is good" (messages)))
            (true (search "Wrote 1 patch in" (messages)))
            ;; git without subtree says so, in git's words
            (true (tuatara.frontend.fake:fake-frame-showing f "subtree"))))))))

(define-test (git many-changes-shown-in-part)
  ;; a file changed in many lines shows the first *git-diff-lines* of them and
  ;; a line saying how many more; TAB there shows more
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "~{line ~d~%~}" (loop for i from 1 to 25 collect i)))
    (let ((tuatara-user:*git-diff-lines* 10))
      (flet ((status-text () (tuatara.buffer:buffer-string (git-status-here))))
        (let ((f (git-session dir "a.txt"
                              (list "M-< n n TAB" (list :until (lambda () (search "more line" (status-text))))
                                    (lambda () (setf (getf (symbol-plist 'many-changes) :first) (status-text)))
                                    "M-< C-s m o r e SPC l i n e RET TAB"
                                    (list :until (lambda () (search "+line 18" (status-text))))))))
          (declare (ignore f))
          (let ((first (getf (symbol-plist 'many-changes) :first)))
            (true (search "... 17 more lines of changes: TAB here shows 10 more" first))
            (false (search "+line 10" first)))
          (true (search "+line 18" (status-text)))
          (true (search "... 7 more lines of changes: TAB here shows 7 more" (status-text))))))))

(define-test (git untracked-folder-one-line)
  ;; a folder none of whose files git tracks is one line of Untracked files,
  ;; however many it holds; l lists the folder, s stages every file in it
  (with-test-directory (dir)
    (scratch-repository dir)
    (ensure-directories-exist (merge-pathnames "big/deep/" dir))
    (dotimes (i 50) (put-file (merge-pathnames (format nil "big/f~d.txt" i) dir) "x"))
    (put-file (merge-pathnames "big/deep/g.txt" dir) "x")
    (flet ((status-text () (tuatara.buffer:buffer-string (git-status-here)))
           (rimu () (find-if #'tuatara.defaults::rimu-list-p tuatara.buffer:*buffers*))
           (staged () (tuatara.defaults::git-state-staged (git-state-here))))
      (git-session dir "a.txt"
                   (list (lambda () (setf (getf (symbol-plist 'untracked-folder) :first) (status-text)))
                         "M-< C-s b i g RET l" (list :until #'rimu)
                         (lambda () (setf (getf (symbol-plist 'untracked-folder) :listed) (tuatara.defaults::rimu-list-directory (rimu))))
                         "C-x g" (list :until (lambda () (eq (tuatara.command:current-buffer) (git-status-here))))
                         "M-< C-s b i g RET s" (list :until (lambda () (= 51 (length (staged)))))))
      (let ((first (getf (symbol-plist 'untracked-folder) :first)))
        (true (search "Untracked files (1)" first))
        (true (search "untracked  big/" first))
        (false (search "f1.txt" first)))
      (true (uiop:string-suffix-p (getf (symbol-plist (quote untracked-folder)) :listed) "big/") "listed ~s; messages ~a" (getf (symbol-plist (quote untracked-folder)) :listed) (messages-text))
      (is = 51 (length (staged))))))

(define-test (git many-files-listed-in-part)
  ;; a section of more files than *git-section-lines* lists that many and a
  ;; line saying how many more; TAB there lists more
  (with-test-directory (dir)
    (scratch-repository dir)
    (dotimes (i 25) (put-file (merge-pathnames (format nil "f~2,'0d.txt" i) dir) "x"))
    (let ((tuatara-user:*git-section-lines* 10))
      (flet ((status-text () (tuatara.buffer:buffer-string (git-status-here))))
        (git-session dir "a.txt"
                     (list (lambda () (setf (getf (symbol-plist 'many-files) :first) (status-text)))
                           "M-< C-s m o r e SPC f i l e RET TAB"
                           (list :until (lambda () (search "f19.txt" (status-text))))))
        (let ((first (getf (symbol-plist 'many-files) :first)))
          (true (search "Untracked files (25)" first))
          (true (search "f09.txt" first))
          (false (search "f10.txt" first))
          (true (search "... 15 more files: TAB here shows 10 more" first)))
        (false (search "f20.txt" (status-text)))
        (true (search "... 5 more files: TAB here shows 5 more" (status-text)))))))

(define-test (git staged-kinds-coloured)
  ;; staged files are coloured as staged, but for a deletion
  (is eq :git-status-deleted (tuatara.defaults::git-staged-face "deleted"))
  (dolist (kind '("new file" "renamed" "modified"))
    (is eq :git-status-staged (tuatara.defaults::git-staged-face kind))))

(define-test (git status-follows-git-elsewhere)
  ;; git run from elsewhere (a terminal) changes the repository: the status
  ;; shown reads it again by itself
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((tuatara-user:*git-watch-seconds* 0.2))
      (flet ((status-text () (tuatara.buffer:buffer-string (git-status-here))))
        (git-session dir "a.txt"
                     (list (list :until (lambda () (tuatara.buffer:buffer-local (git-status-here) 'tuatara.defaults::git-stamps)))
                           (lambda () (git-commit-file dir "b.txt" "b" "Made in a terminal"))
                           (list :until (lambda () (search "Made in a terminal" (status-text))))))
        (true (search "Made in a terminal" (status-text)))))))

(define-test (git status-at-rest-asks-no-wakes)
  ;; a status shown, its repository unchanged, asks the editor to wake for
  ;; nothing: the folders of git's own files are watched on disk instead
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((tick :unset) (watched '()))
      (git-session dir "a.txt"
                   (list (list :until (lambda () (tuatara.buffer:buffer-local (git-status-here) 'tuatara.defaults::git-stamps)))
                         (list :wait 0.3)
                         (lambda ()
                           (setf tick (tuatara.defaults::git-watch-tick tuatara.command:*editor*)
                                 watched (tuatara.defaults::git-watched-folders)))))
      (false tick)
      (true (find-if (lambda (folder) (uiop:string-suffix-p folder "/.git/")) watched)))))

(define-test (git shown-text-closes-with-q)
  ;; a commit shown in a buffer of its own is read in SUPPORT state, and closes
  ;; with q
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((tuatara.defaults::*modal-editing* t)
          (tuatara-user:*lsp-trusted-projects* (list (namestring dir))))
      (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "a.txt")))
                               "C-x g" (list :until #'git-state-here)
                               "d c" (list :until (asking "Show commit")) "RET"
                               (list :until (lambda () (git-buffer-named "*git commit ")))
                               (list :wait 0.2)
                               (lambda () (setf (getf (symbol-plist 'shown-text) :class)
                                                (tuatara.defaults::buffer-modal-class (git-buffer-named "*git commit "))))
                               "q" (list :wait 0.2)
                               (lambda () (setf (getf (symbol-plist 'shown-text) :after)
                                                (tuatara.buffer:buffer-name (tuatara.command:current-buffer)))))
                         :width 100 :height 24)
      (is eq :support (getf (symbol-plist 'shown-text) :class))
      (false (uiop:string-prefix-p "*git commit " (getf (symbol-plist 'shown-text) :after))))))

(define-test (git ignored-files-folded)
  ;; the files git ignores are under Ignored files, folded: TAB on the heading
  ;; lists them (a folder ignored whole as one line) and folds them again; s
  ;; stages none of them
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames ".gitignore" dir) (format nil "*.log~%build/~%"))
    (git-output dir "add" ".gitignore")
    (git-output dir "commit" "-q" "-m" "Ignore")
    (put-file (merge-pathnames "debug.log" dir) "x")
    (ensure-directories-exist (merge-pathnames "build/deep/" dir))
    (put-file (merge-pathnames "build/deep/out.o" dir) "x")
    (flet ((status-text () (tuatara.buffer:buffer-string (git-status-here))))
      (let ((f (git-session dir "a.txt"
                            (list (list :until (lambda () (search "▸ Ignored files" (status-text))))
                                  (lambda () (setf (getf (symbol-plist 'ignored-folded) :folded) (status-text)))
                                  "M-< C-s I g n o r e d SPC f RET TAB" (list :until (lambda () (search "build/" (status-text))))
                                  (lambda () (setf (getf (symbol-plist 'ignored-folded) :open) (status-text)))
                                  "C-s d e b u g RET s" (list :wait 0.2)
                                  "TAB" (list :until (lambda () (not (search "debug.log" (status-text)))))))))
        (let ((folded (getf (symbol-plist 'ignored-folded) :folded))
              (open (getf (symbol-plist 'ignored-folded) :open)))
          (true (< (search "Recent commits" folded) (search "▸ Ignored files" folded)))
          (true (search "▾ Ignored files (2)" open))
          (false (search "debug.log" folded))
          (true (search "ignored    build/" open))
          (true (search "ignored    debug.log" open))
          (false (search "out.o" open)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Ignored by git: it stages none of these"))
        (is equal "" (git-output dir "diff" "--cached" "--name-only"))))))

(define-test (git sections-headed-and-folded-as-every-list)
  ;; the status and the list of references head their sections as every list
  ;; in sections does, ▾ Title (N), and fold them alike: TAB or RET on a
  ;; heading, S-TAB all of them
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "b.txt" dir) "new")
    (let ((seen '()))
      (flet ((status-text () (tuatara.buffer:buffer-string (git-status-here)))
             (refs-text () (let ((refs (git-buffer-named "*git refs: "))) (if refs (tuatara.buffer:buffer-string refs) "")))
             (keep (name text) (lambda () (push (cons name (funcall text)) seen))))
        (git-session dir "a.txt"
                     (list (list :until (lambda () (search "▾ Recent commits (1)" (status-text))))
                           (keep :open #'status-text)
                           "M-< C-s R e c e n t RET TAB" (list :until (lambda () (search "▸ Recent commits (1)" (status-text))))
                           (keep :recent-folded #'status-text)
                           "S-TAB" (list :until (lambda () (search "▸ Untracked files (1)" (status-text))))
                           (keep :all-folded #'status-text)
                           "S-TAB" (list :until (lambda () (search "▾ Untracked files (1)" (status-text))))
                           (keep :all-open #'status-text)
                           (keep :folds (lambda () (tuatara.defaults::list-folded (git-status-here))))
                           "y" (list :until (lambda () (search "▾ Branches (1)" (refs-text))))
                           "TAB" (list :until (lambda () (search "▸ Branches (1)" (refs-text))))
                           (keep :refs-folded #'refs-text)
                           "RET" (list :until (lambda () (search "* main" (refs-text))))
                           (keep :refs-open #'refs-text))))
      (flet ((text (name) (cdr (assoc name seen))))
        (true (search "▾ Untracked files (1)" (text :open)))
        (true (search "First" (text :open) :start2 (search "▾ Recent commits" (text :open))))
        (false (search "First" (text :recent-folded) :start2 (search "▸ Recent commits" (text :recent-folded))))
        (true (search "b.txt" (text :recent-folded)))
        (false (search "b.txt" (text :all-folded)))
        (true (search "▸ Recent commits (1)" (text :all-folded)))
        (true (search "b.txt" (text :all-open)))
        (true (search "▾ Recent commits (1)" (text :all-open)))
        ;; the files ignored, none here, stay folded for when there are some
        (is equal '(:ignored) (text :folds))
        (false (search "main" (text :refs-folded)))
        (true (search "▾ Branches (1)" (text :refs-open)))))))

(define-test (git commands-only-where-they-work)
  ;; the commands of a status, a log, a blame, a list of references, a commit's
  ;; message and a rebase's list run there alone, and M-x offers them nowhere else
  (loop for (mode . commands) in '((tuatara.defaults::git-status-mode tuatara.defaults::git-stage tuatara.defaults::git-unstage
                                    tuatara.defaults::git-reverse tuatara.defaults::git-toggle-diff tuatara.defaults::git-next-section
                                    tuatara.defaults::git-previous-section tuatara.defaults::git-visit tuatara.defaults::git-toggle-full-menus)
                                   (tuatara.defaults::git-log-mode tuatara.defaults::git-log-show)
                                   (tuatara.defaults::git-blame-mode tuatara.defaults::git-blame-show)
                                   (tuatara.defaults::git-refs-mode tuatara.defaults::git-refs-log)
                                   (tuatara.defaults::git-commit-mode tuatara.defaults::git-commit-finish tuatara.defaults::git-commit-cancel)
                                   (tuatara.defaults::git-rebase-mode tuatara.defaults::git-rebase-todo-finish tuatara.defaults::git-rebase-todo-cancel))
        do (dolist (command commands)
             (true (assoc mode (gethash command tuatara.defaults::*mode-commands*)) "~a only in ~a" command mode)))
  (let ((refusal nil) (offered :unset))
    (auto-save-session (list (lambda ()
                               (setf refusal (tuatara.defaults::mode-command-refusal 'tuatara.defaults::git-stage)
                                     offered (find 'tuatara.defaults::git-stage (tuatara.defaults::command-candidates)
                                                   :key #'tuatara.command:candidate-result)))))
    (true (search "works in a repository's status" refusal))
    (false offered)))

(define-test (git git-run-safety)
  ;; git stash runs git again on :/, which --literal-pathspecs would spoil: it
  ;; is run without it, and -u takes the files not tracked away; the paths
  ;; given to other commands stay literal
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "loose.txt" dir) "loose")
    (true (nth-value 1 (tuatara.defaults::run-git (tuatara.io:native dir) (list "stash" "push" "-u" "-m" "Parked"))))
    (false (probe-file (merge-pathnames "loose.txt" dir)))
    (true (search "Parked" (git-output dir "stash" "list")))
    (true (nth-value 1 (tuatara.defaults::run-git (tuatara.io:native dir) (list "stash" "pop"))))
    (true (probe-file (merge-pathnames "loose.txt" dir)))
    (is equal '("--no-lazy-fetch" "-c" "core.fsmonitor=false" "-c" "log.showSignature=false" "-c" "core.pager=cat")
        (tuatara.defaults::git-safety '("stash" "push")))
    (true (member "--literal-pathspecs" (tuatara.defaults::git-safety '("add" "--" "*")) :test #'string=))
    ;; more than the limit: nothing given, and said too large
    (multiple-value-bind (octets ok too-large) (tuatara.defaults::run-git (tuatara.io:native dir) (list "log") :limit 10)
      (false octets) (false ok) (true too-large))))

(define-test (git lines-after-no-newline-staged)
  ;; lines added after a last line with no newline, staged alone: the line
  ;; before them gains its newline, never joined to them
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-commit-file dir "nonl.txt" (format nil "l1~%l2~%l3") "No newline")
    (put-file (merge-pathnames "nonl.txt" dir) (format nil "l1~%l2~%l3~%l4~%l5"))
    (let* ((diff (tuatara.defaults::git-parse-diff (git-output dir "diff" "--no-color" "--" "nonl.txt")))
           (hunk (second diff))
           (l4 (position "+l4" hunk :test #'string=))
           (patch (tuatara.defaults::git-patch (first diff) (list (tuatara.defaults::git-hunk-patch hunk :lines (list l4))))))
      (uiop:run-program (list "git" "-C" (namestring dir) "apply" "--cached" "--whitespace=nowarn")
                        :input (make-string-input-stream patch))
      (is equal (format nil "l1~%l2~%l3~%l4~%") (git-output dir "show" ":nonl.txt"))
      ;; and unstaged again, applied backwards from what is staged
      (let* ((staged (tuatara.defaults::git-parse-diff (git-output dir "diff" "--cached" "--no-color" "--" "nonl.txt")))
             (hunk (second staged))
             (patch (tuatara.defaults::git-patch (first staged)
                                                 (list (tuatara.defaults::git-hunk-patch
                                                        hunk :lines (list (position "+l4" hunk :test #'string=)) :reverse t)))))
        (uiop:run-program (list "git" "-C" (namestring dir) "apply" "--cached" "--reverse" "--whitespace=nowarn")
                          :input (make-string-input-stream patch))
        (is equal (format nil "l1~%l2~%l3~%") (git-output dir "show" ":nonl.txt"))))))

(define-test (git rebase-list-subjects)
  ;; git writes its list as pick HASH # SUBJECT, an empty commit's ending # empty
  (is equal '("abc1234" "Real subject")
      (multiple-value-list (tuatara.defaults::git-reword-line-p "reword abc1234 # Real subject")))
  (is equal '("abc1234" "Multi subject")
      (multiple-value-list (tuatara.defaults::git-reword-line-p "r abc1234 # Multi subject # empty")))
  (is equal '("abc1234" "Plain one")
      (multiple-value-list (tuatara.defaults::git-reword-line-p "reword abc1234 Plain one")))
  (false (tuatara.defaults::git-reword-line-p "pick abc1234 # Real subject"))
  ;; a hash git would read as an option is no reword: never handed to git log
  (false (tuatara.defaults::git-reword-line-p "reword --output=/tmp/x s")))

(define-test (git staged-rename-both-names)
  ;; a rename staged shows both names; u unstages it whole and x undoes it,
  ;; the old name back as committed, the new one gone
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-commit-file dir "b.txt" (format nil "bee~%") "Bee")
    (git-output dir "mv" "a.txt" "a moved.txt")
    (git-output dir "mv" "b.txt" "b moved.txt")
    (let ((f (git-session dir "a moved.txt"
                          (list (list :until (lambda () (search "a.txt -> a moved.txt" (tuatara.buffer:buffer-string (git-status-here)))))
                                "M-< C-s b SPC m o v e d RET u"
                                (list :until (lambda () (search "?? \"b moved.txt\"" (git-output dir "status" "--short"))))
                                "M-< C-s a SPC m o v e d RET x" (list :until (asking "It cannot be undone")) "y e s RET"
                                (list :until (lambda () (probe-file (merge-pathnames "a.txt" dir))))
                                (list :wait 0.3))
                          :width 140)))
      (true (tuatara.frontend.fake:fake-frame-showing f "renamed    a.txt -> a moved.txt"))
      (is equal "" (git-output dir "diff" "--cached" "--name-only" "--" "b.txt" "b moved.txt"))
      (is equal (format nil "one~%") (alexandria:read-file-into-string (merge-pathnames "a.txt" dir)))
      (false (probe-file (merge-pathnames "a moved.txt" dir))))))

(define-test (git ignore-patterns-escaped)
  ;; a name holding what a pattern reads otherwise is written as itself, and a
  ;; line there already is not added again
  (is equal "a\\[1\\].txt" (tuatara.defaults::git-ignore-escaped "a[1].txt"))
  (is equal "back\\\\slash" (tuatara.defaults::git-ignore-escaped "back\\slash"))
  (is equal "star\\*\\?" (tuatara.defaults::git-ignore-escaped "star*?"))
  (is equal "trail\\ \\ " (tuatara.defaults::git-ignore-escaped "trail  "))
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((file (native-in dir ".gitignore")))
      (tuatara.defaults::git-add-ignore-pattern file "/a\\[1\\].txt" (tuatara.io:native dir))
      (tuatara.defaults::git-add-ignore-pattern file "/a\\[1\\].txt" (tuatara.io:native dir))
      (is equal (format nil "/a\\[1\\].txt~%") (alexandria:read-file-into-string file))
      (put-file (sb-ext:parse-native-namestring (native-in dir "a[1].txt")) "x")
      (put-file (merge-pathnames "a1.txt" dir) "x")
      (is equal (format nil "?? .gitignore~%?? a1.txt~%") (git-output dir "status" "--short"))
      ;; git's own exclude file, its folder not there yet: both made
      (uiop:delete-directory-tree (merge-pathnames ".git/info/" dir) :validate t :if-does-not-exist :ignore)
      (tuatara.defaults::git-add-ignore-pattern (native-in dir ".git/info/exclude") "/x" (tuatara.io:native dir))
      (is equal (format nil "/x~%") (alexandria:read-file-into-string (merge-pathnames ".git/info/exclude" dir)))
      ;; a name holding \\ and * offered from its folder as written, not as a Lisp pathname reads it
      (sb-int:encapsulate 'tuatara.defaults::git-file-here 'ignore-test (lambda (f top) (declare (ignore f top)) "sub/a*\\b.txt"))
      (unwind-protect
           (is equal "/a\\*\\\\b.txt"
               (tuatara.defaults::git-ignore-pattern (tuatara.io:native dir) (concatenate 'string (tuatara.io:native dir) "sub/")))
        (sb-int:unencapsulate 'tuatara.defaults::git-file-here 'ignore-test)))))

(define-test (git conflict-staged-whole)
  ;; a hunk of a file in conflict is not made a patch: said plainly that the
  ;; file is staged whole once put right
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-output dir "checkout" "-q" "-b" "side")
    (git-commit-file dir "a.txt" (format nil "side~%") "Side")
    (git-output dir "checkout" "-q" "main")
    (git-commit-file dir "a.txt" (format nil "mainline~%") "Main")
    (git-output dir "merge" "-q" "side")
    (let ((f (git-session dir "a.txt"
                          (list "M-< C-s b o t h SPC c h RET TAB"
                                (list :until (lambda () (search "@@@" (tuatara.buffer:buffer-string (git-status-here)))))
                                "C-s @ @ @ RET s" (list :wait 0.3))
                          :width 140)))
      (true (tuatara.frontend.fake:fake-frame-showing f "a.txt is in conflict: put it right"))
      (false (tuatara.frontend.fake:fake-frame-showing f "git apply")))))

(define-test (git buffers-stand-at-the-top)
  ;; a git buffer stands in its repository's top: files are asked for from there
  (with-test-directory (dir)
    (let* ((top (native-in dir "repo/"))
           (status (tuatara.defaults::git-status-buffer top)))
      (unwind-protect (is equal top (tuatara.command:buffer-directory status))
        (tuatara.buffer:kill-buffer status)))))

(define-test (git no-commit-yet-diff)
  ;; d d in a repository with no commit yet shows the changes, against nothing
  (with-test-directory (dir)
    (git-output dir "init" "-q" "-b" "main")
    (put-file (merge-pathnames "f.txt" dir) (format nil "first~%"))
    (git-output dir "add" "f.txt")
    (let ((f (git-session dir "f.txt" (list "d d" (list :until (lambda () (tuatara.buffer:find-buffer "*git diff*")))))))
      (false (tuatara.frontend.fake:fake-frame-showing f "bad revision"))
      (true (search "+first" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*git diff*")))))))

(define-test (git no-commit-yet-file-unstaged)
  ;; the file menu's unstage works in a repository with no commit yet, as the
  ;; status's does
  (with-test-directory (dir)
    (git-output dir "init" "-q" "-b" "main")
    (put-file (merge-pathnames "f.txt" dir) (format nil "first~%"))
    (git-output dir "add" "f.txt")
    (flet ((staged () (git-output dir "diff" "--cached" "--name-only")))
      (git-session dir "f.txt" (list "C-x b f . t x t RET" "M-x g i t - u n s t a g e - f i l e RET"
                                     (list :until (lambda () (string= "" (string-trim '(#\Newline) (staged)))))))
      (is string= "" (string-trim '(#\Newline) (staged))))))

(define-test (git hashes-told-one-way)
  ;; a commit's hash, whole or cut short, told by one test: the packages' and the logs'
  (true (tuatara.defaults::git-hash-p (make-string 40 :initial-element #\a)))
  (false (tuatara.defaults::git-hash-p "abc1234"))
  (true (tuatara.defaults::git-hash-p "abc1234" :short t))
  (false (tuatara.defaults::git-hash-p "abc12" :short t))
  (false (tuatara.defaults::git-hash-p "abc123g" :short t))
  (is string= "abc1234" (tuatara.defaults::package-commit-name (format nil "abc1234~%") "here"))
  (fail (tuatara.defaults::package-commit-name "not-a-hash" "here"))
  ;; a path as an address writes it, one way for a file: address and the trash
  (is string= "file:///a%20b/c~-._%25" (tuatara.defaults::path-uri "/a b/c~-._%"))
  (is string= "/a%20b/c~-._%25" (tuatara.io:url-path "/a b/c~-._%")))

(define-test (git run-words-split)
  ;; the words of ! are split as a shell splits them, though no shell runs
  (is equal '("log" "-1" "--format=[%s] two words")
      (tuatara.io:command-words "log -1 \"--format=[%s] two words\""))
  (is equal '("a b" "c\"d" "e f" "$HOME")
      (tuatara.io:command-words "'a b' \"c\\\"d\" e\\ f $HOME"))
  (is equal '(nil nil) (subseq (multiple-value-list (tuatara.io:command-words "log 'open")) 0 2))
  (is equal '(nil t) (multiple-value-list (tuatara.io:command-words "   "))))

(define-test (git pull-detached)
  ;; a pull with HEAD detached says so, git not run
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-commit-file dir "b.txt" "b" "Second")
    (git-output dir "checkout" "-q" "HEAD~1")
    (let ((f (git-session dir "a.txt" (list "F u" (list :wait 0.4)))))
      (true (tuatara.frontend.fake:fake-frame-showing f "HEAD is detached: no branch to pull into")))))

(define-test (git binary-changes-said)
  (true (tuatara.defaults::git-diff-binary-p
         (tuatara.defaults::git-parse-diff (format nil "diff --git a/b.dat b/b.dat~%index 1..2 100644~%Binary files a/b.dat and b/b.dat differ~%"))))
  (false (tuatara.defaults::git-diff-binary-p (tuatara.defaults::git-parse-diff (format nil "diff --git a/a b/a~%@@ -1 +1 @@~%-a~%+b~%")))))

(define-test (git refs-tags-and-columns)
  ;; an annotated tag is listed at its commit, with its subject; a wide name
  ;; keeps the columns
  (is = (tuatara.defaults::text-columns (tuatara.defaults::git-refs-line "" "main" "abc1234" "" "x"))
      (tuatara.defaults::text-columns (tuatara.defaults::git-refs-line "" "機能ブランチ" "abc1234" "" "x")))
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-output dir "tag" "-a" "v1" "-m" "Release one")
    (let ((commit (string-trim '(#\Newline) (git-output dir "rev-parse" "--short" "HEAD"))))
      (git-session dir "a.txt" (list "y" (list :until (lambda () (git-buffer-named "*git refs: "))) (list :wait 0.2)))
      (let ((text (tuatara.buffer:buffer-string (git-buffer-named "*git refs: "))))
        (true (search (format nil "v1~22@a ~a" "" commit) text))
        (true (search "First" text :start2 (search "v1" text)))))))

(define-test (git lines-counted-are-changes)
  ;; lines selected and staged are counted by the changes among them
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%"))
    (git-output dir "commit" "-q" "-am" "Two")
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%TWO~%"))
    (let ((f (git-session dir "a.txt"
                          (list "M-< C-s m o d i f i e d RET TAB"
                                (list :until (lambda () (search "+TWO" (tuatara.buffer:buffer-string (git-status-here)))))
                                "M-< C-s - t w o RET C-p C-a C-SPC C-n C-n C-e s"
                                (list :until (lambda () (plusp (length (git-output dir "diff" "--cached"))))))
                          :width 140)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Staged 2 lines of a.txt"))
      (false (tuatara.frontend.fake:fake-frame-showing f "Staged 3 lines")))))

(define-test (git status-window-keeps-its-place)
  ;; a status in a window not selected, read again, keeps its point on its
  ;; entry and its first line
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%"))
    (put-file (merge-pathnames "b.txt" dir) "b")
    (let ((tuatara-user:*git-watch-seconds* 0.2) (entry nil))
      (progn
          (git-session dir "a.txt"
                       (list "M-< C-s m o d i f i e d RET" (list :wait 0.1)
                             "C-x 2 C-x o C-x b a . t x t RET" (list :wait 0.2)
                             (lambda () (git-output dir "add" "a.txt"))
                             (list :until (lambda () (tuatara.defaults::git-state-staged (git-state-here))))
                             (list :wait 0.2)
                             (lambda ()
                               (let ((window (find (git-status-here) (tuatara.command:editor-windows tuatara.command:*editor*)
                                                   :key #'tuatara.display:window-buffer)))
                                 (setf entry (list (tuatara.defaults::list-entry-at (git-status-here) (tuatara.display:window-point window))
                                                   (tuatara.display:window-start window))))))
                       :height 40)
        (is equal '(:file :staged "a.txt") (first entry))
        (is = 0 (second entry))))))

(define-test (git list-point-follows-its-entry)
  ;; a list made again with entries alike but new keeps point on its entry
  (let ((buffer (tuatara.modes:find-or-make-buffer "*git list test*")))
    (unwind-protect
         (progn
           (tuatara.defaults::render-pieces buffer (list (format nil "a~%b~%c~%")) :entries (vector (list :x "a") (list :x "b") (list :x "c")))
           (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-goto-line buffer 2))
           (tuatara.defaults::render-pieces buffer (list (format nil "b~%c~%")) :entries (vector (list :x "b") (list :x "c")))
           (is equal '(:x "c") (tuatara.defaults::list-entry-at buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (git log-how-many)
  ;; = n in the log menu asks how many commits, a whole number
  (with-test-directory (dir)
    (scratch-repository dir)
    (git-commit-file dir "b.txt" "b" "Second")
    (git-session dir "a.txt" (list "L = n" (list :until (asking "How many")) "1 RET" "L"
                                   (list :until (lambda () (tuatara.buffer:find-buffer "*git log*")))))
    (let ((text (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*git log*"))))
      (true (search "Second" text))
      (false (search "First" text)))))

(define-test (git rewind-through-history)
  ;; M-x git-rewind shows a file as its latest commit left it; p goes a commit
  ;; older, n newer, back across a rename, point kept on its line; r takes one
  ;; from its log, c shows the commit, C-c w copies its hash; M-x git-rewind
  ;; again goes back to the file as it is, on the same line
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((git (&rest arguments) (apply #'git-output dir arguments))
           (put (name text) (alexandria:write-string-into-file text (merge-pathnames name dir) :if-exists :supersede))
           (rewind () (git-buffer-named "*git rewind: b.txt*"))
           (rewind-text () (let ((b (git-buffer-named "*git rewind: b.txt*"))) (and b (tuatara.buffer:buffer-string b)))))
      (put "a.txt" (format nil "one~%two~%"))
      (git "commit" "-q" "-am" "Second")
      (git "mv" "a.txt" "b.txt")
      (git "commit" "-q" "-m" "Third")
      (put "b.txt" (format nil "one~%two~%three~%four~%"))
      (git "commit" "-q" "-am" "Fourth")
      (put "b.txt" (format nil "one~%not committed~%"))
      (let ((tuatara.defaults::*modal-editing* t)
            (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
            (fourth (string-trim '(#\Newline) (git "log" "-1" "--format=%h")))
            (seen '()))
        (multiple-value-bind (f editor)
            (auto-save-session
             (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "b.txt")))
                   "j j M-x g i t - r e w i n d RET"
                   (list :until (lambda () (equal (rewind-text) (format nil "one~%two~%three~%four~%"))))
                   (lambda () (push (tuatara.buffer:buffer-line-number (rewind) (tuatara.buffer:buffer-point (rewind))) seen))
                   "p" (list :until (lambda () (equal (rewind-text) (format nil "one~%two~%"))))
                   "p p" (list :until (lambda () (equal (rewind-text) (format nil "one~%"))))
                   "p" (list :wait 0.2)
                   (lambda () (push (search "first commit of the file" (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*"))) seen))
                   "n" (list :until (lambda () (equal (rewind-text) (format nil "one~%two~%"))))
                   (lambda () (push (tuatara.buffer:buffer-line-number (rewind) (tuatara.buffer:buffer-point (rewind))) seen))
                   "r" (list :until (asking "Rewind to:")) "F o u r t h RET"
                   (list :until (lambda () (equal (rewind-text) (format nil "one~%two~%three~%four~%"))))
                   "C-c w" (list :wait 0.1)
                   (lambda () (push (tuatara.defaults::git-rewind-mode-line nil (rewind)) seen)
                                (push (tuatara.defaults::git-rewind-mode-line-place nil (rewind)) seen))
                   "c" (list :until (lambda () (git-buffer-named (format nil "*git commit ~a" fourth))))
                   ;; C-x G, as M-x git-rewind, back to now
                   "q C-x G" (list :until (lambda () (null (rewind))))
                   (lambda () (let ((b (tuatara.command:current-buffer)))
                                (push (list (tuatara.buffer:buffer-name b)
                                            (tuatara.buffer:buffer-line-number b (tuatara.buffer:buffer-point b)))
                                      seen))))
             :width 120 :height 30)
          (declare (ignore f))
          (is equal fourth (first (tuatara.command:editor-kill-ring editor)))
          (true (git-buffer-named (format nil "*git commit ~a" fourth)))
          (destructuring-bind (line end line-after mode-line place back) (reverse seen)
            (is = 2 line)
            (true end)
            ;; kept as far as the shortest went
            (is = 1 line-after)
            (is equal (list (format nil " Rewind ~a " fourth) :mode-line-rewind) mode-line)
            (is equal (format nil "~a 1/4" (subseq (git "log" "-1" "--format=%ad" "--date=short") 0 10)) place)
            ;; back in the file as it is, on the line the history was on
            (is equal '("b.txt" 1) back)))))))

(define-test (git rewind-folder)
  ;; in rimu M-x git-rewind lists the folder as its commits left it, in a rimu
  ;; list of its own: p goes a commit older, l into a folder as it was then, h
  ;; up, RET on a file through its history from there, q back to the list;
  ;; what works on the files as they are now is refused; M-x git-rewind again
  ;; goes back to the folder now, the histories ended
  (with-test-directory (dir)
    (scratch-repository dir)
    (flet ((git (&rest arguments) (apply #'git-output dir arguments))
           (put (name text)
             (ensure-directories-exist (merge-pathnames name dir))
             (alexandria:write-string-into-file text (merge-pathnames name dir) :if-exists :supersede))
           (rewound () (find-if #'tuatara.defaults::rimu-rewound-p tuatara.buffer:*buffers*))
           (text (b) (and b (tuatara.buffer:buffer-string b))))
      (put "a.txt" (format nil "one~%two~%"))
      (put "c.txt" (format nil "kept~%"))
      (put "sub/s.txt" (format nil "inner~%"))
      (git "add" "c.txt" "sub/s.txt")
      (git "commit" "-q" "-am" "Second")
      (git "rm" "-q" "-r" "a.txt" "sub")
      (put "c.txt" (format nil "kept and changed~%"))
      (git "commit" "-q" "-am" "Delete a and sub")
      (let ((tuatara.defaults::*modal-editing* t)
            (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
            (seen '()))
        (auto-save-session
         (list (format nil "C-x d C-a C-k ~a C-j" (rimu-typed (namestring dir)))
               (list :wait 0.3) "M-x g i t - r e w i n d RET"
               (list :until (lambda () (search "c.txt" (or (text (rewound)) ""))))
               (lambda () (push (text (rewound)) seen)
                          (push (tuatara.defaults::git-rewind-mode-line nil (rewound)) seen))
               "p" (list :until (lambda () (search "a.txt" (or (text (rewound)) ""))))
               (lambda () (push (text (rewound)) seen))
               "C" (list :wait 0.2)
               (lambda () (push (tuatara.command:editor-message tuatara.command:*editor*) seen))
               "g g C-s s u b RET l" (list :until (lambda () (search "s.txt" (or (text (rewound)) ""))))
               "h" (list :until (lambda () (search "a.txt" (or (text (rewound)) ""))))
               "g g C-s a . t x t RET RET"
               (list :until (lambda () (equal (text (git-buffer-named "*git rewind: a.txt*")) (format nil "one~%two~%"))))
               "q" (list :wait 0.2)
               (lambda () (push (eq (tuatara.command:current-buffer) (rewound)) seen))
               "M-x g i t - r e w i n d RET" (list :until (lambda () (null (rewound))))
               (lambda () (push (list (git-buffer-named "*git rewind: a.txt*")
                                      (tuatara.defaults::rimu-list-p (tuatara.command:current-buffer)))
                                seen)))
         :width 120 :height 30)
        (destructuring-bind (now mode-line then refused back-to-list back) (reverse seen)
          (true (search "c.txt" now))
          (false (search "a.txt" now))
          (is equal :mode-line-rewind (second mode-line))
          (true (search "a.txt" then))
          (true (search "sub" then))
          (true (search "rimu-copy works on the files as they are now" (format nil "~a" refused)))
          (true back-to-list)
          (is equal '(nil t) back))))))

(define-test (git rewind-folder-reads-git)
  ;; a folder as a commit left it is read from git, not the disk: its entries,
  ;; their kinds and sizes, and a file's text then, for the preview
  (with-test-directory (dir)
    (scratch-repository dir)
    (sb-posix:symlink "a.txt" (namestring (merge-pathnames "l" dir)))
    (git-output dir "add" "l")
    (git-output dir "commit" "-q" "-m" "Link")
    (alexandria:write-string-into-file (format nil "changed now~%") (merge-pathnames "a.txt" dir) :if-exists :supersede)
    (let* ((top (namestring dir))
           (hash (string-trim '(#\Newline) (git-output dir "rev-parse" "HEAD")))
           (listing (tuatara.defaults::rimu-read-at-commit top hash top))
           (names (map 'list #'tuatara.io:listed-name (tuatara.defaults::rimu-listing-entries listing)))
           (a (find "a.txt" (tuatara.defaults::rimu-listing-entries listing) :key #'tuatara.io:listed-name :test #'string=))
           (read nil))
      (is equal '("." ".." "a.txt" "l") names)
      ;; where a link leads, read from git
      (is string= "a.txt" (tuatara.io:listed-target (find "l" (tuatara.defaults::rimu-listing-entries listing)
                                                          :key #'tuatara.io:listed-name :test #'string=)))
      (is = 4 (tuatara.io:stamp-size (tuatara.io:listed-stamp a)))
      (is eq :regular (tuatara.io:stamp-kind (tuatara.io:listed-stamp a)))
      (tuatara.defaults::rimu-call-with-entry (cons top hash) top (tuatara.text:string-octets "a.txt")
                                              (lambda (fd path size)
                                                (declare (ignore path))
                                                (setf read (tuatara.text:octets-string (tuatara.io:read-fd-start fd "a.txt" size 100)))))
      (is equal (format nil "one~%") read)
      (true (tuatara.defaults::rimu-listing-reason (tuatara.defaults::rimu-read-at-commit top hash (concatenate 'string top "gone/")))))))

(define-test (git rewind-log-read)
  ;; each commit of a file's log, with the name it had then; one naming none left out
  (flet ((record (hash short date author subject &optional name)
           (format nil "~c~{~a~^~c~}~c~@[~%~a~c~]" (code-char #x1e)
                   (list hash (code-char #x1f) short (code-char #x1f) date (code-char #x1f) author (code-char #x1f)
                         "2 days ago" (code-char #x1f) "" (code-char #x1f) subject)
                   (code-char 0) name (code-char 0))))
    (let* ((h1 (make-string 40 :initial-element #\a)) (h3 (make-string 40 :initial-element #\c))
           (revisions (tuatara.defaults::git-file-history
                       (concatenate 'string (record h1 "s1" "2026-10-15" "Ann" "Four" "b.txt")
                                    (record (make-string 40 :initial-element #\b) "s2" "2026-10-14" "Bo" "Merge")
                                    (record h3 "s3" "2026-10-13" "Bo" "One" "a.txt")))))
      (is = 2 (length revisions))
      (let ((first (svref revisions 0)))
        (is equal (list h1 "s1" "2026-10-15" "Ann" "2 days ago" "Four" "b.txt")
            (list (tuatara.defaults::git-commit-hash first) (tuatara.defaults::git-commit-short first)
                  (tuatara.defaults::git-commit-date first) (tuatara.defaults::git-commit-author first)
                  (tuatara.defaults::git-commit-ago first) (tuatara.defaults::git-commit-subject first)
                  (tuatara.defaults::git-commit-path first))))
      (is equal "a.txt" (tuatara.defaults::git-commit-path (svref revisions 1))))
    ;; a subject holding git's separators makes no commit of its own, whose hash would go to git
    (let ((revisions (tuatara.defaults::git-file-history
                      (record (make-string 40 :initial-element #\d) "s4" "2026-10-12" "Eve"
                              (format nil "Hi~c--output=/tmp/x~cs~cd~ca~cb~cc~cd" (code-char #x1e) (code-char #x1f) (code-char #x1f)
                                      (code-char #x1f) (code-char #x1f) (code-char #x1f) (code-char #x1f))
                              "c.txt"))))
      (true (every (lambda (revision) (tuatara.defaults::git-hash-p (tuatara.defaults::git-commit-hash revision))) revisions))
      (false (find "--output=/tmp/x" revisions :key #'tuatara.defaults::git-commit-hash :test #'equal))))
  ;; one format for every log: a graph's lines kept, each commit's graph before it
  (let* ((hash (make-string 40 :initial-element #\e))
         (fields (format nil "~a~c~a~c2026-10-11~cAnn~c3 days ago~cHEAD -> main~cA subject" hash (code-char #x1f) "eeeeeee"
                         (code-char #x1f) (code-char #x1f) (code-char #x1f) (code-char #x1f) (code-char #x1f)))
         (commits (tuatara.defaults::parse-git-commits
                   (format nil "* ~c~a~%|\\~%| * ~c~a~%" (code-char #x1e) fields (code-char #x1e) fields))))
    (is = 3 (length commits))
    (is equal "* " (tuatara.defaults::git-commit-graph (first commits)))
    (is equal "HEAD -> main" (tuatara.defaults::git-commit-names (first commits)))
    (false (tuatara.defaults::git-commit-hash (second commits)))
    (is equal "|\\" (tuatara.defaults::git-commit-graph (second commits)))
    (is equal "| * " (tuatara.defaults::git-commit-graph (third commits)))
    (is equal "A subject" (tuatara.defaults::git-commit-subject (third commits)))))

(define-test (git rewind-binary-and-untracked)
  ;; a file that is not text, and one git does not keep, are said so, the
  ;; window left showing the file; one renamed in what is staged goes through
  ;; the history of the name it had
  (with-test-directory (dir)
    (scratch-repository dir)
    (with-open-file (out (merge-pathnames "bin.dat" dir) :direction :output :element-type '(unsigned-byte 8))
      (write-sequence #(1 0 2 0 3) out))
    (git-output dir "add" "bin.dat")
    (git-output dir "commit" "-q" "-m" "Binary")
    (alexandria:write-string-into-file (format nil "new~%") (merge-pathnames "new.txt" dir))
    (git-output dir "mv" "a.txt" "r ü.txt")
    (let ((tuatara.defaults::*modal-editing* nil)
          (tuatara-user:*lsp-trusted-projects* (list (namestring dir)))
          (seen '()))
      (flet ((said () (or (tuatara.command:editor-message tuatara.command:*editor*) ""))
             (note () (push (list (tuatara.command:editor-message tuatara.command:*editor*)
                                  (tuatara.buffer:buffer-name (tuatara.command:current-buffer)))
                            seen)))
        (auto-save-session
         (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "bin.dat")))
               "M-x g i t - r e w i n d RET" (list :until (lambda () (search "not text" (said)))) #'note
               (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "new.txt")))
               "M-x g i t - r e w i n d RET" (list :until (lambda () (search "no history" (said)))) #'note
               (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (native-in dir "r ü.txt")))
               "M-x g i t - r e w i n d RET"
               (list :until (lambda () (let ((b (git-buffer-named "*git rewind: r ü.txt*")))
                                         (and b (tuatara.buffer:buffer-local b 'tuatara.defaults::git-rewind-revisions)))))
               (lambda () (push (tuatara.buffer:buffer-string (git-buffer-named "*git rewind: r ü.txt*")) seen)))
         :width 120 :height 30)
        (destructuring-bind (binary untracked renamed) (reverse seen)
          (true (search "bin.dat is not text: M-x git-log-file lists its commits" (first binary)))
          (is string= "bin.dat" (second binary))
          (true (search "new.txt has no history in git" (first untracked)))
          (is string= "new.txt" (second untracked))
          (false (git-buffer-named "*git rewind: bin.dat*"))
          (false (git-buffer-named "*git rewind: new.txt*"))
          (true (plusp (length renamed))))))))

(define-test (git output-capped)
  ;; what git prints is read up to the programs' limit, and saying so beyond it, a line at a time too
  (with-test-directory (dir)
    (scratch-repository dir)
    (let ((top (tuatara.io:native dir)))
      (is equal (format nil "one~%")
          (tuatara.text:lenient-string (tuatara.defaults::run-git top (list "show" "HEAD:a.txt"))))
      (let ((tuatara-user:*program-output-limit* 2))
        (is equal '(nil nil t) (multiple-value-list (tuatara.defaults::run-git top (list "show" "HEAD:a.txt"))))
        (is equal '(nil nil t) (multiple-value-list (tuatara.defaults::run-git top (list "show" "HEAD:a.txt")
                                                                               :line (lambda (line) (declare (ignore line)) nil))))))))

(define-test (git hunk-changes-counted-once)
  ;; the changes among the lines chosen of a hunk, each line looked at once
  (is = 2 (tuatara.defaults::hunk-changes-among '(" a" "+b" "-c" " d") '(0 1 2 3 9)))
  (flet ((seconds (n)
           (let ((hunk (loop for i below n collect (if (evenp i) "+x" " y")))
                 (numbers (loop for i below n collect i))
                 (start (get-internal-real-time)))
             (tuatara.defaults::hunk-changes-among hunk numbers)
             (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    (let ((each (/ (- (seconds 40000) (seconds 10000)) 30000)))
      (true (< each 0.000005) "~,2f µs for each more line" (* each 1000000)))))

(define-test (git settings-are-the-tests-own)
  ;; git run by the tests reads a settings file of their own, never the developer's
  ;; ~/.gitconfig or the system's
  (let ((origins (uiop:run-program (list "git" "config" "--list" "--show-origin" "--global")
                                   :output :lines :ignore-error-status t))
        (own (namestring *test-state-directory*)))
    (true origins)
    (dolist (line origins)
      (true (search own line) "~a" line))
    (is string= (namestring (merge-pathnames "gitconfig" *test-state-directory*)) (sb-posix:getenv "GIT_CONFIG_GLOBAL"))
    (is string= "1" (sb-posix:getenv "GIT_CONFIG_NOSYSTEM"))))

(define-test (git warnings-kept-apart)
  ;; git warns of a folder it cannot read on its error output: what it prints is read alone,
  ;; every file and the commit there, the warning taken for none of them
  (with-test-directory (dir)
    (scratch-repository dir)
    (dolist (name '("b.txt" "c.txt" "x/in.txt"))
      (ensure-directories-exist (merge-pathnames name dir))
      (alexandria:write-string-into-file (format nil "new~%") (merge-pathnames name dir)))
    (let ((locked (namestring (merge-pathnames "x/" dir))))
      (sb-posix:chmod locked 0)
      (unwind-protect
           (let* ((top (namestring dir))
                  (state (catch 'tuatara.defaults::git-failed
                           (tuatara.defaults::git-read-state (tuatara.defaults::%make-hash-job :what "git") top
                                                             (concatenate 'string top ".git/"))))
                  (untracked (mapcar #'first (tuatara.defaults::git-state-untracked state))))
             (true (member "b.txt" untracked :test #'string=))
             (true (member "c.txt" untracked :test #'string=))
             (false (find-if (lambda (path) (search "warning" path)) untracked))
             (true (tuatara.defaults::git-hash-p (tuatara.defaults::git-state-commit state)))
             (is equal "main" (tuatara.defaults::git-state-branch state)))
        (sb-posix:chmod locked #o700)))))

(define-test (git status-read-in-one-walk)
  ;; the repository read for its status walks its tree once, the files ignored found by that
  ;; same walk, and runs git no more than needed: no stash list with no stash
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames ".gitignore" dir) (format nil "*.log~%build/~%"))
    (git-output dir "add" ".gitignore")
    (git-output dir "commit" "-q" "-m" "Ignore")
    (put-file (merge-pathnames "debug.log" dir) "x")
    (ensure-directories-exist (merge-pathnames "build/deep/" dir))
    (put-file (merge-pathnames "build/deep/out.o" dir) "x")
    (let ((runs '()) (top (namestring dir)))
      (sb-int:encapsulate 'tuatara.defaults::run-git-program 'count-runs
                          (lambda (function git directory arguments &rest keys)
                            (push (first arguments) runs)
                            (apply function git directory arguments keys)))
      (unwind-protect
           (let ((state (catch 'tuatara.defaults::git-failed
                          (tuatara.defaults::git-read-state (tuatara.defaults::%make-hash-job :what "git") top
                                                            (concatenate 'string top ".git/")))))
             (is equal '("build/" "debug.log") (sort (mapcar #'first (tuatara.defaults::git-state-ignored state)) #'string<))
             (is equal 1 (count "status" runs :test #'string=))
             (true (<= (length runs) 3) "git ran ~d times: ~{~a~^, ~}" (length runs) (reverse runs)))
        (sb-int:unencapsulate 'tuatara.defaults::run-git-program 'count-runs)))))

(define-test (git log-filtered-by-a-column)
  ;; the log is a table as the other lists are: a filter tries its pattern on one column
  (with-test-directory (dir)
    (scratch-repository dir)
    (put-file (merge-pathnames "a.txt" dir) (format nil "one~%two~%"))
    (git-output dir "commit" "-q" "-am" "Second, after First")
    (flet ((log-buffer () (tuatara.buffer:find-buffer "*git log*"))
           (shown () (shown-lines (tuatara.buffer:find-buffer "*git log*"))))
      (when (log-buffer) (tuatara.buffer:kill-buffer (log-buffer)))
      (git-session dir "a.txt"
                   (list "~ L L" (list :until (lambda () (and (log-buffer) (search "Second" (tuatara.buffer:buffer-string (log-buffer))))))
                         "C-x / f S u b j e c t RET ^ F i r s t RET"
                         (list :until (lambda () (notany (lambda (line) (search "Second" line)) (shown))))))
      (is equal '("Commit" "Date" "Author" "Subject") (mapcar #'first (tuatara.defaults::list-table-columns (log-buffer))))
      (true (find-if (lambda (line) (search "First" line)) (shown)))
      (false (find-if (lambda (line) (search "Second" line)) (shown))))))

(define-test (git message-git-asked-for)
  ;; Tuatara run by git commit as its editor: C-c C-c saves the message and ends
  ;; the editor, so that git goes on; C-c C-k ends it with 1, so that git makes nothing
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "COMMIT_EDITMSG" dir))))
      (alexandria:write-string-into-file (format nil "~%# Please enter the commit message.~%") file)
      (let ((editor (nth-value 1 (run-keys "F i r s t SPC c o m m i t C-c C-c x y z" :file file :width 80 :height 10))))
        (false (tuatara.command:editor-running editor))
        (is = 0 tuatara.command:*exit-code*)
        ;; the keys after it never ran
        (is string= (format nil "First commit~%# Please enter the commit message.~%") (file-text file)))
      (alexandria:write-string-into-file (format nil "~%# Please enter the commit message.~%") file :if-exists :supersede)
      (forget-file-buffers)
      (let ((editor (nth-value 1 (run-keys "N o p e C-c C-k" :file file :width 80 :height 10))))
        (false (tuatara.command:editor-running editor))
        (is = 1 tuatara.command:*exit-code*)
        (is string= (format nil "~%# Please enter the commit message.~%") (file-text file))))))
