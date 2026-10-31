;;;; lisp/git/branches.lisp --- Branches and worktrees: made, checked out, renamed, reset, deleted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status b opens the branch menu and Z the worktree menu, as Magit's
;;;; do; each action is a command of its own, run from M-x too, in the
;;;; repository of the status or of the current file. Names offered are read
;;;; from git in the background before the question is asked; a name git
;;;; would read as an option (beginning with -) is never given to it.

(in-package #:tuatara.defaults)

;;; Branches

(defun git-ask-branch (prompt top then &key remote default)
  "Ask with PROMPT for a branch of the repository at TOP (with REMOTE those
of its remotes offered too), DEFAULT when nothing is typed (:CURRENT for the
branch checked out), then call THEN with the name and the branch checked
out."
  (git-ask "Reading the branches" top
           (lambda (job top) (values (git-branches job top :remote remote) (git-current-branch job top)))
           (lambda (names current)
             (let ((name (git-read-name prompt names :default (if (eq default :current) current default))))
               (when name (funcall then name current))))))

(define-command git-checkout ()
  "Check out a branch or a revision, asked for: a branch by its name, else a
revision (a commit, a tag, origin/main), which leaves HEAD detached. Git
refuses when changes not committed would be lost, and says so."
  (with-git-repository (top)
    (git-ask-branch "Check out: " top
                    (lambda (name current)
                      (declare (ignore current))
                      (git-run "Checking out" top (list "checkout" name "--") (format nil "Checked out ~a" name)))
                    :remote t)))

(defun git-ask-new-branch (verb top then)
  "Ask for the name of a new branch of the repository at TOP and the
revision it starts from (the branch checked out when nothing is typed), the
prompts beginning with VERB, then call THEN with both."
  (git-ask "Reading the branches" top #'git-branches-and-head
           (lambda (names current)
             (let ((name (git-read-name (format nil "~a branch: " verb) '())))
               (when name
                 (let ((start (git-read-name (format nil "Start ~a at: " name) names :default current)))
                   (when start (funcall then name start))))))))

(define-command git-branch-and-checkout ()
  "Make a new branch, asked for, starting at a revision asked for (the branch
checked out when nothing is typed), and check it out."
  (with-git-repository (top)
    (git-ask-new-branch "New" top
                        (lambda (name start)
                          (git-run "Making a branch" top (list "checkout" "-b" name start "--")
                                   (format nil "On the new branch ~a, from ~a" name start))))))

(define-command git-branch-create ()
  "Make a new branch, asked for, starting at a revision asked for, without
checking it out."
  (with-git-repository (top)
    (git-ask-new-branch "Make" top
                        (lambda (name start)
                          (git-run "Making a branch" top (list "branch" name start)
                                   (format nil "Made the branch ~a, from ~a" name start))))))

(define-command git-branch-spin-off ()
  "Make a new branch, asked for, of the work on the branch checked out and
check it out: the commits the branch has that its upstream has not go with
the new branch, and the branch goes back to where it and its upstream
parted; changes not committed come along. Without an upstream the branch
stays as it is."
  (with-git-repository (top)
    (git-ask "Reading the branch" top
             (lambda (job top)
               (let ((current (git-current-branch job top)))
                 (values current
                         (and current
                              (nth-value 1 (git-job-try job top (list "rev-parse" "--verify" "-q" (format nil "~a@{upstream}" current))))
                              (git-output-line job top (list "merge-base" current (format nil "~a@{upstream}" current)))))))
             (lambda (current base)
               (if (null current)
                   (message "HEAD is detached: no branch to spin off from")
                   (let ((name (git-read-name (format nil "Spin off from ~a as: " current) '())))
                     (when name
                       (start-git-job "Spinning off"
                                      (lambda (job)
                                        (git-job-output job top (list "checkout" "-b" name "--"))
                                        (when base
                                          (git-job-output job top (list "update-ref" "-m" (format nil "spin-off: moving to ~a's upstream" current)
                                                                        (format nil "refs/heads/~a" current) base))))
                                      (lambda () (message "On ~a, its work spun off from ~a~:[~;, which went back to its upstream~]" name current base))
                                      :after (lambda () (git-refresh-repository top))))))))))

(define-command git-branch-rename ()
  "Rename a branch, asked for (the one checked out when nothing is typed), to a name asked for."
  (with-git-repository (top)
    (git-ask-branch "Rename branch: " top
                    (lambda (old current)
                      (declare (ignore current))
                      (let ((new (git-read-name (format nil "Rename ~a to: " old) '())))
                        (when new
                          (git-run "Renaming a branch" top (list "branch" "-m" old new) (format nil "Renamed ~a to ~a" old new)))))
                    :default :current)))

(define-command git-branch-reset ()
  "Move a branch, asked for, to a revision asked for, after asking: the
branch checked out keeps the changes not committed (git refuses when they
would be lost); its commits after the revision stay in git's reflog."
  (with-git-repository (top)
    (git-ask-branch "Reset branch: " top
                    (lambda (branch current)
                      (git-ask "Reading the branches" top
                               (lambda (job top) (git-branches job top :remote t))
                               (lambda (names)
                                 (let ((to (git-read-name (format nil "Reset ~a to: " branch) names)))
                                   (when (and to (yes-or-no-p* (format nil "Move ~a to ~a?" branch to)))
                                     (git-run "Resetting a branch" top
                                              (if (equal branch current)
                                                  (list "reset" "--keep" to "--")
                                                  (list "branch" "-f" branch to))
                                              (format nil "Moved ~a to ~a" branch to)))))))
                    :default :current)))

(defun git-delete-asking-again (doing top arguments refusal forced question done kept failed)
  "Run git with ARGUMENTS for the repository at TOP as a job doing DOING; when
git refuses, saying REFUSAL, ask QUESTION and, with yes, run it with FORCED
instead; DONE is said once one has run, KEPT when the answer is no, FAILED
when git failed otherwise. The status is read again."
  (start-git-job doing
                 (lambda (job)
                   (multiple-value-bind (text ok said) (git-job-try job top arguments :errors t)
                     (declare (ignore text))
                     (values ok (search refusal said))))
                 (lambda (ok refused)
                   (cond (ok (message "~a" done))
                         ((and refused (yes-or-no-p* question)) (git-run doing top forced done))
                         (refused (message "~a" kept))
                         (t (message "~a" failed))))
                 :after (lambda () (git-refresh-repository top))))

(define-command git-branch-delete ()
  "Delete a branch, asked for, not the one checked out: one with commits no
other branch has is deleted only after asking again, as they would be found
only in git's reflog then."
  (with-git-repository (top)
    (git-ask-branch "Delete branch: " top
                    (lambda (branch current)
                      (cond ((equal branch current) (message "~a is checked out: check out another first" branch))
                            ((yes-or-no-p* (format nil "Delete the branch ~a?" branch))
                             (git-delete-asking-again "Deleting a branch" top (list "branch" "-d" branch) "not fully merged"
                                                      (list "branch" "-D" branch)
                                                      (format nil "~a has commits no other branch has; delete it all the same?" branch)
                                                      (format nil "Deleted ~a" branch) (format nil "~a kept" branch)
                                                      (format nil "git branch: ~a could not be deleted" branch))))))))

(define-command git-branch-set-upstream ()
  "Set the upstream of a branch, asked for (the one checked out when nothing
is typed), to a branch of a remote asked for: the one pull brings from and
push sends to, and the status counts ahead and behind of."
  (with-git-repository (top)
    (git-ask-branch "Upstream of: " top
                    (lambda (branch current)
                      (declare (ignore current))
                      (git-ask "Reading the branches" top
                               (lambda (job top) (git-branches job top :remote t :local nil))
                               (lambda (names)
                                 (let ((upstream (git-read-name (format nil "Upstream of ~a: " branch) names)))
                                   (when upstream
                                     (git-run "Setting the upstream" top
                                              (list "branch" (format nil "--set-upstream-to=~a" upstream) branch)
                                              (format nil "~a follows ~a" branch upstream)))))))
                    :default :current)))

(define-menu git-branch-menu (:title "Branch" :show git-menu-shows-p :note git-menu-note)
  "Open the branch menu: check out a branch or a revision, make one (checked
out or not, or spun off with the work not pushed), rename, move, delete one,
or set its upstream. \\`C-h' there lists every action."
  ("Check out"
   ("b" "Branch or revision" git-checkout)
   ("c" "New branch" git-branch-and-checkout)
   ("s" "Spin off the work" git-branch-spin-off))
  ("Make"
   ("n" "New branch, not checked out" git-branch-create))
  ("Change"
   ("m" "Rename" git-branch-rename)
   ("x" "Move to a revision" git-branch-reset)
   ("d" "Delete" git-branch-delete)
   ("u" "Set upstream" git-branch-set-upstream)))

;;; Worktrees

(defun parse-git-worktrees (text)
  "TEXT, what git worktree list --porcelain printed, as a list of (PATH
BRANCH), BRANCH NIL for a detached HEAD or a bare repository, the main
worktree first."
  (let ((trees '()) (path nil) (branch nil))
    (flet ((done () (when path (push (list path branch) trees)) (setf path nil branch nil)))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (cond ((uiop:string-prefix-p "worktree " line) (done) (setf path (subseq line 9)))
              ((uiop:string-prefix-p "branch refs/heads/" line) (setf branch (subseq line 18)))))
      (done))
    (nreverse trees)))

(defun git-worktrees (job top)
  "On JOB's thread: the worktrees of the repository at TOP (PARSE-GIT-WORKTREES)."
  (parse-git-worktrees (git-job-output job top (list "worktree" "list" "--porcelain"))))

(defun git-worktree-candidates (trees)
  "TREES, worktrees as PARSE-GIT-WORKTREES gives them, as candidates of a question: the path, the branch beside it."
  (mapcar (lambda (tree) (make-candidate (abbreviated-path (first tree))
                                         :value (first tree) :annotation (or (second tree) "(detached)")))
          trees))

(defun git-ask-worktree (prompt top then &key others)
  "Ask with PROMPT for a worktree of the repository at TOP, with OTHERS not
the main one, then call THEN with its path; said so when there is none."
  (git-ask "Reading the worktrees" top #'git-worktrees
           (lambda (trees)
             (let ((trees (if others (rest trees) trees)))
               (if (null trees)
                   (message "No worktree but the main one")
                   (let ((path (completing-read prompt (git-worktree-candidates trees) :require-match t)))
                     (when (plusp (length path)) (funcall then (string-right-trim "/" path)))))))))

(defun git-worktree-default-path (top branch)
  "Where a worktree for BRANCH of the repository at TOP goes when nothing is
typed: beside the repository's folder, named after it and the branch."
  (let ((folder (string-right-trim "/" top)))
    (format nil "~a-~a/" folder (substitute #\- #\/ branch))))

(defun git-read-new-folder (prompt &key initial)
  "Ask with PROMPT for a folder that is not there yet, from INITIAL when
given (READ-NEW-NATIVE-PATH); its path without its last /, or NIL, said so."
  (let ((path (apply #'read-new-native-path prompt :kind :any :writing t (and initial (list :initial initial)))))
    (and path (string-right-trim "/" path))))

(defun git-read-worktree-path (branch top)
  "Ask where the worktree for BRANCH goes (GIT-WORKTREE-DEFAULT-PATH offered),
as GIT-READ-NEW-FOLDER asks."
  (git-read-new-folder (format nil "Worktree for ~a at: " branch)
                       :initial (abbreviated-path (git-worktree-default-path top branch))))

(define-command git-worktree-checkout ()
  "Make a worktree for a branch asked for, in a folder asked for: the
branch checked out there, to work on beside this one."
  (with-git-repository (top)
    (git-ask-branch "Worktree for branch: " top
                    (lambda (branch current)
                      (declare (ignore current))
                      (let ((path (git-read-worktree-path branch top)))
                        (when path
                          (git-run "Making a worktree" top (list "worktree" "add" path branch)
                                   (format nil "~a checked out in ~a" branch (abbreviated-path path)))))))))

(define-command git-worktree-branch ()
  "Make a new branch, asked for, starting at a revision asked for, and a
worktree for it in a folder asked for."
  (with-git-repository (top)
    (git-ask-new-branch "New" top
                        (lambda (name start)
                          (let ((path (git-read-worktree-path name top)))
                            (when path
                              (git-run "Making a worktree" top (list "worktree" "add" "-b" name path start)
                                       (format nil "The new branch ~a checked out in ~a" name (abbreviated-path path)))))))))

(define-command git-worktree-move ()
  "Move a worktree, asked for, not the main one, to a folder asked for."
  (with-git-repository (top)
    (git-ask-worktree "Move worktree: " top
                      (lambda (path)
                        (let ((to (git-read-new-folder (format nil "Move ~a to: " (abbreviated-path path)))))
                          (when to
                            (git-run "Moving a worktree" top (list "worktree" "move" path to)
                                     (format nil "Moved the worktree to ~a" (abbreviated-path to))))))
                      :others t)))

(define-command git-worktree-delete ()
  "Delete a worktree, asked for, not the main one, after asking: its folder
goes, its branch stays. One with changes not committed is deleted only after
asking again, as they would be lost."
  (with-git-repository (top)
    (git-ask-worktree "Delete worktree: " top
                      (lambda (path)
                        (let ((shown (abbreviated-path path)))
                          (when (yes-or-no-p* (format nil "Delete the worktree ~a and its folder? Its branch stays" shown))
                            (git-delete-asking-again "Deleting a worktree" top (list "worktree" "remove" path) "modified or untracked"
                                                     (list "worktree" "remove" "--force" path)
                                                     (format nil "~a has changes not committed; delete it all the same? They cannot be got back" shown)
                                                     (format nil "Deleted the worktree ~a" shown) "Worktree kept"
                                                     (format nil "git worktree: ~a could not be deleted" shown)))))
                      :others t)))

(define-command git-worktree-status ()
  "Show the status of a worktree, asked for, as \\[git] shows a repository's."
  (with-git-repository (top)
    (git-ask-worktree "Status of worktree: " top
                      (lambda (path) (git-show-status (concatenate 'string path "/"))))))

(define-menu git-worktree-menu (:title "Worktree" :show git-menu-shows-p :note git-menu-note)
  "Open the worktree menu: make a worktree for a branch, or for a new one,
show a worktree's status, move or delete one. \\`C-h' there lists every action."
  ("Make"
   ("b" "For a branch" git-worktree-checkout)
   ("c" "For a new branch" git-worktree-branch))
  ("Act on one"
   ("g" "Show its status" git-worktree-status)
   ("m" "Move" git-worktree-move)
   ("d" "Delete" git-worktree-delete)))

(defun install-git-branches ()
  "Bind the branch menu to b and the worktree menu to Z in a status, and the
switch between slim and full menus to ~."
  (define-keys *git-status-keymap* '(("b" git-branch-menu) ("Z" git-worktree-menu) ("~" git-toggle-full-menus))))
