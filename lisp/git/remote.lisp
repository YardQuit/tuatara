;;;; lisp/git/remote.lisp --- Fetching, pulling and pushing, each from a menu of its own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status f opens the fetch menu, F the pull menu and P the push menu,
;;;; as Magit's do: from or to the branch's upstream, from or to a remote asked
;;;; for, every remote, tags. Git never asks for a password on the terminal
;;;; (the editor's) and never runs a program an address names; the keys and
;;;; credentials it needs come from your agent and helpers, and on a desktop
;;;; ssh asks for a key's passphrase in a window of its own. A pull with no
;;;; switch only moves the branch on (fast-forward); a forced push is asked
;;;; first.

(in-package #:tuatara.defaults)

(defun ssh-askpass-window-p ()
  "True when ssh can ask for a passphrase in a window: a desktop is running
\(DISPLAY or WAYLAND_DISPLAY) and SSH_ASKPASS names a program that is there,
as gnome-ssh-askpass or ksshaskpass."
  (let ((askpass (uiop:getenv "SSH_ASKPASS")))
    (and (uiop:getenvp "SSH_ASKPASS")
         (or (uiop:getenvp "DISPLAY") (uiop:getenvp "WAYLAND_DISPLAY"))
         (tuatara.io:find-program askpass)
         t)))

(defun git-network-environment (&rest settings)
  "What git is given to fetch or push, with SETTINGS, each (KEY VALUE), of its
configuration besides: never to run a program an address of the ext:: kind
names, nor the ssh a repository's own configuration names. The keys and
credentials it needs come from your agent and helpers; a key's passphrase
the agent does not hold, or a server not yet known, is asked in ssh's own
window on a desktop (SSH-ASKPASS-WINDOW-P), never on the terminal, and with
no desktop ssh asks nothing and fails."
  (let ((settings (cons '("protocol.ext.allow" "never") settings)))
    (append (if (ssh-askpass-window-p)
                (list "GIT_SSH_COMMAND=ssh" "SSH_ASKPASS_REQUIRE=force")
                (list "GIT_SSH_COMMAND=ssh -o BatchMode=yes"))
            (list (format nil "GIT_CONFIG_COUNT=~d" (length settings)))
            (loop for (key value) in settings
                  for i from 0
                  collect (format nil "GIT_CONFIG_KEY_~d=~a" i key)
                  collect (format nil "GIT_CONFIG_VALUE_~d=~a" i value)))))

(defun git-over-network (doing top arguments done)
  "Run git with ARGUMENTS for the repository at TOP over the network, as a job
doing DOING, saying DONE once it has (GIT-RUN), and that it works in the background meanwhile."
  (git-run doing top arguments done :environment (git-network-environment))
  (say-in-background doing))

(defun git-ask-remote (prompt top then)
  "Ask with PROMPT for a remote of the repository at TOP (origin first when
it has one), then call THEN with its name; said so when it has none."
  (git-ask "Reading the remotes" top
           (lambda (job top) (git-lines job top (list "remote")))
           (lambda (remotes)
             (if (null remotes)
                 (message "No remote: M-x git-remote-add adds one")
                 (let ((name (git-read-name prompt remotes :require-match t
                                                           :default (if (member "origin" remotes :test #'string=) "origin" (first remotes)))))
                   (when name (funcall then name)))))))

;;; Fetching

(define-command git-fetch ()
  "Fetch the commits of the branch's upstream remote (or origin) that this
repository has not, the branches checked out left as they are."
  (with-git-repository (top switches)
    (git-over-network "Fetching" top (list* "fetch" switches) "Fetched")))

(define-command git-fetch-all ()
  "Fetch from every remote."
  (with-git-repository (top switches)
    (git-over-network "Fetching from every remote" top (list* "fetch" "--all" switches) "Fetched from every remote")))

(define-command git-fetch-remote ()
  "Fetch from a remote asked for."
  (with-git-repository (top switches)
    (git-ask-remote "Fetch from: " top
                    (lambda (remote)
                      (git-over-network "Fetching" top (append (list "fetch") switches (list remote)) (format nil "Fetched from ~a" remote))))))

(define-menu git-fetch-menu (:title "Fetch" :show git-menu-shows-p :note git-menu-note)
  "Open the fetch menu: fetch from the upstream remote, a remote asked for, or
every remote. \\`C-h' there lists every action."
  ("Fetch from"
   ("u" "The upstream remote" git-fetch)
   ("e" "A remote asked for" git-fetch-remote)
   ("a" "Every remote" git-fetch-all))
  ("Switches"
   ("- p" "Forget branches gone from the remote" :switch "--prune")
   ("- t" "Every tag" :switch "--tags")))

;;; Pulling

(defun git-pull-arguments (arguments)
  "ARGUMENTS for git pull, with --ff-only added when none of them says how
to bring in commits the branch cannot simply move on to."
  (if (intersection arguments '("--rebase" "--no-rebase") :test #'string=)
      arguments
      (cons "--ff-only" arguments)))

(define-command git-pull ()
  "Bring in the upstream's commits: only when the branch can simply move on
to them (fast-forward), unless the pull menu says to rebase (-r) or merge
\(-m) when it cannot. With HEAD detached there is no branch to bring them in to."
  (with-git-repository (top switches)
    (git-ask "Reading the branch" top #'git-current-branch
             (lambda (branch)
               (if branch
                   (git-over-network "Pulling" top (list* "pull" (git-pull-arguments switches)) "Pulled")
                   (message "HEAD is detached: no branch to pull into"))))))

(define-command git-pull-from ()
  "Bring in the commits of a branch, asked for, of a remote asked for, as \\[git-pull] does."
  (with-git-repository (top switches)
    (git-ask-remote "Pull from remote: " top
                    (lambda (remote)
                      (git-ask "Reading the branches" top
                               (lambda (job top) (values (git-lines job top (list "for-each-ref" "--format=%(refname:lstrip=3)"
                                                                                  (format nil "refs/remotes/~a" remote)))
                                                         (git-current-branch job top)))
                               (lambda (branches current)
                                 (let ((branch (git-read-name (format nil "Pull from ~a's branch: " remote) branches :default current)))
                                   (when branch
                                     (git-over-network "Pulling" top (append (list "pull") (git-pull-arguments switches) (list remote branch))
                                                       (format nil "Pulled ~a from ~a" branch remote))))))))))

(define-menu git-pull-menu (:title "Pull" :show git-menu-shows-p :note git-menu-note)
  "Open the pull menu: bring in the upstream's commits, or a branch's of a
remote asked for. \\`C-h' there lists every action."
  ("Pull from"
   ("u" "The upstream" git-pull)
   ("e" "A branch asked for" git-pull-from))
  ("When the branch cannot simply move on"
   ("- r" "Rebase on them" :switch "--rebase")
   ("- m" "Merge them" :switch "--no-rebase"))
  ("Switches"
   ("- A" "Put changes aside meanwhile" :switch "--autostash")))

;;; Pushing

(defun git-push-allowed-p (arguments)
  "True when a push with ARGUMENTS may go: one forcing the remote, which can
take away commits it has, only once asked."
  (cond ((member "--force" arguments :test #'string=)
         (yes-or-no-p* "Force the push? Commits the remote has that this branch has not are taken away"))
        ((member "--force-with-lease" arguments :test #'string=)
         (yes-or-no-p* "Force the push, if the remote has nothing new since the last fetch?"))
        (t t)))

(define-command git-push ()
  "Push the branch to its upstream, in the background; the push menu's
switches go with it (a forced push asked first)."
  (with-git-repository (top switches)
    (when (git-push-allowed-p switches)
      (git-over-network "Pushing" top (list* "push" switches) "Pushed"))))

(define-command git-push-elsewhere ()
  "Push the branch checked out to a remote asked for, under its own name; -u
in the push menu makes that its upstream."
  (with-git-repository (top switches)
    (git-ask "Reading the branch" top #'git-current-branch
             (lambda (branch)
               (if (null branch)
                   (message "HEAD is detached: no branch to push")
                   (git-ask-remote (format nil "Push ~a to: " branch) top
                                   (lambda (remote)
                                     (when (git-push-allowed-p switches)
                                       (git-over-network "Pushing" top (append (list "push") switches (list remote branch))
                                                         (format nil "Pushed ~a to ~a" branch remote))))))))))

(define-command git-push-tags ()
  "Push every tag to the upstream remote (or origin)."
  (with-git-repository (top switches)
    (when (git-push-allowed-p switches)
      (git-over-network "Pushing the tags" top (list* "push" "--tags" switches) "Pushed the tags"))))

(define-command git-push-tag ()
  "Push a tag, asked for, to a remote asked for."
  (with-git-repository (top switches)
    (git-ask-tag "Push tag: " top
                 (lambda (tag)
                   (git-ask-remote (format nil "Push ~a to: " tag) top
                                   (lambda (remote)
                                     (when (git-push-allowed-p switches)
                                       (git-over-network "Pushing a tag" top (append (list "push") switches (list remote (format nil "refs/tags/~a" tag)))
                                                         (format nil "Pushed ~a to ~a" tag remote)))))))))

(define-menu git-push-menu (:title "Push" :show git-menu-shows-p :note git-menu-note)
  "Open the push menu: push the branch to its upstream or a remote asked for,
every tag or one; its switches force, set the upstream, try only, skip the
hooks. \\`C-h' there lists every action."
  ("Push"
   ("p" "To the upstream" git-push)
   ("e" "To a remote asked for" git-push-elsewhere))
  ("Tags"
   ("t" "Every tag" git-push-tags)
   ("T" "A tag" git-push-tag))
  ("Switches"
   ("- f" "Force, if nothing new there" :switch "--force-with-lease")
   ("- F" "Force" :switch "--force")
   ("- u" "Make it the upstream" :switch "--set-upstream")
   ("- n" "Only try" :switch "--dry-run")
   ("- N" "No hooks" :switch "--no-verify")))

;;; Any git command

(defvar *git-built-in-commands* nil
  "The commands built into git, as git --list-cmds=builtins lists them, read
the first time they are wanted.")

(defun git-built-in-commands ()
  "The commands built into git (*GIT-BUILT-IN-COMMANDS*): those it runs
itself, never a program or an alias of a repository's or yours."
  (or *git-built-in-commands*
      (setf *git-built-in-commands*
            (let ((git (tuatara.io:find-program "git")))
              (and git (uiop:split-string (string-trim '(#\Newline) (or (ignore-errors (uiop:run-program (list git "--list-cmds=builtins") :output :string)) ""))
                                          :separator '(#\Newline)))))))

(defparameter *git-line-options-allowed* '("--no-pager" "-P" "--literal-pathspecs")
  "The options before the command \\[git-run-command] takes while evidence
folders are named: none that sets something or points git elsewhere.")

(defparameter *git-config-reading-options*
  '("--get" "--get-all" "--get-regexp" "--get-urlmatch" "--get-color" "--get-colorbool" "--list" "-l")
  "The options of git config that only read its settings.")

(defun git-config-reading-p (more)
  "True when git config with the words MORE after it only reads: given one
of *GIT-CONFIG-READING-OPTIONS*, or its get or list command."
  (or (member (first more) '("get" "list") :test #'equal)
      (some (lambda (word) (member word *git-config-reading-options* :test #'string=)) more)))

(defun git-command-refusal (words)
  "Why git is not run with WORDS, typed for \\[git-run-command], in words; NIL
when it may be. While evidence folders are named (*EVIDENCE-FOLDERS*): no
option before the command but *GIT-LINE-OPTIONS-ALLOWED* (-c, --config-env,
--exec-path and the like could name a program for git to run or point it
elsewhere); no command but those built into git (an alias or a program of
git's family runs what a repository or a setting names); and git config
only reading, never setting what a later command would run (hooks,
filters, programs)."
  (when (tuatara.io:writing-refused-anywhere-p)
    (let* ((rest (git-command-words words))
           (command (first rest))
           (leading (remove-if (lambda (word) (member word *git-line-options-allowed* :test #'string=))
                               (ldiff words rest))))
      (cond (leading
             (format nil "while evidence folders are named, git is given no option before its command here but ~{~a~^, ~} (not ~a)"
                     *git-line-options-allowed* (first leading)))
            ((and command (not (member command (git-built-in-commands) :test #'string=)))
             (format nil "while evidence folders are named, only git's own commands run here, not ~a (an alias, or a program of its family)" command))
            ((and (equal command "config") (not (git-config-reading-p (rest rest))))
             "while evidence folders are named, git config only reads here (--get, --list): a setting it writes could have git run a program")))))

(define-command git-run-command ()
  "Run git with the words asked for (log --oneline -5, say; quotes group
words as in a shell) in the repository at hand, no shell between, and show
what it printed; for what the menus have no action for. While evidence
folders are named, only git's own commands, with no setting given on the
line (GIT-COMMAND-REFUSAL)."
  (with-git-repository (top)
    (multiple-value-bind (words ok) (tuatara.io:command-words (read-string "Run git: "))
      (cond ((not ok) (message "A quote is left open: nothing run"))
            ((null words) (message "Nothing to run"))
            ((git-command-refusal words) (message "Nothing run: ~a" (git-command-refusal words)))
            (t (git-show-output (format nil "Running git ~a" (first words)) top words "*git output*"
                                :mode 'special-mode :empty (format nil "git ~a printed nothing" (first words))
                                :environment (git-network-environment)
                                :after (lambda () (git-refresh-repository top))))))))

(defun install-git-remote ()
  "Bind the fetch menu to f, the pull menu to F, the push menu to P and
running a git command to ! in a status."
  (define-keys *git-status-keymap* '(("f" git-fetch-menu) ("F" git-pull-menu) ("P" git-push-menu) ("!" git-run-command))))
