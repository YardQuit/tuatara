;;;; lisp/git/git.lisp --- Git from the editor: a repository's state, staging, commits, the log, as Magit has them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-x g shows the state of the repository the current file is in, as
;;;; Magit's status does: the branch and how it stands with its upstream, the
;;;; files not tracked, those changed and those staged, and the latest commits.
;;;; On a file, s stages it, u unstages it, x reverses its changes (asked);
;;;; on a section's heading, the same for all its files; on a hunk, the same
;;;; for the hunk, and with lines selected in hunks, for those lines alone.
;;;; TAB shows a file's changes under it, in hunks. The sections are headed
;;;; and folded as in every list in sections: TAB on a heading folds it,
;;;; S-TAB folds them all. n and p go to the next and the
;;;; previous file or hunk, j and k by lines, l (or RET) visits, h closes. c
;;;; writes a commit's message (C-c C-c commits it, C-c C-k gives it up), P
;;;; pushes, F pulls, L lists the log, d shows every change, g looks again.
;;;; In a file's own buffer M-x git-stage-hunk and M-x git-unstage-hunk do it for the hunk at
;;;; point. Git runs as a job in the background: nothing waits for it.
;;;;
;;;; Git runs programs a repository names for itself (hooks on a commit,
;;;; filters as it compares files), so it is run only for a project you
;;;; trust, as language servers are: C-x g asks once for one not trusted.

(in-package #:tuatara.defaults)

(defvar *git-log-length* 200
  "How many commits \\[git-log] lists.")

(defvar *git-recent-commits* 10
  "How many of the latest commits \\[git] shows.")

(defvar *git-diff-lines* 1000
  "How many lines of a file's changes \\[git-toggle-diff] shows under it at first,
and how many more each \\[git-toggle-diff] on the line after them shows.")

(defvar *git-section-lines* 1000
  "How many files each section of a status (\\[git]) lists at first, and how
many more each \\[git-toggle-diff] on the line after them lists.")

(defvar *git-menus* :slim
  "How much the git menus (branch, worktree ...) show: :SLIM the actions
*GIT-SLIM-ACTIONS* names, the ones most used, :FULL every one. \\<git-status-mode>\\[git-toggle-full-menus]
in a status switches between them for that status.")

(defvar *git-slim-actions*
  '(git-commit git-commit-amend git-commit-reword git-commit-extend
    git-checkout git-branch-and-checkout git-branch-rename git-branch-delete
    git-worktree-branch git-worktree-status git-worktree-delete
    git-stash git-stash-apply git-stash-pop git-stash-drop
    git-log git-log-other git-log-file git-diff git-diff-staged git-diff-commit
    git-fetch git-fetch-all git-pull git-push git-push-elsewhere
    git-merge git-merge-continue git-merge-abort git-rebase-upstream git-rebase-onto
    git-rebase-interactive git-rebase-continue git-rebase-abort git-cherry-pick git-cherry-pick-continue git-cherry-pick-abort
    git-revert git-revert-continue git-revert-abort git-reset-mixed git-reset-hard git-reset-file
    git-tag-create git-tag-delete git-remote-add git-remote-set-address
    git-bisect-start git-bisect-good git-bisect-bad git-bisect-skip git-bisect-reset
    git-submodule-add git-submodule-update git-ignore-shared git-ignore-private
    git-stage-file git-unstage-file git-stage-hunk git-unstage-hunk git-diff-file git-blame git-rewind
    git-format-patch git-apply-patch git-am git-am-continue git-am-abort git-notes-edit
    git-subtree-add git-subtree-pull git-subtree-push git-sparse-set git-sparse-disable)
  "The actions the slim git menus show (*GIT-MENUS*), by command: push one to
show it too, or take one away.")

;;; Git in the background

(defstruct (git-job (:include tuatara.runtime:job) (:constructor make-git-job (buffer doing then after quiet)) (:copier nil))
  "Git at work for BUFFER (a status, a log, a commit's message), a job: DOING
says what it does (Pushing); THEN is called on the editor's thread with what
its work gave once it is done, AFTER, when given, however it ended. QUIET
work only reads the repository: it is stopped without asking when its buffer
is killed or the editor left. RESULT is what the work gave, SAID what git said
went wrong when it failed."
  doing then after quiet (result '()) said)

(setf (documentation 'git-job-p 'function) "True when OBJECT is a GIT-JOB.")

(tuatara.runtime:define-job-words git-job
  :doing (git-job-doing job)
  :description (format nil "git: ~(~a~)" (git-job-doing job))
  :left-words (if (git-job-quiet job) "nothing was changed" "the repository as git left it")
  :at-exit (if (git-job-quiet job) :quiet :stop))

(defmethod tuatara.runtime:job-holds-queue-p ((job git-job))
  "False: other work on the buffer need not wait for git."
  nil)

(defmethod tuatara.runtime:job-take-in ((job git-job))
  "Hand what the work gave to the job's THEN, or say how it ended; then call its AFTER."
  (case (tuatara.runtime:job-status job)
    (:done (apply (git-job-then job) (git-job-result job)))
    (:failed (message "~a" (or (git-job-said job) (tuatara.runtime:job-ending-words job))))
    (:stopped (unless (killed-job-buffer-p job)
                (message "~a" (tuatara.runtime:job-ending-words job)))))
  (when (git-job-after job) (funcall (git-job-after job))))

(defun start-git-job (doing work then &key after quiet (buffer (current-buffer)))
  "Run WORK, a function of the job, on a thread of its own as a job of BUFFER
doing DOING (GIT-JOB), listed by \\[list-jobs] and stopped by \\[stop-job];
THEN is called on the editor's thread with the values WORK returned once it
did all of it, AFTER, a function of no arguments, however it ended. QUIET for
work that only reads."
  (tuatara.runtime:start-job-thread
   (make-git-job buffer doing then after quiet)
   (lambda (job)
     (let* ((result '())
            (said (catch 'git-failed
                    (handler-case (setf result (multiple-value-list (funcall work job)))
                      ;; refused for evidence: said as it is, in one sentence
                      (git-refused (e) (throw 'git-failed (princ-to-string e))))
                    nil)))
       (if said
           (setf (git-job-said job) said
                 (tuatara.runtime:job-error job) said
                 (tuatara.runtime:job-status job) :failed)
           (setf (git-job-result job) result))))
   :name "tuatara git"))

(defun git-failure-words (command text)
  "What is said when git COMMAND (push) failed, having printed TEXT: git push:
and the line saying what went wrong, the first beginning fatal: or error:,
else the last line it printed, else that it failed; when the server refused
every ssh key, that, and how to give ssh yours."
  (let ((lines (remove-if (lambda (line) (zerop (length (string-trim '(#\Space #\Tab #\Return) line))))
                          (uiop:split-string text :separator '(#\Newline)))))
    (format nil "git ~a: ~a" command
            (or (and (search "Permission denied (publickey" text)
                     "the server took none of the ssh keys offered (Permission denied (publickey)): ssh-add puts yours in your ssh agent")
                (find-if (lambda (line) (or (uiop:string-prefix-p "fatal:" line) (uiop:string-prefix-p "error:" line))) lines)
                (car (last lines))
                "it failed"))))

(defun git-job-try (job top arguments &key errors environment input)
  "On JOB's thread (with JOB NIL, the thread calling it, never the
editor's): run git with ARGUMENTS in TOP (RUN-GIT), ENVIRONMENT (NAME=VALUE
strings) added and INPUT, a string, on its standard input; what it printed,
as text, and true as a second value when it succeeded; with ERRORS, what it
said on its error output as a third. The job goes on however git ended."
  (multiple-value-bind (octets ok too-large said)
      (run-git top arguments :cancel (and job (tuatara.runtime:job-cancel job)) :errors errors
                             :environment environment :input input)
    (declare (ignore too-large))
    (values (if octets (tuatara.text:lenient-string octets) "") ok said)))

(defparameter *git-shown-limit* (* 64 1024 1024)
  "The most git may print for a commit, a stash or changes shown in a buffer
of their own (GIT-SHOW-OUTPUT).")

(defun git-job-octets (job top arguments &key environment input limit)
  "On JOB's thread: run git with ARGUMENTS in TOP (RUN-GIT), what it says on
its error output read apart, ENVIRONMENT (NAME=VALUE strings) added
and INPUT, a string, on its standard input; what it printed, as octets, when
it succeeded. When it did not, or printed more than LIMIT octets
\(*PROGRAM-OUTPUT-LIMIT* when it is NIL), JOB ends
there: stopped when it was asked to, else failed, saying what git said went
wrong (GIT-FAILURE-WORDS) or that it printed too much."
  (multiple-value-bind (octets ok too-large said)
      (run-git top arguments :cancel (tuatara.runtime:job-cancel job) :errors t
                             :environment environment :input input :limit limit)
    (cond (too-large
           (throw 'git-failed (format nil "git ~a printed more than ~a: too much to show" (first arguments)
                                      (tuatara.io:octets-words (or limit *program-output-limit*)))))
          (ok (or octets (make-array 0 :element-type '(unsigned-byte 8))))
          (t (tuatara.runtime:job-check-stop)
             ;; what it said, or, saying nothing, what it printed (nothing to commit)
             (throw 'git-failed (git-failure-words (first arguments)
                                                   (if (plusp (length said)) said (if octets (tuatara.text:lenient-string octets) ""))))))))

(defun git-job-output (job top arguments &key environment input)
  "On JOB's thread: what git printed with ARGUMENTS in TOP, as text, as
GIT-JOB-OCTETS runs it; JOB ends there when git failed."
  (tuatara.text:lenient-string (git-job-octets job top arguments :environment environment :input input)))

(defun git-output-line (job top arguments)
  "On JOB's thread: what git prints with ARGUMENTS in TOP, often one line,
the newlines at its ends left out (GIT-JOB-OUTPUT)."
  (string-trim '(#\Newline) (git-job-output job top arguments)))

(defun git-in-background (doing top arguments then &key environment input after quiet)
  "Run git with ARGUMENTS for the repository at TOP as a job of the current
buffer doing DOING (START-GIT-JOB, GIT-JOB-OUTPUT); THEN is called with what
it printed, as text, when it succeeded, AFTER however it ended; what went
wrong is said when it failed."
  (start-git-job doing (lambda (job) (git-job-output job top arguments :environment environment :input input))
                 then :after after :quiet quiet))

(defun git-top (buffer)
  "The top of the repository BUFFER's file or folder is in, or NIL."
  (let ((directory (or (let ((file (tuatara.buffer:buffer-file buffer))) (and file (tuatara.io:parent-path file)))
                       (tuatara.buffer:buffer-local buffer 'git-top)
                       (buffer-directory buffer))))
    (and directory (ignore-errors (repository-top directory)))))

(defun git-here (&optional (buffer (current-buffer)))
  "The top of the repository BUFFER (the current buffer), a status, log,
commit's message or history, is for."
  (tuatara.buffer:buffer-local buffer 'git-top))

(defun git-trusted-p (top)
  "True when git may run for the repository at TOP: a project trusted
\(TRUST-PROJECT), asked once when it is not."
  (trust-project top))

(defun git-repository ()
  "The top of the repository to act on: the current status's, log's or
commit message's, else that of the current file or folder when its project
is trusted (asked once when it is not); NIL, said so, when there is none."
  (or (git-here)
      (let ((top (git-top-said (current-buffer))))
        (and top (git-trusted-p top) top))))

(defun git-top-said (buffer)
  "The top of the repository BUFFER's file or folder is in (GIT-TOP); NIL,
said so, when it is in none."
  (or (git-top buffer)
      (progn (message "~a is in no git repository" (tuatara.buffer:buffer-name buffer)) nil)))

(defmacro with-git-repository ((top &optional switches) &body body)
  "Run BODY with TOP bound to the repository to act on (GIT-REPOSITORY), and
SWITCHES, when named, to the switches and options of the menu the command
was run from (MENU-ARGUMENTS), read before anything is asked; nothing when
there is no repository."
  `(let (,@(and switches `((,switches (menu-arguments))))
         (,top (git-repository)))
     (when ,top ,@body)))

(defun git-file-here (top)
  "The file of the repository at TOP to act on, relative to TOP: the one on
point's line of a status, else the one the current buffer shows (SHOWN-FILE:
the entry on the line in a list of a directory, the file visited); NIL when
there is none."
  (let ((entry (and (git-here) (list-entry-at))))
    (if (member (first entry) '(:file :hunk))
        (third entry)
        (let* ((file (shown-file (current-buffer)))
               (file (and file (tuatara.io:native file))))
          (and file (uiop:string-prefix-p top file) (< (length top) (length file)) (subseq file (length top)))))))

(defun call-with-git-file (top missing function)
  "Call FUNCTION with the file of the repository at TOP to act on
\(GIT-FILE-HERE), relative to TOP; say MISSING when there is none."
  (let ((path (git-file-here top)))
    (if path (funcall function path) (message "~a" missing))))

(defun git-inside-path-p (path)
  "True when PATH names a place within a repository, from its top: not empty,
not beginning with - (an option to git) or /, and never going up with .."
  (and (plusp (length path))
       (not (member (char path 0) '(#\- #\/)))
       (not (search ".." path))))

(defun git-refresh-repository (top)
  "Read the repository at TOP again in its status, when one is open."
  (let ((status (git-buffer-of top 'git-status-mode)))
    (when status (git-refresh-status status))))

(defun git-run (doing top arguments done &key input environment then)
  "Run git with ARGUMENTS for the repository at TOP as a job doing DOING,
INPUT on its standard input and ENVIRONMENT added; once it has, say DONE (a
string, or a function of what git printed giving one) and call THEN, when
given, with what it printed; what went wrong is said when it failed. Its
status is read again however it ended."
  (git-in-background doing top arguments
                     (lambda (text)
                       (message "~a" (if (functionp done) (funcall done text) done))
                       (when then (funcall then text)))
                     :input input :environment environment
                     :after (lambda () (git-refresh-repository top))))

(defun git-lines (job top arguments)
  "On JOB's thread: the lines git prints with ARGUMENTS for the repository at TOP, empty ones left out."
  (tuatara.text:split-words (git-job-output job top arguments) '(#\Newline)))

(defun git-ask (doing top work then)
  "Read with WORK, a function of the job and TOP, what a question offers, in
the background, as a job doing DOING; then call THEN with it on the editor's
thread, where questions are asked."
  (start-git-job doing (lambda (job) (funcall work job top)) then :quiet t))

(defun git-branches (job top &key remote (local t))
  "On JOB's thread: the names of the repository's branches at TOP, with LOCAL
its own first, then with REMOTE those of its remotes (origin/main)."
  (git-lines job top (append (list "for-each-ref" "--format=%(refname:short)")
                             (and local (list "refs/heads"))
                             (and remote (list "refs/remotes")))))

(defun git-current-branch (job top)
  "On JOB's thread: the branch checked out at TOP, or NIL when HEAD is detached."
  (let ((name (git-output-line job top (list "branch" "--show-current"))))
    (and (plusp (length name)) name)))

(defun git-branches-and-head (job top)
  "On JOB's thread: every branch of the repository at TOP, its remotes' too,
and the branch checked out (HEAD when detached), as two values."
  (values (git-branches job top :remote t) (or (git-current-branch job top) "HEAD")))

(defun git-name-p (name)
  "True when NAME may be given to git as a branch, revision or remote: not
empty, not beginning with -, which git would read as an option, no space or
control character in it."
  (and (plusp (length name))
       (char/= (char name 0) #\-)
       (notany (lambda (c) (or (char<= c #\Space) (char= c #\Rubout))) name)))

(defun git-read-name (prompt names &key default require-match)
  "Ask for a branch, revision or remote with PROMPT, NAMES offered, DEFAULT
when nothing is typed; NIL, said so, when the name could not be given to git
\(GIT-NAME-P)."
  (let ((name (if default
                  (completing-read prompt names :require-match require-match :default default)
                  (completing-read prompt names :require-match require-match))))
    (if (git-name-p name)
        name
        (progn (message "~:[No name given~;Not a name git takes: ~:*~a~]" (and (plusp (length name)) (escape-controls name))) nil))))

;;; Menus, slim and full

(defun git-menus-level ()
  "How much the git menus show here: the current status's own choice, else *GIT-MENUS*."
  (or (tuatara.buffer:buffer-local (current-buffer) 'git-menus) *git-menus*))

(defun git-menu-shows-p (entry)
  "True when a git menu shows ENTRY: its switches and options always, its
actions in the full menus all, in the slim ones those *GIT-SLIM-ACTIONS* names
\(by name, as an init file's symbols are its own)."
  (let ((command (menu-entry-command entry)))
    (or (null command) (eq (git-menus-level) :full) (member command *git-slim-actions* :test #'string=))))

(defun git-menu-note (menu)
  "The line C-h says first in a git MENU: slim or full, and how many actions the slim one leaves out."
  (let ((hidden (count-if-not #'git-menu-shows-p (menu-entries menu :all t))))
    (if (eq (git-menus-level) :full)
        "Full menus: every action. ~ in the status makes them slim."
        (format nil "Slim menus: ~d action~:p not shown; ~~ in the status shows every one, *git-slim-actions* chooses." hidden))))

(define-command git-toggle-full-menus ()
  "Make the git menus of this status show every action, or only the ones
most used again (*GIT-MENUS*, *GIT-SLIM-ACTIONS*)."
  (let ((full (eq (git-menus-level) :full)))
    (setf (tuatara.buffer:buffer-local (current-buffer) 'git-menus) (if full :slim :full))
    (message (if full "Slim git menus: the actions most used" "Full git menus: every action"))))

(defparameter *git-commit-format* "--format=%x1e%H%x1f%h%x1f%ad%x1f%an%x1f%cr%x1f%D%x1f%s"
  "How git log is asked to write each commit, for PARSE-GIT-COMMITS: a record
separator, then its hash, short hash, date, author, how long ago it was
made, the names at it and its subject, a unit separator between each.")

(defstruct (git-commit (:constructor make-git-commit (&key hash short date author ago names subject path (graph ""))) (:copier nil))
  "A commit as git log wrote it (PARSE-GIT-COMMITS): its HASH and SHORT hash,
its DATE, AUTHOR, how long AGO it was made (2 days ago), the NAMES at it
\(branches, tags) and its SUBJECT, as git writes them; PATH, what the file
of a history was called in it; GRAPH, the characters of the graph before
it, with --graph. A line of the graph alone is a GIT-COMMIT with no HASH."
  hash short date author ago names subject path (graph ""))

(setf (documentation 'git-commit-p 'function) "True when OBJECT is a GIT-COMMIT.")

(defun parse-git-commit-fields (text)
  "TEXT, a commit's fields as *GIT-COMMIT-FORMAT* writes them after its
record separator, as a GIT-COMMIT; NIL when its hash is not one
\(GIT-HASH-P), as a subject holding git's separators would make."
  (let ((fields (uiop:split-string text :separator (list (code-char #x1f)))))
    (when (and (>= (length fields) 7) (git-hash-p (first fields)))
      (destructuring-bind (hash short date author ago names &rest subject) fields
        (make-git-commit :hash hash :short short :date date :author author :ago ago :names names
                         :subject (reduce (lambda (a b) (concatenate 'string a (string (code-char #x1f)) b)) subject))))))

(defun parse-git-commits (text &key names path)
  "TEXT, what git log printed with *GIT-COMMIT-FORMAT*, as a list of
GIT-COMMITs, the latest first. With NAMES, git was asked for the names of
the files each changed (--name-only -z): each one's PATH is the name after
it, one naming none left out. PATH, when given, is every one's. Without
NAMES, the lines of a graph (--graph) are kept: each commit's GRAPH, and a
line of the graph alone a GIT-COMMIT of its own."
  (let* ((chunks (uiop:split-string text :separator (list (code-char #x1e))))
         ;; what comes before a commit on its line: the graph
         (before (first chunks))
         (commits '()))
    (dolist (chunk (rest chunks) (nreverse commits))
      (let* ((end (or (position-if (lambda (c) (or (char= c #\Newline) (char= c (code-char 0)))) chunk) (length chunk)))
             (commit (parse-git-commit-fields (subseq chunk 0 end)))
             (after (subseq chunk end)))
        (if names
            (let ((named (or path (string-trim (list #\Newline (code-char 0)) after))))
              (when (and commit (plusp (length named)))
                (setf (git-commit-path commit) named)
                (push commit commits)))
            (let ((lines (uiop:split-string after :separator '(#\Newline))))
              (when commit
                (setf (git-commit-graph commit) before (git-commit-path commit) path)
                (push commit commits))
              ;; the lines of the graph alone before the next commit, then the next one's graph
              (dolist (line (butlast (rest lines)))
                (push (make-git-commit :graph line) commits))
              (setf before (if (rest lines) (car (last lines)) ""))))))))

(defun git-log-arguments (count)
  "The arguments of git log listing the latest COUNT commits as PARSE-GIT-COMMITS reads them."
  (list "log" (format nil "-n~d" count) *git-commit-format* "--date=short"))

(defun git-commit-line (commit)
  "COMMIT as a status lists it: (SHORT SUBJECT)."
  (list (git-commit-short commit) (git-commit-subject commit)))

;;; What the repository holds

(defstruct (git-state (:constructor make-git-state ()) (:copier nil))
  "A repository as \\[git] shows it: its HEAD (the branch, or NIL when
detached, and the commit, (initial) before the first), the UPSTREAM and how
many commits AHEAD and BEHIND it is, and its files: UNMERGED (in conflict,
git having joined two changes to them only in part, KIND saying how each
side changed it: GIT-UNMERGED-WORD), UNTRACKED, UNSTAGED and
STAGED, each a list of (PATH KIND), KIND a word, a rename staged (PATH KIND
ORIGINAL), ORIGINAL its name before, a folder none of whose files is
tracked one entry of UNTRACKED ending in /; the STASHES, each (NAME
SUBJECT), the latest first; the RECENT commits, each (HASH SUBJECT); the
commits not pushed, UNPUSHED, each (HASH SUBJECT), the latest first, at most
*GIT-RECENT-COMMITS* of UNPUSHED-COUNT, and UNPUSHED-TO, the upstream they
are not in, or NIL for those on no remote at all, a branch with none; and
DOING, what git is in the middle of: :AM, :MERGE, :REBASE, :CHERRY-PICK, :REVERT,
:BISECT, or NIL; its SUBMODULES, each (PATH STATE); the files it IGNORES,
each (PATH KIND), KIND the word ignored, a folder ignored whole one entry
ending in /; and STASH-STAMP, the stamp of git's list of stashes when they
were read, NIL when there was none."
  branch commit upstream (ahead 0) (behind 0)
  (unmerged '()) (untracked '()) (unstaged '()) (staged '()) (stashes '()) (recent '()) (unpushed '()) (unpushed-count 0) unpushed-to
  doing (submodules '()) (ignored '()) stash-stamp)

(defun git-state-unborn-p (state)
  "True when the repository STATE is of has no commit yet."
  (equal (git-state-commit state) "(initial)"))

(defun git-change-word (code)
  "The word for the change git marks with the character CODE."
  (case code
    (#\M "modified") (#\A "new file") (#\D "deleted") (#\R "renamed") (#\C "copied")
    (#\T "typechange") (#\U "unmerged") (t "changed")))

(defun git-unmerged-word (xy)
  "The words for how both sides changed a file in conflict, git marking how
with the two characters XY: us the first side of its conflicts, them the
second, as \\[conflict-markers-mode] says which is which in a merge and in a
rebase."
  (cond ((string= xy "DD") "both deleted")
        ((string= xy "AU") "added by us")
        ((string= xy "UD") "deleted by them")
        ((string= xy "UA") "added by them")
        ((string= xy "DU") "deleted by us")
        ((string= xy "AA") "both added")
        (t "both changed")))

(defun parse-git-status (text)
  "TEXT, what git status --porcelain=v2 --branch -z printed, as a GIT-STATE; a
submodule's KIND is submodule; the files ignored, with --ignored, ignored; a
file in conflict's, how both sides changed it (GIT-UNMERGED-WORD)."
  (let ((state (make-git-state))
        (fields (uiop:split-string text :separator (list (code-char 0)))))
    (loop while fields
          do (let ((entry (pop fields)))
               (cond ((uiop:string-prefix-p "# branch.oid " entry)
                      (setf (git-state-commit state) (subseq entry 13)))
                     ((uiop:string-prefix-p "# branch.head " entry)
                      (let ((head (subseq entry 14)))
                        (setf (git-state-branch state) (and (string/= head "(detached)") head))))
                     ((uiop:string-prefix-p "# branch.upstream " entry)
                      (setf (git-state-upstream state) (subseq entry 18)))
                     ((uiop:string-prefix-p "# branch.ab " entry)
                      (let ((counts (uiop:split-string (subseq entry 12) :separator '(#\Space))))
                        (setf (git-state-ahead state) (abs (or (parse-integer (first counts) :junk-allowed t) 0))
                              (git-state-behind state) (abs (or (parse-integer (or (second counts) "0") :junk-allowed t) 0)))))
                     ((uiop:string-prefix-p "? " entry)
                      (push (list (subseq entry 2) "untracked") (git-state-untracked state)))
                     ((uiop:string-prefix-p "! " entry)
                      (push (list (subseq entry 2) "ignored") (git-state-ignored state)))
                     ((or (uiop:string-prefix-p "1 " entry) (uiop:string-prefix-p "2 " entry) (uiop:string-prefix-p "u " entry))
                      ;; the path is the last field; a rename's original comes after, on its own
                      (let* ((parts (uiop:split-string entry :separator '(#\Space)))
                             (xy (second parts))
                             (submodule (uiop:string-prefix-p "S" (third parts)))
                             (path (format nil "~{~a~^ ~}" (nthcdr (case (char entry 0) (#\1 8) (#\2 9) (t 10)) parts))))
                        (flet ((kind (code) (if submodule "submodule" (git-change-word code))))
                          (let ((original (and (char= (char entry 0) #\2) (pop fields))))
                            (if (char= (char entry 0) #\u)
                                (push (list path (git-unmerged-word xy)) (git-state-unmerged state))
                                (progn
                                  (unless (char= (char xy 0) #\.)
                                    (push (list* path (kind (char xy 0)) (and original (list original))) (git-state-staged state)))
                                  (unless (char= (char xy 1) #\.) (push (list path (kind (char xy 1))) (git-state-unstaged state))))))))))))
    (setf (git-state-untracked state) (nreverse (git-state-untracked state))
          (git-state-ignored state) (nreverse (git-state-ignored state))
          (git-state-unmerged state) (nreverse (git-state-unmerged state))
          (git-state-unstaged state) (nreverse (git-state-unstaged state))
          (git-state-staged state) (nreverse (git-state-staged state)))
    state))

(defun git-records (text count)
  "TEXT, lines git printed each of COUNT fields apart by NUL, as a list of
the lists of their fields; a line of another count of fields left out."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for fields = (uiop:split-string line :separator (list (code-char 0)))
        when (= count (length fields)) collect fields))

(defun git-stashes (job top)
  "On JOB's thread: the stashes of the repository at TOP, the latest first, each (NAME SUBJECT)."
  (git-records (git-job-output job top (list "stash" "list" "--format=%gd%x00%gs")) 2))

(defun git-stash-stamp (folder)
  "The stamp of the list of stashes in git's own FOLDER, NIL when there is none:
no stash."
  (ignore-errors (tuatara.io:path-stamp (concatenate 'string folder "logs/refs/stash") :follow nil)))

(defun git-uncommented-lines (text)
  "The lines of TEXT, those beginning with # left out, joined again by newlines."
  (format nil "~{~a~^~%~}" (remove-if (lambda (line) (uiop:string-prefix-p "#" line))
                                     (uiop:split-string text :separator '(#\Newline)))))

(defparameter *git-operations*
  '((:am "rebase-apply/applying") (:rebase "rebase-merge/" "rebase-apply/") (:merge "MERGE_HEAD") (:cherry-pick "CHERRY_PICK_HEAD")
    (:revert "REVERT_HEAD") (:bisect "BISECT_LOG"))
  "What git can be in the middle of, each with the files in its own folder
that say so, in the order they are looked for.")

(defun git-folder (job top)
  "On JOB's thread: git's own folder for the repository or worktree at TOP, ending in /."
  (tuatara.io:slash-ended (string-right-trim "/" (git-output-line job top (list "rev-parse" "--absolute-git-dir")))))

(defun git-operation (folder)
  "What git is in the middle of (*GIT-OPERATIONS*), its own FOLDER tells, or NIL."
  (loop for (operation . files) in *git-operations*
        when (some (lambda (file) (tuatara.io:path-kind (concatenate 'string folder file) :follow nil)) files)
          return operation))

(defun git-read-state (job top folder &key previous)
  "On JOB's thread: the repository at TOP, git's own FOLDER, as a GIT-STATE,
read by git status (taking no lock that git run elsewhere would meet), the
files it ignores found in the same walk of its tree, a folder ignored whole
one entry; its latest commits by git log, and its stashes by git stash list
when it has any, kept from PREVIOUS, the state read before, while their list
is as it was then."
  (let ((state (parse-git-status (git-job-output job top (list "status" "--porcelain=v2" "--branch" "-z" "--untracked-files=normal"
                                                               "--ignored=traditional")
                                                 :environment (list "GIT_OPTIONAL_LOCKS=0"))))
        (stamp (git-stash-stamp folder)))
    (setf (git-state-doing state) (git-operation folder)
          (git-state-submodules state) (git-submodules job top)
          (git-state-stash-stamp state) stamp)
    (when (and stamp (not (git-state-unborn-p state)))
      (setf (git-state-stashes state)
            (if (and previous (equalp stamp (git-state-stash-stamp previous)))
                (git-state-stashes previous)
                (git-stashes job top))))
    (unless (git-state-unborn-p state)
      (setf (git-state-recent state)
            (mapcar #'git-commit-line (parse-git-commits (git-job-output job top (git-log-arguments *git-recent-commits*)))))
      (git-read-unpushed job top state))
    state))

(defun git-read-unpushed (job top state)
  "On JOB's thread: put in STATE, of the repository at TOP, the commits of its
branch not pushed: those its upstream lacks, when it has one; else, when the
repository has a remote, those on no remote at all."
  (let ((range (cond ((git-state-upstream state) (and (plusp (git-state-ahead state)) (list "@{upstream}..HEAD")))
                     ((plusp (length (string-trim '(#\Space #\Newline) (or (git-job-try job top (list "remote")) ""))))
                      (list "HEAD" "--not" "--remotes")))))
    (when range
      (setf (git-state-unpushed-to state) (git-state-upstream state)
            (git-state-unpushed-count state)
            (if (git-state-upstream state)
                (git-state-ahead state)
                (or (parse-integer (or (git-job-try job top (append (list "rev-list" "--count") range)) "") :junk-allowed t) 0))
            (git-state-unpushed state)
            (and (plusp (git-state-unpushed-count state))
                 (mapcar #'git-commit-line
                         (parse-git-commits (or (git-job-try job top (append (git-log-arguments *git-recent-commits*) range)) ""))))))))

;;; Hunks

(defun git-parse-diff (text)
  "TEXT, what git diff printed for one file, as a list: its header (the lines
before its first hunk, a string ending in a newline, empty with no changes),
then each hunk, a list of its lines, the first its @@ line."
  (if (zerop (length text))
      (list "")
      (let ((header '()) (hunks '()))
        (dolist (line (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline)))
          (cond ((uiop:string-prefix-p "@@" line) (push (list line) hunks))
                (hunks (push line (first hunks)))
                (t (push line header))))
        (cons (format nil "~{~a~%~}" (nreverse header)) (nreverse (mapcar #'reverse hunks))))))

(defun git-read-diff (job top section paths)
  "On JOB's thread: the changes of the file of SECTION (:UNMERGED, :UNSTAGED
or :STAGED) of the repository at TOP (or in that folder, the paths being
relative to it) that PATHS name, its name and, for a rename, its name
before, as GIT-PARSE-DIFF gives them."
  (git-parse-diff (git-job-output job top (append (list "diff" "--no-color" "--no-ext-diff")
                                                  (and (eq section :staged) (list "--cached" "-M"))
                                                  (list "--")
                                                  paths))))

(defun git-entry-paths (file)
  "The paths FILE, an entry of a status's section (PATH KIND [ORIGINAL]), is
about: its own, and for a rename the name it had."
  (list* (first file) (cddr file)))

(defun git-diff-binary-p (diff)
  "True when DIFF, as GIT-PARSE-DIFF gives one, is of a binary file, which git shows no lines of."
  (search "Binary files " (first diff)))

(defun git-hunk-range (hunk)
  "Where HUNK starts in the text before it and how many lines it has there,
then the same in the text after it, as four values, read from its @@ line."
  (multiple-value-bind (whole parts)
      (cl-ppcre:scan-to-strings "^@@ -(\\d+)(?:,(\\d+))? \\+(\\d+)(?:,(\\d+))? @@" (first hunk))
    (if whole
        (flet ((number (i) (if (aref parts i) (parse-integer (aref parts i)) 1)))
          (values (number 0) (number 1) (number 2) (number 3)))
        (values 0 0 0 0))))

(defun git-hunk-line (hunk index)
  "The line (1 the first) of the text after HUNK that its line INDEX (0 its
@@ line) is at, or comes before for a line taken away."
  (let ((line (max 1 (nth-value 2 (git-hunk-range hunk)))))
    (loop for l in (rest hunk)
          for i from 1 below index
          unless (uiop:string-prefix-p "-" l) do (unless (uiop:string-prefix-p "\\" l) (incf line)))
    line))

(defun git-hunk-covers-p (hunk line)
  "True when HUNK changes LINE (1 the first) of the text after it: puts it
there, or only takes lines away, just before it or just after it; the lines
round its changes, which it shows as they are, it does not cover."
  (let ((at (max 1 (nth-value 2 (git-hunk-range hunk))))
        (taken nil))
    (dolist (l (rest hunk) (and taken (<= (1- at) line at)))
      (cond ((uiop:string-prefix-p "+" l) (when (= at line) (return t)) (setf taken nil) (incf at))
            ((uiop:string-prefix-p "-" l) (setf taken t))
            ((uiop:string-prefix-p "\\" l))
            (t (when (and taken (<= (1- at) line at)) (return t))
               (setf taken nil)
               (incf at))))))

(defun git-hunk-patch (hunk &key lines reverse)
  "HUNK, as GIT-PARSE-DIFF gives one, as a hunk of a patch making the changes
of LINES alone, the numbers of its lines to make (1 the first after its @@
line); all of them when LINES is NIL. A change not made stays as it is: a
line taken away is kept, as a line both texts have, and a line added left
out; with REVERSE, for a patch applied backwards, the other way round. A
\\ line (no newline at the end) goes with the line before it; a line kept so
with no newline at the end, a change chosen after it, is given its newline as
a change of its own. NIL when none of LINES is a change."
  (if (null lines)
      hunk
      (let ((body '()) (kept nil) (old 0) (new 0) (changes 0)
            (wanted (sort (copy-list lines) #'<))
            (last-chosen (reduce #'max lines)))
        (loop for (l next) on (rest hunk)
              for i from 1
              for kind = (if (plusp (length l)) (char l 0) #\Space)
              do (loop while (and wanted (< (first wanted) i)) do (pop wanted))
                 (cond ((char= kind #\\) (when kept (push l body)))
                       ((char= kind #\Space) (push l body) (incf old) (incf new) (setf kept t))
                       ((eql (first wanted) i) (push l body) (incf changes) (if (char= kind #\+) (incf new) (incf old)) (setf kept t))
                       ((char= kind (if reverse #\+ #\-))
                        (if (and (uiop:string-prefix-p "\\" next) (< i last-chosen))
                            ;; the line, last of its text, and the same line ended by a newline
                            (progn
                              (if reverse
                                  (progn (push (concatenate 'string "-" (subseq l 1)) body) (push l body) (setf kept t))
                                  (progn (push l body) (push next body) (push (concatenate 'string "+" (subseq l 1)) body)
                                         (setf kept nil)))
                              (incf old) (incf new) (incf changes))
                            (progn (push (concatenate 'string " " (subseq l 1)) body) (incf old) (incf new) (setf kept t))))
                       (t (setf kept nil))))
        (when (plusp changes)
          (multiple-value-bind (old-start old-count new-start) (git-hunk-range hunk)
            (declare (ignore old-count))
            (cons (format nil "@@ -~d,~d +~d,~d @@" old-start old new-start new) (nreverse body)))))))

(defun git-patch (header hunks)
  "A patch of one file: HEADER, as GIT-PARSE-DIFF gives it, and HUNKS."
  (format nil "~a~{~{~a~%~}~}" header hunks))

(defun git-apply-arguments (how)
  "The arguments of git apply applying a patch on its standard input HOW: to
:STAGE it, :UNSTAGE it, reverse it in the files (:REVERSE), or in what is
staged and the files both (:REVERSE-STAGED)."
  (append (list "apply" "--whitespace=nowarn")
          (ecase how
            (:stage (list "--cached"))
            (:unstage (list "--cached" "--reverse"))
            (:reverse (list "--reverse"))
            (:reverse-staged (list "--reverse" "--index")))))

;;; The status

(define-major-mode git-status-mode special-mode
  "The state of a repository (\\[git]): its branch, its files in conflict
\(unmerged), not tracked, changed and staged, and its latest commits. \\<git-status-mode>\\[git-stage] stages the file on the line (on a
section's title, all of its files; on a hunk, the hunk; the lines selected in
hunks alone), \\[git-unstage] unstages, \\[git-reverse] reverses changes (asked), \\[git-toggle-diff] shows a
file's changes under it, and on a section's heading folds the section or
unfolds it, as in every list in sections (▸ before a folded one, the files
ignored folded at first), \\[list-toggle-all-folds] every section;
\\[git-next-section] and \\[git-previous-section] go to the next and the previous file or hunk,
\\[git-visit] visits the file (at the hunk's line) or shows the commit, \\[git-commit-menu] opens the commit
menu, \\[git-push-menu] the push menu, \\[git-pull-menu] the pull menu, \\[git-log-menu] the log menu, \\[git-diff-menu] the diff
menu, \\[revert-buffer] looks again, \\[quit-window] closes. j and k move by lines, as everywhere."
  :lighter "Git" :interactive nil)

(defvar *git-status-keymap* (mode-keymap 'git-status-mode)
  "Keys of a repository's status (git-status-mode's keymap).")

(define-major-mode git-log-mode special-mode
  "A repository's commits (\\[git-log-menu]), a line each: its hash (with the
graph when asked for), date, author, the branches and tags at it, and its
subject. \\<git-log-mode>\\[git-log-show] shows the commit on the line; \\[quit-window] closes."
  :lighter "Git log" :interactive nil)

(defvar *git-log-keymap* (mode-keymap 'git-log-mode)
  "Keys of a repository's log (git-log-mode's keymap).")

(defparameter *git-sections*
  '((:unmerged "Unmerged files") (:untracked "Untracked files") (:unstaged "Unstaged changes") (:staged "Staged changes"))
  "The sections of files in a status, in order, and their titles.")

(defun git-section-files (state section)
  "The files of SECTION (:UNMERGED, :UNTRACKED, :UNSTAGED, :STAGED or :IGNORED) in STATE."
  (ecase section
    (:unmerged (git-state-unmerged state))
    (:untracked (git-state-untracked state))
    (:unstaged (git-state-unstaged state))
    (:staged (git-state-staged state))
    (:ignored (git-state-ignored state))))

(defun git-diff-face (line)
  "The face a line of a difference is shown in."
  (cond ((uiop:string-prefix-p "+" line) :diff.plus)
        ((uiop:string-prefix-p "-" line) :diff.minus)
        ((uiop:string-prefix-p "@@" line) :diff.delta)))

(defparameter *git-operation-words*
  '((:am "taking in patches" "\\<git-status-mode>\\[git-am-menu] c goes on, \\[git-am-menu] s skips this patch, \\[git-am-menu] a stops it")
    (:rebase "rebasing" "\\<git-status-mode>\\[git-rebase-menu] c goes on, \\[git-rebase-menu] s skips this commit, \\[git-rebase-menu] a stops it")
    (:merge "merging" "\\<git-status-mode>\\[git-commit-menu] c commits once the files are put right and staged, \\[git-merge-menu] a stops it")
    (:cherry-pick "cherry-picking" "\\<git-status-mode>\\[git-cherry-pick-menu] c goes on, \\[git-cherry-pick-menu] s skips this commit, \\[git-cherry-pick-menu] a stops it")
    (:revert "reverting" "\\<git-status-mode>\\[git-revert-menu] c goes on, \\[git-revert-menu] s skips this commit, \\[git-revert-menu] a stops it")
    (:bisect "bisecting" "\\<git-status-mode>\\[git-bisect-menu] g, \\[git-bisect-menu] b and \\[git-bisect-menu] s say good, bad, skip; \\[git-bisect-menu] r stops it"))
  "For each of *GIT-OPERATIONS*, what a status says of it and of the keys
going on with it, the menus' keys written in by SUBSTITUTE-COMMAND-KEYS.")

(defun git-status-head-lines (state)
  "The lines at the top of a status of STATE: its head, the branch in a face
of its own (:git-status-branch), then its upstream and
how far ahead and behind it is, when it has one, then what git is in the
middle of and the keys going on with it, when it is."
  (append (list (list "Head:     "
                      (cons (if (git-state-branch state) (escape-controls (git-state-branch state)) "(detached)") :git-status-branch)
                      (format nil "  ~a" (if (git-state-unborn-p state)
                                             "(no commit yet)"
                                             (escape-controls (or (second (first (git-state-recent state))) ""))))))
          (and (git-state-upstream state)
               (list (format nil "Upstream: ~a~:[~*~*~; (~d ahead, ~d behind)~]" (git-state-upstream state)
                             (plusp (+ (git-state-ahead state) (git-state-behind state)))
                             (git-state-ahead state) (git-state-behind state))))
          (let ((words (assoc (git-state-doing state) *git-operation-words*)))
            (and words (list (format nil "Doing:    ~a (~a)" (second words) (substitute-command-keys (third words))))))))

(defun git-staged-face (kind)
  "The face of a file staged as KIND says: a deletion's own, else the one of every file staged."
  (if (string= kind "deleted") :git-status-deleted :git-status-staged))

(defun git-more-line (line left step what entry)
  "Give LINE, a function of a line's text, face and entry (ENTRY-LINES), the
line after a part of a status cut short: that LEFT more of WHAT (a format
given LEFT, its first part the indent) are not shown, and that
\\[git-toggle-diff] on it shows as many more as STEP, at most LEFT; ENTRY is its
entry, (:MORE SECTION) after a section's files, (:MORE SECTION PATH) after a
file's changes."
  (funcall line (format nil "~@?: TAB here shows ~:d more" what left (min step left))
           :list-dim entry))

(defun git-section-shown (files section limits)
  "Those of FILES, of SECTION of a status, that it lists, as many as LIMITS
has for (SECTION), *GIT-SECTION-LINES* when it has none, and how many more
there are, as two values."
  (let* ((limit (gethash (list section) limits *git-section-lines*))
         (left (max 0 (- (length files) limit))))
    (values (if (plusp left) (subseq files 0 limit) files) left)))

(defun git-status-sections (state limits)
  "The sections of a status of STATE, for SECTIONED-PIECES, each (SECTION .
MEMBERS), those with nothing in them left out: the files not tracked,
changed and staged, as many as GIT-SECTION-SHOWN lists, its submodules, its
stashes, its commits not pushed, its latest commits, and the files it
ignores."
  (flet ((section (name members) (and members (list (cons name members)))))
    (append (loop for (section) in *git-sections*
                  append (section section (git-section-shown (git-section-files state section) section limits)))
            (section :submodules (git-state-submodules state))
            (section :stashes (git-state-stashes state))
            (section :unpushed (git-state-unpushed state))
            (section :recent (git-state-recent state))
            (section :ignored (git-section-shown (git-state-ignored state) :ignored limits)))))

(defun git-status-section-title (state section)
  "The title of SECTION of a status of STATE."
  (case section
    (:submodules "Submodules")
    (:stashes "Stashes")
    (:unpushed (if (git-state-unpushed-to state) (format nil "Unpushed to ~a" (git-state-unpushed-to state)) "Unpushed, on no remote"))
    (:recent "Recent commits")
    (:ignored "Ignored files")
    (t (second (assoc section *git-sections*)))))

(defun git-status-section-count (state section members)
  "How many SECTION of a status of STATE holds, MEMBERS of them shown: all
its files, all its commits not pushed."
  (case section
    (:unpushed (git-state-unpushed-count state))
    ((:unmerged :untracked :unstaged :staged :ignored) (length (git-section-files state section)))
    (t (length members))))

(defun git-status-row (member section shown limits)
  "The lines of MEMBER of SECTION of a status, as pieces and entries
\(ENTRY-LINES): a file (staged ones in GIT-STAGED-FACE's face), its changes
under it when SHOWN has them, as many lines of them as LIMITS has for the
file (*GIT-DIFF-LINES* when it has none) then a line saying how many more
there are; an ignored file; a submodule, a stash, or a commit."
  (entry-lines
   (lambda (line)
     (case section
       (:submodules (destructuring-bind (path kind) member
                      (funcall line (format nil "  ~a ~a" (column-text kind 16 :elide nil) (escape-controls path)) nil (list :submodule path))))
       (:stashes (destructuring-bind (name subject) member
                   (funcall line (format nil "  ~a ~a" name (escape-controls subject)) :git-status-commit (list :stash name))))
       ((:unpushed :recent) (destructuring-bind (hash subject) member
                              (funcall line (format nil "  ~a ~a" hash (escape-controls subject)) :git-status-commit (list :commit hash))))
       (:ignored (destructuring-bind (path kind) member
                   (funcall line (format nil "  ~a ~a" (column-text kind 10 :elide nil) (escape-controls path)) :list-dim (list :file :ignored path))))
       (t (destructuring-bind (path kind &optional original) member
            (let* ((key (cons section path))
                   (diff (gethash key shown)))
              (funcall line (format nil "  ~a ~@[~a -> ~]~a" (column-text kind (if (eq section :unmerged) 15 10) :elide nil) (and original (escape-controls original)) (escape-controls path))
                       (and (eq section :staged) (git-staged-face kind))
                       (list :file section path))
              (when diff
                (let ((limit (gethash key limits *git-diff-lines*))
                      (total (loop for hunk in (rest diff) sum (length hunk)))
                      (count 0))
                  (block lines
                    (loop for hunk in (rest diff)
                          for h from 0
                          do (loop for l in hunk
                                   for i from 0
                                   do (when (>= count limit) (return-from lines))
                                      (funcall line (format nil "    ~a" (escape-controls l)) (git-diff-face l)
                                               (list :hunk section path h i))
                                      (incf count))))
                  (when (< count total)
                    (git-more-line line (- total count) *git-diff-lines* "    ... ~:d more line~:p of changes"
                                   (list :more section path))))))))))))

(defun git-status-after (state section members)
  "The line after the MEMBERS of SECTION of a status of STATE, as pieces and
entries (ENTRY-LINES), when it shows only some of them: how many more files
\(GIT-MORE-LINE), how many more commits not pushed."
  (entry-lines
   (lambda (line)
     (case section
       (:unpushed
        (let ((left (- (git-state-unpushed-count state) (length members))))
          (when (plusp left)
            (funcall line (format nil "  ... and ~d more (~a lists them all)" left
                                  (substitute-command-keys "\\<git-status-mode>\\[git-log-menu]"))
                     :list-dim))))
       ((:unmerged :untracked :unstaged :staged :ignored)
        (let ((left (- (or (git-status-section-count state section members) 0) (length members))))
          (when (plusp left)
            (git-more-line line left *git-section-lines* "  ... ~:d more file~:p" (list :more section)))))))))

(defun git-status-render (buffer &optional (point :keep))
  "Fill BUFFER, a status, with its repository's state as last read: its head,
then its sections (GIT-STATUS-SECTIONS), each under a heading saying how many
it holds and folded as in any list in sections (SECTIONED-PIECES)."
  (let ((state (tuatara.buffer:buffer-local buffer 'git-state))
        (shown (tuatara.buffer:buffer-local buffer 'git-shown))
        (limits (or (tuatara.buffer:buffer-local buffer 'git-shown-limits)
                    (setf (tuatara.buffer:buffer-local buffer 'git-shown-limits) (make-hash-table :test 'equal)))))
    (multiple-value-bind (head-pieces head-entries)
        (entry-lines (lambda (line)
                       (if state
                           (progn (mapc line (git-status-head-lines state)) (funcall line ""))
                           (funcall line "Reading the repository..."))))
      (multiple-value-bind (entries pieces)
          (if state
              (sectioned-pieces buffer (git-status-sections state limits)
                                (lambda (section) (git-status-section-title state section))
                                (lambda (member section) (git-status-row member section shown limits))
                                :count (lambda (section members) (git-status-section-count state section members))
                                :after (lambda (section members) (git-status-after state section members)))
              (values '() '()))
        (render-pieces buffer (append head-pieces pieces) :point point
                                                           :entries (concatenate 'vector head-entries entries)
                                                           :refill (lambda (b) (git-refresh-status b))
                                                           :redraw 'git-status-render)))))

(defparameter *git-watched-files* '("index" "HEAD" "logs/HEAD" "logs/refs/stash")
  "The files in git's own folder whose change means the repository changed:
what is staged, the commit checked out, where HEAD has been, and the
stashes. Git changes them whenever it commits, stages, checks out, resets or
stashes, run from the editor or from anywhere else.")

(defun git-stamps (folder)
  "The stamps of *GIT-WATCHED-FILES* in git's own FOLDER, NIL for one not there."
  (mapcar (lambda (file) (ignore-errors (tuatara.io:path-stamp (concatenate 'string folder file) :follow nil)))
          *git-watched-files*))

(defun git-folders-of (folder)
  "The folders in git's own FOLDER that *GIT-WATCHED-FILES* are in, those there."
  (remove-duplicates
   (loop for file in *git-watched-files*
         for directory = (concatenate 'string folder (directory-namestring file))
         when (eq :directory (tuatara.io:path-kind directory))
           collect directory)
   :test #'string= :from-end t))

(defun git-shown-paths (buffer key)
  "The paths whose changes the status BUFFER shows under the file KEY, (SECTION . PATH), names (GIT-ENTRY-PATHS)."
  (let* ((state (tuatara.buffer:buffer-local buffer 'git-state))
         (file (and state (assoc (cdr key) (git-section-files state (car key)) :test #'string=))))
    (if file (git-entry-paths file) (list (cdr key)))))

(defun git-refresh-status (buffer)
  "Read BUFFER's repository again in the background, with the changes of the
files it shows them of, and show it once read; a file with no changes left in
its section shows none. The stamps of git's own files are kept, and the
folders they are in watched, for GIT-WATCH-TICK to see when the repository
changes."
  (let ((top (tuatara.buffer:buffer-local buffer 'git-top))
        (shown (mapcar (lambda (key) (cons key (git-shown-paths buffer key)))
                       (alexandria:hash-table-keys (tuatara.buffer:buffer-local buffer 'git-shown))))
        (folder (tuatara.buffer:buffer-local buffer 'git-folder))
        (previous (tuatara.buffer:buffer-local buffer 'git-state)))
    (setf (tuatara.buffer:buffer-local buffer 'git-reading) t
          (tuatara.buffer:buffer-local buffer 'git-read-at) (get-internal-real-time))
    (start-git-job "Reading the repository"
                   (lambda (job)
                     (let* ((folder (or folder (git-folder job top)))
                            (state (git-read-state job top folder :previous previous))
                            (diffs (loop for (key . paths) in shown
                                         collect (cons key (git-read-diff job top (car key) paths)))))
                       (values state diffs folder (git-stamps folder) (git-folders-of folder))))
                   (lambda (state diffs folder stamps watched)
                     (when (live-buffer-p buffer)
                       (let ((shown (tuatara.buffer:buffer-local buffer 'git-shown)))
                         (loop for (key . diff) in diffs
                               do (if (rest diff) (setf (gethash key shown) diff) (remhash key shown))))
                       (setf (tuatara.buffer:buffer-local buffer 'git-state) state
                             (tuatara.buffer:buffer-local buffer 'git-folder) folder
                             (tuatara.buffer:buffer-local buffer 'git-stamps) stamps
                             (tuatara.buffer:buffer-local buffer 'git-watched) watched)
                       (git-status-render buffer)))
                   :after (lambda () (when (live-buffer-p buffer) (setf (tuatara.buffer:buffer-local buffer 'git-reading) nil)))
                   :quiet t :buffer buffer)))

(defvar *git-watch-seconds* 2
  "The least time, in seconds, between two readings of a status on the screen
when git changes its repository, from a terminal too (GIT-WATCH-TICK), or NIL
for a status never to follow it: then \\<git-status-mode>\\[revert-buffer] reads it again.")

(defun git-statuses-shown ()
  "The statuses shown in the editor's windows, each once."
  (and *editor*
       (remove-duplicates
        (loop for window in (editor-windows *editor*)
              for buffer = (tuatara.display:window-buffer window)
              when (typep (buffer-major-mode buffer) 'git-status-mode)
                collect buffer))))

(defun git-watched-folders ()
  "The folders of git's own files (*GIT-WATCHED-FILES*) of each status shown,
to be watched on disk while *GIT-WATCH-SECONDS* is true."
  (and *git-watch-seconds*
       (loop for buffer in (git-statuses-shown)
             append (tuatara.buffer:buffer-local buffer 'git-watched))))

(defun git-folders-changed (changed)
  "Note that the statuses shown whose git folders are among CHANGED are to be
looked at again, and look at them (GIT-WATCH-TICK)."
  (dolist (buffer (git-statuses-shown))
    (when (intersection (tuatara.buffer:buffer-local buffer 'git-watched) changed :test #'string=)
      (setf (tuatara.buffer:buffer-local buffer 'git-disk-changed) t)))
  (git-watch-tick *editor*))

(defvar *git-statuses-followed* '()
  "The statuses that were shown when GIT-WATCH-TICK last looked.")

(defun git-watch-tick (editor)
  "Tick function: read again each status shown whose repository changed on
disk since it was read, as when git ran in a terminal (*GIT-WATCHED-FILES*),
no sooner than *GIT-WATCH-SECONDS* after its last reading; a status shown
again is looked at once."
  (declare (ignore editor))
  (let ((soonest nil) (shown (git-statuses-shown)))
    (when *git-watch-seconds*
      (dolist (buffer shown)
        (unless (member buffer *git-statuses-followed*)
          (setf (tuatara.buffer:buffer-local buffer 'git-disk-changed) t))))
    (setf *git-statuses-followed* shown)
    (dolist (buffer shown soonest)
      (when (tuatara.buffer:buffer-local buffer 'git-disk-changed)
        (let ((folder (tuatara.buffer:buffer-local buffer 'git-folder))
              (left (if *git-watch-seconds*
                        (- *git-watch-seconds* (tuatara.runtime:seconds-since (or (tuatara.buffer:buffer-local buffer 'git-read-at) 0)))
                        0)))
          (cond ((tuatara.buffer:buffer-local buffer 'git-reading) (setf soonest t))
                ((and folder (equalp (git-stamps folder) (tuatara.buffer:buffer-local buffer 'git-stamps)))
                 (setf (tuatara.buffer:buffer-local buffer 'git-disk-changed) nil))
                ((plusp left) (unless (eq soonest t) (setf soonest (min left (or soonest left)))))
                (t (setf (tuatara.buffer:buffer-local buffer 'git-disk-changed) nil)
                   (git-refresh-status buffer))))))))

(defun git-buffer-of (top mode)
  "The buffer in MODE kept for the repository at TOP (its status, its
commit's message), when there is one."
  (find-if (lambda (b) (and (typep (buffer-major-mode b) mode)
                            (equal (tuatara.buffer:buffer-local b 'git-top) top)))
           tuatara.buffer:*buffers*))

(defun git-buffer-name (control top)
  "A name for a new buffer of the repository at TOP: CONTROL, a format, given
the name of the repository's folder, with <2>, <3>... after it when another
repository's buffer has that name."
  (tuatara.buffer:unique-buffer-name (format nil control (tuatara.io:native-file-name (string-right-trim "/" top)))))

(defun git-status-buffer (top)
  "The status of the repository at TOP, made when there is none, the files
it ignores folded away."
  (or (git-buffer-of top 'git-status-mode)
      (let ((buffer (find-or-make-buffer (git-buffer-name "*git: ~a*" top) :mode 'git-status-mode)))
        (setf (tuatara.buffer:buffer-local buffer 'git-shown) (make-hash-table :test 'equal)
              (tuatara.buffer:buffer-local buffer 'list-folded) (list :ignored))
        (git-set-top buffer top)
        buffer)))

(defun git-show-status (top)
  "Show the status of the repository or worktree at TOP, asking first
whether you trust it when its project is not trusted."
  (when (git-trusted-p top)
    (let ((buffer (git-status-buffer top)))
      (git-status-render buffer 0)
      (show-buffer buffer)
      (git-refresh-status buffer))))

(define-command git ()
  "Show the state of the git repository the current file (or folder) is in:
the branch, the files not tracked, changed and staged, the latest commits;
\\[describe-bindings] there lists its keys. Asks first whether you trust a
project not trusted: git runs programs it names."
  (let ((top (git-top-said (current-buffer))))
    (when top (git-show-status top))))

(defun git-after-save (buffer)
  "After BUFFER is saved: the status of its repository, when shown, looked at again."
  (let ((top (and (tuatara.buffer:buffer-file buffer) (git-top buffer))))
    (when top (git-refresh-repository top))))

;;; Acting on files

(defun git-files (entry)
  "The files ENTRY of a status is about, each (PATH KIND) as its section
holds them, and their section, as two values: a file's line, the file; a
section's title, all its files; the title of a section of commits, stashes
or submodules, none."
  (let ((state (tuatara.buffer:buffer-local (current-buffer) 'git-state)))
    (when state
      (case (first entry)
        ((:file :hunk) (values (list (or (assoc (third entry) (git-section-files state (second entry)) :test #'string=)
                                         (list (third entry) "changed")))
                               (second entry)))
        (:section (when (member (second entry) '(:unmerged :untracked :unstaged :staged :ignored))
                    (values (git-section-files state (second entry)) (second entry))))))))

(defun git-unborn-here-p ()
  "True when the repository of the current status has no commit yet."
  (let ((state (tuatara.buffer:buffer-local (current-buffer) 'git-state)))
    (and state (git-state-unborn-p state))))

(defparameter *git-paths-from-input* (list "--pathspec-from-file=-" "--pathspec-file-nul")
  "The arguments telling git to take the paths it acts on from its standard
input (GIT-PATHS-INPUT), however many there are.")

(defun git-paths-input (paths)
  "PATHS as git reads them with *GIT-PATHS-FROM-INPUT*: each ended by a NUL."
  (with-output-to-string (out)
    (dolist (path paths) (write-string path out) (write-char (code-char 0) out))))

(defun git-lines-selected-p (buffer)
  "True when BUFFER's selection chooses lines: it spans more than one line, or
covers its line from start to end, as selecting lines does; a selection
inside a line, as a search leaves its match, chooses none."
  (and (tuatara.buffer:buffer-mark-active-p buffer)
       (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
         (or (/= (tuatara.buffer:buffer-line-number buffer start) (tuatara.buffer:buffer-line-number buffer end))
             (and (= start (tuatara.buffer:buffer-line-start buffer start))
                  (= end (tuatara.buffer:buffer-line-end buffer start))
                  (< start end))))))

(defun git-entries-selected (buffer)
  "The entries of the lines of BUFFER, a status, that its selection covers,
the line its end is at the start of left out."
  (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
    (loop for line from (tuatara.buffer:buffer-line-number buffer start)
            to (tuatara.buffer:buffer-line-number buffer (max start (1- end)))
          for entry = (list-entry-at buffer (tuatara.buffer:buffer-goto-line buffer line))
          when entry collect entry)))

(defun git-hunks-chosen ()
  "The hunks of the current status chosen, as two values: a list of (PATH .
HUNKS), each of HUNKS (H . LINES), H the hunk's number in the file's changes
and LINES the numbers of its lines chosen, NIL for all of them; and their
section. With lines selected in hunks (GIT-LINES-SELECTED-P), those lines (or
the hunks whose @@ lines alone are selected); else the hunk on point's line.
NIL when no hunk is chosen."
  (let* ((buffer (current-buffer))
         (selected (git-lines-selected-p buffer))
         (entries (remove :hunk (if selected (git-entries-selected buffer) (list (list-entry-at buffer)))
                          :key #'first :test-not #'eq))
         (section (second (first entries)))
         (entries (remove section entries :key #'second :test-not #'eq))
         (lines (and selected (remove 0 entries :key #'fifth)))
         (whole (null lines))
         (chosen '()))
    ;; each line's entry is there once
    (dolist (entry (if whole entries lines))
      (destructuring-bind (path h i) (cddr entry)
        (let* ((file (or (assoc path chosen :test #'string=)
                         (let ((new (list path))) (push new chosen) new)))
               (hunk (or (assoc h (rest file))
                         (let ((new (list h))) (push new (rest file)) new))))
          (unless whole (push i (rest hunk))))))
    (values (nreverse chosen) section)))

(defun git-conflicted-file (chosen section)
  "The first of the files CHOSEN (GIT-HUNKS-CHOSEN) of SECTION of the current
status that git could not join (unmerged), or NIL."
  (and (eq section :unmerged) (first (first chosen))))

(defun hunk-changes-among (hunk numbers)
  "How many of the lines of HUNK, a list of its lines, at NUMBERS (from 0) are
changes: added (+) or taken away (-); the hunk's lines looked at once each."
  (let ((lines (coerce hunk 'vector)))
    (count-if (lambda (i)
                (and (< -1 i (length lines))
                     (let ((line (aref lines i)))
                       (or (uiop:string-prefix-p "+" line) (uiop:string-prefix-p "-" line)))))
              numbers)))

(defun git-hunks-words (chosen section)
  "What CHOSEN of SECTION, as GIT-HUNKS-CHOSEN gives it, is called: the hunk
of a.txt, 2 hunks of a.txt, 3 lines of 2 files, the lines counted those
that are changes."
  (let* ((shown (tuatara.buffer:buffer-local (current-buffer) 'git-shown))
         (hunks (loop for file in chosen append (rest file)))
         (lines (loop for (path . file-hunks) in chosen
                      for diff = (gethash (cons section path) shown)
                      sum (loop with hunks = (coerce (rest diff) 'vector)
                                for (h . numbers) in file-hunks
                                sum (hunk-changes-among (and (< h (length hunks)) (aref hunks h)) numbers)))))
    (format nil "~:[~:[~d hunks~;the hunk~*~]~;~*~d line~:p~] of ~:[~d files~;~*~a~]"
            (plusp lines) (null (rest hunks)) (if (plusp lines) lines (length hunks))
            (null (rest chosen)) (length chosen) (escape-controls (first (first chosen))))))

(defun git-hunks-patch (chosen section reverse)
  "The patch making the changes CHOSEN (GIT-HUNKS-CHOSEN) of SECTION of the
current status, as it shows them, for applying backwards when REVERSE; NIL
when it makes none."
  (let* ((shown (tuatara.buffer:buffer-local (current-buffer) 'git-shown))
         (patches (loop for (path . hunks) in chosen
                        for diff = (gethash (cons section path) shown)
                        for made = (loop for (h . lines) in (sort (copy-list hunks) #'< :key #'first)
                                         for hunk = (nth h (rest diff))
                                         for patch = (and hunk (git-hunk-patch hunk :lines lines :reverse reverse))
                                         when patch collect patch)
                        when made collect (git-patch (first diff) made))))
    (and patches (format nil "~{~a~}" patches))))

(defun git-apply-hunks (chosen section how doing done)
  "Apply the changes CHOSEN (GIT-HUNKS-CHOSEN) of SECTION of the current
status HOW (GIT-APPLY-ARGUMENTS) as a job doing DOING, then say DONE, a
format given what they are called (GIT-HUNKS-WORDS); the selection ends. A
file git could not join is refused: it is staged whole once put right."
  (let ((conflicted (git-conflicted-file chosen section))
        (patch (git-hunks-patch chosen section (not (eq how :stage)))))
    (cond (conflicted
           (message (substitute-command-keys "~a is in conflict: put it right, then \\<git-status-mode>\\[git-stage] on its line stages it whole"
                                            :for-format t)
                    (escape-controls conflicted)))
          ((null patch) (message "No change among the lines selected"))
          (t (when (git-lines-selected-p (current-buffer)) (drop-selection))
             (git-run doing (git-here) (git-apply-arguments how) (format nil done (git-hunks-words chosen section)) :input patch)))))

(define-command git-stage ()
  "Stage the file on the line of the status, or on a section's title all of
its files; on a hunk, the hunk; with lines selected in hunks, those lines."
  (multiple-value-bind (chosen section) (git-hunks-chosen)
    (if chosen
        (if (eq section :staged)
            (message "Staged already")
            (git-apply-hunks chosen section :stage "Staging" "Staged ~a"))
        (multiple-value-bind (files section) (git-files (list-entry-at))
          (cond ((null files) (message "No file on this line"))
                ((eq section :staged) (message "Staged already"))
                ((eq section :ignored) (message "Ignored by git: it stages none of these"))
                (t (git-run "Staging" (git-here) (list* "add" *git-paths-from-input*)
                            (format nil "Staged ~d file~:p" (length files))
                            :input (git-paths-input (mapcar #'first files)))))))))

(defun git-unborn-p (job top)
  "On JOB's thread: true when the repository at TOP has no commit yet."
  (not (nth-value 1 (git-job-try job top (list "rev-parse" "--verify" "-q" "HEAD")))))

(defun git-unstage-paths (top paths done)
  "Take PATHS, files of the repository at TOP, out of what is staged, in the
background: back to HEAD, or out of the index altogether in a repository
with no commit yet; say DONE once done."
  (start-git-job "Unstaging"
                 (lambda (job)
                   (git-job-output job top (append (if (git-unborn-p job top) '("rm" "--cached" "-r" "-q") '("restore" "--staged"))
                                                   *git-paths-from-input*)
                                   :input (git-paths-input paths)))
                 (lambda (text) (declare (ignore text)) (message "~a" done))
                 :after (lambda () (git-refresh-repository top))))

(define-command git-unstage ()
  "Unstage the file on the line of the status, or on a section's title all of
its files, a rename by both its names; on a hunk, the hunk; with lines
selected in hunks, those lines."
  (multiple-value-bind (chosen section) (git-hunks-chosen)
    (if chosen
        (if (eq section :staged)
            (git-apply-hunks chosen section :unstage "Unstaging" "Unstaged ~a")
            (message "Not staged"))
        (multiple-value-bind (files section) (git-files (list-entry-at))
          (cond ((null files) (message "No file on this line"))
                ((not (eq section :staged)) (message "Not staged"))
                (t (git-unstage-paths (git-here) (mapcan #'git-entry-paths files) (format nil "Unstaged ~d file~:p" (length files)))))))))

(defun git-file-inside-p (top path &optional (real-top (real-folder top)))
  "True when the file PATH, relative to TOP, the top of its repository, is
reached from TOP through folders alone, none of them a link, as far as where
they really are tells; REAL-TOP is where TOP really is."
  (let* ((name (string-right-trim "/" path))
         (slash (position #\/ name :from-end t)))
    (and real-top
         (equal (real-folder (tuatara.io:parent-path (concatenate 'string top name)))
                (concatenate 'string real-top (if slash (subseq name 0 (1+ slash)) ""))))))

(defun git-untracked-among (job top paths)
  "On JOB's thread: those of PATHS, files of the repository at TOP, that git
does not track, ignored ones too, as an EQUAL hash table of them, a folder
ending in / there when git tracks none of its files; git asked of a thousand
at a time."
  (let ((untracked (make-hash-table :test 'equal)))
    (loop for rest = paths then (nthcdr 1000 rest)
          while rest
          do (dolist (path (uiop:split-string (git-job-output job top (list* "ls-files" "-z" "--others" "--directory" "--"
                                                                              (subseq rest 0 (min 1000 (length rest)))))
                                              :separator (list (code-char 0))))
               (setf (gethash path untracked) t)))
    untracked))

(defun git-trash-untracked (job top paths)
  "On JOB's thread: move to the trash those of PATHS, files of the repository
at TOP, that git still does not track and that are inside it (GIT-FILE-INSIDE-P),
a link moved as a link, never what it points to. Two values: the paths moved,
and those left as they are, each (PATH WHY)."
  (let ((untracked (git-untracked-among job top paths))
        (real-top (real-folder top))
        (moved '()) (left '()))
    (dolist (path paths)
      (cond ((not (gethash path untracked)) (push (list path "git tracks it now, or it is gone") left))
            ((not (git-file-inside-p top path real-top)) (push (list path "it is reached through a link") left))
            (t (handler-case (progn (tuatara.io:trash-path (concatenate 'string top path)) (push path moved))
                 (tuatara.io:file-system-error (e) (push (list path (princ-to-string e)) left))))))
    (values (nreverse moved) (nreverse left))))

(defun git-reverse-work (top files section unborn)
  "The work, a function of the job, reversing the changes of FILES, each
\(PATH KIND), of SECTION of the status of the repository at TOP, UNBORN when
it has no commit yet: files git does not track, and new files staged once
taken out of the index, are moved to the trash (GIT-TRASH-UNTRACKED); the
changes of the others are undone in the file, and for those staged in the
index too, a rename under both its names; a submodule is left as it is. It
gives the paths whose changes were undone, those moved to the trash, and
those left as they are, each (PATH WHY)."
  (lambda (job)
    (let* ((submodules (remove "submodule" files :key #'second :test-not #'string=))
           (files (remove "submodule" files :key #'second :test #'string=))
           (new (mapcar #'first (case section
                                  ((:untracked :ignored) files)
                                  (:staged (if unborn files (remove "new file" files :key #'second :test-not #'string=))))))
           (changed (and (not (member section '(:untracked :ignored)))
                         (let ((new-set (make-hash-table :test 'equal)))
                           (dolist (path new) (setf (gethash path new-set) t))
                           (mapcan (lambda (file) (and (not (gethash (first file) new-set)) (git-entry-paths file)))
                                   files)))))
      (when changed
        (git-job-output job top (append (list "restore") (and (eq section :staged) (list "--staged" "--worktree"))
                                        *git-paths-from-input*)
                        :input (git-paths-input changed)))
      (when (and new (eq section :staged))
        (git-job-output job top (list* "rm" "--cached" "-r" "-q" *git-paths-from-input*) :input (git-paths-input new)))
      (multiple-value-bind (moved left) (if new (git-trash-untracked job top new) (values '() '()))
        (values (remove-if (lambda (path) (some (lambda (file) (equal path (third file))) files)) changed)
                moved
                (append (mapcar (lambda (file) (list (first file) "a submodule, whose changes are its own repository's"))
                                submodules)
                        left))))))

(defun git-reverse-words (changed moved left)
  "What is said once changes are reversed: of how many files the changes
were undone (CHANGED), how many were moved to the trash (MOVED), and each
file left as it is and why (LEFT, each (PATH WHY))."
  (let ((parts (append (and changed (list (format nil "reversed the changes of ~d file~:p" (length changed))))
                       (and moved (list (format nil "moved ~d file~:p to the trash" (length moved))))
                       (mapcar (lambda (file) (format nil "left ~a as it is: ~a" (escape-controls (first file)) (second file)))
                               left))))
    (if parts
        (let ((words (format nil "~{~a~^; ~}" parts)))
          (concatenate 'string (string-upcase (subseq words 0 1)) (subseq words 1)))
        "Nothing reversed")))

(define-command git-reverse ()
  "Reverse the changes of the file on the line of the status (on a
section's title, of all its files), after asking, which says it cannot be
undone: git kept no copy of them. A file not tracked, or a new file staged,
is moved to the trash instead, from where it can be put back; a staged
change is reversed in the index and the file both, a rename under both its
names; a submodule is left as it is. On a hunk, its changes alone; with
lines selected in hunks, those lines."
  (multiple-value-bind (chosen section) (git-hunks-chosen)
    (if chosen
        (when (yes-or-no-p* (format nil "Reverse ~a~:[~;, staged and in the file~]? It cannot be undone" (git-hunks-words chosen section) (eq section :staged)))
          (git-apply-hunks chosen section (if (eq section :staged) :reverse-staged :reverse) "Reversing changes" "Reversed ~a"))
        (git-reverse-files))))

(defun git-reverse-files ()
  "Reverse the changes of the file on the line of the status, or of all the
files of the section whose title it is, after asking, as \\[git-reverse] does."
  (multiple-value-bind (files section) (git-files (list-entry-at))
    (cond ((null files) (message "No file on this line"))
          ((yes-or-no-p* (if (member section '(:untracked :ignored))
                             (format nil "Move to the trash ~:[~*~a~;~d files~*~]?"
                                     (rest files) (length files) (escape-controls (first (first files))))
                             (format nil "Reverse the changes of ~:[~*~a~;~d files~*~]~:[~;, staged and in the file~]? It cannot be undone"
                                     (rest files) (length files) (escape-controls (first (first files))) (eq section :staged))))
           (let ((top (git-here)))
             (start-git-job "Reversing changes" (git-reverse-work top files section (git-unborn-here-p))
                            (lambda (changed moved left)
                              (when moved (trash-changed))
                              (message "~a" (git-reverse-words changed moved left)))
                            :after (lambda () (git-refresh-repository top))))))))

(define-command git-toggle-diff ()
  "Show the changes of the file on the line of the status under it, in hunks,
the first *GIT-DIFF-LINES* of them, or no longer; on the line saying more
lines or files are not shown, show more of them; on a section's heading, or
another line of it (a commit, a stash, a file ignored), fold the section or
unfold it, as in every list in sections (\\[list-toggle-fold]); the files
ignored are folded at first, and read once opened."
  (let* ((entry (list-entry-at))
         (status (current-buffer))
         (shown (tuatara.buffer:buffer-local status 'git-shown))
         (limits (tuatara.buffer:buffer-local status 'git-shown-limits))
         (key (and (member (first entry) '(:file :hunk :more)) (cons (second entry) (third entry))))
         (top (git-here)))
    (cond ((eq (first entry) :more)
           (let ((step (if (cdr key) *git-diff-lines* *git-section-lines*)))
             (setf (gethash key limits) (+ (gethash key limits step) step)))
           (git-status-render status))
          ((or (null key) (eq (car key) :ignored))
           (let ((section (list-section-at status)))
             (if section
                 (toggle-list-section status section)
                 (message "No file on this line"))))
          ((gethash key shown) (remhash key shown) (remhash key limits) (git-status-render status))
          ((eq (second entry) :untracked) (message "Not tracked: no changes to show"))
          (t (let ((paths (git-shown-paths status key)))
               (start-git-job "Reading the changes" (lambda (job) (git-read-diff job top (car key) paths))
                              (lambda (diff)
                                (when (live-buffer-p status)
                                  (cond ((rest diff) (setf (gethash key shown) diff) (git-status-render status))
                                        ((git-diff-binary-p diff)
                                         (message "~a is binary: its changes are not shown as lines" (escape-controls (cdr key))))
                                        (t (message "No changes to show")))))
                              :quiet t))))))

(define-command git-next-section ()
  "Go to the next file or hunk of the status, or the next title."
  (git-go-to-section 1))

(define-command git-previous-section ()
  "Go to the previous file or hunk of the status, or the previous title."
  (git-go-to-section -1))

(defun git-go-to-section (direction)
  "Put point on the next line of the status (DIRECTION 1), or the previous
\(-1), that begins something: a section's title, a file, a hunk or a commit."
  (unless (list-step-entry (current-buffer) direction
                           :test (lambda (entry) (or (not (eq (first entry) :hunk)) (zerop (fifth entry)))))
    (message (if (plusp direction) "No more below" "No more above"))))

(define-command git-visit ()
  "Visit the file on the line of the status, at the line of the hunk's line
point is on, a file in conflict at its first conflict, its conflicts shown
\(\\[conflict-markers-mode]); or show the commit or the stash on it, or a
submodule's status; a file deleted is said to be. On a section's heading,
fold or unfold the section, as in every list in sections."
  (let ((entry (list-entry-at))
        (top (git-here)))
    (case (first entry)
      ((:file :hunk)
       (let ((path (concatenate 'string top (third entry)))
             (hunk (and (eq (first entry) :hunk)
                        (nth (fourth entry) (rest (gethash (cons (second entry) (third entry))
                                                          (tuatara.buffer:buffer-local (current-buffer) 'git-shown)))))))
         (cond ((not (tuatara.io:path-kind path :follow nil)) (message "~a is deleted" (escape-controls (third entry))))
               ((eq (second entry) :unmerged)
                ;; its conflicts shown, and point at the first
                (let ((buffer (visit-path path)))
                  (unless (tuatara.io:safe-visit-p buffer)
                    (conflict-look-through buffer :tell t :go t))))
               ((and (visit-path path) hunk)
                (goto-shown (current-buffer) (git-line-start (current-buffer) (git-hunk-line hunk (fifth entry))))))))
      (:commit (git-show-commit top (second entry)))
      (:stash (git-show-stash top (second entry)))
      (:submodule (git-show-status (concatenate 'string top (string-right-trim "/" (second entry)) "/")))
      (:section (toggle-list-section (current-buffer) (second entry)))
      (t (message "No file or commit on this line")))))

(defun git-line-of (buffer position)
  "The line of BUFFER holding POSITION as git numbers lines (1 the first),
by newlines alone: a carriage return that no newline follows ends a line in
the editor, but not in git's patches (TUATARA.TEXT:BYTE->ROW). Waits for the
lines of a large file to be counted. Complexity: O(log n)."
  (tuatara.buffer:ensure-indexed buffer)
  (1+ (tuatara.text:byte->row (tuatara.buffer:buffer-text buffer) position)))

(defun git-line-start (buffer line)
  "Where the line LINE (1 the first) of BUFFER as git numbers lines begins
\(GIT-LINE-OF); the end of the text when it has fewer. Complexity: O(log n)."
  (tuatara.buffer:ensure-indexed buffer)
  (tuatara.text:row->byte (tuatara.buffer:buffer-text buffer) (1- line)))

(defun git-file-hunk (how)
  "Stage (HOW :STAGE) or unstage (:UNSTAGE) the hunk of the current file at
point, as git has the file on disk, in the background."
  (let* ((buffer (current-buffer))
         (file (tuatara.buffer:buffer-file buffer))
         (top (and file (git-top-said buffer)))
         (line (git-line-of buffer (tuatara.buffer:buffer-point buffer))))
    (cond ((null top))
          ((tuatara.buffer:buffer-modified-p buffer) (message "Save it first: git takes the hunk from the file on disk"))
          ((git-trusted-p top)
           (start-git-job (if (eq how :stage) "Staging" "Unstaging")
                          (lambda (job)
                            (let* ((folder (tuatara.io:parent-path file))
                                   (diff (git-read-diff job folder (if (eq how :stage) :unstaged :staged)
                                                        (list (tuatara.io:native-file-name file))))
                                   (hunk (find-if (lambda (h) (git-hunk-covers-p h line)) (rest diff))))
                              (when hunk
                                (git-job-output job top (git-apply-arguments how) :input (git-patch (first diff) (list hunk))))
                              (and hunk t)))
                          (lambda (done)
                            (message (if done
                                         (if (eq how :stage) "Staged the hunk at line ~d" "Unstaged the hunk at line ~d")
                                         (if (eq how :stage) "No change at line ~d left to stage" "No change staged at line ~d"))
                                     line)
                            (git-refresh-repository top))
                          :buffer buffer)))))

(define-command git-stage-hunk ()
  "Stage the hunk at point: in the status (\\[git]) the one on point's line,
as \\<git-status-mode>\\[git-stage] does; in a file's own buffer, saved, the change git sees at
point's line, as the file is on disk."
  (if (typep (buffer-major-mode (current-buffer)) 'git-status-mode)
      (git-stage)
      (git-file-hunk :stage)))

(define-command git-unstage-hunk ()
  "Unstage the hunk at point: in the status (\\[git]) the one on point's
line, as \\<git-status-mode>\\[git-unstage] does; in a file's own buffer, saved, the change staged
at point's line."
  (if (typep (buffer-major-mode (current-buffer)) 'git-status-mode)
      (git-unstage)
      (git-file-hunk :unstage)))

;;; Buffers of git's own

(defun git-set-top (buffer top)
  "Make BUFFER one of the repository at TOP: git acts there on it, and files
asked for from it are looked for from its top."
  (setf (tuatara.buffer:buffer-local buffer 'git-top) top
        (tuatara.buffer:buffer-local buffer :directory) top))

(defun git-edit-buffer (control top mode text line &rest locals)
  "Show in another window a new buffer of the repository at TOP to edit, named
by CONTROL (GIT-BUFFER-NAME), holding TEXT, in MODE, point at the start of
LINE (0 the first), LOCALS, names and values in turn, set in it."
  (let ((buffer (find-or-make-buffer (git-buffer-name control top))))
    (tuatara.buffer:widen-buffer buffer)
    (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) text)
    (set-major-mode buffer mode)
    (git-set-top buffer top)
    (loop for (name value) on locals by #'cddr
          do (setf (tuatara.buffer:buffer-local buffer name) value))
    (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-goto-line buffer line))
    (pop-to-buffer buffer :other t)))

(defun git-give-up (local done elsewhere)
  "Kill the current buffer and say DONE when it holds LOCAL, the mark of a
buffer GIT-EDIT-BUFFER made; else say ELSEWHERE."
  (let ((buffer (current-buffer)))
    (if (tuatara.buffer:buffer-local buffer local)
        (progn (tuatara.buffer:kill-buffer buffer) (message "~a" done))
        (message "~a" elsewhere))))

;;; Commits and stashes shown

(defun git-commit-at-point ()
  "The hash of the commit on point's line of a status (its Recent commits) or
of a log, or NIL."
  (let ((entry (and (git-here) (list-entry-at))))
    (cond ((git-commit-p entry) (git-commit-short entry))
          ((eq (first entry) :commit) (second entry))
          ((and (typep (buffer-major-mode (current-buffer)) 'git-log-mode) (stringp (first entry))) (first entry)))))

(defun git-text-buffer (name top text &key (mode 'diff-mode))
  "A buffer NAME showing TEXT (a string or a rope), what git printed for the
repository at TOP, read-only, in MODE, coloured as MODE colours it but read
as a support buffer is in modal editing (moving, selecting, copying, nothing
that edits), q closing it (VIEW-MADE-TEXT)."
  (let ((buffer (buffer-in-mode name mode)))
    (set-made-text buffer text)
    (setf (tuatara.buffer:buffer-point buffer) 0)
    (git-set-top buffer top)
    (view-made-text buffer)))

(defun git-show-output (doing top arguments name &key empty (mode 'diff-mode) environment after)
  "Run git with ARGUMENTS (a list, or a function of the job giving one on its
thread) for the repository at TOP as a job doing DOING, and show what it
printed in the buffer NAME in MODE (GIT-TEXT-BUFFER), its text made on the
job's thread, no more than *GIT-SHOWN-LIMIT* of it; EMPTY is said when it
printed nothing. ENVIRONMENT is added to git's; AFTER, when given, is called
however it ended, else the work only reads."
  (start-git-job doing
                 (lambda (job)
                   (let ((arguments (if (functionp arguments) (funcall arguments job) arguments)))
                     (tuatara.text:octets->rope (coerce (git-job-octets job top arguments :environment environment
                                                                                         :limit *git-shown-limit*)
                                                        'tuatara.text:octets))))
                 (lambda (rope)
                   (if (and empty (zerop (tuatara.text:rope-bytes rope)))
                       (message "~a" empty)
                       (pop-to-buffer (git-text-buffer name top rope :mode mode) :other t)))
                 :after after :quiet (null after)))

(defun git-show-commit (top hash)
  "Show the commit HASH of the repository at TOP: what it says and its changes."
  (git-show-output "Reading the commit" top (list "show" "--no-color" "--no-ext-diff" "--stat" "--patch" "--end-of-options" hash)
                   (format nil "*git commit ~a*" (escape-controls hash))))

(commands-for-mode 'git-status-mode "a repository's status (\\[git])"
                   'git-stage 'git-unstage 'git-reverse 'git-toggle-diff 'git-next-section 'git-previous-section
                   'git-visit 'git-toggle-full-menus 'list-toggle-fold 'list-toggle-all-folds)

(commands-for-mode 'git-log-mode "a repository's log (\\[git-log-menu] in a status)" 'git-log-show)

(defun install-git ()
  "Bind \\`C-x g' to \\[git] and the keys of its status (j and k left to move
by lines, h to close and l to visit, as in every list); and let a save, and
git run from anywhere, look at its repository again (GIT-WATCH-TICK), the
folders of git's own files watched on disk for it."
  (define-key *global-keymap* "C-x g" 'git)
  (define-keys *git-status-keymap* '(("s" git-stage) ("u" git-unstage) ("x" git-reverse) ("TAB" git-toggle-diff)
                                     ("RET" git-visit) ("l" git-visit) ("h" quit-window)
                                     ("n" git-next-section) ("p" git-previous-section) ("S-TAB" list-toggle-all-folds)))
  (pushnew 'git-after-save *after-save-functions*)
  (pushnew 'git-watch-tick *tick-functions*)
  (follow-directories-for 'git-watched-folders 'git-folders-changed))
