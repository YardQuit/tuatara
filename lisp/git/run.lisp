;;;; lisp/git/run.lisp --- How the editor runs git: never through a shell, never on the editor's thread.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every git command of the editor goes through RUN-GIT: the change gutter's
;;;; looks at a file, rimu's git column, the packages fetched and C-x g. Git is
;;;; never asked to fetch objects by itself or to run a file-system monitor, a
;;;; pager or a signature check: a repository's own settings can name programs
;;;; for those, and a repository from an archive or a download is not to be
;;;; trusted.

(in-package #:tuatara.defaults)

(defparameter *git-safety* '("--no-lazy-fetch" "--literal-pathspecs" "-c" "core.fsmonitor=false" "-c" "log.showSignature=false" "-c" "core.pager=cat")
  "Options every git command gets (RUN-GIT): no fetching of objects a partial
clone lacks, no file-system monitor, no signatures checked and no pager; and
the files named taken as they are, never as patterns (a file called * is
that file alone), but in the commands *GIT-OWN-PATHSPECS* names.")

(defparameter *git-own-pathspecs* '("stash")
  "The git commands given no paths that run git again on paths of their own
\(:/ for the whole tree), run without --literal-pathspecs, which would take
those as names of files.")

(defun git-safety (arguments)
  "The options of *GIT-SAFETY* git is given to run with ARGUMENTS."
  (if (member (first (git-command-words arguments)) *git-own-pathspecs* :test #'equal)
      (remove "--literal-pathspecs" *git-safety* :test #'string=)
      *git-safety*))

(defparameter *git-commands-leaving-the-work-tree*
  '("status" "log" "diff" "show" "blame" "cat-file" "rev-parse" "ls-files" "ls-tree" "for-each-ref" "rev-list"
    "describe" "merge-base" "show-ref" "symbolic-ref" "config" "grep" "check-ignore" "check-attr" "shortlog"
    "name-rev" "var" "version" "help" "hash-object" "branch" "tag" "fetch" "push" "remote" "notes"
    "update-ref" "reflog" "count-objects" "fsck" "diff-tree" "diff-index" "diff-files" "cherry" "range-diff"
    "update-index" "write-tree" "commit-tree" "mktree" "ls-remote" "verify-commit" "verify-tag")
  "The git commands that write nothing in a repository's working files (its
.git and its index aside). Every other command is taken to write there
\(GIT-WRITES-WORK-TREE-P), so that a command not named here is held to it
too; add and commit among them, which run programs the repository names on
its working files (clean filters).")

(defparameter *git-options-taking-values*
  '("-c" "-C" "--git-dir" "--work-tree" "--namespace" "--super-prefix" "--config-env" "--attr-source")
  "The options before git's command that take the next word as their value
when it is not written after an = (-C FOLDER).")

(defun git-command-words (arguments)
  "ARGUMENTS of git with the options before the command (-c NAME=VALUE, -C
FOLDER, --git-dir DIR, --no-pager and the like) left out: the command, then
its own."
  (loop while (and arguments (uiop:string-prefix-p "-" (first arguments)))
        do (setf arguments (if (member (first arguments) *git-options-taking-values* :test #'string=)
                               (cddr arguments)
                               (rest arguments))))
  arguments)

(defun git-option-value (option words)
  "The values WORDS give OPTION (\"--output\"): written after an =, or as
the next word, each time it is given."
  (loop for (word next) on words
        when (string= word option) collect next
        when (uiop:string-prefix-p (concatenate 'string option "=") word)
          collect (subseq word (1+ (length option)))))

(defun git-places (directory arguments environment)
  "Where git run with ARGUMENTS in DIRECTORY and ENVIRONMENT (NAME=VALUE
strings) works, as three values, native names: the folder it runs in, each
-C taken in turn; the repository folder --git-dir or GIT_DIR names, or NIL;
the working folder --work-tree or GIT_WORK_TREE names, or NIL."
  (let ((here (tuatara.io:native directory)) (git-dir nil) (work-tree nil))
    (flet ((from-here (path) (tuatara.io:clean-path (if (uiop:string-prefix-p "/" path) path (tuatara.io:path-join here path)))))
      (loop for (name . more) on arguments
            while (uiop:string-prefix-p "-" name)
            do (cond ((and (string= name "-C") more) (setf here (from-here (first more))))
                     ((uiop:string-prefix-p "--git-dir=" name) (setf git-dir (subseq name 10)))
                     ((and (string= name "--git-dir") more) (setf git-dir (first more)))
                     ((uiop:string-prefix-p "--work-tree=" name) (setf work-tree (subseq name 12)))
                     ((and (string= name "--work-tree") more) (setf work-tree (first more)))))
      (dolist (setting environment)
        (cond ((uiop:string-prefix-p "GIT_DIR=" setting) (setf git-dir (subseq setting 8)))
              ((uiop:string-prefix-p "GIT_WORK_TREE=" setting) (setf work-tree (subseq setting 14)))))
      (values here (and git-dir (from-here git-dir)) (and work-tree (from-here work-tree))))))

(defun git-output-paths (words)
  "The files and folders git's command WORDS (GIT-COMMAND-WORDS) writes
elsewhere than in the repository: --output and --output-directory, -o of
format-patch and archive, config's --file or -f, the bundle bundle create
makes, the folder worktree add, move and remove name, the folder clone,
init and submodule add make. As written: relative ones are from where git
runs."
  (destructuring-bind (&optional command &rest more) words
    (let ((plain (remove-if (lambda (word) (uiop:string-prefix-p "-" word)) more)))
      (append (git-option-value "--output" more)
              (git-option-value "--output-directory" more)
              (and (member command '("format-patch" "archive") :test #'equal)
                   (append (git-option-value "-o" more)
                           (loop for word in more
                                 when (and (> (length word) 2) (string= "-o" word :end2 2)) collect (subseq word 2))))
              (and (equal command "config") (append (git-option-value "--file" more) (git-option-value "-f" more)))
              (and (equal command "bundle") (equal (first plain) "create") (list (second plain)))
              (and (equal command "worktree") (member (first plain) '("add" "move" "remove") :test #'string=)
                   (rest plain))
              (and (equal command "clone") (rest plain) (last plain))
              (and (equal command "init") (or plain (list ".")))
              (and (equal command "submodule") (equal (first plain) "add") (cddr plain))))))

(defun git-writes-work-tree-p (arguments)
  "True when git run with ARGUMENTS may write in the repository's working
files: every command but those of *GIT-COMMANDS-LEAVING-THE-WORK-TREE*, and
of the rest these only as they are given: apply with --cached or --check
\(not --index), reset without --hard, --merge, --keep or
--recurse-submodules, restore of --staged alone, rm --cached, stash list and
show, worktree list, submodule status and summary, bisect log,
sparse-checkout list, format-patch --stdout, bundle list-heads and verify,
and archive with no file to write."
  (destructuring-bind (&optional command &rest more) (git-command-words arguments)
    (flet ((given (&rest options) (some (lambda (option) (member option more :test #'string=)) options))
           (then (&rest words) (and more (member (first more) words :test #'string=))))
      (cond ((null command) nil)
            ((member command *git-commands-leaving-the-work-tree* :test #'string=) nil)
            ((string= command "apply") (not (and (given "--cached" "--check") (not (given "--index")))))
            ((string= command "reset") (given "--hard" "--merge" "--keep" "--recurse-submodules"))
            ((string= command "restore") (not (and (given "--staged" "-S") (not (given "--worktree" "-W")))))
            ((string= command "rm") (not (given "--cached")))
            ((string= command "stash") (not (then "list" "show")))
            ((string= command "worktree") (not (then "list")))
            ((string= command "submodule") (not (then "status" "summary")))
            ((string= command "bisect") (not (then "log")))
            ((string= command "sparse-checkout") (not (then "list")))
            ((string= command "format-patch") (not (given "--stdout")))
            ((string= command "bundle") (not (then "list-heads" "verify")))
            ((string= command "archive") (or (given "-o") (some (lambda (word) (uiop:string-prefix-p "--output" word)) more)))
            (t t)))))

(defun check-git-writing (directory arguments &optional environment)
  "Signal TUATARA.IO:WRITING-REFUSED, the folder named, when git run with
ARGUMENTS in DIRECTORY and ENVIRONMENT would write in a place nothing is
written in, an evidence folder: when its repository or working folder,
found from where it runs (-C, --git-dir, --work-tree, GIT_DIR,
GIT_WORK_TREE: GIT-PLACES), is in one, where it writes its .git; when it writes the repository's working files
\(GIT-WRITES-WORK-TREE-P) and the repository holds one
\(TUATARA.IO:CHECK-TREE-WRITING); or when a file or folder it is told to
write (GIT-OUTPUT-PATHS) is in one. Returns true when the repository holds
one, for its hooks to be left out (RUN-GIT-PROGRAM). Nothing is looked at
while no place is refused."
  (when (tuatara.io:writing-refused-anywhere-p)
    (let* ((words (git-command-words arguments))
           (doing (format nil "run git ~a in" (or (first words) ""))))
      (multiple-value-bind (here git-dir work-tree) (git-places directory arguments environment)
        (let ((top (or work-tree (ignore-errors (repository-top here)))))
          (dolist (place (remove nil (list git-dir top)))
            (tuatara.io:check-writing place doing))
          (dolist (path (git-output-paths words))
            (when (and path (plusp (length path)))
              (tuatara.io:check-tree-writing (if (uiop:string-prefix-p "/" path) path (tuatara.io:path-join here path))
                                             (format nil "have git ~a write" (first words)))))
          (when top
            (if (git-writes-work-tree-p arguments)
                (progn (tuatara.io:check-tree-writing top doing) nil)
                (folder-holds-evidence-p top))))))))

(defun git-hash-p (text &key short)
  "True when TEXT is a commit's full hash as git writes it: 40 hexadecimal
digits, or 64 in a repository of SHA-256 hashes; with SHORT, a hash cut as
short as 7 digits too. What a commit says of itself (its subject, its
author) is never taken for one."
  (and (stringp text)
       (if short (<= 7 (length text) 64) (member (length text) '(40 64)))
       (every (lambda (c) (digit-char-p c 16)) text)))

(defparameter *git-look-seconds* 30
  "How long git may take to tell the change gutter and rimu's lists what it
has of a file or folder before it is ended.")

(defun repository-top (directory)
  "The nearest directory from DIRECTORY up holding .git (MARKED-DIRECTORY, as
PROJECT-ROOT finds a project), as a native name ending with a slash, or NIL."
  (marked-directory directory '(".git")))

(defun run-git (directory arguments &key cancel line environment errors input seconds limit)
  "Run git with ARGUMENTS (a list of strings, never through a shell) in
DIRECTORY, with *GIT-SAFETY* and never asking for a password on a terminal,
on the thread calling it, which is never the editor's: what it printed, as octets, and true as a second value when it
succeeded. With LINE, a function, each line it prints is given to it as text
as it comes, in place of the octets, until LINE returns true, which counts as
success. CANCEL, a cons, stops it when its car turns true; on the thread of a
job, stopping the job ends git at once. The process is ended however it
stops. ENVIRONMENT, NAME=VALUE strings, is added to git's; with ERRORS what it
says on its error output (*GIT-SAID-LIMIT* octets of it at most) is read apart
from what it prints, as text, the fourth value; INPUT, a string, is given
on its standard input. With SECONDS, git is ended once it has run that long,
as if stopped. Git printing more octets than LIMIT (*PROGRAM-OUTPUT-LIMIT* when it is
NIL), or with LINE a line longer than that, is ended, NIL and NIL given,
and true as a third value. NIL, and no process at all, when
DIRECTORY is in no repository (REPOSITORY-TOP), in one within an evidence
folder (*EVIDENCE-FOLDERS*), where git would write in its .git, or when git
is not found (FIND-PROGRAM)."
  (let ((git (tuatara.io:find-program "git"))
        (top (ignore-errors (repository-top directory))))
    (unless (and git top (not (evidence-path-p top)))
      (return-from run-git (values nil nil)))
    (run-git-program git directory arguments :cancel cancel :line line :environment environment :errors errors :input input
                                             :seconds seconds :limit limit)))

(defun git-object-name (revision path)
  "How git names the file or folder PATH of REVISION (a commit's hash, HEAD):
REVISION:PATH, PATH from the top of the repository, or from the folder git
runs in when it begins with ./."
  (format nil "~a:~a" revision path))

(defun git-blob-octets (top hash path &key job limit cancel seconds)
  "What the file PATH (from TOP, or from the folder TOP when it begins with
./) held as the commit HASH (or HEAD) of the repository at TOP left it, as
octets, read by git cat-file; with PATH NIL, the text whose own id HASH is:
with JOB, on its thread, as GIT-JOB-OCTETS reads it, JOB ending there when
git failed; else on the thread calling it as RUN-GIT gives it, CANCEL and
SECONDS as it takes them, true as a second value when it was read and as a
third when it was more than LIMIT octets."
  (let ((arguments (list "cat-file" "blob" (if path (git-object-name hash path) hash))))
    (if job
        (git-job-octets job top arguments :limit limit)
        (run-git top arguments :limit limit :cancel cancel :seconds seconds))))

(defparameter *git-said-limit* (* 64 1024)
  "The most octets of what git says on its error output kept, to say why it
failed (RUN-GIT with ERRORS); the rest is read and let go.")

(defun run-git-program (git directory arguments &key cancel line environment errors input seconds limit)
  "Run the program GIT as RUN-GIT runs git, once it has found it. A command
that would write in the working files of a repository holding an evidence
folder is refused, never run (CHECK-GIT-WRITING); in a repository holding
one, git runs no hook of the repository's (core.hooksPath=/dev/null)."
  (let ((limit (or limit *program-output-limit*))
        (arguments (if (check-git-writing directory arguments environment)
                       (list* "-c" "core.hooksPath=/dev/null" arguments)
                       arguments)))
    (handler-case
        (tuatara.io:call-with-program
         (list* git "-C" (tuatara.io:native directory) (append (git-safety arguments) arguments))
         (lambda (process)
           (let ((stream (sb-ext:process-output process))
                 (listener (and errors
                                (tuatara.runtime:make-guarded-thread
                                 (lambda ()
                                   (ignore-errors
                                    (tuatara.io:read-stream-octets (sb-ext:process-error process) :limit *git-said-limit* :drain t)))
                                 :name "tuatara git messages")))
                 (octets nil) (stopped nil) (cancelled nil) (too-large nil)
                 (timer (and seconds (sb-ext:make-timer (lambda () (when (sb-ext:process-alive-p process)
                                                                      (ignore-errors (sb-ext:process-kill process 9))))
                                                        :thread t))))
             (when timer (sb-ext:schedule-timer timer seconds))
             (when input
               (let ((in (sb-ext:process-input process)))
                 (ignore-errors (write-string input in))
                 (ignore-errors (close in))))
             (unwind-protect
                  (if line
                      (loop (multiple-value-bind (text cut) (tuatara.io:read-line-bounded stream limit)
                              (cond ((null text) (return))
                                    (cut (setf too-large t) (return))
                                    ((and cancel (car cancel)) (setf cancelled t) (return))
                                    ((funcall line text) (setf stopped t) (return)))))
                      (setf (values octets too-large cancelled)
                            (tuatara.io:read-stream-octets stream :limit limit :cancel cancel)))
               (when timer (sb-ext:unschedule-timer timer)))
             ;; read to its end: git is done, and how it ended is known once it is waited for
             (unless (or stopped cancelled too-large) (sb-ext:process-wait process))
             (when (and listener (or stopped cancelled too-large) (sb-ext:process-alive-p process))
               (ignore-errors (sb-ext:process-kill process 15)))
             (let ((said (and listener
                              (let ((octets (sb-thread:join-thread listener :default nil)))
                                (if octets (tuatara.text:lenient-string octets) ""))))
                   (ok (and (not too-large) (not cancelled) (or stopped (eql 0 (sb-ext:process-exit-code process))))))
               (cond ((and too-large listener) (values nil nil t said))
                     (too-large (values nil nil t))
                     (listener (values (and (not line) (not cancelled) octets) ok nil said))
                     (t (values (and (not line) (not cancelled) octets) ok))))))
         :job tuatara.runtime:*job* :input (and input :stream) :output :stream :error (and errors :stream)
         :environment (append environment (list "GIT_TERMINAL_PROMPT=0") (sb-ext:posix-environ)))
      (error () (values nil nil)))))
