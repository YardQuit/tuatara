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
  (if (member (first arguments) *git-own-pathspecs* :test #'equal)
      (remove "--literal-pathspecs" *git-safety* :test #'string=)
      *git-safety*))

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
  "Run the program GIT as RUN-GIT runs git, once it has found it."
  (let ((limit (or limit *program-output-limit*)))
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
