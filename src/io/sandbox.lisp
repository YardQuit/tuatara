;;;; src/io/sandbox.lisp --- Running a program on an untrusted file, shut in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file from elsewhere is never parsed by the editor, the terminal or the
;;;; window toolkit: a program of the system decodes it (images to pixels, PDF
;;;; to text), shut in by bubblewrap (bwrap). It sees the system's programs and
;;;; libraries (/usr) and nothing else, read only: no home directory, no other
;;;; file, no network, no other process, no way into the terminal; it starts
;;;; with nothing in its environment and can make no new namespaces. Its
;;;; memory, the processor time it takes, the files it writes in its own
;;;; scratch /tmp and what it prints are limited, and it is ended, with all it
;;;; started, when its time is up or the one waiting stops it. A seccomp
;;;; filter refuses it the system calls no decoder needs and that reach into
;;;; the kernel's most exposed parts: mounting, namespaces, kernel modules and
;;;; keys, tracing other processes, io_uring, BPF, sockets other than local
;;;; ones, and pushing keys into a terminal.
;;;;
;;;; The file is not shown to it by name: it reads it on its standard input, a
;;;; copy of it in memory that nothing can change, made from a descriptor opened
;;;; here the way a safe visit opens a file, so its access time is not changed
;;;; (on your own files) and nothing else in its directory can be reached.
;;;; The file's own descriptor never reaches the program: through it, the file
;;;; could be written, its permissions and times changed.
;;;;
;;;; A program that needs more is shown more, and only that: LaTeX making a
;;;; PDF of a document sees the document's folder and TeX's own, read only,
;;;; and writes in its working directory alone (lisp/export/export.lisp).
;;;;
;;;; bwrap is found on the PATH when first needed and tried once; the result is
;;;; kept, and M-x check-health tries it again. Only SANDBOX-STATUS and
;;;; SANDBOX-RUN are used elsewhere (SANDBOX-RUN-ON-FILE by the tests alone),
;;;; so that another way of shutting a program in can take bwrap's place.

(in-package #:tuatara.io)

(defvar *sandbox-program* "bwrap"
  "The program that shuts in the programs decoding files for the editor, and
LaTeX making PDFs: bubblewrap, named bwrap, looked for on the PATH (or a full
path to it). NIL turns decoding off: images, PDFs and the like are then shown
as octets and text only; PDFs are still made of Org and LaTeX files, LaTeX
then not shut in, and each export says so.")

(defvar *sandbox-seconds* 10
  "The seconds a program decoding a file may run before it is ended, and the
processor time it may take.")

(defvar *sandbox-memory* (* 1024 1024 1024)
  "The octets of memory a program decoding a file may take (1 GB); NIL for no
limit.")

(defvar *sandbox-input-limit* (* 1024 1024 1024)
  "The largest file a program decoding files is given (1 GB): it gets a copy
in memory, which a larger file, or one claiming a huge size with nothing in
it, would fill. A larger one is not decoded, and why is said; NIL for no
limit.")

(defvar *sandbox-total-memory* (* 2 1024 1024 1024)
  "The octets of memory all the programs decoding one file may take together
(2 GB), where the system can hold them to it (systemd-run, a scope of the
user's own); NIL for no such limit, each program still held to
*SANDBOX-MEMORY*.")

(defparameter *sandbox-path* "/usr/local/bin:/usr/bin:/bin"
  "Where a program in the sandbox is looked for: the system's own directories
of programs, the only ones it sees.")

(defparameter *sandbox-said-limit* 4096
  "The octets of what a program in the sandbox writes to its error output that
are kept, to say why it failed.")

(defparameter *sandbox-file-limit* (* 64 1024 1024)
  "The octets a program in the sandbox may write to any one file of its
scratch /tmp.")

(defparameter *sandbox-process-limit* 64
  "The processes and threads a program in the sandbox may have at once, it
and all it starts counted together (only those in the sandbox).")

(defvar *sandbox-seccomp* t
  "True to have the kernel refuse a program decoding a file the system calls no
decoder needs (see the file's header), on x86-64 and AArch64; NIL to leave
them to it, the rest of the sandbox kept.")

(sb-ext:defglobal **sandbox-state** nil
  "What was found when the sandbox was last tried (SANDBOX-STATUS), as a list
(STATUS DETAIL PATH), or NIL before it is first needed.")

(sb-ext:defglobal **sandbox-scope** nil
  "systemd-run's path when it was found, when the sandbox was last tried, to
put a sandbox in a scope of its own whose memory is limited; NIL when not.")

(sb-ext:defglobal **sandbox-lock** (sb-thread:make-mutex :name "sandbox")
  "Held while the sandbox is tried, so that two threads needing it at once try it once.")

(defparameter *default-program-path* "/usr/bin:/bin"
  "Where programs are looked for when the editor was started with no PATH.")

(defun program-search-path ()
  "The directories programs are looked for in, as the PATH lists them:
the editor's own PATH, or *DEFAULT-PROGRAM-PATH* when it has none."
  (or (sb-posix:getenv "PATH") *default-program-path*))

(defun find-program (name &optional (search (program-search-path)))
  "The full path of the program NAME in the directories SEARCH lists (the
PATH), or NAME itself when it has a slash and can be run; NIL when there is no
such program. Only directories named from the root (starting with /) are
looked in: an empty entry, . or bin/ would mean wherever the editor happens
to be, which may be a folder of files not to be trusted."
  (flet ((runnable (path) (and (ignore-errors (sb-posix:access path sb-posix:x-ok) t)
                               (eq :regular (path-kind path))
                               path)))
    (if (find #\/ name)
        (runnable name)
        (loop for directory in (uiop:split-string search :separator ":")
              thereis (and (plusp (length directory)) (char= #\/ (char directory 0))
                           (runnable (path-join directory name)))))))

(defun program-missing (name &key install why)
  "Signal that the program NAME is not installed, saying how to install it
(INSTALL, \"Fedora: sudo dnf install gnupg2\") and what is not done without it
\(WHY), each when given: (or (find-program \"git\") (program-missing \"git\"))."
  (error "~a is not installed~@[ (~a)~]~@[: ~a~]" name install why))

(defun setsid-command (program arguments &rest setsid-flags)
  "How to start PROGRAM with ARGUMENTS in a session of its own (setsid), so
that it stays when the editor or its terminal goes, as two values: the full
path of the program to run and its arguments. That is setsid, given
SETSID-FLAGS (\"-w\" to wait for PROGRAM, \"-c\" to give it its terminal)
before PROGRAM and ARGUMENTS, when setsid is there; else PROGRAM itself.
Each is found as FIND-PROGRAM finds it, so the result is run as it is, never
looked for again; PROGRAM is left as it is when it is not found."
  (let ((setsid (find-program "setsid"))
        (program (or (find-program program) program)))
    (if setsid
        (values setsid (append setsid-flags (cons program arguments)))
        (values program arguments))))

(defparameter *program-output-limit* (* 64 1024 1024)
  "The most octets PROGRAM-OUTPUT takes of what a program writes (64 MB):
one that writes more, a clipboard of a huge copy or a program gone wrong, is
stopped and its output not taken, so that it cannot fill memory.")

(defparameter *program-end-grace* 2
  "The seconds a program no longer wanted has to end once asked to (SIGTERM),
before it is killed (CALL-WITH-PROGRAM).")

(defun call-with-program (command function &key job input output error directory (environment nil environment-p)
                                               preserve-fds (external-format '(:utf-8 :replacement #\?)))
  "Start COMMAND, a program's full name and its arguments (never through a
shell), and call FUNCTION with its process, returning what FUNCTION returns:
INPUT, OUTPUT and ERROR say what the program reads and writes, as
SB-EXT:RUN-PROGRAM takes them (:STREAM for a pipe FUNCTION uses, NIL for
none, :OUTPUT for its errors with what it writes); DIRECTORY is where it
runs, ENVIRONMENT, when given, all it sees of one; PRESERVE-FDS, descriptors
it is given besides (pipes of its own, named in its arguments). With JOB, it
is the program the job runs (TUATARA.RUNTIME:JOB-PROGRAM-STARTED): stopping
the job ends it. However FUNCTION ends, the program is ended when it still
runs, asked to (SIGTERM) and, not ended within *PROGRAM-END-GRACE* seconds,
killed with what it started (SIGKILL), waited for and closed, so that none is
left behind."
  (let ((process (apply #'sb-ext:run-program (first command) (rest command)
                        :wait nil :input input :output output :error error :external-format external-format
                        (append (and directory (list :directory directory))
                                (and environment-p (list :environment environment))
                                (and preserve-fds (list :preserve-fds preserve-fds))))))
    (when job (tuatara.runtime:job-program-started job process))
    (unwind-protect (funcall function process)
      (when (sb-ext:process-alive-p process)
        (ignore-errors (sb-ext:process-kill process sb-unix:sigterm))
        (loop repeat (ceiling *program-end-grace* 0.01)
              while (sb-ext:process-alive-p process)
              do (sleep 0.01))
        (when (process-unreaped-p process)
          (ignore-errors (sb-ext:process-kill process sb-unix:sigkill :process-group))))
      (ignore-errors (sb-ext:process-wait process))
      (ignore-errors (sb-ext:process-close process)))))

;;; Programs in a session of their own

(defun user-shell ()
  "The shell a command typed is run with (a compile's, a terminal's): $SHELL, else /bin/sh."
  (let ((shell (uiop:getenv "SHELL")))
    (if (plusp (length shell)) shell "/bin/sh")))

(defun session-command (command)
  "COMMAND, a program's full name and its arguments, run in a session of its
own with no terminal (setsid -w, which waits for it and gives its exit code):
a program in it that asks for a password (sudo, ssh, gpg) cannot reach the
terminal the editor runs in, and SIGNAL-SESSION reaches every program it
starts. COMMAND as it is when setsid is not there."
  (multiple-value-bind (program arguments) (setsid-command (first command) (rest command) "-w")
    (cons program arguments)))

(defun proc-text (path)
  "The file at PATH under /proc read whole, each octet a character (a
program's name there may be any octets); NIL when it cannot be read."
  (ignore-errors (uiop:read-file-string path :external-format :latin-1)))

(defun process-unreaped-p (process)
  "True while PROCESS, a program started, has not ended and been reaped: its
number is still its own, to be signalled."
  (member (sb-ext:process-status process) '(:running :stopped)))

(defun proc-numbers (path)
  "The numbers in the file at PATH under /proc, read whole as text; NIL when it cannot be read."
  (let ((text (proc-text path)))
    (and text (mapcar #'parse-integer (remove "" (uiop:split-string text :separator '(#\Space #\Newline)) :test #'string=)))))

(defun session-leader (process)
  "The process SESSION-COMMAND's setsid, PROCESS, started in a session of its
own: its first child, whose number is the session's; NIL when it has none,
or once PROCESS has ended, its number no longer its own."
  (when (process-unreaped-p process)
    (let ((pid (sb-ext:process-pid process)))
      (first (proc-numbers (format nil "/proc/~d/task/~d/children" pid pid))))))

(defun session-processes (session)
  "The numbers of the processes in the session numbered SESSION, from /proc."
  (loop for entry in (ignore-errors (directory "/proc/*/" :resolve-symlinks nil))
        for name = (car (last (pathname-directory entry)))
        for stat = (and (every #'digit-char-p name) (proc-text (format nil "/proc/~a/stat" name)))
        ;; pid (name) state ppid pgrp session ...: the fields after the name's closing parenthesis
        for fields = (and stat (uiop:split-string (subseq stat (+ 2 (or (position #\) stat :from-end t) -2))) :separator " "))
        when (and fields (> (length fields) 3) (eql session (ignore-errors (parse-integer (fourth fields)))))
          collect (parse-integer name)))

(defun signal-session (process session signal)
  "Send SIGNAL to PROCESS, started by SESSION-COMMAND, with its group, while
it has not ended (its number may be another's after), and to every process
of the session numbered SESSION (SESSION-LEADER), when known. True when one
of the session's processes was still there."
  (when (process-unreaped-p process)
    (ignore-errors (sb-ext:process-kill process signal :process-group)))
  (let ((there (and session (session-processes session))))
    (dolist (pid there)
      (ignore-errors (sb-posix:kill pid signal)))
    (and there t)))

(defun program-output (command &key input (capture t) (seconds 1) wait (environment nil environment-p) directory
                                    (most *program-output-limit*))
  "Run COMMAND, a program's full name and its arguments (never through a
shell), giving it INPUT (a string, through a pipe: never written to a file)
and, with CAPTURE, taking what it writes, as UTF-8 text (an octet that is
not UTF-8 as its raw byte's character), MOST octets of it at most (a program writing more is stopped, its output not taken); SECONDS at
most, then it is stopped. WAIT, when given, does the waiting instead of this
thread blocking: a function of the thread doing the work and SECONDS (the
editor's interruptible wait, say). ENVIRONMENT, when given, is all the
program sees of one; DIRECTORY, where it runs. Two values: its output (with
CAPTURE) and true when it ended well. A program that stays behind (wl-copy,
serving the clipboard) is not waited for: only the one started."
  (let* ((started (list nil))
         (worker (tuatara.runtime:make-guarded-thread
                  (lambda ()
                    (ignore-errors
                     (call-with-program
                      command
                      (lambda (process)
                        (setf (car started) process)
                        (when input
                          (ignore-errors (write-string input (sb-ext:process-input process)))
                          (ignore-errors (close (sb-ext:process-input process))))
                        (multiple-value-bind (octets too-much)
                            (and capture (ignore-errors (read-stream-octets (sb-ext:process-output process) :limit most)))
                          (unless too-much (sb-ext:process-wait process))
                          (list (and octets (tuatara.text:octets-string octets))
                                (and (not too-much) (eql 0 (sb-ext:process-exit-code process))))))
                      :input (and input :stream) :output (and capture :stream)
                      :directory directory
                      :environment (if environment-p environment (sb-ext:posix-environ)))))
                  :name "program")))
    (if wait
        (funcall wait worker seconds)
        (sb-thread:join-thread worker :timeout seconds :default nil))
    (if (sb-thread:thread-alive-p worker)
        (progn (let ((process (car started)))
                 (when process (ignore-errors (sb-ext:process-kill process 15))))
               (values nil nil))
        (values-list (sb-thread:join-thread worker :default nil)))))

(defparameter *program-said-limit* (* 64 1024)
  "The most octets kept of what a program says on its error output
\(FILTER-THROUGH-PROGRAM); the rest is read and let go.")

(defun filter-through-program (command write &key job directory (environment nil environment-p)
                                                 (limit *program-output-limit*))
  "Run COMMAND, a program's full name and its arguments (never through a
shell), calling WRITE with the descriptor of its standard input to give it
what it reads, a piece at a time (WRITE-OCTETS; a buffer's text from
BUFFER-TEXT, say), its input closed once WRITE returns; what it prints and
what it says on its error output are read meanwhile on threads of their own,
so that none waits on another. A program that ends before it has read all
makes WRITE's next write fail (FILE-SYSTEM-ERROR), which WRITE may take as
the end. With JOB, it is the program the job runs: stopping the job ends it.
DIRECTORY is where it runs, ENVIRONMENT, when given, all it sees of one.
Four values: what it printed, as octets (NIL when it printed more than
LIMIT octets: it is ended then; a LIMIT of NIL takes all it prints); what
it said, as text, its first *PROGRAM-SAID-LIMIT* octets; its exit code, NIL
when it did not exit of itself; and true when it printed more than LIMIT."
  (apply #'call-with-program command
         (lambda (process)
           (flet ((reader (stream most drain name)
                    (tuatara.runtime:make-guarded-thread
                     (lambda ()
                       (multiple-value-bind (octets too-much) (ignore-errors (read-stream-octets stream :limit most :drain drain))
                         ;; ended, so that it waits on no one reading what it prints
                         (when (and too-much (not drain)) (ignore-errors (sb-ext:process-kill process 15)))
                         (list octets too-much)))
                     :name name)))
             (let ((output (reader (sb-ext:process-output process) limit nil "program output"))
                   (said (reader (sb-ext:process-error process) *program-said-limit* t "program messages")))
               (unwind-protect (funcall write (sb-sys:fd-stream-fd (sb-ext:process-input process)))
                 (ignore-errors (close (sb-ext:process-input process))))
               (destructuring-bind (&optional octets too-much) (sb-thread:join-thread output :default nil)
                 (let ((spoken (first (sb-thread:join-thread said :default nil))))
                   (sb-ext:process-wait process)
                   (values (and (not too-much) octets)
                           (if spoken (tuatara.text:lenient-string spoken) "")
                           (and (eq (sb-ext:process-status process) :exited) (sb-ext:process-exit-code process))
                           too-much))))))
         :job job :input :stream :output :stream :error :stream :directory directory
         (and environment-p (list :environment environment))))

(defun command-words (text &key (backslash :shell))
  "TEXT split into a program's arguments as a shell splits a command line,
though no shell runs: at spaces, tabs and line ends, but within '...', which
keeps all it holds as it is, or \"...\", either quote holding the other.
BACKSLASH says what a backslash does: :SHELL, outside quotes it keeps the
character after it as it is, within \"...\" the \" or \\ after it, and is
itself before any other; :ESCAPES, outside quotes it is itself (a Windows
path comes through whole), within \"...\" it begins one of \\t \\n \\r \\\\ and
\\\", a tab, a line end, a carriage return, a backslash and a quote, and any
other is refused. Two values, the words and T; or NIL, NIL and why as a third
when a quote is left open or an escape is refused."
  (let ((words '()) (word (make-string-output-stream)) (in-word nil) (quote-char nil) (i 0) (n (length text)))
    (flet ((end-word () (when in-word (push (get-output-stream-string word) words) (setf in-word nil)))
           (next () (and (< (1+ i) n) (char text (1+ i)))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((eql quote-char #\')
                        (if (char= c #\') (setf quote-char nil) (write-char c word)))
                       ((eql quote-char #\")
                        (cond ((char= c #\") (setf quote-char nil))
                              ((char/= c #\\) (write-char c word))
                              ((eq backslash :escapes)
                               (let ((escaped (case (next) (#\t #\Tab) (#\n #\Newline) (#\r #\Return) (#\\ #\\) (#\" #\"))))
                                 (unless escaped
                                   (return-from command-words
                                     (values nil nil (format nil "within double quotes a backslash begins an escape, and only \\t \\n \\r \\\\ \\\" are taken: ~a; single quotes take a backslash as it is"
                                                             (subseq text i (min n (+ i 2)))))))
                                 (incf i) (write-char escaped word)))
                              ((member (next) '(#\" #\\)) (incf i) (write-char (char text i) word))
                              (t (write-char c word))))
                       ((member c '(#\Space #\Tab #\Newline #\Return #\Page #.(code-char 11))) (end-word))
                       ((member c '(#\' #\")) (setf quote-char c in-word t))
                       ((and (char= c #\\) (eq backslash :shell) (next)) (incf i) (write-char (char text i) word) (setf in-word t))
                       (t (write-char c word) (setf in-word t))))
               (incf i))
      (if quote-char
          (values nil nil "a quote is left open")
          (progn (end-word) (values (nreverse words) t))))))

(defun system-directory-arguments ()
  "bwrap's arguments showing the program the system's directories, read only:
/usr, the directories beside it that are links into it on most systems (made
links in the sandbox too) or directories of their own on others, and what
programs read to find libraries and alternatives."
  (append (list "--ro-bind" "/usr" "/usr")
          (loop for directory in '("/bin" "/sbin" "/lib" "/lib64" "/lib32")
                for target = (ignore-errors (sb-posix:readlink directory))
                append (cond (target (list "--symlink" target directory))
                             ((eq :directory (path-kind directory))
                              (list "--ro-bind" directory directory))))
          (loop for path in '("/etc/ld.so.cache" "/etc/alternatives" "/etc/fonts")
                append (list "--ro-bind-try" path path))))

(defvar *bwrap-versions* (make-hash-table :test 'equal :synchronized t)
  "The version of each bwrap program asked so far, by its path: a list of numbers, or :UNKNOWN.")

(defun bwrap-version-at-least-p (major minor)
  "True when the bwrap found for *SANDBOX-PROGRAM* is version MAJOR.MINOR or later, as it says it is."
  (let ((program (and *sandbox-program* (find-program *sandbox-program*))))
    (when program
      (let ((version (or (gethash program *bwrap-versions*)
                         (setf (gethash program *bwrap-versions*)
                               (or (let ((numbers (cl-ppcre:all-matches-as-strings "\\d+" (version-text program))))
                                     (and numbers (mapcar #'parse-integer numbers)))
                                   :unknown)))))
        (and (consp version)
             (or (> (first version) major)
                 (and (= (first version) major) (>= (or (second version) 0) minor))))))))

(defparameter *seccomp-refused*
  '((:x86-64 #xC000003E
     ;; acct add_key keyctl request_key bpf chroot pivot_root mount umount2 unshare setns
     163 248 250 249 321 161 155 165 166 272 308
     ;; init_module finit_module delete_module kexec_load kexec_file_load reboot swapon swapoff syslog
     175 313 176 246 320 169 167 168 103
     ;; quotactl quotactl_fd perf_event_open ptrace process_vm_readv process_vm_writev userfaultfd
     179 443 298 101 310 311 323
     ;; uselib lookup_dcookie open_by_handle_at name_to_handle_at fanotify_init
     134 212 304 303 300
     ;; open_tree move_mount fsopen fsconfig fsmount fspick mount_setattr
     428 429 430 431 432 433 442
     ;; io_uring_setup io_uring_enter io_uring_register vhangup clock_adjtime clock_settime
     425 426 427 153 305 227
     ;; settimeofday adjtimex iopl ioperm nfsservctl pidfd_getfd
     164 159 172 173 180 438)
    (:arm64 #xC00000B7
     89 217 219 218 280 51 41 40 39 97 268
     105 273 106 104 294 142 224 225 116
     60 443 241 117 270 271 282
     18 265 264 262
     428 429 430 431 432 433 442
     425 426 427 58 266 112
     170 171 42 438))
  "For each processor the seccomp filter knows (a feature of the Lisp, then
the kernel's AUDIT_ARCH number), the system calls refused, by number.")

(defparameter *seccomp-calls*
  '((:x86-64 :clone 56 :clone3 435 :ioctl 16 :socket 41)
    (:arm64 :clone 220 :clone3 435 :ioctl 29 :socket 198))
  "For each processor, the numbers of the system calls the seccomp filter
looks into the arguments of.")

(defun seccomp-program ()
  "The seccomp filter of the sandbox as the octets of a classic BPF program,
as bwrap's --seccomp reads it, for the processor here; NIL on one it does not
know. Another processor's calls (32-bit ones on x86-64) end the program; the
calls of *SECCOMP-REFUSED* fail with EPERM, clone3 with ENOSYS (so that its
flags, out of reach of the filter, are asked through clone), clone making a
namespace, ioctl pushing input into a terminal (TIOCSTI, TIOCLINUX) and a
socket that is not a local one (AF_UNIX) fail too; everything else is let be."
  (let* ((cpu (find-if (lambda (entry) (member (first entry) *features*)) *seccomp-refused*))
         (calls (and cpu (rest (assoc (first cpu) *seccomp-calls*))))
         (code '()))
    (when cpu
      (flet ((op (opcode jt jf k) (push (list opcode jt jf k) code))
             (ret (k) (push (list #x06 0 0 k) code)))
        (let ((allow #x7FFF0000) (kill #x80000000)
              (eperm (logior #x00050000 1)) (enosys (logior #x00050000 38)) (eafnosupport (logior #x00050000 97)))
          (op #x20 0 0 4)                                   ; the processor
          (op #x15 1 0 (second cpu))
          (ret kill)
          (op #x20 0 0 0)                                   ; the call
          (when (eq (first cpu) :x86-64)                    ; x32 calls
            (op #x35 0 1 #x40000000)
            (ret eperm))
          (dolist (number (cddr cpu))
            (op #x15 0 1 number)
            (ret eperm))
          (op #x15 0 1 (getf calls :clone3))
          (ret enosys)
          ;; clone with CLONE_NEWNS, NEWCGROUP, NEWUTS, NEWIPC, NEWUSER, NEWPID, NEWNET or NEWTIME
          (op #x15 0 4 (getf calls :clone))
          (op #x20 0 0 16)
          (op #x45 0 1 #x7E020080)
          (ret eperm)
          (op #x20 0 0 0)
          ;; ioctl TIOCSTI and TIOCLINUX
          (op #x15 0 5 (getf calls :ioctl))
          (op #x20 0 0 24)
          (op #x15 1 0 #x5412)
          (op #x15 0 1 #x541C)
          (ret eperm)
          (op #x20 0 0 0)
          ;; socket of another family than AF_UNIX
          (op #x15 0 4 (getf calls :socket))
          (op #x20 0 0 16)
          (op #x15 1 0 1)
          (ret eafnosupport)
          (op #x20 0 0 0)
          (ret allow)))
      (let ((octets (make-array (* 8 (length code)) :element-type '(unsigned-byte 8))))
        (loop for (opcode jt jf k) in (reverse code)
              for at from 0 by 8
              do (octets-put-integer octets opcode 2 :at at)
                 (setf (aref octets (+ at 2)) jt (aref octets (+ at 3)) jf)
                 (octets-put-integer octets k 4 :at (+ at 4)))
        octets))))

(defun sandbox-filtered-p ()
  "True when programs in the sandbox run under its seccomp filter: turned on
(*SANDBOX-SECCOMP*) and a processor it knows."
  (and *sandbox-seccomp* (find-if (lambda (entry) (member (first entry) *features*)) *seccomp-refused*) t))

(defun sealed-memory-file (name fill &key kept)
  "A descriptor of a new file in memory called NAME, FILL (a function of its
descriptor) having written it, at its start, sealed so that nothing, not even
a program given it, can change it: not written, not made shorter or longer;
with KEPT kept open in programs started (for bwrap to read). Closed, and an
error signalled, when it cannot be made, filled or sealed."
  (let ((fd (sb-alien:alien-funcall
             (sb-alien:extern-alien "memfd_create" (function sb-alien:int sb-alien:c-string sb-alien:unsigned-int))
             name (if kept 2 3))))                          ; MFD_ALLOW_SEALING, and MFD_CLOEXEC when not kept
    (when (minusp fd) (fail "make a file in memory called" name))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (sb-posix:close fd)))))
      (funcall fill fd)
      (when (minusp (fcntl-int fd 1033 15))                 ; F_ADD_SEALS: SEAL, SHRINK, GROW, WRITE
        (fail "seal" "a file in memory"))
      (sb-posix:lseek fd 0 sb-posix:seek-set)
      fd)))

(defun memory-file (name octets &key (kept t))
  "A descriptor of a new file in memory holding OCTETS, at its start, sealed
so that nothing can change it (SEALED-MEMORY-FILE), and with KEPT kept open
in programs started (for bwrap to read)."
  (sealed-memory-file name (lambda (fd) (write-octets fd octets :path name)) :kept kept))

(defparameter *sandbox-own-directories* '("/usr" "/bin" "/sbin" "/lib" "/lib64" "/lib32" "/tmp" "/proc" "/dev" "/etc")
  "The directories at the root that the sandbox makes itself, never made links
because they are links outside it.")

(defun root-link (path)
  "The link at the root PATH begins with (/home leading to /var/home) and what
it leads to, as two values, or NIL when its top is no link or a directory the
sandbox makes itself (*SANDBOX-OWN-DIRECTORIES*)."
  (let* ((top (subseq path 0 (or (position #\/ path :start 1) (length path))))
         (target (and (> (length top) 1) (not (member top *sandbox-own-directories* :test #'string=))
                      (ignore-errors (sb-posix:readlink top)))))
    (and target (values top target))))

(defun root-link-arguments (paths)
  "bwrap's arguments making in the sandbox the links at the root that PATHS
go through here, each as it is here (/home leading to /var/home, where the
system has it so), so that a path written either way finds what is shown at
its real place."
  (let ((made '()))
    (loop for path in paths
          append (multiple-value-bind (top target) (root-link path)
                   (when (and target (not (member top made :test #'string=)))
                     (push top made)
                     (list "--symlink" target top))))))

(defun root-resolved (path)
  "PATH with a link at the root it begins with (/home leading to /var/home)
followed, as the sandbox makes it (ROOT-LINK-ARGUMENTS), the rest as written;
no / at its end."
  (let ((path (string-right-trim "/" path)))
    (multiple-value-bind (top target) (root-link path)
      (if target
          (concatenate 'string (if (char= (char target 0) #\/) "" "/") (string-right-trim "/" target) (subseq path (length top)))
          path))))

(defun real-place (path)
  "PATH with every link in it followed as far as it exists, the rest as it is
\(REAL-PATH-SO-FAR); PATH as it is under a directory the sandbox makes itself
\(*SANDBOX-OWN-DIRECTORIES*: /tmp is its own there). No / at its end."
  (if (some (lambda (own) (or (string= path own) (uiop:string-prefix-p (concatenate 'string own "/") path)))
            *sandbox-own-directories*)
      (string-right-trim "/" path)
      (real-path-so-far path)))

(defun sandbox-place (entry)
  "Where ENTRY of a sandbox's directories goes, as two values, the directory
shown and where the program sees it: (SOURCE . TARGET); a path alone, shown
at its own place. A place outside the directories the sandbox makes itself
is its real one (REAL-PLACE), so that it is found however it is written."
  (if (consp entry)
      (values (car entry) (real-place (cdr entry)))
      (let ((real (real-place entry)))
        (values real real))))

(defun mount-arguments (directories writable overlays)
  "bwrap's arguments showing DIRECTORIES read only, WRITABLE ones to write in
and OVERLAYS, as SANDBOX-RUN has them, those that exist, a directory in
another shown after it, so that both are seen; with the links at the root
their paths go through (ROOT-LINK-ARGUMENTS). A directory a link leads to is
shown where it is written too, so that a path through the link finds it. The
root itself is never shown: the sandbox's root is its own. A directory to
write in that is, or holds, a place nothing may be written in is refused
\(CHECK-TREE-WRITING)."
  (let ((mounts '()) (written '()))
    (flet ((add (entry how)
             (multiple-value-bind (source target) (sandbox-place entry)
               (when (eq how :write)
                 (check-tree-writing source "let a program write in"))
               (push (if (consp entry) (cdr entry) entry) written)
               ;; at its real place, and where it is written when a link leads there (~/scratch to /tmp/scratch)
               (dolist (place (remove-duplicates (list target (if (consp entry) target (root-resolved entry))) :test #'string=))
                 (push (cons place (ecase how
                                     (:read (list "--ro-bind-try" source place))
                                     (:write (list "--bind-try" source place))))
                       mounts)))))
      (dolist (entry directories) (add entry :read))
      (dolist (entry writable) (add entry :write))
      (loop for (source upper work) in overlays
            do (multiple-value-bind (lower target) (sandbox-place source)
                 (push source written)
                 (push (cons target (if (bwrap-version-at-least-p 0 11)
                                        (list "--overlay-src" lower "--overlay" upper work target)
                                        (list "--ro-bind-try" lower target)))
                       mounts))))
    ;; the last given for a place wins; a parent before what is in it; the root itself never
    (setf mounts (stable-sort (remove-duplicates (remove "" (nreverse mounts) :key #'car :test #'string=)
                                                 :key #'car :test #'string= :from-end nil)
                              #'< :key (lambda (mount) (length (car mount)))))
    (append (root-link-arguments (reverse written))
            (loop for mount in mounts append (cdr mount)))))

(defun sandbox-arguments (program arguments seconds memory &key seccomp files environment directories writable overlays
                                                              (directory "/tmp") (file-limit *sandbox-file-limit*)
                                                              (processes *sandbox-process-limit*) (scratch t)
                                                              (descriptors nil))
  "bwrap's arguments for running PROGRAM with ARGUMENTS shut in, limited to
SECONDS of processor time (NIL: no limit) and MEMORY octets (NIL: no limit)
each process, its scratch /tmp to MEMORY octets too, to PROCESSES processes
and threads, and to files of FILE-LIMIT octets (NIL: no limit); SECCOMP the descriptor of its
seccomp filter, FILES the descriptors of files it is shown read only, each
\(PATH . FD), ENVIRONMENT what it has in its environment besides, each (NAME
VALUE), a later one for the same NAME in place of the earlier, DIRECTORIES,
WRITABLE and OVERLAYS the directories it is shown as SANDBOX-RUN says, and
DIRECTORY the one it starts in. Without SCRATCH it has no /tmp at all;
DESCRIPTORS, when given, is the most files it may have open at once."
  (append (list "--unshare-all" "--unshare-user" "--disable-userns" "--die-with-parent" "--new-session"
                "--cap-drop" "ALL" "--clearenv"
                "--setenv" "PATH" *sandbox-path* "--setenv" "HOME" "/tmp" "--setenv" "LC_ALL" "C.UTF-8")
          (loop for (name value) in environment append (list "--setenv" name value))
          (system-directory-arguments)
          (list "--proc" "/proc" "--dev" "/dev")
          ;; the scratch /tmp is memory too: held to the memory a program may take, where bwrap can (0.10 on)
          (and scratch memory (bwrap-version-at-least-p 0 10) (list "--size" (princ-to-string memory)))
          (and scratch (list "--tmpfs" "/tmp"))
          (mount-arguments directories writable overlays)
          (loop for (path . fd) in files append (list "--ro-bind-data" (princ-to-string fd) path))
          (and seccomp (list "--seccomp" (princ-to-string seccomp)))
          (list "--chdir" directory
                "--" "/bin/sh" "-c"
                "ulimit -c 0 && ulimit -v \"$1\" && ulimit -t \"$2\" && ulimit -f \"$3\" && ulimit -u \"$4\" && { [ \"$5\" = as-is ] || ulimit -n \"$5\"; } && shift 5 && exec \"$@\""
                "sh"
                (if memory (princ-to-string (ceiling memory 1024)) "unlimited")
                (if seconds (princ-to-string (max 1 (ceiling seconds))) "unlimited")
                (if file-limit (princ-to-string (ceiling file-limit 512)) "unlimited")
                (princ-to-string processes)
                (if descriptors (princ-to-string descriptors) "as-is")
                program)
          arguments))

(defun said-text (octets)
  "OCTETS a program wrote to say something, as text, trimmed, octets that are
not UTF-8 shown as ?."
  (string-trim '(#\Space #\Newline #\Return #\Tab)
               (tuatara.text:lenient-string octets)))

(defun scope-environment ()
  "What systemd-run needs of the environment to reach your own service
manager, and nothing else."
  (loop for name in '("XDG_RUNTIME_DIR" "DBUS_SESSION_BUS_ADDRESS")
        for value = (sb-posix:getenv name)
        when value collect (format nil "~a=~a" name value)))

(defun scope-arguments (total)
  "systemd-run's arguments putting what follows them in a scope of its own,
its processes together held to TOTAL octets of memory, none of it swapped."
  (list "--user" "--scope" "--quiet" "--collect"
        "-p" (format nil "MemoryMax=~d" total) "-p" "MemorySwapMax=0" "--"))

(defun version-text (program)
  "What PROGRAM says of its version (--version), its environment empty; an
empty string when it says nothing within five seconds."
  (or (program-output (list program "--version") :seconds 5 :environment '()) ""))

(defun try-scope ()
  "systemd-run's path when it can put a program in a scope of your own here, else NIL."
  (let ((path (find-program "systemd-run")))
    (and path
         (nth-value 1 (program-output (append (list path) (scope-arguments (* 64 1024 1024)) (list "/bin/true"))
                                      :capture nil :seconds 10 :environment (scope-environment)))
         path)))

(defun start-sandbox (bwrap program arguments stdin seconds memory
                      &rest keys &key files (total *sandbox-total-memory*) (outputs :stream) &allow-other-keys)
  "Start bwrap at the path BWRAP on PROGRAM with ARGUMENTS, STDIN its input,
shown FILES, each (PATH . TEXT), as SANDBOX-RUN has them, KEYS the rest of
SANDBOX-ARGUMENTS's; the process. In a scope whose memory is held to TOTAL
\(*SANDBOX-TOTAL-MEMORY* unless given) when systemd-run can make one. Its
outputs are streams to read, or let go when OUTPUTS is NIL."
  (let ((seccomp nil) (data '()))
    (unwind-protect
         (let* ((program-octets (and *sandbox-seccomp* (seccomp-program)))
                (arguments (progn
                             (when program-octets (setf seccomp (memory-file "tuatara-seccomp" program-octets)))
                             (loop for (path . text) in files
                                   do (push (cons path (memory-file "tuatara-sandbox-file"
                                                                    (tuatara.text:string-octets text)))
                                            data))
                             (apply #'sandbox-arguments program arguments seconds memory
                                    :seccomp seccomp :files (reverse data)
                                    (alexandria:remove-from-plist keys :files :total :outputs))))
                (keep (remove nil (cons seccomp (mapcar #'cdr data))))
                (scope (and total **sandbox-scope**)))
           (if scope
               ;; systemd-run moves itself into the scope and becomes bwrap: still this one's child
               (sb-ext:run-program scope (append (scope-arguments total) (list bwrap) arguments)
                                   :search nil :wait nil :input stdin :preserve-fds keep
                                   :output outputs :error outputs :environment (scope-environment))
               (sb-ext:run-program bwrap arguments
                                   :search nil :wait nil :input stdin :preserve-fds keep
                                   :output outputs :error outputs :environment '())))
      ;; bwrap has its own descriptors of them once started
      (when seccomp (ignore-errors (sb-posix:close seccomp)))
      (dolist (file data) (ignore-errors (sb-posix:close (cdr file)))))))

(defun sandbox-start (program arguments stdin &key memory directories (processes *sandbox-process-limit*) descriptors)
  "Start PROGRAM (a full path to one it is shown) with ARGUMENTS shut in as
SANDBOX-RUN shuts in a program, left running, with no limit on the processor
time it takes, no /tmp, no file it may write into (writing one ends it) and
its outputs let go; STDIN, a stream on a descriptor, its standard input. It may take MEMORY octets (NIL: no limit),
held to them together with all it starts where systemd-run can, have
PROCESSES processes and threads and DESCRIPTORS files open (NIL: as it is);
DIRECTORIES are directories it is shown, read only, as SANDBOX-RUN has them.
The process, or NIL when there is no sandbox here (SANDBOX-STATUS). Ending
it ends all it started."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (declare (ignore detail))
    (when (eq status :ready)
      (start-sandbox bwrap program arguments stdin nil memory
                     :total memory :outputs nil :directories directories :processes processes
                     :descriptors descriptors :scratch nil :directory "/" :file-limit 0))))

(defun sandbox-memory-scope-p ()
  "True when what is started shut in is held, with all it starts, to its
memory in a scope of its own (systemd-run); without one, each process is
held only to its own address space."
  (and (eq :ready (sandbox-status)) **sandbox-scope** t))

(defun %sandbox-run (bwrap program arguments input seconds memory output-limit cancel ended-well &rest keys)
  "Run PROGRAM with ARGUMENTS in the sandbox bwrap at the path BWRAP makes,
KEYS those of SANDBOX-ARGUMENTS but SECCOMP, FILES each (PATH . TEXT); it
ends well with an exit code among ENDED-WELL. See SANDBOX-RUN."
  (let* ((stdin (and input (sb-sys:make-fd-stream input :input t :element-type '(unsigned-byte 8)
                                                         :buffering :none :auto-close nil)))
         (process (apply #'start-sandbox bwrap program arguments stdin seconds memory keys))
         (out (sb-sys:fd-stream-fd (sb-ext:process-output process)))
         (err (sb-sys:fd-stream-fd (sb-ext:process-error process)))
         (open (list out err))
         (piece (make-array 65536 :element-type '(unsigned-byte 8)))
         ;; what it printed and said
         (output (make-octet-pieces)) (said (make-octet-pieces))
         (deadline (tuatara.runtime:deadline-in seconds))
         (outcome nil))
    (flet ((left () (tuatara.runtime:seconds-left deadline))
           (take (into octets n limit)
             (let ((n (min n (- limit (octet-pieces-size into)))))
               (when (plusp n)
                 (add-octet-piece into (subseq octets 0 n))))))
      (unwind-protect
           (progn
             ;; what it prints, until both its outputs end
             (loop while (and open (null outcome))
                   do (cond ((and cancel (car cancel)) (setf outcome :cancelled))
                            ((<= (left) 0) (setf outcome :timeout))
                            (t (let ((ready (wait-for-fds open (min 0.1 (left)))))
                                 (when (listp ready)
                                   (dolist (fd ready)
                                     (let ((n (read-fd-into fd piece :path "a program's output")))
                                       (cond ((zerop n) (setf open (remove fd open)))
                                             ((and (= fd out) (null output-limit)))   ; read and let go
                                             ((= fd out)
                                              (if (> (+ (octet-pieces-size output) n) output-limit)
                                                  (setf outcome :too-much)
                                                  (take output piece n output-limit)))
                                             (t (take said piece n *sandbox-said-limit*))))))))))
             ;; its end: it may have shut its outputs and gone on
             (loop while (and (null outcome) (sb-ext:process-alive-p process))
                   do (cond ((and cancel (car cancel)) (setf outcome :cancelled))
                            ((<= (left) 0) (setf outcome :timeout))
                            (t (sleep 0.005))))
             (unless outcome
               (setf outcome (if (and (eq (sb-ext:process-status process) :exited)
                                      (member (sb-ext:process-exit-code process) ended-well))
                                 :done
                                 :failed))))
        ;; ending bwrap ends everything in the sandbox with it
        (when (sb-ext:process-alive-p process)
          (ignore-errors (sb-ext:process-kill process sb-unix:sigkill))
          (ignore-errors (sb-ext:process-wait process)))
        (ignore-errors (sb-ext:process-close process)))
      (values (joined-octets output) outcome (said-text (joined-octets said))))))

(defun try-sandbox ()
  "Find bwrap and try it, as a list (STATUS DETAIL PATH): see SANDBOX-STATUS."
  (let ((path (and *sandbox-program* (find-program *sandbox-program*))))
    (cond ((null *sandbox-program*) (list :off "turned off (*sandbox-program* is nil)" nil))
          ((null path) (list :missing (format nil "~a not found on the PATH" *sandbox-program*) nil))
          (t (let ((version (said-text (sb-ext:string-to-octets (version-text path)))))
               (multiple-value-bind (output outcome said)
                   (handler-case (%sandbox-run path "true" '() nil 10 (* 64 1024 1024) 0 nil '(0))
                     (error (e) (values nil :failed (princ-to-string e))))
                 (declare (ignore output))
                 (setf **sandbox-scope** (try-scope))
                 (if (eq outcome :done)
                     (list :ready (format nil "~a (~a)~@[, together held to ~a of memory (systemd-run)~]"
                                          (if (plusp (length version)) version *sandbox-program*) path
                                          (and **sandbox-scope** *sandbox-total-memory*
                                               (format nil "~,1f GB" (/ *sandbox-total-memory* (expt 1024 3)))))
                           path)
                     (list :broken (format nil "~a (~a) cannot run: ~a" (if (plusp (length version)) version *sandbox-program*)
                                           path (if (plusp (length said)) said (string-downcase outcome)))
                           path))))))))

(defun sandbox-find-program (name)
  "The full path of the program NAME where a program in the sandbox finds it
(the system's own directories of programs), or NIL when it is not there: a
program elsewhere, as in the home directory, cannot be run shut in."
  (and (not (find #\/ name)) (find-program name *sandbox-path*)))

(defun sandbox-status (&key again)
  "Whether programs decoding files can be shut in here, as three values: a
keyword, :READY, :MISSING (bwrap is not on the PATH), :BROKEN (it is there but
cannot run, as where the system allows no user namespaces) or :OFF
(*SANDBOX-PROGRAM* is NIL); a sentence saying which program, its version and
where, or what is wrong; and bwrap's path. Tried the first time it is needed
and kept; AGAIN tries it again. Takes a few milliseconds the first time: call
it from a thread doing work in the background."
  (let ((state (sb-thread:with-mutex (**sandbox-lock**)
                 (when (or again (null **sandbox-state**)
                           (not (equal *sandbox-program* (fourth **sandbox-state**))))
                   (setf **sandbox-state** (append (try-sandbox) (list *sandbox-program*))))
                 **sandbox-state**)))
    (values (first state) (second state) (third state))))

(defun sealed-copy (fd)
  "A copy in memory of the open file FD, as a new descriptor at its start,
sealed so that nothing, not even a program given it, can change it: not
written, not made shorter or longer. FD's own offset is left alone. A file
larger than *SANDBOX-INPUT-LIMIT* is refused with a FILE-SYSTEM-ERROR."
  (let ((size (stamp-size (fd-stamp fd "a file"))))
    (when (and *sandbox-input-limit* (> size *sandbox-input-limit*))
      (error 'file-system-error :operation "decode" :path "a file"
                                :reason (format nil "it is ~:d octets, more than *sandbox-input-limit* (~:d)" size *sandbox-input-limit*)))
    (sealed-memory-file "tuatara-decode" (lambda (copy) (copy-fd-contents fd copy size)))))

(defun sealed-p (fd)
  "True when FD is a copy in memory sealed as SEALED-COPY seals it."
  (let ((seals (fcntl-int fd 1034 0)))                      ; F_GET_SEALS
    (and (>= seals 0) (= 15 (logand seals 15)))))

(defun reopen-sealed (fd)
  "A descriptor of its own on the sealed copy FD, at its start, read only:
programs reading it at once each read from their own place."
  (handler-case (sb-posix:open (format nil "/proc/self/fd/~d" fd) sb-posix:o-rdonly)
    (sb-posix:syscall-error (e) (fail "open again" "a copy in memory" (sb-posix:syscall-errno e)))))

(defun sandbox-status-now ()
  "SANDBOX-STATUS's first value without waiting, for the editor's own thread:
what was found when the sandbox was last tried; :OFF or :MISSING, told at
once; or :UNKNOWN while bwrap is there but not tried yet (or tried again), the
trying then started in the background."
  (let ((state **sandbox-state**))
    (cond ((and state (equal *sandbox-program* (fourth state))) (first state))
          ((null *sandbox-program*) :off)
          ((null (find-program *sandbox-program*)) :missing)
          (t (unless (sb-thread:mutex-owner **sandbox-lock**)
               (tuatara.runtime:make-guarded-thread (lambda () (ignore-errors (sandbox-status))) :name "sandbox trial"))
             :unknown))))

(defun sandbox-run (program arguments &key input (seconds *sandbox-seconds*) (memory *sandbox-memory*)
                                           (output-limit (* 256 1024 1024)) cancel files environment directories
                                           writable overlays (directory "/tmp") (file-limit *sandbox-file-limit*)
                                           (ended-well '(0)))
  "Run PROGRAM (a name looked for on the sandbox's PATH, /usr/bin:/bin, or a
full path to one it is shown) with ARGUMENTS (strings, never through a shell)
shut in: it sees the system's programs and libraries, read only, and nothing
else (no home directory, no network, no other process), its time, memory and
the files it writes in its own /tmp limited; reading INPUT, an open descriptor, on its
standard input from its start (nothing when NIL): a sealed copy of it
\(SEALED-COPY), never INPUT itself; an INPUT that is such a copy already is
read as it is, through a descriptor of its own (one copy can serve many runs,
at once too).
Waits for it: call it from a thread doing work in the background. It is ended
after SECONDS, when it prints more than OUTPUT-LIMIT octets (NIL: what it
prints is let go, never kept), or when CANCEL, a cons, has a true car; it may
take MEMORY octets (NIL: no limit), and write files of FILE-LIMIT octets at
most (NIL: no limit). Returns three values: the octets it printed; how it
went, :DONE (it ended well: with an exit code among ENDED-WELL, 0 alone
unless given), :FAILED (it ended with an error, or ran out of memory),
:TIMEOUT, :TOO-MUCH, :CANCELLED, or :UNAVAILABLE when there is no sandbox
here (SANDBOX-STATUS); and what it said on its error output, or why there is
no sandbox.
FILES are files it is shown, read only, each (PATH . TEXT); ENVIRONMENT what
it has in its environment besides PATH, HOME (/tmp) and LC_ALL, or in place
of them, each (NAME VALUE). DIRECTORIES are directories of yours it is shown,
read only, each (SOURCE . TARGET), TARGET where it sees SOURCE (under /tmp,
its home, for LibreOffice's fonts), or a path alone, seen where it is (its
real place, links in it followed, a link at the root as /home kept as it is
here); WRITABLE the same, for directories it may write in; OVERLAYS
directories it sees where they are and may seem to write in, each (SOURCE
UPPER WORK): what it writes goes to UPPER, a directory of the editor's own,
SOURCE never changed, WORK an empty directory beside UPPER that the system
needs for it (where bwrap cannot do that, before 0.11, SOURCE is shown read
only). Those that do not exist are left out; a directory in another one shown
is seen in it. DIRECTORY is where it starts, /tmp unless given."
  (multiple-value-bind (status detail bwrap) (sandbox-status)
    (if (eq status :ready)
        (let ((start (get-internal-real-time))
              (copy (and input (if (sealed-p input) (reopen-sealed input) (sealed-copy input)))))
          (unwind-protect
               (multiple-value-bind (output outcome said)
                   (%sandbox-run bwrap program arguments copy seconds memory output-limit cancel ended-well
                                 :files files :environment environment :directories directories :writable writable
                                 :overlays overlays :directory directory :file-limit file-limit)
                 (tuatara.runtime:debug-report :job "sandbox: ~a ~{~a~^ ~}: ~(~a~) in ~,3f s, ~:d octets~@[: ~a~]"
                                               program arguments outcome
                                               (tuatara.runtime:seconds-since start)
                                               (length output) (and (plusp (length said)) said))
                 (values output outcome said))
            (when copy (sb-posix:close copy))))
        (values (make-array 0 :element-type '(unsigned-byte 8)) :unavailable detail))))

(defun sandbox-run-on-file (path program arguments &rest keys)
  "Run PROGRAM with ARGUMENTS shut in, as SANDBOX-RUN does (KEYS are its own
but INPUT), reading the file at PATH on its standard input. The file is opened
here, its access time left alone where the system allows; what is not a
regular file, a named pipe or a device, is refused without being opened, with
a FILE-SYSTEM-ERROR."
  (let ((fd (open-for-reading path t :accept '(:regular) :operation "decode")))
    (unwind-protect (apply #'sandbox-run program arguments :input fd keys)
      (sb-posix:close fd))))

(defun sandbox-outcome-words (outcome said &key (label "the program") (seconds-setting '*sandbox-seconds*))
  "Why a program run shut in (SANDBOX-RUN) gave nothing, as a sentence
beginning with LABEL, from how it went, OUTCOME, and what it SAID, as
SANDBOX-RUN gives them: there is no sandbox here, and why; it was stopped
after the seconds SECONDS-SETTING (a setting's symbol) holds, or by you; it
printed more than it may; or it failed, with the first line of what it said.
NIL when OUTCOME is :DONE."
  (case outcome
    (:done nil)
    (:unavailable (format nil "~a runs only shut in the sandbox, and there is none here: ~a" label said))
    (:timeout (format nil "~a was stopped after ~d seconds (~(~a~))" label (symbol-value seconds-setting) seconds-setting))
    (:cancelled (format nil "~a was stopped" label))
    (:too-much (format nil "~a printed more than it may" label))
    (t (let ((line (string-trim " " (or (first (uiop:split-string (or said "") :separator '(#\Newline))) ""))))
         (format nil "~a failed~@[: ~a~]" label (and (plusp (length line)) (subseq line 0 (min 300 (length line)))))))))
