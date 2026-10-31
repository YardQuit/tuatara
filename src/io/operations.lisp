;;;; src/io/operations.lisp --- Files and directories copied, moved, made, put in the trash or deleted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; What a file manager does to files, on names given as the system knows
;;;; them, octet for octet (a name that is not UTF-8 is kept as it is). Nothing
;;;; is replaced unless asked for: a rename refuses a name that is there
;;;; (renameat2's RENAME_NOREPLACE), a new file is made only where none is
;;;; (O_EXCL). A copy keeps the mode, the modification time and the extended
;;;; attributes, makes a symbolic link again rather than copying where it
;;;; leads, and shares blocks or lets the kernel copy where it can
;;;; (COPY-FD-CONTENTS). The trash is the one the desktop's file managers use
;;;; (freedesktop.org's): a file goes to it by a rename, with a .trashinfo
;;;; beside it saying where it was and when, so that they can put it back; on
;;;; another drive, that drive's own trash, never a copy.

(in-package #:tuatara.io)

(defun path-octets (path)
  "The name of PATH, or any other name the system is given (a string taken as
it is), as the system is given it: its octets, a NUL after them."
  (terminated-octets (tuatara.text:string-octets (native path))))

(defmacro with-path-sap ((sap path) &body body)
  "Run BODY with SAP pointing at PATH's name as the system is given it."
  (let ((octets (gensym "OCTETS")))
    `(let ((,octets (path-octets ,path)))
       (with-octets-sap (,sap ,octets) ,@body))))

(defun real-path (path)
  "PATH with every symbolic link in it followed and . and .. taken out, as
the system finds it; NIL when it is not there."
  (with-path-sap (name path)
    (let ((found (sb-alien:alien-funcall
                  (sb-alien:extern-alien "realpath" (function sb-sys:system-area-pointer sb-sys:system-area-pointer
                                                              sb-sys:system-area-pointer))
                  name (sb-sys:int-sap 0))))
      (unless (null-pointer-p found)
        (unwind-protect (tuatara.text:octets-string (sap-octets found))
          (sb-alien:alien-funcall (sb-alien:extern-alien "free" (function sb-alien:void sb-sys:system-area-pointer)) found))))))

(defun real-path-so-far (path)
  "PATH with every link in it followed and . and .. taken out as far as it is
there (REAL-PATH of the deepest folder of it that is), the rest after as
written; no / at its end. With /safe/link leading to /etc and no
/safe/link/new, /safe/link/new/file is /etc/new/file. A .. in the rest is
taken out and what it leads back into followed too: /safe/new/../link/file
is /etc/file."
  (let ((path (string-right-trim "/" path)))
    (do ((end (length path) (position #\/ path :end end :from-end t)))
        ((not (and end (plusp end))) path)
      (let ((real (real-path (subseq path 0 end))))
        (when real
          (let ((whole (concatenate 'string (string-right-trim "/" real) (subseq path end))))
            (return (if (member ".." (uiop:split-string (subseq path end) :separator "/") :test #'string=)
                        (real-path-so-far (clean-path whole))
                        whole))))))))

(defun check-call (result operation path)
  "Signal a FILE-SYSTEM-ERROR for OPERATION on PATH when RESULT, a system call's, is negative."
  (when (minusp result) (fail operation path))
  result)

(defun rename-entry (from-dirfd old to-dirfd new flags)
  "renameat2: the entry named at OLD (a pointer to a name ending in NUL) of
the directory open as FROM-DIRFD named NEW in TO-DIRFD's, with FLAGS (1 never
over one there already). 0 when done, else -1 and the errno."
  (if (zerop (sb-alien:alien-funcall
              (sb-alien:extern-alien "renameat2" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                           sb-alien:int sb-sys:system-area-pointer sb-alien:unsigned-int))
              from-dirfd old to-dirfd new flags))
      0
      (values -1 (sb-alien:get-errno))))

(defun rename-without-replacing (from-dirfd from to-dirfd to path)
  "Give the entry FROM (octets) of the directory open as FROM-DIRFD the name
TO (octets) in the directory open as TO-DIRFD, never over one there already:
in one step where the file system can; where it cannot (NFS, SMB and many
FUSE file systems), a file by giving it the second name, which is refused
when that is taken, and then taking its first away; a directory, or a file
where there are no second names, renamed once nothing has the name. A
FILE-SYSTEM-ERROR, PATH named, when it cannot be: EXDEV when TO is on
another file system."
  (let ((old (terminated-octets from)) (new (terminated-octets to)))
    (with-octets-sap (old-sap old)
      (with-octets-sap (new-sap new)
        (multiple-value-bind (result errno) (rename-entry from-dirfd old-sap to-dirfd new-sap 1) ; RENAME_NOREPLACE
          (when (zerop result) (return-from rename-without-replacing))
          (unless (member errno (list sb-posix:einval sb-posix:enosys))
            (fail "rename" path errno)))
        (let ((stamp (or (values (statx-octets from-dirfd from #x100)) (fail "rename" path))))
          (unless (eq (stamp-kind stamp) :directory)
            (if (zerop (sb-alien:alien-funcall
                        (sb-alien:extern-alien "linkat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                                  sb-alien:int sb-sys:system-area-pointer sb-alien:int))
                        from-dirfd old-sap to-dirfd new-sap 0))
                (return-from rename-without-replacing
                  (check-call (sb-alien:alien-funcall
                               (sb-alien:extern-alien "unlinkat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer sb-alien:int))
                               from-dirfd old-sap 0)
                              "rename" path))
                (let ((errno (sb-alien:get-errno)))
                  (unless (member errno (list sb-posix:eperm sb-posix:eopnotsupp sb-posix:emlink))
                    (fail "rename" path errno)))))
          (when (statx-octets to-dirfd to #x100)
            (fail "rename" path sb-posix:eexist))
          (multiple-value-bind (result errno) (rename-entry from-dirfd old-sap to-dirfd new-sap 0)
            (unless (zerop result) (fail "rename" path errno))))))))

(defun rename-path (from to &key replace)
  "Give the file or directory at FROM the name TO, which may be in another
directory of the same file system; a FILE-SYSTEM-ERROR when TO is there
already, unless REPLACE, or when it is on another file system. Refused
\(WRITING-REFUSED) where nothing may be written: FROM, anything within it,
or TO (CHECK-TREE-WRITING)."
  (check-tree-writing from "rename")
  (check-writing to "rename")
  (rename-checked from to :replace replace))

(defun rename-checked (from to &key replace)
  "RENAME-PATH's work, once where it writes has been looked at: by the
folders of FROM and TO held open, each looked at again where it really is
\(CALL-WITH-PARENT :WRITING), so that a folder swapped for a link meanwhile
leads nothing elsewhere."
  (call-with-parent
   from
   (lambda (from-dirfd from-name)
     (call-with-parent
      to
      (lambda (to-dirfd to-name)
        (if replace
            (with-octets-sap (old (terminated-octets from-name))
              (with-octets-sap (new (terminated-octets to-name))
                (multiple-value-bind (result errno) (rename-entry from-dirfd old to-dirfd new 0)
                  (unless (zerop result) (fail "rename" from errno)))))
            (rename-without-replacing from-dirfd from-name to-dirfd to-name from)))
      :writing "rename"))
   :writing "rename")
  to)

(defun ensure-directories (path)
  "Make the directories PATH needs, the one it names when it ends in /, else
the one it is in, as ENSURE-DIRECTORIES-EXIST does, but with its name taken as
it is: a [ or * in it is only a character. Refused (WRITING-REFUSED) where
nothing may be written, whether they are there or not: what they are made
for is written there next."
  (check-writing (if (uiop:string-suffix-p (native path) "/") (string-right-trim "/" (native path)) (parent-path path))
                 "make the folder")
  (ensure-directories-exist (uiop:parse-native-namestring (native path))))

(defun make-directory (path &optional (mode #o777))
  "Make the directory PATH, with MODE as your umask allows; a
FILE-SYSTEM-ERROR when it cannot be made, as when a file of that name is there;
WRITING-REFUSED where nothing may be written, told by the folder it is made
in, held open."
  (call-with-parent path (lambda (dirfd name) (make-directory-at dirfd name path mode)) :writing "make the directory")
  path)

(defun ensure-private-directory (path)
  "Make the directory PATH, and each directory above it that is not there,
for you alone (mode 700) from the moment it is made; those above it there
already are left as they are. PATH itself, when it is there already, is made
yours alone (700) when it is yours and others may use it. Returns PATH's
native name, ending in /. A FILE-SYSTEM-ERROR when one cannot be made, when
something not a directory is in the way, or when PATH is someone else's."
  (let ((name (slash-ended (native path))))
    (loop for slash = (position #\/ name :start 1) then (position #\/ name :start (1+ slash))
          while slash
          do (let ((level (subseq name 0 (1+ slash))))
               (unless (path-stamp level)
                 (handler-case (make-directory level #o700)
                   ;; made meanwhile by another session: what counts is that it is there now
                   (file-system-error (e) (unless (path-stamp level) (error e)))))))
    (unless (eq (path-kind name) :directory)
      (error 'file-system-error :operation "make the directory" :path name :reason "something else is there"))
    (let ((stat (sb-posix:stat name)))
      (unless (= (sb-posix:stat-uid stat) (sb-posix:getuid))
        (error 'file-system-error :operation "keep files in" :path name :reason "the folder is someone else's"))
      (unless (zerop (logand (sb-posix:stat-mode stat) #o077))
        (sb-posix:chmod name (logand (sb-posix:stat-mode stat) #o700))))
    name))

(defun remove-file (path)
  "Take away the name PATH of a file or a symbolic link (not where it leads);
refused (WRITING-REFUSED) where nothing may be written, told by the folder it
is in, held open."
  (call-with-parent path (lambda (dirfd name) (remove-entry-at dirfd name (native path) nil)) :writing "delete"))

(defun touch-path (path)
  "Set the access and modification times of the file or directory at PATH to
now; a symbolic link's own times, not those of where it leads. Refused
\(WRITING-REFUSED) where nothing may be written, told by the folder it is in,
held open, and while some place is, for a file with other names, one of
which may be there."
  (call-with-parent
   path
   (lambda (dirfd name)
     (let ((stamp (and (writing-refused-anywhere-p) (values (statx-octets dirfd name #x100)))))
       (when (and stamp (eq (stamp-kind stamp) :regular) (> (stamp-nlink stamp) 1))
         (error 'writing-refused :operation "touch" :path (native path)
                                 :reason (format nil "it has ~d names, and one may be in an evidence folder" (stamp-nlink stamp)))))
     (with-octets-sap (sap (terminated-octets name))
       (check-call (sb-alien:alien-funcall
                    (sb-alien:extern-alien "utimensat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                                 sb-sys:system-area-pointer sb-alien:int))
                    dirfd sap (sb-sys:int-sap 0) #x100)                ; AT_SYMLINK_NOFOLLOW
                   "touch" path)))
   :writing "touch"))

(defun make-symbolic-link (target path)
  "Make PATH a symbolic link leading to TARGET, a path as it is written in the
link; a FILE-SYSTEM-ERROR when PATH is taken, WRITING-REFUSED where nothing
may be written, told by the folder it is made in, held open."
  (call-with-parent
   path
   (lambda (dirfd name)
     (with-path-sap (to target)
       (with-octets-sap (sap (terminated-octets name))
         (check-call (sb-alien:alien-funcall
                      (sb-alien:extern-alien "symlinkat" (function sb-alien:int sb-sys:system-area-pointer
                                                                   sb-alien:int sb-sys:system-area-pointer))
                      to dirfd sap)
                     "make the link" path))))
   :writing "make the link"))

(defun make-hard-link (from path)
  "Give the file at FROM the second name PATH, on the same drive: one file,
two names (a symbolic link at FROM gets a second name itself). A
FILE-SYSTEM-ERROR when PATH is taken, FROM is a directory or the two are on
different drives; WRITING-REFUSED where nothing may be written, at PATH, told
by the folder it is made in, held open, or at FROM, whose file a second name
changes (its count of names, its time of change)."
  (check-writing from "give another name to")
  (call-with-parent
   path
   (lambda (dirfd name)
     (with-path-sap (old from)
       (with-octets-sap (sap (terminated-octets name))
         (check-call (sb-alien:alien-funcall
                      (sb-alien:extern-alien "linkat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                                sb-alien:int sb-sys:system-area-pointer sb-alien:int))
                      +at-fdcwd+ old dirfd sap 0)
                     "make the link" path))))
   :writing "make the link"))

(defun slash-ended (name)
  "NAME, a string, with a / at its end when it has none: a directory's name
as one to put another name after (\"/\" for an empty one)."
  (if (and (plusp (length name)) (char= #\/ (char name (1- (length name)))))
      name
      (concatenate 'string name "/")))

(defun directory-name (directory &key clean)
  "DIRECTORY (a string or pathname, ~ for the home directory) as an absolute
native name ending with a slash; with CLEAN, . and .. taken away as they
would lead (CLEAN-PATH)."
  (let ((name (native (expand-path directory))))
    (slash-ended (if clean (clean-path name) name))))

(defun path-join (directory name)
  "The path of NAME in DIRECTORY, one / between them."
  (concatenate 'string (slash-ended (native directory)) name))

;;; Copies, moves and deletes
;;;
;;; A tree is walked through descriptors of its directories, each entry named
;;; within the one above it and never followed when it is a symbolic link
;;; (O_NOFOLLOW), so that a directory put in the place of another, or swapped
;;; for a link while the walk goes on, never leads it out of the tree: what is
;;; deleted, read or made is always where the walk found it.

(defun entry-path (path name)
  "The path of the entry whose name is the octets NAME in the directory PATH, for messages."
  (path-join path (tuatara.text:octets-string name)))

(defun open-entry (dirfd name flags path &optional (mode 0))
  "A descriptor of the entry NAME (octets) of the directory open as DIRFD,
opened with FLAGS (made with MODE when they make it) and never through a
symbolic link; a FILE-SYSTEM-ERROR, PATH named, when it cannot be."
  (multiple-value-bind (fd errno) (openat-octets dirfd name (logior flags +o-nofollow+ +o-cloexec+) mode)
    (or fd (fail "open" path errno))))

(defun open-directory-entry (dirfd name path)
  "A descriptor of the directory NAME (octets) in the directory open as DIRFD,
for reading what is in it, its access time left alone where the system allows;
a FILE-SYSTEM-ERROR when it is not a directory now, a symbolic link included."
  (let ((flags (logior sb-posix:o-rdonly +o-directory+)))
    (multiple-value-bind (fd errno) (openat-octets dirfd name (logior flags +o-noatime+ +o-nofollow+ +o-cloexec+))
      (cond (fd fd)
            ((eql errno sb-posix:eperm) (open-entry dirfd name flags path))
            (t (fail "open" path errno))))))

(defun call-with-parent (path function &key writing)
  "Call FUNCTION with a descriptor of the directory PATH is in and PATH's own
name in it (octets), the descriptor closed after. With WRITING, a verb
phrase, refused (WRITING-REFUSED) when nothing may be written at PATH, told
by where the directory opened really is."
  (multiple-value-bind (parent name) (path-folder-and-name (string-right-trim "/" (native path)))
    (let ((own (tuatara.text:string-octets name)))
      (when (or (zerop (length own)) (equalp own #(46)) (equalp own #(46 46)))
        (error 'file-system-error :operation "change" :path path :reason "it names no entry of a directory"))
      ;; O_PATH: a folder you may write in but not list (-wx) is opened all the same
      (let ((dirfd (handler-case (sb-posix:open parent (logior +o-path+ +o-directory+ +o-cloexec+))
                     (sb-posix:syscall-error (e) (fail "open" parent (sb-posix:syscall-errno e))))))
        (unwind-protect (progn (when writing (check-writing path writing :dirfd dirfd))
                               (funcall function dirfd own))
          (sb-posix:close dirfd))))))

(defun fd-entry-names (fd path)
  "The names (octets) of what is in the directory open as FD, but . and ..,
sorted; FD stays open."
  (let ((names '()))
    (map-directory-fd (lambda (octets)
                        (unless (or (equalp octets #(46)) (equalp octets #(46 46)))
                          (push octets names)))
                      fd path)
    (sort names (lambda (a b) (minusp (octets-compare a b))))))

(defun octets-compare (a b)
  "Negative, zero or positive as the octets A sort before, with or after B."
  (let ((n (min (length a) (length b))))
    (dotimes (i n (- (length a) (length b)))
      (let ((d (- (aref a i) (aref b i))))
        (unless (zerop d) (return d))))))

(defun entry-stamp (dirfd name path)
  "The STAMP of the entry NAME (octets) of the directory open as DIRFD, a
symbolic link not followed; a FILE-SYSTEM-ERROR when it is not there."
  (or (values (statx-octets dirfd name #x100)) (fail "examine" path)))

(defun make-directory-at (dirfd name path mode)
  "Make the directory NAME (octets) in the directory open as DIRFD, with MODE
as the umask allows; a FILE-SYSTEM-ERROR, PATH named, when it cannot be made."
  (let ((held (terminated-octets name)))
    (with-octets-sap (sap held)
      (check-call (sb-alien:alien-funcall
                   (sb-alien:extern-alien "mkdirat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer sb-alien:unsigned-int))
                   dirfd sap mode)
                  "make the directory" path))))

(defun remove-entry-at (dirfd name path directory)
  "Take away the entry NAME (octets) of the directory open as DIRFD: an empty
directory when DIRECTORY, else a file or a symbolic link; a FILE-SYSTEM-ERROR,
PATH named, when it cannot be."
  (let ((held (terminated-octets name)))
    (with-octets-sap (sap held)
      (check-call (sb-alien:alien-funcall
                   (sb-alien:extern-alien "unlinkat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer sb-alien:int))
                   dirfd sap (if directory #x200 0))                     ; AT_REMOVEDIR
                  "delete" path))))

(defun set-fd-modification-time (fd nanoseconds)
  "Give the file open as FD the modification time NANOSECONDS (as a stamp has
it), its access time left as it is."
  (let ((times (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap times)
      (setf (sb-sys:sap-ref-64 sap 0) 0
            (sb-sys:sap-ref-64 sap 8) (- (ash 1 30) 2)
            (sb-sys:signed-sap-ref-64 sap 16) (floor nanoseconds 1000000000)
            (sb-sys:sap-ref-64 sap 24) (mod nanoseconds 1000000000))
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "futimens" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer))
       fd sap))))

(defun regular-fd-stamp (in path)
  "The stamp of the file open as IN, PATH's; a FILE-SYSTEM-ERROR when it is
no regular file now."
  (let ((stamp (fd-stamp in path)))
    (unless (eq (stamp-kind stamp) :regular)
      (error 'file-system-error :operation "copy" :path path
                                :reason (format nil "it is ~a now, not a file" (kind-words (stamp-kind stamp)))))
    stamp))

(defun copy-open-contents (in stamp out to-path &key cancel progress)
  "Copy the regular file open as IN, whose stamp is STAMP, into the new,
empty file open as OUT: its contents, its mode (but for setuid, setgid and
sticky), modification time and extended attributes; TO-PATH for messages.
True when all of it was copied; NIL when CANCEL, a cons, had a true car
first (OUT then holds part of it)."
  (when (or (clone-fd in out)
            (copy-fd-contents in out (stamp-size stamp) :path to-path :cancel cancel :progress progress))
    (copy-xattrs in out)
    (sb-posix:fchmod out (logand (stamp-mode stamp) #o777))
    (set-fd-modification-time out (stamp-mtime stamp))
    t))

(defun copy-open-file (in to-dirfd to-name path to-path &key cancel progress)
  "Copy the regular file open as IN to the new file TO-NAME (octets) of the
directory open as TO-DIRFD, not made through a symbolic link, as
COPY-OPEN-CONTENTS does; PATH and TO-PATH for messages. True when all of it
was copied; NIL when CANCEL, a cons, had a true car first (the new file then
holds part of it)."
  (let* ((stamp (regular-fd-stamp in path))
         (out (open-entry to-dirfd to-name (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl) to-path #o600)))
    (unwind-protect (copy-open-contents in stamp out to-path :cancel cancel :progress progress)
      (sb-posix:close out))))

(defun copy-file-at (from-dirfd from-name to-dirfd to-name path to-path &key cancel progress)
  "Copy the regular file FROM-NAME of the directory open as FROM-DIRFD to the
new file TO-NAME of the one open as TO-DIRFD (names octets, PATH and TO-PATH
for messages), as COPY-OPEN-FILE does, FROM-NAME read without changing its
access time where the system allows; neither name followed when it is a
symbolic link; what is not a regular file refused unopened (OPEN-EXAMINED)."
  (let ((in (open-examined from-dirfd from-name (logior sb-posix:o-rdonly sb-posix:o-nonblock +o-nofollow+) path
                           :safe t :operation "copy")))
    (unwind-protect (copy-open-file in to-dirfd to-name path to-path :cancel cancel :progress progress)
      (sb-posix:close in))))

(defun copy-regular-file (from to &key cancel progress)
  "Copy the file FROM, or the one it leads to when it is a symbolic link, to
the new file TO, as COPY-OPEN-FILE does; FROM read without changing its
access time where the system allows, TO never made through a link. True when
all of it was copied; NIL when CANCEL, a cons, had a true car first (TO then
holds part of it). What is not a regular file is refused unopened."
  (let ((in (open-for-reading from t :accept '(:regular) :operation "copy")))
    (unwind-protect (copy-open-file-to in from to :cancel cancel :progress progress)
      (sb-posix:close in))))

(defun copy-open-file-to (in from to &key cancel progress)
  "Copy the regular file open as IN, FROM's (for messages), to the new file
TO, as COPY-OPEN-FILE does, in the folder of TO held open and looked at
first where it really is (CALL-WITH-PARENT :WRITING); IN read at explicit
offsets, so that what was read of it before is copied whole. True when all
of it was copied; NIL when CANCEL, a cons, had a true car first."
  (call-with-parent to
    (lambda (to-dirfd to-name)
      (copy-open-file in to-dirfd to-name (native from) (native to) :cancel cancel :progress progress))
    :writing "copy"))

(defun copy-to-free-name (from directory name &key cancel)
  "Copy the file FROM, or the one it leads to when it is a symbolic link,
into DIRECTORY as COPY-REGULAR-FILE does, as a new file called NAME, or
where that is taken NAME with (2), (3) and so on before its ending
\(NUMBERED-NAME): never over anything there, the name taken in one step
\(CREATE-FREE-FILE). Returns the copy's path; NIL when CANCEL, a cons, had a
true car first (the copy then holds part of it)."
  (let ((in (open-for-reading from t :accept '(:regular) :operation "copy")))
    (unwind-protect
         (let ((stamp (regular-fd-stamp in (native from))))
           (multiple-value-bind (out path) (create-free-file directory (lambda (n) (numbered-name name n)))
             (unwind-protect (and (copy-open-contents in stamp out path :cancel cancel) path)
               (sb-posix:close out))))
      (sb-posix:close in))))

(defparameter *deepest-folder-tree* 200
  "The most folders, one inside another, that copying, moving or deleting a
folder goes down through. Each holds a descriptor open while what is in it is
dealt with, so a tree nested deeper, as a damaged file system or a hostile
archive can hold, is refused at that depth, before descriptors or the stack
run out half-way.")

(defun check-tree-depth (depth path)
  "Signal a FILE-SYSTEM-ERROR for PATH when DEPTH, the folders above it within
the tree being copied or deleted, passes *DEEPEST-FOLDER-TREE*."
  (when (> depth *deepest-folder-tree*)
    (error 'file-system-error :operation "go into" :path path
                              :reason (format nil "it is nested more than ~d folders deep" *deepest-folder-tree*))))

(defun copy-entry-at (from-dirfd from-name to-dirfd to-name path to-path cancel progress skipped &optional (depth 0))
  "COPY-PATH-TREE for the entry FROM-NAME of the directory open as FROM-DIRFD,
copied to TO-NAME of the one open as TO-DIRFD (names octets; PATH and TO-PATH
their paths, for messages), DEPTH folders down the tree (CHECK-TREE-DEPTH)."
  (when (and cancel (car cancel)) (return-from copy-entry-at nil))
  (let* ((stamp (entry-stamp from-dirfd from-name path))
         (kind (stamp-kind stamp)))
    (case kind
      (:regular (copy-file-at from-dirfd from-name to-dirfd to-name path to-path
                              :cancel cancel
                              :progress (and progress (let ((last 0))
                                                        (lambda (done) (funcall progress (- done last)) (setf last done))))))
      (:symlink
       (let ((target (or (link-octets from-dirfd from-name) (fail "read the link" path))))
         (let ((held-target (terminated-octets target)) (held-name (terminated-octets to-name)))
           (with-octets-sap (target-sap held-target)
             (with-octets-sap (name-sap held-name)
               (check-call (sb-alien:alien-funcall
                            (sb-alien:extern-alien "symlinkat" (function sb-alien:int sb-sys:system-area-pointer
                                                                         sb-alien:int sb-sys:system-area-pointer))
                            target-sap to-dirfd name-sap)
                           "make the link" to-path)))))
       t)
      (:directory
       (check-tree-depth depth path)
       (make-directory-at to-dirfd to-name to-path #o700)
       (let ((from-fd (open-directory-entry from-dirfd from-name path)))
         (unwind-protect
              (let ((to-fd (open-entry to-dirfd to-name (logior sb-posix:o-rdonly +o-directory+) to-path))
                    (whole t))
                (unwind-protect
                     (progn
                       (dolist (name (fd-entry-names from-fd path))
                         (unless (copy-entry-at from-fd name to-fd name (entry-path path name) (entry-path to-path name)
                                                cancel progress skipped (1+ depth))
                           (setf whole nil)))
                       (sb-posix:fchmod to-fd (logand (stamp-mode stamp) #o777))
                       (set-fd-modification-time to-fd (stamp-mtime stamp))
                       whole)
                  (sb-posix:close to-fd)))
           (sb-posix:close from-fd))))
      (t (when skipped (funcall skipped path (kind-words kind)))
         nil))))

(defun copy-path-tree (from to &key cancel progress skipped)
  "Copy FROM, a file, a symbolic link or a directory with all in it, to the new
name TO. A symbolic link is made again, holding what it holds; a named pipe, a
socket or a device is not copied, SKIPPED, when given, called with its path
and what it is. CANCEL, a cons, stops it between files when its car turns
true; PROGRESS is called with the octets of each piece copied. True when all
of it was copied; NIL when it was stopped or something in it was not copied.
Nothing in either tree is followed when it is a symbolic link, even one put
there while the copy goes on: all that is read is in FROM, all that is made
is in TO. The copy is made under a hidden name beside TO and given TO's name
once done, so that nothing is ever under TO with part of what it should hold:
a copy stopped, failed or cut short leaves nothing there, and what a crash
leaves is a hidden .tuatara-copy-... name."
  (when (and cancel (car cancel)) (return-from copy-path-tree nil))
  (let ((scratch (scratch-name (parent-path to) "copy")) (named nil))
    (unwind-protect
         (let ((whole (call-with-parent from
                        (lambda (from-dirfd from-name)
                          (call-with-parent scratch
                            (lambda (to-dirfd to-name)
                              (copy-entry-at from-dirfd from-name to-dirfd to-name (native from) scratch cancel progress skipped))
                            :writing "copy")))))
           (unless (and cancel (car cancel))
             (check-writing to "copy")
             (rename-checked scratch to)
             (setf named t))
           (and named whole))
      (unless named (ignore-errors (delete-tree scratch))))))

(defun delete-entry-at (dirfd name path cancel &optional (depth 0))
  "DELETE-TREE for the entry NAME (octets) of the directory open as DIRFD, PATH
its path, DEPTH folders down the tree (CHECK-TREE-DEPTH). An empty directory
that cannot be read is removed all the same."
  (when (and cancel (car cancel)) (return-from delete-entry-at nil))
  (if (eq (stamp-kind (entry-stamp dirfd name path)) :directory)
      (let ((fd (handler-case (open-directory-entry dirfd name path)
                  (file-system-error (condition)
                    ;; a directory that cannot be read (one the kernel leaves with no
                    ;; permissions at all): gone when it is empty, else the reason it cannot be
                    (handler-case (progn (remove-entry-at dirfd name path t)
                                         (return-from delete-entry-at t))
                      (file-system-error () (error condition)))))))
        (unwind-protect
             (progn
               (check-tree-depth depth path)
               (dolist (entry (fd-entry-names fd path))
                 (delete-entry-at fd entry (entry-path path entry) cancel (1+ depth))))
          (sb-posix:close fd))
        (unless (and cancel (car cancel))
          (remove-entry-at dirfd name path t)))
      (remove-entry-at dirfd name path nil))
  t)

(defun delete-tree (path &key cancel)
  "Delete PATH for good: a file or a symbolic link (not where it leads), or a
directory with all in it. CANCEL, a cons, stops it between files. Nothing in
it is followed when it is a symbolic link, even one put there while the
deleting goes on: only what is in PATH is deleted."
  (when (and cancel (car cancel)) (return-from delete-tree nil))
  (check-tree-writing path "delete")
  (call-with-parent path (lambda (dirfd name) (delete-entry-at dirfd name (native path) cancel)) :writing "delete")
  t)

(defun check-tree-writing (path operation)
  "Signal WRITING-REFUSED for OPERATION on PATH when nothing may be written
there, or, PATH being a folder, in any folder within it (WRITING-REFUSAL),
each told by where it really is: a folder holding such a place is not
deleted, moved or renamed as a whole. Links in it are not followed. Looks
within PATH only while some place is refused (WRITING-REFUSED-ANYWHERE-P)."
  (check-writing path operation)
  (when (and (writing-refused-anywhere-p) (eq :directory (path-kind path :follow nil)))
    (labels ((walk (dirfd name path depth)
               (let ((fd (handler-case (open-directory-entry dirfd name path)
                           (file-system-error () nil))))
                 (when fd
                   (unwind-protect
                        (progn
                          (check-writing path operation :fd fd)
                          (check-tree-depth depth path)
                          (dolist (entry (fd-entry-names fd path))
                            (let ((stamp (statx-octets fd entry #x100)))
                              (when (and stamp (eq :directory (stamp-kind stamp)))
                                (walk fd entry (entry-path path entry) (1+ depth))))))
                     (sb-posix:close fd))))))
      (call-with-parent path (lambda (dirfd name) (walk dirfd name (string-right-trim "/" (native path)) 0))))))

(defun move-path (from to &key cancel progress skipped)
  "Move FROM to the new name TO: a rename on one file system; on another, a
copy, FROM deleted only once all of it is copied. When the copy stopped or
left something out (a named pipe, a socket, a device), FROM is left as it was,
nothing of it under TO, and a FILE-SYSTEM-ERROR says so; SKIPPED as
COPY-PATH-TREE has it."
  (check-tree-writing from "move")
  (check-writing to "move")
  (handler-case (rename-checked from to)
    (file-system-error (e)
      (if (and (path-stamp from :follow nil)
               (not (path-stamp to :follow nil))
               (eql (file-system-error-errno e) sb-posix:exdev))  ; another file system
          (if (copy-path-tree from to :cancel cancel :progress progress :skipped skipped)
              (delete-tree from)
              (progn
                ;; what was copied of it taken away again: it stays where it was, whole
                (when (path-stamp to :follow nil) (ignore-errors (delete-tree to)))
                (error 'file-system-error :operation "move" :path from
                                          :reason (format nil "not all of it could be copied to ~a; it is left where it was" to))))
          (error e))))
  to)

;;; The trash (freedesktop.org's Trash specification)

(defun data-home (&optional home)
  "Where your data lives: $XDG_DATA_HOME, or .local/share/ in HOME, your home
directory unless given."
  (or (uiop:getenv-absolute-directory "XDG_DATA_HOME")
      (merge-pathnames ".local/share/" (if home
                                           (uiop:ensure-directory-pathname (uiop:parse-native-namestring (native home)))
                                           (user-homedir-pathname)))))

(defun user-font-directories (&optional (home (home-directory)))
  "Your own font folders, native names without a / at the end: fonts in the
data home ($XDG_DATA_HOME, or .local/share in HOME), then .fonts in HOME;
HOME is your home directory unless given."
  (list (path-join (native (data-home home)) "fonts")
        (format nil "~a/.fonts" (string-right-trim "/" (native home)))))

(defun home-trash ()
  "Your own trash directory: Trash/ in the data home."
  (native (merge-pathnames "Trash/" (data-home))))

(defun parent-path (path)
  "The directory PATH is in, ending in /."
  (let* ((name (string-right-trim "/" (native path)))
         (slash (position #\/ name :from-end t)))
    (if (and slash (plusp slash)) (subseq name 0 (1+ slash)) "/")))

(defun mount-top (directory)
  "The top directory of the file system DIRECTORY is on: the last going up
from it that is on the same device."
  (let ((device (stamp-dev (path-stamp directory))))
    (loop for here = (native directory) then up
          for up = (parent-path here)
          when (or (string= here "/") (not (eql device (let ((stamp (path-stamp up))) (and stamp (stamp-dev stamp))))))
            return (if (string= here "/") "/" (concatenate 'string (string-right-trim "/" here) "/")))))

(defun ensure-trash (trash)
  "Make TRASH, a trash directory, with its files/ and info/, for you alone
where they are made; TRASH as it is."
  (dolist (path (list (path-join trash "files/") (path-join trash "info/")))
    (ignore-errors (ensure-private-directory path)))
  (unless (and (path-stamp (path-join trash "files/")) (path-stamp (path-join trash "info/")))
    (error 'file-system-error :operation "make the trash" :path trash :reason "it cannot be made there"))
  trash)

(defun real-directory-p (path &key (owner nil owner-p))
  "True when PATH is a directory itself, not a symbolic link to one, owned by
OWNER (a user id) when given."
  ;; without its last slash: a path ending in one leads through a link to where it goes
  (let ((stamp (ignore-errors (path-stamp (string-right-trim "/" path) :follow nil))))
    (and stamp (eq (stamp-kind stamp) :directory)
         (or (not owner-p) (= (stamp-uid stamp) owner)))))

(defun drive-trash-sound-p (trash top)
  "True when the trash TRASH on the drive whose top is TOP may be used: it,
its files/ and info/ directories themselves and yours, none a symbolic link
leading elsewhere; a shared .Trash above it a directory itself with the
sticky bit, as the trash's rules ask. A trash put there by someone else, or
with links in it, is left alone: deleting, putting back or putting in it
could reach files outside it."
  (let ((uid (sb-posix:getuid)))
    (and (real-directory-p trash :owner uid)
         (real-directory-p (path-join trash "files/") :owner uid)
         (real-directory-p (path-join trash "info/") :owner uid)
         (let ((shared (format nil "~a.Trash/" top)))
           (or (not (uiop:string-prefix-p shared trash))
               (let ((stamp (ignore-errors (path-stamp (string-right-trim "/" shared) :follow nil))))
                 (and stamp (eq (stamp-kind stamp) :directory)
                      (logtest #o1000 (stamp-mode stamp)))))))))

(defun trash-for (path)
  "The trash directory a file at PATH goes to, as two values: the directory,
and the top it counts names from (NIL for your own trash, where names
are whole). Your own trash when it is on the same file system; else, on
that file system's top, .Trash/UID when an administrator made .Trash (sticky,
not a link), else .Trash-UID; on another drive, only a trash that is sound
\(DRIVE-TRASH-SOUND-P), a FILE-SYSTEM-ERROR when neither is."
  (let* ((device (stamp-dev (or (path-stamp (parent-path path)) (fail "examine" path))))
         (home (home-trash)))
    (ensure-private-directory home)
    (if (eql device (let ((stamp (path-stamp home))) (and stamp (stamp-dev stamp))))
        (values (ensure-trash home) nil)
        (let* ((top (mount-top (parent-path path)))
               (uid (sb-posix:getuid))
               (shared (path-join top ".Trash"))
               (shared-stamp (path-stamp shared :follow nil)))
          (dolist (trash (append (and shared-stamp (eq (stamp-kind shared-stamp) :directory) (logtest (stamp-mode shared-stamp) #o1000)
                                      (list (path-join shared (format nil "~d/" uid))))
                                 (list (path-join top (format nil ".Trash-~d/" uid))))
                         (error 'file-system-error :operation "put in the trash" :path path
                                                   :reason "the trash on its drive has links in it or is not yours"))
            (when (and (ignore-errors (ensure-trash trash)) (drive-trash-sound-p trash top))
              (return (values trash top))))))))

(defun clean-path (path &key strict)
  "PATH, a native name taken from the root, with . and .. taken away as they
would lead and every / doubled or at its end left out: /a/b/../c/./ is /a/c,
and the root is /. A .. at the root stays there, as the system has it; with
STRICT, NIL then instead."
  (let ((parts '()))
    (dolist (part (uiop:split-string path :separator "/"))
      (cond ((or (string= part "") (string= part ".")))
            ((string= part "..") (if parts (pop parts) (when strict (return-from clean-path nil))))
            (t (push part parts))))
    (format nil "/~{~a~^/~}" (reverse parts))))

(defun path-within-p (path top)
  "True when PATH, its . and .. taken as written, is the directory TOP or
within it: /data/case/x is within /data/case, /data/cases/x is not."
  (let ((clean (clean-path path :strict t))
        (top (string-right-trim "/" top)))
    (and clean
         (or (string= clean top)
             (uiop:string-prefix-p (concatenate 'string top "/") clean)))))

(defun real-path-or-parent (path)
  "Where the file at PATH (native) really is, every link followed and . and ..
taken out (REAL-PATH); for a file not there, where its folder really is with
its name after; NIL when neither is there."
  (or (ignore-errors (real-path path))
      (let ((folder (ignore-errors (real-path (parent-path path))))
            (name (native-file-name path)))
        (and folder name (not (member name '("." "..") :test #'string=))
             (path-join folder name)))))

(defun really-within-p (path folder)
  "True when the file at PATH (native) is FOLDER or in it, both where they
really are (REAL-PATH-SO-FAR), so that no link leads it out, however much of
PATH is not there yet: what a file or a program may have the editor read or
write is checked so."
  (let ((top (clean-path (real-path-so-far folder) :strict t)))
    (and top (path-within-p (real-path-so-far path) top))))

(defun percent-encoded (text &key (safe ""))
  "TEXT's octets in UTF-8, those other than ASCII letters, digits and the
characters of SAFE written %XX, as an address or a file name made of
another name is written."
  (with-output-to-string (out)
    (loop for octet across (tuatara.text:string-octets text)
          do (if (or (<= 48 octet 57) (<= 65 octet 90) (<= 97 octet 122) (find (code-char octet) safe))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun path-file-name (path)
  "PATH, a native name, written as one file name that names it alone: each
octet of it in UTF-8 but ASCII letters, digits, . _ and - written as %XX
\(PERCENT-ENCODED); for a name over 240 characters, its first 200 then ~ and
a number made from all of PATH (TUATARA.TEXT:STRING-HASH) in base 36, so that
it always fits in a folder. True as a second value when it was shortened."
  (let ((encoded (percent-encoded path :safe "._-")))
    (if (<= (length encoded) 240)
        (values encoded nil)
        (values (format nil "~a~~~36r" (subseq encoded 0 200) (tuatara.text:string-hash path)) t))))

(defun percent-decoded-octets (text)
  "The octets TEXT stands for, each %XX the octet XX, the rest of it as UTF-8."
  (let ((octets (make-array (length text) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length text))
          do (let ((c (char text i)))
               (if (and (char= c #\%) (< (+ i 2) (length text)) (digit-char-p (char text (1+ i)) 16) (digit-char-p (char text (+ i 2)) 16))
                   (progn (vector-push-extend (parse-integer text :start (1+ i) :end (+ i 3) :radix 16) octets) (incf i 3))
                   (progn (loop for o across (tuatara.text:string-octets (string c)) do (vector-push-extend o octets)) (incf i)))))
    (coerce octets '(simple-array (unsigned-byte 8) (*)))))

(defun percent-decoded (text)
  "TEXT with each %XX made the octet it stands for again, the octets read as
UTF-8: what PERCENT-ENCODED wrote, as a file: address or a .trashinfo has a
path."
  (tuatara.text:octets-string (percent-decoded-octets text)))

(defun url-path (path)
  "PATH as the path of an address has it, a file: address's and a
.trashinfo's Path=: its octets, those other than letters, digits, / and -._~
written %XX."
  (percent-encoded path :safe "/-._~"))

(defun trash-info-path (path top)
  "Where the file PATH was, as its .trashinfo says it: whole in your own
trash (TOP NIL); in a trash on another drive, counted from TOP, that drive's
top (ending in /), with no slash before it."
  (if top (subseq path (length top)) path))

(defun trash-name (base n)
  "The name in the trash of a thing named BASE at the Nth try: BASE, then
BASE.2, BASE.3 and so on, its BASE cut short where the name of its .trashinfo
would be longer than a file's name may be (255 octets)."
  (let ((suffix (if (= n 1) "" (format nil ".~d" n))))
    (loop for end downfrom (length base) above 0
          for name = (concatenate 'string (subseq base 0 end) suffix)
          when (<= (length (tuatara.text:string-octets name)) (- 255 (length ".trashinfo")))
            return name)))

(defun trash-path (path &key (time (get-universal-time)))
  "Move the file or directory PATH to the trash (TRASH-FOR), by a rename, with
a .trashinfo saying where it was and when, so that the desktop's file managers
can put it back. Returns where it went; a FILE-SYSTEM-ERROR when there is no
trash for it."
  (let* ((path (string-right-trim "/" (native path)))
         (base (native-file-name path)))
    (check-tree-writing path "put in the trash")
    (multiple-value-bind (trash top) (trash-for path)
      (flet ((target (n) (path-join trash (format nil "files/~a" (trash-name base n)))))
        ;; the .trashinfo made first keeps the name for this file alone; a name taken
        ;; in files/ with no .trashinfo of its own (left by another program) passed over too
        (multiple-value-bind (fd info n)
            (create-free-file (path-join trash "info/") (lambda (n) (format nil "~a.trashinfo" (trash-name base n)))
                              :usable (lambda (n) (null (path-stamp (target n) :follow nil))))
          (unwind-protect
               (write-octets fd (tuatara.text:string-octets
                                 (format nil "[Trash Info]~%Path=~a~%DeletionDate=~a~%"
                                         (url-path (trash-info-path path top))
                                         (tuatara.dates:format-time "%FT%T" time)))
                             :path info)
            (sb-posix:close fd))
          (handler-case (rename-checked path (target n))
            (error (e) (ignore-errors (remove-file info)) (error e)))
          (target n))))))

;;; New names

(defun numbered-name (name n)
  "NAME for the Nth try at a name not taken: NAME itself, then NAME with
\(2), (3) and so on before its ending, as report (2).pdf."
  (if (= n 1)
      name
      (format nil "~a (~d)~@[.~a~]" (native-file-stem name) n (native-file-type name))))

(defun create-free-file (directory name-of &key (mode #o600) (usable (constantly t)))
  "A new, empty file in DIRECTORY, made with MODE under the first name that
NAME-OF, a function of N, gives for N from 1 where nothing is yet and USABLE,
a function of N, is true. Each name is taken in one step, only when nothing
has it, never through a link, so that no other program is given it
meanwhile. Returns three values: a descriptor writing the file, its path and
N. A FILE-SYSTEM-ERROR when it cannot be made for another reason than its
name being taken, or 10,000 names are; WRITING-REFUSED where nothing may be
written, told by DIRECTORY, held open."
  (call-with-parent
   (path-join directory (funcall name-of 1))
   (lambda (dirfd first)
     (declare (ignore first))
     (loop for n from 1 to 10000
           for name = (funcall name-of n)
           for path = (path-join directory name)
           do (when (funcall usable n)
                (multiple-value-bind (fd errno)
                    (openat-octets dirfd (tuatara.text:string-octets name)
                                   (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl +o-nofollow+ +o-cloexec+) mode)
                  (cond (fd (return (values fd path n)))
                        ((/= errno sb-posix:eexist) (fail "make" path errno)))))
           finally (error 'file-system-error :operation "find a free name in" :path directory
                                             :reason "every name tried is taken")))
   :writing "make"))

;;; Wiping a file

(defparameter *copy-on-write-file-systems*
  (list +btrfs-magic+ +zfs-magic+ +bcachefs-magic+ +f2fs-magic+)
  "The magic numbers of the file systems that write a file's new contents
elsewhere than its old ones: btrfs, ZFS, bcachefs and F2FS (written as a log).
Overwriting a file there leaves the old blocks as they were, until the file
system uses them again.")

(defun copy-on-write-p (path)
  "True when the file at PATH is on a file system that writes changes
elsewhere (*COPY-ON-WRITE-FILE-SYSTEMS*), where overwriting it leaves its old contents on disk."
  (and (member (path-file-system path :follow nil) *copy-on-write-file-systems*) t))

(defun rename-entry-at (dirfd from to path)
  "Give the entry FROM (octets) of the directory open as DIRFD the name TO
\(octets) in it, never over one there already (RENAME-WITHOUT-REPLACING); a
FILE-SYSTEM-ERROR, PATH named, when it cannot be."
  (rename-without-replacing dirfd from dirfd to path))

(defun wipe-file (path &key cancel progress (passes 1))
  "Wipe the file at PATH: its contents overwritten PASSES times with random
octets, each pass put on disk, then the file emptied, renamed to a random
name, deleted, and its folder's change put on disk; so that its contents and
name are not found again where the disk writes in place. A link, what is no
regular file, and a file with other names too (whose contents they keep) are
refused with a FILE-SYSTEM-ERROR. The renaming and deleting go by its folder,
held open, and only while the name still leads to the very file overwritten.
CANCEL, a cons, stops it between pieces when its car turns true (:STOPPED
returned, the file then partly overwritten); PROGRESS is called with the
octets overwritten so far, all passes counted. Returns T once it is gone."
  (let ((native (native path)))
    (call-with-parent
     native
     (lambda (dirfd own)
       (let ((fd (open-examined dirfd own (logior sb-posix:o-wronly +o-nofollow+) native :operation "wipe" :wanted "a file"))
             (random nil)
             (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
         (unwind-protect
              (let* ((stamp (fd-stamp fd native))
                     (size (stamp-size stamp)))
                (setf random (sb-posix:open "/dev/urandom" (logior sb-posix:o-rdonly +o-cloexec+)))
                (when (> (stamp-nlink stamp) 1)
                  (error 'file-system-error :operation "wipe" :path native
                                            :reason (format nil "it has ~d names: the others keep its contents" (stamp-nlink stamp))))
                (dotimes (pass passes)
                  (sb-posix:lseek fd 0 sb-posix:seek-set)
                  (let ((done 0))
                    (loop while (< done size)
                          do (when (and cancel (car cancel)) (return-from wipe-file :stopped))
                             (let ((n (min (length buffer) (- size done))))
                               (read-fd-at random buffer 0 :end n :path "/dev/urandom")
                               (write-octets fd buffer :end n :path native)
                               (incf done n)
                               (when progress (funcall progress (+ (* pass size) done))))))
                  (sb-posix:fsync fd))
                (sb-posix:ftruncate fd 0)
                (sb-posix:fsync fd)
                ;; its name too: renamed to one that says nothing, then gone, the file still the one overwritten
                (let ((hidden (tuatara.text:string-octets (concatenate 'string "." (random-name)))))
                  (flet ((same-file-p (name)
                           (let ((now (entry-stamp dirfd name native)))
                             (and (= (stamp-dev now) (stamp-dev stamp)) (= (stamp-ino now) (stamp-ino stamp))))))
                    (unless (same-file-p own)
                      (error 'file-system-error :operation "wipe" :path native :reason "its name was given to another file meanwhile"))
                    (rename-entry-at dirfd own hidden native)
                    (if (same-file-p hidden)
                        (remove-entry-at dirfd hidden native nil)
                        (error 'file-system-error :operation "wipe" :path native :reason "its name was given to another file meanwhile"))))
                (sync-directory (parent-path native)))
           (sb-posix:close fd)
           (when random (sb-posix:close random))
           (fill buffer 0))))
     :writing "wipe")
    t))
