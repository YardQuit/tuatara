;;;; src/io/posix.lisp --- The system calls files need that sb-posix does not wrap.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; statx for exact times, statfs to tell file systems apart, statvfs for free
;;;; space, the FICLONE ioctl that clones a file in no time where the file system
;;;; shares blocks (btrfs, XFS, bcachefs, ZFS), copy_file_range for copies the
;;;; kernel makes, and extended attributes (SELinux labels, ACLs). Every one has a
;;;; plain fallback in files.lisp, so all Linux file systems are served.

(in-package #:tuatara.io)

(define-condition file-system-error (error)
  ((operation :initarg :operation :reader file-system-error-operation
              :documentation "What was being done, as a verb phrase: \"open\", \"rename\".")
   (path :initarg :path :reader file-system-error-path :documentation "The file concerned.")
   (reason :initarg :reason :reader file-system-error-reason :documentation "Why, in the system's words.")
   (errno :initarg :errno :initform nil :reader file-system-error-errno
          :documentation "The system's number for the reason, or NIL when the refusal is the editor's own."))
  (:report (lambda (c stream)
             (format stream "Could not ~a ~a: ~a"
                     (file-system-error-operation c) (file-system-error-path c) (file-system-error-reason c))))
  (:documentation "A file operation the system refused, with its reason."))

(setf (documentation 'file-system-error-operation 'function) "What a FILE-SYSTEM-ERROR was doing, as a verb phrase."
      (documentation 'file-system-error-path 'function) "The file a FILE-SYSTEM-ERROR concerns."
      (documentation 'file-system-error-reason 'function) "Why the system refused, in its words."
      (documentation 'file-system-error-errno 'function) "The system's number for why a
FILE-SYSTEM-ERROR happened, or NIL.")

;;; Names as the system has them

(defun terminated-octets (octets)
  "OCTETS with a null (NUL) octet after them, for the system."
  (let ((name (make-array (1+ (length octets)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace name octets)
    name))

(defun keep-raw-bytes-in-names ()
  "Have every string handed to the system (a path to open, examine, rename)
keep its raw bytes' characters (U+DC80..U+DCFF, what a name that is not UTF-8
is read as) as the octets they stand for, so that such a name reaches the
system exactly as it was read; any other string is written as UTF-8, as
before. Done once, as the program loads: Steel Bank Common Lisp (SBCL)'s
writer of UTF-8 for the system is wrapped, as its TRACE wraps a function."
  (let ((name 'sb-impl::output-to-c-string/utf-8/lf))
    (unless (sb-int:encapsulated-p name 'raw-bytes)
      (sb-int:encapsulate name 'raw-bytes
                          (lambda (original string)
                            (if (find-if (lambda (c) (tuatara.text:raw-byte-code-p (char-code c))) string)
                                (terminated-octets (tuatara.text:string-octets string))
                                (funcall original string)))))))

(keep-raw-bytes-in-names)

(defun errno-reason (&optional (errno (sb-alien:get-errno)))
  "The system's words for ERRNO, the last error by default."
  (sb-int:strerror errno))

(defun native (path)
  "The name the system knows PATH by: a pathname's native namestring, in which
[, *, ? and \\ are only characters; a string is taken as it is."
  (if (pathnamep path) (sb-ext:native-namestring path) path))

(defun native-file-name (path)
  "The last part of PATH's name, as the system knows it: \"report [final].csv\"."
  (let ((name (string-right-trim "/" (native path))))
    (subseq name (1+ (or (position #\/ name :from-end t) -1)))))

(defun path-directory-and-name (path)
  "PATH's name parted at its last /, as two values: the directory, ending in /
\(./ when there is no /), and the name after it, empty for a name ending in
/. For /data/case/notes [old].org they are \"/data/case/\" and \"notes [old].org\"."
  (let* ((name (native path)) (slash (position #\/ name :from-end t)))
    (if slash
        (values (subseq name 0 (1+ slash)) (subseq name (1+ slash)))
        (values "./" name))))

(defun native-file-stem (path)
  "The last part of PATH's name without its ending: \"report [final]\" for
report [final].tex; a name beginning with a dot alone (.bashrc) kept whole."
  (let* ((name (native-file-name path)) (dot (position #\. name :from-end t)))
    (if (and dot (plusp dot)) (subseq name 0 dot) name)))

(defun native-file-type (path)
  "The ending of PATH's name, without its dot (tex); NIL for none."
  (let* ((name (native-file-name path)) (dot (position #\. name :from-end t)))
    (and dot (plusp dot) (< (1+ dot) (length name)) (subseq name (1+ dot)))))

(defun directory-pathname (directory)
  "DIRECTORY, a pathname or a string taken literally (a [ or * in it is only a
character), as the pathname of a directory, whether or not its name ends with
a slash."
  (sb-ext:parse-native-namestring (native directory) nil *default-pathname-defaults* :as-directory t))

(defun own-directory (name)
  "The directory the environment variable NAME names for the editor itself
(OWN-VARIABLE), as a pathname ending in /; NIL when it is not set, is empty or
does not start at the root."
  (let ((value (own-variable name)))
    (and value (plusp (length value))
         (let ((directory (uiop:ensure-directory-pathname (uiop:parse-native-namestring value))))
           (and (uiop:absolute-pathname-p directory) directory)))))

(defvar *home-names* nil
  "The home directory's names, as HOME gives it and as the file system has it
(where HOME leads there through a link, as /home to /var/home), each ending in
/, kept with the HOME they were found for: (HOME . NAMES).")

(defun home-directory-names ()
  "The names of the home directory, HOME's first, then the file system's own
when it differs (*HOME-NAMES*), found once for each HOME."
  (let ((home (sb-ext:native-namestring (user-homedir-pathname))))
    (unless (equal (car *home-names*) home)
      (let ((true (tuatara.runtime:ignoring-errors-reported ("finding the home directory ~a" home)
                    (probe-file (user-homedir-pathname)))))
        (setf *home-names* (cons home (remove-duplicates (list home (if true (sb-ext:native-namestring true) home))
                                                         :test #'string= :from-end t)))))
    (cdr *home-names*)))

(defun home-directory ()
  "The home directory as the file system names it, ending in /: where ~ leads,
so that a file reached by ~ has the name it has when reached any other way."
  (car (last (home-directory-names))))

(define-setting *default-directory* "~/"
  "The directory of a buffer with no file (*scratch*, the lists).
A file name typed there is taken from it, and \\[search-in-files] searches
it. A buffer with a file has its file's directory.
- \"~/\": your home directory (the standard); ~ is your home directory in any
  name.
- a directory's name: that directory.
- nil: the directory Tuatara was started in."
  :values (or (member nil) directory))

(defun without-dot-components (pathname)
  "PATHNAME without the . in its directory: /a/./b is /a/b, whatever links
are in it (a . leads nowhere else). A .. is left, which a link may make lead
elsewhere than the lexical way back."
  (let ((directory (pathname-directory pathname)))
    (if (member "." directory :test #'equal)
        (make-pathname :directory (remove "." directory :test #'equal) :defaults pathname)
        pathname)))

(defun expand-path (path &key directory (tilde t))
  "PATH as an absolute pathname: a string is taken literally, so that [, *, ?
and \\ in a file's name are only characters, and with TILDE a leading ~ or ~/
means your home directory (HOME-DIRECTORY); a relative one is taken from
DIRECTORY, a pathname or a string taken literally too, by default the
DEFAULT-DIRECTORY, asked for only then: an absolute path asks the file
system nothing. Its . components are taken out (WITHOUT-DOT-COMPONENTS)."
  (let ((pathname (if (pathnamep path)
                      path
                      (sb-ext:parse-native-namestring
                       (let ((home (home-directory)))
                         (cond ((and tilde (string= path "~")) home)
                               ((and tilde (> (length path) 1) (string= "~/" path :end2 2))
                                (concatenate 'string home (subseq path 2)))
                               (t path)))))))
    (without-dot-components
     (if (uiop:absolute-pathname-p pathname)
         pathname
         (merge-pathnames pathname (directory-pathname (or directory (default-directory))))))))

(defvar *default-directory-found* nil
  "The directory DEFAULT-DIRECTORY last found, as (SETTING TIME . PATHNAME):
for SETTING, the value *DEFAULT-DIRECTORY* had, at the internal real TIME.")

(defun default-directory ()
  "The directory *DEFAULT-DIRECTORY* names, as an absolute pathname; the
directory Tuatara was started in when it is NIL or names no directory. One
found is used again for a second, while the setting stays, so that drawing
every frame asks the file system nothing."
  (let ((found *default-directory-found*) (now (get-internal-real-time)))
    (if (and found (equal (first found) *default-directory*)
             (< (- now (second found)) internal-time-units-per-second))
        (cddr found)
        (let ((named (and *default-directory*
                          (tuatara.runtime:ignoring-errors-reported ("finding the directory ~a" *default-directory*)
                            (directory-pathname (expand-path *default-directory* :directory (uiop:getcwd)))))))
          (if (and named (eq :directory (path-kind named)))
              (progn (setf *default-directory-found* (list* *default-directory* now named))
                     named)
              (uiop:getcwd))))))

(defun fail (operation path &optional (errno (sb-alien:get-errno)))
  "Signal a FILE-SYSTEM-ERROR for OPERATION on PATH with ERRNO's reason."
  (error 'file-system-error :operation operation :path path :reason (errno-reason errno) :errno errno))

(defmacro with-octets-sap ((sap octets) &body body)
  "Run BODY with SAP pointing at the pinned vector OCTETS."
  `(sb-sys:with-pinned-objects (,octets)
     (let ((,sap (sb-sys:vector-sap ,octets)))
       ,@body)))

;;; Memory outside Lisp

(declaim (inline null-pointer null-pointer-p))
(defun null-pointer ()
  "The null foreign pointer."
  (sb-sys:int-sap 0))

(defun null-pointer-p (pointer)
  "True when POINTER, a foreign pointer, is null."
  (zerop (sb-sys:sap-int pointer)))

(defmacro with-foreign-memory ((sap size &key zero) &body body)
  "Run BODY with SAP pointing at SIZE octets of foreign memory of its own, all
zero to begin with when ZERO is true, freed when BODY is left, however it is
left."
  (let ((alien (gensym "ALIEN")) (octets (gensym "SIZE")))
    `(let* ((,octets ,size)
            (,alien (sb-alien:make-alien (sb-alien:unsigned 8) ,octets)))
       (unwind-protect
            (let ((,sap (sb-alien:alien-sap ,alien)))
              ,@(when zero
                  `((%memset ,sap 0 ,octets)))
              ,@body)
         (sb-alien:free-alien ,alien)))))

(defmacro with-c-string ((sap string &key (start 0) end length) &body body)
  "Run BODY with SAP pointing at STRING from START below END as C is given
text: UTF-8, a raw byte's character as the octet it stands for (as names are
given to the system), a null (NUL) after it; LENGTH, when given a name, is
bound to the octets before the NUL. They are written on the stack, or for a
long string into octets kept for reuse (TUATARA.TEXT:WITH-OCTET-BLOCK), so no
garbage is made for them; BODY must not keep SAP."
  (let ((s (gensym "STRING")) (from (gensym "START")) (to (gensym "END")) (room (gensym "ROOM"))
        (block (gensym "BLOCK")) (used (gensym "LENGTH")) (call (gensym "CALL")))
    `(let* ((,s ,string) (,from ,start) (,to (or ,end (length ,s)))
            ;; the most octets the characters can take, which PUT-STRING-OCTETS asks room for
            (,room (1+ (* 4 (- ,to ,from)))))
       (flet ((,call (,block)
                (multiple-value-bind (,block ,used) (tuatara.text:put-string-octets ,s ,block 0 :start ,from :end ,to)
                  (setf (aref ,block ,used) 0)
                  (with-octets-sap (,sap ,block)
                    (let (,@(when length `((,length ,used))))
                      ,@body)))))
         (declare (dynamic-extent #',call))
         (if (<= ,room 4096)
             (let ((,block (make-array ,room :element-type '(unsigned-byte 8))))
               (declare (dynamic-extent ,block))
               (,call ,block))
             (tuatara.text:with-octet-block (,block ,room) (,call ,block)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun c-type (type)
    "The alien type DEFINE-C-FUNCTION takes TYPE for: :pointer, :int, :uint,
:short, :uint8, :uint32, :long, :ulong, :size, :double, :float, :void, :boolean (a
truth value as an int, GLib's gboolean), :bool (a truth value of one octet,
C's bool) and :string (text, in UTF-8); any other TYPE is taken as an alien
type itself."
    (case type
      (:pointer 'sb-sys:system-area-pointer)
      (:int 'sb-alien:int)
      (:uint 'sb-alien:unsigned-int)
      (:short 'sb-alien:short)
      (:uint8 '(sb-alien:unsigned 8))
      (:uint32 '(sb-alien:unsigned 32))
      (:long 'sb-alien:long)
      ((:ulong :size) 'sb-alien:unsigned-long)
      (:double 'sb-alien:double)
      (:float 'sb-alien:single-float)
      (:void 'sb-alien:void)
      (:boolean '(sb-alien:boolean 32))
      (:bool '(sb-alien:boolean 8))
      (:string '(sb-alien:c-string :external-format :utf-8))
      (t type))))

(defmacro define-c-function ((c-name name) result &body arguments)
  "Define NAME, a function calling the C function C-NAME, which returns
RESULT: ARGUMENTS, after an optional docstring, are each (ARGUMENT TYPE), the
types C-TYPE knows. Declared inline before it is defined, a call of NAME
costs what the bare call does and makes no garbage, as for the system calls
the editor makes on every key. The library holding C-NAME is not loaded by
NAME: its callers load it first (FOREIGN-LIBRARY-P, as LOAD-GTK-LIBRARIES and
LOAD-NCURSES do). A :string argument may be any string, given as
WITH-C-STRING gives it; a :string result is read as SAP-STRING reads it, NIL
for a null pointer."
  (let* ((names (mapcar #'first (remove-if #'stringp arguments)))
         (documentation (list (if (stringp (first arguments))
                                  (pop arguments)
                                  (format nil "Call the C function ~a~@[ with ~{~(~a~)~^, ~}~] and return what it ~
                                               returns." c-name names))))
         (saps (loop for (argument type) in arguments
                     collect (if (eq type :string) (gensym (symbol-name argument)) argument)))
         (call `(sb-alien:alien-funcall
                 (sb-alien:extern-alien ,c-name (function ,(c-type (if (eq result :string) :pointer result))
                                                          ,@(loop for (nil type) in arguments
                                                                  collect (c-type (if (eq type
                                                                                          :string) :pointer type)))))
                 ,@saps))
         (body (if (eq result :string)
                   (let ((pointer (gensym "POINTER")))
                     `(let ((,pointer ,call))
                        (unless (null-pointer-p ,pointer) (sap-string ,pointer))))
                   call)))
    ;; each string written for C around the call, the result read before they go
    (loop for (argument type) in (reverse arguments)
          for sap in (reverse saps)
          when (eq type :string)
            do (setf body `(with-c-string (,sap ,argument) ,body)))
    `(defun ,name ,names
       ,@documentation
       ,body)))

;;; The system's C functions the editor calls itself

(declaim (inline %accept4 %bind %closedir %connect %copy-file-range %execv %execve %fchmodat %fcntl %fdopendir
                 %fgetxattr %flistxattr %free %fsetxattr %fstatfs %fstatvfs %futimens %getgroups %getsockopt
                 %inotify-add-watch %inotify-init1 %inotify-rm-watch %ioctl %ioctl-with-int %lgetxattr %linkat
                 %linkat-named %listen %memfd-create %memset %mkdirat %openat %pidfd-open %pidfd-send-signal %pipe2
                 %poll %posix-fadvise %pread %pwrite %read %readdir %readlinkat %realpath %recvmsg %renameat2
                 %sendmsg %shutdown %socket %socketpair %statvfs %statx %strlen %symlinkat %sync-file-range %unlinkat
                 %utimensat %write))

(define-c-function ("memset" %memset) :pointer (sap :pointer) (octet :int) (count :size))
(define-c-function ("strlen" %strlen) :size (sap :pointer))
(define-c-function ("free" %free) :void (sap :pointer))
(define-c-function ("realpath" %realpath) :pointer (path :pointer) (resolved :pointer))
(define-c-function ("statx" %statx) :int (dirfd :int) (path :pointer) (flags :int) (mask :uint) (buffer :pointer))
(define-c-function ("openat" %openat) :int (dirfd :int) (path :pointer) (flags :int) (mode :int))
(define-c-function ("fdopendir" %fdopendir) :pointer (fd :int))
(define-c-function ("readdir" %readdir) :pointer (directory :pointer))
(define-c-function ("closedir" %closedir) :int (directory :pointer))
(define-c-function ("readlinkat" %readlinkat) :long (dirfd :int) (path :pointer) (buffer :pointer) (size :size))
(define-c-function ("mkdirat" %mkdirat) :int (dirfd :int) (path :pointer) (mode :uint))
(define-c-function ("linkat" %linkat) :int (from-dirfd :int) (from :pointer) (to-dirfd :int) (to :pointer) (flags :int))
(define-c-function ("linkat" %linkat-named) :int
  (from-dirfd :int) (from :string) (to-dirfd :int) (to :string) (flags :int))
(define-c-function ("symlinkat" %symlinkat) :int (target :pointer) (dirfd :int) (path :pointer))
(define-c-function ("unlinkat" %unlinkat) :int (dirfd :int) (path :pointer) (flags :int))
(define-c-function ("renameat2" %renameat2) :int
  (from-dirfd :int) (from :pointer) (to-dirfd :int) (to :pointer) (flags :uint))
(define-c-function ("fchmodat" %fchmodat) :int (dirfd :int) (path :pointer) (mode :uint) (flags :int))
(define-c-function ("utimensat" %utimensat) :int (dirfd :int) (path :pointer) (times :pointer) (flags :int))
(define-c-function ("futimens" %futimens) :int (fd :int) (times :pointer))
(define-c-function ("lgetxattr" %lgetxattr) :long (path :pointer) (name :pointer) (value :pointer) (size :size))
(define-c-function ("flistxattr" %flistxattr) :long (fd :int) (names :pointer) (size :size))
(define-c-function ("fgetxattr" %fgetxattr) :long (fd :int) (name :pointer) (value :pointer) (size :size))
(define-c-function ("fsetxattr" %fsetxattr) :int (fd :int) (name :pointer) (value :pointer) (size :size) (flags :int))
(define-c-function ("fstatfs" %fstatfs) :int (fd :int) (buffer :pointer))
(define-c-function ("fstatvfs" %fstatvfs) :int (fd :int) (buffer :pointer))
(define-c-function ("statvfs" %statvfs) :int (path :string) (buffer :pointer))
(define-c-function ("fcntl" %fcntl) :int (fd :int) (command :int) (argument :long))
(define-c-function ("ioctl" %ioctl) :int (fd :int) (request :ulong) (argument :pointer))
(define-c-function ("ioctl" %ioctl-with-int) :int (fd :int) (request :ulong) (argument :int))
(define-c-function ("copy_file_range" %copy-file-range) :long
  (in :int) (in-offset :pointer) (out :int) (out-offset :pointer) (length :size) (flags :uint))
(define-c-function ("sync_file_range" %sync-file-range) :int (fd :int) (offset :long) (length :long) (flags :uint))
(define-c-function ("posix_fadvise" %posix-fadvise) :int (fd :int) (offset :long) (length :long) (advice :int))
(define-c-function ("read" %read) :long (fd :int) (sap :pointer) (count :size))
(define-c-function ("pread" %pread) :long (fd :int) (sap :pointer) (count :size) (offset :long))
(define-c-function ("write" %write) :long (fd :int) (sap :pointer) (count :size))
(define-c-function ("pwrite" %pwrite) :long (fd :int) (sap :pointer) (count :size) (offset :long))
(define-c-function ("poll" %poll) :int (fds :pointer) (count :ulong) (milliseconds :int))
(define-c-function ("pipe2" %pipe2) :int (fds :pointer) (flags :int))
(define-c-function ("memfd_create" %memfd-create) :int (name :string) (flags :uint))
(define-c-function ("inotify_init1" %inotify-init1) :int (flags :int))
(define-c-function ("inotify_add_watch" %inotify-add-watch) :int (fd :int) (path :string) (mask :uint))
(define-c-function ("inotify_rm_watch" %inotify-rm-watch) :int (fd :int) (wd :int))
(define-c-function ("socket" %socket) :int (domain :int) (type :int) (protocol :int))
(define-c-function ("socketpair" %socketpair) :int (domain :int) (type :int) (protocol :int) (fds :pointer))
(define-c-function ("bind" %bind) :int (fd :int) (address :pointer) (length :int))
(define-c-function ("listen" %listen) :int (fd :int) (backlog :int))
(define-c-function ("accept4" %accept4) :int (fd :int) (address :pointer) (length :pointer) (flags :int))
(define-c-function ("connect" %connect) :int (fd :int) (address :pointer) (length :int))
(define-c-function ("getsockopt" %getsockopt) :int
  (fd :int) (level :int) (option :int) (value :pointer) (length :pointer))
(define-c-function ("shutdown" %shutdown) :int (fd :int) (how :int))
(define-c-function ("sendmsg" %sendmsg) :long (fd :int) (message :pointer) (flags :int))
(define-c-function ("recvmsg" %recvmsg) :long (fd :int) (message :pointer) (flags :int))
(define-c-function ("getgroups" %getgroups) :int (count :int) (groups :pointer))
(define-c-function ("execv" %execv) :int (path :string) (argv :pointer))
(define-c-function ("execve" %execve) :int (path :string) (argv :pointer) (environment :pointer))
(define-c-function ("syscall" %pidfd-open) :long (number :long) (pid :int) (flags :uint))
(define-c-function ("syscall" %pidfd-send-signal) :long
  (number :long) (pidfd :int) (signal :int) (info :pointer) (flags :uint))

(define-c-function ("strlen" %c-string-length) :size
  "The octets STRING takes as SB-POSIX and SB-ALIEN give a name to the system."
  (string sb-alien:c-string))

;; a name read with a raw byte in it reaches the system as it was read, or the build stops here
(unless (= 1 (%c-string-length (string (code-char #xDCFF))))
  (error "A name that is not UTF-8 would not reach the system as it was read: ~
          KEEP-RAW-BYTES-IN-NAMES no longer wraps what gives names to the system."))

;;; Memory outside Lisp, read

(defun sap-octets (sap &optional length)
  "The LENGTH octets at the foreign pointer SAP, copied into a new octet
vector; without LENGTH, the octets up to the first zero octet, the end of a
C string, that zero left out. Complexity: O(LENGTH), one block copy."
  (let* ((length (or length (%strlen sap)))
         (octets (make-array length :element-type '(unsigned-byte 8))))
    (with-octets-sap (to octets)
      (sb-kernel:system-area-ub8-copy sap 0 to 0 length))
    octets))

(defun sap-string (sap &optional length)
  "The text of the LENGTH octets at the foreign pointer SAP, or of the C
string there without LENGTH, read as the editor reads a file's text: UTF-8,
an octet that is not UTF-8 kept as a raw byte."
  (tuatara.text:octets-string (sap-octets sap length)))

;;; statx: what a file is, exactly

(define-record (stamp (:constructor make-stamp (dev ino size mtime nlink mode uid gid &optional (ctime 0)))
                      (:copier nil))
  "What the system reports about a file: DEV and INO name it; SIZE and MTIME (in
nanoseconds) change when its content does; NLINK, the names it has; MODE,
with its kind; user id (UID) and group id (GID), its owners; CTIME (in
nanoseconds), when it, its owners or its permissions last changed, which no
program can set back."
  (dev 0 :type integer :read-only t :documentation "The device of the file system holding the file.")
  (ino 0 :type integer :read-only t :documentation "The file's inode number on that device.")
  (size 0 :type integer :read-only t :documentation "The file's size in octets.")
  (mtime 0 :type integer :read-only t :documentation "When the file's content last changed, in nanoseconds.")
  (nlink 1 :type integer :read-only t :documentation "How many names (hard links) the file has.")
  (mode 0 :type integer :read-only t :documentation "The file's mode: its kind and permissions.")
  (uid 0 :type integer :read-only t :documentation "The file's owner.")
  (gid 0 :type integer :read-only t :documentation "The file's group.")
  (ctime 0 :type integer :read-only t :documentation "When the file, its owners or its permissions last
changed, in nanoseconds."))

(setf (documentation 'make-stamp 'function) "Return a stamp with the given fields.")

(defun stamp-kind (stamp)
  "The kind of file STAMP describes: :REGULAR, :DIRECTORY, :FIFO, :CHARACTER-DEVICE,
:BLOCK-DEVICE, :SOCKET, :SYMLINK or :OTHER."
  (case (logand (stamp-mode stamp) #o170000)
    (#o100000 :regular) (#o040000 :directory) (#o010000 :fifo) (#o020000 :character-device)
    (#o060000 :block-device) (#o140000 :socket) (#o120000 :symlink) (t :other)))

(defun same-content-p (a b)
  "True when stamps A and B name the same file with the same content as far as
the system tells: device, inode, size and modification time."
  (and a b (= (stamp-dev a) (stamp-dev b)) (= (stamp-ino a) (stamp-ino b))
       (= (stamp-size a) (stamp-size b)) (= (stamp-mtime a) (stamp-mtime b))))

(defconstant +at-fdcwd+ -100 "The directory a relative name is taken from: the current one.")
(defconstant +o-directory+ #o200000 "open's O_DIRECTORY: only a directory is opened.")
(defconstant +o-nofollow+ #o400000 "open's O_NOFOLLOW: a symbolic link is not followed, and not opened.")
(defconstant +o-cloexec+ #o2000000 "open's O_CLOEXEC: the descriptor is not given to programs started.")
(defconstant +o-path+ #o10000000
  "Linux's O_PATH, which sb-posix does not name: a descriptor that only names a
file, so that it can be examined, nothing in it opened; a device or a named
pipe named so is not woken.")

(defun statx-buffer-stamp (buffer)
  "The STAMP in BUFFER, the octets statx filled."
  (make-stamp (+ (ash (octets-integer buffer 136 4) 32) (octets-integer buffer 140 4)) ; dev major, minor
              (octets-integer buffer 32 8) (octets-integer buffer 40 8)
              (+ (* (octets-signed (octets-integer buffer 112 8) 64) 1000000000)
                 (octets-integer buffer 120 4))
              (octets-integer buffer 16 4) (octets-integer buffer 28 2)
              (octets-integer buffer 20 4) (octets-integer buffer 24 4)
              (+ (* (octets-signed (octets-integer buffer 96 8) 64) 1000000000)
                 (octets-integer buffer 104 4))))

(defun statx-fields (dirfd name flags &optional (mask #x7ff))
  "The 256 octets statx fills for NAME (a string, or octets) relative to the
directory open as DIRFD. FLAGS is #x100 for not following a symbolic link,
#x1000 for DIRFD itself and an empty NAME. MASK is the fields asked for: those
of a STAMP as standard, #xfff for the birth time too. Returns NIL and the
errno when it cannot be examined."
  (let ((held (terminated-octets (if (stringp name) (tuatara.text:string-octets name) name)))
        (buffer (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (path held)
      (with-octets-sap (sap buffer)
        (if (zerop (%statx dirfd path flags mask sap))
            buffer
            (values nil (sb-alien:get-errno)))))))

(defun statx-stamp (dirfd path flags)
  "The STAMP statx reports for PATH relative to DIRFD with FLAGS, or NIL with the errno (STATX-FIELDS)."
  (multiple-value-bind (buffer errno) (statx-fields dirfd path flags)
    (if buffer (statx-buffer-stamp buffer) (values nil errno))))

(defun path-times (path)
  "When the file at PATH (a link followed) was last modified, changed (its
contents, name, owner or permissions), accessed and born, as a plist of
:MODIFIED :CHANGED :ACCESSED :BORN, each in nanoseconds since 1970, :BORN NIL
where the file system keeps no birth time; NIL when it cannot be examined.
Asking changes none of them."
  (let ((buffer (statx-fields +at-fdcwd+ (native path) 0 #xfff)))
    (when buffer
      (list :modified (statx-time buffer 112) :changed (statx-time buffer 96) :accessed (statx-time buffer 64)
            :born (statx-birth-time buffer)))))

(defun statx-time (buffer offset)
  "The time in BUFFER, the octets statx filled, at OFFSET, in nanoseconds since 1970."
  (+ (* (octets-signed (octets-integer buffer offset 8) 64) 1000000000)
     (octets-integer buffer (+ offset 8) 4)))

(defun statx-birth-time (buffer)
  "The birth time in BUFFER, the octets statx filled, in nanoseconds since
1970; NIL where the file system keeps none."
  ;; STATX_BTIME in the mask the system filled
  (and (logtest (octets-integer buffer 0 4) #x800) (statx-time buffer 80)))

(defun fd-birth-time (fd)
  "When the file open as file descriptor (FD) was made, in nanoseconds since
1970; NIL where the file system keeps no birth time, or it cannot be examined."
  (let ((buffer (statx-fields fd "" #x1000 #x800))) ; AT_EMPTY_PATH, STATX_BTIME
    (and buffer (statx-birth-time buffer))))

(defun statx-octets (dirfd octets flags)
  "The STAMP statx reports for the entry whose name is OCTETS in the directory
open as DIRFD, with FLAGS (#x100 not following a symbolic link), and as more
values the octets it takes on the disk and the device numbers it stands for,
(MAJOR . MINOR), which mean something for a device only; NIL when it cannot be
examined."
  (let ((buffer (statx-fields dirfd octets flags)))
    (when buffer
      (values (statx-buffer-stamp buffer) (* 512 (octets-integer buffer 48 8))
              (cons (octets-integer buffer 128 4) (octets-integer buffer 132 4))))))

(defun openat-octets (dirfd octets flags &optional (mode 0))
  "A descriptor for the entry whose name is OCTETS in the directory open as
DIRFD, opened with FLAGS (made with MODE when they make it); NIL and the errno
when it cannot be opened."
  (let ((name (terminated-octets octets)))
    (with-octets-sap (path name)
      (let ((fd (%openat dirfd path flags mode)))
        (if (minusp fd) (values nil (sb-alien:get-errno)) fd)))))

(defun map-directory-fd (function fd path)
  "Call FUNCTION with the name, as octets, of each entry of the directory open
as file descriptor (FD), . and .. among them, in the order the system gives
them, read from its start; FD stays open. A FILE-SYSTEM-ERROR, PATH named,
when it cannot be read."
  (let* ((copy (sb-posix:dup fd))
         (dir (%fdopendir copy)))
    (when (null-pointer-p dir)
      (let ((errno (sb-alien:get-errno)))
        (sb-posix:close copy)
        (fail "read" path errno)))
    (unwind-protect
         (progn
           (sb-posix:lseek copy 0 sb-posix:seek-set)
           (loop for entry = (%readdir dir)
                 until (null-pointer-p entry)
                 ;; the name stands at octet 19 of the record, after its inode, offset, length and type
                 do (funcall function (sap-octets (sb-sys:sap+ entry 19)))))
      ;; closes COPY too
      (%closedir dir))))

(defun link-octets (dirfd octets)
  "What the symbolic link whose name is OCTETS in the directory open as DIRFD
holds, as octets; NIL when it cannot be read."
  (let ((name (terminated-octets octets))
        (out (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (path name)
      (with-octets-sap (sap out)
        (let ((n (%readlinkat dirfd path sap 4096)))
          (and (>= n 0) (subseq out 0 n)))))))

(defun entry-label (dirfd octets)
  "What ls -l marks after the permissions of the entry whose name is OCTETS in
the directory open as DIRFD, a symbolic link not followed: #\\+ when it has an
access control list beyond its permissions, #\\. when it has a security
context (SELinux) and no such list, NIL for neither."
  (let ((path (concatenate '(vector (unsigned-byte 8))
                           (sb-ext:string-to-octets (format nil "/proc/self/fd/~d/" dirfd) :external-format :latin-1)
                           octets #(0))))
    (flet ((has-p (attribute)
             (let ((name (sb-ext:string-to-octets attribute :external-format :latin-1 :null-terminate t)))
               (with-octets-sap (sap path)
                 (with-octets-sap (key name)
                   (>= (%lgetxattr sap key (sb-sys:int-sap 0) 0)
                       0))))))
      (cond ((or (has-p "system.posix_acl_access") (has-p "system.posix_acl_default")) #\+)
            ((has-p "security.selinux") #\.)))))

(defun path-stamp (path &key (follow t) (failure :signal))
  "The STAMP of the file at PATH, following a symlink unless FOLLOW is NIL (a
slash at its end, which leads through a link, then disregarded); NIL when
there is no such file. Other failures, as no leave to look there or an error
reading the disk, signal a FILE-SYSTEM-ERROR; with FAILURE NIL they give NIL
as well, for a caller to whom a file it cannot examine is as good as none,
and are noted in the debug log while debug mode (\\[debug-mode]) is on."
  (if failure
      (examined-stamp path follow)
      (values (tuatara.runtime:ignoring-errors-reported ("examining ~a" path)
                (examined-stamp path follow)))))

(defun examined-stamp (path follow)
  "The STAMP of the file at PATH, as PATH-STAMP gives it with FOLLOW: NIL when
there is no such file, a FILE-SYSTEM-ERROR for any other failure."
  (multiple-value-bind (stamp errno)
      (statx-stamp +at-fdcwd+
                   (let ((name (native path)))
                     (if (or follow (<= (length name) 1)) name (string-right-trim "/" name)))
                   (if follow 0 #x100))
    (cond (stamp)
          ((member errno (list sb-posix:enoent sb-posix:enotdir)) nil)
          (t (fail "examine" path errno)))))

(defun path-kind (path &key (follow t))
  "What is at PATH, as STAMP-KIND names it (:REGULAR, :DIRECTORY and the
rest), a symbolic link followed unless FOLLOW is NIL; NIL when nothing is
there or it cannot be examined."
  (let ((stamp (path-stamp path :follow follow :failure nil)))
    (and stamp (stamp-kind stamp))))

(defun path-size (path &optional (default 0))
  "The size in octets of the file at PATH, or DEFAULT when there is no such
file or it cannot be examined."
  (let ((stamp (path-stamp path :failure nil)))
    (if stamp (stamp-size stamp) default)))

(defun fd-stamp (fd path)
  "The STAMP of the file open as file descriptor (FD) (PATH for messages)."
  (multiple-value-bind (stamp errno) (statx-stamp fd "" #x1000) ; AT_EMPTY_PATH
    (or stamp (fail "examine" path errno))))

(defun fd-kind (fd)
  "What the file open as file descriptor (FD) is, as STAMP-KIND names it
\(:REGULAR, :DIRECTORY and the rest), and its STAMP as a second value; NIL
when it cannot be examined."
  (let ((stamp (statx-stamp fd "" #x1000))) ; AT_EMPTY_PATH
    (and stamp (values (stamp-kind stamp) stamp))))

;;; statfs and statvfs: which file system, and how much room

(defconstant +btrfs-magic+ #x9123683e "The magic number of btrfs.")
(defconstant +xfs-magic+ #x58465342 "The magic number of XFS.")
(defconstant +bcachefs-magic+ #xca451a4e "The magic number of bcachefs.")
(defconstant +zfs-magic+ #x2fc12fc1 "The magic number of ZFS.")
(defconstant +f2fs-magic+ #xf2f52010 "The magic number of F2FS.")

(defparameter *pseudo-file-systems*
  '(#x9fa0 #x62656572 #x64626720 #x74726163 #x73636673 #x27e0eb #x63677270 #x62656570
    #xde5e81e4 #x1cd1 #xcafe4a11 #x6165676c #x42494e4d #x19800202 #x6e736673)
  "Magic numbers of file systems whose files the kernel makes up as they are read:
proc, sysfs, debugfs, tracefs, securityfs, cgroup, cgroup2, configfs, efivarfs,
devpts, bpf, pstore, binfmt_misc, mqueue, nsfs. Their sizes are not to be
trusted, they cannot be mapped, and they are written in place.")

(defparameter *remote-file-systems*
  '(#x6969 #xfe534d42 #xff534d42 #x517b #x01021997 #x00c36400 #x5346414f #x73757245
    #x65735546 #x01161970 #x7461636f #x47504653 #x0bd00bd0 #x19830326 #x6b414653)
  "Magic numbers of file systems where another machine or a user-space process
serves the files: NFS, SMB2, CIFS, SMB, 9p, Ceph, AFS, Coda, FUSE (sshfs, rclone
and the rest), GFS2, OCFS2, GPFS, Lustre, fhgfs, kAFS. A mapping of their files
is only as dependable as the server, so a large one is copied to the cache
directory in the background, and its snapshots never go beside it.")

(defparameter *block-sharing-file-systems*
  (list +btrfs-magic+ +xfs-magic+ +bcachefs-magic+ +zfs-magic+)
  "Magic numbers of file systems that can share blocks between files: btrfs,
XFS, bcachefs and ZFS. Where one really does (SHARES-BLOCKS-P), a copy can
take no time and no space, so scratch files go beside the file they serve,
for a save to share their blocks.")

(defun fd-file-system (fd)
  "The magic number of the file system holding the file open as file
descriptor (FD), or NIL."
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap buffer)
      (when (zerop (%fstatfs fd sap))
        (octets-integer buffer 0 8)))))

(defun path-file-system (path &key (follow t))
  "The magic number of the file system holding PATH, a symbolic link followed
unless FOLLOW is NIL, or NIL when it cannot be examined. Only a name for it is
opened (O_PATH): a named pipe or a device there is not woken."
  (let ((fd (tuatara.runtime:ignoring-errors-reported ("examining the file system of ~a" path)
              (handler-case (sb-posix:open (native path) (logior +o-path+ +o-cloexec+ (if follow 0 +o-nofollow+)))
                (sb-posix:syscall-error () nil)))))
    (and fd (unwind-protect (fd-file-system fd) (sb-posix:close fd)))))

(defun file-system-class (magic)
  "How files on the file system with MAGIC are handled: :PSEUDO, :REMOTE or :LOCAL."
  (cond ((null magic) :local)
        ((member magic *pseudo-file-systems*) :pseudo)
        ((member magic *remote-file-systems*) :remote)
        (t :local)))

(defun read-only-mount-p (fd)
  "True when the file open as file descriptor (FD) is on a file system mounted
read-only (a CD, a squashfs image, a read-only mount), where its content
cannot change through that mount."
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap buffer)
      (and (zerop (%fstatvfs fd sap))
           (logbitp 0 (octets-integer buffer 72 8))))))            ; f_flag's ST_RDONLY

(defconstant +f-setlease+ 1024 "fcntl's F_SETLEASE.")
(defconstant +f-getlease+ 1025 "fcntl's F_GETLEASE.")

(defun fcntl-int (fd command argument)
  "Fcntl(2) on file descriptor (FD) with COMMAND and an integer ARGUMENT; its
result, -1 on failure."
  (%fcntl fd command argument))

(defvar *lease-break-pipe* nil
  "A wake pipe (MAKE-WAKE-PIPE) woken whenever the system says a program waits
on a lease, so that a thread can wait on it rather than ask again and again;
made with the first lease taken.")

(defun take-read-lease (fd)
  "Ask for a read lease on the file open as file descriptor (FD): then a
program that opens the file for writing, or truncates it, waits until the
lease is given up (or the system's lease-break time runs out), and
*LEASE-BREAK-PIPE* is woken. True when granted; not granted for files
another user owns, files open for writing now, and file systems without
leases."
  (unless *lease-break-pipe*
    (setf *lease-break-pipe* (make-wake-pipe))
    ;; the signal the system sends a lease holder would end the process unhandled
    (sb-sys:enable-interrupt sb-unix:sigio (lambda (signal info context)
                                             (declare (ignore signal info context))
                                             (wake *lease-break-pipe*))))
  (zerop (fcntl-int fd +f-setlease+ 0)))                ; F_RDLCK

(defun lease-breaking-p (fd)
  "True when a program waits to write the file open as file descriptor (FD)
holds a read lease on."
  (= (fcntl-int fd +f-getlease+ 0) 2))                  ; F_UNLCK: the lease is to go

(defun release-lease (fd)
  "Give up the lease on the file open as file descriptor (FD), letting a
waiting writer go on."
  (fcntl-int fd +f-setlease+ 2)
  nil)

(defun statvfs-fields (path &rest offsets)
  "The fields of what statvfs says of the file system holding PATH, each of 8
octets at one of OFFSETS into its structure, as a list; NIL when the system
does not say."
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap buffer)
      (when (zerop (%statvfs (native path) sap))
        (mapcar (lambda (offset) (octets-integer buffer offset 8)) offsets)))))

(defun free-octets (path)
  "Octets an unprivileged user may still write on the file system holding PATH,
or NIL when the system does not say."
  (let ((fields (statvfs-fields path 32 8)))            ; f_bavail, f_frsize
    (and fields (* (first fields) (second fields)))))

(defun free-inodes (path)
  "How many more files an unprivileged user may make on the file system
holding PATH, and as a second value how many it holds in all; NIL when the
system does not say, or the file system keeps no such count (btrfs)."
  (destructuring-bind (&optional free total) (statvfs-fields path 56 40)   ; f_favail, f_files
    (and total (plusp total) (values free total))))

(defun octets-words (n &key binary compact)
  "N octets in words, to one decimal: 950 B, 12.3 MB, 11.5 GB; with BINARY in
units of 1,024, as 950 B, 954.0 MiB, 1.2 TiB. With COMPACT, as narrow as a
column of sizes wants it, in units of 1,024 with one letter for the unit:
260, 4.1K, 24K, 1.0M; below 10 with one decimal, every figure rounded up."
  (if compact
      (if (< n 1024)
          (princ-to-string n)
          (loop for unit across "KMGTPEZY"
                for scale = 1024 then (* scale 1024)
                do (let ((value (/ n scale)))
                     (if (< value 10)
                         (let ((tenths (ceiling (* 10 value))))
                           (return (if (< tenths 100)
                                       (format nil "~d.~d~c" (floor tenths 10) (mod tenths 10) unit)
                                       (format nil "10~c" unit))))
                         (let ((whole (ceiling value)))
                           (when (< whole 1024)
                             (return (format nil "~d~c" whole unit))))))))
      (let ((base (if binary 1024 1000)))
        (loop for (unit . more) on (if binary '("B" "KiB" "MiB" "GiB" "TiB" "PiB") '("B" "kB" "MB" "GB" "TB"))
              for scale = 1 then (* scale base)
              when (or (< n (* scale base)) (null more))
                return (if (= scale 1) (format nil "~d B" n) (format nil "~,1f ~a" (/ n scale) unit))))))

;;; Copies the file system or the kernel makes

(defun clone-fd (source dest)
  "Make the open file DEST share every block of the open file SOURCE (the
FICLONE ioctl): a copy that takes no time and no space until either changes.
True on success; NIL where the file system cannot (most cannot), for the
caller to copy instead."
  (zerop (%ioctl-with-int dest #x40049409 source)))

(defun copy-fd-range (source offset dest length)
  "Copy LENGTH octets of the open file SOURCE from OFFSET to the current offset
of the open file DEST with copy_file_range, which lets the kernel, or a network
file server, copy without passing the octets through the editor, and share
blocks where it can. Returns how many it copied: LENGTH, or fewer when the
kernel cannot copy these files (the caller then copies the rest itself, DEST's
offset being after what was copied). Other failures signal."
  (let ((cell (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)) (done 0))
    (with-octets-sap (sap cell)
      (setf (sb-sys:sap-ref-64 sap 0) offset)
      (loop while (< done length)
            do (let ((copied (%copy-file-range source sap dest (sb-sys:int-sap 0) (- length done) 0)))
                 (cond ((plusp copied) (incf done copied))
                       ((zerop copied) (error 'file-system-error :operation "copy from" :path "a file"
                                                                 :reason
                                                                 "it ended early; it changed while being copied"))
                       ((= (sb-alien:get-errno) sb-posix:eintr))
                       ((member (sb-alien:get-errno)
                                (list sb-posix:exdev sb-posix:einval sb-posix:enosys sb-posix:eopnotsupp
                                      sb-posix:ebadf sb-posix:eperm))
                        (return))
                       (t (fail "copy" "a file"))))))
    done))

(defun write-sap (fd sap length &key (path "a file") at)
  "Write LENGTH octets at SAP to the file open as file descriptor (FD) at its
current offset, or at offset AT when given (pwrite(2), the descriptor's own
offset left alone), all of them, again where the system wrote fewer or was
interrupted."
  (let ((done 0))
    (loop while (< done length)
          do (let ((written (if at
                                (%pwrite fd (sb-sys:sap+ sap done) (- length done) (+ at done))
                                (%write fd (sb-sys:sap+ sap done) (- length done)))))
               (cond ((plusp written) (incf done written))
                     ((= (sb-alien:get-errno) sb-posix:eintr))
                     (t (fail "write" path)))))))

(defun write-octets (fd octets &key (start 0) (end (length octets)) (path "a file") at)
  "Write OCTETS from START below END to the file open as file descriptor (FD)
at its current offset, or, with AT, at that offset with pwrite(2), which
leaves the descriptor's own offset alone. All of them are written, again where
the system wrote fewer or was interrupted. Signals FILE-SYSTEM-ERROR, naming
PATH, when the system refuses, as when a program reading from a pipe has
ended."
  (with-octets-sap (sap octets)
    (write-sap fd (sb-sys:sap+ sap start) (- end start) :path path :at at)))

;;; Terminals' sizes

(defun window-size-ioctl (fd request sap)
  "Ask the terminal open as file descriptor (FD) for its size, or tell it, by
REQUEST (TIOCGWINSZ or TIOCSWINSZ) with the four 16-bit numbers at SAP:
rows, columns, width and height in pixels. True when it worked."
  (zerop (%ioctl fd request sap)))

(defun terminal-window-size (fd)
  "The size of the terminal open as file descriptor (FD), as four values: its
rows and columns, and its width and height in pixels, 0 when it does not
say; NIL when FD is not a terminal."
  (with-foreign-memory (sap 8 :zero t)
    (when (window-size-ioctl fd #x5413 sap)
      (values (sb-sys:sap-ref-16 sap 0)
              (sb-sys:sap-ref-16 sap 2)
              (sb-sys:sap-ref-16 sap 4)
              (sb-sys:sap-ref-16 sap 6)))))

(defun set-terminal-window-size (fd rows columns width height)
  "Tell the terminal open as file descriptor (FD) its size, which the program
on it reads: ROWS and COLUMNS, and WIDTH and HEIGHT in pixels (0 when not
known), each at most 65535."
  (with-foreign-memory (sap 8)
    (setf (sb-sys:sap-ref-16 sap 0) (min 65535 rows) (sb-sys:sap-ref-16 sap 2) (min 65535 columns)
          (sb-sys:sap-ref-16 sap 4) (min 65535 width) (sb-sys:sap-ref-16 sap 6) (min 65535 height))
    (window-size-ioctl fd #x5414 sap)))

;;; Writing a file through a buffer

(define-record (writer (:constructor make-writer (fd)) (:copier nil))
  "Octets written to the file open as file descriptor (FD) a megabyte at a
time: FILL of them held in BUFFER, WRITTEN already in the file."
  (fd -1 :type fixnum :documentation "The open file a WRITER writes to, a descriptor.")
  (buffer (make-array 1048576 :element-type '(unsigned-byte 8)) :type tuatara.text:octets
          :documentation "The octets of a WRITER held until they are written, a vector of a megabyte.")
  (fill 0 :type fixnum :documentation "How many octets of a WRITER's buffer are held.")
  (written 0 :type fixnum :documentation "How many octets of a WRITER are already in the file."))

(setf (documentation 'make-writer 'function) "A writer to the open file FD.")

(defun writer-flush (writer)
  "Write what WRITER holds to its file."
  (when (plusp (writer-fill writer))
    (write-octets (writer-fd writer) (writer-buffer writer) :end (writer-fill writer) :path "a scratch file")
    (incf (writer-written writer) (writer-fill writer))
    (setf (writer-fill writer) 0)))

(defun writer-length (writer)
  "How many octets have gone through WRITER, held or written."
  (+ (writer-written writer) (writer-fill writer)))

(declaim (inline writer-room))
(defun writer-room (writer wanted)
  "Take room for up to WANTED more octets in WRITER's buffer, written out first
when it is full: return where in the buffer they go and how many fit."
  (declare (type fixnum wanted))
  (when (= (writer-fill writer) (length (writer-buffer writer))) (writer-flush writer))
  (let* ((at (writer-fill writer))
         (n (min wanted (- (length (writer-buffer writer)) at))))
    (setf (writer-fill writer) (+ at n))
    (values at n)))

(defun writer-octets (writer octets &optional (start 0) (end (length octets)))
  "Write OCTETS from START below END through WRITER."
  (declare (type tuatara.text:octets octets) (type fixnum start end))
  (loop while (< start end)
        do (multiple-value-bind (at n) (writer-room writer (- end start))
             (replace (writer-buffer writer) octets :start1 at :start2 start :end2 (+ start n))
             (incf start n))))

(defun writer-sap (writer sap start end)
  "Write the memory at SAP from START below END through WRITER."
  (declare (type sb-sys:system-area-pointer sap) (type fixnum start end))
  (loop while (< start end)
        do (multiple-value-bind (at n) (writer-room writer (- end start))
             (with-octets-sap (to (writer-buffer writer))
               (sb-kernel:system-area-ub8-copy sap start to at n))
             (incf start n))))

(defun writer-rope (writer rope start end)
  "Write the text of ROPE from START below END through WRITER."
  (declare (type fixnum start end))
  (loop while (< start end)
        do (multiple-value-bind (at n) (writer-room writer (- end start))
             (tuatara.text:rope-copy-into rope start (+ start n) (writer-buffer writer) at)
             (incf start n))))

(defun writer-spaces (writer count)
  "Write COUNT spaces through WRITER."
  (declare (type fixnum count))
  (loop while (plusp count)
        do (multiple-value-bind (at n) (writer-room writer count)
             (fill (writer-buffer writer) 32 :start at :end (+ at n))
             (decf count n))))

(defun read-fd-into (fd octets &key (start 0) (end (length octets)) (path "a file"))
  "Read what the file or pipe open as file descriptor (FD) has, as much as
fits, into OCTETS from START below END, read again when a signal broke in:
how many octets came, 0 at its end. A FILE-SYSTEM-ERROR, naming PATH, when
it cannot be read."
  (loop (let ((got (with-octets-sap (sap octets)
                     (%read fd (sb-sys:sap+ sap start) (- end start)))))
          (cond ((>= got 0) (return got))
                ((/= (sb-alien:get-errno) sb-posix:eintr) (fail "read" path))))))

(defun read-fd-octets (fd &key (path "a file") (size-hint 65536) most)
  "Every octet the file open as file descriptor (FD) gives until its end, as
an octet vector: for files whose size the system does not know or report
truly (/proc, /sys), and for files read into memory. With MOST, reading
stops once more than MOST octets have come, so that a file that never ends
cannot fill memory: a vector longer than MOST then says it was larger.
SIZE-HINT, the octets expected, sizes the vector read into, one more than
it so that the end is seen without growing it; the vector returned is that
one, cut to what came."
  (let ((buffer (make-array (max 4096 (1+ size-hint)) :element-type '(unsigned-byte 8))) (filled 0))
    (loop
      (when (and most (> filled most)) (return))
      (when (= filled (length buffer))
        (let ((bigger (make-array (* 2 (length buffer)) :element-type '(unsigned-byte 8))))
          (replace bigger buffer)
          (setf buffer bigger)))
      (let ((got (read-fd-into fd buffer :start filled :path path)))
        (if (zerop got) (return) (incf filled got))))
    (if (= filled (length buffer)) buffer (sb-kernel:%shrink-vector buffer filled))))

(define-record (octet-pieces (:constructor make-octet-pieces ()) (:copier nil))
  "Octets gathered a piece at a time (ADD-OCTET-PIECE), joined into one vector
once all have come (JOINED-OCTETS): SIZE octets in all, PIECES each (VECTOR
. N), the first N octets of VECTOR, the latest first."
  (size 0 :type fixnum :documentation "How many octets an OCTET-PIECES holds in all.")
  (pieces '() :type list :documentation "The pieces of an OCTET-PIECES, each (VECTOR . N), the latest first."))

(setf (documentation 'make-octet-pieces 'function) "A new OCTET-PIECES, holding no octets yet.")

(defun add-octet-piece (pieces octets &optional (n (length octets)))
  "Add the first N octets of OCTETS to PIECES (OCTET-PIECES), OCTETS itself
kept, not copied: it is not to be changed after. Returns PIECES."
  (push (cons octets n) (octet-pieces-pieces pieces))
  (incf (octet-pieces-size pieces) n)
  pieces)

(defun joined-octets (pieces)
  "Every octet of PIECES (OCTET-PIECES), in the order they were added, as one
new vector."
  (let* ((size (octet-pieces-size pieces))
         (all (make-array size :element-type '(unsigned-byte 8)))
         (at size))
    (loop for (piece . n) in (octet-pieces-pieces pieces)
          do (decf at n)
             (replace all piece :start1 at :end2 n))
    all))

(defun wipe-octet-pieces (pieces)
  "Fill every vector of PIECES (OCTET-PIECES) with zeros, so that what they
held is left only where it was joined."
  (loop for (piece) in (octet-pieces-pieces pieces) do (fill piece 0))
  pieces)

(defun read-fd-at (fd octets offset &key (end (length octets)) (path "a file"))
  "Read into OCTETS below END the octets of the file open as file descriptor
(FD) from OFFSET, with pread, which leaves the descriptor's own offset
alone; return how many were read, fewer only where the file ends."
  (let ((filled 0))
    (with-octets-sap (sap octets)
      (loop while (< filled end)
            do (let ((got (%pread fd (sb-sys:sap+ sap filled) (- end filled) (+ offset filled))))
                 (cond ((plusp got) (incf filled got))
                       ((zerop got) (return))
                       ((= (sb-alien:get-errno) sb-posix:eintr))
                       (t (fail "read" path))))))
    filled))

(defun read-fd-exactly (fd count offset)
  "COUNT octets of the file open as file descriptor (FD) from OFFSET, read
with pread (READ-FD-AT); NIL when the file ends before them or cannot be
read."
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (and (= count (or (tuatara.runtime:ignoring-errors-reported ("reading ~d octets at ~d" count offset)
                        (read-fd-at fd octets offset))
                      0))
         octets)))

(defun advise-will-need (fd offset length)
  "Tell the system that LENGTH octets of the file open as file descriptor (FD)
from OFFSET will be read soon, so that it starts reading them from the disk
while the octets before are worked on. Changes nothing; a system that takes
no advice is fine."
  (%posix-fadvise fd offset length 3)  ; POSIX_FADV_WILLNEED
  nil)

(defvar *write-out-window* (* 32 1024 1024)
  "Octets a save or a copy hands the system before it has them written out to
the device and waits for the ones before: the most it leaves in memory not
yet on disk, and so how far its progress can be ahead of the device.")

(define-record (write-out (:constructor make-write-out (fd start &key (wait t) &aux (written start) (going start)))
                         (:copier nil))
  "The octets written to the file open as file descriptor (FD) from START on,
sent out to the device a window at a time (WRITE-OUT-STEP): below WRITTEN
on the device and out of memory, below GOING on their way there; CAN-SEND
false on a file system that cannot be asked. With WAIT false, nothing waits
for the device: what has gone out is let go of from memory as it is there,
the rest sent out."
  (fd -1 :type fixnum :read-only t :documentation "The descriptor a WRITE-OUT writes.")
  (wait t :read-only t :documentation "True when a WRITE-OUT waits for the device.")
  (written 0 :type fixnum :documentation "Below this offset a WRITE-OUT's octets are on the device.")
  (going 0 :type fixnum :documentation "Below this offset a WRITE-OUT's octets are on their way out.")
  (can-send t :documentation "False once a WRITE-OUT's file system cannot be asked to write out."))

(defun write-out-step (out done)
  "With the file of OUT written below offset DONE: once a window
\(*WRITE-OUT-WINDOW*) more than went out last is written, send it out to the
device, wait for the window before it to be there, and have the system let
go of that from memory. So a slow device holds a long write back as it goes
and memory does not fill with what waits for it. Returns the offset below
which the octets are on the device, or DONE on a file system that cannot be
asked (its flush at the end then does it all)."
  (let ((fd (write-out-fd out)) (going (write-out-going out)) (written (write-out-written out)))
    (when (and (write-out-can-send out) (>= (- done going) *write-out-window*))
      (if (sync-range fd going (- done going))
          (progn
            (if (write-out-wait out)
                (when (sync-range fd written (- going written) :wait t)
                  (drop-cached fd written (- going written))
                  (setf (write-out-written out) going))
                ;; what is on the device leaves memory now, the rest as it gets there
                (progn (drop-cached fd written (- going written))
                       (setf (write-out-written out) going)))
            (setf (write-out-going out) done))
          (setf (write-out-can-send out) nil)))
    (if (write-out-can-send out) (write-out-written out) done)))

(defun copy-fd-contents (source dest length &key (path "a file") cancel progress (from 0) (wait t))
  "Copy LENGTH octets of the open file SOURCE, its first or those from FROM
on, to the open file DEST at its current offset: by the kernel
\(COPY-FD-RANGE) where it can, else read and written here in pieces, read at
explicit offsets so that a descriptor shared with others is not moved. A
large file is read through once (TUATARA.TEXT:BEGIN-FILE-READ-PASS) and its
copy written out as it goes (WRITE-OUT-STEP), so that the copy leaves in
memory what was there; with WAIT false, without waiting for the device as it
goes (MAKE-WRITE-OUT). CANCEL, a cons, stops it when its car turns true
\(returning NIL); PROGRESS, when given, is called with the octets done so
far. True when done."
  (let ((pass (tuatara.text:begin-file-read-pass source (+ from length) path)))
    (unwind-protect (copy-fd-pieces source dest length path cancel progress from pass wait)
      (tuatara.text:end-file-read-pass pass))))

(defun copy-fd-pieces (source dest length path cancel progress from pass wait)
  "Copy as COPY-FD-CONTENTS does, the stretches read noted in PASS, a
TUATARA.TEXT:FILE-READ-PASS or NIL, and written out as they go when PASS is
one, waiting for the device when WAIT is true."
  (let ((piece (* 64 1024 1024)) (done 0) (buffer nil)
        (out (and pass (make-write-out dest (sb-posix:lseek dest 0 sb-posix:seek-cur) :wait wait))))
    (loop while (< done length)
          do (when (and cancel (car cancel)) (return-from copy-fd-pieces nil))
             (tuatara.text:file-read-pass-note pass (+ from done) (+ from done (min piece (- length done))))
             (let* ((n (min piece (- length done)))
                    (at (+ done (copy-fd-range source (+ from done) dest n))))
               (loop while (< at (+ done n))
                     do (unless buffer
                          (setf buffer (make-array (min n (* 4 1024 1024)) :element-type '(unsigned-byte 8))))
                        (let* ((want (min (length buffer) (- (+ done n) at)))
                               (got (read-fd-at source buffer (+ from at) :end want :path path)))
                          (when (< got want)
                            (error 'file-system-error :operation "copy" :path path
                                                      :reason "it ended early; it changed while being copied"))
                          (write-octets dest buffer :end got :path path)
                          (incf at got)))
               (incf done n)
               (when out (write-out-step out (sb-posix:lseek dest 0 sb-posix:seek-cur)))
               (when progress (funcall progress done))))
    t))

;;; Waiting: poll, wake pipes and inotify

(defun wait-for-fds (fds seconds)
  "Wait until one of the open files FDS (a list) has something to read, or
SECONDS pass (NIL: as long as it takes). Returns those that have, NIL when the
time ran out, or :INTERRUPTED when a signal came first (a terminal resized)."
  (let* ((count (length fds))
         (cells (make-array (* 8 (max 1 count)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap cells)
      ;; struct pollfd: int fd; short events (POLLIN); short revents
      (loop for fd in fds for at from 0 by 8
            do (setf (sb-sys:signed-sap-ref-32 sap at) fd
                     (sb-sys:sap-ref-16 sap (+ at 4)) 1))
      (let ((ready (%poll sap count (if seconds (max 0 (min 2000000000 (ceiling (* seconds 1000)))) -1))))
        (cond ((plusp ready)
               (loop for fd in fds for at from 0 by 8
                     unless (zerop (sb-sys:sap-ref-16 sap (+ at 6))) collect fd))
              ((zerop ready) nil)
              ((= (sb-alien:get-errno) sb-posix:eintr) :interrupted)
              (t (fail "wait on" "open files")))))))

(defun new-pipe (flags)
  "A new pipe made with FLAGS (pipe2: O_CLOEXEC for both its ends closed in
programs started, O_NONBLOCK for neither waiting): its reading and its
writing descriptor, two values."
  (sb-alien:with-alien ((ends (array sb-alien:int 2)))
    (unless (zerop (%pipe2 (sb-alien:alien-sap ends) flags))
      (fail "make" "a pipe"))
    (values (sb-alien:deref ends 0) (sb-alien:deref ends 1))))

(defun make-wake-pipe ()
  "A pipe that wakes a thread waiting on it with WAIT-FOR-FDS from any other
thread (WAKE): a cons of its read end and its write end, neither blocking,
both closed in programs the editor starts."
  (multiple-value-call #'cons (new-pipe (logior sb-posix:o-nonblock +o-cloexec+))))

(defun wake (pipe)
  "Wake the thread waiting on PIPE (MAKE-WAKE-PIPE). Any thread may call it;
a pipe already full is awake enough."
  (let ((octet (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1)))
    (with-octets-sap (sap octet)
      (%write (cdr pipe) sap 1)))
  nil)

(defun drain-wake-pipe (pipe)
  "Empty PIPE (MAKE-WAKE-PIPE) of the wakes written to it."
  (let ((octets (make-array 64 :element-type '(unsigned-byte 8))))
    (with-octets-sap (sap octets)
      (loop while (plusp (%read (car pipe) sap 64)))))
  nil)

(defun close-wake-pipe (pipe)
  "Close both ends of PIPE (MAKE-WAKE-PIPE)."
  (tuatara.runtime:close-fd-quietly (car pipe) "a wake pipe")
  (tuatara.runtime:close-fd-quietly (cdr pipe) "a wake pipe")
  nil)

(defun poll-fd (fd events milliseconds)
  "Wait up to MILLISECONDS (a second at most) for file descriptor (FD) to have
any of EVENTS \(POLLIN 1, POLLOUT 4); the events it has, 0 when none came.
With MILLISECONDS NIL, wait until FD has one of them or is shut
\(SHUT-SOCKET), however long that is."
  (declare (type (or null fixnum) milliseconds))
  (sb-alien:with-alien ((poll (array (sb-alien:unsigned 8) 8)))
    (let ((sap (sb-alien:alien-sap poll)))
      (setf (sb-sys:signed-sap-ref-32 sap 0) fd
            (sb-sys:sap-ref-16 sap 4) events
            (sb-sys:sap-ref-16 sap 6) 0)
      (if (plusp (%poll sap 1 (if milliseconds (max 0 (min 1000 milliseconds)) -1)))
          (sb-sys:sap-ref-16 sap 6)
          0))))

;;; Local sockets at a path: listening and connecting

(defconstant +most-socket-path+ 107
  "The most octets of the path of a local socket, its terminating NUL aside.")

(defun call-with-socket-address (path function)
  "Call FUNCTION with a local socket's address naming PATH (a struct
sockaddr_un in foreign memory) and its length; an error when PATH is longer
than +MOST-SOCKET-PATH+ octets or holds a NUL."
  (let ((octets (tuatara.text:string-octets (native path))))
    (when (or (> (length octets) +most-socket-path+) (find 0 octets))
      (fail "name a local socket" path sb-posix:enametoolong))
    (with-foreign-memory (sap 110 :zero t)
      (setf (sb-sys:sap-ref-16 sap 0) 1)                     ; AF_UNIX
      (dotimes (i (length octets))
        (setf (sb-sys:sap-ref-8 sap (+ 2 i)) (aref octets i)))
      (funcall function sap (+ 2 (length octets) 1)))))

(defun new-local-socket ()
  "A new local stream socket, closed in programs started; its descriptor."
  (let ((fd (%socket 1 (logior 1 #x80000) 0)))  ; AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC
    (when (minusp fd) (fail "make" "a local socket"))
    fd))

(sb-ext:defglobal **umask** nil
  "Your umask as UMASK-BITS read it, or NIL before it is first needed.")

(defun umask-bits ()
  "The permission bits your umask takes away from what is made, as the system
says (the Umask line of /proc/self/status), read once and kept, never set;
#o022 where the system does not say."
  (or **umask**
      (setf **umask**
            (or (let* ((text (proc-text "/proc/self/status"))
                       (at (and text (search (format nil "~%Umask:") text))))
                  (and at (parse-integer text :start (+ at 7) :radix 8 :junk-allowed t)))
                #o022))))

(defun set-path-mode (path mode)
  "Give the entry at PATH the permissions MODE, never what a link there leads
to: a link is left as it is. True when done, NIL when not (a link, or no
entry there)."
  (with-c-string (name (native path))
    (zerop (%fchmodat +at-fdcwd+ name mode #x100))))  ; AT_SYMLINK_NOFOLLOW

(defun listen-at (path &key (backlog 16))
  "A local socket bound at PATH, listening, as a descriptor that ACCEPT-ON
takes connections from, BACKLOG of them waiting at most; PATH must not be
there. The socket file is made for you alone (0600)."
  (let ((fd (new-local-socket)) (done nil))
    (unwind-protect
         (progn
           (call-with-socket-address
            path (lambda (sap length)
                   (unless (zerop (%bind fd sap length))
                     (fail "listen at" path))))
           ;; the socket itself, never what a link put in its place points at
           (unless (set-path-mode path #o600)
             (fail "make for you alone" path))
           (unless (zerop (%listen fd backlog))
             (fail "listen at" path))
           (setf done t)
           fd)
      (unless done (sb-posix:close fd)))))

(defun accept-on (fd)
  "Wait for a connection to the listening socket FD (LISTEN-AT) and return
its descriptor, closed in programs started; NIL when FD is shut (SHUT-SOCKET)
or fails, with errno as a second value. A connection given up on by its
other end before it is taken is waited past."
  (loop
    (let ((new (%accept4 fd (null-pointer) (null-pointer) #x80000)))  ; SOCK_CLOEXEC
      (if (>= new 0)
          (return new)
          (let ((errno (sb-alien:get-errno)))
            ;; interrupted, or a connection gone before it was taken: wait for the next
            (unless (member errno (list sb-posix:eintr sb-posix:econnaborted sb-posix:eagain))
              (return (values nil errno))))))))

(defun connect-to (path)
  "A connection to the local socket listening at PATH, as a descriptor closed
in programs started; NIL when there is none to connect to, with errno as a
second value (ENOENT: no socket there; ECONNREFUSED: no one listens)."
  (let ((fd (new-local-socket)))
    (call-with-socket-address
     path (lambda (sap length)
            (if (zerop (%connect fd sap length))
                fd
                (let ((errno (sb-alien:get-errno)))
                  (sb-posix:close fd)
                  (values nil errno)))))))

(defun peer-credentials (fd)
  "Who is at the other end of the local socket FD, as the system vouches
for it (SO_PEERCRED): three values, its process id (PID), user id (UID)
and group id (GID); NIL when the system cannot say."
  (sb-alien:with-alien ((credentials (array (sb-alien:unsigned 32) 3))
                        (length sb-alien:unsigned-int 12))
    (if (zerop (%getsockopt fd 1 17 (sb-alien:alien-sap credentials)  ; SOL_SOCKET, SO_PEERCRED
                            (sb-alien:alien-sap (sb-alien:addr length))))
        (values (sb-alien:deref credentials 0) (sb-alien:deref credentials 1) (sb-alien:deref credentials 2))
        nil)))

(defun peer-process (fd)
  "A pidfd of the process at the other end of the local socket FD as it
connected (SO_PEERPIDFD), and its process id (PID) as the system gives it
now, two values: the pidfd names that process however long it lives, never
another given its number after it. NIL when the system cannot say, and
then :UNSUPPORTED as a second value when this kernel has no such question
\(before Linux 6.5); the pidfd is the caller's to close."
  (sb-alien:with-alien ((pidfd sb-alien:int -1) (length sb-alien:unsigned-int 4))
    (if (zerop (%getsockopt fd 1 77 (sb-alien:alien-sap (sb-alien:addr pidfd))  ; SOL_SOCKET, SO_PEERPIDFD
                            (sb-alien:alien-sap (sb-alien:addr length))))
        (let ((pid (pidfd-process pidfd)))
          (if pid
              (values pidfd pid)
              (progn (sb-posix:close pidfd) nil)))
        (values nil (and (= (sb-alien:get-errno) 92) :unsupported)))))   ; ENOPROTOOPT

(defun pidfd-process (pidfd)
  "The process id the pidfd PIDFD names while that process runs, as
/proc/self/fdinfo says it; NIL once it has ended or when it cannot be read."
  (let ((text (handler-case
                  (tuatara.text:lenient-string (file-octets (format nil "/proc/self/fdinfo/~d" pidfd) :limit 4096))
                (file-system-error () nil))))
    (when text
      (let ((at (search (format nil "~%Pid:") text)))
        (when at
          (let ((pid (parse-integer text :start (+ at 5) :junk-allowed t)))
            (and pid (plusp pid) pid)))))))

(defun mount-namespace (pid)
  "The mount namespace the process PID runs in, as (DEVICE . INODE) of its
/proc/PID/ns/mnt: two processes see the same files at the same paths when
these are EQUAL. NIL when it cannot be read, as the system keeps that of a
program such as sshd to itself."
  (let ((stamp (handler-case (path-stamp (format nil "/proc/~d/ns/mnt" pid))
                 (file-system-error () nil))))
    (and stamp (cons (stamp-dev stamp) (stamp-ino stamp)))))

;;; Local sockets, and descriptors passed over them

(defun socket-pair ()
  "Two ends of a new local socket, as two descriptors, neither left open in
programs started (a program is given its end as its input); the first does
not wait when it cannot be read or written at once."
  (sb-alien:with-alien ((fds (array sb-alien:int 2)))
    (unless (zerop (%socketpair 1 (logior 1 #x80000) 0 (sb-alien:alien-sap fds)))  ; AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC
      (fail "make" "a local socket"))
    (let ((mine (sb-alien:deref fds 0)))
      (fcntl-int mine 4 #x800)                                     ; F_SETFL O_NONBLOCK
      (values mine (sb-alien:deref fds 1)))))

(defun shut-socket (fd)
  "Shut the socket open as file descriptor (FD) both ways, so that a thread
waiting to read or write it stops waiting and the other end sees it end; the
descriptor stays open until closed."
  (%shutdown fd 2)  ; SHUT_RDWR
  nil)

(defconstant +most-descriptors+ 4
  "The most descriptors passed with one piece of what a local socket sends.")

(defun send-with-descriptors (fd sap length &optional descriptors)
  "Send up to LENGTH octets at SAP over the local socket open as file
descriptor (FD), without waiting. DESCRIPTORS (a list of at most
+MOST-DESCRIPTORS+ open descriptors) are passed with them to the other end,
where RECEIVE-WITH-DESCRIPTORS takes them. Returns how many octets were sent,
or -1 when none could be, with the reason, errno, as a second value (EAGAIN:
the socket is full for now). A socket whose other end has gone gives -1, never
a signal."
  (let ((count (length descriptors)))
    (assert (<= count +most-descriptors+))
    (sb-alien:with-alien ((part (array (sb-alien:unsigned 64) 2))
                          (control (array (sb-alien:unsigned 8) 32))
                          (message (array (sb-alien:unsigned 8) 56)))
      (let ((p (sb-alien:alien-sap part)) (c (sb-alien:alien-sap control)) (m (sb-alien:alien-sap message))
            (control-length (if descriptors (+ 16 (* 8 (ceiling (* 4 count) 8))) 0)))
        ;; struct iovec, a struct cmsghdr of SCM_RIGHTS, struct msghdr (x86-64 and arm64 lay them out alike)
        (setf (sb-sys:sap-ref-64 p 0) (sb-sys:sap-int sap) (sb-sys:sap-ref-64 p 8) length)
        (dotimes (i 32) (setf (sb-sys:sap-ref-8 c i) 0))
        (setf (sb-sys:sap-ref-64 c 0) (+ 16 (* 4 count)) (sb-sys:sap-ref-32 c 8) 1 (sb-sys:sap-ref-32 c 12) 1)
        (loop for descriptor in descriptors for at from 16 by 4
              do (setf (sb-sys:signed-sap-ref-32 c at) descriptor))
        (dotimes (i 56) (setf (sb-sys:sap-ref-8 m i) 0))
        (setf (sb-sys:sap-ref-64 m 16) (sb-sys:sap-int p) (sb-sys:sap-ref-64 m 24) 1
              (sb-sys:sap-ref-64 m 32) (if descriptors (sb-sys:sap-int c) 0) (sb-sys:sap-ref-64 m 40) control-length)
        (let ((sent (%sendmsg fd m (logior #x4000 #x40))))  ; MSG_NOSIGNAL, MSG_DONTWAIT
          ;; the reason read at once, before anything else can change it
          (values sent (if (minusp sent) (sb-alien:get-errno) 0)))))))

(defun receive-with-descriptors (fd sap length)
  "Receive up to LENGTH octets into SAP from the local socket open as file
descriptor (FD), without waiting, and the descriptors passed with them
\(SEND-WITH-DESCRIPTORS), each closed in programs started. Four values: how
many octets came, 0 when the other end has gone, -1 when none are there now
or the socket failed; the descriptors, a list, which the caller closes; true
when more were passed than +MOST-DESCRIPTORS+, those beyond it closed
unseen; and for -1 the reason, errno (EAGAIN for none now)."
  (sb-alien:with-alien ((part (array (sb-alien:unsigned 64) 2))
                        (control (array (sb-alien:unsigned 8) 32))
                        (message (array (sb-alien:unsigned 8) 56)))
    (let ((p (sb-alien:alien-sap part)) (c (sb-alien:alien-sap control)) (m (sb-alien:alien-sap message)))
      (setf (sb-sys:sap-ref-64 p 0) (sb-sys:sap-int sap) (sb-sys:sap-ref-64 p 8) length)
      (dotimes (i 32) (setf (sb-sys:sap-ref-8 c i) 0))
      (dotimes (i 56) (setf (sb-sys:sap-ref-8 m i) 0))
      (setf (sb-sys:sap-ref-64 m 16) (sb-sys:sap-int p) (sb-sys:sap-ref-64 m 24) 1
            (sb-sys:sap-ref-64 m 32) (sb-sys:sap-int c) (sb-sys:sap-ref-64 m 40) 32)
      (let ((got (%recvmsg fd m (logior #x40000000 #x40)))  ; MSG_CMSG_CLOEXEC, MSG_DONTWAIT
            (errno (sb-alien:get-errno))
            (descriptors '()))
        (when (>= got 0)
          ;; each struct cmsghdr in turn, 8-aligned: its length, level and type, then its data
          (loop with filled = (min 32 (sb-sys:sap-ref-64 m 40))
                for at = 0 then (+ at (* 8 (ceiling size 8)))
                for size = (if (<= (+ at 16) filled) (sb-sys:sap-ref-64 c at) 0)
                while (and (>= size 16) (<= (+ at size) filled))
                when (and (= 1 (sb-sys:sap-ref-32 c (+ at 8))) (= 1 (sb-sys:sap-ref-32 c (+ at 12))))
                  do (loop for k from (+ at 16) below (+ at size) by 4
                           do (push (sb-sys:signed-sap-ref-32 c k) descriptors))))
        (values got (nreverse descriptors) (logtest 8 (sb-sys:sap-ref-32 m 48))  ; MSG_CTRUNC
                (if (minusp got) errno 0))))))

(defconstant +watch-events+ (logior #x2 #x4 #x8 #x40 #x80 #x100 #x200 #x400 #x800 #x1000000)
  "The inotify events a watched directory reports: a file in it written
(IN_MODIFY), its times or permissions changed (IN_ATTRIB), closed after writing
(IN_CLOSE_WRITE), renamed away or in (IN_MOVED_FROM, IN_MOVED_TO), made
(IN_CREATE) or deleted (IN_DELETE); the directory itself deleted or renamed
(IN_DELETE_SELF, IN_MOVE_SELF); watched only if it is a directory (IN_ONLYDIR).")

(defconstant +in-modify+ #x2 "The inotify event of a file written to, IN_MODIFY.")

(defconstant +in-directory-gone+ (logior #x400 #x800 #x8000)
  "The inotify events saying a watch ended: its directory deleted or renamed
(IN_DELETE_SELF, IN_MOVE_SELF), or the watch removed (IN_IGNORED).")

(defconstant +in-queue-overflow+ #x4000 "The inotify event saying events were lost, IN_Q_OVERFLOW.")

(defun inotify-open ()
  "A new inotify descriptor, not blocking and closed in programs the editor
starts; NIL where the system gives none (too many in use)."
  (let ((fd (%inotify-init1 (logior #o4000 #o2000000)))) ; IN_NONBLOCK, IN_CLOEXEC
    (and (>= fd 0) fd)))

(defun inotify-watch (fd directory)
  "Have the inotify file descriptor (FD) report +WATCH-EVENTS+ in DIRECTORY (a
native name). Returns the watch descriptor, or NIL when none can be had (no
permission, the system's limit on watches reached)."
  (let ((wd (%inotify-add-watch fd directory +watch-events+)))
    (and (>= wd 0) wd)))

(defun inotify-unwatch (fd wd)
  "End the watch WD of the inotify file descriptor (FD); nothing when it ended
already."
  (%inotify-rm-watch fd wd)
  nil)

(defun inotify-events (fd octets)
  "The events waiting on the inotify file descriptor (FD), read through OCTETS
(64 KB holds hundreds), in order: lists of the watch descriptor (-1 for the
queue's overflow), the event mask and the name of the file in the directory
as octets (NIL for the directory itself). NIL when none wait."
  (let ((events '()))
    (loop
      (let ((got (with-octets-sap (sap octets)
                   (%read fd sap (length octets)))))
        (cond ((plusp got)
               (let ((at 0))
                 (loop while (< at got)
                       do (let* ((wd (octets-integer octets at 4))
                                 (mask (octets-integer octets (+ at 4) 4))
                                 (length (octets-integer octets (+ at 12) 4))
                                 (start (+ at 16))
                                 (end (or (position 0 octets :start start :end (+ start length)) (+ start length))))
                            (push (list (if (= wd #xffffffff) -1 wd) mask (and (> end start) (subseq octets start end)))
                                  events)
                            (setf at (+ start length))))))
              ((and (minusp got) (= (sb-alien:get-errno) sb-posix:eintr)))
              (t (return)))))
    (nreverse events)))

;;; Extended attributes

(defun fd-xattr-names (fd)
  "The names of the extended attributes of the file open as file descriptor
(FD), or NIL."
  (let ((size (%flistxattr fd (sb-sys:int-sap 0) 0)))
    (when (plusp size)
      (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
        (let ((got (with-octets-sap (sap buffer)
                     (%flistxattr fd sap size))))
          (when (plusp got)
            (loop with start = 0
                  for end = (position 0 buffer :start start :end got)
                  while end
                  when (> end start)
                    collect (tuatara.text:octets-string buffer :start start :end end)
                  do (setf start (1+ end)))))))))

(defun fd-xattr-value (fd name)
  "The value of the extended attribute NAME of the file open as file
descriptor (FD), as octets, or NIL when it has none or it cannot be read."
  (let ((key (path-octets name)))
    (flet ((get-value (value size)
             (with-octets-sap (key-sap key)
               (%fgetxattr fd key-sap value size))))
      (let ((size (get-value (sb-sys:int-sap 0) 0)))
        (when (and (>= size 0) (<= size (* 64 1024)))
          (let* ((value (make-array size :element-type '(unsigned-byte 8)))
                 (got (with-octets-sap (sap value) (get-value sap size))))
            (and (>= got 0) (subseq value 0 got))))))))

(defun copy-xattrs (from to)
  "Give the open file TO every extended attribute of the open file FROM that the
file systems and your rights allow (SELinux label, ACLs, user attributes);
those that cannot be set are left out, as on file systems without them."
  (dolist (name (fd-xattr-names from))
    (tuatara.runtime:ignoring-errors-reported ("copying the extended attribute ~a" name)
      (let ((value (fd-xattr-value from name)))
        (when value
          ;; an empty value set from a buffer of one octet, none of it given
          (let ((buffer (if (plusp (length value)) value (make-array 1 :element-type '(unsigned-byte 8))))
                (key (path-octets name)))
            (with-octets-sap (sap buffer)
              (with-octets-sap (key-sap key)
                (%fsetxattr to key-sap sap (length value) 0)))))))))

;;; Directories and unnamed files

(defun sync-directory (directory)
  "Ask the system to put a rename in DIRECTORY on disk, where it can: a file
renamed over another is then there after a power cut too."
  (let ((fd (tuatara.runtime:ignoring-errors-reported ("opening ~a to sync it" directory)
              (sb-posix:open (native directory) (logior sb-posix:o-rdonly sb-posix:o-directory)))))
    (when fd
      (tuatara.runtime:ignoring-errors-reported ("syncing ~a" directory) (sb-posix:fsync fd))
      (sb-posix:close fd))))

(defconstant +o-noatime+ #o1000000
  "Linux's O_NOATIME, which sb-posix does not name: reading does not change the
file's access time. Allowed only on your own files.")

(defconstant +o-tmpfile+ #o20200000
  "Linux's O_TMPFILE, which sb-posix does not name: open a directory this way to
get a new file with no name, freed when closed and unmapped, even after a crash.")

(sb-ext:defglobal **scratch-counter** (list 0)
  "Numbers the named scratch files made where a file system has no unnamed ones.")

(defun random-name ()
  "A name no one can guess: 64 random bits written in base 36, at most 13
letters and digits."
  (format nil "~36r" (random (expt 2 64) (make-random-state t))))

(defun scratch-name (directory tag)
  "A name for a scratch file in DIRECTORY that nothing else uses: hidden, with
TAG, the process and a counter."
  (format nil "~a/.tuatara-~a-~d-~d" (string-right-trim "/" (native directory)) tag
          (sb-posix:getpid) (sb-ext:atomic-incf (car **scratch-counter**))))

(defun call-with-directory (directory function operation)
  "Call FUNCTION with a descriptor of the directory DIRECTORY, open for making
entries in it and closed after; refused (WRITING-REFUSED, OPERATION said)
when nothing may be written there, told by where the directory opened really
is, so that nothing swapped for it meanwhile is written in."
  (let ((fd (handler-case (sb-posix:open (string-right-trim "/" (native directory))
                                         ;; O_PATH: a directory you may write in but not list (-wx) too
                                         (logior +o-path+ +o-directory+ +o-cloexec+))
              (sb-posix:syscall-error (e) (fail "open" directory (sb-posix:syscall-errno e))))))
    (unwind-protect (progn (check-writing (string-right-trim "/" (native directory)) operation :fd fd)
                           (funcall function fd))
      (sb-posix:close fd))))

(defun unlink-entry-at (dirfd name &key directory)
  "Take away the entry NAME (a string, or octets) of the directory open as
DIRFD: an empty directory when DIRECTORY, else a file or a symbolic link.
True when it went; NIL, with the errno, when not."
  (with-octets-sap (sap (terminated-octets (if (stringp name) (tuatara.text:string-octets name) name)))
    (if (zerop (%unlinkat dirfd sap (if directory #x200 0)))  ; AT_REMOVEDIR
        t
        (values nil (sb-alien:get-errno)))))

(defun open-unnamed-at (dirfd mode)
  "A descriptor, open for reading and writing, for a new file with no name in
the directory open as DIRFD, with permissions MODE (less the umask), gone when it
is closed; NIL where the file system has no unnamed files or the directory may
not be written."
  (values (openat-octets dirfd (tuatara.text:string-octets ".") (logior +o-tmpfile+ sb-posix:o-rdwr +o-cloexec+) mode)))

(defun open-scratch-file (directory)
  "A descriptor for a new file with no name in DIRECTORY, open for reading and
writing. Where the file system has no unnamed files (NFS, FUSE, SMB and others),
a named one removed at once, which comes to the same. Refused
\(WRITING-REFUSED) where nothing may be written, told by the directory opened."
  (unless directory
    (refuse "This text is not written to a scratch file on disk (the text of an encrypted file): too large to work on ~
             in memory"))
  (call-with-directory
   directory
   (lambda (dirfd)
     (or (open-unnamed-at dirfd #o600)
         (let ((name (native-file-name (scratch-name directory "scratch"))))
           (multiple-value-bind (fd errno) (open-new-entry dirfd name #o600 :access sb-posix:o-rdwr)
             (unless fd (fail "create a file in" directory errno))
             (unlink-entry-at dirfd name)
             fd))))
   "make a scratch file in"))

(defvar *proc-fd-available* :unknown
  "Whether /proc/self/fd is there to give unnamed files a name; looked at once.")

(defun proc-fd-available-p ()
  "True when /proc/self/fd is there, through which an open file is opened
again and an unnamed file named: looked at the first time it is asked."
  (when (eq *proc-fd-available* :unknown)
    (setf *proc-fd-available* (and (probe-file "/proc/self/fd/") t)))
  *proc-fd-available*)

(defun open-unnamed-file (directory mode)
  "A descriptor, open for reading and writing, for a new file with no name yet in DIRECTORY
with permissions MODE (less the umask), which NAME-UNNAMED-FILE can name once
it is complete; NIL where the file system or the system cannot, for the caller
to use a named file. A crash before it is named leaves nothing behind.
Refused (WRITING-REFUSED) where nothing may be written, told by the directory
opened."
  (when (proc-fd-available-p)
    (call-with-directory directory
                      (lambda (dirfd) (open-unnamed-at dirfd mode))
                      "create a file in")))

(defun link-unnamed-at (fd dirfd name)
  "Give the file open as file descriptor (FD), one with no name (OPEN-UNNAMED-AT),
the name NAME (a string) in the directory open as DIRFD, the directory it was
made in. True when done; NIL when the system will not, or NAME is taken."
  (zerop (%linkat-named +at-fdcwd+ (format nil "/proc/self/fd/~d" fd) dirfd name #x400)))  ; AT_SYMLINK_FOLLOW

(defun name-unnamed-file (fd directory tag)
  "Give file descriptor (FD), from OPEN-UNNAMED-FILE, a hidden name with TAG
in DIRECTORY (the same directory it was made in) and return the name; NIL
when the system will not. Refused (WRITING-REFUSED) where nothing may be
written, told by the directory opened."
  (let ((name (scratch-name directory tag)))
    (call-with-directory directory
                         (lambda (dirfd) (and (link-unnamed-at fd dirfd (native-file-name name)) name))
                         "name a file in")))

;;; The names of accounts and groups

(defvar *owner-names-lock* (sb-thread:make-mutex :name "owner names")
  "Held while the names of accounts and groups are looked up, which the system
does in one place for every thread.")

(defvar *owner-names* (make-hash-table :test 'equal)
  "The names of accounts and groups looked up so far, by (:USER . user id
(UID)) and \(:GROUP . group id (GID)); NIL for a number with no name.")

(defun looked-up-name (kind number)
  "The name of the account (KIND :USER) or group (KIND :GROUP) whose number is
NUMBER, looked up once and kept (*OWNER-NAMES*); NIL when it has none."
  (sb-thread:with-mutex (*owner-names-lock*)
    (let ((key (cons kind number)))
      (multiple-value-bind (name found) (gethash key *owner-names*)
        (if found
            name
            (setf (gethash key *owner-names*)
                  (tuatara.runtime:ignoring-errors-reported ("looking up the name of ~(~a~) ~d" kind number)
                    (if (eq kind :user)
                        (let ((entry (sb-posix:getpwuid number))) (and entry (sb-posix:passwd-name entry)))
                        (let ((entry (sb-posix:getgrgid number))) (and entry (sb-posix:group-name entry)))))))))))

(defun owner-name (uid)
  "The name of the account whose number is user id (UID) (a file's owner:
STAMP-UID), or NIL when it has none."
  (looked-up-name :user uid))

(defun owner-group-name (gid)
  "The name of the group whose number is group id (GID) (a file's group:
STAMP-GID), or NIL when it has none."
  (looked-up-name :group gid))

(defun process-groups ()
  "The supplementary groups of this process, as numbers."
  (let ((n (%getgroups 0 (sb-sys:int-sap 0))))
    (when (plusp n)
      (let ((buffer (make-array (* 4 n) :element-type '(unsigned-byte 8) :initial-element 0)))
        (with-octets-sap (sap buffer)
          (let ((got (%getgroups n sap)))
            (loop for i below (max 0 got) collect (sb-sys:sap-ref-32 sap (* 4 i)))))))))

(defun can-give-owner-p (stamp)
  "True when a new file this process makes can be given the owner and group in
STAMP: the process runs as root, or as that owner and a member of that group."
  (let ((euid (sb-posix:geteuid)))
    (or (zerop euid)
        (and (= euid (stamp-uid stamp))
             (or (= (sb-posix:getegid) (stamp-gid stamp))
                 (and (member (stamp-gid stamp) (process-groups)) t))))))

;;; Places nothing is written in

(define-condition writing-refused (file-system-error) ()
  (:documentation "A file operation refused by the editor itself for where it
would write, make, change or delete something (WRITING-REFUSAL)."))

(defgeneric host-writing-refusal (host path &key dirfd fd)
  (:documentation "Where the editor writes nothing: why nothing may be written at PATH (a native
name) where something is about to be written, made, renamed, changed or
deleted, in words, or NIL when it may; DIRFD, a descriptor of the directory it is
in, and file descriptor (FD), one of the thing itself, tell where that really
is. With NIL for PATH, true when any place at all is refused. The evidence
directories (*EVIDENCE-DIRECTORIES*) are refused so.")
  (:method ((host tuatara.runtime:host) path &key dirfd fd)
    (declare (ignore path dirfd fd))
    nil))

(defun writing-refusal (path &key dirfd fd)
  "Why nothing may be written at PATH (a native name, or a pathname), in
words, or NIL when it may: as HOST-WRITING-REFUSAL says, by where PATH
really leads, every link followed; with DIRFD, an open descriptor of the
directory PATH is in, or file descriptor (FD), one of PATH itself, by where that
really is now, whatever names lead there."
  (and path (host-writing-refusal tuatara.runtime:*host* (native path) :dirfd dirfd :fd fd)))

(defun writing-refused-anywhere-p ()
  "True when the editor refuses to write in some place
\(HOST-WRITING-REFUSAL), so that what may reach one is looked at."
  (and (host-writing-refusal tuatara.runtime:*host* nil) t))

(defun check-writing (path operation &key dirfd fd)
  "Signal WRITING-REFUSED for OPERATION (a verb phrase, \"save\") on PATH when
nothing may be written there (WRITING-REFUSAL, DIRFD and file descriptor (FD)
as it has them)."
  (let ((why (writing-refusal path :dirfd dirfd :fd fd)))
    (when why
      (error 'writing-refused :operation operation :path (native path) :reason why))))

(defgeneric host-say-writing (host words)
  (:documentation "Say WORDS, something done in writing a file that you should know of (a
file's other names left as they were); called from any thread (TELL-WRITING).")
  (:method ((host tuatara.runtime:host) words)
    (declare (ignore words))
    nil))

(defun tell-writing (words)
  "Say WORDS of a file written (HOST-SAY-WRITING)."
  (tuatara.runtime:ignoring-errors-reported ("telling of a file written")
    (host-say-writing tuatara.runtime:*host* words)))

(defun symlink-target (path)
  "What the symbolic link at PATH holds, as a native name, the link itself read
and nothing it leads to; NIL when PATH is no link or it cannot be read."
  (let ((octets (link-octets +at-fdcwd+ (tuatara.text:string-octets (native path)))))
    (and octets (tuatara.text:octets-string octets))))

(defun fd-real-path (fd)
  "Where the file or directory open as file descriptor (FD) really is, as a
native name with every link followed (as the system names it in
/proc/self/fd); NIL when it does not say, or the file has no name any more."
  (let ((octets (link-octets +at-fdcwd+ (tuatara.text:string-octets (format nil "/proc/self/fd/~d" fd)))))
    (when (and octets (plusp (length octets)) (= (aref octets 0) 47))
      (let ((name (tuatara.text:octets-string octets)))
        (unless (uiop:string-suffix-p name " (deleted)")
          name)))))

(defun writable-p (path)
  "True when this process may write PATH: the file's contents, or for a
directory, files made and taken away in it; never a place the editor writes
nothing in (WRITING-REFUSAL)."
  (and (tuatara.runtime:ignoring-errors-reported ("examining whether ~a may be written" path)
         (handler-case (progn (sb-posix:access (native path) sb-posix:w-ok) t) (sb-posix:syscall-error () nil)))
       (not (writing-refusal path))))

(define-setting *cache-directory* nil
  "The directory Tuatara keeps its cache in.
The cache holds files made again when they are gone: compiled packages,
previews and pages drawn, copies of files opened outside, the debug log and
stderr.log. A directory on an encrypted drive keeps what files you open leave
there on it: make it yourself, for you alone (mkdir -m 700); while it is not
there, its drive not mounted, nothing is kept in its place and the work that
needs it says so.
- nil: ~/.cache/tuatara/ ($XDG_CACHE_HOME/tuatara/), the standard place.
- a directory's name: that directory."
  :values (or (member nil) directory))

(define-setting *state-directory* nil
  "The directory Tuatara keeps what it remembers between sessions in.
That is the files visited lately, places in them, the session, auto-saved
text, the packages fetched. As *CACHE-DIRECTORY*, make it yourself; while it
is not there nothing is kept in its place.
- nil: ~/.local/state/tuatara/ ($XDG_STATE_HOME/tuatara/), the standard
  place.
- a directory's name: that directory."
  :values (or (member nil) directory))

(defvar *directories-in-memory* '()
  "The settings naming a directory that was not there when it was wanted
\(CHOSEN-DIRECTORY), each as (SETTING NAME IN-MEMORY VALUE): the directory's
NAME, the directory in memory used in its place, and the setting's VALUE then.
One whose setting has been changed since counts no more.")

(defun directory-in-memory-p (setting)
  "True while the directory the setting SETTING (a symbol) names is kept in memory
for want of its drive (CHOSEN-DIRECTORY): the setting has not been changed
since."
  (let ((entry (assoc setting *directories-in-memory*)))
    (and entry (eq (fourth entry) (symbol-value setting)))))

(defun chosen-directory (setting standard)
  "The directory the setting SETTING (a symbol) names, as a pathname, when it
names one, else STANDARD, made if needed, for you alone. A directory named must
be there already: while it is not, its drive not mounted, one of the same
name in memory ($XDG_RUNTIME_DIR/tuatara/, gone when the computer stops) is
used in its place and noted in *DIRECTORIES-IN-MEMORY*, so that nothing kept
for it lands on another drive; the note goes once the directory is there or the
setting is changed. A FILE-SYSTEM-ERROR when there is no such memory either,
or when the directory is one nothing may be written in (WRITING-REFUSED)."
  (let ((chosen (symbol-value setting)))
    (flet ((forget-in-memory ()
             (when (assoc setting *directories-in-memory*)
               (setf *directories-in-memory* (remove setting *directories-in-memory* :key #'first)))))
      (unless (directory-in-memory-p setting)
        (forget-in-memory))
      (if (null chosen)
          (directory-pathname (ensure-private-directory standard))
          (let ((name (directory-name chosen)))
            (check-writing name (format nil "keep files (~(~a~)) in" setting))
            (if (eq :directory (path-kind name))
                (progn (forget-in-memory)
                       (directory-pathname name))
                (let ((memory (own-directory "XDG_RUNTIME_DIR")))
                  (unless memory
                    (error 'file-system-error
                           :operation "keep files in" :path name
                           :reason (format nil "it is not there (~(~a~)): make it, or mount its drive" setting)))
                  (let* ((kind (first (uiop:split-string (string-trim "*" (symbol-name setting)) :separator "-")))
                         (in-memory (ensure-private-directory
                                     (merge-pathnames (format nil "tuatara/~(~a~)/" kind) memory))))
                    (unless (assoc setting *directories-in-memory*)
                      (push (list setting name in-memory chosen) *directories-in-memory*))
                    (directory-pathname in-memory)))))))))

(defun directories-in-memory-words ()
  "What is said once the init file is read of the directories it named that are
not there (*DIRECTORIES-IN-MEMORY*), a sentence each; NIL when all are."
  (loop for (setting name in-memory) in (reverse *directories-in-memory*)
        when (directory-in-memory-p setting)
          collect (format nil "~a (~(~a~)) is not there: kept in memory, ~a, until the computer stops"
                          name setting in-memory)))

(defun cache-directory ()
  "The editor's cache directory, made if needed, for you alone: for scratch
files that cannot go beside the file they serve. Never /tmp, which may be
memory. *CACHE-DIRECTORY* when set (CHOSEN-DIRECTORY)."
  (chosen-directory '*cache-directory* (uiop:xdg-cache-home "tuatara/")))

(defun sync-range (fd offset length &key wait)
  "Start writing the octets of the file open as file descriptor (FD) from
OFFSET, LENGTH of them, out to the device; with WAIT, also wait until they
are there. True when the system did so; NIL on a file system that cannot
(the caller's flush at the end then does it all)."
  (and (plusp length)
       (zerop (%sync-file-range fd offset length (if wait 7 2)))))  ; WAIT_BEFORE | WRITE | WAIT_AFTER, or WRITE

(defvar *session-log-words* nil
  "What the session writing to stderr.log is (the window, the terminal), as
SESSION-LOG was told; NIL before it is opened.")

(defun session-log (what &key (name "stderr.log"))
  "A descriptor open for writing on NAME, stderr.log, in the cache directory,
for a session's standard error, kept as the debug log is
\(TUATARA.RUNTIME:OPEN-ROTATED-LOG). It is for you alone. A new one is begun
unless another session writes to it. The logs of the sessions before are kept
as stderr.log.1, the one just before, to stderr.log.3, so that what a session
that ended badly wrote is still there once the next has begun. A line first
says WHAT the session is (the window, the terminal), its process and when it
began. NIL when the log cannot be made. A daemon's log is named for it
\(daemon-NAME.log)."
  (setf *session-log-words* what)
  (let ((directory (tuatara.runtime:ignoring-errors-reported ("finding the cache directory") (cache-directory))))
    (when directory
      (let ((fd (tuatara.runtime:ignoring-errors-reported ("opening the session log in ~a" directory)
                  (tuatara.runtime:open-rotated-log (native (merge-pathnames name directory)) :rotate t))))
        (when fd
          (let ((octets (sb-ext:string-to-octets
                         (format nil "Tuatara ~a on SBCL ~a, ~a, process ~d, began ~a~%"
                                 (tuatara:version) (lisp-implementation-version) what (sb-posix:getpid)
                                 (tuatara.runtime:local-time-stamp (get-universal-time)))
                         :external-format :utf-8)))
            (with-octets-sap (sap octets)
              (handler-case (sb-posix:write fd sap (length octets)) (sb-posix:syscall-error () nil))))
          fd)))))

(defun move-session-log ()
  "When standard error goes to stderr.log in the standard cache directory and
*CACHE-DIRECTORY* names another, send it to a new stderr.log there instead.
Take the standard directory's logs away when no other session writes them, and
the directory when it is left empty. What the session wrote before its init file
was read was only the line saying what it is. Called once the init file is
read."
  (let ((standard (native (uiop:xdg-cache-home "tuatara/stderr.log"))))
    (when (and *cache-directory* *session-log-words*
               (equal (handler-case (sb-posix:readlink "/proc/self/fd/2") (sb-posix:syscall-error () nil)) standard))
      (let ((fd (session-log *session-log-words*)))
        (when fd
          (sb-posix:dup2 fd 2)
          (sb-posix:close fd)
          (unless (tuatara.runtime:log-in-use-p standard)
            (dolist (n '("" ".1" ".2" ".3"))
              (handler-case (sb-posix:unlink (concatenate 'string standard n)) (sb-posix:syscall-error () nil)))
            ;; the directory too, when nothing else is in it
            (handler-case (sb-posix:rmdir (native (uiop:xdg-cache-home "tuatara/")))
              (sb-posix:syscall-error () nil))))))))

(defun state-directory ()
  "The editor's state directory, made if needed, for you alone: what it keeps
between one session and the next, such as the files visited lately.
*STATE-DIRECTORY* when set (CHOSEN-DIRECTORY), else $XDG_STATE_HOME/tuatara/,
or ~/.local/state/tuatara/."
  (let ((home (own-directory "XDG_STATE_HOME")))
    (chosen-directory '*state-directory*
                      (merge-pathnames "tuatara/"
                                       (or home (merge-pathnames ".local/state/" (user-homedir-pathname)))))))

(defun open-new-entry (dirfd name mode &key (access sb-posix:o-wronly))
  "A descriptor of a new file NAME (a string) made in the directory open as
DIRFD with permissions MODE less the umask, open for ACCESS (O_WRONLY, or
O_RDWR), kept from programs started; made only when nothing is at NAME, a
link included, never through one. NIL and the errno when it cannot be made
\(EEXIST: the name is taken)."
  (openat-octets dirfd (tuatara.text:string-octets name)
                 (logior access sb-posix:o-creat sb-posix:o-excl +o-nofollow+ +o-cloexec+) mode))

(defun call-with-new-entry (dirfd name mode function &key (path name))
  "Make the file NAME (a string) in the directory open as DIRFD, new, with
permissions MODE less the umask, open it for writing and call FUNCTION with its
file descriptor (FD), returning what FUNCTION returns. The descriptor is closed
however FUNCTION ends; when an error leaves it, the new file is taken away
again. An error naming PATH, and nothing made, when something is at NAME
already, a link too."
  (multiple-value-bind (fd errno) (open-new-entry dirfd name mode)
    (unless fd (fail "write" path errno))
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (tuatara.runtime:ignoring-errors-reported ("taking away ~a beside ~a" name path)
                              (unlink-entry-at dirfd name)))))
      (unwind-protect (funcall function fd)
        (tuatara.runtime:close-fd-quietly fd "~a beside ~a" name path)))))

(defun replace-file (path contents &key (mode #o666))
  "Write CONTENTS, a string (in UTF-8) or octets, as the whole of the file at
PATH, its name as the system has it ([Content_Types].xml too), made with MODE
less the umask when it is new. It is written into a new file beside it,
renamed over it once written and put on disk, so that the file is always
whole, the old contents or the new, after a power cut too. The new file is
taken away when it cannot be written whole (a full disk). Refused
\(WRITING-REFUSED) where nothing may be written, told by the directory opened, in
which the new file is made and renamed."
  (let ((name (native path))
        (octets (if (stringp contents) (tuatara.text:string-octets contents) contents)))
    (call-with-parent
     name
     (lambda (dirfd own)
       (let ((temporary (format nil "~a.~d.~a.new" (native-file-name name) (sb-posix:getpid) (random-name))))
         (call-with-new-entry
          dirfd temporary mode
          (lambda (fd)
            (write-octets fd octets :path name)
            (flush-fd fd name)
            (with-octets-sap (old (terminated-octets (tuatara.text:string-octets temporary)))
              (with-octets-sap (new (terminated-octets own))
                (multiple-value-bind (result errno) (rename-entry dirfd old dirfd new 0)
                  (unless (zerop result) (fail "write" name errno))))))
          :path name)
         (sync-directory (parent-path name))))
     :writing "write")
    path))

(defun environment-without (prefixes &rest added)
  "The environment of this process, as NAME=VALUE strings, without those that
begin with one of PREFIXES (\"GSK_RENDERER=\", \"TUATARA_\"), ADDED first:
the environment for a program started with settings of its own."
  (append added (remove-if (lambda (setting) (some (lambda (prefix) (uiop:string-prefix-p prefix setting)) prefixes))
                           (sb-ext:posix-environ))))

(defun duplicate-fd (fd &key (lowest 0))
  "A new file descriptor (FD) for what FD is open on, the lowest free one from
LOWEST, kept from the programs started (F_DUPFD_CLOEXEC)."
  (sb-posix:fcntl fd 1030 lowest))

(defun open-named-pipe (path)
  "Make a named pipe at PATH, for its owner alone, and open it both ways
without waiting, kept from programs started: the file descriptor (FD). Opened
both ways, it never makes another side wait to open it, and ends for the other
side when this one is closed. A FILE-SYSTEM-ERROR when it cannot be made."
  (handler-case
      (progn (sb-posix:mkfifo path #o600)
             (sb-posix:open path (logior sb-posix:o-rdwr sb-posix:o-nonblock +o-cloexec+)))
    (sb-posix:syscall-error (condition)
      (error 'file-system-error :operation "make a pipe" :path path
                                :reason (sb-int:strerror (sb-posix:syscall-errno condition))
                                :errno (sb-posix:syscall-errno condition)))))

(defun close-named-pipe (fd path)
  "Take away the named pipe at PATH, opened as the file descriptor (FD) by
OPEN-NAMED-PIPE, and close it: nothing can open it from then on."
  (handler-case (sb-posix:unlink path)
    (sb-posix:syscall-error () nil))
  (sb-posix:close fd))

(defun write-private-file (path text)
  "Write TEXT, a string, as the whole of the file at PATH in UTF-8, readable
and writable by its owner only, in one step (REPLACE-FILE)."
  (replace-file path text :mode #o600))

(defun write-new-file (path contents &key end (mode #o400) sync)
  "Write CONTENTS, a string (in UTF-8) or octets (the first END of them, when
END is given), as a new file at PATH, made with MODE from the start: readable
by you alone and written by nothing again (0400) as standard. An error, and
nothing written, when something is at PATH already, a link too; the new file
taken away when it cannot be written whole (a full disk). With SYNC, it is
on the disk before this returns (fsync). Returns PATH. Refused
\(WRITING-REFUSED) where nothing may be written, told by the directory opened,
in which the file is made."
  (let ((name (native path))
        (octets (if (stringp contents) (tuatara.text:string-octets contents) (coerce contents 'tuatara.text:octets))))
    (call-with-parent
     name
     (lambda (dirfd own)
       (declare (ignore own))
       (call-with-new-entry dirfd (native-file-name name) mode
                            (lambda (fd)
                              (write-octets fd octets :end (or end (length octets)) :path name)
                              (when sync (sb-posix:fsync fd)))
                            :path name))
     :writing "write")
    path))

(defun file-holes-p (path)
  "True when the regular file at PATH, a link not followed, has holes: parts
of it that hold no data on the disk, read as zeros (lseek's SEEK_HOLE finds
one before its end). Opened only when it is a regular file, never waited
on; NIL when it cannot be opened or the system cannot tell."
  (let ((fd (tuatara.runtime:ignoring-errors-reported ("opening ~a to look for holes" path)
              (handler-case (sb-posix:open (native path)
                                           (logior sb-posix:o-rdonly sb-posix:o-nonblock +o-nofollow+ +o-cloexec+))
                (sb-posix:syscall-error () nil)))))
    (when fd
      (unwind-protect
           (let ((stamp (fd-stamp fd path)))
             (and (eq (stamp-kind stamp) :regular)
                  (plusp (stamp-size stamp))
                  ;; SEEK_HOLE is 4: the first hole at or after 0, the file's end when it has none
                  (let ((hole (handler-case (sb-posix:lseek fd 0 4) (sb-posix:syscall-error () nil))))
                    (and hole (< hole (stamp-size stamp))))))
        (sb-posix:close fd)))))

(defun open-lock-file (path &key (wait 0))
  "Open the lock file at PATH, made readable and writable by you alone when it
is not there, never through a link, and take its lock, held alone: the open
file descriptor (FD), to close when the lock is to go, or NIL, nothing left
open, when another holds it for more than WAIT seconds. What is written
through FD once it is had is read by no one in the meantime who takes the
lock. Signals FILE-SYSTEM-ERROR when it cannot be opened."
  (let ((fd (handler-case (sb-posix:open (native path)
                                         (logior sb-posix:o-rdwr sb-posix:o-creat +o-nofollow+ +o-cloexec+) #o600)
              (sb-posix:syscall-error (e) (fail "open" path (sb-posix:syscall-errno e))))))
    (if (tuatara.runtime:lock-fd fd :wait wait)
        fd
        (progn (sb-posix:close fd) nil))))

(defun lock-file-held-p (path)
  "True when the lock of the lock file at PATH (OPEN-LOCK-FILE) is held now,
by another process or this one; NIL when it is free or there is no such
file. Only a plain file counts: a link is not followed, and a named pipe or
a device there is never waited on."
  (let ((fd (handler-case (sb-posix:open (native path) (logior sb-posix:o-rdonly sb-posix:o-nonblock
                                                               +o-nofollow+ +o-cloexec+))
              (sb-posix:syscall-error () nil))))
    (and fd (unwind-protect (and (sb-posix:s-isreg (sb-posix:stat-mode (sb-posix:fstat fd)))
                                 (not (tuatara.runtime:lock-fd fd :shared t)))
              (sb-posix:close fd)))))

(defun call-with-state-lock (path function &key (wait 0.1))
  "Call FUNCTION, of no arguments, holding the lock of the file at PATH, a
file the sessions side by side keep together (the files visited lately, the
places in files). The lock is a lock file beside it, its name with .lock after
it, so that one session at a time reads it, changes it and writes it back, and
none loses what another wrote meanwhile. Returns what FUNCTION returns. It
returns NIL, FUNCTION not called, when another holds the lock for more than
WAIT seconds or the lock file cannot be made, so that the editor is never held
up. It does so too in a place nothing may be written in (WRITING-REFUSAL),
told by the directory opened."
  (let ((fd (tuatara.runtime:ignoring-errors-reported ("making the lock file of ~a" path)
              (call-with-parent (concatenate 'string (native path) ".lock")
                                (lambda (dirfd own)
                                  (values (openat-octets dirfd own
                                                         (logior sb-posix:o-rdwr sb-posix:o-creat
                                                                   +o-nofollow+ +o-cloexec+)
                                                         #o600)))
                                :writing "lock"))))
    (when fd
      (unwind-protect
           (when (tuatara.runtime:lock-fd fd :wait wait)
             (funcall function))
        (sb-posix:close fd)))))

(defun append-to-file (path contents)
  "Write CONTENTS, a string (in UTF-8) or octets, at the end of the file at
PATH in one write, the file made readable and written by you alone when it is
not there, never through a link: a line added this way by several sessions
at once is never cut into another's (O_APPEND). Refused (WRITING-REFUSED)
where nothing may be written, told by the directory opened."
  (let ((octets (if (stringp contents) (tuatara.text:string-octets contents) contents)))
    (call-with-parent (native path)
                      (lambda (dirfd own)
                        (let ((fd (openat-octets dirfd own (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-append
                                                                   +o-nofollow+ +o-cloexec+)
                                                 #o600)))
                          (unwind-protect (write-octets fd octets :path (native path))
                            (sb-posix:close fd))))
                      :writing "append to")))

(defvar *saved-program* nil
  "The path of the saved Tuatara program this process runs as, or NIL when it
runs from the sources inside Steel Bank Common Lisp (SBCL): what
RUN-PROGRAM-AGAIN runs, and what a copy of Tuatara started for a moment is
started as.")

(defun argument-vector (strings)
  "STRINGS as the system takes a program's arguments: foreign memory holding
a pointer to each one's octets, ended by a zero octet, and a null pointer
after the last. Each is written as every name handed to the system is, a raw
byte's character (U+DC80..U+DCFF) as the octet it stands for and the rest as
UTF-8. Freed with FREE-ARGUMENT-VECTOR."
  (let ((argv (sb-alien:make-alien (* sb-alien:char) (1+ (length strings)))))
    (loop for string in strings
          for i from 0
          do (let* ((octets (tuatara.text:string-octets string))
                    (alien (sb-alien:make-alien (sb-alien:unsigned 8) (1+ (length octets))))
                    (sap (sb-alien:alien-sap alien)))
               (loop for octet across octets
                     for at from 0
                     do (setf (sb-sys:sap-ref-8 sap at) octet))
               (setf (sb-sys:sap-ref-8 sap (length octets)) 0)
               (setf (sb-alien:deref argv i) (sb-alien:sap-alien sap (* sb-alien:char)))))
    (setf (sb-alien:deref argv (length strings)) (sb-alien:sap-alien (null-pointer) (* sb-alien:char)))
    argv))

(defun free-argument-vector (argv)
  "Free ARGV, an ARGUMENT-VECTOR, and the strings it points at."
  (loop for i from 0
        for string = (sb-alien:deref argv i)
        until (null-pointer-p (sb-alien:alien-sap string))
        do (sb-alien:free-alien string))
  (sb-alien:free-alien argv))

(defun run-program-again (arguments &key (environment nil environment-p) heap)
  "Run this program again in place of this process: *SAVED-PROGRAM* with
ARGUMENTS, strings, after its name, in ENVIRONMENT, NAME=VALUE strings, or
the environment as it is now; names that are not UTF-8 among them are handed
on as the octets they were read from. HEAP, a number of megabytes, is the
size of the memory it starts with. Returns only when that fails, or at once
when Tuatara runs from the sources."
  (unless *saved-program* (return-from run-program-again nil))
  (let ((argv (argument-vector (cons (first sb-ext:*posix-argv*)
                                     (if heap
                                         (list* "--dynamic-space-size" (format nil "~dMB" heap) arguments)
                                         arguments))))
        (envp (and environment-p (argument-vector environment))))
    (unwind-protect
         (if envp
             (%execve *saved-program* (sb-alien:alien-sap argv) (sb-alien:alien-sap envp))
             (%execv *saved-program* (sb-alien:alien-sap argv)))
      (when envp (free-argument-vector envp))
      (free-argument-vector argv))))
