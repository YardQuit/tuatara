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
      (documentation 'file-system-error-errno 'function) "The system's number for why a FILE-SYSTEM-ERROR happened, or NIL.")

;;; Names as the system has them

(defun keep-raw-bytes-in-names ()
  "Have every string handed to the system (a path to open, examine, rename)
keep its raw bytes' characters (U+DC80..U+DCFF, what a name that is not UTF-8
is read as) as the octets they stand for, so that such a name reaches the
system exactly as it was read; any other string is written as UTF-8, as
before. Done once, as the program loads: SBCL's writer of UTF-8 for the
system is wrapped, as its TRACE wraps a function."
  (let ((name 'sb-impl::output-to-c-string/utf-8/lf))
    (unless (sb-int:encapsulated-p name 'raw-bytes)
      (sb-int:encapsulate name 'raw-bytes
                          (lambda (original string)
                            (if (find-if (lambda (c) (tuatara.text:raw-byte-code-p (char-code c))) string)
                                (let* ((octets (tuatara.text:string-octets string))
                                       (out (make-array (1+ (length octets)) :element-type '(unsigned-byte 8) :initial-element 0)))
                                  (replace out octets)
                                  out)
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

(defun path-folder-and-name (path)
  "PATH's name parted at its last /, as two values: the folder, ending in /
\(./ when there is no /), and the name after it, empty for a name ending in
/: \"/data/case/\" and \"notes [old].org\" for /data/case/notes [old].org."
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

(defvar *home-names* nil
  "The home directory's names, as HOME gives it and as the file system has it
(where HOME leads there through a link, as /home to /var/home), each ending in
/, kept with the HOME they were found for: (HOME . NAMES).")

(defun home-directory-names ()
  "The names of the home directory, HOME's first, then the file system's own
when it differs (*HOME-NAMES*), found once for each HOME."
  (let ((home (sb-ext:native-namestring (user-homedir-pathname))))
    (unless (equal (car *home-names*) home)
      (let ((true (ignore-errors (probe-file (user-homedir-pathname)))))
        (setf *home-names* (cons home (remove-duplicates (list home (if true (sb-ext:native-namestring true) home))
                                                         :test #'string= :from-end t)))))
    (cdr *home-names*)))

(defun home-directory ()
  "The home directory as the file system names it, ending in /: where ~ leads,
so that a file reached by ~ has the name it has when reached any other way."
  (car (last (home-directory-names))))

(defvar *default-directory* "~/"
  "The directory of a buffer with no file (*scratch*, the lists): a file name
typed there is taken from it, and \\[search-in-files] searches it. ~ is your
home directory; NIL for the directory Tuatara was started in. A buffer with a
file has its file's directory.")

(defun expand-path (path &key directory (tilde t))
  "PATH as an absolute pathname: a string is taken literally, so that [, *, ?
and \\ in a file's name are only characters, and with TILDE a leading ~ or ~/
means your home directory (HOME-DIRECTORY); a relative one is taken from
DIRECTORY, a pathname or a string taken literally too, by default the
DEFAULT-DIRECTORY, asked for only then: an absolute path asks the file
system nothing."
  (let ((pathname (if (pathnamep path)
                      path
                      (sb-ext:parse-native-namestring
                       (let ((home (home-directory)))
                         (cond ((and tilde (string= path "~")) home)
                               ((and tilde (> (length path) 1) (string= "~/" path :end2 2))
                                (concatenate 'string home (subseq path 2)))
                               (t path)))))))
    (if (uiop:absolute-pathname-p pathname)
        pathname
        (merge-pathnames pathname (directory-pathname (or directory (default-directory)))))))

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
                          (ignore-errors (directory-pathname (expand-path *default-directory* :directory (uiop:getcwd)))))))
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
                  `((sb-alien:alien-funcall
                     (sb-alien:extern-alien "memset" (function sb-sys:system-area-pointer sb-sys:system-area-pointer
                                                               sb-alien:int sb-alien:unsigned-long))
                     ,sap 0 ,octets)))
              ,@body)
         (sb-alien:free-alien ,alien)))))

(defun sap-octets (sap &optional length)
  "The LENGTH octets at the foreign pointer SAP, copied into a new octet
vector; without LENGTH, the octets up to the first zero octet, the end of a
C string, that zero left out. Complexity: O(LENGTH), one block copy."
  (let* ((length (or length (sb-alien:alien-funcall
                             (sb-alien:extern-alien "strlen" (function sb-alien:unsigned-long sb-sys:system-area-pointer))
                             sap)))
         (octets (make-array length :element-type '(unsigned-byte 8))))
    (with-octets-sap (to octets)
      (sb-kernel:system-area-ub8-copy sap 0 to 0 length))
    octets))

(defun sap-string (sap &optional length)
  "The text of the LENGTH octets at the foreign pointer SAP, or of the C
string there without LENGTH, read as the editor reads a file's text: UTF-8,
an octet that is not UTF-8 kept as a raw byte."
  (tuatara.text:octets-string (sap-octets sap length)))

(defmacro with-c-string ((sap string &key (start 0) end length) &body body)
  "Run BODY with SAP pointing at STRING from START below END as C is given
text: UTF-8, a raw byte's character as the octet it stands for (as names are
given to the system), a NUL after it; LENGTH, when given a name, is bound to
the octets before the NUL. They are written on the stack, or for a long
string into octets kept for reuse (TUATARA.TEXT:WITH-OCTET-BLOCK), so no
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
:short, :uint8, :uint32, :ulong, :size, :double, :float, :void, :boolean (a
truth value as an int: GLib's gboolean), :bool (a truth value of one octet:
C's bool) and :string (text, in UTF-8); any other TYPE is taken as an alien
type itself."
    (case type
      (:pointer 'sb-sys:system-area-pointer)
      (:int 'sb-alien:int)
      (:uint 'sb-alien:unsigned-int)
      (:short 'sb-alien:short)
      (:uint8 '(sb-alien:unsigned 8))
      (:uint32 '(sb-alien:unsigned 32))
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
types C-TYPE knows. The library holding C-NAME is not loaded by NAME: its
callers load it first (FOREIGN-LIBRARY-P, as LOAD-GTK-LIBRARIES and
LOAD-NCURSES do). A :string argument may be any string, given as
WITH-C-STRING gives it; a :string result is read as SAP-STRING reads it, NIL
for a null pointer."
  (let* ((documentation (when (stringp (first arguments)) (list (pop arguments))))
         (names (mapcar #'first arguments))
         (saps (loop for (argument type) in arguments
                     collect (if (eq type :string) (gensym (symbol-name argument)) argument)))
         (call `(sb-alien:alien-funcall
                 (sb-alien:extern-alien ,c-name (function ,(c-type (if (eq result :string) :pointer result))
                                                          ,@(loop for (nil type) in arguments
                                                                  collect (c-type (if (eq type :string) :pointer type)))))
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

;;; statx: what a file is, exactly

(defstruct (stamp (:constructor make-stamp (dev ino size mtime nlink mode uid gid)) (:copier nil))
  "What the system reports about a file: DEV and INO name it; SIZE and MTIME (in
nanoseconds) change when its content does; NLINK, the names it has; MODE,
with its kind; UID and GID, its owners."
  (dev 0 :type integer :read-only t) (ino 0 :type integer :read-only t)
  (size 0 :type integer :read-only t) (mtime 0 :type integer :read-only t)
  (nlink 1 :type integer :read-only t) (mode 0 :type integer :read-only t)
  (uid 0 :type integer :read-only t) (gid 0 :type integer :read-only t))

(setf (documentation 'make-stamp 'function) "Return a stamp with the given fields."
      (documentation 'stamp-p 'function) "True when OBJECT is a STAMP."
      (documentation 'stamp-dev 'function) "The device of the file system holding the file."
      (documentation 'stamp-ino 'function) "The file's inode number on that device."
      (documentation 'stamp-size 'function) "The file's size in octets."
      (documentation 'stamp-mtime 'function) "When the file's content last changed, in nanoseconds."
      (documentation 'stamp-nlink 'function) "How many names (hard links) the file has."
      (documentation 'stamp-mode 'function) "The file's mode: its kind and permissions."
      (documentation 'stamp-uid 'function) "The file's owner."
      (documentation 'stamp-gid 'function) "The file's group.")

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
file, so that it can be examined, nothing in it opened: a device or a named
pipe named so is not woken.")

(defun statx-buffer-stamp (buffer)
  "The STAMP in BUFFER, the octets statx filled."
  (make-stamp (+ (ash (octets-le buffer 136 4) 32) (octets-le buffer 140 4)) ; dev major, minor
              (octets-le buffer 32 8) (octets-le buffer 40 8)
              (+ (* (octets-signed (octets-le buffer 112 8) 64) 1000000000)
                 (octets-le buffer 120 4))
              (octets-le buffer 16 4) (octets-le buffer 28 2) (octets-le buffer 20 4) (octets-le buffer 24 4)))

(defun statx-fields (dirfd name flags &optional (mask #x7ff))
  "The 256 octets statx fills for NAME (a string, or octets) relative to the
directory open as DIRFD, with FLAGS (#x100 not following a symbolic link,
#x1000 for DIRFD itself and an empty NAME) and MASK (the fields asked for:
those of a STAMP as standard, #xfff for the birth time too); NIL and the
errno when it cannot be examined."
  (let ((held (terminated-octets (if (stringp name) (tuatara.text:string-octets name) name)))
        (buffer (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (path held)
      (with-octets-sap (sap buffer)
        (if (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "statx" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                             sb-alien:int sb-alien:unsigned-int sb-sys:system-area-pointer))
                    dirfd path flags mask sap))
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
      (flet ((time-at (offset)
               (+ (* (octets-signed (octets-le buffer offset 8) 64) 1000000000) (octets-le buffer (+ offset 8) 4))))
        (list :modified (time-at 112) :changed (time-at 96) :accessed (time-at 64)
              ;; STATX_BTIME in the mask the system filled: a birth time it keeps
              :born (and (logtest (octets-le buffer 0 4) #x800) (time-at 80)))))))

(defun terminated-octets (octets)
  "OCTETS with a NUL octet after them, for the system."
  (let ((name (make-array (1+ (length octets)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace name octets)
    name))

(defun statx-octets (dirfd octets flags)
  "The STAMP statx reports for the entry whose name is OCTETS in the directory
open as DIRFD, with FLAGS (#x100 not following a symbolic link), and as more
values the octets it takes on the disk and the device numbers it stands for,
(MAJOR . MINOR), which mean something for a device only; NIL when it cannot be
examined."
  (let ((buffer (statx-fields dirfd octets flags)))
    (when buffer
      (values (statx-buffer-stamp buffer) (* 512 (octets-le buffer 48 8)) (cons (octets-le buffer 128 4) (octets-le buffer 132 4))))))

(defun openat-octets (dirfd octets flags &optional (mode 0))
  "A descriptor for the entry whose name is OCTETS in the directory open as
DIRFD, opened with FLAGS (made with MODE when they make it); NIL and the errno
when it cannot be opened."
  (let ((name (terminated-octets octets)))
    (with-octets-sap (path name)
      (let ((fd (sb-alien:alien-funcall
                 (sb-alien:extern-alien "openat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer sb-alien:int sb-alien:int))
                 dirfd path flags mode)))
        (if (minusp fd) (values nil (sb-alien:get-errno)) fd)))))

(defun map-directory-fd (function fd path)
  "Call FUNCTION with the name, as octets, of each entry of the directory open
as FD, . and .. among them, in the order the system gives them, read from its
start; FD stays open. A FILE-SYSTEM-ERROR, PATH named, when it cannot be read."
  (let* ((copy (sb-posix:dup fd))
         (dir (sb-alien:alien-funcall (sb-alien:extern-alien "fdopendir" (function sb-sys:system-area-pointer sb-alien:int)) copy)))
    (when (null-pointer-p dir)
      (let ((errno (sb-alien:get-errno)))
        (sb-posix:close copy)
        (fail "read" path errno)))
    (unwind-protect
         (progn
           (sb-posix:lseek copy 0 sb-posix:seek-set)
           (loop for entry = (sb-alien:alien-funcall
                              (sb-alien:extern-alien "readdir" (function sb-sys:system-area-pointer sb-sys:system-area-pointer))
                              dir)
                 until (null-pointer-p entry)
                 ;; the name stands at octet 19 of the record, after its inode, offset, length and type
                 do (funcall function (sap-octets (sb-sys:sap+ entry 19)))))
      ;; closes COPY too
      (sb-alien:alien-funcall (sb-alien:extern-alien "closedir" (function sb-alien:int sb-sys:system-area-pointer)) dir))))

(defun link-octets (dirfd octets)
  "What the symbolic link whose name is OCTETS in the directory open as DIRFD
holds, as octets; NIL when it cannot be read."
  (let ((name (terminated-octets octets))
        (out (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (path name)
      (with-octets-sap (sap out)
        (let ((n (sb-alien:alien-funcall
                  (sb-alien:extern-alien "readlinkat" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                                sb-sys:system-area-pointer sb-alien:unsigned-long))
                  dirfd path sap 4096)))
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
                   (>= (sb-alien:alien-funcall
                        (sb-alien:extern-alien "lgetxattr" (function sb-alien:long sb-sys:system-area-pointer sb-sys:system-area-pointer
                                                                     sb-sys:system-area-pointer sb-alien:unsigned-long))
                        sap key (sb-sys:int-sap 0) 0)
                       0))))))
      (cond ((or (has-p "system.posix_acl_access") (has-p "system.posix_acl_default")) #\+)
            ((has-p "security.selinux") #\.)))))

(defun path-stamp (path &key (follow t))
  "The STAMP of the file at PATH, following a symlink unless FOLLOW is NIL (a
slash at its end, which leads through a link, then disregarded); NIL when
there is no such file. Other failures signal a FILE-SYSTEM-ERROR."
  (multiple-value-bind (stamp errno) (statx-stamp -100 (let ((name (native path)))
                                                         (if (or follow (<= (length name) 1)) name (string-right-trim "/" name)))
                                                  (if follow 0 #x100))
    (cond (stamp)
          ((member errno (list sb-posix:enoent sb-posix:enotdir)) nil)
          (t (fail "examine" path errno)))))

(defun path-kind (path &key (follow t))
  "What is at PATH, as STAMP-KIND names it (:REGULAR, :DIRECTORY and the
rest), a symbolic link followed unless FOLLOW is NIL; NIL when nothing is
there or it cannot be examined."
  (let ((stamp (ignore-errors (path-stamp path :follow follow))))
    (and stamp (stamp-kind stamp))))

(defun fd-stamp (fd path)
  "The STAMP of the open file FD (PATH for messages)."
  (multiple-value-bind (stamp errno) (statx-stamp fd "" #x1000) ; AT_EMPTY_PATH
    (or stamp (fail "examine" path errno))))

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
XFS, bcachefs and ZFS. There a copy can take no time and no space, so scratch
files go beside the file they serve, for a save to share their blocks.")

(defun fd-file-system (fd)
  "The magic number of the file system holding the open file FD, or NIL."
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap buffer)
      (when (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "fstatfs" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer))
                    fd sap))
        (octets-le buffer 0 8)))))

(defun path-file-system (path &key (follow t))
  "The magic number of the file system holding PATH, a symbolic link followed
unless FOLLOW is NIL, or NIL when it cannot be examined. Only a name for it is
opened (O_PATH): a named pipe or a device there is not woken."
  (let ((fd (ignore-errors (sb-posix:open (native path) (logior +o-path+ +o-cloexec+ (if follow 0 +o-nofollow+))))))
    (and fd (unwind-protect (fd-file-system fd) (sb-posix:close fd)))))

(defun file-system-class (magic)
  "How files on the file system with MAGIC are handled: :PSEUDO, :REMOTE or :LOCAL."
  (cond ((null magic) :local)
        ((member magic *pseudo-file-systems*) :pseudo)
        ((member magic *remote-file-systems*) :remote)
        (t :local)))

(defun read-only-mount-p (fd)
  "True when the open file FD is on a file system mounted read-only (a CD, a
squashfs image, a read-only mount), where its content cannot change through
that mount."
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap buffer)
      (and (zerop (sb-alien:alien-funcall
                   (sb-alien:extern-alien "fstatvfs" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer))
                   fd sap))
           (logbitp 0 (octets-le buffer 72 8))))))            ; f_flag's ST_RDONLY

(defconstant +f-setlease+ 1024 "fcntl's F_SETLEASE.")
(defconstant +f-getlease+ 1025 "fcntl's F_GETLEASE.")

(defun fcntl-int (fd command argument)
  "fcntl(2) on FD with COMMAND and an integer ARGUMENT; its result, -1 on failure."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "fcntl" (function sb-alien:int sb-alien:int sb-alien:int sb-alien:long))
   fd command argument))

(defvar *lease-break-pipe* nil
  "A wake pipe (MAKE-WAKE-PIPE) woken whenever the system says a program waits
on a lease, so that a thread can wait on it rather than ask again and again;
made with the first lease taken.")

(defun take-read-lease (fd)
  "Ask for a read lease on the open file FD: then a program that opens the file
for writing, or truncates it, waits until the lease is given up (or the
system's lease-break time runs out), and *LEASE-BREAK-PIPE* is woken. True
when granted; not granted for files another user owns, files open for writing
now, and file systems without leases."
  (unless *lease-break-pipe*
    (setf *lease-break-pipe* (make-wake-pipe))
    ;; the signal the system sends a lease holder would end the process unhandled
    (sb-sys:enable-interrupt sb-unix:sigio (lambda (signal info context)
                                             (declare (ignore signal info context))
                                             (wake *lease-break-pipe*))))
  (zerop (fcntl-int fd +f-setlease+ 0)))                ; F_RDLCK

(defun lease-breaking-p (fd)
  "True when a program waits to write the file FD holds a read lease on."
  (= (fcntl-int fd +f-getlease+ 0) 2))                  ; F_UNLCK: the lease is to go

(defun release-lease (fd)
  "Give up the lease on the open file FD, letting a waiting writer go on."
  (fcntl-int fd +f-setlease+ 2)
  nil)

(defun free-bytes (path)
  "Octets an unprivileged user may still write on the file system holding PATH,
or NIL when the system does not say."
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (with-octets-sap (sap buffer)
      (when (zerop (sb-alien:alien-funcall
                    (sb-alien:extern-alien "statvfs" (function sb-alien:int sb-alien:c-string sb-sys:system-area-pointer))
                    (native path) sap))
        (* (octets-le buffer 32 8) (octets-le buffer 8 8))))))

(defun octets-words (n &key binary compact)
  "N octets in words, to one decimal: 950 B, 12.3 MB, 11.5 GB; with BINARY in
units of 1,024: 950 B, 954.0 MiB, 1.2 TiB. With COMPACT, as narrow as a
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
  (zerop (sb-alien:alien-funcall
          (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long sb-alien:int))
          dest #x40049409 source)))

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
            do (let ((copied (sb-alien:alien-funcall
                              (sb-alien:extern-alien "copy_file_range"
                                                     (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                               sb-alien:int sb-sys:system-area-pointer
                                                               sb-alien:unsigned-long sb-alien:unsigned-int))
                              source sap dest (sb-sys:int-sap 0) (- length done) 0)))
                 (cond ((plusp copied) (incf done copied))
                       ((zerop copied) (error 'file-system-error :operation "copy from" :path "a file"
                                                                 :reason "it ended early; it changed while being copied"))
                       ((= (sb-alien:get-errno) sb-posix:eintr))
                       ((member (sb-alien:get-errno)
                                (list sb-posix:exdev sb-posix:einval sb-posix:enosys sb-posix:eopnotsupp
                                      sb-posix:ebadf sb-posix:eperm))
                        (return))
                       (t (fail "copy" "a file"))))))
    done))

(defun write-sap (fd sap length &key (path "a file"))
  "Write LENGTH octets at SAP to the open file FD at its current offset, all of
them, again where the system wrote fewer or was interrupted."
  (let ((done 0))
    (loop while (< done length)
          do (let ((written (sb-alien:alien-funcall
                             (sb-alien:extern-alien "write" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                                      sb-alien:unsigned-long))
                             fd (sb-sys:sap+ sap done) (- length done))))
               (cond ((plusp written) (incf done written))
                     ((= (sb-alien:get-errno) sb-posix:eintr))
                     (t (fail "write" path)))))))

(defun write-octets (fd octets &key (start 0) (end (length octets)) (path "a file"))
  "Write OCTETS from START below END to the open file FD at its current
offset, all of them, again where the system wrote fewer or was interrupted;
signals FILE-SYSTEM-ERROR, naming PATH, when the system refuses, as when a
program reading from a pipe has ended."
  (with-octets-sap (sap octets)
    (write-sap fd (sb-sys:sap+ sap start) (- end start) :path path)))

;;; Terminals' sizes

(defun window-size-ioctl (fd request sap)
  "Ask the terminal FD for its size, or tell it, by REQUEST (TIOCGWINSZ or
TIOCSWINSZ) with the four 16-bit numbers at SAP: rows, columns, width and
height in pixels. True when it worked."
  (zerop (sb-alien:alien-funcall
          (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long sb-sys:system-area-pointer))
          fd request sap)))

(defun terminal-window-size (fd)
  "The size of the terminal FD, as four values: its rows and columns, and its
width and height in pixels, 0 when it does not say; NIL when FD is not a terminal."
  (with-foreign-memory (sap 8 :zero t)
    (when (window-size-ioctl fd #x5413 sap)
      (values (sb-sys:sap-ref-16 sap 0) (sb-sys:sap-ref-16 sap 2) (sb-sys:sap-ref-16 sap 4) (sb-sys:sap-ref-16 sap 6)))))

(defun set-terminal-window-size (fd rows columns width height)
  "Tell the terminal FD its size, which the program on it reads: ROWS and
COLUMNS, and WIDTH and HEIGHT in pixels (0 when not known), each at most 65535."
  (with-foreign-memory (sap 8)
    (setf (sb-sys:sap-ref-16 sap 0) (min 65535 rows) (sb-sys:sap-ref-16 sap 2) (min 65535 columns)
          (sb-sys:sap-ref-16 sap 4) (min 65535 width) (sb-sys:sap-ref-16 sap 6) (min 65535 height))
    (window-size-ioctl fd #x5414 sap)))

;;; Writing a file through a buffer

(defstruct (writer (:constructor make-writer (fd)) (:copier nil))
  "Octets written to the open file FD a megabyte at a time: FILL of them held
in BUFFER, WRITTEN already in the file."
  (fd -1 :type fixnum)
  (buffer (make-array 1048576 :element-type '(unsigned-byte 8)) :type tuatara.text:octets)
  (fill 0 :type fixnum)
  (written 0 :type fixnum))

(setf (documentation 'make-writer 'function) "A writer to the open file FD."
      (documentation 'writer-p 'function) "True when OBJECT is a WRITER."
      (documentation 'writer-fd 'function) "The open file a WRITER writes to, a descriptor.")

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
  "Read what the open file or pipe FD has, as much as fits, into OCTETS from
START below END, read again when a signal broke in: how many octets came, 0
at its end. A FILE-SYSTEM-ERROR, naming PATH, when it cannot be read."
  (loop (let ((got (with-octets-sap (sap octets)
                     (sb-alien:alien-funcall
                      (sb-alien:extern-alien "read" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                              sb-alien:unsigned-long))
                      fd (sb-sys:sap+ sap start) (- end start)))))
          (cond ((>= got 0) (return got))
                ((/= (sb-alien:get-errno) sb-posix:eintr) (fail "read" path))))))

(defun read-fd-octets (fd &key (path "a file") (size-hint 65536) most)
  "Every octet the open file FD gives until its end, as an octet vector: for
files whose size the system does not know or report truly (/proc, /sys), and
for files read into memory. With MOST, reading stops once more than MOST
octets have come, so that a file that never ends cannot fill memory: a
vector longer than MOST then says it was larger. SIZE-HINT, the octets
expected, sizes the vector read into, one more than it so that the end is
seen without growing it; the vector returned is that one, cut to what came."
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

(defstruct (octet-pieces (:constructor make-octet-pieces ()) (:copier nil))
  "Octets gathered a piece at a time (ADD-OCTET-PIECE), joined into one vector
once all have come (JOINED-OCTETS): SIZE octets in all, PIECES each (VECTOR
. N), the first N octets of VECTOR, the latest first."
  (size 0 :type fixnum)
  (pieces '() :type list))

(setf (documentation 'make-octet-pieces 'function) "A new OCTET-PIECES, holding no octets yet."
      (documentation 'octet-pieces-size 'function) "How many octets an OCTET-PIECES holds in all.")

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
  "Read into OCTETS below END the octets of the open file FD from OFFSET, with
pread, which leaves the descriptor's own offset alone; return how many were
read, fewer only where the file ends."
  (let ((filled 0))
    (with-octets-sap (sap octets)
      (loop while (< filled end)
            do (let ((got (sb-alien:alien-funcall
                           (sb-alien:extern-alien "pread" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                                    sb-alien:unsigned-long sb-alien:long))
                           fd (sb-sys:sap+ sap filled) (- end filled) (+ offset filled))))
                 (cond ((plusp got) (incf filled got))
                       ((zerop got) (return))
                       ((= (sb-alien:get-errno) sb-posix:eintr))
                       (t (fail "read" path))))))
    filled))

(defun read-fd-exactly (fd count offset)
  "COUNT octets of the open file FD from OFFSET, read with pread (READ-FD-AT);
NIL when the file ends before them or cannot be read."
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (and (= count (or (ignore-errors (read-fd-at fd octets offset)) 0)) octets)))

(defun advise-will-need (fd offset length)
  "Tell the system that LENGTH octets of the open file FD from OFFSET will be
read soon, so that it starts reading them from the disk while the octets before
are worked on. Changes nothing; a system that takes no advice is fine."
  (sb-alien:alien-funcall (sb-alien:extern-alien "posix_fadvise" (function sb-alien:int sb-alien:int sb-alien:long
                                                                           sb-alien:long sb-alien:int))
                          fd offset length 3)             ; POSIX_FADV_WILLNEED
  nil)

(defun copy-fd-contents (source dest length &key (path "a file") cancel progress)
  "Copy the first LENGTH octets of the open file SOURCE to the open file DEST
at its current offset: by the kernel (COPY-FD-RANGE) where it can, else read
and written here in pieces, read at explicit offsets so that a descriptor
shared with others is not moved. CANCEL, a cons, stops it when its car turns
true (returning NIL); PROGRESS, when given, is called with the octets done so
far. True when done."
  (let ((piece (* 64 1024 1024)) (done 0) (buffer nil))
    (loop while (< done length)
          do (when (and cancel (car cancel)) (return-from copy-fd-contents nil))
             (let* ((n (min piece (- length done)))
                    (at (+ done (copy-fd-range source done dest n))))
               (loop while (< at (+ done n))
                     do (unless buffer
                          (setf buffer (make-array (min n (* 4 1024 1024)) :element-type '(unsigned-byte 8))))
                        (let* ((want (min (length buffer) (- (+ done n) at)))
                               (got (read-fd-at source buffer at :end want :path path)))
                          (when (< got want)
                            (error 'file-system-error :operation "copy" :path path
                                                      :reason "it ended early; it changed while being copied"))
                          (write-octets dest buffer :end got :path path)
                          (incf at got)))
               (incf done n)
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
      (let ((ready (sb-alien:alien-funcall
                    (sb-alien:extern-alien "poll" (function sb-alien:int sb-sys:system-area-pointer
                                                            sb-alien:unsigned-long sb-alien:int))
                    sap count (if seconds (max 0 (min 2000000000 (ceiling (* seconds 1000)))) -1))))
        (cond ((plusp ready)
               (loop for fd in fds for at from 0 by 8
                     unless (zerop (sb-sys:sap-ref-16 sap (+ at 6))) collect fd))
              ((zerop ready) nil)
              ((= (sb-alien:get-errno) sb-posix:eintr) :interrupted)
              (t (fail "wait on" "open files")))))))

(defun make-wake-pipe ()
  "A pipe that wakes a thread waiting on it with WAIT-FOR-FDS from any other
thread (WAKE): a cons of its read end and its write end, neither blocking,
both closed in programs the editor starts."
  (let ((ends (make-array 2 :element-type '(signed-byte 32) :initial-element -1)))
    (sb-sys:with-pinned-objects (ends)
      (unless (zerop (sb-alien:alien-funcall
                      (sb-alien:extern-alien "pipe2" (function sb-alien:int sb-sys:system-area-pointer sb-alien:int))
                      (sb-sys:vector-sap ends) (logior #o4000 #o2000000))) ; O_NONBLOCK, O_CLOEXEC
        (fail "make" "a pipe")))
    (cons (aref ends 0) (aref ends 1))))

(defun wake (pipe)
  "Wake the thread waiting on PIPE (MAKE-WAKE-PIPE). Any thread may call it;
a pipe already full is awake enough."
  (let ((octet (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1)))
    (with-octets-sap (sap octet)
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "write" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer sb-alien:unsigned-long))
       (cdr pipe) sap 1)))
  nil)

(defun drain-wake-pipe (pipe)
  "Empty PIPE (MAKE-WAKE-PIPE) of the wakes written to it."
  (let ((octets (make-array 64 :element-type '(unsigned-byte 8))))
    (with-octets-sap (sap octets)
      (loop while (plusp (sb-alien:alien-funcall
                          (sb-alien:extern-alien "read" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                                  sb-alien:unsigned-long))
                          (car pipe) sap 64)))))
  nil)

(defun close-wake-pipe (pipe)
  "Close both ends of PIPE (MAKE-WAKE-PIPE)."
  (ignore-errors (sb-posix:close (car pipe)))
  (ignore-errors (sb-posix:close (cdr pipe)))
  nil)

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
  (let ((fd (sb-alien:alien-funcall (sb-alien:extern-alien "inotify_init1" (function sb-alien:int sb-alien:int))
                                    (logior #o4000 #o2000000)))) ; IN_NONBLOCK, IN_CLOEXEC
    (and (>= fd 0) fd)))

(defun inotify-watch (fd directory)
  "Have the inotify descriptor FD report +WATCH-EVENTS+ in DIRECTORY (a native
name). Returns the watch descriptor, or NIL when none can be had (no
permission, the system's limit on watches reached)."
  (let ((wd (sb-alien:alien-funcall
             (sb-alien:extern-alien "inotify_add_watch" (function sb-alien:int sb-alien:int sb-alien:c-string
                                                                  sb-alien:unsigned-int))
             fd directory +watch-events+)))
    (and (>= wd 0) wd)))

(defun inotify-unwatch (fd wd)
  "End the watch WD of the inotify descriptor FD; nothing when it ended already."
  (sb-alien:alien-funcall (sb-alien:extern-alien "inotify_rm_watch" (function sb-alien:int sb-alien:int sb-alien:int))
                          fd wd)
  nil)

(defun inotify-events (fd octets)
  "The events waiting on the inotify descriptor FD, read through OCTETS (64 KB
holds hundreds), in order: lists of the watch descriptor (-1 for the queue's
overflow), the event mask and the name of the file in the directory as octets
(NIL for the directory itself). NIL when none wait."
  (let ((events '()))
    (loop
      (let ((got (with-octets-sap (sap octets)
                   (sb-alien:alien-funcall
                    (sb-alien:extern-alien "read" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                            sb-alien:unsigned-long))
                    fd sap (length octets)))))
        (cond ((plusp got)
               (let ((at 0))
                 (loop while (< at got)
                       do (let* ((wd (octets-le octets at 4))
                                 (mask (octets-le octets (+ at 4) 4))
                                 (length (octets-le octets (+ at 12) 4))
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
  "The names of the extended attributes of the open file FD, or NIL."
  (let ((size (sb-alien:alien-funcall
               (sb-alien:extern-alien "flistxattr" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                             sb-alien:unsigned-long))
               fd (sb-sys:int-sap 0) 0)))
    (when (plusp size)
      (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
        (let ((got (with-octets-sap (sap buffer)
                     (sb-alien:alien-funcall
                      (sb-alien:extern-alien "flistxattr" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                                    sb-alien:unsigned-long))
                      fd sap size))))
          (when (plusp got)
            (loop with start = 0
                  for end = (position 0 buffer :start start :end got)
                  while end
                  when (> end start)
                    collect (tuatara.text:octets-string buffer :start start :end end)
                  do (setf start (1+ end)))))))))

(defun fd-xattr-value (fd name)
  "The value of the extended attribute NAME of the open file FD, as octets,
or NIL when it has none or it cannot be read."
  (let ((key (path-octets name)))
    (flet ((get-value (value size)
             (with-octets-sap (key-sap key)
               (sb-alien:alien-funcall
                (sb-alien:extern-alien "fgetxattr" (function sb-alien:long sb-alien:int sb-sys:system-area-pointer
                                                             sb-sys:system-area-pointer sb-alien:unsigned-long))
                fd key-sap value size))))
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
    (ignore-errors
     (let ((value (fd-xattr-value from name)))
       (when value
         ;; an empty value set from a buffer of one octet, none of it given
         (let ((buffer (if (plusp (length value)) value (make-array 1 :element-type '(unsigned-byte 8))))
               (key (path-octets name)))
           (with-octets-sap (sap buffer)
             (with-octets-sap (key-sap key)
               (sb-alien:alien-funcall
                (sb-alien:extern-alien "fsetxattr" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                             sb-sys:system-area-pointer sb-alien:unsigned-long
                                                             sb-alien:int))
                to key-sap sap (length value) 0)))))))))

;;; Directories and unnamed files

(defun sync-directory (directory)
  "Ask the system to put a rename in DIRECTORY on disk, where it can: a file
renamed over another is then there after a power cut too."
  (let ((fd (ignore-errors (sb-posix:open (native directory) (logior sb-posix:o-rdonly sb-posix:o-directory)))))
    (when fd
      (ignore-errors (sb-posix:fsync fd))
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

(defun call-with-folder (directory function operation)
  "Call FUNCTION with a descriptor of the folder DIRECTORY, open for making
entries in it and closed after; refused (WRITING-REFUSED, OPERATION said)
when nothing may be written there, told by where the folder opened really
is, so that nothing swapped for it meanwhile is written in."
  (let ((fd (handler-case (sb-posix:open (string-right-trim "/" (native directory))
                                         ;; O_PATH: a folder you may write in but not list (-wx) too
                                         (logior +o-path+ +o-directory+ +o-cloexec+))
              (sb-posix:syscall-error (e) (fail "open" directory (sb-posix:syscall-errno e))))))
    (unwind-protect (progn (check-writing (string-right-trim "/" (native directory)) operation :fd fd)
                           (funcall function fd))
      (sb-posix:close fd))))

(defun unlink-entry-at (dirfd name)
  "Take away the entry NAME (a string) of the folder open as DIRFD; true when it went."
  (with-octets-sap (sap (terminated-octets (tuatara.text:string-octets name)))
    (zerop (sb-alien:alien-funcall
            (sb-alien:extern-alien "unlinkat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer sb-alien:int))
            dirfd sap 0))))

(defun open-scratch-file (directory)
  "A descriptor for a new file with no name in DIRECTORY, open for reading and
writing. Where the file system has no unnamed files (NFS, FUSE, SMB and others),
a named one removed at once, which comes to the same. Refused
\(WRITING-REFUSED) where nothing may be written, told by the folder opened."
  (unless directory
    (error "This text is not written to a scratch file on disk (the text of an encrypted file): too large to work on in memory"))
  (call-with-folder
   directory
   (lambda (dirfd)
     (or (openat-octets dirfd (tuatara.text:string-octets ".") (logior +o-tmpfile+ sb-posix:o-rdwr +o-cloexec+) #o600)
         (let ((name (native-file-name (scratch-name directory "scratch"))))
           (multiple-value-bind (fd errno)
               (openat-octets dirfd (tuatara.text:string-octets name)
                              (logior sb-posix:o-creat sb-posix:o-excl sb-posix:o-rdwr +o-nofollow+ +o-cloexec+) #o600)
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
  "A descriptor, open for writing, for a new file with no name yet in DIRECTORY
with permissions MODE (less the umask), which NAME-UNNAMED-FILE can name once
it is complete; NIL where the file system or the system cannot, for the caller
to use a named file. A crash before it is named leaves nothing behind.
Refused (WRITING-REFUSED) where nothing may be written, told by the folder
opened."
  (when (proc-fd-available-p)
    (call-with-folder directory
                      (lambda (dirfd)
                        (values (openat-octets dirfd (tuatara.text:string-octets ".")
                                               (logior +o-tmpfile+ sb-posix:o-wronly +o-cloexec+) mode)))
                      "create a file in")))

(defun name-unnamed-file (fd directory tag)
  "Give FD, from OPEN-UNNAMED-FILE, a hidden name with TAG in DIRECTORY (the
same directory it was made in) and return the name; NIL when the system will
not. Refused (WRITING-REFUSED) where nothing may be written, told by the
folder opened."
  (let ((name (scratch-name directory tag)))
    (call-with-folder directory
                      (lambda (dirfd)
                        (and (zerop (sb-alien:alien-funcall
                                     (sb-alien:extern-alien "linkat" (function sb-alien:int sb-alien:int sb-alien:c-string
                                                                               sb-alien:int sb-alien:c-string sb-alien:int))
                                     -100 (format nil "/proc/self/fd/~d" fd) dirfd (native-file-name name) #x400)) ; AT_SYMLINK_FOLLOW
                             name))
                      "name a file in")))

;;; The names of accounts and groups

(defvar *owner-names-lock* (sb-thread:make-mutex :name "owner names")
  "Held while the names of accounts and groups are looked up, which the system
does in one place for every thread.")

(defvar *owner-names* (make-hash-table :test 'equal)
  "The names of accounts and groups looked up so far, by (:USER . UID) and
\(:GROUP . GID); NIL for a number with no name.")

(defun looked-up-name (kind number)
  "The name of the account (KIND :USER) or group (KIND :GROUP) whose number is
NUMBER, looked up once and kept (*OWNER-NAMES*); NIL when it has none."
  (sb-thread:with-mutex (*owner-names-lock*)
    (let ((key (cons kind number)))
      (multiple-value-bind (name found) (gethash key *owner-names*)
        (if found
            name
            (setf (gethash key *owner-names*)
                  (ignore-errors
                   (if (eq kind :user)
                       (let ((entry (sb-posix:getpwuid number))) (and entry (sb-posix:passwd-name entry)))
                       (let ((entry (sb-posix:getgrgid number))) (and entry (sb-posix:group-name entry)))))))))))

(defun owner-name (uid)
  "The name of the account whose number is UID (a file's owner: STAMP-UID), or
NIL when it has none."
  (looked-up-name :user uid))

(defun owner-group-name (gid)
  "The name of the group whose number is GID (a file's group: STAMP-GID), or
NIL when it has none."
  (looked-up-name :group gid))

(defun process-groups ()
  "The supplementary groups of this process, as numbers."
  (let ((n (sb-alien:alien-funcall
            (sb-alien:extern-alien "getgroups" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer))
            0 (sb-sys:int-sap 0))))
    (when (plusp n)
      (let ((buffer (make-array (* 4 n) :element-type '(unsigned-byte 8) :initial-element 0)))
        (with-octets-sap (sap buffer)
          (let ((got (sb-alien:alien-funcall
                      (sb-alien:extern-alien "getgroups" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer))
                      n sap)))
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

(defvar *writing-refusal-function* nil
  "A function saying where the editor writes nothing, or NIL for nowhere:
called with a path (a native name) where something is about to be written,
made, renamed, changed or deleted, and the keywords :DIRFD, a descriptor of
the folder it is in, and :FD, one of the thing itself, by which it is told
where that really is; it returns in words why nothing may be written there,
or NIL when it may. Called with NIL for a path, it returns true when it
refuses any place at all. The evidence folders (*EVIDENCE-FOLDERS*) are
refused so.")

(defun writing-refusal (path &key dirfd fd)
  "Why nothing may be written at PATH (a native name, or a pathname), in
words, or NIL when it may: as *WRITING-REFUSAL-FUNCTION* says, by where PATH
really leads, every link followed; with DIRFD, an open descriptor of the
folder PATH is in, or FD, one of PATH itself, by where that really is now,
whatever names lead there."
  (let ((function *writing-refusal-function*))
    (and function path (funcall function (native path) :dirfd dirfd :fd fd))))

(defun writing-refused-anywhere-p ()
  "True when the editor refuses to write in some place
\(*WRITING-REFUSAL-FUNCTION*), so that what may reach one is looked at."
  (let ((function *writing-refusal-function*))
    (and function (funcall function nil) t)))

(defun check-writing (path operation &key dirfd fd)
  "Signal WRITING-REFUSED for OPERATION (a verb phrase, \"save\") on PATH when
nothing may be written there (WRITING-REFUSAL, DIRFD and FD as it has them)."
  (let ((why (writing-refusal path :dirfd dirfd :fd fd)))
    (when why
      (error 'writing-refused :operation operation :path (native path) :reason why))))

(defvar *writing-told-function* nil
  "A function of words saying something done in writing a file that you
should know of (a file's other names left as they were), or NIL to say
nothing; TELL-WRITING calls it, from any thread.")

(defun tell-writing (words)
  "Say WORDS of a file written, through *WRITING-TOLD-FUNCTION*."
  (let ((function *writing-told-function*))
    (when function (ignore-errors (funcall function words)))))

(defun fd-real-path (fd)
  "Where the file or folder open as FD really is, as a native name with every
link followed (as the system names it in /proc/self/fd); NIL when it does not
say, or the file has no name any more."
  (let ((octets (link-octets +at-fdcwd+ (tuatara.text:string-octets (format nil "/proc/self/fd/~d" fd)))))
    (when (and octets (plusp (length octets)) (= (aref octets 0) 47))
      (let ((name (tuatara.text:octets-string octets)))
        (unless (uiop:string-suffix-p name " (deleted)")
          name)))))

(defun writable-p (path)
  "True when this process may write PATH: the file's contents, or for a
directory, files made and taken away in it; never a place the editor writes
nothing in (WRITING-REFUSAL)."
  (and (ignore-errors (sb-posix:access (native path) sb-posix:w-ok) t)
       (not (writing-refusal path))))

(defvar *cache-directory* nil
  "The folder Tuatara keeps its cache in, in place of ~/.cache/tuatara/
\($XDG_CACHE_HOME/tuatara/): files made again when they are gone, as
compiled packages, previews and pages drawn, copies of files opened outside,
the debug log and stderr.log. NIL for that standard place. A folder on an
encrypted drive keeps what files you open leave there on it: make it
yourself, for you alone (mkdir -m 700); while it is not there, its drive
not mounted, nothing is kept in its place and the work that needs it says
so.")

(defvar *state-directory* nil
  "The folder Tuatara keeps what it remembers between sessions in, in place
of ~/.local/state/tuatara/ ($XDG_STATE_HOME/tuatara/): the files visited
lately, places in them, the session, auto-saved text, the packages fetched.
NIL for that standard place. As *CACHE-DIRECTORY*: made by you, and while it
is not there nothing is kept in its place.")

(defvar *directories-in-memory* '()
  "The settings naming a folder that was not there when it was wanted
\(CHOSEN-DIRECTORY), each with the folder in memory used in its place.")

(defun chosen-directory (setting standard)
  "The folder the setting SETTING (a symbol) names, as a pathname, when it
names one, else STANDARD, made if needed, for you alone. A folder named must
be there already: while it is not, its drive not mounted, one of the same
name in memory ($XDG_RUNTIME_DIR/tuatara/, gone when the computer stops) is
used in its place and noted in *DIRECTORIES-IN-MEMORY*, so that nothing kept
for it lands on another drive. A FILE-SYSTEM-ERROR when there is no such
memory either, or when the folder is one nothing may be written in
\(WRITING-REFUSED)."
  (let ((chosen (symbol-value setting)))
    (if (null chosen)
        (directory-pathname (ensure-private-directory standard))
        (let ((name (directory-name chosen)))
          (check-writing name (format nil "keep files (~(~a~)) in" setting))
          (if (eq :directory (path-kind name))
              (directory-pathname name)
              (let ((memory (uiop:getenv-absolute-directory "XDG_RUNTIME_DIR")))
                (unless memory
                  (error 'file-system-error :operation "keep files in" :path name
                                            :reason (format nil "it is not there (~(~a~)): make it, or mount its drive" setting)))
                (let ((in-memory (ensure-private-directory
                                  (merge-pathnames (format nil "tuatara/~(~a~)/" (first (uiop:split-string (string-trim "*" (symbol-name setting))
                                                                                                        :separator "-")))
                                                   memory))))
                  (unless (assoc setting *directories-in-memory*)
                    (push (list setting name in-memory) *directories-in-memory*))
                  (directory-pathname in-memory))))))))

(defun directories-in-memory-words ()
  "What is said once the init file is read of the folders it named that are
not there (*DIRECTORIES-IN-MEMORY*), a sentence each; NIL when all are."
  (loop for (setting name in-memory) in (reverse *directories-in-memory*)
        collect (format nil "~a (~(~a~)) is not there: kept in memory, ~a, until the computer stops"
                        name setting in-memory)))

(defun cache-directory ()
  "The editor's cache directory, made if needed, for you alone: for scratch
files that cannot go beside the file they serve. Never /tmp, which may be
memory. *CACHE-DIRECTORY* when set (CHOSEN-DIRECTORY)."
  (chosen-directory '*cache-directory* (uiop:xdg-cache-home "tuatara/")))

(defun sync-range (fd offset length &key wait)
  "Start writing the octets of the open file FD from OFFSET, LENGTH of them, out
to the device; with WAIT, also wait until they are there. True when the system
did so; NIL on a file system that cannot (the caller's flush at the end then
does it all)."
  (and (plusp length)
       (zerop (sb-alien:alien-funcall
               (sb-alien:extern-alien "sync_file_range" (function sb-alien:int sb-alien:int (sb-alien:signed 64)
                                                                  (sb-alien:signed 64) sb-alien:unsigned-int))
               fd offset length (if wait 7 2)))))  ; WAIT_BEFORE | WRITE | WAIT_AFTER, or WRITE

(defun drop-cached (fd offset length)
  "Tell the system the octets of FD from OFFSET, LENGTH of them, written out
already, are not needed in memory: they need not crowd out what is."
  (when (plusp length)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "posix_fadvise" (function sb-alien:int sb-alien:int (sb-alien:signed 64)
                                                      (sb-alien:signed 64) sb-alien:int))
     fd offset length 4)))              ; POSIX_FADV_DONTNEED

(defvar *session-log-words* nil
  "What the session writing to stderr.log is (the window, the terminal), as
SESSION-LOG was told; NIL before it is opened.")

(defun session-log (what)
  "A descriptor open for writing on stderr.log in the cache directory, for a
session's standard error, kept as the debug log is (TUATARA.RUNTIME:OPEN-ROTATED-LOG):
for you alone, a new one begun unless another session writes to it, the logs
of the sessions before kept as stderr.log.1, the one just before, to
stderr.log.3, so that what a session that ended badly wrote is still there
once the next has begun. A line first says WHAT the session is (the window,
the terminal), its process and when it began. NIL when the log cannot be made."
  (setf *session-log-words* what)
  (let ((directory (ignore-errors (cache-directory))))
    (when directory
      (let ((fd (ignore-errors (tuatara.runtime:open-rotated-log (native (merge-pathnames "stderr.log" directory)) :rotate t))))
        (when fd
          (let ((octets (sb-ext:string-to-octets
                         (format nil "Tuatara ~a on SBCL ~a, ~a, process ~d, began ~a~%"
                                 (tuatara:version) (lisp-implementation-version) what (sb-posix:getpid)
                                 (tuatara.dates:format-time "%F %T" (get-universal-time)))
                         :external-format :utf-8)))
            (with-octets-sap (sap octets)
              (ignore-errors (sb-posix:write fd sap (length octets)))))
          fd)))))

(defun move-session-log ()
  "When standard error goes to stderr.log in the standard cache folder and
*CACHE-DIRECTORY* names another, send it to a new stderr.log there instead,
and take the standard folder's logs away when no other session writes them,
and the folder when it is left empty: what the session wrote before its init
file was read was only the line saying what it is. Called once the init file
is read."
  (let ((standard (native (uiop:xdg-cache-home "tuatara/stderr.log"))))
    (when (and *cache-directory* *session-log-words*
               (equal (ignore-errors (sb-posix:readlink "/proc/self/fd/2")) standard))
      (let ((fd (session-log *session-log-words*)))
        (when fd
          (sb-posix:dup2 fd 2)
          (sb-posix:close fd)
          (unless (tuatara.runtime:log-in-use-p standard)
            (dolist (n '("" ".1" ".2" ".3"))
              (ignore-errors (sb-posix:unlink (concatenate 'string standard n))))
            ;; the folder too, when nothing else is in it
            (ignore-errors (sb-posix:rmdir (native (uiop:xdg-cache-home "tuatara/"))))))))))

(defun state-directory ()
  "The editor's state directory, made if needed, for you alone: what it keeps
between one session and the next, such as the files visited lately.
*STATE-DIRECTORY* when set (CHOSEN-DIRECTORY), else $XDG_STATE_HOME/tuatara/,
or ~/.local/state/tuatara/."
  (let ((home (uiop:getenv-absolute-directory "XDG_STATE_HOME")))
    (chosen-directory '*state-directory*
                      (merge-pathnames "tuatara/" (or home (merge-pathnames ".local/state/" (user-homedir-pathname)))))))

(defun replace-file (path contents &key (mode #o666))
  "Write CONTENTS, a string (in UTF-8) or octets, as the whole of the file at
PATH, its name as the system has it ([Content_Types].xml too), made with MODE
less the umask when it is new: into a new file beside it, renamed over it
once written and put on disk, so that the file is always whole, the old
contents or the new, after a power cut too; the new file taken away when it
cannot be written whole (a full disk). Refused (WRITING-REFUSED) where
nothing may be written, told by the folder opened, in which the new file is
made and renamed."
  (let ((name (native path))
        (octets (if (stringp contents) (tuatara.text:string-octets contents) contents)))
    (call-with-parent
     name
     (lambda (dirfd own)
       (let ((temporary (format nil "~a.~d.~a.new" (native-file-name name) (sb-posix:getpid) (random-name))))
         (multiple-value-bind (fd errno)
             (openat-octets dirfd (tuatara.text:string-octets temporary)
                            (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl +o-nofollow+ +o-cloexec+) mode)
           (unless fd (fail "write" name errno))
           (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (unlink-entry-at dirfd temporary)))))
             (unwind-protect (progn (write-octets fd octets :path name)
                                    (flush-fd fd name))
               (sb-posix:close fd))
             (let ((old (terminated-octets (tuatara.text:string-octets temporary))) (new (terminated-octets own)))
               (with-octets-sap (old-sap old)
                 (with-octets-sap (new-sap new)
                   (unless (zerop (sb-alien:alien-funcall
                                   (sb-alien:extern-alien "renameat" (function sb-alien:int sb-alien:int sb-sys:system-area-pointer
                                                                              sb-alien:int sb-sys:system-area-pointer))
                                   dirfd old-sap dirfd new-sap))
                     (fail "write" name)))))
             (sync-directory (parent-path name))))))
     :writing "write")
    path))

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
\(WRITING-REFUSED) where nothing may be written, told by the folder opened,
in which the file is made."
  (let ((name (native path))
        (octets (if (stringp contents) (tuatara.text:string-octets contents) (coerce contents 'tuatara.text:octets))))
    (call-with-parent
     name
     (lambda (dirfd own)
       (multiple-value-bind (fd errno)
           (openat-octets dirfd own (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl +o-nofollow+ +o-cloexec+) mode)
         (unless fd (fail "write" name errno))
         (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (unlink-entry-at dirfd (native-file-name name))))))
           (unwind-protect (progn (write-octets fd octets :end (or end (length octets)) :path name)
                                  (when sync (sb-posix:fsync fd)))
             (sb-posix:close fd)))))
     :writing "write")
    path))

(defun call-with-state-lock (path function &key (wait 0.1))
  "Call FUNCTION, of no arguments, holding the lock of the file at PATH, a
file the sessions side by side keep together (the files visited lately, the
places in files): a lock file beside it, its name with .lock after it, so
that one session at a time reads it, changes it and writes it back, and
none loses what another wrote meanwhile. Returns what FUNCTION returns; NIL,
FUNCTION not called, when another holds the lock for more than WAIT seconds
or the lock file cannot be made, so that the editor is never held up; nor
in a place nothing may be written in (WRITING-REFUSAL), told by the folder
opened."
  (let ((fd (ignore-errors
             (call-with-parent (concatenate 'string (native path) ".lock")
                               (lambda (dirfd own)
                                 (values (openat-octets dirfd own (logior sb-posix:o-rdwr sb-posix:o-creat +o-nofollow+ +o-cloexec+) #o600)))
                               :writing "lock"))))
    (when fd
      (unwind-protect
           (when (tuatara.runtime:lock-fd fd :wait wait)
             (funcall function))
        (sb-posix:close fd)))))

(defvar *saved-program* nil
  "The path of the saved Tuatara program this process runs as, or NIL when it
runs from the sources inside SBCL: what RUN-PROGRAM-AGAIN runs, and what a
copy of Tuatara started for a moment is started as.")

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

(defun run-program-again (arguments)
  "Run this program again in place of this process: *SAVED-PROGRAM* with
ARGUMENTS, strings, after its name, in the environment as it is now; names
that are not UTF-8 among them are handed on as the octets they were read
from. Returns only when that fails, or at once when Tuatara runs from the
sources."
  (unless *saved-program* (return-from run-program-again nil))
  (let ((argv (argument-vector (cons (first sb-ext:*posix-argv*) arguments))))
    (unwind-protect
         (sb-alien:alien-funcall
          (sb-alien:extern-alien "execv" (function sb-alien:int sb-alien:c-string (* (* sb-alien:char))))
          *saved-program* argv)
      (free-argument-vector argv))))
