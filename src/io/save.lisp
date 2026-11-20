;;;; src/io/save.lisp --- Saving files: by rename, in place, or in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A new file with no name is written beside the old one, flushed to
;;;; disk, given the old one's owner, permissions and extended attributes
;;;; (SELinux label, ACLs), named, renamed over it, and the directory flushed;
;;;; the file on disk is always the old one or the new one, even after a power
;;;; cut, and a crash while writing leaves nothing behind. Where the file system
;;;; has no unnamed files, a hidden named one serves. A symlink is followed and
;;;; kept. A file with several names (hard links), one whose owner or group this
;;;; user cannot give a new file, a directory where no file can be made, a
;;;; rename the file system refuses, and the pseudo file systems are written in
;;;; place instead, after a backup copy that is kept if the write fails. A save
;;;; never overwrites a file changed since it was read without being told to.
;;;; Large buffers are saved in the background. With *SAVE-IN-PLACE*, on a
;;;; file system that cannot share blocks, a large file the buffer reads where
;;;; it is has only its changes written, in place, after a journal of what they
;;;; overwrite, when that writes less (in-place.lisp).
;;;;
;;;; Copying octets that come from the file (the unchanged parts of a large
;;;; buffer) is left to the kernel (copy_file_range), which shares blocks
;;;; instead where the file system can and the offsets allow; the rest is
;;;; written.

(in-package #:tuatara.io)

(define-condition file-changed-on-disk (error)
  ((path :initarg :path :reader file-changed-on-disk-path :documentation "The file."))
  (:report (lambda (c stream)
             (format stream "~a changed on disk since it was read or saved" (file-changed-on-disk-path c))))
  (:documentation "A save found the file changed by someone else since the buffer read or saved it."))

(setf (documentation 'file-changed-on-disk-path 'function) "The file a FILE-CHANGED-ON-DISK concerns.")

(defun check-unchanged (target existing)
  "Signal FILE-CHANGED-ON-DISK when the file at TARGET, a link there not
followed, is no longer the one EXISTING (a STAMP) describes, or, EXISTING
being NIL, when a file has come there since: called just before a save
writes, so that a program changing the file while the save was under way
does not have its change replaced. A file deleted since is written again."
  (let ((now (path-stamp target :follow nil :failure nil)))
    (when (and now (not (and existing (same-content-p existing now))))
      (error 'file-changed-on-disk :path target))))

(defun resolve-links (path)
  "PATH with symbolic links followed to the file they name, even one not there
yet, so that saving writes that file and the link stays a link."
  (let ((path (expand-path path)))
    (loop repeat 40
          do (let ((stamp (path-stamp path :follow nil :failure nil)))
               (unless (and stamp (eq (stamp-kind stamp) :symlink))
                 (return))
               (let ((target (or (symlink-target path) (return))))
                 ;; a link's target is taken literally: a ~ there is a directory's name
                 (setf path (expand-path target :directory (parent-path path) :tilde nil)))))
    path))

(defun directory-class (directory)
  "FILE-SYSTEM-CLASS of the file system holding DIRECTORY."
  (file-system-class (path-file-system directory)))

(defconstant +block-size+ 4096
  "The block size file systems share blocks in: copies of whole blocks at the
same place within a block in both files can share them.")

(defun write-rope (fd rope &key (path "a file") cancel progress kept)
  "Write ROPE's octets to the file open as file descriptor (FD), from offset 0
on. What comes from mapped files is copied by the kernel where it can, the
rest written. Where a stretch from a file sits at the same place within a
block as where it goes (after edits that kept the length, or none), it is
copied up to the next block boundary first, so that file systems that share
blocks share all the whole ones after it.
The octets go out to the device as they are written, *WRITE-OUT-WINDOW* at a
time, and leave memory once there. So a slow device (a USB drive) holds a
large save back as it goes rather than all at the end, and memory does not
fill with what waits for it. PROGRESS is called with the octets on the device
and the total.
KEPT, for a file that holds much of ROPE already, is a function of a stretch
from a mapped file (its MAPPING, the OFFSET in it, its LENGTH and the POSITION
it goes to). It gives whether the file holds its first octets already where
they go, and how many from there are alike. Those it holds are passed over,
not written. Returns true, or NIL when CANCEL's car turned true first."
  (let ((done 0) (total (tuatara.text:rope-octet-length rope))
        (out (make-write-out fd 0)))
    (flet ((move-on ()
             (let ((there (write-out-step out done)))
               (when progress (funcall progress there total)))))
      (block write
        (tuatara.text:map-rope-sources
         (lambda (kind a b c)
           (when (and cancel (car cancel)) (return-from write nil))
           (ecase kind
             (:memory (write-octets fd a :start b :end c :path path) (incf done (- c b)))
             (:file (let ((mapping a) (offset b) (length c))
                      (loop while (plusp length)
                            do (when (and cancel (car cancel)) (return-from write nil))
                               (multiple-value-bind (in-place alike)
                                   (if kept (funcall kept mapping offset length done) (values nil length))
                                 (if in-place
                                     (progn (incf offset alike) (decf length alike) (incf done alike)
                                            (sb-posix:lseek fd done sb-posix:seek-set))
                                     (multiple-value-bind (source at end) (tuatara.text:mapping-source mapping offset)
                                       (let* ((misaligned (mod at +block-size+))
                                              (n (min (- end offset) alike
                                                      (if (and (plusp misaligned)
                                                               (= misaligned (mod done +block-size+)))
                                                          (min length (- +block-size+ misaligned))
                                                          (min length *write-out-window*))))
                                              (copied (progn (tuatara.text:note-read mapping offset (+ offset n))
                                                             (copy-fd-range source at fd n))))
                                         (when (< copied n)
                                           (write-sap fd
                                                      (sb-sys:sap+ (tuatara.text:mapping-sap mapping) (+ offset copied))
                                                      (- n copied) :path path))
                                         (incf offset n) (decf length n) (incf done n)))))
                               (move-on)))))
           (move-on))
         rope)
        t))))

(defun flush-fd (fd path)
  "Ask the system to put the file open as file descriptor (FD) on disk; file
systems that cannot are let be."
  (handler-case (sb-posix:fsync fd)
    (sb-posix:syscall-error (e)
      (unless (member (sb-posix:syscall-errno e) (list sb-posix:einval sb-posix:erofs sb-posix:enosys))
        (fail "flush" path (sb-posix:syscall-errno e))))))

(defun write-by-rename (rope target existing &key cancel progress nofollow mode)
  "Save ROPE as TARGET by writing a new file beside it and renaming it over
TARGET (this file's header). With NOFOLLOW, a symbolic link or anything but a
file found at TARGET just before the rename is refused. MODE, a function of
the permissions the new file has, gives those it gets instead. Returns :DONE,
:STOPPED when cancelled (TARGET untouched), or :REFUSED when the file system
refuses the rename or the naming, for the caller to write in place."
  (let* ((directory (parent-path target))
         (made-mode (if existing #o600 #o666))
         (fd (open-unnamed-file directory made-mode))
         (temp nil))
    (unless fd
      (setf temp (scratch-name directory "save")
            fd (call-with-directory directory
                                    (lambda (dirfd)
                                      (multiple-value-bind (fd errno)
                                          (open-new-entry dirfd (native-file-name temp) made-mode)
                                        (or fd (fail "create a file in" directory errno))))
                                    "save in")))
    (flet ((abandon ()
             (when fd
               (tuatara.runtime:close-fd-quietly fd "the new file for ~a" target)
               (setf fd nil))
             (when temp (tuatara.runtime:ignoring-errors-reported ("taking away ~a" temp) (sb-posix:unlink temp)))))
      (handler-bind ((error (lambda (e) (declare (ignore e)) (abandon))))
        (unless (write-rope fd rope :path target :cancel cancel :progress progress)
          (abandon)
          (return-from write-by-rename :stopped))
        (flush-fd fd target)
        (when progress (funcall progress (tuatara.text:rope-octet-length rope) (tuatara.text:rope-octet-length rope)))
        (when existing
          ;; the owner first: changing it may clear set-id bits the mode then restores
          (handler-case (sb-posix:fchown fd (stamp-uid existing) (stamp-gid existing)) (sb-posix:syscall-error () nil))
          ;; never yours in its place without a word: someone else's file is not taken over
          (let ((now (fd-stamp fd target)))
            (unless (and (= (stamp-uid now) (stamp-uid existing)) (= (stamp-gid now) (stamp-gid existing)))
              (error 'file-system-error :operation "save" :path target
                                        :reason (format nil "it belongs to user ~d and group ~d, and the file of its ~
                                                             own it would be saved as could not be given to them"
                                                        (stamp-uid existing) (stamp-gid existing)))))
          (handler-case (sb-posix:fchmod fd (logand (stamp-mode existing) #o7777)) (sb-posix:syscall-error () nil))
          (let ((source (tuatara.runtime:ignoring-errors-reported ("opening ~a to copy its extended attributes" target)
                          (open-examined +at-fdcwd+ (tuatara.text:string-octets (native target))
                                         (logior sb-posix:o-rdonly sb-posix:o-nonblock (if nofollow +o-nofollow+ 0))
                                         target))))
            (when source
              (unwind-protect (copy-xattrs source fd) (sb-posix:close source)))))
        (when mode
          (sb-posix:fchmod fd (funcall mode (logand (stamp-mode (fd-stamp fd target)) #o7777))))
        (unless temp
          (setf temp (name-unnamed-file fd directory "save"))
          (unless temp
            (abandon)
            (return-from write-by-rename :refused)))
        (sb-posix:close fd)
        (setf fd nil)
        (when nofollow
          (let ((now (path-stamp target :follow nil)))
            (when (and now (not (eq (stamp-kind now) :regular)))
              (error 'file-system-error :operation "save to" :path target
                                        :reason (format nil "it is ~a now" (kind-words (stamp-kind now)))))))
        (check-unchanged target existing)
        ;; renamed within the directory held open, looked at again where it really is
        (call-with-parent
         target
         (lambda (dirfd own)
           (with-octets-sap (old (terminated-octets (tuatara.text:string-octets (native-file-name temp))))
             (with-octets-sap (new (terminated-octets own))
               (multiple-value-bind (result errno) (rename-entry dirfd old dirfd new 0)
                 (unless (zerop result)
                   (if (member errno
                               (list sb-posix:exdev sb-posix:eperm sb-posix:eacces sb-posix:ebusy sb-posix:eopnotsupp
                                           sb-posix:etxtbsy))
                       (progn (abandon) (return-from write-by-rename :refused))
                       (fail "rename a new copy over" target errno)))))))
         :writing "save")
        (sync-directory directory)
        :done))))

(defun backup-copy (target existing &key cancel)
  "Copy TARGET, described by EXISTING, to a backup beside it (or in the cache
directory when its own cannot hold files), flushed to disk. Returns the backup's
descriptor and path, or NIL when CANCEL's car turned true first."
  (let* ((directory (parent-path target))
         (place (if (writable-p directory) directory (cache-directory)))
         ;; a random part: two saves at once, of files of one name, have a backup each
         (path (scratch-name place (concatenate 'string "backup-" (fit-name (native-file-name target) :most 200))))
         ;; made anew, never through a link put there: one left by a crash taken away first
         (fd (flet ((make ()
                      (sb-posix:open path (logior sb-posix:o-rdwr sb-posix:o-creat sb-posix:o-excl +o-nofollow+
                                                  +o-cloexec+)
                                     #o600)))
               (handler-case (make)
                 (sb-posix:syscall-error (e)
                   (if (= (sb-posix:syscall-errno e) sb-posix:eexist)
                       (handler-case (progn (sb-posix:unlink path) (make))
                         (sb-posix:syscall-error (e) (fail "create a backup of" target (sb-posix:syscall-errno e))))
                       (fail "create a backup of" target (sb-posix:syscall-errno e)))))))
         (done nil))
    (unwind-protect
         (let ((source (handler-case (sb-posix:open (native target) sb-posix:o-rdonly)
                         (sb-posix:syscall-error (e) (fail "read" target (sb-posix:syscall-errno e))))))
           (unwind-protect
                (when (or (clone-fd source fd)
                          (copy-fd-contents source fd (stamp-size existing) :path target :cancel cancel))
                  (flush-fd fd path)
                  (setf done t))
             (sb-posix:close source)))
      (unless done
        (tuatara.runtime:close-fd-quietly fd "the backup of ~a" target)
        (tuatara.runtime:ignoring-errors-reported ("taking away the backup ~a" path) (sb-posix:unlink path))))
    (and done (values fd path))))

(defun write-in-place (rope target existing &key mappings (backup t) (sync t) whole cancel progress nofollow mode
                                                (guard t))
  "Overwrite TARGET with ROPE where it is, for a file that must keep its inode
or its owner; with NOFOLLOW, never through a symbolic link at TARGET. MODE, a
function of the file's permissions, gives those it gets instead. With GUARD,
a file no longer the one EXISTING describes is refused just before it is
opened (CHECK-UNCHANGED). With BACKUP, a
copy of the old content is made first and MAPPINGS still reading TARGET's octets
move onto it, so that nothing reads the file while it is rewritten; the copy is
removed once the new content is on disk, and kept, with its name in the error,
when the write fails. WHOLE writes the text with one call, as the files of
pseudo file systems want. CANCEL is heeded until the file itself is
touched. Returns :DONE, or :STOPPED."
  (when existing
    ;; before any backup is made: a file this user cannot write is refused at once
    (handler-case (sb-posix:access (native target) sb-posix:w-ok)
      (sb-posix:syscall-error (e) (fail "write" target (sb-posix:syscall-errno e)))))
  (when (and cancel (car cancel))
    (return-from write-in-place :stopped))
  (multiple-value-bind (backup-fd backup-path)
      (if (and backup existing) (backup-copy target existing :cancel cancel) (values nil nil))
    (when (and backup existing (null backup-fd))
      (return-from write-in-place :stopped))
    (flet ((drop-backup ()
             (when backup-fd
               (tuatara.runtime:close-fd-quietly backup-fd "the backup of ~a" target)
               (tuatara.runtime:ignoring-errors-reported ("taking away the backup ~a" backup-path)
                 (sb-posix:unlink backup-path))
               (setf backup-fd nil))))
      ;; the last moment a stop leaves the file as it was
      (when (and cancel (car cancel))
        (drop-backup)
        (return-from write-in-place :stopped))
      (when backup-fd
        (dolist (mapping mappings)
          (let ((fd (sb-posix:dup backup-fd)))
            (unless (tuatara.text:retarget-mapping mapping fd)
              (sb-posix:close fd))))
        ;; they read the backup now: a lease on the file would only hold up this write
        (give-up-leases (lambda (lease) (member (lease-target lease) mappings))))
      (when guard
        (handler-bind ((file-changed-on-disk (lambda (e) (declare (ignore e)) (drop-backup))))
          (check-unchanged target existing)))
      (let* ((fd (handler-case (sb-posix:open (native target)
                                             (logior (cond ((not existing)
                                                            (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl))
                                                           (backup sb-posix:o-wronly)
                                                           (t (logior sb-posix:o-wronly sb-posix:o-trunc)))
                                                     sb-posix:o-noctty (if nofollow +o-nofollow+ 0))
                                             #o666)
                  (sb-posix:syscall-error (e)
                    ;; nothing was written: the backup is not needed
                    (drop-backup)
                    (fail "open for writing" target (sb-posix:syscall-errno e)))))
             (made (and (not existing) (tuatara.runtime:ignoring-errors-reported ("examining ~a" target)
                                         (fd-stamp fd target)))))
        (handler-case
            (tuatara.runtime:reporting-errors ("writing ~a" target)
              (unwind-protect
                   (progn
                     ;; where the file opened really is, whatever names led to it meanwhile
                     (check-writing target "write" :fd fd)
                     (if whole
                         (write-octets fd (tuatara.text:rope-octets rope) :path target)
                         (write-rope fd rope :path target :progress progress))
                     (when (and existing backup)
                       (handler-case (sb-posix:ftruncate fd (tuatara.text:rope-octet-length rope))
                         (sb-posix:syscall-error (e) (fail "shorten" target (sb-posix:syscall-errno e)))))
                     (when mode
                       (sb-posix:fchmod fd (funcall mode (logand (stamp-mode (fd-stamp fd target)) #o7777))))
                     (when sync (flush-fd fd target))
                     (when progress
                       (funcall progress (tuatara.text:rope-octet-length rope) (tuatara.text:rope-octet-length rope))))
                (sb-posix:close fd)))
          (writing-refused (e)
            (drop-backup)
            ;; the file made here for this write, empty, is not left behind: that very file alone
            (unless existing
              (let ((there (path-stamp target :follow nil :failure nil)))
                (when (and made (same-stamped-file-p made there))
                  (tuatara.runtime:ignoring-errors-reported ("taking away ~a" target)
                    (sb-posix:unlink (native target))))))
            (error e))
          (error (e)
            (when backup-fd (tuatara.runtime:close-fd-quietly backup-fd "the backup of ~a" target))
            (error 'file-system-error :operation "write" :path target
                                      :reason (format nil "~a~@[; the old content is kept in ~a~]"
                                                      (if (typep e 'file-system-error) (file-system-error-reason e) e)
                                                      backup-path)))))
      (drop-backup)
      :done)))

(defun write-text-file (rope path &key (expected :any) mappings cancel progress (follow-links t) mode)
  "Save ROPE as the file at PATH (this file's header). EXPECTED is the STAMP the
buffer last read or wrote, NIL for a file that was not there, or :ANY to skip
the check; a file changed since signals FILE-CHANGED-ON-DISK (one deleted since
is simply written again), after a save in place of it cut short is put right
\(RECOVER-BEFORE-SAVING). MAPPINGS are those still reading PATH's octets, moved
to a copy before any write in place. With FOLLOW-LINKS NIL, PATH itself is
written: a symbolic link there, found first or put there while the file is
written, is refused, never followed. MODE, a function of the permissions the
file would have (a number, as #o644), gives those it gets instead. Returns the
new stamp, the file written (a symlink's target), and true when only the
changes were written, in place (WRITE-CHANGES-IN-PLACE), MAPPINGS still
reading the file where it is unchanged; or :STOPPED when CANCEL stopped it
with the file untouched. Refused (WRITING-REFUSED) where nothing
may be written, looked at again on the file opened when it is written in
place. While some place is refused (WRITING-REFUSED-ANYWHERE-P), a file with
other names is never written in place, for one of them may be in such a
place: it is saved as a file of its own, its other names keeping what they
held, and that is told (TELL-WRITING)."
  (let* ((target (if follow-links (resolve-links path) (expand-path path)))
         (existing (let ((stamp (progn (check-writing target "save")
                                       (path-stamp target :follow follow-links))))
                     ;; a save in place cut short is put right first, so that the file is the one last read
                     (or (recover-before-saving target stamp) stamp)))
         (directory (parent-path target))
         (class (directory-class directory))
         (in-place nil))
    ;; a pseudo file changes as it is read: there is nothing to guard
    (unless (or (eq expected :any) (eq class :pseudo) (null existing)
                (and expected (same-content-p expected existing)))
      (error 'file-changed-on-disk :path target))
    (when (and existing (not (eq (stamp-kind existing) :regular)))
      (error 'file-system-error :operation "save to" :path target
                                :reason (format nil "it is ~a" (kind-words (stamp-kind existing)))))
    (unless (path-stamp directory)
      (error 'file-system-error :operation "save" :path target :reason "its directory does not exist"))
    (let* ((names (and existing (stamp-nlink existing)))
           ;; another name of it may be evidence: written as a file of its own, never in place
           (apart (and names (> names 1) (not (eq class :pseudo)) (writing-refused-anywhere-p)))
           (result
             (cond ((eq class :pseudo)
                    (write-in-place rope target existing :backup nil :sync nil :whole t :guard nil
                                                         :nofollow (not follow-links) :mode mode))
                   ((and (not apart)
                         (or (and existing (> (stamp-nlink existing) 1))
                             (and existing (not (can-give-owner-p existing)))
                             (not (writable-p directory))))
                    (write-in-place rope target existing :mappings mappings :cancel cancel :progress progress
                                                         :nofollow (not follow-links) :mode mode))
                   (t (when (and apart (not (writable-p directory)))
                        (error 'file-system-error :operation "save" :path target
                                                  :reason (format nil "it has ~d names, so it is saved as a file of ~
                                                                       its own beside them, and ~a may not be written ~
                                                                       in"
                                                                  names (native directory))))
                      (let* ((plan (and *ext4-save-in-place* mappings existing (not apart) (not mode) (eq class :local)
                                        (not (shares-blocks-p directory))
                                        (in-place-plan rope existing mappings)))
                             (r (or (and plan
                                         (setf in-place (write-changes-in-place rope target existing plan
                                                                                :mappings mappings :cancel cancel
                                                                                :progress progress)))
                                    (write-by-rename rope target existing :cancel cancel :progress progress
                                                                          :nofollow (not follow-links) :mode mode))))
                        (cond ((and (eq r :refused) apart)
                               (error 'writing-refused :operation "save" :path target
                                                       :reason (format nil "it has ~d names, one may be in an ~
                                                                            evidence directory, and it cannot be ~
                                                                            saved as a file of its own here" names)))
                              ((eq r :refused)
                               (write-in-place rope target existing :mappings mappings :cancel cancel :progress progress
                                                                    :nofollow (not follow-links) :mode mode))
                              (t (when (and apart (eq r :done))
                                   (tell-writing (format nil "~a had ~d names: saved as a file of its own, the others ~
                                                              keep what they held, as one of them may be evidence"
                                                         target names)))
                                 r)))))))
      (if (eq result :stopped)
          :stopped
          (values (path-stamp target :follow follow-links) target (eq in-place :done))))))

(defun protected-mappings (buffer)
  "The mappings of BUFFER that read its file's own octets, which a write in place must move first."
  (let ((mapping (tuatara.buffer:buffer-mapping buffer)))
    (and mapping (member (file-isolation buffer) '(:direct :pending :leased)) (list mapping))))

(defun record-save (buffer path stamp content rope &optional in-place)
  "Note that BUFFER's text of content number CONTENT, ROPE, is saved as PATH,
now at STAMP. A buffer that read its old file directly no longer does (the old
file has no name now, or the buffer reads the backup), so it counts as a
snapshot; unless only the changes were written, IN-PLACE: it then reads the
file where nothing changed, kept as before."
  (setf (tuatara.buffer:buffer-file buffer) path
        (buffer-file-stamp buffer) stamp
        (tuatara.buffer:buffer-local buffer :disk-state) nil)
  (note-file-tail buffer rope)
  (tuatara.buffer:buffer-mark-saved buffer content)
  (when (and (not in-place) (member (file-isolation buffer) '(:direct :pending :leased)))
    (stop-snapshots-of buffer)
    (give-up-leases (lambda (lease) (eq (lease-buffer lease) buffer)))
    (setf (tuatara.buffer:buffer-local buffer :file-isolation) :snapshot)))

(defun check-savable (buffer path)
  "Signal an error unless BUFFER can be saved as PATH: it has a file or PATH,
and its text is still what was read (not stale)."
  (unless path (refuse "Buffer ~a has no file to save to." (tuatara.buffer:buffer-name buffer)))
  (when (eq (file-disk-state buffer) :stale)
    (refuse "Buffer ~a cannot be saved: its file was rewritten on disk while the buffer was shown from it, ~
so its text is no longer what was read. Revert it to read the file again."
           (tuatara.buffer:buffer-name buffer))))

(defun save-expected (buffer path force)
  "What a save of BUFFER as PATH checks the file on disk against: the stamp the
buffer read or wrote, or :ANY when FORCE or when PATH is another file."
  (if (or force (not (equal path (tuatara.buffer:buffer-file buffer))))
      :any
      (buffer-file-stamp buffer)))

(defgeneric host-save-transform (host buffer path)
  (:documentation "The transform of BUFFER's text for its save as PATH, asked on the editor's
thread as the save begins: NIL to write its text as it is, or a function of its
text (a rope) returning what is written instead (octets or a rope), called on
the thread that writes; an encrypted file's text encrypted before anything
touches the disk. It may signal an error to refuse the save.")
  (:method ((host tuatara.runtime:host) buffer path)
    (declare (ignore buffer path))
    nil))

(defun transformed-text (transform rope)
  "ROPE as TRANSFORM has it written, a rope; ROPE itself without TRANSFORM."
  (if transform
      (let ((data (funcall transform rope)))
        (if (tuatara.text:rope-p data) data (tuatara.text:octets->rope (coerce data 'tuatara.text:octets))))
      rope))

(defun save-buffer-file (buffer &key (path (tuatara.buffer:buffer-file buffer)) force)
  "Save BUFFER as PATH (its file by default) now, safely (WRITE-TEXT-FILE); a file
changed on disk since the buffer read or saved it signals FILE-CHANGED-ON-DISK
unless FORCE. Returns the file written."
  (check-savable buffer path)
  (let* ((path (expand-path path))
         (version (tuatara.buffer:buffer-version buffer))
         (content (tuatara.buffer:buffer-content-number buffer))
         (written (transformed-text (host-save-transform tuatara.runtime:*host* buffer path)
                                    (tuatara.text:version-rope version))))
    (multiple-value-bind (stamp target in-place)
        (write-text-file written path :expected (save-expected buffer path force) :mappings (protected-mappings buffer))
      (record-save buffer path stamp content written in-place)
      target)))

;;; Saving in the background

;; a save of the buffer is a job in the background (TUATARA.RUNTIME:JOB)
(define-record (save-job (:include tuatara.runtime:job) (:constructor %make-save-job) (:copier nil))
  "A save of BUFFER's text at VERSION (of content number CONTENT) as PATH
running in the background, a job (TUATARA.RUNTIME:JOB): DONE of TOTAL octets
written; STAMP and TARGET, the file written, once done, and IN-PLACE when
only the changes were written."
  (path nil :documentation "The file a save job writes.")
  (version nil :documentation "The version of the buffer's text a save job writes.")
  (content nil :documentation "The content number of the buffer a save job writes.")
  (done 0 :documentation "How many octets a save job has written so far.")
  (total 0 :documentation "The octets a save job writes.")
  (stamp nil :documentation "The stamp of the file a finished save job wrote.")
  (target nil :documentation "The file a finished save job wrote: a symlink's target.")
  (written nil :documentation "The text a save job writes, as transformed for the file, or NIL until it is worked out.")
  (in-place nil :documentation "True when a finished save job wrote only the changes, in place."))

(setf (documentation 'save-job-buffer 'function) "The buffer a save job saves."
      (documentation 'save-job-status 'function) "A save job's status: :RUNNING, :DONE, :STOPPED or :FAILED."
      (documentation 'save-job-error 'function) "Why a save job failed, in words."
      (documentation 'save-job-started 'function) "The internal real time a save job began."
      (documentation 'save-job-thread 'function) "The thread a save job runs on.")

(defun saves (&key running)
  "The saves in the background, the oldest first: running, or ended and not yet
taken in; with RUNNING those running alone."
  (remove-if-not (lambda (job) (and (save-job-p job) (or (not running) (eq (save-job-status job) :running))))
                 tuatara.runtime:*jobs*))

(defun save-of (buffer)
  "The save of BUFFER running in the background, or NIL."
  (find buffer (saves :running t) :key #'save-job-buffer))

(defun changes-unsaved-p (buffer)
  "True when BUFFER has changes neither saved nor being saved: its text is not
the one last saved, nor the one a save running in the background writes."
  (and (tuatara.buffer:buffer-modified-p buffer)
       (let ((save (save-of buffer)))
         (not (and save (= (save-job-content save) (tuatara.buffer:buffer-content-number buffer)))))))

(defun saving-p (buffer)
  "True while a save of BUFFER runs or has ended without being taken in: the
file on disk is then the editor's own doing."
  (and (find buffer (saves) :key #'save-job-buffer) t))

(defun saves-running ()
  "The saves running in the background."
  (saves :running t))

(defun save-progress (job)
  "How much of JOB is done, from 0 to 100."
  (if (zerop (save-job-total job)) 100 (min 100 (floor (* 100 (save-job-done job)) (save-job-total job)))))

(defmethod tuatara.runtime:job-percent ((job save-job)) (min 99 (save-progress job)))
(tuatara.runtime:define-job-words save-job
  :doing "Saving"
  :description (format nil
                       "Saving ~a to ~a"
                       (tuatara.buffer:buffer-name (save-job-buffer job))
                       (native (save-job-path job)))
  :verb (format nil "save ~a" (tuatara.buffer:buffer-name (save-job-buffer job)))
  :left-words "the file is as it was"
  :at-exit :wait)
(defmethod tuatara.runtime:job-consequence ((job save-job)) "The file stays as it was")
(defmethod tuatara.runtime:job-holds-queue-p ((job save-job)) nil)
(defmethod tuatara.runtime:job-shows-processing-p ((job save-job)) nil)

(defmethod tuatara.runtime:job-take-in ((job save-job))
  "A save done marks its buffer saved as of the text it wrote."
  (when (eq (save-job-status job) :done)
    (record-save (save-job-buffer job) (save-job-path job) (save-job-stamp job) (save-job-content job)
                 (or (save-job-written job) (tuatara.text:version-rope (save-job-version job)))
                 (save-job-in-place job))))

(defun start-save (buffer &key (path (tuatara.buffer:buffer-file buffer)) force)
  "Save BUFFER as PATH in the background: its text as it is now is written while
the buffer stays usable, edits included. Checks the file on disk first as
SAVE-BUFFER-FILE does. Returns the job; FINISH-SAVES takes it in."
  (check-savable buffer path)
  (when (save-of buffer)
    (refuse "Buffer ~a is already being saved." (tuatara.buffer:buffer-name buffer)))
  (let* ((path (expand-path path))
         (expected (save-expected buffer path force)))
    (unless (or (eq expected :any) (eq (tuatara.buffer:buffer-local buffer :file-class) :pseudo))
      (let ((existing (tuatara.runtime:ignoring-errors-reported ("examining ~a" path)
                        (path-stamp (resolve-links path)))))
        (unless (or (null existing) (and expected (same-content-p expected existing)))
          (error 'file-changed-on-disk :path path))))
    (let* ((version (tuatara.buffer:buffer-version buffer))
           (transform (host-save-transform tuatara.runtime:*host* buffer path))
           (job (%make-save-job :buffer buffer :path path :version version
                                :content (tuatara.buffer:buffer-content-number buffer)
                                :total (tuatara.text:rope-octet-length (tuatara.text:version-rope version))))
           (mappings (protected-mappings buffer)))
      (tuatara.runtime:start-job-thread
       job (lambda (job)
             (setf (save-job-written job) (transformed-text transform (tuatara.text:version-rope version)))
             (multiple-value-bind (stamp target in-place)
                 (write-text-file (save-job-written job) path :expected expected :mappings mappings
                                  :cancel (save-job-cancel job)
                                  :progress (lambda (done total) (declare (ignore total))
                                              (setf (save-job-done job) done)))
               (if (eq stamp :stopped)
                   (setf (save-job-status job) :stopped)
                   (setf (save-job-stamp job) stamp (save-job-target job) target (save-job-in-place job) in-place
                         (save-job-status job) :done))))
       :name "tuatara save")
      job)))

(defun finish-saves ()
  "Take in the saves that have ended, on the editor's thread: a finished one
marks its buffer saved as of the text it wrote. Returns the jobs taken in."
  (tuatara.runtime:take-in-jobs #'save-job-p))

(defun settle-file-work (buffer)
  "Kill-buffer function: stop the counting of BUFFER's lines and the snapshot
serving it, give up its leases, and wait for a save of it running in the
background to end, so that nothing reads BUFFER's mapping once it is closed.
When text of BUFFER is kept elsewhere (TUATARA.BUFFER:BUFFER-LENT-P), its
snapshots and leases serve that text instead (LEND-FILE-GUARDS); so too when
its file is being copied for a program about to write it, a large file on a
slow drive taking long: the copy goes on and the mapping is kept until nothing
reads it, the buffer killed at once."
  (stop-indexing buffer)
  (cond ((tuatara.buffer:buffer-lent-p buffer) (lend-file-guards buffer))
        (t (stop-snapshots-of buffer)
           (give-up-leases (lambda (lease) (eq (lease-buffer lease) buffer)))
           (when (sb-thread:with-mutex (*lease-lock*)
                   (some (lambda (lease)
                           (and (eq (lease-buffer lease) buffer) (eq (lease-status lease) :moving))) *leases*))
             (setf (tuatara.buffer:buffer-lent-p buffer) t)
             (lend-file-guards buffer))))
  (let ((job (save-of buffer)))
    (when job
      ;; a save that does not end in time (a write stuck on a hung mount) is left to end on its own, the
      ;; file's mapping kept for it
      (unless (tuatara.runtime:join-within (save-job-thread job))
        (setf (tuatara.buffer:buffer-lent-p buffer) t)
        (lend-file-guards buffer)
        (tuatara.runtime:left-running (save-job-thread job)
                                      (format nil "Saving ~a" (tuatara.buffer:buffer-name buffer)))))))

(defun lend-file-guards (buffer)
  "Let the snapshots and leases of BUFFER, which is being killed, go on without
it, for its text kept elsewhere: a snapshot finishes its copy, and a lease
holds its mapping weakly and is given up once nothing reads the mapping."
  (dolist (snapshot (snapshots))
    (when (eq (snapshot-served snapshot) buffer)
      (setf (snapshot-served snapshot) nil)))
  (sb-thread:with-mutex (*lease-lock*)
    (dolist (lease *leases*)
      (when (eq (lease-buffer lease) buffer)
        (setf (lease-buffer lease) nil)
        (unless (sb-ext:weak-pointer-p (lease-mapping lease))
          (setf (lease-mapping lease) (sb-ext:make-weak-pointer (lease-mapping lease))))))))

(pushnew 'forget-arriving-file tuatara.buffer:*kill-buffer-functions*)
(pushnew 'settle-file-work tuatara.buffer:*kill-buffer-functions*)
