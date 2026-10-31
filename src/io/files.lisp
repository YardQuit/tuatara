;;;; src/io/files.lisp --- Visiting, saving and reverting files, safely on every file system.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Visiting: the text shown must not change because another program
;;;; changes the file. Small files, and the files of pseudo file systems, are
;;;; read into memory. A large file on a local file system is mapped itself,
;;;; so that what the system already has in memory of it serves at once, under
;;;; a read lease: a program that opens it for writing waits until a keeper
;;;; thread has cloned it (btrfs, XFS, bcachefs, ZFS; elsewhere copied) and
;;;; moved the mapping onto the clone. Where no lease is granted (another
;;;; user's file, one open for writing already), the file is cloned at once
;;;; where the file system shares blocks, which leaves the clone to be read from
;;;; disk; else it is mapped and copied in the background, and the mapping
;;;; moves onto the copy when it is done. Copies, and the scratch files of work
;;;; on a buffer's text, go beside the file only where its file system shares
;;;; blocks; elsewhere to the cache directory when it has room (COPIES-DIRECTORY):
;;;; a copy written back to the same drive, a USB stick with exFAT, is slower,
;;;; and gone with the drive when it is unplugged. Until then
;;;; a rewrite in place by another program is caught (CHECK-FILE notes :STALE)
;;;; and the buffer made read-only. A file on a read-only mount is not copied:
;;;; nothing can change it there. Where a file cannot be mapped at all, it is
;;;; copied first. Copies have no name and vanish when closed, even after a
;;;; crash.
;;;;
;;;; Saving: a new file with no name is written beside the old one, flushed to
;;;; disk, given the old one's owner, permissions and extended attributes
;;;; (SELinux label, ACLs), named, renamed over it, and the directory flushed:
;;;; the file on disk is always the old one or the new one, even after a power
;;;; cut, and a crash while writing leaves nothing behind. Where the file system
;;;; has no unnamed files, a hidden named one serves. A symlink is followed and
;;;; kept. A file with several names (hard links), one whose owner or group this
;;;; user cannot give a new file, a directory where no file can be made, a
;;;; rename the file system refuses, and the pseudo file systems are written in
;;;; place instead, after a backup copy that is kept if the write fails. A save
;;;; never overwrites a file changed since it was read without being told to.
;;;; Large buffers are saved in the background.
;;;;
;;;; Copying octets that come from the file (the unchanged parts of a large
;;;; buffer) is left to the kernel (copy_file_range), which shares blocks
;;;; instead where the file system can and the offsets allow; the rest is
;;;; written.

(in-package #:tuatara.io)

(defvar *memory-read-limit* (* 64 1024 1024)
  "Files up to this many octets are read into memory when visited, where no other
program can change them; larger ones are mapped (see this file's header).")

(defvar *lease-large-files* t
  "True to read large local files themselves under a read lease, copying them
only when another program is about to write them. Tests turn it off to take the
other paths.")

(defvar *clone-large-files* t
  "True to clone large files on file systems that share blocks. Tests turn it off
to take the paths other file systems take.")

(defvar *snapshot-large-files* t
  "True to copy a large file that cannot be cloned into a snapshot in the
background, where there is room, so that other programs' writes cannot reach
the buffer.")

(defvar *snapshot-free-margin* (* 1024 1024 1024)
  "Octets of free space a background snapshot must leave on its file system.")

(defun kind-words (kind)
  "KIND of file (as STAMP-KIND gives it) in words."
  (ecase kind
    (:directory "a directory") (:fifo "a named pipe") ((:character-device :block-device) "a device")
    (:socket "a socket") (:symlink "a symbolic link") (:other "not a regular file") (:regular "a file")))

(defun file-isolation (buffer)
  "How BUFFER's text is kept from other programs' changes to its file: :MEMORY
(read into memory), :LEASED (the file itself, which a writer must wait to
change until it is cloned), :CLONE (a clone sharing blocks), :SNAPSHOT (a copy,
or the old file after a save), :PENDING (mapped while a copy is made) or
:DIRECT (mapped with no copy: changes are caught but not kept out)."
  (tuatara.buffer:buffer-local buffer :file-isolation :memory))

(defun file-disk-state (buffer)
  "What has become of BUFFER's file on disk, as CHECK-FILE last saw it: NIL when
it is as the buffer read or saved it; :CHANGED when another program changed or
replaced it, the buffer's text being safe; :DELETED; :STALE when it was
rewritten in place while the buffer still read its octets from it, so that the
text can no longer be trusted (the buffer is read-only until reverted)."
  (tuatara.buffer:buffer-local buffer :disk-state))

;;; Snapshots made in the background

(defstruct (snapshot (:include tuatara.runtime:job) (:constructor %make-snapshot) (:copier nil))
  "A copy of a mapped file being made in the background, a job
\(TUATARA.RUNTIME:JOB) of no buffer: of SOURCE, a descriptor of its own for
the file behind MAPPING, PATH, which was at STAMP; FD is the copy. SERVED is
the buffer whose text it keeps, or NIL; CHANGED is true when the file changed
while it was copied, the copy then of no use."
  mapping source path stamp served fd changed)

(setf (documentation 'snapshot-p 'function) "True when OBJECT is a SNAPSHOT.")

(defvar *on-file-news* nil
  "A function of no arguments the watcher calls, on its own thread, when it has
news for TAKE-FILE-LOOKS: the editor sets it to wake its command loop.")

(tuatara.runtime:define-job-words snapshot
  :doing "Copying"
  :description (format nil "Copying ~a, so that other programs' writes do not reach its buffer" (snapshot-path job))
  :verb "copy the file"
  :left-words "the file is read where it is"
  :at-exit :quiet)
(defmethod tuatara.runtime:job-holds-queue-p ((job snapshot)) nil)
(defmethod tuatara.runtime:job-shows-processing-p ((job snapshot)) nil)

(defmethod tuatara.runtime:job-take-in ((job snapshot))
  "A finished copy becomes what its mapping reads; else the buffer it served
reads the file where it is."
  (let ((buffer (snapshot-served job))
        (taken (and (eq (snapshot-status job) :done) (not (snapshot-changed job))
                    (ignore-errors (tuatara.text:retarget-mapping (snapshot-mapping job) (snapshot-fd job))))))
    (unless taken
      (when (snapshot-fd job) (ignore-errors (sb-posix:close (snapshot-fd job)))))
    (when (and buffer (eq (file-isolation buffer) :pending))
      (setf (tuatara.buffer:buffer-local buffer :file-isolation) (if taken :snapshot :direct)))))

(defun start-snapshot (mapping fd path stamp directory)
  "Start copying the file PATH, open as FD and mapped as MAPPING, into an unnamed
file in DIRECTORY in the background, a job, where its file system has room.
The copy reads through a descriptor of its own. Returns the snapshot, or NIL."
  (let ((free (free-bytes directory)))
    (when (and free (> free (+ (stamp-size stamp) *snapshot-free-margin*)))
      (let* ((source (sb-posix:dup fd))
             (snapshot (%make-snapshot :mapping mapping :source source :path path :stamp stamp)))
        (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (sb-posix:close source)))))
          (tuatara.runtime:start-job-thread
           snapshot
           (lambda (snapshot)
             (unwind-protect
                  (let ((copy (open-scratch-file directory)))
                    (setf (snapshot-fd snapshot) copy)
                    (if (copy-fd-contents source copy (stamp-size stamp) :path path :cancel (snapshot-cancel snapshot))
                        ;; a copy is only good if the file did not change while it was made
                        (setf (snapshot-changed snapshot)
                              (not (same-content-p stamp (ignore-errors (fd-stamp source path)))))
                        (setf (snapshot-status snapshot) :stopped)))
               (ignore-errors (sb-posix:close source))))
           :name "tuatara snapshot"))
        snapshot))))

(defun snapshots ()
  "The background snapshots running or ended and not yet taken in."
  (remove-if-not #'snapshot-p tuatara.runtime:*jobs*))

(defun finish-snapshots ()
  "Take in the background snapshots that have ended, on the editor's thread
\(their JOB-TAKE-IN): a finished copy becomes what its mapping reads."
  (tuatara.runtime:take-in-jobs #'snapshot-p))

(defun stop-snapshots-of (buffer)
  "Stop any background snapshot serving BUFFER."
  (dolist (snapshot (snapshots))
    (when (eq (snapshot-served snapshot) buffer)
      (tuatara.runtime:job-stop snapshot))))

;;; Leases: reading the file itself until someone is about to write it

(defstruct (lease (:constructor %make-lease) (:copier nil))
  "A read lease on a visited file: FD, open on the file at PATH as it was at
STAMP, is what MAPPING reads while STATUS is :HELD. When a program is about to
write the file, the keeper copies it into PLACE, a directory (COPIED-BY :CLONE
or :SNAPSHOT), moves MAPPING onto the copy and lets the writer go: :MOVING,
then :MOVED. :LOST when the copy could not be made, or not in time; :RELEASED
when given up. BUFFER is the buffer it serves: NIL once that buffer is killed
with its text kept elsewhere, MAPPING being then a weak pointer to the mapping
(LEASE-TARGET), and the lease given up when nothing reads the mapping any more."
  mapping fd path stamp buffer place (status :held) copied-by)

(defun lease-target (lease)
  "The mapping LEASE guards, or NIL once it has been garbage collected."
  (let ((mapping (lease-mapping lease)))
    (if (sb-ext:weak-pointer-p mapping) (sb-ext:weak-pointer-value mapping) mapping)))

(setf (documentation 'lease-p 'function) "True when OBJECT is a LEASE.")

(defvar *leases* '()
  "Leases held on visited files, and those that ended and are not yet taken in.")

(defvar *lease-lock* (sb-thread:make-mutex :name "tuatara leases")
  "Held while *LEASES* or a lease's status changes.")

(defvar *lease-keeper* nil
  "The thread that watches the leases, while there are any.")

(defvar *lease-check-interval* 5
  "Seconds the keeper waits at most between looks at the leases. It looks at
once when the system says a program is about to write a leased file, and after
each garbage collection while text kept from a killed buffer holds a lease;
this is for the rare signal that does not come.")

(defvar *lease-grace* 1/2
  "Seconds the keeper waits after moving a mapping before it lets the writer
go, so that reads begun on the file itself have finished.")

(defun copy-for-lease (lease)
  "A descriptor for a copy of LEASE's file in its place, and how it was made: a
clone where the file system shares blocks (:CLONE), else a full copy (:SNAPSHOT)."
  (let* ((fd (lease-fd lease))
         (path (lease-path lease))
         (copy (open-scratch-file (lease-place lease))))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (sb-posix:close copy)))))
      (if (clone-fd fd copy)
          (values copy :clone)
          (progn (copy-fd-contents fd copy (stamp-size (lease-stamp lease)) :path path)
                 (values copy :snapshot))))))

(defun move-leased (lease)
  "A program is about to write LEASE's file: copy it, move the mapping onto the
copy, and only then let the writer go. When the copy cannot be made, or the
system broke the lease before it was, the lease is :LOST, and a change to the
file will show the buffer stale. LEASE is :MOVING already (LEASE-KEEPER-LOOP)."
  (let ((copy nil))
    (handler-case
        (multiple-value-bind (fd how) (copy-for-lease lease)
          (setf copy fd)
          (if (and (lease-target lease)
                   (same-content-p (lease-stamp lease) (fd-stamp (lease-fd lease) (lease-path lease)))
                   (tuatara.text:retarget-mapping (lease-target lease) copy))
              (progn
                (setf copy nil)
                (sleep *lease-grace*)
                (setf (lease-copied-by lease) how
                      (lease-status lease) :moved))
              (setf (lease-status lease) :lost)))
      (error () (setf (lease-status lease) :lost)))
    (when copy (ignore-errors (sb-posix:close copy)))
    (release-lease (lease-fd lease))))

(defun lease-keeper-loop ()
  "What the keeper thread does: whenever woken (*LEASE-BREAK-PIPE*), and every
*LEASE-CHECK-INTERVAL* seconds at most, move the leased files a program is
about to write, and give up the leases whose mapping nothing reads any more;
end when no lease is held."
  (loop
    (drain-wake-pipe *lease-break-pipe*)
    (let ((held (sb-thread:with-mutex (*lease-lock*)
                  (or (remove-if-not (lambda (lease) (eq (lease-status lease) :held)) *leases*)
                      (progn (setf *lease-keeper* nil) (return))))))
      (dolist (lease held)
        (cond ((not (eq (lease-status lease) :held)))
              ((null (lease-target lease)) (give-up-leases (lambda (other) (eq other lease))))
              ((and (lease-breaking-p (lease-fd lease))
                    ;; :MOVING only while still :HELD, so that a lease given up meanwhile is not copied
                    (sb-thread:with-mutex (*lease-lock*)
                      (when (eq (lease-status lease) :held)
                        (setf (lease-status lease) :moving))))
               (move-leased lease)))))
    (wait-for-fds (list (car *lease-break-pipe*)) *lease-check-interval*)))

(defun wake-lease-keeper-after-gc ()
  "Wake the lease keeper after a garbage collection when a lease guards text
kept from a killed buffer, which the collection may have freed."
  (when (and *lease-keeper* (find-if #'sb-ext:weak-pointer-p *leases* :key #'lease-mapping))
    (wake *lease-break-pipe*)))

(pushnew 'wake-lease-keeper-after-gc sb-ext:*after-gc-hooks*)

(defun hold-lease (mapping fd path stamp place)
  "Keep the lease just taken on FD, which MAPPING reads, and see that it is
watched; a copy, if one is needed, goes into the directory PLACE. Returns the lease."
  (let ((lease (%make-lease :mapping mapping :fd fd :path path :stamp stamp :place place)))
    (sb-thread:with-mutex (*lease-lock*)
      (push lease *leases*)
      (unless *lease-keeper*
        (setf *lease-keeper* (tuatara.runtime:make-guarded-thread
                              (lambda ()
                                (unwind-protect (lease-keeper-loop)
                                  ;; ended by trouble: another started when a lease is next held
                                  (sb-thread:with-mutex (*lease-lock*)
                                    (when (eq *lease-keeper* sb-thread:*current-thread*)
                                      (setf *lease-keeper* nil)))))
                              :name "tuatara lease keeper"))))
    lease))

(defun give-up-leases (predicate)
  "Give up the held leases PREDICATE is true of, letting any writer go."
  (sb-thread:with-mutex (*lease-lock*)
    (dolist (lease *leases*)
      (when (and (eq (lease-status lease) :held) (funcall predicate lease))
        (setf (lease-status lease) :released)
        (release-lease (lease-fd lease))))))

(defun finish-leases ()
  "Take in the leases that have ended, on the editor's thread: a buffer whose
file was copied reads a clone or snapshot now; one whose copy failed reads the
file directly, watched for changes like any other."
  (let ((ended (sb-thread:with-mutex (*lease-lock*)
                 (let ((ended (remove-if (lambda (lease) (member (lease-status lease) '(:held :moving))) *leases*)))
                   (setf *leases* (set-difference *leases* ended))
                   ended))))
    (dolist (lease ended)
      (let ((buffer (lease-buffer lease)))
        (when (and buffer (eq (file-isolation buffer) :leased)
                   (eq (tuatara.buffer:buffer-mapping buffer) (lease-mapping lease)))
          (setf (tuatara.buffer:buffer-local buffer :file-isolation)
                (case (lease-status lease)
                  (:moved (lease-copied-by lease))
                  (:lost :direct)
                  (t :snapshot))))))))

(defun leases-moving-p ()
  "True while the keeper is copying a file a program is about to write."
  (some (lambda (lease) (eq (lease-status lease) :moving)) *leases*))

(defun buffer-lease (buffer)
  "The lease that keeps BUFFER's text, or NIL."
  (tuatara.buffer:buffer-local buffer :file-lease))

(defun attend (guard buffer)
  "Let GUARD, the snapshot or lease OPEN-FILE-TEXT returned (or NIL), know the
buffer it serves, and the buffer its lease."
  (setf (tuatara.buffer:buffer-local buffer :file-lease) (and (lease-p guard) guard))
  (etypecase guard
    (null nil)
    (snapshot (setf (snapshot-served guard) buffer))
    (lease (setf (lease-buffer guard) buffer))))

;;; Visiting

(defvar *index-at-open-limit* (* 64 1024 1024)
  "Octets up to which a file's lines are counted as it opens. A larger file
opens at once and has them counted in the background (START-INDEXING), so that
a file of many gigabytes shows before the whole of it has been read.")

(defun rope-over (mapping)
  "A rope over all of MAPPING: indexed, or for a file larger than
*INDEX-AT-OPEN-LIMIT* not yet, its lines to be counted in the background."
  (let ((rope (tuatara.text:mapping-rope mapping)))
    (if (> (tuatara.text:mapping-length mapping) *index-at-open-limit*)
        rope
        (tuatara.text:index-rope rope))))

;;; Counting a large text's lines in the background

(defstruct (index-job (:include tuatara.runtime:job) (:constructor %make-index-job (buffer rope donor)) (:copier nil))
  "The counting of the lines and characters of ROPE, BUFFER's text, in the
background, a job (TUATARA.RUNTIME:JOB), taking what DONOR (the text counted
before, or NIL) has counted where the text is still the same: PROGRESS is a
cons of the octets read and those to read; RESULT the counted rope once done.
TALLY is what is counted so far, from the start of the text: the line
numbers there are known before the whole text is counted
\(COUNTED-LINE-NUMBER, COUNTED-LINE-START)."
  rope donor (progress (cons 0 0)) result (tally (tuatara.text:make-tally)))

(setf (documentation 'index-job-p 'function) "True when OBJECT is an INDEX-JOB."
      (documentation 'index-job-status 'function)
      "How the counting of an INDEX-JOB stands: :RUNNING, :DONE, :STOPPED or :FAILED.")

(defmethod tuatara.runtime:job-percent ((job index-job)) (min 99 (index-progress job)))
(tuatara.runtime:define-job-words index-job
  :doing "Counting lines"
  :description (format nil "Counting the lines of ~a" (tuatara.runtime:job-buffer-name job))
  :verb "count the lines"
  :left-words "the lines are counted as they are needed"
  :at-exit :quiet)
(defmethod tuatara.runtime:job-holds-queue-p ((job index-job)) nil)
(defmethod tuatara.runtime:job-shows-processing-p ((job index-job)) nil)

(defmethod tuatara.runtime:job-take-in ((job index-job))
  "A counting done gives its buffer the counted text, when the text is still
the one counted; one that failed is noted, so that the same text is not
counted again, and one stopped too, so that it is not started again by itself."
  (let ((buffer (index-job-buffer job)))
    (when (tuatara.buffer:live-buffer-p buffer)
      (case (index-job-status job)
        (:failed
         (setf (tuatara.buffer:buffer-local buffer :index-failed) (index-job-rope job)
               (tuatara.buffer:buffer-local buffer :index-error) (index-job-error job))
         (tuatara.runtime:debug-report :file "the lines of ~a could not be counted: ~a"
                                       (tuatara.buffer:buffer-name buffer) (index-job-error job)))
        (:stopped
         (setf (tuatara.buffer:buffer-local buffer :index-stopped) (index-job-rope job)))
        (:done
         (setf (tuatara.buffer:buffer-local buffer :index-donor) (index-job-result job))
         (when (eq (tuatara.buffer:buffer-text buffer) (index-job-rope job))
           (tuatara.buffer:buffer-adopt-text buffer (index-job-result job))))))))

(defvar *index-job-hook* nil
  "A function each counting calls before it begins, or NIL. Tests use it to keep
one running long enough to act while it runs.")

(defun index-jobs ()
  "The countings running in the background, or ended and not taken in yet."
  (remove-if-not #'index-job-p tuatara.runtime:*jobs*))

(defun index-job-of (buffer)
  "The counting of BUFFER's lines that runs or has not been taken in, or NIL."
  (find-if (lambda (job) (and (index-job-p job) (eq (index-job-buffer job) buffer))) tuatara.runtime:*jobs*))

(defun index-progress (job)
  "How much of JOB's counting is done, from 0 to 100."
  (tuatara.runtime:part-percent (index-job-progress job)))

(defun counting-tally (buffer)
  "The tally of the counting of BUFFER's lines, while it runs over the text the
buffer has now; else NIL."
  (let ((job (index-job-of buffer)))
    (and job (eq (index-job-status job) :running)
         (eq (tuatara.buffer:buffer-text buffer) (index-job-rope job))
         (index-job-tally job))))

(defun counted-line-number (buffer byte)
  "While BUFFER's lines are being counted: the line, from 0, holding offset
BYTE, when the count has passed it from the start; else NIL."
  (let ((tally (counting-tally buffer)))
    (and tally (tuatara.text:tally-line-number tally (tuatara.buffer:buffer-text buffer) byte))))

(defun counted-line-start (buffer line)
  "While BUFFER's lines are being counted: where line LINE, from 0, starts,
when the count has passed it from the start; else NIL."
  (let ((tally (counting-tally buffer)))
    (and tally (tuatara.text:tally-line-start tally (tuatara.buffer:buffer-text buffer) line))))

(defun counted-lines (buffer)
  "While BUFFER's lines are being counted: two values, the newlines counted from
the start of its text so far and the offset the count has reached; else NIL."
  (let ((tally (counting-tally buffer)))
    (and tally (tuatara.text:tally-counted-lines tally))))

(defun start-indexing (buffer)
  "Start counting the lines of BUFFER's text in the background, a job, unless
they are known or being counted, or counting this same text failed
\(:INDEX-FAILED): on the worker pool, taking what the text counted last had
where it is the same. Returns the job, or NIL."
  (or (index-job-of buffer)
      (unless (or (tuatara.buffer:buffer-indexed-p buffer)
                  (eq (tuatara.buffer:buffer-local buffer :index-failed) (tuatara.buffer:buffer-text buffer)))
        (let ((hook *index-job-hook*))
          (tuatara.runtime:start-job-thread
           (%make-index-job buffer (tuatara.buffer:buffer-text buffer) (tuatara.buffer:buffer-local buffer :index-donor))
           (lambda (job)
             (when hook (funcall hook))
             (let ((result (tuatara.text:index-rope (index-job-rope job) :donor (index-job-donor job)
                                                                        :progress (index-job-progress job)
                                                                        :cancel (index-job-cancel job)
                                                                        :tally (index-job-tally job))))
               (setf (index-job-result job) result
                     (index-job-status job) (if result :done :stopped))))
           :name "tuatara counting lines")))))

(defvar *index-again-pause* 1/2
  "Seconds a large text counted before, then edited, rests unchanged before its
lines are counted again: while it is typed in, counting it again at each
change would go through all its pieces each time.")

(defvar *index-again-octets* (* 64 1024 1024)
  "Octets from which a text edited while it was counted waits for
*INDEX-AGAIN-PAUSE* before it is counted again.")

(defun index-again-waits-p (buffer)
  "True when BUFFER, a large text counted before and edited since, was
changed less than *INDEX-AGAIN-PAUSE* ago, as the ticks that look at it saw:
its counting then waits for the edits to pause."
  (and (tuatara.buffer:buffer-local buffer :index-donor)
       (> (tuatara.buffer:buffer-size buffer) *index-again-octets*)
       (let ((seen (tuatara.buffer:buffer-local buffer :index-again-seen))
             (version (tuatara.buffer:buffer-version buffer))
             (now (get-internal-real-time)))
         (if (and seen (eql (car seen) version))
             (< (- now (cdr seen)) (* *index-again-pause* internal-time-units-per-second))
             (progn (setf (tuatara.buffer:buffer-local buffer :index-again-seen) (cons version now))
                    t)))))

(defun finish-indexing ()
  "On the editor's thread: take in the countings that have ended (their
JOB-TAKE-IN). A buffer whose text is still the one counted takes the counted
text, the same octets (TUATARA.BUFFER:BUFFER-ADOPT-TEXT); one edited meanwhile
is counted again, which reads only around the edits, once the edits pause
when it is large (INDEX-AGAIN-WAITS-P). A buffer whose text is not counted
and not being counted has a counting started, unless counting that same text
failed (its :INDEX-FAILED and :INDEX-ERROR say so) or was stopped. True while
countings run, or one waits."
  (tuatara.runtime:take-in-jobs #'index-job-p)
  (let ((waiting nil))
    (dolist (buffer tuatara.buffer:*buffers*)
      (unless (or (tuatara.buffer:buffer-indexed-p buffer) (index-job-of buffer)
                  (eq (tuatara.buffer:buffer-local buffer :index-failed) (tuatara.buffer:buffer-text buffer))
                  (eq (tuatara.buffer:buffer-local buffer :index-stopped) (tuatara.buffer:buffer-text buffer)))
        (if (index-again-waits-p buffer)
            (setf waiting t)
            (start-indexing buffer))))
    (when waiting (return-from finish-indexing t)))
  (some (lambda (job) (eq (index-job-status job) :running)) (index-jobs)))

(defun stop-indexing (buffer)
  "Stop the counting of BUFFER's lines, if one runs, and wait for its workers,
so that none reads the buffer's text once its file is closed; one that does
not stop in time (TUATARA.RUNTIME:*STOP-WAIT*) is left to end on its own, the
file kept mapped for it (BUFFER-LENT-P)."
  (let ((job (index-job-of buffer)))
    (when job
      (unless (tuatara.runtime:stop-and-settle job)
        (setf (tuatara.buffer:buffer-lent-p buffer) t))
      (tuatara.runtime:forget-job job))))

(defun try-clone (fd path)
  "A descriptor for an unnamed clone of the open file FD made in its own
directory, sharing its blocks, or NIL where the file system cannot. One that
shares no blocks (SHARES-BLOCKS-P) is not tried, and so not written to."
  (when *clone-large-files*
    (let ((directory (parent-path path)))
      (when (and (writable-p directory) (shares-blocks-p directory))
        (let ((clone (ignore-errors (open-scratch-file directory))))
          (when clone
            (if (clone-fd fd clone)
                clone
                (progn (sb-posix:close clone) nil))))))))

(defun shares-blocks-p (directory)
  "True when the file system holding DIRECTORY can share blocks between files
(*BLOCK-SHARING-FILE-SYSTEMS*)."
  (and (member (path-file-system directory) *block-sharing-file-systems*) t))

(defun copies-directory (directory local room)
  "Where copies and scratch files serving a file in DIRECTORY go, ROOM octets
of them or so: DIRECTORY itself when it is LOCAL (on this machine), takes new
files and its file system shares blocks (btrfs, XFS), where a copy is a clone
taking no time and no space, and a save can share scratch files' blocks. Else
the editor's cache directory, when it has ROOM and 1 GB more free: a copy
written back to the same drive, such as a USB stick with exFAT, is slower,
and is gone with the drive when it is unplugged. Else DIRECTORY, when it is
LOCAL and takes new files; else the cache directory. Never /tmp, which may be
memory."
  (cond ((not (and directory local (writable-p directory))) (cache-directory))
        ((shares-blocks-p directory) directory)
        ((let ((free (free-bytes (cache-directory))))
           (and free (> free (+ room *snapshot-free-margin*))))
         (cache-directory))
        (t directory)))

(defun scratch-directory (path &key (room 0))
  "Where scratch files serving the file at PATH go (the runs of a sort,
rewritten text), ROOM octets of them or so: see COPIES-DIRECTORY. PATH may be
NIL, for a buffer without a file."
  (let ((directory (and path (directory-pathname (parent-path path)))))
    (copies-directory directory (and directory (eq (directory-class directory) :local)) room)))

(defun buffer-scratch-directory (buffer)
  "Where the scratch files of work on BUFFER's text go (the runs of a sort,
rewritten text): as SCRATCH-DIRECTORY has it for its file and a text of its
size, except for a file visited safely, whose file system stays as it was even
once editing is allowed: its scratch files go to the cache directory; and NIL
for an encrypted file's text, which never goes to a scratch file (its work
done in memory, or refused)."
  (cond ((tuatara.buffer:buffer-local buffer :plaintext) nil)
        ((safe-visit-p buffer) (cache-directory))
        (t (scratch-directory (tuatara.buffer:buffer-file buffer) :room (tuatara.buffer:buffer-size buffer)))))

(defun snapshot-directory (path class &key (room 0))
  "Where a copy of the file PATH goes, a snapshot or the copy made when another
program is about to write it, ROOM octets, the file being on a file system of
CLASS (FILE-SYSTEM-CLASS): see COPIES-DIRECTORY."
  (copies-directory (directory-pathname (parent-path path)) (eq class :local) room))

(defun copy-to-cache (fd size path)
  "A descriptor for an unnamed copy, made now in the cache directory, of the
first SIZE octets of the open file FD, which is PATH."
  (let ((copy (open-scratch-file (cache-directory))))
    (handler-bind ((error (lambda (e) (declare (ignore e)) (ignore-errors (sb-posix:close copy)))))
      (copy-fd-contents fd copy size :path path))
    copy))

(defun open-examined (dirfd octets flags path &key (accept '(:regular)) safe (operation "open") (wanted "a file"))
  "A descriptor for the entry whose name is OCTETS in the directory open as
DIRFD (+AT-FDCWD+ for a name from the current directory, or a whole path),
opened with FLAGS, never as a terminal that would take this program as its
own; a symbolic link followed unless FLAGS hold O_NOFOLLOW. The entry is
first only named (O_PATH), which opens nothing, and examined: when it is not
of a kind ACCEPT lists (as STAMP-KIND gives them) it is refused unopened, a
FILE-SYSTEM-ERROR saying for OPERATION it is not WANTED, so that a named pipe
is never waited on and a device never woken (a serial line's control lines,
a tape's position, stay as they were). Then that very file is opened, through
/proc/self/fd, so that what is opened is what was examined even when the
name is changed meanwhile. With SAFE, its access time is left alone where
the system allows (your own files). PATH names it in messages."
  (multiple-value-bind (handle errno)
      (openat-octets dirfd octets (logior +o-path+ +o-cloexec+ (logand flags +o-nofollow+)))
    (unless handle (fail operation path errno))
    (unwind-protect
         (let* ((stamp (fd-stamp handle path))
                (kind (stamp-kind stamp))
                (nofollow (logand flags +o-nofollow+))
                (flags (logior (logandc2 flags +o-nofollow+) +o-cloexec+ sb-posix:o-noctty)))
           (unless (member kind accept)
             (error 'file-system-error :operation operation :path path
                                       :reason (format nil "it is ~a, not ~a" (kind-words kind) wanted)))
           (flet ((try (flags)
                    (if (proc-fd-available-p)
                        (handler-case (sb-posix:open (format nil "/proc/self/fd/~d" handle) flags)
                          (sb-posix:syscall-error (e) (values nil (sb-posix:syscall-errno e))))
                        ;; no /proc: the name opened again, and what it opens checked to be what was examined
                        (multiple-value-bind (fd errno) (openat-octets dirfd octets (logior flags nofollow))
                          (cond ((null fd) (values nil errno))
                                ((let ((now (ignore-errors (fd-stamp fd path))))
                                   (and now (= (stamp-dev now) (stamp-dev stamp)) (= (stamp-ino now) (stamp-ino stamp))))
                                 fd)
                                (t (sb-posix:close fd)
                                   (error 'file-system-error :operation operation :path path
                                                             :reason "it was replaced while it was opened")))))))
             (multiple-value-bind (fd errno) (try (if safe (logior flags +o-noatime+) flags))
               ;; another's file: its access time cannot be kept, it is read all the same
               (when (and (null fd) safe (eql errno sb-posix:eperm))
                 (multiple-value-setq (fd errno) (try flags)))
               (or fd (fail operation path errno)))))
      (sb-posix:close handle))))

(defun open-for-reading (path safe &key (accept '(:regular :directory)) (operation "open") (wanted "a file"))
  "A descriptor for reading the file at PATH, a symbolic link followed, when
it is of a kind ACCEPT lists (a file or a directory as standard): anything
else, a named pipe or a device, is refused without being opened, a
FILE-SYSTEM-ERROR saying for OPERATION it is not WANTED (OPEN-EXAMINED).
With SAFE, its access time is left alone where the system allows (your
own files)."
  (open-examined +at-fdcwd+ (tuatara.text:string-octets (native path)) (logior sb-posix:o-rdonly sb-posix:o-nonblock) path
                 :accept accept :safe safe :operation operation :wanted wanted))

(defun open-file-text (path &key safe)
  "The text of the file at PATH, as six values: a rope, the mapping it reads
(or NIL), the file's STAMP, how the text is kept from other programs' changes
(see FILE-ISOLATION), what keeps it so (the lease for :LEASED, the snapshot
being made for :PENDING; ATTEND tells it the buffer), and the class of its file
system (FILE-SYSTEM-CLASS). Refuses what is not a regular file without opening
it, so that a named pipe or a device is never waited on or woken. See this
file's header for which file gets which.
SAFE keeps the file system the file is on exactly as it was: no clone and
no copy is made beside the file (copies go to the cache directory), and its
access time is left alone where the system allows."
  (let ((fd (open-for-reading path safe :accept '(:regular) :wanted "a file of text"))
        (keep-fd nil))
    (unwind-protect
         (let ((stamp (fd-stamp fd path)))
           (let ((class (file-system-class (fd-file-system fd)))
                 (size (stamp-size stamp)))
             (if (or (eq class :pseudo) (<= size *memory-read-limit*))
                 ;; pseudo files report sizes that are not to be trusted: read to the end, but never past the limit
                 (let ((octets (read-fd-octets fd :path path :size-hint (max 4096 (min size *memory-read-limit*))
                                                  :most *memory-read-limit*)))
                   (when (> (length octets) *memory-read-limit*)
                     (error 'file-system-error :operation "read" :path path
                                               :reason (format nil "it gives more than ~:d octets, the most read into memory"
                                                               *memory-read-limit*)))
                   (values (tuatara.text:octets->rope octets) nil stamp :memory nil class))
                 (let* ((lease (and *lease-large-files* (eq class :local) (take-read-lease fd)
                                    (let ((mapping (ignore-errors (tuatara.text:map-fd fd size path))))
                                      (if mapping
                                          (progn (setf keep-fd t) ; the mapping owns it now
                                                 (hold-lease mapping fd path stamp
                                                             (if safe (cache-directory) (snapshot-directory path class :room size))))
                                          (release-lease fd)))))
                        (clone (and (null lease) (not safe) (eq class :local) (try-clone fd path))))
                   (cond
                     (lease
                      (values (rope-over (lease-mapping lease)) (lease-mapping lease) stamp :leased lease class))
                     (clone
                       (let ((mapping (tuatara.text:map-fd clone size path)))
                         (values (rope-over mapping) mapping stamp :clone nil class)))
                     (t
                       (let ((mapping (ignore-errors (tuatara.text:map-fd fd size path))))
                         (if (null mapping)
                             ;; a file system that cannot map this file: map a copy instead
                             (let ((copy (copy-to-cache fd size path)))
                               (let ((mapping (tuatara.text:map-fd copy size path)))
                                 (values (rope-over mapping) mapping stamp :snapshot nil class)))
                             (progn
                               (setf keep-fd t) ; the mapping owns it now
                               (let ((snapshot (and *snapshot-large-files* (not (read-only-mount-p fd))
                                                    ;; no snapshot is no reason not to show the file
                                                    (ignore-errors
                                                     (start-snapshot mapping fd path stamp
                                                                     (if safe (cache-directory) (snapshot-directory path class :room size)))))))
                                 (values (rope-over mapping) mapping stamp (if snapshot :pending :direct)
                                         snapshot class)))))))))))
      (unless keep-fd (sb-posix:close fd)))))

(defvar *visit-safely-function* nil
  "A function of the path of a file about to be visited, not asked to be
visited safely, that returns why it should be all the same, a phrase, or NIL.")

(defconstant +binary-sample+ 65536
  "The octets at the start of a file looked at to tell whether it is binary.")

(defun binary-head-p (octets &optional (end (length octets)))
  "True when OCTETS below END, the first octets of a file (FILE-HEAD), say it
is binary: a NUL octet among the first 64 KB of them (+BINARY-SAMPLE+), as
executables, archives and disk images have and text has not; NIL for no
octets. Opening a file, previewing it, searching files and keeping
directories read tell binary files alike by it."
  (and octets (find 0 octets :end (min end +binary-sample+)) t))

(defvar *file-heads* nil
  "While a file is visited (CALL-WITH-FILE-HEADS), the first octets of the
files read so far, each (PATH LIMIT SIZE OCTETS), in the car, so that each is
read once; NIL otherwise.")

(defun read-fd-start (fd path size limit &key past-size)
  "The first LIMIT octets, or all when there are fewer, of the open file FD
(PATH for messages) of SIZE octets, as the system says; with PAST-SIZE, SIZE
and one more, at least 8 KB, at most LIMIT, so that a file larger than the
system says (one in /proc) is seen to be."
  (let* ((octets (make-array (min limit (if past-size (max 8192 (1+ size)) size)) :element-type '(unsigned-byte 8)))
         (got (read-fd-at fd octets 0 :path path)))
    (if (= got (length octets)) octets (subseq octets 0 got))))

(defun read-file-head (path limit)
  "The first octets of the regular file at PATH, at most LIMIT (READ-FD-START),
and its size as a second value; opened as CALL-WITH-FILE opens it. NIL when it
is no regular file or cannot be read."
  (handler-case (call-with-file path (lambda (fd path size) (values (read-fd-start fd path size limit :past-size t) size)))
    (error () nil)))

(defun file-head (path &optional (limit +binary-sample+))
  "The first octets of the regular file at PATH, at most LIMIT (64 KB as
standard, +BINARY-SAMPLE+), and its size as a second value: read without
changing its access time where the system allows, never from a named pipe
or a device (NIL for what is no regular file, or cannot be read). While a
file is visited (CALL-WITH-FILE-HEADS), a file's head is read once, however
many look at it: whether it is binary (BINARY-HEAD-P), its kind (OCTETS-KIND),
whether it is a PDF or a picture to show as such."
  (let* ((heads *file-heads*)
         (native (native path))
         (kept (and heads (find native (car heads) :key #'first :test #'string=))))
    (if (and kept (or (>= (second kept) limit) (< (length (fourth kept)) (second kept))))
        (let ((octets (fourth kept)))
          (values (if (> (length octets) limit) (subseq octets 0 limit) octets) (third kept)))
        (multiple-value-bind (octets size) (read-file-head native limit)
          (when (and heads octets)
            (push (list native limit size octets) (car heads)))
          (values octets size)))))

(defun call-with-file-heads (function)
  "Call FUNCTION, of no arguments, each file's first octets read once
meanwhile (FILE-HEAD), and return what it returns: around a visit, whose
views, binary test and kind all look at the same octets."
  (if *file-heads*
      (funcall function)
      (let ((*file-heads* (list '())))
        (funcall function))))

(defstruct (listed (:constructor make-listed (name octets stamp used device target target-stamp &optional label)) (:copier nil))
  "An entry of a directory as LIST-DIRECTORY reads it."
  (name "" :read-only t) (octets nil :read-only t) (stamp nil :read-only t) (used 0 :read-only t)
  (device nil :read-only t) (target nil :read-only t) (target-stamp nil :read-only t) (label nil :read-only t))

(setf (documentation 'make-listed 'function) "Return a LISTED entry with the given fields."
      (documentation 'listed-p 'function) "True when OBJECT is a LISTED entry."
      (documentation 'listed-name 'function)
      "The entry's name, decoded as the display decodes text: an octet that is no UTF-8 as its raw byte's character."
      (documentation 'listed-octets 'function) "The entry's name as the system has it, its octets."
      (documentation 'listed-stamp 'function)
      "The entry's STAMP, a symbolic link not followed; NIL when it could not be examined."
      (documentation 'listed-used 'function) "The octets the entry takes on the disk."
      (documentation 'listed-device 'function)
      "For a device, the numbers it stands for, (MAJOR . MINOR); else NIL."
      (documentation 'listed-target 'function)
      "For a symbolic link, what it holds, decoded as the name is; else NIL."
      (documentation 'listed-target-stamp 'function)
      "For a symbolic link, the STAMP of what it leads to, NIL when that is nowhere; else NIL."
      (documentation 'listed-label 'function)
      "What ls -l marks after the entry's permissions (ENTRY-LABEL): #\\+, #\\. or NIL; NIL as well when not asked for.")

(defun list-directory (directory &key labels)
  "The entries of DIRECTORY, those whose names start with a dot among them and
. and .. too, as LISTED entries in the order the system gives them, with
LABELS what ls -l marks after their permissions (LISTED-LABEL). Only the
directory is opened, without changing its access time where the system allows
(your own directories); what is in it is examined, never opened, so that named pipes
and devices are safe to list. NIL, and the reason as a second value, when it
cannot be read."
  (handler-case
      (let ((fd (open-for-reading directory t :accept '(:directory) :wanted "a directory"))
            (entries '()))
        (unwind-protect
             (map-directory-fd
              (lambda (octets)
                (push (multiple-value-bind (stamp used device) (statx-octets fd octets #x100)
                        (let* ((kind (and stamp (stamp-kind stamp)))
                               (target (and (eq kind :symlink) (link-octets fd octets))))
                          (make-listed (tuatara.text:octets-string octets) octets stamp (or used 0)
                                       (and (member kind '(:character-device :block-device)) device)
                                       (and target (tuatara.text:octets-string target))
                                       (and (eq kind :symlink) (values (statx-octets fd octets 0)))
                                       (and labels stamp (entry-label fd octets)))))
                      entries))
              fd directory)
          (sb-posix:close fd))
        (nreverse entries))
    (file-system-error (e) (values nil (file-system-error-reason e)))))

(defun directory-entries (directory &key stamps (order #'string<))
  "The entries of DIRECTORY, hidden ones included, sorted by name with ORDER
\(a predicate of two names, STRING< as standard): lists of the
name (an octet that is no UTF-8 as its raw byte's character), the kind (as
STAMP-KIND gives it, following symbolic links; :SYMLINK for one that leads
nowhere), the size in octets and the entry's own kind (:SYMLINK for a link,
whatever it leads to; :OTHER when it could not be examined), and with STAMPS
the STAMP of what it leads to (NIL when it could not be examined). Read as
LIST-DIRECTORY reads it: only the directory is opened, so named pipes and
devices in it are safe to list, and nothing is examined twice. NIL when it
cannot be read."
  (sort (loop for entry in (list-directory (native (directory-pathname directory)))
              for octets = (listed-octets entry)
              unless (or (equalp octets #(46)) (equalp octets #(46 46)))
                collect (let* ((own (listed-stamp entry))
                               (stamp (if (and own (eq (stamp-kind own) :symlink))
                                          (or (listed-target-stamp entry) own)
                                          own))
                               (fields (list (listed-name entry) (if stamp (stamp-kind stamp) :other)
                                             (and stamp (stamp-size stamp)) (if own (stamp-kind own) :other))))
                          (if stamps (append fields (list stamp)) fields)))
        order :key #'first))

(defun call-with-entry (directory octets function)
  "Call FUNCTION with a descriptor open on the file whose name is OCTETS in
DIRECTORY, a symbolic link followed, the file's name as text and its size;
closed after, whatever FUNCTION returns. Its access time is left alone where
the system allows (your own files). What is not a regular file is refused
without being opened (OPEN-EXAMINED), so that a named pipe or a device is
never read: a FILE-SYSTEM-ERROR, as for a file that cannot be read."
  (let* ((path (path-join directory (tuatara.text:octets-string octets)))
         (dirfd (open-for-reading directory t :accept '(:directory) :wanted "a directory")))
    (unwind-protect
         (let ((fd (open-examined dirfd octets (logior sb-posix:o-rdonly sb-posix:o-nonblock) path
                                  :safe t :wanted "a file of text")))
           (unwind-protect (funcall function fd path (stamp-size (fd-stamp fd path)))
             (sb-posix:close fd)))
      (sb-posix:close dirfd))))

(defun call-with-file (path function)
  "Call FUNCTION with a descriptor open on the file at PATH, its name and its
size, as CALL-WITH-ENTRY does: its access time left alone where the system
allows, and what is not a regular file refused unopened (a FILE-SYSTEM-ERROR)."
  (let ((fd (open-for-reading path t :accept '(:regular))))
    (unwind-protect (funcall function fd path (stamp-size (fd-stamp fd path)))
      (sb-posix:close fd))))

(defun fd-octets (fd path size &optional limit too-large)
  "The octets of the file open as FD, PATH's, of SIZE octets, read whole from
its start. With LIMIT, a FILE-SYSTEM-ERROR when it has more than LIMIT
octets, as SIZE says or as it turns out when read, no more than that read;
with TOO-LARGE, a function of that size, what it returns instead."
  (flet ((too-large (size)
           (return-from fd-octets
             (if too-large
                 (funcall too-large size)
                 (error 'file-system-error :operation "read" :path path
                                           :reason (format nil "it is larger than ~:d octets" limit))))))
    (when (and limit (> size limit)) (too-large size))
    (sb-posix:lseek fd 0 sb-posix:seek-set)
    (let ((octets (read-fd-octets fd :path path :size-hint size :most limit)))
      (when (and limit (> (length octets) limit)) (too-large (length octets)))
      octets)))

(defun file-octets (path &key limit)
  "The octets of the file at PATH, read whole (CALL-WITH-FILE): its name taken
as it is, a [ or * in it only a character. With LIMIT, a FILE-SYSTEM-ERROR
for a file of more octets than that (FD-OCTETS)."
  (call-with-file path (lambda (fd name size) (fd-octets fd name size limit))))

(defun file-text (path &key limit too-large (missing nil missing-p) (replacement #\?))
  "The text of the file at PATH, read whole as UTF-8 (CALL-WITH-FILE), its
name taken as it is, each octet that is not UTF-8 read as the character
REPLACEMENT (? as standard). With LIMIT, a file of more octets than that is
not read: what TOO-LARGE, a function of the file's name and its size, returns
is returned instead, or without TOO-LARGE a FILE-SYSTEM-ERROR says so. With
MISSING, that is returned when there is no file at PATH, rather than an
error."
  (if (and missing-p (null (path-stamp path)))
      missing
      (call-with-file path
                      (lambda (fd name size)
                        (tuatara.text:lenient-string
                         (fd-octets fd name size limit
                                    (and too-large (lambda (size) (return-from file-text (funcall too-large name size)))))
                         :replacement replacement)))))

(defun read-entry-start (directory octets limit)
  "The first LIMIT octets, or all when there are fewer, of the file whose name is
OCTETS in DIRECTORY, a symbolic link followed; the file's size as a second
value. Opened as CALL-WITH-ENTRY opens it: its access time left alone where the
system allows, and what is not a regular file refused unread."
  (call-with-entry directory octets
                   (lambda (fd path size) (values (read-fd-start fd path size limit) size))))

(defun read-line-bounded (stream most)
  "The next line of STREAM without its line end, or NIL at its end: its first
MOST characters, and true as a second value when it was longer, the rest of
it read and let go."
  (let ((out (make-string-output-stream)) (count 0))
    (loop (let ((c (read-char stream nil)))
            (cond ((and (null c) (zerop count)) (return nil))
                  ((or (null c) (char= c #\Newline))
                   (return (values (get-output-stream-string out) (> count most))))
                  (t (when (< count most) (write-char c out))
                     (incf count)))))))

(defun read-stream-octets (stream &key limit cancel wipe drain)
  "Everything left in STREAM, an octet stream such as a program's output, as
one vector of octets, read in pieces of 64 KiB. With LIMIT, reading stops once
more than LIMIT octets came: NIL, and true as a second value; with DRAIN too,
it goes on to the end of STREAM, what comes past LIMIT let go, and the first
LIMIT octets are given, true as a second value (what a program says of
itself, never left unread for it to wait on). CANCEL, a cons, stops reading
when its car turns true: what was read so far, and true as a third value.
With WIPE, every piece read is filled with zeros once the vector holds it, or
once it is let go, so that what was read is left only in the vector given."
  (let ((pieces (make-octet-pieces)) (too-large nil) (cancelled nil) (spare nil))
    (loop (when (and cancel (car cancel)) (setf cancelled t) (return))
          (let* ((piece (if too-large
                            (or spare (setf spare (make-array 65536 :element-type '(unsigned-byte 8))))
                            (make-array 65536 :element-type '(unsigned-byte 8))))
                 (n (read-sequence piece stream)))
            (unless too-large
              (add-octet-piece pieces piece n)
              (when (and limit (> (octet-pieces-size pieces) limit))
                (setf too-large t)
                (unless drain (return))))
            (when (< n (length piece)) (return))))
    (let ((octets (cond ((not too-large) (joined-octets pieces))
                        (drain (subseq (joined-octets pieces) 0 limit)))))
      (when wipe
        (wipe-octet-pieces pieces)
        (when spare (fill spare 0)))
      (values octets too-large cancelled))))

(defun visit-file (path &key safe)
  "The buffer visiting the file at PATH, made if no live buffer does: its text
kept safe from other programs (OPEN-FILE-TEXT); for a file not there yet, an
empty buffer that saving will create. A new buffer is passed to
TUATARA.BUFFER:*FIND-FILE-FUNCTIONS*, which give it its major mode, unless SAFE:
then the buffer is read-only, no mode and no hook sees it, and the file
system the file is on is left as it was. The buffer is read-only too when this
user may not write the file, or, for a file not there yet, make it in its
directory (\\[read-only-mode] makes it editable). A file not asked to be
visited SAFE is all the same when *VISIT-SAFELY-FUNCTION* gives a reason, kept
for SAFE-VISIT-REASON. Returns the buffer, and true as a second value when a
buffer visited the file already (it comes as it is)."
  (let* ((path (expand-path path))
         (existing (find path tuatara.buffer:*buffers* :key #'tuatara.buffer:buffer-file :test #'equal)))
    (if existing
        (values existing t)
        (let* ((name (tuatara.buffer:unique-buffer-name (native-file-name path)))
               (buffer nil)
               (why (and (not safe) *visit-safely-function* (path-stamp path)
                         (ignore-errors (funcall *visit-safely-function* path))))
               (safe (or safe (and why t))))
          (when (and safe (null (path-stamp path)))
            (error 'file-system-error :operation "open" :path path :reason "there is no such file"))
          (if (path-stamp path)
              (multiple-value-bind (rope mapping stamp isolation guard class) (open-file-text path :safe safe)
                (setf buffer (tuatara.buffer:make-buffer name :text rope :file path)
                      (tuatara.buffer:buffer-mapping buffer) mapping
                      (tuatara.buffer:buffer-local buffer :file-stamp) stamp
                      (tuatara.buffer:buffer-local buffer :file-isolation) isolation
                      (tuatara.buffer:buffer-local buffer :file-class) class
                      (tuatara.buffer:buffer-local buffer :line-ending) (tuatara.buffer:detect-line-ending rope))
                (note-file-tail buffer rope)
                (attend guard buffer))
              (setf buffer (tuatara.buffer:make-buffer name :file path)))
          (if safe
              (setf (tuatara.buffer:buffer-local buffer :read-only) t
                    (tuatara.buffer:buffer-local buffer :safe-visit) (or why t))
              (progn
                (unless (if (path-stamp path)
                            (writable-p path)
                            (writable-p (parent-path path)))
                  (setf (tuatara.buffer:buffer-local buffer :read-only) t))
                (dolist (f tuatara.buffer:*find-file-functions*) (funcall f buffer))))
          (values buffer nil)))))

(defconstant +tail-sample+ 4096
  "Octets of the end of a file kept when it is read or saved, to recognise it
later as the same file grown by appending (APPEND-FROM-DISK).")

(defun note-file-tail (buffer rope)
  "Keep the last octets of ROPE, the text of BUFFER's file as read or saved, for APPEND-FROM-DISK."
  (let ((size (tuatara.text:rope-bytes rope)))
    (setf (tuatara.buffer:buffer-local buffer :file-tail)
          (tuatara.text:rope-octets rope :start (max 0 (- size +tail-sample+)) :end size))))

(defun safe-visit-p (buffer)
  "True when BUFFER visits its file safely (VISIT-FILE's SAFE)."
  (and (tuatara.buffer:buffer-local buffer :safe-visit) t))

(defun safe-visit-reason (buffer)
  "Why BUFFER visits its file safely although that was not asked for, a phrase
(*VISIT-SAFELY-FUNCTION*), or NIL."
  (let ((safe (tuatara.buffer:buffer-local buffer :safe-visit)))
    (and (stringp safe) safe)))

(defun end-safe-visit (buffer)
  "Make BUFFER, visiting its file safely, visit it as usual from now on
(VISIT-FILE): read-only only when this user may not write the file, or, for a
file not there, make it in its directory. Its text stays as it was read, kept
from other programs' changes as before. Returns BUFFER."
  (let ((path (tuatara.buffer:buffer-file buffer)))
    (setf (tuatara.buffer:buffer-local buffer :safe-visit) nil
          (tuatara.buffer:buffer-local buffer :read-only)
          (and path
               (not (if (path-stamp path)
                        (writable-p path)
                        (writable-p (parent-path path))))
               t))
    buffer))

;;; Noticing changes on disk

(defun disk-status (buffer recorded now)
  "How BUFFER's file on disk, now at stamp NOW (NIL when there is none),
compares with RECORDED, the stamp of what the buffer read or saved: :SAME;
:CHANGED when another program replaced or rewrote it, the buffer's text being
safe; :STALE when it was rewritten in place while the buffer still reads its
octets from it, so that the text shown can no longer be trusted; :DELETED.
The files of pseudo file systems (/proc, /sys) change as they are read, and
count as :SAME."
  (cond ((eq (tuatara.buffer:buffer-local buffer :file-class) :pseudo) :same)
        ((and (null recorded) (null now)) :same)
        ((null now) :deleted)
        ((null recorded) :changed)
        ((same-content-p recorded now) :same)
        ((and (reads-own-file-p buffer)
              (= (stamp-dev recorded) (stamp-dev now)) (= (stamp-ino recorded) (stamp-ino now)))
         :stale)
        (t :changed)))

(defun reads-own-file-p (buffer)
  "True when BUFFER's text still reads its file's own octets with nothing to
keep a change out: mapped directly, or waiting on a snapshot, or leased with
the lease lost."
  (case (file-isolation buffer)
    ((:direct :pending) t)
    (:leased (let ((lease (buffer-lease buffer)))
               (not (and lease (eq (lease-status lease) :moved)))))
    (t nil)))

(defun file-status (buffer)
  "How BUFFER's file on disk compares, now, with what the buffer last read or
saved (see DISK-STATUS); :UNKNOWN when the system will not say (a directory
made unreadable); NIL for a buffer without a file."
  (let ((path (tuatara.buffer:buffer-file buffer)))
    (when path
      (disk-status buffer (tuatara.buffer:buffer-local buffer :file-stamp)
                   (handler-case (path-stamp path)
                     (error () (return-from file-status :unknown)))))))

(defun note-disk-status (buffer status)
  "Note STATUS, as FILE-STATUS gives it, in BUFFER's FILE-DISK-STATE. A stale
buffer stays stale until reverted, and becomes read-only when it turns stale.
Returns the state, and as a second value true when it changed."
  (let* ((old (file-disk-state buffer))
         (new (cond ((eq old :stale) :stale)
                    ((member status '(nil :unknown)) old)
                    ((eq status :same) nil)
                    (t status))))
    (unless (eq new old)
      (setf (tuatara.buffer:buffer-local buffer :disk-state) new)
      (when (eq new :stale)
        (setf (tuatara.buffer:buffer-local buffer :read-only-before-stale)
              (tuatara.buffer:buffer-local buffer :read-only)
              (tuatara.buffer:buffer-local buffer :read-only) t)))
    (values new (not (eq new old)))))

(defun check-file (buffer)
  "Look at BUFFER's file on disk now and note what has become of it (see
NOTE-DISK-STATUS); returns the state and whether it changed. The editor has
the watcher look instead (WATCH-FILES, TAKE-FILE-LOOKS), which never holds it
up."
  (note-disk-status buffer (file-status buffer)))

;;; Watching files on disk

(defvar *poll-interval* 2
  "Seconds between looks at a visited file the kernel cannot tell of: one on a
network file system, where another machine's writes raise no inotify event
here, or in a directory no inotify watch could be had for.")

(defvar *settle-time* 1/5
  "Seconds a file being written must be left alone before it is looked at, so
that a burst of writes makes one look.")

(defvar *burst-time* 1/20
  "Seconds the watcher waits after a file is closed after writing, renamed,
made or deleted before looking at it, and within which it looks at other files
together, so that many changed at once (a git checkout) come as one piece of
news and one message.")

(defvar *settle-limit* 1
  "Seconds at most between looks at a file written without a pause, a log, so
that its growth is seen.")

(defvar *watcher* nil
  "The thread that watches the visited files on disk: told by inotify of what
happens in their directories, it looks at the files concerned (and every
*POLL-INTERVAL* seconds at those it cannot be told of), so that no file server
that stops answering holds the editor up. NIL until needed.")

(defvar *watcher-wake* nil
  "The wake pipe the watcher waits on besides inotify, written after each request.")

(defvar *watcher-requests* (sb-concurrency:make-mailbox :name "tuatara watcher requests")
  "Requests for the watcher: (:WATCH ITEMS) to watch the files of ITEMS, lists
of (BUFFER PATH STAMP CLASS), from now on instead of those before;
\(:DIRECTORIES DIRECTORIES) to watch DIRECTORIES whole.")

(defvar *look-results* (sb-concurrency:make-mailbox :name "tuatara file looks done")
  "What the watcher found: (:NEWS . RESULTS), RESULTS being lists of (BUFFER
PATH STAMP NOW), NOW the file's stamp, NIL or :UNKNOWN.")

(defvar *directory-news* (sb-concurrency:make-mailbox :name "tuatara directories changed")
  "Directories the watcher found changed, each a message of their native names,
for TAKE-DIRECTORY-NEWS.")

(defvar *watched-directories* '()
  "The directories WATCH-DIRECTORIES last sent to the watcher.")

(defvar *watched* '()
  "The items WATCH-FILES last sent to the watcher, so that only a change is sent.")

(defstruct (watch-item (:constructor make-watch-item (buffer path stamp keys)) (:copier nil))
  "A file the watcher watches for BUFFER: PATH as the buffer names it, STAMP
what the buffer read or saved, REPORTED the stamp last reported (a file is
reported when that changes). KEYS are the (DIRECTORY . NAME) its events come
under, NAME in octets: its own, and the file its symbolic links lead to. DUE
is when to look at it (an internal real time) or NIL; FIRST when a burst of
writes not yet looked at began; POLLED true when it is looked at every
*POLL-INTERVAL* seconds instead."
  buffer path stamp reported keys (due nil) (first nil) (polled nil))

(setf (documentation 'make-watch-item 'function) "Return a WATCH-ITEM for BUFFER's PATH, read or saved at STAMP, watched under KEYS."
      (documentation 'watch-item-p 'function) "True when OBJECT is a WATCH-ITEM.")

(defstruct (directory-watch (:constructor make-directory-watch (directory stamp)) (:copier nil))
  "A directory the watcher watches whole, for its list: DIRECTORY, its native
name ending in /; STAMP, as last seen (for a directory looked at every
*POLL-INTERVAL* seconds, POLLED); DUE and FIRST as a WATCH-ITEM has them."
  directory stamp (due nil) (first nil) (polled nil))

(setf (documentation 'directory-watch-p 'function) "True when OBJECT is a DIRECTORY-WATCH.")

(defun watch-key (path)
  "The (DIRECTORY . NAME) inotify reports events on PATH's file under: the
native name of its directory and its own name in octets."
  (cons (parent-path path)
        (tuatara.text:string-octets (native-file-name path))))

(defun watch-keys (path)
  "The keys (WATCH-KEY) of PATH and, when symbolic links lead elsewhere, of the file they lead to."
  (let ((own (watch-key path))
        (real (ignore-errors (watch-key (resolve-links path)))))
    (if (and real (not (and (string= (car real) (car own)) (equalp (cdr real) (cdr own)))))
        (list own real)
        (list own))))

(defun look-at (buffer path stamp)
  "What the watcher reports of the file at PATH for BUFFER, which read or saved it at STAMP."
  (list buffer path stamp (handler-case (path-stamp path) (error () :unknown))))

(defun seconds-units (seconds)
  "SECONDS in internal time units."
  (round (* seconds internal-time-units-per-second)))

(defun unique-strings (strings)
  "STRINGS, each once, in their order, the first of those alike kept: in time
growing with their number alone, for lists of tens of thousands."
  (let ((seen (make-hash-table :test 'equal :size (max 16 (length strings)))))
    (loop for string in strings
          unless (gethash string seen)
            collect (setf (gethash string seen) string))))

(defun sync-inotify-watches (inotify directories wds wanted)
  "Make and end the inotify watches of INOTIFY (DIRECTORIES from their native
names to watch descriptors, WDS from each descriptor to the names it stands
for, more than one when they are one directory, through a symbolic link) so
that just WANTED, native names of directories, are watched, as far as
watches can be had. A watch ends when no name wants it any more."
  (when inotify
    (let ((gone '())
          (wanted-set (let ((set (make-hash-table :test 'equal :size (max 16 (length wanted)))))
                        (dolist (directory wanted set) (setf (gethash directory set) t)))))
      (maphash (lambda (directory wd) (unless (gethash directory wanted-set) (push (cons directory wd) gone)))
               directories)
      (loop for (directory . wd) in gone
            do (remhash directory directories)
               (let ((left (remove directory (gethash wd wds) :test #'string=)))
                 (if left
                     (setf (gethash wd wds) left)
                     (progn (inotify-unwatch inotify wd) (remhash wd wds))))))
    (dolist (directory wanted)
      (unless (gethash directory directories)
        (let ((wd (inotify-watch inotify directory)))
          (when wd
            (setf (gethash directory directories) wd)
            (pushnew directory (gethash wd wds) :test #'string=)))))))

(defun rewatch-directories (watches wanted inotify directories wds items)
  "The directory watches for WANTED, native names of directories, keeping what
WATCHES knew of each, with the inotify watches of INOTIFY made and ended to
match those of ITEMS, the files watched, too. A directory no watch can be had
for is looked at every *POLL-INTERVAL* seconds."
  (let* ((now (get-internal-real-time))
         (known (let ((table (make-hash-table :test 'equal :size (max 16 (length watches)))))
                  (dolist (watch watches table) (setf (gethash (directory-watch-directory watch) table) watch))))
         (new (mapcar (lambda (directory)
                        (or (gethash directory known)
                            (make-directory-watch directory (ignore-errors (path-stamp directory)))))
                      wanted)))
    (sync-inotify-watches inotify directories wds
                          (unique-strings (append wanted (loop for item in items
                                                               unless (watch-item-polled item)
                                                                 append (mapcar #'car (watch-item-keys item))))))
    (dolist (watch new)
      ;; polled while no watch can be had, and no longer once one can
      (setf (directory-watch-polled watch) (not (gethash (directory-watch-directory watch) directories)))
      (when (and (directory-watch-polled watch) (null (directory-watch-due watch)))
        (setf (directory-watch-due watch) (+ now (seconds-units *poll-interval*)))))
    new))

(defun rewatch (items requests inotify directories wds &optional also)
  "The watch items for REQUESTS, (BUFFER PATH STAMP CLASS) lists, keeping what
ITEMS knew of each file, with the inotify watches of INOTIFY (DIRECTORIES from
their native names to watch descriptors, WDS the other way) made and ended to
match, and the watches of ALSO, directories watched whole, kept. A file whose
directories cannot all be watched, or on a network file system, is polled. A
file new here, or read or saved again, is looked at once straight away: a
change made before its watch began, or reported against the stamp before, is
not missed."
  (let* ((now (get-internal-real-time))
         (new (mapcar (lambda (request)
                        (destructuring-bind (buffer path stamp class) request
                          (let ((old (find-if (lambda (item) (and (eq (watch-item-buffer item) buffer)
                                                                  (equal (watch-item-path item) path)))
                                              items)))
                            (if old
                                (progn (unless (eq stamp (watch-item-stamp old))
                                         (setf (watch-item-stamp old) stamp (watch-item-reported old) stamp
                                               (watch-item-due old) now))
                                       old)
                                (let ((item (make-watch-item buffer path stamp (watch-keys path))))
                                  (setf (watch-item-reported item) stamp
                                        (watch-item-polled item) (eq class :remote)
                                        (watch-item-due item) now)
                                  item)))))
                      requests))
         (wanted (unique-strings (append also
                                         (loop for item in new
                                               unless (watch-item-polled item)
                                                 append (mapcar #'car (watch-item-keys item)))))))
    (sync-inotify-watches inotify directories wds wanted)
    (dolist (item new)
      (unless (every (lambda (key) (gethash (car key) directories)) (watch-item-keys item))
        (setf (watch-item-polled item) t))
      (when (and (watch-item-polled item) (null (watch-item-due item)))
        (setf (watch-item-due item) (+ now (seconds-units *poll-interval*)))))
    new))

(defun note-watch-event (items event wds directories now &optional inotify)
  "Set when to look at the ITEMS inotify's EVENT (WD MASK NAME) concerns: a
file written to once its writes settle (*SETTLE-TIME*), after any other change
once the burst it is part of is over (*BURST-TIME*), at most *SETTLE-LIMIT*
after the first event either way; everything at once after events were lost;
a directory's files when it went away, polled from then on (its watch ended
in INOTIFY, which goes on watching a directory moved)."
  (destructuring-bind (wd mask name) event
    (flet ((due-now (item) (setf (watch-item-due item) now)))
      (cond ((= wd -1)
             (when (logtest mask +in-queue-overflow+) (mapc #'due-now items)))
            (t
             (let ((names (gethash wd wds)))
               (when names
                 (if (logtest mask +in-directory-gone+)
                     (progn (remhash wd wds)
                            (when inotify (ignore-errors (inotify-unwatch inotify wd)))
                            (dolist (directory names) (remhash directory directories))
                            (dolist (item items)
                              (when (find-if (lambda (key) (member (car key) names :test #'string=)) (watch-item-keys item))
                                (setf (watch-item-polled item) t)
                                (due-now item))))
                     (when name
                       (dolist (item items)
                         (when (find-if (lambda (key) (and (member (car key) names :test #'string=) (equalp (cdr key) name)))
                                        (watch-item-keys item))
                           (let ((first (or (watch-item-first item) now))
                                 (quiet (if (= mask +in-modify+) *settle-time* *burst-time*)))
                             (setf (watch-item-first item) first
                                   (watch-item-due item) (min (+ now (seconds-units quiet))
                                                              (+ first (seconds-units *settle-limit*))))))))))))))))

(defun note-directory-event (watches event wds now)
  "Set when to look at the directories of WATCHES inotify's EVENT (WD MASK
NAME) concerns: once the burst of changes it is part of is over, at most
*SETTLE-LIMIT* after the first; a file still being written (IN_MODIFY alone)
is no change to the list; everything after events were lost; a directory gone
is looked at every *POLL-INTERVAL* seconds from then on."
  (destructuring-bind (wd mask name) event
    (declare (ignore name))
    (flet ((due (watch)
             (let ((first (or (directory-watch-first watch) now)))
               (setf (directory-watch-first watch) first
                     (directory-watch-due watch) (min (+ now (seconds-units *burst-time*))
                                                      (+ first (seconds-units *settle-limit*)))))))
      (cond ((= wd -1) (when (logtest mask +in-queue-overflow+) (mapc #'due watches)))
            ((= mask +in-modify+))
            (t (let ((names (gethash wd wds)))
                 (dolist (watch watches)
                   (when (member (directory-watch-directory watch) names :test #'string=)
                     (when (logtest mask +in-directory-gone+) (setf (directory-watch-polled watch) t))
                     (due watch)))))))))

(defun look-at-due-directories (watches now)
  "The directories of WATCHES whose time has come that changed: every one
told of by inotify, and a polled one when its stamp is not as last seen."
  (loop for watch in watches
        for due = (directory-watch-due watch)
        when (and due (>= now due))
          nconc (let ((stamp (ignore-errors (path-stamp (directory-watch-directory watch))))
                      (told (directory-watch-first watch)))
                  (setf (directory-watch-first watch) nil
                        (directory-watch-due watch) (and (directory-watch-polled watch) (+ now (seconds-units *poll-interval*))))
                  (when (or told
                            (let ((old (directory-watch-stamp watch)))
                              ;; not there, or not readable, as before: no news
                              (and (or stamp old)
                                   (not (and stamp old
                                             (= (stamp-mtime stamp) (stamp-mtime old))
                                             (= (stamp-ino stamp) (stamp-ino old)))))))
                    (setf (directory-watch-stamp watch) stamp)
                    (list (directory-watch-directory watch))))))

(defun look-at-due (items now)
  "Look at the ITEMS whose time has come, and with them those due within
*BURST-TIME*, so that files changed together are reported together; the
results of those whose file is not as last reported, remembering it as
reported."
  (loop with soon = (and (some (lambda (item) (let ((due (watch-item-due item))) (and due (>= now due)))) items)
                         (+ now (seconds-units *burst-time*)))
        for item in items
        for due = (watch-item-due item)
        when (and soon due (<= due soon))
          nconc (let ((result (look-at (watch-item-buffer item) (watch-item-path item) (watch-item-stamp item))))
                  (setf (watch-item-first item) nil
                        (watch-item-due item) (and (watch-item-polled item) (+ now (seconds-units *poll-interval*))))
                  (let ((found (fourth result)) (reported (watch-item-reported item)))
                    (unless (or (eq found :unknown)
                                (and (null found) (null reported))
                                (and found reported (same-content-p found reported)))
                      (setf (watch-item-reported item) found)
                      (list result))))))

(defun tell-editor ()
  "Call *ON-FILE-NEWS*, if set, for the editor to take in what the watcher found."
  (let ((function *on-file-news*))
    (when function (ignore-errors (funcall function)))))

(defun soonest-due (items)
  "The soonest time any of ITEMS is due to be looked at, or NIL when none is."
  (let ((soonest nil))
    (dolist (item items soonest)
      (let ((due (watch-item-due item)))
        (when (and due (or (null soonest) (< due soonest)))
          (setf soonest due))))))

(defun watcher-loop ()
  "What the watcher thread does: take requests, read inotify's events, look at
the files whose time has come, report what changed, and wait for the next of
these, which may be a long time: no looking without cause but for polled files."
  (let ((inotify (inotify-open))
        (buffer (make-array 65536 :element-type '(unsigned-byte 8)))
        (directories (make-hash-table :test 'equal))
        (wds (make-hash-table))
        (items '())
        (watches '()))
    (loop
      (drain-wake-pipe *watcher-wake*)
      (dolist (request (sb-concurrency:receive-pending-messages *watcher-requests*))
        (ecase (first request)
          (:watch (setf items (rewatch items (second request) inotify directories wds
                                       (mapcar #'directory-watch-directory watches))))
          (:directories (setf watches (rewatch-directories watches (second request) inotify directories wds items)))))
      (let ((now (get-internal-real-time)))
        (when inotify
          (dolist (event (inotify-events inotify buffer))
            ;; the lists' first: the files' forgets a directory gone
            (note-directory-event watches event wds now)
            (note-watch-event items event wds directories now inotify)))
        (let ((found (look-at-due items now)))
          (when found
            (sb-concurrency:send-message *look-results* (cons :news found))
            (tell-editor)))
        (let ((changed (look-at-due-directories watches now)))
          (when changed
            (sb-concurrency:send-message *directory-news* changed)
            (tell-editor)))
        (let ((soonest (let ((times (remove nil (cons (soonest-due items) (mapcar #'directory-watch-due watches)))))
                         (and times (reduce #'min times)))))
          (wait-for-fds (if inotify (list inotify (car *watcher-wake*)) (list (car *watcher-wake*)))
                        (and soonest (/ (max 0 (- soonest now)) internal-time-units-per-second))))))))

(defun send-watcher (request)
  "Send REQUEST to the watcher, starting it first when needed, and wake it."
  (unless *watcher-wake* (setf *watcher-wake* (make-wake-pipe)))
  (unless (and *watcher* (sb-thread:thread-alive-p *watcher*))
    (setf *watcher* (tuatara.runtime:make-guarded-thread #'watcher-loop :name "tuatara file watcher")))
  (sb-concurrency:send-message *watcher-requests* request)
  (wake *watcher-wake*))

(defun watch-files (buffers)
  "Have the watcher watch the files of BUFFERS on disk from now on, and no
others, telling it only when something changed since the last call: a file
visited, killed, saved or read again. Files of pseudo file systems (/proc) are
not watched: they change as they are read."
  (flet ((watched-p (buffer)
           (and (tuatara.buffer:buffer-file buffer) (not (eq (tuatara.buffer:buffer-local buffer :file-class) :pseudo))))
         (item (buffer)
           (list buffer (tuatara.buffer:buffer-file buffer) (tuatara.buffer:buffer-local buffer :file-stamp)
                 (tuatara.buffer:buffer-local buffer :file-class))))
    (unless (let ((watched *watched*))
              (and (loop for buffer in buffers
                         always (or (not (watched-p buffer))
                                    (and watched
                                         (destructuring-bind (was-buffer path stamp class) (pop watched)
                                           (and (eq was-buffer buffer) (equal path (tuatara.buffer:buffer-file buffer))
                                                (eq stamp (tuatara.buffer:buffer-local buffer :file-stamp))
                                                (eq class (tuatara.buffer:buffer-local buffer :file-class)))))))
                   (null watched)))
      (setf *watched* (loop for buffer in buffers when (watched-p buffer) collect (item buffer)))
      (send-watcher (list :watch *watched*)))))

(defun watch-directories (directories)
  "Have the watcher watch DIRECTORIES (native names ending in /) whole from now
on, and no others, for lists of them, telling it only when that changed: what
changes in them comes back through TAKE-DIRECTORY-NEWS. Tens of thousands of
directories are compared in time growing with their number alone."
  (let ((wanted (unique-strings directories)))
    (unless (and (= (length wanted) (length *watched-directories*))
                 (let ((watched (make-hash-table :test 'equal :size (max 16 (length wanted)))))
                   (dolist (d *watched-directories*) (setf (gethash d watched) t))
                   (every (lambda (d) (gethash d watched)) wanted)))
      (setf *watched-directories* wanted)
      (send-watcher (list :directories wanted)))
    wanted))

(defun take-directory-news ()
  "The directories (WATCH-DIRECTORIES) the watcher found changed since this
was last asked, native names ending in /."
  (remove-duplicates (loop for message in (sb-concurrency:receive-pending-messages *directory-news*) append message)
                     :test #'string=))

(defun take-file-looks ()
  "Note what the watcher found in each buffer's FILE-DISK-STATE, as CHECK-FILE
does, skipping buffers saved, reverted or killed since, and those being saved
(the change is the save). Returns the buffers whose state changed, and as a
second value those whose file changed again without the state changing (a
file that goes on growing)."
  (let ((changed '()) (again '()))
    (dolist (message (sb-concurrency:receive-pending-messages *look-results*))
      (dolist (result (cdr message))
        (destructuring-bind (buffer path stamp now) result
          (when (and (tuatara.buffer:live-buffer-p buffer)
                     (equal path (tuatara.buffer:buffer-file buffer))
                     (eq stamp (tuatara.buffer:buffer-local buffer :file-stamp))
                     (not (eq now :unknown))
                     (not (saving-p buffer)))
            (multiple-value-bind (state new) (note-disk-status buffer (disk-status buffer stamp now))
              (cond (new (pushnew buffer changed))
                    (state (pushnew buffer again))))))))
    (values (nreverse changed) (nreverse (set-difference again changed)))))

;;; Saving

(define-condition file-changed-on-disk (error)
  ((path :initarg :path :reader file-changed-on-disk-path :documentation "The file."))
  (:report (lambda (c stream)
             (format stream "~a changed on disk since it was read or saved" (file-changed-on-disk-path c))))
  (:documentation "A save found the file changed by someone else since the buffer read or saved it."))

(setf (documentation 'file-changed-on-disk-path 'function) "The file a FILE-CHANGED-ON-DISK concerns.")

(defun resolve-links (path)
  "PATH with symbolic links followed to the file they name, even one not there
yet, so that saving writes that file and the link stays a link."
  (let ((path (expand-path path)))
    (loop repeat 40
          do (let ((stamp (ignore-errors (path-stamp path :follow nil))))
               (unless (and stamp (eq (stamp-kind stamp) :symlink))
                 (return))
               (let ((target (sb-posix:readlink (native path))))
                 ;; a link's target is taken literally: a ~ there is a directory's name
                 (setf path (expand-path target :directory (parent-path path) :tilde nil)))))
    path))

(defun directory-class (directory)
  "FILE-SYSTEM-CLASS of the file system holding DIRECTORY."
  (file-system-class (path-file-system directory)))

(defconstant +block-size+ 4096
  "The block size file systems share blocks in: copies of whole blocks at the
same place within a block in both files can share them.")

(defvar *write-out-window* (* 32 1024 1024)
  "Octets a save hands the system before it has them written out to the device
and waits for the ones before: the most a save leaves in memory not yet on
disk, and so how far its progress can be ahead of the device.")

(defun write-rope (fd rope &key (path "a file") cancel progress)
  "Write ROPE's octets to the open file FD, from offset 0 on: what comes from
mapped files is copied by the kernel where it can, the rest written. Where a
stretch from a file sits at the same place within a block as where it goes (after
edits that kept the length, or none), it is copied up to the next block boundary
first, so that file systems that share blocks share all the whole ones after it.
The octets go out to the device as they are written, *WRITE-OUT-WINDOW* at a
time, and leave memory once there, so that a slow device (a USB drive) holds a
large save back as it goes rather than all at the end, and memory does not fill
with what waits for it. PROGRESS is called with the octets on the device and
the total. Returns true, or NIL when CANCEL's car turned true first."
  (let ((done 0) (total (tuatara.text:rope-bytes rope))
        (written-out 0)                 ; below this, on the device
        (going-out 0)                   ; below this, on its way there
        (can-send t))                   ; NIL on a file system that cannot be asked
    (flet ((move-on ()
             ;; once a window more is written: send it out, wait for the one before
             (when (and can-send (>= (- done going-out) *write-out-window*))
               (if (sync-range fd going-out (- done going-out))
                   (progn
                     (when (sync-range fd written-out (- going-out written-out) :wait t)
                       (drop-cached fd written-out (- going-out written-out))
                       (setf written-out going-out))
                     (setf going-out done))
                   (setf can-send nil)))
             (when progress (funcall progress (if can-send written-out done) total))))
      (block write
        (tuatara.text:map-rope-sources
         (lambda (kind a b c)
           (when (and cancel (car cancel)) (return-from write nil))
           (ecase kind
             (:memory (write-octets fd a :start b :end c :path path) (incf done (- c b)))
             (:file (let ((mapping a) (offset b) (length c))
                      (loop while (plusp length)
                            do (when (and cancel (car cancel)) (return-from write nil))
                               (let* ((misaligned (mod offset +block-size+))
                                      (n (if (and (plusp misaligned) (= misaligned (mod done +block-size+)))
                                             (min length (- +block-size+ misaligned))
                                             (min length *write-out-window*))))
                                 (let ((copied (copy-fd-range (tuatara.text:mapping-fd mapping) offset fd n)))
                                   (when (< copied n)
                                     (write-sap fd (sb-sys:sap+ (tuatara.text:mapping-sap mapping) (+ offset copied))
                                                (- n copied) :path path)))
                                 (incf offset n) (decf length n) (incf done n)
                                 (move-on))))))
           (move-on))
         rope)
        t))))

(defun flush-fd (fd path)
  "Ask the system to put the open file FD on disk; file systems that cannot are let be."
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
            fd (call-with-folder directory
                                 (lambda (dirfd)
                                   (multiple-value-bind (fd errno)
                                       (openat-octets dirfd (tuatara.text:string-octets (native-file-name temp))
                                                      (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl +o-nofollow+ +o-cloexec+)
                                                      made-mode)
                                     (or fd (fail "create a file in" directory errno))))
                                 "save in")))
    (flet ((abandon ()
             (when fd (ignore-errors (sb-posix:close fd)) (setf fd nil))
             (when temp (ignore-errors (sb-posix:unlink temp)))))
      (handler-bind ((error (lambda (e) (declare (ignore e)) (abandon))))
        (unless (write-rope fd rope :path target :cancel cancel :progress progress)
          (abandon)
          (return-from write-by-rename :stopped))
        (flush-fd fd target)
        (when progress (funcall progress (tuatara.text:rope-bytes rope) (tuatara.text:rope-bytes rope)))
        (when existing
          ;; the owner first: changing it may clear set-id bits the mode then restores
          (ignore-errors (sb-posix:fchown fd (stamp-uid existing) (stamp-gid existing)))
          (ignore-errors (sb-posix:fchmod fd (logand (stamp-mode existing) #o7777)))
          (let ((source (ignore-errors (open-examined +at-fdcwd+ (tuatara.text:string-octets (native target))
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
        ;; renamed within the folder held open, looked at again where it really is
        (call-with-parent
         target
         (lambda (dirfd own)
           (with-octets-sap (old (terminated-octets (tuatara.text:string-octets (native-file-name temp))))
             (with-octets-sap (new (terminated-octets own))
               (multiple-value-bind (result errno) (rename-entry dirfd old dirfd new 0)
                 (unless (zerop result)
                   (if (member errno (list sb-posix:exdev sb-posix:eperm sb-posix:eacces sb-posix:ebusy sb-posix:eopnotsupp
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
         ;; the process and a counter: two saves at once, of files of one name, have a backup each
         (path (format nil "~a/.~a.tuatara-backup-~d-~d" (string-right-trim "/" (native place))
                       (native-file-name target) (sb-posix:getpid) (sb-ext:atomic-incf (car **scratch-counter**))))
         ;; made anew, never through a link put there: one left by a crash taken away first
         (fd (flet ((make () (sb-posix:open path (logior sb-posix:o-rdwr sb-posix:o-creat sb-posix:o-excl sb-posix:o-nofollow) #o600)))
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
        (ignore-errors (sb-posix:close fd))
        (ignore-errors (sb-posix:unlink path))))
    (and done (values fd path))))

(defun write-in-place (rope target existing &key mappings (backup t) (sync t) whole cancel progress nofollow mode)
  "Overwrite TARGET with ROPE where it is, for a file that must keep its inode
or its owner; with NOFOLLOW, never through a symbolic link at TARGET. MODE, a
function of the file's permissions, gives those it gets instead. With BACKUP, a copy of the old content is made first and MAPPINGS
still reading TARGET's octets move onto it, so that nothing reads the file
while it is rewritten; the copy is removed once the new content is on disk, and
kept, with its name in the error, when the write fails. WHOLE writes the text
with one call, as the files of pseudo file systems want. CANCEL is heeded until
the file itself is touched. Returns :DONE, or :STOPPED."
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
               (ignore-errors (sb-posix:close backup-fd))
               (ignore-errors (sb-posix:unlink backup-path))
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
      (let ((fd (handler-case (sb-posix:open (native target)
                                             (logior (cond ((not existing) (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-excl))
                                                           (backup sb-posix:o-wronly)
                                                           (t (logior sb-posix:o-wronly sb-posix:o-trunc)))
                                                     sb-posix:o-noctty (if nofollow +o-nofollow+ 0))
                                             #o666)
                  (sb-posix:syscall-error (e)
                    ;; nothing was written: the backup is not needed
                    (drop-backup)
                    (fail "open for writing" target (sb-posix:syscall-errno e))))))
        (handler-case
            (unwind-protect
                 (progn
                   ;; where the file opened really is, whatever names led to it meanwhile
                   (check-writing target "write" :fd fd)
                   (if whole
                       (write-octets fd (tuatara.text:rope-octets rope) :path target)
                       (write-rope fd rope :path target :progress progress))
                   (when (and existing backup)
                     (handler-case (sb-posix:ftruncate fd (tuatara.text:rope-bytes rope))
                       (sb-posix:syscall-error (e) (fail "shorten" target (sb-posix:syscall-errno e)))))
                   (when mode
                     (sb-posix:fchmod fd (funcall mode (logand (stamp-mode (fd-stamp fd target)) #o7777))))
                   (when sync (flush-fd fd target))
                   (when progress (funcall progress (tuatara.text:rope-bytes rope) (tuatara.text:rope-bytes rope))))
              (sb-posix:close fd))
          (writing-refused (e)
            (drop-backup)
            ;; a file made here for this write, empty, is not left behind
            (unless existing (ignore-errors (sb-posix:unlink (native target))))
            (error e))
          (error (e)
            (when backup-fd (ignore-errors (sb-posix:close backup-fd)))
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
is simply written again). MAPPINGS are those still reading PATH's octets, moved
to a copy before any write in place. With FOLLOW-LINKS NIL, PATH itself is
written: a symbolic link there, found first or put there while the file is
written, is refused, never followed. MODE, a function of the permissions the
file would have (a number, as #o644), gives those it gets instead. Returns the
new stamp and the file written (a symlink's target), or :STOPPED when CANCEL
stopped it with the file untouched. Refused (WRITING-REFUSED) where nothing
may be written, looked at again on the file opened when it is written in
place. While some place is refused (WRITING-REFUSED-ANYWHERE-P), a file with
other names is never written in place, for one of them may be in such a
place: it is saved as a file of its own, its other names keeping what they
held, and that is told (TELL-WRITING)."
  (let* ((target (if follow-links (resolve-links path) (expand-path path)))
         (existing (progn (check-writing target "save")
                          (path-stamp target :follow follow-links)))
         (directory (parent-path target))
         (class (directory-class directory)))
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
                    (write-in-place rope target existing :backup nil :sync nil :whole t
                                                         :nofollow (not follow-links) :mode mode))
                   ((and (not apart)
                         (or (and existing (> (stamp-nlink existing) 1))
                             (and existing (not (can-give-owner-p existing)))
                             (not (writable-p directory))))
                    (write-in-place rope target existing :mappings mappings :cancel cancel :progress progress
                                                         :nofollow (not follow-links) :mode mode))
                   (t (let ((r (write-by-rename rope target existing :cancel cancel :progress progress
                                                                     :nofollow (not follow-links) :mode mode)))
                        (cond ((and (eq r :refused) apart)
                               (error 'writing-refused :operation "save" :path target
                                                       :reason (format nil "it has ~d names, one may be in an evidence folder, and it cannot be saved as a file of its own here" names)))
                              ((eq r :refused)
                               (write-in-place rope target existing :mappings mappings :cancel cancel :progress progress
                                                                    :nofollow (not follow-links) :mode mode))
                              (t (when (and apart (eq r :done))
                                   (tell-writing (format nil "~a had ~d names: saved as a file of its own, the others keep what they held, as one of them may be evidence"
                                                         target names)))
                                 r)))))))
      (if (eq result :stopped)
          :stopped
          (values (path-stamp target :follow follow-links) target)))))

(defun protected-mappings (buffer)
  "The mappings of BUFFER that read its file's own octets, which a write in place must move first."
  (let ((mapping (tuatara.buffer:buffer-mapping buffer)))
    (and mapping (member (file-isolation buffer) '(:direct :pending :leased)) (list mapping))))

(defun record-save (buffer path stamp content rope)
  "Note that BUFFER's text of content number CONTENT, ROPE, is saved as PATH,
now at STAMP. A buffer that read its old file directly no longer does (the old
file has no name now, or the buffer reads the backup), so it counts as a
snapshot."
  (setf (tuatara.buffer:buffer-file buffer) path
        (tuatara.buffer:buffer-local buffer :file-stamp) stamp
        (tuatara.buffer:buffer-local buffer :disk-state) nil)
  (note-file-tail buffer rope)
  (tuatara.buffer:buffer-mark-saved buffer content)
  (when (member (file-isolation buffer) '(:direct :pending :leased))
    (stop-snapshots-of buffer)
    (give-up-leases (lambda (lease) (eq (lease-buffer lease) buffer)))
    (setf (tuatara.buffer:buffer-local buffer :file-isolation) :snapshot)))

(defun check-savable (buffer path)
  "Signal an error unless BUFFER can be saved as PATH: it has a file or PATH,
and its text is still what was read (not stale)."
  (unless path (error "Buffer ~a has no file to save to." (tuatara.buffer:buffer-name buffer)))
  (when (eq (file-disk-state buffer) :stale)
    (error "Buffer ~a cannot be saved: its file was rewritten on disk while the buffer was shown from it, ~
so its text is no longer what was read. Revert it to read the file again."
           (tuatara.buffer:buffer-name buffer))))

(defun save-expected (buffer path force)
  "What a save of BUFFER as PATH checks the file on disk against: the stamp the
buffer read or wrote, or :ANY when FORCE or when PATH is another file."
  (if (or force (not (equal path (tuatara.buffer:buffer-file buffer))))
      :any
      (tuatara.buffer:buffer-local buffer :file-stamp)))

(defvar *save-transform-function* nil
  "A function of a buffer and the path it is being saved as, called on the
editor's thread as the save begins, returning NIL to write its text as it is,
or a function of its text (a rope) returning what is written instead (octets
or a rope), called on the thread that writes: an encrypted file's text
encrypted before anything touches the disk. It may signal an error to refuse
the save.")

(defun save-transform (buffer path)
  "The transform of BUFFER's text for its save as PATH (*SAVE-TRANSFORM-FUNCTION*), or NIL."
  (and *save-transform-function* (funcall *save-transform-function* buffer path)))

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
         (written (transformed-text (save-transform buffer path) (tuatara.text:version-rope version))))
    (multiple-value-bind (stamp target)
        (write-text-file written path :expected (save-expected buffer path force) :mappings (protected-mappings buffer))
      (record-save buffer path stamp content written)
      target)))

;;; Saving in the background

;; a save of the buffer is a job in the background (TUATARA.RUNTIME:JOB)
(defstruct (save-job (:include tuatara.runtime:job) (:constructor %make-save-job) (:copier nil))
  "A save of BUFFER's text at VERSION (of content number CONTENT) as PATH
running in the background, a job (TUATARA.RUNTIME:JOB): DONE of TOTAL octets
written; STAMP and TARGET, the file written, once done."
  path version content (done 0) (total 0) stamp target written)

(setf (documentation 'save-job-p 'function) "True when OBJECT is a SAVE-JOB."
      (documentation 'save-job-buffer 'function) "The buffer a save job saves."
      (documentation 'save-job-status 'function) "A save job's status: :RUNNING, :DONE, :STOPPED or :FAILED."
      (documentation 'save-job-error 'function) "Why a save job failed, in words."
      (documentation 'save-job-path 'function) "The file a save job writes."
      (documentation 'save-job-target 'function) "The file a finished save job wrote: a symlink's target."
      (documentation 'save-job-total 'function) "The octets a save job writes."
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
  :description (format nil "Saving ~a to ~a" (tuatara.buffer:buffer-name (save-job-buffer job)) (native (save-job-path job)))
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
                 (or (save-job-written job) (tuatara.text:version-rope (save-job-version job))))))

(defun start-save (buffer &key (path (tuatara.buffer:buffer-file buffer)) force)
  "Save BUFFER as PATH in the background: its text as it is now is written while
the buffer stays usable, edits included. Checks the file on disk first as
SAVE-BUFFER-FILE does. Returns the job; FINISH-SAVES takes it in."
  (check-savable buffer path)
  (when (save-of buffer)
    (error "Buffer ~a is already being saved." (tuatara.buffer:buffer-name buffer)))
  (let* ((path (expand-path path))
         (expected (save-expected buffer path force)))
    (unless (or (eq expected :any) (eq (tuatara.buffer:buffer-local buffer :file-class) :pseudo))
      (let ((existing (ignore-errors (path-stamp (resolve-links path)))))
        (unless (or (null existing) (and expected (same-content-p expected existing)))
          (error 'file-changed-on-disk :path path))))
    (let* ((version (tuatara.buffer:buffer-version buffer))
           (transform (save-transform buffer path))
           (job (%make-save-job :buffer buffer :path path :version version
                                :content (tuatara.buffer:buffer-content-number buffer)
                                :total (tuatara.text:rope-bytes (tuatara.text:version-rope version))))
           (mappings (protected-mappings buffer)))
      (tuatara.runtime:start-job-thread
       job (lambda (job)
             (setf (save-job-written job) (transformed-text transform (tuatara.text:version-rope version)))
             (multiple-value-bind (stamp target)
                 (write-text-file (save-job-written job) path :expected :any :mappings mappings
                                  :cancel (save-job-cancel job)
                                  :progress (lambda (done total) (declare (ignore total))
                                              (setf (save-job-done job) done)))
               (if (eq stamp :stopped)
                   (setf (save-job-status job) :stopped)
                   (setf (save-job-stamp job) stamp (save-job-target job) target (save-job-status job) :done))))
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
                   (some (lambda (lease) (and (eq (lease-buffer lease) buffer) (eq (lease-status lease) :moving))) *leases*))
             (setf (tuatara.buffer:buffer-lent-p buffer) t)
             (lend-file-guards buffer))))
  (let ((job (save-of buffer)))
    (when job
      ;; a save that does not end in time (a write stuck on a hung mount) is left to end on its own, the
      ;; file's mapping kept for it
      (unless (tuatara.runtime:join-within (save-job-thread job))
        (setf (tuatara.buffer:buffer-lent-p buffer) t)
        (lend-file-guards buffer)
        (tuatara.runtime:left-running (save-job-thread job) (format nil "Saving ~a" (tuatara.buffer:buffer-name buffer)))))))

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

(pushnew 'settle-file-work tuatara.buffer:*kill-buffer-functions*)

;;; Reverting

(defun revert-buffer-file (buffer &key map)
  "Read BUFFER's file again and make it the buffer's text: one undoable step,
unless the text was stale (then the history goes too, being no longer
trustworthy). The buffer is then unmodified and as its file. MAP, when given,
is a function of the old text and the new (ropes) returning the functions
BUFFER-SWAP-TEXT maps positions with, forward and back; by default a position
stays, cut to the new text's size. *BEFORE-EDIT-FUNCTIONS* see the whole text
changed before the file is read, and may refuse it. A buffer whose text is
not its file's octets (an encrypted file's, decrypted) is read again by its
own :REREAD function instead. A narrowed buffer is widened first. Returns
BUFFER."
  ;; the file's text comes back whole, shown whole
  (tuatara.buffer:widen-buffer buffer)
  (let ((reread (tuatara.buffer:buffer-local buffer :reread)))
    (when reread
      (funcall reread buffer)
      (return-from revert-buffer-file buffer)))
  (let* ((path (or (tuatara.buffer:buffer-file buffer)
                   (error "Buffer ~a visits no file." (tuatara.buffer:buffer-name buffer))))
         (stale (eq (file-disk-state buffer) :stale))
         (safe (safe-visit-p buffer))
         (read-only (if stale
                        (tuatara.buffer:buffer-local buffer :read-only-before-stale)
                        (tuatara.buffer:buffer-local buffer :read-only)))
         (old-size (tuatara.buffer:buffer-size buffer)))
    (when (save-of buffer)
      (error "Buffer ~a is being saved; revert it once the save is done." (tuatara.buffer:buffer-name buffer)))
    (tuatara.buffer:run-before-edit buffer :all)
    (stop-snapshots-of buffer)
    (multiple-value-bind (rope mapping stamp isolation guard class) (open-file-text path :safe safe)
      (let ((new-size (tuatara.text:rope-bytes rope)) (done nil))
        ;; the file's own text coming back: a read-only buffer takes it too
        (tuatara.buffer:with-read-only-lifted (buffer :after (if done (or safe read-only) (or stale read-only)))
          (multiple-value-bind (forward backward)
              (if map
                  (funcall map (tuatara.buffer:buffer-text buffer) rope)
                  (values (lambda (position bias) (declare (ignore bias)) (min position new-size))
                          (lambda (position bias) (declare (ignore bias)) (min position old-size))))
            (let ((tuatara.buffer:*hex-view-resizing* t))
              (tuatara.buffer:buffer-swap-text buffer rope forward backward))
            (setf done t)))
        (when stale
          (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '()))
        (setf (tuatara.buffer:buffer-mapping buffer) mapping
              (tuatara.buffer:buffer-local buffer :file-stamp) stamp
              (tuatara.buffer:buffer-local buffer :file-isolation) isolation
              (tuatara.buffer:buffer-local buffer :file-class) class
              (tuatara.buffer:buffer-local buffer :line-ending) (tuatara.buffer:detect-line-ending rope)
              (tuatara.buffer:buffer-local buffer :disk-state) nil)
        (note-file-tail buffer rope)
        (tuatara.buffer:buffer-mark-saved buffer)
        ;; the old text's lease serves the old text alone now, given up once nothing reads it
        (lend-file-guards buffer)
        (attend guard buffer)
        buffer))))

;;; A file that grew

(defun add-appended (buffer added stamp tail)
  "Add ADDED, the octets BUFFER's file grew by, to the end of BUFFER's text for
APPEND-FROM-DISK, the file being now at STAMP, its end before TAIL. Appends
one after another with nothing done between undo together."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (at-end (= (tuatara.buffer:buffer-point buffer) size))
         (unmodified (not (tuatara.buffer:buffer-modified-p buffer)))
         (stale (eq (file-disk-state buffer) :stale))
         (read-only (tuatara.buffer:buffer-local buffer (if stale :read-only-before-stale :read-only)))
         (group (eql (tuatara.buffer:buffer-content-number buffer) (tuatara.buffer:buffer-local buffer :appended))))
    (tuatara.buffer:with-read-only-lifted (buffer :after read-only)
      (tuatara.buffer:buffer-insert buffer size added :group group))
    (when at-end
      (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-size buffer)))
    (when unmodified (tuatara.buffer:buffer-mark-saved buffer))
    (setf (tuatara.buffer:buffer-local buffer :appended) (tuatara.buffer:buffer-content-number buffer)
          (tuatara.buffer:buffer-local buffer :file-stamp) stamp
          (tuatara.buffer:buffer-local buffer :disk-state) nil
          (tuatara.buffer:buffer-local buffer :file-tail)
          (if (>= (length added) +tail-sample+)
              (subseq added (- (length added) +tail-sample+))
              (let ((end (concatenate 'tuatara.text:octets tail added)))
                (subseq end (max 0 (- (length end) +tail-sample+))))))))

(defun append-from-disk (buffer)
  "When BUFFER's file on disk has only grown since the buffer read or saved
it (the same file, larger, still holding the octets it ended with), add what
it grew by to the end of the buffer's text as one undoable step, even when the
buffer has changes of its own, and note the file as read to its new end. Point
at the end stays at the end, following the file as it grows; an unmodified
buffer stays unmodified, and a buffer the growth made stale is itself again.
Returns how many octets were added, or NIL, nothing done, when the file
changed otherwise or grew by more than *MEMORY-READ-LIMIT* octets."
  (let ((path (tuatara.buffer:buffer-file buffer))
        (recorded (tuatara.buffer:buffer-local buffer :file-stamp))
        (tail (tuatara.buffer:buffer-local buffer :file-tail)))
    (when (and path recorded tail (not (saving-p buffer)) (not (tuatara.buffer:buffer-local buffer :reread)))
      (let ((fd (handler-case (open-for-reading path (safe-visit-p buffer) :accept '(:regular)) (file-system-error () nil))))
        (when fd
          (unwind-protect
               (let* ((now (fd-stamp fd path))
                      (old (stamp-size recorded))
                      (grown (- (stamp-size now) old)))
                 (when (and (eq (stamp-kind now) :regular)
                            (= (stamp-dev now) (stamp-dev recorded)) (= (stamp-ino now) (stamp-ino recorded))
                            (< 0 grown) (<= grown *memory-read-limit*) (<= (length tail) old))
                   (let ((check (make-array (length tail) :element-type '(unsigned-byte 8)))
                         (added (make-array grown :element-type '(unsigned-byte 8))))
                     (when (and (= (read-fd-at fd check (- old (length tail)) :path path) (length tail))
                                (equalp check tail)
                                (= (read-fd-at fd added old :path path) grown))
                       (add-appended buffer added now tail)
                       grown))))
            (sb-posix:close fd)))))))
