;;;; test/files.lisp --- Visiting, saving and reverting files, on every kind of file system.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test visiting-and-saving
  :description "Files: visiting keeps the text safe from other programs' changes; saving never loses a file and keeps its permissions, links and owner; changes on disk are noticed without holding the editor up; reverting reads the file again.")

(defun kill-buffers-under (directory)
  "Kill the buffers visiting a file in DIRECTORY or below it, changed or not,
so that none is left for a later test to find."
  (let ((prefix (namestring directory)))
    (dolist (buffer (copy-list tuatara.buffer:*buffers*))
      (let ((file (tuatara.buffer:buffer-file buffer)))
        (when (and file (uiop:string-prefix-p prefix (namestring file)))
          (ignore-errors (tuatara.buffer:kill-buffer buffer)))))))

(defun forget-kept-indexes (directory)
  "Remove the indexes kept on disk for DIRECTORY and the directories in it
(those of a directory kept read in a test), so that no test leaves one in the
cache directory."
  (let ((indexes (merge-pathnames "indexes/" (tuatara.io:cache-directory)))
        (prefix (tuatara.io:path-file-name (namestring directory))))
    (dolist (kept (ignore-errors (uiop:subdirectories indexes)))
      (let ((name (car (last (pathname-directory kept)))))
        (when (and (stringp name) (uiop:string-prefix-p prefix name))
          (uiop:delete-directory-tree kept :validate (lambda (p) (search "/indexes/" (namestring p)))))))))

(defun forget-committed-lines ()
  "Forget the line hashes the gutter keeps of the texts git has. They are kept
by the text, whichever repository holds it, so that a text one test committed
is found read already by another that commits the same."
  (sb-ext:with-locked-hash-table (tuatara.defaults::**committed-lines**)
    (clrhash tuatara.defaults::**committed-lines**)))

(defun thread-cpu-time ()
  "The processor time this thread has used, in internal time units, less the
time spent collecting garbage: what work in the thread costs however busy the
rest of the machine is and however full the heap the tests before it left,
for a bound on work that does not wait for another thread or program. The
garbage the work makes is bounded by counting it, not by this."
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-thread-cputime-id)
    (- (+ (* seconds internal-time-units-per-second)
          (floor (* nanoseconds internal-time-units-per-second) 1000000000))
       sb-ext:*gc-run-time*)))

(defvar *slowdown-sink* 0
  "Where MACHINE-SLOWDOWN leaves its sum, so that the work is not dropped.")

(defun machine-slowdown ()
  "How many times longer than its processor time a short piece of work takes
on this thread now: 1 when the machine has a processor to spare, more when
other programs share it. A bound on how long something that waits for another
thread may take is multiplied by it."
  (let ((real (get-internal-real-time))
        (cpu (thread-cpu-time))
        (sum 0))
    (declare (fixnum sum))
    (dotimes (i 20000000) (setf sum (logxor sum i)))
    (setf *slowdown-sink* sum)
    (max 1 (/ (- (get-internal-real-time) real) (max 1 (- (thread-cpu-time) cpu))))))

(defun forget-trusted-under (directory)
  "Take the projects within DIRECTORY out of those trusted when asked."
  (let ((top (string-right-trim "/"
                                (or (tuatara.defaults::real-directory (namestring directory)) (namestring directory)))))
    (tuatara.defaults::state-list-drop
     tuatara.defaults::*trusted*
     (remove-if-not (lambda (entry) (and (stringp entry) (tuatara.io:path-within-p entry top)))
                    (tuatara.defaults::state-list-entries tuatara.defaults::*trusted*)))))

(defmacro with-test-directory ((directory) &body body)
  "Run BODY with DIRECTORY bound to a new empty directory in the cache directory
(on the file system of your home, which shares blocks on btrfs), removed
afterwards with everything in it, the buffers visiting its files killed and
the projects within it trusted no more."
  `(let ((,directory (ensure-directories-exist
                      (merge-pathnames (format nil "test-~d-~36r/" (sb-posix:getpid) (random (expt 36 8) (load-time-value (make-random-state t))))
                                       (tuatara.io:cache-directory)))))
     (unwind-protect (progn ,@body)
       (kill-buffers-under ,directory)
       (forget-kept-indexes ,directory)
       (forget-committed-lines)
       (forget-trusted-under ,directory)
       (uiop:delete-directory-tree ,directory :validate (lambda (p) (uiop:subpathp p *test-cache-directory*))))))

(defmacro with-file-in-tmp ((path ending) &body body)
  "Run BODY with PATH bound to a new name for a file lying directly in /tmp,
as a download or a copy made to look at does, ending in ENDING; whatever BODY
puts there is removed afterwards."
  `(let ((,path (format nil "/tmp/tuatara-test-~d-~36r~a" (sb-posix:getpid)
                        (random (expt 36 8) (load-time-value (make-random-state t))) ,ending)))
     (unwind-protect (progn ,@body)
       (when (probe-file ,path) (delete-file ,path)))))

(defmacro with-visited ((buffer path) &body body)
  "Run BODY with BUFFER visiting the file at PATH, killed afterwards."
  `(let ((,buffer (tuatara.io:visit-file ,path)))
     (unwind-protect (progn ,@body)
       (tuatara.buffer:kill-buffer ,buffer))))

(defun put-file (path text)
  "Make the file at PATH hold TEXT (a string), as a new file."
  (alexandria:write-byte-vector-into-file (to-octets text) path :if-exists :supersede)
  path)

(defun put-program (path text)
  "Write TEXT, a shell script, at PATH as PUT-FILE does, runnable by you alone; its native name."
  (put-file path text)
  (sb-posix:chmod (namestring path) #o700)
  (namestring path))

(defun file-text (path)
  "The text of the file at PATH."
  (sb-ext:octets-to-string (alexandria:read-file-into-byte-vector path) :external-format :utf-8))

(defun read-file-corpus (root)
  "The text files under the directory ROOT, but those its .gitignore files
leave out, read at once into one FILE-CORPUS, as the search of many files
reads them (TUATARA.DEFAULTS::GATHER-TEXT-FILE), up to *SEARCH-FILES-LIMIT*
octets; true as a second value when files were left out."
  (let* ((root (tuatara.defaults::directory-name root))
         (texts '()) (size 0) (left-out nil)
         (gatherer (tuatara.defaults::make-corpus-gatherer
                    tuatara.defaults::*search-files-limit* most-positive-fixnum
                    (lambda (piece octets) (setf texts piece size octets))
                    (lambda (relative path) (declare (ignore relative path)) (setf left-out t)))))
    (multiple-value-bind (files more) (tuatara.defaults::files-under root :gitignore t)
      (dolist (file files)
        (tuatara.defaults::gather-text-file gatherer (cdr file) (car file)))
      (tuatara.defaults::gather-piece gatherer)
      (values (tuatara.defaults::make-corpus (mapcar (lambda (text) (cons (first text) (second text))) texts) size root)
              (or left-out more)))))

(defvar *time-step* 0
  "Seconds added to the clock by each BUMP-TIME, so that no two changes share a time.")

(defun bump-time (path)
  "Give the file at PATH a modification time of its own, later than any before,
so that a change shows whatever the clock's granularity."
  (let ((time (+ (sb-posix:time) 60 (incf *time-step*))))
    (sb-posix:utimes (namestring path) time time)))

(defun rewrite-in-place (path text)
  "Overwrite the file at PATH with TEXT where it is, keeping its inode, as a
program writing in place does."
  (let ((fd (sb-posix:open (namestring path) (logior sb-posix:o-wronly sb-posix:o-trunc))))
    (unwind-protect (tuatara.io:write-octets fd (to-octets text)) (sb-posix:close fd)))
  (bump-time path))

(defun replace-by-rename (path text)
  "Replace the file at PATH with a new one holding TEXT, renamed over it, as a
program saving that way does."
  (let ((temp (concatenate 'string (namestring path) ".other")))
    (put-file temp text)
    (sb-posix:rename temp (namestring path))
    (bump-time path)))

(defun write-from-another-program (path text)
  "Overwrite the file at PATH with TEXT in place from another process, as a
program writing in place does, and wait for it. Returns the seconds it took."
  (let ((start (get-internal-real-time))
        (process (sb-ext:run-program "/bin/sh" (list "-c" "printf %s \"$2\" > \"$1\"" "sh" (namestring path) text)
                                     :wait nil)))
    (loop repeat 3000 while (sb-ext:process-alive-p process) do (sleep 0.01))
    (bump-time path)
    (/ (- (get-internal-real-time) start) internal-time-units-per-second)))

(defun directory-names (directory)
  "The names in DIRECTORY, hidden ones included, sorted."
  (let ((dir (sb-posix:opendir (namestring directory))))
    (unwind-protect
         (sort (loop for entry = (sb-posix:readdir dir)
                     until (sb-alien:null-alien entry)
                     for name = (sb-posix:dirent-name entry)
                     unless (member name '("." "..") :test #'string=) collect name)
               #'string<)
      (sb-posix:closedir dir))))

(defun wait-for-snapshots ()
  "Wait until no background snapshot is still copying, and take them in."
  (loop repeat 1000
        while (some #'tuatara.runtime:job-running-p (tuatara.io:snapshots))
        do (sleep 0.01))
  (tuatara.io:finish-snapshots))

(defun directory-magic (directory)
  "The magic number of the file system holding DIRECTORY."
  (let ((fd (sb-posix:open (namestring directory) sb-posix:o-rdonly)))
    (unwind-protect (tuatara.io:fd-file-system fd) (sb-posix:close fd))))

(defparameter *lines-text* (format nil "~{line ~d~%~}" (loop for i below 3000 collect i))
  "A text of some 30,000 octets for the tests of large files, with the size limits turned down.")

(define-test (visiting-and-saving isolation)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) *lines-text*))
          (upper (string-upcase *lines-text*))
          (tuatara.io:*memory-read-limit* 1024)
          (tuatara.io:*snapshot-free-margin* 0))
      ;; a small file is read into memory
      (let ((tuatara.io:*memory-read-limit* (* 1024 1024)))
        (with-visited (b path)
          (is eq :memory (tuatara.io:file-isolation b))
          (is string= *lines-text* (tuatara.buffer:buffer-string b))))
      ;; a large local file is read itself, under a lease: another program about to
      ;; write it waits until it is copied and the buffer reads the copy
      (with-visited (b path)
        (is eq :leased (tuatara.io:file-isolation b))
        (let ((seconds (write-from-another-program path upper)))
          (true (>= seconds tuatara.io::*lease-grace*))
          (true (< seconds 10)))
        (is string= upper (file-text path))
        (is string= *lines-text* (tuatara.buffer:buffer-string b))
        (is eq :changed (tuatara.io:check-file b))
        (tuatara.io:finish-leases)
        (true (member (tuatara.io:file-isolation b) '(:clone :snapshot))))
      ;; killing the buffer gives its lease up: nobody waits on a file no buffer reads
      (put-file path *lines-text*)
      (with-visited (b path)
        (is eq :leased (tuatara.io:file-isolation b)))
      (false (find-if (lambda (lease) (and (eq (tuatara.io::lease-status lease) :held)
                                           (equal (tuatara.io::lease-path lease) path)))
                      tuatara.io::*leases*))
      (tuatara.io:finish-leases)
      ;; killed while its file is being copied for a writer: no wait, the copy goes on,
      ;; the mapping kept until nothing reads it
      (put-file path *lines-text*)
      (let* ((b (tuatara.io:visit-file path))
             (lease (find-if (lambda (lease) (and (eq (tuatara.io::lease-status lease) :held)
                                                  (equal (tuatara.io::lease-path lease) path)))
                             tuatara.io::*leases*)))
        (true lease)
        (setf (tuatara.io::lease-status lease) :moving)
        (let ((began (get-internal-real-time)))
          (tuatara.buffer:kill-buffer b)
          (true (< (- (get-internal-real-time) began) (* 1/2 internal-time-units-per-second (machine-slowdown)))))
        (true (tuatara.buffer:buffer-lent-p b))
        (false (tuatara.io::lease-buffer lease))
        (setf (tuatara.io::lease-status lease) :released))
      (tuatara.io:finish-leases)
      ;; a file open for writing already gets no lease
      (let ((writer (sb-posix:open (namestring path) sb-posix:o-wronly)))
        (unwind-protect
             (with-visited (b path)
               (isnt eq :leased (tuatara.io:file-isolation b))
               (wait-for-snapshots))
          (sb-posix:close writer)))
      ;; without a lease, a large file is cloned where the file system shares blocks, else
      ;; copied in the background; either way a rewrite in place does not reach the buffer
      (let ((tuatara.io:*lease-large-files* nil))
        (with-visited (b path)
          (true (member (tuatara.io:file-isolation b) '(:clone :pending)))
          (wait-for-snapshots)
          (true (member (tuatara.io:file-isolation b) '(:clone :snapshot)))
          (rewrite-in-place path upper)
          (is string= *lines-text* (tuatara.buffer:buffer-string b))
          (is eq :changed (tuatara.io:check-file b))))
      ;; where nothing clones: mapped at once, copied in the background, then safe
      (put-file path *lines-text*)
      (let ((tuatara.io:*lease-large-files* nil) (tuatara.io:*clone-large-files* nil))
        (with-visited (b path)
          (is eq :pending (tuatara.io:file-isolation b))
          (wait-for-snapshots)
          (is eq :snapshot (tuatara.io:file-isolation b))
          (rewrite-in-place path upper)
          (is string= *lines-text* (tuatara.buffer:buffer-string b))
          (is eq :changed (tuatara.io:check-file b))))
      ;; and with no copy at all, a rewrite in place is caught: the buffer is stale and read-only
      ;; until reverted, which forgets the history, the text having been no longer trustworthy
      (put-file path *lines-text*)
      (let ((tuatara.io:*lease-large-files* nil) (tuatara.io:*clone-large-files* nil)
            (tuatara.io:*snapshot-large-files* nil))
        (with-visited (b path)
          (is eq :direct (tuatara.io:file-isolation b))
          (tuatara.buffer:buffer-insert b 0 "edit ")
          (rewrite-in-place path upper)
          (is eq :stale (tuatara.io:check-file b))
          (true (tuatara.buffer:buffer-local b :read-only))
          (fail (tuatara.buffer:buffer-insert b 0 "x"))
          (fail (tuatara.buffer:buffer-undo b))
          (fail (tuatara.io:save-buffer-file b))
          (fail (tuatara.io:start-save b))
          (is eq :stale (tuatara.io:check-file b))      ; stays stale until reverted
          (tuatara.io:revert-buffer-file b)
          (is string= upper (tuatara.buffer:buffer-string b))
          (false (tuatara.buffer:buffer-local b :read-only))
          (false (tuatara.buffer:buffer-modified-p b))
          (is eq nil (tuatara.io:file-disk-state b))
          (false (tuatara.buffer:buffer-undo-stack b))))
      ;; no scratch files left behind
      (is equal '("big.txt") (directory-names dir)))))

(defun kept-slice (path)
  "Visit PATH, keep a slice of its text as a large kill does, and kill the
buffer. Returns the slice."
  (with-visited (b path)
    (tuatara.buffer:buffer-slice b 5 (tuatara.buffer:buffer-size b))))

(define-test (visiting-and-saving kept-text)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) *lines-text*))
          (upper (string-upcase *lines-text*))
          (tail (subseq *lines-text* 5))
          (tuatara.io:*memory-read-limit* 1024))
      ;; text kept from a killed buffer stays good: its file stays mapped and leased,
      ;; and another program about to write the file waits until the text reads a copy
      (let ((slice (kept-slice path)))
        (is string= tail (tuatara.text:rope-string slice))
        (true (>= (write-from-another-program path upper) tuatara.io::*lease-grace*))
        (is string= upper (file-text path))
        (is string= tail (tuatara.text:rope-string slice)))
      (tuatara.io:finish-leases)
      ;; once nothing holds the text, the lease is given up
      (put-file path *lines-text*)
      (kept-slice path)
      (let ((lease (find-if (lambda (lease) (and (eq (tuatara.io::lease-status lease) :held)
                                                 (equal (tuatara.io::lease-path lease) path)))
                            tuatara.io::*leases*)))
        (true lease)
        (loop repeat 100 while (and lease (eq (tuatara.io::lease-status lease) :held))
              do (sb-ext:gc :full t) (sleep 0.02))
        (is eq :released (and lease (tuatara.io::lease-status lease))))
      (tuatara.io:finish-leases)
      ;; a buffer whose text was not kept closes its file at once
      (let ((mapping (with-visited (b path) (tuatara.buffer:buffer-mapping b))))
        (false (tuatara.text::mapping-live mapping)))
      ;; copying a large file's text, killing its buffer and pasting elsewhere
      (let ((tuatara-user:*clipboard-size-limit* 8))
        (dolist (splice (list nil 10))
          (let ((tuatara.buffer:*large-edit-octets* (or splice tuatara.buffer:*large-edit-octets*)))
            (multiple-value-bind (f editor) (run-keys "C-x h M-w C-x k C-x b k e p t RET C-y" :file (namestring path))
              (declare (ignore f))
              (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                (is string= "kept" (tuatara.buffer:buffer-name b))
                (is string= *lines-text* (tuatara.buffer:buffer-string b))
                (tuatara.buffer:kill-buffer b)))))
        (kill-buffers-visiting dir))
      (is equal '("big.txt") (directory-names dir)))))

(define-test (visiting-and-saving scratch-places)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "a.txt" dir) "x")))
      ;; beside the file where its file system shares blocks, for a save to share them
      (let ((tuatara.io::*block-sharing-file-systems* (list (directory-magic dir))))
        (is equal dir (tuatara.io:scratch-directory path)))
      ;; elsewhere (exFAT on a USB stick) in the cache directory, when it has the room
      (let ((tuatara.io::*block-sharing-file-systems* '()))
        (is equal (tuatara.io:cache-directory) (tuatara.io:scratch-directory path))
        (is equal dir (tuatara.io:scratch-directory path :room (expt 2 62)))))))

(defun unnamed-copies (buffer)
  "The directories, as native names, of the unnamed files (snapshots, copies)
that BUFFER's mapping holds."
  (loop for (nil nil fd) in (car (tuatara.text::mapping-resources (tuatara.buffer:buffer-mapping buffer)))
        for link = (ignore-errors (sb-posix:readlink (format nil "/proc/self/fd/~d" fd)))
        when (and link (search " (deleted)" link))
          collect (namestring (uiop:pathname-directory-pathname
                               (uiop:parse-native-namestring (subseq link 0 (search " (deleted)" link)))))))

(define-test (visiting-and-saving copies-places)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) *lines-text*))
          (upper (string-upcase *lines-text*))
          (cache (namestring (truename (tuatara.io:cache-directory))))
          (tuatara.io:*memory-read-limit* 1024)
          (tuatara.io:*snapshot-free-margin* 0))
      ;; a copy of the file goes beside it where blocks are shared (a clone), else to the cache
      (let ((tuatara.io::*block-sharing-file-systems* (list (directory-magic dir))))
        (is equal dir (tuatara.io::snapshot-directory path :local)))
      (let ((tuatara.io::*block-sharing-file-systems* '()))
        (is equal (tuatara.io:cache-directory) (tuatara.io::snapshot-directory path :local))
        (is equal dir (tuatara.io::snapshot-directory path :local :room (expt 2 62)))
        (is equal (tuatara.io:cache-directory) (tuatara.io::snapshot-directory path :remote))
        ;; there no clone is tried: the file is copied in the background, to the cache
        (let ((tuatara.io:*lease-large-files* nil))
          (with-visited (b path)
            (is eq :pending (tuatara.io:file-isolation b))
            (wait-for-snapshots)
            (is eq :snapshot (tuatara.io:file-isolation b))
            (is equal (list cache) (unnamed-copies b))
            (rewrite-in-place path upper)
            (is string= *lines-text* (tuatara.buffer:buffer-string b))))
        ;; and a leased file is copied there when another program is about to write it
        (put-file path *lines-text*)
        (with-visited (b path)
          (is eq :leased (tuatara.io:file-isolation b))
          (write-from-another-program path upper)
          (is string= *lines-text* (tuatara.buffer:buffer-string b))
          (tuatara.io:finish-leases)
          (is equal (list cache) (unnamed-copies b))))
      (tuatara.io:finish-leases)
      (is equal '("big.txt") (directory-names dir)))))

(define-test (visiting-and-saving remote-file-systems)
  ;; a file system counted as remote: nothing is cloned or copied beside the
  ;; file; the copy goes to the cache directory, and saving works as anywhere
  (with-test-directory (dir)
    (let* ((path (put-file (merge-pathnames "shared.txt" dir) *lines-text*))
           (tuatara.io::*remote-file-systems* (cons (directory-magic dir) tuatara.io::*remote-file-systems*))
           (tuatara.io:*memory-read-limit* 1024)
           (tuatara.io:*snapshot-free-margin* 0))
      (with-visited (b path)
        (is eq :pending (tuatara.io:file-isolation b))
        (wait-for-snapshots)
        (is eq :snapshot (tuatara.io:file-isolation b))
        (tuatara.buffer:buffer-insert b 0 "remote ")
        (tuatara.io:save-buffer-file b)
        (is string= (concatenate 'string "remote " *lines-text*) (file-text path)))
      (is equal '("shared.txt") (directory-names dir)))))

(define-test (visiting-and-saving pseudo-and-special)
  ;; the files of /proc are read to their end, whatever size they report
  (with-visited (b "/proc/self/status")
    (is eq :memory (tuatara.io:file-isolation b))
    (true (search "Name:" (tuatara.buffer:buffer-string b)))
    (is eq :same (tuatara.io:file-status b)))
  ;; but no further than what is read into memory: one that gives more, never ending, is refused
  (let ((tuatara.io:*memory-read-limit* 64))
    (fail (tuatara.io:open-file-text "/proc/self/status") tuatara.io:file-system-error))
  ;; devices, named pipes and directories are refused, without waiting on the pipe
  (fail (tuatara.io:visit-file "/dev/null") tuatara.io:file-system-error)
  (fail (tuatara.io:visit-file (tuatara.io:cache-directory)) tuatara.io:file-system-error)
  (with-test-directory (dir)
    (let ((fifo (namestring (merge-pathnames "pipe" dir))))
      (sb-posix:mkfifo fifo #o600)
      (fail (tuatara.io:visit-file fifo) tuatara.io:file-system-error)))
  ;; a pseudo file is written in place, in one write, with no guard or backup
  ;; (the process's name: the kernel keeps what is written, and adds a newline when read)
  (let ((old (string-right-trim '(#\Newline) (uiop:read-file-string "/proc/self/comm"))))
    (with-visited (b "/proc/self/comm")
      (unwind-protect
           (progn (tuatara.buffer:buffer-replace b 0 (tuatara.buffer:buffer-size b) "tuatara-test")
                  (tuatara.io:save-buffer-file b)
                  (is string= (format nil "tuatara-test~%") (uiop:read-file-string "/proc/self/comm")))
        (with-open-file (out "/proc/self/comm" :direction :output :if-exists :overwrite)
          (write-string old out))))))

(defun count-opens (path function)
  "Call FUNCTION, of no arguments, its errors taken as its value, and return
how many times the file at PATH was opened meanwhile, as the system's file
events say (opened, not only named or examined), and FUNCTION's value as a
second value."
  (let ((watch (sb-alien:alien-funcall (sb-alien:extern-alien "inotify_init1"
                                                              (function sb-alien:int sb-alien:int)) #o4000)))
    (unwind-protect
         (progn
           (sb-alien:alien-funcall (sb-alien:extern-alien "inotify_add_watch"
                                                          (function sb-alien:int sb-alien:int sb-alien:c-string
                                                                    sb-alien:unsigned-int))
                                   watch (namestring path) #x20) ; IN_OPEN
           (let* ((value (handler-case (funcall function) (error (e) e)))
                  (events (make-array 4096 :element-type '(unsigned-byte 8)))
                  (got (tuatara.io:with-octets-sap (sap events)
                         (sb-alien:alien-funcall (sb-alien:extern-alien "read" (function sb-alien:long sb-alien:int
                                                                                         sb-sys:system-area-pointer
                                                                                         sb-alien:unsigned-long))
                                                 watch sap 4096))))
             ;; an event on the watched file itself has no name: 16 octets each
             (values (if (plusp got) (floor got 16) 0) value)))
      (sb-posix:close watch))))

(define-test (visiting-and-saving devices-never-opened)
  ;; a named pipe or a device, reached by a link, is refused before it is opened: opening a serial
  ;; line or a tape does something to it, a safe visit included. A named pipe stands for a device
  ;; here, whose opens the system's file events count.
  (with-test-directory (dir)
    (let ((fifo (namestring (merge-pathnames "pipe" dir)))
          (link (namestring (merge-pathnames "evidence.txt" dir)))
          (null-link (namestring (merge-pathnames "device.txt" dir))))
      (sb-posix:mkfifo fifo #o600)
      (sb-posix:symlink fifo link)
      (sb-posix:symlink "/dev/null" null-link)
      ;; the count sees a real open
      (is = 1
          (count-opens fifo
                       (lambda ()
                         (sb-posix:close (sb-posix:open fifo (logior sb-posix:o-rdonly sb-posix:o-nonblock))))))
      (dolist (try (list (lambda () (tuatara.io:visit-file link :safe t))
                         (lambda () (tuatara.io:visit-file link))
                         (lambda () (tuatara.io:open-for-reading link t))
                         (lambda () (tuatara.io:call-with-file link (constantly t)))
                         (lambda () (tuatara.io:file-text link :limit 100))
                         (lambda () (tuatara.io:file-head link))
                         (lambda () (tuatara.io:read-entry-start dir (tuatara.text:string-octets "evidence.txt") 10))
                         (lambda () (tuatara.io:copy-regular-file link (namestring (merge-pathnames "copy" dir))))))
        (multiple-value-bind (opens value) (count-opens fifo try)
          (is = 0 opens)
          (true (or (null value) (typep value 'tuatara.io:file-system-error)))))
      (let ((refusal (handler-case (tuatara.io:visit-file null-link :safe t)
                       (tuatara.io:file-system-error (e) (tuatara.io:file-system-error-reason e)))))
        (true (search "a device" refusal)))
      ;; a file is still read, its access time kept
      (let ((file (namestring (merge-pathnames "plain.txt" dir))))
        (put-file file "plain")
        (sb-posix:utimes file 946681200 946681200)
        (is string= "plain" (tuatara.io:file-text file))
        (is = 946681200 (sb-posix:stat-atime (sb-posix:stat file)))
        ;; a file larger than a limit: refused, or what TOO-LARGE gives
        (fail (tuatara.io:file-text file :limit 3) tuatara.io:file-system-error)
        (is eq :large
            (tuatara.io:file-text file :limit 3 :too-large (lambda (name size) (declare (ignore name size)) :large)))
        (is eq :none (tuatara.io:file-text (namestring (merge-pathnames "absent" dir)) :missing :none)))))
  ;; foreign memory: zeroed when asked, copied out by length or to its zero octet
  (tuatara.io:with-foreign-memory (sap 8 :zero t)
    (is equalp #(0 0 0 0) (tuatara.io:sap-octets sap 4))
    (setf (sb-sys:sap-ref-8 sap 0) 104 (sb-sys:sap-ref-8 sap 1) 105)
    (is equalp #(104 105) (tuatara.io:sap-octets sap))
    (true (tuatara.io:null-pointer-p (tuatara.io:null-pointer)))))

(define-test (visiting-and-saving read-only-mount)
  ;; in the toolbox the host's /usr is mounted read-only: nothing can change a
  ;; file there, so a large one is mapped without a copy, and saving there fails cleanly
  (let ((file (find-if #'probe-file '("/run/host/usr/bin/bash" "/run/host/usr/bin/ls"))))
    (if (null file)
        (skip "No read-only mount to try" (true t))
        (let ((fd (sb-posix:open file sb-posix:o-rdonly)))
          (unwind-protect (true (tuatara.io:read-only-mount-p fd)) (sb-posix:close fd))
          (let ((tuatara.io:*memory-read-limit* 1024))
            (with-visited (b file)
              (is eq :direct (tuatara.io:file-isolation b))
              (false (tuatara.io:snapshots))))
          (fail (tuatara.io:write-text-file (tuatara.text:string->rope "x")
                                            (merge-pathnames "tuatara-test-file"
                                                             (uiop:pathname-directory-pathname file)))
                tuatara.io:file-system-error)))))

(define-test (visiting-and-saving arguments-handed-on-as-octets)
  ;; a program run again gets each argument as the system gets a name: a raw byte's
  ;; character as its octet, the rest as UTF-8
  (let ((argv (tuatara.io::argument-vector (list "tuatara"
                                                 (format nil "bad~cn~cme" (code-char #xDCFF) (code-char #xE9))))))
    (unwind-protect
         (progn
           (is equalp (tuatara.text:string-octets "tuatara")
               (tuatara.io:sap-octets (sb-alien:alien-sap (sb-alien:deref argv 0))))
           (is equalp #(98 97 100 255 110 195 169 109 101)
               (tuatara.io:sap-octets (sb-alien:alien-sap (sb-alien:deref argv 1))))
           (true (tuatara.io:null-pointer-p (sb-alien:alien-sap (sb-alien:deref argv 2)))))
      (tuatara.io::free-argument-vector argv))))

(define-test (visiting-and-saving snapshots-have-one-name)
  ;; the background snapshots not yet taken in are asked for by one name
  (is eq :external (nth-value 1 (find-symbol "SNAPSHOTS" :tuatara.io)))
  (false (find-symbol "SNAPSHOTS-RUNNING" :tuatara.io)))

(define-test (visiting-and-saving stamp-failure-reported)
  ;; a file in a directory that may not be looked into: an error, unless any failure is asked to
  ;; give NIL, when debug mode keeps it in its log; a file not there is NIL either way
  (with-test-directory (dir)
    (let* ((directory (ensure-directories-exist (merge-pathnames "closed/" dir)))
           (path (tuatara.io:native (put-file (merge-pathnames "inside.txt" directory) "text"))))
      (drain-debug-reports)
      (sb-posix:chmod (namestring directory) 0)
      (unwind-protect
           (progn
             (fail (tuatara.io:path-stamp path) 'tuatara.io:file-system-error)
             (false (tuatara.io:path-stamp path :failure nil))
             (false (tuatara.io:path-kind path))
             (true (sb-concurrency:queue-empty-p tuatara.runtime:*debug-reports*))
             (tuatara.runtime:set-debug t)
             (false (tuatara.io:path-stamp path :failure nil))
             (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                                while report collect (princ-to-string (fourth report)))))
               (true (find-if (lambda (text) (search (format nil "examining ~a failed" path) text)) texts)
                     "~s" texts)))
        (sb-posix:chmod (namestring directory) #o700)
        (drain-debug-reports))
      (false (tuatara.io:path-stamp (format nil "~amissing" (tuatara.io:native directory))))
      (false (tuatara.io:path-stamp (format nil "~amissing" (tuatara.io:native directory)) :failure nil))
      (true (tuatara.io:path-stamp path :failure nil)))))

(define-test (visiting-and-saving file-status-failure-reported)
  ;; a visited file whose directory may no longer be looked into: its state on disk is not known,
  ;; and debug mode keeps why in its log
  (with-test-directory (dir)
    (let* ((directory (ensure-directories-exist (merge-pathnames "shut/" dir)))
           (path (tuatara.io:native (put-file (merge-pathnames "kept.txt" directory) "text"))))
      (with-visited (b path)
        (drain-debug-reports)
        (sb-posix:chmod (namestring directory) 0)
        (unwind-protect
             (progn
               (is eq :unknown (tuatara.io:file-status b))
               (tuatara.runtime:set-debug t)
               (is eq :unknown (tuatara.io:file-status b))
               (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                                  while report collect (princ-to-string (fourth report)))))
                 (true (find-if (lambda (text) (search (format nil "examining ~a failed" path) text)) texts)
                       "~s" texts)))
          (sb-posix:chmod (namestring directory) #o700)
          (drain-debug-reports))))))

(define-test (visiting-and-saving writing-told-failure-reported)
  ;; a failing teller of files written fails nothing else, and is in the debug log; a directory
  ;; that is not there cannot be written, and that is no failure
  (drain-debug-reports)
  (unwind-protect
       (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.io:host-say-writing
                                                          (lambda (words) (error "cannot tell of ~a" words)))))
         (tuatara.runtime:set-debug t)
         (false (tuatara.io:tell-writing "a file"))
         (false (tuatara.io:writable-p "/nonexistent-tuatara-directory/file"))
         (let ((texts (loop for report = (sb-concurrency:dequeue tuatara.runtime:*debug-reports*)
                            while report collect (princ-to-string (fourth report)))))
           (true (find-if (lambda (text) (search "telling of a file written failed: cannot tell of a file" text)) texts)
                 "~s" texts)
           (false (find-if (lambda (text) (search "nonexistent-tuatara-directory" text)) texts) "~s" texts)))
    (drain-debug-reports)))

(define-test (visiting-and-saving saving-keeps-what-the-file-was)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "data.txt" dir) "one")))
      ;; a new copy is renamed into place, with the old permissions
      (sb-posix:chmod (namestring path) #o640)
      (with-visited (b path)
        (tuatara.buffer:buffer-insert b 3 " two")
        (let ((before (tuatara.io:path-stamp path)))
          (tuatara.io:save-buffer-file b)
          (let ((after (tuatara.io:path-stamp path)))
            (is = #o640 (logand (tuatara.io:stamp-mode after) #o7777))
            (isnt = (tuatara.io:stamp-ino before) (tuatara.io:stamp-ino after))
            (is string= "one two" (file-text path))
            (false (tuatara.buffer:buffer-modified-p b))
            (is eq nil (tuatara.io:check-file b)))))
      ;; an executable stays executable
      (sb-posix:chmod (namestring path) #o755)
      (with-visited (b path)
        (tuatara.buffer:buffer-insert b 0 "#")
        (tuatara.io:save-buffer-file b)
        (is = #o755 (logand (tuatara.io:stamp-mode (tuatara.io:path-stamp path)) #o7777)))
      (sb-posix:chmod (namestring path) #o644)
      ;; a symbolic link stays a link, and the file it names gets the text
      (let ((link (merge-pathnames "link.txt" dir)))
        (sb-posix:symlink "data.txt" (namestring link))
        (with-visited (b link)
          (tuatara.buffer:buffer-insert b 0 "via link: ")
          (tuatara.io:save-buffer-file b))
        (is eq :symlink (tuatara.io:stamp-kind (tuatara.io:path-stamp link :follow nil)))
        (is string= "via link: #one two" (file-text path)))
      ;; a file with two names is written in place, so both names see the new text
      (let ((other (merge-pathnames "other-name.txt" dir))
            (inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path))))
        (sb-posix:link (namestring path) (namestring other))
        (with-visited (b path)
          (tuatara.buffer:buffer-insert b 0 "both ")
          (tuatara.io:save-buffer-file b))
        (is = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
        (is string= "both via link: #one two" (file-text other)))
      ;; a new file gets the permissions the umask allows
      (let ((new (merge-pathnames "new.txt" dir))
            (umask (let ((u (sb-posix:umask 0))) (sb-posix:umask u) u)))
        (with-visited (b new)
          (tuatara.buffer:buffer-insert b 0 "fresh")
          (tuatara.io:save-buffer-file b))
        (is = (logand #o666 (lognot umask)) (logand (tuatara.io:stamp-mode (tuatara.io:path-stamp new)) #o777))
        (is string= "fresh" (file-text new)))
      ;; a save told to stop leaves the file as it was
      (is eq :stopped (tuatara.io:write-text-file (tuatara.text:string->rope "never") path :cancel (list t)))
      (is string= "both via link: #one two" (file-text path))
      ;; files this user owns can be given their owner; others' cannot, and are written in place
      (true (tuatara.io:can-give-owner-p (tuatara.io:path-stamp path)))
      (when (/= 0 (sb-posix:geteuid))
        (false (tuatara.io:can-give-owner-p (tuatara.io:path-stamp "/etc/passwd"))))
      ;; nothing left behind: no scratch files, no backups
      (is equal '("data.txt" "link.txt" "new.txt" "other-name.txt") (directory-names dir)))))

(define-test (visiting-and-saving guard-against-overwriting)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "shared.txt" dir) "original")))
      (with-visited (b path)
        (tuatara.buffer:buffer-insert b 0 "mine: ")
        ;; another program replaced it: a save must be told to overwrite
        (replace-by-rename path "theirs")
        (is eq :changed (tuatara.io:check-file b))
        (is string= "Changed on disk" (tuatara.defaults::files-mode-line nil b))
        (fail (tuatara.io:save-buffer-file b) tuatara.io:file-changed-on-disk)
        (fail (tuatara.io:start-save b) tuatara.io:file-changed-on-disk)
        (is string= "theirs" (file-text path))
        (tuatara.io:save-buffer-file b :force t)
        (is string= "mine: original" (file-text path))
        (is eq nil (tuatara.io:check-file b))
        ;; deleted on disk: saving writes it again without asking
        (delete-file path)
        (is eq :deleted (tuatara.io:check-file b))
        (tuatara.buffer:buffer-insert b 0 "again ")
        (tuatara.io:save-buffer-file b)
        (is string= "again mine: original" (file-text path))
        (is eq nil (tuatara.io:check-file b))))))

(define-test (visiting-and-saving changed-while-saving-refused)
  ;; a program changing the file while a save is under way has its change kept: the file is looked at again
  ;; just before the new text takes its place, by rename and in place alike
  (with-test-directory (dir)
    (dolist (in-place '(nil t))
      (let* ((path (put-file (merge-pathnames (if in-place "linked.txt" "renamed.txt") dir) "original"))
             (other (and in-place (namestring (merge-pathnames "other-name.txt" dir))))
             (stamp (tuatara.io:path-stamp path))
             (changed nil))
        (when other (sb-posix:link (namestring path) other))
        (flet ((change-it (&rest progress)
                 (declare (ignore progress))
                 (unless changed
                   (setf changed t)
                   (put-file path "changed meanwhile, by someone else"))))
          ;; in place: changed once the backup is made, before the file is opened to be written
          (sb-int:encapsulate 'tuatara.io::backup-copy 'changed-meanwhile
                              (lambda (function &rest arguments)
                                (multiple-value-prog1 (apply function arguments) (change-it))))
          (unwind-protect
               (fail (tuatara.io:write-text-file (tuatara.text:string->rope "mine") (namestring path)
                                                 :expected stamp :progress (and (not in-place) #'change-it))
                   tuatara.io:file-changed-on-disk "~:[by rename~;in place~]" in-place)
            (sb-int:unencapsulate 'tuatara.io::backup-copy 'changed-meanwhile)))
        (is string= "changed meanwhile, by someone else" (file-text path))))
    (is equal '("linked.txt" "other-name.txt" "renamed.txt") (directory-names dir))))

(define-test (visiting-and-saving made-names-within-255-octets)
  ;; a name numbered, or an attachment's own name, never takes more than a name may on disk: what
  ;; comes before the number, or before the ending, is shortened, never within a character
  (let* ((long (format nil "~a.pdf" (make-string 200 :initial-element (code-char #xe9))))
         (numbered (tuatara.io:numbered-name long 12))
         (attachment (tuatara.defaults::attachment-file-name long)))
    (true (<= (tuatara.text:string-octet-length numbered) 255))
    (true (uiop:string-suffix-p numbered " (12).pdf"))
    (true (<= (tuatara.text:string-octet-length (tuatara.io:numbered-name long 3 :dash)) 255))
    (true (<= (tuatara.text:string-octet-length attachment) 255))
    (true (uiop:string-suffix-p attachment ".pdf"))
    (is string= "short.pdf" (tuatara.defaults::attachment-file-name "a/b\\short.pdf"))))

(define-test (visiting-and-saving copied-to-a-free-name)
  ;; a copy never goes over a file: the name asked for, else (2) and so on before its ending
  (is string= "report.eml" (tuatara.io:numbered-name "report.eml" 1))
  (is string= "report (2).eml" (tuatara.io:numbered-name "report.eml" 2))
  (is string= "notes (3)" (tuatara.io:numbered-name "notes" 3))
  ;; or -2 and so on, for a name with no space, as a file dropped from another machine is given
  (is string= "report.eml" (tuatara.io:numbered-name "report.eml" 1 :dash))
  (is string= "report-2.eml" (tuatara.io:numbered-name "report.eml" 2 :dash))
  (is string= "notes-3" (tuatara.io:numbered-name "notes" 3 :dash))
  (is string= "kea.tar-2.gz" (tuatara.io:numbered-name "kea.tar.gz" 2 :dash))
  (with-test-directory (dir)
    (let ((directory (tuatara.io:native dir))
          (from (namestring (put-file (merge-pathnames "source.bin" dir) "contents"))))
      (put-file (merge-pathnames "report.eml" dir) "already here")
      (let ((copy (tuatara.io:copy-to-free-name from directory "report.eml")))
        (is string= (tuatara.io:path-join directory "report (2).eml") copy)
        (is string= "contents" (file-text copy))
        (is string= "already here" (file-text (merge-pathnames "report.eml" dir))))
      (is string= (tuatara.io:path-join directory "report (3).eml")
          (tuatara.io:copy-to-free-name from directory "report.eml"))
      ;; a link is a name taken too, never written through
      (sb-posix:symlink "/nonexistent/tuatara-test-target" (namestring (merge-pathnames "link.txt" dir)))
      (is string= (tuatara.io:path-join directory "link (2).txt")
          (tuatara.io:copy-to-free-name from directory "link.txt"))
      ;; a name taken only where USABLE says it may be, its number given back
      (multiple-value-bind (fd path n)
          (tuatara.io:create-free-file directory (lambda (n) (format nil "slot-~d" n)) :usable (lambda (n) (> n 2)))
        (sb-posix:close fd)
        (is = 3 n)
        (is string= (tuatara.io:path-join directory "slot-3") path))
      ;; a directory that is not there: said, not tried again and again
      (fail (tuatara.io:create-free-file (tuatara.io:path-join directory "gone/") (lambda (n) (format nil "x-~d" n)))
          tuatara.io:file-system-error))))

(define-test (visiting-and-saving revert)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "notes.txt" dir) "first")))
      (with-visited (b path)
        (tuatara.buffer:buffer-insert b 5 " edit")
        (replace-by-rename path "second version")
        (tuatara.io:revert-buffer-file b)
        (is string= "second version" (tuatara.buffer:buffer-string b))
        (false (tuatara.buffer:buffer-modified-p b))
        (is eq nil (tuatara.io:check-file b))
        ;; one undo takes the revert back
        (tuatara.buffer:buffer-undo b)
        (is string= "first edit" (tuatara.buffer:buffer-string b))))))

(define-test (visiting-and-saving background-save)
  (with-test-directory (dir)
    (let* ((text (random-text 50000))
           (path (put-file (merge-pathnames "big.txt" dir) text)))
      (with-visited (b path)
        (tuatara.buffer:buffer-insert b 0 "A")
        (let ((job (tuatara.io:start-save b)))
          ;; the buffer stays usable; an edit made meanwhile is not in this save
          (tuatara.buffer:buffer-insert b 0 "B")
          (fail (tuatara.io:start-save b))              ; one save of a buffer at a time
          (sb-thread:join-thread (tuatara.io:save-job-thread job) :default nil)
          (is equal (list job) (tuatara.io:finish-saves))
          (is eq :done (tuatara.io:save-job-status job))
          (is string= (concatenate 'string "A" text) (file-text path))
          (true (tuatara.buffer:buffer-modified-p b))
          (is string= (concatenate 'string "BA" text) (tuatara.buffer:buffer-string b))))
      (is equal '("big.txt") (directory-names dir)))))

(define-modal-test (visiting-and-saving save-answers-the-next-key-at-once)
  ;; a save that takes long is waited for only until a key comes: the key after C-x C-s is answered at once
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "slow.txt" dir) "text")))
          (started nil)
          (answered nil)
          (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8)))
      (setf (tuatara.frontend.fake:fake-pending f) (lambda (f) (tuatara.frontend.fake::fake-events f)))
      (queue-test-keys f (list "i x C-x C-s" (lambda () (setf answered (get-internal-real-time))) "y"))
      ;; the save's writing takes a second
      (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.io:host-save-transform
                                                         (lambda (buffer path)
                                                           (declare (ignore buffer path))
                                                           (lambda (rope)
                                                             (setf started (get-internal-real-time))
                                                             (sleep 1)
                                                             rope)))))
        (without-grammar-questions
          (tuatara.command:run-editor f :files (list path) :setup #'tuatara.defaults:install-defaults)))
      (true (and started answered))
      (when (and started answered)
        (let ((waited (/ (- answered started) internal-time-units-per-second)))
          (true (< waited 0.2) "the key after the save was answered ~,2f s after it began" waited)))
      (is string= "xtext" (file-text path)))))

(define-test (visiting-and-saving save-waited-for-while-keys-play)
  ;; keys played from a recording never cut the wait for a save short, however soon they come, so
  ;; that what they do is the same however fast the save; a key typed does, once quick work had its time
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "slow.txt" dir) "text")))
          (results '())
          (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8)))
      (setf (tuatara.frontend.fake:fake-pending f) t)
      (flet ((save-and-wait (playing)
               (let ((b (tuatara.command:current-buffer)))
                 (tuatara.buffer:buffer-insert b 0 "x")
                 (let ((job (tuatara.io:start-save b))
                       (began (get-internal-real-time)))
                   (setf (tuatara.command::editor-keys-playing tuatara.command:*editor*) playing)
                   (unwind-protect (tuatara.defaults::take-in-soon job 5)
                     (setf (tuatara.command::editor-keys-playing tuatara.command:*editor*) nil))
                   (push (list playing (tuatara.runtime:job-running-p job)
                               (/ (- (get-internal-real-time) began) internal-time-units-per-second))
                         results)
                   (sb-thread:join-thread (tuatara.io:save-job-thread job) :default nil)
                   (tuatara.io:finish-saves)))))
        (queue-test-keys f (list (lambda () (save-and-wait t)) (lambda () (save-and-wait nil))))
        (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.io:host-save-transform
                                                           (lambda (buffer path)
                                                             (declare (ignore buffer path))
                                                             (lambda (rope) (sleep 0.5) rope)))))
          (call-without-grammar-questions
           (lambda () (tuatara.command:run-editor f :files (list path) :setup #'tuatara.defaults:install-defaults)))))
      (destructuring-bind ((typed typed-running typed-waited) (played played-running played-waited)) results
        (false typed)
        (true typed-running "a typed key is answered before the save ends")
        (true (< typed-waited 0.3) "~,2f s" typed-waited)
        (true played)
        (false played-running "played keys wait for the save")
        (true (>= played-waited 0.4) "~,2f s" played-waited)))))

(define-test (visiting-and-saving save-buffer-says-every-save-runs-apart)
  ;; every save runs on a thread of its own, however small the buffer: its help says so
  (let ((words (documentation 'tuatara.defaults::save-buffer 'function)))
    (true (search "written on a thread of its own" words))
    (false (search "a large buffer is saved in the background" words))))

(define-modal-test (visiting-and-saving saved-files-counted-once-saved)
  ;; saving several files says how many once they are on disk, saved in the background or not
  (with-test-directory (dir)
    (with-messages-said (said)
      (let ((one (namestring (put-file (merge-pathnames "one.txt" dir) "one")))
            (two (namestring (put-file (merge-pathnames "two.txt" dir) "two")))
            (f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 8)))
        (setf (tuatara.frontend.fake:fake-pending f) t)
        (queue-test-keys f (list (format nil "C-x C-f C-a C-k ~a RET i x C-g" (rimu-typed one))
                                 (format nil "C-x C-f C-a C-k ~a RET i x C-g" (rimu-typed two))
                                 "C-x s y"))
        (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.io:host-save-transform
                                                           (lambda (buffer path)
                                                             (declare (ignore buffer path))
                                                             (lambda (rope) (sleep 0.3) rope)))))
          (call-without-grammar-questions
           (lambda () (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))))
        (is string= "xone" (file-text one))
        (is string= "xtwo" (file-text two))
        (true (search "Saved 2 files" (said)))
        (false (search "Saved 0 files" (said)))))))

(define-test (visiting-and-saving watcher)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "watched.txt" dir) "original")))
      (with-visited (b path)
        (unwind-protect
             (progn
               (tuatara.io:watch-files (list b))
               (sleep 0.1)
               (replace-by-rename path "changed")
               (let ((changed (loop repeat 500
                                    for found = (tuatara.io:take-file-looks)
                                    when found return found
                                    do (sleep 0.01))))
                 (is equal (list b) changed))
               (is eq :changed (tuatara.io:file-disk-state b))
               ;; saved over: what the watcher saw of the file before is not taken
               (tuatara.io:save-buffer-file b :force t)
               (tuatara.io:watch-files (list b))
               (loop repeat 50 do (tuatara.io:take-file-looks) (sleep 0.01))
               (is eq nil (tuatara.io:file-disk-state b)))
          (tuatara.io:watch-files '()))))))

(tuatara.command:define-command test-replace-file-on-disk ()
  "Replace the current buffer's file on disk as another program would, for the tests."
  (replace-by-rename (tuatara.buffer:buffer-file (tuatara.command:current-buffer)) "theirs"))

(define-test (visiting-and-saving written-name-as-shown)
  ;; a file whose name holds a line break is named in "Wrote" as the mode line and lists name it
  (with-test-directory (dir)
    (let ((path (format nil "~anew~cline.txt" (tuatara.io:native dir) #\Newline)))
      (tuatara.io:write-private-file path "x")
      (multiple-value-bind (f editor) (run-keys "i X C-g C-x C-s" :file path :width 120)
        (let ((wrote (loop for row below (length (tuatara.frontend.fake:fake-screen f))
                           for line = (tuatara.frontend.fake:fake-screen-line f row)
                           when (search "Wrote" line) return line)))
          (true (and wrote (search "new^Jline.txt" wrote)) "~s" wrote))
        (tuatara.buffer:kill-buffer (session-buffer editor))))))

(define-modal-test (visiting-and-saving new-file-in-a-new-directory)
  ;; a new file whose directory is not there: editable, said so, and saving makes the directory after asking
  (with-test-directory (dir)
    (let ((path (format nil "~anodir/deeper/new.txt" (tuatara.io:native dir))))
      (multiple-value-bind (f editor)
          (run-keys (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed path)) "i x C-g") :width 160)
        (true (tuatara.frontend.fake:fake-frame-showing f "saving asks to make it"))
        (false (tuatara.buffer:buffer-local (session-buffer editor) :read-only))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; no: nothing made
      (multiple-value-bind (f editor)
          (run-keys (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed path)) "i x C-g C-x C-s n RET") :width 160)
        (true (tuatara.frontend.fake:fake-frame-showing f "Not saved"))
        (false (tuatara.io:path-stamp (format nil "~anodir/" (tuatara.io:native dir))))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; yes: the directories made, the file written
      (multiple-value-bind (f editor)
          (run-keys (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed path)) "i x C-g C-x C-s y RET") :width 160)
        (declare (ignore f))
        (loop repeat 250 until (tuatara.io:path-stamp path) do (sleep 0.02))
        (is string= "x" (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor))))))

(define-modal-test (visiting-and-saving save-command)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "doc.txt" dir) "original")))
      ;; C-x C-s writes the file and says so; a second one has nothing to do
      (multiple-value-bind (f editor) (run-keys "i X C-g C-x C-s C-x C-s" :file path :width 120)
        (is string= "Xoriginal" (file-text path))
        (true (tuatara.frontend.fake:fake-frame-showing f "Wrote"))
        (true (tuatara.frontend.fake:fake-frame-showing f "(No changes need to be saved)"))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; another program replaced the file: saving asks; n keeps theirs
      (multiple-value-bind (f editor)
          (run-keys "i Y C-g M-x t e s t - r e p l a c e - f i l e - o n - d i s k RET C-x C-s n RET"
                    :file path
                    :width 120)
        (true (tuatara.frontend.fake:fake-frame-showing f "changed on disk since it was read"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Not saved"))
        (is string= "theirs" (file-text path))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; y overwrites it
      (multiple-value-bind (f editor)
          (run-keys "i Y C-g M-x t e s t - r e p l a c e - f i l e - o n - d i s k RET C-x C-s y RET"
                    :file path
                    :width 120)
        (declare (ignore f))
        (is string= "Ytheirs" (file-text path))
        (false (tuatara.buffer:buffer-modified-p (session-buffer editor)))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; revert-buffer reads it again, asking first about changes not saved
      (multiple-value-bind (f editor)
          (run-keys "i Z C-g M-x t e s t - r e p l a c e - f i l e - o n - d i s k RET M-x r e v e r t - b u f f e r RET y RET"
                    :file path :width 120)
        (true (tuatara.frontend.fake:fake-frame-showing f "Discard the changes"))
        (is string= "theirs" (session-text editor))
        (tuatara.buffer:kill-buffer (session-buffer editor))))))

(define-modal-test (visiting-and-saving write-protected)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "ro.txt" dir) "original"))
          (locked (ensure-directories-exist (merge-pathnames "locked/" dir))))
      (sb-posix:chmod (namestring path) #o444)
      (sb-posix:chmod (namestring locked) #o555)
      (unwind-protect
           (progn
             ;; a file one may not write opens read-only, and so does a new file where
             ;; none can be made, its directories included; a new one where it can does not
             (flet ((read-only-p (file)
                      (let ((b (tuatara.io:visit-file file)))
                        (prog1 (and (tuatara.buffer:buffer-local b :read-only) t)
                          (tuatara.buffer:kill-buffer b)))))
               (true (read-only-p path))
               (true (read-only-p (merge-pathnames "new.txt" locked)))
               (true (read-only-p (merge-pathnames "missing/new.txt" locked)))
               (false (read-only-p (merge-pathnames "missing/deeper/new.txt" dir)))
               (false (read-only-p (merge-pathnames "new.txt" dir))))
             ;; [%] in the mode line; C-x C-q makes it editable; saving asks, and no keeps the file
             (multiple-value-bind (f editor) (run-keys "C-x C-q i X C-g C-x C-s n RET" :file path :width 120)
               (true (tuatara.frontend.fake:fake-frame-showing f "[%]"))
               (true (tuatara.frontend.fake:fake-frame-showing f "File ro.txt is write-protected; try to save anyway?"))
               (true (tuatara.frontend.fake:fake-frame-showing f "Attempt to save to a file that you aren't allowed to write"))
               (is string= "original" (file-text path))
               (tuatara.buffer:kill-buffer (session-buffer editor)))
             ;; yes saves it, and it stays write-protected
             (multiple-value-bind (f editor) (run-keys "C-x C-q i X C-g C-x C-s y RET" :file path :width 120)
               (declare (ignore f))
               (is string= "Xoriginal" (file-text path))
               (is = #o444 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path)))))
               (tuatara.buffer:kill-buffer (session-buffer editor))))
        (sb-posix:chmod (namestring locked) #o755)
        (sb-posix:chmod (namestring path) #o644)))))

(define-modal-test (visiting-and-saving save-after-background-replacement)
  ;; the text a background replacement leaves reads its own scratch files; saving writes it all
  (with-test-directory (dir)
    (let* ((text (format nil "~{row ~d abc~%~}" (loop for i below 400 collect i)))
           (path (put-file (merge-pathnames "rows.txt" dir) text)))
      (with-background ()
        (multiple-value-bind (f editor)
            (run-keys (format nil "r r % ~a RET" (key-string "abc/xyz")) :file path :width 120)
          (declare (ignore f))
          (let ((b (session-buffer editor)))
            (is string= (cl-ppcre:regex-replace-all "abc" text "xyz") (tuatara.buffer:buffer-string b))
            (tuatara.io:save-buffer-file b)
            (is string= (cl-ppcre:regex-replace-all "abc" text "xyz") (file-text path))
            (tuatara.buffer:kill-buffer b))))
      (is equal '("rows.txt") (directory-names dir)))))

(define-test (visiting-and-saving names-taken-literally)
  ;; [, *, ?, \ and spaces are only characters in a file's name; ~ is the home directory
  (with-test-directory (dir)
    (let* ((name "report [final] *?\\.csv")
           ;; the file's path, and with ~ for the home directory when it is under it: ~ is
           ;; the home directory by the file system's own name for it (/var/home, not /home)
           (path (concatenate 'string (tuatara.io:native dir) name))
           (tilde (tuatara.defaults::abbreviated-path path)))
      (with-visited (b tilde)
        (is string= name (tuatara.buffer:buffer-name b))
        (is string= path (tuatara.io:native (tuatara.buffer:buffer-file b)))
        (tuatara.buffer:buffer-insert b 0 "a,b")
        (tuatara.io:save-buffer-file b))
      (is string= "a,b" (file-text (sb-ext:parse-native-namestring path)))
      (sb-posix:mkfifo (concatenate 'string (tuatara.io:native dir) "pipe") #o600)
      (ensure-directories-exist (merge-pathnames "sub/" dir))
      ;; listing a directory opens nothing in it: the named pipe is listed, not waited on
      (is equal (list (list "pipe" :fifo 0 :fifo) (list name :regular 3 :regular) (list "sub" :directory))
          (mapcar (lambda (entry) (if (eq (second entry) :directory) (subseq entry 0 2) entry))
                  (tuatara.io:directory-entries dir)))
      (let ((entry (find name (tuatara.io:directory-entries dir :stamps t) :key #'first :test #'string=)))
        (is = 3 (tuatara.io:stamp-size (fifth entry))))
      (is equal (list "sub/" "pipe" name)
          (map 'list #'tuatara.command::candidate-string
               (tuatara.defaults::file-candidates (concatenate 'string (tuatara.io:native dir)))))
      ;; nor in a directory's: one called [Gmail] is listed, walked, searched and completed in
      (let ((odd (concatenate 'string (tuatara.io:native dir) "sub/[Gmail] *?")))
        (sb-posix:mkdir odd #o700)
        (with-open-file (s (sb-ext:parse-native-namestring (concatenate 'string odd "/inbox [1].txt"))
                           :direction :output)
          (write-line "a needle here" s))
        (is equal '("inbox [1].txt") (mapcar #'first (tuatara.io:directory-entries odd)))
        (is equal '("inbox [1].txt") (mapcar #'first (tuatara.io:directory-entries (concatenate 'string odd "/"))))
        (is string= (concatenate 'string odd "/inbox [1].txt")
            (tuatara.io:native (tuatara.io:expand-path "inbox [1].txt" :directory odd)))
        (is equal '("sub/[Gmail] *?/inbox [1].txt")
            (remove-if-not (lambda (relative) (search "inbox" relative))
                           (mapcar #'cdr (tuatara.defaults::files-under dir))))
        (is equal '("a needle here")
            (mapcar #'fourth (tuatara.defaults::corpus-lines (read-file-corpus dir) "needle")))
        (is equal '("inbox [1].txt")
            (map 'list
                 #'tuatara.command::candidate-string
                 (tuatara.defaults::file-candidates (concatenate 'string odd "/"))))))))

(define-test (visiting-and-saving default-directory)
  ;; a buffer with no file is in *default-directory*, ~/ unless set: a file
  ;; named there is taken from it, and the mode line shows it
  (with-test-directory (dir)
    (let ((root (tuatara.io:native dir)))
      (let ((tuatara-user:*default-directory* root))
        (is string= root (tuatara.io:native (tuatara.io:default-directory)))
        (is string= (concatenate 'string root "x.txt") (tuatara.io:native (tuatara.io:expand-path "x.txt")))
        (is string= (concatenate 'string root "x.txt")
            (tuatara.io:native (tuatara.defaults::save-target (tuatara.buffer:make-buffer "*no file*") "x.txt"))))
      ;; NIL, or a directory that is not there: where Tuatara was started
      (dolist (setting (list nil (concatenate 'string root "no-such-directory/")))
        (let ((tuatara-user:*default-directory* setting))
          (is string= (tuatara.io:native (uiop:getcwd)) (tuatara.io:native (tuatara.io:default-directory)))))
      (let ((tuatara-user:*default-directory* "~/"))
        (is string= (tuatara.io:home-directory) (tuatara.io:native (tuatara.io:default-directory)))
        (is string= "~/" (tuatara.command::buffer-directory-name (tuatara.buffer:make-buffer "*no file either*")))))))

(define-test (visiting-and-saving saving-a-buffer-with-no-file)
  ;; C-x C-s in a buffer with no file (the scratch buffers) asks as C-x C-f asks:
  ;; from its directory, the names there offered, a directory gone into, a new name taken
  (with-test-directory (dir)
    (let* ((root (tuatara.io:native dir))
           (shown (tuatara.defaults::abbreviated-path dir))
           (tuatara-user:*default-directory* root))
      (ensure-directories-exist (merge-pathnames "notes/" dir))
      (put-file (merge-pathnames "notes/old.org" dir) (format nil "old~%"))
      (flet ((saved (keys)
               (multiple-value-bind (f editor) (run-keys keys :text "kept text" :width (width-for dir) :height 20)
                 (declare (ignore editor))
                 f)))
        (let ((f (saved "C-x C-s n o t e s / n e w . o r g RET")))
          (true (tuatara.frontend.fake:fake-frame-showing f (concatenate 'string "Save as: " shown)))
          ;; the directory's entries offered as candidates, as finding a file offers them
          (true (tuatara.frontend.fake:fake-frame-showing f "old.org"))
          (is string= "kept text" (alexandria:read-file-into-string (merge-pathnames "notes/new.org" dir))))
        ;; a directory alone is no file to save in; C-g saves nothing
        (true (tuatara.frontend.fake:fake-frame-showing (saved "C-x C-s n o t e s / C-j") "is a directory: not saved"))
        (saved "C-x C-s n o t e s / g o n e . o r g C-g")
        (is equal '("new.org" "old.org")
            (sort (mapcar (lambda (p) (file-namestring p))
                          (uiop:directory-files (merge-pathnames "notes/" dir))) #'string<))
        ;; the scratch buffer saved is the file's buffer now: not left for the tests after
        (kill-buffers-visiting dir)))))

(define-test (visiting-and-saving renaming-the-visited-file)
  ;; M-x rename-visited-file renames the file itself, the buffer following it,
  ;; changes not saved kept; into another directory by a path; refused onto a name
  ;; there, for a file visited safely; a buffer with no file saved as asked
  (with-test-directory (dir)
    (let* ((root (tuatara.io:native dir))
           (tuatara-user:*default-directory* root)
           (old (merge-pathnames "old.txt" dir)))
      (ensure-directories-exist (merge-pathnames "sub/" dir))
      (put-file old (format nil "* first~%"))
      (put-file (merge-pathnames "taken.txt" dir) "taken")
      (unwind-protect
           (progn
             (let* ((inode (tuatara.io:stamp-ino (tuatara.io:path-stamp (namestring old))))
                    (editor (nth-value 1 (run-keys "i X C-g M-x r e n a m e - v i s i t e d - f i l e RET C-a C-k n e w . o r g RET"
                                                   :file (namestring old) :width 200 :height 20)))
                    (b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
               (false (probe-file old))
               ;; the same file, not written again: its text as it was, the change still to save
               (is = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp (concatenate 'string root "new.org"))))
               (is string= (format nil "* first~%") (alexandria:read-file-into-string (merge-pathnames "new.org" dir)))
               (is string= "new.org" (tuatara.buffer:buffer-name b))
               (is string= (concatenate 'string root "new.org") (tuatara.io:native (tuatara.buffer:buffer-file b)))
               (true (tuatara.buffer:buffer-modified-p b))
               (is eq 'tuatara.defaults::org-mode (type-of (tuatara.modes:buffer-major-mode b)))
               ;; the list of recent files follows it
               (let ((recent (mapcar #'car (tuatara.defaults::read-recent-files))))
                 (true (member (concatenate 'string root "new.org") recent :test #'equal))
                 (false (member (concatenate 'string root "old.txt") recent :test #'equal))))
             (kill-buffers-visiting dir)
             ;; a file not written yet: only its buffer renamed, and said so
             (let ((f (run-keys "M-x r e n a m e - v i s i t e d - f i l e RET C-a C-k l a t e r . t x t RET"
                                :file (namestring (merge-pathnames "not-yet.txt" dir)) :width 200 :height 20)))
               (true (tuatara.frontend.fake:fake-frame-showing f "not written yet: C-x C-s writes it"))
               (false (probe-file (merge-pathnames "later.txt" dir))))
             (kill-buffers-visiting dir)
             ;; a path: moved into that directory
             (run-keys "M-x r e n a m e - v i s i t e d - f i l e RET C-a C-k s u b / m o v e d . o r g RET"
                       :file (namestring (merge-pathnames "new.org" dir)) :width 200 :height 20)
             (true (probe-file (merge-pathnames "sub/moved.org" dir)))
             (kill-buffers-visiting dir)
             ;; a name there already: refused, both left as they were
             (let ((f (run-keys "M-x r e n a m e - v i s i t e d - f i l e RET C-a C-k . . / t a k e n . t x t RET"
                                :file (namestring (merge-pathnames "sub/moved.org" dir)) :width 200 :height 20)))
               (true (tuatara.frontend.fake:fake-frame-showing f "taken.txt is there already: not renamed"))
               (true (probe-file (merge-pathnames "sub/moved.org" dir))))
             (kill-buffers-visiting dir)
             ;; a file visited safely is left as it is
             (let ((f (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET M-x r e n a m e - v i s i t e d - f i l e RET"
                                        (key-string (namestring (merge-pathnames "taken.txt" dir))))
                                :width 200 :height 20)))
               (true (tuatara.frontend.fake:fake-frame-showing f "taken.txt is visited safely: its file is left as it is")))
             (kill-buffers-visiting dir)
             ;; a buffer with no file: saved under the name asked for
             (run-keys "M-x r e n a m e - v i s i t e d - f i l e RET f r e s h . t x t RET"
                       :text "words"
                       :width 200
                       :height 20)
             (is string= "words" (alexandria:read-file-into-string (merge-pathnames "fresh.txt" dir))))
        (kill-buffers-visiting dir)))))

(define-test (visiting-and-saving one-buffer-per-file)
  ;; written as a file another buffer visits: that buffer killed after asking, or refused while it has changes;
  ;; renamed to a name another buffer visits: refused
  (with-test-directory (dir)
    (let* ((tuatara-user:*default-directory* (tuatara.io:native dir))
           (mine (merge-pathnames "mine.txt" dir))
           (taken (merge-pathnames "taken.txt" dir))
           (visit-taken (format nil "C-x C-f C-a C-k ~a RET" (key-string (namestring taken))))
           (write-taken (format nil "C-x C-w C-a C-k ~a RET" (key-string (namestring taken)))))
      (put-file mine "mine")
      (put-file taken "other")
      (unwind-protect
           (progn
             (let ((f (run-keys (format nil "~a C-x b m i n e . t x t RET ~a y RET" visit-taken write-taken)
                                :file (namestring mine) :width (width-for dir) :height 20)))
               (true (tuatara.frontend.fake:fake-frame-showing f "is open in taken.txt; write over it and kill that buffer?"))
               (is string= "mine" (alexandria:read-file-into-string taken))
               (is = 1 (count (tuatara.io:expand-path (namestring taken)) tuatara.buffer:*buffers*
                              :key #'tuatara.buffer:buffer-file :test #'equal)))
             (kill-buffers-visiting dir)
             (put-file taken "other")
             (let ((f (run-keys (format nil "~a i X C-g C-x b m i n e . t x t RET ~a" visit-taken write-taken)
                                :file (namestring mine) :width (width-for dir) :height 20)))
               (true (tuatara.frontend.fake:fake-frame-showing f "is open in taken.txt with changes not saved: not saved"))
               (is string= "other" (alexandria:read-file-into-string taken)))
             (kill-buffers-visiting dir)
             (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET C-x b m i n e . t x t RET M-x r e n a m e - v i s i t e d - f i l e RET C-a C-k g h o s t . t x t RET"
                                        (key-string (namestring (merge-pathnames "ghost.txt" dir))))
                                :file (namestring mine) :width (width-for dir) :height 20)))
               (true (tuatara.frontend.fake:fake-frame-showing f "ghost.txt is open in ghost.txt: not renamed"))
               (true (probe-file mine))))
        (kill-buffers-visiting dir)))))

(define-modal-test (visiting-and-saving writing-to-another-file)
  ;; C-x C-w writes the buffer to another file, asked for as C-x C-f asks: the
  ;; buffer visits it after, named after it; the file it visited is left as it was
  (with-test-directory (dir)
    (let* ((root (tuatara.io:native dir))
           (tuatara-user:*default-directory* root)
           (old (merge-pathnames "old.txt" dir)))
      (ensure-directories-exist (merge-pathnames "copies/" dir))
      (put-file old (format nil "first~%"))
      (unwind-protect
           (progn
             (multiple-value-bind (f editor)
                 (run-keys "i X C-g C-x C-w n e w . t x t RET" :file (namestring old) :width 400 :height 20)
               (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (true (tuatara.frontend.fake:fake-frame-showing f (concatenate 'string "Write file: " (tuatara.defaults::abbreviated-path dir))))
                 (is string= "new.txt" (tuatara.buffer:buffer-name b))
                 (is string= (concatenate 'string root "new.txt") (tuatara.io:native (tuatara.buffer:buffer-file b)))
                 (false (tuatara.buffer:buffer-modified-p b))))
             (is string= (format nil "Xfirst~%") (alexandria:read-file-into-string (merge-pathnames "new.txt" dir)))
             (is string= (format nil "first~%") (alexandria:read-file-into-string old))
             (kill-buffers-visiting dir)
             ;; a directory alone: the file's own name in it
             (run-keys "C-x C-w c o p i e s / C-j" :file (namestring old) :width 160 :height 20)
             (is string= (format nil "first~%")
                 (alexandria:read-file-into-string (merge-pathnames "copies/old.txt" dir)))
             (kill-buffers-visiting dir)
             ;; a buffer with no file written to x.org (or saved so): in Org mode after
             (dolist (keys '("C-x C-w n o t e s . o r g RET" "C-x C-s m o r e . o r g RET"))
               (let ((editor (nth-value 1 (run-keys keys :text "* Heading" :width 160 :height 20))))
                 (is eq 'tuatara.defaults::org-mode
                     (type-of (tuatara.modes:buffer-major-mode (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))))
             ;; a name calling for no mode keeps the buffer's
             (let ((editor (nth-value 1
                                      (run-keys "M-x o r g - m o d e RET C-x C-w p l a i n RET"
                                                :text "* Heading"
                                                :width 160
                                                :height 20))))
               (is eq 'tuatara.defaults::org-mode
                   (type-of (tuatara.modes:buffer-major-mode (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
             ;; a file opened safely, written as x.org: still shown as it is, in no mode, still safe
             (kill-buffers-visiting dir)
             (put-file (merge-pathnames "evidence.txt" dir) (format nil "* Heading~%#+STARTUP: overview~%"))
             (let* ((editor (nth-value 1 (run-keys (format nil "C-u C-x C-f C-a C-k ~a RET C-x C-w e v i d e n c e . o r g RET"
                                                           (key-string (namestring (merge-pathnames "evidence.txt" dir))))
                                                   :width 160 :height 20)))
                    (b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
               (true (tuatara.io:safe-visit-p b))
               (is eq 'tuatara.modes:fundamental-mode (type-of (tuatara.modes:buffer-major-mode b)))
               (is string= "evidence.org" (tuatara.buffer:buffer-name b))))
        (kill-buffers-visiting dir)))))

(define-test (visiting-and-saving finding-from-the-default-directory)
  ;; C-x C-f offers the directory to start from, *default-directory* in a
  ;; buffer with no file, and a / or ~/ typed after it starts again
  (is string= "/etc/hosts" (tuatara.defaults::substitute-file-input "~/notes//etc/hosts"))
  (is string= "~/notes" (tuatara.defaults::substitute-file-input "/etc/~/notes"))
  (is string= "~/a/b" (tuatara.defaults::substitute-file-input "~/a/b"))
  (is string= "/c" (tuatara.defaults::substitute-file-input "/a//b//c"))
  (is string= "/etc" (tuatara.defaults::substitute-file-input "//etc"))
  (is string= "~/c" (tuatara.defaults::substitute-file-input "/a//b/~/c"))
  (is string= "/c" (tuatara.defaults::substitute-file-input "/a/~/b//c"))
  (with-test-directory (dir)
    (let* ((root (tuatara.io:native dir))
           (shown (tuatara.defaults::abbreviated-path dir))
           (tuatara-user:*default-directory* root))
      (put-file (merge-pathnames "here.txt" dir) (format nil "here~%"))
      (ensure-directories-exist (merge-pathnames "sub/" dir))
      (put-file (merge-pathnames "sub/there.txt" dir) (format nil "there~%"))
      (flet ((visited (keys)
               (multiple-value-bind (f editor) (run-keys keys :text "abc" :width (width-for dir) :height 20)
                 (values (let ((file (tuatara.buffer:buffer-file (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
                           (and file (tuatara.io:native file)))
                         f))))
        (multiple-value-bind (file f) (visited "C-x C-f h e r e . t x t RET")
          (is string= (concatenate 'string root "here.txt") file)
          (true (tuatara.frontend.fake:fake-frame-showing f (concatenate 'string "Find file: " shown))))
        ;; a whole path typed after the directory offered
        (is string= (concatenate 'string root "sub/there.txt")
            (visited (format nil "C-x C-f ~a RET" (key-string (concatenate 'string root "sub/there.txt")))))
        (is string= (concatenate 'string root "sub/there.txt")
            (visited (format nil "C-x C-f ~a RET" (key-string (concatenate 'string shown "sub/there.txt")))))))))

(define-test (visiting-and-saving private-files)
  ;; written whole, readable by its owner alone, and nothing left beside it
  (with-test-directory (dir)
    (let ((path (merge-pathnames "state" dir)))
      (tuatara.io:write-private-file path "first")
      (tuatara.io:write-private-file path (format nil "second ~a~%" (code-char #x2500)))
      (is string= (format nil "second ~a~%" (code-char #x2500)) (file-text path))
      (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path)))))
      (is equal '("state") (mapcar #'first (tuatara.io:directory-entries dir))))))

(define-test (visiting-and-saving show-file-name)
  (with-test-directory (dir)
    (let ((path (namestring (put-file (merge-pathnames "shown.txt" dir) (format nil "one~%")))))
      ;; the full name of the buffer's file in the echo area, kept in *Messages*
      (multiple-value-bind (f editor) (run-keys "M-x s h o w - f i l e - n a m e RET" :file path :width 150)
        (declare (ignore f))
        (is string= path (tuatara.command:editor-message editor))
        (true (search path (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))))
      ;; with a prefix argument, at point too
      (multiple-value-bind (f editor) (run-keys "C-u M-x s h o w - f i l e - n a m e RET" :file path :width 150)
        (declare (ignore f))
        (true (uiop:string-prefix-p path (session-text editor))))
      (let ((b (find (tuatara.io:expand-path path)
                     tuatara.buffer:*buffers*
                     :key #'tuatara.buffer:buffer-file
                     :test #'equal)))
        (when b (tuatara.buffer:kill-buffer b)))))
  ;; a buffer with no file: the directory it stands in
  (multiple-value-bind (f editor) (run-keys "M-x s h o w - f i l e - n a m e RET" :width 150)
    (declare (ignore f))
    (true (search "*scratch* has no file; it stands in " (tuatara.command:editor-message editor)))))

(define-modal-test (visiting-and-saving new-empty-buffer)
  ;; SPC N: an empty buffer, untitled, then untitled<2>; saved where asked
  (with-test-directory (dir)
    (let ((tuatara-user:*default-directory* (tuatara.io:native dir)))
      (dolist (b (copy-list tuatara.buffer:*buffers*))
        (when (uiop:string-prefix-p "untitled" (tuatara.buffer:buffer-name b)) (tuatara.buffer:kill-buffer b)))
      (multiple-value-bind (f editor) (run-keys "SPC N i h i C-g" :width 120 :height 10)
        (declare (ignore f))
        (is string= "untitled" (current-name editor))
        (is string= "hi" (session-text editor))
        ;; leaving would lose it: asked about, as a file's buffer is
        (true (tuatara.defaults::unsaved-p (tuatara.buffer:find-buffer "untitled")))
        (false (tuatara.defaults::unsaved-p (tuatara.buffer:find-buffer "*scratch*"))))
      (multiple-value-bind (f editor) (run-keys "SPC N" :width 120 :height 10)
        (declare (ignore f))
        (is string= "untitled<2>" (current-name editor)))
      (multiple-value-bind (f editor) (run-keys "SPC N i n o t e C-g C-x C-s n o t e . o r g RET" :width 120 :height 10)
        (declare (ignore f))
        (is string= "note" (alexandria:read-file-into-string (merge-pathnames "note.org" dir)))
        (is string= "note.org" (current-name editor)))
      (kill-buffers-visiting dir)
      (dolist (b (copy-list tuatara.buffer:*buffers*))
        (when (uiop:string-prefix-p "untitled" (tuatara.buffer:buffer-name b)) (tuatara.buffer:kill-buffer b))))))

(define-test (visiting-and-saving unreadable-empty-directory-deleted)
  ;; a directory left with no permissions at all, empty (as the kernel leaves an overlay's work
  ;; directory), goes with the tree it is in
  (with-test-directory (dir)
    (let ((top (namestring (merge-pathnames "tree/" dir)))
          (locked (namestring (merge-pathnames "tree/work/work/" dir))))
      (ensure-directories-exist locked)
      (sb-posix:chmod locked 0)
      (true (tuatara.io:delete-tree top))
      (false (probe-file top)))))

(define-test (visiting-and-saving read-whole-within-limit)
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "ten" dir))))
      (put-file path "0123456789")
      (is string= "0123456789" (tuatara.io:file-text path :limit 10))
      (is equalp (tuatara.text:string-octets "0123456789") (tuatara.io:file-octets path :limit 10))
      ;; one octet over: refused, or what TOO-LARGE says of the file's name and size
      (fail (tuatara.io:file-text path :limit 9) 'tuatara.io:file-system-error)
      (fail (tuatara.io:file-octets path :limit 9) 'tuatara.io:file-system-error)
      (is = 10 (second (tuatara.io:file-text path :limit 9 :too-large #'list)))
      (is equal '(:big 10)
          (tuatara.io:call-with-file path (lambda (fd name size)
                                            (tuatara.io:fd-octets fd name size 9 (lambda (size) (list :big size)))))))))

(defun revert-leased (buffer)
  "Revert BUFFER, visiting a leased file, change it, and forget its history,
so that nothing holds its text before: the lease of that text, which then
serves it alone."
  (let ((old (tuatara.io::buffer-lease buffer)))
    (tuatara.io:revert-buffer-file buffer)
    (tuatara.buffer:buffer-insert buffer 0 "x")
    (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '())
    old))

(define-test (visiting-and-saving revert-gives-up-old-lease)
  ;; reverting a leased file: the old text's lease serves that text alone,
  ;; and is given up once nothing reads it, so that a writer waits for one copy
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) *lines-text*))
          (tuatara.io:*memory-read-limit* 1024))
      (flet ((held ()
               (count-if (lambda (lease) (and (eq (tuatara.io::lease-status lease) :held)
                                              (equal (tuatara.io::lease-path lease) path)))
                         tuatara.io::*leases*)))
        (with-visited (b path)
          (is eq :leased (tuatara.io:file-isolation b))
          (let ((old (revert-leased b)))
            (true old)
            (is eq :leased (tuatara.io:file-isolation b))
            (isnt eq old (tuatara.io::buffer-lease b))
            (false (tuatara.io::lease-buffer old))
            (loop repeat 100 while (eq (tuatara.io::lease-status old) :held)
                  do (sb-ext:gc :full t) (sleep 0.02))
            (is eq :released (tuatara.io::lease-status old))
            (is = 1 (held)))))
      (tuatara.io:finish-leases))))

(define-test (visiting-and-saving lease-given-up-is-not-copied)
  ;; a lease given up (its buffer killed) just as a writer comes is let go, not
  ;; copied for a buffer that is gone
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) *lines-text*))
          (tuatara.io:*memory-read-limit* 1024)
          (lease nil))
      (with-visited (b path)
        (setf lease (tuatara.io::buffer-lease b))
        (true lease)
        (sb-int:encapsulate 'tuatara.io:lease-breaking-p 'given-up-meanwhile
                            (lambda (original fd)
                              (if (eql fd (tuatara.io::lease-fd lease))
                                  (progn (tuatara.io::give-up-leases (lambda (other) (eq other lease))) t)
                                  (funcall original fd))))
        (unwind-protect
             (progn (tuatara.io:wake tuatara.io::*lease-break-pipe*)
                    (loop repeat 100 while (eq (tuatara.io::lease-status lease) :held) do (sleep 0.02)))
          (sb-int:unencapsulate 'tuatara.io:lease-breaking-p 'given-up-meanwhile))
        (sleep 0.2)
        (is eq :released (tuatara.io::lease-status lease)))
      (tuatara.io:finish-leases))))

(define-test (visiting-and-saving lease-keeper-sleeps-until-a-writer-comes)
  ;; with a large file open and nothing writing it, the lease keeper does not
  ;; look at the lease again and again; a writer is still noticed at once
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.txt" dir) *lines-text*))
          (tuatara.io:*memory-read-limit* 1024)
          (looks 0))
      (with-visited (b path)
        (is eq :leased (tuatara.io:file-isolation b))
        (sleep 0.2)
        (sb-int:encapsulate 'tuatara.io:lease-breaking-p 'counted
                            (lambda (original fd) (incf looks) (funcall original fd)))
        (unwind-protect (sleep 1)
          (sb-int:unencapsulate 'tuatara.io:lease-breaking-p 'counted))
        (is = 0 looks)
        (let ((seconds (write-from-another-program path (string-upcase *lines-text*))))
          (true (< seconds (/ tuatara.io::*lease-check-interval* 2)) "~,2f s" seconds))
        (tuatara.io:finish-leases)
        (true (member (tuatara.io:file-isolation b) '(:clone :snapshot)))))))

(define-test (visiting-and-saving backups-of-one-name-apart)
  ;; two files of one name saved at once from directories that cannot hold a backup:
  ;; each has its own in the cache, neither taken away by the other
  (with-test-directory (dir)
    (let* ((a (put-file (merge-pathnames "notes.org" (ensure-directories-exist (merge-pathnames "a/" dir))) "first"))
           (b (put-file (merge-pathnames "notes.org" (ensure-directories-exist (merge-pathnames "b/" dir))) "second"))
           (kept '()))
      (dolist (directory '("a/" "b/")) (sb-posix:chmod (namestring (merge-pathnames directory dir)) #o500))
      (unwind-protect
           (dolist (target (list a b))
             (multiple-value-bind (fd path)
                 (tuatara.io::backup-copy (namestring target) (tuatara.io:path-stamp (namestring target)))
               (push (cons fd path) kept)))
        (dolist (directory '("a/" "b/")) (sb-posix:chmod (namestring (merge-pathnames directory dir)) #o700)))
      (unwind-protect
           (destructuring-bind ((fd-b . path-b) (fd-a . path-a)) kept
             (declare (ignore fd-a fd-b))
             (isnt string= path-a path-b)
             (is string= "first" (file-text path-a))
             (is string= "second" (file-text path-b)))
        (loop for (fd . path) in kept do (ignore-errors (sb-posix:close fd)) (ignore-errors (sb-posix:unlink path)))))))

(define-test (visiting-and-saving private-files-put-on-disk)
  ;; a file written whole is put on disk before it is renamed over the old one,
  ;; and the rename after it, so that a power cut leaves the old or the new
  (with-test-directory (dir)
    (let ((path (merge-pathnames "state" dir)) (calls '()))
      (sb-int:encapsulate 'tuatara.io::flush-fd 'counted (lambda (f &rest args) (push :file calls) (apply f args)))
      (sb-int:encapsulate 'tuatara.io::sync-directory 'counted (lambda (f &rest args)
                                                                 (push :directory calls)
                                                                 (apply f args)))
      (unwind-protect (tuatara.io:write-private-file path "first")
        (sb-int:unencapsulate 'tuatara.io::flush-fd 'counted)
        (sb-int:unencapsulate 'tuatara.io::sync-directory 'counted))
      (is equal '(:file :directory) (reverse calls))
      (is string= "first" (file-text path)))))

(define-test (visiting-and-saving copy-taken-on-where-the-kernel-stopped)
  ;; where the kernel copies part of a file and then cannot go on, the rest is
  ;; copied here from where it stopped: the copy is the file, nothing twice
  (with-test-directory (dir)
    (let* ((text (format nil "~{~d ~}" (loop for i below 20000 collect i)))
           (from (put-file (merge-pathnames "from.txt" dir) text))
           (to (merge-pathnames "to.txt" dir)))
      (sb-int:encapsulate 'tuatara.io:copy-fd-range 'half
                          (lambda (original source offset dest length)
                            (let ((copied (funcall original source offset dest (floor length 2))))
                              (and (integerp copied) copied))))
      (unwind-protect
           (let ((in (sb-posix:open (namestring from) sb-posix:o-rdonly))
                 (out (sb-posix:open (namestring to) (logior sb-posix:o-wronly sb-posix:o-creat) #o600)))
             (unwind-protect (true (tuatara.io:copy-fd-contents in out (length text)))
               (sb-posix:close in)
               (sb-posix:close out)))
        (sb-int:unencapsulate 'tuatara.io:copy-fd-range 'half))
      (is string= text (file-text to)))))

(define-test (visiting-and-saving renamed-where-names-cannot-be-kept-whole)
  ;; on a file system that cannot rename "never over another" in one step (NFS,
  ;; SMB), files and directories are renamed all the same, and never over another
  (with-test-directory (dir)
    (let ((refused 0))
     (flet ((at (name) (namestring (merge-pathnames name dir))))
      (put-file (at "a.txt") "a")
      (put-file (at "taken.txt") "taken")
      (put-file (at "other.txt") "other")
      (ensure-directories-exist (at "directory/"))
      (ensure-directories-exist (at "full/"))
      (sb-int:encapsulate 'tuatara.io::rename-entry 'no-noreplace
                          (lambda (original from-dirfd old to-dirfd new flags)
                            (if (logtest flags 1)
                                (progn (incf refused) (values -1 sb-posix:einval))
                                (funcall original from-dirfd old to-dirfd new flags))))
      (unwind-protect
           (progn
             (tuatara.io:rename-path (at "a.txt") (at "b.txt"))
             (is string= "a" (file-text (at "b.txt")))
             (false (probe-file (at "a.txt")))
             (fail (tuatara.io:rename-path (at "other.txt") (at "taken.txt")) 'tuatara.io:file-system-error)
             (is string= "other" (file-text (at "other.txt")))
             (is string= "taken" (file-text (at "taken.txt")))
             (tuatara.io:rename-path (at "directory") (at "moved"))
             (true (uiop:directory-exists-p (at "moved/")))
             (fail (tuatara.io:rename-path (at "moved") (at "full")) 'tuatara.io:file-system-error)
             (true (uiop:directory-exists-p (at "moved/")))
             ;; each rename was first refused, as such a file system refuses it
             (is = 4 refused))
        (sb-int:unencapsulate 'tuatara.io::rename-entry 'no-noreplace))))))

(define-test (visiting-and-saving files-read-into-memory-once)
  ;; a file read into memory is read into one vector of its size: what one more
  ;; octet of it makes is about one octet (four before: grown, then copied)
  (with-test-directory (dir)
    (flet ((made (octets)
             (let ((path (namestring (merge-pathnames (format nil "f~d" octets) dir))))
               (alexandria:write-byte-vector-into-file (make-array octets
                                                                   :element-type '(unsigned-byte 8)
                                                                   :initial-element 65) path)
               (let ((fd (sb-posix:open path sb-posix:o-rdonly)))
                 (unwind-protect
                      (multiple-value-bind (read seconds consed)
                          (measuring (tuatara.io:read-fd-octets fd :size-hint octets))
                        (declare (ignore seconds))
                        (is = octets (length read))
                        consed)
                   (sb-posix:close fd))))))
      (let ((each (/ (- (made (* 4 1024 1024)) (made (* 1024 1024))) (* 3 1024 1024))))
        (true (< each 1.5) "~,2f octets made for each octet read" each)))))

(define-test (visiting-and-saving default-directory-found-once)
  ;; the directory of buffers with no file is found once for a while, not at
  ;; every frame; a new setting is taken at once
  (with-test-directory (dir)
    (let ((tuatara-user:*default-directory* (namestring dir)) (looks 0))
      (setf tuatara.io::*default-directory-found* nil)
      (sb-int:encapsulate 'tuatara.io:path-kind 'counted (lambda (f &rest args) (incf looks) (apply f args)))
      (unwind-protect
           (progn
             (dotimes (i 200) (tuatara.io:default-directory))
             (true (<= looks 2) "~d looks" looks)
             (is equal (namestring dir) (namestring (tuatara.io:default-directory)))
             (let ((tuatara-user:*default-directory* (namestring (ensure-directories-exist (merge-pathnames "other/" dir)))))
               (is equal (namestring (merge-pathnames "other/" dir)) (namestring (tuatara.io:default-directory)))))
        (sb-int:unencapsulate 'tuatara.io:path-kind 'counted)))))

(define-test (visiting-and-saving parent-and-name-parted-alike)
  ;; a path's directory and its own name are parted one way, whether a change is
  ;; made by its directory's descriptor or the two are only named
  (with-test-directory (dir)
    (let ((top (string-right-trim "/" (namestring dir))))
      (dolist (path (list (concatenate 'string top "/x.txt") (concatenate 'string top "/sub/")))
        (let ((given nil))
          (ensure-directories-exist (concatenate 'string top "/sub/"))
          (tuatara.io::call-with-parent path
                                        (lambda (fd name)
                                          (declare (ignore fd))
                                          (setf given (tuatara.text:octets-string name))))
          (is string= (nth-value 1 (tuatara.io:path-directory-and-name (string-right-trim "/" path))) given)))
      (fail (tuatara.io::call-with-parent (concatenate 'string top "/..") (lambda (fd name) (list fd name)))))))

;;; Saving only what changed, in place, where blocks are not shared

(defparameter *long-lines-text* (format nil "~{line ~6d~%~}" (loop for i below 250000 collect i))
  "A text of 3,000,000 octets for the tests of saving in place.")

(defmacro without-shared-blocks (&body body)
  "Run BODY as on a file system that cannot share blocks between files (ext4),
large files mapped from 1,024 octets on, with saving in place turned on."
  `(let ((tuatara.io::*block-sharing-file-systems* '())
         (tuatara.io:*memory-read-limit* 1024)
         (tuatara-user:*save-in-place* t))
     ,@body))

(defun journal-there-p (path)
  "True when the journal of a save in place of PATH is beside it."
  (and (tuatara.io:path-stamp (tuatara.io::journal-path path) :follow nil) t))

(defun edit-far-apart (buffer)
  "Change BUFFER's text in two places far apart: a line replaced, the same
length, and a line inserted near the end. Returns the text it has then."
  (let ((size (tuatara.buffer:buffer-size buffer)))
    (tuatara.buffer:buffer-replace buffer 120000 120012 "CHANGED LINE")
    (tuatara.buffer:buffer-insert buffer (- size 2000) (format nil "inserted line~%"))
    (tuatara.buffer:buffer-string buffer)))

(define-test (visiting-and-saving saved-in-place-where-blocks-are-not-shared)
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
             (inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path))))
        (with-visited (b path)
          (is eq :leased (tuatara.io:file-isolation b))
          (let ((new (edit-far-apart b)))
            (tuatara.io:save-buffer-file b)
            ;; the same file, only the changes written; the buffer still reads it, leased again
            (is = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
            (is string= new (file-text path))
            (is string= new (tuatara.buffer:buffer-string b))
            (false (tuatara.buffer:buffer-modified-p b))
            (is eq nil (tuatara.io:check-file b))
            (tuatara.io:finish-leases)
            (is eq :leased (tuatara.io:file-isolation b))
            (is eq :held (tuatara.io::lease-status (tuatara.io::buffer-lease b)))
            ;; the history reads what the file held before, kept in the journal's octets
            (tuatara.buffer:buffer-undo b)
            (tuatara.buffer:buffer-undo b)
            (is string= *long-lines-text* (tuatara.buffer:buffer-string b))
            (tuatara.buffer:buffer-redo b)
            (tuatara.buffer:buffer-redo b)
            (is string= new (tuatara.buffer:buffer-string b))
            ;; another program about to write the file waits for a copy of what the text reads
            (write-from-another-program path "theirs")
            (is string= new (tuatara.buffer:buffer-string b))
            (tuatara.buffer:buffer-undo b)
            (tuatara.buffer:buffer-undo b)
            (is string= *long-lines-text* (tuatara.buffer:buffer-string b))))
        (tuatara.io:finish-leases)
        ;; a change at the start costs a whole file either way: a new file, renamed over
        (put-file path *long-lines-text*)
        (let ((inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path))))
          (with-visited (b path)
            (tuatara.buffer:buffer-insert b 0 "first ")
            (tuatara.io:save-buffer-file b)
            (isnt = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
            (is string= (concatenate 'string "first " *long-lines-text*) (file-text path))))
        ;; where blocks are shared, a new file as before
        (put-file path *long-lines-text*)
        (let ((inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
              (tuatara.io::*block-sharing-file-systems* (list (directory-magic dir))))
          (with-visited (b path)
            (let ((new (edit-far-apart b)))
              (tuatara.io:save-buffer-file b)
              (isnt = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
              (is string= new (file-text path)))))
        (tuatara.io:finish-leases)
        ;; in the background as well, where the save's own thread sees no shared blocks
        (put-file path *long-lines-text*)
        (let ((inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
              (shared (sb-ext:symbol-global-value 'tuatara.io::*block-sharing-file-systems*))
              (in-place (sb-ext:symbol-global-value 'tuatara-user:*save-in-place*)))
          (setf (sb-ext:symbol-global-value 'tuatara.io::*block-sharing-file-systems*) '()
                (sb-ext:symbol-global-value 'tuatara-user:*save-in-place*) t)
          (unwind-protect
               (with-visited (b path)
                 (let* ((new (edit-far-apart b))
                        (job (tuatara.io:start-save b)))
                   (sb-thread:join-thread (tuatara.io:save-job-thread job) :default nil)
                   (tuatara.io:finish-saves)
                   (is eq :done (tuatara.io:save-job-status job))
                   (is = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
                   (is string= new (file-text path))
                   (false (tuatara.buffer:buffer-modified-p b))))
            (setf (sb-ext:symbol-global-value 'tuatara.io::*block-sharing-file-systems*) shared
                  (sb-ext:symbol-global-value 'tuatara-user:*save-in-place*) in-place)))
        (tuatara.io:finish-leases)
        (is equal '("big.txt") (directory-names dir))))))

;; a file system of a kind that can share blocks may not (an XFS made without reflink):
;; it is tried, once, and there a save writes only the changes as on ext4
(define-test (visiting-and-saving saved-in-place-where-cloning-is-refused)
  (with-test-directory (dir)
    (let ((tuatara.io::*block-sharing-file-systems* (list (directory-magic dir)))
          (tuatara.io::*block-sharing-devices* (make-hash-table :synchronized t))
          (tuatara.io:*memory-read-limit* 1024)
          (tuatara-user:*save-in-place* t)
          (tries 0))
      (sb-int:encapsulate 'tuatara.io:clone-fd 'refused
                          (lambda (function source dest)
                            (declare (ignore function source dest))
                            (incf tries)
                            nil))
      (unwind-protect
           (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
                  (inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path))))
             (false (tuatara.io::shares-blocks-p dir))
             (is = 1 tries)
             (is equal (tuatara.io:cache-directory) (tuatara.io:scratch-directory path))
             (with-visited (b path)
               (let ((new (edit-far-apart b)))
                 (tuatara.io:save-buffer-file b)
                 (is = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
                 (is string= new (file-text path))))
             (tuatara.io:finish-leases)
             ;; the answer is kept for the file system, and the try left nothing behind
             (is = 1 tries)
             (is equal '("big.txt") (directory-names dir)))
        (sb-int:unencapsulate 'tuatara.io:clone-fd 'refused)))))

(define-test (visiting-and-saving saved-whole-unless-saving-in-place-is-on)
  ;; saving in place is turned on in the init file: as standard, a save is a new file put in place
  (with-test-directory (dir)
    (let ((tuatara.io::*block-sharing-file-systems* '())
          (tuatara.io:*memory-read-limit* 1024))
      (false tuatara-user:*save-in-place*)
      (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
             (inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path))))
        (with-visited (b path)
          (let ((new (edit-far-apart b)))
            (tuatara.io:save-buffer-file b)
            (isnt = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
            (is string= new (file-text path))
            (is string= new (tuatara.buffer:buffer-string b))))
        (tuatara.io:finish-leases)
        (is equal '("big.txt") (directory-names dir))))))

(defparameter *saving-steps*
  '(:journal-written :journal-flushed :journal-named :directory-flushed :text-moved :writing
    :file-written :file-flushed :journal-removed :directory-flushed)
  "The steps of a save in place, in order, each :WRITING step but the first left out.")

(defun steps-of-a-save (path &optional stop)
  "Visit PATH, edit it far apart and save it in place, noting each step the
save reaches, until STOP, a number, of them have been reached: the save is
then cut short there, as by a crash. Returns the steps and the text saved."
  (let ((steps '()) (new nil))
    (with-visited (b path)
      (setf new (edit-far-apart b))
      (let ((tuatara.io::*saving-step* (lambda (step)
                                         (push step steps)
                                         (when (and stop (= stop (length steps)))
                                           (throw 'crash nil)))))
        (catch 'crash
          (tuatara.io:save-buffer-file b))))
    (tuatara.io:finish-leases)
    (values (nreverse steps) new)))

(define-test (visiting-and-saving in-place-saves-flush-in-order)
  ;; nothing is written into the file before its journal and the journal's name are on disk,
  ;; and the journal goes only once the file is
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*)))
        (is equal *saving-steps* (loop for (step . rest) on (steps-of-a-save path)
                                       unless (and (eq step :writing) (eq (first rest) :writing)) collect step))
        (false (journal-there-p path))))))

(define-test (visiting-and-saving in-place-save-cut-short-at-every-step)
  ;; a crash at any step leaves a file that the next visit makes the old one or the new
  ;; one, never a mix; from the journal's name to its removal, a journal is there
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
             (all (steps-of-a-save path))
             (mixed 0))
        (true (> (count :writing all) 1))
        (loop for stop from 1 to (length all)
              do (put-file path *long-lines-text*)
                 (multiple-value-bind (steps new) (steps-of-a-save path stop)
                   (is equal (subseq all 0 stop) steps)
                   (let ((on-disk (file-text path)))
                     (unless (or (string= on-disk *long-lines-text*) (string= on-disk new))
                       (incf mixed)))
                   (is eq (and (member :journal-named steps) (not (member :journal-removed steps)) t)
                       (journal-there-p path))
                   ;; the next visit puts the old file back where a journal says so
                   (with-visited (b path)
                     (true (or (string= *long-lines-text* (tuatara.buffer:buffer-string b))
                               (string= new (tuatara.buffer:buffer-string b))))
                     (is string= (tuatara.buffer:buffer-string b) (file-text path)))
                   (tuatara.io:finish-leases)
                   (false (journal-there-p path))
                   (is equal '("big.txt") (directory-names dir))))
        ;; cut short while the file was written, it was left mixed, for the journal to mend
        (true (plusp mixed))))))

(define-test (visiting-and-saving in-place-save-failing-or-stopped-puts-the-file-back)
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*)))
        ;; a failure while the file is written: the old file back at once, and said
        (with-visited (b path)
          (let ((new (edit-far-apart b)))
            (let ((tuatara.io::*saving-step* (lambda (step)
                                               (when (eq step :file-written)
                                                 (error "The disk is full.")))))
              (fail (tuatara.io:save-buffer-file b)))
            (is string= *long-lines-text* (file-text path))
            (false (journal-there-p path))
            (true (tuatara.buffer:buffer-modified-p b))
            (is string= new (tuatara.buffer:buffer-string b))
            ;; and a second try saves it
            (tuatara.io:save-buffer-file b)
            (is string= new (file-text path))))
        (tuatara.io:finish-leases)
        ;; stopped while the file is written: as it was
        (put-file path *long-lines-text*)
        (with-visited (b path)
          (edit-far-apart b)
          (let* ((cancel (list nil))
                 (tuatara.io::*saving-step* (lambda (step) (when (eq step :text-moved) (setf (car cancel) t)))))
            (is eq :stopped
                (tuatara.io:write-text-file (tuatara.text:version-rope (tuatara.buffer:buffer-version b)) path
                                            :mappings (tuatara.io::protected-mappings b) :cancel cancel)))
          (is string= *long-lines-text* (file-text path))
          (false (journal-there-p path)))
        (tuatara.io:finish-leases)
        (is equal '("big.txt") (directory-names dir))))))

(define-test (visiting-and-saving save-cut-short-left-alone-on-a-safe-visit)
  ;; a file opened safely is never written: a journal beside it is said, not used
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*)))
        (with-visited (b path)
          (edit-far-apart b)
          (let ((tuatara.io::*saving-step* (lambda (step) (when (eq step :file-written) (throw 'crash nil)))))
            (catch 'crash (tuatara.io:save-buffer-file b))))
        (tuatara.io:finish-leases)
        (let ((mixed (file-text path)))
          (true (journal-there-p path))
          (let ((b (tuatara.io:visit-file path :safe t)))
            (unwind-protect (is string= mixed (tuatara.buffer:buffer-string b))
              (tuatara.buffer:kill-buffer b)))
          (is string= mixed (file-text path))
          (true (journal-there-p path)))
        ;; opened as usual, it is put back
        (with-visited (b path)
          (is string= *long-lines-text* (tuatara.buffer:buffer-string b)))
        (tuatara.io:finish-leases)
        (false (journal-there-p path))))))

(defun cut-short-save (buffer)
  "Save BUFFER in place, cut short once the file is written but before it is
flushed, as by a crash: the file is then part old and part new, its journal
beside it."
  (let ((tuatara.io::*saving-step* (lambda (step) (when (eq step :file-written) (throw 'crash nil)))))
    (catch 'crash (tuatara.io:save-buffer-file buffer))))

(define-test (visiting-and-saving save-cut-short-put-back-with-saving-in-place-off)
  ;; a journal left while saving in place was on is used when it is off again: by a visit, a revert and a save
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*)))
        ;; visited
        (with-visited (b path)
          (edit-far-apart b)
          (cut-short-save b))
        (tuatara.io:finish-leases)
        (true (journal-there-p path))
        (let ((tuatara-user:*save-in-place* nil))
          (with-visited (b path)
            (is string= *long-lines-text* (tuatara.buffer:buffer-string b))))
        (tuatara.io:finish-leases)
        (false (journal-there-p path))
        (is string= *long-lines-text* (file-text path))
        ;; reverted
        (with-visited (b path)
          (edit-far-apart b)
          (cut-short-save b)
          (true (journal-there-p path))
          (let ((tuatara-user:*save-in-place* nil))
            (tuatara.io:revert-buffer-file b))
          (is string= *long-lines-text* (tuatara.buffer:buffer-string b))
          (false (journal-there-p path))
          (is string= *long-lines-text* (file-text path)))
        (tuatara.io:finish-leases)
        ;; saved: the old file put back, then the new one written whole beside it and put in its place
        (with-visited (b path)
          (let ((new (edit-far-apart b)))
            (cut-short-save b)
            (true (journal-there-p path))
            (let ((tuatara-user:*save-in-place* nil)
                  (inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path))))
              (tuatara.io:save-buffer-file b)
              (isnt = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path))))
            (false (journal-there-p path))
            (is string= new (file-text path))
            (false (tuatara.buffer:buffer-modified-p b))))
        (tuatara.io:finish-leases)
        (is equal '("big.txt") (directory-names dir))))))

(defun cut-short-file (path)
  "Make the file at PATH hold *LONG-LINES-TEXT*, then visit it, edit it far
apart and save it in place cut short (CUT-SHORT-SAVE), as by a crash: the
file is left part old and part new, its journal beside it."
  (put-file path *long-lines-text*)
  (with-visited (b path)
    (edit-far-apart b)
    (cut-short-save b))
  (tuatara.io:finish-leases)
  (true (journal-there-p path))
  (file-text path))

(defun visited-text (path)
  "The text a visit of PATH shows, the buffer killed after."
  (with-visited (b path)
    (prog1 (tuatara.buffer:buffer-string b) (tuatara.io:finish-leases))))

(define-test (visiting-and-saving cut-short-save-put-back-in-the-background)
  ;; putting a file back is a job of its own thread, which the visit waits for
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (merge-pathnames "big.txt" dir))
            (seen '()))
        (cut-short-file path)
        (setf (sb-ext:symbol-global-value 'tuatara.io::*saving-step*)
              (lambda (step)
                (when (eq step :journal-removed)
                  (push (list sb-thread:*current-thread*
                              (and tuatara.runtime:*job* (tuatara.runtime:job-description tuatara.runtime:*job*)))
                        seen))))
        (unwind-protect (is string= *long-lines-text* (visited-text path))
          (setf (sb-ext:symbol-global-value 'tuatara.io::*saving-step*) nil))
        (is = 1 (length seen))
        (isnt eq sb-thread:*current-thread* (first (first seen)))
        (true (search "Putting back" (or (second (first seen)) "")))
        (false (find-if (lambda (job) (typep job 'tuatara.io::recovery)) tuatara.runtime:*jobs*))))))

(define-test (visiting-and-saving journal-not-a-plain-file-left-alone)
  ;; a named pipe in the journal's place is never waited on, never used, and the file is not saved past it
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*)))
        (sb-posix:mkfifo (tuatara.io::journal-path (namestring path)) #o600)
        (let ((thread (sb-thread:make-thread
                       (lambda ()
                         (with-visited (b path)
                           (list (tuatara.buffer:buffer-string b)
                                 (progn (edit-far-apart b)
                                        (handler-case (progn (tuatara.io:save-buffer-file b) :saved)
                                          (tuatara.io:file-system-error () :refused)))))))))
          (is equal (list *long-lines-text* :refused) (sb-thread:join-thread thread :timeout 30 :default :hung)))
        (tuatara.io:finish-leases)
        (true (journal-there-p path))
        (is string= *long-lines-text* (file-text path))))))

(define-test (visiting-and-saving journal-of-another-user-left-alone)
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (merge-pathnames "big.txt" dir))
             (mixed (cut-short-file path)))
        ;; the putting back is a job of its own thread
        (setf (sb-ext:symbol-global-value 'tuatara.io::*journal-owner*) (1+ (sb-posix:geteuid)))
        (unwind-protect
             (progn (is string= mixed (visited-text path))
                    (is string= mixed (file-text path))
                    (true (journal-there-p path)))
          (setf (sb-ext:symbol-global-value 'tuatara.io::*journal-owner*) nil))
        ;; the journal and the file this user's: put back
        (is string= *long-lines-text* (visited-text path))
        (false (journal-there-p path))))))

(define-test (visiting-and-saving journal-left-alone-once-the-file-is-written-again)
  ;; another program wrote the file after the save was cut short: its writing is kept, the journal left
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (merge-pathnames "big.txt" dir)))
        (cut-short-file path)
        (write-from-another-program path "theirs")
        (is string= "theirs" (visited-text path))
        (is string= "theirs" (file-text path))
        (true (journal-there-p path))
        ;; the same length, a day later
        (delete-file (tuatara.io::journal-path (namestring path)))
        (let ((same (make-string (length (cut-short-file path)) :initial-element #\x)))
          (with-open-file (out path :direction :output :if-exists :overwrite)
            (write-string same out))
          (let ((later (+ (sb-posix:time) 86400)))
            (sb-posix:utimes (namestring path) later later))
          (is string= same (visited-text path))
          (is string= same (file-text path))
          (true (journal-there-p path)))))))

(define-test (visiting-and-saving journal-of-a-file-gone-left-alone)
  ;; a new file under the name, even one the system gave the old one's inode number, is not the journal's
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (merge-pathnames "big.txt" dir))
             (journal (tuatara.io::journal-path (namestring path))))
        (cut-short-file path)
        (delete-file path)
        (put-file path *lines-text*)
        (let ((fd (sb-posix:open (namestring path) sb-posix:o-rdonly)))
          (unwind-protect (true (tuatara.io::fd-birth-time fd))
            (sb-posix:close fd)))
        (let* ((octets (alexandria:read-file-into-byte-vector journal))
               (header (sb-ext:octets-to-string octets :end 4096 :external-format :latin-1))
               (stamp (tuatara.io:path-stamp path))
               (old (subseq header (search "file " header) (position #\Newline header :start (search "file " header))))
               (new (format nil "file ~d ~d" (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp)))
               (changed (sb-ext:string-to-octets (concatenate 'string (subseq header 0 (search old header)) new
                                                              (subseq header (+ (search old header) (length old))))
                                                 :external-format :latin-1)))
          (with-open-file (out journal :direction :output :if-exists :overwrite :element-type '(unsigned-byte 8))
            (write-sequence changed out)))
        (is string= *lines-text* (visited-text path))
        (is string= *lines-text* (file-text path))
        (true (journal-there-p path))))))

(define-test (visiting-and-saving not-saved-while-a-journal-stays)
  ;; a journal that cannot be used stops a save: the file keeps what it has until the journal is removed
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (merge-pathnames "big.txt" dir)))
        (cut-short-file path)
        (write-from-another-program path "theirs")
        (with-visited (b path)
          (tuatara.buffer:buffer-insert b 0 "mine, ")
          (dolist (tuatara-user:*save-in-place* '(t nil))
            (fail (tuatara.io:save-buffer-file b) 'tuatara.io:file-system-error)
            (is string= "theirs" (file-text path))
            (true (journal-there-p path)))
          (delete-file (tuatara.io::journal-path (namestring path)))
          (tuatara.io:save-buffer-file b)
          (is string= "mine, theirs" (file-text path)))
        (tuatara.io:finish-leases)))))

(defun hold-journal (path)
  "Have another process hold the lock on the journal of PATH, as a save going
on or a putting back does, until it is ended; returns that process once the
lock is held."
  (let* ((journal (tuatara.io::journal-path (namestring path)))
         (holder (sb-ext:run-program "/bin/sh" (list "-c" "exec 9<\"$1\" && flock 9 && exec sleep 60" "sh" journal)
                                     :wait nil)))
    ;; held once a lock of this process's own is refused
    (loop repeat 1000
          while (let ((fd (sb-posix:open journal sb-posix:o-rdonly)))
                  (unwind-protect (tuatara.runtime:lock-fd fd) (sb-posix:close fd)))
          do (sleep 0.01))
    holder))

(defun end-holder (holder)
  "End HOLDER, from HOLD-JOURNAL, letting the journal go."
  (sb-ext:process-kill holder 15)
  (sb-ext:process-wait holder))

(defmacro with-global ((symbol value) &body body)
  "Run BODY with the global value of SYMBOL, as every thread sees it, VALUE,
the old one put back after."
  (let ((old (gensym "OLD")))
    `(let ((,old (sb-ext:symbol-global-value ',symbol)))
       (setf (sb-ext:symbol-global-value ',symbol) ,value)
       (unwind-protect (progn ,@body)
         (setf (sb-ext:symbol-global-value ',symbol) ,old)))))

(define-test (visiting-and-saving journal-in-use-waited-for-or-refused)
  ;; a journal another save or putting back holds: the file is never opened meanwhile; once it is let go,
  ;; the visit goes on, and while it is held too long the visit is refused, the journal not to be removed
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (merge-pathnames "big.txt" dir)))
        (cut-short-file path)
        (let ((holder (hold-journal path)))
          (unwind-protect
               (with-global (tuatara.io::*journal-busy-wait* 1)
                 (let ((refusal (handler-case (progn (visited-text path) nil)
                                  (tuatara.io::journal-busy (e) e))))
                   (true refusal)
                   (when refusal
                     (let ((words (princ-to-string refusal)))
                       (true (search "try again shortly" words) "~a" words)
                       (false (search "remove" words) "~a" words))))
                 (false (tuatara.io:buffer-for-file path))
                 (true (journal-there-p path)))
            (end-holder holder)))
        ;; let go while the visit waits: put back then
        (let ((holder (hold-journal path)))
          (with-global (tuatara.io::*journal-busy-wait* 30)
            (sb-thread:make-thread (lambda () (sleep 1) (end-holder holder)))
            (is string= *long-lines-text* (visited-text path))))
        (false (journal-there-p path))))))

(define-test (visiting-and-saving journal-left-alone-after-its-last-mark)
  ;; a change to the file seconds after the save last marked its journal is another program's
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (merge-pathnames "big.txt" dir))
             (journal (tuatara.io::journal-path (namestring path)))
             (same (make-string (length (cut-short-file path)) :initial-element #\x))
             (told '()))
        (with-open-file (out path :direction :output :if-exists :overwrite)
          (write-string same out))
        (let ((later (+ 10 (floor (tuatara.io:stamp-mtime (tuatara.io:path-stamp journal)) 1000000000))))
          (sb-posix:utimes (namestring path) later later))
        (with-global (tuatara.runtime:*host* (make-answering-host 'tuatara.io:host-say-writing
                                                                  (lambda (words) (push words told))))
          (is string= same (visited-text path)))
        (true (find-if (lambda (words) (search "changed or replaced since" words)) told) "~s" told)
        (is string= same (file-text path))
        (true (journal-there-p path))))))

(define-test (visiting-and-saving journal-marked-while-the-save-writes)
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
             (journal (tuatara.io::journal-path (namestring path)))
             (marked nil) (slept nil))
        (with-visited (b path)
          (edit-far-apart b)
          (let ((tuatara.io::*saving-step*
                  (lambda (step)
                    (case step
                      (:writing (unless slept (setf slept t) (sleep 2.5)))
                      (:file-written
                       (let ((fd (sb-posix:open journal sb-posix:o-rdonly)))
                         (unwind-protect
                              (setf marked (- (tuatara.io:stamp-mtime (tuatara.io:fd-stamp fd journal))
                                              (getf (tuatara.io::read-journal-header fd journal) :made)))
                           (sb-posix:close fd))))))))
            (tuatara.io:save-buffer-file b)))
        (tuatara.io:finish-leases)
        (true (and marked (> marked 1500000000)) "~a" marked)))))

(define-test (visiting-and-saving saved-whole-where-no-unnamed-file-is-made)
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
             (inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
             (tuatara.io::*proc-fd-available* nil))
        (with-visited (b path)
          (let ((new (edit-far-apart b)))
            (tuatara.io:save-buffer-file b)
            (isnt = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
            (is string= new (file-text path))))
        (tuatara.io:finish-leases)
        (is equal '("big.txt") (directory-names dir))))))

(define-test (visiting-and-saving file-replaced-during-a-save-in-place-untouched)
  ;; another file put in its place while the journal is made is never written into
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
             (other (put-file (merge-pathnames "other.txt" dir) *long-lines-text*))
             (kept (merge-pathnames "kept.txt" dir)))
        (sb-posix:link (namestring other) (namestring kept))
        (with-visited (b path)
          (edit-far-apart b)
          (let ((tuatara.io::*saving-step* (lambda (step)
                                             (when (eq step :text-moved)
                                               (sb-posix:rename (namestring other) (namestring path))))))
            (handler-case (tuatara.io:save-buffer-file b)
              (tuatara.io:file-changed-on-disk () nil))))
        (tuatara.io:finish-leases)
        (is string= *long-lines-text* (file-text kept))
        (false (journal-there-p path))))))

(define-test (visiting-and-saving saved-whole-while-other-text-reads-the-file)
  ;; text read from the file outside the buffer (a mapping of its own) would change under a save in place
  (with-test-directory (dir)
    (without-shared-blocks
      (let* ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*))
             (inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
             (mapping (tuatara.text:map-file (namestring path))))
        (unwind-protect
             (with-visited (b path)
               (let ((new (edit-far-apart b)))
                 (tuatara.io:save-buffer-file b)
                 (isnt = inode (tuatara.io:stamp-ino (tuatara.io:path-stamp path)))
                 (is string= new (file-text path))))
          (tuatara.text:close-mapping mapping))
        (tuatara.io:finish-leases)))))

(define-test (visiting-and-saving moved-stretches-not-unmapped-twice)
  ;; stretches moved onto a journal lie within the region mapped whole: only that region is unmapped
  (with-test-directory (dir)
    (without-shared-blocks
      (let ((path (put-file (merge-pathnames "big.txt" dir) *long-lines-text*)))
        (with-visited (b path)
          (edit-far-apart b)
          (tuatara.io:save-buffer-file b)
          (let ((regions (car (tuatara.text::mapping-resources (tuatara.buffer:buffer-mapping b)))))
            (true (> (length regions) 1))
            (is = 1 (count-if #'first regions))))
        (tuatara.io:finish-leases)))))

(define-test (visiting-and-saving scratch-copy-closed-when-it-cannot-be-mapped)
  ;; standard input's copy is closed, not left open, when it cannot be mapped
  (with-test-directory (dir)
    (let* ((path (namestring (put-file (merge-pathnames "copy" dir) *lines-text*)))
           (fd (sb-posix:open path sb-posix:o-wronly))
           (tuatara.io:*memory-read-limit* 1024))
      (fail (tuatara.io:scratch-file-text fd (length *lines-text*) "standard input"))
      (isnt equal path (ignore-errors (sb-posix:readlink (format nil "/proc/self/fd/~d" fd)))))))

(define-test (visiting-and-saving size-of-a-path)
  (with-test-directory (dir)
    (let ((file (put-file (merge-pathnames "five.txt" dir) "12345")))
      (is = 5 (tuatara.io:path-size file))
      (is = 0 (tuatara.io:path-size (merge-pathnames "none.txt" dir)))
      (is = 7 (tuatara.io:path-size (merge-pathnames "none.txt" dir) 7)))))

(define-test (visiting-and-saving path-question-starts-again-on-slash)
  ;; a ~/ or a / typed after a directory starts the path again: the line holds just that
  (let ((f (run-keys "C-x C-f C-a C-k ~ / s r c / ~ /" :width 80 :height 12)))
    (true (tuatara.frontend.fake:fake-frame-showing f "Find file"))
    (false (tuatara.frontend.fake:fake-frame-showing f "~/src/~/"))
    (false (tuatara.frontend.fake:fake-frame-showing f "~/~/")))
  (let ((f (run-keys "C-x C-f C-a C-k / e t c / /" :width 80 :height 12)))
    (false (tuatara.frontend.fake:fake-frame-showing f "/etc//"))))

(define-test (visiting-and-saving path-question-deleting-keeps-the-line)
  ;; only what is typed starts the path again: deleting back to a // leaves the line as it is
  (let ((text nil))
    (run-keys (list "C-k C-x C-f C-a C-y C-k"
                    (lambda ()
                      (setf text (copy-seq (tuatara.command:editor-minibuffer-text tuatara.command:*editor*)))))
              :text "/a//" :width 80 :height 12)
    (is string= "/a//" text)))

(define-test (visiting-and-saving failure-words-written-as-given)
  ;; a reading that failed is shown in the words its caller makes of why, names with ~ in them as they are
  (let* ((b (tuatara.buffer:make-buffer "*failed reading*"))
         (job (tuatara.defaults::make-differences-job b "/tmp/a~1" "/tmp/b~2")))
    (unwind-protect
         (progn
           (setf (tuatara.runtime:job-status job) :failed (tuatara.runtime:job-error job) "no room")
           (tuatara.runtime:job-take-in job)
           (let ((text (tuatara.buffer:buffer-string b)))
             (true (search "/tmp/a~1 and /tmp/b~2 could not be compared: " text) "~a" text)))
      (tuatara.buffer:kill-buffer b))))

(define-test (visiting-and-saving opened-without-following-a-link)
  ;; asked not to follow one, a link at the name itself is refused, what it leads to never opened;
  ;; a file there is opened, and a link is followed as before when not asked
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "plain.so" dir)))
          (link (namestring (merge-pathnames "link.so" dir))))
      (alexandria:write-string-into-file "x" file)
      (sb-posix:symlink file link)
      (true (typep (handler-case (tuatara.io:open-for-reading link nil :accept '(:regular) :follow nil)
                     (tuatara.io:file-system-error (e) e))
                   'tuatara.io:file-system-error))
      (dolist (path (list file link))
        (let ((fd (tuatara.io:open-for-reading path nil :accept '(:regular) :follow (string= path link))))
          (true fd)
          (sb-posix:close fd))))))
