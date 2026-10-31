;;;; lisp/files/hash.lisp --- SHA-256 sums of a file on disk or of the selected text, in the background.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x sha256-sum sums the file the buffer visits as it is on disk, or with a
;;;; selection the octets selected, or in a buffer with no file its whole text.
;;;; The sum is shown, kept in *Messages* and put on the kill ring to paste into
;;;; notes. A thread of its own reads the octets in pieces, a file through a
;;;; descriptor opened without changing its access time where the system allows,
;;;; and sums them with OpenSSL's libcrypto, whose SHA-256 uses the processor's
;;;; SHA instructions, several times as fast as GNU sha256sum. Without that library the octets are written to
;;;; sha256sum through a pipe instead. Progress shows in the mode line, and M-x
;;;; stop-job stops it. Nothing of the file is run: the library only reads the
;;;; octets it is given, and the program is started with an argument list, never
;;;; through a shell.

(in-package #:tuatara.defaults)

(defvar *hash-library* "libcrypto.so.3"
  "OpenSSL's crypto library, which sums with the processor's SHA instructions;
NIL, or a library that cannot be loaded, and *HASH-PROGRAM* sums instead.")

(defvar *hash-program* "sha256sum"
  "The program that sums when *HASH-LIBRARY* cannot: GNU coreutils' sha256sum,
found on the PATH, which reads octets on its standard input and prints their
SHA-256 sum in hex.")

(defvar *hash-piece* (* 8 1024 1024)
  "Octets read and summed at a time: the step of the progress shown and of stopping.")

(defparameter *hash-carried-settings* '(*hash-library* *hash-program* *hash-piece*)
  "The settings a sum reads as it works, carried to the thread it works on as
they are on the editor's when it starts (TUATARA.RUNTIME:CARRY-SETTINGS).")

(defvar *hash-wait* 1/2
  "Seconds sha256-sum waits for a sum before letting the work go on in the background.")

(defstruct (hash-job (:include tuatara.runtime:job) (:constructor %make-hash-job) (:copier nil))
  "A SHA-256 sum computed for BUFFER, a job (TUATARA.RUNTIME:JOB): WHAT is
summed, for messages, TOTAL octets of it, the car of DONE counting those
summed so far; SUM, once done."
  what (total 0) (done (list 0)) sum)

(setf (documentation 'hash-job-p 'function) "True when OBJECT is a HASH-JOB.")

(tuatara.runtime:define-job-words hash-job
  :at-exit :quiet
  :doing "Summing"
  :description (format nil "Summing ~a" (hash-job-what job))
  :verb (format nil "sum ~a" (hash-job-what job))
  :left-words "nothing summed")

(defun hash-library-p ()
  "True when *HASH-LIBRARY* is loaded, loading it for the first sum (FOREIGN-LIBRARY-P)."
  (and *hash-library* (foreign-library-p *hash-library*)))

(defun hash-progress (job)
  "How much of JOB is summed, from 0 to 99 while it runs."
  (tuatara.runtime:running-percent (car (hash-job-done job)) (hash-job-total job)))

(defmethod tuatara.runtime:job-percent ((job hash-job))
  "How much of the sum JOB is done, from 0 to 99 while it runs."
  (hash-progress job))

(defun library-sum (job source buffer)
  "The SHA-256 sum, in hex, of the octets SOURCE gives, summed by *HASH-LIBRARY*;
NIL when JOB is stopped first. SOURCE is a function that fills BUFFER with the
next octets and returns how many, 0 at the end; each piece counts in JOB's DONE."
  (let ((ctx (foreign-call "EVP_MD_CTX_new" sb-sys:system-area-pointer))
        (null (sb-sys:int-sap 0)))
    (when (sb-sys:sap= ctx null) (error "~a could not start a sum" *hash-library*))
    (unwind-protect
         (progn
           (unless (= 1 (foreign-call "EVP_DigestInit_ex" sb-alien:int (sb-sys:system-area-pointer ctx)
                                     (sb-sys:system-area-pointer (foreign-call "EVP_sha256" sb-sys:system-area-pointer))
                                     (sb-sys:system-area-pointer null)))
             (error "~a could not start a SHA-256 sum" *hash-library*))
           (loop for n = (if (car (hash-job-cancel job)) (return-from library-sum nil) (funcall source buffer))
                 while (plusp n)
                 do (sb-sys:with-pinned-objects (buffer)
                      (foreign-call "EVP_DigestUpdate" sb-alien:int (sb-sys:system-area-pointer ctx)
                                   (sb-sys:system-area-pointer (sb-sys:vector-sap buffer)) (sb-alien:unsigned-long n)))
                    (incf (car (hash-job-done job)) n))
           (let ((sum (make-array 32 :element-type '(unsigned-byte 8))))
             (sb-sys:with-pinned-objects (sum)
               (foreign-call "EVP_DigestFinal_ex" sb-alien:int (sb-sys:system-area-pointer ctx)
                            (sb-sys:system-area-pointer (sb-sys:vector-sap sum)) (sb-sys:system-area-pointer null)))
             (tuatara.text:octets-hex sum)))
      (foreign-call "EVP_MD_CTX_free" sb-alien:void (sb-sys:system-area-pointer ctx)))))

(defun program-sum (job source buffer program)
  "The SHA-256 sum, in hex, of the octets SOURCE gives (as for LIBRARY-SUM),
written through a pipe to PROGRAM, which prints it; NIL when JOB is stopped
first. Signals, with what the program said, when it prints no sum."
  (tuatara.io:call-with-program
   (list (or (tuatara.io:find-program program) (error "There is no program ~a to take the sum with" program)))
   (lambda (process)
     (let ((fd (sb-sys:fd-stream-fd (sb-ext:process-input process))))
       (loop for n = (if (car (hash-job-cancel job)) 0 (funcall source buffer))
             while (plusp n)
             do (handler-case (tuatara.io:write-octets fd buffer :end n :path program)
                  ;; it ended without reading everything: what it printed says why
                  (tuatara.io:file-system-error () (return)))
                (incf (car (hash-job-done job)) n)))
     (ignore-errors (close (sb-ext:process-input process)))
     (sb-ext:process-wait process)
     (unless (car (hash-job-cancel job))
       (let* ((output (with-output-to-string (s)
                        (loop for line = (read-line (sb-ext:process-output process) nil) while line
                              do (write-line line s))))
              (said (string-trim '(#\Newline #\Space) output)))
         (if (and (eql (sb-ext:process-exit-code process) 0) (>= (length said) 64)
                  (every (lambda (c) (digit-char-p c 16)) (subseq said 0 64)))
             (string-downcase (subseq said 0 64))
             (error "~a failed~@[: ~a~]" program (and (plusp (length said)) said))))))
   :job job :input :stream :output :stream :error :output :external-format :default))

(defun file-source (fd path)
  "A source for LIBRARY-SUM of the octets of the open file FD (PATH for
messages) to its end, read at explicit offsets while the system reads ahead;
to the end found, not the size reported, which is 0 for the files the kernel
makes up as they are read (/proc)."
  (let ((at 0))
    (lambda (buffer)
      (let ((n (tuatara.io:read-fd-at fd buffer at :path path)))
        (when (plusp n)
          (tuatara.io:advise-will-need fd (+ at n) (length buffer))
          (incf at n))
        n))))

(defun text-source (rope start end)
  "A source for LIBRARY-SUM of ROPE's octets from START below END."
  (let ((at start))
    (lambda (buffer)
      (let ((n (min (length buffer) (- end at))))
        (when (plusp n)
          (tuatara.text:rope-copy-into rope at (+ at n) buffer)
          (incf at n))
        n))))

(defun hash-piece-octets (total)
  "How many octets a sum of TOTAL octets reads at a time: *HASH-PIECE*, or
TOTAL when less; *HASH-PIECE* for a file that says it holds none, as the files
the kernel makes up as they are read do (/proc, /sys)."
  (if (plusp total) (min *hash-piece* total) *hash-piece*))

(defun start-hash (buffer what total source &optional finish)
  "Start a job summing the TOTAL octets SOURCE gives for BUFFER, WHAT describing
them (TUATARA.RUNTIME:START-JOB-THREAD), which calls FINISH, when given, with
the sum once it has one (to check or signal) and at last with NIL (to let go
of what SOURCE reads), with *HASH-CARRIED-SETTINGS* bound there as they are
here. Returns the job."
  (let ((job (%make-hash-job :buffer buffer :what what :total total))
        (library (hash-library-p)))
    (tuatara.runtime:start-job-thread
     job (tuatara.runtime:carry-settings
          *hash-carried-settings*
          (lambda (job)
            (let ((sum (unwind-protect
                            (let* ((buffer (make-array (hash-piece-octets total) :element-type '(unsigned-byte 8)))
                                   (sum (if library
                                            (library-sum job source buffer)
                                            (program-sum job source buffer *hash-program*))))
                              (when (and sum finish) (funcall finish sum))
                              sum)
                         (when finish (ignore-errors (funcall finish nil))))))
              (setf (hash-job-sum job) sum (hash-job-status job) (if sum :done :stopped)))))
     :name "tuatara sum" :details (format nil "~:d octets" total))))

(defun hash-file (buffer path)
  "Start summing the file at PATH as it is on disk, for BUFFER; or say why not
and return NIL. A file that changes while it is summed has no one sum: that is
said instead, except for the files the kernel makes up as they are read."
  (let ((fd (tuatara.io:open-for-reading path t)) (started nil))
    (unwind-protect
         (let ((stamp (tuatara.io:fd-stamp fd path))
               (made-up (eq :pseudo (tuatara.io:file-system-class (tuatara.io:fd-file-system fd)))))
           (if (not (eq (tuatara.io:stamp-kind stamp) :regular))
               (message "~a is not a regular file: nothing summed" (abbreviated-path path))
               (setf started
                     (start-hash buffer (format nil "~a on disk~:[~;, not the buffer's unsaved changes~]"
                                                (abbreviated-path path) (tuatara.buffer:buffer-modified-p buffer))
                                 (tuatara.io:stamp-size stamp) (file-source fd path)
                                 (lambda (sum)
                                   (if sum
                                       (unless (or made-up (tuatara.io:same-content-p stamp (tuatara.io:fd-stamp fd path)))
                                         (error "~a changed while being summed; sum it again" (abbreviated-path path)))
                                       (sb-posix:close fd)))))))
      (unless started (sb-posix:close fd)))
    started))

(defun file-sha256 (path &key (cancel (list nil)) (done (list 0)) buffer)
  "The SHA-256 sum, in hex, of the file at PATH as it is on disk, summed on the
thread that asks (as \\[sha256-sum] sums, read without changing its access time
where the system allows), the car of DONE counting the octets summed; NIL when
it cannot be read or CANCEL's car turns true first. BUFFER, octets to read
into, is made when not given."
  (let ((fd (ignore-errors (tuatara.io:open-for-reading path t))))
    (when fd
      (unwind-protect
           (let* ((carrier (%make-hash-job :what path :cancel cancel :done done))
                  (buffer (or buffer (make-array *hash-piece* :element-type '(unsigned-byte 8))))
                  (source (file-source fd path)))
             (ignore-errors
              (if (hash-library-p)
                  (library-sum carrier source buffer)
                  (program-sum carrier source buffer *hash-program*))))
        (ignore-errors (sb-posix:close fd))))))

(defun octets-sha256 (octets)
  "The SHA-256 sum, in hex, of OCTETS, summed as \\[sha256-sum] sums."
  (let* ((carrier (%make-hash-job :what "octets" :cancel (list nil) :done (list 0)))
         (buffer (make-array (max 1 (min *hash-piece* (length octets))) :element-type '(unsigned-byte 8)))
         (at 0)
         (source (lambda (buffer)
                   (let ((n (min (length buffer) (- (length octets) at))))
                     (replace buffer octets :start2 at :end2 (+ at n))
                     (incf at n)
                     n))))
    (if (hash-library-p)
        (library-sum carrier source buffer)
        (program-sum carrier source buffer *hash-program*))))

(defmethod tuatara.runtime:job-take-in ((job hash-job))
  "Say how JOB, which has ended, went: show its sum, which *Messages* keeps, and
put it on the kill ring; or say why there is none. Nothing is said once its
buffer was killed."
  (unless (killed-job-buffer-p job)
    (ecase (hash-job-status job)
      (:done (kill-text (hash-job-sum job) :append nil)
             (message "SHA-256 of ~a: ~a (copied)" (hash-job-what job) (hash-job-sum job)))
      ((:stopped :failed) (message "~a" (tuatara.runtime:job-ending-words job))))))

(define-command sha256-sum ()
  "Show the SHA-256 sum of the file the buffer visits as it is on disk, or with
a selection of the octets selected, or in a buffer with no file of its whole
text; keep it in *Messages* and put it on the kill ring. The file is read
without changing its access time where the system allows. A large one is summed
in the background with its progress in the mode line; \\[stop-job] stops it.
While another job runs in the buffer it waits its turn (\\[list-jobs])."
  (let ((b (current-buffer)))
    (multiple-value-bind (start end) (and (tuatara.buffer:buffer-mark-active-p b) (tuatara.buffer:buffer-region b))
      (let ((selection (and start (< start end))))
        (unless (queue-if-busy b (cond (selection (format nil "Summing the selection (~:d octet~:p)" (- end start)))
                                       ((tuatara.buffer:buffer-file b)
                                        (format nil "Summing ~a on disk" (abbreviated-path (tuatara.buffer:buffer-file b))))
                                       (t (format nil "Summing ~a's text" (tuatara.buffer:buffer-name b))))
                               (and selection (list (cons start :before) (cons end :after)))
                               (lambda (&optional start end) (start-sum b start end)))
          (start-sum b (and selection start) (and selection end)))))))

(defun start-sum (b start end)
  "Start the sum \\[sha256-sum] makes for B: of its text from START to END when
given, else of its file on disk, or of its whole text without one; say how it
goes."
  (let ((job (cond (start
                    (start-hash b (format nil "the selection (~:d octet~:p)" (- end start)) (- end start)
                                (text-source (tuatara.buffer:buffer-text b) start end)))
                   ((tuatara.buffer:buffer-file b)
                    (hash-file b (tuatara.buffer:buffer-file b)))
                   (t (let ((size (tuatara.buffer:buffer-size b)))
                        (start-hash b (format nil "~a's text (~:d octet~:p)" (tuatara.buffer:buffer-name b) size) size
                                    (text-source (tuatara.buffer:buffer-text b) 0 size)))))))
    (when job
      (take-in-soon job *hash-wait* (format nil "Summing ~a" (hash-job-what job))))))
