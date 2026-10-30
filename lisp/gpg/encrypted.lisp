;;;; lisp/gpg/encrypted.lisp --- Files encrypted with GnuPG (.gpg): opened decrypted, saved encrypted, their text never on disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file whose name ends in .gpg opens decrypted: gpg (GnuPG) reads it from
;;;; a pipe and gives the text back through one, straight into memory, in the
;;;; background. A save sends the text through gpg first, encrypted to the keys
;;;; the file was encrypted to (or with a passphrase, as it was), so that only
;;;; the encrypted octets touch the disk; a new .gpg file asks whom to encrypt
;;;; it to in the minibuffer, keys or a passphrase, several at once, commas
;;;; between them. The passphrase is asked for by gpg-agent's own
;;;; pinentry, which keeps it out of the editor: its window in a desktop
;;;; session; in a terminal without one, its prompt in the terminal, lent to
;;;; it while gpg runs. Only where neither can ask, or when you say so, the
;;;; minibuffer asks, shown as stars and kept no longer than it takes to hand
;;;; it to gpg (*GPG-PASSPHRASE-FROM*).
;;;;
;;;; The decrypted text is kept from the disk and from other programs: no
;;;; auto-saved copy, no session file, no scratch file of a large sort or
;;;; replacement (refused past memory), no git gutter, no system clipboard but
;;;; on request (M-x copy-to-clipboard), no core dump of the editor while one is
;;;; open; an export asks first, since what it writes is not encrypted. A file
;;;; opened safely is never decrypted: evidence stays its encrypted octets.

(in-package #:tuatara.defaults)

(defvar *gpg-program* "gpg"
  "The GnuPG program that decrypts and encrypts .gpg files, by name or full path.")

(defvar *gpg-passphrase-from* :auto
  "Who asks for a passphrase gpg needs: :AGENT, gpg-agent's pinentry in its
own window; :TERMINAL, gpg-agent's pinentry in the terminal the editor shows
in, lent to it while gpg runs; :MINIBUFFER, the minibuffer, what is typed
shown as stars (gpg's loopback: the passphrase then passes through the
editor); :AUTO, the agent's window in a desktop session, else the terminal,
else the minibuffer. Pinentry keeps the passphrase out of the editor.")

(defvar *gpg-loopback-warning* t
  "When true, the minibuffer asking for a passphrase for gpg's loopback shows
above it, while it asks, a window saying why it asks, how the passphrase goes
and what that risks (the garbage collector's copies, memory not locked, no
keyboard grab, a look-alike). NIL: the question alone, once you know them.")

(defun gpg-path ()
  "The gpg program found (*GPG-PROGRAM*), or NIL."
  (tuatara.io:find-program *gpg-program*))

(defun gpg-terminal ()
  "The terminal device the editor shows in, which gpg-agent's pinentry can be lent; NIL in a window of its own."
  (and *editor* (tuatara.frontend:frontend-terminal-device (editor-frontend *editor*))))

(defun gpg-asker ()
  "Who asks for passphrases, as *GPG-PASSPHRASE-FROM* says: :AGENT, :TERMINAL or :MINIBUFFER."
  (case *gpg-passphrase-from*
    ((:agent :minibuffer) *gpg-passphrase-from*)
    (:terminal (if (gpg-terminal) :terminal :minibuffer))
    (t (cond ((or (uiop:getenv "DISPLAY") (uiop:getenv "WAYLAND_DISPLAY")) :agent)
             ((gpg-terminal) :terminal)
             (t :minibuffer)))))

(defun gpg-minibuffer-p ()
  "True when the minibuffer, not gpg-agent's pinentry, asks for passphrases (GPG-ASKER)."
  (eq (gpg-asker) :minibuffer))

(defun loopback-warning ()
  "The warning shown above the minibuffer while it asks for a passphrase for
gpg's loopback: why it asks, how the passphrase goes, and what that risks."
  (format nil "Warning: this passphrase goes through Tuatara, not gpg-agent's pinentry.
Why: ~a.
How: gpg's loopback mode. Tuatara holds what you type as text, hands it to gpg
through a pipe of its own (never a file or an argument), then overwrites it;
it is kept nowhere and goes to no log. The risks pinentry does not have:
 - the garbage collector may have copied it in memory before it is overwritten,
   and a copy stays until that memory is used again;
 - that memory is not locked: it can reach swap on disk, or a capture of memory;
 - typing has no keyboard grab: a program watching the keyboard could see it;
 - a program that only looks like Tuatara could ask the same way.
Pinentry keeps it out of Tuatara: *gpg-passphrase-from* :auto uses gpg-agent's
window on a desktop, or its prompt in the terminal Tuatara runs in. C-g: nothing.
Off: (setf *gpg-loopback-warning* nil).~%"
          (if (eq *gpg-passphrase-from* :minibuffer)
              "you chose it (*gpg-passphrase-from* :minibuffer)"
              "no desktop for gpg-agent's window, and no terminal Tuatara can lend its prompt")))

(defun gpg-read-passphrase (prompt &key confirm)
  "Ask for a passphrase gpg needs in the minibuffer (READ-PASSWORD, PROMPT and
CONFIRM as it takes them), the risks of gpg's loopback said in a window above
it (LOOPBACK-WARNING) while it asks, unless *GPG-LOOPBACK-WARNING* is NIL."
  (if (not *gpg-loopback-warning*)
      (read-password prompt :confirm confirm)
      (let* ((warning (buffer-in-mode "*Passphrase warning*" 'special-mode))
             (text (loopback-warning)))
        (set-made-text warning text :spans (list (list 0 (position #\Newline text) :warning)))
        (setf (tuatara.buffer:buffer-point warning) 0)
        (let ((window (display-buffer-at warning :bottom :size :fit)))
          (when window
            (setf (tuatara.display:window-start window) 0 (tuatara.display:window-point window) 0))
          (unwind-protect (read-password prompt :confirm confirm)
            (when (and window (member window (editor-windows *editor*))) (leave-window window))
            (tuatara.buffer:kill-buffer warning))))))

(defun with-gpg-terminal (words function)
  "Call FUNCTION, gpg run in it, with the terminal lent to gpg-agent's pinentry
when it asks there (GPG-ASKER :TERMINAL), WORDS shown on the terminal
meanwhile; else just call it."
  (if (eq (gpg-asker) :terminal)
      (tuatara.frontend:frontend-lend-terminal
       (editor-frontend *editor*)
       (lambda ()
         (format t "~%Tuatara: ~a; gpg-agent asks here for a passphrase it needs.~%" words)
         (finish-output)
         (funcall function)))
      (funcall function)))

;;; Running gpg

(defparameter *gpg-least-output-limit* (* 64 1024 1024)
  "The most octets gpg may write however little it is given (GPG-MOST-OUTPUT).")

(defun gpg-most-output (size)
  "The most octets gpg may write for an input of SIZE octets: 32 times SIZE,
at least *GPG-LEAST-OUTPUT-LIMIT*; a file that would decompress to more is
refused."
  (max *gpg-least-output-limit* (* 32 size)))

(defun gpg-feed (fd octets path)
  "Write to FD, a pipe gpg reads, OCTETS, or the file at PATH a block at a
time (FILE-SOURCE), never read whole into memory."
  (if path
      (tuatara.io:call-with-file path
                                 (lambda (in name size)
                                   (declare (ignore size))
                                   (let ((source (file-source in name))
                                         (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
                                     (loop for n = (funcall source buffer)
                                           while (plusp n)
                                           do (tuatara.io:write-octets fd buffer :end n :path "gpg")))))
      (tuatara.io:write-octets fd octets :path "gpg")))

(defun gpg-run (arguments &key input input-path passphrase cancel (asker (gpg-asker)) second-input second-path)
  "Run gpg with ARGUMENTS, never through a shell, giving it INPUT (octets),
or the file at INPUT-PATH a block at a time, through a pipe and taking what
it writes the same way, so that nothing it reads or writes touches the disk;
PASSPHRASE, when given, through a pipe of its own. CANCEL, a cons, stops it
when its car turns true. ASKER (GPG-ASKER) says how a passphrase gpg needs
is asked for: by the minibuffer (loopback, PASSPHRASE given), by gpg-agent
in the terminal lent it (call it within WITH-GPG-TERMINAL), or by its window.
SECOND-INPUT, more octets (a detached signature), or the file at
SECOND-PATH, reach gpg through a pipe of their own, named in ARGUMENTS by
:SECOND-INPUT (made -&N, gpg's name for a descriptor). Its status lines come
through a pipe of their own too, apart from what it says, which a file could
put words in. More output than GPG-MOST-OUTPUT stops it. Four values: the
octets it wrote, its status lines (\"ENC_TO ...\"), true when it ended well,
and what it said besides. An error meanwhile writes no passphrase or text
to the debug log (*SECRET-AT-HAND*)."
  (let* ((tuatara.runtime:*secret-at-hand* t)
         (program (or (gpg-path) (tuatara.io:program-missing "gpg" :why (outside-program-install-words :gpg))))
         (most (gpg-most-output (if input-path
                                    (or (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp input-path))) 0)
                                    (length input))))
         (pipe (and passphrase (multiple-value-list (sb-posix:pipe))))
         (second (and (or second-input second-path) (multiple-value-list (sb-posix:pipe))))
         (status (multiple-value-list (sb-posix:pipe)))
         (process nil) (statuses '()) (said '()) (output nil) (too-much nil))
    (unwind-protect
         (progn
           (when pipe
             ;; the passphrase written and the pipe closed before gpg starts: it reads it whole
             (let ((stream (sb-sys:make-fd-stream (second pipe) :output t :external-format :utf-8 :buffering :full)))
               (write-string passphrase stream) (terpri stream) (close stream)
               (setf (second pipe) nil)))
           (tuatara.io:call-with-program
            (list* program
                   (append (list "--yes" "--status-fd" (princ-to-string (second status))
                                 "--max-output" (princ-to-string most))
                           (and (not (eq asker :terminal)) (list "--no-tty"))
                           (and (eq asker :minibuffer) (list "--pinentry-mode" "loopback"))
                           (and pipe (list "--batch" "--passphrase-fd" (princ-to-string (first pipe))))
                           (and (eq asker :minibuffer) (not pipe) (list "--batch"))
                           (and second (list "--enable-special-filenames"))
                           (substitute (and second (format nil "-&~d" (first second))) :second-input arguments)))
            (lambda (started)
              (setf process started)
              (sb-posix:close (second status)) (setf (second status) nil)
              (when pipe (sb-posix:close (first pipe)) (setf (first pipe) nil))
              (when second
                ;; the second input written on a thread of its own, gpg reading it as it goes
                (sb-posix:close (first second)) (setf (first second) nil)
                (let ((fd (shiftf (second second) nil)))
                  (tuatara.runtime:make-guarded-thread
                   (lambda ()
                     (unwind-protect (ignore-errors (gpg-feed fd second-input second-path))
                       (ignore-errors (sb-posix:close fd))))
                   :name "tuatara gpg second input")))
              (let* ((status-fd (shiftf (first status) nil))
                     (writer (and (or input input-path)
                                  (tuatara.runtime:make-guarded-thread
                                   (lambda ()
                                     (ignore-errors (gpg-feed (sb-sys:fd-stream-fd (sb-ext:process-input process)) input input-path))
                                     (ignore-errors (close (sb-ext:process-input process))))
                                   :name "tuatara gpg input")))
                     (reader (tuatara.runtime:make-guarded-thread
                              (lambda ()
                                (let ((lines (sb-sys:make-fd-stream status-fd :input t :external-format :latin-1 :buffering :full)))
                                  (unwind-protect
                                       (ignore-errors
                                        (loop for line = (tuatara.io:read-line-bounded lines 4096)
                                              for count from 0
                                              while line
                                              when (and (< count 10000) (uiop:string-prefix-p "[GNUPG:] " line))
                                                do (push (subseq line 9) statuses)))
                                    (close lines))))
                              :name "tuatara gpg status"))
                     (listener (tuatara.runtime:make-guarded-thread
                                (lambda ()
                                  (ignore-errors
                                   (loop for line = (tuatara.io:read-line-bounded (sb-ext:process-error process) 1024)
                                         for count from 0
                                         while line
                                         when (< count 200) do (push line said))))
                                :name "tuatara gpg messages"))
                     (cancelled nil))
                ;; what was decrypted is kept only in OUTPUT
                (setf (values output too-much cancelled)
                      (tuatara.io:read-stream-octets (sb-ext:process-output process) :limit most :cancel cancel :wipe t))
                (when (or too-much cancelled) (sb-ext:process-kill process 15))
                (unless output (setf output (make-array 0 :element-type '(unsigned-byte 8))))
                (when writer (sb-thread:join-thread writer :default nil))
                (sb-thread:join-thread reader :default nil)
                (sb-thread:join-thread listener :default nil)
                (sb-ext:process-wait process)
                (when too-much
                  (push (format nil "it gave more than ~a, far more than this holds: stopped" (octets-words most)) said))))
            :input (if (or input input-path) :stream nil) :output :stream :error :stream :external-format :latin-1
            :preserve-fds (append (list (second status)) (and pipe (list (first pipe))) (and second (list (first second))))
            :environment (let ((device (and (eq asker :terminal) (gpg-terminal))))
                           (if device
                               (cons (format nil "GPG_TTY=~a" device)
                                     (remove-if (lambda (e) (uiop:string-prefix-p "GPG_TTY=" e)) (sb-ext:posix-environ)))
                               (sb-ext:posix-environ)))))
      (when pipe (dolist (fd pipe) (when fd (ignore-errors (sb-posix:close fd)))))
      (when second (dolist (fd second) (when fd (ignore-errors (sb-posix:close fd)))))
      (dolist (fd status) (when fd (ignore-errors (sb-posix:close fd)))))
    (values output (nreverse statuses)
            (and process (not too-much) (eql 0 (sb-ext:process-exit-code process)) (not (and cancel (car cancel))))
            (format nil "~{~a~^; ~}" (nreverse said)))))

(defun gpg-status-p (statuses name)
  "True when STATUSES, gpg's status lines, have one called NAME."
  (find-if (lambda (s) (or (string= s name) (uiop:string-prefix-p (concatenate 'string name " ") s))) statuses))

(defun gpg-failure-words (statuses said)
  "Why gpg failed, as words, from its STATUSES and what it SAID."
  (cond ((some (lambda (s) (search "BAD_PASSPHRASE" s)) statuses) "the passphrase was not right")
        ((or (gpg-status-p statuses "MISSING_PASSPHRASE") (gpg-status-p statuses "NO_PASSPHRASE")) "no passphrase was given")
        ((gpg-status-p statuses "NO_SECKEY") "none of the keys it is encrypted to is yours")
        ((gpg-status-p statuses "NODATA") "it holds no encrypted data")
        ((plusp (length said)) said)
        (t "gpg failed")))

;;; Decrypting

(defun gpg-decrypt (octets &key passphrase cancel)
  "OCTETS, an encrypted file's, decrypted by gpg, as octets; as a second value
a plist of what is known of how: :KEYS, the IDs of the keys it is encrypted
to; :SYMMETRIC, true when a passphrase opens it; :NEEDS-PASSPHRASE, true when
it failed for want of one; :ERROR, why it failed (NIL when it did not)."
  (multiple-value-bind (output statuses ok said)
      (gpg-run (list "--decrypt" "--output" "-") :input octets :passphrase passphrase :cancel cancel)
    (let ((keys (loop for s in statuses
                      when (uiop:string-prefix-p "ENC_TO " s)
                        collect (second (uiop:split-string s :separator '(#\Space)))))
          (symmetric (and (gpg-status-p statuses "NEED_PASSPHRASE_SYM") t))
          (decrypted (and ok (gpg-status-p statuses "DECRYPTION_OKAY"))))
      (values (and decrypted output)
              (list :keys (remove-duplicates keys :test #'string=) :symmetric symmetric
                    :needs-passphrase (and (not decrypted) (not passphrase)
                                           (or (gpg-status-p statuses "MISSING_PASSPHRASE") (gpg-status-p statuses "NO_PASSPHRASE")
                                               (gpg-status-p statuses "NEED_PASSPHRASE_SYM") (gpg-status-p statuses "NEED_PASSPHRASE"))
                                           t)
                    :error (and (not decrypted) (gpg-failure-words statuses said)))))))

(defun decrypt-naming-keys (octets &key passphrase cancel)
  "OCTETS decrypted as GPG-DECRYPT does, its plist of how with :KEY-WORDS
besides, the keys it is encrypted to as words (ENCRYPTION-KEY-WORDS), looked
up once it is decrypted."
  (multiple-value-bind (text info) (gpg-decrypt octets :passphrase passphrase :cancel cancel)
    (values text (if (and text (getf info :keys))
                     (list* :key-words (encryption-key-words (getf info :keys)) info)
                     info))))

(defun openpgp-symmetric-p (path)
  "True when the encrypted file at PATH begins with a passphrase's packet (a
symmetric-key encrypted session key), as gpg --symmetric writes them; only
its first octets are read."
  (eql 3 (tuatara.io:openpgp-packet-header (tuatara.io:file-head path 16) 0)))

;;; Keeping the text from the disk

(defvar *encrypted-buffers* '()
  "The buffers holding text that was encrypted (KEEP-FROM-DISK).")

(defun encrypted-buffer-p (buffer)
  "True when BUFFER holds the decrypted text of an encrypted file."
  (and buffer (tuatara.buffer:buffer-local buffer :encrypted) t))

(defun set-dumpable (dumpable)
  "Allow the system to keep a core dump of the editor should it crash, or not
\(Linux's PR_SET_DUMPABLE): not while an encrypted file's text is in memory."
  (ignore-errors
   (sb-alien:alien-funcall (sb-alien:extern-alien "prctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                                    sb-alien:unsigned-long sb-alien:unsigned-long sb-alien:unsigned-long))
                           4 (if dumpable 1 0) 0 0 0)))

(defun keep-from-disk (buffer)
  "Keep BUFFER's text, which was encrypted, from the disk and from other
programs (its :PLAINTEXT local, which auto-save, the session, scratch files,
the git gutter, the clipboard, exports and code files heed); no core dump of
the editor while it is open. A save writes it encrypted, or asks first where
it would write it as it is (ENCRYPTED-SAVE-TRANSFORM)."
  (setf (tuatara.buffer:buffer-local buffer :plaintext) t)
  (pushnew buffer *encrypted-buffers*)
  (set-dumpable nil))

(defun note-encrypted (buffer encryption)
  "Mark BUFFER as holding an encrypted file's text, ENCRYPTION a plist of
:KEYS and :SYMMETRIC, kept from the disk (KEEP-FROM-DISK)."
  (setf (tuatara.buffer:buffer-local buffer :encrypted) encryption)
  (keep-from-disk buffer))

(defun encrypted-killed (buffer)
  "Kill-buffer function: forget BUFFER among the encrypted ones; core dumps allowed again once none is open."
  (when (member buffer *encrypted-buffers*)
    (setf *encrypted-buffers* (remove buffer *encrypted-buffers*))
    (unless (some #'live-buffer-p *encrypted-buffers*)
      (set-dumpable t))))

;;; Opening

(defvar *encrypted-waiting-keymap* (make-keymap :name "Encrypted file waiting for its passphrase")
  "Keys of an encrypted file's buffer waiting for its passphrase: \\`RET' asks for it.")

(defun encrypted-placeholder (buffer text)
  "Show TEXT, what is happening to it, in BUFFER, an encrypted file's buffer, read only."
  (tuatara.buffer:widen-buffer buffer)
  (tuatara.buffer:with-read-only-lifted (buffer :after t)
    (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) text))
  (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '())
  (tuatara.buffer:buffer-mark-saved buffer))

(defstruct (decrypt-job (:include tuatara.runtime:job) (:constructor make-decrypt-job (buffer path passphrase)) (:copier nil))
  "The encrypted file at PATH being decrypted for BUFFER, a job: PASSPHRASE,
the one typed, if any; TEXT, the octets decrypted; INFO, what GPG-DECRYPT says
of how; STAMP, the file's as read."
  path passphrase text info stamp)

(setf (documentation 'decrypt-job-p 'function) "True when OBJECT is a DECRYPT-JOB.")

(tuatara.runtime:define-job-words decrypt-job
  :doing "Decrypting"
  :description (format nil "Decrypting ~a with gpg" (tuatara.io:native-file-name (decrypt-job-path job)))
  :verb "decrypt it"
  :left-words "nothing decrypted")

(defmethod tuatara.runtime:job-take-in ((job decrypt-job))
  "Show what the decryption gave (DECRYPTED-IN)."
  (decrypted-in (tuatara.runtime:job-buffer job) (decrypt-job-path job) (decrypt-job-text job) (decrypt-job-info job)
                (decrypt-job-stamp job)
                (and (not (eq (tuatara.runtime:job-status job) :done)) (tuatara.runtime:job-ending-words job))))

(defun decrypted-in (buffer path text info stamp &optional ending)
  "Show TEXT, the file at PATH decrypted (its STAMP as read), in BUFFER, in the
mode its name without .gpg calls for; or, with no TEXT, say why it could not
be decrypted (INFO's :ERROR, else ENDING), and how to give the passphrase
when it wants one. TEXT is wiped once in BUFFER, and at once when BUFFER is
gone."
  (let ((name (tuatara.io:native-file-name path)))
    (if (not (live-buffer-p buffer))
        (when text (fill text 0))
        (cond
          (text
           (setf (tuatara.buffer:buffer-local buffer :read-only) nil)
           (tuatara.buffer:widen-buffer buffer)
           (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) text)
           (fill text 0)
           (setf (tuatara.buffer:buffer-undo-stack buffer) '() (tuatara.buffer:buffer-redo-stack buffer) '()
                 (tuatara.buffer:buffer-point buffer) 0
                 (tuatara.buffer:buffer-local buffer :file-stamp) stamp
                 (tuatara.buffer:buffer-local buffer :disk-state) nil
                 (tuatara.buffer:buffer-local buffer :disk-state-said) nil
                 (tuatara.buffer:buffer-local buffer :line-ending) (tuatara.buffer:detect-line-ending (tuatara.buffer:buffer-text buffer))
                 (tuatara.buffer:buffer-local buffer :encrypted) (list :keys (getf info :keys) :symmetric (getf info :symmetric))
                 (tuatara.buffer:buffer-local buffer :override-keymap) nil)
           (tuatara.buffer:buffer-mark-saved buffer)
           (set-major-mode buffer (tuatara.modes:mode-for-file path))
           (unless (tuatara.io:writable-p path)
             (setf (tuatara.buffer:buffer-local buffer :read-only) t))
           (message "~a decrypted; a save encrypts it ~{~a~^ and ~} (~a chooses again)"
                    name (remove nil (list (and (getf info :keys) (format nil "to ~{~a~^, ~}" (getf info :key-words)))
                                           (and (getf info :symmetric) "with a passphrase")))
                    (substitute-command-keys "\\[choose-encryption-keys]")))
          ((getf info :needs-passphrase)
           (encrypted-placeholder buffer (format nil "~a needs its passphrase: ~a asks for it.~%" name (substitute-command-keys "\\[decrypt-with-passphrase]")))
           (setf (tuatara.buffer:buffer-local buffer :override-keymap) *encrypted-waiting-keymap*)
           (message "~a needs its passphrase: RET asks for it" name))
          (t
           (encrypted-placeholder buffer (format nil "~a could not be decrypted: ~a~%"
                                         name (or (getf info :error) ending "gpg failed"))))))))

(defun start-decrypting (buffer path &optional passphrase)
  "Decrypt the file at PATH into BUFFER in the background, with PASSPHRASE when
given (wiped once handed to gpg); at once, the terminal lent to gpg-agent's
pinentry, when it asks there (WITH-GPG-TERMINAL)."
  (encrypted-placeholder buffer (format nil "Decrypting ~a with gpg...~%" (tuatara.io:native-file-name path)))
  (if (eq (gpg-asker) :terminal)
      (tuatara.io:call-with-file
       path
       (lambda (fd native size)
         (let ((octets (tuatara.io:read-fd-octets fd :path native :size-hint (1+ size)))
               (stamp (tuatara.io:fd-stamp fd native)))
           (multiple-value-bind (text info)
               (with-gpg-terminal (format nil "decrypting ~a" (tuatara.io:native-file-name native))
                                  (lambda () (decrypt-naming-keys octets)))
             (fill octets 0)
             (decrypted-in buffer native text info stamp)))))
      (start-decrypting-job buffer path passphrase)))

(defun start-decrypting-job (buffer path passphrase)
  "Decrypt the file at PATH into BUFFER on a job, with PASSPHRASE when given (wiped once handed to gpg)."
  (tuatara.runtime:start-job-thread
   (make-decrypt-job buffer path passphrase)
   (lambda (job)
     (tuatara.io:call-with-file
      (decrypt-job-path job)
      (lambda (fd native size)
        (let ((octets (tuatara.io:read-fd-octets fd :path native :size-hint (1+ size))))
          (setf (decrypt-job-stamp job) (tuatara.io:fd-stamp fd native))
          (multiple-value-bind (text info)
              (unwind-protect (decrypt-naming-keys octets :passphrase (decrypt-job-passphrase job) :cancel (tuatara.runtime:job-cancel job))
                (wipe-string (decrypt-job-passphrase job)))
            (setf (decrypt-job-text job) text (decrypt-job-info job) info))))))
   :name "tuatara decrypt"))

(define-command decrypt-with-passphrase ()
  "Ask for the passphrase of the encrypted file this buffer waits for, shown
as stars, and decrypt it with it."
  (let* ((buffer (current-buffer))
         (path (tuatara.buffer:buffer-file buffer)))
    (if (not (and path (encrypted-buffer-p buffer)))
        (message "This buffer waits for no passphrase")
        (start-decrypting buffer (tuatara.io:native path)
                          (gpg-read-passphrase (format nil "Passphrase for ~a: " (tuatara.io:native-file-name path)))))))

(defun encrypted-visit (path show)
  "Show the encrypted file at PATH (a name ending in .gpg), decrypted in the
background into a buffer of its own, with SHOW, a function of the buffer;
NIL for any other file. With the minibuffer asking for passphrases, one
encrypted with a passphrase asks for it first."
  (let ((native (file-view-path path)))
    (when (and native (uiop:string-suffix-p (string-downcase native) ".gpg"))
      (file-view-show native *encrypted-buffers* (lambda (b) (and (live-buffer-p b) (tuatara.buffer:buffer-file b)
                                                                    (tuatara.io:native (tuatara.buffer:buffer-file b))))
                      (lambda ()
                        (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name (tuatara.io:native-file-name native)))))
                          (setf (tuatara.buffer:buffer-file buffer) native
                                (tuatara.buffer:buffer-local buffer :file-stamp) (tuatara.io:path-stamp native)
                                (tuatara.buffer:buffer-local buffer :reread) (lambda (b) (start-decrypting b (tuatara.io:native (tuatara.buffer:buffer-file b)))))
                          (note-encrypted buffer (list :keys '() :symmetric nil))
                          (start-decrypting buffer native
                                            (and (gpg-minibuffer-p)
                                                 (openpgp-symmetric-p native)
                                                 (gpg-read-passphrase (format nil "Passphrase for ~a: " (tuatara.io:native-file-name native)))))
                          buffer))
                      show))))

;;; Saving

(defun gpg-encrypt-arguments (encryption)
  "gpg's arguments encrypting to ENCRYPTION's :KEYS and with a passphrase when :SYMMETRIC."
  (append (and (getf encryption :symmetric) (list "--symmetric"))
          (and (getf encryption :keys)
               (cons "--encrypt" (loop for key in (getf encryption :keys) append (list "--recipient" key))))
          (list "--output" "-")))

(defun encrypted-save-transform (buffer path)
  "The transform of BUFFER's text as its save as PATH has it (*SAVE-TRANSFORM-FUNCTION*):
for an encrypted buffer, or one saved under a name ending in .gpg, its text
encrypted by gpg to the keys and passphrase it has; with none yet, the
question how to encrypt it asked first (READ-ENCRYPTION), the save refused
when nothing is chosen. A buffer holding text decrypted in place in a file
that is not encrypted (\\[decrypt-region]) is saved as it is, the decrypted
text in clear, only when the question asked first is answered yes; else the
save is refused."
  (when (and (tuatara.buffer:buffer-local buffer :plaintext)
             (not (encrypted-buffer-p buffer))
             (not (uiop:string-suffix-p (string-downcase (tuatara.io:native path)) ".gpg"))
             (not (yes-or-no-p* (format nil "~a holds text decrypted in place; save it unencrypted?"
                                        (tuatara.buffer:buffer-name buffer)))))
    (error "~a is not saved: it holds text decrypted in place" (tuatara.buffer:buffer-name buffer)))
  (when (or (encrypted-buffer-p buffer) (uiop:string-suffix-p (string-downcase (tuatara.io:native path)) ".gpg"))
    (let ((encryption (tuatara.buffer:buffer-local buffer :encrypted)))
      (unless (or (getf encryption :keys) (getf encryption :symmetric))
        (setf encryption (or (read-encryption (tuatara.buffer:buffer-name buffer))
                             (error "~a is not saved: nothing chosen to encrypt it to" (tuatara.buffer:buffer-name buffer)))))
      (note-encrypted buffer encryption)
      (let ((passphrase (and (getf encryption :symmetric) (gpg-minibuffer-p)
                             (gpg-read-passphrase (format nil "Passphrase to encrypt ~a with: " (tuatara.buffer:buffer-name buffer)) :confirm t)))
            (name (tuatara.buffer:buffer-name buffer)))
        (when (eq (gpg-asker) :terminal)
          ;; encrypted now, the terminal lent to the pinentry: the save writes what it gave
          (let* ((plain (tuatara.text:rope-octets (tuatara.text:version-rope (tuatara.buffer:buffer-version buffer)))))
            (multiple-value-bind (output statuses ok said)
                (unwind-protect (with-gpg-terminal (format nil "encrypting ~a" name)
                                                   (lambda () (gpg-run (gpg-encrypt-arguments encryption) :input plain)))
                  (fill plain 0))
              (unless (and ok output (plusp (length output)))
                (error "gpg could not encrypt ~a: ~a" name (gpg-failure-words statuses said)))
              (return-from encrypted-save-transform (lambda (rope) (declare (ignore rope)) output)))))
        (lambda (rope)
          (let ((plain (tuatara.text:rope-octets rope)))
            (multiple-value-bind (output statuses ok said)
                (unwind-protect (gpg-run (gpg-encrypt-arguments encryption) :input plain :passphrase passphrase)
                  (fill plain 0)
                  (wipe-string passphrase))
              (unless (and ok output (plusp (length output)))
                (error "gpg could not encrypt ~a: ~a" name (gpg-failure-words statuses said)))
              output)))))))

;;; Choosing keys

(defun gpg-keys (&key all)
  "The keys gpg has that can encrypt, each a plist of :ID, :NAME, :VALIDITY (words),
:EXPIRES (a universal time or NIL), :SECRET (true for one of yours) and
:SUBKEYS (the IDs of its subkeys, which a file names it by), :FINGERPRINT and
:CAPABILITIES (gpg's letters: e encrypts, s signs, c certifies); with ALL, the
keys that cannot encrypt too."
  (flet ((lines (arguments) (uiop:split-string (tuatara.text:lenient-string (or (gpg-run arguments :asker :agent) #())) :separator '(#\Newline)))
         (fields (line) (uiop:split-string line :separator '(#\:))))
    (let ((secret (loop for line in (lines (list "--list-secret-keys" "--with-colons"))
                        for f = (fields line)
                        when (string= (first f) "sec") collect (fifth f)))
          (keys '()) (current nil))
      (dolist (line (lines (list "--list-keys" "--with-colons")))
        (let ((f (fields line)))
          (cond ((string= (first f) "pub")
                 (setf current (and (>= (length f) 12) (or all (find #\E (nth 11 f)))
                                    (list :id (fifth f) :name nil :capabilities (remove-duplicates (string-downcase (nth 11 f)))
                                          :validity (case (char (concatenate 'string (second f) "-") 0)
                                                      (#\u "ultimate") (#\f "full") (#\m "marginal") (#\n "never")
                                                      (#\e "expired") (#\r "revoked") (t "unknown"))
                                          :expires (let ((n (parse-integer (seventh f) :junk-allowed t)))
                                                     (and n (+ n +unix-epoch+)))
                                          :secret (and (member (fifth f) secret :test #'string=) t))))
                 (when current (push current keys)))
                ((and current (string= (first f) "fpr") (null (getf current :fingerprint)))
                 (setf (getf current :fingerprint) (tenth f))
                 (setf (first keys) current))
                ((and current (string= (first f) "sub"))
                 (push (fifth f) (getf current :subkeys))
                 (setf (first keys) current))
                ((and current (string= (first f) "uid") (null (getf current :name)))
                 (setf (getf current :name) (tenth f))
                 (setf (first keys) current)))))
      (nreverse keys))))

(defun key-owners (ids)
  "IDS, keys' or subkeys' IDs a file is encrypted to, each with its owner's
name as your keyring has it, as (ID . NAME); those you do not have left out."
  (let ((keys (ignore-errors (gpg-keys))))
    (loop for id in ids
          for key = (find-if (lambda (k) (member id (cons (getf k :id) (getf k :subkeys)) :test #'string-equal)) keys)
          when (and key (getf key :name)) collect (cons id (getf key :name)))))

(defun encryption-key-words (ids)
  "IDS, keys' or subkeys' IDs a file is encrypted to, as words: each its
owner's name as your keyring has it, else the ID (a key you do not have)."
  (let ((owners (key-owners ids)))
    (mapcar (lambda (id) (or (cdr (assoc id owners :test #'string-equal)) (format nil "~a (a key you do not have)" id)))
            ids)))

(defun encryption-choice (entry)
  "ENTRY, :PASSPHRASE or one of GPG-KEYS, as the encryption question offers
it: a passphrase, or the key's owner and ID (a comma in a name left out, the
choices being separated by commas), with what more to say of it, as a
cons."
  (if (eq entry :passphrase)
      (cons "a passphrase" "asked for as it is saved (symmetric)")
      (cons (format nil "~@[~a ~]~a" (and (getf entry :name) (substitute #\Space #\, (getf entry :name))) (getf entry :id))
            (format nil "~a~@[, expires ~a~]~:[~;, yours~]" (getf entry :validity)
                    (let ((e (getf entry :expires))) (and e (format-time "%F" e))) (getf entry :secret)))))

(defun read-encryption (what &optional encryption)
  "Ask in the minibuffer how WHAT (words: a file's name) is encrypted: to keys,
with a passphrase, or both, several at once with commas between them
\(READ-SEVERAL), those of ENCRYPTION (a plist of :KEYS and :SYMMETRIC) there
already. Returns the encryption chosen, a plist as ENCRYPTION is, or NIL
when nothing is chosen."
  (let* ((entries (cons :passphrase (gpg-keys)))
         (choices (mapcar (lambda (entry) (cons (car (encryption-choice entry)) entry)) entries))
         (initial (loop for entry in entries
                        when (if (eq entry :passphrase)
                                 (getf encryption :symmetric)
                                 (some (lambda (id) (member id (getf encryption :keys) :test #'string-equal))
                                       (cons (getf entry :id) (getf entry :subkeys))))
                          collect (car (encryption-choice entry))))
         (chosen (loop for text in (read-several (format nil "Encrypt ~a to (keys, a passphrase; commas between): " what)
                                                 (lambda () (mapcar #'encryption-choice entries))
                                                 :initial (format nil "~{~a, ~}" initial)
                                                 :none "nothing: no encryption" :more "these, no more")
                       for entry = (cdr (assoc text choices :test #'string=))
                       when entry collect entry)))
    (and chosen
         (list :keys (loop for e in chosen unless (eq e :passphrase) collect (getf e :id))
               :symmetric (and (member :passphrase chosen) t)))))

(defun choose-encryption (what encryption then)
  "Ask how WHAT is encrypted (READ-ENCRYPTION), those of ENCRYPTION offered,
and call THEN with the encryption chosen; with nothing chosen, say so and do
nothing."
  (let ((chosen (read-encryption what encryption)))
    (if chosen
        (funcall then chosen)
        (message "Nothing chosen: nothing done"))))

(define-command choose-encryption-keys ()
  "Choose again whom the encrypted file shown is encrypted to, or a
passphrase, in the minibuffer: from its next save it is encrypted so."
  (let ((buffer (current-buffer)))
    (if (encrypted-buffer-p buffer)
        (choose-encryption (tuatara.buffer:buffer-name buffer) (tuatara.buffer:buffer-local buffer :encrypted)
                           (lambda (encryption)
                             (note-encrypted buffer encryption)
                             (message "~a is encrypted so from its next save" (tuatara.buffer:buffer-name buffer))))
        (message "~a is no encrypted file" (tuatara.buffer:buffer-name buffer)))))

;;; The rest of the editor

(define-command copy-to-clipboard ()
  "Put the text copied last on the system clipboard, even text of an encrypted
file, which is otherwise kept within Tuatara: other programs (clipboard
managers among them) can then read it, and keep it."
  (let ((kill (first (editor-kill-ring *editor*))))
    (if (and (stringp kill) (tuatara.frontend:frontend-set-clipboard (editor-frontend *editor*) kill))
        (progn (setf *clipboard-sent* kill) (message "On the system clipboard"))
        (message "Nothing to put on the system clipboard"))))

(defun encrypted-health-words ()
  "What decrypts .gpg files here, as M-x check-health says it."
  (let ((program (gpg-path)))
    (if program
        (format nil "Encrypted files (.gpg) by ~a, passphrases asked by ~a" program
                (ecase (gpg-asker)
                  (:agent "gpg-agent's pinentry, in its window")
                  (:terminal "gpg-agent's pinentry, in the terminal lent it")
                  (:minibuffer "the minibuffer (gpg's loopback: the passphrase passes through the editor)")))
        (format nil "Encrypted files (.gpg) by none: ~a" (outside-program-install-words :gpg)))))

(defun encrypted-new-file (buffer)
  "Find-file function: a new file whose name ends in .gpg is encrypted from the
start, its text kept from the disk before its first save encrypts it."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (when (and file (uiop:string-suffix-p (string-downcase (tuatara.io:native file)) ".gpg")
               (null (tuatara.io:path-stamp (tuatara.io:native file))))
      (note-encrypted buffer (list :keys '() :symmetric nil)))))

(defun install-encrypted ()
  "Have files ending in .gpg opened decrypted and saved encrypted, their text kept from the disk."
  (pushnew 'encrypted-visit *file-views*)
  (setf tuatara.io:*save-transform-function* 'encrypted-save-transform)
  (pushnew 'encrypted-killed tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'encrypted-new-file tuatara.buffer:*find-file-functions*)
  (define-key *encrypted-waiting-keymap* "RET" 'decrypt-with-passphrase))
