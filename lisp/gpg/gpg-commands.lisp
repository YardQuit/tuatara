;;;; lisp/gpg/gpg-commands.lisp --- GnuPG on files, regions and keys: encrypt, decrypt, sign, verify, and the keys listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands Emacs's EasyPG has, under plain names: a file encrypted
;;;; (FILE.gpg), decrypted (asked where, the text then on disk as it is),
;;;; signed (a detached signature FILE.sig, clear-signed text FILE.asc, or a
;;;; signed file FILE.gpg) or its signature verified, saying who signed it,
;;;; when, and how far that key is trusted; the same on the region, the
;;;; armoured blocks in it decrypted or verified in place; and the keys
;;;; listed (M-x list-keys), their details shown, imported from a file or the
;;;; region, exported, inserted, deleted. gpg reads and writes through pipes
;;;; as for encrypted files (encrypted.lisp): a file is read by Tuatara
;;;; without changing its access time, and what gpg gives back is written by
;;;; Tuatara, a decrypted file for you alone. Work on a file runs in the
;;;; background, or at once with the terminal lent to gpg-agent's pinentry
;;;; when it asks there.

(in-package #:tuatara.defaults)

;;; Running gpg on work that takes time

(defstruct (gpg-job (:include tuatara.runtime:job) (:constructor make-gpg-job (buffer words then quiet)) (:copier nil))
  "gpg at work on WORDS (decrypting report.pdf.gpg), a job; THEN, called on the
editor's thread with the RESULTS of its work, a list of values (what gpg gave:
its output, status lines, whether it ended well and what it said; or the keys
listed). QUIET, true for work that only shows something (the keys listed),
stopped without asking when its buffer is killed."
  words then quiet results)

(setf (documentation 'gpg-job-p 'function) "True when OBJECT is a GPG-JOB.")

(tuatara.runtime:define-job-words gpg-job
  :doing (gpg-job-words job)
  :description (format nil "gpg: ~a" (gpg-job-words job))
  :verb (gpg-job-words job)
  :left-words "nothing done")

(defmethod tuatara.runtime:job-at-exit ((job gpg-job))
  "Stop work that only shows something without asking; ask before stopping any other."
  (if (gpg-job-quiet job) :quiet :stop))

(defmethod tuatara.runtime:job-take-in ((job gpg-job))
  "Hand the results of the job's work to its THEN."
  (if (eq (tuatara.runtime:job-status job) :done)
      (apply (gpg-job-then job) (gpg-job-results job))
      (message "gpg: ~a: ~a" (gpg-job-words job) (tuatara.runtime:job-ending-words job))))

(defun gpg-in-background (words work then &key (asker (gpg-asker)) (buffer (current-buffer)) quiet)
  "Call WORK, a function of a cancel cell running gpg, on WORDS's behalf (words
for the job: decrypting FILE), and THEN on the editor's thread with the values
it returns: in the background, a job of BUFFER's (QUIET as GPG-JOB has it), or
at once with the terminal lent to gpg-agent's pinentry when ASKER is
:TERMINAL (GPG-ASKER)."
  (if (eq asker :terminal)
      (multiple-value-call then (with-gpg-terminal words (lambda () (funcall work nil))))
      (tuatara.runtime:start-job-thread
       (make-gpg-job buffer words then quiet)
       (lambda (job)
         (setf (gpg-job-results job) (multiple-value-list (funcall work (tuatara.runtime:job-cancel job)))))
       :name "tuatara gpg")))

(defun run-gpg-work (words arguments then &key input-path input passphrase second-path (asker (gpg-asker)) (buffer (current-buffer)) quiet)
  "Run gpg with ARGUMENTS on WORDS's behalf (words for the job, decrypting
FILE), its input the file at INPUT-PATH or the octets INPUT, a detached
signature at SECOND-PATH given as its second input; THEN called with what it
gave (its output, status lines, whether it ended well and what it said). In
the background, or at once with the terminal lent to gpg-agent's pinentry
when ASKER (GPG-ASKER) says it asks there (GPG-IN-BACKGROUND, BUFFER and QUIET
as it takes them). PASSPHRASE, for the minibuffer's loopback, is wiped once
gpg has it."
  (gpg-in-background words
                     (lambda (cancel)
                       ;; the files read by gpg's pipes a block at a time, never whole in memory
                       (unwind-protect (gpg-run arguments :input input :input-path (and (null input) input-path)
                                                          :passphrase passphrase :cancel cancel
                                                          :second-path second-path :asker asker)
                         (wipe-string passphrase)))
                     then :asker asker :buffer buffer :quiet quiet))

(defun gpg-own-passphrase (what)
  "With the minibuffer asking for passphrases (GPG-MINIBUFFER-P), the passphrase
of your key for WHAT, asked for: NIL for RET alone (gpg-agent has it). NIL
when gpg-agent's pinentry asks."
  (when (gpg-minibuffer-p)
    (let ((typed (gpg-read-passphrase (format nil "Passphrase of your key for ~a (RET: gpg-agent has it): " what))))
      (if (zerop (length typed)) nil typed))))

(defun write-gpg-output (path octets &key private)
  "Write OCTETS, what gpg gave, as the file at PATH (never through a link),
for you alone with PRIVATE; and show it in the lists of its folder."
  (tuatara.io:write-text-file (tuatara.text:octets->rope (coerce octets 'tuatara.text:octets)) path
                              :expected :any :follow-links nil :mode (and private (constantly #o600)))
  (paths-changed (list path)))

(defun gpg-target-free-p (path)
  "True when nothing is at PATH, or you say to write over what is."
  (or (null (tuatara.io:path-stamp path :follow nil))
      (yes-or-no-p* (format nil "~a exists; write over it?" (abbreviated-path path)))))

(defun gpg-stripped-name (path endings)
  "PATH without the first of ENDINGS it ends in (.gpg), or NIL."
  (let ((ending (find-if (lambda (e) (uiop:string-suffix-p (string-downcase path) e)) endings)))
    (and ending (subseq path 0 (- (length path) (length ending))))))

(defun decrypted-name (path)
  "The name a decrypted copy of the file at PATH is given: PATH without its
.gpg, .pgp or .asc, else PATH with .decrypted after it."
  (or (gpg-stripped-name path '(".gpg" ".pgp" ".asc")) (concatenate 'string path ".decrypted")))

;;; Files

(defun gpg-file-to (path target verb arguments said &key status private passphrase then)
  "Run gpg with ARGUMENTS to VERB (encrypt, decrypt, sign) the file at PATH,
PASSPHRASE, for the minibuffer's loopback, wiped once gpg has it (RUN-GPG-WORK).
When gpg ends well, and has said STATUS when given (else given back anything),
write what it gave as the file TARGET, for you alone and then wiped from
memory when PRIVATE; say SAID (words, or a function of no arguments making
them) and call THEN, a function of no arguments, when given. Else say that the
file was not VERBed, and why."
  (let ((name (tuatara.io:native-file-name path)))
    (run-gpg-work (format nil "~aing ~a" verb name) arguments
                  (lambda (output statuses ok words)
                    (if (and ok (if status (gpg-status-p statuses status) (plusp (length output))))
                        (progn (write-gpg-output target output :private private)
                               (when private (fill output 0))
                               (message "~a" (if (functionp said) (funcall said) said))
                               (when then (funcall then)))
                        (message "~a was not ~aed: ~a" name verb (gpg-failure-words statuses words))))
                  :input-path path :passphrase passphrase)))

(defun encrypt-file-to (path encryption &key then (passphrase nil passphrase-p))
  "Encrypt the file at PATH as ENCRYPTION says (:KEYS, :SYMMETRIC) into PATH.gpg,
asked first when that exists; THEN, a function of no arguments, called once it
is written. PASSPHRASE, for the minibuffer's loopback, is asked for when not
given, and wiped once gpg has it."
  (let ((target (concatenate 'string path ".gpg")))
    (when (gpg-target-free-p target)
      (gpg-file-to path target "encrypt" (gpg-encrypt-arguments encryption)
                   ;; the keys' names looked up once it is done
                   (lambda () (format nil "Encrypted as ~a~@[ to ~{~a~^, ~}~]~:[~; with a passphrase~]" (abbreviated-path target)
                                      (encryption-key-words (getf encryption :keys)) (getf encryption :symmetric)))
                   :then then
                   :passphrase (if passphrase-p
                                   passphrase
                                   (and (getf encryption :symmetric) (gpg-minibuffer-p)
                                        (gpg-read-passphrase (format nil "Passphrase to encrypt ~a with: " (tuatara.io:native-file-name path)) :confirm t)))))))

(define-command encrypt-file ()
  "Encrypt the file shown (in a list of a directory, the one on the line; with
\\[universal-argument], or where there is none, asked for) into FILE.gpg, to the keys
or with a passphrase chosen in the minibuffer, commas between several. Once
it is done, wiping the file itself is offered (\\[wipe-file]); with a
passphrase, only after the passphrase, asked again, opens what was made."
  (let ((path (shown-file-argument "Encrypt file: ")))
    (choose-encryption (tuatara.io:native-file-name path) '()
                       (lambda (encryption)
                         (encrypt-file-to path encryption
                                          :then (lambda () (offer-wipe-original path (concatenate 'string path ".gpg") encryption)))))))

(defun decrypt-file-to (path target &key then)
  "Decrypt the file at PATH into the file TARGET, for you alone; THEN, a function of no arguments, called once it is written."
  (gpg-file-to path target "decrypt" (list "--decrypt" "--output" "-")
               (format nil "Decrypted as ~a, for you alone" (abbreviated-path target))
               :status "DECRYPTION_OKAY" :private t :then then
               :passphrase (and (gpg-minibuffer-p) (openpgp-symmetric-p path)
                                (gpg-read-passphrase (format nil "Passphrase for ~a: " (tuatara.io:native-file-name path))))))

(defun gpgconf-path ()
  "gpgconf, GnuPG's program that runs its parts: the one beside gpg (*GPG-PROGRAM*), else found; NIL when there is none."
  (let ((gpg (gpg-path)))
    (or (and gpg (tuatara.io:find-program (tuatara.io:path-join (tuatara.io:parent-path gpg) "gpgconf")))
        (tuatara.io:find-program "gpgconf"))))

(define-command gpg-lock ()
  "Make gpg-agent forget every passphrase it keeps for a while (ten minutes,
as it comes, its default-cache-ttl), of encrypted files and of your keys: the
next to open one asks again. Text already open stays open: kill those
buffers too to lock it away."
  (let ((gpgconf (or (gpgconf-path) (tuatara.io:program-missing "gpgconf" :why (outside-program-install-words :gpg)))))
    ;; reloading gpg-agent flushes its cache; one not running keeps nothing
    (if (nth-value 1 (tuatara.io:program-output (list gpgconf "--reload" "gpg-agent") :capture nil :seconds 5))
        (message "gpg-agent has forgotten the passphrases it kept")
        (message "gpg-agent could not be told to forget its passphrases: gpgconf failed or took over 5 seconds"))))

(define-command decrypt-file ()
  "Decrypt the file shown (in a list of a directory, the one on the line) into
a file asked for, the name without .gpg offered; after asking, since its text
is then on disk where it can be read (made for you alone). To read it, visit
the encrypted file instead: its text stays in memory."
  (let* ((path (shown-file-argument "Decrypt file: "))
         (offered (decrypted-name path))
         (target (read-native-path "Decrypt to: " :initial (abbreviated-path offered) :kind :any :writing t)))
    (when (and target (gpg-target-free-p target)
               (yes-or-no-p* (format nil "This writes the text of ~a to disk, where it can be read; go on?" (tuatara.io:native-file-name path))))
      (decrypt-file-to path target))))

(defparameter *signature-kinds*
  '(("detached signature (FILE.sig)" ".sig" "--detach-sign")
    ("clear-signed text (FILE.asc)" ".asc" "--clearsign")
    ("signed file (FILE.gpg)" ".gpg" "--sign"))
  "The kinds of signature sign-file makes: the words asked with, the ending of the file made, gpg's option.")

(defun read-signature-kind ()
  "The kind of signature to make, asked for: an entry of *SIGNATURE-KINDS*."
  (assoc (completing-read "Signature: " (mapcar #'first *signature-kinds*) :require-match t :default (first (first *signature-kinds*)))
         *signature-kinds* :test #'string=))

(defun sign-file-as (path kind passphrase)
  "Sign the file at PATH as KIND (an entry of *SIGNATURE-KINDS*) says, into
PATH and the kind's ending, asked first when that exists; PASSPHRASE, for the
minibuffer's loopback, wiped once gpg has it."
  (let ((target (concatenate 'string path (second kind))))
    (if (not (gpg-target-free-p target))
        (wipe-string passphrase)
        (gpg-file-to path target "sign" (list (third kind) "--output" "-") (format nil "Signed: ~a" (abbreviated-path target))
                     :status "SIG_CREATED" :passphrase passphrase))))

(define-command sign-file ()
  "Sign the file shown (in a list of a directory, the one on the line) with your
key: a detached signature (FILE.sig, the file left as it is), clear-signed text
(FILE.asc) or a signed file (FILE.gpg), asked which."
  (let ((path (shown-file-argument "Sign file: ")))
    (sign-file-as path (read-signature-kind) (gpg-own-passphrase (format nil "signing ~a" (tuatara.io:native-file-name path))))))

(defun signature-words (statuses said)
  "What gpg's STATUSES of a verification say, in words: who signed, when, how
far the key is trusted, or why the signature does not hold."
  (flet ((field (name n) (let ((line (gpg-status-p statuses name)))
                           (and line (let ((words (uiop:split-string line :separator '(#\Space)))) (format nil "~{~a~^ ~}" (nthcdr n words)))))))
    (let ((valid (gpg-status-p statuses "VALIDSIG"))
          (trust (cond ((gpg-status-p statuses "TRUST_ULTIMATE") "a key you trust ultimately (yours)")
                       ((gpg-status-p statuses "TRUST_FULLY") "a key trusted fully")
                       ((gpg-status-p statuses "TRUST_MARGINAL") "a key trusted marginally")
                       ((gpg-status-p statuses "TRUST_NEVER") "a key you never trust")
                       (t "a key whose owner nothing confirms (not certified)"))))
      (cond ((gpg-status-p statuses "GOODSIG")
             (format nil "Good signature by ~a (key ~a)~@[, made ~a~], by ~a"
                     (field "GOODSIG" 2) (second (uiop:split-string (gpg-status-p statuses "GOODSIG") :separator '(#\Space)))
                     (and valid (let ((n (ignore-errors (parse-integer (third (uiop:split-string valid :separator '(#\Space)))))))
                                  (and n (time-words (+ n +unix-epoch+))))) trust))
            ((gpg-status-p statuses "BADSIG")
             (format nil "BAD signature by ~a: the file was changed after it was signed, or the signature is not for it" (field "BADSIG" 2)))
            ((gpg-status-p statuses "EXPKEYSIG") (format nil "Signature by ~a, whose key has expired" (field "EXPKEYSIG" 2)))
            ((gpg-status-p statuses "REVKEYSIG") (format nil "Signature by ~a, whose key is revoked" (field "REVKEYSIG" 2)))
            ((gpg-status-p statuses "NO_PUBKEY")
             (format nil "Signed by key ~a, which you do not have: import it (M-x import-keys) to check the signature" (field "NO_PUBKEY" 1)))
            (t (format nil "No signature checked: ~a" (gpg-failure-words statuses said)))))))

(define-command verify-file ()
  "Check the signature of the file shown (in a list of a directory, the one on
the line): a detached signature (FILE.sig, FILE.asc) against its file, else
the signature within the file; say who signed it, when, and how far that key
is trusted. Neither file's access time changes."
  (verify-file-at (shown-file-argument "Verify file: ")))

(defun verify-file-at (path)
  "Check the signature of the file at PATH, as \\[verify-file] does."
  (let* ((data (gpg-stripped-name path '(".sig" ".asc")))
         (detached (and data (tuatara.io:path-stamp data)
                        (not (search "-----BEGIN PGP SIGNED MESSAGE-----"
                                     (tuatara.text:lenient-string (or (ignore-errors (tuatara.io:file-head path 200)) #())))))))
    (run-gpg-work (format nil "verifying ~a" (tuatara.io:native-file-name path))
                  (if detached (list "--verify" "--" :second-input "-") (list "--verify" "--" "-"))
                  (lambda (output statuses ok said)
                    (declare (ignore output ok))
                    (message "~a: ~a" (tuatara.io:native-file-name path) (signature-words statuses said)))
                  :input-path (if detached data path) :second-path (and detached path))))

;;; The region

(defun region-or-error (buffer)
  "BUFFER's selection (SELECTED-REGION), as two values, its start and end; an
error when no text is selected."
  (values-list (or (selected-region buffer) (error "Select the text first"))))

(defun replace-region-when-unchanged (buffer start end version text words)
  "Replace BUFFER's text from START below END with TEXT (octets) as one edit, when
the buffer is still at VERSION, the selection ending with the text it held; else
say that WORDS was not put in, the text having changed."
  (if (and (live-buffer-p buffer) (eq (tuatara.buffer:buffer-version buffer) version))
      (progn (tuatara.buffer:buffer-replace buffer start end text)
             (drop-selection buffer)
             t)
      (progn (message "~a: the text changed meanwhile, nothing put in" words) nil)))

(define-command encrypt-region ()
  "Encrypt the text selected in place, as an armoured block (-----BEGIN PGP
MESSAGE-----), to the keys or with a passphrase chosen in the minibuffer."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (region-or-error buffer)
      (let ((version (tuatara.buffer:buffer-version buffer)))
        (choose-encryption "the selection" '()
                           (lambda (encryption)
                             (let* ((passphrase (and (getf encryption :symmetric) (gpg-minibuffer-p)
                                                     (gpg-read-passphrase "Passphrase to encrypt the selection with: " :confirm t)))
                                    (plain (tuatara.text:rope-octets (tuatara.text:version-rope version) :start start :end end)))
                               (run-gpg-work "encrypting the selection" (cons "--armor" (gpg-encrypt-arguments encryption))
                                             (lambda (output statuses ok said)
                                               (fill plain 0)
                                               (if ok
                                                   (when (replace-region-when-unchanged buffer start end version output "Encrypting")
                                                     (message "The selection is encrypted in place"))
                                                   (message "Not encrypted: ~a" (gpg-failure-words statuses said))))
                                             :input plain :passphrase passphrase))))))))

(defun armoured-blocks (text kind)
  "The armoured blocks of KIND (MESSAGE, SIGNED MESSAGE) in TEXT (octets), in
order, each (START . END), the line end after a block with it."
  (let ((opening (tuatara.text:string-octets (format nil "-----BEGIN PGP ~a-----" kind)))
        (closing (tuatara.text:string-octets (format nil "-----END PGP ~a-----"
                                                         (if (string= kind "SIGNED MESSAGE") "SIGNATURE" kind)))))
    (loop with from = 0
          for begin = (search opening text :start2 from)
          for stop = (and begin (search closing text :start2 begin))
          while stop
          collect (let ((end (+ stop (length closing))))
                    (when (and (< end (length text)) (= (aref text end) 10)) (incf end))
                    (setf from end)
                    (cons begin end)))))

(defun armoured-block (buffer kind)
  "The armoured block of KIND (MESSAGE, SIGNED MESSAGE) to work on in BUFFER,
as two values, where it starts and ends; NIL when there is none. With text
selected, the first in the selection; else the one point is in, or else the
one nearest to point, before or after it."
  (if (region-active-p buffer)
      (multiple-value-bind (start end) (tuatara.buffer:buffer-region buffer)
        (let ((found (first (armoured-blocks (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end) kind))))
          (and found (values (+ start (car found)) (+ start (cdr found))))))
      (let* ((point (tuatara.buffer:buffer-point buffer))
             (blocks (armoured-blocks (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer)) kind))
             (found (first (sort (copy-list blocks) #'<
                                 :key (lambda (b) (cond ((< point (car b)) (- (car b) point))
                                                        ((> point (cdr b)) (- point (cdr b)))
                                                        (t 0)))))))
        (and found (values (car found) (cdr found))))))

(define-command decrypt-region ()
  "Decrypt in place the armoured block (-----BEGIN PGP MESSAGE-----) in the
text selected, or without a selection the one point is in, else the one
nearest to point; the buffer's text is then kept from the disk as an
encrypted file's is (no auto-save, no clipboard but on request), and a save
asks before writing the decrypted text to the file unencrypted."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (armoured-block buffer "MESSAGE")
      (if (null start)
          (message "No encrypted block (-----BEGIN PGP MESSAGE-----) here")
          (let ((version (tuatara.buffer:buffer-version buffer))
                (cipher (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end)))
            (run-gpg-work "decrypting the block" (list "--decrypt" "--output" "-")
                          (lambda (output statuses ok said)
                            (if (and ok (gpg-status-p statuses "DECRYPTION_OKAY"))
                                (progn (keep-from-disk buffer)
                                       (when (replace-region-when-unchanged buffer start end version output "Decrypting")
                                         (message "Decrypted in place: not auto-saved, and a save asks before writing it unencrypted")))
                                (message "Not decrypted: ~a" (gpg-failure-words statuses said))))
                          :input cipher
                          :passphrase (and (gpg-minibuffer-p) (gpg-own-passphrase "the block"))))))))

(define-command sign-region ()
  "Clear-sign the text selected in place (-----BEGIN PGP SIGNED MESSAGE-----), with your key."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (region-or-error buffer)
      (let ((plain (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end))
            (version (tuatara.buffer:buffer-version buffer)))
        (run-gpg-work "signing the selection" (list "--clearsign" "--output" "-")
                      (lambda (output statuses ok said)
                        (if (and ok (gpg-status-p statuses "SIG_CREATED"))
                            (when (replace-region-when-unchanged buffer start end version output "Signing")
                              (message "The selection is signed in place"))
                            (message "Not signed: ~a" (gpg-failure-words statuses said))))
                      :input plain :passphrase (gpg-own-passphrase "signing the selection"))))))

(define-command verify-region ()
  "Check the signature of the clear-signed block (-----BEGIN PGP SIGNED
MESSAGE-----) in the text selected, or without a selection the one point is
in, else the one nearest to point: who signed it, when, and how far that key
is trusted."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (armoured-block buffer "SIGNED MESSAGE")
      (if (null start)
          (message "No signed block (-----BEGIN PGP SIGNED MESSAGE-----) here")
          (run-gpg-work "verifying the block" (list "--verify" "-")
                        (lambda (output statuses ok said)
                          (declare (ignore output ok))
                          (message "~a" (signature-words statuses said)))
                        :input (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end))))))

;;; The keys

(define-major-mode gpg-keys-mode marked-list-mode
  "Your GnuPG keys and those you have of others, one a line: validity, ID,
name, expiry, what it can do (e encrypt, s sign, c certify), yours.
\\<gpg-keys-mode>\\[list-mark-entry] marks the key at the cursor and moves down, \\[list-unmark-entry] unmarks it,
\\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps marked and unmarked; \\[gpg-keys-details] shows its details;
\\[import-keys] imports keys from a file, \\[gpg-keys-export] exports the keys marked (or the one at
the cursor) to a file, \\[gpg-keys-insert] puts them in the buffer you came from, \\[list-delete-marked]
deletes them, after asking; \\[revert-buffer] lists them again."
  :lighter "Keys" :interactive nil)

(defvar *gpg-keys-keymap* (make-keymap :name "The list of GnuPG keys, before the modal keys")
  "Keys of the list of GnuPG keys (\\[list-keys]), before the modal keys.")

(defparameter *gpg-keys-columns* '((" M" :width 2) ("ID" :width 16) ("Validity" :width 9) ("Can" :width 4)
                                   ("Expires" :width 10) ("Yours" :width 5) ("Name"))
  "The columns of the list of keys, as TABLE-HEADING-PIECES takes them: the
mark, the key's ID, how far it is trusted, what it can do (e encrypt, s sign,
c certify), when it expires, whether it is yours, and its name.")

(defun gpg-keys-show (buffer &optional (point :keep))
  "Fill BUFFER, the list of keys, as gpg has them now, listed in the
background, point on line POINT or kept."
  (gpg-in-background "listing keys" (lambda (cancel) (declare (ignore cancel)) (gpg-keys :all t))
                     (lambda (keys) (when (live-buffer-p buffer) (gpg-keys-draw buffer keys point)))
                     :asker :agent :buffer buffer :quiet t))

(defun gpg-keys-draw (buffer keys &optional (point :keep))
  "Make BUFFER, the list of keys, show KEYS (GPG-KEYS), point on line POINT or kept."
  (render-table-list
   buffer *gpg-keys-columns* keys
   (lambda (key mark section)
     (declare (ignore section))
     (list (list-mark-sign mark)
           (cons (getf key :id) (list-mark-face mark))
           (getf key :validity)
           (or (getf key :capabilities) "")
           (let ((e (getf key :expires))) (if e (format-time "%F" e) ""))
           (if (getf key :secret) "yours" "")
           (escape-controls (or (getf key :name) ""))))
   :marked-by (lambda (key section) (declare (ignore section)) (gpg-key-id key))
   :after (list (format nil "~%   ~[No keys~;1 key~:;~:*~d keys~], ~d yours~%" (length keys)
                        (count-if (lambda (key) (getf key :secret)) keys)))
   :point point
   :refill (lambda (b) (gpg-keys-show b :keep))
   :redraw (lambda (b) (gpg-keys-draw b keys :keep))))

(defun gpg-key-id (key)
  "The ID of KEY, one of GPG-KEYS: what a mark on it in the list of keys is kept by."
  (getf key :id))

(define-command list-keys ()
  "List your GnuPG keys and those you have of others: there \\<gpg-keys-mode>\\[list-mark-entry] marks one,
\\[gpg-keys-details] shows its details, \\[import-keys] imports keys from a file, \\[gpg-keys-export] exports, \\[gpg-keys-insert]
inserts and \\[list-delete-marked] deletes those marked."
  (let ((from (current-buffer))
        (buffer (buffer-in-mode "*GnuPG keys*" 'gpg-keys-mode)))
    (unless (eq from buffer) (setf (tuatara.buffer:buffer-local buffer 'gpg-keys-from) from))
    (setf (tuatara.buffer:buffer-local buffer :override-keymap) *gpg-keys-keymap*)
    (gpg-keys-show buffer 2)
    (show-buffer buffer)))

(defun gpg-keys-chosen ()
  "The keys the list's command works on: those marked, else the one at the cursor."
  (list-entries-of (current-buffer) (list-acted-on (current-buffer))))

(defun key-details-text (octets)
  "gpg's details of a key, OCTETS, as text to show, the control characters in
each line written as the editor shows them (ESCAPE-CONTROLS)."
  (format nil "~{~a~%~}" (mapcar #'escape-controls
                                 (uiop:split-string (string-right-trim '(#\Newline) (tuatara.text:lenient-string octets))
                                                    :separator '(#\Newline)))))

(define-command gpg-keys-details ()
  "Show the details of the key at the cursor: its fingerprint, its names, its subkeys and what they can do, when they expire."
  (let ((key (list-entry-at)))
    (if (null key)
        (message "No key on this line")
        (run-gpg-work (format nil "showing the details of ~a" (getf key :id))
                      (list "--list-keys" "--with-subkey-fingerprints" "--fingerprint" (getf key :fingerprint))
                      (lambda (output statuses ok said)
                        (declare (ignore statuses ok))
                        (if (plusp (length output))
                            (show-help (format nil "Key ~a~%~%~a" (getf key :id) (key-details-text output)))
                            (message "No details of ~a: ~a" (getf key :id) (if (plusp (length said)) said "gpg failed"))))
                      :asker :agent :quiet t))))

(define-command import-keys ()
  "Import the keys in a file asked for (armoured or not) into your keyring; say what was imported."
  (let ((path (read-native-path "Import keys from: " :kind :file)))
    (when path
      (run-gpg-work (format nil "importing keys from ~a" (tuatara.io:native-file-name path)) (list "--import")
                    (lambda (output statuses ok said)
                      (declare (ignore output ok))
                      (message "~a" (import-words statuses said))
                      (refill-open-list "*GnuPG keys*" 'gpg-keys-mode))
                    :input-path path))))

(define-command import-keys-region ()
  "Import the keys in the text selected (an armoured -----BEGIN PGP PUBLIC KEY BLOCK-----) into your keyring."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end) (region-or-error buffer)
      (run-gpg-work "importing keys from the selection" (list "--import")
                    (lambda (output statuses ok said)
                      (declare (ignore output ok))
                      (message "~a" (import-words statuses said))
                      (refill-open-list "*GnuPG keys*" 'gpg-keys-mode))
                    :input (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer) :start start :end end)))))

(defun import-words (statuses said)
  "What an import did, as words, from gpg's STATUSES."
  (let ((result (gpg-status-p statuses "IMPORT_RES")))
    (if result
        (let ((n (mapcar (lambda (w) (or (ignore-errors (parse-integer w)) 0)) (rest (uiop:split-string result :separator '(#\Space))))))
          ;; IMPORT_RES count no-user-id imported imported-rsa unchanged uids subkeys signatures revocations
          ;; secrets-read secrets-imported ...
          (format nil "Keys imported: ~d new, ~d unchanged~@[, ~d secret~]" (nth 2 n) (nth 4 n)
                  (and (plusp (nth 10 n)) (nth 10 n))))
        (format nil "Nothing imported: ~a" (gpg-failure-words statuses said)))))

(defun with-exported-keys (keys then)
  "Call THEN with the public KEYS armoured, as octets, gpg run in the background."
  (run-gpg-work (format nil "exporting ~d key~:p" (length keys))
                (append (list "--armor" "--export") (mapcar (lambda (k) (getf k :fingerprint)) keys))
                (lambda (output statuses ok said)
                  (declare (ignore statuses))
                  (if (and ok (plusp (length output)))
                      (funcall then output)
                      (message "No keys exported: ~a" (if (plusp (length said)) said "gpg failed"))))
                :asker :agent))

(define-command gpg-keys-export ()
  "Export the public part of the keys marked (or the one at the cursor), armoured, to a file asked for."
  (let ((keys (gpg-keys-chosen)))
    (if (null keys)
        (message "No key on this line")
        (let ((path (read-native-path "Export the keys to: " :kind :any :writing t)))
          (when (and path (gpg-target-free-p path))
            (with-exported-keys keys
              (lambda (octets)
                (write-gpg-output path octets)
                (message "~d key~:p exported to ~a" (length keys) (abbreviated-path path)))))))))

(define-command gpg-keys-insert ()
  "Put the public part of the keys marked (or the one at the cursor), armoured, at point in the buffer the list was opened from."
  (let ((keys (gpg-keys-chosen))
        (target (tuatara.buffer:buffer-local (current-buffer) 'gpg-keys-from)))
    (cond ((null keys) (message "No key on this line"))
          ((not (live-buffer-p target)) (message "The buffer the list was opened from is gone"))
          (t (with-exported-keys keys
               (lambda (octets)
                 (if (not (live-buffer-p target))
                     (message "The buffer the list was opened from is gone")
                     (progn (tuatara.buffer:buffer-insert target (tuatara.buffer:buffer-point target) octets)
                            (show-buffer target)
                            (message "~d key~:p inserted" (length keys))))))))))

(defmethod list-entry-noun ((mode gpg-keys-mode))
  "A key."
  "key")

(defmethod delete-entries ((mode gpg-keys-mode) list ids mark)
  "Delete the keys whose IDs are IDS from your keyring, after asking; a key of
yours with its secret part, asked twice."
  (declare (ignore mark))
  (let ((keys (list-entries-of list ids)))
    (cond ((null keys) (message "No key on this line"))
          ((not (yes-or-no-p* (format nil "Delete ~{~a~^, ~} from your keyring?" (mapcar (lambda (k) (or (getf k :name) (getf k :id))) keys))))
           (message "Nothing deleted"))
          ((and (some (lambda (k) (getf k :secret)) keys)
                (not (yes-or-no-p* "Some are your own keys: their secret parts go too, and what they encrypted cannot be read again; really?")))
           (message "Nothing deleted"))
          (t (gpg-in-background (format nil "deleting ~d key~:p" (length keys))
                                (lambda (cancel)
                                  (loop for key in keys
                                        until (car cancel)
                                        do (gpg-run (list "--batch" "--yes" (if (getf key :secret) "--delete-secret-and-public-key" "--delete-keys")
                                                          (getf key :fingerprint))
                                                    :asker :agent :cancel cancel)))
                                (lambda (&rest ignored)
                                  (declare (ignore ignored))
                                  (when (live-buffer-p list) (gpg-keys-show list))
                                  (message "~d key~:p deleted" (length keys)))
                                :asker :agent :buffer list)))))

(defun install-gpg-commands ()
  "Bind the keys of the list of GnuPG keys; its keys have no mark for deletion."
  (define-key (mode-keymap 'gpg-keys-mode) "d" :undefined)
  (define-keys *gpg-keys-keymap* '(("RET" gpg-keys-details) ("i" import-keys) ("x" gpg-keys-export)
                                   ("I" gpg-keys-insert) ("D" list-delete-marked))))
