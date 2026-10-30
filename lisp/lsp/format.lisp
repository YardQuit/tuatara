;;;; lisp/lsp/format.lisp --- Code laid out as its language's own formatter lays it out.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x format-buffer gives the buffer's text to its language's formatter
;;;; (rustfmt, ruff, clang-format, shfmt...: *LANGUAGE-FORMATTERS*, else the one
;;;; Helix names) on its standard input, in the background, shut in the sandbox:
;;;; it sees the project's folder and its own, read only, and no network. What
;;;; comes back is put in as one change that undo takes back whole, only the
;;;; lines that differ replaced, so that point and the marks stay on their text;
;;;; when the buffer was changed meanwhile, nothing is put in. With no formatter
;;;; installed, its language server lays the code out where one does; else it
;;;; is indented as Tuatara indents it. *FORMAT-ON-SAVE* has the languages it
;;;; names formatted before each save.

(in-package #:tuatara.defaults)

(defvar *format-on-save* '()
  "The languages whose buffers are formatted (\\[format-buffer]) before each
save, the save going on once the text is laid out: a list of language
names, (rust python), or T for every language with a formatter installed.")

(defvar *format-in-sandbox* t
  "True for formatters to run shut in the sandbox: the project's folder and the
formatter's own seen, read only, nothing written, no network. NIL runs them
as any program, for one that cannot work shut in (it needs its plugins from
elsewhere); a formatter reads the project's settings, and some run code of
the project's.")

(defvar *format-seconds* 30
  "The seconds a formatter may take before it is stopped and nothing is changed.")

(defstruct (format-job (:include tuatara.runtime:job) (:constructor make-format-job (buffer version program then)) (:copier nil))
  "BUFFER's text at VERSION being laid out by PROGRAM, a job: OUTPUT, the text
it gave back, and SAID, what it said when it failed; THEN, a function of the
buffer and whether its text was laid out, called once it is over."
  version program then output said)

(setf (documentation 'format-job-p 'function) "True when OBJECT is a FORMAT-JOB.")

(tuatara.runtime:define-job-words format-job
  :doing "Formatting"
  :description (format nil "Formatting ~a with ~a" (tuatara.buffer:buffer-name (tuatara.runtime:job-buffer job))
                       (tuatara.io:native-file-name (format-job-program job)))
  :verb "format"
  :left-words "the text is as it was")

(defun buffer-language (buffer)
  "The language of BUFFER's major mode, or NIL."
  (mode-language (buffer-major-mode buffer)))

(defun formatting-p (buffer)
  "True while a formatter lays out BUFFER's text."
  (and (tuatara.runtime:running-job buffer 'format-job) t))

(defun format-project-directory (buffer language)
  "The folder BUFFER's formatter works from: the top of its project, the
nearest folder up from its file holding one of LANGUAGE's root files
\(Cargo.toml, pyproject.toml) or a project's marker (.git), else its file's own."
  (let ((here (buffer-directory buffer)))
    (or (let ((roots (language-roots language))) (and roots (marked-directory here roots :below-home t)))
        (project-root here)
        here)))

(defun outside-program-refusal (buffer)
  "Why BUFFER's text is kept from other programs (a formatter, a language
server), in words, or NIL: a file opened safely, or in one of
*SAFE-DIRECTORIES*, and the text of an encrypted file."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (cond ((tuatara.io:safe-visit-p buffer) "it is opened safely")
          ((and file (safe-directory-of (tuatara.io:native file))) "it is in one of *safe-directories*")
          ((tuatara.buffer:buffer-local buffer :plaintext) "its text is kept from other programs (an encrypted file)"))))

(defun formatter-output (program command input directory cancel)
  "What the formatter PROGRAM (its full name) of COMMAND (as chosen, the program
and its arguments) prints when given INPUT (octets) from DIRECTORY, as two
values: the octets, or NIL when it failed, and what it said then. Shut in
the sandbox when *FORMAT-IN-SANDBOX*."
  (if *format-in-sandbox*
      (let ((fd (tuatara.io:memory-file "tuatara-format" input :kept nil)))
        (unwind-protect
             (multiple-value-bind (output outcome said)
                 (tuatara.io:sandbox-run program (rest command)
                                         :input fd :seconds *format-seconds* :cancel cancel
                                         :output-limit (+ (* 4 (length input)) (* 1024 1024))
                                         :directories (remove-duplicates
                                                       (list directory (tuatara.io:parent-path program)
                                                             (tuatara.io:parent-path (or (tuatara.io:resolve-links program) program)))
                                                       :test #'equal)
                                         :directory directory)
               (case outcome
                 (:done (values output nil))
                 (:unavailable (values nil (format nil "~a; *format-in-sandbox* nil runs formatters without it"
                                                   (tuatara.io:sandbox-outcome-words outcome said :label "it"))))
                 (:failed (values nil (if (plusp (length said)) said "it failed")))
                 (t (values nil (tuatara.io:sandbox-outcome-words outcome said :label "it" :seconds-setting '*format-seconds*)))))
          (sb-posix:close fd)))
      (multiple-value-bind (output ok)
          (tuatara.io:program-output (cons program (rest command))
                                     :input (tuatara.text:octets-string input) :seconds *format-seconds* :directory directory)
        (if ok (values (tuatara.text:string-octets output) nil) (values nil "it failed")))))

(defun formatter-output-refusal (input output)
  "Why OUTPUT, the octets a formatter gave back for the octets INPUT, is not
put in their place: nothing for a text that is not empty, or what is not
text (well-formed UTF-8) for one that is; NIL when it may be."
  (flet ((raw-p (octets) (plusp (tuatara.text:rope-invalid (tuatara.text:octets->rope (coerce octets 'tuatara.text:octets))))))
    (cond ((null output) nil)
          ((and (zerop (length output)) (plusp (length input))) "it gave nothing back")
          ((and (raw-p output) (not (raw-p input))) "what it gave back is not text (UTF-8)"))))

(defun indent-whole-buffer (buffer)
  "Indent every line of BUFFER as its mode indents it, or while it is narrowed
every line of the part shown; returns what INDENT-LINES does."
  (multiple-value-call #'indent-lines buffer (tuatara.buffer:buffer-bounds buffer)))

(defun start-formatting (buffer &key then)
  "Lay out BUFFER's text: by its language's formatter in the background, or,
with none installed, by Tuatara's own indentation at once; THEN, a function
of the buffer and whether its text was laid out, called once it is over.
Says what it did. While BUFFER is narrowed only the part shown is laid out:
by its language server where it lays out a part, else by indentation; a
formatter of the whole file is not run then, and that is said."
  (let* ((language (buffer-language buffer))
         (command (and language (language-formatter-command language)))
         (program (installed-program command))
         (refusal (and program (outside-program-refusal buffer)))
         (narrowed (tuatara.buffer:buffer-narrowed-p buffer)))
    (cond
      ((or (tuatara.io:safe-visit-p buffer) (tuatara.buffer:buffer-local buffer :read-only))
       (message "~a is ~:[read only~;opened safely~]: not formatted" (tuatara.buffer:buffer-name buffer) (tuatara.io:safe-visit-p buffer))
       (when then (funcall then buffer nil)))
      ;; narrowed: the part shown, by a language server that lays out a part
      ((and narrowed (buffer-servers buffer)
            (multiple-value-bind (start end) (tuatara.buffer:buffer-bounds buffer)
              (lsp-format buffer :start start :end end :then then))))
      ;; a formatter lays out the whole file, never only the part shown
      ((and narrowed program (not refusal))
       (message "~a" (tuatara.buffer:outside-narrowing-words buffer))
       (when then (funcall then buffer nil)))
      ;; no formatter of its own here: its language server, when one lays out code
      ((and (not narrowed) (null program) (buffer-servers buffer) (lsp-format buffer :then then)))
      ((or (null program) refusal)
       (let ((changed (indent-whole-buffer buffer)))
         (case changed
           (:none (message "~a has no formatter~@[ (~a is not installed)~] and no indentation rules: nothing done"
                           (tuatara.buffer:buffer-name buffer) (and command (first command))))
           (:too-large (message "~a is too large to indent here" (tuatara.buffer:buffer-name buffer)))
           (t (message "~a indented by Tuatara~:[~*~; (~a: not given to a formatter)~]~@[, ~a not being installed~]"
                       (tuatara.buffer:buffer-name buffer) refusal refusal (and command (not refusal) (first command)))))
         (when then (funcall then buffer (not (member changed '(:none :too-large)))))))
      ((formatting-p buffer)
       (message "~a is being formatted already" (tuatara.buffer:buffer-name buffer)))
      (t
       (let* ((version (tuatara.buffer:buffer-version buffer))
              (input (tuatara.text:rope-octets (tuatara.text:version-rope version)))
              (directory (format-project-directory buffer language)))
         (tuatara.runtime:start-job-thread
          (make-format-job buffer version program then)
          (let ((in-sandbox *format-in-sandbox*) (seconds *format-seconds*))
            (lambda (job)
              (let ((*format-in-sandbox* in-sandbox) (*format-seconds* seconds))
                (multiple-value-bind (output said)
                    (formatter-output program command input directory (tuatara.runtime:job-cancel job))
                  (let ((refused (formatter-output-refusal input output)))
                    (setf (format-job-output job) (and (not refused) output)
                          (format-job-said job) (or refused said)))))))
          :name "tuatara format"))))))

(defun finish-formatting (buffer name version failure put-in)
  "Put in what NAME, a formatter or a language server, laid out of BUFFER's
text at VERSION, by PUT-IN, a function of no arguments that returns how many
places it changed, when it did not fail (FAILURE, words, NIL when it did not)
and the buffer is as it was given; say what was done, or why not. True when
the text was put in."
  (cond (failure
         (message "~a was not formatted by ~a: ~a" (tuatara.buffer:buffer-name buffer) name failure)
         nil)
        ((not (eq (tuatara.buffer:buffer-version buffer) version))
         (message "~a changed while ~a laid it out: nothing put in; format it again" (tuatara.buffer:buffer-name buffer) name)
         nil)
        (t (message "~a formatted by ~a~[: it was laid out already~:;, ~:*~d place~:p changed~]"
                    (tuatara.buffer:buffer-name buffer) name (funcall put-in))
           t)))

(defmethod tuatara.runtime:job-take-in ((job format-job))
  "Put the laid-out text in when the buffer is as it was given, say why not
otherwise, and go on with what was waiting for it."
  (let* ((buffer (tuatara.runtime:job-buffer job))
         (output (format-job-output job))
         (formatted
           (cond ((not (live-buffer-p buffer)) (return-from tuatara.runtime:job-take-in))
                 ((not (eq (tuatara.runtime:job-status job) :done))
                  (message "~a was not formatted: ~a" (tuatara.buffer:buffer-name buffer) (tuatara.runtime:job-ending-words job))
                  nil)
                 (t (finish-formatting buffer (tuatara.io:native-file-name (format-job-program job)) (format-job-version job)
                                       (and (null output) (format-job-said job))
                                       (lambda ()
                                         (put-text-by-lines buffer (tuatara.text:version-rope (format-job-version job))
                                                           (tuatara.text:octets->rope (coerce output 'tuatara.text:octets)))))))))
    (when (format-job-then job) (funcall (format-job-then job) buffer formatted))))

(define-command format-buffer ()
  "Lay out the code of the current buffer as its language's formatter does
\(*LANGUAGE-FORMATTERS*, else the language's usual one: rustfmt, ruff,
clang-format, shfmt...), in the background, shut in the sandbox; the change is one that
\\[undo] takes back whole. With no formatter installed, its language server
lays it out where one does, else each line is indented as the mode indents
it. A file opened safely, in *safe-directories*, or the text of an encrypted
file is only indented, never given to another program. In a narrowed buffer
only the part shown: indented, or laid out by a language server that lays
out a part; a formatter of the whole file is not run, and that is said."
  (start-formatting (current-buffer)))

;;; Before saving

(defun format-wanted-p (buffer)
  "True when BUFFER is to be formatted before it is saved (*FORMAT-ON-SAVE*)
and has changed since it last was."
  (let ((language (buffer-language buffer)))
    (and language
         (or (eq *format-on-save* t)
             (and (listp *format-on-save*)
                  (member (symbol-name (language-name language)) *format-on-save* :key #'string :test #'string-equal)))
         (or (installed-program (language-formatter-command language))
             (lsp-server-offers buffer "documentFormattingProvider" "format"))
         (null (outside-program-refusal buffer))
         (not (eql (tuatara.buffer:buffer-local buffer 'formatted-at) (tuatara.buffer:buffer-content-number buffer))))))

(defun format-before-save (buffer)
  "Before BUFFER is saved, when it is to be formatted first (FORMAT-WANTED-P):
start formatting it and return :LATER, the save made once that is over,
laid out or, when the formatter failed, as it is."
  (when (and (format-wanted-p buffer) (not (formatting-p buffer)))
    (let ((path *save-path*))
      (start-formatting buffer :then (lambda (b formatted)
                                       (declare (ignore formatted))
                                       ;; formatted or not, not tried again for this save
                                       (setf (tuatara.buffer:buffer-local b 'formatted-at) (tuatara.buffer:buffer-content-number b))
                                       (save-buffer-with-care b :path path)))
      :later)))

(defun install-format ()
  "Have buffers of the languages in *FORMAT-ON-SAVE* formatted before they are saved."
  (pushnew 'format-before-save *before-save-functions*))
