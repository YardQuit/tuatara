;;;; lisp/packages/loading.lisp --- Packages: compiled, checked against the names Tuatara has, loaded and forgotten.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; What is there is compiled into the cache once for each version of Tuatara, the
;;;; names a package uses are checked against those this Tuatara has, and a package
;;;; that fails to load is taken back out.

(in-package #:tuatara.packages)

;;; Loading what is there

(defvar *tuatara-identity* nil
  "Which Tuatara compiles packages, once known (TUATARA-IDENTITY).")

(defun tuatara-identity ()
  "Which Tuatara this is, as far as compiled packages go: the moment the saved
executable was built, or, run from its sources, the newest of them; another
one compiles packages anew, as what they were compiled against may have
changed."
  (or *tuatara-identity*
      (setf *tuatara-identity*
            (format nil "~:[sources-~;built-~]~d" tuatara:*built-at*
                    (or tuatara:*built-at*
                        (let ((top (asdf:system-source-directory :tuatara)))
                          (loop for file in (append (directory (merge-pathnames "src/**/*.lisp" top))
                                                    (directory (merge-pathnames "lisp/**/*.lisp" top)))
                                maximize (or (file-write-date file) 0))))))))

(defun packages-compiled-directory ()
  "The directory in the cache that packages are compiled into, a directory for each
Steel Bank Common Lisp (SBCL) in it (PACKAGE-CACHE)."
  (format nil "~apackages/" (tuatara.io:native (tuatara.io:cache-directory))))

(defun package-cache ()
  "The directory packages are compiled into for this Tuatara and this Steel Bank
Common Lisp (SBCL); those of other ones are deleted the first time it is
asked for."
  (let* ((sbcl (format nil "~a~a/" (packages-compiled-directory)
                       (tuatara.io:percent-encoded (lisp-implementation-version) :safe "._-")))
         (here (format nil "~a~a/" sbcl (tuatara-identity))))
    (unless (probe-file here)
      (dolist (other (append (ignoring-errors-reported ("reading ~a" sbcl) (uiop:subdirectories sbcl))
                             (ignoring-errors-reported ("reading ~a" sbcl) (uiop:directory-files sbcl))))
        (delete-package-directory (namestring other))))
    here))

(defun package-fasl (file)
  "Where the compiled FILE of a package is kept: in the cache, by which
Tuatara and which Steel Bank Common Lisp (SBCL) compile it (PACKAGE-CACHE), in
directories as the file's own are, fetched/ for a package fetched and local/ for
one of a directory of yours."
  (let* ((native (tuatara.io:native file))
         (fetched (tuatara.io:native (packages-directory)))
         (inside (if (uiop:string-prefix-p fetched native)
                     (concatenate 'string "fetched/" (subseq native (length fetched)))
                     (concatenate 'string "local" native))))
    (format nil "~a~a.fasl" (package-cache) inside)))

(defun tuatara-user:package-file (name)
  "The file NAME (\"helper.sh\") of the package being loaded: in its
directory, or the directory of the file being loaded when no package is. A package
names its own files so, as where it is loaded from is the cache's."
  (merge-pathnames name (or *package-directory*
                            (and *load-truename*
                                 (make-pathname :name nil :type nil :version nil :defaults *load-truename*))
                            *default-pathname-defaults*)))

(defun compile-failure-words (report)
  "Why compiling a file failed, on one line, from REPORT, what the compiler
wrote, the error it caught (a reader error, an unclosed parenthesis, and the
line its form starts on); NIL when it names none."
  (let ((at (search "caught ERROR:" report)))
    (when at
      (let* ((text (subseq report (+ at (length "caught ERROR:"))
                           (or (search "compilation unit" report :start2 at) (length report))))
             (text (cl-ppcre:regex-replace-all "(?m)^;" text ""))
             (text (cl-ppcre:regex-replace-all "#<[^>]*>" text "the file"))
             (text (string-trim " " (cl-ppcre:regex-replace-all "\\s+" text " "))))
        (and (plusp (length text)) (subseq text 0 (min 240 (length text))))))))

;;; Whether the names a package uses are all there

(defun undefined-name (warning)
  "The function or variable WARNING, one of the compiler's, says is defined
nowhere, as two values: :FUNCTION or :VARIABLE, and its name; NIL for any
other warning."
  (when (typep warning 'simple-condition)
    (let ((tail (member-if (lambda (x) (member x '(:function :variable))) (simple-condition-format-arguments warning))))
      (when (and (rest tail) (search "undefined" (princ-to-string warning)))
        (values (first tail) (second tail))))))

(defun needed-name (kind name)
  "The name NAME of the KIND (:FUNCTION or :VARIABLE) a package's code uses
without its definition, as kept beside its compiled file: (KIND PACKAGE
SYMBOL), the names of its symbol's home package and of the symbol, KIND
:SETF-FUNCTION for (setf NAME); NIL for a symbol in no package."
  (let* ((setf-p (consp name))
         (symbol (if setf-p (second name) name))
         (home (and (symbolp symbol) (symbol-package symbol))))
    (and home (list (if setf-p :setf-function kind) (package-name home) (symbol-name symbol)))))

(defun needed-name-p (thing)
  "True when THING is a name as NEEDED-NAME makes it."
  (and (consp thing) (member (first thing) '(:function :setf-function :variable))
       (consp (rest thing)) (stringp (second thing))
       (consp (cddr thing)) (stringp (third thing)) (null (cdddr thing))))

(defun needed-name-symbol (needed)
  "The symbol NEEDED, as NEEDED-NAME makes it, names now; NIL when its
package or the symbol is not there."
  (let ((home (find-package (second needed))))
    (and home (find-symbol (third needed) home))))

(defun needed-name-present-p (needed)
  "True when the function or variable NEEDED (NEEDED-NAME) is defined now."
  (let ((symbol (needed-name-symbol needed)))
    (and symbol
         (ecase (first needed)
           (:function (fboundp symbol))
           (:setf-function (fboundp (list 'setf symbol)))
           (:variable (or (boundp symbol)
                          (member (sb-int:info :variable :kind symbol) '(:special :global :constant :macro))))))))

(defun beside-fasl (fasl type)
  "The file of TYPE kept beside the compiled file FASL: \"needs\", the names
its code uses without defining them (NEEDED-NAME); \"refused\", for the first
file of a package refused for names this Tuatara lacks, its files as they
were then and those names, so that it is refused again before any of it
loads (REFUSE-AS-BEFORE)."
  (concatenate 'string (subseq fasl 0 (- (length fasl) (length "fasl"))) type))

(defun write-kept-list (file list)
  "Write LIST, of keywords, strings, numbers and lists of them, to FILE, which
only you may read."
  (tuatara.io:write-private-file file (with-standard-io-syntax
                                        (let ((*print-readably* nil)) (prin1-to-string list)))))

(defun read-kept-list (file)
  "The list WRITE-KEPT-LIST wrote to FILE, nothing in it evaluated, and true
as a second value; NIL when there is none or it cannot be read."
  (let ((text (ignoring-errors-reported ("reading ~a" file) (and (probe-file file) (tuatara.io:file-text file)))))
    (when text
      (handler-case (reporting-errors ("reading ~a" file)
                                      (with-standard-io-syntax
                                        (let* ((*read-eval* nil)
                                               (*package* (find-package :common-lisp))
                                               (list (read-from-string text)))
                                          (and (listp list) (values list t)))))
        (error () nil)))))

(defvar *package-check* nil
  "While a package loads, what is gathered to see that every name its code
uses is there (a PACKAGE-CHECK); NIL otherwise.")

(defvar *loading-package-name* nil
  "The name of the package its load-package is loading, whose names are
checked (LOAD-PACKAGE-FILE); NIL when a file is loaded by itself.")

(define-record (package-check (:constructor make-package-check (name refusal)) (:copier nil))
  "What is gathered while the package NAME loads: the Lisp packages there
before it, the names its files use without defining them (NEEDS), and the
files loaded, each (FILE . WRITE-DATE) (SOURCES); REFUSAL is the file its
refusal is kept in (BESIDE-FASL). BEGUN turns true as the first of its
compiled files begins to load."
  (name "" :read-only t :documentation "The name of the package being checked.")
  (refusal nil :read-only t :documentation "The file the package's refusal is kept in.")
  (before (list-all-packages) :read-only t :documentation "The Lisp packages there before the package began to load.")
  (needs '() :documentation "The names the package's files use without defining them.")
  (sources '() :documentation "The package's files loaded, each (FILE . WRITE-DATE).")
  (begun nil :documentation "True once a compiled file of the package has begun to load."))

(define-condition package-lacks-names (error)
  ((name :initarg :name :reader package-lacks-names-name)
   (names :initarg :names :reader package-lacks-names-names))
  (:report (lambda (condition stream)
             (format stream "~a needs names this Tuatara lacks: ~{~a~^, ~}; update the package (~a) or Tuatara"
                     (package-lacks-names-name condition) (package-lacks-names-names condition)
                     (substitute-command-keys "\\[package-update]"))))
  (:documentation "Signalled when the package NAME uses functions or variables, NAMES in
words, that this Tuatara does not have: it is not loaded."))

(setf (documentation 'package-lacks-names-name 'function) "The name of the package refused."
      (documentation 'package-lacks-names-names 'function) "The names it uses that are not there, in words.")

(defun package-check-own (check)
  "The Lisp packages of the package CHECK is about: those made since it began
to load, and the one of its name when it had loaded before."
  (let ((own (set-difference (list-all-packages) (package-check-before check)))
        (named (find-package (string-upcase (package-check-name check)))))
    (when (and named (package-loaded-p (package-check-name check)))
      (pushnew named own))
    own))

(defun outside-name-p (needed check own)
  "True when NEEDED (NEEDED-NAME) is a name of a Lisp package that was there
before the package CHECK is about began to load, and not one of OWN, its own
\(PACKAGE-CHECK-OWN): none of its files to come can define it."
  (let ((home (find-package (second needed))))
    (and home
         (member home (package-check-before check))
         (not (member home own)))))

(defun needed-name-words (needed own)
  "NEEDED (NEEDED-NAME) in words: its symbol's name, after its package's name
when that is not one of OWN, the package's own Lisp packages."
  (let* ((home (find-package (second needed)))
         (name (string-downcase (third needed)))
         (word (if (and home (member home own))
                   name
                   (format nil "~(~a~)~:[::~;:~]~a" (second needed)
                           (and home (eq (nth-value 1 (find-symbol (third needed) home)) :external))
                           name))))
    (if (eq (first needed) :setf-function) (format nil "(setf ~a)" word) word)))

(defun forget-package-code (own &optional directory)
  "Take out of Tuatara what the code of the Lisp packages OWN, loaded from
DIRECTORY when given, put there, and delete them. Their jobs running, of their
own kinds, are asked to stop. Their modes go, a buffer in one of them put in
the mode its file calls for, or fundamental-mode. Their commands go, and those
whose function is theirs. The keys bound to their commands go, with their
labels. The places they gave commands to run in (COMMANDS-FOR-MODE) go,
Tuatara's own among them. Their ways of showing text (DEFINE-DISPLAY-TOGGLE)
go. Their functions on the lists of functions Tuatara calls (*FILE-VIEWS*,
*TICK-FUNCTIONS* and the rest) go, and so do their methods of Tuatara's
generic functions. Their settings are settings no more, their values kept for
when they load again. Their faces stay, doing nothing. A function is theirs
when one of their symbols names it, or it was made in one of their functions
or by a file of DIRECTORY."
  (when own
    (labels ((own-p (name)
               (typecase name
                 (null nil)
                 (symbol (and (member (symbol-package name) own) t))
                 (string (and directory (tuatara.io:path-within-p name directory)))
                 (cons (or (own-p (car name)) (own-p (cdr name))))))
             (theirs-p (thing)
               ;; a function made within another is named (LAMBDA (...) :IN NAME-OR-FILE)
               (cond ((functionp thing) (own-p (nth-value 2 (function-lambda-expression thing))))
                     ((symbolp thing) (own-p thing)))))
      (dolist (job tuatara.runtime:*jobs*)
        (when (and (tuatara.runtime:job-running-p job) (theirs-p (type-of job)))
          (tuatara.runtime:job-stop job)))
      (let ((modes (remove-if-not #'theirs-p (mode-names))))
        (mapc #'forget-major-mode modes)
        (dolist (buffer (buffer-list))
          (when (member (type-of (buffer-major-mode buffer)) modes)
            (set-auto-mode buffer))))
      (loop for name being the hash-keys of tuatara.command:*commands* using (hash-value function)
            when (or (theirs-p name) (theirs-p function))
            collect name into gone
            finally (mapc #'tuatara.command:forget-command gone))
      (dolist (command (loop for name being the hash-keys of *mode-commands* collect name))
        (let ((kept (and (not (theirs-p command)) (remove-if #'theirs-p (gethash command *mode-commands*) :key #'car))))
          (if kept
              (setf (gethash command *mode-commands*) kept)
              (remhash command *mode-commands*))))
      (dolist (mode (mode-names))
        (unbind-commands-if #'theirs-p (mode-keymap mode)))
      (when *global-keymap* (unbind-commands-if #'theirs-p *global-keymap*))
      (setf *display-toggles* (remove-if #'theirs-p *display-toggles* :key #'display-toggle-name))
      (do-external-symbols (name :tuatara)
        (when (and (boundp name) (not (constantp name)))
          (let ((value (symbol-value name)))
            (when (and (consp value) (handler-case (list-length value) (type-error () nil)) (some #'theirs-p value))
              (setf (symbol-value name) (remove-if #'theirs-p value))))))
      (let ((names (mapcar #'package-name own))
            (settings '()))
        (do-external-symbols (symbol :tuatara-user)
          (when (member (get symbol 'package-setting) names :test #'equal)
            (push symbol settings)))
        (dolist (symbol settings)
          (remprop symbol 'package-setting)
          (unexport symbol :tuatara-user)))
      (dolist (package own)
        (do-symbols (symbol package)
          (let ((class (and (eq (symbol-package symbol) package) (find-class symbol nil))))
            (when class
              (dolist (method (copy-list (sb-mop:specializer-direct-methods class)))
                (remove-method (sb-mop:method-generic-function method) method)))))))
    (dolist (package own)
      (dolist (user (package-used-by-list package))
        (unuse-package package user))
      (delete-package package))))

(defun take-out-failed-load (check)
  "Take out what of the package CHECK is about loaded before its loading
failed, was stopped or was refused (FORGET-PACKAGE-CODE), and return true,
when no version of it ran before. When one did, nothing is taken out, since
that would take the running version's code with it, and NIL is returned:
the running version goes on whole when nothing of the new one had begun to
load, else the package is marked as running parts of both (*PACKAGES-MIXED*)."
  (let ((name (package-check-name check)))
    (cond ((not (package-loaded-p name))
           (forget-package-code (package-check-own check) (package-directory-native))
           t)
          ((package-check-begun check)
           (pushnew name *packages-mixed* :test #'string=)
           nil))))

(defun refuse-package (check needed)
  "Refuse the package CHECK is about, for the names NEEDED (NEEDED-NAME) it
uses that are not there: its files and those names kept to refuse it again
before it loads (BESIDE-FASL), what of it loaded taken out
\(FORGET-PACKAGE-CODE), and a PACKAGE-LACKS-NAMES error signalled. Nothing is
refused while a package is compiled apart (*PACKAGE-COMPILING* :APART), where
names the editor has may be missing: the editor refuses it as it loads."
  (when (member *package-compiling* '(:apart :apart-said))
    (return-from refuse-package nil))
  (let* ((own (package-check-own check))
         (words (sort (remove-duplicates (mapcar (lambda (n) (needed-name-words n own)) needed) :test #'string=)
                      #'string<)))
    (when (and (package-check-refusal check) (package-check-sources check))
      (ignoring-errors-reported ("writing ~a" (package-check-refusal check))
                                (write-kept-list (package-check-refusal check)
                                                 (list :sources (package-check-sources check) :needs needed))))
    (tuatara.runtime:debug-report :package "~a refused, its code using names this Tuatara lacks: ~{~a~^, ~}"
                                  (package-check-name check) words)
    (when (take-out-failed-load check)
      (setf *loaded-packages* (remove (package-check-name check) *loaded-packages* :test #'string=)))
    (error 'package-lacks-names :name (package-check-name check) :names words)))

(defun refuse-as-before (check)
  "Refuse at once the package CHECK is about when it was refused for names
this Tuatara lacks (BESIDE-FASL), its files are as they were then and those
names are not there still; else forget that it was. Not while a package is
compiled apart (REFUSE-PACKAGE)."
  (let ((file (and (not (member *package-compiling* '(:apart :apart-said))) (package-check-refusal check))))
    (multiple-value-bind (kept read) (read-kept-list file)
      (when read
        (let* ((needs (handler-case (getf kept :needs) (type-error () nil)))
               (sources (handler-case (getf kept :sources) (type-error () nil)))
               (missing (and (listp needs) (remove-if #'needed-name-present-p (remove-if-not #'needed-name-p needs))))
               (kept-at (or (ignoring-errors-reported ("reading the time of ~a" file) (file-write-date file)) 0)))
          ;; file times are whole seconds: a file of the refusal's second may have changed after it
          (if (and missing (consp sources) (handler-case (list-length sources) (type-error () nil))
                   (every (lambda (source)
                            (and (consp source) (stringp (car source)) (integerp (cdr source))
                                 (eql (ignoring-errors-reported ("reading the time of ~a" (car source))
                                                                (file-write-date (car source)))
                                      (cdr source))
                                 (< (cdr source) kept-at)))
                          sources))
              (refuse-package check missing)
              (handler-case (delete-file file) (file-error () nil))))))))

(defun compile-package-file (file fasl)
  "Compile FILE, of a package, into FASL, and keep beside it the names its
code uses without defining them (BESIDE-FASL), which it returns. Its
other warnings are said in *Messages*, the first of them; a file that cannot
be compiled at all is an error saying why (an unclosed parenthesis, where).
Where it may be compiled is *PACKAGE-COMPILING*'s (NOTE-PACKAGE-COMPILING).
FASL is written whole or not at all: compiled under another name beside it
and renamed over it, after the names it needs are kept, so that a load
reading it meanwhile (another process compiling the package, one stopped)
finds the old one or the new, and the new one's names with it."
  (let ((*package* (find-package :tuatara-user))
        (said '())
        (needed '())
        (report (make-string-output-stream))
        (temporary (format nil "~a.~d.~a.new" fasl (sb-posix:getpid) (tuatara.io:random-name))))
    (unwind-protect
         (let ((output (handler-bind ((warning (lambda (w)
                                                 (multiple-value-bind (kind name) (undefined-name w)
                                                   (cond (kind (let ((entry (needed-name kind name)))
                                                                 (when entry (pushnew entry needed :test #'equal))))
                                                         ((not (typep w 'style-warning)) (push (princ-to-string w)
                                                                                               said))))
                                                 (muffle-warning w))))
                         (let ((*error-output* report))
                           (note-package-compiling file)
                           ;; a compilation unit of its own: what it finds undefined is said at its end
                           (with-compilation-unit (:override t)
                             (compile-file file :output-file temporary :verbose nil :print nil))))))
           (unless output
             (refuse "~a could not be compiled~@[: ~a~]" (abbreviated-path (tuatara.io:native file))
                     (or (compile-failure-words (get-output-stream-string report)) (first said))))
           ;; the names first: a fasl is never there before the names it needs
           (write-kept-list (beside-fasl fasl "needs") needed)
           (sb-posix:rename (tuatara.io:native output) fasl)
           (when said
             (log-message (format nil "~a compiled with ~d warning~:p: ~a" (abbreviated-path (tuatara.io:native file))
                                  (length said) (first (last said)))))
           needed)
      (when (probe-file temporary) (handler-case (delete-file temporary) (file-error () nil))))))

(defvar *package-load-stop* nil
  "While a package loads in the background (START-PACKAGE-LOAD), a cons whose
car turns true when the loading is asked to stop: it stops before the next
file of the package (LOAD-CHECKED-FILE); NIL otherwise.")

(define-condition package-load-stopped (error)
  ((name :initarg :name :reader package-load-stopped-name))
  (:report (lambda (condition stream)
             (format stream
                     "The package ~a is half loaded; restart Tuatara before using it (its commands are taken out)"
                     (package-load-stopped-name condition))))
  (:documentation "Signalled when the loading of the package NAME was stopped between two of
its files: what of it loaded is taken out (FORGET-PACKAGE-CODE), its
functions left behind until Tuatara starts again."))

(setf (documentation 'package-load-stopped-name 'function) "The name of the package stopped as it loaded.")

(define-condition package-needs-restart (error)
  ((name :initarg :name :reader package-needs-restart-name)
   (structures :initarg :structures :reader package-needs-restart-structures))
  (:report (lambda (condition stream)
             (format stream "The new version of the package ~a loads at the next start: its structures ~{~a~^, ~} ~
                             are laid out otherwise than in the version running now"
                     (package-needs-restart-name condition) (package-needs-restart-structures condition))))
  (:documentation "Signalled when a new version of the package NAME cannot be loaded over the
one running: it defines STRUCTURES, a list of their names, laid out otherwise.
Nothing of it is loaded, the version running now goes on, and the new one is
loaded at the next start."))

(setf (documentation 'package-needs-restart-name 'function) "The name of the package that loads at the next start."
      (documentation 'package-needs-restart-structures 'function)
      "The names of the structures the new version of the package lays out otherwise.")

(defun load-checked-file (file check)
  "Compile FILE when it has changed since (COMPILE-PACKAGE-FILE) and load it.
With CHECK, the names its code uses without defining them are added to it,
and it is refused before it loads when one of them is another's and not
there (OUTSIDE-NAME-P). A PACKAGE-LOAD-STOPPED, nothing of FILE loaded, once
the loading was asked to stop (*PACKAGE-LOAD-STOP*)."
  (when (and *package-load-stop* (car *package-load-stop*) *loading-package-name*)
    (error 'package-load-stopped :name *loading-package-name*))
  (let ((fasl (package-fasl file)))
    (tuatara.io:ensure-private-directory (directory-namestring fasl))
    (let ((needed (multiple-value-bind (kept read) (read-kept-list (beside-fasl fasl "needs"))
                    ;; file times are whole seconds: a fasl of the same second as its file may
                    ;; be older than the file's last change, and is made again, unless it was
                    ;; just made apart for this very load (made again at the next one)
                    (if (and read (handler-case (every #'needed-name-p kept) (type-error () nil))
                             (probe-file fasl)
                             (funcall (if (eq *package-compiling* :refused) #'>= #'>)
                                      (file-write-date fasl) (file-write-date file)))
                        kept
                        (compile-package-file file fasl)))))
      (when check
        (push (cons (namestring file) (file-write-date file)) (package-check-sources check))
        (when needed
          (let* ((own (package-check-own check))
                 (missing (remove-if (lambda (n) (or (not (outside-name-p n check own)) (needed-name-present-p n)))
                                     needed)))
            (when missing (refuse-package check missing))))
        (setf (package-check-needs check) (union needed (package-check-needs check) :test #'equal)))
      (let ((*package* (find-package :tuatara-user))
            (*package-directory* (make-pathname :name nil :type nil :version nil :defaults file)))
        (when check (setf (package-check-begun check) t))
        (load fasl)))))

(defun tuatara-user:load-package-file (name)
  "Compile, when it has changed since, and load the file NAME (\"example.lisp\")
of the package being loaded, or of the directory of the file being loaded: a
package's entry loads its other files so. Compiled into the cache once, a
package loads in a few milliseconds at the next start; meanwhile
PACKAGE-FILE names its files from its own directory. Its warnings are said in
*Messages*, the first of them; a file that cannot be compiled at all is an
error saying why (an unclosed parenthesis, where).

Every function and variable a package's code uses must be there once all of
its files have loaded, Tuatara's above all: a package load-package loads
that uses a name this Tuatara lacks is refused, with an error naming them,
and what of it loaded is taken out again. A name of Tuatara's or of another
library is looked for before the file using it loads; a package refused is
refused again before any of it loads, until its files or Tuatara change. A
package whose loading fails or is stopped part of the way has what of it
loaded taken out again too.

A package loaded while the editor runs is first compiled in a process of
its own, Tuatara run again without a window or a buffer shown, where its
files load as they are compiled; then they are loaded in the editor, the
editor waiting meanwhile, *EDITOR* the editor's. So the code a package runs
as it loads runs twice, once there with no editor: it defines, binds keys
and adds to lists, and leaves to its commands anything that needs the
editor, a buffer, or changes outside Tuatara (files written, programs
started). Where it fails there, the package is compiled as it loads in the
editor instead."
  (let ((file (tuatara-user:package-file name)))
    (unless (probe-file file) (refuse "The package has no file ~a" (abbreviated-path (tuatara.io:native file))))
    (if (or *package-check* (not *loading-package-name*))
        (load-checked-file file *package-check*)
        (let* ((check (make-package-check *loading-package-name* (beside-fasl (package-fasl file) "refused")))
               (*package-check* check))
          (refuse-as-before check)
          ;; failed or stopped between its files: what of it loaded taken out again
          (let ((loaded nil))
            (unwind-protect (progn (load-checked-file file check) (setf loaded t))
              (unless loaded (take-out-failed-load check))))
          (let ((missing (remove-if #'needed-name-present-p (package-check-needs check))))
            (when missing (refuse-package check missing)))
          (let* ((name (package-check-name check))
                 (own (union (package-check-own check) (cdr (assoc name *package-code* :test #'string=)))))
            (setf *package-code* (acons name own (alist-without name *package-code*))))))
    file))

(defun load-package-for-test (name directory)
  "Load the package NAME from DIRECTORY as load-package loads one fetched anew:
each of its files compiled afresh, its names checked; then DIRECTORY's
test.lisp, its tests, when there is one. For a package repository's tests,
run against a Tuatara source tree. NIL when it loaded; else why Tuatara
refused it, a string: it uses a name this Tuatara lacks."
  (dolist (file (uiop:directory-files directory "*.lisp"))
    (uiop:delete-file-if-exists (package-fasl file)))
  (handler-case
      (let ((*package-directory* directory)
            (*loading-package-name* name))
        (tuatara-user:load-package-file (concatenate 'string name ".lisp"))
        (let ((tests (merge-pathnames "test.lisp" directory)))
          (when (probe-file tests) (load tests)))
        nil)
    (package-lacks-names (refusal) (princ-to-string refusal))))

(defun claim-package-setting (name owner)
  "The symbol of the setting NAME (a string) of the package whose Lisp package
is OWNER: in TUATARA-USER, exported, so that an init file names it as it
names Tuatara's own, and made OWNER's in place of a symbol of that name of
its own. An error when NAME does not begin with OWNER's name (*NAME-), or is
already a setting of Tuatara or of another package."
  (let ((prefix (format nil "*~a-" (package-name owner))))
    (unless (and (> (length name) (length prefix)) (string= prefix name :end2 (length prefix)))
      (refuse "A setting of the package ~(~a~) is named ~(~a~)...*, not ~(~a~)" (package-name owner) prefix name))
    (multiple-value-bind (symbol status) (find-symbol name :tuatara-user)
      (when (and status (not (eq status :internal))
                 (not (equal (get symbol 'package-setting) (package-name owner))))
        (refuse "~(~a~) is already a setting of ~a" name
                (let ((other (get symbol 'package-setting))) (if other (format nil "the package ~(~a~)" other)
                                                                 "Tuatara"))))
      (let ((symbol (or symbol (intern name :tuatara-user)))
            (own (find-symbol name owner)))
        (setf (get symbol 'package-setting) (package-name owner))
        (export symbol :tuatara-user)
        (unless (eq own symbol)
          (when (and own (eq (symbol-package own) owner)) (unintern own owner))
          (import symbol owner))
        symbol))))

(defmacro define-package-setting (name value documentation &key values)
  "Define NAME, a setting of the package being loaded, with VALUE unless an
init file set it first, and DOCUMENTATION: in TUATARA-USER, where an init file
sets it by that name, and NAME in the package's own code
\(CLAIM-PACKAGE-SETTING). Its name begins with the package's (*facematch-).
VALUES describes the values it takes, as Tuatara's own settings do
\(DEFINE-SETTING): boolean, (member :left :right), (integer 0 *), string,
directory, (list-of string), (or (member nil) string) or words. Help and the
manual show them, and \\[check-health] says when the setting holds another.
It comes before the package's code that reads it, at the top of its file: a
name read before it is defined is the package's own, not the setting."
  (let ((owner (package-name *package*)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (claim-package-setting ,(symbol-name name) (find-package ,owner)))
       (define-setting ,(claim-package-setting (symbol-name name) *package*) ,value ,documentation
                       :values ,values))))

(defvar *package-needs* '()
  "What the packages loaded need of the system, each (NAME . NEEDS) as
DECLARE-PACKAGE-NEEDS keeps them; \\[check-health] lists them.")

(defun declare-package-needs (&rest needs)
  "Say what the package being loaded needs of the system, for \\[check-health]
to list as found or missing, in a section of its own. Each of NEEDS is
\(PROGRAMS WHAT &key REQUIRED INSTALL): PROGRAMS the name of a program, or a
list of names of which any one will do; WHAT what the package does with it;
REQUIRED true when the package does not work without it, else it is only
suggested; INSTALL what to do when it is not found (\"install ImageMagick\").
A second call replaces the first. Nothing is run: the programs are only
looked for on the PATH."
  (unless *loading-package-name* (refuse "declare-package-needs is for a package while it loads"))
  (setf *package-needs*
        (acons *loading-package-name*
               (loop for (programs what . options) in needs
                     collect (list :programs programs :what what :required (getf options :required)
                                   :install (getf options :install)))
               (alist-without *loading-package-name* *package-needs*)))
  needs)

(defun package-entry (directory name file)
  "The file a package called NAME in DIRECTORY is loaded by: FILE when given,
else NAME.lisp, else load.lisp; NIL when there is none."
  (some (lambda (entry) (and entry (probe-file (merge-pathnames entry directory)) entry))
        (list file (concatenate 'string name ".lisp") "load.lisp")))

;;; The version of Tuatara a package needs

(defparameter *package-header-octets* 16384
  "How much of a package's entry file is read for the version of Tuatara it
needs: the comment lines at its top.")

(defun needed-version-in (text)
  "The version of Tuatara the start of a package's entry file, TEXT, says it
needs, as a string, a line of the comments at its top that reads
\";;;; Needs: Tuatara 0.91.0\" (any number of semicolons, \"or later\" after
it allowed); NIL when there is none."
  (loop for start = 0 then (1+ end)
        for end = (or (position #\Newline text :start start) (length text))
        for trimmed = (string-trim '(#\Space #\Tab #\Return) (subseq text start end))
        while (or (zerop (length trimmed)) (char= (char trimmed 0) #\;))
        do (let ((found (nth-value 1 (cl-ppcre:scan-to-strings
                                      "(?i)^;+\\s*Needs:\\s*Tuatara\\s+([0-9]+(?:\\.[0-9]+){0,3})(?:\\s+or\\s+later)?\\s*\\.?$"
                                      trimmed))))
             (when found (return (aref found 0))))
        until (= end (length text))))

(defun package-needed-version (directory name file)
  "The version of Tuatara the package NAME in DIRECTORY says it needs in its
entry file's header (NEEDED-VERSION-IN), read and never run; NIL when it
says none, or has no entry."
  (let ((entry (package-entry directory name file)))
    (when entry
      (let ((octets (ignoring-errors-reported ("reading the header of ~a" entry)
                                              (tuatara.io:call-with-file (tuatara.io:native (merge-pathnames entry
                                                                                                             directory))
                                                                         (lambda (fd path size)
                                                                           (tuatara.io:read-fd-start fd path size
                                                                                                     *package-header-octets*))))))
        (and octets (needed-version-in (tuatara.text:lenient-string octets)))))))

(defun tuatara-fits-p (needed)
  "True when this Tuatara is the version NEEDED or later, each number
compared in turn, one left out counted 0; true too when this one's version
is not known."
  (let ((have (tuatara.io:version-numbers (tuatara:version))))
    (or (null have) (tuatara.io:version-at-least-p have (tuatara.io:version-numbers needed)))))

(defun unfitting-version (directory name file)
  "The version of Tuatara the package NAME in DIRECTORY needs when it is newer
than this one, or NIL when it fits."
  (let ((needed (package-needed-version directory name file)))
    (and needed (not (tuatara-fits-p needed)) needed)))

(define-condition package-needs-newer-tuatara (error)
  ((name :initarg :name :reader package-needs-newer-tuatara-name)
   (needed :initarg :needed :reader package-needs-newer-tuatara-needed))
  (:report (lambda (condition stream)
             (format stream "~a needs Tuatara ~a or later, and this is ~a"
                     (package-needs-newer-tuatara-name condition) (package-needs-newer-tuatara-needed condition)
                     (tuatara:version))))
  (:documentation "Signalled when the package NAME says it needs Tuatara NEEDED or later, newer
than this one: it is not loaded."))

(setf (documentation 'package-needs-newer-tuatara-name 'function) "The name of the package not loaded."
      (documentation 'package-needs-newer-tuatara-needed 'function) "The version of Tuatara it needs.")

(defvar *loaded-package-directories* '()
  "The directory each package loaded this session was loaded from, each (NAME
DIRECTORY FILE), FILE its entry when named, the one loaded last first: never
deleted while the editor runs.")

(defun load-package-entry (directory name file)
  "Load the package called NAME in DIRECTORY by its entry (PACKAGE-ENTRY), FILE
when given, its files compiled when they have changed since; one that needs
a newer Tuatara (PACKAGE-NEEDED-VERSION) is an error, nothing of it loaded.
Returns its entry's file."
  (let ((entry (or (package-entry directory name file)
                   (refuse "The package ~a has no ~:[~a.lisp or load.lisp~;~:*~a~] in ~a" name file name
                           (abbreviated-path (tuatara.io:native directory)))))
        (needed (unfitting-version directory name file)))
    (when needed
      (error 'package-needs-newer-tuatara :name name :needed needed))
    (let ((*package-directory* directory) (*loading-package-name* name) (*package-check* nil))
      (prog1 (tuatara-user:load-package-file entry)
        (setf *loaded-package-directories*
              (cons (list name (tuatara.io:native directory) file)
                    (remove name *loaded-package-directories* :key #'first :test #'string=)))))))

(defun load-package-directory (directory name file &optional then)
  "Load the package called NAME in DIRECTORY by its entry (LOAD-PACKAGE-ENTRY),
its files compiled, then what follows (LOADING-PACKAGE): its :config, the
packages waiting for it, the time it took said. Returns its entry's file, or
NIL when it failed or loads in the background, its files compiled apart
\(COMPILE-PACKAGE-APART); THEN, when given, is called with that once it has
loaded, and what it returns returned (LOADING-PACKAGE)."
  (loading-package name (abbreviated-path (tuatara.io:native directory))
                   (lambda () (load-package-entry directory name file))
                   then
                   (lambda (job) (compile-package-apart job directory name file))))

(defun package-question (name where directory then &optional signer)
  "Ask whether to compile and load the package NAME from WHERE, kept in
DIRECTORY, SIGNER saying who signed it when given, answered by one key pressed
once the question is shown (READ-ANSWER-KEY), where it is from and what each
key does shown below it (CALL-WITH-DETAILS-SHOWN): y calls THEN, v lists its
directory, opened safely, n leaves it, as \\`C-g' does (\"not now\"), asked again
at the next start. Any other key says which keys answer and asks again.
THEN at once with *PACKAGE-ASK-BEFORE-LOADING* NIL."
  (if (not *package-ask-before-loading*)
      (funcall then)
      (case (handler-case
                (call-with-details-shown
                 (format nil "The package ~a, from ~a~@[, ~a~].~%~%y  compiles and loads it now~%n  leaves it, ~
                              asked about again at the next start~%v  shows its source, opened safely~%"
                         name (abbreviated-path where) signer)
                 (lambda ()
                   (read-answer-key (format nil "Compile and load the package ~a? (y, n or v) " name)
                                    '(#\y #\n #\v) :fresh t)))
              (quit () nil))
        (#\y (funcall then))
        (#\v (rimu-show-directory (tuatara.io:native directory)
                                  :safe t
                                  :root (tuatara.io:native directory)
                                  :back (current-buffer))
             (message (substitute-command-keys "~a is shown; load-package asks again at the next start, ~
                                                \\[package-update] now" :for-format t)
                      name))
        (#\n (message "The package ~a is not loaded" name))
        (t (message "The package ~a is not loaded now; load-package asks again at the next start" name)))))
