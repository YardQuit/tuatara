;;;; lisp/packages.lisp --- Packages from anywhere: a folder of a git repository, or of your disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package is a folder of Lisp files, NAME/NAME.lisp its entry (load.lisp
;;;; too), in a git repository anyone hosts, one package a repository or a
;;;; folder each, or in a folder of your own disk. There is no index: the init
;;;; file names where each comes from,
;;;;
;;;;   (package-source "mine" "https://example.org/my-packages.git")
;;;;   (load-package "example" :source "mine")
;;;;
;;;; and the first time Tuatara fetches only what it needs, in the background:
;;;; which commit the version asked for is (the repository's newest tag by date
;;;; unless said), then that commit alone and the package's folder alone. It is
;;;; kept in your data folder and loaded from there at every start, never
;;;; fetched again until M-x package-update. Before a package is first compiled
;;;; it asks, its source offered to read; then its files are compiled into the
;;;; cache once a version and loaded. A package has a Lisp package of its own
;;;; that uses TUATARA, the names lisp/api.lisp lists, and makes its settings
;;;; with DEFINE-PACKAGE-SETTING.
;;;;
;;;; A package says which Tuatara it needs in a line of its entry file's
;;;; header, ;;;; Needs: Tuatara 0.91.0, read without running it: a version
;;;; needing a newer one is never taken, an older one that fits taken in its
;;;; place. Who signed the commit or tag fetched is checked by git, and the
;;;; signer of a source's first version taken is remembered: a later version
;;;; signed by another, or unsigned when it was signed, is refused until you
;;;; trust the source again. The version taken and the one before it are
;;;; kept, the one before loaded again when the new one fails to load.

(in-package #:tuatara.defaults)

(defvar *package-ask-before-loading* t
  "True to be asked before a package is first compiled and loaded: one fetched
the first time, and again when an update brings another commit; one of a
folder of yours the first time only, what you change in it later being yours.
y loads it, n leaves it, v shows its source first. NIL to load them without
asking.")

(defvar *package-sources* '()
  "The places packages come from, each (NAME . WHERE): WHERE a git
repository's address or a folder of your disk (PACKAGE-SOURCE sets them).")

(defvar *packages-kept* 500
  "How many fetched packages' versions are remembered.")

(defun package-line (entry)
  "The line of the packages file for ENTRY, (KEY COMMIT TAG ACCEPTED PREVIOUS
PREVIOUS-TAG SIGNER), COMMIT being a dash for a folder of your disk, PREVIOUS
the commit taken before it and kept to fall back on, or NIL, and SIGNER who
signed COMMIT as git checked it when it was taken, a key's fingerprint or
\"unsigned\", or NIL when not known."
  (destructuring-bind (key commit tag accepted &optional previous previous-tag signer) entry
    (format nil "~a~c~a~c~a~c~:[no~;yes~]~@[~a~]" (escape-path key) #\Tab commit #\Tab (escape-path (or tag "")) #\Tab accepted
            (and (or previous signer)
                 (format nil "~c~a~c~a~@[~c~a~]" #\Tab (or previous "") #\Tab (escape-path (or previous-tag ""))
                         (and signer #\Tab) signer)))))

(defun parse-package-line (line)
  "The entry LINE of the packages file writes, (KEY COMMIT TAG ACCEPTED
PREVIOUS PREVIOUS-TAG SIGNER), or NIL for a line that makes no sense."
  (let ((fields (uiop:split-string line :separator '(#\Tab))))
    (when (member (length fields) '(4 6 7))
      (destructuring-bind (key commit tag accepted &optional previous previous-tag signer) fields
        (flet ((tag (text) (let ((tag (unescape-path text))) (and (plusp (length tag)) tag)))
               (commit-p (text) (cl-ppcre:scan "^(?:[0-9a-f]{7,64}|-)$" text)))
          (let ((previous (and previous (plusp (length previous)) previous)))
            (and (plusp (length key)) (commit-p commit) (or (null previous) (commit-p previous))
                 (or (null signer) (string= signer "unsigned") (cl-ppcre:scan "^(?:SHA256:[A-Za-z0-9+/=]+|[0-9A-F]{40,64})$" signer))
                 (list (unescape-path key) commit (tag tag) (string= accepted "yes")
                       previous (and previous (tag previous-tag)) signer))))))))

(define-state-list *fetched-packages* "packages" 'parse-package-line 'package-line '*packages-kept*
  :header (format nil "# The packages fetched, the latest first: the package, its source and the~%~
                       # version asked for; the commit fetched; its tag; whether it was let load;~%~
                       # the commit taken before it and its tag, kept to fall back on.~%")
  :path-of #'first)

(defun fetched-package (key)
  "What is remembered of the package KEY names (PACKAGE-KEY), (KEY COMMIT TAG
ACCEPTED), or NIL."
  (state-list-find *fetched-packages* key))

(defun tuatara-user:package-source (name where &key signers)
  "Name WHERE, a git repository's address or a folder of your disk (~/ for
your home folder), as the source NAME, for load-package's :source:
\(package-source \"mine\" \"https://example.org/my-packages.git\").

SIGNERS, a list of the keys' fingerprints that sign its versions (an SSH
key's SHA256:..., a GPG key's 40 hexadecimal digits), takes from it only
versions signed by one of them, from the first; without it, whoever signed
the first version taken is remembered and asked of every later one. An
address of http:// or git:// is allowed only by :plain t on the
load-package of each package fetched from it."
  (unless (and (stringp name) (plusp (length name)))
    (error "package-source takes a name and an address or a folder: ~s ~s" name where))
  (check-package-where where)
  (note-package-source-terms where signers)
  (setf *package-sources* (acons name where (remove name *package-sources* :key #'car :test #'string=)))
  where)

(defvar *package-signers* '()
  "The signers the init file names for sources, each (WHERE . FINGERPRINTS):
only versions signed by one of FINGERPRINTS are taken from WHERE.")

(defvar *package-plain-allowed* '()
  "The packages the init file allows to be fetched from an address of http://
or git:// by :plain t on their load-package, each (NAME . WHERE).")

(defun signer-fingerprint (text)
  "The key's fingerprint TEXT writes, as it is compared: spaces taken out, and
a GPG key's hexadecimal digits in capitals; an SSH key's (SHA256:...) as
written."
  (let ((bare (remove #\Space text)))
    (if (every (lambda (c) (digit-char-p c 16)) bare) (string-upcase bare) bare)))

(defun note-package-source-terms (where signers)
  "Remember for the source WHERE the SIGNERS its versions must be signed by,
as package-source and load-package give them; an error for SIGNERS that is no
list of fingerprints."
  (unless (listp signers)
    (error "A source's :signers are a list of keys' fingerprints: ~s" signers))
  (dolist (signer signers)
    (let ((bare (and (stringp signer) (remove #\Space signer))))
      (cond ((and bare (cl-ppcre:scan "^(?:SHA256:[A-Za-z0-9+/]{43}=?|[0-9A-Fa-f]{40}|[0-9A-Fa-f]{64})$" bare)))
            ((and bare (cl-ppcre:scan "^(?:0x)?[0-9A-Fa-f]{8,16}$" bare))
             (error "~s in :signers is a GPG key's ID: name its whole fingerprint, the 40 hexadecimal digits gpg --fingerprint shows for the key itself (pub), not a subkey's" signer))
            (t (error "~s in :signers is no key's fingerprint: an SSH key's SHA256:... (ssh-keygen -lf KEY.pub) or a GPG key's 40 hexadecimal digits (gpg --fingerprint)" signer)))))
  (when signers
    (setf *package-signers* (acons where (mapcar #'signer-fingerprint signers)
                                   (remove where *package-signers* :key #'car :test #'string=)))))

(defun package-signers-named (where)
  "The fingerprints the init file names for the source WHERE, or NIL."
  (cdr (assoc where *package-signers* :test #'string=)))

(defun plain-transport-p (where)
  "True when WHERE is an address of http:// or git://, whose transport is not encrypted."
  (and (cl-ppcre:scan "(?i)^(?:http|git)://" where) t))

(defun plain-source-refusal (name where)
  "Why the package NAME is not fetched from WHERE, an address over plain
transport that no load-package allowed with :plain t, with how to allow it;
NIL when WHERE needs no allowing or is allowed."
  (when (and (plain-transport-p where) (not (member (cons name where) *package-plain-allowed* :test #'equal)))
    (format nil "The package ~a is not fetched from ~a: over http:// or git:// anyone on the way can read and change it. ~
                 Use https://, ssh or a folder of your disk, or allow this source by adding :plain t to its load-package"
            name where)))

(defun package-where (source)
  "Where SOURCE, a source's name (PACKAGE-SOURCE) or itself an address or a
folder, is."
  (or (cdr (assoc source *package-sources* :test #'string=)) source))

(defun local-source-p (where)
  "True when WHERE is a folder of your disk rather than a repository's address."
  (and (plusp (length where)) (find (char where 0) "/~")))

(defun check-package-where (where)
  "Signal an error unless WHERE, a package's source, is an address or a folder
named whole (/ or ~/ first): one named from the folder Tuatara starts in would
be another folder at each start, and one that starts with - would be read by
git as an option."
  (unless (and (stringp where) (plusp (length where)))
    (error "A package's source is an address or a folder: ~s" where))
  (when (char= (char where 0) #\-)
    (error "A package's source cannot start with -: ~s" where))
  (when (char= (char where 0) #\.)
    (error "A package's folder is named whole, from / or ~~/: ~s" where))
  (when (cl-ppcre:scan "^[a-z][a-z0-9+.-]*://[^/@]*@" where)
    (error "A package's address cannot hold a name or a password (@): ~s" where))
  (when (cl-ppcre:scan "(?i)^ftps?://" where)
    (error "A package is fetched over https://, ssh or from a folder of your disk, not over ftp: ~s" where)))

(defun package-key (name where version)
  "What a package called NAME from WHERE, VERSION asked for, is remembered by."
  (format nil "~a ~a ~a" name where (or version "release")))

(defvar *packages-directory* nil
  "The folder Tuatara keeps the packages it fetches in, in place of
~/.local/share/tuatara/packages/ ($XDG_DATA_HOME/tuatara/packages/); NIL for
that standard place. As *CACHE-DIRECTORY*: made by you, and while it is not
there no package is fetched or loaded from it.")

(defun packages-directory ()
  "Where fetched packages are kept: *PACKAGES-DIRECTORY* when set
\(TUATARA.IO:CHOSEN-DIRECTORY), else tuatara/packages/ in your data folder."
  (tuatara.io:native (tuatara.io:chosen-directory '*packages-directory*
                                                  (merge-pathnames "tuatara/packages/" (tuatara.io:data-home)))))

(defun package-kept-folder (where name)
  "The folder the package NAME of the repository at WHERE is kept in, a folder
for each commit of it fetched."
  (format nil "~a~a/~a/" (packages-directory) (tuatara.io:path-file-name where) name))

(defun package-folder-for (where name commit)
  "The folder a commit of the repository at WHERE is kept in for the package NAME."
  (format nil "~a~a/" (package-kept-folder where name) commit))

;;; Declaring packages: load-package

(defvar *package-folder* nil
  "The folder of the package being loaded, for LOAD-PACKAGE-FILE.")

(defvar *loaded-packages* '()
  "The names of the packages loaded this session.")

(defvar *fetching-packages* '()
  "The names of the packages being fetched.")

(defvar *package-failures* '()
  "Why packages were not fetched or loaded this session, each (NAME . WORDS).")

(defvar *refused-packages* '()
  "The names of the packages refused this session for using names this
Tuatara lacks; why is in *PACKAGE-FAILURES*.")

(defvar *declared-packages* '()
  "The packages load-package names this session, each (NAME SOURCE VERSION
FILE FOLDER); the first named first.")

(defvar *left-out-packages* '()
  "The packages a load-package left out this session (:disabled, or :if
giving NIL) that name a source, each (NAME SOURCE): what is kept of them is
still wanted.")

(defun declared-package (name)
  "The package called NAME as load-package named it this session, (NAME SOURCE
VERSION FILE FOLDER), or NIL."
  (find name *declared-packages* :key #'first :test #'string=))

(defvar *package-configs* '()
  "The :config forms of each package named, each (NAME . FORMS), evaluated once
it has loaded.")

(defvar *deferred-packages* '()
  "The names of the packages named to be loaded later (:defer, :commands,
:modes) that are not loaded yet.")

(defvar *packages-waiting* '()
  "The packages waiting for another to load first (:after), each (OTHER NAME
. FUNCTION), FUNCTION loading NAME, the one waiting.")

(defvar *commands-after-load* '()
  "Commands run before their package had loaded, waiting to run once it has,
each (NAME COMMAND . FUNCTION): FUNCTION runs COMMAND of the package NAME.")

(defvar *start-package-times* nil
  "While the init file is read, the packages loaded meanwhile, each (NAME .
MILLISECONDS), after :START; NIL otherwise.")

(defparameter *load-package-keywords* '(:source :version :file :folder :signers :plain :disabled :if :defer :after :commands :modes :init :config)
  "What load-package takes after a package's name, each followed by its value
or, for :commands, :modes, :init and :config, by any number of them.")

(defun load-package-options (options)
  "OPTIONS, what load-package was given after the name, as a property list of
each keyword and the list of what followed it; an error naming one it does
not take, or a second value after one that takes one: another load-package
there is said to be inside this one, a parenthesis left open before it."
  (let ((parsed '()) (key nil) (items '()))
    (flet ((close-key ()
             (when key
               (when (and (rest items) (not (member key '(:commands :modes :init :config))))
                 (let ((extra (first (last items 2))))
                   (if (and (consp extra) (symbolp (first extra)) (string-equal (first extra) "LOAD-PACKAGE"))
                       (error "load-package ~s is inside this one, after ~(~s~): a parenthesis is left open before it"
                              (second extra) key)
                       (error "load-package's ~(~s~) takes one value, not also ~s" key extra))))
               (setf (getf parsed key) (reverse items)))))
      (dolist (item options)
        (if (and (keywordp item) (or (member item *load-package-keywords*) (null key) (not (member key '(:init :config)))))
            (progn
              (unless (member item *load-package-keywords*)
                (error "load-package takes ~{~(~s~)~^, ~}, not ~(~s~)" *load-package-keywords* item))
              (close-key)
              (setf key item items '()))
            (progn
              (unless key (error "load-package takes a keyword after the name, not ~s" item))
              (push item items))))
      (close-key))
    parsed))

(defmacro tuatara-user:load-package (name &rest options)
  "Load the package NAME (a string: \"example\"), or set up a part of
Tuatara, the settings and keys of it together. What follows the name, each
keyword with its value, or for the last four with any number of them:

:source  where it comes from: a source's name (package-source), a git
         repository's address or a folder of your disk. Without it, NAME is
         \"tuatara\", a mode's name (\"org\" for org-mode) or a name of
         your own for a group of settings (\"writing\"), which load nothing
         and run :init and :config at once. A name a letter or two from a
         package's or a mode's is taken for a mistyping of it: an error,
         saying which.
:version the tag, branch or commit fetched: the repository's newest tag by
         date when not given or \"release\", its newest commit for \"latest\"
         or when not given and it has no tag yet.
:folder  its folder in the repository or source folder, when not NAME.
:file    its first file, when not NAME.lisp or load.lisp.
:signers the fingerprints of the keys its versions must be signed by, a
         list (as package-source takes them): only those are taken.
:plain   t to allow an address of http:// or git://, whose transport is not
         encrypted: refused without it.
:disabled t to leave the whole block out, as if it were not there.
:if      a form: the block is left out when it gives NIL, as on another
         machine, (:if (string= (machine-instance) \"laptop\")).
:defer   t to load it once the editor has started rather than while the init
         file is read.
:after   another package's or group's name: loaded, or for a group its
         :config run, only once that one has. There is no :before: A
         before B is B :after A.
:commands commands of it, loaded when one of them is first run (\\[execute-extended-command] offers
         them meanwhile, a key may be bound to them).
:modes   modes, loaded when a buffer is first in one of them (\"csv\" for csv-mode).
:init    forms evaluated at once, before it loads.
:config  forms evaluated once it has loaded: its settings and keys (bind-key
         with :mode for keys of a mode).

For a group of settings, \"tuatara\" or a mode's block, nothing loads: :init
runs at once, and :defer, :after, :commands and :modes say when its :config
runs, at once when none is given.

  (load-package \"example\" :source \"mine\" :commands example-run
    :config
    (setf *example-arguments* \"-H\")
    (bind-key \"C-c r\" 'example-run :mode 'csv))

A package of a repository is fetched in the background the first time, and
asked about before it is compiled and loaded (*package-ask-before-loading*);
it is loaded from where it is kept at every start after. Each package loaded
is said in *Messages* with the time it took, and how many at the end of the
init file; one that fails is said, the rest of the init file read all the
same. One whose code uses a function or variable this Tuatara lacks is
refused, naming them, and what of it loaded is taken out
\(LOAD-PACKAGE-FILE). Returns the file loaded, or NIL when it is loaded
later or nothing is."
  (let ((parsed (load-package-options options)))
    (flet ((one (key) (first (getf parsed key))))
      `(declare-package ,name :left-out ,(cond ((one :disabled) :disabled)
                                               ((getf parsed :if) `(and (not ,(one :if)) :if)))
                              :source ,(one :source) :version ,(one :version) :file ,(one :file)
                              :folder ,(one :folder) :signers ,(one :signers) :plain ,(one :plain)
                              :defer ,(one :defer) :after ,(one :after)
                              :commands ',(getf parsed :commands) :modes ',(getf parsed :modes)
                              :init ',(getf parsed :init) :config ',(getf parsed :config)))))

(defun run-package-forms (name what forms)
  "Evaluate FORMS, the :init or :config (WHAT) of the package NAME, in the init
file's package, each by itself, the compiler's warnings unsaid (a setting
named before its package defines it): one that fails is said, with why, and
the others still evaluated."
  (let ((*package* (find-package :tuatara-user)))
    (dolist (form forms)
      (handler-case (handler-bind ((warning #'muffle-warning)) (eval form))
        (error (e)
          (message "~a's ~(~s~): ~a failed: ~a" name what (let ((*print-length* 4) (*print-level* 2)) (prin1-to-string form)) e))))))

(defun package-loaded-p (name)
  "True when the package NAME has loaded this session."
  (member name *loaded-packages* :test #'string=))

(defun package-loaded (name)
  "What follows the package NAME's loading: it counts as loaded, its :config is
evaluated, the packages waiting for it (:after) load, and the commands of it
run before it had loaded run (*COMMANDS-AFTER-LOAD*)."
  (pushnew name *loaded-packages* :test #'string=)
  (run-package-forms name :config (cdr (assoc name *package-configs* :test #'string=)))
  (let ((waiting (remove name *packages-waiting* :key #'car :test-not #'string=)))
    (setf *packages-waiting* (remove name *packages-waiting* :key #'car :test #'string=))
    (dolist (entry (reverse waiting)) (funcall (cddr entry))))
  (let ((commands (remove name *commands-after-load* :key #'first :test-not #'string=)))
    (setf *commands-after-load* (remove name *commands-after-load* :key #'first :test #'string=))
    (dolist (entry (reverse commands)) (defer-action (cddr entry)))))

(defvar *package-compiling* nil
  "How a package's files that changed are compiled as it loads: NIL, here, at
once; :APART, here in a process compiling it apart from the editor
\(COMPILE-PACKAGE-CHILD), saying so on its output the first time; :REFUSED,
not at all, an error, while the editor waits for the loading of what was
compiled apart (START-PACKAGE-LOAD).")

(defun package-load-ended (name where start loaded failure)
  "What follows the loading of the package NAME from WHERE, begun at START
\(internal real time): when it loaded (LOADED, the file it was loaded by),
PACKAGE-LOADED, and the milliseconds it took said in *Messages*; when FAILURE,
the error that ended it, said on the echo row too, the package not counted as
loaded. Returns LOADED, or NIL when it failed."
  (flet ((failed (e)
           (push (cons name (princ-to-string e)) *package-failures*)
           (if (typep e '(or package-lacks-names package-load-stopped))
               (progn (pushnew name *refused-packages* :test #'string=)
                      (message "~a" e))
               (message "Loading package ~a...failed: ~a" name e))
           nil))
    (if failure
        (failed failure)
        (handler-case
            (progn
              (package-loaded name)
              (let ((ms (round (tuatara.runtime:milliseconds-since start))))
                (when *start-package-times* (push (cons name ms) (cdr *start-package-times*)))
                (log-message (format nil "Loading package ~a from ~a...done (~d ms)" name where ms)))
              loaded)
          (error (e) (failed e))))))

(defun load-in-background-p ()
  "True when a package loaded now is compiled and loaded in the background
\(START-PACKAGE-LOAD): the editor's session runs, this is its thread, and
the init file is not being read, which goes on only once its packages have
loaded."
  (and (running-editor) (not (away-from-editor-p)) (null *start-package-times*)))

(defun loading-package (name where load &optional then compile)
  "Load the package NAME by calling LOAD, then what follows (PACKAGE-LOADED),
timed: said in *Messages*, WHERE being where it is loaded from, with the
milliseconds it took; a failure said on the echo row too, the package not
counted as loaded (PACKAGE-LOAD-ENDED). THEN, a function, is called with the
file loaded, or NIL when it failed, and true as a second argument when it
was stopped on purpose; what it returns is returned. While the editor's
session runs (LOAD-IN-BACKGROUND-P) the package is loaded in the background
\(START-PACKAGE-LOAD): COMPILE, a function of the job, compiles it first
apart from the editor, which goes on with your keys meanwhile; LOAD then
runs while the editor waits, and THEN is called on the editor's thread once
it has loaded: NIL is returned at once, and without THEN, once it has
loaded, that it was compiled and loaded is said. Without THEN, what LOAD
returned is returned, or NIL."
  (let ((start (get-internal-real-time))
        (given then)
        (then (or then #'identity)))
    (if (load-in-background-p)
        (progn (start-package-load name where load compile
                                   (lambda (loaded failure compiled stopped)
                                     (let ((loaded (package-load-ended name where start loaded failure)))
                                       (when (and loaded compiled (not given))
                                         (message "The package ~a is compiled and loaded" name))
                                       (if given (funcall then loaded stopped) (funcall then loaded)))))
               nil)
        (multiple-value-bind (loaded failure) (handler-case (funcall load) (error (e) (values nil e)))
          (funcall then (package-load-ended name where start loaded failure))))))

(defun package-command-stub (command name start)
  "Make COMMAND, a command of the package NAME not loaded yet, one that calls
START to load it and then runs the command the package has of that name: at
once when the package loads at once, else once it has (asked about first, or
fetched), or saying why it could not load; a command of that name there is
already is left as it is."
  (unless (find-command (symbol-name command))
    (let (stub)
      (labels ((real ()
                 (let ((other (loop for s being the hash-keys of *commands*
                                    when (and (not (eq s command)) (string= (symbol-name s) (symbol-name command)))
                                      return s)))
                   (cond (other (gethash other *commands*))
                         ((not (eq (symbol-function command) stub)) (symbol-function command)))))
               (run-real ()
                 (let ((real (real)))
                   (when real
                     (setf (symbol-function command) real
                           (gethash command *commands*) real)
                     (funcall real)
                     t))))
        (setf stub (lambda ()
                     (funcall start)
                     (let ((failure (cdr (assoc name *package-failures* :test #'string=))))
                       (cond ((run-real))
                             (failure (message "~(~a~) is of the package ~a, which could not be loaded: ~a" command name failure))
                             (t (setf *commands-after-load*
                                      (cons (list* name command #'run-real) (remove command *commands-after-load* :key #'second)))
                                (message "~(~a~) runs once the package ~a has loaded~:[~; (being fetched)~]"
                                         command name (member name *fetching-packages* :test #'string=)))))))
        (setf (symbol-function command) stub
              (documentation command 'function)
              (format nil "A command of the package ~a: running it loads the package (its load-package's :commands), then runs the package's own." name)
              (gethash command *commands*) stub)))))

(defun arrange-package-load (name load &key defer after commands modes)
  "Have LOAD, a function loading the package NAME, called once: at once; with
AFTER, once the package AFTER has loaded; with COMMANDS or MODES, when one of
those commands is first run or a buffer is first in one of those modes; with
DEFER alone, once the editor has started."
  (let* ((started nil)
         (start (lambda ()
                  (unless started
                    (setf started t
                          *deferred-packages* (remove name *deferred-packages* :test #'string=))
                    (if (and after (not (package-loaded-p after)))
                        (push (list* after name load) *packages-waiting*)
                        (funcall load))))))
    (cond ((or commands modes)
           (pushnew name *deferred-packages* :test #'string=)
           (dolist (command commands) (package-command-stub command name start))
           (dolist (mode modes)
             (add-mode-hook (mode-named mode) (lambda (buffer) (declare (ignore buffer)) (funcall start)))))
          (defer
           (pushnew name *deferred-packages* :test #'string=)
           (defer-action start))
          (t (funcall start)))))

(defun edit-distance (a b)
  "How many letters must be put in, taken out, changed or swapped with the one
beside them to make the string A into B (Damerau's distance, each letter
changed once at most), case ignored."
  (let ((d (make-array (list (1+ (length a)) (1+ (length b))))))
    (dotimes (i (1+ (length a))) (setf (aref d i 0) i))
    (dotimes (j (1+ (length b))) (setf (aref d 0 j) j))
    (loop for i from 1 to (length a)
          do (loop for j from 1 to (length b)
                   do (let ((cost (if (char-equal (char a (1- i)) (char b (1- j))) 0 1)))
                        (setf (aref d i j) (min (1+ (aref d (1- i) j)) (1+ (aref d i (1- j))) (+ (aref d (1- i) (1- j)) cost)))
                        (when (and (> i 1) (> j 1)
                                   (char-equal (char a (1- i)) (char b (- j 2)))
                                   (char-equal (char a (- i 2)) (char b (1- j))))
                          (setf (aref d i j) (min (aref d i j) (1+ (aref d (- i 2) (- j 2)))))))))
    (aref d (length a) (length b))))

(defun known-package-name-near (name)
  "The name of a package, of Tuatara itself or of a mode NAME is a letter or
two away from, as a mistyping of it would be, or NIL: one letter for a name
of four letters or fewer, two for a longer one."
  (let ((most (if (<= (length name) 4) 1 2)))
    (find-if (lambda (known) (<= 1 (edit-distance name known) most))
             (append (mapcar #'first *declared-packages*) (list "tuatara")
                     (mapcar (lambda (mode) (let ((text (string-downcase (symbol-name mode))))
                                              (if (uiop:string-suffix-p text "-mode") (subseq text 0 (- (length text) 5)) text)))
                             (mode-names))))))

(defun declare-package (name &key left-out source version file folder signers plain defer after commands modes init config)
  "Load the package NAME as LOAD-PACKAGE says, each keyword as it takes it,
INIT and CONFIG the forms after :init and :config; LEFT-OUT :disabled or :if
when the block is left out, said in *Messages*. Returns the file loaded, or
NIL when it is loaded later or nothing is."
  (when left-out
    (log-message (format nil "~a is left out (~(~s~))" name left-out))
    (when (and (stringp source) (plusp (length source)))
      (pushnew (list name source) *left-out-packages* :test #'equal))
    (return-from declare-package nil))
  (unless (and (stringp name) (cl-ppcre:scan "^[a-z0-9][a-z0-9-]*$" name))
    (error "A package's name is small letters, digits and -: ~s" name))
  (when (and after (not (and (stringp after) (cl-ppcre:scan "^[a-z0-9][a-z0-9-]*$" after))))
    (error "A package's :after is another package's name: ~s" after))
  (dolist (command commands)
    (unless (symbolp command) (error "A package's :commands are commands' names: ~s" command)))
  (setf modes (mapcar (lambda (mode) (if (stringp mode) (intern (string-upcase mode) :keyword) mode)) modes))
  (mapc #'mode-named modes)
  (setf *package-configs* (acons name config (remove name *package-configs* :key #'car :test #'string=)))
  (flet ((declared (entry)
           (setf *declared-packages* (append (remove name *declared-packages* :key #'first :test #'string=)
                                             (list entry))))
         (arrange (load)
           (let ((result nil))
             ;; a package still being fetched loads later: no file yet
             (arrange-package-load name (lambda () (let ((loaded (funcall load)))
                                                     (setf result (and (or (stringp loaded) (pathnamep loaded)) loaded))))
                                   :defer defer :after after :commands commands :modes modes)
             result)))
    (cond (source
           (run-package-forms name :init init)
           (unless (and (stringp source) (plusp (length source)))
             (error "A package's :source is a source's name, an address or a folder: ~s" source))
           (check-package-where (package-where source))
           (note-package-source-terms (package-where source) signers)
           (setf *package-plain-allowed* (remove name *package-plain-allowed* :key #'car :test #'string=))
           (when plain (push (cons name (package-where source)) *package-plain-allowed*))
           (when (and version (not (and (stringp version) (cl-ppcre:scan "^[^\\s~^:?*\\[\\\\]+$" version))))
             (error "A package's :version is a tag, a branch or a commit: ~s" version))
           (when (and file (not (and (stringp file) (cl-ppcre:scan "^[A-Za-z0-9._-]+\\.lisp$" file))))
             (error "A package's :file is a .lisp file of its folder: ~s" file))
           (when (and folder (not (and (stringp folder) (cl-ppcre:scan "^[A-Za-z0-9_][A-Za-z0-9._-]*$" folder))))
             (error "A package's :folder is the name of a folder in its repository or source folder: ~s" folder))
           (declared (list name source version file folder))
           (arrange (lambda () (load-package-from name source version file))))
          (t
           (let ((mode (and (string/= name "tuatara") (ignore-errors (mode-named (intern (string-upcase name) :keyword)))))
                 (near nil))
             (unless (or mode (string= name "tuatara"))
               (setf near (known-package-name-near name))
               (when near
                 (error "No package or mode called ~a: did you mean ~a?" name near)))
             (run-package-forms name :init init)
             (arrange (lambda ()
                        (let ((start (get-internal-real-time)))
                          (package-loaded name)
                          (log-message (format nil "Settings ~a (~a) set in ~d ms" name
                                               (cond (mode (string-downcase (symbol-name mode)))
                                                     ((string= name "tuatara") "Tuatara itself")
                                                     (t "your own group"))
                                               (round (tuatara.runtime:milliseconds-since start)))))
                        nil)))))))

(defun call-with-start-packages (read)
  "Call READ, which reads the init file, the packages it loads meanwhile timed;
then say in *Messages* how many loaded and in how long, how many wait to be
loaded later, and which wait for a package no load-package names."
  (let ((*start-package-times* (list :start)))
    (multiple-value-prog1 (funcall read)
      (let ((times (rest *start-package-times*)))
        (when (or times *deferred-packages* *fetching-packages*)
          (log-message (format nil "Packages: ~d loaded in ~d ms~[~:;, ~:*~d to load later~]~[~:;, ~:*~d being fetched~]"
                               (length times) (reduce #'+ times :key #'cdr)
                               (length *deferred-packages*) (length *fetching-packages*))))
        (dolist (waiting *packages-waiting*)
          (unless (or (declared-package (car waiting))
                      (member (car waiting) *deferred-packages* :test #'string=))
            (message "~a waits for ~a (:after), which no load-package before it or after it names" (second waiting) (car waiting))))))))

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

(defun package-cache ()
  "The folder packages are compiled into for this Tuatara and this SBCL; those
of other ones are deleted the first time it is asked for."
  (let* ((sbcl (format nil "~apackages/~a/" (tuatara.io:native (tuatara.io:cache-directory))
                       (tuatara.io:percent-encoded (lisp-implementation-version) :safe "._-")))
         (here (format nil "~a~a/" sbcl (tuatara-identity))))
    (unless (probe-file here)
      (dolist (other (append (ignore-errors (uiop:subdirectories sbcl)) (ignore-errors (uiop:directory-files sbcl))))
        (delete-package-folder (namestring other))))
    here))

(defun package-fasl (file)
  "Where the compiled FILE of a package is kept: in the cache, by which
Tuatara and which SBCL compile it (PACKAGE-CACHE), in folders as the file's
own are, fetched/ for a package fetched and local/ for one of a folder of yours."
  (let* ((native (tuatara.io:native file))
         (fetched (tuatara.io:native (packages-directory)))
         (inside (if (uiop:string-prefix-p fetched native)
                     (concatenate 'string "fetched/" (subseq native (length fetched)))
                     (concatenate 'string "local" native))))
    (format nil "~a~a.fasl" (package-cache) inside)))

(defun tuatara-user:package-file (name)
  "The file NAME (\"helper.sh\") of the package being loaded: in its
folder, or the folder of the file being loaded when no package is. A package
names its own files so, as where it is loaded from is the cache's."
  (merge-pathnames name (or *package-folder*
                            (and *load-truename* (make-pathname :name nil :type nil :version nil :defaults *load-truename*))
                            *default-pathname-defaults*)))

(defun compile-failure-words (report)
  "Why compiling a file failed, on one line, from REPORT, what the compiler
wrote: the error it caught (a reader error: an unclosed parenthesis, and the
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
  (let ((text (ignore-errors (and (probe-file file) (tuatara.io:file-text file)))))
    (when text
      (handler-case (with-standard-io-syntax
                      (let* ((*read-eval* nil)
                             (*package* (find-package :common-lisp))
                             (list (read-from-string text)))
                        (and (listp list) (values list t))))
        (error () nil)))))

(defvar *package-check* nil
  "While a package loads, what is gathered to see that every name its code
uses is there (a PACKAGE-CHECK); NIL otherwise.")

(defvar *loading-package-name* nil
  "The name of the package its load-package is loading, whose names are
checked (LOAD-PACKAGE-FILE); NIL when a file is loaded by itself.")

(defstruct (package-check (:constructor make-package-check (name refusal)) (:copier nil))
  "What is gathered while the package NAME loads: the Lisp packages there
before it, the names its files use without defining them (NEEDS), and the
files loaded, each (FILE . WRITE-DATE) (SOURCES); REFUSAL is the file its
refusal is kept in (BESIDE-FASL)."
  (name "" :read-only t)
  (refusal nil :read-only t)
  (before (list-all-packages) :read-only t)
  (needs '())
  (sources '()))

(setf (documentation 'package-check-name 'function) "The name of the package being checked."
      (documentation 'package-check-refusal 'function) "The file the package's refusal is kept in."
      (documentation 'package-check-before 'function) "The Lisp packages there before the package began to load."
      (documentation 'package-check-needs 'function) "The names the package's files use without defining them."
      (documentation 'package-check-sources 'function) "The package's files loaded, each (FILE . WRITE-DATE).")

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

(defun forget-package-code (own)
  "Take out of Tuatara what the code of the Lisp packages OWN put there, and
delete them: their commands and modes, the keys bound to their commands, and
their functions on the lists of functions Tuatara calls (*FILE-VIEWS* and the
rest). Faces and settings they define stay, doing nothing."
  (when own
    (flet ((theirs-p (thing)
             (let ((name (if (functionp thing) (nth-value 2 (function-lambda-expression thing)) thing)))
               (and name (symbolp name) (member (symbol-package name) own)))))
      (dolist (command (loop for name being the hash-keys of tuatara.command:*commands* collect name))
        (when (theirs-p command) (tuatara.command:forget-command command)))
      (dolist (command (loop for name being the hash-keys of *mode-commands* collect name))
        (when (theirs-p command) (remhash command *mode-commands*)))
      (dolist (mode (mode-names))
        (if (theirs-p mode)
            (forget-major-mode mode)
            (unbind-commands-if #'theirs-p (mode-keymap mode))))
      (when *global-keymap* (unbind-commands-if #'theirs-p *global-keymap*))
      (do-external-symbols (name :tuatara)
        (when (and (boundp name) (not (constantp name)))
          (let ((value (symbol-value name)))
            (when (and (consp value) (ignore-errors (list-length value)) (some #'theirs-p value))
              (setf (symbol-value name) (remove-if #'theirs-p value)))))))
    (dolist (package own)
      (dolist (user (package-used-by-list package))
        (unuse-package package user))
      (delete-package package))))

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
      (ignore-errors (write-kept-list (package-check-refusal check)
                                      (list :sources (package-check-sources check) :needs needed))))
    (tuatara.runtime:debug-report :package "~a refused, its code using names this Tuatara lacks: ~{~a~^, ~}"
                                  (package-check-name check) words)
    (forget-package-code own)
    (setf *loaded-packages* (remove (package-check-name check) *loaded-packages* :test #'string=))
    (error 'package-lacks-names :name (package-check-name check) :names words)))

(defun refuse-as-before (check)
  "Refuse at once the package CHECK is about when it was refused for names
this Tuatara lacks (BESIDE-FASL), its files are as they were then and those
names are not there still; else forget that it was. Not while a package is
compiled apart (REFUSE-PACKAGE)."
  (let ((file (and (not (member *package-compiling* '(:apart :apart-said))) (package-check-refusal check))))
    (multiple-value-bind (kept read) (read-kept-list file)
      (when read
        (let* ((needs (ignore-errors (getf kept :needs)))
               (sources (ignore-errors (getf kept :sources)))
               (missing (and (listp needs) (remove-if #'needed-name-present-p (remove-if-not #'needed-name-p needs))))
               (kept-at (or (ignore-errors (file-write-date file)) 0)))
          ;; file times are whole seconds: a file of the refusal's second may have changed after it
          (if (and missing (consp sources) (ignore-errors (list-length sources))
                   (every (lambda (source)
                            (and (consp source) (stringp (car source)) (integerp (cdr source))
                                 (eql (ignore-errors (file-write-date (car source))) (cdr source))
                                 (< (cdr source) kept-at)))
                          sources))
              (refuse-package check missing)
              (ignore-errors (delete-file file))))))))

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
                                                         ((not (typep w 'style-warning)) (push (princ-to-string w) said))))
                                                 (muffle-warning w))))
                         (let ((*error-output* report))
                           (note-package-compiling file)
                           ;; a compilation unit of its own: what it finds undefined is said at its end
                           (with-compilation-unit (:override t)
                             (compile-file file :output-file temporary :verbose nil :print nil))))))
           (unless output
             (error "~a could not be compiled~@[: ~a~]" (abbreviated-path (tuatara.io:native file))
                    (or (compile-failure-words (get-output-stream-string report)) (first said))))
           ;; the names first: a fasl is never there before the names it needs
           (write-kept-list (beside-fasl fasl "needs") needed)
           (sb-posix:rename (tuatara.io:native output) fasl)
           (when said
             (log-message (format nil "~a compiled with ~d warning~:p: ~a" (abbreviated-path (tuatara.io:native file))
                                  (length said) (first (last said)))))
           needed)
      (when (probe-file temporary) (ignore-errors (delete-file temporary))))))

(defvar *package-load-stop* nil
  "While a package loads in the background (START-PACKAGE-LOAD), a cons whose
car turns true when the loading is asked to stop: it stops before the next
file of the package (LOAD-CHECKED-FILE); NIL otherwise.")

(define-condition package-load-stopped (error)
  ((name :initarg :name :reader package-load-stopped-name))
  (:report (lambda (condition stream)
             (format stream "The package ~a is half loaded; restart Tuatara before using it (its commands are taken out)"
                     (package-load-stopped-name condition))))
  (:documentation "Signalled when the loading of the package NAME was stopped between two of
its files: what of it loaded is taken out (FORGET-PACKAGE-CODE), its
functions left behind until Tuatara starts again."))

(setf (documentation 'package-load-stopped-name 'function) "The name of the package stopped as it loaded.")

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
                    ;; be older than the file's last change, and is made again
                    (if (and read (ignore-errors (every #'needed-name-p kept))
                             (probe-file fasl) (> (file-write-date fasl) (file-write-date file)))
                        kept
                        (compile-package-file file fasl)))))
      (when check
        (push (cons (namestring file) (file-write-date file)) (package-check-sources check))
        (when needed
          (let* ((own (package-check-own check))
                 (missing (remove-if (lambda (n) (or (not (outside-name-p n check own)) (needed-name-present-p n))) needed)))
            (when missing (refuse-package check missing))))
        (setf (package-check-needs check) (union needed (package-check-needs check) :test #'equal)))
      (let ((*package* (find-package :tuatara-user))
            (*package-folder* (make-pathname :name nil :type nil :version nil :defaults file)))
        (load fasl)))))

(defun tuatara-user:load-package-file (name)
  "Compile, when it has changed since, and load the file NAME (\"example.lisp\")
of the package being loaded, or of the folder of the file being loaded: a
package's entry loads its other files so. Compiled into the cache once, a
package loads in a few milliseconds at the next start; meanwhile
PACKAGE-FILE names its files from its own folder. Its warnings are said in
*Messages*, the first of them; a file that cannot be compiled at all is an
error saying why (an unclosed parenthesis, where).

Every function and variable a package's code uses must be there once all of
its files have loaded, Tuatara's above all: a package load-package loads
that uses a name this Tuatara lacks is refused, with an error naming them,
and what of it loaded is taken out again. A name of Tuatara's or of another
library is looked for before the file using it loads; a package refused is
refused again before any of it loads, until its files or Tuatara change.

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
    (unless (probe-file file) (error "The package has no file ~a" (abbreviated-path (tuatara.io:native file))))
    (if (or *package-check* (not *loading-package-name*))
        (load-checked-file file *package-check*)
        (let* ((check (make-package-check *loading-package-name* (beside-fasl (package-fasl file) "refused")))
               (*package-check* check))
          (refuse-as-before check)
          ;; stopped between its files: what of it loaded taken out again
          (handler-bind ((package-load-stopped (lambda (e)
                                                 (declare (ignore e))
                                                 (forget-package-code (package-check-own check)))))
            (load-checked-file file check))
          (let ((missing (remove-if #'needed-name-present-p (package-check-needs check))))
            (when missing (refuse-package check missing)))))
    file))

(defun claim-package-setting (name owner)
  "The symbol of the setting NAME (a string) of the package whose Lisp package
is OWNER: in TUATARA-USER, exported, so that an init file names it as it
names Tuatara's own, and made OWNER's in place of a symbol of that name of
its own. An error when NAME does not begin with OWNER's name (*NAME-), or is
already a setting of Tuatara or of another package."
  (let ((prefix (format nil "*~a-" (package-name owner))))
    (unless (and (> (length name) (length prefix)) (string= prefix name :end2 (length prefix)))
      (error "A setting of the package ~(~a~) is named ~(~a~)...*, not ~(~a~)" (package-name owner) prefix name))
    (multiple-value-bind (symbol status) (find-symbol name :tuatara-user)
      (when (and status (not (eq status :internal))
                 (not (equal (get symbol 'package-setting) (package-name owner))))
        (error "~(~a~) is already a setting of ~a" name
               (let ((other (get symbol 'package-setting))) (if other (format nil "the package ~(~a~)" other) "Tuatara"))))
      (let ((symbol (or symbol (intern name :tuatara-user)))
            (own (find-symbol name owner)))
        (setf (get symbol 'package-setting) (package-name owner))
        (export symbol :tuatara-user)
        (unless (eq own symbol)
          (when (and own (eq (symbol-package own) owner)) (unintern own owner))
          (import symbol owner))
        symbol))))

(defmacro define-package-setting (name value documentation)
  "Define NAME, a setting of the package being loaded, with VALUE unless an
init file set it first, and DOCUMENTATION: in TUATARA-USER, where an init file
sets it by that name, and NAME in the package's own code
\(CLAIM-PACKAGE-SETTING). Its name begins with the package's (*facematch-).
It comes before the package's code that reads it, at the top of its file: a
name read before it is defined is the package's own, not the setting."
  (let ((owner (package-name *package*)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (claim-package-setting ,(symbol-name name) (find-package ,owner)))
       (defvar ,(claim-package-setting (symbol-name name) *package*) ,value ,documentation))))

(defun package-entry (folder name file)
  "The file a package called NAME in FOLDER is loaded by: FILE when given,
else NAME.lisp, else load.lisp; NIL when there is none."
  (some (lambda (entry) (and entry (probe-file (merge-pathnames entry folder)) entry))
        (list file (concatenate 'string name ".lisp") "load.lisp")))

;;; The version of Tuatara a package needs

(defparameter *package-header-octets* 16384
  "How much of a package's entry file is read for the version of Tuatara it
needs: the comment lines at its top.")

(defun needed-version-in (text)
  "The version of Tuatara the start of a package's entry file, TEXT, says it
needs, as a string: a line of the comments at its top that reads
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

(defun package-needed-version (folder name file)
  "The version of Tuatara the package NAME in FOLDER says it needs in its
entry file's header (NEEDED-VERSION-IN), read and never run; NIL when it
says none, or has no entry."
  (let ((entry (package-entry folder name file)))
    (when entry
      (let ((octets (ignore-errors
                     (tuatara.io:call-with-file (tuatara.io:native (merge-pathnames entry folder))
                                                (lambda (fd path size) (tuatara.io:read-fd-start fd path size *package-header-octets*))))))
        (and octets (needed-version-in (tuatara.text:lenient-string octets)))))))

(defun version-numbers (text)
  "The numbers of the version TEXT, as a list: 0.90.13 is (0 90 13)."
  (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[0-9]+" text)))

(defun tuatara-fits-p (needed)
  "True when this Tuatara is the version NEEDED or later, each number
compared in turn, one left out counted 0; true too when this one's version
is not known."
  (let ((have (version-numbers (tuatara:version))) (need (version-numbers needed)))
    (or (null have)
        (loop for i below (max (length have) (length need))
              for a = (or (nth i have) 0) and b = (or (nth i need) 0)
              when (> a b) return t
              when (< a b) return nil
              finally (return t)))))

(defun unfitting-version (folder name file)
  "The version of Tuatara the package NAME in FOLDER needs when it is newer
than this one, or NIL when it fits."
  (let ((needed (package-needed-version folder name file)))
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

(defvar *loaded-package-folders* '()
  "The folder each package loaded this session was loaded from, each (NAME
FOLDER FILE), FILE its entry when named, the one loaded last first: never
deleted while the editor runs.")

(defun load-package-entry (folder name file)
  "Load the package called NAME in FOLDER by its entry (PACKAGE-ENTRY), FILE
when given, its files compiled when they have changed since; one that needs
a newer Tuatara (PACKAGE-NEEDED-VERSION) is an error, nothing of it loaded.
Returns its entry's file."
  (let ((entry (or (package-entry folder name file)
                   (error "The package ~a has no ~:[~a.lisp or load.lisp~;~:*~a~] in ~a" name file name
                          (abbreviated-path (tuatara.io:native folder)))))
        (needed (unfitting-version folder name file)))
    (when needed
      (error 'package-needs-newer-tuatara :name name :needed needed))
    (let ((*package-folder* folder) (*loading-package-name* name) (*package-check* nil))
      (prog1 (tuatara-user:load-package-file entry)
        (setf *loaded-package-folders*
              (cons (list name (tuatara.io:native folder) file)
                    (remove name *loaded-package-folders* :key #'first :test #'string=)))))))

(defun load-package-folder (folder name file &optional then)
  "Load the package called NAME in FOLDER by its entry (LOAD-PACKAGE-ENTRY),
its files compiled, then what follows (LOADING-PACKAGE): its :config, the
packages waiting for it, the time it took said. Returns its entry's file, or
NIL when it failed or loads in the background, its files compiled apart
\(COMPILE-PACKAGE-APART); THEN, when given, is called with that once it has
loaded, and what it returns returned (LOADING-PACKAGE)."
  (loading-package name (abbreviated-path (tuatara.io:native folder))
                   (lambda () (load-package-entry folder name file))
                   then
                   (lambda (job) (compile-package-apart job folder name file))))

(defun package-question (name where folder then &optional signer)
  "Ask whether to compile and load the package NAME from WHERE, kept in
FOLDER, SIGNER saying who signed it when given, answered by one key pressed
once the question is shown (READ-ANSWER-KEY): y calls THEN, v lists its
folder, opened safely, n leaves it, as \\`C-g' does (\"not now\"), asked again
at the next start. Any other key says which keys answer and asks again.
THEN at once with *PACKAGE-ASK-BEFORE-LOADING* NIL."
  (if (not *package-ask-before-loading*)
      (funcall then)
      (case (handler-case (read-answer-key (format nil "Compile and load the package ~a from ~a~@[, ~a~]? (y, n, or v to view its source) "
                                                   name (abbreviated-path where) signer)
                                           '(#\y #\n #\v) :fresh t)
              (quit () nil))
        (#\y (funcall then))
        (#\v (rimu-show-directory (tuatara.io:native folder) :safe t :root (tuatara.io:native folder) :back (current-buffer))
         (message (substitute-command-keys "~a is shown; load-package asks again at the next start, \\[package-update] now" :for-format t)
                  name))
        (#\n (message "The package ~a is not loaded" name))
        (t (message "The package ~a is not loaded now; load-package asks again at the next start" name)))))

;;; Fetching

(defun git-for-packages ()
  "The git program packages are fetched with, or an error saying it is missing."
  (or (tuatara.io:find-program "git") (tuatara.io:program-missing "git" :why (format nil "packages are fetched with it: ~a" (outside-program-install-words :git)))))

(defvar *package-plain-transport* nil
  "True while a package whose load-package allows http:// or git:// (:plain
t) is fetched; else git is not let use them, even when a server sends it
there.")

(defparameter *package-git-seconds* 900
  "The most seconds one git command fetching or checking a package may take
before it is ended.")

(defun package-git-environment ()
  "What git is given when it fetches and checks packages: what it is given to
push and pull now (GIT-NETWORK-ENVIRONMENT), and besides: no hooks, no
submodules, no attributes file of yours, and none of the system's, so that
none of your own git's programs runs on a package; symbolic links in a
package checked out as small files of their target's name, never leading
outside it; git-lfs's filters, which would ask other servers, left out; ftp
never, and http:// and git:// not unless *PACKAGE-PLAIN-TRANSPORT*, however
a server redirects."
  (append (apply #'git-network-environment
                 '("core.symlinks" "false") '("filter.lfs.smudge" "") '("filter.lfs.process" "")
                 '("filter.lfs.required" "false") '("core.hooksPath" "/dev/null") '("submodule.recurse" "false")
                 '("core.attributesFile" "/dev/null") '("protocol.ftp.allow" "never") '("protocol.ftps.allow" "never")
                 (unless *package-plain-transport*
                   '(("protocol.http.allow" "never") ("protocol.git.allow" "never"))))
          (list "GIT_LFS_SKIP_SMUDGE=1" "GIT_ATTR_NOSYSTEM=1")))

(defparameter *package-attributes*
  (format nil "* -filter -text -eol -ident -working-tree-encoding~%")
  "The attributes every file of a package is checked out with, before the
repository's own: no filter program, no change of line ends, encoding or
$Id$, so that its files are written as they are.")

(defun package-git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY for a package's fetching: what it
printed, as text; an error with what it said when it fails, or when it takes
more than *PACKAGE-GIT-SECONDS*. A package's own scratch repository may fetch
the files a sparse checkout needs from its address as it goes. A repository
made by init is given *PACKAGE-ATTRIBUTES* as its own."
  (multiple-value-bind (octets ok too-large said)
      (let ((*git-safety* (remove "--no-lazy-fetch" *git-safety* :test #'string=)))
        (run-git-program (git-for-packages) directory arguments :environment (package-git-environment) :errors t
                                                                 :seconds *package-git-seconds*))
    (declare (ignore too-large))
    (unless ok
      (error "~a" (git-failure-words (first arguments) (or said ""))))
    (when (equal (first arguments) "init")
      (let ((info (format nil "~a.git/info/" (tuatara.io:native directory))))
        (tuatara.io:ensure-private-directory info)
        (tuatara.io:write-private-file (concatenate 'string info "attributes") *package-attributes*)))
    (if octets (tuatara.text:octets-string octets) "")))

(defun package-commit-name (text where)
  "TEXT, a commit's name git printed for the repository at WHERE, checked to be
one (GIT-HASH-P, 7 to 64 hexadecimal digits); an error otherwise."
  (let ((name (string-trim '(#\Newline #\Space #\Tab) text)))
    (unless (git-hash-p name :short t)
      (error "~a gave no commit's name: ~s" where (subseq name 0 (min 80 (length name)))))
    name))

(defun package-commit (directory where version)
  "The commit, and its tag or NIL, VERSION of the repository at WHERE is,
asking it from DIRECTORY, a repository of no commit made for it: \"latest\",
its main branch's newest commit; \"release\", its newest tag by date, its
name never read; NIL, the same, or its newest commit when it has no tag; else
that tag, branch or commit as written. Every commit's name is checked to be
one (PACKAGE-COMMIT-NAME)."
  (labels ((first-word (text) (first (uiop:split-string (string-trim '(#\Newline #\Space) text) :separator '(#\Space #\Tab))))
           (newest-commit ()
             (values (package-commit-name (or (first-word (package-git directory "ls-remote" where "HEAD"))
                                              (error "~a has no main branch" where))
                                          where)
                     nil))
           (newest-tag ()
             ;; the tags' commits alone, to find the newest by date
             (package-git directory "fetch" "--quiet" "--depth=1" "--filter=tree:0" where "+refs/tags/*:refs/tags/*")
             (let* ((line (string-trim '(#\Newline #\Space)
                                       (package-git directory "for-each-ref" "--sort=-creatordate" "--count=1"
                                                    "--format=%(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end) %(refname:strip=2)"
                                                    "refs/tags")))
                    (space (position #\Space line)))
               (values (package-commit-name (subseq line 0 space) where) (subseq line (1+ space))))))
    (cond ((equal version "latest") (newest-commit))
          ((member version '(nil "release") :test #'equal)
           (cond ((plusp (length (string-trim '(#\Newline #\Space) (package-git directory "ls-remote" "--tags" where))))
                  (newest-tag))
                 ((null version) (newest-commit))
                 (t (error "~a has no tagged version; :version \"latest\" takes its newest commit" where))))
          (t
           (let* ((listed (package-git directory "ls-remote" where
                                       (format nil "refs/tags/~a" version) (format nil "refs/tags/~a^{}" version)
                                       (format nil "refs/heads/~a" version)))
                  (lines (tuatara.text:split-words listed '(#\Newline)))
                  (peeled (find-if (lambda (line) (search "^{}" line)) lines)))
             (cond (lines (values (package-commit-name (first-word (or peeled (first lines))) where)
                                  (and (search "refs/tags/" (or peeled (first lines))) version)))
                   ((git-hash-p version) (values version nil))
                   ((git-hash-p version :short t)
                    (error "~a has no tag or branch called ~a, and a commit is fetched by its whole name, ~
                            40 hexadecimal digits (git log shows it)" where version))
                   (t (error "~a has no tag, branch or commit called ~a" where version))))))))

(defun delete-package-folder (folder)
  "Delete FOLDER, with all in it, or a file, when it is inside the folders
packages are kept or compiled in, nothing it holds followed when it is a link (DELETE-TREE);
an error for any other folder."
  (let ((native (tuatara.io:native folder)))
    (unless (or (tuatara.io:path-within-p native (tuatara.io:native (packages-directory)))
                (tuatara.io:path-within-p native (format nil "~apackages/" (tuatara.io:native (tuatara.io:cache-directory)))))
      (error "~a is no folder of the packages': not deleted" (abbreviated-path native)))
    (tuatara.io:delete-tree native)))

(defparameter *package-most-octets* (* 512 1024 1024)
  "The most octets a package may take on disk while it is fetched, what git
keeps of its history and its files together: a fetch that grows past it is
ended and its files deleted.")

(define-condition package-too-large (error)
  ((where :initarg :where :reader package-too-large-where))
  (:report (lambda (condition stream)
             (format stream "~a takes more than ~a: the package was not fetched"
                     (package-too-large-where condition) (tuatara.io:octets-words *package-most-octets*))))
  (:documentation "Signalled when a package fetched from WHERE grows past *PACKAGE-MOST-OCTETS*."))

(setf (documentation 'package-too-large-where 'function) "Where the package too large was fetched from.")

(defun call-within-package-size (folder where thunk)
  "Call THUNK, which fetches into FOLDER on the thread of a job, and look at
how much FOLDER takes every second meanwhile: past *PACKAGE-MOST-OCTETS*, the
program the job runs is ended and PACKAGE-TOO-LARGE signalled, also when
THUNK has ended by then."
  (let* ((job tuatara.runtime:*job*) (most *package-most-octets*)
         (too-large nil)
         (too-large-p (lambda () (> (or (rimu-tree-size folder) 0) most)))
         (timer (sb-ext:make-timer (lambda ()
                                     (when (and (not too-large) (funcall too-large-p))
                                       (setf too-large t)
                                       (let ((process (and job (tuatara.runtime:job-process job))))
                                         (when process (ignore-errors (sb-ext:process-kill process 15))))))
                                   :thread t)))
    (flet ((refuse () (error 'package-too-large :where where)))
      (sb-ext:schedule-timer timer 1 :repeat-interval 1)
      (multiple-value-prog1
          (unwind-protect (handler-bind ((error (lambda (e) (declare (ignore e)) (when too-large (refuse)))))
                            (funcall thunk))
            (sb-ext:unschedule-timer timer))
        (when (or too-large (funcall too-large-p)) (refuse))))))

(defparameter *package-versions-tried* 10
  "How many older versions of a package are tried, the newest first, when the
one asked for needs a newer Tuatara than this one: the first that fits is
taken.")

(defparameter *package-history-depth* 100
  "How many commits of a repository's main branch are looked through for an
older version of a package when its newest commit needs a newer Tuatara.")

(defun package-entry-paths (name file)
  "The paths in its repository the package NAME may be entered by: FILE,
NAME.lisp or load.lisp, in its folder (PACKAGE-REPOSITORY-FOLDER) or at the
top."
  (let ((inside (package-repository-folder name)))
    (loop for top in (list (format nil "~a/" inside) "")
          append (loop for entry in (list file (format nil "~a.lisp" name) "load.lisp")
                       when entry collect (concatenate 'string top entry)))))

(defun package-older-versions (probe where version tag name file)
  "The versions of the repository at WHERE older than the one VERSION found,
the newest first, each (COMMIT . TAG), at most *PACKAGE-VERSIONS-TRIED*,
asked from PROBE, the repository it was found in (PACKAGE-COMMIT). For TAG,
the newest tag, the tags made before it; for the newest commit, the newest
commit of each earlier state of the package NAME's entry file
\(PACKAGE-ENTRY-PATHS) on the main branch, within *PACKAGE-HISTORY-DEPTH*
commits; for a tag, branch or commit named, none."
  (flet ((lines (text) (tuatara.text:split-words text '(#\Newline))))
    (cond ((not (member version '(nil "release" "latest") :test #'equal)) '())
          (tag
           (loop for line in (lines (package-git probe "for-each-ref" "--sort=-creatordate"
                                                 (format nil "--count=~d" (1+ *package-versions-tried*))
                                                 "--format=%(if)%(*objectname)%(then)%(*objectname)%(else)%(objectname)%(end) %(refname:strip=2)"
                                                 "refs/tags"))
                 for space = (position #\Space line)
                 when (and space (git-hash-p (subseq line 0 space) :short t) (string/= (subseq line (1+ space)) tag))
                   collect (cons (subseq line 0 space) (subseq line (1+ space)))))
          (t
           (package-git probe "fetch" "--quiet" (format nil "--depth=~d" *package-history-depth*) "--filter=blob:none" where "HEAD")
           (let ((commits (remove-if-not #'git-hash-p
                                         (lines (package-git probe "rev-list" "--first-parent"
                                                             (format nil "--max-count=~d" *package-history-depth*) "FETCH_HEAD"))))
                 (found '()))
             (dolist (change (lines (apply #'package-git probe "log" "--first-parent" "--format=%H"
                                           (format nil "--max-count=~d" *package-history-depth*) "FETCH_HEAD" "--"
                                           (package-entry-paths name file))))
               (let ((older (second (member change commits :test #'string=))))
                 (when (and older (< (length found) *package-versions-tried*))
                   (pushnew (cons older nil) found :key #'car :test #'string=))))
             (nreverse found))))))

(defun built-on (probe where floor versions)
  "Those of VERSIONS, each (COMMIT . TAG) of the repository at WHERE, that are
FLOOR or built on it (git merge-base --is-ancestor), asked from PROBE, the
repository they were found in, their history fetched without files first;
none when FLOOR is not in that history."
  (package-git probe "fetch" "--quiet" "--filter=tree:0" "--depth=2147483647" where "HEAD" "+refs/tags/*:refs/tags/*")
  (remove-if-not (lambda (version)
                   (or (string= (car version) floor)
                       (handler-case (progn (package-git probe "merge-base" "--is-ancestor" floor (car version)) t)
                         (error () nil))))
                 versions))

(defun fetch-package (name where version &key file floor walk-p signature-of)
  "Fetch the package called NAME, VERSION of the repository at WHERE: that
commit alone, and its folder (PACKAGE-REPOSITORY-FOLDER) alone when it has
one, else the whole of it; a repository that gives no file by itself after
its folders, that commit whole; never more than *PACKAGE-MOST-OCTETS*
\(CALL-WITHIN-PACKAGE-SIZE), finding the commit as well as checking it out.
When that version needs a newer Tuatara than this one (UNFITTING-VERSION,
FILE its entry when given), older ones are tried, the newest first
\(PACKAGE-OLDER-VERSIONS), and the first that fits is kept; one that does not
fit is not. None older than FLOOR, the commit taken now, is tried, nor any
not built on it; and none at all when WALK-P, a function of the folder the
version is in, its commit and its tag, says no (its signer not trusted).
Returns the folder it is kept in, its commit, its tag, and, when the version
asked for was passed over, (VERSION NEEDED UNTRUSTED): its tag or commit, the
Tuatara it needs, and true when WALK-P said no; the folder NIL when no version
tried fits. SIGNATURE-OF, a function of a repository holding the version
taken, its commit and its tag (as fetched now: for a commit kept before, the
tag is fetched apart, into a repository of its own), gives the fifth value."
  (let* ((scratch (format nil "~a.fetching-~a/" (packages-directory) (tuatara.io:random-name)))
         (probe (format nil "~aprobe/" scratch))
         (inside-name (package-repository-folder name)))
    (tuatara.io:ensure-private-directory probe)
    (labels ((check-out (files commit tag filter)
               (call-within-package-size scratch where (lambda () (check-out-within files commit tag filter))))
             (check-out-within (files commit tag filter)
               (tuatara.io:ensure-private-directory files)
               (package-git files "init" "--quiet")
               (apply #'package-git files "fetch" "--quiet" "--depth=1"
                      (append (and filter '("--filter=blob:none"))
                              (list where (if tag (format nil "refs/tags/~a" tag) commit))))
               (let ((inside (string-trim '(#\Newline #\Space)
                                          (package-git files "ls-tree" "-d" "--name-only" commit inside-name))))
                 (when (string= inside inside-name)
                   (package-git files "sparse-checkout" "set" "--no-cone" (format nil "/~a/" inside-name))))
               (package-git files "checkout" "--quiet" commit))
             (tag-repository (commit tag)
               ;; the tag as fetched now, apart from the folder kept, within the size watched
               (let ((repository (format nil "~atag-~a/" scratch commit)))
                 (call-within-package-size scratch where
                                           (lambda ()
                                             (tuatara.io:ensure-private-directory repository)
                                             (package-git repository "init" "--quiet")
                                             (package-git repository "fetch" "--quiet" "--depth=1" "--filter=blob:none"
                                                          where (format nil "refs/tags/~a" tag))))
                 repository))
             (kept (commit tag)
               ;; the folder COMMIT is kept in, or NIL and the Tuatara it needs; and the repository to check it in
               (let ((folder (package-folder-for where name commit)))
                 (if (probe-file folder)
                     (let ((needed (unfitting-version (package-folder-in folder name) name file))
                           (looked (if tag (tag-repository commit tag) folder)))
                       (if needed (values nil needed looked) (values folder nil looked)))
                     (let ((files (format nil "~afiles-~a/" scratch commit)))
                       (handler-case (check-out files commit tag t)
                         (package-too-large (e) (error e))
                         (error ()
                           (delete-package-folder files)
                           (setf files (format nil "~awhole-~a/" scratch commit))
                           (check-out files commit tag nil)))
                       (let ((needed (unfitting-version (package-folder-in files name) name file)))
                         (cond (needed
                                (values nil needed files))
                               (t
                                (tuatara.io:ensure-private-directory (directory-namestring (string-right-trim "/" folder)))
                                ;; a fetch of the same commit beside this one may have kept it first
                                (handler-case (sb-posix:rename (string-right-trim "/" files) (string-right-trim "/" folder))
                                  (sb-posix:syscall-error (e) (unless (probe-file folder) (error e))))
                                (values folder nil folder))))))))
             (taken (folder commit tag looked passed)
               (values folder commit tag passed (and signature-of (funcall signature-of looked commit tag)))))
      (unwind-protect
           (multiple-value-bind (commit tag)
               (call-within-package-size scratch where
                                         (lambda ()
                                           (package-git probe "init" "--quiet")
                                           (package-commit probe where version)))
             (multiple-value-bind (folder needed looked) (kept commit tag)
               (cond
                 (folder (taken folder commit tag looked nil))
                 ((and walk-p (not (funcall walk-p looked commit tag)))
                  (values nil nil nil (list (or tag (subseq commit 0 7)) needed t)))
                 (t
                  (let ((passed (list (or tag (subseq commit 0 7)) needed nil)))
                    (dolist (older (call-within-package-size scratch where
                                                             (lambda ()
                                                               (let ((older (package-older-versions probe where version tag name file)))
                                                                 (if floor (built-on probe where floor older) older))))
                                   (values nil nil nil passed))
                      (multiple-value-bind (folder needed looked) (kept (car older) (cdr older))
                        (declare (ignore needed))
                        (when folder
                          (return (taken folder (car older) (cdr older) looked passed))))))))))
        (when (probe-file scratch)
          (delete-package-folder scratch))))))

(defun package-repository-folder (name)
  "The folder the package NAME is in within its repository or source folder:
its load-package's :folder, else NAME."
  (or (fifth (declared-package name)) name))

(defun package-folder-in (folder name)
  "The folder of the package NAME within FOLDER, a repository's or a source
folder: its own folder there (PACKAGE-REPOSITORY-FOLDER) when it has one, else
FOLDER itself."
  (let ((inside (format nil "~a~a/" folder (package-repository-folder name))))
    (if (probe-file inside) inside folder)))

(defun package-folder-of (name where entry)
  "The folder the package NAME from WHERE is loaded from (PACKAGE-FOLDER-IN):
within WHERE when it is a folder of your disk, else within the commit ENTRY,
what is remembered of it, says was fetched; NIL when none was."
  (cond ((local-source-p where)
         (package-folder-in (directory-name where) name))
        ((and entry (string/= (second entry) "-"))
         (package-folder-in (package-folder-for where name (second entry)) name))))

;;; Who signed a version fetched, and whom a source is trusted by

(defun trust-line (entry)
  "The line of the trust file for ENTRY, (WHERE SIGNER NAME KEY)."
  (destructuring-bind (where signer name key) entry
    (format nil "~a~c~a~c~a~c~a" (escape-path where) #\Tab signer #\Tab (escape-path (or name "")) #\Tab (or key ""))))

(defun parse-trust-line (line)
  "The entry LINE of the trust file writes, (WHERE SIGNER NAME KEY), or NIL
for a line that makes no sense: SIGNER a key's fingerprint or \"unsigned\",
NAME the signer's name or NIL, KEY an SSH signer's key or NIL; SIGNER is
key: and a GPG key's fingerprint for a source whose first version was signed
by a key your keyring lacks."
  (let ((fields (uiop:split-string line :separator '(#\Tab))))
    (when (= 4 (length fields))
      (destructuring-bind (where signer name key) fields
        (and (plusp (length where))
             (or (string= signer "unsigned") (cl-ppcre:scan "^(?:SHA256:[A-Za-z0-9+/=]+|[0-9A-F]{40,64}|key:(?:[0-9A-F]{40}|[0-9A-F]{64}))$" signer))
             (or (zerop (length key)) (cl-ppcre:scan "^[a-z0-9@.-]{1,64} [A-Za-z0-9+/]+={0,2}$" key))
             (list (unescape-path where) signer (let ((name (unescape-path name))) (and (plusp (length name)) name))
                   (and (plusp (length key)) key)))))))

(define-state-list *package-trust* "package-trust" 'parse-trust-line 'trust-line '*packages-kept*
  :header (format nil "# The sources of packages trusted, the latest first: the address; who signed~%~
                       # the versions taken from it, a key's fingerprint, or unsigned; the signer's~%~
                       # name; an SSH signer's key. M-x package-trust-again forgets a source's.~%")
  :path-of #'first)

(defun package-signers-file (where)
  "The allowed signers file git checks the SSH signatures of the source WHERE
against, kept beside its packages."
  (format nil "~a~a/.allowed-signers" (packages-directory) (tuatara.io:path-file-name where)))

(defun write-package-signers-file (where)
  "Write the allowed signers file of the source WHERE (PACKAGE-SIGNERS-FILE)
anew: the SSH key trusted for it (*PACKAGE-TRUST*), or nothing. Returns its
name."
  (let ((file (package-signers-file where))
        (trusted (state-list-find *package-trust* where)))
    (tuatara.io:ensure-private-directory (directory-namestring file))
    (tuatara.io:write-private-file file (if (fourth trusted) (format nil "trusted namespaces=\"git\" ~a~%" (fourth trusted)) ""))
    file))

(defun signature-block (text kind)
  "Where the signature of the git object TEXT, of KIND :commit or :tag, lies
in it, as two values, its start and end: a commit's on its header's line
gpgsig (or gpgsig-sha256) and those after it, before the blank line that
ends the header, never another header's (mergetag); a tag's the last block of its
text, where git appends it. NIL when it has none there."
  (let ((header-end (or (search (format nil "~%~%") text) (length text))))
    (multiple-value-bind (begin end)
        (if (eq kind :commit)
            (let* ((line (cl-ppcre:scan "(?m)^gpgsig(?:-sha256)? -----BEGIN " text :end header-end))
                   (begin (and line (search "-----BEGIN " text :start2 line :end2 header-end))))
              (values begin (and begin (search "-----END " text :start2 begin :end2 header-end))))
            (let ((begin (search "-----BEGIN " text :from-end t)))
              (values begin (and begin (search "-----END " text :start2 begin)))))
      (when end
        (values begin (or (position #\Newline text :start end) (length text)))))))

(defun ssh-signature-key (text kind)
  "The public key the SSH signature of the git object TEXT, of KIND :commit
or :tag (SIGNATURE-BLOCK), was made with, as an allowed signers file writes
it (TYPE BASE64), read from the signature's own octets; NIL when it has
none."
  (multiple-value-bind (begin end) (signature-block text kind)
    (let ((mark "-----BEGIN SSH SIGNATURE-----"))
      (when (and begin (string= mark text :start2 begin :end2 (min (length text) (+ begin (length mark)))))
        (let* ((octets (tuatara.io:base64-octets (tuatara.text:string-octets (subseq text (+ begin (length mark)) (search "-----END" text :start2 begin :end2 end)))))
               (size (tuatara.io:octets-be octets 10 4)))
          (when (and size (tuatara.io:octets-at-p octets "SSHSIG") (<= (+ 14 size) (length octets)))
            (let* ((blob (subseq octets 14 (+ 14 size)))
                   (type-size (tuatara.io:octets-be blob 0 4))
                   (type (and type-size (<= (+ 4 type-size) (length blob)) (tuatara.io:octets-ascii blob 4 (+ 4 type-size)))))
              (and type (cl-ppcre:scan "^[a-z0-9@.-]{1,64}$" type)
                   (format nil "~a ~a" type (tuatara.io:base64-string blob))))))))))

(defun ssh-signature-p (text kind)
  "True when the signature of the git object TEXT, of KIND :commit or :tag, is an SSH one."
  (let ((begin (signature-block text kind)))
    (and begin (string= "-----BEGIN SSH SIGNATURE-----" text :start2 begin :end2 (min (length text) (+ begin 29))))))

(defun plain-name (text)
  "TEXT, a signer's name as a key gives it, on one line and without the
characters that change how text is laid out (right to left, hidden ones), so
that it shows as it is; NIL for NIL or nothing left."
  (let ((name (tuatara.text:one-line (and text (remove-if (lambda (c) (eq (ignore-errors (sb-unicode:general-category c)) :cf)) text)))))
    (and name (plusp (length name)) (tuatara.display:shorten-string name 80))))

(defun commit-signature (fields object)
  "The signature of a commit, as PACKAGE-SIGNATURE gives it, from FIELDS,
what git's log said of it (%G?, %GF, %GP, %GS and %GK), and OBJECT, the
commit's object; :ERROR when it is signed but git could not say by whom. A
GPG signature whose key your keyring lacks is :UNCHECKED, its key's
fingerprint left to MISSING-KEY."
  (destructuring-bind (&optional (grade "") subkey primary signer key-id &rest more) fields
    (declare (ignore more))
    (let ((ssh (ssh-signature-p object :commit))
          (signed (signature-block object :commit)))
      (cond ((member grade '("G" "U" "X" "Y") :test #'string=)
             (list :status :good
                   :fingerprint (signer-fingerprint (if (plusp (length primary)) primary subkey))
                   :subkey (and (plusp (length subkey)) (signer-fingerprint subkey))
                   :name (and (not ssh) (plain-name signer))
                   :key (and ssh (ssh-signature-key object :commit))))
            ((member grade '("B" "R") :test #'string=) (list :status :bad))
            ((and (string= grade "E") (not ssh) (cl-ppcre:scan "^[0-9A-Fa-f]{16,64}$" key-id))
             (list :status :unchecked))
            ((and (string= grade "N") (not signed)) (list :status :none))
            (t (list :status :error))))))

(defun gnupg-status (said name)
  "The fields of gpg's status line NAME in SAID, what it wrote, each at the start of a line; NIL when there is none."
  (let ((found (nth-value 1 (cl-ppcre:scan-to-strings (format nil "(?m)^\\[GNUPG:\\] ~a(?: ([^\\n]*))?$" name) said))))
    (and found (or (tuatara.text:split-words (aref found 0)) (list "")))))

(defun tag-signature (said object)
  "The signature of a tag, as PACKAGE-SIGNATURE gives it, from SAID, what git
verify-tag --raw said of it, and OBJECT, the tag's object."
  (if (ssh-signature-p object :tag)
      (let ((good (nth-value 1 (cl-ppcre:scan-to-strings "(?m)^Good \"git\" signature (?:for \\S+ )?with \\S+ key (SHA256:[A-Za-z0-9+/=]+)$" said))))
        (if good
            (list :status :good :fingerprint (aref good 0) :key (ssh-signature-key object :tag))
            (list :status :bad)))
      (let ((valid (gnupg-status said "VALIDSIG"))
            (good (or (gnupg-status said "GOODSIG") (gnupg-status said "EXPSIG") (gnupg-status said "EXPKEYSIG")))
            (missing (or (gnupg-status said "ERRSIG") (gnupg-status said "NO_PUBKEY"))))
        (cond ((or (gnupg-status said "BADSIG") (gnupg-status said "REVKEYSIG")) (list :status :bad))
              ((and valid good)
               (list :status :good :fingerprint (signer-fingerprint (or (nth 9 valid) (first valid)))
                     :subkey (signer-fingerprint (first valid))
                     :name (plain-name (format nil "~{~a~^ ~}" (rest good)))))
              ((and missing (missing-key said)) (list :status :unchecked :key-id (missing-key said)))
              (t (list :status :error))))))

(defun missing-key (said)
  "The fingerprint of the key that made a GPG signature your keyring lacks,
from SAID, gpg's status lines (ERRSIG's seventh field, the issuer's
fingerprint, 40 or 64 hexadecimal digits): the signing key itself, a
subkey or the primary key; NIL when gpg did not give it, as for an old
signature naming only a 64-bit ID, which is too short to hold to."
  (let ((fingerprint (seventh (gnupg-status said "ERRSIG"))))
    (and fingerprint (cl-ppcre:scan "^(?:[0-9A-Fa-f]{40}|[0-9A-Fa-f]{64})$" fingerprint)
         (signer-fingerprint fingerprint))))

(defun tag-object-names-p (object tag)
  "True when OBJECT, a tag's object, names itself TAG in its header (its tag
line, before the blank line that ends the header), whatever its message says."
  (and (cl-ppcre:scan (format nil "(?m)^tag ~a$" (cl-ppcre:quote-meta-chars tag)) object
                      :end (or (search (format nil "~%~%") object) (length object)))
       t))

(defparameter *package-check-seconds* 120
  "The most seconds checking who signed a package's version may take, git
with gpg or ssh-keygen, before it is ended and the version not taken.")

(defun package-signature (folder commit tag where)
  "Who signed COMMIT of the repository at WHERE, kept in FOLDER: the tag TAG
it was fetched by now when that is signed (its object, FETCH_HEAD, naming
TAG and COMMIT), else the commit. Checked by git, with gpg and your keyring
for a GPG signature, with ssh-keygen and the allowed signers file of the key
trusted for WHERE for an SSH one (WRITE-PACKAGE-SIGNERS-FILE); nothing of
the repository is run. A list: :STATUS, :GOOD, :BAD (it does not hold, or
its key is revoked), :UNCHECKED (by a GPG key your keyring lacks, its
fingerprint :KEY-ID), :NONE (unsigned) or :ERROR (signed, but git could not say by
whom); :FINGERPRINT (the primary key's), :SUBKEY, :NAME and for an SSH key
:KEY of a good one, the key seen to have made the signature."
  (labels ((git (allowed &rest arguments)
             (multiple-value-bind (octets ok too-large said)
                 (run-git-program (git-for-packages) folder arguments
                                  :environment (git-network-environment (list "gpg.ssh.allowedSignersFile" allowed))
                                  :errors t :seconds *package-check-seconds*)
               (declare (ignore ok too-large))
               (values (if octets (tuatara.text:octets-string octets) "") (or said ""))))
           (trimmed (text) (string-trim '(#\Newline #\Space) text))
           (check (allowed tag-object)
             (if tag-object
                 (tag-signature (nth-value 1 (git allowed "verify-tag" "--raw" "FETCH_HEAD")) tag-object)
                 (commit-signature (uiop:split-string (trimmed (git allowed "log" "-1" "--format=%G?%x00%GF%x00%GP%x00%GS%x00%GK" commit))
                                                      :separator (list (code-char 0)))
                                   (git allowed "cat-file" "commit" commit)))))
    (let* ((allowed (write-package-signers-file where))
           (object (and tag (string= "tag" (trimmed (git allowed "cat-file" "-t" "FETCH_HEAD")))
                        (git allowed "cat-file" "tag" "FETCH_HEAD")))
           (tag-object (and object (signature-block object :tag) object)))
      (when (and tag-object
                 (not (and (tag-object-names-p tag-object tag)
                           (string= commit (trimmed (git allowed "rev-parse" "--verify" "--quiet" "FETCH_HEAD^{commit}"))))))
        (return-from package-signature (list :status :error :why "its tag object names another tag or commit")))
      (let ((signature (check allowed tag-object)))
        (when (and (eq (getf signature :status) :unchecked) (not (getf signature :key-id)))
          (let ((key (missing-key (nth-value 1 (git allowed "verify-commit" "--raw" commit)))))
            (setf signature (if key (list :status :unchecked :key-id key) (list :status :error)))))
        ;; the key read from the signature is kept only once git has seen it make the signature
        (when (getf signature :key)
          (let ((alone (format nil "~a-check-~a" (package-signers-file where) (tuatara.io:random-name))))
            (unwind-protect
                 (progn
                   (tuatara.io:write-private-file alone (format nil "trusted namespaces=\"git\" ~a~%" (getf signature :key)))
                   (let ((again (check alone tag-object)))
                     (unless (and (eq (getf again :status) :good) (equal (getf again :fingerprint) (getf signature :fingerprint))
                                  (if tag-object
                                      (search "signature for trusted with" (nth-value 1 (git alone "verify-tag" "--raw" "FETCH_HEAD")))
                                      (string= "G" (trimmed (git alone "log" "-1" "--format=%G?" commit)))))
                       (setf (getf signature :key) nil))))
              (ignore-errors (delete-file alone)))))
        signature))))

(defun signer-words (signature)
  "Who signed a version, as SIGNATURE (PACKAGE-SIGNATURE) says, in words."
  (let ((fingerprint (getf signature :fingerprint)))
    (ecase (getf signature :status)
      (:good (if (getf signature :name)
                 (format nil "signed by ~a (key ~a)" (getf signature :name) fingerprint)
                 (format nil "signed by the key ~a" fingerprint)))
      (:unchecked (format nil "signed by the GPG key ~a, which your keyring lacks, so the signature cannot be checked: ~
                               import that key (M-x import-keys), then fetch it again (M-x package-update)"
                          (getf signature :key-id)))
      (:bad "signed, but its signature does not hold: it was changed after it was signed, or its key is revoked")
      (:error (format nil "signed, but who signed it could not be checked~@[ (~a)~]" (getf signature :why)))
      (:none "unsigned"))))

(defun trusted-signer-words (trusted)
  "Who TRUSTED, an entry of *PACKAGE-TRUST*, says signed the versions taken, in words."
  (cond ((string= (second trusted) "unsigned") "no one (unsigned)")
        ((uiop:string-prefix-p "key:" (second trusted))
         (format nil "the GPG key ~a, not yet in your keyring" (subseq (second trusted) 4)))
        ((third trusted) (format nil "~a (key ~a)" (third trusted) (second trusted)))
        (t (format nil "the key ~a" (second trusted)))))

(defun package-trust-verdict (where signature)
  "What becomes of a version of the source WHERE that SIGNATURE
\(PACKAGE-SIGNATURE) says who signed, as three values: :TAKE, :ASK (its
first version: whoever signed it is asked about, as the version is) or
:REFUSE; who signed it in words, or why it is refused; and the entry of
*PACKAGE-TRUST* to remember once it is taken (for one refused, to remember
all the same), or NIL. With the signers the init file names (:signers),
only a version one of them signed is taken. Else the signer of the first
version taken is remembered: later ones signed by that key are taken, and
others refused, unsigned ones too; a source whose first version was
unsigned has its unsigned versions taken, and the first signed one taken
makes its signer the one remembered. A GPG signature whose key your keyring
lacks is refused until the key is imported, its fingerprint remembered for
a source's first version so that only that key (or the key it is a subkey
of) is taken then. A signature
that does not hold, or one git could not check, is refused always."
  (let* ((status (getf signature :status))
         (fingerprint (getf signature :fingerprint))
         (good (eq status :good))
         (words (signer-words signature))
         (remember (and good (list where fingerprint (getf signature :name) (getf signature :key))))
         (named (package-signers-named where))
         (trusted (state-list-find *package-trust* where))
         (pinned-id (and trusted (uiop:string-prefix-p "key:" (second trusted)) (subseq (second trusted) 4))))
    (cond ((member status '(:bad :error)) (values :refuse words nil))
          (named (cond ((and good (member fingerprint named :test #'string=)) (values :take words remember))
                       ((and good (member (getf signature :subkey) named :test #'equal))
                        (values :refuse (format nil "~a; its :signers name the key's signing subkey, ~a: name the key's own fingerprint, ~a"
                                                words (getf signature :subkey) fingerprint)
                                nil))
                       (t (values :refuse (format nil "~a, by none of the signers its :signers name" words) nil))))
          ((eq status :unchecked)
           (values :refuse words (and (null trusted) (list where (format nil "key:~a" (getf signature :key-id)) nil nil))))
          (pinned-id (if (and good (or (string= fingerprint pinned-id) (equal (getf signature :subkey) pinned-id)))
                         (values :take words remember)
                         (values :refuse (format nil "~a, and its first version was signed by ~a" words (trusted-signer-words trusted)) nil)))
          ((null trusted) (values :ask words (or remember (list where "unsigned" nil nil))))
          ((string= (second trusted) "unsigned") (values :take words remember))
          ((and good (string= fingerprint (second trusted))) (values :take words nil))
          (t (values :refuse (format nil "~a, and the versions taken before were signed by ~a" words (trusted-signer-words trusted))
                     nil)))))

;;; Keeping the version taken and the one before it

(defun prune-package-versions (name where)
  "Delete what is kept and compiled of the versions of the package NAME from
WHERE that are no longer wanted: all but the one taken and the one before it
of each version asked for (*FETCHED-PACKAGES*), and the one loaded this
session, never deleted under the editor. Another editor running beside this
one may have loaded a version deleted here: it goes on running what it
loaded, and fetches that version again when it next needs it."
  (let* ((prefix (format nil "~a ~a " name where))
         (keep (loop for entry in (state-list-entries *fetched-packages*)
                     when (uiop:string-prefix-p prefix (first entry))
                       collect (second entry)
                       and when (fifth entry) collect (fifth entry)))
         (loaded (second (assoc name *loaded-package-folders* :test #'string=))))
    (dolist (folder (ignore-errors (uiop:subdirectories (package-kept-folder where name))))
      (let ((native (tuatara.io:native folder))
            (commit (car (last (pathname-directory folder)))))
        (unless (or (not (git-hash-p commit :short t))
                    (member commit keep :test #'string=)
                    (and loaded (tuatara.io:path-within-p loaded native)))
          (let ((compiled (directory-namestring (package-fasl (concatenate 'string native "x")))))
            (when (probe-file compiled) (delete-package-folder compiled)))
          (delete-package-folder native))))))

(defvar *packages-in-hand* '()
  "The packages being fetched or checked, whose version waits to be decided
on (asked about), or being compiled and loaded in the background
\(START-PACKAGE-LOAD), each (NAME . JOB): no other fetch or check of one
starts meanwhile.")

(defvar *package-job-deciding* nil
  "The fetch or check whose version is being decided on (PACKAGE-FETCHED), or NIL.")

(defun load-version (name where folder file before &optional then)
  "Load the package NAME from FOLDER, a version of WHERE; when it fails and
BEFORE, what was remembered of the version taken before it (as
*FETCHED-PACKAGES* has it), names one that is kept, that one is remembered
again as the one taken, said, and loaded in its place once who signed it is
checked against the trust in force now (START-PACKAGE-CHECK). Gives
:TAKEN and the file loaded, :PREVIOUS when the version before it is being
checked, or NIL: to THEN, a function of them, when given, once it has
loaded (a loading stopped on purpose falls back on nothing), which may be later, in the background (LOAD-PACKAGE-FOLDER), and
returns what THEN returns; else returns them, or NIL when it loads later."
  (load-package-folder
   folder name file
   (lambda (loaded &optional stopped)
     (multiple-value-call (or then #'values)
       (if loaded
           (values :taken loaded)
           (let ((back (and before (not stopped) (package-folder-of name where before))))
             (when (and back (probe-file back))
               (state-list-put *fetched-packages* (list before))
               (message "The package ~a could not be loaded (~a): the version before it, ~a, is loaded in its place once who signed it is checked"
                        name (cdr (assoc name *package-failures* :test #'string=))
                        (or (third before) (subseq (second before) 0 (min 7 (length (second before))))))
               (when *package-job-deciding*
                 (let-package-go *package-job-deciding*))
               (start-package-check name where nil file (first before) before :quiet t)
               :previous)))))))

(defun refuse-package-version (name where words &optional remember)
  "Refuse the version of the package NAME fetched from WHERE, WORDS saying
why: said, with how to take it all the same; the version loaded now kept,
the one refused deleted; REMEMBER, an entry of *PACKAGE-TRUST*, remembered
when given."
  (when remember
    (state-list-put *package-trust* (list remember)))
  (let* ((loaded (package-loaded-p name))
         (text (format nil "The package ~a from ~a is refused: ~a" name where words)))
    (unless loaded (push (cons name text) *package-failures*))
    (message "~a. ~:[It is not loaded~;The version loaded now is kept~]; ~a" text loaded
             (if (package-signers-named where)
                 "its :signers in the init file say whose versions are taken"
                 (format nil "~a trusts its source anew, as the first time" (substitute-command-keys "\\[package-trust-again]"))))
    (prune-package-versions name where)))

;;; Fetching in the background

(defstruct (package-fetch (:include tuatara.runtime:job) (:constructor make-package-fetch (name where version file key))
                          (:copier nil))
  "The package NAME being fetched from WHERE, VERSION asked for, FILE its
entry when given, KEY what it is remembered by; once done, its FOLDER,
COMMIT and TAG, PASSED the version passed over for a newer Tuatara and the
one it needs (FETCH-PACKAGE), and SIGNATURE who signed it
\(PACKAGE-SIGNATURE). A package compiled and loaded in the background
\(START-PACKAGE-LOAD) has LOAD, the function loading it, and THEN, called
with the file it was loaded by and the error that ended it once it has
loaded; meanwhile LOADED and FAILURE keep those, and COMPILED is true once a
file of it is compiled."
  name where version file key folder commit tag passed signature load then loaded failure compiled)

(setf (documentation 'package-fetch-p 'function) "True when OBJECT is a PACKAGE-FETCH.")

(tuatara.runtime:define-job-words package-fetch
  :doing (if (package-fetch-load job) "Compiling and loading a package" "Fetching a package")
  :verb (if (package-fetch-load job) "load the package" "fetch the package")
  :description (format nil "~:[Fetching~;Compiling and loading~] the package ~a" (package-fetch-load job) (package-fetch-name job))
  :left-words (if (package-fetch-load job) "the package not loaded" "the package not fetched")
  :at-exit :quiet)

(defmethod tuatara.runtime:job-holds-queue-p ((job package-fetch))
  "NIL: the jobs of a buffer queued behind it need not wait for a package's fetching."
  nil)

(defun take-package-in-hand (job &key quiet)
  "Hold the package of JOB in hand (*PACKAGES-IN-HAND*) and return true; NIL
when a fetch or check of it runs already, its question waits for an
answer or it is being loaded, said on the echo row unless QUIET (a load at
the start, or by one of its commands or modes, which the version being
decided on serves), else in *Messages* alone."
  (let ((name (package-fetch-name job)))
    (if (assoc name *packages-in-hand* :test #'string=)
        (let ((words (format nil "~a: a fetch or check of it is already running, or waits for an answer, or it is being loaded" name)))
          (if quiet (log-message words) (message "~a" words))
          nil)
        (progn (push (cons name job) *packages-in-hand*)
               t))))

(defun let-package-go (job)
  "Let the package JOB holds in hand go (TAKE-PACKAGE-IN-HAND), when JOB still holds it."
  (setf *packages-in-hand* (remove job *packages-in-hand* :key #'cdr)))

(defun start-package-job (job work thread &key quiet)
  "Start JOB, a fetch or check of a package, in the background on a thread
called THREAD, WORK its function: its package held in hand until its version
is decided on, and counted as being fetched until the job ends. NIL, nothing
started, when the package is in hand already (said unless QUIET:
TAKE-PACKAGE-IN-HAND); let go again when the job cannot be started."
  (when (take-package-in-hand job :quiet quiet)
    (let ((name (package-fetch-name job)) (started nil))
      (pushnew name *fetching-packages* :test #'string=)
      (unwind-protect
           (progn (tuatara.runtime:start-job-thread job work :name thread)
                  (setf started t)
                  job)
        (unless started
          (let-package-go job)
          (setf *fetching-packages* (remove name *fetching-packages* :test #'string=)))))))

(defun start-package-load (name where load compile then)
  "Load the package NAME, of WHERE, in the background: a job calls COMPILE,
a function of it, which compiles the package apart from the editor
\(COMPILE-PACKAGE-APART), the editor going on with your keys meanwhile; then
LOAD, the editor waiting between two commands all the while it runs
\(CALL-WITH-EDITOR-HELD), so that no command runs with the package part
loaded, nor compiles any of it there (*PACKAGE-COMPILING* :REFUSED); when it
could not be compiled apart, what is left to compile is compiled as it
loads, the editor waiting for that too. The
package is held in hand meanwhile (*PACKAGES-IN-HAND*), taken over from the
fetch or check deciding on it; while another fetch, check or loading holds
it, nothing starts, said (TAKE-PACKAGE-IN-HAND). Once it has loaded, THEN
is called on the editor's thread with the file it was loaded by, or NIL, the
error that ended it, or NIL, true when a file of it was compiled, and true
when it was stopped on purpose. Returns the job, or NIL."
  (let ((job (make-package-fetch name where nil nil nil))
        (deciding *package-job-deciding*))
    (setf (package-fetch-load job) load
          (package-fetch-then job) then)
    (when (if (and deciding (eq (cdr (assoc name *packages-in-hand* :test #'string=)) deciding))
              (progn (setf (cdr (assoc name *packages-in-hand* :test #'string=)) job) t)
              (take-package-in-hand job))
      (tuatara.runtime:start-job-thread
       job
       (lambda (job)
         (multiple-value-bind (loaded failure)
             (handler-case
                 (let ((apart (and compile (funcall compile job))))
                   (tuatara.runtime:job-check-stop)
                   (let ((stop (list nil)))
                     (call-with-editor-held (lambda () (let ((*package-compiling* (if apart :refused nil))
                                                             (*package-load-stop* stop))
                                                         (funcall load)))
                                            :words (format nil "Loading the package ~a" name)
                                            :stop (lambda () (setf (car stop) t)))))
               (error (e) (values nil e)))
           (setf (package-fetch-loaded job) loaded
                 (package-fetch-failure job) failure)))
       :name "tuatara package load"))))

(defun package-load-taken-in (job)
  "Finish the loading JOB did in the background (START-PACKAGE-LOAD): its
package let go, and its THEN called with what it loaded, the error that
ended it or why it stopped, whether a file of it was compiled, and whether
it was stopped on purpose."
  (let-package-go job)
  (let ((status (package-fetch-status job)))
    (funcall (package-fetch-then job)
             (and (eq status :done) (package-fetch-loaded job))
             (case status
               (:done (package-fetch-failure job))
               (:stopped "it was stopped")
               (t (or (tuatara.runtime:job-error job) "it ended unexpectedly")))
             (package-fetch-compiled job)
             (or (eq status :stopped) (typep (package-fetch-failure job) 'package-load-stopped)))))

;;; Compiling a package apart

(defparameter *package-compiling-line* ";;; tuatara: compiling"
  "What a process compiling a package apart prints on a line of its own as it
begins compiling a file of it (COMPILE-PACKAGE-CHILD).")

(defparameter *package-child-said-lines* 40
  "The most lines kept of what a process compiling a package apart said, the
last ones, to say why it failed.")

(defun note-package-compiling (file)
  "As FILE, of a package, is about to be compiled: an error while the editor
waits for a package to load (*PACKAGE-COMPILING* :REFUSED), for FILE changed
after the package was compiled apart; said on the output the first time in
a process compiling a package apart (:APART)."
  (case *package-compiling*
    (:refused (error "~a changed while the package was being compiled; it loads as changed the next time it is loaded"
                     (abbreviated-path (tuatara.io:native file))))
    (:apart (format t "~&~a~%" *package-compiling-line*)
     (finish-output)
     (setf *package-compiling* :apart-said))))

(defun package-request-text (folder name file token)
  "What a process compiling the package NAME in FOLDER apart is given to do
\(COMPILE-PACKAGE-CHILD), as text read back without evaluating anything: the
package, FILE its entry when named; the packages loaded so far, to load
first, oldest first; where this Tuatara keeps compiled packages; and TOKEN,
which the process is given apart too (PACKAGE-REQUEST-ALLOWED-P)."
  (with-standard-io-syntax
    (let ((*print-readably* t))
      (prin1-to-string
       (list :token token :name name :folder (tuatara.io:native folder) :file file
             :prelude (reverse (remove name *loaded-package-folders* :key #'first :test #'string=))
             :cache (tuatara.io:native (tuatara.io:cache-directory))
             :packages (packages-directory)
             :identity (tuatara-identity))))))

(defparameter *package-child-environment*
  '("PATH" "HOME" "USER" "LOGNAME" "LANG" "LC_ALL" "LC_CTYPE" "TZ" "TMPDIR"
    "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_STATE_HOME"
    "SBCL_HOME" "QUICKLISP_SETUP")
  "The variables of the editor's environment a process compiling a package
apart is given, and no others. This cuts down what it reaches by chance (the
desktop, the keyring's agent, the session's message bus, whose sockets
XDG_RUNTIME_DIR names); it is no sandbox: a package's code runs there with
your full rights, as in the editor.")

(defun package-child-environment (request token)
  "The environment of a process compiling a package apart, as REQUEST, a
file, and TOKEN say (COMPILE-PACKAGE-CHILD): those of
*PACKAGE-CHILD-ENVIRONMENT* the editor has, and what tells the process what
to do."
  (append (loop for name in *package-child-environment*
                for value = (uiop:getenv name)
                when value collect (format nil "~a=~a" name value))
          (list "TUATARA_HEAP_DECIDED=1"
                (format nil "TUATARA_COMPILE_PACKAGE=~a" request)
                (format nil "TUATARA_COMPILE_TOKEN=~a" token)
                (format nil "TUATARA_COMPILE_CACHE=~a" (package-cache)))))

(defun package-compiler-command ()
  "The command that runs this Tuatara again to compile a package apart, told
what to do by its environment (PACKAGE-CHILD-ENVIRONMENT): the saved
executable, or, run from its sources, SBCL loading them first."
  (if tuatara.io:*saved-program*
      (list tuatara.io:*saved-program*)
      (list (uiop:native-namestring sb-ext:*runtime-pathname*) "--non-interactive" "--no-sysinit" "--no-userinit"
            "--load" (uiop:native-namestring (asdf:system-relative-pathname :tuatara "tools/quicklisp.lisp"))
            "--eval" (format nil "(progn (push ~s asdf:*central-registry*) (funcall (find-symbol \"QUICKLOAD\" \"QL\") :tuatara/defaults :silent t))"
                             (uiop:native-namestring (asdf:system-source-directory :tuatara)))
            "--eval" "(sb-ext:exit :code (tuatara.defaults::compile-package-child) :abort t)")))

(defparameter *package-child-line-limit* 1024
  "The most characters kept of a line a process compiling a package apart
says; the rest of it is read and let go.")

(defparameter *package-child-said-limit* (* 64 1024)
  "The most characters read and kept in all of what a process compiling a
package apart says; the rest is read and let go, so that it never waits.")

(defun compile-package-apart (job folder name file)
  "Compile the package NAME in FOLDER, FILE its entry when named, in a process
of its own, this Tuatara run again (PACKAGE-COMPILER-COMMAND): it loads the
packages loaded here so far, then this one, compiling what changed, as a
load here would, so that its compile-time code never runs in the editor.
JOB is the loading's: stopping it ends that process; its COMPILED turns true
as a file of it begins compiling, said. True when it was all compiled so;
NIL when it failed there, as code of the package's that needs what only the
editor has does, the last of what the process said kept in *Messages*. It
is refused for names this Tuatara lacks only as it loads in the editor
\(REFUSE-PACKAGE). What the process says is read a line of at most
*PACKAGE-CHILD-LINE-LIMIT* at a time, *PACKAGE-CHILD-SAID-LIMIT* in all."
  (let ((request (format nil "~acompile-~a.request" (package-cache) (tuatara.io:random-name)))
        (token (tuatara.io:random-name))
        (said '()) (kept 0) (read 0))
    (tuatara.io:ensure-private-directory (package-cache))
    (tuatara.io:write-private-file request (package-request-text folder name file token))
    (unwind-protect
         (let ((code (tuatara.io:call-with-program
                      (package-compiler-command)
                      (lambda (process)
                        (loop for line = (tuatara.io:read-line-bounded (sb-ext:process-output process) *package-child-line-limit*)
                              while line
                              do (incf read (length line))
                                 (cond ((> read *package-child-said-limit*))
                                       ((string= line *package-compiling-line*)
                                        (unless (package-fetch-compiled job)
                                          (setf (package-fetch-compiled job) t)
                                          (message "Compiling the package ~a in the background..." name)))
                                       (t (push line said)
                                          (when (> (incf kept) *package-child-said-lines*)
                                            (setf said (subseq said 0 *package-child-said-lines*) kept *package-child-said-lines*)))))
                        (sb-ext:process-wait process)
                        (sb-ext:process-exit-code process))
                      :job job :output :stream :error :output
                      :environment (package-child-environment request token))))
           (tuatara.runtime:job-check-stop)
           (cond ((eql code 0) t)
                 (t (let ((words (format nil "The package ~a could not be compiled apart from the editor, and is compiled as it loads~@[: ~{~a~^ ~}~]"
                                         name (reverse (mapcar (lambda (l) (string-trim " ;" l)) (remove "" said :test #'string=))))))
                      ;; *Messages* is the editor's
                      (defer-action (lambda () (log-message words))))
                    nil)))
      (ignore-errors (delete-file request)))))

(defun package-request-allowed-p (request token cache)
  "True when REQUEST, a file, is one the editor wrote for a process compiling
a package apart (COMPILE-PACKAGE-APART): a regular file, not a link, in CACHE,
the folder of compiled packages the editor named, called compile-*.request,
yours and read and written by you alone; and TOKEN, given in the
environment, is the one it holds (checked by COMPILE-PACKAGE-CHILD)."
  (let ((stat (and request (plusp (length request)) token (plusp (length token)) cache
                   (ignore-errors (sb-posix:lstat request))))
        (name (and request (tuatara.io:native-file-name request))))
    (and stat
         (= (logand (sb-posix:stat-mode stat) #o170000) #o100000)
         (= (logand (sb-posix:stat-mode stat) #o777) #o600)
         (= (sb-posix:stat-uid stat) (sb-posix:getuid))
         (uiop:string-prefix-p "compile-" name)
         (uiop:string-suffix-p name ".request")
         (equal (tuatara.io:real-path (tuatara.io:parent-path request))
                (tuatara.io:real-path cache)))))

(defun compile-package-child ()
  "Compile a package as the editor asked in a process of its own: the
request it wrote (PACKAGE-REQUEST-TEXT), named in the environment as
TUATARA_COMPILE_PACKAGE, taken only when PACKAGE-REQUEST-ALLOWED-P and
holding the token TUATARA_COMPILE_TOKEN gives; never from the command line.
The packages loaded in the editor are loaded first, one that fails left out,
then the package, each file compiled that changed since, as a load in the
editor would, into the same place (PACKAGE-FASL), nothing refused
\(REFUSE-PACKAGE). Returns the exit code: 0 when it loaded, 1 when it
failed, why printed, 2 when the request was not taken."
  (let ((request (uiop:getenv "TUATARA_COMPILE_PACKAGE"))
        (token (uiop:getenv "TUATARA_COMPILE_TOKEN"))
        (cache (uiop:getenv "TUATARA_COMPILE_CACHE")))
    (dolist (name '("TUATARA_COMPILE_PACKAGE" "TUATARA_COMPILE_TOKEN" "TUATARA_COMPILE_CACHE"))
      (sb-posix:unsetenv name))
    (unless (package-request-allowed-p request token cache)
      (format t "~&Not a request of the editor's: nothing compiled~%")
      (return-from compile-package-child 2))
    (handler-case
        (let ((plist (with-standard-io-syntax
                       (let ((*read-eval* nil)) (read-from-string (tuatara.io:file-text request))))))
          (unless (and (listp plist) (equal (getf plist :token) token))
            (format t "~&Not a request of the editor's: nothing compiled~%")
            (return-from compile-package-child 2))
          (setf *tuatara-identity* (getf plist :identity)
                tuatara-user:*cache-directory* (getf plist :cache)
                tuatara-user:*packages-directory* (getf plist :packages))
          (let ((*package-compiling* :apart))
            (loop for (name folder file) in (getf plist :prelude)
                  do (handler-case (load-package-entry (pathname folder) name file)
                       (error (e) (format t "~&~a was left out: ~a~%" name e))))
            (load-package-entry (pathname (getf plist :folder)) (getf plist :name) (getf plist :file)))
          0)
      (serious-condition (e)
        (format t "~&~a~%" e)
        1))))

(defun start-package-fetch (name where version file key &key quiet)
  "Fetch the package NAME (FETCH-PACKAGE) in the background, and see who
signed it (PACKAGE-SIGNATURE), then ask and load it (PACKAGE-FETCHED). An
address of http:// or git:// not allowed (PLAIN-SOURCE-REFUSAL) is not
fetched from, said; nor is a package in hand already (START-PACKAGE-JOB),
said too unless QUIET. One whose folder package-remove-unused is deleting
waits for it (WAIT-FOR-SWEEP)."
  (let ((refusal (plain-source-refusal name where)))
    (when refusal
      (setf *package-failures* (acons name refusal (remove name *package-failures* :key #'car :test #'string=)))
      (message "~a" refusal)
      (return-from start-package-fetch nil)))
  (when (wait-for-sweep (package-kept-folder where name) (lambda () (start-package-fetch name where version file key :quiet quiet)))
    (return-from start-package-fetch nil))
  (let ((plain (and (plain-transport-p where) t))
        (floor (let ((had (fetched-package key))) (and had (fourth had) (git-hash-p (second had) :short t) (second had)))))
    (when (start-package-job
           (make-package-fetch name where version file key)
           (lambda (job)
             (let ((*package-plain-transport* plain))
               (multiple-value-bind (folder commit tag passed signature)
                   (fetch-package name where version
                                  :file file :floor floor
                                  :walk-p (lambda (looked commit tag)
                                            (not (eq :refuse (package-trust-verdict where (package-signature looked commit tag where)))))
                                  :signature-of (lambda (looked commit tag) (package-signature looked commit tag where)))
                 (setf (package-fetch-folder job) folder (package-fetch-commit job) commit (package-fetch-tag job) tag
                       (package-fetch-passed job) passed (package-fetch-signature job) signature))))
           "tuatara package fetch" :quiet quiet)
      (setf *package-failures* (remove name *package-failures* :key #'car :test #'string=))
      (log-message (format nil "Fetching the package ~a from ~a" name where)))))

(defun start-package-check (name where version file key entry &key quiet)
  "Check in the background who signed the version of the package NAME from
WHERE kept as ENTRY remembers it (PACKAGE-SIGNATURE), then take it as one
just fetched (PACKAGE-FETCHED): asked about with who signed it, loaded, or
refused, as the trust in force says. Nothing is fetched; nothing is done
while the package is in hand already (START-PACKAGE-JOB, QUIET as it takes
it)."
  (let ((job (make-package-fetch name where version file key)))
    (setf (package-fetch-folder job) (package-folder-for where name (second entry))
          (package-fetch-commit job) (second entry)
          (package-fetch-tag job) (third entry))
    (when (start-package-job
           job
           (lambda (job)
             (setf (package-fetch-signature job)
                   (package-signature (package-fetch-folder job) (package-fetch-commit job) (package-fetch-tag job) where)))
           "tuatara package check" :quiet quiet)
      (setf *package-failures* (remove name *package-failures* :key #'car :test #'string=)))))

(defun signature-signer (signature)
  "Who SIGNATURE (PACKAGE-SIGNATURE) says signed a version, as an entry of
*FETCHED-PACKAGES* keeps it: the key's fingerprint, \"unsigned\", or NIL when
it could not be told."
  (case (getf signature :status)
    (:good (getf signature :fingerprint))
    (:none "unsigned")))

(defun kept-version-check-needed-p (where entry)
  "True when the version of the source WHERE that ENTRY of *FETCHED-PACKAGES*
remembers is to be checked again before it loads at a start: who signed it,
as git checked it when it was taken, is not known, or the trust in force
now would not take it (PACKAGE-TRUST-VERDICT: the source trusted anew, its
signer changed since, the init file's :signers naming others)."
  (let ((signer (seventh entry)))
    (or (null signer)
        (not (eq :take (package-trust-verdict where (if (string= signer "unsigned")
                                                        (list :status :none)
                                                        (list :status :good :fingerprint signer))))))))

(defmethod tuatara.runtime:job-take-in ((job package-fetch))
  "Ask about and load the package fetched, or say why it was not; its package
held in hand until that is decided. For a package compiled and loaded in the
background, what follows its loading (PACKAGE-LOAD-TAKEN-IN)."
  (if (package-fetch-load job)
      (let ((job job))
        (defer-action (lambda () (package-load-taken-in job))))
      (progn
        (setf *fetching-packages* (remove (package-fetch-name job) *fetching-packages* :test #'string=))
        (case (package-fetch-status job)
          (:done (let ((job job))
                   (defer-action (lambda ()
                                   (let ((*package-job-deciding* job))
                                     (unwind-protect (package-fetched job)
                                       (let-package-go job)))))))
          (:failed (let-package-go job)
                   (let ((why (or (tuatara.runtime:job-error job) "it ended unexpectedly")))
                     (push (cons (package-fetch-name job) why) *package-failures*)
                     (message "The package ~a was not fetched: ~a" (package-fetch-name job) why)))
          (t (let-package-go job))))))

(defun package-fetched (job)
  "Take the version of a package JOB fetched, or refuse it. One that needs a
newer Tuatara than this was passed over for the newest that fits, said; when
none fits, the version loaded now is kept. One whose signer is not the one
trusted is refused (PACKAGE-TRUST-VERDICT). Else it is loaded: at once when
that commit was let load before and its source's signer is known, said to
be up to date when it is loaded already; else asked about first, who signed
it shown, and that signer remembered once it is let load. The version taken
before is kept to fall back on (LOAD-VERSION), older ones deleted
\(PRUNE-PACKAGE-VERSIONS)."
  (let* ((name (package-fetch-name job)) (key (package-fetch-key job)) (where (package-fetch-where job))
         (commit (package-fetch-commit job)) (tag (package-fetch-tag job))
         (passed (package-fetch-passed job))
         (had (fetched-package key)))
    (cond
      ((null (package-fetch-folder job))
       (let ((words (format nil "The package ~a ~a needs Tuatara ~a or later, and this is ~a; ~:[no older version tried fits~;~
                                 it is not signed as the versions taken before were, so no older version is looked for~]"
                            name (first passed) (second passed) (tuatara:version) (third passed)))
             (loaded (package-loaded-p name)))
         (unless loaded (push (cons name words) *package-failures*))
         (message "~a: ~:[it is not loaded~;the version loaded now is kept~]" words loaded)))
      (t
       (when passed
         (message "The package ~a ~a needs Tuatara ~a or later, and this is ~a: ~a, the newest version that fits, is taken"
                  name (first passed) (second passed) (tuatara:version) (or tag (subseq commit 0 7))))
       (multiple-value-bind (verdict words remember) (package-trust-verdict where (package-fetch-signature job))
         (if (eq verdict :refuse)
             (progn
               ;; a version taken before and refused now is asked about again, never loaded at a start unchecked
               (when (and had (fourth had) (string= (second had) commit))
                 (state-list-put *fetched-packages* (list (list key commit tag nil (fifth had) (sixth had) (seventh had)))))
               (refuse-package-version name where words remember))
             (let* ((folder (package-folder-in (package-fetch-folder job) name))
                    (same (and had (string= (second had) commit)))
                    (previous (if (and had (fourth had) (not same)) (second had) (fifth had)))
                    (previous-tag (if (and had (fourth had) (not same)) (third had) (sixth had)))
                    (signer (signature-signer (package-fetch-signature job)))
                    (trust (lambda ()
                             (when remember
                               (state-list-put *package-trust* (list remember)))))
                    (load (lambda ()
                            (state-list-put *fetched-packages* (list (list key commit tag t previous previous-tag signer)))
                            (load-version name where folder (package-fetch-file job)
                                          (cond ((and had (fourth had) (not same)) had)
                                                (previous (list key previous previous-tag t)))
                                          (lambda (outcome &optional loaded)
                                            (declare (ignore loaded))
                                            (when (eq outcome :taken)
                                              (funcall trust)
                                              (when (package-loaded-p name)
                                                (message "The package ~a~@[ ~a~] is loaded~@[ (~a needs Tuatara ~a or later)~]"
                                                         name (or tag (and passed (subseq commit 0 7))) (first passed) (second passed))))
                                            (prune-package-versions name where))))))
               (cond ((and had (fourth had) same (package-loaded-p name))
                      (funcall trust)
                      (unless (equal (seventh had) signer)
                        (state-list-put *fetched-packages* (list (list key commit tag t (fifth had) (sixth had) signer))))
                      (message "The package ~a is up to date~@[ (~a)~]" name tag))
                     ((and had (fourth had) same (not (eq verdict :ask)))
                      (funcall load))
                     ((and had (fourth had) same)
                      ;; taken before its source's signer was remembered: asked again, who signed it shown
                      (package-question name where folder load words))
                     (t (state-list-put *fetched-packages* (list (list key commit tag nil previous previous-tag signer)))
                        (package-question name where folder load words))))))))))

;;; load-package with a source

(defun load-or-ask (name where folder file accept)
  "Load the package NAME from FOLDER, of WHERE, once let: at once when
*PACKAGE-ASK-BEFORE-LOADING* is NIL, else after asking (PACKAGE-QUESTION),
once the editor has started; ACCEPT, a function, remembers that it was let."
  (flet ((load-it () (funcall accept) (load-package-folder folder name file)))
    (if *package-ask-before-loading*
        (defer-action (lambda () (package-question name where folder #'load-it)))
        (load-it))))

(defun load-package-from (name source version file)
  "Load the package NAME from SOURCE (LOAD-PACKAGE): a folder of your disk at
once, asked about the first time; one fetched before from where it was kept,
the version before it in its place when it fails to load (LOAD-VERSION),
unless it is to be asked about, or who signed it checked again first against
the trust in force (KEPT-VERSION-CHECK-NEEDED-P), in the background
\(START-PACKAGE-CHECK); else
fetched in the background first."
  (let* ((where (package-where source))
         (key (package-key name where version))
         (entry (fetched-package key))
         (folder (package-folder-of name where entry)))
    (cond ((and (not (local-source-p where))
                (wait-for-sweep (package-kept-folder where name) (lambda () (load-package-from name source version file))))
           nil)
          ((not (or (local-source-p where) (and folder (probe-file folder))))
           (start-package-fetch name where version file key :quiet t))
          ((and entry (not (local-source-p where))
                (or (not (fourth entry)) (kept-version-check-needed-p where entry))
                (eq :directory (tuatara.io:path-kind (tuatara.io:native folder))))
           (start-package-check name where version file key entry :quiet t)
           nil)
          ((not (eq :directory (tuatara.io:path-kind (tuatara.io:native folder))))
           (message "The package ~a is not loaded: its folder ~a is not there" name (abbreviated-path (tuatara.io:native folder))))
          ((and entry (fourth entry) (local-source-p where))
           (load-package-folder folder name file))
          ((and entry (fourth entry))
           (multiple-value-bind (outcome loaded) (load-version name where folder file
                                                                       (and (fifth entry) (list key (fifth entry) (sixth entry) t)))
             (declare (ignore outcome))
             loaded))
          (t (load-or-ask name where folder file
                          (lambda () (state-list-put *fetched-packages*
                                                     (list (list key (if entry (second entry) "-") (third entry) t)))))))))


;;; The package manager

(define-command package-update ()
  "Fetch again each package load-package fetched (not the folders of your
disk), in the background: the version it asks for looked up anew (the newest
tag, the newest commit, or the tag, branch or commit named), and fetched and
loaded when it is another commit than the one there, asked about first
\(*PACKAGE-ASK-BEFORE-LOADING*); the others are said to be up to date. A
version that needs a newer Tuatara is passed over for the newest that fits,
or the one loaded now kept; one signed by another than the source's first
version, or unsigned when that was signed, is refused (\[package-trust-again]
takes it); one that fails to load gives way to the version before it."
  (update-packages (mapcar #'first *declared-packages*)))

(defun update-packages (names)
  "Fetch again the packages called NAMES that come from a repository, as
\\[package-update] does; say so when none of them does."
  (let ((fetched (remove-if-not (lambda (p) (and (member (first p) names :test #'string=)
                                                 (second p)
                                                 (not (local-source-p (package-where (second p))))))
                                *declared-packages*)))
    (if (null fetched)
        (message "No package here is fetched from a repository")
        (loop for (name source version file) in fetched
              for where = (package-where source)
              do (start-package-fetch name where version file (package-key name where version))))))

(define-major-mode packages-mode marked-list-mode
  "The package manager (\\[package-manager]): a line each package the init file
loads from a source, as the buffer list (\\[buffers]) has its buffers.
\\<support>\\[next-line] and \\[previous-line] move, \\<packages-mode>\\[list-visit] or \\`RET' lists the folder of
the package on the line in rimu, \\[list-visit-other] in another window, \\[quit-window] or
\\`h' returns; \\[list-mark-entry] marks a package (>), \\[list-mark-for-deletion] marks it for deletion (D),
\\[list-unmark-entry] unmarks it, \\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps marked and unmarked; \\[packages-update-marked] fetches the
marked ones again (the one on the line when none is), \\[list-delete-marked] deletes what
is kept of them after asking, \\[list-execute] of those marked for deletion;
\\[revert-buffer] lists them again, marks kept, as the list does by itself while it
is shown. The README of the package on the line is shown beside the list,
\\[list-toggle-preview] turning that off and on."
  :lighter "Packages" :interactive nil)

(defvar *packages-keymap* (mode-keymap 'packages-mode)
  "Keys of the package manager (packages-mode's keymap).")

(defparameter *packages-columns* '((" M" :width 2) ("Name" :width 18) ("Version" :width 16) ("Commit" :width 8)
                                   ("State" :width 12) ("Signed by" :width 18) ("Source"))
  "The columns of the package manager, as TABLE-HEADING-PIECES takes them: the
mark (> or D), the name, the version (its tag), the commit, what is become of
it this session, who signs its source's versions, and where it comes from.")

(defparameter *packages-source-width* 20
  "The columns of a package's source the package manager keeps in view: its
README goes beside the list only when the list has room for its other
columns and this much of the source, else below it.")

(defun package-state (name source version)
  "What is known of the package NAME from SOURCE, VERSION asked for: the
entry remembered (KEY COMMIT TAG ACCEPTED) and where it is kept, as two values."
  (let* ((where (package-where source))
         (entry (fetched-package (package-key name where version))))
    (values entry (package-folder-of name where entry))))

(defun package-condition (name entry)
  "What is become of the package NAME this session, ENTRY what is remembered
of it: loaded, fetching, loading (compiled and loaded in the background),
refused (it uses names this Tuatara lacks), failed, waiting (for another,
:after), later (to load on a command, a mode or once started), declined (not
let load), kept or not fetched."
  (cond ((package-loaded-p name) "loaded")
        ((member name *fetching-packages* :test #'string=) "fetching")
        ((let ((job (cdr (assoc name *packages-in-hand* :test #'string=))))
           (and job (package-fetch-load job)))
         "loading")
        ((member name *refused-packages* :test #'string=) "refused")
        ((assoc name *package-failures* :test #'string=) "failed")
        ((find name *packages-waiting* :key #'second :test #'string=) "waiting")
        ((member name *deferred-packages* :test #'string=) "later")
        ((and entry (not (fourth entry))) "declined")
        (entry "kept")
        (t "not fetched")))

(defun package-signed-words (where)
  "Who signs the versions of the source WHERE, as the package manager says it:
the signer's name or key remembered (*PACKAGE-TRUST*), unsigned, nothing yet
or for a folder of your disk; \"unencrypted\" first for an address of
http:// or git://."
  (if (local-source-p where)
      ""
      (let ((trusted (state-list-find *package-trust* where)))
        (format nil "~{~a~^, ~}"
                (remove nil (list (and (plain-transport-p where) "unencrypted")
                                  (cond ((null trusted) nil)
                                        ((string= (second trusted) "unsigned") "unsigned")
                                        (t (or (third trusted) (second trusted))))))))))

(defun package-name-face (mark condition)
  "The face of a package's name in the package manager: that of its MARK
\(LIST-MARK-FACE) when it has one; else :list-dim for one not loaded, by its
CONDITION, none for one loaded."
  (or (list-mark-face mark)
      (unless (string= condition "loaded") :list-dim)))

(defun packages-render (list &optional (point :line))
  "Fill LIST, the package manager, with a line for each package of
*DECLARED-PACKAGES* and a summing-up line; marks kept on those still there."
  (render-table-list
   list *packages-columns* (mapcar #'first *declared-packages*)
   (lambda (name mark section)
     (declare (ignore section))
     (let ((declared (assoc name *declared-packages* :test #'string=)))
       (let* ((source (second declared)) (version (third declared))
              (entry (package-state name source version))
              (condition (package-condition name entry))
              (where (package-where source)))
         (list (list-mark-sign mark)
               (cons name (package-name-face mark condition))
               (or (third entry) version (if (local-source-p where) "folder" ""))
               (if (and entry (string/= (second entry) "-")) (subseq (second entry) 0 (min 7 (length (second entry)))) "")
               condition
               (package-signed-words where)
               (if (local-source-p where) (abbreviated-path where) where)))))
   :after (if *declared-packages*
              (list (string #\Newline)
                    (format nil "   ~[No packages~;1 package~:;~:*~d packages~], ~d loaded~%" (length *declared-packages*)
                            (count-if (lambda (p) (package-loaded-p (first p))) *declared-packages*)))
              (list (format nil "~%   No packages: (load-package NAME) in the init file~%")))
   :point point
   :refill (lambda (list) (packages-render list :keep))))

(defun packages-key (list)
  "What the package manager LIST shows, cheaply; another key means it is out
of date (KEEP-SHOWN-CURRENT)."
  (declare (ignore list))
  (list *declared-packages* *loaded-packages* *fetching-packages* *packages-in-hand* *package-failures*
        (state-list-entries *fetched-packages*) (state-list-entries *package-trust*)))

(keep-shown-current 'packages-mode 'packages-key)

(define-command package-manager ()
  "Manage the packages the init file loads from a source (load-package with
:source) in a list, as the buffer list (\\[buffers]) manages buffers. Each
line: its mark (> marked, D marked for deletion), name, the version kept (its
tag), its commit, what is become of it (loaded; fetching; refused, using
names this Tuatara lacks; failed; declined, not let load; kept, not loaded;
not fetched), who signs its source's versions (unsigned, and unencrypted for
an address of http:// or git://) and where it comes from; the names of those not loaded set
apart; why one was refused or failed is shown beside the list in place of
its README. \\<packages-mode>\\[list-visit] or \\`RET' lists a package's
folder in rimu, to read its source, \\[list-visit-other] in another window; \\[list-mark-entry] marks
it, \\[list-mark-for-deletion] marks it for deletion, \\[list-unmark-entry] and \\[list-unmark-all] unmark, \\[list-toggle-marks] swaps marked and unmarked, a count marking as
many; \\[packages-update-marked] fetches the marked ones again, \\[list-delete-marked] deletes what is kept of
them after asking, \\[list-execute] of those marked for deletion. The README of
the package on the line is shown beside the list, \\[list-toggle-preview] turning that
off and on. \\[package-update] fetches them all again."
  (let ((list (buffer-in-mode "*Packages*" 'packages-mode)))
    (when (eq (tuatara.buffer:buffer-local list :list-preview :unset) :unset)
      (setf (tuatara.buffer:buffer-local list :list-preview) t))
    (packages-render list 2)
    (show-buffer list)))

;;; Finding the packages the init file names

(defun load-package-form-p (form)
  "True when FORM is a load-package form loading a package from a source:
\(load-package NAME ... :source WHERE ...)."
  (and (consp form) (symbolp (first form)) (string-equal (symbol-name (first form)) "LOAD-PACKAGE")
       (consp (rest form)) (stringp (second form))
       (member :source (cddr form))
       t))

(defun swallowed-package-forms (form)
  "The load-package forms FORM, a load-package form, holds as values of its
options that take one value, as after a parenthesis left open before them,
and those they hold in turn, in order; never the forms of :init, :config,
:commands or :modes, nor more than 10,000 of FORM's parts."
  (let ((found '()) (key nil) (values 0))
    (loop for item in (cddr form)
          repeat 10000
          do (if (keywordp item)
                 (setf key item values 0)
                 (unless (member key '(:init :config :commands :modes))
                   (when (and (> (incf values) 1) (load-package-form-p item))
                     (push item found)
                     (dolist (inner (swallowed-package-forms item)) (push inner found))))))
    (nreverse found)))

(defun init-file-package-forms (file)
  "The load-package forms of the init file FILE that load a package from a
source, read and never evaluated (no #. either), in the order they stand,
each (FORM LINE INSIDE): LINE where the form holding it at the top begins,
INSIDE true when it was left inside another load-package by a parenthesis
left open (SWALLOWED-PACKAGE-FORMS). Only the init file's own top-level
forms are looked at, not code that decides whether one runs. As a second
value, where the reading stopped, (LINE . WHY), or NIL when the whole file
was read."
  (let ((found '()))
    (let* ((*package* (find-package :tuatara-user))
           (*read-eval* nil)
           (stopped (map-text-forms (tuatara.io:file-text file)
                                    (lambda (form line)
                                      (when (load-package-form-p form)
                                        (push (list form line nil) found)
                                        (dolist (inner (swallowed-package-forms form))
                                          (push (list inner line t) found)))))))
      (values (nreverse found) (and stopped (cons (car stopped) (princ-to-string (cdr stopped))))))))

(defun scan-init-file-packages (file)
  "Load each package the init file FILE names with load-package and :source
that is not loaded or on its way (INIT-FILE-PACKAGE-FORMS): its load-package
evaluated by itself, as at the start, so that it is fetched, asked about and
loaded as there. Each found, (NAME LINE INSIDE OUTCOME), OUTCOME what
became of it (PACKAGE-CONDITION) or why it failed; as a second value, where
the reading stopped, (LINE . WHY), or NIL."
  (multiple-value-bind (found stopped) (init-file-package-forms file)
    (values
     (loop for (form line inside) in found
           for name = (second form)
           collect (list name line inside
                         (let ((before (let ((declared (declared-package name)))
                                         (and declared (package-condition name (package-state name (second declared) (third declared)))))))
                           (if (member before '("loaded" "fetching" "loading" "later" "waiting") :test #'equal)
                               before
                               (handler-case
                                   (let ((*package* (find-package :tuatara-user)))
                                     (setf *package-failures* (remove name *package-failures* :key #'car :test #'string=)
                                           *refused-packages* (remove name *refused-packages* :test #'string=))
                                     (eval form)
                                     (let ((declared (declared-package name)))
                                       (if declared
                                           (package-condition name (package-state name (second declared) (third declared)))
                                           "not named")))
                                 (error (e) (format nil "failed: ~a" e)))))))
     stopped)))

(define-command package-scan ()
  "Look in the init file for the packages it loads from a source (each
load-package with :source), and load each that is not loaded or on its way,
as at the start: fetched in the background, asked about before it is
compiled, loaded. A load-package found inside another form, as after a
parenthesis left open, is loaded by itself and said; where the file could
not be read on is said, the packages after it unseen. The package manager
\(\\[package-manager]) is then shown, following them as they load; what each
became is in *Messages* (\\[view-echo-area-messages])."
  (let ((file (config-file)))
    (unless (probe-file file)
      (error "No init file at ~a" (abbreviated-path file)))
    (multiple-value-bind (results stopped) (scan-init-file-packages file)
      (loop for (name line inside outcome) in results
            do (log-message (format nil "package-scan: ~a, line ~d~:[~;, inside another form (a parenthesis left open before it?)~]: ~a"
                                    name line inside outcome)))
      (when stopped
        (log-message (format nil "package-scan: ~a could not be read on from line ~d: ~a; packages after it are not seen"
                             (abbreviated-path file) (car stopped) (cdr stopped))))
      (package-manager)
      (message "~[No packages~;1 package~:;~:*~d packages~] in ~a~[~:;, ~:*~d inside another form~]~[~:;, ~:*~d failed~]~@[, read up to line ~d~]: \\[view-echo-area-messages] says each"
               (length results) (abbreviated-path file)
               (count-if #'third results)
               (count-if (lambda (r) (uiop:string-prefix-p "failed" (fourth r))) results)
               (car stopped)))))

(defun package-on-line ()
  "The package on the current line of the package manager, (NAME SOURCE
VERSION FILE), or NIL."
  (let ((name (list-entry-at)))
    (and name (declared-package name))))

(defmethod list-entry-noun ((mode packages-mode))
  "A package."
  "package")

(defmethod visit-entry ((mode packages-mode) list name other)
  "List the folder of the package called NAME in rimu, in another window with
OTHER, opened safely, to read its source, each file previewed beside it;
\\<rimu-mode>\\[rimu-up] in the package's folder comes back to LIST."
  (let ((package (declared-package name)))
    (if (null package)
        (message "No package on this line")
        (destructuring-bind (name source version &rest more) package
          (declare (ignore more))
          (let ((folder (nth-value 1 (package-state name source version))))
            (if folder
                (rimu-show-directory (tuatara.io:native folder) :other other :safe t :root (tuatara.io:native folder) :back list)
                (message "~a is not fetched yet" name)))))))

(define-command package-trust-again ()
  "Trust the source of a package asked for anew, as the first time: who
signed the versions taken from it is forgotten, and the package is fetched
again (\\[package-update]); the version found is asked about with who signed it
\(*PACKAGE-ASK-BEFORE-LOADING*), and once it is let load, that signer is the
one remembered. For a version refused because another key signed it, or
because it is unsigned when the versions before were signed. Signers named
in the init file (:signers) still decide for their source."
  (let ((names (loop for (name source) in *declared-packages*
                     when (and source (not (local-source-p (package-where source)))) collect name)))
    (if (null names)
        (message "No package here is fetched from a repository")
        (let ((name (completing-read "Trust anew the source of the package: " names :require-match t)))
          (when (member name names :test #'string=)
            (let ((where (package-where (second (declared-package name)))))
              (state-list-drop *package-trust* (list where))
              (log-message (format nil "The source ~a is trusted anew: who signs it is asked again" where))
              (update-packages (list name))))))))

(define-command packages-update-marked ()
  "Fetch again the marked packages (>), or with none marked the one on the
current line, as \\[package-update] does them all."
  (act-on-list (current-buffer) (lambda (list names) (update-packages names) (unmark-list list)) "package"))

(defun forget-package (name source version)
  "Delete what is kept of the package NAME from SOURCE, VERSION asked for:
what is remembered of it, its commits fetched and their files compiled; for a
folder of your disk, that it was let load and its files compiled, the folder
left as it is."
  (let* ((where (package-where source))
         (local (local-source-p where))
         (top (if local (package-folder-of name where nil) (package-kept-folder where name))))
    (state-list-drop *fetched-packages* (list (package-key name where version)))
    (dolist (folder (cons (directory-namestring (package-fasl (merge-pathnames "x" top))) (unless local (list top))))
      (when (probe-file folder)
        (delete-package-folder folder)))))

(defun packages-delete (list names)
  "Delete what is kept of the packages called NAMES after asking (FORGET-PACKAGE),
and make LIST again."
  (when (yes-or-no-p* (format nil "~a?" (list-question "Delete what Tuatara keeps of" "package" names)))
    (dolist (name names)
      (let ((package (declared-package name)))
        (when package (forget-package (first package) (second package) (third package)))))
    (unmark-list list)
    (message "~[No packages~;1 package~:;~:*~d packages~] deleted: one loaded stays so until Tuatara ends; at the next start its load-package line fetches it again, or for a folder of your disk asks again"
             (length names))))

(defmethod delete-entries ((mode packages-mode) list names mark)
  "Delete what is kept of the packages, after asking (PACKAGES-DELETE)."
  (declare (ignore mark))
  (packages-delete list names))

;;; Package sources no longer named

(defun unused-package-folders ()
  "The folders of the packages directory (PACKAGES-DIRECTORY) that no
package or source named this session wants, each (FOLDER . WORDS), FOLDER a
native name ending in /, WORDS what it holds: a source's whole folder, when
no package-source names it and no load-package (left out ones too) fetches
from it; else the folder of a package of a source that is named, when no
load-package names that package. A source a fetch or check runs for now is
left alone, as are folders whose name begins with a full stop (a fetch's
scratch) and anything that is not a folder of its own (a symbolic link)."
  (let* ((top (tuatara.io:native (packages-directory)))
         (named (append (loop for (name source) in *declared-packages* when source collect (list name source))
                        *left-out-packages*))
         (whole (loop for (nil . where) in *package-sources*
                      unless (local-source-p where) collect (tuatara.io:path-file-name where)))
         (pairs (loop for (name source) in named
                      for where = (package-where source)
                      unless (local-source-p where)
                        collect (cons (tuatara.io:path-file-name where) name)))
         (busy (busy-source-folders))
         (found '()))
    (flet ((own-folders (folder)
             ;; the folders directly in FOLDER, links never followed, nor names beginning with .
             (loop for entry in (ignore-errors (directory (merge-pathnames "*/" folder) :resolve-symlinks nil))
                   for name = (car (last (pathname-directory entry)))
                   for native = (tuatara.io:native entry)
                   when (and (stringp name) (plusp (length name)) (char/= (char name 0) #\.)
                             (eq :directory (tuatara.io:path-kind (string-right-trim "/" native) :follow nil))
                             (tuatara.io:really-within-p native top))
                     collect (cons name native))))
      (dolist (source (own-folders top))
        (destructuring-bind (source-name . source-folder) source
          (cond ((or (member source-name whole :test #'string=) (member (string-right-trim "/" source-folder) busy :key (lambda (b) (string-right-trim "/" b)) :test #'string=)))
                ((find source-name pairs :key #'car :test #'string=)
                 (dolist (package (own-folders source-folder))
                   (unless (find-if (lambda (pair) (and (string= (car pair) source-name) (string= (cdr pair) (car package)))) pairs)
                     (push (cons (cdr package) (format nil "~a from ~a" (car package) (tuatara.io:percent-decoded source-name)))
                           found))))
                (t (push (cons source-folder (format nil "~a, all of it" (tuatara.io:percent-decoded source-name))) found))))))
    (nreverse found)))

(defun forget-packages-within (folder)
  "Forget the versions *FETCHED-PACKAGES* remembers of the packages kept
within FOLDER, deleted. Who signed their source is not forgotten."
  (state-list-drop *fetched-packages*
                   (loop for (key) in (state-list-entries *fetched-packages*)
                         for words = (uiop:split-string key :separator " ")
                         when (and (>= (length words) 3)
                                   (tuatara.io:path-within-p
                                    (tuatara.io:native (package-kept-folder (format nil "~{~a~^ ~}" (butlast (rest words))) (first words)))
                                    folder))
                           collect key)))

(defun package-compiled-folder (folder)
  "The folder the files of what is kept in FOLDER, of the packages directory, are compiled into."
  (directory-namestring (package-fasl (concatenate 'string (tuatara.io:native folder) "x"))))

(defstruct (package-sweep (:include tuatara.runtime:job) (:constructor make-package-sweep (phase folders)) (:copier nil))
  "Package sources no longer named being looked for (PHASE :look), or one
of them being deleted (:delete): FOLDERS, each (FOLDER WORDS OCTETS), what
was found, or the one being deleted followed by those still to go; FREED,
the octets freed so far, and DELETED, how many folders."
  phase folders (freed 0) (deleted 0))

(setf (documentation 'package-sweep-p 'function) "True when OBJECT is a PACKAGE-SWEEP.")

(tuatara.runtime:define-job-words package-sweep
  :doing "Removing package sources no longer named"
  :verb "remove package sources no longer named"
  :description (if (eq (package-sweep-phase job) :look)
                   "Looking for package sources no longer named"
                   "Deleting package sources no longer named")
  :left-words "nothing more deleted"
  :at-exit :quiet)

(defparameter *package-sweep-settings* '(*packages-directory* *declared-packages* *left-out-packages* *package-sources*)
  "What package-remove-unused's work in the background reads as it was on
the editor's thread when that work was started.")

(defun call-with-sweep-settings (values function)
  "Call FUNCTION with *PACKAGE-SWEEP-SETTINGS* bound to VALUES, as they were
on the editor's thread."
  (progv *package-sweep-settings* values (funcall function)))

(defun start-sweep-job (job work)
  "Start JOB, a PACKAGE-SWEEP, running WORK, a function of it, in the
background with the settings of the editor's thread now (CALL-WITH-SWEEP-SETTINGS)."
  (let ((settings (mapcar #'symbol-value *package-sweep-settings*)))
    (tuatara.runtime:start-job-thread job (lambda (job) (call-with-sweep-settings settings (lambda () (funcall work job))))
                                      :name "tuatara package sweep")))

(defun folder-octets (folder)
  "The octets FOLDER and what it holds take, links not followed; 0 when it is not there."
  (if (eq :directory (tuatara.io:path-kind (string-right-trim "/" folder) :follow nil))
      (or (rimu-tree-size folder) 0)
      0))

(define-command package-remove-unused ()
  "Delete the package sources Tuatara keeps that nothing in this session
names any more: a source no package-source names and no load-package
fetches from, or a package of a source no load-package names; one left out
by :disabled or :if counts as named. What would go is found in the
background and listed with its size, then asked once, with the total; nothing
is deleted without yes. Each is looked at again just before it is deleted,
as things are then: one something names by then, or whose source is being
fetched, checked or loaded from, stays. Its files compiled go with it, and
the space freed is said. Only the packages directory Tuatara keeps is
touched, never a folder of yours, never through a link. When the init file
was not read to its end, every package named after that point would look
unused, so nothing is done until it is. Who signed a source's versions is
still remembered: a package fetched from it again is checked against that
signer."
  (if *init-file-stop*
      (message "The init file was not read to its end (~d: ~a); nothing is deleted until it is"
               (car *init-file-stop*) (cdr *init-file-stop*))
      (start-sweep-job (make-package-sweep :look nil)
                       (lambda (job)
                         (setf (package-sweep-folders job)
                               (loop for (folder . words) in (unused-package-folders)
                                     collect (list folder words (+ (folder-octets folder)
                                                                   (folder-octets (package-compiled-folder folder))))))))))

(defvar *sweeping-folder* nil
  "The folder of the packages directory package-remove-unused is deleting
now, or NIL: a package kept in it is loaded once it is done (*AFTER-SWEEP*).")

(defvar *after-sweep* '()
  "What waits to be done once the folder being deleted is (*SWEEPING-FOLDER*),
the latest first, each a function.")

(defun wait-for-sweep (folder function)
  "When FOLDER is within the folder package-remove-unused is deleting now
\(*SWEEPING-FOLDER*), have FUNCTION called once that is done, and return
true; else NIL, nothing kept."
  (when (and *sweeping-folder* folder (tuatara.io:path-within-p (tuatara.io:native folder) *sweeping-folder*))
    (push function *after-sweep*)
    t))

(defun still-unused-p (folder)
  "True when FOLDER, found unused by package-remove-unused, is so still, as
things are now on the editor's thread: no package or source named since
wants it (UNUSED-PACKAGE-FOLDERS), and nothing of it is being fetched,
checked, decided on or loaded from."
  (and (member folder (unused-package-folders) :key #'car :test #'string=)
       (notany (lambda (loaded) (tuatara.io:path-within-p (second loaded) folder)) *loaded-package-folders*)))

(defun sweep-next (folders freed deleted)
  "Delete the first of FOLDERS (each (FOLDER WORDS OCTETS)) still unused
\(STILL-UNUSED-P), decided now on the editor's thread, in the background,
then the rest the same way; once none is left, say how many were deleted
and the octets FREED. DELETED counts those deleted so far."
  (let ((rest (member-if (lambda (entry) (still-unused-p (first entry))) folders)))
    (if (null rest)
        (message "Package sources no longer named deleted: ~d, ~a freed" deleted (tuatara.io:octets-words freed))
        (let ((job (make-package-sweep :delete rest)))
          (setf *sweeping-folder* (tuatara.io:native (first (first rest)))
                (package-sweep-freed job) freed
                (package-sweep-deleted job) deleted)
          (start-sweep-job job (lambda (job) (delete-unused-folder job (first (package-sweep-folders job)))))))))

(defun delete-unused-folder (job entry)
  "Delete ENTRY's folder, (FOLDER WORDS OCTETS), on the thread of JOB, a
PACKAGE-SWEEP, with its files compiled, what is remembered of the packages
in it forgotten (FORGET-PACKAGES-WITHIN), its octets added to those freed;
left as it is when it is no longer a folder of its own whose real path is in
the packages directory, as looked at again now."
  (destructuring-bind (folder words octets) entry
    (declare (ignore words))
    (when (and (eq :directory (tuatara.io:path-kind (string-right-trim "/" folder) :follow nil))
               (tuatara.io:really-within-p folder (tuatara.io:native (packages-directory))))
      (let ((compiled (package-compiled-folder folder)))
        (when (probe-file compiled) (delete-package-folder compiled)))
      (delete-package-folder folder)
      (forget-packages-within folder)
      (incf (package-sweep-freed job) octets)
      (incf (package-sweep-deleted job)))))

(defmethod tuatara.runtime:job-take-in ((job package-sweep))
  "Ask about what was found, go on to the next folder, or say what went wrong."
  (let ((job job))
    (when (eq (package-sweep-phase job) :delete)
      (setf *sweeping-folder* nil)
      (let ((waiting (reverse *after-sweep*)))
        (setf *after-sweep* '())
        (dolist (function waiting) (defer-action function))))
    (defer-action
     (lambda ()
       (case (package-sweep-status job)
         (:done (if (eq (package-sweep-phase job) :delete)
                    (sweep-next (rest (package-sweep-folders job)) (package-sweep-freed job) (package-sweep-deleted job))
                    (packages-sweep-ask (package-sweep-folders job))))
         (:failed (message "~a failed: ~a; ~:[nothing was deleted~;~:*~d deleted before it, ~a freed~]"
                           (if (eq (package-sweep-phase job) :look) "Looking for package sources no longer named" "Deleting a package source")
                           (or (tuatara.runtime:job-error job) "it ended unexpectedly")
                           (and (eq (package-sweep-phase job) :delete) (package-sweep-deleted job))
                           (tuatara.io:octets-words (package-sweep-freed job)))))))))

(defun busy-source-folders ()
  "The folders of the packages directory of the sources a fetch or check of
a package runs for now, or whose version waits to be decided on
\(*FETCHING-PACKAGES*, *PACKAGES-IN-HAND*)."
  (loop for (name source) in *declared-packages*
        when (and source (or (member name *fetching-packages* :test #'string=)
                             (assoc name *packages-in-hand* :test #'string=))
                  (not (local-source-p (package-where source))))
          collect (format nil "~a~a/" (tuatara.io:native (packages-directory)) (tuatara.io:path-file-name (package-where source)))))

(defun packages-sweep-ask (folders)
  "List FOLDERS, each (FOLDER WORDS OCTETS), found by package-remove-unused,
in *Help*, and ask once whether to delete them, with their total; then
delete them one by one, each looked at again first (SWEEP-NEXT)."
  (if (null folders)
      (message "No package source kept is unused")
      (let ((total (reduce #'+ folders :key #'third)))
        (show-help (format nil "Package sources kept that nothing names now, in ~a:~%~%~:{  ~10@a  ~a~%~}~%  ~10@a  in all~%"
                           (abbreviated-path (tuatara.io:native (packages-directory)))
                           (mapcar (lambda (f) (list (tuatara.io:octets-words (third f)) (second f))) folders)
                           (tuatara.io:octets-words total)))
        (if (yes-or-no-p* (format nil "Delete ~[no package source~;1 package source~:;~:*~d package sources~] no longer named, ~a?"
                                  (length folders) (tuatara.io:octets-words total)))
            (sweep-next folders 0 0)
            (message "Nothing deleted")))))

;;; The README of the package on the line, beside the list

(defun package-readme (folder)
  "The README of the package in FOLDER: README.org, README.md, README.txt or
README, in any case; NIL when it has none."
  (let ((files (ignore-errors (uiop:directory-files folder))))
    (loop for name in '("readme.org" "readme.md" "readme.txt" "readme")
          for file = (find name files :key #'file-namestring :test #'string-equal)
          when file return file)))

(defun package-preview-buffer (package)
  "The buffer showing what PACKAGE, (NAME SOURCE VERSION FILE), says of
itself, and true as a second value when it was opened for this: why it was
refused or failed when it was not loaded for that, else its README opened
safely, else a note that it has none or is not fetched yet."
  (destructuring-bind (name source version &rest more) package
    (declare (ignore more))
    (let* ((folder (nth-value 1 (package-state name source version)))
           (failure (and (not (package-loaded-p name)) (assoc name *package-failures* :test #'string=)))
           (readme (and folder (not failure) (package-readme folder))))
      (cond
        (failure
         (values (side-preview-note name (format nil "~a is not loaded: ~a" name (cdr failure))) t))
        (readme
         (multiple-value-bind (buffer existing) (tuatara.io:visit-file (tuatara.io:native readme) :safe t)
           (values buffer (not existing))))
        (t
         (values (side-preview-note name (format nil "~a ~:[is not fetched yet: there is nothing of it to show.~;~:*has no README in ~a.~]"
                                                 name (and folder (abbreviated-path (tuatara.io:native folder)))))
                 t))))))

(defmethod preview-entry ((mode packages-mode) list name)
  "The README of the package called NAME, or why it was not loaded, beside
the package manager (PACKAGE-PREVIEW-BUFFER)."
  (declare (ignore list))
  (let ((package (declared-package name)))
    (and package (package-preview-buffer package))))

(defmethod entry-preview-width ((mode packages-mode) list)
  "The columns of the package manager, its sources' as wide as
*PACKAGES-SOURCE-WIDTH* says."
  (declare (ignore list))
  (table-columns-width *packages-columns* *packages-source-width*))

(defun install-packages ()
  "Bind the package manager's keys, \\`P' turning the README beside it off and on."
  (entry-list-keys 'packages-mode "the package manager (\\[package-manager])")
  (define-keys *packages-keymap* '(("f" packages-update-marked)))
  (entry-preview-keys 'packages-mode "the package manager (\\[package-manager])"))

(commands-for-mode 'packages-mode "the package manager (\\[package-manager])" 'packages-update-marked)
