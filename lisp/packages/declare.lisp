;;;; lisp/packages/declare.lisp --- Packages from anywhere: a directory of a git repository, or of your disk.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package is a directory of Lisp files, NAME/NAME.lisp its entry (load.lisp
;;;; too), in a git repository anyone hosts, one package a repository or a
;;;; directory each, or in a directory of your own disk. There is no index: the init
;;;; file names where each comes from,
;;;;
;;;;   (package-source "mine" "https://example.org/my-packages.git")
;;;;   (load-package "example" :source "mine")
;;;;
;;;; and the first time Tuatara fetches only what it needs, in the background:
;;;; which commit the version asked for is (the repository's newest tag by date
;;;; unless said), then that commit alone and the package's directory alone. It is
;;;; kept in your data directory and loaded from there at every start, never
;;;; fetched again until M-x package-update. Before a package is first compiled
;;;; it asks, its source offered to read; then its files are compiled into the
;;;; cache once a version and loaded. A package has a Lisp package of its own
;;;; that uses TUATARA, the names lisp/api.lisp lists, and makes its settings
;;;; with DEFINE-PACKAGE-SETTING.
;;;;
;;;; A package says which Tuatara it needs in a line of its entry file's
;;;; header, ;;;; Needs: Tuatara 0.91.0, read without running it. A version
;;;; needing a newer one is never taken, an older one that fits taken in its
;;;; place. Who signed the commit or tag fetched is checked by git, and the
;;;; signer of a source's first version taken is remembered: a later version
;;;; signed by another, or unsigned when it was signed, is refused until you
;;;; trust the source again. The version taken and the one before it are
;;;; kept, the one before loaded again when the new one fails to load.

(in-package #:tuatara.packages)

(define-setting *package-ask-before-loading* t
  "Whether you are asked before a package is first compiled and loaded: one
fetched the first time, and again when an update brings another commit; one
of a directory of yours the first time only, what you change in it later being
yours.
- t: you are asked; y loads it, n leaves it, v shows its source first.
- nil: packages load without asking."
  :values boolean)

(define-setting *package-sources* '()
  "The places packages come from (PACKAGE-SOURCE sets them).
- a list of (NAME . WHERE): WHERE a git repository's address or a directory of
  your disk."
  :values (list-of "(NAME . WHERE)"))

(defvar *packages-kept* 500
  "How many fetched packages' versions are remembered.")

(defun package-line (entry)
  "The line of the packages file for ENTRY, (KEY COMMIT TAG ACCEPTED PREVIOUS
PREVIOUS-TAG SIGNER). COMMIT is a dash for a directory of your disk. PREVIOUS is
the commit taken before it and kept to fall back on, or NIL. SIGNER is who
signed COMMIT as git checked it when it was taken, a key's fingerprint or
\"unsigned\", or NIL when not known."
  (destructuring-bind (key commit tag accepted &optional previous previous-tag signer) entry
    (format nil "~a~c~a~c~a~c~:[no~;yes~]~@[~a~]" (escape-path key) #\Tab commit #\Tab (escape-path (or tag "")) #\Tab
            accepted
            (and (or previous signer)
                 (format nil "~c~a~c~a~@[~c~a~]" #\Tab (or previous "") #\Tab (escape-path (or previous-tag ""))
                         (and signer #\Tab) signer)))))

(defun parse-package-line (line)
  "The entry LINE of the packages file writes, (KEY COMMIT TAG ACCEPTED
PREVIOUS PREVIOUS-TAG SIGNER), or NIL for a line that makes no sense. An
empty or missing PREVIOUS, PREVIOUS-TAG or SIGNER is none; fields after those
are passed over (STATE-LINE-FIELDS)."
  (let ((fields (state-line-fields line)))
    (when (>= (length fields) 4)
      (destructuring-bind (key commit tag accepted &optional previous previous-tag signer &rest later) fields
        (declare (ignore later))
        (flet ((tag (text) (let ((tag (unescape-path text))) (and (plusp (length tag)) tag)))
               (commit-p (text) (cl-ppcre:scan "^(?:[0-9a-f]{7,64}|-)$" text)))
          (let ((previous (and previous (plusp (length previous)) previous))
                (signer (and signer (plusp (length signer)) signer)))
            (and (plusp (length key)) (commit-p commit) (or (null previous) (commit-p previous))
                 (or (null signer) (signer-fingerprint-p signer :stored t))
                 (list (unescape-path key) commit (tag tag) (string= accepted "yes")
                       previous (and previous previous-tag (tag previous-tag)) signer))))))))

(define-state-list *fetched-packages* "packages" 'parse-package-line 'package-line '*packages-kept*
                   :header (format nil "# The packages fetched, the latest first: the package, its source and the~%~
                       # version asked for; the commit fetched; its tag; whether it was let load;~%~
                       # the commit taken before it and its tag, kept to fall back on.~%")
                   :path-of #'first :fields 7 :trust t :line-key (field-line-key 4 0))

(defun fetched-package (key)
  "What is remembered of the package KEY names (PACKAGE-KEY), (KEY COMMIT TAG
ACCEPTED), or NIL."
  (state-list-find *fetched-packages* key))

(defun parse-removed-package-line (line)
  "The name of the package LINE of the file of packages removed writes, or
NIL for a line that is no package's name. Fields after it are passed over
\(STATE-LINE-FIELDS)."
  (let ((name (first (state-line-fields line))))
    (and (package-name-p name) name)))

(define-state-list *removed-packages* "packages-removed" 'parse-removed-package-line 'identity '*packages-kept*
                   :header (format nil "# The packages removed (M-x package-remove), the latest first: each is not~%~
                       # loaded though the init file names it, until M-x package-load loads it.~%"))

(defun package-removed-p (name)
  "True when the package NAME is removed (\\[package-remove]): its load-package
in the init file is passed over until \\[package-load] loads it."
  (and (state-list-find *removed-packages* name) t))

(defun tuatara-user:package-source (name where &key signers)
  "Name WHERE, a git repository's address or a directory of your disk (~/ for
your home directory), as the source NAME, for load-package's :source:
\(package-source \"mine\" \"https://example.org/my-packages.git\").

SIGNERS, a list of the keys' fingerprints that sign its versions (a secure
shell (SSH) key's SHA256:..., a GNU Privacy Guard (GPG) key's 40 hexadecimal
digits), takes from it only versions signed by one of them, from the first;
without it, whoever signed the first version taken is remembered and asked of
every later one. An address of http:// or git:// is allowed only by :plain t
on the load-package of each package fetched from it."
  (unless (and (stringp name) (plusp (length name)))
    (refuse "package-source takes a name and an address or a directory: ~s ~s" name where))
  (check-package-where where)
  (note-package-source-terms where signers)
  (setf *package-sources* (acons name where (alist-without name *package-sources*)))
  where)

(defvar *package-signers* '()
  "The signers the init file names for sources, each (WHERE . FINGERPRINTS):
only versions signed by one of FINGERPRINTS are taken from WHERE.")

(defvar *package-plain-allowed* '()
  "The packages the init file allows to be fetched from an address of http://
or git:// by :plain t on their load-package, each (NAME . WHERE).")

(defun alist-without (name alist)
  "ALIST, a list of (NAME . VALUE) keyed by names, without the entry for NAME."
  (remove name alist :key #'car :test #'string=))

(defun signer-fingerprint-p (text &key stored)
  "True when TEXT, a string, writes a key's fingerprint: an SSH key's
SHA256:... or a GNU Privacy Guard (GPG) key's 40 or 64 hexadecimal digits,
spaces among them allowed. With STORED, TEXT is as the state files of every
release keep one: no spaces, an SSH key's SHA256: and any of its letters, a
GPG key's 40 to 64 digits in capitals, and \"unsigned\" and the key:... form
of a key your keyring lacks count too."
  (if stored
      (and (or (string= text "unsigned")
               (cl-ppcre:scan "^(?:SHA256:[A-Za-z0-9+/=]+|[0-9A-F]{40,64}|key:(?:[0-9A-F]{40}|[0-9A-F]{64}))$" text))
           t)
      (and (cl-ppcre:scan "^(?:SHA256:[A-Za-z0-9+/]{43}=?|[0-9A-Fa-f]{40}|[0-9A-Fa-f]{64})$" (remove #\Space text))
           t)))

(defun package-name-p (name)
  "True when NAME is a package's name: small letters, digits and -, not
beginning with a -."
  (and (stringp name) (cl-ppcre:scan "^[a-z0-9][a-z0-9-]*$" name) t))

(defun signer-fingerprint (text)
  "The key's fingerprint TEXT writes, as it is compared: spaces taken out, and
a GNU Privacy Guard (GPG) key's hexadecimal digits in capitals; a secure shell
(SSH) key's (SHA256:...) as written."
  (let ((bare (remove #\Space text)))
    (if (every (lambda (c) (digit-char-p c 16)) bare) (string-upcase bare) bare)))

(defun note-package-source-terms (where signers)
  "Remember for the source WHERE the SIGNERS its versions must be signed by,
as package-source and load-package give them; an error for SIGNERS that is no
list of fingerprints."
  (unless (listp signers)
    (refuse "A source's :signers are a list of keys' fingerprints: ~s" signers))
  (dolist (signer signers)
    (let ((bare (and (stringp signer) (remove #\Space signer))))
      (cond ((and bare (signer-fingerprint-p bare)))
            ((and bare (cl-ppcre:scan "^(?:0x)?[0-9A-Fa-f]{8,16}$" bare))
             (refuse "~s in :signers is a GPG key's ID: name its whole fingerprint, the 40 hexadecimal digits gpg ~
                      --fingerprint shows for the key itself (pub), not a subkey's" signer))
            (t (refuse "~s in :signers is no key's fingerprint: an SSH key's SHA256:... (ssh-keygen -lf KEY.pub) or a ~
                        GPG key's 40 hexadecimal digits (gpg --fingerprint)" signer)))))
  (when signers
    (setf *package-signers* (acons where (mapcar #'signer-fingerprint signers)
                                   (alist-without where *package-signers*)))))

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
    (format nil
            "The package ~a is not fetched from ~a: over http:// or git:// anyone on the way can read and change it. ~
                 Use https://, ssh or a directory of your disk, or allow this source by adding :plain t to its ~
                     load-package"
            name where)))

(defun package-where (source)
  "Where SOURCE, a source's name (PACKAGE-SOURCE) or itself an address or a
directory, is."
  (or (cdr (assoc source *package-sources* :test #'string=)) source))

(defun local-source-p (where)
  "True when WHERE is a directory of your disk rather than a repository's address."
  (and (plusp (length where)) (find (char where 0) "/~")))

(defun check-package-where (where)
  "Signal an error unless WHERE, a package's source, is an address or a directory
named whole (/ or ~/ first). One named from the directory Tuatara starts in would
be another directory at each start. One that starts with - would be read by git
as an option. A line of the lists of packages that starts with @ is of a kind
a later release adds."
  (unless (and (stringp where) (plusp (length where)))
    (refuse "A package's source is an address or a directory: ~s" where))
  (when (char= (char where 0) #\-)
    (refuse "A package's source cannot start with -: ~s" where))
  (when (char= (char where 0) #\@)
    (refuse "A package's source cannot start with @: ~s" where))
  (when (char= (char where 0) #\.)
    (refuse "A package's directory is named whole, from / or ~~/: ~s" where))
  (when (cl-ppcre:scan "^[a-z][a-z0-9+.-]*://[^/@]*@" where)
    (refuse "A package's address cannot hold a name or a password (@): ~s" where))
  (when (cl-ppcre:scan "(?i)^ftps?://" where)
    (refuse "A package is fetched over https://, ssh or from a directory of your disk, not over ftp: ~s" where)))

(defun package-key (name where version)
  "What a package called NAME from WHERE, VERSION asked for, is remembered by."
  (format nil "~a ~a ~a" name where (or version "release")))

(define-setting *packages-directory* nil
  "The directory Tuatara keeps the packages it fetches in. As *CACHE-DIRECTORY*:
made by you, and while it is not there no package is fetched or loaded from
it.
- nil: ~/.local/share/tuatara/packages/ ($XDG_DATA_HOME/tuatara/packages/),
  the standard place.
- a directory's name: that directory."
  :values (or (member nil) directory))

(defun packages-directory ()
  "Where fetched packages are kept: *PACKAGES-DIRECTORY* when set
\(TUATARA.IO:CHOSEN-DIRECTORY), else tuatara/packages/ in your data directory."
  (tuatara.io:native (tuatara.io:chosen-directory '*packages-directory*
                                                  (merge-pathnames "tuatara/packages/" (tuatara.io:data-home)))))

(defun package-kept-directory (where name)
  "The directory the package NAME of the repository at WHERE is kept in, a directory
for each commit of it fetched."
  (format nil "~a~a/~a/" (packages-directory) (tuatara.io:path-file-name where) name))

(defun package-directory-for (where name commit)
  "The directory a commit of the repository at WHERE is kept in for the package NAME."
  (format nil "~a~a/" (package-kept-directory where name) commit))

;;; Declaring packages: load-package

(defvar *package-directory* nil
  "The directory of the package being loaded, for LOAD-PACKAGE-FILE.")

(defun package-directory-native ()
  "The native name of the directory of the package being loaded, or NIL."
  (and *package-directory* (tuatara.io:native *package-directory*)))

(defvar *loaded-packages* '()
  "The names of the packages loaded this session.")

(defvar *unloaded-packages* '()
  "The names of the packages unloaded this session (\\[package-unload]) and not
loaded again since.")

(defvar *package-code* '()
  "The Lisp packages each package loaded this session made, each (NAME .
PACKAGES): what unloading it takes out (FORGET-PACKAGE-CODE).")

(defvar *package-command-stubs* '()
  "The commands of packages not loaded yet that load them when run
\(PACKAGE-COMMAND-STUB), each (COMMAND NAME . FUNCTION), NAME the package.")

(defvar *removed-packages-said* '()
  "The names of the packages removed that this session has said it passes over.")

(defvar *fetching-packages* '()
  "The names of the packages being fetched.")

(defvar *package-failures* '()
  "Why packages were not fetched or loaded this session, each (NAME . WORDS).")

(defun note-package-failure (name words)
  "Remember that the package NAME was not fetched or loaded, for WORDS; one
entry for each name, the latest."
  (setf *package-failures* (acons name words (alist-without name *package-failures*))))

(defun clear-package-failure (name)
  "Forget why the package NAME was not fetched or loaded."
  (setf *package-failures* (alist-without name *package-failures*)))

(defvar *packages-mixed* '()
  "The names of the packages whose new version failed part of the way as it
loaded over the one running this session: parts of both run, and only a new
start runs one whole.")

(defvar *refused-packages* '()
  "The names of the packages refused this session for using names this
Tuatara lacks; why is in *PACKAGE-FAILURES*.")

(defvar *declared-packages* '()
  "The packages load-package names this session, each (NAME SOURCE VERSION
FILE DIRECTORY); the first named first.")

(defvar *left-out-packages* '()
  "The packages a load-package left out this session (:disabled, or :if
giving NIL) that name a source, each (NAME SOURCE): what is kept of them is
still wanted.")

(defun declared-package (name)
  "The package called NAME as load-package named it this session, (NAME SOURCE
VERSION FILE DIRECTORY), or NIL."
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

(defparameter *load-package-keywords*
  '(:source :version :file :directory :signers :plain :disabled :if :defer :after :commands :modes :init :config)
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
                       (refuse "load-package ~s is inside this one, after ~(~s~): a parenthesis is left open before it"
                               (second extra) key)
                       (refuse "load-package's ~(~s~) takes one value, not also ~s" key extra))))
               (setf (getf parsed key) (reverse items)))))
      (dolist (item options)
        (if (and (keywordp item)
                 (or (member item *load-package-keywords*) (null key) (not (member key '(:init :config)))))
            (progn
              (unless (member item *load-package-keywords*)
                (refuse "load-package takes ~{~(~s~)~^, ~}, not ~(~s~)" *load-package-keywords* item))
              (close-key)
              (setf key item items '()))
            (progn
              (unless key (refuse "load-package takes a keyword after the name, not ~s" item))
              (push item items))))
      (close-key))
    parsed))

(defmacro tuatara-user:load-package (name &rest options)
  "Load the package NAME (a string: \"example\"), or set up a part of
Tuatara, the settings and keys of it together. What follows the name, each
keyword with its value, or for the last four with any number of them:

:source  where it comes from: a source's name (package-source), a git
         repository's address or a directory of your disk. Without it, NAME is
         \"tuatara\", a mode's name (\"org\" for org-mode) or a name of
         your own for a group of settings (\"writing\"), which load nothing
         and run :init and :config at once. A name a letter or two from a
         package's or a mode's is taken for a mistyping of it, an error
         saying which.
:version the tag, branch or commit fetched, the repository's newest tag by
         date when not given or \"release\", its newest commit for \"latest\"
         or when not given and it has no tag yet.
:directory  its directory in the repository or source directory, when not NAME.
:file    its first file, when not NAME.lisp or load.lisp.
:signers the fingerprints of the keys its versions must be signed by, a
         list (as package-source takes them); only those are taken.
:plain   t to allow an address of http:// or git://, whose transport is not
         encrypted; refused without it.
:disabled t to leave the whole block out, as if it were not there.
:if      a form; the block is left out when it gives NIL, as on another
         machine, (:if (string= (machine-instance) \"laptop\")).
:defer   t to load it once the editor has started rather than while the init
         file is read.
:after   another package's or group's name; loaded, or for a group its
         :config run, only once that one has. There is no :before: A
         before B is B :after A.
:commands commands of it, loaded when one of them is first run (\\[execute-extended-command] offers
         them meanwhile, a key may be bound to them).
:modes   modes, loaded when a buffer is first in one of them (\"csv\" for csv-mode).
:init    forms evaluated at once, before it loads.
:config  forms evaluated once it has loaded, its settings and keys (bind-key
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
                        :directory ,(one :directory) :signers ,(one :signers) :plain ,(one :plain)
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
      (handler-case (reporting-errors ("evaluating ~a's ~(~s~)" name what)
                                      (handler-bind ((warning #'muffle-warning)) (eval form)))
        (error (e)
          (message "~a's ~(~s~): ~a failed: ~a" name what (let ((*print-length* 4) (*print-level* 2))
                                                            (prin1-to-string form)) e))))))

(defun package-loaded-p (name)
  "True when the package NAME has loaded this session."
  (member name *loaded-packages* :test #'string=))

(defun package-loaded (name)
  "What follows the package NAME's loading: it counts as loaded, its :config is
evaluated, the packages waiting for it (:after) load, and the commands of it
run before it had loaded run (*COMMANDS-AFTER-LOAD*)."
  (pushnew name *loaded-packages* :test #'string=)
  (setf *unloaded-packages* (remove name *unloaded-packages* :test #'string=))
  (run-package-forms name :config (cdr (assoc name *package-configs* :test #'string=)))
  (let ((waiting (remove name *packages-waiting* :key #'car :test-not #'string=)))
    (setf *packages-waiting* (alist-without name *packages-waiting*))
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

(defun say-package-needs-restart (name &optional version)
  "Say that the version of the package NAME taken, VERSION (its tag or the
start of its commit) when given, is fetched and loads at the next start, the
version running now going on until then."
  (message "The package ~a~@[ ~a~] is fetched; restart Tuatara to use it" name version))

(defun package-load-ended (name where start loaded failure)
  "What follows the loading of the package NAME from WHERE, begun at START
\(internal real time): when it loaded (LOADED, the file it was loaded by),
PACKAGE-LOADED, and the milliseconds it took said in *Messages*; when FAILURE,
the error that ended it, said on the echo row too, the package not counted as
loaded. A PACKAGE-NEEDS-RESTART is no failure: the version running goes on,
said in *Messages* alone. Returns LOADED, or NIL when it failed."
  (flet ((failed (e)
           (note-package-failure name (princ-to-string e))
           (if (typep e '(or package-lacks-names package-load-stopped))
               (progn (pushnew name *refused-packages* :test #'string=)
                      (message "~a" e))
               (message "Loading package ~a...failed: ~a" name e))
           nil))
    (cond
      ((typep failure 'package-needs-restart)
       (log-message (princ-to-string failure))
       nil)
      (failure (failed failure))
      (t
        (handler-case
            (reporting-errors ("ending the loading of the package ~a" name)
                              (package-loaded name)
                              (let ((ms (round (tuatara.runtime:milliseconds-since start))))
                                (when *start-package-times* (push (cons name ms) (cdr *start-package-times*)))
                                (log-message (format nil "Loading package ~a from ~a...done (~d ms)" name where ms)))
                              loaded)
          (error (e) (failed e)))))))

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
file loaded, or NIL when it failed, and as a second argument :STOPPED when it
was stopped on purpose, :RESTART when it loads only at the next start
\(PACKAGE-NEEDS-RESTART), else NIL; what it returns is returned. While the editor's
session runs (LOAD-IN-BACKGROUND-P) the package is loaded in the background
\(START-PACKAGE-LOAD). COMPILE, a function of the job, compiles it first
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
                                     (let ((loaded (package-load-ended name where start loaded failure))
                                           (restart (typep failure 'package-needs-restart)))
                                       (cond (given)
                                             ((and loaded compiled)
                                              (message "The package ~a is compiled and loaded" name))
                                             (restart (say-package-needs-restart name)))
                                       (if given
                                           (funcall then loaded (cond (stopped :stopped) (restart :restart)))
                                           (funcall then loaded)))))
               nil)
        (multiple-value-bind (loaded failure) (handler-case (reporting-errors ("loading the package ~a" name)
                                                                              (funcall load))
                                                (error (e) (values nil e)))
          (funcall then (package-load-ended name where start loaded failure))))))

(defun package-command-stub (command name start)
  "Make COMMAND, a command of the package NAME not loaded yet, one that calls
START to load it and then runs the command the package has of that name. It
runs at once when the package loads at once. Otherwise it runs once the
package has loaded (asked about first, or fetched), or says why it could not
load. A command of that name there is already is left as it is."
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
                     (install-command command real)
                     (funcall real)
                     t))))
        (setf stub (lambda ()
                     (funcall start)
                     (let ((failure (cdr (assoc name *package-failures* :test #'string=))))
                       (cond ((run-real))
                             (failure (message "~(~a~) is of the package ~a, which could not be loaded: ~a" command
                                               name failure))
                             (t (setf *commands-after-load*
                                      (cons (list* name command #'run-real)
                                            (remove command *commands-after-load* :key #'second)))
                                (message "~(~a~) runs once the package ~a has loaded~:[~; (being fetched)~]"
                                         command name (member name *fetching-packages* :test #'string=)))))))
        (install-command command stub
                         (format nil "A command of the package ~a: running it loads the package (its load-package's ~
                                      :commands), then runs the package's own." name))
        (setf *package-command-stubs* (acons command (cons name stub)
                                             (remove command *package-command-stubs* :key #'car)))))))

(defun say-package-removed (name)
  "Say in *Messages* and the debug log, once a session, that the package NAME
is removed and so not loaded (PACKAGE-REMOVED-P)."
  (unless (member name *removed-packages-said* :test #'string=)
    (push name *removed-packages-said*)
    (tuatara.runtime:debug-report :package "~a is removed: its load-package is passed over" name)
    (log-message (substitute-command-keys
                  (format nil "The package ~a is removed and not loaded, though the init file names it; ~
                               \\[package-load] loads it" name)))))

(defun arrange-package-load (name load &key defer after commands modes)
  "Have LOAD, a function loading the package NAME, called once: at once; with
AFTER, once the package AFTER has loaded; with COMMANDS or MODES, when one of
those commands is first run or a buffer is first in one of those modes; with
DEFER alone, once the editor has started. Never once NAME is removed
\(PACKAGE-REMOVED-P)."
  (let* ((started nil)
         (start (lambda ()
                  (unless started
                    (setf started t
                          *deferred-packages* (remove name *deferred-packages* :test #'string=))
                    (cond ((package-removed-p name) (say-package-removed name))
                          ((and after (not (package-loaded-p after)))
                           (push (list* after name load) *packages-waiting*))
                          (t (funcall load)))))))
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
                        (setf (aref d i j)
                              (min (1+ (aref d (1- i) j)) (1+ (aref d i (1- j))) (+ (aref d (1- i) (1- j)) cost)))
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
                                              (if (uiop:string-suffix-p text "-mode")
                                                  (subseq text 0 (- (length text) 5))
                                                  text)))
                             (mode-names))))))

(defun declare-package
    (name &key left-out source version file directory signers plain defer after commands modes init config)
  "Load the package NAME as LOAD-PACKAGE says, each keyword as it takes it,
INIT and CONFIG the forms after :init and :config; LEFT-OUT :disabled or :if
when the block is left out, said in *Messages*. A package removed
\(PACKAGE-REMOVED-P) is listed, nothing of it run or loaded, and that said
once. Returns the file loaded, or NIL when it is loaded later or nothing is."
  (when left-out
    (log-message (format nil "~a is left out (~(~s~))" name left-out))
    (when (and (stringp source) (plusp (length source)))
      (pushnew (list name source) *left-out-packages* :test #'equal))
    (return-from declare-package nil))
  (unless (package-name-p name)
    (refuse "A package's name is small letters, digits and -: ~s" name))
  (when (and after (not (package-name-p after)))
    (refuse "A package's :after is another package's name: ~s" after))
  (dolist (command commands)
    (unless (symbolp command) (refuse "A package's :commands are commands' names: ~s" command)))
  (setf modes (mapcar (lambda (mode) (if (stringp mode) (intern (string-upcase mode) :keyword) mode)) modes))
  (mapc #'mode-named modes)
  (setf *package-configs* (acons name config (alist-without name *package-configs*)))
  (flet ((declared (entry)
           (setf *declared-packages* (append (remove name *declared-packages* :key #'first :test #'string=)
                                             (list entry))))
         (arrange (load)
           (let ((result nil))
             ;; a package still being fetched loads later: no file yet
             (arrange-package-load name (lambda () (let ((loaded (funcall load)))
                                                     (setf result (and (or (stringp loaded) (pathnamep loaded))
                                                                       loaded))))
                                   :defer defer :after after :commands commands :modes modes)
             result)))
    (cond ((and source (package-removed-p name))
           ;; listed as removed, nothing of it run
           (when (and (stringp source) (plusp (length source)))
             (declared (list name source version file directory)))
           (say-package-removed name)
           nil)
          (source
           (run-package-forms name :init init)
           (unless (and (stringp source) (plusp (length source)))
             (refuse "A package's :source is a source's name, an address or a directory: ~s" source))
           (check-package-where (package-where source))
           (note-package-source-terms (package-where source) signers)
           (setf *package-plain-allowed* (alist-without name *package-plain-allowed*))
           (when plain (push (cons name (package-where source)) *package-plain-allowed*))
           (when (and version (not (and (stringp version) (cl-ppcre:scan "^[^\\s~^:?*\\[\\\\]+$" version))))
             (refuse "A package's :version is a tag, a branch or a commit: ~s" version))
           (when (and file (not (and (stringp file) (cl-ppcre:scan "^[A-Za-z0-9._-]+\\.lisp$" file))))
             (refuse "A package's :file is a .lisp file of its directory: ~s" file))
           (when (and directory (not (and (stringp directory) (cl-ppcre:scan "^[A-Za-z0-9_][A-Za-z0-9._-]*$"
                                                                             directory))))
             (refuse "A package's :directory is the name of a directory in its repository or source directory: ~s"
                     directory))
           (declared (list name source version file directory))
           (arrange (lambda () (load-package-from name source version file))))
          (t
           (let ((mode (and (string/= name "tuatara") (ignoring-errors-reported ("finding the mode ~a" name)
                                                                                (mode-named (intern (string-upcase name) :keyword)))))
                 (near nil))
             (unless (or mode (string= name "tuatara"))
               (setf near (known-package-name-near name))
               (when near
                 (refuse "No package or mode called ~a: did you mean ~a?" name near)))
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
          (log-message (format nil
                               "Packages: ~d loaded in ~d ms~[~:;, ~:*~d to load later~]~[~:;, ~:*~d being fetched~]"
                               (length times) (reduce #'+ times :key #'cdr)
                               (length *deferred-packages*) (length *fetching-packages*))))
        (dolist (waiting *packages-waiting*)
          (unless (or (declared-package (car waiting))
                      (member (car waiting) *deferred-packages* :test #'string=))
            (message "~a waits for ~a (:after), which no load-package before it or after it names"
                     (second waiting)
                     (car waiting))))))))

;;; load-package with a source

(defun load-or-ask (name where directory file accept)
  "Load the package NAME from DIRECTORY, of WHERE, once let: at once when
*PACKAGE-ASK-BEFORE-LOADING* is NIL, else after asking (PACKAGE-QUESTION),
once the editor has started; ACCEPT, a function, remembers that it was let."
  (flet ((load-it () (funcall accept) (load-package-directory directory name file)))
    (if *package-ask-before-loading*
        (defer-action (lambda () (package-question name where directory #'load-it)))
        (load-it))))

(defun load-package-from (name source version file)
  "Load the package NAME from SOURCE (LOAD-PACKAGE). A directory of your disk is
loaded at once, asked about the first time. One fetched before is loaded from
where it was kept, the version before it in its place when it fails to load
\(LOAD-VERSION). That holds unless it is to be asked about, or who signed it is
to be checked again first against the trust in force
\(KEPT-VERSION-CHECK-NEEDED-P), in the background (START-PACKAGE-CHECK).
Otherwise it is fetched in the background first."
  (let* ((where (package-where source))
         (key (package-key name where version))
         (entry (fetched-package key))
         (directory (package-directory-of name where entry)))
    (cond ((and (not (local-source-p where))
                (wait-for-sweep (package-kept-directory where name)
                                (lambda () (load-package-from name source version file))))
           nil)
          ((not (or (local-source-p where) (and directory (probe-file directory))))
           (start-package-fetch name where version file key :quiet t))
          ((and entry (not (local-source-p where))
                (or (not (fourth entry)) (kept-version-check-needed-p where entry))
                (eq :directory (tuatara.io:path-kind (tuatara.io:native directory))))
           (start-package-check name where version file key entry :quiet t)
           nil)
          ((not (eq :directory (tuatara.io:path-kind (tuatara.io:native directory))))
           (message "The package ~a is not loaded: its directory ~a is not there" name
                    (abbreviated-path (tuatara.io:native directory))))
          ((and entry (fourth entry) (local-source-p where))
           (load-package-directory directory name file))
          ((and entry (fourth entry))
           (multiple-value-bind (outcome loaded) (load-version name where directory file
                                                               (and (fifth entry)
                                                                    (list key (fifth entry) (sixth entry) t)))
             (declare (ignore outcome))
             loaded))
          (t (load-or-ask name where directory file
                          (lambda () (state-list-put *fetched-packages*
                                                     (list (list key (if entry (second entry) "-") (third entry)
                                                                 t)))))))))
