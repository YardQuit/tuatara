;;;; lisp/packages/manager.lisp --- The package manager: a list of the packages, and the init file they are named in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x package-manager lists the packages the init file names, with their state;
;;;; the commands of its mode update, remove and read about them, and package-scan
;;;; reads which the init file names.

(in-package #:tuatara.packages)

(define-command package-update ()
  "Fetch again each package load-package fetched (not the directories of your
disk), in the background. The version it asks for is looked up anew (the
newest tag, the newest commit, or the tag, branch or commit named). It is
fetched and loaded when it is another commit than the one there, asked about
first (*PACKAGE-ASK-BEFORE-LOADING*); the others are said to be up to date. A
version that needs a newer Tuatara is passed over for the newest that fits,
or the one loaded now kept; one signed by another than the source's first
version, or unsigned when that was signed, is refused (\[package-trust-again]
takes it); one that fails to load gives way to the version before it. A new
version of a package running now that lays out its structures otherwise
cannot replace it in this session: it is kept as the version taken and loads
at the next start, said."
  (update-packages (mapcar #'first *declared-packages*)))

(defun update-packages (names)
  "Fetch again the packages called NAMES that come from a repository, as
\\[package-update] does; say so when none of them does, and, when more than
one is fetched, once all of them are done (SAY-PACKAGE-UPDATE-DONE)."
  (let ((fetched (remove-if-not (lambda (p) (and (member (first p) names :test #'string=)
                                                 (second p)
                                                 (not (local-source-p (package-where (second p))))))
                                *declared-packages*)))
    (if (null fetched)
        (message "No package here is fetched from a repository")
        (let ((begun (loop for (name source version file) in fetched
                           for where = (package-where source)
                           when (start-package-fetch name where version file (package-key name where version))
                           collect name)))
          (when (rest begun)
            (setf *package-update-round* (union begun *package-update-round* :test #'string=)))))))

(define-major-mode packages-mode marked-list-mode
  "The package manager (\\[package-manager]): a line each package the init file
loads from a source, as the buffer list (\\[buffers]) has its buffers.
\\<support>\\[next-line] and \\[previous-line] move.
\\<packages-mode>\\[list-visit] or \\`RET' lists the directory of
the package on the line in rimu, \\[list-visit-other] in another window.
\\[quit-window] or \\`h' returns. \\[list-mark-entry] marks a package (>),
\\[list-mark-for-deletion] marks it for deletion (D), \\[list-unmark-entry]
unmarks it, \\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps
marked and unmarked. \\[package-update-marked] fetches the marked ones again
\(the one on the line when none is). \\[package-load] loads them,
\\[package-unload] unloads them from the running editor, \\[package-remove]
removes them from the packages loaded, \\[list-delete-marked] unloads,
removes and deletes them after asking (\\[package-delete]), \\[list-execute]
those marked for deletion. \\[revert-buffer] lists them again, marks kept,
as the list does by itself while it is shown. The README of the package on
the line is shown beside the list, \\[list-toggle-preview] turning that off
and on."
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
    (values entry (package-directory-of name where entry))))

(defun package-condition (name entry)
  "What is become of the package NAME this session, ENTRY what is remembered
of it. It is one of: restart (loaded, with a newer version taken that loads
at the next start, PACKAGE-AWAITS-RESTART-P), loaded, fetching, loading
\(compiled and loaded in the background), removed (not loaded though the init file names it), refused (it
uses names this Tuatara lacks), failed, waiting (for another, :after), later
\(to load on a command, a mode or once started), unloaded, declined (not let
load), kept or not fetched."
  (cond ((package-awaits-restart-p name) "restart")
        ((package-loaded-p name) "loaded")
        ((member name *fetching-packages* :test #'string=) "fetching")
        ((let ((job (cdr (assoc name *packages-in-hand* :test #'string=))))
           (and job (package-fetch-load job)))
         "loading")
        ((package-removed-p name) "removed")
        ((member name *refused-packages* :test #'string=) "refused")
        ((assoc name *package-failures* :test #'string=) "failed")
        ((find name *packages-waiting* :key #'second :test #'string=) "waiting")
        ((member name *deferred-packages* :test #'string=) "later")
        ((member name *unloaded-packages* :test #'string=) "unloaded")
        ((and entry (not (fourth entry))) "declined")
        (entry "kept")
        (t "not fetched")))

(defun package-signed-words (where)
  "Who signs the versions of the source WHERE, as the package manager says it:
the signer's name or key remembered (*PACKAGE-TRUST*), unsigned, nothing yet
or for a directory of your disk; \"unencrypted\" first for an address of
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
      (unless (member condition '("loaded" "restart") :test #'string=) :list-dim)))

(defun declared-package-condition (name)
  "How the package NAME stands as the init file declares it (PACKAGE-CONDITION),
or NIL when the init file does not name it."
  (let ((declared (declared-package name)))
    (and declared (package-condition name (package-state name (second declared) (third declared))))))

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
               (or (third entry) version (if (local-source-p where) "directory" ""))
               (if (and entry (string/= (second entry) "-")) (subseq (second entry) 0 (min 7 (length (second entry))))
                   "")
               condition
               (package-signed-words where)
               (if (local-source-p where) (abbreviated-path where) where)))))
   :after (if *declared-packages*
              (list (string #\Newline)
                    (format nil
                            "   ~[No packages~;1 package~:;~:*~d packages~], ~d loaded~%"
                            (length *declared-packages*)
                            (count-if (lambda (p) (package-loaded-p (first p))) *declared-packages*)))
              (list (format nil "~%   No packages: (load-package NAME) in the init file~%")))
   :point point
   :refill (lambda (list) (packages-render list :keep))))

(defun packages-key (list)
  "What the package manager LIST shows, cheaply; another key means it is out
of date (KEEP-SHOWN-CURRENT)."
  (declare (ignore list))
  (list *declared-packages* *loaded-packages* *unloaded-packages* *fetching-packages* *packages-in-hand*
        *package-failures* (state-list-entries *fetched-packages*) (state-list-entries *package-trust*)
        (state-list-entries *removed-packages*)))

(keep-shown-current 'packages-mode 'packages-key)

(define-command package-manager ()
  "Manage the packages the init file loads from a source (load-package with
:source) in a list, as the buffer list (\\[buffers]) manages buffers. Each
line has its mark (> marked, D marked for deletion), name, the version kept
\(its tag), its commit, and what is become of it (loaded; fetching; removed,
not loaded though the init file names it; refused, using names this Tuatara
lacks; failed; unloaded; declined, not let load; kept, not loaded; not
fetched). It also has who signs its source's versions (unsigned, and
unencrypted for an address of http:// or git://) and where it comes from. The
names of those not loaded are set apart. Why one was refused or failed is
shown beside the list in place of its README. \\<packages-mode>\\[list-visit]
or \\`RET' lists a package's directory in rimu, to read its source,
\\[list-visit-other] in another window. \\[list-mark-entry] marks it,
\\[list-mark-for-deletion] marks it for deletion, \\[list-unmark-entry] and
\\[list-unmark-all] unmark, \\[list-toggle-marks] swaps marked and unmarked, a
count marking as many. \\[package-update-marked] fetches the marked ones again,
\\[package-load] loads them, \\[package-unload] unloads them,
\\[package-remove] removes them from the packages loaded,
\\[list-delete-marked] unloads, removes and deletes them after asking
\(\\[package-delete]), \\[list-execute] those marked for deletion. The README
of the package on the line is shown beside the list,
\\[list-toggle-preview] turning that off and on. \\[package-update]
fetches them all again."
  (let ((list (buffer-in-mode "*Packages*" 'packages-mode)))
    (when (eq (tuatara.buffer:buffer-local list :list-preview :unset) :unset)
      (setf (list-preview-p list) t))
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

(defun scan-init-file-packages (file &optional names)
  "Load each package the init file FILE names with load-package and :source
that is not loaded or on its way (INIT-FILE-PACKAGE-FORMS), of NAMES alone
when given: its load-package evaluated by itself, as at the start, so that it
is fetched, asked about and loaded as there. Each found, (NAME LINE INSIDE
OUTCOME), OUTCOME what became of it (PACKAGE-CONDITION) or why it failed; as
a second value, where the reading stopped, (LINE . WHY), or NIL."
  (multiple-value-bind (found stopped) (init-file-package-forms file)
    (when names
      (setf found (remove-if-not (lambda (item) (member (second (first item)) names :test #'string=)) found)))
    (values
     (loop for (form line inside) in found
           for name = (second form)
           collect (list name line inside
                         (let ((before (declared-package-condition name)))
                           (if (member before '("restart" "loaded" "fetching" "loading" "later" "waiting")
                                       :test #'equal)
                               before
                               (handler-case
                                   (reporting-errors ("evaluating the package ~a" name)
                                                     (let ((*package* (find-package :tuatara-user)))
                                                       (clear-package-failure name)
                                                       (setf *refused-packages*
                                                             (remove name *refused-packages* :test #'string=))
                                                       (eval form)
                                                       (or (declared-package-condition name) "not named")))
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
      (refuse "No init file at ~a" (abbreviated-path file)))
    (multiple-value-bind (results stopped) (scan-init-file-packages file)
      (loop for (name line inside outcome) in results
            do (log-message (format nil
                                    "package-scan: ~a, line ~d~:[~;, inside another form (a parenthesis left open before it?)~]: ~a"
                                    name line inside outcome)))
      (when stopped
        (log-message (format nil
                             "package-scan: ~a could not be read on from line ~d: ~a; packages after it are not seen"
                             (abbreviated-path file) (car stopped) (cdr stopped))))
      (package-manager)
      (message "~[No packages~;1 package~:;~:*~d packages~] in ~a~[~:;, ~:*~d inside another form~]~[~:;, ~:*~d ~
                failed~]~@[, read up to line ~d~]: \\[view-echo-area-messages] says each"
               (length results) (abbreviated-path file)
               (count-if #'third results)
               (count-if (lambda (r) (uiop:string-prefix-p "failed" (fourth r))) results)
               (car stopped)))))

(defmethod list-entry-noun ((mode packages-mode))
  "A package."
  "package")

(defmethod visit-entry ((mode packages-mode) list name other)
  "List the directory of the package called NAME in rimu, in another window with
OTHER, opened safely, to read its source, each file previewed beside it;
\\<rimu-mode>\\[rimu-up] in the package's directory comes back to LIST."
  (let ((package (declared-package name)))
    (if (null package)
        (message "No package on this line")
        (destructuring-bind (name source version &rest more) package
          (declare (ignore more))
          (let ((directory (nth-value 1 (package-state name source version))))
            (if directory
                (rimu-show-directory (tuatara.io:native directory)
                                     :other other
                                     :safe t
                                     :root (tuatara.io:native directory)
                                     :back list)
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

(define-command package-update-marked ()
  "Fetch again the marked packages (>), or with none marked the one on the
current line, as \\[package-update] does them all."
  (act-on-list (current-buffer) (lambda (list names) (update-packages names) (unmark-list list)) "package"))

(defmethod delete-entries ((mode packages-mode) list names mark)
  "Unload, remove and delete the packages, after asking (\\[package-delete])."
  (declare (ignore mark))
  (start-packages-delete names)
  (unmark-list list))

(defun package-readme (directory)
  "The README of the package in DIRECTORY: README.org, README.md, README.txt or
README, in any case; NIL when it has none."
  (let ((files (ignoring-errors-reported ("reading ~a" directory) (uiop:directory-files directory))))
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
    (let* ((directory (nth-value 1 (package-state name source version)))
           (failure (and (not (package-loaded-p name)) (assoc name *package-failures* :test #'string=)))
           (readme (and directory (not failure) (package-readme directory))))
      (cond
        (failure
         (values (side-preview-note name (format nil "~a is not loaded: ~a" name (cdr failure))) t))
        (readme
         (multiple-value-bind (buffer existing) (tuatara.io:visit-file (tuatara.io:native readme) :safe t)
           (values buffer (not existing))))
        (t
         (values (side-preview-note name (format nil
                                                 "~a ~:[is not fetched yet: there is nothing of it to show.~;~:*has no README in ~a.~]"
                                                 name (and directory (abbreviated-path (tuatara.io:native directory)))))
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
  "Bind the package manager's keys, \\`P' turning the README beside it off and
on, \\`L', \\`K' and \\`R' loading, unloading and removing the packages marked."
  (entry-list-keys 'packages-mode "the package manager (\\[package-manager])")
  (define-keys *packages-keymap* '(("f" package-update-marked) ("L" package-load) ("K" package-unload)
                                   ("R" package-remove)))
  (entry-preview-keys 'packages-mode "the package manager (\\[package-manager])"))

(commands-for-mode 'packages-mode "the package manager (\\[package-manager])" 'package-update-marked)
