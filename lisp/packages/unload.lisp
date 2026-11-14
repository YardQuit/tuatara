;;;; lisp/packages/unload.lisp --- Packages unloaded, removed, deleted, and loaded again.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package is unloaded from the running editor, removed from the packages
;;;; loaded (its load-package passed over, the init file left as it is), or
;;;; deleted: unloaded, removed, and what is kept of it deleted in the
;;;; background as package-remove-unused deletes. package-load loads one again
;;;; by its load-package. Each acts on the packages marked in the package
;;;; manager, or there and elsewhere on one asked for.

(in-package #:tuatara.packages)

(defun unload-package (name)
  "Take the package NAME, loaded, out of the running editor: what its code put
there is taken out and its Lisp packages deleted (FORGET-PACKAGE-CODE), and
it counts as not loaded. Its load-package stays, so it loads again at the
next start, or now by \\[package-load]. True when it had Lisp packages of its
own to take out; what a package defines in another's stays until Tuatara
ends."
  (let ((own (remove-if-not #'package-name (cdr (assoc name *package-code* :test #'string=))))
        (directory (second (assoc name *loaded-package-directories* :test #'string=))))
    (forget-package-code own directory)
    (setf *package-code* (alist-without name *package-code*)
          *loaded-packages* (remove name *loaded-packages* :test #'string=)
          *packages-mixed* (remove name *packages-mixed* :test #'string=)
          *loaded-package-directories* (remove name *loaded-package-directories* :key #'first :test #'string=))
    (setf *package-needs* (alist-without name *package-needs*))
    (pushnew name *unloaded-packages* :test #'string=)
    (tuatara.runtime:debug-report :package "~a unloaded, ~d Lisp package~:p of its own deleted" name (length own))
    (and own t)))

(defun remove-package (name)
  "Unload the package NAME when it is loaded (UNLOAD-PACKAGE) and remove it:
it is not loaded from now on, at the next start neither, though the init file
names it, until \\[package-load] loads it (*REMOVED-PACKAGES*). What was
arranged to load it later is taken back: its commands that would load it
\(:commands), and its waiting for another (:after) or for the start (:defer)."
  (when (package-loaded-p name)
    (unload-package name))
  (state-list-put *removed-packages* (list name))
  (loop for (command owner . stub) in *package-command-stubs*
        when (and (string= owner name) (eq (gethash command *commands*) stub))
        do (forget-command command))
  (setf *package-command-stubs* (remove name *package-command-stubs* :key #'second :test #'string=)
        *deferred-packages* (remove name *deferred-packages* :test #'string=)
        *packages-waiting* (remove name *packages-waiting* :key #'second :test #'string=)
        *commands-after-load* (remove name *commands-after-load* :key #'first :test #'string=))
  (tuatara.runtime:debug-report :package "~a removed: its load-package is passed over" name))

(defun load-packages-again (names)
  "Load the packages called NAMES by their load-package in the init file, as
at the start (SCAN-INIT-FILE-PACKAGES), each removed one no longer removed;
say what became of each, in *Messages* too."
  (let ((file (config-file)))
    (unless (probe-file file)
      (refuse "No init file at ~a" (abbreviated-path file)))
    (state-list-drop *removed-packages* names)
    (setf *removed-packages-said* (set-difference *removed-packages-said* names :test #'string=))
    (let* ((results (scan-init-file-packages file names))
           (words (loop for name in names
                        for found = (find name results :key #'first :test #'string=)
                        collect (format nil "~a ~:[has no load-package in the init file~;~:*~a~]" name
                                        (fourth found)))))
      (dolist (line words) (log-message (format nil "package-load: ~a" line)))
      (message "~{~a~^; ~}" words))))

(defun act-on-packages (verb candidates act)
  "Call ACT with the names of the packages a command acts on: in the package
manager, those marked (>), or with none marked the one on the line
\(ACT-ON-LIST), then unmark them; elsewhere one asked for among CANDIDATES,
VERB (\"unload\") saying what is done to it, or that there is none said."
  (cond ((typep (buffer-major-mode (current-buffer)) 'packages-mode)
         (act-on-list (current-buffer) (lambda (list names) (funcall act names) (unmark-list list)) "package"))
        ((null candidates)
         (message "No package to ~a" verb))
        (t
         (let ((name (completing-read (format nil "~@(~a~) the package: " verb) candidates :require-match t)))
           (when (member name candidates :test #'string=)
             (funcall act (list name)))))))

(defun packages-not-busy (names)
  "NAMES, the packages that are not being fetched, checked or loaded now
\(PACKAGE-BUSY-P); those that are said to be so."
  (let ((busy (remove-if-not #'package-busy-p names)))
    (when busy
      (message "~{~a~^, ~} ~:[is~;are~] being fetched or loaded: try again once ~:*~:[it has~;they have~]"
               busy (rest busy)))
    (set-difference names busy :test #'string=)))

(defun source-packages ()
  "The names of the packages the init file loads from a source, the first named first."
  (loop for (name source) in *declared-packages* when source collect name))

(define-command package-unload ()
  "Take a package out of the running editor: its commands and keys, with their
labels, its modes, the places it gave Tuatara's commands, its ways of showing
text, its functions on Tuatara's lists of hooks and of work to do now and
then, its settings and its work in the background, which is stopped. A buffer
in one of its modes is put in the mode its file calls for, or in
fundamental-mode, its text and any changes not saved kept as they are; kill
it from the buffer list (\\[buffers]) when it is of no more use. The package's
faces stay. Its code stays on disk, and its load-package in the init file, so
it loads again at the next start, or now by \\[package-load]. In the package
manager (\\[package-manager]) it unloads the packages marked (>), or the one on
the line."
  (act-on-packages "unload" (remove-if-not #'package-loaded-p (source-packages))
                   (lambda (names)
                     (let ((loaded (remove-if-not #'package-loaded-p (packages-not-busy names))))
                       (if (null loaded)
                           (message "~:[None of them is~;~:*~a is not~] loaded" (and (null (rest names)) (first names)))
                           (let ((kept (remove-if #'unload-package loaded)))
                             (message "~{~a~^, ~} unloaded; \\[package-load] loads ~:[it~;them~] again~@[; ~a~]"
                                      loaded (rest loaded)
                                      (and kept (format nil
                                                        "what ~{~a~^, ~} defined outside a Lisp package of ~:[its~;their~] own stays until Tuatara ends"
                                                        kept (rest kept))))))))))

(define-command package-remove ()
  "Unload a package (\\[package-unload]) and take it out of the packages loaded:
it is not loaded from now on, nor at the next start, though the init file
names it; the init file is left as it is, and Tuatara says in *Messages* once
a session that it passes the package over. Its code and compiled files stay
on disk; \\[package-load] loads it again, and \\[package-delete] deletes it. In
the package manager (\\[package-manager]) it removes the packages marked (>), or
the one on the line."
  (act-on-packages "remove" (remove-if #'package-removed-p (source-packages))
                   (lambda (names)
                     (let ((names (packages-not-busy names)))
                       (when names
                         (mapc #'remove-package names)
                         (message "~{~a~^, ~} removed: not loaded from now on, though the init file names ~
                                   ~:[it~;them~]; \\[package-load] loads ~:*~:[it~;them~] again"
                                  names (rest names)))))))

(defun package-deletion-entry (name)
  "What deleting the package NAME deletes, as package-remove-unused's work
takes it, (DIRECTORY WORDS OCTETS NAME), OCTETS 0 until measured: the directory its
versions fetched are kept in, or for a directory of your disk the directory its
files are compiled into, the directory of yours left as it is."
  (let ((where (package-where (second (declared-package name)))))
    (if (local-source-p where)
        (list (compiled-directory-of (package-directory-of name where nil))
              (format nil "~a, its compiled files (~a stays)" name (abbreviated-path where)) 0 name)
        (list (package-kept-directory where name) (format nil "~a from ~a" name where) 0 name))))

(defun start-packages-delete (names)
  "Measure in the background what deleting the packages NAMES deletes
\(PACKAGE-DELETION-ENTRY), then ask (PACKAGES-DELETE-ASK)."
  (let ((names (packages-not-busy (remove-if-not #'declared-package names))))
    (when names
      (start-sweep-job (make-package-sweep :look (mapcar #'package-deletion-entry names) names)
                       (lambda (job)
                         (setf (package-sweep-directories job)
                               (loop for (directory words nil name) in (package-sweep-directories job)
                                     collect (list directory words (sweep-entry-octets directory) name))))))))

(defun packages-delete-ask (names directories)
  "Ask once whether to unload, remove and delete the packages NAMES, DIRECTORIES
\(each (DIRECTORY WORDS OCTETS NAME)) what is deleted of them, with their total;
on yes, remove each (REMOVE-PACKAGE), forget that one of a directory of yours
was let load, and delete the directories one by one in the background, each
looked at again first (SWEEP-NEXT)."
  (let ((total (reduce #'+ directories :key #'third)))
    (if (yes-or-no-p* (format nil "~a, ~a?" (list-question "Unload, remove and delete" "package" names)
                              (tuatara.io:octets-words total)))
        (let ((names (packages-not-busy names)))
          (dolist (name names)
            (remove-package name)
            (destructuring-bind (name source version &rest more) (declared-package name)
              (declare (ignore more))
              (state-list-drop *fetched-packages* (list (package-key name (package-where source) version)))))
          (sweep-next (remove-if-not (lambda (entry) (member (fourth entry) names :test #'string=)) directories) 0 0
                      names))
        (message "Nothing deleted"))))

(define-command package-delete ()
  "Unload a package (\\[package-unload]), remove it from the packages loaded
\(\\[package-remove]), and delete what Tuatara keeps of it: the versions of it
fetched and their compiled files, or for a directory of your disk its compiled
files alone, the directory of yours left as it is. What would go is measured in
the background and asked once, with its size; nothing is deleted without
yes, and each directory is looked at again just before it is deleted. Who
signed its source's versions is still remembered. \\[package-load] fetches it
again. In the package manager (\\[package-manager]) it deletes the packages
marked (>), or the one on the line, as \\<packages-mode>\\[list-delete-marked] does."
  (act-on-packages "delete" (source-packages) #'start-packages-delete))

(define-command package-load ()
  "Load a package the init file loads from a source that is not loaded now,
by its load-package there, as at the start: one unloaded (\\[package-unload]),
removed (\\[package-remove]), whose loading failed, or one deleted, fetched
again. A removed one is no longer removed. Says what became of it, in
*Messages* too. In the package manager (\\[package-manager]) it loads the
packages marked (>), or the one on the line."
  (act-on-packages "load"
                   (remove-if (lambda (name) (or (package-loaded-p name) (package-busy-p name))) (source-packages))
                   (lambda (names)
                     (let ((names (remove-if #'package-loaded-p (packages-not-busy names))))
                       (if names
                           (load-packages-again names)
                           (message "Already loaded"))))))
