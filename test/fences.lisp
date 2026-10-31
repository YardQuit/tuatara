;;;; test/fences.lisp --- What keeps the tests away from the developer's own files and settings.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loaded first with the tests, however they are run (tools/test.sh,
;;;; asdf:test-system, one test from a Lisp session): the tests never read the
;;;; developer's init file, note templates, capture templates, snippets or git
;;;; settings, never write their state (the recent files), their trashes (the
;;;; home one or a drive's), or their spelling word lists. Each test run has
;;;; directories of its own for these, which REMOVE-TEST-DIRECTORIES takes
;;;; away. In a Lisp session that also runs the editor, loading the tests sets
;;;; these for that session.

(in-package #:tuatara.tests)

;; Tests never read the developer's real init file; the config test binds its own.
;; Nor their own note templates: the formatted test binds its own.
(setf tuatara.defaults::*note-templates-directory* "/nonexistent/tuatara-test-note-templates/")
;; Nor their own capture templates, which file into their real files, nor their
;; snippets: the capture and snippets tests bind their own.
(setf tuatara-user:*capture-template-directories* (list "/nonexistent/tuatara-test-capture-templates/"))
(setf tuatara-user:*snippet-directories* (list "/nonexistent/tuatara-test-snippets/"))
(setf tuatara.defaults:*config-file* #p"/nonexistent/tuatara-test-init.lisp")
;; Nor write the developer's state (the list of recent files): a directory of
;; their own, removed when they are done.
(defvar *test-state-directory*
  (merge-pathnames (format nil "tuatara-test-state-~d/" (sb-posix:getpid)) (uiop:temporary-directory)))
(sb-posix:setenv "XDG_STATE_HOME" (namestring *test-state-directory*) 1)
;; Nor read or write the developer's configuration folder (their Word and LaTeX
;; templates): one of the run's own.
(sb-posix:setenv "XDG_CONFIG_HOME" (namestring (merge-pathnames "config/" *test-state-directory*)) 1)
;; Nor touch the developer's trashes, the home one or a drive's (a USB stick's): the
;; tests' data home is their own, on the drive their files are made on, and they see
;; no drive mounted.
(defvar *test-data-directory*
  (merge-pathnames (format nil "test-data-~d/" (sb-posix:getpid)) (tuatara.io:cache-directory)))
(ensure-directories-exist *test-data-directory*)
(sb-posix:setenv "XDG_DATA_HOME" (namestring *test-data-directory*) 1)
(let ((no-drives (merge-pathnames "no-drives" *test-state-directory*)))
  (ensure-directories-exist no-drives)
  (with-open-file (out no-drives :direction :output :if-exists :supersede))
  (setf tuatara.defaults::*mounts-file* (namestring no-drives)))
;; Nor add to the developer's spelling word lists: enchant keeps the tests' own there.
(sb-posix:setenv "ENCHANT_CONFIG_DIR" (namestring (merge-pathnames "enchant/" *test-state-directory*)) 1)
;; Nor use the developer's own GnuPG keys or agent: a keyring of the tests' own, made empty.
(let ((home (merge-pathnames "gnupg/" *test-state-directory*)))
  (ensure-directories-exist home)
  (sb-posix:chmod (namestring home) #o700)
  (sb-posix:setenv "GNUPGHOME" (namestring home) 1))
;; Nor read the developer's git settings (their name, their signing, their
;; aliases): a global settings file of the tests' own, and none of the system's.
(let ((settings (merge-pathnames "gitconfig" *test-state-directory*)))
  (ensure-directories-exist settings)
  (with-open-file (out settings :direction :output :if-exists :supersede)
    (format out "[user]~%	name = Test~%	email = test@example.invalid~%[commit]~%	gpgsign = false~%[init]~%	defaultBranch = main~%"))
  (sb-posix:setenv "GIT_CONFIG_GLOBAL" (namestring settings) 1)
  (sb-posix:setenv "GIT_CONFIG_NOSYSTEM" "1" 1))
;; Nor copy changes not saved anywhere: the auto-save tests turn it on for themselves.
(setf tuatara-user:*auto-save* nil)
;; The fake frontend has no screen to flicker: work in the background is looked
;; at often, so that the tests do not wait a second for each tick.
(setf tuatara.command:*tick-interval* 1/20)
;; the file questions list their entries in plain order, not by what earlier tests chose
(setf tuatara-user:*last-used-first* nil)
;; a language's rules are compiled before its colours are checked, however long it takes
(setf tuatara.defaults::*rules-wait* 60
      tuatara.defaults::*rules-editor-wait* 60)
;; and the program parsing a file's text started before its first frame is
;; drawn, however busy the machine: a test reads the colours of that frame
;; (the tests of starting it on time bind their own wait)
(setf tuatara.syntax::*at-once-wait* 30)

(defun remove-test-directories ()
  "Take away the directories this test run kept its state and data in, the
gpg-agent of the tests' own keyring stopped first."
  (ignore-errors (uiop:run-program (list "gpgconf" "--homedir" (namestring (merge-pathnames "gnupg/" *test-state-directory*))
                                         "--kill" "gpg-agent")
                                   :ignore-error-status t))
  (uiop:delete-directory-tree *test-state-directory* :validate t :if-does-not-exist :ignore)
  (uiop:delete-directory-tree *test-data-directory* :validate t :if-does-not-exist :ignore))
;; however the tests are run, their directories go when Lisp exits
(pushnew 'remove-test-directories sb-ext:*exit-hooks*)

(defun remove-stale-test-directories ()
  "Take away the state, data and test directories of earlier test runs whose
Lisp has ended without taking them away (one killed, say): those named for a
process no longer running."
  (flet ((sweep (parent prefix)
           (dolist (dir (uiop:subdirectories parent))
             (let* ((name (car (last (pathname-directory dir))))
                    (pid (and (uiop:string-prefix-p prefix name)
                              (parse-integer name :start (length prefix) :junk-allowed t))))
               (when (and pid (not (probe-file (format nil "/proc/~d/" pid))))
                 (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))))
    (sweep (uiop:temporary-directory) "tuatara-test-state-")
    (sweep (tuatara.io:cache-directory) "test-data-")
    (sweep (tuatara.io:cache-directory) "test-")))
(remove-stale-test-directories)
;; the tests' own report template, which notes in their test directories name by its path
(push (namestring (asdf:system-relative-pathname :tuatara "test/latex-export/")) tuatara-user:*export-trusted-folders*)

;; The tests take the leader keys the starter init file offers (commented out
;; there, SPC being yours), as one who takes them does: the tests that
;; reach commands through SPC keep doing so, and the offered lines are seen to work.
(let ((*package* (find-package :tuatara-user)))
  (dolist (line (uiop:read-file-lines (asdf:system-relative-pathname :tuatara "data/init.lisp")))
    (let ((line (string-left-trim " " line)))
      (when (cl-ppcre:scan "^;; \\(bind-key \"[^\"]+\" (nil|'[a-z-]+) :map :leader( :mode '(?:[a-z-]+|\\([a-z -]+\\)))?( :label \"[^\"]*\")?\\)\\s*$" line)
        (eval (read-from-string (subseq line 3)))))))
