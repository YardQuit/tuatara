;;;; lisp/git/patches.lisp --- Patches written and applied, mail-style patches taken in, bundles.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In a status W opens the patch menu and w the menu of patches by mail
;;;; (git am), as Magit's do: commits written out as patch files, a patch
;;;; file applied to the files or what is staged, the changes not committed
;;;; saved as one, a whole repository written to a single bundle file and a
;;;; bundle checked. git am stopped half way goes on, skips or stops.

(in-package #:tuatara.defaults)

(defun git-switches-among (switches allowed)
  "Those of SWITCHES, a menu's switches turned on, that are among ALLOWED:
the ones of a menu that has switches for several of its actions which the
action at hand takes."
  (remove-if-not (lambda (switch) (member switch allowed :test #'string=)) switches))

(defun git-ask-range (top then)
  "Ask for the commits to act on in the repository at TOP: those after a
revision asked for, up to the branch checked out; then call THEN with the
range (abc123..HEAD)."
  (git-ask-revision "Commits after: " top (lambda (base) (funcall then (format nil "~a..HEAD" base)))))

(define-command git-format-patch ()
  "Write the commits after a revision asked for as patch files, a commit a
file, in a folder asked for; the patch menu's switches number them, add a
cover letter, or sign them off (those for applying are left out)."
  (with-git-repository (top switches)
    (git-ask-range top
                   (lambda (range)
                     (let ((folder (read-native-path "Write the patches in: " :writing t)))
                       (when folder
                         (let ((folder (tuatara.io:slash-ended folder)))
                           (git-run "Writing patches" top (append (list "format-patch")
                                                                  (git-switches-among switches '("--numbered" "--cover-letter" "--signoff"))
                                                                  (list "-o" folder range))
                                    (lambda (text)
                                      (let ((n (length (tuatara.text:split-words text '(#\Newline)))))
                                        (format nil "Wrote ~d patch~:[es~;~] in ~a" n (= n 1) (abbreviated-path folder))))))))))))

(defun git-ask-patch-file (prompt then)
  "Ask with PROMPT for a patch file that is there, then call THEN with its native path."
  (let ((path (read-native-path prompt :kind :file)))
    (cond ((null path))
          ((eq (tuatara.io:path-kind path) :regular) (funcall then path))
          (t (message "~a is not a file" (abbreviated-path path))))))

(define-command git-apply-patch ()
  "Apply a patch file asked for to the files, as git apply does; the patch
menu's switches stage it too (--index), only stage it (--cached), or join it
three ways where it does not fit (those for writing patches are left out)."
  (with-git-repository (top switches)
    (git-ask-patch-file "Apply patch: "
                        (lambda (path)
                          (git-run "Applying a patch" top (append (list "apply")
                                                                  (git-switches-among switches '("--index" "--cached" "--3way"))
                                                                  (list "--" path))
                                   (format nil "Applied ~a" (abbreviated-path path)))))))

(define-command git-save-patch ()
  "Save every change not committed, staged or not, as a new patch file asked
for, its octets as git writes them."
  (with-git-repository (top)
    (let ((path (read-new-native-path "Save the changes as: " :kind :file :writing t)))
      (when path
        (git-ask "Reading the changes" top
                 (lambda (job top)
                   (git-job-octets job top (list "diff" "--no-color" "--no-ext-diff" "--binary" (git-head-or-empty job top))))
                 (lambda (octets)
                   (if (zerop (length octets))
                       (message "No changes to save")
                       (progn
                         (tuatara.io:write-new-file path octets :mode #o600)
                         (message "Saved the changes as ~a" (abbreviated-path path))))))))))

(define-command git-bundle-create ()
  "Write the whole repository, every branch and tag, as a single bundle file
asked for: one file to carry it, from which it can be cloned."
  (with-git-repository (top)
    (let ((path (read-new-native-path "Write the bundle as: " :kind :file :writing t)))
      (when path
        (git-run "Writing a bundle" top (list "bundle" "create" path "--all")
                 (format nil "Wrote the bundle ~a" (abbreviated-path path)))))))

(define-command git-bundle-verify ()
  "Check a bundle file asked for: whether this repository has what it needs to take it in."
  (with-git-repository (top)
    (git-ask-patch-file "Check bundle: "
                        (lambda (path)
                          (git-run "Checking a bundle" top (list "bundle" "verify" path)
                                   (lambda (text) (declare (ignore text)) (format nil "~a is good" (abbreviated-path path))))))))

(define-menu git-patch-menu (:title "Patch" :show git-menu-shows-p :note git-menu-note)
  "Open the patch menu: write commits as patch files, apply a patch file,
save the changes not committed as one, or write or check a bundle.
\\`C-h' there lists every action."
  ("Patches"
   ("c" "Write commits as patches" git-format-patch)
   ("a" "Apply a patch" git-apply-patch)
   ("s" "Save the changes as a patch" git-save-patch))
  ("Bundles"
   ("b" "Write the repository as a bundle" git-bundle-create)
   ("v" "Check a bundle" git-bundle-verify))
  ("Switches"
   ("- n" "Number the patches (writing)" :switch "--numbered")
   ("- c" "Add a cover letter (writing)" :switch "--cover-letter")
   ("- s" "Sign off (writing)" :switch "--signoff")
   ("- i" "Apply staged too (applying)" :switch "--index")
   ("- C" "Apply to what is staged only (applying)" :switch "--cached")
   ("- 3" "Join three ways (applying)" :switch "--3way")))

;;; Patches by mail

(define-command git-am ()
  "Take in patch files asked for, as sent by mail (git format-patch writes
them), each made a commit with its author and message; one that does not
fit stops it half way (w c goes on, w s skips it, w a stops it)."
  (with-git-repository (top switches)
    (git-ask-patch-file "Take in patch: "
                        (lambda (path)
                          (git-run "Taking in a patch" top (append (list "am") switches (list "--" path))
                                   (format nil "Took in ~a" (abbreviated-path path))
                                   :environment *git-keep-messages*)))))

(define-git-sequence-commands "am" "taking in of patches")

(define-menu git-am-menu (:title "Patches by mail" :show git-menu-shows-p :note git-menu-note)
  "Open the menu of patches by mail: take patch files in as commits, or go
on with, skip one of, or stop a taking in stopped half way. \\`C-h' there
lists every action."
  ("Take in"
   ("w" "Patch files as commits" git-am))
  ("Stopped half way"
   ("c" "Go on" git-am-continue)
   ("s" "Skip this patch" git-am-skip)
   ("a" "Stop" git-am-abort))
  ("Switches"
   ("- 3" "Join three ways" :switch "--3way")
   ("- s" "Sign off" :switch "--signoff")))

(defun install-git-patches ()
  "Bind the patch menu to W and the menu of patches by mail to w in a status."
  (define-keys *git-status-keymap* '(("W" git-patch-menu) ("w" git-am-menu))))
