;;;; lisp/git/rewind.lisp --- Going back through git's history: a file, or a folder, as each commit left it.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's git-timemachine does for a file: C-x G in a file
;;;; shows it as its latest commit left it, read only, in its own mode; p goes
;;;; a commit older, n newer, r to a commit chosen from its log, c shows the
;;;; commit, C-c w copies its hash. In rimu it does that for the folder, in
;;;; a rimu list of its own (lisp/rimu/git.lisp), and RET on a file goes on
;;;; through that file's history from there. C-x G again goes back
;;;; to the file or the folder as it is now. The mode line says Rewind and
;;;; the commit, lit up.

(in-package #:tuatara.defaults)

;;; Histories read

(defun git-file-history (text)
  "TEXT, what git log printed for a file with *GIT-COMMIT-FORMAT*,
--name-only and -z, as a vector of GIT-COMMITs, the latest first, each with
the file's name in it (PARSE-GIT-COMMITS)."
  (coerce (parse-git-commits text :names t) 'simple-vector))

(defun git-rewind-file-log (job top path &optional at)
  "On JOB's thread: the commits of PATH in the repository at TOP, the latest
first (GIT-FILE-HISTORY), followed back across renames, from the commit AT
or HEAD; one that deleted it left out."
  (git-file-history (git-job-output job top (append (list "log" "--follow" "--diff-filter=d" "--date=short" "--name-only" "-z"
                                                          *git-commit-format*)
                                                    (and at (list "--end-of-options" at))
                                                    (list "--" path)))))

(defun git-rewind-revision-octets (job top revision)
  "On JOB's thread: the file as REVISION, a GIT-COMMIT of its history, left
it, as octets, read from the repository at TOP; no more than
*GIT-SHOWN-LIMIT*."
  (git-blob-octets top (git-commit-hash revision) (git-commit-path revision) :job job :limit *git-shown-limit*))

;;; Buffers going back

(defvar *git-rewind-keymap* (make-keymap :name "git history, a commit at a time" :parent *made-text-keymap*)
  "The keys of a file or folder shown as a commit left it (\\[git-rewind]),
before the modal keys and a rimu list's: \\`p' a commit older, \\`n' newer, \\`r'
one chosen from its log, \\`c' the commit, \\`C-c w' copies its hash, \\`q'
closes.")

(defun git-rewind-buffer-p (buffer)
  "True when BUFFER goes through a file's or a folder's history (\\[git-rewind])."
  (and (tuatara.buffer:buffer-local buffer 'git-rewind-revisions) t))

(defun git-rewind-words (buffer index)
  "What is said of the commit INDEX (0 the latest) of BUFFER's history: which
of how many, its short hash, date, author and subject."
  (let ((revisions (tuatara.buffer:buffer-local buffer 'git-rewind-revisions)))
    (let ((revision (svref revisions index)))
      (format nil "~:d of ~:d: ~a ~a ~a: ~a" (1+ index) (length revisions) (git-commit-short revision) (git-commit-date revision)
              (escape-controls (git-commit-author revision)) (escape-controls (git-commit-subject revision))))))

(defun git-rewind-buffer (top name mode)
  "Show at once, in the current window, a new buffer NAME (one by that name
before is killed) in MODE for a history in the repository at TOP, saying it
is being read, read only, its keys *GIT-REWIND-KEYMAP*'s. Returns it."
  (let ((buffer (fresh-made-buffer name mode (format nil "Reading the history...~%"))))
    (git-set-top buffer top)
    (setf (tuatara.buffer:buffer-local buffer :override-keymap) *git-rewind-keymap*)
    (show-buffer buffer)
    buffer))

;;; A file

(defun git-rewind-file (top path &key (line 0) at list)
  "Go through the history of PATH (from TOP) in the repository at TOP, in a
buffer of its own shown at once, in the mode of the file: the file as its
latest commit left it, or with AT, a commit, as it was then; point at the
start of LINE (0 the first). LIST is the folder's history it was opened
from, if any."
  (let ((buffer (git-rewind-buffer top (format nil "*git rewind: ~a*" (tuatara.io:native-file-name path))
                                   (mode-for-file (sb-ext:parse-native-namestring (concatenate 'string top path))))))
    (setf (tuatara.buffer:buffer-local buffer 'git-rewind-path) path
          (tuatara.buffer:buffer-local buffer 'git-rewind-list) list)
    (start-git-job "Reading the file's history"
                   (lambda (job)
                     ;; from a folder's commit: the file followed back from there, and the
                     ;; commits after it that kept its name (following from HEAD a name
                     ;; gone there follows another file)
                     (let* ((newer (and at (git-file-history
                                            (git-job-output job top (list "log" "--diff-filter=d" "--date=short" "--name-only" "-z"
                                                                          *git-commit-format* "--end-of-options" (format nil "~a..HEAD" at)
                                                                          "--" path)))))
                            (revisions (concatenate 'simple-vector newer (git-rewind-file-log job top path at)))
                            ;; renamed in what is staged: its history is that of the name HEAD has
                            (revisions (or (and (zerop (length revisions)) (null at)
                                                (let ((before (git-staged-rename-source job top path)))
                                                  (and before (git-rewind-file-log job top before))))
                                           revisions))
                            (index (min (length newer) (max 0 (1- (length revisions))))))
                       (values revisions index
                               (and (plusp (length revisions)) (git-rewind-revision-octets job top (svref revisions index))))))
                   (lambda (revisions index octets)
                     (cond ((zerop (length revisions))
                            (git-rewind-given-up buffer (format nil "~a has no history in git" (escape-controls path))))
                           ((tuatara.io:binary-head-p octets)
                            (git-rewind-given-up buffer (format nil "~a is not text: \\[git-log-file] lists its commits"
                                                                (escape-controls path))))
                           (t (git-rewind-file-begin buffer path revisions index octets line))))
                   :quiet t :buffer buffer)))

(defun git-rewind-given-up (buffer words)
  "End BUFFER, a file's history that has nothing to show, the window going
back to what it showed before, and say WORDS, with their keys put in."
  (tuatara.buffer:kill-buffer buffer)
  (message "~a" (substitute-command-keys words)))

(defun git-staged-rename-source (job top path)
  "On JOB's thread: the name the file PATH (from TOP) has in HEAD of the
repository at TOP when it is renamed in what is staged; NIL when it is not."
  (let ((fields (uiop:split-string (git-job-try job top (list "diff" "--cached" "-M" "--name-status" "-z" "HEAD"))
                                   :separator (list (code-char 0)))))
    (loop while fields
          do (let ((status (pop fields)))
               (if (and (plusp (length status)) (member (char status 0) '(#\R #\C)))
                   (let ((from (pop fields)) (to (pop fields)))
                     (when (and (char= (char status 0) #\R) (equal to path))
                       (return from)))
                   (pop fields))))))

(defun git-rewind-file-begin (buffer path revisions index octets line)
  "Show in BUFFER, opened by GIT-REWIND-FILE, the file PATH as the commit
INDEX of REVISIONS (GIT-REWIND-FILE-LOG) left it, OCTETS, point at the start
of LINE (0 the first, the last there is when it has fewer); a file whose name
gives no mode takes the one its first line gives (#!/bin/sh)."
  (let ((first-line (tuatara.text:octets-string octets :end (or (tuatara.text:octets-line-end-position octets 0 (min (length octets) 512))
                                                                (min (length octets) 512)))))
    (setf (tuatara.buffer:buffer-local buffer 'git-rewind-revisions) revisions
          (tuatara.buffer:buffer-local buffer 'git-rewind-at) index
          (tuatara.buffer:buffer-local buffer 'git-rewind-wanted) index)
    (when (eq (type-of (buffer-major-mode buffer)) 'fundamental-mode)
      (let ((mode (mode-for-file (sb-ext:parse-native-namestring path) (string-right-trim '(#\Return) first-line))))
        (unless (eq mode 'fundamental-mode)
          (set-major-mode buffer mode))))
    (show-list-text buffer (tuatara.text:octets->rope (coerce octets 'tuatara.text:octets)) nil nil :point line)
    (message "~a" (git-rewind-words buffer index))))

;;; Going through it

(defun git-rewind-show (buffer index)
  "Show in BUFFER, a file's or a folder's history, what its commit INDEX (0
the latest) left, read in the background, point kept on its line; when a
later move asked for another before it came, that one is shown instead."
  (if (rimu-rewound-p buffer)
      (rimu-rewind-show buffer index)
      (let ((top (git-here buffer))
            (revision (svref (tuatara.buffer:buffer-local buffer 'git-rewind-revisions) index)))
        (setf (tuatara.buffer:buffer-local buffer 'git-rewind-wanted) index)
        (start-git-job "Reading a revision"
                       (lambda (job)
                         (tuatara.text:octets->rope (coerce (git-rewind-revision-octets job top revision) 'tuatara.text:octets)))
                       (lambda (rope)
                         (when (eql index (tuatara.buffer:buffer-local buffer 'git-rewind-wanted))
                           (show-list-text buffer rope nil nil)
                           (setf (tuatara.buffer:buffer-local buffer 'git-rewind-at) index)
                           (message "~a" (git-rewind-words buffer index))))
                       :quiet t :buffer buffer))))

(defun git-rewind-move (by)
  "Show the current history BY commits older (newer for a negative BY), said
so at its ends."
  (let* ((buffer (current-buffer))
         (revisions (tuatara.buffer:buffer-local buffer 'git-rewind-revisions))
         (index (+ (tuatara.buffer:buffer-local buffer 'git-rewind-wanted) by))
         (what (if (rimu-rewound-p buffer) "folder" "file")))
    (cond ((< index 0) (message "This is the latest commit of the ~a" what))
          ((>= index (length revisions)) (message "This is the first commit of the ~a" what))
          (t (git-rewind-show buffer index)))))

(define-command git-rewind-older ()
  "Show the file or folder of this history as the commit before left it."
  (git-rewind-move 1))

(define-command git-rewind-newer ()
  "Show the file or folder of this history as the commit after left it."
  (git-rewind-move -1))

(define-command git-rewind-to ()
  "Show the file or folder of this history as a commit chosen from its log
left it, the latest first, by its hash, date, author or subject."
  (let ((index (completing-read-value
                "Rewind to: "
                (loop for revision across (tuatara.buffer:buffer-local (current-buffer) 'git-rewind-revisions)
                      for i from 0
                      collect (make-candidate (format nil "~a ~a ~a: ~a" (git-commit-short revision) (git-commit-date revision)
                                                      (escape-controls (git-commit-author revision)) (escape-controls (git-commit-subject revision)))
                                              :value i))
                :require-match t :keep-order t)))
    (when (integerp index) (git-rewind-show (current-buffer) index))))

(define-command git-rewind-show-commit ()
  "Show the commit that left the file or folder of this history as it is
shown: what it says and its changes."
  (let ((revision (git-rewind-shown (current-buffer))))
    (if revision
        (git-show-commit (git-here) (git-commit-hash revision))
        (message "No commit shown yet: it is being read"))))

(define-command git-rewind-copy-hash ()
  "Copy the short hash of the commit that left the file or folder of this
history as it is shown."
  (let ((short (let ((revision (git-rewind-shown (current-buffer)))) (and revision (git-commit-short revision)))))
    (if short
        (progn (kill-text short :append nil)
               (message "Copied ~a" short))
        (message "No commit shown yet: it is being read"))))

(commands-for-buffers 'git-rewind-buffer-p "a file's or folder's history (\\[git-rewind])"
                      'git-rewind-older 'git-rewind-newer 'git-rewind-to 'git-rewind-show-commit 'git-rewind-copy-hash)

;;; Back to now

(defun git-rewind-leave (buffer)
  "End the history BUFFER shows, and a folder's it was opened from: show the
file as it is now on the same line, or the folder now in rimu
\(RIMU-REWIND-LEAVE); a file gone now, the nearest folder above it that is
there, said so."
  (if (rimu-rewound-p buffer)
      (rimu-rewind-leave buffer)
      (let* ((top (git-here buffer))
             (path (concatenate 'string top (tuatara.buffer:buffer-local buffer 'git-rewind-path)))
             (line (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer)))
             (list (tuatara.buffer:buffer-local buffer 'git-rewind-list)))
        (tuatara.buffer:kill-buffer buffer)
        (if (eq :regular (tuatara.io:path-kind path))
            (progn
              (when (and list (live-buffer-p list))
                (tuatara.buffer:kill-buffer list))
              (let ((file (visit-path path)))
                (when file
                  (goto-shown file (tuatara.buffer:buffer-goto-line file (min line (max 0 (1- (tuatara.buffer:buffer-line-count file)))))))))
            (progn
              (if (and list (live-buffer-p list))
                  (rimu-rewind-leave list)
                  (rimu-show-nearest (tuatara.io:parent-path path)))
              (message "~a is not there now" (escape-controls (tuatara.io:native-file-name path))))))))

;;; The command

(define-command git-rewind ()
  "Go back through git's history a commit at a time, and come back. In a
file, or on a file's line of a status, the file is shown as its latest commit
left it, in a buffer of its own in its mode, read only, on the line you were
on. In a list of a directory (\\[rimu]), the folder is listed as its latest
commit left it, as rimu lists it, in a list of its own: \\<rimu-mode>\\[rimu-open] on a folder
lists it as that commit left it, on a file goes through the file's history
from that commit (\\[quit-window] comes back to the folder), \\[rimu-up] goes up; the preview
shows the files as they were then, and what changes files or reads them as
they are now is refused there.
In either, \\<*git-rewind-keymap*>\\[git-rewind-older] shows a commit older, \\[git-rewind-newer] a commit newer, \\[git-rewind-to] one
chosen from its log; \\[git-rewind-show-commit] shows the commit, \\[git-rewind-copy-hash] copies its hash, and the mode
line says which it is, Rewind and its hash lit up. Run again there, it goes
back to the file or folder as it is now, on the same line or entry. A file
is followed back across renames; changes not committed are not in it, nor is
a file that is not text shown."
  (let ((buffer (current-buffer)))
    (cond ((or (rimu-rewound-p buffer) (tuatara.buffer:buffer-local buffer 'git-rewind-path))
           (git-rewind-leave buffer))
          ((rimu-list-p buffer)
           (with-git-repository (top)
             (let ((directory (tuatara.io:native (tuatara.buffer:buffer-local buffer 'rimu-directory))))
               (if (tuatara.io:path-within-p directory top)
                   (rimu-rewind buffer top)
                   (message "~a is not in the repository at ~a" (abbreviated-path directory) (abbreviated-path top))))))
          (t
           (with-git-repository (top)
             (call-with-git-file top "No file here to rewind"
                                 (lambda (path)
                                   (if (or (uiop:string-suffix-p path "/")
                                           (eq :directory (tuatara.io:path-kind (concatenate 'string top path))))
                                       (message "~a is a folder: rewind a file in it, or the folder from its list (\\[rimu])"
                                                (escape-controls path))
                                       (git-rewind-file top path
                                                        :line (if (tuatara.buffer:buffer-file buffer)
                                                                  (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer))
                                                                  0))))))))))

(defun git-rewind-shown (buffer)
  "The commit BUFFER's history shows, a GIT-COMMIT, and
as a second value which it is (0 the latest) of how many, a third; NIL when
BUFFER shows none."
  (let ((revisions (tuatara.buffer:buffer-local buffer 'git-rewind-revisions)))
    (let ((at (tuatara.buffer:buffer-local buffer 'git-rewind-at)))
      (when (and revisions at)
        (values (svref revisions at) at (length revisions))))))

(defun git-rewind-mode-line (editor buffer)
  "What the mode line of a history adds first: Rewind and the short hash of
the commit shown, lit up in the warning colour (:mode-line-rewind)."
  (declare (ignore editor))
  (let ((revision (git-rewind-shown buffer)))
    (and revision (list (format nil " Rewind ~a " (git-commit-short revision)) :mode-line-rewind))))

(defun git-rewind-mode-line-place (editor buffer)
  "What the mode line of a history adds after GIT-REWIND-MODE-LINE: the date
of the commit shown, and which of how many it is."
  (declare (ignore editor))
  (multiple-value-bind (revision at count) (git-rewind-shown buffer)
    (and revision (format nil "~a ~:d/~:d" (git-commit-date revision) (1+ at) count))))

(defun install-git-rewind ()
  "Bind \\`C-x G' to \\[git-rewind], beside \\`C-x g' for the status, and the keys of a
history; let the mode line say the commit shown."
  (define-key *global-keymap* "C-x G" 'git-rewind)
  (define-keys *git-rewind-keymap* '(("p" git-rewind-older) ("n" git-rewind-newer) ("r" git-rewind-to)
                                     ("c" git-rewind-show-commit) ("C-c w" git-rewind-copy-hash)))
  ;; pushed in turn to the front: Rewind and the hash first, the date and place after
  (pushnew 'git-rewind-mode-line-place *mode-line-functions*)
  (pushnew 'git-rewind-mode-line *mode-line-functions*))
