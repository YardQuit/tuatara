;;;; lisp/files/recent.lisp --- The files visited lately: find-recent-file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The list of recent files lives in the state directory (state.lisp): each
;;;; visit puts the file first, written in the background with what the other
;;;; sessions put there, so that sessions side by side, in the terminal and in
;;;; the window, share one list.
;;;; A file visited safely is remembered as such and opened safely again; a
;;;; file visited both ways is in the list twice, once each way.

(in-package #:tuatara.defaults)

(defvar *remember-recent-files* t
  "True to keep a list of the files you visit, the latest first, for
find-recent-file (\\[find-recent-file], \\<normal>\\[find-recent-file]) to offer;
NIL adds nothing to it. The list is the file recent-files in
$XDG_STATE_HOME/tuatara/ (~/.local/state/tuatara/), readable by you alone.")

(defvar *recent-files-max* 25
  "How many files the list of recent files keeps.")

(defvar *recent-files-auto-cleanup* t
  "True to take out of the list of recent files, when the editor starts, the
files no longer there; NIL keeps them, shown as not found. A file that cannot
be looked at just then, on a drive not answering, is kept.")

(defvar *recent-files-cleaned* nil
  "True once this session has cleaned the list of recent files.")

(defvar *recent-files-cleaner* nil
  "The thread looking for the recent files no longer there, while it runs.")

(defvar *recent-files-gone* nil
  "The paths the clean-up found no longer there, for the editor to take out.")

(defun parse-recent-file-line (line)
  "The entry of the list of recent files LINE writes, (PATH . SAFE): file,
or safe for a file visited safely, then its path (ESCAPE-PATH); NIL for a
line that makes no sense."
  (let ((space (position #\Space line)))
    (when (and space (member (subseq line 0 space) '("file" "safe") :test #'string=))
      (let ((path (unescape-path (subseq line (1+ space)))))
        (and (plusp (length path)) (cons path (string= "safe" line :end2 space)))))))

(defun recent-file-line (entry)
  "The line of the list of recent files for ENTRY, (PATH . SAFE)."
  (format nil "~:[file~;safe~] ~a" (cdr entry) (escape-path (car entry))))

(define-state-list *recent-files* "recent-files" 'parse-recent-file-line 'recent-file-line '*recent-files-max*
  :header (format nil "# The files Tuatara visited lately, the latest first: file, or safe for one~%~
                       # opened safely, then its path (\\\\, \\n and \\r stand for \\, a newline and a return).~%")
  :path-of #'car :key #'identity)

(defun read-recent-files ()
  "The list of recent files: a list of (PATH . SAFE), the latest first, SAFE
true for a file visited safely, a file visited both ways there once each
way; empty when there is none (STATE-LIST-ENTRIES)."
  (state-list-entries *recent-files*))

(defun remember-file (path &key safe)
  "Put PATH first in the list of recent files, remembered as visited SAFE or
not, when *REMEMBER-RECENT-FILES* is true; visited the other way too, it stays
there that way as well. The list on disk is written in the background, with
what sessions side by side put there meanwhile (STATE-LIST-PUT)."
  (when *remember-recent-files*
    (let ((native (tuatara.io:native path)))
      (when native
        (state-list-put *recent-files* (list (cons native (and safe t))))))))

(defun remember-buffer-file (buffer)
  "Put the file BUFFER visits first in the list of recent files, when there is
one on disk: after a visit, and after a save (a new file, or saving as)."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (when (and file (ignore-errors (tuatara.io:path-stamp file)))
      (remember-file file :safe (tuatara.io:safe-visit-p buffer)))))

(defun recent-file-fields (entry stamps now)
  "What is shown at the right of ENTRY, a (PATH . SAFE) of the recent files: as
for any file its permissions, size and when it last changed, or not found.
STAMPS keeps what was learnt of each file while the question is open, so that
each is examined once."
  (let ((path (car entry)))
    (multiple-value-bind (stamp known) (gethash path stamps)
      (unless known
        (setf stamp (ignore-errors (tuatara.io:path-stamp path))
              (gethash path stamps) stamp))
      (if stamp
          (stamp-fields stamp now)
          (list (list "not found" :completion-annotation))))))

(define-command find-recent-file ()
  "Visit a file visited lately: the latest first, each with its permissions,
size and when it last changed beside it. A file opened safely is marked safe
and opened safely again; one opened both ways is there twice, so either way
can be chosen. The file of the current buffer is left out, so that
\\<minibuffer>\\[minibuffer-done] alone goes back to the one before. With *preview-files*, the
file selected is shown meanwhile, opened safely."
  (let* ((current (let ((file (tuatara.buffer:buffer-file (current-buffer))))
                    (and file (tuatara.io:native file))))
         (entries (remove current (ignore-errors (read-recent-files)) :key #'car :test #'equal)))
    (if (null entries)
        (message "No recent files")
        (let* ((stamps (make-hash-table :test 'equal))
               (now (get-universal-time))
               ;; a file opened safely says so right after its name
               (candidates (mapcar (lambda (entry)
                                     (make-candidate (abbreviated-path (car entry)) :value entry
                                                     :suffix (and (cdr entry) (list (list "safe" :completion-safe)))))
                                   entries))
               (chosen (call-with-window-preview
                        (lambda (candidate made)
                          (preview-file-buffer (car (candidate-value candidate)) made))
                        (lambda (show)
                          (completing-read "Open recent file: " candidates
                                        ;; the latest first, among those that match, not the best
                                        ;; match first: the two ways of one file stay in order
                                        :keep-order t
                                        :require-match t
                                        :annotate (lambda (candidate)
                                                    (recent-file-fields (candidate-value candidate) stamps now))
                                        :preview (and *preview-files* show))))))
          (when (consp chosen)
            (visit-path (car chosen) :safe (cdr chosen)))))))

(defun recent-file-renamed (from to)
  "Have the list of recent files name TO, first, where it named FROM, a file
just renamed, visited safely or not as it was."
  (let* ((from (tuatara.io:native from))
         (entries (remove from (read-recent-files) :key #'car :test-not #'equal)))
    (when entries
      (drop-recent-files (list from))
      (state-list-put *recent-files* (mapcar (lambda (entry) (cons (tuatara.io:native to) (cdr entry))) entries)))))

(defun gone-recent-files (entries)
  "The paths of ENTRIES, (PATH . SAFE) of the list of recent files, that name
no file any more. One that cannot be looked at is not counted gone."
  (remove-duplicates (loop for (path) in entries
                           when (ignore-errors (null (tuatara.io:path-stamp path)))
                             collect path)
                     :test #'string=))

(defun drop-recent-files (paths)
  "Take PATHS out of the list of recent files (STATE-LIST-DROP)."
  (state-list-drop *recent-files* paths))

(defun start-recent-files-cleanup (editor)
  "Once a session, when *RECENT-FILES-AUTO-CLEANUP* is true: look in the
background for the files of the list of recent files no longer there, which
the list then loses (RECENT-FILES-TICK), so that a drive not answering cannot
hold the editor up. How many went is said, and kept in *Messages*."
  (when (and *remember-recent-files* *recent-files-auto-cleanup* (not *recent-files-cleaned*))
    (setf *recent-files-cleaned* t)
    (let ((entries (ignore-errors (read-recent-files))))
      (if (null entries)
          (say-recent-files-cleaned 0)
          (setf *recent-files-cleaner*
                (tuatara.runtime:make-guarded-thread
                 (lambda ()
                   (setf *recent-files-gone* (handler-case (gone-recent-files entries) (error () :failed)))
                   (wake-editor editor))
                 :name "tuatara recent files"))))))

(defun recent-files-tick (editor)
  "Tick function: once the clean-up has looked, take the files it found gone
out of the list of recent files; true while it looks."
  (declare (ignore editor))
  (let ((thread *recent-files-cleaner*))
    (cond ((null thread) nil)
          ((sb-thread:thread-alive-p thread) t)
          (t (setf *recent-files-cleaner* nil)
             (let ((gone (shiftf *recent-files-gone* nil)))
               (if (eq gone :failed)
                   (message "Cleaning up the list of recent files...failed: it could not be read")
                   (progn
                     (when gone (drop-recent-files gone))
                     (say-recent-files-cleaned (length gone)))))
             nil))))

(defun say-recent-files-cleaned (removed)
  "Say that the list of recent files lost REMOVED files no longer there: in
the echo area when there were any, else only in *Messages*, leaving the echo
area to what the file the editor started with says."
  (let ((words (format nil "Cleaning up the list of recent files...done (~d removed)" removed)))
    (if (plusp removed)
        (message "~a" words)
        (log-message words))))

(defun install-recent-files ()
  "Remember the files visited, saved and renamed in the list of recent files, and take
those no longer there out of it as the editor starts (START-RECENT-FILES-CLEANUP,
once the init file has had its say). Idempotent: the hooks are named functions."
  (pushnew 'remember-buffer-file tuatara.buffer:*find-file-functions*)
  (pushnew 'remember-buffer-file *after-save-functions*)
  (pushnew 'recent-file-renamed *file-renamed-functions*)
  (pushnew 'recent-files-tick *tick-functions*))
