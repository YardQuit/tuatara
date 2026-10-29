;;;; lisp/gutter/vc.lisp --- A version-control-aware gutter: added, changed and deleted lines.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; After a file is loaded or saved, its text is compared with the text git
;;;; has for it in HEAD, and each hunk becomes a mark anchored at its first line
;;;; so it follows edits. Between saves, lines touched by edits are marked as
;;;; changed from the edit batches, which costs nothing per keystroke. A file,
;;;; or its committed text, above *VC-GUTTER-MAX-BYTES* is not compared.
;;;;
;;;; Git is only asked for the id of the committed text (rev-parse) once for
;;;; each look at a file, and for that text (cat-file, which runs no filter)
;;;; once for each id, never outside a repository, never to look at
;;;; the file itself, and never to fetch or to run a file-system monitor: a
;;;; repository's own settings can name programs for those, and a repository
;;;; from an archive or a download is not to be trusted (RUN-GIT, which every
;;;; git command of the editor goes through). The lines are compared here, by
;;;; Myers's algorithm (diff.lisp).

(in-package #:tuatara.defaults)

(defvar *vc-gutter-enabled* t
  "True to show version-control marks in the gutter for files under git.")

(defvar *vc-gutter-max-bytes* (* 200 1024 1024)
  "The most octets of a file, and of its text as git has it in HEAD, that are
compared for the gutter's marks; past it the file has no marks.")

(defstruct (vc-mark (:constructor make-vc-mark (anchor last kind)) (:copier nil))
  "A gutter mark over whole lines: ANCHOR at the start of its first line,
LAST at the start of its last, so that lines typed or deleted within it widen
or narrow it; KIND :added, :changed or :deleted."
  (anchor nil) (last nil) (kind :changed))

(setf (documentation 'make-vc-mark 'function) "Return a mark of KIND over the lines from ANCHOR's to LAST's."
      (documentation 'vc-mark-p 'function) "True when OBJECT is a VC-MARK."
      (documentation 'vc-mark-anchor 'function) "The anchor at the mark's first line."
      (documentation 'vc-mark-last 'function) "The anchor at the start of the mark's last line."
      (documentation 'vc-mark-kind 'function) "The mark's kind: :added, :changed or :deleted.")

(defun committed-name (path)
  "The name of the file at PATH from its folder, as git is given it there (GIT-OBJECT-NAME)."
  (concatenate 'string "./" (tuatara.io:native-file-name path)))

(defun committed-id (path &key cancel)
  "The id git gives the text of the file at PATH in HEAD, asked by one git
process (RUN-GIT) that reads nothing of it; NIL when git has no such file,
and no process at all outside a repository."
  (multiple-value-bind (octets ok)
      (run-git (tuatara.io:parent-path path) (list "rev-parse" "--verify" "--quiet" (git-object-name "HEAD" (committed-name path)))
               :cancel cancel :seconds *git-look-seconds*)
    (let ((id (and ok octets (string-trim '(#\Newline) (tuatara.text:lenient-string octets)))))
      (and (git-hash-p id) id))))

(defun committed-text (path &key cancel id)
  "The text of the file at PATH as git has it in HEAD, or the text whose id
is ID, as octets, by one git process (GIT-BLOB-OCTETS); NIL when git has no
such file, and no process at all outside a repository. A text larger than
*VC-GUTTER-MAX-BYTES* is not read: NIL, and true as a second value."
  (multiple-value-bind (octets ok too-large)
      (git-blob-octets (tuatara.io:parent-path path) (or id "HEAD") (and (not id) (committed-name path))
                       :cancel cancel :seconds *git-look-seconds* :limit *vc-gutter-max-bytes*)
    (values (and ok octets) too-large)))

(sb-ext:defglobal **committed-lines** (make-hash-table :test 'equal :synchronized t)
  "The line hashes (LINE-HASHES) of the texts git has that the gutter compared
files with last, by the texts' ids: a file saved again and again is read
from git and its lines hashed once while what HEAD has of it stays the same.")

(defparameter *committed-lines-kept* 32
  "How many committed texts' line hashes the gutter keeps (**COMMITTED-LINES**).")

(defun committed-line-hashes (path &key cancel)
  "The line hashes (LINE-HASHES) of the text of the file at PATH as git has it
in HEAD: worked out once for each text git has (COMMITTED-ID), read from git
the first time (COMMITTED-TEXT). NIL when git has no such file; NIL and true
as a second value for a text larger than *VC-GUTTER-MAX-BYTES*."
  (let ((id (committed-id path :cancel cancel)))
    (when id
      (or (gethash id **committed-lines**)
          (multiple-value-bind (committed too-large) (committed-text path :cancel cancel :id id)
            (if (not committed)
                (values nil too-large)
                (let ((hashes (line-hashes committed)))
                  (sb-ext:with-locked-hash-table (**committed-lines**)
                    (when (>= (hash-table-count **committed-lines**) *committed-lines-kept*)
                      (clrhash **committed-lines**))
                    (setf (gethash id **committed-lines**) hashes)))))))))

(defun git-tracked-p (path &key cancel)
  "True when git has the file at PATH in HEAD, asked by one git process
\(RUN-GIT) that reads nothing of it; NIL with no process at all outside a
repository."
  (nth-value 1 (run-git (tuatara.io:parent-path path) (list "cat-file" "-e" (git-object-name "HEAD" (committed-name path)))
                        :cancel cancel :seconds *git-look-seconds*)))

(defun line-mark (buffer first last kind)
  "A mark of KIND over the lines FIRST to LAST (from 0) of BUFFER's text as it is now."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (make-vc-mark (tuatara.text:make-anchor version (tuatara.buffer:buffer-goto-line buffer first))
                  (tuatara.text:make-anchor version (tuatara.buffer:buffer-goto-line buffer last))
                  kind)))

(defun hunk-marks (buffer hunks)
  "Turn HUNKS into marks anchored in BUFFER's current version: a vector in
order of place, or NIL for none. Where two would share a line, the first
keeps it."
  (let ((marks '()) (taken -1) (lines (tuatara.buffer:buffer-line-count buffer)))
    (dolist (h hunks)
      (destructuring-bind (old-start old-count new-start new-count) h
        (declare (ignore old-start))
        (multiple-value-bind (first last kind)
            (if (zerop new-count)
                ;; git reports a deletion after 1-based line NEW-START: mark the line
                ;; that follows the gap (0-based NEW-START), or the last line
                (let ((line (min new-start (1- lines)))) (values line line :deleted))
                (values (1- new-start) (min (1- lines) (+ new-start new-count -2)) (if (zerop old-count) :added :changed)))
          (let ((first (max first (1+ taken))))
            (when (<= first last)
              (push (line-mark buffer first last kind) marks)
              (setf taken last))))))
    (and marks (coerce (nreverse marks) 'simple-vector))))

(defstruct (vc-job (:include shown-text-job) (:constructor make-vc-job (buffer version)) (:copier nil))
  "The version-control marks of BUFFER's file worked out in the background
from its text at VERSION, a look at a file shown (SHOWN-TEXT-JOB); RESULT,
once done, (TRACKED HUNKS) as VC-START-REFRESH says.")

(setf (documentation 'vc-job-p 'function) "True when OBJECT is a VC-JOB.")

(tuatara.runtime:define-job-words vc-job
  :doing "Comparing with git"
  :left-words "the marks are as they were")

(defun vc-start-refresh (buffer)
  "Work out in the background whether BUFFER's file is in git and its marks,
a job (VC-JOB): git asked for the committed text's lines (COMMITTED-LINE-HASHES,
or GIT-TRACKED-P for a file too large to compare) and the texts compared on a thread of their own, from
the buffer's text as it is now, which never changes, so that nothing waits
for it; the result put in as the job is taken in. A refresh started before
for BUFFER is stopped (START-SHOWN-TEXT-JOB). The text of a file mapped from
disk and compared is read as a slice (BUFFER-SLICE), which keeps the mapping
while the job reads it."
  (let* ((path (tuatara.buffer:buffer-file buffer))
         (compare (and (ignore-errors (tuatara.io:path-stamp path))
                       (<= (tuatara.buffer:buffer-size buffer) *vc-gutter-max-bytes*))))
    (setf (tuatara.buffer:buffer-local buffer 'vc-edited) nil)
    (start-shown-text-job
     (make-vc-job buffer nil) 'vc-refresh
     (lambda (job)
       (let ((cancel (tuatara.runtime:job-cancel job)))
         (if compare
             (multiple-value-bind (committed too-large) (committed-line-hashes path :cancel cancel)
               (list (and (or committed too-large) t)
                     (and committed (diff-hunks committed (rope-line-hashes (vc-job-rope job))))))
             (list (git-tracked-p path :cancel cancel) nil))))
     :name "tuatara version control" :slice compare)))

(defmethod tuatara.runtime:job-take-in ((job vc-job))
  "Put in the marks worked out in the background when the buffer's text is
still the one compared, else start again; a file git could not be asked
about is left without marks."
  (let ((buffer (vc-job-buffer job)))
    (when (shown-text-job-taken-p job 'vc-refresh)
      (destructuring-bind (&optional tracked hunks) (vc-job-result job)
        (setf (tuatara.buffer:buffer-local buffer 'vc-tracked) (and tracked *vc-gutter-enabled* t))
        (when (tuatara.buffer:buffer-local buffer 'vc-tracked)
          (pushnew #'vc-note-edit (tuatara.buffer:buffer-change-functions buffer))
          (if (eq (vc-job-version job) (tuatara.buffer:buffer-version buffer))
              (setf (tuatara.buffer:buffer-local buffer 'vc-marks) (hunk-marks buffer hunks))
              (vc-start-refresh buffer)))))))

(defun touched-lines (buffer batch)
  "The lines of BUFFER's text, as it is after BATCH, that BATCH's edits
touched: a list of (FIRST . LAST) line ranges, from 0, in order, ranges that
meet or touch joined. Complexity: O(edits × log lines)."
  (let ((size (tuatara.buffer:buffer-size buffer)) (ranges '()))
    (loop for e across (tuatara.text:batch-edits batch)
          for p of-type fixnum across (tuatara.text:batch-positions batch)
          do (let ((first (tuatara.buffer:buffer-line-number buffer (min size (max 0 (- p (tuatara.text:edit-insert-length e))))))
                   (last (tuatara.buffer:buffer-line-number buffer (min size p))))
               (if (and ranges (<= first (1+ (cdar ranges))))
                   (setf (cdar ranges) (max last (cdar ranges)))
                   (push (cons first last) ranges))))
    (nreverse ranges)))

(defun first-mark-ending-at-or-after (marks version byte)
  "The index of the first of MARKS, a vector in order of place, whose last
line starts at or after BYTE of the text at VERSION; its length when none.
Complexity: O(log marks)."
  (tuatara.runtime:bisect (mid 0 (length marks))
                          (< (tuatara.text:anchor-position (vc-mark-last (svref marks mid)) version) byte)))

(defun first-mark-starting-at-or-after (marks version byte)
  "The index of the first of MARKS, a vector in order of place, that starts at
or after BYTE of the text at VERSION; its length when none. Complexity:
O(log marks)."
  (tuatara.runtime:bisect (mid 0 (length marks))
                          (< (tuatara.text:anchor-position (vc-mark-anchor (svref marks mid)) version) byte)))

(defun mark-lines (buffer mark)
  "The first and the last line MARK covers in BUFFER's text as it is now, two values."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (values (tuatara.buffer:buffer-line-number buffer (tuatara.text:anchor-position (vc-mark-anchor mark) version))
            (tuatara.buffer:buffer-line-number buffer (tuatara.text:anchor-position (vc-mark-last mark) version)))))

(defun line-after-start (buffer line)
  "Where the line after LINE of BUFFER starts, or one past the text's end when LINE is its last."
  (if (< line (1- (tuatara.buffer:buffer-line-count buffer)))
      (tuatara.buffer:buffer-goto-line buffer (1+ line))
      (1+ (tuatara.buffer:buffer-size buffer))))

(defun vc-note-edit (buffer batch)
  "Change function: mark the lines touched by BATCH that no mark covers yet as
changed until the next save, or until the text is back to what was saved
\(VC-ATTACH-SHOWN); none while the text's lines are still being counted.
Complexity: O(edits × log lines + marks met), and O(marks) more when new
marks are put in among them."
  (when (and (tuatara.buffer:buffer-local buffer 'vc-tracked) (tuatara.buffer:buffer-indexed-p buffer))
    (setf (tuatara.buffer:buffer-local buffer 'vc-edited) t)
    (let* ((version (tuatara.buffer:buffer-version buffer))
           (marks (or (tuatara.buffer:buffer-local buffer 'vc-marks) #()))
           (added '()))
      ;; each range's lines no mark covers, marked, with the index among MARKS each goes before
      (loop for (first . last) in (touched-lines buffer batch)
            do (let ((high (first-mark-starting-at-or-after marks version (line-after-start buffer last)))
                     (next first))
                 (loop for j from (first-mark-ending-at-or-after marks version (tuatara.buffer:buffer-goto-line buffer first))
                         below high
                       do (multiple-value-bind (from to) (mark-lines buffer (svref marks j))
                            (when (< next from)
                              (push (cons j (line-mark buffer next (1- from) :changed)) added))
                            (setf next (max next (1+ to)))))
                 (when (<= next last)
                   (push (cons high (line-mark buffer next last :changed)) added))))
      (when added
        (let ((all (make-array (+ (length marks) (length added)))) (at 0) (from 0))
          (loop for (index . mark) in (nreverse added)
                do (replace all marks :start1 at :start2 from :end2 index)
                   (incf at (- index from))
                   (setf from index
                         (svref all at) mark)
                   (incf at))
          (replace all marks :start1 at :start2 from)
          (setf (tuatara.buffer:buffer-local buffer 'vc-marks) all))))))

(defun vc-visible-marks (buffer from to)
  "The marks of BUFFER over its text from byte FROM to byte TO, worked out once
for a frame: a list of (FIRST LAST KIND), the lines each covers, in order.
Complexity: O(log marks + the marks there)."
  (let ((marks (tuatara.buffer:buffer-local buffer 'vc-marks))
        (version (tuatara.buffer:buffer-version buffer)))
    (when marks
      (loop for j from (first-mark-ending-at-or-after marks version (tuatara.buffer:buffer-line-start buffer from))
              below (first-mark-starting-at-or-after marks version (1+ to))
            collect (multiple-value-bind (first last) (mark-lines buffer (svref marks j))
                      (list first last (vc-mark-kind (svref marks j))))))))

(defun vc-kind-on (visible line)
  "The kind of the first of VISIBLE (as VC-VISIBLE-MARKS gives them) that covers LINE, or NIL."
  (loop for (first last kind) in visible
        when (<= first line last) return kind))

(defun vc-mark-at (buffer line)
  "The kind of mark covering 0-based LINE in BUFFER, or NIL. Complexity: O(log marks)."
  (let ((start (tuatara.buffer:buffer-goto-line buffer line)))
    (vc-kind-on (vc-visible-marks buffer start start) line)))

(defun vc-gutter-prepare (editor window rows)
  "The marks over the text of ROWS of WINDOW, worked out once a frame (VC-VISIBLE-MARKS)."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (plusp (length rows)) (tuatara.buffer:buffer-local buffer 'vc-marks) (tuatara.buffer:buffer-indexed-p buffer))
      (vc-visible-marks buffer (tuatara.display:row-start (aref rows 0)) (tuatara.display:row-end (aref rows (1- (length rows))))))))

(defun vc-gutter-width (editor window)
  "One column for marks when the buffer is a tracked file, else 0."
  (declare (ignore editor))
  (if (tuatara.buffer:buffer-local (tuatara.display:window-buffer window) 'vc-tracked) 1 0))

(defun vc-gutter-cell (editor window row line &optional (visible nil prepared))
  "The mark beside ROW: + added, ~ changed, - deleted, blank otherwise; from
VISIBLE, the marks of the frame VC-GUTTER-PREPARE gives, when given."
  (declare (ignore editor row))
  (if (null line)
      (values " " nil)
      (case (if prepared (vc-kind-on visible line) (vc-mark-at (tuatara.display:window-buffer window) line))
        (:added (values "+" :vc-added))
        (:changed (values "~" :vc-changed))
        (:deleted (values "-" :vc-deleted))
        (t (values " " nil)))))

(defun vc-attach (buffer)
  "Start tracking BUFFER if its file is in git: whether it is, and its marks,
worked out in the background (VC-START-REFRESH), again after every save;
edits marked as they are made."
  (when (and *vc-gutter-enabled* (tuatara.buffer:buffer-file buffer))
    (vc-start-refresh buffer)))

(define-command vc-gutter-refresh ()
  "Recompute the version-control marks of the current buffer from git."
  (vc-attach (current-buffer))
  (message "Version-control marks being worked out again"))

(define-command vc-gutter-mode ()
  "Switch the version-control gutter on or off for all buffers; with a count,
on when it is positive, off when it is zero or less."
  (setf *vc-gutter-enabled* (switched-by-count *vc-gutter-enabled*))
  (unless *vc-gutter-enabled*
    (dolist (b tuatara.buffer:*buffers*) (setf (tuatara.buffer:buffer-local b 'vc-tracked) nil)))
  (when *vc-gutter-enabled* (vc-attach (current-buffer)))
  (message "Version-control gutter ~a" (if *vc-gutter-enabled* "on" "off")))

(defun vc-after-save (buffer)
  "After BUFFER is saved: its marks worked out again, in the background, once a window shows it."
  (setf (tuatara.buffer:buffer-local buffer 'vc-checked) nil))

(defun vc-attach-shown (editor)
  "After each command: attach the version-control gutter to the buffers EDITOR's
windows show that have a file, once each (LOOK-AT-SHOWN-FILES); never to a
file opened safely, whose repository's settings git would read, nor to one
whose text no program may see. A tracked buffer edited and brought back to
its saved text (by undo) has its marks worked out again, so that the lines
put back are marked no more."
  (when *vc-gutter-enabled*
    (look-at-shown-files editor 'vc-checked #'vc-attach
                         :again (lambda (b)
                                  (when (and (tuatara.buffer:buffer-local b 'vc-edited)
                                             (tuatara.buffer:buffer-local b 'vc-tracked)
                                             (not (tuatara.buffer:buffer-modified-p b)))
                                    (vc-start-refresh b))))))

(defun install-vc-gutter (editor)
  "Register the version-control gutter provider and the refresh after saves,
and attach it to files as EDITOR's windows show them."
  (pushnew 'vc-attach-shown (editor-post-command-functions editor))
  (add-gutter-provider :vc #'vc-gutter-width #'vc-gutter-cell :prepare #'vc-gutter-prepare)
  (pushnew 'vc-after-save *after-save-functions*))
