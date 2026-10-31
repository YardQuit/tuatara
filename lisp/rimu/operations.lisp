;;;; lisp/rimu/operations.lisp --- Rimu: marks, and files copied, moved, made and put in the trash.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A list's entries can be marked (>) or marked for deletion (D), a mark in
;;;; the first column of its line, kept by name while the list is read again.
;;;; The operations work on the marked entries, or the one on the line: copied
;;;; (C) or moved and renamed (R) to a directory asked for, another list's
;;;; beside it offered first; put in the trash (D, and x for those marked for
;;;; deletion), or with C-u D deleted for good; packed into one tar archive
;;;; (c), or archives extracted (Z) or listed (C-c e), as archives.lisp does
;;;; it; two compared (=), as differences.lisp does it; + makes a
;;;; directory. What is asked, the place and whether names already there are
;;;; replaced, is asked first; the work then runs as a job in the background
;;;; (jobs.lisp), P lit in the list's mode line, C-x J (stop-job) stopping it,
;;;; and the lists of the directories it touched are read again after.
;;;; Nothing is replaced unasked, and what is replaced goes to the trash first.
;;;; Buffers visiting a file moved follow it. As Emacs's dired does, the trash
;;;; as delete-by-moving-to-trash has it.

(in-package #:tuatara.defaults)

(defvar *rimu-delete-to-trash* t
  "True for rimu's deletions (\\<rimu-mode>\\[list-delete-marked], \\[list-execute]) to move files and
directories to the trash the desktop's file managers use, from which they can
be put back; NIL to delete them for good, after asking twice.")

;;; Marks

(defun rimu-marks (buffer)
  "The marks of the rimu list BUFFER: a table from a file's full native name
\(RIMU-MARK-KEY) to its mark, #\\> marked, #\\D marked for deletion, or
another character a command gives (RIMU-MARK-AND-GO-ON), as RIMU-MARK-MASTER marks
masters M. Marks stay with their files while the list goes to other
directories, so that files of several can be marked and acted on together."
  (or (tuatara.buffer:buffer-local buffer 'rimu-marks)
      (setf (tuatara.buffer:buffer-local buffer 'rimu-marks) (make-hash-table :test 'equal))))

(defun rimu-mark-key (directory entry)
  "The key of the mark of ENTRY of DIRECTORY in a list's marks (RIMU-MARKS):
its full native name."
  (rimu-path directory entry))

(defun rimu-show-mark (buffer line mark)
  "Show MARK (or none, NIL) in the first column of LINE of the rimu list BUFFER."
  (let ((start (tuatara.buffer:buffer-goto-line buffer line)))
    (when (< start (tuatara.buffer:buffer-size buffer))
      (tuatara.buffer:with-read-only-lifted (buffer :after t)
        (tuatara.buffer:buffer-replace buffer start (1+ start) (string (or mark #\Space))))
      (tuatara.buffer:buffer-mark-saved buffer)
      (setf (tuatara.buffer:buffer-undo-stack buffer) '()
            (tuatara.buffer:buffer-redo-stack buffer) '()))))

(defun rimu-markable-p (entry)
  "True when ENTRY can be marked: any but . and .."
  (and entry (not (rimu-dots-p entry))))

(defun rimu-set-mark (buffer line mark)
  "Give the entry on LINE of the rimu list BUFFER MARK, or none with NIL; a
list narrowed by search filters is laid out again soon after, so that the
whole list it is narrowed from shows the mark too (RIMU-LAY-OUT-MARKED)."
  (let ((entry (rimu-line-entry buffer line)))
    (when (rimu-markable-p entry)
      (let ((key (rimu-mark-key (tuatara.buffer:buffer-local buffer 'rimu-directory) entry)))
        (if mark
            (setf (gethash key (rimu-marks buffer)) mark)
            (remhash key (rimu-marks buffer))))
      (rimu-show-mark buffer line mark)
      (when (list-filters-on-p buffer)
        (setf (tuatara.buffer:buffer-local buffer 'rimu-marks-changed) t))
      t)))

(defun rimu-mark-and-go-on (mark)
  "Give the entry on the current line MARK (NIL: none) and go to the next line."
  (let* ((buffer (current-buffer))
         (line (tuatara.buffer:buffer-line-number buffer (point))))
    (rimu-set-mark buffer line mark)
    (when (rimu-line-entry buffer (1+ line))
      (goto (rimu-line-position buffer (1+ line))))))

(defmethod mark-entries ((mode rimu-mode) list mark count)
  "Marked (>), marked for deletion (D) or no mark, kept by the file's full
name (RIMU-MARKS), the line shown again at once."
  (let ((char (case mark (:marked #\>) (:deletion #\D))))
    (loop repeat (abs count)
          do (if (plusp count)
                 (rimu-mark-and-go-on char)
                 (let ((line (1- (tuatara.buffer:buffer-line-number list (point)))))
                   (when (>= line 0)
                     (goto (rimu-line-position list line))
                     (rimu-set-mark list line char)))))))

(defun rimu-each-line (buffer function)
  "Call FUNCTION with each line of the rimu list BUFFER showing an entry, and
the entry, in the order shown; those search filters hide
\(\\[search-filter-regex]) too, with NIL for their line."
  (let ((layout (tuatara.buffer:buffer-local buffer 'rimu-layout)))
    (when layout
      (let ((lines (and (list-filters-on-p buffer)
                        (let ((table (make-hash-table :test 'eq)))
                          (multiple-value-bind (entries lines) (rimu-shown-entries buffer)
                            (loop for entry in entries
                                  for line across lines
                                  do (setf (gethash entry table) line)))
                          table))))
        (loop for entry across (rimu-layout-lines layout)
              for line from 0
              when (rimu-markable-p entry)
                do (funcall function (if lines (gethash entry lines) line) entry))))))

(defparameter *rimu-marks-shown-by-line* 200
  "The most lines of a rimu list whose marks are changed line by line at once;
when more change together, the list is laid out again in the background with
its new marks (RIMU-SET-MARKS).")

(defun rimu-set-marks (buffer changes)
  "Give the entries of the rimu list BUFFER new marks, CHANGES each (LINE ENTRY
MARK), MARK NIL for none: line by line for a few, else in its marks table,
the list then laid out again in the background, as it is while search
filters narrow it."
  (if (and (<= (length changes) *rimu-marks-shown-by-line*) (not (list-filters-on-p buffer)))
      (loop for (line nil mark) in changes do (rimu-set-mark buffer line mark))
      (let ((marks (rimu-marks buffer))
            (directory (tuatara.buffer:buffer-local buffer 'rimu-directory)))
        (loop for (nil entry mark) in changes
              for key = (rimu-mark-key directory entry)
              do (if mark (setf (gethash key marks) mark) (remhash key marks)))
        (rimu-update buffer))))

(defmethod unmark-entries ((mode rimu-mode) buffer)
  "Every mark of any kind, in this directory and the others; the line saying
how many are marked in other directories goes too."
  (let* ((marks (rimu-marks buffer))
         (count (hash-table-count marks))
         (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (found (rimu-found-p buffer)))
    (if (loop for key being the hash-keys of marks thereis (not (rimu-mark-here-p key directory found)))
        (progn (clrhash marks)
               (rimu-update buffer))
        (let ((changes '()))
          (rimu-each-line buffer (lambda (line entry)
                                   (when (gethash (rimu-mark-key directory entry) marks)
                                     (push (list line entry nil) changes))))
          (rimu-set-marks buffer changes)
          (clrhash marks)))
    count))

(defmethod swap-entry-marks ((mode rimu-mode) buffer)
  "The entries of the directory shown; the files marked in other directories
stay so, as do the entries search filters hide."
  (let ((marked 0)
        (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
        (changes '()))
    (rimu-each-line buffer (lambda (line entry)
                             (when line
                               (case (gethash (rimu-mark-key directory entry) (rimu-marks buffer))
                                 (#\> (push (list line entry nil) changes))
                                 ((nil) (push (list line entry #\>) changes) (incf marked))))))
    (rimu-set-marks buffer changes)
    marked))

(defun rimu-chosen (buffer mark &key here-only)
  "The entries of the directory the rimu list BUFFER shows with MARK, in the
order shown, those search filters hide among them; with MARK #\\* and none marked in any directory (with HERE-ONLY,
in this one), the one on the line, when it is not . or .. (RIMU-CHOSEN-PATHS
takes those of the others too)."
  (let ((chosen '())
        (directory (tuatara.buffer:buffer-local buffer 'rimu-directory)))
    (rimu-each-line buffer (lambda (line entry)
                             (declare (ignore line))
                             (when (eql mark (gethash (rimu-mark-key directory entry) (rimu-marks buffer)))
                               (push entry chosen))))
    (or (nreverse chosen)
        (and (eql mark #\>)
             (or here-only (null (rimu-mark-keys-elsewhere buffer mark)))
             (let ((entry (rimu-entry-at buffer (tuatara.buffer:buffer-point buffer))))
               (and (rimu-markable-p entry) (list entry)))))))

(defun rimu-mark-here-p (key directory &optional found)
  "True when KEY, the full name of a file marked (RIMU-MARK-KEY), is of the
directory DIRECTORY (a native name ending in /) a list shows: in it, or with
FOUND, for a list of files found under DIRECTORY, anywhere under it."
  (let ((length (length directory)))
    (and (> (length key) length)
         (string= directory key :end2 length)
         (or found (not (position #\/ key :start length :end (1- (length key))))))))

(defun rimu-marked-elsewhere (buffer &key (mark #\>) files-only)
  "The full names of the files of the rimu list BUFFER with MARK in other
directories than the one it shows, still there, in order of their names; with
FILES-ONLY only regular files, a link to one too. Each is looked at once."
  (sort (remove-if-not (lambda (key) (rimu-mark-still-there-p key files-only))
                       (rimu-mark-keys-elsewhere buffer mark))
        #'string<))

(defun rimu-nothing-words (buffer verb &key (mark #\>) files-only none)
  "What is said when an operation that would VERB the entries with MARK of
the rimu list BUFFER finds none (RIMU-CHOSEN-PATHS, FILES-ONLY as there):
that those marked in other directories are gone, or not files, when it has
such marks; else NONE, or that there is nothing to VERB here."
  (cond ((rimu-mark-keys-elsewhere buffer mark)
         (substitute-command-keys
          (format nil "Nothing to ~a: the files marked in other directories are gone~:[~; or are not files~] (\\<rimu-mode>\\[list-unmark-all] takes the marks away)"
                  verb files-only)))
        (none)
        (t (format nil "Nothing to ~a here" verb))))

(defun rimu-take-out (buffer entries &key reread only)
  "Take the lines of ENTRIES, of the directory the rimu list BUFFER shows, out
of the list, the files left as they are, and lay it out again, reading the
directory again with REREAD; with ONLY, the lines taken out before are shown
again, so that ENTRIES alone are out (none: every line shown).
\\<rimu-mode>\\[revert-buffer] or listing another directory shows them again. Their
marks stay."
  (when only (setf (tuatara.buffer:buffer-local buffer 'rimu-taken-out) nil))
  (let ((out (or (tuatara.buffer:buffer-local buffer 'rimu-taken-out)
                 (setf (tuatara.buffer:buffer-local buffer 'rimu-taken-out) (make-hash-table :test 'equalp)))))
    (dolist (entry entries)
      (setf (gethash (tuatara.io:listed-octets entry) out) t))
    (rimu-update buffer :reread reread)))

(define-command rimu-take-out-lines ()
  "Take the lines of the entries marked in this directory out of this list,
or with none marked here the line point is on, their marks taken away: the files stay as they are, only
the list shows fewer, to look at what is left. \\[revert-buffer] shows them again, as
does listing another directory."
  (let* ((buffer (current-buffer))
         (chosen (rimu-chosen buffer #\> :here-only t)))
    (if (null chosen)
        (message "No file on this line")
        (progn
          (dolist (entry chosen)
            (remhash (rimu-mark-key (tuatara.buffer:buffer-local buffer 'rimu-directory) entry) (rimu-marks buffer)))
          (rimu-take-out buffer chosen)
          (message "~d line~:p taken out of the list; \\<rimu-mode>\\[revert-buffer] shows ~:*~[~;it~:;them~] again" (length chosen))))))

(defun rimu-chosen-paths (buffer &key files-only (mark #\>) here-only)
  "The paths of the entries an operation in the rimu list BUFFER works on, those
with MARK: the directory's shown in their order (RIMU-CHOSEN), then, unless
HERE-ONLY, those marked in other directories (RIMU-MARKED-ELSEWHERE); with
FILES-ONLY only regular files. NIL when BUFFER is no list."
  (when (rimu-list-p buffer)
    (let ((directory (tuatara.buffer:buffer-local buffer 'rimu-directory)))
      (append (loop for entry in (rimu-chosen buffer mark)
                    when (or (not files-only) (eq (rimu-leads-to entry) :regular))
                      collect (tuatara.io:native (rimu-path directory entry)))
              (and (not here-only)
                   (rimu-marked-elsewhere buffer :mark mark :files-only files-only))))))

(defun rimu-paths-marked-anywhere (mark &key files-only)
  "The paths of the entries with MARK in every rimu list and every directory
each has shown, as RIMU-CHOSEN-PATHS gives them, each once, the first list's
first; with FILES-ONLY only regular files."
  (remove-duplicates (loop for buffer in tuatara.buffer:*buffers*
                           when (rimu-list-p buffer)
                             append (rimu-chosen-paths buffer :files-only files-only :mark mark))
                     :test #'string= :from-end t))

(defun rimu-paths-marked-apart (mark &key files-only (buffer (current-buffer)))
  "The paths with MARK in every rimu list (RIMU-PATHS-MARKED-ANYWHERE), and
those an operation in the rimu list BUFFER works on in the directory it shows
(RIMU-CHOSEN-PATHS), less those with MARK, as two values: masters marked
anywhere, and what is compared with them here. With FILES-ONLY only regular
files."
  (let ((marked (rimu-paths-marked-anywhere mark :files-only files-only)))
    (values marked
            (remove-if (lambda (path) (member path marked :test #'string=))
                       (rimu-chosen-paths buffer :files-only files-only :here-only t)))))

(defparameter *rimu-master-mark* #\M
  "The mark of a master in a rimu list (\\<rimu-mode>\\[rimu-mark-master]): a file the
files marked here are compared with, by the commands that compare them.")

(define-command rimu-mark-master ()
  "Mark the file on the line of this rimu list as a master (M), a file that
the files marked (\\<rimu-mode>\\[list-mark-entry]) are compared with by the
commands that compare files, a photo of the person looked for among photos,
and go to the next line; \\[list-unmark-entry] takes the mark away. Masters
may be marked in any list: such a command takes them all."
  (if (rimu-list-p (current-buffer))
      (rimu-mark-and-go-on *rimu-master-mark*)
      (message "Masters are marked in a rimu list (\\[rimu])")))

;;; Where to

(defun rimu-other-directory (buffer)
  "The directory of another rimu list shown in a window beside BUFFER's, the
target copies and moves are offered first; NIL when there is none."
  (loop for window in (editor-windows)
        for shown = (tuatara.display:window-buffer window)
        when (and (not (eq shown buffer)) (rimu-list-p shown)
                  (not (tuatara.buffer:buffer-local shown 'rimu-preview-buffer)))
          return (tuatara.buffer:buffer-local shown 'rimu-directory)))

(defun rimu-destinations (paths target)
  "Where each of PATHS goes, copied or moved to TARGET: into it when it is a
directory, else (one path only) to that name. A list of (FROM . TO)."
  (if (eq :directory (tuatara.io:path-kind target))
      (mapcar (lambda (from) (cons from (tuatara.io:path-join target (tuatara.io:native-file-name from)))) paths)
      (list (cons (first paths) (string-right-trim "/" target)))))

;;; Work in the background

(defstruct (rimu-operation (:include tuatara.runtime:job) (:constructor make-rimu-operation (verb pairs buffer left &optional then))
                           (:copier nil))
  "Files being worked on in the background for BUFFER, a rimu list or the
list of the trash, a job (TUATARA.RUNTIME:JOB): VERB, one of *RIMU-VERBS*; PAIRS, each (FROM . TO), TO NIL for a deletion; LEFT, what was
left alone before it began, as words; DONE, how many are through; FAILED, sentences of what could
not be done; REPLACED, how many names there went to the trash first;
DONE-PAIRS, the pairs done; THEN, a function of no arguments called once it
is taken in, or NIL. It stops between files."
  verb pairs left then (done 0) (failed '()) (replaced 0) (done-pairs '()))

(setf (documentation 'rimu-operation-p 'function) "True when OBJECT is a RIMU-OPERATION.")

(defparameter *rimu-verbs*
  '((:copy "copy" "Copy" "Copying" "Copied")
    (:move "move" "Move" "Moving" "Moved")
    (:symlink "symlink" "Symlink" "Symlinking" "Symlinked")
    (:hardlink "hard link" "Hard link" "Hard linking" "Hard linked")
    (:archive "archive" "Archive" "Archiving" "Archived")
    (:extract "extract" "Extract" "Extracting" "Extracted")
    (:trash "move to the trash" "Move to the trash" "Moving to the trash" "Moved to the trash")
    (:delete "delete" "Delete" "Deleting" "Deleted for good"))
  "What rimu does to files in the background, each with the words said of it:
the verb, the verb to ask with, what it is doing and what it did.")

(defun rimu-verb-words (verb which)
  "The words of VERB, one of *RIMU-VERBS*: WHICH :verb (copy), :ask (Copy),
:doing (Copying) or :done (Copied)."
  (nth (ecase which (:verb 1) (:ask 2) (:doing 3) (:done 4)) (assoc verb *rimu-verbs*)))

(defun rimu-counted (verb count)
  "COUNT of what VERB acts on, in words: 3 files, or for an extraction 1 archive."
  (format nil "~:d ~a~:[s~;~]" count (if (eq verb :extract) "archive" "file") (= count 1)))

(defun rimu-target (verb pairs)
  "Where VERB puts what it does to PAIRS, in words, NIL for a deletion: the
directory copies, moves and links go to, the archive made, the directory
archives are extracted into."
  (let ((to (cdr (first pairs))))
    (and to (case verb
              ((:copy :move :symlink :hardlink) (abbreviated-path (tuatara.io:parent-path to)))
              ((:archive :extract) (abbreviated-path to))))))

(defun rimu-operation-words (operation)
  "What OPERATION does, in a word or a few: Copying, Moving, Symlinking and the others."
  (rimu-verb-words (rimu-operation-verb operation) :doing))

(defmethod tuatara.runtime:job-percent ((job rimu-operation))
  "How much of the files of JOB, a copy, move or other operation, is done, from 0 to 99."
  (tuatara.runtime:running-percent (rimu-operation-done job) (length (rimu-operation-pairs job))))

(tuatara.runtime:define-job-words rimu-operation
  :doing (rimu-operation-words job)
  :description (let ((pairs (rimu-operation-pairs job)))
    (format nil "~a ~a~@[ to ~a~]" (rimu-operation-words job) (rimu-counted (rimu-operation-verb job) (length pairs))
            (rimu-target (rimu-operation-verb job) pairs)))
  :verb (rimu-verb-words (rimu-operation-verb job) :verb))

(defmethod tuatara.runtime:job-consequence ((job rimu-operation))
  "What stopping JOB, an operation on files, leaves: the file under way taken
away for a copy, a move or an archive, what is extracted so far kept."
  (case (rimu-operation-verb job)
    ((:copy :move :archive) "The file under way is taken away")
    (:extract "What is extracted so far stays")))

(defmethod tuatara.runtime:job-holds-queue-p ((job rimu-operation))
  "False: other work on the list need not wait for an operation on its files."
  nil)

(defmethod tuatara.runtime:job-stop ((job rimu-operation))
  "It is waited for a moment, the copy under way taken away."
  (call-next-method)
  (let ((thread (rimu-operation-thread job)))
    (when (and thread (not (eq thread sb-thread:*current-thread*)))
      (ignore-errors (sb-thread:join-thread thread :timeout 5 :default nil)))))

(defun rimu-work (verb pair cancel)
  "Do VERB to PAIR, (FROM . TO), stopping between files when CANCEL's car
turns true. An error when not all of it was done: a named pipe, a socket or a
device left out of a copy is said; a copy stopped or failed half-way is taken
away, so that no file is left with part of what it should hold."
  (destructuring-bind (from . to) pair
    (let ((skipped '()))
      (flet ((skip (path words) (push (format nil "~a (~a)" (tuatara.io:native-file-name path) words) skipped)))
        (ecase verb
          ;; a copy stopped or failed half-way leaves nothing under its name (COPY-PATH-TREE)
          (:copy (let ((whole (tuatara.io:copy-path-tree from to :cancel cancel :skipped #'skip)))
                   (cond ((and (not whole) (car cancel))
                          (error "stopped; what was copied of it taken away"))
                         ((not whole)
                          (error "not all copied~@[, left out: ~{~a~^, ~}~]" (reverse skipped))))))
          (:move (handler-case (tuatara.io:move-path from to :cancel cancel :skipped #'skip)
                   (tuatara.io:file-system-error (e)
                     (if skipped (error "~a; left out: ~{~a~^, ~}" e (reverse skipped)) (error e)))))
          (:symlink (tuatara.io:make-symbolic-link (string-right-trim "/" from) to))
          (:hardlink (tuatara.io:make-hard-link (string-right-trim "/" from) to))
          (:extract (extract-archive from to :cancel cancel))
          (:trash (tuatara.io:trash-path from))
          (:delete (tuatara.io:delete-tree from :cancel cancel)))))))

(defun rimu-same-file-p (a b)
  "True when the paths A and B are the same file, as two names of one
directory reached through a symbolic link are, or when A is a symbolic link
that leads to B: put in B's place, it would lose what it leads to."
  (let ((other (tuatara.io:path-stamp b :follow nil)))
    (flet ((same-p (one)
             (and one
                  (eql (tuatara.io:stamp-dev one) (tuatara.io:stamp-dev other))
                  (eql (tuatara.io:stamp-ino one) (tuatara.io:stamp-ino other)))))
      (and other
           (or (same-p (tuatara.io:path-stamp a :follow nil))
               (same-p (ignore-errors (tuatara.io:path-stamp a))))))))

(defun rimu-archive-work (operation pairs replace)
  "Pack the FROMs of PAIRS, all of one TO, into that archive for OPERATION
\(WRITE-ARCHIVE), with REPLACE what is there moved to the trash first: all of
them in one step, or none."
  (let ((archive (cdr (first pairs))))
    (handler-case
        (let ((there (tuatara.io:path-stamp archive :follow nil)))
          (write-archive (mapcar #'car pairs) archive :cancel (rimu-operation-cancel operation) :replace replace)
          (when (and replace there) (incf (rimu-operation-replaced operation)))
          (setf (rimu-operation-done-pairs operation) (reverse pairs)
                (rimu-operation-done operation) (length pairs)))
      (serious-condition (e)
        (push (format nil "~a: ~a" (tuatara.io:native-file-name archive) e) (rimu-operation-failed operation))
        (setf (rimu-operation-done operation) 1)))))

(defun rimu-start-operation (verb pairs list &key replace left then along)
  "Do VERB to each of PAIRS, (FROM . TO), as a job of LIST, a rimu list or the
list of the trash (TUATARA.RUNTIME:START-JOB-THREAD), :archive to all of them
at once (RIMU-ARCHIVE-WORK); with REPLACE, the names
there first moved to the trash. ALONG, a function of a pair's FROM, gives a
file to delete with it once it is done, or NIL (a thing's .trashinfo in the
trash), not counted apart. LEFT, what was left alone, is said when it is
over; THEN, a function of no arguments, is called then. Nothing is begun
when one of them would write in an evidence folder (*EVIDENCE-FOLDERS*):
that is said, and NIL returned."
  (let ((refusal (loop for (from . to) in pairs
                       thereis (or (and (member verb '(:move :trash :delete :hardlink))
                                        (evidence-refusal (rimu-verb-words verb :verb) from))
                                   (and to (evidence-refusal (rimu-verb-words verb :verb) to))))))
    (when refusal
      (message "~a: nothing done" refusal)
      (return-from rimu-start-operation nil)))
  (when (member verb '(:move :trash :delete))
    (check-not-saving (mapcar #'car pairs)))
  (let ((operation (make-rimu-operation verb pairs list left then)))
    (tuatara.runtime:start-job-thread
     operation (lambda (operation)
                 (if (eq verb :archive)
                     (rimu-archive-work operation pairs replace)
                     (dolist (pair pairs)
                       (when (car (rimu-operation-cancel operation)) (return))
                       (handler-case
                           (progn
                             (when (and (cdr pair) (rimu-same-file-p (car pair) (cdr pair)))
                               (error "it is there already, the very same file"))
                             (when (and replace (cdr pair) (tuatara.io:path-stamp (cdr pair) :follow nil))
                               (tuatara.io:trash-path (cdr pair))
                               (incf (rimu-operation-replaced operation)))
                             (rimu-work verb pair (rimu-operation-cancel operation))
                             (let ((with (and along (funcall along (car pair)))))
                               (when (and with (tuatara.io:path-stamp with :follow nil))
                                 (tuatara.io:remove-file with)))
                             (push pair (rimu-operation-done-pairs operation)))
                         (serious-condition (e) (push (format nil "~a: ~a" (tuatara.io:native-file-name (car pair)) e)
                                                      (rimu-operation-failed operation))))
                       (incf (rimu-operation-done operation))
                       (let ((wake (rimu-operation-wake operation)))
                         (when wake (ignore-errors (funcall wake)))))))
     :name "tuatara rimu operation" :details (format nil "~:d file~:p" (length pairs)))
    (message "~a ~d...~@[ (~a)~]" (rimu-operation-words operation) (length pairs) (rimu-target verb pairs))
    operation))

(defun rimu-refresh-lists (paths)
  "Read again, in the background, the rimu lists showing a directory any of
PATHS is in, or is, and the lists of files found under a directory any of
PATHS is in."
  (let ((directories (make-hash-table :test 'equal)))
    (dolist (path paths)
      (setf (gethash (tuatara.io:parent-path path) directories) t
            (gethash (concatenate 'string (string-right-trim "/" path) "/") directories) t))
    (dolist (buffer (copy-list tuatara.buffer:*buffers*))
      (when (and (rimu-list-p buffer) (not (rimu-rewound-p buffer))
                 (let ((directory (tuatara.buffer:buffer-local buffer 'rimu-directory)))
                   (if (rimu-found-p buffer)
                       (some (lambda (path) (tuatara.io:path-within-p path directory)) paths)
                       (gethash directory directories))))
        (rimu-update buffer :reread t :wait nil)))))

(defmethod tuatara.runtime:job-take-in ((operation rimu-operation))
  "Take in OPERATION, over: say how it went, follow the files moved, forget the
marks of what went, and read the lists touched again."
  (let* ((pairs (rimu-operation-pairs operation))
         ;; only what was done is followed and forgotten: a move that failed leaves its buffers and marks
         (done (rimu-operation-done-pairs operation))
         (failed (reverse (rimu-operation-failed operation)))
         (list (rimu-operation-buffer operation))
         (count (- (rimu-operation-done operation) (length failed)))
         (target (rimu-target (rimu-operation-verb operation) pairs)))
    (when (eq (rimu-operation-verb operation) :move)
      (follow-moved-paths done))
    (when (and (live-buffer-p list) (rimu-list-p list) (member (rimu-operation-verb operation) '(:move :trash :delete)))
      (let ((directory (tuatara.buffer:buffer-local list 'rimu-directory))
            (found (rimu-found-p list))
            (elsewhere nil))
        (dolist (pair done)
          (let ((key (tuatara.io:native (car pair))))
            (when (and (remhash key (rimu-marks list)) (not (rimu-mark-here-p key directory found)))
              (setf elsewhere t))))
        ;; the line saying how many are marked elsewhere, laid out again (a read of its directory below comes after)
        (when elsewhere (rimu-update list :wait nil))))
    (rimu-refresh-lists (append (mapcar #'car pairs) (remove nil (mapcar #'cdr pairs))))
    (when (or (member (rimu-operation-verb operation) '(:trash :delete)) (plusp (rimu-operation-replaced operation)))
      (trash-changed))
    (when (rimu-operation-then operation)
      (funcall (rimu-operation-then operation)))
    (let ((said (format nil "~a ~a~@[ to ~a~]~@[ (~d replaced, the old in the trash)~]~@[; left alone: ~{~a~^, ~}~]~@[; not done: ~{~a~^; ~}~]"
                        (rimu-verb-words (rimu-operation-verb operation) :done)
                        (rimu-counted (rimu-operation-verb operation) count) target (and (plusp (rimu-operation-replaced operation)) (rimu-operation-replaced operation))
                        (rimu-operation-left operation) failed)))
      (ecase (rimu-operation-status operation)
        (:done (message "~a" said))
        ;; what was done before it stopped, or failed, is said too
        (:stopped (message "Stopped: ~(~a~)~a" (subseq said 0 1) (subseq said 1)))
        (:failed (message "~a; ~(~a~)~a" (tuatara.runtime:job-ending-words operation) (subseq said 0 1) (subseq said 1)))))))

;;; The operations

(defun rimu-question (verb paths &key here after)
  "What rimu asks before VERB acts on PATHS, as the lists ask (LIST-QUESTION),
without the question mark: VERB, the name of one or how many, and AFTER. With
HERE, the directory the list shows, also where they are when that is
elsewhere: the whole name of one, or \"7 files in 3 directories\". How many
of them the search filters narrowing the current list hide is said: 3 files
\(1 hidden by the filter)."
  (let ((directories (make-hash-table :test 'equal))
        (*list-hidden-count* (if (and *editor* (rimu-list-p (current-buffer)))
                                 (list-hidden-count (current-buffer) paths)
                                 *list-hidden-count*)))
    (when here
      (dolist (path paths) (setf (gethash (tuatara.io:parent-path path) directories) t)))
    (list-question verb "file" paths :the nil :after after
                   :name-of (lambda (path)
                              (if (and here (not (gethash here directories)))
                                  (abbreviated-path path)
                                  (tuatara.io:native-file-name path)))
                   :where (and here (rest paths)
                               (not (and (= 1 (hash-table-count directories)) (gethash here directories)))
                               (if (= 1 (hash-table-count directories))
                                   (format nil "in ~a" (abbreviated-path (tuatara.io:parent-path (first paths))))
                                   (format nil "in ~d directories" (hash-table-count directories)))))))

(defun rimu-place (verb)
  "Copy (VERB :copy), move (:move) or link (:symlink, :hardlink) the chosen
entries to a place asked for."
  (let* ((buffer (current-buffer))
         (paths (rimu-chosen-paths buffer))
         (here (tuatara.buffer:buffer-local buffer 'rimu-directory)))
    (if (null paths)
        (message "~a" (rimu-nothing-words buffer (rimu-verb-words verb :verb)))
        (let* ((offered (or (rimu-other-directory buffer)
                            here))
               (target (read-native-path (format nil "~a "
                                                 (rimu-question (rimu-verb-words verb :ask) paths :here here :after "to:"))
                                         :initial (abbreviated-path offered) :writing t)))
          (cond ((null target) (message "Nowhere given: nothing done"))
                ((and (eq verb :move) (not (eq :directory (tuatara.io:path-kind target))))
                 (message "~a is no directory: nothing moved (\\<rimu-mode>\\[rimu-rename] renames)" (abbreviated-path target)))
                ((and (> (length paths) 1) (not (eq :directory (tuatara.io:path-kind target))))
                 (message "~a is no directory: ~d files go only into one" (abbreviated-path target) (length paths)))
                (t (rimu-place-to verb paths target buffer)))))))

(defun rimu-itself-p (pair)
  "True when PAIR, (FROM . TO), would copy or move FROM onto itself or into
itself, a symbolic link on the way followed (FROM's own last link not
followed: a link is copied as a link)."
  (or (string= (string-right-trim "/" (car pair)) (cdr pair))
      (tuatara.io:path-within-p (cdr pair) (car pair))
      (let ((from (let ((above (tuatara.io:real-path (tuatara.io:parent-path (car pair)))))
                    (and above (tuatara.io:path-join above (tuatara.io:native-file-name (car pair))))))
            (into (tuatara.io:real-path (tuatara.io:parent-path (cdr pair)))))
        (and from into (tuatara.io:path-within-p into from)))))

(defun rimu-doubled-pairs (pairs)
  "Those of PAIRS, each (FROM . TO), whose TO an earlier one has: files of
one name from several directories, of which only one can go there."
  (let ((seen (make-hash-table :test 'equal)))
    (remove-if-not (lambda (pair) (shiftf (gethash (cdr pair) seen) t)) pairs)))

(defun rimu-doubled-question (verb doubled pairs)
  "The question whether to VERB (one of *RIMU-VERBS*) only the first of PAIRS
of each name there, DOUBLED (RIMU-DOUBLED-PAIRS) left alone."
  (let ((names (remove-duplicates (mapcar (lambda (pair) (escape-controls (tuatara.io:native-file-name (cdr pair)))) doubled)
                                  :test #'string= :from-end t))
        (verb (rimu-verb-words verb :verb)))
    (if (rest names)
        (format nil "Files of the same name from several directories (~{~a~^, ~}): only one of each can be there; ~a the first of each and leave the others alone?"
                names verb)
        (let ((others (length doubled)))
          (format nil "~d files named ~a: only one can be there; ~a the one in ~a and leave the other~p alone?"
                  (1+ others) (first names) verb
                  (abbreviated-path (tuatara.io:parent-path (car (find (cdr (first doubled)) pairs :key #'cdr :test #'string=))))
                  others)))))

(defun rimu-place-to (verb paths target buffer)
  "Copy (VERB :copy), move (:move) or link (:symlink, :hardlink) PATHS, chosen
in the rimu list BUFFER, to TARGET, asking first about files of one name from several directories and
about names there already, in the background; what is left alone is said."
  (let* ((pairs (rimu-sort-pairs (rimu-destinations paths target) paths))
         (itself (remove-if-not #'rimu-itself-p pairs))
         (pairs (remove-if (lambda (pair) (member pair itself :test #'eq)) pairs))
         (doubled (rimu-doubled-pairs pairs)))
    (when (and doubled (not (yes-or-no-p* (rimu-doubled-question verb doubled pairs))))
      (message "Nothing ~(~a~): only one file of a name can be there" (rimu-verb-words verb :done))
      (return-from rimu-place-to nil))
    (let* ((pairs (remove-if (lambda (pair) (member pair doubled :test #'eq)) pairs))
           (there (remove-if-not (lambda (pair) (tuatara.io:path-stamp (cdr pair) :follow nil)) pairs))
           (directories (remove-if-not (lambda (pair) (eq :directory (tuatara.io:path-kind (cdr pair)))) there))
           (pairs (set-difference pairs directories))
           (there (set-difference there directories))
           (replace (and there
                         (yes-or-no-p* (format nil "~a ~:[is~;are~] there already: replace, the old to the trash?"
                                               (rimu-question nil (mapcar #'car there)) (rest there)))))
           (pairs (if replace pairs (set-difference pairs there)))
           (left (append (mapcar (lambda (p) (format nil "~a (itself)" (tuatara.io:native-file-name (car p)))) itself)
                         (mapcar (lambda (p) (format nil "~a (another of its name goes there)" (tuatara.io:native-file-name (car p))))
                                 doubled)
                         (mapcar (lambda (p) (format nil "~a (a directory is there)" (tuatara.io:native-file-name (car p))))
                                 directories)
                         (and (not replace)
                              (mapcar (lambda (p) (format nil "~a (there already)" (tuatara.io:native-file-name (car p)))) there)))))
      (if pairs
          (rimu-start-operation verb (rimu-sort-pairs pairs paths) buffer :replace replace :left left)
          (message "Nothing to ~a~@[; left alone: ~{~a~^, ~}~]" (rimu-verb-words verb :verb) left)))))

(defun rimu-sort-pairs (pairs paths)
  "PAIRS, each (FROM . TO), in the order of their FROM among PATHS. O(n log n)."
  (let ((order (make-hash-table :test 'equal)))
    (loop for path in paths
          for i from 0
          do (unless (gethash path order) (setf (gethash path order) i)))
    (stable-sort (copy-list pairs) #'< :key (lambda (pair) (gethash (car pair) order 0)))))

(define-command rimu-copy ()
  "Copy the marked entries, or the one on the line, to a place asked for: the
directory of the other list beside this one offered first, or this one; one
entry may be given a new name. A directory is copied with all in it, links as
links, the modes and times kept. Names already there are replaced only when
you say so, the old going to the trash; a directory there is not merged."
  (rimu-place :copy))

(define-command rimu-move ()
  "Move the marked entries, or the one on the line, into a directory asked
for, navigating to it as \\[find-file] does, only directories offered: the directory
of the other list beside this one offered first. On another drive, a copy,
the original taken away once all of it is copied. Buffers visiting a file
moved follow it. Names already there are replaced only when you say so, the
old going to the trash. \\<rimu-mode>\\[rimu-rename] renames one in its place."
  (rimu-place :move))

(define-command rimu-symlink ()
  "Make symbolic links to the marked entries, or the one on the line, in a
place asked for, as \\<rimu-mode>\\[rimu-copy] asks: the directory of the other list beside
this one offered first; one entry's link may be given a new name. Each link
leads to its entry's whole name. Names already there are replaced only when
you say so, the old going to the trash."
  (rimu-place :symlink))

(define-command rimu-hardlink ()
  "Give the marked files, or the one on the line, a second name in a place
asked for, as \\<rimu-mode>\\[rimu-copy] asks: one file, two names, on the same drive
only, and no directory. Names already there are replaced only when you say
so, the old going to the trash."
  (rimu-place :hardlink))

(define-command rimu-archive ()
  "Pack the marked entries, from every directory they are marked in, or the
one on the line, into one tar archive asked for, navigating to it as
\\[find-file] does: the directory of the other list beside this one offered
first. Its name says how it is compressed: .tar.gz or .tgz with gzip, .tar.bz2
with bzip2, .tar.xz with xz, .tar.zst with zstd, .tar not at all; a directory
given, it is named after what it holds, .tar.gz. Directories go in with all
in them, links as links; inside, each keeps its path below the deepest
directory all of them are in. tar does it in the background, shut in the
sandbox, the archive given its name only once whole. One there already is
replaced only when you say so, the old going to the trash."
  (let* ((buffer (current-buffer))
         (paths (rimu-chosen-paths buffer))
         (here (tuatara.buffer:buffer-local buffer 'rimu-directory)))
    (if (null paths)
        (message "~a" (rimu-nothing-words buffer "archive"))
        (let* ((name (format nil "~a.tar.gz"
                             (tuatara.io:native-file-name
                              (if (rest paths) (string-right-trim "/" (paths-common-directory paths)) (first paths)))))
               (target (read-native-path (format nil "~a " (rimu-question "Archive" paths :here here :after "as:"))
                                         :initial (abbreviated-path (or (rimu-other-directory buffer) here))
                                         :kind :any :writing t))
               (target (if (and target (eq :directory (tuatara.io:path-kind target)))
                           (tuatara.io:path-join target (if (string= name ".tar.gz") "archive.tar.gz" name))
                           target)))
          (cond ((null target) (message "Nowhere given: nothing archived"))
                ((null (archive-kind-of-name target))
                 (message "Name the archive .tar, .tar.gz, .tar.bz2, .tar.xz or .tar.zst: nothing archived"))
                ((some (lambda (path) (tuatara.io:path-within-p target path)) paths)
                 (message "~a would be inside what it holds: nothing archived" (abbreviated-path target)))
                (t (let ((replace (and (tuatara.io:path-stamp target :follow nil)
                                       (yes-or-no-p* (format nil "~a is there already: replace, the old to the trash?"
                                                             (abbreviated-path target))))))
                     (if (and (tuatara.io:path-stamp target :follow nil) (not replace))
                         (message "Nothing archived: ~a is there already" (abbreviated-path target))
                         (rimu-start-operation :archive (mapcar (lambda (path) (cons path target)) paths) buffer
                                               :replace replace)))))))))

(define-command rimu-diff ()
  "Compare the two entries marked, from every directory they are marked in,
the one shown first taken as the old, and show the differences in a buffer
of their own, which \\`q' closes, as \\[diff-files] does: unified, the old's
lines taken out after -, the other's put in after +; two directories with
all in them. With one marked, or none, that one, or the one on the line, is
compared with another asked for, navigating to it as \\[find-file] does: the
directory of the other list beside this one offered first. diff does it in
the background, shut in the sandbox."
  (let* ((buffer (current-buffer))
         (paths (rimu-chosen-paths buffer))
         (here (tuatara.buffer:buffer-local buffer 'rimu-directory)))
    (cond ((null paths) (message "~a" (rimu-nothing-words buffer "compare")))
          ((rest (rest paths))
           (message "~d entries are marked: mark two to compare them, or one to compare with another" (length paths)))
          ((rest paths) (compare-paths (first paths) (second paths)))
          (t (let ((other (read-native-path (format nil "~a " (rimu-question "Compare" paths :here here :after "with:"))
                                            :initial (abbreviated-path (or (rimu-other-directory buffer) here))
                                            :kind :any)))
               (if other
                   (compare-paths (first paths) other)
                   (message "Nothing to compare with")))))))

(define-command rimu-extract ()
  "Extract the tar archives marked, from every directory they are marked in,
or the one on the line, into a directory asked for, navigating to it as
\\[find-file] does, made when it is not there: for one archive, a directory of
its name beside it offered first. gzip, bzip2, xz and zstd archives are told
by what they hold, whatever their names. The files extracted are yours, none
written outside that directory whatever names and links an archive holds, and
those of a name there already left as they are. tar does it in the
background, shut in the sandbox."
  (let* ((buffer (current-buffer))
         (paths (or (rimu-chosen-paths buffer :files-only t)
                    (error "~a" (rimu-nothing-words buffer "extract" :files-only t
                                                    :none "No file here: mark archives, or put the cursor on one"))))
         (here (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (offered (if (rest paths)
                      here
                      (tuatara.io:slash-ended (tuatara.io:path-join (tuatara.io:parent-path (first paths))
                                                                    (archive-stem (first paths))))))
         (target (read-native-path (format nil "~a " (rimu-question "Extract" paths :here here :after "into:"))
                                   :initial (abbreviated-path offered) :writing t)))
    (cond ((null target) (message "Nowhere given: nothing extracted"))
          ((and (tuatara.io:path-kind target :follow nil) (not (eq :directory (tuatara.io:path-kind target))))
           (message "~a is no directory: nothing extracted" (abbreviated-path target)))
          (t (rimu-start-operation :extract (mapcar (lambda (path) (cons path (tuatara.io:slash-ended target))) paths)
                                   buffer)))))

(defun plain-file-name-p (name)
  "True when NAME can be the name of a file in a directory as it is: not
empty, not . nor .., with no / and no control character in it."
  (and (plusp (length name))
       (not (member name '("." "..") :test #'string=))
       (notany (lambda (c) (or (char= c #\/) (< (char-code c) 32) (= (char-code c) 127))) name)))

(define-command rimu-rename ()
  "Rename the entry on the line, or the one marked, in its place, the new name
asked for, the old offered: a name only, no directory; a name there already
is refused. Buffers visiting it follow it. \\<rimu-mode>\\[rimu-edit-names] renames many as text,
\\[rimu-move] moves into another directory."
  (let* ((buffer (current-buffer))
         (paths (rimu-chosen-paths buffer)))
    (cond ((null paths) (message "~a" (rimu-nothing-words buffer "rename")))
          ((rest paths) (message "~d marked: \\<rimu-mode>\\[rimu-rename] renames one; \\[rimu-edit-names] renames many as text, \\[rimu-move] moves them" (length paths)))
          (t
           (let* ((from (first paths))
                  (old (tuatara.io:native-file-name from))
                  (new (read-string (format nil "~a " (rimu-question "Rename" paths :here (tuatara.buffer:buffer-local buffer 'rimu-directory)
                                                                                   :after "to:"))
                                    :initial old))
                  (to (tuatara.io:path-join (tuatara.io:parent-path from) new)))
             (cond ((or (string= new "") (string= new old)) (message "Not renamed"))
                   ((not (plain-file-name-p new))
                    (message "~a is no name: a name only, no directory nor control character (\\<rimu-mode>\\[rimu-move] moves)"
                             (escape-controls new)))
                   ((tuatara.io:path-stamp to :follow nil) (message "~a is there already" new))
                   (t (handler-case
                          (progn
                            (rename-path-following from to)
                            (remhash (tuatara.io:native from) (rimu-marks buffer))
                            (rimu-update buffer :reread t :point-on (tuatara.text:string-octets new))
                            (message "Renamed to ~a" new))
                        (error (e) (message "Not renamed: ~a" e))))))))))

(defun rimu-delete-paths (paths buffer &key for-good)
  "Put PATHS, of the rimu list BUFFER, in the trash, or with FOR-GOOD delete
them, asking first; those that cannot go to the trash are deleted only after a
second, plain yes."
  (let ((here (and (live-buffer-p buffer) (tuatara.buffer:buffer-local buffer 'rimu-directory))))
    (cond ((or for-good (not *rimu-delete-to-trash*))
           (when (and (yes-or-no-p* (format nil "~a for good?" (rimu-question "Delete" paths :here here)))
                      (yes-or-no-p* (format nil "~a for good? It cannot be put back." (rimu-question "Really delete" paths :here here))))
             (rimu-start-operation :delete (mapcar #'list paths) buffer)))
          ((yes-or-no-p* (format nil "~a?" (rimu-question "Move" paths :here here :after "to the trash")))
           ;; whether a trash can be had is the same for every file of one directory
           (let ((trash-p (make-hash-table :test 'equal)) (to-trash '()) (no-trash '()))
             (dolist (path paths)
               (let ((directory (tuatara.io:parent-path path)))
                 (multiple-value-bind (known found) (gethash directory trash-p)
                   (if (if found known (setf (gethash directory trash-p) (and (ignore-errors (tuatara.io:trash-for path)) t)))
                       (push path to-trash)
                       (push path no-trash)))))
             (setf to-trash (nreverse to-trash) no-trash (nreverse no-trash))
             (when to-trash
               (rimu-start-operation :trash (mapcar #'list to-trash) buffer))
             (when (and no-trash
                        (yes-or-no-p* (format nil "~a cannot go to a trash (none can be made on its drive): delete for good?"
                                              (rimu-question nil no-trash))))
               (rimu-start-operation :delete (mapcar #'list no-trash) buffer)))))))

(defmethod delete-list-entries ((mode rimu-mode) list mark)
  "Move the entries marked for deletion (D), or the marked ones (>) or with
none marked the one on the line, to the trash the desktop's file managers use,
after asking, so that they can be put back; on another drive, that drive's
own trash; those marked in other directories too. \\<rimu-mode>\\[list-delete-marked] with
\\[universal-argument] deletes them for good, asked twice; *RIMU-DELETE-TO-TRASH* NIL
always does."
  (let* ((deletion (eq mark :deletion))
         (paths (rimu-chosen-paths list :mark (if deletion #\D #\>))))
    (cond (paths (rimu-delete-paths paths list :for-good (and (not deletion) (prefix-argument))))
          (deletion (message "~a" (rimu-nothing-words list "delete" :mark #\D
                                                      :none (substitute-command-keys "Nothing marked for deletion (\\<rimu-mode>\\[list-mark-for-deletion] marks one)"))))
          (t (message "~a" (rimu-nothing-words list "delete"))))))

(define-command rimu-make-directory ()
  "Make a directory, asked for, in this list's directory unless given
elsewhere, with those above it that are not there; the list shows it."
  (let* ((buffer (current-buffer))
         (here (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (native (read-native-path "Make directory: " :initial (abbreviated-path here) :writing t))
         (path (and native (string-right-trim "/" native))))
    (cond ((or (null path) (string= (concatenate 'string path "/") here)) (message "No directory named"))
          ((tuatara.io:path-stamp path :follow nil) (message "~a is there already" (abbreviated-path path)))
          (t (handler-case
                 (progn
                   ;; those above it too, where they are not there
                   (tuatara.io:ensure-directories (tuatara.io:slash-ended path))
                   (rimu-refresh-lists (list path))
                   (message "Made ~a" (abbreviated-path path)))
               (error (e) (message "~a" e)))))))

;;; Names edited as text

(define-major-mode rimu-names-mode fundamental-mode
  "The names of a directory's entries as text, a line each, to rename them
by editing (\\<rimu-mode>\\[rimu-edit-names] in a list): \\<rimu-names-mode>\\[rimu-names-apply] renames those whose line
changed, \\[rimu-names-abort] leaves them as they are. Each line stands for its entry: lines
are not added, taken away or joined; an empty line leaves its entry's name."
  :lighter "Rename" :interactive nil)

(defvar *rimu-names-keymap* (mode-keymap 'rimu-names-mode)
  "Keys of a list's names edited as text (rimu-names-mode's keymap).")

(defun rimu-name-editable-p (entry)
  "True when ENTRY's name can be edited as a line of text: it holds no control
or invisible character (ESCAPE-CONTROLS leaves it as it is) and is written as
its octets are."
  (let ((name (tuatara.io:listed-name entry)))
    (and (eq (escape-controls name) name)
         (equalp (tuatara.text:string-octets name) (tuatara.io:listed-octets entry)))))

(define-command rimu-edit-names ()
  "Edit the names of this list's entries as text, a line each, in its window,
with every key that edits text: \\<rimu-names-mode>\\[rimu-names-apply] renames those whose line changed,
\\[rimu-names-abort] leaves them. Names may be swapped; a name given twice, or one
another file has, is refused. Each line stays its own entry's: when a line is
taken away, added or joined to another, nothing is renamed. A name with a
control or invisible character in it (a line break, a tab) is left out, the
header line saying how many, for \\<rimu-mode>\\[rimu-rename] to rename. Buffers visiting a file
renamed follow it. Not in a list of files found, whose names are paths.
While search filters narrow the list, only the names it shows are edited."
  (let* ((list (current-buffer))
         (here (rimu-entry-at list (tuatara.buffer:buffer-point list)))
         (all (remove-if-not #'rimu-markable-p (rimu-shown-entries list)))
         (entries (remove-if-not #'rimu-name-editable-p all))
         (left-out (- (length all) (length entries))))
    (cond
      ((rimu-found-p list)
       (message "Names are edited in a directory's own list; R renames the files found"))
      ((null all)
       (message "No names to edit here"))
      ((null entries)
       (message "No names to edit here: ~d with a control or invisible character in it (\\<rimu-mode>\\[rimu-rename] renames one)" left-out))
      (t
        (let ((buffer (tuatara.buffer:make-buffer
                       (tuatara.buffer:unique-buffer-name (format nil "*Rename: ~a*" (tuatara.io:native-file-name (string-right-trim "/" (tuatara.buffer:buffer-local list 'rimu-directory)))))
                       :text (format nil "~{~a~%~}" (mapcar #'tuatara.io:listed-name entries)))))
          (set-major-mode buffer 'rimu-names-mode)
          (setf (tuatara.buffer:buffer-local buffer 'rimu-names-list) list
                (tuatara.buffer:buffer-local buffer 'rimu-names-entries) (coerce entries 'vector)
                ;; each entry's line followed through the edits, to know it is still its own
                (tuatara.buffer:buffer-local buffer 'rimu-names-starts)
                (let ((version (tuatara.buffer:buffer-version buffer)))
                  (coerce (loop for line below (length entries)
                                collect (tuatara.text:make-anchor version (tuatara.buffer:buffer-goto-line buffer line)))
                          'vector))
                (tuatara.buffer:buffer-local buffer :directory) (tuatara.buffer:buffer-local list 'rimu-directory)
                (tuatara.buffer:buffer-local buffer :header-line)
                (let ((text (substitute-command-keys
                             (format nil " Rename: \\<rimu-names-mode>\\[rimu-names-apply] renames the lines changed, \\[rimu-names-abort] leaves them~[~:;; ~:*~d name~:p with a control or invisible character left out (\\<rimu-mode>\\[rimu-rename] renames one)~]"
                                     left-out))))
                  (lambda (window) (declare (ignore window)) text)))
          (let ((line (or (position here entries) 0)))
            (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-goto-line buffer line)))
          (tuatara.buffer:buffer-mark-saved buffer)
          (show-buffer buffer))))))

(defun rimu-names-paired-p (buffer lines)
  "True when LINES, those of BUFFER, a list's names edited as text, are still
each its entry's: as many as the entries, each entry's line where its start
has been followed to, none taken away, added or joined to another."
  (let ((starts (tuatara.buffer:buffer-local buffer 'rimu-names-starts))
        (version (tuatara.buffer:buffer-version buffer)))
    (and (= (length lines) (length starts))
         (loop for anchor across starts
               for before = -1 then at
               for at = (tuatara.text:anchor-position anchor version)
               always (and (> at before) (= at (tuatara.buffer:buffer-line-start buffer at)))))))

(defun rimu-names-lines (buffer)
  "The lines of BUFFER, a list's names edited as text, the empty one after the last newline left out."
  (let ((lines (uiop:split-string (tuatara.buffer:buffer-string buffer) :separator '(#\Newline))))
    (if (and lines (string= "" (car (last lines)))) (butlast lines) lines)))

(defun rimu-names-back (buffer)
  "Leave BUFFER, a list's names edited as text, for its list: shown in its place, BUFFER killed."
  (let ((list (tuatara.buffer:buffer-local buffer 'rimu-names-list)))
    (when (live-buffer-p list) (show-buffer list))
    (tuatara.buffer:kill-buffer buffer)
    list))

(define-command rimu-names-abort ()
  "Leave the names as they are, and go back to the list."
  (rimu-names-back (current-buffer))
  (message "No name changed"))

(defun rimu-names-refusal (directory names finals changes)
  "Why the files NAMES of DIRECTORY cannot be given the names FINALS, each
CHANGES, (OLD . NEW), one changed: a name given twice, one that is no name,
or one another file there has that is not renamed away; NIL when they can.
Time grows with the number of names alone."
  (let ((given (make-hash-table :test 'equal :size (max 16 (length finals))))
        (old (make-hash-table :test 'equal :size (max 16 (length names)))))
    (dolist (name names) (setf (gethash name old) t))
    (dolist (name finals)
      (when (gethash name given)
        (return-from rimu-names-refusal (format nil "~a is given twice" name)))
      (setf (gethash name given) t))
    (loop for (nil . new) in changes
          do (cond ((not (plain-file-name-p new))
                    (return (format nil "~a is no name in this directory" (escape-controls new))))
                   ((and (not (gethash new old))
                         (tuatara.io:path-stamp (tuatara.io:path-join directory new) :follow nil))
                    (return (format nil "~a is there already" new)))))))

(define-command rimu-names-apply ()
  "Rename the entries whose line changed, and go back to the list: all or,
when a line does not do, none, and why is said."
  (let* ((buffer (current-buffer))
         (list (tuatara.buffer:buffer-local buffer 'rimu-names-list))
         (entries (tuatara.buffer:buffer-local buffer 'rimu-names-entries))
         ;; the directory the names were taken from, wherever the list has gone since
         (directory (tuatara.buffer:buffer-local buffer :directory))
         (lines (rimu-names-lines buffer)))
    (block apply
      (flet ((refuse (control &rest arguments)
               (message "Nothing renamed: ~?" control arguments)
               (return-from apply nil)))
        (unless (rimu-names-paired-p buffer lines)
          (refuse (substitute-command-keys "a line was added, taken away or joined (~d line~:p for ~d name~:p); \\<rimu-names-mode>\\[rimu-names-abort], then \\<rimu-mode>\\[rimu-edit-names] starts again")
                  (length lines) (length entries)))
        (let* ((names (map 'list #'tuatara.io:listed-name entries))
               (finals (loop for line in lines for name in names collect (if (string= line "") name line)))
               (changes (loop for old in names for new in finals unless (string= old new) collect (cons old new))))
          (when (null changes)
            (rimu-names-back buffer)
            (message "No name changed")
            (return-from apply nil))
          (let ((why (rimu-names-refusal directory names finals changes)))
            (when why (refuse "~a" why)))
          (handler-case (check-not-saving (mapcar (lambda (change) (tuatara.io:path-join directory (car change))) changes))
            (error (e) (refuse "~a" e)))
          ;; two steps, so that names can be swapped: each to a name of its own, then to its new name
          (let* ((temporary (loop for change in changes
                                  for n from 0
                                  collect (tuatara.io:path-join directory (format nil ".tuatara-rename-~d-~d" (sb-posix:getpid) n))))
                 (done '()) (failed '()) (moved '()))
            ;; the first step: one that fails puts back those gone before it, and nothing is renamed
            (loop for (old . nil) in changes
                  for temp in temporary
                  do (handler-case (progn (tuatara.io:rename-path (tuatara.io:path-join directory old) temp)
                                          (push (cons temp old) moved))
                       (error (e)
                         (let ((stuck (loop for (temp . old) in moved
                                            unless (ignore-errors (tuatara.io:rename-path temp (tuatara.io:path-join directory old)) t)
                                              collect (format nil "~a is left as ~a" old (tuatara.io:native-file-name temp)))))
                           (refuse "~a: ~a~@[; ~{~a~^; ~}~]" old e stuck)))))
            (loop for (old . new) in changes
                  for temp in temporary
                  do (handler-case (progn (tuatara.io:rename-path temp (tuatara.io:path-join directory new))
                                          (push (cons (tuatara.io:path-join directory old) (tuatara.io:path-join directory new)) done))
                       (error (e)
                         (push (if (ignore-errors (tuatara.io:rename-path temp (tuatara.io:path-join directory old)) t)
                                   (format nil "~a: ~a" old e)
                                   ;; its old name taken meanwhile by another's new one: where it is, said
                                   (format nil "~a: ~a; it is left as ~a" old e (tuatara.io:native-file-name temp)))
                               failed))))
            (follow-moved-paths done)
            ;; marks go with the files renamed
            (when (live-buffer-p list)
              (let* ((marks (rimu-marks list))
                     (moving (loop for (from . to) in done
                                   for key = (tuatara.io:native from)
                                   for mark = (gethash key marks)
                                   do (remhash key marks)
                                   when mark collect (cons (tuatara.io:native to) mark))))
                (loop for (key . mark) in moving do (setf (gethash key marks) mark))))
            (let ((list (rimu-names-back buffer)))
              (when (live-buffer-p list)
                (rimu-update list :reread t :point-on (and done (tuatara.text:string-octets (tuatara.io:native-file-name (cdr (first done))))))))
            (message "Renamed ~d~@[; not renamed: ~{~a~^; ~}~]" (length done) (reverse failed))))))))

;;; New files

(define-command rimu-touch ()
  "Touch a name asked for in this list's directory, as touch does: a name not
there is made a new, empty file, the list on it for \\<rimu-mode>\\[rimu-open] to
open it; a file or directory there has its times set to now (a link's own,
not those of where it leads). \\`RET' alone touches the marked entries, or the
one on the line. A name with a / or a control character in it is refused."
  (let* ((buffer (current-buffer))
         (here (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (chosen (rimu-chosen-paths buffer))
         (name (read-string (format nil "Touch in ~a~@[ (RET: ~a)~]: " (abbreviated-path here)
                                    (and chosen (rimu-question nil chosen)))))
         (path (tuatara.io:path-join here name)))
    (flet ((touch (paths)
             (handler-case
                 (progn
                   (dolist (path paths) (tuatara.io:touch-path path))
                   (rimu-refresh-lists paths)
                   (message "Touched ~:[~a~;~*~d entries~]" (rest paths)
                            (tuatara.io:native-file-name (string-right-trim "/" (first paths))) (length paths)))
               (error (e) (message "~a" e)))))
      (cond ((and (string= name "") chosen) (touch chosen))
            ((string= name "") (message "No file named"))
            ((not (plain-file-name-p name))
             (message "~a is no name of a file in ~a" (escape-controls name) (abbreviated-path here)))
            ((tuatara.io:path-stamp path :follow nil) (touch (list path)))
            (t (handler-case
                   (progn
                     (tuatara.io:write-new-file path "" :mode #o666)
                     (rimu-show-made buffer path))
                 (error (e) (message "~a" e))))))))

(defun rimu-show-made (list path)
  "Show PATH, made just now, in LIST when it is in LIST's directory, point on
it; the other lists of its directory read again."
  (rimu-refresh-lists (list path))
  (when (string= (tuatara.io:parent-path path) (tuatara.buffer:buffer-local list 'rimu-directory))
    (rimu-update list :reread t :point-on (tuatara.text:string-octets (tuatara.io:native-file-name path))))
  (message "Made ~a; \\<rimu-mode>\\[rimu-open] opens it" (abbreviated-path path)))

(define-command rimu-make-formatted-file ()
  "Make a new file with a formatted name in this list's directory, asking
what new-file-formatted asks but the subdirectory (the title, the keywords,
the kind and the template), with the header of its kind, saved at once; the
list shows it, point on it."
  (let* ((list (current-buffer))
         (directory (tuatara.buffer:buffer-local list 'rimu-directory)))
    (multiple-value-bind (asked title keywords type template) (ask-formatted-file-answers :skip '(:subdirectory))
      (declare (ignore asked))
      (handler-case
          (let ((made (make-formatted-file directory title keywords (second type) :template template :write t)))
            (cond ((stringp made) (rimu-show-made list made))
                  (made (message "~a is made when saved, encrypted: ~a"
                                 (tuatara.buffer:buffer-name made) (substitute-command-keys "\\[save-buffer]")))))
        (error (e) (message "~a" e))))))

(define-command rimu-wipe-files ()
  "Wipe the files marked (or the one on the line) as \\[wipe-file] does, asked once: overwritten, then deleted."
  (let* ((buffer (current-buffer))
         (paths (or (rimu-chosen-paths buffer :files-only t)
                    (error "~a" (rimu-nothing-words buffer "wipe" :files-only t :none "No file here: mark files, or put the cursor on one")))))
    (when (confirm-wipe paths (rimu-question nil paths :here (tuatara.buffer:buffer-local buffer 'rimu-directory)))
      (start-wiping paths))))

(defun install-rimu-operations ()
  "Bind the keys of the marks and operations of rimu's lists."
  (define-keys *rimu-keymap* '(("m" list-mark-entry) ("u" list-unmark-entry) ("U" list-unmark-all) ("t" list-toggle-marks)
                               ("d" list-mark-for-deletion) ("x" list-execute) ("D" list-delete-marked)
                               ("C" rimu-copy) ("R" rimu-rename) ("M" rimu-move) ("Y" rimu-symlink) ("L" rimu-hardlink)
                               ("c" rimu-archive) ("Z" rimu-extract) ("C-c e" list-archive) ("=" rimu-diff)
                               ("+" rimu-make-directory) ("K" rimu-take-out-lines)
                               ("a" rimu-touch) ("A" rimu-make-formatted-file) ("C-x C-q" rimu-edit-names)
                               ("g t" rimu-trash) ("g w" rimu-wipe-files) ("C-c f m" rimu-mark-master)))
  (commands-for-mode 'rimu-mode "a rimu list (\\[rimu])"
                     'list-mark-entry 'list-mark-for-deletion 'list-unmark-entry 'list-unmark-all 'list-toggle-marks
                     'list-execute 'list-delete-marked 'rimu-mark-master)
  (define-keys *rimu-names-keymap* '(("C-c C-c" rimu-names-apply) ("C-c C-k" rimu-names-abort)))
  (install-rimu-trash))
