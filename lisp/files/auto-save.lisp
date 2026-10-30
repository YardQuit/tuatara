;;;; lisp/files/auto-save.lisp --- Changes not saved kept in a copy of their own, and taken back with M-x recover-file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A buffer visiting a file, with changes not saved, is written now and then
;;;; to a copy of its own in *AUTO-SAVE-DIRECTORY* (a folder only you can read,
;;;; in Tuatara's state folder as standard): after *AUTO-SAVE-IDLE* seconds
;;;; without a change, and at least every *AUTO-SAVE-INTERVAL* seconds while
;;;; the changes go on. The file itself is never written but by a save. The
;;;; copy is written on a thread of its own from the text as it was, so that
;;;; typing never waits. A save, or killing the buffer with nothing to save,
;;;; deletes the copy; quitting without saving keeps it, and visiting the file
;;;; again says that it holds changes newer than the file, which
;;;; M-x recover-file takes into the buffer as one edit.
;;;;
;;;; Nothing opened safely is copied (a file from evidence, or in
;;;; *SAFE-DIRECTORIES*), nor a buffer that cannot change, nor one larger than
;;;; *AUTO-SAVE-LARGEST*, nor the made-up files of /proc. The copies belong to
;;;; the files, not to windows or frames.

(in-package #:tuatara.defaults)

(defvar *auto-save* t
  "True for buffers visiting files to have their changes not saved written now
and then to a copy of their own in *AUTO-SAVE-DIRECTORY*, never to the file
itself; M-x recover-file takes such a copy back. NIL writes none.")

(defvar *auto-save-directory* nil
  "The folder auto-saved copies are kept in, made for you alone when it is not
there: NIL for auto-save/ in Tuatara's state folder ($XDG_STATE_HOME/tuatara/,
or ~/.local/state/tuatara/), or a folder's name, \"~/backups/tuatara/\".")

(defvar *auto-save-idle* 5
  "Seconds without a change after which a buffer's changes not saved are
auto-saved (*AUTO-SAVE*).")

(defvar *auto-save-interval* 60
  "The most seconds changes not saved go without an auto-save while they go
on without a pause (*AUTO-SAVE*).")

(defvar *auto-save-largest* (* 256 1024 1024)
  "The largest buffer auto-saved, in octets: a larger one is not copied (each
copy would be written whole).")

(defun auto-save-directory ()
  "The folder auto-saved copies go in, as *AUTO-SAVE-DIRECTORY* says; a native
name ending in /. It is made, for you alone, when a copy is first written."
  (tuatara.io:slash-ended
   (if *auto-save-directory*
       (tuatara.io:native (tuatara.io:expand-path (string-right-trim "/" *auto-save-directory*)))
       (tuatara.io:path-join (tuatara.io:native (tuatara.io:state-directory)) "auto-save"))))

(defun auto-save-path (file)
  "The auto-saved copy of FILE, a native name: its whole name written as one
name in the folder (AUTO-SAVE-DIRECTORY, TUATARA.IO:PATH-FILE-NAME), or, for a
name too long for that, its start and a number made from all of it, its own
name then kept beside it in NAME.path."
  (concatenate 'string (auto-save-directory) (tuatara.io:path-file-name file)))

(defun auto-save-wanted-p (buffer)
  "True when BUFFER's changes not saved may be auto-saved: it visits a file, was
not opened safely, is not an encrypted file's text, can change, is no larger than *AUTO-SAVE-LARGEST*, and is
not one of the system's made-up files."
  (and *auto-save*
       (tuatara.buffer:buffer-file buffer)
       (not (tuatara.io:safe-visit-p buffer))
       (not (tuatara.buffer:buffer-local buffer :plaintext))
       (not (tuatara.buffer:buffer-local buffer :read-only))
       (not (eq (tuatara.buffer:buffer-local buffer :file-class) :pseudo))
       (<= (tuatara.buffer:buffer-size buffer) *auto-save-largest*)))

(defun delete-auto-save-of (file)
  "Delete the auto-saved copy of the native FILE, and its name kept beside it,
when there is one."
  (let ((copy (auto-save-path file)))
    (when (and copy (tuatara.io:path-stamp copy))
      (ignore-errors (tuatara.io:remove-file copy))
      (ignore-errors (tuatara.io:remove-file (concatenate 'string copy ".path")))
      (ignore-errors (refill-open-list "*Auto-saves*" 'auto-saves-mode)))))

(defun delete-auto-save (buffer)
  "Delete BUFFER's auto-saved copy, and its name kept beside it, when there is
one, and the copy made under the name its file had before, when it was
renamed or written as another file since."
  (let ((file (tuatara.buffer:buffer-file buffer))
        (before (tuatara.buffer:buffer-local buffer 'auto-save-file)))
    (when before (delete-auto-save-of before))
    (when (and file (not (equal before (tuatara.io:native file))))
      (delete-auto-save-of (tuatara.io:native file))))
  (setf (tuatara.buffer:buffer-local buffer 'auto-save-written) nil
        (tuatara.buffer:buffer-local buffer 'auto-save-seen) nil
        (tuatara.buffer:buffer-local buffer 'auto-save-file) nil))

(defun auto-save-follow-file (buffer)
  "When BUFFER's file has been given another name since its changes were
auto-saved, delete the copy made under the old name, for its changes to be
auto-saved under the new one."
  (let ((before (tuatara.buffer:buffer-local buffer 'auto-save-file))
        (file (tuatara.buffer:buffer-file buffer)))
    (when (and before (not (and file (equal before (tuatara.io:native file))))
               (not (tuatara.buffer:buffer-local buffer 'auto-save-job)))
      (delete-auto-save-of before)
      (setf (tuatara.buffer:buffer-local buffer 'auto-save-written) nil
            (tuatara.buffer:buffer-local buffer 'auto-save-file) nil))))

(defstruct (auto-save-job (:include tuatara.runtime:job) (:constructor make-auto-save-job (buffer content)) (:copier nil))
  "BUFFER's text, CONTENT the number of the text it is, written to its
auto-saved copy in the background, a job."
  content)

(setf (documentation 'auto-save-job-p 'function) "True when OBJECT is an AUTO-SAVE-JOB.")

(tuatara.runtime:define-job-words auto-save-job
  :doing "Auto-saving"
  :left-words "the auto-saved copy as it was"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-holds-queue-p ((job auto-save-job))
  "False: other work on the buffer need not wait for its auto-save."
  nil)

(defmethod tuatara.runtime:job-shows-processing-p ((job auto-save-job))
  "False: an auto-save is not shown as work under way in the mode line."
  nil)

(defun start-auto-save (buffer)
  "Write BUFFER's text as it is now to its auto-saved copy, a job in the
background."
  (let* ((rope (tuatara.text:version-rope (tuatara.buffer:buffer-version buffer)))
         (file (tuatara.io:native (tuatara.buffer:buffer-file buffer)))
         (copy (progn (tuatara.io:ensure-private-directory (auto-save-directory)) (auto-save-path file)))
         (job (make-auto-save-job buffer (tuatara.buffer:buffer-content-number buffer))))
    (setf (tuatara.buffer:buffer-local buffer 'auto-save-job) job
          (tuatara.buffer:buffer-local buffer 'auto-save-seen) nil
          (tuatara.buffer:buffer-local buffer 'auto-save-file) file)
    (tuatara.runtime:start-job-thread
     job
     (lambda (job)
       (declare (ignore job))
       (tuatara.io:write-text-file rope copy :expected :any :follow-links nil :mode (constantly #o600))
       (when (nth-value 1 (tuatara.io:path-file-name file))
         (tuatara.io:write-private-file (concatenate 'string copy ".path") file)))
     :name "tuatara auto-save")))

(defmethod tuatara.runtime:job-take-in ((job auto-save-job))
  "Note the copy written, or say why it was not; a buffer saved meanwhile has
its copy deleted, the file holding all it has."
  (let ((buffer (auto-save-job-buffer job)))
    (when (and (live-buffer-p buffer) (eq (tuatara.buffer:buffer-local buffer 'auto-save-job) job))
      (setf (tuatara.buffer:buffer-local buffer 'auto-save-job) nil)
      (case (auto-save-job-status job)
        (:done (setf (tuatara.buffer:buffer-local buffer 'auto-save-written) (auto-save-job-content job))
               (refill-open-list "*Auto-saves*" 'auto-saves-mode))
        (:failed (message "Auto-saving ~a failed: ~a" (tuatara.buffer:buffer-name buffer)
                          (tuatara.runtime:job-ending-words job))))
      (unless (tuatara.buffer:buffer-modified-p buffer) (delete-auto-save buffer)))))

(defun auto-save-tick (editor)
  "Auto-save the buffers whose changes not saved are due (*AUTO-SAVE-IDLE*,
*AUTO-SAVE-INTERVAL*), each a job (START-AUTO-SAVE); the seconds until the
next is due, T while one is written, or NIL."
  (declare (ignore editor))
  (let ((now (/ (get-internal-real-time) internal-time-units-per-second))
        (soonest nil) (writing nil))
    (dolist (buffer tuatara.buffer:*buffers*)
      (auto-save-follow-file buffer)
      (cond ((tuatara.buffer:buffer-local buffer 'auto-save-job) (setf writing t))
            ((and (tuatara.buffer:buffer-modified-p buffer) (auto-save-wanted-p buffer))
             (let ((content (tuatara.buffer:buffer-content-number buffer))
                   (seen (tuatara.buffer:buffer-local buffer 'auto-save-seen)))
               (unless (eql content (tuatara.buffer:buffer-local buffer 'auto-save-written))
                 ;; SEEN: the text's number, when it changed last, and when the first change not yet copied was made
                 (unless (and seen (eql (first seen) content))
                   (setf seen (list content now (if seen (third seen) now))
                         (tuatara.buffer:buffer-local buffer 'auto-save-seen) seen))
                 (let ((due (min (+ (second seen) *auto-save-idle*) (+ (third seen) *auto-save-interval*))))
                   (if (>= now due)
                       (progn (start-auto-save buffer) (setf writing t))
                       (setf soonest (if soonest (min soonest (- due now)) (- due now))))))))))
    (or writing soonest)))

(defun auto-save-newer-p (buffer)
  "The auto-saved copy of BUFFER's file when there is one changed later than
the file, with when it was written (a universal time) as a second value; NIL
otherwise."
  (let* ((file (tuatara.buffer:buffer-file buffer))
         (copy (and file (auto-save-path (tuatara.io:native file))))
         (copy-stamp (and copy (tuatara.io:path-stamp copy)))
         (file-stamp (and file (tuatara.io:path-stamp (tuatara.io:native file)))))
    (when (and copy-stamp (or (null file-stamp) (> (tuatara.io:stamp-mtime copy-stamp) (tuatara.io:stamp-mtime file-stamp))))
      (values copy (mtime-universal-time (tuatara.io:stamp-mtime copy-stamp))))))

(defun auto-save-visited (buffer)
  "Say, when BUFFER's file was just visited, that an auto-saved copy holds changes newer than it."
  (unless (tuatara.io:safe-visit-p buffer)
    (multiple-value-bind (copy time) (auto-save-newer-p buffer)
      (when copy
        (message "~a has changes auto-saved ~a, newer than the file: ~a takes them"
                 (tuatara.buffer:buffer-name buffer) (short-time-words time)
                 (substitute-command-keys "\\[recover-file]"))))))

(define-command recover-file ()
  "Take into the current buffer the changes auto-saved for its file (*AUTO-SAVE*)
after asking, as one edit: \\[undo] takes it back, a save writes it to the file.
For a buffer whose file has no auto-saved copy newer than it, says so."
  (let ((buffer (current-buffer)))
    (cond ((null (tuatara.buffer:buffer-file buffer)) (message "~a visits no file" (tuatara.buffer:buffer-name buffer)))
          ((tuatara.io:safe-visit-p buffer) (message "~a is opened safely: nothing is recovered into it" (tuatara.buffer:buffer-name buffer)))
          ((tuatara.buffer:buffer-local buffer :read-only) (message "~a is read-only: ~a first" (tuatara.buffer:buffer-name buffer)
                                                                    (substitute-command-keys "\\[read-only-mode]")))
          (t (multiple-value-bind (copy time) (auto-save-newer-p buffer)
               (cond ((null copy) (message "~a has no auto-saved changes newer than its file" (tuatara.buffer:buffer-name buffer)))
                     ((yes-or-no-p* (format nil "Recover ~a from its changes auto-saved ~a?" (tuatara.buffer:buffer-name buffer) (short-time-words time)))
                      (let ((octets (tuatara.io:file-octets copy)))
                        (tuatara.buffer:widen-buffer buffer)
                        (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) octets)
                        (setf (tuatara.buffer:buffer-point buffer) 0)
                        (message "~a recovered: ~a saves it, ~a takes it back" (tuatara.buffer:buffer-name buffer)
                                 (substitute-command-keys "\\[save-buffer]") (substitute-command-keys "\\[undo]"))))
                     (t (message "Nothing recovered"))))))))

(defun auto-save-reverted (buffer)
  "Delete the copy BUFFER auto-saved of its changes, about to be thrown away by
reading its file again; a copy it did not write itself is kept. NIL, nothing
to say."
  (when (tuatara.buffer:buffer-local buffer 'auto-save-file)
    (delete-auto-save buffer))
  nil)

(defun auto-save-killed (buffer)
  "Delete the auto-saved copy of BUFFER, being killed, when it has nothing to save."
  (unless (tuatara.buffer:buffer-modified-p buffer)
    (delete-auto-save buffer)))

(defun auto-save-copies ()
  "The auto-saved copies there are, newest first, each a plist: :COPY, its path;
:FILE, the file it is of; :TIME, when it was written (a universal time);
:SIZE, its octets; :STATE, :NEWER (than the file), :OLDER or :GONE (the file
is not there)."
  (let ((directory (auto-save-directory)) (copies '()))
    (loop for (name nil nil own stamp) in (and directory (ignore-errors (tuatara.io:directory-entries directory :stamps t)))
          for copy = (concatenate 'string directory name)
          do (when (and stamp (eq own :regular) (not (uiop:string-suffix-p name ".path")))
          (let* ((file (or (ignore-errors (string-right-trim '(#\Newline) (tuatara.io:file-text (concatenate 'string copy ".path"))))
                           (ignore-errors (tuatara.io:percent-decoded name))))
                 (file-stamp (and file (ignore-errors (tuatara.io:path-stamp file)))))
            (when (and file (plusp (length file)) (char= (char file 0) #\/))
              (push (list :copy copy :file file :time (mtime-universal-time (tuatara.io:stamp-mtime stamp))
                          :size (tuatara.io:stamp-size stamp)
                          :state (cond ((null file-stamp) :gone)
                                       ((> (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-mtime file-stamp)) :newer)
                                       (t :older)))
                    copies)))))
    (sort copies #'> :key (lambda (c) (getf c :time)))))

(define-major-mode auto-saves-mode marked-list-mode
  "The auto-saved copies there are (\\[list-auto-saves]), a line each: its mark,
when written, how large, whether newer than their file, and the file; shown
again as copies are written and deleted. \\<auto-saves-mode>\\[list-visit] opens the file on the
line and offers to take its changes in; \\[list-mark-entry] marks a copy (>), \\[list-mark-for-deletion] marks
it for deletion (D), \\[list-unmark-entry] unmarks it, \\[list-unmark-all] unmarks them all; \\[list-delete-marked]
deletes the marked copies (the one on the line when none is), \\[list-execute] those
marked for deletion, after asking, their files not touched; \\[revert-buffer]
lists them again; \\[quit-window] closes."
  :lighter "Auto-saves" :interactive nil)

(defvar *auto-saves-keymap* (mode-keymap 'auto-saves-mode)
  "Keys of the list of auto-saved copies (auto-saves-mode's keymap).")

(defparameter *auto-saves-columns* '((" " :width 1) ("Written" :width 16) ("Size" :width 9 :right t :elide nil) ("State" :width 19)
                                     ("File"))
  "The columns of the list of auto-saved copies, as TABLE-HEADING-PIECES takes
them: the mark (> or D), when a copy was written, its size, whether it is
newer than its file, and the file.")

(defun auto-saves-show (buffer &optional (point 2))
  "Fill BUFFER, the list of auto-saved copies, point on line POINT, or with
:keep on the line it was on, marks kept on the copies still there; a copy
newer than its file in :warning, the file of a marked one in its mark's face."
  (let ((copies (auto-save-copies)))
    (render-table-list
     buffer *auto-saves-columns* copies
     (lambda (copy mark section)
       (declare (ignore section))
       (let ((face (and (eq (getf copy :state) :newer) :warning)))
         (list (list-mark-sign mark)
               (cons (short-time-words (getf copy :time)) face)
               (cons (octets-words (getf copy :size)) face)
               (cons (case (getf copy :state)
                       (:newer "newer than the file")
                       (:older "older than the file")
                       (t "the file is gone"))
                     face)
               (cons (abbreviated-path (getf copy :file)) (or (list-mark-face mark) face)))))
     :marked-by (lambda (copy section) (declare (ignore section)) (auto-save-copy-key copy))
     :after (list (format nil "~%Auto-saved changes in ~a: ~d~%" (abbreviated-path (auto-save-directory)) (length copies)))
     :point point
     :refill (lambda (buffer) (auto-saves-show buffer :keep)))))

(defun auto-save-copy-key (copy)
  "What a mark on COPY, one of AUTO-SAVE-COPIES, in their list is kept by: the copy's file."
  (getf copy :copy))

(define-command list-auto-saves ()
  "List the auto-saved copies of changes not saved (*AUTO-SAVE-DIRECTORY*), the
newest first: after a crash or a quit without saving, what can be taken back.
There \\<auto-saves-mode>\\[list-visit] opens the file on the line and offers to take its
changes in (\\[recover-file]), \\[list-delete-marked] deletes the marked copies or the one on
the line."
  (let ((buffer (buffer-in-mode "*Auto-saves*" 'auto-saves-mode)))
    (auto-saves-show buffer)
    (show-buffer buffer)))

(defmethod list-entry-noun ((mode auto-saves-mode))
  "An auto-saved copy."
  "auto-save")

(defmethod visit-entry ((mode auto-saves-mode) list copy other)
  "Open the file of COPY and offer to take its changes in (\\[recover-file]); a
file no longer there is opened as a new one."
  (declare (ignore list other))
  (visit-path (getf copy :file))
  (if (auto-save-newer-p (current-buffer))
      (recover-file)
      (message "The copy is older than ~a: nothing to take in" (abbreviated-path (getf copy :file)))))

(defmethod delete-entries ((mode auto-saves-mode) list keys mark)
  "Delete the auto-saved copies, after asking; their files are not touched."
  (declare (ignore mark))
  (let ((copies (list-entries-of list keys)))
    (if (yes-or-no-p* (format nil "~a?" (list-question "Delete the auto-saved changes of" "file" copies :the nil
                                                       :name-of (lambda (copy) (abbreviated-path (getf copy :file))))))
        (progn (dolist (copy copies)
                 (ignore-errors (tuatara.io:remove-file (getf copy :copy)))
                 (ignore-errors (tuatara.io:remove-file (concatenate 'string (getf copy :copy) ".path"))))
               (unmark-list list)
               (message "Deleted ~d auto-saved cop~:@p" (length copies)))
        (message "Kept"))))

(defvar *auto-saves-counting* nil
  "The GATHERING counting in the background, as the editor starts, the files
whose auto-saved changes are newer than them (AUTO-SAVES-STARTUP-NOTICE).")

(defun auto-saves-startup-notice (editor)
  "Tick function: say once, as the editor starts, how many files have
auto-saved changes newer than them, and how to see them
\\(\\[list-auto-saves]); they are counted in the background, so that a file
on a drive not answering cannot hold the start up. Looks again every
twentieth of a second until they are."
  (declare (ignore editor))
  (let ((counting *auto-saves-counting*))
    (cond ((null counting)
           (setf *auto-saves-counting*
                 (start-gathering "tuatara auto-saves counted"
                                  (tuatara.runtime:carry-settings
                                   '(*auto-save-directory*)
                                   (lambda (give cancel)
                                     (declare (ignore cancel))
                                     (funcall give (count :newer (ignore-errors (auto-save-copies))
                                                          :key (lambda (c) (getf c :state))))))))
           1/20)
          ((not (gathering-done counting)) 1/20)
          (t
           (setf *tick-functions* (remove 'auto-saves-startup-notice *tick-functions*)
                 *auto-saves-counting* nil)
           (let ((newer (or (first (take-gathered counting)) 0)))
             (when (plusp newer)
               (message "~d file~:p ~:*~[~;has~:;have~] changes auto-saved and not saved: ~a lists them"
                        newer (substitute-command-keys "\\[list-auto-saves]"))))
           nil))))

(defun install-auto-save ()
  "Have buffers' changes not saved auto-saved (*AUTO-SAVE*), their copies
deleted on a save, a file whose copy is newer said so when visited, and how
many have newer copies said as the editor starts; bind the keys of their list."
  (setf *auto-saves-counting* nil)
  (pushnew 'auto-save-tick *tick-functions*)
  (pushnew 'delete-auto-save *after-save-functions*)
  (pushnew 'auto-save-reverted *before-reload-functions*)
  (pushnew 'auto-save-visited tuatara.buffer:*find-file-functions*)
  (pushnew 'auto-save-killed tuatara.buffer:*kill-buffer-functions*)
  (when *auto-save* (pushnew 'auto-saves-startup-notice *tick-functions*))
  (define-key *auto-saves-keymap* "RET" 'list-visit))

(commands-for-mode 'auto-saves-mode "the list of auto-saved copies (\\[list-auto-saves])" 'list-visit)
