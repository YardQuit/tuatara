;;;; lisp/rimu/trash.lisp --- The trash listed: what is in it put back, or deleted for good.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The trash rimu puts files in is the desktop's (freedesktop.org's): your
;;;; own, and that of each drive mounted that has one for you. Its list shows
;;;; each thing in it, when it went there and where it was, as its .trashinfo
;;;; says; it is read in the background, when the list is made, on r, and when
;;;; rimu has changed the trash while a window shows it. One put back goes
;;;; where it was (another name asked for when that is taken, the directories
;;;; above it made again when gone), one deleted goes for good, in the
;;;; background, and the whole trash can be emptied, asked twice.

(in-package #:tuatara.defaults)

(define-major-mode rimu-trash-mode sectioned-list-mode
  "What is in the trash (\\[rimu-trash]), as the buffer list (\\[buffers]) has its buffers: a
section for your own trash and one for each drive's, a line each thing:
its mark, name, when it went there and where it was. \\<support>\\[next-line] and \\[previous-line]
move, \\<rimu-trash-mode>\\[list-visit] or \\`RET' opens the thing on the line safely, to look at
\(on a heading, folds or unfolds its section), \\[list-visit-other] in another window,
\\[quit-window] or \\`h' returns; \\[list-toggle-fold] folds or unfolds the section, \\[list-toggle-all-folds] all of them;
\\[list-mark-entry] marks a thing (>), \\[list-mark-for-deletion] marks it for deletion (D), \\[list-unmark-entry] unmarks
it, \\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps marked and unmarked; \\[rimu-trash-restore] puts the marked things (the one on the
line when none is) back where they were, after asking, \\[list-delete-marked] deletes them for
good, after asking, \\[list-execute] those marked for deletion; \\[rimu-trash-empty] empties the
trash (asked twice); \\[revert-buffer] lists it again, marks kept."
  :lighter "Trash" :interactive nil)

(defvar *rimu-trash-keymap* (mode-keymap 'rimu-trash-mode)
  "Keys of the list of the trash (rimu-trash-mode's keymap).")

(defstruct (trashed (:constructor make-trashed (trash name top path date folder)) (:copier nil))
  "A thing in the trash: TRASH, the trash directory it is in; NAME, its name
there; TOP, the top of the drive TRASH is on (NIL for your own trash); PATH,
where it was, whole; DATE, when it went there, as its .trashinfo has it;
FOLDER, true when it is a directory."
  trash name top path date folder)

(setf (documentation 'trashed-p 'function) "True when OBJECT is a TRASHED.")

(defun trashed-files-path (thing)
  "Where THING, in the trash, is."
  (tuatara.io:path-join (trashed-trash thing) (format nil "files/~a" (trashed-name thing))))

(defun trashed-info-path (thing)
  "THING's .trashinfo."
  (tuatara.io:path-join (trashed-trash thing) (format nil "info/~a.trashinfo" (trashed-name thing))))

(defun trash-sound-things (things)
  "Those of THINGS whose trash may still be used (TUATARA.IO:DRIVE-TRASH-SOUND-P),
looked at again just before they are changed, each trash once."
  (let ((sound (make-hash-table :test 'equal)))
    (remove-if-not (lambda (thing)
                     (or (null (trashed-top thing))
                         (multiple-value-bind (known found) (gethash (trashed-trash thing) sound)
                           (if found
                               known
                               (setf (gethash (trashed-trash thing) sound)
                                     (tuatara.io:drive-trash-sound-p (trashed-trash thing) (trashed-top thing)))))))
                   things)))

(defun mount-field (raw)
  "RAW, a field of /proc/self/mounts, its \\NNN (a space, a tab, a line feed, a
backslash, in octal) made the characters again."
  (cl-ppcre:regex-replace-all "\\\\([0-7]{3})" raw
                              (lambda (match code) (declare (ignore match)) (string (code-char (parse-integer code :radix 8))))
                              :simple-calls t))

(defparameter *mounts-file* "/proc/self/mounts"
  "The list of the drives mounted, as the system gives it, where the trashes of
the drives are found (TRASH-DIRECTORIES).")

(defun trash-directories ()
  "The trash directories there are: your own, and those for you on the
drives mounted (.Trash/UID or .Trash-UID at their tops), each with the
top the paths in it are counted from (NIL for your own), as a list of
\(DIRECTORY . TOP)."
  (let ((uid (sb-posix:getuid))
        (found (list (cons (tuatara.io:home-trash) nil))))
    (with-open-file (in *mounts-file* :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil)
              while line
              do (let* ((fields (uiop:split-string line :separator " "))
                        (top (and (second fields)
                                  (let ((raw (second fields)))
                                    (concatenate 'string (string-right-trim "/" (mount-field raw)) "/")))))
                   (when top
                     (dolist (trash (list (format nil "~a.Trash/~d/" top uid) (format nil "~a.Trash-~d/" top uid)))
                       (when (and (not (string= trash (tuatara.io:home-trash)))
                                  (ignore-errors (tuatara.io:path-stamp (tuatara.io:path-join trash "info/")))
                                  (tuatara.io:drive-trash-sound-p trash top))
                         (pushnew (cons trash top) found :key #'car :test #'string=))))))))
    ;; one trash seen through two mounts (a bind mount) listed once
    (let ((seen '()))
      (remove-if (lambda (entry)
                   (let* ((stamp (ignore-errors (tuatara.io:path-stamp (tuatara.io:path-join (car entry) "info/"))))
                          (key (and stamp (cons (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp)))))
                     (cond ((null key) nil)
                           ((member key seen :test #'equal) t)
                           (t (push key seen) nil))))
                 (nreverse found)))))

(defparameter *trash-info-limit* 65536
  "The most octets of a .trashinfo read; a larger one is left out, as one
that cannot be read.")

(defun trash-contents ()
  "What is in the trashes (TRASH-DIRECTORIES), a list of TRASHED, the last to
go there first; those whose .trashinfo cannot be read left out. It reads
the disk, so it is a job's work (RIMU-TRASH-READ). A .trashinfo
is read as any file is read safely (TUATARA.IO:FILE-TEXT): only a regular
file, a link followed, never a named pipe or a device, at most
*TRASH-INFO-LIMIT* octets, its access time kept where the system allows."
  (let ((things '()))
    (loop for (trash . top) in (trash-directories)
          do (dolist (entry (ignore-errors (tuatara.io:directory-entries (tuatara.io:path-join trash "info/"))))
               (let ((info (first entry)))
                 (when (and (> (length info) 10) (string= ".trashinfo" info :start2 (- (length info) 10))
                            (not (member (subseq info 0 (- (length info) 10)) '("." "..") :test #'string=)))
                   (let* ((text (ignore-errors (tuatara.io:file-text (tuatara.io:path-join trash (format nil "info/~a" info))
                                                                     :limit *trash-info-limit* :too-large (constantly nil))))
                          (lines (and text (uiop:split-string text :separator '(#\Newline))))
                          (path (loop for line in lines
                                      when (uiop:string-prefix-p "Path=" line) return (tuatara.io:percent-decoded (subseq line 5))))
                          (date (loop for line in lines
                                      when (uiop:string-prefix-p "DeletionDate=" line) return (subseq line 13))))
                     (when path
                       (let ((name (subseq info 0 (- (length info) 10))))
                         (push (make-trashed trash name top
                                             (if (and top (not (uiop:string-prefix-p "/" path)))
                                                 (tuatara.io:path-join top path)
                                                 path)
                                             (or date "")
                                             (eq :directory (ignore-errors (tuatara.io:path-kind
                                                                            (tuatara.io:path-join trash (format nil "files/~a" name))))))
                               things))))))))
    (sort things #'string> :key #'trashed-date)))

(defparameter *trash-columns* '((" M" :width 2) ("Name" :width 28) ("Deleted" :width 19) ("Where it was"))
  "The columns of the list of the trash, as TABLE-HEADING-PIECES takes them:
the mark (> or D), the thing's name, when it went there and the folder it was in.")

(defun trashed-key (thing)
  "What a mark on THING in the list of the trash is kept by: its trash and its
name there."
  (cons (trashed-trash thing) (trashed-name thing)))

(defun trash-sections (things)
  "THINGS in sections, each (SECTION . THINGS): your own trash first (SECTION
:HOME), then each drive's in the order of their names, SECTION the drive's
top folder (GROUP-SECTIONS)."
  (group-sections things (lambda (thing) (or (trashed-top thing) :home)) :first :home :order #'string<))

(defun trash-section-title (section)
  "What the list of the trash calls SECTION: your trash, or a drive's."
  (if (eq section :home) "Your trash" (format nil "The trash on ~a" (abbreviated-path section))))

(defun trash-cells (thing mark)
  "The cells of THING's line, marked MARK, in the list of the trash."
  (let ((folder (trashed-folder thing)))
    (list (list-mark-sign mark)
          (cons (escape-controls (format nil "~a~:[~;/~]" (tuatara.io:native-file-name (trashed-path thing)) folder))
                (or (list-mark-face mark) (and folder :list-directory)))
          (substitute #\Space #\T (trashed-date thing))
          (abbreviated-path (tuatara.io:parent-path (trashed-path thing))))))

;;; Read in the background, drawn from what was read

(defvar *trash-changes* 0
  "How many times rimu has changed what is in the trash (TRASH-CHANGED): a
list of the trash read at another count is read again when a window shows it.")

(defun trash-changed ()
  "Note that what is in the trash has changed: the list of the trash is read
again when a window shows it."
  (incf *trash-changes*))

(defstruct (trash-read (:include tuatara.runtime:job) (:constructor make-trash-read (buffer changes)) (:copier nil))
  "What is in the trash read in the background for BUFFER, the list of the
trash, a job (TUATARA.RUNTIME:JOB): CHANGES, *TRASH-CHANGES* as it began;
THINGS, once done, what it found (TRASH-CONTENTS)."
  changes things)

(setf (documentation 'trash-read-p 'function) "True when OBJECT is a TRASH-READ.")

(tuatara.runtime:define-job-words trash-read
  :doing "Reading the trash"
  :verb "read the trash"
  :left-words "the list of the trash is as it was"
  :at-exit :quiet)

(defmethod tuatara.runtime:job-holds-queue-p ((job trash-read))
  "False: other work on the trash's list need not wait for the trash to be read."
  nil)

(defun rimu-trash-show (buffer &optional (point :keep))
  "Draw BUFFER, the list of the trash, from what was last read of the trash
\(RIMU-TRASH-READ), in its sections (TRASH-SECTIONS), its marks and folds as
they are, point as RENDER-PIECES puts it; while nothing has been read yet,
saying that the trash is being read."
  (let* ((read (tuatara.buffer:buffer-local buffer 'trash-read-at))
         (things (tuatara.buffer:buffer-local buffer 'trash-things)))
    (render-table-list
     buffer *trash-columns* (trash-sections things)
     (lambda (thing mark section) (declare (ignore section)) (trash-cells thing mark))
     :sections t :section-title #'trash-section-title
     :marked-by (lambda (thing section) (declare (ignore section)) (trashed-key thing))
     :keep-marks nil
     :after (list (if read
                      (format nil "~%   ~[Nothing in the trash~;1 thing in the trash~:;~:*~d things in the trash~]~%"
                              (length things))
                      (format nil "~%   Reading the trash...~%")))
     :point point :key (or read *trash-changes*)
     :refill (lambda (buffer) (rimu-trash-read buffer :wait t))
     :redraw 'rimu-trash-show)))

(defun trash-point-on-thing (list)
  "Put point of LIST, the list of the trash, on the first thing in it, unless
it is on one."
  (unless (list-member-at list)
    (let ((first (find :member (tuatara.buffer:buffer-local list 'list-entries) :key #'first)))
      (when first (list-goto-entry list first)))))

(defun rimu-trash-read (list &key wait)
  "Read what is in the trash for LIST, the list of the trash, in the
background (TRASH-READ), unless that is under way, and draw it there once it
is read, marks kept on the things still there. With WAIT, wait up to
*RIMU-READ-WAIT* seconds for it, so that a trash read quickly shows at once."
  (let ((under-way (tuatara.buffer:buffer-local list 'trash-read)))
    ;; one read at a time: the one asked for until it is taken in, unless it was dropped
    (unless (and under-way (member under-way tuatara.runtime:*jobs*))
      (let ((job (make-trash-read list *trash-changes*)))
        (setf (tuatara.buffer:buffer-local list 'trash-read) job)
        (tuatara.runtime:start-job-thread job (tuatara.runtime:carry-settings
                                               '(*mounts-file* *trash-info-limit*)
                                               (lambda (job) (setf (trash-read-things job) (trash-contents))))
                                          :name "tuatara trash")
        (when (and wait (plusp *rimu-read-wait*))
          (sb-thread:join-thread (tuatara.runtime:job-thread job) :timeout *rimu-read-wait* :default nil)
          (unless (tuatara.runtime:job-running-p job)
            (tuatara.runtime:take-in-jobs (lambda (other) (eq other job)))))))))

(defmethod tuatara.runtime:job-take-in ((job trash-read))
  "Draw in its list what JOB found in the trash, marks kept on the things
still there, point on the first thing the first time; say why when the trash
could not be read."
  (let ((list (trash-read-buffer job)))
    (when (and (live-buffer-p list) (eq (tuatara.buffer:buffer-local list 'trash-read) job))
      (setf (tuatara.buffer:buffer-local list 'trash-read) nil)
      (if (eq (trash-read-status job) :done)
          (let ((first (null (tuatara.buffer:buffer-local list 'trash-read-at)))
                (things (trash-read-things job)))
            (keep-list-marks list (mapcar #'trashed-key things))
            (setf (tuatara.buffer:buffer-local list 'trash-things) things
                  (tuatara.buffer:buffer-local list 'trash-read-at) (trash-read-changes job))
            (rimu-trash-show list)
            (when first (trash-point-on-thing list)))
          (message "~a" (tuatara.runtime:job-ending-words job))))))

(keep-shown-current 'rimu-trash-mode
                    (lambda (list) (declare (ignore list)) *trash-changes*)
                    (lambda (list window) (declare (ignore window)) (rimu-trash-read list)))

(define-command rimu-trash ()
  "List what is in the trash, as the buffer list (\\[buffers]) lists buffers: a section
for your own trash and one for each drive's, the last to go there first, each
thing's name, when it went and where it was. There \\<rimu-trash-mode>\\[list-visit] opens one safely,
\\[list-mark-entry] and \\[list-mark-for-deletion] mark, \\[rimu-trash-restore] puts the marked ones back,
\\[list-delete-marked] deletes them for good, \\[rimu-trash-empty] empties the trash."
  (let ((buffer (buffer-in-mode "*Trash*" 'rimu-trash-mode)))
    (show-buffer buffer)
    (unless (tuatara.buffer:buffer-local buffer 'trash-read-at)
      (rimu-trash-show buffer :line))
    (rimu-trash-read buffer :wait t)
    (trash-point-on-thing buffer)))

;;; Put back, deleted for good

(defun trash-things-of (list keys)
  "The things in the trash the marks KEYS of LIST stand for, as LIST last read them."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (thing (tuatara.buffer:buffer-local list 'trash-things))
      (setf (gethash (trashed-key thing) table) thing))
    (remove nil (mapcar (lambda (key) (gethash key table)) keys))))

(defun trash-restore-refusal (thing)
  "Why THING is not put back where its .trashinfo says it was, in a few words,
or NIL when it may be: that place must be a whole path in your own trash, and
within its drive in a drive's (the folders on the way as they really are,
links followed); it may not climb with .. anywhere, nor hold a control
character (a NUL among them)."
  (let ((path (trashed-path thing))
        (top (trashed-top thing)))
    (cond ((find-if (lambda (c) (or (char< c #\Space) (char= c #\Rubout))) path)
           "its .trashinfo's path holds a control character")
          ((member ".." (uiop:split-string path :separator "/") :test #'string=)
           "its .trashinfo's path climbs with ..")
          ((not (uiop:string-prefix-p "/" path))
           "its .trashinfo gives no whole path")
          ((and top (not (and (tuatara.io:path-within-p path top)
                              ;; the nearest folder there, links followed
                              (let ((above (tuatara.io:parent-path path)))
                                (loop until (or (tuatara.io:path-stamp above) (string= above "/"))
                                      do (setf above (tuatara.io:parent-path (string-right-trim "/" above))))
                                (tuatara.io:really-within-p above top)))))
           "its .trashinfo's path leads out of its drive"))))

(defun trash-restore-thing (thing)
  "Put THING back where it was: another name asked for when that one is
taken; the directories above it made again when they are gone. Returns where
it went, or NIL (said why)."
  (let* ((target (trashed-path thing))
         (target (if (tuatara.io:path-stamp target :follow nil)
                     (read-native-path (format nil "~a is taken; put it back as: " (abbreviated-path target))
                                       :initial (abbreviated-path target) :writing t)
                     target)))
    (when target
      (handler-case
          (let ((above (tuatara.io:parent-path target)))
            (unless (tuatara.io:path-stamp above)
              (tuatara.io:ensure-directories above))
            (tuatara.io:rename-path (trashed-files-path thing) target)
            (ignore-errors (tuatara.io:remove-file (trashed-info-path thing)))
            target)
        (error (e) (message "~a not put back: ~a" (tuatara.io:native-file-name (trashed-path thing)) e) nil)))))

(defun trash-restore-question (things refused)
  "The question whether to put THINGS back, where they go said; REFUSED, each
\(THING . WHY), those left in the trash (TRASH-RESTORE-REFUSAL)."
  (let ((folders (remove-duplicates (mapcar (lambda (thing) (abbreviated-path (tuatara.io:parent-path (trashed-path thing)))) things)
                                    :test #'string= :from-end t)))
    (format nil "~a?~@[ (left in the trash: ~{~a~^; ~})~]"
            (list-question "Put back" "thing" things
                           :name-of (lambda (thing) (abbreviated-path (trashed-path thing)))
                           :where (and (rest things)
                                       (format nil "to ~{~a~^, ~}~@[ and ~d more folders~]"
                                               (subseq folders 0 (min 3 (length folders)))
                                               (and (> (length folders) 3) (- (length folders) 3)))))
            (trash-refused-words refused))))

(defun trash-refused-words (refused)
  "Each of REFUSED, (THING . WHY), as its name and why, or NIL for none."
  (mapcar (lambda (pair)
            (format nil "~a: ~a" (escape-controls (tuatara.io:native-file-name (trashed-path (car pair)))) (cdr pair)))
          refused))

(defun trash-restore-marked (list keys)
  "Put the things KEYS mark in LIST back where they were, after asking
\(TRASH-RESTORE-THING), those whose .trashinfo names no place they may go
\(TRASH-RESTORE-REFUSAL) left in the trash, and why said; LIST read again after."
  (let* ((things (trash-things-of list keys))
         (refused (loop for thing in things
                        for why = (trash-restore-refusal thing)
                        when why collect (cons thing why)))
         (things (remove-if (lambda (thing) (assoc thing refused)) things)))
    (cond ((/= (length things) (length (trash-sound-things things)))
           (message "Not put back: the trash on that drive has links in it or is not yours"))
          ((null things)
           (message "Not put back: ~{~a~^; ~}" (trash-refused-words refused)))
          ((y-or-n-p* (trash-restore-question things refused))
           (let ((back (remove nil (mapcar #'trash-restore-thing things))))
             (rimu-refresh-lists back)
             (trash-changed)
             (unmark-list list)
             (rimu-trash-read list :wait t)
             (when back
               (message "Put back: ~{~a~^, ~}~@[; left in the trash: ~{~a~^; ~}~]"
                        (mapcar #'abbreviated-path back) (trash-refused-words refused))))))))

(defun trash-delete-things (things list)
  "Delete THINGS, in the trash, for good, each with its .trashinfo, in the
background (RIMU-START-OPERATION); the marks of LIST, the list of the trash,
taken away and the list read again once that is done."
  (let ((infos (make-hash-table :test 'equal)))
    (dolist (thing things)
      (setf (gethash (trashed-files-path thing) infos) (trashed-info-path thing)))
    (rimu-start-operation :delete (mapcar (lambda (thing) (list (trashed-files-path thing))) things) list
                          :along (lambda (path) (gethash path infos))
                          :then (lambda ()
                                  (when (live-buffer-p list)
                                    (unmark-list list)
                                    (rimu-trash-read list))))))

(defun trash-delete-marked (list keys)
  "Delete the things KEYS mark in LIST for good, after asking, in the
background (TRASH-DELETE-THINGS); they cannot be put back."
  (let ((things (trash-things-of list keys)))
    (cond ((/= (length things) (length (trash-sound-things things)))
           (message "Not deleted: the trash on that drive has links in it or is not yours"))
          ((yes-or-no-p* (format nil "~a for good? It cannot be put back."
                                 (list-question "Delete" "thing" things
                                                :name-of (lambda (thing) (tuatara.io:native-file-name (trashed-path thing))))))
           (trash-delete-things things list)))))

(define-command rimu-trash-restore ()
  "Put the marked things (>), or with none marked the one on the current line,
back where they were, after asking, where they go said; another name asked
for when one's is taken, the directories above it made again when they are
gone. One whose .trashinfo gives no whole path, climbs with .., or, in a
drive's trash, leads out of the drive, is left in the trash, and why said.
Lists of their directories show them."
  (act-on-list (current-buffer) #'trash-restore-marked "thing"))

(defmethod list-entry-noun ((mode rimu-trash-mode))
  "A thing in the trash."
  "thing")

(defmethod delete-entries ((mode rimu-trash-mode) list keys mark)
  "Delete the things for good, after asking, in the background
\(TRASH-DELETE-MARKED); they cannot be put back."
  (declare (ignore mark))
  (trash-delete-marked list keys))

(defmethod visit-entry ((mode rimu-trash-mode) list thing other)
  "Open THING safely, to look at, in another window with OTHER, the list staying
selected."
  (declare (ignore list))
  (visit-path (trashed-files-path thing) :safe t
              :show (if other
                        (lambda (b) (display-buffer b :other t))
                        (lambda (b) (show-buffer b)))))

(define-command rimu-trash-empty ()
  "Empty the trash: everything the list of the trash shows deleted for good,
after asking twice, in the background."
  (let* ((list (current-buffer))
         (things (trash-sound-things (tuatara.buffer:buffer-local list 'trash-things))))
    (cond ((null (tuatara.buffer:buffer-local list 'trash-read-at)) (message "The trash is still being read"))
          ((null things) (message "The trash is empty"))
          ((and (yes-or-no-p* (format nil "Empty the trash, ~d thing~:p, for good?" (length things)))
                (yes-or-no-p* "Really? Nothing in it can be put back after."))
           (trash-delete-things things list)))))

(defun install-rimu-trash ()
  "Bind the keys of the list of the trash."
  (define-keys *rimu-trash-keymap* '(("RET" list-visit) ("l" list-visit) ("o" list-visit-other)
                                     ("h" quit-window) ("R" rimu-trash-restore) ("X" rimu-trash-empty))))

(commands-for-mode 'rimu-trash-mode "the list of the trash (\\[rimu-trash])"
                   'list-visit 'list-visit-other 'rimu-trash-restore 'rimu-trash-empty)
