;;;; lisp/files/files.lisp --- Saving with care, files changed on disk, reverting.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The commands' side of tuatara.io. A save runs on a thread of its
;;;; own and is waited for briefly: a small file is written before you notice,
;;;; a large one carries on in the background with its progress in the mode
;;;; line while you work. Saving asks before overwriting a file another program
;;;; changed, a write-protected file, or another file by saving as. The
;;;; command loop's ticks take in finished saves and snapshots, and keep a
;;;; watcher thread told of the visited files: inotify tells it what happens to
;;;; them (files on network file systems it looks at every two seconds), so that
;;;; the editor hears of a change at once without looking for it, and no file
;;;; server that stops answering can hold the editor up. A file changed on disk
;;;; is said and shown in the mode line, many at once in one message; a buffer
;;;; whose text can no longer be trusted is made read-only; M-x revert-buffer
;;;; reads the file again, keeping point on the same text. With
;;;; *reload-changed-files*, or M-x follow-file for one buffer, a buffer keeps
;;;; up by itself: what a growing file gains is added (tail -f), and a file
;;;; rewritten is read again when the buffer has no changes of its own. Killing
;;;; a buffer being saved, or leaving the editor while saves run, asks and
;;;; waits for them.

(in-package #:tuatara.defaults)

(defvar *save-wait* 1/2
  "Seconds a save is waited for before it carries on in the background, with
its progress in the mode line.")

(define-setting *reload-changed-files* nil
  "Whether every buffer keeps up with its file on disk by itself.
What another program appends to the file is added to the buffer at once, even
one with changes of its own. With point at the end, it stays at the end, as
tail -f has it. A file rewritten is read again when the buffer has no changes
of its own, point and the window staying on the same text, and one undo takes
that back. \\[follow-file] chooses for one buffer.
- t: buffers keep up by themselves.
- nil: the mode line and echo area say the file changed, and \\[revert-buffer]
  reads it again. This is the standard."
  :values boolean)

(defvar *save-refusals* '()
  "Functions called with a buffer about to be saved: one returning a string
refuses the save, and the string is said. Work still writing into the buffer
adds one here, so that it is never saved half done.")

(defvar *before-reload-functions* '()
  "Functions called with a buffer whose text is about to be read again from
its file, replacing what it holds. One that returns a string has it said with
what reading again says.")

(defvar *after-reload-functions* '()
  "Functions called with a buffer whose text was just read again from its file.")

(defvar *saves-as* '()
  "Background saves that give their buffer a new file, whose buffer takes the
file's name when they are done.")

(defvar *save-continuations* '()
  "The saves under way whose command goes on once they are done, each (JOB .
THEN): THEN, a function of the buffer, runs as the next command once JOB has
saved it (SAVE-BUFFER-WITH-CARE's THEN).")

(defmethod tuatara.runtime:job-take-in :after ((job tuatara.io:save-job))
  "Say how the background save JOB ended. After a save, give a buffer saved as
another file that file's name, and the major mode that name calls for when it
calls for one. A scratch buffer saved as notes.org is in Org mode then. A
buffer opened safely keeps showing its text as it is. Then run
*AFTER-SAVE-FUNCTIONS*, and go on with what was to follow the save
\(*SAVE-CONTINUATIONS*)."
  (let ((buffer (tuatara.io:save-job-buffer job))
        (seconds (tuatara.runtime:seconds-since (tuatara.io:save-job-started job)))
        (then (cdr (assoc job *save-continuations*))))
    (setf *save-continuations* (remove job *save-continuations* :key #'car))
    (when (and then (eq (tuatara.io:save-job-status job) :done) (live-buffer-p buffer))
      (defer-action (lambda () (funcall then buffer))))
    (ecase (tuatara.io:save-job-status job)
      (:done
       (when (and (member job *saves-as*) (live-buffer-p buffer))
         (let ((name (tuatara.io:native-file-name (tuatara.buffer:buffer-file buffer))))
           (unless (string= name (tuatara.buffer:buffer-name buffer))
             (setf (tuatara.buffer:buffer-name buffer) (tuatara.buffer:unique-buffer-name name)))
           (saved-out-of-evidence buffer)
           (take-mode-of-name buffer)))
       (message "Wrote ~a~:[~*~; (~a in ~,1f s)~]" (abbreviated-path (tuatara.io:save-job-target job))
                (> seconds *save-wait*) (octets-words (tuatara.io:save-job-total job)) seconds)
       (when (live-buffer-p buffer)
         (run-hook *after-save-functions* buffer)))
      ((:stopped :failed) (message "~a" (tuatara.runtime:job-ending-words job))))
    (setf *saves-as* (remove job *saves-as*))))

(defun saved-out-of-evidence (buffer)
  "When BUFFER, opened safely for being evidence, has just been saved as a
file outside every evidence directory: it stays opened safely, what it holds
being the evidence's still, but may be changed now when you may write the
new file; \\[end-safe-visit] opens it as usual. Said so."
  (when (shiftf (tuatara.buffer:buffer-local buffer :saved-from-evidence) nil)
    (when (and (tuatara.io:safe-visit-p buffer) (null (buffer-evidence buffer)))
      ;; the evidence it opened safely for is no longer the file it visits
      (setf (tuatara.io:safe-visit-p buffer) t)
      (setf (buffer-read-only-p buffer)
            (not (tuatara.io:writable-p (tuatara.buffer:buffer-file buffer))))
      (message "~a is a copy out of evidence: still opened safely, and may be changed; \\[end-safe-visit] opens it as usual"
               (tuatara.buffer:buffer-name buffer)))))

(defun take-mode-of-name (buffer)
  "Give BUFFER the major mode the name of its file calls for, but for a name
that calls for none, or a file visited safely."
  (let ((mode (mode-for-file (tuatara.buffer:buffer-file buffer))))
    (unless (or (eq mode 'fundamental-mode) (eq mode (type-of (buffer-major-mode buffer)))
                (tuatara.io:safe-visit-p buffer))
      (set-major-mode buffer mode))))

(defun save-target (buffer path)
  "The file a save of BUFFER writes: PATH, read relative to the buffer's
directory (or *DEFAULT-DIRECTORY*) with a leading ~ for the home directory, or
the buffer's own file."
  (if (null path)
      (tuatara.buffer:buffer-file buffer)
      (let ((file (tuatara.buffer:buffer-file buffer)))
        (tuatara.io:expand-path path :directory (if file (tuatara.io:parent-path file) (buffer-directory buffer))))))

(defvar *save-path* nil
  "The file the save being made writes, when it is saved as another: for
*BEFORE-SAVE-FUNCTIONS* that make it again later.")

(defvar *save-then* nil
  "What is to follow the save being made, a function of the buffer, or NIL:
for *BEFORE-SAVE-FUNCTIONS* that make it again later (SAVE-BUFFER-WITH-CARE's
THEN).")

(defun save-question (quiet function prompt)
  "Ask PROMPT with FUNCTION (YES-OR-NO-P*, Y-OR-N-P*) unless QUIET, when it
is not asked and counts as answered no."
  (and (not quiet) (funcall function prompt)))

(defun save-buffer-with-care (buffer &key path then quiet)
  "Save BUFFER to its file, or to PATH (save as), as \\[save-buffer] does: ask
before overwriting a file changed on disk since it was read, a write-protected
file, or another existing file; then save on a thread of its own, waiting up to
*SAVE-WAIT* seconds for it, after which it carries on in the background. An
unmodified buffer whose file is as it was is not written again. A new file
in a directory not there yet has the directory made, after asking. A new file
whose name says encrypted but not by GnuPG (.age) is not written: its text
would be there to read. Nor is a buffer that work still writes into
\(*SAVE-REFUSALS*), such as typing at a split's places. Saved as a file
another buffer visits, that buffer is killed after asking, or the save is
refused when it has changes not saved. A function of
*BEFORE-SAVE-FUNCTIONS* that returns :LATER (formatting first) has the save
wait: it saves the buffer itself once ready. THEN, a function of the buffer,
runs once the buffer is saved, as the next command when the save went on in
the background, or at once when nothing needed saving; never when it was not
saved. True when BUFFER is saved, or being saved; NIL when it was not. With
QUIET nothing is asked, each question counting as answered no (the file
changed on disk, write-protected, another file's name): the save is refused."
  (let* ((file (save-target buffer path))
         (name (tuatara.buffer:buffer-name buffer))
         (as (and path (not (equal file (tuatara.buffer:buffer-file buffer)))))
         (target (tuatara.io:resolve-links file))
         (existing (tuatara.io:path-stamp target))
         (running (tuatara.io:save-of buffer))
         (other (and as (buffer-visiting file :except buffer)))
         (refused (some (lambda (function) (funcall function buffer)) *save-refusals*))
         (called (if as (abbreviated-path file) name)))
    (cond
      (running (message "~a is already being saved (~d%)" name (tuatara.io:save-progress running)))
      (refused (message "~a" refused))
      ((remote-name-p path)
       (message "~a is on another machine: not saved; open it there (\\[find-file]) and save that buffer" path))
      ((eq (tuatara.io:file-disk-state buffer) :stale)
       (message "~a cannot be saved: its file was rewritten on disk while shown from it; \\[revert-buffer] reads it again"
                name))
      ((and (not as) existing (not (tuatara.buffer:buffer-modified-p buffer))
            (null (tuatara.io:check-file buffer)))
       (message "(No changes need to be saved)")
       (when then (funcall then buffer))
       (return-from save-buffer-with-care t))
      ((and other (tuatara.buffer:buffer-modified-p other))
       (message "~a is open in ~a with changes not saved: not saved" called (tuatara.buffer:buffer-name other)))
      ((and other (not (and (kill-buffer-allowed-p other)
                            (save-question quiet
                                           #'yes-or-no-p*
                                           (format nil "~a is open in ~a; write over it and kill that buffer?"
                             called (tuatara.buffer:buffer-name other))))))
       (message "Not saved"))
      ((and as existing (not other) (not (save-question quiet
                                                        #'yes-or-no-p*
                                                        (format nil "~a exists; overwrite it?" called))))
       (message "Not saved"))
      ((and (null existing) (unencryptable-name-p (tuatara.io:native-file-name file)))
       (message "~a is not saved: only .gpg files can be encrypted, and its text would be there to read" called))
      ((and existing (not (tuatara.io:writable-p target))
            (not (save-question quiet #'yes-or-no-p* (format nil "File ~a is write-protected; try to save anyway?"
                                       (tuatara.io:native-file-name target)))))
       (message "Attempt to save to a file that you aren't allowed to write"))
      ((and (null existing) (null (tuatara.io:path-stamp (tuatara.io:parent-path target)))
            (not (save-question quiet #'y-or-n-p* (format nil "The directory ~a is not there; make it?"
                                       (escape-controls (abbreviated-path (tuatara.io:parent-path target)))))))
       (message "Not saved"))
      (t
       (unless (tuatara.io:path-stamp (tuatara.io:parent-path target))
         (tuatara.io:ensure-directories (tuatara.io:parent-path target)))
       (when other (tuatara.buffer:kill-buffer other))
       (when (let ((*save-path* path) (*save-then* then))
               (loop for function in *before-save-functions*
                     thereis (eq :later (call-hook-function function buffer))))
         (return-from save-buffer-with-care t))
       ;; saved as a file out of evidence, it may be changed once written (SAVED-OUT-OF-EVIDENCE)
       (when as
         (setf (tuatara.buffer:buffer-local buffer :saved-from-evidence)
               (and (tuatara.io:safe-visit-p buffer) (buffer-evidence buffer) t)))
       (let ((job (handler-case (tuatara.io:start-save buffer :path file :force as)
                    (tuatara.io:file-changed-on-disk ()
                      (if (save-question quiet
                                         #'yes-or-no-p*
                                         (format nil
                                                 "~a changed on disk since it was read; overwrite it with this buffer?"
                           called))
                          (tuatara.io:start-save buffer :path file :force t)
                          (progn (message "Not saved") nil))))))
         (when job
           (when as (push job *saves-as*))
           (when then (push (cons job then) *save-continuations*))
           (take-in-soon job *save-wait* (format nil "Saving ~a" name))
           (return-from save-buffer-with-care t)))))
    nil))

(defun following-p (buffer)
  "True when BUFFER keeps up with its file on disk by itself: as
\\[follow-file] chose for it, else as *RELOAD-CHANGED-FILES* says."
  (case (tuatara.buffer:buffer-local buffer :follow-file)
    (:on t) (:off nil) (t *reload-changed-files*)))

(defun reload-buffer (buffer)
  "Read BUFFER's file again (TUATARA.IO:REVERT-BUFFER-FILE), point and the
first line of each window showing it staying on the same text (TEXT-MAPPING), once
*BEFORE-RELOAD-FUNCTIONS* have run; then *AFTER-RELOAD-FUNCTIONS*. Returns BUFFER, and what those functions
said they did, joined in one string, or NIL."
  (let* ((said (format nil "~{~a~^; ~}"
                       (remove-if-not #'stringp (mapcar (lambda (function) (call-hook-function function buffer))
                                                        *before-reload-functions*))))
         (forward nil))
    (tuatara.display:call-keeping-window-starts
     (buffer-windows buffer)
     (lambda ()
       (tuatara.io:revert-buffer-file buffer :map (lambda (old new)
                                                    (multiple-value-bind (f b) (text-mapping old new)
                                                      (setf forward f)
                                                      (values f b))))
       forward))
    (run-hook *after-reload-functions* buffer)
    (values buffer (and (plusp (length said)) said))))

(defun catch-up (buffer)
  "Bring BUFFER up to its file on disk, which another program changed: add what
the file grew by when it only grew, else read it again when the buffer has no
changes of its own. Returns :APPENDED, :RELOADED, or NIL when the buffer was
left as it is (it has changes of its own, or the file is gone)."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (cond ((or (null file) (tuatara.io:saving-p buffer)
               (not (member (tuatara.io:file-disk-state buffer) '(:changed :stale))))
           nil)
          ((tuatara.io:append-from-disk buffer) :appended)
          ((or (tuatara.buffer:buffer-modified-p buffer) (null (tuatara.io:path-stamp file :failure nil))) nil)
          (t (handler-case (progn (reload-buffer buffer)
                                  (setf (tuatara.buffer:buffer-local buffer :disk-state-said) nil)
                                  :reloaded)
               (tuatara.io:file-system-error () nil))))))

(defun buffer-names (buffers)
  "The names of BUFFERS for a message: a, a and b, a, b and c, a, b, c and 2 more."
  (let ((names (mapcar #'tuatara.buffer:buffer-name buffers)))
    (if (> (length names) 4)
        (format nil "~{~a~^, ~} and ~d more" (subseq names 0 3) (- (length names) 3))
        (format nil "~{~a~#[~; and ~:;, ~]~}" names))))

(defun react-to-disk (buffers &key visited)
  "Take in what the watcher found about BUFFERS, whose files changed on disk:
bring those that follow their files up to date (CATCH-UP), and say in one
message what became of them all, each state once. Growth taken in is not
said: a log followed would say it every second. With VISITED, BUFFERS were
just visited again: each is brought up to date as one that follows its file,
unless \\[follow-file] stopped it following or it has changes of its own, which
are never touched then; what became of it is said even when it was said
before."
  (let ((read-again '()) (changed '()) (kept '()) (deleted '()) (stale '()))
    (dolist (buffer buffers)
      (let* ((following (or (following-p buffer)
                            (and visited (not (eq (tuatara.buffer:buffer-local buffer :follow-file) :off)))))
             (done (and following
                        (not (and visited (tuatara.buffer:buffer-modified-p buffer)))
                        (catch-up buffer)))
             (state (tuatara.io:file-disk-state buffer)))
        (cond ((eq done :reloaded) (push buffer read-again))
              ((eq done :appended))
              ((and (not visited) (eq state (tuatara.buffer:buffer-local buffer :disk-state-said))))
              (t (case state
                   ;; one that follows its file, left as it is: say why
                   (:changed (if (and following (tuatara.buffer:buffer-modified-p buffer))
                                 (push buffer kept)
                                 (push buffer changed)))
                   (:deleted (push buffer deleted))
                   (:stale (push buffer stale)))))
        (setf (tuatara.buffer:buffer-local buffer :disk-state-said) (tuatara.io:file-disk-state buffer))))
    (flet ((names (list) (buffer-names (reverse list))))
      (let ((parts (remove nil
                           (list (and read-again
                                      (format nil
                                              "Read ~a again, changed on disk (undo takes it back)"
                                              (names read-again)))
                                 (and kept (format nil (substitute-command-keys
                                                        "~a changed on disk but ~:[is~;are~] not read again, having changes of ~
                                                         ~:*~:[its~;their~] own: \\[revert-buffer] takes the disk's, ~
                                                         \\[save-buffer] writes yours over it"
                                                        :marks t :for-format t)
                                                   (names kept) (rest kept)))
                                 (disk-state-words :changed changed)
                                 (disk-state-words :deleted deleted)
                                 (disk-state-words :stale stale)))))
        (when parts (message "~{~a~^; ~}" parts))))))

(defun disk-state-words (state buffers)
  "What has become on disk of the files of BUFFERS, given the latest last, in
the STATE they are in (:CHANGED, :DELETED or :STALE), as a sentence; NIL for
no buffers."
  (when buffers
    (format nil (substitute-command-keys
                 (ecase state
                   (:changed "~a changed on disk; \\[revert-buffer] reads ~:[it~;each~] again")
                   (:deleted "~a ~:[was~;were~] deleted on disk; saving writes ~:*~:[it~;them~] again")
                   (:stale "~a ~:[was~;were~] rewritten on disk while shown from ~:*~:[it~;them~]; read-only now, ~
                            \\[revert-buffer] reads ~:*~:[it~;each~] again"))
                 :marks t :for-format t)
            (buffer-names (reverse buffers)) (rest buffers))))

(defun say-disk-state (buffer)
  "Say once what has become of BUFFER's file on disk, if anything."
  (let ((state (tuatara.io:file-disk-state buffer)))
    (unless (eq state (tuatara.buffer:buffer-local buffer :disk-state-said))
      (setf (tuatara.buffer:buffer-local buffer :disk-state-said) state)
      (when (member state '(:changed :deleted :stale))
        (message "~a" (disk-state-words state (list buffer)))))))

(defvar *disk-news-put-off* '()
  "Buffers whose files changed on disk while a question waiting in a frame was
about them (BUFFER-HELD-BY-QUESTION-P): what the change brings is taken in
once it is answered.")

(defun files-tick (editor)
  "Tick function: take in the countings of lines, saves and snapshots that
have ended (a large file's lines are counted in the background after it
opens), and keep the watcher told of the files visited. Take in what it found
\(REACT-TO-DISK), but for buffers a waiting question is about, put that off
until it is answered (*DISK-NEWS-PUT-OFF*). Say what has become of the shown
buffer's file. Asks to be called soon while work runs; otherwise the watcher
wakes the editor when it has news."
  (let ((shown (tuatara.display:window-buffer (editor-window editor)))
        (counting (tuatara.io:finish-indexing)))
    (tuatara.io:finish-saves)
    (tuatara.io:finish-snapshots)
    (tuatara.io:finish-leases)
    (tuatara.io:watch-files tuatara.buffer:*buffers*)
    (multiple-value-bind (changed again) (tuatara.io:take-file-looks)
      (when (or changed again)
        (tuatara.runtime:debug-report :file
                                      "~@[changed on disk: ~{~a (~(~a~))~^, ~}~]~:[~;; ~]~@[grown on disk: ~{~a~^, ~}~]"
                                      (loop for b in changed
                                            collect (tuatara.buffer:buffer-name b)
                                            collect (tuatara.io:file-disk-state b))
                                      (and changed again) (mapcar #'tuatara.buffer:buffer-name again)))
      (let* ((news (remove-duplicates (append (shiftf *disk-news-put-off* '())
                                              changed (remove-if-not #'following-p again))))
             (now (remove-if #'buffer-held-by-question-p news)))
        (setf *disk-news-put-off* (remove-if-not #'buffer-held-by-question-p news))
        (when now (react-to-disk now))))
    (say-disk-state shown)
    (and (or counting (tuatara.io:saves-running) (tuatara.io:snapshots)
             (tuatara.io:leases-moving-p) *disk-news-put-off*)
         t)))

(defun safe-mode-line (editor buffer)
  "Mode line function, just before the buffer's name: Safe for a file opened
safely."
  (declare (ignore editor))
  (and (tuatara.io:safe-visit-p buffer) "Safe"))

(defun files-mode-line (editor buffer)
  "Mode line function: carriage return and line feed (CRLF) for a buffer whose
lines end as Windows ends them, carriage return (CR) for one whose lines
end with a carriage return alone, Counting lines while a large file's lines
are counted, the saves running (this buffer's first), and what has become
of this buffer's file on disk."
  (declare (ignore editor))
  (let ((saves (sort (copy-list (tuatara.io:saves-running)) #'<
                     :key (lambda (job) (if (eq (tuatara.io:save-job-buffer job) buffer) 0 1))))
        (state (case (tuatara.io:file-disk-state buffer)
                 (:changed "Changed on disk") (:deleted "Deleted on disk") (:stale "Stale, read-only")))
        (ending (case (tuatara.buffer:buffer-line-ending buffer) (:crlf "CRLF") (:cr "CR")))
        (following (eq (tuatara.buffer:buffer-local buffer :follow-file) :on))
        (counting (tuatara.io:index-job-of buffer)))
    (when (or saves state ending following counting)
      (format nil "~{~a~^  ~}"
              (append (and ending (list ending))
                      (and following (list "Following"))
                      (and counting (list (format nil "Counting lines ~d%" (tuatara.io:index-progress counting))))
                      (mapcar (lambda (job)
                                (format nil "~:[~a: ~;~*~]Saving ~d%"
                                        (eq (tuatara.io:save-job-buffer job) buffer)
                                        (tuatara.buffer:buffer-name (tuatara.io:save-job-buffer job))
                                        (tuatara.io:save-progress job)))
                              saves)
                      (and state (list state)))))))

(defun wait-for-saves (jobs &key seconds)
  "Wait for the background saves JOBS to end, saying so meanwhile, the editor
reading keys all the while, and take them in: true when they all ended; NIL
when \\[keyboard-quit] stopped the waiting (a save on a file system that does not
answer), or SECONDS in all went by when given, the saves going on in the
background."
  (let ((deadline (and seconds (tuatara.runtime:deadline-in seconds))))
    (prog1 (loop for job in jobs
                 always (wait-interruptibly (tuatara.runtime:job-thread job)
                                            :message (format nil "Waiting for ~a to be saved"
                                                             (tuatara.buffer:buffer-name
                                                              (tuatara.runtime:job-buffer job)))
                                            :grace 1/20
                                            :seconds (and deadline (tuatara.runtime:seconds-left deadline))))
      (tuatara.io:finish-saves))))

(define-command revert-buffer ()
  "Read the current buffer's file again, replacing its text: one step that undo
takes back, unless the text could no longer be trusted (then the history goes
too). Point and the window stay on the same text. A file that only grew has
just what it grew by added. Asks first when the buffer has changes not saved.
In a buffer a command made (a list, help, a report), make it again from what
it shows, point staying on its entry or its line: \\`r' and \\`g r' there.
A narrowed buffer is widened: all of it is read again. A grammar that went
wrong on its text before is tried on it again, the text parsed afresh."
  (let* ((buffer (current-buffer))
         (file (tuatara.buffer:buffer-file buffer))
         (refill (tuatara.buffer:buffer-local buffer :refill)))
    (cond ((and refill (null file)) (funcall refill buffer) (parse-afresh buffer))
          ;; a list or report not made again from anything, or a buffer of your own
          ((null file) (parse-afresh buffer)
                       (message (if (buffer-read-only-p buffer)
                                    "~a shows what was said or made then: nothing to read again"
                                    "~a visits no file: nothing to read again")
                                (tuatara.buffer:buffer-name buffer)))
          ((tuatara.io:save-of buffer) (message "~a is being saved; revert once the save is done"
                                                (tuatara.buffer:buffer-name buffer)))
          ((null (tuatara.io:path-stamp file)) (message "~a is not on disk" (abbreviated-path file)))
          ((and (tuatara.buffer:buffer-modified-p buffer)
                (not (yes-or-no-p* (format nil "Discard the changes to ~a and read it again?"
                                           (tuatara.buffer:buffer-name buffer)))))
           (message "Not reverted"))
          (t (tuatara.buffer:widen-buffer buffer)
             (let* ((added (and (not (tuatara.buffer:buffer-modified-p buffer))
                                (tuatara.io:append-from-disk buffer)))
                    (said (and (not added) (nth-value 1 (reload-buffer buffer)))))
               (setf (tuatara.buffer:buffer-local buffer :disk-state-said) nil)
               (parse-afresh buffer)
               (if added
                   (message "Read what ~a grew by on disk (~a)" (tuatara.buffer:buffer-name buffer) (octets-words added))
                   (message "Read ~a again~@[; ~a~]" (tuatara.buffer:buffer-name buffer) said)))))))

(define-command follow-file ()
  "Keep the current buffer up with its file on disk, or stop. What another
program appends to the file is added at once, even with changes of your own in
the buffer, and with point at the end it stays at the end, as tail -f has it.
A file rewritten is read again when the buffer has no changes of its own,
point staying on the same text. The mode line says Following.
*RELOAD-CHANGED-FILES* does this for every buffer."
  (let* ((buffer (current-buffer)) (on (not (following-p buffer))))
    (cond ((null (tuatara.buffer:buffer-file buffer)) (message "This buffer visits no file"))
          (t (setf (tuatara.buffer:buffer-local buffer :follow-file) (if on :on :off))
             (let ((done (and on (catch-up buffer))))
               (message "~:[No longer following~;Following~] ~a~@[ (~a)~]" on (tuatara.buffer:buffer-name buffer)
                        (case done (:appended "added what it grew by") (:reloaded "read it again"))))))))

(defun wait-for-counting (buffer &optional line)
  "Wait for the lines of BUFFER's text to be counted in the background, for a
command that needs them (going to a line, say). It waits for all of them, or
with LINE, from 0, only until the count has passed that line. Far into a large
file on a slow drive, that line is there as soon as the lines before it are
counted, not the whole file. The echo area shows how far the count has got;
\\`C-g' gives up the command, the counting going on; other keys wait their
turn; a counting that failed gives an error saying why. With LINE, where that
line starts; NIL when the lines are all counted by then, or there is no
counting. Outside a command loop the lines are counted there and then (NIL
with LINE). Refused, saying so, while the file is still arriving
\(BUFFER-ARRIVING-P)."
  (when (tuatara.io:buffer-arriving-p buffer)
    (refuse "The lines of ~a are counted once the whole file has arrived" (tuatara.buffer:buffer-name buffer)))
  (let ((editor *editor*))
    (if (or (null editor) (null (editor-frontend editor)))
        (unless line
          (tuatara.buffer:buffer-adopt-text buffer (tuatara.text:index-rope (tuatara.buffer:buffer-text buffer)))
          nil)
        (loop
          (tuatara.io:finish-indexing)
          (when (tuatara.buffer:buffer-indexed-p buffer) (return nil))
          (let ((start (and line (tuatara.io:counted-line-start buffer line))))
            (when start (return start)))
          (when (eq (tuatara.buffer:buffer-local buffer :index-failed) (tuatara.buffer:buffer-text buffer))
            (refuse "The lines of ~a could not be counted: ~a" (tuatara.buffer:buffer-name buffer)
                   (tuatara.buffer:buffer-local buffer :index-error)))
          (let ((job (or (tuatara.io:index-job-of buffer) (tuatara.io:start-indexing buffer))))
            (unless job (return nil))
            (unless (wait-until-interruptibly
                     (lambda () (or (not (eq (tuatara.io:index-job-status job) :running))
                                    (and line (tuatara.io:counted-line-start buffer line))))
                     :message (lambda ()
                                (if line
                                    (format nil "Counting the lines of ~a up to line ~:d: ~d%"
                                            (tuatara.buffer:buffer-name buffer) (1+ line)
                                            (tuatara.runtime:running-percent (or (tuatara.io:counted-lines buffer) 0) line))
                                    (format nil "Counting the lines of ~a: ~d%"
                                            (tuatara.buffer:buffer-name buffer) (tuatara.io:index-progress job)))))
              (keyboard-quit-signal)))))))

(defmethod tuatara.buffer:host-wait-for-index ((host defaults-host) buffer)
  "Wait for BUFFER's lines to be counted in the background (WAIT-FOR-COUNTING)."
  (wait-for-counting buffer))

(defmethod tuatara.runtime:host-wait-for-job ((host defaults-host) job words)
  "Wait for JOB to end, the echo area showing WORDS while it takes a while;
\\`C-g' gives up what waited, JOB going on."
  (unless (wait-interruptibly (tuatara.runtime:job-thread job) :message words)
    (keyboard-quit-signal)))

(define-setting *safe-directories* '()
  "Directories whose files always open safely, as \\[find-file-safely] opens a
file.
A file opens read-only, with no major mode, hooks or version control, and
nothing is written beside it. This holds however it is opened:
\\[find-file], the files visited lately, a drop on the window, the command
line. \\[end-safe-visit] opens one as usual after all.
- nil: no directory, as standard.
- a list of directories' names, '(\"~/Evidence/\") for example."
  :values (list-of directory))

(define-setting *open-binary-files-safely* t
  "Whether a file that looks binary opens safely, as \\[find-file-safely] opens
it.
A file looks binary when it has a null (NUL) octet in its first 64 KB, as
executables, archives and disk images have and text has not.
\\[end-safe-visit] opens one as usual after all.
- t: such a file opens safely.
- nil: it opens as any file."
  :values boolean)


(defun paths-common-directory (paths)
  "The deepest directory all of PATHS (full native names) are in, ending in /."
  (let ((common (tuatara.io:parent-path (first paths))))
    (dolist (path (rest paths) common)
      (loop until (or (string= common "/") (tuatara.io:path-within-p path common))
            do (setf common (tuatara.io:parent-path common))))))

(defun path-uri (path)
  "The file: address of the native PATH, its characters other than letters, digits and -._~/ encoded (URL-PATH)."
  (concatenate 'string "file://" (tuatara.io:url-path (tuatara.io:native path))))

(defun uri-path (uri &key (hosts '("localhost")))
  "The native path of the file: address uniform resource identifier (URI), its
. and .. taken as they lead; NIL for another kind of address, one holding a
null (NUL), and one not from the root or of a computer other than those
HOSTS name (in any case); only file:///PATH and file://HOST/PATH are taken."
  (when (and (stringp uri) (> (length uri) 7) (string-equal "file://" uri :end2 7))
    (let* ((rest (subseq uri 7))
           (slash (position #\/ rest)))
      (when (and slash (or (zerop slash) (member (subseq rest 0 slash) hosts :test #'string-equal)))
        (let ((path (tuatara.io:percent-decoded (subseq rest slash))))
          (and (not (find (code-char 0) path)) (tuatara.io:clean-path path)))))))

(defun sandbox-time-zone ()
  "The environment that has a program shut in the sandbox write times in
your time zone, a list of (NAME VALUE): TZ as yours is set, or as the zone
/etc/localtime names; NIL when neither says one."
  (let ((zone (or (tuatara.io:own-variable "TZ") (system-time-zone-name))))
    (and zone (plusp (length zone)) (list (list "TZ" zone)))))

(defun safe-directories ()
  "The directories of *SAFE-DIRECTORIES*, read as a list however it was set
(SETTING-LIST): one directory's name given alone, or a list with a dotted
tail, counts every directory it names."
  (setting-list '*safe-directories* *safe-directories*))

(defun safe-directory-of (path)
  "The directory of *SAFE-DIRECTORIES* that the file or directory at PATH is,
or is in or below, by the names as given, by the file a link leads to, or by
where their links lead (PATH-WITHIN-P, REALLY-WITHIN-P), or NIL; T when
*SAFE-DIRECTORIES* cannot be read, every file then counting as in one.
Where a link leads is looked for only when there are directories."
  (handler-case
      (reporting-errors ("looking for ~a in the safe directories" path)
        (let ((directories (safe-directories)))
          (when directories
            (let* ((path (or (ignoring-errors-reported ("expanding ~a" path)
                               (tuatara.io:native (tuatara.io:expand-path path)))
                             path))
                   (leads-to (or (ignoring-errors-reported ("following the links of ~a" path)
                                   (tuatara.io:real-path-so-far path))
                                 path)))
              (find-if (lambda (directory)
                         (let ((directory (tuatara.io:native (tuatara.io:expand-path directory))))
                           (or (tuatara.io:path-within-p path directory)
                               (tuatara.io:path-within-p leads-to directory)
                               (tuatara.io:really-within-p path directory))))
                       directories)))))
    (error () t)))

(defun visit-safely-reason (path &optional stamp)
  "Why the file at PATH opens safely although that was not asked for: it is
evidence (*EVIDENCE-DIRECTORIES*), it is in one of *SAFE-DIRECTORIES*, or it looks
binary while *OPEN-BINARY-FILES-SAFELY* is true, by its own name, by the
file a link leads to, or, given its STAMP, by another name it has; NIL for none. When
*SAFE-DIRECTORIES* cannot be read, every file opens safely, and the reason
says so."
  (let ((directory (safe-directory-of path)))
    (cond ((evidence-visit-reason path))
          ((and stamp (evidence-inode-p stamp))
           "it has more than one name, and one may be in an evidence directory (*evidence-directories*)")
          ((eq directory t) "*safe-directories* could not be read")
          (directory (format nil "it is in ~a (*safe-directories*)" directory))
          ((and *open-binary-files-safely* (tuatara.io:binary-head-p (tuatara.io:file-head path)))
           "it looks binary (*open-binary-files-safely*)"))))

(defmethod tuatara.io:host-visit-safely-reason ((host defaults-host) path &optional stamp)
  "Why the file at PATH opens safely although not asked to (VISIT-SAFELY-REASON)."
  (visit-safely-reason path stamp))

(defun say-why-safe (buffer &optional also)
  "Say why BUFFER's file opened safely, when that was not asked for, and ALSO,
what else there is to say of how it opened (why it is not shown as a picture)."
  (let ((why (tuatara.io:safe-visit-reason buffer)))
    (when why
      (message "~a opened safely: ~a~@[; ~a~]; ~a" (tuatara.buffer:buffer-name buffer) why also (end-safe-visit-words buffer)))))

(defgeneric mode-file-name-at-point (mode buffer)
  (:documentation "The full name of the file BUFFER's point is on, for a mode
whose buffers list files (a rimu list's entry on the current line), or NIL.")
  (:method (mode buffer) (declare (ignore mode buffer)) nil))

(define-command show-file-name ()
  "Say the full name of the file the current buffer shows, its directory and
name, in the echo area, where *Messages* keeps it too: in a rimu list, that of
the entry on the current line; in a buffer with no file, the directory it
stands in. With a prefix argument (\\[universal-argument]), put it at point as well."
  (let* ((buffer (current-buffer))
         (file (tuatara.buffer:buffer-file buffer))
         (name (or (mode-file-name-at-point (buffer-major-mode buffer) buffer)
                   (and file (tuatara.io:native file)))))
    (if name
        (message "~a" name)
        (message "~a has no file; it stands in ~a" (tuatara.buffer:buffer-name buffer) (buffer-directory buffer)))
    (when (prefix-argument)
      (goto (tuatara.buffer:buffer-insert buffer (point) (or name (buffer-directory buffer)))))
    name))

;;; Directories watched on disk, for whichever parts want them

(defvar *directory-followers* '()
  "The parts that follow directories on disk, each (WANTED CHANGED TELLS):
WANTED a function of no arguments returning the directories (native names
ending in /) the part wants watched now, CHANGED a function called with those
of them the watcher found changed (a file in one made, deleted, renamed or
written). With TELLS true the part calls DIRECTORIES-FOLLOWED-CHANGED whenever
what WANTED returns changes, and WANTED is asked only then and when a buffer
is killed; else it is asked at every tick.")

(defvar *directories-followed* '()
  "What each part of *DIRECTORY-FOLLOWERS* wanted watched at the last tick, each
\(WANTED . DIRECTORIES), so that the watcher is told only when that changed.")

(defvar *directories-followed-count* (list 0)
  "A cons whose car counts the calls of DIRECTORIES-FOLLOWED-CHANGED.")

(defvar *directories-followed-seen* -1
  "The count of *DIRECTORIES-FOLLOWED-COUNT* at the last tick.")

(defun directories-followed-changed ()
  "Say that the directories a part following them (FOLLOW-DIRECTORIES-FOR with
TELLS) wants watched have changed, from any thread: they are asked for at the
next tick."
  (sb-ext:atomic-incf (car *directories-followed-count*))
  nil)

(defun follow-directories (editor)
  "Tick function: have the watcher watch the directories every part of
*DIRECTORY-FOLLOWERS* wants, telling it only when they changed (a part's list
the same object as before, or alike, costs nothing to compare; a directory
kept read has tens of thousands), and tell each part of those found changed.
A part that tells of its changes is asked only after it did."
  (declare (ignore editor))
  (let* ((count (car *directories-followed-count*))
         (told (/= count *directories-followed-seen*))
         (moved nil))
    (unless (and (= (length *directory-followers*) (length *directories-followed*))
                 (loop for follower in *directory-followers*
                       for kept in *directories-followed*
                       always (eq (first follower) (car kept))))
      (setf *directories-followed* (loop for (want) in *directory-followers* collect (list want))
            told t
            moved t))
    (loop for (want nil tells) in *directory-followers*
          for kept in *directories-followed*
          unless (and tells (not told))
            do (let ((new (ignoring-errors-reported ("asking which directories to follow") (funcall want))))
                 (unless (or (eq new (cdr kept)) (equal new (cdr kept)))
                   (setf (cdr kept) new
                         moved t))))
    (setf *directories-followed-seen* count)
    (when moved
      (tuatara.io:watch-directories (loop for (nil . list) in *directories-followed* append list)))
    (let ((changed (tuatara.io:take-directory-news)))
      (when changed
        (loop for (nil told) in *directory-followers*
              do (ignoring-errors-reported ("telling of directories changed") (funcall told changed))))))
  nil)

(defun buffer-leaving-followers (buffer)
  "Say that BUFFER, about to be killed, may take with it directories a part
follows (DIRECTORIES-FOLLOWED-CHANGED)."
  (declare (ignore buffer))
  (directories-followed-changed))

(defun follow-directories-for (wanted changed &key tells)
  "Have the directories WANTED returns watched on disk, and CHANGED called with
those found changed (see *DIRECTORY-FOLLOWERS*). With TELLS, WANTED is asked
only after DIRECTORIES-FOLLOWED-CHANGED is called, which the part does
whenever what WANTED returns changes, or a buffer is killed: nothing is
asked at each tick."
  (setf *directory-followers*
        (append (remove wanted *directory-followers* :key #'car) (list (list wanted changed tells)))
        *directories-followed-seen* -1))

(defun install-files ()
  "Let saves, snapshots, countings of lines and the watcher of files on disk
tick, what they find show in the mode line, a buffer's private directory be
deleted when it is killed (FORGET-PRIVATE-DIRECTORY) and kept fresh while it
lives (KEEP-PRIVATE-DIRECTORIES-FRESH), \\[wipe-file] be refused in a buffer opened
safely, and the evidence directories be marked (INSTALL-EVIDENCE)."
  (pushnew 'wipe-file *file-changing-commands*)
  (pushnew 'files-tick *tick-functions*)
  (pushnew 'follow-directories *tick-functions*)
  (pushnew 'buffer-leaving-followers tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'keep-private-directories-fresh *tick-functions*)
  (pushnew 'forget-private-directory tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'files-mode-line *mode-line-functions*)
  (pushnew 'safe-mode-line *mode-line-name-functions*)
  (install-evidence))
