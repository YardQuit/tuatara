;;;; lisp/search/jobs.lisp --- Jobs in the background: taken in, stopped, asked about, queued, and listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Every job in the background runs on the one runner (TUATARA.RUNTIME:JOB):
;;;; a replacement or a count, a sort or a filter, a sum, a count of words, a
;;;; split's search, a change at a large rectangle or at every match, dynamic
;;;; blocks made, a save, a list's files copied, an agenda worked out, a
;;;; calendar's days marked. Here the editor takes them in as they end, lights
;;;; P in the mode line while the buffer has its own, stops one with C-x J
;;;; (stop-job), asks once about all of them before a buffer is killed or the
;;;; editor left, asks before an edit spoils the work of one running over the
;;;; text edited, and lists them with C-x j (list-jobs).
;;;;
;;;; A buffer runs one job of most kinds at a time. Another asked for meanwhile
;;;; waits its turn instead of being refused, first in first out, and starts
;;;; once the one before has ended and been taken in, on the text as it leaves
;;;; it. The text a waiting job will read is followed by anchors, which follow
;;;; every change made to the buffer meanwhile, the jobs' own included, and it
;;;; is painted while it waits. Jobs of different buffers do not wait for one
;;;; another.

(in-package #:tuatara.defaults)

(defstruct (queued-job (:constructor make-queued-job (buffer words starter anchors shade counted place)) (:copier nil))
  "A job waiting its turn in BUFFER: WORDS say what it will do; STARTER, a
function of the offsets its ANCHORS stand at when its turn comes, starts it;
SHADE, a function of those offsets and the offsets FROM and TO of the text
shown, gives the (START END) stretches it will change, painted while it waits;
COUNTED true when it needs BUFFER's lines counted first (LINES-UNCOUNTED-P);
PLACE is where in BUFFER's undo history it was given
\(TUATARA.BUFFER:MAKE-UNDO-PLACE), for its change to stand there
\(JOB-BEGINS-WHERE-GIVEN); QUEUED is the time it was queued at."
  buffer words starter anchors shade counted place (queued (get-universal-time)))

(setf (documentation 'queued-job-p 'function) "True when OBJECT is a QUEUED-JOB.")

(defvar *job-queue* '()
  "The jobs waiting their turn, in every buffer, the oldest first.")

(defvar *starting-queued-job* nil
  "True while a job whose turn has come is started: it starts, not queued again.")

(defvar *queued-job-since* nil
  "While a job whose turn has come is started, its buffer's undo stack where it
was given (QUEUED-JOB-PLACE), else NIL.")

(defun editor-job-waker ()
  "A function waking the editor, for the thread of a job made now to call when
it has something new to show; NIL without an editor (TUATARA.RUNTIME:*JOB-WAKER*)."
  (let ((editor *editor*))
    (and editor (lambda () (wake-editor editor)))))

(defun running-jobs ()
  "The jobs running in the background, of every buffer, the oldest first."
  (remove-if-not #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*))

(defun killed-job-buffer-p (job)
  "True when the buffer JOB belongs to has been killed: what it did is no
longer said there."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (and buffer (not (live-buffer-p buffer)))))

(defun lines-uncounted-p (buffer)
  "True while BUFFER's lines are still being counted in the background, a file
of gigabytes just opened: a job that needs line numbers waits its turn until
they are (QUEUE-IF-BUSY with :COUNTED), rather than holding the editor. A
counting that failed is not waited for."
  (and (not (tuatara.buffer:buffer-indexed-p buffer))
       (not (eq (tuatara.buffer:buffer-local buffer :index-failed) (tuatara.buffer:buffer-text buffer)))))

(defun job-busy-p (buffer)
  "True while a job in the background holds BUFFER's queue
(TUATARA.RUNTIME:JOB-HOLDS-QUEUE-P): running, or ended and its result not yet
taken in."
  (and (find-if #'tuatara.runtime:job-holds-queue-p (jobs-of buffer)) t))

(defun say-in-background (words &optional then)
  "Say that the work WORDS name goes on in the background, as every kind of job
says it: Sorting in the background; carry on, THEN when given, and the key that
stops it."
  (message "~a in the background; carry on~@[, ~a~] (\\[stop-job] stops it)" words then))

(defparameter *job-verbs*
  '((:search "Searching" "Searched" "search" nil)
    (:type "Writing" "Wrote" "write" "at")
    (:append "Writing" "Wrote" "write" "at")
    (:change "Changing" "Changed" "change" nil)
    (:delete "Deleting" "Deleted" "delete" nil)
    (:cut "Cutting" "Cut" "cut" nil)
    (:wrap "Wrapping" "Wrapped" "wrap" nil)
    (:unwrap "Unwrapping" "Unwrapped" "unwrap" nil)
    (:paste "Pasting" "Pasted" "paste" nil)
    (:clear "Clearing" "Cleared" "clear" nil)
    (:number "Numbering" "Numbered" "number" nil)
    (:number-in "Numbering" "Numbered" "number" nil))
  "The words of every kind of work done at many places in the background: a
split's search and its writing out of sight, the cursors' and a rectangle's.
Each is (KIND DOING DONE VERB PREPOSITION): what it is doing (the list of
jobs, the mode line's questions, what is said as it starts or waits), what it
did (its report), what it does as a verb (Could not ...), and the word between
those and the places named, if any.")

(defun job-words (kind form &optional words)
  "The word of *JOB-VERBS* for KIND in FORM, :DOING (Cutting), :DONE (Cut) or
:VERB (cut); with WORDS, the places named, followed by them, after the kind's
preposition when it has one: Writing at the other lines."
  (destructuring-bind (doing done verb preposition) (rest (or (assoc kind *job-verbs*) (error "No words for ~s" kind)))
    (let ((word (ecase form (:doing doing) (:done done) (:verb verb))))
      (if words (format nil "~a~@[ ~a~] ~a" word preposition words) word))))

(defun job-done-words (seconds kept &optional detail)
  "How long a job in the background took, as every job's report says it: in
SECONDS s, followed by DETAIL when given, then how many of its changes were KEPT
as you edited them meanwhile, when any were."
  (format nil "in ~,1f s~@[: ~a~]~[~:;; ~:*~:d kept as you edited them~]" seconds detail kept))

(defun job-report (kind counted whole &key (refused (list 0)) (hidden (list 0)))
  "A job's REPORT (REWRITE) for doing KIND (JOB-WORDS) at many places in the
background, as every such job says how it ended: what was done to the places
COUNTED names (a function of the changes made giving them in words), in how
long, then how many were kept as edited meanwhile, how many were left as they
were because the text beside them was not what was deleted beside the places
in sight (the car of REFUSED) or because a fold hides part of what would go
\(the car of HIDDEN), and how many a fold hides any of were left alone
\(HIDDEN-LEFT-WORDS); or, with no change, that doing it at WHOLE, the places
in words, changed nothing."
  (lambda (job how changes seconds kept)
    (case how
      (:done (format nil "~a ~a~[~:;; ~:*~:d left as they were, the text beside them not the same~]~[~:;; ~:*~:d left as they were, partly hidden in a fold~]~a"
                     (job-words kind :done (funcall counted changes)) (job-done-words seconds kept) (car refused) (car hidden)
                     (hidden-left-words job)))
      (:none (format nil "~a changed nothing~a" (job-words kind :doing whole) (hidden-left-words job))))))

(defun spans-followed (buffer spans)
  "A function of no arguments giving SPANS of BUFFER, a vector of (START .
END), where they stand when it is called: anchors follow them through every
change made meanwhile, for a job that waits its turn."
  (let* ((version (tuatara.buffer:buffer-version buffer))
         (held (map 'simple-vector (lambda (span) (cons (tuatara.text:make-anchor version (car span) :bias :before)
                                                        (tuatara.text:make-anchor version (cdr span) :bias :after)))
                    spans)))
    (lambda ()
      (let ((now (tuatara.buffer:buffer-version buffer)))
        (map 'simple-vector (lambda (anchors) (cons (tuatara.text:anchor-position (car anchors) now)
                                                    (tuatara.text:anchor-position (cdr anchors) now)))
             held)))))

(defun take-in-soon (job seconds &optional words then)
  "Wait up to SECONDS for JOB, just started, to end: when it has, take it in
now, with the other jobs of its kind that have ended, so that quick work looks
done at once; else, with WORDS, say that it goes on in the background, and
THEN after it when given (SAY-IN-BACKGROUND)."
  (when (plusp seconds)
    (sb-thread:join-thread (tuatara.runtime:job-thread job) :timeout seconds :default nil))
  (if (tuatara.runtime:job-running-p job)
      (when words (say-in-background words then))
      (let ((kind (type-of job)))
        (tuatara.runtime:take-in-jobs (lambda (other) (typep other kind))))))

(defun result-refused-p (buffer what &optional (change 0))
  "True, once it is said, when BUFFER may no longer take the result of a job
that ended in the background, WHAT that result is (\"the replacement of a\"):
BUFFER became read-only meanwhile, or is shown in the hex view now and the
result would change its size by CHANGE octets, where octets are only typed
over (TUATARA.BUFFER:HEX-VIEW-REFUSES-P). Every job that puts its result in
a buffer asks this before it does, so that the result is left out calmly,
said in one way, rather than refused with an error."
  (let ((why (cond ((tuatara.buffer:buffer-local buffer :read-only) "became read-only")
                   ((tuatara.buffer:hex-view-refuses-p buffer change)
                    "turned to the hex view, which only types over octets,"))))
    (when why
      (message "~a ~a meanwhile; ~a was not applied: run it again" (tuatara.buffer:buffer-name buffer) why what)
      t)))

(defvar *edit-asked-about* nil
  "While an edit of a buffer is asked about (GUARD-JOB-EDITS), that buffer: its
jobs that end meanwhile are taken in once the question is answered, so that its
text stays as it was asked about.")

(defun jobs-tick (editor)
  "Tick function: take in the jobs in the background that have ended, the
oldest first (TUATARA.RUNTIME:TAKE-IN-JOBS), but for those of a buffer whose
edit is being asked about; true while some still run."
  (declare (ignore editor))
  (if *edit-asked-about*
      (tuatara.runtime:take-in-jobs (lambda (job) (not (eq (tuatara.runtime:job-buffer job) *edit-asked-about*))))
      (tuatara.runtime:take-in-jobs))
  (and (find-if #'tuatara.runtime:job-running-p tuatara.runtime:*jobs*) t))

(defun queued-in (buffer)
  "The jobs waiting their turn in BUFFER, the first first."
  (remove-if-not (lambda (job) (eq (queued-job-buffer job) buffer)) *job-queue*))

(defun ranges-shade (positions from to)
  "The stretches POSITIONS make taken two by two, (START END), as far as they
reach into the text from FROM below TO."
  (loop for (start end) on positions by #'cddr
        when (and end (< start to) (> end from) (< start end))
          collect (list (max start from) (min end to))))

(defun block-shade (buffer left right &key wide (tab-width (tuatara.display:tab-width-of buffer)))
  "A SHADE for a job waiting its turn over a rectangle's rows in BUFFER: from
the line its first offset starts to the one its second starts, the text
between the columns LEFT and RIGHT of each row shown, or with WIDE the whole
line; the columns on the screen, a tab TAB-WIDTH columns at most, or with
TAB-WIDTH NIL characters."
  (lambda (positions from to)
    (destructuring-bind (top bottom &rest more) positions
      (declare (ignore more))
      (let* ((rope (tuatara.buffer:buffer-text buffer))
             (start (max top (tuatara.text:rope-line-start rope (min from (tuatara.text:rope-bytes rope)))))
             (end (min (1+ bottom) to))
             (ranges '()))
        (when (< start end)
          (apply
           #'tuatara.text:map-line-slices
           (lambda (line s e start-column end-column line-end)
             (declare (ignore start-column end-column))
             (let ((s (if wide line s)) (e (if wide line-end e)))
               (when (and (< s e) (< s to) (> e from))
                 (push (list (max s from) (min e to)) ranges))))
           rope start end (if wide 0 left) (if wide 0 right)
           (tuatara.display:screen-slice-keys (buffer-crlf-p buffer) tab-width)))
        (nreverse ranges)))))

(defun queue-if-busy (buffer words positions starter &key (shade #'ranges-shade) counted)
  "When a job runs in BUFFER, or others wait their turn there, or with COUNTED
its lines are still being counted (LINES-UNCOUNTED-P), queue the job STARTER
starts, saying WORDS of it, and return true; else return NIL, for it to start
at once. POSITIONS, each (OFFSET . BIAS), are what the job reads its text by:
anchors follow them while it waits, and STARTER is called with where they
stand when its turn comes. SHADE, a function of those offsets and the offsets
FROM and TO of the text shown, gives the stretches it will change, painted
meanwhile; by default the offsets taken two by two."
  (when (and (not *starting-queued-job*)
             (or (job-busy-p buffer) (queued-in buffer) (and counted (lines-uncounted-p buffer))))
    (let* ((version (tuatara.buffer:buffer-version buffer))
           ;; a job asked for by a thorough command stays thorough when its turn comes
           (starter (if tuatara.syntax:*regex-thorough*
                        (lambda (&rest positions)
                          (let ((tuatara.syntax:*regex-thorough* t)) (apply starter positions)))
                        starter))
           (job (make-queued-job buffer words starter
                                 (mapcar (lambda (p) (tuatara.text:make-anchor version (car p) :bias (cdr p))) positions)
                                 shade counted (tuatara.buffer:make-undo-place buffer))))
      (setf *job-queue* (append *job-queue* (list job)))
      (tuatara.runtime:debug-report :job "~a in ~a queued, ~:r in line" words (tuatara.buffer:buffer-name buffer)
                                    (length (queued-in buffer)))
      (message "Queued, ~:r in line: ~a (\\[list-jobs] lists the jobs)" (length (queued-in buffer)) words)
      t)))

(defun job-begins-where-given (buffer low high &key untouched)
  "Where a job changing BUFFER from LOW below HIGH begins: on the text now, or,
for a job queued whose turn has come, on the text as it stood where it was
given (TUATARA.BUFFER:BUFFER-TEXT-AT), its change to stand there in the undo
history, before the changes made since. Five values: that text, LOW and HIGH
there, the undo stack there, and the changes made since, the batches oldest
first, for the job to follow as made meanwhile. On the text now when that
text can no longer be told, or with UNTOUCHED when a change made since
touched the job's text (TUATARA.BUFFER:SPAN-AFTER-BATCH)."
  (let ((stack (tuatara.buffer:buffer-undo-stack buffer)))
    (when (and *queued-job-since* (not (eq *queued-job-since* stack)))
      (multiple-value-bind (text made back) (tuatara.buffer:buffer-text-at buffer *queued-job-since*)
        (when text
          (let* ((from (funcall back low :before)) (to (funcall back high :after)) (span (cons from to)))
            (when (or (not untouched)
                      (loop for batch in made always (setf span (tuatara.buffer:span-after-batch span batch))))
              (return-from job-begins-where-given (values text from to *queued-job-since* made)))))))
    (values (tuatara.buffer:buffer-text buffer) low high stack '())))

(defun queued-positions (job)
  "Where the anchors of JOB, waiting its turn, stand in its buffer now."
  (let ((version (tuatara.buffer:buffer-version (queued-job-buffer job))))
    (mapcar (lambda (anchor) (tuatara.text:anchor-position anchor version)) (queued-job-anchors job))))

(defun job-queue-tick (editor)
  "Tick function: drop the jobs of buffers killed, and start the first job
waiting in each buffer that has none running, its result taken in, once the
buffer's lines are counted when it needs them; true while jobs wait, or one
has started."
  (declare (ignore editor))
  (let ((started nil))
    (dolist (job (copy-list *job-queue*))
      (let ((buffer (queued-job-buffer job)))
        (cond ((not (live-buffer-p buffer))
               (setf *job-queue* (remove job *job-queue*)))
              ((and (not (job-busy-p buffer)) (eq job (first (queued-in buffer)))
                    (not (and (queued-job-counted job) (lines-uncounted-p buffer))))
               (setf *job-queue* (remove job *job-queue*))
               (tuatara.buffer:forget-undo-place buffer (queued-job-place job))
               (tuatara.runtime:debug-report :job "~a in ~a: its turn" (queued-job-words job) (tuatara.buffer:buffer-name buffer))
               (setf started t)
               (let ((*starting-queued-job* t)
                     (*queued-job-since* (car (queued-job-place job))))
                 (apply (queued-job-starter job) (queued-positions job)))))))
    (and (or started *job-queue*) t)))

(defun processing-job (buffer)
  "The job running in BUFFER whose progress its mode line shows
\(TUATARA.RUNTIME:JOB-SHOWS-PROCESSING-P), the one holding its queue first; NIL
when none runs there."
  (let ((running (remove-if-not #'tuatara.runtime:job-shows-processing-p (jobs-of buffer :running t))))
    (or (find-if #'tuatara.runtime:job-holds-queue-p running) (first running))))

(defvar *outside-work-functions* '()
  "Functions giving the work other programs do for Tuatara: called with a
buffer, the work done for it, else with no argument, all of it; each a list
of (WORDS PERCENT WHO), WORDS what is being done, PERCENT how far it has got
or NIL, WHO what does it (a language server indexing a project). The mode
line shows it as a job of the buffer, the list of jobs (\\[list-jobs]) what it is.")

(defun outside-work (&optional (buffer nil buffer-p))
  "The work *OUTSIDE-WORK-FUNCTIONS* give for BUFFER, or with none given all of it."
  (loop for f in *outside-work-functions*
        append (if buffer-p (funcall f buffer) (funcall f))))

(defvar *processing-indicator* "P"
  "What the mode line shows first, before [-] and the signs of directories
kept read (*INDEXING-INDICATOR*), always in the same place: in
:mode-line-sign while a job runs or waits its turn in the buffer, or another
program works for it (a language server indexing), else faint, in
:mode-line-sign-idle, so that nothing after it moves as work starts and
ends. What each job does, and how far it has got, are in the list of jobs
\\(\\[list-jobs]). P (for processing) as standard, which any font has; any
text, such as ⚙, or \"\" (nf-fa-cog, U+F013) for a Nerd Font; NIL to show
nothing.")

(defun buffer-working-p (buffer)
  "True while work for BUFFER runs or waits its turn: a job whose progress its
mode line shows (PROCESSING-JOB), the work of another program for it
\(*OUTSIDE-WORK-FUNCTIONS*), or a job queued there."
  (and (or (processing-job buffer) (outside-work buffer) (find buffer *job-queue* :key #'queued-job-buffer)) t))

(defun processing-mode-line (editor buffer)
  "The mode line's sign of work for BUFFER, *PROCESSING-INDICATOR*: lit while
it runs or waits (BUFFER-WORKING-P), else faint (MODE-LINE-SIGN); NIL with no
indicator."
  (declare (ignore editor))
  (and *processing-indicator* (mode-line-sign *processing-indicator* (buffer-working-p buffer))))

(defun queue-highlights (editor window rows)
  "Job highlight function: the text the jobs waiting their turn in WINDOW's
buffer will change, in ROWS only, in :job-waiting, only what the rows show
looked at (SHOWN-RANGES: not the lines search filters hide between them); a
running job's shading is laid over it."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (when (and (plusp (length rows)) (queued-in buffer))
      (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
            nconc (loop for job in (queued-in buffer)
                        nconc (loop for (start end) in (funcall (queued-job-shade job) (queued-positions job) from to)
                                    collect (list start end :job-waiting)))))))

(defun drop-queued-job (job)
  "Take JOB out of the queue, never to run."
  (setf *job-queue* (remove job *job-queue*))
  (tuatara.buffer:forget-undo-place (queued-job-buffer job) (queued-job-place job))
  (tuatara.runtime:debug-report :job "~a in ~a dropped from the queue" (queued-job-words job)
                                (tuatara.buffer:buffer-name (queued-job-buffer job))))

(defun jobs-question-words (running waiting)
  "The jobs RUNNING, each as BUFFER: state, and how many are WAITING their
turn, as the questions before a kill or an exit list them."
  (format nil "~{~a~^, ~}~:[~;, ~]~[~:;~:*~d queued~]"
          (mapcar (lambda (job) (format nil "~a: ~a" (tuatara.runtime:job-buffer-name job)
                                        (tuatara.runtime:job-state-words job)))
                  running)
          (and running (plusp waiting)) waiting))

(defun jobs-ask (running waiting then)
  "Ask once before the jobs RUNNING are stopped, or waited for when they save
\(TUATARA.RUNTIME:JOB-AT-EXIT), and the WAITING jobs queued dropped, THEN (kill
the buffer, exit) being done: In the background: *scratch*: Sorting 42%, 1
queued; stop them, then exit? True to go on."
  (let ((waits (find :wait running :key #'tuatara.runtime:job-at-exit))
        (stops (or (find :stop running :key #'tuatara.runtime:job-at-exit) (plusp waiting)))
        (many (> (+ (length running) waiting) 1)))
    (y-or-n-p* (format nil "In the background: ~a; ~:[stop ~:[it~;them~]~*~;~*~:[wait for the saving~;wait for the saving, stop the rest~]~], then ~a?"
                          (jobs-question-words running waiting) waits many (and stops t) then))))

(defun wait-for-saving (running)
  "Wait for those of the jobs RUNNING that are waited for at a kill or an exit
\(TUATARA.RUNTIME:JOB-AT-EXIT :WAIT), the saves (WAIT-FOR-SAVES): true when
they all ended, or there are none."
  (let ((waits (remove :stop running :key #'tuatara.runtime:job-at-exit)))
    (or (null waits) (wait-for-saves waits))))

(defun jobs-kill-query (buffer)
  "Kill-buffer query: when jobs of BUFFER run or wait their turn, ask once
whether to stop them, a save waited for, and kill the buffer; true to go on.
SETTLE-JOBS stops them once the buffer is killed, and those that only show
something (TUATARA.RUNTIME:JOB-AT-EXIT :QUIET) without asking."
  (let ((running (remove :quiet (jobs-of buffer :running t) :key #'tuatara.runtime:job-at-exit))
        (waiting (length (queued-in buffer))))
    (or (and (null running) (zerop waiting))
        (and (jobs-ask running waiting "kill the buffer")
             (or (wait-for-saving running)
                 (progn (message "Not killed: ~a is still being saved" (tuatara.buffer:buffer-name buffer)) nil))))))

(defun jobs-quit-query ()
  "Quit query: when jobs run or wait their turn, ask once whether to stop
them, the saves waited for at once, and exit; the others are stopped and those
waiting dropped once the exit is certain, those that only show something
\(TUATARA.RUNTIME:JOB-AT-EXIT :QUIET) without asking. NIL to stay."
  (let* ((all (running-jobs))
         (running (remove :quiet all :key #'tuatara.runtime:job-at-exit))
         (waiting (length *job-queue*)))
    (or (and (null all) (zerop waiting))
        (and (or (and (null running) (zerop waiting))
                 (jobs-ask running waiting "exit"))
             (or (wait-for-saving running)
                 (progn (message "Not left: saves are still running") nil))
             (let ((stops (remove :wait all :key #'tuatara.runtime:job-at-exit)))
               (lambda ()
                 (mapc #'tuatara.runtime:job-stop stops)
                 (mapc #'drop-queued-job (copy-list *job-queue*))))))))

(defun settle-jobs (buffer)
  "Kill-buffer function: stop the jobs running in BUFFER and wait for them to
end, so that none reads the buffer's text once its file is unmapped; one that
does not stop in time (TUATARA.RUNTIME:*STOP-WAIT*) is left to end on its own,
the file kept mapped for it (BUFFER-LENT-P). A save is waited for, not stopped
\(TUATARA.IO::SETTLE-FILE-WORK)."
  (dolist (job (jobs-of buffer :running t))
    (when (and (member (tuatara.runtime:job-at-exit job) '(:stop :quiet))
               (not (tuatara.runtime:stop-and-settle job)))
      (setf (tuatara.buffer:buffer-lent-p buffer) t))))

(define-command stop-job ()
  "Stop the work running in the background in the current buffer, after
asking: a replacement or count, a search for a split, a change at a rectangle
or at every match, a sort or filter, a SHA-256 sum, a count of words, dynamic
blocks made, a save, a list's copying, moving or deleting, an agenda worked
out, a calendar's days marked. The buffer stays as it is, and a file being
saved as it was (a file written in place is finished first, since stopping
half-way would spoil it). The jobs waiting their turn behind it then start, one
after another; \\[list-jobs] lists them, and drops those not wanted. Changing
the text a sort, a filter or dynamic blocks being made work on asks first
whether to stop them, since the change would spoil their work. A job already
asked to stop, and ending, is not asked about again."
  (let* ((running (remove-if (lambda (job) (car (tuatara.runtime:job-cancel job)))
                             (jobs-of (current-buffer) :running t)))
         (job (or (find-if #'tuatara.runtime:job-holds-queue-p running) (first running))))
    (cond ((null job) (message "Nothing is running in this buffer"))
          ((y-or-n-p* (tuatara.runtime:job-stop-question job))
           (tuatara.runtime:job-stop job)
           (message "Stopping")))))

;;; Edits that would spoil a job's work

(defun guard-job-edits (buffer edits)
  "Before-edit function (TUATARA.BUFFER:*BEFORE-EDIT-FUNCTIONS*): when EDITS of
BUFFER, made by a command, touch text a job running there works on and would
spoil its work (TUATARA.RUNTIME:JOB-GUARDED-SPANS), a sort or filter over its
lines or dynamic blocks being made, ask whether to stop the job. Yes stops it
and lets the edits through; no refuses them, the text left as it was and the
job going on, and the command's later edits are refused without asking. Asked
once a command. Nothing is asked of the edits a job's own result makes as it
is taken in, of those made outside a command or while the minibuffer reads,
nor of edits elsewhere: the job then gives up its work by itself when they
spoil it. O(jobs of BUFFER) when none of them guards text."
  (let ((notes *command-notes*))
    (when (and notes (null tuatara.runtime:*job-taken-in*) *editor* (not (editor-minibuffer-active *editor*)))
      (let* ((guards (tuatara.runtime:job-guards buffer))
             (job (and guards
                       (loop for edit across edits
                             for start = (tuatara.text:edit-start edit) and end = (tuatara.text:edit-end edit)
                             thereis (and (or (< start end) (plusp (tuatara.text:edit-insert-length edit)))
                                          (tuatara.runtime:guard-touched guards start end))))))
        (when job
          (when (getf (car notes) :spoiling-edits-refused)
            (error "~a" (tuatara.runtime:job-guard-refusal job)))
          (let ((version (tuatara.buffer:buffer-version buffer)))
            (if (let ((*edit-asked-about* buffer))
                  (y-or-n-p* (tuatara.runtime:job-guard-question job)))
                (tuatara.runtime:job-stop job)
                (progn (setf (getf (car notes) :spoiling-edits-refused) t)
                       (error "~a" (tuatara.runtime:job-guard-refusal job))))
            (unless (eq version (tuatara.buffer:buffer-version buffer))
              (error "Not changed: the text changed meanwhile; do it again"))))))))

;;; The list of jobs

(define-major-mode jobs-mode marked-list-mode
  "The list of jobs in the background (\\[list-jobs]): those running in each
buffer, with how far they have got, and those waiting their turn, in the order
they will run. \\<jobs-mode>\\[list-mark-for-deletion] marks the job on the line for deletion,
\\[list-mark-entry] marks it, \\[list-unmark-entry] unmarks it; \\[list-execute] drops the jobs marked for
deletion that wait and stops those running, after asking, \\[list-delete-marked] the
marked ones, or the one on the line; \\[revert-buffer] lists them again, as
the list does by itself while it is shown, when they have changed;
\\[quit-window] or \\`h' returns."
  :lighter "Jobs" :interactive nil)

(defparameter *jobs-title-lines* 2
  "The lines of *Jobs* above its first job: the column titles and the dashes under them.")

(defun jobs-entries ()
  "What *Jobs* lists, in order: each running job (TUATARA.RUNTIME:JOB), then
its buffer's jobs waiting their turn, each a QUEUED-JOB; then the jobs waiting
in buffers with none running."
  (let ((entries '()))
    (dolist (job (running-jobs))
      (push job entries)
      (dolist (queued (queued-in (tuatara.runtime:job-buffer job)))
        (unless (member queued entries) (push queued entries))))
    (dolist (job *job-queue*)
      (unless (member job entries) (push job entries)))
    (nreverse entries)))

(defparameter *jobs-columns*
  '(("  " :width 2) ("Buffer" :width 20) ("State" :width 26 :shade t) ("Job"))
  "The columns of *Jobs*, as TABLE-HEADING-PIECES takes them: the mark (* or D),
the buffer, the state, shaded as the job's text is, and what the job does.")

(defun jobs-render (list &optional (point :line))
  "Fill LIST, a *Jobs* buffer, with a line for each job (JOBS-ENTRIES), marks
kept on the jobs still there, point on the same job or line, at its start, or
with POINT :keep at its column."
  (let* ((key (jobs-key list))
         (entries (jobs-entries))
         (place (make-hash-table :test 'eq)))
    (render-table-list
     list *jobs-columns* entries
     (lambda (entry mark section)
       (declare (ignore section))
       (if (queued-job-p entry)
           (let ((buffer (queued-job-buffer entry)))
             (list (format nil " ~a" (list-mark-sign mark))
                   (cons (tuatara.buffer:buffer-name buffer) (list-mark-face mark))
                   (cons (format nil "waiting, ~:r in line" (incf (gethash buffer place 0))) :job-waiting)
                   (queued-job-words entry)))
           (list (format nil " ~a" (list-mark-sign mark))
                 (cons (tuatara.runtime:job-buffer-name entry) (list-mark-face mark))
                 (cons (tuatara.runtime:job-state-words entry) :job-queued)
                 (tuatara.runtime:job-description entry))))
     ;; what other programs do, listed after, never marked
     :after (append (loop for (words percent who) in (outside-work)
                          append (table-row-pieces *jobs-columns*
                                                   (list "  " who (cons (if percent (format nil "~d%" percent) "working") :job-queued)
                                                         words)))
                    (and (null entries) (null (outside-work)) (list (format nil "~%   No jobs in the background~%"))))
     :point point :first-line *jobs-title-lines* :key key
     :refill (lambda (list) (jobs-render list :keep)))))

(defun jobs-key (list)
  "What *Jobs* shows, cheaply: each job listed, with its buffer's name and the
words that say how far it has got and what it does, and the work of other
programs; another key means the list is out of date (KEEP-SHOWN-CURRENT)."
  (declare (ignore list))
  (append (mapcar (lambda (entry)
                    (if (queued-job-p entry)
                        (list entry (tuatara.buffer:buffer-name (queued-job-buffer entry)) (queued-job-words entry))
                        (list entry (tuatara.runtime:job-buffer-name entry) (tuatara.runtime:job-state-words entry)
                              (tuatara.runtime:job-description entry))))
                  (jobs-entries))
          (outside-work)))

(keep-shown-current 'jobs-mode 'jobs-key)

(define-command list-jobs ()
  "List the jobs in the background in *Jobs*: in each buffer those running,
with how far they have got, and those waiting their turn, in the order they
will run, their text painted in the buffer meanwhile. \\<jobs-mode>\\[list-mark-for-deletion]
marks the job on the line for deletion and \\[list-unmark-entry] unmarks it; \\[list-execute]
drops the jobs marked for deletion that wait, and stops those running, after
asking, \\[list-delete-marked] the marked ones (\\[list-mark-entry]), or the one on the line.
\\[revert-buffer] lists them again, as the list does by itself while it is
shown, when they have changed; \\[quit-window] or \\`h' returns."
  (let ((list (buffer-in-mode "*Jobs*" 'jobs-mode)))
    (jobs-render list)
    (show-buffer list)))

(defmethod list-entry-noun ((mode jobs-mode))
  "A job."
  "job")

(defmethod delete-entries ((mode jobs-mode) list jobs mark)
  "Drop JOBS waiting their turn and stop those running, after asking: a job
dropped never runs, one stopped leaves the text as it was. The jobs waiting
after one stopped move up."
  (declare (ignore mark))
  (let ((waiting (remove-if-not #'queued-job-p jobs))
        (running (remove-if #'queued-job-p jobs)))
    (when (y-or-n-p* (format nil "~@[Drop ~d waiting job~:p~]~:[~; and ~]~@[stop ~d running~]?"
                                (and waiting (length waiting)) (and waiting running) (and running (length running))))
      (mapc #'drop-queued-job waiting)
      (mapc #'tuatara.runtime:job-stop running)
      (unmark-list list)
      (message "~@[Dropped ~d waiting job~:p~]~:[~;; ~]~@[stopping ~d~]"
               (and waiting (length waiting)) (and waiting running) (and running (length running))))))

(defun install-jobs ()
  "Have the jobs in the background wake the editor and be taken in as they
end, the jobs waiting their turn start as each buffer's job ends, their text
painted, and the jobs running or waiting asked about before their buffer is
killed or the editor left, and stopped when their buffer is killed; and an
edit by a command that would spoil the work of a job running over the text it
touches asked about first (GUARD-JOB-EDITS). The mode line's sign of the work
\(PROCESSING-MODE-LINE) is shown with those of directories kept read
\(MODE-LINE-SIGNS)."
  (setf tuatara.runtime:*job-waker* 'editor-job-waker)
  (pushnew 'job-queue-tick *tick-functions*)
  (pushnew 'jobs-tick *tick-functions*)
  (pushnew 'queue-highlights *job-highlight-functions*)
  (pushnew 'jobs-kill-query *kill-buffer-query-functions*)
  (pushnew 'settle-jobs tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'jobs-quit-query *quit-query-functions*)
  (pushnew 'guard-job-edits tuatara.buffer:*before-edit-functions*)
  (define-key (mode-keymap 'jobs-mode) "h" 'quit-window))
