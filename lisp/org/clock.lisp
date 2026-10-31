;;;; lisp/org/clock.lisp --- Org's clock: time spent on a task, kept in its LOGBOOK, shown while it runs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-c C-x C-i clocks in on the headline point is under: a line CLOCK:
;;;; [2026-10-01 Thu 17:30] goes into its :LOGBOOK: drawer, the newest first,
;;;; after its planning line and properties, as Org puts it. C-c C-x C-o
;;;; clocks out, the line finished with the time it ended and how long it
;;;; ran (=> 1:12); C-c C-x C-q cancels it, the line taken away; C-c C-x C-j
;;;; goes to the task. While it runs the mode line and the window's title
;;;; show [0:22] (Task); quitting asks whether to clock out first.

(in-package #:tuatara.defaults)

(defvar *org-clock-into-drawer* t
  "True for clock lines to go into a :LOGBOOK: drawer of their headline;
NIL for them to go right under it.")

(defstruct (org-clock (:conc-name org-clock-))
  "The clock running: the BUFFER of the task, an anchor on the start of its
CLOCK: LINE, when it STARTED (universal time, whole minutes), the TASK's
title and the PATH of its file, where its line is found again when the
buffer is killed (ORG-CLOCK-LIVE-BUFFER)."
  buffer line started task path)

(defvar *org-clock* nil
  "The clock running, an ORG-CLOCK, or NIL.")

(defun org-duration-text (minutes)
  "MINUTES as a clock line writes its duration: \" 1:12\", \"39:35\"."
  (multiple-value-bind (hours minutes) (floor (max 0 minutes) 60)
    (format nil "~2d:~2,'0d" hours minutes)))

(defun org-minute (time)
  "The universal TIME without its seconds, as a timestamp keeps it."
  (* 60 (floor time 60)))

(defun org-clock-string (clock now)
  "What the mode line shows of CLOCK at the universal time NOW: [0:22] (Task)."
  (format nil "[~a] (~a)" (string-left-trim " " (org-duration-text (floor (- now (org-clock-started clock)) 60)))
          (org-clock-task clock)))

(defun org-clock-line-start (clock)
  "Where CLOCK's line begins now in its buffer (octets), or NIL when it is gone."
  (let ((buffer (org-clock-buffer clock)))
    (and (live-buffer-p buffer)
         (ignore-errors (tuatara.text:anchor-position (org-clock-line clock) (tuatara.buffer:buffer-version buffer))))))

(defun org-clock-hold (clock buffer octet)
  "Hold CLOCK to its CLOCK: line beginning at OCTET of BUFFER, BUFFER's file
kept as its own, where it is found again (ORG-CLOCK-LIVE-BUFFER)."
  (setf (org-clock-buffer clock) buffer
        (org-clock-path clock) (tuatara.buffer:buffer-file buffer)
        (org-clock-line clock) (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer) octet :bias :after)))

(defun org-clock-live-buffer (clock)
  "CLOCK's buffer, live: when the buffer it ran in was killed, its file
visited again, unseen, and its open CLOCK: line, of the time it started,
held again (ORG-CLOCK-HOLD). NIL, the clock stopped and that said, when the
file or the line is no longer there, or the buffer had no file."
  (let ((buffer (org-clock-buffer clock))
        (path (org-clock-path clock)))
    (if (live-buffer-p buffer)
        buffer
        (let* ((buffer (and path (eq (tuatara.io:path-kind path) :regular) (tuatara.io:visit-file path)))
               (text (and buffer (nth-value 1 (org-tree-now buffer))))
               (line (format nil "CLOCK: ~a" (org-timestamp-text (org-clock-started clock))))
               (at (and text (loop for at = (search line text) then (search line text :start2 (1+ at))
                                   while at
                                   when (org-read-clock-line text (string-line-start text at))
                                     do (multiple-value-bind (from to) (org-read-clock-line text (string-line-start text at))
                                          (when (and from (not to)) (return at)))))))
          (if at
              (progn (org-clock-hold clock buffer (org-char->byte buffer (string-line-start text at)))
                     buffer)
              (progn (when (eq *org-clock* clock) (setf *org-clock* nil))
                     (message "The clock's open CLOCK: line is no longer ~:[there~;~:*in ~a~]: the clock stopped (~a)"
                              (and path (abbreviated-path path)) (org-clock-task clock))
                     nil))))))

(defparameter *org-clocks-named* 5
  "How many clocks left open, besides the one taken up, are named when
Tuatara finds them (ORG-CLOCK-TAKE-UP); the rest are counted.")

(defun org-clock-take-up (open)
  "Of OPEN, the clocks left running (ORG-CLOCKS-LEFT-OPEN), have the one
started last run again from when it started, unless a clock runs already,
and say so; name the others left open, in *Notifications* too, so that none
is forgotten with only its start. The clock running, or NIL."
  (let* ((last (first open))
         (running (or *org-clock*
                      (and last (progn (setf *org-clock* last) (org-clock-live-buffer last)) last)))
         (others (remove-if (lambda (clock) (and running (equal (org-clock-path clock) (org-clock-path running))
                                                 (eql (org-clock-started clock) (org-clock-started running))))
                            open)))
    (when (and running (eq running last))
      (message "The clock open since ~a runs again: ~a" (format-time "%F %R" (org-clock-started running)) (org-clock-task running)))
    (when others
      (let ((words (format nil "~{~a~^; ~}~@[; and ~d more~]"
                           (loop for clock in others repeat *org-clocks-named*
                                 collect (format nil "~a since ~a" (org-clock-task clock) (format-time "%F %R" (org-clock-started clock))))
                           (and (> (length others) *org-clocks-named*) (- (length others) *org-clocks-named*)))))
        (notify (format nil "~d CLOCK: line~:p left open, with only a start" (length others)) words :source "Clock" :desktop nil)
        (message "Open CLOCK: lines left as well, with only a start: ~a" words)))
    running))

(defun org-clock-running ()
  "The clock running: *ORG-CLOCK*, else the one left running in the agenda
files that was started last (ORG-CLOCK-TAKE-UP), as after a restart,
running again from when it started; NIL when there is none."
  (or *org-clock* (org-clock-take-up (org-clocks-left-open))))

(defun org-clock-open-line (clock)
  "CLOCK's line in its buffer as two values, its start and end (octets), when
it is still an open CLOCK: line; else NIL."
  (let* ((buffer (org-clock-buffer clock))
         (start (org-clock-line-start clock)))
    (when start
      (let* ((end (tuatara.buffer:buffer-line-end buffer start))
             (line (tuatara.buffer:buffer-string buffer :start start :end end)))
        (multiple-value-bind (from to duration) (org-read-clock-line line)
          (when (and from (not to) (not duration))
            (values start end)))))))

(defun org-clock-in-here (buffer &key (now (org-now)))
  "Clock in on the task at BUFFER's point, the clock running clocked out
first: the list item with a checkbox whose first line point is on, else the
headline it is under. CLOCK: and the time go at the top of its :LOGBOOK:
drawer (ORG-LOG-EDIT), made when it has none; a headline's with no drawer
when *ORG-CLOCK-INTO-DRAWER* is NIL, though BUFFER is narrowed to a part
below the headline. Saved when that is BUFFER's only change
\(ORG-SAVE-CHANGED), so that the clock runs again after a restart."
  ;; the clock running stopped first: its line changes the text the new one goes into
  (when *org-clock* (org-clock-out-now :now now))
  (multiple-value-bind (tree text) (org-tree-now buffer)
    (let* ((*org-current-text* (or text ""))
           (position (org-point-char buffer))
           (item (and tree (org-checkbox-item-at tree text position)))
           (owner (or item (and tree (org-current-headline tree position)))))
      (if (null owner)
          (message "Not under a headline: clock in on a task")
          (let* ((started (org-minute now))
                 (line (format nil "CLOCK: ~a" (org-timestamp-text started)))
                 (title (if item (org-checkbox-title item text) (or (org-property owner :raw-value) "")))
                 (edit (org-log-edit owner text (list line) :drawer (or item *org-clock-into-drawer*)))
                 (at (search line (third edit)))
                 ;; where the clock's line begins in the text after the edit
                 (line-start (+ (first edit) (string-line-start (third edit) at)))
                 (modified (tuatara.io:changes-unsaved-p buffer)))
            ;; the task's headline may be above the part of the buffer shown
            (tuatara.buffer:with-narrowing-lifted (buffer)
              (org-apply-edits buffer (list edit) :point position))
            (setf *org-clock* (make-org-clock :started started :task title))
            (org-clock-hold *org-clock* buffer (org-char->byte buffer line-start))
            (org-save-changed buffer modified :existing t)
            (message "Clock starts at ~a: ~a" (format-time "%R" started) title))))))

(define-command org-clock-in ()
  "Clock in on the task point is under (\\`C-c C-x C-i'): CLOCK: and the time go into its :LOGBOOK: drawer, made
when it has none, after its planning line and properties; a clock running is
clocked out first. While it runs, the mode line and the window's title show
how long, and the task. On a list item's line with a checkbox, on that item:
its CLOCK: line goes into a :LOGBOOK: drawer under it. The note is saved at
once when the clock's line is its only change, so that the clock runs again
after a restart; with changes of yours not saved in it, it is left for you
to save."
  (org-clock-in-here (current-buffer)))

(defun org-clock-out-now (&key (now (org-now)))
  "Clock out of the clock running: its line finished with the time it ended
and how long it ran (=> 1:12), and saved when that is its buffer's only
change (ORG-SAVE-CHANGED). The line is finished wherever it is, though its
buffer is narrowed to another part; the clock stops only once it is."
  (let ((clock *org-clock*))
    (unless (org-clock-live-buffer clock)
      (return-from org-clock-out-now nil))
    ;; a buffer that may not be written keeps the clock running, its line open
    (check-writable (org-clock-buffer clock))
    (multiple-value-bind (start end) (org-clock-open-line clock)
      (if (null start)
          (progn (setf *org-clock* nil)
                 (message "The clock's line is gone: nothing to finish"))
          (let* ((buffer (org-clock-buffer clock))
                 (ended (org-minute now))
                 (minutes (floor (- ended (org-clock-started clock)) 60))
                 (line (tuatara.buffer:buffer-string buffer :start start :end end))
                 (indent (subseq line 0 (position #\C line)))
                 (new (format nil "~aCLOCK: ~a--~a => ~a" indent (org-timestamp-text (org-clock-started clock))
                              (org-timestamp-text ended) (org-duration-text minutes)))
                 (point (tuatara.buffer:buffer-point buffer))
                 (modified (tuatara.io:changes-unsaved-p buffer)))
            ;; the task's line may be outside the part of its buffer shown
            (tuatara.buffer:with-narrowing-lifted (buffer)
              (tuatara.buffer:buffer-replace buffer start end new))
            ;; point on the line stays on it, as far in as the new line goes
            (setf (tuatara.buffer:buffer-point buffer)
                  (if (> point end) (+ point (- (tuatara.text:string-octet-length new) (- end start))) (min point (+ start (tuatara.text:string-octet-length new)))))
            ;; the clock is let go only once its line is finished
            (setf *org-clock* nil)
            (org-save-changed buffer modified :existing t)
            (message "Clock stopped at ~a after ~a: ~a" (format-time "%R" ended)
                     (string-left-trim " " (org-duration-text minutes)) (org-clock-task clock)))))))

(define-command org-clock-out ()
  "Clock out (\\`C-c C-x C-o', from any buffer \\`C-x a o'):
the clock's line finished with the time it ended and how long it ran; with
none running, of the one left open in the agenda files (ORG-CLOCK-RUNNING).
The note is saved at once when the clock's line is its only change, so
that the clock stays stopped after a restart; with changes of yours not
saved in it, it is left for you to save."
  (if (org-clock-running) (org-clock-out-now) (message "No clock is running")))

(define-command org-clock-cancel ()
  "Stop the clock running and take its line away
(\\`C-c C-x C-q'): its :LOGBOOK: drawer too, when nothing else is in it;
saved when that is its buffer's only change, as \\[org-clock-out] does."
  (let ((clock (org-clock-running)))
    (cond
      ((null clock) (message "No clock is running"))
      ((null (org-clock-live-buffer clock)))
      (t
        (check-writable (org-clock-buffer clock))
        (setf *org-clock* nil)
        (multiple-value-bind (start end) (org-clock-open-line clock)
          (if (null start)
              (message "The clock's line is gone: clock cancelled")
              (let* ((buffer (org-clock-buffer clock))
                     (rope (tuatara.buffer:buffer-text buffer))
                     (size (tuatara.buffer:buffer-size buffer))
                     (end (min size (1+ (tuatara.text:rope-line-end rope end))))
                     (before (tuatara.buffer:buffer-line-start buffer (max 0 (1- start))))
                     (above (and (plusp start) (tuatara.buffer:buffer-string buffer :start before :end start)))
                     (below (tuatara.buffer:buffer-string buffer :start end :end (min size (tuatara.buffer:buffer-line-end buffer end))))
                     (blank '(#\Space #\Tab #\Return #\Newline))
                     (empty-drawer (and above (string= (string-trim blank above) ":LOGBOOK:")
                                        (string= (string-trim blank below) ":END:")))
                     (modified (tuatara.io:changes-unsaved-p buffer)))
                (if empty-drawer
                    (tuatara.buffer:buffer-delete buffer before (min size (1+ (tuatara.text:rope-line-end rope end))))
                    (tuatara.buffer:buffer-delete buffer start end))
                (org-save-changed buffer modified :existing t)
                (message "Clock cancelled: ~a" (org-clock-task clock)))))))))

(define-command org-clock-goto ()
  "Go to the task the clock runs on (\\`C-c C-x C-j', from any buffer \\`C-x a j'):
its file opened again when its buffer was killed; with none running, one
left open in the agenda files, as after a restart, runs again
\(ORG-CLOCK-RUNNING)."
  (let* ((clock (org-clock-running))
         (start (and clock (org-clock-live-buffer clock) (org-clock-line-start clock))))
    (if (null start)
        (unless (and clock (null *org-clock*)) (message "No clock is running"))
        (let ((buffer (org-clock-buffer clock)))
          (show-buffer buffer)
          ;; the task's headline: the nearest line above beginning with *, the clock's line shown
          (goto-shown buffer (loop for line = (tuatara.buffer:buffer-line-start buffer start)
                                     then (tuatara.buffer:buffer-line-start buffer (1- line))
                                   when (or (zerop line) (eql (tuatara.buffer:buffer-char-at buffer line) #\*)) return line)
                      :open start)))))

(defun org-update-clock-line (buffer text position)
  "When the line of the character POSITION of BUFFER (TEXT its text) is a
finished CLOCK: line, write its duration again from its two times; true when it was."
  (let* ((start (string-line-start text position))
         (line (org-line-at text start)))
    (multiple-value-bind (from-text to-text) (org-read-clock-line line)
      (let ((from (and to-text (org-ts-universal-time from-text))) (to (and to-text (org-ts-universal-time to-text))))
        (when (and from to)
          (let* ((minutes (floor (- to from) 60))
                 (new (format nil "~a => ~a" (subseq line 0 (+ (search to-text line :from-end t) (length to-text)))
                              (org-duration-text minutes))))
            (org-replace-chars buffer start (+ start (length line)) new :point (min position (+ start (length new))))
            (message "Clock line: ~a" (string-left-trim " " (org-duration-text minutes)))
            t))))))

(defun org-clock-mode-line (editor buffer)
  "For the mode line: the clock running, how long and on what task."
  (declare (ignore editor buffer))
  (and *org-clock* (list (org-clock-string *org-clock* (org-now)) :org-mode-line-clock)))

(defun org-clock-title (editor)
  "For the window's title: the clock running, how long and on what task."
  (declare (ignore editor))
  (and *org-clock* (org-clock-string *org-clock* (org-now))))

(defun org-clock-tick (editor)
  "While a clock runs, ask to be called again when the minute changes, for the mode line to show it."
  (declare (ignore editor))
  (and *org-clock* (- 60 (mod (org-now) 60))))

(defun org-clock-quit-query ()
  "Before quitting: with a clock running, ask whether to clock out first."
  (when (and *org-clock* (y-or-n-p* (format nil "The clock runs on ~a: clock out before quitting?" (org-clock-task *org-clock*))))
    (org-clock-out-now))
  t)

(defun install-org-clock ()
  "Bind Org's clock keys in Org buffers, under \\`C-c C-x' (clock, blocks and
more), show the clock running, and ask about it before quitting."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-x C-i" 'org-clock-in)
    (define-key keymap "C-c C-x C-o" 'org-clock-out)
    (define-key keymap "C-c C-x C-q" 'org-clock-cancel)
    (define-key keymap "C-c C-x C-j" 'org-clock-goto)
    (label-keys keymap '(("C-c C-x" "clock, blocks and more"))))
  (pushnew 'org-clock-mode-line *mode-line-functions*)
  (pushnew 'org-clock-title *title-functions*)
  (pushnew 'org-clock-tick *tick-functions*)
  (pushnew 'org-clock-quit-query *quit-query-functions*)
  (pushnew 'org-clock-buffer-killed tuatara.buffer:*kill-buffer-functions*))

(defun org-clock-buffer-killed (buffer)
  "When BUFFER, being killed, holds the clock running: one in a file runs on,
its file opened again when it is asked for (ORG-CLOCK-LIVE-BUFFER); one in a
buffer of no file stops, its line left as it is, open. Either is said."
  (let ((clock *org-clock*))
    (when (and clock (eq (org-clock-buffer clock) buffer))
      (if (org-clock-path clock)
          (message (substitute-command-keys "The clock on ~a runs on; \\[org-clock-goto] opens its file again")
                   (org-clock-task clock))
          (progn
            (setf *org-clock* nil)
            (message "The clock on ~a stopped with its buffer; its CLOCK: line stays open" (org-clock-task clock)))))))
