;;;; lisp/org/schedule.lisp --- Org tasks scheduled and given deadlines, dates read as Org reads them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; C-c C-s schedules the task point is under, C-c C-d gives it a deadline.
;;;; The date is asked for as any date is (read-date: fri, +3d, 10-15,
;;;; sep 30, tomorrow, 2026-12-24 14:00, 3pm), the timestamp it reads shown as it is
;;;; typed, a date already past taken in the year or month to come. The
;;;; planning line under the headline gets it, as Org writes it, a repeater
;;;; and warning there before kept; with C-u it is taken away. With
;;;; *org-log-reschedule* and *org-log-redeadline* the date it had is logged.

(in-package #:tuatara.defaults)

(define-setting *org-log-reschedule* nil
  "What Org notes when a task's scheduled date is changed or taken away.
- nil: nothing.
- :time: the date it had, and when."
  :values (member :time nil))

(define-setting *org-log-redeadline* nil
  "What Org notes when a task's deadline is changed or taken away.
- nil: nothing.
- :time: the deadline it had, and when."
  :values (member :time nil))

(defun org-date-ts (day hour minute end-hour end-minute)
  "An active timestamp, an ORG-TS, of the day number DAY, at HOUR:MINUTE
when HOUR is given, ending at END-HOUR:END-MINUTE when that is given."
  (org-ts-set-day (make-org-ts :active t :hour hour :minute minute :end-hour end-hour :end-minute end-minute) day))

(defun org-read-date-text (input now)
  "The active timestamp INPUT, a date as typed, stands for at the universal
time NOW, an ORG-TS, or NIL when INPUT is no date: what READ-DATE-TEXT reads
\(fri, +3d, 10-15, sep 30, tomorrow, 2026-12-24 14:00, 3pm)."
  (multiple-value-bind (day hour minute end-hour end-minute) (read-date-text input (org-today now))
    (and day (org-date-ts day hour minute end-hour end-minute))))

(define-setting *org-read-date-popup-calendar* t
  "Whether Org's date prompt shows the calendar below while it asks.
The prompt is that of \\[org-schedule] and \\[org-deadline].
- t: the calendar shown, the day read from what is typed marked there.
  \\`S-<left>' and \\`S-<right>' move a day either way, \\`S-<up>' and
  \\`S-<down>' a week, \\`M-S-<left>' and \\`M-S-<right>' a month,
  \\`M-S-<up>' and \\`M-S-<down>' a year, \\`C-.' to today, the date then
  written at the prompt.
- nil: asked without it."
  :values boolean)

(defvar *org-date-calendar* nil
  "While Org's date prompt shows the calendar: its buffer; NIL otherwise.")
(pushnew '*org-date-calendar* *question-variables*)

(defun org-date-calendar-live ()
  "The calendar shown under a date prompt, or NIL when there is none or it is gone."
  (and *org-date-calendar* (live-buffer-p *org-date-calendar*) *org-date-calendar*))

(defun org-date-calendar-show (day)
  "Show the calendar below the window the date prompt was asked from, point on
the day DAY, that window staying selected; true when a window was made for it."
  (let* ((buffer (calendar-buffer))
         (selected (editor-window *editor*))
         (shown (buffer-window buffer)))
    (calendar-show-day buffer day :centre t)
    (let ((window (display-buffer buffer)))
      (select-window selected)
      (calendar-show-day buffer day)
      (and window (not shown) window))))

(defun org-date-calendar-follow (ts)
  "Put the calendar the date prompt shows on the day of TS, as it is read."
  (when (org-date-calendar-live)
    (calendar-show-day *org-date-calendar* (org-ts-day-number ts))))

(sb-ext:defglobal **org-date-prompt-keymap**
    (let ((keymap (make-keymap :name "org-date-prompt")))
      (define-keys keymap '(("S-<right>" org-calendar-forward-day) ("S-<left>" org-calendar-backward-day)
                            ("S-<down>" org-calendar-forward-week) ("S-<up>" org-calendar-backward-week)
                            ("M-S-<right>" org-calendar-forward-month) ("M-S-<left>" org-calendar-backward-month)
                            ("M-S-<down>" org-calendar-forward-year) ("M-S-<up>" org-calendar-backward-year)
                            ("C-." org-calendar-goto-today)))
      keymap)
  "The keys of Org's date prompt that move the calendar below it, the minibuffer's besides.")

(define-setting *org-read-date-in-calendar* t
  "Whether Org's date question begins in the calendar shown below it.
The question is that of \\[org-schedule], \\[org-deadline], \\[org-time-stamp]
and a capture template's %^T; the calendar is shown as
*ORG-READ-DATE-POPUP-CALENDAR* says.
- t: it begins in the calendar. \\`h' \\`l' and the arrows move its day a
  day, \\`j' \\`k' a week, \\`m' marks the day, \\`u' takes the mark off.
  \\`RET' writes the days chosen at the prompt, where a time (14:00-15:00) or
  a repeat is added, \\`RET' then taking it. Several days are marked where the
  question takes several (\\[org-time-stamp], %^T): days one after another
  make a range, the others a timestamp each. Any other key begins the date at
  the prompt (fri, +3d).
- nil: it begins at the prompt."
  :values boolean)

(define-record (org-date-choice (:copier nil))
  "The days chosen in the calendar while Org's date question begins there:
MARKED, the day numbers marked; SEVERAL, true when more than one may be;
PROMPT, the question asked; KEYMAP, its keys once it is answered at the prompt."
  (marked '() :documentation "The day numbers marked in the calendar for ORG-DATE-CHOICE.")
  (several nil :documentation "True when ORG-DATE-CHOICE may take more than one day.")
  (prompt nil :documentation "The question ORG-DATE-CHOICE asks.")
  (keymap nil :documentation "The keys of ORG-DATE-CHOICE once the question is answered at the prompt."))

(defvar *org-date-choosing* nil
  "While Org's date question is answered in the calendar, nothing typed at the
prompt yet (*ORG-READ-DATE-IN-CALENDAR*): its ORG-DATE-CHOICE; NIL otherwise.")
(pushnew '*org-date-choosing* *question-variables*)

(defparameter *org-date-choose-keys*
  '(("h" org-calendar-backward-day) ("C-b" org-calendar-backward-day) ("<left>" org-calendar-backward-day)
    ("l" org-calendar-forward-day) ("C-f" org-calendar-forward-day) ("<right>" org-calendar-forward-day)
    ("k" org-calendar-backward-week) ("C-p" org-calendar-backward-week) ("<up>" org-calendar-backward-week)
    ("j" org-calendar-forward-week) ("C-n" org-calendar-forward-week) ("<down>" org-calendar-forward-week)
    ("m" org-date-choose-mark) ("u" org-date-choose-unmark)
    ("RET" org-date-choose-write))
  "The keys of Org's date question while it is answered in the calendar
\(*ORG-READ-DATE-IN-CALENDAR*), before the question's own: each (KEY COMMAND).")

(defun org-date-choice-days (choice)
  "The days CHOICE stands for, in order: those marked, or the calendar's day
when none is."
  (or (sort (copy-list (org-date-choice-marked choice)) #'<)
      (list (or (calendar-date-at *org-date-calendar*) (org-today)))))

(defun org-date-runs (days)
  "DAYS, day numbers in order, as runs of days one after another, each
\(FIRST LAST): (1 2 3 5) is ((1 3) (5 5))."
  (let ((runs '()))
    (dolist (day days (nreverse runs))
      (if (and runs (= day (1+ (second (first runs)))))
          (setf (second (first runs)) day)
          (push (list day day) runs)))))

(defun org-date-choice-text (choice &optional (pattern "%F") (days (org-date-choice-days choice)))
  "The days of CHOICE (DAYS, its ORG-DATE-CHOICE-DAYS) written by PATTERN
\(FORMAT-DAY's), a run of days FIRST--LAST, several apart by commas:
2026-11-02, 2026-11-09--2026-11-11, as the date question reads them."
  (format nil "~{~a~^, ~}"
          (loop for (first last) in (org-date-runs days)
                collect (if (= first last)
                            (format-day pattern first)
                            (format nil "~a--~a" (format-day pattern first) (format-day pattern last))))))

(defun org-date-choosing-prompt (prompt)
  "PROMPT as Org's date question shows it: while it is answered in the
calendar, the days chosen and the keys after it."
  (let ((choice *org-date-choosing*))
    (if choice
        (let ((days (org-date-choice-days choice)))
          (format nil "~a~a  (h j k l move, m marks, u unmarks, RET writes ~:[it~;them~] here) "
                  prompt (org-date-choice-text choice "%F %a" days) (rest days)))
        prompt)))

(defun org-date-choice-shown (choice)
  "Show the days CHOICE has marked in the calendar of the date question, and
the days chosen in its prompt."
  (when (org-date-calendar-live)
    (setf (tuatara.buffer:buffer-local *org-date-calendar* 'calendar-chosen) (org-date-choice-marked choice)))
  (when (editor-minibuffer-active *editor*)
    (setf (editor-minibuffer-prompt *editor*) (org-date-choosing-prompt (org-date-choice-prompt choice)))))

(defun org-date-choosing-end (&key keep-marks)
  "End answering Org's date question in the calendar: what is typed goes to
the prompt, with the question's own keys; the marks stay shown with KEEP-MARKS."
  (let ((choice *org-date-choosing*))
    (when choice
      (setf *org-date-choosing* nil
            *minibuffer-question-keymap* (org-date-choice-keymap choice))
      (unless keep-marks
        (setf (org-date-choice-marked choice) '()))
      (org-date-choice-shown choice))))

(defun org-date-choose-move (days &optional (months 0))
  "Move the calendar of the date question DAYS days and MONTHS months on from its day."
  (when (org-date-calendar-live)
    (let ((from (or (calendar-date-at *org-date-calendar*) (org-today))))
      (calendar-show-day *org-date-calendar* (if (zerop months) (+ from days) (day-months-on from months))))
    (when *org-date-choosing* (org-date-choice-shown *org-date-choosing*))))

(define-command org-date-choose-mark ()
  "In Org's date question, answered in the calendar: mark the calendar's day,
beside those marked where the question takes several days, else in place of
the one marked."
  (let ((choice *org-date-choosing*) (day (calendar-date-at *org-date-calendar*)))
    (when (and choice day)
      (setf (org-date-choice-marked choice)
            (if (org-date-choice-several choice) (adjoin day (org-date-choice-marked choice)) (list day)))
      (org-date-choice-shown choice))))

(define-command org-date-choose-unmark ()
  "In Org's date question, answered in the calendar: take the mark off the calendar's day."
  (let ((choice *org-date-choosing*) (day (calendar-date-at *org-date-calendar*)))
    (when (and choice day)
      (setf (org-date-choice-marked choice) (remove day (org-date-choice-marked choice)))
      (org-date-choice-shown choice))))

(define-command org-date-choose-write ()
  "In Org's date question, answered in the calendar: write the days chosen at
the prompt (the calendar's day when none is marked), to add a time or a
repeat to; RET there takes it."
  (let ((choice *org-date-choosing*))
    (when choice
      (let ((text (org-date-choice-text choice)))
        (org-date-choosing-end :keep-marks t)
        (setf (editor-minibuffer-text *editor*) text
              (editor-minibuffer-point *editor*) (length text))))))

(defun org-read-several-dates (text today)
  "TEXT, days apart by commas and runs of days FIRST--LAST, as the date question
writes them from the calendar (2026-11-02, 2026-11-09--2026-11-11 14:00-15:00),
or as timestamps one after another as a note has them (<2026-11-02 Mon 09:00>
<2026-11-09 Mon>--<2026-11-11 Wed>), as the timestamps it makes, one after
another. A run is a range <FIRST>--<LAST>, the time written after the last day
given to every day written without one. TODAY is the day number of today. NIL
when TEXT is one day alone, or not such."
  ;; timestamps side by side are days apart, as commas part them
  (setf text (cl-ppcre:regex-replace-all "([>\\]])\\s+([<\\[])" text "\\1, \\2"))
  (when (or (find #\, text) (search "--" text))
    (flet ((day-of (piece)
             (let ((parts (and (plusp (length piece)) (multiple-value-list (read-date-text piece today)))))
               (if (first parts) parts (return-from org-read-several-dates nil)))))
      (let* ((read (loop for piece in (uiop:split-string text :separator ",")
                         collect (let ((ends (cl-ppcre:split "--" (string-trim " " piece))))
                                   (unless (<= 1 (length ends) 2) (return-from org-read-several-dates nil))
                                   (mapcar (lambda (end) (day-of (string-trim " " end))) ends))))
             (time (rest (car (last (car (last read)))))))
        (format nil "~{~a~^ ~}"
                (loop for ends in read
                      collect (format nil "~{~a~^--~}"
                                      (loop for (day . own) in ends
                                            collect (org-ts-text
                                                     (apply #'org-date-ts day (if (first own) own time)))))))))))

(defun org-date-prompt-other (typed &key repeating several)
  "What Org's date prompt reads of TYPED when it is no date, and the words
shown for it, as two values; NIL for nothing: several days as the calendar
writes them, (:SEVERAL TEXT), with SEVERAL; a timestamp typed whole, an
ORG-TS; with REPEATING, a timestamp by rule typed whole, its text, or a repeat
in words (ORG-WORDS-REPEAT)."
  (let ((dates (and several (org-read-several-dates typed (org-today))))
        (ts (org-read-ts typed)))
    (cond (dates (values (list :several dates) dates))
          (ts (values ts (org-ts-text ts)))
          ((and repeating (org-read-rule typed)) (values typed typed))
          (repeating (let ((value (org-words-repeat typed (org-now))))
                       (values value (and value (org-repeat-words value))))))))

(defun read-org-date (prompt &key repeating several keymap)
  "Ask PROMPT for a date as any date is asked for (READ-DATE), what it reads
shown as it is typed as the timestamp it makes, and the calendar below, its
day marked (*ORG-READ-DATE-POPUP-CALENDAR*); a timestamp typed whole
\(<2026-10-05 Mon 09:00 +1w>) is taken as it is. With REPEATING, a repeat in
words is read too (ORG-WORDS-REPEAT: every week 10:00, first monday every
month), and a timestamp by rule typed whole. The keys that move the
calendar are those of **ORG-DATE-PROMPT-KEYMAP**, or of KEYMAP when given,
whose parent it is. With SEVERAL, several days are read too, as the
calendar writes them (ORG-READ-SEVERAL-DATES). Begun in the calendar as
*ORG-READ-DATE-IN-CALENDAR* says. The ORG-TS, the text of a timestamp by rule
or of several timestamps, or NIL when what was typed is no date."
  (let* ((window (and *org-read-date-popup-calendar* (org-date-calendar-show (org-today))))
         (*org-date-calendar* (and *org-read-date-popup-calendar* (calendar-buffer)))
         (keymap (or keymap **org-date-prompt-keymap**))
         (*org-date-choosing* (and *org-date-calendar* *org-read-date-in-calendar*
                                   (make-org-date-choice :several several :prompt prompt :keymap keymap)))
         (first-keys (if *org-date-choosing*
                         (let ((choosing (make-keymap :name "org-date-choose" :parent keymap)))
                           (define-keys choosing *org-date-choose-keys*)
                           choosing)
                         keymap))
         (asking (if *org-date-choosing*
                     (lambda () (org-date-choosing-prompt prompt))
                     prompt)))
    (unwind-protect
         (multiple-value-bind (time day hour minute end-hour end-minute)
             (read-date asking :now (org-now)
                               :shown (lambda (&rest parts) (org-ts-text (apply #'org-date-ts parts)))
                               :follow (lambda (&rest parts)
                                         ;; the calendar's day is the answer while it is chosen there
                                         (unless *org-date-choosing*
                                           (org-date-calendar-follow (apply #'org-date-ts parts))))
                               ;; a key typed while the calendar is answered in begins the date at the prompt;
                               ;; nothing is offered to take till then
                               :typed (lambda (input)
                                        (when (plusp (length input)) (org-date-choosing-end))
                                        *org-date-choosing*)
                               :other (lambda (typed)
                                        (org-date-prompt-other typed :repeating repeating :several several))
                               :keymap first-keys)
           (cond ((or (org-ts-p time) (stringp time)) time)
                 ((and (consp time) (eq (first time) :several)) (second time))
                 (t (and time (org-date-ts day hour minute end-hour end-minute)))))
      (when (org-date-calendar-live)
        (setf (tuatara.buffer:buffer-local *org-date-calendar* 'calendar-chosen) nil))
      ;; the window made for the calendar goes with the question
      (when (and window (member window (editor-windows *editor*)))
        (leave-window window)))))

(defparameter *repeat-weekday-arguments* (map 'list #'string-downcase *short-weekday-names*)
  "The switches of the repeat menu (REPEAT-MENU) for the days of the week, by
their number, 0 for Sunday: sun, mon and so on.")

(defparameter *org-repeat-counts*
  '(("one" . 1) ("two" . 2) ("other" . 2) ("second" . 2) ("three" . 3) ("third" . 3) ("four" . 4) ("fourth" . 4)
    ("five" . 5) ("fifth" . 5) ("six" . 6) ("sixth" . 6) ("seven" . 7) ("eight" . 8) ("nine" . 9) ("ten" . 10)
    ("eleven" . 11) ("twelve" . 12))
  "The words for how often a repeat comes round: every other week, every third month.")

(defun org-repeat-weekday-set (words)
  "The days of the week WORDS name, 0 for Sunday to 6 for Saturday, in
order, or NIL when they name anything else: weekday or workday (Monday to
Friday), weekend, a day to a day (friday to monday too), or days listed,
and between them passed over."
  (let ((words (remove "and" words :test #'string=)))
    (cond ((and (= (length words) 1)
                (member (first words) '("weekday" "weekdays" "workday" "workdays") :test #'string=))
           (list 1 2 3 4 5))
          ((and (= (length words) 1) (member (first words) '("weekend" "weekends") :test #'string=))
           (list 0 6))
          ((and (= (length words) 3) (string= (second words) "to"))
           (let ((from (weekday-of-word (first words))) (to (weekday-of-word (third words))))
             (and from to (sort (loop for k from 0 to (mod (- to from) 7) collect (mod (+ from k) 7)) #'<))))
          (t (let ((days (mapcar #'weekday-of-word words)))
               (and days (every #'identity days) (sort (remove-duplicates days) #'<)))))))

(defun org-repeat-range (input)
  "INPUT, a repeat in words, cut where it says when it runs: three values, the
repeat, the text of the date it runs from (after from) and of the date it
runs to (after to or until, once from is said, or until alone); NIL when
INPUT says neither."
  (let* ((from (cl-ppcre:scan "(?i)(?:^|\\s)from\\s" input))
         (until (if from
                    (cl-ppcre:scan "(?i)\\s(?:to|until|till)(?:\\s|$)" input :start (1+ from))
                    (cl-ppcre:scan "(?i)(?:^|\\s)(?:until|till)(?:\\s|$)" input))))
    (when (or from until)
      (flet ((after (at) (let ((space (position-if (lambda (c) (member c '(#\Space #\Tab))) input :start (1+ at))))
                           (if space (string-trim " " (subseq input space)) ""))))
        (values (string-trim " " (subseq input 0 (or from until)))
                (and from
                     (string-trim " "
                                  (subseq input
                                          (+ from (if (char-equal (char input from) #\f) 0 1) 4)
                                          (or until (length input)))))
                (and until (after until)))))))

(defparameter *org-repeat-opening-words*
  '(("every") ("each") ("everyday" "every" "day") ("daily" "every" "day") ("weekly" "every" "week")
    ("monthly" "every" "month") ("yearly" "every" "year"))
  "The words a repeat in words is begun with (ORG-WORDS-REPEAT), each with the
words it stands for when it is one of every day, week, month or year.")

(defparameter *org-repeat-opening-scanner*
  (cl-ppcre:create-scanner (format nil "(?i)(?:^|\\s)(?:~{~a~^|~})(?:\\s|$)"
                                   (mapcar #'first *org-repeat-opening-words*)))
  "A word of *ORG-REPEAT-OPENING-WORDS* standing alone.")

(defun org-repeat-words-p (text)
  "True when TEXT holds a word a repeat in words is begun with (every,
each, daily ...: *ORG-REPEAT-OPENING-WORDS*)."
  (and (cl-ppcre:scan *org-repeat-opening-scanner* text) t))

(defun org-words-repeat (input now)
  "INPUT, a repeat in words, read at the universal time NOW, as
ORG-WORDS-REPEAT-PLAIN reads it, and from and to when it says them
\(ORG-REPEAT-RANGE): from a date, a repeater begins there. To or until a date,
it is a timestamp by rule (<%%(and (diary-block ...) ...)>) that ends there,
written as the repeat menu writes it (REPEAT-VALUE), its dates read as a date is
typed (nov 2, 2026-11-02, fri). NIL when INPUT is none, a date is none, it ends
before it begins, it cannot be written to end, or a rule is given a date to
begin from and none to end."
  (multiple-value-bind (head from-text to-text)
      ;; the time, wherever it is said, is the repeat's
      (multiple-value-bind (start end) (find-time-range input)
        (let ((time (and start (subseq input start end)))
              (rest (if start (concatenate 'string (subseq input 0 start) (subseq input end)) input)))
          (multiple-value-bind (head from-text to-text) (org-repeat-range rest)
            (values (and head (format nil "~a~@[ ~a~]" head time)) from-text to-text))))
    (if (null head)
        (org-words-repeat-plain input now)
        (let* ((today (org-today now))
               (start (if from-text (read-date-text from-text today) today))
               (end (and to-text (read-date-text to-text today))))
          (when (and start (or (null to-text) (and end (<= start end))))
            (multiple-value-bind (value arguments) (org-words-repeat-plain head (day-universal-time start 12 0))
              (cond ((null value) nil)
                    ((null end) (and (org-ts-p value) value))
                    (t (let ((*org-now* now)
                             (ends (format nil "ends=~a" (format-day "%F" end))))
                         (values (repeat-value (append arguments (list ends)))))))))))))

(defun org-words-repeat-plain (input now)
  "INPUT, a repeat in words, read at the universal time NOW, or NIL when it is
none. Every day, week, month or year, every N or other of them, every Monday
or every other Friday: an active timestamp (ORG-TS) from today, or the
weekday to come, with its repeater (+2d, +1w). Days of the week, every
weekday, every weekend, every monday to friday, every mon, wed and fri
\(ORG-REPEAT-WEEKDAY-SET), more than one: the text of a timestamp by rule,
<%%(memq (calendar-day-of-week date) '(1 2 3 4 5))>. The first, second ... fifth
or last weekday of every month, or of every N months: the text of a
timestamp by rule (<%%(diary-float t 1 1)>), the months counted from the next
time it comes, N dividing twelve. A time of day or a time range may go
anywhere (10:00, 10:00-11:00); of, the, on, at and commas are passed over;
everyday and daily are every day, weekly, monthly and yearly likewise. It is
written as the repeat menu writes it (REPEAT-FORM), from the menu's
arguments for it, which are the second value."
  (let* ((words (loop for w in (cl-ppcre:split "[\\s,]+" (string-downcase (string-trim " " input)))
                      for whole = (cdr (assoc w *org-repeat-opening-words* :test #'string=))
                      unless (member w '("" "of" "the" "on" "at") :test #'string=)
                        append (or whole (list w))))
         (time (find-if #'read-time-range words))
         (words (remove time words :test #'equal))
         (every (position-if (lambda (w) (member w '("every" "each") :test #'string=)) words))
         (today (org-today now)))
    (when every
      (let* ((before (subseq words 0 every))
             (after (subseq words (1+ every)))
             (days (and (null before) (org-repeat-weekday-set after)))
             (count (let ((w (first after)))
                      (cond ((null w) nil)
                            ((cl-ppcre:scan "^[0-9]{1,3}$" w) (parse-integer w))
                            (t (cdr (assoc w *org-repeat-counts* :test #'string=))))))
             (after (if count (rest after) after))
             (count (or count 1))
             (start today)
             (kind
               (cond
                 ;; several days of the week
                 ((and days (rest days))
                  (if (equal days '(1 2 3 4 5))
                      (list "repeats=weekdays")
                      (list* "repeats=weekly" (mapcar (lambda (d) (nth d *repeat-weekday-arguments*)) days))))
                 ;; every N days, weeks, months, years, or weekdays
                 ((and (null before) (= (length after) 1) (plusp count))
                  (let ((unit (string-right-trim "s" (first after)))
                        (weekday (weekday-of-word (first after))))
                    (cond (weekday (list "repeats=weekly" (nth weekday *repeat-weekday-arguments*)))
                          ((string= unit "day") (list "repeats=daily"))
                          ((string= unit "week") (list "repeats=weekly"))
                          ((string= unit "month") (list "repeats=monthly-day"))
                          ((string= unit "year") (list "repeats=yearly")))))
                 ;; the Nth or last weekday of every month, or of every N months, from the next time it comes
                 ((and (= (length before) 2) (= (length after) 1)
                       (string= (string-right-trim "s" (first after)) "month"))
                  (let ((n (if (string= (first before) "last") -1 (ordinal-of-word (first before))))
                        (dayname (weekday-of-word (second before))))
                    (when (and n dayname)
                      (setf start (org-rule-next (make-org-rule :kind :float :arguments (list t dayname n nil)) today))
                      (and start (list (if (minusp n) "repeats=monthly-last" "repeats=monthly-nth")))))))))
        (when kind
          (let* ((arguments (append (list (format nil "start=~a" (format-day "%F" start)))
                                    (and time
                                         (list (format nil "time=~a" (multiple-value-call #'time-range-text
                                                                       (read-time-range time)))))
                                    (and (> count 1) (list (format nil "every=~d" count)))
                                    kind))
                 (form (repeat-form arguments start nil)))
            (cond ((null form) nil)
                  ((org-ts-p form) (values form arguments))
                  (t (values (repeat-rule-text form (repeat-time arguments)) arguments)))))))))

(defun org-repeat-words (value)
  "VALUE, what ORG-WORDS-REPEAT read, as the question shows it: the timestamp
it writes, and for one by rule when it next comes."
  (if (stringp value)
      (let ((next (org-rule-next (org-read-rule value) (org-today))))
        (format nil "~a~@[   next ~a~]" value (and next (format-day "%F %a" next))))
      (org-ts-text value)))

(defun org-repeat-date-place (text)
  "Where in TEXT, a repeat in words, the date the calendar writes goes: after
the last of from, or to or until once from is said, or until alone; NIL when
TEXT is no repeat in words (every, each, daily ...) or says none of them."
  (when (org-repeat-words-p text)
    (let ((from (cl-ppcre:scan "(?i)(?:^|\\s)from(?:\\s|$)" text)) (place nil))
      (cl-ppcre:do-scans (s e rs re (if from
                                        "(?i)(?:^|\\s)(?:from|to|until|till)(?:\\s|$)"
                                        "(?i)(?:^|\\s)(?:until|till)(?:\\s|$)")
                          text nil :start (or from 0))
        (setf place e))
      place)))

(defun org-date-calendar-step (days months)
  "In Org's date prompt with the calendar: move its day DAYS days and MONTHS
months on from the one marked, and write that date at the prompt, a time
typed kept after it, as the prompt reads it. In a repeat in words, the date
goes where ORG-REPEAT-DATE-PLACE says, in place of what was typed after it,
or after from, put at the end, when it says none. While the question is
answered in the calendar (*ORG-DATE-CHOOSING*), the calendar's day alone moves."
  (cond
    ((not (org-date-calendar-live))
     (message "Only while a date is asked for with the calendar"))
    (*org-date-choosing* (org-date-choose-move days months))
    (t
     (let* ((from (or (calendar-date-at *org-date-calendar*) (org-today)))
            (day (if (zerop months) (+ from days) (day-months-on from months)))
            (text (editor-minibuffer-text *editor*))
            (time (multiple-value-bind (start end) (find-time-range text) (and start (subseq text start end))))
            (words (org-repeat-words-p text))
            (place (and words (org-repeat-date-place text)))
            (new (cond (place (format nil "~a~a~a" (subseq text 0 place)
                                      (if (char= (char text (1- place)) #\Space) "" " ")
                                      (format-day "%F" day)))
                       (words (format nil "~a from ~a" (string-right-trim " " text) (format-day "%F" day)))
                       (t (format nil "~a~@[ ~a~]" (format-day "%F" day) time)))))
       (calendar-show-day *org-date-calendar* day)
       (setf (editor-minibuffer-text *editor*) new
             (editor-minibuffer-point *editor*) (length new))))))

(define-command org-calendar-forward-day ()
  "In Org's date prompt: the calendar's day a day on, written at the prompt."
  (org-date-calendar-step 1 0))

(define-command org-calendar-backward-day ()
  "In Org's date prompt: the calendar's day a day back, written at the prompt."
  (org-date-calendar-step -1 0))

(define-command org-calendar-forward-week ()
  "In Org's date prompt: the calendar's day a week on, written at the prompt."
  (org-date-calendar-step 7 0))

(define-command org-calendar-backward-week ()
  "In Org's date prompt: the calendar's day a week back, written at the prompt."
  (org-date-calendar-step -7 0))

(define-command org-calendar-forward-month ()
  "In Org's date prompt: the calendar's day a month on, written at the prompt as
the same day of the month, or the month's last when it has fewer (31 January
a month on is 28 February), as the calendar moves; +1m typed counts on past
the month's end instead (3 March)."
  (org-date-calendar-step 0 1))

(define-command org-calendar-backward-month ()
  "In Org's date prompt: the calendar's day a month back, written at the
prompt as the same day of the month, or the month's last when it has fewer, as
the calendar moves; -1m typed counts on past the month's end instead."
  (org-date-calendar-step 0 -1))

(define-command org-calendar-forward-year ()
  "In Org's date prompt: the calendar's day a year on, written at the prompt, so
29 February a year on is 28 February, as the calendar moves; +1y typed is
1 March."
  (org-date-calendar-step 0 12))

(define-command org-calendar-backward-year ()
  "In Org's date prompt: the calendar's day a year back, written at the
prompt, so 29 February a year back is 28 February, as the calendar moves; -1y
typed is 1 March."
  (org-date-calendar-step 0 -12))

(define-command org-calendar-goto-today ()
  "In Org's date prompt: the calendar on today, written at the prompt."
  (when (org-date-calendar-live)
    (org-date-calendar-step (- (org-today) (or (calendar-date-at *org-date-calendar*) (org-today))) 0)))

(defun org-planning-tokens (line)
  "The planning LINE's keywords and their timestamps, in order, as (KEYWORD
. TIMESTAMP), a range of two as one: <2026-10-05 Mon>--<2026-10-07 Wed>."
  (let ((tokens '()))
    (cl-ppcre:do-register-groups (keyword timestamp) (*org-planning-item-scanner* line)
      (push (cons keyword timestamp) tokens))
    (nreverse tokens)))

(defun org-inactive-text (timestamp)
  "TIMESTAMP's text with [ and ] about it, as a date is written in a log."
  (format nil "[~a]" (subseq timestamp 1 (1- (length timestamp)))))

(defun org-ts-keep-cycle (new old)
  "NEW, an ORG-TS, given the repeater and the warning of OLD (an ORG-TS or NIL)
where it has none of its own."
  (when old
    (unless (org-ts-repeater new)
      (setf (org-ts-repeater new) (org-ts-repeater old) (org-ts-repeat-value new) (org-ts-repeat-value old)
            (org-ts-repeat-unit new) (org-ts-repeat-unit old)))
    (unless (org-ts-warning new)
      (setf (org-ts-warning new) (org-ts-warning old) (org-ts-warning-value new) (org-ts-warning-value old)
            (org-ts-warning-unit new) (org-ts-warning-unit old))))
  new)

(defun org-no-date ()
  "Say that what was typed at a date prompt is no date, with the kinds it reads."
  (message "That is no date: fri, +3d, 10-15, sep 30 or 2026-12-24 14:00"))

(defun org-planning-tokens-with (tokens keyword text)
  "The planning line's TOKENS, each (KEYWORD . TIMESTAMP), with KEYWORD's
timestamp TEXT, put first when it had none, or KEYWORD taken away when TEXT
is NIL; a CLOSED: goes when a date is given, the task open again."
  (let ((open (if text (remove "CLOSED" tokens :key #'car :test #'string=) tokens)))
    (cond ((null text) (remove keyword open :key #'car :test #'string=))
          ((assoc keyword open :test #'string=)
           (mapcar (lambda (token) (if (string= (car token) keyword) (cons keyword text) token)) open))
          (t (cons (cons keyword text) open)))))

(defun org-plan-log-note (keyword old new)
  "The note logged when the planning KEYWORD's timestamp OLD is changed for
NEW (NIL when it is taken away), as *ORG-LOG-RESCHEDULE* and
*ORG-LOG-REDEADLINE* say; NIL when none is."
  (let ((deadline-p (string= keyword "DEADLINE")))
    (and old (if deadline-p *org-log-redeadline* *org-log-reschedule*)
         (format nil "- ~a \"~a\" on ~a"
                 (cond ((and deadline-p new) "New deadline from")
                       (deadline-p "Removed deadline, was")
                       (new "Rescheduled from")
                       (t "Not scheduled, was"))
                 (org-inactive-text old) (org-timestamp-text (org-now))))))

(defun org-plan (keyword new)
  "Give the task point is under the planning KEYWORD (\"SCHEDULED\" or
\"DEADLINE\") with the ORG-TS NEW, or take it away when NEW is NIL, logging the
date it had as their settings say. A date by rule is not replaced by one
date that does not repeat, which would end its repeats: that is said."
  (with-org-structure (parse buffer tree text position)
    (let ((headline (org-current-headline tree position)))
      (if (null headline)
          (message "Not under a headline")
          (let* ((begin (org-node-begin headline))
                 (line-end (string-line-end text begin))
                 (end (org-entry-end headline text))
                 (first-line (and (< line-end end) (org-line-at text (1+ line-end))))
                 (planning-p (and first-line (org-planning-line-p first-line)))
                 (tokens (and planning-p (org-planning-tokens first-line)))
                 (old (cdr (assoc keyword tokens :test #'string=)))
                 (deadline-p (string= keyword "DEADLINE")))
            (cond ((and (null new) (null old))
                   (message "~:[Not scheduled~;No deadline~]" deadline-p))
                  ;; a date by rule is changed for another repeat, never quietly ended by one date
                  ((and old (org-ts-p new) (not (org-repeating-ts-p new)) (org-rule-text-p old))
                   (message (substitute-command-keys
                             "~a repeats by rule, ~a: one date would end its repeats. Type a repeat ~
(every weekday 9:00) or C-r at the date prompt for the repeat menu; t at the question moves one time; ~
\\<org-mode>\\[universal-argument] \\[org-schedule] takes it away")
                            (org-property headline :raw-value) old))
                  (t
                   (let* ((new-text (cond ((null new) nil)
                                          ((stringp new) new)
                                          (t (org-ts-text (org-ts-keep-cycle new (and old (org-read-ts old)))))))
                          (planning (format nil "~{~a~^ ~}"
                                            (mapcar (lambda (token) (format nil "~a: ~a" (car token) (cdr token)))
                                                    (org-planning-tokens-with tokens keyword new-text))))
                          (note (org-plan-log-note keyword old new))
                          (entry (org-entry-with-log headline text (and note (list note)) *org-log-into-drawer*))
                          (entry-line-end (or (position #\Newline entry) (length entry)))
                          (body (if (< entry-line-end (length entry)) (subseq entry (1+ entry-line-end)) ""))
                          (lines (text-lines body))
                          (new-entry (org-entry-from-lines
                                      (subseq text begin line-end)
                                      (append (and (plusp (length planning)) (list planning))
                                              (if planning-p (rest lines) lines))
                                      (ends-with-newline-p entry))))
                     (org-replace-chars parse begin end new-entry
                                        :point (cond ((<= position line-end) position)
                                                     ((>= position end)
                                                      (+ position (- (length new-entry) (- end begin))))
                                                     (t begin)))
                     (if new
                         (message "~a: ~a" keyword new-text)
                         (message (if deadline-p "Deadline taken away" "Scheduling taken away")))))))))))

(define-command org-schedule ()
  "Schedule the task point is under (\\`C-c C-s'): the
date is asked for (fri, +3d, 10-15, sep 30, 2026-12-24 14:00),
what it reads shown as it is typed, and SCHEDULED: goes on its planning line,
a repeater and warning it had kept. With \\[universal-argument], take it away.
An entry whose date repeats asks first: \\`t' moves this time alone to the
day asked for, its time kept unless another is typed, the rest as it was,
noted in its log; done there, it is done for the time it was. In the agenda
it is the time of the line. \\`s' schedules the series."
  (if (prefix-argument)
      (org-plan "SCHEDULED" nil)
      (case (org-occurrence-choice "Schedule")
        (:none nil)
        (:this (org-move-asked))
        (:series (org-ask-date "Schedule" (lambda (ts) (if ts (org-plan "SCHEDULED" ts) (org-no-date))))))))

(define-command org-deadline ()
  "Give the task point is under a deadline (\\`C-c C-d'):
the date asked for as \\[org-schedule] asks it, DEADLINE: on its planning
line, a repeater and warning it had kept. With \\[universal-argument], take it away."
  (if (prefix-argument)
      (org-plan "DEADLINE" nil)
      (org-ask-date "Deadline" (lambda (ts) (if ts (org-plan "DEADLINE" ts) (org-no-date))))))

(defun org-timestamp-around (text position)
  "Where the timestamp of TEXT that holds the character POSITION, or ends just
before it, begins and ends, on its line: in a heading, its text, or a
planning line (SCHEDULED: <...>); each date of a range alone. NIL when there
is none."
  (cl-ppcre:do-matches (start end *org-one-timestamp-scanner* text nil
                              :start (string-line-start text position) :end (string-line-end text position))
    (when (and (<= start position end) (org-read-ts (subseq text start end)))
      (return-from org-timestamp-around (values start end)))))

(defun org-insert-timestamp (active)
  "Ask for a date and put it in at point as a timestamp, ACTIVE <...> or
inactive [...]; on a timestamp or just after it (ORG-TIMESTAMP-AROUND), put it
in that one's place, its repeater and warning kept, and its time when none
was typed. An active one may be several days chosen in the calendar, put in
one after another (*ORG-READ-DATE-IN-CALENDAR*)."
  (org-ask-date
   (if active "Date" "Inactive date")
   (lambda (ts)
     (cond
       ((null ts) (org-no-date))
       ((stringp ts)
        (if active
            (with-org-structure (parse buffer tree text position)
              (org-replace-chars parse position position ts :point (+ position (length ts))))
            (message "~a" (substitute-command-keys
                           "A date by rule is an active timestamp only: \\[org-time-stamp] writes it"))))
       (t
        (with-org-structure (parse buffer tree text position)
          (multiple-value-bind (from to) (org-timestamp-around text position)
            (let* ((old (and from (org-read-ts (subseq text from to))))
                   (new (progn (setf (org-ts-active ts) active)
                               (when (and old (org-ts-hour old) (not (org-ts-hour ts)))
                                 (setf (org-ts-hour ts) (org-ts-hour old) (org-ts-minute ts) (org-ts-minute old)
                                       (org-ts-end-hour ts) (org-ts-end-hour old) (org-ts-end-minute ts)
                                       (org-ts-end-minute old)))
                               (org-ts-text (org-ts-keep-cycle ts old))))
                   (from (or from position)))
              (org-replace-chars parse from (or to position) new :point (+ from (length new)))))))))
   :several active))

(define-command org-time-stamp ()
  "Put in a timestamp at point (\\`C-c .'), the date asked for as
\\[org-schedule] asks it: <2026-10-13 Tue>, or with a time <2026-10-13 Tue
14:00>. An active timestamp shows its entry in the agenda on that day. On a
timestamp or just after it, change its date, its repeater and warning kept."
  (org-insert-timestamp t))

(define-command org-time-stamp-inactive ()
  "Put in an inactive timestamp at point (\\`C-c !'): as \\[org-time-stamp],
written [2026-10-13 Tue], which the agenda leaves out. On a timestamp, put
an inactive one in its place."
  (org-insert-timestamp nil))

(defun install-org-schedule ()
  "Bind \\`C-c C-s', \\`C-c C-d', \\`C-c .' and \\`C-c !' in Org buffers."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-s" 'org-schedule)
    (define-key keymap "C-c C-d" 'org-deadline)
    (define-key keymap "C-c ." 'org-time-stamp)
    (define-key keymap "C-c !" 'org-time-stamp-inactive)))

