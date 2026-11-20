;;;; lisp/org/repeat-menu.lisp --- The menu of a repeat: start, time, how it repeats, its days, its end.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; At the date prompt of C-c C-s, C-c C-d and C-c ., C-r opens a menu of the
;;;; repeat, as a calendar program asks for one: the day it starts and its
;;;; time, how it repeats (each choice named from the start day, as monthly on
;;;; the first Monday when it starts on one, or custom, which is every so many days,
;;;; weeks, months or years), every how many, which days of the week, and when
;;;; it ends, never, on a day or after so many times. What
;;;; they come to is shown under the menu, the timestamp and the days to come,
;;;; and RET writes it where the prompt would have. A repeat Org's repeaters
;;;; can say is written as one (<2026-11-02 Mon +1w>); any other as a date by
;;;; rule (<%%(...)>).

(in-package #:tuatara.defaults)

(defvar *repeat-menu-target* nil
  "While the repeat menu is open, the function given what it writes: an
ORG-TS, or the text of a timestamp by rule.")

(defun repeat-argument (arguments name)
  "The value of the option NAME= among ARGUMENTS, the repeat menu's, or NIL."
  (let ((given (find-if (lambda (a) (uiop:string-prefix-p (format nil "~a=" name) a)) arguments)))
    (and given (subseq given (1+ (length name))))))

(defun repeat-start (arguments)
  "Two values: the day number the repeat of ARGUMENTS starts on, today when
none is given, and the text of a time given with it, or NIL."
  (let ((start (repeat-argument arguments "start")))
    (multiple-value-bind (day hour minute end-hour end-minute) (and start (read-date-text start (org-today)))
      (values (or day (org-today)) (time-range-text hour minute end-hour end-minute)))))

(defun repeat-time (arguments)
  "The time of the repeat of ARGUMENTS as a timestamp writes it, 10:00 or
10:00-11:00: its own, else the one given with its start; NIL for none."
  (or (repeat-argument arguments "time") (nth-value 1 (repeat-start arguments))))

(defun repeat-nth (day)
  "Which of its weekday in its month the day number DAY is, 1 to 5, and
whether it is the last of them (ORG-FLOAT-DAY), as two values."
  (multiple-value-bind (year month date) (day-date day)
    (values (ceiling date 7) (eql day (org-float-day year month (weekday day) -1 nil)))))

(defun repeat-kinds (start)
  "The ways a repeat starting on the day number START may go, each (KEY
WORDS), WORDS what the menu says, named from START."
  (multiple-value-bind (nth last) (repeat-nth start)
    (let ((weekday (aref *weekday-names* (weekday start)))
          (ordinal (nth (1- nth) *ordinal-words*)))
      (remove nil
              (list '("none" "Does not repeat")
                    '("daily" "Daily")
                    '("weekdays" "Every weekday, Monday to Friday")
                    (list "weekly" (format nil "Weekly on ~a" weekday))
                    (list "monthly-day" (format nil "Monthly on day ~d" (nth-value 2 (day-date start))))
                    (and (< nth 5) (list "monthly-nth" (format nil "Monthly on the ~a ~a" ordinal weekday)))
                    (and last (list "monthly-last" (format nil "Monthly on the last ~a" weekday)))
                    (list "yearly" (format-day "Yearly on %e %B" start))
                    '("custom" "Custom: every so many days, weeks, months or years"))))))

(defparameter *repeat-units*
  '(("day" "daily") ("week" "weekly") ("month" "monthly-day") ("year" "yearly"))
  "The units of a custom repeat, each (UNIT KIND): every so many of UNIT
comes as the repeat KIND of REPEAT-KINDS does.")

(defun repeat-unit (arguments)
  "The unit of the custom repeat of ARGUMENTS, day, week, month or year, as
its every= says it (every=2 weeks), or week."
  (let ((every (repeat-argument arguments "every")))
    (or (and every (cl-ppcre:register-groups-bind (unit) ("^\\d{1,3} (day|week|month|year)s?$" every) unit))
        "week")))

(defun repeat-custom-p (arguments)
  "True when the repeat of ARGUMENTS is custom, its unit chosen."
  (equal (repeat-argument arguments "repeats") "custom"))

(defun repeat-kind (arguments)
  "How the repeat of ARGUMENTS goes, a key of REPEAT-KINDS: its own, or
none; a custom one goes as its unit's (*REPEAT-UNITS*)."
  (if (repeat-custom-p arguments)
      (second (assoc (repeat-unit arguments) *repeat-units* :test #'string=))
      (or (repeat-argument arguments "repeats") "none")))

(defun repeat-every (arguments)
  "Every how many days, weeks, months or years the repeat of ARGUMENTS comes,
1 when not given; a custom one's every= says its unit after it (2 weeks)."
  (let ((every (repeat-argument arguments "every")))
    (or (and every (cl-ppcre:scan "^\\d{1,3}( (day|week|month|year)s?)?$" every)
             (max 1 (parse-integer every :junk-allowed t)))
        1)))

(defun repeat-weekdays-on (arguments)
  "The days of the week switched on among the repeat menu's ARGUMENTS, in
order, 0 for Sunday; NIL for none."
  (loop for name in *repeat-weekday-arguments* for k from 0
        when (member name arguments :test #'string=) collect k))

(defun repeat-weekdays (arguments start)
  "The days of the week the weekly repeat of ARGUMENTS comes on, in order,
0 for Sunday: those switched on, else START's."
  (or (repeat-weekdays-on arguments) (list (weekday start))))

(defun repeat-months (start every)
  "The months a repeat from the day number START every EVERY months comes in,
as a rule takes them: T for every month, a list, or NIL when EVERY does not
divide twelve."
  (cond ((= every 1) t)
        ((zerop (mod 12 every))
         (let ((month (nth-value 1 (day-date start))))
           (sort (loop for k below (/ 12 every) collect (1+ (mod (+ (1- month) (* k every)) 12))) #'<)))))

(defun repeat-form (arguments start end)
  "The repeat of ARGUMENTS, from the day number START until END (or with no
end), as two values: an ORG-TS with a repeater, or an ORG-RULE with no time,
or NIL when it cannot be written; and the words saying why not."
  (let* ((kind (repeat-kind arguments))
         (every (repeat-every arguments))
         (time (repeat-time arguments)))
    (multiple-value-bind (year month date) (day-date start)
      (declare (ignore month date))
      (flet ((repeater (value unit &optional (day start))
               (let ((ts (org-date-ts day nil nil nil nil)))
                 (multiple-value-bind (hour minute end-hour end-minute) (read-time-range (or time ""))
                   (setf (org-ts-hour ts) hour (org-ts-minute ts) minute
                         (org-ts-end-hour ts) end-hour (org-ts-end-minute ts) end-minute))
                 (when value
                   (setf (org-ts-repeater ts) :cumulate (org-ts-repeat-value ts) value (org-ts-repeat-unit ts) unit))
                 ts))
             (rule (kind &rest arguments) (make-org-rule :kind kind :arguments arguments)))
        (cond
          ((string= kind "none") (repeater nil nil))
          ((string= kind "daily")
           ;; the days of the week switched on are the only ones it comes on
           (let ((days (repeat-weekdays-on arguments)))
             (cond ((and days (= every 1)) (rule :weekdays days))
                   (days (rule :all (list (rule :cyclic every start) (rule :weekdays days))))
                   (end (rule :cyclic every start))
                   (t (repeater every #\d)))))
          ((string= kind "weekdays") (rule :weekdays (list 1 2 3 4 5)))
          ((string= kind "weekly")
           (let ((days (repeat-weekdays arguments start)))
             (flet ((cyclic (d)
                      ;; the day every so many weeks, from its first at or after the start
                      (rule :cyclic (* 7 every) (+ start (mod (- d (weekday start)) 7)))))
               (cond ((and (null (rest days)) (null end))
                      (repeater every #\w (+ start (mod (- (first days) (weekday start)) 7))))
                     ((null (rest days)) (cyclic (first days)))
                     ((= every 1) (rule :weekdays days))
                     (t (rule :any (mapcar #'cyclic days)))))))
          ((string= kind "monthly-day")
           (let ((months (repeat-months start every)))
             (cond ((null end) (repeater every #\m))
                   (months (rule :date t months (nth-value 2 (day-date start))))
                   (t (values nil (format nil "Every ~d months has no end date: 12 must be a multiple of it" every))))))
          ((member kind '("monthly-nth" "monthly-last") :test #'string=)
           (let ((months (repeat-months start every)))
             (if months
                 (rule :float months (weekday start) (if (string= kind "monthly-last") -1 (repeat-nth start)) nil)
                 (values nil (format nil "Every ~d months on a weekday: 12 must be a multiple of it" every)))))
          ((string= kind "yearly")
           (multiple-value-bind (y m d) (day-date start)
             (declare (ignore y))
             (cond ((null end) (repeater every #\y))
                   ((= every 1) (rule :anniversary m d nil))
                   (t (rule :date (loop for y from year to (day-date end) by every collect y) m d))))))))))

(defun repeat-open-end (start)
  "The last day a date by rule of no end of its own that begins on the day
number START is written to take: 1 January a hundred years on."
  (day-number (+ 100 (day-date start)) 1 1))

(defun repeat-rule-text (form time &optional from to)
  "The timestamp by rule of FORM, an ORG-RULE or the text of its form
\(diary-float t 1 1), TIME after it (10:00, or NIL), taken from the day number
FROM to the day TO when they are given: <%%(and (diary-block 11 2 2026 12 21
2026) (diary-cyclic 7 11 2 2026)) 10:00>. NIL for a FORM that is no rule
ORG-RULE-FROM-DATA knows."
  (let ((rule (if (stringp form) (org-rule-from-data (org-read-rule-data form 0 (length form))) form)))
    (when rule
      (let ((rule (if from
                      ;; a rule that takes the days all its rules take has the block among its own
                      (make-org-rule :kind :all
                                     :arguments (list (cons (make-org-rule :kind :block :arguments (list from to))
                                                            (if (eq (org-rule-kind rule) :all)
                                                                (first (org-rule-arguments rule))
                                                                (list rule)))))
                      (copy-org-rule rule))))
        (multiple-value-bind (hour minute end-hour end-minute) (read-time-range (or time ""))
          (setf (org-rule-hour rule) hour (org-rule-minute rule) minute
                (org-rule-end-hour rule) end-hour (org-rule-end-minute rule) end-minute))
        (org-rule-text rule)))))

(defun repeat-days (value from count)
  "The first COUNT days, at or after the day number FROM, that VALUE (an
ORG-TS, an ORG-RULE or the text of a timestamp by rule) comes on, as day
numbers."
  (if (org-ts-p value)
      (let ((day (org-ts-day-number value)))
        (if (org-repeating-ts-p value)
            (let ((repeater (list value (org-ts-repeat-value value) (org-ts-repeat-unit value) day)))
              (loop repeat count
                    for next = (org-repeat-day repeater from :future) then (org-repeat-day repeater (1+ next) :future)
                    collect next))
            (and (>= day from) (list day))))
      (let ((rule (if (stringp value) (org-read-rule value) value)))
        (loop with day = from repeat count
              for next = (and rule (org-rule-next rule day))
              while next collect next do (setf day (1+ next))))))

(defun repeat-ends (arguments)
  "When the repeat of the menu's ARGUMENTS ends, as two values: after how
many times (8x), or on which day; NIL for each it does not say. The text it
gives as a third value."
  (let* ((ends (repeat-argument arguments "ends"))
         (count (and ends (cl-ppcre:scan "^\\d{1,4}x$" ends) (parse-integer ends :junk-allowed t))))
    (values count (and ends (not count) (read-date-text ends (org-today))) ends)))

(defun repeat-value (arguments)
  "What the repeat menu's ARGUMENTS write, as two values: an ORG-TS, or the
text of a timestamp by rule, or NIL; and the words saying why it cannot be
written. An end on a day, or after so many times (8x), makes it a rule
bounded by the days it runs (diary-block); a start after today does too
when it is a rule, so that it comes no sooner."
  (let ((start (repeat-start arguments))
        (time (repeat-time arguments)))
    (multiple-value-bind (count end-day ends) (repeat-ends arguments)
      (when (and ends (not count) (not end-day))
        (return-from repeat-value (values nil (format nil "The end, ~a, is no date" ends))))
      (when (and time (null (read-time-range time)))
        (return-from repeat-value
          (values nil (format nil "The time, ~a, is none: write 10:00 or 10:00-11:00" time))))
      (flet ((written (end)
               (multiple-value-bind (form why) (repeat-form arguments start end)
                 (cond ((null form) (values nil why))
                       ((org-ts-p form) form)
                       ;; a rule of no start of its own begins no sooner than the menu's
                       ((or end (and (> start (org-today)) (not (eq (org-rule-kind form) :any))))
                        (repeat-rule-text form time start (or end (repeat-open-end start))))
                       (t (repeat-rule-text form time))))))
        (cond ((and end-day (< end-day start))
               (values nil "It ends before it starts"))
              (count
               ;; after so many times: the last of them is its end
               (let* ((open (written nil))
                      (days (and open (repeat-days open start count))))
                 (cond ((null open) (written nil))
                       ((< (length days) count) (values nil (format nil "It does not come ~d times" count)))
                       ((string= (repeat-kind arguments) "none") open)
                       (t (written (car (last days)))))))
              ((and end-day (not (string= (repeat-kind arguments) "none"))) (written end-day))
              (t (written nil)))))))

(defun repeat-words (arguments)
  "The repeat of the menu's ARGUMENTS in words: every 2 weeks on Monday and
Wednesday, 18:00, until Mon 21 Dec 2026."
  (let* ((start (repeat-start arguments))
         (kind (repeat-kind arguments))
         (every (repeat-every arguments))
         (weekday (aref *weekday-names* (weekday start)))
         (ends (multiple-value-list (repeat-ends arguments)))
         (count (first ends))
         (end-day (second ends)))
    (flet ((every-of (unit units)
             (if (= every 1) (format nil "Every ~a" unit) (format nil "Every ~d ~a" every units))))
      (format nil "~a~@[, ~a~]~:[~;~:*, ~a~]"
              (cond ((string= kind "none") (repeat-day-words "Once, on %a %e %b %Y" start))
                    ((string= kind "daily")
                     (let ((days (repeat-weekdays-on arguments)))
                       (format nil "~a~@[, only on ~a~]" (every-of "day" "days")
                               (cond ((null days) nil)
                                     ((equal days '(1 2 3 4 5)) "weekdays")
                                     (t (format nil
                                                "~{~a~#[~; and ~:;, ~]~}"
                                                (mapcar (lambda (d) (aref *weekday-names* d)) days)))))))
                    ((string= kind "weekdays") "Every weekday, Monday to Friday")
                    ((string= kind "weekly")
                     (let ((names (mapcar (lambda (d) (aref *weekday-names* d)) (repeat-weekdays arguments start))))
                       (format nil "~a on ~{~a~#[~; and ~:;, ~]~}" (every-of "week" "weeks") names)))
                    ((string= kind "monthly-day")
                     (format nil "~a on day ~d" (every-of "month" "months") (nth-value 2 (day-date start))))
                    ((string= kind "monthly-nth")
                     (format nil "~a on the ~a ~a" (every-of "month" "months")
                             (nth (1- (repeat-nth start)) *ordinal-words*) weekday))
                    ((string= kind "monthly-last") (format nil "~a on the last ~a" (every-of "month" "months") weekday))
                    (t (format nil "~a on ~a" (every-of "year" "years") (repeat-day-words "%e %B" start))))
              (repeat-time arguments)
              (cond ((string= kind "none") nil)
                    (count (format nil "~d times" count))
                    (end-day (repeat-day-words "until %a %e %b %Y" end-day))
                    (t (repeat-day-words "from %a %e %b %Y" start)))))))

(defun repeat-summary (arguments)
  "The lines under the repeat menu: in words how it repeats, what it writes,
and the days to come; or why it cannot be written."
  (multiple-value-bind (value why) (repeat-value arguments)
    (if (null value)
        (list (format nil "Cannot be written: ~a" why))
        (let* ((text (if (stringp value) value (org-ts-text value)))
               (days (repeat-days value (org-today) 5)))
          (list (repeat-words arguments)
                (format nil "Writes  ~a" text)
                (format nil "Next    ~:[none~;~:*~{~a~^ · ~}~]"
                        (let ((year (day-date (org-today))))
                          (mapcar (lambda (d) (repeat-day-words (if (= (day-date d) year) "%a %e %b" "%a %e %b %Y") d))
                                  days))))))))

(defun repeat-day-words (pattern day)
  "The day number DAY written in PATTERN (FORMAT-DAY's), one space where the
day of the month leaves two."
  (string-trim " " (cl-ppcre:regex-replace-all "  +" (format-day pattern day) " ")))

(defun repeat-menu-shows-p (entry)
  "True when the repeat menu has ENTRY now: the days of the week for a weekly
or a daily repeat, every how many for one that comes daily, weekly, monthly or yearly."
  (let* ((argument (menu-entry-argument entry))
         (arguments (menu-arguments))
         (kind (repeat-kind arguments)))
    (cond ((member argument *repeat-weekday-arguments* :test #'string=)
           (member kind '("weekly" "daily") :test #'string=))
          ((equal argument "every=") (not (member kind '("none" "weekdays") :test #'string=)))
          (t t))))

(defun read-repeat-start (prompt &optional current)
  "Ask PROMPT for the day a repeat starts, and a time with it, as dates are
asked for, the calendar below (READ-ORG-DATE); 2026-11-02, or 2026-11-02 10:00,
or CURRENT when nothing is read."
  (let ((ts (read-org-date prompt)))
    (if (org-ts-p ts)
        (format nil "~a~@[ ~a~]" (format-day "%F" (org-ts-day-number ts)) (org-ts-time-text ts))
        (or current ""))))

(defun read-repeat-time (prompt &optional current)
  "Ask PROMPT for a time of day, 10:00 or 10:00-11:00, CURRENT offered; an
empty answer takes it away."
  (read-string (format nil "~a(10:00 or 10:00-11:00) " prompt)
               :initial (or current "") :select-initial t :trim t))

(defun read-repeat-kind (prompt &optional current)
  "Ask PROMPT how a repeat goes, the ways named from its start (REPEAT-KINDS); CURRENT the one it had."
  (declare (ignore current))
  (completing-read prompt (mapcar (lambda (kind) (make-candidate (second kind) :value (first kind)))
                                  (repeat-kinds (repeat-start (menu-arguments))))
                   :require-match t :keep-order t))

(defun read-repeat-every (prompt &optional current)
  "Ask PROMPT every how many days, weeks, months or years a repeat comes,
CURRENT offered; for a custom repeat, the unit after it (2 weeks), asked
next."
  (let* ((number (and current (cl-ppcre:scan-to-strings "^\\d+" current)))
         (typed (read-string prompt :initial (or number "2") :select-initial t :trim t)))
    (cond ((not (cl-ppcre:scan "^\\d{1,3}$" typed)) "")
          ((repeat-custom-p (menu-arguments))
           (let ((unit (completing-read (format nil "Every ~a " typed) (mapcar #'first *repeat-units*)
                                        :require-match t :keep-order t)))
             (format nil "~a ~a~:[s~;~]" typed unit (string= typed "1"))))
          (t typed))))

(defun read-repeat-end (prompt &optional current)
  "Ask PROMPT when a repeat ends: never, on a day (asked as dates are), or after
a number of times; the day (2026-12-21), the times (8x), or empty for never.
CURRENT, what it said."
  (declare (ignore current))
  (let ((how (completing-read prompt (list (make-candidate "Never" :value :never)
                                           (make-candidate "On a day" :value :day)
                                           (make-candidate "After a number of times" :value :times))
                              :require-match t :keep-order t)))
    (case how
      (:day (let ((ts (read-org-date "Ends on: ")))
              (if (org-ts-p ts) (format-day "%F" (org-ts-day-number ts)) "")))
      (:times (let ((typed (read-string "How many times: " :initial "10" :select-initial t :trim t)))
                (if (cl-ppcre:scan "^\\d{1,4}$" typed) (format nil "~ax" typed) "")))
      (t ""))))

(define-command repeat-menu-write (:interactive nil)
  "Write the repeat of the open repeat menu where the date prompt it came from
would have written its date; when it cannot be written, say why and keep the
menu open, what was set in it kept."
  (multiple-value-bind (value why) (repeat-value (menu-arguments))
    (cond ((null value) (message "Not written: ~a" why) (keep-menu-open))
          ((null *repeat-menu-target*) (message "No date prompt to write the repeat for"))
          (t (funcall (shiftf *repeat-menu-target* nil) value)))))

(define-menu repeat-menu (:title "Repeat" :show repeat-menu-shows-p :summary repeat-summary :hold t :interactive nil)
  "Open the menu of a repeat: its start, time, how it repeats and when it
ends, what they come to shown under it; RET writes it. Custom, among the
ways it repeats, is every so many days, weeks, months or years (n asks the
number, then the unit), on the days of the week switched on (1 to 7) for
weeks; for days, those switched on are the only ones it comes on (1 to 5:
weekends dropped). A key not in the menu leaves it open, and \\`C-g' asks
before what was set is lost. It is opened from the date prompt of \\[org-schedule], \\[org-deadline] and
\\[org-time-stamp] by \\`C-r'
\(ORG-DATE-REPEAT-MENU)."
  ("When"
   ("s" "Starts" :option "start=" :read read-repeat-start :replace t)
   ("t" "Time" :option "time=" :read read-repeat-time :replace t))
  ("Repeats"
   ("r" "Repeats" :option "repeats=" :read read-repeat-kind :replace t)
   ("n" "Every" :option "every=" :read read-repeat-every :replace t)
   ("1" "Mon" :switch "mon") ("2" "Tue" :switch "tue") ("3" "Wed" :switch "wed") ("4" "Thu" :switch "thu")
   ("5" "Fri" :switch "fri") ("6" "Sat" :switch "sat") ("7" "Sun" :switch "sun"))
  ("Ends"
   ("e" "Ends" :option "ends=" :read read-repeat-end :replace t))
  ("Write"
   ("RET" "Write it" repeat-menu-write)))

(defun repeat-arguments-of (value day)
  "The repeat menu's arguments for VALUE, what the date prompt read (an ORG-TS,
or the text of a timestamp by rule, or NIL), or for the day number DAY when
it read none: its start and time, and how an ORG-TS repeats; a rule starts
the menu from its next day."
  (cond ((org-ts-p value)
         (let ((n (org-ts-repeat-value value)) (unit (org-ts-repeat-unit value)))
           (append (list (format nil "start=~a" (format-day "%F" (org-ts-day-number value))))
                   (and (org-ts-hour value) (list (format nil "time=~a" (org-ts-time-text value))))
                   (and (org-repeating-ts-p value) (member unit '(#\d #\w #\m #\y))
                        (list (format nil "repeats=~a"
                                      (ecase unit (#\d "daily") (#\w "weekly") (#\m "monthly-day") (#\y "yearly")))))
                   (and (org-repeating-ts-p value) (> n 1) (list (format nil "every=~d" n))))))
        ((stringp value)
         (let* ((rule (org-read-rule value)) (next (and rule (org-rule-next rule (org-today)))))
           (append (list (format nil "start=~a" (format-day "%F" (or next day))))
                   (and rule (org-rule-hour rule) (list (format nil "time=~a" (org-rule-time-text rule)))))))
        (t (list (format nil "start=~a" (format-day "%F" day))))))

(defvar *org-date-repeat-asked* nil
  "While a date prompt that takes repeats is open, a list (ASKED TEXT DAY)
that ORG-DATE-REPEAT-MENU fills: true, what was typed, and the calendar's
day, so that the prompt opens the repeat menu from them.")
(pushnew '*org-date-repeat-asked* *question-variables*)

(define-command org-date-repeat-menu (:interactive nil)
  "At the date prompt of \\[org-schedule], \\[org-deadline] and \\[org-time-stamp]: leave
it for the menu of a repeat (REPEAT-MENU), started from what was typed, or the
calendar's day."
  (if (null *org-date-repeat-asked*)
      (message "Only at the date prompt of a schedule, deadline or timestamp")
      (progn (setf (first *org-date-repeat-asked*) t
                   (second *org-date-repeat-asked*) (editor-minibuffer-text *editor*)
                   (third *org-date-repeat-asked*) (and (org-date-calendar-live)
                                                        (calendar-date-at *org-date-calendar*)))
             (finish-minibuffer))))

(sb-ext:defglobal **org-repeat-date-prompt-keymap**
    (let ((keymap (make-keymap :name "org-repeat-date-prompt" :parent **org-date-prompt-keymap**)))
      (define-key keymap "C-r" 'org-date-repeat-menu)
      keymap)
  "The keys of the date prompt that takes repeats, those of every Org date prompt and the minibuffer's besides.")

(defun org-ask-date (prompt then &key several)
  "Ask PROMPT (Schedule, with no colon) for a date, a repeat in words too, with
SEVERAL several days (READ-ORG-DATE), the key of the repeat menu named after
PROMPT, and call THEN with what it read (an ORG-TS, the text of a
timestamp by rule or of several timestamps, or NIL);
or, when \\[org-date-repeat-menu] is typed, open the repeat menu from what
was typed, or the calendar's day, THEN called when it writes."
  (let* ((asked (list nil nil nil))
         (value (let ((*org-date-repeat-asked* asked))
                  (read-org-date (substitute-command-keys
                                  (format nil "~a (\\[org-date-repeat-menu] repeats): " prompt)
                                  :keymap **org-repeat-date-prompt-keymap**)
                                 :repeating t :several several :keymap **org-repeat-date-prompt-keymap**))))
    (if (first asked)
        (let ((text (let ((typed (string-trim " " (or (second asked) "")))) (and (plusp (length typed)) typed)))
              (day (or (third asked) (org-today))))
          (setf *repeat-menu-target* then)
          (open-menu (menu-named 'repeat-menu)
                     :arguments (repeat-arguments-of (and text (or (org-read-date-text text (org-now))
                                                                   (org-words-repeat text (org-now))))
                                                     day)))
        (funcall then value))))
