;;;; lisp/org/dates.lisp --- Org's timestamps read, moved and written as Org moves and writes them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A timestamp <2026-10-05 Mon 09:00-10:00 +1w -3d> is read into its date,
;;;; its times, its repeater (+1w, ++1w, .+1w: again a week on, a week on from
;;;; the last that is still to come, a week from today) and its warning or
;;;; delay (-3d, --3d), and written back in Org's order. Moving one by months
;;;; or years rolls over as Emacs's dates do: 31 January a month on is 3 March.

(in-package #:tuatara.defaults)

(defvar *org-now* nil
  "The universal time Org's commands take as now; NIL for the clock's.")

(defun org-now ()
  "The universal time Org's commands take as now: *ORG-NOW*, else the clock's."
  (or *org-now* (get-universal-time)))

(defun org-today (&optional (now (org-now)))
  "Today as a day number, at the universal time NOW, any year."
  (nth-value 0 (time-day now)))

(define-record (org-ts (:conc-name org-ts-))
  "A timestamp read: ACTIVE (<> rather than []), YEAR MONTH DAY, HOUR and
MINUTE (NIL without a time), END-HOUR and END-MINUTE (a time range, else NIL),
the REPEATER (:cumulate +, :catch-up ++, :restart .+) with REPEAT-VALUE and
REPEAT-UNIT (#\\h #\\d #\\w #\\m #\\y), and the WARNING (:all -, :first --) with
WARNING-VALUE and WARNING-UNIT."
  (active nil :documentation "True when a timestamp is active, <> rather than [].")
  (year nil :documentation "The year of a timestamp.")
  (month nil :documentation "The month of a timestamp, 1 to 12.")
  (day nil :documentation "The day of the month of a timestamp.")
  (hour nil :documentation "The hour a timestamp begins at, or NIL when it has no time.")
  (minute nil :documentation "The minute a timestamp begins at, or NIL when it has no time.")
  (end-hour nil :documentation "The hour a timestamp's range of times ends at, or NIL.")
  (end-minute nil :documentation "The minute a timestamp's range of times ends at, or NIL.")
  (repeater nil :documentation "A timestamp's repeater: :CUMULATE (+), :CATCH-UP (++), :RESTART (.+), or NIL.")
  (repeat-value nil :documentation "How many units a timestamp repeats every, or NIL.")
  (repeat-unit nil :documentation "The unit a timestamp repeats by: #\\h #\\d #\\w #\\m or #\\y, or NIL.")
  (warning nil :documentation "A timestamp's warning: :ALL (-), :FIRST (--), or NIL.")
  (warning-value nil :documentation "How many units before its date a timestamp's warning begins, or NIL.")
  (warning-unit nil :documentation "The unit a timestamp's warning is given in: #\\h #\\d #\\w #\\m or #\\y, or NIL."))

(defparameter *org-timestamp-pattern*
  (format nil "^([<\\[])~a[]>]$" (org-timestamp-body t))
  "A timestamp of one date (ORG-TIMESTAMP-BODY): its kind, its date, its
times, and its repeaters and warnings, each a group.")

(defparameter *org-timestamp-scanner* (cl-ppcre:create-scanner *org-timestamp-pattern*)
  "*ORG-TIMESTAMP-PATTERN* made ready to match once, not again at each timestamp read.")

(defun org-read-ts (text)
  "TEXT, one timestamp (<2026-10-05 Mon 09:00 +1w -3d>), read as an ORG-TS, or
NIL; one whose time is none (25:00, a range ending 10:00-24:30) is none."
  (multiple-value-bind (match end starts ends) (cl-ppcre:scan *org-timestamp-scanner* text)
    (declare (ignore end))
    (when match
      (flet ((int (i) (and (aref starts i) (parse-integer text :start (aref starts i) :end (aref ends i)))))
        (multiple-value-bind (at hour minute end-hour end-minute)
            (and (aref starts 4) (time-range-at text (aref starts 4) (length text)))
          (when (or at (null (aref starts 4)))
            (let ((ts (make-org-ts :active (char= (char text 0) #\<)
                                   :year (int 1) :month (int 2) :day (int 3)
                                   :hour hour :minute minute :end-hour end-hour :end-minute end-minute)))
              (cl-ppcre:do-register-groups (mark value unit)
                ("(\\.\\+|\\+\\+|\\+|--|-)(\\d+)([hdwmy])" text nil :start (aref starts 8) :end (aref ends 8))
                (let ((value (parse-integer value)) (unit (char unit 0)))
                  (cond ((member mark '("+" "++" ".+") :test #'string=)
                         (setf (org-ts-repeater ts)
                               (cond ((string= mark "+") :cumulate) ((string= mark "++") :catch-up) (t :restart))
                               (org-ts-repeat-value ts) value (org-ts-repeat-unit ts) unit))
                        (t (setf (org-ts-warning ts) (if (string= mark "--") :first :all)
                                 (org-ts-warning-value ts) value (org-ts-warning-unit ts) unit)))))
              ts)))))))

(defun org-ts-day-number (ts)
  "The day of the timestamp TS as a day number, a date past its month's end
counted on into the next (2026-02-31 is 2026-03-03)."
  (day-number (org-ts-year ts) (org-ts-month ts) (org-ts-day ts)))

(defun org-ts-set-day (ts day)
  "Make TS's date the day number DAY, its time kept; TS is returned."
  (multiple-value-bind (year month date) (day-date day)
    (setf (org-ts-year ts) year (org-ts-month ts) month (org-ts-day ts) date))
  ts)

(defun org-ts-time (ts)
  "The universal time TS begins at (midnight when it has no time), any year (DAY-UNIVERSAL-TIME)."
  (day-universal-time (org-ts-day-number ts) (or (org-ts-hour ts) 0) (or (org-ts-minute ts) 0)))

(defun org-ts-universal-time (text)
  "The universal time of TEXT, one timestamp with a time of day
\([2026-10-01 Thu 17:30]), or NIL when it is none or names a day or time
that is not (2026-02-31, 25:00)."
  (let ((ts (org-read-ts text)))
    (and ts (org-ts-hour ts)
         (valid-date-p (org-ts-year ts) (org-ts-month ts) (org-ts-day ts))
         (org-ts-time ts))))

(defun org-ts-set-time (ts time)
  "Make TS's date (and its time, when it has one) that of the universal TIME, its time range kept as long."
  (multiple-value-bind (day hour minute) (time-day time)
    (when (and (org-ts-hour ts) (org-ts-end-hour ts))
      (let ((length (- (+ (* 60 (org-ts-end-hour ts)) (org-ts-end-minute ts))
                       (+ (* 60 (org-ts-hour ts)) (org-ts-minute ts)))))
        (multiple-value-bind (h m) (floor (+ (* 60 hour) minute length) 60)
          (setf (org-ts-end-hour ts) (mod h 24) (org-ts-end-minute ts) m))))
    (org-ts-set-day ts day)
    (when (org-ts-hour ts) (setf (org-ts-hour ts) hour (org-ts-minute ts) minute))
    ts))

(defun org-ts-shift (ts value unit)
  "Move TS by VALUE UNITs (#\\h #\\d #\\w #\\m #\\y). Months and years keep
the day of the month, a date past the month's end counted on into the next:
31 January a month on is 3 March (2 March in a leap year)."
  (case unit
    ((#\m #\y)
     (org-ts-set-day ts (day-number (if (char= unit #\y) (+ (org-ts-year ts) value) (org-ts-year ts))
                                    (if (char= unit #\m) (+ (org-ts-month ts) value) (org-ts-month ts))
                                    (org-ts-day ts))))
    (#\h (org-ts-set-time ts (+ (org-ts-time ts) (* value 3600))))
    (t (org-ts-set-day ts (+ (org-ts-day-number ts) (* value (if (char= unit #\w) 7 1))))))
  ts)

(defun org-ts-time-text (ts)
  "The time or range of times of TS as a timestamp writes it, 09:00 or
09:00-10:00 (TIME-RANGE-TEXT); NIL when it has none."
  (time-range-text (org-ts-hour ts) (org-ts-minute ts) (org-ts-end-hour ts) (org-ts-end-minute ts)))

(defun org-ts-text (ts)
  "TS written as a note writes a timestamp: <2026-10-05 Mon 09:00-10:00 +1w -3d>."
  (format nil "~:[[~;<~]~a~@[ ~a~]~@[ ~a~]~:[]~;>~]"
          (org-ts-active ts)
          (date-time-text (org-ts-day-number ts)
                          (org-ts-hour ts)
                          (org-ts-minute ts)
                          (org-ts-end-hour ts)
                          (org-ts-end-minute ts))
          (and (org-ts-repeater ts)
               (format nil "~a~d~c" (ecase (org-ts-repeater ts) (:cumulate "+") (:catch-up "++") (:restart ".+"))
                       (org-ts-repeat-value ts) (org-ts-repeat-unit ts)))
          (and (org-ts-warning ts)
               (format nil "~a~d~c" (if (eq (org-ts-warning ts) :first) "--" "-")
                       (org-ts-warning-value ts) (org-ts-warning-unit ts)))
          (org-ts-active ts)))

(defun org-day-timestamp-text (day &key active hour minute)
  "The day number DAY as an Org timestamp, any year: [2026-10-01 Thu], or
<2026-10-01 Thu> when ACTIVE, with HOUR and MINUTE when HOUR is given as
[2026-10-01 Thu 17:30]."
  (format nil (if active "<~a>" "[~a]") (date-time-text day hour minute)))

(defun org-timestamp-text (time &key active (clock t))
  "The Org timestamp of the universal TIME to the minute, any year: [2026-10-01
Thu 17:30], or <2026-10-01 Thu 17:30> when ACTIVE; without CLOCK, its day
alone, [2026-10-01 Thu]."
  (multiple-value-bind (day hour minute) (time-day time)
    (org-day-timestamp-text day :active active :hour (and clock hour) :minute minute)))

(defun org-ts-repeat (ts now)
  "Move TS, a timestamp with a repeater, to its next time after it is done at
the universal time NOW. +1w and ++1w go a week on until the time is past NOW.
A time missed is not owed: one done late goes on to its first time to come.
.+1w goes a week on from NOW. Hours of a timestamp of a day alone count from
its midnight, and it goes to the day of the time they come to. One repeating
every 0 stays."
  (let ((value (org-ts-repeat-value ts)) (unit (org-ts-repeat-unit ts)))
    (cond ((not (and value (plusp value))))
          ((eq (org-ts-repeater ts) :restart)
           (if (char= unit #\h)
               (org-ts-set-time ts (+ now (* value 3600)))
               (org-ts-shift (org-ts-set-day ts (org-today now)) value unit)))
          ((char= unit #\h)
           ;; in seconds: a timestamp of a day alone keeps no hour to step on from
           (let ((step (* value 3600)) (time (org-ts-time ts)))
             (org-ts-set-time ts (+ time (* step (1+ (max 0 (floor (- now time) step))))))))
          (t
           ;; days and weeks: most of the way in one step, the rest one at a time
           (let ((days (* value (case unit (#\d 1) (#\w 7) (t 0)))))
             (when (plusp days)
               (let ((steps (1- (floor (- now (org-ts-time ts)) (* days 86400)))))
                 (when (plusp steps) (org-ts-shift ts (* steps value) unit)))))
           (loop do (org-ts-shift ts value unit)
                 while (<= (org-ts-time ts) now)))))
  ts)

(defun org-ts-next (ts)
  "Move TS, a timestamp with a repeater, to the time it repeats at after its
own (ORG-TS-REPEAT); a timestamp of a day alone to the first after its day."
  (org-ts-repeat ts (if (org-ts-hour ts)
                        (org-ts-time ts)
                        (1- (day-universal-time (1+ (org-ts-day-number ts)))))))

(defun org-repeating-ts-p (ts)
  "True when TS, an ORG-TS or NIL, repeats: it has a repeater, and not of 0 (+0d)."
  (and ts (org-ts-repeater ts) (plusp (org-ts-repeat-value ts))))

(defun org-repeating-timestamp-in-p (text start end)
  "True when TEXT holds an active timestamp that repeats (ORG-REPEATING-TS-P)
from START below END."
  (cl-ppcre:do-matches (s e *org-active-timestamp-scanner* text nil :start start :end end)
    (when (org-repeating-ts-p (org-read-ts (subseq text s e)))
      (return t))))

(defun org-repeat-timestamps (text now &optional cancelled)
  "TEXT with each active timestamp in it that has a repeater moved to its next
time (ORG-TS-REPEAT), as when their task is done at the universal time NOW,
on past the times CANCELLED (ORG-OCCURRENCE-NOTES) cancels; and how many
were moved, as a second value. A repeater of 0 (+0d) repeats nothing."
  (let ((count 0))
    (values (cl-ppcre:regex-replace-all
             *org-active-timestamp-scanner* text
             (lambda (match &rest groups)
               (declare (ignore groups))
               (let ((ts (org-read-ts match)))
                 (if (org-repeating-ts-p ts)
                     (progn
                       (incf count)
                       (org-ts-repeat ts now)
                       (org-step-past ts #'org-ts-next
                                      (lambda (ts)
                                        (org-cancelled-p cancelled (org-ts-day-number ts) (org-ts-time-of-day ts))))
                       (org-ts-text ts))
                     match)))
             :simple-calls t)
            count)))

;;; One time of a repeat cancelled or moved, the rest going on: a note in
;;; the entry's log, as a list item, State "CANCELLED"  for [2026-10-30 Fri 08:00]
;;; [...] or State "MOVED"      for [2026-10-26 Mon] to [2026-10-27 Tue] [...].
;;; A time is an occurrence, (DAY . TIME): a day number, and the time of day
;;; as 930 for 09:30, or NIL for all that day.

(defparameter *org-repeat-steps* 1000
  "How many times of a repeat are stepped past at most, and how many notes of
times cancelled or moved an entry's log is read for, so that a log that
cancels them all ends the search.")

(defun org-step-past (start next skip)
  "The first of START and what NEXT, a function, gives after it in turn that
SKIP, a function, is false of, *ORG-REPEAT-STEPS* at most; NIL when NEXT
gives NIL first, or none is found."
  (loop repeat *org-repeat-steps*
        for x = start then (funcall next x)
        while x
        unless (funcall skip x) return x))

(defun org-time-number (hour minute)
  "HOUR and MINUTE as one number, 930 for 09:30; NIL when HOUR is."
  (and hour (+ (* 100 hour) (or minute 0))))

(defun org-time-hour-minute (time)
  "The hour and the minute of TIME, a time of day as ORG-TIME-NUMBER makes
it (930 for 09:30), as two values; NIL and NIL when TIME is NIL."
  (if time (floor time 100) (values nil nil)))

(defun org-ts-time-of-day (ts)
  "The time of day of TS, 930 for 09:30, or NIL when it has none."
  (org-time-number (org-ts-hour ts) (org-ts-minute ts)))

(defun org-occurrence-stamp (occurrence)
  "OCCURRENCE, (DAY . TIME), as a note writes it: [2026-10-30 Fri 08:00], or
[2026-10-30 Fri] when it has no time of day."
  (multiple-value-bind (hour minute) (org-time-hour-minute (cdr occurrence))
    (org-day-timestamp-text (car occurrence) :hour hour :minute minute)))

(defun org-occurrence-words (occurrence)
  "OCCURRENCE, (DAY . TIME), in words: 2026-10-30 Fri, or 2026-10-30 Fri 08:00."
  (multiple-value-bind (hour minute) (org-time-hour-minute (cdr occurrence))
    (date-time-text (car occurrence) hour minute)))

(defun org-occurrence-note (kind from to now)
  "The note an entry's log keeps of its time FROM, an occurrence, cancelled
\(KIND :cancelled) or moved to the occurrence TO (KIND :moved), at the
universal time NOW."
  (format nil "- State ~12a for ~a~@[ to ~a~] ~a"
          (if (eq kind :moved) "\"MOVED\"" "\"CANCELLED\"")
          (org-occurrence-stamp from) (and to (org-occurrence-stamp to)) (org-timestamp-text now)))

(defparameter *org-occurrence-note-scanner*
  (cl-ppcre:create-scanner (format nil "- State \"(CANCELLED|MOVED)\"[ \\t]+for (\\[~a\\])(?:[ \\t]+to (\\[~a\\]))?"
                                   (org-timestamp-body) (org-timestamp-body)))
  "A note of one time of a repeat cancelled or moved (ORG-OCCURRENCE-NOTE):
its kind, the time it was for and the one it went to, three groups.")

(defun org-occurrence-of (stamp)
  "The occurrence the inactive timestamp STAMP says, (DAY . TIME), or NIL."
  (let ((ts (org-read-ts stamp)))
    (and ts (cons (org-ts-day-number ts) (org-ts-time-of-day ts)))))

(defun org-occurrence-notes (text start end)
  "The times of an entry cancelled or moved one at a time, as the notes of
its log in TEXT from START below END say (ORG-OCCURRENCE-NOTE), as two
values: the occurrences cancelled; and the times moved, each (ORIGIN .
TARGET), ORIGIN where it was and TARGET where it is now, a time moved again
followed to its last place and one moved back to where it was dropped.
Notes standing alone on their lines are read, *ORG-REPEAT-STEPS* at most."
  (let ((notes '()) (count 0))
    (cl-ppcre:do-scans (s e starts ends *org-occurrence-note-scanner* text nil :start start :end end)
      (when (and (< count *org-repeat-steps*)
                 (blank-text-p text (string-line-start text s) s))
        (let ((from (org-occurrence-of (subseq text (aref starts 1) (aref ends 1))))
              (to (and (aref starts 2) (org-occurrence-of (subseq text (aref starts 2) (aref ends 2))))))
          (when from
            (incf count)
            (push (list (if (char= (char text (aref starts 0)) #\M) :moved :cancelled) from to) notes)))))
    ;; pushed in the order of the text, the newest note first: so oldest first now
    (let ((cancelled '()) (moved '()))
      (dolist (note notes)
        (destructuring-bind (kind from to) note
          (cond ((eq kind :cancelled) (push from cancelled))
                (to (let ((before (find from moved :key #'cdr :test #'equal)))
                      (if before (setf (cdr before) to) (push (cons from to) moved)))))))
      (values (nreverse cancelled)
              (nreverse (remove-if (lambda (m) (equal (car m) (cdr m))) moved))))))

(defun org-occurrence-is-p (occurrence day time)
  "True when OCCURRENCE, (DAY . TIME), is the time on the day number DAY at
TIME: that day with no time of its own, or at TIME."
  (and (= (car occurrence) day) (or (null (cdr occurrence)) (eql (cdr occurrence) time))))

(defun org-cancelled-p (cancelled day time)
  "True when CANCELLED, occurrences (ORG-OCCURRENCE-NOTES), cancel the time
on the day number DAY at TIME (930, or NIL)."
  (find-if (lambda (occurrence) (org-occurrence-is-p occurrence day time)) cancelled))

(defun org-moved-from-p (moved day time)
  "The move of MOVED (ORG-OCCURRENCE-NOTES) that took the time on the day
number DAY at TIME elsewhere, (ORIGIN . TARGET), or NIL."
  (find-if (lambda (move) (org-occurrence-is-p (car move) day time)) moved))

(defun org-moved-target (moved occurrence)
  "Where OCCURRENCE stands now as MOVED (ORG-OCCURRENCE-NOTES) moved it;
OCCURRENCE when it was not moved."
  (let ((move (org-moved-from-p moved (car occurrence) (cdr occurrence))))
    (if move (cdr move) occurrence)))

;;; Dates by rule: <%%(diary-float t 1 1) 10:00>, the first Monday of every
;;; month. The rule between the brackets is read as data, a few forms of
;;; diary rules that say which days, and never run.

(define-record (org-rule (:conc-name org-rule-))
  "A date by rule, read from a timestamp <%%(...)>: KIND, one of :FLOAT \(the
Nth or last weekday of a month), :CYCLIC (every N days), :BLOCK (every day from
one date to another), :ANNIVERSARY (a day each year), :DATE (the days whose
year, month and day are as given). KIND may also be :WEEKDAYS (the days of the
week listed, 0 for Sunday to 6 for Saturday), :ALL (the days all its rules take,
from one date to another say) or :ANY (the days any of them takes). ARGUMENTS,
what its form says, as ORG-READ-RULE reads them. And HOUR, MINUTE, END-HOUR and
END-MINUTE of the time written after it, or NIL."
  (kind nil
        :documentation "A date by rule's kind: :FLOAT, :CYCLIC, :BLOCK, :ANNIVERSARY, :DATE, :WEEKDAYS, :ALL or :ANY.")
  (arguments nil :documentation "What a date by rule's form says, as ORG-RULE-FROM-DATA reads it for its kind.")
  (hour nil :documentation "The hour of the time written after a date by rule, or NIL.")
  (minute nil :documentation "The minute of the time written after a date by rule, or NIL.")
  (end-hour nil :documentation "The hour the range of times after a date by rule ends at, or NIL.")
  (end-minute nil :documentation "The minute the range of times after a date by rule ends at, or NIL."))

(defparameter *org-rule-scanner* (cl-ppcre:create-scanner (format nil "^<~a>$" (org-rule-timestamp-body t)))
  "A timestamp by rule, whole (ORG-RULE-TIMESTAMP-BODY): its form and its
time or range of times, each a group.")

(defun org-read-rule-data (text start end)
  "What TEXT holds from START below END read as data, never run: numbers,
t and nil, names (as strings), and lists of them in brackets, a quote before
one passed over. Two values: the list of what it holds, and the index after
it; NIL for anything else."
  (let ((i start) (stack (list '())))
    (loop
      (loop while (and (< i end) (space-or-tab-char-p (char text i))) do (incf i))
      (when (>= i end)
        (return (and (null (rest stack)) (values (reverse (first stack)) i))))
      (let ((c (char text i)))
        (cond ((char= c #\') (incf i))
              ((char= c #\()
               (when (> (length stack) 3) (return nil))
               (push '() stack) (incf i))
              ((char= c #\))
               (when (null (rest stack)) (return nil))
               (let ((list (reverse (pop stack)))) (push list (first stack)))
               (incf i))
              (t (let* ((stop (or (position-if (lambda (ch) (member ch '(#\Space #\Tab #\( #\) #\')))
                                               text :start i :end end)
                                  end))
                        (word (subseq text i stop))
                        (value (cond ((cl-ppcre:scan "^-?[0-9]{1,6}$" word) (parse-integer word))
                                     ((string= word "t") t)
                                     ((string= word "nil") nil)
                                     ((cl-ppcre:scan "^[a-z][a-z-]*$" word) word)
                                     (t (return nil)))))
                   (push value (first stack))
                   (setf i stop))))))))

(defun org-rule-date (a b c)
  "A date of a rule's form, its parts A B C read as year, month, day when A
is a year (more than 31), else as month, day, year, the order the diary forms
take by default. Three values: year, month and day, each a number, T for
any, or a list of numbers."
  (if (and (integerp a) (> a 31)) (values a b c) (values c a b)))

(defun org-rule-part-p (part)
  "True when PART of a rule's form is T, a number or a list of numbers."
  (or (eq part t) (integerp part) (and (consp part) (every #'integerp part))))

(defparameter *org-rule-forms*
  '(("diary-float" "(diary-float MONTH WEEKDAY N), the Nth WEEKDAY of the month (a negative N counts from the
month's end): (diary-float t 1 1) is the first Monday of every month. MONTH is t, a number or a list of
numbers; WEEKDAY 0 for Sunday to 6; N from 1 to 5; a fourth number may name a day of the month")
    ("diary-cyclic" "(diary-cyclic N DATE), every N days, counting from the date given as year month day, or
as month day year")
    ("diary-block" "(diary-block FROM TO), every day from one date to another, each as three numbers")
    ("diary-anniversary" "(diary-anniversary DATE), the day of the year the month and day say, as
(diary-anniversary 10 5) or with its year")
    ("diary-date" "(diary-date MONTH DAY YEAR), the days those match; t stands for any, and a list for
several")
    ("memq" "(memq (calendar-day-of-week date) '(1 2 3 4 5)), the weekdays listed, 0 for Sunday to 6")
    ("and" "(and FORM FORM), the days all the forms take")
    ("or" "(or FORM FORM), the days any of the forms takes"))
  "The forms a date by rule may be, each its name and what it says. A date's
three numbers are read as year month day when the first is more than 31, else
as month day year.")

(defun org-rule-from-data (data)
  "DATA, a rule's form as ORG-READ-RULE-DATA reads it, as an ORG-RULE with no
time, or NIL when it is not one of those known: diary-float, diary-cyclic,
diary-block, diary-anniversary and diary-date, with numbers, t and lists of
numbers as their arguments. Also (memq (calendar-day-of-week date) '(1 2 3 4
5)), the days of the week listed; (and FORM ...), the days all its forms take,
and (or FORM ...), the days any of them takes, of these."
  (let ((name (first data)) (args (rest data)))
    (flet ((date (a b c)
             (multiple-value-bind (y m d) (org-rule-date a b c)
               (and (integerp y) (integerp m) (integerp d) (valid-date-p y m d) (day-number y m d)))))
      (cond ((not (stringp name)) nil)
            ((string= name "diary-float")
             (destructuring-bind (&optional month dayname n day &rest more) args
               (and (null more) (org-rule-part-p month) (org-rule-part-p dayname)
                    (integerp n) (<= 1 (abs n) 5) (or (null day) (and (integerp day) (<= 1 day 31)))
                    (every (lambda (d) (<= 0 d 6))
                           (if (integerp dayname) (list dayname) (if (consp dayname) dayname '())))
                    (make-org-rule :kind :float :arguments (list month dayname n day)))))
            ((string= name "diary-cyclic")
             (destructuring-bind (&optional n a b c &rest more) args
               (let ((start (and (integerp n) (plusp n) (null more) (date a b c))))
                 (and start (make-org-rule :kind :cyclic :arguments (list n start))))))
            ((string= name "diary-block")
             (destructuring-bind (&optional a b c d e f &rest more) args
               (let ((from (and (null more) (date a b c))) (to (date d e f)))
                 (and from to (<= from to) (make-org-rule :kind :block :arguments (list from to))))))
            ((string= name "diary-anniversary")
             (destructuring-bind (&optional a b c &rest more) args
               (when (and (null more) (integerp a) (integerp b) (or (null c) (integerp c)))
                 (multiple-value-bind (y m d) (if c (org-rule-date a b c) (values nil a b))
                   (and (integerp m) (integerp d) (<= 1 m 12) (<= 1 d (month-length 2000 m))
                        (make-org-rule :kind :anniversary :arguments (list m d y)))))))
            ((string= name "memq")
             (destructuring-bind (&optional of days &rest more) args
               (and (null more) (equal of '("calendar-day-of-week" "date"))
                    (consp days) (every (lambda (d) (and (integerp d) (<= 0 d 6))) days)
                    (make-org-rule :kind :weekdays :arguments (list (remove-duplicates days))))))
            ((string= name "diary-date")
             (destructuring-bind (&optional a b c &rest more) args
               (when (and (null more) (org-rule-part-p a) (org-rule-part-p b) (org-rule-part-p c))
                 (multiple-value-bind (y m d) (org-rule-date a b c)
                   (make-org-rule :kind :date :arguments (list y m d))))))
            ((member name '("and" "or") :test #'string=)
             (let ((parts (mapcar (lambda (part) (and (consp part) (org-rule-from-data part))) args)))
               (and parts (every #'identity parts)
                    (make-org-rule :kind (if (string= name "and") :all :any) :arguments (list parts)))))))))

(defun org-rule-text-p (text)
  "True when TEXT begins as a timestamp by rule does, <%%(."
  (and (> (length text) 4) (string= "<%%(" text :end2 4)))

(defun org-read-rule (text)
  "TEXT, a timestamp by rule (<%%(diary-float t 1 1) 10:00>), read as an
ORG-RULE, or NIL when it is none or its form is not one of those
ORG-RULE-FROM-DATA knows."
  (multiple-value-bind (match groups)
      (and (org-rule-text-p text) (cl-ppcre:scan-to-strings *org-rule-scanner* text))
    (when match
      (let* ((form (aref groups 0))
             (rule (org-rule-from-data (org-read-rule-data form 0 (length form)))))
        (when rule
          (if (null (aref groups 1))
              rule
              (multiple-value-bind (hour minute end-hour end-minute) (read-time-range (aref groups 1))
                (when hour
                  (setf (org-rule-hour rule) hour (org-rule-minute rule) minute
                        (org-rule-end-hour rule) end-hour (org-rule-end-minute rule) end-minute)
                  rule))))))))

(defun org-rule-part-text (part)
  "PART of a rule's form (T, a number or a list of numbers) as the form
writes it: t, 3, '(1 4 7 10)."
  (cond ((eq part t) "t")
        ((consp part) (format nil "'(~{~d~^ ~})" part))
        (t (format nil "~d" part))))

(defun org-rule-form-text (rule)
  "The form of RULE, an ORG-RULE, as a timestamp by rule writes it between
its brackets, ORG-RULE-FROM-DATA reading it back: diary-float t 1 1, and (or
(...) (...)) of several. Dates are written month, day, year, the order other
programs reading these forms take by default."
  (flet ((date (day)
           (multiple-value-bind (year month date) (day-date day)
             (format nil "~d ~d ~d" month date year))))
    (destructuring-bind (&optional a b c d) (org-rule-arguments rule)
      (ecase (org-rule-kind rule)
        (:float (format nil "diary-float ~a ~a ~d~@[ ~d~]" (org-rule-part-text a) (org-rule-part-text b) c d))
        (:cyclic (format nil "diary-cyclic ~d ~a" a (date b)))
        (:block (format nil "diary-block ~a ~a" (date a) (date b)))
        (:anniversary (format nil "diary-anniversary ~d ~d~@[ ~d~]" a b c))
        (:date (format nil "diary-date ~a ~a ~a" (org-rule-part-text b) (org-rule-part-text c) (org-rule-part-text a)))
        (:weekdays (format nil "memq (calendar-day-of-week date) ~a" (org-rule-part-text a)))
        ((:all :any) (format nil "~:[or~;and~]~{ (~a)~}" (eq (org-rule-kind rule) :all)
                             (mapcar #'org-rule-form-text a)))))))

(defun org-rule-text (rule)
  "RULE, an ORG-RULE, as a timestamp by rule, its time after its form:
<%%(diary-float t 1 1) 10:00>. ORG-READ-RULE reads it back."
  (format nil "<%%(~a)~@[ ~a~]>" (org-rule-form-text rule) (org-rule-time-text rule)))

(defun org-rule-part-has-p (part value)
  "True when PART of a rule (T, a number or a list of numbers) takes VALUE."
  (or (eq part t) (eql part value) (and (consp part) (member value part))))

(defun org-float-day (year month dayname n day)
  "The day number of the Nth DAYNAME (0 Sunday to 6 Saturday) of MONTH of
YEAR, counted from its DAY (the first when NIL), or with N below 0 the -Nth
last, counted back from its DAY (its last day when NIL); NIL when that day is
not in MONTH and no DAY was given."
  (let* ((length (month-length year month))
         (occurrence (if (plusp n)
                         (let ((from (day-number year month (or day 1))))
                           (+ from (mod (- dayname (weekday from)) 7) (* 7 (1- n))))
                         (let ((to (day-number year month (or day length))))
                           (- to (mod (- (weekday to) dayname) 7) (* 7 (1- (- n))))))))
    (if (or day (multiple-value-bind (y m) (day-date occurrence) (and (= y year) (= m month))))
        occurrence
        nil)))

(defun org-rule-matches-p (rule day)
  "True when RULE (an ORG-RULE) takes the day number DAY."
  (destructuring-bind (a &optional b c d) (org-rule-arguments rule)
    (ecase (org-rule-kind rule)
      (:float
       (let ((dayname b) (n c) (from d))
         (and (org-rule-part-has-p dayname (weekday day))
              (multiple-value-bind (year month) (day-date day)
                ;; counted from a day of the month, the Nth may fall in the month before or after
                (loop for shift in (if from '(0 -1 1) '(0))
                      thereis (multiple-value-bind (y m) (floor (+ (* 12 year) (1- month) shift) 12)
                                (let ((m (1+ m)))
                                  (and (org-rule-part-has-p a m)
                                       (eql day (org-float-day y m (weekday day) n from))))))))))
      (:cyclic (and (>= day b) (zerop (mod (- day b) a))))
      (:weekdays (and (member (weekday day) a) t))
      (:all (every (lambda (part) (org-rule-matches-p part day)) a))
      (:any (some (lambda (part) (org-rule-matches-p part day)) a))
      (:block (<= a day b))
      (:anniversary (multiple-value-bind (y m dd) (day-date day)
                      (and (= m a) (= dd b) (or (null c) (>= y c)))))
      (:date (multiple-value-bind (y m dd) (day-date day)
               (and (org-rule-part-has-p a y) (org-rule-part-has-p b m) (org-rule-part-has-p c dd)))))))

(defparameter *org-rule-reach* (* 8 366)
  "The most days looked through for a rule's next or last day: a day each
eight years, 29 February in a year that leaves one out, is still found.")

(defun org-rule-span (rule)
  "The first and last day RULE can take, as two values, each NIL for none:
those of a range of days in it, of the years it lists or the year an
anniversary begins, every other day open."
  (destructuring-bind (&optional a b c &rest more) (org-rule-arguments rule)
    (declare (ignore more))
    (case (org-rule-kind rule)
      (:block (values a b))
      (:anniversary (values (and c (day-number c 1 1)) nil))
      (:date (let ((years (cond ((integerp a) (list a)) ((consp a) a))))
               (if years
                   (values (day-number (reduce #'min years) 1 1) (day-number (reduce #'max years) 12 31))
                   (values nil nil))))
      (:all (let ((first nil) (last nil))
              (dolist (part a (values first last))
                (multiple-value-bind (from to) (org-rule-span part)
                  (when from (setf first (if first (max first from) from)))
                  (when to (setf last (if last (min last to) to)))))))
      (t (values nil nil)))))

(defun org-rule-step (rule)
  "How the days RULE can take are looked through: :YEAR when a year holds one
at most (a day of a month), :MONTH when a month does (a day of every month,
the Nth weekday of a month), NIL day by day."
  (destructuring-bind (&optional a b c d) (org-rule-arguments rule)
    (declare (ignore a))
    (case (org-rule-kind rule)
      (:anniversary :year)
      (:date (cond ((and (integerp b) (integerp c)) :year) ((integerp c) :month)))
      (:float (and (integerp b) (null d) :month)))))

(defun org-rule-candidate (rule year month)
  "The one day RULE can take in YEAR, or in MONTH of YEAR when it is looked
through by month (ORG-RULE-STEP), or NIL."
  (destructuring-bind (a &optional b c d) (org-rule-arguments rule)
    (declare (ignore d))
    (ecase (org-rule-kind rule)
      (:anniversary (and (valid-date-p year a b) (day-number year a b)))
      (:date (let ((month (if (integerp b) b month)))
               (and (valid-date-p year month c) (day-number year month c))))
      (:float (org-float-day year month b c nil)))))

(defun org-rule-search (rule from to)
  "The first day RULE takes from the day FROM to the day TO, looking back
when TO is before FROM, or NIL: by year or by month where it can
\(ORG-RULE-STEP), else day by day."
  (let ((by (if (<= from to) 1 -1)))
    (flet ((taken (day)
             (and day (if (= by 1) (<= from day to) (<= to day from)) (org-rule-matches-p rule day) day))
           (index (day step)
             (multiple-value-bind (year month) (day-date day)
               (if (eq step :year) year (+ (* 12 year) (1- month))))))
      (let ((step (org-rule-step rule)))
        (if (null step)
            (loop for k = from then (+ k by)
                  until (if (= by 1) (> k to) (< k to))
                  when (org-rule-matches-p rule k) return k)
            (loop for i = (index from step) then (+ i by)
                  until (if (= by 1) (> i (index to step)) (< i (index to step)))
                  thereis (taken (if (eq step :year)
                                     (org-rule-candidate rule i nil)
                                     (org-rule-candidate rule (floor i 12) (1+ (mod i 12)))))))))))

(defun org-rule-next (rule day)
  "The first day number at or after DAY that RULE takes, or NIL when none
does within *ORG-RULE-REACH* days or its range of days (ORG-RULE-SPAN)."
  (case (org-rule-kind rule)
    (:cyclic (destructuring-bind (n start) (org-rule-arguments rule)
               (if (<= day start) start (+ start (* n (ceiling (- day start) n))))))
    (:block (destructuring-bind (from to) (org-rule-arguments rule)
              (cond ((<= day from) from) ((<= day to) day))))
    (t (multiple-value-bind (first last) (org-rule-span rule)
         (let ((from (max day (or first day)))
               (to (min (+ day *org-rule-reach*) (or last (+ day *org-rule-reach*)))))
           (and (<= from to) (org-rule-search rule from to)))))))

(defun org-rule-previous (rule day)
  "The last day number at or before DAY that RULE takes, or NIL when none
does within *ORG-RULE-REACH* days or its range of days (ORG-RULE-SPAN)."
  (case (org-rule-kind rule)
    (:cyclic (destructuring-bind (n start) (org-rule-arguments rule)
               (and (>= day start) (- day (mod (- day start) n)))))
    (:block (destructuring-bind (from to) (org-rule-arguments rule)
              (cond ((>= day to) to) ((>= day from) day))))
    (t (multiple-value-bind (first last) (org-rule-span rule)
         (let ((from (min day (or last day)))
               (to (max (- day *org-rule-reach*) (or first (- day *org-rule-reach*)))))
           (and (>= from to) (org-rule-search rule from to)))))))

(defun org-rule-time-text (rule)
  "The time RULE says, as a timestamp writes it (10:00, 10:00-11:00), or NIL."
  (time-range-text (org-rule-hour rule) (org-rule-minute rule) (org-rule-end-hour rule) (org-rule-end-minute rule)))

(defun org-rule-time-of-day (rule)
  "The time of day RULE gives, 930 for 09:30, or NIL when it gives none."
  (org-time-number (org-rule-hour rule) (org-rule-minute rule)))

(defun org-rule-next-open (rule day cancelled &optional (next (lambda (day) (org-rule-next rule day))))
  "The first of RULE's days at or after the day number DAY that CANCELLED
\(ORG-OCCURRENCE-NOTES) does not cancel, or NIL; NEXT gives RULE's first day
at or after a day."
  (if (null cancelled)
      (funcall next day)
      (let ((time (org-rule-time-of-day rule)))
        (org-step-past (funcall next day) (lambda (day) (funcall next (1+ day)))
                       (lambda (day) (org-cancelled-p cancelled day time))))))

(defun org-rule-done-day (rule last-done today &optional cancelled)
  "The day RULE's task is done for when it is marked done today, TODAY:
the first of its days after LAST-DONE (a day number, or NIL when it has
never been done), or the last up to today when that is later, missed days
passed over; never done before, the one of its days nearest today. Days
CANCELLED cancels (ORG-OCCURRENCE-NOTES) are passed over. NIL when none of
its days is left after LAST-DONE."
  (let* ((time (org-rule-time-of-day rule))
         (before (org-step-past (org-rule-previous rule today) (lambda (day) (org-rule-previous rule (1- day)))
                                (lambda (day) (org-cancelled-p cancelled day time))))
         (before (if (and last-done before (<= before last-done)) nil before))
         (after (org-rule-next-open rule (if last-done (1+ last-done) today) cancelled)))
    (cond (last-done (if (and before after) (max before after) (or after before)))
          ((and before after) (if (<= (- today before) (- after today)) before after))
          (t (or before after)))))

(defun org-rule-timestamp-in (text start end)
  "The first timestamp by rule (ORG-READ-RULE) in TEXT from START below END:
its ORG-RULE, or NIL; its text as a second value."
  (cl-ppcre:do-matches (s e *org-rule-timestamp-scanner* text nil :start start :end end)
    (let ((rule (org-read-rule (subseq text s e))))
      (when rule (return (values rule (subseq text s e)))))))

(defun org-rule-goes-on-p (rule last-repeat now &optional cancelled)
  "True when a task by RULE, done at the universal time NOW, comes again: a
day of RULE is left after the one it is done for (ORG-RULE-DONE-TEXT),
LAST-REPEAT being what its LAST_REPEAT property says now, those CANCELLED
cancels (ORG-OCCURRENCE-NOTES) passed over."
  (let ((done (org-rule-done-text rule last-repeat now cancelled)))
    (and done (org-rule-next-open rule (1+ (org-ts-day-number (org-read-ts done))) cancelled) t)))

(defun org-rule-done-text (rule last-repeat now &optional cancelled)
  "What LAST_REPEAT says once a task by RULE is done at the universal time
NOW, LAST-REPEAT being what its property says now (when it was last done for,
or NIL): the day it is done for (ORG-RULE-DONE-DAY, those CANCELLED cancels
passed over) as an inactive timestamp, with the time RULE gives; NIL when
RULE takes no day near."
  (let* ((last (let ((ts (and last-repeat (org-read-ts last-repeat)))) (and ts (org-ts-day-number ts))))
         (day (org-rule-done-day rule last (org-today now) cancelled)))
    (and day (org-day-timestamp-text day :hour (org-rule-hour rule) :minute (org-rule-minute rule)))))

(setf (documentation 'make-org-ts 'function)
      "An ORG-TS of the parts given by their keywords, each as ORG-TS says; ORG-TS-TEXT writes it.")

;;; What a timestamp may hold

(defparameter *org-timestamp-units*
  '((#\h "hours") (#\d "days") (#\w "weeks") (#\m "months") (#\y "years"))
  "The units a repeater, a warning or a delay counts in, each its letter and its name.")

(define-command describe-timestamp-settings ()
  "Show what a timestamp in an Org note may hold that Tuatara reads: the
brackets that make it active or inactive, its times and ranges, repeaters,
warnings and delays, the planning lines SCHEDULED, DEADLINE and CLOSED, and
the dates by rule, with examples of each. The lines are filled to
*HELP-COLUMN*."
  (show-help
   (with-output-to-string (s)
     (write-help-section s "Timestamps"
                         :intro "\\<org-mode>\\[org-time-stamp] writes one at point, \\[org-time-stamp-inactive] an
                          inactive
                         one, \\[org-schedule] and \\[org-deadline] the planning lines. The day's name is any
                         word, or none."
                         :example "<2026-10-05 Mon 09:00-10:00 +1w -3d>")
     (write-help-section s "Active and inactive"
                         :rows '("<2026-10-05 Mon>" "active: the agenda lists the entry on that day"
                                 "[2026-10-05 Mon]" "inactive: a date in the text, not in the agenda"
                                 "<2026-10-05>--<2026-10-09>" "a range of days, listed on each; both ends
                                 active"
                                 "<2026-10-05 Mon 09:00>" "a time of day, one or two digits for the hour"
                                 "<2026-10-05 Mon 09:00-10:00>" "a range of times on the day")
                         :width 30)
     (write-help-section s "Repeaters"
                         :intro "After the time, a sign, a number and a unit letter:"
                         :rows (append
                                '("+1w" "again a week on from the date it had, as often as it is done
                                 (cumulate)"
                                  "++1w" "a week on from the date it had until that is still to come: a task done
                                  late goes on to its next date in the future (catch up)"
                                  ".+1w" "a week on from the day it is done (restart)"
                                  "+1w/2w" "a limit after a slash is accepted and not acted on")
                                (list "units" (format nil "~{~a~^, ~}"
                                                      (loop for (letter name) in *org-timestamp-units*
                                                            collect (format nil "~c ~a" letter name)))))
                         :width 14)
     (write-help-section s "Warnings and delays"
                         :rows '("-3d" "on a DEADLINE: the agenda lists it from 3 days before; with none,
                                 *ORG-DEADLINE-WARNING-DAYS* say how many"
                                 "--3d" "the same sign with two dashes, read alike"
                                 "SCHEDULED -3d" "a delay: the agenda does not list the task until 3 days after
                                 its date")
                         :width 16)
     (write-help-section s "Planning lines"
                         :intro "On the line right under a heading, in any order, each a word, a colon and a
                         timestamp:"
                         :rows '("SCHEDULED:" "when to start; the agenda lists the entry that day, and as
                                 overdue after it"
                                 "DEADLINE:" "when it is due; listed ahead of it as its warning says"
                                 "CLOSED:" "when it was done, an inactive timestamp set as the task is marked
                                 done (*ORG-LOG-DONE*); the agenda lists it in log mode")
                         :example "* TODO Report
SCHEDULED: <2026-10-05 Mon> DEADLINE: <2026-10-09 Fri -2d>"
                         :width 14)
     (write-help-section s "Dates by rule"
                         :intro "A timestamp <%%(FORM) 10:00> holds a rule that says which days, and an
                         optional time of day or range. The rule is read as data and never run; only these
                         forms are read."
                         :rows (loop for (name words) in *org-rule-forms* append (list name words))
                         :example "<%%(diary-float t 1 1) 10:00>"
                         :width 20)
     (write-help-section s "One time of a repeat"
                         :intro "A log line - State \"CANCELLED\" for [2026-10-12 Mon] under the entry
                         cancels that time of its repeat; - State \"MOVED\" for [2026-10-12 Mon] to
                         [2026-10-14 Wed] moves it."))))
