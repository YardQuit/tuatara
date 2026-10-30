;;;; lisp/org/agenda-view.lisp --- Org's agenda shown: the week, the TODO list, matches, and views of your own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; org-agenda (C-x a a) asks which view to show and shows it in *Org Agenda*, laid
;;;; out as Org lays its agenda out, line for line: the days of the span under
;;;; their dates, each entry after its category, time and kind (Scheduled:, In
;;;; 3 d.:), its tags at the window's edge; today's time grid and the time now;
;;;; the TODO list and the entries a match selects under their headers. The
;;;; lines are what agenda.lisp lists; the buffer is drawn again when an agenda
;;;; file changes, in a buffer or on disk. It is a list as every list is
;;;; (SHOW-LIST-TEXT), so that search filters narrow it.

(in-package #:tuatara.defaults)

(defvar *org-agenda-span* :week
  "How many days \\[org-agenda]'s agenda shows: :day, :week, :month or :year,
or a number of days.")

(defvar *org-agenda-start-on-weekday* 1
  "The day a week's agenda starts on, 1 for Monday to 6 for Saturday and 7 or 0
for Sunday; NIL to start on today.")

(defvar *org-agenda-time-grid*
  '((:daily :today :require-timed) (800 1000 1200 1400 1600 1800 2000) " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
  "Today's time grid in the agenda: (WHEN TIMES
AFTER-TIME LINE). WHEN holds :daily for a grid on every day of a day's agenda,
:today for one on today, :weekly for one on every day, :require-timed for none
on a day with no entry at a time of day; TIMES the times of the grid's lines
(800 for 8:00); AFTER-TIME what follows a time, on the grid's lines and the
entries' alike; LINE the grid's line. NIL for no grid.")

(defvar *org-agenda-category-width* 12
  "How wide the agenda's first column is, its category (the file's name, or
its #+CATEGORY:) and a colon, in columns: a longer one is cut short with …,
so that every line's time and text stand in one column.")

(defun org-agenda-category-column (category)
  "CATEGORY and a colon, *ORG-AGENDA-CATEGORY-WIDTH* columns wide: filled out
with spaces, or cut short with … when longer (TUATARA.DISPLAY:FIT-COLUMNS),
wide characters counted as the columns they take."
  (let* ((width (max 3 *org-agenda-category-width*))
         (shown (concatenate 'string (values (tuatara.display:fit-columns (princ-to-string category) '() (1- width))) ":")))
    (concatenate 'string shown (make-string (max 0 (- width (tuatara.display:string-columns shown))) :initial-element #\Space))))

(defvar *org-agenda-untimed-text* "all day"
  "What the agenda's time column says on a line with no time of day, faint:
\"all day\", so that every line's kind and text stand in one column; \"\"
leaves the column empty, lined up all the same; NIL leaves it out, the line's
text where the time would have been.")

(defvar *org-agenda-event-kind-text* "Event:"
  "What the agenda's column of kinds (Scheduled:, Deadline:) says on the line
of an event, a date in the text of an entry with no TODO keyword
\(:org-agenda-event-kind): Event:, so that every line says what it is and
its heading stands in one column, the kinds shorter than the column (1/3):
filled out to it; \"\" leaves the column empty, lined up all the same; NIL
leaves it out, the heading where the kind would have been, as Org's, a
task's too (*ORG-AGENDA-TASK-KIND-TEXT*).")

(defvar *org-agenda-task-kind-text* "Task:"
  "What the agenda's column of kinds says on the line of a date in the text
of a task, an entry with a TODO keyword, not on its SCHEDULED: or DEADLINE:
line: Task:, told apart from an event's (*ORG-AGENDA-EVENT-KIND-TEXT*, which
says whether the column is there at all).")

(defvar *org-agenda-current-time-string* "← now ───────────────────────────────────────────────"
  "The line that shows the time now in today's time grid; NIL for none.")

(defvar *org-agenda-refresh-minutes* 10
  "Minutes after which an agenda shown is drawn again by itself, so that the
time now in its grid and the days left to deadlines stay true; NIL for
never. It is drawn again just after midnight too, and whenever an agenda
file changes, in a buffer or on disk.")

(defvar *org-agenda-custom-commands*
  '(("u" "Untagged tasks" :tags-todo "-{.*}" :header "Untagged tasks"))
  "Views of your own that \\[org-agenda] offers beside its own: each (KEY
DESCRIPTION TYPE MATCH :header HEADER).
TYPE is :tags-todo (the tasks not done that MATCH, an Org match, selects),
:tags (every headline it selects) or :todo (the tasks with the keyword MATCH);
HEADER, when given, is the line above them. The standard one is u, the tasks
with no tag: -{.*}.")

(define-major-mode org-agenda-mode special-mode
  "The agenda: the days of a span with their entries, the TODO list, or the
entries a match selects (\\[org-agenda]). \\<org-agenda-mode>\\[org-agenda-switch-to] opens the entry on the line and
\\[org-agenda-goto] shows it in the other window; \\[org-agenda-later] and \\[org-agenda-earlier]
go to the next and the previous span, \\[org-agenda-goto-today] to today, \\[org-agenda-day-view] shows a
day; \\[org-agenda-log-mode] lists what was closed and clocked too; \\[revert-buffer] draws it again
from the agenda files as they are, \\[quit-window] leaves. It is drawn again by itself
too when an agenda file changes, in a buffer or on disk, every
*org-agenda-refresh-minutes* minutes and just after midnight.
\\`C-x a w', \\`C-x a m' and \\`C-x a y' show a week, a month and a year.
\\[search-filter-regex] narrows the agenda to the entries whose lines match what is typed, as
in every list, each date shown over the entries left under it and hidden with
none; the commands work on the entry on the line as before. The filters stay
while the agenda is drawn again (\\[revert-buffer], \\[org-agenda-log-mode], by itself) and go
when it shows another view or span.
\\<*tuatara-list-keymap*>\\[next-line] and \\[previous-line] move a line, \\[select-lines] selects lines and
\\[copy-selection-or-char] copies them; the keys that move by words or select parts of lines are not bound here."
  :lighter "Org-Agenda" :interactive nil)

(defvar *org-agenda-keymap* (mode-keymap 'org-agenda-mode)
  "Keys of the agenda (org-agenda-mode's keymap).")

;;; Days and weeks

(defun org-agenda-date-line (day)
  "The line of the date DAY heads its entries with: Monday     28 September
2026 W40, the week on
Mondays."
  (multiple-value-bind (year month dom) (day-date day)
    (format nil "~10a ~2d ~a ~4d~@[ W~2,'0d~]" (aref *weekday-names* (weekday day)) dom
            (aref *month-names* (1- month)) year
            (and (= (weekday day) 1) (nth-value 1 (iso-week day))))))

(defun org-agenda-span-days (span start)
  "How many days SPAN shows from the day START: 1 for :day, 7 for :week, the
days of START's month or year for :month and :year, or SPAN itself."
  (multiple-value-bind (year month) (day-date start)
    (case span
      (:day 1)
      (:week 7)
      (:month (month-length year month))
      (:year (if (leap-year-p year) 366 365))
      (t span))))

(defun org-agenda-span-start (span day)
  "The first day of the span SPAN holding the day DAY: DAY for a day, the week's
first day (*ORG-AGENDA-START-ON-WEEKDAY*), the first of the month or the year."
  (multiple-value-bind (year month) (day-date day)
    (case span
      (:week (if *org-agenda-start-on-weekday*
                 (week-start day *org-agenda-start-on-weekday*)
                 day))
      (:month (day-number year month 1))
      (:year (day-number year 1 1))
      (t day))))

(defun org-agenda-span-name (span)
  "SPAN's name in the agenda's first line: Day, Week, Month, Year, or 8 days."
  (case span (:day "Day") (:week "Week") (:month "Month") (:year "Year") (t (format nil "~d days" span))))

;;; Lines

(defun org-time-text (time)
  "The time of day TIME (930) written as the agenda writes it: 9:30."
  (multiple-value-bind (hour minute) (floor time 100)
    (format nil "~d:~2,'0d" hour minute)))

(defun org-agenda-time-column (time end)
  "The agenda's time column for an entry at TIME ending at END (or NIL): 14:00-15:00,
or  8:00 followed by the time grid's AFTER-TIME, twelve columns wide."
  (let ((text (if end
                  (format nil "~5@a-~a" (org-time-text time) (org-time-text end))
                  (format nil "~5@a~a" (org-time-text time) (or (third *org-agenda-time-grid*) "......")))))
    (format nil "~12a" text)))

(defun org-agenda-tags-text (entry)
  "ENTRY's tags as the agenda shows them: :home::work: for home inherited and
work its own, :home:: for home inherited and none its own, :work: for its own
alone; NIL for none. A list item's checkbox kept as a task has no tags of its
own, only those of its headline: :home:, nothing after them."
  (let* ((local (agenda-entry-local-tags entry))
         (inherited (remove-if (lambda (tag) (member tag local :test #'string=)) (agenda-entry-tags entry))))
    (cond ((and (null inherited) (null local)) nil)
          ((eq (agenda-entry-kind entry) :checkbox) (format nil ":~{~a~^:~}:" (agenda-entry-tags entry)))
          ((null inherited) (format nil ":~{~a~^:~}:" local))
          (t (format nil ":~{~a~^:~}::~{~a:~}" inherited local)))))

(defun org-agenda-item-text (item)
  "The text of the agenda line ITEM: its entry's headline as written, its
keyword and priority, without its tags, its time taken out where the time
column shows it."
  (let* ((entry (org-item-entry item))
         (heading (string-right-trim '(#\Space #\Tab) (cl-ppcre:regex-replace "[ \\t]+:[\\w@#%:]+:[ \\t]*$" (agenda-entry-heading entry) "")))
         (raw (agenda-entry-title entry)))
    (cond ((org-item-type-in item "closed" "clock") heading)
          ;; a repeating task done that day, its keyword the done one it was given
          ((org-item-type-in item "repeat-done" "cancelled" "moved" "check") (org-item-title item))
          ((and (<= (length raw) (length heading)) (string= raw heading :start2 (- (length heading) (length raw))))
           (concatenate 'string (subseq heading 0 (- (length heading) (length raw))) (org-item-title item)))
          (t (org-item-title item)))))

(defun org-agenda-deadline-face (item)
  "The face of a deadline line: imminent on its day and after, upcoming in the
second half of its warning, distant before."
  (destructuring-bind (&optional deadline own-warning repeats) (getf (agenda-entry-facts (org-item-entry item)) :deadline)
    (declare (ignore repeats))
    (if (null deadline)
        :org-upcoming-distant-deadline
        (let* ((diff (if (= deadline (org-item-day item)) 0 (- deadline (org-item-day item))))
               (fraction (- 1 (/ diff (max 1 (or own-warning *org-deadline-warning-days*))))))
          (cond ((>= fraction 1) :org-imminent-deadline)
                ((>= fraction 1/2) :org-upcoming-deadline)
                ((>= fraction 0) :org-upcoming-distant-deadline))))))

(defun org-agenda-event-over-p (item today)
  "True when ITEM is the line of an event (a timestamp or a range of days of
an entry that is no task) that is over: of a day before TODAY, or today with
its end, else its start, past now; a range's line today only on its last
day, whose time is its end."
  (let ((entry (org-item-entry item))
        (day (org-item-day item)))
    (and (org-item-type-in item "timestamp" "block")
         (null (agenda-entry-todo entry))
         (or (< day today)
             (and (= day today) (org-item-time item)
                  (if (org-item-type-in item "block")
                      (and (loop for (nil nil first last) in (getf (agenda-entry-facts entry) :ranges)
                                 thereis (and (= last today) (< first today)))
                           (< (org-item-time item) (org-agenda-now-time (org-now))))
                      (< (or (org-item-end item) (org-item-time item)) (org-agenda-now-time (org-now)))))))))

(defun org-agenda-item-face (item today)
  "The face of the agenda line ITEM, today being TODAY, by what it is: done,
scheduled, a deadline, an event; an event that is over faint, as one done
(ORG-AGENDA-EVENT-OVER-P)."
  (cond ((org-item-type-in item "closed" "repeat-done" "cancelled" "moved") :org-agenda-done)
        ((org-item-type-in item "clock") :org-agenda-calendar-event)
        ((org-item-type-in item "todo" "tagsmatch") nil)
        ((org-item-type-in item "check") :warning)
        ((org-done-p (org-item-entry item)) :org-agenda-done)
        ((org-item-type-in item "past-scheduled") :org-scheduled-previously)
        ((org-item-type-in item "scheduled") (if (eql (org-item-day item) today) :org-scheduled-today :org-scheduled))
        ((org-item-type-in item "deadline" "upcoming-deadline") (org-agenda-deadline-face item))
        ((org-agenda-event-over-p item today) :org-agenda-done)
        (t :org-agenda-calendar-event)))

(defun org-agenda-pieces-text (pieces)
  "The text of PIECES, each (TEXT . FACE), one after the other."
  (apply #'concatenate 'string (mapcar #'car pieces)))

(defun org-agenda-text-parts (text todo)
  "TEXT, an agenda item's, in its parts: its keyword (TODO when TEXT starts
with it), the text before a priority cookie ([#A]), the cookie and the text
after it; NIL for a part it lacks, the text after it all when it has no
cookie."
  (let ((keyword (and todo (uiop:string-prefix-p (concatenate 'string todo " ") text) todo)))
    (let ((rest (if keyword (subseq text (length todo)) text)))
      (multiple-value-bind (start end) (cl-ppcre:scan "\\[#[A-Z0-9]\\]" rest)
        (if start
            (values keyword (subseq rest 0 start) (subseq rest start end) (subseq rest end))
            (values keyword nil nil rest))))))

(defparameter *org-agenda-columns* '("category" "time" "kind" "keyword" "priority" "heading" "tags")
  "The columns of an agenda's entries a filter can try a pattern on alone
\(ORG-AGENDA-ITEM-CELLS).")

(defun org-agenda-item-cells (item &key (times t))
  "The texts of ITEM's line in the agenda's columns (*ORG-AGENDA-COLUMNS*): its
category, its time (unless TIMES is NIL), its kind (Scheduled:, In 3 d.:), its
keyword, priority cookie, heading and tags; empty where it has none."
  (let ((entry (org-item-entry item)))
    (multiple-value-bind (keyword before priority after)
        (org-agenda-text-parts (org-agenda-item-text item) (agenda-entry-todo entry))
      (vector (princ-to-string (agenda-entry-category entry))
              (cond ((not times) "")
                    ((org-item-time item) (string-trim " " (org-agenda-time-column (org-item-time item) (org-item-end item))))
                    (t (or *org-agenda-untimed-text* "")))
              (string-trim " " (or (org-item-extra item) ""))
              (or keyword "")
              (or priority "")
              (string-trim " " (concatenate 'string before after))
              (or (org-agenda-tags-text entry) "")))))

(defun org-agenda-item-pieces (item width today &key (times t))
  "The line of ITEM, WIDTH columns wide, as pieces (TEXT . FACE): its
category, its time (unless TIMES is NIL, as in the TODO list), its kind
(Scheduled:, In 3 d.:) in the face of what it is (ORG-AGENDA-ITEM-FACE), its
text with its keyword and priority coloured, its tags at the window's edge;
each part in a face of its own, so that they are told apart, a line done
faint throughout."
  (let* ((entry (org-item-entry item))
         (face (org-agenda-item-face item today))
         (done (eq face :org-agenda-done))
         (todo (agenda-entry-todo entry))
         (text (org-agenda-item-text item))
         (extra (org-item-extra item))
         (tags (org-agenda-tags-text entry))
         (pieces '()))
    (flet ((piece (text own) (push (cons text (if done face own)) pieces)))
      (piece "  " nil)
      (piece (org-agenda-category-column (agenda-entry-category entry)) :org-agenda-category)
      (cond ((not times))
            ((org-item-time item)
             (piece (org-agenda-time-column (org-item-time item) (org-item-end item)) :org-agenda-time))
            (*org-agenda-untimed-text*
             (piece (format nil "~12a" *org-agenda-untimed-text*) :org-agenda-untimed)))
      (cond ((plusp (length extra))
             (piece (if (and times *org-agenda-event-kind-text*) (format nil "~11a" extra) extra) face)
             (piece " " nil))
            ((and times *org-agenda-event-kind-text*)
             (piece (format nil "~11a" (if (and todo *org-agenda-task-kind-text*) *org-agenda-task-kind-text* *org-agenda-event-kind-text*))
                    :org-agenda-event-kind)
             (piece " " nil))))
    ;; the entry's own text in the text's colour, unless it is done
    (setf face (and done face))
    ;; the keyword in its face, a priority cookie in its own
    (multiple-value-bind (keyword before priority after) (org-agenda-text-parts text todo)
      (when keyword
        (let ((closed (eq (agenda-entry-todo-type entry) 'done)))
          (push (cons keyword (if (and (eq (agenda-entry-kind entry) :checkbox) (not closed))
                                  ;; a list item's checkbox kept as a task: a colour of its own
                                  :org-checkbox-todo
                                  (org-todo-keyword-face keyword closed)))
                pieces)))
      (when priority
        (push (cons before face) pieces)
        (push (cons priority :org-priority) pieces))
      (push (cons after face) pieces))
    (when tags
      (let ((used (text-columns (org-agenda-pieces-text pieces)))
            (column (- width 1 (text-columns tags))))
        (push (cons (make-string (max 1 (- column used)) :initial-element #\Space) face) pieces)
        (push (cons tags (or face :org-agenda-tags)) pieces)))
    (nreverse pieces)))

(defun org-agenda-grid-items (day items today now &optional (days 7))
  "The time grid's lines for the day DAY whose lines are ITEMS, today being
TODAY and the time of day now NOW (1427), in an agenda of DAYS days, as
*ORG-AGENDA-TIME-GRID* says: agenda items of the types grid and now."
  (destructuring-bind (&optional when times after line) *org-agenda-time-grid*
    (declare (ignore after))
    (when (and when
               (or (and (= day today) (member :today when))
                   (and (= days 1) (member :daily when))
                   (member :weekly when))
               (or (not (member :require-timed when)) (some #'org-item-time items)))
      (append (loop for time in times
                    collect (make-org-agenda-item :day day :type "grid" :extra "" :time time :title line))
              (and (= day today) *org-agenda-current-time-string*
                   (list (make-org-agenda-item :day day :type "now" :extra "" :time now
                                               :title *org-agenda-current-time-string*)))))))

(defun org-agenda-grid-pieces (item)
  "The line of the time grid's ITEM as pieces: its time and the grid's line, or the time now."
  (let ((face (if (org-item-type-in item "now") :org-agenda-current-time :org-time-grid)))
    (list (cons "  " nil)
          (cons (concatenate 'string (make-string 12 :initial-element #\Space)
                             (org-agenda-time-column (org-item-time item) nil) (org-item-title item))
                face))))

(defun org-agenda-now-time (now)
  "The time of day of the universal time NOW as 1427."
  (multiple-value-bind (day hour minute) (time-day now)
    (declare (ignore day))
    (+ (* 100 hour) minute)))

(defun org-agenda-todo-keywords (files)
  "The TODO keywords of FILES, each once, in the order of their sequences, the
first file's first."
  (let ((seen '()))
    (dolist (path files)
      (multiple-value-bind (source) (org-agenda-file-source path)
        (let ((text (org-agenda-source-text path source)))
          (when text
            (destructuring-bind (not-done . done) (org-todo-keywords-of text)
              (dolist (word (append not-done done))
                (pushnew word seen :test #'string=)))))))
    (nreverse seen)))

(defun org-agenda-view-lines (view width)
  "The lines of the agenda VIEW (a plist, ORG-AGENDA-VIEW) WIDTH columns
wide, as a list of (PIECES . WHAT): PIECES each (TEXT . FACE), WHAT the agenda
item the line shows, (:DAY N) for a date's line, (:GRID TIME) for a line of
the time grid, or NIL."
  (let* ((lines '())
         (today (org-today))
         (now (org-agenda-now-time (org-now)))
         (files (org-agenda-file-paths)))
    (flet ((line (pieces &optional what) (push (cons pieces what) lines))
           (structure (text) (list (cons text :org-agenda-structure))))
      (ecase (getf view :kind)
        (:agenda
         (let* ((span (getf view :span))
                (start (getf view :start))
                (count (org-agenda-span-days span start))
                (items (org-agenda-span start count :log (getf view :log)))
                (w1 (nth-value 1 (iso-week start))) (w2 (nth-value 1 (iso-week (+ start count -1)))))
           (line (structure (format nil "~a-agenda (W~2,'0d~:[-W~2,'0d~;~*~]):" (org-agenda-span-name span) w1 (= w1 w2) w2)))
           (loop for day from start below (+ start count)
                 do (let* ((day-items (loop while (and items (eql (org-item-day (first items)) day)) collect (pop items)))
                           (all (org-agenda-sort (append (org-agenda-grid-items day day-items today now count) day-items) '(:time :urgency)))
                           (weekend (member (weekday day) *calendar-weekend-days*)))
                      (line (list (cons (org-agenda-date-line day)
                                        (cond ((and weekend (= day today)) :org-agenda-date-weekend-today)
                                              (weekend :org-agenda-date-weekend)
                                              ((= day today) :org-agenda-date-today)
                                              (t :org-agenda-date))))
                            (list :day day))
                      (dolist (item all)
                        (if (org-item-type-in item "grid" "now")
                            (line (org-agenda-grid-pieces item) (list :grid (org-item-time item)))
                            (line (org-agenda-item-pieces item width today) item)))))))
        (:todo
         (let* ((keywords (org-agenda-todo-keywords files))
                (keyword (let ((wanted (getf view :keyword)))
                           (if (integerp wanted) (and (plusp wanted) (nth (1- wanted) keywords)) wanted)))
                (items (org-agenda-todo-list)))
           (when keyword
             (setf items (remove-if-not (lambda (item) (equal (agenda-entry-todo (org-item-entry item)) keyword))
                                        (org-agenda-sort
                                         (loop for path in files
                                               append (loop for entry in (org-agenda-entries path)
                                                            when (equal (agenda-entry-todo entry) keyword)
                                                              collect (org-agenda-item entry nil "todo" "" :heading
                                                                                       (1+ (agenda-entry-priority entry)) :keep-title t)))
                                         '(:urgency)))))
           (line (structure (format nil "Global list of TODO items of type: ~a" (or keyword "ALL"))))
           ;; the keywords to choose from, wrapped at the window's edge
           (let ((text (with-output-to-string (out)
                         (let ((column 0))
                           (flet ((put (s) (write-string s out) (incf column (length s))))
                             (put "Press ‘C-u N r’ (e.g. ‘C-u 0 r’) to search again:")
                             (loop for word in (cons (if keyword "ALL" "[ALL]")
                                                     (mapcar (lambda (k) (if (equal k keyword) (format nil "[~a]" k) k)) keywords))
                                   for n from 0
                                   for s = (format nil "(~d)~a" n word)
                                   do (when (> (+ column (length s) 1) width)
                                        (format out "~%~21@a" "")
                                        (setf column 21))
                                      (put " ") (put s)))))))
             (dolist (part (uiop:split-string text :separator '(#\Newline)))
               (line (list (cons part nil)))))
           (dolist (item items) (line (org-agenda-item-pieces item width today :times nil) item))))
        (:match
         (let ((items (org-agenda-match (getf view :match) :todo-only (getf view :todo-only)))
               (header (getf view :header)))
           (if header
               (line (structure header))
               (progn (line (structure (format nil "Headlines with TAGS match: ~a" (getf view :match))))
                      (line (list (cons "Press ‘C-u r’ to search again" nil)))))
           (dolist (item items) (line (org-agenda-item-pieces item width today :times nil) item))))
        (:check
         (let ((items (org-agenda-date-check)))
           (line (structure (format nil "Dates to look at: ~d" (length items))))
           (if items
               (dolist (item items) (line (org-agenda-item-pieces item width today :times nil) item))
               (line (list (cons "None: every entry's dates agree with what it is" nil))))))))
    (nreverse lines)))

;;; The buffer

(defun org-agenda-date-line-p (what)
  "True when WHAT, what an agenda line shows, is a date's line, (:DAY N)."
  (and (consp what) (eq (first what) :day)))

(defun org-agenda-grid-line-p (what)
  "True when WHAT, what an agenda line shows, is a line of the time grid, (:GRID TIME)."
  (and (consp what) (eq (first what) :grid)))

(defun org-agenda-buffer ()
  "The buffer *Org Agenda*, made when there is none: a list search filters
narrow (\\[search-filter-regex]), its dates heading the entries under them
and hidden with none left, the time grid's lines going with their date."
  (find-or-make-buffer "*Org Agenda*" :mode 'org-agenda-mode
                                      :made (lambda (buffer)
                                              (choose-display buffer 'line-numbers nil)
                                              (setf (tuatara.buffer:buffer-local buffer :read-only) t
                                                    (tuatara.buffer:buffer-local buffer :tuatara-support-keymap)
                                                    *tuatara-list-keymap*)
                                              (set-search-filtering buffer :heading #'org-agenda-date-line-p :empty-headings nil
                                                                           :decoration #'org-agenda-grid-line-p))))

(defun org-agenda-view (buffer)
  "What BUFFER's agenda shows, a plist: :KIND :agenda with :SPAN, :START
(a day number) and :LOG; :todo with :KEYWORD (a keyword, or N for the Nth
of those the agenda files have); or :match with :MATCH, :TODO-ONLY and
:HEADER."
  (tuatara.buffer:buffer-local buffer 'org-agenda-view))

(defun org-agenda-window-width (buffer)
  "The columns a window showing BUFFER (the selected one, else) has for text."
  (let ((window (or (buffer-window buffer)
                    (editor-window *editor*))))
    (- (tuatara.display:area-width window) (window-margin-for *editor* window))))

(defun org-agenda-sources-key ()
  "What tells whether an agenda file changed: each file's buffer version or time and size."
  (mapcar (lambda (path) (nth-value 1 (org-agenda-file-source path))) (org-agenda-file-paths)))

(defun org-agenda-compose (view width)
  "The agenda VIEW, WIDTH columns wide, as three values: its text, the spans
of its faces ((START END FACE) in octets) and what each line shows (a
vector, as ORG-AGENDA-VIEW-LINES has it)."
  (let ((lines (org-agenda-view-lines view width)))
    (multiple-value-bind (text spans)
        (pieces-text (loop for ((pieces . nil) . more) on lines
                           append pieces
                           when more collect (string #\Newline)))
      (values text spans (coerce (mapcar #'cdr lines) 'vector)))))

(defun org-agenda-apply (buffer view text spans whats sources point)
  "Put the agenda TEXT of VIEW, with its SPANS and WHATS (ORG-AGENDA-COMPOSE), in
BUFFER, drawn from the agenda files as SOURCES (ORG-AGENDA-SOURCES-KEY) found
them, narrowed again by the search filters on it (SHOW-LIST-TEXT). POINT
:keep keeps it on its line, :today puts it on today's date (or the first
date), :first on the first entry; a number puts it on that line."
  (let ((old-line (ignore-errors (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer)))))
    (show-list-text buffer text spans '() :entries whats :key sources :refill 'org-agenda-refill)
    (setf (tuatara.buffer:buffer-local buffer 'org-agenda-drawn-view) view
          (tuatara.buffer:buffer-local buffer 'org-agenda-drawn-at) (org-now))
    (let ((texts (make-hash-table :test 'eq)) (first nil) (times (eq (getf view :kind) :agenda)))
      (loop for what across whats
            when (org-agenda-item-p what)
              do (let ((cells (org-agenda-item-cells what :times times)))
                   (setf (gethash what texts) cells)
                   (unless first (setf first cells))))
      (keep-list-table buffer (coerce *org-agenda-columns* 'simple-vector) texts first))
    (let* ((shown (tuatara.buffer:buffer-local buffer 'list-entries #()))
           (first (tuatara.buffer:buffer-local buffer 'list-first-line 0))
           (last (max 0 (1- (tuatara.buffer:buffer-line-count buffer))))
           (line (if (integerp point)
                     (min point last)
                     (ecase point
                       (:keep (min (or old-line 0) last))
                       (:today (let ((today (position-if (lambda (what) (and (org-agenda-date-line-p what) (= (second what) (org-today))))
                                                         shown)))
                                 (if today (+ first today) (min (1+ first) last))))
                       (:first (let ((item (position-if #'org-agenda-item-p shown)))
                                 (if item (+ first item) 0)))))))
      (setf (tuatara.buffer:buffer-point buffer) (tuatara.buffer:buffer-goto-line buffer line)))))

(defun org-agenda-render (buffer &key (point :keep))
  "Draw BUFFER's agenda again from its view and the agenda files, at once.
POINT as ORG-AGENDA-APPLY has it; where point goes once an agenda worked out
in the background is put in (ORG-AGENDA-START-REFRESH) is forgotten."
  (let ((sources (org-agenda-sources-key))
        (view (copy-list (org-agenda-view buffer))))
    (multiple-value-bind (text spans whats) (org-agenda-compose view (org-agenda-window-width buffer))
      (setf (tuatara.buffer:buffer-local buffer 'org-agenda-refresh-point) nil)
      (org-agenda-apply buffer view text spans whats sources point))))

(defparameter *org-agenda-carried-settings*
  '(*org-now* *org-agenda-files* *org-agenda-file-functions* *org-agenda-time-grid* *org-agenda-category-width* *org-agenda-untimed-text* *org-agenda-event-kind-text* *org-agenda-task-kind-text* *org-agenda-date-in-title* *org-agenda-current-time-string*
    *org-deadline-warning-days* *org-agenda-start-on-weekday*)
  "The settings the agenda is worked out with, carried to the thread that works
it out in the background, and to the one marking its days in the calendar, as
they are on the editor's (TUATARA.RUNTIME:CARRY-SETTINGS).")

(defstruct (agenda-refresh (:include tuatara.runtime:job) (:constructor make-agenda-refresh (buffer view sources))
                           (:copier nil))
  "An agenda of BUFFER worked out again in the background, a job
\(TUATARA.RUNTIME:JOB): VIEW the view it is worked out for, SOURCES the agenda
files as it found them (ORG-AGENDA-SOURCES-KEY), known once it has looked; RESULT, once done, the text,
spans and lines ORG-AGENDA-COMPOSE gives, NIL when it gave up."
  view sources result)

(setf (documentation 'agenda-refresh-p 'function) "True when OBJECT is an AGENDA-REFRESH.")

(tuatara.runtime:define-job-words agenda-refresh
  :doing "Working out the agenda"
  :verb "work out the agenda"
  :left-words "the agenda is as it was"
  :at-exit :quiet)
(defmethod tuatara.runtime:job-holds-queue-p ((job agenda-refresh))
  "False: the agenda, worked out again in the background, keeps no other job of its buffer waiting."
  nil)

(defun org-agenda-start-refresh (buffer &key (point :keep))
  "Work BUFFER's agenda out again in the background, a job of BUFFER
\(AGENDA-REFRESH), from the agenda files as they are now, those open as
their buffers have them taken here and the rest looked at on the job's thread
\(ORG-AGENDA-OPEN-SNAPSHOT, ORG-AGENDA-SNAPSHOT-WITH), unless that is under way; the result is put in as the
job is taken in, point put as POINT says (as ORG-AGENDA-APPLY has it), or as
the last POINT other than :keep asked for meanwhile. An agenda never drawn
yet says it is being worked out."
  (unless (eq point :keep)
    (setf (tuatara.buffer:buffer-local buffer 'org-agenda-refresh-point) point))
  (when (zerop (tuatara.buffer:buffer-size buffer))
    (org-agenda-apply buffer nil "Working out the agenda..." '() #() nil 0))
  (unless (tuatara.buffer:buffer-local buffer 'org-agenda-refresh)
    (let* ((view (copy-list (org-agenda-view buffer)))
           (width (org-agenda-window-width buffer))
           (open (org-agenda-open-snapshot))
           (job (make-agenda-refresh buffer view nil)))
      (setf (tuatara.buffer:buffer-local buffer 'org-agenda-refresh) job)
      (tuatara.runtime:start-job-thread
       job (tuatara.runtime:carry-settings
            *org-agenda-carried-settings*
            (lambda (job)
              ;; an agenda that cannot be worked out fails as a job does, saying why:
              ;; asked for again when its files change
              (setf (agenda-refresh-result job)
                    (let ((*org-agenda-snapshot* (org-agenda-snapshot-with open)))
                      (setf (agenda-refresh-sources job) (mapcar #'third *org-agenda-snapshot*))
                      (multiple-value-list (org-agenda-compose view width))))))
       :name "tuatara agenda"))))

(defmethod tuatara.runtime:job-take-in ((job agenda-refresh))
  "Put in the agenda worked out in the background, when the view is still the
one it was worked out for, point where the view asked for it; start again
where another view was asked for meanwhile, or an agenda file shown changed.
One that failed or was stopped says so, in the agenda too when it was never
drawn, and is worked out again when its files change or another view is
asked for, not at every tick."
  (let ((buffer (agenda-refresh-buffer job))
        (view (agenda-refresh-view job))
        (sources (agenda-refresh-sources job))
        (result (agenda-refresh-result job)))
    (when (and (live-buffer-p buffer)
               (eq (tuatara.buffer:buffer-local buffer 'org-agenda-refresh) job))
      (setf (tuatara.buffer:buffer-local buffer 'org-agenda-refresh) nil)
      (if (not (eq (agenda-refresh-status job) :done))
          (let ((words (tuatara.runtime:job-ending-words job)))
            ;; an agenda never drawn says so in place of being worked out
            (unless (tuatara.buffer:buffer-local buffer 'org-agenda-drawn-view)
              (org-agenda-apply buffer nil words '() #() nil 0))
            ;; worked out again once its files change, not at every tick; another view asked for
            ;; meanwhile at once
            (setf (tuatara.buffer:buffer-local buffer 'list-key) sources)
            (message "~a" words)
            (unless (equal view (org-agenda-view buffer))
              (org-agenda-start-refresh buffer)))
          (let ((now (org-agenda-sources-key))
                (current (org-agenda-view buffer)))
            ;; a result of the files as they were: never put over an agenda drawn from them as they are
            (when (and result (equal view current) (equal sources now))
              (destructuring-bind (text spans whats) result
                (org-agenda-apply buffer view text spans whats sources
                                  (or (tuatara.buffer:buffer-local buffer 'org-agenda-refresh-point) :keep)))
              (setf (tuatara.buffer:buffer-local buffer 'org-agenda-refresh-point) nil))
            (when (or (and (not (equal view current))
                           (not (and (equal current (tuatara.buffer:buffer-local buffer 'org-agenda-drawn-view))
                                     (equal now (tuatara.buffer:buffer-local buffer 'list-key)))))
                      (and *editor*
                           (buffer-window buffer)
                           (not (equal now (tuatara.buffer:buffer-local buffer 'list-key)))
                           ;; a result that never came is asked for again only when the files changed since
                           (or result (not (equal now sources)))))
              (org-agenda-start-refresh buffer)))))))

(defun org-agenda-set-view (buffer view)
  "Make VIEW what BUFFER's agenda shows, the view it showed before and the
line point was on remembered for \\[org-agenda-previous-view] when it is another. Another
view or span, holding other entries, is shown whole: the search filters
narrowing the agenda (\\[search-filter-regex]) go."
  (let ((old (org-agenda-view buffer)))
    (when (and old (not (equal old view)))
      (setf (tuatara.buffer:buffer-local buffer 'org-agenda-previous)
            (cons old (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer))))
      (drop-search-filters buffer))
    (setf (tuatara.buffer:buffer-local buffer 'org-agenda-view) view)))

(defun org-agenda-show (view &key (point :today))
  "Show VIEW (as ORG-AGENDA-VIEW has it) in *Org Agenda*, drawn afresh."
  (let ((buffer (org-agenda-buffer)))
    (org-agenda-set-view buffer view)
    (unless (eq (tuatara.display:window-buffer (editor-window *editor*)) buffer)
      (show-buffer buffer))
    (org-agenda-redraw buffer :point point)
    buffer))

(defun org-agenda-week (&key (span *org-agenda-span*) (day (org-today)))
  "Show the agenda of the span SPAN holding the day DAY."
  (org-agenda-show (list :kind :agenda :span span :start (org-agenda-span-start span day)
                         :log (let ((buffer (tuatara.buffer:find-buffer "*Org Agenda*")))
                                (if (and buffer (eq (getf (org-agenda-view buffer) :kind) :agenda))
                                    (getf (org-agenda-view buffer) :log)
                                    *org-agenda-log-mode*)))))

(defun org-agenda-follow (buffer window)
  "Draw again the agenda BUFFER, shown in WINDOW, one of its agenda files
having changed, in a buffer or on disk (KEEP-SHOWN-CURRENT): at once in the
window selected, the agenda being where you are, while the files are small
\(*ORG-AGENDA-AT-ONCE*); else worked out in the background, so that typing
in an agenda file beside it never waits for it."
  (when (org-agenda-view buffer)
    (if (and (eq window (editor-window *editor*)) (not (tuatara.buffer:buffer-local buffer 'org-agenda-refresh))
             (org-agenda-files-small-p))
        (org-agenda-render buffer)
        (org-agenda-start-refresh buffer))))

(keep-shown-current 'org-agenda-mode (lambda (buffer) (declare (ignore buffer)) (org-agenda-sources-key)) 'org-agenda-follow)

(defun org-agenda-shown-window (editor)
  "The agenda shown in a window of EDITOR and that window, the selected one when
it shows it, as two values; NIL when no window shows the agenda."
  (let* ((buffer (tuatara.buffer:find-buffer "*Org Agenda*"))
         (selected (editor-window editor))
         (window (and buffer (if (eq (tuatara.display:window-buffer selected) buffer)
                                 selected
                                 (buffer-window buffer editor)))))
    (and window (values buffer window))))

(defvar *org-agenda-folders-kept* nil
  "The folders of the agenda files last worked out (ORG-AGENDA-FOLDERS):
\(FILES . FOLDERS), FILES the setting they came from.")

(defun org-agenda-folders ()
  "The folders the agenda files (ORG-AGENDA-FILES-IN-FORCE) are in, native names
ending in /: a folder listed, the folder of a pattern, and that of each file
listed, whether it is there yet or not. Worked out again only when the
setting changes."
  (let ((kept *org-agenda-folders-kept*) (files (org-agenda-files-in-force)))
    (if (and kept (equal (car kept) files))
        (cdr kept)
        (let ((folders (loop for file in files
                             for path = (ignore-errors (tuatara.io:native (tuatara.io:expand-path file)))
                             when path
                               collect (multiple-value-bind (parent name) (tuatara.io:path-folder-and-name path)
                                         (if (and (string/= name "") (not (find-if (lambda (c) (find c "*?")) name))
                                                  (eq :directory (tuatara.io:path-kind path)))
                                             (concatenate 'string path "/")
                                             parent)))))
          (setf folders (remove-duplicates folders :test #'string= :from-end t)
                *org-agenda-folders-kept* (cons (copy-tree files) folders))
          folders))))

(defun org-agenda-watched-folders ()
  "The folders of the agenda files, watched on disk while an agenda is shown."
  (and *editor* (org-agenda-shown-window *editor*) (org-agenda-folders)))

(defun org-agenda-folders-changed (changed)
  "Draw the agenda shown again when one of the folders CHANGED holds an agenda
file that changed, made or deleted there, as by another program
\(KEEP-SHOWN-LISTS-CURRENT)."
  (when (intersection changed (org-agenda-watched-folders) :test #'string=)
    (keep-shown-lists-current *editor*)))

(defun org-agenda-next-redraw (drawn)
  "The universal time an agenda drawn at DRAWN is drawn again by itself:
*ORG-AGENDA-REFRESH-MINUTES* later, or at the midnight after DRAWN, whichever
comes first."
  (let ((midnight (day-universal-time (1+ (org-today drawn)))))
    (if *org-agenda-refresh-minutes*
        (min midnight (+ drawn (max 1 (round (* 60 *org-agenda-refresh-minutes*)))))
        midnight)))

(defun org-agenda-redraw-due (buffer drawn)
  "The universal time the agenda BUFFER, drawn at DRAWN, is drawn again by
itself (ORG-AGENDA-NEXT-REDRAW), worked out again only when DRAWN or
*ORG-AGENDA-REFRESH-MINUTES* changed."
  (let ((kept (tuatara.buffer:buffer-local buffer 'org-agenda-redraw-due)))
    (if (and kept (eql (first kept) drawn) (eql (second kept) *org-agenda-refresh-minutes*))
        (third kept)
        (let ((due (org-agenda-next-redraw drawn)))
          (setf (tuatara.buffer:buffer-local buffer 'org-agenda-redraw-due) (list drawn *org-agenda-refresh-minutes* due))
          due))))

(defun org-agenda-refresh-tick (editor)
  "Tick function: draw the agenda a window of EDITOR shows again once its
time comes (ORG-AGENDA-REDRAW-DUE), as when an agenda file changes
\(ORG-AGENDA-FOLLOW). Wants a tick at that time; none with no agenda shown."
  (multiple-value-bind (buffer window) (org-agenda-shown-window editor)
    (when (and buffer (org-agenda-view buffer))
      (let* ((now (org-now))
             (drawn (or (tuatara.buffer:buffer-local buffer 'org-agenda-drawn-at) now)))
        (when (>= now (org-agenda-redraw-due buffer drawn))
          (setf drawn now
                (tuatara.buffer:buffer-local buffer 'org-agenda-drawn-at) now)
          (org-agenda-follow buffer window))
        (max 1 (- (org-agenda-redraw-due buffer drawn) now))))))

;;; Commands

(define-command org-agenda ()
  "Show the agenda, asking which view: \\`a' the agenda of the week (or
*ORG-AGENDA-SPAN*), \\`t' the TODO list, \\`m' the headlines a match selects
(work+urgent-home, TODO=\"NEXT\"), or a view of your own
(*ORG-AGENDA-CUSTOM-COMMANDS*, \\`u' the tasks with no tag), or \\`c' the
entries whose dates look wrong (\\[org-agenda-check-dates]), read from
*ORG-AGENDA-FILES*."
  (let* ((own (mapcar (lambda (c) (format nil "~a ~a" (first c) (second c))) *org-agenda-custom-commands*))
         (key (read-key (format nil "Agenda: a week  t TODO list  m match  c check dates~{  ~a~}" own)))
         (char (progn (when (quit-key-p key) (keyboard-quit-signal))
                      (setf (editor-message *editor*) nil)
                      (tuatara.frontend:key-base key)))
         (custom (and (characterp char) (find (string char) *org-agenda-custom-commands* :key #'first :test #'string=))))
    (cond ((eql char #\a) (org-agenda-week))
          ((eql char #\t) (org-agenda-show (list :kind :todo) :point :first))
          ((eql char #\m) (org-tags-view))
          ((eql char #\c) (org-agenda-check-dates))
          (custom (destructuring-bind (key description type match &key header) custom
                    (declare (ignore key))
                    (ecase type
                      (:tags-todo (org-agenda-show (list :kind :match :match match :todo-only t :header (or header description)) :point :first))
                      (:tags (org-agenda-show (list :kind :match :match match :header (or header description)) :point :first))
                      (:todo (org-agenda-show (list :kind :todo :keyword match) :point :first)))))
          (t (message "No agenda view on ~a" (tuatara.frontend:key-description (tuatara.frontend:key-spec key)))))))

(define-command org-agenda-list ()
  "Show the agenda of this week (or *ORG-AGENDA-SPAN*): each day with its
scheduled tasks, deadlines and timestamps."
  (org-agenda-week))

(define-command org-todo-list ()
  "Show the TODO list: every task not done in *ORG-AGENDA-FILES*.
\\<org-agenda-mode>\\[universal-argument] N \\[revert-buffer] there shows the tasks with the Nth keyword."
  (org-agenda-show (list :kind :todo) :point :first))

(define-command org-agenda-check-dates ()
  "Show the entries of *ORG-AGENDA-FILES* whose dates look wrong, each with
what is wrong (ORG-ENTRY-DATE-PROBLEM): Both: a date in its text and a
SCHEDULED: or DEADLINE: too; No repeat: a task with times done, moved or
cancelled one at a time whose dates no longer repeat; Plain date: a task with
a date in its text alone. Nothing is changed: \\<org-agenda-mode>\\[org-agenda-switch-to] opens the entry,
and the agenda's keys act on it as on any line."
  (org-agenda-show (list :kind :check) :point :first))

(define-command org-tags-view ()
  "Show the headlines of *ORG-AGENDA-FILES* an Org match selects, read in the
minibuffer: work, +work-home, work|proj, TODO=\"NEXT\", LEVEL>1, -{.*} for those
with no tag."
  (let ((match (string-trim " " (read-string "Match: "))))
    (if (zerop (length match))
        (message "No match given")
        (org-agenda-show (list :kind :match :match match) :point :first))))

(defun org-agenda-here ()
  "The agenda buffer when it is the current buffer, or NIL."
  (let ((buffer (current-buffer)))
    (and (typep (buffer-major-mode buffer) 'org-agenda-mode) (org-agenda-view buffer) buffer)))

(defun org-agenda-line-what (&optional (buffer (current-buffer)))
  "What the agenda line at point shows: an agenda item, (:DAY N), (:GRID
TIME), or NIL; among the lines search filters leave, while they narrow it."
  (list-entry-at buffer))

(defun org-agenda-day-at-point (buffer)
  "The day of the agenda line at point in BUFFER: of its item or date, or of the date above it; NIL outside a day's lines."
  (let ((whats (tuatara.buffer:buffer-local buffer 'list-entries #()))
        (first (tuatara.buffer:buffer-local buffer 'list-first-line 0)))
    (loop for index downfrom (- (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer)) first) to 0
          for what = (and (< index (length whats)) (svref whats index))
          when (org-agenda-date-line-p what) return (second what)
          when (and (org-agenda-item-p what) (org-item-day what)) return (org-item-day what))))

(defun org-agenda-change-span (span)
  "Show the agenda in SPAN (:day, :week...): from the day at point in the
agenda, or today's span elsewhere."
  (let* ((buffer (org-agenda-here))
         (view (and buffer (org-agenda-view buffer)))
         (day (or (and view (eq (getf view :kind) :agenda) (org-agenda-day-at-point buffer)) (org-today))))
    (org-agenda-week :span span :day day)))

(define-command org-agenda-day-view ()
  "Show the agenda of one day: the day at point in the agenda, else today."
  (org-agenda-change-span :day))

(define-command org-agenda-week-view ()
  "Show the agenda of a week: the week of the day at point in the agenda, else this week."
  (org-agenda-change-span :week))

(define-command org-agenda-month-view ()
  "Show the agenda of a month: the month of the day at point in the agenda, else this month."
  (org-agenda-change-span :month))

(define-command org-agenda-year-view ()
  "Show the agenda of a year: the year of the day at point in the agenda, else this year."
  (org-agenda-change-span :year))

(defun org-agenda-shift (direction)
  "Show the span after the agenda's (DIRECTION 1) or before it (-1), as many spans as the prefix argument says."
  (let* ((buffer (org-agenda-here))
         (view (and buffer (copy-list (org-agenda-view buffer)))))
    (if (not (and view (eq (getf view :kind) :agenda)))
        (message "Not in an agenda of days")
        (let* ((span (getf view :span)) (start (getf view :start)) (n (* direction (prefix))))
          (multiple-value-bind (year month) (day-date start)
            (setf (getf view :start)
                  (case span
                    (:day (+ start n))
                    (:week (+ start (* 7 n)))
                    (:month (day-number year (+ month n) 1))
                    (:year (day-number (+ year n) 1 1))
                    (t (+ start (* span n))))))
          (org-agenda-set-view buffer view)
          (org-agenda-redraw buffer :point :today)))))

(define-command org-agenda-later ()
  "Show the agenda's next span: the next week in a week's agenda, the next day in a day's."
  (org-agenda-shift 1))

(define-command org-agenda-earlier ()
  "Show the agenda's previous span: the previous week in a week's agenda, the previous day in a day's."
  (org-agenda-shift -1))

(define-command org-agenda-goto-today ()
  "Show the agenda's span holding today, point on today's date."
  (let* ((buffer (org-agenda-here)) (view (and buffer (copy-list (org-agenda-view buffer)))))
    (if (and view (eq (getf view :kind) :agenda))
        (progn (setf (getf view :start) (org-agenda-span-start (getf view :span) (org-today)))
               (org-agenda-set-view buffer view)
               (org-agenda-redraw buffer :point :today))
        (org-agenda-week))))

(define-command org-agenda-log-mode ()
  "List what was closed and clocked on each day too, or no longer;
*ORG-AGENDA-LOG-MODE* says how the agenda starts."
  (let* ((buffer (org-agenda-here)) (view (and buffer (copy-list (org-agenda-view buffer)))))
    (if (not (and view (eq (getf view :kind) :agenda)))
        (message "Not in an agenda of days")
        (progn (setf (getf view :log) (not (getf view :log))
                     (tuatara.buffer:buffer-local buffer 'org-agenda-view) view)
               (org-agenda-redraw buffer)
               (message "Log mode is ~:[off~;on~]" (getf view :log))))))

(defun org-agenda-refill (buffer)
  "Draw the agenda BUFFER again from the agenda files
\(\\[revert-buffer] there), narrowed by the search filters on it as before. In the
TODO list, with a prefix argument N, the tasks with its Nth keyword (0 for
all); in a match's list, with \\[universal-argument], asks for a new match: another view,
shown whole."
  (let ((view (copy-list (org-agenda-view buffer)))
        (argument (prefix-argument)))
    (cond ((null view) (org-agenda-week))
          ((and argument (eq (getf view :kind) :todo))
           (let ((n (if (integerp argument) argument 0)))
             (setf (getf view :keyword) (and (plusp n) n))
             (org-agenda-set-view buffer view)
             (org-agenda-redraw buffer :point :first)))
          ((and argument (eq (getf view :kind) :match) (not (getf view :header)))
           (org-tags-view))
          (t (org-agenda-redraw buffer)))))

(define-command org-agenda-previous-view ()
  "Go back to the view or span the agenda showed before this one, point on
the line it was on, as cd - goes back to the directory before: pressed
again, return. Moving to another span (\\<org-agenda-mode>\\[org-agenda-later], \\[org-agenda-earlier],
\\[org-agenda-goto-today], \\[org-agenda-day-view]) or view counts; drawing again does not."
  (let* ((buffer (org-agenda-here))
         (previous (and buffer (tuatara.buffer:buffer-local buffer 'org-agenda-previous))))
    (if (null previous)
        (message "No view before this one")
        (progn (org-agenda-set-view buffer (car previous))
               (org-agenda-redraw buffer :point (cdr previous))))))

(defun org-agenda-checkbox-start (buffer entry)
  "Where ENTRY's list item begins in BUFFER, ENTRY a checkbox item's: the line
it was on when the agenda read it when that line is still the item, else the
nearest line that is an item with a checkbox and that title; NIL when none is."
  (let* ((line (count #\Newline (agenda-entry-text entry) :end (agenda-entry-position entry)))
         (scanner (cl-ppcre:create-scanner
                   (format nil "^(?:[ \\t]*(?:[-+]|[0-9]+[.)])|[ \\t]+\\*)[ \\t]+\\[[ X-]\\][ \\t]*~a[ \\t]*\r?$"
                           (cl-ppcre:quote-meta-chars (agenda-entry-title entry)))))
         (guess (tuatara.buffer:buffer-goto-line buffer line)))
    (flet ((holds-p (start)
             (cl-ppcre:scan scanner (tuatara.buffer:buffer-string buffer :start start
                                                                        :end (tuatara.buffer:buffer-line-end buffer start)))))
      (if (holds-p guess)
          guess
          (let ((text (tuatara.buffer:buffer-string buffer)) (best nil) (best-distance nil) (here 0) (start 0))
            (loop while (< start (length text))
                  do (let ((end (string-line-end text start)))
                       (when (cl-ppcre:scan scanner text :start start :end end)
                         (let ((distance (abs (- here line))))
                           (when (or (null best) (< distance best-distance))
                             (setf best start best-distance distance))))
                       (setf start (1+ end))
                       (incf here)))
            (and best (tuatara.buffer:buffer-goto-line buffer (count #\Newline text :end best))))))))

(defun org-agenda-entry-start (buffer entry)
  "Where ENTRY's headline starts in BUFFER, visiting its file: where it was
when the agenda read it, else, the file changed since, the line holding that
headline whole nearest to it; NIL when no line does (the headline itself was
changed or taken away)."
  (when (eq (agenda-entry-kind entry) :checkbox)
    (return-from org-agenda-entry-start (org-agenda-checkbox-start buffer entry)))
  (let* ((heading (format nil "~a ~a" (make-string (or (agenda-entry-level entry) 1) :initial-element #\*) (agenda-entry-heading entry)))
         (line (count #\Newline (agenda-entry-text entry) :end (agenda-entry-position entry)))
         (guess (tuatara.buffer:buffer-goto-line buffer line)))
    (flet ((holds-p (start)
             (string= heading (string-right-trim '(#\Return)
                                                 (tuatara.buffer:buffer-string buffer :start start
                                                                                      :end (tuatara.buffer:buffer-line-end buffer start))))))
      (if (holds-p guess)
          guess
          ;; the file changed since: the nearest line that is that headline, whole
          (let* ((text (tuatara.buffer:buffer-string buffer))
                 (best nil) (best-distance nil))
            (loop for at = (search heading text) then (search heading text :start2 (1+ at))
                  while at
                  do (let ((after (+ at (length heading))))
                       (when (and (or (zerop at) (char= (char text (1- at)) #\Newline))
                                  (or (= after (length text)) (member (char text after) '(#\Newline #\Return))))
                         (let* ((here (count #\Newline text :end at))
                                (distance (abs (- here line))))
                           (when (or (null best) (< distance best-distance))
                             (setf best here best-distance distance))))))
            (and best (tuatara.buffer:buffer-goto-line buffer best)))))))

(defun org-agenda-visit-entry (item &key other)
  "Visit the file of ITEM's entry and put point on its headline, shown in the
selected window, or in another with OTHER."
  (let* ((entry (org-item-entry item))
         (buffer (visit-path (agenda-entry-file entry)
                             :show (lambda (b) (if other (pop-to-buffer b :other t) (show-buffer b)))))
         (start (or (org-agenda-entry-start buffer entry)
                    (progn (message "That entry is no longer in its file as it was: \\<org-agenda-mode>\\[revert-buffer] draws the agenda again")
                           (return-from org-agenda-visit-entry buffer)))))
    (goto-shown buffer start)
    buffer))

(defun org-agenda-item-at-point ()
  "The agenda item on the line at point, or NIL, having said so."
  (let ((what (and (org-agenda-here) (org-agenda-line-what))))
    (if (org-agenda-item-p what)
        what
        (progn (message "No entry on this line") nil))))

(define-command org-agenda-switch-to ()
  "Open the entry on the agenda's line in this window, point on its headline."
  (let ((item (org-agenda-item-at-point)))
    (when item (org-agenda-visit-entry item))))

(define-command org-agenda-goto ()
  "Show the entry on the agenda's line in the other window and go there."
  (let ((item (org-agenda-item-at-point)))
    (when item (org-agenda-visit-entry item :other t))))

;;; Changing entries from the agenda

(defvar *org-agenda-autosave-files* nil
  "The agenda files saved as soon as a change made from the agenda (a keyword,
a date, tags, a note, the clock) changes them: NIL for none
\(\\<org-agenda-mode>\\[org-agenda-save-all-buffers] saves them), T for every
agenda file, or a list of files, folders and patterns as *ORG-AGENDA-FILES*
takes them: '(\"~/notes/agenda.org\").")

(defun org-agenda-autosave-file-p (buffer)
  "True when BUFFER's file is one of *ORG-AGENDA-AUTOSAVE-FILES*, saved at once
after changes made from the agenda."
  (and *org-agenda-autosave-files* buffer (tuatara.buffer:buffer-file buffer)
       (member (tuatara.io:native (tuatara.buffer:buffer-file buffer))
               (org-agenda-file-paths (if (eq *org-agenda-autosave-files* t)
                                          (org-agenda-files-in-force)
                                          *org-agenda-autosave-files*))
               :test #'string=)))

(defun org-agenda-autosave (buffer)
  "Save BUFFER when it has changes not saved and its file is one of
*ORG-AGENDA-AUTOSAVE-FILES* (ORG-SAVE-SOON)."
  (when (and (org-agenda-autosave-file-p buffer) (tuatara.buffer:buffer-modified-p buffer))
    (org-save-soon buffer)))

(defparameter *org-agenda-at-once* (* 256 1024)
  "The most octets the agenda files may hold together for the agenda to be
drawn again at once after a change made from it; past that it is worked out
in the background, the change shown once that is done.")

(defun org-agenda-files-small-p ()
  "True when the agenda files hold at most *ORG-AGENDA-AT-ONCE* octets together."
  (<= (loop for key in (org-agenda-sources-key)
            sum (cond ((eq (first key) :buffer) (tuatara.buffer:buffer-size (second key)))
                      ((eq (first key) :file) (or (third key) 0))
                      (t 0)))
      *org-agenda-at-once*))

(defun org-agenda-redraw (buffer &key (point :keep))
  "Draw BUFFER's agenda again, of the view it is to show now: at once while
the agenda files are small (*ORG-AGENDA-AT-ONCE*), else worked out in the
background, the editor free meanwhile and the agenda shown once it is done.
POINT as ORG-AGENDA-APPLY has it."
  (if (org-agenda-files-small-p)
      (org-agenda-render buffer :point point)
      (org-agenda-start-refresh buffer :point point)))

(defun org-agenda-act (command &key elsewhere)
  "Run the Org command COMMAND on the entry of the agenda's line at point, in
its file's buffer (visited unseen when it is not open), point on its
headline, and draw the agenda again; the buffer is left with the change, to
be saved (\\<org-agenda-mode>\\[org-agenda-save-all-buffers]), or saved at once when it is
one of *ORG-AGENDA-AUTOSAVE-FILES*. With ELSEWHERE, in an Org buffer COMMAND
runs there as usual; in any other buffer it is said where it works."
  (cond ((org-agenda-here)
         (let ((agenda (current-buffer)) (item (org-agenda-item-at-point)))
           (when item
             (let* ((entry (org-item-entry item))
                    (buffer (tuatara.io:visit-file (agenda-entry-file entry)))
                    (start (org-agenda-entry-start buffer entry)))
               (cond
                 ((and (eq (agenda-entry-kind entry) :checkbox)
                       (member command '(org-schedule org-deadline org-set-tags-command)))
                  (message "A list item's checkbox has no ~a: a headline's task has (~a)"
                           (case command (org-schedule "schedule") (org-deadline "deadline") (t "tags"))
                           (agenda-entry-title entry)))
                 ((and (eq (agenda-entry-kind entry) :checkbox) (eq command 'org-refile))
                  (message "A list item's checkbox is not refiled: it moves with its heading, refiled from its file (~a)"
                           (agenda-entry-title entry)))
                 ((null start)
                  (message "That entry is no longer in its file as it was: \\<org-agenda-mode>\\[revert-buffer] draws the agenda again, nothing changed"))
                 (t (let ((before (first (tuatara.buffer:buffer-undo-stack buffer))))
                      ;; the entry may be outside the part of its file shown
                      (let ((*current-buffer* buffer))
                        (tuatara.buffer:with-narrowing-lifted (buffer)
                          (setf (tuatara.buffer:buffer-point buffer) start)
                          (funcall command)))
                      ;; the change remembered for undo from the agenda
                      (let ((group (first (tuatara.buffer:buffer-undo-stack buffer))))
                        (when (and group (not (eq group before)))
                          (push (cons buffer group) (tuatara.buffer:buffer-local agenda 'org-agenda-changes)))))
                    (org-agenda-autosave buffer)
                    (org-agenda-redraw agenda)))))))
        ((and elsewhere (typep (buffer-major-mode (current-buffer)) 'org-mode))
         (funcall command))
        (t (message "~(~a~) works on an entry of the agenda~:[~; or an Org file~]" command elsewhere))))

(define-command org-agenda-undo ()
  "Undo the last change made from this agenda, in the file it was made in,
and draw the agenda again; when the file was changed there since, nothing is
undone and you are told to undo in the file first."
  (let ((agenda (org-agenda-here)))
    (if (null agenda)
        (message "Only in an agenda")
        (let ((change (pop (tuatara.buffer:buffer-local agenda 'org-agenda-changes))))
          (destructuring-bind (&optional buffer . group) change
            (cond ((null change) (message "No change made from this agenda to undo"))
                  ((not (live-buffer-p buffer))
                   (message "The file of that change is no longer open: nothing undone"))
                  ((not (eq group (first (tuatara.buffer:buffer-undo-stack buffer))))
                   (push change (tuatara.buffer:buffer-local agenda 'org-agenda-changes))
                   (message "~a was changed since, in the file: undo there first" (tuatara.buffer:buffer-name buffer)))
                  (t (tuatara.buffer:buffer-undo buffer)
                     (org-agenda-autosave buffer)
                     (org-agenda-redraw agenda)
                     (message "Undone in ~a" (tuatara.buffer:buffer-name buffer)))))))))

(define-command org-agenda-todo ()
  "Change the TODO keyword of the entry on the agenda's line to its next, as
\\<org-mode>\\[org-todo] does in its file: a task done is noted with CLOSED:, one that
repeats moves on to its next date."
  (org-agenda-act 'org-todo))

(defun org-agenda-item-occurrence (item)
  "The time of the entry the agenda line ITEM stands for, as
*ORG-AGENDA-OCCURRENCE* takes it, an occurrence for a line on the day of
its time, NIL for one of a time before or to come listed today (Sched. 2x:,
In 3 d.:), which stands for the time open now."
  (and (or (org-item-type-in item "scheduled" "timestamp")
           (and (org-item-type-in item "deadline") (uiop:string-prefix-p "Deadline:" (org-item-extra item))))
       (cons (org-item-day item) (org-item-time item))))

(define-command org-agenda-cancel-task ()
  "Cancel the task on the agenda's line, as \\<org-mode>\\[org-cancel-task] does in its
file: its keyword made CANCELLED and noted with CLOSED:. One whose date
repeats asks: \\`t' cancels the time of the line alone, on whatever day it
is, the rest going on; \\`s' ends the series."
  (let ((item (org-agenda-item-at-point)))
    (if (and item (org-item-type-in item "cancelled" "repeat-done"))
        (message "That line is of a time ~:[done~;cancelled~] already" (org-item-type-in item "cancelled"))
        (let ((*org-agenda-occurrence* (and item (org-agenda-item-occurrence item))))
          (org-agenda-act 'org-cancel-task)))))

(define-command org-agenda-schedule ()
  "Schedule the entry on the agenda's line, the date asked for as
\\<org-mode>\\[org-schedule] asks it; with \\[universal-argument], take its scheduling away.
One whose date repeats asks: \\`t' moves the time of the line alone to the
day asked for, \\`s' schedules the series."
  (let ((item (org-agenda-item-at-point)))
    (if (and item (org-item-type-in item "cancelled" "repeat-done" "moved") (not (prefix-argument)))
        (message "That line is of a time ~a: its own line, if any, is the one to move"
                 (cond ((org-item-type-in item "cancelled") "cancelled") ((org-item-type-in item "moved") "moved away") (t "done")))
        (let ((*org-agenda-occurrence* (and item (org-agenda-item-occurrence item))))
          (org-agenda-act 'org-schedule)))))

(define-command org-agenda-deadline ()
  "Give the entry on the agenda's line a deadline, the date asked for as
\\<org-mode>\\[org-deadline] asks it; with \\[universal-argument], take it away. In an Org file, the
headline point is under."
  (org-agenda-act 'org-deadline :elsewhere t))

(define-command org-agenda-set-tags ()
  "Set the tags of the entry on the agenda's line, asked for as
\\<org-mode>\\[org-set-tags-command] asks them. In an Org file, the headline point is under."
  (org-agenda-act 'org-set-tags-command :elsewhere t))

(define-command org-agenda-add-note ()
  "Add a note to the entry on the agenda's line, as \\<org-mode>\\[org-add-note] adds one.
In an Org file, to the entry point is under."
  (org-agenda-act 'org-add-note :elsewhere t))

(defun org-agenda-clock-act (command)
  "Run the clock command COMMAND, then save the file the clock was in when it
is one of *ORG-AGENDA-AUTOSAVE-FILES* and the agenda is the buffer it was run in."
  (let ((buffer (and *org-clock* (org-clock-live-buffer *org-clock*)))
        (agenda (org-agenda-here)))
    (funcall command)
    (when agenda (org-agenda-autosave buffer))))

(define-command org-agenda-clock-out ()
  "Clock out, as \\<org-mode>\\[org-clock-out] does, from the agenda: the clock's file saved
at once when it is one of *ORG-AGENDA-AUTOSAVE-FILES*."
  (org-agenda-clock-act 'org-clock-out))

(define-command org-agenda-clock-cancel ()
  "Cancel the clock, as \\<org-mode>\\[org-clock-cancel] does, from the agenda: the clock's
file saved at once when it is one of *ORG-AGENDA-AUTOSAVE-FILES*."
  (org-agenda-clock-act 'org-clock-cancel))

(define-command org-agenda-clock-in ()
  "Clock in on the entry on the agenda's line, as \\[org-clock-in] does: a clock
running is clocked out first."
  ;; the clock running elsewhere is clocked out first: that file saved too, as the entry's is
  (let ((clocked (and *org-clock* (org-clock-live-buffer *org-clock*))))
    (org-agenda-act 'org-clock-in)
    (when (and clocked (org-agenda-here)) (org-agenda-autosave clocked))))

(sb-ext:defglobal **org-clock-looked-for** nil
  "True once Tuatara, as it started, began looking in the agenda files for
clocks left running (ORG-CLOCK-START-TICK): it looks once a session.")

(defun org-clock-start-tick (editor)
  "Tick function: once, as Tuatara starts, look in the agenda files for
clocks left running (ORG-CLOCKS-LEFT-OPEN) on a thread of their own, the
files open taken here, and take them up on the editor's (ORG-CLOCK-TAKE-UP),
so that the mode line shows the clock from the start. NIL: not again."
  (declare (ignore editor))
  (unless **org-clock-looked-for**
    (setf **org-clock-looked-for** t)
    (org-agenda-in-background "tuatara clocks left open" *org-agenda-carried-settings*
                              (lambda (snapshot)
                                (let ((found (let ((*org-agenda-snapshot* snapshot)) (org-clocks-left-open))))
                                  (when found
                                    (defer-action (lambda () (org-clock-take-up found))))))))
  nil)

(defun save-org-buffers-where (test)
  "Save each Org buffer visiting a file with changes not saved that TEST, a
function of the buffer, is true of; how many were saved."
  (save-buffers-where (lambda (buffer) (and (typep (buffer-major-mode buffer) 'org-mode) (funcall test buffer)))))

(define-command org-save-all-org-buffers ()
  "Save every Org buffer with changes not saved:
those the agenda changed among them, and any other. \\<org-agenda-mode>\\[org-agenda-save-all-buffers]
saves only the agenda files."
  (message "~[No Org buffer to save~;Saved 1 Org buffer~:;Saved ~:*~d Org buffers~]"
           (save-org-buffers-where (constantly t))))

(define-command org-agenda-save-all-buffers ()
  "Save the buffers of the agenda files (*org-agenda-files*, and those added to them) with changes not
saved, those the agenda changed among them; other Org buffers are left as they
are (\\[org-save-all-org-buffers] saves them all)."
  (let ((paths (org-agenda-file-paths)))
    (message "~[No agenda file to save~;Saved 1 agenda file~:;Saved ~:*~d agenda files~]"
             (save-org-buffers-where
              (lambda (buffer) (member (tuatara.io:native (tuatara.buffer:buffer-file buffer)) paths :test #'equal))))))

;;; The calendar and the agenda

(defun org-calendar-mark-source (first last)
  "For the calendar (*CALENDAR-MARK-FUNCTIONS*): on the editor's thread, the
agenda files open as their buffers have them (ORG-AGENDA-OPEN-SNAPSHOT) and
the agenda's settings; the function that, on a thread of its own, looks at
the others on disk (ORG-AGENDA-SNAPSHOT-WITH) and gives the days from FIRST to
LAST the agenda lists entries on. NIL with no agenda files."
  (let ((open (org-agenda-open-snapshot)))
    (and (org-agenda-files-in-force)
         (tuatara.runtime:carry-settings
          *org-agenda-carried-settings*
          (lambda ()
            (let* ((*org-agenda-snapshot* (org-agenda-snapshot-with open))
                   (*org-agenda-ts-memo* (make-hash-table :test 'equal))
                   (*org-agenda-time-memo* (make-hash-table :test 'equal))
                   (files (mapcar #'first *org-agenda-snapshot*))
                   (entries (mapcar #'org-agenda-entries files)))
              (loop for day from first to last
                    when (org-agenda-day day :files files :entries-by-file entries :log nil)
                      collect day)))))))

(define-command org-calendar-goto-agenda ()
  "Show the agenda of the week holding the calendar's day at point, point on
that day."
  (let* ((day (calendar-cursor-date))
         (buffer (org-agenda-week :day day))
         (whats (tuatara.buffer:buffer-local buffer 'list-entries #()))
         (index (position-if (lambda (what) (and (org-agenda-date-line-p what) (= (second what) day))) whats)))
    (when index
      (goto-shown buffer (tuatara.buffer:buffer-goto-line buffer (+ index (tuatara.buffer:buffer-local buffer 'list-first-line 0)))))))

(define-command org-agenda-goto-calendar ()
  "Show the calendar on the day of the agenda line at point."
  (let ((buffer (org-agenda-here)))
    (calendar-open (or (and buffer (org-agenda-day-at-point buffer)) (org-today)))))

(defun install-org-agenda-view (editor)
  "Bind the agenda's keys, undo's among them (those EDITOR runs undo by, and
u), and the calendar's key that shows the agenda of its day; and keep an
agenda shown current by itself: drawn again when an agenda file changes on
disk, its folder watched (ORG-AGENDA-FOLDERS-CHANGED), and every
*ORG-AGENDA-REFRESH-MINUTES* and at midnight (ORG-AGENDA-REFRESH-TICK); and,
as Tuatara starts, take up the clocks left running in the agenda files
\(ORG-CLOCK-START-TICK)."
  (dolist (key (cons "u" (where-is 'undo (list (editor-global-keymap editor)))))
    (define-key *org-agenda-keymap* key 'org-agenda-undo))
  (define-keys *org-agenda-keymap* '(("RET" org-agenda-switch-to) ("TAB" org-agenda-goto)
                                     ("l" org-agenda-later) ("h" org-agenda-earlier) ("g h" org-agenda-goto-today)
                                     ("g g" beginning-of-buffer) ("g e" end-of-buffer) ("g l" org-agenda-previous-view)
                                     ;; the views, spans and changes to entries, a key each
                                     ("a" org-agenda) ("w" org-agenda-week-view) ("m" org-agenda-month-view)
                                     ("Y" org-agenda-year-view) ("D" org-agenda-day-view)
                                     ("d" org-agenda-deadline) ("n" org-agenda-add-note) ("t" org-agenda-set-tags)
                                     ("I" org-agenda-clock-in) ("O" org-agenda-clock-out) ("C" org-agenda-clock-cancel)
                                     ("T" org-agenda-todo) ("L" org-agenda-log-mode)
                                     ("C-c C-t" org-agenda-todo) ("C-c C-x C" org-agenda-cancel-task) ("C-c C-s" org-agenda-schedule) ("C-c C-d" org-agenda-deadline)
                                     ("C-c C-q" org-agenda-set-tags) ("C-c C-z" org-agenda-add-note) ("C-c C-w" org-agenda-refile)
                                     ("s" org-agenda-save-all-buffers) ("c" org-agenda-goto-calendar)
                                     ("i" org-capture-on-day)))
  (pushnew 'org-agenda-refresh-tick *tick-functions*)
  (pushnew 'org-clock-start-tick *tick-functions*)
  (follow-directories-for 'org-agenda-watched-folders 'org-agenda-folders-changed)
  (pushnew 'org-calendar-mark-source *calendar-mark-functions*)
  (pushnew '(:calendar-marked org-calendar-goto-agenda "agenda") *calendar-day-openers* :test #'equal)
  (define-keys (mode-keymap 'calendar-mode) '(("a" org-calendar-goto-agenda) ("i" org-capture-on-day)))
  (commands-for-mode 'calendar-mode "the calendar (\\[calendar])" 'org-capture-on-day))

(commands-for-mode 'org-agenda-mode "The agenda (\\[org-agenda])"
                   'org-agenda-switch-to 'org-agenda-goto 'org-agenda-later 'org-agenda-earlier
                   'org-agenda-goto-today 'org-agenda-day-view 'org-agenda-log-mode 'org-agenda-previous-view
                   'org-agenda-todo 'org-agenda-schedule 'org-agenda-clock-in 'org-capture-on-day)
