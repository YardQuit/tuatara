;;;; test/org-internals.lisp --- Org's shared readers: #+STARTUP, the entries, CLOCK lines, the logbook, clocking in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test org-internals
  :description "What Org's features share: the #+STARTUP: lines of a buffer's first megabyte, the
entries of a file as the agenda and the clock table walk them (COMMENT and archived subtrees left
out), a CLOCK: line, where a logged line goes, and clocking in from any key.")

(defun org-internals-buffer (text)
  "A new buffer holding TEXT, not in Org mode, named for these tests."
  (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*org-internals*") :text text))

(defun org-internals-filler (octets)
  "Lines of text, no #+STARTUP: among them, OCTETS long or a line longer."
  (with-output-to-string (out)
    (loop for written from 0 below octets by 64
          do (format out "~63a~%" "# text, # and ### here, nothing to read"))))

(define-test (org-internals startup-lines-read)
  (flet ((options (text)
           (let ((buffer (org-internals-buffer text)))
             (unwind-protect (tuatara.defaults::org-startup-options buffer)
               (tuatara.buffer:kill-buffer buffer)))))
    ;; every #+STARTUP: line, in order, any case, as Org reads them all
    (is equal '("overview" "indent" "literallinks" "content")
        (options (format nil "#+TITLE: x~%#+STARTUP: overview~%* A~%  #+startup:  Indent   literallinks~%text~%#+STARTUP: content~%")))
    (is equal '() (options (format nil "* A~%text #+STARTUP: overview~%#+STARTUPS: content~%")))
    ;; past a large start of text, still in the first megabyte
    (is equal '("content") (options (concatenate 'string (org-internals-filler 600000) (format nil "#+STARTUP: content~%"))))
    ;; not past the first megabyte
    (is equal '("overview")
        (options (concatenate 'string (format nil "#+STARTUP: overview~%") (org-internals-filler (* 1100 1024))
                              (format nil "#+STARTUP: content~%")))))
  ;; read where the text is kept: a buffer of 3 MB, its lines copied out, not its text
  (let ((buffer (org-internals-buffer (concatenate 'string (format nil "#+STARTUP: content~%") (org-internals-filler (* 3 1024 1024))))))
    (unwind-protect
         (progn
           (is equal '("content") (tuatara.defaults::org-startup-options buffer))
           ;; the least of a few readings, other threads' work left out: a copy of the text is 3 MB or more each time
           (let ((consed (loop repeat 3
                               minimize (let ((before (sb-ext:get-bytes-consed)))
                                          (tuatara.defaults::org-startup-options buffer)
                                          (- (sb-ext:get-bytes-consed) before)))))
             (true (< consed (* 256 1024)) "consed ~:d octets" consed)))
      (tuatara.buffer:kill-buffer buffer)))
  ;; the last word of a kind says, as in Org: folding and the indented view
  (let ((tuatara-user:*org-startup-indented* nil))
    (dolist (case `((,(format nil "#+STARTUP: overview~%#+STARTUP: content~%* A~%** B~%") :contents nil)
                    (,(format nil "#+STARTUP: content overview indent~%* A~%** B~%") :overview t)
                    (,(format nil "#+STARTUP: noindent~%#+STARTUP: indent showall~%* A~%** B~%") nil t)))
      (destructuring-bind (text view indented) case
        (let ((buffer (org-internals-buffer text)))
          (unwind-protect
               (progn
                 (tuatara.modes:set-major-mode buffer 'tuatara.defaults::org-mode)
                 (is eq view (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-global-view) "~s" text)
                 (is eq indented (tuatara.defaults::org-indented-p buffer) "~s" text))
            (tuatara.buffer:kill-buffer buffer)))))))

(defparameter *org-internals-clock-lines*
  ;; each line, then what Org 9.7.11 makes of it (batch Emacs, org-element and org-clock-sum):
  ;; a clock or not, its status, duration and timestamp, and the minutes the clock table counts
  `(("CLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00"
     t "closed" "1:00" "[2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00]" 60)
    ("CLOCK: [2026-10-01 Thu 17:00]" t "running" nil "[2026-10-01 Thu 17:00]" 0)
    ("  CLOCK: [2026-10-01 Thu 17:10]--[2026-10-01 Thu 17:20]"
     t "running" nil "[2026-10-01 Thu 17:10]--[2026-10-01 Thu 17:20]" 10)
    ("CLOCK: => 0:07" t "closed" "0:07" nil 7)
    ("CLOCK:   [2026-10-01 Thu 16:00]---[2026-10-01 Thu 16:10] =>  0:10" nil nil nil nil 0)
    (,(format nil "CLOCK:~c[2026-10-01 Thu 11:00]--[2026-10-01 Thu 11:45]~c=>~c0:45  " #\Tab #\Tab #\Tab)
     t "running" nil "[2026-10-01 Thu 11:00]--[2026-10-01 Thu 11:45]" 45)
    ("clock: [2026-10-01 Thu 12:00]--[2026-10-01 Thu 12:30] =>  0:30"
     t "closed" "0:30" "[2026-10-01 Thu 12:00]--[2026-10-01 Thu 12:30]" 30)
    ("CLOCK: [2026-10-01 Thu 13:00]--[2026-10-01 Thu 13:20] => 0:20 late" nil nil nil nil 0)
    ("CLOCK:    [2026-10-01 Thu 14:00]--[2026-10-01 Thu 14:05]   =>    0:05"
     t "closed" "0:05" "[2026-10-01 Thu 14:00]--[2026-10-01 Thu 14:05]" 5)
    ("CLOCK:[2026-10-01 Thu 15:00]--[2026-10-01 Thu 15:30] =>  0:30" nil nil nil nil 0))
  "CLOCK: lines written well and oddly, and what Org reads of each.")

(define-test (org-internals clock-lines-read-alike)
  (dolist (case *org-internals-clock-lines*)
    (destructuring-bind (line clock-p status duration value minutes) case
      (let* ((text (format nil "* Task~%~a~%" line))
             (tree (tuatara.defaults::org-parse text))
             (headline (first (tuatara.defaults::org-node-children tree)))
             (section (find 'tuatara.defaults::section (tuatara.defaults::org-node-children headline) :key #'tuatara.defaults::org-node-type))
             (node (first (tuatara.defaults::org-node-children section))))
        ;; the parse
        (is eq clock-p (eq (tuatara.defaults::org-node-type node) 'tuatara.defaults::clock) "a clock: ~s" line)
        (when clock-p
          (is string-equal status (symbol-name (tuatara.defaults::org-property node :status)) "status: ~s" line)
          (is equal duration (tuatara.defaults::org-property node :duration) "duration: ~s" line)
          (is equal value (tuatara.defaults::org-property node :value) "value: ~s" line))
        ;; the reader the rest use
        (multiple-value-bind (from to how-long) (tuatara.defaults::org-read-clock-line line)
          (is eq clock-p (and (or from how-long) t) "read: ~s" line)
          (is equal value (and from (if to (format nil "~a--~a" from to) from)) "read: ~s" line)
          (is equal duration how-long "read: ~s" line))
        ;; the clock table's minutes, as org-clock-sum counts them
        (let ((tuatara.defaults::*org-text* text))
          (is = minutes (tuatara.defaults::clocktable-minutes headline text nil nil) "minutes: ~s" line)))))
  ;; the agenda's log: a clock line written with CLOCK: and a space, how long and when it stopped when it says both,
  ;; and whether it is still open, no end written
  (let* ((text (format nil "* Task~%~{~a~%~}" (mapcar #'first *org-internals-clock-lines*)))
         (entry (first (tuatara.defaults::org-agenda-file-entries "/x/lines.org" text))))
    (is equal '((:clock "[2026-10-01 Thu 09:00]" "1:00" "10:00" nil)
                (:clock "[2026-10-01 Thu 17:00]" nil nil t)
                (:clock "[2026-10-01 Thu 17:10]" nil nil nil)
                (:clock "[2026-10-01 Thu 14:00]" "0:05" "14:05" nil))
        (tuatara.defaults::agenda-entry-logs entry)))
  ;; C-c C-c on a clock line with two times writes how long, the rest of the line as it was
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026)))
    (is string= (format nil "* T~%  CLOCK: [2026-10-01 Thu 17:10]--[2026-10-01 Thu 17:20] =>  0:10~%")
        (org-text-after "j C-c C-c" (format nil "* T~%  CLOCK: [2026-10-01 Thu 17:10]--[2026-10-01 Thu 17:20]~%")))
    (is string= (format nil "* T~%CLOCK:    [2026-10-01 Thu 14:00]--[2026-10-01 Thu 14:05] =>  0:05~%")
        (org-text-after "j C-c C-c" (format nil "* T~%CLOCK:    [2026-10-01 Thu 14:00]--[2026-10-01 Thu 14:05]   =>    1:05~%")))))

(defparameter *org-internals-skipping*
  (format nil "#+FILETAGS: :f:~%* TODO Plain task :b:~%SCHEDULED: <2026-10-01 Thu>~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 09:00]--[2026-10-01 Thu 10:00] =>  1:00~%:END:~%** TODO Child with a tag twice :f:c:b:~%SCHEDULED: <2026-10-01 Thu>~%* COMMENT TODO Commented task~%SCHEDULED: <2026-10-01 Thu>~%CLOCK: [2026-10-01 Thu 11:00]--[2026-10-01 Thu 11:30] =>  0:30~%** TODO Under a commented one :b:~%DEADLINE: <2026-10-01 Thu>~%* TODO Archived task :ARCHIVE:~%SCHEDULED: <2026-10-01 Thu>~%CLOCK: [2026-10-01 Thu 13:00]--[2026-10-01 Thu 13:45] =>  0:45~%** TODO Under an archived one :b:~%<2026-10-01 Thu 15:00>~%* TODO After them~%<2026-10-01 Thu 16:00>~%CLOCK: [2026-10-01 Thu 17:00]--[2026-10-01 Thu 17:30] =>  0:30~%# CLOCK: [2026-10-01 Thu 19:00]--[2026-10-01 Thu 19:05] =>  0:05~%# a comment <2026-10-01 Thu 20:00>~%")
  "An agenda file with tasks under a COMMENT headline and an archived one, a tag given twice, and comment lines.")

(define-test (org-internals comment-and-archive-skipped-as-org-does)
  ;; Org 9.7.11's agenda (batch Emacs) on this file for Thursday 2026-10-01, 18:00: what is under
  ;; COMMENT or ARCHIVE left out, and a comment line's timestamp and clock; tags inherited first,
  ;; a tag given twice where it is given last; a file whose #+FILETAGS: has ARCHIVE left out whole
  (with-test-directory (dir)
    (let ((file (namestring (merge-pathnames "skip.org" dir)))
          (archived (namestring (merge-pathnames "archived.org" dir)))
          (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
          (today (tuatara.dates:day-number 2026 10 1)))
      (alexandria:write-string-into-file *org-internals-skipping* file :if-exists :supersede)
      (alexandria:write-string-into-file (format nil "#+FILETAGS: :ARCHIVE:~%* TODO In an archived file~%SCHEDULED: <2026-10-01 Thu>~%CLOCK: [2026-10-01 Thu 08:00]--[2026-10-01 Thu 08:30] =>  0:30~%")
                                         archived :if-exists :supersede)
      (flet ((rows (items)
               (mapcar (lambda (item) (let ((entry (tuatara.defaults::org-item-entry item)))
                                        (list (tuatara.defaults::org-item-type item) (tuatara.defaults::agenda-entry-title entry)
                                              (tuatara.defaults::agenda-entry-tags entry))))
                       items)))
        (let ((files (list file archived)))
          ;; the day's lines (Org calls a task scheduled for today at 18:00 past-scheduled, a matter of its own)
          (is equal '(("After them" ("f")) ("Plain task" ("f" "b")) ("Child with a tag twice" ("f" "c" "b")))
              (mapcar #'rest (rows (tuatara.defaults::org-agenda-day today :files files :log nil))))
          (is equal '(("clock" "Plain task" ("f" "b")) ("timestamp" "After them" ("f")) ("clock" "After them" ("f"))
                      ("scheduled" "Plain task" ("f" "b")) ("scheduled" "Child with a tag twice" ("f" "c" "b")))
              (rows (tuatara.defaults::org-agenda-day today :files files :log t)))
          (is equal '(("todo" "Plain task" ("f" "b")) ("todo" "Child with a tag twice" ("f" "c" "b")) ("todo" "After them" ("f")))
              (rows (tuatara.defaults::org-agenda-todo-list :files files)))
          (is equal '(("tagsmatch" "Plain task" ("f" "b")) ("tagsmatch" "Child with a tag twice" ("f" "c" "b")))
              (rows (tuatara.defaults::org-agenda-match "b" :files files)))
          (is equal '() (tuatara.defaults::org-agenda-entries archived))))))
  ;; the clock table reads them all, as Org's does (its oracle: test/org/dblock/skipping.org)
  (let* ((text *org-internals-skipping*)
         (rows (tuatara.defaults::clocktable-rows (tuatara.defaults::org-parse text) text "/x/skip.org" 3 nil nil)))
    (is equal '((1 "Plain task" 60 ("f" "b")) (1 "TODO Commented task" 30 ("f"))
                (1 "Archived task" 45 ("f" "ARCHIVE")) (1 "After them" 30 ("f")))
        (mapcar (lambda (row) (list (tuatara.defaults::clocktable-entry-level row) (tuatara.defaults::clocktable-entry-title row)
                                    (tuatara.defaults::clocktable-entry-minutes row) (tuatara.defaults::clocktable-entry-tags row)))
                rows))))

(define-test (org-internals logbook-lines-placed-alike)
  ;; clocking in, a note, a change of state and a reschedule logged in one place, as Org 9.7.11 logs them
  ;; (batch Emacs): at the top of the entry's :LOGBOOK: drawer wherever it stands in the entry's text
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
        ;; Org's own TODO and DONE, as Org logged these
        (tuatara-user:*org-standard-todo-keywords* '(("TODO") . ("DONE")))
        (tuatara.defaults::*org-log-into-drawer* t)
        (tuatara.defaults::*org-log-repeat* :time)
        (tuatara.defaults::*org-log-reschedule* :time)
        (tuatara-user:*org-read-date-popup-calendar* nil)
        (body (format nil "Body~%:LOGBOOK:~%- old~%:END:~%")))
    (flet ((after (keys text)
             (prog1 (org-text-after keys text) (setf tuatara.defaults::*org-clock* nil))))
      (is string= (format nil "* TODO Task~%Body~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 18:00]--[2026-10-01 Thu 18:00] =>  0:00~%- old~%:END:~%More~%")
          (after "C-c C-x C-i C-c C-x C-o" (format nil "* TODO Task~%~aMore~%" body)))
      (is string= (format nil "* TODO Task~%Body~%:LOGBOOK:~%- Note taken on [2026-10-01 Thu 18:00] \\\\~%  hi~%- old~%:END:~%More~%")
          (after "C-c C-z h i RET" (format nil "* TODO Task~%~aMore~%" body)))
      (is string= (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon +1w>~%:PROPERTIES:~%:LAST_REPEAT: [2026-10-01 Thu 18:00]~%:END:~%Body~%:LOGBOOK:~%- State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]~%- old~%:END:~%")
          (after "C-c C-t" (format nil "* TODO Task~%SCHEDULED: <2026-09-28 Mon +1w>~%~a" body)))
      (is string= (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%Body~%:LOGBOOK:~%- Rescheduled from \"[2026-09-28 Mon]\" on [2026-10-01 Thu 18:00]~%- old~%:END:~%")
          (after "C-c C-s 2 0 2 6 - 1 0 - 0 5 RET" (format nil "* TODO Task~%SCHEDULED: <2026-09-28 Mon>~%~a" body)))
      ;; none yet: one made after the planning line and properties, before another drawer
      (is string= (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%:PROPERTIES:~%:ID: x~%:END:~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 18:00]--[2026-10-01 Thu 18:00] =>  0:00~%:END:~%:NOTES:~%x~%:END:~%")
          (after "C-c C-x C-i C-c C-x C-o" (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%:PROPERTIES:~%:ID: x~%:END:~%:NOTES:~%x~%:END:~%")))
      (is string= (format nil "* TODO Task~%:LOGBOOK:~%- Note taken on [2026-10-01 Thu 18:00] \\\\~%  hi~%:END:~%:NOTES:~%x~%:END:~%")
          (after "C-c C-z h i RET" (format nil "* TODO Task~%:NOTES:~%x~%:END:~%")))
      ;; a note with no drawer goes right after the planning line, a drawer there left alone
      (let ((tuatara.defaults::*org-log-into-drawer* nil))
        (is string= (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%- Note taken on [2026-10-01 Thu 18:00] \\\\~%  hi~%:LOGBOOK:~%- old~%:END:~%Body~%")
            (after "C-c C-z h i RET" (format nil "* TODO Task~%SCHEDULED: <2026-10-05 Mon>~%:LOGBOOK:~%- old~%:END:~%Body~%"))))
      ;; a list item with a checkbox: its own drawer under it, each line on top
      (is string= (format nil "* Errands~%- [X] call Anna~%  :LOGBOOK:~%  - State \"DONE\"       from \"TODO\"       [2026-10-01 Thu 18:00]~%  - Note taken on [2026-10-01 Thu 18:00] \\\\~%    hi~%  CLOCK: [2026-10-01 Thu 18:00]--[2026-10-01 Thu 18:00] =>  0:00~%  :END:~%- [ ] buy milk~%")
          (after "j C-c C-x C-i C-c C-x C-o C-c C-z h i RET C-c C-t" (format nil "* Errands~%- [ ] call Anna~%- [ ] buy milk~%"))))))

(define-test (org-internals clock-in-alike-from-every-key)
  ;; C-c C-x C-i in the file, I on the agenda's line: the same line in the same place, a task's or a checkbox item's
  (with-test-directory (dir)
    (let* ((file (merge-pathnames "work.org" dir))
           (tuatara.defaults::*org-agenda-files* (list (namestring file)))
           (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026)))
      (flet ((fresh (text)
               (dolist (name '("work.org" "*Org Agenda*"))
                 (let ((b (tuatara.buffer:find-buffer name)))
                   (when b (tuatara.buffer:buffer-mark-saved b) (tuatara.buffer:kill-buffer b))))
               (setf tuatara.defaults::*org-clock* nil)
               (alexandria:write-string-into-file text file :if-exists :supersede))
             (from-agenda ()
               (run-keys "SPC a a t I" :width 100 :height 20)
               (prog1 (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "work.org"))
                 (setf tuatara.defaults::*org-clock* nil)))
             (in-file (keys text)
               (prog1 (org-text-after keys text) (setf tuatara.defaults::*org-clock* nil))))
        (let ((task (format nil "* TODO Write~%SCHEDULED: <2026-10-01 Thu>~%:PROPERTIES:~%:ID: w~%:END:~%Body~%")))
          (fresh task)
          (let ((agenda (from-agenda)))
            (is string= (format nil "* TODO Write~%SCHEDULED: <2026-10-01 Thu>~%:PROPERTIES:~%:ID: w~%:END:~%:LOGBOOK:~%CLOCK: [2026-10-01 Thu 18:00]~%:END:~%Body~%")
                agenda)
            (is string= agenda (in-file "C-c C-x C-i" task))))
        (let ((items (format nil "* Errands~%- [ ] call Anna~%")))
          (fresh items)
          (let ((agenda (from-agenda)))
            (is string= (format nil "* Errands~%- [ ] call Anna~%  :LOGBOOK:~%  CLOCK: [2026-10-01 Thu 18:00]~%  :END:~%") agenda)
            (is string= agenda (in-file "j C-c C-x C-i" items))))
        (fresh "")))))

(define-test (org-internals links-shown-as-labels)
  ;; a link with a label shows its label, one without its target, escapes taken off, wherever a note's text is shown plain
  (let ((labelled "Read [[a\\]b][x]] first") (bare "Read [[target]] first") (escaped "Read [[a\\]b]] first"))
    (is string= "Read x first" (tuatara.defaults::org-links-as-labels labelled))
    (is string= "Read target first" (tuatara.defaults::org-links-as-labels bare))
    (is string= "Read a]b first" (tuatara.defaults::org-links-as-labels escaped))
    ;; a table's field takes the columns of what it shows
    (let ((tuatara.defaults::*org-link-descriptive* t))
      (is = 12 (tuatara.defaults::org-string-columns labelled))
      (is = 17 (tuatara.defaults::org-string-columns bare))
      (is = 14 (tuatara.defaults::org-string-columns escaped)))
    ;; a clock table's row and its sorting
    (true (search "][Read x first]]" (tuatara.defaults::clocktable-headline labelled t nil)))
    (true (search "][Read target first]]" (tuatara.defaults::clocktable-headline bare t nil)))
    (is string= "read a]b first" (tuatara.defaults::clocktable-sort-key escaped #\a))
    ;; the enhanced export's measure of a table's cell
    (is string= "Read x first" (tuatara.defaults::org-latex-visible-text labelled))
    (is string= "Read target first" (tuatara.defaults::org-latex-visible-text bare))
    ;; a table of contents
    (let ((lines (mapcar #'first (tuatara.defaults::toc-entries
                                  (format nil "* Contents :TOC:~%* ~a~%* ~a~%" labelled bare)))))
      (true (find-if (lambda (line) (search "Read x first]]" line)) lines))
      (true (find-if (lambda (line) (search "Read target first]]" line)) lines)))))

(define-test (org-internals statistics-cookies-left-out)
  ;; a heading's [2/3] or [50%] is left out of a link to it, its label and a table of contents alike
  (is string= "Task done" (tuatara.defaults::org-link-search-words "Task [2/3] done"))
  (is string= "[[*Task done][Task done]]" (tuatara.defaults::clocktable-headline "Task [2/3]  done" t nil))
  (true (find-if (lambda (line) (search "][Task]]" line))
                 (mapcar #'first (tuatara.defaults::toc-entries (format nil "* Contents :TOC:~%* Task [50%]~%"))))))

(define-test (org-internals plain-text-of-a-cell)
  ;; links as labels, the marks of emphasis off, a lone + or * kept
  (is string= "bold and it y z" (tuatara.defaults::org-plain-text "*bold* and /it/ [[x][y]] [[z]]"))
  (is string= "a + b * c" (tuatara.defaults::org-latex-visible-text "a + b * c"))
  (is string= "zeta" (tuatara.defaults::clocktable-sort-key "=Zeta=" #\a)))

(define-test (org-internals dynamic-blocks-as-parsed)
  ;; what the parse calls a dynamic block is what updating the blocks finds: no blank needed after
  ;; #+BEGIN:, #+END with or without its colon, a line with more after #+END: no end
  (flet ((found (text)
           (mapcar (lambda (block) (list (fourth block) (fifth block))) (tuatara.defaults::dblock-blocks (tuatara.defaults::org-parse text) text)))
         (parsed (text)
           (and (tuatara.defaults::org-find-in-tree (tuatara.defaults::org-parse text)
                                                    (lambda (n) (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::dynamic-block)))
                t)))
    (let ((text (format nil "#+BEGIN:clocktable :scope file  ~%old~%#+END~%after~%")))
      (is equal '(("clocktable" ":scope file")) (found text))
      (true (parsed text)))
    (let ((text (format nil "#+BEGIN: clocktable~%old~%#+END: later~%")))
      (is equal '() (found text))
      (false (parsed text)))
    (let ((text (format nil "#+begin: columnview :id local~%#+end:~%")))
      (is equal '(("columnview" ":id local")) (found text))
      (true (parsed text)))))

(define-test (org-internals planning-ranges-read-alike)
  ;; a SCHEDULED range is read whole by the parse, the agenda and scheduling alike
  (let* ((range "<2026-10-01 Thu>--<2026-10-03 Sat>")
         (line (format nil "SCHEDULED: ~a DEADLINE: <2026-10-05 Mon>" range))
         (text (format nil "* TODO Task~%~a~%" line)))
    (is equal `(("SCHEDULED" . ,range) ("DEADLINE" . "<2026-10-05 Mon>")) (tuatara.defaults::org-planning-tokens line))
    (let ((planning (tuatara.defaults::org-find-in-tree (tuatara.defaults::org-parse text)
                                                        (lambda (n) (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::planning)))))
      (is string= range (tuatara.defaults::org-property planning :scheduled)))
    (with-test-directory (dir)
      (let ((file (namestring (merge-pathnames "plan.org" dir))))
        (alexandria:write-string-into-file text file :if-exists :supersede)
        (is string= range (tuatara.defaults::agenda-entry-scheduled (first (tuatara.defaults::org-agenda-entries file))))))))

(define-test (org-internals tree-walk-takes-titles)
  ;; every node is visited, a headline's title too: a <<target>> in a heading is found
  (let* ((text (format nil "* A heading <<here>>~%#+NAME: one~%#+begin_src sh~%echo~%#+end_src~%** B~%#+begin_src python~%x~%#+end_src~%"))
         (tree (tuatara.defaults::org-parse text)))
    (true (tuatara.defaults::org-find-in-tree tree (lambda (n) (and (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::target)
                                                                     (equal (tuatara.defaults::org-property n :value) "here")))))
    (is equal '("python" "sh") (sort (copy-list (tuatara.defaults::org-src-language-names tree)) #'string<))
    (true (gethash "one" (tuatara.defaults::org-named-blocks tree)))
    (is equal "B"
        (let ((seen '()))
          (tuatara.defaults::org-walk-tree tree (lambda (n) (when (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::headline)
                                                              (push (tuatara.defaults::org-property n :raw-value) seen))))
          (first seen)))))

(define-test (org-internals imported-tables-alike)
  ;; a table read from LaTeX is written as one read from a Word document: |- under the first row,
  ;; a | in a cell \vert{}
  (let* ((latex (format nil "\\begin{tabular}{ll}~%a & b|c \\\\~%\\hline~%d & e \\\\~%\\end{tabular}~%"))
         (tuatara.defaults::*latex-org-text* latex)
         (expected (format nil "| a | b\\vert{}c |~%|-~%| d | e |")))
    (is string= expected (tuatara.defaults::latex-org-note-text 0 (length latex)))
    (is string= (format nil "~a~2%" expected)
        (with-output-to-string (out) (tuatara.defaults::org-write-table-rows '(("a" "b|c") ("d" "e")) out)))
    (is string= (format nil "  | a |~%  | b |~2%")
        (with-output-to-string (out) (tuatara.defaults::org-write-table-rows '(("a") ("b")) out :indent "  " :rule nil)))))

(define-test (org-internals macro-definitions-one-way)
  ;; a macro's last #+MACRO: line is the one, its setup files' read too, for the export, the list
  ;; of macros to wrap text in, and the check for names only the enhanced export knows
  (is string= "two" (cdr (assoc "m" (let ((tuatara.defaults::*org-latex* (tuatara.defaults::make-org-latex "" nil nil)))
                                       (setf (tuatara.defaults::org-latex-keywords tuatara.defaults::*org-latex*)
                                             '(("MACRO" . "m one") ("MACRO" . "M  two")))
                                       (tuatara.defaults::org-macro-definitions))
                                :test #'string-equal)))
  (true (search "Say two." (tuatara.defaults::org-latex-document (format nil "#+MACRO: m one~%#+MACRO: m two~%Say {{{m}}}.~%"))))
  (let ((b (tuatara.buffer:make-buffer "*macros*" :text (format nil "#+macro: who first~%#+macro: who second~%"))))
    (unwind-protect
         (let ((who (find "who" (tuatara.defaults::org-macro-choices b) :key #'tuatara.command:candidate-string :test #'string=)))
           (is string= "declared: second" (tuatara.command:candidate-annotation who)))
      (tuatara.buffer:kill-buffer b)))
  (with-test-directory (dir)
    (alexandria:write-string-into-file (format nil "#+MACRO: R [$1]~%") (merge-pathnames "setup.org" dir) :if-exists :supersede)
    (let ((folder (tuatara.io:parent-path (namestring (merge-pathnames "note.org" dir)))))
      (is eq nil (tuatara.defaults::only-enhanced-macro (format nil "#+SETUPFILE: setup.org~%See {{{R(x)}}}.~%") folder))
      (is string= "R" (tuatara.defaults::only-enhanced-macro (format nil "See {{{R(x)}}}.~%") folder)))))

(define-test (org-internals property-additions)
  ;; NAME replaces, NAME+ adds, from the file's #+PROPERTY: lines down through the headlines
  (let* ((text (format nil "#+PROPERTY: header-args :a 1~%#+PROPERTY: header-args+ :b 2~%* H~%:PROPERTIES:~%:header-args+: :c 3~%:END:~%** I~%:PROPERTIES:~%:header-args: :d 4~%:END:~%text~%"))
         (tree (tuatara.defaults::org-parse text)))
    (is string= ":a 1 :b 2" (tuatara.defaults::org-file-properties tree "header-args"))
    (flet ((at (title)
             (tuatara.defaults::org-find-in-tree tree (lambda (n) (and (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::headline)
                                                                        (equal (tuatara.defaults::org-property n :raw-value) title))))))
      (is string= ":a 1 :b 2 :c 3" (tuatara.defaults::org-inherited-property tree (first (tuatara.defaults::org-node-children (at "H"))) "header-args"))
      (is string= ":d 4" (tuatara.defaults::org-inherited-property tree (first (tuatara.defaults::org-node-children (at "I"))) "header-args")))))

(define-test (org-internals siblings-of-a-headline)
  ;; the headlines beside one, under the same parent
  (let* ((tree (tuatara.defaults::org-parse (format nil "* A~%** A1~%text~%** A2~%* B~%")))
         (a1 (tuatara.defaults::org-find-in-tree tree (lambda (n) (equal (tuatara.defaults::org-property n :raw-value) "A1"))))
         (a (tuatara.defaults::org-node-parent a1)))
    (flet ((titles (headlines) (mapcar (lambda (h) (tuatara.defaults::org-property h :raw-value)) headlines)))
      (is equal '("A1" "A2") (titles (tuatara.defaults::org-siblings a1 tree)))
      (is equal '("A" "B") (titles (tuatara.defaults::org-siblings a tree))))))

(define-test (org-internals warning-delay-is-no-range)
  ;; a deadline's warning written --2d leaves the timestamp one; two dates joined by -- make a range
  (flet ((types (text)
           (let ((found '()))
             (tuatara.defaults::org-walk-tree (tuatara.defaults::org-parse text)
                                              (lambda (n) (when (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::timestamp)
                                                            (push (tuatara.defaults::org-property n :type) found))))
             (nreverse found))))
    (is equal '(tuatara.defaults::active) (types (format nil "Due <2026-10-01 Thu --2d> then.~%")))
    (is equal '(tuatara.defaults::inactive) (types (format nil "Was [2026-10-01 Thu --2d] then.~%")))
    (is equal '(tuatara.defaults::active-range) (types (format nil "From <2026-10-01 Thu>--<2026-10-03 Sat>.~%")))
    (is equal '(tuatara.defaults::inactive-range) (types (format nil "From [2026-10-01 Thu]--[2026-10-03 Sat].~%")))
    (is equal '(tuatara.defaults::active-range) (types (format nil "At <2026-10-01 Thu 09:00-10:00>.~%")))))

(define-test (org-internals returns-counted-across-chunk-edge)
  ;; a carriage return ending one megabyte of the text, its newline beginning the next: the returns after it where the stripped text has them
  (let ((text (concatenate 'string (make-string 1048575 :initial-element #\a) (format nil "~C~%b~C~%c~C~%" #\Return #\Return #\Return))))
    (is equalp (nth-value 1 (tuatara.defaults::org-strip-returns text))
        (tuatara.defaults::org-returns-in-rope (tuatara.text:string->rope text)))))

(defun org-internals-headline-count (buffer)
  "How many headlines BUFFER's parts begin with."
  (let ((parts (tuatara.defaults::org-buffer-parts buffer)))
    (loop for i below (tuatara.defaults::org-parts-count parts)
          count (plusp (tuatara.defaults::org-part-level (tuatara.defaults::org-part-at parts i))))))

(define-test (org-internals headline-index-costs-no-copy)
  ;; where the headlines begin, found by reading the text a window at a time: no copy of it, however long it is
  (flet ((consed (octets)
           (let ((buffer (org-internals-buffer (concatenate 'string (format nil "* A~%") (org-internals-filler octets) (format nil "* B~%")))))
             (unwind-protect
                  (progn
                    (is = 2 (org-internals-headline-count buffer))
                    (loop repeat 3
                          minimize (let ((before (sb-ext:get-bytes-consed)))
                                     (setf (tuatara.buffer:buffer-local buffer 'tuatara.defaults::org-parts) nil)
                                     (tuatara.defaults::org-buffer-parts buffer)
                                     (- (sb-ext:get-bytes-consed) before))))
               (tuatara.buffer:kill-buffer buffer)))))
    ;; for each megabyte more of text: a copy was a megabyte, none now
    (let ((per-megabyte (/ (- (consed (* 4 1024 1024)) (consed (* 1024 1024))) 3)))
      (true (< per-megabyte (* 16 1024)) "~:d octets for each megabyte" per-megabyte))))

(define-test (org-internals bullets-look-only-at-shown-headlines)
  ;; the signs of the headlines in sight: found by halves, not by walking every headline of the file for each window drawn
  (flet ((seconds (headlines)
           (let ((buffer (org-internals-buffer (with-output-to-string (s) (dotimes (i headlines) (format s "* h~d~%" i))))))
             (unwind-protect
                  (progn (is = headlines (org-internals-headline-count buffer))
                         (is = 4 (length (tuatara.defaults::org-headline-signs buffer 0 20)))
                         (loop repeat 3
                               minimize (let ((start (get-internal-real-time)))
                                          (loop repeat 1000 do (tuatara.defaults::org-headline-signs buffer 0 20))
                                          (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
               (tuatara.buffer:kill-buffer buffer)))))
    ;; for each thousand headlines more, the time of one call: microseconds when every headline was looked at
    (let ((per-thousand (/ (- (seconds 200000) (seconds 20000)) 1000 180)))
      (true (< per-thousand 5/10000000) "~,3f µs for each thousand headlines" (* per-thousand 1000000)))))

(define-test (org-internals line-head-cut-between-characters)
  ;; the start of a long line read to its limit, cut before a character the limit falls in; a short line whole
  (let ((buffer (org-internals-buffer (concatenate 'string "a" (make-string 600 :initial-element #\é) (string #\Newline) "- [ ] x"))))
    (unwind-protect
         (let ((head (tuatara.defaults::org-line-head buffer 0)))
           (is = 256 (length head))
           (true (tuatara.defaults::org-checkbox-line-p (tuatara.defaults::org-line-head buffer 1205)))
           (false (tuatara.defaults::org-checkbox-line-p head)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (org-internals unclosed-openings-parsed-in-a-line-each)
  ;; drawers, blocks and environments never closed, and \( or @@ never closed in a paragraph: each line looked at a few times, not to the end again for each
  (flet ((per-line (line)
           (flet ((seconds (lines)
                    (let ((text (with-output-to-string (s) (dotimes (i lines) (write-line line s)))))
                      (loop repeat 2
                            minimize (let ((start (get-internal-real-time)))
                                       (tuatara.defaults::org-parse text)
                                       (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))
             (/ (- (seconds 8000) (seconds 2000)) 6000))))
    (dolist (line '(":a:" "#+BEGIN_X" "\\begin{x}" "- item" "word \\( and @@b:x and [[a][b"))
      (let ((seconds (per-line line)))
        ;; a line: microseconds; looking to the end again for each was milliseconds
        (true (< seconds 1/10000) "~a: ~,1f µs a line" line (* seconds 1000000))))))

(define-test (org-internals drawers-folded-in-a-pass)
  ;; a section of lines like drawers, none ended: each line looked at once, not to the section's end again for each
  (flet ((seconds (lines)
           (let ((buffer (org-internals-buffer (with-output-to-string (s)
                                                 (format s "* A~%:LOGBOOK:~%CLOCK: x~%:END:~%")
                                                 (dotimes (i lines) (format s ":a~d:~%" i))
                                                 (format s "* B~%")))))
             (unwind-protect
                  (let ((start (get-internal-real-time)))
                    (tuatara.defaults::org-fold-drawers-in buffer 0 (tuatara.buffer:buffer-size buffer))
                    (prog1 (/ (- (get-internal-real-time) start) internal-time-units-per-second)
                      ;; the drawer that is ended is folded
                      (is = 1 (length (tuatara.display:buffer-folds buffer)))))
               (tuatara.buffer:kill-buffer buffer)))))
    (let ((per-line (/ (- (seconds 8000) (seconds 2000)) 6000)))
      (true (< per-line 1/20000) "~,1f µs a line" (* per-line 1000000)))))

(define-test (org-internals dynamic-block-found-in-the-parse)
  ;; the block point is in found in the parse: in place, a #+BEGIN: line inside a source block none, no line copied for it
  (let* ((text (format nil "* A~%#+BEGIN: clocktable :maxlevel 2~%| x |~%#+END:~%#+begin_src org~%#+BEGIN: clocktable~%#+END:~%#+end_src~%"))
         (tree (tuatara.defaults::org-parse text)))
    (is equal (list 4 36 42 "clocktable" ":maxlevel 2") (tuatara.defaults::dblock-at tree text 10))
    (is equal (tuatara.defaults::dblock-at tree text 10) (tuatara.defaults::dblock-at tree text 47))
    (false (tuatara.defaults::dblock-at tree text 2))
    (false (tuatara.defaults::dblock-at tree text (search (format nil "#+BEGIN: clocktable~%") text)))
    (is = 1 (length (tuatara.defaults::dblock-blocks tree text))))
  (flet ((consed (lines)
           (let* ((text (concatenate 'string (format nil "#+BEGIN: clocktable~%#+END:~%") (org-internals-filler (* lines 64))))
                  (tree (tuatara.defaults::org-parse text)))
             (loop repeat 3
                   minimize (let ((before (sb-ext:get-bytes-consed)))
                              (tuatara.defaults::dblock-at tree text 3)
                              (- (sb-ext:get-bytes-consed) before))))))
    ;; for each line more of the note: a copy of it, with the lines split again, before
    (let ((per-line (/ (- (consed 40000) (consed 10000)) 30000)))
      (true (< per-line 1) "~,1f octets a line" per-line))))

(define-test (org-internals top-level-found-by-halves)
  ;; in a flat note of many headlines, what is in sight is found by halves: the node at a place, the hidden parts of links, alike to before
  (flet ((note (headlines)
           (with-output-to-string (s)
             (dotimes (i headlines) (format s "* h~d~%" i))
             (format s "* See [[x][y]] here~%"))))
    (let* ((text (note 3))
           (tree (tuatara.defaults::org-parse text))
           (at (search "[[" text)))
      (is equal (list (cons at (+ at 5)) (cons (+ at 6) (+ at 8))) (tuatara.defaults::org-hidden-chars tree 0 (length text)))
      (is eq 'tuatara.defaults::headline
          (tuatara.defaults::org-node-type (tuatara.defaults::org-node-at tree at (lambda (n) (eq (tuatara.defaults::org-node-type n) 'tuatara.defaults::headline)))))
      (false (tuatara.defaults::org-hidden-chars tree 0 at)))
    (flet ((seconds (headlines)
             (let* ((text (note headlines))
                    (tree (tuatara.defaults::org-parse text))
                    (at (search "[[" text)))
               (tuatara.defaults::org-hidden-chars tree at (length text))
               (let ((start (get-internal-real-time)))
                 (loop repeat 1000 do (tuatara.defaults::org-hidden-chars tree at (length text))
                                      (tuatara.defaults::org-node-at tree at (constantly t)))
                 (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
      ;; for each thousand headlines more, the time of a call: tens of microseconds when each was looked at
      (let ((per-thousand (/ (- (seconds 100000) (seconds 10000)) 1000 90)))
        (true (< per-thousand 1/1000000) "~,3f µs for each thousand headlines" (* per-thousand 1000000))))))

(define-test (org-internals headline-on-a-line-without-a-list)
  ;; the headline of the line TAB is on: found in the tree, no list of every headline made for it
  (flet ((consed (headlines)
           (let* ((text (with-output-to-string (s) (dotimes (i headlines) (format s "* h~d~%** sub~%" i))))
                  (tree (tuatara.defaults::org-parse text))
                  (at (- (length text) 3)))
             (is equal "sub" (tuatara.defaults::org-property (tuatara.defaults::org-headline-on-line tree text at) :raw-value))
             (loop repeat 3
                   minimize (let ((before (sb-ext:get-bytes-consed)))
                              (tuatara.defaults::org-headline-on-line tree text at)
                              (- (sb-ext:get-bytes-consed) before))))))
    (let ((per-headline (/ (- (consed 20000) (consed 2000)) 36000)))
      (true (< per-headline 1) "~,1f octets a headline" per-headline))))

(define-test (org-internals line-ends-put-in-as-the-buffer-has)
  ;; text an Org edit puts in ends its lines as the buffer's lines end, whichever
  ;; way it came: a replacement and edits of items alike, as a yank does
  (dolist (ending '(:lf :crlf))
    (flet ((after (edit)
             (let ((b (tuatara.buffer:make-buffer (format nil "org line ends ~a" ending))))
               (unwind-protect
                    (progn (setf (tuatara.buffer:buffer-local b :line-ending) ending)
                           (funcall edit b)
                           (tuatara.buffer:buffer-string b))
                 (tuatara.buffer:kill-buffer b))))
           (wanted ()
             (if (eq ending :crlf)
                 (format nil "a~c~%b~c~%c" #\Return #\Return)
                 (format nil "a~%b~%c")))
           (given () (format nil "a~c~%b~%c" #\Return)))
      (is string= (wanted) (after (lambda (b) (tuatara.defaults::org-replace-chars b 0 0 (given)))) "~a" ending)
      (is string= (wanted) (after (lambda (b) (tuatara.defaults::org-apply-edits b (list (list 0 0 (given)))))) "~a" ending))))

(define-test (org-internals long-lists-and-sections-found-by-halves)
  :time-limit 900
  ;; a note with no headline (one section of many paragraphs) or one long list: what is in sight
  ;; is found among their children by halves, alike to before
  (flet ((note (kind count)
           (with-output-to-string (s)
             (dotimes (i count) (format s (if (eq kind :list) "- i~d~%" "p~d~%~%") i))
             (format s (if (eq kind :list) "- See [[x][y]] here~%" "See [[x][y]] here~%")))))
    (dolist (kind '(:list :paragraphs))
      (let* ((text (note kind 100))
             (tree (tuatara.defaults::org-parse text))
             (at (search "[[" text)))
        (is equal (list (cons at (+ at 5)) (cons (+ at 6) (+ at 8))) (tuatara.defaults::org-hidden-chars tree 0 (length text)))
        (is equal (list (cons at (+ at 5)) (cons (+ at 6) (+ at 8))) (tuatara.defaults::org-hidden-chars tree at (length text)))
        (false (tuatara.defaults::org-hidden-chars tree 0 at)))
      (flet ((seconds (count)
               (let* ((text (note kind count))
                      (tree (tuatara.defaults::org-parse text))
                      (at (search "[[" text)))
                 (tuatara.defaults::org-hidden-chars tree at (length text))
                 (let ((start (get-internal-real-time)))
                   (loop repeat 1000 do (tuatara.defaults::org-hidden-chars tree at (length text)))
                   (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))
        ;; for each thousand children more, the time of a call: tens of microseconds when each was looked at
        (let ((per-thousand (/ (- (seconds 100000) (seconds 10000)) 1000 90)))
          (true (< per-thousand 1/1000000) "~a: ~,3f µs for each thousand children" kind (* per-thousand 1000000)))))))

(define-test (org-internals unclosed-blocks-of-many-names)
  ;; blocks, environments and headlines of many names and depths make no pattern each, kept for
  ;; the session; never closed, they are parsed in time growing with their number, not its square
  (flet ((note (count &key headlines)
           (with-output-to-string (s)
             (dotimes (i count)
               (when headlines (format s "~a ~d~%" (make-string (1+ (mod i 40)) :initial-element #\*) i))
               (format s "#+begin_b~d~%x~%\\begin{e~d}~%y~%" i i))))
         (seconds (text)
           (let ((start (get-internal-real-time)))
             (tuatara.defaults::org-parse text)
             (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    (let ((before (hash-table-count tuatara.defaults::*org-scanners*)))
      (tuatara.defaults::org-parse (note 500 :headlines t))
      (true (< (- (hash-table-count tuatara.defaults::*org-scanners*) before) 5)
            "~d patterns more" (- (hash-table-count tuatara.defaults::*org-scanners*) before)))
    ;; a block closed further on is still found, its name in any case
    (let ((tree (tuatara.defaults::org-parse (format nil "#+begin_quote~%a~%#+begin_x~%#+END_QUOTE~%"))))
      (is eq 'tuatara.defaults::quote-block (tuatara.defaults::org-node-type (first (tuatara.defaults::org-node-children (first (tuatara.defaults::org-node-children tree)))))))
    (let* ((small (note 2000)) (large (note 8000)))
      (seconds small)
      (let ((per-small (/ (seconds small) 2000)) (per-large (/ (seconds large) 8000)))
        (true (< per-large (+ (* 3 per-small) 1/100000)) "~,1f µs a block at 2,000, ~,1f µs at 8,000"
              (* per-small 1000000) (* per-large 1000000))))))

(define-test (org-internals markdown-return-reads-its-line)
  ;; RET on a Markdown line reads that line for a link, never a copy of the whole file
  (let ((buffer (org-internals-buffer (format nil "See [the end](#the-end) or [x](y.md)~%text~%# The end~%"))))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-point buffer) 8)
           (true (tuatara.defaults::markdown-follow-link buffer))
           (setf (tuatara.buffer:buffer-point buffer) 2)
           (false (tuatara.defaults::markdown-follow-link buffer)))
      (tuatara.buffer:kill-buffer buffer)))
  (flet ((consed (octets)
           (let ((buffer (org-internals-buffer (concatenate 'string (format nil "plain words~%") (org-internals-filler octets)))))
             (unwind-protect
                  (loop repeat 3
                        minimize (let ((before (sb-ext:get-bytes-consed)))
                                   (tuatara.defaults::markdown-follow-link buffer)
                                   (- (sb-ext:get-bytes-consed) before)))
               (tuatara.buffer:kill-buffer buffer)))))
    ;; for each megabyte more of text: a copy was megabytes, nothing now
    (let ((per-megabyte (/ (- (consed (* 4 1024 1024)) (consed (* 1024 1024))) 3)))
      (true (< per-megabyte (* 16 1024)) "~:d octets for each megabyte" per-megabyte))))

(define-test (org-internals tab-reads-a-bounded-subtree)
  ;; TAB on a headline of a large file not yet parsed reads at most a few megabytes for its
  ;; subtree, whatever its length; a longer one is left to the parse
  (flet ((seconds (octets)
           (let ((buffer (org-internals-buffer (concatenate 'string (format nil "* A~%") (org-internals-filler octets) (format nil "* B~%")))))
             (unwind-protect
                  (let ((start (get-internal-real-time)))
                    (false (tuatara.defaults::org-cycle-headline-at buffer 0))
                    (/ (- (get-internal-real-time) start) internal-time-units-per-second))
               (tuatara.buffer:kill-buffer buffer)))))
    (seconds (* 8 1024 1024))
    ;; for each megabyte more of subtree: a millisecond or more when every line was read
    (let ((per-megabyte (/ (- (seconds (* 32 1024 1024)) (seconds (* 8 1024 1024))) 24)))
      (true (< per-megabyte 1/2000) "~,2f ms for each megabyte" (* per-megabyte 1000)))))

(define-test (org-internals agenda-reads-to-the-last-line)
  ;; a file whose last line has no newline after it is read to its end, whatever
  ;; that line is: a comment, a checkbox item, text with a date, the heading itself
  (dolist (last '("# a comment" "- [ ] an item" "Due <2026-10-27 Tue>" ""))
    (let* ((text (format nil "#+title: Ends without a newline~%~%* TODO My task~%~a" last))
           (text (string-right-trim '(#\Newline) text))
           (entries (tuatara.defaults::org-agenda-file-entries "/x/no-newline.org" text)))
      (true entries "read: ~s" last)
      (is string= "My task" (tuatara.defaults::agenda-entry-title (first entries)) "read: ~s" last)
      (when (search "Due" last)
        (is equal '("<2026-10-27 Tue>") (tuatara.defaults::agenda-entry-stamps (first entries)))))))

(define-test (org-internals block-contents-escaped-both-ways)
  ;; lines that would end a block or begin a heading or keyword get a comma, as Org writes them, and lose it read back
  (let ((value (format nil "*not a heading~%  #+end_example~%,*already one~%,,#+two~%plain, line~%#comment")))
    (is string= (format nil ",*not a heading~%  ,#+end_example~%,,*already one~%,,,#+two~%plain, line~%#comment")
        (tuatara.defaults::org-escape-block value))
    (is string= value (tuatara.defaults::org-unescape-block (tuatara.defaults::org-escape-block value)))
    ;; a comma before a comma that protects a star is taken away too, as Org takes it
    (is string= ",*x" (tuatara.defaults::org-unescape-block ",,*x"))))

(define-test (org-internals safe-line-read-back)
  ;; words kept from markup on one line say what they said, line breaks apart
  (dolist (words '("Cost $5 [[file:x]] \\input{y} @@latex:z@@ {{{m}}}" "\\dollar{} as written" "plain"))
    (is string= words (tuatara.defaults::org-safe-line-words (tuatara.defaults::org-safe-line words))))
  (is string= "two lines" (tuatara.defaults::org-safe-line-words (tuatara.defaults::org-safe-line (format nil "two~%lines")))))
