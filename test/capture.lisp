;;;; test/capture.lisp --- Notes and tasks taken down and filed: org-capture.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test capture
  :description "A template chosen by its key, its questions, the *Capture* buffer filed under a heading, at a file's end or in the journal, levels and tags put right, the link back, the clock, thrown away, and the agenda file saved.")

(defparameter *capture-agenda*
  (format nil "#+title: Agenda~%~%* Tasks~%** TODO Old task~%Some text.~%~%* Other~%** Leave me :home:~%")
  "An agenda file for the capture tests.")

(defmacro with-capture ((directory agenda) &body body)
  "Run BODY with the journal's settings of WITH-JOURNAL in DIRECTORY, AGENDA the
path of an agenda file holding *CAPTURE-AGENDA* there, it the only agenda file
and saved as the agenda saves, Tuatara's templates filing there as they
follow the agenda files, and your
templates those in DIRECTORY's capture-templates/ (none unless a test makes them)."
  `(with-journal (,directory)
     (let* ((,agenda (namestring (merge-pathnames "agenda.org" ,directory)))
            (tuatara-user:*org-agenda-files* (list ,agenda))
            (tuatara-user:*org-agenda-autosave-files* t)
            (tuatara.defaults::*org-clock* nil)
            (tuatara-user:*capture-template-directories* (list (namestring (merge-pathnames "capture-templates/" ,directory)))))
       (declare (ignorable tuatara-user:*capture-template-directories*))
       (alexandria:write-string-into-file *capture-agenda* ,agenda)
       ,@body)))

(define-modal-test (capture todo-under-its-heading)
  (with-capture (dir agenda)
    ;; from a note: t, a tag chosen, the task typed, C-c C-c
    (let ((note (namestring (merge-pathnames "note.org" dir))))
      (alexandria:write-string-into-file (format nil "* Findings~%Text.~%") note)
      (multiple-value-bind (f editor)
          (run-keys "j SPC a c t p r o j e c t s RET W r i t e SPC t e s t s C-c C-c" :file note :width 100 :height 16)
        ;; filed at the end of what is under the heading, a level below it, tags at their column
        (let ((text (file-text agenda)))
          (true (search (format nil "** TODO Old task~%Some text.~%** TODO Write tests") text) "~a" text)
          (true (cl-ppcre:scan "\\*\\* TODO Write tests +:projects:" text))
          (true (search "[2026-03-20 Fri 13:39]" text))
          (true (search "[[file:" text))
          (true (search "::*Findings][Findings]]" text))
          ;; the heading after it untouched
          (true (search (format nil "* Other~%** Leave me :home:") text)))
        ;; back where it began, the capture buffer gone
        (is string= "note.org" (tuatara.buffer:buffer-name (session-buffer editor)))
        (false (tuatara.buffer:find-buffer "*Capture*"))
        (true (tuatara.frontend.fake:fake-frame-showing f "TODO for the agenda filed under Tasks in agenda.org"))))))

(define-modal-test (capture note-to-a-new-heading-and-thrown-away)
  (with-capture (dir agenda)
    ;; the inbox heading made at the file's end
    (run-keys "SPC a c n C a l l SPC b a c k C-c C-c" :text "x" :width 100 :height 16)
    (true (search (format nil "* Inbox~%** Call back~%[2026-03-20 Fri 13:39]") (file-text agenda)))
    ;; thrown away: nothing filed
    (run-keys "SPC a c n N o p e C-c C-k" :text "x" :width 100 :height 16)
    (false (search "Nope" (file-text agenda)))
    ;; a key with no template
    (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC a c Z" :text "x" :width 100 :height 16)
                                                    "No capture template on that key"))))

(define-modal-test (capture templates-in-files)
  (with-capture (dir agenda)
    (let ((directory (merge-pathnames "capture-templates/" dir)))
      (ensure-directories-exist directory)
      ;; one of yours on t takes the place of Tuatara's; another of a key of its own, under an outline path
      (alexandria:write-string-into-file
       (format nil "# name: My task~%# key: t~%# file: ~a~%# heading: Other~%# --~%* MINE %?~%" agenda)
       (merge-pathnames "t-mine.org" directory))
      (alexandria:write-string-into-file
       (format nil "# name: Book to read~%# key: b~%# file: ~a~%# outline: Other / Leave me~%# --~%* %^{Title}%?~%" agenda)
       (merge-pathnames "b-book.org" directory))
      (alexandria:write-string-into-file (format nil "# name: Nowhere~%# key: w~%# --~%* x~%")
                                         (merge-pathnames "w-nowhere.org" directory))
      (alexandria:write-string-into-file (format nil "no head at all~%") (merge-pathnames "loose.org" directory))
      (let ((templates (tuatara.defaults::capture-templates)))
        (is equal '("b" "j" "m" "M" "n" "t" "w") (mapcar #'tuatara.defaults::capture-template-key templates))
        (let ((mine (find "t" templates :key #'tuatara.defaults::capture-template-key :test #'string=)))
          (is string= "My task" (tuatara.defaults::capture-template-name mine))
          (false (tuatara.defaults::capture-template-own mine))
          (is equal (list :file-headline agenda "Other") (tuatara.defaults::capture-template-target mine))
          (is string= "* MINE %?" (tuatara.defaults::capture-template-text mine)))
        (is equal (list :olp agenda "Other" "Leave me")
            (tuatara.defaults::capture-template-target (find "b" templates :key #'tuatara.defaults::capture-template-key :test #'string=)))
        (let ((meeting (find "M" templates :key #'tuatara.defaults::capture-template-key :test #'string=)))
          (true (tuatara.defaults::capture-template-own meeting))
          (true (tuatara.defaults::capture-template-clock-in meeting))
          (true (tuatara.defaults::capture-template-clock-resume meeting))))
      ;; the prompt says whose each is; yours files where it says
      (let ((f (run-keys "SPC a c t W r i t e C-c C-c" :text "x" :width 200 :height 16)))
        (true (tuatara.frontend.fake:fake-frame-showing f "[t] My task (yours)"))
        (true (tuatara.frontend.fake:fake-frame-showing f "[j] Journal entry  [m]")))
      (true (search (format nil "** Leave me :home:~%** MINE Write") (file-text agenda)))
      (run-keys "SPC a c b D u n e RET C-c C-c" :text "x" :width 200 :height 16)
      (true (search (format nil "** Leave me :home:~%*** Dune") (file-text agenda)))
      ;; one saying nowhere to file it
      (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC a c w" :text "x" :width 200 :height 16)
                                                      "The capture template w-nowhere.org says nowhere to file it")))))

(define-test (capture copied-to-be-yours)
  (with-capture (dir agenda)
    (let ((copy (merge-pathnames "capture-templates/M-meeting-notes.org" dir)))
      (multiple-value-bind (f editor)
          (run-keys "M-x o r g - c a p t u r e - t e m p l a t e - c o p y RET M e e t i n g SPC n o t e s RET"
                    :text "x"
                    :width 120
                    :height 16)
        (true (probe-file copy))
        (true (search "# clock-resume: t" (file-text copy)))
        (is string= "M-meeting-notes.org" (tuatara.buffer:buffer-name (session-buffer editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied the capture template M-meeting-notes.org to")))
      (forget-file-buffers)
      ;; never written over
      (alexandria:write-string-into-file (format nil "# name: Mine~%# key: M~%# journal: t~%# --~%x~%") copy :if-exists :supersede)
      (let ((f (run-keys "M-x o r g - c a p t u r e - t e m p l a t e - c o p y RET M e e t i n g SPC n o t e s RET"
                         :text "x"
                         :width 120
                         :height 16)))
        (true (tuatara.frontend.fake:fake-frame-showing f "is yours already"))
        (true (search "# name: Mine" (file-text copy))))
      (forget-file-buffers))))

(define-modal-test (capture meetings-as-standard)
  ;; Tuatara's own: m a meeting planned, its time asked and no clock; M the notes of one, its clock running
  (with-capture (dir agenda)
    (let ((templates (tuatara.defaults::capture-templates)))
      (flet ((own (key) (find key templates :key #'tuatara.defaults::capture-template-key :test #'string=)))
        (true (tuatara.defaults::capture-template-own (own "m")))
        (true (search "%^T" (tuatara.defaults::capture-template-text (own "m"))))
        (false (tuatara.defaults::capture-template-clock-in (own "m")))
        (true (tuatara.defaults::capture-template-clock-in (own "M")))
        (true (tuatara.defaults::capture-template-clock-resume (own "M")))))
    (run-keys "SPC a c M T a l k RET A n a RET S a i d C-c C-c" :text "x" :width 120 :height 16)
    (let ((text (file-text agenda)))
      (true (cl-ppcre:scan "\\* Meetings\\n\\*\\* Meeting: Talk +:meeting:" text) "~a" text)
      (true (search "CLOCK: [" text) "~a" text))
    (true (probe-file dir))))

(define-modal-test (capture journal-entry)
  (with-capture (dir agenda)
    (run-keys "SPC a c j W r o t e SPC i t C-c C-c" :text "x" :width 100 :height 16)
    (let ((files (journal-files dir)))
      (is = 1 (length files))
      (true (search (format nil "* 13:39 Wrote it")
                    (file-text (merge-pathnames (concatenate 'string "journal/" (first files)) dir)))))))

(define-modal-test (capture meeting-clocked)
  (with-capture (dir agenda)
    ;; asked for the subject and attendees; clocked from when it began, the clock before going on after
    (let ((task (namestring (merge-pathnames "task.org" dir))))
      (alexandria:write-string-into-file (format nil "* Working~%") task)
      (run-keys (list "M-x o r g - c l o c k - i n RET"
                      (lambda () (setf tuatara.defaults::*org-now* (+ *journal-moment* 600)))
                      "SPC a c M P l a n RET A n n a RET N o t e s"
                      (lambda () (setf tuatara.defaults::*org-now* (+ *journal-moment* 1800)))
                      "C-c C-c")
                :file task :width 100 :height 16)
      (let ((text (file-text agenda)))
        (true (cl-ppcre:scan "\\* Meetings\\n\\*\\* Meeting: Plan +:meeting:" text) "~a" text)
        (true (search "Attendees: Anna" text))
        ;; the entry ends as written, with no blank lines added after it
        (true (alexandria:ends-with-subseq (format nil "Notes~%") text) "~s" (subseq text (max 0 (- (length text) 40))))
        (true (search "CLOCK: [2026-03-20 Fri 13:49]--[2026-03-20 Fri 14:09] =>  0:20" text)))
      ;; the task clocked before: stopped when the meeting began, clocked again after
      (let ((working (tuatara.buffer:buffer-string (tuatara.io:visit-file task))))
        (true (search "CLOCK: [2026-03-20 Fri 13:39]--[2026-03-20 Fri 13:49] =>  0:10" working) "~a" working)
        (true (search "CLOCK: [2026-03-20 Fri 14:09]" working)))
      (true tuatara.defaults::*org-clock*))))

(define-modal-test (capture meeting-planned-on-a-date)
  (with-capture (dir agenda)
    ;; m plans a meeting: its date asked as C-c C-s asks it, no clock; M, a capital, a template of its own
    (let ((directory (merge-pathnames "capture-templates/" dir)))
      (ensure-directories-exist directory)
      (alexandria:write-string-into-file
       (format nil "# name: Meeting planned~%# key: m~%# file: ~a~%# heading: Meetings~%# --~%* Meeting: %^{Subject}~%%^T~%%?~%" agenda)
       (merge-pathnames "m-planned.org" directory))
      (alexandria:write-string-into-file
       (format nil "# name: Meeting notes~%# key: M~%# file: ~a~%# heading: Meetings~%# clock-in: t~%# clock-resume: t~%# --~%* Notes: %^{Subject}~%%?~%" agenda)
       (merge-pathnames "M-notes.org" directory))
      (let ((f (run-keys "SPC a c m R e v i e w RET 2 0 2 6 - 0 3 - 2 3 SPC 1 0 : 0 0 RET A g e n d a C-c C-c"
                         :text "x"
                         :width 120
                         :height 16)))
        ;; asked with the calendar beside, as C-c C-s asks
        (true (tuatara.frontend.fake:fake-frame-showing f "Calendar")))
      (let ((text (file-text agenda)))
        (true (search (format nil "** Meeting: Review~%<2026-03-23 Mon 10:00>~%Agenda") text) "~a" text)
        (false (search "CLOCK:" text)))
      (false tuatara.defaults::*org-clock*)
      ;; two days chosen in the calendar, a timestamp each
      (let ((tuatara-user:*org-read-date-in-calendar* t))
        (run-keys "SPC a c m P l a n RET m l l m RET SPC 1 0 : 0 0 RET N o t e s C-c C-c"
                  :text "x"
                  :width 120
                  :height 16))
      (let ((text (file-text agenda)))
        (true (search (format nil "** Meeting: Plan~%<2026-03-20 Fri 10:00> <2026-03-22 Sun 10:00>~%Notes") text) "~a" text))
      ;; what is no date asked again, never written empty
      (multiple-value-bind (f)
          (run-keys "SPC a c m L a t e r RET x y z RET 2 0 2 6 - 0 3 - 2 4 RET N o C-c C-c"
                    :text "x"
                    :width 120
                    :height 16)
        (true (tuatara.frontend.fake:fake-frame-showing f "No date read, when:")))
      (true (search (format nil "** Meeting: Later~%<2026-03-24 Tue>~%No") (file-text agenda)) "~a" (file-text agenda))
      (run-keys "SPC a c M T a l k RET S a i d C-c C-c" :text "x" :width 120 :height 16)
      (let ((text (file-text agenda)))
        (true (search (format nil "** Notes: Talk~%") text) "~a" text)
        (true (search "CLOCK: [2026-03-20 Fri 13:39]" text) "~a" text)))))

(define-test (capture filled-in)
  ;; the escapes filled in, read from the template alone
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 3 14 6 10 2026)))
    (is string= "<2026-10-06 Tue> [2026-10-06 Tue 14:03] 14:03 50% sel [[file:x][X]]"
        (tuatara.defaults::org-capture-fill "%t %U %<%H:%M> 50%% %i %a" :link '("file:x" "X") :selection "sel"))
    (is = 2 (nth-value 1 (tuatara.defaults::org-capture-fill "a %?b")))
    (is string= (format nil "** x~%*** y~%no * star")
        (tuatara.defaults::org-shift-levels (format nil "* x~%** y~%no * star") 1))))

;;; Refile

(defparameter *refile-agenda*
  (format nil "* Inbox~%** Call back~%About the case.~%*** Detail~%~%* Commitments and Jobs~%** TODO Old~%Text.~%")
  "An agenda file for the refile tests.")

(define-modal-test (capture refile-within-a-file)
  (with-capture (dir agenda)
    (alexandria:write-string-into-file *refile-agenda* agenda :if-exists :supersede)
    ;; on Call back: C-c C-w, Commitments and Jobs: moved with what is under it, a level the same
    (multiple-value-bind (f editor)
        (run-keys "j j C-c C-w C o m m i t m e n t s RET" :file agenda :width 100 :height 16)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Refiled to agenda.org/Commitments and Jobs"))
      (is string= (format nil "* Inbox~%~%* Commitments and Jobs~%** TODO Old~%Text.~%** Call back~%About the case.~%*** Detail~%")
          (file-text agenda)))
    ;; into a deeper heading: its levels put one below it
    (forget-file-buffers)
    (alexandria:write-string-into-file *refile-agenda* agenda :if-exists :supersede)
    (run-keys "j j C-c C-w C o m m i t m e n t s / O l d RET" :file agenda :width 100 :height 16)
    (true (search (format nil "** TODO Old~%Text.~%*** Call back~%About the case.~%**** Detail~%") (file-text agenda))
          "~a" (file-text agenda))
    ;; a heading and what is under it are never offered as where it goes
    (forget-file-buffers)
    (alexandria:write-string-into-file *refile-agenda* agenda :if-exists :supersede)
    (let* ((buffer (tuatara.io:visit-file agenda))
           (names (mapcar #'tuatara.command:candidate-string
                          (tuatara.defaults::org-refile-candidates buffer :exclude (list (tuatara.defaults::org-parse-now buffer) 0 (search "* Commitments" *refile-agenda*))))))
      (false (find "agenda.org/Inbox" names :test #'string=))
      (false (find "agenda.org/Inbox/Call back" names :test #'string=))
      (true (find "agenda.org/Commitments and Jobs/Old" names :test #'string=)))))

(define-modal-test (capture refile-between-files-and-clock)
  (with-capture (dir agenda)
    (alexandria:write-string-into-file *refile-agenda* agenda :if-exists :supersede)
    (let ((note (namestring (merge-pathnames "note.org" dir))))
      (alexandria:write-string-into-file (format nil "* Keep~%* Idea~%Worth doing.~%") note)
      ;; clocked in on Idea, refiled into the agenda: both files saved, the clock going on there
      (run-keys (list "j M-x o r g - c l o c k - i n RET C-c C-w C o m m i t m e n t s RET"
                      (lambda () (setf tuatara.defaults::*org-now* (+ *journal-moment* 600)))
                      "SPC a O")
                :file note :width 100 :height 16)
      ;; the note had a change of its own (the clock's line), so it is left to be saved; the agenda saved
      (is string= (format nil "* Keep~%") (tuatara.buffer:buffer-string (tuatara.io:visit-file note)))
      (true (search "** Idea" (file-text agenda)))
      ;; the clock went with it, and stopped there
      (let ((text (tuatara.buffer:buffer-string (tuatara.io:visit-file agenda))))
        (true (search (format nil "** Idea~%:LOGBOOK:~%CLOCK: [2026-03-20 Fri 13:39]--[2026-03-20 Fri 13:49] =>  0:10~%:END:~%Worth doing.") text)
              "~a" text)))))

(define-modal-test (capture refile-from-agenda-and-capture)
  (with-capture (dir agenda)
    (alexandria:write-string-into-file (format nil "* Inbox~%** TODO Sort me~%* Commitments and Jobs~%") agenda :if-exists :supersede)
    ;; from the agenda's TODO list, the entry on the line
    (run-keys "SPC a a t / S o r t RET C-c C-w C o m m i t RET" :text "x" :width 100 :height 16)
    (true (search (format nil "* Inbox~%* Commitments and Jobs~%** TODO Sort me") (file-text agenda))
          "~a"
          (file-text agenda))
    ;; a capture filed elsewhere than its template says
    (run-keys "SPC a c n E l s e w h e r e C-c C-w C o m m i t RET" :text "x" :width 100 :height 16)
    (true (search (format nil "** TODO Sort me~%** Elsewhere") (file-text agenda)) "~a" (file-text agenda))
    (false (search "Inbox~%** Elsewhere" (file-text agenda)))
    ;; C-u C-c C-w goes to the heading, moving nothing
    (forget-file-buffers)
    (multiple-value-bind (f editor) (run-keys "C-u C-c C-w C o m m i t RET" :file agenda :width 100 :height 16)
      (declare (ignore f))
      (is = (search "* Commitments" (session-text editor)) (session-point editor)))))

(define-modal-test (capture resumes-the-right-task)
  (with-capture (dir agenda)
    ;; the meeting filed above the task clocked: the clock goes back to that task, not the entry above it
    (alexandria:write-string-into-file (format nil "* Meetings~%* Work~%** Task~%") agenda :if-exists :supersede)
    (run-keys (list "j j M-x o r g - c l o c k - i n RET"
                    (lambda () (setf tuatara.defaults::*org-now* (+ *journal-moment* 600)))
                    "SPC a c M P l a n RET A n n a RET N o t e s"
                    (lambda () (setf tuatara.defaults::*org-now* (+ *journal-moment* 1800)))
                    "C-c C-c")
              :file agenda :width 100 :height 16)
    (is string= "Task" (tuatara.defaults::org-clock-task tuatara.defaults::*org-clock*))
    (let ((text (tuatara.buffer:buffer-string (tuatara.io:visit-file agenda))))
      (true (cl-ppcre:scan "\\*\\* Task\\n:LOGBOOK:\\nCLOCK: \\[2026-03-20 Fri 14:09\\]\\n" text) "~a" text))))

(define-modal-test (capture journal-over-midnight-and-escaped-link)
  (with-capture (dir agenda)
    ;; begun before midnight, filed after: in the entry of the day it was begun
    (run-keys (list (lambda () (setf tuatara.defaults::*org-now* (encode-universal-time 0 58 23 20 3 2026)))
                    "SPC a c j L a t e"
                    (lambda () (setf tuatara.defaults::*org-now* (encode-universal-time 0 2 0 21 3 2026)))
                    "C-c C-c")
              :text "x" :width 100 :height 16)
    (let ((files (journal-files dir)))
      (is = 1 (length files))
      (true (search "20260320" (first files)) "~a" files)))
  ;; the link back escaped as Org escapes it
  (is string= "[[file:x::a\\[0\\]][a[0]]]"
      (tuatara.defaults::org-capture-fill "%a" :link '("file:x::a[0]" "a[0]"))))

(define-test (capture refile-to-one-of-two-and-to-nothing)
  (with-capture (dir agenda)
    (alexandria:write-string-into-file (format nil "* Projects~%** Notes~%first~%** Notes~%second~%* Inbox~%** Item~%") agenda :if-exists :supersede)
    (let* ((buffer (tuatara.io:visit-file agenda))
           (tree (tuatara.defaults::org-parse-tree (tuatara.defaults::org-parse-now buffer)))
           (item (find "Item"
                       (tuatara.defaults::org-headlines tree)
                       :key (lambda (h) (tuatara.defaults::org-property h :raw-value))
                       :test #'string=))
           (second-notes (second (remove "Notes"
                                         (tuatara.defaults::org-headlines tree)
                                         :key (lambda (h) (tuatara.defaults::org-property h :raw-value))
                                         :test-not #'string=))))
      ;; offered twice, told apart by where each begins
      (is = 2 (count "agenda.org/Projects/Notes" (tuatara.defaults::org-refile-candidates buffer)
                     :key #'tuatara.command:candidate-string :test #'string=))
      ;; to a heading that is not there: nothing taken away
      (fail (tuatara.defaults::org-refile-subtree buffer item (list :olp agenda "Nope")))
      (is string= (format nil "* Projects~%** Notes~%first~%** Notes~%second~%* Inbox~%** Item~%")
          (tuatara.buffer:buffer-string buffer))
      ;; to the second of two with the same titles
      (tuatara.defaults::org-refile-subtree buffer item (list :olp agenda "Projects" "Notes") (tuatara.defaults::org-node-begin second-notes))
      (is string= (format nil "* Projects~%** Notes~%first~%** Notes~%second~%*** Item~%* Inbox~%")
          (tuatara.buffer:buffer-string buffer)))))

(defun capture-headline (buffer title)
  "The headline of BUFFER's parse now whose title is TITLE."
  (find title (tuatara.defaults::org-headlines (tuatara.defaults::org-parse-tree (tuatara.defaults::org-parse-now buffer)))
        :key (lambda (h) (tuatara.defaults::org-property h :raw-value)) :test #'string=))

(define-test (capture refiled-clock-keeps-its-own-line)
  ;; a clock line left open above the running one in the subtree moved: the clock stays on its own
  (with-capture (dir agenda)
    (alexandria:write-string-into-file
     (format nil "* Inbox~%** Item~%:LOGBOOK:~%CLOCK: [2026-01-01 Thu 10:00]~%:END:~%*** Sub~%* Projects~%")
     agenda :if-exists :supersede)
    (let ((buffer (tuatara.io:visit-file agenda)))
      (unwind-protect
           (progn
             (setf (tuatara.buffer:buffer-point buffer)
                   (search "*** Sub" (tuatara.buffer:buffer-string buffer)))
             (tuatara.defaults::org-clock-in-here buffer)
             (tuatara.defaults::org-refile-subtree buffer (capture-headline buffer "Item") (list :olp agenda "Projects"))
             (let* ((clock tuatara.defaults::*org-clock*)
                    (start (tuatara.defaults::org-clock-line-start clock))
                    (line (tuatara.buffer:buffer-string buffer :start start
                                                               :end (tuatara.buffer:buffer-line-end buffer start))))
               (true (search (format nil "* Projects~%** Item") (tuatara.buffer:buffer-string buffer))
                     "~a" (tuatara.buffer:buffer-string buffer))
               (false (search "2026-01-01" line) "~a" line)))
        (setf tuatara.defaults::*org-clock* nil)))))

(define-test (capture clocking-in-with-no-task-keeps-the-clock-running)
  ;; C-c C-x C-i above the first headline says so and leaves the clock running, its line open
  (with-capture (dir agenda)
    (alexandria:write-string-into-file (format nil "#+title: Notes~%~%* Task~%Text~%") agenda :if-exists :supersede)
    (let ((buffer (tuatara.io:visit-file agenda)))
      (unwind-protect
           (progn
             (setf (tuatara.buffer:buffer-point buffer) (search "* Task" (tuatara.buffer:buffer-string buffer)))
             (tuatara.defaults::org-clock-in-here buffer)
             (let ((clock tuatara.defaults::*org-clock*))
               (true clock)
               (setf (tuatara.buffer:buffer-point buffer) 0)
               (tuatara.defaults::org-clock-in-here buffer)
               (true (eq clock tuatara.defaults::*org-clock*))
               (false (search "=>" (tuatara.buffer:buffer-string buffer)) "~a" (tuatara.buffer:buffer-string buffer))))
        (setf tuatara.defaults::*org-clock* nil)))))

(define-test (capture refile-targets-alike-in-agenda-files)
  ;; an agenda file offers its archived and commented headings to refile into, as any file does
  (with-capture (dir agenda)
    (alexandria:write-string-into-file (format nil "* Tasks~%* Archive :ARCHIVE:~%* COMMENT Drafts~%") agenda
                                       :if-exists :supersede)
    (let ((titles (mapcar #'second (first (tuatara.defaults::org-files-headings (list (namestring agenda)))))))
      (is equal '(("Tasks") ("Archive") ("Drafts")) titles))))

(define-modal-test (capture empty-question-and-windows-lines)
  (with-capture (dir agenda)
    (let ((directory (merge-pathnames "capture-templates/" dir)))
      (ensure-directories-exist directory)
      ;; a template file whose lines end in carriage returns, asking with no prompt of its own
      (alexandria:write-string-into-file
       (format nil
               "~{~a~c~%~}"
               (loop for line in (list "# name: Plain" "# key: p" (format nil "# file: ~a" agenda) "# heading: Other"
                                        "# --" "* %^{}%?" "Body")
                     append (list line #\Return)))
       (merge-pathnames "p-plain.org" directory))
      (let ((template (find "p"
                            (tuatara.defaults::capture-templates)
                            :key #'tuatara.defaults::capture-template-key
                            :test #'string=)))
        (is string= (format nil "* %^{}%?~%Body") (tuatara.defaults::capture-template-text template)))
      (let ((f (run-keys "SPC a c p" :text "x" :width 200 :height 16)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Answer:")))
      (run-keys "SPC a c p H e l l o RET C-c C-c" :text "x" :width 200 :height 16)
      (true (search (format nil "** Hello~%Body") (file-text agenda)) "~a" (file-text agenda)))))

(define-test (capture templates-in-the-configuration-directory)
  ;; no directories named: your templates are those in capture-templates/ of the configuration directory,
  ;; XDG_CONFIG_HOME's
  (with-test-directory (dir)
    (let ((directory (merge-pathnames "tuatara/capture-templates/" dir))
          (tuatara-user:*capture-template-directories* nil))
      (ensure-directories-exist directory)
      (alexandria:write-string-into-file (format nil
                                                 "# name: Kea seen~%# key: z~%# file: ~~/notes/kea.org~%# --~%* %?~%")
                                         (merge-pathnames "z-kea.org" directory))
      (with-environment-variable ("XDG_CONFIG_HOME" (namestring dir))
        (is equal (list (namestring directory)) (tuatara.defaults::capture-template-directories))
        (true (find "z" (tuatara.defaults::capture-templates) :key #'tuatara.defaults::capture-template-key
                                                               :test #'string=))))))

(define-modal-test (capture on-the-day-at-point)
  (with-capture (dir agenda)
    ;; from the calendar, three days on: the day's date under the heading
    (run-keys "M-x c a l e n d a r RET l l l i n D e n t i s t C-c C-c" :text "x" :width 100 :height 30)
    (true (search (format nil "* Inbox~%** Dentist~%<2026-03-23 Mon>~%[2026-03-20 Fri 13:39]") (file-text agenda))
          "~a" (file-text agenda))
    ;; a journal template: into that day's entry
    (run-keys "M-x c a l e n d a r RET l l l l i j P a c k e d C-c C-c" :text "x" :width 100 :height 30)
    (let ((files (journal-files dir)))
      (is = 1 (length files))
      (true (search "20260324" (first files)) "~a" files))
    ;; from the agenda, the day of the line point is on
    (run-keys "SPC a a a i n C a l l SPC R N C-c C-c" :text "x" :width 100 :height 30)
    (true (search (format nil "** Call RN~%<2026-03-20 Fri>~%") (file-text agenda)) "~a" (file-text agenda))
    ;; the agenda's title line has no day: nothing asked
    (true (tuatara.frontend.fake:fake-frame-showing (run-keys "SPC a a a g g i" :text "x" :width 100 :height 30)
                                                    "No day here")))
  ;; %t and %T give the day; the date put after the first line, point moved with it
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 3 14 6 10 2026))
        (day (tuatara.defaults::org-today (encode-universal-time 0 0 12 9 10 2026))))
    (is string= "<2026-10-09 Fri> <2026-10-09 Fri> [2026-10-06 Tue 14:03]"
        (tuatara.defaults::org-capture-fill "%t %T %U" :day day))
    (is equal (list (format nil "* a~%<2026-10-09 Fri>~%b") 21)
        (multiple-value-list (tuatara.defaults::org-capture-on-day-text (format nil "* a~%b") 4 day)))
    (is equal (list (format nil "* a~%<2026-10-09 Fri>") 2)
        (multiple-value-list (tuatara.defaults::org-capture-on-day-text "* a" 2 day)))))

(define-modal-test (capture into-a-directory-not-there-yet)
  ;; the agenda file in a directory not there yet: the task filed, the directory made once asked
  (with-capture (dir agenda)
    (let* ((file (namestring (merge-pathnames "new/tasks.org" dir)))
           (tuatara-user:*org-agenda-files* (list file)))
      (multiple-value-bind (f editor)
          (run-keys "SPC a c n C a l l SPC N g a i o C-c C-c y RET" :text "x" :width 100 :height 16)
        (declare (ignore editor))
        (false (tuatara.frontend.fake:fake-frame-showing f "read-only"))
        (true (probe-file file))
        (true (search "** Call Ngaio" (file-text file))))
      (let ((b (tuatara.buffer:find-buffer "tasks.org")))
        (when b (tuatara.buffer:buffer-mark-saved b) (tuatara.buffer:kill-buffer b))))
    ;; a directory of agenda files: agenda.org in it
    (let ((tuatara-user:*org-agenda-files* (list (namestring dir))))
      (is string= (namestring (merge-pathnames "agenda.org" dir)) (tuatara.defaults::org-agenda-main-file)))
    (let ((tuatara-user:*org-agenda-files* (list (concatenate 'string (namestring dir) "*.org"))))
      (is string= (namestring (merge-pathnames "agenda.org" dir)) (tuatara.defaults::org-agenda-main-file)))
    ;; a package's file function that asks for the main file, as a calendar package does: answered, not asked again
    (let ((tuatara-user:*org-agenda-files* (list (namestring (merge-pathnames "main.org" dir))))
          (tuatara:*org-agenda-file-functions*
            (list (lambda () (list (namestring (merge-pathnames "calendars.org" (tuatara.defaults::org-agenda-main-file))))))))
      (is string= (namestring (merge-pathnames "main.org" dir))
          (handler-case (tuatara.defaults::org-agenda-main-file)
            (storage-condition () "the main file asked for itself without end"))))))

(define-test (capture menu-fits-a-terminal)
  ;; the choice of Tuatara's own templates is said whole within 120 columns, the first choice in sight
  (let ((prompt (tuatara.defaults::org-capture-prompt
                 (loop for (name text) in tuatara.defaults::*built-in-capture-templates*
                       collect (tuatara.defaults::read-capture-template name text t)))))
    (true (< (length prompt) 110) "~d characters: ~a" (length prompt) prompt)
    (true (search "[j] Journal entry" prompt))))

(defvar *capture-with-target* nil
  "Where CAPTURE-WITH-TEST-COMMAND files, for the test of ORG-CAPTURE-WITH.")

(tuatara.command:define-command capture-with-test-command ()
  "Capture with a template given as text, for the test of ORG-CAPTURE-WITH."
  (tuatara.defaults::org-capture-with "Event" *capture-with-target*
                                      (format nil "* %?~%:PROPERTIES:~%:LOCATION:~%:END:~%")))

(define-test (capture with-a-template-given)
  ;; a template a package gives as text, filed under the heading it names
  (with-capture (dir agenda)
    (let ((*capture-with-target* (list :olp agenda "Other")))
      (let ((f (run-keys "M-x c a p t u r e - w i t h - t e s t - c o m m a n d RET D e n t i s t C-c C-c"
                         :text "x"
                         :width 100
                         :height 16)))
        (true (search (format nil "* Other~%** Leave me :home:~%** Dentist~%:PROPERTIES:~%:LOCATION:~%:END:")
                      (file-text agenda))
              "~a" (file-text agenda))
        (true (tuatara.frontend.fake:fake-frame-showing f "Event filed under agenda.org/Other"))))))

(define-test (capture refile-the-last-subtree-from-the-end)
  ;; point at the very end of the file is under its last heading, which C-c C-w refiles
  (with-capture (dir agenda)
    (alexandria:write-string-into-file *refile-agenda* agenda :if-exists :supersede)
    (multiple-value-bind (f editor)
        (run-keys "M-> C-c C-w I n b o x RET" :file agenda :width 100 :height 16)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Refiled to agenda.org/Inbox"))
      (let ((text (file-text agenda)))
        (true (< (search "** TODO Old" text) (search "* Commitments and Jobs" text)) "~a" text)))))

(define-modal-test (capture refile-of-a-heading-gone-while-asking)
  ;; the heading being refiled taken away while its target is asked for: refused, the next heading left
  (with-capture (dir agenda)
    (alexandria:write-string-into-file *refile-agenda* agenda :if-exists :supersede)
    (multiple-value-bind (f editor)
        (run-keys (list "j C-c C-w"
                        (lambda ()
                          (let* ((buffer (tuatara.buffer:find-buffer "agenda.org"))
                                 (gone (format nil "** Call back~%About the case.~%*** Detail~%~%"))
                                 (at (search gone (tuatara.buffer:buffer-string buffer))))
                            (tuatara.buffer:buffer-delete buffer at (+ at (length gone)))))
                        "I n b o x RET")
                  :file agenda :width 100 :height 16)
      (true (tuatara.frontend.fake:fake-frame-showing f "The headline is no longer there"))
      (is string= (format nil "* Inbox~%* Commitments and Jobs~%** TODO Old~%Text.~%") (session-text editor)))))
