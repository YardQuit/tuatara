;;;; test/list-buffers.lisp --- The buffers commands fill: one fill, one refresh, kept current, one quit.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test list-buffers
  :description "The lists, help and reports commands make are filled one way (set-made-text,
render-pieces), their faces found by halves; r, g r and revert-buffer make each again, q leaves each
alike; *Buffers* keeps its marks across a refresh and follows the buffers' changes while shown, *Jobs*
is filled again only when the jobs have changed.")

(defparameter *list-modes*
  '(buffers-mode jobs-mode help-mode org-agenda-mode calendar-mode rimu-mode rimu-trash-mode font-list-mode
    xref-mode about-mode backtrace-mode code-files-mode pdf-attachments-mode)
  "The major modes of the buffers commands make, which every list's keys reach.")

(defun list-key-binding (mode keys)
  "What KEYS run in the buffers of major mode MODE, by its keymap and those of the modes it is made from."
  (tuatara.command:lookup-key (tuatara.modes:mode-keymap (find-symbol (symbol-name mode) :tuatara.defaults))
                              (tuatara.frontend:parse-key-sequence keys)))

(defun list-line (editor name)
  "The line of the current buffer of EDITOR that holds NAME."
  (find-if (lambda (l) (search name l)) (uiop:split-string (session-text editor) :separator '(#\Newline))))

(define-test (list-buffers same-keys-everywhere)
  ;; one refresh and one quit, the same keys in every list; the copies are gone
  (run-keys "" :text "")
  (dolist (mode *list-modes*)
    (is eq 'tuatara.defaults::revert-buffer (list-key-binding mode "r") "r in ~(~a~)" mode)
    (is eq 'tuatara.defaults::revert-buffer (list-key-binding mode "g r") "g r in ~(~a~)" mode)
    (is eq 'tuatara.defaults::quit-window (list-key-binding mode "q") "q in ~(~a~)" mode))
  (is eq 'tuatara.defaults::quit-window (list-key-binding 'buffers-mode "h"))
  (is eq 'tuatara.defaults::quit-window (list-key-binding 'jobs-mode "h"))
  ;; every list whose entries are opened goes back with h
  (dolist (mode '(buffers-mode packages-mode face-list-mode latex-errors-mode redactions-mode))
    (is eq 'tuatara.defaults::quit-window (list-key-binding mode "h") "h in ~(~a~)" mode))
  (dolist (gone '("BUFFERS-QUIT" "JOBS-QUIT" "HELP-QUIT" "ORG-AGENDA-QUIT" "CALENDAR-EXIT" "RIMU-QUIT"
                  "BUFFERS-REFRESH" "JOBS-REFRESH" "FONT-LIST-REFRESH" "RIMU-REFRESH" "RIMU-TRASH-REFRESH"
                  "ORG-AGENDA-REFRESH"))
    (let ((symbol (find-symbol gone :tuatara.defaults)))
      (false (and symbol (gethash symbol tuatara.command:*commands*)) "~(~a~) is no command" gone))))

(define-test (list-buffers spans-found-by-halves)
  ;; the faces shown between two places are those a look at every span finds, overlapping ones too
  (let ((b (tuatara.buffer:make-buffer "spans-test")))
    (unwind-protect
         (let* ((text (make-string 2000 :initial-element #\x))
                (spans (sort (loop repeat 300
                                   for start = (random 1990)
                                   collect (list start (+ start 1 (random (if (zerop (random 5)) 400 10)))
                                                 (nth (random 3) '(:type :keyword :comment))))
                             #'< :key #'first)))
           (tuatara.defaults::set-made-text b text :spans spans)
           (true (tuatara.buffer:buffer-local b :read-only))
           (false (tuatara.buffer:buffer-modified-p b))
           (dotimes (i 200)
             (let* ((from (random 2000)) (to (+ from (random 300))))
               (is equal (remove-if-not (lambda (span) (and (< (first span) to) (> (second span) from))) spans)
                   (tuatara.defaults::made-spans-between b from to))))
           ;; spans given out of order are put in order of their starts
           (tuatara.defaults::set-made-text b "abcdef" :spans '((4 6 :type) (0 2 :keyword)))
           (is equal '((0 2 :keyword) (4 6 :type)) (tuatara.defaults::made-spans-between b 0 6))
           ;; pieces: octets, faces, buttons and keys over a face
           (multiple-value-bind (text spans buttons)
               (tuatara.defaults::pieces-text (list "é" (cons "ab" :type) (list "cd" :keyword 'push-button)
                                                    (list (tuatara.command:substitute-command-keys "go \\`C-x' on" :marks t) :comment))
                                              :keys t)
             (is string= "éabcdgo C-x on" text)
             (is equal '((2 4 :type) (4 6 :keyword) (6 15 :comment) (9 12 :help-key-binding)) spans)
             (is equal '((4 6 push-button)) buttons)))
      (tuatara.buffer:kill-buffer b))))

(define-test (list-buffers table-columns)
  ;; the table helper lays out *Buffers*' and *Jobs*' titles and rows as they were written by hand
  (flet ((text (pieces) (tuatara.defaults::pieces-text pieces)))
    (is string= (format nil " MR  ~18a ~9@a ~16a Filename/Process~% --  ~18a ~9@a ~16a ----------------~%"
                        "Name" "Size" "Mode" "----" "----" "----")
        (text (tuatara.defaults::table-heading-pieces tuatara.defaults::*buffers-columns*)))
    (is string= (format nil "   ~20a ~26a Job~%   ~20a ~26a ---~%" "Buffer" "State" "------" "-----")
        (text (tuatara.defaults::table-heading-pieces tuatara.defaults::*jobs-columns*)))
    (is string= (format nil "D*%  a-name-longer-t... ~9@a Fundamental      ~%" "12,345")
        (text (tuatara.defaults::table-row-pieces tuatara.defaults::*buffers-columns*
                                                  (list "D*%" "a-name-longer-than-the-column" "12,345" "Fundamental" ""))))
    ;; a shaded column's face covers the spaces either side of it
    (multiple-value-bind (text spans)
        (tuatara.defaults::pieces-text (tuatara.defaults::table-row-pieces tuatara.defaults::*jobs-columns*
                                                                          (list " D" (cons "b" :list-deletion)
                                                                                (cons "Sorting 42%" :job-queued) "Sorting")))
      (is string= (format nil " D ~20a ~26a Sorting~%" "b" "Sorting 42%") text)
      (is equal '((3 23 :list-deletion) (23 51 :job-queued)) spans))))

(define-test (list-buffers marks-sections-questions)
  ;; marks kept by their keys, the first marked first; those of entries gone dropped
  (let ((list (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*marks test*"))))
    (unwind-protect
         (progn
           (tuatara.defaults::set-entry-mark list "b" :marked)
           (tuatara.defaults::set-entry-mark list "a" :marked)
           (tuatara.defaults::set-entry-mark list "c" :deletion)
           (is equal '("b" "a") (tuatara.defaults::list-marked list :marked))
           (is eq :deletion (tuatara.defaults::entry-mark list "c"))
           (tuatara.defaults::keep-list-marks list '("a" "c"))
           (is equal '("a") (tuatara.defaults::list-marked list :marked))
           (is eq nil (tuatara.defaults::entry-mark list "b"))
           (tuatara.defaults::set-entry-mark list "c" nil)
           (is equal '(("a" . :marked)) (tuatara.defaults::list-marks list)))
      (tuatara.buffer:kill-buffer list)))
  ;; sections in the order of their first items, or sorted, one put first and one last
  (flet ((numbers (sections) (mapcar (lambda (section) (cons (car section) (mapcar #'car (cdr section)))) sections)))
    (let ((items '((1 . "/b/") (2 . :home) (3 . "/a/") (4 . "/b/") (5 . :none))))
      (is equal '((:home 2) ("/b/" 1 4) ("/a/" 3) (:none 5))
          (numbers (tuatara.defaults::group-sections items #'cdr :first :home :last :none)))
      (is equal '((:home 2) ("/a/" 3) ("/b/" 1 4) (:none 5))
          (numbers (tuatara.defaults::group-sections items #'cdr :first :home :last :none :order #'string<)))))
  ;; the questions asked before acting on entries
  (is string= "Delete the bookmark x" (tuatara.defaults::list-question "Delete" "bookmark" '("x")))
  (is string= "Kill the 2 buffers of y" (tuatara.defaults::list-question "Kill" "buffer" '("x" "z") :after "of y"))
  (is string= "Copy 2 files in 2 directories to:"
      (tuatara.defaults::list-question "Copy" "file" '("a" "b") :the nil :where "in 2 directories" :after "to:"))
  (is string= "a" (tuatara.defaults::list-question nil "file" '("a") :the nil)))

(define-test (list-buffers buffers-refreshed-with-marks)
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (let ((one (tuatara.buffer:make-buffer "one" :text "1"))
        (two (tuatara.buffer:make-buffer "two" :text "2")))
    (unwind-protect
         (progn
           ;; r, g r and M-x revert-buffer make the list again, its marks kept
           (dolist (refresh '("r" "g r" "M-x r e v e r t - b u f f e r RET"))
             (let ((version nil))
               (multiple-value-bind (f editor)
                   (fresh-list-session (list "C-x C-b C-s o n e RET m g g C-s t w o RET d"
                                             (lambda () (setf version (tuatara.buffer:buffer-version (tuatara.buffer:find-buffer "*Buffers*"))))
                                             refresh)
                                       :width 100 :height 16)
                 (declare (ignore f))
                 (is string= "*Buffers*" (current-name editor))
                 (false (eq version (tuatara.buffer:buffer-version (tuatara.buffer:find-buffer "*Buffers*"))) "~a fills again" refresh)
                 (true (uiop:string-prefix-p ">" (list-line editor "one")) "~a keeps >" refresh)
                 (true (uiop:string-prefix-p "D" (list-line editor "two")) "~a keeps D: ~a" refresh (session-text editor))
                 (false (search "visits no file" (or (tuatara.command:editor-message editor) ""))))))
           ;; q and h leave it alike
           (dolist (keys '("C-x C-b q" "C-x C-b h"))
             (is string= "*scratch*" (current-name (nth-value 1 (fresh-list-session keys))))))
      (dolist (b (list one two)) (when (tuatara.command:live-buffer-p b) (tuatara.buffer:kill-buffer b)))
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (list-buffers buffers-follow-changes)
  ;; *Buffers* shown in a window follows what another window does: a buffer changed shows *, its size
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (let ((notes (tuatara.buffer:make-buffer "notes" :text "12345")))
    (unwind-protect
         (multiple-value-bind (f editor)
             (fresh-list-session "C-x C-b C-s n o t e s RET m C-x 2 C-x o C-x b n o t e s RET i x y C-g"
                                 :width 100 :height 24)
           (declare (ignore f editor))
           (let ((line (find-if (lambda (l) (search "notes" l))
                                (uiop:split-string (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Buffers*"))
                                                   :separator '(#\Newline)))))
             (true (uiop:string-prefix-p ">*" line) "~s" line)
             (true (search "      7 Fundamental" line) "~s" line)))
      (when (tuatara.command:live-buffer-p notes) (tuatara.buffer:kill-buffer notes))
      (let ((l (tuatara.buffer:find-buffer "*Buffers*"))) (when l (tuatara.buffer:kill-buffer l))))))

(define-test (list-buffers jobs-filled-when-changed)
  ;; *Jobs* shown is not filled again while nothing changes
  (let ((versions '()))
    (flet ((note () (push (tuatara.buffer:buffer-version (tuatara.buffer:find-buffer "*Jobs*")) versions)))
      (multiple-value-bind (f editor)
          (run-keys (list "M-x l i s t - j o b s RET" #'note "j" :tick "k" :tick "j" #'note "r" #'note)
                    :text "x" :width 150 :height 12)
        (declare (ignore f))
        (is string= "*Jobs*" (current-name editor))
        (destructuring-bind (after-r after-moves first) versions
          (is eq first after-moves)
          (false (eq after-moves after-r))))))
  ;; and is when the jobs change: the replacements listed, then gone once done
  (let ((tuatara.defaults::*rewrite-piece-hook* (lambda () (sleep 0.2)))
        (seen nil))
    (with-background ()
      (multiple-value-bind (f editor)
          (run-keys (list "r r % a / b RET r r % b / c RET M-x l i s t - j o b s RET"
                          (lambda () (setf seen (search "Replacing" (session-text tuatara.command:*editor*))))
                          (lambda () (loop repeat 400
                                           until (search "No jobs in the background" (session-text tuatara.command:*editor*))
                                           do (tuatara.command::run-tick-functions tuatara.command:*editor*)
                                              (sleep 0.05)))
                          :tick "j")
                    :text "a a a" :width 150 :height 12)
        (declare (ignore f))
        (true seen)
        (true (search "No jobs in the background" (session-text editor)))))))

(define-test (list-buffers job-ending-while-listed)
  ;; a job that ends just as *Jobs* is filled does not stay listed: the list is made again
  (let* ((job (tuatara.defaults::%make-hash-job :what "a sum"))
         (tuatara.runtime:*jobs* (list job))
         (ended nil))
    (sb-int:encapsulate 'tuatara.defaults::jobs-entries 'ends-the-job
                        (lambda (function)
                          (prog1 (funcall function)
                            (unless ended
                              (setf ended t
                                    (tuatara.runtime:job-status job) :done)))))
    (unwind-protect
         (multiple-value-bind (f editor)
             (run-keys (list "M-x l i s t - j o b s RET" :tick :tick) :text "x" :width 100 :height 12)
           (declare (ignore f))
           (is string= "*Jobs*" (current-name editor))
           (true (search "No jobs in the background" (session-text editor))))
      (sb-int:unencapsulate 'tuatara.defaults::jobs-entries 'ends-the-job)
      (tuatara.runtime:forget-job job))))

(defvar *list-test-value* 1
  "A value the help shown in a test describes, changed while it is shown.")

(define-test (list-buffers help-made-again)
  ;; help made from what it describes is made again by r, from what that is now; q leaves it
  (setf *list-test-value* 1)
  (multiple-value-bind (f editor)
      (run-keys (list (lambda () (tuatara.defaults::show-help (lambda () (format nil "Value: ~d~%" *list-test-value*))))
                      "C-x o"
                      (lambda () (setf *list-test-value* 2))
                      "r")
                :width 80 :height 20)
    (declare (ignore f))
    (is string= "*Help*" (current-name editor))
    (is string= (format nil "Value: 2~%") (session-text editor)))
  (multiple-value-bind (f editor) (run-keys "C-h k C-f C-x o q" :width 80 :height 20)
    (declare (ignore f))
    (is string= "*scratch*" (current-name editor))
    (is = 1 (length (tuatara.command:editor-windows editor))))
  ;; its faces: the keys named, in :help-key-binding, found as before
  (multiple-value-bind (f editor) (run-keys "C-h k C-f" :width 90 :height 30)
    (declare (ignore editor))
    (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "Keys: ")
      (let ((row (position-if (lambda (line) (search "Keys: " line)) screen)))
        (is equal '((6 7 :help-key-binding) (9 12 :help-key-binding) (14 21 :help-key-binding))
            (aref spans row))))))

(define-test (list-buffers agenda-made-again)
  (with-test-directory (dir)
    (let* ((source (asdf:system-relative-pathname :tuatara "test/org/agenda/"))
           (tasks (merge-pathnames "tasks.org" dir)))
      (uiop:copy-file (merge-pathnames "tasks.org" source) tasks)
      (let ((tuatara.defaults::*org-agenda-files* (list (namestring tasks)))
            (tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
            (tuatara.defaults::*org-agenda-log-mode* nil)
            (version nil))
        ;; r makes it again, as revert-buffer does, never "visits no file"; q leaves
        (dolist (refresh '("r" "g r" "M-x r e v e r t - b u f f e r RET"))
          (multiple-value-bind (f editor)
              (run-keys (list "SPC a a a" (lambda () (setf version (tuatara.buffer:buffer-version (session-buffer tuatara.command:*editor*))))
                              refresh)
                        :width 100 :height 40)
            (declare (ignore f))
            (is string= "*Org Agenda*" (current-name editor))
            (false (eq version (tuatara.buffer:buffer-version (session-buffer editor))) "~a fills again" refresh)
            (false (search "visits no file" (or (tuatara.command:editor-message editor) "")))))
        (is string= "*scratch*" (current-name (nth-value 1 (run-keys "SPC a a a q" :width 100 :height 40))))
        ;; its faces, found by halves as before
        (multiple-value-bind (f editor) (run-keys "SPC a a a" :width 100 :height 60)
          (declare (ignore editor))
          (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "Week-agenda")
            (let ((row (position-if (lambda (line) (search "Week-agenda" line)) screen)))
              (true (find :org-agenda-structure (aref spans row) :key #'third)))))))))

(define-test (list-buffers others-made-again)
  ;; *Apropos* and the calendar: r makes them again where they are, q leaves
  (multiple-value-bind (f editor) (run-keys "C-h a t r a n s p o s e RET r" :width 100 :height 40)
    (declare (ignore f))
    (is string= "*Apropos*" (current-name editor))
    (true (search "transpose" (session-text editor)))
    (false (search "visits no file" (or (tuatara.command:editor-message editor) ""))))
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 0 18 1 10 2026))
        (tuatara.defaults::*org-agenda-files* '()))
    (multiple-value-bind (f editor) (run-keys "M-x c a l e n d a r RET C-f r" :width 100 :height 30)
      (declare (ignore f))
      (is string= "*Calendar*" (current-name editor))
      (is = (tuatara.defaults::day-number 2026 10 2)
          (tuatara.defaults::calendar-date-at (session-buffer editor))))
    (multiple-value-bind (f editor) (run-keys "M-x c a l e n d a r RET q" :width 100 :height 30)
      (declare (ignore f))
      (is string= "*scratch*" (current-name editor)))))

(tuatara.modes:define-major-mode test-previewed-list-mode tuatara.defaults::special-mode
  "A list made by the tests, the entry on its current line shown beside it."
  :lighter "Previewed" :interactive nil)

(defmethod tuatara.defaults::preview-entry ((mode test-previewed-list-mode) list entry)
  "A note made for the preview, saying ENTRY's name."
  (declare (ignore list))
  (values (tuatara.defaults::side-preview-note entry (format nil "All about ~a." entry)) t))

(tuatara.defaults::entry-preview-keys 'test-previewed-list-mode "a list made by the tests")

(define-test (list-buffers previewed-beside)
  ;; a list whose major mode has a preview-entry method shows the entry on its line
  ;; beside it as the cursor moves; P turns that off and on, the notes made for it going
  (flet ((session (&rest keys)
           (run-keys (list* (lambda ()
                              (let ((list (tuatara.defaults::buffer-in-mode "*Previewed*" 'test-previewed-list-mode)))
                                (tuatara.defaults::render-pieces list (list (format nil "alpha~%beta~%")) :entries '("alpha" "beta") :point :start)
                                (setf (tuatara.buffer:buffer-local list :list-preview) t)
                                (tuatara.command:show-buffer list)))
                            keys)
                     :width 100 :height 12))
         (shown (f text) (loop for row below 12 thereis (search text (tuatara.frontend.fake:fake-screen-line f row))))
         (notes () (remove-if-not (lambda (b) (search "(preview)" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*)))
    (unwind-protect
         (progn
           (let ((f (session "C-p" "C-n")))
             (true (tuatara.frontend.fake:fake-frame-showing f "All about alpha."))
             (true (shown f "All about beta."))
             (false (shown f "All about alpha."))
             (is = 1 (length (notes))))
           (let ((f (session "C-n" "P")))
             (false (shown f "All about"))
             (true (search "Preview off" (echo-text f)))
             (is = 0 (length (notes))))
           (let ((f (session "P" "P")))
             (true (shown f "All about alpha."))))
      (let ((list (tuatara.buffer:find-buffer "*Previewed*")))
        (when list (tuatara.buffer:kill-buffer list)))
      (mapc #'tuatara.buffer:kill-buffer (notes)))))

(define-test (list-buffers marks-of-every-list)
  ;; one sign and one face for a mark in any list: the keywords of the lists, rimu's characters
  (is eq :list-marked (tuatara.defaults::list-mark-face :marked))
  (is eq :list-marked (tuatara.defaults::list-mark-face #\>))
  (is eq :list-deletion (tuatara.defaults::list-mark-face :deletion))
  (is eq :list-deletion (tuatara.defaults::list-mark-face #\D))
  (is eq :list-marked-other (tuatara.defaults::list-mark-face #\M))
  (false (tuatara.defaults::list-mark-face nil))
  (is equal '(">" "D" "M" " ") (mapcar #'tuatara.defaults::list-mark-sign '(:marked :deletion #\M nil))))

(define-test (list-buffers pieces-collected)
  ;; pieces collected in order, each in the form the lists draw: a string, (text . face),
  ;; (text face action), or one made already
  (let* ((action (lambda () nil))
         (pieces (tuatara.defaults::collecting-pieces (put)
                   (put "plain")
                   (put "faced" :heading)
                   (put "button" :button action)
                   (put (cons "made" :list-dim)))))
    (is equal (list "plain" (cons "faced" :heading) (list "button" :button action) (cons "made" :list-dim)) pieces)
    (multiple-value-bind (text spans buttons) (tuatara.defaults::pieces-text pieces)
      (is string= "plainfacedbuttonmade" text)
      (is equal '((5 10 :heading) (10 16 :button) (16 20 :list-dim)) spans)
      (is equal (list (list 10 16 action)) buttons))))
