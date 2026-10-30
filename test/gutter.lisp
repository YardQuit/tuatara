;;;; test/gutter.lisp --- Line numbers and the version-control gutter.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test gutter
  :description "Line numbers absolute and relative in the margin, cursor offset, and git marks for added, changed and deleted lines.")

(define-test (gutter line-numbers)
  (multiple-value-bind (f editor) (run-keys "M-x d i s p l a y - l i n e - n u m b e r s - m o d e RET j" :text (format nil "aa~%bb~%cc"))
    (declare (ignore editor))
    (is string= "1 aa" (tuatara.frontend.fake:fake-screen-line f 0))
    (is string= "2 bb" (tuatara.frontend.fake:fake-screen-line f 1))
    ;; cursor sits after the margin
    (is equal '(1 . 2) (tuatara.frontend.fake:fake-cursor f))
    ;; the current line's number is bold, the others dim
    (is equal '((0 1 :current-line-number)) (aref (tuatara.frontend.fake:fake-spans f) 1))
    (is equal '((0 1 :line-number)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
  (multiple-value-bind (f editor) (run-keys "M-x d i s p l a y - r e l a t i v e - l i n e - n u m b e r s - m o d e RET j j"
                                            :text (format nil "~{l~a~%~}" (loop for i below 12 collect i)))
    (declare (ignore editor))
    ;; width follows the 13 lines; row 2 is current and shows 3, the others their distance
    (is string= " 2 l0" (tuatara.frontend.fake:fake-screen-line f 0))
    (is string= " 1 l1" (tuatara.frontend.fake:fake-screen-line f 1))
    (is string= " 3 l2" (tuatara.frontend.fake:fake-screen-line f 2))
    (is string= " 1 l3" (tuatara.frontend.fake:fake-screen-line f 3)))
  ;; the default for every buffer, as the init file sets it; a buffer's own choice wins
  (let ((tuatara-user:*line-numbers-default* :relative))
    (multiple-value-bind (f editor) (run-keys "j j" :text (format nil "l0~%l1~%l2~%l3~%"))
      (declare (ignore editor))
      (is string= "2 l0" (tuatara.frontend.fake:fake-screen-line f 0))
      (is string= "3 l2" (tuatara.frontend.fake:fake-screen-line f 2))
      (is string= "1 l3" (tuatara.frontend.fake:fake-screen-line f 3)))
    (multiple-value-bind (f editor) (run-keys "M-x d i s p l a y - r e l a t i v e - l i n e - n u m b e r s - m o d e RET" :text "x")
      (declare (ignore editor))
      (is string= "x" (tuatara.frontend.fake:fake-screen-line f 0))))
  ;; the line after the last newline has no number: ~ there, in the plain
  ;; colour when point rests on it, and the width counts the numbered lines
  (let ((tuatara-user:*line-numbers-default* :absolute))
    (multiple-value-bind (f editor) (run-keys "g e" :text (format nil "~{l~a~%~}" (loop for i below 9 collect i)) :height 14)
      (declare (ignore editor))
      (is string= "1 l0" (tuatara.frontend.fake:fake-screen-line f 0))
      (is string= "~" (tuatara.frontend.fake:fake-screen-line f 9))
      (is equal '((0 1 :line-number)) (aref (tuatara.frontend.fake:fake-spans f) 9))
      (is equal '(9 . 2) (tuatara.frontend.fake:fake-cursor f)))
    (is string= "~" (tuatara.frontend.fake:fake-screen-line (run-keys "" :text "") 0))
    ;; the last line with text numbered as ever
    (is string= "2 b" (tuatara.frontend.fake:fake-screen-line (run-keys "" :text (format nil "a~%b")) 1)))
  (let ((tuatara-user:*line-numbers-default* :relative))
    (is string= "~" (tuatara.frontend.fake:fake-screen-line (run-keys "" :text (format nil "a~%")) 1)))
  ;; toggling off restores the plain rows
  (multiple-value-bind (f editor) (run-keys "M-x t o g g l e - l i n e - n u m b e r s RET M-x t o g g l e - l i n e - n u m b e r s RET M-x t o g g l e - l i n e - n u m b e r s RET" :text "x")
    (declare (ignore editor))
    (is string= "x" (tuatara.frontend.fake:fake-screen-line f 0))))

(defun git (directory &rest args)
  "Run git in DIRECTORY with ARGS; return its output."
  (uiop:run-program (list* "git" "-C" (namestring directory) args) :output :string :error-output nil :ignore-error-status t))

(defun vc-session (keys file)
  "Run KEYS on FILE once the version-control marks, worked out in the
background, are in; return the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 40 :height 8))
        (start (get-internal-real-time)))
    (labels ((wait ()
               (if (and (some (lambda (b) (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-refresh)) tuatara.buffer:*buffers*)
                        (< (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'wait))
                   (tuatara.frontend.fake:fake-queue-keys f keys))))
      (tuatara.frontend.fake:fake-queue-action f #'wait))
    (let ((editor (tuatara.command:run-editor f :files (list file) :setup #'tuatara.defaults:install-defaults)))
      (values f editor))))

(define-test (gutter vc-marks)
  (let ((dir (uiop:ensure-directory-pathname (merge-pathnames (format nil "tuatara-vc-~a/" (random 1000000)) (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (let ((file (merge-pathnames "data.txt" dir)))
           (git dir "init" "-q")
           (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
           (git dir "config" "commit.gpgsign" "false")
           (with-open-file (o file :direction :output) (format o "one~%two~%three~%four~%"))
           (git dir "add" "data.txt") (git dir "commit" "-q" "-m" "base")
           ;; on disk: line 1 changed, line 3 deleted (not adjacent to the change, or git merges
           ;; the two into one change hunk), a line added at the end; marks come from git
           (with-open-file (o file :direction :output :if-exists :supersede) (format o "onex~%two~%four~%new~%"))
           (multiple-value-bind (f editor) (vc-session "j" file)
             (is string= "~ onex" (tuatara.frontend.fake:fake-screen-line f 0))
             (is string= "  two" (tuatara.frontend.fake:fake-screen-line f 1))
             (is string= "- four" (tuatara.frontend.fake:fake-screen-line f 2))
             (is string= "+ new" (tuatara.frontend.fake:fake-screen-line f 3))
             (is equal '((0 1 :vc-changed)) (aref (tuatara.frontend.fake:fake-spans f) 0))
             (is equal '((0 1 :vc-deleted)) (aref (tuatara.frontend.fake:fake-spans f) 2))
             (is equal '((0 1 :vc-added)) (aref (tuatara.frontend.fake:fake-spans f) 3))
             (is equal '(1 . 2) (tuatara.frontend.fake:fake-cursor f))
             (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
           ;; before a save, an edited line is marked as changed at once
           (multiple-value-bind (f editor) (vc-session "j A y C-g" file)
             (is string= "~ twoy" (tuatara.frontend.fake:fake-screen-line f 1))
             (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
      (forget-committed-lines)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(define-test (gutter git-in-odd-directories)
  ;; git is run in the directory of a file whose directory's name holds [ ] * or \,
  ;; characters, not patterns
  (with-test-directory (dir)
    (dolist (name '("notes [1]*" "back\\slash"))
      (let* ((directory (concatenate 'string (tuatara.io:native dir) name "/"))
             (file (concatenate 'string directory "data.txt")))
        (sb-posix:mkdir directory #o700)
        (put-file (sb-ext:parse-native-namestring file) "data")
        (uiop:run-program (list "git" "-C" directory "init" "-q") :output nil :error-output nil :ignore-error-status t)
        (true (nth-value 1 (tuatara.defaults::run-git (tuatara.io:parent-path file) '("rev-parse" "--is-inside-work-tree")))
              "~a" name)))))

(define-test (gutter committed-text-bounded)
  ;; a committed text larger than the gutter compares is never read whole, however small the file is now
  (with-test-directory (dir)
    (let ((file (merge-pathnames "data.txt" dir)))
      (git dir "init" "-q")
      (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
      (git dir "config" "commit.gpgsign" "false")
      (put-file file (make-string 5000 :initial-element #\x))
      (git dir "add" "data.txt") (git dir "commit" "-q" "-m" "base")
      (put-file file "small")
      (let ((tuatara.defaults::*vc-gutter-max-bytes* 1000))
        (multiple-value-bind (octets too-large) (tuatara.defaults::committed-text file)
          (false octets)
          (true too-large)))
      (let ((tuatara.defaults::*vc-gutter-max-bytes* 10000))
        (is = 5000 (length (tuatara.defaults::committed-text file)))))))

(define-test (gutter committed-lines-read-once)
  ;; the committed text is read and its lines hashed once while HEAD holds the same text;
  ;; a commit changing it has it read again
  (with-test-directory (dir)
    (let ((file (merge-pathnames "data.txt" dir))
          (read 0))
      (git dir "init" "-q")
      (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
      (git dir "config" "commit.gpgsign" "false")
      (put-file file (format nil "one~%two~%"))
      (git dir "add" "data.txt") (git dir "commit" "-q" "-m" "base")
      (sb-int:encapsulate 'tuatara.defaults::run-git 'counted
                          (lambda (function directory arguments &rest keys)
                            (when (equal (first arguments) "cat-file") (incf read))
                            (apply function directory arguments keys)))
      (unwind-protect
           (let ((first (tuatara.defaults::committed-line-hashes file)))
             (is = 2 (length first))
             (is equalp first (tuatara.defaults::committed-line-hashes file))
             (is equalp first (tuatara.defaults::committed-line-hashes file))
             (is = 1 read)
             (put-file file (format nil "one~%two~%three~%"))
             (git dir "commit" "-q" "-a" "-m" "more")
             (is = 3 (length (tuatara.defaults::committed-line-hashes file)))
             (is = 2 read))
        (sb-int:unencapsulate 'tuatara.defaults::run-git 'counted)))))

(define-test (gutter test-directories-leave-no-committed-lines)
  ;; what a test commits is not found read already by the next one that commits the same text
  (forget-committed-lines)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "data.txt" dir)))
      (git dir "init" "-q")
      (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
      (git dir "config" "commit.gpgsign" "false")
      (put-file file (format nil "one~%two~%"))
      (git dir "add" "data.txt") (git dir "commit" "-q" "-m" "base")
      (is = 2 (length (tuatara.defaults::committed-line-hashes file)))
      (is = 1 (hash-table-count tuatara.defaults::**committed-lines**))))
  (is = 0 (hash-table-count tuatara.defaults::**committed-lines**)))

(define-test (gutter vc-marks-after-a-save)
  ;; a save has the marks worked out again: a line edited back to what git has is unmarked
  (with-test-directory (dir)
    (let ((file (merge-pathnames "data.txt" dir))
          (marks :unread))
      (git dir "init" "-q")
      (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
      (git dir "config" "commit.gpgsign" "false")
      (put-file file (format nil "one~%two~%"))
      (git dir "add" "data.txt") (git dir "commit" "-q" "-m" "base")
      (put-file file (format nil "onex~%two~%"))
      (flet ((visited () (tuatara.buffer:find-buffer "data.txt"))
             (settled () (let ((b (tuatara.buffer:find-buffer "data.txt")))
                           (and b (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-tracked)
                                (null (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-refresh))))))
        (let ((tuatara.defaults::*modal-editing* nil))
          (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))
                                   (list :until #'settled)
                                   (list :until (lambda () (tuatara.buffer:buffer-local (visited) 'tuatara.defaults::vc-marks)))
                                   "C-e DEL C-x C-s" (list :wait 0.1)
                                   (list :until #'settled)
                                   (list :until (lambda () (setf marks (tuatara.buffer:buffer-local (visited) 'tuatara.defaults::vc-marks))
                                                  (null marks))))))
        (is equal nil marks)))))

(define-test (gutter vc-marks-after-undo)
  ;; undo back to the text as it was read has the marks worked out again: only
  ;; the line that differs from git keeps one
  (with-test-directory (dir)
    (let ((file (merge-pathnames "data.txt" dir))
          (kinds :unread))
      (git dir "init" "-q")
      (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
      (git dir "config" "commit.gpgsign" "false")
      (put-file file (format nil "one~%two~%three~%"))
      (git dir "add" "data.txt") (git dir "commit" "-q" "-m" "base")
      (put-file file (format nil "onex~%two~%three~%"))
      (flet ((visited () (tuatara.buffer:find-buffer "data.txt"))
             (settled () (let ((b (tuatara.buffer:find-buffer "data.txt")))
                           (and b (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-tracked)
                                (null (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-refresh)))))
             (kinds () (loop for line below 3 collect (tuatara.defaults::vc-mark-at (tuatara.buffer:find-buffer "data.txt") line))))
        (let ((tuatara.defaults::*modal-editing* nil))
          (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed (namestring file)))
                                   (list :until #'settled)
                                   (list :until (lambda () (tuatara.buffer:buffer-local (visited) 'tuatara.defaults::vc-marks)))
                                   "C-n C-e y C-n C-e z" (list :wait 0.1)
                                   (list :until (lambda () (equal (kinds) '(:changed :changed :changed))))
                                   "C-/ C-/" (list :wait 0.1)
                                   (list :until #'settled)
                                   (list :until (lambda () (setf kinds (kinds)) (equal kinds '(:changed nil nil)))))))
        (is equal '(:changed nil nil) kinds)))))

(define-test (gutter vc-marks-many-edits)
  ;; marks from git's hunks follow the text, lines typed in widen them, and
  ;; each line an edit touches is marked once, however many marks there are
  (let ((b (tuatara.buffer:make-buffer "vc-many-edits" :text (format nil "~{line ~d~%~}" (loop for i below 20000 collect i)))))
    (flet ((kind (line) (tuatara.defaults::vc-mark-at b line))
           (type-at-lines (lines)
             (tuatara.buffer:buffer-edit b (loop for l in lines
                                                 for at = (tuatara.buffer:buffer-goto-line b l)
                                                 collect (tuatara.text:make-edit at at (tuatara.text:string-octets "x")))))
           (marks () (length (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-marks))))
      (unwind-protect
           (progn
             (tuatara.buffer::ensure-indexed b)
             (setf (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-tracked) t
                   (tuatara.buffer:buffer-local b 'tuatara.defaults::vc-marks)
                   (tuatara.defaults::hunk-marks b '((1 0 3 2) (5 1 10 0) (8 1 20 1))))
             (push #'tuatara.defaults::vc-note-edit (tuatara.buffer:buffer-change-functions b))
             (is equal '(nil nil :added :added nil) (mapcar #'kind '(0 1 2 3 4)))
             (is eq :deleted (kind 10))
             (is eq :changed (kind 19))
             ;; 5,000 cursors typing on every fourth line, twice: a mark for each line not
             ;; marked yet, none more the second time, in well under a second each
             (let ((start (get-internal-real-time)))
               (type-at-lines (loop for l from 0 below 20000 by 4 collect l))
               (is = 5003 (marks))
               (type-at-lines (loop for l from 0 below 20000 by 4 collect l))
               (is = 5003 (marks))
               (true (< (- (get-internal-real-time) start) internal-time-units-per-second)))
             (is equal '(:changed nil :added :added :changed nil) (mapcar #'kind '(0 1 2 3 4 5)))
             (is eq :deleted (kind 10))
             (is eq :changed (kind 19))
             (is eq :changed (kind 19996))
             ;; a line broken in two within an added one: both halves added, the line after too
             (let ((at (+ (tuatara.buffer:buffer-goto-line b 2) 3)))
               (tuatara.buffer:buffer-insert b at (string #\Newline)))
             (is equal '(nil :added :added :added :changed) (mapcar #'kind '(1 2 3 4 5)))
             (is = 5003 (marks))
             ;; edits on neighbouring lines in one batch are one mark
             (type-at-lines '(1 6 7))
             (is = 5005 (marks))
             (is equal '(:changed :added :changed :changed) (mapcar #'kind '(1 2 6 7))))
        (tuatara.buffer:kill-buffer b)))))
