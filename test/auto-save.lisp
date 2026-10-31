;;;; test/auto-save.lisp --- Changes not saved kept in a copy of their own, and taken back with M-x recover-file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test auto-save
  :description "Changes not saved are copied after a pause into a folder of their own, for you alone,
never into the file; a save deletes the copy, quitting without one keeps it, the next visit says
so and M-x recover-file takes it back as one edit; nothing opened safely is copied; a name too
long is kept beside its copy.")

(defun auto-save-session (steps &key (width 100) (height 20))
  "Run an editor session on STEPS, each a string of keys, (:wait SECONDS), or
\(:until FUNCTION) waiting (ten seconds at most) until FUNCTION of no
arguments is true. Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height)))
    (labels ((take (steps start)
               (let ((step (first steps)))
                 (cond ((null steps))
                       ((and (consp step) (eq (first step) :wait))
                        (if (> (- (get-internal-real-time) start) (* (second step) internal-time-units-per-second))
                            (take (rest steps) (get-internal-real-time))
                            (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f (lambda () (take steps start))))))
                       ((and (consp step) (eq (first step) :until))
                        (if (or (funcall (second step)) (> (- (get-internal-real-time) start) (* 10 internal-time-units-per-second)))
                            (take (rest steps) (get-internal-real-time))
                            (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f (lambda () (take steps start))))))
                       (t (queue-test-keys f (list step))
                          (tuatara.frontend.fake:fake-queue-action f (lambda () (take (rest steps) (get-internal-real-time)))))))))
      (tuatara.frontend.fake:fake-queue-action f (lambda () (take steps (get-internal-real-time))))
      (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults)))))

(defun copies-in (directory)
  "The names of the files in DIRECTORY."
  (mapcar #'file-namestring (uiop:directory-files directory)))

(define-test (auto-save copied-and-recovered)
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "notes.txt" dir)))
           (copies (namestring (merge-pathnames "copies/" dir)))
           (tuatara-user:*auto-save* t)
           (tuatara-user:*auto-save-directory* copies)
           (tuatara-user:*auto-save-idle* 0.2)
           (tuatara.defaults::*modal-editing* nil))
      (alexandria:write-string-into-file "first line" file)
      (flet ((copy () (first (uiop:directory-files copies))))
        ;; typed, then a pause: copied, the file as it was
        (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-> SPC k e a"
                                 (list :until (lambda () (copy)))))
        ;; killed with its changes not saved: the copy kept
        (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "notes.txt"))
        (true (copy))
        (when (copy)
          (is string= "first line kea" (alexandria:read-file-into-string (copy)))
          (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (copy))))))
          (is = #o700 (logand #o777 (sb-posix:stat-mode (sb-posix:stat copies))))
          (true (search "notes.txt" (file-namestring (copy)))))
        (is string= "first line" (alexandria:read-file-into-string file))
        ;; the editor started again says so; the list shows it, newer than its file
        ;; (counted in the background, said a moment after the start)
        (let ((f (auto-save-session (list (list :wait 0.3) (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string "list-auto-saves"))))))
          (true (tuatara.frontend.fake:fake-frame-showing f "1 file has changes auto-saved and not saved"))
          (true (tuatara.frontend.fake:fake-frame-showing f "newer than the file"))
          (let ((list (tuatara.buffer:find-buffer "*Auto-saves*")))
            (true (and list (search "notes.txt" (tuatara.buffer:buffer-string list))))
            (when list (tuatara.buffer:kill-buffer list))))
        ;; visited again: said; recovered as one edit, which undo takes back; a save deletes the copy
        (multiple-value-bind (f editor)
            (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                     (format nil "M-x ~{~a~^ ~} RET y" (map 'list #'string "recover-file"))))
          (declare (ignore editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "has changes auto-saved"))
          (true (tuatara.frontend.fake:fake-frame-showing f "notes.txt recovered"))
          (let ((buffer (tuatara.buffer:find-buffer "notes.txt")))
            (is string= "first line kea" (tuatara.buffer:buffer-string buffer))
            (true (tuatara.buffer:buffer-modified-p buffer))
            (tuatara.buffer:kill-buffer buffer)))
        (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file))
                                 (format nil "M-x ~{~a~^ ~} RET y" (map 'list #'string "recover-file"))
                                 "C-x C-s" (list :until (lambda () (null (copy))))))
        (is string= "first line kea" (alexandria:read-file-into-string file))
        (false (copy))
        (let ((buffer (tuatara.buffer:find-buffer "notes.txt")))
          (when buffer (tuatara.buffer:kill-buffer buffer)))
        ;; an open list follows the copies as they are written
        (auto-save-session (list (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string "list-auto-saves"))
                                 (format nil "C-x b ~a RET" (rimu-typed "*lisp-scratch*"))
                                 (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-> SPC o w l" (list :until (lambda () (copy)))
                                 (list :wait 0.2)))
        (let ((list (tuatara.buffer:find-buffer "*Auto-saves*")))
          (true (and list (search "newer than the file" (tuatara.buffer:buffer-string list))))
          (when list (tuatara.buffer:kill-buffer list)))
        (let ((buffer (tuatara.buffer:find-buffer "notes.txt")))
          (when buffer
            (setf (tuatara.buffer:buffer-saved-number buffer) (tuatara.buffer:buffer-content-number buffer))
            (tuatara.buffer:kill-buffer buffer)))
        ;; from the list: RET opens the file and takes its changes in, d marks a copy for deletion and x
        ;; deletes it, after asking; R is not a second RET
        (false (tuatara.command:lookup-key tuatara.defaults::*auto-saves-keymap* (tuatara.frontend:parse-key-sequence "R")))
        (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-> SPC b i r d" (list :until (lambda () (copy)))))
        (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "notes.txt"))
        (auto-save-session (list (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string "list-auto-saves")) "RET y"))
        (let ((buffer (tuatara.buffer:find-buffer "notes.txt")))
          (true buffer)
          (when buffer
            (is string= "first line kea bird" (tuatara.buffer:buffer-string buffer))
            (setf (tuatara.buffer:buffer-saved-number buffer) (tuatara.buffer:buffer-content-number buffer))
            (tuatara.buffer:kill-buffer buffer)))
        ;; a copy again, its buffer killed with the changes not saved
        (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-> SPC o w l" (list :until (lambda () (copy)))))
        (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "notes.txt"))
        (true (copy))
        (let ((editor (nth-value 1 (auto-save-session (list (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string "list-auto-saves"))
                                                            "d x y RET")))))
          (is string= "Deleted 1 auto-saved copy" (tuatara.command:editor-message editor)))
        (false (copy))
        (let ((list (tuatara.buffer:find-buffer "*Auto-saves*"))) (when list (tuatara.buffer:kill-buffer list)))))))

(define-test (auto-save following-the-file)
  ;; renamed, the copy follows the name; written as another file, saved, or its changes thrown away by a revert: gone
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "orig.txt" dir)))
           (copies (namestring (merge-pathnames "copies/" dir)))
           (tuatara-user:*auto-save* t)
           (tuatara-user:*auto-save-directory* copies)
           (tuatara-user:*auto-save-idle* 0.2)
           (tuatara.defaults::*modal-editing* nil))
      (alexandria:write-string-into-file "text" file)
      (flet ((copy-of (name) (find-if (lambda (copy) (search name copy)) (ignore-errors (copies-in copies))))
             (no-copies () (null (ignore-errors (copies-in copies))))
             (command (name) (format nil "M-x ~{~a~^ ~} RET" (map 'list #'string name))))
        (auto-save-session (list (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed file)) "M-> SPC a"
                                 (list :until (lambda () (copy-of "orig.txt")))
                                 (format nil "~a C-a C-k ~a RET" (command "rename-visited-file") (rimu-typed "new.txt"))
                                 (list :until (lambda () (and (copy-of "new.txt") (not (copy-of "orig.txt")))))))
        (true (copy-of "new.txt"))
        (false (copy-of "orig.txt"))
        (auto-save-session (list (format nil "C-x b ~a RET" (rimu-typed "new.txt")) "C-x C-s" (list :until #'no-copies)
                                 "SPC b" (list :until (lambda () (copy-of "new.txt")))
                                 (format nil "C-x C-w C-a C-k ~a RET" (rimu-typed (namestring (merge-pathnames "other.txt" dir))))
                                 (list :until #'no-copies)))
        (true (no-copies))
        (auto-save-session (list (format nil "C-x b ~a RET" (rimu-typed "other.txt")) "SPC c" (list :until (lambda () (copy-of "other.txt")))
                                 (format nil "~a y RET" (command "revert-buffer"))
                                 (list :until #'no-copies)))
        (true (no-copies))
        (kill-buffers-visiting dir)))))

(define-test (auto-save never-for-evidence)
  ;; a file opened safely, a read-only one, a buffer too large: never copied; a name too long kept beside its copy
  (with-test-directory (dir)
    (let* ((copies (namestring (merge-pathnames "copies/" dir)))
           (tuatara-user:*auto-save* t)
           (tuatara-user:*auto-save-directory* copies))
      (let ((buffer (tuatara.buffer:make-buffer "evidence" :text "x")))
        (setf (tuatara.buffer:buffer-file buffer) (namestring (merge-pathnames "evidence.txt" dir)))
        (true (tuatara.defaults::auto-save-wanted-p buffer))
        (setf (tuatara.buffer:buffer-local buffer :read-only) t)
        (false (tuatara.defaults::auto-save-wanted-p buffer))
        (setf (tuatara.buffer:buffer-local buffer :read-only) nil)
        (let ((tuatara-user:*auto-save-largest* 0))
          (false (tuatara.defaults::auto-save-wanted-p buffer)))
        (let ((tuatara-user:*auto-save* nil))
          (false (tuatara.defaults::auto-save-wanted-p buffer)))
        (tuatara.buffer:kill-buffer buffer))
      (let ((file (namestring (merge-pathnames "kept.txt" dir))))
        (alexandria:write-string-into-file "kept" file)
        (sb-posix:utimes file 946681200 946681200)
        (let ((tuatara.defaults::*modal-editing* nil))
          (auto-save-session (list (format nil "C-u C-x C-f C-a C-k ~a RET" (rimu-typed file)) "C-x C-q M-> SPC x" (list :wait 0.3))))
        (let ((buffer (tuatara.buffer:find-buffer "kept.txt"))) (when buffer (tuatara.buffer:kill-buffer buffer)))
        (false (and (probe-file copies) (uiop:directory-files copies)))
        (is = 946681200 (sb-posix:stat-atime (sb-posix:stat file))))
      ;; a name too long for one name: its start and a number, the name beside it
      (let* ((long (format nil "/tmp/~a/notes.txt" (make-string 300 :initial-element #\a)))
             (copy (tuatara.defaults::auto-save-path long)))
        (true (< (length (file-namestring copy)) 255))
        (true (uiop:string-prefix-p copies copy))))))

(define-test (auto-save startup-count-in-the-background)
  ;; the files with newer auto-saved changes are counted away from the editor's thread, so that
  ;; a drive not answering cannot hold the start up; the count is said once they are
  (let ((thread nil) (here sb-thread:*current-thread*)
        (tuatara.defaults::*auto-saves-counting* nil)
        (tuatara.command:*tick-functions* (list 'tuatara.defaults::auto-saves-startup-notice)))
    (sb-int:encapsulate 'tuatara.defaults::auto-save-copies 'where
                        (lambda (f) (setf thread sb-thread:*current-thread*) (funcall f)))
    (unwind-protect
         (progn
           (loop repeat 500
                 while (tuatara.defaults::auto-saves-startup-notice nil)
                 do (sleep 0.01))
           (true thread)
           (false (eq thread here)))
      (sb-int:unencapsulate 'tuatara.defaults::auto-save-copies 'where))
    (false (member 'tuatara.defaults::auto-saves-startup-notice tuatara.command:*tick-functions*))))
