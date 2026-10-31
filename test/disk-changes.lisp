;;;; test/disk-changes.lisp --- Files changed on disk: noticed at once, followed, read again in place.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test files-changed-on-disk
  :description "The watcher hears of changes through inotify, not by looking every few seconds, and not of the editor's own saves; a file that only grew has just the new part added (tail -f); a file read again keeps point on the same text; with *reload-changed-files* or M-x follow-file buffers keep up by themselves; many files changed at once make one message.")

(defun wait-until (predicate &optional (seconds 3))
  "Call PREDICATE every hundredth of a second until it answers true, at most
SECONDS; its answer, or NIL."
  (loop repeat (round (* seconds 100))
        for answer = (funcall predicate)
        when answer return answer
        do (sleep 0.01)))

(defun append-on-disk (path text)
  "Append TEXT (a string, or octets) to the file at PATH, as a program writing a log does."
  (let ((fd (sb-posix:open (namestring path) (logior sb-posix:o-wronly sb-posix:o-append))))
    (unwind-protect (tuatara.io:write-octets fd (if (stringp text) (to-octets text) (coerce text 'tuatara.text:octets)))
      (sb-posix:close fd))))

(defmacro with-slow-polling (&body body)
  "Run BODY with the watcher polling only every thousand seconds, so that what
it notices it was told of by inotify; the watcher is told of no file afterwards."
  `(let ((old tuatara.io:*poll-interval*))
     (setf tuatara.io:*poll-interval* 1000)
     (unwind-protect (progn ,@body)
       (setf tuatara.io:*poll-interval* old)
       (tuatara.io:watch-files '()))))

(defun wait-for-looks (&optional (seconds 3))
  "Take in the watcher's news until some comes, at most SECONDS: the buffers
whose state changed, and those whose file changed again."
  (let ((changed nil) (again nil))
    (wait-until (lambda () (multiple-value-setq (changed again) (tuatara.io:take-file-looks)) (or changed again))
                seconds)
    (values changed again)))

(define-test (files-changed-on-disk told-by-inotify)
  (with-slow-polling
    (with-test-directory (dir)
      (let ((path (put-file (merge-pathnames "watched.csv" dir) (format nil "a,b~%1,2~%"))))
        (with-visited (b path)
          (tuatara.io:watch-files (list b))
          (sleep 0.2)                           ; the watch begins; the first look finds nothing new
          (false (tuatara.io:take-file-looks))
          (let ((start (get-internal-real-time)))
            (replace-by-rename path (format nil "a,b~%3,4~%"))
            (is equal (list b) (wait-for-looks))
            (true (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second (machine-slowdown)))))
          (is eq :changed (tuatara.io:file-disk-state b))
          ;; changed again: reported as such, the state staying
          (rewrite-in-place path (format nil "a,b~%5,6~%"))
          (multiple-value-bind (changed again) (wait-for-looks)
            (false changed)
            (is equal (list b) again))
          ;; deleted
          (delete-file path)
          (is equal (list b) (wait-for-looks))
          (is eq :deleted (tuatara.io:file-disk-state b)))))))

(define-test (files-changed-on-disk own-saves-are-quiet)
  (with-slow-polling
    (with-test-directory (dir)
      (let ((path (put-file (merge-pathnames "mine.txt" dir) "one")))
        (with-visited (b path)
          (tuatara.io:watch-files (list b))
          (sleep 0.1)
          (tuatara.buffer:buffer-insert b 3 " two")
          (tuatara.io:save-buffer-file b)
          (tuatara.io:watch-files (list b))
          (false (wait-for-looks 1/2))
          (is eq nil (tuatara.io:file-disk-state b))
          ;; a change after the save is still heard of
          (replace-by-rename path "theirs")
          (is equal (list b) (wait-for-looks)))))))

(define-test (files-changed-on-disk large-growth-copied-once)
  ;; a file grown by much more than the tail kept is followed, its new tail
  ;; taken from what it grew by without copying all of that again
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "big.log" dir) (format nil "start~%")))
          (grown (format nil "~{line ~d~%~}" (loop for i below 200000 collect i))))
      (with-visited (b path)
        (append-on-disk path grown)
        (let* ((before (sb-ext:get-bytes-consed))
               (added (tuatara.io:append-from-disk b))
               (made (- (sb-ext:get-bytes-consed) before)))
          (is = (length grown) added)
          (true (< made (* 5/2 added)) "~:d octets made for ~:d added" made added))
        (is string= (subseq grown (- (length grown) tuatara.io::+tail-sample+))
            (tuatara.text:octets-string (tuatara.buffer:buffer-local b :file-tail)))
        (append-on-disk path (format nil "end~%"))
        (is = 4 (tuatara.io:append-from-disk b))
        (is string= (format nil "start~%~aend~%" grown) (tuatara.buffer:buffer-string b))))))

(define-test (files-changed-on-disk appended)
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "app.log" dir) (format nil "one~%two~%"))))
      (with-visited (b path)
        (setf (tuatara.buffer:buffer-point b) (tuatara.buffer:buffer-size b))
        (append-on-disk path (format nil "three~%"))
        (is = 6 (tuatara.io:append-from-disk b))
        (is string= (format nil "one~%two~%three~%") (tuatara.buffer:buffer-string b))
        (is = (tuatara.buffer:buffer-size b) (tuatara.buffer:buffer-point b)) ; point follows the end
        (false (tuatara.buffer:buffer-modified-p b))
        (is eq nil (tuatara.io:check-file b))
        ;; nothing new: nothing done
        (false (tuatara.io:append-from-disk b))
        ;; appends one after another undo together
        (append-on-disk path (format nil "four~%"))
        (tuatara.io:append-from-disk b)
        (tuatara.buffer:buffer-undo b)
        (is string= (format nil "one~%two~%") (tuatara.buffer:buffer-string b))
        (true (tuatara.buffer:buffer-modified-p b))
        (tuatara.buffer:buffer-redo b)
        (false (tuatara.buffer:buffer-modified-p b))
        ;; changes of the buffer's own stay, the new part added after them
        (tuatara.buffer:buffer-insert b 0 "# ")
        (setf (tuatara.buffer:buffer-point b) 0)
        (append-on-disk path (format nil "five~%"))
        (is = 5 (tuatara.io:append-from-disk b))
        (is string= (format nil "# one~%two~%three~%four~%five~%") (tuatara.buffer:buffer-string b))
        (is = 0 (tuatara.buffer:buffer-point b))
        (true (tuatara.buffer:buffer-modified-p b))
        ;; saved, then grown: the save's end is what is recognised
        (tuatara.io:save-buffer-file b)
        (append-on-disk path (format nil "six~%"))
        (is = 4 (tuatara.io:append-from-disk b))
        ;; not grown but rewritten: not an append
        (rewrite-in-place path (format nil "# one~%two~%three~%FOUR~%five~%six~%seven~%"))
        (false (tuatara.io:append-from-disk b))
        ;; a character cut in two by the writer comes whole once the rest is there
        (tuatara.io:revert-buffer-file b)
        (append-on-disk path #(#xc3))
        (is = 1 (tuatara.io:append-from-disk b))
        (is = 1 (tuatara.text:rope-invalid (tuatara.buffer:buffer-text b)))
        (append-on-disk path #(#xa9))
        (is = 1 (tuatara.io:append-from-disk b))
        (is = 0 (tuatara.text:rope-invalid (tuatara.buffer:buffer-text b)))
        (true (alexandria:ends-with (code-char #xe9) (tuatara.buffer:buffer-string b)))))))

(define-test (files-changed-on-disk text-mapping)
  ;; positions go to the same line and column of the new text
  (flet ((mapped (old new position)
           (let ((a (tuatara.text:string->rope old)) (b (tuatara.text:string->rope new)))
             (funcall (tuatara.defaults::text-mapping a b) position :before))))
    (let ((old (format nil "l1~%l2~%l3~%l4~%")))
      (is = 10 (mapped old (format nil "l0~%l1~%l2~%l3~%l4~%") 7))      ; a line added above
      (is = 4 (mapped old (format nil "l2~%l3~%l4~%") 7))               ; a line removed above
      (is = 7 (mapped old (format nil "l1~%l2~%l3~%l4~%l5~%") 7))       ; below: no move
      (is = 7 (mapped old (format nil "l1~%XX~%l3~%l4~%") 7))           ; a line changed above, same length
      (is = 7 (mapped old (format nil "l1~%l2~%L~%l4~%") 8))            ; its own line shorter: cut
      (is = 3 (mapped (format nil "ab~%") (format nil "abc") 3)))))     ; past the end

(defun visited-session-buffer ()
  "The current buffer of the running session."
  (tuatara.command:current-buffer))

(tuatara.command:define-command test-prepend-line-on-disk ()
  "Put a line at the start of the current buffer's file on disk, as another program would, for the tests."
  (let ((path (tuatara.buffer:buffer-file (visited-session-buffer))))
    (replace-by-rename path (concatenate 'string (format nil "l0~%") (file-text path)))))

(tuatara.command:define-command test-change-and-catch-up ()
  "Put a line at the start of the current buffer's file on disk and take in the watcher's news, for the tests."
  (let ((b (visited-session-buffer)))
    (tuatara.defaults::files-tick tuatara.command:*editor*)
    (sleep 0.1)
    (let ((path (tuatara.buffer:buffer-file b)) (size (tuatara.buffer:buffer-size b)))
      (replace-by-rename path (concatenate 'string (format nil "l0~%") (file-text path)))
      (wait-until (lambda () (tuatara.defaults::files-tick tuatara.command:*editor*)
                    (/= size (tuatara.buffer:buffer-size b)))))))

(tuatara.command:define-command test-change-and-wait ()
  "Put a line at the start of the current buffer's file on disk and wait for the watcher's news of it, for the tests."
  (let ((b (visited-session-buffer)))
    (tuatara.defaults::files-tick tuatara.command:*editor*)
    (sleep 0.1)
    (let ((path (tuatara.buffer:buffer-file b)))
      (replace-by-rename path (concatenate 'string (format nil "l0~%") (file-text path)))
      (wait-until (lambda () (tuatara.defaults::files-tick tuatara.command:*editor*)
                    (tuatara.io:file-disk-state b))))))

(tuatara.command:define-command test-append-and-catch-up ()
  "Append a line to the current buffer's file on disk and take in the watcher's news, for the tests."
  (let ((b (visited-session-buffer)))
    (tuatara.defaults::files-tick tuatara.command:*editor*)
    (sleep 0.1)
    (let ((size (tuatara.buffer:buffer-size b)))
      (append-on-disk (tuatara.buffer:buffer-file b) (format nil "more~%"))
      (wait-until (lambda () (tuatara.defaults::files-tick tuatara.command:*editor*)
                    (/= size (tuatara.buffer:buffer-size b)))))))

(tuatara.command:define-command test-change-several-on-disk ()
  "Visit the other files in the current buffer's directory, change them all on disk and take in the watcher's news, for the tests."
  (let* ((b (visited-session-buffer))
         (directory (uiop:pathname-directory-pathname (tuatara.buffer:buffer-file b)))
         (others (mapcar #'tuatara.io:visit-file (remove (tuatara.buffer:buffer-file b) (directory (merge-pathnames "*.txt" directory))
                                                         :test #'equal)))
         (all (cons b others)))
    (tuatara.defaults::files-tick tuatara.command:*editor*)
    (sleep 0.2)
    (tuatara.defaults::files-tick tuatara.command:*editor*)
    (dolist (buffer all) (replace-by-rename (tuatara.buffer:buffer-file buffer) "theirs"))
    (wait-until (lambda () (tuatara.defaults::files-tick tuatara.command:*editor*)
                  (every #'tuatara.io:file-disk-state all)))))

(define-test (files-changed-on-disk reading-again-in-place)
  (with-slow-polling
    (with-test-directory (dir)
      (let ((path (put-file (merge-pathnames "lines.txt" dir) (format nil "l1~%l2~%l3~%l4~%"))))
        ;; M-x revert-buffer keeps point on the same text
        (multiple-value-bind (f editor)
            (run-keys "C-n C-n C-f M-x t e s t - p r e p e n d - l i n e - o n - d i s k RET M-x r e v e r t - b u f f e r RET"
                      :file path :width 100)
          (is string= (format nil "l0~%l1~%l2~%l3~%l4~%") (session-text editor))
          (is = 10 (session-point editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "Read lines.txt again"))
          (tuatara.buffer:kill-buffer (session-buffer editor)))
        ;; with the option, by itself, and undo takes it back
        (put-file path (format nil "l1~%l2~%l3~%l4~%"))
        (let ((tuatara-user:*reload-changed-files* t))
          (multiple-value-bind (f editor)
              (run-keys "C-n C-n C-f M-x t e s t - c h a n g e - a n d - c a t c h - u p RET" :file path :width 100)
            (is string= (format nil "l0~%l1~%l2~%l3~%l4~%") (session-text editor))
            (is = 10 (session-point editor))
            (true (tuatara.frontend.fake:fake-frame-showing f "Read lines.txt again, changed on disk (undo takes it back)"))
            (tuatara.buffer:buffer-undo (session-buffer editor))
            (is string= (format nil "l1~%l2~%l3~%l4~%") (session-text editor))
            (tuatara.buffer:kill-buffer (session-buffer editor))))))))

(define-test (files-changed-on-disk every-window-keeps-its-place)
  ;; read again, each window showing the buffer stays on the line it showed first
  (with-test-directory (dir)
    (let ((path (put-file (merge-pathnames "lines.txt" dir) (format nil "l1~%l2~%l3~%l4~%"))))
      (multiple-value-bind (f editor) (run-keys "" :file path :width 100 :height 12)
        (declare (ignore f))
        (let* ((tuatara.command:*editor* editor)
               (buffer (session-buffer editor))
               (windows (progn (tuatara.command:split-window (tuatara.command:editor-window editor) :below)
                               (tuatara.command:buffer-windows buffer))))
          (is = 2 (length windows))
          (setf (tuatara.display:window-start (first windows)) 3
                (tuatara.display:window-start (second windows)) 6)
          (put-file path (format nil "l0~%l1~%l2~%l3~%l4~%"))
          (tuatara.defaults::reload-buffer buffer)
          (is = 6 (tuatara.display:window-start (first windows)))
          (is = 9 (tuatara.display:window-start (second windows)))
          ;; a text too large to compare by lines: each place kept as it was, at its line's start
          (put-file path (format nil "l0-l1-l2-l3~%l4~%"))
          (let ((tuatara.defaults::*reload-diff-limit* 0))
            (tuatara.defaults::reload-buffer buffer))
          (is = 0 (tuatara.display:window-start (first windows)))
          (is = 0 (tuatara.display:window-start (second windows)))
          (tuatara.buffer:kill-buffer buffer))))))

(define-test (files-changed-on-disk kept-with-changes-of-its-own)
  ;; a buffer that follows its file but has changes of its own is not read again,
  ;; and the message says why and what to do
  (with-slow-polling
    (with-test-directory (dir)
      (let ((path (put-file (merge-pathnames "lines.txt" dir) (format nil "l1~%l2~%")))
            (tuatara-user:*reload-changed-files* t))
        (multiple-value-bind (f editor)
            (run-keys "i X C-g M-x t e s t - c h a n g e - a n d - w a i t RET" :file path :width 220)
          (true (tuatara.frontend.fake:fake-frame-showing
                 f "lines.txt changed on disk but is not read again, having changes of its own"))
          (true (tuatara.frontend.fake:fake-frame-showing f "M-x revert-buffer takes the disk's"))
          (is string= (format nil "Xl1~%l2~%") (session-text editor))
          (tuatara.buffer:kill-buffer (session-buffer editor)))))))

(define-test (files-changed-on-disk following)
  (with-slow-polling
    (with-test-directory (dir)
      (let ((path (put-file (merge-pathnames "run.log" dir) (format nil "start~%"))))
        ;; M-x follow-file: the growth comes in, point at the end follows it
        (multiple-value-bind (f editor)
            (run-keys "M-x f o l l o w - f i l e RET M-> M-x t e s t - a p p e n d - a n d - c a t c h - u p RET"
                      :file path :width 100)
          (true (tuatara.frontend.fake:fake-frame-showing f "Following run.log"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Following"))
          (is string= (format nil "start~%more~%") (session-text editor))
          (is = (length (format nil "start~%more~%")) (session-point editor))
          (false (tuatara.buffer:buffer-modified-p (session-buffer editor)))
          (tuatara.buffer:kill-buffer (session-buffer editor)))
        ;; with changes of the buffer's own, the growth still comes in at the end
        (let ((tuatara-user:*reload-changed-files* t))
          (multiple-value-bind (f editor)
              (run-keys "i # SPC C-g M-x t e s t - a p p e n d - a n d - c a t c h - u p RET" :file path :width 100)
            (declare (ignore f))
            (is string= (format nil "# start~%more~%more~%") (session-text editor))
            (true (tuatara.buffer:buffer-modified-p (session-buffer editor)))
            (tuatara.buffer:kill-buffer (session-buffer editor))))
        ;; without either, nothing comes in by itself: the change is only said
        (multiple-value-bind (f editor)
            (run-keys "M-x t e s t - c h a n g e - a n d - w a i t RET" :file path :width 100)
          (is string= (format nil "start~%more~%more~%") (session-text editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "run.log changed on disk; M-x revert-buffer reads it again"))
          (tuatara.buffer:kill-buffer (session-buffer editor)))))))

(define-test (files-changed-on-disk many-at-once)
  (with-slow-polling
    (with-test-directory (dir)
      (let ((paths (loop for name in '("a.txt" "b.txt" "c.txt")
                         collect (put-file (merge-pathnames name dir) (format nil "~a~%" name)))))
        (multiple-value-bind (f editor)
            (run-keys "M-x t e s t - c h a n g e - s e v e r a l - o n - d i s k RET" :file (first paths) :width 160)
          (declare (ignore editor))
          ;; one message for all three
          (true (tuatara.frontend.fake:fake-frame-showing f "a.txt" "b.txt" "c.txt" "changed on disk; M-x revert-buffer reads each again")))
        (dolist (name '("a.txt" "b.txt" "c.txt"))
          (let ((b (tuatara.buffer:find-buffer name))) (when b (tuatara.buffer:kill-buffer b))))))))

(defun thread-cpu-seconds (thread)
  "The processor time THREAD has used, in seconds, as Linux counts it."
  (with-open-file (in (format nil "/proc/self/task/~d/stat" (sb-thread::thread-os-tid thread)) :if-does-not-exist nil)
    (when in
      (let* ((line (read-line in))
             (fields (uiop:split-string (string-trim " " (subseq line (1+ (position #\) line :from-end t))))
                                        :separator " ")))
        (/ (+ (parse-integer (nth 11 fields)) (parse-integer (nth 12 fields))) 100)))))

(define-test (files-changed-on-disk watcher-idle)
  ;; with nothing due, the watcher waits for news rather than looking again at once
  (is eq nil (tuatara.io::soonest-due '()))
  (is eq nil (tuatara.io::soonest-due (list (tuatara.io::make-watch-item nil "a" nil '()))))
  (let ((later (tuatara.io::make-watch-item nil "b" nil '())) (sooner (tuatara.io::make-watch-item nil "c" nil '())))
    (setf (tuatara.io::watch-item-due later) 200 (tuatara.io::watch-item-due sooner) 100)
    (is = 100 (tuatara.io::soonest-due (list later (tuatara.io::make-watch-item nil "a" nil '()) sooner))))
  ;; watching a file, it sleeps: well under a tenth of the time spent working
  (let ((path (format nil "~aidle-~d.txt" (namestring (uiop:temporary-directory)) (random 1000000))))
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string "quiet" o))
    (let ((buffer (tuatara.io:visit-file path)))
      (unwind-protect
           (progn
             (tuatara.io:watch-files (list buffer))
             (sleep 0.3)
             (let* ((watcher tuatara.io::*watcher*)
                    (before (thread-cpu-seconds watcher)))
               (sleep 1)
               (true (< (- (thread-cpu-seconds watcher) before) 1/10))))
        (tuatara.io:watch-files '())
        (tuatara.buffer:kill-buffer buffer)
        (ignore-errors (delete-file path))))))

(defun directory-news-within (seconds)
  "The directories the watcher reports changed within SECONDS, all of them."
  (let ((news '()) (end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop while (< (get-internal-real-time) end)
          do (setf news (union news (tuatara.io:take-directory-news) :test #'string=))
             (sleep 0.05))
    news))

(define-test (files-changed-on-disk directories-watched)
  (with-test-directory (dir)
    (let* ((real (format nil "~areal/" (namestring dir)))
           (link (format nil "~alink/" (namestring dir)))
           (gone (format nil "~agone/" (namestring dir)))
           (missing (format nil "~amissing/" (namestring dir)))
           (interval tuatara.io::*poll-interval*))
      (ensure-directories-exist real)
      (ensure-directories-exist gone)
      (sb-posix:symlink "real" (string-right-trim "/" link))
      (unwind-protect
           (progn
             (setf tuatara.io::*poll-interval* 1/4)
             ;; one directory listed by two names: a change told for both
             (tuatara.io:watch-directories (list real link gone missing))
             (directory-news-within 0.5)
             (alexandria:write-string-into-file "x" (format nil "~anew.txt" real))
             (let ((news (directory-news-within 1.5)))
               (true (member real news :test #'string=))
               (true (member link news :test #'string=)))
             ;; one name no longer watched: the other still is
             (tuatara.io:watch-directories (list link gone missing))
             (directory-news-within 0.5)
             (alexandria:write-string-into-file "y" (format nil "~aother.txt" real))
             (true (member link (directory-news-within 1.5) :test #'string=))
             ;; a directory taken away: told
             (sb-posix:rmdir (string-right-trim "/" gone))
             (true (member gone (directory-news-within 1.5) :test #'string=))
             ;; one not there, looked at again and again: not told again and again
             (false (member missing (directory-news-within 1.5) :test #'string=))
             ;; made again: told
             (ensure-directories-exist missing)
             (true (member missing (directory-news-within 1.5) :test #'string=)))
        (setf tuatara.io::*poll-interval* interval)
        (tuatara.io:watch-directories '())))))
