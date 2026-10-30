;;;; lisp/editing/choosing.lisp --- While choosing: what completion shows beside files, and the previews.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

;;; What is shown beside files among completion candidates

(defvar *relative-age-limit* (* 14 24 60 60)
  "Seconds within which the last change of a file among completion candidates
is shown as an age (5 min ago, 3 days ago); older ones show the date, and with
0 every one does.")

(defun age-words (mtime &optional (now (get-universal-time)))
  "When a file last changed, MTIME (a STAMP-MTIME, in nanoseconds), in words
beside a completion candidate: an age within *RELATIVE-AGE-LIMIT* (42 s ago,
5 min ago, 3 h ago, 2 days ago), else the date, with the time when it is this
year: 16 Apr 19:23, 2 Apr 2025."
  (let* ((time (mtime-universal-time mtime))
         (age (- now time)))
    (cond ((and (<= 0 age) (< age *relative-age-limit*))
           (cond ((< age 60) (format nil "~d s ago" age))
                 ((< age 3600) (format nil "~d min ago" (floor age 60)))
                 ((< age 86400) (format nil "~d h ago" (floor age 3600)))
                 (t (format nil "~d day~:p ago" (floor age 86400)))))
          (t (format-time (if (= (day-date (time-day time)) (day-date (time-day now))) "%-d %b %R" "%-d %b %Y") time)))))

(defun mode-field (mode)
  "A file's MODE (STAMP-MODE) as ls writes it, drwxr-xr-x, as a field whose
letters have the :completion-mode- faces."
  (let ((string (make-string 10 :initial-element #\-)))
    (setf (char string 0) (case (logand mode #o170000)
                            (#o040000 #\d) (#o120000 #\l) (#o010000 #\p) (#o140000 #\s)
                            (#o020000 #\c) (#o060000 #\b) (t #\-)))
    (loop for bit in '(#o400 #o200 #o100 #o40 #o20 #o10 #o4 #o2 #o1)
          for letter across "rwxrwxrwx"
          for i from 1
          do (when (logtest mode bit) (setf (char string i) letter)))
    (when (logtest mode #o4000) (setf (char string 3) (if (logtest mode #o100) #\s #\S)))
    (when (logtest mode #o2000) (setf (char string 6) (if (logtest mode #o10) #\s #\S)))
    (when (logtest mode #o1000) (setf (char string 9) (if (logtest mode #o1) #\t #\T)))
    (list string
          (loop for letter across string
                for i from 0
                collect (list i (1+ i) (case letter
                                         (#\- :completion-mode-none) (#\d :completion-mode-directory)
                                         (#\l :completion-mode-link) (#\r :completion-mode-read)
                                         (#\w :completion-mode-write) (#\x :completion-mode-exec)
                                         (t :completion-mode-other)))))))

(defun mode-octal-text (mode)
  "A file's MODE (STAMP-MODE) as its kind's letter, as ls writes it, then its
permissions in octal, as chmod takes them: d755, -644; with four digits when
it has the set-user, set-group or sticky bit, -4755."
  (let ((bits (logand mode #o7777)))
    (format nil "~c~:[~3,'0o~;~4,'0o~]" (char (first (mode-field mode)) 0) (> bits #o777) bits)))

(defun stamp-fields (stamp &optional (now (get-universal-time)))
  "What is shown beside a file whose STAMP is known: its permissions, its size
(for a regular file; what it is, for a named pipe, a device or a socket) and
when it last changed."
  (let ((kind (tuatara.io:stamp-kind stamp)))
    (list (mode-field (tuatara.io:stamp-mode stamp))
          (list (case kind
                  (:regular (octets-words (tuatara.io:stamp-size stamp)))
                  (:directory "")
                  (t (tuatara.io:kind-words kind)))
                :list-size -8)
          (list (age-words (tuatara.io:stamp-mtime stamp) now) :list-date -12))))

;;; Previews while choosing

(defvar *preview-files* nil
  "True to show, while you choose a file with \\[find-file] (and the other ways
of finding a file) or \\[find-recent-file], the file selected in the window you are in,
opened safely: read-only, no mode or hook run on it, its times untouched, a
binary one in the hex view; what the window showed comes back when the
question ends. NIL, as standard, to show nothing until a file is chosen.")

(defvar *preview-formatted-files* nil
  "True to show, while you choose one of your files with formatted names with
\\[open-file-formatted], the file selected in the window you are in, opened
safely, as *PREVIEW-FILES* shows files. NIL, as standard, for none.")

(defvar *preview-formatted-search* nil
  "True to show, while you search your files with formatted names with
\\[search-in-formatted], the file of the line selected in the window you are in, at
that line, opened safely, as *PREVIEW-FILES* shows files. NIL, as standard,
for none.")

(defvar *preview-file-search* nil
  "True to show, while you search the files of a project or a directory with
\\[search-in-files], the file of the line selected in the window you are in, at
that line, opened safely, as *PREVIEW-FILES* shows files. NIL, as standard,
for none.")

(defvar *preview-buffers* nil
  "True to show, while you choose a buffer with \\[switch-to-buffer] (and
\\[switch-to-buffer-other-window], and every other question for a buffer), the
buffer selected in the window you are in, what it showed coming back when the
question ends. NIL, as standard, for none.")

(defun preview-file-buffer (path made)
  "The buffer to preview the file at PATH in: the one visiting it already, or
one made by visiting it safely, then given to MADE, a function, to be killed
once the preview is over; a binary file in the hex view, as visiting shows
it (SHOW-BINARY-IN-HEX). NIL for a directory or a file that is not there."
  (let ((kind (tuatara.io:path-kind path)))
    (when (and kind (not (eq kind :directory)))
      (multiple-value-bind (buffer existing) (tuatara.io:visit-file path :safe t)
        (unless existing
          (funcall made buffer)
          (show-binary-in-hex buffer))
        buffer))))

(defun call-with-window-preview (show function)
  "Call FUNCTION with a preview function for COMPLETING-READ: called with the
candidate selected (NIL for none, when what is shown stays), it shows in the
selected window the buffer SHOW returns for it, or the window's own when SHOW
returns NIL; when SHOW
returns a position as a second value, there, in the middle of the window.
SHOW is called with the candidate and a function to give the buffers made
only for the preview. However FUNCTION ends, the window shows what it showed
before, as it showed it, buffers already open that were shown elsewhere have
point back where it was, and the buffers made for the preview are killed,
where point was in their files not remembered. A place outside the part
shown of a narrowed buffer is shown widened, as choosing it shows it, and
narrowed again after."
  (let* ((window (editor-window *editor*))
         (original (tuatara.display:window-buffer window))
         (point (tuatara.display:window-point window))
         (start (tuatara.display:window-start window))
         (made '())
         ;; buffers already open whose point a preview moved: each (BUFFER . POINT)
         (moved '())
         ;; narrowed buffers widened to show a place outside: each (BUFFER . NARROWING)
         (widened '()))
    (unwind-protect
         (funcall function
                  (lambda (candidate)
                    (multiple-value-bind (buffer position)
                        (and candidate (ignore-errors (funcall show candidate (lambda (b) (pushnew b made)))))
                      ;; nothing selected for a moment (the list being worked out again):
                      ;; what is shown stays, rather than the window's own buffer and back
                      (let ((buffer (if candidate (or buffer original) (tuatara.display:window-buffer window))))
                        (unless (eq buffer (tuatara.display:window-buffer window))
                          (tuatara.display:set-window-buffer window buffer :remember nil)
                          (when (eq buffer original)
                            (setf (tuatara.display:window-point window) point
                                  (tuatara.display:window-start window) start)))
                        (when position
                          (unless (or (eq buffer original) (member buffer made) (assoc buffer moved))
                            (push (cons buffer (tuatara.buffer:buffer-point buffer)) moved))
                          (let ((at (min position (tuatara.buffer:buffer-size buffer))))
                            (when (/= at (tuatara.buffer:within-narrowing buffer at))
                              (push (cons buffer (tuatara.buffer:buffer-narrowing buffer)) widened)
                              (tuatara.buffer:widen-buffer buffer))
                            (setf (tuatara.display:window-point window) at))
                          (tuatara.display:window-recenter window))))))
      (loop for (buffer . narrowing) in widened
            when (and (live-buffer-p buffer) (null (tuatara.buffer:buffer-narrowing buffer)))
              do (setf (tuatara.buffer:buffer-narrowing buffer) narrowing)
                 (tuatara.buffer:keep-within-narrowing buffer))
      (when (member window (editor-windows *editor*))
        (unless (eq original (tuatara.display:window-buffer window))
          (tuatara.display:set-window-buffer window original :remember nil))
        (setf (tuatara.display:window-point window) point
              (tuatara.display:window-start window) start))
      (loop for (buffer . was) in moved
            when (live-buffer-p buffer)
              do (setf (tuatara.buffer:buffer-point buffer) (min was (tuatara.buffer:buffer-size buffer))))
      (let ((tuatara.buffer:*kill-buffer-functions* (remove 'save-place-of tuatara.buffer:*kill-buffer-functions*)))
        (dolist (buffer made)
          (when (live-buffer-p buffer) (tuatara.buffer:kill-buffer buffer)))))))
