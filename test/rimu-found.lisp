;;;; test/rimu-found.lisp --- Rimu lists of files found: by name, by text, and duplicates.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test rimu-found
  :description "M-x rimu-find lists the files under a directory whose names match a pattern,
rimu-find-text those whose text matches, rimu-duplicates the files the same as another in groups:
rimu lists whose names are paths, found in the background, the operations working on them, g r
finding them again, a file gone leaving the list.")

(defun searches-done ()
  "True when no rimu search runs or waits to be shown."
  (notany (lambda (job) (typep job 'tuatara.defaults::rimu-search)) tuatara.runtime:*jobs*))

(defun after-searches (keys)
  "An action for QUEUE-TEST-KEYS that waits for the rimu searches to be shown,
then has KEYS typed."
  (let ((start (get-internal-real-time)))
    (labels ((check ()
               (let ((f (tuatara.command:editor-frontend tuatara.command:*editor*)))
                 (if (and (not (searches-done)) (< (- (get-internal-real-time) start) (* 20 internal-time-units-per-second)))
                     (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                     ;; one tick more, for the job to be taken in
                     (progn (tuatara.frontend.fake:fake-queue-action f (lambda () nil))
                            (tuatara.frontend.fake:fake-queue-keys f keys))))))
      #'check)))

(defun found-session (command directory answer &optional (then "") (prefix ""))
  "Run M-x COMMAND on DIRECTORY with ANSWER to its second question (none when
NIL), wait for what it finds, then type THEN; PREFIX typed before M-x.
Returns the frontend and the editor."
  (rimu-forget)
  (let ((tuatara-user:*rimu-preview* nil))
    (run-keys (list (format nil "~@[~a ~]M-x ~a RET C-a C-k ~a C-j~@[ ~a RET~]"
                            (and (plusp (length prefix)) prefix) (rimu-typed command)
                            (rimu-typed (namestring directory)) (and answer (rimu-typed answer)))
                    (after-searches (if (plusp (length then)) then "C-l")))
              :width 120 :height 30)))

(defmacro with-found-tree ((directory) &body body)
  "Run BODY with DIRECTORY a new directory holding box/ with notes.txt
\(\"apple pie\"), copy.txt and deep/again.txt (\"same text\"), other.txt
\(\"same size\"), .hidden.txt and deep/report.TXT, and a trash of its own
\(XDG_DATA_HOME in it, outside box/), no drive's trash seen."
  (let ((old (gensym "OLD")))
    `(with-test-directory (,directory)
       (ensure-directories-exist (merge-pathnames "box/deep/" ,directory))
       (loop for (name text) in '(("box/notes.txt" "apple pie") ("box/copy.txt" "same text")
                                  ("box/deep/again.txt" "same text") ("box/other.txt" "same size")
                                  ("box/.hidden.txt" "same text") ("box/deep/report.TXT" "pear"))
             do (alexandria:write-string-into-file text (merge-pathnames name ,directory)))
       (let ((,old (sb-posix:getenv "XDG_DATA_HOME")))
         (sb-posix:setenv "XDG_DATA_HOME" (namestring (merge-pathnames "data/" ,directory)) 1)
         (unwind-protect (let ((tuatara.defaults::*mounts-file* (namestring (merge-pathnames "no-drives" ,directory))))
                           ,@body)
           (if ,old (sb-posix:setenv "XDG_DATA_HOME" ,old 1) (sb-posix:unsetenv "XDG_DATA_HOME")))))))

(defun line-with (lines text)
  "The first of LINES holding TEXT."
  (find-if (lambda (l) (search text l)) lines))

(define-test (rimu-found by-name)
  (with-found-tree (dir)
    (let ((box (merge-pathnames "box/" dir)))
      ;; any case, paths from the directory, hidden names left out
      (multiple-value-bind (f editor) (found-session "rimu-find" box "*.txt")
        (declare (ignore f))
        (let ((lines (rimu-lines editor)))
          (true (line-with lines "5 found: *.txt in"))
          (true (line-with lines "deep/again.txt"))
          (true (line-with lines "deep/report.TXT"))
          (true (line-with lines "notes.txt"))
          (false (line-with lines ".hidden.txt"))))
      ;; a capital letter asks for that case; directories match too
      (multiple-value-bind (f editor) (found-session "rimu-find" box "*.TXT")
        (declare (ignore f))
        (let ((lines (rimu-lines editor)))
          (true (line-with lines "deep/report.TXT"))
          (false (line-with lines "notes.txt"))))
      (multiple-value-bind (f editor) (found-session "rimu-find" box "de*")
        (declare (ignore f))
        (true (line-with (rimu-lines editor) "  deep")))
      ;; C-u: hidden names too
      (multiple-value-bind (f editor) (found-session "rimu-find" box "*.txt" "" "C-u")
        (declare (ignore f))
        (true (line-with (rimu-lines editor) ".hidden.txt"))))))

(define-test (rimu-found by-text)
  (with-found-tree (dir)
    (multiple-value-bind (f editor) (found-session "rimu-find-text" (merge-pathnames "box/" dir) "apple")
      (declare (ignore f))
      (let ((lines (rimu-lines editor)))
        (true (line-with lines "1 found: files holding apple"))
        (true (line-with lines "notes.txt"))
        (false (line-with lines "copy.txt"))))))

(define-test (rimu-found works-as-a-list)
  (with-found-tree (dir)
    (let ((box (merge-pathnames "box/" dir)))
      ;; RET opens a file found by its path
      (multiple-value-bind (f editor) (found-session "rimu-find" box "again*" "g f d e e p / a g a i n . t x t RET RET")
        (declare (ignore f))
        (is string= "same text" (session-text editor)))
      ;; D puts it in the trash, and the list keeps up
      (multiple-value-bind (f editor) (found-session "rimu-find" box "*.txt" "g f d e e p / a g a i n . t x t RET D y")
        (declare (ignore f))
        (loop repeat 200 while (find-if #'tuatara.defaults::rimu-operation-p tuatara.runtime:*jobs*) do (sleep 0.02))
        (false (probe-file (merge-pathnames "deep/again.txt" box)))
        (true (line-with (rimu-lines editor) "notes.txt")))
      ;; C-x C-q is for a directory's own list
      (multiple-value-bind (f editor) (found-session "rimu-find" box "*.txt" "C-x C-q")
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Names are edited in a directory's own list"))))))

(define-test (rimu-found shown-in-the-preview-window)
  ;; C-o with the preview on shows the file found in the preview's window: the list and that one window
  (with-found-tree (dir)
    (rimu-forget)
    (unwind-protect
         (let ((tuatara-user:*rimu-preview* t)
               (tuatara-user:*rimu-preview-delay* 0))
           (multiple-value-bind (f editor)
               (run-keys (list (format nil "M-x ~a RET C-a C-k ~a C-j ~a RET" (rimu-typed "rimu-find")
                                       (rimu-typed (namestring (merge-pathnames "box/" dir))) (rimu-typed "*.txt"))
                               (after-searches "g f n o t e s . t x t RET C-o"))
                         :width 120 :height 30)
             (declare (ignore f))
             (is = 2 (length (tuatara.command:editor-windows editor)))
             (is equal '("notes.txt") (loop for window in (tuatara.command:editor-windows editor)
                                            unless (eq window (tuatara.command:editor-window editor))
                                              collect (tuatara.buffer:buffer-name (tuatara.display:window-buffer window))))
             (true (tuatara.defaults::rimu-found-p (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
      (kill-buffers-under dir)
      (rimu-forget))))

(define-test (rimu-found same-files-grouped)
  (with-found-tree (dir)
    (let ((box (merge-pathnames "box/" dir)))
      ;; same size and same text: a group; same size alone is not enough
      (multiple-value-bind (f editor) (found-session "rimu-duplicates" box nil)
        (declare (ignore f))
        (let ((lines (rimu-lines editor)))
          (true (line-with lines "2 files the same as another, in 1 group; the copies take 9 B"))
          (true (line-with lines "copy.txt"))
          (true (line-with lines "deep/again.txt"))
          (false (line-with lines "other.txt"))
          (false (line-with lines "notes.txt"))))
      ;; C-u: the hidden one joins the group
      (multiple-value-bind (f editor) (found-session "rimu-duplicates" box nil "" "C-u")
        (declare (ignore f))
        (true (line-with (rimu-lines editor) "3 files the same as another")))
      ;; a group left with one file leaves the list
      (let* ((directory (namestring box))
             (entries (vector (tuatara.defaults::rimu-found-entry directory "copy.txt")
                              (tuatara.defaults::rimu-found-entry directory "deep/again.txt")))
             (listing (tuatara.defaults::make-rimu-listing directory entries nil nil "" (vector 1 1))))
        (delete-file (merge-pathnames "copy.txt" box))
        (is = 0 (length (tuatara.defaults::rimu-listing-entries (tuatara.defaults::rimu-found-again listing))))))))

(define-test (rimu-found from-marked-directories)
  (with-found-tree (dir)
    (let ((box (merge-pathnames "box/" dir)))
      (alexandria:write-string-into-file "same text" (merge-pathnames "box/deep/third.txt" dir))
      ;; with deep/ marked, only what is under it is compared
      (multiple-value-bind (f editor)
          (let ((tuatara-user:*rimu-preview* nil))
            (rimu-forget)
            (run-keys (list (format nil "C-x d C-a C-k ~a C-j g f d e e p RET m M-x ~a RET"
                                    (rimu-typed (namestring box)) (rimu-typed "rimu-duplicates"))
                            (after-searches "C-l"))
                      :width 120 :height 30))
        (declare (ignore f))
        (let ((lines (rimu-lines editor)))
          (true (line-with lines "2 files the same as another"))
          (true (line-with lines "deep/again.txt"))
          (true (line-with lines "deep/third.txt"))
          (false (line-with lines "copy.txt")))))))

(define-test (rimu-found given-files-in-order)
  (with-found-tree (dir)
    ;; given files of two directories, in groups and an order of their own (not by name), the
    ;; first group alone a line apart; each named by its path from the directory both are in
    (let ((box (namestring (merge-pathnames "box/" dir))))
      (rimu-forget)
      (multiple-value-bind (f editor)
          (run-keys (list (lambda ()
                            (tuatara.defaults::rimu-show-paths
                             (list (list (concatenate 'string box "other.txt"))
                                   (list (concatenate 'string box "notes.txt") (concatenate 'string box "deep/again.txt")))
                             :name "*given*" :words "Three files given" :safe t))
                          (after-searches "g r")
                          (after-searches "C-l"))
                    :width 120 :height 30)
        (let* ((lines (rimu-lines editor))
               (other (position-if (lambda (l) (search "other.txt" l)) lines))
               (notes (position-if (lambda (l) (search "notes.txt" l)) lines))
               (again (position-if (lambda (l) (search "deep/again.txt" l)) lines)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Three files given"))
          ;; read again (g r), the group of one is kept and the order with it
          (true (and other notes again (< other notes again)) "~{~a~%~}" lines)
          (is string= "" (string-trim " " (nth (1- notes) lines)))
          (true (tuatara.io:safe-visit-p (session-buffer editor)))
          (false (line-with lines "copy.txt")))))))
