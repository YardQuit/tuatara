;;;; test/picking.lisp --- One way of picking things: directories, buffers, defaults, order, progress.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test picking
  :description "Every question for a directory asks as C-x C-f does (any part of a name matches, // and ~/
start again, RET goes into a directory and takes the line typed); a question's default and kept
order; every question for a buffer offers the same and previews the same; a list that fills in
the background says how far it has got in the same words everywhere.")

(defun shown-candidates ()
  "The candidates the question open shows, as their strings."
  (map 'list #'tuatara.command:candidate-string
       (tuatara.command::completion-candidates (tuatara.command:editor-completion tuatara.command:*editor*))))

(defmacro with-test-home ((home) &body body)
  "Run BODY with HOME bound to a home folder of the test's own, in a new
temporary directory, holding the folders Documents/ and notes/ and the file
todo.org: ~ leads there meanwhile, never to your own home, and HOME is as it
was afterwards (unset when it was)."
  (let ((old (gensym "OLD")) (dir (gensym "DIR")))
    `(with-test-directory (,dir)
       (let ((,home (merge-pathnames "home/" ,dir))
             (,old (uiop:getenv "HOME")))
         (ensure-directories-exist (merge-pathnames "Documents/" ,home))
         (ensure-directories-exist (merge-pathnames "notes/" ,home))
         (put-file (merge-pathnames "todo.org" ,home) "* one")
         (unwind-protect
              (progn (sb-posix:setenv "HOME" (string-right-trim "/" (namestring ,home)) 1)
                     ;; the folder ~ named a moment ago is looked up again
                     (setf tuatara.io::*default-directory-found* nil)
                     ,@body)
           (if ,old (sb-posix:setenv "HOME" ,old 1) (sb-posix:unsetenv "HOME"))
           (setf tuatara.io::*default-directory-found* nil))))))

(define-test (picking directory-in-search-in-files)
  (with-test-directory (dir)
    (let ((root (tuatara.io:native dir)))
      (ensure-directories-exist (merge-pathnames "alpha-project/" dir))
      (ensure-directories-exist (merge-pathnames "beta/" dir))
      (put-file (merge-pathnames "top.txt" dir) (format nil "top~%"))
      (flet ((searched-in (keys)
               (let ((prompt nil))
                 (run-keys (list (format nil "C-x C-f ~a RET C-u M-s g ~a" (key-string (concatenate 'string root "top.txt")) keys)
                                 :wait
                                 (lambda () (setf prompt (tuatara.command:editor-minibuffer-prompt tuatara.command:*editor*))))
                           :width 250 :height 20)
                 prompt)))
        ;; any part of a name: apj is alpha-project/; RET goes into it, RET then takes the line typed
        (is string= (format nil "Regex search files in ~aalpha-project/: " (tuatara.defaults::abbreviated-path root))
            (searched-in "a p j RET RET"))
        ;; a path typed after the one offered starts again from //
        (is string= (format nil "Regex search files in ~abeta/: " (tuatara.defaults::abbreviated-path root))
            (searched-in (format nil "~a RET" (key-string (concatenate 'string "alpha-project/" root "beta/")))))
        ;; the line typed, a directory, taken at once
        (is string= (format nil "Regex search files in ~a: " (tuatara.defaults::abbreviated-path root))
            (searched-in "RET"))
        ;; files are not offered, only directories
        (let ((offered nil))
          (run-keys (list (format nil "C-x C-f ~a RET C-u M-s g" (key-string (concatenate 'string root "top.txt")))
                          (lambda () (setf offered (shown-candidates))))
                    :width 250 :height 20)
          (is equal '("beta/" "alpha-project/") offered))))))

(define-test (picking directory-in-rimu-copy)
  (with-operations-tree (dir)
    (let ((there (namestring (merge-pathnames "there-place/" dir)))
          (root (namestring dir)))
      (ensure-directories-exist there)
      ;; any part of a name: thpl is there-place/; RET goes into it, RET takes it
      (let ((f (ops-session dir (format nil "g f b . t x t RET C C-u ~a t h p l RET RET" (rimu-typed root)))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied 1 file to"))
        (is string= "beta" (alexandria:read-file-into-string (here there "b.txt"))))
      ;; a whole path typed after the one offered: from its // on
      (let ((f (ops-session dir (format nil "g f a . t x t RET C C-u ~a RET" (rimu-typed (concatenate 'string root "sub/" root "there-place/"))))))
        (true (tuatara.frontend.fake:fake-frame-showing f "Copied 1 file to"))
        (is string= "alpha" (alexandria:read-file-into-string (here there "a.txt"))))
      ;; ~/ starts again from the home directory: its directories offered
      (with-test-home (home)
        (let ((offered :none))
          (ops-session dir (list (format nil "g f a . t x t RET C C-u ~a" (rimu-typed (concatenate 'string root "sub/~/")))
                                 (lambda () (setf offered (sort (shown-candidates) #'string<)))
                                 "C-g"))
          (is equal '("Documents/" "notes/") offered))))))

(defvar *picked* nil
  "What the test commands of picking read.")

(tuatara.command:define-command test-pick-default ()
  "Read among alpha, beta and gamma, gamma the default."
  (setf *picked* (tuatara.command:completing-read "Pick: " '("alpha" "beta" "gamma") :default "gamma")))

(tuatara.command:define-command test-pick-default-elsewhere ()
  "Read among alpha and beta, delta, none of them, the default."
  (setf *picked* (tuatara.command:completing-read "Pick: " '("alpha" "beta") :default "delta" :require-match t)))

(tuatara.command:define-command test-pick-in-order ()
  "Read among zeta, alpha and beta, kept in that order."
  (setf *picked* (tuatara.command:completing-read "Pick: " '("zeta" "alpha" "beta") :keep-order t)))

(tuatara.command:define-command test-pick-sorted ()
  "Read among zeta, alpha and beta, sorted best first."
  (setf *picked* (tuatara.command:completing-read "Pick: " '("zeta" "alpha" "beta"))))

(define-test (picking default-and-kept-order)
  ;; the default: named in the prompt, offered first, taken by RET alone
  (let ((*picked* nil) (offered nil))
    (let ((f (run-keys (list "M-x t e s t - p i c k - d e f a u l t RET" (lambda () (setf offered (shown-candidates))) "RET")
                       :width 80 :height 12)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Pick (default gamma): "))
      (is equal "gamma" (first offered))
      (is = 3 (length offered))
      (is equal "gamma" *picked*)))
  ;; typed: what matches, not the default
  (let ((*picked* nil))
    (run-keys "M-x t e s t - p i c k - d e f a u l t RET a l RET" :width 80 :height 12)
    (is equal "alpha" *picked*))
  ;; moved away from it: the one moved to
  (let ((*picked* nil) (offered nil))
    (run-keys (list "M-x t e s t - p i c k - d e f a u l t RET" (lambda () (setf offered (shown-candidates))) "C-n RET")
              :width 80 :height 12)
    (is equal (second offered) *picked*))
  ;; a default that is none of the candidates: RET alone takes it all the same
  (let ((*picked* nil))
    (run-keys "M-x t e s t - p i c k - d e f a u l t - e l s e w h e r e RET RET" :width 80 :height 12)
    (is equal "delta" *picked*))
  ;; kept in order: those matching as given, not the best match first
  (let ((*picked* nil) (offered nil))
    (run-keys (list "M-x t e s t - p i c k - i n - o r d e r RET a" (lambda () (setf offered (shown-candidates))) "RET")
              :width 80 :height 12)
    (is equal '("zeta" "alpha" "beta") offered)
    (is equal "zeta" *picked*))
  (let ((offered nil))
    (run-keys (list "M-x t e s t - p i c k - s o r t e d RET a" (lambda () (setf offered (shown-candidates))))
              :width 80 :height 12)
    (is equal "alpha" (first offered)))
  ;; C-h f in a Lisp buffer: the symbol at point the default, named in the prompt
  (multiple-value-bind (f editor) (run-keys-in-turns '("C-h f RET") :text "mapcar #'1+ list" :mode 'tuatara.defaults::lisp-mode)
    (true (tuatara.frontend.fake:fake-frame-showing f "Describe function (default mapcar): "))
    (true (search "mapcar is a function" (help-text editor)))))

(define-test (picking buffers-alike)
  (let ((other (tuatara.buffer:make-buffer "picking-other" :text "SHOWN-BY-PREVIEW")))
    (unwind-protect
         (progn
           ;; C-x 4 b previews as C-x b does, in the window asked from
           (let ((tuatara-user:*preview-buffers* t))
             (true (tuatara.frontend.fake:fake-frame-showing
                    (run-keys "C-x b p i c k i n g - o t h" :width 80 :height 12) "SHOWN-BY-PREVIEW"))
             (true (tuatara.frontend.fake:fake-frame-showing
                    (run-keys "C-x 4 b p i c k i n g - o t h" :width 80 :height 12) "SHOWN-BY-PREVIEW"))
             ;; taken: shown in another window, the one asked from as it was
             (multiple-value-bind (f editor) (run-keys "C-x 4 b p i c k i n g - o t h RET" :width 80 :height 12)
               (declare (ignore f))
               (let ((windows (tuatara.command:editor-windows editor)))
                 (is = 2 (length windows))
                 (is string= "picking-other" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (true (find "*scratch*" windows :key (lambda (w) (tuatara.buffer:buffer-name (tuatara.display:window-buffer w)))
                                                 :test #'string=)))))
           ;; previews off: nothing shown until chosen
           (let ((tuatara-user:*preview-buffers* nil))
             (false (tuatara.frontend.fake:fake-frame-showing
                     (run-keys "C-x 4 b p i c k i n g - o t h" :width 80 :height 12) "SHOWN-BY-PREVIEW")))
           ;; the same candidates, with the same fields beside them, in C-x b, C-x 4 b and *ielm*'s C-c C-b
           (flet ((offered (keys)
                    (let ((seen nil))
                      (run-keys (list keys (lambda ()
                                             (setf seen (map 'list (lambda (c) (list (tuatara.command:candidate-string c)
                                                                                     (tuatara.command:candidate-annotation c)))
                                                             (tuatara.command::completion-candidates
                                                              (tuatara.command:editor-completion tuatara.command:*editor*))))))
                                :width 80 :height 12)
                      seen)))
             (let ((switching (offered "C-x b"))
                   (other-window (offered "C-x 4 b")))
               (true (find "picking-other" switching :key #'first :test #'string=))
               (is equal (mapcar #'first switching) (mapcar #'first other-window))
               (is equal (find "picking-other" switching :key #'first :test #'string=)
                   (find "picking-other" other-window :key #'first :test #'string=))
               (let ((ielm (offered "M-x i e l m RET C-c C-b")))
                 (true (find "picking-other" ielm :key #'first :test #'string=))
                 (true (find "*scratch*" ielm :key #'first :test #'string=))
                 ;; the buffer used last first, as in C-x b
                 (is string= "*scratch*" (first (first ielm)))))))
      (tuatara.buffer:kill-buffer other)
      (let ((ielm (tuatara.buffer:find-buffer "*ielm*"))) (when ielm (tuatara.buffer:kill-buffer ielm))))))

(defvar *held-gathering* nil
  "The gathering the progress test's questions take from: it gives two things, then waits to be stopped.")

(defun held-gathering (things)
  "A gathering that gives THINGS, then waits until it is stopped."
  (tuatara.defaults::start-gathering "tuatara test gathering"
                                     (lambda (give cancel)
                                       (dolist (thing things) (funcall give thing))
                                       (loop until (car cancel) do (sleep 0.01)))))

(defun wait-for-held-gathering ()
  "Wait, on the editor's thread, until the held gathering has given all it gives."
  (loop repeat 500
        until (and *held-gathering* (= 2 (tuatara.defaults::gathering-count *held-gathering*)))
        do (sleep 0.01)))

(tuatara.command:define-command test-pick-subdirectory ()
  "Ask for a subdirectory of the notes, the subdirectories coming from the held gathering."
  (setf *held-gathering* (held-gathering (list "~/one/" "~/two/")))
  (tuatara.defaults::read-formatted-subdirectory *held-gathering*))

(tuatara.command:define-command test-pick-keywords ()
  "Ask for keywords, those of the notes coming from the held gathering."
  (setf *held-gathering* (held-gathering (list '("alpha") '("beta"))))
  (tuatara.defaults::read-keywords "New file KEYWORDS: " (list nil *held-gathering*)))

(define-test (picking progress-alike)
  ;; two kinds of question filling in the background say how far they have got alike
  (let ((tuatara-user:*formatted-file-keywords* '("garden")))
    (let ((*held-gathering* nil))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys (list "M-x t e s t - p i c k - s u b d i r e c t o r y RET" #'wait-for-held-gathering :tick "C-g")
                       :width 120 :height 12)
             "Select SUBDIRECTORY (listing, 3 so far): ")))
    (let ((*held-gathering* nil))
      (true (tuatara.frontend.fake:fake-frame-showing
             (run-keys (list "M-x t e s t - p i c k - k e y w o r d s RET" #'wait-for-held-gathering :tick "C-g")
                       :width 120 :height 12)
             "New file KEYWORDS (listing, 3 so far): "))))
  ;; once all have come and there are none: the words given for that
  (with-formatted-directory (dir)
    (true (tuatara.frontend.fake:fake-frame-showing
           (run-keys (list "SPC d f" :wait) :width 200 :height 12)
           "Open file (none in"))))
