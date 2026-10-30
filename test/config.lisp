;;;; test/config.lisp --- The init file and bind-key.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test config
  :description "The init file is loaded in TUATARA-USER; bind-key binds in the global, NORMAL and leader maps; an error in it becomes a message.")

(define-test (config bind-key)
  (let ((file (merge-pathnames (format nil "tuatara-init-~a.lisp" (random 1000000)) (uiop:temporary-directory)))
        ;; what the leader's ; is before "; ;" makes it a prefix, put back after
        (semicolon (tuatara.command:lookup-key tuatara.defaults::*tuatara-leader-keymap* (tuatara.frontend:parse-key-sequence ";"))))
    (unwind-protect
         (progn
           (with-open-file (o file :direction :output)
             (format o "(bind-key \"C-c t\" 'end-of-buffer)~%(bind-key \"SPC .\" 'beginning-of-buffer :map :normal)~%(bind-key \"; ;\" 'end-of-line :map :leader)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "C-c t" :text "abc")
               (declare (ignore f))
               (is = 3 (session-point editor)))
             (multiple-value-bind (f editor) (run-keys "g e SPC ." :text "abc")
               (declare (ignore f))
               (is = 0 (session-point editor)))
             (multiple-value-bind (f editor) (run-keys "SPC ; ;" :text "abc")
               (declare (ignore f))
               (is = 3 (session-point editor))))
           ;; faces can be changed from the init file
           (with-open-file (o file :direction :output :if-exists :supersede)
             (format o "(set-face :comment :fg :cyan :italic nil)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (run-keys "" :text "")
             (is eq :cyan (tuatara.display:face-attribute :comment :fg))
             (false (tuatara.display:face-attribute :comment :italic)))
           ;; set-face is your own setting, kept across themes: clear it for the other tests
           (setf tuatara.display:*user-face-settings* '())
           (tuatara.display:apply-theme nil)
           ;; special buffers' maps are reachable by name
           (with-open-file (o file :direction :output :if-exists :supersede)
             (format o "(bind-key \"z\" 'quit-window :map :buffers)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (fresh-list-session "C-x C-b z")
               (declare (ignore f))
               (is string= "*scratch*" (current-name editor))))
           ;; a broken init file reports and the editor still runs
           (with-open-file (o file :direction :output :if-exists :supersede)
             (format o "(bind-key \"C-c u\" 'no-such-command)~%"))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "" :text "abc")
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "Error in" "no-such-command")))))
      (ignore-errors (delete-file file))
      ;; leave no binding behind for the other tests
      (tuatara-user:unbind-key "C-c t")
      (tuatara-user:unbind-key "SPC ." :map :normal)
      (tuatara-user:unbind-key "; ;" :map :leader)
      (tuatara.command:define-key tuatara.defaults::*tuatara-leader-keymap* ";" semicolon)
      (tuatara-user:unbind-key "z" :map :buffers))))

(define-test (config key-labels)
  ;; the leader's prefixes are labelled; bind-key labels a key, either way it
  ;; takes a label, and a prefix; the key hints show the labels
  (flet ((hints (keys) (run-keys keys :text "abc" :width 140 :height 20)))
    ;; the standard prefixes are named
    (let ((f (let ((tuatara-user:*key-hint-rows* 30)) (run-keys "C-x" :text "abc" :width 140 :height 40))))
      (true (tuatara.frontend.fake:fake-frame-showing f "r +rectangles"))
      (true (tuatara.frontend.fake:fake-frame-showing f "m +bookmarks"))
      (true (tuatara.frontend.fake:fake-frame-showing f "4 +other window"))
      (true (tuatara.frontend.fake:fake-frame-showing f "n +notes")))
    (let ((f (hints "SPC")))
      (true (tuatara.frontend.fake:fake-frame-showing f "t +toggle"))
      (true (tuatara.frontend.fake:fake-frame-showing f "w +window"))
      (true (tuatara.frontend.fake:fake-frame-showing f "i +input")))
    (let ((file (merge-pathnames (format nil "tuatara-init-~a.lisp" (random 1000000)) (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (o file :direction :output)
               (format o "(bind-key \"t z\" 'end-of-buffer :map :leader :label \"to the end\")~%~
                          (bind-key \"t y\" '(\"to the top\" . beginning-of-buffer) :map :leader)~%~
                          (bind-key \"t\" nil :map :leader :label \"toggles\")~%"))
             (let ((tuatara.defaults:*config-file* file))
               (true (tuatara.frontend.fake:fake-frame-showing (hints "SPC") "t +toggles"))
               (let ((f (hints "SPC t")))
                 (true (tuatara.frontend.fake:fake-frame-showing f "z to the end"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "y to the top"))
                 (true (tuatara.frontend.fake:fake-frame-showing f "h hl-line-mode")))
               ;; a labelled key still runs its command
               (is = 3 (session-point (nth-value 1 (run-keys "SPC t z" :text "abc"))))))
        (ignore-errors (delete-file file))
        (tuatara-user:unbind-key "t z" :map :leader)
        (tuatara-user:unbind-key "t y" :map :leader)
        (tuatara-user:bind-key "t" nil :map :leader :label "toggle")))
    ;; unbound, a key keeps no label
    (let ((leader tuatara.defaults::*tuatara-leader-keymap*))
      (false (tuatara.command:key-label (tuatara.command:lookup-key leader (tuatara.frontend:parse-key-sequence "t"))
                                        (first (tuatara.frontend:parse-key-sequence "z")))))
    ;; a key bound to a command is no prefix to label under
    (fail (tuatara-user:bind-key "b x" nil :map :leader :label "no"))))

(define-test (config keys-for-modes)
  (flet ((point-after (keys) (session-point (nth-value 1 (run-keys keys :text "abc"))))
         (hints (keys) (run-keys keys :text "abc" :width 140 :height 20)))
    (unwind-protect
         (progn
           ;; SPC o z everywhere, another in Org and Markdown, SPC o y in text modes and those made from them
           (tuatara-user:bind-key "o z" 'end-of-buffer :map :leader)
           (tuatara-user:bind-key "o z" 'beginning-of-buffer :map :leader :mode '(org-mode markdown-mode))
           (tuatara-user:bind-key "o y" 'end-of-line :map :leader :mode 'text-mode)
           (tuatara-user:bind-key "o" nil :map :leader :label "own")
           (is = 3 (point-after "SPC o z"))
           (is = 0 (point-after "M-x o r g - m o d e RET g e SPC o z"))
           (is = 0 (point-after "M-x m a r k d o w n - m o d e RET g e SPC o z"))
           (is = 3 (point-after "M-x o r g - m o d e RET SPC o y"))
           (is = 0 (point-after "SPC o y"))
           ;; the key hints: the mode's keys over the others, the prefix's label as everywhere
           (true (tuatara.frontend.fake:fake-frame-showing (hints "M-x o r g - m o d e RET SPC") "o +own"))
           (let ((f (hints "M-x o r g - m o d e RET SPC o")))
             (true (tuatara.frontend.fake:fake-frame-showing f "z beginning-of-buffer"))
             (true (tuatara.frontend.fake:fake-frame-showing f "y end-of-line")))
           (true (tuatara.frontend.fake:fake-frame-showing (hints "SPC o") "z end-of-buffer"))
           ;; a mode's keys in a state; a map that is no state's nor the leader's refused with :mode
           (tuatara-user:bind-key "C-c z" 'beginning-of-buffer :map :normal :mode 'org-mode)
           (is = 0 (point-after "M-x o r g - m o d e RET g e C-c z"))
           (fail (tuatara-user:bind-key "x" 'end-of-line :map :buffers :mode 'org-mode))
           (fail (tuatara-user:bind-key "x" 'end-of-line :map :leader :mode 'no-such-mode))
           ;; unbound for the mode, the key everywhere is the key there too
           (tuatara-user:unbind-key "o z" :map :leader :mode '(org-mode markdown-mode))
           (is = 3 (point-after "M-x o r g - m o d e RET SPC o z")))
      (tuatara-user:unbind-key "o" :map :leader)
      (tuatara-user:unbind-key "o" :map :leader :mode '(org-mode markdown-mode text-mode))
      (tuatara-user:unbind-key "C-c z" :map :normal :mode 'org-mode))))

(define-test (config leader-on-another-key)
  (unwind-protect
       (let ((tuatara-user:*leader-key* ","))
         (tuatara-user:bind-key "o z" 'end-of-buffer :map :leader)
         (tuatara-user:bind-key "o y" 'end-of-line :map :leader :mode 'org-mode)
         ;; the leader's keys after , in NORMAL state and in a mode's own; SPC no longer opens them
         (is = 3 (session-point (nth-value 1 (run-keys ", o z" :text "abc"))))
         (is = 3 (session-point (nth-value 1 (run-keys "M-x o r g - m o d e RET , o y" :text "abc"))))
         (true (tuatara.frontend.fake:fake-frame-showing (run-keys "," :text "abc" :width 140 :height 20) "o +"))
         (false (eq tuatara.defaults::*tuatara-leader-keymap*
                    (tuatara.command:lookup-key tuatara.defaults::*tuatara-normal-keymap* (tuatara.frontend:parse-key-sequence "SPC")))))
    (tuatara-user:unbind-key "o" :map :leader)
    (tuatara-user:unbind-key "o" :map :leader :mode 'org-mode)
    (tuatara.defaults::put-leader-on "SPC")
    (true (eq tuatara.defaults::*tuatara-leader-keymap*
              (tuatara.command:lookup-key tuatara.defaults::*tuatara-normal-keymap* (tuatara.frontend:parse-key-sequence "SPC"))))))

(define-test (config options-reach-the-editor)
  ;; the options documented for the init file are, read in
  ;; TUATARA-USER, the very variables the editor uses
  (loop for (name used) in '(("*MESSAGE-LOG-MAX*" tuatara.command:*message-log-max*)
                             ("*SORT-FOLD-CASE*" tuatara.defaults::*sort-fold-case*)
                             ("*RELOAD-CHANGED-FILES*" tuatara.defaults::*reload-changed-files*)
                             ("*LINE-NUMBERS-DEFAULT*" tuatara.defaults::*line-numbers-default*)
                             ("*INDENT-NEW-LINES*" tuatara.defaults::*indent-new-lines*)
                             ("*STARTUP-BUFFER*" tuatara-user:*startup-buffer*)
                             ("*DARK-THEME*" tuatara-user:*dark-theme*)
                             ("*LIGHT-THEME*" tuatara-user:*light-theme*))
        do (multiple-value-bind (symbol status) (find-symbol name :tuatara-user)
             (is eq used symbol)
             (is eq :external status)
             (true (boundp symbol))
             (true (documentation symbol 'variable)))))

(define-test (config starter-names-every-setting)
  ;; every setting the starter init file sets is one the init file reaches, an
  ;; external of TUATARA-USER, and every such setting is named there (a package's
  ;; own, in the example of one, aside)
  (let ((starter tuatara.defaults::*starter-config*) (set '()))
    (loop with start = 0
          for at = (search "(setf *" starter :start2 start)
          while at
          do (let ((end (position-if (lambda (c) (member c '(#\Space #\Newline #\)))) starter :start (+ at 6))))
               (pushnew (subseq starter (+ at 6) end) set :test #'string=)
               (setf start end)))
    (dolist (name set)
      (unless (search "*example-" name)
        (multiple-value-bind (symbol status) (find-symbol (string-upcase name) :tuatara-user)
          (declare (ignore symbol))
          (is eq :external status "~a, set in the starter init file" name))))
    (do-external-symbols (symbol :tuatara-user)
      (when (and (boundp symbol) (eq :special (sb-int:info :variable :kind symbol)))
        (true (search (string-downcase (symbol-name symbol)) starter) "~(~a~) in the starter init file" symbol)))))

(define-test (config list-settings-read-as-meant)
  (flet ((read-as (value &optional pairs)
           (tuatara.defaults::setting-list (gensym "SETTING") value :pairs pairs)))
    ;; a list as it is, a dotted tail one more element, a value alone a list of it
    (let ((list '(a b)))
      (is eq list (read-as list)))
    (is equal '() (read-as nil))
    (is equal '(a b c) (read-as '(a b . c)))
    (is equal '("~/Evidence/") (read-as "~/Evidence/"))
    (is equal '(org-mode) (read-as 'org-mode))
    ;; settings of pairs: one pair alone, a dotted tail that is no pair left out
    (is equal '((#\( . #\))) (read-as '(#\( . #\)) t))
    (is equal '((org-mode)) (read-as '(org-mode) t))
    (is equal '((#\( . #\)) (#\[ . #\])) (read-as '((#\( . #\)) (#\[ . #\]) . #\x) t))
    (is equal '((a . t)) (read-as '((a . t) b) t)))
  ;; said once, the same reading kept
  (let* ((value '(a . b))
         (first-read (tuatara.defaults::setting-list 'some-setting value)))
    (is eq first-read (tuatara.defaults::setting-list 'some-setting value)))
  (remhash 'tuatara-user:*pair-delimiters* tuatara.defaults::*settings-read-as*)
  (true (tuatara.frontend.fake:fake-frame-showing
         (let ((tuatara-user:*electric-pair-mode* t) (tuatara-user:*pair-delimiters* '(#\{ . #\})))
           (run-keys "i { C-g" :text "" :width 140))
         "*pair-delimiters* is (#\\{ . #\\}), not a list of pairs: read as ((#\\{ . #\\}))")))

(define-test (config starting-past-a-slip)
  ;; an init file setting the pairs as one dotted pair: the editor starts, and pairs
  (let* ((file (merge-pathnames (format nil "init-~36r.lisp" (random (expt 36 8))) (tuatara.io:cache-directory)))
         (pairs tuatara-user:*pair-delimiters*)
         (mode tuatara-user:*electric-pair-mode*))
    (unwind-protect
         (progn
           (with-open-file (out (ensure-directories-exist file) :direction :output :if-exists :supersede)
             (write-string "(setf *pair-delimiters* '(#\\( . #\\)))
(setf *electric-pair-mode* t)
" out))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "i ( a C-g" :text "" :width 140)
               (declare (ignore f))
               (is eq :read tuatara.defaults::*init-file-outcome*)
               (is string= "(a)" (session-text editor)))))
      (setf tuatara-user:*pair-delimiters* pairs
            tuatara-user:*electric-pair-mode* mode)
      (ignore-errors (delete-file file))))
  ;; a step of starting that fails is said, and the rest carries on
  (let ((said nil))
    (false (tuatara.defaults::after-init-step "Binding something" (lambda () (error "it broke"))))
    (setf said (tuatara.defaults::after-init-step "Something else" (lambda () :done)))
    (is eq :done said)))

;; the commented settings of an init file, each read as it would be without its ;;
(defun commented-forms (text)
  "The forms of TEXT, an init file, that are commented out with ;; in front of
each of their lines, read; a form over several lines read whole. A form
commented inside another one commented (;;   ;; (setf ...) in a load-package's
:config) is read too, by itself, its lines being those of its own depth."
  (let ((forms '()) (pending (make-hash-table :test 'equal)))
    (dolist (line (uiop:split-string text :separator '(#\Newline)) (nreverse forms))
      (let* ((run (or (position-if-not (lambda (c) (member c '(#\Space #\;))) line) (length line)))
             (last (position #\; line :end run :from-end t)))
        (when (and last (plusp last) (char= #\; (char line (1- last))))
          (let* ((prefix (subseq line 0 (1+ last)))
                 (content (subseq line (1+ last)))
                 (so-far (gethash prefix pending)))
            (when (or so-far (uiop:string-prefix-p "(" (string-left-trim " " content)))
              (let* ((text (if so-far (format nil "~a~%~a" so-far content) content))
                     (form (handler-case (read-from-string text) (end-of-file () :more))))
                (if (eq form :more)
                    (setf (gethash prefix pending) text)
                    (progn (push form forms) (remhash prefix pending)))))))))))

(define-test (config starter-init-file)
  (let* ((home (ensure-directories-exist
                (merge-pathnames (format nil "config-~36r/" (random (expt 36 8))) (tuatara.io:cache-directory))))
         (file (merge-pathnames "tuatara/init.lisp" home))
         (old (uiop:getenv "XDG_CONFIG_HOME"))
         ;; loading the starter sets tabs for some modes: put back afterwards
         (tabs (alexandria:copy-hash-table tuatara.defaults::*mode-indent-tabs*)))
    (unwind-protect
         (progn
           ;; the first start without an init file writes one there and says so
           (sb-posix:setenv "XDG_CONFIG_HOME" (namestring home) 1)
           (let ((tuatara.defaults:*config-file* nil))
             (multiple-value-bind (f editor) (run-keys "" :text "abc" :width 140)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "Wrote a starter init file" "tuatara/init.lisp"))))
           ;; and reads it at once: Makefiles have their tabs from the first start
           (true (gethash 'tuatara.defaults::makefile-mode tuatara.defaults::*mode-indent-tabs*))
           (let ((text (uiop:read-file-string file)))
             (true (search "(setf *reload-changed-files* t)" text))
             (true (search "(bind-key \"C-c t\" 'toggle-light-dark-theme)" text))
             (true (search "(set-colouring 'rust :sparse)" text))
             ;; every line of it is a comment but the tabs some languages' own
             ;; tools require, a line each, and the load-package forms holding the
             ;; settings, by what they are for
             (true (every (lambda (line)
                            (let ((line (string-left-trim " " line)))
                              (or (zerop (length line)) (char= (char line 0) #\;)
                                  (string= line ":config") (string= line ")")
                                  (cl-ppcre:scan "^\\(load-package \"[a-z-]+\"$" line)
                                  (and (string= "(set-indent-tabs-mode '" line :end2 (min (length line) 23))
                                       (search " t)" line)))))
                          (uiop:split-string text :separator '(#\Newline))))
             (true (search "(set-indent-tabs-mode 'makefile t)" text))
             (true (search "(set-indent-tabs-mode 'go t)" text)))
           ;; each setting in it works once its ;; is taken away, from every line of it
           (let ((*package* (find-package :tuatara-user)))
             (dolist (form (commented-forms (uiop:read-file-string file)))
               (when (and (consp form) (eq (first form) 'setf))
                 (true (every #'boundp (loop for (place) on (rest form) by #'cddr collect place))))))
           ;; never written again, never written over
           (with-open-file (o file :direction :output :if-exists :supersede) (write-string ";; mine" o))
           (false (tuatara.defaults::write-starter-config file))
           (let ((tuatara.defaults:*config-file* nil))
             (run-keys "" :text "abc"))
           (is string= ";; mine" (uiop:read-file-string file)))
      (if old (sb-posix:setenv "XDG_CONFIG_HOME" old 1) (sb-posix:unsetenv "XDG_CONFIG_HOME"))
      (setf tuatara.defaults::*mode-indent-tabs* tabs)
      (uiop:delete-directory-tree home :validate (lambda (p) (search "/tuatara/config-" (namestring p)))))))

(define-test (config bind-key-a-mode-either-way)
  ;; :map 'csv-mode and :map :global :mode 'csv are the same keys: the mode's own
  (is equal (tuatara.defaults::mode-keymaps 'csv-mode nil) (tuatara.defaults::mode-keymaps :global 'csv))
  (is eq (tuatara.defaults::mode-keymap 'tuatara.defaults::csv-mode) (first (tuatara.defaults::mode-keymaps 'csv-mode nil))))

(define-test (config init-file-read-past-an-error)
  ;; a form that fails is said with its line, and the forms after it are read all the same
  (let ((file (merge-pathnames (format nil "init-~36r.lisp" (random (expt 36 8))) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (alexandria:write-string-into-file
            (format nil "(setf (symbol-value 'tuatara-user::*init-test-mark*) :before)~%;; a comment~%~%(load-package \"calendr\")~%(setf (symbol-value 'tuatara-user::*init-test-mark*) :after)~%")
            file :if-exists :supersede)
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "" :text "abc" :width 160)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "line 4" "did you mean calendar?"))))
           (is eq :after (symbol-value 'tuatara-user::*init-test-mark*))
           (true (consp tuatara.defaults::*init-file-outcome*)))
      (ignore-errors (delete-file file))
      (makunbound 'tuatara-user::*init-test-mark*))))

(define-test (config error-line-past-a-block-comment)
  ;; the line said is the failing form's, not that of a block comment before it
  (is = 10 (tuatara.defaults::next-form-start "  #| a |# (x)" 0))
  (is = 23 (tuatara.defaults::next-form-start (format nil "#| a #| nested |# b |#~%(x)") 0))
  (is = 4 (tuatara.defaults::next-form-start "#| a" 0))
  (is = 0 (tuatara.defaults::next-form-start "#'car" 0))
  (let ((said nil) (*package* (find-package :tuatara-user)))
    (tuatara.defaults::map-text-forms (format nil "(a)~%#| one~%two |#~%(b c d)~%")
                                      (lambda (form line) (push (cons form line) said)))
    (is equal '((tuatara-user::a . 1) (tuatara-user::b . 4)) (mapcar (lambda (each) (cons (first (car each)) (cdr each))) (reverse said)))))

(define-test (config bind-key-maps-said-exist)
  ;; every map the help of bind-key names is one it takes
  (let* ((help (documentation 'tuatara-user:bind-key 'function))
         (named (remove-duplicates
                 (mapcar (lambda (word) (intern (string-upcase (subseq word 1)) :keyword))
                         (cl-ppcre:all-matches-as-strings ":[a-z][a-z-]*" help))))
         (maps (list* :minibuffer (mapcar #'car (tuatara.defaults::keymap-registry)))))
    (true (member :pattern named))
    (is equal '() (set-difference named (list* :map :mode :label maps)))))

(define-test (config data-folder-not-kept-in-the-program)
  ;; a saved program forgets where data/ was when it was built
  (true (member 'tuatara.defaults::forget-data-directory sb-ext:*save-hooks*))
  (let ((tuatara.defaults::*data-directory* tuatara.defaults::*data-directory*))
    (tuatara.defaults::forget-data-directory)
    (is eq nil tuatara.defaults::*data-directory*)))

(define-test (config subfolders-named-by-config-path)
  ;; every subfolder of the configuration folder is named by CONFIG-PATH, and so
  ;; follows XDG_CONFIG_HOME alike
  (true (< 100 (length (source-files "lisp"))))
  (dolist (file (source-files "lisp"))
    (unless (equal (pathname-name file) "config")
      (false (search "xdg-config-home" (uiop:read-file-string file)) "~a" (enough-namestring file))))
  (let ((config (tuatara.defaults::config-path ""))
        (tuatara-user:*capture-template-directories* nil)
        (tuatara-user:*logos-directory* nil)
        (tuatara.defaults::*note-templates-directory* nil))
    (dolist (folder (list (first (tuatara.defaults::capture-template-directories))
                          (tuatara.defaults::logos-directory)
                          (tuatara.defaults::docx-templates-directory)
                          (tuatara.defaults::latex-templates-directory)
                          (tuatara.defaults::note-templates-directory)))
      (true (uiop:string-prefix-p config folder) "~a" folder))))

(define-test (config macro-limit-is-a-setting)
  ;; how far macro calls may grow a note for an export is set in the init file
  (is eq :external (nth-value 1 (find-symbol "*ORG-MACRO-LIMIT*" :tuatara-user)))
  (true (search ";; (setf *org-macro-limit* " tuatara.defaults::*starter-config*)))

(define-test (config definitions-named-in-the-init-file)
  ;; help names the init file as where its commands and recorded keys are defined, and its
  ;; link goes to the line there
  (let* ((file (merge-pathnames (format nil "init-~36r.lisp" (random (expt 36 8))) (tuatara.io:cache-directory)))
         (names '(tuatara-user::test-init-command tuatara-user::test-init-keys)))
    (unwind-protect
         (progn
           (with-open-file (out (ensure-directories-exist file) :direction :output :if-exists :supersede)
             (write-string "(define-command test-init-command () \"Say hello.\" (message \"hello\"))
(define-recorded-keys 'test-init-keys \"C-e\")
" out))
           (let ((tuatara.defaults:*config-file* file))
             (multiple-value-bind (f editor) (run-keys "C-h f t e s t - i n i t - k e y s RET" :text "abc" :width 100)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f (format nil "Defined in ~a" (file-namestring file))))
               (false (tuatara.frontend.fake:fake-frame-showing f "recorded-keys.lisp"))))
           (dolist (name names)
             (let ((words (tuatara.defaults::pieces-text (tuatara.defaults::source-location-pieces name :function))))
               (true (search (format nil "Defined in ~a" (file-namestring file)) words) "~(~a~): ~a" name words)))
           (let ((definitions (tuatara.defaults::lisp-definitions 'tuatara-user::test-init-keys)))
             (is = 1 (length definitions))
             (multiple-value-bind (buffer at) (funcall (tuatara.defaults::definition-locate (first definitions)))
               (unwind-protect
                    (let ((line "(define-recorded-keys 'test-init-keys"))
                      (is string= line (subseq (tuatara.buffer:buffer-string buffer) at (+ at (length line)))))
                 (when buffer (tuatara.buffer:kill-buffer buffer))))))
      (dolist (name names)
        (tuatara.command:forget-command name)
        (remhash name tuatara.defaults::*init-file-definitions*)
        (fmakunbound name))
      (ignore-errors (delete-file file)))))

(define-test (config files-the-init-file-loads-keep-their-own-source)
  ;; a file compiled and loaded, or loaded as it is, while the init file is read is where its
  ;; own definitions are, for help and its link, never the init file
  (let* ((dir (ensure-directories-exist
               (merge-pathnames (format nil "init-~36r/" (random (expt 36 8))) (tuatara.io:cache-directory))))
         (init (merge-pathnames "init.lisp" dir))
         (compiled (merge-pathnames "compiled-helper.lisp" dir))
         (plain (merge-pathnames "plain-helper.lisp" dir))
         (names '(tuatara-user::test-compiled-helper tuatara-user::test-plain-helper)))
    (unwind-protect
         (progn
           (with-open-file (out compiled :direction :output :if-exists :supersede)
             (write-string "(in-package :tuatara-user)
(defun test-compiled-helper () \"One.\" 1)
" out))
           (with-open-file (out plain :direction :output :if-exists :supersede)
             (write-string "(in-package :tuatara-user)
(defun test-plain-helper () \"Two.\" 2)
" out))
           (with-open-file (out init :direction :output :if-exists :supersede)
             (format out "(cl:load (cl:compile-file ~s :output-file ~s))~%(cl:load ~s)~%"
                     (namestring compiled) (namestring (merge-pathnames "compiled-helper.fasl" dir)) (namestring plain)))
           (let ((tuatara.defaults:*config-file* init))
             (run-keys "" :text "abc"))
           (loop for name in names
                 for file in (list compiled plain)
                 do (let ((words (tuatara.defaults::pieces-text (tuatara.defaults::source-location-pieces name :function))))
                      (true (search (format nil "Defined in ~a" (file-namestring file)) words) "~(~a~): ~a" name words))
                    (is equal (list (file-namestring file))
                        (mapcar (lambda (d) (file-namestring (tuatara.defaults::definition-group d)))
                                (tuatara.defaults::lisp-definitions name)))))
      (dolist (name names) (fmakunbound name))
      (uiop:delete-directory-tree dir :validate (lambda (p) (search "/init-" (namestring p)))))))
