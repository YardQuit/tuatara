;;;; test/health.lisp --- M-x check-health: what each language and mode has here.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test health
  :description "M-x check-health shows *Health*: where the init file and grammars are, each language's
grammar and rules checked (a missing grammar or rules that do not compile said, and why), each major
mode's language, comments, tabs and files; the rules are compiled in the background when that takes
a moment.")

(defun health-text ()
  "The text of *Health*, or NIL when there is none."
  (let ((b (tuatara.buffer:find-buffer "*Health*")))
    (and b (tuatara.buffer:buffer-string b))))

(defun health-line (text start)
  "The line of TEXT that begins with START, or NIL."
  (find-if (lambda (line) (uiop:string-prefix-p start line))
           (uiop:split-string text :separator '(#\Newline))))

(define-test (health languages-and-modes)
  (let ((tuatara.defaults::*health-wait* 60))
    (multiple-value-bind (f editor) (run-keys "M-x c h e c k - h e a l t h RET" :width 120 :height 50)
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Tuatara health"))
      (let ((text (health-text)))
        (true (search "Init file" text))
        ;; the sandbox, found and working in the test toolbox
        (true (search "bubblewrap" (health-line text "Sandbox")))
        (true (search "no network" (health-line text "Sandbox")))
        (true (search "(seccomp)" (health-line text "Sandbox")))
        ;; and what decodes PDFs: poppler, installed in the test toolbox
        (true (search "PDF text by pdftotext and pdfinfo (poppler)" (health-line text "Decoders")))
        (true (search "the program that runs them and" text))
        ;; every language's grammar found and its rules working
        (dolist (language '("bash" "c" "containerfile" "lisp" "rust"))
          (let ((line (health-line text (format nil "~a " language))))
            (true line)
            (false (search "✘" line))
            (true (search "✓ rich" line))))
        ;; Lisp indents by its own rules, and has definitions by brackets
        (let ((line (health-line text "lisp ")))
          (true (search "Lisp" line :start2 5))
          (true (search "brackets" line)))
        (true (search "✓" (health-line text "rust ") :start2 20))
        ;; the modes, their language, comments, tabs and files
        (let ((line (health-line text "rust-mode ")))
          (true (search "rust" line :start2 9))
          (true (search "//" line))
          (true (search "4 spaces" line))
          (true (search ".rs" line))
          (true (search "#!rust-script" line)))
        (true (search "4 tabs" (health-line text "tsv-mode ")))
        (true (search ";;" (health-line text "lisp-mode ")))
        (true (search "The editor's own buffers:" text))
        (true (search "help-mode" text))
        (true (search "None found." text))))))

(define-test (health problems-said)
  (unwind-protect
       (progn
         (tuatara.defaults::register-language 'health-broken "Rules that do not compile." "rust" '()
                                              :definitions "(no_such_node) @definition")
         (tuatara.defaults::register-language 'health-missing "A grammar that is not there." "no-such-grammar" '())
         (let* ((check (tuatara.defaults::start-health-check)))
           (sb-thread:join-thread (tuatara.runtime:job-thread check))
           (tuatara.runtime:forget-job check)
           (let ((text (tuatara.defaults::health-report check)))
             ;; the broken rules: ✘ in their column, and why among the problems
             (let ((line (health-line text "health-broken ")))
               (true (search "✘" line))
               (true (search "✓" line)))
             (true (search "health-broken: its defuns rules do not compile:" text))
             ;; no grammar: ✘ for it, its rules not checked
             (let ((line (health-line text "health-missing ")))
               (true (search "✘" line)))
             (true (search "health-missing: no grammar no-such-grammar.so in" text))
             (false (search "None found." text)))))
    (setf tuatara.defaults::*languages*
          (remove-if (lambda (l) (member (tuatara.defaults::language-name l) '(health-broken health-missing)))
                     tuatara.defaults::*languages*))))

(define-test (health checked-in-the-background)
  ;; not ready at once: said so, and shown by the tick when it is
  (let ((tuatara.defaults::*health-wait* 0))
    (let ((scratch (tuatara.buffer:find-buffer "*Health*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (multiple-value-bind (f editor)
        (labels ((settle ()
                   ;; waits, the editor taking jobs in between, until the check is taken in
                   (when (some #'tuatara.defaults::health-check-p tuatara.runtime:*jobs*)
                     (sleep 0.02)
                     (tuatara.frontend.fake:fake-queue-action (tuatara.command::editor-frontend tuatara.command::*editor*) #'settle))))
          (run-keys (list "M-x c h e c k - h e a l t h RET" #'settle) :width 120 :height 40))
      (declare (ignore editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Checking the languages' rules in the background"))
      (true (tuatara.frontend.fake:fake-frame-showing f "the report comes when it is ready"))
      (false (tuatara.defaults::health-check-running))
      (true (search "Tuatara health" (health-text))))))

(define-test (health report-keys)
  (let ((tuatara.defaults::*health-wait* 60))
    ;; q in the report closes its window; r checks again
    (multiple-value-bind (f editor) (run-keys "M-x c h e c k - h e a l t h RET C-x o r q" :width 120 :height 40)
      (is string= "*scratch*" (current-name editor))
      (is = 1 (length (tuatara.command:editor-windows editor)))
      (false (loop for row from 0 below (length (tuatara.frontend.fake:fake-screen f))
                   thereis (search "Tuatara health" (tuatara.frontend.fake:fake-screen-line f row))))
      (true (health-text)))))

(define-test (health facts-from-elsewhere)
  (multiple-value-bind (extensions names interpreters patterns)
      (tuatara.modes:mode-file-rules 'tuatara.defaults::rust-mode)
    (is equal '("rs") extensions)
    (is equal '() names)
    (is equal '("cargo" "rust-script") interpreters)
    (is equal '() patterns))
  (let ((facts (tuatara.frontend:frontend-health (tuatara.frontend.tui:make-tui-frontend))))
    (true (assoc "Display" facts :test #'string=))
    (true (assoc "Clipboard" facts :test #'string=)))
  (is equal '() (tuatara.frontend:frontend-health (tuatara.frontend.fake:make-fake-frontend))))

(define-test (health latex-shut-in-said)
  ;; health says whether LaTeX runs shut in by the sandbox, where an engine is found
  (when (tuatara.defaults::latex-engines-found)
    (if (eq :ready (tuatara.io:sandbox-status))
        (true (search "shut in by the sandbox" (tuatara.defaults::latex-export-words)))
        (true (search "unconfined" (tuatara.defaults::latex-export-words))))
    ;; no sandbox program: unconfined, and said so
    (unwind-protect
         (let ((tuatara-user:*sandbox-program* nil))
           (tuatara.io:sandbox-status :again t)
           (true (search "unconfined" (tuatara.defaults::latex-export-words))))
      (tuatara.io:sandbox-status :again t))))

(define-test (health drawing-recommended)
  ;; the window's way of drawing to use, from what each way measured: GL, the
  ;; standard, unless another shows clearly more of the keys held, or as many sooner
  (flet ((figures (frames median) (list :renderer "r" :keys 180 :frames frames :median median :slowest (* 2 median))))
    (is eq :gl (tuatara.defaults::drawing-choice (list (cons :gl (figures 180 15.0)) (cons :vulkan (figures 178 14.0)))))
    (is eq :gl (tuatara.defaults::drawing-choice (list (cons :gl (figures 180 15.0)) (cons :vulkan (figures 180 14.0)))))
    (is eq :vulkan (tuatara.defaults::drawing-choice (list (cons :gl (figures 150 23.0)) (cons :vulkan (figures 180 25.0)))))
    (is eq :vulkan (tuatara.defaults::drawing-choice (list (cons :gl (figures 180 15.0)) (cons :vulkan (figures 180 12.0)))))
    ;; a millisecond sooner as a rule but slower at the slowest of twenty: alike, the standard kept
    (flet ((with (frames median slowest) (list :renderer "r" :keys 180 :frames frames :median median :slowest slowest)))
      (is eq :gl (tuatara.defaults::drawing-choice (list (cons :gl (with 179 6.0 7.0)) (cons :vulkan (with 180 5.0 9.0)))))
      (is eq :gl (tuatara.defaults::drawing-choice (list (cons :gl (with 180 3.0 4.0)) (cons :vulkan (with 180 2.5 3.0))))))
    ;; one not measured: the other; none: the standard
    (is eq :vulkan (tuatara.defaults::drawing-choice (list (cons :gl nil) (cons :vulkan (figures 120 30.0)))))
    (is eq :gl (tuatara.defaults::drawing-choice (list (cons :gl nil) (cons :vulkan nil))))
    ;; the report: each way's figures, then which to use, and how when it is not the one in use
    (let ((words (tuatara.defaults::drawing-words (list (cons :gl (figures 150 23.0)) (cons :vulkan (figures 180 25.0))) :gl)))
      (is = 3 (length words))
      (true (search "GL: 23.0 ms" (first words)))
      (true (search "150 frames for 180 keys" (first words)))
      (true (search "(setf *gui-renderer* :vulkan)" (third words))))
    (true (search "the one in use" (third (tuatara.defaults::drawing-words (list (cons :gl (figures 180 15.0)) (cons :vulkan nil)) :gl))))
    (false (tuatara.defaults::drawing-words '() :gl))))

(define-test (health drawing-fallen-back)
  ;; a way GTK could not draw with, falling back to another, is said to have fallen back, its
  ;; figures never those of the way asked for, and never called the one in use
  (flet ((figures (renderer frames median)
           (list :renderer renderer :keys 180 :frames frames :median median :slowest (* 2 median))))
    (let ((words (tuatara.defaults::drawing-words (list (cons :gl (figures "GskBroadwayRenderer" 180 10.5))
                                                        (cons :vulkan (figures "GskBroadwayRenderer" 180 9.1)))
                                                  :gl)))
      (true (search "GL: not to be had here, GTK drew with GskBroadwayRenderer instead" (first words)))
      (true (search "Vulkan: not to be had here" (second words)))
      (false (search "in use" (third words))))
    ;; one fell back, the other drew as asked: the other is chosen
    (is eq :vulkan (tuatara.defaults::drawing-choice (list (cons :gl (figures "GskCairoRenderer" 180 5.0))
                                                           (cons :vulkan (figures "GskVulkanRenderer" 150 20.0)))))
    ;; both as asked, under their own names
    (is eq :gl (tuatara.defaults::drawing-choice (list (cons :gl (figures "GskGLRenderer" 180 15.0))
                                                       (cons :vulkan (figures "GskVulkanRenderer" 178 14.0)))))))

(define-test (health outside-programs-agree)
  ;; the software from outside, one table: the distribution packages recommend and suggest
  ;; what it names, and M-x check-health says what to install from it, git among them
  (let* ((script (uiop:read-file-lines (merge-pathnames "tools/distro-packages.sh" (asdf:system-source-directory :tuatara))))
         (programs tuatara.defaults::*outside-programs*))
    (flet ((field (prefix commas)
             ;; the packages of the line beginning with PREFIX, with commas between them or not
             (let ((line (find-if (lambda (line) (and (uiop:string-prefix-p prefix line) (eq commas (and (find #\, line) t))))
                                  script)))
               (and line (sort (mapcar (lambda (word) (string-trim ", " word))
                                       (tuatara.text:split-words (subseq line (length prefix))))
                               #'string<))))
           (table (system weight)
             (sort (loop for (nil . entry) in programs
                         when (or (null weight) (eq weight (getf entry :weight)))
                           append (copy-list (getf entry system)))
                   #'string<)))
      (is equal (table :fedora :recommends) (field "Recommends: " nil))
      (is equal (table :fedora :suggests) (field "Suggests: " nil))
      (is equal (table :debian :recommends) (field "Recommends: " t))
      (is equal (table :debian :suggests) (field "Suggests: " t))
      ;; Arch's optional packages, each with what it is for
      (is equal (sort (loop for (nil . entry) in programs
                            append (mapcar (lambda (package) (format nil "~a: ~a" package (getf entry :what))) (getf entry :arch)))
                      #'string<)
          (sort (loop for line in script
                      for start = (position #\' line)
                      for end = (and start (position #\' line :start (1+ start)))
                      when (and end (or (search "optdepends=(" line) (uiop:string-prefix-p "            '" line)))
                        collect (subseq line (1+ start) end))
                #'string<))))
  (is string= "install GnuPG (Fedora: gnupg2)" (tuatara.defaults::outside-program-install-words :gpg :fedora))
  (true (search "Debian and Ubuntu: gnupg; Arch: gnupg" (tuatara.defaults::outside-program-install-words :gpg nil)))
  (let ((words (tuatara.defaults::git-health-words)))
    (true (or (search "by /" words) (search "install git" words)) "~a" words)))
