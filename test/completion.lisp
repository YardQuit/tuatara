;;;; test/completion.lisp --- Matching in any order, the completion minibuffer, key hints.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test completion
  :description "Matching in any order and sorting, completing reads through the fake frontend, the key-hint panel.")

(define-test (completion matching)
  (is eq :prefix (tuatara.command:component-match "for" "forward-char"))
  (is eq :substring (tuatara.command:component-match "ward" "forward-char"))
  (is eq :initialism (tuatara.command:component-match "fc" "forward-char"))
  (is eq :initialism (tuatara.command:component-match "sbke" "save-buffers-kill-editor"))
  (is eq :flex (tuatara.command:component-match "fwdc" "forward-char"))
  (false (tuatara.command:component-match "xyz" "forward-char"))
  ;; upper case makes a component case-sensitive
  (is eq :prefix (tuatara.command:component-match "for" "Forward"))
  (false (tuatara.command:component-match "For" "forward"))
  (let* ((names '("forward-char" "backward-char" "end-of-buffer" "beginning-of-buffer" "find-file"))
         (cands (mapcar #'tuatara.command:make-candidate names)))
    (flet ((strings (input &optional history)
             (map 'list #'tuatara.command:candidate-string
                  (tuatara.command:filter-candidates input cands :history history))))
      ;; components in any order, all must match
      (is equal '("end-of-buffer" "beginning-of-buffer") (strings "buffer of"))
      (is equal '("end-of-buffer") (strings "of end"))
      ;; empty input matches everything, shorter first
      (is = 5 (length (strings "")))
      ;; prefix matches rank before substring matches, shorter before longer
      (is equal '("forward-char" "backward-char") (strings "char"))
      (is equal '("find-file" "forward-char") (subseq (strings "f") 0 2))
      ;; history comes first
      (is equal '("backward-char" "forward-char") (strings "char" '("backward-char")))))
  ;; the name typed exactly comes before history, ranks and better-placed matches
  (flet ((strings (input names &rest keys)
           (map 'list #'tuatara.command:candidate-string
                (apply #'tuatara.command:filter-candidates input (mapcar #'tuatara.command:make-candidate names) keys))))
    (is equal '("g.txt" "long.txt") (strings "g.txt" '("g.txt" "long.txt") :history '("long.txt")))
    (is equal '("emo.txt" "emo2.txt") (strings "emo.txt" '("emo2.txt" "emo.txt") :history '("emo2.txt")))
    (is equal '("emo" "emo2.txt" "demo") (strings "emo" '("demo" "emo2.txt" "emo") :history '("emo2.txt" "demo")))
    (is equal '("ab" "xab") (strings "ab" '("xab" "ab") :keep-order t))
    ;; a closer match before history and ranks, which order those matching as closely
    (is equal '("latex-templates/" "docx-templates/")
        (strings "lat" '("docx-templates/" "latex-templates/") :history '("docx-templates/")))
    (is equal '("docx-templates/" "latex-templates/")
        (strings "tem" '("latex-templates/" "docx-templates/") :history '("docx-templates/")))
    (is equal '("latex-templates/" "docx-templates/" "note-templates/")
        (map 'list #'tuatara.command:candidate-string
             (tuatara.command:filter-candidates
              "lat" (list (tuatara.command:make-candidate "note-templates/")
                          (tuatara.command:make-candidate "docx-templates/" :rank 0)
                          (tuatara.command:make-candidate "latex-templates/")))))
    (is equal '("my-latex.tex" "latest.org")
        (strings "lat" '("latest.org" "my-latex.tex") :history '("my-latex.tex")))
    (is equal "ab" (tuatara.command:candidate-string
                    (aref (tuatara.command:filter-candidates
                           "ab" (list (tuatara.command:make-candidate "ab")
                                      (tuatara.command:make-candidate "abc" :rank 0)))
                          0))))
  ;; among file names, those with most of what is typed in one piece and nearest the start first
  (is equal '("report-2026.org" "old-report.org" "draft-rep.org" "ripe-pears.org")
      (map 'list #'tuatara.command:candidate-string
           (tuatara.command:filter-candidates "rep" (mapcar #'tuatara.command:make-candidate
                                                            '("ripe-pears.org" "draft-rep.org" "old-report.org" "report-2026.org"))))))

(define-test (completion m-x-and-buffers)
  ;; partial, out-of-order input selects end-of-buffer; RET runs it
  (multiple-value-bind (f editor) (run-keys "M-x b u f SPC e n d RET" :text "abc")
    (declare (ignore f))
    (is = 3 (session-point editor)))
  ;; C-n moves to the next candidate: "forward-" matches forward-char first,
  ;; then forward-sexp, which moves over the whole word
  (multiple-value-bind (f editor) (run-keys "M-x f o r w a r d - RET" :text "abc")
    (declare (ignore f))
    (is = 1 (session-point editor)))
  (multiple-value-bind (f editor) (run-keys "M-x f o r w a r d - C-n RET" :text "abc")
    (declare (ignore f))
    (is = 3 (session-point editor)))
  ;; the panel shows candidates with annotations while the minibuffer is open
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 20)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "x")
    (setf tuatara.defaults::*command-history* '())
    (tuatara.frontend.fake:fake-queue-keys f "M-x e n d SPC o f SPC b u f")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
    (multiple-value-bind (screen spans row-faces) (tuatara.frontend.fake:fake-frame-with f "M-x end of buf")
      (declare (ignore spans))
      (is eq :completion-selected (aref row-faces (position-if (lambda (row) (search "end-of-buffer" row)) screen))))
    (true (tuatara.frontend.fake:fake-frame-showing f "end-of-buffer" "Move point to the end"))
    (true (tuatara.frontend.fake:fake-frame-showing f "end-of-buffer" "M->"))
    (true (tuatara.frontend.fake:fake-frame-showing f "M-x end of buf")))
  ;; switching buffers by a fragment of the name (the current buffer is not offered)
  (multiple-value-bind (f editor) (run-keys "C-x b o t h e r C-j C-x b s c r RET i z C-g" :text "")
    (declare (ignore f))
    (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
    (is string= "z" (session-text editor))
    (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "other")))
  ;; C-j submits the typed text as a new buffer name even though candidates exist
  (multiple-value-bind (f editor) (run-keys "C-x b n e w b u f C-j" :text "")
    (declare (ignore f))
    (is string= "newbuf" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
    (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "newbuf"))))

(define-test (completion completing-files)
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "tuatara-fc-~a/" (random 1000000)) (uiop:temporary-directory)))))
    (ensure-directories-exist (merge-pathnames "sub/" dir))
    (with-open-file (o (merge-pathnames "alpha.txt" dir) :direction :output) (write-string "A" o))
    (with-open-file (o (merge-pathnames "beta.txt" dir) :direction :output) (write-string "B" o))
    (unwind-protect
         (progn
           ;; type the directory, then "bet" to narrow, RET opens beta.txt
           (let ((keys (format nil "C-x C-f C-a C-k ~{~a ~}b e t RET"
                               (map 'list (lambda (c) (string c)) (namestring dir)))))
             (multiple-value-bind (f editor) (run-keys keys :text "")
               (declare (ignore f))
               (is string= "B" (session-text editor))))
           ;; TAB on a directory candidate descends into it
           (let ((keys (format nil "C-x C-f C-a C-k ~{~a ~}s u TAB" (map 'list (lambda (c) (string c)) (namestring dir)))))
             (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 12)))
               (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
               (tuatara.buffer:make-buffer "*scratch*" :text "")
               (tuatara.frontend.fake:fake-queue-keys f keys)
               (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
               (true (tuatara.frontend.fake:fake-frame-showing f (concatenate 'string (namestring dir) "sub/"))))))
      (dolist (b (remove-if-not (lambda (b) (and (tuatara.buffer:buffer-file b)
                                                 (search (namestring dir) (namestring (tuatara.buffer:buffer-file b)))))
                                tuatara.buffer:*buffers*))
        (tuatara.buffer:kill-buffer b))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(define-test (completion key-hints)
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 140 :height 20)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f "C-x")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
    (true (tuatara.frontend.fake:fake-frame-showing f "C-f find-file"))
    (true (tuatara.frontend.fake:fake-frame-showing f "C-s save-buffer"))
    (true (tuatara.frontend.fake:fake-frame-showing f "C-x-")))
  ;; the NORMAL-state prefix g shows its entries too
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 12)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f "g")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
    (true (tuatara.frontend.fake:fake-frame-showing f "j join-selected-lines"))))

(define-test (completion counting-candidates)
  ;; before the prompt: the selected candidate's place and how many match, as typed
  (multiple-value-bind (f editor) (run-keys "M-x e n d - o f - b u f" :text "abc" :width 80 :height 12)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "1/1    M-x end-of-buf")))
  (multiple-value-bind (f editor) (run-keys "M-x f o r w a r d - C-n" :text "abc" :width 80 :height 12)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "2/11   M-x forward-")))
  ;; nothing matches: ! where a match is needed, * where the text typed will do
  (multiple-value-bind (f editor) (run-keys "M-x z q x q z" :text "abc" :width 80 :height 12)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "!/0    M-x zqxqz")))
  (multiple-value-bind (f editor) (run-keys "C-x b z q x q z" :text "abc" :width 80 :height 12)
    (declare (ignore editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "*/0    Switch to buffer: zqxqz")))
  (let ((tuatara-user:*show-completion-count* nil))
    (multiple-value-bind (f editor) (run-keys "M-x z q x q z" :text "abc" :width 80 :height 12)
      (declare (ignore editor))
      (false (tuatara.frontend.fake:fake-frame-showing f "!/0")))))

(define-test (completion the-panel-layout)
  ;; the prompt comes right under the mode line, the candidates under it, the
  ;; selected one first: the mode line and the selection never touch
  (multiple-value-bind (f editor) (run-keys "M-x e n d SPC o f SPC b u f" :text "abc" :width 80 :height 16)
    (declare (ignore editor))
    (multiple-value-bind (screen spans row-faces) (tuatara.frontend.fake:fake-frame-with f "M-x end of buf")
      (declare (ignore spans))
      (let ((prompt (position-if (lambda (row) (search "M-x end of buf" row)) screen))
            (selected (position :completion-selected row-faces)))
        (is eq :mode-line (aref row-faces (1- prompt)))
        (is = (1+ prompt) selected)
        ;; the annotation ends at the last column but one, the documentation in the
        ;; right half, its keys before it
        (let ((row (aref screen selected)))
          (is = 79 (length row))
          (is = 40 (search "Move point to the end of the buffer." row))
          ;; the keys in brackets right after the name
          (is = 0 (search "end-of-buffer (" row))
          (true (< (search "M->" row) 40))))))
  ;; with *prompt-above-candidates* NIL, the prompt is on the bottom row, under them
  (let ((tuatara-user:*prompt-above-candidates* nil))
    (multiple-value-bind (f editor) (run-keys "M-x e n d SPC o f SPC b u f" :text "abc" :width 80 :height 16)
      (declare (ignore editor))
      (multiple-value-bind (screen spans row-faces) (tuatara.frontend.fake:fake-frame-with f "M-x end of buf")
        (declare (ignore spans))
        (is = 15 (position-if (lambda (row) (search "M-x end of buf" row)) screen))
        (true (< (position :completion-selected row-faces) 15))))))

(define-test (completion groups-and-highlights)
  ;; each group's candidates come together, where its best one comes
  (let ((cands (vector (tuatara.command:make-candidate "a1" :group "A")
                       (tuatara.command:make-candidate "b1" :group "B")
                       (tuatara.command:make-candidate "a2" :group "A"))))
    (is equal '("a1" "a2" "b1") (map 'list #'tuatara.command:candidate-string (tuatara.command:group-candidates cands)))
    (let ((plain (vector (tuatara.command:make-candidate "x"))))
      (is eq plain (tuatara.command:group-candidates plain))))
  ;; the parts the words typed match, each word in its face
  (is equal '((0 3 :completion-match-1) (7 10 :completion-match-0))
      (tuatara.command:match-spans '("buf" "end") "end-of-buffer"))
  ;; initials and scattered letters are shown where they matched
  (is equal '((0 1 :completion-match-0) (8 9 :completion-match-0))
      (tuatara.command:match-spans '("fc") "forward-char"))
  (is equal '((0 1 :completion-match-0) (4 5 :completion-match-0))
      (tuatara.command:match-spans '("fa") "forward"))
  (is equal '() (tuatara.command:match-spans '("zz") "forward")))

(define-test (completion fitting-rows)
  ;; cut at the end, or in the middle keeping the directory and the name
  (is string= "abcde…" (tuatara.display:fit-columns "abcdefghij" nil 6))
  (is string= "abcdefghij" (tuatara.display:fit-columns "abcdefghij" nil 10))
  (is string= "/hom…s/file.txt" (tuatara.display:fit-columns "/home/user/documents/file.txt" nil 15 :middle t))
  (multiple-value-bind (string spans) (tuatara.display:fit-columns "abcdefghij" '((0 3 :a) (8 10 :b)) 7 :middle t)
    (is string= "ab…ghij" string)
    (is equal '((0 2 :a) (5 7 :b)) spans))
  ;; a wide character takes two columns
  (is = 6 (tuatara.command:panel-columns (format nil "a~a~ab" (code-char #x4E2D) (code-char #x6587))))
  ;; control, direction and invisible characters written out as everywhere else, their widths counted so
  (destructuring-bind (string face spans)
      (tuatara.command::candidate-row (tuatara.command:make-candidate (format nil "tab~cname~cgnp" #\Tab (code-char #x202E)))
                                      nil 40 nil '("name") (list (format nil "zw~csp" (code-char #x200B))))
    (declare (ignore face))
    (is string= "tab^Iname<202E>gnp" (subseq string 0 18))
    (is = 39 (tuatara.command:panel-columns string))
    (is string= "zw<200B>sp" (subseq string 29))
    (is equal '(5 9 :completion-match-0) (first spans)))
  (is string= (format nil "~a <200B> " (make-string 4 :initial-element tuatara.command::*group-line-character*))
      (subseq (first (tuatara.command::group-title-row (string (code-char #x200B)) 20)) 0 12))
  ;; fields at the right give way from the left when there is no room
  (let ((fields '(("-rw-r--r--" nil) ("1.0 kB" :list-size -8) ("2 days ago" :list-date -12))))
    (is string= "-rw-r--r--    1.0 kB    2 days ago" (tuatara.command:fit-fields fields 40))
    (is string= "  1.0 kB    2 days ago" (tuatara.command:fit-fields fields 24))
    (is string= "  2 days ago" (tuatara.command:fit-fields fields 13)))
  ;; a share of the row takes what the others leave it
  (let ((fields '(("C-x f" :completion-key) ("Set the column lines are filled to, as it is." :completion-annotation 1/2))))
    (is string= "C-x f  Set the column lines are fil…" (tuatara.command:fit-fields fields 36 :row-width 80))
    (is string= "C-x f  Set the column lines are filled to, as it is.  " (tuatara.command:fit-fields fields 80 :row-width 94)))
  ;; the first sentence of a docstring, on one line
  (is string= "Insert the text." (tuatara.command:first-sentence (format nil "Insert the text.  More here.")))
  (is string= "Line one continues here." (tuatara.command:first-sentence (format nil "Line one~%continues here. Second.")))
  (is string= "No stop" (tuatara.command:first-sentence (format nil "No stop~%~%New paragraph")))
  (is string= "Uses v1.2 today." (tuatara.command:first-sentence "Uses v1.2 today. Then."))
  (is string= "" (tuatara.command:first-sentence nil)))

(define-test (completion beside-files)
  (let ((now (encode-universal-time 0 0 12 28 9 2026)))
    (flet ((ago (seconds)
             (tuatara.defaults::age-words (* (- now seconds tuatara.dates:+unix-epoch+) 1000000000) now)))
      (is string= "42 s ago" (ago 42))
      (is string= "1 min ago" (ago 90))
      (is string= "3 h ago" (ago (* 3 3600)))
      (is string= "1 day ago" (ago 86400))
      (is string= "2 days ago" (ago (* 2 86400)))
      (is string= "8 Sep 12:00" (ago (* 20 86400)))
      (is string= "28 Sep 2025" (ago (* 365 86400)))
      (let ((tuatara-user:*relative-age-limit* 0))
        (is string= "28 Sep 11:59" (ago 60)))))
  (flet ((modes (mode) (first (tuatara.defaults::mode-field mode))))
    (is string= "drwxr-xr-x" (modes #o40755))
    (is string= "-rw-r--r--" (modes #o100644))
    (is string= "-rwsr-xr-x" (modes #o104755))
    (is string= "drwxrwxrwt" (modes #o41777))
    (is string= "lrwxrwxrwx" (modes #o120777)))
  (is equal '((0 1 :completion-mode-directory) (1 2 :completion-mode-read) (2 3 :completion-mode-write)
              (3 4 :completion-mode-exec) (4 5 :completion-mode-read) (5 6 :completion-mode-none))
      (subseq (second (tuatara.defaults::mode-field #o40755)) 0 6)))

(define-test (completion candidates-tall)
  ;; M-t: as many candidates as the frame has room for, the window above down to a row and
  ;; its mode line; again, as many as usual
  (flet ((mode-line-row (keys)
           (let ((row nil))
             (run-keys (append (list "M-x") keys
                               (list (lambda ()
                                       (tuatara.command::redisplay tuatara.command:*editor*)
                                       (let ((f (tuatara.command:editor-frontend tuatara.command:*editor*)))
                                         (setf row (loop for r from 0 below 40
                                                         when (search "*scratch*" (tuatara.frontend.fake:fake-screen-line f r))
                                                           return r))))))
                       :text "" :width 100 :height 40)
             row)))
    (let ((usual (mode-line-row '())) (tall (mode-line-row '("M-t"))) (back (mode-line-row '("M-t" "M-t"))))
      (true (> usual 25))
      (is = 1 tall)
      (is = usual back))))

(define-test (completion narrow-panel)
  ;; in a frame 40 columns by 8 rows the panel leaves a window's row and its
  ;; mode line, and no row runs past the edge: keys give way whole from the end
  (let (rows)
    (run-keys (list "M-x r" (lambda () (setf rows (tuatara.command:panel-rows tuatara.command:*editor*))))
              :text "abc" :width 40 :height 8)
    (true (<= (length rows) 6))
    (dolist (row rows)
      (true (<= (tuatara.command:panel-columns (first row)) 39) "~s fits in 39 columns" (first row)))
    (true (find-if (lambda (row) (search "recenter (z z, …)  " (first row))) rows)))
  (flet ((fit (columns)
           (tuatara.command::fit-key-list "undo (u, C-/, C-_)" '((5 18 :completion-key)) columns 4)))
    (is string= "undo (u, C-/, C-_)" (fit 18))
    (is string= "undo (u, C-/, …)" (fit 17))
    (is string= "undo (u, …)" (fit 12))
    (is string= "undo" (fit 10))
    (is string= "un…" (fit 3))
    (is equal '((5 11 :completion-key)) (nth-value 1 (fit 12)))))

(defvar *menu-test-given* :none
  "What the test menu's action was given, MENU-ARGUMENTS when it ran.")

(tuatara.command:define-command menu-test-act ()
  "Keep the arguments of the open menu, for the menu tests."
  (setf *menu-test-given* (tuatara.command:menu-arguments)))

(tuatara.command:define-command menu-test-hidden ()
  "An action the test menu leaves out."
  (setf *menu-test-given* :hidden))

(defun menu-test-shows-p (entry)
  "The test menu shows every entry but MENU-TEST-HIDDEN."
  (not (eq (tuatara.command:menu-entry-command entry) 'menu-test-hidden)))

(tuatara.command:define-menu menu-test-menu (:title "Test menu" :show menu-test-shows-p)
  "Open the menu the tests use."
  ("Do" ("a" "Act" menu-test-act) ("z" "Hidden" menu-test-hidden))
  ("Switches" ("- f" "Force" :switch "--force") ("= d" "Depth" :option "--depth=")))

(defmacro define-test-tall-menu ()
  "Define the menu of 24 switches the menu tests use, taller than the key hints."
  `(tuatara.command:define-menu menu-test-tall-menu (:title "Tall test menu")
     "Open the tall menu the tests use."
     ("Switches" ,@(loop for i from 1 to 24
                         collect (list (format nil "- ~a" (char "abcdefghijklmnopqrstuvwx" (1- i)))
                                       (format nil "Switch number ~d" i) :switch (format nil "--switch-~d" i))))))

(define-test-tall-menu)

(define-test (completion menu-switches-by-name)
  ;; a switch is on only as itself: --force is not given by --force-with-lease, nor --sign by --signoff;
  ;; an option is given by its flag with any value
  (let ((force (tuatara.command::make-menu-entry "- F" "Force" :switch "--force"))
        (sign (tuatara.command::make-menu-entry "- S" "Sign" :switch "--sign"))
        (depth (tuatara.command::make-menu-entry "= d" "Depth" :option "--depth="))
        (search (tuatara.command::make-menu-entry "= S" "Changes the count of" :option "-S")))
    (false (tuatara.command::menu-given force '("--force-with-lease")))
    (is equal "--force" (tuatara.command::menu-given force '("--force-with-lease" "--force")))
    (false (tuatara.command::menu-given sign '("--signoff")))
    (is equal "--depth=3" (tuatara.command::menu-given depth '("--depth=3")))
    (is equal "-Sfoo" (tuatara.command::menu-given search '("-Sfoo")))))

(define-test (completion menus)
  ;; a switch and an option typed keep the menu open; the action gets them and closes it
  (setf *menu-test-given* :none)
  (let ((f (run-keys "M-x m e n u - t e s t - m e n u RET - f = d 3 RET" :width 100 :height 16)))
    (true (tuatara.frontend.fake:fake-frame-showing f "Test menu"))
    (true (tuatara.frontend.fake:fake-frame-showing f "a Act"))
    (true (tuatara.frontend.fake:fake-frame-showing f "- f Force (--force)"))
    (true (tuatara.frontend.fake:fake-frame-showing f "= d Depth (--depth=3)"))
    (false (tuatara.frontend.fake:fake-frame-showing f "Hidden")))
  (run-keys "M-x m e n u - t e s t - m e n u RET - f = d 3 RET a" :width 100 :height 16)
  (is equal '("--force" "--depth=3") *menu-test-given*)
  ;; a switch typed twice is off again; an option typed again loses its value
  (run-keys "M-x m e n u - t e s t - m e n u RET - f - f = d 3 RET = d a" :width 100 :height 16)
  (is equal '() *menu-test-given*)
  ;; the action closes the menu: a key after it is the buffer's own
  (multiple-value-bind (f editor) (run-keys "M-x m e n u - t e s t - m e n u RET a x" :width 100 :height 16)
    (declare (ignore f))
    (is equal nil (tuatara.command:editor-menu editor)))
  ;; C-g closes it, the action not run
  (setf *menu-test-given* :none)
  (run-keys "M-x m e n u - t e s t - m e n u RET C-g a" :width 100 :height 16)
  (is eq :none *menu-test-given*)
  ;; the key of an entry it leaves out says so, runs nothing and keeps it open
  (multiple-value-bind (f editor) (run-keys "M-x m e n u - t e s t - m e n u RET z" :width 100 :height 16)
    (true (tuatara.frontend.fake:fake-frame-showing f "Hidden (z) is left out of this menu now"))
    (true (tuatara.command:editor-menu editor))
    (is eq :none *menu-test-given*))
  ;; run by M-x, an action gets no arguments
  (run-keys "M-x m e n u - t e s t - a c t RET" :width 100 :height 16)
  (is equal '() *menu-test-given*)
  ;; with - or = typed, the menu stays in the panel with its labels
  (let ((f (run-keys "M-x m e n u - t e s t - m e n u RET -" :width 100 :height 16)))
    (true (tuatara.frontend.fake:fake-frame-showing f "- f Force (--force)"))
    (false (tuatara.frontend.fake:fake-frame-showing f "menu-toggle-switch")))
  ;; a menu taller than the key hints takes the rows the frame has; cut, it says C-h lists it all, whole
  (let ((f (run-keys "M-x m e n u - t e s t - t a l l - m e n u RET" :width 60 :height 30)))
    (true (tuatara.frontend.fake:fake-frame-showing f "- x Switch number 24"))
    (false (tuatara.frontend.fake:fake-frame-showing f "lists all")))
  (let ((f (run-keys "M-x m e n u - t e s t - t a l l - m e n u RET" :width 60 :height 10)))
    (true (tuatara.frontend.fake:fake-frame-showing f "... C-h lists all"))
    (false (tuatara.frontend.fake:fake-frame-showing f "- x Switch number 24")))
  ;; the menu's own commands are not offered by M-x, and reached by a key with no menu open they say where they work
  (let ((f (run-keys "M-x m e n u - t o g g l e" :width 100 :height 16)))
    (false (tuatara.frontend.fake:fake-frame-showing f "menu-toggle-switch")))
  (unwind-protect
       (let ((f (run-keys (format nil "M-: ~a RET C-c z" (key-string "(bind-key \"C-c z\" 'tuatara.command::menu-toggle-switch)"))
                          :width 120 :height 16)))
         (true (tuatara.frontend.fake:fake-frame-showing f "menu-toggle-switch works in an open menu only")))
    ;; the key given back, for the tests after
    (tuatara-user:unbind-key "C-c z")))

;;; A menu opened with values, its option asked again, a summary under it, a switch that comes with a value.

(defun menu-test-read-again (prompt &optional current)
  "Read the test menu's option again, CURRENT offered."
  (tuatara.command:read-string prompt :initial (or current "")))

(defun menu-test-summary (arguments)
  "The test menu's summary of its ARGUMENTS."
  (list (format nil "Summed: ~{~a~^ ~}" arguments)))

(defun menu-test-filled-shows-p (entry)
  "The filled test menu shows its deep switch only once a depth is given."
  (or (not (equal (tuatara.command:menu-entry-argument entry) "--deep"))
      (find-if (lambda (a) (uiop:string-prefix-p "--depth=" a)) (tuatara.command:menu-arguments))))

(tuatara.command:define-menu menu-test-filled-menu (:title "Filled menu" :show menu-test-filled-shows-p :summary menu-test-summary)
  "Open the filled menu the tests use."
  ("Do" ("a" "Act" menu-test-act))
  ("Options" ("= d" "Depth" :option "--depth=" :read menu-test-read-again :replace t) ("- e" "Deep" :switch "--deep")))

(tuatara.command:define-command menu-test-open-filled ()
  "Open the filled test menu with a name given."
  (tuatara.command:open-menu (tuatara.command:menu-named 'menu-test-filled-menu) :arguments '("--name=kea")))

(define-test (completion menus-asked-again-and-summed)
  ;; opened with values, they are its arguments, its summary under its entries
  (setf *menu-test-given* :none)
  (let ((f (run-keys "M-x m e n u - t e s t - o p e n - f i l l e d RET" :width 100 :height 16)))
    (true (tuatara.frontend.fake:fake-frame-showing f "Summed: --name=kea")))
  ;; a switch the menu has only with a value: left out, then there
  (multiple-value-bind (f editor) (run-keys "M-x m e n u - t e s t - o p e n - f i l l e d RET - e" :width 100 :height 16)
    (true (tuatara.frontend.fake:fake-frame-showing f "Deep (- e) is left out of this menu now"))
    (true (tuatara.command:editor-menu editor)))
  (run-keys "M-x m e n u - t e s t - o p e n - f i l l e d RET = d 3 RET - e a" :width 100 :height 16)
  (is equal '("--name=kea" "--depth=3" "--deep") *menu-test-given*)
  ;; an option asked again shows its value and takes the new one; an empty answer takes it away
  (let ((f (run-keys "M-x m e n u - t e s t - o p e n - f i l l e d RET = d 3 RET = d DEL 5 RET" :width 100 :height 16)))
    (true (tuatara.frontend.fake:fake-frame-showing f "Summed: --name=kea --depth=5")))
  (run-keys "M-x m e n u - t e s t - o p e n - f i l l e d RET = d 3 RET = d DEL RET a" :width 100 :height 16)
  (is equal '("--name=kea") *menu-test-given*))

(define-test (completion key-hint-layout-linear)
  ;; laying out key hints costs the same for each hint however many there are
  (flet ((seconds-per-hint (n)
           (let ((entries (loop for i below n collect (cons (format nil "k~d" i) (format nil "command-~d" i))))
                 (start (thread-cpu-time))
                 (calls 0))
             (loop do (tuatara.command::key-hint-rows entries 200 n) (incf calls)
                   until (> (- (thread-cpu-time) start) (* 1/5 internal-time-units-per-second)))
             (/ (- (thread-cpu-time) start) internal-time-units-per-second calls n))))
    (let ((small (seconds-per-hint 500))
          (large (seconds-per-hint 10000)))
      (true (< large (* 3 small)) "~,3e against ~,3e s per hint" large small))))

(define-test (completion key-hint-groups-line-up)
  ;; groups laid out one under another with one column width have their columns at the same places
  (let* ((short '(("a" . "act") ("b" . "back") ("c" . "cut")))
         (long '(("x" . "a much longer name") ("y" . "why") ("z" . "zed")))
         (column (tuatara.command::key-hint-column-width short long)))
    (flet ((second-column (rows) (search "  " (first (first rows)) :from-end nil)))
      (let ((one (tuatara.command::key-hint-rows short 60 1 :column-width column))
            (two (tuatara.command::key-hint-rows long 60 1 :column-width column)))
        (is = (search "b back" (first (first one))) (search "y why" (first (first two))))
        (true (second-column one)))))
  ;; a menu's groups line up: the second column of the filled test menu's options under its action's
  (let ((f (run-keys "M-x m e n u - t e s t - o p e n - f i l l e d RET = d 3 RET" :width 100 :height 16)))
    (let ((rows (loop for i below 16 collect (tuatara.frontend.fake:fake-screen-line f i))))
      (true (find-if (lambda (row) (search "= d Depth (--depth=3)" row)) rows)))))

(define-test (completion flat-spans-by-runs)
  ;; spans laid flat: later ones' faces first, runs of the same faces joined
  (is equal '((0 2 :a) (2 4 (:b :a)) (4 6 :b))
      (tuatara.command::flatten-spans '((0 4 :a) (2 6 :b)) 10))
  (is equal '((0 2 :a) (2 6 :b)) (tuatara.command::flatten-spans '((0 4 :a) (2 6 :b)) 10 :last t))
  (is equal '((0 6 :a)) (tuatara.command::flatten-spans '((0 3 :a) (3 6 :a)) 10))
  (is equal '((0 3 :a)) (tuatara.command::flatten-spans '((-2 3 :a) (12 14 :b)) 10))
  ;; the work goes by spans, not by characters
  (flet ((consed (length)
           (nth-value 2 (tuatara.bench::measuring
                          (tuatara.command::flatten-spans (list (list 0 length :a) (list 5 (- length 5) :b)) length)))))
    (true (< (consed 1000) 2000))
    (true (< (consed 100000) 2000))))

(define-test (completion one-walk-for-match-and-places)
  ;; the places a word typed lights up are those its match is made of, found by the same walk
  (let ((names '("forward-char" "save-buffers-kill-editor" "Forward" "kill-region" "describe-key-briefly"
                 "find-file-other-window" "org-agenda" "OrgMode" "x" "")))
    (dolist (string names)
      (dolist (component '("for" "ward" "fc" "sbke" "fwdc" "xyz" "For" "kr" "dkb" "ffow" "oa" "OM" "x" "e"))
        (let ((kind (tuatara.command:component-match component string))
              (places (tuatara.command::component-positions component string)))
          (is eq (and kind t) (and places t) "~s in ~s" component string)
          (when places
            (is = (length component) (length places))
            (true (apply #'< places))
            (true (every (lambda (c p) (char-equal c (char string p))) component places)))
          (when (member kind '(:prefix :substring))
            (is equal (loop for i from (nth-value 1 (tuatara.command:component-match component string))
                            repeat (length component) collect i)
                places))))))
  ;; and it goes faster than a plain search for the word over the names
  (let ((candidates (loop for i below 3000 collect (tuatara.command:make-candidate (format nil "kea-~d-mountain-parrot-of-the-alps" i)))))
    (flet ((fastest (function)
             (loop repeat 5
                   minimize (let ((start (tuatara.bench::monotonic-seconds)))
                              (funcall function)
                              (- (tuatara.bench::monotonic-seconds) start)))))
      (let ((filter (fastest (lambda () (tuatara.command:filter-candidates "kmpz" candidates))))
            (search (fastest (lambda () (dolist (c candidates) (search "kmpz" (tuatara.command:candidate-string c) :test #'char-equal))))))
        (true (< filter search) "filtering took ~,2f ms, searching ~,2f ms" (* 1000 filter) (* 1000 search))))))

(define-test (completion filtering-makes-no-garbage-for-each-candidate)
  ;; filtering and sorting many candidates makes only the vector of those matching:
  ;; octets for each candidate more, as the difference between two sizes
  (flet ((made (count input &rest keys)
           (let ((candidates (loop for i below count collect (tuatara.command:make-candidate (format nil "file-~d.txt" i)))))
             (apply #'tuatara.command:filter-candidates input candidates keys)
             (let ((before (sb-ext:get-bytes-consed)))
               (apply #'tuatara.command:filter-candidates input candidates keys)
               (- (sb-ext:get-bytes-consed) before)))))
    (made 40000 "f")
    (dolist (case '(("f") ("le 1") ("" :keep-order :until-typed)))
      (let ((each (/ (- (apply #'made 40000 case) (apply #'made 20000 case)) 20000.0)))
        ;; 8 octets for each, for the vector returned
        (true (< each 16) "~s: ~,1f octets for each candidate" case each)))))

(define-test (completion sorted-alike-with-many-candidates)
  ;; the order is the one documented, however many there are
  (let* ((names (loop for i below 5000 collect (format nil "~:[note~;Note~]-~d" (evenp i) (mod (* i 7919) 5000))))
         (sorted (map 'list #'tuatara.command:candidate-string
                      (tuatara.command:filter-candidates "note" (mapcar #'tuatara.command:make-candidate names)))))
    (is = 5000 (length sorted))
    ;; all match at a word's start alike: shorter first, then by characters' codes
    (true (loop for (a b) on sorted while b
                always (or (< (length a) (length b)) (and (= (length a) (length b)) (string<= a b)))))))

(define-test (completion exact-buffer-name-wins-over-longer-names)
  ;; a buffer's name typed whole and RET switches to that buffer, though other names hold it
  (flet ((switched-to (current keys)
           (let ((made (mapcar (lambda (name) (tuatara.buffer:make-buffer name :text name))
                               '("a.txt" "*Conflict: a.txt*" "*Diff: a.txt*" "b.txt"))))
             (unwind-protect
                  (multiple-value-bind (f editor) (run-keys (format nil "C-x b ~a RET ~a" (key-string current) keys) :text "")
                    (declare (ignore f))
                    (tuatara.buffer:buffer-name (session-buffer editor)))
               (mapc #'tuatara.buffer:kill-buffer made)))))
    (is string= "a.txt" (switched-to "*Diff: a.txt*" "C-x b a . t x t RET"))
    (is string= "a.txt" (switched-to "b.txt" "C-x b a . t x t RET"))
    ;; the buffer shown already is no candidate: the name typed is taken as typed
    (is string= "a.txt" (switched-to "a.txt" "C-x b a . t x t RET"))))
