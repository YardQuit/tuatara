;;;; test/org-tables.lisp --- Org's tables and their formulas, and code blocks written to files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (org tables)
  (flet ((lines (&rest lines) (format nil "~{~a~^~%~}" lines)))
    ;; C-c C-c aligns: numbers to the right, |- a rule, short rows filled, the first row's indentation
    (is string= (lines "  |   a | bb   |       |" "  |-----+------+-------|" "  |   1 | 22.5 |       |"
                       "  | 333 | x    | extra |" "  |     |      |       |" "")
        (remove #\@ (org-table-after "C-c C-c" (lines "  | a@ | bb |" "|-" "| 1 | 22.5 |" "| 333 |  x  |  extra |" "|  |" ""))))
    (is string= (lines "| name  |   n |" "| alpha |   1 |" "| b     | 200 |" "| c     |     |" "| ddd   |   x |@")
        (org-table-after "i C-c C-c" (lines "| name | n |" "| alpha | 1 |" "| b | 200 |" "| c |  |" "| ddd | x |@")))
    (is string= (lines "|  <c>   |    <r> | <l> |" "|   a    |      b | c   |" "| longer | longer | 1   |")
        (remove #\@ (org-table-after "C-c C-c" (lines "| <c> | <r> | <l> |" "| a | b | c |" "| l@onger | longer | 1 |"))))
    (is string= (lines "| 日本 | a  |" "| x    | bb |") (remove #\@ (org-table-after "C-c C-c" (lines "| 日本 | a@ |" "| x | bb |"))))
    (is string= (lines "| a |   | c |" "| b |   |   |") (remove #\@ (org-table-after "C-c C-c" (lines "| a |  | c |" "| b@ | |"))))
    (is string= (lines "| 1 | 2 |" "#+TBLFM: $2=$1*2") (remove #\@ (org-table-after "C-c C-c" (lines "| 1 | 2@ |" "#+TBLFM: $2=$1*2"))))
    ;; a call of redact or protect counts as its text alone, the braces hidden on screen
    (is string= (lines "| User | Host |" "| {{{R(a1)}}}   | {{{P(w)}}}    |")
        (remove #\@ (org-table-after "C-c C-c" (lines "| User@ | Host |" "| {{{R(a1)}}} | {{{P(w)}}} |"))))
    ;; TAB aligns so too
    (is string= (lines "| User | @Host |" "| {{{R(a1)}}}   | {{{P(w)}}}    |")
        (org-table-after "TAB" (lines "| User@ | Host |" "| {{{R(a1)}}} | {{{P(w)}}} |")))
    ;; point stays on its character
    (is string= (lines "| abc | d   |" "| e   | fg@h |") (org-table-after "C-c C-c" (lines "| abc | d |" "| e | fg@h |")))
    ;; TAB: the next field, over rules, a new row after the last (before a closing rule)
    (is string= (lines "| a | @b |" "|---+---|" "| c | d |") (org-table-after "TAB" (lines "| a@ | b |" "|---+---|" "| c | d |")))
    (is string= (lines "| a | b |" "|---+---|" "| @c | d |") (org-table-after "TAB" (lines "| a | @b |" "|---+---|" "| c | d |")))
    (is string= (lines "| a | b |" "| c | d |" "| @  |   |" "after") (org-table-after "TAB" (lines "| a | b |" "| c | @d |" "after")))
    (is string= (lines "| a | b |" "| c | d |" "| @  |   |" "|---+---|" "after")
        (org-table-after "TAB" (lines "| a | b |" "| c | @d |" "|---+---|" "after")))
    (is string= (lines "| a | b |" "| @  |   |" "") (org-table-after "i TAB" (lines "| a | b@" "")))
    (is string= (lines "| a | bb |" "|---+----|" "| @  |    |" "") (org-table-after "i TAB" (lines "| a | bb |" "|-@" "")))
    (is string= (lines "| a | bb |" "|---+----|" "| @c | d  |" "") (org-table-after "TAB" (lines "| a | bb |" "|@-" "| c | d |" "")))
    (is string= (lines "  | a   |" "  | @bbb |") (org-table-after "TAB" (lines "  | a@ |" "  | bbb |")))
    ;; in a column of numbers, aligned right, TAB goes to the field's start, before the spaces, as Org goes
    (is string= (lines "| a | @  1 |" "| b | 200 |") (org-table-after "TAB" (lines "| a@ | 1 |" "| b | 200 |")))
    (is string= (lines "| 1 | 2 |" "| 3 | 4 |" "| @  |   |" "") (org-table-after "TAB" (lines "| 1 | 2 |" "| 3 | 4@ |" "")))
    (is string= (lines "|   1 | x |" "| @200 | y |") (org-table-after "i RET" (lines "| 1@ | x |" "| 200 | y |")))
    ;; S-TAB back a field
    (is string= (lines "| a | @b |" "| c | d |") (org-table-after "S-TAB" (lines "| a | b |" "| @c | d |")))
    ;; RET: the row below, a new one at the end or before a rule
    (is string= (lines "| a | b |" "| @c | d |") (org-table-after "i RET" (lines "| a@ | b |" "| c | d |")))
    (is string= (lines "| a | b |" "|   | @  |" "after") (org-table-after "i RET" (lines "| a | b@ |" "after")))
    (is string= (lines "| a | b |" "|   | @  |" "|---+---|" "| c | d |") (org-table-after "i RET" (lines "| a | b@ |" "|---+---|" "| c | d |")))
    ;; C-c - a rule below
    (is string= (lines "| a | b@ |" "|---+---|" "| c | d |") (org-table-after "C-c -" (lines "| a | b@ |" "| c | d |")))
    ;; M-j M-k rows, M-h M-l columns, M-H M-L delete and insert columns, M-S-up M-S-down rows
    (is string= (lines "| c | d |" "| a@ | b |" "| e | f |") (org-table-after "M-j" (lines "| a@ | b |" "| c | d |" "| e | f |")))
    (is string= (lines "| c | d@ |" "| a | b |" "") (org-table-after "M-k" (lines "| a | b |" "| c | d@ |" "")))
    (is string= (lines "| bb | @a |" "| d  | c |" "") (org-table-after "M-l" (lines "| a@ | bb |" "| c | d |" "")))
    (is string= (lines "| @bb | a |" "| d  | c |" "") (org-table-after "M-h" (lines "| a | b@b |" "| c | d |" "")))
    (is string= (lines "| a | @c |" "|---+---|" "| c | e |") (org-table-after "M-H" (lines "| a | b@b | c |" "|---+---+---|" "| c | d | e |")))
    (is string= (lines "| a | @  | bb |" "|---+---+----|" "| c |   | d  |" "") (org-table-after "M-L" (lines "| a | b@b |" "|---+---|" "| c | d |" "")))
    (is string= (lines "| a | b |" "| e@ | f |") (org-table-after "M-S-<up>" (lines "| a | b |" "| c@ | d |" "| e | f |")))
    (is string= (lines "| a | b  |" "| @  |    |" "| c | dd |") (org-table-after "M-S-<down>" (lines "| a | b |" "| c@ | dd |")))
    ;; at the edge, nothing moves
    (is string= (lines "| a@ | b |" "| c | d |") (org-table-after "M-k" (lines "| a@ | b |" "| c | d |")))
    ;; outside a table, M-j still moves subtrees, TAB still folds
    (is string= (lines "* B" "* A" "") (remove #\@ (org-table-after "M-j" (lines "@* A" "* B" ""))))
    ;; one u undoes TAB's align and new row
    (is string= (lines "| a | b" "") (remove #\@ (org-table-after "i TAB C-g u" (lines "| a | b@" ""))))))

(define-test (org table-formulas)
  ;; C-c * works the formulas out as Org (and its Calc) does: every case of test/org/formulas.sexp
  (let ((cases (with-open-file (in (asdf:system-relative-pathname :tuatara "test/org/formulas.sexp") :external-format :utf-8)
                 (let ((*read-eval* nil)) (loop for form = (read in nil in) until (eq form in) collect form)))))
    (dolist (case cases)
      (destructuring-bind (name table expected) case
        (is string= expected (org-text-after "C-c *" table) "~a" name))))
  (flet ((lines (&rest lines) (format nil "~{~a~^~%~}" lines)))
    ;; C-c C-c on the #+TBLFM: line too
    (is string= (lines "| 1 | 2 | 3 |" "#+TBLFM: $3=$1+$2")
        (remove #\@ (org-table-after "C-c C-c" (lines "| 1 | 2 |   |" "#+T@BLFM: $3=$1+$2"))))
    ;; =formula typed in a field, then TAB: a column formula, stored and worked out; :=, the field's own
    (is string= (lines "| 1 | 2 |" "| 3 | 6 |" "#+TBLFM: $2=$1*2" "")
        (remove #\@ (org-table-after "i = $ 1 * 2 TAB" (lines "| 1 | @ |" "| 3 |   |" ""))))
    (is string= (lines "| 1 |   |" "| 3 | 4 |" "| @  |   |" "#+TBLFM: $3=$1::@2$2=$1+1" "")
        (org-table-after "i : = $ 1 + 1 TAB" (lines "| 1 |   |" "| 3 | @ |" "#+TBLFM: $3=$1" "")))
    ;; C-c = asks for the column's formula
    (is string= (lines "| 2 | 4 |" "#+TBLFM: $2=$1^2" "")
        (remove #\@ (org-table-after "C-c = $ 1 ^ 2 RET" (lines "| 2 | @ |" ""))))
    ;; a reference outside the table: said, and nothing changed
    (let ((f (run-keys "M-x o r g - m o d e RET C-c *" :text (lines "| 1 | |" "#+TBLFM: $2=@-1$1") :width 80 :height 12)))
      (true (tuatara.frontend.fake:fake-frame-showing f "Row descriptor -1 leads outside the table")))
    (is string= (lines "| 1 | |" "#+TBLFM: $2=@-1$1") (org-text-after "C-c *" (lines "| 1 | |" "#+TBLFM: $2=@-1$1")))
    ;; formulas in Emacs Lisp are never run; the others still are
    (multiple-value-bind (f editor)
        (run-keys "M-x o r g - m o d e RET C-c *" :text (lines "| 1 | | |" "#+TBLFM: $2='(+ 1 $1)::$3=$1+1") :width 120 :height 12)
      (true (tuatara.frontend.fake:fake-frame-showing f "not run"))
      (is string= (lines "| 1 |   | 2 |" "#+TBLFM: $2='(+ 1 $1)::$3=$1+1") (session-text editor)))
    ;; one u undoes it all
    (is string= (lines "| 1 | |" "#+TBLFM: $2=$1+1") (org-text-after "C-c * u" (lines "| 1 | |" "#+TBLFM: $2=$1+1")))
    ;; numbers too large to work out, from a file not trusted: #ERROR at once, never a hang
    (dolist (table (list (lines "| 1e30000000 | |" "#+TBLFM: $2=$1+1") (lines "| 2 | |" "#+TBLFM: $2=$1^999999999")
                         (lines "| 1 | |" "#+TBLFM: $2=$1;%999999999d") (lines "| 1 | |" "#+TBLFM: $2=$1;%.9999999f")
                         (lines "| 1.5 | |" "#+TBLFM: $2=round($1,99999999)") (lines "| 1.5 | |" "#+TBLFM: $2=$1;f99999999")
                         ;; past what the machine's numbers hold
                         (lines "| 1000 | |" "#+TBLFM: $2=exp($1)") (lines (format nil "| ~a | |" (make-string 400 :initial-element #\9)) "#+TBLFM: $2=sqrt($1)")))
      (let ((start (thread-cpu-time)))
        (true (search "#ERROR" (org-text-after "C-c *" table)) "~s" table)
        (true (< (- (thread-cpu-time) start) (* 2 internal-time-units-per-second)) "~s at once" table)))
    ;; a formula nested past reason: refused in words, never the stack exhausted
    (let ((text (org-text-after "C-c *" (lines "| 1 | |" (format nil "#+TBLFM: $2=~a$1~a" (make-string 20000 :initial-element #\() (make-string 20000 :initial-element #\)))))))
      (false (search "| 1 | 1 |" text))))
  ;; a table of 20,000 rows is worked out at once, not in time growing faster than its rows
  (let* ((rows (append (list (list "a" "b" "c") :rule)
                       (loop for i below 20000 collect (list (princ-to-string i) (princ-to-string (* 2 i)) ""))))
         (model (tuatara.defaults::make-org-table-model :rows rows :indent "" :row 0 :field 0 :offset 0))
         (start (thread-cpu-time)))
    (tuatara.defaults::org-apply-formulas
     model (tuatara.defaults::org-runnable-formulas (list "$3=$1*$2+@-1$1" "@>$1=vsum(@I..@>>)")))
    (true (< (- (thread-cpu-time) start) internal-time-units-per-second))
    (is string= "799940000" (third (car (last (tuatara.defaults::org-table-rows model)))))))

(defparameter *org-tangled*
  (format nil "#+PROPERTY: header-args:sh :tangle out.sh
#+PROPERTY: header-args:emacs-lisp :tangle yes

* One
#+begin_src sh
echo one
  indented
#+end_src

#+begin_src emacs-lisp
(setq a 1)
#+end_src

  #+begin_src sh
    echo two
      deeper
  #+end_src

#+begin_src python :tangle no
print(1)
#+end_src

* Two
:PROPERTIES:
:header-args:sh: :tangle other.sh
:END:
#+begin_src sh :shebang \"#!/bin/bash\"
echo three
#+end_src

#+NAME: greeting
#+begin_src text
hello
#+end_src

#+begin_src python :tangle py/main.py :mkdirp yes :noweb yes
print(\"<<greeting>>\")
<<greeting()>>
,* not a headline
#+end_src

#+begin_src conf :tangle out.conf :padline no
a=1
#+end_src
#+begin_src conf :tangle out.conf :padline no
b=2
#+end_src
#+begin_src bash :tangle nope/x.sh
echo
#+end_src
#+begin_src bash :tangle made/y.sh :mkdirp yes

   echo made   
  
#+end_src
* COMMENT Left out
#+begin_src sh :tangle out.sh
echo commented
#+end_src
")
  "An Org file whose source blocks write code files, as tested against Org 9.7.11's org-babel-tangle.")

(define-test (org tangle-code-files)
  (with-test-directory (dir)
    (let ((org (merge-pathnames "t.org" dir)))
      (alexandria:write-string-into-file *org-tangled* org)
      (flet ((session (keys) (run-keys (format nil "C-x C-f C-a C-k ~a RET ~a" (rimu-typed (namestring org)) keys) :width 120 :height 20))
             (file (name) (let ((path (merge-pathnames name dir)))
                            (and (probe-file path) (alexandria:read-file-into-string path))))
             (mode (name) (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (merge-pathnames name dir)))))))
        ;; n: the files listed, nothing written
        (let ((f (session "C-c C-v t n RET")))
          (true (tuatara.frontend.fake:fake-frame-showing f "new" "2 blocks" "out.sh"))
          (true (tuatara.frontend.fake:fake-frame-showing f "other.sh" "executable"))
          (true (tuatara.frontend.fake:fake-frame-showing f "cannot" "nope/x.sh"))
          (true (tuatara.frontend.fake:fake-frame-showing f "its folder does not exist"))
          (true (tuatara.frontend.fake:fake-frame-showing f "new" "folder made" "made/y.sh"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Write 6 files?"))
          (true (tuatara.frontend.fake:fake-frame-showing f "Nothing written")))
        (false (file "out.sh"))
        ;; y: written as Org writes them
        (session "C-c C-v t y RET")
        (is string= (format nil "echo one~%  indented~%~%echo two~%  deeper~%") (file "out.sh"))
        (is string= (format nil "#!/bin/bash~%echo three~%") (file "other.sh"))
        (is = #o111 (logand #o111 (mode "other.sh")))
        (is string= (format nil "a=1~%b=2~%") (file "out.conf"))
        (is string= (format nil "(setq a 1)~%") (file "t.el"))
        ;; noweb put in; <<name()>>, which would run the block, left as it is
        (is string= (format nil "print(\"hello\")~%<<greeting()>>~%* not a headline~%") (file "py/main.py"))
        (false (file "nope/x.sh"))
        ;; its folder made; the white space around the code trimmed, as Org trims it
        (is string= (format nil "echo made~%") (file "made/y.sh"))
        ;; again: all the same, nothing to write
        (true (tuatara.frontend.fake:fake-frame-showing (session "C-c C-v t") "Nothing to write"))
        ;; outside the Org file's folder, said
        (let ((open (tuatara.buffer:find-buffer "t.org"))) (when open (tuatara.buffer:kill-buffer open)))
        (alexandria:write-string-into-file (format nil "#+begin_src sh :tangle ../outside-~a.sh~%echo~%#+end_src~%" (random 1000000)) org
                                           :if-exists :supersede)
        (let ((f (session "C-c C-v t n RET")))
          (true (tuatara.frontend.fake:fake-frame-showing f "outside the Org file's folder"))
          (true (tuatara.frontend.fake:fake-frame-showing f "(1 outside the Org file's folder)")))
        ;; a link in the folder, to a file outside it: where it lands shown, and outside; a mode with setuid
        ;; written without it; a block referring to itself left as it is
        (let ((open (tuatara.buffer:find-buffer "t.org"))) (when open (tuatara.buffer:kill-buffer open)))
        (let ((target (merge-pathnames (format nil "../elsewhere-~d/rc" (random 1000000)) dir)))
          (ensure-directories-exist target)
          (alexandria:write-string-into-file "mine" target)
          (sb-posix:symlink (namestring target) (namestring (merge-pathnames "linked.sh" dir)))
          (alexandria:write-string-into-file
           (format nil "#+begin_src sh :tangle linked.sh~%echo~%#+end_src~%#+begin_src sh :tangle setuid.sh :tangle-mode o4777~%echo~%#+end_src~%#+name: a~%#+begin_src sh :tangle loop.sh :noweb yes~%<<a>>~%<<a>>~%#+end_src~%")
           org :if-exists :supersede)
          (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET C-c C-v t n RET" (rimu-typed (namestring org))) :width 400 :height 20)))
            (true (tuatara.frontend.fake:fake-frame-showing f "linked.sh" "→" "/rc" "outside the Org file's folder"))
            (true (tuatara.frontend.fake:fake-frame-showing f "(1 outside the Org file's folder)")))
          ;; what was written, kept in *Messages*, says where a link led
          (let ((open (tuatara.buffer:find-buffer "t.org"))) (when open (tuatara.buffer:kill-buffer open)))
          (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET C-c C-v t y RET" (rimu-typed (namestring org))) :width 400 :height 20)))
            (true (tuatara.frontend.fake:fake-frame-showing f "Wrote 3 files:" "linked.sh → "))
            (true (search "linked.sh → " (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Messages*")))))
          (alexandria:write-string-into-file "mine" target :if-exists :supersede)
          (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET C-c C-v t n RET" (rimu-typed (namestring org))) :width 400 :height 20)))
            (true (tuatara.frontend.fake:fake-frame-showing f "setuid.sh"))
            (true (tuatara.frontend.fake:fake-frame-showing f "mode 777")))
          (let ((open (tuatara.buffer:find-buffer "t.org"))) (when open (tuatara.buffer:kill-buffer open)))
          (session "C-c C-v t y RET")
          (is = #o777 (logand #o7777 (sb-posix:stat-mode (sb-posix:stat (namestring (merge-pathnames "setuid.sh" dir))))))
          (is string= (format nil "<<a>>~%<<a>>~%") (file "loop.sh"))
          (uiop:delete-directory-tree (uiop:pathname-directory-pathname (truename target)) :validate t))))
    (dolist (buffer (copy-list tuatara.buffer:*buffers*))
      (when (equal "t.org" (tuatara.buffer:buffer-name buffer))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (org tangle-code-files-link-swapped)
  ;; a link put in a code file's place after it was looked at, while tangle-code-files asks, is
  ;; never followed: the file it leads to is left as it was
  (with-test-directory (dir)
    (let ((decoy (namestring (merge-pathnames "decoy" dir)))
          (made (namestring (merge-pathnames "made.sh" dir)))
          (kept (namestring (merge-pathnames "kept.sh" dir))))
      (alexandria:write-string-into-file "mine" decoy)
      (alexandria:write-string-into-file "old" kept)
      (flet ((checked (path text &optional mode)
               (tuatara.defaults::check-code-file (tuatara.defaults::make-code-file :path path :text text :mode mode))))
        ;; new when looked at, a link there when written
        (let ((file (checked made "echo")))
          (is eq :new (tuatara.defaults::code-file-state file))
          (sb-posix:symlink decoy made)
          (true (tuatara.defaults::tangle-write-code-file file))
          (is string= "mine" (alexandria:read-file-into-string decoy))
          (true (sb-posix:readlink made)))
        ;; a file when looked at, swapped for a link before it is written
        (let ((file (checked kept "new")))
          (is eq :changed (tuatara.defaults::code-file-state file))
          (sb-posix:unlink kept)
          (sb-posix:symlink decoy kept)
          (true (tuatara.defaults::tangle-write-code-file file))
          (is string= "mine" (alexandria:read-file-into-string decoy)))
        ;; as it was looked at: written, its mode given
        (sb-posix:unlink kept)
        (alexandria:write-string-into-file "old" kept)
        (let ((file (checked kept (format nil "echo~%") :executable)))
          (false (tuatara.defaults::tangle-write-code-file file))
          (is string= (format nil "echo~%") (alexandria:read-file-into-string kept))
          (is = #o111 (logand #o111 (sb-posix:stat-mode (sb-posix:lstat kept)))))))))

(define-test (org tangle-report-names-each-file-written)
  ;; x.sh written and x.sh.bak not: x.sh said to be written, though its name begins the other's
  (with-test-directory (dir)
    (let ((org (merge-pathnames "t.org" dir)))
      (alexandria:write-string-into-file
       (format nil "#+begin_src sh :tangle x.sh~%echo x~%#+end_src~%#+begin_src sh :tangle x.sh.bak~%echo bak~%#+end_src~%") org)
      (sb-int:encapsulate 'tuatara.defaults::tangle-write-code-file 'refuse-bak
                          (lambda (function file)
                            (if (uiop:string-suffix-p (tuatara.defaults::code-file-path file) ".bak")
                                "refused for the test"
                                (funcall function file))))
      (unwind-protect
           (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET C-c C-v t y RET" (rimu-typed (namestring org))) :width 200 :height 20)))
             (true (tuatara.frontend.fake:fake-frame-showing f "Wrote 1 of 2 (" "x.sh); not written:" "x.sh.bak: refused for the test")))
        (sb-int:unencapsulate 'tuatara.defaults::tangle-write-code-file 'refuse-bak)))))
