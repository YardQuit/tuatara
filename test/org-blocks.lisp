;;;; test/org-blocks.lisp --- Org's dynamic blocks, tables of contents and formatted blocks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test (org toc-as-toc-org)
  ;; each file of test/org/toc/, Org's and Markdown's, given its table of contents as Emacs's toc-org gives it
  ;; (tools/toc-oracle.el)
  (true (directory (merge-pathnames "test/org/toc/*.org" (asdf:system-source-directory :tuatara))))
  (true (directory (merge-pathnames "test/org/toc/*.md" (asdf:system-source-directory :tuatara))))
  (dolist (path (append (directory (merge-pathnames "test/org/toc/*.org" (asdf:system-source-directory :tuatara)))
                        (directory (merge-pathnames "test/org/toc/*.md" (asdf:system-source-directory :tuatara)))))
    (let* ((markdown (string-equal (pathname-type path) "md"))
           (text (alexandria:read-file-into-string path :external-format :utf-8))
           (expected (alexandria:read-file-into-string (make-pathname :type "expected" :defaults path) :external-format :utf-8))
           (made (multiple-value-bind (start end replacement) (tuatara.defaults::toc-change text :markdown markdown)
                   (if (integerp start) (concatenate 'string (subseq text 0 start) replacement (subseq text end)) text))))
      (is string= expected made "~a" (file-namestring path))
      ;; and made again, it stays as it is
      (is eq (if (string= expected text) (tuatara.defaults::toc-change text :markdown markdown) :same)
          (let ((again (tuatara.defaults::toc-change made :markdown markdown))) (if (integerp again) :changed again))
          "~a again" (file-namestring path)))))

(define-test (org toc-leaves-out-standard-keywords)
  ;; WAIT and CANCELLED, standard keywords, are left out of a table of contents as TODO and DONE are
  (let ((replacement (nth-value 2 (tuatara.defaults::toc-change (format nil "* Contents :TOC:~%* WAIT Call~%* CANCELLED Fence~%")))))
    (true (search "Call" replacement) "~s" replacement)
    (true (search "Fence" replacement) "~s" replacement)
    (false (search "WAIT" replacement) "~s" replacement)
    (false (search "CANCELLED" replacement) "~s" replacement)))

(define-test (org toc-in-markdown)
  ;; saving a Markdown file makes its table again; its links lead to their headings, RET and C-c C-o
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "readme.md" dir))))
      (alexandria:write-string-into-file (format nil "# Readme~%## Contents <!-- :TOC: -->~%## Install~%Text.~%## Use~%Words.~%") path)
      ;; changed, then saved: an unchanged buffer is not saved, as in Emacs
      (progn (run-keys "A ! C-g C-x C-s" :file path :width 80 :height 12)
        (is string= (format nil "# Readme!~%## Contents <!-- :TOC: -->~%- [Readme!](#readme)~%  - [Install](#install)~%  - [Use](#use)~%~%## Install~%Text.~%## Use~%Words.~%")
            (alexandria:read-file-into-string path))
        (forget-file-buffers))
      ;; RET in NORMAL state on [Use](#use), C-c C-o on [Install](#install)
      (let ((editor (nth-value 1 (run-keys "j j j j l l l l l RET" :file path :width 80 :height 12))))
        (is = (search "## Use" (session-text editor)) (session-point editor)))
      (forget-file-buffers)
      (let ((editor (nth-value 1 (run-keys "j j j l l l l l C-c C-o" :file path :width 80 :height 12))))
        (is = (search "## Install" (session-text editor)) (session-point editor)))
      (forget-file-buffers)
      ;; off: saved as it is
      (let ((tuatara-user:*markdown-toc-on-save* nil))
        (alexandria:write-string-into-file (format nil "# A~%## C :TOC:~%## B~%") path :if-exists :supersede)
        (run-keys "A ! C-g C-x C-s" :file path :width 80 :height 12)
        (forget-file-buffers)
        (is string= (format nil "# A!~%## C :TOC:~%## B~%") (alexandria:read-file-into-string path)))))
  ;; a link's place on its line, label and target
  (is equal '("Use" "#use") (multiple-value-list (tuatara.defaults::markdown-link-at "- [Use](#use) x" 4)))
  (false (tuatara.defaults::markdown-link-at "- [Use](#use) x" 14))
  (is equal '("pic" "a.png") (multiple-value-list (tuatara.defaults::markdown-link-at "![pic](a.png \"title\")" 2))))

(define-test (org toc-kept-and-followed)
  (with-test-directory (dir)
    (let ((file (merge-pathnames "toc.org" dir)))
      (flet ((in-file (keys)
               (let ((b (tuatara.buffer:find-buffer "toc.org")))
                 (when b (tuatara.buffer:buffer-mark-saved b) (tuatara.buffer:kill-buffer b)))
               (nth-value 1 (run-keys (format nil "C-x C-f C-a C-k ~a RET ~a" (rimu-typed (namestring file)) keys)
                                      :width 120 :height 20)))
             (point-line (editor)
               (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (tuatara.buffer:buffer-string b :start (tuatara.buffer:buffer-line-start b (tuatara.buffer:buffer-point b))
                                                 :end (tuatara.buffer:buffer-line-end b (tuatara.buffer:buffer-point b))))))
        ;; saving makes the table again, under the headline tagged :TOC:
        (alexandria:write-string-into-file (format nil "* Contents :TOC:~%~%* Same~%* Same~%** Child~%* Other [1/2]~%") file)
        (in-file "g e O * SPC N e w ESC C-x C-s")
        (is string= (format nil "* Contents :TOC:~%- [[#same][Same]]~%- [[#same-1][Same]]~%  - [[#child][Child]]~%- [[#other-12][Other]]~%- [[#new][New]]~%~%* Same~%* Same~%** Child~%* Other [1/2]~%* New~%")
            (alexandria:read-file-into-string file))
        ;; its links lead to their headlines, the second of two of a title to the second
        (is string= "* Same" (point-line (in-file "j j l l l RET")))
        (is = 8 (let ((editor (in-file "j j l l l RET")))
                  (tuatara.buffer:buffer-line-number (tuatara.display:window-buffer (tuatara.command:editor-window editor))
                                                     (session-point editor))))
        (is string= "* Other [1/2]" (point-line (in-file "j j j j l l l RET")))
        ;; the titles style, :TOC_2_org:, leads by the title as the table lists it
        (alexandria:write-string-into-file (format nil "* Contents :TOC_2_org:~%~%* Other [1/2]~%") file :if-exists :supersede)
        (in-file "g e O * SPC N e w ESC C-x C-s")
        (true (search "- [[Other][Other]]" (alexandria:read-file-into-string file)))
        (is string= "* Other [1/2]" (point-line (in-file "j l l l RET")))
        ;; lines ending with CR LF: the table's too
        (alexandria:write-string-into-file (format nil "* Contents :TOC:~c~%~c~%* A~c~%" #\Return #\Return #\Return) file :if-exists :supersede)
        (in-file "g e O * SPC B ESC C-x C-s")
        (is string= (format nil "* Contents :TOC:~c~%- [[#a][A]]~c~%- [[#b][B]]~c~%~c~%* A~c~%* B~c~%"
                            #\Return #\Return #\Return #\Return #\Return #\Return)
            (alexandria:read-file-into-string file))
        ;; with *org-toc-on-save* nil, saving leaves it; M-x toc-org-insert-toc makes it
        (alexandria:write-string-into-file (format nil "* Contents :TOC:~%~%* A~%") file :if-exists :supersede)
        (let ((tuatara-user:*org-toc-on-save* nil))
          (in-file "g e O * SPC B ESC C-x C-s")
          (false (search "[[#a]" (alexandria:read-file-into-string file)))
          (in-file "M-x t o c - o r g - i n s e r t - t o c RET C-x C-s")
          (true (search "- [[#a][A]]" (alexandria:read-file-into-string file))))
        (let ((b (tuatara.buffer:find-buffer "toc.org")))
          (when b (tuatara.buffer:buffer-mark-saved b) (tuatara.buffer:kill-buffer b)))))))

(defun dblock-masked (text)
  "TEXT with the time its clock tables were made at left out, in any language."
  (cl-ppcre:regex-replace-all "(?m)^(#\\+CAPTION: [^[\\n]*)\\[[^]]*\\]" text "\\1[...]"))

(define-test (org dblocks-as-org)
  ;; each file of test/org/dblock/ with its clock tables made as Org makes them (tools/dblock-oracle.el):
  ;; read in that folder, the agenda the files of multi/ there
  (let* ((folder (merge-pathnames "test/org/dblock/" (asdf:system-source-directory :tuatara)))
         (tuatara.defaults::*org-now* (encode-universal-time 0 40 15 2 10 2026))
         (tuatara-user:*default-directory* (tuatara.io:native folder))
         (tuatara-user:*org-agenda-files* (list (concatenate 'string (tuatara.io:native folder) "multi/"))))
    (true (directory (merge-pathnames "test/org/dblock/*.org" (asdf:system-source-directory :tuatara))))
    (dolist (path (directory (merge-pathnames "test/org/dblock/*.org" (asdf:system-source-directory :tuatara))))
      (let* ((text (alexandria:read-file-into-string path :external-format :utf-8))
             (expected (alexandria:read-file-into-string (make-pathname :type "expected" :defaults path) :external-format :utf-8)))
        (is string= (dblock-masked expected) (dblock-masked (org-text-after "M-x o r g - u p d a t e - a l l - d b l o c k s RET" text))
            "~a" (file-namestring path))))))

(define-test (org dblocks-step-needs-times)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 40 15 2 10 2026))
        (text (format nil "* A~%:LOGBOOK:~%CLOCK: [2026-10-02 Fri 08:00]--[2026-10-02 Fri 09:00] =>  1:00~%:END:~%#+BEGIN: clocktable :step day~%old~%#+END:~%")))
    ;; a step with neither a block nor both times leaves the block as it was, and says so
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-u 4 j C-c C-x C-u" :text text :width 100 :height 12)
      (is string= text (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "needs a :block")))
    (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-u 4 j C-c C-x C-u" :text (cl-ppcre:regex-replace ":step day" text ":step fortnight :block today") :width 100 :height 12)
      (is string= (cl-ppcre:regex-replace ":step day" text ":step fortnight :block today") (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "not one Org knows")))
    ;; (2 . ?t) and \\n read as Emacs reads them
    (is equal (list :sort (cons 2 #\t) :header (format nil "x~%y"))
        (tuatara.defaults::dblock-read-params ":sort (2 . ?t) :header \"x\\ny\""))))

(define-test (org blocks-read-nothing-they-should-not)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 40 15 2 10 2026)))
    ;; a glob matched in a pass: a pattern that would take for ever as a regular expression ends at once
    (true (tuatara.defaults::glob-match "*.org" "notes.org"))
    (false (tuatara.defaults::glob-match "*.org" "notes.txt"))
    (true (tuatara.defaults::glob-match "a?c*" "abcdef"))
    (false (tuatara.defaults::glob-match "a?c" "abcd"))
    (true (tuatara.defaults::glob-match "**x" "aaaax"))
    (true (tuatara.defaults::glob-match "" ""))
    (let ((start (thread-cpu-time)))
      (false (tuatara.defaults::glob-match (format nil "~30@{*~}a" nil) (make-string 2000 :initial-element #\b)))
      (true (< (- (thread-cpu-time) start) internal-time-units-per-second)))
    ;; a block's :scope names only agenda files or files beside its own: another is left out, and said
    (let ((text (format nil "* A~%:LOGBOOK:~%CLOCK: [2026-10-02 Fri 08:00]--[2026-10-02 Fri 09:00] =>  1:00~%:END:~%#+BEGIN: clocktable :scope (\"/dev/zero\" \"/etc/hostname\")~%#+END:~%")))
      (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-u 4 j C-c C-c" :text text :width 120 :height 12)
        (true (search "*Total time*" (session-text editor)))
        (false (search "*File time*" (session-text editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "/etc/hostname out"))))
    ;; a pattern in a block's :match is not run
    (let ((text (format nil "* A :work:~%:LOGBOOK:~%CLOCK: [2026-10-02 Fri 08:00]--[2026-10-02 Fri 09:00] =>  1:00~%:END:~%#+BEGIN: clocktable :match \"{(a*)*b}\"~%#+END:~%")))
      (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-u 4 j C-c C-c" :text text :width 120 :height 12)
        (true (search "| A " (session-text editor)))
        (true (search "1:00" (session-text editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "{pattern}"))))
    ;; steps that would go on for ever stop, and say so; a block Org does not know leaves the block as it is
    (let ((text (format nil "* A~%:LOGBOOK:~%CLOCK: [2026-10-02 Fri 08:00]--[2026-10-02 Fri 09:00] =>  1:00~%:END:~%#+BEGIN: clocktable :step day :tstart \"<1900-01-01 Mon>\" :tend \"<2030-01-01 Tue>\" :stepskip0 t~%#+END:~%")))
      (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-u 4 j C-c C-c" :text text :width 120 :height 12)
        (declare (ignore editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "stops at 1000 steps"))))
    (let ((text (format nil "* A~%#+BEGIN: clocktable :block thiswek~%old~%#+END:~%")))
      (is string= text (org-text-after "C-u 1 j C-c C-c" text)))
    (let ((text (format nil "* A~%#+BEGIN: clocktable :step week :wstart 7 :block thismonth~%old~%#+END:~%")))
      (true (search "Weekly report" (org-text-after "C-u 1 j C-c C-c" text))))
    ;; numbers and keywords of a size no file gives
    (is equal (list :maxlevel "123456789012345678901234567890") (tuatara.defaults::dblock-read-params ":maxlevel 123456789012345678901234567890"))))

(define-test (org toc-tags-read-in-a-pass)
  (is equal '("TOC" "x") (tuatara.defaults::toc-line-tags "* Contents :TOC:x:"))
  (is equal '("toc_3_org") (tuatara.defaults::toc-line-tags "* Contents <-- :toc_3_org: -->"))
  (is equal '("toc") (tuatara.defaults::toc-line-tags "* Contents x:toc:"))
  (false (tuatara.defaults::toc-line-tags "* Contents :toc"))
  (is string= "TOC_3_org" (tuatara.defaults::toc-tag-of "* C :a:TOC_3_org:b:"))
  (false (tuatara.defaults::toc-tag-of "* C :tocx:"))
  (is string= "* Head" (tuatara.defaults::toc-without-tags "* Head  :a:b:  "))
  (is string= "* Head" (tuatara.defaults::toc-without-tags "* Head:a:"))
  (is string= "* Head :a" (tuatara.defaults::toc-without-tags "* Head :a"))
  (is string= "* Head" (tuatara.defaults::toc-without-tags "* Head ::"))
  ;; a line of a million colons is looked at once
  (let ((start (thread-cpu-time)))
    (tuatara.defaults::toc-without-tags (concatenate 'string "* " (make-string 1000000 :initial-element #\:) "x"))
    (true (< (- (thread-cpu-time) start) internal-time-units-per-second))))

(define-test (org clocktable-archives-and-returns)
  (let* ((folder (tuatara.io:native (merge-pathnames "test/org/dblock/" (asdf:system-source-directory :tuatara))))
         (tuatara.defaults::*org-now* (encode-universal-time 0 40 15 2 10 2026))
         (tuatara-user:*default-directory* folder)
         (tuatara-user:*org-agenda-files* (list (concatenate 'string folder "multi/")))
         (a (concatenate 'string folder "multi/a.org")))
    ;; an agenda file's archive, found as Org finds it: NAME_archive beside it, or what #+ARCHIVE: names; beside it only
    (is equal (list (concatenate 'string folder "multi/a.org_archive")) (tuatara.defaults::org-archive-files-of a "* x"))
    (is equal '() (tuatara.defaults::org-archive-files-of a (format nil "#+ARCHIVE: /etc/hostname::~%* x~%")))
    (is equal (list (concatenate 'string folder "multi/b.org")) (tuatara.defaults::org-archive-files-of a (format nil "#+ARCHIVE: b.org::* Done~%")))
    ;; its time among the agenda's files
    (let ((new (org-text-after "SPC O b" (format nil "#+BEGIN: clocktable :scope agenda-with-archives :maxlevel 1 :fileskip0 t~%#+END:~%"))))
      (true (search "| a.org_archive " new))
      (true (search "Archived task" new))
      (true (search "0:40" new)))
    ;; a formatter of your own, and one that is not there
    (tuatara-user:define-clocktable-formatter "mine" (tables params)
      (format nil "~d file~:p, ~d minutes, ~a~%" (length tables) (reduce #'+ tables :key #'second) (getf params :maxlevel)))
    (let ((text (format nil "* A~%:LOGBOOK:~%CLOCK: [2026-10-02 Fri 08:00]--[2026-10-02 Fri 09:30] =>  1:30~%:END:~%#+BEGIN: clocktable :formatter \"mine\" :maxlevel 3~%#+END:~%")))
      (true (search (format nil "~%1 file, 90 minutes, 3~%") (org-text-after "SPC O b" text)))
      (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-u 4 j C-c C-c" :text (cl-ppcre:regex-replace "mine" text "theirs") :width 100 :height 12)
        (true (search "*Total time*" (session-text editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "No clock table formatter is named theirs")))))
  ;; the carriage returns found in the text itself, as the stripped text has them, a chunk's edge between a return and its newline too
  (dolist (text (list (format nil "a~C~%b~C~%" #\Return #\Return) (format nil "é中~C~%x~Cy~%~C" #\Return #\Return #\Return)
                      (format nil "~C~%~C~%" #\Return #\Return) "plain" (format nil "end~C" #\Return)
                      (concatenate 'string (make-string 1048575 :initial-element #\a) (string #\Return) (string #\Newline) "b")
                      (concatenate 'string (make-string 1048574 :initial-element #\é) (string #\Return) (string #\Return) (string #\Newline))))
    (is equalp (nth-value 1 (tuatara.defaults::org-strip-returns text))
        (tuatara.defaults::org-returns-in-rope (tuatara.text:string->rope text)))))

(define-test (org dblocks-kept)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 40 15 2 10 2026))
        (text (format nil "* A~%:LOGBOOK:~%CLOCK: [2026-10-02 Fri 08:00]--[2026-10-02 Fri 09:00] =>  1:00~%:END:~%#+BEGIN: clocktable :block today~%#+END:~%")))
    ;; C-c C-c on its #+BEGIN: line, C-c C-x C-u anywhere in it, SPC O b for all
    (dolist (keys '("C-u 4 j C-c C-c" "C-u 5 j C-c C-x C-u" "SPC O b"))
      (true (search "| A            | 1:00   |" (org-text-after keys text)) "~a" keys))
    ;; point after the block stays on its text; all blocks made again are one step to undo
    (let ((two (format nil "~a~%#+BEGIN: clocktable :block today~%#+END:~%after~%" text)))
      (let ((editor (nth-value 1 (run-keys "M-x o r g - m o d e RET M-> SPC O b" :text two :width 100 :height 20))))
        (let ((new (session-text editor)))
          (is = 2 (count "| A            | 1:00   |" (cl-ppcre:split "\\n" new) :test #'string=))
          (is = (length new) (session-point editor))))
      (is string= two (session-text (nth-value 1 (run-keys-in-turns '("M-x o r g - m o d e RET SPC O b" "u") :text two :width 60 :height 12)))))
    ;; a writer of one's own, its parameters read as data
    (tuatara-user:define-dblock-writer "hello" (params) (format nil "Hello, ~a ~s~%" (getf params :who) (getf params :n)))
    (is string= (format nil "#+BEGIN: hello :who \"you\" :n 3~%Hello, you 3~%#+END:~%")
        (org-text-after "C-c C-c" (format nil "#+BEGIN: hello :who \"you\" :n 3~%#+END:~%")))
    ;; no writer of that name: said, the block left as it is
    (is string= (format nil "#+BEGIN: nosuch~%old~%#+END:~%") (org-text-after "C-c C-c" (format nil "#+BEGIN: nosuch~%old~%#+END:~%")))
    ;; what a file writes after a block's name is never run: #. and (lisp forms) are words and lists
    (is equal '(:a "#." ("error") :b ("x" 1) :c "file") (tuatara.defaults::dblock-read-params ":a #.(error) :b (\"x\" 1) :c file"))
    (remhash "hello" tuatara.defaults::*dblock-writers*)))

(defun formatted-notes-folder ()
  "The notes of test/org/formatted/notes/, as a native folder name ending with a slash."
  (tuatara.io:native (merge-pathnames "test/org/formatted/notes/" (asdf:system-source-directory :tuatara))))

(define-test (org formatted-blocks-as-denote)
  ;; each note of test/org/formatted/notes/ named --case- with its blocks made as denote-org makes
  ;; them (tools/formatted-oracle.el), the notes there the notes directory
  (let ((tuatara-user:*org-tags-column* 0)
        (tuatara.defaults::*formatted-file-directory* (formatted-notes-folder))
        (cases (directory (merge-pathnames "test/org/formatted/notes/*--case-*.org" (asdf:system-source-directory :tuatara)))))
    (is = 6 (length cases))
    (unwind-protect
         (dolist (path cases)
           (let* ((name (second (cl-ppcre:split "--case-|__" (file-namestring path))))
                  (expected (alexandria:read-file-into-string
                             (merge-pathnames (format nil "test/org/formatted/expected/~a.org" name) (asdf:system-source-directory :tuatara))
                             :external-format :utf-8))
                  (editor (nth-value 1 (run-keys "SPC O b" :file (namestring path) :width 100 :height 20))))
             (is string= expected (session-text editor) "~a" name)
             (forget-file-buffers)))
      (forget-file-buffers))))

(define-test (org formatted-blocks-own)
  (let ((tuatara-user:*org-tags-column* 0)
        (tuatara.defaults::*formatted-file-directory* (formatted-notes-folder)))
    (flet ((made (first) (org-text-after "C-c C-c" (format nil "~a~%#+END:~%" first)))
           (links (text) (cl-ppcre:all-matches-as-strings "denote:[0-9T]+" text)))
      ;; Tuatara's names read their regular expressions as its searches do: groups, smart case
      (is equal '("denote:20260102T090000" "denote:20260103T090000")
          (links (made "#+BEGIN: formatted-links :regexp \"(beta|gamma)-\"")))
      (is equal '() (links (made "#+BEGIN: formatted-links :regexp \"_ALPHA\"")))
      (is = 4 (length (links (made "#+BEGIN: formatted-links :regexp \"_alpha\""))))
      ;; Denote's read them as Emacs writes them, case ignored
      (is = 4 (length (links (made "#+BEGIN: denote-links :regexp \"_ALPHA\""))))
      (is equal '() (links (made "#+BEGIN: denote-links :regexp \"(beta|gamma)-\"")))
      ;; a note that links to none of them misses them all with Tuatara's name; none with Denote's, as Denote has it
      (is = 4 (length (links (made "#+BEGIN: formatted-missing-links :regexp \"_alpha\""))))
      (is = 0 (length (links (made "#+BEGIN: denote-missing-links :regexp \"_alpha\""))))
      ;; what is no regular expression, or a list as Emacs's rx writes one, leaves the block as it was, and says so
      (dolist (first '("#+BEGIN: formatted-links :regexp \"(unclosed\"" "#+BEGIN: denote-links :regexp \"\\\\(unclosed\""
                       "#+BEGIN: denote-links :regexp (or \"a\" \"b\")"))
        (let ((text (format nil "~a~%old~%#+END:~%" first)))
          (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-c C-c" :text text :width 100 :height 12)
            (is string= text (session-text editor) "~a" first)
            (true (or (tuatara.frontend.fake:fake-frame-showing f "no regular expression")
                      (tuatara.frontend.fake:fake-frame-showing f "write it as a string"))
                  "~a" first))))
      ;; a buffer with no file has no backlinks: said, the block as it was
      (let ((text (format nil "#+BEGIN: formatted-backlinks~%old~%#+END:~%")))
        (multiple-value-bind (f editor) (run-keys "M-x o r g - m o d e RET C-c C-c" :text text :width 100 :height 12)
          (is string= text (session-text editor))
          (true (tuatara.frontend.fake:fake-frame-showing f "no identifier"))))
      ;; a sequence no file has: said, the block as it was
      (let ((text (format nil "#+BEGIN: formatted-sequence :sequence \"9\" :depth 2~%old~%#+END:~%")))
        (is string= text (org-text-after "C-c C-c" text)))
      ;; only notes' text is read into a block, never another kind of file
      (with-test-directory (dir)
        (let ((tuatara.defaults::*formatted-file-directory* (namestring dir)))
          (alexandria:write-string-into-file "#+title: A note" (merge-pathnames "20260101T090000--note__x.org" dir))
          (alexandria:write-string-into-file "binary" (merge-pathnames "20260101T090001--picture__x.png" dir))
          (let ((new (made "#+BEGIN: formatted-files :regexp \"_x\"")))
            (true (search "A note" new))
            (false (search "binary" new))))))))

(define-test (org emacs-regexes-translated)
  (flet ((tr (regex) (tuatara.defaults::org-regexp-to-ppcre regex))
         (matches (regex text) (and (cl-ppcre:scan (tuatara.defaults::org-regexp-to-ppcre regex) text) t)))
    (is string= "(beta|gamma)-" (tr "\\(beta\\|gamma\\)-"))
    (is string= "\\(a\\)\\|b" (tr "(a)|b"))
    (is string= "(?:x)" (tr "\\(?:x\\)"))
    (is string= "(x)" (tr "\\(?2:x\\)"))
    (is string= "a{2,3}" (tr "a\\{2,3\\}"))
    (is string= "\\Ab\\z" (tr "\\`b\\'"))
    (true (matches "^\\*" "*star"))
    (true (matches "a^b" "a^b"))
    (true (matches "x$" "x"))
    (true (matches "a$b" "a$b"))
    (true (matches "*a" "*a"))
    (true (matches "[]a]" "]"))
    (true (matches "[\\]" "\\"))
    (true (matches "[[:digit:]]+" "123"))
    (false (matches "^[[:digit:]]+$" "12a"))
    (true (matches "[[:alpha:]]" "é"))
    (true (matches "\\s." "a,b"))
    (false (tr "[[:nosuchclass:]]"))
    (true (matches "\\s-+" "  "))
    (true (matches "\\<word\\>" "a word here"))
    (false (matches "\\<word\\>" "swordfish"))
    (true (matches "\\(a\\)\\1" "aa"))
    (true (matches "\\." "."))
    (false (matches "\\." "x"))
    ;; what Tuatara cannot read so is refused, never guessed
    (false (tr "\\ca"))
    (false (tr "a\\"))
    (false (tr "\\(unclosed"))))

(defvar *slow-writer-go* nil
  "True once the slow writer of the tests may finish.")

(define-test (org dblocks-in-the-background)
  ;; a writer that takes its time: the editor goes on meanwhile, and the block is put in where it is then
  (tuatara-user:define-dblock-writer "slow" (params)
    (declare (ignore params))
    (loop until *slow-writer-go* do (tuatara.defaults::dblock-progress 1/2) (sleep 0.01))
    (format nil "made~%"))
  (flet ((session (text turns)
           ;; TURNS: keys, :go to let the writer finish, or a function called in its turn
           (setf *slow-writer-go* nil)
           (let ((f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 12)))
             (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
               (when scratch (tuatara.buffer:kill-buffer scratch)))
             (tuatara.buffer:make-buffer "*scratch*" :text text)
             (tuatara.frontend.fake:fake-queue-keys f "M-x o r g - m o d e RET")
             (dolist (turn turns)
               (cond ((eq turn :go)
                      (tuatara.frontend.fake:fake-queue-action f (lambda () (setf *slow-writer-go* t))))
                     ((functionp turn) (tuatara.frontend.fake:fake-queue-action f turn))
                     (t (tuatara.frontend.fake:fake-queue-keys f turn))))
             (let ((tuatara.defaults::*dblock-wait* 0))
               (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                       :buffer (tuatara.buffer:find-buffer "*scratch*")))))))
    (unwind-protect
         (let ((text (format nil "top~%#+BEGIN: slow~%old~%#+END:~%end~%")))
           ;; typed above the block while it is made: the block put in below what was typed, point kept on it
           (multiple-value-bind (f editor) (session text '("j C-c C-c" "k i n e w SPC C-g" :go))
             (is string= (format nil "new top~%#+BEGIN: slow~%made~%#+END:~%end~%") (session-text editor))
             (is = 4 (session-point editor))
             (true (processing-shown f)))
           ;; its #+BEGIN: line changed meanwhile, not by a command (a command asks first,
           ;; test/job-guards.lisp): left as it is, and said
           (multiple-value-bind (f editor)
               (session text (list "j C-c C-c"
                                   (lambda ()
                                     (let ((b (tuatara.buffer:find-buffer "*scratch*")))
                                       (tuatara.buffer:buffer-insert b (tuatara.buffer:buffer-line-end b 4) "x")))
                                   :go))
             (is string= (format nil "top~%#+BEGIN: slowx~%old~%#+END:~%end~%") (session-text editor))
             (true (tuatara.frontend.fake:fake-frame-showing f "changed while it was made")))
           ;; stopped from the list of jobs: the block as it was
           (setf *slow-writer-go* nil)
           (let ((f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 12)))
             (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
               (when scratch (tuatara.buffer:kill-buffer scratch)))
             (tuatara.buffer:make-buffer "*scratch*" :text text)
             (tuatara.frontend.fake:fake-queue-keys f "M-x o r g - m o d e RET j C-c C-c")
             (tuatara.frontend.fake:fake-queue-action
              f (lambda ()
                  (let ((job (find "*scratch*" (tuatara.defaults::running-jobs)
                                   :key (lambda (job) (tuatara.buffer:buffer-name (tuatara.runtime:job-buffer job)))
                                   :test #'string=)))
                    (true job)
                    (when job (tuatara.runtime:job-stop job)))))
             (let* ((editor (let ((tuatara.defaults::*dblock-wait* 0))
                              (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))
               (is string= text (session-text editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "Stopped: the dynamic blocks are as they were")))))
      (setf *slow-writer-go* t)
      (remhash "slow" tuatara.defaults::*dblock-writers*))))

(define-test (org dblocks-put-in)
  (let ((tuatara.defaults::*org-now* (encode-universal-time 0 40 15 2 10 2026))
        (tuatara.defaults::*formatted-file-directory* (formatted-notes-folder)))
    ;; C-c C-x x offers the kinds of block; a clock table of the subtree, two levels, as Org's org-clock-report
    (let ((new (org-text-after "C-u 3 j C-c C-x x c l o c k t a b l e RET"
                               (format nil "* A~%:LOGBOOK:~%CLOCK: [2026-10-02 Fri 08:00]--[2026-10-02 Fri 09:00] =>  1:00~%:END:~%"))))
      (true (search (format nil "#+BEGIN: clocktable :scope subtree :maxlevel 2~%#+CAPTION:") new))
      (true (search "| A            | 1:00   |" new)))
    ;; before the first headline, the file; on a line with text, the block on a line after it
    (true (search (format nil "intro~%#+BEGIN: clocktable :scope file :maxlevel 2~%")
                  (org-text-after "M-x o r g - c l o c k - r e p o r t RET" (format nil "intro~%* A~%"))))
    ;; links to the files a regular expression matches, the block as denote-org's commands write it
    (let ((new (org-text-after "M-x i n s e r t - f o r m a t t e d - l i n k s - b l o c k RET _ a l p h a RET" "")))
      (true (search "#+BEGIN: formatted-links :regexp \"_alpha\" :not-regexp nil :excluded-dirs-regexp nil :sort-by-component nil :reverse-sort nil :id-only nil :include-date nil" new))
      (true (search "- [[denote:20260101T090000][1  Alpha root]]" new)))
    ;; the files of a sequence, its file chosen and its levels given
    (let ((new (org-text-after "M-x i n s e r t - f o r m a t t e d - s e q u e n c e - b l o c k RET C-a C-k a l p h a RET C-a C-k 2 RET" "")))
      (true (search "#+BEGIN: formatted-sequence :sequence \"1\" :depth 2" new))
      (true (search "  - [[denote:20260102T090000][1=1: 1=1  Beta child]]" new)))))

(define-test (org source-block-at)
  ;; a source block from its #+begin_src line to its #+end_src line, as the parse has it
  (let* ((text (format nil "Before~%#+begin_src sh~%echo~%#+end_src~%~%After~%"))
         (tree (tuatara.defaults::org-parse text)))
    (false (tuatara.defaults::org-src-block-at tree 2))
    (true (tuatara.defaults::org-src-block-at tree (search "begin" text)))
    (true (tuatara.defaults::org-src-block-at tree (search "echo" text)))
    (true (tuatara.defaults::org-src-block-at tree (+ 3 (search "#+end_src" text))))
    (false (tuatara.defaults::org-src-block-at tree (search "After" text)))))

(define-test (org dblock-copies-no-large-edited-buffer)
  ;; an edited file mapped from disk, larger than a dynamic block reads: not copied when a block is asked for
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "big.csv" dir)))
           (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "big.csv") :text (make-string 5000 :initial-element #\x))))
      (setf (tuatara.buffer:buffer-file buffer) path
            (tuatara.buffer:buffer-mapping buffer) :mapped)
      (tuatara.buffer:buffer-mark-saved buffer -1)
      (unwind-protect
           (progn
             (let ((tuatara.defaults::*org-file-read-limit* 1000))
               (false (nth-value 1 (gethash path (tuatara.defaults::dblock-snapshot-sources)))))
             (is = 5000 (length (gethash path (tuatara.defaults::dblock-snapshot-sources)))))
        (setf (tuatara.buffer:buffer-mapping buffer) nil)
        (tuatara.buffer:kill-buffer buffer)))))
