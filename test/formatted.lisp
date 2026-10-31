;;;; test/formatted.lisp --- Files with formatted names, searching files, and going back a buffer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test formatted
  :description "Formatted names and headers as Denote makes them; SPC f, SPC d f and SPC d n make files so named; M-s g and SPC d g find text in files as you type; SPC b goes back a buffer; a new file named as encrypted is not saved.")

(defun local-time (year month day hour minute second)
  "The universal time of a moment of the local time zone."
  (encode-universal-time second minute hour day month year))

(defmacro with-formatted-directory ((directory) &body body)
  "Run BODY with DIRECTORY a new empty directory, the directory of the files
with formatted names, and the keywords garden and project known, as an init
file sets them."
  `(with-test-directory (,directory)
     (let ((tuatara-user:*formatted-file-directory* (namestring ,directory))
           (tuatara-user:*formatted-file-keywords* '("garden" "project")))
       ,@body)))

(defun buffer-named-like (prefix)
  "The live buffer whose name starts with PREFIX, or NIL."
  (find-if (lambda (b) (let ((name (tuatara.buffer:buffer-name b)))
                         (and (>= (length name) (length prefix)) (string= prefix name :end2 (length prefix)))))
           tuatara.buffer:*buffers*))

(defun kill-buffers-visiting (directory)
  "Kill the buffers visiting files in DIRECTORY."
  (let ((prefix (tuatara.io:native directory)))
    (dolist (b (copy-list tuatara.buffer:*buffers*))
      (let ((file (tuatara.buffer:buffer-file b)))
        (when (and file (search prefix (tuatara.io:native file)))
          (tuatara.buffer:kill-buffer b))))))

(define-test (formatted names-as-denote-makes-them)
  ;; what denote 4.2.3 makes of the same titles and keywords (denote-format-file-name)
  (dolist (case '(("My notes on the garden" ("garden" "project") ".org" "20260927T213446--my-notes-on-the-garden__garden_project.org")
                  ("Hello, World! (draft)" ("project" "draft") ".org" "20260927T213446--hello-world-draft__draft_project.org")
                  ("  Leading and trailing  " () ".org" "20260927T213446--leading-and-trailing.org")
                  ("Ärger über Straßen" ("größe") ".org" "20260927T213446--ärger-über-straßen__größe.org")
                  ("a_b c--d e" ("garden bed" "multi-word") ".org" "20260927T213446--a-b-c-d-e__gardenbed_multiword.org")
                  ("Quotes ‘single’ “double” and #hash" ("zeta" "alpha") ".org" "20260927T213446--quotes-single-double-and-hash__alpha_zeta.org")
                  ("Path/with/slashes and dots... v1.2" ("x") ".org" "20260927T213446--pathwithslashes-and-dots-v12__x.org")
                  ("Tabs	and  many   spaces" ("a=b" "c_d") ".org" "20260927T213446--tabs	and-many-spaces__ab_cd.org")
                  ("" ("solo") ".txt" "20260927T213446__solo.txt")
                  ("Just a title" () ".md" "20260927T213446--just-a-title.md")))
    (destructuring-bind (title keywords extension name) case
      (is string= name (tuatara.defaults::formatted-file-name "20260927T213446" title keywords extension) "~s" title)))
  (is string= "20260927T213446" (tuatara.defaults::time-identifier (local-time 2026 9 27 21 34 46)))
  (is string= ".org.gpg" (tuatara.defaults::name-extension "notes.org.gpg"))
  (is string= ".org" (tuatara.defaults::name-extension "my findings.org"))
  (is string= "" (tuatara.defaults::name-extension ".bashrc"))
  (true (tuatara.defaults::encrypted-name-p "x.txt.gpg"))
  (false (tuatara.defaults::encrypted-name-p "x.org"))
  (is equal '("garden" "index" "home")
      (tuatara.defaults::name-keyword-list "20260224T141326==12--bird-count-record__garden_index_home.org.gpg")))

(define-test (formatted headers-as-denote-makes-them)
  (let ((time (local-time 2026 9 27 21 34 46))
        (keywords '("project" "garden")))
    (flet ((header (name) (tuatara.defaults::formatted-file-header-text (assoc name tuatara-user:*formatted-file-types*)
                                                         "Hello, \"World\"" keywords "20260927T213446" time)))
      (is string= (format nil "#+title: Hello, \"World\"~%#+date: [2026-09-27 Sun 21:34]~%#+filetags: :garden:project:~%#+identifier: 20260927T213446~2%#+options: toc:t tags:nil~%#+export_exclude_tags: noexport~%#+export_select_tags: export~%#+property: header-args :mkdirp yes~2%")
          (header :org))
      (is string= (format nil "title:      Hello, \"World\"~%date:       2026-09-27~%tags:       garden  project~%identifier: 20260927T213446~%---------------------------~2%")
          (header :text))
      ;; the offset is the local zone's, +02:00 where denote's cases were made
      (multiple-value-bind (s m h d mo y w daylight zone) (decode-universal-time time)
        (declare (ignore s m h d mo y w))
        (let* ((east (round (* 60 (+ (- zone) (if daylight 1 0)))))
               (offset (format nil "~:[+~;-~]~2,'0d:~2,'0d" (minusp east) (floor (abs east) 60) (mod (abs east) 60))))
          (is string= (format nil "---~%title:      \"Hello, \\\"World\\\"\"~%date:       2026-09-27T21:34:46~a~%tags:       [\"garden\", \"project\"]~%identifier: \"20260927T213446\"~%---~2%" offset)
              (header :markdown)))))))

(define-test (formatted find-file-formatted)
  (with-formatted-directory (dir)
    (unwind-protect
         (let ((path (namestring (merge-pathnames "my findings.org" dir))))
           ;; a new name: the title (the name typed, replaced), keywords by completion, the name made
           (multiple-value-bind (f editor)
               (run-keys (format nil "SPC f ~a RET C-a C-k ~a RET g a r TAB , SPC p r o RET RET"
                                 (key-string path) (key-string "My notes on the garden"))
                         :width 100 :height 20)
             (declare (ignore f))
             (let* ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                    (name (tuatara.buffer:buffer-name buffer)))
               (true (and (tuatara.defaults::identifier-at-start-p name)
                          (string= "--my-notes-on-the-garden__garden_project.org" (subseq name 15)))
                     "~a" name)
               (true (search "#+title: My notes on the garden" (tuatara.buffer:buffer-string buffer)))
               (true (search (format nil "#+identifier: ~a" (subseq name 0 15)) (tuatara.buffer:buffer-string buffer)))
               ;; nothing written until it is saved; point after the header
               (false (probe-file (merge-pathnames name dir)))
               (false (probe-file path))
               (true (tuatara.buffer:buffer-modified-p buffer))
               (is = (tuatara.buffer:buffer-size buffer) (tuatara.buffer:buffer-point buffer))))
           ;; a name that exists is visited as it is
           (put-file (merge-pathnames "plain.txt" dir) (format nil "plain text~%"))
           (multiple-value-bind (f editor) (run-keys (format nil "SPC f ~a RET" (key-string (namestring (merge-pathnames "plain.txt" dir)))))
             (declare (ignore f))
             (is string= "plain.txt" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
             (is string= (format nil "plain text~%") (session-text editor)))
           ;; a name encrypted as Tuatara cannot (.age) is refused; a .gpg one is made, encrypted when saved
           (multiple-value-bind (f editor) (run-keys (format nil "SPC f ~a RET" (key-string (namestring (merge-pathnames "secret.org.age" dir)))) :width 100)
             (true (fake-frame-showing* f "only .gpg files can be encrypted"))
             (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
           (multiple-value-bind (f editor) (run-keys (format nil "SPC f ~a RET" (key-string (namestring (merge-pathnames "secret.org.gpg" dir)))) :width 100)
             (declare (ignore editor))
             (true (fake-frame-showing* f "New file TITLE"))
             (false (probe-file (merge-pathnames "secret.org.gpg" dir)))))
      (kill-buffers-visiting dir))))

(define-test (formatted empty-lines-for-new-files)
  ;; SPC f offers an empty line first, selected: a new file in the directory
  ;; typed, its title, the end of its name (free text, org the default) and
  ;; keywords asked; at the keywords, an empty line first too: none, or no more
  (with-formatted-directory (dir)
    (unwind-protect
         (let ((root (tuatara.io:native dir))
               ;; offered as your init file offers it; none as standard
               (tuatara-user:*formatted-file-extension* "org"))
           (flet ((made (keys)
                    (multiple-value-bind (f editor) (run-keys keys :width 120 :height 20)
                      (values (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                              f
                              (session-text editor)))))
             (let ((tuatara-user:*formatted-file-extension* ""))
               (multiple-value-bind (name f) (made (format nil "SPC f ~a RET ~a RET RET RET" (key-string root) (key-string "None offered")))
                 (true (tuatara.frontend.fake:fake-frame-showing f "New file EXTENSION: "))
                 (false (tuatara.frontend.fake:fake-frame-showing f "(default"))
                 (true (and (tuatara.defaults::identifier-at-start-p name) (string= "--none-offered" (subseq name 15))) "~a" name)))
             (multiple-value-bind (name f) (made (format nil "SPC f ~a RET ~a RET RET RET" (key-string root) (key-string "Empty line note")))
               (true (and (tuatara.defaults::identifier-at-start-p name)
                          (string= "--empty-line-note.org" (subseq name 15)))
                     "~a" name)
               (true (tuatara.frontend.fake:fake-frame-showing f "New file EXTENSION (default org): "))
               (true (tuatara.frontend.fake:fake-frame-showing f "no keywords")))
             ;; the end of the name as typed, its header with it
             (multiple-value-bind (name f text)
                 (made (format nil "SPC f ~a RET ~a RET m d RET RET" (key-string root) (key-string "In markdown")))
               (declare (ignore f))
               (true (and (tuatara.defaults::identifier-at-start-p name) (string= "--in-markdown.md" (subseq name 15))) "~a" name)
               (true (search "title:      \"In markdown\"" text)))
             ;; a lone dot: no end at all
             (let ((name (made (format nil "SPC f ~a RET ~a RET . RET RET" (key-string root) (key-string "Bare")))))
               (true (and (tuatara.defaults::identifier-at-start-p name) (string= "--bare" (subseq name 15))) "~a" name))
             ;; a name typed without its end: that is asked
             (let ((name (made (format nil "SPC f ~a RET RET RET RET" (key-string (concatenate 'string root "no end"))))))
               (true (and (tuatara.defaults::identifier-at-start-p name) (string= "--no-end.org" (subseq name 15))) "~a" name))
             ;; after a keyword and a comma, the empty line: no more keywords
             (let ((name (made (format nil "SPC f ~a RET ~a RET RET g a r TAB , RET RET" (key-string root) (key-string "One tag")))))
               (true (and (tuatara.defaults::identifier-at-start-p name) (string= "--one-tag__garden.org" (subseq name 15))) "~a" name))
             ;; the empty line offered only while no name is typed: a name typed is taken
             (let ((name (made (format nil "SPC f ~a RET C-a C-k ~a RET RET" (key-string (concatenate 'string root "typed.org")) (key-string "Typed")))))
               (true (and (tuatara.defaults::identifier-at-start-p name) (string= "--typed.org" (subseq name 15))) "~a" name))))
      (kill-buffers-visiting dir))))

(defun fake-frame-showing* (frontend text)
  "True when some frame FRONTEND painted had TEXT on a row."
  (tuatara.frontend.fake:fake-frame-showing frontend text))

(define-test (formatted the-command-names)
  ;; a new file is made by new-file-formatted, on SPC d n, beside find-file-formatted
  ;; and open-file-formatted
  (run-keys "")
  (true (tuatara.command:command-symbol "new-file-formatted"))
  (false (tuatara.command:command-symbol "new-note"))
  (is eq 'tuatara.defaults::new-file-formatted
      (tuatara.command:lookup-key tuatara.defaults::*tuatara-leader-keymap* (tuatara.frontend:parse-key-sequence "d n"))))

(define-test (formatted find-and-make-files)
  (with-formatted-directory (dir)
    (unwind-protect
     (let ((tuatara.defaults::*formatted-file-subdirectory-history* '()))
         (progn
           (ensure-directories-exist (merge-pathnames "sub/" dir))
           (put-file (merge-pathnames "20250101T100000--older-note__project.org" dir) (format nil "older~%"))
           (put-file (merge-pathnames "sub/20260101T100000--newer-note__sketch.txt" dir) (format nil "newer~%"))
           (put-file (merge-pathnames "not-a-note.txt" dir) (format nil "plain~%"))
           ;; the one changed last is offered first
           (sb-posix:utimes (namestring (merge-pathnames "20250101T100000--older-note__project.org" dir)) 1000000000 1000000000)
           (multiple-value-bind (f editor) (run-keys "SPC d f RET" :width 100 :height 20)
             (declare (ignore f))
             (is string= "20260101T100000--newer-note__sketch.txt"
                 (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
           (multiple-value-bind (f editor) (run-keys "SPC d f o l d e r RET" :width 100 :height 20)
             (declare (ignore f))
             (is string= "20250101T100000--older-note__project.org"
                 (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
           ;; SPC d f makes no file: a name that is none of them says so
           (multiple-value-bind (f editor)
               (run-keys (format nil "SPC d f ~a C-j" (key-string "Brand new idea")) :width 100 :height 20)
             (true (tuatara.frontend.fake:fake-frame-showing f "Brand new idea is none of your files with formatted names"))
             (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
             (false (find-if (lambda (b) (search "brand-new-idea" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*)))
           ;; SPC d n makes one: its title, the root, keywords, org
           (multiple-value-bind (f editor)
               (run-keys (list (format nil "SPC d n ~a RET" (key-string "Brand new idea")) :wait "RET f o r e n s i c s C-j RET RET")
                         :width 100 :height 20)
             (declare (ignore f))
             (let* ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                    (name (tuatara.buffer:buffer-name buffer)))
               (is string= "--brand-new-idea__forensics.org" (subseq name 15))
               (is string= (tuatara.io:native (merge-pathnames name dir)) (tuatara.io:native (tuatara.buffer:buffer-file buffer)))
               (true (search "#+filetags: :forensics:" (tuatara.buffer:buffer-string buffer)))))
           ;; SPC d n: a text file in the subdirectory (the root chosen just before ranks
           ;; first among the matches when the test directory's random name holds the
           ;; letters of sub: the history starts empty)
           (multiple-value-bind (f editor)
               (let ((tuatara.defaults::*formatted-file-subdirectory-history* '()))
                 (run-keys (list (format nil "SPC d n ~a RET" (key-string "Shopping list")) :wait "s u b RET C-j t e x t RET")
                           :width 100 :height 20))
             (declare (ignore f))
             (let* ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
                    (name (tuatara.buffer:buffer-name buffer)))
               (is string= "--shopping-list.txt" (subseq name 15))
               (true (search "/sub/" (tuatara.io:native (tuatara.buffer:buffer-file buffer))))
               (true (search "title:      Shopping list" (tuatara.buffer:buffer-string buffer)))))))
      (kill-buffers-visiting dir))))

(define-test (formatted searching-files)
  (with-formatted-directory (dir)
    (unwind-protect
         (progn
           (put-file (merge-pathnames "a.txt" dir) (format nil "first line~%the foo and the bar~%foo alone~%"))
           (ensure-directories-exist (merge-pathnames "deeper/" dir))
           (put-file (merge-pathnames "deeper/b.org" dir) (format nil "* Heading~%bar then foo~%"))
           (put-file (merge-pathnames "a.txt~" dir) (format nil "foo in a backup~%"))
           (ensure-directories-exist (merge-pathnames ".hidden/" dir))
           (put-file (merge-pathnames ".hidden/c.txt" dir) (format nil "foo hidden~%"))
           (alexandria:write-byte-vector-into-file (coerce #(102 111 111 0 1 2) '(vector (unsigned-byte 8)))
                                                   (merge-pathnames "binary.bin" dir))
           (let ((corpus (read-file-corpus dir)))
             (is equalp #("a.txt" "deeper/b.org") (tuatara.defaults::file-corpus-paths corpus))
             (is equal '((0 2 4 "the foo and the bar") (0 3 0 "foo alone") (1 2 9 "bar then foo"))
                 (tuatara.defaults::corpus-lines corpus "foo"))
             ;; every word on the line, in any order; each a regular expression
             (is equal '((0 2 16 "the foo and the bar") (1 2 0 "bar then foo"))
                 (tuatara.defaults::corpus-lines corpus "bar foo"))
             (is equal '((0 3 0 "foo alone")) (tuatara.defaults::corpus-lines corpus "^f.o al"))
             ;; not a regular expression: its characters
             (is equal '() (tuatara.defaults::corpus-lines corpus "foo("))
             (is equal '() (tuatara.defaults::corpus-lines corpus "foo( bar"))
             ;; literal text: the whole input as it stands, spaces and all, nothing in it special
             (is equal '((0 2 4 "the foo and the bar")) (tuatara.defaults::corpus-lines corpus "foo and" :regex nil))
             (is equal '() (tuatara.defaults::corpus-lines corpus "bar foo" :regex nil))
             (is equal '() (tuatara.defaults::corpus-lines corpus "^f.o al" :regex nil))
             (is equal '((0 3 0 "foo alone")) (tuatara.defaults::corpus-lines corpus "foo al" :regex nil))
             (is equal '() (tuatara.defaults::corpus-lines corpus "Foo al" :regex nil)))
           ;; SPC d g: the second line found, visited there
           (multiple-value-bind (f editor) (run-keys '("SPC d g" :wait "f o o" :wait "C-n RET") :width 100 :height 20)
             (declare (ignore f))
             (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
               (is string= "a.txt" (tuatara.buffer:buffer-name buffer))
               (is = (length (format nil "first line~%the foo and the bar~%")) (tuatara.buffer:buffer-point buffer))))
           ;; M-r: literal text, the prompt saying so, and back to a regular expression
           (let ((f (run-keys '("C-x n g" :wait "f . o SPC a l" :wait) :width 100 :height 20)))
             (true (fake-frame-showing* f "Regex search formatted files: "))
             (true (fake-frame-showing* f "foo alone")))
           (let ((f (run-keys '("C-x n g" :wait "M-r" :wait "f . o SPC a l" :wait) :width 100 :height 20)))
             (true (fake-frame-showing* f "Literal search formatted files: "))
             (false (fake-frame-showing* f "foo alone")))
           (multiple-value-bind (f editor) (run-keys '("C-x n g" :wait "M-r f o o SPC a l" :wait "RET") :width 100 :height 20)
             (declare (ignore f))
             (is = (length (format nil "first line~%the foo and the bar~%"))
                 (tuatara.buffer:buffer-point (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
           (let ((f (run-keys '("C-x n g" :wait "M-r M-r f . o SPC a l" :wait) :width 100 :height 20)))
             (true (fake-frame-showing* f "foo alone")))
           ;; M-s g from a file there: the files of its directory
           (multiple-value-bind (f editor)
               (run-keys (format nil "C-x C-f ~a RET M-s g h e a d RET" (key-string (namestring (merge-pathnames "a.txt" dir))))
                         :width 100 :height 20)
             (declare (ignore f))
             (is string= "b.org" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
           ;; a prompt wider than the screen scrolls, so that what is typed shows, the cursor after it
           (multiple-value-bind (f editor)
               (run-keys (format nil "C-x C-f ~a RET M-s g x y z" (key-string (namestring (merge-pathnames "a.txt" dir))))
                         :width 40 :height 10)
             (declare (ignore editor))
             (true (some (lambda (screen)
                           (let ((row (string-right-trim " " (aref screen 9))))
                             (and (>= (length row) 3) (string= "xyz" (subseq row (- (length row) 3)))
                                  (<= (length row) 40))))
                         (tuatara.frontend.fake:fake-history f)))))
      (kill-buffers-visiting dir))))

(define-test (formatted searching-a-project)
  ;; M-s g from a file in a project searches all of the project: the nearest
  ;; directory above the file holding .git or .project, below the home
  ;; directory; outside any, the file's own directory
  (with-test-directory (dir)
    (let ((root (tuatara.io:native dir)))
      (unwind-protect
           (progn
             (ensure-directories-exist (merge-pathnames "proj/.git/" dir))
             (ensure-directories-exist (merge-pathnames "proj/src/" dir))
             (ensure-directories-exist (merge-pathnames "other/deep/" dir))
             (put-file (merge-pathnames "proj/src/a.txt" dir) (format nil "alpha here~%"))
             (put-file (merge-pathnames "proj/b.txt" dir) (format nil "beta here~%"))
             (put-file (merge-pathnames "other/.project" dir) "")
             (is string= (concatenate 'string root "proj/") (tuatara.defaults::project-root (merge-pathnames "proj/src/" dir)))
             (is string= (concatenate 'string root "proj/") (tuatara.defaults::project-root (merge-pathnames "proj/" dir)))
             (is string= (concatenate 'string root "other/") (tuatara.defaults::project-root (merge-pathnames "other/deep/" dir)))
             (let ((tuatara.defaults::*project-root-markers* '("no-such-marker")))
               (false (tuatara.defaults::project-root (merge-pathnames "proj/src/" dir))))
             ;; the home directory is never a project's root, nor what is above it
             (false (tuatara.defaults::project-root (tuatara.io:home-directory)))
             (false (tuatara.defaults::project-root "/"))
             ;; what .gitignore files leave out is not searched: a pattern with a
             ;; / before its end is of paths from its file, one ending with / of
             ;; directories only, ! takes back, ** spans parts
             (put-file (merge-pathnames "proj/.gitignore" dir)
                       (format nil "# built~%/build/~%*.log~%!keep.log~%cache/~%docs/**/draft*~%\\#*~%"))
             (put-file (merge-pathnames "proj/src/.gitignore" dir) (format nil "local.txt~%"))
             (dolist (sub '("proj/build/" "proj/src/build/" "proj/src/cache/" "proj/docs/a/"))
               (ensure-directories-exist (merge-pathnames sub dir)))
             (dolist (file '("proj/build/out.txt" "proj/src/build/inner.txt" "proj/src/x.log" "proj/src/keep.log"
                             "proj/src/cache/c.txt" "proj/docs/a/draft1.txt" "proj/docs/a/final.txt" "proj/src/local.txt"
                             "proj/#hash.txt"))
               (put-file (merge-pathnames file dir) (format nil "beta built~%")))
             ;; and, in a repository, its .git/info/exclude and your excludes file
             (ensure-directories-exist (merge-pathnames "proj/.git/info/" dir))
             (put-file (merge-pathnames "proj/.git/info/exclude" dir) (format nil "*.tmp~%"))
             (put-file (merge-pathnames "excludes" dir) (format nil "*.secret~%"))
             (dolist (file '("proj/src/a.tmp" "proj/src/b.secret"))
               (put-file (merge-pathnames file dir) (format nil "beta built~%")))
             (let ((tuatara.defaults::*git-excludes-file* (tuatara.io:native (merge-pathnames "excludes" dir))))
               (is equal '("b.txt" "docs/a/final.txt" "src/a.txt" "src/build/inner.txt" "src/keep.log")
                   (sort (mapcar #'cdr (tuatara.defaults::files-under (merge-pathnames "proj/" dir) :gitignore t)) #'string<))
               ;; searching a directory within: the .gitignore above it counts too, its
               ;; /build/ of the directory it is in
               (is equal '("a.txt" "build/inner.txt" "keep.log")
                   (sort (mapcar #'cdr (tuatara.defaults::files-under (merge-pathnames "proj/src/" dir) :gitignore t)) #'string<)))
             (let ((tuatara.defaults::*git-excludes-file* nil))
               (true (find "src/b.secret" (mapcar #'cdr (tuatara.defaults::files-under (merge-pathnames "proj/" dir) :gitignore t))
                           :test #'string=)))
             (is = 13 (length (tuatara.defaults::files-under (merge-pathnames "proj/" dir))))
             ;; from src/a.txt, b.txt in the project's root is found
             (multiple-value-bind (f editor)
                 (run-keys (format nil "C-x C-f ~a RET M-s g b e t a RET" (key-string (concatenate 'string root "proj/src/a.txt")))
                           :width 100 :height 20)
               ;; the prompt names the project's root; a long path may put it on a row of its own
               (true (tuatara.frontend.fake:fake-frame-showing f "Regex search files in"))
               (true (tuatara.frontend.fake:fake-frame-showing f "proj/:"))
               (is string= "b.txt" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
        (kill-buffers-visiting dir)))))

(define-test (formatted back-a-buffer)
  (multiple-value-bind (f editor) (run-keys "C-x b z z b 1 RET SPC b")
    (declare (ignore f))
    (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
  (multiple-value-bind (f editor) (run-keys "C-x b z z b 1 RET SPC b SPC b")
    (declare (ignore f))
    (is string= "zzb1" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
  ;; C-x b RET alone: the buffer used last
  (multiple-value-bind (f editor) (run-keys "C-x b z z b 2 RET C-x b RET")
    (declare (ignore f))
    (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
  ;; the buffers made here are not left for the tests after, whose names they would match
  (dolist (name '("zzb1" "zzb2"))
    (let ((buffer (tuatara.buffer:find-buffer name)))
      (when buffer (tuatara.buffer:kill-buffer buffer)))))

(define-test (formatted encrypted-names-not-saved)
  ;; an .age name is not saved, its text would be there to read; a .gpg one asks how to encrypt it first
  (with-test-directory (dir)
    (unwind-protect
         (let ((age (namestring (merge-pathnames "secret.txt.age" dir)))
               (gpg (namestring (merge-pathnames "secret.txt.gpg" dir))))
           (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f ~a RET i s e c r e t C-g C-x C-s" (key-string age)) :width 120)
             (declare (ignore editor))
             (true (fake-frame-showing* f "only .gpg files can be encrypted"))
             (false (probe-file age)))
           (when (tuatara.defaults::gpg-path)
             (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f ~a RET i s e c r e t C-g C-x C-s" (key-string gpg)) :width 120)
               (declare (ignore editor))
               (true (fake-frame-showing* f "asked for as it is saved"))
               (false (probe-file gpg)))))
      (kill-buffers-visiting dir))))

;; with the command tests, but here, where WITH-TEST-DIRECTORY is defined
(define-test (command clipboard-programs)
  ;; the terminal's clipboard runs the programs that serve it: stand-ins here,
  ;; which keep the clipboard in a file, so that the real one is left alone
  (with-test-directory (dir)
    (let* ((file (namestring (merge-pathnames "clipboard.txt" dir)))
           (write (namestring (merge-pathnames "copy" dir)))
           (read (namestring (merge-pathnames "paste" dir)))
           (slow (namestring (merge-pathnames "slow" dir)))
           (tui (make-instance 'tuatara.frontend.tui::tui-frontend)))
      (flet ((script (path text)
               (with-open-file (o path :direction :output :if-exists :supersede)
                 (format o "#!/bin/sh~%~a~%" text))
               (sb-posix:chmod path #o700)))
        (script write (format nil "cat > '~a'" file))
        (script read (format nil "cat '~a'" file))
        (script slow "exec sleep 5"))
      (let ((tuatara.frontend.tui::*clipboard-programs* (list (list write) (list read))))
        (true (tuatara.frontend:frontend-set-clipboard tui "copied: ä → ok"))
        (is string= "copied: ä → ok" (tuatara.frontend:frontend-clipboard tui)))
      ;; a clipboard that does not answer counts as none, in about a second
      (let ((tuatara.frontend.tui::*clipboard-programs* (list (list slow) (list slow)))
            (start (get-internal-real-time)))
        (false (tuatara.frontend:frontend-clipboard tui))
        (true (< (- (get-internal-real-time) start) (* 3 internal-time-units-per-second (machine-slowdown))))
        ;; and leaves no thread behind waiting on it
        (loop repeat 40 while (find "clipboard" (sb-thread:list-all-threads) :key #'sb-thread:thread-name :test #'equal)
              do (sleep 0.05))
        (false (find "clipboard" (sb-thread:list-all-threads) :key #'sb-thread:thread-name :test #'equal)))
      (let ((tuatara.frontend.tui::*clipboard-programs* nil))
        (false (tuatara.frontend:frontend-clipboard tui))))))

(define-test (formatted the-list-of-files)
  ;; every part of a name, a signature included, as Denote writes them
  (is equal '("20260503T131940" "5=2" "garden-plan" ("home" "project"))
      (multiple-value-list (tuatara.defaults::name-parts "20260503T131940==5=2--garden-plan__home_project.org")))
  (is equal '("20260101T100000" nil nil ("kw"))
      (multiple-value-list (tuatara.defaults::name-parts "20260101T100000__kw.org")))
  (is equal '("20260101T100000" nil "a-title" nil)
      (multiple-value-list (tuatara.defaults::name-parts "20260101T100000--a-title.txt")))
  (is equal '("20260101T100000" nil "t" ("k"))
      (multiple-value-list (tuatara.defaults::name-parts "20260101T100000--t__k.org.gpg")))
  (is equal '("20260101T100000" nil "title-first" ("x"))
      (multiple-value-list (tuatara.defaults::name-parts "--title-first@@20260101T100000__x.md")))
  (is string= "2026-05-03" (tuatara.defaults::identifier-date "20260503T131940"))
  (is string= "sub/deeper/" (tuatara.defaults::formatted-file-group "sub/deeper/20260101T100000--x.org"))
  (is string= "org" (tuatara.defaults::formatted-file-group "20260101T100000--x.org"))
  (is string= "org.gpg" (tuatara.defaults::formatted-file-group "20260101T100000--x.org.gpg"))
  (is string= "Other files" (tuatara.defaults::formatted-file-group "20260101T100000--x"))
  ;; SPC d f: each group under its title, the date, title and keywords in columns
  (with-formatted-directory (dir)
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "sub/" dir))
           (put-file (merge-pathnames "20250101T100000==7--older-note__project.org" dir) (format nil "older~%"))
           (put-file (merge-pathnames "20250202T100000--a-much-longer-title-here__garden_project.org" dir) (format nil "longer~%"))
           (put-file (merge-pathnames "sub/20260101T100000--newer-note__sketch.txt" dir) (format nil "newer~%"))
           (sb-posix:utimes (namestring (merge-pathnames "20250101T100000==7--older-note__project.org" dir)) 1000000000 1000000000)
           (sb-posix:utimes (namestring (merge-pathnames "20250202T100000--a-much-longer-title-here__garden_project.org" dir)) 1000000500 1000000500)
           (multiple-value-bind (f editor) (run-keys "SPC d f" :width 100 :height 20)
             (declare (ignore editor))
             (multiple-value-bind (screen spans row-faces) (tuatara.frontend.fake:fake-frame-with f "Open file")
               (declare (ignore row-faces))
               (let ((sub (position-if (lambda (row) (search " sub/ " row)) screen))
                     (org (position-if (lambda (row) (search " org " row)) screen))
                     (newer (position-if (lambda (row) (search "newer-note" row)) screen))
                     (longer (position-if (lambda (row) (search "a-much-longer-title-here" row)) screen))
                     (older (position-if (lambda (row) (search "older-note" row)) screen)))
                 ;; the group of the file changed last first, each title before its files
                 (true (and sub org newer longer older))
                 (true (< sub newer org longer older))
                 (true (search (string (code-char #x2500)) (aref screen sub)))
                 (is eq :completion-group-title (third (second (aref spans sub))))
                 ;; the columns line up: the date, then the titles and the keywords at one column
                 (is = 0 (search "2025-02-02" (aref screen longer)))
                 (is = 12 (search "a-much-longer-title-here" (aref screen longer)))
                 (is = 12 (search "older-note" (aref screen older)))
                 (is = (search "garden project" (aref screen longer)) (search "=7  project" (aref screen older)))
                 (is eq :list-date (third (first (aref spans older))))
                 ;; the size and when it changed at the right
                 (true (search "6 B" (aref screen newer)))
                 (true (search "2001" (aref screen older)))))))
      (kill-buffers-visiting dir))))

(defun gathering-threads-alive (name)
  "The threads named NAME still running."
  (remove-if-not (lambda (thread) (and (equal (sb-thread:thread-name thread) name) (sb-thread:thread-alive-p thread)))
                 (sb-thread:list-all-threads)))

(define-test (formatted gathering-waited-for-asleep)
  ;; waiting for a listing sleeps until it is done or the time is up, never looking a thousand times a second
  (let* ((gate (list nil))
         (gathering (tuatara.defaults::start-gathering
                     "tuatara test gathering"
                     (lambda (give cancel)
                       (declare (ignore give))
                       (loop until (or (car gate) (car cancel)) do (sleep 0.01)))))
         (cpu (thread-cpu-time))
         (start (get-internal-real-time)))
    (unwind-protect
         (progn
           (false (tuatara.defaults::wait-for-gathering gathering 0.2))
           (true (>= (- (get-internal-real-time) start) (* 0.19 internal-time-units-per-second)))
           ;; the thread itself wakes a hundred times a second: well under what a loop of looks costs
           (true (< (- (thread-cpu-time) cpu) (* 0.05 internal-time-units-per-second))
                 "~,3f seconds of processor" (/ (- (thread-cpu-time) cpu) internal-time-units-per-second))
           (setf (car gate) t)
           (true (tuatara.defaults::wait-for-gathering gathering 5)))
      (tuatara.defaults::stop-gathering gathering))))

(define-test (formatted read-as-you-type)
  ;; what a thread finds is taken as it is found, and it stops when told
  (let ((gathering (tuatara.defaults::start-gathering
                    "tuatara test gathering"
                    (lambda (give cancel)
                      (loop for i from 0 until (car cancel) do (funcall give i) (sleep 0.001))))))
    (sleep 0.05)
    (tuatara.defaults::stop-gathering gathering)
    (loop repeat 200 until (tuatara.defaults::gathering-done gathering) do (sleep 0.01))
    (true (tuatara.defaults::gathering-done gathering))
    (multiple-value-bind (found done) (tuatara.defaults::take-gathered gathering)
      (true done)
      (true (plusp (length found)))
      ;; in the order found
      (is equal (loop for i from 0 below (length found) collect i) found)))
  (with-formatted-directory (dir)
    (unwind-protect
         (progn
           ;; many files: listed as found, the one changed last chosen once all are
           (dotimes (i 400)
             (put-file (merge-pathnames (format nil "2025~2,'0d~2,'0dT100000--note-~d__project.org" (1+ (mod i 12)) (1+ (mod i 28)) i) dir)
                       (format nil "text ~d~%" i))
             (sb-posix:utimes (namestring (merge-pathnames (format nil "2025~2,'0d~2,'0dT100000--note-~d__project.org" (1+ (mod i 12)) (1+ (mod i 28)) i) dir))
                              (+ 1000000000 i) (+ 1000000000 i)))
           (put-file (merge-pathnames "20260101T100000--the-newest__project.org" dir) (format nil "needle newest~%"))
           (multiple-value-bind (f editor) (run-keys "SPC d f RET" :width 100 :height 20)
             (declare (ignore f))
             (is string= "20260101T100000--the-newest__project.org"
                 (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
           ;; typed before they are all listed: matched among all
           (multiple-value-bind (f editor) (run-keys "SPC d f n o t e - 3 9 9 RET" :width 100 :height 20)
             (declare (ignore f))
             (is string= "20250408T100000--note-399__project.org"
                 (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
           ;; the reading stops once a file is chosen
           (loop repeat 100 while (gathering-threads-alive "tuatara listing formatted files") do (sleep 0.01))
           (false (gathering-threads-alive "tuatara listing formatted files"))
           ;; SPC d g: the text read in many pieces, each searched as it comes, the lines in file order
           (put-file (merge-pathnames "zz-last.txt" dir) (format nil "a needle at the end~%"))
           (let ((tuatara.defaults::*search-files-piece* 200))
             (multiple-value-bind (f editor) (run-keys '("SPC d g" :wait "n e e d l e") :width 100 :height 20)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "20260101T100000--the-newest__project.org:1: needle newest"))
               (true (tuatara.frontend.fake:fake-frame-showing f "zz-last.txt:1: a needle at the end")))
             ;; RET typed before all is read: the first line found in all of it
             (multiple-value-bind (f editor) (run-keys "SPC d g n e e d l e RET" :width 100 :height 20)
               (declare (ignore f))
               (is string= "20260101T100000--the-newest__project.org"
                   (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
             (loop repeat 100 while (gathering-threads-alive "tuatara reading files to search") do (sleep 0.01))
             (false (gathering-threads-alive "tuatara reading files to search"))))
      (kill-buffers-visiting dir))))

(define-test (formatted kept-read)
  (with-formatted-directory (dir)
    (unwind-protect
         (let ((root (tuatara.defaults::directory-name dir)))
           (ensure-directories-exist (merge-pathnames "sub/" dir))
           (put-file (merge-pathnames "20250101T100000--older-note__project.org" dir) (format nil "a needle in the older~%"))
           (put-file (merge-pathnames "sub/20260101T100000--newer-note__sketch.txt" dir) (format nil "the newer needle~%"))
           (sb-posix:utimes (namestring (merge-pathnames "20250101T100000--older-note__project.org" dir)) 1000000000 1000000000)
           (let ((tuatara-user:*indexed-directories* (list (namestring dir))))
             ;; read in the background as the editor starts
             (run-keys "" :width 100 :height 20)
             (let ((index (tuatara.defaults::index-for dir)))
               (true index)
               (is equal '("sub/20260101T100000--newer-note__sketch.txt" "20250101T100000--older-note__project.org")
                   (mapcar #'car (tuatara.defaults::directory-index-files index)))
               ;; its directories watched on disk
               (is equal (list root (concatenate 'string root "sub/")) (tuatara.defaults::indexed-directories-watched)))
             ;; a directory under it: its files, their paths from there
             (is equal '("20260101T100000--newer-note__sketch.txt")
                 (mapcar #'car (tuatara.defaults::indexed-files (merge-pathnames "sub/" dir))))
             (multiple-value-bind (files gathering) (tuatara.defaults::list-formatted-files (namestring dir))
               (is equal '("sub/20260101T100000--newer-note__sketch.txt" "20250101T100000--older-note__project.org")
                   (mapcar #'car files))
               (false gathering))
             ;; SPC d f and SPC d g answer at once from it, nothing read while they ask
             (multiple-value-bind (f editor) (run-keys "SPC d f o l d e r RET" :width 100 :height 20)
               (false (tuatara.frontend.fake:fake-frame-showing f "(listing"))
               (is string= "20250101T100000--older-note__project.org"
                   (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor)))))
             (multiple-value-bind (f editor) (run-keys "SPC d g n e e d l e" :width 100 :height 20)
               (declare (ignore editor))
               (false (tuatara.frontend.fake:fake-frame-showing f "(listing"))
               (true (tuatara.frontend.fake:fake-frame-showing f "20250101T100000--older-note__project.org:1: a needle in the older"))
               (true (tuatara.frontend.fake:fake-frame-showing f "sub/20260101T100000--newer-note__sketch.txt:1: the newer needle")))
             ;; M-s g in the directory under it: its files only, their paths from there
             (multiple-value-bind (f editor)
                 (run-keys (format nil "C-x C-f ~a RET M-s g n e e d l e"
                                   (key-string (namestring (merge-pathnames "sub/20260101T100000--newer-note__sketch.txt" dir))))
                           :width 100 :height 20)
               (declare (ignore editor))
               (true (tuatara.frontend.fake:fake-frame-showing f "20260101T100000--newer-note__sketch.txt:1: the newer needle"))
               (false (tuatara.frontend.fake:fake-frame-showing f "older-note__project.org:1:")))
             ;; text cut short by the limits: a directory under it searched from disk instead
             (setf (tuatara.defaults::directory-index-left-out (tuatara.defaults::index-for dir)) t)
             (let ((tuatara.defaults::*directory-indexes*
                     (list (let ((cut (tuatara.defaults::make-directory-index (tuatara.defaults::directory-index-root (tuatara.defaults::index-for dir)))))
                             (setf (tuatara.defaults::directory-index-ready cut) t
                                   (tuatara.defaults::directory-index-left-out cut) t)
                             cut))))
               (multiple-value-bind (f editor)
                   (run-keys `(,(format nil "C-x C-f ~a RET M-s g"
                                        (key-string (namestring (merge-pathnames "sub/20260101T100000--newer-note__sketch.txt" dir))))
                               :wait "n e e d l e")
                             :width 100 :height 20)
                 (declare (ignore editor))
                 (true (tuatara.frontend.fake:fake-frame-showing f "20260101T100000--newer-note__sketch.txt:1: the newer needle"))))
             (setf (tuatara.defaults::directory-index-left-out (tuatara.defaults::index-for dir)) nil)
             ;; a change on disk: read again in the background, the new file then there
             (put-file (merge-pathnames "20260202T100000--newest__project.org" dir) (format nil "needle~%"))
             (tuatara.defaults::indexed-directories-changed (list root))
             (run-keys "" :width 100 :height 20)
             (is string= "20260202T100000--newest__project.org"
                 (car (first (tuatara.defaults::directory-index-files (tuatara.defaults::index-for dir))))))
           ;; no longer asked for: let go of
           (run-keys "" :width 100 :height 20)
           (false (tuatara.defaults::index-for dir)))
      (setf tuatara.defaults::*directory-indexes* '())
      (kill-buffers-visiting dir))))

(define-test (formatted previews-while-choosing)
  (with-formatted-directory (dir)
    (unwind-protect
         (let ((note (merge-pathnames "20250101T100000--older-note__project.org" dir))
               (tuatara-user:*save-place-mode* t))
           (put-file note (format nil "the text of the older note~%"))
           ;; no place of it kept from an earlier run (test directories are named alike in each)
           (tuatara.defaults::state-list-drop tuatara.defaults::*saved-places* (list (tuatara.io:native note)))
           ;; off, as standard: nothing shown until chosen
           (let ((tuatara-user:*preview-files* nil) (tuatara-user:*preview-formatted-files* nil) (tuatara-user:*preview-buffers* nil))
             (let ((f (run-keys (format nil "C-x C-f ~a o l d e r C-g" (key-string (namestring dir))) :width 100 :height 20)))
               (false (tuatara.frontend.fake:fake-frame-showing f "the text of the older note")))
             (let ((f (run-keys '("SPC d f" :wait "o l d e r C-g") :width 100 :height 20)))
               (false (tuatara.frontend.fake:fake-frame-showing f "the text of the older note"))))
           ;; files: the one selected shown, opened safely; gone again once the question ends,
           ;; where point was in it not remembered
           (let ((tuatara-user:*preview-files* t))
             (multiple-value-bind (f editor)
                 (run-keys (format nil "C-x C-f ~a o l d e r C-g" (key-string (namestring dir))) :width 100 :height 20)
               (true (tuatara.frontend.fake:fake-frame-showing f "the text of the older note"))
               (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
               (false (find-if (lambda (b) (search "older-note" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*))
               (false (assoc (tuatara.io:native note) (tuatara.defaults::read-saved-places) :test #'string=)))
             ;; chosen: visited as usual, not safely
             (multiple-value-bind (f editor)
                 (run-keys (format nil "C-x C-f ~a o l d e r RET" (key-string (namestring dir))) :width 100 :height 20)
               (declare (ignore f))
               (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                 (is string= "20250101T100000--older-note__project.org" (tuatara.buffer:buffer-name buffer))
                 (false (tuatara.io:safe-visit-p buffer))))
             (kill-buffers-visiting dir))
           ;; formatted files
           (let ((tuatara-user:*preview-formatted-files* t))
             (multiple-value-bind (f editor) (run-keys '("SPC d f" :wait "o l d e r C-g") :width 100 :height 20)
               (true (tuatara.frontend.fake:fake-frame-showing f "the text of the older note"))
               (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))))
           ;; searches: the file of the line selected, at that line; back on C-g
           (let ((deep (merge-pathnames "20250202T100000--deep-note__project.org" dir)))
             (put-file deep (format nil "~{~a~%~}" (loop for i from 1 to 60
                                                        collect (if (= i 45) "line 45 a needle here" (format nil "line ~d" i)))))
             (flet ((shown-at-line (f)
                      (some (lambda (screen)
                              (some (lambda (row) (uiop:string-prefix-p "line 45 a needle" (string-left-trim " " row))) screen))
                            (tuatara.frontend.fake:fake-history f))))
               (let ((tuatara-user:*preview-formatted-search* nil))
                 (false (shown-at-line (run-keys '("SPC d g" :wait "n e e d l e" :wait "C-g") :width 100 :height 20))))
               (let ((tuatara-user:*preview-formatted-search* t))
                 (multiple-value-bind (f editor) (run-keys '("SPC d g" :wait "n e e d l e" :wait "C-g") :width 100 :height 20)
                   (true (shown-at-line f))
                   (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                   (false (find-if (lambda (b) (search "deep-note" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*))))
               ;; M-s g from the file itself: shown there at the line, point back where it was after
               (let ((tuatara-user:*preview-file-search* t))
                 (multiple-value-bind (f editor)
                     (run-keys `(,(format nil "C-x C-f ~a RET M-s g" (key-string (namestring deep))) :wait "n e e d l e" :wait "C-g")
                               :width 100 :height 20)
                   (true (shown-at-line f))
                   (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                     (is string= "20250202T100000--deep-note__project.org" (tuatara.buffer:buffer-name buffer))
                     (is = 0 (tuatara.buffer:buffer-point buffer)))))))
           (kill-buffers-visiting dir)
           ;; buffers: the one selected shown, the one before back on C-g
           (let ((tuatara-user:*preview-buffers* t))
             (let ((other (tuatara.buffer:make-buffer "previewed-buffer" :text "what the other buffer holds")))
               (unwind-protect
                    (multiple-value-bind (f editor) (run-keys "SPC , p r e v i e w e d C-g" :width 100 :height 20)
                      (true (tuatara.frontend.fake:fake-frame-showing f "what the other buffer holds"))
                      (is string= "*scratch*" (tuatara.buffer:buffer-name (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                      (true (tuatara.defaults::live-buffer-p other)))
                 (tuatara.buffer:kill-buffer other)))))
      (kill-buffers-visiting dir))))

(define-test (formatted encrypted-never-read)
  (flet ((octets (&rest parts)
           (coerce (loop for part in parts
                         append (if (stringp part) (map 'list #'char-code part) (list part)))
                   '(vector (unsigned-byte 8)))))
    ;; told by their headers: gpg's packets (as your .gpg files begin: 84 5e 03), age's, either written as text
    (true (tuatara.defaults::encrypted-head-p (octets #x84 #x5e #x03 #xc0 "x")))
    (true (tuatara.defaults::encrypted-head-p (octets #x85 #x01 #x0c #x03 "x")))
    (true (tuatara.defaults::encrypted-head-p (octets #x8c #x0d #x04 #x03)))
    (true (tuatara.defaults::encrypted-head-p (octets #xc1 #x5e #x03 "x")))
    (true (tuatara.defaults::encrypted-head-p (octets #xd2 #x10 #x01 "x")))
    (true (tuatara.defaults::encrypted-head-p (octets (format nil "age-encryption.org/v1~%-> X25519 x"))))
    (true (tuatara.defaults::encrypted-head-p (format nil "~%  -----BEGIN PGP MESSAGE-----~%~%hQEMA")))
    (true (tuatara.defaults::encrypted-head-p (octets "-----BEGIN AGE ENCRYPTED FILE-----")))
    ;; other kinds: LUKS and BitLocker volumes, Ansible's vault, openssl enc, a key under a passphrase
    (true (tuatara.defaults::encrypted-head-p (octets "LUKS" #xba #xbe 0 2)))
    (true (tuatara.defaults::encrypted-head-p (octets #xeb #x58 #x90 "-FVE-FS-" 0)))
    (true (tuatara.defaults::encrypted-head-p (octets (format nil "$ANSIBLE_VAULT;1.1;AES256~%6162"))))
    (true (tuatara.defaults::encrypted-head-p (octets "Salted__" #x8f #x11)))
    (true (tuatara.defaults::encrypted-head-p (octets (format nil "-----BEGIN ENCRYPTED PRIVATE KEY-----~%MIIF"))))
    (true (tuatara.defaults::encrypted-head-p (octets (format nil "-----BEGIN RSA PRIVATE KEY-----~%Proc-Type: 4,ENCRYPTED~%DEK-Info: AES"))))
    (false (tuatara.defaults::encrypted-head-p (octets (format nil "-----BEGIN RSA PRIVATE KEY-----~%MIIEow"))))
    (false (tuatara.defaults::encrypted-head-p (octets #xeb #x52 #x90 "NTFS    " 0)))
    ;; other kinds: LUKS and BitLocker volumes, Ansible's vault, openssl enc, a key under a passphrase
    (true (tuatara.defaults::encrypted-head-p (octets "LUKS" #xba #xbe 0 2)))
    (true (tuatara.defaults::encrypted-head-p (octets #xeb #x58 #x90 "-FVE-FS-" 0)))
    (true (tuatara.defaults::encrypted-head-p (octets (format nil "$ANSIBLE_VAULT;1.1;AES256~%6162"))))
    (true (tuatara.defaults::encrypted-head-p (octets "Salted__" #x8f #x11)))
    (true (tuatara.defaults::encrypted-head-p (octets (format nil "-----BEGIN ENCRYPTED PRIVATE KEY-----~%MIIF"))))
    (true (tuatara.defaults::encrypted-head-p (octets (format nil "-----BEGIN RSA PRIVATE KEY-----~%Proc-Type: 4,ENCRYPTED~%DEK-Info: AES"))))
    (false (tuatara.defaults::encrypted-head-p (octets (format nil "-----BEGIN RSA PRIVATE KEY-----~%MIIEow"))))
    (false (tuatara.defaults::encrypted-head-p (octets #xeb #x52 #x90 "NTFS    " 0)))
    ;; not: text, other pictures and packets
    (false (tuatara.defaults::encrypted-head-p (octets "#+title: a note")))
    (false (tuatara.defaults::encrypted-head-p (octets #x89 "PNG" 13 10)))
    (false (tuatara.defaults::encrypted-head-p (octets #x84 #x05 #x09 "x")))
    (false (tuatara.defaults::encrypted-head-p (octets)))
    (false (tuatara.defaults::encrypted-head-p "")))
  (with-formatted-directory (dir)
    (unwind-protect
         (let ((plain (merge-pathnames "20250101T100000--plain__project.org" dir))
               (locked (merge-pathnames "20250102T100000--locked__project.org.gpg" dir))
               (no-suffix (merge-pathnames "secret" dir))
               (armoured (merge-pathnames "20250103T100000--armoured__project.org" dir)))
           (put-file plain (format nil "a needle in plain sight~%"))
           ;; as if decrypted on disk, the name alone tells
           (put-file locked (format nil "a needle under lock~%"))
           ;; no suffix, a binary OpenPGP header, then text with no zero octet
           (alexandria:write-byte-vector-into-file
            (coerce (append '(#x84 #x5e #x03) (map 'list #'char-code (format nil " a needle without a suffix~%")))
                    '(vector (unsigned-byte 8)))
            no-suffix)
           (put-file armoured (format nil "-----BEGIN PGP MESSAGE-----~%a needle in armour~%-----END PGP MESSAGE-----~%"))
           ;; searching reads none of them
           (is equalp #("20250101T100000--plain__project.org") (tuatara.defaults::file-corpus-paths (read-file-corpus dir)))
           (let ((f (run-keys '("SPC d g" :wait "n e e d l e") :width 100 :height 20)))
             (true (tuatara.frontend.fake:fake-frame-showing f "a needle in plain sight"))
             (false (tuatara.frontend.fake:fake-frame-showing f "under lock"))
             (false (tuatara.frontend.fake:fake-frame-showing f "without a suffix"))
             (false (tuatara.frontend.fake:fake-frame-showing f "in armour")))
           ;; a directory kept read: their names listed, their text not kept
           (let ((tuatara-user:*indexed-directories* (list (namestring dir))))
             (run-keys "" :width 100 :height 20)
             (let ((index (tuatara.defaults::index-for dir)))
               (is = 4 (length (tuatara.defaults::directory-index-files index)))
               (is equal '("20250101T100000--plain__project.org")
                   (loop for corpus across (tuatara.defaults::directory-index-corpora index)
                         append (coerce (tuatara.defaults::file-corpus-paths corpus) 'list)))))
           (setf tuatara.defaults::*directory-indexes* '())
           ;; a dynamic block never reads one, even shown decrypted in a buffer
           (dolist (path (list locked no-suffix armoured))
             (let ((buffer (tuatara.io:visit-file path)))
               (setf (tuatara.buffer:buffer-local buffer :read-only) nil)
               (tuatara.buffer:buffer-delete buffer 0 (tuatara.buffer:buffer-size buffer))
               (tuatara.buffer:buffer-insert buffer 0 "#+title: unlocked\na needle decrypted\n")
               (false (tuatara.defaults::formatted-file-text (cons (tuatara.io:native path) "x")))))
           (true (search "plain sight" (tuatara.defaults::formatted-file-text (cons (tuatara.io:native plain) "x")))))
      (forget-file-buffers)
      (kill-buffers-visiting dir))))

(define-test (formatted kept-read-costs-nothing-between-changes)
  ;; tens of thousands of directories followed: the watcher told once, each tick after costs nothing
  (let* ((index (tuatara.defaults::make-directory-index "/nowhere/"))
         (calls 0)
         (tuatara.defaults::*directory-indexes* (list index))
         (tuatara.defaults::*directories-followed* '())
         (tuatara.defaults::*indexed-directories-watched* (cons nil '()))
         (tuatara.defaults::*directory-followers*
           (list (cons 'tuatara.defaults::indexed-directories-watched (lambda (changed) (declare (ignore changed)))))))
    (setf (tuatara.defaults::directory-index-directories index)
          (loop for i below 50000 collect (format nil "/nowhere/d~d/" i)))
    (sb-int:encapsulate 'tuatara.io:watch-directories 'count-calls
                        (lambda (f directories) (declare (ignore f directories)) (incf calls) '()))
    (unwind-protect
         (let ((start (thread-cpu-time)))
           (tuatara.defaults::follow-directories nil)
           (is = 1 calls)
           (let ((again (thread-cpu-time)))
             (dotimes (i 1000) (tuatara.defaults::follow-directories nil))
             (is = 1 calls)
             ;; a thousand ticks well within a frame, all together
             (true (< (- (thread-cpu-time) again) (* 0.016 internal-time-units-per-second))))
           ;; read again: a new list, told once more
           (setf (tuatara.defaults::directory-index-directories index)
                 (cons "/nowhere/new/" (tuatara.defaults::directory-index-directories index)))
           (tuatara.defaults::follow-directories nil)
           (is = 2 calls)
           (true (< (- (thread-cpu-time) start) (* 2 internal-time-units-per-second))))
      (sb-int:unencapsulate 'tuatara.io:watch-directories 'count-calls)))
  ;; alike names once, in order, in time growing with their number alone
  (is equal '("a" "b" "c") (tuatara.io::unique-strings (list "a" "b" "a" "c" "b")))
  (let ((many (loop for i below 100000 collect (format nil "/d~d/" (mod i 60000))))
        (start (thread-cpu-time)))
    (is = 60000 (length (tuatara.io::unique-strings many)))
    (true (< (- (thread-cpu-time) start) (* 0.5 internal-time-units-per-second)))))

(define-test (formatted only-text-read)
  (with-test-directory (dir)
    ;; told by their first octets, even with no zero octet near the start: not read as text
    (put-file (merge-pathnames "paper.txt" dir) (format nil "%PDF-1.4~%a needle in a pdf~%"))
    (put-file (merge-pathnames "picture" dir) (format nil "GIF89a needle"))
    (put-file (merge-pathnames "note.txt" dir) (format nil "a needle in text~%"))
    (put-file (merge-pathnames "drawing.svg" dir) (format nil "<svg><text>a needle drawn</text></svg>~%"))
    (false (tuatara.defaults::read-text-file (namestring (merge-pathnames "paper.txt" dir))))
    (false (tuatara.defaults::read-text-file (namestring (merge-pathnames "picture" dir))))
    (true (tuatara.defaults::read-text-file (namestring (merge-pathnames "note.txt" dir))))
    ;; a drawing written as text is text
    (true (tuatara.defaults::read-text-file (namestring (merge-pathnames "drawing.svg" dir))))))

(define-test (formatted indexing-shown)
  (with-test-directory (dir)
    (put-file (merge-pathnames "note.txt" dir) (format nil "text~%"))
    (let ((index (tuatara.defaults::make-directory-index (tuatara.defaults::directory-name dir))))
      ;; while directories are kept read the signs keep their place: U lit while one is read,
      ;; S while one is searched, each faint otherwise; nothing with no sign, or with no
      ;; directories kept read (P, the buffer's own sign before them, left out: test/jobs.lisp)
      (clrhash tuatara.defaults::*signs-lit-until*)
      (setf (tuatara.defaults::directory-index-reading index) t)
      (let ((tuatara.defaults::*directory-indexes* (list index))
            (tuatara-user:*indexed-directories* (list (namestring dir)))
            (tuatara.defaults::*sign-lit-seconds* 0)
            (tuatara-user:*processing-indicator* nil))
        (is equal '("U" :mode-line-sign) (tuatara.defaults::indexing-mode-line nil nil))
        (is equal '(("U" :mode-line-sign) ("S" :mode-line-sign-idle)) (tuatara.defaults::mode-line-signs nil nil))
        (let ((tuatara-user:*indexing-indicator* nil))
          (false (tuatara.defaults::indexing-mode-line nil nil))
          (is equal '(("S" :mode-line-sign-idle)) (tuatara.defaults::mode-line-signs nil nil)))
        (setf (tuatara.defaults::directory-index-reading index) nil)
        (is equal '("U" :mode-line-sign-idle) (tuatara.defaults::indexing-mode-line nil nil))
        ;; a search over a directory kept read lights S
        (let ((tuatara.defaults::*index-searches* (list 1)))
          (is equal '("S" :mode-line-sign) (tuatara.defaults::searching-mode-line nil nil)))
        (is equal '("S" :mode-line-sign-idle) (tuatara.defaults::searching-mode-line nil nil))
        (let ((tuatara-user:*indexed-directories* '()))
          (false (tuatara.defaults::indexing-mode-line nil nil))
          (false (tuatara.defaults::mode-line-signs nil nil))))
      ;; work noted as it begins and ends, on whatever thread, lights its sign a moment, so
      ;; that work done at once is seen; the screen is wanted drawn again as it fades
      (clrhash tuatara.defaults::*signs-lit-until*)
      (let ((tuatara.defaults::*directory-indexes* (list index))
            (tuatara-user:*indexed-directories* (list (namestring dir)))
            (tuatara.defaults::*sign-lit-seconds* 1/5))
        (false (tuatara.defaults::signs-tick nil))
        (tuatara.defaults::note-sign-work :searching)
        (is equal '("S" :mode-line-sign) (tuatara.defaults::searching-mode-line nil nil))
        (is equal '("U" :mode-line-sign-idle) (tuatara.defaults::indexing-mode-line nil nil))
        (let ((in (tuatara.defaults::signs-tick nil)))
          (true (and in (< 0 in 1/4))))
        (true (member 'tuatara.defaults::signs-tick tuatara.command:*tick-functions*))
        (sleep 1/4)
        (is equal '("S" :mode-line-sign-idle) (tuatara.defaults::searching-mode-line nil nil))
        (false (tuatara.defaults::signs-tick nil))))
    ;; a search over a directory kept read lights S however quick it is
    (clrhash tuatara.defaults::*signs-lit-until*)
    (let ((tuatara-user:*indexed-directories* (list (namestring dir))))
      (run-keys "" :width 100 :height 10)
      (run-keys (format nil "C-u M-s g C-a C-k ~a RET t e x t" (key-string (namestring dir))) :width 100 :height 10)
      (true (gethash :searching tuatara.defaults::*signs-lit-until*)))
    (setf tuatara.defaults::*directory-indexes* '())
    ;; first in the mode line, US between the modal state and [-], so that nothing coming and going moves them
    (let ((tuatara-user:*indexed-directories* (list (namestring dir))))
      (let ((f (run-keys "" :width 100 :height 10)))
        (true (tuatara.frontend.fake:fake-frame-showing f " TUATARA[N] PUS [-] *scratch*  L"))))
    (setf tuatara.defaults::*directory-indexes* '())
    (clrhash tuatara.defaults::*signs-lit-until*)))

(defun index-texts (index)
  "The paths of the files whose text INDEX keeps, in order."
  (loop for corpus across (tuatara.defaults::directory-index-corpora index)
        append (coerce (tuatara.defaults::file-corpus-paths corpus) 'list)))

(defun index-now (index result)
  "Put RESULT, what READ-DIRECTORY-INDEX gave, in place in INDEX, as the tick does."
  (destructuring-bind (files corpora left-out large directories) result
    (setf (tuatara.defaults::directory-index-files index) files
          (tuatara.defaults::directory-index-corpora index) corpora
          (tuatara.defaults::directory-index-left-out index) left-out
          (tuatara.defaults::directory-index-large index) large
          (tuatara.defaults::directory-index-directories index) directories
          (tuatara.defaults::directory-index-ready index) t)
    index))

(define-test (formatted kept-read-as-it-changes)
  (with-test-directory (dir)
    (let* ((root (tuatara.defaults::directory-name dir))
           (reads 0))
      (dolist (sub '("a/" "b/")) (ensure-directories-exist (merge-pathnames sub dir)))
      (dotimes (i 20) (put-file (merge-pathnames (format nil "a/n~2,'0d.txt" i) dir) (format nil "note ~d~%" i)))
      (dotimes (i 20) (put-file (merge-pathnames (format nil "b/m~2,'0d.txt" i) dir) (format nil "memo ~d~%" i)))
      (sb-int:encapsulate 'tuatara.defaults::read-text-file 'count-reads
                          (lambda (f &rest arguments) (incf reads) (apply f arguments)))
      (unwind-protect
           (let ((index (tuatara.defaults::make-directory-index root)))
             ;; read whole the first time, in pieces of about 64 octets
             (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64))
             (is = 40 reads)
             (is = 40 (length (index-texts index)))
             (let ((before (copy-seq (tuatara.defaults::directory-index-corpora index))))
               ;; one file changed: only it read again, the pieces of the other directory kept as they were
               (setf reads 0)
               (sleep 0.01)
               (put-file (merge-pathnames "a/n05.txt" dir) (format nil "note five, changed~%"))
               (index-now index (tuatara.defaults::read-directory-index index (list (concatenate 'string root "a/")) (* 1024 1024) 64))
               (is = 1 reads)
               (let ((after (tuatara.defaults::directory-index-corpora index)))
                 (true (search "note five, changed"
                               (some (lambda (corpus) (let ((text (tuatara.text:rope-string (tuatara.defaults::file-corpus-rope corpus))))
                                                        (and (search "five" text) text)))
                                     (coerce after 'list))))
                 ;; most pieces are the very same
                 (true (>= (count-if (lambda (corpus) (find corpus before)) after) (- (length after) 2)))))
             ;; a directory made, one deleted, a .gitignore leaving files out
             (setf reads 0)
             (ensure-directories-exist (merge-pathnames "c/" dir))
             (put-file (merge-pathnames "c/new.txt" dir) (format nil "new~%"))
             (uiop:delete-directory-tree (merge-pathnames "b/" dir) :validate t)
             (put-file (merge-pathnames ".gitignore" dir) (format nil "n1*~%"))
             (index-now index (tuatara.defaults::read-directory-index index (list root) (* 1024 1024) 64))
             (is = 2 reads)             ; the new file, and the .gitignore for its rules
             (let ((texts (index-texts index)))
               (true (member "c/new.txt" texts :test #'string=))
               (false (find-if (lambda (path) (uiop:string-prefix-p "b/" path)) texts))
               (false (member "a/n12.txt" texts :test #'string=))
               (true (member "a/n05.txt" texts :test #'string=)))
             (true (member (concatenate 'string root "c/") (tuatara.defaults::directory-index-directories index) :test #'string=))
             (false (member (concatenate 'string root "b/") (tuatara.defaults::directory-index-directories index) :test #'string=)))
        (sb-int:unencapsulate 'tuatara.defaults::read-text-file 'count-reads)))))

(define-test (formatted kept-read-between-sessions)
  (with-test-directory (dir)
    (let* ((root (tuatara.defaults::directory-name dir))
           (cache (tuatara.defaults::index-cache-directory root))
           (reads 0))
      (dotimes (i 10) (put-file (merge-pathnames (format nil "n~d.txt" i) dir) (format nil "note ~d~%" i)))
      (put-file (merge-pathnames "secret.org.gpg" dir) (format nil "a needle under lock~%"))
      (sb-int:encapsulate 'tuatara.defaults::read-text-file 'count-reads
                          (lambda (f &rest arguments) (incf reads) (apply f arguments)))
      (unwind-protect
           (flet ((all-mapped-p (index)
                    ;; every piece's text read from its file kept on disk, none of it in the heap
                    (every (lambda (corpus)
                             (let ((statistics (tuatara.text::rope-statistics (tuatara.defaults::file-corpus-rope corpus))))
                               (and (zerop (getf statistics :memory-bytes)) (plusp (getf statistics :file-bytes)))))
                           (tuatara.defaults::directory-index-corpora index))))
             (let ((index (tuatara.defaults::make-directory-index root)))
               (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64 t))
               (true (all-mapped-p index)))
             ;; kept on disk for you alone, never an encrypted file's text
             (is = #o700 (logand #o777 (sb-posix:stat-mode (sb-posix:stat cache))))
             (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (concatenate 'string cache "index")))))
             (dolist (entry (tuatara.io:directory-entries cache))
               (when (uiop:string-prefix-p "piece-" (first entry))
                 (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (concatenate 'string cache (first entry))))))
                 (false (search "needle" (alexandria:read-file-into-string (concatenate 'string cache (first entry)))))))
             ;; the next session: nothing read but what changed meanwhile
             (sleep 0.01)
             (put-file (merge-pathnames "n3.txt" dir) (format nil "note three, changed~%"))
             (setf reads 0)
             (let ((index (tuatara.defaults::make-directory-index root)))
               (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64 t))
               (is = 1 reads)        ; the file changed alone: the encrypted one is known not to be text
               (is = 10 (length (index-texts index)))
               (true (all-mapped-p index))
               (true (some (lambda (corpus) (search "note three, changed" (tuatara.text:rope-string (tuatara.defaults::file-corpus-rope corpus))))
                           (coerce (tuatara.defaults::directory-index-corpora index) 'list))))
             ;; something kept that makes no sense: read whole, as if nothing were kept
             (with-open-file (out (concatenate 'string cache "index") :direction :output :if-exists :supersede)
               (format out "tuatara index 1~%P piece-gone 3 99~%F x~%"))
             (setf reads 0)
             (let ((index (tuatara.defaults::make-directory-index root)))
               (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64 t))
               (is = 11 reads)
               (is = 10 (length (index-texts index)))))
        (sb-int:unencapsulate 'tuatara.defaults::read-text-file 'count-reads)
        (uiop:delete-directory-tree (pathname cache) :validate t)))))

(define-test (formatted index-leaves-out-what-git-ignores)
  ;; what .gitignore leaves out is neither listed, walked nor watched: its files are not offered either
  (with-test-directory (dir)
    (let* ((root (tuatara.defaults::directory-name dir))
           (index (tuatara.defaults::make-directory-index root)))
      (dolist (folder '("build/deep/" "src/out/" "src/lib/out/" "src/keep/"))
        (ensure-directories-exist (merge-pathnames folder dir)))
      (put-file (merge-pathnames ".gitignore" dir) (format nil "build/~%"))
      (put-file (merge-pathnames "src/.gitignore" dir) (format nil "/out/~%"))
      (dolist (file '("build/a.txt" "build/deep/b.txt" "src/out/c.txt" "src/lib/out/d.txt" "src/keep/e.txt" "top.txt"))
        (put-file (merge-pathnames file dir) (format nil "~a~%" file)))
      (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64))
      (flet ((files () (sort (remove ".gitignore" (mapcar (lambda (f) (pathname-name (car f))) (tuatara.defaults::directory-index-files index))
                                     :test #'string= :key (lambda (n) (or n "")))
                             #'string<))
             (walked-p (folder) (and (gethash (concatenate 'string root folder) (tuatara.defaults::directory-index-tree index)) t)))
        ;; build/ anywhere and src/out/ alone (anchored): src/lib/out/ is kept
        (is equal '("d" "e" "top") (remove nil (files)))
        (false (walked-p "build/"))
        (false (walked-p "build/deep/"))
        (false (walked-p "src/out/"))
        (true (walked-p "src/lib/out/"))
        (false (member (concatenate 'string root "build/") (tuatara.defaults::directory-index-directories index) :test #'string=))
        ;; a folder made in an ignored one stays out; the rule taken away, the folder comes in
        (ensure-directories-exist (merge-pathnames "build/new/" dir))
        (index-now index (tuatara.defaults::read-directory-index index (list (concatenate 'string root "build/")) (* 1024 1024) 64))
        (false (walked-p "build/new/"))
        (put-file (merge-pathnames ".gitignore" dir) (format nil "# nothing left out~%"))
        (index-now index (tuatara.defaults::read-directory-index index (list root) (* 1024 1024) 64))
        (true (walked-p "build/deep/"))
        (true (walked-p "build/new/"))
        (true (member "b" (files) :test #'equal)))
      ;; the rules above a directory, from the index's own down (its own .gitignore now leaves nothing out)
      (is equal '("out")
          (mapcar #'caar (tuatara.defaults::index-tree-rules (tuatara.defaults::directory-index-tree index) root
                                                             (concatenate 'string root "src/lib/") '()))))))

(define-test (formatted index-names-compact)
  ;; an index keeps its names and paths at an octet a character where they are ASCII, the others as they are
  (with-test-directory (dir)
    (let* ((root (tuatara.defaults::directory-name dir))
           (index (tuatara.defaults::make-directory-index root)))
      (ensure-directories-exist (merge-pathnames "plain/" dir))
      (put-file (merge-pathnames "plain/note.txt" dir) (format nil "a note~%"))
      (put-file (merge-pathnames "plain/née.txt" dir) (format nil "another~%"))
      (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64))
      (let ((files (mapcar #'car (tuatara.defaults::directory-index-files index))))
        (is equal '("plain/note.txt" "plain/née.txt") (sort (copy-list files) #'string<))
        (true (typep (find "plain/note.txt" files :test #'string=) 'simple-base-string))
        (true (string= "plain/née.txt" (find "plain/née.txt" files :test #'string=))))
      (true (every (lambda (path) (typep path 'simple-base-string)) (tuatara.defaults::directory-index-directories index))))))

(define-test (formatted cache-names-one-to-one)
  ;; each octet of the name written apart: a dash (U+2014) and " 14" no longer share a directory
  (is string= "%2Fnotes%E2%80%94%2F" (tuatara.defaults::index-cache-name "/notes—/"))
  (is string= "%2Fnotes%2014%2F" (tuatara.defaults::index-cache-name "/notes 14/"))
  (is string= "%2Fn%C3%A9_-1%2F" (tuatara.defaults::index-cache-name "/né_-1/"))
  ;; an index kept for another directory under the same name is passed over, not taken
  (with-test-directory (dir)
    (with-test-directory (cache)
      (let ((root (tuatara.defaults::directory-name dir))
            (kept (tuatara.io:native cache)))
        (put-file (merge-pathnames "n.txt" dir) (format nil "a note~%"))
        (let ((index (tuatara.defaults::make-directory-index root)))
          (tuatara.defaults::read-directory-index index :all (* 1024 1024) 64)
          (tuatara.defaults::save-index index kept))
        (true (tuatara.defaults::load-index (tuatara.defaults::make-directory-index root) kept))
        (false (tuatara.defaults::load-index (tuatara.defaults::make-directory-index (concatenate 'string root "other/")) kept))))))

(define-test (formatted binary-told-alike)
  ;; opening a file and searching files tell a binary one by the same first 64 KB
  (with-test-directory (dir)
    (flet ((file (name size &optional nul)
             (let ((octets (make-array size :element-type '(unsigned-byte 8) :initial-element 97))
                   (path (merge-pathnames name dir)))
               (when nul (setf (aref octets nul) 0))
               (alexandria:write-byte-vector-into-file octets path)
               (namestring path))))
      ;; a zero octet 10 KB in: binary to both
      (let ((path (file "late-zero.txt" 20000 10000)))
        (true (tuatara.io:binary-head-p (tuatara.io:file-head path)))
        (false (tuatara.defaults::read-text-file path))
        (false (tuatara.defaults::read-text-file path :most 100)))
      ;; one past the first 64 KB: text to both, read whole
      (let ((path (file "very-late-zero.txt" 70000 68000)))
        (false (tuatara.io:binary-head-p (tuatara.io:file-head path)))
        (is = 70000 (length (tuatara.defaults::read-text-file path)))
        (is eq :too-large (nth-value 1 (tuatara.defaults::read-text-file path :most 1000))))
      ;; a small file read whole
      (is = 5 (length (tuatara.defaults::read-text-file (file "small.txt" 5)))))))

(define-test (formatted chosen-files-visited-as-usual)
  ;; a line chosen in a search of files, and SPC f on a name that is there, open the
  ;; file as C-x C-f does: why it opened safely said, a binary one in the hex view
  (with-formatted-directory (dir)
    (unwind-protect
         (progn
           (put-file (merge-pathnames "a.txt" dir) (format nil "first line~%foo here~%"))
           (let ((tuatara-user:*safe-directories* (list (namestring dir))))
             (multiple-value-bind (f editor) (run-keys '("SPC d g" :wait "f o o" :wait "RET") :width 120 :height 20)
               (let ((buffer (session-buffer editor)))
                 (is string= "a.txt" (tuatara.buffer:buffer-name buffer))
                 (true (tuatara.io:safe-visit-p buffer))
                 (is = (length (format nil "first line~%")) (tuatara.buffer:buffer-point buffer))
                 (true (fake-frame-showing* f "a.txt opened safely: it is in")))))
           (kill-buffers-visiting dir)
           (alexandria:write-byte-vector-into-file (coerce #(102 111 111 0 1 2) '(vector (unsigned-byte 8)))
                                                   (merge-pathnames "binary.bin" dir))
           (multiple-value-bind (f editor)
               (run-keys (format nil "SPC f ~a RET" (key-string (namestring (merge-pathnames "binary.bin" dir)))) :width 120)
             (declare (ignore f))
             (is string= "binary.bin" (tuatara.buffer:buffer-name (session-buffer editor)))
             (true (tuatara.display:hex-view-p (session-buffer editor)))))
      (kill-buffers-visiting dir))))

(define-test (formatted previews-hex-as-set)
  ;; a binary file previewed is in the hex view only while *hex-view-binary-files* is true
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "binary.bin" dir))))
      (alexandria:write-byte-vector-into-file (coerce #(102 111 111 0 1 2) '(vector (unsigned-byte 8))) path)
      (dolist (setting '(nil t))
        (let* ((tuatara-user:*hex-view-binary-files* setting)
               (made '())
               (buffer (tuatara.defaults::preview-file-buffer path (lambda (b) (push b made)))))
          (unwind-protect
               (progn
                 (true (member buffer made))
                 (is eq setting (and (tuatara.display:hex-view-p buffer) t)))
            (tuatara.buffer:kill-buffer buffer)))))))

(define-test (formatted gigantic-files-not-read)
  (with-test-directory (dir)
    (put-file (merge-pathnames "a-small.txt" dir) (format nil "small needle~%"))
    (put-file (merge-pathnames "b-large.txt" dir) (make-string 50000 :initial-element #\x))
    (put-file (merge-pathnames "c-small.txt" dir) (format nil "another needle~%"))
    ;; larger than what is left: not read at all, its size known from the system
    (multiple-value-bind (octets why) (tuatara.defaults::read-text-file (namestring (merge-pathnames "b-large.txt" dir)) :most 1000)
      (false octets)
      (is eq :too-large why))
    ;; the searches' limit: the large one left out, those after it still read
    (let ((tuatara.defaults::*search-files-limit* 10000))
      (multiple-value-bind (corpus left-out) (read-file-corpus dir)
        (is equalp #("a-small.txt" "c-small.txt") (tuatara.defaults::file-corpus-paths corpus))
        (true left-out)))
    (let* ((root (tuatara.defaults::directory-name dir))
           (index (tuatara.defaults::make-directory-index root)))
      (index-now index (tuatara.defaults::read-directory-index index :all 10000 4096))
      (is equal '("a-small.txt" "c-small.txt") (index-texts index))
      ;; the large one kept aside, to be searched from disk
      (is equal '("b-large.txt") (mapcar #'first (tuatara.defaults::directory-index-large index)))
      (false (tuatara.defaults::directory-index-left-out index)))
    ;; searched as grep searches, a piece at a time, never held whole: its lines found, numbered right
    (put-file (merge-pathnames "d-log.txt" dir)
              (with-output-to-string (out) (dotimes (i 3000) (format out "line ~d~:[~; with a needle~]~%" (1+ i) (= i 2499)))))
    (let ((tuatara.defaults::*search-disk-piece* 4096))
      (let ((found (tuatara.defaults::lines-on-disk (namestring (merge-pathnames "d-log.txt" dir)) "d-log.txt"
                                                    (tuatara.defaults::directory-name dir) "needle")))
        (is = 1 (length found))
        (is string= "d-log.txt:2500: line 2500 with a needle" (tuatara.defaults::candidate-string (first found)))))
    ;; in a search: what fits kept, the rest searched from disk, all its lines listed
    (let ((tuatara.defaults::*search-files-limit* 100)
          (tuatara-user:*formatted-file-directory* (namestring dir)))
      (let ((f (run-keys '("SPC d g" :wait "n e e d l e" :wait) :width 100 :height 20)))
        (true (tuatara.frontend.fake:fake-frame-showing f "a-small.txt:1: small needle"))
        (true (tuatara.frontend.fake:fake-frame-showing f "d-log.txt:2500: line 2500 with a needle"))))))

(define-test (formatted search-list-steady)
  ;; typing on: the lines found before stay listed until the new search gives its own,
  ;; never an empty list between keys (the list and a preview emptied and filled again)
  (with-formatted-directory (dir)
    (unwind-protect
         (progn
           (dotimes (i 30) (put-file (merge-pathnames (format nil "f~d.txt" i) dir) (format nil "a needle here ~d~%a needless line~%" i)))
           (let ((f (run-keys '("SPC d g" :wait "n e e d" :wait "l" :wait "e" :wait) :width 100 :height 20)))
             (let* ((frames (reverse (tuatara.frontend.fake:fake-history f)))
                    (counts (loop for screen in frames
                                  for prompt = (find-if (lambda (row) (search "Regex search formatted files" row)) screen)
                                  when prompt
                                    collect (let ((slash (position #\/ prompt)))
                                              (and slash (ignore-errors (parse-integer prompt :end slash :junk-allowed t))))))
                    (first-found (position-if (lambda (n) (and n (plusp n))) counts)))
               (true first-found)
               ;; once lines are listed, the list never shows none again while typing on
               (false (and first-found (find 0 (subseq counts first-found)))))))
      (kill-buffers-visiting dir))))

(define-test (formatted panel-keeps-its-height)
  ;; typing narrows the list from many to one: the panel keeps its height, the window above stays put
  (with-formatted-directory (dir)
    (unwind-protect
         (progn
           (dotimes (i 12) (put-file (merge-pathnames (format nil "2025010~dT10000~d--note-~a__p.org" (1+ (mod i 9)) (mod i 10) (code-char (+ 97 i))) dir)
                                     (format nil "x~%")))
           (let* ((f (run-keys '("SPC d f" :wait "n o t e - l") :width 100 :height 24))
                  (frames (reverse (tuatara.frontend.fake:fake-history f)))
                  (mode-line-rows (loop for screen in frames
                                        when (find-if (lambda (row) (search "Open file" row)) screen)
                                          collect (position-if (lambda (row) (search "TUATARA[" row)) screen))))
             ;; one candidate left at the end
             (true (tuatara.frontend.fake:fake-frame-showing f "note-l"))
             (true (plusp (length mode-line-rows)))
             ;; the panel may grow as the list comes, never shrink: the mode line above never moves down
             (true (every #'>= mode-line-rows (rest mode-line-rows)))
             (true (> (first mode-line-rows) (car (last mode-line-rows))))))
      (kill-buffers-visiting dir))))

(define-test (formatted note-templates)
  ;; the files in note-templates/ offered after the templates set, by name, their %date% and %author% filled
  (with-test-directory (dir)
    (let ((old (sb-posix:getenv "XDG_CONFIG_HOME")))
      (unwind-protect
           (progn
             (sb-posix:setenv "XDG_CONFIG_HOME" (namestring dir) 1)
             (let ((folder (merge-pathnames "tuatara/note-templates/" dir)))
               (ensure-directories-exist folder)
               (alexandria:write-string-into-file (format nil "#+issue_date: %date%~%#+author: %author%~%* Summary~%")
                                                  (merge-pathnames "report.org" folder))
               (alexandria:write-string-into-file "x" (merge-pathnames "report.org~" folder)))
             (let* ((tuatara.defaults::*note-templates-directory* nil)
                    (templates (tuatara.defaults::formatted-templates)))
               (is equal '("blank" "report") (mapcar #'first templates))
               (let ((text (tuatara.defaults::formatted-template-text (second templates))))
                 (true (cl-ppcre:scan "^#\\+issue_date: \\d{4}-\\d\\d-\\d\\d$" (first (uiop:split-string text :separator '(#\Newline)))))
                 (true (search (format nil "#+author: ~a~%" (tuatara.defaults::user-full-name)) text))
                 (true (search "* Summary" text))))
             (let ((tuatara.defaults::*note-templates-directory* nil))
               (is string= "" (tuatara.defaults::formatted-template-text (first (tuatara.defaults::formatted-templates))))))
        (if old (sb-posix:setenv "XDG_CONFIG_HOME" old 1) (sb-posix:unsetenv "XDG_CONFIG_HOME"))))))

(define-test (formatted insert-note-template)
  ;; C-x i: a template's text read into the buffer at point, never its file opened; any file too; asked over text
  (with-test-directory (dir)
    (let* ((folder (merge-pathnames "templates/" dir))
           (template (merge-pathnames "report.org" folder))
           (other (namestring (merge-pathnames "plain.txt" dir)))
           (tuatara.defaults::*note-templates-directory* (namestring folder)))
      (ensure-directories-exist folder)
      (alexandria:write-string-into-file (format nil "#+author: %author%~%* Summary~%") template)
      (alexandria:write-string-into-file (format nil "As it is: %date%~%") other)
      ;; an empty buffer: the template's text, its fields filled; the buffer no file, the template untouched
      (multiple-value-bind (f editor) (run-keys "C-x i r e p o r t RET" :text "" :width 80 :height 10)
        (is string= (format nil "#+author: ~a~%* Summary~%" (tuatara.defaults::user-full-name)) (session-text editor))
        (false (tuatara.buffer:buffer-file (session-buffer editor)))
        (true (tuatara.frontend.fake:fake-frame-showing f "Inserted report")))
      (is string= (format nil "#+author: %author%~%* Summary~%") (alexandria:read-file-into-string template))
      ;; a buffer with text: asked; no leaves it as it was, yes inserts at point
      (multiple-value-bind (f editor) (run-keys "C-x i r e p o r t RET n RET" :text "kept" :width 80 :height 10)
        (is string= "kept" (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Nothing inserted")))
      (multiple-value-bind (f editor) (run-keys "C-x i r e p o r t RET y RET" :text "kept" :width 80 :height 10)
        (declare (ignore f))
        (true (uiop:string-prefix-p "#+author:" (session-text editor)))
        (true (alexandria:ends-with-subseq "kept" (session-text editor))))
      ;; any other file, as it is: nothing filled in
      (multiple-value-bind (f editor)
          (run-keys (format nil "C-x i A n o t h e r RET C-a C-k ~a RET" (key-string other)) :text "" :width 80 :height 10)
        (declare (ignore f))
        (is string= (format nil "As it is: %date%~%") (session-text editor)))
      ;; into a file whose lines end with a carriage return and a newline: its lines end so too
      (let ((windows (namestring (merge-pathnames "windows.txt" dir))))
        (alexandria:write-string-into-file (format nil "kept~c~%" #\Return) windows)
        (multiple-value-bind (f editor) (run-keys "g e C-x i r e p o r t RET y RET" :file windows :width 80 :height 10)
          (declare (ignore f))
          (let ((text (session-text editor)))
            (true (search (format nil "* Summary~c~%" #\Return) text) "~s" text)
            (false (cl-ppcre:scan "[^\\r]\\n" text) "~s" text)))))))

(define-test (formatted note-title-from-front-matter)
  ;; the title of a note is its front matter's, quotes and colons kept; a line of its body is never taken
  (with-test-directory (dir)
    (flet ((title (name text)
             (let ((path (namestring (merge-pathnames name dir))))
               (alexandria:write-string-into-file text path :if-exists :supersede)
               (tuatara.defaults::org-note-title path))))
      (is string= "My Real Title: A/B? \"quoted\""
          (title "20261015T120000--old-title__alpha.org"
                 (format nil "#+title:      My Real Title: A/B? \"quoted\"~%#+filetags: :alpha:~%~%Title: not this~%")))
      (is string= "old title" (title "20261015T120000--old-title__alpha.org" (format nil "Some words~%Title: not this~%")))
      (false (title "plain note.org" (format nil "Notes~%Title: Report~%")))
      (is string= "Say \"hi\"" (title "20261015T120000--say-hi.md" (format nil "---~%title:      \"Say \\\"hi\\\"\"~%date: 2026~%---~%~%title: body~%")))
      (is string= "Plain words" (title "20261015T120000--plain-words.txt" (format nil "title:      Plain words~%date: 2026~%---------~%"))))))

(define-test (formatted front-matter-read-alike)
  ;; one reader of a note's title and tags, for Org, Markdown (YAML and TOML) and text, as the
  ;; headers new notes are made with write them, agreeing with the readers the Org features had
  (let ((cases (list (list :org (format nil "#+title: A walk on Mt Taranaki~%#+date: [2026-09-27 Sun 21:34]~%#+filetags: :garden:project:~%~%Body")
                           "A walk on Mt Taranaki" '("garden" "project"))
                     (list :markdown (format nil "---~%title: \"Tui \\\"the bird\\\" Rotorua\"~%tags: [\"garden\", \"project\"]~%---~%Body")
                           "Tui \"the bird\" Rotorua" '("garden" "project"))
                     (list :markdown (format nil "+++~%title = \"Ann\"~%tags = [\"a\"]~%+++~%")
                           "Ann" '("a"))
                     (list :text (format nil "title:      Notes~%date:       2026-09-27~%tags:       garden  project~%---~%")
                           "Notes" '("garden" "project"))
                     (list :org (format nil "#+filetags: :x:~%# a comment~%#+title: Later~%") "Later" '("x"))
                     (list :org (format nil "Body first~%#+title: Not front matter~%") nil nil)
                     (list :org (format nil "#+title:~%") nil nil))))
    (loop for (style head title tags) in cases
          do (multiple-value-bind (read-title read-tags) (tuatara.defaults::front-matter head style)
               (is equal title read-title "~s" head)
               (is equal tags read-tags "~s" head))))
  ;; from a file, by the kind its name says
  (with-test-directory (dir)
    (let ((path (concatenate 'string (tuatara.io:native dir) "20260927T213446--walk__garden.org")))
      (alexandria:write-string-into-file (format nil "#+title: Walk~%#+filetags: :garden:~%") path)
      (is equal '("Walk" ("garden")) (multiple-value-list (tuatara.defaults::note-front-matter path)))
      (is string= "Walk" (tuatara.defaults::title-for-renaming path)))))

(define-test (formatted note-readers-take-front-matter-only)
  ;; a note's title and tags come from its front matter alike for a link, a block's
  ;; link and a block's headline; a line of its body is never taken
  (with-test-directory (dir)
    (let* ((name "20261015T120000--walk-home__garden.org")
           (path (concatenate 'string (tuatara.io:native dir) name))
           (file (cons path name)))
      (flet ((write-note (text)
               (alexandria:write-string-into-file text path :if-exists :supersede)))
        (write-note (format nil "#+filetags: :garden:~%~%* Notes~%#+title: Not this~%"))
        (is string= "walk home" (tuatara.defaults::org-note-title path))
        (is string= "walk-home" (tuatara.defaults::formatted-title-or-name file :org))
        (false (search "* Not this" (tuatara.defaults::formatted-heading-contents file nil nil)))
        (write-note (format nil "#+title: Walk to Rotorua~%#+filetags: :garden:trip:~%~%Body~%#+title: Not this~%"))
        (is string= "Walk to Rotorua" (tuatara.defaults::org-note-title path))
        (is string= "Walk to Rotorua" (tuatara.defaults::formatted-title-or-name file :org))
        (let ((heading (tuatara.defaults::formatted-heading-contents file nil nil)))
          (true (cl-ppcre:scan "^\\* Walk to Rotorua +:garden:trip:" heading) "~s" heading))))))

(define-test (formatted large-buffer-header-not-rewritten)
  ;; a note open in a buffer too large to rewrite whole as the command waits
  ;; keeps its header too, said so, the buffer left as it was
  (with-test-directory (dir)
    (let ((path (concatenate 'string (tuatara.io:native dir) "20260927T213446--big__log.txt"))
          (tuatara.defaults::*formatted-header-limit* 100))
      (alexandria:write-string-into-file (format nil "title: Big~%~%~a" (make-string 200 :initial-element #\x)) path)
      (let ((buffer (tuatara.io:visit-file path)))
        (unwind-protect
             (progn
               (loop repeat 200 until (search "xxx" (tuatara.buffer:buffer-string buffer)) do (sleep 0.01))
               (let ((said (tuatara.defaults::write-formatted-header path "Bigger" '("log") "20260927T213446")))
                 (true (and said (search "header was not written" said)) "~s" said))
               (true (uiop:string-prefix-p "title: Big" (tuatara.buffer:buffer-string buffer)))
               (false (tuatara.buffer:buffer-modified-p buffer)))
          (tuatara.buffer:kill-buffer buffer))))))

(define-test (formatted made-at-once-never-over-another)
  ;; a formatted file made at once is written with its header; a name there
  ;; already is said and nothing made, the file there left as it is
  (with-test-directory (dir)
    (let* ((directory (tuatara.io:native dir))
           (made (tuatara.defaults::make-formatted-file directory "First note" '("garden") ".org" :write t)))
      (true (stringp made))
      (true (search "#+title: First note" (alexandria:read-file-into-string made)))
      (sb-int:encapsulate 'tuatara.defaults::unique-identifier 'same-second
                          (lambda (function directory time) (declare (ignore function directory time))
                            (subseq (tuatara.io:native-file-name made) 0 15)))
      (unwind-protect
           (false (tuatara.defaults::make-formatted-file directory "First note" '("garden") ".org" :write t))
        (sb-int:unencapsulate 'tuatara.defaults::unique-identifier 'same-second))
      (true (search "First note" (alexandria:read-file-into-string made))))))

(define-test (formatted large-header-not-rewritten)
  ;; a note too large to read and write whole as the command waits keeps its header, said so
  (with-test-directory (dir)
    (let ((path (concatenate 'string (tuatara.io:native dir) "20260927T213446--big__log.txt"))
          (tuatara.defaults::*formatted-header-limit* 100))
      (alexandria:write-string-into-file (format nil "title: Big~%~%~a" (make-string 200 :initial-element #\x)) path)
      (let ((said (tuatara.defaults::write-formatted-header path "Bigger" '("log") "20260927T213446")))
        (true (and said (search "header was not written" said)) "~s" said))
      (true (uiop:string-prefix-p "title: Big" (alexandria:read-file-into-string path)))
      ;; within the limit, written
      (let ((tuatara.defaults::*formatted-header-limit* 100000))
        (false (tuatara.defaults::write-formatted-header path "Bigger" '("log") "20260927T213446"))
        (true (search "Bigger" (alexandria:read-file-into-string path)))))))
