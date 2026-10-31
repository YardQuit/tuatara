;;;; test/state-and-files.lisp --- Files and state: the lists kept between sessions, a file's first octets, git, logs and paths.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test state-and-files
  :description "Files and state: the recent files, places, PDF places and paths
chosen kept by one machinery, two sessions' entries merged, the files as written before still
read; a file's first octets read once a visit, binary told alike everywhere; git never run
outside a repository and once a visit; the logs rotated alike and private; paths with odd
names.")

(defmacro with-fresh-state (&body body)
  "Run BODY with a state directory of its own, empty, removed afterwards,
what is still being written to it written first."
  (let ((old (gensym)) (state (gensym)))
    `(with-test-directory (,state)
       (let ((,old (sb-posix:getenv "XDG_STATE_HOME")))
         (sb-posix:setenv "XDG_STATE_HOME" (namestring ,state) 1)
         (unwind-protect (progn ,@body)
           (tuatara.defaults::finish-state-writes)
           (if ,old (sb-posix:setenv "XDG_STATE_HOME" ,old 1) (sb-posix:unsetenv "XDG_STATE_HOME")))))))

(defun another-session (list)
  "A copy of the state list LIST as another session has it: the same file,
nothing read yet."
  (tuatara.defaults::make-state-list (tuatara.defaults::state-list-name list)
                                     (tuatara.defaults::state-list-parse list)
                                     (tuatara.defaults::state-list-print list)
                                     (tuatara.defaults::state-list-limit-variable list)
                                     :header (tuatara.defaults::state-list-header list)
                                     :path-of (tuatara.defaults::state-list-path-of list)
                                     :key (tuatara.defaults::state-list-key list)))

(defparameter *state-list-samples*
  '((tuatara.defaults::*recent-files* ("/other/one.txt" . nil) ("/mine/two.txt" . t))
    (tuatara.defaults::*saved-places* ("/other/one.txt" . 12) ("/mine/two.txt" . 34))
    (tuatara.defaults::*pdf-places* ("/other/one.pdf" 3 :width 1.25) ("/mine/two.pdf" 1 :page 1.5))
    (tuatara.defaults::*chosen-paths* "/other/one/" "/mine/two.txt"))
  "Each list kept between sessions with an entry another session puts there and one put here.")

(define-test (state-and-files wipe-compares-regular-files-only)
  ;; the comparison before a wipe refuses a named pipe put in the file's place
  ;; at once, never waiting on it
  (with-test-directory (dir)
    (let* ((fifo (concatenate 'string (tuatara.io:native dir) "pipe"))
           (result :waiting)
           (thread (progn (sb-posix:mkfifo fifo #o600)
                          (sb-thread:make-thread
                           (lambda () (setf result (ignore-errors (tuatara.defaults::file-holds-p fifo (tuatara.text:string-octets "x")))))))))
      (loop repeat 100 while (sb-thread:thread-alive-p thread) do (sleep 0.02))
      (let ((waiting (sb-thread:thread-alive-p thread)))
        (when waiting
          ;; a reader waiting on the pipe is let go: a writer opens it and closes it
          (ignore-errors (sb-posix:close (sb-posix:open fifo (logior sb-posix:o-wronly sb-posix:o-nonblock)))))
        (sb-thread:join-thread thread :default nil :timeout 5)
        (false waiting "the comparison waited on the pipe")
        (false result)))))

(define-test (state-and-files globs-read-alike)
  ;; a glob read with braces and without them reads the same
  ;; syntax and agrees on a name: *, ?, classes, ranges, [!...], \\ and the [ that opens nothing
  (dolist (case '(("*.org" "notes.org" t) ("*.org" "notes.txt" nil) ("a?c" "abc" t) ("a?c" "abcd" nil)
                  ("[abc]x" "bx" t) ("[abc]x" "dx" nil) ("report-202[4-6].*" "report-2025.pdf" t)
                  ("report-202[4-6].*" "report-2023.pdf" nil) ("[!0-9]*" "x1" t) ("[!0-9]*" "1x" nil)
                  ("[]]" "]" t) ("[!]]x" "ax" t) ("[!]]x" "]x" nil) ("a\\*b" "a*b" t) ("a\\*b" "aXb" nil)
                  ("a\\?" "a?" t) ("a\\?" "ab" nil) ("[ab" "[ab" t) ("[ab" "a" nil) ("" "" t)))
    (destructuring-bind (glob name expected) case
      (is eq expected (tuatara.defaults::glob-match glob name) "glob-match ~s ~s" glob name)
      (is eq expected (tuatara.defaults::glob-match glob name :braces t) "glob-match :braces ~s ~s" glob name)))
  ;; any case, as rimu's name search asks for a pattern of small letters: a range too
  (true (tuatara.defaults::glob-match "[a-c]*.PDF" "B-report.pdf" :test #'char-equal))
  (false (tuatara.defaults::glob-match "[a-c]*.pdf" "B-report.pdf"))
  ;; classes keep the name's glob linear: a pattern that would take for ever as a regular expression ends at once
  (let ((start (thread-cpu-time)))
    (false (tuatara.defaults::glob-match (format nil "~30@{*[ab]~}c" nil) (make-string 2000 :initial-element #\b)))
    (true (< (- (thread-cpu-time) start) internal-time-units-per-second))))

(define-test (state-and-files sessions-merged)
  ;; every list: what another session wrote meanwhile kept, what is put here first, on
  ;; disk and in each session's list once the file has changed
  (with-fresh-state
    (loop for (variable other-entry mine) in *state-list-samples*
          for list = (symbol-value variable)
          do (let ((other (another-session list)))
               ;; both sessions change the list before either has written
               (tuatara.defaults::state-list-put other (list other-entry))
               (tuatara.defaults::state-list-put list (list mine))
               (true (tuatara.defaults::write-state-list other) "~a" variable)
               (tuatara.defaults::finish-state-writes)
               ;; both kept, whichever was written last first
               (let ((written (tuatara.defaults::read-state-file list (tuatara.defaults::state-list-path list))))
                 (is = 2 (length written) "~a" variable)
                 (true (member mine written :test #'equal) "~a" variable)
                 (true (member other-entry written :test #'equal) "~a" variable)
                 (is equal written (tuatara.defaults::state-list-entries list) "~a" variable)
                 (is equal written (tuatara.defaults::state-list-entries other) "~a" variable))
               ;; private
               (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (tuatara.defaults::state-list-path list)))))))
    ;; put while the other session's write is on disk: put first, the other kept
    (let ((list tuatara.defaults::*recent-files*))
      (tuatara.defaults::remember-file "/mine/three.txt")
      (tuatara.defaults::finish-state-writes)
      (is equal "/mine/three.txt" (car (first (tuatara.defaults::read-state-file list (tuatara.defaults::state-list-path tuatara.defaults::*recent-files*)))))
      (is = 3 (length (tuatara.defaults::read-recent-files))))
    ;; the same list given again while nothing changed, and taken out alike
    (let ((list tuatara.defaults::*chosen-paths*))
      (is eq (tuatara.defaults::state-list-entries list) (tuatara.defaults::state-list-entries list))
      (tuatara.defaults::state-list-drop list '("/other/one/"))
      (is equal '("/mine/two.txt") (tuatara.defaults::state-list-entries list))
      (tuatara.defaults::finish-state-writes)
      (is string= (format nil "/mine/two.txt~%") (file-text (tuatara.defaults::state-list-path list))))
    ;; a PDF whose path has spaces keeps its place
    (tuatara.defaults::state-list-put tuatara.defaults::*pdf-places* (list (list "/a b/c d.pdf" 7 :width 1.5)))
    (tuatara.defaults::finish-state-writes)
    (is equal '("/a b/c d.pdf" 7 :width 1.5)
        (first (tuatara.defaults::read-state-file tuatara.defaults::*pdf-places* (tuatara.defaults::state-list-path tuatara.defaults::*pdf-places*))))))

(define-test (state-and-files old-formats-read)
  ;; the files as the editor wrote them before are read, and written again just so
  (with-fresh-state
    (let ((recent (format nil "# The files Tuatara visited lately, the latest first: file, or safe for one~%~
                               # opened safely, then its path (\\\\, \\n and \\r stand for \\, a newline and a return).~%~
                               safe /a/b c.txt~%file /x\\ny~%"))
          (places (format nil "# Where point was in the files Tuatara visited, the latest first: the octet,~%~
                               # then the file's path (\\\\, \\n and \\r stand for \\, a newline and a return).~%~
                               12 /a/b c.txt~%0 /x\\\\y~%"))
          (pdf (format nil "3 width 1.250 /a/doc.pdf~%1 page 1.000 /b/other doc.pdf~%"))
          (chosen (format nil "/a/b c.txt~%/a/~%")))
      (tuatara.io:write-private-file (tuatara.defaults::state-list-path tuatara.defaults::*recent-files*) recent)
      (tuatara.io:write-private-file (tuatara.defaults::state-list-path tuatara.defaults::*saved-places*) places)
      (tuatara.io:write-private-file (tuatara.defaults::state-list-path tuatara.defaults::*pdf-places*) pdf)
      (tuatara.io:write-private-file (tuatara.defaults::chosen-paths-file) chosen)
      (is equal (list (cons "/a/b c.txt" t) (cons (format nil "/x~%y") nil)) (tuatara.defaults::read-recent-files))
      (is equal '(("/a/b c.txt" . 12) ("/x\\y" . 0)) (tuatara.defaults::read-saved-places))
      (is equal '(("/a/doc.pdf" 3 :width 1.25) ("/b/other doc.pdf" 1 :page 1.0)) (tuatara.defaults::read-pdf-places))
      (is equal '("/a/b c.txt" "/a/") (tuatara.defaults::chosen-paths))
      ;; a change written leaves the rest as it was
      (tuatara.defaults::drop-recent-files '("/nowhere"))
      (tuatara.defaults::state-list-drop tuatara.defaults::*saved-places* '("/nowhere"))
      (tuatara.defaults::state-list-drop tuatara.defaults::*pdf-places* '("/nowhere"))
      (tuatara.defaults::state-list-drop tuatara.defaults::*chosen-paths* '("/nowhere"))
      (tuatara.defaults::finish-state-writes)
      (is string= recent (file-text (tuatara.defaults::state-list-path tuatara.defaults::*recent-files*)))
      (is string= places (file-text (tuatara.defaults::state-list-path tuatara.defaults::*saved-places*)))
      (is string= pdf (file-text (tuatara.defaults::state-list-path tuatara.defaults::*pdf-places*)))
      (is string= chosen (file-text (tuatara.defaults::chosen-paths-file))))))

(defun binary-and-text (dir)
  "Make in DIR a file binary by a zero octet 10 KB in, named as text, and a
file of text; their native names, as two values."
  (let ((octets (make-array 20000 :element-type '(unsigned-byte 8) :initial-element 97))
        (binary (merge-pathnames "late-zero.txt" dir)))
    (setf (aref octets 10000) 0)
    (alexandria:write-byte-vector-into-file octets binary)
    (values (tuatara.io:native binary)
            (tuatara.io:native (put-file (merge-pathnames "plain.txt" dir) (format nil "plain text~%"))))))

(define-test (state-and-files head-read-once-a-visit)
  ;; the views, the binary test that opens a file safely and the hex view look at one read
  (with-test-directory (dir)
    (multiple-value-bind (binary text) (binary-and-text dir)
      (let ((reads '()))
        (sb-int:encapsulate 'tuatara.io::read-file-head 'count-reads
                            (lambda (function path limit)
                              (push (tuatara.io:native path) reads)
                              (funcall function path limit)))
        (unwind-protect
             (dolist (path (list binary text))
               (setf reads '())
               (let ((tuatara-user:*open-binary-files-safely* t)
                     (tuatara-user:*hex-view-binary-files* t))
                 (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f C-a C-k ~a RET" (key-string path)) :width 120 :height 12)
                   (declare (ignore f))
                   (let ((buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
                     (is eq (eq path binary) (and (tuatara.display:hex-view-p buffer) t))
                     (is eq (eq path binary) (and (tuatara.io:safe-visit-p buffer) t)))))
               (is = 1 (count path reads :test #'string=) "~a: ~a" path reads))
          (sb-int:unencapsulate 'tuatara.io::read-file-head 'count-reads)
          (kill-buffers-visiting dir))))))

(define-test (state-and-files binary-told-alike-everywhere)
  ;; visiting, previewing, searching and keeping a directory read tell a binary file alike
  (with-test-directory (dir)
    (multiple-value-bind (binary text) (binary-and-text dir)
      (unwind-protect
           (progn
             (true (tuatara.io:binary-head-p (tuatara.io:file-head binary)))
             (false (tuatara.io:binary-head-p (tuatara.io:file-head text)))
             (false (tuatara.io:binary-head-p nil))
             ;; previews
             (let ((made '()))
               (true (tuatara.display:hex-view-p (tuatara.defaults::preview-file-buffer binary (lambda (b) (push b made)))))
               (false (tuatara.display:hex-view-p (tuatara.defaults::preview-file-buffer text (lambda (b) (push b made)))))
               (mapc #'tuatara.buffer:kill-buffer made))
             ;; searching
             (false (tuatara.defaults::read-text-file binary))
             (true (tuatara.defaults::read-text-file text))
             ;; a directory kept read
             (let* ((root (tuatara.defaults::directory-name dir))
                    (index (tuatara.defaults::make-directory-index root)))
               (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 4096))
               (is equal '("plain.txt") (index-texts index))
               (true (gethash "late-zero.txt" (tuatara.defaults::directory-index-not-text index)))))
        (kill-buffers-visiting dir)))))

(define-test (state-and-files walks-alike)
  ;; the walks list each directory once, as the index lists it: hidden names, backups and
  ;; links to directories left out, a directory's own kind known without looking again
  (with-test-directory (dir)
    (let ((root (tuatara.defaults::directory-name dir)))
      (put-file (merge-pathnames "a.txt" dir) "a")
      (put-file (merge-pathnames "a.txt~" dir) "backup")
      (sb-posix:mkdir (concatenate 'string root "sub [1]*") #o700)
      (put-file (sb-ext:parse-native-namestring (concatenate 'string root "sub [1]*/b.txt")) "b")
      (sb-posix:symlink (concatenate 'string root "sub [1]*") (concatenate 'string root "link"))
      (let ((entry (find "link" (tuatara.io:directory-entries root) :key #'first :test #'string=)))
        (is eq :directory (second entry))
        (is eq :symlink (fourth entry)))
      (is equal '(("a.txt" :regular) ("sub [1]*" :directory))
          (mapcar (lambda (entry) (subseq entry 0 2))
                  (tuatara.defaults::index-directory-entries (tuatara.defaults::list-index-directory root))))
      (is equal '("a.txt" "sub [1]*/b.txt") (mapcar #'cdr (tuatara.defaults::files-under root)))
      ;; kept read: walked from the index, the disk not listed again
      (let ((index (tuatara.defaults::make-directory-index root))
            (listed 0))
        (index-now index (tuatara.defaults::read-directory-index index :all (* 1024 1024) 4096))
        (sb-int:encapsulate 'tuatara.defaults::list-index-directory 'count-listings
                            (lambda (function &rest arguments) (incf listed) (apply function arguments)))
        (unwind-protect
             (let ((tuatara.defaults::*directory-indexes* (list index)))
               (is equal '("a.txt" "sub [1]*/b.txt") (mapcar #'cdr (tuatara.defaults::files-under root)))
               (is = 0 listed))
          (sb-int:unencapsulate 'tuatara.defaults::list-index-directory 'count-listings))))))

(defun count-git-processes (function)
  "Call FUNCTION, and return how many git processes it started."
  (let ((count 0))
    (sb-ext:without-package-locks
      (sb-int:encapsulate 'sb-ext:run-program 'count-git
                          (lambda (run program &rest arguments)
                            (when (equal (file-namestring program) "git") (incf count))
                            (apply run program arguments))))
    (unwind-protect (funcall function)
      (sb-ext:without-package-locks
        (sb-int:unencapsulate 'sb-ext:run-program 'count-git)))
    count))

(define-test (state-and-files git-only-in-repositories)
  ;; no git at all outside a repository: the gutter, rimu's column and RUN-GIT itself
  (let ((dir (uiop:ensure-directory-pathname (merge-pathnames (format nil "tuatara-no-repo-~36r/" (random (expt 36 8)))
                                                              (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (let* ((native (tuatara.io:native dir))
                (file (tuatara.io:native (put-file (merge-pathnames "data.txt" dir) "data"))))
           (if (tuatara.defaults::repository-top native)
               (skip "the temporary directory is in a repository" (true t))
               (is = 0 (count-git-processes
                        (lambda ()
                          (false (tuatara.defaults::run-git native '("status")))
                          (false (tuatara.defaults::committed-text file))
                          (false (tuatara.defaults::git-tracked-p file))
                          (false (tuatara.defaults::rimu-git-commits native '("data.txt"))))))))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(define-test (state-and-files git-once-a-visit)
  ;; in a repository: the gutter's look at a file asks git for the id of its text in HEAD, and reads
  ;; that text only the first time it meets that id: two git processes, then one; none on the
  ;; editor's thread
  (with-test-directory (dir)
    (let ((file (merge-pathnames "data.txt" dir)))
      (git dir "init" "-q")
      (git dir "config" "user.email" "t@example.com") (git dir "config" "user.name" "t")
      (git dir "config" "commit.gpgsign" "false")
      (put-file file (format nil "one~%two~%"))
      (git dir "add" "data.txt") (git dir "commit" "-q" "-m" "base")
      (put-file file (format nil "one~%two~%three~%"))
      (let ((threads '()))
        (sb-int:encapsulate 'tuatara.defaults::run-git 'note-thread
                            (lambda (function &rest arguments)
                              (push sb-thread:*current-thread* threads)
                              (apply function arguments)))
        (unwind-protect
             (flet ((visit ()
                      (count-git-processes
                       (lambda ()
                         (multiple-value-bind (f editor) (vc-session "" file)
                           (declare (ignore editor))
                           (is string= "+ three" (tuatara.frontend.fake:fake-screen-line f 2))
                           (forget-file-buffers))))))
               (clrhash tuatara.defaults::**committed-lines**)
               (is = 2 (visit))
               (is = 1 (visit)))
          (sb-int:unencapsulate 'tuatara.defaults::run-git 'note-thread))
        (false (member sb-thread:*current-thread* threads))))))

(define-test (state-and-files logs-rotated-alike)
  ;; the debug log and the log of standard error: the one before kept aside, the new one
  ;; private from the start, neither put aside while another session writes to it
  (drain-debug-reports)
  (with-test-directory (dir)
    (let ((cache (sb-posix:getenv "XDG_CACHE_HOME"))
          (debug-log (tuatara.io:native (merge-pathnames "debug.log" dir)))
          (stderr-log (tuatara.io:native (merge-pathnames "tuatara/stderr.log" dir))))
      (ensure-directories-exist (merge-pathnames "tuatara/" dir))
      (dolist (log (list debug-log stderr-log))
        (put-file (sb-ext:parse-native-namestring log) (format nil "a session before~%"))
        (sb-posix:chmod log #o644))
      (sb-posix:setenv "XDG_CACHE_HOME" (tuatara.io:native dir) 1)
      (unwind-protect
           (progn
             (let ((tuatara.runtime:*debug-log-path* debug-log)
                   (tuatara.runtime::*debug-log-begun* nil))
               (tuatara.runtime:set-debug t)
               (tuatara.runtime:debug-report :test "this session")
               (tuatara.runtime:set-debug nil))
             (sb-posix:close (tuatara.io:session-log "a test"))
             (dolist (log (list debug-log stderr-log))
               (is string= (format nil "a session before~%") (file-text (sb-ext:parse-native-namestring (format nil "~a.1" log))))
               (false (search "a session before" (file-text (sb-ext:parse-native-namestring log))) "~a" log)
               (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat log))) "~a" log))
             ;; another session writing the log of standard error: written after, not put aside
             (let ((fd (sb-posix:open stderr-log sb-posix:o-rdonly)))
               (unwind-protect
                    (progn
                      (true (tuatara.runtime:lock-fd fd :shared t))
                      (sb-posix:close (tuatara.io:session-log "another test")))
                 (sb-posix:close fd)))
             (let ((text (file-text (sb-ext:parse-native-namestring stderr-log))))
               (true (search "a test, process" text) "~a" text)
               (true (search "another test, process" text) "~a" text))
             (is string= (format nil "a session before~%") (file-text (sb-ext:parse-native-namestring (format nil "~a.1" stderr-log)))))
        (if cache (sb-posix:setenv "XDG_CACHE_HOME" cache 1) (sb-posix:unsetenv "XDG_CACHE_HOME"))
        (drain-debug-reports)))))

(define-test (state-and-files odd-paths)
  ;; [ ] * ? and \ in names are only characters to the path helpers
  (is string= "/a/b [1]*/" (tuatara.io:parent-path "/a/b [1]*/c\\d?.txt"))
  (is string= "/a/" (tuatara.io:parent-path "/a/b [1]*/"))
  (is string= "/" (tuatara.io:parent-path "/x"))
  (is string= "/" (tuatara.io:parent-path "/"))
  (is string= "/a/b?/" (tuatara.io:parent-path (sb-ext:parse-native-namestring "/a/b?/c*.txt")))
  (is string= "~/odd [1]*/x" (tuatara.command:abbreviated-path (concatenate 'string (tuatara.io:home-directory) "odd [1]*/x")))
  (is string= "/elsewhere [2]" (tuatara.command:abbreviated-path "/elsewhere [2]"))
  ;; a path with a tab, a newline or a backslash in it stays one field of a line parted by tabs
  (let* ((odd (format nil "/a~cb/c~%d\\e~cf" #\Tab #\Return))
         (line (tuatara.defaults::escape-path odd)))
    (false (find #\Tab line))
    (false (find #\Newline line))
    (is string= odd (tuatara.defaults::unescape-path line)))
  (with-test-directory (dir)
    (let* ((root (tuatara.io:native dir))
           (project (concatenate 'string root "odd [1]*?/"))
           (deep (concatenate 'string project "sub\\dir/")))
      (ensure-directories-exist (sb-ext:parse-native-namestring (concatenate 'string deep "x")))
      (put-file (sb-ext:parse-native-namestring (concatenate 'string project ".project")) "")
      (is string= project (tuatara.defaults::project-root deep))
      (unless (tuatara.defaults::repository-top root)
        (false (tuatara.defaults::repository-top deep)))
      (sb-posix:mkdir (concatenate 'string project ".git") #o700)
      (is string= project (tuatara.defaults::repository-top deep))
      (is string= project (tuatara.defaults::marked-directory deep '(".git")))
      ;; the home directory and / are never a project's root
      (false (tuatara.defaults::project-root (tuatara.io:home-directory)))
      (false (tuatara.defaults::project-root "/")))))

(define-test (state-and-files folders-chosen)
  ;; *cache-directory*, *state-directory* and *packages-directory* name folders of your own; one not
  ;; there is never made, its things kept in memory ($XDG_RUNTIME_DIR) instead and said
  (with-test-directory (dir)
    (let* ((chosen (namestring (merge-pathnames "chosen/" dir)))
           (gone (namestring (merge-pathnames "drive/state/" dir)))
           (memory (namestring (merge-pathnames "memory/" dir)))
           (old-memory (sb-posix:getenv "XDG_RUNTIME_DIR"))
           (tuatara.io::*directories-in-memory* '()))
      (ensure-directories-exist chosen)
      (ensure-directories-exist memory)
      (unwind-protect
           (progn
             (sb-posix:setenv "XDG_RUNTIME_DIR" memory 1)
             (let ((tuatara-user:*cache-directory* chosen))
               (is string= chosen (namestring (tuatara.io:cache-directory))))
             (let ((tuatara-user:*state-directory* gone))
               (is string= (format nil "~atuatara/state/" memory) (namestring (tuatara.io:state-directory)))
               (false (probe-file (merge-pathnames "drive/" dir)))
               (true (search "(*state-directory*) is not there: kept in memory"
                             (first (tuatara.io:directories-in-memory-words)))))
             (let ((tuatara-user:*packages-directory* chosen))
               (is string= chosen (tuatara.defaults::packages-directory)))
             ;; unset, the standard places
             (true (search "tuatara/" (namestring (tuatara.io:state-directory)))))
        (if old-memory (sb-posix:setenv "XDG_RUNTIME_DIR" old-memory 1) (sb-posix:unsetenv "XDG_RUNTIME_DIR"))))))

(define-test (state-and-files path-helpers)
  ;; the one way of each: the folder paths share, a file: address, a folder
  ;; kept safe, a project's top found by a pattern, a glob, the buffer
  ;; visiting a file, a file holding octets, a size walk let go
  (is string= "/a/" (tuatara.defaults::paths-common-directory '("/a/b/c.txt" "/a/bc/d.txt" "/a/x")))
  (is string= "/a/b/" (tuatara.defaults::paths-common-directory '("/a/b/c.txt" "/a/b/sub/")))
  (is string= "/" (tuatara.defaults::paths-common-directory '("/a/x" "/b/y")))
  (is string= "/x" (tuatara.defaults::uri-path (format nil "file://~a/x" (machine-instance)) :hosts (list (machine-instance))))
  (false (tuatara.defaults::uri-path (format nil "file://~a/x" (machine-instance))))
  (is string= "/x" (tuatara.defaults::uri-path "FILE:///x"))
  (false (tuatara.defaults::uri-path "file://elsewhere/x" :hosts (list (machine-instance))))
  (true (tuatara.defaults::glob-match "**/*.{ts,js}" "src/a/b.js" :braces t))
  (false (tuatara.defaults::glob-match "**/*.{ts,js}" "src/a/b.js"))
  (true (tuatara.defaults::glob-match "a\\*b" "a*b"))
  (with-test-directory (dir)
    (let* ((root (tuatara.io:native dir))
           (evidence (concatenate 'string root "evidence/"))
           (project (concatenate 'string root "solution/"))
           (deep (concatenate 'string project "src/deep/")))
      (ensure-directories-exist (sb-ext:parse-native-namestring (concatenate 'string evidence "x")))
      (ensure-directories-exist (sb-ext:parse-native-namestring (concatenate 'string deep "x")))
      (put-file (sb-ext:parse-native-namestring (concatenate 'string project "app.sln")) "")
      (let ((tuatara-user:*safe-directories* (list evidence)))
        (true (tuatara.defaults::safe-directory-of evidence))
        (true (tuatara.defaults::safe-directory-of (concatenate 'string evidence "inside.txt")))
        (false (tuatara.defaults::safe-directory-of project)))
      (is string= project (tuatara.defaults::marked-directory deep '("*.sln")))
      (false (tuatara.defaults::marked-directory deep '("*.csproj") :below-home t))
      (let ((file (concatenate 'string project "app.sln")))
        (true (tuatara.defaults::file-holds-p file (make-array 0 :element-type '(unsigned-byte 8))))
        (put-file (sb-ext:parse-native-namestring file) "same")
        (true (tuatara.defaults::file-holds-p file (tuatara.text:string-octets "same")))
        (false (tuatara.defaults::file-holds-p file (tuatara.text:string-octets "samE")))
        (false (tuatara.defaults::file-holds-p file (tuatara.text:string-octets "same and more")))
        (let ((buffer (tuatara.buffer:make-buffer "visiting app.sln")))
          (unwind-protect
               (progn (setf (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path file))
                      (is eq buffer (tuatara.defaults::buffer-visiting file))
                      (false (tuatara.defaults::buffer-visiting file :except buffer)))
            (tuatara.buffer:kill-buffer buffer))))
      (dotimes (i 70) (ensure-directories-exist (sb-ext:parse-native-namestring (format nil "~ad~d/x" root i))))
      (false (tuatara.defaults::rimu-tree-size root :wanted (constantly nil)))
      (true (tuatara.defaults::rimu-tree-size root :wanted (constantly t))))))

(define-test (state-and-files stream-octets-read-whole)
  ;; a program's output read in pieces as one vector: all of it, more than the limit refused,
  ;; stopped when asked, the same with its pieces wiped
  (with-test-directory (dir)
    (let* ((octets (let ((v (make-array 200000 :element-type '(unsigned-byte 8))))
                     (dotimes (i (length v) v) (setf (aref v i) (mod (* i 7) 256)))))
           (path (merge-pathnames "out.bin" dir)))
      (alexandria:write-byte-vector-into-file octets path)
      (flet ((read-it (&rest keys)
               (with-open-file (s path :element-type '(unsigned-byte 8))
                 (multiple-value-list (apply #'tuatara.io:read-stream-octets s keys)))))
        (destructuring-bind (read too-large cancelled) (read-it)
          (true (equalp octets read))
          (false too-large)
          (false cancelled))
        (true (equalp octets (first (read-it :wipe t :limit 200000))))
        (is equal '(nil t nil) (read-it :limit 1000 :wipe t))
        (destructuring-bind (read too-large cancelled) (read-it :cancel (list t))
          (is = 0 (length read))
          (false too-large)
          (true cancelled))))))

(define-test (state-and-files path-one-file-name)
  ;; a path written as one name: %XX but for letters, digits and ._-; a long one shortened with a
  ;; number made from all of it, so that an index's cache of a deep folder can still be written
  (is equal '("%2Fa%20b%2Fc.txt" nil) (multiple-value-list (tuatara.io:path-file-name "/a b/c.txt")))
  (is = 2819150120103270219 (tuatara.text:string-hash "abc"))
  (let* ((root (format nil "/~a/" (make-string 400 :initial-element #\r)))
         (other (format nil "/~a/" (make-string 401 :initial-element #\r))))
    (multiple-value-bind (name shortened) (tuatara.io:path-file-name root)
      (true shortened)
      (true (<= (length (tuatara.text:string-octets name)) 255))
      (false (string= name (tuatara.io:path-file-name other))))
    (true (<= (length (tuatara.text:string-octets (tuatara.defaults::index-cache-name root))) 255))
    (is string= (concatenate 'string (tuatara.defaults::auto-save-directory) (tuatara.io:path-file-name root))
        (tuatara.defaults::auto-save-path root))))

(define-test (state-and-files walk-directory-policies)
  ;; one walk of the folders under a folder: depth first or breadth first, links followed only
  ;; when asked, a depth and a number of folders at most, stopped when asked
  (with-test-directory (dir)
    (let ((root (tuatara.io:native dir)))
      (dolist (file '("a/x.txt" "a/deep/y.txt" "b/z.txt" ".h/w.txt"))
        (put-file (ensure-directories-exist (merge-pathnames file dir)) "x"))
      (sb-posix:symlink "a" (concatenate 'string root "link"))
      (flet ((entered (&rest keys)
               (let ((folders '()))
                 (apply #'tuatara.defaults::walk-directory root
                        (lambda (directory relative entry)
                          (declare (ignore directory))
                          (if entry (eq (second entry) :directory) (push relative folders)))
                        keys)
                 (nreverse folders))))
        (is equal '("" ".h/" "a/" "a/deep/" "b/") (entered))
        (is equal '("" ".h/" "a/" "b/" "a/deep/") (entered :breadth-first t))
        (is equal '("" ".h/" "a/" "a/deep/" "b/" "link/" "link/deep/") (entered :follow-links t))
        (is equal '("" ".h/" "a/" "b/") (entered :max-depth 1))
        (is equal '("" ".h/") (entered :limit 2))
        (is equal '() (entered :cancel (list t)))
        (false (tuatara.defaults::walk-directory root (constantly t) :limit 2)))
      ;; the language servers' watch: nearest folders first, hidden ones left out, as many as allowed
      (let ((tuatara.defaults::*lsp-watched-most* 3))
        (multiple-value-bind (folders stamps) (tuatara.defaults::project-folders root)
          (is equal (list root (concatenate 'string root "a/") (concatenate 'string root "b/")) folders)
          (is eq :directory (gethash "deep" (gethash (concatenate 'string root "a/") stamps)))
          (true (gethash ".h" (gethash root stamps)))))
      ;; files under a folder keep their order, a folder's files in place among its folders
      (sb-posix:unlink (concatenate 'string root "link"))
      (is equal '("a/deep/y.txt" "a/x.txt" "b/z.txt")
          (mapcar #'cdr (tuatara.defaults::files-under root)))
      (is = 2 (tuatara.defaults::rimu-tree-size (concatenate 'string root "a/")))
      (is = 4 (tuatara.defaults::rimu-tree-size root)))))

(define-test (state-and-files gitignore-globs-end-quickly)
  ;; a .gitignore line planted to make the matcher step back for ever is matched at once
  (let ((rules (list (cons (tuatara.defaults::gitignore-rule "*a*a*a*a*a*a*a*a*c*b") "/top/")
                     (cons (tuatara.defaults::gitignore-rule "/**a**a**a**a**a**a**a**a**c**b") "/top/")))
        (name (format nil "~a~a" (make-string 60 :initial-element #\a) "b"))
        (start (thread-cpu-time)))
    (false (tuatara.defaults::gitignored-p rules name (concatenate 'string "/top/" name) nil))
    (true (< (- (thread-cpu-time) start) (/ internal-time-units-per-second 4))))
  ;; git's stars: * and ? within a folder's name, ** across folders, **/ none too
  (dolist (case '(("**/x" "x" t) ("**/x" "a/b/x" t) ("a/**/b" "a/b" t) ("a/**/b" "a/x/y/b" t) ("a/*" "a/b/c" nil)
                  ("a/*" "a/b" t) ("a/**" "a/b/c" t) ("a?b" "a/b" nil) ("*.o" "lib/x.o" nil) ("**.o" "lib/x.o" t)))
    (destructuring-bind (glob path expected) case
      (is eq expected (tuatara.defaults::glob-match glob path) "glob-match ~s ~s" glob path)))
  (let ((rules (list (cons (tuatara.defaults::gitignore-rule "/build/*.o") "/top/")
                     (cons (tuatara.defaults::gitignore-rule "*.log") "/top/"))))
    (true (tuatara.defaults::gitignored-p rules "x.o" "/top/build/x.o" nil))
    (false (tuatara.defaults::gitignored-p rules "x.o" "/top/src/build/x.o" nil))
    (true (tuatara.defaults::gitignored-p rules "a.log" "/top/deep/a.log" nil)))
  ;; braces, one inside another too, and too many of them read as nothing
  (is equal '("a.ts" "a.js") (tuatara.defaults::glob-alternatives "a.{ts,js}"))
  (is equal '("ab" "acd" "ace") (tuatara.defaults::glob-alternatives "a{b,c{d,e}}"))
  (is equal '("[{]x") (tuatara.defaults::glob-alternatives "[{]x"))
  (false (tuatara.defaults::glob-alternatives "{a,b}{a,b}{a,b}{a,b}{a,b}{a,b}{a,b}{a,b}{a,b}")))

(define-test (state-and-files glob-class-negation-and-escapes)
  ;; one reading of a class for both globs: ^ negates as ! does, as git reads them, and a ] or a \
  ;; escaped inside a class is a member, written escaped in the regular expression
  (dolist (case '(("[^a]x" "bx" t) ("[^a]x" "ax" nil) ("[!a]x" "bx" t) ("[a\\]]x" "]x" t) ("[a\\]]x" "ax" t)
                  ("[a\\]]x" "bx" nil) ("[\\\\]x" "\\x" t) ("[\\\\]x" "ax" nil) ("[a^]x" "^x" t) ("[a-]x" "-x" t)
                  ("[[]x" "[x" t) ("[z-a]x" "mx" nil) ("[.]x" "ax" nil)))
    (destructuring-bind (glob name expected) case
      (is eq expected (tuatara.defaults::glob-match glob name) "glob-match ~s ~s" glob name)
      (is eq expected (tuatara.defaults::glob-match glob name :braces t) "glob-match :braces ~s ~s" glob name)))
  (true (tuatara.defaults::glob-match "[^A-C]*" "d.txt" :test #'char-equal))
  (false (tuatara.defaults::glob-match "[^A-C]*" "b.txt" :test #'char-equal)))

(define-test (state-and-files numbers-read-without-reader)
  ;; numbers kept between sessions read as digits alone: never a symbol made, never anything run
  (is eql 0.5d0 (tuatara.defaults::session-number "0.500000"))
  (is eql 12 (tuatara.defaults::session-number "12"))
  (is eql 0.123456789012d0 (tuatara.defaults::session-number "0.123456789012"))
  (false (tuatara.defaults::session-number "1e3"))
  (false (tuatara.defaults::session-number "#.(+ 1 2)"))
  (is equal '("/tmp/a.pdf" 3 :width 1.25) (tuatara.defaults::parse-pdf-place-line "3 width 1.250 /tmp/a.pdf"))
  (let* ((*package* (find-package :tuatara.tests))
         (name (format nil "ZZPLACE~36r" (random (expt 36 8) (make-random-state t))))
         (entry (tuatara.defaults::parse-pdf-place-line (format nil "2 page ~a /tmp/b.pdf" (string-downcase name)))))
    (is eql 1 (fourth entry))
    (false (find-symbol name :tuatara.tests))
    (is eql 1 (fourth (tuatara.defaults::parse-pdf-place-line "2 page #.(error\"run\") /tmp/b.pdf")))))
