;;;; test/evidence.lisp --- Evidence directories: opened safely, and nothing written in them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test evidence
  :description "Evidence directories: the entries of *evidence-directories* matched by globs, names and patterns, links and mounts followed; every write there refused in one wording; files there opened safely and marked in the mode line; copies out checked and logged; nothing looked at while the setting is empty.")

(defmacro with-evidence-directories ((value) &body body)
  "Run BODY with *EVIDENCE-DIRECTORIES* set to VALUE for every thread, set back after."
  (let ((old (gensym)))
    `(let ((,old tuatara-user:*evidence-directories*))
       (unwind-protect (progn (setf tuatara-user:*evidence-directories* ,value) ,@body)
         (setf tuatara-user:*evidence-directories* ,old)))))

(defun wait-for-evidence-links ()
  "Wait, ten seconds at most, until the links the entries of *EVIDENCE-DIRECTORIES*
match have been looked for in the background."
  (loop repeat 1000
        until (let ((rules (tuatara.defaults::evidence-rules)))
                (or (null rules) (tuatara.defaults::evidence-rules-linked rules)))
        do (sleep 1/100)))

(defun wait-for-evidence-generation (after)
  "Wait, ten seconds at most, until *EVIDENCE-DIRECTORIES* has been read anew since
generation AFTER."
  (loop repeat 1000
        until (> (or (tuatara.defaults::evidence-generation) 0) after)
        do (sleep 1/100)))

(defmacro with-case-directory ((root evidence work) &body body)
  "Run BODY in a test directory ROOT holding cases/2026-0001/evidence/ (EVIDENCE,
with a.txt and b.pdf in it, and a directory sub/ holding c.txt) and
cases/2026-0001/work/ (WORK), each bound to its native name ending in /."
  `(with-test-directory (,root)
     (let* ((,root (namestring ,root))
            (,evidence (namestring (ensure-directories-exist (merge-pathnames "cases/2026-0001/evidence/sub/" ,root))))
            (,evidence (subseq ,evidence 0 (- (length ,evidence) 4)))
            (,work (namestring (ensure-directories-exist (merge-pathnames "cases/2026-0001/work/" ,root)))))
       (declare (ignorable ,work))
       (put-file (concatenate 'string ,evidence "a.txt") "kept as found")
       (put-file (concatenate 'string ,evidence "b.pdf") "%PDF-1.4 not really")
       (put-file (concatenate 'string ,evidence "sub/c.txt") "deeper")
       (unwind-protect (progn ,@body)
         (kill-buffers-visiting ,root)
         ;; the evidence made writable again, so that the test directory can go
         (setf tuatara-user:*evidence-directories* nil)))))

(defun evidence-directory-of (path)
  "The evidence directory PATH is in, or NIL (EVIDENCE-PATH-P)."
  (values (tuatara.defaults::evidence-path-p path)))

(define-test (evidence matched-by-globs-names-and-patterns)
  (with-case-directory (root evidence work)
    (let ((file (concatenate 'string evidence "a.txt"))
          (deeper (concatenate 'string evidence "sub/c.txt")))
      ;; empty: nothing is evidence, and nothing is looked at
      (with-evidence-directories ('())
        (false (evidence-directory-of file))
        (false (tuatara.io:writing-refused-anywhere-p)))
      (dolist (setting (list (list (concatenate 'string root "cases/*/evidence/"))
                             (list (concatenate 'string root "**/evidence"))
                             (concatenate 'string root "cases/*/evidence/") ; one entry alone
                             (list (list :name "evidence" :within (concatenate 'string root "cases/")))
                             (list (list :name "evidence"))
                             (list (list :regex
                                         "^[0-9]{4}-[0-9]{4}$"
                                         :within (concatenate 'string root "cases/")
                                         :then "evidence"))
                             ;; names compared without regard to case
                             (list (concatenate 'string root "CASES/*/Evidence/"))
                             (list (list :name "EVIDENCE"))))
        (with-evidence-directories (setting)
          (is string= evidence (evidence-directory-of file) "~s" setting)
          (is string= evidence (evidence-directory-of deeper) "~s" setting)
          (is string= evidence (evidence-directory-of evidence) "the directory itself: ~s" setting)
          (false (evidence-directory-of (concatenate 'string work "notes.org")) "~s" setting)
          (false (evidence-directory-of (concatenate 'string root "cases/2026-0001/")) "~s" setting)
          (true (tuatara.io:writing-refused-anywhere-p))))
      ;; the entry that matched is told as written
      (let ((entry (concatenate 'string root "cases/*/evidence/")))
        (with-evidence-directories (`("/nowhere/*/evidence/" ,entry))
          (is equal entry (nth-value 1 (tuatara.defaults::evidence-path-p file)))))
      ;; a pattern that matches only part of a name, or a directory outside :within, is no evidence
      (with-evidence-directories (`((:regex "^[0-9]{4}$" :within ,(concatenate 'string root "cases/") :then "evidence")
                                (:name "evidence" :within "/nowhere/")))
        (false (evidence-directory-of file))))))

(define-test (evidence entries-read-or-said)
  ;; an entry that cannot be read is left out and said; the others still count
  (with-case-directory (root evidence work)
    (with-evidence-directories (`("relative/evidence/" (:regex "([" :within "/") (:name "a/b") (:then "x") 42
                             ,(concatenate 'string root "cases/*/evidence/")))
      (let ((problems (tuatara.defaults::evidence-problems-words)))
        (is = 5 (length problems))
        (true (every (lambda (words) (search "is left out" words)) problems))
        (true (find-if (lambda (words) (search ":regex \"([\" cannot be used" words)) problems)))
      (is string= evidence (evidence-directory-of (concatenate 'string evidence "a.txt")))
      (true (find-if (lambda (line) (search "not read, left out" line)) (tuatara.defaults::evidence-health-lines))))
    ;; check-health: none (off), or each entry with what it matches
    (with-evidence-directories ('())
      (is equal '("Evidence directories: none (off)") (tuatara.defaults::evidence-health-lines)))
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")
                              ,(concatenate 'string root "nothing/*/here/")))
      (let ((lines (tuatara.defaults::evidence-health-lines)))
        (true (search "2 entries" (first lines)))
        (true (search "1 directory, " (second lines)))
        (true (search "matches nothing" (third lines)))))))

(define-test (evidence links-and-mounts-followed)
  (with-case-directory (root evidence work)
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (let ((link (concatenate 'string work "link-to-evidence"))
            (file-link (concatenate 'string work "a-link.txt")))
        (sb-posix:symlink (string-right-trim "/" evidence) link)
        (sb-posix:symlink (concatenate 'string evidence "a.txt") file-link)
        ;; through a link to the directory, and through .. on the way
        (is string= evidence (evidence-directory-of (concatenate 'string link "/a.txt")))
        (is string= evidence (evidence-directory-of (concatenate 'string work "../evidence/a.txt")))
        ;; a link elsewhere leading to a file of evidence is the link's own place: taking it away is no write there
        (false (evidence-directory-of file-link))
        (false (tuatara.io:writing-refusal file-link))
        ;; writing through it is: the save goes to where it leads
        (fail (tuatara.io:write-text-file (tuatara.text:string->rope "changed") file-link) 'tuatara.io:writing-refused)
        (is string= "kept as found" (file-text (concatenate 'string evidence "a.txt")))
        ;; a directory opened is judged by where it really is, whatever path led to it
        (let ((fd (sb-posix:open link (logior sb-posix:o-rdonly sb-posix:o-directory))))
          (unwind-protect (true (tuatara.io:writing-refusal (concatenate 'string work "elsewhere.txt") :dirfd fd))
            (sb-posix:close fd))))
      ;; the same directory mounted at a second place is evidence there too
      (let ((info (concatenate 'string root "mountinfo"))
            (other (namestring (ensure-directories-exist (merge-pathnames "mounted-again/" root)))))
        (put-file info (format nil "1 0 0:900 / / rw - x x rw~%2 1 0:901 / ~a rw - x x rw~%3 1 0:901 / ~a rw - x x rw~%"
                               (string-right-trim "/" (concatenate 'string root "cases"))
                               (string-right-trim "/" other)))
        (let ((tuatara.defaults::*mounts-info-file* info))
          (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
            (is string= evidence (evidence-directory-of (concatenate 'string other "2026-0001/evidence/a.txt")))
            (false (evidence-directory-of (concatenate 'string other "2026-0001/work/a.txt")))))))))

(defmacro refused (form)
  "True when FORM signals WRITING-REFUSED saying the evidence rule."
  `(handler-case (progn ,form nil)
     (tuatara.io:writing-refused (e)
       (and (search "is an evidence directory (*evidence-directories*: " (princ-to-string e)) t))))

(defmacro refused-any (form)
  "True when FORM signals WRITING-REFUSED."
  `(handler-case (progn ,form nil) (tuatara.io:writing-refused () t)))

(define-test (evidence every-write-refused)
  (with-case-directory (root evidence work)
    (let* ((a (concatenate 'string evidence "a.txt"))
           (outside (put-file (concatenate 'string work "out.txt") "outside"))
           (outside (namestring outside))
           (before (directory-listing-of root)))
      (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
        (true (refused (tuatara.io:write-text-file (tuatara.text:string->rope "x") a)))
        (true (refused (tuatara.io:write-text-file (tuatara.text:string->rope "x")
                                                   (concatenate 'string evidence "new.txt"))))
        (true (refused (tuatara.io:replace-file a "x")))
        (true (refused (tuatara.io:write-new-file (concatenate 'string evidence "new.txt") "x")))
        (true (refused (tuatara.io:write-private-file (concatenate 'string evidence "new.txt") "x")))
        (true (refused (tuatara.io:copy-regular-file outside (concatenate 'string evidence "in.txt"))))
        (true (refused (tuatara.io:copy-path-tree outside (concatenate 'string evidence "in.txt"))))
        (true (refused (tuatara.io:copy-to-free-name outside evidence "in.txt")))
        (true (refused (tuatara.io:call-with-free-file evidence "in.mbox" (lambda (fd path) (list fd path)))))
        (true (refused (tuatara.io:append-to-file (concatenate 'string evidence "journal") "x")))
        (true (refused (tuatara.io:rename-path a (concatenate 'string work "a.txt"))))
        (true (refused (tuatara.io:rename-path outside (concatenate 'string evidence "out.txt"))))
        (true (refused (tuatara.io:move-path a (concatenate 'string work "a.txt"))))
        (true (refused (tuatara.io:make-directory (concatenate 'string evidence "new/"))))
        (true (refused (tuatara.io:ensure-directories (concatenate 'string evidence "new/deeper/"))))
        (true (refused (tuatara.io:remove-file a)))
        (true (refused (tuatara.io:touch-path a)))
        (true (refused (tuatara.io:make-symbolic-link "/etc/hostname" (concatenate 'string evidence "l"))))
        (true (refused (tuatara.io:make-hard-link outside (concatenate 'string evidence "h"))))
        ;; a second name elsewhere changes the file of evidence too
        (true (refused (tuatara.io:make-hard-link a (concatenate 'string work "h"))))
        (false (tuatara.defaults::rimu-start-operation :hardlink (list (cons a (concatenate 'string work "h2"))) nil))
        (true (refused (tuatara.io:wipe-file a)))
        (true (refused (tuatara.io:delete-tree a)))
        (true (refused (tuatara.io:trash-path a)))
        (true (refused (tuatara.io:open-scratch-file evidence)))
        (true (refused (tuatara.io:create-free-file evidence (lambda (n) (format nil "f~d" n)))))
        ;; the directory itself, and a directory holding it, are not deleted, moved or renamed
        (true (refused (tuatara.io:delete-tree evidence)))
        (true (refused (tuatara.io:delete-tree (concatenate 'string root "cases/"))))
        (true (refused (tuatara.io:rename-path (concatenate 'string root "cases/2026-0001")
                                               (concatenate 'string root "moved"))))
        (true (refused (tuatara.io:trash-path (concatenate 'string root "cases/"))))
        ;; nor a program let write in it
        (true (refused (tuatara.io::mount-arguments '()
                                                    (list (string-right-trim "/" (concatenate 'string root "cases")))
                                                    '())))
        ;; the editor's own directories are not put there, and a state file's lock is not made there
        (let ((tuatara-user:*cache-directory* evidence))
          (true (refused (tuatara.io:cache-directory))))
        (false (tuatara.io:call-with-state-lock (concatenate 'string evidence "recent") (lambda () t)))
        ;; and it is not writable, so that nothing tries
        (false (tuatara.io:writable-p evidence))
        (false (tuatara.io:writable-p a))
        ;; next to it, all as usual
        (tuatara.io:write-new-file (concatenate 'string work "new.txt") "fine")
        (tuatara.io:copy-regular-file a (concatenate 'string work "copy.txt"))
        (is string= "kept as found" (file-text (concatenate 'string work "copy.txt")))
        (true (tuatara.io:writable-p work)))
      ;; nothing in the evidence changed, nothing added
      (is equal (remove-if (lambda (p) (search "/work/" p)) before)
          (remove-if (lambda (p) (search "/work/" p)) (directory-listing-of root))))))

(defun directory-listing-of (root)
  "Every file and directory under ROOT with its contents, sorted, as strings."
  (let ((found '()))
    (uiop:collect-sub*directories (pathname root) t t
                                  (lambda (dir)
                                    (push (namestring dir) found)
                                    (dolist (file (uiop:directory-files dir))
                                      (push (format nil
                                                    "~a=~a"
                                                    (namestring file)
                                                    (ignore-errors (file-text file))) found))))
    (sort found #'string<)))

(define-test (evidence opened-safely-and-marked)
  (with-case-directory (root evidence work)
    (with-fresh-recent-files
      (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
        (let ((a (concatenate 'string evidence "a.txt")))
          ;; C-x C-f opens it safely by itself, and says why
          (multiple-value-bind (f editor)
              (run-keys (format nil "C-x C-f C-a C-k ~a RET" (key-string a)) :width 700 :height 12)
            (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
              (true (tuatara.io:safe-visit-p b))
              (true (tuatara.buffer:buffer-local b :read-only))
              (true (tuatara.frontend.fake:fake-frame-showing f "a.txt opened safely: it is in the evidence directory"))
              (true (tuatara.frontend.fake:fake-frame-showing f "it stays so, being evidence"))
              ;; Evidence in its own face, before the name
              (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f " Evidence ")
                (let* ((row (position-if (lambda (r) (search " Evidence " r)) screen))
                       (at (search " Evidence " (aref screen row))))
                  (true (find-if (lambda (span)
                                   (and (= (first span) at)
                                        (member :mode-line-evidence (alexandria:ensure-list (third span)))))
                                 (aref spans row)))
                  (true (< at (search "a.txt" (aref screen row))))))))
          (kill-buffers-visiting root)
          ;; it stays opened safely
          (true (tuatara.frontend.fake:fake-frame-showing
                 (run-keys (format nil "C-x C-f C-a C-k ~a RET M-x e n d - s a f e - v i s i t RET" (key-string a))
                           :width 700
                           :height 12)
                 "a.txt stays opened safely: it is in the evidence directory"))
          (kill-buffers-visiting root)
          ;; M-x describe-evidence says yes, and by which entry; no for a file beside it
          (let ((f (run-keys (format nil
                                     "C-x C-f C-a C-k ~a RET M-x d e s c r i b e - e v i d e n c e RET"
                                     (key-string a)) :width 700 :height 12)))
            (true (tuatara.frontend.fake:fake-frame-showing f "Yes: "))
            (true (tuatara.frontend.fake:fake-frame-showing f "cases/*/evidence/\")")))
          (kill-buffers-visiting root)
          (true (tuatara.frontend.fake:fake-frame-showing
                 (run-keys "M-x d e s c r i b e - e v i d e n c e RET"
                           :file (namestring (put-file (concatenate 'string work "w.txt") "w"))
                                                                         :width 700 :height 12)
                 "is in no evidence directory"))
          (kill-buffers-visiting root)
          ;; C-x C-w of a file elsewhere into the evidence directory is refused, said in the one wording
          (let ((f (run-keys (format nil "C-x C-w C-a C-k ~a RET" (key-string (concatenate 'string evidence "w.txt")))
                             :file (concatenate 'string work "w.txt") :width 700 :height 12)))
            (true (tuatara.frontend.fake:fake-frame-showing f "is an evidence directory (*evidence-directories*: "))
            (false (probe-file (concatenate 'string evidence "w.txt")))))))))

(define-test (evidence setting-changed-after-opening)
  ;; a file opened before its directory was named evidence gets the rule once it is
  (with-case-directory (root evidence work)
    (let ((a (concatenate 'string evidence "a.txt")))
      (multiple-value-bind (f editor)
          (run-keys (list "i x C-g" :wait
                          (lambda ()
                            (setf tuatara-user:*evidence-directories*
                                  (list (concatenate 'string root "cases/*/evidence/"))))
                          :wait "C-x C-s")
                    :file a :width 700 :height 12)
        (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (true (tuatara.buffer:buffer-local b :read-only))
          (true (tuatara.frontend.fake:fake-frame-showing f "is in the evidence directory"))
          (true (tuatara.frontend.fake:fake-frame-showing f " Evidence "))))
      (is string= "kept as found" (file-text a)))))

(define-test (evidence copied-out-checked-and-logged)
  (with-case-directory (root evidence work)
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (let* ((a (concatenate 'string evidence "a.txt"))
             (b (concatenate 'string evidence "b.pdf"))
             (logged nil))
        (tuatara.defaults::copy-from-evidence-work (list (cons a (concatenate 'string work "a.txt"))
                                                         (cons b (concatenate 'string work "b.pdf")))
                                                   work (list nil) (lambda (log) (setf logged log)))
        (is string= "kept as found" (file-text (concatenate 'string work "a.txt")))
        (true logged)
        (is string= work (tuatara.io:parent-path logged))
        (true (search "copied-from-evidence-" (tuatara.io:native-file-name logged)))
        ;; the log says what, where from, the sums and when; read-only
        (let ((text (file-text logged))
              (sum (tuatara.defaults::file-sha256 a)))
          (true (search (format nil "From: ~a" a) text))
          (true (search (format nil "SHA-256 of the original: ~a" sum) text))
          (true (search (format nil "SHA-256 of the copy:     ~a" sum) text))
          (true (search "Size: 13 octets" text))
          (true (search "When: " text))
          (true (search "Evidence directory: " text))
          (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp logged)))))
        ;; a copy whose sum differs is an error naming it
        (fail (tuatara.defaults::copy-file-checked a (concatenate 'string work "again.txt") :sum "0000") 'error)
        (false (probe-file (concatenate 'string work "again.txt")) "an untrue copy is taken away")
        ;; never into an evidence directory
        (true (tuatara.defaults::evidence-refusal "copy into" evidence))
        (true (refused (tuatara.defaults::copy-file-checked a (concatenate 'string evidence "sub/a.txt"))))
        ;; by the command: a directory asked for in the minibuffer
        (let ((f (run-keys (list (format nil "C-x C-f C-a C-k ~a RET M-x c o p y - f r o m - e v i d e n c e RET C-a C-k ~a RET"
                                         (key-string b) (key-string (namestring (ensure-directories-exist (concatenate 'string work "out/")))))
                                 :wait)
                           :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "each copy's SHA-256 checked against its original"))
          (is string= "%PDF-1.4 not really" (file-text (concatenate 'string work "out/b.pdf"))))
        (kill-buffers-visiting root)
        ;; into evidence by the command: refused, said
        (let ((f (run-keys (format nil
                                   "C-x C-f C-a C-k ~a RET M-x c o p y - f r o m - e v i d e n c e RET C-a C-k ~a RET"
                                   (key-string b) (key-string (concatenate 'string evidence "sub/")))
                           :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "is an evidence directory (*evidence-directories*: "))
          (false (probe-file (concatenate 'string evidence "sub/b.pdf"))))))))

(define-test (evidence rimu-and-export-refused)
  (with-case-directory (root evidence work)
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      ;; rimu says so before anything is begun
      (false (tuatara.defaults::rimu-start-operation :delete (list (list (concatenate 'string evidence "a.txt"))) nil))
      (false (tuatara.defaults::rimu-start-operation :copy (list (cons (concatenate 'string work "../evidence/a.txt")
                                                                       (concatenate 'string evidence "a2.txt")))
                                                     nil))
      ;; nor is an archive extracted into it
      (false (tuatara.defaults::rimu-start-operation :extract (list (cons (concatenate 'string work "found.zip")
                                                                          (concatenate 'string evidence "found/")))
                                                     nil))
      (false (probe-file (concatenate 'string evidence "found/")))
      (true (probe-file (concatenate 'string evidence "a.txt")))
      (kill-buffers-visiting root)
      ;; an Org note elsewhere exporting into it is refused before anything is made
      (let ((note (namestring (put-file (concatenate 'string work "note.org")
                                        (format nil "#+export_file_name: ../evidence/note~%* Heading~%text~%")))))
        (let ((f (run-keys "M-x o r g - e x p o r t - l a t e x RET" :file note :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "is an evidence directory (*evidence-directories*: ")))
        (false (probe-file (concatenate 'string evidence "note.tex")))))))

(define-test (evidence empty-setting-costs-nothing)
  ;; with no directory named, nothing is read from the disk and nothing is made for an answer
  (with-evidence-directories ('())
    (let ((path "/var/tmp/somewhere/file.txt"))
      (flet ((consed (n)
               (let ((before (sb-ext:get-bytes-consed)))
                 (dotimes (i n)
                   (tuatara.defaults::evidence-path-p path)
                   (tuatara.io:writing-refusal path)
                   (tuatara.io:writing-refused-anywhere-p))
                 (- (sb-ext:get-bytes-consed) before))))
        (consed 10)
        ;; the least of a few tries, other threads consing meanwhile
        (true (< (loop repeat 5 minimize (- (consed 100000) (consed 10000))) 90000)
              "more than an octet for each question while the setting is empty")))))

(define-test (evidence git-writes-refused-where-a-repository-holds-evidence)
  ;; git's commands that write working files are refused in a repository holding an evidence directory; reading ones run
  (true (tuatara.git::git-writes-work-tree-p '("checkout" "--" "a.txt")))
  (true (tuatara.git::git-writes-work-tree-p '("-c" "x=y" "reset" "--hard")))
  (true (tuatara.git::git-writes-work-tree-p '("stash" "push")))
  (true (tuatara.git::git-writes-work-tree-p '("apply" "--index" "-")))
  (true (tuatara.git::git-writes-work-tree-p '("restore" "--staged" "--worktree" "a.txt")))
  (true (tuatara.git::git-writes-work-tree-p '("a-command-not-known-yet")))
  ;; commit and add run the repository's clean filters on its working files
  (true (tuatara.git::git-writes-work-tree-p '("commit" "-m" "x")))
  (dolist (reading '(("status" "--porcelain") ("log" "-1") ("diff") ("blame" "a.txt") ("apply" "--cached" "-")
                     ("reset" "-q" "--" "a.txt") ("restore" "--staged" "a.txt") ("rm" "--cached" "a.txt")
                     ("stash" "list") ("format-patch" "--stdout" "-1")))
    (false (tuatara.git::git-writes-work-tree-p reading) "~s" reading))
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (evidence (namestring (ensure-directories-exist (merge-pathnames "evidence/" dir)))))
      (scratch-repository dir)
      (git-commit-file dir "evidence/kept.txt" "as found" "Evidence")
      (put-file (merge-pathnames "evidence/kept.txt" dir) "changed meanwhile by another program")
      (with-evidence-directories (`(,evidence))
        (dolist (writing '(("checkout" "--" "evidence/kept.txt") ("reset" "--hard") ("stash") ("clean" "-fdx")
                           ("restore" "evidence/kept.txt")))
          (handler-case (progn (tuatara.git:run-git root writing) (false t "~s ran" writing))
            (tuatara.io:writing-refused (e)
              (true (search "is an evidence directory (*evidence-directories*: " (princ-to-string e)) "~s" writing)
              (true (uiop:string-prefix-p (format nil "git ~a is not run: " (first writing)) (princ-to-string e))))))
        (is string= "changed meanwhile by another program" (file-text (merge-pathnames "evidence/kept.txt" dir)))
        ;; reading runs
        (multiple-value-bind (octets ok) (tuatara.git:run-git root '("status" "--porcelain"))
          (true ok)
          (true (search "evidence/kept.txt" (tuatara.text:octets-string octets)))))
      ;; with the setting empty, as before
      (with-evidence-directories ('())
        (true (nth-value 1 (tuatara.git:run-git root '("checkout" "--" "evidence/kept.txt"))))
        (is string= "as found" (file-text (merge-pathnames "evidence/kept.txt" dir)))))))

(define-test (evidence never-offered-to-write-in)
  ;; a question whose answer is written to never starts in an evidence directory nor offers one first; reading ones still may
  (with-case-directory (root evidence work)
    (with-fresh-recent-files
      (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
        (let ((a (concatenate 'string evidence "a.txt")))
          ;; the directory chosen last is evidence: offered for reading, not for writing
          (let ((seen '()))
            (run-keys (list (lambda ()
                              (tuatara.defaults::note-chosen-path evidence :directory t)
                              (setf seen (list (tuatara.defaults::path-question-start)
                                               (tuatara.defaults::path-question-start :writing t)
                                               (tuatara.defaults::writing-start (concatenate 'string evidence "b.pdf"))
                                               (tuatara.defaults::writing-start (concatenate 'string work "x.txt"))))))
                      :file a :width 700 :height 12)
            (destructuring-bind (reading writing moved kept) seen
              (true (search "/evidence/" reading))
              (false (search "/evidence/" writing))
              (is string= (concatenate 'string writing "b.pdf") moved)
              (is string= (concatenate 'string work "x.txt") kept)))
          (kill-buffers-visiting root)
          ;; from a file of evidence: C-x C-w starts elsewhere, C-x C-f where it is
          (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET C-x C-w" (key-string a)) :width 700 :height 12)))
            (let ((screen (tuatara.frontend.fake:fake-frame-with f "Write file: ")))
              (true screen)
              (false (find-if (lambda (row) (and (search "Write file: " row) (search "/evidence/" row))) screen))))
          (kill-buffers-visiting root)
          (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET C-x C-f" (key-string a)) :width 700 :height 12)))
            (true (find-if (lambda (row) (and (search "Find file: " row) (search "/evidence/" row)))
                           (tuatara.frontend.fake:fake-frame-with f "Find file: ")))))))))

(define-test (evidence named-through-a-link)
  ;; an evidence directory that is a link to a drive's directory protects that directory, however the setting names it
  (with-case-directory (root evidence work)
    (let* ((stick (namestring (ensure-directories-exist (merge-pathnames "media/STICK/" root))))
           (link (concatenate 'string root "cases/2026-0002/evidence")))
      (ensure-directories-exist (concatenate 'string root "cases/2026-0002/"))
      (put-file (concatenate 'string stick "found.txt") "on the stick")
      (sb-posix:symlink (string-right-trim "/" stick) link)
      (dolist (setting (list (list link)
                             (list (concatenate 'string root "cases/*/evidence/"))
                             (list (list :name "evidence" :within (concatenate 'string root "cases/")))
                             (list (list :regex
                                         "^[0-9]{4}-[0-9]{4}$"
                                         :within (concatenate 'string root "cases/")
                                         :then "evidence"))))
        (with-evidence-directories (setting)
          (wait-for-evidence-links)
          (true (refused (tuatara.io:write-new-file (concatenate 'string stick "new.txt") "x")) "~s" setting)
          (true (refused (tuatara.io:write-new-file (concatenate 'string link "/new.txt") "x")) "~s" setting)
          (true (evidence-directory-of (concatenate 'string stick "found.txt")) "~s" setting)
          (false (probe-file (concatenate 'string stick "new.txt")) "~s" setting)))
      (sb-posix:unlink link))))

(define-test (evidence whole-names-composed-alike)
  ;; a pattern matches a directory's whole name, its alternatives in any order; names compared in one Unicode form
  (with-case-directory (root evidence work)
    (let ((file (concatenate 'string evidence "a.txt")))
      (with-evidence-directories (`((:regex
                                 "2026|2026-0001"
                                 :within ,(concatenate 'string root "cases/")
                                 :then "evidence")))
        (is string= evidence (evidence-directory-of file)))
      (with-evidence-directories (`((:regex "2026" :within ,(concatenate 'string root "cases/") :then "evidence")))
        (false (evidence-directory-of file)))
      (let ((composed (namestring (ensure-directories-exist (merge-pathnames (format nil
                                                                                     "~cvidence/"
                                                                                     (code-char #xe9)) root)))))
        (with-evidence-directories (`((:name ,(format nil "e~cvidence" (code-char #x301)) :within ,root)))
          (true (evidence-directory-of (concatenate 'string composed "x.txt"))))))))

(define-test (evidence refused-by-the-directory-held-open)
  ;; each write asks again by the directory it holds open, so that a directory swapped for a link meanwhile leads
  ;; nowhere
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (kept (namestring (put-file (merge-pathnames "kept.txt" dir) "kept")))
           (tuatara.runtime:*host*
             (make-answering-host 'tuatara.io:host-writing-refusal
                                  (lambda (path &key dirfd fd)
                                    (cond ((null path) t)
                                          ((or dirfd fd) "refused by the directory held open"))))))
      (true (handler-case (progn (tuatara.io:write-new-file (concatenate 'string root "n.txt") "x") nil)
              (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:replace-file kept "x") nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:remove-file kept) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:touch-path kept) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:make-directory (concatenate 'string root "d/")) nil)
              (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:make-symbolic-link kept (concatenate 'string root "l")) nil)
              (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:rename-path kept (concatenate 'string root "r.txt")) nil)
              (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:create-free-file root (lambda (n) (format nil "f~d" n))) nil)
              (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:open-scratch-file root) nil) (tuatara.io:writing-refused () t)))
      (false (tuatara.io:call-with-state-lock (concatenate 'string root "state") (lambda () t)))
      (is string= "kept" (file-text kept))
      (is equal '("kept.txt") (mapcar #'file-namestring (uiop:directory-files dir))))
    ;; a relative path is judged from the process's own directory, as the system takes it
    (let ((here (sb-posix:getcwd)))
      (unwind-protect
           (progn (sb-posix:chdir (namestring dir))
                  (with-evidence-directories ((list (namestring dir)))
                    (true (tuatara.io:writing-refusal "relative.txt"))))
        (sb-posix:chdir here)))))

(define-test (evidence other-names-never-written-in-place)
  ;; a file with another name in evidence, made before, is saved as a file of its own, never through the shared file
  (with-case-directory (root evidence work)
    (let ((a (concatenate 'string evidence "a.txt"))
          (other (concatenate 'string work "a-hard.txt"))
          (third (concatenate 'string work "a-third.txt")))
      (sb-posix:link a other)
      (sb-posix:link a third)
      (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
        (tuatara.io:write-text-file (tuatara.text:string->rope "changed through another name") other)
        (is string= "kept as found" (file-text a))
        (is string= "changed through another name" (file-text other))
        (is = 2 (tuatara.io:stamp-nlink (tuatara.io:path-stamp a)))
        (true (refused-any (tuatara.io:touch-path third)))
        (is string= "kept as found" (file-text third))))))

(define-test (evidence mounts-changed-looked-at-again)
  ;; what is kept of each directory is dropped once the drives mounted change
  (with-case-directory (root evidence work)
    (let* ((info (concatenate 'string root "mountinfo"))
           (other (namestring (ensure-directories-exist (merge-pathnames "mounted-again/" root))))
           (file (concatenate 'string
                              (namestring (ensure-directories-exist (concatenate 'string other "2026-0001/evidence/")))
                              "a.txt"))
           (old-info tuatara.defaults::*mounts-info-file*))
      (put-file info (format nil "1 0 0:900 / / rw - x x rw~%"))
      ;; for every thread: the drives are looked at in the background
      (setf tuatara.defaults::*mounts-info-file* info)
      (unwind-protect
           (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
             (wait-for-evidence-links)
             (false (evidence-directory-of file))
             (let ((before (tuatara.defaults::evidence-generation)))
               (put-file info
                         (format nil
                                 "1 0 0:900 / / rw - x x rw~%2 1 0:901 / ~a rw - x x rw~%3 1 0:901 / ~a rw - x x rw~%"
                                 (string-right-trim "/" (concatenate 'string root "cases"))
                                 (string-right-trim "/" other)))
               (tuatara.defaults::note-mounts-changed)
               (wait-for-evidence-generation before)
               (wait-for-evidence-links))
             (is string= evidence (evidence-directory-of file)))
        (setf tuatara.defaults::*mounts-info-file* old-info)))))

(define-test (evidence mounts-not-read-again-at-rest)
  ;; with nothing said of the drives changing, the keeper reads nothing: the rules stay those in force
  (with-case-directory (root evidence work)
    (let ((info (concatenate 'string root "mountinfo"))
          (old-info tuatara.defaults::*mounts-info-file*))
      (put-file info (format nil "1 0 0:900 / / rw - x x rw~%"))
      (setf tuatara.defaults::*mounts-info-file* info)
      (unwind-protect
           (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
             (wait-for-evidence-links)
             (let ((before (tuatara.defaults::evidence-generation)))
               (put-file info (format nil "1 0 0:900 / / rw - x x rw~%2 1 0:901 / /mnt rw - x x rw~%"))
               (sleep 2.5)
               (is = before (tuatara.defaults::evidence-generation))))
        (setf tuatara.defaults::*mounts-info-file* old-info)))))

(define-test (evidence generation-counted-from-many-threads)
  ;; generations begun by several threads at once are each counted
  (let* ((before (car tuatara.defaults::*evidence-generation*))
         (threads (loop repeat 4
                        collect (sb-thread:make-thread
                                 (lambda () (loop repeat 100000 do (tuatara.defaults::next-evidence-generation)))))))
    (mapc #'sb-thread:join-thread threads)
    (is = (+ before 400000) (car tuatara.defaults::*evidence-generation*))))

(define-test (evidence failed-holding-look-counts-as-holding)
  ;; a look through a directory that fails leaves words saying so, never "being looked through" for good
  (with-case-directory (root evidence work)
    (with-evidence-directories ('((:name "evidence")))
      (wait-for-evidence-links)
      (sb-int:encapsulate 'tuatara.defaults::search-holding 'failing
                          (lambda (function &rest arguments)
                            (declare (ignore function arguments))
                            (error "the directory went away")))
      (unwind-protect
           (let ((words (tuatara.defaults::search-holding-or-why (tuatara.defaults::evidence-rules)
                                                                 (list (cons work nil)))))
             (true (search "could not all be looked through" words)))
        (sb-int:unencapsulate 'tuatara.defaults::search-holding 'failing)))))

(define-test (evidence unknown-answer-counts-as-evidence)
  ;; a file whose place cannot be told is shown and kept as evidence, not as a plain file
  (with-case-directory (root evidence work)
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (let ((b (tuatara.buffer:make-buffer "unknown-evidence")))
        (unwind-protect
             (progn
               (setf (tuatara.buffer:buffer-file b) (concatenate 'string work "w.txt"))
               (sb-int:encapsulate 'tuatara.defaults::evidence-path-p 'failing
                                   (lambda (function &rest arguments)
                                     (declare (ignore function arguments))
                                     (error "no answer")))
               (unwind-protect (is eq :unknown (tuatara.defaults::buffer-evidence b))
                 (sb-int:unencapsulate 'tuatara.defaults::evidence-path-p 'failing)))
          (tuatara.buffer:kill-buffer b))))))

(define-test (evidence inode-look-held-to-its-limit)
  ;; the look for files of several names goes through so many directories at most, then counts as not whole
  (with-case-directory (root evidence work)
    (dotimes (i 30) (ensure-directories-exist (format nil "~asub~d/" evidence i)))
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (let ((tuatara.defaults::*evidence-health-directories* 10))
        (false (car (tuatara.defaults::look-for-evidence-inodes (tuatara.defaults::evidence-rules))))))))

(define-test (evidence git-writes-nowhere-in-evidence)
  ;; git's output files, other directories and repositories named, are refused in evidence; hooks off where a repository holds it
  (with-test-directory (dir)
    (let* ((repo (namestring (ensure-directories-exist (merge-pathnames "repo/" dir))))
           (ev (namestring (ensure-directories-exist (merge-pathnames "ev/" dir))))
           (inner (namestring (ensure-directories-exist (merge-pathnames "ev/inner/" dir)))))
      (scratch-repository repo)
      (scratch-repository inner)
      (with-evidence-directories ((list ev))
        (dolist (arguments `(("format-patch" "-o" ,ev "-1")
                             ("format-patch" ,(format nil "--output-directory=~a" ev) "-1")
                             ("bundle" "create" ,(concatenate 'string ev "b.bundle") "--all")
                             ("log" ,(format nil "--output=~ax.txt" ev))
                             ("diff" "--output" ,(concatenate 'string ev "d.txt"))
                             ("config" "--file" ,(concatenate 'string ev "cfg") "a.b" "c")
                             ("worktree" "add" ,(concatenate 'string ev "wt"))
                             ("-C" ,inner "status") (,(format nil "--work-tree=~a" ev) "checkout" "--" "a.txt")
                             (,(format nil "--git-dir=~a.git" inner) "log")))
          (true (refused-any (tuatara.git:run-git repo arguments)) "~s" arguments))
        (is equal '("inner") (mapcar (lambda (p) (car (last (pathname-directory p)))) (uiop:subdirectories ev)))
        (true (nth-value 1 (tuatara.git:run-git repo '("log" "-1"))))
        ;; a repository holding evidence: no commit (its filters), its hooks never run
        (let ((holding (namestring dir)))
          (scratch-repository holding)
          (true (refused-any (tuatara.git:run-git holding '("commit" "-m" "x" "--allow-empty"))))
          (is string= (format nil "/dev/null~%")
              (tuatara.text:octets-string (tuatara.git:run-git holding '("config" "--get" "core.hooksPath")))))))))

(define-test (evidence projects-holding-evidence-untrusted)
  ;; a project holding an evidence directory runs no program of its own, trusted or not
  (with-case-directory (root evidence work)
    (let ((tuatara-user:*trusted-projects* (list root)))
      (with-evidence-directories ('())
        (true (tuatara.defaults::project-trusted-p root)))
      (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
        (false (tuatara.defaults::project-trusted-p root))
        (true (tuatara.defaults::project-trusted-p work))))))

(define-test (evidence holding-told-without-reading-every-directory)
  ;; whether a project holds evidence is told from the setting: a big tree beside the cases is not read
  (with-case-directory (root evidence work)
    (dotimes (i 40)
      (dotimes (j 60)
        (ensure-directories-exist (format nil "~asrc/m~d/p~d/" root i j))))
    (scratch-repository root)
    (flet ((seconds (function)
             (let ((start (get-internal-real-time)))
               (funcall function)
               (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
      (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
        (wait-for-evidence-links)
        (let (answer)
          (true (< (seconds (lambda () (setf answer (tuatara.defaults::directory-holds-evidence-p root)))) 1/2))
          (true (search "is an evidence directory" answer)))
        (false (tuatara.defaults::directory-holds-evidence-p (concatenate 'string root "src/")))
        (let ((tuatara-user:*trusted-projects* (list root)))
          (true (< (seconds (lambda () (tuatara.defaults::project-trusted-p (concatenate 'string root "src/")))) 1/2)))
        (true (< (seconds (lambda () (tuatara.git:run-git root '("status" "--porcelain")))) 5)))
      (with-evidence-directories ('("/nowhere/*/evidence/"))
        (wait-for-evidence-links)
        (true (< (seconds (lambda () (tuatara.defaults::directory-holds-evidence-p root))) 1/10))
        (false (tuatara.defaults::directory-holds-evidence-p root)))
      ;; where only reading can tell, a tree too large counts as holding, said so
      (let ((tuatara.defaults::*evidence-holding-search-directories* 100))
        (with-evidence-directories ('((:name "evidence")))
          (wait-for-evidence-links)
          (true (search "more than 100 directories"
                        (tuatara.defaults::directory-holds-evidence-p (concatenate 'string root "src/")))))))))

(define-test (evidence git-words-never-name-evidence)
  ;; any word given to git that leads into evidence is refused, however its option is written
  (with-test-directory (dir)
    (let* ((repo (namestring (ensure-directories-exist (merge-pathnames "repo/" dir))))
           (ev (namestring (ensure-directories-exist (merge-pathnames "ev/" dir))))
           (link (concatenate 'string repo "into-ev")))
      (scratch-repository repo)
      (sb-posix:symlink (string-right-trim "/" ev) link)
      (with-evidence-directories ((list ev))
        (dolist (arguments `(("config" "--fil" ,(concatenate 'string ev "cfg") "a.b" "c")
                             ("config" ,(format nil "-f~acfg" ev) "a.b" "c")
                             ("checkout-index" "-a" ,(format nil "--prefix=~aout/" ev))
                             ("merge-file" ,(concatenate 'string ev "m.txt") "a.txt" "a.txt")
                             ("fast-export" ,(format nil "--export-marks=~amarks" ev) "HEAD")
                             ("pack-objects" ,(concatenate 'string ev "pack"))
                             ("index-pack" "-o" ,(concatenate 'string ev "x.idx") "x.pack")
                             ("bugreport" "-o" ,ev) ("diagnose" "-o" ,ev)
                             ("config" "--file" "into-ev/cfg" "a.b" "c")))
          (true (refused-any (tuatara.git:run-git repo arguments)) "~s" arguments))
        (is equal '() (uiop:directory-files ev))
        (is equal '() (uiop:subdirectories ev))
        ;; the safety options win over the line's own
        (is string= (format nil "false~%")
            (tuatara.text:octets-string (tuatara.git:run-git repo
                                                             '("-c"
                                                               "core.fsmonitor=true"
                                                               "config"
                                                               "--get"
                                                               "core.fsmonitor"))))
        ;; the ! command: no setting on the line, git's own commands only
        (true (tuatara.git::git-command-refusal (list "-c" (format nil "alias.zz=!touch ~ax" ev) "zz")))
        (true (tuatara.git::git-command-refusal '("zz")))
        (false (tuatara.git::git-command-refusal '("log" "--oneline" "-5"))))
      (with-evidence-directories ('())
        (false (tuatara.git::git-command-refusal '("-c" "x.y=z" "zz")))))))

(define-test (evidence directories-written-not-read)
  ;; a directory you may write in but not list (-wx) takes new files, as before, the setting on or off
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (blind (namestring (ensure-directories-exist (merge-pathnames "blind/" dir)))))
      (put-file (concatenate 'string blind "old.txt") "old")
      (sb-posix:chmod blind #o300)
      (unwind-protect
           (dolist (setting (list '() (list (concatenate 'string root "elsewhere/*/evidence/"))))
             (with-evidence-directories (setting)
               (tuatara.io:write-new-file (concatenate 'string blind "new.txt") "new")
               (tuatara.io:write-text-file (tuatara.text:string->rope "saved") (concatenate 'string blind "saved.txt"))
               (tuatara.io:remove-file (concatenate 'string blind "new.txt"))
               (tuatara.io:remove-file (concatenate 'string blind "saved.txt"))
               (is string= "old" (file-text (concatenate 'string blind "old.txt")))))
        (sb-posix:chmod blind #o700)))))

(define-test (evidence saved-apart-said-plainly)
  ;; a file with other names in a directory that may not be written in is refused, saying why
  (with-case-directory (root evidence work)
    (let* ((shut (namestring (ensure-directories-exist (concatenate 'string work "shut/"))))
           (other (concatenate 'string shut "a.txt")))
      (sb-posix:link (concatenate 'string evidence "a.txt") other)
      (sb-posix:chmod shut #o500)
      (unwind-protect
           (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
             (handler-case (progn (tuatara.io:write-text-file (tuatara.text:string->rope "x") other) (false t))
               (tuatara.io:file-system-error (e) (true (search "may not be written in" (princ-to-string e)))))
             (is string= "kept as found" (file-text other)))
        (sb-posix:chmod shut #o700)))))

(define-test (evidence links-learnt-as-written-through)
  ;; an entry that may match anywhere learns the directory a link leads to once the link is written through
  (with-case-directory (root evidence work)
    (let* ((stick (namestring (ensure-directories-exist (merge-pathnames "media/STICK2/" root))))
           (link (concatenate 'string work "evidence")))
      (sb-posix:symlink (string-right-trim "/" stick) link)
      (with-evidence-directories ('((:name "evidence")))
        (wait-for-evidence-links)
        (true (refused (tuatara.io:write-new-file (concatenate 'string link "/n.txt") "x")))
        (true (refused (tuatara.io:write-new-file (concatenate 'string stick "n.txt") "x")))
        (true (find-if (lambda (line) (search "counts as holding an evidence directory" line))
                       (tuatara.defaults::evidence-health-lines))))
      (sb-posix:unlink link))))

(define-test (evidence holding-seen-through-a-second-mount)
  ;; a repository reached through a second mount of the cases holds their evidence: git writes none of it
  (with-case-directory (root evidence work)
    (let* ((info (concatenate 'string root "mountinfo"))
           (other (namestring (ensure-directories-exist (merge-pathnames "other/" root))))
           (repo (namestring (ensure-directories-exist (concatenate 'string other "2026-0001/evidence/"))))
           (repo (subseq repo 0 (- (length repo) (length "evidence/"))))
           (old-info tuatara.defaults::*mounts-info-file*))
      (scratch-repository repo)
      (git-commit-file repo "evidence/a.txt" "as found" "Evidence")
      (put-file (concatenate 'string repo "evidence/a.txt") "changed by another program")
      (put-file info (format nil "1 0 0:900 / / rw - x x rw~%2 1 0:901 / ~a rw - x x rw~%3 1 0:901 / ~a rw - x x rw~%"
                             (string-right-trim "/" (concatenate 'string root "cases")) (string-right-trim "/" other)))
      (setf tuatara.defaults::*mounts-info-file* info)
      (unwind-protect
           (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
             (wait-for-evidence-links)
             (true (tuatara.io:writing-refusal (concatenate 'string repo "evidence/a.txt")))
             (true (tuatara.defaults::directory-holds-evidence-p repo))
             (true (refused-any (tuatara.git:run-git repo '("checkout" "--" "."))))
             (is string= "changed by another program" (file-text (concatenate 'string repo "evidence/a.txt"))))
        (setf tuatara.defaults::*mounts-info-file* old-info)))))

(define-test (evidence answers-dropped-stay-dropped)
  ;; an answer worked out before the kept ones were dropped is not kept after
  (with-case-directory (root evidence work)
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (let* ((rules (tuatara.defaults::evidence-rules))
             (table (tuatara.defaults::evidence-rules-holding rules))
             (since (tuatara.defaults::evidence-rules-forgotten rules)))
        (tuatara.defaults::forget-kept-answers rules)
        (tuatara.defaults::keep-answer rules table "/stale" nil since)
        (is eq :absent (gethash "/stale" table :absent))
        (tuatara.defaults::keep-answer rules table "/fresh" nil (tuatara.defaults::evidence-rules-forgotten rules))
        (is eq nil (gethash "/fresh" table :absent))))))

(define-test (evidence keeper-ends-when-off)
  ;; with the setting emptied nothing keeps running; a keeper that ended is started again; none while saving
  (with-case-directory (root evidence work)
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (tuatara.defaults::evidence-rules)
      (let ((keeper tuatara.defaults::*evidence-keeper*))
        (true (and keeper (sb-thread:thread-alive-p keeper)))
        (sb-thread:terminate-thread keeper)
        (sb-thread:join-thread keeper :default nil :timeout 5)
        (tuatara.defaults::evidence-rules)
        (true (sb-thread:thread-alive-p tuatara.defaults::*evidence-keeper*))))
    (let ((keeper tuatara.defaults::*evidence-keeper*))
      (false (tuatara.defaults::evidence-rules))
      (false tuatara.defaults::*evidence-rules*)
      (sb-thread:join-thread keeper :default nil :timeout 5)
      (false (sb-thread:thread-alive-p keeper)))
    (true (member 'tuatara.defaults::stop-evidence-keeper sb-ext:*save-hooks*))))

(define-test (evidence analysts-read-evidence-through-git)
  ;; a reading command may name files of evidence after --; nothing before it may, and the ! command sets nothing
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (ev (namestring (ensure-directories-exist (merge-pathnames "evidence/" dir)))))
      (scratch-repository dir)
      (git-commit-file dir "evidence/x.txt" "found" "Evidence")
      (with-evidence-directories ((list ev))
        (true (nth-value 1 (tuatara.git:run-git root '("log" "--oneline" "--" "evidence/x.txt"))))
        (true (refused-any (tuatara.git:run-git root '("log" "--output=evidence/y" "--" "x.txt"))))
        (true (refused-any (tuatara.git:run-git root '("config" "--file" "evidence/cfg" "--" "a.b" "c"))))
        (false (probe-file (concatenate 'string ev "y")))
        (dolist (words '(("config" "core.hooksPath" "/tmp/hooks") ("config" "--add" "diff.external" "x")
                         ("--exec-path=/tmp" "log") ("-c" "a.b=c" "log") ("--git-dir=/tmp/x" "log")))
          (true (tuatara.git::git-command-refusal words) "~s" words))
        (dolist (words '(("config" "--get" "user.name") ("config" "--list") ("--no-pager" "log" "-1") ("-P" "status")))
          (false (tuatara.git::git-command-refusal words) "~s" words))))))

(define-test (evidence repositories-after-dashes-refused)
  ;; after -- push, fetch and remote name repositories: one in evidence is refused there; config never sets through the ! command
  (with-test-directory (dir)
    (let* ((repo (namestring (ensure-directories-exist (merge-pathnames "repo/" dir))))
           (ev (namestring (ensure-directories-exist (merge-pathnames "ev/" dir))))
           (seized (namestring (ensure-directories-exist (merge-pathnames "ev/seized/" dir)))))
      (scratch-repository repo)
      (scratch-repository seized)
      (let ((before (git-output seized "for-each-ref")))
        (with-evidence-directories ((list ev))
          (true (refused-any (tuatara.git:run-git repo (list "push" "--" seized "HEAD:refs/heads/pushed"))))
          (true (refused-any (tuatara.git:run-git repo (list "fetch" "--" seized))))
          (true (refused-any (tuatara.git:run-git repo (list "ls-remote" "--" seized))))
          (dolist (words '(("config" "core.hooksPath" "/tmp/h" "--get") ("config" "--get" "core.hooksPath" "x" "y")
                           ("config" "--list" "core.hooksPath") ("config" "--get" "--add" "a.b" "c")
                           ("config" "a.b" "--list") ("config" "set" "a.b" "c")))
            (true (tuatara.git::git-command-refusal words) "~s" words))
          (dolist (words '(("config" "--get" "core.hooksPath") ("config" "--get-regexp" "core" "hooks")
                           ("config" "list") ("config" "get" "user.name") ("config" "-l")))
            (false (tuatara.git::git-command-refusal words) "~s" words)))
        (is string= before (git-output seized "for-each-ref"))))))

(define-test (evidence no-keeper-while-saving)
  ;; once the program is being saved, no question starts the thread keeping the rules again
  (with-case-directory (root evidence work)
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (tuatara.defaults::evidence-rules)
      (unwind-protect
           (progn
             (tuatara.defaults::stop-evidence-keeper)
             (tuatara.io:writing-refusal (concatenate 'string evidence "a.txt"))
             (let ((keeper tuatara.defaults::*evidence-keeper*))
               (false (and keeper (sb-thread:thread-alive-p keeper)))))
        (tuatara.defaults::evidence-saved))
      (tuatara.defaults::evidence-rules)
      (true (sb-thread:thread-alive-p tuatara.defaults::*evidence-keeper*)))
    (true (member 'tuatara.defaults::evidence-saved sb-ext:*init-hooks*))))

(define-test (evidence saved-as-out-of-evidence)
  ;; a file of evidence saved as a file elsewhere: the buffer visits that file, safely still, writable, no longer marked Evidence
  (with-case-directory (root evidence work)
    (with-fresh-recent-files
      (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
        (let ((a (concatenate 'string evidence "a.txt"))
              (copy (concatenate 'string work "a-copy.txt")))
          (multiple-value-bind (f editor)
              (run-keys (format nil "C-x C-f C-a C-k ~a RET C-x C-w C-a C-k ~a RET" (key-string a) (key-string copy))
                        :width 700 :height 12)
            (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
              (is string= copy (tuatara.io:native (tuatara.buffer:buffer-file b)))
              (false (tuatara.defaults::buffer-evidence b))
              ;; still what the evidence held: opened safely, but no longer read-only
              (true (tuatara.io:safe-visit-p b))
              (false (tuatara.buffer:buffer-local b :read-only))
              (false (find-if (lambda (row) (search " Evidence " row)) (last-screen-rows f)))))
          ;; the copy is no evidence: it may be opened as usual
          (true (tuatara.frontend.fake:fake-frame-showing
                 (run-keys "C-x b a - c o p y . t x t RET M-x e n d - s a f e - v i s i t RET" :width 700 :height 12)
                 "a-copy.txt is open as usual now"))
          (is string= "kept as found" (file-text a))
          (is string= "kept as found" (file-text copy)))))))

(define-test (evidence git-refused-in-one-sentence)
  ;; holding evidence never trusts a repository for git: it is looked at only, said at once; a refused write is said in one sentence
  (with-case-directory (root evidence work)
    (scratch-repository root)
    (put-file (concatenate 'string work "notes.txt") "notes")
    (with-evidence-directories (`(,(concatenate 'string root "cases/*/evidence/")))
      (wait-for-evidence-links)
      (false (tuatara.defaults::project-trusted-p root))
      (let* ((f (run-keys "C-x g" :file (concatenate 'string work "notes.txt") :width 400 :height 20))
             (status (tuatara.git::git-buffer-of (tuatara.git:repository-top root) 'tuatara.git::git-status-mode))
             (first-line (and status (let ((text (tuatara.buffer:buffer-string status)))
                                       (subseq text 0 (or (position #\Newline text) (length text)))))))
        ;; the whole line as the status holds it, however wide the window: the frame shows its start
        (true (tuatara.frontend.fake:fake-frame-showing f "Look only: "))
        (true (uiop:string-prefix-p "Look only: " first-line) "~s" first-line)
        (true (search "is an evidence directory (*evidence-directories*: " first-line) "~s" first-line)
        (false (tuatara.frontend.fake:fake-frame-showing-across-rows f "(y, l or n)")))
      (handler-case (progn (tuatara.git:run-git root '("checkout" "--" ".")) (false t))
        (tuatara.io:writing-refused (e)
          (let ((words (princ-to-string e)))
            (true (uiop:string-prefix-p "git checkout is not run: " words) "~a" words)
            (true (search "is an evidence directory (*evidence-directories*: " words))
            (false (search "Could not" words))))))))

(define-test (evidence read-directory-path-in-the-api)
  ;; every question taking :writing is a name packages may use
  (dolist (name '("READ-PATH" "READ-FILE-PATH" "READ-DIRECTORY-PATH" "READ-NATIVE-PATH" "READ-NEW-NATIVE-PATH"))
    (is eq :external (nth-value 1 (find-symbol name :tuatara)) "~a" name)))

(define-test (evidence link-to-evidence-opens-safely)
  ;; a symbolic link outside the evidence directory to a file in it is evidence too
  (with-case-directory (root evidence work)
    (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
      (let ((link (concatenate 'string work "link.txt")))
        (sb-posix:symlink (concatenate 'string evidence "a.txt") link)
        (true (tuatara.defaults::visit-safely-reason link))
        (let ((buffer (tuatara.io:visit-file link)))
          (unwind-protect
               (progn (true (tuatara.io:safe-visit-p buffer))
                      (true (tuatara.buffer:buffer-local buffer :read-only)))
            (tuatara.buffer:kill-buffer buffer)))))))

(defun atime-of (path)
  "The access time of the file at PATH, in seconds."
  (sb-posix:stat-atime (sb-posix:stat path)))

(define-test (evidence links-through-directories-and-parent-names)
  ;; a directory link and .. on the way: the file opened is evidence, and its access time is not changed
  (with-case-directory (root evidence work)
    (let ((a (concatenate 'string evidence "a.txt")))
      (sb-posix:symlink (string-right-trim "/" (concatenate 'string evidence "sub/")) (concatenate 'string work "dl"))
      (sb-posix:symlink "dl/../a.txt" (concatenate 'string work "dotdot.txt"))
      (sb-posix:utimes a 1000 (- (get-universal-time) 100000))
      (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
        (let ((buffer (tuatara.io:visit-file (concatenate 'string work "dotdot.txt"))))
          (unwind-protect
               (progn (true (tuatara.io:safe-visit-p buffer))
                      (is string= "kept as found" (tuatara.buffer:buffer-string buffer))
                      (is = 1000 (atime-of a)))
            (tuatara.buffer:kill-buffer buffer)))))))

(define-test (evidence link-changed-before-it-is-read)
  ;; a buffer of an ordinary file whose link now leads to evidence: read again, it is opened safely
  (with-case-directory (root evidence work)
    (let ((link (concatenate 'string work "link.txt"))
          (plain (put-file (concatenate 'string work "plain.txt") "plain")))
      (sb-posix:symlink (tuatara.io:native plain) link)
      (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
        (let ((buffer (tuatara.io:visit-file link)))
          (unwind-protect
               (progn
                 (false (tuatara.io:safe-visit-p buffer))
                 (sb-posix:unlink link)
                 (sb-posix:symlink (concatenate 'string evidence "a.txt") link)
                 (tuatara.io:revert-buffer-file buffer)
                 (true (tuatara.io:safe-visit-p buffer))
                 (true (tuatara.buffer:buffer-local buffer :read-only))
                 (is string= "kept as found" (tuatara.buffer:buffer-string buffer)))
            (tuatara.buffer:kill-buffer buffer)))))))

(define-test (evidence links-looked-at-only-when-wanted)
  ;; with no evidence directory and no safe directory a link is not followed to decide
  (with-test-directory (dir)
    (let ((file (put-file (merge-pathnames "p.txt" dir) "x"))
          (calls 0)
          (old tuatara-user:*safe-directories*)
          (functions '(tuatara.io:real-path-so-far tuatara.io:real-path tuatara.io:resolve-links)))
      (setf tuatara-user:*safe-directories* '())
      (dolist (function functions)
        (sb-int:encapsulate function 'counting (lambda (f &rest arguments) (incf calls) (apply f arguments))))
      (unwind-protect
           (with-evidence-directories ('())
             (false (tuatara.defaults::visit-safely-reason (tuatara.io:native file)))
             (is = 0 calls))
        (dolist (function functions) (sb-int:unencapsulate function 'counting))
        (setf tuatara-user:*safe-directories* old)))))

(define-test (evidence hard-link-to-evidence)
  ;; another name outside the directory of an evidence file is evidence: opened safely, access time kept;
  ;; an ordinary file with two names is opened as usual once the evidence directories are gone through
  (with-case-directory (root evidence work)
    (let* ((a (concatenate 'string evidence "a.txt"))
           (hard (concatenate 'string work "hard.txt"))
           (plain (put-file (concatenate 'string work "plain.txt") "plain"))
           (twin (concatenate 'string work "twin.txt")))
      (sb-posix:link a hard)
      (sb-posix:link (tuatara.io:native plain) twin)
      (sb-posix:utimes a 1000 (- (get-universal-time) 100000))
      (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
        (dolist (waited '(nil t))
          (when waited
            (loop repeat 1000
                  until (let ((state (cdr tuatara.defaults::*evidence-inodes*))) (and (consp state) (car state)))
                  do (sleep 1/100)))
          (let ((buffer (tuatara.io:visit-file hard)))
            (unwind-protect
                 (progn (true (tuatara.io:safe-visit-p buffer) "waited ~s" waited)
                        (is = 1000 (atime-of a)))
              (tuatara.buffer:kill-buffer buffer))))
        (let ((buffer (tuatara.io:visit-file twin)))
          (unwind-protect (false (tuatara.io:safe-visit-p buffer))
            (tuatara.buffer:kill-buffer buffer)))))))

(define-test (evidence failed-check-opens-safely)
  ;; when the check whether a file opens safely cannot be made, the file opens safely
  (with-test-directory (dir)
    (let ((file (put-file (merge-pathnames "p.txt" dir) "x")))
      (sb-int:encapsulate 'tuatara.defaults::visit-safely-reason 'failing
                          (lambda (f &rest arguments) (declare (ignore f arguments)) (error "no way to tell")))
      (unwind-protect
           (let ((buffer (tuatara.io:visit-file (tuatara.io:native file))))
             (unwind-protect
                  (progn (true (tuatara.io:safe-visit-p buffer))
                         (true (search "no way to tell" (tuatara.io:safe-visit-reason buffer))))
               (tuatara.buffer:kill-buffer buffer)))
        (sb-int:unencapsulate 'tuatara.defaults::visit-safely-reason 'failing)))))

(define-test (evidence untrue-copy-named-whatever-its-name)
  ;; a copy whose sum differs is refused naming it, a ~ in its name written as it is
  (with-test-directory (dir)
    (let* ((a (put-file (merge-pathnames "a.txt" dir) "kept as found"))
           (copy (concatenate 'string (tuatara.io:native dir) "copy ~a ~b.txt"))
           (words (handler-case (progn (tuatara.defaults::copy-file-checked (tuatara.io:native a) copy :sum "0000") nil)
                    (error (e) (princ-to-string e)))))
      (true (typep (nth-value 1 (ignore-errors (tuatara.defaults::copy-file-checked (tuatara.io:native a) copy
                                                                                     :sum "0000")))
                   'tuatara.runtime:refusal))
      (is string= (format nil "the copy at ~a does not have its original's SHA-256: taken away" copy) words)
      (false (probe-file copy) "the untrue copy is taken away"))))

(define-test (evidence link-to-evidence-is-evidence)
  ;; a link to a file of evidence, or to an evidence directory, is evidence wherever it is asked;
  ;; another name made by a hard link may be, and stays opened safely
  (with-case-directory (root evidence work)
    (let ((link (concatenate 'string work "link.txt"))
          (directory-link (concatenate 'string work "dl"))
          (hard (concatenate 'string work "hard.txt")))
      (sb-posix:symlink (concatenate 'string evidence "a.txt") link)
      (sb-posix:symlink (string-right-trim "/" (concatenate 'string evidence "sub/")) directory-link)
      (sb-posix:link (concatenate 'string evidence "a.txt") hard)
      (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
        ;; judged by its own place where it is taken away, by what it leads to where that is read
        (false (evidence-directory-of link))
        (is string= evidence (values (tuatara.defaults::evidence-path-p link :follow t)))
        (is string= evidence (values (tuatara.defaults::evidence-path-p directory-link :follow t)))
        (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET M-x d e s c r i b e - e v i d e n c e RET"
                                   (key-string link))
                           :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f " Evidence "))
          (true (tuatara.frontend.fake:fake-frame-showing f "Yes: ")))
        (kill-buffers-visiting root)
        (true (tuatara.frontend.fake:fake-frame-showing
               (run-keys (format nil "C-x C-f C-a C-k ~a RET M-x e n d - s a f e - v i s i t RET" (key-string link))
                         :width 700 :height 12)
               "link.txt stays opened safely: it is in the evidence directory"))
        (kill-buffers-visiting root)
        (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET M-x d e s c r i b e - e v i d e n c e RET ~
                                        M-x e n d - s a f e - v i s i t RET"
                                   (key-string hard))
                           :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "Maybe: "))
          (true (tuatara.frontend.fake:fake-frame-showing f "hard.txt stays opened safely: it has more than one name")))
        (kill-buffers-visiting root)))))

(defun retarget-link (link target)
  "Have the symbolic link LINK lead to TARGET from now on."
  (sb-posix:unlink link)
  (sb-posix:symlink target link))

(define-test (evidence safe-visit-ended-for-the-file-read)
  ;; end-safe-visit judges the file the buffer read, not where its link leads by then
  (with-case-directory (root evidence work)
    (let ((link (concatenate 'string work "l.txt"))
          (other (concatenate 'string work "r.txt"))
          (plain (tuatara.io:native (put-file (concatenate 'string work "other.txt") "elsewhere"))))
      (sb-posix:symlink (concatenate 'string evidence "a.txt") link)
      (sb-posix:symlink plain other)
      (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
        ;; the refusal is looked for in *Messages*: the watcher's word on the changed link may paint over it
        (flet ((refused (file why)
                 (let ((b (find (pathname file)
                                tuatara.buffer:*buffers*
                                :key #'tuatara.buffer:buffer-file
                                :test #'equal)))
                   (true b)
                   (when b
                     (true (search (format nil "~a stays opened safely: ~a" (tuatara.buffer:buffer-name b) why)
                                   (messages-text)))
                     (true (tuatara.io:safe-visit-p b))))))
          ;; opened as evidence, its link then led out and the answers kept forgotten
          (run-keys (list (format nil "C-x C-f C-a C-k ~a RET" (key-string link))
                          (lambda ()
                            (retarget-link link plain)
                            (tuatara.defaults::next-evidence-generation))
                          "M-x e n d - s a f e - v i s i t RET")
                    :width 700 :height 12)
          (refused link "it is in the evidence directory")
          ;; opened safely as an ordinary file, its link then led into evidence
          (run-keys (list (format nil "C-u C-x C-f C-a C-k ~a RET" (key-string other))
                          (lambda () (retarget-link other (concatenate 'string evidence "a.txt")))
                          "M-x e n d - s a f e - v i s i t RET")
                    :width 700 :height 12)
          (refused other "its name no longer leads to the file it read"))
        (kill-buffers-visiting root)))))

(define-test (evidence copied-out-by-the-file-opened)
  ;; a link led out of evidence between the check and the copy: nothing copied, nothing logged as evidence
  (with-case-directory (root evidence work)
    (let ((link (concatenate 'string work "l.txt"))
          (plain (tuatara.io:native (put-file (concatenate 'string work "other.txt") "elsewhere")))
          (out (namestring (ensure-directories-exist (concatenate 'string work "out/"))))
          (logged nil))
      (sb-posix:symlink (concatenate 'string evidence "a.txt") link)
      (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
        (sb-int:encapsulate 'tuatara.defaults::copy-file-checked 'retarget
                            (lambda (f &rest arguments) (retarget-link link plain) (apply f arguments)))
        (unwind-protect
             (handler-case (tuatara.defaults::copy-from-evidence-work
                            (list (cons link (concatenate 'string out "l.txt"))) out (list nil)
                            (lambda (log) (setf logged log)))
               (error () nil))
          (sb-int:unencapsulate 'tuatara.defaults::copy-file-checked 'retarget))
        (false (probe-file (concatenate 'string out "l.txt")) "a file outside evidence is not copied as from it")
        (false logged)))))

(defun link-access-time (path)
  "The access time of the link at PATH itself, in seconds."
  (sb-posix:stat-atime (sb-posix:lstat path)))

(defun put-link-access-time-back (path)
  "Set the access time of the link at PATH itself to the start of 2000, before
its change time, so that reading it moves the time even where the file system
moves it only now and then (relatime)."
  (uiop:run-program (list "touch" "-h" "-a" "-d" "@946684800" path)))

(define-test (evidence no-link-in-evidence-read)
  ;; telling whether a path is evidence never reads a link in an evidence directory: that changes its access time
  (with-case-directory (root evidence work)
    (let ((inner (concatenate 'string evidence "inner"))
          (directory-link (concatenate 'string evidence "subl"))
          (link (concatenate 'string work "l.txt"))
          (control (concatenate 'string work "control")))
      (sb-posix:symlink (concatenate 'string evidence "a.txt") inner)
      (sb-posix:symlink (concatenate 'string evidence "sub") directory-link)
      (sb-posix:symlink inner link)
      (sb-posix:symlink link control)
      (with-evidence-directories ((list (concatenate 'string root "cases/*/evidence/")))
        (wait-for-evidence-links)
        (put-link-access-time-back control)
        (sb-posix:readlink control)
        (if (= 946684800 (link-access-time control))
            (skip "this file system does not move a link's access time when it is read" (true nil))
            (progn
              (dolist (path (list inner directory-link)) (put-link-access-time-back path))
              (is string= evidence (values (tuatara.defaults::evidence-path-p link :follow t)))
              (is string= evidence
                  (values (tuatara.defaults::evidence-path-p (concatenate 'string directory-link "/c.txt"))))
              (true (tuatara.defaults::evidence-writing-refusal (concatenate 'string directory-link "/new.txt")))
              (is = 946684800 (link-access-time inner))
              (is = 946684800 (link-access-time directory-link))))))))
