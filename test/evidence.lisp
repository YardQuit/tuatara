;;;; test/evidence.lisp --- Evidence folders: opened safely, and nothing written in them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test evidence
  :description "Evidence folders: the entries of *evidence-folders* matched by globs, names and patterns, links and mounts followed; every write there refused in one wording; files there opened safely and marked in the mode line; copies out checked and logged; nothing looked at while the setting is empty.")

(defmacro with-evidence-folders ((value) &body body)
  "Run BODY with *EVIDENCE-FOLDERS* set to VALUE for every thread, set back after."
  (let ((old (gensym)))
    `(let ((,old tuatara-user:*evidence-folders*))
       (unwind-protect (progn (setf tuatara-user:*evidence-folders* ,value) ,@body)
         (setf tuatara-user:*evidence-folders* ,old)))))

(defun wait-for-evidence-links ()
  "Wait, ten seconds at most, until the links the entries of *EVIDENCE-FOLDERS*
match have been looked for in the background."
  (loop repeat 1000
        until (tuatara.defaults::evidence-links-learnt-p)
        do (sleep 1/100)))

(defun wait-for-evidence-generation (after)
  "Wait, ten seconds at most, until *EVIDENCE-FOLDERS* has been read anew since
generation AFTER."
  (loop repeat 1000
        until (> (or (tuatara.defaults::evidence-generation) 0) after)
        do (sleep 1/100)))

(defmacro with-case-folder ((root evidence work) &body body)
  "Run BODY in a test folder ROOT holding cases/2026-0001/evidence/ (EVIDENCE,
with a.txt and b.pdf in it, and a folder sub/ holding c.txt) and
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
         ;; the evidence made writable again, so that the test folder can go
         (setf tuatara-user:*evidence-folders* nil)))))

(defun evidence-folder-of (path)
  "The evidence folder PATH is in, or NIL (EVIDENCE-PATH-P)."
  (values (tuatara.defaults::evidence-path-p path)))

(define-test (evidence matched-by-globs-names-and-patterns)
  (with-case-folder (root evidence work)
    (let ((file (concatenate 'string evidence "a.txt"))
          (deeper (concatenate 'string evidence "sub/c.txt")))
      ;; empty: nothing is evidence, and nothing is looked at
      (with-evidence-folders ('())
        (false (evidence-folder-of file))
        (false (tuatara.io:writing-refused-anywhere-p)))
      (dolist (setting (list (list (concatenate 'string root "cases/*/evidence/"))
                             (list (concatenate 'string root "**/evidence"))
                             (concatenate 'string root "cases/*/evidence/") ; one entry alone
                             (list (list :name "evidence" :within (concatenate 'string root "cases/")))
                             (list (list :name "evidence"))
                             (list (list :regex "^[0-9]{4}-[0-9]{4}$" :within (concatenate 'string root "cases/") :then "evidence"))
                             ;; names compared without regard to case
                             (list (concatenate 'string root "CASES/*/Evidence/"))
                             (list (list :name "EVIDENCE"))))
        (with-evidence-folders (setting)
          (is string= evidence (evidence-folder-of file) "~s" setting)
          (is string= evidence (evidence-folder-of deeper) "~s" setting)
          (is string= evidence (evidence-folder-of evidence) "the folder itself: ~s" setting)
          (false (evidence-folder-of (concatenate 'string work "notes.org")) "~s" setting)
          (false (evidence-folder-of (concatenate 'string root "cases/2026-0001/")) "~s" setting)
          (true (tuatara.io:writing-refused-anywhere-p))))
      ;; the entry that matched is told as written
      (let ((entry (concatenate 'string root "cases/*/evidence/")))
        (with-evidence-folders (`("/nowhere/*/evidence/" ,entry))
          (is equal entry (nth-value 1 (tuatara.defaults::evidence-path-p file)))))
      ;; a pattern that matches only part of a name, or a folder outside :within, is no evidence
      (with-evidence-folders (`((:regex "^[0-9]{4}$" :within ,(concatenate 'string root "cases/") :then "evidence")
                                (:name "evidence" :within "/nowhere/")))
        (false (evidence-folder-of file))))))

(define-test (evidence entries-read-or-said)
  ;; an entry that cannot be read is left out and said; the others still count
  (with-case-folder (root evidence work)
    (with-evidence-folders (`("relative/evidence/" (:regex "([" :within "/") (:name "a/b") (:then "x") 42
                             ,(concatenate 'string root "cases/*/evidence/")))
      (let ((problems (tuatara.defaults::evidence-problems-words)))
        (is = 5 (length problems))
        (true (every (lambda (words) (search "is left out" words)) problems))
        (true (find-if (lambda (words) (search ":regex \"([\" cannot be used" words)) problems)))
      (is string= evidence (evidence-folder-of (concatenate 'string evidence "a.txt")))
      (true (find-if (lambda (line) (search "not read, left out" line)) (tuatara.defaults::evidence-health-lines))))
    ;; check-health: none (off), or each entry with what it matches
    (with-evidence-folders ('())
      (is equal '("Evidence folders: none (off)") (tuatara.defaults::evidence-health-lines)))
    (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/") ,(concatenate 'string root "nothing/*/here/")))
      (let ((lines (tuatara.defaults::evidence-health-lines)))
        (true (search "2 entries" (first lines)))
        (true (search "1 folder, " (second lines)))
        (true (search "matches nothing" (third lines)))))))

(define-test (evidence links-and-mounts-followed)
  (with-case-folder (root evidence work)
    (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
      (let ((link (concatenate 'string work "link-to-evidence"))
            (file-link (concatenate 'string work "a-link.txt")))
        (sb-posix:symlink (string-right-trim "/" evidence) link)
        (sb-posix:symlink (concatenate 'string evidence "a.txt") file-link)
        ;; through a link to the folder, and through .. on the way
        (is string= evidence (evidence-folder-of (concatenate 'string link "/a.txt")))
        (is string= evidence (evidence-folder-of (concatenate 'string work "../evidence/a.txt")))
        ;; a link elsewhere leading to a file of evidence is the link's own place: taking it away is no write there
        (false (evidence-folder-of file-link))
        (false (tuatara.io:writing-refusal file-link))
        ;; writing through it is: the save goes to where it leads
        (fail (tuatara.io:write-text-file (tuatara.text:string->rope "changed") file-link) 'tuatara.io:writing-refused)
        (is string= "kept as found" (file-text (concatenate 'string evidence "a.txt")))
        ;; a folder opened is judged by where it really is, whatever path led to it
        (let ((fd (sb-posix:open link (logior sb-posix:o-rdonly sb-posix:o-directory))))
          (unwind-protect (true (tuatara.io:writing-refusal (concatenate 'string work "elsewhere.txt") :dirfd fd))
            (sb-posix:close fd))))
      ;; the same folder mounted at a second place is evidence there too
      (let ((info (concatenate 'string root "mountinfo"))
            (other (namestring (ensure-directories-exist (merge-pathnames "mounted-again/" root)))))
        (put-file info (format nil "1 0 0:900 / / rw - x x rw~%2 1 0:901 / ~a rw - x x rw~%3 1 0:901 / ~a rw - x x rw~%"
                               (string-right-trim "/" (concatenate 'string root "cases")) (string-right-trim "/" other)))
        (let ((tuatara.defaults::*mounts-info-file* info))
          (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
            (is string= evidence (evidence-folder-of (concatenate 'string other "2026-0001/evidence/a.txt")))
            (false (evidence-folder-of (concatenate 'string other "2026-0001/work/a.txt")))))))))

(defmacro refused (form)
  "True when FORM signals WRITING-REFUSED saying the evidence rule."
  `(handler-case (progn ,form nil)
     (tuatara.io:writing-refused (e) (and (search "is an evidence folder (*evidence-folders*: " (princ-to-string e)) t))))

(defmacro refused-any (form)
  "True when FORM signals WRITING-REFUSED."
  `(handler-case (progn ,form nil) (tuatara.io:writing-refused () t)))

(define-test (evidence every-write-refused)
  (with-case-folder (root evidence work)
    (let* ((a (concatenate 'string evidence "a.txt"))
           (outside (put-file (concatenate 'string work "out.txt") "outside"))
           (outside (namestring outside))
           (before (directory-listing-of root)))
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (true (refused (tuatara.io:write-text-file (tuatara.text:string->rope "x") a)))
        (true (refused (tuatara.io:write-text-file (tuatara.text:string->rope "x") (concatenate 'string evidence "new.txt"))))
        (true (refused (tuatara.io:replace-file a "x")))
        (true (refused (tuatara.io:write-new-file (concatenate 'string evidence "new.txt") "x")))
        (true (refused (tuatara.io:write-private-file (concatenate 'string evidence "new.txt") "x")))
        (true (refused (tuatara.io:copy-regular-file outside (concatenate 'string evidence "in.txt"))))
        (true (refused (tuatara.io:copy-path-tree outside (concatenate 'string evidence "in.txt"))))
        (true (refused (tuatara.io:copy-to-free-name outside evidence "in.txt")))
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
        ;; the folder itself, and a folder holding it, are not deleted, moved or renamed
        (true (refused (tuatara.io:delete-tree evidence)))
        (true (refused (tuatara.io:delete-tree (concatenate 'string root "cases/"))))
        (true (refused (tuatara.io:rename-path (concatenate 'string root "cases/2026-0001") (concatenate 'string root "moved"))))
        (true (refused (tuatara.io:trash-path (concatenate 'string root "cases/"))))
        ;; nor a program let write in it
        (true (refused (tuatara.io::mount-arguments '() (list (string-right-trim "/" (concatenate 'string root "cases"))) '())))
        ;; the editor's own folders are not put there, and a state file's lock is not made there
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
  "Every file and folder under ROOT with its contents, sorted, as strings."
  (let ((found '()))
    (uiop:collect-sub*directories (pathname root) t t
                                  (lambda (dir)
                                    (push (namestring dir) found)
                                    (dolist (file (uiop:directory-files dir))
                                      (push (format nil "~a=~a" (namestring file) (ignore-errors (file-text file))) found))))
    (sort found #'string<)))

(define-test (evidence opened-safely-and-marked)
  (with-case-folder (root evidence work)
    (with-fresh-recent-files
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (let ((a (concatenate 'string evidence "a.txt")))
          ;; C-x C-f opens it safely by itself, and says why
          (multiple-value-bind (f editor) (run-keys (format nil "C-x C-f C-a C-k ~a RET" (key-string a)) :width 700 :height 12)
            (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
              (true (tuatara.io:safe-visit-p b))
              (true (tuatara.buffer:buffer-local b :read-only))
              (true (tuatara.frontend.fake:fake-frame-showing f "a.txt opened safely: it is in the evidence folder"))
              (true (tuatara.frontend.fake:fake-frame-showing f "it stays so, being evidence"))
              ;; Evidence in its own face, before the name
              (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f " Evidence ")
                (let* ((row (position-if (lambda (r) (search " Evidence " r)) screen))
                       (at (search " Evidence " (aref screen row))))
                  (true (find-if (lambda (span) (and (= (first span) at) (member :mode-line-evidence (alexandria:ensure-list (third span)))))
                                 (aref spans row)))
                  (true (< at (search "a.txt" (aref screen row))))))))
          (kill-buffers-visiting root)
          ;; it stays opened safely
          (true (tuatara.frontend.fake:fake-frame-showing
                 (run-keys (format nil "C-x C-f C-a C-k ~a RET M-x e n d - s a f e - v i s i t RET" (key-string a)) :width 700 :height 12)
                 "a.txt stays opened safely: it is in the evidence folder"))
          (kill-buffers-visiting root)
          ;; M-x evidence-folder-p says yes, and by which entry; no for a file beside it
          (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET M-x e v i d e n c e - f o l d e r - p RET" (key-string a)) :width 700 :height 12)))
            (true (tuatara.frontend.fake:fake-frame-showing f "Yes: "))
            (true (tuatara.frontend.fake:fake-frame-showing f "cases/*/evidence/\")")))
          (kill-buffers-visiting root)
          (true (tuatara.frontend.fake:fake-frame-showing
                 (run-keys "M-x e v i d e n c e - f o l d e r - p RET" :file (namestring (put-file (concatenate 'string work "w.txt") "w"))
                                                                         :width 700 :height 12)
                 "is in no evidence folder"))
          (kill-buffers-visiting root)
          ;; C-x C-w of a file elsewhere into the evidence folder is refused, said in the one wording
          (let ((f (run-keys (format nil "C-x C-w C-a C-k ~a RET" (key-string (concatenate 'string evidence "w.txt")))
                             :file (concatenate 'string work "w.txt") :width 700 :height 12)))
            (true (tuatara.frontend.fake:fake-frame-showing f "is an evidence folder (*evidence-folders*: "))
            (false (probe-file (concatenate 'string evidence "w.txt")))))))))

(define-test (evidence setting-changed-after-opening)
  ;; a file opened before its folder was named evidence gets the rule once it is
  (with-case-folder (root evidence work)
    (let ((a (concatenate 'string evidence "a.txt")))
      (multiple-value-bind (f editor)
          (run-keys (list "i x C-g" :wait
                          (lambda () (setf tuatara-user:*evidence-folders* (list (concatenate 'string root "cases/*/evidence/"))))
                          :wait "C-x C-s")
                    :file a :width 700 :height 12)
        (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (true (tuatara.buffer:buffer-local b :read-only))
          (true (tuatara.frontend.fake:fake-frame-showing f "is in the evidence folder"))
          (true (tuatara.frontend.fake:fake-frame-showing f " Evidence "))))
      (is string= "kept as found" (file-text a)))))

(define-test (evidence copied-out-checked-and-logged)
  (with-case-folder (root evidence work)
    (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
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
          (true (search "Evidence folder: " text))
          (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp logged)))))
        ;; a copy whose sum differs is an error naming it
        (fail (tuatara.defaults::copy-file-checked a (concatenate 'string work "again.txt") :sum "0000") 'error)
        (false (probe-file (concatenate 'string work "again.txt")) "an untrue copy is taken away")
        ;; never into an evidence folder
        (true (tuatara.defaults::evidence-refusal "copy into" evidence))
        (true (refused (tuatara.defaults::copy-file-checked a (concatenate 'string evidence "sub/a.txt"))))
        ;; by the command: a folder asked for in the minibuffer
        (let ((f (run-keys (list (format nil "C-x C-f C-a C-k ~a RET M-x c o p y - f r o m - e v i d e n c e RET C-a C-k ~a RET"
                                         (key-string b) (key-string (namestring (ensure-directories-exist (concatenate 'string work "out/")))))
                                 :wait)
                           :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "each copy's SHA-256 checked against its original"))
          (is string= "%PDF-1.4 not really" (file-text (concatenate 'string work "out/b.pdf"))))
        (kill-buffers-visiting root)
        ;; into evidence by the command: refused, said
        (let ((f (run-keys (format nil "C-x C-f C-a C-k ~a RET M-x c o p y - f r o m - e v i d e n c e RET C-a C-k ~a RET"
                                   (key-string b) (key-string (concatenate 'string evidence "sub/")))
                           :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "is an evidence folder (*evidence-folders*: "))
          (false (probe-file (concatenate 'string evidence "sub/b.pdf"))))))))

(define-test (evidence rimu-and-export-refused)
  (with-case-folder (root evidence work)
    (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
      ;; rimu says so before anything is begun
      (false (tuatara.defaults::rimu-start-operation :delete (list (list (concatenate 'string evidence "a.txt"))) nil))
      (false (tuatara.defaults::rimu-start-operation :copy (list (cons (concatenate 'string work "../evidence/a.txt")
                                                                       (concatenate 'string evidence "a2.txt")))
                                                     nil))
      (true (probe-file (concatenate 'string evidence "a.txt")))
      (kill-buffers-visiting root)
      ;; an Org note elsewhere exporting into it is refused before anything is made
      (let ((note (namestring (put-file (concatenate 'string work "note.org")
                                        (format nil "#+export_file_name: ../evidence/note~%* Heading~%text~%")))))
        (let ((f (run-keys "M-x o r g - e x p o r t - l a t e x RET" :file note :width 700 :height 12)))
          (true (tuatara.frontend.fake:fake-frame-showing f "is an evidence folder (*evidence-folders*: ")))
        (false (probe-file (concatenate 'string evidence "note.tex")))))))

(define-test (evidence empty-setting-costs-nothing)
  ;; with no folder named, nothing is read from the disk and nothing is made for an answer
  (with-evidence-folders ('())
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
  ;; git's commands that write working files are refused in a repository holding an evidence folder; reading ones run
  (true (tuatara.defaults::git-writes-work-tree-p '("checkout" "--" "a.txt")))
  (true (tuatara.defaults::git-writes-work-tree-p '("-c" "x=y" "reset" "--hard")))
  (true (tuatara.defaults::git-writes-work-tree-p '("stash" "push")))
  (true (tuatara.defaults::git-writes-work-tree-p '("apply" "--index" "-")))
  (true (tuatara.defaults::git-writes-work-tree-p '("restore" "--staged" "--worktree" "a.txt")))
  (true (tuatara.defaults::git-writes-work-tree-p '("a-command-not-known-yet")))
  ;; commit and add run the repository's clean filters on its working files
  (true (tuatara.defaults::git-writes-work-tree-p '("commit" "-m" "x")))
  (dolist (reading '(("status" "--porcelain") ("log" "-1") ("diff") ("blame" "a.txt") ("apply" "--cached" "-")
                     ("reset" "-q" "--" "a.txt") ("restore" "--staged" "a.txt") ("rm" "--cached" "a.txt")
                     ("stash" "list") ("format-patch" "--stdout" "-1")))
    (false (tuatara.defaults::git-writes-work-tree-p reading) "~s" reading))
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (evidence (namestring (ensure-directories-exist (merge-pathnames "evidence/" dir)))))
      (scratch-repository dir)
      (git-commit-file dir "evidence/kept.txt" "as found" "Evidence")
      (put-file (merge-pathnames "evidence/kept.txt" dir) "changed meanwhile by another program")
      (with-evidence-folders (`(,evidence))
        (dolist (writing '(("checkout" "--" "evidence/kept.txt") ("reset" "--hard") ("stash") ("clean" "-fdx")
                           ("restore" "evidence/kept.txt")))
          (handler-case (progn (tuatara.defaults::run-git root writing) (false t "~s ran" writing))
            (tuatara.io:writing-refused (e)
              (true (search "is an evidence folder (*evidence-folders*: " (princ-to-string e)) "~s" writing)
              (true (search (format nil "Could not run git ~a in " (first writing)) (princ-to-string e))))))
        (is string= "changed meanwhile by another program" (file-text (merge-pathnames "evidence/kept.txt" dir)))
        ;; reading runs
        (multiple-value-bind (octets ok) (tuatara.defaults::run-git root '("status" "--porcelain"))
          (true ok)
          (true (search "evidence/kept.txt" (tuatara.text:octets-string octets)))))
      ;; with the setting empty, as before
      (with-evidence-folders ('())
        (true (nth-value 1 (tuatara.defaults::run-git root '("checkout" "--" "evidence/kept.txt"))))
        (is string= "as found" (file-text (merge-pathnames "evidence/kept.txt" dir)))))))

(define-test (evidence never-offered-to-write-in)
  ;; a question whose answer is written to never starts in an evidence folder nor offers one first; reading ones still may
  (with-case-folder (root evidence work)
    (with-fresh-recent-files
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (let ((a (concatenate 'string evidence "a.txt")))
          ;; the folder chosen last is evidence: offered for reading, not for writing
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
  ;; an evidence folder that is a link to a drive's folder protects that folder, however the setting names it
  (with-case-folder (root evidence work)
    (let* ((stick (namestring (ensure-directories-exist (merge-pathnames "media/STICK/" root))))
           (link (concatenate 'string root "cases/2026-0002/evidence")))
      (ensure-directories-exist (concatenate 'string root "cases/2026-0002/"))
      (put-file (concatenate 'string stick "found.txt") "on the stick")
      (sb-posix:symlink (string-right-trim "/" stick) link)
      (dolist (setting (list (list link)
                             (list (concatenate 'string root "cases/*/evidence/"))
                             (list (list :name "evidence" :within (concatenate 'string root "cases/")))
                             (list (list :regex "^[0-9]{4}-[0-9]{4}$" :within (concatenate 'string root "cases/") :then "evidence"))))
        (with-evidence-folders (setting)
          (wait-for-evidence-links)
          (true (refused (tuatara.io:write-new-file (concatenate 'string stick "new.txt") "x")) "~s" setting)
          (true (refused (tuatara.io:write-new-file (concatenate 'string link "/new.txt") "x")) "~s" setting)
          (true (evidence-folder-of (concatenate 'string stick "found.txt")) "~s" setting)
          (false (probe-file (concatenate 'string stick "new.txt")) "~s" setting)))
      (sb-posix:unlink link))))

(define-test (evidence whole-names-composed-alike)
  ;; a pattern matches a folder's whole name, its alternatives in any order; names compared in one Unicode form
  (with-case-folder (root evidence work)
    (let ((file (concatenate 'string evidence "a.txt")))
      (with-evidence-folders (`((:regex "2026|2026-0001" :within ,(concatenate 'string root "cases/") :then "evidence")))
        (is string= evidence (evidence-folder-of file)))
      (with-evidence-folders (`((:regex "2026" :within ,(concatenate 'string root "cases/") :then "evidence")))
        (false (evidence-folder-of file)))
      (let ((composed (namestring (ensure-directories-exist (merge-pathnames (format nil "~cvidence/" (code-char #xe9)) root)))))
        (with-evidence-folders (`((:name ,(format nil "e~cvidence" (code-char #x301)) :within ,root)))
          (true (evidence-folder-of (concatenate 'string composed "x.txt"))))))))

(define-test (evidence refused-by-the-folder-held-open)
  ;; each write asks again by the folder it holds open, so that a folder swapped for a link meanwhile leads nowhere
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (kept (namestring (put-file (merge-pathnames "kept.txt" dir) "kept")))
           (tuatara.io:*writing-refusal-function*
             (lambda (path &key dirfd fd)
               (cond ((null path) t)
                     ((or dirfd fd) "refused by the folder held open")))))
      (true (handler-case (progn (tuatara.io:write-new-file (concatenate 'string root "n.txt") "x") nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:replace-file kept "x") nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:remove-file kept) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:touch-path kept) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:make-directory (concatenate 'string root "d/")) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:make-symbolic-link kept (concatenate 'string root "l")) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:rename-path kept (concatenate 'string root "r.txt")) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:create-free-file root (lambda (n) (format nil "f~d" n))) nil) (tuatara.io:writing-refused () t)))
      (true (handler-case (progn (tuatara.io:open-scratch-file root) nil) (tuatara.io:writing-refused () t)))
      (false (tuatara.io:call-with-state-lock (concatenate 'string root "state") (lambda () t)))
      (is string= "kept" (file-text kept))
      (is equal '("kept.txt") (mapcar #'file-namestring (uiop:directory-files dir))))
    ;; a relative path is judged from the process's own folder, as the system takes it
    (let ((here (sb-posix:getcwd)))
      (unwind-protect
           (progn (sb-posix:chdir (namestring dir))
                  (with-evidence-folders ((list (namestring dir)))
                    (true (tuatara.io:writing-refusal "relative.txt"))))
        (sb-posix:chdir here)))))

(define-test (evidence other-names-never-written-in-place)
  ;; a file with another name in evidence, made before, is saved as a file of its own, never through the shared file
  (with-case-folder (root evidence work)
    (let ((a (concatenate 'string evidence "a.txt"))
          (other (concatenate 'string work "a-hard.txt"))
          (third (concatenate 'string work "a-third.txt")))
      (sb-posix:link a other)
      (sb-posix:link a third)
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (tuatara.io:write-text-file (tuatara.text:string->rope "changed through another name") other)
        (is string= "kept as found" (file-text a))
        (is string= "changed through another name" (file-text other))
        (is = 2 (tuatara.io:stamp-nlink (tuatara.io:path-stamp a)))
        (true (refused-any (tuatara.io:touch-path third)))
        (is string= "kept as found" (file-text third))))))

(define-test (evidence mounts-changed-looked-at-again)
  ;; what is kept of each folder is dropped once the drives mounted change
  (with-case-folder (root evidence work)
    (let* ((info (concatenate 'string root "mountinfo"))
           (other (namestring (ensure-directories-exist (merge-pathnames "mounted-again/" root))))
           (file (concatenate 'string (namestring (ensure-directories-exist (concatenate 'string other "2026-0001/evidence/"))) "a.txt"))
           (old-info tuatara.defaults::*mounts-info-file*)
           (old-interval tuatara.defaults::*mounts-look-interval*))
      (put-file info (format nil "1 0 0:900 / / rw - x x rw~%"))
      ;; for every thread: the drives are looked at in the background
      (setf tuatara.defaults::*mounts-info-file* info tuatara.defaults::*mounts-look-interval* 0)
      (unwind-protect
           (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
             (wait-for-evidence-links)
             (false (evidence-folder-of file))
             (let ((before (tuatara.defaults::evidence-generation)))
               (put-file info (format nil "1 0 0:900 / / rw - x x rw~%2 1 0:901 / ~a rw - x x rw~%3 1 0:901 / ~a rw - x x rw~%"
                                      (string-right-trim "/" (concatenate 'string root "cases")) (string-right-trim "/" other)))
               (wait-for-evidence-generation before)
               (wait-for-evidence-links))
             (is string= evidence (evidence-folder-of file)))
        (setf tuatara.defaults::*mounts-info-file* old-info tuatara.defaults::*mounts-look-interval* old-interval)))))

(define-test (evidence git-writes-nowhere-in-evidence)
  ;; git's output files, other folders and repositories named, are refused in evidence; hooks off where a repository holds it
  (with-test-directory (dir)
    (let* ((repo (namestring (ensure-directories-exist (merge-pathnames "repo/" dir))))
           (ev (namestring (ensure-directories-exist (merge-pathnames "ev/" dir))))
           (inner (namestring (ensure-directories-exist (merge-pathnames "ev/inner/" dir)))))
      (scratch-repository repo)
      (scratch-repository inner)
      (with-evidence-folders ((list ev))
        (dolist (arguments `(("format-patch" "-o" ,ev "-1") ("format-patch" ,(format nil "--output-directory=~a" ev) "-1")
                             ("bundle" "create" ,(concatenate 'string ev "b.bundle") "--all")
                             ("log" ,(format nil "--output=~ax.txt" ev)) ("diff" "--output" ,(concatenate 'string ev "d.txt"))
                             ("config" "--file" ,(concatenate 'string ev "cfg") "a.b" "c")
                             ("worktree" "add" ,(concatenate 'string ev "wt"))
                             ("-C" ,inner "status") (,(format nil "--work-tree=~a" ev) "checkout" "--" "a.txt")
                             (,(format nil "--git-dir=~a.git" inner) "log")))
          (true (refused-any (tuatara.defaults::run-git repo arguments)) "~s" arguments))
        (is equal '("inner") (mapcar (lambda (p) (car (last (pathname-directory p)))) (uiop:subdirectories ev)))
        (true (nth-value 1 (tuatara.defaults::run-git repo '("log" "-1"))))
        ;; a repository holding evidence: no commit (its filters), its hooks never run
        (let ((holding (namestring dir)))
          (scratch-repository holding)
          (true (refused-any (tuatara.defaults::run-git holding '("commit" "-m" "x" "--allow-empty"))))
          (is string= (format nil "/dev/null~%")
              (tuatara.text:octets-string (tuatara.defaults::run-git holding '("config" "--get" "core.hooksPath")))))))))

(define-test (evidence projects-holding-evidence-untrusted)
  ;; a project holding an evidence folder runs no program of its own, trusted or not
  (with-case-folder (root evidence work)
    (let ((tuatara-user:*lsp-trusted-projects* (list root)))
      (with-evidence-folders ('())
        (true (tuatara.defaults::project-trusted-p root)))
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (false (tuatara.defaults::project-trusted-p root))
        (true (tuatara.defaults::project-trusted-p work))))))

(define-test (evidence holding-told-without-reading-every-folder)
  ;; whether a project holds evidence is told from the setting: a big tree beside the cases is not read
  (with-case-folder (root evidence work)
    (dotimes (i 40)
      (dotimes (j 60)
        (ensure-directories-exist (format nil "~asrc/m~d/p~d/" root i j))))
    (scratch-repository root)
    (flet ((seconds (function)
             (let ((start (get-internal-real-time)))
               (funcall function)
               (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
      (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
        (wait-for-evidence-links)
        (let (answer)
          (true (< (seconds (lambda () (setf answer (tuatara.defaults::folder-holds-evidence-p root)))) 1/2))
          (true (search "is an evidence folder" answer)))
        (false (tuatara.defaults::folder-holds-evidence-p (concatenate 'string root "src/")))
        (let ((tuatara-user:*lsp-trusted-projects* (list root)))
          (true (< (seconds (lambda () (tuatara.defaults::project-trusted-p (concatenate 'string root "src/")))) 1/2)))
        (true (< (seconds (lambda () (tuatara.defaults::run-git root '("status" "--porcelain")))) 5)))
      (with-evidence-folders ('("/nowhere/*/evidence/"))
        (wait-for-evidence-links)
        (true (< (seconds (lambda () (tuatara.defaults::folder-holds-evidence-p root))) 1/10))
        (false (tuatara.defaults::folder-holds-evidence-p root)))
      ;; where only reading can tell, a tree too large counts as holding, said so
      (let ((tuatara.defaults::*evidence-holding-search-folders* 100))
        (with-evidence-folders ('((:name "evidence")))
          (wait-for-evidence-links)
          (true (search "more than 100 folders" (tuatara.defaults::folder-holds-evidence-p (concatenate 'string root "src/")))))))))

(define-test (evidence folders-written-not-read)
  ;; a folder you may write in but not list (-wx) takes new files, as before, the setting on or off
  (with-test-directory (dir)
    (let* ((root (namestring dir))
           (blind (namestring (ensure-directories-exist (merge-pathnames "blind/" dir)))))
      (put-file (concatenate 'string blind "old.txt") "old")
      (sb-posix:chmod blind #o300)
      (unwind-protect
           (dolist (setting (list '() (list (concatenate 'string root "elsewhere/*/evidence/"))))
             (with-evidence-folders (setting)
               (tuatara.io:write-new-file (concatenate 'string blind "new.txt") "new")
               (tuatara.io:write-text-file (tuatara.text:string->rope "saved") (concatenate 'string blind "saved.txt"))
               (tuatara.io:remove-file (concatenate 'string blind "new.txt"))
               (tuatara.io:remove-file (concatenate 'string blind "saved.txt"))
               (is string= "old" (file-text (concatenate 'string blind "old.txt")))))
        (sb-posix:chmod blind #o700)))))

(define-test (evidence saved-apart-said-plainly)
  ;; a file with other names in a folder that may not be written in is refused, saying why
  (with-case-folder (root evidence work)
    (let* ((shut (namestring (ensure-directories-exist (concatenate 'string work "shut/"))))
           (other (concatenate 'string shut "a.txt")))
      (sb-posix:link (concatenate 'string evidence "a.txt") other)
      (sb-posix:chmod shut #o500)
      (unwind-protect
           (with-evidence-folders (`(,(concatenate 'string root "cases/*/evidence/")))
             (handler-case (progn (tuatara.io:write-text-file (tuatara.text:string->rope "x") other) (false t))
               (tuatara.io:file-system-error (e) (true (search "may not be written in" (princ-to-string e)))))
             (is string= "kept as found" (file-text other)))
        (sb-posix:chmod shut #o700)))))

(define-test (evidence links-learnt-as-written-through)
  ;; an entry that may match anywhere learns the folder a link leads to once the link is written through
  (with-case-folder (root evidence work)
    (let* ((stick (namestring (ensure-directories-exist (merge-pathnames "media/STICK2/" root))))
           (link (concatenate 'string work "evidence")))
      (sb-posix:symlink (string-right-trim "/" stick) link)
      (with-evidence-folders ('((:name "evidence")))
        (wait-for-evidence-links)
        (true (refused (tuatara.io:write-new-file (concatenate 'string link "/n.txt") "x")))
        (true (refused (tuatara.io:write-new-file (concatenate 'string stick "n.txt") "x")))
        (true (find-if (lambda (line) (search "not looked for, as it may match anywhere" line))
                       (tuatara.defaults::evidence-health-lines))))
      (sb-posix:unlink link))))
