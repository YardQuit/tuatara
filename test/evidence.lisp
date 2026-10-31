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
