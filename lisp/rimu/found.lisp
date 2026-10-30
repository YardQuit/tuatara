;;;; lisp/rimu/found.lisp --- Rimu lists of files found under a directory: by name or text, and duplicates.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x rimu-find lists the files under a directory whose names match a
;;;; pattern (*.pdf), as Emacs's find-name-dired; M-x rimu-find-text those
;;;; whose text matches, as find-grep-dired; M-x rimu-duplicates the files
;;;; that are the same as another, grouped, as the dired-duplicates package.
;;;; Each is a rimu list like any other, its names the paths from the
;;;; directory searched: marks, C R D, the preview and the columns work on
;;;; them. The walk is Tuatara's own (FILES-UNDER), no find program run, as a
;;;; job of the list with its progress in the mode line; duplicates are files
;;;; of the same size whose SHA-256 sums agree. g r searches again; after an
;;;; operation only the files listed are looked at again. RIMU-SHOW-PATHS
;;;; lists given files, from any directories, in an order of its own, as a
;;;; command shows what it found (a ranking of photos).

(in-package #:tuatara.defaults)

(defun rimu-found-p (buffer)
  "True when the rimu list BUFFER lists files found (\\[rimu-find],
\\[rimu-find-text], \\[rimu-duplicates]) rather than a directory: what was
searched for then, a plist."
  (tuatara.buffer:buffer-local buffer 'rimu-found))

(defun rimu-forget-found (buffer)
  "Make the rimu list BUFFER a list of a directory again, no longer of files found."
  (let ((job (tuatara.buffer:buffer-local buffer 'rimu-search)))
    (when job (setf (car (tuatara.runtime:job-cancel job)) t)))
  (setf (tuatara.buffer:buffer-local buffer 'rimu-found) nil
        (tuatara.buffer:buffer-local buffer 'rimu-search) nil))

(defun rimu-found-entry (directory relative)
  "The entry of a list of files found under DIRECTORY (a native name ending
in /) for the one at RELATIVE, its path from there (a directory's ending in
/): a TUATARA.IO:LISTED named by that path; NIL when it is no longer there."
  (let* ((name (string-right-trim "/" relative))
         (path (concatenate 'string directory name))
         (stamp (ignore-errors (tuatara.io:path-stamp path :follow nil))))
    (when stamp
      (tuatara.io:make-listed name (tuatara.text:string-octets name) stamp (tuatara.io:stamp-size stamp) nil nil
                              (and (eq (tuatara.io:stamp-kind stamp) :symlink)
                                   (ignore-errors (tuatara.io:path-stamp path)))))))

(defun rimu-duplicates-words (entries groups)
  "The line under a list of duplicates saying what it holds: how many files in
how many groups, and what the copies beyond the first of each take."
  (let ((sizes (make-hash-table)) (counts (make-hash-table)))
    (loop for entry across entries
          for group across groups
          do (setf (gethash group sizes) (tuatara.io:stamp-size (tuatara.io:listed-stamp entry)))
             (incf (gethash group counts 0)))
    (if (zerop (length entries))
        "No two files the same"
        (format nil "~:d file~:p the same as another, in ~:d group~:p; the copies take ~a"
                (length entries) (hash-table-count counts)
                (octets-words (loop for group being the hash-keys of counts using (hash-value n)
                                    sum (* (1- n) (gethash group sizes))))))))

(defun rimu-found-again (listing)
  "LISTING, a list of files found, with each file looked at again: those gone
left out, and of duplicates the groups left with one file; a group numbered
below 0, of a list in an order of its own (RIMU-SHOW-PATHS), kept whatever
its size."
  (let* ((directory (rimu-listing-directory listing))
         (groups (rimu-listing-groups listing))
         (kept (loop for entry across (rimu-listing-entries listing)
                     for i from 0
                     for again = (rimu-found-entry directory (tuatara.io:listed-name entry))
                     when again collect (cons again (and groups (aref groups i))))))
    (if groups
        (let* ((counts (let ((table (make-hash-table)))
                         (dolist (k kept table) (incf (gethash (cdr k) table 0)))))
               (left (remove-if (lambda (k) (and (plusp (cdr k)) (< (gethash (cdr k) counts) 2))) kept))
               (entries (map 'simple-vector #'car left))
               (groups (map 'simple-vector #'cdr left)))
          (make-rimu-listing directory entries nil nil (rimu-duplicates-words entries groups) groups))
        (make-rimu-listing directory (map 'simple-vector #'car kept) nil nil (rimu-listing-found listing)))))

;;; Finding, as a job of the list

(defstruct (rimu-search (:include tuatara.runtime:job) (:constructor make-rimu-search (buffer spec)) (:copier nil))
  "Files being found for the rimu list BUFFER, a job (TUATARA.RUNTIME:JOB):
SPEC, what is searched for (RIMU-FOUND-P); SEEN, how many files were looked
at; while duplicates are compared, the car of DONE the octets summed of
TOTAL; LISTING, what was found, once done or stopped."
  spec (seen 0) (done (list 0)) (total 0) listing)

(setf (documentation 'rimu-search-p 'function) "True when OBJECT is a RIMU-SEARCH.")

(defun rimu-search-what (spec)
  "What SPEC searches for, in words: *.pdf in ~/x, duplicates in ~/x."
  (let ((where (abbreviated-path (getf spec :directory))))
    (ecase (getf spec :kind)
      (:name (format nil "~a in ~a" (getf spec :pattern) where))
      (:text (format nil "files holding ~a in ~a" (getf spec :text) where))
      (:duplicates (format nil "duplicates in ~a" where))
      (:given (getf spec :words)))))

(defmethod tuatara.runtime:job-percent ((job rimu-search))
  "How much of the files to look through JOB, a search for files, has gone
through, from 0 to 99; NIL while they are counted."
  (and (plusp (rimu-search-total job))
       (tuatara.runtime:running-percent (car (rimu-search-done job)) (rimu-search-total job))))

(tuatara.runtime:define-job-words rimu-search
  :doing (if (plusp (rimu-search-total job)) "Comparing" "Finding")
  :description (format nil "Finding ~a (~:d looked at)" (rimu-search-what (rimu-search-spec job)) (rimu-search-seen job))
  :verb (format nil "find ~a" (rimu-search-what (rimu-search-spec job)))
  :left-words (if (eq (getf (rimu-search-spec job) :kind) :duplicates)
      "no duplicates listed"
      "the files found so far listed")
  :at-exit :quiet)

(defmethod tuatara.runtime:job-holds-queue-p ((job rimu-search))
  "False: other work on the list need not wait for a search for files."
  nil)

(defun rimu-search-name (job path relative)
  "True when the file at PATH (RELATIVE its path from the directory searched)
is one JOB looks for by name or by text."
  (let* ((spec (rimu-search-spec job))
         (directory-p (uiop:string-suffix-p relative "/"))
         (name (tuatara.io:native-file-name relative)))
    (ecase (getf spec :kind)
      (:name (let ((pattern (getf spec :pattern)))
               (glob-match pattern name :test (if (some #'upper-case-p pattern) #'char= #'char-equal))))
      (:text (and (not directory-p)
                  (lines-on-disk path relative (getf spec :directory) (getf spec :text)
                                 :cancel (rimu-search-cancel job) :most 1 :parallel nil))))))

(defun rimu-search-found (job)
  "Find the files JOB searches for, on its thread: a RIMU-LISTING."
  (let* ((spec (rimu-search-spec job))
         (directory (getf spec :directory))
         (cancel (rimu-search-cancel job))
         (found '()))
    (flet ((walk (root each &key directories)
             (files-under root :limit most-positive-fixnum :hidden (getf spec :hidden) :cancel cancel
                               :directories directories
                               :each (lambda (path relative)
                                       (incf (rimu-search-seen job))
                                       (funcall each path relative)))))
      (if (eq (getf spec :kind) :duplicates)
          (let ((by-size (make-hash-table)))
            ;; files of one size only may be the same
            (dolist (root (getf spec :roots))
              (let ((prefix (subseq root (length directory))))
                (walk root (lambda (path relative)
                             (let ((stamp (ignore-errors (tuatara.io:path-stamp path :follow nil))))
                               (when (and stamp (eq (tuatara.io:stamp-kind stamp) :regular))
                                 (push (concatenate 'string prefix relative) (gethash (tuatara.io:stamp-size stamp) by-size))))))))
            (let ((alike (loop for size being the hash-keys of by-size using (hash-value names)
                               when (rest names) collect (cons size (sort names #'string<))))
                  (buffer (make-array (* 1024 1024) :element-type '(unsigned-byte 8)))
                  (groups '()) (group 0))
              (setf (rimu-search-total job) (max 1 (loop for (size . names) in alike sum (* size (length names)))))
              ;; the largest first
              (dolist (same (sort alike #'> :key #'car))
                (let ((by-sum (make-hash-table :test 'equal)))
                  (dolist (relative (cdr same))
                    (when (car cancel) (return-from rimu-search-found nil))
                    (let ((sum (file-sha256 (concatenate 'string directory relative)
                                            :cancel cancel :done (rimu-search-done job) :buffer buffer)))
                      (when sum (push relative (gethash sum by-sum)))))
                  (loop for names being the hash-values of by-sum
                        when (rest names)
                          do (incf group)
                             (dolist (relative (sort names #'string<))
                               (let ((entry (rimu-found-entry directory relative)))
                                 (when entry (push (cons entry group) groups)))))))
              (let* ((pairs (nreverse groups))
                     (entries (map 'simple-vector #'car pairs))
                     (numbers (map 'simple-vector #'cdr pairs)))
                (make-rimu-listing directory entries nil nil (rimu-duplicates-words entries numbers) numbers))))
          (if (eq (getf spec :kind) :given)
              ;; given files, in their groups and order: nothing to walk
              (let ((pairs (loop for (path . group) in (getf spec :paths)
                                 for entry = (rimu-found-entry directory (subseq path (length directory)))
                                 when entry collect (cons entry group))))
                (make-rimu-listing directory (map 'simple-vector #'car pairs) nil nil (getf spec :words)
                                   (map 'simple-vector #'cdr pairs)))
          (progn
            (walk directory (lambda (path relative)
                              (when (rimu-search-name job path relative)
                                (let ((entry (rimu-found-entry directory relative)))
                                  (when entry (push entry found)))))
                  :directories (if (eq (getf spec :kind) :name) :also nil))
            (make-rimu-listing directory (coerce (nreverse found) 'simple-vector) nil nil
                               (format nil "~:d found: ~a~@[ (stopped)~]" (length found) (rimu-search-what spec) (car cancel)))))))))

(defun rimu-start-search (buffer)
  "Find again the files the rimu list BUFFER lists (RIMU-FOUND-P), as a job
of it, the list saying meanwhile that they are being found."
  (let* ((spec (rimu-found-p buffer))
         (old (tuatara.buffer:buffer-local buffer 'rimu-search))
         (job (make-rimu-search buffer spec)))
    (when old (setf (car (tuatara.runtime:job-cancel old)) t))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-search) job
          (tuatara.buffer:buffer-local buffer 'rimu-listing) nil)
    (rimu-show-layout buffer (rimu-reading-layout (getf spec :directory) (format nil "Finding ~a..." (rimu-search-what spec)))
                      nil :fresh t)
    (tuatara.runtime:start-job-thread
     job (lambda (job)
           (let ((listing (rimu-search-found job)))
             (setf (rimu-search-listing job) listing)))
     :name "tuatara rimu search" :details (rimu-search-what spec))
    job))

(defmethod tuatara.runtime:job-take-in ((job rimu-search))
  "Show in its list what JOB found, when the list still waits for it; say how
many, or why there are none."
  (let ((buffer (rimu-search-buffer job)))
    (when (and (live-buffer-p buffer) (eq (tuatara.buffer:buffer-local buffer 'rimu-search) job))
      (setf (tuatara.buffer:buffer-local buffer 'rimu-search) nil)
      (let ((listing (or (rimu-search-listing job)
                         (make-rimu-listing (getf (rimu-search-spec job) :directory) #() nil nil
                                            (substitute-command-keys
                                             "Nothing listed: stopped; \\<rimu-mode>\\[revert-buffer] looks again")))))
        (setf (tuatara.buffer:buffer-local buffer 'rimu-listing) listing)
        (rimu-update buffer :fresh t)
        (if (eq (rimu-search-status job) :done)
            (message "~a" (rimu-listing-found listing))
            (message "~a" (tuatara.runtime:job-ending-words job)))))))

(defun rimu-show-found (spec name)
  "Show a rimu list, called NAME, of the files SPEC searches for (RIMU-FOUND-P),
finding them in the background."
  (let ((buffer (rimu-make-list (getf spec :directory) :name name)))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-found) spec)
    ;; names starting with a dot, when looked for, are shown
    (when (getf spec :hidden)
      (setf (tuatara.buffer:buffer-local buffer 'rimu-dotfiles) t))
    (show-buffer buffer)
    (rimu-start-search buffer)
    buffer))

(defun rimu-show-paths (groups &key name words safe (details nil details-p))
  "Show a rimu list, called NAME, of the files of GROUPS, a list of lists of
full native names, in that order: each group after the one before, a line
apart, its files in the order given; the files may stand in any directories,
each named by its path from the one all are in. WORDS is the line saying what
the list holds; with SAFE it is opened safely (its preview showing files as
they are opened safely); DETAILS, when given, says whether it shows the
details (\\<rimu-mode>\\[rimu-toggle-details]). It is like any list of files found: marks, the
preview, the operations and the columns work on it, and \\<rimu-mode>\\[revert-buffer]
looks at each file again, keeping the order. Returns the list."
  (let* ((paths (loop for group in groups
                      for number downfrom -1
                      append (loop for path in group collect (cons path number))))
         (directory (paths-common-directory (mapcar #'car paths)))
         (spec (list :kind :given :directory directory :paths paths :words words))
         (buffer (rimu-make-list directory :name (or name (rimu-buffer-name directory)))))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-found) spec)
    (when safe (setf (tuatara.buffer:buffer-local buffer :safe-visit) t))
    (when details-p (setf (tuatara.buffer:buffer-local buffer 'rimu-details) details))
    (show-buffer buffer)
    (rimu-start-search buffer)
    buffer))

(defun rimu-search-directory (prompt)
  "A directory asked for with PROMPT, from where a question for a path starts
\(PATH-QUESTION-START), as a native name ending in /; NIL when none."
  (let ((chosen (read-native-path prompt)))
    (and chosen (directory-name chosen :clean t))))

(define-command rimu-find ()
  "List in a rimu list the files and directories under a directory asked for
whose names match a pattern: * stands for any characters, ? for one
\(*.pdf, report-202?.*); upper and lower case alike unless the pattern has an
upper-case letter. Names starting with a dot and backups are left out; with
\\[universal-argument] first, those starting with a dot are looked in too. The search is
Tuatara's own, in the background (\\[stop-job] stops it); the list works as
any other, its names the paths from that directory, and \\<rimu-mode>\\[revert-buffer]
searches again."
  (let* ((hidden (and (prefix-argument) t))
         (directory (rimu-search-directory "Find in: "))
         (pattern (and directory (read-string "Names matching (*.pdf): "))))
    (when (and pattern (plusp (length pattern)))
      (rimu-show-found (list :kind :name :directory directory :pattern pattern :hidden hidden)
                       (format nil "*Found ~a in ~a*" pattern (abbreviated-path directory))))))

(define-command rimu-find-text ()
  "List in a rimu list the files under a directory asked for whose text
matches what you type, as \\[search-in-files] matches a line: each word a
regular expression or its own characters, all on one line. Files that are not
text, names starting with a dot and backups are left out; with
\\[universal-argument] first, those starting with a dot are looked in too. In the
background (\\[stop-job] stops it); \\<rimu-mode>\\[revert-buffer] searches again."
  (let* ((hidden (and (prefix-argument) t))
         (directory (rimu-search-directory "Find files in: "))
         (text (and directory (read-string "Files holding: "))))
    (when (and text (plusp (length text)))
      (rimu-show-found (list :kind :text :directory directory :text text :hidden hidden)
                       (format nil "*Found ~a in ~a*" text (abbreviated-path directory))))))

(define-command rimu-duplicates ()
  "List in a rimu list the files that are the same as another, in groups:
in the directories marked in this rimu list, or
else under a directory asked for. Files of the same size are compared by their
SHA-256 sums, in the background, its progress in the mode line (\\[stop-job]
stops it); the largest first. Names starting with a dot and backups are left
out; with \\[universal-argument] first, those starting with a dot are looked in too. Mark
the copies you do not want and \\<rimu-mode>\\[list-delete-marked] puts them in the trash; the list keeps
up, a group left with one file leaving it. \\[revert-buffer] compares again."
  (let* ((hidden (and (prefix-argument) t))
         (buffer (current-buffer))
         (marked (and (rimu-list-p buffer) (not (rimu-found-p buffer))
                      (remove-if-not #'rimu-directory-p
                                     (let ((marks (tuatara.buffer:buffer-local buffer 'rimu-marks)))
                                       (and marks (plusp (hash-table-count marks)) (rimu-chosen buffer #\>))))))
         (directory (if marked
                        (tuatara.buffer:buffer-local buffer 'rimu-directory)
                        (rimu-search-directory "Find duplicates in: "))))
    (when directory
      (rimu-show-found (list :kind :duplicates :directory directory :hidden hidden
                             :roots (if marked
                                        (mapcar (lambda (entry) (directory-name (rimu-path directory entry) :clean t)) marked)
                                        (list directory)))
                       (format nil "*Duplicates in ~a*" (abbreviated-path directory))))))
