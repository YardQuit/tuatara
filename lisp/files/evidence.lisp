;;;; lisp/files/evidence.lisp --- Evidence folders: every file in them opened safely, and nothing written in them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; *EVIDENCE-FOLDERS* names the folders whose files are evidence: a path
;;;; with * and ** in it, a folder's name, or a pattern for case folders with
;;;; the folder of evidence in each. A file is evidence when a folder on its
;;;; real path, links followed and a drive mounted twice seen under both its
;;;; names, is one of them; everything below it is too. Such a file opens
;;;; safely however it is opened, and the editor writes nothing there: the
;;;; core asks EVIDENCE-WRITING-REFUSAL before every file it writes, makes,
;;;; renames or deletes (TUATARA.IO:*WRITING-REFUSAL-FUNCTION*), told where the
;;;; folder it opened really is. Names are compared without regard to case, as
;;;; some drives store them. Each folder's answer is kept until the setting
;;;; changes or the folder is another one (a drive mounted over it); with no
;;;; folder named, nothing is looked at. COPY-FROM-EVIDENCE copies files out,
;;;; each checked by its SHA-256, with a log beside the copies.

(in-package #:tuatara.defaults)

(defvar *evidence-folders* '()
  "The folders whose files are evidence: each opens safely however it is
opened (read-only, no mode, hooks or version control run on it), and
nothing is written in such a folder or below it, by saving, saving as,
exporting, copying, moving, renaming, deleting or making folders, nor any
of the editor's own files (backups, auto-saves, locks). The folder itself is
not renamed, moved or deleted either, nor a folder holding one. Empty, as
standard: no folder is evidence and nothing is looked at.

Each entry is one of:
  \"~/cases/*/evidence/\"   a folder's path, * standing for any name of one
                           folder, ** for any folders, none too;
  (:name \"evidence\" :within \"~/cases/\")
                           every folder of that name, within the folder
                           :within names (anywhere without it);
  (:regex \"^[0-9]{4}-[0-9]{4}$\" :within \"~/cases/\" :then \"evidence\")
                           in every folder within ~/cases/ whose whole name
                           the regular expression matches (as searches read
                           one), the folder :then names inside it (that
                           folder itself without :then).
Names are compared without regard to case. A file is evidence when the
folder it really is in, links followed, is one of these or below one, and a
drive mounted in a second place is evidence under both names. An entry that
cannot be read is said in *Messages* and by \\[check-health], and left out.
\\[evidence-folder-p] says whether a file is evidence and by which entry;
\\[copy-from-evidence] copies files out of one, each checked by its SHA-256.")

;;; The entries as read

(defstruct (evidence-entry (:constructor make-evidence-entry (given kind pattern within then)) (:copier nil))
  "An entry of *EVIDENCE-FOLDERS* as it is read: GIVEN, the entry as it was
written; KIND, :GLOB, :NAME or :REGEX; PATTERN, the glob (its fixed folders
at the front where they really are), the name, or the scanner; WITHIN, the
folder (where it really is, with no / at its end) it is held to, or NIL;
THEN, the names of the folders below a matched one that are the evidence."
  given kind pattern within then)

(setf (documentation 'evidence-entry-given 'function) "The entry of *EVIDENCE-FOLDERS* as written."
      (documentation 'evidence-entry-kind 'function) "What kind of entry it is: :GLOB, :NAME or :REGEX."
      (documentation 'evidence-entry-pattern 'function) "The glob, the folder's name or the scanner of the entry."
      (documentation 'evidence-entry-within 'function) "The folder the entry is held to, or NIL."
      (documentation 'evidence-entry-then 'function) "The names of the folders below a matched one that are the evidence."
      (documentation 'evidence-entry-p 'function) "True when OBJECT is an EVIDENCE-ENTRY.")

(defun real-folder-name (path)
  "PATH, a folder's path (~ for the home folder), as a native name from the
root where it really is as far as it is there (REAL-PATH-SO-FAR), with no /
at its end (/ for the root)."
  (let ((real (tuatara.io:real-path-so-far (tuatara.io:native (tuatara.io:expand-path path)))))
    (if (zerop (length real)) "/" real)))

(defun glob-fixed-end (glob)
  "Where the folders at the front of GLOB that hold none of * ? [ \\ end:
the position of the / after the last of them."
  (let ((special (position-if (lambda (c) (find c "*?[\\")) glob)))
    (or (position #\/ glob :end (or special (length glob)) :from-end t) 0)))

(defun read-glob-entry (given)
  "GIVEN, a folder's path with * and ** in it, read as an evidence entry; an
error saying why it cannot be."
  (when (zerop (length (string-trim " " given)))
    (error "an empty path names no folder"))
  (unless (or (char= (char given 0) #\/) (char= (char given 0) #\~))
    (error "~s is not a path from the root or from ~~/" given))
  (let* ((glob (string-right-trim "/" (tuatara.io:native (tuatara.io:expand-path given))))
         (end (glob-fixed-end glob))
         (front (if (plusp end) (real-folder-name (subseq glob 0 end)) "")))
    (make-evidence-entry given :glob (concatenate 'string (string-right-trim "/" front) (subseq glob end)) nil nil)))

(defun read-folder-within (within)
  "The folder :within names, WITHIN, where it really is; an error when it is
not a path from the root or from ~/."
  (unless (and (stringp within) (plusp (length within)) (find (char within 0) "/~"))
    (error ":within ~s is not a folder's path from the root or from ~~/" within))
  (real-folder-name within))

(defun read-list-entry (given)
  "GIVEN, a list beginning with :name or :regex, read as an evidence entry; an
error saying why it cannot be."
  (unless (and (keywordp (first given)) (evenp (length given)))
    (error "~s is not :name or :regex followed by keywords and their values" given))
  (loop for (key) on given by #'cddr
        unless (member key '(:name :regex :within :then))
          do (error "~s is none of :name, :regex, :within and :then" key))
  (let ((name (getf given :name)) (regex (getf given :regex))
        (within (getf given :within)) (then (getf given :then)))
    (when (and name regex) (error "an entry has :name or :regex, not both"))
    (let ((within (and within (read-folder-within within))))
      (cond (name
             (unless (and (stringp name) (plusp (length name)) (not (find #\/ name)))
               (error ":name ~s is not the name of one folder" name))
             (when then (error ":then goes with :regex"))
             (make-evidence-entry given :name name within nil))
            (regex
             (unless (stringp regex) (error ":regex ~s is not a string" regex))
             (let ((scanner (handler-case (tuatara.syntax:compile-pattern regex :case-fold t)
                              (error (e) (error ":regex ~s cannot be used: ~a" regex e))))
                   (names (and then
                               (if (and (stringp then) (plusp (length then)) (char/= (char then 0) #\/))
                                   (remove "" (uiop:split-string (string-right-trim "/" then) :separator "/") :test #'string=)
                                   (error ":then ~s is not a folder's name, or path, inside the one matched" then)))))
               (when (intersection names '("." "..") :test #'string=)
                 (error ":then ~s leads out of the folder matched" then))
               (make-evidence-entry given :regex scanner within names)))
            (t (error "~s has neither :name nor :regex" given))))))

(defun read-evidence-entry (given)
  "GIVEN, an entry of *EVIDENCE-FOLDERS*, as an EVIDENCE-ENTRY; an error
saying why it cannot be read."
  (cond ((stringp given) (read-glob-entry given))
        ((consp given) (read-list-entry given))
        (t (error "~s is neither a folder's path nor a list beginning with :name or :regex" given))))

;;; The setting in force

(defstruct (evidence-rules (:constructor make-evidence-rules (setting copy entries problems)) (:copier nil))
  "*EVIDENCE-FOLDERS* as read: SETTING, its value, and COPY, a copy of it, to
tell when it changes; ENTRIES, those read (EVIDENCE-ENTRY); PROBLEMS, each
\(GIVEN . WHY) for an entry that could not be; FOLDERS, each folder's answer
by its real path (FOLDER-EVIDENCE)."
  setting copy entries problems
  (folders (make-hash-table :test 'equal :synchronized t)))

(setf (documentation 'evidence-rules-setting 'function) "The value of *EVIDENCE-FOLDERS* the rules were read from."
      (documentation 'evidence-rules-copy 'function) "A copy of the value the rules were read from."
      (documentation 'evidence-rules-entries 'function) "The entries read."
      (documentation 'evidence-rules-problems 'function) "The entries not read, each (GIVEN . WHY)."
      (documentation 'evidence-rules-folders 'function) "Each folder's answer so far, by its real path."
      (documentation 'evidence-rules-p 'function) "True when OBJECT is an EVIDENCE-RULES.")

(defvar *evidence-rules* nil
  "*EVIDENCE-FOLDERS* as last read (EVIDENCE-RULES), or NIL.")

(defvar *evidence-generation* 0
  "How many times *EVIDENCE-FOLDERS* has been read anew: a buffer's answer
\(BUFFER-EVIDENCE) stands while it is the same.")

(defvar *evidence-lock* (sb-thread:make-mutex :name "tuatara evidence folders")
  "Held while *EVIDENCE-FOLDERS* is read anew.")

(defun evidence-rules ()
  "*EVIDENCE-FOLDERS* as read now (EVIDENCE-RULES), read again when it was
changed; NIL at once when it names no folder."
  (let ((value *evidence-folders*))
    (when value
      (let ((rules *evidence-rules*))
        (if (and rules (eq value (evidence-rules-setting rules)) (equal value (evidence-rules-copy rules)))
            rules
            (sb-thread:with-mutex (*evidence-lock*)
              (let ((rules *evidence-rules*))
                (if (and rules (eq value (evidence-rules-setting rules)) (equal value (evidence-rules-copy rules)))
                    rules
                    (let ((entries '()) (problems '()))
                      (dolist (given (setting-list '*evidence-folders* value))
                        (handler-case (push (read-evidence-entry given) entries)
                          (error (e) (push (cons given (princ-to-string e)) problems))))
                      (incf *evidence-generation*)
                      (setf *evidence-rules* (make-evidence-rules value (copy-tree value) (nreverse entries) (nreverse problems))))))))))))

(defun evidence-problems-words ()
  "What is wrong with the entries of *EVIDENCE-FOLDERS*, a sentence each; NIL
when nothing is."
  (let ((rules (evidence-rules)))
    (and rules
         (loop for (given . why) in (evidence-rules-problems rules)
               collect (let ((*print-pretty* nil))
                         (format nil "*evidence-folders*: ~s is left out: ~a" given why))))))

;;; Which folders are evidence

(defun within-folder-p (folder top)
  "True when FOLDER, a real path, is TOP or within it, their case disregarded."
  (tuatara.io:path-within-p (string-downcase folder) (string-downcase top)))

(defun folder-names (folder)
  "The names of the folders on the path FOLDER, from the root."
  (remove "" (uiop:split-string folder :separator "/") :test #'string=))

(defun whole-name-match-p (scanner name)
  "True when SCANNER, a regular expression, matches all of NAME."
  (multiple-value-bind (start end) (cl-ppcre:scan scanner name)
    (and start (zerop start) (= end (length name)))))

(defun evidence-entry-matches-p (entry folder)
  "True when FOLDER, a folder's real path with no / at its end, is an
evidence folder by ENTRY itself (folders within it are not looked at here)."
  (let ((within (evidence-entry-within entry)))
    (ecase (evidence-entry-kind entry)
      (:glob (glob-match (evidence-entry-pattern entry) folder :test #'char-equal))
      (:name (and (string-equal (tuatara.io:native-file-name folder) (evidence-entry-pattern entry))
                  (or (null within) (within-folder-p folder within))))
      (:regex (let* ((names (folder-names folder))
                     (then (evidence-entry-then entry))
                     (case (- (length names) (length then) 1)))
                (and (>= case 0)
                     (every #'string-equal then (nthcdr (1+ case) names))
                     (whole-name-match-p (evidence-entry-pattern entry) (nth case names))
                     (or (null within)
                         (within-folder-p (format nil "/~{~a~^/~}" (subseq names 0 (1+ case))) within))))))))

(defparameter *mounts-info-file* "/proc/self/mountinfo"
  "Where the drives mounted, and the folder of each shown at each place, are read from.")

(defun mounts-shown ()
  "The drives mounted, each (DEVICE ROOT PLACE): the device's numbers, the
folder of it shown, and where it is shown, their escapes read (MOUNT-FIELD)."
  (with-open-file (in *mounts-info-file* :if-does-not-exist nil)
    (and in
         (loop for line = (read-line in nil)
               while line
               for fields = (uiop:split-string line :separator " ")
               when (>= (length fields) 5)
                 collect (list (third fields) (mount-field (fourth fields)) (mount-field (fifth fields)))))))

(defvar *mounts* :unread
  "The drives mounted (MOUNTS-SHOWN) while one folder is looked at: read
once when first wanted.")

(defun folder-aliases (folder)
  "The other paths that lead to the folder FOLDER (a real path) through a
drive, or a folder of one, mounted in more than one place."
  (when (eq *mounts* :unread)
    (setf *mounts* (ignore-errors (mounts-shown))))
  (let ((here (loop with best = nil
                    for mount in *mounts*
                    when (and (tuatara.io:path-within-p folder (third mount))
                              (or (null best) (> (length (third mount)) (length (third best)))))
                      do (setf best mount)
                    finally (return best))))
    (when here
      (destructuring-bind (device root place) here
        (let ((inside (concatenate 'string (string-right-trim "/" root) (subseq folder (length (string-right-trim "/" place))))))
          (loop for (other-device other-root other-place) in *mounts*
                when (and (string= device other-device)
                          (not (and (string= root other-root) (string= place other-place)))
                          (tuatara.io:path-within-p inside other-root))
                  collect (let ((alias (concatenate 'string (string-right-trim "/" other-place)
                                                    (subseq inside (length (string-right-trim "/" other-root))))))
                            (if (zerop (length alias)) "/" alias))))))))

(defun folder-above (folder)
  "The folder FOLDER (a path with no / at its end) is in, with no / at its
end: / for one at the root."
  (let ((above (string-right-trim "/" (tuatara.io:parent-path folder))))
    (if (zerop (length above)) "/" above)))

(defvar *aliases-followed* nil
  "True while a folder's other names are looked at, so that theirs are not.")

(defun folder-evidence (rules folder)
  "The evidence folder FOLDER (a real path with no / at its end, / for the
root) is, or is within, by RULES, as (EVIDENCE-FOLDER . ENTRY), or NIL: by
its own path or any folder above it, or by another path it is mounted at
too. Each folder's answer is kept until RULES change or the folder is
another (a drive mounted over it): one examination of it when kept."
  (let* ((stamp (ignore-errors (tuatara.io:path-stamp folder)))
         (key (and stamp (cons (tuatara.io:stamp-dev stamp) (tuatara.io:stamp-ino stamp))))
         (kept (and key (gethash folder (evidence-rules-folders rules)))))
    (if (and kept (equal (cdr kept) key) (not *aliases-followed*))
        (car kept)
        (let ((found (or (let ((entry (find-if (lambda (entry) (evidence-entry-matches-p entry folder)) (evidence-rules-entries rules))))
                           (and entry (cons folder entry)))
                         (and (string/= folder "/")
                              (folder-evidence rules (folder-above folder)))
                         ;; its other names last: by its own, the folder is named as you know it
                         (and (not *aliases-followed*)
                              (let ((*aliases-followed* t))
                                (some (lambda (alias) (folder-evidence rules alias)) (folder-aliases folder)))))))
          (when (and key (not *aliases-followed*))
            (setf (gethash folder (evidence-rules-folders rules)) (cons found key)))
          found))))

(defun place-real-path (path)
  "Where the place PATH names really is: the folder it is in where that
really is (REAL-PATH-SO-FAR), with PATH's own name after it, not followed
when it is a link; / for the root."
  (let ((name (string-right-trim "/" (tuatara.io:native (tuatara.io:expand-path path)))))
    (if (zerop (length name))
        "/"
        (let ((folder (string-right-trim "/" (tuatara.io:real-path-so-far (tuatara.io:parent-path name)))))
          (concatenate 'string folder "/" (tuatara.io:native-file-name name))))))

(defun evidence-match (rules real)
  "The evidence folder the place at REAL (a real path) is, or is in, by RULES,
as (EVIDENCE-FOLDER . ENTRY), or NIL."
  (let ((*mounts* :unread))
    (or (let ((entry (find-if (lambda (entry) (evidence-entry-matches-p entry real)) (evidence-rules-entries rules))))
          (and entry (cons real entry)))
        (if (eq :directory (tuatara.io:path-kind real :follow nil))
            (folder-evidence rules real)
            (and (string/= real "/")
                 (folder-evidence rules (folder-above real)))))))

(defun evidence-path-p (path)
  "True when the file or folder at PATH is evidence (*EVIDENCE-FOLDERS*): it
is an evidence folder, or within one, by where it really is, links on the
way followed (its own name not, when it is a link). Two values then: the
evidence folder (a native name ending in /) and the entry of
*EVIDENCE-FOLDERS* that makes it one, as written. NIL at once when the
setting names no folder."
  (let ((rules (evidence-rules)))
    (when (and rules path)
      (let ((match (evidence-match rules (place-real-path path))))
        (and match (values (tuatara.io:slash-ended (car match)) (evidence-entry-given (cdr match))))))))

(defun evidence-words (folder entry)
  "Why nothing is written in FOLDER, an evidence folder made one by ENTRY of
*EVIDENCE-FOLDERS*, as every refusal of it says it."
  (let ((*print-pretty* nil))
    (format nil "~a is an evidence folder (*evidence-folders*: ~s), and nothing is written in it"
            (abbreviated-path folder) entry)))

(defun evidence-refusal (operation path)
  "The words refusing OPERATION (a verb phrase, \"save\") on PATH, as every
refusal of a write in an evidence folder says them: NIL when PATH is no
evidence (EVIDENCE-PATH-P)."
  (multiple-value-bind (folder entry) (evidence-path-p path)
    (and folder
         (princ-to-string (make-condition 'tuatara.io:writing-refused :operation operation
                                                                       :path (abbreviated-path (tuatara.io:clean-path (tuatara.io:native (tuatara.io:expand-path path))))
                                                                       :reason (evidence-words folder entry))))))

(defun evidence-writing-refusal (path &key dirfd fd)
  "Why nothing may be written at PATH, the words of EVIDENCE-WORDS, when it is
evidence: told by where the thing open as FD, or the folder open as DIRFD
with PATH's name in it, really is, else by where PATH leads; NIL when it is
not. With PATH NIL, true when *EVIDENCE-FOLDERS* names any folder. The core
asks this before it writes (TUATARA.IO:*WRITING-REFUSAL-FUNCTION*)."
  (let ((rules (evidence-rules)))
    (cond ((null rules) nil)
          ((null path) t)
          (t (let* ((real (or (and fd (tuatara.io:fd-real-path fd))
                              (and dirfd (let ((folder (tuatara.io:fd-real-path dirfd)))
                                           (and folder (concatenate 'string (string-right-trim "/" folder) "/"
                                                                    (tuatara.io:native-file-name path)))))
                              (place-real-path path)))
                    (match (evidence-match rules real)))
               (and match (evidence-words (car match) (evidence-entry-given (cdr match)))))))))

;; asked before anything is written, from the moment the defaults are loaded
(setf tuatara.io:*writing-refusal-function* 'evidence-writing-refusal)

;;; Buffers of evidence

(defun buffer-evidence (buffer)
  "The evidence folder BUFFER's file is in, or NIL, worked out once for each
reading of *EVIDENCE-FOLDERS* (EVIDENCE-PATH-P): what the mode line asks on
every redisplay. NIL at once when the setting names no folder."
  (when *evidence-folders*
    (let ((file (tuatara.buffer:buffer-file buffer))
          (kept (tuatara.buffer:buffer-local buffer :evidence)))
      (when file
        (evidence-rules)
        (if (and kept (eql (car kept) *evidence-generation*))
            (cdr kept)
            (let ((folder (ignore-errors (evidence-path-p (tuatara.io:native file)))))
              (setf (tuatara.buffer:buffer-local buffer :evidence) (cons *evidence-generation* folder))
              folder))))))

(defun evidence-mode-line (editor buffer)
  "Mode line function, just before the buffer's name: Evidence, in
:mode-line-evidence, for a file in an evidence folder (*EVIDENCE-FOLDERS*)."
  (declare (ignore editor))
  (and (buffer-evidence buffer) (list " Evidence " :mode-line-evidence)))

(defun evidence-visit-reason (path)
  "Why the file at PATH opens safely when it is evidence, a phrase; NIL when it
is not."
  (multiple-value-bind (folder entry) (evidence-path-p path)
    (and folder
         (let ((*print-pretty* nil))
           (format nil "it is in the evidence folder ~a (*evidence-folders*: ~s)" (abbreviated-path folder) entry)))))

(defvar *evidence-folders-seen* (list nil)
  "The value of *EVIDENCE-FOLDERS* when the buffers were last told of it, and
a copy of it, as (VALUE . COPY).")

(defun evidence-tick (editor)
  "Tick function: once *EVIDENCE-FOLDERS* has changed, say what is wrong with
its entries, and make each buffer whose file is evidence now, opened before,
read-only, saying so; nothing is written there in any case. A comparison of
one value when it has not changed."
  (declare (ignore editor))
  (let ((value *evidence-folders*) (seen *evidence-folders-seen*))
    (unless (or (eq value (car seen)) (and value (equal value (cdr seen))))
      (setf *evidence-folders-seen* (cons value (copy-tree value)))
      (dolist (words (evidence-problems-words)) (message "~a" words))
      (dolist (buffer (copy-list tuatara.buffer:*buffers*))
        (let ((folder (buffer-evidence buffer)))
          (when (and folder (not (tuatara.io:safe-visit-p buffer)) (not (tuatara.buffer:buffer-local buffer :read-only)))
            (setf (tuatara.buffer:buffer-local buffer :read-only) t)
            (message "~a is in the evidence folder ~a now: read-only, and nothing is written there"
                     (tuatara.buffer:buffer-name buffer) (abbreviated-path folder)))))))
  nil)

(defun end-safe-visit-words (buffer)
  "What is said of ending BUFFER's safe visit: how to (\\[end-safe-visit]), or
that a file of evidence stays opened safely."
  (if (buffer-evidence buffer)
      "it stays so, being evidence"
      (substitute-command-keys "\\[end-safe-visit] opens it as usual")))

;;; Asking

(defun thing-at-point-path (buffer)
  "The file or folder the current line or point of BUFFER names, a native
name, or NIL: a rimu list's entry on the line, the path written at point,
or BUFFER's file."
  (or (mode-file-name-at-point (buffer-major-mode buffer) buffer)
      (let* ((point (tuatara.buffer:buffer-point buffer))
             (start (word-bounds-at buffer point #'path-part-p *popup-reach*))
             (end (nth-value 1 (word-bounds-at buffer point #'path-part-p *popup-reach*)))
             (text (and (< start end) (tuatara.buffer:buffer-string buffer :start start :end end)))
             (text (if (and text (> (length text) 5) (string= "file:" text :end2 5)) (subseq text 5) text)))
        (and text (find #\/ text)
             (tuatara.io:native (path-directory text buffer))))
      (let ((file (tuatara.buffer:buffer-file buffer)))
        (and file (tuatara.io:native file)))))

(define-command evidence-folder-p ()
  "Say whether the file or folder here is evidence, and by which entry of
*EVIDENCE-FOLDERS*: a rimu list's entry on the line, a path written at
point, or the buffer's file. Evidence opens safely, and nothing is written
where it is."
  (let ((path (thing-at-point-path (current-buffer))))
    (cond ((null path) (message "Nothing here names a file or folder"))
          ((null *evidence-folders*) (message "No: *evidence-folders* names no folder (off)"))
          (t (multiple-value-bind (folder entry) (evidence-path-p path)
               (let ((*print-pretty* nil))
                 (if folder
                     (message "Yes: ~a is in the evidence folder ~a (*evidence-folders*: ~s)"
                              (abbreviated-path path) (abbreviated-path folder) entry)
                     (message "No: ~a is in no evidence folder" (abbreviated-path path)))))))))

;;; Copying out

(defun copies-log-text (copies folder time)
  "The log written beside copies taken out of an evidence folder into FOLDER
at TIME (a universal time): what each was, where from, its size and the
SHA-256 of the original and of the copy, each of COPIES being (FROM TO SIZE
ORIGINAL-SUM COPY-SUM EVIDENCE-FOLDER ENTRY)."
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (format s "Copied out of evidence by ~a~%" (tuatara:version-line))
      (format s "When: ~a~%Into: ~a~%Files: ~d~%" (tuatara.dates:format-time "%F %T %z" time) folder (length copies))
      (loop for (from to size original copy evidence entry) in copies
            do (format s "~%File: ~a~%  From: ~a~%  Evidence folder: ~a (*evidence-folders*: ~s)~%  Size: ~:d octets~%  SHA-256 of the original: ~a~%  SHA-256 of the copy:     ~a~%"
                       (tuatara.io:native-file-name to) from evidence entry size original copy)))))

(defun write-copies-log (copies folder time)
  "Write the log of COPIES (COPIES-LOG-TEXT) in FOLDER, a new file read-only
from the start, copied-from-evidence-DATE-TIME.log, with (2) and so on
before its ending when that is taken; returns its path."
  (multiple-value-bind (fd path)
      (tuatara.io:create-free-file folder (lambda (n)
                                            (tuatara.io:numbered-name
                                             (format nil "copied-from-evidence-~a.log" (tuatara.dates:format-time "%Y%m%d-%H%M%S" time))
                                             n))
                                   :mode #o400)
    (unwind-protect (tuatara.io:write-octets fd (tuatara.text:string-octets (copies-log-text copies folder time)) :path path)
      (sb-posix:close fd))
    path))

(defun copy-from-evidence-work (pairs folder cancel logged)
  "Copy each of PAIRS, (FROM . TO), checked (COPY-FILE-CHECKED), on the thread
calling it, stopping between files when CANCEL's car turns true; then, or
when it stops or fails, write the log of those copied beside them in FOLDER
\(WRITE-COPIES-LOG) and call LOGGED with its path."
  (let ((copies '()) (time (get-universal-time)))
    (unwind-protect
         (dolist (pair pairs)
           (when (car cancel) (return))
           (destructuring-bind (from . to) pair
             (multiple-value-bind (evidence entry) (evidence-path-p from)
               (multiple-value-bind (copy original) (copy-file-checked from to :cancel cancel)
                 (when copy
                   (push (list from to (tuatara.io:stamp-size (tuatara.io:path-stamp to)) original copy evidence entry)
                         copies))))))
      (when copies
        (funcall logged (write-copies-log (reverse copies) folder time))))))

(defstruct (evidence-copy (:include tuatara.runtime:job) (:constructor make-evidence-copy (pairs folder)) (:copier nil))
  "Files being copied out of evidence, a job: PAIRS, each (FROM . TO), into
FOLDER; LOG, the log written beside them once done."
  pairs folder log)

(setf (documentation 'evidence-copy-pairs 'function) "The files being copied, each (FROM . TO)."
      (documentation 'evidence-copy-folder 'function) "Where the files are copied to."
      (documentation 'evidence-copy-log 'function) "The log written beside the copies, once done."
      (documentation 'evidence-copy-p 'function) "True when OBJECT is an EVIDENCE-COPY.")

(tuatara.runtime:define-job-words evidence-copy
  :doing "Copying out of evidence"
  :description (format nil "Copying ~:d file~:p out of evidence to ~a"
                       (length (evidence-copy-pairs job)) (abbreviated-path (evidence-copy-folder job)))
  :verb "copy them"
  :left-words "the file under way is taken away")

(defmethod tuatara.runtime:job-take-in ((job evidence-copy))
  "Say how many files were copied out, each checked, and where the log is; or why not."
  (rimu-refresh-lists (list (evidence-copy-folder job)))
  (let ((log (evidence-copy-log job)))
    (if (and log (eq (tuatara.runtime:job-status job) :done))
        (message "Copied ~:d file~:p to ~a, each copy's SHA-256 checked against its original; the log is ~a"
                 (length (evidence-copy-pairs job)) (abbreviated-path (evidence-copy-folder job)) (tuatara.io:native-file-name log))
        (message "Not all copied: ~a~@[; the log of those copied is ~a~]"
                 (tuatara.runtime:job-ending-words job) (and log (tuatara.io:native-file-name log))))))

(defun copy-from-evidence-paths (buffer)
  "The files \\[copy-from-evidence] copies from BUFFER: the marked ones in a
rimu list, or the one on the line; else the file at point or BUFFER's
\(THING-AT-POINT-PATH)."
  (if (rimu-list-p buffer)
      (rimu-chosen-paths buffer)
      (let ((path (thing-at-point-path buffer)))
        (and path (list path)))))

(define-command copy-from-evidence ()
  "Copy files out of an evidence folder (*EVIDENCE-FOLDERS*) to a folder
asked for, which may not be in one: in a rimu list the marked files, or the
one on the line; elsewhere the file at point or the buffer's file, each of
them in an evidence folder. Each
copy's SHA-256 is checked against its original's, and a log beside the
copies, copied-from-evidence-DATE-TIME.log, says of each what it was, where
from, its size and both sums, and when. Nothing there is written over, and
folders are not copied: their files are. Copied in the background."
  (let* ((buffer (current-buffer))
         (paths (copy-from-evidence-paths buffer)))
    (cond ((null paths) (message "Nothing here to copy out of evidence"))
          ((find-if-not (lambda (path) (eq :regular (tuatara.io:path-kind path))) paths)
           (message "~a is not a file: only files are copied out (mark the files in a folder)"
                    (abbreviated-path (find-if-not (lambda (path) (eq :regular (tuatara.io:path-kind path))) paths))))
          ((find-if-not #'evidence-path-p paths)
           (message "~a is in no evidence folder (*evidence-folders*): nothing copied"
                    (abbreviated-path (find-if-not #'evidence-path-p paths))))
          (t
           (let ((folder (read-native-path (format nil "Copy ~a out of evidence to folder: "
                                                   (rimu-question nil paths)))))
             (cond ((null folder) (message "Nowhere given: nothing copied"))
                   ((not (eq :directory (tuatara.io:path-kind folder)))
                    (message "~a is no folder: nothing copied" (abbreviated-path folder)))
                   ((evidence-refusal "copy into" folder) (message "~a" (evidence-refusal "copy into" folder)))
                   (t (let* ((folder (tuatara.io:slash-ended folder))
                             (pairs (mapcar (lambda (path) (cons path (tuatara.io:path-join folder (tuatara.io:native-file-name path))))
                                            paths))
                             (there (remove-if-not (lambda (pair) (tuatara.io:path-kind (cdr pair) :follow nil)) pairs)))
                        (if there
                            (message "Nothing copied: ~{~a~^, ~} ~:[is~;are~] in ~a already, and nothing is written over"
                                     (mapcar (lambda (pair) (tuatara.io:native-file-name (cdr pair))) there) (rest there)
                                     (abbreviated-path folder))
                            (let ((job (make-evidence-copy pairs folder)))
                              (tuatara.runtime:start-job-thread
                               job (lambda (job)
                                     (copy-from-evidence-work pairs folder (tuatara.runtime:job-cancel job)
                                                              (lambda (log) (setf (evidence-copy-log job) log))))
                               :name "tuatara copying out of evidence")
                              (message "Copying ~:d file~:p out of evidence to ~a..." (length pairs) (abbreviated-path folder))))))))))))

;;; What check-health says

(defparameter *evidence-health-folders* 20000
  "The most folders \\[check-health] looks through for each entry of
*EVIDENCE-FOLDERS*, to say whether it matches any.")

(defun entry-search-root (entry)
  "Where to look for the folders ENTRY matches: its fixed front for a glob,
its :within folder, else NIL (anywhere)."
  (ecase (evidence-entry-kind entry)
    (:glob (let* ((glob (evidence-entry-pattern entry)) (end (glob-fixed-end glob)))
             (if (plusp end) (subseq glob 0 end) "/")))
    ((:name :regex) (evidence-entry-within entry))))

(defun entry-folders-found (entry)
  "The folders on disk ENTRY of *EVIDENCE-FOLDERS* makes evidence, looking
through at most *EVIDENCE-HEALTH-FOLDERS* folders on the drive of where it
starts (ENTRY-SEARCH-ROOT), links not followed; two values, those found and
whether every folder there was looked through. NIL and NIL when there is
nowhere to start from."
  (let ((root (entry-search-root entry)) (found '()))
    (when (and root (eq :directory (tuatara.io:path-kind root)))
      (let ((whole (walk-directory (tuatara.io:slash-ended root)
                                   (lambda (folder relative entry-there)
                                     (declare (ignore relative))
                                     (cond ((null entry-there)
                                            (when (evidence-entry-matches-p entry (string-right-trim "/" folder))
                                              (push folder found))
                                            t)
                                           ;; within an evidence folder, nothing more to find
                                           (t (not (member folder found :test #'string=)))))
                                   :same-device t :limit *evidence-health-folders*)))
        (values (nreverse found) whole)))))

(defun evidence-health-lines ()
  "What \\[check-health] says of *EVIDENCE-FOLDERS*, a line each: none (off),
or each entry with the folders it matches on disk, and those not read."
  (let ((rules (evidence-rules)) (*print-pretty* nil))
    (if (null rules)
        (list "Evidence folders: none (off)")
        (append (list (format nil "Evidence folders: ~:d entr~:@p (*evidence-folders*)" (length (setting-list '*evidence-folders* *evidence-folders*))))
                (loop for entry in (evidence-rules-entries rules)
                      collect (multiple-value-bind (found whole) (entry-folders-found entry)
                                (format nil "  ~s: ~:[matches nothing~;~:*~a~]~:[ (not all looked through)~;~]"
                                        (evidence-entry-given entry)
                                        (and found (format nil "~:d folder~:p, ~{~a~^, ~}~:[~;, …~]" (length found)
                                                           (mapcar #'abbreviated-path (subseq found 0 (min 3 (length found))))
                                                           (> (length found) 3)))
                                        whole)))
                (loop for (given . why) in (evidence-rules-problems rules)
                      collect (format nil "  ~s: not read, left out: ~a" given why))))))

(defun install-evidence ()
  "Have the core ask before every write whether it is in an evidence folder
\(*EVIDENCE-FOLDERS*), the mode line say Evidence for a file in one, and the
buffers of files that become evidence once the setting changes made
read-only."
  (setf tuatara.io:*writing-refusal-function* 'evidence-writing-refusal)
  (pushnew 'evidence-mode-line *mode-line-name-functions*)
  (pushnew 'evidence-tick *tick-functions*))
