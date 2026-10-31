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

(defstruct (evidence-entry (:constructor make-evidence-entry (given kind pattern withins then)) (:copier nil))
  "An entry of *EVIDENCE-FOLDERS* as it is read: GIVEN, the entry as it was
written; KIND, :GLOB, :NAME or :REGEX; PATTERN, the globs (as written, and
with their fixed folders at the front where they really are), the name, or
the scanner; WITHINS, the folder it is held to as written and where it
really is (each with no / at its end), or NIL; THEN, the names of the
folders below a matched one that are the evidence; TARGETS, the real paths
of the folders that links it matches lead to (ENTRY-LINK-TARGETS), learnt in
the background and as they are met; LINKS, how they were looked for. Names
are kept in Unicode's composed form (NFC-NAME)."
  given kind pattern withins then (targets '()) (links nil))

(setf (documentation 'evidence-entry-given 'function) "The entry of *EVIDENCE-FOLDERS* as written."
      (documentation 'evidence-entry-kind 'function) "What kind of entry it is: :GLOB, :NAME or :REGEX."
      (documentation 'evidence-entry-pattern 'function) "The globs, the folder's name or the scanner of the entry."
      (documentation 'evidence-entry-withins 'function) "The folder the entry is held to, as written and where it really is, or NIL."
      (documentation 'evidence-entry-then 'function) "The names of the folders below a matched one that are the evidence."
      (documentation 'evidence-entry-targets 'function) "The real paths of the folders that links the entry matches lead to."
      (documentation 'evidence-entry-links 'function) "How the links the entry matches were looked for: NIL not yet, :WHOLE, :CAPPED (not all of its folders), :ANYWHERE (not looked for: it may match anywhere)."
      (documentation 'evidence-entry-p 'function) "True when OBJECT is an EVIDENCE-ENTRY.")

(defun nfc-name (text)
  "TEXT in Unicode's composed form (NFC), so that a name written é and one
written e and a combining accent compare alike, as some drives store either."
  (if (every (lambda (c) (< (char-code c) 128)) text)
      text
      (sb-unicode:normalize-string text :nfc)))

(defun written-folder-name (path)
  "PATH, a folder's path (~ for the home folder), as a native name from the
root as written, . and .. taken out, with no / at its end (/ for the root)."
  (tuatara.io:clean-path (tuatara.io:native (tuatara.io:expand-path path))))

(defun real-folder-name (path)
  "PATH, a folder's path (~ for the home folder), as a native name from the
root where it really is as far as it is there (REAL-PATH-SO-FAR), every
link followed, its last name too, with no / at its end (/ for the root)."
  (let ((real (tuatara.io:real-path-so-far (tuatara.io:native (tuatara.io:expand-path path)))))
    (if (zerop (length real)) "/" real)))

(defun glob-fixed-end (glob)
  "Where the folders at the front of GLOB that hold none of * ? [ \\ end:
the position of the / after the last of them, or GLOB's length when it holds
none of them at all."
  (let ((special (position-if (lambda (c) (find c "*?[\\")) glob)))
    (if special
        (or (position #\/ glob :end special :from-end t) 0)
        (length glob))))

(defun read-glob-entry (given)
  "GIVEN, a folder's path with * and ** in it, read as an evidence entry, its
glob kept as written and with its fixed front where it really is, the whole
of it followed when it holds no * ? or [ (a link named as the evidence
folder counts as the folder it leads to); an error saying why it cannot be."
  (when (zerop (length (string-trim " " given)))
    (error "an empty path names no folder"))
  (unless (or (char= (char given 0) #\/) (char= (char given 0) #\~))
    (error "~s is not a path from the root or from ~~/" given))
  (let* ((glob (nfc-name (written-folder-name given)))
         (end (glob-fixed-end glob))
         (front (if (plusp end) (string-right-trim "/" (nfc-name (real-folder-name (subseq glob 0 end)))) "")))
    (make-evidence-entry given :glob (remove-duplicates (list glob (concatenate 'string front (subseq glob end))) :test #'string=)
                         nil nil)))

(defun read-folder-within (within)
  "The folder :within names, WITHIN, as written and where it really is; an
error when it is not a path from the root or from ~/."
  (unless (and (stringp within) (plusp (length within)) (find (char within 0) "/~"))
    (error ":within ~s is not a folder's path from the root or from ~~/" within))
  (remove-duplicates (list (nfc-name (written-folder-name within)) (nfc-name (real-folder-name within))) :test #'string=))

(defun read-list-entry (given)
  "GIVEN, a list beginning with :name or :regex, read as an evidence entry; an
error saying why it cannot be. A regular expression matches a folder's
whole name: it is read as \\A(?:RE)\\z."
  (unless (and (keywordp (first given)) (evenp (length given)))
    (error "~s is not :name or :regex followed by keywords and their values" given))
  (loop for (key) on given by #'cddr
        unless (member key '(:name :regex :within :then))
          do (error "~s is none of :name, :regex, :within and :then" key))
  (let ((name (getf given :name)) (regex (getf given :regex))
        (within (getf given :within)) (then (getf given :then)))
    (when (and name regex) (error "an entry has :name or :regex, not both"))
    (let ((withins (and within (read-folder-within within))))
      (cond (name
             (unless (and (stringp name) (plusp (length name)) (not (find #\/ name)))
               (error ":name ~s is not the name of one folder" name))
             (when then (error ":then goes with :regex"))
             (make-evidence-entry given :name (nfc-name name) withins nil))
            (regex
             (unless (stringp regex) (error ":regex ~s is not a string" regex))
             (handler-case (tuatara.syntax:compile-pattern regex :case-fold t)
               (error (e) (error ":regex ~s cannot be used: ~a" regex e)))
             (let ((scanner (tuatara.syntax:compile-pattern (format nil "\\A(?:~a)\\z" regex) :case-fold t))
                   (names (and then
                               (if (and (stringp then) (plusp (length then)) (char/= (char then 0) #\/))
                                   (mapcar #'nfc-name
                                           (remove "" (uiop:split-string (string-right-trim "/" then) :separator "/") :test #'string=))
                                   (error ":then ~s is not a folder's name, or path, inside the one matched" then)))))
               (when (intersection names '("." "..") :test #'string=)
                 (error ":then ~s leads out of the folder matched" then))
               (make-evidence-entry given :regex scanner withins names)))
            (t (error "~s has neither :name nor :regex" given))))))

(defun read-evidence-entry (given)
  "GIVEN, an entry of *EVIDENCE-FOLDERS*, as an EVIDENCE-ENTRY, the links it
matches not looked for yet (EVIDENCE-KEEPER does, in the background); an
error saying why it cannot be read."
  (cond ((stringp given) (read-glob-entry given))
        ((consp given) (read-list-entry given))
        (t (error "~s is neither a folder's path nor a list beginning with :name or :regex" given))))

;;; The setting in force

(defstruct (evidence-rules (:constructor make-evidence-rules (setting copy entries problems)) (:copier nil))
  "*EVIDENCE-FOLDERS* as read: SETTING, its value, and COPY, a copy of it, to
tell when it changes; ENTRIES, those read (EVIDENCE-ENTRY); PROBLEMS, each
\(GIVEN . WHY) for an entry that could not be; FOLDERS, each folder's answer
by its real path (FOLDER-EVIDENCE); HOLDING, each folder's answer of
FOLDER-HOLDS-EVIDENCE-P; LINKED, true once the links its entries match have
been looked for (EVIDENCE-KEEPER); FORGOTTEN, how many times FOLDERS and
HOLDING were emptied (FORGET-KEPT-ANSWERS), so that an answer worked out
before is not kept after."
  setting copy entries problems
  (folders (make-hash-table :test 'equal :synchronized t))
  (holding (make-hash-table :test 'equal :synchronized t))
  (linked nil)
  (forgotten 0))

(setf (documentation 'evidence-rules-setting 'function) "The value of *EVIDENCE-FOLDERS* the rules were read from."
      (documentation 'evidence-rules-copy 'function) "A copy of the value the rules were read from."
      (documentation 'evidence-rules-entries 'function) "The entries read."
      (documentation 'evidence-rules-problems 'function) "The entries not read, each (GIVEN . WHY)."
      (documentation 'evidence-rules-folders 'function) "Each folder's answer so far, by its real path."
      (documentation 'evidence-rules-holding 'function) "Each folder's answer of FOLDER-HOLDS-EVIDENCE-P so far."
      (documentation 'evidence-rules-linked 'function) "True once the links the entries match have been looked for."
      (documentation 'evidence-rules-forgotten 'function) "How many times the answers kept have been dropped."
      (documentation 'evidence-rules-p 'function) "True when OBJECT is an EVIDENCE-RULES.")

(defvar *evidence-rules* nil
  "*EVIDENCE-FOLDERS* as last read (EVIDENCE-RULES), or NIL.")

(defvar *evidence-generation* 0
  "How many times *EVIDENCE-FOLDERS* has been read anew: a buffer's answer
\(BUFFER-EVIDENCE) stands while it is the same.")

(defun evidence-generation ()
  "How many times *EVIDENCE-FOLDERS* has been read anew so far, for answers
kept elsewhere to be looked at again when it changes; NIL while it names no
folder."
  (and (evidence-rules) *evidence-generation*))

(defvar *evidence-lock* (sb-thread:make-mutex :name "tuatara evidence folders")
  "Held while *EVIDENCE-FOLDERS* is read anew.")

(defparameter *mounts-info-file* "/proc/self/mountinfo"
  "Where the drives mounted, and the folder of each shown at each place, are read from.")

(defparameter *mounts-look-interval* 1
  "The seconds between two looks at the drives mounted (*MOUNTS-INFO-FILE*)
while evidence folders are named, in the background: when they changed, the
setting is read again, what is kept of each folder dropped (EVIDENCE-KEEPER).")

(defun read-evidence-rules (value)
  "VALUE, a value of *EVIDENCE-FOLDERS*, read as EVIDENCE-RULES, the links its
entries match not looked for yet: quick, on whichever thread asks."
  (let ((entries '()) (problems '()))
    (dolist (given (setting-list '*evidence-folders* value))
      (handler-case (push (read-evidence-entry given) entries)
        (error (e) (push (cons given (princ-to-string e)) problems))))
    (make-evidence-rules value (copy-tree value) (nreverse entries) (nreverse problems))))

(defun publish-evidence-rules (rules)
  "Put RULES in force: a new generation, every answer kept elsewhere looked at again."
  (incf *evidence-generation*)
  (setf *evidence-rules* rules))

(defun link-evidence-rules (rules)
  "Look for the links the entries of RULES match (ENTRY-LINK-TARGETS), and
once found put them in force: what was kept of each folder dropped, a new
generation begun."
  (dolist (entry (evidence-rules-entries rules))
    (multiple-value-bind (targets how) (entry-link-targets entry)
      (setf (evidence-entry-targets entry) (union targets (evidence-entry-targets entry) :test #'string=)
            (evidence-entry-links entry) how)))
  (forget-kept-answers rules)
  (setf (evidence-rules-linked rules) t)
  (incf *evidence-generation*))

(defun forget-kept-answers (rules)
  "Drop every answer RULES keep, of each folder (FOLDER-EVIDENCE) and of
holding (FOLDER-HOLDS-EVIDENCE-P), counting it, so that an answer worked out
meanwhile is not kept (KEEP-ANSWER)."
  (sb-ext:with-locked-hash-table ((evidence-rules-holding rules))
    (incf (evidence-rules-forgotten rules))
    (clrhash (evidence-rules-folders rules))
    (clrhash (evidence-rules-holding rules))))

(defun keep-answer (rules table key value since)
  "Keep VALUE for KEY in TABLE, one of RULES' tables, unless the answers were
dropped since SINCE, the count of FORGET-KEPT-ANSWERS when it was worked out
from. Returns VALUE."
  (sb-ext:with-locked-hash-table ((evidence-rules-holding rules))
    (when (= since (evidence-rules-forgotten rules))
      (setf (gethash key table) value)))
  value)

(defun evidence-keeper (rules)
  "The work of the thread that keeps RULES, those in force: the links their
entries match looked for, then the drives mounted looked at every
*MOUNTS-LOOK-INTERVAL* seconds; when they change, the rules read again here,
links and all, and put in force in one step, the old ones serving
meanwhile. Ends once other rules are in force, or none, or the setting
names no folder."
  (unless (evidence-rules-linked rules)
    (link-evidence-rules rules))
  (let ((seen (ignore-errors (uiop:read-file-string *mounts-info-file*))))
    (loop
      (sleep (max 1/20 *mounts-look-interval*))
      (unless (and *evidence-folders* (eq *evidence-rules* rules)) (return))
      (let ((now (ignore-errors (uiop:read-file-string *mounts-info-file*))))
        (unless (equal now seen)
          (setf seen now)
          (let ((fresh (read-evidence-rules (evidence-rules-setting rules))))
            (setf (evidence-rules-setting fresh) (evidence-rules-setting rules))
            (link-evidence-rules fresh)
            (sb-thread:with-mutex (*evidence-lock*)
              (unless (eq *evidence-rules* rules) (return))
              (publish-evidence-rules fresh))
            (setf rules fresh)))))))

(defvar *evidence-keeper* nil
  "The thread keeping the rules in force (EVIDENCE-KEEPER), or NIL.")

(defvar *evidence-saving* nil
  "True while the program is being saved: no thread keeping the rules is
started then (STOP-EVIDENCE-KEEPER).")

(defun start-evidence-keeper (rules)
  "Have a thread of its own keep RULES (EVIDENCE-KEEPER); none while the
program is being saved (*EVIDENCE-SAVING*)."
  (unless *evidence-saving*
    (setf *evidence-keeper*
          (tuatara.runtime:make-guarded-thread (lambda () (evidence-keeper rules)) :name "tuatara evidence folders"))))

(defun evidence-rules ()
  "*EVIDENCE-FOLDERS* as read now (EVIDENCE-RULES), read again when it was
changed: quickly, on whichever thread asks, the links its entries match and
the drives mounted then followed in the background (EVIDENCE-KEEPER), which
reads them again when the drives change, and is started again should it
have ended. NIL at once when it names no folder; the rules read before are
then let go of, and the thread keeping them ends."
  (let ((value *evidence-folders*))
    (if (null value)
        (when *evidence-rules*
          (sb-thread:with-mutex (*evidence-lock*)
            (setf *evidence-rules* nil))
          nil)
        (let ((rules *evidence-rules*))
          (if (and rules (eq value (evidence-rules-setting rules)) (equal value (evidence-rules-copy rules)))
              (progn
                (let ((keeper *evidence-keeper*))
                  (unless (and keeper (sb-thread:thread-alive-p keeper))
                    (sb-thread:with-mutex (*evidence-lock*)
                      (let ((keeper *evidence-keeper*))
                        (unless (and keeper (sb-thread:thread-alive-p keeper))
                          (start-evidence-keeper rules))))))
                rules)
              (sb-thread:with-mutex (*evidence-lock*)
                (let ((rules *evidence-rules*))
                  (if (and rules (eq value (evidence-rules-setting rules)) (equal value (evidence-rules-copy rules)))
                      rules
                      (let ((fresh (read-evidence-rules value)))
                        (publish-evidence-rules fresh)
                        (start-evidence-keeper fresh)
                        fresh)))))))))

(defun stop-evidence-keeper ()
  "Let go of the rules in force and wait, a few seconds at most, for the
thread keeping them to end, none started again until the saved program
starts (*EVIDENCE-SAVING*): none runs while the program is saved."
  (setf *evidence-saving* t
        *evidence-rules* nil)
  (let ((keeper *evidence-keeper*))
    (when (and keeper (sb-thread:thread-alive-p keeper))
      (ignore-errors (sb-thread:join-thread keeper :timeout 5 :default nil))))
  (setf *evidence-keeper* nil))

(defun evidence-saved ()
  "In the saved program as it starts: threads keeping the rules may be started again."
  (setf *evidence-saving* nil))

(pushnew 'stop-evidence-keeper sb-ext:*save-hooks*)
(pushnew 'evidence-saved sb-ext:*init-hooks*)

(defun evidence-links-learnt-p ()
  "True once the links the entries of *EVIDENCE-FOLDERS* match have been
looked for (EVIDENCE-KEEPER), or when it names no folder."
  (let ((rules (evidence-rules)))
    (or (null rules) (evidence-rules-linked rules))))

(defun evidence-problems-words ()
  "What is wrong with the entries of *EVIDENCE-FOLDERS*, a sentence each; NIL
when nothing is."
  (let ((rules (evidence-rules)))
    (and rules
         (loop for (given . why) in (evidence-rules-problems rules)
               collect (let ((*print-pretty* nil))
                         (format nil "*evidence-folders*: ~s is left out: ~a" given why))))))

;;; Which folders are evidence

(defun within-folder-p (folder tops)
  "True when FOLDER, a path, is one of TOPS or within one, case disregarded."
  (some (lambda (top) (tuatara.io:path-within-p (string-downcase folder) (string-downcase top))) tops))

(defun folder-names (folder)
  "The names of the folders on the path FOLDER, from the root."
  (remove "" (uiop:split-string folder :separator "/") :test #'string=))

(defun evidence-entry-matches-p (entry folder)
  "True when FOLDER, a folder's path (as written or where it really is) with no
/ at its end, is an evidence folder by ENTRY itself (folders within it are
not looked at here): its path or names as the entry says, or a folder a link
the entry matches leads to."
  (let ((folder (nfc-name folder))
        (withins (evidence-entry-withins entry)))
    (or (member folder (evidence-entry-targets entry) :test #'string-equal)
        (ecase (evidence-entry-kind entry)
          (:glob (some (lambda (glob) (glob-match glob folder :test #'char-equal)) (evidence-entry-pattern entry)))
          (:name (and (string-equal (tuatara.io:native-file-name folder) (evidence-entry-pattern entry))
                      (or (null withins) (within-folder-p folder withins))))
          (:regex (let* ((names (folder-names folder))
                         (then (evidence-entry-then entry))
                         (case (- (length names) (length then) 1)))
                    (and (>= case 0)
                         (every #'string-equal then (nthcdr (1+ case) names))
                         (cl-ppcre:scan (evidence-entry-pattern entry) (nth case names))
                         (or (null withins)
                             (within-folder-p (format nil "/~{~a~^/~}" (subseq names 0 (1+ case))) withins)))))))))

(defparameter *evidence-link-search-folders* 5000
  "The most folders looked through for each entry of *EVIDENCE-FOLDERS*, when
it is read, for links it matches (ENTRY-LINK-TARGETS).")

(defun entry-search-root (entry)
  "Where to look for the folders ENTRY matches: its fixed front for a glob
\(where it really is), its :within folder, else NIL (anywhere)."
  (ecase (evidence-entry-kind entry)
    (:glob (let* ((glob (car (last (evidence-entry-pattern entry)))) (end (glob-fixed-end glob)))
             (cond ((= end (length glob)) (tuatara.io:parent-path glob))
                   ((plusp end) (subseq glob 0 end))
                   (t "/"))))
    ((:name :regex) (car (last (evidence-entry-withins entry))))))

(defun entry-link-targets (entry)
  "The real paths of the folders that links matched by ENTRY lead to: a
link named as an evidence folder (~/cases/2026-0001/evidence leading to a
drive's folder) makes the folder it leads to evidence under its own path
too. Looked for where the entry starts (ENTRY-SEARCH-ROOT), within
*EVIDENCE-LINK-SEARCH-FOLDERS* folders, as deep as a glob without ** can
match; NIL for an entry that may match anywhere. As a second value how they
were looked for: :WHOLE, :CAPPED (the folders there were more) or :ANYWHERE."
  (let ((root (entry-search-root entry)) (found '()) (how (if (entry-search-root entry) :whole :anywhere)))
    (when (and root (eq :directory (tuatara.io:path-kind root)))
      (let* ((glob (and (eq (evidence-entry-kind entry) :glob) (first (evidence-entry-pattern entry))))
             (depth (and glob (not (search "**" glob))
                         (max 0 (- (count #\/ glob) (count #\/ (string-right-trim "/" root)))))))
        (unless (walk-directory (tuatara.io:slash-ended root)
                                (lambda (folder relative listed)
                                  (declare (ignore relative))
                                  (when listed
                                    (destructuring-bind (name kind &optional size own &rest rest) listed
                                      (declare (ignore size rest))
                                      (when (and (eq kind :directory) (eq own :symlink))
                                        (let ((link (concatenate 'string folder name)))
                                          (when (evidence-entry-matches-p entry link)
                                            (let ((real (ignore-errors (tuatara.io:real-path link))))
                                              (when real (pushnew (nfc-name real) found :test #'string=))))))))
                                  t)
                                :max-depth depth :same-device t :limit *evidence-link-search-folders*)
          (setf how :capped))))
    (values found how)))

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
  (let* ((since (evidence-rules-forgotten rules))
         (stamp (ignore-errors (tuatara.io:path-stamp folder)))
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
            (keep-answer rules (evidence-rules-folders rules) folder (cons found key) since))
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

(defun written-evidence-match (rules written)
  "The evidence folder the place WRITTEN (an absolute path as written, . and ..
taken out) is, or is in, by RULES and the names on it alone, the disk not
asked: the deepest folder of it an entry matches, as (EVIDENCE-FOLDER .
ENTRY), or NIL. A link named as an evidence folder is told so however far
it leads."
  (loop for end = (length written) then (position #\/ written :end end :from-end t)
        while (and end (plusp end))
        do (let* ((folder (subseq written 0 end))
                  (entry (find-if (lambda (entry) (evidence-entry-matches-p entry folder)) (evidence-rules-entries rules))))
             (when entry
               (learn-link-target rules entry folder)
               (return (cons folder entry))))))

(defun learn-link-target (rules entry folder)
  "When FOLDER, an evidence folder by ENTRY of RULES, is a link, make the
folder it leads to evidence by ENTRY too from now on, what is kept of each
folder dropped."
  (when (eq :symlink (tuatara.io:path-kind folder :follow nil))
    (let ((real (ignore-errors (tuatara.io:real-path folder))))
      (when (and real (not (member (nfc-name real) (evidence-entry-targets entry) :test #'string-equal)))
        (push (nfc-name real) (evidence-entry-targets entry))
        (forget-kept-answers rules)))))

(defun absolute-native (path)
  "PATH as a native name from the root, as the system takes it: a relative
one from the process's own folder, . and .. taken out."
  (let ((name (tuatara.io:native path)))
    (tuatara.io:clean-path (if (and (plusp (length name)) (char= (char name 0) #\/))
                               name
                               (concatenate 'string (tuatara.io:slash-ended (sb-posix:getcwd)) name)))))

(defun evidence-match (rules real &optional written)
  "The evidence folder the place at REAL (a real path) is, or is in, by RULES,
as (EVIDENCE-FOLDER . ENTRY), or NIL; WRITTEN, the place as written
\(ABSOLUTE-NATIVE), is matched too (WRITTEN-EVIDENCE-MATCH)."
  (let ((*mounts* :unread))
    (or (and written (written-evidence-match rules written))
        (let ((entry (find-if (lambda (entry) (evidence-entry-matches-p entry real)) (evidence-rules-entries rules))))
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
      (let* ((written (absolute-native (tuatara.io:expand-path path)))
             (match (evidence-match rules (place-real-path written) written)))
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
evidence: told by the path as written (a relative one taken from the
process's folder, as the system takes it) and by where the thing open as
FD, or the folder open as DIRFD with PATH's name in it, really is, else by
where PATH leads; NIL when it is not. With PATH NIL, true when
*EVIDENCE-FOLDERS* names any folder. The core asks this before it writes
\(TUATARA.IO:*WRITING-REFUSAL-FUNCTION*)."
  (let ((rules (evidence-rules)))
    (cond ((null rules) nil)
          ((null path) t)
          (t (let* ((written (absolute-native path))
                    (real (or (and fd (tuatara.io:fd-real-path fd))
                              (and dirfd (let ((folder (tuatara.io:fd-real-path dirfd)))
                                           (and folder (concatenate 'string (string-right-trim "/" folder) "/"
                                                                    (tuatara.io:native-file-name written)))))
                              (place-real-path written)))
                    (match (evidence-match rules real written)))
               (and match (evidence-words (car match) (evidence-entry-given (cdr match)))))))))

;;; Folders holding evidence

(defparameter *evidence-holding-search-folders* 5000
  "The most folders looked through to tell whether a folder holds an evidence
folder (FOLDER-HOLDS-EVIDENCE-P) where the entries alone cannot tell: one
with more, not all looked through, counts as holding one.")

(defun glob-components (path)
  "The names of PATH, a glob or a path from the root, in order."
  (remove "" (uiop:split-string path :separator "/") :test #'string=))

(defun glob-holding (glob folder)
  "What the glob GLOB of an entry says of FOLDER (a path from the root, no /
at its end) holding a folder it matches: :YES when a folder it names exactly,
with nothing special in it, is within FOLDER and there; :NO when it can
match nothing within FOLDER; else the folder to look through, the deeper of
FOLDER and the glob's fixed front, and how deep the glob reaches below it
\(NIL for any depth, with **)."
  (let ((end (glob-fixed-end glob)))
    (cond ((= end (length glob))
           (if (and (within-folder-p glob (list folder)) (tuatara.io:path-kind glob)) :yes :no))
          (t (let* ((front (if (plusp end) (subseq glob 0 end) "/"))
                    (pattern (glob-components (subseq glob end)))
                    (deep (if (find "**" pattern :test #'string=) nil (length pattern))))
               (cond ((within-folder-p front (list folder)) (values front deep))
                     ((within-folder-p folder (list front))
                      (let ((below (glob-components (subseq folder (length (string-right-trim "/" front)))))
                            (left pattern))
                        (dolist (name below (if left (values folder (and deep (length left))) :yes))
                          (cond ((null left) (return :yes))
                                ((string= (first left) "**") (return (values folder nil)))
                                ((glob-match (first left) (nfc-name name) :test #'char-equal) (pop left))
                                (t (return :no))))))
                     (t :no)))))))

(defun entry-holding (entry folder)
  "What ENTRY says of FOLDER (a path from the root, no / at its end) holding
an evidence folder: :YES, :NO, or the folders to look through, each (ROOT .
DEPTH), DEPTH NIL for any depth. Told from the entry and the links found for
it alone; the disk is asked only whether a folder it names exactly is there."
  (if (some (lambda (target) (within-folder-p target (list folder))) (evidence-entry-targets entry))
      :yes
      (ecase (evidence-entry-kind entry)
        (:glob (let ((looks '()))
                 (dolist (glob (evidence-entry-pattern entry) (or looks :no))
                   (multiple-value-bind (answer deep) (glob-holding glob folder)
                     (cond ((eq answer :yes) (return :yes))
                           ((stringp answer) (push (cons answer deep) looks)))))))
        ((:name :regex)
         (let ((withins (evidence-entry-withins entry)))
           (if (null withins)
               (list (cons folder nil))
               (or (loop for within in withins
                         when (within-folder-p within (list folder)) collect (cons within nil)
                         when (within-folder-p folder (list within)) collect (cons folder nil))
                   :no)))))))

(defun search-holding (rules looks)
  "Look through LOOKS, each (ROOT . DEPTH), for a folder an entry of RULES
is, or is in, an evidence folder of RULES, by its path, the paths it is
mounted at too and the links learnt (FOLDER-EVIDENCE),
*EVIDENCE-HOLDING-SEARCH-FOLDERS* folders at most in all, links not
followed: the words saying which (EVIDENCE-WORDS), words saying not all
could be looked through, or NIL."
  (let ((left *evidence-holding-search-folders*))
    (dolist (look looks nil)
      (destructuring-bind (root . deep) look
        (when (eq :directory (tuatara.io:path-kind root :follow nil))
          (let ((found nil) (entered 0))
            (unless (walk-directory (tuatara.io:slash-ended root)
                                    (lambda (folder relative listed)
                                      (declare (ignore relative))
                                      (cond (found nil)
                                            ((null listed)
                                             (incf entered)
                                             (let ((match (let ((*mounts* :unread))
                                                            (folder-evidence rules (string-right-trim "/" folder)))))
                                               (when match (setf found match)))
                                             t)
                                            (t t)))
                                    :max-depth deep :limit left)
              (unless found
                (return (format nil "~a has more than ~:d folders to look through for evidence folders (*evidence-holding-search-folders*)"
                                (abbreviated-path root) *evidence-holding-search-folders*))))
            (when found
              (return (evidence-words (car found) (evidence-entry-given (cdr found)))))
            (decf left entered)))))))

(defun rules-settled-p (rules)
  "True when RULES alone tell whether a folder holds an evidence folder: the
links every entry matches have been looked for, all of them."
  (and (evidence-rules-linked rules)
       (every (lambda (entry) (eq (evidence-entry-links entry) :whole)) (evidence-rules-entries rules))))

(defun folder-holds-evidence-p (folder)
  "Words saying which evidence folder FOLDER is, or holds within it, or NIL.
Told from the entries of *EVIDENCE-FOLDERS* and the links found for them
\(ENTRY-HOLDING), for FOLDER by its path, where it really is and the paths it
is mounted at too (FOLDER-ALIASES); the disk looked through (SEARCH-HOLDING)
where an entry may match below one of them, and, unless the entries settle
it (RULES-SETTLED-P: some links not looked for, or not all), FOLDER itself
where it really is; within *EVIDENCE-HOLDING-SEARCH-FOLDERS* folders, more
counting as holding one. Kept for each reading of the setting. On the
editor's thread a look that would take time is made in the background,
FOLDER counting as holding one meanwhile, said so. NIL at once when the
setting names no folder."
  (let ((rules (evidence-rules)))
    (when rules
      (let* ((folder (string-right-trim "/" (tuatara.io:native (tuatara.io:expand-path folder))))
             (folder (if (zerop (length folder)) "/" folder))
             (table (evidence-rules-holding rules))
             (since (evidence-rules-forgotten rules))
             (kept (gethash folder table :unknown)))
        (if (not (eq kept :unknown))
            (if (eq kept :looking) (format nil "~a is being looked through for evidence folders" (abbreviated-path folder)) kept)
            (let* ((real (real-folder-name folder))
                   (places (remove-duplicates (append (list folder real) (let ((*mounts* :unread)) (folder-aliases real)))
                                              :test #'string=))
                   (looks (if (rules-settled-p rules) '() (list (cons real nil))))
                   (yes nil))
              (dolist (place places)
                (dolist (entry (evidence-rules-entries rules))
                  (let ((answer (entry-holding entry place)))
                    (cond ((eq answer :yes) (setf yes (evidence-words place (evidence-entry-given entry))))
                          ((consp answer) (setf looks (append looks answer)))))))
              (cond (yes (keep-answer rules table folder yes since))
                    ((null looks) (keep-answer rules table folder nil since))
                    ((or (null (tuatara.command:running-editor)) (tuatara.command:away-from-editor-p))
                     (keep-answer rules table folder (search-holding rules looks) since))
                    (t (keep-answer rules table folder :looking since)
                       (tuatara.runtime:make-guarded-thread
                        (lambda () (keep-answer rules table folder (search-holding rules looks) since))
                        :name "tuatara evidence holding")
                       (format nil "~a is being looked through for evidence folders" (abbreviated-path folder))))))))))

(defvar *writing-told* (make-hash-table :test 'equal :synchronized t)
  "What SAY-WRITING-ONCE has said so far, each once.")

(defun say-writing-once (words)
  "Say WORDS, something done in writing a file you should know of, in the
echo area and *Messages*, the first time only."
  (unless (gethash words *writing-told*)
    (setf (gethash words *writing-told*) t)
    (message "~a" words)))

;; asked before anything is written, from the moment the defaults are loaded
(setf tuatara.io:*writing-refusal-function* 'evidence-writing-refusal
      tuatara.io:*writing-told-function* 'say-writing-once)

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
                                                   (rimu-question nil paths))
                                       :writing t)))
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
                                (format nil "  ~s: ~:[matches nothing~;~:*~a~]~:[ (not all looked through)~;~]~@[; ~a~]"
                                        (evidence-entry-given entry)
                                        (and found (format nil "~:d folder~:p, ~{~a~^, ~}~:[~;, …~]" (length found)
                                                           (mapcar #'abbreviated-path (subseq found 0 (min 3 (length found))))
                                                           (> (length found) 3)))
                                        whole
                                        (case (evidence-entry-links entry)
                                          ((nil) "the links it matches still being looked for")
                                          (:capped (format nil "links it matches looked for in ~:d folders only (*evidence-link-search-folders*); a folder a link leads to is evidence once the link is written through"
                                                           *evidence-link-search-folders*))
                                          (:anywhere (format nil "it may match anywhere, so links to it are not looked for (a folder a link leads to is evidence once the link is written through), and a repository or project of more than ~:d folders counts as holding an evidence folder: git's checkout, stash, pull and the like are refused there, and its language servers do not start; give it :within"
                                                             *evidence-holding-search-folders*))))))
                (loop for (given . why) in (evidence-rules-problems rules)
                      collect (format nil "  ~s: not read, left out: ~a" given why))))))

(defun install-evidence ()
  "Have the core ask before every write whether it is in an evidence folder
\(*EVIDENCE-FOLDERS*), the mode line say Evidence for a file in one, and the
buffers of files that become evidence once the setting changes made
read-only."
  (setf tuatara.io:*writing-refusal-function* 'evidence-writing-refusal
        tuatara.io:*writing-told-function* 'say-writing-once)
  (pushnew 'evidence-mode-line *mode-line-name-functions*)
  (pushnew 'evidence-tick *tick-functions*))
