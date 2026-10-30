;;;; lisp/rimu/rimu.lisp --- Rimu: a directory listed, and moved through.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x rimu (C-x d) lists a directory in a buffer of its own, laid out as
;;;; Emacs's dired lays it out: the directory's name over its entries, one a
;;;; line, directories first. I shows or hides the details before the names
;;;; (the columns of *RIMU-COLUMNS*), H the entries whose names start with a
;;;; dot, s orders them by name or by time; l or RET opens the entry on the
;;;; line, a directory in the same buffer, and h goes up. A directory is read
;;;; and laid out on a thread of its own, the list saying it is being read
;;;; when that takes a while. The preview (preview.lisp) shows the entry on
;;;; the line in a window beside the list.

(in-package #:tuatara.defaults)

;;; Settings

(defvar *rimu-details* nil
  "True for rimu to show the details of each entry before its name: its inode
number, as ls -i does, then as ls -l does its permissions, links, owner,
group, size and when it last changed, and after the name where a symbolic
link leads. \\<rimu-mode>\\[rimu-toggle-details] shows or hides them in one list;
*RIMU-HIDDEN-COLUMNS* leaves some of them out.")

(defvar *rimu-permissions* :symbolic
  "How rimu's details write each entry's permissions: :SYMBOLIC, as ls -l
does, drwxr-xr-x; :OCTAL, its kind's letter and the number chmod takes,
d755, -644, -4755 with the set-user, set-group or sticky bit.")

(defvar *rimu-hidden-columns* '()
  "The columns rimu's lists never show, however they are asked for: among the
details :INODE, :PERMISSIONS, :LINKS, :OWNER, :GROUP, :SIZE and :TIME, and
the columns of their own :SIZES (directories' sizes) and :GIT (the last
commits). (setf *rimu-hidden-columns* '(:inode :group)) shows the details
without the inode numbers and groups.")

(defvar *rimu-dotfiles* nil
  "True for rimu to show the entries whose names start with a dot, . and ..
among them. \\<rimu-mode>\\[rimu-toggle-dotfiles] shows or hides them in one list.")

(defvar *rimu-directory-sizes* nil
  "True for rimu's lists to show each entry's size before its name, a
directory's with everything under it; NIL, as standard, to show them only on
\\<rimu-mode>\\[rimu-toggle-sizes] in a list.")

(defvar *rimu-git-info* nil
  "True for rimu's lists to show, after each name, the entry's last commit in
git's history: when it was made and the first line of its message. NIL, as standard, to show it only on
\\<rimu-mode>\\[rimu-toggle-git] in a list.")

(defvar *rimu-sort* :name
  "The order rimu lists entries in: :NAME, by name, octet by octet as ls
orders names in the C locale (capitals before small letters); :TIME, those
changed last first. \\<rimu-mode>\\[rimu-toggle-sort] switches between them in one list.")

(defvar *rimu-directories-first* t
  "True for rimu to list directories, and the symbolic links that lead to
directories, before everything else, as ls --group-directories-first does.")

(defvar *rimu-ignored-extensions* '(".o" ".so" ".a" ".fasl" ".elc" ".pyc" ".pyo" ".class" "~" "#")
  "Endings of names rimu shows set back, in the :list-dim face: compiled
files, backups (~) and the files some editors save as you type (#).")

(defvar *rimu-follow-changes* t
  "True for rimu's lists to be read again by themselves when their directory
changes on disk: a file made, deleted, renamed or finished being written by
another program, many at once as one; NIL to read one again only with
\\<rimu-mode>\\[revert-buffer].")

(defvar *rimu-reuse-lists* t
  "True for a folder a bookmark goes to (\\[bookmark-jump]), or a list goes to
across a project's edge, to be listed in a rimu list there is of its project,
moved there, the one used last (for a folder in no project, one in none): a
list of its own only when there is none. NIL to list it in a list of its own.
A list listing that folder already is the one shown either way.")

(defparameter *rimu-read-wait* 0.2
  "Seconds a command waits for a directory to be read and laid out before
showing that it is being read; the list fills in when the reading is done.")

;;; The mode

(define-major-mode rimu-mode special-mode
  "A directory as rimu lists it (\\[rimu]): its name, then a line per entry.
\\<support>\\[next-line] and \\[previous-line] move; \\<rimu-mode>\\[rimu-open] or \\`RET'
opens the entry on the line, a directory in this buffer, a file as
\\[find-file] visits it; \\[rimu-up] goes up to the directory holding this one;
\\[rimu-quick-edit] edits the file in the preview's place, the quick edit, until
\\<*rimu-quick-edit-normal-keymap*>\\[rimu-quick-edit-done] there, or coming back to the list, returns to the preview; \\<rimu-mode>\\[rimu-display] shows the
entry in another window, staying in the list. \\[rimu-goto-file] goes to an
entry by name, \\[rimu-next-directory] and \\[rimu-previous-directory] to the next
and the previous directory. \\[rimu-toggle-details] shows or hides the details,
\\[rimu-toggle-dotfiles] the entries whose names start with a dot;
\\[rimu-toggle-sort] orders by name or by time; \\[list-toggle-preview] turns the
preview on or off, \\[rimu-toggle-picture] switches it between a file's picture and
its text; \\[revert-buffer] reads the directory again; \\[rimu-home] lists your home
directory, \\[rimu-last-directory] the one listed before; \\[quit-window] leaves.
\\[list-mark-entry] marks the entry on the line, \\[list-unmark-entry] unmarks it, \\[list-unmark-all] unmarks all,
\\[list-toggle-marks] turns the marks about, \\[list-mark-for-deletion] marks it for deletion and \\[list-execute] puts
those marked for deletion in the trash; on the marked entries, or the one on
the line, \\[rimu-copy] copies, \\[rimu-move] moves into a directory, \\[rimu-symlink] and \\[rimu-hardlink]
make symbolic and hard links to them, \\[rimu-rename] renames one, \\[list-delete-marked] puts in the trash (with
\\[universal-argument], deletes for good); \\[rimu-make-directory] makes a directory, \\[rimu-touch] an empty file or touches one there,
\\[rimu-make-formatted-file] a file with a formatted name; \\[rimu-edit-names] edits the names as text to rename
them; \\[rimu-trash] lists the trash, to put things back. \\[search-filter-regex]
narrows the list to the entries whose lines match what is typed, as in every
list, the filters kept while it is read again and dropped when another
directory is listed; the commands on the marked entries take those it hides
too, their questions saying how many it hides. \\<support>\\[select-lines] selects lines, \\[select-next-line] and
\\[select-previous-line] taking in the next and the previous line; NORMAL state's
motions by word and by expression and its other selections are not bound
here: \\`w', \\`b', \\`W', \\`B', \\`v' and \\`%' are free."
  :lighter "Rimu" :interactive nil)

(defvar *rimu-keymap* (mode-keymap 'rimu-mode)
  "Keys of rimu's lists (rimu-mode's keymap).")

;;; What a directory holds

(defstruct (rimu-listing (:constructor make-rimu-listing (directory entries free reason &optional found groups)) (:copier nil))
  "A directory as rimu read it: DIRECTORY, its native name ending in /;
ENTRIES, a vector of its entries (TUATARA.IO:LISTED), . and .. among them;
FREE, the octets that may still be written on its file system, or NIL;
REASON, why it could not be read, or NIL; CELLS, the texts of the columns
found so far, by column name, each a vector in the order of ENTRIES, or
:PENDING while they are found in the background. A list of files found
under DIRECTORY (\\[rimu-find], \\[rimu-duplicates]) has FOUND, a line saying
what was found, its entries named by their paths from DIRECTORY, and GROUPS,
for files kept together in the order given, the group of each entry, a
vector, or NIL. GIT-STAMPS are the stamps of the files of git's own that
say which commit is checked out and what is staged, when its git column was
read, for a list read again to keep that column while they are unchanged
\(RIMU-CARRY-CELLS); MARK-KEYS, each entry's key among the marks
\(RIMU-ENTRY-MARK)."
  (directory "" :read-only t) (entries #() :read-only t) (free nil :read-only t) (reason nil :read-only t)
  (found nil :read-only t) (groups nil :read-only t)
  (cells (make-hash-table :test 'eq :synchronized t) :read-only t)
  (git-stamps nil)
  (mark-keys (make-hash-table :test 'eq :synchronized t) :read-only t))

(setf (documentation 'rimu-listing-p 'function) "True when OBJECT is a RIMU-LISTING.")

(defun rimu-read (directory)
  "Read DIRECTORY, a native name ending in /, into a RIMU-LISTING."
  (multiple-value-bind (entries reason) (tuatara.io:list-directory directory :labels t)
    (make-rimu-listing directory (coerce entries 'simple-vector)
                       (and (null reason) (ignore-errors (tuatara.io:free-bytes directory)))
                       reason)))

(defun rimu-kind (entry)
  "The kind of ENTRY (as TUATARA.IO:STAMP-KIND gives it), a symbolic link not
followed; NIL when it could not be examined."
  (let ((stamp (tuatara.io:listed-stamp entry)))
    (and stamp (tuatara.io:stamp-kind stamp))))

(defun rimu-leads-to (entry)
  "The kind of what ENTRY is, a symbolic link followed; NIL for a link that
leads nowhere, and for an entry that could not be examined."
  (let ((kind (rimu-kind entry)))
    (if (eq kind :symlink)
        (let ((target (tuatara.io:listed-target-stamp entry)))
          (and target (tuatara.io:stamp-kind target)))
        kind)))

(defun rimu-directory-p (entry)
  "True when ENTRY is a directory, or a symbolic link leading to one."
  (eq (rimu-leads-to entry) :directory))

(defun rimu-dots-p (entry)
  "True when ENTRY is . or .., the directory itself or the one holding it."
  (let ((octets (tuatara.io:listed-octets entry)))
    (and (<= 1 (length octets) 2) (every (lambda (o) (= o 46)) octets))))

(defun rimu-dotfile-p (entry)
  "True when ENTRY's name starts with a dot."
  (let ((octets (tuatara.io:listed-octets entry)))
    (and (plusp (length octets)) (= (aref octets 0) 46))))

(defun rimu-octets< (a b)
  "True when the octets A come before the octets B, taken octet by octet, a
name coming before the longer names it starts."
  (let ((i (mismatch a b)))
    (and i (or (= i (length a)) (and (< i (length b)) (< (aref a i) (aref b i)))))))

(defun rimu-mtime (entry)
  "When ENTRY last changed, in nanoseconds since 1970; 0 when it could not be examined."
  (let ((stamp (tuatara.io:listed-stamp entry)))
    (if stamp (tuatara.io:stamp-mtime stamp) 0)))

(defun rimu-path (directory entry)
  "The native name of ENTRY of DIRECTORY, a native name ending in /."
  (concatenate 'string directory (tuatara.io:listed-name entry)))

(defun rimu-size-words (octets)
  "OCTETS as a rimu list says a size, in its columns and its first lines: as
narrow as a column wants, in units of 1,024, 4.1K, 12M."
  (tuatara.io:octets-words octets :compact t))

;;; Columns

(defstruct (rimu-column (:constructor make-rimu-column (name cells place align shown documentation)) (:copier nil))
  "A column rimu can show beside the names of entries (SET-RIMU-COLUMN): NAME,
a keyword; CELLS, the function giving its texts; PLACE, :BEFORE or :AFTER the
names; ALIGN, :LEFT or :RIGHT; SHOWN, a function of a list's buffer, true
where it shows; DOCUMENTATION, what it shows."
  name cells place align shown documentation)

(setf (documentation 'rimu-column-p 'function) "True when OBJECT is a RIMU-COLUMN.")

(defvar *rimu-columns* '()
  "The columns rimu can show beside the names of entries, in the order they
show, each a RIMU-COLUMN made by SET-RIMU-COLUMN: the details (inode,
permissions, links, owner, group, size, time) before the names, and any others.")

(defvar *rimu-ready* '()
  "Texts of columns found in the background and given by RIMU-COLUMN-READY,
each (NAME ENTRIES CELLS), for RIMU-TICK to show.")

(defvar *rimu-ready-lock* (sb-thread:make-mutex :name "rimu ready")
  "Held while *RIMU-READY* changes, which threads other than the editor's do.")

(defvar *rimu-editor* nil
  "The editor rimu last ran in, which RIMU-COLUMN-READY wakes.")

(defun set-rimu-column (name cells &key (place :before) (align :left) (shown 'rimu-details-p) documentation)
  "Make NAME, a keyword, a column rimu shows beside the names of entries, in
place of the column of that name, or after the others. CELLS is a function of
a directory (its native name, ending in /) and a vector of its entries
(TUATARA.IO:LISTED), returning a vector of the texts to show beside them in
the same order, each a string, a (STRING . FACE), a list of those, or NIL for
none; or :PENDING when they are found in the background, to be given by
RIMU-COLUMN-READY. PLACE :BEFORE puts the column before the names, among the
details, :AFTER after them; ALIGN :LEFT or :RIGHT lines its texts up on that
side; SHOWN, a function of a list's buffer, is true where the column shows: by
default where the details do (\\<rimu-mode>\\[rimu-toggle-details]). Returns NAME."
  (let ((column (make-rimu-column name cells place align shown documentation))
        (old (find name *rimu-columns* :key #'rimu-column-name)))
    (setf *rimu-columns* (if old (substitute column old *rimu-columns*) (append *rimu-columns* (list column))))
    name))

(defun rimu-column-ready (name entries cells)
  "Give the column NAME the CELLS it found in the background for ENTRIES, the
vector its function was given when it returned :PENDING; the lists showing
those entries show them. Any thread may call it."
  (sb-thread:with-mutex (*rimu-ready-lock*)
    (push (list name entries cells) *rimu-ready*))
  (let ((editor *rimu-editor*))
    (when editor (wake-editor editor)))
  name)

(defun rimu-column-in-background (column entries thread-name cells)
  "Start working out COLUMN (:SIZES, :GIT) for ENTRIES on a thread called
THREAD-NAME, CELLS a function of their names giving the column's texts, a
vector, given to the lists by RIMU-COLUMN-READY; however it ends, the column
is given, empty when CELLS gave nothing, never pending for ever. Returns :PENDING."
  (let ((names (map 'list #'tuatara.io:listed-name entries)))
    (tuatara.runtime:make-guarded-thread
     (lambda ()
       (let ((given nil))
         (unwind-protect
              (progn (rimu-column-ready column entries (funcall cells names))
                     (setf given t))
           (unless given
             (ignore-errors (rimu-column-ready column entries (make-array (length entries) :initial-element nil)))))))
     :name thread-name)
    :pending))

(defun rimu-cells (listing column)
  "The texts of COLUMN for LISTING's entries, a vector in their order, found
now when they were not before; :PENDING while they are found in the
background. A column whose function fails shows nothing."
  (let ((table (rimu-listing-cells listing))
        (name (rimu-column-name column)))
    (or (gethash name table)
        (setf (gethash name table)
              (or (ignore-errors (funcall (rimu-column-cells column)
                                          (rimu-listing-directory listing) (rimu-listing-entries listing)))
                  (make-array (length (rimu-listing-entries listing)) :initial-element nil))))))

(defun rimu-cell-pieces (cell)
  "CELL, a column's text for an entry, as a list of (STRING . FACE)."
  (cond ((null cell) '())
        ((stringp cell) (list (cons cell nil)))
        ((stringp (car cell)) (list cell))
        (t cell)))

(defun rimu-pieces-width (pieces)
  "The columns of the screen PIECES, a list of (STRING . FACE), take."
  (loop for piece in pieces sum (text-columns (car piece))))

(defun rimu-faced-runs (text face-at)
  "TEXT as a list of (STRING . FACE), each the longest run of its characters to
which FACE-AT, a function of a character's index, gives one face."
  (let ((runs '()) (start 0) (end (length text)))
    (loop for i from 1 to end
          when (or (= i end) (not (eq (funcall face-at i) (funcall face-at start))))
            do (push (cons (subseq text start i) (funcall face-at start)) runs)
               (setf start i))
    (nreverse runs)))

(defun rimu-permissions-cell (entry)
  "ENTRY's permissions as *RIMU-PERMISSIONS* says: as ls -l writes them,
drwxr-xr-x, the set-user, set-group and sticky letters (s, S, t, T) in
:rimu-set-id, the group's and others' w in :rimu-perm-write; or in octal,
d755, the digit of those bits in :rimu-set-id. After them . for a security
context (SELinux), + for an access control list; question marks when it
could not be examined."
  (let ((stamp (tuatara.io:listed-stamp entry))
        (octal (eq *rimu-permissions* :octal)))
    (cond ((null stamp) (if octal "????" "??????????"))
          (octal
           (let ((text (format nil "~a~@[~c~]" (mode-octal-text (tuatara.io:stamp-mode stamp)) (tuatara.io:listed-label entry))))
             (rimu-faced-runs text (lambda (i) (and (= i 1) (logtest (tuatara.io:stamp-mode stamp) #o7000) :rimu-set-id)))))
          (t
           (let ((text (format nil "~a~@[~c~]" (first (mode-field (tuatara.io:stamp-mode stamp))) (tuatara.io:listed-label entry))))
             (rimu-faced-runs text (lambda (i)
                                     (let ((c (char text i)))
                                       (cond ((and (member i '(3 6 9)) (find c "sStT")) :rimu-set-id)
                                             ((and (member i '(5 8)) (char= c #\w)) :rimu-perm-write))))))))))

(defun rimu-time-text (mtime now)
  "When an entry last changed, MTIME (nanoseconds since 1970), as ls -l writes
it: Sep 30 11:03 within half a year before NOW (a universal time), else
Sep 30  2025."
  (let ((time (mtime-universal-time mtime)))
    (format-time (if (< (- now 15778476) time (1+ now)) "%b %e %R" "%b %e  %Y") time)))

(defun rimu-cells-by-entry (function)
  "A function giving a column's texts (SET-RIMU-COLUMN) by calling FUNCTION on
each entry, for its text."
  (lambda (directory entries)
    (declare (ignore directory))
    (map 'simple-vector function entries)))

(set-rimu-column :inode (rimu-cells-by-entry (lambda (entry)
                                               (let ((stamp (tuatara.io:listed-stamp entry)))
                                                 (cond ((null stamp) "?")
                                                       ;; none: a folder as a commit left it, not on the disk
                                                       ((zerop (tuatara.io:stamp-ino stamp)) "-")
                                                       (t (princ-to-string (tuatara.io:stamp-ino stamp)))))))
                 :align :right :documentation "The inode number of each entry, as ls -i writes it: which file on its file system it is; - for an entry of a folder as a commit left it.")

(set-rimu-column :permissions (rimu-cells-by-entry 'rimu-permissions-cell)
                 :documentation "The permissions of each entry, as ls -l writes them: drwxr-xr-x.")

(set-rimu-column :links (rimu-cells-by-entry (lambda (entry)
                                               (let ((stamp (tuatara.io:listed-stamp entry)))
                                                 (if stamp (princ-to-string (tuatara.io:stamp-nlink stamp)) "?"))))
                 :align :right :documentation "How many names each entry has, its links.")

(set-rimu-column :owner (rimu-cells-by-entry (lambda (entry)
                                               (let ((stamp (tuatara.io:listed-stamp entry)))
                                                 (if stamp (or (tuatara.io:owner-name (tuatara.io:stamp-uid stamp)) (princ-to-string (tuatara.io:stamp-uid stamp))) "?"))))
                 :documentation "The account that owns each entry.")

(set-rimu-column :group (rimu-cells-by-entry (lambda (entry)
                                               (let ((stamp (tuatara.io:listed-stamp entry)))
                                                 (if stamp (or (tuatara.io:owner-group-name (tuatara.io:stamp-gid stamp)) (princ-to-string (tuatara.io:stamp-gid stamp))) "?"))))
                 :documentation "The group each entry belongs to.")

(set-rimu-column :size (rimu-cells-by-entry (lambda (entry)
                                              (let ((stamp (tuatara.io:listed-stamp entry))
                                                    (device (tuatara.io:listed-device entry)))
                                                (cond ((null stamp) "?")
                                                      (device (format nil "~d, ~d" (car device) (cdr device)))
                                                      (t (rimu-size-words (tuatara.io:stamp-size stamp)))))))
                 :align :right :documentation "The size of each entry as ls -h writes it, 4.1K or 12M; a device's numbers.")

(set-rimu-column :time (lambda (directory entries)
                         (declare (ignore directory))
                         (let ((now (get-universal-time)))
                           (map 'simple-vector (lambda (entry)
                                                 (if (tuatara.io:listed-stamp entry)
                                                     (rimu-time-text (rimu-mtime entry) now)
                                                     "?"))
                                entries)))
                 :documentation "When each entry last changed, as ls -l writes it: Sep 30 11:03, or Sep 30  2025 when older than half a year.")

;;; Laying a directory out

(defstruct (rimu-view (:constructor make-rimu-view (details dotfiles sort directories-first ignored columns marks elsewhere
                                                    taken-out))
                      (:copier nil))
  "How a rimu list lays its directory out, taken as it is asked for: DETAILS,
DOTFILES and SORT as the list has them; DIRECTORIES-FIRST and IGNORED, the
values of *RIMU-DIRECTORIES-FIRST* and *RIMU-IGNORED-EXTENSIONS*; COLUMNS, the
RIMU-COLUMNs it shows; MARKS, a copy of its marks (RIMU-MARKS) of the files of
its directory, or NIL; ELSEWHERE, the full names of the files it has marked
\(>) in other directories (RIMU-MARK-KEYS-ELSEWHERE);
TAKEN-OUT, a copy of the names of the entries taken out of it
\(\\<rimu-mode>\\[rimu-take-out-lines]), or NIL."
  details dotfiles sort directories-first ignored columns marks elsewhere taken-out)

(setf (documentation 'rimu-view-p 'function) "True when OBJECT is a RIMU-VIEW.")

(defstruct (rimu-layout (:constructor make-rimu-layout (text spans lines names &optional table)) (:copier nil))
  "A directory laid out for its list: TEXT; SPANS, a vector of (START END
FACE), octets of TEXT in faces, in order; LINES, the entry on each line, NIL
on the others; NAMES, a table from each entry to the octets its name starts
after the start of its line, so that its name is found on whichever line
shows it, the list narrowed by search filters or not; TABLE, its columns as
KEEP-LIST-TABLE takes them (TITLES TEXTS FIRST), the name among them, or NIL
when it shows the names alone."
  text spans lines names table)

(setf (documentation 'rimu-layout-p 'function) "True when OBJECT is a RIMU-LAYOUT.")

(defun rimu-details-p (buffer)
  "True when the rimu list BUFFER shows the details (\\<rimu-mode>\\[rimu-toggle-details])."
  (tuatara.buffer:buffer-local buffer 'rimu-details))

(defun rimu-marks-here (buffer)
  "A copy of the marks of the rimu list BUFFER (RIMU-MARKS) on files of the
directory it shows (RIMU-MARK-HERE-P), or NIL when there are none."
  (let ((marks (tuatara.buffer:buffer-local buffer 'rimu-marks))
        (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
        (found (rimu-found-p buffer))
        (here nil))
    (when (and marks (plusp (hash-table-count marks)))
      (maphash (lambda (key mark)
                 (when (rimu-mark-here-p key directory found)
                   (setf (gethash key (or here (setf here (make-hash-table :test 'equal)))) mark)))
               marks))
    here))

(defun rimu-mark-keys-elsewhere (buffer mark)
  "The full names of the files of the rimu list BUFFER with MARK in other
directories than the one it shows, the files not looked at on the disk."
  (let ((marks (tuatara.buffer:buffer-local buffer 'rimu-marks))
        (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
        (found (rimu-found-p buffer))
        (keys '()))
    (when marks
      (maphash (lambda (key value)
                 (when (and (eql value mark) (not (rimu-mark-here-p key directory found)))
                   (push key keys)))
               marks))
    keys))

(defun rimu-mark-still-there-p (key &optional files-only)
  "True when the file KEY names, one marked (RIMU-MARK-KEY), is still there;
with FILES-ONLY, only when it is a regular file, or a link to one."
  (if files-only
      (eq (tuatara.io:path-kind key) :regular)
      (tuatara.io:path-kind key :follow nil)))

(defun rimu-view-of (buffer)
  "How the rimu list BUFFER lays its directory out now, a RIMU-VIEW."
  (make-rimu-view (tuatara.buffer:buffer-local buffer 'rimu-details)
                  (tuatara.buffer:buffer-local buffer 'rimu-dotfiles)
                  (tuatara.buffer:buffer-local buffer 'rimu-sort)
                  *rimu-directories-first* (setting-list '*rimu-ignored-extensions* *rimu-ignored-extensions*)
                  (let ((hidden (setting-list '*rimu-hidden-columns* *rimu-hidden-columns*)))
                    (remove-if-not (lambda (column) (and (not (member (rimu-column-name column) hidden))
                                                         (funcall (rimu-column-shown column) buffer)))
                                   *rimu-columns*))
                  (rimu-marks-here buffer)
                  (rimu-mark-keys-elsewhere buffer #\>)
                  (let ((out (tuatara.buffer:buffer-local buffer 'rimu-taken-out)))
                    (and out (plusp (hash-table-count out)) (alexandria:copy-hash-table out)))))

(defun rimu-shown (listing view)
  "The positions among LISTING's entries of those VIEW shows, in the order it
shows them: . and .. first, when names starting with a dot are shown; then by
name or by time, the directories first when VIEW says so; files found in
groups (RIMU-LISTING-GROUPS) in the order given. Those taken out of the list
are left out."
  (let* ((entries (rimu-listing-entries listing))
         (dotfiles (rimu-view-dotfiles view))
         (out (rimu-view-taken-out view))
         (all (loop for i below (length entries) collect i))
         (dots (and dotfiles
                    (sort (remove-if-not (lambda (i) (rimu-dots-p (aref entries i))) all) #'<
                          :key (lambda (i) (length (tuatara.io:listed-octets (aref entries i)))))))
         (rest (remove-if (lambda (i)
                            (let ((entry (aref entries i)))
                              (or (rimu-dots-p entry) (and (not dotfiles) (rimu-dotfile-p entry))
                                  (and out (gethash (tuatara.io:listed-octets entry) out)))))
                          all))
         (sorted (cond ((rimu-listing-groups listing) rest)
                       ((eq (rimu-view-sort view) :time)
                        (sort rest (lambda (a b)
                                     (let ((ta (rimu-mtime (aref entries a))) (tb (rimu-mtime (aref entries b))))
                                       (if (= ta tb)
                                           (rimu-octets< (tuatara.io:listed-octets (aref entries a))
                                                         (tuatara.io:listed-octets (aref entries b)))
                                           (> ta tb))))))
                       (t (sort rest (lambda (a b)
                                       (rimu-octets< (tuatara.io:listed-octets (aref entries a))
                                                     (tuatara.io:listed-octets (aref entries b)))))))))
    (append dots
            (if (and (rimu-view-directories-first view) (not (rimu-listing-groups listing)))
                (append (remove-if-not (lambda (i) (rimu-directory-p (aref entries i))) sorted)
                        (remove-if (lambda (i) (rimu-directory-p (aref entries i))) sorted))
                sorted))))

(defun rimu-name-face (entry ignored)
  "The face of ENTRY's name: that of a directory, a symbolic link (one leading
nowhere its own), a named pipe, socket or device; a file whose name ends with
one of IGNORED dimmed."
  (case (rimu-kind entry)
    (:directory :list-directory)
    (:symlink (if (tuatara.io:listed-target-stamp entry) :rimu-symlink :rimu-broken-symlink))
    ((:fifo :socket :character-device :block-device) :rimu-special)
    (:regular (let ((name (tuatara.io:listed-name entry)))
                (and (some (lambda (ending)
                             (and (> (length name) (length ending))
                                  (string= ending name :start2 (- (length name) (length ending)))))
                           ignored)
                     :list-dim)))))

(defun rimu-header-name (directory)
  "DIRECTORY, a native name ending in /, as the first line of its list shows
it: without the / at its end, but for /, each control character in it written
as the lists write one (ESCAPE-CONTROLS), so that it takes one line."
  (escape-controls (if (string= directory "/") "/" (string-right-trim "/" directory))))

(defun rimu-name-part (entry details)
  "The text of ENTRY's name as its line shows it, with DETAILS where a symbolic
link leads, and the name alone as a second value."
  (let ((name (escape-controls (tuatara.io:listed-name entry))))
    (values (if (and details (tuatara.io:listed-target entry))
                (concatenate 'string name " -> " (escape-controls (tuatara.io:listed-target entry)))
                name)
            name)))

(defun rimu-lay-out (listing view)
  "LISTING laid out as VIEW says, a RIMU-LAYOUT: the directory's name; with
the details, the octets its entries shown take on the disk and those still
free; why it could not be read, if it could not; then a line per entry shown:
two spaces, for marks, the columns before the name lined up, the name, where a
symbolic link leads (with the details), and the columns after the name lined
up after the longest."
  (let* ((entries (rimu-listing-entries listing))
         (shown (rimu-shown listing view))
         (columns (rimu-view-columns view))
         (cells (mapcar (lambda (column) (rimu-cells listing column)) columns))
         (widths (loop for vector in cells
                       collect (if (vectorp vector)
                                   (loop for i in shown maximize (rimu-pieces-width (rimu-cell-pieces (aref vector i))))
                                   0)))
         (last-after (position :after columns :key #'rimu-column-place :from-end t))
         (name-width (if last-after
                         (loop for i in shown maximize (text-columns (rimu-name-part (aref entries i) (rimu-view-details view))))
                         0))
         (out (make-string-output-stream))
         (names (make-hash-table :test 'eq))
         ;; the columns in the order the line shows them, NIL for the name
         (ordered (and columns
                       (append (loop for column in columns for vector in cells
                                     when (eq (rimu-column-place column) :before) collect (cons column vector))
                               (list nil)
                               (loop for column in columns for vector in cells
                                     when (eq (rimu-column-place column) :after) collect (cons column vector)))))
         (texts (and ordered (make-hash-table :test 'eq)))
         (first-texts nil)
         (octets 0) (spans '()) (lines '()) (last-group nil))
    (labels ((put (text &optional face)
               (let ((n (tuatara.text:string-octet-length text)))
                 (when (and face (plusp n)) (push (list octets (+ octets n) face) spans))
                 (write-string text out)
                 (incf octets n)))
             (pad (n) (when (plusp n) (put (make-string n :initial-element #\Space))))
             (put-cell (pieces width align)
               (let ((room (- width (rimu-pieces-width pieces))))
                 (when (eq align :right) (pad room))
                 (dolist (piece pieces) (put (car piece) (cdr piece)))
                 (when (eq align :left) (pad room))))
             (end-line (entry)
               (put (string #\Newline))
               (push entry lines)))
      (put "  ")
      (put (rimu-header-name (rimu-listing-directory listing)) :heading)
      (put ":")
      (end-line nil)
      ;; marks kept in other directories, which the operations take too
      (let ((elsewhere (count-if #'rimu-mark-still-there-p (rimu-view-elsewhere view))))
        (when (plusp elsewhere)
          (put (format nil "  ~d marked in other directories, taken by the operations too (U takes every mark away)" elsewhere)
               :list-marked)
          (end-line nil)))
      (when (rimu-listing-found listing)
        (put "  ")
        (put (escape-controls (rimu-listing-found listing)))
        (end-line nil))
      (when (rimu-view-details view)
        (let ((free (rimu-listing-free listing)))
          (put (format nil "  total used in directory ~a available ~a"
                       (rimu-size-words (loop for i in shown sum (tuatara.io:listed-used (aref entries i))))
                       (if free (rimu-size-words free) "unknown"))))
        (end-line nil))
      (when (rimu-listing-reason listing)
        (put "  ")
        (put (rimu-listing-reason listing) :error)
        (end-line nil))
      (dolist (i shown)
        ;; a line apart between groups
        (let ((groups (rimu-listing-groups listing)))
          (when groups
            (unless (eql (aref groups i) last-group)
              (when last-group (end-line nil))
              (setf last-group (aref groups i)))))
        (let* ((entry (aref entries i))
               (line-start octets)
               (mark (and (rimu-view-marks view)
                          (gethash (rimu-mark-key (rimu-listing-directory listing) entry) (rimu-view-marks view)))))
          ;; the first column for its mark; its colour and its name's come from the marks as
          ;; they are when shown (mode-syntax-highlights), so that unmarking shows at once
          (put (string (or mark #\Space)))
          (put " ")
          (loop for column in columns
                for vector in cells
                for width in widths
                when (eq (rimu-column-place column) :before)
                  do (put-cell (rimu-cell-pieces (and (vectorp vector) (aref vector i))) width (rimu-column-align column))
                     (put " "))
          (let ((name octets))
            (setf (gethash entry names) (- name line-start))
            (multiple-value-bind (part alone) (rimu-name-part entry (rimu-view-details view))
              (put alone (rimu-name-face entry (rimu-view-ignored view)))
              (when (> (length part) (length alone))
                (put " -> ")
                (put (subseq part (+ (length alone) 4))
                     (case (rimu-leads-to entry) ((nil) :rimu-broken-symlink) (:directory :list-directory))))
              ;; the columns after the name, those empty at the end of the line left out
              (let ((after (loop for column in columns
                                 for vector in cells
                                 for width in widths
                                 for k from 0
                                 when (eq (rimu-column-place column) :after)
                                   collect (list (rimu-cell-pieces (and (vectorp vector) (aref vector i)))
                                                 (if (eql k last-after) 0 width) (rimu-column-align column)))))
                (loop while (and after (null (first (car (last after))))) do (setf after (butlast after)))
                (when after
                  (pad (- name-width (text-columns part)))
                  (loop for (pieces width align) in after
                        for k from 1
                        do (put "  ")
                           (put-cell pieces (if (= k (length after)) 0 width) align)))))
            (when texts
              (let ((row (map 'simple-vector
                              (lambda (place)
                                (if place
                                    (let ((vector (cdr place)))
                                      (format nil "~{~a~}" (mapcar #'car (rimu-cell-pieces (and (vectorp vector) (aref vector i))))))
                                    (escape-controls (tuatara.io:listed-name entry))))
                              ordered)))
                (setf (gethash entry texts) row)
                (unless first-texts (setf first-texts row))))
            (end-line entry)))))
    (make-rimu-layout (get-output-stream-string out)
                      (coerce (nreverse spans) 'simple-vector)
                      (coerce (nreverse lines) 'simple-vector)
                      names
                      (and texts
                           (list (map 'simple-vector
                                      (lambda (place) (if place (string-downcase (rimu-column-name (car place))) "name"))
                                      ordered)
                                 texts first-texts)))))

(defun rimu-reading-layout (directory &optional (doing "Reading..."))
  "The layout of a list whose DIRECTORY is being read: its name, and DOING."
  (let ((name (rimu-header-name directory)))
    (make-rimu-layout (format nil "  ~a:~%  ~a~%" name doing)
                      (vector (list 2 (+ 2 (tuatara.text:string-octet-length name)) :heading))
                      (vector nil nil) (make-hash-table :test 'eq))))

;;; A list's buffer

(defun rimu-list-p (buffer)
  "True when BUFFER is a rimu list."
  (typep (buffer-major-mode buffer) 'rimu-mode))

(defun rimu-rewound-p (buffer)
  "True when BUFFER is a rimu list of a folder as a commit left it
\(\\[git-rewind]): (TOP . HASH), the top of its repository and the commit."
  (and (rimu-list-p buffer) (tuatara.buffer:buffer-local buffer 'rimu-rewound)))

(defun rimu-column-allowed-p (buffer key)
  "True when the rimu list BUFFER has the column KEY (its buffer-local
setting) turned on, and may show it: never in a list opened safely, as
working the column out reads the disk below, nor in one of a folder as a
commit left it, whose files are not on the disk."
  (and (tuatara.buffer:buffer-local buffer key) (not (tuatara.io:safe-visit-p buffer)) (not (rimu-rewound-p buffer))))

(defun rimu-list-directory (buffer)
  "The directory the rimu list BUFFER shows, a native name ending in /; for a
list of files found, the one they are all under."
  (tuatara.buffer:buffer-local buffer 'rimu-directory))

(defun rimu-buffer-name (directory)
  "The name of a rimu list of DIRECTORY, a native name ending in /: the last
part of it, with its /."
  (if (string= directory "/")
      "/"
      (concatenate 'string (tuatara.io:native-file-name directory) "/")))

(defun rimu-set-up (buffer directory &optional like)
  "Make BUFFER, in rimu-mode, a list of DIRECTORY (a native name ending in /),
showing what LIKE, another list, shows (details, names starting with a dot,
order, and opened safely when it is), or else what the settings say."
  (flet ((from (key setting)
           (if like (tuatara.buffer:buffer-local like key) setting)))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-directory) directory
          (tuatara.buffer:buffer-local buffer :directory) directory
          (tuatara.buffer:buffer-local buffer 'rimu-listing) nil
          (tuatara.buffer:buffer-local buffer 'rimu-generation) 0
          (tuatara.buffer:buffer-local buffer 'rimu-details) (from 'rimu-details *rimu-details*)
          (tuatara.buffer:buffer-local buffer 'rimu-dotfiles) (from 'rimu-dotfiles *rimu-dotfiles*)
          (tuatara.buffer:buffer-local buffer 'rimu-git) (from 'rimu-git *rimu-git-info*)
          (tuatara.buffer:buffer-local buffer 'rimu-sizes) (from 'rimu-sizes *rimu-directory-sizes*)
          (tuatara.buffer:buffer-local buffer 'rimu-sort) (from 'rimu-sort *rimu-sort*)
          (tuatara.buffer:buffer-local buffer :list-preview) *rimu-preview*
          (tuatara.buffer:buffer-local buffer :read-only) t)
    ;; a list made from one opened safely, a preview's, is opened safely too
    (when (and like (tuatara.io:safe-visit-p like))
      (setf (tuatara.buffer:buffer-local buffer :safe-visit) t))
    ;; and from one of a commit, a preview's, lists that commit too
    (when (and like (rimu-rewound-p like))
      (setf (tuatara.buffer:buffer-local buffer 'rimu-rewound) (rimu-rewound-p like)
            (tuatara.buffer:buffer-local buffer 'git-top) (car (rimu-rewound-p like))))))

(defun rimu-make-list (directory &key name like)
  "A new rimu list of DIRECTORY (a native name ending in /), called NAME or
after the directory, showing what LIKE, another list, shows."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name (or name (rimu-buffer-name directory))))))
    (set-major-mode buffer 'rimu-mode)
    (rimu-set-up buffer directory like)
    buffer))

(defun rimu-entry-at (buffer position)
  "The entry on the line of the rimu list BUFFER holding POSITION, or NIL on
the lines that show none."
  (list-entry-at buffer position))

(defun rimu-line-entry (buffer line)
  "The entry LINE of the rimu list BUFFER shows, or NIL for a line that shows
none: the entries are those the list shows, the search filters narrowing it
or not (LIST-ENTRY-AT)."
  (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries #()))
        (index (- line (tuatara.buffer:buffer-local buffer 'list-first-line 0))))
    (and (< -1 index (length entries)) (svref entries index))))

(defun rimu-shown-entries (buffer)
  "The entries the rimu list BUFFER shows, in their order, those a search
filter hides (\\[search-filter-regex]) left out, with the line each is on as a
second value, a vector of them."
  (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries #()))
        (first (tuatara.buffer:buffer-local buffer 'list-first-line 0))
        (shown '()) (lines '()))
    (loop for entry across entries
          for line from first
          when entry do (push entry shown) (push line lines))
    (values (nreverse shown) (coerce (nreverse lines) 'vector))))

(defun rimu-entry-at-point ()
  "The entry on the current line of the current rimu list, or NIL."
  (rimu-entry-at (current-buffer) (point)))

(defun rimu-line-position (buffer line)
  "Where point goes on LINE of the rimu list BUFFER: the start of the entry's
name, else the line's start."
  (let* ((layout (tuatara.buffer:buffer-local buffer 'rimu-layout))
         (entry (rimu-line-entry buffer line))
         (start (tuatara.buffer:buffer-goto-line buffer line)))
    (+ start (or (and entry layout (gethash entry (rimu-layout-names layout))) 0))))

(defun rimu-entry-line (buffer octets)
  "The line of the rimu list BUFFER showing the entry whose name is the octets
OCTETS, else the line of its first entry shown, else 0."
  (let ((entries (tuatara.buffer:buffer-local buffer 'list-entries #()))
        (first (tuatara.buffer:buffer-local buffer 'list-first-line 0)))
    (+ first
       (or (and octets (position-if (lambda (entry) (and entry (equalp (tuatara.io:listed-octets entry) octets))) entries))
           (position-if #'identity entries)
           (- first)))))

(defun rimu-entry-mark-key (buffer)
  "A function giving what the mark of an entry of the rimu list BUFFER is kept
by (ENTRY-MARK-KEY): its full name (RIMU-MARK-KEY); NIL for . and .., which
take none."
  (lambda (entry)
    (and (rimu-markable-p entry) (rimu-mark-key (tuatara.buffer:buffer-local buffer 'rimu-directory) entry))))

(defun rimu-show-layout (buffer layout point-on &key fresh)
  "Make the rimu list BUFFER show LAYOUT, point on the name of the entry whose
name is the octets POINT-ON when it is shown, else on the first entry; each
other window showing the list keeps to the entry it was on, and so does
each window that showed it before and shows it again when it comes back to
it (TUATARA.DISPLAY:WINDOW-HISTORY). FRESH when the
list shows another directory than before: its windows show it from its start,
on the entry point goes to. A preview beside it follows the entry now at point,
which may be another after the entries changed. The list keeps its entries as
every list does (SHOW-LIST-TEXT), so that search filters
\(\\[search-filter-regex]) narrow it to the entries whose lines match, LAYOUT
kept whole and narrowed again each time it is laid out."
  (let* ((selected (and *editor* (editor-window *editor*)))
         (others (and *editor*
                      (loop for window in (editor-windows)
                            when (and (eq (tuatara.display:window-buffer window) buffer) (not (eq window selected)))
                              collect (cons window (and (not fresh)
                                                        (let ((entry (rimu-entry-at buffer (tuatara.display:window-point window))))
                                                          (and entry (tuatara.io:listed-octets entry))))))))
         (version (tuatara.buffer:buffer-version buffer))
         (remembered (and *editor* (not fresh)
                          (loop for window in (editor-windows)
                                nconc (loop for entry in (tuatara.display:window-history window)
                                            when (eq (first entry) buffer)
                                              collect (list window
                                                            (let ((was (rimu-entry-at buffer (tuatara.text:anchor-position (third entry) version))))
                                                              (and was (tuatara.io:listed-octets was)))))))))
    (show-list-text buffer (rimu-layout-text layout) (rimu-layout-spans layout) '()
                    :entries (rimu-layout-lines layout) :point :start :refill 'rimu-refill
                    :mark-key (rimu-entry-mark-key buffer))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-layout) layout)
    (apply #'keep-list-table buffer (or (rimu-layout-table layout) (list nil nil nil)))
    (setf (tuatara.buffer:buffer-point buffer) (rimu-line-position buffer (rimu-entry-line buffer point-on)))
    (loop for (window . octets) in others
          do (setf (tuatara.display:window-point-anchor window)
                   (tuatara.text:make-anchor (tuatara.buffer:buffer-version buffer)
                                             (if (or fresh (null octets))
                                                 (tuatara.buffer:buffer-point buffer)
                                                 (rimu-line-position buffer (rimu-entry-line buffer octets)))
                                             :bias :after)))
    (loop with version = (tuatara.buffer:buffer-version buffer)
          for (window octets) in remembered
          for at = (rimu-line-position buffer (rimu-entry-line buffer octets))
          do (setf (tuatara.display:window-history window)
                   (substitute (list buffer
                                     (tuatara.text:make-anchor version (tuatara.buffer:buffer-line-start buffer at) :bias :before)
                                     (tuatara.text:make-anchor version at :bias :after))
                               buffer (tuatara.display:window-history window) :key #'first :count 1)))
    (when (and fresh *editor*)
      (dolist (window (buffer-windows buffer))
        (setf (tuatara.display:window-start window) 0)))
    (when (and *editor* selected (eq (tuatara.display:window-buffer selected) buffer))
      (rimu-preview-follow *editor*))))

;;; Reading in the background

(defstruct (rimu-job (:constructor make-rimu-job (generation point-on fresh)) (:copier nil))
  "The reading and laying out of a rimu list's directory on THREAD:
GENERATION, which of the list's jobs it is, only its last being shown;
POINT-ON, the octets of the name point goes to, or NIL for the entry point is
on when it is shown; FRESH, true when the list shows another directory than
before; LISTING and LAYOUT once done, or the REASON it failed; DONE true
then."
  generation point-on fresh thread listing layout reason done)

(setf (documentation 'rimu-job-p 'function) "True when OBJECT is a RIMU-JOB.")

(defvar *rimu-waiting* '()
  "The rimu lists whose directory is read or laid out in the background, for
RIMU-TICK to show when that is done.")

(defun rimu-update (buffer &key reread point-on fresh (wait t))
  "Lay the rimu list BUFFER's directory out again from what was read, reading
it afresh first with REREAD or when nothing was, on a thread of its own; point
goes to the entry whose name is the octets POINT-ON, else stays on the entry it
is on when the list is shown. FRESH when the list shows another directory
than before. With WAIT, for what is typed, waits *RIMU-READ-WAIT* seconds for it; after that, and at
once without WAIT, the list goes on to be shown by RIMU-TICK, saying
meanwhile, when FRESH, that the directory is being read.
In a list of files found, reading again looks at those files again
\(RIMU-FOUND-AGAIN); nothing is done while they are being found."
  (let* ((directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (old (tuatara.buffer:buffer-local buffer 'rimu-listing))
         (listing (and (not reread) old))
         (found (rimu-found-p buffer))
         (rewound (rimu-rewound-p buffer))
         (view (rimu-view-of buffer))
         (job (make-rimu-job (setf (tuatara.buffer:buffer-local buffer 'rimu-generation)
                                   (1+ (tuatara.buffer:buffer-local buffer 'rimu-generation 0)))
                             point-on fresh))
         (editor *editor*))
    (when (and found (null old))
      (return-from rimu-update buffer))
    (setf *rimu-editor* editor
          (rimu-job-thread job)
          (tuatara.runtime:make-guarded-thread
           (tuatara.runtime:carry-settings
            '(*rimu-permissions*)
            (lambda ()
             (unwind-protect
                  (handler-case
                      (let ((listing (or listing (cond (found (rimu-found-again old))
                                                       (rewound (rimu-read-at-commit (car rewound) (cdr rewound) directory))
                                                       (t (rimu-carry-cells old (rimu-read directory) view))))))
                        (setf (rimu-job-layout job) (rimu-lay-out listing view)
                              (rimu-job-listing job) listing))
                    (serious-condition (e) (setf (rimu-job-reason job) (princ-to-string e))))
               (setf (rimu-job-done job) t)
               (when editor (wake-editor editor)))))
           :name "tuatara rimu"))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-job) job)
    (when (and wait (plusp *rimu-read-wait*))
      (sb-thread:join-thread (rimu-job-thread job) :timeout *rimu-read-wait* :default nil))
    (if (rimu-job-done job)
        (rimu-take-in buffer job)
        (progn
          (pushnew buffer *rimu-waiting*)
          (when fresh
            (rimu-show-layout buffer (rimu-reading-layout directory) nil :fresh t))))
    buffer))

(defun rimu-carry-cells (old new view)
  "NEW, a listing of the directory OLD listed read again, with what of OLD's
columns still holds for it, in VIEW: the git column kept whole while the
same names are listed and git's own files say the same commit is checked
out and the same staged (GIT-STAMPS); the sizes, worked out again in the
background, showing the old ones of the same names meanwhile. Returns NEW."
  (let* ((columns (mapcar #'rimu-column-name (rimu-view-columns view)))
         (git (and (member :git columns)
                   (let ((top (ignore-errors (repository-top (rimu-listing-directory new)))))
                     (and top (let ((stamps (git-stamps (concatenate 'string top ".git/"))))
                                (and (some #'identity stamps) stamps)))))))
    (setf (rimu-listing-git-stamps new) git)
    (when (and old (equal (rimu-listing-directory old) (rimu-listing-directory new)))
      (let ((old-cells (rimu-listing-cells old)) (new-cells (rimu-listing-cells new))
            (old-entries (rimu-listing-entries old)) (new-entries (rimu-listing-entries new)))
        (let ((cells (gethash :git old-cells)))
          (when (and git (vectorp cells) (equalp git (rimu-listing-git-stamps old))
                     (= (length old-entries) (length new-entries))
                     (every (lambda (a b) (equalp (tuatara.io:listed-octets a) (tuatara.io:listed-octets b))) old-entries new-entries))
            (setf (gethash :git new-cells) cells)))
        (let ((cells (gethash :sizes old-cells))
              (column (find :sizes (rimu-view-columns view) :key #'rimu-column-name)))
          (when (and column (vectorp cells))
            (let ((by-name (make-hash-table :test 'equalp)))
              (loop for entry across old-entries
                    for cell across cells
                    do (setf (gethash (tuatara.io:listed-octets entry) by-name) cell))
              ;; the walk begun again; the old sizes shown until it gives the new
              (rimu-cells new column)
              (setf (gethash :sizes new-cells)
                    (map 'simple-vector (lambda (entry) (gethash (tuatara.io:listed-octets entry) by-name)) new-entries)))))))
    new))

(defun rimu-take-in (buffer job)
  "Show in the rimu list BUFFER what JOB read and laid out, unless a later job
took its place or the buffer was killed meanwhile."
  (when (and (live-buffer-p buffer)
             (eql (rimu-job-generation job) (tuatara.buffer:buffer-local buffer 'rimu-generation)))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-job) nil)
    (if (rimu-job-reason job)
        (message "Could not list ~a: ~a" (tuatara.buffer:buffer-local buffer 'rimu-directory) (rimu-job-reason job))
        (progn
          (setf (tuatara.buffer:buffer-local buffer 'rimu-listing) (rimu-job-listing job))
          (rimu-show-layout buffer (rimu-job-layout job)
                            (or (rimu-job-point-on job)
                                (and (not (rimu-job-fresh job))
                                     (let ((entry (rimu-entry-at buffer (tuatara.buffer:buffer-point buffer))))
                                       (and entry (tuatara.io:listed-octets entry)))))
                            :fresh (rimu-job-fresh job))
          (when (rimu-rewound-p buffer)
            (rimu-rewound-taken-in buffer))))))

(defun rimu-show-ready ()
  "Show the texts of columns found in the background (RIMU-COLUMN-READY) in
the lists showing the entries they are for."
  (let ((ready (sb-thread:with-mutex (*rimu-ready-lock*) (shiftf *rimu-ready* '())))
        (unshown '()))
    (loop for item in (reverse ready)
          for (name entries cells) = item
          do (unless (plusp (loop for buffer in tuatara.buffer:*buffers*
                                  for listing = (and (rimu-list-p buffer) (tuatara.buffer:buffer-local buffer 'rimu-listing))
                                  when (and listing (eq (rimu-listing-entries listing) entries))
                                    do (setf (gethash name (rimu-listing-cells listing)) cells)
                                       (rimu-update buffer :wait nil)
                                    and count t))
               (push item unshown)))
    ;; for a list read in the background not shown yet: kept for when it is
    (when (and unshown *rimu-waiting*)
      (sb-thread:with-mutex (*rimu-ready-lock*)
        (setf *rimu-ready* (append *rimu-ready* unshown))))))

(defun rimu-pending-p ()
  "True when a column of a list shown is being found in the background."
  (some (lambda (buffer)
          (let ((listing (and (rimu-list-p buffer) (tuatara.buffer:buffer-local buffer 'rimu-listing))))
            (and listing
                 (loop for cells being the hash-values of (rimu-listing-cells listing)
                       thereis (eq cells :pending)))))
        tuatara.buffer:*buffers*))

(defun rimu-watched-directories ()
  "The directories of the lists shown, to be watched on disk while
*RIMU-FOLLOW-CHANGES* is true."
  (and *rimu-follow-changes*
       (loop for buffer in tuatara.buffer:*buffers*
             when (and (rimu-list-p buffer) (not (rimu-rewound-p buffer)))
               collect (tuatara.buffer:buffer-local buffer 'rimu-directory))))

(defun rimu-directories-changed (changed)
  "Note that the lists of the directories CHANGED (native names) are to be read
again, and read them (RIMU-FOLLOW-DISK)."
  (dolist (list (remove-if-not #'rimu-list-p tuatara.buffer:*buffers*))
    (when (member (tuatara.buffer:buffer-local list 'rimu-directory) changed :test #'string=)
      (setf (tuatara.buffer:buffer-local list 'rimu-disk-changed) t)))
  (rimu-follow-disk))

(defun rimu-follow-disk ()
  "Read again, in the background, the lists whose directory changed on disk
\(*RIMU-FOLLOW-CHANGES*), point and marks kept; a list being read just then,
once that is over."
  (when *rimu-follow-changes*
    (dolist (list (remove-if-not #'rimu-list-p tuatara.buffer:*buffers*))
      (when (and (tuatara.buffer:buffer-local list 'rimu-disk-changed)
                 (not (tuatara.buffer:buffer-local list 'rimu-job)))
        (setf (tuatara.buffer:buffer-local list 'rimu-disk-changed) nil)
        (rimu-update list :reread t :wait nil)))))

(defun rimu-lay-out-marked ()
  "Lay out again, in the background, the lists narrowed by search filters
whose marks changed line by line since they were laid out, so that the whole
list they are narrowed from shows those marks too; a list being read just
then, once that is over."
  (dolist (list (remove-if-not #'rimu-list-p tuatara.buffer:*buffers*))
    (when (and (tuatara.buffer:buffer-local list 'rimu-marks-changed)
               (not (tuatara.buffer:buffer-local list 'rimu-job)))
      (setf (tuatara.buffer:buffer-local list 'rimu-marks-changed) nil)
      (rimu-update list :wait nil))))

(defun rimu-tick (editor)
  "Tick function: show the lists whose directories were read in the
background, the texts of columns found there, and the preview; T while
something is read, the seconds until the preview is due, or NIL."
  (setf *rimu-editor* editor)
  (let ((working nil))
    (when *rimu-ready* (rimu-show-ready))
    (dolist (buffer (copy-list *rimu-waiting*))
      (let ((job (and (live-buffer-p buffer) (tuatara.buffer:buffer-local buffer 'rimu-job))))
        (cond ((or (null job) (rimu-job-done job))
               (setf *rimu-waiting* (remove buffer *rimu-waiting*))
               (when job (rimu-take-in buffer job)))
              (t (setf working t)))))
    (rimu-follow-disk)
    (rimu-lay-out-marked)
    (let ((preview (rimu-preview-tick editor)))
      (cond ((or working (eq preview t) (rimu-pending-p)) t)
            (t preview)))))

(defun rimu-entry-mark (buffer entry marks)
  "The mark ENTRY of the rimu list BUFFER has among MARKS (RIMU-MARKS), its
key (RIMU-MARK-KEY) made once for each entry listed."
  (let* ((listing (tuatara.buffer:buffer-local buffer 'rimu-listing))
         (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (key (if (and listing (equal directory (rimu-listing-directory listing)))
                  (let ((keys (rimu-listing-mark-keys listing)))
                    (or (gethash entry keys) (setf (gethash entry keys) (rimu-mark-key directory entry))))
                  (rimu-mark-key directory entry))))
    (gethash key marks)))

(defmethod mode-syntax-highlights ((mode rimu-mode) buffer rows)
  "The faces of a rimu list's rows ROWS: those it was laid out with, and the
mark and name of each entry marked since in the face of its mark."
  (let ((layout (tuatara.buffer:buffer-local buffer 'rimu-layout)))
    (when layout
      (loop for (from . to) in (tuatara.display:shown-ranges rows :buffer buffer)
            nconc (made-spans-between buffer from to)
            ;; marks given since the list was laid out: the mark and the name in its face
            nconc (let ((marks (tuatara.buffer:buffer-local buffer 'rimu-marks)))
                    (when (and marks (plusp (hash-table-count marks)))
                      (loop for line from (tuatara.buffer:buffer-line-number buffer from)
                              to (tuatara.buffer:buffer-line-number buffer (max from (1- to)))
                            for entry = (rimu-line-entry buffer line)
                            for mark = (and entry (rimu-entry-mark buffer entry marks))
                            when mark
                              collect (let ((start (tuatara.buffer:buffer-goto-line buffer line)))
                                        (list start (1+ start) (list-mark-face mark)))
                              and collect (let ((name (rimu-line-position buffer line)))
                                            (list name (tuatara.buffer:buffer-line-end buffer name) (list-mark-face mark))))))))))

(defmethod mode-file-name-at-point ((mode rimu-mode) buffer)
  "The file or folder of the entry on point's line in a rimu list, else the
folder it lists, for the commands that offer a file name."
  (let ((entry (rimu-entry-at buffer (tuatara.buffer:buffer-point buffer)))
        (directory (tuatara.buffer:buffer-local buffer 'rimu-directory)))
    (cond ((null entry) directory)
          ((= (length (tuatara.io:listed-octets entry)) 1) directory)
          ((rimu-dots-p entry) (tuatara.io:parent-path directory))
          (t (rimu-path directory entry)))))

(defmethod mode-line-extra ((mode rimu-mode) buffer)
  "What the mode line says of a rimu list's order: by time or by name."
  (if (eq (tuatara.buffer:buffer-local buffer 'rimu-sort) :time) "by time" "by name"))

;;; Commands

(defun rimu-show-directory (path &key point-on other show safe root back)
  "List the directory at PATH with rimu: in the list of it there is, else in a
new one; in the selected window, or with OTHER in another, or as SHOW, a
function of the list, shows it; point on the entry whose name is the octets
POINT-ON. With SAFE the list is opened safely: what is opened from it opens
safely, its preview shows files as they are opened safely, and it shows no
git column or sizes. With ROOT, a directory at or above PATH, \\<rimu-mode>\\[rimu-up]
there goes back to BACK, the buffer the list was opened from, instead of the
directory above. Returns the list, or NIL (said) when PATH is no directory."
  (let* ((directory (directory-name path :clean t))
         (stamp (ignore-errors (tuatara.io:path-stamp directory))))
    (cond ((null stamp) (message "No directory ~a" (abbreviated-path directory)) nil)
          ((not (eq (tuatara.io:stamp-kind stamp) :directory))
           (message "~a is not a directory" (abbreviated-path (string-right-trim "/" directory))) nil)
          (t (let ((buffer (or (find-if (lambda (b)
                                          (and (rimu-list-p b)
                                               (not (tuatara.buffer:buffer-local b 'rimu-preview-buffer))
                                               (not (rimu-found-p b))
                                               (not (rimu-rewound-p b))
                                               (equal (tuatara.buffer:buffer-local b 'rimu-directory) directory)))
                                        tuatara.buffer:*buffers*)
                                (rimu-make-list directory))))
               (when safe (setf (tuatara.buffer:buffer-local buffer :safe-visit) t))
               (when root
                 (setf (tuatara.buffer:buffer-local buffer 'rimu-root) (directory-name root :clean t)
                       (tuatara.buffer:buffer-local buffer 'rimu-back) back))
               (cond (show (funcall show buffer))
                     (other (pop-to-buffer buffer :other t))
                     (t (show-buffer buffer)))
               (rimu-update buffer :reread t :point-on point-on
                                   :fresh (null (tuatara.buffer:buffer-local buffer 'rimu-layout)))
               buffer)))))

(defun rimu-list-of-project (directory)
  "The rimu list used last that lists a folder of the project DIRECTORY is in
\(PROJECT-ROOT), or of no project when it is in none; neither a preview's list
nor one of files found. NIL when there is none."
  (let ((root (project-root directory)))
    (find-if (lambda (b)
               (and (live-buffer-p b) (rimu-list-p b)
                    (not (tuatara.buffer:buffer-local b 'rimu-preview-buffer))
                    (not (rimu-found-p b))
                    (not (rimu-rewound-p b))
                    (equal root (project-root (tuatara.buffer:buffer-local b 'rimu-directory)))))
             (append *buffer-order* (remove-if (lambda (b) (member b *buffer-order*)) tuatara.buffer:*buffers*)))))

(defun rimu-show-in-project (directory &key point-on show safe)
  "List DIRECTORY with rimu as RIMU-SHOW-DIRECTORY does (SHOW and POINT-ON as
it takes them), but with *RIMU-REUSE-LISTS*, when no list lists it yet, in the
list of its project used last (RIMU-LIST-OF-PROJECT), moved there; with SAFE
in a list of its own, opened safely. Returns the list, or NIL (said) when
DIRECTORY is no directory."
  (let* ((directory (directory-name directory :clean t))
         (reused (and *rimu-reuse-lists* (not safe)
                      (eq :directory (ignore-errors (tuatara.io:path-kind directory)))
                      (notany (lambda (b) (and (rimu-list-p b) (not (rimu-rewound-p b))
                                               (equal (tuatara.buffer:buffer-local b 'rimu-directory) directory)))
                              tuatara.buffer:*buffers*)
                      (rimu-list-of-project directory))))
    (if reused
        (progn (if show (funcall show reused) (show-buffer reused))
               (rimu-enter reused directory :point-on point-on)
               reused)
        (rimu-show-directory directory :point-on point-on :show show :safe safe))))

(defun rimu-visit-directory (path show)
  "Show the directory at PATH as its rimu list, with SHOW, a function of the
list, when it is a directory: what visiting a directory gives (tuatara
~/src/ in a terminal), opened safely when it is visited safely
\(*VISITING-SAFELY*). Returns the list, or NIL for a file visited as usual."
  (let ((native (ignore-errors (tuatara.io:native (tuatara.io:expand-path path)))))
    (when (and native (eq :directory (tuatara.io:path-kind native)))
      (rimu-show-directory native :show show :safe *visiting-safely*))))

(defun rimu-enter-elsewhere (buffer directory point-on)
  "List DIRECTORY, in another project than the rimu list BUFFER's, in another
list in BUFFER's place, BUFFER left listing what it lists: the list of
DIRECTORY there is, else one of its project (RIMU-SHOW-IN-PROJECT), else a
new one, point on the entry whose name is the octets POINT-ON; \\<rimu-mode>\\[rimu-last-directory] there comes back to BUFFER's directory."
  (let* ((leaving (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (entry (rimu-entry-at buffer (tuatara.buffer:buffer-point buffer)))
         (list (rimu-show-in-project directory :point-on point-on :safe (tuatara.io:safe-visit-p buffer))))
    (when list
      (setf (tuatara.buffer:buffer-local list 'rimu-last-directory)
            (cons leaving (and entry (tuatara.io:listed-octets entry)))))
    list))

(defun rimu-enter (buffer directory &key point-on)
  "Make the rimu list BUFFER list DIRECTORY (a native name ending in /) in
place of the one it lists, named after it, point on the entry whose name is
the octets POINT-ON. The directory it leaves is kept, with the entry point
was on, for \\<rimu-mode>\\[rimu-last-directory]; its marks stay with their files (RIMU-MARKED-ELSEWHERE),
and the lines taken out of it come back, as does everything the search
filters (\\[search-filter-regex]) hid: the filters go, another directory
being listed whole. A DIRECTORY in another project than the one it leaves
\(PROJECT-ROOT), or in a project when that is in none or the other way about,
is listed in another list instead (RIMU-ENTER-ELSEWHERE): a list
stays in its project."
  (let ((leaving (tuatara.buffer:buffer-local buffer 'rimu-directory)))
    (when (and leaving (not (rimu-rewound-p buffer)) (not (equal (project-root leaving) (project-root directory))))
      (return-from rimu-enter (rimu-enter-elsewhere buffer directory point-on)))
    (when (and leaving (not (string= leaving directory)))
      (setf (tuatara.buffer:buffer-local buffer 'rimu-last-directory)
            (cons leaving (let ((entry (rimu-entry-at buffer (tuatara.buffer:buffer-point buffer))))
                            (and entry (tuatara.io:listed-octets entry)))))
      ;; the lines taken out and the filters go; the marks stay with their files
      (let ((table (tuatara.buffer:buffer-local buffer 'rimu-taken-out)))
        (when table (clrhash table)))
      (drop-search-filters buffer)))
  (rimu-forget-found buffer)
  (setf (tuatara.buffer:buffer-local buffer 'rimu-directory) directory
        (tuatara.buffer:buffer-local buffer :directory) directory
        (tuatara.buffer:buffer-local buffer 'rimu-listing) nil)
  (let ((name (if (rimu-rewound-p buffer)
                  (format nil "*git rewind: ~a*" (rimu-buffer-name directory))
                  (rimu-buffer-name directory))))
    (unless (string= (tuatara.buffer:buffer-name buffer) name)
      (setf (tuatara.buffer:buffer-name buffer) (tuatara.buffer:unique-buffer-name name))))
  (rimu-update buffer :reread t :point-on point-on :fresh t))

(define-command rimu ()
  "List a folder with rimu, asked for with completion from the folder of the
path chosen last in a question for a path, else the current buffer's: its
name, then its entries a line each, folders first.
\\<rimu-mode>\\[rimu-open] opens the entry on the line, a folder in the same
list, \\[rimu-up] goes up; \\[rimu-toggle-details] shows the details
(permissions, links, owner, group, size, time), \\[rimu-toggle-dotfiles] the names
starting with a dot, \\[list-toggle-preview] the preview beside the list. See
rimu-mode for its keys."
  (let ((chosen (read-directory-path "Rimu (directory): " :note nil)))
    (when chosen (note-visited-path chosen (rimu-show-directory chosen) :directory t))))

(define-command rimu-jump ()
  "List the directory of the current buffer's file with rimu, point on the
file; a buffer with no file lists the directory it
stands in. In a rimu list, go up to the directory holding its own
(\\<rimu-mode>\\[rimu-up])."
  (let* ((buffer (current-buffer))
         (file (tuatara.buffer:buffer-file buffer)))
    (cond ((rimu-list-p buffer) (rimu-up))
          (file (rimu-show-file file))
          (t (rimu-show-directory (tuatara.io:native (buffer-directory buffer)))))))

(defun rimu-show-file (path)
  "List the directory of the file at PATH with rimu, in the selected window,
point on the file; returns the list."
  (rimu-show-directory (tuatara.io:parent-path path)
                       :point-on (tuatara.text:string-octets (tuatara.io:native-file-name path))))

(define-command rimu-up ()
  "List the directory holding this one, in this list, point on the one left;
in a list of files found, list the directory they were found in. At the top
of a list opened from somewhere with a top of its own, a package's folder
opened from the package manager, go back there."
  (let* ((buffer (current-buffer))
         (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (parent (and (string/= directory "/") (tuatara.io:parent-path directory)))
         (back (tuatara.buffer:buffer-local buffer 'rimu-back)))
    (cond
      ((rimu-found-p buffer) (rimu-show-directory directory))
      ((and (rimu-rewound-p buffer) (string= directory (car (rimu-rewound-p buffer))))
       (message "This is the top of the repository"))
      ((equal directory (tuatara.buffer:buffer-local buffer 'rimu-root))
       (if (live-buffer-p back)
           (show-buffer back)
           (message "~a is the top of this list" (abbreviated-path directory))))
      ((null parent)
       (message "/ has nothing above it"))
      (t (rimu-enter buffer parent :point-on (tuatara.text:string-octets (tuatara.io:native-file-name directory)))))))

(defun rimu-unopenable-reason (entry)
  "Why ENTRY of a rimu list is not opened, a sentence; NIL when it can be: an
entry that could not be examined, a link that leads nowhere, a named pipe, a
socket or a device."
  (let ((name (tuatara.io:listed-name entry))
        (kind (rimu-leads-to entry)))
    (cond ((null (tuatara.io:listed-stamp entry)) (format nil "~a could not be examined" name))
          ((and (null kind) (eq (rimu-kind entry) :symlink))
           (format nil "~a leads nowhere: ~a" name (tuatara.io:listed-target entry)))
          ((not (member kind '(:directory :regular)))
           (format nil "~a is ~a; rimu does not open it" name (tuatara.io:kind-words kind))))))

(defun rimu-open-entry (entry where &key safe)
  "Open ENTRY of the current rimu list: a directory in the list itself (WHERE
:HERE) or in another list in another window (:DISPLAY, the list staying
selected); a file visited as \\[find-file] visits it, safely where that would be
or with SAFE, and shown there, a quick edit beside the list ending first
(asking whether to save). A named pipe, a socket, a device and a link leading
nowhere are not opened: what they are is said. In a list of files found, a
directory is listed in a list of its own, the files found kept. In a list
opened safely, everything opens safely."
  (let* ((buffer (current-buffer))
         (directory (tuatara.buffer:buffer-local buffer 'rimu-directory))
         (safe (or safe (tuatara.io:safe-visit-p buffer))))
    (cond ((null entry) (message "No file on this line"))
          ((and (rimu-dots-p entry) (eq where :here))
           (if (= (length (tuatara.io:listed-octets entry)) 1) (rimu-refill buffer) (rimu-up)))
          ((rimu-unopenable-reason entry) (message "~a" (rimu-unopenable-reason entry)))
          ;; a folder as a commit left it: what is in it as that commit left it
          ((rimu-rewound-p buffer) (rimu-rewind-open buffer entry))
          ((rimu-directory-p entry)
           (let ((path (directory-name (rimu-path directory entry) :clean t)))
             (cond ((and (eq where :here) (rimu-found-p buffer)) (rimu-show-directory path :safe safe))
                   ((eq where :here) (rimu-enter buffer path))
                   (t (rimu-show-directory path :show #'rimu-display-show :safe safe)))))
          (t (when (eq where :here)
               (rimu-end-quick-edit-first))
             (visit-path (rimu-path directory entry)
                         :safe safe
                         :show (ecase where
                                 (:here (lambda (b) (show-buffer b)))
                                 (:display #'rimu-display-show)))))))

(define-command rimu-open ()
  "Open the entry on the current line: a directory is listed in its place in
this list; a file is visited as \\[find-file] visits it, safely where that would
be, and with a prefix argument (\\[universal-argument]) safely always. A named
pipe, a socket or a device is not opened; rimu says what it is."
  (rimu-open-entry (rimu-entry-at-point) :here :safe (and (prefix-argument) t)))

(define-command rimu-display ()
  "Show the entry on the current line in another window, staying in the list:
a directory in a list of its own, a file as \\<rimu-mode>\\[rimu-open] opens it. With
the preview on, it is shown in the preview's window, in place of the preview
until point moves on; a quick edit there ends first (asking whether to save)."
  (rimu-end-quick-edit-first)
  (rimu-open-entry (rimu-entry-at-point) :display :safe (and (prefix-argument) t)))

(defun rimu-goto-line (buffer line)
  "Put point on the name of the entry on LINE of the rimu list BUFFER."
  (setf (tuatara.buffer:buffer-point buffer) (rimu-line-position buffer line)))

(defun rimu-keep-point-on-entries (editor)
  "After each command: in the rimu list in EDITOR's selected window, point gone
below the last entry (\\[end-of-buffer], \\[next-line] on the last entry, a
list come back to) goes back to the name of the last entry."
  (let* ((buffer (tuatara.display:window-buffer (editor-window editor)))
         (entries (and (rimu-list-p buffer) (tuatara.buffer:buffer-local buffer 'list-entries)))
         (index (and entries (position-if #'identity entries :from-end t)))
         (last (and index (+ index (tuatara.buffer:buffer-local buffer 'list-first-line 0)))))
    (when (and last (> (tuatara.buffer:buffer-line-number buffer (tuatara.buffer:buffer-point buffer)) last))
      (rimu-goto-line buffer last))))

(define-command rimu-goto-file ()
  "Go to an entry of this list, its name asked for with completion; among
those the search filters leave, while they narrow the list."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (entries lines) (rimu-shown-entries buffer)
      (let ((names (mapcar (lambda (entry) (escape-controls (tuatara.io:listed-name entry))) entries)))
        (if (null names)
            (message "No files in this list")
            (let* ((chosen (completing-read "Go to file: " names :require-match t))
                   (index (position chosen names :test #'string=)))
              (when index (rimu-goto-line buffer (aref lines index)))))))))

(defun rimu-step-directory (step)
  "Put point on the name of the next directory of this list (STEP 1) or the
one before (STEP -1); true when there is one."
  (let ((buffer (current-buffer)))
    (when (list-step-entry buffer step :test #'rimu-directory-p)
      (rimu-goto-line buffer (tuatara.buffer:buffer-line-number buffer (point)))
      t)))

(define-command rimu-next-directory ()
  "Go to the next directory of this list."
  (unless (rimu-step-directory 1) (message "No directory below")))

(define-command rimu-previous-directory ()
  "Go to the previous directory of this list."
  (unless (rimu-step-directory -1) (message "No directory above")))

(defun rimu-toggle (key on off)
  "Turn the rimu list's KEY local on or off, lay it out again, and say which:
ON or OFF."
  (let* ((buffer (current-buffer))
         (value (setf (tuatara.buffer:buffer-local buffer key) (not (tuatara.buffer:buffer-local buffer key)))))
    (rimu-update buffer)
    (message (if value on off))))

(define-command rimu-toggle-details ()
  "Show or hide the details before each name in this list: its inode number,
as ls -i shows it, then its permissions, links, owner, group, size and when
it last changed, as ls -l shows them, and where a symbolic link leads. *RIMU-DETAILS* says whether new lists show them."
  (rimu-toggle 'rimu-details "Details shown" "Details hidden"))

(define-command rimu-toggle-dotfiles ()
  "Show or hide the entries of this list whose names start with a dot, . and
.. among them. *RIMU-DOTFILES* says whether new lists show them."
  (rimu-toggle 'rimu-dotfiles "Names starting with a dot shown" "Names starting with a dot hidden"))

(define-command rimu-toggle-sort ()
  "Order this list by name or by time, those changed last first; the mode
line says which. *RIMU-SORT* says how new lists are ordered."
  (let ((buffer (current-buffer)))
    (setf (tuatara.buffer:buffer-local buffer 'rimu-sort)
          (if (eq (tuatara.buffer:buffer-local buffer 'rimu-sort) :time) :name :time))
    (rimu-update buffer)
    (message (if (eq (tuatara.buffer:buffer-local buffer 'rimu-sort) :time)
                 "Sorted by time, the latest first"
                 "Sorted by name"))))

(defun rimu-refill (buffer)
  "Read the directory of the rimu list BUFFER again, point staying on its
entry, the lines taken out of it shown again (\\[revert-buffer]); in a list of
files found, find them again."
  (let ((out (tuatara.buffer:buffer-local buffer 'rimu-taken-out)))
    (when out (clrhash out)))
  (if (rimu-found-p buffer)
      (rimu-start-search buffer)
      (rimu-update buffer :reread t)))

(define-command rimu-home ()
  "List your home directory in this list's place."
  (let ((home (directory-name (tuatara.io:native (tuatara.io:home-directory)) :clean t)))
    (rimu-end-quick-edit-first)
    (rimu-enter (current-buffer) home)))

(define-command rimu-last-directory ()
  "List again the directory this list listed before, point on the entry it was
on; again, back to this one, as cd - goes back and forth."
  (let ((last (tuatara.buffer:buffer-local (current-buffer) 'rimu-last-directory)))
    (if (null last)
        (message "No directory listed before in this list")
        (progn
          (rimu-end-quick-edit-first)
          (rimu-enter (current-buffer) (car last) :point-on (cdr last))))))

(defmethod mode-before-quit ((mode rimu-mode) buffer)
  "Leaving a rimu list (\\[quit-window]): a quick edit beside it ends first,
asking whether to save, and its preview closes."
  (declare (ignore buffer))
  (rimu-end-quick-edit-first)
  (rimu-close-preview))

(defun install-rimu (editor)
  "Bind the keys of rimu's lists, of its quick edits and those that start it,
show what is read in the background, mark a quick edit in the mode line, keep
point on a list's entries, have EDITOR's preview follow the list, and refuse
the commands that change files in a list opened safely."
  (pushnew 'rimu-preview-follow (editor-post-command-functions editor))
  (pushnew 'rimu-keep-point-on-entries (editor-post-command-functions editor))
  (dolist (command '(rimu-copy rimu-move rimu-symlink rimu-hardlink rimu-rename list-delete-marked list-execute rimu-make-directory
                     rimu-edit-names rimu-names-apply rimu-touch rimu-make-formatted-file rimu-wipe-files
                     rimu-trash rimu-trash-restore rimu-trash-empty
                     rimu-encrypt-files rimu-decrypt-files rimu-sign-files
                     rimu-archive rimu-extract rename-formatted rename-formatted-keywords))
    (pushnew command *file-changing-commands*))
  (define-keys *rimu-keymap* '(("RET" rimu-open) ("l" rimu-open) ("h" rimu-up)
                               ("o" rimu-quick-edit) ("C-o" rimu-display) ("g f" rimu-goto-file)
                               (">" rimu-next-directory) ("<" rimu-previous-directory)
                               ("I" rimu-toggle-details) ("H" rimu-toggle-dotfiles)
                               ("s" rimu-toggle-sort) ("T" rimu-toggle-picture)
                               ("g h" rimu-home) ("g l" rimu-last-directory)))
  (entry-preview-keys 'rimu-mode "a rimu list (\\[rimu])")
  (free-list-keys *rimu-keymap*)
  (define-key *rimu-quick-edit-keymap* "C-c C-c" 'rimu-quick-edit-done)
  (define-key *rimu-quick-edit-normal-keymap* "q" 'rimu-quick-edit-done)
  (pushnew 'rimu-quick-edit-mode-line *mode-line-functions*)
  (define-key *global-keymap* "C-x d" 'rimu)
  (define-key *global-keymap* "C-x C-j" 'rimu-jump)
  (pushnew 'rimu-preview-hex *hex-view-functions*)
  (install-rimu-operations)
  (install-rimu-gpg)
  (install-rimu-git)
  (install-rimu-sizes)
  (pushnew 'rimu-visit-directory *file-views*)
  (pushnew 'rimu-refresh-lists *paths-changed-functions*)
  (pushnew 'rimu-tick *tick-functions*)
  (follow-directories-for 'rimu-watched-directories 'rimu-directories-changed))

(commands-for-mode 'rimu-mode "a rimu list (\\[rimu])"
                   'rimu-up 'rimu-open 'rimu-quick-edit 'rimu-display 'rimu-goto-file
                   'rimu-next-directory 'rimu-previous-directory 'rimu-toggle-details 'rimu-toggle-dotfiles
                   'rimu-toggle-sort)
