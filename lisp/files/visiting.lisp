;;;; lisp/files/visiting.lisp --- Asking for files and folders, visiting, saving and renaming files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The questions for a path (with the paths chosen before first), the views
;;;; a file may be shown in, and the commands that find, save, write and
;;;; rename the file of a buffer.

(in-package #:tuatara.defaults)

;;; Asking for files and folders

(defun buffer-visiting (path &key except)
  "The buffer visiting the file at PATH (a native name or a pathname), found
as \\[find-file] finds one, other than EXCEPT; NIL when there is none."
  (let ((expanded (ignore-errors (tuatara.io:expand-path path))))
    (and expanded
         (find-if (lambda (buffer) (and (not (eq buffer except)) (equal (tuatara.buffer:buffer-file buffer) expanded)))
                  tuatara.buffer:*buffers*))))

(defvar *last-used-first* t
  "True for the questions that choose a file or a folder (\\[find-file], \\[rimu]
and the rest) to start in the folder of the path chosen last in any of them,
and to list first, in each folder, what was chosen there last (or something
in it), TAB then going the way last gone; the others follow in order of
name. As you type, what was chosen there lately comes first among the names
matching as closely. NIL for questions starting in the folder of the current
buffer, their lists in order of name alone; either way the folders come
first, then the files, each in order of name, numbers by their value (file2
before file10), case not counted. A path opened safely, or in one of
*SAFE-DIRECTORIES*, is never remembered so.")

(defvar *chosen-paths-kept* 1000
  "How many files and directories chosen in the questions for them are kept,
to offer again first.")

(defun parse-chosen-path-line (line)
  "The path chosen lately LINE writes (ESCAPE-PATH)."
  (unescape-path line))

(define-state-list *chosen-paths* "chosen-paths" 'parse-chosen-path-line 'escape-path '*chosen-paths-kept*)

(defun chosen-paths-file ()
  "Where the files and directories chosen lately are kept: in the state directory."
  (state-list-path *chosen-paths*))

(defun chosen-paths ()
  "The files and directories chosen lately in the questions for them (\\[find-file],
\\[rimu] and the rest), native names, the latest first, with what other
sessions chose (STATE-LIST-ENTRIES)."
  (state-list-entries *chosen-paths*))

(defun shown-safely-p (native)
  "True when a buffer shows the file or folder NATIVE (a native name) opened
safely: visiting it, or listing it with \\[rimu]."
  (let ((folder (tuatara.io:slash-ended native)))
    (some (lambda (buffer)
            (and (tuatara.io:safe-visit-p buffer)
                 (let ((file (tuatara.buffer:buffer-file buffer))
                       (listed (tuatara.buffer:buffer-local buffer :directory)))
                   (or (and file (equal (tuatara.io:native file) native))
                       (and listed (equal (tuatara.io:slash-ended (tuatara.io:native listed)) folder))))))
          tuatara.buffer:*buffers*)))

(defun note-chosen-path (path &key directory safe)
  "Remember PATH, a file or a folder (with DIRECTORY, a folder whatever its
name ends in) just chosen in a question for one, as the latest: the next
question starts in its folder and that folder's list offers it first; kept
in the state folder, with what other sessions chose (STATE-LIST-PUT).
Nothing is remembered of a path opened safely (SAFE, or a buffer showing it
so: SHOWN-SAFELY-P) or in one of *SAFE-DIRECTORIES*: a later question
neither starts there nor offers it."
  (let ((native (ignore-errors (tuatara.io:native (tuatara.io:expand-path path)))))
    (when (and native (plusp (length native)) (not safe)
               (not (safe-directory-of native)) (not (shown-safely-p native)))
      (state-list-put *chosen-paths*
                      (list (if directory (tuatara.io:slash-ended native) native))))))

(defun note-visited-path (path buffer &key directory)
  "Remember PATH, just chosen and shown in BUFFER (NIL when it could not be
shown), as NOTE-CHOSEN-PATH does, unless BUFFER shows it opened safely."
  (when buffer
    (note-chosen-path path :directory directory :safe (tuatara.io:safe-visit-p buffer))))

(defun named-safe-p (native)
  "True when NATIVE, a path remembered, is in one of *SAFE-DIRECTORIES* by the
names as written (PATH-WITHIN-P), or a buffer shows it opened safely; true
for every path when *SAFE-DIRECTORIES* cannot be read. The disk is not asked."
  (or (shown-safely-p native)
      (handler-case (some (lambda (directory)
                            (tuatara.io:path-within-p native (tuatara.io:native (tuatara.io:expand-path directory))))
                          (safe-directories))
        (error () t))))

(defun writing-chosen-p (native)
  "True when NATIVE, a path remembered, is never offered first in a question
whose answer is written to: in one of *SAFE-DIRECTORIES* (NAMED-SAFE-P), or
in an evidence folder (EVIDENCE-PATH-P)."
  (or (named-safe-p native) (evidence-path-p native)))

(defun last-chosen-native-folder (&key writing)
  "The folder, a native name ending in /, of the latest path chosen in a
question for a file or a folder (CHOSEN-PATHS) that is not of evidence
\(NAMED-SAFE-P; with WRITING, for a question whose answer is written to,
WRITING-CHOSEN-P): the path itself when it is a folder (its name ends in /),
else the folder it is in; NIL when there is none or *LAST-USED-FIRST* is NIL."
  (let ((last (and *last-used-first* (find-if-not (if writing #'writing-chosen-p #'named-safe-p) (chosen-paths)))))
    (and last (if (directory-input-p last) last (tuatara.io:parent-path last)))))

(defun last-chosen-folder ()
  "The folder of the path chosen last in a question for a file or a folder
\(LAST-CHOSEN-NATIVE-FOLDER) as the questions show it (~/notes/), or NIL.
The disk is not asked: a folder no longer there is listed as empty, the
question's listing finding it so."
  (let ((folder (last-chosen-native-folder)))
    (and folder (abbreviated-path folder))))

(defvar *slow-listings* '()
  "The folders whose listing in a question for a path had not ended within
*FILE-LISTING-WAIT*, still going (a drive that does not answer): each
\(FOLDER . GATHERING), FOLDER a native name ending in /. One is let go of
once its listing ends.")

(defun slow-listing-p (folder)
  "True while the listing of FOLDER (a native name) begun in an earlier
question has not ended (*SLOW-LISTINGS*); those ended are let go of."
  (setf *slow-listings* (remove-if (lambda (entry)
                                     (declare (notinline gathering-done))
                                     (gathering-done (cdr entry)))
                                   *slow-listings*))
  (and (assoc (tuatara.io:slash-ended folder) *slow-listings* :test #'string=) t))

(defun path-question-start (&key writing)
  "The text a question for a path starts with when it is given none: the
folder chosen last (LAST-CHOSEN-FOLDER), else, or while that folder's
listing has not ended (SLOW-LISTING-P), that of the current buffer
\(BUFFER-DIRECTORY). With WRITING, for a question whose answer is written
to, never a folder of evidence (*EVIDENCE-FOLDERS*): your home folder in its
place."
  (let ((folder (last-chosen-native-folder :writing writing)))
    (if (and folder (not (slow-listing-p folder)))
        (abbreviated-path folder)
        (let ((here (buffer-directory (current-buffer))))
          (if (and writing (evidence-path-p here)) "~/" (abbreviated-path here))))))

(defun writing-start (initial)
  "INITIAL, the text a question whose answer is written to starts with, or
in its place, when it names a place in an evidence folder, the folder
PATH-QUESTION-START offers for writing, with INITIAL's name after it when
INITIAL names a file."
  (if (and initial (plusp (length initial))
           (evidence-path-p (tuatara.io:native (tuatara.io:expand-path initial))))
      (concatenate 'string (path-question-start :writing t)
                   (if (directory-input-p initial) "" (tuatara.io:native-file-name initial)))
      initial))

(defun chosen-ranks (directory chosen)
  "How lately each entry of DIRECTORY (a native name ending in /) was chosen,
or something in it, by CHOSEN, the paths chosen lately (CHOSEN-PATHS): a
table from its name, a folder's with a slash after it, to its place among
them, 0 the latest; those in one of *SAFE-DIRECTORIES* left out
\(SAFE-DIRECTORY-OF, which may ask the disk: called where the folder is read)."
  (let ((ranks (make-hash-table :test 'equal)) (length (length directory)))
    (loop for path in chosen
          for place from 0
          when (and (> (length path) length) (string= directory path :end2 length))
            do (let* ((slash (position #\/ path :start length))
                      (name (if slash (subseq path length (1+ slash)) (subseq path length))))
                 ;; what is in one of *safe-directories* is offered first never
                 (unless (safe-directory-of path)
                   (unless (gethash name ranks) (setf (gethash name ranks) place))
                   ;; a directory chosen itself, its name ending in / or not
                   (unless (or slash (gethash (concatenate 'string name "/") ranks))
                     (setf (gethash (concatenate 'string name "/") ranks) place)))))
    ranks))

(defvar *file-candidates-kept* nil
  "The candidates FILE-CANDIDATES made last, with what tells whether they are
still good: (DIRECTORY TIME CHOSEN CANDIDATES), the directory's native name,
its time of change, and the paths chosen lately as they were.")

(defparameter *file-listing-wait* 1/10
  "Seconds a question for a file waits for a directory to be read before it
shows that it is still being read, the editor free meanwhile (READ-PATH).")

(defun file-candidates (input)
  "Completion candidates for INPUT, a path being typed (a leading ~ is the home
directory): the entries of its directory part (the current directory when there
is none), directories first and marked by a trailing slash, each with its
permissions, its size (or what it is, for a named pipe or a device) and when it
last changed beside it; those chosen lately in a question for files, or with
something in them chosen, ranked the latest first (CHOSEN-RANKS). Nothing but
the directory is opened, so a named pipe in it cannot hold the editor up.
The candidates of a directory are made once while it does not change, not
again at each key typed in it."
  (directory-candidates (input-directory-path input) (and *last-used-first* (chosen-paths))))

(defun input-directory-path (input)
  "The directory INPUT, a path being typed, names the entries of: its part up
to its last slash as an absolute pathname, the current directory when it has none."
  (let ((dir (input-directory input)))
    (tuatara.io:expand-path (if (string= dir "") "./" dir))))

(defun directory-candidates (expanded chosen)
  "The candidates FILE-CANDIDATES gives for the entries of the directory
EXPANDED, CHOSEN being the paths chosen lately (CHOSEN-PATHS) or NIL: those
made last while the directory has not changed since, else made afresh. The
directory is read on the thread calling this, which in a question for a file
is one of its own (READ-PATH)."
  (let* ((native (tuatara.io:native expanded))
         (time (let ((stamp (ignore-errors (tuatara.io:path-stamp expanded)))) (and stamp (tuatara.io:stamp-mtime stamp))))
         (kept *file-candidates-kept*))
    (if (and kept time (equal (first kept) native) (eql (second kept) time) (eq (third kept) chosen))
        (fourth kept)
        (let ((candidates (file-candidates-of expanded chosen)))
          (when time (setf *file-candidates-kept* (list native time chosen candidates)))
          candidates))))

(defun file-candidates-of (expanded chosen)
  "The completion candidates of the entries of the folder EXPANDED, as
FILE-CANDIDATES gives them, ranked by CHOSEN, the paths chosen lately: the one
chosen there last first, then the folders and then the files, each in order
of name (NATURAL-STRING-LESSP)."
  (let* ((entries (tuatara.io:directory-entries expanded :stamps t
                                                         :order #'tuatara.text:natural-string-lessp))
         (ranks (chosen-ranks (tuatara.io:slash-ended (tuatara.io:native expanded)) chosen))
         (now (get-universal-time)))
    (flet ((fields (kind stamp)
             (if stamp (stamp-fields stamp now) (list (tuatara.io:kind-words kind)))))
      (last-chosen-first
       (append
        (loop for (name kind nil nil stamp) in entries
              when (eq kind :directory)
                collect (let ((shown (concatenate 'string name "/")))
                          (make-candidate shown :annotation (fields kind stamp) :rank (gethash shown ranks))))
        (loop for (name kind nil nil stamp) in entries
              unless (eq kind :directory)
                collect (make-candidate name :annotation (fields kind stamp) :rank (gethash name ranks))))))))

(defun last-chosen-first (candidates)
  "CANDIDATES, a fresh list, with the one chosen last (the lowest rank)
moved to the front, the others in their order."
  (let ((latest nil))
    (dolist (c candidates)
      (let ((rank (candidate-rank c)))
        (when (and rank (or (null latest) (< rank (candidate-rank latest))))
          (setf latest c))))
    (if latest (cons latest (delete latest candidates :count 1 :test #'eq)) candidates)))

(defun input-directory (input)
  "The directory part of INPUT, a path being typed: up to its last slash."
  (let ((slash (position #\/ input :from-end t)))
    (if slash (subseq input 0 (1+ slash)) "")))

(defun substitute-file-input (input)
  "INPUT, a path being typed, as the file prompts read it: a path typed after
the directory offered starts again, from its last // on without the first
slash (~/notes//etc/hosts is /etc/hosts) and from its last /~/ on without what
is before the ~ (/etc/~/notes is ~/notes)."
  (let* ((double (search "//" input :from-end t))
         (home (or (search "/~/" input :from-end t)
                   (and (>= (length input) 2) (string= "/~" input :start2 (- (length input) 2))
                        (- (length input) 2))))
         (from (max (if double (1+ double) 0) (if home (1+ home) 0))))
    (if (plusp from) (subseq input from) input)))

(defun resolve-file-input (input chosen)
  "The path to visit when CHOSEN (a candidate string or the raw input) was
chosen while INPUT was typed: the candidate is relative to INPUT's directory part."
  (let ((slash (position #\/ input :from-end t)))
    (if (and slash (not (find #\/ chosen :end (max 0 (1- (length chosen))))))
        (concatenate 'string (subseq input 0 (1+ slash)) chosen)
        chosen)))

(defun input-when-directory (candidates input)
  "The selection of a file or directory question, as INPUT changes: the line
typed itself when it names a directory (ends in /), so that RET takes that
directory; else the first of CANDIDATES."
  (declare (ignore candidates))
  (if (directory-input-p input) :input 0))

(defun directory-input-p (path)
  "True when PATH, a path typed or chosen, names a directory: it ends in /."
  (and (plusp (length path)) (char= #\/ (char path (1- (length path))))))

(defvar *path-question-keymap* (make-keymap :name "path-question")
  "The keys of a question for a path (READ-PATH), beside the minibuffer's:
\\<path-question>\\[minibuffer-home-directory] starts the path again from your
home folder.")

(defvar *asking-for-path* nil
  "True while a question for a path asks (READ-PATH).")

(defun asking-for-path-p (buffer)
  "True while a question for a file or a folder asks, whatever BUFFER is."
  (declare (ignore buffer))
  (and *asking-for-path* *editor* (editor-minibuffer-active *editor*) t))

(define-command minibuffer-home-directory ()
  "In a question for a file or a folder, put ~/, your home folder, in place
of the path typed, listing what is in it."
  (replace-minibuffer-text 0 (length (editor-minibuffer-text *editor*)) "~/"))

(commands-for-buffers 'asking-for-path-p "a question for a file or a folder" 'minibuffer-home-directory)

(define-keys *path-question-keymap* '(("S-TAB" minibuffer-home-directory)))

(defun read-path (prompt &key writing (initial (path-question-start :writing writing)) directories preview)
  "Ask with PROMPT for a path, as READ-FILE-PATH and READ-DIRECTORY-PATH do:
from INITIAL (as standard the folder of the path chosen last in such a
question, else the folder the current buffer stands in:
PATH-QUESTION-START; with WRITING, for a question whose answer is written
to, never a place in an evidence folder, which is then neither where it
starts (WRITING-START) nor ranked first among the entries), its name completed against the entries of the folder
being typed (FILE-CANDIDATES; with DIRECTORIES, its folders only), matched
by any part of the name. While nothing is typed after the folder, the entry
chosen there last comes first, the others in order of name; as you type,
the best matches first, those chosen lately before others. A / or ~/ typed
after it starts the path again (SUBSTITUTE-FILE-INPUT), and
\\<path-question>\\[minibuffer-home-directory] starts it from ~/. The line
typed is selected while it names a folder (ends in /), else the first entry
matching; a folder chosen is gone into, its entries offered in turn. With PREVIEW, a
function as CALL-WITH-WINDOW-PREVIEW gives, the file selected is shown
meanwhile. Returns the path, and as a second value true when it is the line
typed, taken as it is (\\<minibuffer>\\[minibuffer-done] on it, or
\\[minibuffer-submit-input]), NIL when it is a file chosen."
  ;; the entries of a directory read afresh for each question
  (setf *file-candidates-kept* nil)
  (let ((dir (if writing (writing-start initial) initial)) (input ""))
    (call-with-window-preview
     (lambda (candidate made)
       (let ((chosen (candidate-result candidate)))
         (when (stringp chosen)
           (preview-file-buffer (tuatara.io:native (tuatara.io:expand-path
                                                    (substitute-file-input
                                                     (resolve-file-input (substitute-file-input input) chosen))))
                                made))))
     (lambda (show)
       (loop
         (setf input dir)
         (let* ((listed nil)
                (listed-for nil)
                (listing nil)
                (chosen (labels ((take-listed ()
                                   ;; what the listing found, once it has: true when it came now
                                   (multiple-value-bind (found done) (take-gathered listing)
                                     (when done (setf listing nil))
                                     (when found (setf listed (first found)))
                                     (and found t)))
                                 (list-directory (path)
                                   (let* ((expanded (input-directory-path path))
                                          (folder (tuatara.io:slash-ended (tuatara.io:native expanded)))
                                          (chosen (and *last-used-first*
                                                       (if writing
                                                           (remove-if #'evidence-path-p (chosen-paths))
                                                           (chosen-paths)))))
                                     (setf listed '() listing nil)
                                     ;; a folder whose listing has not ended yet is not listed again
                                     (unless (slow-listing-p folder)
                                       (setf listing (start-gathering
                                                      "tuatara file names"
                                                      (lambda (give cancel)
                                                        (declare (ignore cancel))
                                                        (let ((all (directory-candidates expanded chosen)))
                                                          (funcall give (if directories
                                                                            (remove-if-not (lambda (c) (directory-input-p (candidate-string c)))
                                                                                           all)
                                                                            all))))))
                                       ;; a folder read quickly is shown at once, as though read here
                                       (unless (wait-for-gathering listing *file-listing-wait*)
                                         (push (cons folder listing) *slow-listings*))
                                       (take-listed)))))
                          (unwind-protect
                               (let ((*asking-for-path* t))
                                (completing-read prompt
                                                 (lambda (typed)
                                                   ;; the directory is listed again only when its part of the input changes
                                                   (setf input typed)
                                                   (let* ((path (substitute-file-input typed))
                                                          (directory (input-directory path)))
                                                     (unless (equal directory listed-for)
                                                       (when listing (stop-gathering listing))
                                                       (setf listed-for directory)
                                                       (list-directory path))
                                                     listed))
                                                 :initial dir
                                                 :keymap *path-question-keymap*
                                                 :keep-order :until-typed
                                                 :select #'input-when-directory
                                                 :reselect t
                                                 :preview (and preview show)
                                                 :source (lambda ()
                                                           (if listing
                                                               (values (take-listed) (and listing t) (length listed))
                                                               (values nil nil (length listed))))))
                            (when listing (stop-gathering listing)))))
                ;; the line typed taken (RET on it, or C-j), not a candidate
                (typed (and (stringp chosen) (string= chosen input)))
                (path (substitute-file-input (if typed chosen (resolve-file-input (substitute-file-input input) chosen)))))
           (if (and (not typed) (directory-input-p path))
               (setf dir path)
               (return (values path typed)))))))))

(defun read-file-path (prompt &key new-file preview (note t) writing)
  "Ask for a file with PROMPT, completing its name against the folder being
typed, from the folder of the path chosen last in such a question, else the
folder the current buffer stands in (BUFFER-DIRECTORY): that of its file,
the one it lists, or *DEFAULT-DIRECTORY*, your home folder written ~
\(READ-PATH, PATH-QUESTION-START). The file chosen is remembered as the
latest (NOTE-CHOSEN-PATH) unless NOTE is NIL. A / or ~/ typed after it starts the path
again (SUBSTITUTE-FILE-INPUT). The first file selected, or the line typed when
it names a directory or nothing matches; a directory candidate descends into
it. Returns the path typed or chosen, or NIL (said) when nothing was named; a
directory typed and taken comes with :DIRECTORY as a second value, or with
NEW-FILE :NEW, for a new file there. With PREVIEW, the file selected is
shown meanwhile (CALL-WITH-WINDOW-PREVIEW). WRITING, for a question whose
answer is written to, as READ-PATH has it."
  (let ((path (read-path prompt :preview preview :writing writing)))
    (cond ((zerop (length path))
           (if new-file
               (values "./" :new)
               (progn (message "No file name") nil)))
          ((directory-input-p path) (values path (if new-file :new :directory)))
          (t (when note (note-chosen-path path)) path))))

(defun read-directory-path (prompt &key writing (initial (path-question-start :writing writing)) (note t))
  "Ask for a folder with PROMPT, from INITIAL (as standard the folder of the
path chosen last in such a question, else the folder the current buffer
stands in: PATH-QUESTION-START), as READ-FILE-PATH asks for a file: the
folders in the one being typed offered, matched by any part of the name,
those chosen lately first; a / or ~/ typed after it starts the path again.
The line typed is selected while it names a directory (ends in /), else the
first directory matching. \\<minibuffer>\\[minibuffer-done] or
\\[minibuffer-complete] on a directory goes into it; \\[minibuffer-done] on the
line typed, one step above the first (\\[minibuffer-previous-candidate]), takes
it, as \\[minibuffer-submit-input] does at any time: a directory, or a new
name. Returns what was taken, ./ when nothing was typed, remembered as
chosen lately (NOTE-CHOSEN-PATH) unless NOTE is NIL: a caller that shows the
folder then remembers it itself, once shown (NOTE-VISITED-PATH). WRITING,
for a question whose answer is written to, as READ-PATH has it."
  (let* ((path (read-path prompt :initial initial :directories t :writing writing))
         (path (if (zerop (length path)) "./" path)))
    (when note (note-chosen-path path :directory t))
    path))

(defun read-native-path (prompt &key initial (kind :directory) (note t) writing)
  "Ask with PROMPT for a path of KIND, :DIRECTORY (or a new name) as
READ-DIRECTORY-PATH asks, :FILE as READ-FILE-PATH asks, or :ANY as READ-PATH
asks, from INITIAL where it takes one, and return what was taken as a whole
native name, ~ and the like expanded; NIL when nothing was typed. A folder
or file taken is remembered as chosen lately unless NOTE is NIL, for a
caller that opens it and remembers it once opened (NOTE-VISITED-PATH).
WRITING, for a question whose answer is written to (a place to save or copy
to), never starts in an evidence folder nor offers one first (READ-PATH)."
  (let ((typed (ecase kind
                 (:directory (if initial
                                 (read-directory-path prompt :initial initial :note note :writing writing)
                                 (read-directory-path prompt :note note :writing writing)))
                 (:file (read-file-path prompt :note note :writing writing))
                 (:any (if initial
                           (read-path prompt :initial initial :writing writing)
                           (read-path prompt :writing writing))))))
    (and typed (plusp (length typed)) (tuatara.io:native (tuatara.io:expand-path typed)))))

(defun read-new-native-path (prompt &rest keys)
  "Ask with PROMPT for a path that is not there yet, as READ-NATIVE-PATH asks
with KEYS; NIL, said, when nothing was typed or something is there already
\(a link counting as there)."
  (let ((path (apply #'read-native-path prompt keys)))
    (cond ((null path) (message "No name given") nil)
          ((tuatara.io:path-kind path :follow nil) (message "~a is there already" (abbreviated-path path)) nil)
          (t path))))

;;; Views of files, and visiting them

(defvar *file-views* '()
  "Functions of a file's PATH and a function SHOW of a buffer that may show the
file visited as something other than its text (a PDF as its pages): each
returns the buffer it showed with SHOW, or NIL to leave the file to the next,
and at last to its text. A safe visit shows the text always.")

(defvar *file-view-said* :none
  "Why a view of *FILE-VIEWS* left the file being visited to its octets, while
a visit is made (NIL until one says); :NONE outside a visit.")

(defun file-view-refuses (format &rest arguments)
  "Say why a view of *FILE-VIEWS* leaves the file to its octets (FORMAT with
ARGUMENTS): within a visit, kept to be said with how the file opened (a
binary one safely); elsewhere, at once."
  (let ((words (apply #'format nil format arguments)))
    (if (eq *file-view-said* :none)
        (message "~a" words)
        (setf *file-view-said* words))))

(defun file-view-path (path)
  "The native path of PATH when it is a regular file a view of *FILE-VIEWS*
may show; NIL otherwise."
  (let ((native (ignore-errors (tuatara.io:native (tuatara.io:expand-path path)))))
    (and native (eq :regular (tuatara.io:path-kind native)) native)))

(defun file-view-can-draw-p (native decoder install)
  "True when a program shut in by the sandbox can draw the file NATIVE:
the sandbox is ready (or not known yet: the view says why when it cannot)
and DECODER is true; otherwise said why (FILE-VIEW-REFUSES), INSTALL the
words naming what to install, and NIL."
  (let ((status (tuatara.io:sandbox-status-now)))
    (cond ((not (member status '(:ready :unknown)))
           (file-view-refuses "~a opens as octets: ~a" (tuatara.io:native-file-name native) (no-sandbox-words status))
           nil)
          ((null decoder)
           (file-view-refuses "~a opens as octets: ~a" (tuatara.io:native-file-name native) install)
           nil)
          (t t))))

(defun file-view-show (native views view-path make show)
  "Show the view of the file NATIVE with SHOW, a function of the buffer: the
one among VIEWS (buffers) whose VIEW-PATH (a function of a buffer) is NATIVE,
else the one MAKE (a function of no arguments) makes; the file remembered
among the recent ones. Returns the buffer."
  (let ((buffer (or (find native views :key view-path :test #'equal) (funcall make))))
    (funcall show buffer)
    (remember-file native)
    buffer))

(defun make-file-view-buffer (native mode text what)
  "A new buffer showing the file at NATIVE as WHAT (\"its pages\", \"its
picture\"), in major MODE: read only, shut in, its directory the file's,
TEXT in it but blank until the picture is drawn. \\[hex-mode] in it opens
the file's own octets (FILE-VIEW-HEX)."
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name (tuatara.io:native-file-name native)) :text text)))
    (set-major-mode buffer mode)
    (setf (tuatara.buffer:buffer-local buffer :read-only) t
          (tuatara.buffer:buffer-local buffer :sandboxed) t
          ;; a picture in place of the text, no cursor, even before the first is drawn
          (tuatara.buffer:buffer-local buffer :picture-view) t
          ;; blank while a picture is on its way, never the text in its place
          (tuatara.buffer:buffer-local buffer :picture-pending) t
          (tuatara.buffer:buffer-local buffer :viewed-file) native
          (tuatara.buffer:buffer-local buffer :viewed-as) what
          (tuatara.buffer:buffer-local buffer :directory) (tuatara.io:parent-path native))
    buffer))

(defun reading-buffer (registry view-path native name text mode start &key private-folder)
  "The buffer among those REGISTRY (the symbol of a variable holding a list of
buffers) holds whose VIEW-PATH (a function of a buffer) is NATIVE, the path of
a file; else a new one for that file: called NAME (made unique), saying TEXT
until the file is read, read only, in major MODE, its directory the file's,
added to REGISTRY, and START called with it, to say what it shows and start
reading the file in the background. With PRIVATE-FOLDER, the name of a folder
in the private cache directory (\"palimpsest/\"), the new buffer is given a
folder of its own in it before START (NEW-PRIVATE-FOLDER), which
BUFFER-PRIVATE-FOLDER names and which is deleted when the buffer is killed,
once the buffer's jobs have ended (FORGET-PRIVATE-FOLDER)."
  (or (find-if (lambda (b) (and (live-buffer-p b) (equal native (funcall view-path b)))) (symbol-value registry))
      (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name name) :text text)))
        (set-major-mode buffer mode)
        (setf (tuatara.buffer:buffer-local buffer :read-only) t
              (tuatara.buffer:buffer-local buffer :directory) (tuatara.io:parent-path native)
              (symbol-value registry) (cons buffer (remove-if-not #'live-buffer-p (symbol-value registry))))
        (when private-folder
          (setf (tuatara.buffer:buffer-local buffer 'private-folder)
                (new-private-folder (private-cache-directory private-folder))))
        (funcall start buffer)
        buffer)))

(defun buffer-private-folder (buffer)
  "The folder of BUFFER's own READING-BUFFER made for it in the private cache
directory, a native name ending in /; NIL when it has none."
  (tuatara.buffer:buffer-local buffer 'private-folder))

(defun forget-private-folder (buffer)
  "Kill-buffer function: delete BUFFER's private folder (BUFFER-PRIVATE-FOLDER)
and all in it, in the background, once the jobs of BUFFER, stopped as it goes,
have ended. The folder is still named while the other kill-buffer functions
run."
  (let ((folder (buffer-private-folder buffer))
        (threads (remove nil (mapcar #'tuatara.runtime:job-thread (jobs-of buffer)))))
    (when folder
      (tuatara.runtime:make-guarded-thread
       (lambda ()
         (dolist (thread threads) (sb-thread:join-thread thread :default nil))
         (delete-private-folder folder))
       :name "tuatara private folder deleted"))))

(defun show-reading-result (job path shown &optional (failure "~a could not be read: ~a~%"))
  "Show in the buffer of JOB, a job reading the file at PATH, what was read,
when that buffer is still there: when JOB is done, SHOWN, the text to show
(point put at its start) or a function of the buffer that shows it; else why
the file could not be read, FAILURE given the file's name and the words for
how JOB ended, point at its start."
  (let ((buffer (tuatara.runtime:job-buffer job)))
    (when (live-buffer-p buffer)
      (cond ((not (eq (tuatara.runtime:job-status job) :done))
             (set-made-text buffer (format nil failure (tuatara.io:native-file-name path) (tuatara.runtime:job-ending-words job)))
             (setf (tuatara.buffer:buffer-point buffer) 0))
            ((functionp shown) (funcall shown buffer))
            (t (set-made-text buffer shown)
               (setf (tuatara.buffer:buffer-point buffer) 0))))))

(defun hex-view-on (buffer &optional (at 0))
  "Show BUFFER in the hex view (\\[hex-mode]), its cursor on the octet AT."
  (setf (tuatara.buffer:buffer-local buffer :hex-view) t
        (tuatara.display:hex-point buffer) at))

(defun show-octets-in-hex (name octets &key (show #'pop-to-buffer))
  "Show OCTETS in a buffer of their own called NAME, read only, in the hex
view (\\[hex-mode]), with SHOW, a function of the buffer: in another window
as standard. A buffer of that name already there is replaced. Returns the
buffer."
  (let ((old (tuatara.buffer:find-buffer name)))
    (when old (tuatara.buffer:kill-buffer old)))
  (let ((buffer (tuatara.buffer:make-buffer name :text (coerce octets 'tuatara.text:octets))))
    (setf (tuatara.buffer:buffer-local buffer :read-only) t)
    (hex-view-on buffer)
    (funcall show buffer)
    buffer))

(defun file-view-octets (path what)
  "Open the octets of the file at PATH, shown by a view as WHAT (its pages,
its picture), safely (read-only, nothing acted on), in the hex view, in the
window; the view stays as it was."
  (let ((octets (visit-path path :safe t)))
    (hex-view-on octets)
    (message "~a's octets, opened safely: \\[kill-buffer] or \\[switch-to-buffer] goes back to ~a"
             (tuatara.io:native-file-name path) what)))

(defun file-view-hex (buffer)
  "For \\[hex-mode] in BUFFER, a file shown as its pages or its picture
\(MAKE-FILE-VIEW-BUFFER): open the file's own octets in the hex view
\(FILE-VIEW-OCTETS), the view staying. True when BUFFER is one."
  (let ((native (tuatara.buffer:buffer-local buffer :viewed-file))
        (what (tuatara.buffer:buffer-local buffer :viewed-as)))
    (when (and native what)
      (file-view-octets native what)
      t)))

(defvar *hex-view-binary-files* t
  "True to show a file that looks binary in the hex view (\\[hex-mode]) when it
is opened: one with a NUL octet in its first 64 KB, as executables, archives
and disk images have and text has not. Pictures and PDFs shown as such keep
their views; \\[hex-mode] shows any file as text again. NIL to show them as
text.")

(defun show-binary-in-hex (buffer)
  "Show BUFFER, a file just opened, in the hex view when its file looks binary
(TUATARA.IO:BINARY-HEAD-P of its TUATARA.IO:FILE-HEAD) and
*HEX-VIEW-BINARY-FILES* is true."
  (when (and *hex-view-binary-files* (tuatara.buffer:buffer-file buffer)
             (tuatara.io:binary-head-p (tuatara.io:file-head (tuatara.buffer:buffer-file buffer))))
    (hex-view-on buffer (tuatara.buffer:buffer-point buffer))))

(defun visit-path (path &key safe (show (lambda (buffer) (show-buffer buffer))))
  "Visit the file at PATH, as usual or with SAFE safely (TUATARA.IO:VISIT-FILE),
show its buffer with SHOW, a function of the buffer (in the selected window by
default), and put it first among the recent files. A name that does not exist
becomes a new file, except for a safe visit. Visited as usual, a file one of
*FILE-VIEWS* shows otherwise, a PDF, is shown so; one that looks binary, in
the hex view (*HEX-VIEW-BINARY-FILES*). Why a file opened safely unasked is
said, and a file opened safely goes back to where it was left. A file open
already that was left in INSERT state is shown in NORMAL state when it is
visited from another buffer (*SHOWN-AGAIN-FUNCTIONS*); visited from its own
buffer, going to another place in it, it stays as it is. The file's first
octets are read once for all of this (TUATARA.IO:CALL-WITH-FILE-HEADS).
Returns the buffer, and true as a second value when it was open already."
  (tuatara.io:call-with-file-heads (lambda () (visit-path-once path safe show))))

(defvar *shown-again-functions* '()
  "Functions called with a buffer of a file open already, just shown by a
visit of its file from another buffer (VISIT-PATH). A buffer a command opens
without showing it (a capture's target, a journal's day) has none of them
called.")

(defvar *visiting-safely* nil
  "True while a file or folder is being visited safely, for the views that
show one (*FILE-VIEWS*): a folder's list is then opened safely too.")

(defun visit-path-once (path safe show)
  "Visit the file at PATH as VISIT-PATH does, SAFE and SHOW as it takes them,
its first octets read once meanwhile. A file in one of *SAFE-DIRECTORIES*, or
of evidence (*EVIDENCE-FOLDERS*), is shown by no view (decrypted, read by a
program), however it is opened; a
folder is listed (\\[rimu]) however it is opened, its access time kept and
nothing in it opened."
  (let ((*file-view-said* nil)
        (*visiting-safely* (and safe t)))
    (unless (and (or safe (safe-directory-of path) (evidence-path-p path))
                 (not (eq :directory (ignore-errors (tuatara.io:path-kind (tuatara.io:native (tuatara.io:expand-path path)))))))
      (let ((view (some (lambda (function) (funcall function path show)) *file-views*)))
        (when view (return-from visit-path-once view))))
    (visit-path-as-text path safe show)))

(defun nearest-folder (path)
  "The nearest folder above PATH that is there, a native name ending in /."
  (loop for folder = (tuatara.io:parent-path path) then (tuatara.io:parent-path folder)
        until (or (string= folder "/") (tuatara.io:path-stamp folder))
        finally (return folder)))

(defun say-new-file-folder (buffer)
  "When BUFFER is a new file whose folder is not there yet: say so, and that
saving makes it, the buffer editable when that folder can be made, else why it
is read-only."
  (let* ((path (tuatara.buffer:buffer-file buffer))
         (folder (and path (tuatara.io:parent-path path))))
    (when (and folder (null (tuatara.io:path-stamp path)) (null (tuatara.io:path-stamp folder)))
      (let ((above (nearest-folder path)))
        (if (tuatara.io:writable-p above)
            (progn (setf (tuatara.buffer:buffer-local buffer :read-only) nil)
                   (message "(New file; its folder ~a is not there yet: saving asks to make it)"
                            (escape-controls (abbreviated-path folder))))
            (message "(New file, read-only: its folder ~a is not there, and ~a may not be written)"
                     (escape-controls (abbreviated-path folder)) (escape-controls (abbreviated-path above))))))))

(defun visit-path-as-text (path safe show)
  "Visit the file at PATH as its text (or octets), as VISIT-PATH-ONCE does
when no view of *FILE-VIEWS* shows it, saying why a view left it so
\(*FILE-VIEW-SAID*) with how it opened; called outside a visit, by a view of
its own, it says only how the file opened. A file open already, shown from
another buffer, has *SHOWN-AGAIN-FUNCTIONS* called with it."
  ;; none before the editor has a window (a file named as it starts)
  (let ((from (and *editor* (editor-window *editor*) (current-buffer))))
    (multiple-value-bind (buffer existing) (tuatara.io:visit-file path :safe safe)
      (unless existing (show-binary-in-hex buffer))
      (funcall show buffer)
      (when (and existing (not (eq buffer from)))
        (dolist (function *shown-again-functions*) (funcall function buffer)))
      (when (and safe existing (not (tuatara.io:safe-visit-p buffer)))
        (message "~a is open already, with its mode; kill that buffer to open the file safely"
                 (tuatara.buffer:buffer-name buffer)))
      (when (and (not safe) existing (tuatara.io:safe-visit-p buffer))
        (message "~a is open safely; ~a" (tuatara.buffer:buffer-name buffer) (end-safe-visit-words buffer)))
      (let ((said (and (not (eq *file-view-said* :none)) *file-view-said*)))
        (cond ((and (not existing) (tuatara.io:safe-visit-reason buffer)) (say-why-safe buffer said))
              (said (message "~a" said))
              ((and (not existing) (not safe)) (say-new-file-folder buffer))))
      ;; a file opened safely runs nothing after its visit, but goes back to where it was left
      (when (and (not existing) (tuatara.io:safe-visit-p buffer)) (restore-place buffer))
      ;; a new visit as usual was remembered as it was made (*find-file-functions*)
      (when (or existing (tuatara.io:safe-visit-p buffer)) (remember-buffer-file buffer))
      (values buffer existing))))

;;; Finding, saving and renaming files

(defvar *paths-changed-functions* '()
  "Functions called with a list of native paths just made, renamed, written or
removed by a command, so that the lists showing them read them again.")

(defun paths-changed (paths)
  "Tell each of *PATHS-CHANGED-FUNCTIONS* that the native PATHS were just
made, renamed, written or removed."
  (dolist (function *paths-changed-functions*) (funcall function paths)))

(defvar *file-renamed-functions* '()
  "Functions called with FROM and TO, native paths, once the file FROM is
renamed TO and the buffers visiting it follow it (FOLLOW-RENAMED-FILE).")

(defun follow-renamed-file (from to)
  "Have the buffers visiting FROM visit TO, the same file renamed, named after
it, and tell *FILE-RENAMED-FUNCTIONS*."
  (dolist (buffer tuatara.buffer:*buffers*)
    (when (equal (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path from))
      (let ((old-name (tuatara.io:native-file-name from)))
        (setf (tuatara.buffer:buffer-file buffer) (tuatara.io:expand-path to))
        (when (string= (tuatara.buffer:buffer-name buffer) old-name)
          (setf (tuatara.buffer:buffer-name buffer)
                (tuatara.buffer:unique-buffer-name (tuatara.io:native-file-name to)))))))
  (dolist (function *file-renamed-functions*) (funcall function from to)))

(defun follow-moved-paths (pairs)
  "Have the buffers visiting a file moved by PAIRS, each (FROM . TO) of native
names, a folder's files too, visit it where it went (FOLLOW-RENAMED-FILE)."
  (dolist (buffer tuatara.buffer:*buffers*)
    (let ((file (tuatara.buffer:buffer-file buffer)))
      (when file
        (let ((native (tuatara.io:native file)))
          (loop for (from . to) in pairs
                when (tuatara.io:path-within-p native from)
                  do (follow-renamed-file native (concatenate 'string to (subseq native (length (string-right-trim "/" from)))))
                     (return)))))))

(defun check-not-saving (paths)
  "An error when a buffer is being saved into one of PATHS (or a folder's
files): what it writes would land at the old name once moved."
  (dolist (buffer tuatara.buffer:*buffers*)
    (let ((file (tuatara.buffer:buffer-file buffer)))
      (when (and file (tuatara.io:save-of buffer)
                 (some (lambda (path) (tuatara.io:path-within-p (tuatara.io:native file) path)) paths))
        (error "~a is being saved: try again once it is" (tuatara.buffer:buffer-name buffer))))))

(defun rename-path-following (from to &key (on-disk t) (tell t))
  "Rename the file or folder FROM to TO, native names, the buffers visiting it
following it (FOLLOW-RENAMED-FILE), or those visiting a file in the folder
\(FOLLOW-MOVED-PATHS), and with TELL say so to the lists showing them
\(PATHS-CHANGED). An error, nothing renamed, while a buffer is being saved
into it. With ON-DISK NIL, only the buffers follow: a file not written yet,
or one another program renamed."
  (when on-disk
    (check-not-saving (list from))
    (tuatara.io:rename-path from to))
  (if (eq :directory (tuatara.io:path-kind to :follow nil))
      (follow-moved-paths (list (cons from to)))
      (follow-renamed-file from to))
  (when tell
    (paths-changed (list from to))))

(defun find-file-reading (safe &key (prompt (if safe "Find file safely: " "Find file: "))
                                    (show (lambda (buffer) (show-buffer buffer))))
  "Ask for a file with PROMPT (READ-FILE-PATH) and show it with SHOW, a
function of the buffer (in the selected window by default): visited as usual,
or with SAFE safely (VISIT-PATH), a folder listed so. A name that does not
exist becomes a new file, except for a safe visit. The path is remembered
as the latest chosen (NOTE-CHOSEN-PATH) unless it opened safely."
  (multiple-value-bind (path kind) (read-file-path prompt :preview *preview-files* :note nil)
    (cond ((null path))
          ;; a directory taken as typed: listed in rimu
          ((and (eq kind :directory) (fboundp 'rimu-show-directory))
           (note-visited-path path (funcall 'rimu-show-directory (tuatara.io:native (tuatara.io:expand-path path))
                                            :safe safe)
                              :directory t))
          ;; noted once shown: what opened safely, asked or not, leaves no trace
          (t (note-visited-path path (visit-path path :safe safe :show show))))))

(define-command find-file ()
  "Visit a file, completing its name against the folder being typed.
The question starts in the folder of the path chosen last in a
question for a file or a folder (*LAST-USED-FIRST*), the entry chosen
there last listed first, the others by name;
\\<path-question>\\[minibuffer-home-directory] starts again from ~/. A
folder candidate descends into it; a name that does not exist becomes
a new file. With a prefix argument (\\[universal-argument]
\\[find-file]), open it safely, as find-file-safely does."
  (find-file-reading (and (prefix-argument) t)))

(define-command find-file-safely ()
  "Open a file safely, for looking at a file from a source you do not trust:
read-only, with no major mode, no hooks and no version control, so that
nothing in the file is acted on; its file system left exactly as it was (no
copy beside it), and its access time too where the system allows. The mode
line says Safe, and the files visited lately remember it: it opens safely
from there again. \\[read-only-mode] allows editing it after all;
\\[end-safe-visit] opens it as usual."
  (find-file-reading t))

(define-command end-safe-visit ()
  "Open the file of this buffer, opened safely, as usual from now on, where it
is: the mode line no longer says Safe, the buffer is editable when you may
write the file, and it gets what \\[find-file] gives a file: its major mode
(or keeps the one chosen meanwhile, its colours now shown), version control,
and a place among the files visited lately that opens it as usual. Point,
changes not saved and undo stay as they are. A file of evidence
\(*EVIDENCE-FOLDERS*) stays opened safely."
  (let* ((b (current-buffer))
         (mode (buffer-major-mode b))
         (evidence (buffer-evidence b)))
    (cond
      ((not (tuatara.io:safe-visit-p b))
       (message "~a is not open safely" (tuatara.buffer:buffer-name b)))
      (evidence
       (message "~a stays opened safely: ~a" (tuatara.buffer:buffer-name b)
                (evidence-visit-reason (tuatara.io:native (tuatara.buffer:buffer-file b)))))
      (t
       (let ((chosen (not (eq (type-of mode) 'fundamental-mode))))
         (tuatara.io:end-safe-visit b)
         (when chosen
           ;; the mode stays as it is set up, its language now parsed
           (let ((language (mode-language mode)))
             (when language (start-parsing b language))))
         (dolist (f tuatara.buffer:*find-file-functions*)
           (unless (and chosen (eq f 'set-auto-mode)) (funcall f b)))
         (message "~a is open as usual now~:[~;; read-only, since you may not write it~]"
                  (tuatara.buffer:buffer-name b) (tuatara.buffer:buffer-local b :read-only)))))))

(define-command read-only-mode ()
  "Make the current buffer read-only, or editable again if it is; with a
count, read-only when it is positive, editable when it is zero or less."
  (let* ((b (current-buffer)) (now (switched-by-count (tuatara.buffer:buffer-local b :read-only))))
    (setf (tuatara.buffer:buffer-local b :read-only) now)
    (message "Read-only mode ~:[off~;on~]" now)))

(defvar *after-save-functions* '()
  "Functions called with the buffer after it is saved.")

(defvar *before-save-functions* '()
  "Functions called with the buffer just before it is saved; what they change
is saved with it. One that returns :LATER stops the save: it saves the buffer
itself once ready (SAVE-BUFFER-WITH-CARE).")

(define-command save-buffer ()
  "Save the current buffer to its file, asking for one if it has none, as
\\[find-file] asks: completing the names of the folder being typed, from the
folder of the path chosen last in a question for a path (else the buffer's
own folder, your home folder for the scratch buffers); a name that does not
exist is the new file. The file is written safely (a new copy
renamed over the old, keeping its owner, permissions and labels); a file
changed on disk since it was read is not overwritten without asking; a large
buffer is saved in the background while you carry on."
  (let ((b (current-buffer)))
    (if (tuatara.buffer:buffer-file b)
        (save-buffer-with-care b)
        (multiple-value-bind (path kind) (read-file-path "Save as: " :writing t)
          (cond ((null path) (message "Not saved"))
                (kind (message "~a is a folder: not saved; give the file's name after it" path))
                (t (save-buffer-with-care b :path path)))))))

(define-command write-file ()
  "Write the current buffer to another file, asked for as \\[find-file] asks
\(\\`C-x C-w'), from the folder of the path chosen last in a question for a
path, else the buffer's own folder. The buffer then visits that file, named
after it, in the major mode its name calls for; the file it visited before is
left as it was. A folder alone is the file's
own name in that folder. A file there already is overwritten only when you
say so."
  (let ((b (current-buffer)))
    (multiple-value-bind (path kind) (read-file-path "Write file: " :writing t)
      (let ((path (if (and path kind (tuatara.buffer:buffer-file b))
                      ;; into the folder, under the file's own name
                      (concatenate 'string path (tuatara.io:native-file-name (tuatara.buffer:buffer-file b)))
                      path)))
        (cond ((null path) (message "Not written"))
              ((char= (char path (1- (length path))) #\/)
               (message "~a is a folder: not written; give the file's name after it" path))
              (t (save-buffer-with-care b :path path)))))))

(define-command rename-visited-file ()
  "Give the file the current buffer visits a new name, asked for, its own
offered to change: a name alone keeps it in its folder, a path (~/notes/a.org,
../a.org) moves it there on the same drive. The file itself is renamed, not
written again, and the buffer follows it, named after it, in the mode its
new name calls for; changes not saved stay so. A name there already, or
one another buffer visits, is refused. A buffer with no file is saved under
the name asked for, as \\[write-file] does; a file visited safely is left as
it is."
  (let* ((b (current-buffer))
         (file (tuatara.buffer:buffer-file b)))
    (cond ((null file) (write-file))
          ((tuatara.io:safe-visit-p b) (message "~a is visited safely: its file is left as it is" (tuatara.buffer:buffer-name b)))
          ((tuatara.io:save-of b) (message "~a is being saved: try again once it is" (tuatara.buffer:buffer-name b)))
          (t
           (let* ((from (tuatara.io:native file))
                  (old (tuatara.io:native-file-name from))
                  (new (string-trim " " (read-string (format nil "Rename ~a to: " old) :initial old)))
                  (to (and (plusp (length new))
                           (tuatara.io:native (tuatara.io:expand-path new :directory (tuatara.io:parent-path from)))))
                  (taken (and to (buffer-visiting to))))
             (cond ((or (null to) (string= to from)) (message "Not renamed"))
                   ((char= (char new (1- (length new))) #\/)
                    (message "~a is a folder: not renamed; give the file's name after it" new))
                   ((tuatara.io:path-stamp to :follow nil) (message "~a is there already: not renamed" (abbreviated-path to)))
                   ((and taken (not (eq taken b)))
                    (message "~a is open in ~a: not renamed" (abbreviated-path to) (tuatara.buffer:buffer-name taken)))
                   ((and (encrypted-name-p (tuatara.io:native-file-name to)) (not (encrypted-name-p old)))
                    (message "~a is not renamed: its name would say encrypted, and its text would be there to read"
                             (tuatara.io:native-file-name to)))
                   (t
                    (handler-case
                        (let ((written (tuatara.io:path-stamp from :follow nil)))
                          ;; a file not written yet has only its buffer to rename
                          (rename-path-following from to :on-disk written)
                          (take-mode-of-name b)
                          (if written
                              (message "Renamed to ~a" (abbreviated-path to))
                              (message "~a now visits ~a, not written yet: ~a writes it"
                                       (tuatara.buffer:buffer-name b) (abbreviated-path to)
                                       (substitute-command-keys "\\[save-buffer]"))))
                      (error (e)
                        (message "~a is not renamed: ~a~@[; \\[write-file] writes it there, the old file left as it is~]"
                                 old e (not (equal (tuatara.io:parent-path from) (tuatara.io:parent-path to)))))))))))))
