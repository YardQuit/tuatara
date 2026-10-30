;;;; lisp/files/paths-in-text.lisp --- Paths in the text: completed as typed, opened from the cursor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Typing a slash in a path in a buffer (/etc/, ~/notes/, ./src/) lists the
;;;; entries of its directory beside the cursor, an idea taken from Helix's
;;;; path completion. The directory is read on a thread of its own, so a
;;;; slow drive never holds typing up, and the list opens when it is read, if
;;;; the path is still being typed. The list is the one completion at point
;;;; shows (COMPLETE-IN-TEXT), its candidates those of the questions for a file
;;;; (DIRECTORY-CANDIDATES). C-x C-p opens the path at the cursor as C-x C-f
;;;; would: a folder in a list of it, a file in its buffer.

(in-package #:tuatara.defaults)

(defvar *complete-paths* t
  "True to list the entries of a directory beside the cursor when a slash is
typed in a buffer after its name: a path is a word with a slash in it (/etc/,
~/notes/, ./src/, or a file: link's), one not starting with / or ~ taken from
the directory the buffer stands in. The list opens with nothing chosen; typing
narrows it, \\<popup>\\[popup-next] and \\[popup-previous] choose, \\[popup-insert] puts the chosen one in (the first
when none is chosen), and so does \\`RET' once one is chosen; a directory put
in lists its own entries in turn. NIL to list nothing as paths are typed.
*COMPLETE-PATHS-MODES* says otherwise for the major modes it names.")

(defvar *complete-paths-modes* '()
  "Major modes whose buffers list a path's directory as it is typed, or not,
whatever *COMPLETE-PATHS* says: a list of (MODE . ON), ON true or NIL, MODE a
major mode's name, with or without -mode, for example ((org-mode . nil)
\\(prog-mode . t)). A mode counts for the modes made from it, the nearest one
listed deciding.")

(defun complete-paths-p (buffer)
  "True when typing a path in BUFFER lists its directory: as
*COMPLETE-PATHS-MODES* says of its major mode or the nearest mode that mode
is made from, else as *COMPLETE-PATHS* says."
  (buffer-setting buffer :complete-paths (setting-list '*complete-paths-modes* *complete-paths-modes* :pairs t)
                  *complete-paths*))

(defparameter *path-delimiters* "\"'`()[]{}<>,;|&="
  "The characters that end a path being typed, besides blanks: those that
stand around a path in a program or in prose.")

(defun path-part-p (char)
  "True when CHAR can be part of a path being typed: not a blank, nor one of
*PATH-DELIMITERS*."
  (not (or (member char '(#\Space #\Tab #\Newline #\Return #\Page))
           (find char *path-delimiters*))))

(defun path-name-part-p (char)
  "True when CHAR can be part of the name a path's list completes: a part of
a path (PATH-PART-P) other than a slash."
  (and (char/= char #\/) (path-part-p char)))

(defun path-at-point (buffer position)
  "The path being typed at POSITION of BUFFER, as four values: where it
starts, where the name after its last slash starts and ends (octets), and its
directory part, up to that slash, as typed; NIL when the word there has no
slash before POSITION. A file: link's path is the part after file:."
  (let ((start (word-bounds-at buffer position #'path-part-p *popup-reach*)))
    (when (< start position)
      (let* ((typed (tuatara.buffer:buffer-string buffer :start start :end position))
             (skip (if (and (> (length typed) 5) (string= "file:" typed :end2 5)) 5 0))
             (slash (position #\/ typed :from-end t :start skip)))
        (when slash
          (let ((name-start (+ start (tuatara.text:string-octet-length typed :end (1+ slash)))))
            (values (+ start (tuatara.text:string-octet-length typed :end skip))
                    name-start
                    (nth-value 1 (word-bounds-at buffer name-start #'path-name-part-p *popup-reach*))
                    (subseq typed skip (1+ slash)))))))))

(defun path-directory (directory buffer)
  "The directory DIRECTORY, the directory part of a path typed in BUFFER,
names: a leading ~ is the home directory, and one not starting with / or ~ is
taken from the directory BUFFER stands in (BUFFER-DIRECTORY)."
  (tuatara.io:expand-path directory :directory (buffer-directory buffer)))

(defvar *path-listing* nil
  "The directory being read for the path typed, while it is: (BUFFER START
DIRECTORY GATHERING), the buffer and where in it the path starts, its
directory part as typed, and what reads it (START-GATHERING).")

(defun start-path-listing (editor)
  "After a command that typed a slash, or put in a directory from a list
beside the cursor, in a path being typed (PATH-AT-POINT): read the path's
directory on a thread of its own, for PATH-COMPLETION-TICK to list it once
read. Nothing is done where COMPLETE-PATHS-P says not, the buffer is
read-only or shown as a hex dump, or a list is shown already."
  (let ((buffer (current-buffer)))
    (when (and (member (editor-last-command editor) '(self-insert-command popup-insert))
               (null (editor-popup editor))
               (not (editor-minibuffer-active editor))
               (eql (char-before-position buffer (tuatara.buffer:buffer-point buffer)) #\/)
               (not (tuatara.buffer:buffer-local buffer :read-only))
               (not (tuatara.display:hex-view-p buffer))
               (complete-paths-p buffer))
      (multiple-value-bind (start name-start name-end directory) (path-at-point buffer (tuatara.buffer:buffer-point buffer))
        (declare (ignore name-start name-end))
        (let ((expanded (and start (ignore-errors (path-directory directory buffer))))
              (chosen (and *last-used-first* (chosen-paths))))
          (when expanded
            (when *path-listing* (stop-gathering (fourth *path-listing*)))
            (setf *path-listing*
                  (list buffer start directory
                        (start-gathering "tuatara path completion"
                                         (lambda (give cancel)
                                           (declare (ignore cancel))
                                           (funcall give (path-listing-order
                                                          (ignore-errors (directory-candidates expanded chosen))))))))))))))

(defun path-listing-order (candidates)
  "CANDIDATES, the entries of a folder as DIRECTORY-CANDIDATES gives them,
ranked for the list beside the cursor: among those that match as closely,
the folders first (those chosen lately first), then the files chosen
lately, then the others."
  (mapcar (lambda (c)
            (let ((folder (uiop:string-suffix-p (candidate-string c) "/"))
                  (rank (candidate-rank c)))
              (make-candidate (candidate-string c) :annotation (candidate-annotation c) :value (candidate-value c)
                                                   :group (candidate-group c) :columns (candidate-columns c)
                                                   :suffix (candidate-suffix c)
                                                   :rank (cond (folder (or rank 500000))
                                                               (rank (+ 1000000 rank))))))
          candidates))

(defun path-completion-tick (editor)
  "Tick function: once the directory of the path typed is read, list its
entries beside the cursor, if that path is still being typed there and no
other list is shown; true while it is being read."
  (let ((listing *path-listing*))
    (when listing
      (destructuring-bind (buffer start directory gathering) listing
        (multiple-value-bind (found done) (take-gathered gathering)
          (if (not done)
              t
              (let ((window (editor-window editor)))
                (setf *path-listing* nil)
                (when (and (first found) window
                           (eq (tuatara.display:window-buffer window) buffer)
                           (null (editor-popup editor))
                           (not (editor-minibuffer-active editor)))
                  (multiple-value-bind (now name-start name-end now-directory)
                      (path-at-point buffer (tuatara.buffer:buffer-point buffer))
                    (when (and (eql now start) (equal now-directory directory))
                      (complete-in-text buffer name-start name-end (first found)
                                        :constituent #'path-name-part-p :auto t))))
                nil)))))))

;;; Opening the path at the cursor

(defun path-around (buffer position)
  "The path in the text around POSITION of BUFFER, as typed: the word there
made of what a path is made of (PATH-PART-P), a file: link's without its
file:; NIL when there is none."
  (multiple-value-bind (start end) (word-bounds-at buffer position #'path-part-p *popup-reach*)
    (when (< start end)
      (let ((word (tuatara.buffer:buffer-string buffer :start start :end end)))
        (if (and (> (length word) 5) (string= "file:" word :end2 5)) (subseq word 5) word)))))

(defun existing-path (typed buffer)
  "The native name of what TYPED, a path in the text of BUFFER, names, with
no . or .. in it (TUATARA.IO:CLEAN-PATH): taken from the directory BUFFER
stands in when it does not start with / or ~ (PATH-DIRECTORY), or without the
full stop, comma, colon or the like that ends a sentence after it; NIL when
nothing is there."
  (flet ((there (path)
           (let ((native (ignore-errors (tuatara.io:clean-path (tuatara.io:native (path-directory path buffer))))))
             (and native (ignore-errors (tuatara.io:path-kind native)) native))))
    (or (there typed)
        (let ((trimmed (string-right-trim ".,:;!?" typed)))
          (and (plusp (length trimmed)) (string/= trimmed typed) (there trimmed))))))

(define-command open-path-at-point ()
  "Open the file or folder whose path is at the cursor in the text, as
\\[find-file] opens it: a folder (/etc/, ~/notes/) in a list of it, a file
\\(~/notes/todo.org, ./README.org, or a file: link's) in its buffer. A path not
starting with / or ~ is taken from the directory the buffer stands in. With a
prefix argument (\\[universal-argument] \\[open-path-at-point]), open it safely, as
\\[find-file-safely] does. A path that names nothing is said, not made. The
path is remembered as chosen lately (NOTE-VISITED-PATH) unless it opened
safely or the buffer it is written in is open safely."
  (let* ((b (current-buffer))
         (typed (path-around b (tuatara.buffer:buffer-point b)))
         (native (and typed (existing-path typed b))))
    (cond ((null typed) (message "No path at point"))
          ((null native) (message "No file or folder: ~a" typed))
          (t (let ((buffer (visit-path native :safe (and (prefix-argument) t))))
               ;; a path written in a file opened safely is evidence too: not remembered
               (unless (tuatara.io:safe-visit-p b)
                 (note-visited-path native buffer
                                    :directory (eq :directory (ignore-errors (tuatara.io:path-kind native))))))))))

(defun install-paths-in-text (editor)
  "List the entries of a path's directory beside the cursor as the path is
typed in a buffer (*COMPLETE-PATHS*, *COMPLETE-PATHS-MODES*), and bind
\\`C-x C-p' to \\[open-path-at-point]."
  (pushnew 'start-path-listing (editor-post-command-functions editor))
  (pushnew 'path-completion-tick *tick-functions*)
  (define-key *global-keymap* "C-x C-p" 'open-path-at-point))
