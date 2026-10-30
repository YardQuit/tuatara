;;;; lisp/files/bookmarks.lisp --- Places in files kept by name, between sessions: Emacs's bookmarks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x bookmark-set keeps where point is in the current file (or the folder a
;;;; directory list shows) under a name; M-x bookmark-jump goes back there, in a
;;;; later session too; M-x bookmark-manager manages them in a list as the buffer
;;;; list does buffers: marks, deleting, renaming. A bookmark keeps the
;;;; text after its place too, so that it is found again after the file was
;;;; changed above it. They live in one file of the state directory
;;;; (bookmarks), shared by sessions side by side, as the recent files are.

(in-package #:tuatara.defaults)

(defvar *bookmarks-kept* 1000
  "How many bookmarks are kept, the latest set or gone to first.")

(defparameter *bookmark-context-length* 32
  "How many characters after a bookmark's place it keeps, to find the place
again once the text before it has changed.")

(defun parse-bookmark-line (line)
  "The bookmark LINE writes, (NAME PATH OFFSET CONTEXT CATEGORIES SAFE), or NIL
for a line that makes no sense. A tab ends each field, and each is escaped
\(ESCAPE-PATH). In order: the octet, the name, the path and the text after
the place; then, when there are any, the categories with a comma between
them, and the word safe for a file opened safely."
  (let ((fields (uiop:split-string line :separator '(#\Tab))))
    (when (and (<= 4 (length fields) 6) (plusp (length (first fields))) (every #'digit-char-p (first fields))
               (< (length (first fields)) 19))
      (destructuring-bind (offset name path context &optional (categories "") (safe "")) fields
        (let ((name (unescape-path name)) (path (unescape-path path)))
          (and (plusp (length name)) (plusp (length path))
               (list name path (parse-integer offset) (unescape-path context)
                     (category-list (unescape-path categories)) (string= safe "safe"))))))))

(defun category-list (text)
  "The categories TEXT names apart by commas, each once, blanks around them
taken away."
  (remove-duplicates (split-choices text) :test #'string= :from-end t))

(defun bookmark-categories (bookmark)
  "The categories BOOKMARK is in, a list of names; none for one in no category."
  (fifth bookmark))

(defun bookmark-line (bookmark)
  "The line of the bookmarks file for BOOKMARK, (NAME PATH OFFSET CONTEXT
CATEGORIES SAFE): the categories and safe last, when it has them."
  (destructuring-bind (name path offset context &optional categories safe) bookmark
    (format nil "~d~c~a~c~a~c~a~:[~2*~;~c~a~]~:[~;~csafe~]" offset #\Tab (escape-path name) #\Tab (escape-path path) #\Tab (escape-path context)
            (or categories safe) #\Tab (escape-path (format nil "~{~a~^,~}" categories))
            safe #\Tab)))

(define-state-list *bookmarks* "bookmarks" 'parse-bookmark-line 'bookmark-line '*bookmarks-kept*
  :header (format nil "# Bookmarks, the latest set or gone to first, one a line. A tab ends each~%~
                       # field: the octet, the name, the file's path, the text after the place, the~%~
                       # categories with a comma between them, and safe for a file opened safely~%~
                       # (\\\\, \\n and \\r stand for \\, a newline and a return).~%")
  ;; a bookmark is known by its name
  :path-of #'first)

(defun bookmark-names ()
  "The names of the bookmarks, the latest first."
  (mapcar #'first (state-list-entries *bookmarks*)))

(defun find-bookmark (name)
  "The bookmark called NAME, or NIL."
  (state-list-find *bookmarks* name))

(defun bookmark-target (buffer)
  "What a bookmark of BUFFER keeps a place in: its file, or the folder a
directory list shows; NIL for a buffer of neither."
  (let ((file (tuatara.buffer:buffer-file buffer)))
    (cond (file (tuatara.io:native file))
          ((buffer-mode-named-p buffer 'rimu-mode) (buffer-directory buffer)))))

(defun character-start (buffer octet)
  "OCTET of BUFFER, or the start of the character it falls inside, as far as
the text goes."
  (tuatara.text:rope-char-start (tuatara.buffer:buffer-text buffer) (min octet (tuatara.buffer:buffer-size buffer))))

(defun bookmark-context (buffer position)
  "The text after POSITION of BUFFER a bookmark keeps: *BOOKMARK-CONTEXT-LENGTH*
characters at most, up to a tab."
  (let* ((start (character-start buffer position))
         (text (tuatara.buffer:buffer-string buffer :start start
                                                    :end (character-start buffer (+ start (* 4 *bookmark-context-length*)))))
         (text (subseq text 0 (min (length text) *bookmark-context-length*))))
    (subseq text 0 (or (position #\Tab text) (length text)))))

(defun bookmark-place (buffer offset context)
  "Where in BUFFER a bookmark of OFFSET and CONTEXT is now: OFFSET (or the start
of the character it falls inside) when the text there still begins with
CONTEXT, else the nearest place within a megabyte that does, else OFFSET
\(within the text)."
  (let* ((size (tuatara.buffer:buffer-size buffer))
         (offset (character-start buffer offset))
         (here (tuatara.buffer:buffer-string buffer :start offset :end (character-start buffer (+ offset (* 4 (length context)))))))
    (if (or (zerop (length context)) (uiop:string-prefix-p context here))
        offset
        (let* ((from (character-start buffer (max 0 (- offset (* 1024 1024)))))
               (to (character-start buffer (min size (+ offset (* 1024 1024)))))
               (text (tuatara.buffer:buffer-string buffer :start from :end to))
               (best nil))
          (loop with byte = from and counted = 0
                for at = (search context text) then (search context text :start2 (1+ at))
                while at
                do (incf byte (tuatara.text:string-octet-length text :start counted :end at))
                   (setf counted at)
                   (when (or (null best) (< (abs (- byte offset)) (abs (- best offset))))
                     (setf best byte)))
          (or best offset)))))

(defvar *bookmark-set-asks-categories* t
  "True for \\[bookmark-set] to ask, after the name, which categories the
bookmark is in (those used already offered, new ones typed); NIL never to ask.
The bookmark manager (\\[bookmark-manager]) sets them too.")

(defun bookmark-category-names ()
  "Every category a bookmark is in, each once, in the order of their names."
  (sort (remove-duplicates (loop for b in (state-list-entries *bookmarks*) append (bookmark-categories b)) :test #'string=)
        #'string-lessp))

(defun read-bookmark-categories (prompt &optional current)
  "Ask with PROMPT for categories, several apart by commas, those used already
offered and new ones typed (READ-SEVERAL), CURRENT, a list, written in first."
  (category-list (format nil "~{~a~^,~}" (read-several prompt #'bookmark-category-names
                                                       :initial (format nil "~{~a, ~}" current)
                                                       :none "no category" :more "no more categories"))))

(define-command bookmark-set ()
  "Keep where point is in the current file (or the folder a directory list
shows and the entry the cursor is on) under a name you give, the file's name
offered, then in the categories you choose, those used already offered, new
ones typed (*BOOKMARK-SET-ASKS-CATEGORIES*): \\[bookmark-jump] goes back there,
in later sessions too, and \\[bookmark-manager] lists it under each of its
categories. A name already given is moved here, its categories kept when none
are chosen."
  (let* ((buffer (current-buffer))
         (target (bookmark-target buffer)))
    (if (null target)
        (message "~a has no file to keep a place in" (tuatara.buffer:buffer-name buffer))
        (let ((name (string-trim " " (substitute #\Space #\Tab
                                                 (let ((offered (tuatara.buffer:buffer-name buffer)))
                                                   (completing-read "Set bookmark: " (bookmark-names)
                                                                    :default offered))))))
          (if (zerop (length name))
              (message "No name: no bookmark set")
              (let* ((had (bookmark-categories (find-bookmark name)))
                     (categories (or (and *bookmark-set-asks-categories*
                                          (read-bookmark-categories (format nil "Categories of ~a: " name)))
                                     had)))
                (state-list-put *bookmarks* (list (list name target (point) (bookmark-context buffer (point)) categories
                                                        (tuatara.io:safe-visit-p buffer))))
                (message "Bookmark ~a set~@[ in ~{~a~^, ~}~]" name categories)))))))

(defun bookmark-kind (bookmark)
  "What BOOKMARK keeps a place in, as the bookmark manager says it: \"file\",
\"folder\" (a directory list's), or \"gone\" when it is no longer there."
  (let ((kind (ignore-errors (tuatara.io:path-kind (second bookmark)))))
    (case kind
      ((nil) "gone")
      (:directory "folder")
      (t "file"))))

(defun go-to-bookmark (bookmark &key other)
  "Visit BOOKMARK's file (in another window with OTHER), safely when it was
set in a file opened safely, and go to its place, found again where the text
has moved it; a folder's in a rimu list of its project there is, moved there
\(*RIMU-REUSE-LISTS*), else in one of its own. It becomes the latest. A file or folder no longer there is
said so, nothing made in its place."
  (destructuring-bind (name path offset context &optional categories safe) bookmark
    (declare (ignore categories))
    (let* ((kind (bookmark-kind bookmark))
           (buffer (or (buffer-visiting path)
                       (and (string= kind "folder") (rimu-show-in-project path :show #'identity))
                       (and (string/= kind "gone")
                            (visit-path path :safe safe :show #'identity)))))
      (if (not (typep buffer 'tuatara.buffer:buffer))
          (message "~a: ~a is no longer there" name (abbreviated-path path))
          (progn (if other (pop-to-buffer buffer :other t) (show-buffer buffer))
                 (goto-shown buffer (bookmark-place buffer offset context))
                 (state-list-put *bookmarks* (list bookmark)))))))

(defun read-bookmark (prompt)
  "A bookmark chosen with PROMPT, each listed with its file; NIL with none kept."
  (let ((all (state-list-entries *bookmarks*)))
    (if (null all)
        (progn (message "No bookmarks (~a sets one)" (substitute-command-keys "\\[bookmark-set]")) nil)
        (let ((chosen (completing-read prompt
                                       (mapcar (lambda (b) (make-candidate (first b) :value b :annotation (abbreviated-path (second b))))
                                               all)
                                       :require-match t)))
          (if (stringp chosen) (find-bookmark chosen) chosen)))))

(define-command bookmark-jump ()
  "Go to a bookmark, chosen by its name (each listed with its file): its file
visited, point where it was set, found again when the text above it changed."
  (let ((bookmark (read-bookmark "Jump to bookmark: ")))
    (when bookmark (go-to-bookmark bookmark))))

(define-command bookmark-delete ()
  "Forget a bookmark, chosen by its name."
  (let ((bookmark (read-bookmark "Delete bookmark: ")))
    (when bookmark
      (state-list-drop *bookmarks* (list (first bookmark)))
      (message "Bookmark ~a deleted" (first bookmark)))))

;;; The bookmark manager

(define-major-mode bookmarks-mode sectioned-list-mode
  "The bookmark manager (\\[bookmark-manager]): a section for each category, then
one for the bookmarks in none, a line each bookmark in it, the latest set or
gone to first, as the buffer list (\\[buffers]) has its buffers.
\\<support>\\[next-line] and \\[previous-line] move, \\<bookmarks-mode>\\[list-visit] or \\`RET' goes to the
bookmark on the line (on a heading, folds or unfolds its section),
\\[list-visit-other] in another window, \\[quit-window] or \\`h' returns;
\\[list-toggle-fold] folds or unfolds the section, \\[list-toggle-all-folds] all of them;
\\[list-mark-entry] marks a bookmark (>), \\[list-mark-for-deletion] marks it for deletion (D),
\\[list-unmark-entry] unmarks it, \\[list-unmark-all] unmarks them all, \\[list-toggle-marks] swaps marked and unmarked; \\[list-delete-marked]
deletes the marked bookmarks (the one on the line when none is) from their
sections after asking, \\[list-execute] those marked for deletion, the sections
holding them unfolded first; a bookmark in another category stays there.
\\[bookmarks-set-categories] says which categories the marked bookmarks or the one on the line
are in, \\[bookmarks-rename] renames one; \\[revert-buffer] lists them again, marks kept, as
the list does by itself while it is shown."
  :lighter "Bookmarks" :interactive nil)

(defvar *bookmarks-keymap* (mode-keymap 'bookmarks-mode)
  "Keys of the bookmark manager (bookmarks-mode's keymap).")

(defparameter *bookmarks-columns* '((" M" :width 2) ("Name" :width 24) ("Kind" :width 8) ("File or folder"))
  "The columns of the bookmark manager, as TABLE-HEADING-PIECES takes them: the
mark (> or D), the name, whether it keeps a place in a file or a folder, and
the file or folder.")

(defun bookmark-name-face (mark kind)
  "The face of a bookmark's name in the bookmark manager: that of its MARK
\(LIST-MARK-FACE) when it has one; else :list-directory for a folder's,
:list-dim for one whose file or folder is gone (its KIND), none for a file's."
  (or (list-mark-face mark)
      (cond ((string= kind "gone") :list-dim)
            ((string= kind "folder") :list-directory))))

(defparameter *no-category* "No category"
  "The title of the bookmark manager's section for the bookmarks in no category.")

(defun bookmark-section-title (section)
  "What the bookmark manager calls SECTION: a category's name as it is, and
*NO-CATEGORY* for :NONE, the section of the bookmarks in none."
  (if (eq section :none) *no-category* section))

(defun bookmark-sections ()
  "The bookmark manager's sections, each (SECTION . BOOKMARKS): one for each
category, SECTION its name, in the order of their names, then one for the
bookmarks in none, SECTION :NONE; in each, the latest set or gone to first.
A bookmark in several categories is in the section of each."
  (let ((all (state-list-entries *bookmarks*)))
    (remove nil
            (append (loop for category in (bookmark-category-names)
                          collect (cons category (remove-if-not (lambda (b) (member category (bookmark-categories b) :test #'string=)) all)))
                    (let ((none (remove-if #'bookmark-categories all)))
                      (and none (list (cons :none none))))))))

(defun bookmark-cells (bookmark mark kind)
  "The cells of the line of BOOKMARK, marked MARK, in the bookmark manager:
its mark, name, kind (KIND, a function of a bookmark) and file."
  (list (list-mark-sign mark)
        (cons (first bookmark) (bookmark-name-face mark (funcall kind bookmark)))
        (funcall kind bookmark)
        (abbreviated-path (second bookmark))))

(defun bookmarks-summary (all kind)
  "The pieces of the bookmark manager's last line: how many bookmarks ALL
holds, how many of them are gone (KIND, a function of a bookmark) and how
many categories there are; with none, how to set one."
  (if all
      (table-row-pieces *bookmarks-columns*
                        (list "" (format nil "~[No bookmarks~;1 bookmark~:;~:*~d bookmarks~]" (length all))
                              ""
                              (format nil "~:d gone, ~[no categories~;1 category~:;~:*~d categories~]"
                                      (count "gone" all :key kind :test #'string=)
                                      (length (bookmark-category-names)))))
      (list (format nil "   No bookmarks: ~a sets one~%" (substitute-command-keys "\\[bookmark-set]")))))

(defun bookmarks-render (list &optional (point :line))
  "Fill LIST, the bookmark manager, with its sections (BOOKMARK-SECTIONS,
SECTIONED-PIECES) and a summing-up line (BOOKMARKS-SUMMARY); marks kept on
those still there, a mark kept by the bookmark's name and its section."
  (let ((kinds (make-hash-table :test 'equal)))
    (flet ((kind (bookmark)
             ;; each file looked at once
             (or (gethash (second bookmark) kinds)
                 (setf (gethash (second bookmark) kinds) (bookmark-kind bookmark)))))
      (render-table-list
       list *bookmarks-columns* (bookmark-sections)
       (lambda (bookmark mark section) (declare (ignore section)) (bookmark-cells bookmark mark #'kind))
       :sections t :section-title #'bookmark-section-title
       :marked-by (lambda (bookmark section) (cons (first bookmark) section))
       :after (cons (string #\Newline) (bookmarks-summary (state-list-entries *bookmarks*) #'kind))
       :point point
       :refill (lambda (list) (bookmarks-render list :keep))))))

(defun bookmarks-key (list)
  "What the bookmark manager LIST shows, cheaply: the bookmarks in their
order; another key means the list is out of date (KEEP-SHOWN-CURRENT)."
  (declare (ignore list))
  (mapcar (lambda (b) (list (first b) (second b) (bookmark-categories b))) (state-list-entries *bookmarks*)))

(keep-shown-current 'bookmarks-mode 'bookmarks-key)

(define-command bookmark-manager ()
  "Manage the bookmarks in a list, as the buffer list (\\[buffers]) manages
buffers, in sections: one for each category (\\[bookmark-set] asks for them,
\\<bookmarks-mode>\\[bookmarks-set-categories] here sets them), its heading saying how many it holds, then
one for the bookmarks in none; a bookmark in two categories is in both. Each
line: its mark (> marked, D marked for deletion), name, whether it keeps a
place in a file or a folder (a rimu list's: the entry the cursor was on), and
the file or folder; the names of folders, of bookmarks whose file or folder
is gone and of those marked each set apart in a face of their own.
\\[list-toggle-fold] or \\`RET' on a heading folds or unfolds a section,
\\[list-toggle-all-folds] all of them. \\[list-visit] or \\`RET' goes to the bookmark on the line,
\\[list-visit-other] in another window; \\[list-mark-entry] marks it, \\[list-mark-for-deletion] marks it for deletion,
\\[list-unmark-entry] and \\[list-unmark-all] unmark, \\[list-toggle-marks] swaps marked and unmarked, a count marking as many; \\[list-delete-marked] deletes the
marked ones after asking, \\[list-execute] those marked for deletion, each from its
section only: deleted from category a, a bookmark stays in category b, and
goes only when it is in no other. \\[bookmarks-rename] renames one."
  (let ((list (buffer-in-mode "*Bookmarks*" 'bookmarks-mode)))
    (show-buffer list)
    (bookmarks-render list 3)))

(defmethod list-entry-noun ((mode bookmarks-mode))
  "A bookmark."
  "bookmark")

(defmethod visit-entry ((mode bookmarks-mode) list bookmark other)
  "Go to BOOKMARK, in another window with OTHER."
  (declare (ignore list))
  (go-to-bookmark bookmark :other other))

(defun bookmarks-delete (list places)
  "Delete the bookmarks PLACES name, each (NAME . SECTION), after asking, the
sections holding them unfolded first, and make LIST again: from that category
only, the bookmark staying in its others; the bookmark itself when it is in
no other, or in the section of those in none."
  (unfold-list-sections list (mapcar #'cdr places))
  (when (yes-or-no-p* (format nil "~a?" (list-question "Delete" "bookmark" places
                                                       :name-of (lambda (place)
                                                                  (format nil "~a~:[ from ~a~;~*~]" (car place)
                                                                          (eq (cdr place) :none) (cdr place))))))
    (let ((gone 0))
      (loop for (name . section) in places
            for bookmark = (find-bookmark name)
            when bookmark
              do (let ((left (remove section (bookmark-categories bookmark) :test #'equal)))
                   (if (or (eq section :none) (null left))
                       (progn (state-list-drop *bookmarks* (list name)) (incf gone))
                       (set-bookmark-categories bookmark left))))
      (bookmarks-render list)
      (message "~d deleted~[~:;, ~:*~d of them from a category only~]" (length places) (- (length places) gone)))))

(defun set-bookmark-categories (bookmark categories)
  "Keep BOOKMARK in CATEGORIES, where it stands among the bookmarks."
  (state-list-replace *bookmarks* (list (append (subseq bookmark 0 4) (list categories (sixth bookmark))))))

(defmethod delete-entries ((mode bookmarks-mode) list places mark)
  "Delete the bookmarks from their sections, after asking, the sections holding
them unfolded first: a bookmark in several categories stays in the others;
one in no other is deleted (BOOKMARKS-DELETE)."
  (declare (ignore mark))
  (bookmarks-delete list places))

(define-command bookmarks-set-categories ()
  "Say which categories the bookmark on the current line is in, or the marked
ones (>) are, the sections holding them unfolded first: those used already
offered, new ones typed, several apart by commas; the bookmark's own written
in first, to change. Each is then listed under each of its categories."
  (let* ((list (current-buffer))
         (places (list-acted-on list))
         (bookmarks (remove nil (mapcar #'find-bookmark (remove-duplicates (mapcar #'car places) :test #'string=)))))
    (if (null bookmarks)
        (message "No bookmark on this line")
        (let ((categories (progn (unfold-list-sections list (mapcar #'cdr places))
                                 (read-bookmark-categories (if (rest bookmarks)
                                                               (format nil "Categories of ~d bookmarks: " (length bookmarks))
                                                               (format nil "Categories of ~a: " (first (first bookmarks))))
                                                           (and (null (rest bookmarks)) (bookmark-categories (first bookmarks)))))))
          (dolist (bookmark bookmarks)
            (set-bookmark-categories bookmark categories))
          (unmark-list list)
          (message "~:[In no category~;~:*In ~{~a~^, ~}~]" categories)))))

(define-command bookmarks-rename ()
  "Give the bookmark on the current line another name, in every section it is
in; a name another bookmark has already is taken from it after asking."
  (let ((bookmark (list-member-at (current-buffer))))
    (if (null bookmark)
        (message "No bookmark on this line")
        (let ((name (string-trim " " (substitute #\Space #\Tab
                                                 (read-string (format nil "Rename ~a to: " (first bookmark))
                                                              :initial (first bookmark) :select-initial t)))))
          (when (and (plusp (length name)) (string/= name (first bookmark))
                     (or (null (find-bookmark name))
                         (yes-or-no-p* (format nil "~a is the name of another bookmark: replace that one?" name))))
            (state-list-drop *bookmarks* (list (first bookmark)))
            (state-list-put *bookmarks* (list (cons name (rest bookmark))))
            (bookmarks-render (current-buffer))
            (message "Bookmark ~a is now ~a" (first bookmark) name))))))

(defun install-bookmarks ()
  "Bind the bookmarks' keys under \\`C-x m': \\`C-x m m' sets one, \\`C-x m b'
goes to one, \\`C-x m l' manages them (\\[bookmark-manager]), \\`C-x m d' deletes one;
and the manager's own."
  (define-keys *global-keymap* '(("C-x m m" bookmark-set) ("C-x m b" bookmark-jump) ("C-x m l" bookmark-manager)
                                 ("C-x m d" bookmark-delete)))
  (define-keys *bookmarks-keymap* '(("RET" list-visit) ("l" list-visit) ("o" list-visit-other) ("h" quit-window)
                                    ("r" bookmarks-rename) ("c" bookmarks-set-categories))))

(commands-for-mode 'bookmarks-mode "the bookmark manager (\\[bookmark-manager])"
                   'list-visit 'list-visit-other 'bookmarks-rename
                   'bookmarks-set-categories)
