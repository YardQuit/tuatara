;;;; lisp/files/rename-formatted.lisp --- Files given formatted names, their headers written and saved.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A file already there is given a formatted name (formatted.lisp): its
;;;; identifier kept, or made from when it was last changed, a title and
;;;; keywords asked for, its signature and extension kept. A file of a kind with
;;;; a header (*FORMATTED-FILE-TYPES*) gets the header, or has the title and
;;;; keywords in its header written anew, and is saved at once, so that many
;;;; files renamed one after another need nothing more. A file open with
;;;; changes not saved keeps them: its header is not written, and that is said.
;;;; The buffers visiting a file renamed follow it.
;;;; As Denote's denote-rename-file and denote-rename-file-keywords do.

(in-package #:tuatara.defaults)

(defun file-identifier-time (path)
  "The universal time the file at PATH was last changed."
  (let ((stamp (tuatara.io:path-stamp path)))
    (if stamp
        (mtime-universal-time (tuatara.io:stamp-mtime stamp))
        (get-universal-time))))

(defun formatted-rename-target (path title keywords)
  "The formatted name the file at PATH gets with TITLE and KEYWORDS, as two
values: the new name, and its identifier. Its identifier and signature are
kept; a file with none gets the identifier of when it was last changed, the
next second's when another file in its directory has that one."
  (let ((name (tuatara.io:native-file-name path)))
    (multiple-value-bind (identifier signature) (name-parts name)
      (let* ((identifier (or identifier
                             (unique-identifier (tuatara.io:parent-path path) (file-identifier-time path))))
             (plain (formatted-file-name identifier title keywords (name-extension name))))
        (values (if (and signature (plusp (length signature)))
                    (concatenate 'string identifier "==" signature (subseq plain (length identifier)))
                    plain)
                identifier)))))

(defun header-line-prefixes (style)
  "What begins the lines of a header of STYLE that hold the title and the
keywords, as two values."
  (ecase style
    (:org (values "#+title:" "#+filetags:"))
    ((:text :markdown) (values "title:" "tags:"))))

(defun header-prefix-p (prefix line)
  "True when LINE begins with PREFIX, case ignored (#+TITLE: as #+title:)."
  (and (>= (length line) (length prefix)) (string-equal prefix line :end2 (length prefix))))

(defun text-header-line-p (line)
  "True when LINE can be one of a text file's header: a field (a name and a
colon, as title: and tags:) or a line of dashes under them."
  (let ((colon (position #\: line)))
    (or (and colon (plusp colon) (every (lambda (c) (or (alphanumericp c) (char= c #\_) (char= c #\-))) (subseq line 0 colon)))
        (and (plusp (length line)) (every (lambda (c) (char= c #\-)) (string-trim " " line))))))

(defun header-lines-end (lines style)
  "How many of LINES, a file's first, are its header of STYLE; 0 when it has none."
  (ecase style
    (:org (or (position-if-not (lambda (line) (uiop:string-prefix-p "#+" line)) lines) (length lines)))
    (:text (if (and lines (header-prefix-p "title:" (first lines)))
               (or (position-if-not #'text-header-line-p lines) (length lines))
               0))
    (:markdown (if (and lines (string= "---" (string-trim " " (first lines))))
                   (let ((end (position "---" lines :start 1 :test (lambda (a b) (string= a (string-trim " " b))))))
                     (if end (1+ end) 0))
                   0))))

(defun text-with-header (text type title keywords identifier time)
  "TEXT, a file of TYPE (an entry of *FORMATTED-FILE-TYPES*), with its header
saying TITLE and KEYWORDS: the lines holding them written anew when it has a
header, else the header a new file of TYPE has put before it. Lines ending in
CR LF keep those ends; NIL for TEXT with lines of both ends, left as it is."
  (let ((crlf (search (coerce '(#\Return #\Newline) 'string) text)))
    (cond ((null crlf) (%text-with-header text type title keywords identifier time))
          ;; every line ending in CR LF, or none: else which a new line takes is not known
          ((/= (count #\Newline text) (search-count (coerce '(#\Return #\Newline) 'string) text)) nil)
          (t (let ((plain (%text-with-header (lf-newlines text) type title keywords identifier time)))
               (crlf-newlines plain))))))

(defun %text-with-header (text type title keywords identifier time)
  "TEXT-WITH-HEADER for TEXT whose lines end in line feeds alone."
  (destructuring-bind (name extension style header) type
    (declare (ignore name extension header))
    (let* ((lines (uiop:split-string text :separator '(#\Newline)))
           (end (header-lines-end lines style)))
      (if (zerop end)
          (concatenate 'string (formatted-file-header-text type title keywords identifier time) text)
          (multiple-value-bind (title-prefix keywords-prefix) (header-line-prefixes style)
            (let ((quoted (eq style :markdown))
                  (written-keywords nil))
              (flet ((field (prefix old value)
                       ;; the value where the old one began, the spaces after the prefix kept
                       (let* ((after (subseq old (length prefix)))
                              (gap (subseq after 0 (or (position #\Space after :test-not #'char=) (length after)))))
                         ;; the prefix as the line has it, #+TITLE: kept in capitals
                         (concatenate 'string (subseq old 0 (length prefix)) (if (string= gap "") " " gap) value))))
                (let ((header (loop with in-keywords = nil
                                    for line in (subseq lines 0 end)
                                    ;; a list of keywords a line each under tags: (Markdown's YAML), replaced with them
                                    when (and in-keywords (eq style :markdown)
                                              (uiop:string-prefix-p "-" (string-left-trim '(#\Space #\Tab) line))
                                              (not (string= "---" (string-trim " " line))))
                                      do (progn)
                                    else
                                      collect (progn
                                                (setf in-keywords nil)
                                                (cond ((header-prefix-p title-prefix line)
                                                       (field title-prefix line (if quoted (format nil "~s" title) title)))
                                                      ((header-prefix-p keywords-prefix line)
                                                       (setf written-keywords t in-keywords t)
                                                       (field keywords-prefix line (header-keywords style (name-keywords keywords))))
                                                      (t line))))))
                  ;; a header with no line for the keywords gets one after the title's
                  (unless written-keywords
                    (let ((at (position-if (lambda (line) (header-prefix-p title-prefix line)) header)))
                      (when at
                        (setf header (append (subseq header 0 (1+ at))
                                             (list (concatenate 'string keywords-prefix " " (header-keywords style (name-keywords keywords))))
                                             (subseq header (1+ at)))))))
                  (format nil "~{~a~^~%~}" (append header (subseq lines end)))))))))))

(defparameter *formatted-header-limit* (* 8 1024 1024)
  "The most octets of a file, or of the buffer visiting it, whose header
renaming it writes: it is read and written whole as the command waits, so a
larger one is renamed with its header as it is, and that is said.")

(defun write-formatted-header (path title keywords identifier)
  "Write the header of the file at PATH, when its kind has one, saying TITLE
and KEYWORDS, and save it at once: in the buffer visiting it and saved from
there, or on the disk, of at most *FORMATTED-HEADER-LIMIT* octets either way.
Returns a sentence when it was not written, else NIL."
  (let* ((name (tuatara.io:native-file-name path))
         (type (and (not (encrypted-name-p name)) (formatted-file-type-for (name-extension name))))
         (time (or (identifier-time identifier) (get-universal-time)))
         (buffer (buffer-visiting path)))
    (cond ((null type) nil)
          ((and buffer (tuatara.buffer:buffer-modified-p buffer))
           (format nil "~a has changes not saved: its header was not written" name))
          ((> (if buffer
                  (tuatara.buffer:buffer-size buffer)
                  (or (ignore-errors (tuatara.io:stamp-size (tuatara.io:path-stamp path))) 0))
              *formatted-header-limit*)
           (format nil "~a is larger than ~a (*formatted-header-limit*): its header was not written"
                   (escape-controls name) (octets-words *formatted-header-limit*)))
          (buffer
           (let* ((old (tuatara.buffer:buffer-string buffer))
                  (new (and (text-octets-p old (tuatara.text:rope-octets (tuatara.buffer:buffer-text buffer)))
                            (text-with-header old type title keywords identifier time))))
             (cond ((null new) (format nil "~a is not all text, or its lines end two ways: its header was not written" name))
                   (t (unless (string= old new)
                        (tuatara.buffer:widen-buffer buffer)
                        (tuatara.buffer:buffer-replace buffer 0 (tuatara.buffer:buffer-size buffer) new)
                        (tuatara.io:save-buffer-file buffer))
                      nil))))
          (t
           (let* ((octets (tuatara.io:file-octets path :limit *formatted-header-limit*))
                  (old (ignore-errors (sb-ext:octets-to-string octets :external-format :utf-8)))
                  (new (and old (text-octets-p old octets) (text-with-header old type title keywords identifier time))))
             (cond ((null new) (format nil "~a is not all text, or its lines end two ways: its header was not written" name))
                   (t (unless (string= old new)
                        (tuatara.io:write-text-file (tuatara.text:string->rope new) path))
                      nil)))))))

(defun text-octets-p (text octets)
  "True when TEXT is all of OCTETS, as UTF-8, octet for octet: nothing in them
that is not text, so that writing TEXT back changes nothing else."
  (equalp (tuatara.text:string-octets text) octets))

(defun rename-file-formatted (path title keywords)
  "Give the file at PATH the formatted name TITLE and KEYWORDS make, its header
written and saved when its kind has one; the buffers visiting it follow.
Returns the new path, and as a second value a sentence of what was not done."
  (multiple-value-bind (name identifier) (formatted-rename-target path title keywords)
    ;; renamed first: a header that cannot be written then leaves the name given and says so
    (let ((to (tuatara.io:path-join (tuatara.io:parent-path path) name)))
      (unless (string= (tuatara.io:native path) to)
        (rename-path-following (tuatara.io:native path) to :tell nil))
      (values to (handler-case (write-formatted-header to title keywords identifier)
                   (error (e) (format nil "its header was not written: ~a" e)))))))

(defun rename-formatted-targets ()
  "The files a rename to formatted names works on: the marked ones in a list
of a directory (those of the directory it shows), or the one on its line,
else the current buffer's file. Refused in a buffer opened safely, whose
files stay as they are."
  (let ((buffer (current-buffer)))
    (when (tuatara.io:safe-visit-p buffer)
      (error "~a is opened safely: its files are not renamed" (tuatara.buffer:buffer-name buffer)))
    (or (and (fboundp 'rimu-chosen-paths) (funcall 'rimu-chosen-paths buffer :files-only t :here-only t))
        (let ((file (tuatara.buffer:buffer-file buffer)))
          (and file (list (tuatara.io:native file)))))))

(defun title-for-renaming (path)
  "The title of the file at PATH when it is renamed: offered by
\\[rename-formatted], kept by \\[rename-formatted-keywords]. For a kind of
file with a header, the title its header says, as it is written there; else
the title its formatted name has, in words; else its name without its
extension."
  (let ((name (tuatara.io:native-file-name path)))
    (or (and (not (encrypted-name-p name)) (formatted-file-type-for (name-extension name))
             (note-front-matter path))
        (multiple-value-bind (identifier signature title) (name-parts name)
          (declare (ignore identifier signature))
          (if title
              (substitute #\Space #\- title)
              (file-name-base-of name))))))

(defun rename-paths-formatted (paths title-and-keywords)
  "Give each of PATHS a formatted name (RENAME-FILE-FORMATTED), its title and
keywords the two values TITLE-AND-KEYWORDS, a function of the path, gives;
say how many were renamed and what was not done, and have the lists showing
them read again."
  (let ((done 0) (notes '()) (last nil))
    (dolist (path paths)
      (multiple-value-bind (title keywords) (funcall title-and-keywords path)
        (handler-case
            (multiple-value-bind (to note) (rename-file-formatted path title keywords)
              (incf done) (setf last (tuatara.io:native-file-name to)) (push note notes))
          (error (e) (push (format nil "~a: ~a" (tuatara.io:native-file-name path) e) notes)))))
    (message "~:[~d files renamed~*~;Renamed to ~*~a~]~@[; ~{~a~^; ~}~]"
             (= done 1) done last (reverse (remove nil notes)))
    (paths-changed (mapcar #'tuatara.io:native paths))))

(define-command rename-formatted ()
  "Give a file a formatted name: its identifier kept, or made from when it was
last changed; a title and keywords asked for, the file's own offered; its
extension kept. An Org, text or Markdown file gets the header of its kind, or
the title and keywords in its header written anew, and is saved at once. The
current buffer's file, or in a list of a directory the marked files, one after
another, or the one on the line. No keywords typed keeps those it has. The
buffers visiting a file follow it."
  (let ((paths (rename-formatted-targets)))
    (if (null paths)
        (message "No file to rename here")
        (rename-paths-formatted
         paths
         (lambda (path)
           (let* ((name (tuatara.io:native-file-name path))
                  (title (read-string (format nil "Title of ~a: " name) :initial (title-for-renaming path)))
                  (kept (name-keyword-list name))
                  (keywords (read-keywords (format nil "Keywords of ~a~@[ (nothing typed keeps ~{~a~^, ~})~]: " name kept))))
             (values title (or keywords kept))))))))

(define-command rename-formatted-keywords ()
  "Give files formatted names with other keywords, their titles kept, asked
for once for them all: the current buffer's file, or in a list of a
directory the marked files or the one on the line. Headers are written and
saved as \\[rename-formatted] writes them."
  (let ((paths (rename-formatted-targets)))
    (if (null paths)
        (message "No file to rename here")
        (let ((keywords (read-keywords (format nil "Keywords of ~:[~d files~;~*~a~]: " (= 1 (length paths))
                                               (length paths) (tuatara.io:native-file-name (first paths))))))
          (rename-paths-formatted paths (lambda (path) (values (title-for-renaming path) keywords)))))))
