;;;; lisp/org/refile.lisp --- An entry moved under another heading: org-refile, and entries filed under a heading.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As Emacs's org-refile: the heading point is under, with all under it, moved
;;;; to the end of what is under another heading, chosen by its path
;;;; (agenda.org/Commitments and Jobs/...) among the headings of the agenda
;;;; files and this file, its levels put one below. Capture files its entries
;;;; the same way (capture.lisp).

(in-package #:tuatara.defaults)

(defvar *org-refile-targets* '((:agenda-files . 3) (:this-file . 3))
  "Where \\[org-refile] offers to move an entry: a list of (WHERE . LEVELS),
WHERE :agenda-files (*ORG-AGENDA-FILES*), :this-file, or a file's name,
LEVELS the deepest headings offered (3: down to ***).")

;;; Entries put under a heading

(defun org-shift-levels (text by)
  "TEXT with each of its headings BY levels deeper (shallower when negative,
never above the first)."
  (if (zerop by)
      text
      (cl-ppcre:regex-replace-all "(?m)^(\\*+) " text
                                  (lambda (match stars)
                                    (declare (ignore match))
                                    (format nil "~a " (make-string (max 1 (+ (length stars) by)) :initial-element #\*)))
                                  :simple-calls t)))

(defun org-tags-aligned (text)
  "TEXT with the tags of its first line, when it is a heading, put at
*ORG-TAGS-COLUMN*."
  (let* ((end (or (position #\Newline text) (length text)))
         (line (subseq text 0 end)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(\\*+ .*?)[ \\t]+(:[\\w@#%:]+:)[ \\t]*$" line)
      (if (not match)
          text
          (concatenate 'string
                       (org-headline-with-tags (aref groups 0) (org-tags-from-text (aref groups 1)))
                       (subseq text end))))))

(defun org-top-level (text)
  "The level of the shallowest heading of TEXT, or NIL when it has none."
  (let ((top nil))
    (cl-ppcre:do-register-groups (stars) ("(?m)^(\\*+) " text)
      (setf top (if top (min top (length stars)) (length stars))))
    top))

(defun org-insert-entry (buffer entry &key headline title)
  "Put ENTRY, Org text, into BUFFER at the end of what is under HEADLINE (a
headline of its parse now), its headings put so that the shallowest is a
level below it; with no HEADLINE, at the end, under a heading TITLE made
there when given, else as they are. One change to undo. Returns where the entry
begins and where it ends (octets), two values."
  (multiple-value-bind (tree chars) (org-tree-now buffer)
    (declare (ignore tree))
    (let* ((level (cond (headline (org-property headline :level)) (title 1) (t nil)))
           (top (org-top-level entry))
           (entry (org-tags-aligned (org-shift-levels (org-ensure-newline entry)
                                                      (if (and level top) (- (1+ level) top) 0))))
           (size (length chars))
           (newline (if (and (plusp size) (char/= (char chars (1- size)) #\Newline)) (string #\Newline) "")))
      ;; BEFORE goes in first at AT, then the entry
      (flet ((put (at before entry)
               (org-replace-chars buffer at at (concatenate 'string before entry))
               (values (org-char->byte buffer (+ at (length before)))
                       (org-char->byte buffer (+ at (length before) (length entry))))))
        (cond
          (headline
           (put (or (cdr (org-fold-range headline chars)) (string-line-end chars (org-node-begin headline)))
                (string #\Newline) (string-right-trim '(#\Newline) entry)))
          (title (put size (format nil "~a* ~a~%" newline title) entry))
          (t (put size newline entry)))))))

;;; Headings by their paths

(defun org-headline-path (headline)
  "The titles from the top heading down to HEADLINE, a list."
  (loop for node = headline then (org-node-parent node)
        while (and node (eq (org-node-type node) 'headline))
        collect (org-property node :raw-value) into titles
        finally (return (nreverse titles))))

(defun org-headline-at-path (tree path &optional at)
  "The headline of TREE whose titles from the top are PATH, or NIL; of
several with those titles, the one beginning at the character AT when given,
else the first."
  (let ((nodes (list tree)))
    (dolist (title path)
      (setf nodes (loop for node in nodes
                        append (remove-if-not (lambda (child) (string= (org-property child :raw-value) title))
                                              (org-child-headlines node))))
      (unless nodes (return-from org-headline-at-path nil)))
    (or (and at (find at nodes :key #'org-node-begin)) (first nodes))))

(defun org-refile-files (buffer)
  "The files *ORG-REFILE-TARGETS* offers headings of, from BUFFER, each (PATH . LEVELS), each once."
  (let ((files '()))
    (flet ((add (path levels)
             (when path
               (let ((old (assoc path files :test #'string=)))
                 (if old (setf (cdr old) (max (cdr old) levels)) (push (cons path levels) files))))))
      (loop for (where . levels) in *org-refile-targets*
            do (case where
                 (:agenda-files (dolist (p (ignore-errors (org-agenda-file-paths))) (add p levels)))
                 (:this-file (add (and (tuatara.buffer:buffer-file buffer) (tuatara.io:native (tuatara.buffer:buffer-file buffer))) levels))
                 (t (add (tuatara.io:native (tuatara.io:expand-path where)) levels)))))
    (nreverse files)))

(defun org-file-headings (path)
  "The headings of the Org file at PATH, each (LEVEL TITLES POSITION): its
level, its titles from the top heading down (its path), the character it
begins at. An agenda file's are those the agenda keeps (ORG-AGENDA-ENTRIES),
read again only when it changed and never opened for it; another's from its
buffer's parse."
  (if (member path (ignore-errors (org-agenda-file-paths)) :test #'string=)
      (let ((stack '()))
        (loop for entry in (org-agenda-entries path)
              when (eq (agenda-entry-kind entry) :headline)
                collect (let ((level (agenda-entry-level entry)))
                          (loop while (and stack (>= (car (first stack)) level)) do (pop stack))
                          (push (cons level (agenda-entry-title entry)) stack)
                          (list level (reverse (mapcar #'cdr stack)) (agenda-entry-position entry)))))
      (let ((target (ignore-errors (tuatara.io:visit-file path))))
        (when target
          (let ((tree (org-tree-now target)))
            (loop for headline in (and tree (org-headlines tree))
                  collect (list (org-property headline :level) (org-headline-path headline) (org-node-begin headline))))))))

(defun org-refile-candidates (buffer &key exclude)
  "Completion candidates for the headings *ORG-REFILE-TARGETS* offers from
BUFFER (ORG-FILE-HEADINGS), each shown as its file's name and path
(agenda.org/Commitments and Jobs), its value ((:olp FILE TITLE ...) POSITION);
those of EXCLUDE (BUFFER START END, octets) left out."
  (loop for (path . levels) in (org-refile-files buffer)
        for excluded = (and exclude (let ((file (tuatara.buffer:buffer-file (first exclude))))
                                      (and file (string= path (tuatara.io:native file)))))
        append (loop for (level olp position) in (org-file-headings path)
                     when (and (<= level levels)
                               (not (and excluded
                                         (<= (second exclude) (org-char->byte (first exclude) position))
                                         (< (org-char->byte (first exclude) position) (third exclude)))))
                       collect (make-candidate (format nil "~a/~{~a~^/~}" (tuatara.io:native-file-name path) olp)
                                               :value (list (list* :olp path olp) position)))))

(defun org-read-refile-target (prompt buffer &key exclude)
  "Ask with PROMPT for a heading *ORG-REFILE-TARGETS* offers from BUFFER; its
target (:olp FILE TITLE ...), and as a second value the character it begins
at, which tells it from another of the same titles."
  (let* ((candidates (org-refile-candidates buffer :exclude exclude))
         (chosen (completing-read prompt candidates :require-match t)))
    (if (and (consp chosen) (consp (first chosen)) (eq (first (first chosen)) :olp))
        (values (first chosen) (second chosen))
        (error "No heading chosen"))))

(defun org-olp-text (target)
  "TARGET (:olp FILE TITLE ...) in words: agenda.org/Commitments and Jobs."
  (format nil "~a/~{~a~^/~}" (tuatara.io:native-file-name (second target)) (cddr target)))

;;; Refiling

(defun org-clock-in-range-p (buffer start end)
  "True when the running clock's line is in BUFFER between the octets START and END."
  (and *org-clock* (eq (org-clock-buffer *org-clock*) buffer)
       (<= start (tuatara.text:anchor-position (org-clock-line *org-clock*) (tuatara.buffer:buffer-version buffer)) (1- end))))

(defun org-clock-follow (buffer start end)
  "Point the running clock at its CLOCK: line in BUFFER between the octets
START and END, where an entry moved with it went."
  (let* ((text (tuatara.buffer:buffer-string buffer :start start :end (min end (tuatara.buffer:buffer-size buffer))))
         (at (cl-ppcre:scan "(?m)^[ \\t]*CLOCK: \\[[^]]*\\][ \\t]*$" text)))
    (when at
      (org-clock-hold *org-clock* buffer (+ start (tuatara.text:string-octet-length (subseq text 0 at)))))))

(defun org-refile-subtree (buffer headline target &optional target-at)
  "Move HEADLINE of BUFFER (a headline of its parse now), with all under it,
to the end of what is under TARGET (:olp FILE TITLE ...; of several with
those titles, the one beginning at the character TARGET-AT), its levels put
one below; a running clock in it goes with it. The target is found before
anything is taken away, and what was taken away is put back when it cannot
be filed. Both files saved as filing saves them."
  (check-writable buffer)
  (multiple-value-bind (tree chars) (org-tree-now buffer)
    (declare (ignore tree))
    (let* ((begin (org-node-begin headline))
           (last (or (cdr (org-fold-range headline chars)) (string-line-end chars begin)))
           (end (min (length chars) (1+ last)))
           (text (subseq chars begin last))
           (clocked (org-clock-in-range-p buffer (org-char->byte buffer begin) (org-char->byte buffer end)))
           (into (tuatara.io:visit-file (second target)))
           (was-modified (tuatara.io:changes-unsaved-p buffer))
           (into-was-modified (tuatara.io:changes-unsaved-p into)))
      (check-writable into)
      (let* ((under (or (org-headline-at-path (org-tree-now into) (cddr target) target-at)
                        (error "~a is no longer there" (org-olp-text target))))
             ;; where the target is, held while the entry is taken out above it
             (anchor (tuatara.text:make-anchor (tuatara.buffer:buffer-version into)
                                               (org-char->byte into (org-node-begin under)) :bias :after))
             (filed nil))
        (org-replace-chars buffer begin end "")
        (unwind-protect
             (let* ((under-at (org-byte->char into (tuatara.text:anchor-position anchor (tuatara.buffer:buffer-version into))))
                    (under (or (org-headline-at-path (org-tree-now into) (cddr target) under-at)
                               (error "~a is no longer there" (org-olp-text target)))))
               (multiple-value-bind (at to) (org-insert-entry into text :headline under)
                 (setf filed t)
                 (when clocked (org-clock-follow into at to))))
          (unless filed
            (org-replace-chars buffer begin begin (subseq chars begin end)))))
      (org-save-changed buffer was-modified)
      (unless (eq into buffer) (org-save-changed into into-was-modified)))))

(define-command org-refile ()
  "Move the heading point is under, with all under it, to another heading:
chosen by its path (agenda.org/Commitments and Jobs) among the headings
*ORG-REFILE-TARGETS* offers, put at the end of what is under it, a level
below; a clock running in it goes on there. Both files are saved as the
agenda saves its files. With \\[universal-argument], go to the heading chosen instead."
  (let ((buffer (current-buffer)))
    (if (prefix-argument)
        (multiple-value-bind (target at) (org-read-refile-target "Go to heading: " buffer)
          (let ((into (visit-path (second target))))
            (let ((under (org-headline-at-path (org-tree-now into) (cddr target) at)))
              (when under
                (goto-shown into (org-char->byte into (org-node-begin under)))))))
        (multiple-value-bind (tree chars) (org-tree-now buffer)
          (declare (ignore chars))
          (let ((headline (and tree (org-current-headline tree (org-point-char buffer)))))
            (unless headline (error "Not under a headline: nothing to refile"))
            (multiple-value-bind (target at)
                (org-read-refile-target
                 (format nil "Refile “~a” to: " (org-property headline :raw-value)) buffer
                 :exclude (list buffer (org-char->byte buffer (org-node-begin headline))
                                (org-char->byte buffer (org-node-end headline))))
              (org-refile-subtree buffer headline target at)
              (message "Refiled to ~a" (org-olp-text target))))))))

(define-command org-agenda-refile ()
  "Move the entry on the agenda's line under another heading, as
\\<org-mode>\\[org-refile] moves one. In an Org file, the heading point is under."
  (org-agenda-act 'org-refile :elsewhere t))
