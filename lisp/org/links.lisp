;;;; lisp/org/links.lisp --- Org's links followed: RET on one, as org-return-follows-link, and C-c C-o.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A link in an Org buffer is followed as Org's org-open-at-point follows
;;;; it. One to a headline (*Title), a custom ID (#id), a target or a name in
;;;; the buffer goes there. A file: link visits the file (with ::*Heading or
;;;; ::text, there in it). An id: link goes to the headline with that ID, here
;;;; or in the Org files of your notes (*formatted-file-directory*). A denote:
;;;; link visits the note with that identifier. A denote:query-filenames: link
;;;; lists the notes whose names match (match-denote-files-link inserts one).
;;;; A formatted-match: link does so by a regular expression
;;;; (match-formatted-files-link). A web address or a mail address is handed to
;;;; the desktop's program after asking, as it runs outside Tuatara.

(in-package #:tuatara.defaults)

(define-setting *org-return-follows-link* t
  "Whether \\`RET' on an Org link follows it.
\\[org-open-at-point] follows one always.
- t: \\`RET' follows the link.
- nil: \\`RET' does what it does elsewhere."
  :values boolean)

(defun org-links-descriptive-p (buffer)
  "True when BUFFER, an Org buffer, shows its links as their labels."
  (not (tuatara.buffer:buffer-local buffer 'org-links-literal)))

(defun org-hidden-chars (tree from to)
  "The parts of TREE from character FROM below TO hidden while links are shown
as their labels, each (START END TEXT), characters in order, TEXT what is
shown in their place: of a bracket link, [[target][ and ]] around a label,
[[ and ]] around a target alone; of a call of a macro with arguments,
{{{name( and )}}} around them (redact and protect among them), TEXT empty for
these; and each entity, \\dollar{}, TEXT the character it stands for. As a
second value, the calls' own (START . END)."
  (let ((found '()) (calls '()))
    (labels ((walk (node)
               (when (and (< (org-node-begin node) to) (> (max (org-node-end node) (1+ (org-node-begin node))) from))
                 (let ((begin (org-node-begin node))
                       (end (- (org-node-end node) (org-node-post-blank node))))
                   (cond ((and (eq (org-node-type node) 'link) (eq (org-property node :format) 'bracket))
                          (if (org-node-contents-begin node)
                              (progn (push (list begin (org-node-contents-begin node) "") found)
                                     (push (list (org-node-contents-end node) end "") found))
                              (progn (push (list begin (+ begin 2) "") found)
                                     (push (list (- end 2) end "") found))))
                         ((eq (org-node-type node) 'entity)
                          (push (list begin end (gethash (org-property node :name) *org-entities*)) found))
                         ((and (eq (org-node-type node) 'macro) (org-property node :args))
                          (let ((open (+ begin 3 (length (org-property node :key)) 1)))
                            (when (< open (- end 4))
                              (push (list begin open "") found)
                              (push (list (- end 4) end "") found)
                              (push (cons begin end) calls))))
                         (t
                          (when (eq (org-node-type node) 'headline)
                            (mapc #'walk (org-property node :title)))
                          (org-children-in-range node from to #'walk)))))))
      (walk tree))
    (values (sort found #'< :key #'car) (nreverse calls))))

(defun org-hidden-ranges (buffer start end &key (cache t))
  "The parts of BUFFER from octet START below END hidden while links are
shown as their labels (ORG-HIDDEN-CHARS), as LAYOUT's HIDDEN takes them,
\(START END \"\" NIL) octets. As a second value, the calls of macros there,
\(START . END) octets. Both are read from the parses of the parts of BUFFER
there (ORG-VIEWS). Where a large part's parse in the background is not yet of
its text as it is now, those found last are given, carried through the edits
since, and no calls.
With CACHE, the answer is kept, and given again while nothing it depends on
changed, and a part of it is given for a part of what it covers. What is found
for a row's start alone, as soft wrap finds where its rows begin
\(*LAYOUT-PROBE*), is never put in place of the window's own."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (version (tuatara.buffer:buffer-version buffer)))
    (when (tuatara.text:rope-indexed-p rope)
      (let ((key (list version start end))
            (kept (and cache (tuatara.buffer:buffer-local buffer 'org-hidden))))
        (cond
          ((and kept (equal key (first kept)))
           (values (second kept) (third kept)))
          ;; within what is kept: those of it there
          ((and kept (eql version (first (first kept)))
                (<= (second (first kept)) start) (<= end (third (first kept))))
           (values (remove-if-not (lambda (r) (and (> (second r) start) (< (first r) end))) (second kept))
                   (remove-if-not (lambda (c) (and (> (cdr c) start) (< (car c) end))) (third kept))))
          (t
           (let ((end (min end (tuatara.text:rope-octet-length rope)))
                 (ranges '()) (calls '()) (current t))
             (dolist (view (org-views buffer start end))
               (let ((from (max start (org-view-from view)))
                     (to (min end (org-view-to view))))
                 (if (org-view-current view)
                     (multiple-value-bind (pieces view-calls)
                         (org-hidden-chars (org-view-tree view)
                                           (org-view-char buffer view from)
                                           (org-view-char buffer view to))
                       (flet ((octet-of (char) (org-view-byte buffer view char)))
                         (setf ranges (nconc ranges (loop for (s e text) in pieces
                                                          collect (list (octet-of s) (octet-of e) text nil)))
                               calls (nconc calls (loop for (s . e) in view-calls
                                                        collect (cons (octet-of s) (octet-of e)))))))
                     (setf current nil
                           ranges (nconc ranges (org-hidden-carried buffer from to))))))
             ;; what a row's start is found with stays aside, the window's own kept
             (when (and cache current (not tuatara.display:*layout-probe*))
               (setf (tuatara.buffer:buffer-local buffer 'org-hidden) (list key ranges calls)
                     (tuatara.buffer:buffer-local buffer 'org-hidden-anchors)
                     (loop for (s e text) in ranges
                           collect (list (tuatara.text:make-anchor version s :bias :after)
                                         (tuatara.text:make-anchor version e :bias :before)
                                         text))))
             (values ranges calls))))))))

(defun org-hidden-ranges-shown (buffer start end &key (cache t))
  "The parts of BUFFER from octet START below END hidden as it is shown now
\(ORG-HIDDEN-RANGES): the braces of the macro call point is in shown, so
that it can be edited."
  (multiple-value-bind (ranges calls) (org-hidden-ranges buffer start end :cache cache)
    (let* ((point (tuatara.buffer:buffer-point buffer))
           (call (find-if (lambda (c) (<= (car c) point (cdr c))) calls)))
      (if call
          (remove-if (lambda (r) (and (<= (car call) (first r)) (<= (second r) (cdr call)))) ranges)
          ranges))))

(defun org-hidden-carried (buffer start end)
  "The hidden parts found last in BUFFER, where the edits since have moved
them, those from octet START below END, as ORG-HIDDEN-RANGES gives them; one
an edit emptied left out."
  (let ((version (tuatara.buffer:buffer-version buffer)))
    (loop for (from to text) in (tuatara.buffer:buffer-local buffer 'org-hidden-anchors)
          for s = (ignoring-errors-reported ("carrying a hidden part of ~a" (tuatara.buffer:buffer-name buffer))
                    (tuatara.text:anchor-position from version))
          for e = (ignoring-errors-reported ("carrying a hidden part of ~a" (tuatara.buffer:buffer-name buffer))
                    (tuatara.text:anchor-position to version))
          when (and s e (< s e) (< s end) (> e start))
            collect (list s e text nil))))

(defun org-line-holds-octet-p (buffer position octets)
  "True when the line of BUFFER that octet POSITION is on holds one of OCTETS,
a list: a cheap test of whether it can hold a link (a colon or a bracket) or
markup shown otherwise (a bracket, a backslash or a brace), read with no copy
of the line. A line of more than 4096 octets is taken to hold one, unread."
  (let ((rope (tuatara.buffer:buffer-text buffer))
        (start (tuatara.buffer:buffer-line-start buffer position))
        (end (tuatara.buffer:buffer-line-end buffer position)))
    (or (> (- end start) 4096)
        (loop for octet in octets
              thereis (tuatara.text:rope-find-octet rope octet :start start :end end)))))

(defun keep-point-out-of-hidden-markup (editor)
  "Post-command function: in an Org buffer showing links as their labels, a
point the command moved into a hidden part of a link goes past it the way it
was going, so that point never rests in what is not shown; into a macro
call, it shows that call's braces instead. On an entity shown as its
character, point rests at its start, the character, and goes past the rest."
  (let ((buffer (tuatara.display:window-buffer (editor-window editor))))
    (when (and (typep (buffer-major-mode buffer) 'org-mode) (org-links-descriptive-p buffer)
               ;; a line with no [ ] \ { or } hides nothing: the common case, read with no parse
               (org-line-holds-octet-p buffer (tuatara.buffer:buffer-point buffer) '(91 93 92 123 125)))
      (let* ((point (tuatara.buffer:buffer-point buffer))
             (line (tuatara.buffer:buffer-line-start buffer point))
             (hidden (find-if (lambda (r) (and (if (plusp (length (third r))) (< (first r) point) (<= (first r) point))
                                               (< point (second r))))
                              (org-hidden-ranges-shown buffer line (1+ (tuatara.buffer:buffer-line-end buffer point))
                                                       :cache nil)))
             (before (and (eq (car *point-before-command*) buffer) (cdr *point-before-command*))))
        (when hidden
          (let ((start (first hidden)) (end (second hidden)))
            (setf (tuatara.buffer:buffer-point buffer)
                  (cond ((not (and before (> before point))) end)
                        ;; going back: onto an entity's character, or what comes before a hidden part
                        ((plusp (length (third hidden))) start)
                        ((> start line) (1- start))
                        (t end)))))))))

(define-command org-toggle-link-display ()
  "Show this Org buffer's links as written, [[target][label]], and its
entities as written, \\dollar{}, or the links as their labels and the
entities as their characters again (\\`C-c C-x L'). The text is not changed."
  (let ((buffer (current-buffer)))
    (if (not (typep (buffer-major-mode buffer) 'org-mode))
        (message "Only Org buffers show links as their labels")
        (progn
          (setf (tuatara.buffer:buffer-local buffer 'org-links-literal) (org-links-descriptive-p buffer))
          (message "Links shown ~:[as written~;as their labels~]" (org-links-descriptive-p buffer))))))

(defun org-link-at (tree position)
  "The link of TREE holding the character POSITION, or NIL."
  (org-node-at tree position (lambda (node) (and (eq (org-node-type node) 'link) (org-before-blank-p node position)))
               :titles t))

(defun org-goto-char (parse position)
  "Put the point of PARSE's buffer at the character POSITION of PARSE (an
ORG-PARSE), shown in the window, the fold it is in opened."
  (goto-shown (org-parse-buffer parse) (org-parse-octet parse position)))

(defun org-find-in-tree (tree predicate)
  "The first node of TREE, a headline's title too, PREDICATE (a function of a
node) is true of, or NIL."
  (org-walk-tree tree (lambda (node) (when (funcall predicate node) (return-from org-find-in-tree node))))
  nil)

(defun org-headline-with-property (tree key value)
  "The headline of TREE whose property drawer gives KEY the VALUE, or NIL."
  (let ((property (org-find-in-tree tree
                                    (lambda (node) (and (eq (org-node-type node) 'node-property)
                                                        (string-equal (org-property node :key) key)
                                                        (string= (string-trim " " (or (org-property node :value) ""))
                                                                 value))))))
    (loop for node = property then (org-node-parent node)
          while node
          when (eq (org-node-type node) 'headline) return node)))

(defun org-link-target-kinds (target)
  "What a link to TARGET, as written in the link ([[TARGET]]), may lead to,
in the order to look for it, each (KIND . VALUE): *Title a headline titled
so (:TITLE); #id a headline with that custom ID (:CUSTOM-ID); else a
<<target>> (:TARGET), an element with that #+NAME: (:NAME), then a headline
of that title (:TITLE). Following a link and exporting one both go by it."
  (cond ((uiop:string-prefix-p "*" target) (list (cons :title (string-trim " " (subseq target 1)))))
        ((uiop:string-prefix-p "#" target) (list (cons :custom-id (subseq target 1))))
        (t (list (cons :target target) (cons :name target) (cons :title target)))))

(defun org-search-target (parse target)
  "Go to TARGET in the buffer of PARSE (an ORG-PARSE), what a link of that text
leads to (ORG-LINK-TARGET-KINDS); failing those, the headline a link of the
table of contents leads to (TOC-ORG-INSERT-TOC). True when found."
  (let* ((tree (org-parse-tree parse))
         (node
          (loop for (kind . value) in (org-link-target-kinds target)
                thereis (ecase kind
                          (:title (org-headline-titled tree value))
                          (:custom-id (org-headline-with-property tree "CUSTOM_ID" value))
                          (:target (org-find-in-tree tree
                                                     (lambda (n) (and (member (org-node-type n) '(target radio-target))
                                                                      (equal (org-property n :value) value)))))
                          (:name (org-find-in-tree tree (lambda (n) (equal (org-property n :name) value)))))))
         ;; a link of the table of contents (#a-github-anchor, or a title as it lists it)
         (listed (and (null node) (toc-target-position (org-parse-text parse) target))))
    (cond (node (org-goto-char parse (org-node-begin node)) t)
          (listed (org-goto-char parse listed) t))))

(defvar *org-notes-search* nil
  "The search of your notes under way for a link followed (ORG-SEARCH-NOTES),
as (GATHERING WHAT FOUND), or NIL.")

(defun org-search-notes (what wanted found)
  "Look through your notes, the files in *FORMATTED-FILE-DIRECTORY* and under
it, for the first whose path WANTED is true of (a function called away from the
editor's thread), and call FOUND with that path on the editor's thread once it
is found. WHAT names what is looked for, in the words said meanwhile and when
none has it (No match for WHAT). The files are those kept read when the
directory is one of *INDEXED-DIRECTORIES*, else found in the background; a
search under way before stops."
  (let ((root (directory-name *formatted-file-directory*)))
    (multiple-value-bind (files indexed) (indexed-files root)
      (when *org-notes-search* (stop-gathering (first *org-notes-search*)))
      (setf *org-notes-search*
            (list (start-gathering
                   "tuatara searching notes"
                   (lambda (give cancel)
                     (flet ((try (path)
                              (when (and (not (car cancel)) (funcall wanted path))
                                (funcall give path)
                                (setf (car cancel) t))))
                       (if indexed
                           (dolist (file files) (try (concatenate 'string root (car file))))
                           (files-under root :cancel cancel
                                             :each (lambda (path relative) (declare (ignore relative)) (try path)))))))
                  what found))
      (message "Looking for ~a in your notes" what))))

(defun org-notes-search-tick (editor)
  "Tick function: once the search of your notes under way (ORG-SEARCH-NOTES)
has found its file, act on it; once it has ended with none, say so. T while
it runs."
  (declare (ignore editor))
  (let ((search *org-notes-search*))
    (when search
      (destructuring-bind (gathering what found) search
        (multiple-value-bind (paths done) (take-gathered gathering)
          (cond (paths
                 (setf *org-notes-search* nil)
                 (stop-gathering gathering)
                 (funcall found (first paths))
                 nil)
                (done
                 (setf *org-notes-search* nil)
                 (message "No match for ~a" what)
                 nil)
                (t t)))))))

(defun org-show-id (path id &optional safe)
  "Visit the Org file at PATH, with SAFE safely, and go to its headline whose ID is ID."
  (let ((other (visit-path path :safe safe)))
    (show-buffer other)
    (let* ((parse (org-parse-now other))
           (headline (and parse (org-headline-with-property (org-parse-tree parse) "ID" id))))
      (when headline (org-goto-char parse (org-node-begin headline))))))

(defun org-follow-id (parse id)
  "Go to the headline whose ID is ID: in PARSE's buffer at once, else in the Org files
of your notes, looked through in the background (ORG-SEARCH-NOTES), each as
its buffer has it when one is open, else read from disk as the agenda reads
it, untouched (ORG-READ-FILE-TEXT); found, it is shown, safely when PARSE's
buffer was opened safely."
  (let ((here (org-headline-with-property (org-parse-tree parse) "ID" id)))
    (if here
        (progn (org-goto-char parse (org-node-begin here)) t)
        (let ((scanner (cl-ppcre:create-scanner (format nil
                                                        "(?im)^[ \\t]*:ID:[ \\t]+~a[ \\t]*$"
                                                        (cl-ppcre:quote-meta-chars id))))
              ;; the Org files open now, as their buffers have them
              (open (org-agenda-open-snapshot)))
          (org-search-notes (format nil "id:~a" id)
                            (lambda (path)
                              (and (uiop:string-suffix-p path ".org")
                                   (let* ((taken (assoc path open :test #'equal))
                                          (contents (if taken
                                                        (org-agenda-source-text path (second taken))
                                                        (and (not (encrypted-file-p path))
                                                             (org-agenda-source-text path :disk)))))
                                     (and contents (cl-ppcre:scan scanner contents)))))
                            (let ((safe (tuatara.io:safe-visit-p (org-parse-buffer parse))))
                              (lambda (path) (org-show-id path id safe))))
          t))))

(defun org-follow-denote (identifier &optional safe)
  "Visit the note whose name begins with IDENTIFIER, among your notes, looked
for in the background (ORG-SEARCH-NOTES), with SAFE safely."
  (org-search-notes (format nil "denote:~a" identifier)
                    (lambda (path) (uiop:string-prefix-p identifier (tuatara.io:native-file-name path)))
                    (lambda (path) (show-buffer (visit-path path :safe safe))))
  t)

(defparameter *formatted-files-link-type* "formatted-match"
  "The type of the links match-formatted-files-link makes: [[formatted-match:REGEX][LABEL]].")

(defparameter *org-address-link-types* '("http" "https" "ftp" "mailto" "news" "doi")
  "The types of link that lead to an address outside the note: web, mail,
news and DOI links.")

(defun org-link-address (type path)
  "The address a link of TYPE to PATH leads to outside the note, a web, mail
or news address, a DOI's at https://doi.org/; NIL for a link of any other type."
  (when (member type *org-address-link-types* :test #'string=)
    (if (string= type "doi") (format nil "https://doi.org/~a" path) (format nil "~a:~a" type path))))

(defun org-follow-link (parse link)
  "Follow LINK, a link of PARSE (an ORG-PARSE): visit the file or the place in
its buffer it leads to, or open the address outside it. A file is visited
safely when PARSE's buffer was opened safely."
  (let ((buffer (org-parse-buffer parse))
        (type (org-property link :type)) (path (org-property link :path)) (raw (org-property link :raw-link)))
    ;; a type Org itself does not know, read as text to search for; denote: leads to a file by its identifier
    (when (and (string= type "fuzzy") (uiop:string-prefix-p "denote:" path))
      (setf type "denote" path (subseq path 7)))
    (when (and (string= type "fuzzy") (uiop:string-prefix-p (format nil "~a:" *formatted-files-link-type*) path))
      (setf type *formatted-files-link-type* path (subseq path (1+ (length *formatted-files-link-type*)))))
    (flet ((not-found () (message "No match for ~a" raw)))
      (cond
        ((member type '("fuzzy" "custom-id") :test #'string=)
         (unless (org-search-target parse (if (string= type "custom-id") (concatenate 'string "#" path) path))
           (not-found)))
        ((string= type "file")
         (let* ((search (let ((at (search "::" raw))) (and at (subseq raw (+ at 2)))))
                (directory (buffer-directory buffer))
                (file (tuatara.io:native (tuatara.io:expand-path path :directory directory)))
                (other (visit-path file :safe (tuatara.io:safe-visit-p buffer))))
           (show-buffer other)
           (when (and search (plusp (length search)))
             (mode-link-search (buffer-major-mode other) other search))))
        ((string= type "id") (org-follow-id parse path))
        ((and (string= type "denote") (uiop:string-prefix-p "query-filenames:" path))
         (follow-denote-files-link (subseq path (length "query-filenames:"))))
        ((string= type *formatted-files-link-type*) (follow-formatted-files-link path))
        ((and (string= type "denote") (uiop:string-prefix-p "query-contents:" path))
         (message "Links searching file contents are not followed yet: ~a" raw))
        ((string= type "denote") (org-follow-denote path (tuatara.io:safe-visit-p buffer)))
        ((org-link-address type path) (open-address (org-link-address type path)))
        (t (message "Links of type ~a are not followed yet: ~a" type raw))))))

(defmethod mode-link-search (mode buffer search)
  "Go to the headline, custom ID, target or name SEARCH names, as Org finds
one (ORG-SEARCH-TARGET)."
  (declare (ignore mode))
  (let ((parse (org-parse-now buffer)))
    (and parse (org-search-target parse search))))

(define-command org-open-at-point ()
  "Follow the link point is on (\\`C-c C-o'):
to a headline, a custom ID or a target here, a file, an ID, a note by its
identifier, or a web or mail address in the desktop's program, after asking."
  (with-org-structure (parse buffer tree text position)
    (let ((link (org-link-at tree position)))
      (if link
          (org-follow-link parse link)
          (message "No link here")))))

(defun org-follow-link-at-point (buffer)
  "For \\`RET' in NORMAL state: follow the Org link point is on, when BUFFER is
in Org mode and *ORG-RETURN-FOLLOWS-LINK* is true; true when it did. A line
with no colon or bracket (ORG-LINE-HOLDS-OCTET-P), so holding no link, is
not parsed for one."
  (when (and *org-return-follows-link* (typep (buffer-major-mode buffer) 'org-mode)
             (org-line-holds-octet-p buffer (tuatara.buffer:buffer-point buffer) '(58 91 93)))
    (let* ((parse (org-parse-now buffer))
           (link (and parse (org-link-at (org-parse-tree parse) (org-parse-point parse)))))
      (when link
        (org-follow-link parse link)
        t))))

(define-command org-return ()
  "\\`RET' in Org: on a link, follow it (*org-return-follows-link*); in a table,
go to the row below, aligned; else a new line, as \\`RET' does in text."
  (unless (or (org-follow-link-at-point (current-buffer)) (org-table-next-row))
    (newline)))

(defun install-org-links (editor)
  "Bind Org's keys for links: \\`C-c C-o' follows the one at point, \\`RET' too
\(when typing, else a new line); \\`RET' in NORMAL state follows one. Keep
EDITOR's point out of what a link shown as its label hides (but the braces of a
call of redact or protect point is in, shown). And act on what the search of
your notes for a link finds (ORG-NOTES-SEARCH-TICK)."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-o" 'org-open-at-point)
    (define-key keymap "C-c C-x L" 'org-toggle-link-display)
    (define-key keymap "RET" 'org-return))
  (pushnew 'org-follow-link-at-point *enter-dwim-functions*)
  (pushnew 'org-notes-search-tick *tick-functions*)
  (pushnew 'keep-point-out-of-hidden-markup (editor-post-command-functions editor)))
