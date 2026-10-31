;;;; lisp/org/links.lisp --- Org's links followed: RET on one, as org-return-follows-link, and C-c C-o.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A link in an Org buffer is followed as Org's org-open-at-point follows
;;;; it: one to a headline (*Title), a custom ID (#id), a target or a name in
;;;; the buffer goes there; file: visits the file (with ::*Heading or ::text,
;;;; there in it); id: goes to the headline with that ID, here or in the Org
;;;; files of your notes (*formatted-file-directory*); denote: visits the
;;;; note with that identifier, and denote:query-filenames: lists the notes
;;;; whose names match (match-denote-files-link inserts one), as
;;;; formatted-match: does by a regular expression (match-formatted-files-link);
;;;; a web address or a mail address is handed to
;;;; the desktop's program after asking, as it runs outside Tuatara.

(in-package #:tuatara.defaults)

(defvar *org-return-follows-link* t
  "True for \\`RET' on an Org link to follow it;
\\[org-open-at-point] follows one always.")

(defun org-links-descriptive-p (buffer)
  "True when BUFFER, an Org buffer, shows its links as their labels."
  (not (tuatara.buffer:buffer-local buffer 'org-links-literal)))

(defun org-hidden-chars (tree from to)
  "The parts of TREE from character FROM below TO hidden while links are shown
as their labels, as (START . END) characters, in order: of a bracket link,
[[target][ and ]] around a label, [[ and ]] around a target alone; of a call
of a macro with arguments, {{{name( and )}}} around them (redact and protect
among them); and each entity, \\dollar{}, as (START END TEXT), TEXT the
character it stands for, shown in its place. As a second value, the calls'
own (START . END)."
  (let ((found '()) (calls '()))
    (labels ((walk (node)
               (when (and (< (org-node-begin node) to) (> (max (org-node-end node) (1+ (org-node-begin node))) from))
                 (let ((begin (org-node-begin node))
                       (end (- (org-node-end node) (org-node-post-blank node))))
                   (cond ((and (eq (org-node-type node) 'link) (eq (org-property node :format) 'bracket))
                          (if (org-node-contents-begin node)
                              (progn (push (cons begin (org-node-contents-begin node)) found)
                                     (push (cons (org-node-contents-end node) end) found))
                              (progn (push (cons begin (+ begin 2)) found)
                                     (push (cons (- end 2) end) found))))
                         ((eq (org-node-type node) 'entity)
                          (push (list begin end (gethash (org-property node :name) *org-entities*)) found))
                         ((and (eq (org-node-type node) 'macro) (org-property node :args))
                          (let ((open (+ begin 3 (length (org-property node :key)) 1)))
                            (when (< open (- end 4))
                              (push (cons begin open) found)
                              (push (cons (- end 4) end) found)
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
(START END \"\" NIL) octets, and as a second value the calls of macros
there, (START . END) octets, read from the parses of the parts of BUFFER
there (ORG-VIEWS). Where a large part's parse in the background is not yet
of its text as it is now, those found last, carried through the edits since,
and no calls. With CACHE, the answer is kept, and given again while nothing
it depends on changed, and a part of it given for a part of what it covers;
what is found for a row's start alone, as soft wrap finds where its rows
begin (*LAYOUT-PROBE*), never put in place of the window's own."
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
           (let ((end (min end (tuatara.text:rope-bytes rope)))
                 (ranges '()) (calls '()) (current t))
             (dolist (view (org-views buffer start end))
               (let ((from (max start (org-view-from view)))
                     (to (min end (org-view-to view))))
                 (if (org-view-current view)
                     (multiple-value-bind (pieces view-calls)
                         (org-hidden-chars (org-view-tree view) (org-view-char buffer view from) (org-view-char buffer view to))
                       (flet ((octet-of (char) (org-view-byte buffer view char)))
                         (setf ranges (nconc ranges (loop for (s . e) in pieces
                                                          collect (if (consp e)
                                                                      (list (octet-of s) (octet-of (first e)) (second e) nil)
                                                                      (list (octet-of s) (octet-of e) "" nil))))
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
          for s = (ignore-errors (tuatara.text:anchor-position from version))
          for e = (ignore-errors (tuatara.text:anchor-position to version))
          when (and s e (< s e) (< s end) (> e start))
            collect (list s e text nil))))

(defun keep-point-out-of-hidden-markup (editor)
  "Post-command function: in an Org buffer showing links as their labels, a
point the command moved into a hidden part of a link goes past it the way it
was going, so that point never rests in what is not shown; into a macro
call, it shows that call's braces instead. On an entity shown as its
character, point rests at its start, the character, and goes past the rest."
  (let ((buffer (tuatara.display:window-buffer (editor-window editor))))
    (when (and (typep (buffer-major-mode buffer) 'org-mode) (org-links-descriptive-p buffer))
      (let* ((point (tuatara.buffer:buffer-point buffer))
             (line (tuatara.buffer:buffer-line-start buffer point))
             (hidden (find-if (lambda (r) (and (if (plusp (length (third r))) (< (first r) point) (<= (first r) point))
                                               (< point (second r))))
                              (org-hidden-ranges-shown buffer line (1+ (tuatara.buffer:buffer-line-end buffer point)) :cache nil)))
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

(defun org-goto-char (buffer position)
  "Put BUFFER's point at the character POSITION of its parse, shown in the window, the fold it is in opened."
  (goto-shown buffer (org-char->byte buffer position)))

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
                                                        (string= (string-trim " " (or (org-property node :value) "")) value))))))
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

(defun org-search-target (buffer tree text target)
  "Go to TARGET in BUFFER (TREE and TEXT its parse), what a link of that text
leads to (ORG-LINK-TARGET-KINDS); failing those, the headline a link of the
table of contents leads to (TOC-ORG-INSERT-TOC). True when found."
  (let* ((node
          (loop for (kind . value) in (org-link-target-kinds target)
                thereis (ecase kind
                          (:title (org-headline-titled tree value))
                          (:custom-id (org-headline-with-property tree "CUSTOM_ID" value))
                          (:target (org-find-in-tree tree (lambda (n) (and (member (org-node-type n) '(target radio-target))
                                                                                (equal (org-property n :value) value)))))
                          (:name (org-find-in-tree tree (lambda (n) (equal (org-property n :name) value)))))))
         ;; a link of the table of contents (#a-github-anchor, or a title as it lists it)
         (listed (and (null node) (toc-target-position text target))))
    (cond (node (org-goto-char buffer (org-node-begin node)) t)
          (listed (org-goto-char buffer listed) t))))

(defvar *org-notes-search* nil
  "The search of your notes under way for a link followed (ORG-SEARCH-NOTES),
as (GATHERING WHAT FOUND), or NIL.")

(defun org-search-notes (what wanted found)
  "Look through your notes, the files in *FORMATTED-FILE-DIRECTORY* and under
it, for the first whose path WANTED is true of (a function called away from
the editor's thread), and call FOUND with that path on the editor's thread
once it is found; WHAT names what is looked for, in the words said meanwhile
and when none has it (No match for WHAT). The files are those kept read when
the directory is one of *INDEXED-DIRECTORIES*, else found in the background;
a search under way before stops."
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

(defun org-show-id (path id)
  "Visit the Org file at PATH and go to its headline whose ID is ID."
  (let ((other (visit-path path)))
    (show-buffer other)
    (let* ((other-tree (org-tree-now other))
           (headline (and other-tree (org-headline-with-property other-tree "ID" id))))
      (when headline (org-goto-char other (org-node-begin headline))))))

(defun org-follow-id (buffer tree id)
  "Go to the headline whose ID is ID: in BUFFER at once, else in the Org files
of your notes, looked through in the background (ORG-SEARCH-NOTES): each as
its buffer has it when one is open, else read from disk as the agenda reads
it, untouched (ORG-READ-FILE-TEXT); found, it is shown. True when found here
or looked for."
  (let ((here (org-headline-with-property tree "ID" id)))
    (if here
        (progn (org-goto-char buffer (org-node-begin here)) t)
        (let ((scanner (cl-ppcre:create-scanner (format nil "(?im)^[ \\t]*:ID:[ \\t]+~a[ \\t]*$" (cl-ppcre:quote-meta-chars id))))
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
                            (lambda (path) (org-show-id path id)))
          t))))

(defun org-follow-denote (identifier)
  "Visit the note whose name begins with IDENTIFIER, among your notes, looked
for in the background (ORG-SEARCH-NOTES). True: it is looked for."
  (org-search-notes (format nil "denote:~a" identifier)
                    (lambda (path) (uiop:string-prefix-p identifier (tuatara.io:native-file-name path)))
                    (lambda (path) (show-buffer (visit-path path))))
  t)

(defun denote-files-scanner (query)
  "A scanner for QUERY as denote:query-filenames: links match names, ignoring case:
QUERY that is no regular expression is matched as text."
  (tuatara.syntax:compile-pattern query :case-fold t :or-text t))

(defun denote-files-matching (query files)
  "Those of FILES, your files with formatted names, each a list whose first is
its path from *FORMATTED-FILE-DIRECTORY*, whose path QUERY matches as
denote:query-filenames: links match (DENOTE-FILES-SCANNER)."
  (let ((scanner (denote-files-scanner query)))
    (remove-if-not (lambda (file) (cl-ppcre:scan scanner (car file))) files)))

(defun formatted-files-scanner (regex)
  "A scanner for REGEX, a regular expression, ignoring case unless it has a
capital letter (smart case); NIL when it is no regular expression."
  (ignore-errors (tuatara.syntax:compile-pattern regex)))

(defun formatted-files-matching (regex files)
  "Those of FILES, your files with formatted names, each a list whose first is
its path from *FORMATTED-FILE-DIRECTORY*, whose path REGEX, a regular
expression, matches, ignoring case unless it has a capital letter; NIL when
REGEX is no regular expression."
  (let ((scanner (formatted-files-scanner regex)))
    (and scanner (remove-if-not (lambda (file) (cl-ppcre:scan scanner (car file))) files))))

(defun read-formatted-files-query (words matching)
  "Ask for a regular expression the names of your files with formatted names
are to match, for what WORDS says is done with it, the prompt naming its kind
first as a search's does (Regex match file names: ), the files MATCHING (a
function of a regular expression and the files) gives listed as it is
typed, the files found in the background meanwhile
\(CALL-WITH-FORMATTED-FILES); \\<pattern>\\[search-toggle-regex] switches to literal
text and back. \\<pattern-answer>\\[pattern-done] takes what is typed, whichever file is
selected: a regular expression, literal text with its
special characters escaped; a regular expression that cannot be used says
why in the prompt, waiting for it to be mended."
  (let ((typed "") (regex t) (refused nil))
    (call-with-formatted-files
     (lambda (listed source)
       (let ((*toggle-pattern-kind* (lambda () (setf regex (not regex))))
             (*pattern-done* (lambda ()
                               (if (pattern-problem (editor-minibuffer-text *editor*) regex)
                                   (setf refused (editor-minibuffer-text *editor*))
                                   (finish-minibuffer)))))
         (completing-read (lambda ()
                            (let ((text (editor-minibuffer-text *editor*)))
                              (refusable-pattern-prompt regex words (pattern-problem text regex) (equal refused text))))
                          (lambda (input)
                            (setf typed input)
                            (mapcar #'third (funcall matching (pattern-regex input regex) (funcall listed))))
                          :filter nil :select (lambda (candidates input) (declare (ignore candidates input)) :input) :reselect t
                          :source source :keymap *pattern-answer-keymap*
                          :none (format nil "none in ~a" (abbreviated-path (directory-name *formatted-file-directory*)))))))
    (keep-typed-pattern typed)
    (pattern-regex typed regex)))

(defun show-formatted-files (title keep)
  "List your files with formatted names whose path from
*FORMATTED-FILE-DIRECTORY* KEEP is true of, found in the background, under
TITLE, to choose one to visit (READ-FORMATTED-FILE)."
  (let ((chosen (read-formatted-file title :keep keep :preview t)))
    (when chosen
      (visit-path (concatenate 'string (directory-name *formatted-file-directory*) chosen)))))

(defun follow-formatted-files-link (regex)
  "List your files with formatted names whose names REGEX matches, to choose
one to visit, as a link match-formatted-files-link makes is followed."
  (let ((scanner (formatted-files-scanner regex)))
    (if (null scanner)
        (message "~a is no regular expression" regex)
        (show-formatted-files (format nil "Files matching ~a: " regex)
                              (lambda (relative) (cl-ppcre:scan scanner relative))))))

(defun follow-denote-files-link (query)
  "List your files with formatted names whose names QUERY matches, to choose
one to visit: where a denote:query-filenames: link leads."
  (let ((scanner (denote-files-scanner query)))
    (show-formatted-files (format nil "Files matching ~a: " query)
                          (lambda (relative) (cl-ppcre:scan scanner relative)))))

(defun insert-link-text (buffer region link)
  "Put LINK in BUFFER in place of REGION, (START END) octets, or at point when
REGION is NIL, point after it."
  (if region
      (progn (setf (tuatara.buffer:buffer-mark-active-p buffer) nil)
             (tuatara.buffer:buffer-replace buffer (first region) (second region) link)
             (setf (tuatara.buffer:buffer-point buffer) (+ (first region) (tuatara.text:string-octet-length link))))
      (let ((at (point)))
        (tuatara.buffer:buffer-insert buffer at link)
        (setf (tuatara.buffer:buffer-point buffer) (+ at (tuatara.text:string-octet-length link))))))

(defparameter *formatted-files-link-type* "formatted-match"
  "The type of the links match-formatted-files-link makes: [[formatted-match:REGEX][LABEL]].")

(defun read-link-label (region buffer suggested)
  "The label of a link being made: the text of REGION of BUFFER when text is
selected, else asked for, SUGGESTED offered selected, to take or type over."
  (if region
      (tuatara.buffer:buffer-string buffer :start (first region) :end (second region))
      (read-string "Link LABEL: " :initial suggested :select-initial t)))

(defun link-label-problem (label)
  "Why LABEL cannot be an Org link's label, or NIL: one holding ]] or ending in ] would end the link early."
  (when (or (search "]]" label) (and (plusp (length label)) (char= (char label (1- (length label))) #\])))
    "a label ending in ] or holding ]] would end the link early"))

(define-command match-formatted-files-link ()
  "Insert a link to your files with formatted names whose names match a
regular expression, asked for, the files it matches listed as it is typed:
[[formatted-match:REGEX][LABEL]]. The text selected becomes the link, its
label; with none, the label is asked for, REGEX offered. Following it (\\`RET'
or \\[org-open-at-point] in Org) lists the files in *FORMATTED-FILE-DIRECTORY*
and under it whose path from there REGEX matches, ignoring case unless it has
a capital letter, to visit one. Brackets in it are written \\=\\[ and \\=\\]
in the link. It is Tuatara's own kind of link, which other Org programs do not
follow; match-denote-files-link makes one that other programs reading
denote: links follow too."
  (let* ((buffer (current-buffer))
         (region (selected-region buffer))
         (regex (read-formatted-files-query "match file names" #'formatted-files-matching)))
    (cond ((string= (string-trim " " regex) "") (message "No link inserted: nothing to match"))
          ((null (formatted-files-scanner regex)) (message "No link inserted: ~a is no regular expression" regex))
          (t (let* ((label (read-link-label region buffer (if (find #\] regex) "matching files" regex)))
                    (problem (link-label-problem label)))
               (if problem
                   (message "No link inserted: ~a" problem)
                   (insert-link-text buffer region
                                     (org-link-text (format nil "~a:~a" *formatted-files-link-type* regex) label))))))))

(defun match-denote-files-link-text (query label extension)
  "The denote: link to the files whose names QUERY matches, labelled LABEL, as
written in a file ending in EXTENSION: [[denote:query-filenames:QUERY][LABEL]]
in Org (and others), [LABEL](denote:query-filenames:QUERY) in Markdown,
<denote:query-filenames:QUERY> in text, which has no label."
  (cond ((string-equal extension ".md") (format nil "[~a](denote:query-filenames:~a)" label query))
        ((string-equal extension ".txt") (format nil "<denote:query-filenames:~a>" query))
        (t (org-link-text (format nil "denote:query-filenames:~a" query) label))))

(define-command match-denote-files-link ()
  "Insert a link to your files with formatted names whose names match what is
asked for, the files it matches listed as it is typed, as a denote: link,
which other programs reading those links follow too:
[[denote:query-filenames:TEXT][LABEL]]; in Markdown
[LABEL](denote:query-filenames:TEXT) and in text files
<denote:query-filenames:TEXT>. The text selected becomes the link, its label; with
none, the label is asked for, TEXT offered. Following it (\\`RET' or
\\[org-open-at-point] in Org) lists the files in *FORMATTED-FILE-DIRECTORY* and
under it whose names match, ignoring case, to visit one. Brackets and
parentheses in what is matched are refused, as these links cannot hold them."
  (let* ((buffer (current-buffer))
         (region (selected-region buffer))
         (query (read-formatted-files-query "match file names (Denote)" #'denote-files-matching))
         (file (tuatara.buffer:buffer-file buffer))
         (extension (if file (name-extension (tuatara.io:native-file-name file)) ".org")))
    (cond ((string= (string-trim " " query) "") (message "No link inserted: nothing to match"))
          ((or (find #\[ query) (find #\] query) (find #\( query) (find #\) query))
           (message "No link inserted: denote: links take no brackets or parentheses"))
          (t (let* ((label (if (string-equal extension ".txt") "" (read-link-label region buffer query)))
                    (problem (link-label-problem label)))
               (if problem
                   (message "No link inserted: ~a" problem)
                   (insert-link-text buffer region (match-denote-files-link-text query label extension))))))))

(defparameter *org-address-link-types* '("http" "https" "ftp" "mailto" "news" "doi")
  "The types of link that lead to an address outside the note: web, mail,
news and DOI links.")

(defun org-link-address (type path)
  "The address a link of TYPE to PATH leads to outside the note, a web, mail
or news address, a DOI's at https://doi.org/; NIL for a link of any other type."
  (when (member type *org-address-link-types* :test #'string=)
    (if (string= type "doi") (format nil "https://doi.org/~a" path) (format nil "~a:~a" type path))))

(defun org-follow-link (buffer tree text link)
  "Follow LINK, a link of BUFFER's parse (TREE, TEXT): visit the file or the
place in this one it leads to, or open the address outside it."
  (let ((type (org-property link :type)) (path (org-property link :path)) (raw (org-property link :raw-link)))
    ;; a type Org itself does not know, read as text to search for: denote: leads to a file by its identifier
    (when (and (string= type "fuzzy") (uiop:string-prefix-p "denote:" path))
      (setf type "denote" path (subseq path 7)))
    (when (and (string= type "fuzzy") (uiop:string-prefix-p (format nil "~a:" *formatted-files-link-type*) path))
      (setf type *formatted-files-link-type* path (subseq path (1+ (length *formatted-files-link-type*)))))
    (flet ((not-found () (message "No match for ~a" raw)))
      (cond
        ((member type '("fuzzy" "custom-id") :test #'string=)
         (unless (org-search-target buffer tree text (if (string= type "custom-id") (concatenate 'string "#" path) path))
           (not-found)))
        ((string= type "file")
         (let* ((search (let ((at (search "::" raw))) (and at (subseq raw (+ at 2)))))
                (directory (buffer-directory buffer))
                (file (tuatara.io:native (tuatara.io:expand-path path :directory directory)))
                (other (visit-path file)))
           (show-buffer other)
           (when (and search (plusp (length search)))
             (multiple-value-bind (other-tree other-text) (org-tree-now other)
               (when other-tree (org-search-target other other-tree other-text search))))))
        ((string= type "id") (unless (org-follow-id buffer tree path) (not-found)))
        ((and (string= type "denote") (uiop:string-prefix-p "query-filenames:" path))
         (follow-denote-files-link (subseq path (length "query-filenames:"))))
        ((string= type "formatted-match") (follow-formatted-files-link path))
        ((and (string= type "denote") (uiop:string-prefix-p "query-contents:" path))
         (message "Links searching file contents are not followed yet: ~a" raw))
        ((string= type "denote") (unless (org-follow-denote path) (not-found)))
        ((org-link-address type path) (open-address (org-link-address type path)))
        (t (message "Links of type ~a are not followed yet: ~a" type raw))))))

(define-command org-open-at-point ()
  "Follow the link point is on (\\`C-c C-o'):
to a headline, a custom ID or a target here, a file, an ID, a note by its
identifier, or a web or mail address in the desktop's program, after asking."
  (with-org-structure (buffer tree text position)
    (let ((link (org-link-at tree position)))
      (if link
          (org-follow-link buffer tree text link)
          (message "No link here")))))

(defun org-line-may-hold-link-p (buffer position)
  "True unless the line of BUFFER that octet POSITION is on surely holds no
link: a line of more than 4096 octets may; a shorter one holds none when it
has no colon, [[ or ]]."
  (let ((start (tuatara.buffer:buffer-line-start buffer position))
        (end (tuatara.buffer:buffer-line-end buffer position)))
    (or (> (- end start) 4096)
        (let ((line (tuatara.buffer:buffer-string buffer :start start :end end)))
          (or (find #\: line) (search "[[" line) (search "]]" line))))))

(defun org-follow-link-at-point (buffer)
  "For \\`RET' in NORMAL state: follow the Org link point is on, when BUFFER is
in Org mode and *ORG-RETURN-FOLLOWS-LINK* is true; true when it did. A line
that can hold no link (ORG-LINE-MAY-HOLD-LINK-P) is not parsed for one."
  (when (and *org-return-follows-link* (typep (buffer-major-mode buffer) 'org-mode)
             (org-line-may-hold-link-p buffer (tuatara.buffer:buffer-point buffer)))
    (multiple-value-bind (tree text) (org-tree-now buffer)
      (let ((*org-current-text* (or text "")))
        (let ((link (and tree (org-link-at tree (org-point-char buffer)))))
          (when link
            (org-follow-link buffer tree text link)
            t))))))

(define-command org-return ()
  "\\`RET' in Org: on a link, follow it (*org-return-follows-link*); in a table,
go to the row below, aligned; else a new line, as \\`RET' does in text."
  (unless (or (org-follow-link-at-point (current-buffer)) (org-table-next-row))
    (newline)))

(defun install-markdown-links ()
  "Bind Markdown's keys for links: \\`C-c C-o' follows the one point is on, and
\\`RET' in NORMAL state does too."
  (define-key (mode-keymap 'markdown-mode) "C-c C-o" 'markdown-follow-link-at-point)
  (pushnew 'markdown-follow-link-for-return *enter-dwim-functions*))

(defun install-org-links (editor)
  "Bind Org's keys for links: \\`C-c C-o' follows the one at point, \\`RET' too (when typing,
else a new line); \\`RET' in NORMAL state follows one; keep EDITOR's point out of what a
link shown as its label hides (but the braces of a call of redact or protect point is in, shown); and act on what the search of your notes for a
link finds (ORG-NOTES-SEARCH-TICK)."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "C-c C-o" 'org-open-at-point)
    (define-key keymap "C-c C-x L" 'org-toggle-link-display)
    (define-key keymap "RET" 'org-return))
  (pushnew 'org-follow-link-at-point *enter-dwim-functions*)
  (pushnew 'org-notes-search-tick *tick-functions*)
  (pushnew 'keep-point-out-of-hidden-markup (editor-post-command-functions editor)))
