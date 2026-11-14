;;;; lisp/org/fold.lisp --- Org's subtrees folded and shown: TAB and S-TAB, as Org cycles them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; TAB on a headline cycles its subtree as Org's org-cycle does: folded (the
;;;; headline alone), its children (their headlines, their bodies folded), all
;;;; of it, and folded again. S-TAB cycles the whole buffer as
;;;; org-global-cycle does: the overview (the first level's headlines), the
;;;; contents (every headline), everything. Folding hides text from the
;;;; windows (src/display/folds.lisp) and changes none; point is kept out of
;;;; what is hidden. #+STARTUP: overview, content and showall set how a file
;;;; opens.

(in-package #:tuatara.defaults)

;;; Folds made stale by a command

(defun open-stale-org-folds-after-command (editor)
  "Post-command function: in an Org buffer with folds that the command
changed, a fold left after a line that no longer folds is opened
\(OPEN-STALE-ORG-FOLDS), before point is kept out of what is hidden
\(KEEP-POINT-OUT-OF-FOLDS)."
  (let* ((buffer (tuatara.display:window-buffer (editor-window editor)))
         (before (and (eq (car *point-before-command*) buffer) (cdr *point-before-command*))))
    (when (and (tuatara.display:buffer-hides-p buffer)
               before (not (eq *version-before-command* (tuatara.buffer:buffer-version buffer)))
               (typep (buffer-major-mode buffer) 'org-mode))
      (open-stale-org-folds buffer before))))

;;; Subtrees

(defun org-headlines (tree)
  "Every headline of TREE, in order."
  (let ((found '()))
    (labels ((walk (node)
               (when (eq (org-node-type node) 'headline) (push node found))
               (when (member (org-node-type node) '(org-data headline))
                 (mapc #'walk (org-node-children node)))))
      (walk tree))
    (nreverse found)))

(defun org-headline-titled (tree title)
  "The first headline of TREE whose title, as written, is TITLE; NIL for none."
  (find title (org-headlines tree) :key (lambda (h) (org-property h :raw-value)) :test #'string=))

(defun org-headline-on-line (tree text position)
  "The headline whose line holds the character POSITION of TEXT, or NIL."
  (let ((line (string-line-start text position)))
    (org-node-at tree line (lambda (node) (and (eq (org-node-type node) 'headline) (= (org-node-begin node) line))))))

(defun org-child-headlines (headline)
  "The headlines right under HEADLINE."
  (org-children-of headline 'headline))

(defun org-last-content-end (text from to)
  "Where the text from FROM below TO ends, the blank lines after it left out:
the newline ending its last line that is not blank (or TO)."
  (let* ((*org-text* text)
         (last (org-skip-blanks-backward to from)))
    (if (= last from) from (org-line-end (1- last)))))

(defun org-fold-chars (parse from to)
  "Fold the characters FROM below TO of PARSE (an ORG-PARSE) in its buffer."
  (when (< from to)
    (tuatara.display:fold-region (org-parse-buffer parse) (org-parse-octet parse from) (org-parse-octet parse to))))

(defun org-fold-range (node text &optional (limit (org-node-end node)))
  "The characters of TEXT folding NODE, a headline or a list item, hides,
\(START . END): from the end of its first line to the end of its last line
that is not blank before LIMIT (its end when not given); NIL when it has
nothing under its first line."
  (let* ((start (string-line-end text (org-node-begin node)))
         (end (org-last-content-end text start limit)))
    (and (< start end) (cons start end))))

(defun org-section-range (headline text)
  "The characters of TEXT holding HEADLINE's own text, before its first child
headline, (START . END) as ORG-FOLD-RANGE has them; NIL when there is none."
  (let ((child (first (org-child-headlines headline))))
    (org-fold-range headline text (if child (org-node-begin child) (org-node-end headline)))))

(defun org-folded-p (parse range)
  "True when the folds of PARSE's buffer hide the whole of RANGE, characters
\(START . END) of PARSE (an ORG-PARSE)."
  (tuatara.display:range-folded-p (org-parse-buffer parse)
                                 (org-parse-octet parse (car range)) (org-parse-octet parse (cdr range))))

(define-setting *org-cycle-hide-drawers* t
  "Whether drawers and a task's notes stay folded as \\`TAB' shows the rest.
Drawers are :LOGBOOK: and :PROPERTIES:; the notes are ORG-NOTES-RANGE.
- t: they stay folded when \\`TAB' shows a headline's or an item's children
  or all of it; \\`TAB' on a drawer's line, or on the first of the notes,
  opens it.
- nil: they are shown with the rest."
  :values boolean)

(define-setting *org-fold-metadata* t
  "Whether an Org file opens with what is kept of each task folded.
What is kept is its drawers (:PROPERTIES:, :LOGBOOK:) and its notes (- State
\"DONE\" ..., - Note taken on ..., CLOCK: lines), of headings and list items
alike; its SCHEDULED: and DEADLINE: are shown either way. A file's #+STARTUP:
showdrawers keeps them open, hidedrawers folds them whatever this says.
- t: each folded to one line; \\`TAB' on the line opens it. A file larger
  than *org-fold-metadata-most* opens with them shown all the same.
- nil: every file opens with them shown."
  :values boolean)

(defparameter *org-note-line-re*
  (cl-ppcre:create-scanner
   (concatenate 'string "^[ \\t]*(?:- (?:State \"|Note taken on |Rescheduled from |Not scheduled, was |"
                "New deadline from |Removed deadline, was |Refiled on |CLOSING NOTE )|CLOCK: )"))
  "A line a task's log keeps: a note of a change of state, a reschedule or a
note taken, or a clock's line.")

(defun org-notes-range (buffer line)
  "The octets of BUFFER folding the notes a task keeps that begin at LINE
\(*ORG-NOTE-LINE-RE*), (START . END): from the end of LINE to the end of the
last of them, the lines going on a note (set in deeper than its dash) with
it; NIL when LINE begins no such note or it stands alone. As a second
value, the line after them."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-octet-length rope))
         (head (buffer-line-head buffer line)))
    (when (cl-ppcre:scan *org-note-line-re* head)
      (let ((indent (or (position-if-not #'space-or-tab-char-p head) 0))
            (last line))
        (loop for next = (1+ (tuatara.text:rope-line-end rope last))
              while (< next size)
              do (let* ((text (buffer-line-head buffer next))
                        (own (position-if-not #'space-or-tab-char-p text)))
                   (if (or (cl-ppcre:scan *org-note-line-re* text)
                           (and own (> own indent) (not (eql (char text own) #\-))))
                       (setf last next)
                       (return))))
        (let ((after (min size (1+ (tuatara.text:rope-line-end rope last)))))
          (values (and (/= last line)
                       (cons (tuatara.buffer:buffer-line-end buffer line) (tuatara.buffer:buffer-line-end buffer last)))
                  after))))))

(defparameter *org-drawer-line-re* (concatenate 'string "^" *org-drawer-re*)
  "A line beginning an Org drawer, :LOGBOOK: or :PROPERTIES:, as the parser reads one (*ORG-DRAWER-RE*).")

(defun org-drawer-range (buffer line)
  "The octets of BUFFER folding the drawer whose first line begins at LINE
hides, (START . END): from the end of that line to the end of its :END: line;
NIL when LINE begins no drawer that ends before the next headline. As a
second value, where the search for its end stopped: the start of the :END:
line, of the next headline, or the text's end."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-octet-length rope))
         (first-end (tuatara.buffer:buffer-line-end buffer line))
         (head (buffer-line-head buffer line)))
    (when (and (cl-ppcre:scan *org-drawer-line-re* head)
               (not (cl-ppcre:scan "(?i)^[ \\t]*:END:" head)))
      (loop for next = (1+ (tuatara.text:rope-line-end rope line)) then (1+ (tuatara.text:rope-line-end rope next))
            while (< next size)
            do (cond ((org-stars rope next) (return (values nil next)))
                     ((cl-ppcre:scan "(?i)^[ \\t]*:END:[ \\t]*$" (buffer-line-head buffer next))
                      (return (values (cons first-end (tuatara.buffer:buffer-line-end buffer next)) next))))
            finally (return (values nil size))))))

(defun org-fold-drawers-in (buffer start end)
  "Fold each drawer of BUFFER whose first line begins from octet START below
END, and each run of a task's notes (ORG-NOTES-RANGE), each line looked at
once: what follows a drawer that is never ended holds no drawer's end
either, up to the next headline."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-octet-length rope))
         (line (tuatara.buffer:buffer-line-start buffer start)))
    (loop while (and (< line end) (< line size))
          do (let ((next (1+ (tuatara.text:rope-line-end rope line)))
                   (first (loop for i from line below (min end size)
                                for octet = (tuatara.text:rope-octet rope i)
                                while (member octet '(32 9))
                                finally (return (and (< i size) (tuatara.text:rope-octet rope i))))))
               (when (member first '(58 45 67)) ; a line whose text begins with a colon, a dash or a C
                 (multiple-value-bind (range stop)
                     (if (eql first 58) (org-drawer-range buffer line) (org-notes-range buffer line))
                   (when range (tuatara.display:fold-region buffer (car range) (cdr range)))
                   (when stop (setf next (max next stop)))))
               (setf line next)))))

(defun org-cycle-drawer (buffer line)
  "Fold the drawer whose first line begins at LINE of BUFFER, or the notes
that begin there (ORG-NOTES-RANGE), or open it when it is folded; true when
LINE begins either."
  (let* ((drawer (org-drawer-range buffer line))
         (range (or drawer (org-notes-range buffer line)))
         (what (if drawer "Drawer" "Notes")))
    (when range
      (if (tuatara.display:range-folded-p buffer (car range) (cdr range))
          (progn (tuatara.display:unfold-region buffer (car range) (cdr range)) (message "~a shown" what))
          (progn (tuatara.display:fold-region buffer (car range) (cdr range)) (message "~a folded" what)))
      t)))

(defun org-cycle-ranges (buffer range children)
  "Cycle a subtree of BUFFER whose fold hides RANGE and its direct children's
folds CHILDREN (a list, NIL for a child with nothing under it), all (START .
END) in octets: folded, then its own text and its children folded, then all
of it, then folded again."
  (flet ((folded-p (r) (tuatara.display:range-folded-p buffer (car r) (cdr r)))
         (fold (r) (tuatara.display:fold-region buffer (car r) (cdr r)))
         (unfold (r) (tuatara.display:unfold-region buffer (car r) (cdr r))))
    (cond
      ((null range) (message "Empty entry"))
      ((folded-p range)
       (unfold range)
       (when *org-cycle-hide-drawers* (org-fold-drawers-in buffer (car range) (cdr range)))
       (if children
           (progn (dolist (child children) (when child (fold child)))
                  (message "Children"))
           (message "Subtree")))
      ((and children
            (every (lambda (child) (or (null child) (folded-p child))) children)
            (some #'identity children))
       (unfold range)
       (when *org-cycle-hide-drawers* (org-fold-drawers-in buffer (car range) (cdr range)))
       (message "Subtree"))
      (t
       (unfold range)
       (fold range)
       (message "Folded")))))

(defun org-octet-range (parse range)
  "RANGE, characters (START . END) of PARSE (an ORG-PARSE), as octets of its
buffer; NIL for none."
  (and range (cons (org-parse-octet parse (car range)) (org-parse-octet parse (cdr range)))))

(defun org-cycle-headline (parse headline)
  "Cycle HEADLINE's subtree, of PARSE: folded, then its children, then all of it, then folded again."
  (let ((text (org-parse-text parse)))
    (org-cycle-ranges (org-parse-buffer parse) (org-octet-range parse (org-fold-range headline text))
                      (mapcar (lambda (child) (org-octet-range parse (org-fold-range child text)))
                              (org-child-headlines headline)))))

(defun org-stars (rope line)
  "The level of the headline whose line starts at the octet LINE of ROPE (how
many stars begin it, a space or the line's end after them), or NIL when the
line is no headline."
  (let ((size (tuatara.text:rope-octet-length rope)))
    (loop for i from line below size
          for octet = (tuatara.text:rope-octet rope i)
          while (= octet 42)
          finally (return (and (> i line)
                               (or (>= i size) (member (tuatara.text:rope-octet rope i) tuatara.text:*blank-octets*))
                               (- i line))))))

(defun org-line-folds-p (buffer line)
  "True when the line of BUFFER starting at the octet LINE begins what \\`TAB'
folds: a headline, a list item or a drawer; only its first 512 octets read."
  (let ((text (tuatara.buffer:buffer-string
               buffer :start line :end (min (tuatara.buffer:buffer-line-end buffer line) (+ line 512)))))
    (or (org-stars (tuatara.buffer:buffer-text buffer) line)
        (cl-ppcre:scan *org-drawer-line-re* text)
        (cl-ppcre:scan (org-item-line-scanner) text))))

(defun open-stale-org-folds (buffer previous)
  "Show again what is folded after the line point is on, or the one it was on
before the command (PREVIOUS), when that line no longer begins a headline, a
list item or a drawer (ORG-LINE-FOLDS-P): a heading whose stars were deleted
leaves its text in sight, under a plain line, rather than hidden there. Only
those two lines are looked at."
  (let ((size (tuatara.buffer:buffer-size buffer)))
    (dolist (position (remove-duplicates (remove nil (list (tuatara.buffer:buffer-point buffer)
                                                           (and previous (min previous size))))))
      (let* ((line (tuatara.buffer:buffer-line-start buffer position))
             (end (tuatara.buffer:buffer-line-end buffer line))
             (fold (tuatara.display:fold-from buffer end)))
        (when (and fold (= (car fold) end) (not (org-line-folds-p buffer line)))
          (tuatara.display:unfold-region buffer (car fold) (cdr fold)))))))

(defun org-text-end-between (buffer from to)
  "Where BUFFER's text from the octet FROM below TO ends, the blank lines after
it left out: the end of its last line that is not blank (before its line
end), or FROM."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (loop with p = to
          while (> p from)
          do (let* ((line (max from (tuatara.buffer:buffer-line-start buffer (1- p))))
                    (end (min p (tuatara.buffer:buffer-line-end buffer line))))
               (when (loop for i from line below end
                           thereis (not (member (tuatara.text:rope-octet rope i) tuatara.text:*blank-octets*)))
                 (return end))
               (setf p line))
          finally (return from))))

(defparameter *org-cycle-unparsed-most* (* 4 1024 1024)
  "How many octets of a large file \\`TAB' on a headline reads to find its
subtree while the file's parse is not ready; a longer subtree folds once the
parse is.")

(defun org-next-star-line (rope from end)
  "The start of the first line of ROPE after the octet FROM, below END, that
begins with a star, or NIL; found by looking for the stars alone."
  (loop for star = (tuatara.text:rope-find-octet rope 42 :start (1+ from) :end end)
        while star
        do (if (= (tuatara.text:rope-octet rope (1- star)) 10)
               (return star)
               (setf from star))))

(defun org-cycle-headline-at (buffer line)
  "Cycle the subtree of the headline whose line starts at the octet LINE of
BUFFER, as ORG-CYCLE-HEADLINE does, found by reading its lines rather than by
parsing the file: it ends at the next headline of its level or above, its
direct children are the headlines in it with no shallower one before them.
True when it did; NIL, nothing done, when the subtree is longer than
*ORG-CYCLE-UNPARSED-MOST*."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (size (tuatara.text:rope-octet-length rope))
         (bound (min size (+ line *org-cycle-unparsed-most*)))
         (level (org-stars rope line))
         (start (tuatara.buffer:buffer-line-end buffer line))
         (children '())
         (end nil))
    (loop with shallowest = most-positive-fixnum
          for next = (org-next-star-line rope line bound) then (org-next-star-line rope next bound)
          while next
          do (let ((stars (org-stars rope next)))
               (when stars
                 (when (<= stars level) (setf end next) (return))
                 (when (<= stars shallowest) (setf shallowest stars) (push next children)))))
    (when (and (null end) (= bound size))
      (setf end size))
    (when end
      (setf children (nreverse children))
      (flet ((range (from limit) (let ((e (org-text-end-between buffer from limit))) (and (< from e) (cons from e)))))
        (org-cycle-ranges buffer (range start end)
                          ;; a child's subtree ends where the next child begins: a headline of its level or
                          ;; above before then would be a child itself
                          (loop for (child . more) on children
                                collect (range (tuatara.buffer:buffer-line-end buffer child) (or (first more) end)))))
      t)))

(define-setting *org-cycle-include-plain-lists* t
  "What \\`TAB' does on a list item with sub-items or text under its first line.
- t: folds it as on a headline, folded first, then its sub-items, then all
  of it.
- nil: indents."
  :values boolean)

(defun org-item-on-line (tree position)
  "The list item of TREE beginning at the character POSITION (a line's start), or NIL."
  (org-node-at tree position (lambda (node) (and (eq (org-node-type node) 'item) (= (org-node-begin node) position)))))

(defun org-cycle-item (parse item)
  "Cycle the list ITEM, of PARSE, as ORG-CYCLE-HEADLINE cycles a headline:
folded, then its sub-items, then all of it; one with nothing under its first
line is said to be empty."
  (let* ((text (org-parse-text parse))
         (range (org-fold-range item text)))
    (if (null range)
        (message "Empty entry")
        (org-cycle-ranges (org-parse-buffer parse) (org-octet-range parse range)
                          (mapcar (lambda (child) (org-octet-range parse (org-fold-range child text)))
                                  (org-child-items item))))
    t))

(defparameter *org-drawer-look-back* 256
  "The most lines \\`TAB' looks back over for the start of a drawer it may be
in (ORG-LINE-IN-DRAWER-P).")

(defun org-line-in-drawer-p (buffer line)
  "True when the line of BUFFER beginning at the octet LINE is inside a
drawer (:LOGBOOK:, :PROPERTIES: and the like): after a drawer's first line
with no :END: between, under the same headline; looked for no further back
than *ORG-DRAWER-LOOK-BACK* lines."
  (let ((rope (tuatara.buffer:buffer-text buffer)))
    (loop repeat *org-drawer-look-back*
          for start = line then above
          for above = (and (plusp start) (tuatara.buffer:buffer-line-start buffer (1- start)))
          while above
          do (let ((text (tuatara.buffer:buffer-string buffer :start above
                                                       :end (min (+ above 512)
                                                                 (tuatara.buffer:buffer-line-end buffer above)))))
               (cond ((org-stars rope above) (return nil))
                     ((cl-ppcre:scan *org-drawer-end-re* text) (return nil))
                     ((cl-ppcre:scan *org-drawer-line-re* text) (return t)))))))

(defun org-line-kept-from-tab-p (buffer line text)
  "True when \\`TAB' leaves the line of BUFFER beginning at the octet LINE,
whose first characters are TEXT, as it is: a planning line (CLOSED:,
SCHEDULED:, DEADLINE:), a drawer's :END:, or a line inside a drawer."
  (or (org-planning-line-p text)
      (cl-ppcre:scan *org-drawer-end-re* text)
      (org-line-in-drawer-p buffer line)))

(define-command org-cycle ()
  "On a headline, fold its subtree to its headline alone, then show its
children's headlines, then all of it, and fold it again, a step each time.
On a list item with something under it, the same for
the item (*org-cycle-include-plain-lists*). On a drawer's first line
\(:LOGBOOK:), fold or open the drawer; on a planning line (CLOSED:,
SCHEDULED:, DEADLINE:), a drawer's :END: or a line inside a drawer, nothing,
so that what Org keeps there stays as written. In a table, align it and go
to the next field. After a snippet's key (<s for a source block), the snippet
\(\\[insert-snippet]). Elsewhere, what \\`TAB' does in text."
  (let* ((buffer (current-buffer))
         (start (tuatara.buffer:buffer-line-start buffer (point)))
         ;; the line's beginning tells what it is: a line of a million characters is not read whole
         (line (buffer-line-head buffer start))
         (bare (string-left-trim *org-blanks* line))
         (headline-table-or-template (or (uiop:string-prefix-p "*" line) (uiop:string-prefix-p "|" bare)
                                         (uiop:string-prefix-p "<" bare))))
    ;; a drawer's first line, the drawer folded or opened, found by its lines
    (when (and (or (uiop:string-prefix-p ":" bare)
                   (uiop:string-prefix-p "- " bare)
                   (uiop:string-prefix-p "CLOCK: " bare))
               (org-cycle-drawer buffer start))
      (return-from org-cycle))
    ;; planning lines and a drawer's lines: left as Org writes them
    (when (org-line-kept-from-tab-p buffer start line)
      (return-from org-cycle))
    ;; a line that is no headline, table, template or list item to fold: indented, the file not parsed for it
    (unless (or headline-table-or-template
                (and *org-cycle-include-plain-lists* (cl-ppcre:scan (org-item-line-scanner) line)))
      (return-from org-cycle (indent-for-tab-command)))
    ;; a list item of a large file whose parse is not ready: said, the parse under way
    (when (and (not headline-table-or-template)
               (> (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
               (progn (org-buffer-parse buffer)
                      (not (org-parse-current-p buffer))))
      (return-from org-cycle (message "The list folds once the file is read, in a moment")))
    ;; a headline of a large file whose parse is not ready: its subtree found by its lines, nothing parsed
    (when (and (org-stars (tuatara.buffer:buffer-text buffer) start)
               (> (tuatara.buffer:buffer-size buffer) *org-parse-at-once*)
               (not (org-parse-current-p buffer)))
      (when (org-cycle-headline-at buffer start)
        (return-from org-cycle)))
    (let* ((parse (org-parse-now buffer))
           (tree (and parse (org-parse-tree parse)))
           (text (and parse (org-parse-text parse)))
           (headline (and tree (org-headline-on-line tree text (org-parse-point parse)))))
      (cond ((org-table-next-field))
            ((expand-snippet-at-point))
            (headline (org-cycle-headline parse headline))
            ((and *org-cycle-include-plain-lists* tree
                  (let ((item (org-item-on-line tree (string-line-start text (org-parse-point parse)))))
                    (and item (org-cycle-item parse item)))))
            (t (indent-for-tab-command))))))

(defun org-overview (parse)
  "Show only the headlines of the first level of PARSE's buffer, all else folded."
  (tuatara.display:unfold-all (org-parse-buffer parse))
  (dolist (headline (org-node-children (org-parse-tree parse)))
    (when (eq (org-node-type headline) 'headline)
      (let ((range (org-fold-range headline (org-parse-text parse))))
        (when range (org-fold-chars parse (car range) (cdr range)))))))

(defun org-contents (parse)
  "Show all the headlines of PARSE's buffer, their own text folded."
  (tuatara.display:unfold-all (org-parse-buffer parse))
  (dolist (headline (org-headlines (org-parse-tree parse)))
    (let ((section (org-section-range headline (org-parse-text parse))))
      (when section (org-fold-chars parse (car section) (cdr section))))))

(define-command org-global-cycle ()
  "Cycle the whole buffer: its overview (the headlines of the first level),
then its contents (every headline), then everything, and the overview again.
In a table, go back a field, aligned."
  (let ((buffer (current-buffer)))
    (let ((parse (org-parse-now buffer)))
      (when (and parse (not (org-table-previous-field)))
        (case (tuatara.buffer:buffer-local buffer 'org-global-view)
          (:overview (org-contents parse)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :contents)
                     (message "Contents"))
          (:contents (tuatara.display:unfold-all buffer)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :all)
                     (message "Show all"))
          (t (org-overview parse)
             (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :overview)
             (message "Overview")))
        (tuatara.display:point-out-of-folds buffer nil)))))

(defun org-startup-visibility (buffer options)
  "Fold BUFFER as its #+STARTUP: words OPTIONS ask (ORG-STARTUP-OPTIONS), the
last of them saying: overview (or fold), content, or showall (nofold); the
file parsed only when one of the first two does."
  (let ((choice (org-startup-choice options :visibility)))
    (when (member choice '("overview" "fold" "content") :test #'string=)
      (let ((parse (org-parse-now buffer)))
        (when parse
          (if (string= choice "content")
              (progn (org-contents parse)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :contents))
              (progn (org-overview parse)
                     (setf (tuatara.buffer:buffer-local buffer 'org-global-view) :overview))))))))

(defparameter *org-fold-metadata-most* (* 4 1024 1024)
  "The largest Org file, in octets, whose drawers and notes are folded as it
opens (*ORG-FOLD-METADATA*): each of its lines is looked at then. A larger
one opens at once with them shown, each folded by \\`TAB' on its line.")

(defun org-startup-metadata (buffer options)
  "Fold the drawers and notes of BUFFER's tasks (ORG-FOLD-DRAWERS-IN) as its
#+STARTUP: words OPTIONS ask, hidedrawers or showdrawers, the last of them
saying; with neither, as *ORG-FOLD-METADATA* says. A file larger than
*ORG-FOLD-METADATA-MOST* is left as it is."
  (let ((choice (org-startup-choice options :drawers))
        (size (tuatara.buffer:buffer-size buffer)))
    (when (and (if choice (string= choice "hidedrawers") *org-fold-metadata*)
               (<= size *org-fold-metadata-most*))
      (org-fold-drawers-in buffer 0 size))))

(defmethod mode-enter ((mode org-mode) buffer)
  "Show BUFFER as its #+STARTUP: lines say: folded or open, indented or not,
the drawers and notes of its tasks folded (*ORG-FOLD-METADATA*)."
  (call-next-method)
  (let ((options (ignoring-errors-reported ("reading the startup options of ~a" (tuatara.buffer:buffer-name buffer))
                   (org-startup-options buffer))))
    (ignoring-errors-reported ("folding the drawers of ~a" (tuatara.buffer:buffer-name buffer))
      (org-startup-metadata buffer options))
    (ignoring-errors-reported ("folding ~a as it opens" (tuatara.buffer:buffer-name buffer))
      (org-startup-visibility buffer options))
    (ignoring-errors-reported ("indenting ~a as it opens" (tuatara.buffer:buffer-name buffer))
      (org-startup-indentation buffer options))
    (ignoring-errors-reported ("showing the pictures of ~a" (tuatara.buffer:buffer-name buffer))
      (org-startup-pictures buffer options))))

(defparameter *org-mode-locals*
  '(:line-scale-function :line-prefix-function :folds :inline-pictures-function :row-pictures-function
    :tall-lines-function :line-float-function :float-before-function inline-picture-blocks
    org-indent org-links-literal org-state org-parts org-fresh-returns org-hidden org-hidden-anchors
    org-src-colours org-laid-out-ranges org-formula-preamble org-global-view)
  "The buffer-locals Org mode gives a buffer, each let go of as it leaves the
mode: its view (indentation, signs, folds, large headlines, pictures), its
parse and its parts, and what is kept from them.")

(defmethod mode-exit ((mode org-mode) buffer)
  "Let go of the view Org mode gave BUFFER and of its parse
\(*ORG-MODE-LOCALS*), so that the mode that follows shows the text as it is
and the parse is not kept."
  (dolist (local *org-mode-locals*)
    (setf (tuatara.buffer:buffer-local buffer local) nil))
  (setf (tuatara.display:buffer-replacements-function buffer) nil)
  (call-next-method))

(defun install-org-folding (editor)
  "Bind TAB and S-TAB in Org buffers to cycle their subtrees, and keep point out of what is folded."
  (let ((keymap (mode-keymap 'org-mode)))
    (define-key keymap "TAB" 'org-cycle)
    (define-key keymap "S-TAB" 'org-global-cycle))
  (pushnew 'note-point-before-command (editor-pre-command-functions editor))
  (pushnew 'keep-point-out-of-folds (editor-post-command-functions editor))
  (pushnew 'open-stale-org-folds-after-command (editor-post-command-functions editor)))
