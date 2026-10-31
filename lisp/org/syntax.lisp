;;;; lisp/org/syntax.lisp --- Org's syntax: a document parsed into its elements and objects.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Org document parsed as the Org Syntax specification describes it, into
;;;; the tree Emacs's org-element makes: the same types of node, the same
;;;; positions and the same properties, so that what Org mode does in Emacs can
;;;; be done here on the same reading of a file. Elements (headlines, sections,
;;;; paragraphs, lists, tables, blocks, drawers...) are read a line at a time;
;;;; the objects inside them (markup, links, timestamps...) a character at a
;;;; time. Positions are characters from 0. test/org.lisp compares the tree
;;;; with Emacs's own (tools/org-oracle.el) for a corpus of files.

(in-package #:tuatara.defaults)

;;; The tree

(defstruct (org-node (:constructor make-org-node (type &key begin end contents-begin contents-end
                                                       (post-blank 0) post-affiliated properties)))
  "A node of a parsed Org document: TYPE (headline, paragraph, bold...); BEGIN
and END, where it is in the text, END past the blank lines after it; where
its contents are (CONTENTS-BEGIN, CONTENTS-END), when it has any; POST-BLANK,
how many blank lines (for an object, spaces) follow it; POST-AFFILIATED, where
it begins after the keywords naming or captioning it; PROPERTIES, a plist of
what else is known of it (a headline's level, a link's path...); CHILDREN,
the nodes in it, in order; and PARENT."
  type begin end contents-begin contents-end (post-blank 0) post-affiliated properties
  (children '()) parent)

(setf (documentation 'org-node-p 'function) "True when OBJECT is an ORG-NODE.")

(defun org-property (node key)
  "The property KEY of NODE (ORG-NODE-PROPERTIES), or NIL."
  (getf (org-node-properties node) key))

(defun org-adopt (parent children)
  "Make CHILDREN, a list of nodes, PARENT's, in order. Returns PARENT."
  (dolist (child children) (setf (org-node-parent child) parent))
  (remf (org-node-properties parent) :children-vector)
  (setf (org-node-children parent) children)
  parent)

(defun org-walk-tree (node function)
  "Call FUNCTION with NODE and every node below it, a headline's title too, in order."
  (funcall function node)
  (dolist (object (and (eq (org-node-type node) 'headline) (org-property node :title)))
    (org-walk-tree object function))
  (dolist (child (org-node-children node))
    (org-walk-tree child function)))

(defparameter *org-greater-elements*
  '(org-data section headline center-block drawer dynamic-block footnote-definition item
    plain-list property-drawer quote-block special-block table)
  "The elements made of other elements.")

;;; Reading the text

(defvar *org-text* ""
  "The text being parsed.")

(defvar *org-object-limit* 0
  "The end of the text whose objects are being parsed: no object goes past it.")

(defvar *org-object-begin* 0
  "The start of the text whose objects are being parsed: as a line's start to
what begins there, nothing before it seen.")

(defparameter *org-deepest* 200
  "The most elements and objects of Org text the parser reads one within
another: what lies deeper is read as plain text, so that a file made of
nothing but nesting cannot exhaust the parser.")

(defvar *org-depth* 0
  "How many elements and objects the parser is within.")

(defvar *org-emphasis-misses* '()
  "While the objects of a stretch of text are parsed, a plist of each
emphasis mark (*, /, _...) and where on no closing one is to be found before
the stretch's end: each opening mark looks no further than once.")

(defvar *org-close-misses* '()
  "While the objects of a stretch of text are parsed, each (NEEDLE . FROM):
where on the closing NEEDLE (\\), @@, ]]) is not to be found before the
stretch's end (ORG-SEARCH-CLOSE).")

(defvar *org-standard-todo-keywords* '(("TODO" "WAIT") . ("DONE" "CANCELLED"))
  "The TODO keywords of an Org file with no #+TODO: line of its own, as
\(NOT-DONE . DONE): TODO and WAIT for tasks open, DONE and CANCELLED for
tasks closed. A file's #+TODO: line, such as #+TODO: TODO NEXT | DONE, sets
its own instead.")

(defparameter *org-cancel-keywords* '("CANCELLED" "CANCELED")
  "The done keywords that cancel a task rather than finish it, in the order
\\[org-cancel-task] looks for them in a file's keywords.")

(defvar *org-todo-keywords* *org-standard-todo-keywords*
  "The keywords of the document being parsed that make a headline a task: (NOT-DONE . DONE).")

(declaim (inline org-char))
(defun org-char (position)
  "The character at POSITION of the text, or NIL past its end."
  (and (< -1 position (length *org-text*)) (char *org-text* position)))

(defun org-line-end (position)
  "Where the line holding POSITION ends: its newline, or the text's end."
  (string-line-end *org-text* position))

(defun org-next-line (position)
  "Where the line after the one holding POSITION begins, or the text's end."
  (min (length *org-text*) (1+ (org-line-end position))))

(defun org-line-start (position)
  "Where the line holding POSITION begins."
  (string-line-start *org-text* position))

(defun org-line-at (text position)
  "The line of TEXT beginning at POSITION, without its newline."
  (subseq text position (string-line-end text position)))

(defun org-line (position)
  "The line beginning at POSITION, without its newline."
  (org-line-at *org-text* position))

(defun org-blank-line-p (position)
  "True when the line beginning at POSITION holds only spaces and tabs."
  (loop for i from position below (org-line-end position)
        always (member (char *org-text* i) '(#\Space #\Tab #\Return))))

(defun org-skip-blanks-forward (position limit)
  "POSITION past the spaces, tabs, returns and newlines from it, short of LIMIT."
  (loop while (and (< position limit) (blank-char-p (char *org-text* position))) do (incf position))
  position)

(defun org-skip-blanks-backward (position floor)
  "POSITION back over the spaces, tabs, returns and newlines before it, not past FLOOR."
  (loop while (and (> position floor) (blank-char-p (char *org-text* (1- position)))) do (decf position))
  position)

(defun org-end-of-content (position floor)
  "Where the content before POSITION ends, its blank lines left out: the start
of the line after its last character that is not blank (not before FLOOR)."
  (let ((last (org-skip-blanks-backward position floor)))
    (if (= last floor) floor (org-next-line (1- last)))))

(defun org-count-blank-lines (from to)
  "How many lines begin from FROM below TO: the blank lines between content and an end."
  (loop for p = from then (org-next-line p)
        while (< p to)
        count t
        when (= p (length *org-text*)) do (loop-finish)))

(defun org-indentation (position)
  "The columns of spaces and tabs the line beginning at POSITION begins with, a tab to the next multiple of eight."
  (loop with column = 0
        for i from position below (length *org-text*)
        for c = (char *org-text* i)
        do (case c
             (#\Space (incf column))
             (#\Tab (setf column (tuatara.display:next-tab-stop column 8)))
             (t (return column)))
        finally (return column)))

(defvar *org-scanners* (make-hash-table :test 'equal :synchronized t)
  "The anchored scanners made from the patterns ORG-LOOKING-AT and ORG-SCAN are given.")

(defun org-character-property (name)
  "The test of a character \\p{NAME} stands for in Org's patterns:
Alnum, a letter or digit of any script; Alpha, a letter."
  (cond ((string= name "Alnum") #'alphanumericp)
        ((string= name "Alpha") #'alpha-char-p)
        (t (error "No such character property in Org's patterns: ~a" name))))

(defvar *org-parse-scanners* nil
  "While a text is parsed, the scanners ORG-RE gave it, by the pattern
itself (EQ), on the parse's own thread: found again with no lock and no
hashing of the pattern's text, a parse asking for the same few patterns at
every line.")

(defun org-re (pattern)
  "The scanner matching PATTERN only where it is told to begin (\\A), lines'
starts and ends matching ^ and $, \\p{Alnum} a letter or digit; made once
for each pattern (*ORG-SCANNERS*), kept for a parse too
\(*ORG-PARSE-SCANNERS*)."
  (flet ((shared ()
           (or (gethash pattern *org-scanners*)
               (setf (gethash pattern *org-scanners*)
                     (let ((cl-ppcre:*property-resolver* #'org-character-property))
                       (cl-ppcre:create-scanner (format nil "\\A(?:~a)" pattern) :multi-line-mode t))))))
    (let ((own *org-parse-scanners*))
      (if own
          (or (gethash pattern own) (setf (gethash pattern own) (shared)))
          (shared)))))

(defmacro org-scan ((&rest registers) pattern position &body body)
  "When PATTERN matches the text at POSITION (and only there), within its line
(or, for a pattern given as (PATTERN :END END), up to END), run BODY with
REGISTERS bound to the strings of its groups and MATCH-END to where it ended;
else NIL. Bounded so, a pattern's search never goes through the rest of the text."
  (let ((start (gensym)) (end (gensym)) (rs (gensym)) (re (gensym)) (p (gensym))
        (limit (if (and (consp pattern) (eq (second pattern) :end)) (third pattern) nil))
        (pattern (if (and (consp pattern) (eq (second pattern) :end)) (first pattern) pattern)))
    `(multiple-value-bind (,start ,end ,rs ,re)
         (let ((,p ,position))
           (cl-ppcre:scan (org-re ,pattern) *org-text* :start ,p :end ,(or limit `(org-line-end ,p))))
       (declare (ignorable ,re ,rs))
       (when ,start
         (let ((match-end ,end)
               ,@(loop for register in registers
                       for i from 0
                       collect `(,register (and (aref ,rs ,i) (subseq *org-text* (aref ,rs ,i) (aref ,re ,i))))))
           (declare (ignorable match-end))
           ,@body)))))

(defun org-looking-at (pattern position &optional (limit (org-line-end position)))
  "True when PATTERN matches the text at POSITION (and only there), within its
line or up to LIMIT; its end as a second value."
  (multiple-value-bind (start end) (cl-ppcre:scan (org-re pattern) *org-text* :start position :end limit)
    (and start (values t end))))

(defvar *org-line-misses* nil
  "While a text is parsed, (TEXT . TABLE): for each pattern ORG-SEARCH-LINE
looked for in TEXT and did not find, (FROM . LIMIT), the stretch looked
through, so that the same pattern is not looked for within it again.")

(defun org-search-line (regex from limit)
  "The start of the first line from FROM below LIMIT that REGEX matches at its
start, or NIL. While a text is parsed, a stretch where REGEX was not found
is not looked through again (*ORG-LINE-MISSES*)."
  (let* ((misses (and *org-line-misses* (eq (car *org-line-misses*) *org-text*) (cdr *org-line-misses*)))
         (miss (and misses (gethash regex misses))))
    (unless (and miss (<= (car miss) from) (<= limit (cdr miss)))
      (or (loop for p = from then (org-next-line p)
                while (< p limit)
                when (org-looking-at regex p) return p
                when (>= (org-next-line p) (length *org-text*)) return nil)
          (progn
            (when misses
              (setf (gethash regex misses)
                    (cond ((null miss) (cons from limit))
                          ;; one stretch on from the other: both together
                          ((<= from (car miss) limit) (cons from (max limit (cdr miss))))
                          ((> limit (cdr miss)) (cons from limit))
                          (t miss))))
            nil)))))

;;; Closing lines, found once a parse

(defun org-parse-memo (key make)
  "What KEY stands for in the text being parsed, made by calling MAKE the
first time and kept for the rest of the parse (*ORG-LINE-MISSES*); made
again each time outside a parse."
  (let ((table (and *org-line-misses* (eq (car *org-line-misses*) *org-text*) (cdr *org-line-misses*))))
    (if table
        (multiple-value-bind (value found) (gethash key table)
          (if found value (setf (gethash key table) (funcall make))))
        (funcall make))))

(defun org-closing-lines (kind)
  "A table from each name to the starts, in order, of the lines of the text
being parsed that close what it names: for KIND :BLOCK the lines #+END and
the name (_SRC, or nothing) written in capitals, for :LATEX the lines
\\end{NAME}."
  (let ((table (make-hash-table :test 'equal)))
    (cl-ppcre:do-scans (s e rs re (if (eq kind :block)
                                      (load-time-value (cl-ppcre:create-scanner "(?im)^[ \\t]*#\\+END(\\S*)[ \\t]*$"))
                                      (load-time-value (cl-ppcre:create-scanner "(?m)^[ \\t]*\\\\end\\{(.*)\\}[ \\t]*$")))
                       *org-text*)
      (let ((name (subseq *org-text* (aref rs 0) (aref re 0))))
        (when (eq kind :block) (setf name (string-upcase name)))
        (vector-push-extend s (or (gethash name table)
                                  (setf (gethash name table) (make-array 4 :adjustable t :fill-pointer 0))))))
    table))

(defun org-closing-line (kind name from limit)
  "The start of the first line from FROM below LIMIT that closes what NAME
opened, or NIL: for KIND :BLOCK a line #+END and NAME (_SRC, or nothing),
case ignored; for :LATEX a line \\end{NAME}. The closing lines of the text
are found once a parse, whatever names it holds."
  (let ((lines (gethash (if (eq kind :block) (string-upcase name) name)
                        (org-parse-memo kind (lambda () (org-closing-lines kind))))))
    (when lines
      (let ((i (tuatara.runtime:bisect (mid 0 (length lines)) (< (aref lines mid) from))))
        (and (< i (length lines)) (< (aref lines i) limit) (aref lines i))))))

(defun org-headline-lines ()
  "The starts of the headlines of the text being parsed and their numbers of
stars, as two vectors in a cons."
  (let ((starts (make-array 8 :adjustable t :fill-pointer 0))
        (stars (make-array 8 :adjustable t :fill-pointer 0)))
    (cl-ppcre:do-matches (s e (load-time-value (cl-ppcre:create-scanner "(?m)^\\*+ ")) *org-text*)
      (vector-push-extend s starts)
      (vector-push-extend (- e s 1) stars))
    (cons starts stars)))

(defun org-headline-end (level from limit)
  "The start of the first headline from FROM below LIMIT of LEVEL stars or
fewer, or NIL. The headlines of the text are found once a parse."
  (destructuring-bind (starts . stars) (org-parse-memo :headlines #'org-headline-lines)
    (loop for i from (tuatara.runtime:bisect (mid 0 (length starts)) (< (aref starts mid) from)) below (length starts)
          while (< (aref starts i) limit)
          when (<= (aref stars i) level) return (aref starts i))))

;;; Element-level regular expressions (org-element's)

(defun org-timestamp-body (&optional groups)
  "What a timestamp holds between its brackets, as a pattern: its date, the
day's name (any word, lun. too), its time or times, and its repeaters and
warnings (+1w .+1d ++2m/3m -3d --2d) in any order, spaces before the
bracket. With GROUPS, the year, month, day, the hour and minute it begins and
ends, and the repeaters and warnings together are groups, in that order
\(ORG-READ-TS)."
  (flet ((g (pattern) (format nil (if groups "(~a)" "(?:~a)") pattern)))
    (format nil "~a-~a-~a(?: +[^]+0-9>\\s-]+)?(?: +~a:~a(?:-~a:~a)?)?~a *"
            (g "\\d{4}") (g "\\d{2}") (g "\\d{2}")
            (g "\\d{1,2}") (g "\\d{2}") (g "\\d{1,2}") (g "\\d{2}")
            (g "(?: +(?:\\.\\+|\\+\\+|\\+|--|-)\\d+[hdwmy](?:/\\d+[hdwmy])?)*"))))

(defun org-rule-timestamp-body (&optional groups)
  "What a timestamp by rule holds between its brackets, as a pattern: %%, its
form in parentheses, and a time of day or a range of two after it, an hour
to 23 and minutes to 59, a range ending at 24:00 too
\(%%(diary-float t 1 1) 10:00-11:00). With GROUPS, the form within its
parentheses and the time are groups, in that order (ORG-READ-RULE)."
  (let ((hour "(?:[01]?[0-9]|2[0-3]):[0-5][0-9]"))
    (format nil (if groups "%%\\(([^>\\n]*)\\)(?: +(~a(?:-~a)?))?" "%%\\((?:[^>\\n]*)\\)(?: +(?:~a(?:-~a)?))?")
            hour (format nil "(?:~a|24:00)" hour))))

(defparameter *org-headline-re* "\\*+[ \\t]"
  "A headline's start: stars, then a space.")

(defparameter *org-clock-line*
  (let ((stamp (format nil "\\[(~a)\\]" (org-timestamp-body)))
        (duration "[ \\t]+=>[ \\t]+(\\d+:\\d\\d)"))
    (format nil "CLOCK:(?:[ \\t]+~a(?:--~a(?:~a)?)?|~a)[ \\t]*$" stamp stamp duration duration))
  "A clock line from its CLOCK: on: a timestamp, the time a clock started,
then --, the time it stopped and, once written, how long (=> 1:30); or how
long alone. The timestamps and the times are groups.")

(defparameter *org-paragraph-separate*
  (concatenate 'string
               "(?i)(?:\\*+ |\\[fn:[-_\\w]+\\]|%%\\(|[ \\t]*(?:$|\\||\\+(?:-+\\+)+[ \\t]*$"
                "|#(?: |$|\\+(?:BEGIN_|\\S+?(?:\\[.*\\])?:[ \\t]*))"
                "|:(?: |$|[-_\\w]+:[ \\t]*$)"
                "|-{5,}[ \\t]*$"
                "|\\\\begin\\{([A-Za-z0-9*]+)\\}"
                "|" *org-clock-line*
                "|(?:[-+*]|[0-9]+[.)])(?:[ \\t]|$)))")
  "A line that may end a paragraph: what can begin another element.")

(defvar *org-list-allow-alphabetical* nil
  "True while a document is read with lists whose bullets are single letters
\(a. b) A.); NIL for numbers alone, as lists are read as standard. The enhanced
export binds it true.")

(defun org-alphabetical-form (pattern)
  "PATTERN, a regex with numbered bullets [0-9]+[.)], with single letters allowed there too."
  (cl-ppcre:regex-replace-all "\\[0-9\\]\\+\\[\\.\\)\\]" pattern "(?:[0-9]+|[A-Za-z])[.)]"))

(defun org-paragraph-separate ()
  "*ORG-PARAGRAPH-SEPARATE*, letters allowed as bullets while *ORG-LIST-ALLOW-ALPHABETICAL*."
  (if *org-list-allow-alphabetical*
      (load-time-value (org-alphabetical-form *org-paragraph-separate*))
      *org-paragraph-separate*))

(defparameter *org-item-re* "(?:[ \\t]*(?:[-+]|[0-9]+[.)])|[ \\t]+\\*)(?:[ \\t]+|$)"
  "A list item's first line.")

(defun org-item-re ()
  "*ORG-ITEM-RE*, letters allowed as bullets while *ORG-LIST-ALLOW-ALPHABETICAL*."
  (if *org-list-allow-alphabetical*
      (load-time-value (org-alphabetical-form *org-item-re*))
      *org-item-re*))

(defparameter *org-full-item-re*
  "[ \\t]*((?:[-+*]|(?:[0-9]+|[A-Za-z])[.)])(?:[ \\t]+|$))(?:\\[@(?:start:)?([0-9]+|[A-Za-z])\\][ \\t]*)?(?:(\\[[ X-]\\])(?:[ \\t]+|$))?(?:(.*)[ \\t]+::(?:[ \\t]+|$))?"
  "A list item's first line, its parts: bullet, counter, checkbox, tag.")

(defparameter *org-drawer-re* "[ \\t]*:((?:\\w|[-_])+):[ \\t]*$"
  "A drawer's first line, its name.")

(defparameter *org-drawer-end-re* "(?i)[ \\t]*:END:[ \\t]*$"
  "A drawer's last line.")

(defparameter *org-property-drawer-re*
  "(?i)[ \\t]*:PROPERTIES:[ \\t]*\\n(?:[ \\t]*:\\S+:(?: .*)?[ \\t]*\\n)*?[ \\t]*:END:[ \\t]*$"
  "A property drawer, whole.")

(defparameter *org-planning-re* "[ \\t]*(?:CLOSED:|DEADLINE:|SCHEDULED:)"
  "A planning line's start.")

(defparameter *org-planning-line-scanner* (cl-ppcre:create-scanner (concatenate 'string "^" *org-planning-re*))
  "*ORG-PLANNING-RE* at a line's start, made ready to match once.")

(defparameter *org-planning-item-scanner*
  (cl-ppcre:create-scanner "(CLOSED|DEADLINE|SCHEDULED):[ \\t]*([<\\[][^]>\\n]*[]>](?:--[<\\[][^]>\\n]*[]>])?)")
  "One keyword of a planning line and its timestamp or range of two, as groups.")

(defun org-planning-line-p (text &key (start 0) (end (length text)))
  "True when the line of TEXT from START (below END) is a planning line:
CLOSED:, DEADLINE: or SCHEDULED: first on it."
  (cl-ppcre:scan *org-planning-line-scanner* text :start start :end end))

(defparameter *org-clock-re* (format nil "(?i)[ \\t]*~a" *org-clock-line*)
  "A clock line, *ORG-CLOCK-LINE* after blanks, case ignored.")

(defun org-read-clock-line (text &optional (start 0) (end (string-line-end text start)))
  "When TEXT from START below END (by default its line) is a clock line
\(*ORG-CLOCK-RE*), three values: the timestamp it started at
\([2026-10-01 Thu 09:00]) or NIL, the one it stopped at or NIL while it runs,
and how long it ran (1:30), written after => and a space, or NIL; NIL when it
is no clock line."
  (multiple-value-bind (match match-end starts ends) (cl-ppcre:scan (org-re *org-clock-re*) text :start start :end end)
    (declare (ignore match-end))
    (when match
      (flet ((group (i) (and (aref starts i) (subseq text (aref starts i) (aref ends i)))))
        (let* ((at (or (aref starts 2) (aref starts 3)))
               ;; how long it ran, written after => and a space, looked for on its own line
               (arrow (and at (search "=>" text :start2 start :end2 at :from-end t))))
          (values (and (group 0) (format nil "[~a]" (string-right-trim " " (group 0))))
                  (and (group 1) (format nil "[~a]" (string-right-trim " " (group 1))))
                  (and arrow (char= (char text (+ arrow 2)) #\Space) (or (group 2) (group 3)))))))))

(defparameter *org-latex-begin-re* "[ \\t]*\\\\begin\\{([A-Za-z0-9*]+)\\}"
  "A LaTeX environment's first line.")

(defparameter *org-footnote-definition-re* "\\[fn:([-_\\w]+)\\]"
  "A footnote definition's start, at a line's start.")

(defparameter *org-dynamic-block-re* "(?i)[ \\t]*#\\+BEGIN:[ \\t]*(\\S+)(?:[ \\t]+(.*?))?[ \\t]*$"
  "A dynamic block's first line: its name and arguments.")

(defparameter *org-dynamic-block-end-re* "(?i)[ \\t]*#\\+END:?[ \\t]*$"
  "A dynamic block's last line.")

(defparameter *org-dual-keywords* '("CAPTION" "RESULTS")
  "The affiliated keywords that take a second value in brackets.")

(defparameter *org-affiliated-keywords* '("CAPTION" "DATA" "HEADER" "HEADERS" "LABEL" "NAME" "PLOT" "RESNAME" "RESULT" "RESULTS" "SOURCE" "SRCNAME" "TBLNAME")
  "The keywords that name, caption or otherwise go with the element after them.")

(defparameter *org-attribute-keywords* '(:attr_latex :attr_html :attr_odt :attr_org :attr_beamer :attr_texinfo :attr_md :attr_ascii)
  "The #+ATTR_...: lines whose settings are kept with the element after them,
one for each kind of export. Other #+ATTR_...: lines still go with the element,
and what they say is passed over.")

(defun org-known-keyword (name)
  "The keyword named NAME, in upper case, when Tuatara or your init file
already uses one by that name, else NIL."
  ;; looked up, never made: a keyword made from a file's name would stay for the
  ;; whole session, and a file with a great many names would grow the editor
  (values (find-symbol (string-upcase name) :keyword)))

;;; Parsing a document

(defvar *org-list-items* nil
  "While a text is parsed, a table from where each item of its plain lists
begins to what its list's structure says of it (ORG-LIST-STRUCTURE), the
list read last for an item in more than one; NIL before the first list.")

(defun org-todo-keywords-of (text)
  "The TODO keywords TEXT's #+TODO:, #+SEQ_TODO: and #+TYP_TODO: lines set, as
(NOT-DONE . DONE), or *ORG-STANDARD-TODO-KEYWORDS* when it has none."
  (let ((not-done '()) (done '()) (found nil))
    (cl-ppcre:do-register-groups (words) ("(?im)^[ \\t]*#\\+(?:TODO|SEQ_TODO|TYP_TODO):[ \\t]*(.*)$" text)
      (setf found t)
      (let* ((names (mapcar (lambda (w) (subseq w 0 (or (position #\( w) (length w))))
                            (uiop:split-string (string-trim " " words) :separator '(#\Space #\Tab))))
             (names (remove "" names :test #'string=))
             (bar (position "|" names :test #'string=)))
        (cond (bar (setf not-done (append not-done (subseq names 0 bar)) done (append done (subseq names (1+ bar)))))
              (names (setf not-done (append not-done (butlast names)) done (append done (last names)))))))
    (if found (cons not-done done) *org-standard-todo-keywords*)))

(defparameter *org-children-by-halves* 32
  "How many children a node of the parse has before those in sight are found
among them by halves rather than one by one.")

(defun org-children-in-range (node from to function)
  "Call FUNCTION on each child of NODE that holds a character from FROM below
TO, in order. Among more than *ORG-CHILDREN-BY-HALVES* children (a flat file
has thousands at its top, a long list or log as many items) the first is
found by halves, the children kept as a vector on NODE the first time; the
rest follow in order until one begins at or past TO."
  (if (nthcdr *org-children-by-halves* (org-node-children node))
      (let* ((children (or (getf (org-node-properties node) :children-vector)
                           (setf (getf (org-node-properties node) :children-vector)
                                 (coerce (org-node-children node) 'simple-vector))))
             (first (tuatara.runtime:bisect (mid 0 (length children))
                                            (let ((child (svref children mid)))
                                              (<= (max (org-node-end child) (1+ (org-node-begin child))) from)))))
        (loop for i from first below (length children)
              for child = (svref children i)
              while (< (org-node-begin child) to)
              do (funcall function child)))
      (dolist (child (org-node-children node))
        (when (and (< (org-node-begin child) to) (> (max (org-node-end child) (1+ (org-node-begin child))) from))
          (funcall function child)))))

(defun org-node-at (tree position test &key innermost titles)
  "The node of TREE holding the character POSITION that TEST, a function of a
node, is true of: the outermost, or with INNERMOST the innermost; with TITLES
the objects of headlines' titles looked in too, each given its headline as
parent. NIL when there is none."
  (let ((found nil))
    (labels ((walk (node)
               (when (and (<= (org-node-begin node) position) (< position (max (org-node-end node) (1+ (org-node-begin node)))))
                 (when (funcall test node)
                   (if innermost (setf found node) (return-from org-node-at node)))
                 (when (and titles (eq (org-node-type node) 'headline))
                   (dolist (object (org-property node :title))
                     (setf (org-node-parent object) node)
                     (walk object)))
                 (org-children-in-range node position (1+ position) #'walk))))
      (walk tree))
    found))

(defun org-before-blank-p (node position)
  "True when the character POSITION is before the blank lines or spaces after NODE."
  (< position (- (org-node-end node) (org-node-post-blank node))))

(defun org-parse (text &key todo-keywords)
  "TEXT, an Org document, parsed: its tree, an ORG-NODE of type ORG-DATA.
TODO-KEYWORDS, (NOT-DONE . DONE), are the keywords that make a headline a
task, given for TEXT that is a part of a document whose #+TODO: lines may be
elsewhere; those TEXT's own lines set when not given (ORG-TODO-KEYWORDS-OF)."
  (let* ((text (coerce text 'simple-string))
         (*org-text* text)
         (*org-todo-keywords* (or todo-keywords (org-todo-keywords-of text)))
         (*org-list-items* nil)
         (*org-line-misses* (cons text (make-hash-table :test 'equal :size 8)))
         (*org-parse-scanners* (make-hash-table :test 'eq :size 8))
         (root (org-empty-tree (length text))))
    (org-adopt root (org-parse-elements 0 (length text) :first-section))))

(defun org-empty-tree (length)
  "The root of a parse of an Org text of LENGTH characters, with nothing in
it yet: an ORG-NODE of type ORG-DATA over all of the text."
  (make-org-node 'org-data :begin 0 :end length :contents-begin 0 :contents-end length :post-affiliated 0))

(defparameter *org-standard-objects*
  '(bold code entity export-snippet footnote-reference inline-babel-call inline-src-block italic
    line-break latex-fragment link macro radio-target statistics-cookie strike-through subscript
    superscript target timestamp underline verbatim citation)
  "The objects text may hold.")

(defun org-parse-line (text)
  "TEXT, a line of Org text such as a keyword's value, parsed as a line of
words: its objects alone, in a paragraph node, never a list, heading or table
whatever it begins with (A. Writer stays A. Writer)."
  (let* ((text (coerce text 'simple-string))
         (*org-text* text)
         (*org-list-items* nil)
         (*org-parse-scanners* (make-hash-table :test 'eq :size 8))
         (paragraph (make-org-node 'paragraph :begin 0 :end (length text) :contents-begin 0 :contents-end (length text)
                                              :post-affiliated 0)))
    (org-adopt paragraph (org-parse-objects 0 (length text) *org-standard-objects*))))

(defun org-parse-elements (begin end mode)
  "The elements from BEGIN below END, in order, the first read as MODE says
(:first-section, :section, :planning, :item, :node-property, :table-row...),
each with its contents parsed."
  (let ((elements '()) (position begin))
    (loop while (< position end)
          do (let ((element (org-current-element position end mode)))
               (org-parse-contents element)
               (push element elements)
               (setf position (org-node-end element)
                     mode (org-next-mode mode (org-node-type element)))))
    (nreverse elements)))

(defun org-next-mode (mode type)
  "How the element after one of TYPE, read as MODE, is read."
  (case mode
    (:item :item)
    (:node-property :node-property)
    (:table-row :table-row)
    (:planning (if (eq type 'planning) :property-drawer nil))
    (:first-section nil)
    (t (if (eq type 'headline) :section nil))))

(defun org-child-mode (type)
  "How the first element inside one of TYPE is read."
  (case type
    (headline :section)
    (section :planning)
    (plain-list :item)
    (property-drawer :node-property)
    (table :table-row)
    (t nil)))

(defun org-parse-contents (element)
  "Parse ELEMENT's contents, when it has any: the elements of a greater
element, the objects of one that holds text; none deeper than *ORG-DEEPEST*."
  (let ((type (org-node-type element))
        (begin (org-node-contents-begin element))
        (end (org-node-contents-end element))
        (*org-depth* (1+ *org-depth*)))
    (cond ((> *org-depth* *org-deepest*))
          ((and (member type *org-greater-elements*) begin end (< begin end) (not (eq type 'table)))
           (org-adopt element (org-parse-elements begin end (org-child-mode type))))
          ((and (eq type 'table) (eq (org-property element :type) 'org) begin end)
           (org-adopt element (org-parse-elements begin end :table-row)))
          ((and (member type '(paragraph verse-block table-row)) begin end (< begin end))
           (org-adopt element (org-parse-objects begin end (org-object-types type)))))
    ;; a headline's title's objects are kept apart, as a secondary string, whether it has contents or not
    (when (and (eq type 'headline) (org-property element :title-begin))
      (let ((title (org-parse-objects (org-property element :title-begin) (org-property element :title-end)
                                      (org-object-types 'headline))))
        (dolist (object title) (setf (org-node-parent object) element))
        (setf (getf (org-node-properties element) :title) title)))
    element))

(defun org-current-element (position limit mode)
  "The element beginning at POSITION, not past LIMIT, read as MODE says."
  (cond
    ((eq mode :item) (org-item-parser position limit))
    ((eq mode :table-row) (org-table-row-parser position limit))
    ((eq mode :node-property) (org-node-property-parser position limit))
    ((and (= position (org-line-start position)) (org-looking-at *org-headline-re* position))
     (org-headline-parser position limit))
    ((member mode '(:section :first-section))
     (org-section-parser position (or (org-next-headline position limit) limit)))
    ((org-looking-at "[ \\t]*#(?: |$)" position) (org-comment-parser position limit))
    ((and (eq mode :planning) (plusp position) (eql #\* (org-char (org-line-start (1- position))))
          (org-looking-at *org-planning-re* position))
     (org-planning-parser position limit))
    ((and (member mode '(:planning :property-drawer))
          (plusp position)
          (if (eq mode :planning)
              (eql #\* (org-char (org-line-start (1- position))))
              (not (org-blank-line-p (org-line-start (1- position)))))
          (org-looking-at *org-property-drawer-re* position limit))
     (org-property-drawer-parser position limit))
    ((/= position (org-line-start position)) (org-paragraph-parser position limit position))
    ((org-looking-at *org-clock-re* position) (org-clock-parser position limit))
    (t
     (multiple-value-bind (start affiliated) (org-collect-affiliated position limit)
       (cond
         ;; keywords with nothing after them to belong to: the first a keyword of its own
         ((and affiliated (>= start limit))
          (org-keyword-parser position position limit nil))
         ((org-looking-at *org-latex-begin-re* start) (org-latex-environment-parser position start limit affiliated))
         ((org-looking-at *org-drawer-re* start) (org-drawer-parser position start limit affiliated))
         ((org-looking-at "[ \\t]*:(?: |$)" start) (org-fixed-width-parser position start limit affiliated))
         ((org-looking-at "[ \\t]*#\\+" start)
          (multiple-value-bind (ok after) (org-looking-at "[ \\t]*#\\+" start)
            (declare (ignore ok))
            (cond ((org-looking-at "(?i)BEGIN_(\\S+)" after) (org-block-parser position start limit affiliated))
                  ((org-looking-at "(?i)CALL:" after) (org-babel-call-parser position start limit affiliated))
                  ((org-looking-at *org-dynamic-block-re* start) (org-dynamic-block-parser position start limit affiliated))
                  ((org-looking-at "\\S+?:" after) (org-keyword-parser position start limit affiliated))
                  (t (org-paragraph-parser position limit start affiliated)))))
         ((org-looking-at *org-footnote-definition-re* start) (org-footnote-definition-parser position start limit affiliated))
         ((org-looking-at "[ \\t]*-{5,}[ \\t]*$" start) (org-horizontal-rule-parser position start limit affiliated))
         ((org-looking-at "%%\\(" start) (org-diary-sexp-parser position start limit affiliated))
         ((or (org-looking-at "[ \\t]*\\|" start)
              (and (org-looking-at "[ \\t]*\\+-[-+]*\\+[ \\t]*$" start)
                   (org-looking-at "[ \\t]*\\|" (org-next-line start))))
          (org-table-parser position start limit affiliated))
         ((org-looking-at (org-item-re) start) (org-plain-list-parser position start limit affiliated))
         (t (org-paragraph-parser position limit start affiliated)))))))

(defun org-next-headline (position limit)
  "Where the next headline after the line at POSITION begins, below LIMIT, or NIL."
  (org-search-line *org-headline-re* (org-next-line position) limit))

;;; Affiliated keywords

(defun org-collect-affiliated (position limit)
  "The affiliated keywords from POSITION (#+NAME:, #+CAPTION:, #+ATTR_...: lines
before an element), as two values: where the element itself begins, and a
plist of their values (:name, :caption, :attr_html...), NIL when there are none."
  (let ((p position) (found '()))
    (loop
      (when (>= p limit) (return))
      (let ((matched nil))
        (org-scan (key) "[ \\t]*#\\+((?:[Aa][Tt][Tt][Rr]_[-_A-Za-z0-9]+)|[A-Za-z]+)(?:\\[.*\\])?:[ \\t]*" p
          (when (or (member (string-upcase key) *org-affiliated-keywords* :test #'string=)
                    (uiop:string-prefix-p "ATTR_" (string-upcase key)))
            (let ((value (string-trim " 	" (subseq *org-text* match-end (org-line-end p)))))
              (setf matched t)
              (let* ((upper (string-upcase key))
                     (name (cond ((member upper '("DATA" "LABEL" "RESNAME" "SOURCE" "SRCNAME" "TBLNAME") :test #'string=) :name)
                                 ((string= upper "RESULT") :results)
                                 ((string= upper "HEADERS") :header)
                                 ((uiop:string-prefix-p "ATTR_" upper)
                                  (find upper *org-attribute-keywords* :key #'symbol-name :test #'string=))
                                 ;; one of *org-affiliated-keywords*, a fixed few
                                 (t (intern upper :keyword)))))
                (when (and name (not (getf found name))) (setf (getf found name) value))))))
        (if matched (setf p (org-next-line p)) (return))))
    (values p found)))

(defun org-element-with-affiliated (node position start affiliated)
  "NODE given its affiliated keywords: it begins at POSITION, where they do,
and after them at START; AFFILIATED, their plist."
  (setf (org-node-begin node) position
        (org-node-post-affiliated node) start)
  (when (getf affiliated :name)
    (setf (getf (org-node-properties node) :name) (getf affiliated :name)))
  (when affiliated
    (setf (getf (org-node-properties node) :affiliated) affiliated))
  node)

(defun org-count-lines (from to)
  "How many lines lie between FROM and TO: the newlines between, and one more when TO is not where a line begins."
  (if (= from to)
      0
      (+ (count #\Newline *org-text* :start from :end to)
         (if (= to (org-line-start to)) 0 1))))

(defun org-finish (node content-end limit &key (count-from content-end))
  "NODE ended after CONTENT-END, the blank lines after it to LIMIT counted in:
its END past them and its POST-BLANK, the lines from COUNT-FROM to there."
  (let* ((after (org-skip-blanks-forward content-end limit))
         (end (if (>= after limit) limit (org-line-start after))))
    (setf (org-node-end node) end
          (org-node-post-blank node) (org-count-lines count-from end))
    node))

;;; Headlines and sections

(defun org-headline-parser (position limit)
  "The headline at POSITION, its subtree to LIMIT."
  (let* ((line-end (org-line-end position))
         (level (loop for i from position while (eql (org-char i) #\*) count t))
         (p (+ position level))
         (properties (list :level level)))
    (setf p (loop while (member (org-char p) '(#\Space #\Tab)) do (incf p) finally (return p)))
    ;; the TODO keyword, followed by a space or the line's end
    (let ((keyword (find-if (lambda (k) (and (<= (+ p (length k)) line-end)
                                             (string= k *org-text* :start2 p :end2 (+ p (length k)))
                                             (or (= (+ p (length k)) line-end) (eql (org-char (+ p (length k))) #\Space))))
                            (append (car *org-todo-keywords*) (cdr *org-todo-keywords*)))))
      (when keyword
        (setf (getf properties :todo-keyword) keyword
              (getf properties :todo-type) (if (member keyword (car *org-todo-keywords*) :test #'string=) 'todo 'done))
        (setf p (+ p (length keyword)))
        (setf p (loop while (member (org-char p) '(#\Space #\Tab)) do (incf p) finally (return p)))))
    ;; the priority
    (org-scan () "\\[#.\\][ \\t]*" p
      (setf (getf properties :priority) (char-code (org-char (+ p 2))) p match-end))
    ;; COMMENT
    (when (and (<= (+ p 7) line-end) (string= "COMMENT" *org-text* :start2 p :end2 (+ p 7))
               (or (= (+ p 7) line-end) (eql (org-char (+ p 7)) #\Space)))
      (setf (getf properties :commentedp) t
            p (+ p 7))
      (setf p (loop while (member (org-char p) '(#\Space #\Tab)) do (incf p) finally (return p))))
    ;; the tags at the line's end
    (let* ((line (subseq *org-text* p line-end))
           (title-end line-end))
      (multiple-value-bind (start end rs re) (cl-ppcre:scan "[ \\t]+(:[\\w@#%:]+:)[ \\t]*$" line)
        (declare (ignore end))
        (when start
          (let ((tags (tuatara.text:split-words (subseq line (aref rs 0) (aref re 0)) ":")))
            (setf (getf properties :tags) tags
                  title-end (+ p start))
            (when (member "ARCHIVE" tags :test #'string=) (setf (getf properties :archivedp) t)))))
      (let* ((title-begin (min p title-end))
             (raw (string-trim " 	" (subseq *org-text* title-begin title-end))))
        (setf (getf properties :raw-value) raw
              (getf properties :title-begin) title-begin
              (getf properties :title-end) (org-skip-blanks-backward title-end title-begin))))
    (let* ((end (or (org-headline-end level (org-next-line position) limit) limit))
           (contents-begin (let ((c (org-skip-blanks-forward (org-next-line position) end)))
                             (and (< c end) (org-line-start c))))
           ;; its blank lines its section's
           (contents-end (and contents-begin end))
           (node (make-org-node 'headline :begin position :end end :contents-begin contents-begin
                                          :contents-end contents-end :post-affiliated position
                                          :properties properties)))
      (setf (org-node-post-blank node)
            (org-count-blank-lines (or contents-end (org-next-line position)) end))
      node)))

(defun org-section-parser (position limit)
  "The section at POSITION, the elements up to LIMIT, the next headline, its
blank lines its own."
  (make-org-node 'section :begin position :end limit :contents-begin position
                          :contents-end limit :post-affiliated position))

(defun org-planning-parser (position limit)
  "The planning line at POSITION: when the task was closed, its deadline, when it is scheduled."
  (let* ((line-end (org-line-end position))
         (properties '()))
    (cl-ppcre:do-scans (s e rs re *org-planning-item-scanner* *org-text*
                          nil :start position :end line-end)
      (let ((keyword (subseq *org-text* (aref rs 0) (aref re 0)))
            (stamp (subseq *org-text* (aref rs 1) (aref re 1))))
        (setf (getf properties (cond ((string= keyword "CLOSED") :closed)
                                     ((string= keyword "DEADLINE") :deadline)
                                     (t :scheduled)))
              stamp)))
    (org-finish (make-org-node 'planning :begin position :post-affiliated position :properties properties)
                (org-next-line position) limit)))

(defun org-property-drawer-parser (position limit)
  "The property drawer at POSITION."
  (let* ((end-line (org-search-line *org-drawer-end-re* (org-next-line position) limit))
         (contents-begin (org-next-line position))
         (node (make-org-node 'property-drawer :begin position :post-affiliated position
                                               :contents-begin (and (< contents-begin end-line) contents-begin)
                                               :contents-end (and (< contents-begin end-line) end-line))))
    (org-finish node (org-next-line end-line) limit)))

(defun org-node-property-parser (position limit)
  "The property line at POSITION: its key and value."
  (declare (ignore limit))
  (let ((node (make-org-node 'node-property :begin position :post-affiliated position)))
    (org-scan (key value) "[ \\t]*:(\\S+?):(?:$|[ \\t]+(.*?))[ \\t]*$" position
      (setf (org-node-properties node) (list :key key :value (or value ""))))
    (setf (org-node-end node) (org-next-line position))
    node))

(defun org-clock-parser (position limit)
  "The clock line at POSITION: its time, closed with its duration or still running."
  (let ((node (make-org-node 'clock :begin position :post-affiliated position)))
    (multiple-value-bind (from to duration) (org-read-clock-line *org-text* position (org-line-end position))
      (setf (org-node-properties node)
            (append (list :status (if duration 'closed 'running))
                    (and duration (list :duration duration))
                    (list :value (if to (format nil "~a--~a" from to) from)))))
    (org-finish node (org-next-line position) limit)))

;;; Lesser elements of one line or a few

(defun org-comment-parser (position limit)
  "The comment lines from POSITION (each # then a space, or # alone)."
  (let ((p position) (lines '()))
    (loop while (and (< p limit) (org-looking-at "[ \\t]*#(?: |$)" p))
          do (multiple-value-bind (ok after) (org-looking-at "[ \\t]*# ?" p)
               (declare (ignore ok))
               (push (subseq *org-text* after (org-line-end p)) lines))
             (setf p (org-next-line p)))
    (org-finish (make-org-node 'comment :begin position :post-affiliated position
                                        :properties (list :value (format nil "~{~a~^~%~}" (nreverse lines))))
                p limit)))

(defun org-fixed-width-parser (position start limit affiliated)
  "The fixed-width lines from START (each : then a space, or : alone)."
  (let ((p start) (lines '()))
    (loop while (and (< p limit) (org-looking-at "[ \\t]*:(?: |$)" p))
          do (multiple-value-bind (ok after) (org-looking-at "[ \\t]*: ?" p)
               (declare (ignore ok))
               (push (subseq *org-text* after (org-line-end p)) lines))
             (setf p (org-next-line p)))
    (org-finish (org-element-with-affiliated
                 (make-org-node 'fixed-width :properties (list :value (format nil "~{~a~^~%~}" (nreverse lines))))
                 position start affiliated)
                p limit
                ;; its blank lines counted from the end of its last line
                :count-from (if (and (plusp p) (= p (org-line-start p)) (eql (org-char (1- p)) #\Newline)) (1- p) p))))

(defun org-keyword-parser (position start limit affiliated)
  "The keyword line at START: #+KEY: value."
  (let ((node (make-org-node 'keyword)))
    (org-scan (key value) "[ \\t]*#\\+(\\S+?):[ \\t]*(.*?)[ \\t]*$" start
      (setf (org-node-properties node) (list :key (string-upcase key) :value value)))
    (org-finish (org-element-with-affiliated node position start affiliated) (org-next-line start) limit)))

(defun org-babel-call-parser (position start limit affiliated)
  "The #+CALL: line at START: the call, its headers and arguments."
  (let ((node (make-org-node 'babel-call))
        (value (string-trim " 	" (subseq *org-text* (+ (search ":" *org-text* :start2 start) 1) (org-line-end start)))))
    (setf (org-node-properties node) (org-call-parts value))
    (org-finish (org-element-with-affiliated node position start affiliated) (org-next-line start) limit)))

(defun org-call-parts (value)
  "VALUE, a call as #+CALL: and call_ write it, NAME[INSIDE](ARGUMENTS)[END], as a plist."
  (multiple-value-bind (start end rs re) (cl-ppcre:scan "^([^()\\[\\] \\t]+)(?:\\[([^]]*)\\])?\\(([^)]*)\\)(?:[ \\t]*\\[([^]]*)\\]|[ \\t]+(.*?))?[ \\t]*$" value)
    (declare (ignore end))
    (if start
        (flet ((group (i) (and (aref rs i) (let ((s (subseq value (aref rs i) (aref re i)))) (and (plusp (length s)) s)))))
          (append (list :call (group 0))
                  (and (group 1) (list :inside-header (group 1)))
                  (and (group 2) (list :arguments (group 2)))
                  (and (or (group 3) (group 4)) (list :end-header (or (group 3) (group 4))))))
        (list :call value))))

(defun org-horizontal-rule-parser (position start limit affiliated)
  "The horizontal rule at START: five dashes or more."
  (org-finish (org-element-with-affiliated (make-org-node 'horizontal-rule) position start affiliated)
              (org-next-line start) limit))

(defun org-diary-sexp-parser (position start limit affiliated)
  "The diary sexp at START: %%( and the rest of its line."
  (org-finish (org-element-with-affiliated (make-org-node 'diary-sexp :properties (list :value (org-line start)))
                                           position start affiliated)
              (org-next-line start) limit))

;;; Blocks, drawers and environments

(defun org-block-parser (position start limit affiliated)
  "The block #+BEGIN_NAME at START, ended by #+END_NAME; a paragraph when it is not ended."
  (let* ((name (org-scan (n) "[ \\t]*#\\+(?i:BEGIN_)(\\S+)" start n))
         (end-line (org-closing-line :block (concatenate 'string "_" name) (org-next-line start) limit)))
    (if (null end-line)
        (org-paragraph-parser position limit start affiliated)
        (let* ((type (string-upcase name))
               (contents-begin (org-next-line start))
               (contents (subseq *org-text* contents-begin end-line))
               (parameters (or (org-scan (rest) "[ \\t]*#\\+(?i:BEGIN_)\\S+[ \\t]*(.*?)[ \\t]*$" start rest) ""))
               (element-type (cond ((string= type "CENTER") 'center-block)
                                   ((string= type "COMMENT") 'comment-block)
                                   ((string= type "EXAMPLE") 'example-block)
                                   ((string= type "EXPORT") 'export-block)
                                   ((string= type "QUOTE") 'quote-block)
                                   ((string= type "SRC") 'src-block)
                                   ((string= type "VERSE") 'verse-block)
                                   (t 'special-block)))
               (greater (member element-type '(center-block quote-block special-block)))
               (node (make-org-node element-type)))
          (case element-type
            (src-block
             (multiple-value-bind (language switches params) (org-src-header parameters)
               (setf (org-node-properties node)
                     (append (and language (list :language language))
                             (and switches (list :switches switches))
                             (and params (list :parameters params))
                             (list :value (org-unescape-block contents))))))
            (example-block
             (setf (org-node-properties node)
                   (append (and (plusp (length parameters)) (list :switches parameters))
                           (list :value (org-unescape-block contents)))))
            (export-block
             (setf (org-node-properties node)
                   (list :type (string-upcase (or (first (uiop:split-string parameters)) "")) :value (org-unescape-block contents))))
            (comment-block
             (setf (org-node-properties node) (list :value (org-unescape-block contents))))
            (special-block
             (setf (org-node-properties node) (list :type name))))
          (when (or greater (eq element-type 'verse-block))
            (when (< contents-begin end-line)
              (setf (org-node-contents-begin node) contents-begin
                    (org-node-contents-end node) end-line)))
          (org-finish (org-element-with-affiliated node position start affiliated) (org-next-line end-line) limit)))))

(defun org-unescape-block (contents)
  "CONTENTS of a block as its value: a comma that protects a line beginning
with * or #+ (or with commas and then one of them) taken away."
  (cl-ppcre:regex-replace-all "(?m)^([ \\t]*),(,*[*]|,*#\\+)" contents "\\1\\2"))

(defun org-escape-block (value)
  "VALUE written as a block's contents, as ORG-UNESCAPE-BLOCK reads it back:
a comma before each line beginning with * or #+ (or with commas and then one
of them), after its blanks, so that no line of it ends the block or begins a
heading or a keyword."
  (cl-ppcre:regex-replace-all "(?m)^([ \\t]*)(,*[*]|,*#\\+)" value "\\1,\\2"))

(defun org-src-header (parameters)
  "PARAMETERS of a #+BEGIN_SRC line split: the language, the switches (-n,
-r, -l \"...\"), and the header arguments, as three values."
  (let* ((words (string-trim " 	" parameters))
         (language (let ((first (first (uiop:split-string words :separator '(#\Space #\Tab)))))
                     (and (plusp (length first)) first)))
         (rest (if language (string-trim " 	" (subseq words (length language))) "")))
    (multiple-value-bind (start end) (cl-ppcre:scan "^(?:[-+][A-Za-z](?:[ \\t]+(?:\"[^\"]*\"|[^-+ \\t:]\\S*))?[ \\t]*)+" rest)
      (let ((switches (and start (string-trim " 	" (subseq rest start end))))
            (params (string-trim " 	" (if start (subseq rest end) rest))))
        (values language (and (plusp (length switches)) switches) (and (plusp (length params)) params))))))

(defun org-drawer-parser (position start limit affiliated)
  "The drawer :NAME: at START, ended by :END:; a paragraph when it is not ended."
  (let ((end-line (org-search-line *org-drawer-end-re* (org-next-line start) limit)))
    (if (null end-line)
        (org-paragraph-parser position limit start affiliated)
        (let* ((name (org-scan (n) *org-drawer-re* start n))
               (contents-begin (org-next-line start))
               (node (make-org-node 'drawer :properties (list :drawer-name name))))
          (when (< contents-begin end-line)
            (setf (org-node-contents-begin node) contents-begin (org-node-contents-end node) end-line))
          (org-finish (org-element-with-affiliated node position start affiliated) (org-next-line end-line) limit)))))

(defun org-dynamic-block-parser (position start limit affiliated)
  "The dynamic block #+BEGIN: NAME at START, ended by #+END:; a paragraph when it is not ended."
  (let ((end-line (org-search-line *org-dynamic-block-end-re* (org-next-line start) limit)))
    (if (null end-line)
        (org-paragraph-parser position limit start affiliated)
        (let* ((contents-begin (org-next-line start))
               (node (make-org-node 'dynamic-block)))
          (org-scan (name arguments) *org-dynamic-block-re* start
            (setf (org-node-properties node) (append (list :block-name name) (and arguments (list :arguments arguments)))))
          (when (< contents-begin end-line)
            (setf (org-node-contents-begin node) contents-begin (org-node-contents-end node) end-line))
          (org-finish (org-element-with-affiliated node position start affiliated) (org-next-line end-line) limit)))))

(defun org-latex-environment-parser (position start limit affiliated)
  "The LaTeX environment \\begin{NAME} at START, ended by \\end{NAME}; a paragraph when it is not ended."
  (let* ((name (org-scan (n) *org-latex-begin-re* start n))
         (end-line (org-closing-line :latex name start limit)))
    (if (null end-line)
        (org-paragraph-parser position limit start affiliated)
        (let ((after (org-next-line end-line)))
          (org-finish (org-element-with-affiliated
                       (make-org-node 'latex-environment
                                      :properties (list :value (subseq *org-text* start after)))
                       position start affiliated)
                      after limit)))))

(defun org-footnote-definition-parser (position start limit affiliated)
  "The footnote definition [fn:LABEL] at START: to the next definition or
headline, or two blank lines."
  (let* ((label (org-scan (l) *org-footnote-definition-re* start l))
         (after-label (+ start (length label) 5))
         (end-of-definition
           (loop for p = (org-next-line start) then (org-next-line p)
                 when (>= p limit) return limit
                 when (or (org-looking-at *org-footnote-definition-re* p)
                          (org-looking-at *org-headline-re* p)
                          (and (org-blank-line-p p) (< (org-next-line p) limit) (org-blank-line-p (org-next-line p))))
                   return p))
         (contents-begin (let ((c (org-skip-blanks-forward after-label end-of-definition)))
                           (and (< c end-of-definition) c)))
         (contents-end (and contents-begin (org-end-of-content end-of-definition contents-begin)))
         (node (make-org-node 'footnote-definition :contents-begin contents-begin :contents-end contents-end
                                                   :properties (list :label label))))
    (org-finish (org-element-with-affiliated node position start affiliated)
                (or contents-end (org-next-line start)) limit)))

;;; Paragraphs

(defun org-paragraph-parser (position limit start &optional affiliated)
  "The paragraph at START: to the next line that begins another element (or
a blank line), not counting openings of blocks, drawers and environments that
are never closed."
  (let ((p (org-next-line start)))
    (loop
      (when (>= p limit) (setf p limit) (return))
      (when (org-looking-at (org-paragraph-separate) p)
        (cond ((org-looking-at *org-drawer-re* p)
               (when (org-search-line *org-drawer-end-re* (org-next-line p) limit) (return)))
              ((org-looking-at "(?i)[ \\t]*#\\+BEGIN_(\\S+)" p)
               (let ((name (org-scan (n) "(?i)[ \\t]*#\\+BEGIN_(\\S+)" p n)))
                 (when (org-closing-line :block (concatenate 'string "_" name) (org-next-line p) limit)
                   (return))))
              ((org-looking-at *org-latex-begin-re* p)
               (let ((name (org-scan (n) *org-latex-begin-re* p n)))
                 (when (org-closing-line :latex name p limit)
                   (return))))
              ((org-looking-at "[ \\t]*#\\+(\\S+?)\\[.*\\]:" p)
               (let ((key (org-scan (k) "[ \\t]*#\\+(\\S+?)\\[.*\\]:" p k)))
                 (when (member (string-upcase key) *org-dual-keywords* :test #'string=) (return))))
              (t (return))))
      (setf p (org-next-line p)))
    (let* ((contents-end (org-end-of-content p start))
           (node (make-org-node 'paragraph :contents-begin start :contents-end contents-end)))
      (org-finish (org-element-with-affiliated node position start affiliated) contents-end limit))))

;;; Lists

(defun org-list-structure (start limit)
  "The items of the list whose first item is at START, below LIMIT: a list of
(BEGIN INDENT BULLET COUNTER CHECKBOX TAG END), in order, as org-list reads them."
  (let ((p start) (top-indent most-positive-fixnum) (open '()) (closed '()))
    (flet ((finish (end)
             (dolist (item open) (setf (seventh item) end) (push item closed))
             (return-from org-list-structure (sort closed #'< :key #'first))))
      (loop
        (cond
          ((>= p limit) (finish (org-end-of-content limit start)))
          ((and (org-blank-line-p p) (< (org-next-line p) limit) (org-blank-line-p (org-next-line p))
                (< (org-line-end p) (length *org-text*)))
           (finish p))
          ((org-looking-at (org-item-re) p)
           (let ((indent (org-indentation p)))
             (setf top-indent (min top-indent indent))
             (loop while (and open (<= indent (second (first open))))
                   do (let ((item (pop open))) (setf (seventh item) p) (push item closed)))
             (org-scan (bullet counter checkbox tag) *org-full-item-re* p
               (push (list p indent bullet counter checkbox (and (find-if (lambda (c) (find c "-+*")) bullet) tag) nil)
                     open))
             (setf p (org-next-line p))))
          ((org-blank-line-p p) (setf p (org-next-line p)))
          ((org-looking-at *org-headline-re* p) (finish (org-end-of-content p start)))
          (t
           (let ((indent (org-indentation p)))
             (when (<= indent top-indent)
               (finish (org-end-of-content p start)))
             (loop while (and open (<= indent (second (first open))))
                   do (let ((item (pop open)))
                        (setf (seventh item) (org-line-start (org-end-of-content p start)))
                        (push item closed)
                        (unless open (finish (org-end-of-content p start)))))
             ;; blocks and drawers are skipped whole
             (let ((skip (or (let ((name (org-scan (n) "(?i)[ \\t]*#\\+BEGIN(:|_\\S+)" p n)))
                               (and name (org-closing-line :block (if (string= name ":") "" name) (org-next-line p) limit)))
                             (and (org-looking-at *org-drawer-re* p)
                                  (org-search-line *org-drawer-end-re* (org-next-line p) limit)))))
               (setf p (org-next-line (or skip p)))))))))))

(defun org-plain-list-parser (position start limit affiliated)
  "The plain list whose first item is at START."
  (let* ((structure (org-list-structure start limit))
         (first (first structure))
         (indent (second first))
         (siblings (remove indent structure :key #'second :test-not #'=))
         (contents-end (seventh (car (last siblings))))
         (bullet (third first))
         (type (cond ((cl-ppcre:scan "^[ \\t]*[0-9A-Za-z]+[.)]" bullet) 'ordered)
                     ((sixth first) 'descriptive)
                     (t 'unordered)))
         (node (make-org-node 'plain-list :contents-begin start :contents-end contents-end
                                          :properties (list :type type))))
    (let ((items (or *org-list-items* (setf *org-list-items* (make-hash-table)))))
      (dolist (item structure) (setf (gethash (first item) items) item)))
    (org-finish (org-element-with-affiliated node position start affiliated) contents-end limit)))

(defun org-item-parser (position limit)
  "The item at POSITION, in the list structure of its plain list."
  (let* ((item (gethash position *org-list-items*))
         (end (or (seventh item) limit)))
    (destructuring-bind (begin indent bullet counter checkbox tag item-end) item
      (declare (ignore indent item-end))
      (let* ((contents-begin
               (org-scan () *org-full-item-re* begin
                 (let ((after (cond (tag (multiple-value-bind (ok e) (org-looking-at "[ \\t]*\\S.*?[ \\t]+::(?:[ \\t]+|$)" begin)
                                           (declare (ignore ok)) e))
                                    (t match-end))))
                   (org-skip-blanks-forward after end))))
             (contents-begin (and (< contents-begin end)
                                  (if (= (org-line-start contents-begin) (org-line-start begin))
                                      contents-begin
                                      (org-line-start contents-begin))))
             (contents-end (and contents-begin (org-end-of-content end contents-begin)))
             (node (make-org-node 'item :begin begin :end end :post-affiliated begin
                                        :contents-begin contents-begin :contents-end contents-end
                                        :properties (append (list :bullet bullet)
                                                            (and checkbox
                                                                 (list :checkbox (case (char checkbox 1) (#\X 'on) (#\- 'trans) (t 'off))))
                                                            (and counter (list :counter (or (ignore-errors (parse-integer counter)) counter)))
                                                            (and tag (list :tag tag))))))
        (setf (org-node-post-blank node) (org-count-lines (or contents-end begin) end))
        node))))

;;; Tables

(defun org-table-parser (position start limit affiliated)
  "The table at START: an Org table (| lines), with its #+TBLFM: lines, or a table.el one (+- lines)."
  (let* ((table-el (org-looking-at "[ \\t]*\\+" start))
         (p start))
    (loop while (and (< p limit) (org-looking-at (if table-el "[ \\t]*[|+]" "[ \\t]*\\|") p))
          do (setf p (org-next-line p)))
    (let ((contents-end p) (formulas '()))
      (loop while (and (< p limit) (org-looking-at "(?i)[ \\t]*#\\+TBLFM:" p))
            do (push (string-trim " 	" (subseq *org-text* (+ (search ":" *org-text* :start2 p) 1) (org-line-end p))) formulas)
               (setf p (org-next-line p)))
      (let ((node (make-org-node 'table :contents-begin (if table-el nil start) :contents-end (if table-el nil contents-end)
                                        :properties (append (list :type (if table-el 'table.el 'org))
                                                            (and formulas (list :tblfm (reverse formulas)))))))
        (org-finish (org-element-with-affiliated node position start affiliated) p limit)))))

(defun org-table-row-parser (position limit)
  "The table row at POSITION: a rule (|-) or a row of cells."
  (declare (ignore limit))
  (let* ((rule (org-looking-at "[ \\t]*\\|-" position))
         (contents-begin (and (not rule) (multiple-value-bind (ok e) (org-looking-at "[ \\t]*\\|" position) (declare (ignore ok)) e)))
         (line-end (org-line-end position))
         (node (make-org-node 'table-row :begin position :end (org-next-line position) :post-affiliated position
                                         :contents-begin contents-begin
                                         :contents-end (and contents-begin (let ((e (org-skip-blanks-backward line-end contents-begin))) e))
                                         :properties (list :type (if rule 'rule 'standard)))))
    node))

;;; Objects (inline: markup, links, timestamps...)

(defun org-object-types (type)
  "The objects an element or object of TYPE may hold."
  (case type
    ((bold italic underline strike-through) (remove-if (lambda (o) (member o '(line-break))) *org-standard-objects*))
    ((radio-target link) '(bold code entity export-snippet inline-babel-call inline-src-block italic latex-fragment
                           macro statistics-cookie strike-through subscript superscript underline verbatim))
    (table-row '(table-cell))
    (citation '(citation-reference))
    (table-cell '(bold code entity export-snippet footnote-reference italic latex-fragment link macro radio-target
                  strike-through subscript superscript target timestamp underline verbatim citation))
    (headline (remove 'line-break *org-standard-objects*))
    ((footnote-reference) *org-standard-objects*)
    ((subscript superscript) (remove 'line-break *org-standard-objects*))
    (t *org-standard-objects*)))

(defun org-search-close (needle start)
  "Where NEEDLE, what closes an object, first is from START below the end of
the stretch whose objects are parsed, or NIL: once it is not found from a
place on, it is not looked for again after that place (*ORG-CLOSE-MISSES*)."
  (let ((miss (cdr (assoc needle *org-close-misses* :test #'string=))))
    (unless (and miss (>= start miss))
      (or (search needle *org-text* :start2 start :end2 *org-object-limit*)
          (progn (push (cons needle start) *org-close-misses*) nil)))))

(defun org-parse-objects (begin end types)
  "The objects from BEGIN below END that TYPES allow, in order, each with its
contents parsed; none deeper than *ORG-DEEPEST*."
  (let ((objects '()) (p begin) (*org-object-begin* begin) (*org-object-limit* end)
        (*org-emphasis-misses* '()) (*org-close-misses* '()) (*org-depth* (1+ *org-depth*)))
    (when (> *org-depth* *org-deepest*)
      (return-from org-parse-objects '()))
    (loop
      (multiple-value-bind (object) (org-next-object p end types)
        (unless object (return))
        (when (and (org-node-contents-begin object) (org-node-contents-end object)
                   (not (member (org-node-type object) '(table-cell citation-reference))))
          (org-adopt object (org-parse-objects (org-node-contents-begin object) (org-node-contents-end object)
                                               (org-object-types (org-node-type object)))))
        (when (eq (org-node-type object) 'table-cell)
          (when (and (org-node-contents-begin object) (< (org-node-contents-begin object) (org-node-contents-end object)))
            (org-adopt object (org-parse-objects (org-node-contents-begin object) (org-node-contents-end object)
                                                 (org-object-types 'table-cell)))))
        (push object objects)
        (setf p (org-node-end object))))
    (nreverse objects)))

(defun org-object-candidate (start end)
  "The first place from START below END where an object may begin: one of
_^*/~=+@{$<\\=\\[, call_ or src_, or a link's type and its colon at a word's
start; NIL when there is none. Many places are false: each object's parser
tells."
  (let ((text *org-text*))
    (declare (type simple-string text) (type fixnum start end))
    (loop for i of-type fixnum from start below end
          for c = (schar text i)
          when (or (find c "_^*/~=+@{$<\\[")
                   (and (member c '(#\c #\C #\s #\S))
                        (org-looking-at "(?i)call_|src_" i (min end (+ i 5))))
                   (and (alpha-char-p c)
                        (or (= i 0) (not (alphanumericp (schar text (1- i)))))
                        ;; held to the longest type's length, so that no search runs on through a long line
                        (org-looking-at "(?:https?|ftp|mailto|file|id|doi|news|shell|elisp|help|info|irc|attachment|docview):" i (min end (+ i 11)))))
            return i)))

(defun org-next-object (start end types)
  "The first object from START below END that TYPES allow, or NIL."
  (cond
    ((member 'table-cell types) (org-table-cell-at start end))
    ((member 'citation-reference types) (org-citation-reference-at start end))
    (t
      (let ((*org-object-limit* end) (p start))
        (loop
          (let ((candidate (org-object-candidate p end)))
            (unless candidate (return nil))
            (let ((object (org-object-at candidate types)))
              (when (and object (<= (org-node-end object) end))
                (return object))
              (setf p (1+ candidate)))))))))

(defun org-object-at (p types)
  "The object of TYPES beginning at P, or NIL."
  (let ((c (org-char p)))
    (flet ((try (type parser) (and (member type types) (funcall parser p))))
      (cond
        ((org-looking-at "(?i)call_" p (min (length *org-text*) (+ p 5))) (try 'inline-babel-call #'org-inline-babel-call-parser))
        ((org-looking-at "(?i)src_" p (min (length *org-text*) (+ p 4))) (try 'inline-src-block #'org-inline-src-block-parser))
        (t
         (case c
           (#\^ (try 'superscript #'org-superscript-parser))
           (#\_ (or (try 'subscript #'org-subscript-parser) (try 'underline (lambda (p) (org-emphasis-parser p #\_ 'underline)))))
           (#\* (try 'bold (lambda (p) (org-emphasis-parser p #\* 'bold))))
           (#\/ (try 'italic (lambda (p) (org-emphasis-parser p #\/ 'italic))))
           (#\~ (try 'code (lambda (p) (org-emphasis-parser p #\~ 'code))))
           (#\= (try 'verbatim (lambda (p) (org-emphasis-parser p #\= 'verbatim))))
           (#\+ (try 'strike-through (lambda (p) (org-emphasis-parser p #\+ 'strike-through))))
           (#\@ (try 'export-snippet #'org-export-snippet-parser))
           (#\{ (try 'macro #'org-macro-parser))
           (#\$ (try 'latex-fragment #'org-latex-fragment-parser))
           (#\< (if (eql (org-char (1+ p)) #\<)
                    (or (try 'radio-target #'org-radio-target-parser) (try 'target #'org-target-parser))
                    (or (try 'timestamp #'org-timestamp-parser) (try 'link #'org-link-parser))))
           (#\\ (if (eql (org-char (1+ p)) #\\)
                    (try 'line-break #'org-line-break-parser)
                    (or (try 'entity #'org-entity-parser) (try 'latex-fragment #'org-latex-fragment-parser))))
           (#\[ (case (org-char (1+ p))
                  (#\[ (try 'link #'org-link-parser))
                  (#\f (try 'footnote-reference #'org-footnote-reference-parser))
                  (#\c (try 'citation #'org-citation-parser))
                  ((#\% #\/) (try 'statistics-cookie #'org-statistics-cookie-parser))
                  (t (or (try 'timestamp #'org-timestamp-parser) (try 'statistics-cookie #'org-statistics-cookie-parser)))))
           (t (try 'link #'org-link-parser))))))))

(defun org-object-finish (node end)
  "NODE, an object ending at END, with the spaces and tabs after it counted as its POST-BLANK."
  (let ((after end))
    (loop while (and (< after *org-object-limit*) (member (org-char after) '(#\Space #\Tab))) do (incf after))
    (setf (org-node-end node) after
          (org-node-post-blank node) (- after end))
    node))

(defun org-space-p (c)
  "True when C is white space: a space, a tab, a line end, a carriage return or a page break."
  (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun org-emphasis-parser (p mark type)
  "The emphasis of TYPE marked by MARK at P (*bold*, /italic/...): MARK after
the line's start, a space or one of -('\"{, its contents beginning and ending
with what is no space, the closing MARK before a space, punctuation or the
line's end."
  (let ((before (and (> p *org-object-begin*) (let ((c (org-char (1- p)))) (and (not (eql c #\Newline)) c))))
        (miss (getf *org-emphasis-misses* mark)))
    (when (and (or (null before) (org-space-p before) (find before "-('\"{"))
               (org-char (1+ p)) (not (org-space-p (org-char (1+ p))))
               (not (and miss (>= (+ p 2) miss))))
      (or (loop for q from (+ p 2) below *org-object-limit*
                for c = (org-char q)
                when (and (eql c mark) (not (org-space-p (org-char (1- q))))
                          (let ((after (org-char (1+ q))))
                            (or (null after) (>= (1+ q) *org-object-limit*) (org-space-p after) (find after "-.,;:!?'\")}\\["))))
                  return (let ((node (make-org-node type :begin p)))
                           (if (member type '(code verbatim))
                               (setf (org-node-properties node) (list :value (subseq *org-text* (1+ p) q)))
                               (setf (org-node-contents-begin node) (1+ p) (org-node-contents-end node) q))
                           (org-object-finish node (1+ q))))
          ;; no closing mark from here on: none for a later opening one either
          (progn (setf (getf *org-emphasis-misses* mark) (+ p 2)) nil)))))

(defmacro org-entity-table (&optional latex)
  "Org's entities as data/org-entities.txt lists them, read when this file is
compiled and kept in the program: a table from each name to the character it
stands for, or with LATEX true to its LaTeX, as (LATEX . MATH), MATH true for
LaTeX that is written in math mode."
  (let ((entries '()))
    (with-input-from-string (in (data-file-text "org-entities.txt"))
      (loop for line = (read-line in nil)
            while line
            unless (or (zerop (length line)) (char= (char line 0) #\;))
              do (let ((fields (uiop:split-string line :separator '(#\Tab))))
                   (push (cons (first fields) (if latex
                                                  (cons (second fields) (string= (third fields) "t"))
                                                  (car (last fields))))
                         entries))))
    `(let ((table (make-hash-table :test 'equal)))
       (loop for (name . character) in ',(nreverse entries)
             do (setf (gethash name table) character))
       table)))

(defparameter *org-entities* (org-entity-table)
  "Org's entities (\\alpha...), each name with the character it stands for, as Org 9.7 has them.")

(defparameter *org-entities-latex* (org-entity-table t)
  "Org's entities (\\alpha...), each name with its LaTeX and whether that is
math, (LATEX . MATH), as Org 9.7 has them.")

(defun org-entity-parser (p)
  "The entity at P: \\NAME, a name Org knows, ended by {} or what is not a letter."
  (org-scan (spaces name brackets) "\\\\(?:(_ +)|(there4|sup[123]|frac[13][24]|[a-zA-Z]+)($|\\{\\}|[^a-zA-Z]))" p
    (cond (spaces nil)
          ((gethash name *org-entities*)
           (let ((end (+ p 1 (length name) (if (equal brackets "{}") 2 0))))
             (org-object-finish (make-org-node 'entity :begin p :properties (list :name name :use-brackets-p (equal brackets "{}")))
                                end))))))

(defun org-latex-fragment-parser (p)
  "The LaTeX fragment at P: \\=\\(...\\=\\), \\=\\[...\\=\\], $...$, $$...$$ or a \\=\\command."
  (let ((end
          (case (org-char p)
            (#\\ (case (org-char (1+ p))
                   (#\( (let ((close (org-search-close "\\)" (+ p 2)))) (and close (+ close 2))))
                   (#\[ (let ((close (org-search-close "\\]" (+ p 2)))) (and close (+ close 2))))
                   (t (org-scan () "\\\\[a-zA-Z]+\\*?(?:\\[[^][\\n{}]*\\]|\\{[^{}\\n]*\\})*" p match-end))))
            (#\$ (cond ((eql (org-char (1+ p)) #\$)
                        (let ((close (org-search-close "$$" (+ p 2)))) (and close (+ close 2))))
                       ((eql (org-char (1- p)) #\$) nil)
                       (t (org-scan () "\\$(?:[^ \\t\\n,.;$]\\$|[^ \\t\\n,.;$][^$]*?[^ \\t\\n,.$]\\$)(?=[-!\"#$%&'()*+,./:;<=>?@\\[\\\\\\]^_`{|}~ \\t\\n]|$)" p match-end)))))))
    (when end
      (org-object-finish (make-org-node 'latex-fragment :begin p :properties (list :value (subseq *org-text* p end))) end))))

(defun org-line-break-parser (p)
  "The line break at P: \\\\ at the end of a line."
  (when (and (org-looking-at "\\\\\\\\[ \\t]*$" p) (not (eql (org-char (1- p)) #\\)))
    (let ((node (make-org-node 'line-break :begin p)))
      (setf (org-node-end node) (min *org-object-limit* (org-next-line p)) (org-node-post-blank node) 0)
      node)))

(defparameter *org-export-snippet-re* "@@[-A-Za-z0-9]+:.*?@@"
  "A regular expression matching an export snippet, @@BACKEND:VALUE@@, its
back end named as ORG-EXPORT-SNIPPET-PARSER reads one.")

(defun org-export-snippet-parser (p)
  "The export snippet at P: @@BACKEND:VALUE@@."
  (org-scan (back-end) "@@([-A-Za-z0-9]+):" p
    (let ((close (org-search-close "@@" match-end)))
      (when close
        (org-object-finish (make-org-node 'export-snippet :begin p
                                                          :properties (list :back-end back-end :value (subseq *org-text* match-end close)))
                           (+ close 2))))))

(defparameter *org-hiding-macros* '(("redact" "r") ("protect" "p"))
  "The macros that hide their text in the enhanced export, each its name and
its short name: {{{redact(John Otago)}}} or {{{R(John Otago)}}}, {{{protect(...)}}}
or {{{P(...)}}}, in any case.")

(defun org-hiding-kind (name)
  "What the macro NAME hides its text as, \"redact\" or \"protect\" (its
short name R or P too, any case, *ORG-HIDING-MACROS*); NIL for another macro."
  (first (find-if (lambda (names) (member name names :test #'string-equal)) *org-hiding-macros*)))

(defparameter *org-hiding-call-re*
  (format nil "(?i)\\{\\{\\{(~{~a~^|~})\\(((?:.|\\n)*?)\\)\\}\\}\\}" (reduce #'append *org-hiding-macros*))
  "A call of a macro that hides its text (*ORG-HIDING-MACROS*), over lines as
ORG-MACRO-PARSER reads one: the name as written, then the text, as its two
groups.")

(defparameter *org-macro-call-re* "\\{\\{\\{([a-zA-Z][-\\w]*)\\(((?:.|\\n)*?)\\)\\}\\}\\}"
  "A call of a macro with arguments, as ORG-MACRO-PARSER reads one: its name
and what is between its parentheses as its two groups.")

(defun org-macro-parser (p)
  "The macro call at P: {{{NAME}}} or {{{NAME(ARGUMENTS)}}}."
  (org-scan (key args) ("\\{\\{\\{([a-zA-Z][-\\w]*)(?:\\(((?:.|\\n)*?)\\))?\\}\\}\\}" :end *org-object-limit*) p
    (org-object-finish (make-org-node 'macro :begin p
                                             :properties (list :key (string-downcase key)
                                                               :args (and args (mapcar (lambda (a) (string-trim " 	" a))
                                                                                       (cl-ppcre:split "(?<!\\\\)," args :limit most-positive-fixnum)))))
                       match-end)))

(defun org-target-parser (p)
  "The target at P: <<TEXT>>."
  (org-scan (value) "<<([^<>\\n \\t](?:[^<>\\n]*[^<>\\n \\t])?)>>" p
    (org-object-finish (make-org-node 'target :begin p :properties (list :value value)) match-end)))

(defun org-radio-target-parser (p)
  "The radio target at P: <<<TEXT>>>."
  (org-scan (value) "<<<([^<>\\n \\t](?:[^<>\\n]*[^<>\\n \\t])?)>>>" p
    (let ((node (make-org-node 'radio-target :begin p :contents-begin (+ p 3) :contents-end (+ p 3 (length value))
                                             :properties (list :value value))))
      (org-object-finish node match-end))))

(defparameter *org-statistics-cookie-re* "\\[[0-9]*(?:%|/[0-9]*)\\]"
  "A statistics cookie: [N/M], [N%], [/] or [%].")

(defun org-statistics-cookie-parser (p)
  "The statistics cookie at P: [N/M], [N%], [/] or [%]."
  (org-scan () *org-statistics-cookie-re* p
    (org-object-finish (make-org-node 'statistics-cookie :begin p :properties (list :value (subseq *org-text* p match-end)))
                       match-end)))

(defun org-script-parser (p type)
  "The subscript or superscript (TYPE) at P, after what is no space: _x, ^{...}."
  (when (and (> p *org-object-begin*) (org-char (1- p)) (not (org-space-p (org-char (1- p)))))
    (org-scan (braced plain) "[_^](?:(\\{(?:[^{}]|\\{[^{}]*\\})*\\})|(\\*|[+-]?[\\p{Alnum}.,\\\\]*\\p{Alnum}))" p
      (let ((node (make-org-node type :begin p :properties (list :use-brackets-p (and braced t)))))
        (if braced
            (setf (org-node-contents-begin node) (+ p 2) (org-node-contents-end node) (1- match-end))
            (setf (org-node-contents-begin node) (1+ p) (org-node-contents-end node) (+ p 1 (length plain))))
        (org-object-finish node match-end)))))

(defun org-subscript-parser (p) "The subscript at P." (org-script-parser p 'subscript))
(defun org-superscript-parser (p) "The superscript at P." (org-script-parser p 'superscript))

(defun org-balanced-end (p open close)
  "Where the bracketed text beginning at P with OPEN ends, just past its CLOSE, the brackets in it balanced; NIL when unbalanced on its line."
  (let ((depth 0))
    (loop for q from p below *org-object-limit*
          for c = (org-char q)
          do (cond ((eql c #\Newline) (return nil))
                   ((eql c open) (incf depth))
                   ((eql c close) (decf depth) (when (zerop depth) (return (1+ q))))))))

(defun org-inline-src-block-parser (p)
  "The inline source block at P: src_LANG[HEADERS]{BODY}."
  (org-scan (language) "(?i)src_([^ \\t\\n\\[\\]{}]+)" p
    (let* ((after match-end)
           (parameters nil))
      (when (eql (org-char after) #\[)
        (let ((close (org-balanced-end after #\[ #\])))
          (unless close (return-from org-inline-src-block-parser nil))
          (setf parameters (subseq *org-text* (1+ after) (1- close)) after close)))
      (when (eql (org-char after) #\{)
        (let ((close (org-balanced-end after #\{ #\})))
          (when close
            (org-object-finish (make-org-node 'inline-src-block :begin p
                                                                :properties (append (list :language language)
                                                                                    (and parameters (list :parameters parameters))
                                                                                    (list :value (subseq *org-text* (1+ after) (1- close)))))
                               close)))))))

(defun org-inline-babel-call-parser (p)
  "The inline babel call at P: call_NAME[INSIDE](ARGUMENTS)[END]."
  (org-scan (name) "(?i)call_([^()\\[\\] \\t\\n]+)" p
    (let ((after match-end) inside arguments end-header)
      (when (eql (org-char after) #\[)
        (let ((close (org-balanced-end after #\[ #\]))) (unless close (return-from org-inline-babel-call-parser nil))
          (setf inside (subseq *org-text* (1+ after) (1- close)) after close)))
      (when (eql (org-char after) #\()
        (let ((close (org-balanced-end after #\( #\))))
          (when close
            (setf arguments (subseq *org-text* (1+ after) (1- close)) after close)
            (when (eql (org-char after) #\[)
              (let ((close (org-balanced-end after #\[ #\])))
                (when close (setf end-header (subseq *org-text* (1+ after) (1- close)) after close))))
            (org-object-finish (make-org-node 'inline-babel-call :begin p
                                                                 :properties (append (list :call name)
                                                                                     (and inside (plusp (length inside)) (list :inside-header inside))
                                                                                     (and (plusp (length arguments)) (list :arguments arguments))
                                                                                     (and end-header (plusp (length end-header)) (list :end-header end-header))))
                               after)))))))

(defun org-footnote-reference-parser (p)
  "The footnote reference at P: [fn:LABEL], [fn:: inline text] or [fn:LABEL:inline text]."
  (org-scan (label) "\\[fn:([-_\\w]*)" p
    (let ((after match-end))
      (case (org-char after)
        (#\] (when (plusp (length label))
               (org-object-finish (make-org-node 'footnote-reference :begin p :properties (list :label label :type 'standard))
                                  (1+ after))))
        (#\: (let ((close (org-balanced-end p #\[ #\])))
               (when close
                 (org-object-finish (make-org-node 'footnote-reference :begin p :contents-begin (1+ after) :contents-end (1- close)
                                                                       :properties (append (and (plusp (length label)) (list :label label))
                                                                                           (list :type 'inline)))
                                    close))))))))

(defun org-citation-parser (p)
  "The citation at P: [cite/STYLE:prefix;@key suffix;...]."
  (org-scan (style) "\\[cite(?:/([/_a-z0-9-]+))?:" p
    (let ((close (org-balanced-end p #\[ #\])))
      (when (and close (search "@" *org-text* :start2 match-end :end2 close))
        (let ((node (make-org-node 'citation :begin p :contents-begin match-end :contents-end (1- close)
                                             :properties (and style (list :style style)))))
          (org-object-finish node close))))))

(defparameter *org-citation-key-re* "@([-\\w.:?!`'/*@+|(){}<>&_^$#%~]+)"
  "A citation's key, after @.")

(defun org-citation-reference-at (start end)
  "The citation reference from START below END: to just past its ;, or END."
  (when (< start end)
    (multiple-value-bind (key-start key-end rs re) (cl-ppcre:scan *org-citation-key-re* *org-text* :start start :end end)
      (declare (ignore key-start))
      (when rs
        (let* ((separator (position #\; *org-text* :start key-end :end end))
               (reference-end (if separator (1+ separator) end)))
          (make-org-node 'citation-reference :begin start :end reference-end
                                             :properties (list :key (subseq *org-text* (aref rs 0) (aref re 0)))))))))

;;; Timestamps

(defparameter *org-timestamp-re*
  (let ((stamp (org-timestamp-body)))
    (format nil "(?:<~a>(?:--<~a>)?|\\[~a\\](?:--\\[~a\\])?|<~a>)" stamp stamp stamp stamp (org-rule-timestamp-body)))
  "A timestamp, active or inactive, a range, or a diary sexp, a time or a
time range after it too (<%%(diary-float t 1 1) 10:00>).")

(defparameter *org-one-timestamp-scanner*
  (let ((stamp (org-timestamp-body))) (cl-ppcre:create-scanner (format nil "<~a>|\\[~a\\]" stamp stamp)))
  "One timestamp, active or inactive: each date of a range alone.")

(defparameter *org-active-timestamp-scanner*
  (cl-ppcre:create-scanner (format nil "<~a>" (org-timestamp-body)))
  "One active timestamp: each date of a range alone.")

(defparameter *org-rule-timestamp-scanner*
  (cl-ppcre:create-scanner (format nil "<~a>" (org-rule-timestamp-body)))
  "One timestamp by rule, <%%(diary-float t 1 1) 10:00> (ORG-READ-RULE).")

(defparameter *org-active-range-scanner*
  (let ((stamp (org-timestamp-body))) (cl-ppcre:create-scanner (format nil "<~a>--<~a>" stamp stamp)))
  "A range of two active timestamps, <a>--<b>.")

(defparameter *org-timestamp-or-range-scanner*
  (let ((one (format nil "(?:<~a>|\\[~a\\])" (org-timestamp-body) (org-timestamp-body))))
    (cl-ppcre:create-scanner (format nil "~a(?:--~a)?" one one)))
  "One timestamp, active or inactive, or a range of two, <a>--<b>; no diary sexp.")

(defun org-timestamp-parser (p)
  "The timestamp at P: <2026-10-01 Thu 09:00>, [2026-10-01 Thu], a range or a diary sexp."
  (org-scan () *org-timestamp-re* p
    (let* ((raw (subseq *org-text* p match-end))
           (activep (char= (char raw 0) #\<))
           (range (or (search ">--<" raw) (search "]--[" raw)))
           (time-range (cl-ppcre:scan "\\d{1,2}:\\d{2}-\\d{1,2}:\\d{2}" raw))
           (type (cond ((uiop:string-prefix-p "<%%" raw) 'diary)
                       ((or range time-range) (if activep 'active-range 'inactive-range))
                       (activep 'active)
                       (t 'inactive))))
      (org-object-finish (make-org-node 'timestamp :begin p :properties (list :type type :raw-value raw)) match-end))))

;;; Links

(defparameter *org-link-types*
  '("shell" "news" "mailto" "https" "http" "ftp" "help" "file" "elisp" "doi" "id" "info" "irc" "attachment" "docview")
  "The link types Org knows as standard.")

(defun org-link-parts (raw)
  "RAW, a link's target as written, as two values: its type and path."
  (let ((colon (position #\: raw)))
    (cond ((and (plusp (length raw)) (char= (char raw 0) #\#)) (values "custom-id" (subseq raw 1)))
          ((and (plusp (length raw)) (char= (char raw 0) #\() (char= (char raw (1- (length raw))) #\)))
           (values "coderef" (subseq raw 1 (1- (length raw)))))
          ((and colon (member (subseq raw 0 colon) *org-link-types* :test #'string=))
           (let ((type (subseq raw 0 colon)) (path (subseq raw (1+ colon))))
             (if (string= type "file")
                 (values type (subseq path 0 (or (search "::" path) (length path))))
                 (values type path))))
          ((or (uiop:string-prefix-p "/" raw) (uiop:string-prefix-p "./" raw) (uiop:string-prefix-p "~/" raw))
           (values "file" raw))
          (t (values "fuzzy" raw)))))

(defun org-link-unescape (link)
  "LINK, the target of a bracket link, as it is meant: a run of backslashes
before [, ] or the end halved."
  (cl-ppcre:regex-replace-all "(\\\\+)(?=[][]|$)" link
                              (lambda (match &rest groups)
                                (declare (ignore groups))
                                (make-string (floor (length match) 2) :initial-element #\\))
                              :simple-calls t))

(defun org-link-escape (link)
  "LINK as the target of a bracket link holds it: a backslash before each [ and
], the backslashes before them and at the end doubled."
  (cl-ppcre:regex-replace-all "(\\\\*)([][]|$)" link
                              (lambda (match backslashes bracket)
                                (declare (ignore match))
                                (concatenate 'string backslashes backslashes
                                             (if (plusp (length bracket)) (concatenate 'string "\\" bracket) "")))
                              :simple-calls t))

(defparameter *org-bracket-link-scanner*
  (cl-ppcre:create-scanner "\\[\\[((?:[^]\\\\]|\\\\.)*)\\](?:\\[(.*?)\\])?\\]")
  "A bracket link, [[TARGET][LABEL]] or [[TARGET]], its target as written and
its label the groups.")

(defun org-links-as-labels (text)
  "TEXT with each bracket link written as it is shown: [[target][label]] as
label, [[target]] as its target with the backslashes of its escapes taken off."
  (if (search "[[" text)
      (cl-ppcre:regex-replace-all *org-bracket-link-scanner* text
                                  (lambda (match target label)
                                    (declare (ignore match))
                                    (or label (org-link-unescape target)))
                                  :simple-calls t)
      text))

(defun org-plain-text (text)
  "TEXT as words alone: links as they are shown (ORG-LINKS-AS-LABELS) and the
marks of emphasis (*bold*, /italic/, _under_, =code=, ~verbatim~, +struck+) off."
  (cl-ppcre:regex-replace-all "(?<![\\w*/_=~+])([*/_=~+])(\\S|\\S.*?\\S)\\1(?![\\w*/_=~+])"
                              (org-links-as-labels text) "\\2"))

(defun org-link-parser (p)
  "The link at P: [[TARGET][DESCRIPTION]], [[TARGET]], <TYPE:PATH> or a plain TYPE:PATH."
  (case (org-char p)
    (#\[
     (org-scan (target) ("\\[\\[((?:[^][\\\\]|\\\\.)+)\\]" :end *org-object-limit*) p
       (let* ((after match-end)
              (description-end (and (eql (org-char after) #\[)
                                    (org-search-close "]]" after)))
              (end (cond (description-end (+ description-end 2))
                         ((eql (org-char after) #\]) (1+ after)))))
         (when end
           (let* ((raw (org-link-unescape (cl-ppcre:regex-replace-all "\\s+" (string-trim "
" target) " "))))
             (multiple-value-bind (type path) (org-link-parts raw)
               (let ((node (make-org-node 'link :begin p :properties (list :type type :path path :format 'bracket :raw-link raw))))
                 (when description-end
                   (setf (org-node-contents-begin node) (1+ after) (org-node-contents-end node) description-end))
                 (org-object-finish node end))))))))
    (#\<
     (org-scan (type path) "<([a-z]+):([^]<>\\n]*?)>" p
       (when (member type *org-link-types* :test #'string=)
         (org-object-finish (make-org-node 'link :begin p :properties (list :type type :path path :format 'angle
                                                                            :raw-link (format nil "~a:~a" type path)))
                            match-end))))
    (t
     (when (or (<= p *org-object-begin*) (not (alphanumericp (org-char (1- p)))))
       (org-scan (type path) "([a-z]+):((?:[^][ \\t\\n()<>]|\\((?:[^][ \\t\\n()<>]|\\([^][ \\t\\n()<>]*\\))*\\))+(?:[^\\s!\"#$%&'()*+,\\-./:;<=>?@\\[\\\\\\]^_`{|}~]|/|\\((?:[^][ \\t\\n()<>]|\\([^][ \\t\\n()<>]*\\))*\\)))" p
         (when (member type *org-link-types* :test #'string=)
           (org-object-finish (make-org-node 'link :begin p :properties (list :type type :path path :format 'plain
                                                                              :raw-link (format nil "~a:~a" type path)))
                              match-end)))))))

;;; Table cells

(defun org-table-cell-at (start end)
  "The table cell beginning at START, below END: what is up to the next |."
  (when (< start end)
    (let* ((bar (position #\| *org-text* :start start :end end))
           (cell-end (if bar (1+ bar) end))
           (text-end (or bar end))
           (contents-begin (loop for q from start below text-end
                                 unless (member (org-char q) '(#\Space #\Tab)) return q
                                 finally (return text-end)))
           (contents-end (org-skip-blanks-backward text-end contents-begin)))
      (when (or bar (< contents-begin text-end))
        (let ((node (make-org-node 'table-cell :begin start :end cell-end
                                               :contents-begin contents-begin :contents-end contents-end)))
          node)))))
