;;;; lisp/org/export-settings.lisp --- The :EXPORT: drawer: how a heading, table, list, block or picture is laid out in the PDF and the Word document, in plain words.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A drawer named EXPORT holds settings for the PDF the enhanced export
;;;; makes and for the Word document (M-x org-export-docx), a setting a line,
;;;; written as words rather than LaTeX or Word's XML:
;;;;
;;;;   :EXPORT:
;;;;   widths: 30% * *
;;;;   align: left right centre
;;;;   font: small
;;;;   :END:
;;;;
;;;; Right under a heading (after its :PROPERTIES:) it sets the heading; just
;;;; before a table, a list, a block or a picture it sets that. Tuatara's
;;;; exports never print the drawer; Org's own do, unless the note's
;;;; #+options: say d:(not "EXPORT" "LOGBOOK"), which
;;;; M-x org-insert-export-drawer adds. *ORG-LATEX-SETTINGS* lists every setting,
;;;; and M-x describe-export-settings shows them.

(in-package #:tuatara.defaults)

(defparameter *org-latex-settings*
  '(("numbered" (:heading) "no prints the heading without a number, as :UNNUMBERED: t does; yes, as standard, with one")
    ("annex" (:heading) "yes begins the annexes at this heading: on a page of their own, numbered anew, as the :appendix: tag does")
    ("contents" (:heading) "no leaves the heading out of the table of contents; yes, as standard, in it")
    ("new page" (:heading :paragraph :list :block :table :picture) "before, after or both: a new page begins there (a heading: after all under it); no, as standard")
    ("space before" (:heading :paragraph :list :block :table :picture) "a length (6pt, 1em, 1cm): that much space above it")
    ("space after" (:heading :paragraph :list :block :table :picture) "a length (6pt, 1em, 1cm): that much space under it (a heading: after all under it)")
    ("spacing" (:heading :paragraph :list :block :table) "single, 1.5, double or a number (1.15): the space between the lines, as a word processor counts it")
    ("columns" (:heading :paragraph :list :block :table) "a number: the text set in that many columns (a heading: all under it, the heading across them)")
    ("page" (:heading :paragraph :list :block :table :picture) "landscape: on pages turned sideways, from a page of their own (a heading: it and all under it; side by side, they share the pages); portrait, as standard")
    ("font" (:heading :paragraph :list :block :table) "tiny, scriptsize, footnote, small, normal, large or larger: the size of the type")
    ("indent" (:paragraph :list :block :table :picture) "a length (2em, 1cm): set in from the left by that much")
    ("width" (:picture :table) "a share of the page (60%) or a length (8cm); for a table, full: from margin to margin, its columns sharing the width as their text does")
    ("widths" (:table) "each column's width in turn: a share of the page (30%), a length (5cm, 22em), * for an equal share of what is left, or auto")
    ("align" (:paragraph :table) "a paragraph: left, right, centre or justify; a table: each column in turn, left, right, centre or auto (numbers right, words left)")
    ("position" (:table :picture) "left or right: the table or picture against that margin; centre, as standard, in the middle")
    ("repeat header" (:table) "yes, as standard: the rows above the first rule again at the top of each page; no, only once")
    ("lines" (:table) "the lines printed, any of: top (above the table), header (under the header), rules (where the table's own |---| lines are), bottom, left, right (down its sides), columns (between the columns), rows (between every row); box (top bottom left right), all, or none; top header rules bottom, as standard")
    ("style" (:list) "bullets, none (no bullets), numbers, letters (a), Letters (A), roman (i), Roman (I)")
    ("place" (:picture) "here: a picture with a caption exactly where it is written; anywhere: where it fits best, as LaTeX chooses")
    ("beside" (:picture) "left or right: the picture beside the paragraph after it, on that side, as two columns"))
  "The settings an :EXPORT: drawer may hold: each its name, what it may set
\(:heading, :paragraph, :list, :block, :table, :picture) and what it does.
Any of them may be auto, which leaves it as the export would have it.")

(defun export-side (word)
  "What WORD, the value of an export setting, says of where things go: :LEFT,
:RIGHT, :CENTRE (centre or center) or :JUSTIFY; NIL for none of them."
  (cond ((null word) nil)
        ((string-equal word "left") :left)
        ((string-equal word "right") :right)
        ((or (string-equal word "centre") (string-equal word "center")) :centre)
        ((string-equal word "justify") :justify)))

(defun org-latex-setting-name-p (name)
  "True when NAME, in any case, is a setting an :EXPORT: drawer may hold (*ORG-LATEX-SETTINGS*)."
  (and (assoc name *org-latex-settings* :test #'string-equal) t))

(defparameter *org-latex-font-sizes*
  '(("tiny" . "\\tiny") ("scriptsize" . "\\scriptsize") ("footnote" . "\\footnotesize") ("footnotesize" . "\\footnotesize")
    ("small" . "\\small") ("normal" . "\\normalsize") ("large" . "\\large") ("larger" . "\\Large") ("largest" . "\\LARGE"))
  "The sizes of type the font setting takes, and the LaTeX for each.")

(defparameter *export-setting-words*
  '(("numbered" "yes" "no") ("annex" "yes" "no") ("contents" "yes" "no") ("new page" "before" "after" "both" "no")
    ("page" "landscape" "portrait") ("align" "left" "right" "centre" "center" "justify" "auto")
    ("position" "left" "right" "centre" "center") ("repeat header" "yes" "no")
    ("style" "bullets" "none" "numbers" "letters" "roman") ("place" "here" "anywhere") ("beside" "left" "right"))
  "The words each setting of *ORG-LATEX-SETTINGS* that takes words is given,
in any case; align one for each column of a table.")

(defun export-setting-fault (name value)
  "What is wrong with VALUE for the setting NAME, as words to say after the
export; NIL when the setting takes it. Lengths and a table's widths and
lines are looked at where they are read."
  (let ((words (rest (assoc name *export-setting-words* :test #'string=))))
    (cond (words
           (let ((bad (remove-if (lambda (w) (member w words :test #'string-equal))
                                 (if (string= name "align") (tuatara.text:split-words value) (list value)))))
             (when bad (format nil "~a: ~{~a~^ ~} is none of ~{~a~^, ~}; left out" name bad words))))
          ((string= name "spacing")
           (unless (spacing-number value)
             (format nil "spacing: ~a is none of single, double or a number (1.15); left out" value)))
          ((string= name "font")
           (unless (or (assoc value *org-latex-font-sizes* :test #'string-equal) (char= (char value 0) #\\))
             (format nil "font: ~a is none of ~{~a~^, ~}; left out" value (mapcar #'car *org-latex-font-sizes*))))
          ((string= name "columns")
           (unless (let ((n (ignore-errors (parse-integer value)))) (and n (plusp n)))
             (format nil "columns: ~a is no number of columns; left out" value))))))

(defun org-export-drawer-p (node)
  "True when NODE is an :EXPORT: drawer."
  (and (eq (org-node-type node) 'drawer) (string-equal (org-property node :drawer-name) "EXPORT")))

(defun org-latex-node-kind (node)
  "What NODE is, as the settings name it: :heading, :paragraph, :picture (a
paragraph that is a picture alone), :list, :table or :block; NIL for what
takes no settings."
  (case (org-node-type node)
    (headline :heading)
    (paragraph (let ((children (org-node-children node)))
                 (if (and (= 1 (length children)) (org-latex-image-link-p (first children))) :picture :paragraph)))
    (plain-list :list)
    (table :table)
    ((src-block example-block fixed-width quote-block center-block special-block verse-block) :block)))

(defun org-latex-note-at (node format &rest arguments)
  "Say, after the document is made, FORMAT with ARGUMENTS, of the :EXPORT:
drawer NODE, naming its line."
  (export-note "Line ~d, an :EXPORT: drawer: ~?" (1+ (count #\Newline *org-latex-text* :end (org-node-begin node)))
               format arguments))

(defun org-export-drawer-settings (drawer)
  "The settings the :EXPORT: DRAWER holds, ((NAME . VALUE) ...) in order, names
in lower case; a line that is no NAME: VALUE, or names no setting, said
\(ORG-LATEX-NOTE-AT) and left out. A value of auto is left out too; a value
the setting does not take is said (EXPORT-SETTING-FAULT), and has no effect."
  (let ((begin (org-node-contents-begin drawer)) (end (org-node-contents-end drawer)) (found '()))
    (when (and begin end)
      (dolist (line (uiop:split-string (subseq *org-latex-text* begin end) :separator '(#\Newline)))
        (let* ((line (string-trim " 	" line))
               (colon (position #\: line)))
          (cond ((zerop (length line)))
                ((null colon) (org-latex-note-at drawer "~s is not NAME: VALUE; left out" line))
                (t (let ((name (string-downcase (string-trim " 	" (subseq line 0 colon))))
                         (value (string-trim " 	" (subseq line (1+ colon)))))
                     (cond ((not (assoc name *org-latex-settings* :test #'string=))
                            (org-latex-note-at drawer "~a is no setting; there are ~{~a~^, ~}"
                                               name (mapcar #'first *org-latex-settings*)))
                           ;; nothing given, or auto: the standard
                           ((or (string= value "") (string-equal value "auto")))
                           (t (let ((fault (export-setting-fault name value)))
                                (when fault (org-latex-note-at drawer "~a" fault)))
                              (push (cons name value) found)))))))))
    (nreverse found)))

(defun org-latex-gather-settings (tree)
  "Every node of TREE the :EXPORT: drawers set, in a table by node: its
settings, ((NAME . VALUE) ...). A drawer right under a heading (only its
properties, planning or other such drawers before it) gives the heading its
settings for a heading; one just before an element gives that element the
others. A setting where it cannot apply is said and left out."
  (let ((table (make-hash-table :test 'eq)))
    (labels ((give (node settings drawer)
               (let ((kind (org-latex-node-kind node)))
                 (dolist (setting settings)
                   (if (member kind (second (assoc (car setting) *org-latex-settings* :test #'string=)))
                       (setf (gethash node table) (append (gethash node table) (list setting)))
                       (org-latex-note-at drawer "~a does not apply to ~(~a~); left out" (car setting) (or kind "what follows it"))))))
             (walk (node)
               (let ((children (org-node-children node)))
                 (loop for (child . rest) on children
                       for leading = t then (and leading (member (org-node-type before) '(property-drawer planning drawer)))
                       for before = child
                       do (when (org-export-drawer-p child)
                            (let* ((settings (org-export-drawer-settings child))
                                   (heading (and leading (eq (org-node-type node) 'section)
                                                 (eq (org-node-type (org-node-parent node)) 'headline)
                                                 (org-node-parent node)))
                                   (for-heading (remove-if-not (lambda (s) (member :heading (second (assoc (car s) *org-latex-settings* :test #'string=))))
                                                               settings))
                                   (next (find-if-not #'org-export-drawer-p rest)))
                              (if heading
                                  (progn (give heading for-heading child)
                                         ;; columns and font, both a heading's and an element's: the heading's when first under it
                                         (setf settings (set-difference settings for-heading :test #'equal)))
                                  (when (and for-heading (not next))
                                    (org-latex-note-at child "~{~a~^, ~} set~:[s~;~] a heading: put the drawer right under it"
                                                       (mapcar #'car for-heading) (rest for-heading))))
                              (when settings
                                (if next
                                    (give next settings child)
                                    (org-latex-note-at child "nothing follows it to set; left out")))))
                          (walk child)))))
      (walk tree))
    table))

(defun org-latex-setting (node name)
  "The value of the setting NAME the :EXPORT: drawers give NODE in the enhanced
export, or NIL."
  (let ((table (org-latex-settings *org-latex*)))
    (and table (cdr (assoc name (gethash node table) :test #'string=)))))

(defun export-new-page (node)
  "Whether a new page begins before NODE and after it, as its new page
setting says: two values, each true or NIL."
  (let ((value (org-latex-setting node "new page")))
    (values (and value (or (string-equal value "before") (string-equal value "both")) t)
            (and value (or (string-equal value "after") (string-equal value "both")) t))))

(defun call-with-export-breaks (node out body &key length page space)
  "Write NODE to OUT by calling BODY with OUT, with a new page and space
before and after it as its new page, space before and space after settings
say: LENGTH reads a length setting as the export takes it, NIL for none;
PAGE writes a new page to OUT, and SPACE such a length's space, each called
with OUT (SPACE with the length after it)."
  (multiple-value-bind (page-before page-after) (export-new-page node)
    (let ((space-before (funcall length (org-latex-setting node "space before")))
          (space-after (funcall length (org-latex-setting node "space after"))))
      (when page-before (funcall page out))
      (when space-before (funcall space out space-before))
      (funcall body out)
      (when space-after (funcall space out space-after))
      (when page-after (funcall page out)))))

(defun export-line-spacing (node)
  "The space between the lines NODE's spacing setting asks for, as a word
processor counts it (1 single, 1.5, 2 double); NIL when it asks for none or
for what is no number."
  (spacing-number (org-latex-setting node "spacing")))

(defun spacing-number (value)
  "VALUE, what a spacing setting says, as a word processor counts the space
between lines (1 single, 1.5, 2 double); NIL for none, or what is no number."
  (cond ((null value) nil)
        ((string-equal value "single") 1)
        ((string-equal value "double") 2)
        (t (let ((n (parse-decimal value))) (and n (< 0 n 10) n)))))

(defun export-paragraph-align (node)
  "How the paragraph NODE's align setting sets its lines: :LEFT, :RIGHT,
:CENTRE or :JUSTIFY; NIL when it says none of them."
  (export-side (org-latex-setting node "align")))

(defun export-position (node)
  "The margin the table or picture NODE's position setting puts it against:
:LEFT or :RIGHT; NIL for the middle."
  (let ((side (export-side (org-latex-setting node "position"))))
    (and (member side '(:left :right)) side)))

(defun export-columns (node)
  "How many columns NODE's columns setting sets its text in, two or more;
NIL for one."
  (let* ((value (org-latex-setting node "columns"))
         (n (and value (ignore-errors (parse-integer value)))))
    (and n (> n 1) n)))

(defun export-setting-no-p (node name)
  "True when NODE's setting NAME says no."
  (string-equal (org-latex-setting node name) "no"))

(defun picture-attributes (node &key latex)
  "The picture NODE's #+ATTR_LATEX: line as a plist (ORG-LATEX-ATTRIBUTES),
its width setting over the line's :width: as a LaTeX length with LATEX, as
written without; with LATEX, its place setting too, as :placement."
  (let ((attributes (copy-list (org-latex-attributes node "latex")))
        (width (org-latex-setting node "width"))
        (place (and latex (org-latex-setting node "place"))))
    (let ((width (if latex (org-latex-length width) width)))
      (when width (setf (getf attributes :width) width)))
    (when place (setf (getf attributes :placement) (if (string-equal place "here") "H" "htbp")))
    attributes))

(defparameter *table-lines*
  '(("top" :top) ("header" :header) ("rules" :rules) ("bottom" :bottom) ("left" :left) ("right" :right)
    ("columns" :columns) ("rows" :rows) ("box" :top :bottom :left :right)
    ("all" :top :header :rules :bottom :left :right :columns :rows) ("none"))
  "The words a table's lines setting takes, and the lines each prints.")

(defun table-lines (node)
  "The lines the table NODE's lines setting prints, as keywords of
*TABLE-LINES*: top header rules bottom when it has none; a word that names
none of them said and left out. The one reading of it for both exports."
  (let ((words (tuatara.text:split-words (org-latex-setting node "lines"))))
    (if (null words)
        '(:top :header :rules :bottom)
        (remove-duplicates
         (loop for word in words
               for entry = (assoc word *table-lines* :test #'string-equal)
               unless entry
                 do (export-note "A table's lines: ~a is none of ~{~a~^, ~}; left out" word (mapcar #'first *table-lines*))
               append (rest entry))))))

(defun export-table-header (rows)
  "How many of ROWS, a table's, are its header, those above its first rule,
when rows follow the rule; NIL when it has none."
  (let ((rule (position 'rule rows :key (lambda (r) (org-property r :type)))))
    (and rule (plusp rule) (< rule (1- (length rows))) rule)))

(defun table-column-alignment (index standard cookies)
  "How column INDEX of a table is aligned, :LEFT, :RIGHT or :CENTRE, as
`org-column-alignment' decides from its cell in COOKIES, the row of widths
and alignments, and its cells in STANDARD, the rows of cells."
  (flet ((text (row)
           (let ((cell (and row (nth index (org-node-children row)))))
             (if cell (org-latex-cell-text cell) ""))))
    (org-column-alignment (cons (text cookies) (mapcar #'text standard)))))

(defun export-column-alignment (node index special standard cookies)
  "How column INDEX of the table NODE is aligned, :LEFT, :RIGHT or :CENTRE:
as its align setting's word for it says, else as its numbers or words are
\(TABLE-COLUMN-ALIGNMENT); SPECIAL true when its first column, only marking
rows, is not counted in INDEX."
  (let ((side (export-side (nth index (tuatara.text:split-words (org-latex-setting node "align"))))))
    (if (member side '(:left :right :centre))
        side
        (table-column-alignment (+ index (if special 1 0)) standard cookies))))

(defun export-table-measure (columns special standard)
  "How the COLUMNS of a table print, as four values, each a list with one
entry a column: the lengths of its cells in STANDARD, the rows of cells, as
they print (ORG-LATEX-VISIBLE-LENGTH); its widest, at least 1; whether it
has words to wrap, wider than 20 characters with a space in a cell; and
the longest part of a cell that cannot go on to the next line
\(ORG-LATEX-LONGEST-PIECE). SPECIAL true when the first column, only
marking rows, is left out. The one measure of a table's columns for both
exports."
  (flet ((cell (row i) (nth i (nthcdr (if special 1 0) (org-node-children row)))))
    (let* ((lengths (loop for i below columns
                          collect (loop for row in standard
                                        for cell = (cell row i)
                                        collect (if cell (org-latex-visible-length (org-latex-cell-text cell)) 0))))
           (widest (mapcar (lambda (l) (reduce #'max l :initial-value 1)) lengths)))
      (values lengths widest
              (loop for i below columns
                    collect (and (> (nth i widest) 20)
                                 (some (lambda (row) (let ((cell (cell row i)))
                                                       (and cell (find #\Space (org-latex-cell-text cell)))))
                                       standard)))
              (loop for i below columns
                    collect (or (loop for row in standard
                                      for cell = (cell row i)
                                      maximize (if cell (org-latex-longest-piece (org-latex-cell-text cell)) 0))
                                0))))))

(defun export-table-widths (node columns)
  "The widths setting of the table NODE, one entry for each of its COLUMNS:
:SHARE for *, an equal share of what the others leave, :AUTO for auto or
none given, as wide as its text, else the length as written; NIL when the
setting is not given. The one reading of it for both exports."
  (let ((words (tuatara.text:split-words (org-latex-setting node "widths"))))
    (and words
         (loop for i below columns
               for word = (or (nth i words) "auto")
               collect (cond ((string= word "*") :share)
                             ((string-equal word "auto") :auto)
                             (t word))))))

(defun org-latex-font-command (value)
  "The LaTeX for the size of type VALUE names (*ORG-LATEX-FONT-SIZES*), a
LaTeX size command as written (\\small) taken as it is; NIL for none."
  (cond ((or (null value) (string= value "")) nil)
        ((char= (char value 0) #\\) value)
        (t (cdr (assoc value *org-latex-font-sizes* :test #'string-equal)))))

(defun setting-length (value)
  "VALUE, a length as a setting gives it, read: two values, the number and its
unit, :SHARE for a share of the text's width (30%, 33.3%), else :CM :MM :IN
:EM or :PT (a number alone is points); NIL for neither. The one reading of
lengths for both exports."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^([0-9]+(?:\\.[0-9]+)?)\\s*(%|cm|mm|in|em|pt)?$"
                                                                (string-trim " " (or value "")))
    (let ((n (and match (parse-decimal (aref groups 0)))))
      (when n
        (values n (let ((unit (aref groups 1)))
                    (cond ((null unit) :pt) ((string= unit "%") :share)
                          (t (intern (string-upcase unit) :keyword)))))))))

(defun org-latex-length (value)
  "VALUE, a share of the page (30%) or a length (5cm; a number alone points),
as a LaTeX length, or a share of one of LaTeX's widths as written
\(0.5\\textwidth); NIL for none, and for anything else, said after the export."
  (multiple-value-bind (n unit) (setting-length value)
    (cond ((null value) nil)
          ((eq unit :share) (format nil "~,3f\\linewidth" (/ n 100)))
          (n (format nil "~a~(~a~)" (points-text n) unit))
          ((cl-ppcre:scan "^[0-9]*\\.?[0-9]*\\\\(?:linewidth|textwidth|columnwidth|textheight|baselineskip)$" value) value)
          (t (export-note "~a is no length (6pt, 1em, 1cm, 30%); left out" value)))))

(defparameter *org-page-run-types*
  '(headline paragraph plain-list table src-block example-block quote-block center-block special-block
    verse-block fixed-width export-block dynamic-block)
  "The kinds of element a page setting is given to, and looked at beside one another (ORG-PAGE-RUN).")

(defun org-page-setting (node)
  "The page NODE's :EXPORT: drawer gives it: :LANDSCAPE, or NIL as standard."
  (and node (string-equal (or (org-latex-setting node "page") "") "landscape") :landscape))

(defun org-page-run (node)
  "NODE's page (ORG-PAGE-SETTING), and as second and third values whether a
run of pages so begins and ends with it: elements side by side with the same
page share them, the drawers and keywords between not counting."
  (let ((page (org-page-setting node)))
    (if (null page)
        (values nil nil nil)
        (let* ((siblings (remove-if-not (lambda (n) (member (org-node-type n) *org-page-run-types*))
                                        (and (org-node-parent node) (org-node-children (org-node-parent node)))))
               (at (position node siblings)))
          (values page
                  (or (null at) (zerop at) (not (eq page (org-page-setting (nth (1- at) siblings)))))
                  (or (null at) (not (eq page (org-page-setting (nth (1+ at) siblings))))))))))

(defvar *org-latex-turned* nil
  "True while what is written is on pages turned sideways already: a page
setting within it turns nothing again.")

(defun org-latex-with-page (node out writer)
  "Call WRITER with OUT, on pages turned sideways (*ORG-LATEX-LANDSCAPE*) when NODE's run of them
\(ORG-PAGE-RUN) is on such pages, begun and ended where the run is; within
pages turned already, as they are."
  (multiple-value-bind (page begins ends) (org-page-run node)
    (if (or (null page) *org-latex-turned*)
        (funcall writer out)
        (progn
          (when begins (format out "~&\\begin{tuataralandscape}~%"))
          (let ((*org-latex-turned* t)) (funcall writer out))
          (when ends (format out "~&\\end{tuataralandscape}~%"))))))

(defun latex-template-body-in-columns-p (template)
  "True when the LaTeX TEMPLATE puts the note's text, {{{body}}}, in columns
\(within \\begin{multicols}), as one made from a two-column Word document does."
  (let ((at (search "{{{body}}}" template)))
    (and at
         (> (search-count "\\begin{multicols}" template :end at)
            (search-count "\\end{multicols}" template :end at)))))

(defparameter *latex-paper-widths*
  '(("a3paper" . 297) ("a4paper" . 210) ("a5paper" . 148) ("a6paper" . 105) ("b5paper" . 176)
    ("letterpaper" . 2159/10) ("legalpaper" . 2159/10) ("executivepaper" . 18415/100))
  "The width in millimetres of each paper size a geometry option names.")

(defun latex-length-millimetres (value)
  "VALUE, a length as written in a template (28mm, 1.5cm, 1in, 12pt), in
millimetres; NIL for anything else."
  (multiple-value-bind (n unit) (setting-length value)
    (and n (case unit
             (:mm n) (:cm (* 10 n)) (:in (* 254/10 n)) (:pt (* 3515/10000 n))))))

(defun latex-template-text-characters (template)
  "About how many characters of the normal size go across the text of the
page the LaTeX TEMPLATE sets with its geometry package (a paper size or
paperwidth, and the margins left and right, margin or hmargin, or
textwidth), as the enhanced export's own A4 page, 154 mm across, has
*ORG-LATEX-TEXT-CHARACTERS*; NIL when the template says too little to know."
  (let ((options (nth-value 1 (cl-ppcre:scan-to-strings "\\\\usepackage\\[([^\\]]*)\\]\\{geometry\\}" template))))
    (when options
      (let ((given '()))
        (dolist (option (cl-ppcre:split "\\s*,\\s*" (string-trim '(#\Space #\Newline) (aref options 0))))
          (let ((at (position #\= option)))
            (push (if at
                      (cons (string-trim " " (subseq option 0 at)) (string-trim " {}" (subseq option (1+ at))))
                      (cons (string-trim " " option) t))
                  given)))
        (flet ((value (&rest keys)
                 (loop for key in keys
                       for found = (cdr (assoc key given :test #'string=))
                       when (stringp found) return found)))
          (let* ((paper (or (let ((w (value "paperwidth"))) (and w (latex-length-millimetres w)))
                            (loop for (name . width) in *latex-paper-widths*
                                  when (assoc name given :test #'string=) return width)))
                 (text (let ((w (value "textwidth" "width"))) (and w (latex-length-millimetres w))))
                 (left (let ((m (value "left" "lmargin" "inner" "margin" "hmargin"))) (and m (latex-length-millimetres m))))
                 (right (let ((m (value "right" "rmargin" "outer" "margin" "hmargin"))) (and m (latex-length-millimetres m))))
                 (width (or text (and paper left right (- paper left right)))))
            (and width (plusp width)
                 (max 30 (round (* *org-latex-text-characters* width) 154)))))))))

(defun latex-line-stretch (spacing)
  "The stretch LaTeX's \\linespread takes for the SPACING between lines a word
processor counts (1 single, 1.5, 2 double), as setspace's one and a half
and double spacing have it."
  (if (<= spacing 3/2)
      (+ 1 (* (- spacing 1) 0.426))
      (+ 1.213 (* (- spacing 3/2) 0.81))))

(defun org-latex-with-settings (node out writer)
  "Call WRITER with a stream, NODE's LaTeX written to it, and write that to OUT
in what NODE's settings for any element say (ORG-LATEX-SETTING): indent, its
font, the space between its lines and its columns."
  (let ((indent (org-latex-length (org-latex-setting node "indent")))
        (font (let ((size (org-latex-font-command (org-latex-setting node "font")))
                    (spacing (export-line-spacing node)))
                (if spacing
                    (format nil "~@[~a~]\\linespread{~,3f}\\selectfont" size (latex-line-stretch spacing))
                    size)))
        (columns (export-columns node)))
    (if (not (or indent font columns))
        (funcall writer out)
        (let ((inner (let ((*org-latex-in-columns* (or *org-latex-in-columns* (and columns t))))
                       (with-output-to-string (s) (funcall writer s)))))
          (when indent (format out "\\begin{list}{}{\\setlength{\\leftmargin}{~a}}\\item\\relax~%" indent))
          (when font (format out "{~a~%" font))
          (when columns (format out "\\begin{multicols}{~d}~%" columns))
          (write-string inner out)
          (when columns (format out "\\end{multicols}~%"))
          (when font (format out "\\par}~%"))
          (when indent (format out "\\end{list}~%"))))))

;;; Shown and written for you

(defparameter *export-tips-both*
  '("\\\\ at the end of a line" "a line break, the paragraph going on on the next line"
    "a blank line" "a new paragraph"
    "*bold* /italic/" "bold and italic; _underlined_, +struck through+, =verbatim=, ~code~"
    "-- and ---" "an en dash and an em dash; ... an ellipsis"
    "10\\nbsp{}kg" "a space no line breaks at"
    "hyphen\\-ation" "where a long word may be broken over two lines"
    "H_{2}O, x^{2}" "below and above the line (with #+options: ^:{})"
    "[fn:1] and [fn:1] text" "a footnote: the mark, and its text on a line of its own"
    "-----" "a line across the page: five dashes alone on a line"
    "#+caption: and #+name:" "above a table or picture: Table N or Figure N, and [[name]] a link to it"
    "[[*Heading]]" "a link to that heading, by its number"
    "|---| under the top rows" "a table's header: those rows, in bold, again on each page")
  "What writes the same in the PDF and the Word document, each what is written and what it does, for \\[describe-export-settings].")

(defparameter *export-tips-latex*
  '("\\newline" "a line break (\\\\ at the end of a line works in both)"
    "#+latex: \\clearpage" "a new page (new page: before in an :EXPORT: drawer works in both)"
    "\\vspace{1em}" "space between paragraphs (space before: 1em works in both)"
    "@@latex:...@@" "LaTeX in the middle of a line"
    "#+begin_export latex" "lines of LaTeX of its own"
    "#+attr_latex:" "LaTeX's own layout of a table or picture (an :EXPORT: drawer works in both)")
  "What only the PDF takes, each what is written and what it does, for \\[describe-export-settings].")

(define-command describe-export-settings ()
  "Show the settings an :EXPORT: drawer may hold in an Org note, what each sets
and what it takes: how the enhanced export sets a heading, a table, a list, a
block or a picture, in words rather than LaTeX; then tips on what writes the
same in the PDF and the Word document, and what only in the PDF, the lines
filled to *HELP-COLUMN*. \\[org-insert-export-drawer] writes a drawer with
those that fit what point is on."
  (show-help
   (with-output-to-string (s)
     (format s "Settings of an :EXPORT: drawer~%~%")
     (write-filled s "" "A drawer right under a heading sets the heading; one just before a table, a list,
                         a block or a picture sets that, above its #+caption: and #+name: lines. Each line is
                         NAME: VALUE; auto leaves it as the export would have it. \\[org-insert-export-drawer]
                         writes one for what point is on.")
     (format s "~%:EXPORT:~%widths: 30% * *~%align: left right centre~%font: small~%:END:~%~%")
     (dolist (kind '(:heading :table :list :block :paragraph :picture))
       (format s "~@(~a~)~%" (case kind (:heading "A heading") (:table "A table") (:list "A list")
                                   (:block "A block (code, example, quote)") (:paragraph "A paragraph") (:picture "A picture")))
       (loop for (name kinds words) in *org-latex-settings*
             when (member kind kinds)
               do (write-filled s (format nil "  ~a:" name) words :indent 6))
       (terpri s))
     (format s "A table's lines~%")
     (loop for (example words) on '("lines: none" "no line at all"
                                    "lines: top header bottom" "above it, under the header, under it"
                                    "lines: box header" "a frame, and a line under the header"
                                    "lines: all" "every line, a grid")
           by #'cddr
           do (write-filled s (format nil "  ~26a " example) words))
     (terpri s)
     (write-filled s "" "Other programs exporting Org notes print a drawer's lines unless the note says
                         #+options: d:(not \"EXPORT\" \"LOGBOOK\"), which \\[org-insert-export-drawer] adds.")
     (format s "~%Tips and tricks~%~%In the PDF and the Word document alike~%")
     (loop for (written words) on *export-tips-both* by #'cddr
           do (write-filled s (format nil "  ~26a " written) words))
     (format s "~%In the PDF only (the Word document prints them as written, or leaves~%them out)~%")
     (loop for (written words) on *export-tips-latex* by #'cddr
           do (write-filled s (format nil "  ~26a " written) words)))))

(defun org-latex-settings-target (tree position)
  "What an :EXPORT: drawer written for the character POSITION of TREE would
set: the innermost table, list, block or picture holding it, else the
heading it is under; NIL when it is under none. As a second value its
kind (ORG-LATEX-NODE-KIND)."
  (let ((found nil))
    (labels ((walk (node)
               (when (and (<= (org-node-begin node) position) (< position (org-node-end node)))
                 (let ((kind (org-latex-node-kind node)))
                   (when (and kind (not (member kind '(:heading :paragraph))))
                     (setf found node)))
                 (mapc #'walk (org-node-children node)))))
      (walk tree))
    (let ((node (or found (org-current-headline tree position))))
      (values node (and node (org-latex-node-kind node))))))

(defparameter *org-latex-hidden-drawers-option* "d:(not \"EXPORT\" \"LOGBOOK\")"
  "The #+options: word that has other programs exporting the note leave :EXPORT: drawers out.")

(define-command org-insert-export-drawer ()
  "Write an :EXPORT: drawer for the table, list, block or picture point is on,
or else for the heading it is under, holding the settings that fit it, each
auto to change (\\[describe-export-settings] says what each takes). The note's
#+options: get d:(not \"EXPORT\" \"LOGBOOK\") when they say nothing of drawers,
so that other programs exporting the note as Org leave the drawers out too."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (tree text) (org-tree-now buffer)
      (let ((*org-current-text* text))
        (multiple-value-bind (node kind) (and tree (org-latex-settings-target tree (org-point-char buffer)))
          (if (null node)
              (message "Nothing here an :EXPORT: drawer sets: a heading, a table, a list, a block or a picture")
              (let* ((names (loop for (name kinds) in *org-latex-settings* when (member kind kinds) collect name))
                     (drawer (format nil ":EXPORT:~%~{~a: auto~%~}:END:~%" names))
                     (at (if (eq kind :heading)
                             ;; under the heading, after its planning and properties
                             (let* ((section (find 'section (org-node-children node) :key #'org-node-type))
                                    (leading (and section (remove-if-not (lambda (n) (member (org-node-type n) '(planning property-drawer)))
                                                                         (org-node-children section)))))
                               (if leading
                                   (org-node-end (car (last leading)))
                                   (min (length text) (1+ (or (position #\Newline text :start (org-node-begin node)) (1- (length text)))))))
                             (org-node-begin node))))
                (org-replace-chars buffer at at drawer :point (+ at (length ":EXPORT:") 1))
                (let ((said (org-latex-hide-drawers-option buffer)))
                  (message "An :EXPORT: drawer for this ~(~a~): change auto to what you want (M-x describe-export-settings)~@[; ~a~]"
                           kind said)))))))))

(defun org-latex-hide-drawers-option (buffer)
  "Have BUFFER's #+options: leave :EXPORT: drawers out when other programs export it
\(*ORG-LATEX-HIDDEN-DRAWERS-OPTION*), unless they say something of drawers
already; a sentence saying what was done, or NIL."
  (let* ((text (org-buffer-text buffer)))
    (cond ((cl-ppcre:scan "(?im)^[ \\t]*#\\+options:.*(?:^|\\s)d:" text) nil)
          (t (multiple-value-bind (start end) (cl-ppcre:scan "(?im)^[ \\t]*#\\+options:[^\\n]*" text)
               (if start
                   (progn
                     (org-replace-chars buffer end end (format nil " ~a" *org-latex-hidden-drawers-option*))
                     "#+options: gained d:(not \"EXPORT\" \"LOGBOOK\") for other programs' exports")
                   ;; after the keyword lines at the top, else at the very top
                   (let ((after (or (nth-value 1 (cl-ppcre:scan "^(?:[ \\t]*#\\+[^\\n]*\\n)+" text)) 0)))
                     (org-replace-chars buffer after after (format nil "#+options: ~a~%" *org-latex-hidden-drawers-option*))
                     "a #+options: line keeps them out of other programs' exports")))))))
