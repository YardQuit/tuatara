;;;; lisp/org/docx.lisp --- Org notes written as Word documents (.docx) by Tuatara itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x org-export-docx writes the Org note of the buffer as a Word document:
;;;; the note read as the PDF export reads it (its macros expanded, its
;;;; #+options and its :EXPORT: drawers' settings), written as Word's own XML
;;;; (WordprocessingML) in a zip archive (ZIP-OCTETS). Headings carry their
;;;; numbers as the PDF does (1, 1.2, the annexes anew), so that the
;;;; contents and the links to them read alike; tables keep a header row on
;;;; each page; lists keep their kind of number; pictures are in the file;
;;;; links inside the note lead where they lead in the PDF. A text hidden by
;;;; redact or protect is a black bar of as many characters, the text itself
;;;; nowhere in the file. The look is Tuatara's own (Word's styles Normal,
;;;; Heading 1 and so on), in the fonts the PDF's article template takes.

(in-package #:tuatara.defaults)

(defstruct (docx (:constructor make-docx ()) (:copier nil))
  "What a Word document being written gathers besides its text: RELATIONS,
each (ID TYPE TARGET EXTERNAL), the files and addresses its XML names; MEDIA,
each (NAME . OCTETS), its pictures; LISTS, each (ID KIND START), the
numbered and bulleted lists; FOOTNOTES, each (ID . XML); the counts of
FIGURES and TABLES; NUMBERS, each headline's number by node; BOOKMARKS, a
name by node; SECTION, the page's properties as XML, from which a section of
columns takes its own; TEXT-WIDTH, the width the text has, in twentieths of
a point; LIST-BASE, the first number its lists take, above a template's;
MEDIA-PREFIX, what its pictures' names begin with; PICTURES, each (PATH NAME
ID), the file each picture in MEDIA was taken from, its name there and its
relation; DRAWINGS, how many pictures it shows."
  (relations '()) (media '()) (pictures '()) (drawings 0) (lists '()) (footnotes '()) (figures 0) (tables 0)
  (numbers (make-hash-table :test 'eq)) (bookmarks (make-hash-table :test 'eq)) (next-bookmark 0)
  (section "") (text-width 8730) (list-base 3) (media-prefix "image"))

(defvar *docx* nil
  "The DOCX of the Word document being written.")

;;; XML

(defparameter *docx-xml-declaration* "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>"
  "What every XML part of a Word document begins with.")

(defun docx-xml-part (control &rest arguments)
  "A part of a Word document: *DOCX-XML-DECLARATION*, a newline, then CONTROL
filled with ARGUMENTS as FORMAT fills it."
  (format nil "~a~%~?" *docx-xml-declaration* control arguments))

(defun docx-xml-part-to (stream control &rest arguments)
  "Write to STREAM the part DOCX-XML-PART makes of CONTROL and ARGUMENTS."
  (format stream "~a~%~?" *docx-xml-declaration* control arguments))

(defun docx-relationships (relationships)
  "A part of relationships (_rels/...rels) holding RELATIONSHIPS, Relationship elements as text."
  (docx-xml-part "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">~a</Relationships>"
                 relationships))

(defun docx-relations-xml (relations)
  "RELATIONS, each (ID TYPE TARGET EXTERNAL), as Relationship elements, the oldest first."
  (with-output-to-string (s)
    (dolist (r (reverse relations))
      (destructuring-bind (id type target external) r
        (format s "<Relationship Id=\"~a\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/~a\" Target=\"~a\"~:[~; TargetMode=\"External\"~]/>"
                id type (xml-escape target) external)))))

(defun docx-relation (type target &key external)
  "The id of a relation of the document to TARGET, of TYPE (image, hyperlink),
made the first time it is asked for."
  (let ((found (find-if (lambda (r) (and (string= (second r) type) (string= (third r) target))) (docx-relations *docx*))))
    (if found
        (first found)
        ;; a name of their own, so that a template's relations keep theirs
        (let ((id (format nil "rIdTuatara~d" (1+ (length (docx-relations *docx*))))))
          (push (list id type target external) (docx-relations *docx*))
          id))))

(defun docx-bookmark (node)
  "The name of the bookmark at NODE, a headline or an element links lead to, made once."
  (or (gethash node (docx-bookmarks *docx*))
      (setf (gethash node (docx-bookmarks *docx*)) (format nil "_Tuatara~d" (incf (docx-next-bookmark *docx*))))))

(defun docx-bookmarked (node xml)
  "XML within NODE's bookmark."
  (let ((name (docx-bookmark node)) (id (incf (docx-next-bookmark *docx*))))
    (format nil "<w:bookmarkStart w:id=\"~d\" w:name=\"~a\"/>~a<w:bookmarkEnd w:id=\"~d\"/>" id name xml id)))

;;; Runs: text in one look

(defparameter *docx-font-sizes*
  '(("tiny" . 6/11) ("scriptsize" . 8/11) ("footnote" . 9/11) ("footnotesize" . 9/11) ("small" . 10/11)
    ("normal" . 1) ("large" . 12/11) ("larger" . 14/11) ("largest" . 17/11))
  "The sizes of type the font setting takes, each as a part of the size the
text has otherwise, as LaTeX's are of its 11 points.")

(defvar *docx-base-size* 22
  "The size of the text being written before any font setting, in half points:
11 points, or a template's own size.")

(defvar *docx-character-ratio* 3/5
  "How wide a character of the text's font is, as a part of its size: a
monospaced font's own width when a template's font is one installed here
\(FONT-CHARACTER-RATIO), else Courier New's 0.6, the widest the text is likely
set in.")

(defun docx-character-width ()
  "How wide a character is taken to be, in twentieths of a point, when a
table's columns or a code block's lines are measured: *DOCX-CHARACTER-RATIO*
of *DOCX-BASE-SIZE* (132 at 11 points in Courier New)."
  (round (* 10 *docx-base-size* *docx-character-ratio*)))

(defun font-file-tables (octets)
  "The tables of the TrueType or OpenType font OCTETS, each (TAG OFFSET LENGTH); NIL for another kind."
  (when (and (>= (length octets) 12) (member (tuatara.io:octets-be octets 0 4) '(#x00010000 #x4F54544F)))
    (loop for i below (tuatara.io:octets-be octets 4 2)
          for at = (+ 12 (* 16 i))
          while (<= (+ at 16) (length octets))
          collect (list (tuatara.text:octets-latin-1 octets :start at :end (+ at 4))
                        (tuatara.io:octets-be octets (+ at 8) 4) (tuatara.io:octets-be octets (+ at 12) 4)))))

(defun font-character-ratio (family)
  "The width of a character of the font FAMILY as a part of its size, when it
is installed here (fontconfig finds it by that name) and monospaced: nine in
ten of its glyphs that width. NIL otherwise."
  (let* ((found (fontconfig-match family "%{family}|%{file}"))
         (bar (and found (position #\| found))))
    (when (and bar (member family (mapcar (lambda (name) (string-trim " " name))
                                          (uiop:split-string (subseq found 0 bar) :separator '(#\,)))
                           :test #'string-equal))
      (ignore-errors
       (let* ((octets (tuatara.io:file-octets (subseq found (1+ bar))))
              (tables (font-file-tables octets)))
         (flet ((table (tag) (second (assoc tag tables :test #'string=))))
           (let ((head (table "head")) (hhea (table "hhea")) (hmtx (table "hmtx")))
             (when (and head hhea hmtx)
               (let* ((units (tuatara.io:octets-be octets (+ head 18) 2))
                      (metrics (tuatara.io:octets-be octets (+ hhea 34) 2))
                      (widths (loop for i below metrics collect (tuatara.io:octets-be octets (+ hmtx (* 4 i)) 2)))
                      (counts (make-hash-table)))
                 (dolist (w widths) (when (plusp w) (incf (gethash w counts 0))))
                 (let ((common 0) (most 0))
                   (maphash (lambda (w n) (when (> n most) (setf common w most n))) counts)
                   (and (plusp units) (>= (* 10 most) (* 9 (count-if #'plusp widths)))
                        (/ common units))))))))))))

(defparameter *docx-column-gap* 360
  "The space between the columns of a section of columns, in twentieths of a point (0.635 cm).")

(defvar *docx-columns* 1
  "How many columns the text being written is set in.")

(defvar *docx-run* '()
  "The look of the text being written, a plist: :BOLD :ITALIC :UNDERLINE
:STRIKE :CODE :VERT (superscript or subscript) :SCALE (of *DOCX-BASE-SIZE*) :STYLE.")

(defun docx-run-properties (look)
  "The w:rPr of LOOK (*DOCX-RUN*), in the order Word reads them; an empty string for none."
  (let ((parts (remove nil (list (and (getf look :style) (format nil "<w:rStyle w:val=\"~a\"/>" (xml-escape (getf look :style))))
                                 (and (getf look :code) "<w:rStyle w:val=\"VerbatimChar\"/>")
                                 (and (getf look :bold) "<w:b/>")
                                 (and (getf look :italic) "<w:i/>")
                                 (and (getf look :strike) "<w:strike/>")
                                 (and (getf look :colour) (format nil "<w:color w:val=\"~a\"/>" (getf look :colour)))
                                 (and (getf look :scale)
                                      (let ((size (round (* *docx-base-size* (getf look :scale)))))
                                        (format nil "<w:sz w:val=\"~d\"/><w:szCs w:val=\"~d\"/>" size size)))
                                 (and (getf look :underline) "<w:u w:val=\"single\"/>")
                                 (and (getf look :shade) (format nil "<w:shd w:val=\"clear\" w:color=\"auto\" w:fill=\"~a\"/>" (getf look :shade)))
                                 (and (getf look :vert) (format nil "<w:vertAlign w:val=\"~a\"/>" (getf look :vert)))))))
    (if parts (format nil "<w:rPr>~{~a~}</w:rPr>" parts) "")))

(defun docx-run (text &optional (look *docx-run*))
  "TEXT as runs in LOOK: a tab as w:tab, a line end as w:br."
  (if (zerop (length text))
      ""
      (with-output-to-string (out)
        (let ((properties (docx-run-properties look)))
          (format out "<w:r>~a" properties)
          (let ((start 0))
            (flet ((piece (end)
                     (when (< start end)
                       (format out "<w:t xml:space=\"preserve\">~a</w:t>" (xml-escape (subseq text start end))))))
              (loop for i from 0 below (length text)
                    for c = (char text i)
                    do (case c
                         (#\Tab (piece i) (write-string "<w:tab/>" out) (setf start (1+ i)))
                         (#\Newline (piece i) (write-string "<w:br/>" out) (setf start (1+ i)))))
              (piece (length text))))
          (write-string "</w:r>" out)))))

(defun docx-special-strings (string)
  "STRING with Org's special strings as the characters they stand for: --- a
long dash, -- a short one, ... an ellipsis, \\- a place to break a word."
  (if (org-latex-option "-" t)
      (let* ((s (cl-ppcre:regex-replace-all "---" string (string (code-char #x2014))))
             (s (cl-ppcre:regex-replace-all "--" s (string (code-char #x2013))))
             (s (cl-ppcre:regex-replace-all "\\.\\.\\." s (string (code-char #x2026)))))
        (cl-ppcre:regex-replace-all "\\\\-" s (string (code-char #xAD))))
      string))

(defvar *docx-keep-lines* nil
  "True while the text written keeps its lines, as a verse's does.")

(defun docx-plain-text (text)
  "TEXT, Org text between objects, as runs: each line end, with the spaces
around it, one space, as a paragraph's lines are joined, or kept with
*DOCX-KEEP-LINES*; each straight quote as the mark for it
\(EXPORT-SMART-QUOTES)."
  (docx-run (docx-special-strings (export-smart-quotes (if *docx-keep-lines* text (cl-ppcre:regex-replace-all "[ \\t]*\\n[ \\t]*" text " "))))))

;;; Objects: the text of a paragraph, a cell, a headline

(defun docx-objects-between (objects begin end out)
  "Write the text from BEGIN below END to OUT as runs, OBJECTS, the objects in
it, as their own and the rest as text."
  (org-export-objects-between objects begin end out
                              :text #'docx-plain-text
                              :object #'docx-object
                              :blank (lambda (n) (docx-run (make-string n :initial-element #\Space)))))

(defun docx-objects (node)
  "The contents of NODE, text and objects, as runs, without the line end after it."
  (if (org-node-contents-begin node)
      (with-output-to-string (s)
        (docx-objects-between (org-node-children node) (org-node-contents-begin node)
                              (let ((end (org-node-contents-end node)))
                                (if (and (> end (org-node-contents-begin node)) (char= #\Newline (char *org-latex-text* (1- end))))
                                    (1- end)
                                    end))
                              s))
      ""))

(defun docx-as-written (node)
  "NODE's text as it is written, as runs."
  (docx-run (subseq *org-latex-text* (org-node-begin node) (- (org-node-end node) (org-node-post-blank node)))))

(defun docx-string (string)
  "STRING, a line of Org text such as a keyword's value, as runs (ORG-PARSE-LINE)."
  (let ((*org-latex-text* string))
    (docx-objects (org-parse-line string))))

(defun docx-redaction (n)
  "A black bar as wide as N characters, as many letters n black on black, as
the PDF measures its box, holding none of what it hides."
  (docx-run (make-string n :initial-element #\n) (list* :colour "000000" :shade "000000" *docx-run*)))

(defvar *docx-paragraph-style* nil
  "The style of the paragraphs being written, or NIL for the normal one.")

(defun docx-footnote (node out)
  "Write the footnote reference NODE to OUT, its text among the document's footnotes."
  (let* ((definition (with-org-latex-expanding (:footnote (or (org-property node :label) node)) (nil)
                       (if (eq (org-property node :type) 'inline)
                           (format nil "<w:p><w:pPr><w:pStyle w:val=\"FootnoteText\"/></w:pPr>~a</w:p>" (docx-objects node))
                           (let ((found (gethash (org-property node :label) (org-latex-footnotes *org-latex*))))
                             (and found (with-output-to-string (s)
                                          (let ((*docx-paragraph-style* "FootnoteText"))
                                            (dolist (child (org-node-children found)) (docx-element child s)))))))))
         (id (+ 2 (length (docx-footnotes *docx*)))))
    (when (org-latex-option "f" t)
      (push (cons id (if (and definition (search "<w:p" definition)) definition
                         (format nil "<w:p><w:pPr><w:pStyle w:val=\"FootnoteText\"/></w:pPr>~a</w:p>" (or definition ""))))
            (docx-footnotes *docx*))
      (format out "<w:r><w:rPr><w:rStyle w:val=\"FootnoteReference\"/></w:rPr><w:footnoteReference w:id=\"~d\"/></w:r>" id))))

(defun docx-plain-title (text)
  "TEXT, a heading's title as written once its macros are expanded, as plain
words for the contents and links: a hidden text [redacted], other export
snippets (@@latex:...@@) left out, so that neither markup nor what is hidden
shows."
  (cl-ppcre:regex-replace-all *org-export-snippet-re*
                              (cl-ppcre:regex-replace-all *org-redaction-scanner* text *redacted-words*)
                              ""))

(defun docx-link-words (target)
  "What a link with no words of its own shows of TARGET: a heading's number
and title, Table N or Figure N for a captioned element, else its own words."
  (case (org-node-type target)
    (headline (let ((number (gethash target (docx-numbers *docx*))))
                (if (consp number) (format nil "section ~a" (first number)) (docx-plain-title (org-property target :raw-value)))))
    (t (or (let ((number (gethash target (docx-numbers *docx*)))) (and (stringp number) number))
           (org-property target :value) (org-property target :name) ""))))

(defun docx-link (node out)
  "Write the link NODE to OUT: a picture shown, an address of the web or a
file as a hyperlink, a place in the note as a link to its bookmark."
  (let* ((type (org-property node :type))
         (path (org-property node :path))
         ;; its words in the document's link style (blue and underlined, or black, as its styles say)
         (words (and (org-node-contents-begin node) (not (org-latex-image-link-p node))
                     (let ((*docx-run* (list* :style "Hyperlink" *docx-run*))) (docx-objects node)))))
    (cond ((org-latex-image-link-p node)
           (write-string (docx-picture node (org-latex-attributes (org-node-parent node) "latex")) out))
          ((or (string= type "file") (org-link-address type path))
           (let* ((url (if (string= type "file") (path-uri (org-export-path path)) (org-link-address type path)))
                  (id (docx-relation "hyperlink" url :external t)))
             (format out "<w:hyperlink r:id=\"~a\">~a</w:hyperlink>" id
                     (or words (docx-run path (list* :style "Hyperlink" *docx-run*))))))
          (t
           (let ((target (org-latex-link-target node)))
             (if target
                 (format out "<w:hyperlink w:anchor=\"~a\">~a</w:hyperlink>" (docx-bookmark target)
                         (or words (docx-run (docx-link-words target) (list* :style "Hyperlink" *docx-run*))))
                 (write-string (or words (docx-run path)) out)))))))

(defun docx-object (node out)
  "Write the object NODE to OUT as runs."
  (flet ((within (&rest look)
           (let ((*docx-run* (append look *docx-run*)))
             (write-string (docx-objects node) out))))
    (case (org-node-type node)
      (bold (within :bold t))
      (italic (within :italic t))
      (underline (within :underline t))
      (strike-through (within :strike t))
      ((code verbatim inline-src-block) (write-string (docx-run (org-property node :value) (list* :code t *docx-run*)) out))
      (entity (let ((character (gethash (org-property node :name) *org-entities*)))
                (write-string (if (and character (org-latex-option "e" t)) (docx-run character) (docx-as-written node)) out)))
      (line-break (write-string "<w:r><w:br/></w:r>" out))
      (export-snippet
       (when (string-equal (org-property node :back-end) "docx")
         (let ((value (org-property node :value)))
           (when (uiop:string-prefix-p "redacted:" value)
             (write-string (docx-redaction (or (ignore-errors (parse-integer value :start 9)) 1)) out)))))
      (footnote-reference (docx-footnote node out))
      (link (docx-link node out))
      (target (write-string (docx-bookmarked node "") out))
      (radio-target (write-string (docx-bookmarked node (docx-objects node)) out))
      (timestamp (when (org-latex-option "<" t)
                   (write-string (docx-run (org-property node :raw-value) (list* :italic t *docx-run*)) out)))
      ((subscript superscript)
       (let ((script (org-latex-option "^" "{}")))
         (if (or (null script) (and (equal script "{}") (not (org-property node :use-brackets-p))))
             (write-string (docx-as-written node) out)
             (within :vert (if (eq (org-node-type node) 'subscript) "subscript" "superscript")))))
      (macro (let ((value (org-latex-macro-value node)))
               (write-string (if value
                                 (with-org-latex-expanding (:macro (org-property node :key)) ((docx-as-written node))
                                   (docx-string value))
                                 (docx-as-written node))
                             out)))
      ((inline-babel-call) nil)
      ;; what is left as written: a fragment of LaTeX, a macro not known, a citation
      (t (write-string (docx-as-written node) out)))))

;;; Pictures

(defun docx-picture-size (path)
  "The width and height of the picture at PATH in pixels, as its first octets
give them (TUATARA.IO:OCTETS-PICTURE-SIZE), or NIL."
  (let ((head (tuatara.io:file-head path)))
    (and head (tuatara.io:octets-picture-size head))))

(defun docx-length (value)
  "VALUE, a length as a setting gives it (5cm, 30mm, 2in, 22em, 12pt), in
twentieths of a point; a share (30%) of the text's width; NIL for neither."
  (multiple-value-bind (n unit) (setting-length value)
    (when n
      (round (* n (ecase unit
                    (:share (/ (docx-text-width *docx*) 100))
                    (:cm 567) (:mm 567/10) (:in 1440) (:em 220) (:pt 20)))))))

(defun docx-media-name (picture)
  "The name the picture file PICTURE takes among the document's media, the
next free: image1.png, image2.jpeg."
  (let ((type (string-downcase (or (tuatara.io:native-file-type picture) ""))))
    (format nil "~a~d.~a" (docx-media-prefix *docx*) (1+ (length (docx-media *docx*))) (if (string= type "jpg") "jpeg" type))))

(defun docx-picture (link attributes &key width height asked)
  "A run showing the picture LINK leads to, in the document's media, as wide
as the width setting or WIDTH (twentieths of a point) says, or as high as
HEIGHT says, else its own width at 96 pixels an inch but never wider than
the text; a note when it cannot be shown or is outside the folders an export
may take files from (ORG-EXPORT-FILE-ALLOWED-P), and its name in its place.
A picture your own template ASKED the note for ({{{logo:path}}}) shows from
wherever the note names it, as the PDF export's does."
  (let* ((full (or (ignore-errors (org-export-path (org-property link :path)))
                   (org-property link :path)))
         (picture (multiple-value-bind (made why) (if (or asked (org-export-file-allowed-p full)) (latex-picture full) (values nil :outside))
                    (cond ((and made (member (string-downcase (or (tuatara.io:native-file-type made) "")) '("png" "jpg" "jpeg") :test #'string=))
                           made)
                          ((eq why :outside) nil)
                          (why (export-note "~a" why))
                          (t (export-note "~a: a Word document takes PNG and JPEG pictures, not this one" (tuatara.io:native-file-name full)))))))
    (if (null picture)
        (docx-run (tuatara.io:native-file-name full))
        (multiple-value-bind (pw ph) (docx-picture-size picture)
          (let* ((pw (or pw 400)) (ph (or ph 300))
                 (natural (* pw 15))    ; a pixel at 96 to the inch, in twentieths of a point
                 (asked (or width (docx-length (getf attributes :width)) (and height (round (* height pw) ph))))
                 (twips (min (docx-text-width *docx*) (or asked natural)))
                 (cx (* twips 635)) (cy (round (* cx ph) pw))
                 (known (rest (assoc picture (docx-pictures *docx*) :test #'equal)))
                 (name (or (first known) (docx-media-name picture)))
                 (id (or (second known)
                         (let ((id (progn (push (cons name (tuatara.io:file-octets picture)) (docx-media *docx*))
                                          (docx-relation "image" (format nil "media/~a" name)))))
                           (push (list picture name id) (docx-pictures *docx*))
                           id)))
                 (n (incf (docx-drawings *docx*))))
            (format nil "<w:r><w:drawing><wp:inline xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" distT=\"0\" distB=\"0\" distL=\"0\" distR=\"0\"><wp:extent cx=\"~d\" cy=\"~d\"/>~
<wp:docPr id=\"~d\" name=\"Picture ~d\"/><a:graphic xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\">~
<a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/picture\"><pic:pic xmlns:pic=\"http://schemas.openxmlformats.org/drawingml/2006/picture\">~
<pic:nvPicPr><pic:cNvPr id=\"~d\" name=\"~a\"/><pic:cNvPicPr/></pic:nvPicPr><pic:blipFill><a:blip r:embed=\"~a\"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>~
<pic:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/><a:ext cx=\"~d\" cy=\"~d\"/></a:xfrm><a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom></pic:spPr></pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r>"
                    cx cy n n n name id cx cy))))))

;;; Paragraphs

(defvar *docx-indent* 0
  "How far the paragraphs being written are set in, in twentieths of a point.")

(defvar *docx-line-spacing* nil
  "The space between the lines of the paragraphs being written, as a word processor counts it (1.5); NIL as their style has it.")

(defvar *docx-at-foot* nil
  "True while the paragraphs being written go in a frame at the foot of the page, as a signature does.")

(defun docx-paragraph (runs &key style justify keep-next page-break numbering)
  "A paragraph of RUNS: in STYLE (else *DOCX-PARAGRAPH-STYLE*), JUSTIFY (left
center right both), kept with the next with KEEP-NEXT, after a page break
with PAGE-BREAK, an item of a list with NUMBERING, (ID . LEVEL); its lines
as far apart as *DOCX-LINE-SPACING* says."
  (let* ((style (or style *docx-paragraph-style*))
         (properties (remove nil (list (and style (format nil "<w:pStyle w:val=\"~a\"/>" (xml-escape style)))
                                       (and keep-next "<w:keepNext/>")
                                       (and page-break "<w:pageBreakBefore/>")
                                       (and *docx-at-foot*
                                            (format nil "<w:framePr w:w=\"~d\" w:wrap=\"around\" w:vAnchor=\"margin\" w:hAnchor=\"margin\" w:yAlign=\"bottom\"/>"
                                                    (docx-text-width *docx*)))
                                       (and numbering (format nil "<w:numPr><w:ilvl w:val=\"~d\"/><w:numId w:val=\"~d\"/></w:numPr>" (cdr numbering) (car numbering)))
                                       (and *docx-line-spacing* (format nil "<w:spacing w:line=\"~d\" w:lineRule=\"auto\"/>" (round (* 240 *docx-line-spacing*))))
                                       (and (plusp *docx-indent*) (not numbering) (format nil "<w:ind w:left=\"~d\"/>" *docx-indent*))
                                       (and justify (format nil "<w:jc w:val=\"~a\"/>" justify))))))
    (format nil "<w:p>~:[~*~;<w:pPr>~{~a~}</w:pPr>~]~a</w:p>~%" properties properties runs)))

(defun docx-number-everything (tree)
  "Give each heading of TREE its number, as the PDF numbers them (the annexes
anew, lettered: A, A.1, B), and each captioned table and picture its Table N or Figure N, in
DOCX-NUMBERS, before anything is written, so that a link may name what
comes after it."
  (let ((counters (make-array 9 :initial-element 0)) (annex nil))
    (labels ((walk (node)
               (case (org-node-type node)
                 (headline
                  (unless (org-latex-skipped-p node)
                    (let ((level (min 9 (1+ (- (org-property node :level) (org-latex-min-level *org-latex*))))))
                      (when (and (not annex)
                                 (or (some (lambda (tag) (string-equal tag "appendix")) (org-property node :tags))
                                     (string-equal (org-latex-setting node "annex") "yes")))
                        (setf annex t)
                        (setf (gethash node (docx-numbers *docx*)) :annex)
                        (fill counters 0))
                      (when (org-latex-numbered-p node)
                        (incf (aref counters (1- level)))
                        (fill counters 0 :start level)
                        (setf (gethash node (docx-numbers *docx*))
                              (list (format nil "~{~a~^.~}"
                                            (let ((numbers (coerce (subseq counters 0 level) 'list)))
                                              ;; in the annexes the first number a letter, A, A.1, B, as LaTeX letters them
                                              (if (and annex (<= 1 (first numbers) 26))
                                                  (cons (code-char (+ 64 (first numbers))) (rest numbers))
                                                  numbers)))
                                    (eq (gethash node (docx-numbers *docx*)) :annex))))
                      (mapc #'walk (org-node-children node)))))
                 (table (when (getf (org-property node :affiliated) :caption)
                          (setf (gethash node (docx-numbers *docx*)) (format nil "~a ~d" (export-language-word :table) (incf (docx-tables *docx*))))))
                 (paragraph (when (and (getf (org-property node :affiliated) :caption)
                                       (let ((children (org-node-children node)))
                                         (and (= 1 (length children)) (org-latex-image-link-p (first children)))))
                              (setf (gethash node (docx-numbers *docx*)) (format nil "~a ~d" (export-language-word :figure) (incf (docx-figures *docx*))))))
                 (t (mapc #'walk (org-node-children node))))))
      (walk tree))))

(defun docx-caption (node kind &key (justify "center"))
  "The caption paragraph of NODE, its Table N or Figure N (DOCX-NUMBER-EVERYTHING;
KIND :table or :figure) and its #+CAPTION, NODE's bookmark on it, aligned as
JUSTIFY says (centred, or left over a table at the left margin); NIL when it has none."
  (let ((caption (getf (org-property node :affiliated) :caption)))
    (when caption
      (let ((label (gethash node (docx-numbers *docx*))))
        (docx-paragraph (docx-bookmarked node (concatenate 'string (docx-run (format nil "~a: " label)) (docx-string caption)))
                        :style "Caption" :justify justify :keep-next (eq kind :table))))))

(defun docx-text-paragraph (node out)
  "Write the paragraph NODE to OUT: a picture alone in it centred, or
against the margin its position names, with its caption under it; beside
the paragraph after it when its :EXPORT: drawer says beside; else its text,
set as its align says."
  (let* ((children (org-node-children node))
         (alone (org-latex-picture-alone-p node)))
    (if (not alone)
        (write-string (docx-paragraph (docx-objects node)
                                      :justify (case (export-paragraph-align node)
                                                 (:left "left") (:right "right") (:centre "center") (:justify "both")))
                      out)
        (let* ((attributes (picture-attributes node))
               (beside (org-latex-setting node "beside"))
               (text (and beside (org-latex-paragraph-after node))))
          (if text
              ;; side by side: two cells of a table without lines
              (let* ((half (floor (docx-text-width *docx*) 2))
                     (picture (concatenate 'string (docx-paragraph (docx-picture (first children) attributes :width (- half 200)) :justify "center")
                                           (or (docx-caption node :figure) "")))
                     (words (docx-paragraph (docx-objects text)))
                     (cells (if (string-equal beside "right") (list words picture) (list picture words))))
                (setf (gethash text (org-latex-consumed *org-latex*)) t)
                (format out "<w:tbl><w:tblPr><w:tblW w:w=\"5000\" w:type=\"pct\"/><w:tblLayout w:type=\"fixed\"/></w:tblPr><w:tblGrid><w:gridCol w:w=\"~d\"/><w:gridCol w:w=\"~d\"/></w:tblGrid><w:tr>~{<w:tc><w:tcPr><w:tcW w:w=\"2500\" w:type=\"pct\"/><w:vAlign w:val=\"center\"/></w:tcPr>~a</w:tc>~}</w:tr></w:tbl>~%"
                        half half cells))
              (let ((place (case (export-position node) (:left "left") (:right "right") (t "center"))))
                (write-string (docx-paragraph (docx-picture (first children) attributes) :justify place
                                              :keep-next (getf (org-property node :affiliated) :caption))
                              out)
                (write-string (or (docx-caption node :figure :justify place) "") out)))))))

;;; Headings: numbered as the PDF numbers them

(defvar *docx-contents* '()
  "The headings written, the latest first, for the contents: each (LEVEL NUMBER TITLE BOOKMARK).")

(defun docx-headline (headline out)
  "Write HEADLINE and what is under it to OUT: a heading in the style of its
level, its number before it as the PDF has it (the annexes numbered anew,
from a page of their own), its bookmark on it for links and the contents
\(unless its :EXPORT: drawer says contents: no)."
  (unless (org-latex-skipped-p headline)
    (let* ((level (min 9 (1+ (- (org-property headline :level) (org-latex-min-level *org-latex*)))))
           (given (gethash headline (docx-numbers *docx*)))
           (annex (or (eq given :annex) (and (consp given) (second given))))
           (number (and (consp given) (first given)))
           (title (with-output-to-string (s)
                    (let ((todo (org-property headline :todo-keyword)))
                      (when (and todo (org-latex-option "todo" t))
                        (write-string (docx-run (format nil "~a " todo) (list* :bold t *docx-run*)) s)))
                    (docx-objects-between (org-property headline :title) (org-property headline :title-begin)
                                          (org-property headline :title-end) s))))
      (unless (export-setting-no-p headline "contents")
        (push (list level number (docx-plain-title (org-property headline :raw-value)) (docx-bookmark headline)) *docx-contents*))
      (write-string (docx-paragraph (docx-bookmarked headline (concatenate 'string (if number (docx-run (format nil "~a  " number)) "") title))
                                    :style (format nil "Heading~d" level) :keep-next t :page-break annex)
                    out)
      (docx-with-settings headline out
                          (lambda (s) (dolist (child (org-node-children headline)) (docx-element child s)))))))

;;; Lists

(defun docx-list-kind (node)
  "How the items of the list NODE are marked: :bullet, :none, :decimal,
:decimal-paren, :lower-letter, :upper-letter, :lower-roman or :upper-roman,
as its :EXPORT: drawer's style says, else as its first bullet is written."
  (let ((style (org-latex-setting node "style"))
        (bullet (string-trim " 	" (or (org-property (first (org-node-children node)) :bullet) ""))))
    (cond ((string= (or style "") "numbers") :decimal)
          ((string= (or style "") "letters") :lower-letter)
          ((string= (or style "") "Letters") :upper-letter)
          ((string= (or style "") "roman") :lower-roman)
          ((string= (or style "") "Roman") :upper-roman)
          ((string-equal (or style "") "none") :none)
          ((string-equal (or style "") "bullets") :bullet)
          ((not (eq (org-property node :type) 'ordered)) :bullet)
          ((zerop (length bullet)) :decimal)
          ((lower-case-p (char bullet 0)) :lower-letter)
          ((upper-case-p (char bullet 0)) :upper-letter)
          ((uiop:string-suffix-p bullet ")") :decimal-paren)
          (t :decimal))))

(defun docx-list (node out)
  "Write the plain list NODE to OUT: each item a paragraph marked as the list
is (DOCX-LIST-KIND), a checkbox before its words, a description's term in
bold; what else an item holds set in under its words."
  (let* ((depth (min 8 (count 'plain-list (loop for up = (org-node-parent node) then (org-node-parent up) while up collect (org-node-type up)))))
         (descriptive (eq (org-property node :type) 'descriptive))
         (first-counter (org-property (first (org-node-children node)) :counter))
         (id (unless descriptive
               (let ((id (+ (docx-list-base *docx*) (length (docx-lists *docx*)))))
                 (push (list id (docx-list-kind node) (if (integerp first-counter) first-counter 1) depth) (docx-lists *docx*))
                 id))))
    (dolist (item (org-node-children node))
      (let* ((children (org-node-children item))
             (first (and (eq (org-node-type (first children)) 'paragraph) (first children)))
             (checkbox (case (org-property item :checkbox) (on "☒ ") (off "☐ ") (trans "☐ ")))
             (tag (org-property item :tag))
             (lead (concatenate 'string (if checkbox (docx-run checkbox) "")
                                (if tag (concatenate 'string (let ((*docx-run* (list* :bold t *docx-run*))) (docx-string tag)) (docx-run " ")) ""))))
        (write-string (docx-paragraph (concatenate 'string lead (if first (docx-objects first) ""))
                                      :numbering (and id (cons id depth))
                                      :style (or *docx-paragraph-style* "ListParagraph"))
                      out)
        (let ((*docx-indent* (* 360 (+ depth 2))))
          (dolist (child (if first (rest children) children))
            (docx-element child out)))))))

;;; Tables

(defun docx-table-widths (node columns special standard overhead text-width table-width)
  "The widths of the COLUMNS of the table NODE, in twentieths of a point, and
the scale its type is set in, NIL for its own size: as its :EXPORT: drawer's
widths say; else each as wide as its widest cell and OVERHEAD (its margins
and a little to spare) when together they fit in TEXT-WIDTH, widened to it
when the table is given TABLE-WIDTH; else the columns with words to wrap
sharing what the others leave, as the PDF's do; still too wide, in smaller
type. SPECIAL and STANDARD are as ORG-EXPORT-TABLE-SHAPE found them."
  (multiple-value-bind (lengths widest wraps) (export-table-measure columns special standard)
    (declare (ignore widest))
    (let* ((widths (export-table-widths node columns))
           (characters (mapcar (lambda (l) (reduce #'max l :initial-value 0)) lengths))
           (natural (mapcar (lambda (n) (+ overhead (* (docx-character-width) n))) characters))
           (scale nil))
      (values
       (if widths
           (let* ((given (loop for w in widths
                               for n in natural
                               collect (case w
                                         (:share :share)
                                         (:auto n)
                                         (t (or (docx-length w) n)))))
                  (shares (count :share given))
                  (left-over (max 0 (- text-width (reduce #'+ (remove :share given))))))
             (mapcar (lambda (g) (if (eq g :share) (floor left-over (max 1 shares)) g)) given))
           (let* ((total (reduce #'+ natural))
                  (fixed (loop for w in natural for wrap in wraps unless wrap sum w))
                  (wrapping (- total fixed)))
             (when (and (> total text-width) (or (zerop wrapping) (>= fixed text-width)))
               (setf scale (if (<= (+ (* overhead columns) (* (docx-character-width) 10/11 (reduce #'+ characters))) text-width) 10/11 9/11)
                     natural (mapcar (lambda (n) (+ overhead (round (* (docx-character-width) n scale)))) characters)
                     total (reduce #'+ natural)
                     fixed (loop for w in natural for wrap in wraps unless wrap sum w)
                     wrapping (- total fixed)))
             (cond ((and table-width (<= total text-width))
                    ;; as wide as asked, every column widened as its text is wide
                    (loop with given = 0
                          for (w . more) on natural
                          for share = (if more (floor (* w text-width) total) (- text-width given))
                          do (incf given share)
                          collect share))
                   ((<= total text-width) natural)
                   ((and (plusp wrapping) (< fixed text-width))
                    (loop for w in natural for wrap in wraps
                          collect (if wrap (floor (* w (- text-width fixed)) wrapping) w)))
                   (t (mapcar (lambda (w) (floor (* w text-width) total)) natural)))))
       scale))))

(defun docx-table-borders (lines)
  "The borders of a table printing LINES (TABLE-LINES) as Word's XML, the
outer lines thicker, as the PDF's top and bottom rules are."
  (format nil "<w:tblBorders>~{<w:~a w:val=~:[\"nil\"~;\"single\" w:sz=\"~d\" w:space=\"0\" w:color=\"000000\"~]/>~}</w:tblBorders>"
          (loop for (side line size) in '(("top" :top 12) ("left" :left 6) ("bottom" :bottom 12) ("right" :right 6)
                                          ("insideH" :rows 6) ("insideV" :columns 6))
                for on = (member line lines)
                collect side collect on when on collect size)))

(defun docx-table (node out)
  "Write the table NODE to OUT: its caption above it (Table N), its columns as
wide as their text but no wider together than the page, or as the :EXPORT:
drawer's widths say, or sharing the width it gives the table (full: margin
to margin) as their text does; in the middle of the page, or against the
margin its position names; each aligned as it says or as its numbers are; the
rows above its first rule a header in bold, again at the top of each page
unless the drawer says repeat header: no; the lines its lines setting
prints (TABLE-LINES), as the PDF's: above, under the header, between its
parts and under it as standard; with the template's table style, the
style's unless the drawer gives lines."
  (if (not (eq (org-property node :type) 'org))
      (docx-verbatim (subseq *org-latex-text* (org-node-post-affiliated node) (- (org-node-end node) (org-node-post-blank node))) out)
      (multiple-value-bind (rows special standard cookies columns) (org-export-table-shape node)
        (let* ((header (export-table-header rows))
               (repeat (not (export-setting-no-p node "repeat header")))
               ;; against a margin when asked, or when the template's LaTeX half sets its tables so
               (design (getf (docx-section *docx*) :design))
               (place (let ((position (or (org-latex-setting node "position") (getf design :table-position))))
                        (case (export-side position)
                          (:left "left")
                          (:right "right")
                          (t (if (and (null position) (getf (docx-section *docx*) :tables-left)) "left" "center")))))
               ;; each cell's margins, as the table's style has them, and a little to spare: a character's
               ;; width when the font's own is not known (bold and capitals wider than the 0.6 taken)
               (overhead (+ (if (= *docx-character-ratio* 3/5) (docx-character-width) 10)
                            (* 2 (or (and (getf design :table-style) (getf design :cell-margin)) 80))))
               (table-width (let ((w (org-latex-setting node "width")))
                              (and w (if (string-equal w "full")
                                         (docx-text-width *docx*)
                                         (let ((l (docx-length w))) (and l (plusp l) (min l (docx-text-width *docx*))))))))
               (table-style (getf design :table-style))
               (lines (table-lines node))
               ;; the lines asked for, over the template's table style's
               (own-lines (or (null table-style) (org-latex-setting node "lines")))
               (borders (and own-lines (docx-table-borders lines)))
               (justify (loop for i below columns
                              collect (ecase (export-column-alignment node i special standard cookies)
                                        (:left "left") (:right "right") (:centre "center"))))
               (rule-after (loop for (row next) on rows
                                 for index from 0
                                 when (and next (eq (org-property next :type) 'rule) (not (eq (org-property row :type) 'rule)))
                                   collect index)))
          (multiple-value-bind (twips scale)
              (docx-table-widths node columns special standard overhead (or table-width (docx-text-width *docx*)) table-width)
            (write-string (or (docx-caption node :table :justify place) "") out)
            (if table-style
                ;; the template's own table style: its borders, shading and header row
                (format out "<w:tbl><w:tblPr><w:tblStyle w:val=\"~a\"/><w:tblW w:w=\"~d\" w:type=\"dxa\"/><w:jc w:val=\"~a\"/>~
  ~@[~a~]<w:tblLayout w:type=\"fixed\"/><w:tblLook w:val=\"04A0\" w:firstRow=\"~:[0~;1~]\" w:lastRow=\"0\" w:firstColumn=\"0\" w:lastColumn=\"0\" w:noHBand=\"0\" w:noVBand=\"1\"/></w:tblPr><w:tblGrid>~{<w:gridCol w:w=\"~d\"/>~}</w:tblGrid>~%"
                        (xml-escape table-style) (reduce #'+ twips) place borders header twips)
                (format out "<w:tbl><w:tblPr><w:tblW w:w=\"~d\" w:type=\"dxa\"/><w:jc w:val=\"~a\"/>~
  ~a<w:tblLayout w:type=\"fixed\"/><w:tblCellMar><w:left w:w=\"80\" w:type=\"dxa\"/><w:right w:w=\"80\" w:type=\"dxa\"/></w:tblCellMar></w:tblPr><w:tblGrid>~{<w:gridCol w:w=\"~d\"/>~}</w:tblGrid>~%"
                        (reduce #'+ twips) place borders twips))
            (loop with row-count = (length rows)
                  for row in rows
                  for index from 0
                  unless (eq (org-property row :type) 'rule)
                    do (let* ((in-header (and header (< index header)))
                              (cells (nthcdr (if special 1 0) (org-node-children row)))
                              ;; under the header, or a rule of the table's own, as the lines say
                              (ruled (and own-lines (member index rule-after) (< index (1- row-count))
                                          (if (eql (1+ index) header) (member :header lines) (member :rules lines)))))
                         (format out "<w:tr>~:[~;<w:trPr><w:tblHeader/></w:trPr>~]" (and in-header repeat))
                         (loop for i below columns
                               for cell = (pop cells)
                               do (format out "<w:tc><w:tcPr><w:tcW w:w=\"~d\" w:type=\"dxa\"/>~:[~;<w:tcBorders><w:bottom w:val=\"single\" w:sz=\"6\" w:space=\"0\" w:color=\"000000\"/></w:tcBorders>~]</w:tcPr>~a</w:tc>"
                                          (nth i twips) ruled
                                          (docx-paragraph (if cell (let ((*docx-run* (append (and in-header (not table-style) (list :bold t)) (and scale (list :scale scale)) *docx-run*)))
                                                                     (docx-objects cell))
                                                              "")
                                                          :style "Compact" :justify (nth i justify))))
                         (format out "</w:tr>~%")))
            (format out "</w:tbl>~%")
            ;; air under the table, as the PDF leaves
            (write-string (docx-space 160) out))))))

;;; Blocks

(defun docx-verbatim (value out)
  "Write VALUE, code or an example as it is written, to OUT, a paragraph in the
SourceCode style for each of its lines; nothing for a value all blank."
  (let ((value (string-right-trim '(#\Newline) value)))
    (unless (zerop (length (string-trim '(#\Space #\Tab #\Newline) value)))
      (let* ((lines (uiop:split-string value :separator '(#\Newline)))
             (longest (reduce #'max lines :key #'length))
             (room (- (floor (- (docx-text-width *docx*) (* *docx-column-gap* (1- *docx-columns*))) *docx-columns*)
                      220 *docx-indent*))
             (asked (or (getf *docx-run* :scale) 1))
             ;; lines too long for the column in smaller type, down to 8/11, as the PDF's would fit
             (scale (or (find-if (lambda (s) (<= (* longest (docx-character-width) s) room))
                                 (remove-if (lambda (s) (> s asked)) (list asked 10/11 9/11 8/11)))
                        8/11))
             (*docx-run* (if (= scale 1) *docx-run* (list* :scale scale *docx-run*))))
        (dolist (line lines)
          (write-string (docx-paragraph (docx-run line) :style "SourceCode") out))))))

(defun docx-with-settings (node out writer)
  "Call WRITER with a stream, NODE's XML written to it, and write that to OUT
as NODE's :EXPORT: drawer says of any element: set in by its indent, in the
size of its font, its lines as far apart as its spacing says, in its number
of columns (a section of its own)."
  (let ((indent (docx-length (org-latex-setting node "indent")))
        (spacing (export-line-spacing node))
        (size (cdr (assoc (org-latex-setting node "font") *docx-font-sizes* :test #'equalp)))
        (columns (export-columns node)))
    (let ((*docx-indent* (+ *docx-indent* (or indent 0)))
          (*docx-line-spacing* (or spacing *docx-line-spacing*))
          (*docx-run* (if size (list* :scale size *docx-run*) *docx-run*))
          (*docx-columns* (or columns *docx-columns*)))
      (if columns
          (progn (write-string (docx-section-break 1) out)
                 (funcall writer out)
                 (write-string (docx-section-break columns) out))
          (funcall writer out)))))

(defvar *docx-landscape* nil
  "True while what is written goes on pages turned sideways (a page: landscape setting).")

(defun docx-page-number (page element attribute)
  "The number ATTRIBUTE of the ELEMENT in PAGE, a section's page XML, or NIL."
  (let ((value (xml-value page element attribute)))
    (and value (every #'digit-char-p value) (plusp (length value)) (parse-integer value))))

(defun docx-page-xml (&optional (landscape *docx-landscape*))
  "The page of the document's sections as XML, its size and margins: as the
document or its template has it, or with LANDSCAPE turned sideways, its width
and height swapped (when it is upright) and said to be landscape."
  (let ((page (getf (docx-section *docx*) :page "")))
    (if (not landscape)
        page
        (let ((width (docx-page-number page "w:pgSz" "w:w")) (height (docx-page-number page "w:pgSz" "w:h")))
          (if (and width height)
              (cl-ppcre:regex-replace "<w:pgSz\\s[^>]*?/>" page
                                      (format nil "<w:pgSz w:w=\"~d\" w:h=\"~d\" w:orient=\"landscape\"/>"
                                              (max width height) (min width height)))
              page)))))

(defun docx-page-text-width (landscape)
  "The width the text has on the document's page, turned sideways with
LANDSCAPE, in twentieths of a point; NIL when the page does not say."
  (let* ((page (docx-page-xml landscape))
         (width (docx-page-number page "w:pgSz" "w:w"))
         (left (docx-page-number page "w:pgMar" "w:left"))
         (right (docx-page-number page "w:pgMar" "w:right")))
    (and width left right (- width left right))))

(defun docx-section-break (columns &key (next :continuous))
  "A paragraph ending a section of the page in COLUMNS columns, on pages as
*DOCX-LANDSCAPE* says, begun as the section before it ended (on the same
page, or a new one after pages turned another way); the section after it
begins as NEXT says, :CONTINUOUS or :NEXT-PAGE. The first such section keeps
a first page of its own (its header and footer) when the document's page has
one."
  (let ((first (not (getf (docx-section *docx*) :broken)))
        (start (getf (docx-section *docx*) :start :continuous)))
    (setf (getf (docx-section *docx*) :broken) t
          (getf (docx-section *docx*) :start) next)
    (format nil "<w:p><w:pPr><w:sectPr>~a<w:type w:val=\"~:[nextPage~;continuous~]\"/>~a<w:cols w:num=\"~d\" w:space=\"~d\"/>~:[~;<w:titlePg/>~]</w:sectPr></w:pPr></w:p>~%"
            (getf (docx-section *docx*) :references "") (eq start :continuous) (docx-page-xml) columns *docx-column-gap*
            (and first (getf (docx-section *docx*) :title-page)))))

(defun docx-fold-last-break (body)
  "BODY, a document's text as XML, without the section break that ends it
when one does (nothing after it but blank space), and as a second value that
break's w:sectPr, to be the document's last section rather than an empty one
after it; BODY itself and NIL otherwise."
  (let* ((trimmed (string-right-trim '(#\Space #\Newline #\Tab) body))
         (closing "</w:sectPr></w:pPr></w:p>"))
    (if (alexandria:ends-with-subseq closing trimmed)
        (let ((start (search "<w:p><w:pPr><w:sectPr>" trimmed :from-end t)))
          (if start
              (values (subseq trimmed 0 start)
                      (subseq trimmed (+ start (length "<w:p><w:pPr>")) (- (length trimmed) (length "</w:pPr></w:p>"))))
              (values body nil)))
        (values body nil))))

(defun docx-with-page (node out writer)
  "Call WRITER with OUT, on pages turned sideways when NODE's run of them is
\(ORG-PAGE-RUN): a section of its own, from a page of its own, the text as
wide as the page then gives it, and the pages after upright again."
  (multiple-value-bind (page begins ends) (org-page-run node)
    ;; within pages turned already, as they are
    (if (or (null page) *docx-landscape*)
        (funcall writer out)
        (progn
          (when begins (write-string (docx-section-break *docx-columns* :next :next-page) out))
          (let* ((*docx-landscape* t)
                 (upright (docx-text-width *docx*))
                 (wide (docx-page-text-width t)))
            (unwind-protect
                 (progn (when wide (setf (docx-text-width *docx*) wide))
                        (funcall writer out)
                        ;; after columns, a section of its own on their page balances them, as Word does
                        (when ends (write-string (docx-section-break *docx-columns* :next :next-page) out)))
              (setf (docx-text-width *docx*) upright)))))))

(defun docx-page-break ()
  "A paragraph a line high ending the page, what follows it beginning a new one."
  "<w:p><w:pPr><w:spacing w:before=\"0\" w:after=\"0\" w:line=\"20\" w:lineRule=\"exact\"/></w:pPr><w:r><w:br w:type=\"page\"/></w:r></w:p>")

(defun docx-space (twips)
  "An empty paragraph TWIPS high (twentieths of a point): space between what is before and after it."
  (format nil "<w:p><w:pPr><w:spacing w:before=\"0\" w:after=\"0\" w:line=\"~d\" w:lineRule=\"exact\"/></w:pPr></w:p>~%" (max 1 twips)))

(defun docx-element (node out)
  "Write the element NODE to OUT as Word's XML, in what its :EXPORT: drawer
says, a new page and space before and after it as it says; nothing for one
written already as part of another."
  (unless (gethash node (org-latex-consumed *org-latex*))
    (call-with-export-breaks
     node out
     (lambda (out)
       (docx-with-page node out
                       (lambda (out)
                         (if (eq (org-node-type node) 'headline)
                             (docx-headline node out)
                             (docx-with-settings node out (lambda (s) (docx-element-itself node s)))))))
     :length #'docx-length
     :page (lambda (out) (write-string (docx-page-break) out))
     :space (lambda (out twips) (write-string (docx-space twips) out)))))

(defun docx-elements (node out)
  "Write the elements in NODE to OUT."
  (dolist (child (org-node-children node)) (docx-element child out)))

(defun docx-element-itself (node out)
  "Write the element NODE to OUT as its kind is written."
  (case (org-node-type node)
    ((org-data section dynamic-block) (docx-elements node out))
    (paragraph (docx-text-paragraph node out))
    (plain-list (docx-list node out))
    (table (docx-table node out))
    (src-block (docx-verbatim (org-block-code node) out))
    ((example-block fixed-width) (docx-verbatim (or (org-property node :value) "") out))
    (quote-block (let ((*docx-paragraph-style* "Quote")) (docx-elements node out)))
    (center-block (let ((*docx-paragraph-style* "Centred")) (docx-elements node out)))
    (verse-block (write-string (docx-paragraph (let ((*docx-keep-lines* t)) (docx-objects node)) :style "Verse") out))
    ;; a block of its own kind in a style of that name, when the document has it; a signature
    ;; its lines as written, at the foot of its page, the text going on on the next, as the PDF's
    (special-block (let* ((signature (string-equal (org-property node :type) "signature"))
                          (*docx-paragraph-style* (string-capitalize (org-property node :type)))
                          (*docx-keep-lines* signature)
                          (*docx-at-foot* signature))
                     (docx-elements node out)
                     (when signature (write-string (docx-page-break) out))))
    (horizontal-rule (write-string "<w:p><w:pPr><w:pBdr><w:bottom w:val=\"single\" w:sz=\"6\" w:space=\"1\" w:color=\"000000\"/></w:pBdr></w:pPr></w:p>" out))
    (drawer (let ((drawers (org-latex-option "d" t)))
              (unless (or (null drawers) (member (org-property node :drawer-name) '("LOGBOOK" "EXPORT") :test #'string-equal))
                (docx-elements node out))))
    ;; what is never written: properties, planning, clocks, comments, LaTeX of its own, footnotes' definitions (at their references)
    (t nil)))

;;; The document around the text

(defparameter *docx-namespaces*
  "xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\""
  "The XML namespaces of a Word document's parts.")

(defun docx-contents (depth)
  "The contents of the document: a paragraph for each heading written of a
level up to DEPTH, its number and title linked to it, the page it is on
after a tab (a PAGEREF field, which Word and LibreOffice fill in)."
  (with-output-to-string (out)
    (format out "<w:p><w:pPr><w:pStyle w:val=\"TOCHeading\"/></w:pPr>~a</w:p>~%" (docx-run (export-language-word :contents)))
    (dolist (entry (reverse *docx-contents*))
      (destructuring-bind (level number title bookmark) entry
        (when (<= level depth)
          (format out "<w:p><w:pPr><w:pStyle w:val=\"TOC~d\"/><w:tabs><w:tab w:val=\"right\" w:leader=\"dot\" w:pos=\"~d\"/></w:tabs></w:pPr>~
<w:hyperlink w:anchor=\"~a\">~a<w:r><w:tab/></w:r><w:r><w:fldChar w:fldCharType=\"begin\"/></w:r><w:r><w:instrText xml:space=\"preserve\"> PAGEREF ~a \\h </w:instrText></w:r><w:r><w:fldChar w:fldCharType=\"separate\"/></w:r><w:r><w:t>1</w:t></w:r><w:r><w:fldChar w:fldCharType=\"end\"/></w:r></w:hyperlink></w:p>~%"
                  level (docx-text-width *docx*) bookmark (docx-run (format nil "~@[~a  ~]~a" number title)) bookmark))))))

(defun docx-language ()
  "The language the Word document being written is in, for Word's spelling:
what the note's #+LANGUAGE: says (en, de-CH) when it is a language's code,
else British English (en-GB)."
  (let ((language (string-trim " " (or (and *org-latex* (org-latex-keyword "LANGUAGE")) ""))))
    (if (cl-ppcre:scan "^[A-Za-z]{2,8}(?:-[A-Za-z0-9]{1,8})*$" language) language "en-GB")))

(defun docx-styles (fonts)
  "styles.xml: the paragraph and character styles the document uses, its text
in FONTS, (MAIN SANS MONO) family names or NIL for Word's own, in the note's
language (DOCX-LANGUAGE)."
  (destructuring-bind (main sans mono) fonts
    (flet ((font (family)
             (if family
                 (let ((family (xml-escape family)))
                   (format nil "<w:rFonts w:ascii=\"~a\" w:hAnsi=\"~a\" w:cs=\"~a\" w:eastAsia=\"~a\"/>" family family family family))
                 "")))
      (with-output-to-string (s)
        (docx-xml-part-to s "<w:styles ~a>" *docx-namespaces*)
        (format s "<w:docDefaults><w:rPrDefault><w:rPr>~a<w:sz w:val=\"22\"/><w:szCs w:val=\"22\"/><w:lang w:val=\"~a\"/></w:rPr></w:rPrDefault>~
<w:pPrDefault><w:pPr><w:spacing w:after=\"160\" w:line=\"264\" w:lineRule=\"auto\"/></w:pPr></w:pPrDefault></w:docDefaults>" (font main) (docx-language))
        (flet ((paragraph (id name &key (based "Normal") next ppr rpr)
                 (format s "<w:style w:type=\"paragraph\"~:[~; w:default=\"1\"~] w:styleId=\"~a\"><w:name w:val=\"~a\"/>~@[<w:basedOn w:val=\"~a\"/>~]~@[<w:next w:val=\"~a\"/>~]<w:qFormat/>~@[<w:pPr>~a</w:pPr>~]~@[<w:rPr>~a</w:rPr>~]</w:style>"
                         (string= id "Normal") id name (and (string/= id "Normal") based) next ppr rpr))
               (character-style (id name rpr)
                 (format s "<w:style w:type=\"character\" w:styleId=\"~a\"><w:name w:val=\"~a\"/><w:rPr>~a</w:rPr></w:style>" id name rpr)))
          (paragraph "Normal" "Normal")
          (paragraph "Title" "Title" :next "Normal" :ppr "<w:jc w:val=\"center\"/><w:spacing w:before=\"480\" w:after=\"240\"/>"
                     :rpr (format nil "~a<w:b/><w:sz w:val=\"40\"/><w:szCs w:val=\"40\"/>" (font sans)))
          (paragraph "Subtitle" "Subtitle" :ppr "<w:jc w:val=\"center\"/>" :rpr "<w:sz w:val=\"28\"/>")
          (paragraph "Author" "Author" :ppr "<w:jc w:val=\"center\"/><w:spacing w:after=\"0\"/>")
          (paragraph "Date" "Date" :ppr "<w:jc w:val=\"center\"/><w:spacing w:after=\"360\"/>")
          (loop for level from 1 to 9
                for size in '(32 28 24 22 22 22 22 22 22)
                do (paragraph (format nil "Heading~d" level) (format nil "heading ~d" level) :next "Normal"
                              :ppr (format nil "<w:keepNext/><w:keepLines/><w:spacing w:before=\"~d\" w:after=\"120\"/><w:outlineLvl w:val=\"~d\"/>"
                                           (if (= level 1) 360 240) (1- level))
                              :rpr (format nil "~a<w:b/><w:sz w:val=\"~d\"/><w:szCs w:val=\"~d\"/>" (font sans) size size)))
          (paragraph "TOCHeading" "TOC Heading" :based "Heading1" :next "Normal" :ppr "<w:outlineLvl w:val=\"9\"/>")
          (loop for level from 1 to 9
                do (paragraph (format nil "TOC~d" level) (format nil "toc ~d" level)
                              :ppr (format nil "<w:spacing w:after=\"60\"/><w:ind w:left=\"~d\"/>" (* 220 (1- level)))))
          (paragraph "Caption" "caption" :ppr "<w:spacing w:before=\"120\" w:after=\"120\"/>" :rpr "<w:sz w:val=\"20\"/>")
          (paragraph "Compact" "Compact" :ppr "<w:spacing w:before=\"20\" w:after=\"20\"/>")
          (paragraph "ListParagraph" "List Paragraph" :ppr "<w:spacing w:after=\"60\"/>")
          (paragraph "SourceCode" "Source Code" :ppr "<w:spacing w:after=\"0\" w:line=\"240\" w:lineRule=\"auto\"/><w:ind w:left=\"220\"/>"
                     :rpr (font mono))
          (paragraph "Quote" "Quote" :ppr "<w:ind w:left=\"720\" w:right=\"720\"/>")
          (paragraph "Centred" "Centred" :ppr "<w:jc w:val=\"center\"/>")
          (paragraph "Verse" "Verse" :ppr "<w:ind w:left=\"720\"/>")
          (paragraph "Signature" "Signature" :ppr "<w:keepNext/><w:keepLines/><w:spacing w:before=\"720\" w:after=\"0\"/>")
          (paragraph "FootnoteText" "footnote text" :ppr "<w:spacing w:after=\"0\"/>" :rpr "<w:sz w:val=\"18\"/><w:szCs w:val=\"18\"/>")
          (paragraph "Footer" "footer" :ppr "<w:jc w:val=\"right\"/><w:spacing w:after=\"0\"/>")
          (character-style "VerbatimChar" "Verbatim Char" (font mono))
          (character-style "Hyperlink" "Hyperlink" "<w:color w:val=\"000000\"/>")
          (character-style "FootnoteReference" "footnote reference" "<w:vertAlign w:val=\"superscript\"/>"))
        (format s "</w:styles>")))))

(defun docx-numbering ()
  "numbering.xml: a definition for each list written, its marks of KIND at
the level of its depth, set in as deep, counting from its start; or, where
a template says (DOCX-TEMPLATE-DESIGN), the template's own bullets and
numbers, counting anew from the list's start."
  (with-output-to-string (s)
    (docx-xml-part-to s "<w:numbering ~a>" *docx-namespaces*)
    (let* ((lists (reverse (docx-lists *docx*)))
           (design (getf (docx-section *docx*) :design))
           ;; a template's own bullets and numbers for the note's lists, as it says (its tuatara.txt)
           (theirs (lambda (kind depth)
                     (case kind
                       (:bullet (and (member depth (getf design :bullet-levels)) (getf design :bullets)))
                       ((:decimal :decimal-paren) (and (member depth (getf design :number-levels)) (getf design :numbers)))))))
      (dolist (entry lists)
        (destructuring-bind (id kind start depth) entry
          (unless (funcall theirs kind depth)
            (format s "<w:abstractNum w:abstractNumId=\"~d\"><w:multiLevelType w:val=\"hybridMultilevel\"/>" id)
            (dotimes (level 9)
              (multiple-value-bind (kind-name marks)
                  (ecase kind
                    (:bullet (values "bullet" (if (evenp level) "•" "–")))
                    (:none (values "none" ""))
                    (:decimal (values "decimal" (format nil "%~d." (1+ level))))
                    (:decimal-paren (values "decimal" (format nil "%~d)" (1+ level))))
                    (:lower-letter (values "lowerLetter" (format nil "%~d)" (1+ level))))
                    (:upper-letter (values "upperLetter" (format nil "%~d." (1+ level))))
                    (:lower-roman (values "lowerRoman" (format nil "%~d." (1+ level))))
                    (:upper-roman (values "upperRoman" (format nil "%~d." (1+ level)))))
                (format s "<w:lvl w:ilvl=\"~d\"><w:start w:val=\"~d\"/><w:numFmt w:val=\"~a\"/><w:lvlText w:val=\"~a\"/><w:lvlJc w:val=\"left\"/><w:pPr><w:ind w:left=\"~d\" w:hanging=\"360\"/></w:pPr></w:lvl>"
                        level start kind-name marks (* 360 (+ level 2)))))
            (format s "</w:abstractNum>"))))
      (dolist (entry lists)
        (destructuring-bind (id kind start depth) entry
          (let ((abstract (funcall theirs kind depth)))
            (if abstract
                ;; the template's numbering, counting anew from the list's start at its level
                (format s "<w:num w:numId=\"~d\"><w:abstractNumId w:val=\"~a\"/><w:lvlOverride w:ilvl=\"~d\"><w:startOverride w:val=\"~d\"/></w:lvlOverride></w:num>"
                        id abstract depth start)
                (format s "<w:num w:numId=\"~d\"><w:abstractNumId w:val=\"~d\"/></w:num>" id id))))))
    (format s "</w:numbering>")))

(defun docx-footnotes-part ()
  "footnotes.xml: Word's separators, then each footnote written."
  (with-output-to-string (s)
    (docx-xml-part-to s "<w:footnotes ~a>~
<w:footnote w:type=\"separator\" w:id=\"0\"><w:p><w:r><w:separator/></w:r></w:p></w:footnote>~
<w:footnote w:type=\"continuationSeparator\" w:id=\"1\"><w:p><w:r><w:continuationSeparator/></w:r></w:p></w:footnote>" *docx-namespaces*)
    (dolist (note (reverse (docx-footnotes *docx*)))
      (let ((xml (cdr note)))
        ;; the reference mark before the footnote's own text
        (format s "<w:footnote w:id=\"~d\">~a</w:footnote>" (car note)
                (cl-ppcre:regex-replace "</w:pPr>" xml "</w:pPr><w:r><w:rPr><w:rStyle w:val=\"FootnoteReference\"/></w:rPr><w:footnoteRef/></w:r><w:r><w:t xml:space=\"preserve\"> </w:t></w:r>"))))
    (format s "</w:footnotes>")))

(defun docx-core (title author)
  "docProps/core.xml: the document's TITLE and AUTHOR as plain text, what they
hide left out ([redacted])."
  (docx-xml-part "<cp:coreProperties xmlns:cp=\"http://schemas.openxmlformats.org/package/2006/metadata/core-properties\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><dc:title>~a</dc:title><dc:creator>~a</dc:creator></cp:coreProperties>"
          (xml-escape title) (xml-escape author)))

(defun built-in-docx-template ()
  "Tuatara's own look for Word documents as a Word template's parts, each
(NAME . OCTETS) as DOCX-READ-FOLDER gives a template's: an A4 page, its
number at the foot, the built-in styles (DOCX-STYLES), the note's title,
subtitle, author and date where it gives them, its contents unless #+OPTIONS:
toc:nil, then {{{body}}}. Filled by DOCX-TEMPLATE-PACKAGE as a template of
your own is."
  (flet ((octets (text) (tuatara.text:string-octets text))
         (kept (name style)
           ;; a paragraph of STYLE kept only when the note gives NAME a value
           (format nil "<w:p><w:r><w:t>{{{#~a}}}</w:t></w:r></w:p><w:p><w:pPr><w:pStyle w:val=\"~a\"/></w:pPr><w:r><w:t>{{{~a}}}</w:t></w:r></w:p><w:p><w:r><w:t>{{{/~a}}}</w:t></w:r></w:p>"
                   name style name name)))
    (mapcar (lambda (part) (cons (car part) (octets (cdr part))))
            (list (cons "[Content_Types].xml"
                        (docx-xml-part "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">~
<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/><Default Extension=\"xml\" ContentType=\"application/xml\"/>~
<Default Extension=\"png\" ContentType=\"image/png\"/><Default Extension=\"jpeg\" ContentType=\"image/jpeg\"/>~
<Override PartName=\"/word/document.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml\"/>~
<Override PartName=\"/word/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml\"/>~
<Override PartName=\"/word/numbering.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml\"/>~
<Override PartName=\"/word/settings.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml\"/>~
<Override PartName=\"/word/footnotes.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml\"/>~
<Override PartName=\"/word/footer1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml\"/>~
<Override PartName=\"/docProps/core.xml\" ContentType=\"application/vnd.openxmlformats-package.core-properties+xml\"/></Types>"))
                  (cons "_rels/.rels"
                        (docx-relationships (format nil "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"word/document.xml\"/>~
<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties\" Target=\"docProps/core.xml\"/>")))
                  (cons "word/document.xml"
                        (docx-xml-part "<w:document ~a><w:body>~a~a~a~a<w:p><w:r><w:t>{{{contents}}}</w:t></w:r></w:p><w:p><w:r><w:t>{{{body}}}</w:t></w:r></w:p>~
<w:sectPr><w:footerReference w:type=\"default\" r:id=\"rId5\"/><w:pgSz w:w=\"11906\" w:h=\"16838\"/>~
<w:pgMar w:top=\"1417\" w:right=\"1588\" w:bottom=\"1417\" w:left=\"1588\" w:header=\"709\" w:footer=\"709\" w:gutter=\"0\"/><w:cols w:space=\"720\"/></w:sectPr></w:body></w:document>"
                                       *docx-namespaces*
                                       (kept "title" "Title") (kept "subtitle" "Subtitle") (kept "author" "Author") (kept "date" "Date")))
                  (cons "word/styles.xml"
                        (docx-styles (chosen-fonts)))
                  (cons "word/settings.xml"
                        (docx-xml-part "<w:settings ~a><w:updateFields w:val=\"true\"/><w:defaultTabStop w:val=\"720\"/>~
<w:footnotePr><w:footnote w:id=\"0\"/><w:footnote w:id=\"1\"/></w:footnotePr><w:compat><w:compatSetting w:name=\"compatibilityMode\" w:uri=\"http://schemas.microsoft.com/office/word\" w:val=\"15\"/></w:compat></w:settings>"
                                       *docx-namespaces*))
                  (cons "word/footer1.xml"
                        (docx-xml-part "<w:ftr ~a><w:p><w:pPr><w:pStyle w:val=\"Footer\"/></w:pPr>~
<w:r><w:fldChar w:fldCharType=\"begin\"/></w:r><w:r><w:instrText xml:space=\"preserve\"> PAGE </w:instrText></w:r><w:r><w:fldChar w:fldCharType=\"separate\"/></w:r><w:r><w:t>1</w:t></w:r><w:r><w:fldChar w:fldCharType=\"end\"/></w:r></w:p></w:ftr>"
                                       *docx-namespaces*))
                  (cons "word/_rels/document.xml.rels"
                        (docx-relationships (format nil "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>~
<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering\" Target=\"numbering.xml\"/>~
<Relationship Id=\"rId3\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings\" Target=\"settings.xml\"/>~
<Relationship Id=\"rId4\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes\" Target=\"footnotes.xml\"/>~
<Relationship Id=\"rId5\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer\" Target=\"footer1.xml\"/>")))))))

(defun org-docx-document (text &key file)
  "TEXT, an Org document, written as a Word document, FILE its file or NIL:
read as the enhanced PDF export reads it (its macros expanded, what redact
and protect hide never in it, its :EXPORT: drawers' settings), its title,
author and date first, the contents when #+OPTIONS: toc says so, then its
text. Returns the octets of the .docx; as a second value where
#+EXPORT_FILE_NAME says to write it, a full name ending in .docx, or NIL; as
a third what else to say of it, a list of sentences."
  (let* ((directory (and file (tuatara.io:parent-path file)))
         (*docx-base-size* *docx-base-size*)
         (*docx-character-ratio* *docx-character-ratio*)
         (*org-list-allow-alphabetical* t)
         (*org-latex* (let ((latex (make-org-latex text directory file)))
                        (setf (org-latex-enhanced latex) t)
                        latex))
         (original text)
         (text (let ((expanded (org-latex-expand-macros text)))
                 (org-latex-check-hidden expanded original nil)
                 expanded))
         (tree (org-parse text))
         (*org-latex-text* text)
         (*docx* (make-docx))
         (*docx-contents* '())
         (*docx-run* '())
         (*docx-paragraph-style* nil)
         (*docx-indent* 0))
    (org-latex-take-in text tree directory)
    (let* ((named (org-template-name))
           (folder (and named (docx-template-folder named directory)))
           (own (and folder (docx-read-folder folder)))
           ;; a template of your own, else Tuatara's own look, both filled by DOCX-TEMPLATE-PACKAGE
           (parts (or own (built-in-docx-template))))
      (docx-template-start parts)
      (when own
        ;; its LaTeX half setting tables against the left margin: the Word half's there too
        (let ((latex (ignore-errors (latex-template-for named directory))))
          (when (and latex (cl-ppcre:scan "\\\\setlength\\s*\\{?\\s*\\\\LTleft\\s*\\}?\\s*\\{\\s*0(pt|cm|mm|em)?\\s*\\}" latex))
            (setf (getf (docx-section *docx*) :tables-left) t))))
      (when (and named (null own))
        (export-note "The template ~a has no Word half (~a~a/): Tuatara's own look" named
                     (abbreviated-path (docx-templates-directory)) named))
      (docx-number-everything tree)
      (let ((body (with-output-to-string (s) (docx-element tree s))))
        (when own
          (dolist (note (docx-pair-notes named own directory)) (export-note "~a" note)))
        (values (zip-octets (docx-template-package parts body))
                (let ((tex (org-latex-output-name)))
                  (and tex (replaced-type tex "docx")))
                (reverse (org-latex-notes *org-latex*)))))))
