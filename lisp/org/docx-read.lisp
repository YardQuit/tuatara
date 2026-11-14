;;;; lisp/org/docx-read.lisp --- Word documents read as Org text, by Tuatara itself, safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A Word document (.docx) is read here as text: its zip archive unpacked
;;;; (ZIP-ENTRIES, INFLATE), its XML read by a reader that knows no DTD and
;;;; no entity but XML's five and numbered characters, so that nothing in the
;;;; file is fetched, expanded or run; each part no larger than
;;;; *ZIP-LARGEST-ENTRY*, the elements no deeper than *XML-DEEPEST*. It is
;;;; written as an Org note, with headings by their styles, lists by their
;;;; numbering, tables as Org tables, bold, italic and the rest as Org's
;;;; markup, links as links, footnotes and comments as Org footnotes, tracked
;;;; changes marked where they are (inserted underlined, deleted struck
;;;; through) and listed, hidden text marked [hidden: ...]. A first section
;;;; says what the file is: who made it and last changed it, when, with what
;;;; program and template, and whether it holds macros, embedded files or
;;;; links to places outside it, none of which is opened or run.

(in-package #:tuatara.defaults)

(defparameter *docx-largest-file* (* 256 1024 1024)
  "The most octets a Word document, an Excel workbook or a PowerPoint
presentation read as text may have.")

(defparameter *docx-largest-unpacked* (* 256 1024 1024)
  "The most octets the parts read of a Word document, an Excel workbook or a
PowerPoint presentation may take unpacked, together: past it the document is
refused, as made to exhaust the memory.")

(defun docx-read-directory (directory)
  "Every file under DIRECTORY, a Word template unpacked, as (NAME . OCTETS), NAME
its path from DIRECTORY with /, sorted; backups left out (FILES-UNDER). Signals
a FILE-SYSTEM-ERROR when its files take more than *DOCX-LARGEST-UNPACKED*
octets together."
  (let ((left *docx-largest-unpacked*))
    (sort (loop for (path . relative) in (files-under directory :hidden t)
                for octets = (tuatara.io:file-octets path :limit left)
                do (decf left (length octets))
                collect (cons relative octets))
          #'string< :key #'car)))

(defparameter *docx-largest-xml* (* 32 1024 1024)
  "The most octets one XML part of a document read may take unpacked (a Word
document's text, styles, notes; a workbook's sheet; a slide); a picture is
held only to *ZIP-LARGEST-ENTRY*.")

(define-setting *office-note-size-limit* (* 16 1024 1024)
  "The most characters a note read from an office file may have.
Office files are Word documents, Excel workbooks, PowerPoint presentations and
OpenDocument files. Past the limit the rest of the text is left out, and the
note says so, so that a file whose few octets stand for text copied many times
cannot fill the memory."
  :values (integer 1 *))

(define-setting *office-value-length-limit* 2000
  "The most characters of one value an office file's note shows.
A value is a cell of a sheet or a table, or a formula; a longer one is cut
after a word, with ..."
  :values (integer 1 *))

(defparameter *office-most-listed* 500
  "The most lines a list in an office file's note shows, such as its
paragraphs set by hand or the addresses outside it it links to; the rest are
counted, and the note says how many.")

(defvar *docx-in-change* nil
  "True while the text of a tracked change is read: a change within it is
part of it, not noted again.")

;;; The document's parts

(defun docx-entries-by (entries key)
  "A table of the zip ENTRIES by what KEY, a function of a name, makes of
their names, the first of each."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries table)
      (let ((name (funcall key (getf entry :name))))
        (unless (gethash name table) (setf (gethash name table) entry))))))

(define-record (docx-reading (:constructor make-docx-reading
                                          (octets entries cancel &aux (by-name (docx-entries-by entries #'identity))
                                                  (by-lower-name (docx-entries-by entries #'string-downcase))))
                            (:copier nil))
  "A Word document, an Excel workbook or a PowerPoint presentation being read:
its OCTETS, its zip ENTRIES, BY-NAME and BY-LOWER-NAME, the entries by their
names as written and in small letters (the first of each). The PARTS read so far
by name, as XML trees, and their TEXTS; the RELATIONS of each part read so far \(DOCX-PART-RELATIONS). Of a
Word document, its STYLES (id to name and outline level), NUMBERING (list kinds
by number and level), FOOTNOTES (endnotes as eID) and COMMENTS by id, CHANGES,
the tracked changes met, each (KIND AUTHOR DATE TEXT). UNPACKED, the octets of
its parts unpacked so far, each counted once (the names in UNPACKED-NAMES), held
to *DOCX-LARGEST-UNPACKED*. ELEMENTS, the XML
elements of its parts read so far, held to *XML-MOST-ELEMENTS*. ROOM, the
characters its note may still take (*OFFICE-NOTE-SIZE-LIMIT*). CANCEL, a cons
whose car turns true to stop the reading, or NIL."
  (octets nil :documentation "The octets of the file DOCX-READING reads.")
  (entries nil :documentation "The zip entries of DOCX-READING.")
  (cancel nil :documentation "A cons whose car turns true to stop the reading of DOCX-READING, or NIL.")
  (unpacked 0 :documentation "The octets of its parts that DOCX-READING has unpacked so far, each counted once.")
  (elements 0 :documentation "How many XML elements DOCX-READING has read so far, held to *XML-MOST-ELEMENTS*.")
  (room *office-note-size-limit*
        :documentation "The characters the note of DOCX-READING may still take, from *OFFICE-NOTE-SIZE-LIMIT*.")
  (by-name nil :documentation "The entries of DOCX-READING by their names as written, the first of each.")
  (by-lower-name nil :documentation "The entries of DOCX-READING by their names in small letters, the first of each.")
  (parts (make-hash-table :test 'equal) :documentation "The parts DOCX-READING has read so far, XML trees by name.")
  (texts (make-hash-table :test 'equal) :documentation "The text of the parts DOCX-READING has read so far, by name.")
  (unpacked-names (make-hash-table :test 'equal)
                  :documentation "The names of the parts already counted in the unpacked octets of DOCX-READING.")
  (relations (make-hash-table :test 'equal)
             :documentation "The relations of each part DOCX-READING has read so far, by part name
\(DOCX-PART-RELATIONS).")
  (styles (make-hash-table :test 'equal)
          :documentation "The styles of a Word document being read, an id to a name and outline level.")
  (numbering (make-hash-table :test 'equal)
             :documentation "The list kinds of a Word document being read, by number and level.")
  (footnotes (make-hash-table :test 'equal)
             :documentation "The footnotes of a Word document being read, and its endnotes as eID, by id.")
  (comments (make-hash-table :test 'equal) :documentation "The comments of a Word document being read, by id.")
  (changes '()
           :documentation "The tracked changes met in a Word document being read, each (KIND AUTHOR DATE TEXT), the
latest first.")
  (comment-order '() :documentation "The ids of the comments met in a Word document being read, the latest first."))

(define-condition office-note-full (error) ()
  (:report "The note has as many characters as *OFFICE-NOTE-SIZE-LIMIT* allows")
  (:documentation "Signalled when the note read from an office file would take more than
*OFFICE-NOTE-SIZE-LIMIT* characters."))

(defvar *docx-read-writing* nil
  "True while a Word document is read as a note to write in rather than to
look at: its tracked changes accepted, its hidden text and comments left
out, a text box's text as it is, a picture a link to its file under
*DOCX-READ-MEDIA*.")

(defvar *docx-read-media* nil
  "Where a Word document read to write in has its pictures saved, a directory's
name as its links write it (~ for your home directory), or NIL; the pictures
linked are gathered in *DOCX-READ-PICTURES*, each (TARGET . FILE).")

(defvar *docx-read-pictures* '()
  "The pictures a Word document read to write in links to, each (TARGET . FILE).")

(defvar *docx-reading* nil
  "The DOCX-READING of the Word document, Excel workbook or PowerPoint
presentation being read.")

(defun docx-reading-part (name)
  "The part NAME of the document being read, as an XML tree, or NIL when it
has none; its elements and those of the parts read before it no more than
*XML-MOST-ELEMENTS* together."
  (let ((reading *docx-reading*))
    (multiple-value-bind (tree found) (gethash name (docx-reading-parts reading))
      (if found
          tree
          (setf (gethash name (docx-reading-parts reading))
                (let ((text (docx-reading-text name :keep nil)))
                  (and text
                       (multiple-value-bind (tree elements)
                           (xml-parse text :most (- *xml-most-elements* (docx-reading-elements reading)))
                         (incf (docx-reading-elements reading) elements)
                         tree))))))))

(defun docx-reading-of (path &optional octets cancel)
  "A DOCX-READING of the document at PATH, a zip archive, from its OCTETS when
they are given; the file is read whole (TUATARA.IO:FILE-OCTETS, held to
*DOCX-LARGEST-FILE*) when they are not. CANCEL, a cons, stops the reading
when its car turns true. Signals ZIP-ERROR for one that is no zip archive,
or one locked."
  (let ((octets (or octets (tuatara.io:file-octets path :limit *docx-largest-file*))))
    (make-docx-reading octets (zip-entries octets) cancel)))

(defun office-spend (characters)
  "Take CHARACTERS from the room left for the note of the office file being
read, signalling OFFICE-NOTE-FULL when there is none left; and end the
reading there, TUATARA.RUNTIME:JOB-STOPPED, when it has been asked to stop."
  (let ((reading *docx-reading*))
    (when (car (docx-reading-cancel reading)) (error 'tuatara.runtime:job-stopped))
    (tuatara.runtime:job-check-stop)
    (when (minusp (decf (docx-reading-room reading) characters))
      (error 'office-note-full))))

(defun office-value (text)
  "TEXT, a value an office file holds (a cell's, a formula), no longer than
*OFFICE-VALUE-LENGTH-LIMIT* characters, its length taken from the room left for
the note (OFFICE-SPEND)."
  (let ((text (tuatara.display:shorten-string text *office-value-length-limit*)))
    (office-spend (length text))
    text))

(defun office-written-text (writer)
  "What WRITER, a function of a stream, writes to it, as a string: a section
of an office file's note, or a part of one; cut where the note reached
*OFFICE-NOTE-SIZE-LIMIT* characters (OFFICE-NOTE-FULL), and said there. Each
section that takes room in the note is written through this."
  (let ((out (make-string-output-stream)))
    (handler-case (funcall writer out)
      (office-note-full ()
        (format out
                "~&~%The rest is left out: the note reached ~:d characters (*office-note-size-limit*).~%~%"
                *office-note-size-limit*)))
    (get-output-stream-string out)))

(defun docx-main-tree (path usual root what)
  "The main part of the document being read (DOCX-MAIN-PART, USUAL as
standard) as an XML tree whose root element is ROOT whatever its prefix
\(document, workbook, presentation); an error saying the file at PATH holds
no WHAT (a Word document) otherwise."
  (let ((tree (docx-reading-part (docx-main-part usual))))
    (if (and tree (equal (xml-local-name (xml-root tree)) root))
        tree
        (refuse "~a holds no ~a" (tuatara.io:native-file-name path) what))))

(defun docx-entry (name)
  "The zip entry of the part NAME of the document being read: of that name,
else of that name in any case, as Word finds parts; NIL when it has none."
  (or (gethash name (docx-reading-by-name *docx-reading*))
      (gethash (string-downcase name) (docx-reading-by-lower-name *docx-reading*))))

(defun docx-main-part (&optional (usual "word/document.xml"))
  "The name of the document's main part, the one its program shows: the one
its _rels/.rels names, USUAL as standard (word/document.xml for a Word
document, xl/workbook.xml for an Excel workbook, ppt/presentation.xml for a
PowerPoint presentation)."
  (or (gethash :main (docx-reading-parts *docx-reading*))
      (setf (gethash :main (docx-reading-parts *docx-reading*))
            (or (let ((rels (docx-reading-part "_rels/.rels")))
                  (loop for rel in (and rels (xml-find-all rels "Relationship"))
                        when (uiop:string-suffix-p (or (xml-attribute rel "Type") "") "/officeDocument")
                        return (let ((target (string-left-trim "/" (or (xml-attribute rel "Target") ""))))
                                 (and (docx-entry target) (getf (docx-entry target) :name)))))
                usual))))

(defun docx-target-part (part target)
  "The name of the part TARGET, a relation's target written in PART, stands
for: from the archive's top when it begins with /, else from PART's directory,
each .. going a directory up (never above the top)."
  (let ((pieces (if (and (plusp (length target)) (char= (char target 0) #\/))
                    '()
                    (butlast (uiop:split-string part :separator '(#\/))))))
    (dolist (piece (uiop:split-string target :separator '(#\/)))
      (cond ((member piece '("" ".") :test #'string=))
            ((string= piece "..") (setf pieces (butlast pieces)))
            (t (setf pieces (append pieces (list piece))))))
    (format nil "~{~a~^/~}" pieces)))

(defun docx-part-relations (part)
  "The relations of PART of the document being read, each a plist (:ID :TARGET
:TYPE :OUTSIDE), TARGET the name of the part it leads to (DOCX-TARGET-PART),
or for one leading outside the document (OUTSIDE true) the address as written."
  (let ((known (docx-reading-relations *docx-reading*)))
    (multiple-value-bind (relations found) (gethash part known)
      (if found
          relations
          (setf (gethash part known)
                (let ((rels (docx-reading-part (docx-relations-part part))))
                  (loop for rel in (and rels (xml-find-all rels "Relationship"))
                        for outside = (equal (xml-attribute rel "TargetMode") "External")
                        collect (list :id (xml-attribute rel "Id")
                                      :type (or (xml-attribute rel "Type") "")
                                      :outside outside
                                      :target (if outside
                                                  (or (xml-attribute rel "Target") "")
                                                  (docx-target-part part (or (xml-attribute rel "Target") "")))))))))))

(defun docx-relation-with-id (relations id)
  "The relation among RELATIONS (DOCX-PART-RELATIONS) whose id is ID, or NIL."
  (and id (find id relations :key (lambda (r) (getf r :id)) :test #'equal)))

(defun docx-relation-target (relations id)
  "The part of the document the relation ID among RELATIONS leads to, or NIL
for none or one leading outside the document."
  (let ((relation (docx-relation-with-id relations id)))
    (and relation (not (getf relation :outside)) (getf relation :target))))

(defun docx-related (relations kind)
  "The relation among RELATIONS (DOCX-PART-RELATIONS) of the KIND that ends
its type (/sharedStrings, /notesSlide), to a part of the document, or NIL."
  (find-if (lambda (r) (and (not (getf r :outside)) (uiop:string-suffix-p (getf r :type) kind))) relations))

(defun docx-relations-part (part)
  "The name of the part holding the relations of PART: word/_rels/document.xml.rels for word/document.xml."
  (let ((slash (position #\/ part :from-end t)))
    (format nil "~a_rels/~a.rels" (if slash (subseq part 0 (1+ slash)) "") (if slash (subseq part (1+ slash)) part))))

(defun docx-reading-octets-of (name)
  "The octets of the part NAME of the document being read, unpacked, or NIL
when it has none; an error when they would take the document's parts past
*DOCX-LARGEST-UNPACKED* together, each part counted once however often it is
read."
  (let ((entry (docx-entry name)) (reading *docx-reading*))
    (when entry
      (unless (gethash (getf entry :name) (docx-reading-unpacked-names reading))
        (setf (gethash (getf entry :name) (docx-reading-unpacked-names reading)) t)
        (when (> (incf (docx-reading-unpacked reading) (getf entry :size)) *docx-largest-unpacked*)
          (refuse "The document's parts unpacked take more than ~:d octets together" *docx-largest-unpacked*)))
      (zip-entry-octets (docx-reading-octets reading) entry))))

(defun docx-reading-text (name &key (keep t))
  "The part NAME of the document being read as text (UTF-8, what is not as ?),
or NIL; an error when it is larger than *DOCX-LARGEST-XML*. With KEEP, as
standard, the text is kept, and a part is unpacked once however often its text
is asked for."
  (let ((entry (docx-entry name)) (texts (docx-reading-texts *docx-reading*)))
    (when (and entry (> (getf entry :size) *docx-largest-xml*))
      (refuse "The document's part ~a takes ~:d octets unpacked, more than ~:d" name (getf entry :size)
              *docx-largest-xml*))
    (multiple-value-bind (text found) (gethash name texts)
      (cond (found text)
            (t (let* ((octets (docx-reading-octets-of name))
                      (text (and octets (tuatara.text:lenient-string octets))))
                 (if keep (setf (gethash name texts) text) text)))))))

(defun docx-reading-setup ()
  "Read the document's styles, numbering, footnotes and comments."
  (let ((r *docx-reading*))
    (let ((styles (docx-reading-part "word/styles.xml")))
      (dolist (style (and styles (xml-find-all styles "w:style")))
        (let* ((name (let ((n (xml-child style "w:name"))) (and n (xml-attribute n "w:val"))))
               (outline (let ((o (xml-find-all style "w:outlineLvl")))
                          (and o (whole-number (xml-attribute (first o) "w:val"))))))
          (setf (gethash (xml-attribute style "w:styleId") (docx-reading-styles r)) (list name outline)))))
    (let ((numbering (docx-reading-part "word/numbering.xml")))
      (when numbering
        (let ((abstracts (make-hash-table :test 'equal)))
          (dolist (a (xml-find-all numbering "w:abstractNum"))
            (setf (gethash (xml-attribute a "w:abstractNumId") abstracts)
                  (loop for lvl in (xml-find-all a "w:lvl")
                        collect (cons (whole-number (xml-attribute lvl "w:ilvl"))
                                      (let ((f (xml-child lvl "w:numFmt"))) (and f (xml-attribute f "w:val")))))))
          (dolist (num (xml-find-all numbering "w:num"))
            (let ((abstract (xml-child num "w:abstractNumId")))
              (setf (gethash (xml-attribute num "w:numId") (docx-reading-numbering r))
                    (and abstract (gethash (xml-attribute abstract "w:val") abstracts))))))))
    (loop for (part element kind) in '(("word/footnotes.xml" "w:footnote" :footnote)
                                       ("word/endnotes.xml" "w:endnote" :endnote))
          do (let ((notes (docx-reading-part part)))
               (dolist (note (and notes (xml-find-all notes element)))
                 (unless (xml-attribute note "w:type")
                   (setf (gethash (docx-read-note-label kind (xml-attribute note "w:id"))
                                  (docx-reading-footnotes r)) note)))))
    (let ((comments (docx-reading-part "word/comments.xml")))
      (dolist (comment (and comments (xml-find-all comments "w:comment")))
        (setf (gethash (xml-attribute comment "w:id") (docx-reading-comments r)) comment)))))

;;; Runs and paragraphs as Org text

(defun docx-read-run-look (run)
  "The look of the run RUN that Org's markup can say: a list of :BOLD :ITALIC
:UNDERLINE :STRIKE :HIDDEN :SUPERSCRIPT :SUBSCRIPT."
  (let ((rpr (xml-child run "w:rPr")) (look '()))
    (when rpr
      (flet ((on (name)
               (let ((e (xml-child rpr name)))
                 (and e (not (member (xml-attribute e "w:val") '("0" "false" "none") :test #'equal))))))
        (when (on "w:b") (push :bold look))
        (when (on "w:i") (push :italic look))
        (when (on "w:u") (push :underline look))
        (when (or (on "w:strike") (on "w:dstrike")) (push :strike look))
        (when (on "w:vanish") (push :hidden look))
        (let ((vert (xml-child rpr "w:vertAlign")))
          (cond ((equal (xml-attribute vert "w:val") "superscript") (push :superscript look))
                ((equal (xml-attribute vert "w:val") "subscript") (push :subscript look))))))
    look))

(defun docx-read-marked (text look)
  "TEXT with Org's markup for LOOK around it, when it has letters at its ends;
hidden text as [hidden: TEXT]."
  (let ((trimmed (string-trim " " text)))
    (if (zerop (length trimmed))
        text
        (let* ((lead (subseq text 0 (position-if-not (lambda (c) (char= c #\Space)) text)))
               (tail (subseq text (1+ (position-if-not (lambda (c) (char= c #\Space)) text :from-end t))))
               (inner trimmed))
          (when (member :hidden look)
            (if *docx-read-writing*
                (return-from docx-read-marked (if (and (plusp (length lead)) (plusp (length tail))) " " ""))
                (setf inner (format nil "[hidden: ~a]" inner))))
          (cond ((member :superscript look) (setf inner (format nil "^{~a}" inner)))
                ((member :subscript look) (setf inner (format nil "_{~a}" inner))))
          ;; a mark is kept from text beginning or ending with its character, where Org would end it
          (loop for (key mark) in '((:bold "*") (:italic "/") (:underline "_") (:strike "+"))
                when (and (member key look)
                          (char/= (char mark 0) (char inner 0))
                          (char/= (char mark 0) (char inner (1- (length inner)))))
                do (setf inner (concatenate 'string mark inner mark)))
          (concatenate 'string lead inner tail)))))

(defun docx-read-label (id)
  "ID, a note's or a comment's id in a Word document, as an Org footnote's
label may have it: its letters, digits, - and _ only, never markup of its own."
  (cl-ppcre:regex-replace-all "[^A-Za-z0-9_-]" (or id "") "_"))

(defun docx-read-note-label (kind id)
  "The label of the Org footnote standing for the note or comment ID of a Word
document, KIND :FOOTNOTE, :ENDNOTE or :COMMENT (DOCX-READ-LABEL): an endnote's
e and its id (e1), a comment's c and its id (c1), a footnote's its id (1),
with f before it when that begins with c, e or f. No two kinds share a label."
  (let ((label (docx-read-label id)))
    (ecase kind
      (:footnote (if (and (plusp (length label)) (find (char-downcase (char label 0)) "cef"))
                     (concatenate 'string "f" label)
                     label))
      (:endnote (concatenate 'string "e" label))
      (:comment (concatenate 'string "c" label)))))

(defun docx-read-math (node)
  "The equation NODE (Office Math) as a line of text: x^{2}, x_{i}, (a)/(b), √(x)."
  (labels ((text (n)
             (if (stringp n)
                 ""
                 (let ((name (xml-name n)))
                   (flet ((part (child-name) (let ((c (xml-child n child-name))) (if c (text c) ""))))
                     (cond ((string= name "m:t") (org-escaped-words (xml-all-text n)))
                           ((string= name "m:sSup") (format nil "~a^{~a}" (part "m:e") (part "m:sup")))
                           ((string= name "m:sSub") (format nil "~a_{~a}" (part "m:e") (part "m:sub")))
                           ((string= name "m:sSubSup")
                            (format nil "~a_{~a}^{~a}" (part "m:e") (part "m:sub") (part "m:sup")))
                           ((string= name "m:f") (format nil "(~a)/(~a)" (part "m:num") (part "m:den")))
                           ((string= name "m:rad") (format nil "√(~a)" (part "m:e")))
                           ((member name
                                    '("m:rPr" "m:ctrlPr" "m:fPr" "m:radPr" "m:sSupPr" "m:sSubPr" "w:rPr")
                                    :test #'string=) "")
                           (t (format nil "~{~a~}" (mapcar #'text (xml-children n))))))))))
    (text node)))

(defun docx-read-run-part (part out)
  "Write PART, what a run holds (its text, a tab, a break, a note's
reference, a picture, a text box), to OUT as Org text."
  (when (consp part)
    (let ((pn (xml-name part)))
      (cond ((member pn '("w:t" "w:delText") :test #'string=)
             (write-string (org-escaped-words (xml-all-text part)) out))
            ((string= pn "w:tab") (write-char #\Tab out))
            ((member pn '("w:br" "w:cr") :test #'string=)
             (if (equal (xml-attribute part "w:type") "page") (write-string " " out) (format out "\\\\~%")))
            ((string= pn "w:noBreakHyphen") (write-char #\- out))
            ((string= pn "w:footnoteReference")
             (format out "[fn:~a]" (docx-read-note-label :footnote (xml-attribute part "w:id"))))
            ((string= pn "w:endnoteReference")
             (format out "[fn:~a]" (docx-read-note-label :endnote (xml-attribute part "w:id"))))
            ((and *docx-read-writing* (string= pn "w:commentReference")))
            ((string= pn "w:commentReference")
             (let ((id (xml-attribute part "w:id")))
               (pushnew id (docx-reading-comment-order *docx-reading*) :test #'equal)
               (format out "[fn:~a]" (docx-read-note-label :comment id))))
            ;; Word's newer form and an older one for other programs: the newer read, never both
            ((string= pn "mc:AlternateContent")
             (let ((choice (or (xml-child part "mc:Choice") (xml-child part "mc:Fallback"))))
               (when choice (dolist (c (xml-children choice)) (docx-read-run-part c out)))))
            ((member pn '("w:drawing" "w:pict") :test #'string=)
             (let ((box (first (xml-find-all part "w:txbxContent"))))
               (if box
                   (format out (if *docx-read-writing* "~a" "[text box: ~a]")
                           (string-trim " "
                                        (org-safe-line (with-output-to-string (b) (docx-read-block box b)) :escaped t)))
                   (let* ((blip (first (xml-find-all part "a:blip")))
                          (target (and blip
                                       (docx-relation-target (docx-part-relations (docx-main-part))
                                                             (xml-attribute blip "r:embed"))))
                          (doc (first (xml-find-all part "wp:docPr")))
                          (alt (and doc (or (xml-attribute doc "descr") (xml-attribute doc "title")))))
                     (if (and *docx-read-writing* *docx-read-media* target
                              (safe-part-name-p (tuatara.io:native-file-name target) :link t))
                         (let ((file (import-picture-file target
                                                          *docx-read-media*
                                                          (tuatara.io:native-file-name target)
                                                          *docx-read-pictures*)))
                           (pushnew (cons target file) *docx-read-pictures* :test #'equal)
                           (format out "[[file:~a]]" file))
                         (format out "[picture~@[ ~a~]~@[: ~a~]]" (and target (org-safe-line target))
                                 (and alt (plusp (length alt)) (org-safe-line alt))))))))
            ((string= pn "w:object") (write-string "[an embedded object]" out))))))

(defun docx-read-inline (node out &key change)
  "Write the runs, links, changes and marks in NODE (a paragraph or what it
holds) to OUT as Org text; CHANGE, :INSERTED or :DELETED, while within a
tracked change."
  (dolist (child (xml-children node))
    (when (consp child)
      (let ((name (xml-name child)))
        (cond
          ((string= name "w:r")
           (let ((look (docx-read-run-look child))
                 (text (with-output-to-string (s)
                         (dolist (part (xml-children child))
                           (docx-read-run-part part s)))))
             (write-string (docx-read-marked text (append (case change (:inserted '(:underline)) (:deleted '(:strike)))
                                                          look))
                           out)))
          ((string= name "w:hyperlink")
           ;; a link within the document (its contents, a cross-reference) as its words
           (let* ((words (with-output-to-string (s) (docx-read-inline child s :change change)))
                  (target (getf (docx-relation-with-id (docx-part-relations (docx-main-part))
                                                       (xml-attribute child "r:id")) :target)))
             (if (and target (plusp (length words)))
                 (write-string (org-safe-link target words) out)
                 (write-string words out))))
          ((member name '("w:ins" "w:del" "w:moveFrom" "w:moveTo") :test #'string=)
           (let ((kind (if (member name '("w:ins" "w:moveTo") :test #'string=) :inserted :deleted)))
             ;; the outermost change noted with all its text; one within it is part of it, so
             ;; that changes nested deep never copy the same text again at each level
             (unless *docx-in-change*
               (push (list kind (xml-attribute child "w:author") (xml-attribute child "w:date") (xml-all-text child))
                     (docx-reading-changes *docx-reading*)))
             (let ((*docx-in-change* t))
               (cond ((not *docx-read-writing*) (docx-read-inline child out :change kind))
                     ;; to write in: what the changes made, accepted
                     ((eq kind :inserted) (docx-read-inline child out))))))
          ((member name '("m:oMath" "m:oMathPara") :test #'string=)
           (format out (if *docx-read-writing* "~a" "[equation: ~a]") (docx-read-math child)))
          ((string= name "mc:AlternateContent")
           (let ((choice (or (xml-child child "mc:Choice") (xml-child child "mc:Fallback"))))
             (when choice (docx-read-inline choice out :change change))))
          ((member name
                   '("w:smartTag" "w:sdt" "w:sdtContent" "w:customXml" "w:fldSimple" "w:bdo" "w:dir")
                   :test #'string=)
           (docx-read-inline child out :change change)))))))

(defun docx-read-heading-level (style own)
  "The heading level of a paragraph of the style STYLE (its id, or NIL) whose
own outline level is OWN (or NIL): 1 for a first-level heading, by its own
level, else its style's, else a style named heading N; NIL for no heading,
a level below 0 or past 8 none."
  (let* ((known (and style (gethash style (docx-reading-styles *docx-reading*))))
         (outline (or own
                      (second known)
                      (let ((name (or (first known) style "")))
                        (multiple-value-bind (m g) (cl-ppcre:scan-to-strings "(?i)^heading\\s*(\\d)$" name)
                          (and m (1- (parse-integer (aref g 0)))))))))
    (and outline (<= 0 outline 8) (1+ outline))))

(defun docx-read-paragraph-kind (paragraph)
  "What PARAGRAPH is: (:HEADING LEVEL), (:ITEM LEVEL ORDERED), or NIL for text."
  (let* ((ppr (xml-child paragraph "w:pPr"))
         (style (let ((s (and ppr (xml-child ppr "w:pStyle")))) (and s (xml-attribute s "w:val"))))
         (known (and style (gethash style (docx-reading-styles *docx-reading*))))
         (heading (docx-read-heading-level
                   style (let ((o (and ppr (xml-child ppr "w:outlineLvl"))))
                           (and o (whole-number (xml-attribute o "w:val"))))))
         (numpr (and ppr (xml-child ppr "w:numPr"))))
    (cond (heading (list :heading heading))
          ((and style (cl-ppcre:scan "(?i)^title$" (or (first known) style))) (list :title))
          (numpr (let* ((id (let ((e (xml-child numpr "w:numId"))) (and e (xml-attribute e "w:val"))))
                        (level (max 0 (min 8 (or (let ((e (xml-child numpr "w:ilvl")))
                                                   (and e (whole-number (xml-attribute e "w:val"))))
                                                 0))))
                        (format (cdr (assoc level (gethash id (docx-reading-numbering *docx-reading*))))))
                   (and id (not (equal id "0"))
                        (list :item level (and format (not (member format '("bullet" "none") :test #'string=))))))))))

(defun docx-read-table (table out)
  "Write the table TABLE to OUT as an Org table, a rule under its first row."
  (org-write-table-rows
   (loop for row in (remove-if-not (lambda (n) (equal (xml-name n) "w:tr")) (xml-children table))
         collect (mapcan (lambda (cell)
                           (let ((text (format nil "~{~a~^ ~}"
                                               (mapcar (lambda (p)
                                                         (string-trim " " (with-output-to-string (s)
                                                                            (docx-read-inline p s))))
                                                       (xml-find-all cell "w:p")))))
                             ;; a cell spanning columns, empty ones after it, so that the columns line up;
                             ;; each cell takes room in the note, an empty one too
                             (let ((span (max 1 (min 63 (or (whole-number
                                                             (xml-attribute
                                                              (first (xml-find-all (xml-child cell "w:tcPr")
                                                                                   "w:gridSpan"))
                                                              "w:val"))
                                                            1)))))
                               (office-spend (+ (length text) span))
                               (cons text (make-list (1- span) :initial-element "")))))
                         (remove-if-not (lambda (n) (equal (xml-name n) "w:tc")) (xml-children row))))
   out))

(defun docx-read-block (node out)
  "Write the paragraphs and tables of NODE (a body, a cell, a note) to OUT as Org text."
  (let ((numbers (make-hash-table :test 'equal)) (in-list nil) (captioned nil))
    (dolist (child (xml-children node))
      (when (consp child)
        (let ((name (xml-name child)))
          (cond
            ;; to write in, a caption before a table its #+caption:
            ((and *docx-read-writing* (string= name "w:p")
                  (let* ((ppr (xml-child child "w:pPr")) (style (and ppr (xml-child ppr "w:pStyle"))))
                    (and style (string-equal (docx-read-style-name (xml-attribute style "w:val")) "caption")))
                  (let ((next (find-if #'consp (rest (member child (xml-children node))))))
                    (equal (xml-name next) "w:tbl")))
             (format out "#+caption: ~a~%"
                     (string-trim " " (org-safe-line (with-output-to-string (s) (docx-read-inline child s))
                                                     :escaped t)))
             (setf captioned t))
            ((string= name "w:p")
             (let* ((kind (docx-read-paragraph-kind child))
                    (text (string-right-trim '(#\Space #\Tab) (with-output-to-string (s) (docx-read-inline child s))))
                    ;; each line Org would read as structure set in by a space, after a break too
                    (text (if (eq (first kind) :heading) text (org-guarded-lines text))))
               (office-spend (length text))
               ;; a list ended by a blank line, so that what follows is not taken into its last item
               (when (and in-list (not (eq (first kind) :item)) (or kind (plusp (length text))))
                 (terpri out)
                 (setf in-list nil))
               (case (first kind)
                 (:heading (format out "~a ~a~%~%" (make-string (second kind) :initial-element #\*)
                                   (org-safe-line text :escaped t)))
                 (:title (format out "#+title: ~a~%~%" (org-safe-line text :escaped t)))
                 (:item (setf in-list t)
                        (let ((level (second kind)))
                          (format out "~a~a ~a~%" (spaces (* 2 (max 0 (min 8 level))))
                                  (if (third kind) (format nil "~d." (incf (gethash level numbers 0))) "-")
                                  text)))
                 (t (clrhash numbers)
                    (if (zerop (length text)) nil (format out "~a~%~%" text))))))
            ((string= name "w:tbl")
             (clrhash numbers) (setf in-list nil)
             (unless captioned (terpri out))
             (setf captioned nil)
             (docx-read-table child out))
            ((member name '("w:sdt" "w:sdtContent" "w:customXml") :test #'string=) (docx-read-block child out))))))))

;;; Its look: pages, fonts, styles, what is set by hand

(defun docx-twips-cm (twips)
  "TWIPS, twentieths of a point, in centimetres, as words: 2.54."
  (format nil "~,2f" (/ twips 567.0)))

(defun points-text (points)
  "POINTS, a number, as LaTeX and words take it: 12 or 13.5, never 27/2."
  (if (integerp points)
      (format nil "~d" points)
      (string-right-trim "." (string-right-trim "0" (format nil "~,2f" (float points))))))

(defun docx-twips-pt (twips)
  "TWIPS, twentieths of a point, in points, as words: 12 or 13.5."
  (points-text (/ twips 20)))

(defun docx-read-number (element attribute)
  "ELEMENT's ATTRIBUTE as an integer, or NIL."
  (and element (whole-number (xml-attribute element attribute))))

(defun docx-read-theme-fonts ()
  "The document's theme's fonts: its headings' and its text's, two values, or NIL."
  (let ((theme (or (docx-reading-part "word/theme/theme1.xml")
                   (let ((related (docx-related (docx-part-relations (docx-main-part)) "/theme")))
                     (and related (docx-reading-part (getf related :target)))))))
    (flet ((latin (name) (let* ((font (and theme (first (xml-find-all theme name))))
                                (latin (and font (xml-child font "a:latin"))))
                           (and latin (xml-attribute latin "typeface")))))
      (values (latin "a:majorFont") (latin "a:minorFont")))))

(defun docx-read-properties (ppr rpr)
  "What the paragraph properties PPR and run properties RPR say of a look, a
plist of those they set: :FONT (a name, or (:THEME :MAJOR) or (:THEME :MINOR)),
:SIZE (half points), :BOLD, :ITALIC, :CAPS, :COLOUR (RRGGBB), :BEFORE, :AFTER, :LINE and
:LINE-RULE, :LEFT, :FIRST, :HANGING, :ALIGN."
  (let ((look '()))
    (flet ((put (key value) (setf (getf look key) value))
           (on (element)
             (and element (not (member (xml-attribute element "w:val") '("0" "false" "off") :test #'equal)))))
      (when rpr
        (let ((fonts (xml-child rpr "w:rFonts")))
          (when fonts
            (let ((name (xml-attribute fonts "w:ascii")) (theme (xml-attribute fonts "w:asciiTheme")))
              (cond (name (put :font name))
                    (theme (put :font (list :theme (if (search "major" theme) :major :minor))))))))
        (let ((size (docx-read-number (xml-child rpr "w:sz") "w:val"))) (when size (put :size size)))
        (let ((colour (xml-attribute (xml-child rpr "w:color") "w:val")))
          (when (and colour (cl-ppcre:scan "^[0-9A-Fa-f]{6}$" colour)) (put :colour colour)))
        (loop for (name key) in '(("w:b" :bold) ("w:i" :italic) ("w:caps" :caps) ("w:smallCaps" :caps))
              for element = (xml-child rpr name)
              when element do (put key (on element))))
      (when ppr
        (let ((spacing (xml-child ppr "w:spacing")))
          (when spacing
            (loop for (attribute key) in '(("w:before" :before) ("w:after" :after) ("w:line" :line))
                  for value = (docx-read-number spacing attribute)
                  when value do (put key value))
            (when (xml-attribute spacing "w:lineRule") (put :line-rule (xml-attribute spacing "w:lineRule")))))
        (let ((indent (xml-child ppr "w:ind")))
          (when indent
            (loop for (attributes key) in '((("w:left" "w:start") :left)
                                            (("w:firstLine") :first)
                                            (("w:hanging") :hanging))
                  for value = (some (lambda (a) (docx-read-number indent a)) attributes)
                  when value do (put key value))))
        (let ((align (xml-child ppr "w:jc"))) (when align (put :align (xml-attribute align "w:val"))))))
    look))

(defun docx-read-merge (under over)
  "The look OVER on top of UNDER, both plists as DOCX-READ-PROPERTIES gives."
  (let ((merged (copy-list under)))
    (loop for (key value) on over by #'cddr do (setf (getf merged key) value))
    merged))

(defun docx-read-style-nodes ()
  "The document's styles, a table of their elements by id, made once."
  (or (gethash :style-nodes (docx-reading-parts *docx-reading*))
      (setf (gethash :style-nodes (docx-reading-parts *docx-reading*))
            (let ((table (make-hash-table :test 'equal)) (styles (docx-reading-part "word/styles.xml")))
              (dolist (style (and styles (xml-find-all styles "w:style")) table)
                (setf (gethash (xml-attribute style "w:styleId") table) style))))))

(defun docx-read-defaults ()
  "The look of text before any style, the document's defaults."
  (let* ((styles (docx-reading-part "word/styles.xml"))
         (defaults (and styles (first (xml-find-all styles "w:docDefaults"))))
         (rpr (and defaults (first (xml-find-all defaults "w:rPr"))))
         (ppr (and defaults (first (xml-find-all defaults "w:pPr")))))
    (docx-read-merge '(:size 20) (docx-read-properties ppr rpr))))

(defun docx-read-style-look (id &optional (depth 0))
  "The look of the style ID, through the styles it is based on, without the defaults."
  (let ((style (gethash id (docx-read-style-nodes))))
    (if (or (null style) (> depth 20))
        '()
        (let ((based (xml-child style "w:basedOn")))
          (docx-read-merge (if based (docx-read-style-look (xml-attribute based "w:val") (1+ depth)) '())
                           (docx-read-properties (xml-child style "w:pPr") (xml-child style "w:rPr")))))))

(defun docx-read-default-style ()
  "The id of the style of paragraphs that name none (Normal)."
  (loop for id being the hash-keys of (docx-read-style-nodes) using (hash-value style)
        when (and (equal (xml-attribute style "w:type") "paragraph") (equal (xml-attribute style "w:default") "1"))
        return id))

(defun docx-read-style-name (id)
  "The name the style ID is shown by."
  (or (first (gethash id (docx-reading-styles *docx-reading*))) id))

(defun docx-read-font-name (font major minor)
  "FONT as DOCX-READ-PROPERTIES gives it, a name: a theme's font by its name, and which it is."
  (cond ((stringp font) font)
        ((equal font '(:theme :major)) (format nil "~a (the theme's headings font)" (or major "unknown")))
        ((equal font '(:theme :minor)) (format nil "~a (the theme's text font)" (or minor "unknown")))
        (t "unknown")))

(defun docx-read-lines-words (look)
  "LOOK's line spacing as words: single, 1.15 lines, exactly 14 pt, at least 12 pt."
  (let ((line (getf look :line)) (rule (getf look :line-rule)))
    (cond ((null line) "single")
          ((equal rule "exact") (format nil "exactly ~a pt" (docx-twips-pt line)))
          ((equal rule "atLeast") (format nil "at least ~a pt" (docx-twips-pt line)))
          ((= line 240) "single")
          (t (format nil "~a lines" (points-text (/ line 240)))))))

(defun docx-read-indent-words (look)
  "LOOK's indentation as words, or an empty string."
  (let ((left (getf look :left)) (first (getf look :first)) (hanging (getf look :hanging)))
    (format nil "~:[~;~:*~a cm~]~:[~;~:*~@[, ~]first line ~a cm~]~:[~;~:*, hanging ~a cm~]"
            (and left (plusp left) (docx-twips-cm left))
            (and first (plusp first) (docx-twips-cm first))
            (and hanging (plusp hanging) (docx-twips-cm hanging)))))

(defun docx-read-align-words (align)
  "Word's alignment ALIGN in words: left, centred, right, justified."
  (cond ((null align) "left")
        ((member align '("both" "distribute") :test #'string=) "justified")
        ((string= align "center") "centred")
        ((member align '("start") :test #'string=) "left")
        ((member align '("end") :test #'string=) "right")
        (t align)))

(defparameter *docx-read-most-stand-ins* 16
  "The most fonts a document uses that are not installed whose stand-in, the
font shown in their place, the note of a Word document names.")

(defun docx-read-font-here (family installed stand-ins)
  "Whether FAMILY is installed here, as words: installed, or shown in what
stands in for it; INSTALLED, the families installed (INSTALLED-FONT-FAMILIES
of every kind), or NIL when fontconfig is not here. STAND-INS, a cons whose
car counts the stand-ins asked for so far: past *DOCX-READ-MOST-STAND-INS*
a missing font is said to be missing alone."
  (cond ((null installed) "not known (no fontconfig)")
        ((gethash family installed) "installed")
        ((>= (car stand-ins) *docx-read-most-stand-ins*) "not installed")
        (t (incf (car stand-ins))
           (let* ((found (fontconfig-match family "%{family}|%{familylang}"))
                  (bar (and found (position #\| found)))
                  (shown (and bar (own-family (subseq found 0 bar)
                                              (string-trim '(#\Newline) (subseq found (1+ bar)))))))
             (if (and shown (plusp (length shown)))
                 (format nil "not installed: shown in ~a" shown)
                 "not installed")))))

(defun docx-read-font-alternatives ()
  "A hash table from the name of each font in the document's table of fonts
to the font it names to stand in where it is missing."
  (let ((table (docx-reading-part "word/fontTable.xml")) (alternatives (make-hash-table :test 'equal)))
    (dolist (font (and table (xml-find-all table "w:font")) alternatives)
      (let ((alt (xml-child font "w:altName")))
        (when alt (setf (gethash (xml-attribute font "w:name") alternatives) (xml-attribute alt "w:val")))))))

(defun docx-read-first-words (node)
  "The first words of the text of NODE, at most 40 characters and an ellipsis."
  ;; the start of the text alone: a paragraph's nested ones never walked whole again
  (let ((text (string-trim " " (xml-all-text node 200))))
    (if (> (length text) 40) (format nil "~a..." (subseq text 0 40)) text)))

(defun docx-read-pages (s body)
  "Write to stream S the table of the pages of each section of BODY, a
document's body: its paper, margins, columns and headers and footers."
  (format s "** Pages~%| Section | Paper | Margins: top, bottom, left, right | Columns | Headers, footers |~%|-~%")
  (loop for section in (and body (xml-find-all body "w:sectPr"))
        for n from 1
        do (let* ((size (xml-child section "w:pgSz"))
                  (margins (xml-child section "w:pgMar"))
                  (cols (xml-child section "w:cols"))
                  (w (docx-read-number size "w:w")) (h (docx-read-number size "w:h"))
                  (paper (and w h (let ((short (min w h)) (long (max w h)))
                                    (loop for (name sw sh) in '(("A4" 11906 16838)
                                                                ("Letter" 12240 15840)
                                                                ("Legal" 12240 20160)
                                                                ("A3" 16838 23811)
                                                                ("A5" 8391 11906))
                                          when (and (< (abs (- short sw)) 30) (< (abs (- long sh)) 30)) return name))))
                  (columns (or (docx-read-number cols "w:num") 1)))
             (format s "| ~d | ~:[~a × ~a cm~*~;~*~*~a~], ~:[portrait~;landscape~] | ~{~a~^, ~} cm | ~d~:[~*~;, ~a ~
cm apart~] | ~a |~%"
                     n paper (and w (docx-twips-cm w)) (and h (docx-twips-cm h)) paper (and w h (> w h))
                     (mapcar (lambda (a) (let ((v (docx-read-number margins a))) (if v (docx-twips-cm v) "?")))
                             '("w:top" "w:bottom" "w:left" "w:right"))
                     columns (> columns 1)
                     (let ((space (docx-read-number cols "w:space"))) (if space (docx-twips-cm space) "?"))
                     (format nil "~d~:[~;, the first page its own~]"
                             (+ (length (xml-find-all section "w:headerReference"))
                                (length (xml-find-all section "w:footerReference")))
                             (xml-child section "w:titlePg"))))))

(defun docx-read-by-hand-words (look)
  "What LOOK, a paragraph's properties set by hand, sets, as a list of words:
spacing before and after, lines, indentation and alignment."
  (remove nil (list (and (getf look :before) (format nil "~a pt before" (docx-twips-pt (getf look :before))))
                    (and (getf look :after) (format nil "~a pt after" (docx-twips-pt (getf look :after))))
                    (and (getf look :line) (format nil "lines ~a" (docx-read-lines-words look)))
                    (let ((indent (docx-read-indent-words look)))
                      (and (plusp (length indent)) (format nil "indented ~a" indent)))
                    (and (getf look :align) (docx-read-align-words (getf look :align))))))

(defun docx-read-paragraph-styles (paragraphs normal)
  "What PARAGRAPHS, those of a Word document's body, say of its styles, as
four values: a table of how many paragraphs use each style, by its id (NORMAL,
the default style's, for those naming none); the styles in the order first
used; the paragraphs set by hand, each (FIRST-WORDS . PROPERTIES), at most
*OFFICE-MOST-LISTED* of them, the last first; and how many there are."
  (let ((used (make-hash-table :test 'equal)) (order '()) (by-hand '()) (count 0))
    (dolist (paragraph paragraphs)
      (let* ((ppr (xml-child paragraph "w:pPr"))
             (style (or (let ((s (and ppr (xml-child ppr "w:pStyle")))) (and s (xml-attribute s "w:val"))) normal
                        "Normal"))
             (direct (docx-read-properties (and ppr (remove "w:rPr" ppr :key #'xml-name :test #'equal)) nil)))
        (unless (gethash style used) (push style order))
        (incf (gethash style used 0))
        (office-spend 0)
        (when direct
          (let ((words (docx-read-first-words paragraph)))
            (when (plusp (length words))
              (incf count)
              (when (<= count *office-most-listed*)
                (push (cons words direct) by-hand)))))))
    (values used (nreverse order) by-hand count)))

(defun docx-read-run-fonts (body major minor use-font)
  "Read the runs of BODY, a Word document's body, each once wherever it is
nested: USE-FONT, a function of a font and where it is used, is called for each
font a run's character style or the run itself sets (MAJOR and MINOR the
theme's fonts). Returns a table of the runs whose font or size is set by hand,
by those words, each (COUNT FIRST-WORDS)."
  (let ((by-hand (make-hash-table :test 'equal)))
    (dolist (run (and body (xml-find-all body "w:r")) by-hand)
      (let* ((rpr (xml-child run "w:rPr"))
             (character (let ((s (and rpr (xml-child rpr "w:rStyle")))) (and s (xml-attribute s "w:val"))))
             (own (docx-read-properties nil rpr)))
        (when character
          (let ((font (getf (docx-read-style-look character) :font)))
            (when font (funcall use-font font (format nil "~a (character style)" (docx-read-style-name character))))))
        (when (or (getf own :font) (getf own :size))
          (let ((key (format nil
                             "~@[~a~]~@[, ~a pt~]"
                             (and (getf own :font) (docx-read-font-name (getf own :font) major minor))
                             (and (getf own :size) (docx-twips-pt (* 10 (getf own :size)))))))
            (when (getf own :font) (funcall use-font (getf own :font) "set by hand"))
            (let ((entry (gethash key by-hand)))
              (setf (gethash key by-hand)
                    (list (1+ (or (first entry) 0)) (or (second entry) (docx-read-first-words run)))))))))))

(defun docx-read-look (s)
  "Write to S the section saying how the document looks: its pages, the fonts
it uses and whether they are installed here, the paragraph styles used with
their size and spacing, and what is set by hand (at most *OFFICE-MOST-LISTED*
paragraphs), each line taken from the room left for the note (OFFICE-SPEND)."
  (multiple-value-bind (major minor) (docx-read-theme-fonts)
    (let* ((document (docx-reading-part (docx-main-part)))
           (body (and document (first (xml-find-all document "w:body"))))
           (paragraphs (and body (xml-find-all body "w:p")))
           (defaults (docx-read-defaults))
           (normal (docx-read-default-style))
           (fonts (make-hash-table :test 'equal))
           (font-table (docx-read-font-alternatives)))
      (flet ((use-font (font where)
               (let ((name (docx-read-font-name font major minor)))
                 (pushnew where (gethash name fonts) :test #'string=)))
             (cell (text) (org-table-field-text (org-safe-line text))))
        (multiple-value-bind (used order by-hand-paragraphs by-hand-count)
            (docx-read-paragraph-styles paragraphs normal)
          (let ((by-hand-runs (docx-read-run-fonts body major minor #'use-font)))
            (format s "* Its look~%~%")
            (docx-read-pages s body)
            (format s "~%** Paragraph styles used~%~
| Style | Paragraphs | Font | Size | Before | After | Lines | Indent | Align |~%|-~%")
            (dolist (id order)
              (let ((look (docx-read-merge defaults
                                           (docx-read-merge (if (and normal (not (equal id normal)))
                                                                (docx-read-style-look normal) '())
                                                            (docx-read-style-look id)))))
                (when (getf look :font) (use-font (getf look :font) (docx-read-style-name id)))
                (format s "| ~a | ~d | ~a~:[~; bold~]~:[~; italic~] | ~a pt | ~a pt | ~a pt | ~a | ~a | ~a |~%"
                        (cell (docx-read-style-name id)) (gethash id used)
                        (if (getf look :font) (cell (docx-read-font-name (getf look :font) major minor)) "?")
                        (getf look :bold) (getf look :italic)
                        (docx-twips-pt (* 10 (or (getf look :size) 20)))
                        (docx-twips-pt (or (getf look :before) 0)) (docx-twips-pt (or (getf look :after) 0))
                        (docx-read-lines-words look) (docx-read-indent-words look)
                        (docx-read-align-words (getf look :align)))))
            (format s "~%** Fonts~%| Font | Used by | Stands in where missing | Here |~%|-~%")
            (let ((names '()) (installed (installed-font-families :every-kind t)) (stand-ins (list 0)))
              (maphash (lambda (name where) (push (cons name (reverse where)) names)) fonts)
              (dolist (entry (sort names #'string< :key #'car))
                (let ((family (let ((paren (search " (" (car entry))))
                                (if paren (subseq (car entry) 0 paren) (car entry)))))
                  (format s
                          "| ~a | ~{~a~^, ~} | ~a | ~a |~%"
                          (cell (car entry))
                          (mapcar #'cell (cdr entry))
                          (cell (or (gethash family font-table) ""))
                          (if (string= family "unknown") "" (docx-read-font-here family installed stand-ins))))))
            (when (or by-hand-paragraphs (plusp (hash-table-count by-hand-runs)))
              (format s "~%** Set by hand, not by a style~%")
              (dolist (entry (reverse by-hand-paragraphs))
                (let ((line (format nil "- Paragraph \"~a\" :: ~{~a~^, ~}~%"
                                    (org-safe-line (car entry)) (docx-read-by-hand-words (cdr entry)))))
                  (office-spend (length line))
                  (write-string line s)))
              (when (> by-hand-count *office-most-listed*)
                (format s "- And ~:d paragraphs more (*office-most-listed*)~%" (- by-hand-count *office-most-listed*)))
              (maphash (lambda (key value)
                         (format s "- Text in ~a :: ~d run~:p, first \"~a\"~%"
                                 (org-safe-line key) (first value) (org-safe-line (second value))))
                       by-hand-runs))
            (terpri s)))))))

(defun docx-read-headers (s)
  "Write to S the section giving the text of the document's headers and
footers, when it has any."
  (let ((parts '()))
    (dolist (rel (docx-part-relations (docx-main-part)))
      (let ((type (getf rel :type)))
        (when (and (not (getf rel :outside))
                   (or (uiop:string-suffix-p type "/header") (uiop:string-suffix-p type "/footer")))
          (pushnew (cons (if (uiop:string-suffix-p type "/header") "Header" "Footer") (getf rel :target)) parts
                   :test #'equal))))
    (when parts
      (format s "* Headers and footers~%")
      (dolist (part (sort parts #'string< :key #'cdr))
        (let ((tree (docx-reading-part (cdr part))))
          (format s "- ~a (~a) :: ~a~%" (car part) (org-safe-line (cdr part))
                  (if tree
                      (string-trim " " (org-safe-line (with-output-to-string (b)
                                                        (dolist (root (xml-children tree))
                                                          (when (consp root) (docx-read-block root b))))
                                                      :escaped t))
                      ""))))
      (terpri s))))

;;; The note

(defun metadata-value (text)
  "TEXT, a value a file holds, as the note shows it: on one line, kept from
being read as markup (ORG-SAFE-LINE), at most *METADATA-VALUE-MOST*
characters."
  (let ((text (org-safe-line (string-trim '(#\Space #\Tab #\Nul) (or text ""))))
        (most *metadata-value-most*))
    (if (> (length text) most) (format nil "~a... (~:d characters)" (subseq text 0 most) (length text)) text)))

(defun metadata-lines (fields out)
  "Write to OUT a line - LABEL :: VALUE for each of FIELDS, each (LABEL .
VALUE), both as METADATA-VALUE shows them; those with no value left out.
Two colons or more in a label, which a file names itself, are written as
one, so that the line's first :: always ends its label, for Org and for a
program reading the note."
  (loop for (label . value) in fields
        when (plusp (length value))
        do (format out "- ~a :: ~a~%" (cl-ppcre:regex-replace-all "::+" (metadata-value label) ":")
                   (metadata-value value))))

(defun office-about-section (what path fields &key more macros events embedded outside (changes 0) (comments 0))
  "The first section of a note read from the office file at PATH, WHAT in
words (a document, a workbook, a presentation): its name and size, FIELDS,
what it says of itself as (LABEL . VALUE) (METADATA-LINES), then MORE (lines
of the note, or NIL), then the names of the MACROS it holds, of the macros
EVENTS start, of its EMBEDDED files and the addresses OUTSIDE it it links to,
none of which is run, opened or followed, and how many tracked CHANGES and
COMMENTS it has."
  (with-output-to-string (s)
    (format s "* About this ~a~%" what)
    (metadata-lines (cons (cons "File" (format nil "~a (~a)" (tuatara.io:native-file-name path)
                                               (octets-words (length (docx-reading-octets *docx-reading*)))))
                          fields)
                    s)
    (when more (write-string more s))
    (when macros (format s "- Macros :: yes (~{~a~^, ~}), never run~%" (mapcar #'org-safe-line macros)))
    (flet ((names (label names after)
             (when names (format s "- ~a :: ~{~a~^, ~} ~a~%" label (mapcar #'org-safe-line names) after))))
      (names "Macros run on events" events "(never run)")
      (names "Embedded files" embedded "(not opened)")
      (names "Links outside it" outside "(none followed)"))
    (when (plusp changes) (format s "- Tracked changes :: ~d (listed at the end)~%" changes))
    (when (plusp comments) (format s "- Comments :: ~d~%" comments))
    (terpri s)))

(defun office-changes-section (changes)
  "The section listing CHANGES, the tracked changes of a document, each (KIND
AUTHOR DATE WORDS), KIND :INSERTED, :DELETED or another for formatting
changed; an empty string when there are none."
  (if (null changes)
      ""
      (with-output-to-string (s)
        (format s "* Tracked changes~%")
        (loop for (kind author date words) in changes
              do (format s
                         "- ~a by ~a~@[, ~a~] :: ~a~%"
                         (case kind (:inserted "Inserted") (:deleted "Deleted") (t "Formatting changed"))
                         (org-safe-line (or author "someone")) (and date (org-safe-line date)) (org-safe-line words)))
        (terpri s))))

(defun office-note (path &rest sections)
  "The note read from the office file at PATH: its name as the title, then SECTIONS, strings, one after the other."
  (apply #'concatenate 'string (format nil "#+title: ~a~%~%" (org-safe-line (tuatara.io:native-file-name path)))
         sections))

(defun office-text-section (text)
  "The section holding TEXT, a document's own text read as a note, its headings put below the section's."
  (concatenate 'string (format nil "* The text~%~%") (org-shift-levels text 1)))

(defun docx-read-about (path &key (what "document") (program "Word") (usual "word/document.xml") more)
  "The first section of the note (OFFICE-ABOUT-SECTION): what the document at
PATH is, WHAT in words \(a document, a workbook, a presentation), made for
PROGRAM (Word, Excel, PowerPoint), whose main part is USUAL as standard; what
the file says of itself, then MORE (lines of the note, or NIL), then whether
it holds macros, embedded files or links outside it."
  (let* ((core (docx-reading-part "docProps/core.xml"))
         (app (docx-reading-part "docProps/app.xml"))
         (settings (docx-reading-part "word/settings.xml"))
         (names (mapcar (lambda (e) (getf e :name)) (docx-reading-entries *docx-reading*))))
    (flet ((core (name) (xml-first-text core name))
           (app (name) (xml-first-text app name)))
      (office-about-section
       what path
       (list (cons "Title" (core "dc:title")) (cons "Subject" (core "dc:subject"))
             (cons "Made by" (core "dc:creator")) (cons "Last changed by" (core "cp:lastModifiedBy"))
             (cons "Made" (core "dcterms:created")) (cons "Last changed" (core "dcterms:modified"))
             (cons "Printed" (core "cp:lastPrinted")) (cons "Revision" (core "cp:revision"))
             (cons "Keywords" (core "cp:keywords")) (cons "Description" (core "dc:description"))
             (cons "Company" (app "Company")) (cons "Program" (app "Application"))
             (cons "Template" (app "Template"))
             (cons "Editing time" (let ((m (app "TotalTime"))) (and m (format nil "~a minutes" m))))
             (cons "Pages, words"
                   (let ((p (app "Pages")) (w (app "Words")))
                     (and (or p w) (format nil "~@[~a pages~]~@[, ~a words~]" p w))))
             (cons "Slides" (let ((n (app "Slides")) (hidden (app "HiddenSlides")) (notes (app "Notes")))
                              (and n (format nil "~a~@[, ~a hidden~]~@[, ~a with notes~]" n
                                             (and (not (equal hidden "0")) hidden)
                                             (and (not (equal notes "0")) notes))))))
       :more (with-output-to-string (s)
               (let ((attached (and settings (first (xml-find-all settings "w:attachedTemplate")))))
                 (when attached
                   (let ((target (getf (docx-relation-with-id (docx-part-relations "word/settings.xml")
                                                              (xml-attribute attached "r:id")) :target)))
                     (when target (format s "- Attached template :: ~a (not fetched)~%" (org-safe-line target))))))
               ;; the part its program shows is elsewhere than usual: said, and what is under the usual name too
               (let ((main (docx-main-part usual)))
                 (unless (string= main usual)
                   (format s "- Main part :: ~a, as _rels/.rels says (what ~a shows)~:[~;; it also holds ~a, which ~
~a does not show~]~%"
                           (org-safe-line main) program (docx-entry usual) usual program)))
               (when more (write-string more s)))
       :macros (let ((macros (remove-if-not (lambda (n)
                                              (or (search "vba" (string-downcase n)) (search "/macrosheets/" n)))
                                            names)))
                 (and (find-if (lambda (n) (or (search "vbaProject" n) (search "/macrosheets/" n))) macros)
                      macros))
       :embedded (let ((directory (subseq usual 0 (1+ (or (position #\/ usual) -1)))))
                   (remove-if-not (lambda (n) (or (uiop:string-prefix-p (format nil "~aembeddings/" directory) n)
                                                  (uiop:string-prefix-p (format nil "~aactiveX/" directory) n)))
                                  names))
       :outside (docx-outside-links names)
       :changes (length (docx-reading-changes *docx-reading*))
       :comments (hash-table-count (docx-reading-comments *docx-reading*))))))

(defun office-listed (table)
  "The keys of TABLE, strings, sorted, as a list a note shows: at most
*OFFICE-MOST-LISTED* of them, then how many more there are (and 12 more)."
  (let* ((keys (sort (loop for key being the hash-keys of table collect key) #'string<))
         (more (- (length keys) *office-most-listed*)))
    (if (plusp more)
        (append (subseq keys 0 *office-most-listed*) (list (format nil "and ~:d more" more)))
        keys)))

(defun docx-outside-links (names)
  "The addresses outside it the document being read links to, sorted, each
once, as many as OFFICE-LISTED gives: those of the relations in every part of
NAMES, the names of its parts, that holds relations (its main part's, its
headers', its sheets', its slides'). None is followed."
  (let ((outside (make-hash-table :test 'equal)))
    (dolist (name names)
      (when (and (uiop:string-suffix-p name ".rels") (search "_rels/" name))
        (let ((rels (docx-reading-part name)))
          (dolist (rel (and rels (xml-find-all rels "Relationship")))
            (when (equal (xml-attribute rel "TargetMode") "External")
              (setf (gethash (or (xml-attribute rel "Target") "") outside) t))))))
    (office-listed outside)))

(defun docx-read-footnote-lines (text out)
  "Write to OUT the definition of each footnote and endnote TEXT, Org text read
of a Word document, refers to ([fn:1], [fn:e1], by their labels as
DOCX-READ-LABEL writes them), in the order first referred to, as Org
footnotes; true when there was one."
  (let ((ids '()))
    (cl-ppcre:do-register-groups (id) ("\\[fn:([A-Za-z0-9_-]+)\\]" text) (pushnew id ids :test #'string=))
    (let ((any nil))
      (dolist (id (reverse ids) any)
        (let ((note (gethash id (docx-reading-footnotes *docx-reading*))))
          (when note
            (setf any t)
            (format out "[fn:~a] ~a~%" id (string-trim '(#\Space #\Newline)
                                                       (with-output-to-string (n) (docx-read-block note n))))))))))

(defun docx-read-note (path &key octets cancel)
  "The Word document at PATH as an Org note, read as this file's header says,
from its OCTETS when they are given, no larger than *OFFICE-NOTE-SIZE-LIMIT*
characters; CANCEL, a cons, stops the reading when its car turns true.
Signals ZIP-ERROR or an error when it cannot be read."
  (let ((*docx-reading* (docx-reading-of path octets cancel)))
    (docx-reading-setup)
    (let* ((document (docx-main-tree path "word/document.xml" "document" "Word document"))
           (body (first (xml-find-all document "w:body")))
           (look (office-written-text #'docx-read-look))
           (headers (office-written-text #'docx-read-headers))
           (text (office-written-text (lambda (s) (when body (docx-read-block body s)))))
           (notes (office-written-text
                   (lambda (s)
                     (docx-read-footnote-lines text s)
                     (dolist (id (reverse (docx-reading-comment-order *docx-reading*)))
                       (let ((comment (gethash id (docx-reading-comments *docx-reading*))))
                         (when comment
                           (format s
                                   "[fn:~a] Comment by ~a~@[, ~a~]: ~a~%"
                                   (docx-read-note-label :comment id)
                                   (org-safe-line (or (xml-attribute comment "w:author") "someone"))
                                   (let ((date (xml-attribute comment "w:date"))) (and date (org-safe-line date)))
                                   (string-trim " "
                                                (org-safe-line (with-output-to-string (n) (docx-read-block comment n))
                                                               :escaped t)))))))))
           (changes (office-changes-section (reverse (docx-reading-changes *docx-reading*)))))
      (office-note path
                   (docx-read-about path)
                   look
                   headers
                   (office-text-section text)
                   changes
                   (if (plusp (length notes)) (format nil "* Footnotes~%~a" notes) "")))))
