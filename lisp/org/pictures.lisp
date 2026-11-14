;;;; lisp/org/pictures.lisp --- Org's linked pictures shown among its lines, and their widths.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A link to a picture alone on its line, [[file:diagram.png]] or
;;;; [[./diagram.png]] with no description, is shown as the picture while
;;;; inline-pictures-mode is on (#+STARTUP: inlineimages turns it on for a
;;;; file, noinlineimages off). Its #+ATTR_ORG: line sizes and places it as
;;;; Org 9.7 reads that line: :width 400 or 400px in pixels, 50% or 0.5 of the
;;;; text's width (the window's, up to the fill column), t its own width, else
;;;; the first other #+ATTR_...: line's width that reads so; for a picture
;;;; alone in its paragraph, :align left, center or right, or :center t, from
;;;; #+ATTR_ORG: first, then any other. C-c C-x w and then + = - 0 change the
;;;; width written there. Formulas are shown as pictures too: $...$ and \(...\)
;;;; inside a line, \[...\] or $$...$$ alone on their lines and LaTeX
;;;; environments on rows of their own.

(in-package #:tuatara.defaults)

(defun org-picture-link-p (node)
  "True when NODE is a link shown as a picture among the lines: to a file, with
no description, whose name says it is a picture the image view draws
\(ORG-IMAGE-LINK-P)."
  (org-image-link-p node #'inline-picture-path-p))

(defun org-formula-fragment-p (node)
  "True when NODE is a formula shown as a picture: a LaTeX fragment in dollar
signs or LaTeX's own math delimiters, not a command alone."
  (and (eq (org-node-type node) 'latex-fragment)
       (let ((value (org-property node :value)))
         (or (uiop:string-prefix-p "$" value) (uiop:string-prefix-p "\\(" value)
             (uiop:string-prefix-p "\\[" value)))))

(defparameter *org-formula-header-reach* 65536
  "How many octets from its start an Org note's #+LATEX_HEADER: lines are
looked for in, for its formulas shown as pictures, as far as its first
heading.")

(define-setting *org-startup-shows-pictures* nil
  "Whether an Org file's #+STARTUP: may turn its pictures and formulas on.
#+STARTUP: inlineimages or latexpreview would show them, its formulas drawn by
LaTeX as it opens; noinlineimages and nolatexpreview turn them off either way.
- nil: a file's words only turn them off; \\[inline-pictures-mode] turns them on.
- t: its words turn them on too, in a file visited as usual, not safely or
  read-only."
  :values boolean)

(defun org-own-words-p (buffer)
  "True when BUFFER's Org file is visited as usual: not safely
\(TUATARA.IO:SAFE-VISIT-P), and may be written. A note given you, opened
safely or read-only, never has what it says of itself run."
  (not (or (tuatara.io:safe-visit-p buffer) (tuatara.buffer:buffer-read-only-p buffer))))

(defun org-file-trusted-p (buffer)
  "True when BUFFER's Org file is visited as usual (ORG-OWN-WORDS-P) and is in
a project you trust (BUFFER-IN-TRUSTED-PROJECT-P): its own #+LATEX_HEADER:
lines are then drawn with its formulas."
  (and (org-own-words-p buffer) (buffer-in-trusted-project-p buffer)))

(defun org-formula-preamble (buffer)
  "The LaTeX the formulas of BUFFER, an Org buffer, are drawn with before
\\begin{document}: *FORMULA-PREAMBLE*, then its #+LATEX_HEADER: lines above
its first heading (within *ORG-FORMULA-HEADER-REACH*) when the file is trusted
\(ORG-FILE-TRUSTED-P), kept while its text is unchanged."
  (let ((version (tuatara.buffer:buffer-version buffer))
        (kept (tuatara.buffer:buffer-local buffer 'org-formula-preamble)))
    (if (and kept (eq (car kept) version))
        (cdr kept)
        (let* ((text (tuatara.buffer:buffer-string buffer :end (min (tuatara.buffer:buffer-size buffer)
                                                                    *org-formula-header-reach*)))
               (head (subseq text 0 (or (cl-ppcre:scan "(?m)^\\*+ " text) (length text))))
               (lines (let ((found '()))
                        (when (org-file-trusted-p buffer)
                          (cl-ppcre:do-register-groups (line) ("(?im)^[ \\t]*#\\+latex_header:[ \\t]?(.*)$" head)
                            (push line found)))
                        (nreverse found))))
          (cdr (setf (tuatara.buffer:buffer-local buffer 'org-formula-preamble)
                     (cons version (format nil "~a~{~%~a~}" *formula-preamble* lines))))))))

(defun org-attribute-number (text)
  "The number TEXT, digits with perhaps a point among them, as an exact
rational; NIL when it is no such number."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^\\+?([0-9]{0,6})(?:\\.([0-9]{1,6}))?$" text)
    (when (and match (or (plusp (length (aref groups 0))) (aref groups 1)))
      (+ (if (plusp (length (aref groups 0))) (parse-integer (aref groups 0)) 0)
         (if (aref groups 1)
             (/ (parse-integer (aref groups 1)) (expt 10 (length (aref groups 1))))
             0)))))

(defun org-picture-width-value (text)
  "The width a #+ATTR_...: line's :width TEXT asks of a picture shown among the
lines, as an INLINE-PICTURE's width: T for t (its own width), a whole number
of pixels for 400 or 400px, a fraction of the text's width
\(INLINE-PICTURE-ROOM) for 50% or for a number with a point up to 2 (0.5); NIL
when it reads as none of these."
  (when text
    (let ((text (string-trim " " text)))
      (cond ((string-equal text "t") t)
            ((cl-ppcre:scan "^\\+?[0-9]{1,6}(?:px)?$" text)
             (max 1 (parse-integer (string-right-trim "px" text) :junk-allowed t)))
            ((and (> (length text) 1) (char= (char text (1- (length text))) #\%))
             (let ((number (org-attribute-number (subseq text 0 (1- (length text))))))
               (and number (plusp number) (/ number 100))))
            ((find #\. text)
             (let ((number (org-attribute-number text)))
               (and number (< 0 number) (<= number 2) number)))))))

(defun org-picture-float-value (float placement)
  "The side a #+ATTR_...: line's :float FLOAT and :placement PLACEMENT have a
picture stand on with the text going round it: :left or :right for left or
right; for wrap (LaTeX's wrapfigure), :right when PLACEMENT begins {r or {R,
else :left, as Org's LaTeX export places it; NIL for anything else."
  (cond ((string-equal float "left") :left)
        ((string-equal float "right") :right)
        ((string-equal float "wrap")
         (if (and placement (cl-ppcre:scan "^\\s*\\{[rR]" placement)) :right :left))))

(defun org-picture-attributes (paragraph)
  "How the #+ATTR_...: lines of PARAGRAPH size and place its picture, as three
values: its width (ORG-PICTURE-WIDTH-VALUE), from #+ATTR_ORG: when it reads,
else from the first other line whose :width reads; its place across the
window, :left, :center or :right, from :align (or :center t) on #+ATTR_ORG:,
else on any other #+ATTR_...: line; and the side it stands on with the text
going round it (ORG-PICTURE-FLOAT-VALUE), from :float on #+ATTR_ORG: first,
then on any other, or NIL."
  (let* ((affiliated (org-property paragraph :affiliated))
         (lines (cons (getf affiliated :attr_org)
                      (loop for (key value) on affiliated by #'cddr
                            when (and (member key *org-attribute-keywords*) (not (eq key :attr_org)))
                              collect value)))
         (settings (mapcar #'org-keyword-arguments lines)))
    (flet ((setting (arguments name) (cdr (assoc name arguments :test #'string-equal))))
      (values (loop for arguments in settings
                    thereis (org-picture-width-value (setting arguments "width")))
              (or (loop for arguments in settings
                        thereis (let ((align (setting arguments "align")))
                                  (cond ((equal (setting arguments "center") "t") :center)
                                        ((string-equal align "center") :center)
                                        ((string-equal align "right") :right)
                                        ((string-equal align "left") :left))))
                  :left)
              (loop for arguments in settings
                    thereis (org-picture-float-value (setting arguments "float") (setting arguments "placement")))))))

(defun org-inline-pictures (buffer start end &key (formulas t))
  "The pictures BUFFER, an Org buffer, shows among its lines that begin from
octet START below END, as INLINE-PICTUREs in order: each link to a picture
\(ORG-PICTURE-LINK-P) with nothing else on its line but blanks, sized by the
#+ATTR_...: lines of its paragraph (ORG-PICTURE-ATTRIBUTES), and placed
across the window by them when it is the paragraph's only content; its path
from the buffer's directory. With FORMULAS, each formula too, drawn with
ORG-FORMULA-PREAMBLE: a fragment (ORG-FORMULA-FRAGMENT-P) inside a line, in
its columns; one in display delimiters alone on its lines, and a LaTeX
environment, centred on rows of their own. True as a second value when the
parses read are of the text as it is now."
  (let ((found '()) (current t)
        (end (min end (tuatara.buffer:buffer-size buffer)))
        (directory (buffer-directory buffer)))
    (dolist (view (org-views buffer start (max end (1+ start))))
      (if (not (org-view-current view))
          (setf current nil)
          (let* ((text (org-view-text view))
                 (*org-export-text* text)
                 (from (org-view-char buffer view (max start (org-view-from view))))
                 (to (org-view-char buffer view (max (min end (org-view-to view)) (org-view-from view))))
                 ;; a link on the last line begun is looked for to that line's end
                 (reach (or (position #\Newline text :start (min to (length text))) (length text))))
            (labels ((picture (paragraph link)
                       (let* ((begin (org-node-begin link))
                              (line (1+ (or (position #\Newline text :end begin :from-end t) -1)))
                              (after (min (length text) (org-node-end link)))
                              (line-end (or (position #\Newline text :start after) (length text))))
                         (when (and (<= from line) (< line to)
                                    (blank-text-p text line begin) (blank-text-p text after line-end))
                           (let ((line-start (org-view-byte buffer view line)))
                             (multiple-value-bind (width align float) (org-picture-attributes paragraph)
                               (push (make-inline-picture
                                      line-start (tuatara.buffer:buffer-line-end buffer line-start)
                                      (tuatara.io:native (tuatara.io:expand-path (org-property link :path)
                                                                                 :directory directory))
                                      :width width :float float
                                      :align (if (org-export-picture-alone-p paragraph #'org-picture-link-p)
                                                 align
                                                 :left))
                                     found))))))
                     (formula (from-char to-char value inline)
                       ;; FROM-CHAR its start, TO-CHAR its end, as characters of the view's text
                       (when (and (<= from from-char) (< from-char to))
                         (push (make-inline-picture (org-view-byte buffer view from-char)
                                                    (org-view-byte buffer view to-char)
                                                    (make-formula value (org-formula-preamble buffer)
                                                                  :inline inline)
                                                    :align (if inline :left :center))
                               found)))
                     (fragment (child)
                       (let* ((value (org-property child :value))
                              (begin (org-node-begin child))
                              (after (min (length text) (+ begin (length value))))
                              (line (1+ (or (position #\Newline text :end begin :from-end t) -1)))
                              (line-end (or (position #\Newline text :start after) (length text))))
                         (if (and (or (uiop:string-prefix-p "\\[" value) (uiop:string-prefix-p "$$" value))
                                  (blank-text-p text line begin) (blank-text-p text after line-end))
                             (formula line line-end value nil)
                             (formula begin after value t))))
                     (environment (node)
                       (let* ((value (org-property node :value))
                              (line (or (search value text :start2 (org-node-begin node)) (org-node-begin node)))
                              (body (string-right-trim '(#\Newline #\Return) value)))
                         (formula line (min (length text) (+ line (length body)))
                                  (string-trim *org-blanks* body) nil)))
                     (walk (node)
                       (case (org-node-type node)
                         (paragraph
                          (org-children-in-range node from (max reach (1+ from))
                                                 (lambda (child)
                                                   (cond ((org-picture-link-p child) (picture node child))
                                                         ((and formulas (org-formula-fragment-p child))
                                                          (fragment child))))))
                         (latex-environment (when formulas (environment node)))
                         (t (org-children-in-range node from (max reach (1+ from)) #'walk)))))
              (walk (org-view-tree view))))))
    (values (sort found #'< :key #'inline-picture-start) current)))

(defun org-startup-pictures (buffer options)
  "Have BUFFER, an Org buffer whose #+STARTUP: words are OPTIONS, show its
linked pictures and its formulas among its lines while
\\[inline-pictures-mode] is on there, and turn that on or off for it where
the last of inlineimages and latexpreview (on), noinlineimages and
nolatexpreview (off) there says so: on only as *ORG-STARTUP-SHOWS-PICTURES*
allows, in a file visited as usual (ORG-OWN-WORDS-P)."
  (show-inline-pictures buffer 'org-inline-pictures)
  (let* ((choice (org-startup-choice options :pictures))
         (on (member choice '("inlineimages" "latexpreview") :test #'string=)))
    (when (and choice (or (not on) (and *org-startup-shows-pictures* (org-own-words-p buffer))))
      (setf (tuatara.buffer:buffer-local buffer :inline-pictures) (and on t)))))

;;; Changing a picture's width

(defparameter *org-picture-step* 6/5
  "How many times as wide \\[org-resize-picture] makes a picture at each step
larger, and as many times narrower at each step smaller.")

(defparameter *org-picture-resize-repeat-keys*
  '(("+" org-resize-picture) ("=" org-resize-picture) ("-" org-resize-picture) ("0" org-resize-picture))
  "The keys that, right after \\[org-resize-picture], change the picture's
width again by themselves.")

(defun org-keyword-line-p (buffer line-start)
  "True when BUFFER's line beginning at LINE-START is a keyword line, #+NAME:
#+CAPTION: #+ATTR_ORG: and the like."
  (cl-ppcre:scan "^[ \\t]*#\\+[A-Za-z_]+(?:\\[.*\\])?:"
                 (tuatara.buffer:buffer-string buffer :start line-start
                                                      :end (tuatara.buffer:buffer-line-end buffer line-start))))

(defun org-picture-near-point (buffer)
  "The picture (an INLINE-PICTURE) on the line point is on in BUFFER, or on the
first line after the keyword lines (#+ATTR_ORG: and the like) point is on;
NIL when there is none."
  (let ((line (tuatara.buffer:buffer-line-start buffer (tuatara.buffer:buffer-point buffer)))
        (size (tuatara.buffer:buffer-size buffer)))
    (loop repeat 32
          do (let ((picture (find line (org-inline-pictures buffer line (1+ line) :formulas nil)
                                  :key #'inline-picture-start))
                   (line-end (tuatara.buffer:buffer-line-end buffer line)))
               (cond (picture (return picture))
                     ((or (>= line-end size) (not (org-keyword-line-p buffer line))) (return nil))
                     (t (setf line (1+ line-end))))))))

(defun org-attr-org-line (buffer picture)
  "Where the #+ATTR_ORG: line of PICTURE, BUFFER's, is, among the keyword lines
right above its line, as two values, its start and its end; NIL when it has
none."
  (loop with line = (inline-picture-start picture)
        while (plusp line)
        do (setf line (tuatara.buffer:buffer-line-start buffer (1- line)))
           (unless (org-keyword-line-p buffer line) (return nil))
           (let ((end (tuatara.buffer:buffer-line-end buffer line)))
             (when (cl-ppcre:scan "(?i)^[ \\t]*#\\+attr_org:"
                                  (tuatara.buffer:buffer-string buffer :start line :end end))
               (return (values line end))))))

(defun org-picture-width-words (width)
  "WIDTH, a whole number of pixels or a share of the text's width, as a
#+ATTR_ORG: line's :width says it: 480, or 60%."
  (if (integerp width) (format nil "~d" width) (format nil "~d%" (round (* 100 width)))))

(defun org-resized-width (written shown steps)
  "The width a picture is given STEPS steps (*ORG-PICTURE-STEP*) larger,
fewer than none smaller, from the width WRITTEN on its #+ATTR_ORG: line
\(ORG-PICTURE-WIDTH-VALUE: a percentage or a fraction stays one), or from the
pixels it is SHOWN at when that gives none in pixels or a share."
  (let ((factor (expt *org-picture-step* steps)))
    (if (and (realp written) (not (integerp written)))
        (max 1/100 (/ (round (* 100 written factor)) 100))
        (max 1 (round (* shown factor))))))

(defun org-write-picture-width (buffer picture width)
  "Write WIDTH (a whole number of pixels or a fraction of the text's width,
see ORG-PICTURE-WIDTH-WORDS), or NIL for none, as :width on PICTURE's
#+ATTR_ORG: line in BUFFER, the line made right above the picture's when
there is none; with NIL, the :width taken out, and the line too when nothing
is left on it."
  (multiple-value-bind (start end) (org-attr-org-line buffer picture)
    (let* ((line (and start (tuatara.buffer:buffer-string buffer :start start :end end)))
           (head (and line (cl-ppcre:scan-to-strings "(?i)^[ \\t]*#\\+attr_org:" line)))
           (arguments (and line (remove "width" (org-keyword-arguments (subseq line (length head)))
                                        :key #'car :test #'string-equal)))
           (arguments (if width (append arguments (list (cons "width" (org-picture-width-words width)))) arguments))
           (words (format nil "~{~a~^ ~}" (loop for (key . value) in arguments
                                                collect (if (string= value "") (format nil ":~a" key)
                                                            (format nil ":~a ~a" key value))))))
      (cond ((and line arguments) (tuatara.buffer:buffer-replace buffer start end (format nil "~a ~a" head words)))
            (line (tuatara.buffer:buffer-delete buffer start (min (tuatara.buffer:buffer-size buffer) (1+ end))))
            (arguments
             (let* ((at (inline-picture-start picture))
                    (indent (tuatara.buffer:buffer-string buffer :start at :end (inline-picture-end picture)))
                    (blanks (subseq indent 0 (or (position-if-not #'space-or-tab-char-p indent) 0))))
               (tuatara.buffer:buffer-insert buffer at (format nil "~a#+ATTR_ORG: ~a~%" blanks words))))))))

(define-command org-resize-picture ()
  "Make the picture on this line, or after the #+ATTR_ORG: line point is on,
wider or narrower by the last key that ran it: \\`+' or \\`=' a step wider
\(*ORG-PICTURE-STEP*), \\`-' a step narrower, \\`0' back to the width it has
when none is written; any other key shows it and says its width. A prefix
argument gives that many steps, wider for any other key. Right after it,
\\`+', \\`=', \\`-' and \\`0' alone go on. The width is written as :width on the picture's
#+ATTR_ORG: line, in pixels, or as a share of the text's width when it was
written so (50%); the line is made when there is none, and \\`0' takes the
width out, and the line when nothing else is on it. The picture is shown
while it is resized, point on its line or not."
  (let* ((buffer (current-buffer))
         (picture (org-picture-near-point buffer)))
    (cond ((not (typep (buffer-major-mode buffer) 'org-mode))
           (message "Only Org buffers resize their pictures"))
          ((null picture)
           (message "No picture here: point on a link to one alone on its line, or on its #+ATTR_ORG: line"))
          ((not (inline-pictures-p buffer))
           (message "~a" (substitute-command-keys "Pictures are not shown here: \\[inline-pictures-mode] shows them")))
          (t
           (check-writable buffer)
           (let* ((key (editor-this-key *editor*))
                  (base (and key (tuatara.frontend:key-base key)))
                  (written (inline-picture-width picture))
                  (shown (or (inline-picture-cells buffer picture (editor-window *editor*))
                             (nth-value 1 (picture-source-size buffer (inline-picture-source picture)))
                             400))
                  (width (case base
                           (#\0 nil)
                           ((#\+ #\=) (org-resized-width written shown (prefix)))
                           (#\- (org-resized-width written shown (- (prefix))))
                           (t (if (prefix-argument) (org-resized-width written shown (prefix)) :unchanged)))))
             (unless (eq width :unchanged)
               (org-write-picture-width buffer picture width))
             (message "~a: + wider, - narrower, 0 as when none is written"
                      (cond ((eq width :unchanged)
                             (if written
                                 (format nil "Width ~a" (org-picture-width-words written))
                                 (format nil "Width ~d pixels, none written" shown)))
                            (width (format nil "Width ~a" (org-picture-width-words width)))
                            (t "No width written"))))))))

;;; What a note writes to size and place its pictures

(defun describe-picture-turning-on (stream)
  "Write to STREAM the last sections of \\[describe-inline-picture-settings]:
how pictures are turned on, the keys and the settings."
  (let ((s stream))
     (write-help-section s "Turning pictures on"
                         :intro "#+STARTUP: inlineimages turns the pictures of a note on and latexpreview its
                         formulas; noinlineimages and nolatexpreview turn them off. The words turn them on only
                         when *ORG-STARTUP-SHOWS-PICTURES* is t and the note is visited as usual, not safely or
                         read-only; off they always turn them.")
     (write-help-section s "Keys"
                         :rows (list (substitute-command-keys "\\[inline-pictures-mode]")
                                     "show or hide the pictures of the buffer"
                                     (substitute-command-keys "\\<org-mode>\\[inline-add-picture]")
                                     "ask for a picture and write its link with the lines it takes"
                                     (substitute-command-keys "\\<org-mode>\\[org-resize-picture]")
                                     (format nil "on a picture or its #+ATTR_ORG: line, then ~{~a~^ ~}: a step wider,
                                     the same, a step narrower, no width written; a prefix argument gives that many
                                     steps. The width is written as :width on the #+ATTR_ORG: line, in pixels, or
                                     as a share where it was written so"
                                             (mapcar #'first *org-picture-resize-repeat-keys*)))
                         :width 12)
     (write-help-section s "Settings"
                         :rows '("*org-startup-shows-pictures*" "t lets the #+STARTUP: words turn pictures on;
                                 nil, as standard, only off"
                                 "*inline-picture-width*" "how wide a picture is drawn when nothing gives its
                                 width: t, its own, or a number of pixels; never wider than the text"
                                 "*global-inline-pictures-mode*" "show pictures in every buffer that can"
                                 "*inline-pictures-modes*" "show them in the modes listed, as '((org-mode . t))"
                                 "*formula-preamble*" "the LaTeX put before each formula's own")
                         :width 30)))

(define-command describe-inline-picture-settings ()
  "Show what a note writes to have its pictures and formulas shown among its
lines: how the #+ATTR_ORG: line above a picture sizes and places it, which
links and formulas are shown as pictures in Org, Markdown and LaTeX files, the
keys that show and resize them, and the settings that change them. The lines
are filled to *HELP-COLUMN*."
  (show-help
   (with-output-to-string (s)
     (write-help-section s "Pictures among the lines"
                         :intro "A link to a picture alone on its line is shown as the picture, and a formula
                         as it is set, while \\[inline-pictures-mode] is on for the buffer. The text stays as
                         written; point on the picture shows it. A terminal that draws no pictures shows a
                         line naming each picture and its size. Never in a file opened safely."
                         :example "#+CAPTION: The plan
#+ATTR_ORG: :width 50% :align center
[[file:plan.png]]")
     (write-help-section s "What shows as a picture"
                         :rows (list "Org"
                                     (format nil "[[file:diagram.png]] or [[./diagram.png]] alone on its line, with
                                     no label, to a file of one of these kinds: ~{~a~^, ~}. As formulas: $...$
                                     and \\(...\\) in a line, \\=\\[...\\] and $$...$$ alone on their lines,
                                     and LaTeX environments."
                                             (mapcar #'cdr *image-formats*))
                                     "Markdown"
                                     "None among the lines. \\<org-mode>\\[inline-add-picture] writes
                                     ![](picture.png){width=100%} for the programs that make the page."
                                     "LaTeX"
                                     (format nil "Formulas only: $...$, \\(...\\), \\=\\[...\\] and the ~{~a~^, ~}
                                     environments. \\<org-mode>\\[inline-add-picture] writes a figure with
                                     \\includegraphics, which is not shown."
                                             *latex-math-environments*)
                                     "Writing a link"
                                     "\\<org-mode>\\[insert-link] writes a link as the mode writes links; a picture
                                      is a link
                                     to its file with no label.")
                         :width 14)
     (write-help-section s "#+ATTR_ORG: and the lines like it"
                         :intro "Written right above the link, after #+CAPTION: and #+NAME:. Each argument is
                         written :NAME VALUE."
                         :rows '(":width 400" "that many pixels; 400px is the same"
                                 ":width 50%" "half the text's width (the window's, up to the fill column)"
                                 ":width 0.5" "the same: a number with a point, up to 2"
                                 ":width t" "the picture's own width"
                                 ":align left" "where a picture alone in its paragraph stands: left (as standard),
                                 center or right"
                                 ":center t" "the same as :align center"
                                 ":float left" "the text goes round the picture, which stands on the left; right
                                 likewise; wrap, on the side :placement says"
                                 ":placement {r}" "with :float wrap, {r or {R puts the picture on the right, any
                                 other value on the left")
                         :width 16)
     (write-help-section s "Other #+ATTR_...: lines"
                         :intro (format nil "~{~a~^, ~}. Where #+ATTR_ORG: gives no width, align or float, the
                         first of these lines that does is read. :float is Tuatara's own; other programs that
                         read Org ignore it."
                                        (mapcar (lambda (keyword) (format nil "#+~a:" keyword))
                                                *org-attribute-keywords*)))
     (describe-picture-turning-on s))))

(defun install-org-pictures ()
  "Bind \\[org-resize-picture] in Org buffers, and the keys that go on with it."
  (define-key (mode-keymap 'org-mode) "C-c C-x w" 'org-resize-picture)
  (pushnew 'org-resize-picture *picture-showing-commands*)
  (define-repeat-keys *org-picture-resize-repeat-keys*))
