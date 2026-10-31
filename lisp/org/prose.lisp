;;;; lisp/org/prose.lisp --- Where prose is written: in an Org note by its parse, in other text by its syntax tree.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Prose is what is written as sentences: in a mode of text all but its
;;;; code, tables, links and maths; in a programming language only its
;;;; comments. An Org note is read by its own parse, any other text by its
;;;; syntax tree, a line half typed (a link or a code span not closed yet)
;;;; being no prose until it is closed. Here, after Org, so that its parse is
;;;; at hand; asked by commands and packages that treat sentences.

(in-package #:tuatara.defaults)

(defparameter *org-laid-out-types*
  '(src-block example-block export-block comment-block fixed-width
    table table-row table-cell
    link code verbatim inline-src-block
    latex-fragment latex-environment macro
    citation citation-reference footnote-reference timestamp
    keyword node-property property-drawer)
  "What Org's parse names the parts of a note that are laid out or read as
they are rather than written as prose.")

(defparameter *laid-out-node-names*
  "code|fence|table|link_destination|uri|url|math|formula|equation|verbatim|raw|literal|front_matter|metadata|html_block"
  "A pattern matching the names a language's syntax tree gives the parts of a
document that are not prose: code, tables, link targets, maths.")

(defun org-laid-out-ranges (tree text)
  "The parts of TEXT, an Org note, and TREE, its parse, that are laid out
rather than written (*ORG-LAID-OUT-TYPES*), and the blocks not closed yet
whose contents are not prose (*ORG-LAID-OUT-BLOCKS*), to the next headline or
the end: each (START . END) in characters, in order."
  (let ((ranges '()) (open nil) (opened 0))
    (labels ((walk (node)
               (if (member (org-node-type node) *org-laid-out-types*)
                   (push (cons (org-node-begin node) (org-node-end node)) ranges)
                   (progn
                     (when (eq (org-node-type node) 'headline) (mapc #'walk (org-property node :title)))
                     (mapc #'walk (org-node-children node))))))
      (walk tree))
    (loop with from = 0
          do (multiple-value-bind (start end group-starts group-ends)
                 (cl-ppcre:scan "(?im)^(?:\\*+ |[ \\t]*#\\+(begin|end)_(\\S+))" text :start from)
               (unless start (return))
               (let ((kind (and (aref group-starts 0) (subseq text (aref group-starts 0) (aref group-ends 0))))
                     (name (and (aref group-starts 1) (string-downcase (subseq text (aref group-starts 1) (aref group-ends 1))))))
                 (cond ((null kind)
                        (when open (push (cons opened start) ranges))
                        (setf open nil))
                       ((string-equal kind "begin") (unless open (setf open name opened start)))
                       ((equal name open) (setf open nil))))
               (setf from (max end (1+ start)))))
    (when (and open (member open *org-laid-out-blocks* :test #'string=))
      (push (cons opened (length text)) ranges))
    (sort ranges #'< :key #'car)))

(defun within-ranges-p (ranges position)
  "True when POSITION is within one of RANGES, each (START . END), in order of
their starts."
  (loop for (start . end) in ranges
        while (<= start position)
        thereis (< position end)))

(defun half-typed-p (line-text)
  "True when LINE-TEXT, a line up to a position, leaves something open that is
not prose: an Org link with no ]] yet, a Markdown link's target with no ) yet,
an odd number of backquotes (a code span), or an Org ~code~ or =verbatim=
begun after a blank or at the line's start and not closed yet."
  (or (cl-ppcre:scan "\\[\\[[^\\]]*$" line-text)
      (cl-ppcre:scan "\\]\\([^)]*$" line-text)
      (oddp (count #\` line-text))
      (cl-ppcre:scan "(?:^|[ \\t])(?:~[^ \\t~][^~]*|=[^ \\t=][^=]*)$" line-text)))

(defun line-before (buffer position)
  "The text of BUFFER's line up to POSITION."
  (tuatara.buffer:buffer-string buffer :start (tuatara.buffer:buffer-line-start buffer position) :end position))

(defun org-section-around (buffer position)
  "The part of BUFFER's Org text read to tell whether POSITION is prose: from
the headline above it, or at most *ORG-PARSE-AT-ONCE* octets back, to the end
of POSITION's line, as two values: the text and the character of it POSITION
is."
  (let* ((floor (max 0 (- position *org-parse-at-once*)))
         (start (loop with line = (tuatara.buffer:buffer-line-start buffer position)
                      do (when (or (<= line floor) (zerop line))
                           (return (tuatara.buffer:buffer-line-start buffer (max line floor))))
                         (when (eql 0 (cl-ppcre:scan *org-headline-re* (tuatara.buffer:buffer-line-text buffer line)))
                           (return line))
                         (setf line (tuatara.buffer:buffer-line-start buffer (1- line)))))
         (end (tuatara.buffer:buffer-line-end buffer position)))
    (values (org-buffer-text buffer :start start :end end) (length (org-buffer-text buffer :start start :end position)))))

(defun org-text-prose-p (ranges text index)
  "True when the character INDEX of TEXT, Org text, is in prose: outside
RANGES, the parts of TEXT laid out as not prose (ORG-LAID-OUT-RANGES), its
line before it leaving nothing open (HALF-TYPED-P)."
  (not (or (within-ranges-p ranges index)
           (half-typed-p (subseq text (tuatara.text:string-line-start text index) index)))))

(defun org-buffer-laid-out-ranges (buffer tree text)
  "ORG-LAID-OUT-RANGES of TREE, the parse of TEXT, BUFFER's text or a part of
it, worked out once for each parse, the last asked for kept."
  (let ((kept (tuatara.buffer:buffer-local buffer 'org-laid-out-ranges)))
    (if (eq (car kept) tree)
        (cdr kept)
        (let ((ranges (org-laid-out-ranges tree text)))
          (setf (tuatara.buffer:buffer-local buffer 'org-laid-out-ranges) (cons tree ranges))
          ranges))))

(defun org-prose-p (buffer position)
  "True when POSITION of BUFFER, an Org buffer, is in prose, as the parse of
the part of it POSITION is in tells (ORG-VIEW-AT) when it is of the text as
it is now, else the parse of the section around POSITION (ORG-SECTION-AROUND)."
  (let ((view (org-view-at buffer position)))
    (if (and view (org-view-current view))
        (let ((text (org-view-text view)))
          (org-text-prose-p (org-buffer-laid-out-ranges buffer (org-view-tree view) text) text
                            (min (length text) (org-view-char buffer view position))))
        (multiple-value-bind (text index) (org-section-around buffer position)
          (org-text-prose-p (org-laid-out-ranges (org-parse text) text) text index)))))

(defun prose-buffer-p (buffer)
  "True when BUFFER's major mode is one for text, prose throughout but its
code, tables, links and maths: text-mode and those made from it (Org among
them), and fundamental-mode itself; false in a programming language, whose
prose is its comments."
  (let ((mode (buffer-major-mode buffer)))
    (or (typep mode 'text-mode) (eq (type-of mode) 'fundamental-mode))))

(defun laid-out-node-p (syntax position)
  "True when POSITION of SYNTAX's text is inside a part its tree names as not
prose (*LAID-OUT-NODE-NAMES*)."
  (some (lambda (node)
          (and (tuatara.syntax:tree-node-named node)
               (cl-ppcre:scan *laid-out-node-names* (tuatara.syntax:tree-node-name node))))
        (tuatara.syntax:node-ancestry syntax position (1+ position))))

(defun prose-ready-p (buffer)
  "True when PROSE-P can tell for BUFFER now: an Org buffer, one with no
syntax tree, or one whose syntax tree is up to date."
  (or (typep (buffer-major-mode buffer) 'org-mode)
      (null (tuatara.syntax:buffer-syntax buffer))
      (and (ready-syntax buffer) t)))

(defun prose-p (buffer position)
  "True when the octet POSITION of BUFFER is in prose: in an Org buffer as its
parse tells; in another mode of text (PROSE-BUFFER-P) outside the code,
tables, links and maths its syntax tree names, and anywhere in one with no
syntax tree; in a programming language only inside a comment. Never in a
line's link or code span not closed yet. NIL where a syntax tree is needed
and is not ready (PROSE-READY-P)."
  (cond ((typep (buffer-major-mode buffer) 'org-mode) (org-prose-p buffer position))
        ((prose-buffer-p buffer)
         (or (null (tuatara.syntax:buffer-syntax buffer))
             (let ((syntax (ready-syntax buffer)))
               (and syntax
                    (not (laid-out-node-p syntax position))
                    (not (half-typed-p (line-before buffer position)))))))
        (t (let ((syntax (ready-syntax buffer)))
             (and syntax (eq (tuatara.syntax:text-kind-around syntax position) :comment))))))

(defun prose-test (buffer)
  "A function telling, as PROSE-P does, whether an octet position of BUFFER is
in prose, for positions asked in order from the first: an Org buffer's parse
read once for all of them."
  (if (typep (buffer-major-mode buffer) 'org-mode)
      (multiple-value-bind (tree text) (org-tree-now buffer)
        (let ((ranges (org-buffer-laid-out-ranges buffer tree text)))
          (lambda (position)
            (let ((at (org-byte->char buffer position)))
              (loop while (and ranges (<= (cdar ranges) at)) do (pop ranges))
              (org-text-prose-p ranges text at)))))
      (lambda (position) (prose-p buffer position))))
