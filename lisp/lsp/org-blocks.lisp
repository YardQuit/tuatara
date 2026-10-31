;;;; lisp/lsp/org-blocks.lisp --- A note's source blocks seen by their languages' servers.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; In an Org note, the code of each language's source blocks is a document
;;;; of its own for that language's servers: the note's lines, those of no
;;;; such block left empty and a block's own indentation and Org's escaping
;;;; commas left out, so that a place in it is the same line of the note. What
;;;; a server finds wrong in the code is underlined in the note; completion,
;;;; definitions, descriptions, renames and fixes work within the blocks as in
;;;; a file of the language. All of one language's blocks are one document, so
;;;; that a name defined in one is known in the next. Blocks written to one file
;;;; of the project (:tangle, as Org's header arguments give it) are seen as
;;;; that file; others by the note's name with the language's ending
;;;; (notes.org.py), a file that is never written.

(in-package #:tuatara.defaults)

(defun org-source-blocks (text)
  "The source blocks of TEXT, an Org note, in order: each (NAME FIRST LAST),
NAME the language it is given, FIRST and LAST the 0-based lines of its code
\(LAST past it): line for line, as the note's documents need them."
  (let ((lines (uiop:split-string text :separator '(#\Newline)))
        (blocks '()) (open nil))
    (loop for line in lines
          for n from 0
          do (if open
                 (when (cl-ppcre:scan "(?i)^[ \\t]*#\\+end_src\\b" line)
                   (push (list (first open) (second open) n) blocks)
                   (setf open nil))
                 (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "(?i)^[ \\t]*#\\+begin_src[ \\t]+([^ \\t]+)" line)
                   (when match
                     (setf open (list (aref groups 0) (1+ n)))))))
    (nreverse blocks)))

(defun org-blocks-tangle-values (buffer language)
  "The :tangle values of LANGUAGE's source blocks of BUFFER, a note, as Org
merges its header arguments (ORG-BLOCK-HEADER-ARGUMENTS), each with the
block's language as written, from the note's last parse; NIL before there is
one."
  (let ((tree (org-buffer-tree buffer))
        (found '()))
    (labels ((walk (node)
               (when (and (eq (org-node-type node) 'src-block) (not (org-commented-p node))
                          (eq (org-src-language (or (org-property node :language) "")) language))
                 (push (cons (cdr (assoc "tangle" (org-block-header-arguments tree node) :test #'string=))
                             (org-property node :language))
                       found))
               (mapc #'walk (org-node-children node))))
      (when tree (walk tree)))
    found))

(defun org-blocks-shadow (buffer language)
  "The code of LANGUAGE's source blocks of BUFFER, a note, line for line with
it, and the octets left out at the start of each of its lines (NIL for a line
of no such code), as ORG-BLOCK-DOC-SPECS's documents show it."
  (let* ((text (tuatara.text:rope-string (tuatara.buffer:buffer-text buffer)))
         (lines (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))
         (shown (make-array (length lines) :initial-element ""))
         (skipped (make-array (length lines) :initial-element nil)))
    (loop for (name first last) in (org-source-blocks text)
          when (eq (org-src-language name) language)
            do (let ((indent (loop for n from first below last
                                   for line = (aref lines n)
                                   unless (zerop (length (string-trim '(#\Space #\Tab) line)))
                                     minimize (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line) 0))))
                 (loop for n from first below last
                       for line = (aref lines n)
                       for cut = (min (or indent 0) (length line))
                       ;; Org's comma before a line that would read as a headline or a keyword
                       for comma = (let ((rest (subseq line cut)))
                                     (if (or (uiop:string-prefix-p ",*" rest) (uiop:string-prefix-p ",#+" rest)) 1 0))
                       do (setf (aref shown n) (subseq line (+ cut comma))
                                (aref skipped n) (tuatara.text:string-octet-length line :end (+ cut comma))))))
    (values (format nil "~{~a~^~%~}" (coerce shown 'list)) skipped)))

(defun org-blocks-uri (buffer language)
  "The address LANGUAGE's source blocks of BUFFER are seen at: the file of
its project they are all written to (:tangle), when no buffer has it open;
else the note's own name with LANGUAGE's ending."
  (let* ((file (tuatara.io:native (tuatara.buffer:buffer-file buffer)))
         (tangled (remove-duplicates (org-blocks-tangle-values buffer language) :test #'equal :key #'car))
         (target (and (= 1 (length tangled))
                      (destructuring-bind ((tangle . name)) tangled
                        (cond ((member tangle '("no" "") :test #'string-equal) nil)
                              ((string-equal tangle "yes")
                               (format nil "~a.~a" (tuatara.io:native-file-stem file)
                                       (or (cdr (assoc name *code-file-extensions* :test #'string=)) (language-extension language))))
                              (t tangle)))))
         (path (and target (tuatara.io:clean-path (tuatara.io:native (tuatara.io:expand-path target :directory (tuatara.io:parent-path file)))))))
    (path-uri (if (and path (tuatara.io:really-within-p path (lsp-project-of buffer language)) (not (buffer-visiting path)))
                  path
                  (format nil "~a.~a" file (language-extension language))))))

(defun org-blocks-languages (buffer)
  "The languages a server knows of BUFFER's source blocks, a note, in the
order they first come: from its last parse (ORG-BUFFER-TREE), those of
commented subtrees left out; from its text before there is one."
  (let ((tree (org-buffer-tree buffer))
        (languages '()))
    (flet ((take (name)
             (let ((language (org-src-language (or name ""))))
               (when (and language (language-server-commands language))
                 (pushnew language languages)))))
      (if tree
          (labels ((walk (node)
                     (if (eq (org-node-type node) 'src-block)
                         (unless (org-commented-p node) (take (org-property node :language)))
                         (mapc #'walk (org-node-children node)))))
            (walk tree))
          (dolist (block (org-source-blocks (tuatara.text:rope-string (tuatara.buffer:buffer-text buffer))))
            (take (first block)))))
    (nreverse languages)))

(defun org-block-doc-specs (buffer)
  "What servers see of BUFFER when it is a note with source blocks of a
language a server knows: for each such language, its blocks, as
*LSP-DOC-SPEC-FUNCTIONS* has them; NIL for any other buffer. Worked out from
the note's parse once for each change of what its blocks are (ORG-BLOCKS-KEY),
not again for text typed elsewhere."
  (when (and (typep (buffer-major-mode buffer) 'org-mode) (tuatara.buffer:buffer-file buffer))
    (let ((key (org-blocks-key buffer))
          (kept (tuatara.buffer:buffer-local buffer 'org-block-specs)))
      (if (and kept (equal key (car kept)))
          (cdr kept)
          (let ((specs (loop for language in (org-blocks-languages buffer)
                             collect (let ((language language))
                                       (list language (org-blocks-uri buffer language)
                                             (lambda (b) (org-blocks-shadow b language)))))))
            ;; kept only when read from the parse of the text as it is now
            (when (and (org-buffer-tree buffer) (org-parse-current-p buffer))
              (setf (tuatara.buffer:buffer-local buffer 'org-block-specs) (cons key specs)))
            specs)))))

(defun org-blocks-visit (buffer)
  "As a note is opened: have its changes noted, so that source blocks added
later are seen by their servers too (in a trusted project)."
  (when (and (typep (buffer-major-mode buffer) 'org-mode) (tuatara.buffer:buffer-file buffer)
             (null (lsp-refusal buffer)))
    (pushnew 'lsp-note-change (tuatara.buffer:buffer-change-functions buffer))))

(defun install-org-blocks ()
  "Have notes' source blocks seen by their languages' servers."
  (pushnew 'org-block-doc-specs *lsp-doc-spec-functions*)
  (setf tuatara.buffer:*find-file-functions*
        (append (remove 'org-blocks-visit tuatara.buffer:*find-file-functions*) (list 'org-blocks-visit))))
