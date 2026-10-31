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
;;;; (notes.org.py), a file that is never written. As the note is changed, a
;;;; server is sent only what changed in that code: the blocks a change
;;;; reaches are read again, the others moved with the lines before them
;;;; (ORG-BLOCKS-SHADOW-CHANGES).

(in-package #:tuatara.defaults)

(defun org-source-block-edge (line open)
  "What LINE, a line of a note's text, is to its source blocks: with OPEN true
\(a block begun before it), true when it ends that block; else the name of
the language when it begins one; NIL when it does neither."
  (if open
      (and (cl-ppcre:scan "(?i)^[ \\t]*#\\+end_src\\b" line) t)
      (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "(?i)^[ \\t]*#\\+begin_src[ \\t]+([^ \\t]+)" line)
        (and match (aref groups 0)))))

(defun org-pair-source-blocks (lines)
  "The source blocks of a note whose lines that may begin or end one are
LINES, each (NUMBER . TEXT) in order, as ORG-SOURCE-BLOCKS gives them."
  (let ((blocks '()) (open nil))
    (loop for (n . line) in lines
          do (if open
                 (when (org-source-block-edge line t)
                   (push (list (first open) (second open) n) blocks)
                   (setf open nil))
                 (let ((name (org-source-block-edge line nil)))
                   (when name
                     (setf open (list name (1+ n)))))))
    (nreverse blocks)))

(defun org-source-blocks (text)
  "The source blocks of TEXT, an Org note, in order: each (NAME FIRST LAST),
NAME the language it is given, FIRST and LAST the 0-based lines of its code
\(LAST past it): line for line, as the note's documents need them."
  (org-pair-source-blocks (loop for line in (uiop:split-string text :separator '(#\Newline))
                                for n from 0
                                collect (cons n line))))

(defun org-code-line-reader (buffer line)
  "A function giving, each time it is called, the next line of BUFFER, a note,
from its 0-based LINE on, that may begin or end a source block, as its number
and its text (two values), NIL past the last: those its parts keep as lines
that may say where blocks are (ORG-BUFFER-PARTS). Complexity: O(log of the
text) a line."
  (let* ((parts (org-buffer-parts buffer))
         (rope (tuatara.buffer:buffer-text buffer))
         (from (tuatara.text:line->byte rope (min line (tuatara.text:rope-lines rope))))
         (index (org-part-index parts from))
         (offsets (and (< index (org-parts-count parts)) (org-part-code-lines (org-part-at parts index)))))
    (lambda ()
      (loop
        (cond (offsets
               (let ((at (+ (org-part-start parts index) (pop offsets))))
                 (when (>= at from)
                   (return (values (tuatara.text:byte->line rope at)
                                   (tuatara.text:rope-string rope :start at :end (tuatara.text:rope-line-end rope at)))))))
              ((< (incf index) (org-parts-count parts))
               (setf offsets (org-part-code-lines (org-part-at parts index))))
              (t (return nil)))))))

(defun org-note-source-blocks (buffer)
  "The source blocks of BUFFER, a note, as ORG-SOURCE-BLOCKS gives them of its
text, found among the lines that may begin or end one (ORG-CODE-LINE-READER).
Complexity: O(parts + those lines x log of the text)."
  (let ((next (org-code-line-reader buffer 0)))
    (org-pair-source-blocks (loop for (n line) = (multiple-value-list (funcall next))
                                  while n collect (cons n line)))))

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

(defun org-block-shown (rope first last)
  "The code of the block of ROPE's lines FIRST below LAST as its document
shows it, its indentation and Org's escaping commas left out: a list of the
text of its lines, and a vector of the octets left out at the start of each."
  (let* ((lines (loop for n from first below last collect (rope-line-text rope n)))
         (indent (loop for line in lines
                       unless (zerop (length (string-trim '(#\Space #\Tab) line)))
                         minimize (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line) 0)))
         (skipped (make-array (length lines))))
    (values (loop for line in lines
                  for n from 0
                  for cut = (min (or indent 0) (length line))
                  ;; Org's comma before a line that would read as a headline or a keyword
                  for comma = (let ((rest (subseq line cut)))
                                (if (or (uiop:string-prefix-p ",*" rest) (uiop:string-prefix-p ",#+" rest)) 1 0))
                  do (setf (svref skipped n) (tuatara.text:string-octet-length line :end (+ cut comma)))
                  collect (subseq line (+ cut comma)))
            skipped)))

(defun write-code-lines (lines first at out)
  "Write LINES, the text of lines FIRST on, to OUT, where line AT was the last
written to, the lines between them empty; return the number of the last."
  (loop for line in lines
        for n from first
        do (loop repeat (- n at) do (write-char #\Newline out))
           (write-string line out)
           (setf at n))
  at)

(defun org-blocks-shadow (buffer language &optional doc stretches)
  "The code of LANGUAGE's source blocks of BUFFER, a note, line for line with
it, and the stretches of its lines that are code, each (FIRST LAST OCTETS),
OCTETS the octets left out at the start of each line of LANGUAGE's blocks,
NIL for another's: as ORG-BLOCK-DOC-SPECS's documents show it. Given DOC, the
document as last sent, and STRETCHES, what changed since, what changed in
that code instead (ORG-BLOCKS-SHADOW-CHANGES). Complexity: O(blocks, their
code and the lines of the note)."
  (if doc
      (org-blocks-shadow-changes buffer language doc stretches)
      (let* ((rope (tuatara.buffer:buffer-text buffer))
             (lines (1+ (tuatara.text:rope-lines rope)))
             (at 0) (kept '()))
        (values (with-output-to-string (out)
                  (loop for (name first last) in (org-note-source-blocks buffer)
                        do (if (eq (org-src-language name) language)
                               (multiple-value-bind (shown skipped) (org-block-shown rope first last)
                                 (setf at (write-code-lines shown first at out))
                                 (push (list first last skipped) kept))
                               (push (list first last nil) kept)))
                  (loop repeat (- lines 1 at) do (write-char #\Newline out)))
                (coerce (nreverse kept) 'simple-vector)))))

(defun stretches-lines (stretches before after)
  "STRETCHES, as CHANGED-STRETCHES gives them, as the lines they are on: a
list in order of (OLD-FIRST OLD-LAST NEW-FIRST NEW-LAST), 0-based lines of
BEFORE, the text then, and of AFTER, the text now, the last of each in it;
stretches on a line in common made one."
  (let ((lines '()))
    (loop for (old-start old-end new-start new-end) in stretches
          do (let ((line (list (tuatara.text:byte->line before old-start) (tuatara.text:byte->line before old-end)
                               (tuatara.text:byte->line after new-start) (tuatara.text:byte->line after new-end)))
                   (last (first lines)))
               (if (and last (<= (first line) (second last)))
                   (setf (second last) (second line) (fourth last) (fourth line))
                   (push line lines))))
    (nreverse lines)))

(defun org-blocks-read-again (buffer old lines)
  "The source blocks of BUFFER, a note, now, from OLD, those of its text as
last sent (each (FIRST LAST ...) as ORG-BLOCKS-SHADOW keeps them), and LINES,
the lines changed since (STRETCHES-LINES): a vector in order of OLD's
blocks moved by the lines added and taken away before them, each
\(FIRST LAST OCTETS), and of those read again, each (FIRST LAST NIL NAME).
Only the lines that may begin or end a block are read, from the block a
change begins in until a block begins where one began before. Complexity:
O(blocks, and the lines read again)."
  (let* ((count (length old))
         (out (make-array count :fill-pointer 0 :adjustable t))
         (i 0) (shift 0))
    (flet ((keep-before (line)
             ;; OLD's blocks ending before LINE of the text then, moved
             (loop while (and (< i count) (< (second (svref old i)) line))
                   do (destructuring-bind (first last octets &rest name) (svref old i)
                        (declare (ignore name))
                        (vector-push-extend (if (zerop shift) (svref old i) (list (+ first shift) (+ last shift) octets)) out))
                      (incf i))))
      (loop while lines
            do (destructuring-bind (old-first old-last new-first new-last) (pop lines)
                 (declare (ignore new-first))
                 (let* ((k (+ i (tuatara.runtime:bisect (mid 0 (- count i)) (< (second (svref old (+ i mid))) old-first))))
                        ;; read from the line beginning the block it is in, or from it outside one,
                        ;; or after the last block when it is past it
                        (start (cond ((and (< k count) (<= (1- (first (svref old k))) old-first)) (1- (first (svref old k))))
                                     ((< k count) old-first)
                                     ((plusp count) (1+ (second (svref old (1- count)))))
                                     (t 0)))
                        (next (progn (keep-before start)
                                     (org-code-line-reader buffer (+ start shift))))
                        (covered new-last) (open nil))
                   (setf shift (- new-last old-last))
                   (loop
                     (multiple-value-bind (n text) (funcall next)
                       (unless n
                         ;; read to the end: none of OLD's blocks after where it began stays
                         (setf i count lines '())
                         (return))
                       ;; the changes the reading has come to, taken in
                       (loop while (and lines (<= (third (first lines)) n))
                             do (destructuring-bind (a0 a1 b0 b1) (pop lines)
                                  (declare (ignore a0 b0))
                                  (setf covered b1 shift (- b1 a1))))
                       (if open
                           (when (org-source-block-edge text t)
                             (vector-push-extend (list (second open) n nil (first open)) out)
                             (setf open nil))
                           (let ((name (org-source-block-edge text nil)))
                             (when name
                               ;; a block beginning past the changes where one began before: the
                               ;; rest as it was
                               (let ((again (and (> n covered)
                                                 (let* ((line (- n shift))
                                                        (j (+ i (tuatara.runtime:bisect (mid 0 (- count i))
                                                                                        (< (first (svref old (+ i mid))) (1+ line))))))
                                                   (and (< j count) (= (first (svref old j)) (1+ line)) j)))))
                                 (when again
                                   (setf i again)
                                   (return)))
                               (setf open (list name (1+ n)))))))))))
      (keep-before most-positive-fixnum))
    out))

(defun widen-to-blocks (lines old new read-again)
  "LINES, as STRETCHES-LINES gives them, with the lines of READ-AGAIN, blocks
of NEW read again (ORG-BLOCKS-READ-AGAIN), each widened until it holds whole
every block of OLD, the blocks as last sent, and of NEW, those now, that it
reaches into: from the line beginning one to the line ending it, then and
now alike; those that come to meet made one."
  (let ((changed (coerce lines 'simple-vector)))
    (labels ((place (line from)
               ;; LINE of the text then (FROM 0) or now (FROM 2) as (OLD-FIRST OLD-LAST NEW-FIRST
               ;; NEW-LAST): the lines changed it is in, whole, or where it is in each
               (let* ((at (1- (tuatara.runtime:bisect (mid 0 (length changed)) (<= (nth from (svref changed mid)) line))))
                      (span (and (>= at 0) (svref changed at))))
                 (cond ((null span) (list line line line line))
                       ((<= line (nth (1+ from) span)) (copy-list span))
                       (t (let ((other (+ line (- (nth (- 3 from) span) (nth (1+ from) span)))))
                            (if (zerop from) (list line line other other) (list other other line line)))))))
             (span-of (begin end from)
               (let ((low (place begin from)) (high (place end from)))
                 (list (first low) (second high) (third low) (fourth high))))
             (widen (line blocks from)
               ;; LINE grown over the blocks of BLOCKS, of the text FROM says, that reach into it
               (let* ((low (nth from line))
                      (high (nth (1+ from) line))
                      (at (tuatara.runtime:bisect (mid 0 (length blocks)) (< (second (svref blocks mid)) low)))
                      (grown nil))
                 (loop for i from at below (length blocks)
                       for block = (svref blocks i)
                       while (<= (1- (first block)) high)
                       do (let ((span (span-of (1- (first block)) (second block) from)))
                            (when (or (< (first span) (first line)) (> (second span) (second line)))
                              (setf (first line) (min (first line) (first span))
                                    (third line) (min (third line) (third span))
                                    (second line) (max (second line) (second span))
                                    (fourth line) (max (fourth line) (fourth span))
                                    grown t))))
                 grown))
             (merged (lines)
               ;; in order, those that meet made one
               (loop with out = '()
                     for line in (sort lines #'< :key #'first)
                     for last = (first out)
                     do (if (and last (<= (first line) (second last)))
                            (setf (second last) (max (second last) (second line))
                                  (fourth last) (max (fourth last) (fourth line)))
                            (push line out))
                     finally (return (nreverse out)))))
      (let ((lines (merged (append (mapcar #'copy-list lines)
                                   (loop for (first last) in read-again collect (span-of (1- first) last 2))))))
        (loop
          (let ((grown nil))
            (dolist (line lines)
              (when (widen line old 0) (setf grown t))
              (when (widen line new 2) (setf grown t)))
            (let ((count (length lines)))
              (setf lines (merged lines))
              (unless (or grown (/= count (length lines)))
                (return lines)))))))))

(defun org-blocks-shadow-changes (buffer language doc stretches)
  "What changed in the code of LANGUAGE's source blocks of BUFFER, a note,
since DOC, its document, was last sent, STRETCHES the stretches of the note
changed since (CHANGED-STRETCHES): as DOC-CHANGES gives them, and the
stretches of its lines that are code now, as ORG-BLOCKS-SHADOW gives them.
Only the lines changed are read again, with every block they reach into
whole (ORG-BLOCKS-READ-AGAIN, WIDEN-TO-BLOCKS). Complexity: O(blocks, and
the lines of the blocks changed)."
  (let* ((before (lsp-doc-sent-rope doc))
         (after (tuatara.buffer:buffer-text buffer))
         (shown (lsp-doc-rope doc))
         (old (lsp-doc-skipped doc))
         (spans (stretches-lines stretches before after))
         (new (coerce (org-blocks-read-again buffer old spans) 'simple-vector))
         (lines (widen-to-blocks spans old new (loop for block across new when (fourth block) collect block)))
         (ours (make-hash-table :test 'equal))
         (changes '()))
    (flet ((ours-p (block)
             (destructuring-bind (first last octets &optional name) block
               (declare (ignore first last))
               (if (null name)
                   (and octets t)
                   (multiple-value-bind (known found) (gethash name ours)
                     (if found known (setf (gethash name ours) (eq (org-src-language name) language))))))))
      (dolist (line lines)
        (destructuring-bind (old-first old-last new-first new-last) line
          ;; the code of these lines now, the blocks in them read again
          (let* ((j (tuatara.runtime:bisect (mid 0 (length new)) (< (first (aref new mid)) new-first)))
                 (text (with-output-to-string (out)
                         (let ((at new-first))
                           (loop while (and (< j (length new)) (<= (first (aref new j)) new-last))
                                 do (let ((block (aref new j)))
                                      (destructuring-bind (first last &rest rest) block
                                        (declare (ignore rest))
                                        (setf (aref new j)
                                              (if (ours-p block)
                                                  (multiple-value-bind (code skipped) (org-block-shown after first last)
                                                    (setf at (write-code-lines code first at out))
                                                    (list first last skipped))
                                                  (list first last nil)))))
                                    (incf j))
                           (loop repeat (- new-last at) do (write-char #\Newline out)))))
                 (change (trimmed-change shown (tuatara.text:line->byte shown old-first) (line-end-byte shown old-last) text)))
            (when change (push change changes))))))
    (values (coerce (nreverse changes) 'simple-vector) new)))

(defun trimmed-change (rope start end text)
  "The change of ROPE's octets START below END for the string TEXT, as
DOC-CHANGES gives one, less what the two begin and end with alike; NIL when
they are the same."
  (let* ((old (tuatara.text:rope-string rope :start start :end end))
         (head (or (mismatch old text) (return-from trimmed-change nil)))
         (tail (- (length old) (mismatch old text :from-end t)))
         (tail (min tail (- (length old) head) (- (length text) head))))
    (list (+ start (tuatara.text:string-octet-length old :end head))
          (- end (tuatara.text:string-octet-length old :start (- (length old) tail)))
          (subseq text head (- (length text) tail)))))

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

(defun org-block-specs-key (buffer)
  "What ORG-BLOCK-DOC-SPECS's answer for BUFFER, a note, depends on, as data
to compare: its blocks (ORG-BLOCKS-KEY) and the files open in buffers, as
a block written to a file open in one is seen by the note's own name."
  (cons (sort (loop for other in tuatara.buffer:*buffers*
                    for file = (tuatara.buffer:buffer-file other)
                    when file collect (namestring file))
              #'string<)
        (org-blocks-key buffer)))

(defun org-block-doc-specs (buffer)
  "What servers see of BUFFER when it is a note with source blocks of a
language a server knows: for each such language, its blocks, as
*LSP-DOC-SPEC-FUNCTIONS* has them; NIL for any other buffer. Worked out from
the note's parse once for each change of what its blocks are (ORG-BLOCKS-KEY),
not again for text typed elsewhere; again when the files open change
\(ORG-BLOCK-SPECS-KEY)."
  (when (and (typep (buffer-major-mode buffer) 'org-mode) (tuatara.buffer:buffer-file buffer))
    (let ((key (org-block-specs-key buffer))
          (kept (tuatara.buffer:buffer-local buffer 'org-block-specs)))
      (if (and kept (equal key (car kept)))
          (cdr kept)
          (let ((specs (loop for language in (org-blocks-languages buffer)
                             collect (let ((language language))
                                       (list language (org-blocks-uri buffer language)
                                             (lambda (b &optional doc stretches) (org-blocks-shadow b language doc stretches)))))))
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
