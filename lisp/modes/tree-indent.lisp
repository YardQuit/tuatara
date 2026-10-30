;;;; lisp/modes/tree-indent.lisp --- Indenting by the syntax tree, as Helix does.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A language with indentation rules (syntax/indents/) is indented by its
;;;; syntax tree as Helix indents a line: one level for each node the rules
;;;; mark @indent that holds the line and begins on a line before it (nodes
;;;; beginning on the same line count once), and one level less for each node
;;;; marked @outdent that begins the line. @align lines a line up under the
;;;; @anchor of its rule, and the lines inside a node marked @opaque keep
;;;; their blanks. A node marked @extend reaches on to the line after it when
;;;; that line is begun right after it or is indented deeper than it begins
;;;; (a block of Python goes on while its lines do), unless a node marked
;;;; @extend.prevent-once ends there (a return).

(in-package #:tuatara.defaults)

(defun indent-captures (query tree rope start end)
  "What QUERY, a language's indentation rules, captures in TREE (the tree of
ROPE) touching octets START below END: a table from each node captured, as a
list (START END SYMBOL), to the lines it begins and ends on, NIL until they
are needed, and what it is captured as, (FIRST-LINE LAST-LINE . WHAT), each of
WHAT one of (:INDENT
HEADER), (:INDENT-ALWAYS HEADER), (:OUTDENT), (:OUTDENT-ALWAYS), (:ALIGN
ANCHOR), ANCHOR being the octet its lines line up under, (:OPAQUE), (:EXTEND)
or (:EXTEND-PREVENT-ONCE). HEADER is true when the rule has the node's lines
begin at its parent's first line."
  (let ((table (make-hash-table :test 'equal))
        (names (tuatara.syntax:query-captures query))
        (properties (tuatara.syntax:query-properties query)))
    (tuatara.syntax:map-query-matches
     (lambda (pattern captures count)
       (let ((header (equal (getf (aref properties pattern) :scope) "header"))
             (anchor (loop for i from 0 below count
                           when (string= (aref names (aref captures (* 4 i))) "anchor")
                             return (aref captures (+ 1 (* 4 i))))))
         (dotimes (i count)
           (let* ((name (aref names (aref captures (* 4 i))))
                  (what (cond ((string= name "indent") (list :indent header))
                              ((string= name "indent.always") (list :indent-always header))
                              ((string= name "outdent") (list :outdent))
                              ((string= name "outdent.always") (list :outdent-always))
                              ((string= name "align") (and anchor (list :align anchor)))
                              ((string= name "opaque") (list :opaque))
                              ((string= name "extend") (list :extend))
                              ((string= name "extend.prevent-once") (list :extend-prevent-once)))))
             (when what
               (let* ((key (list (aref captures (+ 1 (* 4 i))) (aref captures (+ 2 (* 4 i)))
                                 (aref captures (+ 3 (* 4 i)))))
                      (entry (or (gethash key table) (setf (gethash key table) (list nil nil)))))
                 (push what (cddr entry))))))))
     query tree rope start end)
    table))

(defun text-column (rope position moved)
  "The column of POSITION in ROPE, a tab to the next tab stop, its line
taken as indented when MOVED, the table of lines indented before it in the
same pass (or NIL), has it: each line's start to (COLUMN . WIDTH), the column
its text moves to and the octets of blanks it begins with now."
  (let* ((start (tuatara.text:rope-line-start rope position))
         (move (and moved (gethash start moved))))
    (text-columns (tuatara.text:rope-string rope :start (if move (min position (+ start (cdr move))) start)
                                                   :end position)
                   (if move (car move) 0))))

(defun line-indent-column (rope line)
  "The column the text of line number LINE of ROPE begins at."
  (nth-value 2 (rope-line-blanks rope (tuatara.text:line->byte rope line))))

(defun extended-nodes (syntax rope position target chain captures)
  "The nodes that reach on to line TARGET, whose text begins at octet POSITION
of ROPE, by the indentation rules' CAPTURES marking them @extend: the node
ending before POSITION inside the innermost node of CHAIN (the nodes holding
POSITION, innermost first) and those around it up to that innermost node,
from the one marked @extend that reaches, outwards; NIL when none reaches. It
reaches when its text ends on the line before TARGET that was just typed after
(*TYPED-LINE*), or when that line, else line TARGET, is indented deeper than
the node's first line; a node marked @extend.prevent-once passed on the way
up stops the next one reaching (a block does not go on after its return)."
  (let* ((inner (first chain))
         (tree (tuatara.syntax:syntax-tree syntax))
         (before (and inner
                      (find-if (lambda (child) (<= (cdr child) position))
                               (tuatara.syntax:node-child-ranges tree (tuatara.syntax:tree-node-start inner)
                                                                  (tuatara.syntax:tree-node-end inner)
                                                                  (tuatara.syntax:tree-node-symbol inner)
                                                                  :to position)
                               :from-end t)))
         ;; the end of its text, without the blanks and line ends it may close on
         (last (and before (tuatara.text:rope-skip-octets rope (cdr before) tuatara.text:*blank-octets*
                                                           :backward t :bound (car before)))))
    (flet ((key (node) (list (tuatara.syntax:tree-node-start node) (tuatara.syntax:tree-node-end node)
                             (tuatara.syntax:tree-node-symbol node))))
      (when (and last (< (car before) last))
        (let* ((up (tuatara.syntax:node-ancestry syntax (1- last) last))
               (stop (member (key inner) up :key #'key :test #'equal))
               (line (if *typed-line* (1- target) target))
               (ends (tuatara.text:byte->line rope (1- last)))
               (indent nil)
               (prevented nil))
          (loop for rest on up
                for node = (first rest)
                until (eq rest stop)
                do (let ((what (cddr (gethash (key node) captures))))
                     (when (assoc :extend-prevent-once what) (setf prevented t))
                     (when (assoc :extend what)
                       (if prevented
                           (setf prevented nil)
                           (when (or (= ends line)
                                     (> (or indent (setf indent (line-indent-column rope line)))
                                        (line-indent-column rope (tuatara.text:byte->line
                                                                  rope (tuatara.syntax:tree-node-start node)))))
                             (return (ldiff rest stop))))))))))))

(defun leaf-node-p (syntax node)
  "True when NODE of SYNTAX's tree has no parts at all, not even a bracket: a
comment, a string's text."
  (null (tuatara.syntax:node-child-ranges (tuatara.syntax:syntax-tree syntax)
                                           (tuatara.syntax:tree-node-start node) (tuatara.syntax:tree-node-end node)
                                           (tuatara.syntax:tree-node-symbol node))))

(defun tree-line-column (syntax rope position target captures unit moved)
  "The column the line whose text begins at octet POSITION of ROPE, line
TARGET, goes to by the indentation rules' CAPTURES in SYNTAX's tree, one level
being UNIT columns; NIL to leave the line as it is, inside a node whose lines
keep their blanks, or inside a comment or string begun on a line before, whose
blanks are its text. MOVED is the table of lines indented before it in the
same pass."
  (let* ((levels 0) (always 0) (outdents 0) (outdents-always 0)
         (align nil) (counted '())
         (line-start (tuatara.text:rope-line-start rope position))
         (holder (first (tuatara.syntax:node-ancestry syntax line-start line-start)))
         (before (and (> line-start 0) (first (tuatara.syntax:node-ancestry syntax (1- line-start) line-start))))
         (chain (tuatara.syntax:node-ancestry syntax position position))
         (extended (extended-nodes syntax rope position target chain captures)))
    ;; a line whose blanks lie inside a comment or a string (a node of no parts
    ;; of its own, begun on a line before) keeps them: they are its text; so
    ;; does a line a string's text runs up to (its closing quotes begin it)
    (when (or (and holder (< (tuatara.syntax:tree-node-start holder) line-start)
                   (leaf-node-p syntax holder))
              (and before (= (tuatara.syntax:tree-node-end before) line-start)
                   (leaf-node-p syntax before)
                   (< (tuatara.syntax:tree-node-start before) (tuatara.text:rope-line-start rope (1- line-start)))
                   (not (search "comment" (tuatara.syntax:tree-node-name before)))))
      (return-from tree-line-column nil))
    (loop for (node . parents) on (append extended chain)
          for entry = (gethash (list (tuatara.syntax:tree-node-start node) (tuatara.syntax:tree-node-end node)
                                     (tuatara.syntax:tree-node-symbol node))
                               captures)
          when entry
            do (let* ((start (tuatara.syntax:tree-node-start node))
                      (first-line (or (first entry) (setf (first entry) (tuatara.text:byte->line rope start))))
                      (last-line (or (second entry)
                                     (setf (second entry)
                                           (tuatara.text:byte->line rope (tuatara.syntax:tree-node-end node)))))
                      (what (cddr entry))
                      ;; every rule that captured the node counts, each with its own scope
                      (indents (remove :indent what :key #'first :test-not #'eq))
                      (indents-always (remove :indent-always what :key #'first :test-not #'eq))
                      (outdent (assoc :outdent what))
                      (outdent-always (assoc :outdent-always what))
                      (aligning (assoc :align what))
                      (parent-line nil))
                 (flet ((opening (header)
                          ;; the line the node's lines begin after: its own first, or with
                          ;; the header scope its parent's
                          (if (and header parents)
                              (or parent-line
                                  (setf parent-line (tuatara.text:byte->line
                                                     rope (tuatara.syntax:tree-node-start (first parents)))))
                              first-line))
                        (inside (opening) (and (< opening target)
                                               ;; a node reaching on to the line holds it
                                               (or (<= target last-line) (member node extended)))))
                   (when (and (assoc :opaque what) (<= start position) (< position (tuatara.syntax:tree-node-end node))
                              (< first-line target))
                     (return-from tree-line-column nil))
                   ;; a node both indenting and outdenting does neither
                   (unless (and (or indents indents-always) (or outdent outdent-always))
                     (dolist (indent indents)
                       (let ((opening (opening (second indent))))
                         (when (and (inside opening) (not (member opening counted)))
                           (push opening counted)
                           (incf levels))))
                     (dolist (indent indents-always)
                       (when (inside (opening (second indent))) (incf always)))
                     (when (and outdent (= first-line target)) (incf outdents))
                     (when (and outdent-always (= first-line target)) (incf outdents-always)))
                   ;; the innermost alignment
                   (when (and aligning (null align)
                              (inside (opening (some #'second (append indents indents-always)))))
                     (setf align (second aligning))))))
    (when align (setf levels 0 always 0))
    (max 0 (+ (if align (text-column rope align moved) 0)
              (* unit (- (+ levels always) outdents outdents-always))))))

(defun tree-indent-lines (buffer language start end &optional blank-lines)
  "How LANGUAGE's indentation rules indent the lines of BUFFER from the one
holding octet START to the one holding END, as MODE-INDENT-LINES gives it: the
lines whose indentation changes, blank lines left alone unless BLANK-LINES;
lines inside a node whose lines keep their blanks stay as they are. NIL, said
in *Messages*, when the buffer has no syntax tree."
  (let* ((rope (tuatara.buffer:buffer-text buffer))
         (syntax (ready-syntax buffer))
         (tree (and syntax (tuatara.syntax:syntax-tree syntax))))
    (if (null tree)
        (progn (message "No syntax tree to indent by") '())
        (let* ((first (tuatara.text:rope-line-start rope start))
               (stop (tuatara.text:rope-line-start rope end))
               ;; from the text before the first line, which may reach on to it (@extend)
               (from (let* ((low (max 0 (- first 4096)))
                            (text (tuatara.text:rope-octets rope :start low :end first))
                            (last (position-if-not (lambda (octet) (member octet tuatara.text:*blank-octets*)) text :from-end t)))
                       (if last (+ low last) first)))
               (captures (indent-captures (language-compiled-query language :indent) tree rope from
                                          (tuatara.text:rope-line-end rope stop)))
               (moved (make-hash-table))
               (changes '()))
          (loop for line = first then (1+ line-end)
                for line-end = (tuatara.text:rope-line-end rope line)
                for number from (tuatara.text:byte->line rope first)
                do (multiple-value-bind (width blank current) (rope-line-blanks rope line)
                     (unless (and blank (not blank-lines))
                       (let ((column (tree-line-column syntax rope (+ line width) number captures
                                                       (language-indent language) moved)))
                         (when (and column (/= column current))
                           (setf (gethash line moved) (cons column width))
                           (push (cons line column) changes)))))
                until (>= line stop))
          (nreverse changes)))))

(defmethod mode-indent-lines ((mode fundamental-mode) buffer start end &optional blank-lines)
  "How the lines of BUFFER from START to END are indented by its language's
rules; :NONE without them."
  (let ((language (mode-language mode)))
    (if (and language (language-compiled-query language :indent))
        (tree-indent-lines buffer language start end blank-lines)
        :none)))
