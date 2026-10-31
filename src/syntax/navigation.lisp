;;;; src/syntax/navigation.lisp --- Moving by the syntax tree: expressions and brackets.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An expression is what the grammar makes of the text: a word, or a construct
;;;; opened by a bracket or by punctuation (a quote, a string's quote mark) that
;;;; goes as one; comments are passed over as blank space is. The brackets
;;;; around a place are found as Emacs finds them, by counting brackets in the
;;;; text, but the tree says which are in strings, comments and character
;;;; literals, so those do not count.

(in-package #:tuatara.syntax)

(defconstant +deepest+ 1024
  "The most nodes around a place looked at, from the innermost out.")

(defun node-ancestry (syntax start end)
  "The smallest node of SYNTAX's tree spanning octets START below END, then
each node around it out to the root, as a list of TREE-NODE; NIL when there
is no tree."
  (let ((tree (syntax-tree syntax))
        (symbols (grammar-symbols (syntax-grammar syntax))))
    (when tree
      (with-call (call (tree-helper tree) +op-ancestry+)
        (call-u32 call (tree-number tree)) (call-u32 call start) (call-u32 call end)
        (when (eql 0 (call-send call))
          ;; each node around the one before it, all within the text
          (loop with bytes = (tuatara.text:rope-bytes (syntax-rope syntax))
                with low = most-positive-fixnum and high = -1
                for i from 0 below (min +deepest+ (floor (call-in-size call) 20))
                collect (flet ((at (k) (reply-u32 call (+ (* 5 i) k))))
                          (unless (and (<= (at 0) (min low (at 1))) (<= (max high (at 0)) (at 1) bytes))
                            (reply-overrun call))
                          (setf low (at 0) high (at 1))
                          (make-tree-node (at 0) (at 1)
                                          ;; the symbols past the grammar's own are tree-sitter's errors
                                          (if (< (at 2) (length symbols)) (aref symbols (at 2)) "ERROR")
                                          (logbitp 0 (at 3)) (at 4) (logbitp 1 (at 3)) (at 2)))))))))

(defun select-node (syntax start end how)
  "The octets a named node of SYNTAX's tree starts and ends at, as two values,
for a selection of octets START below END, by HOW: :SMALLEST, the smallest
named node spanning them; :LARGER, the smallest spanning more than them;
:FIRST-CHILD, the first named child of the smallest; :NEXT or :PREVIOUS, the
next or previous named sibling of the smallest, or when it has none, that of
the nearest node around it that has one. NIL when there is no tree or no such
node."
  (let ((tree (syntax-tree syntax)))
    (when tree
      (with-call (call (tree-helper tree) +op-select+)
        (call-u32 call (tree-number tree)) (call-u32 call start) (call-u32 call end)
        (call-u32 call (ecase how (:smallest 0) (:larger 1) (:first-child 2) (:next 3) (:previous 4)))
        (when (and (eql 0 (call-send call)) (= 1 (reply-u32 call 0)))
          (let ((from (reply-u32 call 1)) (to (reply-u32 call 2)))
            (unless (<= from to (tuatara.text:rope-bytes (syntax-rope syntax)))
              (reply-overrun call))
            (values from to)))))))

;;; Kinds of node

(defun opening-bracket-node-p (node)
  "True when NODE is a literal token of opening brackets only: ( [ { or [[ and the like."
  (let ((name (tree-node-name node)))
    (and (not (tree-node-named node)) (plusp (length name))
         (every (lambda (c) (find c tuatara.text:*openers*)) name))))

(defun closing-bracket-node-p (node)
  "True when NODE is a literal token of closing brackets only: ) ] } or ]] and the like."
  (let ((name (tree-node-name node)))
    (and (not (tree-node-named node)) (plusp (length name))
         (every (lambda (c) (find c tuatara.text:*closers*)) name))))

(defun comment-type-p (name)
  "True when node type NAME is a comment's."
  (search "comment" name))

(defun string-type-p (name)
  "True when node type NAME is that of a string or character literal, taken as one piece of text."
  (and (cl-ppcre:scan "string|str_lit|char_lit|char_literal|heredoc_body|fancy_literal|regex" name) t))

(defun comment-node-p (node)
  "True when NODE is a comment."
  (and (tree-node-named node) (comment-type-p (tree-node-name node))))

(defun string-node-p (node)
  "True when NODE is a string or character literal."
  (and (tree-node-named node) (string-type-p (tree-node-name node))))

(defun punctuation-node-p (node)
  "True when NODE is a literal token with no letter or digit in it."
  (and (not (tree-node-named node)) (notany #'alphanumericp (tree-node-name node))))

(defparameter *separating-octets* '(32 9 10 13 12)
  "The octets that separate expressions: a space, a tab, a newline, a carriage
return and a form feed.")

(defun whitespace-octet-p (octet)
  "True for an octet that separates expressions (*SEPARATING-OCTETS*)."
  (member octet *separating-octets*))

(defun compact-text-p (rope start end)
  "True when the octets of ROPE from START below END are few and hold no blank,
bracket or quote mark: text that reads as part of one expression."
  (and (<= (- end start) 256)
       (loop for i from start below end
             for o = (tuatara.text:rope-octet rope i)
             never (or (whitespace-octet-p o) (member o tuatara.text:*bracket-octets*) (= o 34)))))

(defun prefix-text-p (rope start end)
  "True when the octets of ROPE from START below END are a few marks put before
an expression (Lisp's quote and #', Rust's &): no blank, bracket, letter or digit."
  (and (< start end) (<= (- end start) 8)
       (loop for i from start below end
             for o = (tuatara.text:rope-octet rope i)
             never (or (whitespace-octet-p o) (member o tuatara.text:*bracket-octets*) (= o 34)
                       (<= 48 o 57) (<= 65 o 90) (<= 97 o 122) (= o 95)))))

;;; Expressions

(defun expression-end (syntax position)
  "Where the expression after POSITION in SYNTAX's text ends: the octet after
it; NIL when a closing bracket comes first, POSITION being at the end of the
expression around it; :UNBALANCED when an opening bracket comes first whose
partner is not found; :TEXT when POSITION is inside a string or comment, whose
text has no expressions, only words. Comments and punctuation between
expressions are passed over."
  (let* ((rope (syntax-rope syntax))
         (size (tuatara.text:rope-bytes rope))
         (p position))
    (loop
      (setf p (tuatara.text:rope-skip-octets rope p *separating-octets*))
      (when (>= p size) (return size))
      (let* ((chain (node-ancestry syntax p (tuatara.text:rope-next-boundary rope p)))
             (leaf (first chain))
             (above (butlast (rest chain)))  ; the root is never an expression
             (comment (find-if #'comment-node-p chain))
             (inside-string (find-if (lambda (n) (and (string-node-p n) (< (tree-node-start n) p))) chain))
             (string (find-if (lambda (n) (and (string-node-p n) (= (tree-node-start n) p))) chain :from-end t)))
        (cond ((null leaf) (return nil))
              (comment (if (< (tree-node-start comment) p)
                           (return :text)
                           (setf p (max (1+ p) (tree-node-end comment)))))
              (inside-string (return :text))
              (string (return (tree-node-end string)))
              ((closing-bracket-node-p leaf) (return nil))
              ((opening-bracket-node-p leaf)
               ;; the partner of a token's first bracket closes the whole of [[
               (let ((end (scan-closer-after (make-bracket-scan syntax p) p)))
                 (return (or (and end (max end (tree-node-end leaf))) :unbalanced))))
              ((punctuation-node-p leaf)
               ;; punctuation that begins a construct takes it along; other punctuation is passed over
               (let ((parent (first above)))
                 (if (and parent (= (tree-node-start parent) p))
                     (return (tree-node-end parent))
                     (setf p (max (1+ p) (tree-node-end leaf))))))
              (t
               ;; a word, and what is joined to it without a break (pkg:name, a.b)
               (let ((x leaf))
                 (loop for parent in above
                       while (and (= (tree-node-start parent) (tree-node-start x))
                                  (compact-text-p rope (tree-node-end x) (tree-node-end parent)))
                       do (setf x parent))
                 (return (tree-node-end x)))))))))

(defun expression-start (syntax position)
  "Where the expression before POSITION in SYNTAX's text starts: its first
octet; NIL when an opening bracket comes first, POSITION being at the start
of the expression around it; :UNBALANCED when a closing bracket comes first
whose partner is not found; :TEXT when POSITION is inside a string or
comment. Comments and punctuation between expressions are passed over, and
marks put before an expression (a quote) go with it."
  (let* ((rope (syntax-rope syntax))
         (p position))
    (loop
      (let ((from p))
       (setf p (tuatara.text:rope-skip-octets rope p *separating-octets* :backward t))
       (when (<= p 0) (return 0))
       (let* ((before (tuatara.text:rope-previous-boundary rope p))
             (chain (node-ancestry syntax before p))
             (leaf (first chain))
             (above (butlast (rest chain)))
             (comment (find-if #'comment-node-p chain))
             (inside-string (find-if (lambda (n) (and (string-node-p n) (< (tree-node-start n) from (tree-node-end n)))) chain))
             (string (find-if (lambda (n) (and (string-node-p n) (<= p (tree-node-end n) from))) chain :from-end t)))
        (flet ((with-prefixes (x &optional (chain chain))
                 ;; marks before X that belong to it, as a quote does
                 (loop for parent in (member x chain)
                       while (and (= (tree-node-end parent) (tree-node-end x))
                                  (not (eq parent (car (last chain))))
                                  (or (= (tree-node-start parent) (tree-node-start x))
                                      (prefix-text-p rope (tree-node-start parent) (tree-node-start x))))
                       do (setf x parent))
                 (tree-node-start x)))
          (cond ((null leaf) (return nil))
                (comment (if (< (tree-node-start comment) from (tree-node-end comment))
                             (return :text)
                             (setf p (min (1- p) (tree-node-start comment)))))
                (inside-string (return :text))
                (string (return (with-prefixes string)))
                ((opening-bracket-node-p leaf) (return nil))
                ((closing-bracket-node-p leaf)
                 (let ((open (scan-opener-of (make-bracket-scan syntax before) (1- (tree-node-end leaf)))))
                   ;; the partner of a token's last bracket opens the whole of ]]
                   (return
                     (if open
                         ;; with the marks before it that belong to it: the node the
                         ;; brackets end, if one starts at the opening bracket or before
                         (let* ((around (node-ancestry syntax open (tree-node-end leaf)))
                                (node (find-if (lambda (n) (and (<= (tree-node-start n) open)
                                                                (= (tree-node-end n) (tree-node-end leaf))))
                                               around)))
                           (if (and node (or (= (tree-node-start node) open)
                                             (prefix-text-p rope (tree-node-start node) open)))
                               (with-prefixes node around)
                               open))
                         :unbalanced))))
                ((punctuation-node-p leaf)
                 (setf p (min (1- p) (tree-node-start leaf))))
                (t
                 (let ((x leaf))
                   (loop for parent in above
                         while (and (= (tree-node-end parent) (tree-node-end x))
                                    (compact-text-p rope (tree-node-start parent) (tree-node-start x)))
                         do (setf x parent))
                   (return (tree-node-start x)))))))))))

;;; Strings and comments, and the brackets around a place

(defun spans-query (grammar)
  "GRAMMAR's query capturing its strings, character literals and comments as
@span, compiled when first needed; NIL when it has none."
  (or (grammar-spans-query grammar)
      (setf (grammar-spans-query grammar)
            (let ((names (remove-duplicates
                          (loop for name across (grammar-symbols grammar)
                                for named across (grammar-named grammar)
                                when (and named (or (comment-type-p name) (string-type-p name)))
                                  collect name)
                          :test #'string=)))
              (and names
                   (compile-query grammar (format nil "[~{(~a)~^ ~}] @span" names) "string and comment"))))))

(defun opaque-spans (syntax start end)
  "The strings, character literals and comments of SYNTAX's text touching
octets START below END, as (START . END) ranges in order, those inside
others left out: text whose brackets do not count."
  (let ((query (spans-query (syntax-grammar syntax)))
        (spans '()))
    (when (and query (syntax-tree syntax))
      (map-query-matches (lambda (pattern captures count)
                           (declare (ignore pattern))
                           (dotimes (i count)
                             (push (cons (aref captures (+ 1 (* 4 i))) (aref captures (+ 2 (* 4 i)))) spans)))
                         query (syntax-tree syntax) (syntax-rope syntax) start end))
    (let ((sorted (sort spans (lambda (a b) (or (< (car a) (car b)) (and (= (car a) (car b)) (> (cdr a) (cdr b)))))))
          (kept '()))
      (dolist (span sorted (nreverse kept))
        (unless (and kept (<= (cdr span) (cdr (first kept))))
          (push span kept))))))

(defparameter *bracket-pairs* (mapcar #'cons tuatara.text:*opening-octets* tuatara.text:*closing-octets*)
  "The octets of the brackets that pair: ( ), [ ] and { }.")

(defconstant +bracket-reach+ (* 256 1024)
  "The most octets looked through for a bracket's partner, or for the brackets around a place.")

(defstruct (bracket-scan (:constructor %make-bracket-scan (rope spans from to)) (:copier nil))
  "What looking for brackets in ROPE between octets FROM and TO needs: SPANS,
the strings, character literals and comments there, whose brackets do not count."
  (rope nil :read-only t)
  (spans #() :type simple-vector :read-only t)
  (from 0 :type fixnum :read-only t)
  (to 0 :type fixnum :read-only t))

(setf (documentation 'bracket-scan-p 'function) "True when OBJECT is a BRACKET-SCAN."
      (documentation 'bracket-scan-rope 'function) "The text looked through."
      (documentation 'bracket-scan-spans 'function) "The ranges whose brackets do not count, in order."
      (documentation 'bracket-scan-from 'function) "The first octet looked at."
      (documentation 'bracket-scan-to 'function) "The octet after the last one looked at.")

(defun make-bracket-scan (syntax around)
  "A BRACKET-SCAN of SYNTAX's text within +BRACKET-REACH+ octets of AROUND."
  (let* ((rope (syntax-rope syntax))
         (from (max 0 (- around +bracket-reach+)))
         (to (min (tuatara.text:rope-bytes rope) (+ around +bracket-reach+))))
    (%make-bracket-scan rope (coerce (opaque-spans syntax from to) 'simple-vector) from to)))

(defun scan-span-at (scan p)
  "The range of SCAN whose brackets do not count holding octet P, or NIL."
  (let* ((spans (bracket-scan-spans scan))
         (lo (tuatara.runtime:bisect (mid 0 (length spans)) (<= (cdr (aref spans mid)) p))))
    (and (< lo (length spans)) (<= (car (aref spans lo)) p) (aref spans lo))))

(defun scan-bracket-at (scan p)
  "The octet of a bracket that counts at P in SCAN's text, or NIL."
  (let ((o (tuatara.text:rope-octet (bracket-scan-rope scan) p)))
    (and (or (assoc o *bracket-pairs*) (rassoc o *bracket-pairs*))
         (not (scan-span-at scan p))
         o)))

(defun scan-closer-after (scan open)
  "The octet after the bracket that closes the one at OPEN in SCAN's text;
NIL when it is not found, or is not the partner of the opening one."
  (multiple-value-bind (end same)
      (tuatara.text:rope-bracket-match (bracket-scan-rope scan) open :end (bracket-scan-to scan)
                                                                     :spans (bracket-scan-spans scan))
    (and same end)))

(defun scan-opener-of (scan close)
  "The octet of the bracket that the one at CLOSE closes in SCAN's text; NIL
when it is not found, or is not the partner of the closing one."
  (multiple-value-bind (start same)
      (tuatara.text:rope-bracket-match (bracket-scan-rope scan) (1+ close) :backward t :start (bracket-scan-from scan)
                                                                           :spans (bracket-scan-spans scan))
    (and same start)))

(defun scan-opener-before (scan p)
  "The octet of the nearest opening bracket before P in SCAN's text that is
not closed before P, or NIL."
  (tuatara.text:rope-bracket-match (bracket-scan-rope scan) p :backward t :inside t :start (bracket-scan-from scan)
                                                              :spans (bracket-scan-spans scan)))

(defun enclosing-pairs (syntax position &optional (count 1))
  "The COUNT bracketed expressions around POSITION in SYNTAX's text, innermost
first, each as (START . END): START at its opening bracket, END after its
closing one. POSITION at an opening or closing bracket counts as inside that
pair. Brackets in strings, comments and character literals do not count, nor
those in POSITION's own string when it is in one. Fewer than COUNT come back
when there are fewer, or when their brackets do not balance."
  (pairs-around (make-bracket-scan syntax position) position count))

(defun text-enclosing-pairs (rope position &optional (count 1))
  "As ENCLOSING-PAIRS, for ROPE, a text with no syntax tree: every bracket counts."
  (pairs-around (bracket-scan-near nil rope position) position count))

(defun pairs-around (scan position count)
  "The COUNT bracketed expressions around POSITION in SCAN's text, innermost
first, as ENCLOSING-PAIRS gives them."
  (let* ((size (tuatara.text:rope-bytes (bracket-scan-rope scan)))
         (p (if (and (< position size) (eql (scan-bracket-at scan position)
                                             (car (assoc (tuatara.text:rope-octet (bracket-scan-rope scan) position)
                                                         *bracket-pairs*))))
                (1+ position)
                position))
         (pairs '()))
    (loop repeat count
          do (let* ((open (scan-opener-before scan p))
                    (close (and open (scan-closer-after scan open))))
               (unless close (return))
               (push (cons open close) pairs)
               (setf p open)))
    (nreverse pairs)))

;;; Groups: moving over, into and out of bracketed expressions

(defun bracket-scan-near (syntax rope position)
  "A BRACKET-SCAN of the text within +BRACKET-REACH+ octets of POSITION:
SYNTAX's text, whose strings, comments and character literals do not count,
or when SYNTAX is NIL, ROPE, every bracket counting."
  (if syntax
      (make-bracket-scan syntax position)
      (let ((size (tuatara.text:rope-bytes rope)))
        (%make-bracket-scan rope #() (max 0 (- position +bracket-reach+)) (min size (+ position +bracket-reach+))))))

(defun bracket-scan-between (rope start end)
  "A BRACKET-SCAN of the octets of ROPE from START below END, every bracket
there counting: the text of one string or comment."
  (%make-bracket-scan rope #() start end))

(defun scan-next-bracket (scan position)
  "The first octet at or after POSITION in SCAN's text that is a bracket that
counts, and true as a second value when it opens; NIL when there is none
before the end of the scan."
  (let ((rope (bracket-scan-rope scan)) (q position))
    (loop
      (let ((at (and (< q (bracket-scan-to scan))
                     (tuatara.text:rope-find-any-octet rope tuatara.text:*bracket-octets* :start q :end (bracket-scan-to scan)))))
        (unless at (return nil))
        (let ((span (scan-span-at scan at)))
          (if span
              (setf q (cdr span))
              (return (values at (and (assoc (tuatara.text:rope-octet rope at) *bracket-pairs*) t)))))))))

(defun scan-previous-bracket (scan position)
  "The last octet before POSITION in SCAN's text that is a bracket that
counts, and true as a second value when it opens; NIL when there is none after
the start of the scan."
  (let ((rope (bracket-scan-rope scan)) (q position))
    (loop
      (let ((at (and (> q (bracket-scan-from scan))
                     (tuatara.text:rope-find-any-octet rope tuatara.text:*bracket-octets* :start (bracket-scan-from scan) :end q
                                                                             :from-end t))))
        (unless at (return nil))
        (let ((span (scan-span-at scan at)))
          (if span
              (setf q (car span))
              (return (values at (and (assoc (tuatara.text:rope-octet rope at) *bracket-pairs*) t)))))))))

(defun scan-reached-end-p (scan forward)
  "True when SCAN reaches the end of its text (FORWARD) or its start, rather
than stopping short of it at +BRACKET-REACH+."
  (if forward
      (>= (bracket-scan-to scan) (tuatara.text:rope-bytes (bracket-scan-rope scan)))
      (<= (bracket-scan-from scan) 0)))

(defun next-group (scan position)
  "The bracketed group after POSITION in SCAN's text, words, strings and
comments before it passed over: its opening bracket's octet and the octet
after its closing one, as two values. Else NIL and why: :END when a closing
bracket comes first, POSITION being at the end of the group around it;
:UNBALANCED when the opening bracket has no partner; :NONE when no bracket
comes before the end of the text, :FAR when none does within the scan."
  (multiple-value-bind (p opening) (scan-next-bracket scan position)
    (cond ((null p) (values nil (if (scan-reached-end-p scan t) :none :far)))
          ((not opening) (values nil :end))
          (t (let ((end (scan-closer-after scan p)))
               (if end (values p end) (values nil :unbalanced)))))))

(defun previous-group (scan position)
  "The bracketed group before POSITION in SCAN's text, as NEXT-GROUP finds
the one after: its start and end as two values; else NIL and :START when an
opening bracket comes first, :UNBALANCED, :NONE or :FAR."
  (multiple-value-bind (p opening) (scan-previous-bracket scan position)
    (cond ((null p) (values nil (if (scan-reached-end-p scan nil) :none :far)))
          (opening (values nil :start))
          (t (let ((open (scan-opener-of scan p)))
               (if open (values open (1+ p)) (values nil :unbalanced)))))))

(defun group-start-around (scan position)
  "The octet of the opening bracket of the group around POSITION in SCAN's
text: the nearest before it that is not closed before it; NIL when there is
none within the scan."
  (scan-opener-before scan position))

(defun group-end-around (scan position)
  "The octet after the closing bracket of the group around POSITION in SCAN's
text: the nearest at or after it that is not opened after it; NIL when there
is none within the scan."
  (values (tuatara.text:rope-bracket-match (bracket-scan-rope scan) position :inside t :end (bracket-scan-to scan)
                                                                             :spans (bracket-scan-spans scan))))

(defun block-comment-end-p (rope end)
  "True when the octets of ROPE just before END close a comment that has an
end of its own: |# or */."
  (and (>= end 2)
       (member (tuatara.text:rope-octet rope (- end 2)) '(124 42))
       (= (tuatara.text:rope-octet rope (1- end)) (if (= (tuatara.text:rope-octet rope (- end 2)) 124) 35 47))))

(defun text-kind-around (syntax position)
  "What POSITION of SYNTAX's text is inside, as three values: :STRING or
:COMMENT and the start and end of that string or comment, the innermost; NIL
when it is in neither. Inside means after its first octet and before the end
of its last; for a comment that runs to the end of its line, at that end too,
whether the grammar takes the line end into the comment or not."
  (when (plusp position)
    (let ((rope (syntax-rope syntax)))
      (dolist (node (node-ancestry syntax (1- position) position))
        (let ((start (tree-node-start node)) (end (tree-node-end node)))
          (cond ((and (string-node-p node) (< start position end))
                 (return (values :string start end)))
                ((and (comment-node-p node)
                      (or (< start position end)
                          ;; at the line end a comment stops short of
                          (and (= position end) (< start end)
                               (/= (1- end) (tuatara.text:rope-line-end rope (1- end)))
                               (not (block-comment-end-p rope end))
                               (= end (tuatara.text:rope-line-end rope end)))))
                 (return (values :comment start end)))))))))
