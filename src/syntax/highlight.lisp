;;;; src/syntax/highlight.lisp --- Syntax colours from a tree and a query.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A query's captures name what to colour, and where several cover the same
;;;; text, the one to show is chosen as Helix chooses it: of captures of the
;;;; very same text the last pattern wins, and an inner node's colour shows
;;;; over an outer one's. A capture may win its text and colour nothing, which
;;;; is how fewer things are coloured with the same rules.

(in-package #:tuatara.syntax)

(defun rich-highlights (query tree rope start end faces)
  "The colours QUERY gives the text of ROPE (whose syntax tree is TREE) from
octet START below END: a list of (START END FACE) ranges, outer ones first, so
that painting them in order leaves each character the colour of the innermost
node captured there. FACES holds the face of each capture of QUERY by number,
NIL for a capture that colours nothing. Of captures of the very same text, the
one from the pattern latest in the query wins; a capture whose face is :NONE
may win and then takes the colour away."
  (let ((best (make-hash-table)))
    (map-query-matches
     (lambda (pattern captures count)
       (dotimes (i count)
         (let ((face (aref faces (aref captures (* 4 i))))
               (s (aref captures (+ 1 (* 4 i))))
               (e (aref captures (+ 2 (* 4 i)))))
           (when (and face (< s e))
             (let* ((key (logior (ash s 31) e))
                    (old (gethash key best)))
               (when (or (null old) (>= pattern (car old)))
                 (setf (gethash key best) (cons pattern face))))))))
     query tree rope start end)
    (let ((ranges '()))
      (maphash (lambda (key value)
                 ;; a capture of none wins its text but colours nothing
                 (unless (eq (cdr value) :none)
                   (push (list (ash key -31) (logand key #x7fffffff) (cdr value)) ranges)))
               best)
      (sort ranges (lambda (a b)
                     (or (< (first a) (first b))
                         (and (= (first a) (first b)) (> (second a) (second b)))))))))

;;; Text in another language

(defun directive-p (properties key)
  "True when the #set! directives of a pattern, PROPERTIES, set KEY, with a
value or without."
  (loop for (k) on properties by #'cddr thereis (eq k key)))

(defun content-ranges (tree start end symbol include-children &optional (from start) (to end))
  "The text of the node of TREE from START below END of type SYMBOL that is in
another language, as a list of (START . END), of it only what lies from FROM
below TO: all of it when INCLUDE-CHILDREN is :ALL, the parts between its named
children when it is :UNNAMED (its literal tokens kept), else the parts between
all its children."
  (let ((low (max start from)) (high (min end to)))
    (cond ((>= low high) '())
          ((eq include-children :all) (list (cons low high)))
          (t (let ((ranges '()) (at low))
               (dolist (child (node-child-ranges tree start end symbol :named (eq include-children :unnamed)
                                                                       :from low :to high))
                 (when (< at (car child)) (push (cons at (min (car child) high)) ranges))
                 (setf at (max at (cdr child))))
               (when (< at high) (push (cons at high) ranges))
               (nreverse ranges))))))

(defun injection-layers (query tree rope start end &optional (from 0) (to most-positive-fixnum))
  "The texts in another language that QUERY, a language's injection rules,
finds in TREE (the tree of ROPE) touching octets START below END: a list of
(NAME RANGES), NAME the language as the rules name it, or as (:SHEBANG LINE)
or (:FILENAME NAME) where a #! line or a file name says which, and RANGES its
text, each (START . END), in order, of it only what lies from FROM below TO.
What the matches of one pattern that combines its texts find is one text."
  (let ((names (query-captures query))
        (properties (query-properties query))
        (combined (make-hash-table :test 'equal))
        (layers '()))
    (map-query-matches
     (lambda (pattern captures count)
       (let* ((set (aref properties pattern))
              (name (or (getf set :injection.language)
                        (loop for i from 0 below count
                              for capture = (aref names (aref captures (* 4 i)))
                              for from = (aref captures (+ 1 (* 4 i)))
                              for to = (aref captures (+ 2 (* 4 i)))
                              when (string= capture "injection.language")
                                return (capture-text rope from to)
                              ;; the language a #! line names, or a file name's
                              when (string= capture "injection.shebang")
                                return (let ((line (string-left-trim '(#\Space #\Tab)
                                                                     (capture-text rope from (min to (+ from 256))))))
                                         (and (> (length line) 2) (string= "#!" line :end2 2)
                                              (list :shebang (subseq line 0 (position #\Newline line)))))
                              when (string= capture "injection.filename")
                                return (list :filename (capture-text rope from (min to (+ from 256)))))))
              (include-children (cond ((directive-p set :injection.include-children) :all)
                                      ((directive-p set :injection.include-unnamed-children) :unnamed)))
              (ranges (loop for i from 0 below count
                            when (string= (aref names (aref captures (* 4 i))) "injection.content")
                              append (content-ranges tree (aref captures (+ 1 (* 4 i))) (aref captures (+ 2 (* 4 i)))
                                                     (aref captures (+ 3 (* 4 i))) include-children from to))))
         (when (and name ranges)
           (if (directive-p set :injection.combined)
               (let ((key (cons pattern name)))
                 (setf (gethash key combined) (append (gethash key combined) ranges)))
               (push (list name (tuatara.text:merge-ranges ranges)) layers)))))
     query tree rope start end)
    (maphash (lambda (key ranges) (push (list (cdr key) (tuatara.text:merge-ranges ranges)) layers)) combined)
    layers))

(defparameter *injection-time-limit* 0.5
  "The seconds a parse of text in another language may take while lines are
coloured; past it that text is left without its colours.")

(defun parse-ranges (grammar rope ranges)
  "A syntax tree of ROPE's text in GRAMMAR, reading only RANGES, each (START .
END), in order and apart; NIL when it cannot be parsed, with :SLOW, :MEMORY or
:FAILED as a second value when it took too long or too much memory or the
grammar went wrong (PARSE-ROPE). A failure inside the
grammar's own code turns the grammar off, as a failed parse of a buffer
does."
  (unless (grammar-faulted grammar)
    (let ((parser (make-parser grammar)))
      (handler-case
          (multiple-value-bind (tree why)
              (if (set-included-ranges parser rope ranges)
                  (parse-rope parser rope :limit *injection-time-limit*
                                          :budget (parse-memory-limit (loop for (start . end) in ranges sum (- end start))))
                  nil)
            (delete-parser parser)
            (when (eq why :failed)
              (setf (grammar-faulted grammar) (make-condition 'grammar-failure :grammar grammar)))
            (values tree why))
        (sb-sys:memory-fault-error (fault)
          ;; the parser is left alone: its memory may be damaged
          (setf (grammar-faulted grammar) fault)
          nil)
        (error () (delete-parser parser) nil)))))

(defparameter *injection-depth* 3
  "How deeply text in another language may hold text in yet another.")

(defparameter *injection-reach* (* 64 1024)
  "How much of a text in another language before and after the lines being
coloured is parsed with them, in octets: enough for its colours there to be
right, while a very long one (a code block of megabytes) costs no more than a
short one.")

(defun injection-cut (rope byte)
  "Where to cut a text in another language near BYTE of ROPE: the start of
BYTE's line when it is near, else a character boundary."
  (let ((newline (tuatara.text:rope-find-octet-backward rope 10 :start (max 0 (- byte 4096)) :end byte)))
    (if newline (1+ newline) (tuatara.text:rope-char-start rope byte))))

(defparameter *locals-reach* (* 256 1024)
  "How far before the lines being coloured the definitions of local names
are looked for, in octets: a name defined further back, as a parameter of a
function of many thousands of lines can be, keeps the colour it has.")

(defvar *injected-trees* nil
  "While SYNTAX-HIGHLIGHTS colours a buffer's text, the table of the trees of
the text in other languages in it already parsed (KEEP-INJECTED-TREES); NIL
elsewhere, each such tree then parsed for the one use.")

(defparameter *injected-trees-kept* 64
  "The most trees of text in other languages kept for one buffer's colours:
past them, those kept are let go and parsed again as they are needed.")

(defun forget-injected-trees (syntax)
  "Let go of the trees of text in other languages kept for SYNTAX's colours."
  (let ((kept (syntax-injected syntax)))
    (when kept
      (maphash (lambda (key tree) (declare (ignore key)) (when tree (delete-tree tree))) (cdr kept))
      (setf (syntax-injected syntax) nil))))

(defun keep-injected-trees (syntax)
  "The table of the trees of text in other languages parsed for SYNTAX's
colours, by language name and ranges, for its tree as it is now: those parsed
for an earlier tree, or past *INJECTED-TREES-KEPT*, let go first. So the
text in other languages a window shows is parsed once for each change of
the text, not again each time the window scrolls."
  (let ((kept (syntax-injected syntax)))
    (when (and kept (or (/= (car kept) (syntax-generation syntax))
                        (>= (hash-table-count (cdr kept)) *injected-trees-kept*)))
      (forget-injected-trees syntax))
    (cdr (or (syntax-injected syntax)
             (setf (syntax-injected syntax) (cons (syntax-generation syntax) (make-hash-table :test 'equal)))))))

(defun language-highlights (resolve name ranges rope start end &optional (depth 1))
  "The colours of the text of ROPE in RANGES (each (START . END), in order and
apart) read as the language NAME, from octet START below END, as RICH-HIGHLIGHTS
gives them, with those of text in other languages inside it: RESOLVE is a
function of a language's name returning what SYNTAX-HIGHLIGHTS's INJECTIONS
resolve to. NIL for a language not coloured, or text that cannot be parsed."
  (multiple-value-bind (grammar colours faces inner locals face-of) (funcall resolve name)
    (let* ((key (and *injected-trees* grammar colours ranges (list name ranges)))
           (inner-tree (if key
                           (multiple-value-bind (tree known) (gethash key *injected-trees*)
                             (if known tree (setf (gethash key *injected-trees*) (parse-ranges grammar rope ranges))))
                           (and grammar colours ranges (parse-ranges grammar rope ranges)))))
      (when inner-tree
        (unwind-protect
             (multiple-value-bind (local-ranges references)
                 (and locals face-of
                      (local-highlights locals inner-tree rope
                                        (max (car (first ranges)) (- start *locals-reach*))
                                        start end face-of))
               (append (let ((*local-references* references))
                         (rich-highlights colours inner-tree rope start end faces))
                       local-ranges
                       (when (and inner (< depth *injection-depth*))
                         (injected-highlights (list inner resolve) inner-tree rope start end (1+ depth)))))
          (unless key (delete-tree inner-tree)))))))

(defun injected-highlights (injections tree rope start end &optional (depth 1))
  "The colours of the text in other languages that INJECTIONS finds in TREE (the
tree of ROPE) from octet START below END, as RICH-HIGHLIGHTS gives them, a
name defined earlier in the same text coloured as its definition where the
language has rules for local names. See SYNTAX-HIGHLIGHTS for INJECTIONS."
  (destructuring-bind (query resolve) injections
    (loop with from = (injection-cut rope (max 0 (- start *injection-reach*)))
          with to = (tuatara.text:rope-char-start rope (min (tuatara.text:rope-bytes rope) (+ end *injection-reach*)))
          for (name ranges) in (injection-layers query tree rope start end from to)
          append (language-highlights resolve name ranges rope start end depth))))

;;; Local names

(defun local-highlights (query tree rope from start end face-of)
  "The colours QUERY, a language's rules for local names, gives the names in
TREE (the tree of ROPE) from octet START below END that are defined earlier
in a scope around them, read from FROM on (where the definitions of names in
that part may begin): each such name takes the colour of the kind of its
definition, the definition nearest in the innermost scope winning. FACE-OF is
a function of a kind (such as variable.parameter) returning its face, or NIL
to leave it. Names the rules also capture as _ are left alone. A scope whose
rules set local.scope-inherits to false (a Ruby method) sees no definition
made outside it. A list of (START END FACE) ranges, as RICH-HIGHLIGHTS gives,
and as a second value the names that name a local definition, as
*LOCAL-REFERENCES* holds them."
  (let ((names (query-captures query))
        (properties (query-properties query))
        (scopes '()) (definitions (make-hash-table :test 'equal)) (references '()) (left (make-hash-table))
        (resolved (make-hash-table)))
    (map-query-matches
     (lambda (pattern captures count)
       (dotimes (i count)
         (let ((name (aref names (aref captures (* 4 i))))
               (s (aref captures (+ 1 (* 4 i))))
               (e (aref captures (+ 2 (* 4 i)))))
           (cond ((string= name "local.scope")
                  (push (list s e (equal (getf (aref properties pattern) :local.scope-inherits) "false")) scopes))
                 ((string= name "local.reference") (push (cons s e) references))
                 ((and (> (length name) 17) (string= "local.definition." name :end2 17))
                  (push (list s e (subseq name 17)) (gethash (capture-text rope s e) definitions)))
                 ((string= name "_") (setf (gethash (logior (ash s 31) e) left) t))))))
     query tree rope from end)
    ;; one sweep in the order of the text: the scopes open at each point, each
    ;; with the latest definition of each name so far in it; a name takes the
    ;; definition of the innermost open scope that has one, else one made
    ;; outside every scope. Scopes nest, as the nodes they are.
    (let ((items '())
          (opens (sort scopes (lambda (a b) (or (< (first a) (first b))
                                                (and (= (first a) (first b)) (> (second a) (second b)))))))
          (stack '())                   ; open scopes, innermost first: (END TABLE SEALED)
          (outside (make-hash-table :test 'equal))
          (ranges '()))
      (maphash (lambda (text list)
                 (dolist (definition list)
                   (push (list (first definition) 1 text (third definition)) items)))
               definitions)
      (dolist (reference references)
        (destructuring-bind (s . e) reference
          (when (and (< s end) (> e start) (not (gethash (logior (ash s 31) e) left)))
            (push (list s 0 (capture-text rope s e) e) items))))
      ;; by place, a name used before one defined at the same place
      (setf items (sort items (lambda (a b) (or (< (first a) (first b))
                                                (and (= (first a) (first b)) (< (second a) (second b)))))))
      (dolist (item items)
        (destructuring-bind (at what text other) item
          ;; open the scopes begun by here, closing those ended before each
          (loop while (and opens (<= (first (first opens)) at))
                do (destructuring-bind (s e sealed) (pop opens)
                     (loop while (and stack (<= (first (first stack)) s)) do (pop stack))
                     (push (list e (make-hash-table :test 'equal) sealed) stack)))
          (loop while (and stack (<= (first (first stack)) at)) do (pop stack))
          (if (= what 1)
              ;; a definition, OTHER its kind
              (setf (gethash text (if stack (second (first stack)) outside)) other)
              ;; a name used, OTHER where it ends; a sealed scope looks no further out
              (let* ((kind (loop for (nil table sealed) in stack
                                 for found = (gethash text table)
                                 when found return found
                                 when sealed return nil
                                 finally (return (gethash text outside))))
                     (face (and kind (funcall face-of kind))))
                (when kind
                  (setf (gethash (logior (ash at 31) other) resolved) t))
                (when (and face (not (eq face :none)))
                  (push (list at other face) ranges))))))
      (values (nreverse ranges) resolved))))

;;; Colours for display

(defvar *rules-generation* (list 0)
  "How many times a language's rules for colouring have been compiled, in its
car: colours kept for a text with other languages in it (a Markdown file's
code, an Org note's) are made again when it grows, those made while one of
those languages' rules was not yet at hand among them.")

(defparameter *syntax-slice-reach* (* 8 1024)
  "How much of a text before and after the lines being coloured is parsed with
them, in octets, while the parse of the whole text is not ready (a large file
just opened, or changed all through) or the text is too large to parse whole:
the lines shown are coloured at once from it, and as the whole text is parsed
once that parse is done.")

(defparameter *colours-kept* 8
  "How many answers of SYNTAX-HIGHLIGHTS are kept for one buffer: enough for
the parts of it every window shows in a frame.")

(defun kept-colours (syntax key make)
  "The colours kept for SYNTAX under KEY, a list compared with EQUAL; else
those MAKE, a function of no arguments, works out, then kept with the
*COLOURS-KEPT* - 1 used most lately."
  (let ((hit (assoc key (syntax-cache syntax) :test #'equal)))
    (if hit
        (cdr hit)
        (let ((ranges (funcall make)))
          (setf (syntax-cache syntax)
                (cons (cons key ranges)
                      (loop for entry in (syntax-cache syntax) repeat (1- *colours-kept*) collect entry)))
          ranges))))

(defun slice-highlights (syntax query faces start end injections)
  "The colours of SYNTAX's buffer text from octet START below END under QUERY,
from a parse of that text alone and *SYNTAX-SLICE-REACH* octets on either side
of it, as SYNTAX-HIGHLIGHTS gives them (no local names); NIL when it cannot be
parsed. Where the text is too large to parse whole, a parse of its lines that
takes too long or too much memory leaves it uncoloured from then on
\(SYNTAX-BROKEN), so that scrolling never waits on it again. Kept, as SYNTAX-HIGHLIGHTS keeps its answers, while the text and lines
are the same."
  (let* ((rope (tuatara.buffer:buffer-text (syntax-buffer syntax)))
         (key (list :slice rope start end query faces (and injections (first injections))
                    (and injections (car *rules-generation*)))))
    (kept-colours
     syntax key
     (lambda ()
        (let* ((bytes (tuatara.text:rope-bytes rope))
               (from (injection-cut rope (max 0 (- start *syntax-slice-reach*))))
               (to (let ((reach (+ end *syntax-slice-reach*)))
                     (if (>= reach bytes) bytes (max end (injection-cut rope reach)))))
               ;; the slice is parsed as a text of its own, its places then moved to the buffer's
               (slice (and (< from to) (tuatara.text:rope-slice rope from to)))
               (tree (when slice
                       (multiple-value-bind (tree why) (parse-ranges (syntax-grammar syntax) slice (list (cons 0 (- to from))))
                         ;; a text too large to parse whole whose lines are too slow to parse is left uncoloured
                         (when (and why (syntax-too-big syntax))
                           (setf (syntax-broken syntax) why))
                         tree)))
               (ranges (when tree
                         (unwind-protect
                              (loop for (s e face) in (append (rich-highlights query tree slice (- start from) (- end from) faces)
                                                              (and injections
                                                                   (let ((*injected-trees* nil))
                                                                     (injected-highlights injections tree slice
                                                                                          (- start from) (- end from)))))
                                    collect (list (+ s from) (+ e from) face))
                           (delete-tree tree)))))
          ranges)))))

(defun syntax-highlights (syntax query faces start end &optional injections locals)
  "The syntax colours of SYNTAX's buffer text from octet START below END under
QUERY, as a list of (START END FACE) ranges to paint in order; NIL while there
is no tree. FACES holds the face of each capture of QUERY by number (see
RICH-HIGHLIGHTS). INJECTIONS, when given, is (RULES RESOLVE): the language's
injection rules, and a function of a language's name as they name it that
returns, for a language this editor colours, its grammar, colouring query,
faces, injection rules, rules for local names and the function giving the
face of each kind of definition (as LOCALS below) as six values, NIL for one
it does not; the text in another language is then coloured as that language,
over the colours of the text around it. LOCALS, when given, is (RULES FACE-OF): the language's
rules for local names and the face of each kind of definition (see
LOCAL-HIGHLIGHTS); a name defined earlier in a scope around it is then
coloured as its definition is. Waits a moment for a parse under way (see
*SYNTAX-WAIT*); while there is no tree yet, or the text is too large to parse
whole, the colours of a parse of these lines alone (SLICE-HIGHLIGHTS). The
answers for the last few ranges asked for are kept (KEPT-COLOURS) and given
again while nothing they depend on has changed."
  (let ((tree (syntax-ready-tree syntax)))
    (unless (or tree (syntax-broken syntax))
      (return-from syntax-highlights (slice-highlights syntax query faces start end injections)))
    (when tree
      (let ((key (list (syntax-generation syntax) start end query faces
                       (and injections (first injections)) (and locals (first locals))
                       (and injections (car *rules-generation*)))))
        (kept-colours
         syntax key
         (lambda ()
            (let* ((rope (syntax-rope syntax))
                   (*injected-trees* (and injections (keep-injected-trees syntax)))
                   (ranges (multiple-value-bind (local-ranges local-references)
                               (and locals
                                    ;; definitions may begin as early as the top-level form holding START
                                    (let* ((chain (node-ancestry syntax start start))
                                           (from (if (rest chain) (tree-node-start (car (last chain 2))) start)))
                                      (local-highlights (first locals) tree rope
                                                        (max (min from start) (- start *locals-reach*)) start end
                                                        (second locals))))
                             ;; the local names known first, for the rules' #is-not? local
                             (append (let ((*local-references* local-references))
                                       (rich-highlights query tree rope start end faces))
                                     local-ranges
                                     (and injections (injected-highlights injections tree rope start end))))))
              ranges)))))))
