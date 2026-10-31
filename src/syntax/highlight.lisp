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

(defun content-ranges (low high children from to)
  "The parts of the text from LOW below HIGH between the children whose
starts and ends are the numbers of the fixnum vector CHILDREN from FROM
below TO, two each, as a list of (START . END); the whole of it when there
are none."
  (let ((ranges '()) (at low))
    (loop for i from from below to by 2
          do (when (< at (aref children i)) (push (cons at (min (aref children i) high)) ranges))
             (setf at (max at (aref children (1+ i)))))
    (when (< at high) (push (cons at high) ranges))
    (nreverse ranges)))

(defstruct (injection-texts (:constructor make-injection-texts (tree properties found texts count asked)) (:copier nil))
  "The texts in another language a language's injection rules found in TREE,
before the children of their nodes are known: FOUND, for each match finding
some, (NAME . HOW-MANY) in order, the language and how many of TEXTS are its;
TEXTS, seven numbers for each of COUNT nodes holding such text: its match's
pattern, 0 when all its children are part of it, 1 when only its named ones
are left out, else 2; the part of it wanted (start, end); the node's start,
end and symbol. ASKED of them need their children; PROPERTIES are the rules'
#set! directives by pattern."
  (tree nil :read-only t)
  (properties #() :read-only t)
  (found '() :read-only t)
  (texts #() :type (simple-array fixnum (*)) :read-only t)
  (count 0 :type fixnum :read-only t)
  (asked 0 :type fixnum :read-only t))

(setf (documentation 'injection-texts-tree 'function) "The tree the texts were found in."
      (documentation 'injection-texts-properties 'function) "The rules' #set! directives by pattern."
      (documentation 'injection-texts-found 'function) "For each match finding text, its language and how many texts."
      (documentation 'injection-texts-texts 'function) "Seven numbers for each node holding such text."
      (documentation 'injection-texts-count 'function) "How many nodes hold such text."
      (documentation 'injection-texts-asked 'function) "How many of the nodes need their children."
      (documentation 'injection-texts-p 'function) "True when OBJECT is an INJECTION-TEXTS.")

(defun find-injection-texts (query tree rope start end from to)
  "The texts in another language QUERY, a language's injection rules, finds
in TREE (the tree of ROPE) touching octets START below END, of them only
what lies from FROM below TO, as INJECTION-TEXTS; NIL when there are none."
  (let ((names (query-captures query))
        (properties (query-properties query))
        (found '())
        ;; seven numbers for each node holding such text: its match's pattern,
        ;; 0 when all its children count, 1 when only its named ones are left
        ;; out, else 2; the part of it wanted; its start, end and symbol
        (texts (make-array 0 :element-type 'fixnum))
        (count 0)
        (asked 0))
    (declare (type (simple-array fixnum (*)) texts) (type fixnum count asked))
    (map-query-matches
     (lambda (pattern captures n)
       (let* ((set (aref properties pattern))
              (name (or (getf set :injection.language)
                        (loop for i from 0 below n
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
              (children (cond ((directive-p set :injection.include-children) 0)
                              ((directive-p set :injection.include-unnamed-children) 1)
                              (t 2)))
              (before count))
         (when name
           (dotimes (i n)
             (when (string= (aref names (aref captures (* 4 i))) "injection.content")
               (let ((low (max (aref captures (+ 1 (* 4 i))) from))
                     (high (min (aref captures (+ 2 (* 4 i))) to)))
                 (when (< low high)
                   (when (> (* 7 (1+ count)) (length texts))
                     (setf texts (replace (make-array (max 70 (* 2 (length texts))) :element-type 'fixnum) texts)))
                   (let ((at (* 7 count)))
                     (setf (aref texts at) pattern (aref texts (+ at 1)) children
                           (aref texts (+ at 2)) low (aref texts (+ at 3)) high
                           (aref texts (+ at 4)) (aref captures (+ 1 (* 4 i)))
                           (aref texts (+ at 5)) (aref captures (+ 2 (* 4 i)))
                           (aref texts (+ at 6)) (aref captures (+ 3 (* 4 i)))))
                   (unless (= children 0) (incf asked))
                   (incf count)))))
           (when (> count before) (push (cons name (- count before)) found)))))
     query tree rope start end)
    (and (plusp count)
         (make-injection-texts tree properties (nreverse found) texts count asked))))

(defun injection-texts-children (all helper)
  "The children of the nodes of each of ALL, INJECTION-TEXTS of trees HELPER
holds, that need them, asked for in one request: two values as
NODE-CHILDREN-MANY gives them, in the order of ALL; NIL when none are asked
for or they cannot be had."
  (let ((asked (loop for texts in all sum (injection-texts-asked texts))))
    (when (plusp asked)
      (let ((nodes (make-array (* 7 asked) :element-type 'fixnum)) (k 0))
        (dolist (found all)
          (let ((texts (injection-texts-texts found)) (number (tree-number (injection-texts-tree found))))
            (dotimes (i (injection-texts-count found))
              (let ((at (* 7 i)))
                (unless (= (aref texts (+ at 1)) 0)
                  (setf (aref nodes k) number
                        (aref nodes (+ k 1)) (aref texts (+ at 4)) (aref nodes (+ k 2)) (aref texts (+ at 5))
                        (aref nodes (+ k 3)) (aref texts (+ at 6))
                        (aref nodes (+ k 4)) (if (= (aref texts (+ at 1)) 1) 1 0)
                        (aref nodes (+ k 5)) (aref texts (+ at 2)) (aref nodes (+ k 6)) (aref texts (+ at 3)))
                  (incf k 7))))))
        (node-children-many helper nodes asked)))))

(defun injection-texts-layers (found pairs starts first)
  "The texts of FOUND, INJECTION-TEXTS, as INJECTION-LAYERS gives them, the
children of its nodes that need them in PAIRS from the FIRST of STARTS on
\(INJECTION-TEXTS-CHILDREN); the whole of each node's text when PAIRS is NIL."
  (let ((texts (injection-texts-texts found))
        (properties (injection-texts-properties found))
        (combined '()) (layers '()) (i 0) (j first))
    (declare (type fixnum i j))
    (loop for (name . texts-of-match) in (injection-texts-found found)
          for pattern = (aref texts (* 7 i))
          for ranges = (loop repeat texts-of-match
                             for at = (* 7 i)
                             for low = (aref texts (+ at 2)) for high = (aref texts (+ at 3))
                             append (if (= (aref texts (+ at 1)) 0)
                                        (list (cons low high))
                                        (prog1 (if pairs
                                                   (content-ranges low high pairs (aref starts j) (aref starts (1+ j)))
                                                   (list (cons low high)))
                                          (incf j)))
                             do (incf i))
          do (if (directive-p (aref properties pattern) :injection.combined)
                 (let ((entry (assoc (cons pattern name) combined :test #'equal)))
                   (if entry
                       (setf (cdr entry) (append (cdr entry) ranges))
                       (push (cons (cons pattern name) ranges) combined)))
                 (push (list name (tuatara.text:merge-ranges ranges)) layers)))
    (dolist (entry (reverse combined) layers)
      (push (list (cdar entry) (tuatara.text:merge-ranges (cdr entry))) layers))))

(defun injection-layers (query tree rope start end &optional (from 0) (to most-positive-fixnum))
  "The texts in another language that QUERY, a language's injection rules,
finds in TREE (the tree of ROPE) touching octets START below END: a list of
(NAME RANGES), NAME the language as the rules name it, or as (:SHEBANG LINE)
or (:FILENAME NAME) where a #! line or a file name says which, and RANGES its
text, each (START . END), in order, of it only what lies from FROM below TO:
all of a node's text when the rules include its children, the parts between
its named children when they include its unnamed ones, else the parts
between all its children. What the matches of one pattern that combines its
texts find is one text."
  (let ((found (find-injection-texts query tree rope start end from to)))
    (when found
      (multiple-value-bind (pairs starts) (injection-texts-children (list found) (tree-helper tree))
        (injection-texts-layers found pairs starts 0)))))

(defparameter *injection-time-limit* 0.5
  "The seconds a parse of text in another language may take while lines are
coloured; past it that text is left without its colours.")

(defun note-parse-failure (grammar why helper)
  "Turn GRAMMAR off for the text being parsed (*PARSING-BUFFER*'s, else
HELPER's), and for all once it has gone wrong on several
\(GRAMMAR-WENT-WRONG), when a parse of text in it went wrong as WHY says: a
failure inside the grammar's own code, or the program parsing ending as it
parsed or taking too long, so that it is not tried again on each redraw."
  (let ((where (or *parsing-buffer* helper)))
    (case why
      (:failed (grammar-went-wrong grammar where (make-condition 'grammar-failure :grammar grammar)))
      ((:crashed :hung) (grammar-went-wrong grammar where why)))))

(defun parse-ranges (grammar rope ranges)
  "A syntax tree of ROPE's text in GRAMMAR, reading only RANGES, each (START .
END), in order and apart; NIL when it cannot be parsed, with why as a second
value (PARSE-ROPE). A failure turns the grammar off (NOTE-PARSE-FAILURE)."
  (let ((helper (current-helper)))
    (unless (or (null helper) (grammar-off-p grammar (or *parsing-buffer* helper)))
      (multiple-value-bind (tree why)
          (parse-rope-ranges grammar rope ranges :limit *injection-time-limit* :helper helper
                                                 :budget (parse-memory-limit (loop for (start . end) in ranges
                                                                                   sum (- end start))))
        (note-parse-failure grammar why helper)
        (values tree why)))))

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
  (let ((newline (tuatara.text:rope-find-line-end-backward rope :start (max 0 (- byte 4096)) :end byte)))
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

(defun layers-highlights (resolve layers rope start end depth)
  "The colours of each of LAYERS, (NAME RANGES), the text of ROPE in RANGES
read as the language NAME, as LANGUAGE-HIGHLIGHTS gives them: a list of
them, in the order of LAYERS. Those of one depth are parsed and their rules'
matches found in one request, and the children of the nodes holding text in
yet other languages in them asked for in one more."
  (let* ((helper (current-helper))
         (layers (loop for (name ranges) in layers
                       collect (multiple-value-bind (grammar colours faces inner locals face-of) (funcall resolve name)
                                 (and helper grammar colours ranges (not (grammar-off-p grammar (or *parsing-buffer* helper)))
                                      (let ((key (and *injected-trees* (list name ranges))))
                                        (multiple-value-bind (tree known)
                                            (if key (gethash key *injected-trees*) (values nil nil))
                                          (and (or tree (not known))
                                               (list :grammar grammar :ranges ranges :colours colours :faces faces
                                                     :inner (and inner (< depth *injection-depth*) inner)
                                                     :locals (and face-of locals) :face-of face-of :key key :tree tree
                                                     :local-from (max (car (first ranges)) (- start *locals-reach*))))))))))
         (wanted (remove nil layers)))
    (if (null wanted)
        (make-list (length layers))
        (parse-texts
         rope
         (loop for layer in wanted
               collect (destructuring-bind (&key grammar ranges colours inner locals tree local-from &allow-other-keys) layer
                         (make-text-parse grammar ranges tree
                                          (remove nil (list (list colours start end)
                                                            (and locals (list locals local-from end))
                                                            (and inner (list inner start end))))
                                          *injection-time-limit*
                                          (parse-memory-limit (loop for (s . e) in ranges sum (- e s))))))
         ;; in HELPER, the current one
         (lambda (trees)
           (let ((made '()))
             (unwind-protect
                  (let* ((trees (loop for layer in wanted
                                      for (tree why) in trees
                                      do (destructuring-bind (&key grammar key ((:tree given)) &allow-other-keys) layer
                                           (note-parse-failure grammar why helper)
                                           (unless given
                                             (if key (setf (gethash key *injected-trees*) tree) (when tree (push tree made)))))
                                      collect tree))
                         ;; each one's own colours, and where text in yet other languages lies in it
                         (own (loop for layer in wanted
                                    for tree in trees
                                    collect (when tree
                                              (destructuring-bind (&key colours faces locals face-of local-from &allow-other-keys) layer
                                                (multiple-value-bind (local-ranges references)
                                                    (and locals (local-highlights locals tree rope local-from start end face-of))
                                                  (append (let ((*local-references* references))
                                                            (rich-highlights colours tree rope start end faces))
                                                          local-ranges))))))
                         (from (injection-cut rope (max 0 (- start *injection-reach*))))
                         (to (tuatara.text:rope-char-start rope (min (tuatara.text:rope-bytes rope) (+ end *injection-reach*))))
                         (found (loop for layer in wanted
                                      for tree in trees
                                      collect (let ((inner (getf layer :inner)))
                                                (and tree inner (find-injection-texts inner tree rope start end from to)))))
                         (inner-layers
                           (let ((all (remove nil found)))
                             (when all
                               (multiple-value-bind (pairs starts) (injection-texts-children all (tree-helper (injection-texts-tree (first all))))
                                 (let ((first 0))
                                   (loop for texts in found
                                         collect (when texts
                                                   (prog1 (injection-texts-layers texts pairs starts first)
                                                     (incf first (injection-texts-asked texts))))))))))
                         (inner (and inner-layers
                                     (layers-highlights resolve (loop for list in inner-layers append list)
                                                        rope start end (1+ depth))))
                         (results (loop for colours in own
                                        for list in (or inner-layers (make-list (length own)))
                                        collect (append colours
                                                        (loop repeat (length list) append (pop inner))))))
                    (loop for layer in layers
                          collect (and layer (pop results))))
               (dolist (tree made) (delete-tree tree)))))))))

(defun language-highlights (resolve name ranges rope start end &optional (depth 1))
  "The colours of the text of ROPE in RANGES (each (START . END), in order and
apart) read as the language NAME, from octet START below END, as RICH-HIGHLIGHTS
gives them, with those of text in other languages inside it: RESOLVE is a
function of a language's name returning what SYNTAX-HIGHLIGHTS's INJECTIONS
resolve to. NIL for a language not coloured, or text that cannot be parsed."
  (first (layers-highlights resolve (list (list name ranges)) rope start end depth)))

(defun injected-highlights (injections tree rope start end &optional (depth 1))
  "The colours of the text in other languages that INJECTIONS finds in TREE (the
tree of ROPE) from octet START below END, as RICH-HIGHLIGHTS gives them, a
name defined earlier in the same text coloured as its definition where the
language has rules for local names. See SYNTAX-HIGHLIGHTS for INJECTIONS."
  (destructuring-bind (query resolve) injections
    (let ((layers (injection-layers query tree rope start end
                                    (injection-cut rope (max 0 (- start *injection-reach*)))
                                    (tuatara.text:rope-char-start rope (min (tuatara.text:rope-bytes rope)
                                                                             (+ end *injection-reach*))))))
      (and layers
           (loop for colours in (layers-highlights resolve layers rope start end depth)
                 append colours)))))

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
                    (and injections (car *rules-generation*))))
         ;; none while its helper starts: shown once it has (BUFFER-HELPER-SOON)
         (helper (buffer-helper-soon (syntax-buffer syntax))))
    (when helper
     (kept-colours
     syntax key
     (lambda ()
        (let* ((*helper* helper)
               (*parsing-buffer* (syntax-buffer syntax))
               (bytes (tuatara.text:rope-bytes rope))
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
          ranges))))))

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
                   (*helper* (or (syntax-helper syntax) :none))
                   (*parsing-buffer* (syntax-buffer syntax))
                   (*injected-trees* (and injections (keep-injected-trees syntax)))
                   ;; definitions may begin as early as the top-level form holding START
                   (local-from (and locals
                                    (let* ((chain (node-ancestry syntax start start))
                                           (from (if (rest chain) (tree-node-start (car (last chain 2))) start)))
                                      (max (min from start) (- start *locals-reach*))))))
              ;; the matches of all three rules asked for at once
              (call-with-prefetched-matches
               (tree-helper tree) (remove nil (list (and locals (list (first locals) tree local-from end))
                                          (list query tree start end)
                                          (and injections (list (first injections) tree start end))))
               (lambda ()
                 (multiple-value-bind (local-ranges local-references)
                     (and locals (local-highlights (first locals) tree rope local-from start end (second locals)))
                   ;; the local names known first, for the rules' #is-not? local
                   (append (let ((*local-references* local-references))
                             (rich-highlights query tree rope start end faces))
                           local-ranges
                           (and injections (injected-highlights injections tree rope start end)))))))))))))
