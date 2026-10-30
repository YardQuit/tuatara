;;;; src/syntax/tree-sitter.lisp --- Tree-sitter: grammars, parsers, trees and queries.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Tree-sitter parses text into a syntax tree with a grammar compiled to C, and
;;;; finds patterns in the tree with queries. The library and the grammars are
;;;; compiled from pinned sources by tools/grammars.sh into a directory of shared
;;;; objects beside the program, loaded when a buffer first needs one and never
;;;; kept in a saved image. The parser reads the text through a callback that
;;;; copies it from the rope a piece at a time, so the text is never copied whole.

(in-package #:tuatara.syntax)

;;; Calling the library

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun foreign-type (keyword)
    "The alien type TS-CALL uses for KEYWORD: :pointer, :u16, :u32, :u64, :i32, :i64, :bool or :void."
    (ecase keyword
      (:pointer 'sb-sys:system-area-pointer)
      (:u16 '(sb-alien:unsigned 16))
      (:u32 '(sb-alien:unsigned 32))
      (:u64 '(sb-alien:unsigned 64))
      (:i32 '(sb-alien:signed 32))
      (:i64 '(sb-alien:signed 64))
      (:bool '(sb-alien:boolean 8))
      (:void 'sb-alien:void))))

(defmacro ts-call (name result &rest arguments)
  "Call the C function NAME of the tree-sitter library, which returns RESULT,
with ARGUMENTS, alternately a type and a value. The types are the keywords
FOREIGN-TYPE knows."
  (let ((pairs (loop for (type value) on arguments by #'cddr collect (list type value))))
    `(sb-alien:alien-funcall
      (sb-alien:extern-alien ,name (function ,(foreign-type result) ,@(mapcar (lambda (p) (foreign-type (first p))) pairs)))
      ,@(mapcar #'second pairs))))

(defun foreign-string (pointer &optional length)
  "The UTF-8 text of LENGTH octets at the foreign POINTER, as a string; without
LENGTH, the octets up to the first zero."
  (tuatara.text:lenient-string (sap-octets pointer length)))

;;; Finding and loading the library and the grammars

(defvar *grammar-directories* '()
  "Directories searched in order for the tree-sitter library and the grammars;
when empty, grammars/ beside the program, then the source tree's build/grammars/.")

(defvar *grammars* (make-hash-table :test 'equal :synchronized t)
  "Every grammar looked for, by name: its GRAMMAR, or :MISSING.")

(defun grammar-directories ()
  "The directories searched for the tree-sitter library and grammars."
  (or *grammar-directories* (tuatara.runtime:resource-directories "grammars/" "build/grammars/")))

(defun find-grammar-file (name)
  "The first file called NAME in the grammar directories, or NIL."
  (loop for directory in (grammar-directories)
        for path = (probe-file (merge-pathnames name directory))
        when path return path))

(defun syntax-library-p ()
  "True when the tree-sitter library is loaded, loading it the first time it is needed."
  (let ((path (find-grammar-file "libtuatara-syntax.so")))
    (and path (tuatara.runtime:foreign-library-p (namestring path)))))

(defstruct (grammar (:constructor make-grammar (name pointer symbols named)) (:copier nil))
  "A loaded tree-sitter grammar: its NAME, the POINTER to its language,
SYMBOLS, the name of each of its node types by symbol number, and NAMED,
true for each that names a node rather than a literal token. FAULTED is set
when its own code failed while reading a text: it reads nothing more."
  (name "" :type string :read-only t)
  (pointer (null-pointer) :type sb-sys:system-area-pointer :read-only t)
  (symbols #() :type simple-vector :read-only t)
  (named #() :type simple-vector :read-only t)
  (spans-query nil)
  (faulted nil))

(setf (documentation 'grammar-p 'function) "True when OBJECT is a GRAMMAR."
      (documentation 'grammar-name 'function) "The grammar's name, as its file is named: rust for rust.so."
      (documentation 'grammar-pointer 'function) "The grammar's tree-sitter language, a foreign pointer."
      (documentation 'grammar-symbols 'function) "The grammar's node type names, by symbol number."
      (documentation 'grammar-named 'function) "For each symbol of the grammar, true when it names a node rather than a literal token."
      (documentation 'grammar-faulted 'function) "What failed once the grammar's own code failed while reading a text (a GRAMMAR-FAILURE or a memory fault), else NIL; it reads nothing more."
      (documentation 'grammar-spans-query 'function) "The grammar's query for strings and comments, compiled when first needed.")

(defmethod print-object ((grammar grammar) stream)
  (print-unreadable-object (grammar stream :type t)
    (write-string (grammar-name grammar) stream)))

(defconstant +oldest-grammar-version+ 13
  "The oldest grammar format the tree-sitter library reads.")

(defconstant +newest-grammar-version+ 15
  "The newest grammar format the tree-sitter library reads.")

(defun grammar-entry (path name)
  "The address of the function tree_sitter_NAME in the grammar library at PATH,
opened so that the names in it are its own: a grammar with a copy of another's
code under that one's names (Vue's of HTML's) stays apart from that other one,
whichever comes first. NIL when it cannot be opened or has no such function."
  (let ((library (sb-alien:alien-funcall
                  (sb-alien:extern-alien "dlopen" (function sb-sys:system-area-pointer sb-alien:c-string sb-alien:int))
                  ;; RTLD_NOW, RTLD_LOCAL being 0: every name bound at once, none of them shared
                  (sb-ext:native-namestring path) 2)))
    (unless (null-pointer-p library)
      (let ((entry (sb-alien:alien-funcall
                    (sb-alien:extern-alien "dlsym" (function sb-sys:system-area-pointer sb-sys:system-area-pointer sb-alien:c-string))
                    library (format nil "tree_sitter_~a" (substitute #\_ #\- name)))))
        (unless (null-pointer-p entry)
          (sb-sys:sap-int entry))))))

(defun load-grammar (name)
  "Load the grammar NAME from NAME.so in the grammar directories and return it,
or NIL when it is missing or of a version the library cannot read."
  (let ((path (and (syntax-library-p) (find-grammar-file (format nil "~a.so" name)))))
    (when path
      (let ((address (grammar-entry path name)))
        (when address
          (let* ((pointer (sb-alien:alien-funcall
                           (sb-alien:sap-alien (sb-sys:int-sap address) (function sb-sys:system-area-pointer))))
                 (version (ts-call "ts_language_abi_version" :u32 :pointer pointer)))
            (when (<= +oldest-grammar-version+ version +newest-grammar-version+)
              (let* ((count (ts-call "ts_language_symbol_count" :u32 :pointer pointer))
                     (symbols (make-array count))
                     (named (make-array count)))
                (dotimes (i count)
                  (let ((name (ts-call "ts_language_symbol_name" :pointer :pointer pointer :u16 i)))
                    (setf (aref symbols i) (if (null-pointer-p name) "" (foreign-string name))
                          ;; 0 is a regular symbol, one that names nodes
                          (aref named i) (zerop (ts-call "ts_language_symbol_type" :u32 :pointer pointer :u16 i)))))
                (make-grammar name pointer symbols named)))))))))

(defun grammar-name-p (name)
  "True when NAME can name a grammar: one to 64 lower-case letters, digits,
_ and - (rust, c-sharp), so that it names a file in the grammar directories
and nothing elsewhere."
  (and (stringp name) (<= 1 (length name) 64)
       (every (lambda (c) (or (char<= #\a c #\z) (char<= #\0 c #\9) (char= c #\_) (char= c #\-))) name)))

(defun find-grammar (name)
  "The grammar called NAME (a string such as \"rust\"), loaded the first time it
is asked for; NIL when there is none, or NAME cannot name one (GRAMMAR-NAME-P)."
  (when (grammar-name-p name)
    (let ((known (gethash name *grammars*)))
      (if known
          (and (grammar-p known) known)
          (let ((grammar (load-grammar name)))
            (setf (gethash name *grammars*) (or grammar :missing))
            grammar)))))

(defun forget-grammars ()
  "Forget the loaded grammars, which a saved image does not keep, so that the
program loads them again when it next needs them."
  (clrhash *grammars*))

(pushnew 'forget-grammars sb-ext:*save-hooks*)

;;; Parsers and trees

(defun make-parser (grammar)
  "A new tree-sitter parser for GRAMMAR, as a foreign pointer; release it with DELETE-PARSER."
  (let ((parser (ts-call "ts_parser_new" :pointer)))
    (unless (ts-call "ts_parser_set_language" :bool :pointer parser :pointer (grammar-pointer grammar))
      (ts-call "ts_parser_delete" :void :pointer parser)
      (error "The ~a grammar cannot be used." (grammar-name grammar)))
    parser))

(defun delete-parser (parser)
  "Release PARSER, made by MAKE-PARSER."
  (ts-call "ts_parser_delete" :void :pointer parser))

(defun copy-tree-pointer (tree)
  "A copy of TREE that another thread may use; cheap, as the two share their nodes."
  (ts-call "ts_tree_copy" :pointer :pointer tree))

(defun delete-tree (tree)
  "Release TREE."
  (ts-call "ts_tree_delete" :void :pointer tree))

(defun set-included-ranges (parser rope ranges)
  "Make PARSER read only RANGES of ROPE's text, each (START . END) in octets,
in order and apart. True when tree-sitter takes them."
  (let ((count (length ranges)))
    (with-foreign-memory (sap (* 4 6 (max 1 count)))
      (loop for (start . end) in ranges
            for i from 0
            do (let ((from (tuatara.text:byte->line rope start))
                     (to (tuatara.text:byte->line rope end)))
                 (setf (sb-sys:sap-ref-32 sap (* 4 (* 6 i))) start
                       (sb-sys:sap-ref-32 sap (* 4 (+ 1 (* 6 i)))) end
                       (sb-sys:sap-ref-32 sap (* 4 (+ 2 (* 6 i)))) from
                       (sb-sys:sap-ref-32 sap (* 4 (+ 3 (* 6 i)))) (- start (tuatara.text:line->byte rope from))
                       (sb-sys:sap-ref-32 sap (* 4 (+ 4 (* 6 i)))) to
                       (sb-sys:sap-ref-32 sap (* 4 (+ 5 (* 6 i)))) (- end (tuatara.text:line->byte rope to)))))
      (= 1 (ts-call "tuatara_set_included_ranges" :i32 :pointer parser :pointer sap :u32 count)))))

(defconstant +most-children+ 4096
  "The most children of one node looked at.")

(defun node-child-ranges (tree start end symbol &key named (from start) (to end))
  "The octets each child of the node of TREE from START below END of type
SYMBOL starts and ends at, as a list of (START . END): its named children
alone when NAMED, else named children and literal tokens alike, of those
ending after FROM and starting before TO; NIL when there is no such node."
  (with-foreign-memory (sap (* 4 2 +most-children+))
    (let ((n (ts-call "tuatara_node_child_ranges" :i32 :pointer tree :u32 start :u32 end :u32 symbol
                      :bool named :u32 from :u32 to :pointer sap :u32 +most-children+)))
      (loop for i from 0 below (max n 0)
            collect (cons (sb-sys:sap-ref-32 sap (* 4 (* 2 i))) (sb-sys:sap-ref-32 sap (* 4 (1+ (* 2 i)))))))))

(defun tree-has-error-p (tree)
  "True when TREE's text has syntax errors."
  (ts-call "tuatara_tree_has_error" :bool :pointer tree))

(defun edit-tree (tree start old-end new-end start-point old-end-point new-end-point)
  "Tell TREE that its octets from START below OLD-END became the octets from
START below NEW-END; each point is a cons of the row and the column (in octets)
of the offset before it."
  (ts-call "tuatara_edit" :void :pointer tree :u32 start :u32 old-end :u32 new-end
           :u32 (car start-point) :u32 (cdr start-point)
           :u32 (car old-end-point) :u32 (cdr old-end-point)
           :u32 (car new-end-point) :u32 (cdr new-end-point)))

(defstruct (tree-node (:constructor make-tree-node (start end name named children &optional broken symbol)) (:copier nil))
  "A node of a syntax tree: the octets START below END it covers, the NAME of
its type (for a literal token, its text), whether it is NAMED, how many named
CHILDREN it has, whether it is BROKEN (a syntax error, or holding one), and
the number of its type in the grammar, its SYMBOL."
  (start 0 :type fixnum :read-only t)
  (end 0 :type fixnum :read-only t)
  (name "" :type string :read-only t)
  (named nil :read-only t)
  (children 0 :type fixnum :read-only t)
  (broken nil :read-only t)
  (symbol 0 :type fixnum :read-only t))

(setf (documentation 'make-tree-node 'function) "A TREE-NODE from its parts."
      (documentation 'tree-node-p 'function) "True when OBJECT is a TREE-NODE."
      (documentation 'tree-node-start 'function) "The octet the node starts at."
      (documentation 'tree-node-end 'function) "The octet after the node."
      (documentation 'tree-node-name 'function) "The name of the node's type; for a literal token, its text."
      (documentation 'tree-node-named 'function) "True for a named node, NIL for a literal token."
      (documentation 'tree-node-children 'function) "How many named children the node has."
      (documentation 'tree-node-broken 'function) "True when the node is a syntax error or holds one."
      (documentation 'tree-node-symbol 'function) "The number of the node's type in its grammar.")

;;; Reading the text for the parser

(defconstant +read-size+ 65536
  "The most octets the parser is given at a time.")

(defstruct (parse-input (:constructor make-parse-input (rope buffer)) (:copier nil))
  "The text a parse reads: ROPE, copied a piece at a time into the foreign
BUFFER, which holds the octets from FROM below TO. FAILED is set when reading
went wrong, and the parse is then thrown away."
  (rope nil :read-only t)
  (buffer (null-pointer) :type sb-sys:system-area-pointer :read-only t)
  (from 0 :type fixnum)
  (to 0 :type fixnum)
  (failed nil))

(setf (documentation 'make-parse-input 'function) "A PARSE-INPUT reading ROPE through the foreign BUFFER."
      (documentation 'parse-input-p 'function) "True when OBJECT is a PARSE-INPUT."
      (documentation 'parse-input-rope 'function) "The rope being parsed."
      (documentation 'parse-input-buffer 'function) "The foreign buffer the parser reads from."
      (documentation 'parse-input-from 'function) "The offset of the first octet in the buffer."
      (documentation 'parse-input-to 'function) "The offset after the last octet in the buffer."
      (documentation 'parse-input-failed 'function) "True when reading the text went wrong.")

(defvar *parse-input* nil
  "The PARSE-INPUT of the parse running in this thread.")

(defun read-for-parse (byte length)
  "Give the parse running in this thread the text from octet BYTE on: return a
foreign pointer to the octets, storing how many there are at the foreign
LENGTH, zero at the end of the text. Never signals: a failure marks the input
failed and ends the text, so no error unwinds through the parser."
  (let* ((input *parse-input*)
         (buffer (parse-input-buffer input)))
    (handler-case
        (let* ((rope (parse-input-rope input))
               (size (tuatara.text:rope-bytes rope))
               (to (parse-input-to input)))
          (cond ((>= byte size)
                 (setf (sb-sys:sap-ref-32 length 0) 0)
                 buffer)
                ;; still in the buffer, unless too little is left to finish a character
                ((and (<= (parse-input-from input) byte) (< byte to) (or (>= (- to byte) 4) (= to size)))
                 (setf (sb-sys:sap-ref-32 length 0) (- to byte))
                 (sb-sys:sap+ buffer (- byte (parse-input-from input))))
                (t
                 (let ((end (min size (+ byte +read-size+))))
                   (tuatara.text:rope-copy-to-sap rope byte end buffer)
                   (setf (parse-input-from input) byte
                         (parse-input-to input) end
                         (sb-sys:sap-ref-32 length 0) (- end byte))
                   buffer))))
      (serious-condition ()
        (setf (parse-input-failed input) t
              (sb-sys:sap-ref-32 length 0) 0)
        buffer))))

(sb-alien:define-alien-callable tuatara-read-for-parse sb-sys:system-area-pointer
    ((payload sb-sys:system-area-pointer) (byte (sb-alien:unsigned 32)) (length sb-sys:system-area-pointer))
  payload                               ; not used: a callable's body takes no declarations
  (read-for-parse byte length))

(defun parse-rope (parser rope &key old cancel limit budget)
  "Parse ROPE with PARSER and return the new tree, or NIL when the parse was
cancelled, ran out of time or memory, the grammar went wrong or the text
could not be read. The second value then says why: :SLOW when it ran out of
time, :MEMORY when it took more memory than BUDGET, :SHORT when the memory
the editor may take ran short (or was short, and it did not start) and
:FAILED when the grammar's own code gave the parser what it cannot have (a
token the grammar has no number for). The third value is true when PARSER
was left half way, memory running out within it, and is not to be used
again, not even to be deleted. A scanner writing past the memory given to it
for its state faults there, as SB-SYS:MEMORY-FAULT-ERROR. OLD is the tree of
the text before, edited to match ROPE with EDIT-TREE, or NIL to parse from
scratch. CANCEL is a foreign 32-bit flag: the parse stops soon after it
becomes nonzero. LIMIT is how many seconds the parse may take, BUDGET how
many octets of memory more than the tree-sitter library held before it; NIL
for no limit."
  (with-foreign-memory (buffer +read-size+)
    (with-foreign-memory (own-flag 4 :zero t)
      (let* ((flag (or cancel own-flag))
             (input (make-parse-input rope buffer))
             (tree (let ((*parse-input* input))
                     (ts-call "tuatara_parse" :pointer :pointer parser :pointer (or old (null-pointer))
                              :pointer (sb-alien:alien-sap (sb-alien:alien-callable-function 'tuatara-read-for-parse))
                              :pointer (null-pointer) :pointer flag
                              :u32 (if limit (max 1 (min #xffffffff (round (* limit 1000)))) 0)
                              :u64 (if budget (max 1 (min (1- (expt 2 63)) (round budget))) 0)))))
        (cond ((null-pointer-p tree)
               (let ((code (sb-sys:signed-sap-ref-32 flag 0)))
                 (if (= code 7)
                     (values nil :short t)
                     (progn (ts-call "ts_parser_reset" :void :pointer parser)
                            (values nil (case code (2 :slow) (3 :memory) (4 :failed) (6 :short)) nil)))))
              ((parse-input-failed input)
               (delete-tree tree)
               (ts-call "ts_parser_reset" :void :pointer parser)
               nil)
              (t tree))))))

;;; Queries

(defstruct (query (:constructor %make-query) (:copier nil))
  "A compiled tree-sitter query for GRAMMAR: POINTER to it, the SOURCE text,
CAPTURES (the name of each capture, by number), PREDICATES (for each pattern, a
list of the tests its matches must pass), STARTS (the octet in the source each
pattern starts at) and PROPERTIES (for each pattern, what its #set!
directives set)."
  (grammar nil :read-only t)
  (pointer (null-pointer) :type sb-sys:system-area-pointer :read-only t)
  (source "" :type string :read-only t)
  (captures #() :type simple-vector :read-only t)
  (predicates #() :type simple-vector :read-only t)
  (starts #() :type simple-vector :read-only t)
  (properties #() :type simple-vector :read-only t))

(setf (documentation 'query-p 'function) "True when OBJECT is a QUERY."
      (documentation 'query-grammar 'function) "The grammar the query is for."
      (documentation 'query-pointer 'function) "The compiled tree-sitter query, a foreign pointer."
      (documentation 'query-source 'function) "The text the query was compiled from."
      (documentation 'query-captures 'function) "The query's capture names, by capture number."
      (documentation 'query-predicates 'function) "For each pattern of the query, the tests its matches must pass."
      (documentation 'query-starts 'function) "For each pattern of the query, the octet of the source it starts at."
      (documentation 'query-properties 'function) "For each pattern, a property list of what its #set! directives set.")

(defmethod print-object ((query query) stream)
  (print-unreadable-object (query stream :type t :identity t)
    (format stream "~a, ~d patterns" (grammar-name (query-grammar query)) (length (query-predicates query)))))

(define-condition query-error (error)
  ((grammar :initarg :grammar :reader query-error-grammar)
   (kind :initarg :kind :reader query-error-kind)
   (line :initarg :line :reader query-error-line)
   (text :initarg :text :reader query-error-text))
  (:documentation "A query that tree-sitter could not compile: its KIND of error and where.")
  (:report (lambda (c s)
             (format s "~a in the ~a rules at line ~a: ~a" (query-error-kind c)
                     (grammar-name (query-error-grammar c)) (query-error-line c) (query-error-text c)))))

(setf (documentation 'query-error-grammar 'function) "The grammar the failed query was for."
      (documentation 'query-error-kind 'function) "What was wrong, in words."
      (documentation 'query-error-line 'function) "The line of the query source where it went wrong."
      (documentation 'query-error-text 'function) "The text of the query source around the error.")

;;; A grammar going wrong

(define-condition grammar-failure (error)
  ((grammar :initarg :grammar :reader grammar-failure-grammar))
  (:documentation "A grammar's own code gave the parser what it cannot have,
such as a token the grammar has no number for, and the parse stopped.")
  (:report (lambda (c s)
             (format s "the ~a grammar gave the parser a value it does not have"
                     (grammar-name (grammar-failure-grammar c))))))

(setf (documentation 'grammar-failure-grammar 'function) "The grammar that went wrong.")

(defparameter *query-error-kinds*
  #("No error" "Syntax error" "Unknown node type" "Unknown field" "Unknown capture" "Impossible pattern" "Wrong grammar")
  "What each tree-sitter query error number means.")

(defun predicate-step-strings (query pattern)
  "The predicates and directives of PATTERN in the compiled QUERY, as lists
whose elements are strings (for string steps) and capture numbers."
  (sb-alien:with-alien ((count (sb-alien:unsigned 32)))
    (let* ((steps (ts-call "ts_query_predicates_for_pattern" :pointer :pointer query :u32 pattern
                           :pointer (sb-alien:alien-sap (sb-alien:addr count))))
           (result '()) (current '()))
      (dotimes (i count)
        (let ((type (sb-sys:sap-ref-32 steps (* 8 i)))
              (value (sb-sys:sap-ref-32 steps (+ 4 (* 8 i)))))
          (case type
            (0 (push (nreverse current) result) (setf current '()))
            (1 (push value current))
            (2 (push (sb-alien:with-alien ((length (sb-alien:unsigned 32)))
                       (foreign-string (ts-call "ts_query_string_value_for_id" :pointer :pointer query :u32 value
                                                :pointer (sb-alien:alien-sap (sb-alien:addr length)))
                                       length))
                     current)))))
      (nreverse result))))

(defun compile-regex-for-query (string)
  "A scanner for the regular expression STRING of a #match? predicate; one that
never matches when STRING is not a regular expression this editor reads."
  (handler-case (cl-ppcre:create-scanner string)
    (error () (constantly nil))))

(defvar *local-references* nil
  "While a language's colouring rules run: the names in the text coloured that
name a definition in a scope around them (LOCAL-HIGHLIGHTS), as a hash table
of their octets, start and end (LOGIOR (ASH START 31) END); NIL when not
known, and #is-not? local then passes.")

(defun parse-predicate (steps &optional (symbols #()))
  "The test for one predicate, from STEPS as PREDICATE-STEP-STRINGS gives them;
NIL for a directive or a predicate this editor does not know, which passes.
A test is a list (KIND CAPTURE ARGUMENT NEGATED). SYMBOLS, the names of the
grammar's symbols, are for tests of a node's kind. #is? local, true when a
node the match captures names a local definition, and #is-not? local, true
when none does, are the test :LOCAL (see *LOCAL-REFERENCES*)."
  (when (and (member (first steps) '("is?" "is-not?") :test #'equal) (equal (second steps) "local"))
    (return-from parse-predicate (list :local nil nil (string= (first steps) "is-not?"))))
  (destructuring-bind (name &optional capture &rest arguments) steps
    (let ((negated (and (> (length name) 4) (string= "not-" name :end2 4))))
      (when (integerp capture)
        (let ((base (if negated (subseq name 4) name)))
          (cond ((and (member base '("match?" "lua-match?") :test #'string=) (stringp (first arguments)))
                 (list :match capture (compile-regex-for-query (first arguments)) negated))
                ((and (string= base "eq?") arguments)
                 ;; a text to compare with, as its octets; or another capture's number
                 (list :eq capture (if (stringp (first arguments))
                                       (tuatara.text:string-octets (first arguments))
                                       (first arguments))
                       negated))
                ((string= base "any-of?")
                 ;; the texts, after the most octets one of them has
                 (let ((texts (remove-if-not #'stringp arguments)))
                   (list :any-of capture
                         (cons (reduce #'max texts :key (lambda (text) (length (tuatara.text:string-octets text)))
                                                   :initial-value 0)
                               texts)
                         negated)))
                ((and (string= base "same-line?") (integerp (first arguments)))
                 (list :same-line capture (first arguments) negated))
                ((string= base "one-line?")
                 (list :one-line capture nil negated))
                ((and (string= base "kind-eq?") (stringp (first arguments)))
                 (list :kind capture
                       (loop for i from 0 below (length symbols)
                             when (string= (aref symbols i) (first arguments)) collect i)
                       negated))))))))

(defun parse-directive (steps)
  "The property a #set! directive in STEPS sets, as a list (KEY VALUE) of a
keyword and a string, or NIL."
  (when (and (string= (first steps) "set!") (stringp (second steps)))
    (list (intern (string-upcase (second steps)) :keyword) (and (stringp (third steps)) (third steps)))))

(defun compile-query (grammar source &optional what)
  "Compile SOURCE, the text of a tree-sitter query, for GRAMMAR. Signals a
QUERY-ERROR when tree-sitter rejects it. WHAT, when given, says what the rules
are for (colouring, indentation), for the reports of debug mode."
  (let ((began (get-internal-real-time)))
    (handler-bind ((query-error (lambda (e)
                                  (tuatara.runtime:debug-report :rules "~a ~@[~a ~]rules do not compile: ~a"
                                                                (grammar-name grammar) what e))))
      (prog1 (compile-query-text grammar source)
        (tuatara.runtime:debug-report :rules "~a ~@[~a ~]rules, ~:d characters, compiled in ~,1f ms"
                                      (grammar-name grammar) what (length source) (tuatara.runtime:milliseconds-since began))))))

(defun compile-query-text (grammar source)
  "The work of COMPILE-QUERY: compile SOURCE, a tree-sitter query, for GRAMMAR."
  (let ((octets (sb-ext:string-to-octets source :external-format :utf-8)))
    (sb-alien:with-alien ((offset (sb-alien:unsigned 32)) (kind (sb-alien:unsigned 32)))
      (let ((pointer (sb-sys:with-pinned-objects (octets)
                       (ts-call "ts_query_new" :pointer :pointer (grammar-pointer grammar)
                                :pointer (sb-sys:vector-sap octets) :u32 (length octets)
                                :pointer (sb-alien:alien-sap (sb-alien:addr offset))
                                :pointer (sb-alien:alien-sap (sb-alien:addr kind))))))
        (when (null-pointer-p pointer)
          (let* ((at (min offset (length octets)))
                 (line-start (1+ (or (position 10 octets :end at :from-end t) -1)))
                 (line-end (or (position 10 octets :start at) (length octets))))
            (error 'query-error :grammar grammar
                                :kind (if (< kind (length *query-error-kinds*)) (aref *query-error-kinds* kind) "Error")
                                :line (1+ (count 10 octets :end at))
                                :text (tuatara.text:lenient-string octets :start line-start :end line-end))))
        (let* ((patterns (ts-call "ts_query_pattern_count" :u32 :pointer pointer))
               (captures (make-array (ts-call "ts_query_capture_count" :u32 :pointer pointer)))
               (predicates (make-array patterns))
               (starts (make-array patterns))
               (properties (make-array patterns))
               (query nil))
          (unwind-protect
               (progn
                 (dotimes (i (length captures))
                   (setf (aref captures i)
                         (sb-alien:with-alien ((length (sb-alien:unsigned 32)))
                           (foreign-string (ts-call "ts_query_capture_name_for_id" :pointer :pointer pointer :u32 i
                                                    :pointer (sb-alien:alien-sap (sb-alien:addr length)))
                                           length))))
                 (fill-patterns pointer predicates properties starts (grammar-symbols grammar))
                 (setf query (%make-query :grammar grammar :pointer pointer :source source :captures captures
                                          :predicates predicates :starts starts :properties properties))
                 (sb-ext:finalize query (lambda () (ts-call "ts_query_delete" :void :pointer pointer)) :dont-save t)
                 query)
            (unless query (ts-call "ts_query_delete" :void :pointer pointer))))))))

(defun query-non-local-patterns (query)
  "The numbers of QUERY's patterns with more than one node at their root, such
as a sibling sequence outside any parent: colouring a window with such a
query cannot skip the text before the window, so it slows down in large files."
  (loop for i from 0 below (length (query-predicates query))
        when (ts-call "ts_query_is_pattern_non_local" :bool :pointer (query-pointer query) :u32 i)
          collect i))

(defun disable-query-pattern (query pattern)
  "Make pattern number PATTERN of QUERY match nothing from now on."
  (ts-call "ts_query_disable_pattern" :void :pointer (query-pointer query) :u32 pattern))

(defun fill-patterns (pointer predicates properties starts symbols)
  "Fill in, for each pattern of the compiled query at POINTER, its PREDICATES,
PROPERTIES and STARTS; SYMBOLS are the names of the grammar's symbols."
  (dotimes (i (length predicates))
    (let ((steps (predicate-step-strings pointer i)))
      (setf (aref predicates i) (remove nil (mapcar (lambda (step) (parse-predicate step symbols)) steps))
            (aref properties i) (loop for (key value) in (remove nil (mapcar #'parse-directive steps))
                                      append (list key value))
            (aref starts i) (ts-call "ts_query_start_byte_for_pattern" :u32 :pointer pointer :u32 i)))))

;;; Running queries

(defconstant +most-captures+ 64
  "The most captures of one match looked at.")

(defstruct (query-run (:constructor %make-query-run) (:copier nil))
  "What running queries needs in one thread: a tree-sitter query CURSOR, the
foreign CAPTURES array matches are read into, and PATTERN, where the pattern
number goes."
  (cursor (null-pointer) :type sb-sys:system-area-pointer :read-only t)
  (captures (null-pointer) :type sb-sys:system-area-pointer :read-only t)
  (pattern (null-pointer) :type sb-sys:system-area-pointer :read-only t))

(setf (documentation 'query-run-p 'function) "True when OBJECT is a QUERY-RUN."
      (documentation 'query-run-cursor 'function) "The tree-sitter query cursor."
      (documentation 'query-run-captures 'function) "The foreign array the captures of a match are read into."
      (documentation 'query-run-pattern 'function) "The foreign number the pattern of a match is read into.")

(defconstant +match-limit+ 256
  "The most matches a query keeps in progress at once, so that a pattern that
matches almost anywhere cannot make a query slow.")

(defun make-query-run ()
  "A QUERY-RUN, for one thread at a time; released when it is garbage collected."
  (let* ((cursor (ts-call "ts_query_cursor_new" :pointer))
         (captures (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 32) (* 4 +most-captures+))))
         (pattern (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:unsigned 32))))
         (run (%make-query-run :cursor cursor :captures captures :pattern pattern)))
    (ts-call "ts_query_cursor_set_match_limit" :void :pointer cursor :u32 +match-limit+)
    (sb-ext:finalize run (lambda ()
                           (ts-call "ts_query_cursor_delete" :void :pointer cursor)
                           (sb-alien:free-alien (sb-alien:sap-alien captures (* (sb-alien:unsigned 32))))
                           (sb-alien:free-alien (sb-alien:sap-alien pattern (* (sb-alien:unsigned 32)))))
                     :dont-save t)
    run))

(sb-ext:defglobal **idle-query-runs** '()
  "The QUERY-RUNs no query is using now, to be taken by the next (WITH-QUERY-RUN).")

(sb-ext:defglobal **query-runs-lock** (sb-thread:make-mutex :name "query runs")
  "Held while a QUERY-RUN is taken from or given back to **IDLE-QUERY-RUNS**.")

(defun call-with-query-run (function)
  "Call FUNCTION with a QUERY-RUN no other query uses meanwhile, on any thread,
and a query run inside FUNCTION getting one of its own: taken from those idle,
or made when none is, and given back once FUNCTION returns."
  (let ((run (or (sb-thread:with-mutex (**query-runs-lock**) (pop **idle-query-runs**))
                 (make-query-run))))
    (unwind-protect (funcall function run)
      (sb-thread:with-mutex (**query-runs-lock**) (push run **idle-query-runs**)))))

(defun rope-octets-equal-p (rope start end octets)
  "True when the octets of ROPE from START below END are OCTETS, read a
window of them at a time."
  (declare (type fixnum start end) (type tuatara.text:octets octets))
  (and (= (length octets) (- end start))
       (let ((window (make-array 256 :element-type '(unsigned-byte 8))))
         (declare (dynamic-extent window))
         (loop for from of-type fixnum from start below end by 256
               for to of-type fixnum = (min end (+ from 256))
               always (progn (tuatara.text:rope-copy-into rope from to window)
                             (loop for i of-type fixnum from 0 below (- to from)
                                   always (= (aref window i) (aref octets (+ (- from start) i)))))))))

(defun capture-text (rope start end)
  "The text of ROPE from octet START below END, raw bytes as their characters."
  (tuatara.text:rope-string rope :start start :end end))

(defconstant +short-capture+ 256
  "Octets of a captured node's text read on the stack to be tested (CALL-WITH-CAPTURE-TEXT).")

(defun call-with-capture-text (rope start end function)
  "Call FUNCTION with a string holding the text of ROPE from octet START below
END, raw bytes as their characters, and that text's length, the string
perhaps longer: for a text of up to +SHORT-CAPTURE+ octets one on the stack,
which FUNCTION must not keep."
  (declare (type fixnum start end) (type function function))
  (let ((n (- end start)))
    (if (> n +short-capture+)
        (let ((text (capture-text rope start end))) (funcall function text (length text)))
        (let ((octets (make-array +short-capture+ :element-type '(unsigned-byte 8)))
              (text (make-string +short-capture+)))
          (declare (dynamic-extent octets text))
          (tuatara.text:rope-copy-into rope start end octets)
          (let ((i 0) (k 0))
            (declare (type fixnum i k))
            (loop while (< i n)
                  do (multiple-value-bind (code length) (tuatara.text:octets-decode octets i n)
                       (setf (schar text k) (code-char code))
                       (incf k) (incf i length)))
            (funcall function text k))))))

(defun rope-ranges-equal-p (rope start end other-start other-end)
  "True when ROPE holds the same octets from START below END as from OTHER-START below OTHER-END."
  (declare (type fixnum start end other-start other-end))
  (and (= (- end start) (- other-end other-start))
       (let ((a (make-array 256 :element-type '(unsigned-byte 8)))
             (b (make-array 256 :element-type '(unsigned-byte 8))))
         (declare (dynamic-extent a b))
         (loop for from of-type fixnum from 0 below (- end start) by 256
               for n of-type fixnum = (min 256 (- end start from))
               always (progn (tuatara.text:rope-copy-into rope (+ start from) (+ start from n) a)
                             (tuatara.text:rope-copy-into rope (+ other-start from) (+ other-start from n) b)
                             (loop for i of-type fixnum from 0 below n always (= (aref a i) (aref b i))))))))

(defun test-passes-p (test rope captures count)
  "True when the match whose COUNT captures (numbers, start, end, symbol, four
each) are in the fixnum vector CAPTURES passes TEST, reading text from ROPE.
A test of a capture the match lacks passes, but for same-line? and one-line?,
which look at the first node of each capture named and fail without one."
  (destructuring-bind (kind capture argument negated) test
    (flet ((first-node (number)
             (loop for i from 0 below count
                   when (= number (aref captures (* 4 i))) return i))
           (line (octet) (tuatara.text:byte->line rope octet)))
      (case kind
        (:local
         ;; a node of the match names a local definition; unknown, it passes
         (or (null *local-references*)
             (not (eq negated (loop for i from 0 below count
                                    thereis (gethash (logior (ash (aref captures (+ 1 (* 4 i))) 31) (aref captures (+ 2 (* 4 i))))
                                                     *local-references*))))))
        (:same-line
         (let ((a (first-node capture)) (b (first-node argument)))
           (and a b (not (eq negated (= (line (aref captures (+ 1 (* 4 a))))
                                        (line (aref captures (+ 1 (* 4 b))))))))))
        (:one-line
         (let ((a (first-node capture)))
           (and a (not (eq negated (= (line (aref captures (+ 1 (* 4 a))))
                                      (line (aref captures (+ 2 (* 4 a))))))))))
        (:kind
         (let ((a (first-node capture)))
           (or (null a) (not (eq negated (and (member (aref captures (+ 3 (* 4 a))) argument) t))))))
        (t (text-test-passes-p kind capture argument negated rope captures count))))))

(defun text-test-passes-p (kind capture argument negated rope captures count)
  "True when every node of CAPTURE in the match whose COUNT captures are in
CAPTURES passes the test of the text KIND with ARGUMENT, NEGATED or not,
reading text from ROPE; a capture the match lacks passes."
  (dotimes (i count t)
    (when (= capture (aref captures (* 4 i)))
      (let* ((start (aref captures (+ 1 (* 4 i))))
             (end (aref captures (+ 2 (* 4 i))))
             (holds (ecase kind
                      (:match (flet ((scan (text length)
                                       (and (cl-ppcre:scan argument text :end length) t)))
                                (declare (dynamic-extent #'scan))
                                (call-with-capture-text rope start end #'scan)))
                      (:eq (if (vectorp argument)
                               (rope-octets-equal-p rope start end argument)
                               ;; another capture: the same text is the same octets
                               (let ((other (loop for j from 0 below count
                                                  when (= argument (aref captures (* 4 j))) return j)))
                                 (and other
                                      (rope-ranges-equal-p rope start end (aref captures (+ 1 (* 4 other)))
                                                           (aref captures (+ 2 (* 4 other))))))))
                      ;; a text longer than every one of them is none of them
                      (:any-of (and (<= (- end start) (car argument))
                                    (flet ((any-of (text length)
                                             (loop for word in (cdr argument)
                                                   thereis (string= word text :end2 length))))
                                      (declare (dynamic-extent #'any-of))
                                      (call-with-capture-text rope start end #'any-of))
                                    t)))))
        (when (eq holds negated) (return nil))))))

(defun map-query-matches (function query tree rope start end)
  "Call FUNCTION with (PATTERN CAPTURES COUNT) for each match of QUERY in TREE
(the tree of ROPE) that touches octets START below END and passes its
predicates: PATTERN is the pattern's number, and CAPTURES a fixnum vector
holding COUNT captures as four numbers each: the capture's number, the octets
its node starts and ends at, and the node's symbol. CAPTURES is reused for the
next match. Matches with more than +MOST-CAPTURES+ captures are cut short."
  (declare (type function function))
  (call-with-query-run
   (lambda (run)
     (let ((cursor (query-run-cursor run))
           (foreign (query-run-captures run))
           (pattern-cell (query-run-pattern run))
           (captures (make-array (* 4 +most-captures+) :element-type 'fixnum))
           (predicates (query-predicates query)))
       (ts-call "tuatara_query" :void :pointer cursor :pointer (query-pointer query) :pointer tree
                :u32 (max 0 start) :u32 (max 0 end))
       (loop
         (let ((count (ts-call "tuatara_next_match" :i32 :pointer cursor :pointer pattern-cell
                               :pointer foreign :u32 +most-captures+)))
           (when (minusp count) (return))
           (let ((count (min count +most-captures+))
                 (pattern (sb-sys:sap-ref-32 pattern-cell 0)))
             (dotimes (i (* 4 count))
               (setf (aref captures i) (sb-sys:sap-ref-32 foreign (* 4 i))))
             (when (loop for test in (aref predicates pattern)
                         always (test-passes-p test rope captures count))
               (funcall function pattern captures count)))))))))
