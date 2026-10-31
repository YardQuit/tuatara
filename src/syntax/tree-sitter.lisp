;;;; src/syntax/tree-sitter.lisp --- Tree-sitter: grammars, parsers, trees and queries.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Tree-sitter parses text into a syntax tree with a grammar compiled to C, and
;;;; finds patterns in the tree with queries. The grammars are compiled from
;;;; pinned sources by tools/grammars.sh into a directory beside the program,
;;;; and run by the program that parses for the editor, shut in
;;;; (src/syntax/helper.lisp): a grammar, a parser, a tree and a query here
;;;; stand for those it holds. The parser reads the text from the rope a piece
;;;; at a time as it asks for it, so the text is never copied whole.

(in-package #:tuatara.syntax)

;;; Grammars

(defvar *grammars* (make-hash-table :test 'equal :synchronized t)
  "Every grammar looked for, by name: its GRAMMAR, or :MISSING.")

(defun find-grammar-file (name)
  "The first file called NAME in the grammar directories, or NIL."
  (loop for directory in (grammar-directories)
        for path = (probe-file (merge-pathnames name directory))
        when path return path))

(defstruct (grammar (:constructor make-grammar (name symbols named)) (:copier nil))
  "A tree-sitter grammar: its NAME, SYMBOLS, the name of each of its node
types by symbol number, and NAMED, true for each that names a node rather
than a literal token. FAULTED is what failed once its own code failed while
reading a text: it reads nothing more."
  (name "" :type string :read-only t)
  (symbols #() :type simple-vector :read-only t)
  (named #() :type simple-vector :read-only t)
  (spans-query nil)
  (faulted nil))

(setf (documentation 'grammar-p 'function) "True when OBJECT is a GRAMMAR."
      (documentation 'grammar-name 'function) "The grammar's name, as its file is named: rust for rust.so."
      (documentation 'grammar-symbols 'function) "The grammar's node type names, by symbol number."
      (documentation 'grammar-named 'function) "For each symbol of the grammar, true when it names a node rather than a literal token."
      (documentation 'grammar-faulted 'function) "What failed once the grammar's own code failed while reading a text (a GRAMMAR-FAILURE, a memory fault, or :CRASHED when it ended the program parsing), else NIL; it reads nothing more."
      (documentation 'grammar-spans-query 'function) "The grammar's query for strings and comments, compiled when first needed.")

(defmethod print-object ((grammar grammar) stream)
  (print-unreadable-object (grammar stream :type t)
    (write-string (grammar-name grammar) stream)))

(defparameter *grammar-faults-before-off* 3
  "How many different texts a grammar may go wrong on, crashing or failing
in the program parsing each, before it is turned off for every text: until
then it is off only for the text it went wrong on, so that one file made to
break it takes no language's colours away from any other.")

(sb-ext:defglobal **grammar-faults** (make-hash-table :test 'equal :synchronized t)
  "For each grammar's name, the texts it went wrong on (TEXT-IDENTITY).")

(defun grammar-went-wrong (grammar where fault)
  "Note that GRAMMAR went wrong, FAULT saying how (:CRASHED, :HUNG, or a
GRAMMAR-FAILURE), reading the text of WHERE, a buffer or a helper: it is off
for that text from now on (FAULTED-NAMES), and for all text once it has
gone wrong on *GRAMMAR-FAULTS-BEFORE-OFF* texts, each file counted once by
where it really is however often it is opened, a text with no file by its
content (TEXT-IDENTITY). Three different texts turning a language off for
all is as meant: a grammar going wrong that often is not to be trusted.
With no WHERE, off for all at once. True when it is now off for all."
  (when where
    (note-faulted where (grammar-name grammar)))
  (let ((count (length (sb-ext:with-locked-hash-table (**grammar-faults**)
                         (pushnew (if where (text-identity where) :none) (gethash (grammar-name grammar) **grammar-faults**)
                                  :test #'equal)))))
    (when (or (null where) (>= count *grammar-faults-before-off*))
      (setf (grammar-faulted grammar) fault)
      t)))

(defun grammar-off-p (grammar &optional where)
  "True when GRAMMAR reads nothing more: for any text, or for that of WHERE,
a buffer or a helper (FAULTED-NAMES)."
  (or (grammar-faulted grammar)
      (and where
           (let ((names (faulted-names where)))
             (or (member :all names) (member (grammar-name grammar) names :test #'equal)))
           t)))

(defun helper-gone-why (helper)
  "Why a parse in HELPER, which ended as it ran, gave nothing: :HUNG when it
was too long answering, :STOPPED when the editor ended it (for room, or its
buffer gone), else :CRASHED."
  (case (and helper (helper-ended helper))
    (:hung :hung)
    (:slow :slow)
    (:stopped :stopped)
    (t :crashed)))

(defconstant +most-symbols+ 65535
  "The most node types a grammar may have, as tree-sitter numbers them in 16 bits.")

(defconstant +longest-name+ 4096
  "The longest name of a node type or a capture a helper's reply may hold.")

(defparameter *grammar-load-deaths-most* 3
  "How many times a grammar may end the program loading grammars as it is
loaded before it is given up on, as one missing, until the editor starts again.")

(sb-ext:defglobal **grammar-load-deaths** (make-hash-table :test 'equal :synchronized t)
  "For each grammar's name, how many times loading it ended the program loading it.")

(sb-ext:defglobal **grammar-loads** (list 0)
  "How many grammars are being loaded now, in a cons for atomic counting.")

(sb-ext:defglobal **grammar-loads-begun** (list 0)
  "How many grammar loads have begun, in a cons for atomic counting: one that
begun while another ran was not alone.")

(defparameter *grammar-load-wait* 30
  "The seconds loading a grammar may take before it is given up on, this
time, as slow: a large grammar takes a while on a slow machine.")

(defun load-grammar (name)
  "The grammar NAME, as the helper compiling rules loads it from NAME.so in the grammar
directories, or NIL when it is missing or of a version tree-sitter cannot
read; :UNKNOWN when there is no helper to ask, or it did not answer, to be
asked again. When the helper ends as the grammar loads, alone, that counts
against it (*GRAMMAR-LOAD-DEATHS-MOST*); a slow load, or the helper ending
while others load too, does not."
  (let ((octets (tuatara.text:string-octets name))
        (helper (rules-helper)))
    (unless (and helper (helper-alive helper)) (return-from load-grammar :unknown))
    (let ((loaded
            (with-call (call helper +op-grammar+)
              (call-octets call octets)
              ;; alone: none loading when it began, none begun while it ran
              (let* ((others (sb-ext:atomic-incf (car **grammar-loads**)))
                     (begun (sb-ext:atomic-incf (car **grammar-loads-begun**)))
                     (status (unwind-protect (call-send call :seconds *grammar-load-wait* :late :slow)
                               (sb-ext:atomic-decf (car **grammar-loads**))))
                     (alone (and (= 0 others) (= (car **grammar-loads-begun**) (1+ begun)))))
                (cond ((and (eq status :dead) alone (eq (helper-ended helper) :crashed))
                       ;; the helper ended loading it: tried again a few times, then given up on
                       (if (< (incf (gethash name **grammar-load-deaths** 0)) *grammar-load-deaths-most*)
                           :unknown
                           (progn (tuatara.runtime:debug-report :error "the ~a grammar ended the program loading it ~d times: not loaded"
                                                                name *grammar-load-deaths-most*)
                                  :missing)))
                      ((eq status :dead) :unknown)
                      ((not (eql 0 status)) :missing)
                      (t
                       (let* ((count (reply-count call 4 8))
                              (symbols (make-array (if (<= count +most-symbols+) count (reply-overrun call))))
                              (named (make-array count))
                              (at 8))
                         (dotimes (i count)
                           (let ((length (reply-u32-at call (+ at 4))))
                             (when (> length +longest-name+) (reply-overrun call))
                             (setf (aref named i) (= 1 (reply-u32-at call at))
                                   (aref symbols i) (reply-string call (+ at 8) length))
                             (incf at (+ 8 (* 4 (ceiling length 4))))))
                         (make-grammar name symbols named))))))))
      ;; no answer had at all (the helper gone meanwhile): asked again
      (case loaded
        ((nil) :unknown)
        (:missing nil)
        (t loaded)))))

(defun grammar-name-p (name)
  "True when NAME can name a grammar: one to 64 lower-case letters, digits,
_ and - (rust, c-sharp), so that it names a file in the grammar directories
and nothing elsewhere."
  (and (stringp name) (<= 1 (length name) 64)
       (every (lambda (c) (or (char<= #\a c #\z) (char<= #\0 c #\9) (char= c #\_) (char= c #\-))) name)))

(defun own-grammar-p (name)
  "True when Tuatara has a grammar called NAME of its own."
  (and (loop for directory in (own-grammar-directories)
             thereis (probe-file (merge-pathnames (format nil "~a.so" name) directory)))
       t))

(defun regular-file-p (path)
  "True when PATH is a plain file itself, not a link nor anything else."
  (let ((stat (ignore-errors (sb-posix:lstat (uiop:native-namestring path)))))
    (and stat (= (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt) sb-posix:s-ifreg))))

(defun user-grammar-file (name)
  "The file NAME.so in the folder of grammars you add (USER-GRAMMAR-DIRECTORY)
when it is a grammar you added: a plain file there, not a link, and Tuatara
has no grammar of that name; else NIL."
  (let ((path (merge-pathnames (format nil "~a.so" name) (user-grammar-directory))))
    (and (grammar-name-p name) (regular-file-p path) (not (own-grammar-p name)) path)))

(defun user-grammar-p (name)
  "True when NAME names a grammar you added: in the folder you add them to, or
kept from it (USER-GRAMMAR-STORE), Tuatara having none of that name. Such a
grammar is loaded only as *USER-GRAMMAR-FUNCTION* allows."
  (and (grammar-name-p name)
       (not (own-grammar-p name))
       (or (ignore-errors (sb-posix:lstat (uiop:native-namestring (merge-pathnames (format nil "~a.so" name) (user-grammar-directory)))))
           (ignore-errors (sb-posix:lstat (uiop:native-namestring (merge-pathnames (format nil "~a.so" name) (user-grammar-store))))))
       t))

(defvar *user-grammar-function* nil
  "A function of the name of a grammar you added, true when it may be loaded
now: it is on, and the copy kept of it (USER-GRAMMAR-STORE) is what was
turned on; NIL, and none may.")

(defun find-grammar (name)
  "The grammar called NAME (a string such as \"rust\"), loaded the first time it
is asked for; NIL when there is none, or NAME cannot name one (GRAMMAR-NAME-P).
A grammar that could not be asked for, the program parsing having ended, is
asked for again next time. A grammar you added (USER-GRAMMAR-P) is loaded
only when *USER-GRAMMAR-FUNCTION* allows it, its kept copy then, and asked
about again next time when not."
  (when (grammar-name-p name)
    (let ((known (gethash name *grammars*)))
      (if known
          (and (grammar-p known) known)
          (let* ((refused (and (user-grammar-p name)
                               (not (and *user-grammar-function* (funcall *user-grammar-function* name)))))
                 (file (and (not refused) (find-grammar-file (format nil "~a.so" name))))
                 (grammar (cond (refused :unknown)
                                (file (load-grammar name)))))
            (unless (eq grammar :unknown)
              (setf (gethash name *grammars*) (or grammar :missing)))
            (and (grammar-p grammar) grammar))))))

(defun grammar-pending-p (name)
  "True when the grammar NAME is there but not loaded yet, the program loading
grammars not running (it is started in the background): asked for again, it
may be had."
  (and (grammar-name-p name)
       (null (gethash name *grammars*))
       (not (helpers-refused-p :rules))
       (find-grammar-file (format nil "~a.so" name))
       t))

(defun forget-grammars ()
  "Forget the grammars looked for, so that they are looked for again when next needed."
  (clrhash *grammars*))

;;; Parsers and trees

(defstruct (parser (:constructor %make-parser (helper number grammar)) (:copier nil))
  "A tree-sitter parser for GRAMMAR, held by HELPER as its parser NUMBER."
  (helper nil :read-only t)
  (number 0 :type fixnum :read-only t)
  (grammar nil :read-only t))

(setf (documentation 'parser-p 'function) "True when OBJECT is a PARSER."
      (documentation 'parser-helper 'function) "The helper holding the parser."
      (documentation 'parser-number 'function) "The parser's number in its helper."
      (documentation 'parser-grammar 'function) "The grammar the parser parses with.")

(defstruct (tree (:constructor %make-tree (helper number)) (:copier nil))
  "A syntax tree, held by HELPER as its tree NUMBER."
  (helper nil :read-only t)
  (number 0 :type fixnum :read-only t))

(setf (documentation 'tree-p 'function) "True when OBJECT is a TREE."
      (documentation 'tree-helper 'function) "The helper holding the tree."
      (documentation 'tree-number 'function) "The tree's number in its helper.")

(defun make-parser (grammar &optional (helper (current-helper)))
  "A new tree-sitter parser for GRAMMAR in HELPER (the one parsing the text
worked on now unless given); release it with DELETE-PARSER. NIL when there is
no helper."
  (let ((octets (tuatara.text:string-octets (grammar-name grammar))))
    (with-call (call helper +op-parser+)
      (call-octets call octets)
      (let ((status (call-send call)))
        (cond ((eql 0 status)
               (let ((number (reply-u32 call 0)))
                 (if (zerop number) (reply-overrun call) (%make-parser helper number grammar))))
              ((eq status :dead) nil)
              (t (error "The ~a grammar cannot be used." (grammar-name grammar))))))))

(defun delete-parser (parser)
  "Release PARSER, made by MAKE-PARSER."
  (when parser
    (with-call (call (parser-helper parser) +op-parser-delete+)
      (call-u32 call (parser-number parser))
      (call-send call))))

(defun copy-tree-pointer (tree)
  "A copy of TREE that another thread may use; cheap, as the two share their
nodes. NIL when its helper has ended."
  (with-call (call (tree-helper tree) +op-copy+)
    (call-u32 call (tree-number tree))
    (when (eql 0 (call-send call))
      (let ((number (reply-u32 call 0)))
        (and (/= number 0) (%make-tree (tree-helper tree) number))))))

(defun delete-tree (tree)
  "Release TREE."
  (with-call (call (tree-helper tree) +op-tree-delete+)
    (call-u32 call (tree-number tree))
    (call-send call)))

(defconstant +most-children+ 4096
  "The most children of one node looked at.")

(defun node-child-ranges (tree start end symbol &key named (from start) (to end))
  "The octets each child of the node of TREE from START below END of type
SYMBOL starts and ends at, as a list of (START . END): its named children
alone when NAMED, else named children and literal tokens alike, of those
ending after FROM and starting before TO; NIL when there is no such node. A
reply with children outside the node ends the helper."
  (let ((helper (tree-helper tree)))
    (with-call (call helper +op-children+)
      (call-u32 call (tree-number tree)) (call-u32 call start) (call-u32 call end) (call-u32 call symbol)
      (call-u32 call (if named 1 0)) (call-u32 call from) (call-u32 call to)
      (when (eql 0 (call-send call))
        (let* ((sap (call-in call)) (size (call-in-size call))
               (count (if (>= size 4) (sb-sys:signed-sap-ref-32 sap 0) most-positive-fixnum)))
          (cond ((minusp count) nil)
                ((or (> count +most-children+) (> (+ 4 (* 8 count)) size)) (reply-malformed helper))
                (t (loop for i from 0 below count
                         for s = (sb-sys:sap-ref-32 sap (+ 4 (* 8 i)))
                         for e = (sb-sys:sap-ref-32 sap (+ 8 (* 8 i)))
                         unless (<= start s e end)
                           do (return (reply-malformed helper))
                         collect (cons s e)))))))))

(defun node-children-many (helper nodes count)
  "The children of COUNT nodes of trees HELPER holds, asked for in one
request, NODES holding seven numbers for each: the number of its tree, then
as NODE-CHILD-RANGES takes them its start, end and symbol, 1 for named
children alone else 0, and the octets the children must end after and start
before. Two values: a fixnum vector of the children's starts and ends, two
numbers each, and a fixnum vector giving for each node where its children
start in the first, and at COUNT where the last ones end; NIL when the
helper did not answer, or answered with children outside their nodes, which
ends it."
  (declare (type (simple-array fixnum (*)) nodes) (type fixnum count))
  (when (<= 1 count +most-children+)
    (with-call (call helper +op-children-many+)
      (call-u32 call count)
      (dotimes (i (* 7 count)) (call-u32 call (aref nodes i)))
      (when (eql 0 (call-send call :most (* count (+ 4 (* 8 +most-children+)))))
        (let* ((sap (call-in call)) (size (call-in-size call))
               (starts (make-array (1+ count) :element-type 'fixnum))
               (total (loop with at = 0 for i from 0 below count
                            for n = (if (<= (+ at 4) size) (sb-sys:signed-sap-ref-32 sap at) most-positive-fixnum)
                            do (setf n (max n 0))
                               (when (or (> n +most-children+) (> (+ at 4 (* 8 n)) size))
                                 (return (reply-malformed helper)))
                               (incf at (+ 4 (* 8 n)))
                            sum n)))
          (when total
            (let ((pairs (make-array (* 2 total) :element-type 'fixnum)) (at 0) (k 0))
              (dotimes (i count)
                (let ((n (max 0 (sb-sys:signed-sap-ref-32 sap at)))
                      (low (aref nodes (+ 1 (* 7 i)))) (high (aref nodes (+ 2 (* 7 i)))))
                  (setf (aref starts i) k)
                  (dotimes (j n)
                    (let ((s (sb-sys:sap-ref-32 sap (+ at 4 (* 8 j))))
                          (e (sb-sys:sap-ref-32 sap (+ at 8 (* 8 j)))))
                      (unless (<= low s e high)
                        (return-from node-children-many (reply-malformed helper)))
                      (setf (aref pairs k) s (aref pairs (1+ k)) e)
                      (incf k 2)))
                  (incf at (+ 4 (* 8 n)))))
              (setf (aref starts count) k)
              (values pairs starts))))))))

(defun tree-has-error-p (tree)
  "True when TREE's text has syntax errors."
  (with-call (call (tree-helper tree) +op-has-error+)
    (call-u32 call (tree-number tree))
    (and (eql 0 (call-send call)) (= 1 (reply-u32 call 0)))))

(defun edit-tree (tree start old-end new-end start-point old-end-point new-end-point)
  "Tell TREE that its octets from START below OLD-END became the octets from
START below NEW-END; each point is a cons of the row and the column (in octets)
of the offset before it."
  (with-call (call (tree-helper tree) +op-edit+)
    (call-u32 call (tree-number tree))
    (call-u32 call start) (call-u32 call old-end) (call-u32 call new-end)
    (call-u32 call (car start-point)) (call-u32 call (cdr start-point))
    (call-u32 call (car old-end-point)) (call-u32 call (cdr old-end-point))
    (call-u32 call (car new-end-point)) (call-u32 call (cdr new-end-point))
    (call-send call)))

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

;;; Parsing

(defun parse-rope (parser rope &key old cancel limit budget)
  "Parse ROPE with PARSER and return the new tree, or NIL when the parse was
cancelled, ran out of time or memory, the grammar went wrong, the text could
not be read or the helper holding the parser ended. The second value then
says why: :SLOW when it ran out of time, :MEMORY when it took more memory
than BUDGET, :SHORT when the memory the helper may take ran short (or was
short, and it did not start), :FAILED when the grammar's own code gave the
parser what it cannot have (a token the grammar has no number for), :CRASHED
when the helper ended while it parsed, and :HUNG when it went on past LIMIT
and was ended. The third value is true when PARSER is not to be used again,
not even to be deleted: left half way, memory running out within it, or its
helper ended. OLD is the tree of the text before, edited to match ROPE with
EDIT-TREE, or NIL to parse from scratch. CANCEL is a foreign 32-bit flag: the
parse stops soon after it becomes nonzero. LIMIT is how many seconds the
parse may take, BUDGET how many octets of memory more than the parsing
program held before it; NIL for no limit."
  (let ((helper (parser-helper parser))
        (input (make-parse-input rope (tuatara.text:rope-bytes rope) (parser-number parser))))
    (unless (helper-alive helper)
      (return-from parse-rope (values nil (helper-gone-why helper) t)))
    (let ((result
           (with-call (call helper +op-parse+)
          (call-u32 call (parser-number parser))
          (call-u32 call (if old (tree-number old) 0))
          (call-u32 call (if limit (max 1 (min #xffffffff (round (* limit 1000)))) 0))
          (call-u64 call (if budget (max 1 (min (1- (expt 2 63)) (round budget))) 0))
          (multiple-value-list
           (let* ((asked-to-stop nil)
                 (status (while-parsing (helper) (call-send call
                                    :input input
                                    :seconds (+ (or limit 86400) *syntax-helper-wait*)
                                    :cancel (lambda ()
                                              (when (and cancel (not asked-to-stop)
                                                         (/= 0 (sb-sys:signed-sap-ref-32 cancel 0)))
                                                (setf asked-to-stop t)
                                                (send-cancel helper (parser-number parser))))))))
            (cond ((eq status :dead)
                   (values nil (helper-gone-why helper) t))
                  ((/= 0 status) (values nil nil nil))
                  (t
                   (let ((number (reply-u32 call 0))
                         (code (reply-u32 call 1)))
                     (when (and cancel (/= code 0) (= 0 (sb-sys:signed-sap-ref-32 cancel 0)))
                       (setf (sb-sys:signed-sap-ref-32 cancel 0) code))
                     (cond ((and (/= number 0) (parse-input-over input))
                            ;; read without end: given up as too slow
                            (delete-tree (%make-tree helper number))
                            (with-call (reset helper +op-reset+)
                              (call-u32 reset (parser-number parser))
                              (call-send reset))
                            (values nil :slow))
                           ((parse-input-over input) (values nil :slow))
                           ((and (/= number 0) (parse-input-failed input))
                            (delete-tree (%make-tree helper number))
                            (with-call (reset helper +op-reset+)
                              (call-u32 reset (parser-number parser))
                              (call-send reset))
                            nil)
                           ((/= number 0) (%make-tree helper number))
                           (t (values nil (case code (2 :slow) (3 :memory) (4 :failed) ((6 7) :short))
                                      (= code 7))))))))))))
      (if result (values-list result) (values nil (helper-gone-why helper) t)))))

;;; Queries

(defstruct (query (:constructor %make-query) (:copier nil))
  "A tree-sitter query for GRAMMAR, known to the helpers by NUMBER and compiled
in each when it first runs there: the SOURCE text, CAPTURES (the name of each
capture, by number), PREDICATES (for each pattern, a list of the tests its
matches must pass), STARTS (the octet in the source each pattern starts at),
PROPERTIES (for each pattern, what its #set! directives set), NON-LOCAL (the
numbers of its patterns that are not local, see QUERY-NON-LOCAL-PATTERNS) and
DISABLED (the numbers of those made to match nothing)."
  (grammar nil :read-only t)
  (number 0 :type fixnum :read-only t)
  (source "" :type string :read-only t)
  (captures #() :type simple-vector :read-only t)
  (predicates #() :type simple-vector :read-only t)
  (starts #() :type simple-vector :read-only t)
  (properties #() :type simple-vector :read-only t)
  (non-local '() :read-only t)
  (disabled '()))

(setf (documentation 'query-p 'function) "True when OBJECT is a QUERY."
      (documentation 'query-grammar 'function) "The grammar the query is for."
      (documentation 'query-number 'function) "The number the helpers know the query by."
      (documentation 'query-non-local 'function) "The numbers of the query's patterns that are not local."
      (documentation 'query-disabled 'function) "The numbers of the query's patterns made to match nothing."
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

(define-condition query-not-answered (error)
  ((grammar :initarg :grammar :reader query-not-answered-grammar))
  (:documentation "Rules that could not be compiled this time: the program compiling
them ended, or did not answer in time (a busy moment). Asked again later,
they may compile.")
  (:report (lambda (c s)
             (format s "the program compiling the ~a rules did not answer" (grammar-name (query-not-answered-grammar c))))))

(setf (documentation 'query-not-answered-grammar 'function) "The grammar whose rules were not compiled.")

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

(defun reply-predicate-steps (call at count &optional (longest +reply-most+))
  "The COUNT predicate steps of a pattern in CALL's reply to a query, from
octet AT: as lists whose elements are strings (for string steps) and capture
numbers, and the octet after them, as two values. A string longer than
LONGEST octets ends the helper (REPLY-OVERRUN)."
  (let ((result '()) (current '()))
    (dotimes (i count)
      (let ((type (reply-u32-at call at)))
        (case type
          (0 (push (nreverse current) result) (setf current '()) (incf at 8))
          (1 (push (reply-u32-at call (+ at 4)) current) (incf at 8))
          (t (let ((length (reply-u32-at call (+ at 4))))
               (when (> length longest) (reply-overrun call))
               (push (reply-string call (+ at 8) length) current)
               (incf at (+ 8 (* 4 (ceiling length 4)))))))))
    (values (nreverse result) at)))

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
  (unless (and steps (stringp (first steps)) (plusp (length (first steps))))
    (return-from parse-predicate nil))
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
keyword and a string, or NIL: only a property the editor knows of, a
keyword it already has."
  (when (and (equal (first steps) "set!") (stringp (second steps)))
    (let ((key (find-symbol (string-upcase (second steps)) :keyword)))
      (and key (list key (and (stringp (third steps)) (third steps)))))))

(defun disagreeing-rules (helper grammar)
  "End HELPER, which compiled a query for GRAMMAR to what its text does not
say, and signal a QUERY-ERROR."
  (reply-malformed helper)
  (error 'query-error :grammar grammar :kind "Error" :line 0
                      :text "the program compiling the rules answered what they do not say"))

(define-condition query-source-error (error) ()
  (:documentation "Signalled when the text of a query cannot be read as tree-sitter reads it."))

(defun read-query-source (source)
  "Read SOURCE, the text of a tree-sitter query, as tree-sitter reads it, for
what the editor needs of it: the capture names, numbered in the order they
first come, as a vector; and a vector holding for each pattern, in order, a
cons of the octet it starts at and its predicates, each a list of the predicate's name (as
eq? or set!, without # or .) and its arguments, a capture's number or a
string. Signals QUERY-SOURCE-ERROR when SOURCE is not a query."
  (let ((at 0) (octet 0) (end (length source))
        (captures (make-array 0 :adjustable t :fill-pointer t))
        (patterns '()) (predicates '()))
    (labels ((next () (and (< at end) (char source at)))
             (advance ()
               (incf octet (let ((code (char-code (char source at))))
                             (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4))))
               (incf at))
             (fail () (error 'query-source-error))
             (skip ()
               (loop for c = (next)
                     do (cond ((null c) (return))
                              ((member c '(#\Space #\Tab #\Newline #\Return #\Page #\Vt)) (advance))
                              ((char= c #\;) (loop until (or (null (next)) (char= (next) #\Newline)) do (advance)))
                              (t (return)))))
             (ident-start-p () (let ((c (next))) (and c (or (alphanumericp c) (char= c #\_) (char= c #\-)))))
             (identifier ()
               (let ((from at))
                 (loop do (advance)
                       while (let ((c (next))) (and c (or (alphanumericp c) (member c '(#\_ #\- #\.))))))
                 (subseq source from at)))
             (string-literal ()
               ;; at the opening quote
               (advance)
               (with-output-to-string (out)
                 (loop (let ((c (next)))
                         (cond ((null c) (fail))
                               ((char= c #\Newline) (fail))
                               ((char= c #\") (advance) (return))
                               ((char= c #\\)
                                (advance)
                                (let ((e (or (next) (fail))))
                                  (write-char (case e (#\n #\Newline) (#\r #\Return) (#\t #\Tab) (#\0 (code-char 0)) (t e)) out)
                                  (advance)))
                               (t (write-char c out) (advance)))))))
             (capture ()
               ;; at the @
               (advance)
               (unless (ident-start-p) (fail))
               (let ((name (identifier)))
                 (unless (find name captures :test #'string=)
                   (vector-push-extend name captures))))
             (predicate ()
               ;; past the # or .
               (unless (ident-start-p) (fail))
               (let ((name (identifier)))
                 (unless (member (next) '(#\? #\!)) (fail))
                 (setf name (concatenate 'string name (string (next))))
                 (advance)
                 (skip)
                 (let ((steps (list name)))
                   (loop (let ((c (next)))
                           (cond ((null c) (fail))
                                 ((char= c #\)) (advance) (skip) (return))
                                 ((char= c #\@)
                                  (advance)
                                  (unless (ident-start-p) (fail))
                                  (let ((index (position (identifier) captures :test #'string=)))
                                    (unless index (fail))
                                    (push index steps)))
                                 ((char= c #\") (push (string-literal) steps))
                                 ((ident-start-p) (push (identifier) steps))
                                 (t (fail))))
                         (skip))
                   (push (nreverse steps) predicates))))
             (inside (closing)
               ;; the rest of a group, node or alternation, to its CLOSING character
               (loop (skip)
                     (let ((c (next)))
                       (cond ((null c) (fail))
                             ((char= c closing) (advance) (return))
                             ((member c '(#\) #\])) (fail))
                             (t (part))))))
             (part ()
               ;; one thing inside a pattern, its suffixes after it
               (let ((c (next)))
                 (cond ((char= c #\()
                        (advance) (skip)
                        (if (member (next) '(#\# #\.))
                            (progn (advance) (predicate))
                            (inside #\))))
                       ((char= c #\[) (advance) (inside #\]))
                       ((char= c #\") (string-literal))
                       ((char= c #\@) (capture))
                       ((member c '(#\+ #\* #\? #\. #\! #\: #\/)) (advance))
                       ((ident-start-p) (identifier))
                       (t (fail)))))
             (pattern ()
               ;; one pattern at the top, its captures and quantifiers after it
               (let ((c (next)))
                 (cond ((char= c #\() (part))
                       ((char= c #\[) (part))
                       ((char= c #\") (string-literal))
                       ((ident-start-p) (identifier) (when (eql (next) #\:) (advance) (skip) (pattern)))
                       ((char= c #\.) (advance) (skip) (pattern))
                       ((char= c #\!) (advance) (unless (ident-start-p) (fail)) (identifier))
                       (t (fail))))
               (loop (skip)
                     (let ((c (next)))
                       (cond ((member c '(#\+ #\* #\?)) (advance))
                             ((eql c #\@) (capture))
                             (t (return)))))))
      (skip)
      (loop while (< at end)
            do (let ((start octet))
                 (setf predicates '())
                 (pattern)
                 (skip)
                 (push (cons start (nreverse predicates)) patterns)))
      (values (coerce captures 'simple-vector) (coerce (nreverse patterns) 'simple-vector)))))

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

(sb-ext:defglobal **query-numbers** (list 0)
  "The number of the last query made, in a cons for atomic counting.")

(defun send-query (call number grammar source)
  "Ask over CALL that SOURCE be compiled for GRAMMAR as query NUMBER; the status of the reply."
  (call-begin call +op-query+)
  (let ((name (tuatara.text:string-octets (grammar-name grammar))))
    (call-u32 call number)
    (call-u32 call (length name))
    (call-octets call name :pad t)
    (call-octets call (sb-ext:string-to-octets source :external-format :utf-8)))
  ;; all a query's reply says comes from its text: a few octets for each of it;
  ;; a large query takes a while on a busy machine, which is slowness, not a hang
  (call-send call :most (+ 1024 (* 16 (length source))) :seconds (query-compile-wait source) :late :slow))

(defparameter *query-compile-wait* 10
  "The seconds compiling rules may take before the program compiling them is
given up on, as slow, for rules of up to 10,000 characters; a second more
for each 10,000 more.")

(defun query-compile-wait (source)
  "The seconds compiling SOURCE, the text of rules, may take: *QUERY-COMPILE-WAIT*
and a second for each 10,000 characters past the first 10,000, and never
less than *SYNTAX-HELPER-WAIT*."
  (max *syntax-helper-wait* (+ *query-compile-wait* (floor (max 0 (- (length source) 10000)) 10000))))

(defun compile-query-text (grammar source)
  "The work of COMPILE-QUERY: compile SOURCE, a tree-sitter query, for GRAMMAR,
in the helper compiling rules, which is never given any text to parse.
Signals QUERY-NOT-ANSWERED when that helper ended or did not answer in
time (QUERY-COMPILE-WAIT): the rules may compile when asked again."
  (let ((number (1+ (sb-ext:atomic-incf (car **query-numbers**))))
        (helper (rules-helper)))
    (with-call (call helper +op-query+)
      (let ((status (send-query call number grammar source)))
        (unless (eql 0 status)
          (if (eq status :dead)
              (error 'query-not-answered :grammar grammar)
              (error 'query-error :grammar grammar :kind "Error" :line 0 :text "the program that parses did not answer"))))
      (when (= 1 (reply-u32 call 0))
        (let* ((octets (sb-ext:string-to-octets source :external-format :utf-8))
               (offset (reply-u32 call 1)) (kind (reply-u32 call 2))
               (at (min offset (length octets)))
               (line-start (1+ (or (position 10 octets :end at :from-end t) -1)))
               (line-end (or (position 10 octets :start at) (length octets))))
          (error 'query-error :grammar grammar
                              :kind (if (< kind (length *query-error-kinds*)) (aref *query-error-kinds* kind) "Error")
                              :line (1+ (count 10 octets :end at))
                              :text (tuatara.text:lenient-string octets :start line-start :end line-end))))
      ;; the rules as the editor reads them, the helper's answer held to them,
      ;; each count checked before anything is made of it
      (multiple-value-bind (own-captures own-patterns)
          (handler-case (read-query-source source)
            (query-source-error () (values #() :unread)))
        (unless (and (vectorp own-patterns) (= (reply-count call 4 4) (length own-captures)))
          (disagreeing-rules helper grammar))
      (let* ((captures (make-array (length own-captures)))
             (at 8))
        (dotimes (i (length captures))
          (let ((length (reply-u32-at call at)))
            (unless (= length (length (tuatara.text:string-octets (aref own-captures i))))
              (disagreeing-rules helper grammar))
            (setf (aref captures i) (reply-string call (+ at 4) length))
            (incf at (+ 4 (* 4 (ceiling length 4))))))
          (unless (every #'string= own-captures captures)
            (disagreeing-rules helper grammar))
        (let* ((patterns (if (= (reply-count call at 12) (length own-patterns))
                             (length own-patterns)
                             (disagreeing-rules helper grammar)))
               (predicates (make-array patterns)) (starts (make-array patterns)) (properties (make-array patterns))
               (non-local '()))
          (incf at 4)
          (dotimes (i patterns)
            (setf (aref starts i) (reply-u32-at call at))
            (when (= 1 (reply-u32-at call (+ at 4))) (push i non-local))
            (multiple-value-bind (steps after)
                (reply-predicate-steps call (+ at 12)
                                       (let ((count (reply-count call (+ at 8) 8))
                                             (own (cdr (elt own-patterns i))))
                                         ;; each predicate its steps and an end
                                         (if (= count (loop for predicate in own sum (1+ (length predicate))))
                                             count
                                             (disagreeing-rules helper grammar)))
                                       ;; no string longer than the rules' text, in octets
                                       (* 4 (length source)))
              (let ((own (and (< i (length own-patterns)) (elt own-patterns i))))
                (unless (and own (= (car own) (aref starts i)) (equal (cdr own) steps))
                  (disagreeing-rules helper grammar)))
              (setf at after
                    (aref predicates i) (remove nil (mapcar (lambda (step) (parse-predicate step (grammar-symbols grammar))) steps))
                    (aref properties i) (loop for (key value) in (remove nil (mapcar #'parse-directive steps))
                                              append (list key value)))))
          (unless (= patterns (length own-patterns))
            (disagreeing-rules helper grammar))
          (setf (gethash number (helper-queries helper)) t)
          (let ((query (%make-query :grammar grammar :number number :source source :captures captures
                                    :predicates predicates :starts starts :properties properties
                                    :non-local (nreverse non-local))))
            (sb-ext:finalize query (lambda () (forget-query number)) :dont-save t)
            query)))))))

(defun forget-query (number)
  "Have every helper let go of the query NUMBER, no longer used."
  (dolist (helper (sb-thread:with-mutex (**helpers-lock**) (copy-list **helpers**)))
    (when (remhash number (helper-queries helper))
      (with-call (call helper +op-query-delete+)
        (call-u32 call number)
        (call-send call)))))

(defun ensure-query (helper query)
  "Have HELPER hold QUERY, compiling it there the first time; true when it does."
  (or (gethash (query-number query) (helper-queries helper))
      (with-call (call helper +op-query+)
        (when (and (eql 0 (send-query call (query-number query) (query-grammar query) (query-source query)))
                   (= 0 (reply-u32 call 0)))
          (dolist (pattern (query-disabled query))
            (call-begin call +op-disable+)
            (call-u32 call (query-number query))
            (call-u32 call pattern)
            (call-send call))
          (setf (gethash (query-number query) (helper-queries helper)) t)))))

(defun query-non-local-patterns (query)
  "The numbers of QUERY's patterns with more than one node at their root, such
as a sibling sequence outside any parent: colouring a window with such a
query cannot skip the text before the window, so it slows down in large files."
  (query-non-local query))

(defun disable-query-pattern (query pattern)
  "Make pattern number PATTERN of QUERY match nothing from now on."
  (pushnew pattern (query-disabled query))
  (dolist (helper (sb-thread:with-mutex (**helpers-lock**) (copy-list **helpers**)))
    (when (gethash (query-number query) (helper-queries helper))
      (with-call (call helper +op-disable+)
        (call-u32 call (query-number query))
        (call-u32 call pattern)
        (call-send call)))))

;;; Running queries

(defconstant +most-captures+ 64
  "The most captures of one match looked at.")

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
             (holds (progn
                      ;; past the text the tests may read, the test does not pass
                      (unless (capture-text-read
                               (let ((n (- end start)))
                                 (ecase kind
                                   (:match n)
                                   (:eq (cond ((integerp argument) (* 2 n)) ((= n (length argument)) n) (t 0)))
                                   (:any-of (if (<= n (car argument)) n 0)))))
                        (return nil))
                      (ecase kind
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
                                    t))))))
        (when (eq holds negated) (return nil))))))

(defvar *prefetched* nil
  "Matches asked for ahead, all in one request: (CALL . ENTRIES), ENTRIES a
vector of six things for each: QUERY TREE START END AT SIZE, the matches of
QUERY in TREE from START below END being SIZE octets of CALL's reply from AT;
NIL when there are none.")

(defun most-matches (start end)
  "The most matches a reply may give for the text from START below END: a
few for each octet of it, and some, past which the reply cannot be right."
  (+ (* 4 (max 0 (- end start))) 4096))

(defun matches-reply-most (start end)
  "The most octets a reply of the matches for the text from START below END may hold."
  (* (most-matches start end) (+ 8 (* 16 +most-captures+))))

(defvar *capture-text-left* most-positive-fixnum
  "The octets of captured text the tests of a reply's matches may still read
\(DECODE-MATCHES): past them, a test of a text is taken as not passing.")

(defun capture-text-read (octets)
  "True when OCTETS more of captured text may be read by a match's test,
counted against *CAPTURE-TEXT-LEFT*; else NIL, said once in the debug log."
  (cond ((zerop octets) t)
        ((<= octets *capture-text-left*) (decf *capture-text-left* octets) t)
        (t (when (/= *capture-text-left* -1)
             (setf *capture-text-left* -1)
             (tuatara.runtime:debug-report :parse "matches' tests read too much text: the rest taken as not passing"))
           nil)))

(defun decode-matches (function query rope helper sap at size start end)
  "Call FUNCTION as MAP-QUERY-MATCHES does for each match of QUERY in the SIZE
octets at AT of SAP, a reply of HELPER about the tree of ROPE from START
below END, those passing their predicates. A reply that cannot be right (a
pattern or a capture the query does not have, a node outside the text, more
matches than the text asked about could have) ends HELPER, the rest of it
left alone. Tests of text past what that text holds many times over are
taken as not passing (CAPTURE-TEXT-READ). A capture may lie outside the text asked about: a match's other
nodes may lie far from the one that touches it."
  (declare (type function function) (type fixnum at size start end))
  (when (<= size 0)
    (return-from decode-matches nil))
  (let ((captures (make-array (* 4 +most-captures+) :element-type 'fixnum))
        (predicates (query-predicates query))
        (patterns (length (query-predicates query)))
        (names (length (query-captures query)))
        (bytes (tuatara.text:rope-bytes rope))
        (matches-left (most-matches start end))
        (stop (+ at size))
        (*capture-text-left* (+ (* 8 (max 0 (- end start))) (* 1024 1024))))
    (declare (type fixnum patterns names bytes stop matches-left) (dynamic-extent captures))
    (loop while (< at stop)
                      do (let ((pattern (sb-sys:sap-ref-32 sap at))
                               (count (if (<= (+ at 8) stop) (sb-sys:sap-ref-32 sap (+ at 4)) most-positive-fixnum)))
                           (unless (and (< pattern patterns) (<= count +most-captures+) (<= (+ at 8 (* 16 count)) stop)
                                        (plusp matches-left))
                             (return (reply-malformed helper)))
                           (decf matches-left)
                           (dotimes (i count)
                             (let ((number (sb-sys:sap-ref-32 sap (+ at 8 (* 16 i))))
                                   (s (sb-sys:sap-ref-32 sap (+ at 12 (* 16 i))))
                                   (e (sb-sys:sap-ref-32 sap (+ at 16 (* 16 i)))))
                               (unless (and (< number names) (<= s e bytes))
                                 (return-from decode-matches (reply-malformed helper)))
                               (setf (aref captures (* 4 i)) number
                                     (aref captures (+ 1 (* 4 i))) s
                                     (aref captures (+ 2 (* 4 i))) e
                                     (aref captures (+ 3 (* 4 i))) (sb-sys:sap-ref-32 sap (+ at 20 (* 16 i))))))
                           (incf at (+ 8 (* 16 count)))
                           (when (loop for test in (aref predicates pattern)
                                       always (test-passes-p test rope captures count))
                             (funcall function pattern captures count))))))

(defun prefetched-matches (query tree start end)
  "Where the matches of QUERY in TREE from START below END are among those
asked for ahead (*PREFETCHED*): the call holding them, the octet they start
at and their octets, as three values; NIL when they were not asked for."
  (when *prefetched*
    (let ((entries (cdr *prefetched*)))
      (declare (type simple-vector entries))
      (loop for i from 0 below (length entries) by 6
            when (and (eq (svref entries i) query) (eq (svref entries (+ i 1)) tree)
                      (= (svref entries (+ i 2)) start) (= (svref entries (+ i 3)) end))
              return (values (car *prefetched*) (svref entries (+ i 4)) (svref entries (+ i 5)))))))

(defun map-query-matches (function query tree rope start end)
  "Call FUNCTION with (PATTERN CAPTURES COUNT) for each match of QUERY in TREE
(the tree of ROPE) that touches octets START below END and passes its
predicates: PATTERN is the pattern's number, and CAPTURES a fixnum vector
holding COUNT captures as four numbers each: the capture's number, the octets
its node starts and ends at, and the node's symbol. CAPTURES is reused for the
next match and gone once MAP-QUERY-MATCHES returns, so it must not be kept.
Matches with more than +MOST-CAPTURES+ captures are cut short. Nothing
when the helper holding TREE has ended. The matches asked for ahead
\(CALL-WITH-PREFETCHED-MATCHES) are taken from those."
  (declare (type function function))
  (let ((helper (tree-helper tree))
        (start (max 0 start)) (end (max 0 end)))
    (multiple-value-bind (call at size) (prefetched-matches query tree start end)
      (if call
          (unless (= size #xffffffff)
            (decode-matches function query rope helper (call-in call) at size start end))
          (when (ensure-query helper query)
            (with-call (call helper +op-matches+)
              (call-u32 call (query-number query)) (call-u32 call (tree-number tree))
              (call-u32 call start) (call-u32 call end)
              ;; a whole text's matches may take a while: a second a megabyte more,
              ;; and past it given up as slow, not the helper's fault
              (when (eql 0 (call-send call :most (matches-reply-most start end)
                                           :seconds (+ *syntax-helper-wait* (/ (max 0 (- end start)) (* 1024 1024)))
                                           :late :slow))
                (decode-matches function query rope helper (call-in call) 0 (call-in-size call) start end))))))))

(defun prefetch-entries (call specs tree at)
  "The entries of *PREFETCHED* for SPECS, each (QUERY TREE START END), or
\(QUERY START END) with TREE given for all, whose matches follow one another
in CALL's reply from octet AT, each after its octets; NIL when the reply
cannot hold them."
  (let ((size (call-in-size call))
        (entries (make-array (* 6 (length specs)))))
    (loop for spec in specs
          for i from 0 by 6
          for length = (and (<= (+ at 4) size) (sb-sys:sap-ref-32 (call-in call) at))
          unless (and length (or (= length #xffffffff) (<= (+ at 4 length) size)))
            return nil
          do (let ((rest (if tree (cdr spec) (cddr spec))))
               (setf (svref entries i) (first spec) (svref entries (+ i 1)) (or tree (second spec))
                     (svref entries (+ i 2)) (max 0 (first rest)) (svref entries (+ i 3)) (max 0 (second rest))
                     (svref entries (+ i 4)) (+ at 4) (svref entries (+ i 5)) length))
             (incf at (+ 4 (if (= length #xffffffff) 0 length)))
          finally (return entries))))

(defun call-with-prefetched-matches (helper specs function)
  "Call FUNCTION with the matches of each of SPECS, (QUERY TREE START END),
those of TREEs HELPER holds, asked for ahead in one request, so that
MAP-QUERY-MATCHES takes them from there; FUNCTION's values."
  (let ((specs (flet ((usable (spec) (and (first spec) (second spec) (ensure-query helper (first spec)))))
                 (if (every #'usable specs) specs (remove-if-not #'usable specs)))))
    (if (or (null specs) (> (length specs) 64))
        (funcall function)
        (let ((result
                (with-call (call helper +op-matches-many+)
                  (call-u32 call (length specs))
                  (loop for (query tree start end) in specs
                        do (call-u32 call (query-number query)) (call-u32 call (tree-number tree))
                           (call-u32 call (max 0 start)) (call-u32 call (max 0 end)))
                  (let ((entries (and (eql 0 (call-send call :most (loop for (nil nil start end) in specs
                                                                         sum (+ 4 (matches-reply-most (max 0 start) (max 0 end))))))
                                      (prefetch-entries call specs nil 0))))
                    (multiple-value-list
                     (if entries
                         (let ((*prefetched* (cons call entries))) (funcall function))
                         (funcall function)))))))
          (if result (values-list result) (funcall function))))))

;;; Parsing text in another language

(defparameter *given-text-most* (* 256 1024)
  "The most octets of text in another language sent with the request to parse
it; a longer one is asked for as a parse asks for its text.")

(defstruct (text-parse (:constructor make-text-parse (grammar ranges tree queries limit budget)) (:copier nil))
  "Text in another language to parse with others in one request
\(PARSE-TEXTS): RANGES of the text, each (START . END), in order and apart,
in GRAMMAR, with LIMIT seconds and BUDGET octets as PARSE-ROPE takes them;
or TREE, one made already, in place of all those. QUERIES are run on the
tree, each (QUERY START END)."
  (grammar nil :read-only t)
  (ranges '() :read-only t)
  (tree nil :read-only t)
  (queries '() :read-only t)
  (limit nil :read-only t)
  (budget nil :read-only t))

(setf (documentation 'text-parse-grammar 'function) "The grammar to parse the text in."
      (documentation 'text-parse-ranges 'function) "The ranges of the text to parse."
      (documentation 'text-parse-tree 'function) "The tree made already, in place of parsing."
      (documentation 'text-parse-queries 'function) "The queries to run on the tree, each (QUERY START END)."
      (documentation 'text-parse-limit 'function) "The seconds the parse may take."
      (documentation 'text-parse-budget 'function) "The octets of memory the parse may take."
      (documentation 'text-parse-p 'function) "True when OBJECT is a TEXT-PARSE.")

(defun write-text-parses (call helper rope parses)
  "Write the request of CALL to HELPER parsing PARSES, TEXT-PARSEs of ROPE's
text, the queries of each those HELPER knows; the seconds they may take in
all."
  (let ((low (tuatara.text:rope-bytes rope)) (high 0) (seconds 0))
    (call-u32 call (length parses))
    (dolist (parse parses)
      (let ((tree (text-parse-tree parse)))
        (if tree
            (call-u32 call (tree-number tree))
            (let ((name (tuatara.text:string-octets (grammar-name (text-parse-grammar parse))))
                  (ranges (text-parse-ranges parse))
                  (limit (text-parse-limit parse))
                  (budget (text-parse-budget parse)))
              (call-u32 call 0)
              (call-u32 call (length name))
              (call-octets call name :pad t)
              (call-u32 call (length ranges))
              (loop for (start . end) in ranges
                    do (let ((from (tuatara.text:byte->line rope start))
                             (to (tuatara.text:byte->line rope end)))
                         (setf low (min low start) high (max high end))
                         (call-u32 call start) (call-u32 call end)
                         (call-u32 call from) (call-u32 call (- start (tuatara.text:line->byte rope from)))
                         (call-u32 call to) (call-u32 call (- end (tuatara.text:line->byte rope to)))))
              (call-u32 call (if limit (max 1 (min #xffffffff (round (* limit 1000)))) 0))
              (call-u64 call (if budget (max 1 (min (1- (expt 2 63)) (round budget))) 0))
              (incf seconds (or limit 86400)))))
      (let ((queries (text-parse-queries parse)))
        (call-u32 call (count-if (lambda (spec) (ensure-query helper (first spec))) queries))
        (loop for (query start end) in queries
              when (ensure-query helper query)
                do (call-u32 call (query-number query)) (call-u32 call (max 0 start)) (call-u32 call (max 0 end)))))
    ;; the text itself, when it is not long, so that the parses need not ask for it
    (let ((length (if (< low high (+ low *given-text-most*)) (- high low) 0)))
      (call-u32 call low)
      (call-u32 call length)
      (when (plusp length)
        (call-room call length)
        (tuatara.text:rope-copy-to-sap rope low high (sb-sys:sap+ (call-out call) (call-out-size call)))
        (incf (call-out-size call) length)))
    (values seconds high)))

(defun read-text-parses (call helper parses input)
  "The trees and why of each of PARSES from the reply of CALL from HELPER, as
a list of (TREE WHY) as PARSE-ROPE gives them, and the entries of
*PREFETCHED* for the matches of their queries, as two values; NIL when the
reply cannot be right, which ends HELPER."
  (let* ((sap (call-in call)) (size (call-in-size call)) (at 0)
         (entries (make-array (* 6 (loop for parse in parses
                                         sum (count-if (lambda (spec) (ensure-query helper (first spec)))
                                                       (text-parse-queries parse))))))
         (e 0) (made '()))
    (flet ((malformed ()
             (dolist (tree made) (delete-tree tree))
             (return-from read-text-parses (reply-malformed helper))))
      (values
       (loop for parse in parses
             collect (progn
                       (when (> (+ at 8) size) (malformed))
                       (let* ((number (sb-sys:sap-ref-32 sap at))
                              (code (sb-sys:sap-ref-32 sap (+ at 4)))
                              (given (text-parse-tree parse))
                              (tree (cond ((zerop number) nil)
                                          (given (if (= number (tree-number given)) given (malformed)))
                                          ((or (parse-input-failed input) (parse-input-over input))
                                           (delete-tree (%make-tree helper number)) nil)
                                          (t (first (push (%make-tree helper number) made))))))
                         (incf at 8)
                         (loop for (query start end) in (text-parse-queries parse)
                               when (ensure-query helper query)
                                 do (let ((length (if (<= (+ at 4) size) (sb-sys:sap-ref-32 sap at) (malformed))))
                                      (unless (or (= length #xffffffff) (<= (+ at 4 length) size)) (malformed))
                                      (setf (svref entries e) query (svref entries (+ e 1)) tree
                                            (svref entries (+ e 2)) (max 0 start) (svref entries (+ e 3)) (max 0 end)
                                            (svref entries (+ e 4)) (+ at 4) (svref entries (+ e 5)) length)
                                      (incf e 6)
                                      (incf at (+ 4 (if (= length #xffffffff) 0 length)))))
                         (list tree (and (null tree) (not given)
                                         (if (parse-input-over input)
                                             :slow
                                             (case code (2 :slow) (3 :memory) (4 :failed) ((6 7) :short))))))))
       entries))))

(defun parse-texts (rope parses function &key (helper (current-helper)))
  "Parse PARSES, TEXT-PARSEs of ROPE's text, in HELPER (the one parsing the
text worked on now unless given), each with a parser HELPER keeps for its
grammar, and run their queries on their trees, all in one request. Calls
FUNCTION with a list of (TREE WHY) for each as PARSE-ROPE gives them, while
MAP-QUERY-MATCHES takes the matches of their queries from those found;
FUNCTION's values. Each is (NIL :CRASHED) when HELPER ended first."
  (let* ((input nil)
         (ran nil)
         (result
           (with-call (call helper +op-parse-ranges+)
             (multiple-value-bind (seconds high) (write-text-parses call helper rope parses)
               (setf input (make-parse-input rope high 0
                                             (loop for parse in parses
                                                   sum (loop for (start . end) in (text-parse-ranges parse)
                                                             sum (- end start)))))
               (let ((status (while-parsing (helper)
                               (call-send call :input input :seconds (+ seconds *syntax-helper-wait*)
                                               :most (loop for parse in parses
                                                           sum (+ 8 (loop for (nil start end) in (text-parse-queries parse)
                                                                          sum (+ 4 (matches-reply-most (max 0 start)
                                                                                                       (max 0 end))))))))))
                 (multiple-value-bind (trees entries)
                     (cond ((eq status :dead) nil)
                           ((/= 0 status) (values (mapcar (lambda (parse) (declare (ignore parse)) (list nil nil)) parses)
                                                  #()))
                           (t (read-text-parses call helper parses input)))
                   (when trees
                     (setf ran t)
                     (multiple-value-list
                      (let ((*prefetched* (cons call entries)))
                        (funcall function trees))))))))))
    (if ran
        (values-list result)
        (let ((why (helper-gone-why helper)))
          (funcall function (mapcar (lambda (parse) (declare (ignore parse)) (list nil why)) parses))))))

(defun parse-rope-ranges (grammar rope ranges &key limit budget (helper (current-helper)))
  "Parse RANGES of ROPE's text, each (START . END) in octets, in order and
apart, in GRAMMAR, in HELPER (the one parsing the text worked on now unless
given), as one text in another language, with a parser HELPER keeps for
GRAMMAR. The tree, or NIL with why as PARSE-ROPE says; LIMIT and BUDGET as
for PARSE-ROPE."
  (parse-texts rope (list (make-text-parse grammar ranges nil '() limit budget))
               (lambda (trees) (values-list (first trees)))
               :helper helper))
