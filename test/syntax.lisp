;;;; test/syntax.lisp --- Tree-sitter: grammars, rules, colours, trees kept up to date.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test syntax
  :description "Tree-sitter: the grammars load and the rules compile; colours as the rules give them
(rich), and fewer of them with the rules' precedence kept (sparse); trees kept up to date
through edits, undo and whole-text changes, parsed in the background; the language modes
and the colouring options.")

(defun syntax-sample (name)
  "The path of the colouring sample NAME in test/syntax/."
  (asdf:system-relative-pathname "tuatara" (format nil "test/syntax/~a" name)))

(defun language-named (name)
  "The language called NAME."
  (tuatara.defaults::find-language name))

(defun parse-string (grammar text)
  "Parse TEXT with the grammar called GRAMMAR; return the tree, the rope and
the parser. Release them with RELEASE-PARSE."
  (let* ((g (tuatara.syntax:find-grammar grammar))
         (parser (tuatara.syntax:make-parser g (tuatara.syntax:kept-helper :shared)))
         (rope (tuatara.text:string->rope text)))
    (values (tuatara.syntax:parse-rope parser rope) rope parser)))

(defun release-parse (tree parser)
  "Release TREE and PARSER, made by PARSE-STRING."
  (tuatara.syntax:delete-tree tree)
  (tuatara.syntax:delete-parser parser))

(defun rich-ranges (grammar text query-source)
  "The rich colours QUERY-SOURCE gives TEXT in GRAMMAR, faces named after the captures."
  (multiple-value-bind (tree rope parser) (parse-string grammar text)
    (unwind-protect
         (let ((query (tuatara.syntax:compile-query (tuatara.syntax:find-grammar grammar) query-source)))
           (tuatara.syntax:rich-highlights query tree rope 0 (tuatara.text:rope-octet-length rope)
                                           (map 'vector (lambda (name)
                                                          (cond ((string= name "none") :none)
                                                                ((char= (char name 0) #\_) nil)
                                                                (t (intern (string-upcase name) :keyword))))
                                                (tuatara.syntax:query-captures query))))
      (release-parse tree parser))))

(defun ranges-text (text ranges)
  "RANGES of ASCII TEXT as (TEXT FACE) pairs."
  (mapcar (lambda (r) (list (subseq text (first r) (second r)) (third r))) ranges))

(define-test (syntax rules-colour-windows-quickly)
  ;; no pattern keeps a query from skipping the text before the lines it
  ;; colours or indents
  (dolist (name '(rust bash lisp containerfile c))
    (let* ((language (language-named name))
           (grammar (tuatara.syntax:find-grammar (tuatara.defaults::language-grammar language))))
      (dolist (source (remove nil (list (tuatara.defaults::language-rules language)
                                        (tuatara.defaults::language-indents language)
                                        (tuatara.defaults::language-injections language)
                                        (tuatara.defaults::language-locals language))))
        (let ((query (tuatara.syntax:compile-query grammar source)))
          (is equal '()
              (let ((octets (sb-ext:string-to-octets source :external-format :utf-8)))
                (mapcar (lambda (pattern)
                          (1+ (count 10 octets :end (aref (tuatara.syntax:query-starts query) pattern))))
                        (tuatara.syntax:query-non-local query)))
              "lines of the ~(~a~) rules starting patterns with more than one root" name)))))
  ;; the languages indented by the tree have their rules, and they compile
  (dolist (name '(rust bash))
    (true (tuatara.defaults::language-compiled-query (language-named name) :indent)))
  (false (tuatara.defaults::language-indents (language-named 'lisp))))

(define-test (syntax grammars-and-rules)
  (dolist (name '("rust" "bash" "commonlisp" "dockerfile" "c"))
    (let ((grammar (tuatara.syntax:find-grammar name)))
      (true grammar)
      (true (plusp (length (tuatara.syntax:grammar-symbols grammar))))))
  (false (tuatara.syntax:find-grammar "no-such-grammar"))
  ;; the names in a grammar are its own: Vue's copy of HTML's scanner, and
  ;; Glimmer's of JavaScript's, never answer for HTML or JavaScript
  (dolist (name '("vue" "html" "glimmer-javascript" "javascript"))
    (true (tuatara.syntax:find-grammar name) "~a" name))
  (dolist (name '("tree_sitter_html_external_scanner_scan" "tree_sitter_javascript_external_scanner_scan" "tree_sitter_vue"))
    (false (sb-sys:find-dynamic-foreign-symbol-address name) "~a" name))
  (multiple-value-bind (tree rope parser) (parse-string "html"
                                                        (format nil "<p>a <b>b</b></p>~%<script>let x = 1;</script>~%"))
    (declare (ignore rope))
    (false (tuatara.syntax:tree-has-error-p tree))
    (release-parse tree parser))
  ;; every language's rules compile, and its sample parses without errors
  (loop for (name grammar sample) in '((rust "rust" "sample.rs")
                                       (bash "bash" "sample.sh")
                                       (lisp "commonlisp" "sample.lisp")
                                       (containerfile "dockerfile" "Containerfile") (c "c" "sample.c"))
        do (let ((language (language-named name)))
             (true language)
             (true (tuatara.defaults::language-compiled-query language :colouring))
             (multiple-value-bind (tree rope parser) (parse-string grammar
                                                                   (uiop:read-file-string (syntax-sample sample)))
               (declare (ignore rope))
               (false (tuatara.syntax:tree-has-error-p tree) "~a parses without errors" sample)
               (release-parse tree parser))))
  ;; a broken query says where
  (let ((error (handler-case (tuatara.syntax:compile-query (tuatara.syntax:find-grammar "rust") "(identifier) @a
(no_such_node) @b")
                 (tuatara.syntax:query-error (e) e))))
    (true error)
    (is = 2 (tuatara.syntax:query-error-line error))
    (is string= "Unknown node type" (tuatara.syntax:query-error-kind error))))

(define-test (syntax lisp-parses-cleanly)
  ;; loops with of-type, and strings with ~ that no format control reads, parse
  ;; without errors, the directives each one node
  (let ((text "(defun f (v) \"See ~/.config/tuatara/; ~ marks a change.\"
  (loop for i of-type fixnum from 0 below 3
        with total of-type (unsigned-byte 8) = 0
        for ,j of-type fixnum = (- i 1)
        do (format t \"~10@a ~:* ~,1f ~/pkg:fn/ ~a~%\" i total j)))"))
    (multiple-value-bind (tree rope parser) (parse-string "commonlisp" text)
      (unwind-protect
           (progn
             (false (tuatara.syntax:tree-has-error-p tree))
             (is equal '("~/" "~ " "~10@a" "~:*" "~,1f" "~/pkg:fn/" "~a" "~%")
                 (let ((found '())
                       (query (tuatara.syntax:compile-query (tuatara.syntax:find-grammar "commonlisp") "(format_specifier) @f")))
                   (tuatara.syntax:map-query-matches
                    (lambda (pattern captures count)
                      (declare (ignore pattern count))
                      (push (subseq text (aref captures 1) (aref captures 2)) found))
                    query tree rope 0 (length text))
                   (nreverse found))))
        (release-parse tree parser))))
  ;; block comments ended by ||#, and nil for an empty lambda list
  (dolist (text '("#|| a ||# (foo)" "(lambda nil 1)" "(defun f nil 1)"))
    (multiple-value-bind (tree rope parser) (parse-string "commonlisp" text)
      (declare (ignore rope))
      (false (tuatara.syntax:tree-has-error-p tree))
      (release-parse tree parser))))

(define-test (syntax lisp-loop-named-parses)
  ;; a loop given a name for return-from parses without errors, the name in a clause of its own
  (let ((text "(loop named outer for i below 3 do (when (= i 1) (return-from outer i)))"))
    (multiple-value-bind (tree rope parser) (parse-string "commonlisp" text)
      (unwind-protect
           (progn
             (false (tuatara.syntax:tree-has-error-p tree))
             (is equal '("named outer")
                 (let ((found '())
                       (query (tuatara.syntax:compile-query (tuatara.syntax:find-grammar "commonlisp")
                                                            "(named_clause) @n")))
                   (tuatara.syntax:map-query-matches
                    (lambda (pattern captures count)
                      (declare (ignore pattern count))
                      (push (subseq text (aref captures 1) (aref captures 2)) found))
                    query tree rope 0 (length text))
                   (nreverse found))))
        (release-parse tree parser)))))

(define-test (syntax latex-table-columns)
  ;; every & between a table's columns is coloured, those written up against
  ;; the text too (syntax/patches/latex.patch), an escaped \& is not
  (let ((text (format nil "\\begin{longtable}{lll}~%a & b & c \\\\~%d&e&\\textbf{f} \\\\~%\\end{longtable}~%Tom \\& Jerry~%")))
    (multiple-value-bind (tree rope parser) (parse-string "latex" text)
      (declare (ignore rope))
      (false (tuatara.syntax:tree-has-error-p tree))
      (release-parse tree parser))
    (let ((ranges (ranges-text text
                               (rich-ranges "latex" text (tuatara.defaults::language-rules (language-named 'latex))))))
      (is equal (make-list 4 :initial-element '("&" :punctuation.special.separator))
          (remove-if-not (lambda (range) (string= "&" (first range))) ranges))
      (is eq :function (second (assoc "\\&" ranges :test #'string=))))
    ;; in the sparse style too, from level 2, as keywords are
    (let* ((query (tuatara.defaults::language-compiled-query (language-named 'latex) :colouring))
           (at (position "punctuation.special.separator" (tuatara.syntax:query-captures query) :test #'string=)))
      (is eq :punctuation.special (aref (tuatara.defaults::query-faces query :sparse 2) at))
      (is eq :none (aref (tuatara.defaults::query-faces query :sparse 1) at)))))

(defun expression-walk (grammar marked direction)
  "The pieces of text successive moves by expression pass over in the text
MARKED, parsed with GRAMMAR, from the | in it (or, going :BACKWARD, the ^),
the markers taken out; each piece trimmed of blanks, and the last element
what stopped the walk (NIL at a bracket)."
  (let* ((text (remove #\^ (remove #\| marked :count 1) :count 1))
         (from (if (eq direction :forward)
                   (position #\| (remove #\^ marked :count 1))
                   (position #\^ (remove #\| marked :count 1))))
         (buffer (tuatara.buffer:make-buffer "*expressions*" :text text))
         (syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar grammar)))
         (tuatara.syntax:*syntax-wait* 30))
    (unwind-protect
         (progn
           (tuatara.syntax:syntax-ready-tree syntax)
           (loop with p = from
                 repeat 40
                 for next = (if (eq direction :forward)
                                (tuatara.syntax:expression-end syntax p)
                                (tuatara.syntax:expression-start syntax p))
                 if (integerp next)
                   collect (string-trim '(#\Space #\Newline)
                                        (if (eq direction :forward) (subseq text p next) (subseq text next p)))
                   and do (setf p next)
                 else collect next and do (loop-finish)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax expressions)
  ;; brackets in strings, comments and character literals never count; a
  ;; quoted form, a string and a construct opened by punctuation go whole;
  ;; punctuation and comments between expressions are passed over
  (let ((lisp "(list |'a '(b c) #'car pkg:sym :key s \"x ( \" #\\( ; c (
  z `(,a ,@b)^)"))
    (is equal '("'a" "'(b c)" "#'car" "pkg:sym" ":key" "s" "\"x ( \"" "#\\(" "; c (
  z" "`(,a ,@b)" nil)
        (expression-walk "commonlisp" lisp :forward))
    (is equal '("`(,a ,@b)" "z" "#\\( ; c (" "\"x ( \"" "s" ":key" "pkg:sym" "#'car" "'(b c)" "'a" "list" nil)
        (expression-walk "commonlisp" lisp :backward)))
  (let ((rust "fn f() { |let s = \"(\"; foo(a, b.c)[0] += &x; let c = '('; v.push(1) ^}"))
    (is equal '("let" "s" "= \"(\"" "; foo" "(a, b.c)" "[0]" "+= &x" "; let" "c" "= '('" "; v.push" "(1)" nil)
        (expression-walk "rust" rust :forward))
    (is equal '("(1)" "v.push" "'(';" "c =" "let" "&x;" "[0] +=" "(a, b.c)" "foo" "\"(\";" "s =" "let" nil)
        (expression-walk "rust" rust :backward)))
  (let ((bash "f() { |echo \"$(date) (\" # )
  x=${y:-z}; [[ -n $x ]] && echo \"$HOME\"; ^}"))
    (is equal '("echo" "\"$(date) (\"" "# )
  x" "=${y:-z}" "; [[ -n $x ]]" "&& echo" "\"$HOME\"" nil)
        (expression-walk "bash" bash :forward))
    (is equal '("\"$HOME\";" "echo" "[[ -n $x ]] &&" "${y:-z};" "x=" "\"$(date) (\" # )" "echo" nil)
        (expression-walk "bash" bash :backward)))
  ;; the brackets around a place, innermost first, at a bracket or inside a string
  (let* ((text "(defun f (x) \"doc (\" ; a (
  (let ((s #\\( )) (list 'a \"x ) y\")))")
         (buffer (tuatara.buffer:make-buffer "*pairs*" :text text))
         (syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "commonlisp")))
         (tuatara.syntax:*syntax-wait* 30))
    (unwind-protect
         (flet ((pairs (at count)
                  (mapcar (lambda (pair) (subseq text (car pair) (cdr pair)))
                          (tuatara.syntax:enclosing-pairs syntax at count))))
           (tuatara.syntax:syntax-ready-tree syntax)
           (is equal '("(list 'a \"x ) y\")" "(let ((s #\\( )) (list 'a \"x ) y\"))")
               (pairs (search "x )" text) 2))
           (is equal '("(list 'a \"x ) y\")") (pairs (search "(list" text) 1))
           (is equal '("(s #\\( )") (pairs (search "#\\(" text) 1))
           (is equal 3 (length (pairs (search "'a" text) 9))))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax rich-precedence)
  ;; of captures of the same text the later pattern wins; an inner node's
  ;; colour shows over an outer one's; none takes the colour away
  (let ((text "fn f() { let X = y; \"a\\nb\"; skip; }"))
    (is equal '(("X" :constant) ("y" :variable) ("\"a\\nb\"" :string) ("\\n" :escape) ("skip" :variable))
        (remove-if (lambda (pair) (member (second pair) '(:function)))
                   (ranges-text text (rich-ranges "rust" text "
(identifier) @variable
((identifier) @constant (#match? @constant \"^[A-Z]\"))
(string_literal) @string
(escape_sequence) @escape
(function_item name: (identifier) @function)"))))
    (is equal '(("f" :variable) ("X" :variable))
        (ranges-text text (rich-ranges "rust" text "
(identifier) @variable
((identifier) @none (#any-of? @none \"y\" \"skip\"))")))))

(define-test (syntax predicates)
  (let ((text "fn main() { let alpha = beta; let beta = gamma; }"))
    (flet ((names (query) (mapcar #'first (ranges-text text (rich-ranges "rust" text query)))))
      (is equal '("beta" "beta") (names "((identifier) @x (#eq? @x \"beta\"))"))
      (is equal '("main" "alpha" "gamma") (names "((identifier) @x (#not-eq? @x \"beta\"))"))
      (is equal '("main" "alpha") (names "((identifier) @x (#match? @x \"^[am]\"))"))
      (is equal '("beta" "beta" "gamma") (names "((identifier) @x (#not-match? @x \"^[am]\"))"))
      (is equal '("alpha" "gamma") (names "((identifier) @x (#any-of? @x \"alpha\" \"gamma\"))"))
      (is equal '("main" "beta" "beta") (names "((identifier) @x (#not-any-of? @x \"alpha\" \"gamma\"))"))
      ;; two captures compared
      (is equal '("beta") (names "(let_declaration pattern: (identifier) @x value: (identifier) @_v (#eq? @x \"beta\") (#not-eq? @x @_v))"))
      ;; a regular expression the editor cannot read never matches
      (is equal '() (names "((identifier) @x (#match? @x \"[\"))"))))
  ;; the lines nodes begin on, and their kinds, as indentation rules test them
  (let ((text (format nil "fn main() {~%    let alpha =~%        beta;~%    let gamma = delta;~%}")))
    (flet ((names (query) (mapcar #'first (ranges-text text (rich-ranges "rust" text query)))))
      (is equal '("alpha") (names "(let_declaration pattern: (_) @x value: (_) @_v (#not-same-line? @x @_v))"))
      (is equal '("gamma") (names "(let_declaration pattern: (_) @x value: (_) @_v (#same-line? @x @_v))"))
      ;; a capture the match lacks fails the test
      (is equal '() (names "(let_declaration pattern: (_) @x alternative: (_)? @_a (#same-line? @x @_a))"))
      (is equal '("let gamma = delta;") (names "((let_declaration) @x (#one-line? @x))"))
      (is equal '("{~%    let alpha =~%        beta;~%    let gamma = delta;~%}")
          (mapcar (lambda (s) (cl-ppcre:regex-replace-all (string #\Newline) s "~%"))
                  (names "((block) @x (#not-one-line? @x))")))
      (is equal '("main") (names "((identifier) @x (#not-kind-eq? @x \"type_identifier\") (#match? @x \"^m\"))"))
      (is equal '() (names "((identifier) @x (#kind-eq? @x \"type_identifier\"))")))))

(define-test (syntax sparse-keeps-precedence)
  ;; the sparse style leaves kinds plain by level, and what it leaves plain
  ;; still wins its text: the colours left are the rich style's
  (let* ((query (tuatara.defaults::language-compiled-query (language-named 'rust) :colouring))
         (captures (tuatara.syntax:query-captures query)))
    (flet ((face (name style level)
             (aref (tuatara.defaults::query-faces query style level) (position name captures :test #'string=))))
      (is eq :none (face "variable" :sparse 3))
      (is eq :none (face "variable.parameter" :sparse 3))
      (is eq :none (face "punctuation.bracket" :sparse 3))
      (is eq :none (face "operator" :sparse 3))
      (is eq :keyword (face "keyword.control.conditional" :sparse 3))
      (is eq :constant.numeric (face "constant.numeric.integer" :sparse 3))
      (is eq :comment (face "comment.line" :sparse 1))
      (is eq :none (face "keyword" :sparse 1))
      (is eq :function.macro (face "function.macro" :sparse 2))
      (is eq :none (face "type" :sparse 2))
      (is eq :punctuation.bracket (face "punctuation.bracket" :sparse 4))
      (is eq :variable (face "variable" :rich 0))))
  (let* ((text "fn f(p: u8) { g(p); }")
         (query (tuatara.syntax:compile-query (tuatara.syntax:find-grammar "rust") "
(identifier) @function
((identifier) @variable.parameter (#eq? @variable.parameter \"p\"))")))
    (multiple-value-bind (tree rope parser) (parse-string "rust" text)
      (unwind-protect
           (is equal '(("f" :function) ("g" :function))
               (ranges-text text (tuatara.syntax:rich-highlights query tree rope 0 (length text)
                                                                 (tuatara.defaults::query-faces query :sparse 3))))
        (release-parse tree parser)))))

(defun full-rich-ranges (syntax query)
  "The rich colours of all of SYNTAX's buffer, waiting for its parse to finish."
  (let ((tuatara.syntax:*syntax-wait* 30)
        (buffer (tuatara.syntax::syntax-buffer syntax)))
    (first (tuatara.syntax:syntax-highlights syntax query (tuatara.defaults::query-faces query :rich 0)
                                             (list (cons 0 (tuatara.buffer:buffer-size buffer)))))))

(defun fresh-rich-ranges (grammar text query)
  "The rich colours of TEXT parsed from scratch with GRAMMAR."
  (multiple-value-bind (tree rope parser) (parse-string grammar text)
    (unwind-protect (tuatara.syntax:rich-highlights query tree rope 0 (tuatara.text:rope-octet-length rope)
                                                    (tuatara.defaults::query-faces query :rich 0))
      (release-parse tree parser))))

(define-test (syntax edits-keep-the-tree)
  ;; after each of many edits, the tree kept up to date colours the text as a
  ;; tree parsed afresh does; the edits include text of several octets a character
  (let* ((text (uiop:read-file-string (syntax-sample "sample.rs")))
         (buffer (tuatara.buffer:make-buffer "*syntax-edits*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (syntax (tuatara.syntax:attach-syntax buffer grammar))
         (random (sb-ext:seed-random-state 20260927))
         (snippets #("x" "\"" "{" "}" "// é → note
" "let a = 1;" "/*" "*/" "fn g() {}
" "'" "\\" "    " "
" "\"ünïcödé\"")))
    (unwind-protect
         (progn
           (is equal (fresh-rich-ranges "rust" text query) (full-rich-ranges syntax query))
           (dotimes (i 150)
             (let* ((string (tuatara.buffer:buffer-string buffer))
                    (at (tuatara.text:char->byte (tuatara.buffer:buffer-text buffer)
                                                 (random (1+ (length string)) random))))
               (if (and (plusp (length string)) (< (random 3 random) 1))
                   (let ((end (tuatara.text:char->byte (tuatara.buffer:buffer-text buffer)
                                                       (min (length string) (+ (tuatara.text:byte->char (tuatara.buffer:buffer-text buffer) at)
                                                                               1 (random 12 random))))))
                     (tuatara.buffer:buffer-delete buffer at end))
                   (tuatara.buffer:buffer-insert buffer at (aref snippets (random (length snippets) random))))
               (when (zerop (mod i 10))
                 (is equal (fresh-rich-ranges "rust" (tuatara.buffer:buffer-string buffer) query)
                     (full-rich-ranges syntax query)
                     "after edit ~a" i))))
           ;; undo all the way back, and redo some
           (loop while (tuatara.buffer:buffer-undo buffer))
           (is string= text (tuatara.buffer:buffer-string buffer))
           (is equal (fresh-rich-ranges "rust" text query) (full-rich-ranges syntax query))
           (dotimes (i 20) (tuatara.buffer:buffer-redo buffer))
           (is equal
               (fresh-rich-ranges "rust" (tuatara.buffer:buffer-string buffer) query)
               (full-rich-ranges syntax query))
           ;; a change of the whole text is parsed afresh
           (tuatara.buffer:buffer-swap-text buffer (tuatara.text:string->rope "fn swapped() {}")
                                            (lambda (p b) (declare (ignore b)) (min p 15))
                                            (lambda (p b) (declare (ignore b)) p))
           (is equal (fresh-rich-ranges "rust" "fn swapped() {}" query) (full-rich-ranges syntax query)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax parses-in-the-background)
  ;; a large text is parsed without holding up the editor, and says so when done
  (let* ((text (with-output-to-string (s)
                 (let ((sample (uiop:read-file-string (syntax-sample "sample.rs"))))
                   (dotimes (i 1500) (write-string sample s)))))
         (buffer (tuatara.buffer:make-buffer "*syntax-large*" :text text))
         (woken 0)
         (tuatara.runtime:*host* (make-answering-host 'tuatara.runtime:host-wake (lambda () (incf woken)))))
    (unwind-protect
         (let* ((start (thread-cpu-time))
                (syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (true (< (- (thread-cpu-time) start) (* 0.1 internal-time-units-per-second)))
           (true (tuatara.syntax:syntax-job syntax))
           (let ((tuatara.syntax:*syntax-wait* 0))
             (false (tuatara.syntax:syntax-ready-tree syntax)))
           (let ((tuatara.syntax:*syntax-wait* 60))
             (true (tuatara.syntax:syntax-ready-tree syntax)))
           (true (tuatara.syntax:syntax-current syntax))
           (loop repeat 100 until (plusp woken) do (sleep 0.01))
           (is = 1 woken)
           ;; stopping a parse under way is quick
           (tuatara.buffer:buffer-insert buffer 0 "\"")
           (true (tuatara.syntax:syntax-job syntax))
           (let ((stop (get-internal-real-time)))
             (tuatara.syntax:detach-syntax buffer)
             (true (< (- (get-internal-real-time) stop) (* internal-time-units-per-second (machine-slowdown)))))
           (false (tuatara.syntax:buffer-syntax buffer)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax helpers-end-with-their-buffers)
  ;; the text of each file is parsed by a program of its own, no more than
  ;; *syntax-helpers-most* at once, those used longest ago ended for room;
  ;; killing the buffers ends the programs, none left running
  (let ((started '()))
    (with-test-directory (directory)
      (let ((tuatara.syntax::*syntax-helpers-most* 4))
        (dotimes (i 12)
          (let ((path (merge-pathnames (format nil "f~d.rs" i) directory)))
            (with-open-file (o path :direction :output) (format o "fn f~d() {}~%" i))
            (run-keys "C-g" :file (namestring path))
            (dolist (helper tuatara.syntax::**helpers**)
              (when (eq :own (tuatara.syntax::helper-kind helper)) (pushnew helper started)))
            (true (<= (count :own tuatara.syntax::**helpers** :key #'tuatara.syntax::helper-kind) 4))))))
    (true (>= (length started) 4))
    ;; the buffers are killed with their directory: so are their programs
    (dolist (helper started)
      (false (tuatara.syntax:helper-alive helper))
      (false (loop repeat 200
                   while (sb-ext:process-alive-p (tuatara.syntax::helper-process helper))
                   do (sleep 0.01)
                   finally (return (sb-ext:process-alive-p (tuatara.syntax::helper-process helper))))))
    (false (intersection started tuatara.syntax::**helpers**))))

;;; A helper that is not the program: it answers as each test says

(defun fake-send-octet (fd octet)
  "Write the one OCTET to FD; NIL when it cannot be written."
  (let ((one (make-array 1 :element-type '(unsigned-byte 8) :initial-element octet)))
    (sb-sys:with-pinned-objects (one)
      (eq t (tuatara.syntax::transfer fd (sb-sys:vector-sap one) 1 :write nil)))))

(defun octets-u32 (octets at)
  "The little-endian 32-bit number of OCTETS at AT."
  (logior (aref octets at)
          (ash (aref octets (+ at 1)) 8)
          (ash (aref octets (+ at 2)) 16)
          (ash (aref octets (+ at 3)) 24)))

(defun put-octets-u32 (octets at value)
  "Put VALUE in OCTETS at AT as a little-endian 32-bit number."
  (dotimes (i 4) (setf (aref octets (+ at i)) (ldb (byte 8 (* 8 i)) value))))

(defun fake-send (fd id how octets)
  "Write to FD a message as a helper writes it: ID, HOW and the OCTETS of its body."
  (let ((all (make-array (+ 12 (length octets)) :element-type '(unsigned-byte 8))))
    (put-octets-u32 all 0 (length octets)) (put-octets-u32 all 4 id) (put-octets-u32 all 8 how)
    (replace all octets :start1 12)
    (sb-sys:with-pinned-objects (all)
      (eq t (tuatara.syntax::transfer fd (sb-sys:vector-sap all) (length all) :write nil)))))

(defun fake-receive (fd)
  "The next message on FD, as its header's three numbers and its body's
octets, four values; NIL when FD has ended."
  (let ((header (make-array 12 :element-type '(unsigned-byte 8))))
    (when (sb-sys:with-pinned-objects (header)
            (eq t (tuatara.syntax::transfer fd (sb-sys:vector-sap header) 12 :read nil)))
      (let ((body (make-array (octets-u32 header 0) :element-type '(unsigned-byte 8))))
        (when (or (zerop (length body))
                  (sb-sys:with-pinned-objects (body)
                    (eq t (tuatara.syntax::transfer fd (sb-sys:vector-sap body) (length body) :read nil))))
          (values (octets-u32 header 0) (octets-u32 header 4) (octets-u32 header 8) body))))))

(defun words (&rest numbers)
  "NUMBERS as 32-bit words, the octets of a message's body."
  (let ((octets (make-array (* 4 (length numbers)) :element-type '(unsigned-byte 8))))
    (loop for n in numbers for i from 0 do (put-octets-u32 octets (* 4 i) n))
    octets))

(defun call-with-fake-helper (answer function)
  "Call FUNCTION with a helper whose one connection a thread of the test's
answers: ANSWER is called with the descriptor, the request's how and its body
for each request, and writes what it likes; :NEVER for a helper that reads
nothing. The helper is ended after."
  (multiple-value-bind (first first-theirs) (tuatara.syntax::socket-pair)
    (multiple-value-bind (mine theirs) (tuatara.syntax::socket-pair)
      (let* ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-helper-ended (constantly nil)))
             (helper (tuatara.syntax::%make-helper nil first '() :own))
             (call (tuatara.syntax::make-call helper mine))
             (thread (sb-thread:make-thread
                      (lambda ()
                        (if (eq answer :never)
                            (loop while (tuatara.syntax:helper-alive helper) do (sleep 0.05))
                            (loop (multiple-value-bind (size id how body) (fake-receive theirs)
                                    (declare (ignore id))
                                    (unless size (return))
                                    (funcall answer theirs how body)))))
                      :name "fake syntax helper")))
        (sb-thread:with-mutex ((tuatara.syntax::helper-write-lock helper))
          (push call (tuatara.syntax::helper-idle helper)))
        (unwind-protect (funcall function helper)
          (tuatara.syntax::end-helper helper :stopped)
          (sb-thread:join-thread thread :default nil :timeout 5)
          (sb-posix:close theirs)
          (sb-posix:close first-theirs))))))

(defconstant +top-bit+ #x80000000)

(define-test (syntax helper-reads-only-the-parse-it-runs)
  ;; a helper may read only the text of the parse it was asked for, while it
  ;; runs, on its connection, within the text; asking for more ends it
  (let ((grammar (tuatara.syntax:find-grammar "rust"))
        (rope (tuatara.text:string->rope "abcdef")))
    (flet ((try (answer function)
             (let (ended)
               (call-with-fake-helper answer
                                      (lambda (helper)
                                        (funcall function helper)
                                        (setf ended (tuatara.syntax:helper-ended helper))))
               ended)))
      ;; no parse under way: asking for text ends it
      (is eq :broken (try (lambda (fd how body) (declare (ignore how body))
                            (fake-send fd (logior +top-bit+ 0) 64 (words 0 0)))
                          (lambda (helper)
                            (false (tuatara.syntax:tree-has-error-p (tuatara.syntax::%make-tree helper 1))))))
      (dolist (bad (list (list (logior +top-bit+ 9) (words 9 0))        ; another parser's
                         (list (logior +top-bit+ 5) (words 5 100))      ; past the text
                         (list (logior +top-bit+ 5) (words 5 0 0 0))    ; not a request for text
                         (list (logior +top-bit+ 5) (words 6 0))))      ; asked as one, for another
        (is eq :broken (try (lambda (fd how body) (declare (ignore how body))
                              (fake-send fd (first bad) 64 (second bad)))
                            (lambda (helper)
                              (false (tuatara.syntax:parse-rope (tuatara.syntax::%make-parser helper 5 grammar) rope))))
            "~s" bad))
      ;; the parse asking for its own text gets it, and only from where it asked
      (let ((given nil))
        (false (try (lambda (fd how body) (declare (ignore body))
                      (when (= how 4)
                        (fake-send fd (logior +top-bit+ 5) 64 (words 5 2))
                        (multiple-value-bind (size id status text) (fake-receive fd)
                          (declare (ignore size status))
                          (when (= id (logior +top-bit+ 5)) (setf given (sb-ext:octets-to-string text))))
                        (fake-send fd 1 0 (words 0 2))))
                    (lambda (helper)
                      (multiple-value-bind (tree why)
                          (tuatara.syntax:parse-rope (tuatara.syntax::%make-parser helper 5 grammar) rope)
                        (false tree)
                        (is eq :slow why)))))
        (is equal "cdef" given)))))

(defun padded-string-words (string)
  "STRING as a reply holds it: its length, then its octets made up to four."
  (let* ((octets (sb-ext:string-to-octets string :external-format :utf-8))
         (padded (make-array (* 4 (ceiling (length octets) 4)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace padded octets)
    (concatenate '(vector (unsigned-byte 8)) (words (length octets)) padded)))

(define-test (syntax compiled-rules-held-to-their-text)
  ;; what the helper compiling rules answers cannot add a test or a property
  ;; the rules do not hold: a string not in them is refused, the helper
  ;; ended; a property the editor does not know is left out, never made
  (let ((grammar (tuatara.syntax:find-grammar "rust")))
    (flet ((compile-with (source reply)
             (let (result ended)
               (call-with-fake-helper
                (lambda (fd how body) (declare (ignore how body)) (fake-send fd 1 0 reply))
                (lambda (helper)
                  (let ((tuatara.syntax:*grammar-directories* (list "/nonexistent/fake-rules/")))
                    (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                      (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper)
                            tuatara.syntax::**kept-helpers**))
                    (unwind-protect
                         (setf result (handler-case (tuatara.syntax::compile-query-text grammar source)
                                        (tuatara.syntax:query-error () :refused)))
                      (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                        (setf tuatara.syntax::**kept-helpers**
                              (remove helper tuatara.syntax::**kept-helpers** :key #'cdr)))))
                  (setf ended (tuatara.syntax:helper-ended helper))))
               (values result ended))))
      ;; a #match? the rules do not have
      (multiple-value-bind (result ended)
          (compile-with "(identifier) @name"
                        (concatenate '(vector (unsigned-byte 8))
                                     (words 0 1) (padded-string-words "name")
                                     (words 1 0 0 4)
                                     (words 2) (padded-string-words "match?") (words 1 0)
                                     (words 2) (padded-string-words "^evil$") (words 0 0)))
        (is eq :refused result)
        (is eq :broken ended))
      ;; a property no one knows: not made a keyword
      (multiple-value-bind (result ended)
          (compile-with "((identifier) @name (#set! zzq-unknown-property \"v\"))"
                        (concatenate '(vector (unsigned-byte 8))
                                     (words 0 1) (padded-string-words "name")
                                     (words 1 0 0 4)
                                     (words 2) (padded-string-words "set!")
                                     (words 2) (padded-string-words "zzq-unknown-property")
                                     (words 2) (padded-string-words "v") (words 0 0)))
        (true (tuatara.syntax::query-p result))
        (false ended)
        (when (tuatara.syntax::query-p result)
          (false (aref (tuatara.syntax::query-properties result) 0)))
        (false (find-symbol "ZZQ-UNKNOWN-PROPERTY" :keyword))))))

(define-test (syntax late-helper-ended-at-its-deadline)
  ;; one deadline holds for the whole of a call, both ways: a reply that
  ;; comes an octet at a time, a parse asking for its text without end, a
  ;; helper reading nothing of a long request; each helper is ended in time,
  ;; and the editor carries on
  (let ((tuatara.syntax:*syntax-helper-wait* 1)
        (grammar (tuatara.syntax:find-grammar "rust")))
    (flet ((ends-in-time (answer ask)
             (let ((began (get-internal-real-time)) ended)
               (call-with-fake-helper answer (lambda (helper)
                                               (funcall ask helper)
                                               (setf ended (tuatara.syntax:helper-ended helper))))
               (true (< (tuatara.runtime:seconds-since began) 4))
               ended)))
      ;; slow drip
      (is eq :hung (ends-in-time
                    (lambda (fd how body)
                      (declare (ignore how body))
                      (let ((all (words 4 1 0 1)))
                        (dotimes (i (length all))
                          (unless (fake-send-octet fd (aref all i)) (return))
                          (sleep 0.3))))
                    (lambda (helper) (false (tuatara.syntax:tree-has-error-p (tuatara.syntax::%make-tree helper 1))))))
      ;; asking for text without end
      (true (member (ends-in-time
                     (lambda (fd how body)
                       (declare (ignore body))
                       (when (= how 4)
                         (loop (unless (fake-send fd (logior +top-bit+ 5) 64 (words 5 0)) (return))
                               (unless (fake-receive fd) (return)))))
                     (lambda (helper)
                       (false (tuatara.syntax:parse-rope (tuatara.syntax::%make-parser helper 5 grammar)
                                                         (tuatara.text:string->rope "abcdef") :limit 0.5))))
                    '(:hung :broken)))
      ;; reading nothing
      (is eq :hung (ends-in-time
                    :never
                    (lambda (helper)
                      (tuatara.syntax::with-call (call helper 16)
                        (tuatara.syntax::call-octets call (make-array (* 4 1024 1024) :element-type '(unsigned-byte 8)))
                        (is eq :dead (tuatara.syntax::call-send call))))))
      (true (parses-elsewhere)))))

(defun garbled-words (random)
  "Octets of a reply made up of numbers a reply might hold and numbers it
should not, drawn with the random state RANDOM."
  (apply #'words (loop repeat (random 40 random)
                       collect (case (random 6 random)
                                 (0 0) (1 1) (2 (random 8 random)) (3 (random 64 random))
                                 (4 #xffffffff) (t (random (expt 2 32) random))))))

(define-test (syntax garbled-replies-end-the-helper)
  ;; a reply that cannot be right ends its helper, and what was asked gives
  ;; nothing: never an error, never something outside the text
  (let* ((random (sb-ext:seed-random-state 20261030))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar "(identifier) @name"))
         (rope (tuatara.text:string->rope "fn main() { let x = y; }"))
         (bytes (tuatara.text:rope-octet-length rope))
         (buffer (tuatara.buffer:make-buffer "*garbled*" :text "fn main() { let x = y; }"))
         (syntax (let ((syntax (tuatara.syntax:attach-syntax buffer grammar))) (parse-done syntax) syntax))
         (errors 0)
         (ended 0))
    (flet ((within (ranges) (every (lambda (r) (<= 0 (car r) (cdr r) bytes)) ranges)))
      (let ((asks
              (list (lambda (helper tree) (declare (ignore helper)) (tuatara.syntax:tree-has-error-p tree))
                    (lambda (helper tree) (declare (ignore helper)) (tuatara.syntax::copy-tree-pointer tree))
                    (lambda (helper tree) (declare (ignore helper))
                      (true (within (tuatara.syntax:node-child-ranges tree 0 bytes 1))))
                    (lambda (helper tree)
                      (declare (ignore tree))
                      (let ((nodes (make-array 7 :element-type 'fixnum :initial-contents (list 1 0 bytes 1 0 0 bytes))))
                        (multiple-value-bind (pairs starts) (tuatara.syntax::node-children-many helper nodes 1)
                          (when pairs
                            (is = (length pairs) (aref starts 1))
                            (true (every (lambda (n) (<= 0 n bytes)) pairs))))))
                    (lambda (helper tree)
                      (setf (gethash (tuatara.syntax::query-number query) (tuatara.syntax::helper-queries helper)) t)
                      (tuatara.syntax:map-query-matches
                       (lambda (pattern captures count)
                         (declare (ignore pattern))
                         (dotimes (i count)
                           (true (<= 0 (aref captures (+ 1 (* 4 i))) (aref captures (+ 2 (* 4 i))) bytes))))
                       query tree rope 0 bytes))
                    (lambda (helper tree)
                      (setf (gethash (tuatara.syntax::query-number query) (tuatara.syntax::helper-queries helper)) t)
                      (tuatara.syntax::parse-texts
                       rope (list (tuatara.syntax::make-text-parse nil nil tree (list (list query 0 bytes)) nil nil)
                                  (tuatara.syntax::make-text-parse grammar (list (cons 0 bytes)) nil
                                                                   (list (list query 0 bytes)) 1 nil))
                       (lambda (trees)
                         (is = 2 (length trees))
                         (tuatara.syntax:map-query-matches
                          (lambda (pattern captures count)
                            (declare (ignore pattern))
                            (dotimes (i count)
                              (true (<= 0 (aref captures (+ 1 (* 4 i))) (aref captures (+ 2 (* 4 i))) bytes))))
                          query tree rope 0 bytes))
                       :helper helper))
                    (lambda (helper tree) (declare (ignore tree)) (tuatara.syntax:helper-memory helper))
                    ;; with a buffer's syntax whose tree the helper holds
                    (lambda (helper tree)
                      (declare (ignore helper))
                      (let ((old (tuatara.syntax:syntax-tree syntax)))
                        (setf (tuatara.syntax:syntax-tree syntax) tree)
                        (unwind-protect
                             (progn
                               (dolist (node (tuatara.syntax:node-ancestry syntax 3 5))
                                 (true (<= 0
                                           (tuatara.syntax:tree-node-start node)
                                           (tuatara.syntax:tree-node-end node) bytes)))
                               (multiple-value-bind (from to) (tuatara.syntax:select-node syntax 3 5 :larger)
                                 (when from (true (<= 0 from to bytes)))))
                          (setf (tuatara.syntax:syntax-tree syntax) old))))
                    ;; as the helper compiling rules, which grammars are loaded in
                    (lambda (helper tree)
                      (declare (ignore tree))
                      (let ((tuatara.syntax:*grammar-directories* (list "/nonexistent/fake-helper/")))
                        (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                          (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper)
                                tuatara.syntax::**kept-helpers**))
                        (unwind-protect
                             (progn
                               (let ((loaded (tuatara.syntax::load-grammar "fake")))
                                 (true (or (member loaded '(nil :unknown)) (tuatara.syntax:grammar-p loaded))))
                               (handler-case (tuatara.syntax::compile-query-text grammar "(identifier) @name")
                                 (tuatara.syntax:query-error () nil)))
                          (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                            (setf tuatara.syntax::**kept-helpers**
                                  (remove helper tuatara.syntax::**kept-helpers** :key #'cdr)))))))))
        (dolist (ask asks)
          (dotimes (i 60)
            (let ((reply (garbled-words random))
                  (header (case (random 10 random) (0 :huge) (1 :id) (t :good))))
              (call-with-fake-helper
               (lambda (fd how body)
                 (declare (ignore how body))
                 (ecase header
                   (:good (fake-send fd 1 0 reply))
                   (:id (fake-send fd 7 0 reply))
                   (:huge (let ((all (words #x7fffffff 1 0)))
                            (sb-sys:with-pinned-objects (all)
                              (tuatara.syntax::transfer fd (sb-sys:vector-sap all) 12 :write nil))))))
               (lambda (helper)
                 (handler-case (funcall ask helper (tuatara.syntax::%make-tree helper 1))
                   (error (e) (incf errors) (format t "~&~a~%" e)))
                 (when (eq :broken (tuatara.syntax:helper-ended helper)) (incf ended))
                 (when (member header '(:huge :id))
                   (is eq :broken (tuatara.syntax:helper-ended helper) "~s" header)))))))
        (is = 0 errors)
        (true (plusp ended))))
    (tuatara.syntax:detach-syntax buffer)
    (tuatara.buffer:kill-buffer buffer)))

(define-test (syntax parses-share-one-thread)
  ;; parses after one edit after another run in one thread of the buffer's,
  ;; not a thread each, and closing the buffer ends that thread
  (let ((buffer (tuatara.buffer:make-buffer "*syntax-edits*" :text "fn main() {}"))
        (made 0))
    (sb-int:encapsulate 'tuatara.runtime:make-guarded-thread 'counted
                        (lambda (original &rest arguments) (incf made) (apply original arguments)))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust")))
               (tuatara.syntax:*syntax-wait* 10))
           (dotimes (i 50)
             (tuatara.buffer:buffer-insert buffer 0 " ")
             (true (tuatara.syntax:syntax-ready-tree syntax)))
           (true (tuatara.syntax:syntax-current syntax))
           (true (<= made 2) "~d threads for 50 parses" made)
           (let ((worker (tuatara.syntax::syntax-worker syntax)))
             (true worker)
             (tuatara.syntax:detach-syntax buffer)
             (false (sb-thread:thread-alive-p worker))))
      (sb-int:unencapsulate 'tuatara.runtime:make-guarded-thread 'counted)
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(defun fault-buffer (name)
  "A Rust buffer NAME whose parse failed in the grammar's own code, as the
parsing thread records it, and its syntax, as two values."
  (let ((buffer (tuatara.buffer:make-buffer name :text (format nil "fn main() {} // ~a" name))))
    (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
    (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
      (let ((tuatara.syntax:*syntax-wait* 10)) (tuatara.syntax:syntax-ready-tree syntax))
      (let* ((cancel (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:signed 32))))
             (job (tuatara.syntax::make-parse-job (tuatara.buffer:buffer-text buffer) nil cancel nil)))
        (setf (tuatara.syntax::parse-job-fault job) :crashed
              (tuatara.syntax::parse-job-finished job) t
              (tuatara.syntax:syntax-job syntax) job)
        (tuatara.syntax::catch-up syntax))
      (values buffer syntax))))

(define-test (syntax grammar-faults-per-text)
  ;; a grammar going wrong on the text of one buffer is turned off for that
  ;; text alone, which a hostile file cannot change; once it has gone wrong
  ;; on *grammar-faults-before-off* texts, it is off for every text
  (let ((grammar (tuatara.syntax:find-grammar "rust"))
        (buffers '())
        (said '()))
    (clrhash tuatara.syntax::**grammar-faults**)
    (unwind-protect
         (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-fault
                                                            (lambda (syntax) (push syntax said)))))
           (multiple-value-bind (buffer syntax) (fault-buffer "*fault-1*")
             (push buffer buffers)
             (is eq syntax (first said))
             (false (tuatara.syntax:grammar-faulted grammar))
             (true (tuatara.syntax:syntax-broken syntax))
             (true (tuatara.syntax:syntax-tree syntax))
             ;; off for that buffer, set up again: another helper, the grammar still off there
             (true (tuatara.syntax::grammar-off-p grammar (tuatara.syntax:buffer-helper buffer)))
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
             (false (tuatara.syntax:buffer-syntax buffer)))
           ;; another Rust buffer is parsed all the same
           (let ((other (tuatara.buffer:make-buffer "*fault-other*" :text "fn f() {}")))
             (push other buffers)
             (tuatara.modes:set-major-mode other 'tuatara.defaults::rust-mode)
             (true (tuatara.syntax:buffer-syntax other)))
           ;; gone wrong on three texts: off for all
           (dotimes (i (1- tuatara.syntax::*grammar-faults-before-off*))
             (push (fault-buffer (format nil "*fault-~d*" (+ i 2))) buffers))
           (is eq :crashed (tuatara.syntax:grammar-faulted grammar))
           (let ((later (tuatara.buffer:make-buffer "*fault-later*" :text "fn h() {}")))
             (push later buffers)
             (tuatara.modes:set-major-mode later 'tuatara.defaults::rust-mode)
             (false (tuatara.syntax:buffer-syntax later))))
      (mapc #'tuatara.buffer:kill-buffer buffers)
      (clrhash tuatara.syntax::**grammar-faults**)
      (setf (tuatara.syntax:grammar-faulted grammar) nil))))

(define-test (syntax helper-ended-by-the-editor-parses-again)
  ;; a helper ended for room as it parsed is no fault of the grammar: the
  ;; text is parsed again in a new one
  (let ((buffer (tuatara.buffer:make-buffer "*stopped-helper*" :text "fn main() {}"))
        (grammar (tuatara.syntax:find-grammar "rust")))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer grammar)))
           (parse-done syntax)
           (let ((first (tuatara.syntax::syntax-helper syntax)))
             (multiple-value-bind (tree why)
                 (progn (tuatara.syntax::stop-helper first)
                        (tuatara.syntax:parse-rope (tuatara.syntax::%make-parser first 1 grammar)
                                                   (tuatara.text:string->rope "fn f() {}")))
               (false tree)
               (is eq :stopped why))
             (tuatara.buffer:buffer-insert buffer 0 " ")
             (parse-done syntax)
             (true (tuatara.syntax:syntax-tree syntax))
             (false (tuatara.syntax:syntax-broken syntax))
             (false (tuatara.syntax::grammar-off-p grammar (tuatara.syntax::syntax-helper syntax)))
             (true (tuatara.syntax:helper-alive (tuatara.syntax::syntax-helper syntax)))
             (false (eq first (tuatara.syntax::syntax-helper syntax)))))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax untrusted-text-parsed-in-its-own-helper)
  ;; a buffer with no file is parsed by a helper of its own, as a file
  ;; outside the projects you trust is, its lines alone (a text too large to
  ;; parse whole) too; an export's source blocks are parsed in one of its own
  (let ((buffer (tuatara.buffer:make-buffer "*pasted*"
                                            :text (format nil "~{fn f~d() {}~%~}" (loop for i below 200 collect i))))
        (helpers '()))
    (sb-int:encapsulate 'tuatara.syntax::parse-rope-ranges 'which
                        (lambda (function &rest arguments)
                          (push (tuatara.syntax::current-helper) helpers)
                          (apply function arguments)))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 100)
                (syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust")))
                (own (tuatara.syntax:buffer-helper buffer))
                (query (tuatara.syntax:compile-query (tuatara.syntax:find-grammar "rust") "(identifier) @name")))
           (is eq :own (tuatara.syntax:helper-kind own))
           (true (tuatara.syntax::slice-highlights syntax query (vector :variable) 0 60 nil))
           (true helpers)
           (true (every (lambda (helper) (eq helper own)) helpers)))
      (sb-int:unencapsulate 'tuatara.syntax::parse-rope-ranges 'which)
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer)))
  (let ((used nil))
    (tuatara.syntax:with-own-helper ()
      (true (tuatara.defaults::org-latex-code-faces "fn main() { let x = 1; }" "rust"))
      (setf used tuatara.syntax:*helper*)
      (true (tuatara.syntax:helper-p used))
      (is eq :own (tuatara.syntax:helper-kind used)))
    (false (tuatara.syntax:helper-alive used))))

(defun call-without-sandbox (function)
  "Call FUNCTION with no sandbox to be had (bwrap not found), as on a system
without bubblewrap, and the sandbox as it was after."
  (let ((before tuatara.io::*bwrap-name*))
    (setf tuatara.io::*bwrap-name* "/nonexistent/bwrap"
          tuatara.syntax::**refused-said** nil)
    (unwind-protect (funcall function)
      (setf tuatara.io::*bwrap-name* before
            tuatara.syntax::**refused-said** nil)
      (tuatara.io:sandbox-status :again t))))

(define-test (syntax no-sandbox-no-parsing)
  ;; without the sandbox, text outside the projects you trust is never parsed,
  ;; said once; the projects you trust only when *syntax-helper-unconfined* allows;
  ;; a sandbox that fails to start starts nothing in its place
  (let ((said '()))
    (call-without-sandbox
     (lambda ()
       (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-helper-refused
                                                        (lambda (trusted) (push trusted said))))
             (tuatara.syntax::*syntax-helper-unconfined* nil))
         (false (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own))
         (false (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :shared))
         (is equal '(nil) said)
         (let ((buffer (tuatara.buffer:make-buffer "*no-sandbox*" :text "fn main() {}")))
           (unwind-protect
                (progn (false (tuatara.syntax:buffer-helper buffer))
                       (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
                         (when syntax
                           (parse-done syntax)
                           (true (tuatara.syntax:syntax-broken syntax))
                           (false (tuatara.syntax:syntax-tree syntax)))))
             (tuatara.syntax:detach-syntax buffer)
             (tuatara.buffer:kill-buffer buffer)))
         (let ((tuatara.syntax::*syntax-helper-unconfined* t))
           (false (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own))
           (let ((shared (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :shared)))
             (true shared)
             (when shared (tuatara.syntax::stop-helper shared)))))))
    ;; the sandbox there, but failing to start: no helper, no other process
    (sb-int:encapsulate 'tuatara.io:sandbox-start 'failing (lambda (function &rest arguments)
                                                            (declare (ignore function arguments))
                                                            (error "bwrap would not start")))
    (unwind-protect
         (let ((before (length tuatara.syntax::**helpers**)))
           (false (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own))
           (is = before (length tuatara.syntax::**helpers**)))
      (sb-int:unencapsulate 'tuatara.io:sandbox-start 'failing))))

(define-test (syntax trusted-or-not-follows-the-buffer)
  ;; a buffer whose file comes to be in a project you trust is parsed by the
  ;; shared helper from its next parse, and by one of its own again after
  (let* ((trusted nil)
         (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-trusted-text-p
                                                      (lambda (buffer) (declare (ignore buffer)) trusted)))
         (buffer (tuatara.buffer:make-buffer "*trust-follows*" :text "fn main() {}")))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (parse-done syntax)
           (is eq :own (tuatara.syntax:helper-kind (tuatara.syntax::syntax-helper syntax)))
           (setf trusted t)
           (tuatara.buffer:buffer-insert buffer 0 " ")
           (parse-done syntax)
           (is eq :shared (tuatara.syntax:helper-kind (tuatara.syntax::syntax-helper syntax)))
           (true (tuatara.syntax:syntax-tree syntax))
           (setf trusted nil)
           (tuatara.buffer:buffer-insert buffer 0 " ")
           (parse-done syntax)
           (is eq :own (tuatara.syntax:helper-kind (tuatara.syntax::syntax-helper syntax)))
           (true (tuatara.syntax:syntax-tree syntax)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax grammar-faults)
  ;; a grammar whose own code fails while reading is turned off for the session:
  ;; the buffer keeps the colours it had, no text is parsed with the grammar
  ;; again, the editor says so, and closing the buffer leaves the parser alone
  (let* ((buffer (tuatara.buffer:make-buffer "*fault*" :text "fn main() {}"))
         (grammar (tuatara.syntax:find-grammar "rust"))
         ;; off for all after one text
         (tuatara.syntax::*grammar-faults-before-off* 1)
         (said nil))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (let ((syntax (tuatara.syntax:buffer-syntax buffer))
                 (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-fault
                                                              (lambda (syntax) (setf said syntax)))))
             (let ((tuatara.syntax:*syntax-wait* 10)) (tuatara.syntax:syntax-ready-tree syntax))
             (true (tuatara.syntax:syntax-tree syntax))
             ;; a parse whose grammar failed, as the parsing thread records it
             (let* ((cancel (sb-alien:alien-sap (sb-alien:make-alien (sb-alien:signed 32))))
                    (job (tuatara.syntax::make-parse-job (tuatara.buffer:buffer-text buffer) nil cancel nil)))
               (setf (tuatara.syntax::parse-job-fault job) (make-condition 'sb-sys:memory-fault-error)
                     (tuatara.syntax::parse-job-finished job) t
                     (tuatara.syntax:syntax-job syntax) job)
               (tuatara.syntax::catch-up syntax))
             (is eq syntax said)
             (true (tuatara.syntax:grammar-faulted grammar))
             (true (tuatara.syntax:syntax-broken syntax))
             (true (tuatara.syntax:syntax-tree syntax))
             (false (tuatara.syntax:syntax-job syntax))
             ;; another Rust buffer gets no syntax tree
             (let ((other (tuatara.buffer:make-buffer "*fault-other*" :text "fn f() {}")))
               (unwind-protect
                    (progn (tuatara.modes:set-major-mode other 'tuatara.defaults::rust-mode)
                           (false (tuatara.syntax:buffer-syntax other)))
                 (tuatara.buffer:kill-buffer other)))))
      (tuatara.buffer:kill-buffer buffer)
      (clrhash tuatara.syntax::**grammar-faults**)
      (setf (tuatara.syntax:grammar-faulted grammar) nil)))
  ;; the flag is reset: Rust parses again
  (let ((buffer (tuatara.buffer:make-buffer "*fault-after*" :text "fn g() {}")))
    (unwind-protect
         (progn (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
                (true (tuatara.syntax:buffer-syntax buffer)))
      (tuatara.buffer:kill-buffer buffer))))

;; a grammar that goes wrong on purpose, test/grammars/misbehaving/

(defun call-with-misbehaving-grammar (function)
  "Call FUNCTION with the grammar of test/grammars/misbehaving/, compiled as the
build compiles every grammar into a directory of the test's own, and that
directory, which the programs parsing meanwhile load grammars from; the
grammar is forgotten afterwards, and those programs ended."
  (with-test-directory (directory)
    (let ((source (uiop:native-namestring (asdf:system-relative-pathname :tuatara "test/grammars/misbehaving/src/")))
          (library (uiop:native-namestring (merge-pathnames "misbehaving.so" directory))))
      (multiple-value-bind (output error code)
          (uiop:run-program (list "bash" "-c" ". \"$1\" && compile_grammar \"$2\" \"$3\"" "_"
                                  (tools-script "grammar-build.sh") source library)
                            :output :string :error-output :output :ignore-error-status t)
        (declare (ignore error))
        (is = 0 code "the test grammar did not compile:~%~a" output))
      (remhash "misbehaving" tuatara.syntax::**grammars**)
      (remhash "misbehaving" tuatara.syntax::**grammar-faults**)
      (unwind-protect
           (let ((tuatara.syntax:*grammar-directories* (cons directory (tuatara.syntax:grammar-directories))))
             (funcall function (tuatara.syntax:find-grammar "misbehaving") directory))
        (remhash "misbehaving" tuatara.syntax::**grammars**)
        (remhash "misbehaving" tuatara.syntax::**grammar-faults**)
        (dolist (helper (copy-list tuatara.syntax::**helpers**))
          (when (member directory (tuatara.syntax::helper-directories helper) :test #'equal)
            (tuatara.syntax::stop-helper helper)))))))

(defun held-in-helper (helper)
  "The octets HELPER says its trees and grammars hold."
  (or (tuatara.syntax:helper-memory helper) 0))

(defun held-falls-to (helper octets)
  "True once HELPER holds less than OCTETS, waiting two seconds at most: it
lets go of a parser in a thread of the parser's own."
  (loop repeat 200
        thereis (< (held-in-helper helper) octets)
        do (sleep 0.01)))

(defun parse-in-buffer (grammar text)
  "A buffer holding TEXT whose syntax in GRAMMAR has been parsed, or given up,
and its SYNTAX; the buffer is to be killed by the caller."
  (let* ((buffer (tuatara.buffer:make-buffer "*misbehaving*" :text text))
         (syntax (tuatara.syntax:attach-syntax buffer grammar)))
    (parse-done syntax)
    (values buffer syntax)))

(defun parse-done (syntax)
  "Wait for SYNTAX's parses to finish, ten seconds at most, and take their result."
  (loop repeat 1000
        do (tuatara.syntax:catch-up syntax)
        while (tuatara.syntax:syntax-job syntax)
        do (sleep 0.01)))

(define-test (syntax wrong-values-fail-the-parse)
  ;; each value a grammar's own code gives the parser that it cannot have
  ;; fails the parse: a token number from the scanner ("!s"), from the
  ;; lexing function ("#") and from the keyword lexing function ("bad"), and
  ;; a length of the scanner's state longer than its buffer ("!z"); the
  ;; parser reads a right text afterwards
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (true grammar)
     (when grammar
       (dolist (text '("abc !s def" "abc # def" "abc bad def" "abc !z def"))
         (let ((parser (tuatara.syntax:make-parser grammar (tuatara.syntax:kept-helper :shared))))
           (unwind-protect
                (multiple-value-bind (tree why)
                    (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text))
                  (false tree "~s parsed" text)
                  (is eq :failed why "~s" text)
                  (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc begin !x def"))))
                    (true tree "after ~s" text)
                    (when tree (tuatara.syntax:delete-tree tree))))
             (tuatara.syntax:delete-parser parser))))))))

(define-test (syntax failing-grammar-turned-off)
  ;; in a buffer, a grammar that gave the parser a wrong value is turned off
  ;; for that text and said to have failed, its parser deleted
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((said nil))
       (unwind-protect
            (let ((tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-fault
                                                               (lambda (syntax) (setf said syntax)))))
              (multiple-value-bind (buffer syntax) (parse-in-buffer grammar "abc !s def")
                (unwind-protect
                     (progn
                       (is eq syntax said)
                       (false (tuatara.syntax:grammar-faulted grammar))
                       (true (tuatara.syntax::grammar-off-p grammar (tuatara.syntax::syntax-helper syntax)))
                       (true (tuatara.syntax:syntax-broken syntax))
                       (false (tuatara.syntax::syntax-parser syntax))
                       (false (tuatara.syntax:syntax-tree syntax)))
                  (tuatara.syntax:detach-syntax buffer)
                  (tuatara.buffer:kill-buffer buffer))))
         (setf (tuatara.syntax:grammar-faulted grammar) nil))))))

(define-test (syntax scanner-memory-counted)
  ;; the memory a grammar's scanner takes is counted with tree-sitter's: a
  ;; scanner taking a megabyte for each mark is stopped by the parse's
  ;; budget; in a buffer the parse is given up, the editor says so and goes
  ;; on, and the scanner's memory is let go
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let* ((text (with-output-to-string (o) (dotimes (i 64) (write-string "word !m " o))))
            (parser (tuatara.syntax:make-parser grammar (tuatara.syntax:kept-helper :shared)))
            (helper (tuatara.syntax::parser-helper parser))
            (held (held-in-helper helper)))
       (unwind-protect
            (multiple-value-bind (tree why)
                (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text) :budget (* 8 1024 1024))
              (false tree)
              (is eq :memory why))
         (tuatara.syntax:delete-parser parser))
       (true (held-falls-to helper (+ held (* 1024 1024))))
       (let ((said nil))
         (let ((tuatara.syntax:*syntax-memory-limit* (* 8 1024 1024))
               (tuatara.syntax:*syntax-memory-per-octet* 0)
               (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-slow
                                                            (lambda (syntax) (setf said syntax)))))
           (multiple-value-bind (buffer syntax) (parse-in-buffer grammar text)
             (unwind-protect
                  (progn
                    (is eq syntax said)
                    (is eq :memory (tuatara.syntax:syntax-broken syntax))
                    (false (tuatara.syntax:grammar-faulted grammar))
                    ;; editing goes on, the text no longer parsed
                    (tuatara.buffer:buffer-insert buffer 0 "more ")
                    (false (tuatara.syntax:syntax-job syntax)))
               (tuatara.syntax:detach-syntax buffer)
               (tuatara.buffer:kill-buffer buffer)))))))))

(define-test (syntax tight-memory-budget-tried-again)
  ;; the program parsing a buffer holds a parse to half of what it may still
  ;; take: a parse spending that is given up as memory running short, and
  ;; the text is parsed again at its next edit
  (let* ((numbers (format nil "[~{~d~^,~}]" (loop repeat 2000000 collect (random 10))))
         (said nil)
         ;; found before the buffer's own helper, held to little memory, is started
         (grammar (tuatara.syntax:find-grammar "json"))
         (tuatara.syntax:*syntax-helper-memory* (* 256 1024 1024))
         (tuatara.runtime:*host* (make-answering-host 'tuatara.syntax:host-say-syntax-slow
                                                      (lambda (syntax) (setf said syntax)))))
    (let* ((buffer (tuatara.buffer:make-buffer "*tight*" :text numbers))
           (syntax (progn
                     ;; a program of its own, as a file's text has, held to that little memory
                     (setf (tuatara.buffer:buffer-local buffer :syntax-helper)
                           (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own))
                     (tuatara.syntax:attach-syntax buffer grammar))))
      (parse-done syntax)
      (unwind-protect
           (progn
             (is eq :short (tuatara.syntax:syntax-broken syntax))
             (is eq syntax said)
             (false (tuatara.syntax:grammar-faulted (tuatara.syntax:syntax-grammar syntax)))
             ;; the text made short: memory is back, and it is parsed again
             (tuatara.buffer:buffer-delete buffer 4 (- (length numbers) 1))
             (parse-done syntax)
             (is eq nil (tuatara.syntax:syntax-broken syntax))
             (true (tuatara.syntax:syntax-tree syntax)))
        (tuatara.syntax:detach-syntax buffer)
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (syntax no-memory-for-one-request)
  ;; tree-sitter asked, in a parse, for more memory than can be had at once
  ;; ("!h", a terabyte): the parse is given up and its parser let go,
  ;; counted; the program parsing goes on, and so does the editor
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let* ((parser (tuatara.syntax:make-parser grammar (tuatara.syntax:kept-helper :shared)))
            (helper (tuatara.syntax::parser-helper parser)))
       (multiple-value-bind (tree why unusable)
           (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !h def"))
         (false tree)
         (is eq :short why)
         (true unusable))
       (true (tuatara.syntax:helper-alive helper))
       (true (nth-value 1 (tuatara.syntax:helper-memory helper))))
     (let ((before (tuatara.syntax:parsers-let-go)))
       (multiple-value-bind (buffer syntax) (parse-in-buffer grammar "abc !h def")
         (unwind-protect
              (progn
                (is eq :short (tuatara.syntax:syntax-broken syntax))
                (is = (1+ before) (tuatara.syntax:parsers-let-go))
                ;; a new parser, at the next edit
                (tuatara.buffer:buffer-delete buffer 4 7)
                (parse-done syntax)
                (is eq nil (tuatara.syntax:syntax-broken syntax))
                (true (tuatara.syntax:syntax-tree syntax)))
           (tuatara.syntax:detach-syntax buffer)
           (tuatara.buffer:kill-buffer buffer)))))))

(defun parses-elsewhere ()
  "True when text is still parsed after a program parsing went wrong."
  (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json") (tuatara.syntax:kept-helper :shared))))
    (unwind-protect
         (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "[1, 2]"))))
           (prog1 (and tree t)
             (when tree (tuatara.syntax:delete-tree tree))))
      (tuatara.syntax:delete-parser parser))))

(defun call-with-own-helper (function)
  "Call FUNCTION with a program parsing of its own, as a file you do not trust
has, ended after."
  (let ((helper (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own)))
    (unwind-protect (funcall function helper)
      (when helper (tuatara.syntax::stop-helper helper)))))

(define-test (syntax looping-text-in-another-language-hangs-once)
  ;; text in another language whose scanner never returns hangs colouring
  ;; once: the grammar is then off for that buffer's text, not tried again
  ;; at each redraw
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let* ((tuatara.syntax:*syntax-helper-wait* 1)
            (tuatara.syntax::*injection-time-limit* 0.3)
            (colours (tuatara.syntax:compile-query grammar "(_) @x"))
            (buffer (tuatara.buffer:make-buffer "*looping-block*" :text "abc !l def"))
            (rope (tuatara.buffer:buffer-text buffer))
            (resolve (lambda (name) (declare (ignore name)) (values grammar colours (vector :keyword)))))
       (unwind-protect
            (let ((tuatara.syntax:*parsing-buffer* buffer)
                  (tuatara.syntax:*helper* (tuatara.syntax:buffer-helper buffer)))
              (let ((began (get-internal-real-time)))
                (false (tuatara.syntax:language-highlights resolve "misbehaving" (list (cons 0 10)) rope 0 10))
                (true (< (tuatara.runtime:seconds-since began) 5)))
              (true (tuatara.syntax::grammar-off-p grammar buffer))
              (false (tuatara.syntax:grammar-faulted grammar))
              ;; the next redraw: no wait at all
              (let* ((tuatara.syntax:*helper* (tuatara.syntax:buffer-helper buffer))
                     (began (get-internal-real-time)))
                (false (tuatara.syntax:language-highlights resolve "misbehaving" (list (cons 0 10)) rope 0 10))
                (true (< (tuatara.runtime:seconds-since began) 0.2))))
         (tuatara.buffer:kill-buffer buffer))))))

(define-test (syntax helper-stalled-outside-a-parse-stops-its-buffer)
  ;; a buffer's helper that stops answering, or answers wrongly, while not
  ;; parsing: nothing more of that buffer is parsed, so that it hangs once
  (let ((buffer (tuatara.buffer:make-buffer "*stalled*" :text "fn main() {}")))
    (unwind-protect
         (let ((helper (tuatara.syntax:buffer-helper buffer)))
           (true helper)
           (tuatara.syntax::end-helper helper :hung)
           (true (tuatara.syntax::grammar-off-p (tuatara.syntax:find-grammar "rust") buffer))
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (false (tuatara.syntax:buffer-syntax buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax lua-patterns-read-as-lua)
  ;; #lua-match? reads its pattern as Lua does: classes, %-quoting and - for a short repeat
  (flet ((regex (pattern) (tuatara.syntax::lua-pattern-regex pattern)))
    (is string= "^[A-Z][A-Z_0-9]+$" (regex "^[A-Z][A-Z_0-9]+$"))
    (is string= "^[-][-][-]" (regex "^[-][-][-]"))
    (is string= "[0-9]+\\.[A-Za-z]*?x" (regex "%d+%.%a-x"))
    (is string= "[^0-9]a\\^b\\$c" (regex "%Da^b$c"))
    (false (regex "%bxy"))
    (false (regex "%f[%w]"))
    (false (regex "[%S]"))
    (true (cl-ppcre:scan (regex "^%u%l+$") "Kea"))
    (false (cl-ppcre:scan (regex "^%u%l+$") "kea"))))

(define-test (syntax unreadable-rules-leave-helper-alone)
  ;; rules the helper compiles but the editor's own reader cannot read are refused
  ;; as rules, the helper that compiled them left running
  (let ((grammar (tuatara.syntax:find-grammar "rust"))
        (helper (tuatara.syntax::kept-helper :rules)))
    (sb-int:encapsulate 'tuatara.syntax::read-query-source 'unreadable
                        (lambda (function source)
                          (declare (ignore function source))
                          (error 'tuatara.syntax::query-source-error)))
    (unwind-protect
         (fail (tuatara.syntax:compile-query grammar "(identifier) @not.read") 'tuatara.syntax:query-error)
      (sb-int:unencapsulate 'tuatara.syntax::read-query-source 'unreadable))
    (true (tuatara.syntax::helper-alive helper))))

(define-test (syntax queries-let-go-by-the-next-query)
  ;; a query no longer used is let go of by the helpers when the next query runs,
  ;; not by the collector's own thread
  (let* ((grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar "(identifier) @let.go"))
         (number (tuatara.syntax::query-number query))
         (helper (tuatara.syntax::kept-helper :rules)))
    (true (gethash number (tuatara.syntax::helper-queries helper)))
    (sb-ext:atomic-push number (car tuatara.syntax::**queries-let-go**))
    (tuatara.syntax::ensure-query helper (tuatara.syntax:compile-query grammar "(identifier) @kept.on"))
    (false (gethash number (tuatara.syntax::helper-queries helper)))
    (true (tuatara.syntax::helper-alive helper))))

(define-test (syntax string-kinds-told-without-garbage)
  ;; telling a string's node kind, asked for each node around a place on every
  ;; expression motion, makes nothing
  (true (tuatara.syntax::string-type-p "string_literal"))
  (true (tuatara.syntax::string-type-p "char_literal"))
  (false (tuatara.syntax::string-type-p "identifier"))
  (is = 0 (least-consed (lambda () (tuatara.syntax::string-type-p "call_expression")))))


(define-test (syntax moving-by-the-tree-waits-briefly)
  ;; moving by the tree asks the program parsing with a short wait, so one that
  ;; hangs holds the editor a quarter of a second, not the usual wait
  (let* ((buffer (tuatara.buffer:make-buffer "*syntax-moving*" :text "fn main() { let x = (1 + 2); }"))
         (waits '()))
    (unwind-protect
         (let ((syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (let ((tuatara.syntax:*syntax-wait* 30)) (true (tuatara.syntax:syntax-ready-tree syntax)))
           (sb-int:encapsulate 'tuatara.syntax::call-send 'seconds
                               (lambda (function call &rest keys &key seconds &allow-other-keys)
                                 (push seconds waits)
                                 (apply function call keys)))
           (unwind-protect
                (progn (tuatara.syntax:node-ancestry syntax 12 13)
                       (tuatara.syntax:select-node syntax 12 13 :larger))
             (sb-int:unencapsulate 'tuatara.syntax::call-send 'seconds))
           (is equal '(1/4 1/4) waits))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax machine-memory-asked-once)
  ;; the machine's memory, asked for each parse's limit, is asked of the system once
  (let ((memory (tuatara.syntax::physical-memory)))
    (true (plusp memory))
    (is eql memory tuatara.syntax::**physical-memory**)
    (is eql memory (tuatara.syntax::physical-memory))))
