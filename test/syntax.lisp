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
         (parser (tuatara.syntax:make-parser g))
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
           (tuatara.syntax:rich-highlights query tree rope 0 (tuatara.text:rope-bytes rope)
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
                (mapcar (lambda (pattern) (1+ (count 10 octets :end (aref (tuatara.syntax:query-starts query) pattern))))
                        (tuatara.syntax:query-non-local-patterns query)))
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
  (multiple-value-bind (tree rope parser) (parse-string "html" (format nil "<p>a <b>b</b></p>~%<script>let x = 1;</script>~%"))
    (declare (ignore rope))
    (false (tuatara.syntax:tree-has-error-p tree))
    (release-parse tree parser))
  ;; every language's rules compile, and its sample parses without errors
  (loop for (name grammar sample) in '((rust "rust" "sample.rs") (bash "bash" "sample.sh") (lisp "commonlisp" "sample.lisp")
                                       (containerfile "dockerfile" "Containerfile") (c "c" "sample.c"))
        do (let ((language (language-named name)))
             (true language)
             (true (tuatara.defaults::language-compiled-query language :colouring))
             (multiple-value-bind (tree rope parser) (parse-string grammar (uiop:read-file-string (syntax-sample sample)))
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
      (release-parse tree parser)))
  ;; so does all of this editor's own Lisp
  (let ((broken '())
        ;; a pattern, which system-relative-pathname would take as a name
        (files (directory (merge-pathnames "**/*.lisp" (asdf:system-source-directory "tuatara")))))
    (true (> (length files) 100) "~d files" (length files))
    (dolist (file files)
      (unless (or (search "/build/" (namestring file)) (search "/.project/" (namestring file))
                  ;; the grammar takes the symbols ^ and \\ (Calc's operators) and #\; before a
                  ;; blank for no Lisp: a gap in the grammar, not in the file
                  (search "lisp/org/formulas.lisp" (namestring file)))
        (multiple-value-bind (tree rope parser) (parse-string "commonlisp" (uiop:read-file-string file))
          (declare (ignore rope))
          (when (tuatara.syntax:tree-has-error-p tree)
            (push (enough-namestring file (asdf:system-source-directory "tuatara")) broken))
          (release-parse tree parser))))
    (is equal '() broken)))

(define-test (syntax latex-table-columns)
  ;; every & between a table's columns is coloured, those written up against
  ;; the text too (syntax/patches/latex.patch), an escaped \& is not
  (let ((text (format nil "\\begin{longtable}{lll}~%a & b & c \\\\~%d&e&\\textbf{f} \\\\~%\\end{longtable}~%Tom \\& Jerry~%")))
    (multiple-value-bind (tree rope parser) (parse-string "latex" text)
      (declare (ignore rope))
      (false (tuatara.syntax:tree-has-error-p tree))
      (release-parse tree parser))
    (let ((ranges (ranges-text text (rich-ranges "latex" text (tuatara.defaults::language-rules (language-named 'latex))))))
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
                   collect (string-trim '(#\Space #\Newline) (if (eq direction :forward) (subseq text p next) (subseq text next p)))
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
    (tuatara.syntax:syntax-highlights syntax query (tuatara.defaults::query-faces query :rich 0)
                                      0 (tuatara.buffer:buffer-size buffer))))

(defun fresh-rich-ranges (grammar text query)
  "The rich colours of TEXT parsed from scratch with GRAMMAR."
  (multiple-value-bind (tree rope parser) (parse-string grammar text)
    (unwind-protect (tuatara.syntax:rich-highlights query tree rope 0 (tuatara.text:rope-bytes rope)
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
                    (at (tuatara.text:char->byte (tuatara.buffer:buffer-text buffer) (random (1+ (length string)) random))))
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
           (is equal (fresh-rich-ranges "rust" (tuatara.buffer:buffer-string buffer) query) (full-rich-ranges syntax query))
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
         (tuatara.syntax:*syntax-wake-function* (lambda () (incf woken))))
    (unwind-protect
         (let* ((start (get-internal-real-time))
                (syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (true (< (- (get-internal-real-time) start) (* 0.1 internal-time-units-per-second)))
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
             (true (< (- (get-internal-real-time) stop) internal-time-units-per-second)))
           (false (tuatara.syntax:buffer-syntax buffer)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

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

(define-test (syntax grammar-faults)
  ;; a grammar whose own code fails while reading is turned off for the session:
  ;; the buffer keeps the colours it had, no text is parsed with the grammar
  ;; again, the editor says so, and closing the buffer leaves the parser alone
  (let* ((buffer (tuatara.buffer:make-buffer "*fault*" :text "fn main() {}"))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (said nil))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (let ((syntax (tuatara.syntax:buffer-syntax buffer))
                 (tuatara.syntax:*syntax-fault-function* (lambda (syntax) (setf said syntax))))
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
build compiles every grammar into a directory of the test's own, and loaded
from there, and that directory; the grammar is forgotten afterwards."
  (with-test-directory (directory)
    (let ((source (uiop:native-namestring (asdf:system-relative-pathname :tuatara "test/grammars/misbehaving/src/")))
          (library (uiop:native-namestring (merge-pathnames "misbehaving.so" directory))))
      (multiple-value-bind (output error code)
          (uiop:run-program (list "bash" "-c" ". \"$1\" && compile_grammar \"$2\" \"$3\"" "_"
                                  (tools-script "grammar-build.sh") source library)
                            :output :string :error-output :output :ignore-error-status t)
        (declare (ignore error))
        (is = 0 code "the test grammar did not compile:~%~a" output))
      (remhash "misbehaving" tuatara.syntax::*grammars*)
      (unwind-protect
           (funcall function
                    (let ((tuatara.syntax:*grammar-directories* (cons directory (tuatara.syntax:grammar-directories))))
                      (tuatara.syntax:find-grammar "misbehaving"))
                    directory)
        (remhash "misbehaving" tuatara.syntax::*grammars*)))))

(defun parse-in-buffer (grammar text)
  "A buffer holding TEXT whose syntax in GRAMMAR has been parsed, or given up,
and its SYNTAX; the buffer is to be killed by the caller."
  (let* ((buffer (tuatara.buffer:make-buffer "*misbehaving*" :text text))
         (syntax (tuatara.syntax:attach-syntax buffer grammar)))
    (loop repeat 500 while (tuatara.syntax:syntax-job syntax)
          do (let ((tuatara.syntax:*syntax-wait* 0.01)) (tuatara.syntax:wait-for-parse syntax)))
    (values buffer syntax)))

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
         (let ((parser (tuatara.syntax:make-parser grammar)))
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
  ;; for the session and said to have failed, its parser deleted
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((said nil))
       (unwind-protect
            (let ((tuatara.syntax:*syntax-fault-function* (lambda (syntax) (setf said syntax))))
              (multiple-value-bind (buffer syntax) (parse-in-buffer grammar "abc !s def")
                (unwind-protect
                     (progn
                       (is eq syntax said)
                       (true (typep (tuatara.syntax:grammar-faulted grammar) 'tuatara.syntax:grammar-failure))
                       (true (tuatara.syntax:syntax-broken syntax))
                       (true (tuatara.io:null-pointer-p (tuatara.syntax::syntax-parser syntax)))
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
     (let ((text (with-output-to-string (o) (dotimes (i 64) (write-string "word !m " o))))
           (parser (tuatara.syntax:make-parser grammar)))
       (unwind-protect
            (multiple-value-bind (tree why)
                (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text) :budget (* 8 1024 1024))
              (false tree)
              (is eq :memory why))
         (tuatara.syntax:delete-parser parser))
       (let ((held (tuatara.syntax::ts-call "tuatara_held" :i64))
             (said nil))
         (let ((tuatara.syntax:*syntax-memory-limit* (* 8 1024 1024))
               (tuatara.syntax:*syntax-memory-per-octet* 0)
               (tuatara.syntax:*syntax-slow-function* (lambda (syntax) (setf said syntax))))
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
               (tuatara.buffer:kill-buffer buffer))))
         (true (< (- (tuatara.syntax::ts-call "tuatara_held" :i64) held) (* 1024 1024))))))))

(defmacro found-in-child ((found case) &body body)
  "Run BODY with FOUND what test/syntax/memory.lisp found for CASE, run in a
process of its own held to 3 GB of address space, with the grammar of
test/grammars/misbehaving/ beside it (SURVIVING): one whose memory runs
short, or whose memory a scanner writes past, so that it is not the test
run's."
  (let ((grammar (gensym "GRAMMAR")) (directory (gensym "DIRECTORY")))
    `(call-with-misbehaving-grammar
      (lambda (,grammar ,directory)
        (declare (ignore ,grammar))
        (surviving (,found ,case :script "test/syntax/memory.lisp" :heap "1GB"
                                 :address-space (* 3 1024 1024 1024) :directory ,directory)
          ,@body)))))

(define-test (syntax parses-stop-when-memory-runs-short)
  ;; with the address space all but full, a parse that needs more is given up
  ;; once the memory kept back for that is given back, and its parser reads
  ;; again; while memory stays short no parse starts; a buffer whose parse
  ;; was given up so is parsed again at its next edit once memory is back
  (found-in-child (found "reserve")
    (is equal '(nil :short nil) (getf found :first))
    (false (getf found :back-while-full))
    (is equal '(nil :short) (getf found :while-short))
    (is eq :short (getf found :buffer-broken))
    (true (getf found :back-after))
    ;; the figure of the memory left is read again when it holds a parse down
    (true (> (getf found :limit-after 0) (* 64 1024 1024)))
    (is eq nil (getf found :buffer-broken-after))
    (true (getf found :buffer-tree-after))
    (true (getf found :parsed-after))))

(define-test (syntax tight-memory-budget-tried-again)
  ;; with little memory left, a parse's budget is held down by it: spent,
  ;; the parse is given up as memory running short, and the text is parsed
  ;; again at its next edit once memory is back
  (found-in-child (found "tight")
    (true (getf found :tight-limit))
    (is eq :short (getf found :buffer-broken))
    (is eq nil (getf found :buffer-broken-after))
    (true (getf found :buffer-tree-after))))

(define-test (syntax scanner-writing-past-its-buffer-faults)
  ;; a scanner writing past the buffer for its state, a little or much,
  ;; faults on the page after it before it reaches anything else: the parse
  ;; is given up as a memory fault, in a buffer the grammar is turned off,
  ;; and the editor carries on
  (found-in-child (found "overflow")
    (is equal '(:fault :fault) (getf found :faults))
    (true (getf found :buffer-fault))
    (true (getf found :buffer-broken))
    (true (getf found :other-parses))))

(define-test (syntax no-memory-for-one-request)
  ;; tree-sitter asked, in a parse, for more memory than can be had at once:
  ;; the parse is given up and its parser let go, and counted so in a
  ;; buffer; the editor carries on, the memory kept back untouched
  (found-in-child (found "huge")
    (is equal '(nil :short t) (getf found :in-parse))
    (true (getf found :back))
    (is eq :short (getf found :buffer-broken))
    (is eql 1 (getf found :let-go))
    (true (getf found :parsed-after))))

;; how the parse of a big file stops when it takes too long
(define-test (syntax slow-parses)
  ;; a parse that takes longer than it may is given up: the buffer shows no
  ;; syntax colours, is not parsed again (which would take as long), and the
  ;; editor says so
  (let ((text (with-output-to-string (o)
                (dotimes (i 20000) (format o "fn f~d() {{ let x = ~d; }}~%" i i)))))
    (let* ((grammar (tuatara.syntax:find-grammar "rust"))
           (parser (tuatara.syntax:make-parser grammar)))
      (unwind-protect
           (multiple-value-bind (tree why)
               (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text) :limit 0.0001)
             (false tree)
             (is eq :slow why))
        (tuatara.syntax:delete-parser parser)))
    (let* ((buffer (tuatara.buffer:make-buffer "*slow*" :text text))
           (said nil)
           (tuatara.syntax:*syntax-time-limit* 0.0001)
           (tuatara.syntax:*syntax-time-per-megabyte* 0)
           (tuatara.syntax:*syntax-slow-function* (lambda (syntax) (setf said syntax))))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (let ((tuatara.syntax:*syntax-wait* 10)) (tuatara.syntax:syntax-ready-tree syntax))
               (is eq :slow (tuatara.syntax:syntax-broken syntax))
               (is eq syntax said)
               (false (tuatara.syntax:syntax-tree syntax))
               (false (tuatara.syntax:syntax-job syntax))))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (syntax runaway-parses)
  ;; a parse is given up once it takes more memory than it may, as a grammar
  ;; gone wrong would take without end, and all it took is given back; the
  ;; buffer shows no syntax colours and the editor says so
  (let ((numbers (format nil "[~{~d~^,~}]~%" (loop repeat 100000 collect (random 10))))
        (held (lambda () (tuatara.syntax::ts-call "tuatara_held" :i64))))
    (let* ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json")))
           (before (funcall held)))
      (unwind-protect
           (multiple-value-bind (tree why)
               (tuatara.syntax:parse-rope parser (tuatara.text:string->rope numbers) :budget (* 1024 1024))
             (false tree)
             (is eq :memory why))
        (tuatara.syntax:delete-parser parser))
      (true (< (- (funcall held) before) (* 1024 1024))))
    ;; within the budget its length and tokens call for, it is parsed
    (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json"))))
      (unwind-protect
           (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope numbers)
                                                  :budget (tuatara.syntax:parse-memory-limit (length numbers)))))
             (true tree)
             (when tree (tuatara.syntax:delete-tree tree)))
        (tuatara.syntax:delete-parser parser)))
    (let* ((buffer (tuatara.buffer:make-buffer "*runaway*" :text numbers))
           (said nil)
           (tuatara.syntax:*syntax-memory-limit* (* 1024 1024))
           (tuatara.syntax:*syntax-memory-per-octet* 0)
           (tuatara.syntax:*syntax-slow-function* (lambda (syntax) (setf said syntax))))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::json-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (let ((tuatara.syntax:*syntax-wait* 10)) (tuatara.syntax:syntax-ready-tree syntax))
               (is eq :memory (tuatara.syntax:syntax-broken syntax))
               (is eq syntax said)
               (false (tuatara.syntax:syntax-tree syntax))))
        (tuatara.buffer:kill-buffer buffer))))
  ;; grammars that built without end, patched: Beancount's on a balance line
  ;; ending in # before a lone * at the end (syntax/patches/beancount.patch),
  ;; Tera's on a lone { at the end, as while typing {{ (tera.patch): parsed at once
  (loop for (grammar text) in (list (list "beancount" (format nil "2014-01-01 balance Assets:Cash 0.0 #~%*~%"))
                                    (list "tera" "{")
                                    (list "tera" (format nil "{% for x in y %}~%  {{ x }}~%{% endfor %}{")))
        do (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar grammar)))
                 (start (get-internal-real-time)))
             (unwind-protect
                  (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text)
                                                         :limit 5 :budget (* 64 1024 1024))))
                    (true tree "~a" grammar)
                    (true (< (- (get-internal-real-time) start) (floor internal-time-units-per-second 2)) "~a" grammar)
                    (when tree (tuatara.syntax:delete-tree tree)))
               (tuatara.syntax:delete-parser parser)))))

(define-test (syntax parsed-afresh-when-left-alone)
  ;; a tree brought up to date by parses after edits is parsed afresh, in the
  ;; background, once the text is left alone; an edit stops that parse, and one
  ;; given up leaves the tree and its colours as they were
  (flet ((settle (syntax)
           ;; until the parse under way, a parse afresh too, is taken in
           (loop repeat 1000 while (tuatara.syntax:syntax-job syntax)
                 do (sleep 0.01) (tuatara.syntax:catch-up syntax))))
    (let ((buffer (tuatara.buffer:make-buffer "*afresh*" :text (format nil "fn main() {~%    let x = 1;~%}~%")))
          (tuatara.syntax:*syntax-wait* 10))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (tuatara.syntax:syntax-ready-tree syntax)
               ;; parsed from nothing: nothing to put right
               (false (tuatara.syntax:syntax-patched syntax))
               (false (tuatara.syntax:refresh-syntax syntax))
               (tuatara.buffer:buffer-insert buffer 0 (format nil "// hi~%"))
               (tuatara.syntax:syntax-ready-tree syntax)
               (true (tuatara.syntax:syntax-patched syntax))
               ;; not left alone long enough yet: how long until it is
               (let ((tuatara.syntax:*syntax-refresh-delay* 60))
                 (let ((wait (tuatara.syntax:refresh-syntax syntax)))
                   (true (and wait (< 50 wait 60.01)))))
               (let ((tuatara.syntax:*syntax-refresh-delay* nil))
                 (false (tuatara.syntax:refresh-syntax syntax)))
               ;; left alone long enough: parsed afresh, redisplay not waiting for it
               ;; (the ten times its parse from nothing took left out, as the
               ;; machine may be slow)
               (let ((tuatara.syntax:*syntax-refresh-delay* 0))
                 (setf (tuatara.syntax::syntax-fresh-cost syntax) 0)
                 (false (tuatara.syntax:refresh-syntax syntax))
                 (true (tuatara.syntax::parse-job-refresh (tuatara.syntax:syntax-job syntax))))
               (settle syntax)
               (false (tuatara.syntax:syntax-patched syntax))
               (true (tuatara.syntax:syntax-current syntax))
               (true (tuatara.syntax:syntax-tree syntax))))
        (tuatara.buffer:kill-buffer buffer)))
    ;; an edit stops a parse afresh under way: the parse after the edit follows
    (let* ((text (with-output-to-string (o)
                   (dotimes (i 20000) (format o "fn f~d() {{ let x = ~d; }}~%" i i))))
           (buffer (tuatara.buffer:make-buffer "*afresh*" :text text))
           (tuatara.syntax:*syntax-wait* 60))
      (unwind-protect
           (progn
             (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
             (let ((syntax (tuatara.syntax:buffer-syntax buffer)))
               (tuatara.syntax:syntax-ready-tree syntax)
               (tuatara.buffer:buffer-insert buffer 0 (format nil "// a~%"))
               (tuatara.syntax:syntax-ready-tree syntax)
               (let ((tuatara.syntax:*syntax-refresh-delay* 0))
                 ;; a text is left alone ten times as long as its parse from nothing took
                 (let ((wait (tuatara.syntax:refresh-syntax syntax)))
                   (true (and wait (< (* 9 (/ (tuatara.syntax::syntax-fresh-cost syntax) internal-time-units-per-second))
                                      wait))))
                 (setf (tuatara.syntax::syntax-fresh-cost syntax) 0)
                 (tuatara.syntax:refresh-syntax syntax))
               (let ((job (tuatara.syntax:syntax-job syntax)))
                 (true (and job (tuatara.syntax::parse-job-refresh job)))
                 (tuatara.buffer:buffer-insert buffer 0 (format nil "// b~%"))
                 (true (tuatara.syntax::parse-job-obsolete job)))
               (settle syntax)
               (tuatara.syntax:syntax-ready-tree syntax)
               (true (tuatara.syntax:syntax-current syntax))
               (false (tuatara.syntax:syntax-no-refresh syntax))
               (false (tuatara.syntax:syntax-broken syntax))
               ;; a parse afresh given up: the tree kept, not tried again
               (let ((tuatara.syntax:*syntax-refresh-delay* 0)
                     (tuatara.syntax:*syntax-time-limit* 0.0001)
                     (tuatara.syntax:*syntax-time-per-megabyte* 0))
                 (setf (tuatara.syntax::syntax-fresh-cost syntax) 0)
                 (tuatara.syntax:refresh-syntax syntax))
               (settle syntax)
               (true (tuatara.syntax:syntax-no-refresh syntax))
               (false (tuatara.syntax:syntax-broken syntax))
               (true (tuatara.syntax:syntax-tree syntax))
               (let ((tuatara.syntax:*syntax-refresh-delay* 0))
                 (false (tuatara.syntax:refresh-syntax syntax))
                 (false (tuatara.syntax:syntax-job syntax)))))
        (tuatara.buffer:kill-buffer buffer)))))

(define-test (syntax colours-before-the-whole-parse)
  ;; a text too large to parse whole: the lines shown are coloured from a parse
  ;; of them and the text near them, as a parse of the whole text colours them
  (let* ((text (with-output-to-string (s)
                 (dotimes (i 400) (format s "fn f~d(p: u8) -> u8 {~%    g(p, \"line ~d\") // note~%}~%" i i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-slice*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0)))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 1000)
                (tuatara.syntax::*syntax-slice-reach* 200)
                (syntax (tuatara.syntax:attach-syntax buffer grammar))
                (start (search "fn f200(" text))
                (end (search "fn f203(" text)))
           (true (tuatara.syntax:syntax-too-big syntax))
           (false (tuatara.syntax:syntax-ready-tree syntax))
           (let ((shown (tuatara.syntax:syntax-highlights syntax query faces start end)))
             (true shown)
             (is equal (multiple-value-bind (tree rope parser) (parse-string "rust" text)
                         (unwind-protect (tuatara.syntax:rich-highlights query tree rope start end faces)
                           (release-parse tree parser)))
                 shown)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax limits)
  ;; a text over the size limit is not parsed until it shrinks
  (let ((buffer (tuatara.buffer:make-buffer "*syntax-limit*" :text (make-string 300 :initial-element #\x))))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 200)
                (syntax (tuatara.syntax:attach-syntax buffer (tuatara.syntax:find-grammar "rust"))))
           (true (tuatara.syntax:syntax-too-big syntax))
           (false (tuatara.syntax:syntax-ready-tree syntax))
           (tuatara.buffer:buffer-delete buffer 0 250)
           (let ((tuatara.syntax:*syntax-wait* 30))
             (true (tuatara.syntax:syntax-ready-tree syntax)))
           (false (tuatara.syntax:syntax-too-big syntax)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer)))
  ;; a file visited safely gets no mode by itself, and is never given to a
  ;; parser, even in the mode of its language
  (let* ((directory (uiop:ensure-directory-pathname (merge-pathnames (format nil "tuatara-syntax-~a/" (random 1000000))
                                                                      (uiop:temporary-directory))))
         (path (merge-pathnames "evidence.rs" directory)))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (o path :direction :output) (write-string "fn main() {}" o))
           (let ((buffer (tuatara.io:visit-file path :safe t)))
             (unwind-protect
                  (progn
                    (true (tuatara.io:safe-visit-p buffer))
                    (is eq 'tuatara.modes:fundamental-mode (type-of (tuatara.modes:buffer-major-mode buffer)))
                    (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
                    (false (tuatara.syntax:buffer-syntax buffer)))
               (tuatara.buffer:kill-buffer buffer))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(define-test (syntax language-modes)
  (is eq 'tuatara.defaults::rust-mode (tuatara.modes:mode-for-file #p"/src/main.rs"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/bin/run.sh"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/home/me/.bashrc"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/pkg/PKGBUILD"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/usr/share/bash-completion/completions/git"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/bin/script" "#!/usr/bin/env bash"))
  (is eq 'tuatara.defaults::bash-mode (tuatara.modes:mode-for-file #p"/bin/script" "#!/bin/sh -e"))
  (is eq 'tuatara.defaults::lisp-mode (tuatara.modes:mode-for-file #p"/src/x.lisp"))
  (dolist (name '("/src/Containerfile" "/src/Dockerfile" "/src/containerfile" "/src/Containerfile.dev"
                  "/src/Dockerfile.rust" "/src/app.dockerfile" "/src/app.containerfile"))
    (is eq 'tuatara.defaults::containerfile-mode (tuatara.modes:mode-for-file (pathname name)) name))
  ;; entering a language's mode parses the buffer; leaving it stops
  (let ((buffer (tuatara.buffer:make-buffer "*syntax-mode*" :text "fn main() {}")))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (true (tuatara.syntax:buffer-syntax buffer))
           (is string= "rust" (tuatara.syntax:grammar-name (tuatara.syntax:syntax-grammar (tuatara.syntax:buffer-syntax buffer))))
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::text-mode)
           (false (tuatara.syntax:buffer-syntax buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(defun colour-session (keys text extension &key (width 60))
  "Run KEYS on a file with EXTENSION holding TEXT; return the fake frontend."
  (let* ((directory (uiop:ensure-directory-pathname (merge-pathnames (format nil "tuatara-syntax-~a/" (random 1000000))
                                                                      (uiop:temporary-directory))))
         (path (merge-pathnames (format nil "sample.~a" extension) directory)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output) (write-string text o))
    (unwind-protect
         (multiple-value-bind (f editor) (run-keys keys :file path :width width)
           (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window editor)))
           f)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defmacro with-file-session (((f editor) keys text extension) &body body)
  "Run KEYS on a file with EXTENSION holding TEXT, and BODY with F and EDITOR
bound to the fake frontend and the editor; the file and its buffer go afterwards."
  (let ((directory (gensym "DIRECTORY")) (path (gensym "PATH")))
    `(let* ((,directory (uiop:ensure-directory-pathname
                         (merge-pathnames (format nil "tuatara-syntax-~a/" (random 1000000)) (uiop:temporary-directory))))
            (,path (merge-pathnames (format nil "sample.~a" ,extension) ,directory)))
       (ensure-directories-exist ,path)
       (with-open-file (o ,path :direction :output) (write-string ,text o))
       (unwind-protect
            (multiple-value-bind (,f ,editor) (run-keys ,keys :file ,path :width 60)
              (declare (ignorable ,f))
              (unwind-protect (progn ,@body)
                (tuatara.buffer:kill-buffer (tuatara.display:window-buffer (tuatara.command:editor-window ,editor)))))
         (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore)))))

(defun session-region-text (editor)
  "The text of the active region of EDITOR's current buffer, or NIL when none is active."
  (let ((b (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
    (when (tuatara.buffer:buffer-mark-active-p b)
      (multiple-value-bind (start end) (tuatara.buffer:buffer-region b)
        (tuatara.buffer:buffer-string b :start start :end end)))))

(define-test (syntax rules-compiled-in-the-background)
  ;; the rules of a language are compiled in the background the first time they
  ;; are needed: the text shows at once, its colours when they are compiled
  (let ((language (tuatara.defaults::find-language 'verilog)))
    (setf (tuatara.defaults::language-query language) nil
          (tuatara.defaults::language-injection-query language) nil
          (tuatara.defaults::language-locals-query language) nil)
    (let ((tuatara.defaults::*rules-wait* 0))
      (with-file-session ((f editor) "" (format nil "module m(input a, output b);~%  assign b = a;~%endmodule~%") "v")
        (let ((thread (gethash language tuatara.defaults::*warming*)))
          (when thread
            (is equal '() (aref (tuatara.frontend.fake:fake-spans f) 0))
            (sb-thread:join-thread thread :default nil)))
        (true (tuatara.defaults::rules-ready-p language))
        (let ((tuatara.command:*editor* editor)) (tuatara.command:redisplay editor))
        (true (plusp (length (aref (tuatara.frontend.fake:fake-spans f) 0))))))))

(define-test (syntax long-lines-coloured-where-shown)
  ;; a line scrolled sideways is coloured only where it shows: the rest of a
  ;; line of megabytes (a minified script) is not looked at
  (let* ((text (format nil "~{~a~}~%" (loop for i below 2000 collect (format nil "function f~d(a){return a+~d};" i i))))
         (rows (tuatara.display:layout (tuatara.text:string->rope text) 0 80 3 :hscroll 30000)))
    (destructuring-bind ((start . end)) (tuatara.display:shown-ranges rows)
      (true (<= 29000 start 30000))
      (true (<= (- end start) 200))))
  (let ((text (format nil "~{~a~}~%" (loop for i below 3000 collect (format nil "function f~d(a){return a+~d};" i i)))))
    (with-file-session ((f editor) "<end>" text "js")
      ;; the end of the line, coloured
      (true (find :keyword (aref (tuatara.frontend.fake:fake-spans f) 0)
                  :key (lambda (s) (let ((face (third s))) (if (listp face) (first face) face))))))))

(define-test (syntax marking-expressions)
  ;; m I and m A select inside and around the brackets around point, the
  ;; grammar leaving out the bracket in the string; again at once, one pair out
  (let ((text "(a (b \"x ) y\" c) d)"))
    (with-file-session ((f editor) "C-u 1 4 l m I" text "lisp")
      (is string= "b \"x ) y\" c" (session-region-text editor))
      (is = 4 (session-point editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "Marked inner expression")))
    (with-file-session ((f editor) "C-u 1 4 l m I m I" text "lisp")
      (is string= "a (b \"x ) y\" c) d" (session-region-text editor)))
    (with-file-session ((f editor) "C-u 1 4 l m A" text "lisp")
      (is string= "(b \"x ) y\" c)" (session-region-text editor)))
    (with-file-session ((f editor) "C-u 1 4 l C-u 2 m A" text "lisp")
      (is string= text (session-region-text editor)))
    (with-file-session ((f editor) "C-u 2 m A" text "lisp")
      (false (session-region-text editor)))
    ;; on a bracket: that bracket's pair
    (with-file-session ((f editor) "C-u 3 l m A" text "lisp")
      (is string= "(b \"x ) y\" c)" (session-region-text editor)))
    (with-file-session ((f editor) "C-u 1 5 l m A" text "lisp")
      (is string= "(b \"x ) y\" c)" (session-region-text editor)))
    ;; no pair further out, and an empty pair inside
    (with-file-session ((f editor) "C-u 1 4 l m A m A m A" text "lisp")
      (true (tuatara.frontend.fake:fake-frame-showing f "Not inside a balanced expression")))
    (with-file-session ((f editor) "l m I" "()" "lisp")
      (true (tuatara.frontend.fake:fake-frame-showing f "Empty expression"))
      (false (session-region-text editor)))
    ;; with no grammar every bracket counts, the one in the string too
    (with-file-session ((f editor) "C-u 1 4 l m I" text "txt")
      (is string= "a (b \"x ) y\" c" (session-region-text editor)))))

(define-test (syntax commenting-lines)
  ;; C comments or uncomments whole lines: the marker at the smallest
  ;; indentation, blank lines left alone, one undo
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil ";; (foo)~%;;   (bar)~%") (after "V j C" (format nil "(foo)~%  (bar)~%") "lisp"))
    (is string= (format nil "(foo)~%  (bar)~%") (after "V j C" (format nil ";; (foo)~%;;   (bar)~%") "lisp"))
    (is string= (format nil "  ;; (foo)~%~%  ;;   (bar)~%") (after "V j j C" (format nil "  (foo)~%~%    (bar)~%") "lisp"))
    ;; lines not all commented are commented once more
    (is string= (format nil ";; ;; a~%;; (b)~%") (after "V j C" (format nil ";; a~%(b)~%") "lisp"))
    (is string= (format nil "header~%") (after "C" (format nil ";;;; header~%") "lisp"))
    ;; the current line only, without a selection
    (is string= (format nil "  // let x = 1;~%  foo();~%") (after "C" (format nil "  let x = 1;~%  foo();~%") "rs"))
    (is string= (format nil "x~%") (after "C" (format nil "//x~%") "rs"))
    (is string= (format nil "# echo a~%#   echo b~%") (after "V j C" (format nil "echo a~%  echo b~%") "sh"))
    ;; undone in one step
    (is string= (format nil "(foo)~%  (bar)~%") (after "V j C u" (format nil "(foo)~%  (bar)~%") "lisp"))
    ;; a buffer with no comments says so and changes nothing
    (with-file-session ((f editor) "C" "plain" "txt")
      (is string= "plain" (session-text editor))
      (true (tuatara.frontend.fake:fake-frame-showing f "No comment syntax is defined")))))

(define-test (syntax indenting-leaves-text-alone)
  ;; indenting lines leaves the blanks of lines inside a comment or a string
  ;; begun on a line before, and of a line a string runs up to: they are text
  (flet ((indented (text mode)
           (let ((buffer (tuatara.buffer:make-buffer "*indent-text*" :text text)))
             (unwind-protect
                  (progn
                    (tuatara.modes:set-major-mode buffer mode)
                    (let ((tuatara.syntax:*syntax-wait* 10))
                      (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer))
                      (tuatara.defaults::indent-lines buffer 0 (tuatara.buffer:buffer-size buffer)))
                    (tuatara.buffer:buffer-string buffer))
               (tuatara.buffer:kill-buffer buffer)))))
    (let ((python (format nil "def f():~%    \"\"\"Doc.~%       kept as it is~%    \"\"\"~%    x = '''~%  raw~%'''~%    return x~%")))
      (is string= python (indented python 'tuatara.defaults::python-mode)))
    (let ((java (format nil "/**~% * Doc.~% *   kept~% */~%class A {~%  int x;~%}~%")))
      (is string= java (indented java 'tuatara.defaults::java-mode)))
    ;; C-M-\ (indent-region) does it to the selection, and without one says so
    (let (result)
      (with-file-session ((f editor) "V j C-M-\\" (format nil "class A {~%int x;~%}~%") "java")
        (setf result (session-text editor))
        (true (tuatara.frontend.fake:fake-frame-showing f "Indented 1 line")))
      (is string= (format nil "class A {~%  int x;~%}~%") result))
    (with-file-session ((f editor) "C-M-\\" (format nil "class A {~%int x;~%}~%") "java")
      (true (tuatara.frontend.fake:fake-frame-showing f "No selection: indent-region was not run")))
    ;; the lines around them are indented still
    (is string= (format nil "class A {~%  /*~%   * x~%   */~%  int y;~%}~%")
        (indented (format nil "class A {~%/*~%   * x~%   */~%int y;~%}~%") 'tuatara.defaults::java-mode))))

(define-test (syntax commenting-with-ends)
  ;; a language whose comments have an end (HTML, CSS) comments each line
  ;; between them, and uncomments it again
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil "<!-- <p>a</p> -->~%") (after "C" (format nil "<p>a</p>~%") "html"))
    (is string= (format nil "<p>a</p>~%") (after "C" (format nil "<!-- <p>a</p> -->~%") "html"))
    (is string= (format nil "  /* a { } */~%  /* b { } */~%") (after "V j C" (format nil "  a { }~%  b { }~%") "css"))
    ;; a blank line is left alone, the others line up
    (is string= (format nil "/* a { } */~%~%/*   b { } */~%") (after "V j j C" (format nil "a { }~%~%  b { }~%") "css"))))

(define-test (syntax comment-syntax-in-definitions)
  ;; each language's comments as its mode's definition gives them, or the definition of the mode it is made from
  (loop for (mode line block)
          in '((tuatara.defaults::lisp-mode (";" ";;") (nil)) (tuatara.defaults::lisp-interaction-mode (";" ";;") (nil))
               (tuatara.defaults::ielm-mode (";" ";;") (nil)) (tuatara.defaults::emacs-lisp-mode (";" ";;") (nil))
               (tuatara.defaults::scheme-mode (";" ";;") (nil)) (tuatara.defaults::wat-mode (";;" ";;") (nil))
               (tuatara.defaults::rust-mode ("//" "//") (nil)) (tuatara.defaults::c-mode ("//" "//") (nil))
               (tuatara.defaults::js-mode ("//" "//") (nil)) (tuatara.defaults::bash-mode ("#" "#") (nil))
               (tuatara.defaults::containerfile-mode ("#" "#") (nil)) (tuatara.defaults::python-mode ("#" "#") (nil))
               (tuatara.defaults::org-mode ("#" "#") (nil)) (tuatara.defaults::git-commit-mode ("#" "#") (nil))
               (tuatara.defaults::yaml-mode ("#" "#") (nil)) (tuatara.defaults::docker-compose-mode ("#" "#") (nil))
               (tuatara.defaults::sql-mode ("--" "--") (nil)) (tuatara.defaults::haskell-mode ("--" "--") (nil))
               (tuatara.defaults::erlang-mode ("%%" "%%") (nil)) (tuatara.defaults::latex-mode ("%" "%") (nil))
               (tuatara.defaults::fortran-mode ("!" "!") (nil)) (tuatara.defaults::vimrc-mode ("\"" "\"") (nil))
               (tuatara.defaults::forth-mode ("\\" "\\") (nil)) (tuatara.defaults::tla-mode ("\\*" "\\*") (nil))
               (tuatara.defaults::basic-mode ("REM" "REM") (nil)) (tuatara.defaults::jj-description-mode ("JJ:" "JJ:") (nil))
               (tuatara.defaults::html-mode (nil) ("<!--" "-->")) (tuatara.defaults::css-mode (nil) ("/*" "*/"))
               (tuatara.defaults::ocaml-mode (nil) ("(*" "*)")) (tuatara.defaults::jinja2-mode (nil) ("{#" "#}"))
               (tuatara.defaults::uxntal-mode (nil) ("(" ")")) (tuatara.defaults::vento-mode (nil) ("{{#" "#}}"))
               (tuatara.defaults::text-mode (nil) (nil)) (tuatara.defaults::json-mode (nil) (nil))
               (tuatara.modes:fundamental-mode (nil) (nil)))
        do (let ((instance (make-instance mode)))
             (is equal line (multiple-value-list (tuatara.modes:mode-comment-syntax instance)) "~(~a~)" mode)
             (is equal block (multiple-value-list (tuatara.modes:mode-block-comment-syntax instance)) "~(~a~)" mode)))
  ;; C comments a line of a language among those made from Helix's as its definition says
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil "-- select 1;~%") (after "C" (format nil "select 1;~%") "sql"))
    (is string= (format nil "%% f() -> ok.~%") (after "C" (format nil "f() -> ok.~%") "erl"))
    (is string= (format nil "{# a #}~%") (after "C" (format nil "a~%") "j2"))))

(define-test (syntax indenting-lisp)
  ;; > indents Lisp: indent-before.lisp, indented whole, gives indent-after.lisp
  (let ((tuatara.syntax:*syntax-wait* 10))
    (flet ((after (keys text)
             (let (result)
               (with-file-session ((f editor) keys text "lisp")
                 (setf result (session-text editor)))
               result))
           (body (text)
             ;; without the two lines of heading, which differ
             (subseq text (1+ (position #\Newline text :start (1+ (position #\Newline text)))))))
      (is string= (body (uiop:read-file-string (syntax-sample "indent-after.lisp")))
          (body (after "% >" (uiop:read-file-string (syntax-sample "indent-before.lisp")))))
      ;; one line is indented as TAB indents it; in a region a line lines up
      ;; with the one before it at its depth, as with indent-region
      (is string= (format nil "(~% foo bar~%     baz)~%") (after "j j >" (format nil "(~% foo bar~%baz)~%")))
      (is string= (format nil "(~% foo bar~% baz)~%") (after "% >" (format nil "(~% foo bar~%baz)~%")))
      ;; the lines a selection touches
      (is string= (format nil "(when x~%  (foo)~%  (bar))~%") (after "j V j >" (format nil "(when x~%(foo)~%(bar))~%")))
      ;; a bracket at the start of a line in a docstring starts no form
      (is string= (format nil "(defun f ()~%  \"A list:~%(1 2)~%\"~%  (list 1~%        2))~%")
          (after "j j j j j >" (format nil "(defun f ()~%  \"A list:~%(1 2)~%\"~%  (list 1~%2))~%")))
      ;; a line inside a string stays as it is
      (is string= (format nil "(defun f ()~%  \"A list:~%(1 2)\")~%") (after "j j >" (format nil "(defun f ()~%  \"A list:~%(1 2)\")~%")))
      ;; text the grammar cannot read is indented all the same
      (is string= (format nil "(defun f ()~%  (let ((2^n 4))~%    (print 2^n)))~%")
          (after "j j >" (format nil "(defun f ()~%  (let ((2^n 4))~%(print 2^n)))~%")))
      ;; undone in one step
      (is string= (format nil "(when x~%(foo)~%(bar))~%") (after "% > u" (format nil "(when x~%(foo)~%(bar))~%")))))
  ;; a buffer whose mode has no rules for indenting says so
  (with-file-session ((f editor) ">" "plain" "txt")
    (is string= "plain" (session-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No indentation rules for"))))

(define-test (syntax indenting-new-lines)
  ;; RET, o and O put the new line at its column as the language's mode
  ;; indents it, point where its text begins
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil "(defun f ()~%  (g)~%") (after "A RET ( g ) C-g" (format nil "(defun f ()~%") "lisp"))
    ;; a line begun and left blank loses its blanks
    (is string= (format nil "(defun f ()~%~%") (after "A RET C-g" (format nil "(defun f ()~%") "lisp"))
    (is string= (format nil "(defun f ()~%~%  x~%") (after "A RET RET x C-g" (format nil "(defun f ()~%") "lisp"))
    ;; one u undoes RET and the indenting with it
    (is string= (format nil "(defun f ()~%") (after "A RET C-g u" (format nil "(defun f ()~%") "lisp"))
    (is string= (format nil "(let ((a 1))~%  a)~%") (after "A RET a ) C-g" (format nil "(let ((a 1))~%") "lisp"))
    ;; a package's settings after :config two columns in, as Emacs's use-package
    (is string= (format nil "(load-package \"showing\"~%  :config)~%") (after "A RET : c o n f i g ) C-g" (format nil "(load-package \"showing\"~%") "lisp"))
    (is string= (format nil "fn main() {~%    x~%}~%") (after "o x C-g" (format nil "fn main() {~%}~%") "rs"))
    (is string= (format nil "fn main() {~%    x~%}~%") (after "j O x C-g" (format nil "fn main() {~%}~%") "rs"))
    (is string= (format nil "if true; then~%  echo a~%fi~%") (after "o e c h o SPC a C-g" (format nil "if true; then~%fi~%") "sh"))
    ;; O leaves the line it pushes down as it was
    (is string= (format nil "fn main() {~%    x~%        }~%") (after "j O x C-g" (format nil "fn main() {~%        }~%") "rs"))
    ;; a buffer with no rules for indenting: column 0, as before
    (is string= (format nil "  words~%more~%") (after "A RET m o r e C-g" (format nil "  words~%") "txt"))
    ;; a Python block goes on after its last line (@extend), but not after its return
    (is string= (format nil "def f(x):~%    y = x~%    z~%") (after "j A RET z C-g" (format nil "def f(x):~%    y = x~%") "py"))
    (is string= (format nil "def f(x):~%    return x~%z~%") (after "j A RET z C-g" (format nil "def f(x):~%    return x~%") "py"))
    (is string= (format nil "def f(x):~%    y = x~%    z~%") (after "j o z C-g" (format nil "def f(x):~%    y = x~%") "py"))
    ;; nested: the innermost block reaches on
    (is string= (format nil "def f(x):~%    if x:~%        y = 1~%        z~%")
        (after "j j A RET z C-g" (format nil "def f(x):~%    if x:~%        y = 1~%") "py"))
    ;; a Go block's closing brace is not passed by it: spaces, or a tab where Go is set to have tabs
    (is string= (format nil "func f() {~%    x~%}~%") (after "o x C-g" (format nil "func f() {~%}~%") "go"))
    (with-mode-tabs ((go :tabs t))
      (is string= (format nil "func f() {~%~cx~%}~%" #\Tab) (after "o x C-g" (format nil "func f() {~%}~%") "go"))))
  ;; turned off in the init file: column 0
  (let ((tuatara.defaults::*indent-new-lines* nil))
    (let (result)
      (with-file-session ((f editor) "o x C-g" (format nil "fn main() {~%}~%") "rs")
        (setf result (session-text editor)))
      (is string= (format nil "fn main() {~%x~%}~%") result))))

(define-test (syntax new-line-in-a-block-not-closed-yet)
  ;; RET after a shell block's opening line puts the new line inside the block
  ;; before its closing word is typed, as it does once the block is closed
  (flet ((column (text)
           (let (result)
             (with-file-session ((f editor) "M-> A RET x C-g" text "sh")
               (setf result (session-text editor)))
             (let ((line (find "x" (uiop:split-string result :separator '(#\Newline))
                               :test #'string= :key (lambda (line) (string-left-trim " " line)) :from-end t)))
               (and line (position #\x line))))))
    (loop for (text expected)
            in '(("if [ -n \"$1\" ]; then" 2) ("if true; then~%  echo a" 2) ("if a; then~%  x~%else" 2)
                 ("if a; then~%  x~%elif b; then" 2) ("if a; then~%  if b; then" 4)
                 ("for a in b; do" 2) ("while true; do" 2) ("until a; do" 2) ("select a in b c; do" 2)
                 ("f() {" 2) ("function f {" 2) ("f() {~%  echo a" 2) ("{" 2)
                 ("case $1 in" 2) ("case $1 in~%  a)" 4) ("case $1 in~%  a) echo;;" 2)
                 ("case $1 in~%  a)~%    echo" 4) ("case $1 in~%  a)~%    echo~%    ;;" 2)
                 ;; as yet a blank line ends a block not closed, and an unclosed function
                 ;; reads as one piece of text the grammar cannot place: Emacs gives 2 and 6
                 ("if a; then~%" 0) ("f() {~%  case $1 in~%    a)" 4)
                 ;; closed, the line after is outside
                 ("if true; then~%  echo~%fi" 0) ("for a in b; do~%  x~%done" 0)
                 ("case x in~%  a) echo;;~%esac" 0) ("case x in~%  a)~%esac" 0))
          do (is = expected (column (format nil text)) "after ~s" text))))

(define-test (syntax indenting-lisp-quickly)
  ;; a form of 100,000 lines is indented in one pass, and one line at its end
  ;; without reading more than that form
  (let* ((text (with-output-to-string (out)
                 (format out "(defparameter *table*~%'(")
                 (dotimes (i 100000) (format out "(~d \"row ~d\" :key ~d)~%" i i (* 7 i)))
                 (format out "))~%")))
         (buffer (tuatara.buffer:make-buffer "*indent*" :text text)))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::lisp-mode)
           (let ((tuatara.syntax:*syntax-wait* 60))
             (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer)))
           (let ((start (get-internal-real-time)))
             (is = 100001 (tuatara.defaults::indent-lines buffer 0 (tuatara.buffer:buffer-size buffer)))
             (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))))
           (is string= "    (99999 \"row 99999\" :key 699993)"
               (let ((line (tuatara.buffer:buffer-goto-line buffer 100000)))
                 (tuatara.buffer:buffer-string buffer :start line :end (tuatara.buffer:buffer-line-end buffer line))))
           (let ((line (tuatara.buffer:buffer-goto-line buffer 100000))
                 (start (get-internal-real-time)))
             (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit line (+ line 3))))
             (is = 1 (tuatara.defaults::indent-lines buffer line line))
             (true (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second)))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax indenting-by-the-tree)
  ;; Rust and shell scripts are indented by their languages' rules: the samples,
  ;; every line indented as expected, stay as they are, and come back from the
  ;; left margin, but for lines inside strings, which keep their blanks
  (flet ((indented (text mode)
           (let ((buffer (tuatara.buffer:make-buffer "*indent*" :text text)))
             (unwind-protect
                  (progn
                    (tuatara.modes:set-major-mode buffer mode)
                    (let ((tuatara.syntax:*syntax-wait* 10))
                      (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer))
                      (tuatara.defaults::indent-lines buffer 0 (tuatara.buffer:buffer-size buffer)))
                    (tuatara.buffer:buffer-string buffer))
               (tuatara.buffer:kill-buffer buffer))))
         (flat (text)
           (format nil "~{~a~^~%~}" (mapcar (lambda (line) (string-left-trim " " line))
                                            (uiop:split-string text :separator '(#\Newline))))))
    (let ((rust (uiop:read-file-string (syntax-sample "indentation/rust.rs")))
          (shell (uiop:read-file-string (syntax-sample "indentation/bash.sh"))))
      (is string= rust (indented rust 'tuatara.defaults::rust-mode))
      (is string= shell (indented shell 'tuatara.defaults::bash-mode))
      (is string= (cl-ppcre:regex-replace-all "(?m)^ +(indented (in string\";|raw\"#;))$" rust "\\1")
          (indented (flat rust) 'tuatara.defaults::rust-mode))
      (is string= shell (indented (flat shell) 'tuatara.defaults::bash-mode)))
    ;; C, whose sample is indented two columns a level; four is the level here
    (let ((c (uiop:read-file-string (syntax-sample "indentation/c.c")))
          (language (language-named 'c)))
      (let ((level (tuatara.defaults::language-indent language)))
        (unwind-protect
             (progn
               (tuatara-user:set-indentation 'c 2)
               (is string= c (indented c 'tuatara.defaults::c-mode))
               (is string= c (indented (flat c) 'tuatara.defaults::c-mode)))
          (setf (tuatara.defaults::language-indent language) level)))
      (is = 4 (tuatara.defaults::language-indent language))))
  ;; > in a Rust or shell buffer, on one line or the lines a selection touches
  (flet ((after (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-text editor)))
             result)))
    (is string= (format nil "fn f() {~%    let x = 1;~%}~%") (after "j >" (format nil "fn f() {~%let x = 1;~%}~%") "rs"))
    (is string= (format nil "if true; then~%  echo a~%fi~%") (after "j V j >" (format nil "if true; then~%echo a~%  fi~%") "sh"))
    (is string= (format nil "if true; then~%echo a~%  fi~%") (after "j V j > u" (format nil "if true; then~%echo a~%  fi~%") "sh"))))

(define-test (syntax selecting-by-the-tree)
  ;; m f / m F, m t / m T, m r / m R and m c / m C select the function, type,
  ;; parameter or comment around point, inside or all of it; M-o grows the
  ;; selection to the syntax around it, M-i goes back, M-n and M-p step to the
  ;; piece beside it
  (flet ((region (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-region-text editor)))
             result))
         (to (line column)
           ;; keys that put point on LINE (from 0) at COLUMN
           (format nil "~{~a~^ ~}" (append (make-list line :initial-element "j")
                                           (make-list column :initial-element "l")))))
    (let ((rust (format nil "fn main() {~%    let x = foo(a, (b, c), d);~%    // one~%    // two~%}~%struct P { x: i32 }~%")))
      (is string= (format nil "{~%    let x = foo(a, (b, c), d);~%    // one~%    // two~%}")
          (region (format nil "~a m f" (to 1 20)) rust "rs"))
      (is string= (subseq rust 0 (search "struct" rust)) (format nil "~a~%" (region (format nil "~a m F" (to 1 20)) rust "rs")))
      (is string= "(b, c)" (region (format nil "~a m r" (to 1 20)) rust "rs"))
      (is string= "(b, c)," (region (format nil "~a m R" (to 1 20)) rust "rs"))
      (is string= "a," (region (format nil "~a m R" (to 1 16)) rust "rs"))
      (is string= "{ x: i32 }" (region (format nil "~a m t" (to 5 12)) rust "rs"))
      (is string= "struct P { x: i32 }" (region (format nil "~a m T" (to 5 12)) rust "rs"))
      (is string= "// two" (region (format nil "~a m c" (to 3 6)) rust "rs"))
      (is string= (format nil "// one~%    // two") (region (format nil "~a m C" (to 3 6)) rust "rs"))
      ;; pressed again at once, the next function out: from a closure's body to the fn's
      (let ((nested (format nil "fn f() {~%    let g = |x| {~%        x + 1~%    };~%}~%")))
        (is string= (format nil "{~%        x + 1~%    }") (region (format nil "~a m f" (to 2 8)) nested "rs"))
        (is string= (subseq nested (search "{" nested) (1+ (search "}" nested :from-end t)))
            (region (format nil "~a m f m f" (to 2 8)) nested "rs")))
      ;; growing, shrinking and stepping
      (is string= "(b, c)" (region (format nil "~a M-o" (to 1 20)) rust "rs"))
      (is string= "(a, (b, c), d)" (region (format nil "~a M-o M-o" (to 1 20)) rust "rs"))
      (is string= "foo(a, (b, c), d)" (region (format nil "~a M-o M-o M-o" (to 1 20)) rust "rs"))
      (is string= "foo" (region (format nil "~a M-o" (to 1 12)) rust "rs"))
      (is string= "(b, c)" (region (format nil "~a M-o M-o M-o M-i M-i" (to 1 20)) rust "rs"))
      (false (region (format nil "~a M-o M-i" (to 1 20)) rust "rs"))
      (is string= "d" (region (format nil "~a M-o M-n" (to 1 20)) rust "rs"))
      (is string= "a" (region (format nil "~a M-o M-p" (to 1 20)) rust "rs"))
      (is string= "(a, (b, c), d)" (region (format nil "~a M-o M-o M-o M-i" (to 1 20)) rust "rs"))
      ;; a selection not grown by M-o shrinks to the first piece inside it
      (is string= "x: i32" (region (format nil "~a m t M-i" (to 5 12)) rust "rs")))
    ;; shell scripts and Lisp
    (is string= (format nil "{~%  echo \"hi\" there~%}") (region "j l l m f" (format nil "greet() {~%  echo \"hi\" there~%}~%") "sh"))
    (is string= "there" (region "j l l l l l l l l l l l l m r" (format nil "greet() {~%  echo \"hi\" there~%}~%") "sh"))
    (let ((lisp (format nil "(defun area (shape)~%  \"The area.\"~%  (* (width shape) 2))~%(defstruct point x y)~%")))
      (is string= (format nil "\"The area.\"~%  (* (width shape) 2)") (region "j j l l l l l l m f" lisp "lisp"))
      (is string= (subseq lisp 0 (search (format nil "~%(defstruct") lisp)) (region "j j l l l l l l m F" lisp "lisp"))
      (is string= "shape" (region "l l l l l l l l l l l l l m r" lisp "lisp"))
      (is string= "x y" (region "j j j l l l l l l l l l l l l l l l l l m t" lisp "lisp"))
      (is string= "width" (region "j j l l l l l l l M-o" lisp "lisp"))
      (is string= "(width shape)" (region "j j l l l l l l l M-o M-o" lisp "lisp"))))
  ;; with nothing around point, or no tree, it says so
  (with-file-session ((f editor) "m f" (format nil "struct P { x: i32 }~%") "rs")
    (false (session-region-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No function around point")))
  (with-file-session ((f editor) "M-o" "plain" "txt")
    (true (tuatara.frontend.fake:fake-frame-showing f "No syntax tree to select by"))))

(define-test (syntax selecting-tests-entries-elements)
  ;; m y / m Y a test, m e / m E an entry, m x / m X an element, inside or all
  ;; of it: from the grammar and its rules alone, nothing else installed
  (flet ((region (keys text extension)
           (let (result)
             (with-file-session ((f editor) keys text extension)
               (setf result (session-region-text editor)))
             result))
         (to (line column)
           (format nil "~{~a~^ ~}" (append (make-list line :initial-element "j")
                                           (make-list column :initial-element "l")))))
    (let ((rust (format nil "#[test]~%fn a() {~%    x();~%}~%~%fn b() {}~%~%#[test]~%#[should_panic]~%// why~%fn c() {}~%")))
      (is string= (format nil "{~%    x();~%}") (region (format nil "~a m y" (to 2 4)) rust "rs"))
      (is string= (format nil "fn a() {~%    x();~%}") (region (format nil "~a m Y" (to 2 4)) rust "rs"))
      ;; a function after a test is not one; one after more attributes and a comment is
      (false (region (format nil "~a m Y" (to 5 4)) rust "rs"))
      (is string= "fn c() {}" (region (format nil "~a m Y" (to 10 4)) rust "rs")))
    (is string= "assert f() == 2" (region (format nil "~a m y" (to 1 6)) (format nil "def test_f():~%    assert f() == 2~%") "py"))
    (let ((json (format nil "{\"a\": 1, \"b\": [2, 3]}~%")))
      (is string= "1" (region (format nil "~a m e" (to 0 6)) json "json"))
      (is string= "\"a\": 1" (region (format nil "~a m E" (to 0 6)) json "json"))
      (is string= "3" (region (format nil "~a m E" (to 0 18)) json "json"))
      (is string= "\"b\": [2, 3]" (region (format nil "~a m E m E" (to 0 18)) json "json")))
    (is string= "name: app" (region (format nil "~a m E" (to 0 7)) (format nil "name: app~%ports:~%  - 80~%") "yaml"))
    (let ((html (format nil "<ul><li>one</li><li>two</li></ul>~%")))
      (is string= "two" (region (format nil "~a m x" (to 0 20)) html "html"))
      (is string= "<li>two</li>" (region (format nil "~a m X" (to 0 20)) html "html"))
      (is string= "<ul><li>one</li><li>two</li></ul>" (region (format nil "~a m X m X" (to 0 20)) html "html"))))
  (with-file-session ((f editor) "j m y" (format nil "fn main() {~%    x();~%}~%") "rs")
    (false (session-region-text editor))
    (true (tuatara.frontend.fake:fake-frame-showing f "No test around point"))))

(define-test (syntax selecting-quickly)
  ;; selecting a function or a test reads the text near point, not the
  ;; whole file
  (let* ((text (with-output-to-string (out)
                 (dotimes (i 20000)
                   (format out "#[test]~%fn f~d(a: i32, b: i32) -> i32 {~%    // sum~%    a + b~%}~%~%" i))))
         (buffer (tuatara.buffer:make-buffer "*select*" :text text)))
    (unwind-protect
         (progn
           (tuatara.modes:set-major-mode buffer 'tuatara.defaults::rust-mode)
           (let ((tuatara.syntax:*syntax-wait* 60))
             (tuatara.syntax:syntax-ready-tree (tuatara.syntax:buffer-syntax buffer)))
           (let* ((language (tuatara.defaults::find-language 'rust))
                  (query (tuatara.defaults::language-compiled-query language :text-objects))
                  (rope (tuatara.buffer:buffer-text buffer))
                  (point (tuatara.buffer:buffer-goto-line buffer 119995))
                  (start (get-internal-real-time))
                  (ranges (tuatara.defaults::text-object-ranges
                           query (tuatara.syntax:syntax-tree (tuatara.syntax:buffer-syntax buffer)) rope
                           "function.around" (- point 65536) (min (tuatara.text:rope-bytes rope) (+ point 65536)))))
             (true (< (- (get-internal-real-time) start) (* 1/5 internal-time-units-per-second)))
             (is string= (format nil "fn f19999(a: i32, b: i32) -> i32 {~%    // sum~%    a + b~%}")
                 (let ((r (tuatara.defaults::smallest-around ranges point point nil)))
                   (and r (tuatara.buffer:buffer-string buffer :start (car r) :end (cdr r)))))
             ;; so does a test, its rule anchored to the attribute before it
             (let* ((start (get-internal-real-time))
                    (tests (tuatara.defaults::text-object-ranges
                            query (tuatara.syntax:syntax-tree (tuatara.syntax:buffer-syntax buffer)) rope
                            "test.around" (- point 65536) (min (tuatara.text:rope-bytes rope) (+ point 65536)))))
               (true (< (- (get-internal-real-time) start) (* 1/5 internal-time-units-per-second)))
               (true (tuatara.defaults::smallest-around tests point point nil)))))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax text-in-other-languages)
  ;; the commands after RUN in a Containerfile are coloured as shell, and the
  ;; body of a Rust macro as Rust, over the colours around them
  (let* ((f (colour-session "" (format nil "FROM fedora:44~%RUN dnf install -y gcc && echo \"$HOME\"~%") "containerfile"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 1)))
    (is equal '(0 3 :keyword) (first spans))
    (true (member '(4 7 :function) spans :test #'equal))
    (true (member '(23 25 :operator) spans :test #'equal))
    (true (member '(26 30 :function) spans :test #'equal)))
  (let* ((f (colour-session "" (format nil "fn f() { m!(if a { b } else { c }); }~%") "rs"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((top (start end)
             ;; the face painted last over the text, where colours lie over others
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :keyword (top 12 14))
      (is eq :keyword (top 23 27))))
  ;; a comment with no marks in it (TODO, FIXME) keeps its colour alone
  (let ((f (colour-session "" (format nil "// a comment~%fn f() {}~%") "rs")))
    (is equal '((0 12 :comment)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
  ;; a #! line names the language where no name is given: a Markdown code
  ;; block, a justfile's recipe
  (let ((spans (aref (tuatara.frontend.fake:fake-spans
                      (colour-session "" (format nil "```~%#!/bin/bash~%echo $HOME~%```~%") "md"))
                     2)))
    (true (find :function spans :key (lambda (s) (let ((f (third s))) (if (listp f) (first f) f))))))
  (let ((spans (aref (tuatara.frontend.fake:fake-spans
                      (colour-session "" (format nil "run:~%    #!/usr/bin/env python3~%    def f(): pass~%") "just"))
                     2)))
    (true (find :keyword spans :key (lambda (s) (let ((f (third s))) (if (listp f) (first f) f))))))
  ;; names of languages this editor does not colour are passed over: the
  ;; block has Markdown's colour alone
  (let ((f (colour-session "" (format nil "```nosuchlang~%x + 1~%```~%") "md")))
    (is equal '((0 5 :markup.raw.block)) (aref (tuatara.frontend.fake:fake-spans f) 1)))
  ;; a file name given in the text (Nix's writeText) is taken literally: a [ or * in it is only a character
  (true (tuatara.defaults::language-for-injection '(:filename "x.py")))
  (is eq (tuatara.defaults::language-for-injection '(:filename "x.py"))
      (tuatara.defaults::language-for-injection '(:filename "[x] *?.py"))))

(define-test (syntax long-text-in-other-languages)
  ;; a long text in another language (a code block of megabytes) is parsed
  ;; only near the lines being coloured, which are coloured all the same
  (let* ((text (with-output-to-string (o)
                 (format o "```python~%")
                 (dotimes (i 100000) (format o "def f~d(x):~%    return x~%" i))
                 (format o "```~%")))
         (markdown (tuatara.defaults::find-language 'markdown))
         (injections (list (tuatara.defaults::language-compiled-query markdown :injection) (tuatara.defaults::injection-resolver :rich 3))))
    (multiple-value-bind (tree rope parser) (parse-string "markdown" text)
      (unwind-protect
           (let* ((middle (tuatara.text:rope-line-start rope (floor (length text) 2)))
                  (ranges (tuatara.syntax::injected-highlights injections tree rope middle (+ middle 60))))
             ;; def as a keyword, the function's name as a function, in the middle
             (true (find-if (lambda (r) (and (<= middle (first r)) (eq (third r) :keyword))) ranges))
             (true (find-if (lambda (r) (and (<= middle (first r)) (eq (third r) :function))) ranges))
             ;; the text parsed is what lies near: the block's lines, cut near the window
             (destructuring-bind ((name spans)) (tuatara.syntax::injection-layers
                                                 (first injections) tree rope middle (+ middle 60)
                                                 (- middle 1000) (+ middle 1000))
               (is string= "python" name)
               (true (every (lambda (span) (and (<= (- middle 1000) (car span)) (<= (cdr span) (+ middle 1000)))) spans))))
        (release-parse tree parser)))))

(define-test (syntax local-names)
  ;; a parameter is coloured as a parameter wherever its function uses it;
  ;; other names keep their colours
  (let* ((f (colour-session "" (format nil "fn f(count: i32) -> i32 { let x = count + 1; x }~%") "rs"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((top (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (top 5 10))
      (is eq :variable.parameter (top 34 39))
      (is eq :variable (top 30 31))
      (is eq :variable (top 45 46))))
  ;; a name is its definition's in the innermost scope that has it: count in g
  ;; is g's own, not f's parameter
  (let* ((f (colour-session "" (format nil "fn f(count: i32) {} fn g() { let count = 1; count; }~%") "rs"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((top (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (top 5 10))
      (is eq :variable (top 44 49)))))

(define-test (syntax local-names-in-embedded-text)
  ;; in a Rust block of a Markdown file the parameter is coloured as one where
  ;; the function uses it, as in a Rust file
  (let* ((f (colour-session "" (format nil "```rust~%fn f(count: i32) -> i32 { count + 1 }~%```~%") "md"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 1)))
    (flet ((top (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (top 5 10))
      (is eq :variable.parameter (top 26 31)))))

(define-test (syntax local-names-in-rules)
  ;; #is-not? local: a pattern for a builtin passes over a name that is local
  ;; where it is used; and a scope whose rules say local.scope-inherits false
  ;; (a Ruby method) sees no name defined outside it
  (is equal '(:local nil nil t) (tuatara.syntax::parse-predicate '("is-not?" "local")))
  (flet ((setup (language text)
           (let* ((language (tuatara.defaults::find-language language))
                  (grammar (tuatara.syntax:find-grammar (tuatara.defaults::language-grammar language)))
                  (rope (tuatara.text:string->rope text))
                  (parser (tuatara.syntax:make-parser grammar)))
             (values (tuatara.syntax:parse-rope parser rope) rope language parser)))
         (resolved-starts (table)
           (sort (loop for key being the hash-keys of table collect (ash key -31)) #'<)))
    (let ((text (format nil "console.log(1);~%function f(console) { return console; }~%")))
      (multiple-value-bind (tree rope language parser) (setup 'javascript text)
        (unwind-protect
             (multiple-value-bind (ranges resolved)
                 (tuatara.syntax::local-highlights (tuatara.defaults::language-compiled-query language :locals) tree rope 0 0 (length text)
                                                   (constantly nil))
               (declare (ignore ranges))
               (let* ((used (search "return console" text))
                      (at (+ used 7))
                      (query (tuatara.defaults::language-compiled-query language :colouring))
                      (faces (map 'vector (lambda (name) (intern (string-upcase name) :keyword)) (tuatara.syntax:query-captures query))))
                 (is equal (list at) (resolved-starts resolved))
                 (flet ((face-at (start local)
                          (let ((tuatara.syntax::*local-references* local))
                            (third (find start (tuatara.syntax::rich-highlights query tree rope 0 (length text) faces)
                                         :key #'first)))))
                   (is eq :variable.builtin (face-at 0 resolved))
                   (is eq :variable.builtin (face-at at nil))
                   (is eq :variable (face-at at resolved)))))
          (tuatara.syntax:delete-tree tree)
          (tuatara.syntax:delete-parser parser))))
    (let ((text (format nil "proc { |a|~%  def m~%    a~%  end~%  a~%}~%")))
      (multiple-value-bind (tree rope language parser) (setup 'ruby text)
        (unwind-protect
             (multiple-value-bind (ranges resolved)
                 (tuatara.syntax::local-highlights (tuatara.defaults::language-compiled-query language :locals) tree rope 0 0 (length text)
                                                   (constantly nil))
               (declare (ignore ranges))
               ;; the a after the method is the block's parameter; the one in it is not
               (is equal (list (search (format nil "a~%}") text)) (resolved-starts resolved)))
          (tuatara.syntax:delete-tree tree)
          (tuatara.syntax:delete-parser parser))))
    ;; every local name used gets its colour, not only the last found
    (let ((text (format nil "function f(a, b) { return a + b; }~%")))
      (multiple-value-bind (tree rope language parser) (setup 'javascript text)
        (unwind-protect
             (let ((ranges (tuatara.syntax::local-highlights (tuatara.defaults::language-compiled-query language :locals) tree rope 0 0 (length text)
                                                             (constantly :variable))))
               (is equal (list (search "a + b" text) (+ 4 (search "a + b" text)))
                   (remove-if (lambda (start) (< start (search "return" text))) (mapcar #'first ranges))))
          (tuatara.syntax:delete-tree tree)
          (tuatara.syntax:delete-parser parser))))))

(define-test (syntax colours-on-screen)
  (let ((f (colour-session "" (format nil "fn main() {~%    let x = 1; // one~%}~%") "rs")))
    (is equal '((0 2 :keyword) (3 7 :function) (7 9 :punctuation.bracket) (10 11 :punctuation.bracket))
        (aref (tuatara.frontend.fake:fake-spans f) 0))
    (is equal '((4 7 :keyword) (8 9 :variable) (10 11 :operator) (12 13 :constant.numeric) (13 14 :punctuation) (15 21 :comment))
        (aref (tuatara.frontend.fake:fake-spans f) 1)))
  ;; typed text is coloured in the frame that shows it
  (let ((f (colour-session "i p u b SPC C-g" (format nil "fn main() {}~%") "rs")))
    (is string= "pub fn main() {}" (tuatara.frontend.fake:fake-screen-line f 0))
    (is equal '(0 3 :keyword) (first (aref (tuatara.frontend.fake:fake-spans f) 0))))
  ;; the sparse style, chosen for the language, colours less
  (unwind-protect
       (progn
         (tuatara-user:set-colouring 'rust :sparse)
         (let ((f (colour-session "" (format nil "fn main() {~%    let x = 1; // one~%}~%") "rs")))
           (is equal '((0 2 :keyword) (3 7 :function)) (aref (tuatara.frontend.fake:fake-spans f) 0))
           (is equal '((4 7 :keyword) (12 13 :constant.numeric) (15 21 :comment))
               (aref (tuatara.frontend.fake:fake-spans f) 1)))
         (tuatara-user:set-colouring 'rust :sparse :level 1)
         (let ((f (colour-session "" (format nil "fn main() {~%    let x = 1; // one~%}~%") "rs")))
           (is equal '() (aref (tuatara.frontend.fake:fake-spans f) 0))
           (is equal '((15 21 :comment)) (aref (tuatara.frontend.fake:fake-spans f) 1)))
         (tuatara-user:set-colouring 'rust :sparse :level 4)
         (let ((f (colour-session "" (format nil "fn main() {~%}~%") "rs")))
           (is equal '((0 2 :keyword) (3 7 :function) (7 9 :punctuation.bracket) (10 11 :punctuation.bracket))
               (aref (tuatara.frontend.fake:fake-spans f) 0))))
    (tuatara-user:set-colouring 'rust :rich :level 3))
  ;; M-x rotate-colouring goes from rich to sparse to none and round again
  (unwind-protect
       (let ((text (format nil "fn main() {~%    let x = 1; // one~%}~%")))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" text "rs")))
           (true (tuatara.frontend.fake:fake-frame-showing f "Rust colouring: sparse"))
           (is equal '((0 2 :keyword) (3 7 :function)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" text "rs")))
           (true (tuatara.frontend.fake:fake-frame-showing f "Rust colouring: none"))
           (is equal '() (aref (tuatara.frontend.fake:fake-spans f) 0)))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" text "rs")))
           (true (tuatara.frontend.fake:fake-frame-showing f "Rust colouring: rich"))
           (is equal '((0 2 :keyword) (3 7 :function) (7 9 :punctuation.bracket) (10 11 :punctuation.bracket))
               (aref (tuatara.frontend.fake:fake-spans f) 0)))
         (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" "plain" "txt")))
           (true (tuatara.frontend.fake:fake-frame-showing f "is not in the mode of a language"))))
    (tuatara-user:set-colouring 'rust :rich))
  (fail (tuatara-user:set-colouring 'cobol :rich))
  (fail (tuatara-user:set-colouring 'rust :loud))
  (fail (tuatara-user:set-colouring 'lisp :sparse :level 5)))

(define-test (syntax colouring-by-mode)
  ;; a style for every buffer (t), one for the modes made from a mode
  ;; (prog-mode) and a language's own: the language's first, then the nearest
  ;; mode's, then every buffer's
  (let ((rust (tuatara.defaults::find-language 'rust))
        (bash (tuatara.defaults::find-language 'bash))
        (csv (make-instance 'tuatara.defaults::csv-mode)))
    (flet ((colouring (language) (multiple-value-list (tuatara.defaults::language-colouring language)))
           (unset ()
             (setf tuatara.defaults::*colouring-rules* '()
                   tuatara.defaults::*csv-colouring* nil
                   (tuatara.defaults::language-style rust) nil (tuatara.defaults::language-level rust) nil
                   (tuatara.defaults::language-style bash) nil (tuatara.defaults::language-level bash) nil)))
      (unwind-protect
           (progn
             (unset)
             (is equal '(:rich 3) (colouring bash))
             (tuatara-user:set-colouring t :none)
             (is equal '(:none 3) (colouring bash))
             (is eq :none (tuatara.defaults::csv-colouring csv))
             ;; text in no mode of its own, as Markdown's inline text, goes by every buffer's
             (is equal '(:none 3) (colouring (tuatara.defaults::find-language 'markdown-inline)))
             (tuatara-user:set-colouring 'prog-mode :sparse :level 1)
             (is equal '(:sparse 1) (colouring bash))
             (is eq :none (tuatara.defaults::csv-colouring csv))
             (tuatara-user:set-colouring 'rust :rich)
             (is equal '(:rich 3) (colouring rust))
             ;; on the screen: Rust rich, a shell script sparse at level 1
             (let ((f (colour-session "" (format nil "fn main() {}~%") "rs")))
               (is equal '(0 2 :keyword) (first (aref (tuatara.frontend.fake:fake-spans f) 0))))
             (let ((f (colour-session "" (format nil "echo hi # note~%") "sh")))
               (is equal '((8 14 :comment)) (aref (tuatara.frontend.fake:fake-spans f) 0)))
             ;; rotate-colouring goes on from what the buffer shows, as the language's own
             (let ((f (colour-session "M-x r o t a t e - c o l o u r i n g RET" (format nil "echo hi~%") "sh")))
               (true (tuatara.frontend.fake:fake-frame-showing f "Bash colouring: none")))
             (is eq :none (tuatara.defaults::language-style bash))
             ;; a mode no language is coloured in is refused
             (fail (tuatara-user:set-colouring 'special-mode :sparse))
             (fail (tuatara-user:set-colouring 'calendar-mode :sparse))
             ;; Org, coloured by rules of its own, takes a style as a language does
             (is eq :sparse (tuatara-user:set-colouring 'org-mode :sparse))
             (is equal '(:sparse 3) (multiple-value-list (tuatara.defaults::mode-colouring (make-instance 'tuatara.defaults::org-mode)))))
        (unset)))))

(define-test (syntax c-language)
  ;; .c and .h files are C
  (is eq 'tuatara.defaults::c-mode (tuatara.modes:mode-for-file #p"/src/main.c"))
  (is eq 'tuatara.defaults::c-mode (tuatara.modes:mode-for-file #p"/src/main.h"))
  ;; coloured by the rules for C
  (let* ((f (colour-session "" (format nil "int main(void) {~%    return 0; // done~%}~%") "c"))
         (first-row (aref (tuatara.frontend.fake:fake-spans f) 0))
         (second-row (aref (tuatara.frontend.fake:fake-spans f) 1)))
    (flet ((face-at (spans start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (true (search "TYPE" (symbol-name (face-at first-row 0 3))))
      (is eq :function (face-at first-row 4 8))
      (true (search "KEYWORD" (symbol-name (face-at second-row 4 10))))
      (is eq :comment (face-at second-row 14 21))))
  ;; a parameter is coloured as one wherever the function uses it
  (let* ((f (colour-session "" (format nil "int f(int count) { return count + 1; }~%") "c"))
         (spans (aref (tuatara.frontend.fake:fake-spans f) 0)))
    (flet ((face-at (start end)
             (let ((face (third (find-if (lambda (s) (and (= (first s) start) (= (second s) end))) spans))))
               (if (listp face) (first face) face))))
      (is eq :variable.parameter (face-at 10 15))
      (is eq :variable.parameter (face-at 26 31))))
  ;; a function, a parameter and a type selected, and commented with //
  (flet ((region (keys text)
           (let (result)
             (with-file-session ((f editor) keys text "c")
               (setf result (session-region-text editor)))
             result))
         (after (keys text)
           (let (result)
             (with-file-session ((f editor) keys text "c")
               (setf result (session-text editor)))
             result)))
    (let ((c (format nil "int add(int a, int b) {~%    return a + b;~%}~%struct p { int x; };~%")))
      (is string= (format nil "{~%    return a + b;~%}") (region "j m f" c))
      (is string= "int b" (region "l l l l l l l l l l l l l l l l m r" c))
      (is string= "{ int x; }" (region "j j j l l l l l l l l l l m t" c))
      (is string= "struct p { int x; }" (region "j j j l l l l l l l l l l m T" c)))
    (is string= (format nil "// x = 1;~%") (after "C" (format nil "x = 1;~%")))
    ;; > indents four columns a level
    (is string= (format nil "int f() {~%    return 1;~%}~%") (after "j >" (format nil "int f() {~%return 1;~%}~%")))))

(define-test (syntax colours-far-into-a-large-text)
  ;; lines beyond the first 2 and 4 GiB of a text are coloured where they stand
  (let* ((filler (tuatara.text:string->rope
                  (with-output-to-string (s) (loop repeat (* 16 1024) do (format s "// ~60,,,'-a~%" "")))))
         (code (format nil "fn f(p: u8) -> u8 {~%    g(p, \"line\")~%}~%"))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0)))
    (dolist (copies (list (ceiling (* 5/2 (expt 2 30)) (tuatara.text:rope-bytes filler))
                          (ceiling (* 9/2 (expt 2 30)) (tuatara.text:rope-bytes filler))))
      (let* ((rope (apply #'tuatara.text:splice-rope (tuatara.text:make-rope) 0 0
                          (append (make-list copies :initial-element filler)
                                  (list (tuatara.text:string->rope code) filler))))
             (at (* copies (tuatara.text:rope-bytes filler)))
             (buffer (tuatara.buffer:make-buffer "*syntax-far*" :text rope)))
        (unwind-protect
             (let* ((syntax (tuatara.syntax:attach-syntax buffer grammar))
                    (shown (tuatara.syntax:syntax-highlights syntax query faces at (+ at (length code))))
                    (string (+ at (search "\"line\"" code))))
               (true (tuatara.syntax:syntax-too-big syntax))
               (true (find-if (lambda (range) (and (= string (first range)) (= (+ string 6) (second range)))) shown)
                     "the string at ~d is coloured" string)
               (true (every (lambda (range) (<= at (first range) (second range) (+ at (length code)))) shown)))
          (tuatara.syntax:detach-syntax buffer)
          (tuatara.buffer:kill-buffer buffer))))))

(define-test (syntax grammar-names-name-files-there)
  ;; a grammar's name never leads out of the grammar directories
  (let ((looked '()))
    (sb-int:encapsulate 'tuatara.syntax::find-grammar-file 'noting-looks
                        (lambda (function name) (push name looked) (funcall function name)))
    (unwind-protect
         (progn
           (false (tuatara.syntax:find-grammar "../grammars/rust"))
           (false (tuatara.syntax:find-grammar "/tmp/x"))
           (false (tuatara.syntax:find-grammar "Rust"))
           (false (find-if (lambda (name) (find #\/ name)) looked))
           (true (tuatara.syntax:find-grammar "rust"))
           (true (tuatara.syntax:find-grammar "c-sharp")))
      (sb-int:unencapsulate 'tuatara.syntax::find-grammar-file 'noting-looks))))

(define-test (syntax predicates-copy-no-long-text)
  ;; #any-of? on a long node and #eq? with a text make no copy of either
  (let* ((test-any (tuatara.syntax::parse-predicate (list "any-of?" 0 "fn" "let")))
         (test-eq (tuatara.syntax::parse-predicate (list "eq?" 0 "fn"))))
    (flet ((consed (test length)
             (let* ((rope (tuatara.text:string->rope (make-string length :initial-element #\a)))
                    (captures (make-array 4 :element-type 'fixnum :initial-contents (list 0 0 length 0))))
               (nth-value 2 (tuatara.bench::measuring
                              (loop repeat 100 do (tuatara.syntax::text-test-passes-p
                                                   (first test) 0 (third test) nil rope captures 1)))))))
      (true (< (consed test-any (* 1024 1024)) 4096) "#any-of? on a megabyte")
      (true (< (consed test-any (* 64 1024)) 4096) "#any-of? on 64 KB")
      (true (< (consed test-eq 2) 1024) "#eq? on two octets"))))

(define-test (syntax predicates-tested-without-copies)
  ;; the tests of a rule on its nodes' text read short text on the stack: matching the
  ;; rules of colour over a file makes next to nothing for each match
  (let* ((test-match (tuatara.syntax::parse-predicate (list "match?" 0 "^[A-Z]")))
         (test-any (tuatara.syntax::parse-predicate (list "any-of?" 0 "fn" "Kea")))
         (test-eq (tuatara.syntax::parse-predicate (list "eq?" 0 1)))
         (rope (tuatara.text:string->rope "Kea kea é Kea"))
         (captures (make-array 8 :element-type 'fixnum :initial-contents (list 0 0 3 0 1 11 14 0))))
    (flet ((passes (test) (tuatara.syntax::text-test-passes-p (first test) 0 (third test) nil rope captures 2)))
      (true (passes test-match))
      (true (passes test-any))
      (true (passes test-eq))
      (setf (aref captures 5) 4 (aref captures 6) 7)
      (false (passes test-eq))
      (is = 0 (nth-value 2 (tuatara.bench::measuring
                             (loop repeat 100 do (passes test-match) (passes test-any) (passes test-eq)))))))
  (let ((language (tuatara.defaults::find-language 'rust)))
    (when (and language (tuatara.defaults::language-compiled-query language :colouring))
      (let* ((query (tuatara.defaults::language-compiled-query language :colouring))
             (text (with-output-to-string (o)
                     (dotimes (i 300)
                       (format o "pub fn kea_~d(x: &str) -> Result<String, Error> {~%    if x == \"kea\" { return Ok(None); }~%    self.call(true, 'c')?~%}~%" i))))
             (rope (tuatara.text:string->rope text))
             (parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar (tuatara.defaults::language-grammar language))))
             (tree (tuatara.syntax:parse-rope parser rope))
             (matches 0))
        (unwind-protect
             (flet ((run () (tuatara.syntax:map-query-matches (lambda (p c n) (declare (ignore p c n)) (incf matches))
                                                              query tree rope 0 (length text))))
               (run)
               (let ((consed (loop repeat 3 minimize (nth-value 2 (tuatara.bench::measuring (run))))))
                 (true (< consed (* 16 1024)) "~:d matches consed ~:d octets" (floor matches 4) consed)))
          (release-parse tree parser))))))

(define-test (syntax colours-kept-for-several-ranges)
  ;; two parts of a buffer shown at once keep their colours, each worked out once
  (let* ((text (with-output-to-string (s) (dotimes (i 40) (format s "fn f~d() {}~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-kept*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0)))
    (unwind-protect
         (let* ((syntax (tuatara.syntax:attach-syntax buffer grammar))
                (tuatara.syntax:*syntax-wait* 30)
                (top (tuatara.syntax:syntax-highlights syntax query faces 0 100))
                (bottom (tuatara.syntax:syntax-highlights syntax query faces 300 400)))
           (true top)
           (true bottom)
           (is eq top (tuatara.syntax:syntax-highlights syntax query faces 0 100))
           (is eq bottom (tuatara.syntax:syntax-highlights syntax query faces 300 400)))
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax slow-lines-of-a-large-text-parsed-once)
  ;; a text too large to parse whole whose lines parse too slowly is not parsed again on every scroll
  (let* ((text (with-output-to-string (s) (dotimes (i 400) (format s "fn f~d(p: u8) -> u8 { g(p) }~%" i))))
         (buffer (tuatara.buffer:make-buffer "*syntax-slow-slice*" :text text))
         (grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar (tuatara.defaults::language-rules (language-named 'rust))))
         (faces (tuatara.defaults::query-faces query :rich 0))
         (parses 0))
    (sb-int:encapsulate 'tuatara.syntax::parse-ranges 'counting
                        (lambda (function &rest arguments) (incf parses) (apply function arguments)))
    (unwind-protect
         (let* ((tuatara.syntax:*syntax-size-limit* 1000)
                (tuatara.syntax::*injection-time-limit* 1d-9)
                (syntax (tuatara.syntax:attach-syntax buffer grammar)))
           (true (tuatara.syntax:syntax-too-big syntax))
           (false (tuatara.syntax:syntax-highlights syntax query faces 0 100))
           (is eq :slow (tuatara.syntax:syntax-broken syntax))
           (false (tuatara.syntax:syntax-highlights syntax query faces 3000 3100))
           (is = 1 parses))
      (sb-int:unencapsulate 'tuatara.syntax::parse-ranges 'counting)
      (tuatara.syntax:detach-syntax buffer)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax defaults-name-only-exported-syntax)
  ;; the limits, child ranges, group counts and bracket reach the defaults use are
  ;; exported, and no file of the defaults reaches into tuatara.syntax with two colons
  (dolist (name '("PATTERN-GROUP-COUNT" "+BRACKET-REACH+" "NODE-CHILD-RANGES" "PARSE-MEMORY-LIMIT" "PARSE-TIME-LIMIT"))
    (is eq :external (nth-value 1 (find-symbol name :tuatara.syntax)) name))
  (true (< 100 (length (source-files "lisp"))))
  (dolist (file (source-files "lisp"))
    (false (search "tuatara.syntax::" (uiop:read-file-string file)) "~a" (enough-namestring file))))
