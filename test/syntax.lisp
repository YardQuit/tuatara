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
         (parser (tuatara.syntax:make-parser g (tuatara.syntax::shared-helper)))
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
    ;; the buffers are killed with their folder: so are their programs
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
  (logior (aref octets at) (ash (aref octets (+ at 1)) 8) (ash (aref octets (+ at 2)) 16) (ash (aref octets (+ at 3)) 24)))

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
      (let* ((tuatara.syntax:*helper-ended-function* nil)
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
                      (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper) tuatara.syntax::**kept-helpers**))
                    (unwind-protect
                         (setf result (handler-case (tuatara.syntax::compile-query-text grammar source)
                                        (tuatara.syntax:query-error () :refused)))
                      (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                        (setf tuatara.syntax::**kept-helpers** (remove helper tuatara.syntax::**kept-helpers** :key #'cdr)))))
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
         (bytes (tuatara.text:rope-bytes rope))
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
                                 (true (<= 0 (tuatara.syntax:tree-node-start node) (tuatara.syntax:tree-node-end node) bytes)))
                               (multiple-value-bind (from to) (tuatara.syntax:select-node syntax 3 5 :larger)
                                 (when from (true (<= 0 from to bytes)))))
                          (setf (tuatara.syntax:syntax-tree syntax) old))))
                    ;; as the helper compiling rules, which grammars are loaded in
                    (lambda (helper tree)
                      (declare (ignore tree))
                      (let ((tuatara.syntax:*grammar-directories* (list "/nonexistent/fake-helper/")))
                        (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                          (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper) tuatara.syntax::**kept-helpers**))
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
         (let ((tuatara.syntax:*syntax-fault-function* (lambda (syntax) (push syntax said))))
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
  (let ((buffer (tuatara.buffer:make-buffer "*pasted*" :text (format nil "~{fn f~d() {}~%~}" (loop for i below 200 collect i))))
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
  (let ((before tuatara-user:*sandbox-program*))
    (setf tuatara-user:*sandbox-program* "/nonexistent/bwrap"
          tuatara.syntax::**refused-said** nil)
    (unwind-protect (funcall function)
      (setf tuatara-user:*sandbox-program* before
            tuatara.syntax::**refused-said** nil)
      (tuatara.io:sandbox-status :again t))))

(define-test (syntax no-sandbox-no-parsing)
  ;; without the sandbox, text outside the projects you trust is never parsed,
  ;; said once; the projects you trust only when *syntax-helper-unconfined* allows;
  ;; a sandbox that fails to start starts nothing in its place
  (let ((said '()))
    (call-without-sandbox
     (lambda ()
       (let ((tuatara.syntax:*helper-refused-function* (lambda (trusted) (push trusted said)))
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
         (tuatara.syntax:*syntax-helper-shared-function* (lambda (buffer) (declare (ignore buffer)) trusted))
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
      (remhash "misbehaving" tuatara.syntax::*grammars*)
      (remhash "misbehaving" tuatara.syntax::**grammar-faults**)
      (unwind-protect
           (let ((tuatara.syntax:*grammar-directories* (cons directory (tuatara.syntax:grammar-directories))))
             (funcall function (tuatara.syntax:find-grammar "misbehaving") directory))
        (remhash "misbehaving" tuatara.syntax::*grammars*)
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
         (let ((parser (tuatara.syntax:make-parser grammar (tuatara.syntax::shared-helper))))
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
            (let ((tuatara.syntax:*syntax-fault-function* (lambda (syntax) (setf said syntax))))
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
            (parser (tuatara.syntax:make-parser grammar (tuatara.syntax::shared-helper)))
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
         (tuatara.syntax:*syntax-slow-function* (lambda (syntax) (setf said syntax))))
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
     (let* ((parser (tuatara.syntax:make-parser grammar (tuatara.syntax::shared-helper)))
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
  (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json") (tuatara.syntax::shared-helper))))
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

(define-test (syntax lying-matches-cost-little)
  ;; a reply of more matches than the text asked about could have ends its
  ;; helper at once; matches whose tests would read the whole text over and
  ;; over fail those tests past what they may read, at once
  (let* ((grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar "((identifier) @name (#match? @name \"^x\"))"))
         (rope (tuatara.text:string->rope (make-string (* 4 1024 1024) :initial-element #\x)))
         (bytes (tuatara.text:rope-bytes rope)))
    (dolist (case (list (list 5000 10) (list 100 bytes)))
      (destructuring-bind (matches width) case
        (let ((reply (apply #'concatenate '(vector (unsigned-byte 8))
                            (loop repeat matches collect (words 0 1 0 0 width 0))))
              (seen 0) ended (began (get-internal-real-time)))
          (call-with-fake-helper
           (lambda (fd how body) (declare (ignore how body)) (fake-send fd 1 0 reply))
           (lambda (helper)
             (setf (gethash (tuatara.syntax::query-number query) (tuatara.syntax::helper-queries helper)) t)
             (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                                 (declare (ignore pattern captures count))
                                                 (incf seen))
                                               query (tuatara.syntax::%make-tree helper 1) rope 0 10)
             (setf ended (tuatara.syntax:helper-ended helper))))
          (if (= matches 5000)
              (is eq :broken ended "~s" case)
              (progn (false ended) (is = 0 seen)))
          (true (< seen matches))
          (true (< (tuatara.runtime:seconds-since began) 2)))))))

(define-test (syntax one-helper-for-a-buffer)
  ;; threads asking at once for a buffer's helper all get the one helper
  (let ((buffer (tuatara.buffer:make-buffer "*one-helper*" :text "fn main() {}"))
        (got '()) (lock (sb-thread:make-mutex)))
    (unwind-protect
         (let ((threads (loop repeat 4
                              collect (sb-thread:make-thread
                                       (lambda ()
                                         (let ((helper (tuatara.syntax:buffer-helper buffer)))
                                           (sb-thread:with-mutex (lock) (push helper got))))))))
           (mapc #'sb-thread:join-thread threads)
           (is = 4 (length got))
           (is = 1 (length (remove-duplicates got)))
           (is = 1 (count buffer tuatara.syntax::**helpers** :key #'tuatara.syntax::helper-buffer)))
      (tuatara.buffer:kill-buffer buffer))
    ;; killed, its helper ends, and none is started for it after
    (is = 0 (count buffer tuatara.syntax::**helpers** :key #'tuatara.syntax::helper-buffer))
    (false (tuatara.syntax:buffer-helper buffer))))

(define-test (syntax a-file-counts-once-however-often-opened)
  ;; a grammar going wrong on one file opened again and again is still one
  ;; text, not one more each time: no language is turned off for all by it
  (let ((grammar (tuatara.syntax:find-grammar "rust"))
        (buffers '()))
    (clrhash tuatara.syntax::**grammar-faults**)
    (unwind-protect
         (progn
           (dotimes (i 4)
             (let ((buffer (tuatara.buffer:make-buffer (format nil "*same-file-~d*" i) :text "fn main() {}")))
               (setf (tuatara.buffer:buffer-file buffer) "/nonexistent/evidence/same.rs")
               (push buffer buffers)
               (tuatara.syntax::grammar-went-wrong grammar buffer :crashed)))
           (false (tuatara.syntax:grammar-faulted grammar))
           (true (every (lambda (buffer) (tuatara.syntax::grammar-off-p grammar buffer)) buffers)))
      (mapc #'tuatara.buffer:kill-buffer buffers)
      (clrhash tuatara.syntax::**grammar-faults**)
      (setf (tuatara.syntax:grammar-faulted grammar) nil))))

(define-test (syntax one-kept-helper-of-a-kind)
  ;; threads asking at once for the helper kept for all start one of it
  (with-test-directory (directory)
    (let* ((directories (cons (namestring directory) (tuatara.syntax:grammar-directories)))
           (got '()) (lock (sb-thread:make-mutex)))
      (unwind-protect
           (let ((threads (loop repeat 4
                                collect (sb-thread:make-thread
                                         (lambda ()
                                           (let* ((tuatara.syntax:*grammar-directories* directories)
                                                  (helper (tuatara.syntax:shared-helper)))
                                             (sb-thread:with-mutex (lock) (push helper got))))))))
             (mapc #'sb-thread:join-thread threads)
             (is = 1 (length (remove-duplicates got)))
             (is = 1 (count-if (lambda (helper) (and (eq :shared (tuatara.syntax:helper-kind helper))
                                                     (equal directories (tuatara.syntax::helper-directories helper))))
                               tuatara.syntax::**helpers**)))
        (dolist (helper (copy-list tuatara.syntax::**helpers**))
          (when (equal directories (tuatara.syntax::helper-directories helper))
            (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax editor-thread-never-waits-for-a-start)
  ;; on a thread that must not wait, a helper kept for all that is not
  ;; running is started in the background, NIL given at once meanwhile
  (with-test-directory (directory)
    (let ((directories (cons (namestring directory) (tuatara.syntax:grammar-directories))))
      (unwind-protect
           (let ((tuatara.syntax:*grammar-directories* directories))
             (let ((tuatara.syntax:*must-not-wait-function* (lambda () t))
                   (asking sb-thread:*current-thread*)
                   (started-by '()))
               ;; where each start is made: never on the thread that asked
               (sb-int:encapsulate 'tuatara.syntax::start-helper 'note-thread
                                   (lambda (function &rest arguments)
                                     (push sb-thread:*current-thread* started-by)
                                     (apply function arguments)))
               (unwind-protect
                    (progn
                      (false (tuatara.syntax:rules-helper))
                      (true (loop repeat 500
                                  thereis (tuatara.syntax::known-kept-helper (cons :rules directories))
                                  do (sleep 0.01)))
                      (true started-by)
                      (false (member asking started-by)))
                 (sb-int:unencapsulate 'tuatara.syntax::start-helper 'note-thread))))
        (dolist (helper (copy-list tuatara.syntax::**helpers**))
          (when (equal directories (tuatara.syntax::helper-directories helper))
            (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax starting-thread-never-waited-for-forever)
  ;; a start that ends with a condition no error, or takes too long, is
  ;; given up with that said, and the next start goes on as before
  (multiple-value-bind (result condition) (tuatara.syntax::in-starter (lambda () (error 'storage-condition)))
    (false result)
    (true (typep condition 'storage-condition)))
  (let ((tuatara.syntax::*starter-wait* 0.5)
        (began (get-internal-real-time)))
    (multiple-value-bind (result condition) (tuatara.syntax::in-starter (lambda () (sleep 3) :late))
      (false result)
      (true condition)
      (true (< (tuatara.runtime:seconds-since began) 2))))
  (is eq :fine (tuatara.syntax::in-starter (lambda () :fine))))

(define-test (syntax a-link-lends-no-trust)
  ;; a file reached through a link inside a project you trust is parsed as
  ;; the file it really is, outside it
  (with-test-directory (trusted)
    (with-test-directory (elsewhere)
      (let ((tuatara-user:*lsp-trusted-projects* (list (namestring trusted)))
            (inside (namestring (merge-pathnames "inside.rs" trusted)))
            (outside (namestring (merge-pathnames "evidence.rs" elsewhere)))
            (link (namestring (merge-pathnames "link.rs" trusted))))
        (ensure-directories-exist (merge-pathnames ".git/" trusted))
        (dolist (path (list inside outside)) (with-open-file (o path :direction :output) (write-line "fn f() {}" o)))
        (sb-posix:symlink outside link)
        (flet ((trusted-p (path)
                 (let ((buffer (tuatara.buffer:make-buffer (format nil "*trust ~a*" path) :text "")))
                   (unwind-protect
                        (progn (setf (tuatara.buffer:buffer-file buffer) path)
                               (tuatara.defaults::buffer-in-trusted-project-p buffer))
                     (tuatara.buffer:kill-buffer buffer)))))
          (true (trusted-p inside))
          (false (trusted-p link)))))))

(define-test (syntax counts-checked-before-anything-made)
  ;; a reply to compiling rules naming more captures than the rules have,
  ;; or a grammar of more node types than tree-sitter can number, is
  ;; refused before anything is made of it
  (let ((grammar (tuatara.syntax:find-grammar "rust")))
    (dolist (case (list (list :rules (words 0 #x7ffffff)) (list :grammar (words 1 #x7ffffff))))
      (destructuring-bind (kind reply) case
        (let (ended (before (sb-ext:get-bytes-consed)))
          (call-with-fake-helper
           (lambda (fd how body) (declare (ignore how body)) (fake-send fd 1 0 reply))
           (lambda (helper)
             (let ((tuatara.syntax:*grammar-directories* (list "/nonexistent/fake-counts/")))
               (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                 (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper) tuatara.syntax::**kept-helpers**))
               (unwind-protect
                    (handler-case (if (eq kind :rules)
                                      (tuatara.syntax::compile-query-text grammar "(identifier) @name")
                                      (tuatara.syntax::load-grammar "fake"))
                      (tuatara.syntax:query-error () nil))
                 (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
                   (setf tuatara.syntax::**kept-helpers** (remove helper tuatara.syntax::**kept-helpers** :key #'cdr)))))
             (setf ended (tuatara.syntax:helper-ended helper))))
          (is eq :broken ended "~s" kind)
          (true (< (- (sb-ext:get-bytes-consed) before) (* 64 1024 1024)) "~s" kind))))))

(define-test (syntax reset-refused-while-parsing)
  ;; a parser is not reset, nor deleted under it, while it parses on
  ;; another connection
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((tuatara.syntax:*syntax-helper-wait* 1))
       (call-with-own-helper
        (lambda (helper)
          (let* ((parser (tuatara.syntax:make-parser grammar helper))
                 (parsing (sb-thread:make-thread
                           (lambda ()
                             (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !l def") :limit 2)))))
            (sleep 0.3)
            (tuatara.syntax::with-call (call helper tuatara.syntax::+op-reset+)
              (tuatara.syntax::call-u32 call (tuatara.syntax::parser-number parser))
              (is eql 1 (tuatara.syntax::call-send call)))
            (sb-thread:join-thread parsing :default nil))))))))

(define-test (syntax helpers-in-use-not-ended-for-room)
  ;; past *syntax-helpers-most*, a helper whose buffer is shown or parsing
  ;; is left running, the helpers going past the most
  (let ((buffers (loop for i below 3 collect (tuatara.buffer:make-buffer (format nil "*in-use-~d*" i) :text "fn f() {}"))))
    (unwind-protect
         (let ((tuatara.syntax::*syntax-helpers-most* 1)
               (tuatara.syntax:*helper-in-use-function* (lambda (buffer) (member buffer buffers))))
           (let ((helpers (mapcar #'tuatara.syntax:buffer-helper buffers)))
             (true (every #'tuatara.syntax:helper-alive helpers))))
      (mapc #'tuatara.buffer:kill-buffer buffers))))

(define-test (syntax no-sandbox-nothing-pending)
  ;; without the sandbox a grammar not yet loaded is not waited for: no
  ;; helper is started again and again, no thread made for each redraw
  (let ((starts 0) (kept (gethash "toml" tuatara.syntax::*grammars*)))
    (call-without-sandbox
     (lambda ()
       (tuatara.io:sandbox-status :again t)
       (sb-int:encapsulate 'tuatara.syntax::start-helper 'counted
                           (lambda (function &rest arguments) (incf starts) (apply function arguments)))
       (remhash "toml" tuatara.syntax::*grammars*)
       (let ((buffer (tuatara.buffer:make-buffer "*pending.toml*" :text "a = 1"))
             (tuatara.syntax::*syntax-helper-unconfined* nil)
             (tuatara.syntax:*must-not-wait-function* (lambda () t)))
         (unwind-protect
              (let ((threads (length (sb-thread:list-all-threads))))
                (tuatara.modes:set-major-mode buffer 'tuatara.defaults::toml-mode)
                (dotimes (i 20) (tuatara.defaults::syntax-refresh-tick nil))
                (false (tuatara.syntax:grammar-pending-p "toml"))
                (false (tuatara.buffer:buffer-local buffer :syntax-pending))
                (is = 0 starts)
                (true (<= (length (sb-thread:list-all-threads)) (+ threads 1))))
           (sb-int:unencapsulate 'tuatara.syntax::start-helper 'counted)
           (tuatara.buffer:kill-buffer buffer)))))
    (if kept (setf (gethash "toml" tuatara.syntax::*grammars*) kept) (remhash "toml" tuatara.syntax::*grammars*))))

(define-test (syntax grammar-ending-its-loader-given-up)
  ;; a grammar whose loading ends the program loading it is tried a few
  ;; times, then given up on as missing: no helper started without end
  (with-test-directory (directory)
    (let ((source (merge-pathnames "crashy.c" directory))
          (library (merge-pathnames "crashy.so" directory))
          (rules 0))
      (with-open-file (o source :direction :output)
        (format o "#include <stdlib.h>~%const void *tree_sitter_crashy(void) { abort(); }~%"))
      (multiple-value-bind (output error code)
          (uiop:run-program (list "cc" "-shared" "-fPIC" "-o" (namestring library) (namestring source))
                            :output :string :error-output :output :ignore-error-status t)
        (declare (ignore error))
        (is = 0 code "~a" output))
      (sb-int:encapsulate 'tuatara.syntax::start-helper 'counted
                          (lambda (function directories kind)
                            (when (eq kind :rules) (incf rules))
                            (funcall function directories kind)))
      (unwind-protect
           (let ((tuatara.syntax:*grammar-directories* (cons (namestring directory) (tuatara.syntax:grammar-directories))))
             (remhash "crashy" tuatara.syntax::**grammar-load-deaths**)
             (dotimes (i 10) (false (tuatara.syntax:find-grammar "crashy")))
             (true (<= rules (1+ tuatara.syntax::*grammar-load-deaths-most*)) "~d starts" rules)
             (false (tuatara.syntax:grammar-pending-p "crashy")))
        (sb-int:unencapsulate 'tuatara.syntax::start-helper 'counted)
        (remhash "crashy" tuatara.syntax::*grammars*)
        (remhash "crashy" tuatara.syntax::**grammar-load-deaths**)
        (dolist (helper (copy-list tuatara.syntax::**helpers**))
          (when (member (namestring directory) (tuatara.syntax::helper-directories helper) :test #'equal)
            (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax reply-too-large-refused-cleanly)
  ;; a helper whose reply would be past its most refuses it whole, writes
  ;; nothing past it, and goes on; built here with a small most
  (with-test-directory (directory)
    (let* ((ts (namestring (asdf:system-relative-pathname :tuatara "build/grammar-sources/tree-sitter/")))
           (root (namestring (asdf:system-relative-pathname :tuatara "")))
           (program (namestring (merge-pathnames "tuatara-syntax-helper" directory))))
      (multiple-value-bind (output error code)
          (uiop:run-program (list "cc" "-O1" "-std=c11" "-DNDEBUG" "-D_POSIX_C_SOURCE=200112L" "-D_DEFAULT_SOURCE"
                                  "-DREPLY_MOST=16384u"
                                  (format nil "-I~alib/include" ts) (format nil "-I~alib/src" ts)
                                  (format nil "~alib/src/lib.c" ts) (format nil "~asyntax/syntax.c" root)
                                  (format nil "~asyntax/helper.c" root)
                                  "-rdynamic" "-lpthread" "-ldl" "-o" program)
                            :output :string :error-output :output :ignore-error-status t)
        (declare (ignore error))
        (is = 0 code "~a" output))
      (let* ((tuatara.syntax:*grammar-directories* (cons (namestring directory) (tuatara.syntax:grammar-directories)))
             (helper (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own))
             (grammar (tuatara.syntax:find-grammar "rust"))
             (query (tuatara.syntax:compile-query grammar "(identifier) @name"))
             (rope (tuatara.text:string->rope (format nil "~{fn f~d(a: i32, b: i32) -> i32 { a + b }~%~}" (loop for i below 400 collect i)))))
        (unwind-protect
             (let* ((parser (tuatara.syntax:make-parser grammar helper))
                    (tree (tuatara.syntax:parse-rope parser rope))
                    (seen 0) (began (get-internal-real-time)))
               (true tree)
               (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                                   (declare (ignore pattern captures count))
                                                   (incf seen))
                                                 query tree rope 0 (tuatara.text:rope-bytes rope))
               (is = 0 seen)
               (true (< (tuatara.runtime:seconds-since began) 1))
               (true (tuatara.syntax:helper-alive helper))
               ;; and a small one is answered as before
               (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                                   (declare (ignore pattern captures count))
                                                   (incf seen))
                                                 query tree rope 0 40)
               (true (plusp seen))
               (true (tuatara.syntax:helper-alive helper))
               ;; text in another language whose matches are too many: its trees let go
               (let ((held (tuatara.syntax:helper-memory helper)))
                 (dotimes (i 5)
                   (tuatara.syntax::parse-texts rope (list (tuatara.syntax::make-text-parse
                                                            grammar (list (cons 0 (tuatara.text:rope-bytes rope))) nil
                                                            (list (list query 0 (tuatara.text:rope-bytes rope))) 2 nil))
                                                (lambda (trees) (declare (ignore trees)))
                                                :helper helper))
                 (true (< (tuatara.syntax:helper-memory helper) (+ held (* 64 1024))) "~d held, ~d before"
                       (tuatara.syntax:helper-memory helper) held)))
          (when helper (tuatara.syntax::stop-helper helper)))))))

(define-test (syntax given-up-starting-thread-takes-no-more)
  ;; a starting thread given up on takes no more work, its late answer
  ;; taken for no other start, and every starting thread ends with the rest
  (let ((tuatara.syntax::*starter-wait* 0.3))
    (multiple-value-bind (result condition) (tuatara.syntax::in-starter (lambda () (sleep 1) :old))
      (false result)
      (true condition)))
  (is eq :new (tuatara.syntax::in-starter (lambda () :new)))
  (sleep 1)
  (is eq :newer (tuatara.syntax::in-starter (lambda () :newer)))
  (true (>= (length tuatara.syntax::**starters**) 2))
  (let ((starters (copy-list tuatara.syntax::**starters**)))
    (tuatara.syntax:stop-helpers)
    (true (notany #'sb-thread:thread-alive-p starters))))

(define-test (syntax showing-text-never-waits-for-its-helper)
  ;; on the editor's own thread, a buffer's helper is set going at once when
  ;; nothing holds it up; while another start does, none is given at once,
  ;; its helper started in the background
  (tuatara.io:sandbox-status)
  (let ((buffer (tuatara.buffer:make-buffer "*soon*" :text "fn main() {}"))
        (holding (sb-thread:make-semaphore)))
    (unwind-protect
         (let* ((tuatara.syntax:*must-not-wait-function* (lambda () t))
               (letting-go (sb-thread:make-semaphore))
               (given-up nil)
               (holder (sb-thread:make-thread
                        (lambda ()
                          (sb-thread:with-mutex (tuatara.syntax::**room-lock**)
                            (sb-thread:signal-semaphore holding)
                            (unless (sb-thread:wait-on-semaphore letting-go :timeout 10)
                              (setf given-up t)))))))
           (sb-thread:wait-on-semaphore holding)
           (false (tuatara.syntax:buffer-helper-soon buffer))
           ;; given up on while the start was still held up, not after waiting for it
           (false given-up)
           (sb-thread:signal-semaphore letting-go)
           (sb-thread:join-thread holder)
           (true (loop repeat 500
                       thereis (let ((own (tuatara.buffer:buffer-local buffer :syntax-helper)))
                                 (and own (tuatara.syntax:helper-alive own)))
                       do (sleep 0.01))))
      (tuatara.buffer:kill-buffer buffer)))
  ;; nothing holding it up: set going at once
  (let ((buffer (tuatara.buffer:make-buffer "*soon-now*" :text "fn main() {}")))
    (unwind-protect
         (let ((tuatara.syntax:*must-not-wait-function* (lambda () t))
               (tuatara.syntax::*at-once-wait* 30))
           (true (tuatara.syntax:buffer-helper-soon buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax buffer-killed-while-its-helper-starts)
  ;; a buffer killed while its helper starts leaves none behind
  (let ((buffer (tuatara.buffer:make-buffer "*killed-starting*" :text "fn main() {}")))
    (let ((killer nil))
      ;; killed from another thread while the start is under way
      (sb-int:encapsulate 'tuatara.syntax::start-helper 'kill-meanwhile
                          (lambda (function &rest arguments)
                            (setf killer (sb-thread:make-thread (lambda () (tuatara.buffer:kill-buffer buffer))))
                            (sleep 0.2)
                            (apply function arguments)))
      (unwind-protect
           (tuatara.syntax:buffer-helper buffer)
        (sb-int:unencapsulate 'tuatara.syntax::start-helper 'kill-meanwhile)
        (when killer (sb-thread:join-thread killer :default nil :timeout 10))))
    (false (find-if (lambda (helper) (and (eq buffer (tuatara.syntax::helper-buffer helper))
                                          (tuatara.syntax:helper-alive helper)))
                    tuatara.syntax::**helpers**))))

(define-test (syntax honest-helpers-not-blamed)
  ;; tests of a long captured text past what they may read do not pass, the
  ;; helper left alone; a whole text's matches taking long are slow, not a
  ;; fault of the buffer's text; reading it again tries its grammars again
  (let* ((grammar (tuatara.syntax:find-grammar "rust"))
         (query (tuatara.syntax:compile-query grammar "((string_literal) @s (#match? @s \"x\"))"))
         (text (format nil "fn f() { let s = \"~a\"; }~%" (make-string (* 3 1024 1024) :initial-element #\x)))
         (rope (tuatara.text:string->rope text)))
    (call-with-own-helper
     (lambda (helper)
       (let* ((parser (tuatara.syntax:make-parser grammar helper))
              (tree (tuatara.syntax:parse-rope parser rope))
              (seen 0))
         (true tree)
         (tuatara.syntax:map-query-matches (lambda (pattern captures count)
                                             (declare (ignore pattern captures count))
                                             (incf seen))
                                           query tree rope 0 20)
         (is = 0 seen)
         (true (tuatara.syntax:helper-alive helper))))))
  (let ((buffer (tuatara.buffer:make-buffer "*slow-whole*" :text "fn main() {}"))
        (tuatara.syntax:*syntax-helper-wait* 0.5))
    (unwind-protect
         (progn
           (call-with-fake-helper
            :never
            (lambda (helper)
              (setf (tuatara.syntax::helper-buffer helper) buffer)
              (let ((query (tuatara.syntax:compile-query (tuatara.syntax:find-grammar "rust") "(identifier) @x")))
                (setf (gethash (tuatara.syntax::query-number query) (tuatara.syntax::helper-queries helper)) t)
                (tuatara.syntax:map-query-matches (lambda (&rest r) (declare (ignore r)))
                                                  query (tuatara.syntax::%make-tree helper 1)
                                                  (tuatara.buffer:buffer-text buffer) 0 12))
              (is eq :slow (tuatara.syntax:helper-ended helper))))
           (false (tuatara.syntax::grammar-off-p (tuatara.syntax:find-grammar "rust") buffer))
           (tuatara.syntax::note-faulted buffer :all)
           (true (tuatara.syntax::grammar-off-p (tuatara.syntax:find-grammar "rust") buffer))
           (tuatara.defaults::parse-afresh buffer)
           (false (tuatara.syntax::grammar-off-p (tuatara.syntax:find-grammar "rust") buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax a-text-counted-once-whatever-it-is-called)
  ;; one file reached by a link and by its own path, one text pasted twice,
  ;; one note exported again and again: each one text
  (with-test-directory (directory)
    (let* ((real (namestring (merge-pathnames "real.rs" directory)))
           (link (namestring (merge-pathnames "link.rs" directory))))
      (with-open-file (o real :direction :output) (write-line "fn f() {}" o))
      (sb-posix:symlink real link)
      (is equal (tuatara.syntax:text-key :file real) (tuatara.syntax:text-key :file link))
      (let ((a (tuatara.buffer:make-buffer "*pasted-a*" :text "fn g() {}"))
            (b (tuatara.buffer:make-buffer "*pasted-b*" :text "fn g() {}"))
            (c (tuatara.buffer:make-buffer "*pasted-c*" :text "fn h() {}")))
        (unwind-protect
             (progn
               (is equal (tuatara.syntax::text-identity a) (tuatara.syntax::text-identity b))
               (false (equal (tuatara.syntax::text-identity a) (tuatara.syntax::text-identity c))))
          (mapc #'tuatara.buffer:kill-buffer (list a b c))))
      (is equal (tuatara.syntax:text-key :file nil :text "* a note") (tuatara.syntax:text-key :text "* a note")))))

(define-test (syntax replies-held-to-what-was-asked)
  ;; a reply to a parse longer than a parse's reply can be ends its helper
  (let (ended)
    (call-with-fake-helper
     (lambda (fd how body) (declare (ignore how body)) (fake-send fd 1 0 (apply #'words (loop repeat 25 collect 1))))
     (lambda (helper)
       (tuatara.syntax:parse-rope (tuatara.syntax::%make-parser helper 5 (tuatara.syntax:find-grammar "rust"))
                                  (tuatara.text:string->rope "fn f() {}"))
       (setf ended (tuatara.syntax:helper-ended helper))))
    (is eq :broken ended)))

(define-test (syntax grammar-unanswered-asked-again)
  ;; a grammar asked for while no connection to the helper loading grammars
  ;; can be had is asked for again later, not taken as missing
  (let ((kept (gethash "toml" tuatara.syntax::*grammars*)))
    (remhash "toml" tuatara.syntax::*grammars*)
    (unwind-protect
         (progn
           (tuatara.syntax:rules-helper)
           (sb-int:encapsulate 'tuatara.syntax::take-call 'none (lambda (function helper) (declare (ignore function helper)) nil))
           (unwind-protect (false (tuatara.syntax:find-grammar "toml"))
             (sb-int:unencapsulate 'tuatara.syntax::take-call 'none))
           (false (gethash "toml" tuatara.syntax::*grammars*))
           (true (tuatara.syntax:find-grammar "toml")))
      (if kept (setf (gethash "toml" tuatara.syntax::*grammars*) kept) (remhash "toml" tuatara.syntax::*grammars*)))))

(define-test (syntax slow-grammar-load-not-counted)
  ;; a grammar slow to load is given up on this time, not counted against it
  ;; nor taken as missing
  (remhash "slowly" tuatara.syntax::**grammar-load-deaths**)
  (call-with-fake-helper
   :never
   (lambda (helper)
     (let ((tuatara.syntax:*grammar-directories* (list "/nonexistent/fake-slow/"))
           (tuatara.syntax::*grammar-load-wait* 0.2))
       (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
         (push (cons (cons :rules (tuatara.syntax:grammar-directories)) helper) tuatara.syntax::**kept-helpers**))
       (unwind-protect
            (is eq :unknown (tuatara.syntax::load-grammar "slowly"))
         (sb-thread:with-mutex (tuatara.syntax::**helpers-lock**)
           (setf tuatara.syntax::**kept-helpers** (remove helper tuatara.syntax::**kept-helpers** :key #'cdr)))))
     (is eq :slow (tuatara.syntax:helper-ended helper))))
  (false (gethash "slowly" tuatara.syntax::**grammar-load-deaths**)))

(define-test (syntax start-at-once-never-waits)
  ;; on the editor's own thread a start that would wait (another under way,
  ;; or the program slow to set going) is left to the background at once,
  ;; no helper ended for room by it
  (tuatara.io:sandbox-status)
  (let ((other (tuatara.buffer:make-buffer "*room-other*" :text "fn f() {}"))
        (buffer (tuatara.buffer:make-buffer "*at-once*" :text "fn main() {}")))
    (unwind-protect
         (let ((tuatara.syntax::*syntax-helpers-most* 1)
               (tuatara.syntax:*must-not-wait-function* (lambda () t))
               (tuatara.syntax::*at-once-wait* 0.05)
               (holding (sb-thread:make-semaphore)))
           (let ((first (tuatara.syntax:buffer-helper other))
                 (holder (sb-thread:make-thread
                          (lambda ()
                            (sb-thread:with-mutex (tuatara.syntax::**starting**)
                              (sb-thread:signal-semaphore holding)
                              (sleep 0.5))))))
             (sb-thread:wait-on-semaphore holding)
             ;; the start busy: given up on at once, nothing ended for room
             (let ((began (get-internal-real-time)))
               (is eq :busy (tuatara.syntax::start-own-helper :buffer buffer :at-once t))
               (true (< (tuatara.runtime:seconds-since began) 0.05)))
             (true (tuatara.syntax:helper-alive first))
             (sb-thread:join-thread holder))
           ;; set going slowly: given up on at once, started in the background
           (tuatara.syntax:forget-buffer-helper buffer)
           (sb-int:encapsulate 'tuatara.io:sandbox-start 'slow (lambda (function &rest arguments)
                                                                (sleep 0.3)
                                                                (apply function arguments)))
           (unwind-protect
                (let ((began (get-internal-real-time)))
                  (false (tuatara.syntax:buffer-helper-soon buffer))
                  (true (< (tuatara.runtime:seconds-since began) 0.2)))
             (sb-int:unencapsulate 'tuatara.io:sandbox-start 'slow))
           (true (loop repeat 500
                       thereis (let ((own (tuatara.buffer:buffer-local buffer :syntax-helper)))
                                 (and own (tuatara.syntax:helper-alive own)))
                       do (sleep 0.01)))
           (sleep 0.5)
           (is = 1 (count-if (lambda (helper) (and (eq buffer (tuatara.syntax::helper-buffer helper))
                                                   (tuatara.syntax:helper-alive helper)))
                             tuatara.syntax::**helpers**)))
      (tuatara.buffer:kill-buffer buffer)
      (tuatara.buffer:kill-buffer other))))

(define-test (syntax start-given-up-closes-what-it-held)
  ;; a start given up on before it was ever taken closes the helper's end of
  ;; its socket: no descriptor left open by it
  (tuatara.io:sandbox-status)
  (flet ((open-descriptors () (length (directory "/proc/self/fd/*" :resolve-symlinks nil))))
    (let ((before (open-descriptors)))
      (let ((tuatara.syntax::*at-once-wait* 0))
        (dotimes (i 5)
          (let ((helper (tuatara.syntax::start-helper (tuatara.syntax:grammar-directories) :own :at-once t)))
            (when (tuatara.syntax:helper-p helper) (tuatara.syntax::stop-helper helper)))))
      (sleep 1)
      (true (<= (open-descriptors) before) "~d open, ~d before" (open-descriptors) before))))

(define-test (syntax revert-forgets-faults-once-it-reads-again)
  ;; reading a text again forgets what went wrong in it, with the export's
  ;; count of its file too; answering no to reading it again forgets nothing
  (with-test-directory (directory)
    (let ((path (namestring (merge-pathnames "note.rs" directory)))
          (answer nil) (buffer nil) (kept-after-no nil))
      (with-open-file (o path :direction :output) (write-line "fn f() {}" o))
      (sb-int:encapsulate 'tuatara.command:yes-or-no-p* 'answer (lambda (function prompt) (declare (ignore function prompt)) answer))
      (unwind-protect
           (run-keys (list (lambda ()
                             (setf buffer (tuatara.command:current-buffer))
                             (tuatara.syntax::note-faulted buffer :all)
                             (tuatara.syntax::note-faulted (tuatara.syntax:text-key :file path) "rust")
                             (tuatara.buffer:buffer-insert buffer 0 " "))
                           "M-x r e v e r t - b u f f e r RET"
                           (lambda ()
                             (setf kept-after-no (and (tuatara.syntax::faulted-names buffer) t)
                                   answer t))
                           "M-x r e v e r t - b u f f e r RET")
                     :file path)
        (sb-int:unencapsulate 'tuatara.command:yes-or-no-p* 'answer))
      (true kept-after-no)
      (false (tuatara.syntax::faulted-names buffer))
      (false (tuatara.syntax::faulted-names (tuatara.syntax:text-key :file path))))))

(define-test (syntax spent-text-budget-keeps-tests-that-read-nothing)
  ;; once the text matches' tests may read is spent, a test that reads none
  ;; still passes as it would: a not-equal of texts of other lengths
  (let ((rope (tuatara.text:string->rope "abc"))
        (captures (make-array 4 :element-type 'fixnum :initial-contents '(0 0 3 0)))
        (tuatara.syntax::*capture-text-left* -1))
    (true (tuatara.syntax::text-test-passes-p :eq 0 (tuatara.text:string-octets "longer text") t rope captures 1))))

(define-test (syntax looping-scanner-ended-at-its-deadline)
  ;; a scanner that never returns ("!l") cannot be stopped from inside: the
  ;; program parsing is ended when its time is up, and the editor carries on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((tuatara.syntax:*syntax-helper-wait* 1))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper))
                (began (get-internal-real-time)))
            (multiple-value-bind (tree why) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !l def")
                                                                       :limit 0.5)
              (false tree)
              (is eq :hung why)
              (is eq :hung (tuatara.syntax:helper-ended helper))
              (true (< (tuatara.runtime:seconds-since began) 5))
              (false (loop repeat 200
                           while (sb-ext:process-alive-p (tuatara.syntax::helper-process helper))
                           do (sleep 0.01)
                           finally (return (sb-ext:process-alive-p (tuatara.syntax::helper-process helper)))))
              (true (parses-elsewhere))))))))))

(define-test (syntax runaway-scanner-held-to-its-memory)
  ;; a scanner taking address space without end ("!r") gets no more than
  ;; the program parsing is held to, and the editor carries on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (let ((tuatara.syntax:*syntax-helper-memory* (* 512 1024 1024))
           (editor-before (sb-kernel:dynamic-usage)))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper)))
            (multiple-value-bind (tree why) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !r def"))
              (declare (ignore why))
              (when (eq :ready (tuatara.io:sandbox-status-now))
                ;; held to its memory: what it may still take is less than one more step
                (multiple-value-bind (held back left) (tuatara.syntax:helper-memory helper)
                  (declare (ignore held back))
                  (true (and left (<= 0 left (* 64 1024 1024))) "~s left" left)))
              (when tree (tuatara.syntax:delete-tree tree))))))
       (true (< (- (sb-kernel:dynamic-usage) editor-before) (* 256 1024 1024)))
       (true (parses-elsewhere))))))

(define-test (syntax long-read-back-given-up-as-slow)
  ;; a parse reading its text many times over is given no more and given up
  ;; as too slow: no fault of the grammar, its helper left running
  (let ((tuatara.syntax::*read-back-times* 0)
        (tuatara.syntax::*read-back-more* 1000)
        (tuatara.syntax::*given-text-most* 0)
        (grammar (tuatara.syntax:find-grammar "rust")))
    (call-with-own-helper
     (lambda (helper)
       (let ((parser (tuatara.syntax:make-parser grammar helper)))
         (multiple-value-bind (tree why)
             (tuatara.syntax:parse-rope parser (tuatara.text:string->rope
                                                (format nil "~{fn f~d() { (\"[<'\") }~%~}" (loop for i below 3000 collect i))))
           (false tree)
           (is eq :slow why)
           (true (tuatara.syntax:helper-alive helper))
           (false (tuatara.syntax::grammar-off-p grammar helper))))
       ;; text in another language read many times over (asked for, not sent with the request): the same
       (multiple-value-bind (tree why)
           (tuatara.syntax::parse-rope-ranges grammar (tuatara.text:string->rope
                                                       (format nil "~{fn g~d() {}~%~}" (loop for i below 3000 collect i)))
                                              (list (cons 0 30000)) :helper helper)
         (false tree)
         (is eq :slow why (format nil "~s" (tuatara.syntax:helper-ended helper)))
         (true (tuatara.syntax:helper-alive helper)))))))

(define-test (syntax scanner-cannot-fill-memory-files)
  ;; a scanner writing into /dev/shm, memory no limit counts, is ended by
  ;; the limit on files it may write (none): the editor carries on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (when (eq :ready (tuatara.io:sandbox-status))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper)))
            (multiple-value-bind (tree why)
                (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "abc !w/dev/shm/tuatara-shm-probe def"))
              (false tree)
              (is eq :crashed why)
              (false (tuatara.syntax:helper-alive helper))))))
       (true (parses-elsewhere))))))

(define-test (syntax no-helper-unless-named)
  ;; text parsed with no helper named is not parsed, never sent to the
  ;; shared one; Org's source blocks are parsed in their buffer's own
  (let ((tuatara.syntax:*helper* :none))
    (false (tuatara.syntax::current-helper))
    (false (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "rust")))
    (false (tuatara.syntax:language-highlights (tuatara.defaults::injection-resolver :rich 3) "rust"
                                               (list (cons 0 12)) (tuatara.text:string->rope "fn main() {}") 0 12)))
  (is eq :none (symbol-value 'tuatara.syntax:*helper*))
  (let* ((text (format nil "#+begin_src rust~%fn main() {}~%#+end_src~%"))
         (buffer (tuatara.buffer:make-buffer "*org-blocks*" :text text))
         (helpers '()))
    (sb-int:encapsulate 'tuatara.syntax::parse-texts 'which
                        (lambda (function &rest arguments)
                          (push (tuatara.syntax::current-helper) helpers)
                          (apply function arguments)))
    (unwind-protect
         (progn
           ;; its own helper started, then the blocks coloured
           (true (tuatara.syntax:buffer-helper buffer))
           (true (tuatara.defaults::org-src-highlights buffer (tuatara.buffer:buffer-text buffer)
                                                       (list (list "rust" 17 30)) 0 (length text)))
           (true helpers)
           (true (every (lambda (helper) (and helper (eq :own (tuatara.syntax:helper-kind helper))
                                              (eq helper (tuatara.syntax:buffer-helper buffer))))
                        helpers)))
      (sb-int:unencapsulate 'tuatara.syntax::parse-texts 'which)
      (tuatara.buffer:kill-buffer buffer))))

(define-test (syntax scanner-cannot-write-files)
  ;; a scanner writing a file ("!w" and a path) is refused by the sandbox:
  ;; the file is not made, and the parse goes on
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (let ((path (uiop:native-namestring (merge-pathnames "written-by-scanner" directory))))
       (call-with-own-helper
        (lambda (helper)
          (let ((parser (tuatara.syntax:make-parser grammar helper)))
            (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope (format nil "abc !w~a def" path)))))
              (true tree)
              (when tree (tuatara.syntax:delete-tree tree))))))
       ;; without the sandbox the program is only a process of its own, and may write
       (is eq (not (eq :ready (tuatara.io:sandbox-status-now))) (and (probe-file path) t))))))

(define-test (syntax scanner-writing-past-its-buffer-faults)
  ;; a scanner writing past the buffer for its state, a little or much,
  ;; faults on the page after it before it reaches anything else, ending the
  ;; program parsing; in a buffer the grammar is turned off, and the editor
  ;; carries on, other text parsed by another
  (call-with-misbehaving-grammar
   (lambda (grammar directory)
     (declare (ignore directory))
     (dolist (text '("abc !o def" "abc !O def"))
       (let ((parser (tuatara.syntax:make-parser grammar (tuatara.syntax::shared-helper))))
         (multiple-value-bind (tree why unusable) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text))
           (false tree)
           (is eq :crashed why "~s" text)
           (true unusable)
           (false (tuatara.syntax:helper-alive (tuatara.syntax::parser-helper parser)))))
       (let ((said nil))
         (unwind-protect
              (let ((tuatara.syntax:*syntax-fault-function* (lambda (syntax) (setf said syntax))))
                (multiple-value-bind (buffer syntax) (parse-in-buffer grammar text)
                  (unwind-protect
                       (progn
                         (is eq syntax said)
                         (true (tuatara.syntax::grammar-off-p grammar (tuatara.syntax::syntax-helper syntax)) "~s" text)
                         (true (tuatara.syntax:syntax-broken syntax))
                         ;; editing goes on, and another language still parses
                         (tuatara.buffer:buffer-insert buffer 0 "more ")
                         (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json") (tuatara.syntax::shared-helper))))
                           (unwind-protect
                                (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope "[1, 2]"))))
                                  (true tree)
                                  (when tree (tuatara.syntax:delete-tree tree)))
                             (tuatara.syntax:delete-parser parser))))
                    (tuatara.syntax:detach-syntax buffer)
                    (tuatara.buffer:kill-buffer buffer))))
           (setf (tuatara.syntax:grammar-faulted grammar) nil)))))))

;; how the parse of a big file stops when it takes too long
(define-test (syntax slow-parses)
  ;; a parse that takes longer than it may is given up: the buffer shows no
  ;; syntax colours, is not parsed again (which would take as long), and the
  ;; editor says so
  (let ((text (with-output-to-string (o)
                (dotimes (i 20000) (format o "fn f~d() {{ let x = ~d; }}~%" i i)))))
    (let* ((grammar (tuatara.syntax:find-grammar "rust"))
           (parser (tuatara.syntax:make-parser grammar (tuatara.syntax::shared-helper))))
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
        (held (lambda () (held-in-helper (tuatara.syntax::shared-helper)))))
    (let* ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json") (tuatara.syntax::shared-helper)))
           (before (funcall held)))
      (unwind-protect
           (multiple-value-bind (tree why)
               (tuatara.syntax:parse-rope parser (tuatara.text:string->rope numbers) :budget (* 1024 1024))
             (false tree)
             (is eq :memory why))
        (tuatara.syntax:delete-parser parser))
      (true (held-falls-to (tuatara.syntax::shared-helper) (+ before (* 1024 1024)))))
    ;; within the budget its length and tokens call for, it is parsed
    (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar "json") (tuatara.syntax::shared-helper))))
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
        do (let ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar grammar) (tuatara.syntax::shared-helper)))
                 (start (get-internal-real-time)))
             (unwind-protect
                  (let ((tree (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text)
                                                         :limit 5 :budget (* 64 1024 1024))))
                    (true tree "~a" grammar)
                    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)) "~a" grammar)
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
    (let ((tuatara.defaults::*rules-wait* 0)
          (tuatara-user:*grammars-on* '(:common verilog)))
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
    ;; a language whose grammar is off would ask first, taking C as the answer
    (let ((tuatara-user:*grammars-on* '(:common jinja)))
      (is string= (format nil "{# a #}~%") (after "C" (format nil "a~%") "j2")))))

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
           (let ((start (thread-cpu-time)))
             (is = 100001 (tuatara.defaults::indent-lines buffer 0 (tuatara.buffer:buffer-size buffer)))
             (true (< (- (thread-cpu-time) start) (* 2 internal-time-units-per-second))))
           (is string= "    (99999 \"row 99999\" :key 699993)"
               (let ((line (tuatara.buffer:buffer-goto-line buffer 100000)))
                 (tuatara.buffer:buffer-string buffer :start line :end (tuatara.buffer:buffer-line-end buffer line))))
           (let ((line (tuatara.buffer:buffer-goto-line buffer 100000))
                 (start (thread-cpu-time)))
             (tuatara.buffer:buffer-edit buffer (vector (tuatara.text:make-edit line (+ line 3))))
             (is = 1 (tuatara.defaults::indent-lines buffer line line))
             (true (< (- (thread-cpu-time) start) (* 1/2 internal-time-units-per-second)))))
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
                  (start (thread-cpu-time))
                  (ranges (tuatara.defaults::text-object-ranges
                           query (tuatara.syntax:syntax-tree (tuatara.syntax:buffer-syntax buffer)) rope
                           "function.around" (- point 65536) (min (tuatara.text:rope-bytes rope) (+ point 65536)))))
             (true (< (- (thread-cpu-time) start) (* 1/5 internal-time-units-per-second)))
             (is string= (format nil "fn f19999(a: i32, b: i32) -> i32 {~%    // sum~%    a + b~%}")
                 (let ((r (tuatara.defaults::smallest-around ranges point point nil)))
                   (and r (tuatara.buffer:buffer-string buffer :start (car r) :end (cdr r)))))
             ;; so does a test, its rule anchored to the attribute before it
             (let* ((start (thread-cpu-time))
                    (tests (tuatara.defaults::text-object-ranges
                            query (tuatara.syntax:syntax-tree (tuatara.syntax:buffer-syntax buffer)) rope
                            "test.around" (- point 65536) (min (tuatara.text:rope-bytes rope) (+ point 65536)))))
               (true (< (- (thread-cpu-time) start) (* 1/5 internal-time-units-per-second)))
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
  (let ((spans (let ((tuatara-user:*grammars-on* '(:common just)))
                 (aref (tuatara.frontend.fake:fake-spans
                        (colour-session "" (format nil "run:~%    #!/usr/bin/env python3~%    def f(): pass~%") "just"))
                       2))))
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
                  (ranges (let ((tuatara.syntax:*helper* :shared))
                            (tuatara.syntax::injected-highlights injections tree rope middle (+ middle 60)))))
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
                  (parser (tuatara.syntax:make-parser grammar (tuatara.syntax::shared-helper))))
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
             (parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar (tuatara.defaults::language-grammar language)) (tuatara.syntax::shared-helper)))
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

(define-test (syntax no-sandbox-said-as-it-is)
  ;; the message without a sandbox says that text is not parsed, in words that
  ;; read right for both kinds of text
  (flet ((said (trusted)
           (let ((tuatara.command::*messages-kept* (list '())))
             (tuatara.defaults::report-no-sandbox-for-grammars trusted))))
    (true (search "Files outside the projects you trust are not coloured or parsed" (said nil)))
    (true (search "No text is coloured or parsed" (said t)))))

(define-test (syntax deeply-nested-text-parsed)
  ;; text nested hundreds of levels deep, a string open across lines in it:
  ;; the scanner keeps the indents it saves within the room the parser gives
  ;; it, and the text is parsed, not the program parsing it ended
  (flet ((deep-yaml (n)
           (with-output-to-string (o)
             (dotimes (i n) (format o "~va:~%" i ""))
             (format o "~vax: 1~%" n "")))
         (deep-python (n)
           (with-output-to-string (o)
             (dotimes (i n) (format o "~vaif x:~%" i ""))
             (format o "~vax = f\"\"\"a~%b~%\"\"\"~%" n ""))))
    (loop for (grammar text) in (list (list "yaml" (deep-yaml 300)) (list "python" (deep-python 600))
                                      (list "mojo" (deep-python 600))
                                      ;; Typst's list items, each inside the one before
                                      (list "typst" (with-output-to-string (o)
                                                      (dotimes (i 400) (format o "~va- item~%" (* 2 i) "")))))
          do (let* ((parser (tuatara.syntax:make-parser (tuatara.syntax:find-grammar grammar) (tuatara.syntax::shared-helper))))
               (multiple-value-bind (tree why) (tuatara.syntax:parse-rope parser (tuatara.text:string->rope text))
                 (true tree "~a nested deeply is parsed (~s)" grammar why)
                 (when tree (tuatara.syntax:delete-tree tree)))
               (ignore-errors (tuatara.syntax:delete-parser parser))))))

(define-test (syntax rules-not-answered-asked-again)
  ;; rules the program compiling them did not answer for (a busy moment) are
  ;; not taken for failed: asked for again after a wait, they compile
  (let* ((language (tuatara.defaults::find-language 'toml))
         (kept (tuatara.defaults::language-query language))
         (tuatara.defaults::*rules-unanswered-wait* 0.2))
    (tuatara.syntax:rules-helper)
    (setf (tuatara.defaults::language-query language) nil)
    (remhash language tuatara.defaults::*rules-unanswered*)
    (unwind-protect
         (progn
           (sb-int:encapsulate 'tuatara.syntax::send-query 'dead
                               (lambda (function call &rest arguments)
                                 (declare (ignore function arguments))
                                 (setf (tuatara.syntax::call-status call) :dead)))
           (unwind-protect
                (false (tuatara.defaults::language-compiled-query language :colouring))
             (sb-int:unencapsulate 'tuatara.syntax::send-query 'dead))
           (false (tuatara.defaults::language-query language) "not kept as failed")
           (false (tuatara.defaults::language-compiled-query language :colouring) "not asked again at once")
           (sleep 0.3)
           (true (tuatara.syntax:query-p (tuatara.defaults::language-compiled-query language :colouring))
                 "compiled when asked again"))
      (remhash language tuatara.defaults::*rules-unanswered*)
      (unless (tuatara.syntax:query-p (tuatara.defaults::language-query language))
        (setf (tuatara.defaults::language-query language) kept)))))

(define-test (syntax large-rules-given-time)
  ;; compiling rules waits longer than other calls, the longer the rules, and
  ;; lateness there is slowness, not a hang
  (let ((asked '())
        (grammar (tuatara.syntax:find-grammar "toml"))
        (source (tuatara.defaults::language-rules (tuatara.defaults::find-language 'toml))))
    (sb-int:encapsulate 'tuatara.syntax::call-send 'seen
                        (lambda (function call &rest keys)
                          (push keys asked)
                          (apply function call keys)))
    (unwind-protect (tuatara.syntax:compile-query grammar source)
      (sb-int:unencapsulate 'tuatara.syntax::call-send 'seen))
    (let ((keys (first asked)))
      (is eq :slow (getf keys :late))
      (true (and (getf keys :seconds) (> (getf keys :seconds) tuatara.syntax:*syntax-helper-wait*))))))

(define-test (syntax slow-rules-never-hold-the-editor)
  ;; a key needing rules not compiled yet (RET indenting) waits no longer
  ;; than *rules-editor-wait*, however slow the compiling, and the rules
  ;; are kept once compiled in the background
  (let* ((language (tuatara.defaults::find-language 'rust))
         (kept (tuatara.defaults::language-indent-query language))
         (tuatara.defaults::*rules-editor-wait* 0.2))
    (tuatara.syntax:rules-helper)
    (setf (tuatara.defaults::language-indent-query language) nil)
    (sb-int:encapsulate 'tuatara.syntax:compile-query 'slow
                        (lambda (function grammar source &rest more)
                          (when (equal (first more) "indentation") (sleep 3))
                          (apply function grammar source more)))
    (unwind-protect
         (let ((began (get-internal-real-time)))
           (with-file-session ((f editor) "C-e i RET" (format nil "fn f() {~%}~%") "rs")
             (true (< (tuatara.runtime:seconds-since began) 2.5) "~,2f s" (tuatara.runtime:seconds-since began)))
           (true (loop repeat 100 thereis (tuatara.syntax:query-p (tuatara.defaults::language-indent-query language))
                       do (sleep 0.1))
                 "kept once compiled"))
      (sb-int:unencapsulate 'tuatara.syntax:compile-query 'slow)
      (unless (tuatara.syntax:query-p (tuatara.defaults::language-indent-query language))
        (setf (tuatara.defaults::language-indent-query language) kept)))))

(define-test (syntax rules-unanswered-given-up-at-last)
  ;; rules the program compiling them never answers for are given up on
  ;; after *rules-unanswered-most* times, as each time it is ended
  (let* ((language (tuatara.defaults::find-language 'toml))
         (kept (tuatara.defaults::language-query language))
         (tuatara.defaults::*rules-unanswered-wait* 0.01)
         (tuatara.defaults::*rules-unanswered-most* 2)
         (tuatara.defaults::*rules-quiet* t))
    (tuatara.syntax:rules-helper)
    (setf (tuatara.defaults::language-query language) nil)
    (remhash language tuatara.defaults::*rules-unanswered*)
    (sb-int:encapsulate 'tuatara.syntax::send-query 'dead
                        (lambda (function call &rest arguments)
                          (declare (ignore function arguments))
                          (setf (tuatara.syntax::call-status call) :dead)))
    (unwind-protect
         (progn
           (tuatara.defaults::language-compiled-query language :colouring)
           (false (tuatara.defaults::language-query language) "asked again after the first")
           (sleep 0.05)
           (tuatara.defaults::language-compiled-query language :colouring)
           (is eq :failed (tuatara.defaults::language-query language) "given up on after the second"))
      (sb-int:unencapsulate 'tuatara.syntax::send-query 'dead)
      (remhash language tuatara.defaults::*rules-unanswered*)
      (setf (tuatara.defaults::language-query language) kept))))
