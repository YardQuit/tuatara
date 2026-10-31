; The colouring rules for jinja: tree-sitter-jinja/queries/highlights.scm of
; tree-sitter-jinja (https://github.com/cathaysia/tree-sitter-jinja) at
; commit c213d3745ccd, under the Apache License 2.0 (its text goes with the
; grammar), with the names of its captures changed to Tuatara's where they
; differ (@boolean is @constant.builtin.boolean, @number @constant.numeric,
; @keyword.conditional @keyword.control.conditional).

[
  "{{"
  "{{-"
  "{{+"
  "+}}"
  "-}}"
  "}}"
  "{%"
  "{%-"
  "{%+"
  "+%}"
  "-%}"
  "%}"
] @keyword.directive

(string_literal) @string

(number_literal) @constant.numeric

(float_literal) @constant.numeric.float

(boolean_literal) @constant.builtin.boolean

(null_literal) @constant

"defined" @constant

(comment) @comment

[
  ","
  "."
  ":"
] @punctuation.delimiter

[
  (attribute_ignore)
  (attribute_context)
  "recursive"
] @attribute.builtin

[
  "("
  ")"
  "["
  "]"
  "<"
  ">"
] @punctuation.bracket

(binary_operator) @operator

[
  "block"
  "with"
  "filter"
  "macro"
  "set"
  "trans"
  "pluralize"
  "autoescape"
  "required"
] @keyword

[
  (endtrans_statement)
  (endblock_statement)
  (endwith_statement)
  (endfilter_statement)
  (endmacro_statement)
  (endcall_statement)
  (endset_statement)
  (endtrans_statement)
  (endautoescape_statement)
] @keyword

(do_statement
  "do" @keyword)

[
  "include"
  "import"
  "from"
  "extends"
  "as"
] @keyword.control.import

(import_statement
  (identifier) @variable)

(import_as
  (identifier) @variable)

[
  "if"
  (else_statement)
  (endif_statement)
  (elif_block)
] @keyword.control.conditional

[
  "for"
  "in"
  "continue"
  "break"
  (endfor_statement)
] @keyword.control.repeat

"call" @function

(function_call
  (identifier) @function)

(arg
  (identifier) @variable.parameter)

(arg
  (expression
    (binary_expression
      (unary_expression
        (primary_expression
          (identifier) @variable.parameter)))))

(expression
  "."
  (expression)+ @variable.other.member)

(assignment_expression
  "."
  (identifier)+ @variable.other.member)

(inline_trans
  "_" @function.builtin)

"debug" @function.builtin

; TODO: only match raw
(raw_start) @keyword

(raw_end) @keyword

(raw_body) @markup.raw.block

(builtin_test
  [
    "boolean"
    "even"
    "in"
    "mapping"
    "sequence"
    "callable"
    "integer"
    "ne"
    "string"
    "defined"
    "filter"
    "iterable"
    "none"
    "test"
    "divisibleby"
    "float"
    "le"
    "number"
    "eq"
    "ge"
    "lower"
    "odd"
    "undefined"
    "escaped"
    "gt"
    "lt"
    "sameas"
    "upper"
  ] @keyword.operator)
