; The colouring rules for protobuf: queries/highlights.scm of
; tree-sitter-proto (https://github.com/coder3101/tree-sitter-proto) at
; commit 18366491f37d, under the MIT License (its text goes with the
; grammar), with the names of its captures changed to Tuatara's where they
; differ (@boolean is @constant.builtin.boolean, @number @constant.numeric,
; @module @namespace).

(package
  (full_ident
    (identifier) @namespace))

(extend
  (full_ident
    (identifier) @type))

(constant
  (full_ident
    (identifier) @constant))

(field
  (identifier) @property)

(map_field
  (identifier) @property)

(oneof
  (identifier) @type)

(oneof_field
  (identifier) @property)

(field_option
  (identifier) @property)

(enum_value_option
  (identifier) @property)

(block_lit
  (identifier) @property)

; Extension names and Any type URLs in aggregate option values,
; e.g. { [foo.bar]: 1 } and { [type.googleapis.com/foo.Bar]: {} }
(extension_name
  name: (full_ident
    (identifier) @variable))

(extension_name
  type: (full_ident
    (identifier) @type))

; Extension option names, e.g. option (foo.bar) = ...
; Also matches field/enum-value options, e.g. [(foo.bar) = ...]
[
  (option
    (full_ident
      (identifier) @variable))
  (field_option
    (full_ident
      (identifier) @variable))
  (enum_value_option
    (full_ident
      (identifier) @variable))
]

[
  (option
    (full_ident
      (identifier)
      (identifier) @variable.other.member))
  (field_option
    (full_ident
      (identifier)
      (identifier) @variable.other.member))
  (enum_value_option
    (full_ident
      (identifier)
      (identifier) @variable.other.member))
]

; Bare option names, e.g. option java_package = ...
; Also matches the trailing segments of a parenthesized name,
; e.g. option (foo.bar).baz = ...
; Matches the @property treatment of bare field_option/enum_value_option
; names below, since these all name a field on a proto *Options message.
(option
  (identifier) @property)

[
  "option"
  "syntax"
  "edition"
] @keyword.directive

[
  "reserved"
  "to"
  "max"
] @keyword

[
  "enum"
  "extend"
  "extensions"
  "group"
  "message"
  "map"
  "oneof"
  "service"
] @keyword.type

"rpc" @keyword.function

"returns" @keyword.control.return

[
  "export"
  "local"
  "optional"
  "repeated"
  "required"
  "stream"
  "weak"
  "public"
] @keyword.modifier

[
  "package"
  "import"
] @keyword.control.import

[
  (key_type)
  (type)
] @type.builtin

[
  (message_name)
  (enum_name)
  (service_name)
  (message_or_enum_type)
] @type

(rpc_name) @function.method

(enum_field
  (identifier) @constant)

(string) @string

; reserved names are their own node type rather than (string), so they need
; their own rule - without it they are the only unhighlighted literal.
(reserved_identifier) @string

(import
  path: (string) @string.special.path)

(syntax
  version: (string) @string.special.symbol)

(escape_sequence) @constant.character.escape

(int_lit) @constant.numeric

(float_lit) @constant.numeric.float

[
  (true)
  (false)
] @constant.builtin.boolean

(comment)

(comment) @comment

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "<"
  ">"
] @punctuation.bracket

[
  ";"
  ","
  "."
  ":"
] @punctuation.delimiter

[
  "="
  "-"
  "+"
] @operator
