; The colouring rules for dtd: queries/dtd/highlights.scm of
; tree-sitter-xml (https://github.com/tree-sitter-grammars/tree-sitter-xml)
; at commit 5000ae8f22d1, under the MIT License (its text goes with the
; grammar), with the names of its captures changed to Tuatara's where they
; differ (@boolean is @constant.builtin.boolean, @number @constant.numeric)
; and without its rule colouring every parse error.

;; XML declaration

"xml" @keyword

[ "version" "encoding" ] @property

(EncName) @string.special

(VersionNum) @constant.numeric

;; Processing instructions

(PI) @embedded

(PI (PITarget) @keyword)

;; Element declaration

(elementdecl
  "ELEMENT" @keyword
  (Name) @tag)

(contentspec
  (_ (Name) @property))

"#PCDATA" @type.builtin

[ "EMPTY" "ANY" ] @string.special.symbol

[ "*" "?" "+" ] @operator

;; Entity declaration

(GEDecl
  "ENTITY" @keyword
  (Name) @constant)

(GEDecl (EntityValue) @string)

(NDataDecl
  "NDATA" @keyword
  (Name) @label)

;; Parsed entity declaration

(PEDecl
  "ENTITY" @keyword
  "%" @operator
  (Name) @constant)

(PEDecl (EntityValue) @string)

;; Notation declaration

(NotationDecl
  "NOTATION" @keyword
  (Name) @constant)

(NotationDecl
  (ExternalID
    (SystemLiteral (URI) @string.special)))

;; Attlist declaration

(AttlistDecl
  "ATTLIST" @keyword
  (Name) @tag)

(AttDef (Name) @property)

(AttDef (Enumeration (Nmtoken) @string))

(DefaultDecl (AttValue) @string)

[
  (StringType)
  (TokenizedType)
] @type.builtin

(NotationType "NOTATION" @type.builtin)

[
  "#REQUIRED"
  "#IMPLIED"
  "#FIXED"
] @attribute

;; Entities

(EntityRef) @constant

((EntityRef) @constant.builtin
 (#any-of? @constant.builtin
   "&amp;" "&lt;" "&gt;" "&quot;" "&apos;"))

(CharRef) @constant

(PEReference) @constant

;; External references

[ "PUBLIC" "SYSTEM" ] @keyword

(PubidLiteral) @string.special

(SystemLiteral (URI) @markup.link)

;; Delimiters & punctuation

[
  "<?" "?>"
  "<!" ">"
  "<![" "]]>"
] @punctuation.delimiter

[ "(" ")" "[" ] @punctuation.bracket

[ "\"" "'" ] @punctuation.delimiter

[ "," "|" "=" ] @operator

;; Misc

; FIXME: enable when ts test is fixed
; [ "INCLUDE" "IGNORE" ] @keyword

(Comment) @comment
