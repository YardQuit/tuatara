; The colouring rules for svelte: queries/highlights.scm of
; tree-sitter-svelte (https://github.com/tree-sitter-grammars/tree-sitter-svelte)
; at commit ae5199db4775, under the MIT License (its text goes with the
; grammar), with the names of its captures changed to Tuatara's where they
; differ (@keyword.conditional is @keyword.control.conditional), and the
; words of a block ({:else if}, {@const}) coloured as keywords.

; inherits: html

(raw_text) @none

[
  "as"
  "key"
  "html"
  "snippet"
  "render"
] @keyword

"const" @keyword.storage.modifier

[
  "if"
  "else"
  "else if"
  "then"
] @keyword.control.conditional

"each" @keyword.control.repeat

[
  "await"
  "then"
] @keyword.control

"catch" @keyword.control.exception

"debug" @keyword

[
  "{"
  "}"
] @punctuation.bracket

[
  "#"
  ":"
  "/"
  "@"
] @punctuation.special
