; The rules for text in other languages for lean: queries/injections.scm of
; tree-sitter-lean (https://github.com/Julian/tree-sitter-lean) at commit 916c44f4d89c,
; under the MIT License (its text goes with the grammar), unchanged below
; this notice.

; Language injections for tree-sitter-lean.

; Doc comments are Markdown / Verso content.
((doc_comment) @injection.content
  (#set! injection.language "markdown"))

((module_doc_comment) @injection.content
  (#set! injection.language "markdown"))

; Raw string contents stay plain; no injection beyond default text.
