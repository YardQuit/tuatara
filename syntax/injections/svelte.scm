; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in svelte:
; runtime/queries/svelte/injections.scm of Helix
; (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14, with its
; node names changed to those of tree-sitter-svelte
; (https://github.com/tree-sitter-grammars/tree-sitter-svelte) at commit
; ae5199db4775. That grammar keeps the code in braces as text, not as
; JavaScript or TypeScript apart: it is read as JavaScript.

; Script elements with TypeScript aliases
((script_element
  (start_tag
    (attribute
      (quoted_attribute_value
        (attribute_value) @_value)))
  (raw_text) @injection.content)
  (#any-of? @_value "ts" "typescript" "text/typescript")
  (#set! injection.language "typescript"))

; Script elements with explicit language
((script_element
  (start_tag
    (attribute
      (attribute_name) @_lang
      (quoted_attribute_value
        (attribute_value) @injection.language)))
  (raw_text) @injection.content)
  (#eq? @_lang "lang")
  (#not-any-of? @injection.language "ts" "typescript" "text/typescript"))

; Script content defaults to JavaScript
((script_element
  (raw_text) @injection.content)
  (#set! injection.language "javascript"))

; Style with lang="scss"
((style_element
  (start_tag
    (attribute
      (quoted_attribute_value
        (attribute_value) @_scss)))
  (raw_text) @injection.content)
  (#eq? @_scss "scss")
  (#set! injection.language "scss"))

; Style with lang="sass"
((style_element
  (start_tag
    (attribute
      (quoted_attribute_value
        (attribute_value) @_sass)))
  (raw_text) @injection.content)
  (#eq? @_sass "sass")
  (#set! injection.language "sass"))

; Style with lang="less"
((style_element
  (start_tag
    (attribute
      (quoted_attribute_value
        (attribute_value) @_less)))
  (raw_text) @injection.content)
  (#eq? @_less "less")
  (#set! injection.language "less"))

; Style with explicit language
((style_element
  (start_tag
    (attribute
      (attribute_name) @_lang
      (quoted_attribute_value
        (attribute_value) @injection.language)))
  (raw_text) @injection.content)
  (#eq? @_lang "lang")
  (#not-any-of? @injection.language "scss" "sass" "less"))

; Style content defaults to CSS when no lang attribute is present
((style_element
  (start_tag
    (attribute
      (attribute_name) @_style_attr)*)
  (raw_text) @injection.content)
  (#not-any-of? @_style_attr "lang")
  (#set! injection.language "css"))

; Inline style attribute
((attribute
  (attribute_name) @_style_name
  (quoted_attribute_value (attribute_value) @injection.content))
  (#eq? @_style_name "style")
  (#set! injection.language "css"))

; Expressions, tags, the conditions and bindings of blocks, and a
; snippet's parameters
((svelte_raw_text) @injection.content
  (#set! injection.language "javascript"))

; Comments
((comment) @injection.content
  (#set! injection.language "comment"))
