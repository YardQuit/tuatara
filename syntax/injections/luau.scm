; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in luau: Helix's rules for Lua,
; runtime/queries/lua/injections.scm of Helix
; (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice: the grammar of tree-sitter-luau
; (https://github.com/tree-sitter-grammars/tree-sitter-luau) at commit
; a8914d6c1fc5 is Lua's grammar with Luau's types added, and names Lua's
; nodes as Lua's does.

((comment) @injection.content
 (#set! injection.language "comment")
 (#set! injection.include-children))

; string.match("123", "%d+")
(function_call
  (dot_index_expression
    field: (identifier) @_method
    (#any-of? @_method "find" "match" "gmatch" "gsub"))
  arguments: (arguments
    .
    (_)
    .
    (string
      content: (string_content) @injection.content
      (#set! injection.language "luap")
      (#set! injection.include-children))))

; ("123"):match("%d+")
(function_call
  (method_index_expression
    method: (identifier) @_method
    (#any-of? @_method "find" "match" "gmatch" "gsub"))
  arguments: (arguments
    .
    (string
      content: (string_content) @injection.content
      (#set! injection.language "luap")
      (#set! injection.include-children))))

; string.format("format string", ...)
((function_call
  name: (dot_index_expression
    table: (identifier) @_table
    field: (identifier) @_function)
  arguments: (arguments
    .
    (string
      content: (string_content) @injection.content)))
  (#eq? @_table "string")
  (#eq? @_function "format")
  (#set! injection.language "lua-format-string"))

; ("format"):format(...)
((function_call
  name: (method_index_expression
    table: (parenthesized_expression
      (string
        content: (string_content) @injection.content))
    method: (identifier) @_function))
  (#eq? @_function "format")
  (#set! injection.language "lua-format-string"))
