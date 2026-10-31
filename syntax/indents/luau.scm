; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for luau: Helix's rules for Lua,
; runtime/queries/lua/indents.scm of Helix
; (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice: the grammar of tree-sitter-luau
; (https://github.com/tree-sitter-grammars/tree-sitter-luau) at commit
; a8914d6c1fc5 is Lua's grammar with Luau's types added, and names Lua's
; nodes as Lua's does.

[
  (function_definition)
  (function_declaration)
  (method_index_expression)
  (field)
  (if_statement)
  (for_statement)
  (repeat_statement)
  (while_statement)
  (table_constructor)
  (arguments)
  (do_statement)
] @indent

[
  "end"
  "until"
  "else"
  "elseif"
  "}"
  ")"
] @outdent
