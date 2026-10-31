; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for svelte: runtime/queries/svelte/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; with its node names changed to those of tree-sitter-svelte
; (https://github.com/tree-sitter-grammars/tree-sitter-svelte) at commit
; ae5199db4775.

[
  (element)
  (if_statement)
  (each_statement)
  (await_statement)
  (key_statement)
  (snippet_statement)
] @indent

[
  (end_tag)
  (if_end)
  (each_end)
  (await_end)
  (key_end)
  (snippet_end)
  (else_if_start)
  (else_start)
  (then_start)
  (catch_start)
] @outdent
