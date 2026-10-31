; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in svelte:
; runtime/queries/svelte/tags.scm of Helix
; (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14, with its
; node names changed to those of tree-sitter-svelte
; (https://github.com/tree-sitter-grammars/tree-sitter-svelte) at commit
; ae5199db4775.

; Snippet declarations
(snippet_statement
  (snippet_start
    (snippet_name) @name)) @definition.function

; Component references
((tag_name) @name @reference.class
  (#match? @name "^[A-Z]"))
