; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for svelte: runtime/queries/svelte/textobjects.scm
; of Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; with its node names changed to those of tree-sitter-svelte
; (https://github.com/tree-sitter-grammars/tree-sitter-svelte) at commit
; ae5199db4775. A snippet's parameters, text in that grammar, are selected
; as one.

(element (start_tag) (_)* @xml-element.inside (end_tag))

(element) @xml-element.around

(comment) @comment.around

(snippet_start
  (svelte_raw_text) @parameter.inside)

(snippet_statement
  (snippet_start) (_)* @function.inside (snippet_end)) @function.around
