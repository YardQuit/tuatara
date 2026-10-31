; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in svelte: runtime/queries/svelte/locals.scm
; of Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; with its node names changed to those of tree-sitter-svelte
; (https://github.com/tree-sitter-grammars/tree-sitter-svelte) at commit
; ae5199db4775. A snippet's parameters, text in that grammar, are not
; local names.

(snippet_statement) @local.scope

(snippet_name) @local.definition.function

(tag_name) @local.reference
