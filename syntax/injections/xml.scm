; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in xml: runtime/queries/xml/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

; changed: the grammar of tree-sitter-grammars, kept up, names the parts of a document as the XML standard does; it read ((comment) @injection.content
((Comment) @injection.content
 (#set! injection.language "comment"))
