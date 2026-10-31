; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for xml: runtime/queries/xml/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

; changed: the grammar of tree-sitter-grammars, kept up, names the parts of a document as the XML standard does; it read (element (start_tag) (_)* @xml-element.inside (end_tag))
(element (STag) (_)* @xml-element.inside (ETag))

(element) @xml-element.around

; changed: the grammar of tree-sitter-grammars, kept up, names the parts of a document as the XML standard does; it read (comment) @comment.around
(Comment) @comment.around
