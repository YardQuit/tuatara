; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for text in other languages in jinja: runtime/queries/jinja/injections.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

; changed: the grammar of cathaysia, kept up, parses the template whole: the text between its tags is HTML; it read
;   ((source_file) @injection.content
;    (#set! injection.combined)
;    (#set! injection.include-children)
((content) @injection.content
 (#set! injection.combined)
 (#set! injection.language "html"))