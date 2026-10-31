; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in slisp: runtime/queries/slisp/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit ba40e547426b,
; unchanged below this notice.

;; def
(function_definition name: (symbol) @name) @definition.function

;; ext
(external_definition name: (symbol) @name) @definition.function

;; mac
(macro_definition name: (symbol) @name) @definition.function
