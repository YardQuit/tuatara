; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules for local names in ink: runtime/queries/ink/locals.scm of
; Helix (https://github.com/helix-editor/helix) at commit ba40e547426b,
; unchanged below this notice.

(knot_block) @local.scope
(stitch_block) @local.scope

(param (identifier) @local.definition.variable.parameter)
(param (divert (identifier) @local.definition.variable.parameter))
(temp_def name: (identifier) @local.definition.variable)

(identifier) @local.reference
(divert (identifier) @local.reference)
