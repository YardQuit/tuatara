; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for ink: runtime/queries/ink/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit ba40e547426b,
; unchanged below this notice.

(choice_block
  (choice (choice_marks) . _ @anchor)) @align
(gather_block
  (gather (gather_marks) . _ @anchor)) @align
