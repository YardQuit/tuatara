; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for bash: runtime/queries/bash/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

[
  (function_definition)
  (if_statement)
  (for_statement)
  (while_statement)
  (case_statement)
  (case_item)
  (pipeline)
] @indent

[
  "}"
  "fi"
  "elif"
  "else"
  "done"
  "esac"
] @outdent

; changed: added, a block not closed yet reaches on to the line begun right
; after it, so that the line after "then", "else", "do", "in" or "{" goes inside
(if_statement (MISSING "fi")) @extend
(do_group (MISSING "done")) @extend
(ERROR "case" "in") @indent @extend
(ERROR "{") @indent @extend

; changed: added, a case item reaches on to the line after it ("a)" then its
; commands) until it ends in ;; ;& or ;;&
(case_item) @extend
(case_item [";;" ";&" ";;&"] @extend.prevent-once)
