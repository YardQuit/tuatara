; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for ink: runtime/queries/ink/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit ba40e547426b,
; unchanged below this notice.

(knot_block (knot function: _) . _+ @function.inside) @function.around
(knot_block (knot !function) . _+ @class.inside) @class.around
(stitch_block (stitch) . _+ @class.inside) @class.around


(params ((param) @parameter.inside . ","? @parameter.around) @parameter.around)
(args ((_) @parameter.inside . ","? @parameter.around) @parameter.around)

(line_comment) @comment.inside
(line_comment)+ @comment.around

(block_comment) @comment.inside @comment.around

(((list_value_def) @entry.inside ) . ","? @entry.around) @entry.around

