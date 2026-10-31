; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The selection rules for protobuf: runtime/queries/protobuf/textobjects.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

(message (message_body) @class.inside) @class.around
(enum (enum_body) @class.inside) @class.around
; changed: the grammar of coder3101, kept up, has no node for the braces of a oneof, a service or a method; it read (service (service_body) @class.inside) @class.around
(service (service_name) (_)* @class.inside) @class.around

(rpc (message_or_enum_type) @parameter.inside) @function.inside
(rpc (message_or_enum_type) @parameter.around) @function.around

(comment) @comment.inside
(comment)+ @comment.around
