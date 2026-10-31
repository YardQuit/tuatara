; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The indentation rules for protobuf: runtime/queries/protobuf/indents.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

[
  (message_body)
  (enum_body)
; changed: the grammar of coder3101, kept up, has no node for the braces of a oneof, a service or a method; it read
;     (oneof_body)
;     (service_body)
;     (rpc_body)
  (oneof)
  (service)
  (rpc)
  (block_lit)
] @indent

"}" @outdent

