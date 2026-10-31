; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The rules naming the definitions and headings in protobuf: runtime/queries/protobuf/tags.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

(message_name (identifier) @definition.class)

(enum_name (identifier) @definition.class)

(service_name (identifier) @definition.class)

(rpc_name (identifier) @definition.function)

; changed: the grammar of coder3101, kept up, has no node for the name of a field or of an enumeration value; it read (enum_variant_name (identifier) @definition.constant)
(enum_field (identifier) @definition.constant)

; changed: the grammar of coder3101, kept up, has no node for the name of a field or of an enumeration value; it read (field_name (identifier) @definition.constant)
(field (identifier) @definition.constant)
