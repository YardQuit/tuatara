; This Source Code Form is subject to the terms of the Mozilla Public
; License, v. 2.0. If a copy of the MPL was not distributed with this
; file, You can obtain one at https://mozilla.org/MPL/2.0/.
;
; The colouring rules for vue: runtime/queries/vue/highlights.scm of
; Helix (https://github.com/helix-editor/helix) at commit 9bcdf4d79d14,
; unchanged below this notice but where a comment says what was changed.

(tag_name) @tag
(end_tag) @tag

(directive_name) @keyword
; changed: the grammar of tree-sitter-grammars, kept up, names the argument of a directive (:class, @click) its value, after a mark of its own; it read (directive_argument) @constant
(directive_value) @constant

(attribute
  (attribute_name) @attribute
  [(attribute_value) (quoted_attribute_value)]? @string)
 
; changed: the grammar of tree-sitter-grammars, kept up, names the argument of a directive (:class, @click) its value, after a mark of its own; it read
;   (directive_attribute
;     (directive_name) @attribute
;     (directive_argument)? @attribute
;     (directive_modifiers)? @attribute
;     [(attribute_value) (quoted_attribute_value)]? @string) 
(directive_attribute
  [(directive_name) (directive_value) (dynamic_directive_value) (directive_modifiers) ":" "." "@" "#"] @attribute)

(directive_attribute
  [(attribute_value) (quoted_attribute_value)] @string)

(comment) @comment

[
  "<"
  ">"
  "</"
  "{{"
  "}}"
  "/>" 
] @punctuation.bracket
"=" @punctuation.delimiter

