;;;; src/text/package.lisp --- Package definition for tuatara.text.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.text
  (:use #:cl #:alexandria)
  (:documentation "The text model: a rope of UTF-8 leaves with cached counts, mapped files, edit batches, versions and anchors.")
  (:export #:xml-entity #:xml-tag-end #:xml-text-value
   #:xml-name #:xml-attribute #:xml-children #:xml-child #:xml-root #:xml-local-name #:xml-unprefixed #:xml-local-children #:xml-find-all #:xml-all-text
   #:xml-first-text #:xml-parse #:*xml-deepest* #:*xml-most-elements* #:xml-escape
   #:xml-elements #:xml-element #:xml-top-elements #:xml-element-inner #:xml-element-inside #:xml-replace-elements #:xml-value
   #:utf-16-text #:parse-decimal #:split-words #:number-formatter #:merge-ranges #:+fnv-offset+ #:+fnv-prime+ #:octets-hash #:string-hash
   ;; JSON
   #:json-read #:json-path-bounds #:json-write #:json-object #:json-get #:json-integer #:json-text #:json-items #:json-true-p #:json-error #:json-error-position #:json-error-text #:*json-deepest*
   ;; types and tunables
   #:octets #:make-octets #:with-octet-block #:fixvec #:+max-leaf+ #:+target-leaf+ #:+min-leaf+ #:+max-fanout+ #:+span-size+
   ;; mapped files
   #:mapping #:mapping-p #:map-file #:map-fd #:close-mapping #:retarget-mapping
   #:mapping-path #:mapping-length #:mapping-live #:mapping-fd #:mapping-sap #:mapping-rope #:map-rope-sources
   ;; ropes
   #:rope #:rope-p #:make-rope #:string->rope #:octets->rope #:open-file-rope #:index-rope
   #:rope-bytes #:rope-chars #:rope-utf16 #:rope-lines #:rope-indexed-p
   #:rope-statistics
   ;; conversions
   #:byte->char #:char->byte #:byte->utf16 #:utf16->byte #:byte->line #:line->byte #:char-boundary-p
   ;; reading
   #:map-leaf-pieces #:piece-copy #:piece-octet #:rope-octets #:rope-copy-into #:rope-copy-to-sap #:rope-string #:rope-octet
   #:rope-next-boundary #:rope-previous-boundary #:string-cluster-end #:string-cluster-start #:cluster-joins-p #:regional-indicator-code-p #:rope-char-start #:rope-char-at #:rope-find-octet #:rope-find-octet-backward #:rope-find-any-octet #:rope-bracket-match #:rope-escaped-p #:rope-skip-octets #:*blank-octets*
   #:rope-line-downward #:rope-line-upward #:map-line-slices #:rope-count-chars #:*text-octets*
   #:tally #:tally-p #:make-tally #:tally-line-number #:tally-line-start #:tally-counted-lines
   #:rope-count-newlines #:rope-nth-newline
   #:rope-line-start #:rope-line-end #:rope-line-pieces #:blank-line-octet-p #:*blank-line-octets*
   #:rope-pieces #:rope-slice #:splice-rope
   ;; edits
   #:edit #:edit-p #:make-edit #:edit-start #:edit-end #:edit-insert #:edit-deleted
   #:edit-insert-length #:edit-delete-length
   #:apply-edits #:inverse-edits #:rope-insert #:rope-delete
   ;; versions, batches and anchors
   #:version #:version-p #:make-version #:version-rope #:version-number #:version-batch #:version-next
   #:version-current-p #:version-commit #:edits-batch #:swap-batch #:version-swap #:version-adopt #:version-undo-edits
   #:batch #:batch-p #:batch-edits #:batch-deltas #:batch-positions #:batch-swap
   #:swap #:swap-p #:make-swap #:swap-before #:swap-after #:swap-forward #:swap-backward
   #:map-position #:map-forward
   #:anchor #:anchor-p #:make-anchor #:anchor-position #:anchor-move #:anchor-link #:anchor-bias
   ;; invariants
   #:check-rope #:summarise #:continuation-byte-p
   ;; strict UTF-8
   #:octets-decode #:octets-char-start-p #:octets-pending #:octets-string #:string-octets #:put-string-octets #:string-octet-length #:char-octet-length #:lenient-string #:raw-byte-code-p #:control-code-p #:crlf-octets-p #:line-text-end #:rope-line-text-end #:rope-crlf-p #:*openers* #:*closers* #:*opening-octets* #:*closing-octets* #:*bracket-octets* #:rope-invalid
   ;; octets and lines of plain strings
   #:octets-latin-1 #:word-char-p #:name-char-p #:charset-text #:octets-hex #:*hex-pairs* #:string-line-start #:string-line-end #:one-line #:natural-string-lessp))
