;;;; src/syntax/package.lisp --- Package definition for tuatara.syntax.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.syntax
  (:use #:cl #:alexandria)
  (:import-from #:tuatara.io #:null-pointer #:null-pointer-p #:sap-octets #:with-foreign-memory)
  (:import-from #:tuatara-user #:*syntax-helper-memory* #:*syntax-helper-wait* #:*syntax-helpers-most* #:*syntax-helper-unconfined*)
  (:documentation "The search and regex interface, the tree-sitter binding, parsing in the background, colouring and moving by syntax.")
  (:export #:search-forward #:search-backward #:search-all #:case-fold-p #:*serial-window* #:*parallel-piece*
           #:compile-regex #:compile-pattern #:*regex-thorough* #:regex-search-forward #:regex-search-backward #:regex-search-all
           #:literal-pattern-p #:regex-case-fold-p #:map-regex-matches #:matched-text #:map-slice-matches #:make-decoder #:with-decoder #:*regex-longest-match*
           #:map-literal-matches #:collect-matches #:join-pieces #:bracket-end #:pattern-refused #:pattern-refused-words #:pattern-group-count
           ;; tree-sitter: grammars, parsing, queries
           #:find-grammar #:grammar-pending-p #:grammar #:grammar-p #:grammar-name #:grammar-symbols #:syntax-helper-program
           #:*helper* #:*parsing-buffer* #:text-key #:forget-text-faults #:with-own-helper #:buffer-helper #:buffer-helper-soon #:shared-helper #:rules-helper #:forget-buffer-helper #:helper-memory #:*syntax-helper-memory* #:*syntax-helper-wait* #:*syntax-helper-shared-function* #:*helper-ended-function* #:*helper-refused-function* #:*helper-in-use-function* #:*must-not-wait-function* #:helper-parsing #:helper-kind #:helper #:helper-p #:helper-alive #:helper-shared #:helper-ended #:stop-helpers #:tree #:tree-p #:parser #:parser-p
           #:*grammar-directories* #:grammar-directories #:forget-grammars #:user-grammar-directory #:user-grammar-file #:grammar-name-p #:*user-grammar-function*
           #:make-parser #:delete-parser #:parse-rope #:delete-tree #:edit-tree #:tree-has-error-p
           #:compile-query #:query #:query-p #:query-grammar #:query-captures #:query-properties
           #:query-not-answered #:query-error #:query-error-kind #:query-error-line #:query-error-text #:query-error-grammar
           #:map-query-matches #:query-non-local-patterns #:disable-query-pattern #:query-starts #:query-source
           ;; syntax trees of buffers, and their colours
           #:syntax #:syntax-p #:syntax-buffer #:buffer-syntax #:attach-syntax #:detach-syntax #:syntax-tree #:syntax-grammar
           #:syntax-rope #:syntax-broken #:parse-memory-limit #:parse-time-limit #:*syntax-fault-function* #:*syntax-slow-function* #:*syntax-time-limit* #:*syntax-time-per-megabyte* #:*syntax-memory-limit* #:*syntax-memory-per-octet* #:grammar-faulted #:grammar-failure #:parsers-let-go #:syntax-too-big #:syntax-current #:syntax-job #:syntax-generation
           #:syntax-ready-tree #:parse-under-way-p #:copy-tree-pointer #:wait-for-parse #:catch-up #:refresh-syntax #:*syntax-refresh-delay* #:syntax-patched #:syntax-no-refresh
           #:*syntax-size-limit* #:*syntax-wait* #:*syntax-edit-limit* #:*syntax-wake-function*
           #:syntax-highlights #:*rules-generation* #:rich-highlights #:language-highlights #:injection-cut #:*injection-reach*
           ;; moving by the tree
           #:tree-node #:tree-node-p #:tree-node-start #:tree-node-end #:tree-node-name #:tree-node-named
           #:tree-node-children #:tree-node-broken #:tree-node-symbol #:node-child-ranges #:node-ancestry #:select-node #:expression-end #:expression-start #:enclosing-pairs
           #:text-enclosing-pairs
           #:opaque-spans #:grammar-named #:string-node-p #:comment-node-p #:opening-bracket-node-p #:closing-bracket-node-p
           #:+bracket-reach+ #:bracket-scan-near #:bracket-scan-between #:scan-next-bracket #:scan-previous-bracket
           #:next-group #:previous-group #:group-start-around #:group-end-around #:text-kind-around))
