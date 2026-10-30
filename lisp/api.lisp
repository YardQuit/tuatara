;;;; lisp/api.lisp --- The names packages are written against, exported from TUATARA.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package has a Lisp package of its own that uses TUATARA,
;;;;
;;;;   (defpackage #:example (:use #:cl #:tuatara))
;;;;
;;;; and these are the names it may use (*API*, and *API-FROM-LIBRARIES* of
;;;; the libraries it is built with): they keep their meaning, everything
;;;; else Tuatara may change. Positions are octets from 0 into a buffer's text
;;;; as it is now, as the core counts them; the Emacs names that count
;;;; characters from 1 (point, goto-char) are an init file's, never here.
;;;; Loaded last, when every name below is defined; the few states of a
;;;; buffer a package may read and set are given their names here.

(in-package #:tuatara.defaults)

(defun buffer-read-only-p (buffer)
  "True when BUFFER may not be changed by typing or by commands that edit."
  (tuatara.buffer:buffer-local buffer :read-only))

(defun (setf buffer-read-only-p) (value buffer)
  "Make BUFFER read-only when VALUE is true, changeable again when it is NIL."
  (setf (tuatara.buffer:buffer-local buffer :read-only) value))

(defun list-preview-p (buffer)
  "True when BUFFER, a list, shows the entry on its line in a window beside
it (\\[list-toggle-preview] switches it)."
  (tuatara.buffer:buffer-local buffer :list-preview))

(defun (setf list-preview-p) (value buffer)
  "Have BUFFER, a list, show the entry on its line beside it when VALUE is
true; not when it is NIL."
  (setf (tuatara.buffer:buffer-local buffer :list-preview) value))

(defun (setf sentence-end-double-space-p) (value buffer)
  "Make BUFFER's sentences two spaces apart when VALUE is true, one space when
it is NIL, whatever *SENTENCE-END-DOUBLE-SPACE* says."
  (setf (tuatara.buffer:buffer-local buffer :sentence-end-double-space) value))

(defparameter *api*
  '(;; defining commands, modes, keys, faces, settings and background work
    tuatara.command:define-command tuatara.modes:define-major-mode define-face define-package-setting
    tuatara.command:define-key tuatara.command:define-keys tuatara.command:label-keys tuatara.modes:mode-keymap
    tuatara.runtime:define-job-words tuatara.command:substitute-command-keys commands-for-mode buttons-in-mode
    tuatara.modes:fundamental-mode special-mode text-mode org-mode org-agenda-mode
    ;; the editor and its buffers
    current-buffer message buffer-list tuatara.command:prefix-argument package-file load-package-file
    tuatara.buffer:make-buffer tuatara.buffer:find-buffer tuatara.buffer:kill-buffer tuatara.buffer:live-buffer-p
    tuatara.buffer:unique-buffer-name tuatara.buffer:buffer-name tuatara.buffer:buffer-file tuatara.command:buffer-directory
    tuatara.buffer:buffer-point tuatara.buffer:buffer-string tuatara.buffer:buffer-size tuatara.buffer:buffer-region
    tuatara.buffer:buffer-start tuatara.buffer:buffer-end tuatara.buffer:buffer-narrowed-p tuatara.buffer:with-narrowing-lifted
    tuatara.buffer:buffer-line-start tuatara.buffer:buffer-line-end tuatara.buffer:buffer-line-text
    tuatara.buffer:buffer-char-at tuatara.buffer:buffer-forward-char char-before-position buffer-crlf-p
    tuatara.buffer:buffer-mark-active-p tuatara.buffer:buffer-version tuatara.buffer:buffer-local delete-spans
    tuatara.modes:buffer-major-mode tuatara.modes:set-major-mode buffer-in-mode
    tuatara.command:show-buffer tuatara.command:display-buffer tuatara.command:display-buffer-at tuatara.command:buffer-window tuatara.command:leave-window tuatara.display:window-point tuatara.command:pop-to-buffer tuatara.command:abbreviated-path
    tuatara.modes:find-or-make-buffer
    ;; a buffer's text as it stands, unchanged by later edits, for work in the background
    tuatara.buffer:buffer-text tuatara.text:rope-bytes tuatara.text:rope-octet tuatara.text:rope-copy-into
    tuatara.text:rope-find-octet tuatara.text:rope-line-start tuatara.text:rope-line-end
    ;; asking and waiting
    tuatara.command:read-string tuatara.command:finish-minibuffer tuatara.command:quit tuatara.command:completing-read tuatara.command:completing-read-value tuatara.command:make-candidate tuatara.command:yes-or-no-p*
    tuatara.command:complete-in-minibuffer
    read-date tuatara.dates:read-date-text
    read-path read-file-path read-native-path read-new-native-path read-whole-number tuatara.command:wait-interruptibly tuatara.command:wait-until-interruptibly
    tuatara.command:first-line tuatara.text:one-line
    ;; telling what has just happened, on the desktop and in *Notifications*
    notify
    ;; lists and text made by a command
    set-made-text render-pieces render-table-list pieces-text collecting-pieces table-cell table-heading-pieces table-row-pieces list-entry-at ansi-pieces
    set-search-filtering search-filters search-filter-p search-filter-string search-filter-where search-filter-match-p
    light-search-filters search-filter-regex list-search-filters remove-search-filters
    marked-list-mode sectioned-list-mode sectioned-pieces entry-member entry-mark keep-list-marks list-acted-on
    list-mark-sign list-mark-face list-question toggle-list-section quit-window
    list-entries-of list-entry-noun delete-entries visit-entry list-visit list-visit-other list-execute list-delete-marked
    entry-list-keys list-step-entry list-next-entry list-previous-entry
    preview-entry entry-preview-width entry-preview-keys list-toggle-preview side-preview-note
    ;; menus, help and switches
    tuatara.command:define-menu tuatara.command:menu-arguments show-help show-help-pieces define-display-toggle
    ;; work in the background
    tuatara.runtime:job tuatara.runtime:start-job-thread tuatara.runtime:job-check-stop tuatara.runtime:job-cancel
    tuatara.runtime:job-buffer tuatara.runtime:job-running-p tuatara.runtime:job-ending-words tuatara.runtime:running-percent
    tuatara.runtime:jobs-of tuatara.runtime:make-guarded-thread tuatara.runtime:deadline-in tuatara.runtime:deadline-passed-p
    tuatara.runtime:job-take-in tuatara.runtime:job-percent tuatara.runtime:job-state-words tuatara.runtime:job-status
    tuatara.runtime:job-program-started tuatara.runtime:running-job take-in-soon
    ;; hooks, each a list a package adds its function to
    *post-self-insert-functions* tuatara.buffer:*kill-buffer-functions* *kill-buffer-query-functions* tuatara.command:*mode-line-functions*
    *health-decoder-functions* *preview-text-functions* *file-local-setting-functions* *file-local-taken-functions* *file-views*
    *org-agenda-file-functions* tuatara.command:*tick-functions*
    ;; showing a file a way of its own
    file-view-path file-view-show file-view-refuses file-view-can-draw-p reading-buffer show-reading-result buffer-private-folder
    visit-path visit-path-as-text image-view-visit pdf-view-visit pdf-file-p office-view-visit view-made-text tuatara.io:safe-visit-p *file-changing-commands*
    ;; files and paths
    tuatara.io:native tuatara.io:native-file-name tuatara.io:native-file-stem tuatara.io:native-file-type tuatara.io:expand-path tuatara.io:parent-path
    tuatara.io:slash-ended tuatara.io:path-within-p tuatara.io:real-path-or-parent tuatara.io:path-join tuatara.io:path-kind tuatara.io:path-stamp tuatara.io:stamp-size tuatara.io:free-bytes
    tuatara.io:directory-entries walk-directory tuatara.io:call-with-file tuatara.io:file-head tuatara.io:binary-head-p tuatara.io:read-fd-start
    tuatara.io:read-fd-octets tuatara.io:write-octets tuatara.io:file-system-error tuatara.io:copy-regular-file tuatara.io:write-new-file tuatara.io:make-directory tuatara.io:remove-file tuatara.io:delete-tree
    tuatara.io:percent-decoded-octets file-kind tuatara.io:octets-kind tuatara.io:kind-name-disagrees-p
    tuatara.io:+binary-sample+ tuatara.io:*compound-file-signature*
    ;; octets, text and dates
    tuatara.text:octets tuatara.io:octets-at-p tuatara.io:octets-le tuatara.io:octets-signed tuatara.io:base64-octets tuatara.io:octets-words
    tuatara.text:octets-string tuatara.text:octets-latin-1 tuatara.text:charset-text tuatara.text:utf-16-text tuatara.text:string-octets
    tuatara.text:string-octet-length tuatara.text:string-line-start tuatara.text:parse-decimal
    tuatara.text:lenient-string tuatara.text:merge-ranges tuatara.text:string-hash
    tuatara.text:xml-entity tuatara.text:xml-tag-end tuatara.text:xml-text-value tuatara.io:fd-octets tuatara.io:file-octets tuatara.text:xml-escape
    tuatara.text:xml-name tuatara.text:xml-attribute tuatara.text:xml-children tuatara.text:xml-child tuatara.text:xml-root
    tuatara.text:xml-local-name tuatara.text:xml-local-children tuatara.text:xml-find-all tuatara.text:xml-all-text
    tuatara.text:json-read tuatara.text:json-get tuatara.text:json-items tuatara.text:json-integer tuatara.text:json-text
    tuatara.dates:format-time tuatara.dates:valid-date-p tuatara.dates:time-words tuatara.dates:short-time-words tuatara.dates:utc-time-words iso-date-time
    ;; days as numbers, and time zones by name
    tuatara.dates:day-number tuatara.dates:day-date tuatara.dates:weekday tuatara.dates:month-length tuatara.dates:leap-year-p tuatara.dates:add-months
    tuatara.dates:time-zone tuatara.dates:time-zone-p tuatara.dates:time-zone-name tuatara.dates:time-zone-rule tuatara.dates:fixed-time-zone tuatara.dates:ruled-time-zone
    tuatara.dates:zone-named tuatara.dates:zone-name-p tuatara.dates:system-zone-name tuatara.dates:system-time-zone
    tuatara.dates:zone-offset tuatara.dates:zone-local-time tuatara.dates:zone-universal-time tuatara.dates:rule-day
    tuatara.dates:zone-rule tuatara.dates:make-zone-rule tuatara.dates:zone-rule-standard-name tuatara.dates:zone-rule-standard-offset
    tuatara.dates:zone-rule-daylight-name tuatara.dates:zone-rule-daylight-offset tuatara.dates:zone-rule-to-daylight tuatara.dates:zone-rule-to-standard
    ;; Org entries as the agenda reads them, their timestamps, dates by rule and times cancelled or moved
    org-agenda-file-entries agenda-entry-file agenda-entry-text agenda-entry-position agenda-entry-end agenda-entry-level
    agenda-entry-todo agenda-entry-todo-type agenda-entry-title agenda-entry-heading agenda-entry-tags agenda-entry-local-tags
    agenda-entry-scheduled agenda-entry-deadline agenda-entry-closed agenda-entry-properties agenda-entry-stamps agenda-entry-ranges
    org-read-ts make-org-ts org-ts-text org-ts-day-number org-ts-active org-ts-year org-ts-month org-ts-day org-ts-hour org-ts-minute
    org-ts-end-hour org-ts-end-minute org-ts-repeater org-ts-repeat-value org-ts-repeat-unit org-ts-time-text org-day-timestamp-text
    org-read-rule org-rule-text-p org-rule-next org-rule-kind org-rule-arguments org-rule-hour org-rule-minute org-rule-end-hour org-rule-end-minute
    org-occurrence-notes org-occurrence-note repeat-rule-text repeat-months repeat-open-end org-agenda-main-file org-capture-with
    org-planning-line-p org-ends-with-newline-p org-ensure-newline tuatara.io:file-text
    ;; an Org file written to while it may be open: its buffer, its new text by lines, saved
    tuatara.io:visit-file put-text-by-lines save-buffer-with-care
    ;; text from elsewhere written into Org safely, and read back
    org-safe-line org-safe-line-words org-escape-block org-unescape-block
    tuatara.io:inflate tuatara.text:xml-parse xmp-fields jpeg-metadata file-sha256 shown-file
    ;; PDF objects, read from octets alone
    pdf-object-at pdf-skip-space pdf-delimiter-p pdf-literal-string-at pdf-hex-string-at pdf-string-text pdf-date-words
    *pdf-most-items* tuatara.io:pdf-header-at pdf-white-p pdf-line-end-after pdf-integer-at pdf-number-at
    pdf-get pdf-dictionary-p pdf-ref-p pdf-array-items pdf-object-header-before pdf-indirect-object-at
    pdf-indirect-p pdf-indirect-number pdf-indirect-generation pdf-indirect-value pdf-indirect-at pdf-indirect-end
    pdf-indirect-stream-start pdf-indirect-stream-end pdf-indirect-length pdf-indirect-packed-in
    pdf-stream-filters pdf-stream-octets *pdf-picture-filters* pdf-object-stream-offsets pdf-packed-object
    ;; octets shown as a hex dump, text shortened, and text from a file with its control characters shown
    show-octets-in-hex tuatara.text:octets-hex tuatara.display:shorten-string tuatara.command:escape-controls tuatara.text:xml-unprefixed tuatara.io:ensure-directories
    ;; programs from outside, shut in
    tuatara.io:find-program tuatara.io:program-missing tuatara.io:program-output tuatara.io:sandbox-run
    ;; a program of your own run on what a job gives it through a pipe, its arguments split as a shell would
    tuatara.io:call-with-program tuatara.io:filter-through-program tuatara.io:command-words tuatara.io:read-stream-octets
    tuatara.io:sandbox-find-program run-on-copies copies-original-names
    run-on-copies-for-report tuatara.io:sandbox-outcome-words sandbox-pdf program-health-words likelihood-face
    read-decimal-number read-fraction fraction-p tuatara.io:random-name tuatara.io:copy-to-free-name tuatara.io:percent-decoded tuatara.dates:*short-month-names*
    user-program-path start-detached open-externally-file new-private-folder private-cache-directory private-state-directory tuatara.io:replace-file
    tuatara.io:memory-file sandbox-font-directories pdf-view-make
    ;; pictures turned into pixels in the sandbox, and their pixels read
    decode-picture tuatara.frontend:image-width tuatara.frontend:image-height tuatara.frontend:image-channels tuatara.frontend:image-pixels
    write-private-copy attachment-file-name
    ;; rimu, the file manager
    rimu-mode *rimu-keymap* rimu-chosen-paths rimu-paths-marked-anywhere rimu-paths-marked-apart *rimu-master-mark* rimu-mark-master rimu-chosen rimu-marks rimu-take-out rimu-mark-and-go-on rimu-each-line
    rimu-show-paths rimu-show-file rimu-leads-to rimu-list-p rimu-list-directory rimu-path set-rimu-column
    ;; delimited data, regular expressions, and lines changed all at once
    csv-mode tsv-mode filter-lines filter-line-matcher mode-record-fields make-record-fields csv-delimiter csv-fields csv-field-spans csv-field-bounds csv-value-bounds csv-field-value csv-header-names tuatara.syntax:compile-regex
    selected-lines rewrite-lines line-diff
    ;; sentences, prose and what a file says of its settings
    prose-p prose-test prose-buffer-p prose-ready-p sentence-end-char-p sentence-closing-char-p *sentence-enders*
    *sentence-closers* sentence-end-double-space-p *sentence-abbreviations* *extra-sentence-abbreviations*
    *removed-sentence-abbreviations* sentence-abbreviations-in-force abbreviation-before-p
    switched-by-count too-large-here-p
    ;; the selection, banked lines, and whether a buffer may be changed
    selected-region selection-range banked-spans span-line-count buffer-banks check-writable buffer-read-only-p list-preview-p
    file-declares set-file-local-setting *file-local-variables*)
  "The names a package may use, exported from TUATARA: each keeps its meaning
from one version of Tuatara to the next.")

(import *api* :tuatara)
(export *api* :tuatara)

(defparameter *api-from-libraries*
  '(cl-ppcre:create-scanner cl-ppcre:scan cl-ppcre:scan-to-strings cl-ppcre:all-matches cl-ppcre:do-scans
    cl-ppcre:do-register-groups cl-ppcre:regex-replace-all cl-ppcre:quote-meta-chars
    uiop:string-prefix-p uiop:string-suffix-p uiop:split-string uiop:getenv)
  "The names of the libraries Tuatara is built with that a package may use as
well, written with their library's package in front: cl-ppcre's regular
expressions (a scanner COMPILE-REGEX makes is cl-ppcre's) and uiop's
string-prefix-p, string-suffix-p, split-string and getenv. Each keeps its
meaning from one version of Tuatara to the next, as the names above do.")
