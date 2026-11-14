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
;;;; Loaded last, when every name below is defined.

(in-package #:tuatara.defaults)

(defun rope-octet (rope byte)
  "The octet at offset BYTE of ROPE, a buffer's text. Refused (NOT-ARRIVED)
for a part of a file that has not arrived: what the system gives for it, zero,
is no octet of the file."
  (tuatara.text:rope-check-arrived rope byte (1+ byte))
  (tuatara.text:rope-octet rope byte))

(defun rope-find-octet (rope value &key (start 0) (end (tuatara.text:rope-octet-length rope)))
  "The offset of the first octet VALUE of ROPE, a buffer's text, from START
below END, or NIL. Refused (NOT-ARRIVED) when part of that range of a file has
not arrived, for what is there cannot then be told."
  (tuatara.text:rope-check-arrived rope start end)
  (tuatara.text:rope-find-octet rope value :start start :end end))

(defun agenda-entry-get (entry field)
  "What the agenda entry ENTRY (as ORG-AGENDA-FILE-ENTRIES gives them) holds
for FIELD. :FILE is the path of its file. :TEXT is the whole text of that file
as it was read. :POSITION and :END are the characters of that text its
headline begins at and its own text ends at, its headlines below left out.
:LEVEL is how many stars it has. :TODO is its TODO keyword, or NIL. :TODO-TYPE
is TODO or DONE for that keyword, NIL for none. :TITLE is its headline without
stars, keyword, priority and tags. :HEADING is its headline after the stars as
written. :TAGS is its tags, those it inherits first. :LOCAL-TAGS is its own,
on its headline. :SCHEDULED, :DEADLINE and :CLOSED are the texts of those
timestamps, or NIL. :PROPERTIES is an alist of names and values. :STAMPS is
the texts of the active timestamps in its text, its planning line's left out.
:RANGES is its ranges of days, each (FROM TO), the two timestamps' texts.
:KIND is :HEADLINE, or :CHECKBOX for a list item with a checkbox kept as a
task. :PRIORITY is Org's number for it, 2000 for [#A], 1000 for [#B] and none,
0 for [#C]. :CATEGORY is its category. :LOGS is what its log says, in order.
Each is one of \(:CLOSED TIMESTAMP NIL NIL) for its CLOSED: line. \(:CLOCK
FROM DURATION STOPPED OPEN) is for a CLOCK: line, FROM its start as written,
DURATION as written (1:05) and STOPPED the time of day it stopped (10:00) when
it has an end, OPEN true while it has none. \(:DONE-NOTE TIMESTAMP NIL NIL
TICKED) is for a note of being done, TICKED (DONE . TOTAL) of its checkboxes
when the line after says so, else NIL."
  (ecase field
    (:file (agenda-entry-file entry))
    (:text (agenda-entry-text entry))
    (:position (agenda-entry-position entry))
    (:end (agenda-entry-end entry))
    (:level (agenda-entry-level entry))
    (:todo (agenda-entry-todo entry))
    (:todo-type (agenda-entry-todo-type entry))
    (:title (agenda-entry-title entry))
    (:heading (agenda-entry-heading entry))
    (:tags (agenda-entry-tags entry))
    (:local-tags (agenda-entry-local-tags entry))
    (:scheduled (agenda-entry-scheduled entry))
    (:deadline (agenda-entry-deadline entry))
    (:closed (agenda-entry-closed entry))
    (:properties (agenda-entry-properties entry))
    (:stamps (agenda-entry-stamps entry))
    (:ranges (agenda-entry-ranges entry))
    (:kind (agenda-entry-kind entry))
    (:priority (agenda-entry-priority entry))
    (:category (agenda-entry-category entry))
    (:logs (agenda-entry-logs entry))))

(defun org-ts-get (ts field)
  "What the timestamp TS (as ORG-READ-TS reads one) holds for FIELD. :ACTIVE
is true for <...>, NIL for [...]. :YEAR, :MONTH and :DAY are its date as
written. :DAY-NUMBER is its day as a day number (DAY-NUMBER), a date past its
month's end counted on into the next. :HOUR and :MINUTE are its time, NIL
without one. :END-HOUR and :END-MINUTE are the end of its time range, NIL
without one. :REPEATER is how it repeats, :CUMULATE (+), :CATCH-UP (++) or
:RESTART (.+), NIL when it does not. :REPEAT-VALUE and :REPEAT-UNIT are how
often, the unit #\\h, #\\d, #\\w, #\\m or #\\y. :WARNING is how its warning
counts, :ALL (-) or :FIRST \(--), NIL without one. :WARNING-VALUE and
:WARNING-UNIT are how long before, in the same units."
  (ecase field
    (:active (org-ts-active ts))
    (:year (org-ts-year ts))
    (:month (org-ts-month ts))
    (:day (org-ts-day ts))
    (:day-number (org-ts-day-number ts))
    (:hour (org-ts-hour ts))
    (:minute (org-ts-minute ts))
    (:end-hour (org-ts-end-hour ts))
    (:end-minute (org-ts-end-minute ts))
    (:repeater (org-ts-repeater ts))
    (:repeat-value (org-ts-repeat-value ts))
    (:repeat-unit (org-ts-repeat-unit ts))
    (:warning (org-ts-warning ts))
    (:warning-value (org-ts-warning-value ts))
    (:warning-unit (org-ts-warning-unit ts))))

(defun org-rule-get (rule field)
  "What the date by rule RULE (as ORG-READ-RULE reads one) holds for FIELD.
:KIND is one of :FLOAT (the Nth or last weekday of a month), :CYCLIC (every N
days), :BLOCK (every day from one date to another), :ANNIVERSARY (a day each
year) and :DATE (the days whose year, month and day are as given). It is also
one of :WEEKDAYS \(the days of the week listed, 0 for Sunday to 6 for
Saturday), :ALL (the days all its rules take) or :ANY (the days any of them
takes). :ARGUMENTS is what its form says, as ORG-READ-RULE reads them. :HOUR,
:MINUTE, :END-HOUR and :END-MINUTE are the time written after it, or NIL."
  (ecase field
    (:kind (org-rule-kind rule))
    (:arguments (org-rule-arguments rule))
    (:hour (org-rule-hour rule))
    (:minute (org-rule-minute rule))
    (:end-hour (org-rule-end-hour rule))
    (:end-minute (org-rule-end-minute rule))))

(defparameter *api*
  '(;; defining commands, modes, keys, faces, settings and background work
    tuatara.command:define-command tuatara.modes:define-major-mode define-face tuatara-user:define-theme
    tuatara.packages:define-package-setting tuatara.packages:declare-package-needs tuatara.command:define-key
    tuatara.command:define-keys tuatara.command:label-keys tuatara.modes:mode-keymap tuatara.runtime:define-job-words
    tuatara.runtime:define-record tuatara.command:substitute-command-keys commands-for-mode buttons-in-mode
    tuatara.modes:fundamental-mode special-mode text-mode org-mode org-agenda-mode
    ;; colours, as themes derive their shades
    tuatara.display:colour-components tuatara.display:colour-hex tuatara.display:mix-colours
    ;; the editor and its buffers
    current-buffer tuatara.command:selected-window-buffer message buffer-list tuatara.command:prefix-argument
    package-file load-package-file tuatara.command:add-gutter-provider tuatara.command:buffer-mode-named-p
    tuatara.buffer:make-buffer tuatara.buffer:find-buffer tuatara.buffer:kill-buffer tuatara.buffer:live-buffer-p
    tuatara.buffer:unique-buffer-name tuatara.buffer:buffer-name tuatara.buffer:buffer-file
    tuatara.command:buffer-directory tuatara.buffer:buffer-point tuatara.buffer:buffer-string tuatara.buffer:buffer-size
    tuatara.buffer:buffer-region tuatara.buffer:buffer-start tuatara.buffer:buffer-end tuatara.buffer:buffer-narrowed-p
    tuatara.buffer:with-narrowing-lifted tuatara.buffer:buffer-line-start tuatara.buffer:buffer-line-end
    tuatara.buffer:buffer-line-text tuatara.buffer:buffer-char-at tuatara.buffer:buffer-forward-char
    tuatara.buffer:buffer-char-before buffer-crlf-p tuatara.buffer:buffer-mark-active-p tuatara.buffer:buffer-version
    tuatara.buffer:buffer-local delete-spans tuatara.modes:buffer-major-mode tuatara.modes:set-major-mode buffer-in-mode
    tuatara.command:show-buffer tuatara.command:display-buffer tuatara.command:display-buffer-at
    tuatara.command:buffer-window tuatara.command:leave-window tuatara.display:window-point
    tuatara.command:pop-to-buffer tuatara.command:abbreviated-path tuatara.modes:find-or-make-buffer
    ;; a buffer's text as it stands, unchanged by later edits, for work in the background
    tuatara.buffer:buffer-text tuatara.text:rope-octet-length rope-octet tuatara.text:rope-copy-into rope-find-octet
    tuatara.text:rope-line-start tuatara.text:rope-line-end
    ;; asking and waiting
    tuatara.command:read-string tuatara.command:finish-minibuffer tuatara.command:quit tuatara.command:completing-read
    tuatara.command:completing-read-value tuatara.command:make-candidate tuatara.command:yes-or-no-p*
    tuatara.command:y-or-n-p* tuatara.command:complete-in-minibuffer tuatara.command:*question-variables*
    tuatara.command:*buffers-asked-about* read-date tuatara.dates:read-date-text read-path read-file-path
    read-directory-path read-native-path read-new-native-path read-whole-number tuatara.command:wait-interruptibly
    tuatara.command:wait-until-interruptibly tuatara.command:first-line tuatara.text:one-line read-decimal-number
    read-fraction fraction-p
    ;; telling what has just happened, on the desktop and in *Notifications*
    notify
    ;; lists and text made by a command
    set-made-text render-pieces render-table-list pieces-text collecting-pieces table-cell table-heading-pieces
    table-row-pieces list-entry-at tuatara.terminal-emulator:ansi-pieces set-search-filtering search-filters
    search-filter-p search-filter-string search-filter-where search-filter-match-p light-search-filters
    search-filter-regex search-filter-list search-filter-remove-all marked-list-mode sectioned-list-mode
    sectioned-pieces entry-member entry-mark keep-list-marks list-acted-on list-mark-sign list-mark-face list-question
    list-toggle-section quit-window list-entries-of list-entry-noun delete-entries visit-entry list-visit
    list-visit-other list-execute list-delete-marked entry-list-keys list-step-entry list-next-entry list-previous-entry
    preview-entry entry-preview-width entry-preview-keys list-toggle-preview side-preview-note
    ;; menus, help and switches
    tuatara.command:define-menu tuatara.command:menu-arguments show-help show-help-pieces define-display-toggle
    ;; work in the background
    tuatara.runtime:job tuatara.runtime:start-job-thread tuatara.runtime:job-check-stop tuatara.runtime:job-cancel
    tuatara.runtime:job-buffer tuatara.runtime:job-running-p tuatara.runtime:job-ending-words
    tuatara.runtime:running-percent tuatara.runtime:jobs-of tuatara.runtime:make-guarded-thread
    tuatara.runtime:thread-result tuatara.runtime:deadline-in tuatara.runtime:deadline-passed-p
    tuatara.runtime:job-take-in
    tuatara.runtime:job-percent tuatara.runtime:job-state-words tuatara.runtime:job-status
    tuatara.runtime:job-program-started tuatara.runtime:running-job take-in-soon
    ;; failures nothing can be done about, kept in the debug log, and failures said
    tuatara.runtime:ignoring-errors-reported tuatara.runtime:*secret-at-hand* tuatara.runtime:failure-words
    ;; hooks, each a list a package adds its function to
    *post-self-insert-functions* tuatara.buffer:*kill-buffer-functions* *kill-buffer-query-functions*
    tuatara.command:*mode-line-functions* *health-decoder-functions* *preview-text-functions*
    *file-local-setting-functions* *file-local-taken-functions* *file-views* *org-agenda-file-functions*
    tuatara.command:*tick-functions*
    ;; showing a file a way of its own
    file-view-path file-view-show file-view-refuses file-view-can-draw-p reading-buffer show-reading-result
    buffer-private-directory visit-path visit-path-as-text image-view-visit pdf-view-visit pdf-file-p office-view-visit
    view-made-text tuatara.io:safe-visit-p *file-changing-commands*
    ;; files and paths
    tuatara.io:native tuatara.io:native-file-name tuatara.io:native-file-stem tuatara.io:native-file-type
    tuatara.io:expand-path tuatara.io:parent-path tuatara.io:slash-ended tuatara.io:path-within-p
    tuatara.io:real-path-or-parent tuatara.io:path-join tuatara.io:path-kind tuatara.io:path-stamp tuatara.io:stamp-size
    tuatara.io:free-octets tuatara.io:directory-entries walk-directory tuatara.io:call-with-file tuatara.io:file-head
    tuatara.io:binary-head-p tuatara.io:read-fd-start tuatara.io:read-fd-octets tuatara.io:write-octets
    tuatara.io:file-system-error tuatara.io:copy-regular-file tuatara.io:write-new-file tuatara.io:make-directory
    tuatara.io:remove-file tuatara.io:delete-tree tuatara.io:random-name tuatara.io:copy-to-free-name
    tuatara.io:fit-name tuatara.io:percent-decoded tuatara.io:file-text tuatara.io:ensure-directories
    ;; evidence, and the places nothing is written in
    tuatara.io:writing-refusal tuatara.io:writing-refused copy-file-checked tuatara.io:percent-decoded-octets file-kind
    tuatara.io:octets-kind tuatara.io:kind-name-disagrees-p tuatara.io:+binary-sample+
    tuatara.io:*compound-file-signature*
    ;; octets, text and dates
    tuatara.text:octets tuatara.io:octets-at-p tuatara.io:octets-search tuatara.io:octets-integer
    tuatara.io:octets-signed tuatara.io:base64-octets tuatara.io:octets-words tuatara.text:octets-string
    tuatara.text:octets-latin-1 tuatara.text:charset-text tuatara.text:utf-16-text tuatara.text:string-octets
    tuatara.text:string-octet-length tuatara.text:string-line-start tuatara.text:parse-decimal tuatara.text:whole-number
    tuatara.text:lenient-string tuatara.text:merge-ranges tuatara.text:string-hash tuatara.text:octets-hash
    tuatara.text:blank-text-p tuatara.text:xml-entity tuatara.text:xml-tag-end tuatara.text:xml-text-value
    tuatara.io:fd-octets tuatara.io:file-octets tuatara.text:xml-escape tuatara.text:xml-attribute
    tuatara.text:xml-children tuatara.text:xml-root tuatara.text:xml-local-name tuatara.text:xml-local-children
    tuatara.text:xml-find-all tuatara.text:xml-all-text tuatara.text:json-read tuatara.text:json-get
    tuatara.text:json-items tuatara.text:json-integer tuatara.text:json-text tuatara.dates:format-time
    tuatara.dates:valid-date-p tuatara.dates:time-words tuatara.dates:short-time-words tuatara.dates:utc-time-words
    iso-date-time tuatara.dates:*short-month-names*
    ;; days as numbers, and time zones by name
    tuatara.dates:day-number tuatara.dates:day-date tuatara.dates:weekday tuatara.dates:month-length
    tuatara.dates:leap-year-p tuatara.dates:add-months tuatara.dates:time-zone tuatara.dates:time-zone-p
    tuatara.dates:time-zone-name tuatara.dates:time-zone-rule tuatara.dates:fixed-time-zone
    tuatara.dates:ruled-time-zone tuatara.dates:time-zone-named tuatara.dates:time-zone-name-p
    tuatara.dates:system-time-zone-name tuatara.dates:system-time-zone tuatara.dates:zone-offset
    tuatara.dates:zone-local-time tuatara.dates:zone-universal-time tuatara.dates:rule-day tuatara.dates:zone-rule
    tuatara.dates:make-zone-rule tuatara.dates:zone-rule-standard-name tuatara.dates:zone-rule-standard-offset
    tuatara.dates:zone-rule-daylight-name tuatara.dates:zone-rule-daylight-offset tuatara.dates:zone-rule-to-daylight
    tuatara.dates:zone-rule-to-standard
    ;; Org entries as the agenda reads them, their timestamps, dates by rule and times cancelled or moved
    org-agenda-file-entries agenda-entry-get org-read-ts make-org-ts org-ts-get org-ts-text org-ts-time-text
    org-day-timestamp-text org-read-rule org-rule-get org-rule-text-p org-rule-part-text org-rule-next
    org-occurrence-notes org-occurrence-note repeat-rule-text repeat-months repeat-open-end org-agenda-main-file
    org-capture-with org-planning-line-p org-ends-with-newline-p org-ensure-newline
    ;; an Org file written to while it may be open: its buffer, its new text by lines, saved
    tuatara.io:visit-file put-text-by-lines save-buffer-with-care
    ;; text from elsewhere written into Org safely, and read back
    org-safe-line org-safe-line-words org-escape-block org-unescape-block tuatara.io:inflate tuatara.text:xml-parse
    xmp-fields jpeg-metadata octets-metadata tuatara.io:jpeg-segments file-sha256 octets-sha256 octets-md5 shown-file
    ;; PDF objects, read from octets alone
    pdf-object-at pdf-date-words *pdf-item-limit* tuatara.io:pdf-header-at pdf-white-p pdf-get pdf-dictionary-p
    pdf-ref-p pdf-array-items map-pdf-dictionary pdf-indirect-object-at pdf-indirect-get map-pdf-written-objects
    pdf-eof-lines pdf-startxref pdf-offset-base map-pdf-cross-reference-table map-pdf-cross-reference-stream
    pdf-stream-filters pdf-stream-octets *pdf-picture-filters* pdf-object-stream-offsets pdf-packed-object
    map-pdf-content
    ;; octets shown as a hex dump, text shortened, and text from a file with its control characters shown
    show-octets-in-hex tuatara.text:octets-hex tuatara.display:shorten-string tuatara.command:escape-controls
    tuatara.text:xml-unprefixed
    ;; programs from outside, shut in
    tuatara.io:find-program tuatara.io:program-missing tuatara.io:program-output tuatara.io:sandbox-run
    tuatara.io:program-variable tuatara.io:own-variable tuatara.io:own-directory
    ;; a program of your own run on what a job gives it through a pipe, its arguments split as a shell would
    tuatara.io:call-with-program tuatara.io:filter-through-program tuatara.io:command-words
    tuatara.io:read-stream-octets tuatara.io:sandbox-find-program run-on-copies copies-original-names
    run-on-copies-for-report sandbox-outcome-words sandbox-pdf program-health-words user-program-path
    start-detached open-externally-file new-private-directory private-cache-directory private-state-directory
    tuatara.io:replace-file tuatara.io:memory-file sandbox-font-directories pdf-view-make
    ;; pictures turned into pixels in the sandbox, and their pixels read
    decode-picture tuatara.frontend:image-width tuatara.frontend:image-height tuatara.frontend:image-channels
    tuatara.frontend:image-pixels write-private-copy attachment-file-name
    ;; rimu, the file manager
    rimu-mode *rimu-keymap* rimu-chosen-paths rimu-paths-marked-anywhere rimu-paths-marked-apart *rimu-master-mark*
    rimu-mark-master rimu-chosen rimu-marks rimu-take-out rimu-mark-and-go-on rimu-each-line rimu-show-paths
    rimu-show-file rimu-leads-to rimu-list-p rimu-list-directory rimu-path rimu-set-column
    ;; delimited data, regular expressions, and lines changed all at once
    csv-mode tsv-mode filter-lines filter-line-matcher mode-record-fields make-record-fields csv-delimiter csv-fields
    csv-field-spans csv-field-bounds csv-value-bounds csv-field-value csv-header-names
    rope-record-start csv-record-end tuatara.syntax:compile-regex
    selected-lines rewrite-lines line-diff
    ;; sentences, prose and what a file says of its settings
    prose-p prose-test prose-buffer-p prose-ready-p sentence-end-char-p sentence-closing-char-p *sentence-enders*
    *sentence-closers* sentence-end-double-space-p *sentence-abbreviations* *extra-sentence-abbreviations*
    *removed-sentence-abbreviations* sentence-abbreviations-in-force abbreviation-before-p switched-by-count
    too-large-here-p
    ;; the selection, banked lines, and whether a buffer may be changed
    selected-region selection-range banked-spans span-line-count buffer-banks check-writable buffer-read-only-p
    list-preview-p file-declares set-file-local-setting *file-local-variables*)
  "The names a package may use, exported from TUATARA: each keeps its meaning
from one version of Tuatara to the next.")

(import *api* :tuatara)
(export *api* :tuatara)

(defparameter *api-from-libraries*
  '(cl-ppcre:create-scanner cl-ppcre:scan cl-ppcre:scan-to-strings cl-ppcre:all-matches cl-ppcre:do-scans
    cl-ppcre:do-register-groups cl-ppcre:regex-replace-all cl-ppcre:quote-meta-chars
    uiop:string-prefix-p uiop:string-suffix-p uiop:split-string)
  "The names of the libraries Tuatara is built with that a package may use as
well, written with their library's package in front: cl-ppcre's regular
expressions (a scanner COMPILE-REGEX makes is cl-ppcre's) and uiop's
string-prefix-p, string-suffix-p and split-string. Each keeps its meaning
from one version of Tuatara to the next, as the names above do. An
environment variable is read with PROGRAM-VARIABLE (yours, as the client you
work from has it) or OWN-VARIABLE (the editor process's own), not getenv.")
