;;;; src/frontend/package.lisp --- Package definition for tuatara.frontend.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.frontend
  (:use #:cl #:alexandria)
  (:import-from #:tuatara.runtime #:define-record #:define-setting)
  (:documentation "The narrow interface every painter implements: frames and windows, input,
glyph-row output, cursor shape, clipboard, pictures, keys and their
descriptions.")
  (:export #:key #:key-p #:make-key #:key-base #:key-ctrl #:key-meta #:key-shift #:key-column #:key-row #:key-spec
           #:key-sequence-description
           #:parse-key #:parse-key-sequence #:key-description #:spec-key
           #:event #:event-p #:input-event-p #:make-event #:event-kind #:event-key #:event-width #:event-height
           #:event-files #:event-column #:event-row #:event-text #:event-frame #:dropped-name-p #:events-held
           #:event-queue #:make-event-queue #:queue-push #:queue-pop #:queue-empty-p #:queue-input-p #:tick-p
           #:frontend-start #:frontend-stop #:frontend-size #:frontend-read-event #:frontend-read-event-timeout
           #:frontend-input-pending-p #:frontend-wake #:frontend-another #:frontend-frame #:frontend-raise
           #:frontend-scales-rows-p #:frontend-ask-another
           #:frontend-colour-count #:frontend-description #:frontend-health #:frontend-set-clipboard
           #:frontend-terminal-device #:frontend-lend-terminal #:frontend-clipboard #:frontend-set-title
           #:frontend-font-steps #:frontend-fonts #:frontend-font-faces #:frontend-set-font #:frontend-toggle-state
           #:frontend-desktop-style #:frontend-settings-changed #:frontend-renderers #:frontend-measure-drawing
           #:padding-sides
           #:frontend-begin-frame #:frontend-put-row #:frontend-set-cursor #:frontend-end-frame
           #:image #:image-p #:make-image #:image-width #:image-height #:image-fit-scale #:image-channels #:image-pixels
           #:image-id
           #:frontend-primary-selection #:frontend-set-primary-selection #:frontend-images-p #:frontend-reorders-text-p
           #:frontend-cell-pixels #:frontend-put-image #:*kitty-marks* #:+kitty-placeholder+
           ;; frontends in another process (src/frontend/remote.lisp)
           #:remote-frontend #:make-remote-frontend #:remote-client #:make-remote-client #:run-remote-client
           #:remote-refused #:remote-refused-words #:remote-refused-other #:malformed-message #:malformed-message-reason
           #:remote-request #:remote-exit-code #:remote-exit-words #:refuse-connection #:shut-and-close #:shown-words
           #:request #:make-request #:request-p #:request-directory #:request-terminal #:request-environment
           #:request-files #:request-safe #:request-hex #:request-standard-input #:request-diff #:request-merge
           #:request-frames #:request-wait #:request-stop #:request-joins #:request-far #:frontend-focused-p
           #:request-agent #:request-notes #:greet-editor #:*starting-seconds* #:take-window-asked-for
           #:windows-not-come #:*window-seconds* #:+most-client-windows+ #:remote-pid #:decline-window
           #:remote-client-painted #:+protocol+
           ;; the octets of a connection, sent and received, for other protocols over a local socket too
           #:outbox #:make-outbox #:outbox-octets #:outbox-fill #:outbox-room #:put-octets #:send-outbox
           #:inbox #:make-inbox #:inbox-octets #:inbox-start #:inbox-at #:inbox-stop #:inbox-deadline #:fill-inbox
           #:take-place #:message-read #:malformed
           #:null-frontend #:make-null-frontend))
