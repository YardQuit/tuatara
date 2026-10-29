;;;; src/frontend/package.lisp --- Package definition for tuatara.frontend.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.frontend
  (:use #:cl #:alexandria)
  (:documentation "The narrow interface every painter implements: frames and windows, input, glyph-row output, cursor shape, clipboard, pictures, keys and their descriptions.")
  (:export #:key #:key-p #:make-key #:key-base #:key-ctrl #:key-meta #:key-shift #:key-column #:key-row #:key-spec #:key-sequence-description
           #:parse-key #:parse-key-sequence #:key-description #:spec-key
           #:event #:event-p #:make-event #:event-kind #:event-key #:event-width #:event-height
           #:event-files #:event-column #:event-row #:event-text
           #:frontend-start #:frontend-stop #:frontend-size #:frontend-read-event #:frontend-read-event-timeout
           #:frontend-input-pending-p #:frontend-wake
           #:frontend-colour-count #:frontend-description #:frontend-health #:frontend-set-clipboard #:frontend-terminal-device #:frontend-lend-terminal #:frontend-clipboard #:frontend-set-title #:frontend-font-steps #:frontend-fonts #:frontend-font-faces #:frontend-set-font #:frontend-toggle-state #:frontend-desktop-style #:frontend-settings-changed #:frontend-renderers #:frontend-measure-drawing #:padding-sides
           #:frontend-begin-frame #:frontend-put-row #:frontend-set-cursor #:frontend-end-frame
           #:image #:image-p #:make-image #:image-width #:image-height #:image-fit-scale #:image-channels #:image-pixels #:image-id
           #:frontend-primary-selection #:frontend-set-primary-selection #:frontend-images-p #:frontend-cell-pixels #:frontend-put-image #:*kitty-marks* #:+kitty-placeholder+))
