;;;; src/package.lisp --- The umbrella package.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;; UIOP's define-package, as lisp/api.lisp adds the names it exports: loaded
;; again, this package is put back as written here, with no warning
(uiop:define-package #:tuatara
  (:use #:cl)
  (:documentation "The names packages are written against: the version here, and every name a
package may use once the defaults are loaded (lisp/api.lisp). A package's own
Lisp package uses this one; TUATARA-USER, which init files are read in, does too.")
  (:export #:*version* #:version #:*build* #:*built-at* #:version-line))

(defpackage #:tuatara-user
  (:use #:cl #:tuatara)
  ;; Emacs's names, counting characters, in place of the API's of these names, counting octets
  (:shadow #:buffer-name #:buffer-string #:buffer-size
           ;; the API's octet bounds of a narrowed buffer's part: point-min and point-max here
           #:buffer-start #:buffer-end)
  (:documentation "The package user configuration files are read in: BIND-KEY and UNBIND-KEY are
available unqualified, and the public API of TUATARA is used.")
  (:export #:bind-key #:unbind-key #:define-recorded-keys #:set-face #:load-theme #:load-package #:define-theme #:define-named-pattern
           #:package-source #:load-package-file #:package-file #:*package-ask-before-loading* #:*package-sources* #:*light-theme* #:*dark-theme* #:*startup-buffer* #:*auto-scratch-buffers*
           #:set-colouring #:set-indentation
           ;; for Lisp typed at M-:, in *lisp-scratch* and in the init file, the basics of
           ;; working on buffers and their text; positions are characters counted from 1
           #:message #:current-buffer #:point #:point-min #:point-max #:goto-char #:insert
           #:delete-region #:erase-buffer #:buffer-substring #:buffer-string #:buffer-size
           #:buffer-name #:buffer-file-name #:set-buffer #:with-current-buffer #:save-excursion
           #:get-buffer #:get-buffer-create #:buffer-list #:switch-to-buffer #:find-file
           #:bobp #:eobp #:bolp #:eolp #:char-after #:char-before
           #:line-beginning-position #:line-end-position #:forward-line #:forward-char #:backward-char
           #:count-lines #:count-words #:line-number-at-pos #:kill-new
           ;; options an init file sets by these names, defined where they are used
           #:*message-log-max* #:*mode-line-name-max* #:*sort-fold-case* #:*reload-changed-files* #:*line-numbers-default*
           #:*indent-new-lines* #:*tab-width* #:*indent-tabs-mode* #:set-tab-width #:set-indent-tabs-mode
           #:*show-paren-mode* #:*debug-on-error* #:*tab-always-indent* #:*blink-matching-paren*
           #:*debug-mode* #:*debug-log-max* #:*idle-collection-seconds* #:*csv-separator* #:*list-header-lock* #:*csv-header*
           ;; saving work and the session
           #:*auto-save* #:*auto-save-directory* #:*auto-save-idle* #:*auto-save-interval* #:*auto-save-largest* #:*keep-session*
           #:*keep-session-interval*
           ;; encryption, and wiping files
           #:*gpg-program* #:*gpg-passphrase-from* #:*metadata-look-up-key-owners* #:*wipe-passes* #:*cache-directory* #:*state-directory* #:*packages-directory* #:*package-most-octets* #:*vc-gutter-max-bytes* #:*conflict-markers-max-bytes* #:*archive-seconds* #:*archive-free-reserve* #:*archive-listing-limit* #:*differences-limit* #:*gpg-loopback-warning*
           ;; language servers and formatters
           #:*language-servers* #:*language-formatters* #:*lsp-trusted-projects* #:*lsp-languages* #:*lsp-signature-help* #:*lsp-inlay-hints*
           #:*lsp-wait* #:*lsp-events-kept* #:*lsp-watch-files* #:*lsp-watch-skipped* #:*lsp-watched-most* #:*lsp-most-changes* #:*lsp-event-length*
           #:*lsp-sync-delay* #:*lsp-idle-delay* #:*lsp-hover-echo* #:*lsp-highlight-symbol* #:*lsp-code-lenses* #:*lsp-outline*
           #:*lsp-diagnostics-display* #:*lsp-diagnostics-style* #:*lsp-semantic-tokens* #:*lsp-semantic-faces* #:*format-on-save*
           #:*format-in-sandbox* #:*format-seconds* #:*lsp-location-files-total*
           ;; file-local variables, snippets, bookmarks and version control
           #:*file-local-variables* #:*snippet-directories* #:*snippet-tab* #:*bookmarks-kept* #:*bookmark-set-asks-categories* #:*git-log-length*
           #:*git-recent-commits* #:*git-menus* #:*git-slim-actions* #:*git-diff-lines* #:*git-section-lines* #:*git-watch-seconds*
           ;; rimu, the lists of directories
           #:*rimu-details* #:*rimu-hidden-columns* #:*rimu-permissions* #:*rimu-dotfiles* #:*rimu-git-info* #:*rimu-directory-sizes* #:*rimu-sort*
           #:*rimu-directories-first* #:*rimu-ignored-extensions*
           #:*rimu-reuse-lists* #:*rimu-preview* #:*rimu-preview-delay* #:*rimu-preview-size* #:*rimu-follow-changes*
           ;; the directory of a buffer with no file, and what makes a project
           #:*default-directory* #:*project-root-markers* #:*compile-command* #:*compile-output-most* #:*last-used-first* #:*complete-paths* #:*complete-paths-modes*
           ;; Org: notes, the agenda, capture, code files and dynamic blocks
           #:*org-startup-indented* #:*org-bullets* #:*org-list-bullets* #:*org-checkbox-signs*
           #:*org-complete-checkboxes* #:*org-return-cycles-checkboxes* #:*org-return-cycles-todo* #:*org-log-checkboxes*
           #:*org-standard-todo-keywords* #:*org-todo-keywords-not-cycled* #:*org-todo-keyword-faces*
           #:*org-todo-follows-checkboxes* #:*org-cycle-hide-drawers* #:*org-fold-metadata* #:*org-hide-leading-stars*
           #:*org-cycle-include-plain-lists* #:*org-return-follows-link* #:*org-src-languages*
           #:*code-files-make-folders* #:*org-link-descriptive* #:*org-clock-into-drawer* #:*org-log-done*
           #:*org-log-into-drawer* #:*org-log-repeat* #:*org-log-reschedule* #:*org-log-redeadline*
           #:*org-agenda-files* #:*capture-template-directories* #:*org-refile-targets*
           #:*org-deadline-warning-days* #:*org-agenda-log-mode* #:*org-agenda-span*
           #:*org-agenda-start-on-weekday* #:*org-agenda-time-grid* #:*org-agenda-category-width* #:*org-agenda-untimed-text* #:*org-agenda-event-kind-text* #:*org-agenda-task-kind-text* #:*org-agenda-date-in-title* #:*org-agenda-current-time-string*
           #:*org-agenda-custom-commands* #:*org-tags-column* #:*org-agenda-autosave-files* #:*org-agenda-refresh-minutes*
           #:*org-notify-tag* #:*org-notify-before* #:*org-notify-day-time* #:*notify-program*
           #:*org-toc-on-save* #:*org-read-date-popup-calendar* #:*org-read-date-in-calendar* #:*org-toc-max-depth* #:*org-toc-style* #:*org-deepest* #:*org-formula-deepest*
           #:define-dblock-writer #:define-clocktable-formatter
           ;; Markdown
           #:*markdown-toc-on-save*
           ;; the calendar and the journal
           #:*calendar-week-start-day* #:*calendar-months* #:*calendar-months-per-row* #:*calendar-mark-days*
           #:*journal-directory* #:*journal-keyword* #:*journal-title-format* #:*journal-interval*
           #:*journal-file-type* #:*journal-file-name* #:*journal-org-header*
           ;; the terminal
           #:*kitty-keyboard* #:*terminal-editor-keys* #:*terminal-paste-keys* #:*terminal-title-max*
           #:*terminal-compress-after* #:*terminal-picture-memory* #:*terminal-place* #:*terminal-size*
           #:*terminal-width* #:*terminal-program* #:*terminal-scrollback*
           ;; marks, places kept, input methods, help windows and Escape
           #:*help-window-select* #:*insert-file-limit* #:*latex-import-picture-limit* #:*escape-quits* #:*save-place-mode*
           #:*save-place-limit* #:*default-input-method* #:*input-method-keys* #:*mark-ring-max*
           #:*global-mark-ring-max* #:*set-mark-command-repeat-pop*
           ;; the sandbox programs decoding files run in
           #:*sandbox-program* #:*sandbox-seconds* #:*sandbox-memory* #:*sandbox-input-limit* #:*program-output-limit* #:*deepest-folder-tree* #:*office-note-largest* #:*office-value-longest* #:*docx-read-most-stand-ins* #:*sandbox-total-memory* #:*sandbox-seccomp* #:*magick-largest-side* #:*picture-memory* #:*pdf-view-dark-pages* #:*office-view-pages* #:*user-full-name*
           #:*open-externally-program* #:*open-externally-programs* #:*hex-view-binary-files* #:*open-externally-kinds* #:*open-externally-ask* #:*rimu-delete-to-trash* #:*latex-engines* #:*latex-main-font* #:*latex-sans-font* #:*latex-mono-font* #:*latex-redaction-commands* #:*export-trusted-folders* #:*logos-directory* #:*org-macro-limit*
           ;; the cursor, and its look in each of the modal states
           #:*cursor-shape* #:*cursor-colour*
           #:*tuatara-normal-cursor-shape* #:*tuatara-normal-cursor-colour* #:*tuatara-insert-cursor-shape*
           #:*tuatara-insert-cursor-colour* #:*tuatara-support-cursor-shape* #:*tuatara-support-cursor-colour*
           ;; where buffers are shown, and window sizes
           #:place-buffers #:*split-height-threshold* #:*split-width-threshold*
           #:*window-min-height* #:*window-min-width* #:*scroll-margin* #:*scroll-recenter*
           ;; files with formatted names, searching files
           #:set-formatted-file-header
           #:*formatted-file-directory* #:*formatted-file-extension* #:*formatted-file-keywords* #:*formatted-file-sort-keywords* #:*formatted-file-prompts* #:*formatted-file-type*
           #:*formatted-file-types* #:*search-files-min-input* #:*search-files-limit* #:*indexed-directories* #:*processing-indicator* #:*indexing-indicator* #:*searching-indicator* #:*indexes-kept-on-disk* #:*preview-files* #:*preview-formatted-files* #:*preview-formatted-search* #:*preview-file-search* #:*preview-buffers*
           #:*search-files-max-lines* #:*search-filter-screen-seconds* #:*typing-pause* #:*typing-pause-octets* #:*line-record-modes*
           ;; the kill ring and the system clipboard
           #:*kill-ring-max* #:*kill-ring-max-size* #:*undo-limit* #:*use-system-clipboard* #:*clipboard-max-size*
           #:*show-completion-count* #:*prompt-above-candidates* #:*key-hint-rows* #:*fill-column* #:*leader-key* #:*sentence-end-double-space* #:*extra-sentence-abbreviations* #:*removed-sentence-abbreviations* #:*git-commit-fill-column* #:*fill-paragraph-limit* #:*fill-ask-column* #:*global-hl-line-mode* #:*global-soft-wrap-mode* #:*soft-wrap-modes* #:*centred-text-width* #:*centred-text-modes* #:*soft-wrap-at-fill-column* #:*soft-wrap-indicator* #:*global-visual-line-mode* #:*visual-line-modes*
           #:*hl-line-modes* #:*global-cursor-column-mode* #:*cursor-column-modes* #:*global-spellcheck-mode* #:*spellcheck-modes* #:*spellcheck-languages* #:*spellcheck-excluded-faces* #:*spellcheck-comment-faces* #:*spellcheck-string-faces* #:*global-centred-text-mode* #:*line-numbers-modes* #:*global-invisible-characters-mode* #:*invisible-characters-modes* #:*invisible-characters-when-safe*
           ;; files visited lately
           #:*remember-recent-files* #:*recent-files-max* #:*recent-files-auto-cleanup* #:*relative-age-limit*
           ;; pairs of delimiters
           #:*pair-delimiters* #:*electric-pair-mode* #:*electric-pair-typing* #:*electric-pair-safe-exclusions*
           ;; cursors
           #:*cursors-limit* #:*modal-editing* #:*mode-line-tuatara* #:*rectangle-cursors-limit* #:*split-limit-ask* #:*split-limit* #:*tuatara-normal-tab-modes*
           ;; the window frontend
           #:*gui-font* #:*gui-renderer* #:*gui-title* #:*gui-padding* #:*window-colours-from-desktop* #:*tui-padding* #:*tui-pictures* #:*tui-mouse* #:*tui-colours* #:*follow-desktop-style* #:*open-dropped-files-safely*
           #:*safe-directories* #:*open-binary-files-safely*))
