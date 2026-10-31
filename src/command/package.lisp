;;;; src/command/package.lisp --- Package definition for tuatara.command.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.command
  (:use #:cl #:alexandria)
  (:import-from #:tuatara-user #:*cursor-shape* #:*cursor-colour* #:message #:current-buffer #:*message-log-max* #:*mode-line-name-max* #:*gui-title* #:place-buffers #:*split-height-threshold* #:*split-width-threshold*
                #:*show-completion-count* #:*prompt-above-candidates* #:*key-hint-rows* #:*debug-on-error*)
  (:import-from #:tuatara.buffer #:live-buffer-p)
  (:documentation "Events, keymaps and key-sequence reading in precedence order, interactive calls, hooks, the minibuffer core, the modal state slot.")
  (:export #:escape-controls)
  (:shadow #:quit)
  (:export #:keymap #:keymap-p #:make-keymap #:define-key #:define-keys #:label-keys #:lookup-key #:keymaps-lookup #:map-keymap-bindings #:keymap-parent #:keymap-entries #:keymap-name #:unbind-commands-if
           #:define-command #:find-command #:command-symbol #:command-names #:*commands* #:forget-command
           #:quit #:keyboard-quit-signal #:editor #:editor-p #:make-editor #:*editor* #:current-buffer #:*current-buffer* #:*command-refusal-function* #:current-window
           #:message #:say-with-command-messages #:*messages-kept* #:*exit-functions* #:*startup-buffers* #:*exit-code* #:cursor-look #:*cursor-colour-functions* #:echo #:*message-log-max* #:log-message #:add-to-log #:messages-buffer #:*messages-made-function* #:read-string #:finish-minibuffer #:minibuffer-previous-history #:minibuffer-next-history #:echo-question-p #:yes-or-no-p* #:read-password #:wipe-string #:*minibuffer-hidden* #:show-buffer #:run-editor #:*visit-file-function* #:command-loop #:redisplay #:redisplay-or-say
           #:quit-key-p #:*keys-as-typed* #:wait-interruptibly #:wait-until-interruptibly #:wait-while-interruptibly #:editor-unread #:editor-deferred #:running-editor #:defer-action #:away-from-editor-p #:wake-editor
           #:editor-frontend #:editor-window #:editor-global-keymap #:editor-state-keymap #:editor-minibuffer-keymap
           #:editor-message #:editor-kill-ring #:editor-running #:editor-last-command #:editor-this-command
           #:editor-prefix-argument #:prefix-argument #:editor-mode-line-function #:editor-cursor-shape-function
           #:editor-post-command-functions #:buffer-directory #:editor-minibuffer-active #:editor-pending
           #:editor-this-key #:editor-self-insert-command #:minibuffer-insert #:replace-minibuffer-text #:*minibuffer-question-keymap* #:editor-collecting-prefix #:universal-argument
           #:editor-fallback-keymap #:*mode-line-functions* #:*mode-line-lead-functions* #:*mode-line-name-functions* #:*startup-buffer-function* #:*line-highlight-functions*
           #:resize #:run-command #:handle-key #:default-mode-line
           #:candidate #:make-candidate #:candidate-string #:candidate-annotation #:candidate-value #:candidate-result
           #:candidate-group #:candidate-columns #:candidate-suffix #:candidate-rank #:group-candidates #:match-spans #:join-fields #:fit-fields #:fields-columns #:panel-columns
           #:filter-candidates #:component-match #:first-line #:first-sentence #:where-is #:prefix-entries #:set-key-label #:key-label
           ;; keys named in text
           #:substitute-command-keys #:unmark-keys #:text-spans #:mark-key #:key-mark-p #:key-description< #:command-key
           #:keymap-called #:keymap-summary #:valid-key-text-p #:*key-keymaps-function* #:*keymap-named-function*
           #:completing-read #:completing-read-value #:key-waiting-p #:completion #:editor-completion #:editor-key-hints #:editor-width #:editor-height
           #:active-keymaps #:*state-keys-function* #:*prefix-help-function* #:resolve-keys #:candidate-score #:panel-rows #:*highlight-functions* #:*key-hint-focus-functions* #:*job-highlight-functions* #:*syntax-highlight-functions* #:*text-highlight-functions* #:window-highlights-for #:window-syntax #:read-key #:read-answer-key #:read-event #:*tick-functions* #:*last-input-time* #:*after-tick-functions* #:*title-functions* #:*background-work* #:*tick-interval* #:run-tick-functions
           #:*gutter-providers* #:window-margin-for #:gutter-cells #:*ruler-functions*
           #:editor-minibuffer-prompt #:editor-minibuffer-text #:editor-minibuffer-point
           ;; windows
           #:editor-windows #:buffer-window #:buffer-windows #:editor-selections #:one-window-p #:fit-windows #:select-window #:split-window
           #:remove-window #:only-window #:swap-windows #:rotate-window-states #:window-towards
           #:window-cursor-cell #:window-at-cell #:window-text-cell #:window-cell-position #:most-recent-window
           #:place-buffers #:*buffer-places* #:*places* #:buffer-place #:buffer-place-p #:buffer-place-which
           #:buffer-place-place #:buffer-place-size #:buffer-place-keep #:buffer-place-select
           #:buffer-place-for #:buffer-place-fits-p #:buffer-mode-named-p #:display-buffer #:display-buffer-at #:pop-to-buffer #:leave-window
           #:replacement-buffer #:new-buffer #:*new-buffer-function* #:abbreviated-path #:other-buffer #:buffers-in-use-order #:note-selected-buffer #:*buffer-order* #:live-buffer-p #:*split-height-threshold* #:*split-width-threshold*
           #:*window-divider*
           ;; repeating
           #:define-repeat-keys #:*repeat-maps* #:*not-repeatable* #:editor-repeatable #:editor-repeat-map
           ;; recorded keys
           #:start-keys-recording #:stop-keys-recording #:drop-command-keys #:keys-recording-p #:keys-playing-p #:quit-while-playing-p
           #:play-keys #:note-command-failed #:*keys-play-progress*
           #:*show-completion-count* #:*prompt-above-candidates* #:completion-count-prefix #:*panel-max-rows*
           #:define-menu #:open-menu #:menu-named #:menu-entry-argument #:menu-entry-command #:menu-groups #:menu-entries #:menu-arguments #:menu-open-p #:*menu-help-function* #:editor-menu
           #:set-transient-map #:clear-transient-map #:editor-transient-map #:editor-transient-keep
           #:editor-transient-exit #:editor-pre-command-functions #:rectangle-rows
           #:rectangle-live-p #:end-rectangle #:rectangle-point-column #:keep-rectangle-column
           #:rectangle-columns #:region-rectangle-rows #:*other-cursor-functions* #:*point-marked-functions* #:*read-key-replay*
           #:*this-command-keys* #:*command-undo-since* #:*command-notes* #:editor-transient-fresh #:*dropped*
           #:*debug-on-error* #:*backtrace-function* #:*backtrace-frames* #:capture-backtrace
           ;; completing a word in the text
           #:complete-in-text #:complete-in-minibuffer #:popup-insert #:*popup-insert-functions* #:close-popup #:show-note #:close-note #:editor-note #:*note-limits* #:editor-popup #:popup-p #:popup-target #:popup-candidates #:popup-index
           #:*popup-keymap* #:*popup-chosen-keymap* #:*popup-rows* #:*popup-reach* #:editor-minibuffer-completer #:splice-row #:lay-boxes-over))
