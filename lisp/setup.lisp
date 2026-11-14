;;;; lisp/setup.lisp --- Installing the defaults into an editor.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.defaults)

(defparameter *global-bindings*
  '(("C-x C-f" find-file) ("C-x C-s" save-buffer)
    ("C-x s" save-some-buffers)
    ("C-x C-w" write-file)
    ("C-x C-c" save-buffers-kill-editor)
    ("C-x C-q" read-only-mode)
    ("C-x C-r" find-recent-file) ("C-x x b" switch-other-buffer)
    ("C-x x n" narrow-to-region) ("C-x x s" narrow-to-section) ("C-x x w" widen) ("C-x j" list-jobs) ("C-x J" stop-job)
    ;; windows swapped, rotated and turned, beside the C-x 0 1 2 3 o ^ { } + - keys
    ("C-x w h" swap-window-left)
    ("C-x w j" swap-window-below)
    ("C-x w k" swap-window-above)
    ("C-x w l" swap-window-right)
    ("C-x w x" window-swap-states)
    ("C-x w r" rotate-windows)
    ("C-x w R" rotate-windows-back)
    ("C-x w t" transpose-window-layout)
    ("C-x w ." rotate-window-layout-clockwise) ("C-x w ," rotate-window-layout-anticlockwise)
    ;; notes with formatted names, the journal and the calendar
    ("C-x n f" find-file-formatted)
    ("C-x n o" open-file-formatted)
    ("C-x n n" new-file-formatted)
    ("C-x n g" search-in-formatted)
    ("C-x n r" rename-formatted) ("C-x n k" rename-formatted-keywords) ("C-x n l" match-formatted-files-link)
    ("C-x n j" journal-new-or-existing-entry)
    ("C-x n a" calendar)
    ("C-x n A" calendar-here)
    ("C-x n e" new-empty-buffer)
    ;; Org's agenda, capture and links and the clock, from any buffer
    ("C-x a a" org-agenda)
    ("C-x a w" org-agenda-week-view)
    ("C-x a m" org-agenda-month-view)
    ("C-x a y" org-agenda-year-view)
    ("C-x a c" org-capture) ("C-x a l" org-store-link) ("C-x a o" org-clock-out) ("C-x a j" org-clock-goto)
    ;; what the display shows
    ("C-x T h" hl-line-mode) ("C-x T c" cursor-column-mode) ("C-x T l" soft-wrap-mode)
    ("C-x T v" display-fill-column-mode)
    ("C-x T o" centred-text-mode)
    ("C-x T i" invisible-characters-mode)
    ("C-x T p" inline-pictures-mode)
    ("C-x T s" search-toggle-highlight)
    ("C-x T t" toggle-mode-line-tuatara)
    ;; characters and input methods
    ("C-x 8 &" insert-digraph)
    ("C-x 8 ." digraph-input-mode)
    ("C-x 8 -" deactivate-input-method)
    ("C-\\" toggle-input-method)
    ;; keys recorded and played back
    ("C-x (" start-recording-keys) ("C-x )" stop-recording-keys) ("C-x e" play-recorded-keys)
    ("C-x b" switch-to-buffer)
    ("C-x k" kill-buffer)
    ("C-x u" undo)
    ("M-(" insert-pair)
    ("M-;" comment-dwim)
    ("M-x" execute-extended-command) ("M-:" eval-expression) ("M-g g" goto-line) ("M-g M-g" goto-line)
    ("C-f" forward-char) ("C-b" backward-char) ("C-n" next-line) ("C-p" previous-line)
    ("<right>" forward-char) ("<left>" backward-char) ("<down>" next-line) ("<up>" previous-line)
    ("C-a" beginning-of-line) ("C-e" end-of-line) ("<home>" beginning-of-line) ("<end>" end-of-line)
    ("M-<" beginning-of-buffer) ("M->" end-of-buffer) ("M-f" forward-word) ("M-b" backward-word)
    ("M-}" forward-paragraph) ("M-{" backward-paragraph) ("M-e" forward-sentence) ("M-a" backward-sentence)
    ("M-@" mark-word) ("M-h" mark-paragraph)
    ("C-v" scroll-up-command) ("M-v" scroll-down-command) ("<next>" scroll-up-command) ("<prior>" scroll-down-command)
    ("C-l" recenter)
    ("RET" newline)
    ("C-j" newline)
    ("TAB" indent-for-tab-command)
    ("C-z TAB" tab-to-tab-stop)
    ("C-q" quoted-insert)
    ("DEL" delete-backward-char)
    ("<delete>" delete-char)
    ("C-d" delete-char) ("C-k" kill-line) ("C-y" yank) ("C-w" kill-region) ("M-w" kill-ring-save)
    ("C-SPC" set-mark-command) ("C-x C-SPC" pop-global-mark) ("C-x C-x" exchange-point-and-mark) ("C-x z" repeat)
    ("C-x r SPC" rectangle-mark-mode) ("C-x r k" kill-rectangle) ("C-x r M-w" copy-rectangle-as-kill)
    ("C-x r d" delete-rectangle) ("C-x r y" yank-rectangle) ("C-x r t" string-rectangle)
    ("C-x r c" clear-rectangle) ("C-x r N" number-rectangle-lines)
    ("C-x r s" register-copy) ("C-x r p" register-point) ("C-x r n" register-number)
    ("C-x r +" register-add) ("C-x r i" register-insert) ("C-x r j" register-jump) ("C-x r l" register-list)
    ("C-/" undo) ("C-_" undo) ("M-_" redo) ("C-?" redo) ("C-M-_" redo) ("C-g" keyboard-quit) ("C-u" universal-argument)
    ("M-^" join-line) ("M-\\" delete-horizontal-space) ("M-SPC" cycle-spacing) ("C-o" open-line)
    ("M-m" back-to-indentation) ("C-x C-o" delete-blank-lines)
    ("C-t" transpose-chars) ("M-t" transpose-words) ("C-x C-t" transpose-lines) ("C-M-t" transpose-sexps)
    ("C-S-<backspace>" kill-whole-line)
    ("M-z" zap-up-to-char)
    ("M-/" expand-word)
    ("C-M-/" complete-word)
    ("M-=" count-words)
    ("C-M-f" forward-sexp) ("C-M-b" backward-sexp) ("C-x h" mark-whole-buffer)
    ;; the syntax tree: grow, shrink and step the selection (the keys of C-z m select by it)
    ("M-o" expand-selection) ("M-i" shrink-selection) ("M-n" select-next-sibling) ("M-p" select-previous-sibling)
    ("C-M-n" forward-list) ("C-M-p" backward-list) ("C-M-u" backward-up-list) ("C-M-d" down-list)
    ("C-M-a" beginning-of-defun) ("C-M-e" end-of-defun) ("C-M-SPC" mark-sexp) ("C-M-@" mark-sexp)
    ("C-M-DEL" backward-kill-sexp)
    ("C-s" regex-search-forward) ("C-r" regex-search-backward)
    ("C-x / s" search-filter-regex) ("C-x / l" search-filter-list) ("C-x / r" search-filter-remove-all)
    ("C-x / f" search-filter-show-records) ("C-x / h" search-filter-hide-records) ("C-x / k" list-keep-records)
    ("C-x / d" list-delete-records) ("C-x / t" list-toggle-header) ("C-x / L" list-toggle-header-lock)
    ("C-M-s" literal-search-forward)
    ("C-M-r" literal-search-backward)
    ("M-%" query-replace)
    ("M-s g" search-in-files) ("M-q" fill-paragraph) ("C-M-\\" indent-region)
    ("C-x f" set-fill-column) ("C-x i" insert-formatted-note-template)
    ("M-u" upcase-word) ("M-l" downcase-word) ("M-c" capitalize-word)
    ("C-x C-u" upcase-region) ("C-x C-l" downcase-region)
    ("<wheel-down>" scroll-by-wheel) ("<wheel-up>" scroll-by-wheel)
    ("<down-mouse-1>" mouse-set-point)
    ("<drag-mouse-1>" mouse-drag-region)
    ("<up-mouse-1>" mouse-release)
    ("<mouse-2>" mouse-yank-primary)
    ("<double-mouse-1>" mouse-select-word) ("<triple-mouse-1>" mouse-select-line))
  "The standard bindings, active in every modal state.")

(defun syntax-highlights-for-window (editor window rows)
  "Syntax colours from the major mode of WINDOW's buffer for ROWS."
  (declare (ignore editor))
  (let ((buffer (tuatara.display:window-buffer window)))
    (mode-syntax-highlights (buffer-major-mode buffer) buffer rows)))

(defun mode-line-extra-for-buffer (editor buffer)
  "The major mode's addition to the mode line for BUFFER."
  (declare (ignore editor))
  (mode-line-extra (buffer-major-mode buffer) buffer))

(defun mode-marks-point-of-buffer (buffer)
  "True when the major mode of BUFFER marks point in a look of its own."
  (mode-marks-point-p (buffer-major-mode buffer)))

(defun leave-mode-on-kill (buffer)
  "Let BUFFER's major mode release what it attached before the buffer is killed."
  (mode-exit (buffer-major-mode buffer) buffer))

(defun install-modes ()
  "Choose major modes for visited files and let modes colour text and extend the mode line;
bind \\`C-M-i' to complete the word at point in every programming language's buffers.
Idempotent: the hooks are named functions."
  (pushnew 'set-auto-mode tuatara.buffer:*find-file-functions*)
  (pushnew 'leave-mode-on-kill tuatara.buffer:*kill-buffer-functions*)
  (pushnew 'syntax-highlights-for-window *syntax-highlight-functions*)
  (pushnew 'mode-line-extra-for-buffer *mode-line-functions*)
  (pushnew 'mode-marks-point-of-buffer *point-marked-functions*)
  (let ((map (mode-keymap 'prog-mode)))
    ;; C-M-i reaches a terminal as M-TAB
    (define-key map "C-M-i" 'completion-at-point)
    (define-key map "M-TAB" 'completion-at-point)))

(defun keymaps-for-keys ()
  "The keymaps a command's keys named in help and messages are looked for in,
in order: the global keymap, then the NORMAL state's keymap (its leader under
\\`SPC'). A major mode's keys are named only where the text names its keymap
first (\\=\\<buffers-mode>), as they work only in its buffers."
  (list *global-keymap* *tuatara-normal-keymap*))

(defmethod host-key-keymaps ((host defaults-host))
  "The keymaps a command's keys are named from (KEYMAPS-FOR-KEYS), once the
defaults are installed in an editor; before, as the core has them."
  (if *global-keymap* (keymaps-for-keys) (call-next-method)))

(defmethod host-keymap-named ((host defaults-host) name)
  "The keymap called NAME in help and messages: one bind-key names (normal,
leader, minibuffer, popup, as KEYMAP-REGISTRY has them), a variable of the editor's
holding one (*tuatara-normal-keymap*), else a major mode's (buffers-mode); NIL
when there is none."
  (or (cdr (assoc name (keymap-registry) :test (lambda (name key) (string-equal name (symbol-name key)))))
      (loop for package in (editor-packages)
            for symbol = (find-symbol (string-upcase name) package)
            when (and symbol (boundp symbol) (keymap-p (symbol-value symbol)))
              return (symbol-value symbol))
      (let ((mode (find (string-upcase name) (mode-names) :key #'symbol-name :test #'string=)))
        (and mode (mode-keymap mode)))))

(defun after-init-step (what function)
  "Call FUNCTION, a step of starting the editor that follows the init file's
settings; when it fails, say in *Messages* that WHAT failed and why, and go
on, so that a setting written wrong never stops the editor starting."
  (handler-case (reporting-errors ("~a" what) (funcall function))
    (error (e)
      (message "~a failed (check the init file's settings): ~a" what e)
      nil)))

(defparameter *global-prefix-labels*
  '(("C-x n" "notes") ("C-x a" "agenda")
    ("C-x T" "toggles")
    ("C-x w" "windows")
    ("C-z" "modal editing keys")
    ("C-x x" "buffers")
    ("C-x 8" "characters")
    ("C-x r" "rectangles and registers")
    ("C-x m" "bookmarks")
    ("C-x p" "project")
    ("C-x 4" "other window")
    ("C-x 5" "other frame")
    ("C-x RET" "input method")
    ("C-x /" "filters"))
  "What the key hints call the prefixes of the standard keys.")

;;; C-c in every mode

(defparameter *context-keys-left-free* '("x" "s" "t")
  "The keys after \\`C-c' never given a mode's command, left for your own (C-c x).")

(defparameter *context-keys-kept-from* '(tuatara.terminal-emulator:terminal-mode)
  "The modes whose keys are not put under \\`C-c': the terminal, where C-c goes to the program.")

(defun leave-context-key-to-program (name)
  "Take away, in the major mode called NAME, the keys of one character its
parent modes put under \\`C-c', so that \\`C-c' there goes to the program it runs."
  (let* ((keymap (mode-keymap name))
         (prefix (and (keymap-parent keymap)
                      (lookup-key (keymap-parent keymap) (tuatara.frontend:parse-key-sequence "C-c")))))
    (when (keymap-p prefix)
      (loop for (spec) in (keymap-entries prefix)
            for key = (tuatara.frontend:key-description spec)
            when (= 1 (length key))
              do (define-key keymap (format nil "C-c ~a" key) :undefined)))))

(defun mirror-mode-keys (name)
  "Put the keys of one character the major mode called NAME binds itself (a
list's h, r, q) under \\`C-c' too, and take away there those it takes away;
those under \\`C-c' already, and *CONTEXT-KEYS-LEFT-FREE*, left alone. A
parent mode's keys are under its own \\`C-c', which the mode's falls through to."
  (let* ((keymap (mode-keymap name))
         (inherited (and (keymap-parent keymap) (keymap-entries (keymap-parent keymap)))))
    (loop for (spec . binding) in (keymap-entries keymap)
          for key = (tuatara.frontend:key-description spec)
          for under = (format nil "C-c ~a" key)
          when (and (= 1 (length key)) (graphic-char-p (char key 0)) (char/= (char key 0) #\Space)
                    (symbolp binding) binding
                    (not (member key *context-keys-left-free* :test #'string=))
                    (not (eq binding (cdr (assoc spec inherited :test #'equal)))))
            do (let ((there (lookup-key keymap (tuatara.frontend:parse-key-sequence under))))
                 (cond ((not (eq binding :undefined)) (unless there (define-key keymap under binding)))
                       ((and there (not (eq there :undefined))) (define-key keymap under :undefined)))))))

(defun mirror-context-keys ()
  "Put every mode's keys of one character (a list's h, r, q) under \\`C-c' too,
so that \\`C-c' shows, and runs, what the buffer at hand can do in every mode
\(MIRROR-MODE-KEYS); the modes of *CONTEXT-KEYS-KEPT-FROM* have none there."
  (dolist (name (tuatara.modes:mode-names))
    (if (member name *context-keys-kept-from*)
        (leave-context-key-to-program name)
        (mirror-mode-keys name))))

(defun install-defaults (editor)
  "Install the standard bindings and the modal editing into EDITOR. Pass as :SETUP to RUN-EDITOR."
  (let ((global (editor-global-keymap editor)))
    (setf *global-keymap* global)
    (define-keys global *global-bindings*)
    (label-keys global *global-prefix-labels*)
    (setf (editor-self-insert-command editor) 'self-insert-command)
    (install-pairs)
    (install-input-methods editor)
    (install-characters editor)
    (install-search-keys)
    (install-recorded-keys)
    (define-repeat-keys '(("z" repeat)))
    (install-search-filters)
    (install-rewrites)
    (install-jobs)
    (install-files)
    (install-narrowing)
    (install-list-marks)
    (install-recent-files)
    (install-auto-save)
    (install-session)
    (install-encrypted)
    (tuatara.lsp:install-lsp editor)
    (tuatara.git:install-git)
    (install-conflicts editor)
    (install-gpg-commands)
    (install-places)
    (install-bookmarks)
    (install-registers)
    (tuatara.packages:install-packages)
    (install-servers)
    (install-remote-files)
    (install-file-local-variables)
    (install-snippets editor)
    (install-paths-in-text editor)
    (install-next-place)
    (tuatara.compile:install-compile)
    (install-search-results)
    (install-state-lists)
    (install-indexes)
    (install-sorting)
    (install-hex)
    (install-modes)
    (install-colouring)
    (install-lisp-mode editor)
    (install-show-paren)
    (install-xref)
    (install-backtrace)
    (install-ielm)
    (install-macroexpand)
    (install-csv)
    (install-latex)
    (install-special-buffers)
    (install-entry-previews editor)
    (install-help)
    (install-faces-help)
    (install-about)
    (install-char-help)
    (install-debug)
    (install-notifications)
    (install-man)
    (install-outline)
    (install-buffers)
    (install-rimu editor)
    (install-pdf-view)
    (tuatara.terminal-emulator:install-terminal)
    (install-office-view)
    (install-metadata)
    (install-open-externally)
    (install-image-view)
    (install-org-pictures)
    (install-org-folding editor)
    (install-org-parts)
    (install-org-editing)
    (install-org-links editor) (install-markdown-links) (install-linking) (install-org-clock) (install-org-schedule)
    (install-org-items)
    (install-org-tables)
    (install-org-formulas)
    (install-code-files)
    (install-org-agenda-view editor)
    (install-org-notify)
    (install-display-toggles)
    (install-hl-line)
    (install-cursor-column)
    (install-spellcheck)
    (install-soft-wrap)
    (install-centred-text)
    (install-display-fill-column)
    (install-windows)
    (install-frames)
    (tuatara.export:install-latex-export)
    ;; after the windows' standard rules, which it adds to
    (install-calendar)
    (tuatara.lsp:install-diagnostics-place)
    (install-journal)
    (install-font)
    (install-font-list)
    (install-line-numbers)
    (install-vc-gutter editor)
    (install-tuatara-modal editor)
    ;; every mode's keys of one character under C-c too, once all are bound
    (mirror-context-keys)
    (tuatara.display:apply-theme *startup-theme*)
    (setf *theme-chosen* nil *style-followed* nil)
    (load-user-config)
    ;; the logs in the cache directory the init file chose
    (after-init-step "Moving the logs to *cache-directory*"
                     (lambda ()
                       ;; each directory the init file named looked at now, so that one not there is said at once
                       (when *cache-directory*
                         (ignoring-errors-reported ("making the cache directory") (tuatara.io:cache-directory)))
                       (when *state-directory*
                         (ignoring-errors-reported ("making the state directory") (tuatara.io:state-directory)))
                       (when *packages-directory*
                         (ignoring-errors-reported ("making the packages directory")
                           (tuatara.packages:packages-directory)))
                       (let ((words (tuatara.io:directories-in-memory-words)))
                         (when words (message "~{~a~^; ~}" words)))
                       (tuatara.io:move-session-log)
                       (unless tuatara.runtime:*debug-log-path*
                         (setf tuatara.runtime:*debug-log-path*
                               (handler-case (namestring (merge-pathnames "debug.log" (tuatara.io:cache-directory)))
                                 ((or file-error sb-posix:syscall-error) () nil))))))
    ;; debug mode, when the init file asks for it
    (after-init-step "Debug mode" (lambda () (if *debug-mode* (switch-debug-mode t) (tuatara.runtime:set-debug nil))))
    ;; after the init file, which may say which to make
    (after-init-step "Making the scratch buffers" #'ensure-scratch-buffers)
    ;; the patterns saved under a name, but those the init file names itself
    (after-init-step "Naming the saved patterns" #'define-saved-patterns)
    ;; the keys of modes the init file's packages brought, under C-c too
    (after-init-step "Putting the modes' keys under C-c" #'mirror-context-keys)
    ;; the list of recent files loses the files no longer there, unless the init file said not to
    (after-init-step "Cleaning up the recent files" (lambda () (start-recent-files-cleanup editor)))
    ;; the window's theme after the desktop's light or dark style, unless the init file chose one
    (after-init-step "Following the desktop's style" (lambda () (follow-desktop-style editor)))
    (pushnew 'desktop-style-tick *tick-functions*)
    (pushnew 'start-up-garbage-tick *tick-functions*)
    (pushnew 'idle-garbage-tick *tick-functions*)
    ;; pairs the init file added are keys that wrap, too
    (after-init-step "Binding the wrap keys of *pair-delimiters*" #'tuatara-normal-claim-wrap-keys)
    ;; the leader's keys on the key the init file chose
    (after-init-step "Putting the leader's keys on *leader-key*" (lambda () (put-leader-on *leader-key*)))
    ;; the input methods the init file put under the leader's i
    (after-init-step "Binding *input-method-keys*" #'install-input-method-keys)
    ;; colours would not show: use the colourless theme whatever was chosen
    (after-init-step "Choosing the colourless theme"
                     (lambda ()
                       ;; the colours the init file asked the frontend for first
                       (tuatara.frontend:frontend-settings-changed (editor-frontend editor))
                       (when (< (tuatara.frontend:frontend-colour-count (editor-frontend editor)) 8)
                         (tuatara.display:apply-theme *colourless-theme*))))
    editor))
