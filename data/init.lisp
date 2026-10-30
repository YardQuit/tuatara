;;;; ~/.config/tuatara/init.lisp --- Your Tuatara settings.
;;;;
;;;; Read at every start, after the defaults, in the package TUATARA-USER.
;;;; Each setting has a paragraph above it saying what it does, and after
;;;; "Not set:" what it is when it is not set. Every setting is commented out
;;;; but the tabs some languages' own tools require: take away the ;; in front
;;;; of one to use it, from every line of it when it runs over several, or put
;;;; it in front of one of those. Tuatara wrote this file once, the first time
;;;; it ran without one; it is yours now, and never written again. An error
;;;; here is shown on the echo row, and the editor still starts.
;;;;
;;;; Settings are kept together by what they are for, each group in a
;;;; load-package with the settings and keys after its :config: "tuatara" for
;;;; Tuatara itself, a mode's name ("org", "rimu", "csv", "terminal") for that
;;;; mode, a package's name for that package, or a name of your own for a
;;;; group of settings that go together ("writing"). Keys say their mode
;;;; themselves (bind-key ... :mode 'org), whichever group they are in.

;;; Tuatara itself: what is for no one mode or package

(load-package "tuatara"
  :config

  ;;; Keys
  ;; C- is Control, M- Meta, S- Shift; C-h k shows what a key does, C-h f where a
  ;; command is.

  ;; A key of your own, in every buffer and both modal states.
  ;; (bind-key "C-c t" 'toggle-light-dark-theme)

  ;; SPC o in NORMAL state.
  ;; (bind-key "o" 'find-file :map :leader)

  ;; Only in CSV buffers.
  ;; (bind-key "C-c e" 'end-of-buffer :map 'csv-mode)

  ;; SPC m l only in Org and Markdown and the modes made from them; bound without
  ;; :mode, the same key stays as it is everywhere else.
  ;; (bind-key "m l" 'org-store-link :map :leader :mode '(org-mode markdown-mode))

  ;; The leader's keys after , in NORMAL state, the lists and the calendar, SPC
  ;; freed. Not set: "SPC".
  ;; (setf *leader-key* ",")

  ;; SPC t n, called "line numbers" in the key hints.
  ;; (bind-key "t n" '("line numbers" . toggle-line-numbers) :map :leader)

  ;; A prefix's label: SPC shows t +toggles.
  ;; (bind-key "t" nil :map :leader :label "toggles")

  ;; C-s finds every match up to 1 MB whole, across lines; slower.
  ;; (bind-key "C-s" 'regex-search-forward-thorough)

  ;; f the same; M-x replace-regexp-thorough and the rest too.
  ;; (bind-key "f" 'split-selection-thorough :map :normal)

  ;; How r r and r q show a replacement while it is typed: the text going and
  ;; the text coming, as a difference shows them, each with its own colour and
  ;; ground. Not set: the theme's (Ao: red on dark red, green on dark green).
  ;; (set-face :replace-going :fg "#fa7970" :bg "#3a1416")
  ;; (set-face :replace-coming :fg "#81be83" :bg "#12301f" :bold t)

  ;; A key no longer bound.
  ;; (unbind-key "C-c t")

  ;; The leader, SPC in NORMAL state, is yours alone and empty until you give it
  ;; keys: take out the ";; " before those you want. Every command also has a key
  ;; of its own under C-x, C-c or M-g (C-h f names it).
  ;; (bind-key "," 'switch-to-buffer :map :leader)
  ;; (bind-key "b" 'switch-other-buffer :map :leader)
  ;; (bind-key "I" 'buffers :map :leader)
  ;; (bind-key "N" 'new-empty-buffer :map :leader)
  ;; (bind-key "c" 'calendar :map :leader)
  ;; (bind-key "C" 'calendar-here :map :leader)
  ;; (bind-key "f" 'find-file-formatted :map :leader)
  ;; (bind-key "r" 'find-recent-file :map :leader)
  ;; (bind-key ";" 'buffer-outline :map :leader)
  ;; (bind-key "d" nil :map :leader :label "formatted/rimu")
  ;; (bind-key "d f" 'open-file-formatted :map :leader)
  ;; (bind-key "d n" 'new-file-formatted :map :leader)
  ;; (bind-key "d g" 'search-in-formatted :map :leader)
  ;; (bind-key "d d" 'rimu-jump :map :leader)
  ;; (bind-key "d j" 'journal-new-or-existing-entry :map :leader)
  ;; (bind-key "d r" 'rename-formatted :map :leader)
  ;; (bind-key "d k" 'rename-formatted-keywords :map :leader)
  ;; (bind-key "d G" 'match-formatted-files-link :map :leader)
  ;; (bind-key "SPC" 'other-window :map :leader)
  ;; (bind-key "g" 'other-window :map :leader)
  ;; (bind-key "H" 'select-window-left :map :leader)
  ;; (bind-key "J" 'select-window-below :map :leader)
  ;; (bind-key "K" 'select-window-above :map :leader)
  ;; (bind-key "L" 'select-window-right :map :leader)
  ;; (bind-key "w" nil :map :leader :label "window")
  ;; (bind-key "w h" 'swap-window-left :map :leader)
  ;; (bind-key "w j" 'swap-window-below :map :leader)
  ;; (bind-key "w k" 'swap-window-above :map :leader)
  ;; (bind-key "w l" 'swap-window-right :map :leader)
  ;; (bind-key "w s" 'split-window-below :map :leader)
  ;; (bind-key "w v" 'split-window-right :map :leader)
  ;; (bind-key "w q" 'delete-window :map :leader)
  ;; (bind-key "w o" 'delete-other-windows :map :leader)
  ;; (bind-key "w w" 'other-window :map :leader)
  ;; (bind-key "w x" 'window-swap-states :map :leader)
  ;; (bind-key "w r" 'rotate-windows :map :leader)
  ;; (bind-key "w R" 'rotate-windows-back :map :leader)
  ;; (bind-key "w t" 'transpose-window-layout :map :leader)
  ;; (bind-key "w ." 'rotate-window-layout-clockwise :map :leader)
  ;; (bind-key "w ," 'rotate-window-layout-anticlockwise :map :leader)
  ;; (bind-key "w =" 'balance-windows :map :leader)
  ;; (bind-key "w +" 'enlarge-window :map :leader)
  ;; (bind-key "w -" 'shrink-window :map :leader)
  ;; (bind-key "w >" 'enlarge-window-horizontally :map :leader)
  ;; (bind-key "w <" 'shrink-window-horizontally :map :leader)
  ;; (bind-key "k" nil :map :leader :label "kill/window")
  ;; (bind-key "k K" 'delete-window :map :leader)
  ;; (bind-key "k k" 'kill-buffer-and-window :map :leader)
  ;; (bind-key "t" nil :map :leader :label "toggle")
  ;; (bind-key "t h" 'hl-line-mode :map :leader)
  ;; (bind-key "t |" 'cursor-column-mode :map :leader)
  ;; (bind-key "t l" 'soft-wrap-mode :map :leader)
  ;; (bind-key "t v" 'visual-line-mode :map :leader)
  ;; (bind-key "t c" 'centred-text-mode :map :leader)
  ;; (bind-key "t i" 'invisible-characters-mode :map :leader)
  ;; (bind-key "i" nil :map :leader :label "input")
  ;; (bind-key "i &" 'insert-digraph :map :leader)
  ;; (bind-key "i ." 'digraph-input-mode :map :leader)
  ;; (bind-key "i -" 'input-method-off :map :leader)

  ;;; Looks: themes, fonts, colours (M-x load-theme lists the themes)

  ;; A theme in force (ao-dark)
  ;; (load-theme 'ao-light)

  ;; No colours: bold, underline, reverse video and dim alone.
  ;; (load-theme 'plain)

  ;; A theme of your own: Ao with colours of yours for its roles.
  ;; (define-theme my-ao
  ;;   "Ao on black."
  ;;   :faces-from ao-dark
  ;;   :palette ((ground "#000000")))

  ;; And in force (the roles are in lisp/theme.lisp: ground, text, comment, blue,
  ;; selection ...)
  ;; (load-theme 'my-ao)

  ;; What M-x toggle-light-dark-theme turns to. Not set: ao-light.
  ;; (setf *light-theme* 'plain)

  ;; And back to. Not set: ao-dark.
  ;; (setf *dark-theme* 'terminal)

  ;; One theme, whatever the desktop's light or dark style. Not set: t: follows
  ;; it.
  ;; (setf *follow-desktop-style* nil)

  ;; A face of your own, kept when the theme changes.
  ;; (set-face :comment :italic t)

  ;; The window's font, tuatara --gui. Not set: the desktop's monospace font.
  ;; (setf *gui-font* "Adwaita Mono 12")

  ;; How the window is drawn: :gl, by the graphics card through OpenGL;
  ;; :vulkan; :cairo, by the processor; nil, as GTK chooses. M-x check-health,
  ;; in the window, measures which is faster here. Not set: :gl.
  ;; (setf *gui-renderer* :vulkan)

  ;; The window's title, from the buffer's name. Not set: "~a - Tuatara".
  ;; (setf *gui-title* "~a")

  ;; Pixels between the window's edges and the text; '(4 12) top and bottom,
  ;; sides. Not set: 8.
  ;; (setf *gui-padding* 12)

  ;; The terminal and plain themes in a window take the desktop's colours. Not
  ;; set: nil: black on white.
  ;; (setf *window-colours-from-desktop* t)

  ;; Cells between the terminal's edges and the text. Not set: 0.
  ;; (setf *tui-padding* 1)

  ;; No pictures in a terminal, their octets instead. Not set: :auto: where the
  ;; terminal draws them, kitty and Ghostty.
  ;; (setf *tui-pictures* nil)

  ;; In a terminal the mouse left to it, its own selection. Not set: t: as in the
  ;; window, Shift held for the terminal's.
  ;; (setf *tui-mouse* nil)

  ;; Colours in a terminal said outright, for one that does not say (COLORTERM
  ;; is not passed on by ssh): :24-bit, 256, 16, 8, or nil for none. Not set:
  ;; :auto: as the terminal says, none where NO_COLOR is set.
  ;; (setf *tui-colours* :24-bit)

  ;; Buffer names cut to 30 columns in the mode line, … in the middle. Not set:
  ;; nil: as they fit.
  ;; (setf *mode-line-name-max* 30)

  ;; The mode line's sign, first, lit while a buffer's jobs run or wait: a Nerd
  ;; Font glyph. Not set: "P"; nil: none.
  ;; (setf *processing-indicator* "")

  ;; Every buffer coloured sparsely, types and constants the most coloured,
  ;; where its mode or language says nothing. Not set: :rich, everything its
  ;; rules name.
  ;; (set-colouring t :sparse)

  ;; Every programming language, before the setting for every buffer.
  ;; (set-colouring 'prog-mode :sparse)

  ;; Rust with fewer colours: :rich, :sparse or :none.
  ;; (set-colouring 'rust :sparse)

  ;; Only comments and strings (levels 1 to 4; 3)
  ;; (set-colouring 'lisp :sparse :level 1)

  ;; CSV and TSV: separators, quoted fields and # lines, no colour a
  ;; column (:rich)
  ;; (set-colouring 'csv-mode :sparse)

  ;; Org's headlines, comments, keywords, tags and emphasis, no dates (:rich)
  ;; (set-colouring 'org-mode :sparse :level 2)

  ;; The cursor: a block in the colour of the text unless set, in the window and
  ;; the terminal alike

  ;; :block, :bar or :underline, for all. Not set: :block.
  ;; (setf *cursor-shape* :bar)

  ;; Its colour, as set-face takes colours. Not set: nil: the text's.
  ;; (setf *cursor-colour* "#dadada")

  ;; In NORMAL state. Not set: nil: *cursor-shape*.
  ;; (setf *tuatara-normal-cursor-shape* :block)

  ;; Ao's blue in NORMAL state. Not set: nil: *cursor-colour*.
  ;; (setf *tuatara-normal-cursor-colour* "#45b1e8")

  ;; In INSERT state, while typing. Not set: nil: *cursor-shape*.
  ;; (setf *tuatara-insert-cursor-shape* :bar)

  ;; Ao's red while typing. Not set: nil: *cursor-colour*.
  ;; (setf *tuatara-insert-cursor-colour* "#fa7970")

  ;; In lists, help and *Messages*. Not set: nil: *cursor-shape*.
  ;; (setf *tuatara-support-cursor-shape* :underline)

  ;; A colour: :red, a number 0-255 or "#rrggbb". Not set: nil: *cursor-colour*.
  ;; (setf *tuatara-support-cursor-colour* :green)

  ;;; Modal editing (NORMAL and INSERT states) and Escape

  ;; No NORMAL and INSERT states: each mode's keys and the global ones alone. Not
  ;; set: t.
  ;; (setf *modal-editing* nil)

  ;; The modal state alone in the mode line, [N], without TUATARA before it
  ;; (C-x T t switches it). Not set: t: TUATARA[N].
  ;; (setf *mode-line-tuatara* nil)

  ;; TAB in NORMAL state does nothing, in Org too. Not set: '(org-mode).
  ;; (setf *tuatara-normal-tab-modes* '())

  ;; Escape does what C-g does, everywhere, a terminal too. Not set: nil:
  ;; Escape is a key of its own, doing what it is bound to; Meta only when a
  ;; terminal sends Escape and another key together.
  ;; (setf *escape-quits* t)

  ;; Never the kitty keyboard protocol in a terminal. Not set: :auto: asked of the
  ;; terminal; with it Escape and Alt arrive whole.
  ;; (setf *kitty-keyboard* nil)

  ;;; Editing: tabs, indentation, pairs, filling
  ;; TAB indents where the mode indents, else goes on to the next tab stop; M-i
  ;; goes to it anywhere; C-q TAB puts a tab in

  ;; Tab stops every 8 columns: where TAB goes, how wide a tab shows. Not set: 4.
  ;; (setf *tab-width* 8)

  ;; TAB and indentation put tabs in, not spaces. Not set: nil.
  ;; (setf *indent-tabs-mode* t)

  ;; TAB indents only, never completes. Not set: :complete.
  ;; (setf *tab-always-indent* t)

  ;; For one major mode and those made from it.
  ;; (set-tab-width 'c-mode 8)

  ;; Tabs in shell scripts only (tsv-mode has them already)
  ;; (set-indent-tabs-mode 'bash t)

  ;; Tabs in LaTeX too.
  ;; (set-indent-tabs-mode 'latex t)

  ;; C indented two columns a level (4)
  ;; (set-indentation 'c 2)

  ;; tabs where a language's own tools require them, spaces everywhere else; a ;;
  ;; in front of one gives it spaces

  ;; Make: a recipe line must begin with a tab.
  (set-indent-tabs-mode 'makefile t)

  ;; Gofmt indents Go with tabs.
  (set-indent-tabs-mode 'go t)

  ;; go.mod.
  (set-indent-tabs-mode 'go-mod t)

  ;; And go.work.
  (set-indent-tabs-mode 'go-work t)

  ;; v fmt.
  (set-indent-tabs-mode 'v t)

  ;; Cue fmt.
  (set-indent-tabs-mode 'cue t)

  ;; Godot's style for GDScript, and gdformat.
  (set-indent-tabs-mode 'gdscript t)

  ;; Odinfmt.
  (set-indent-tabs-mode 'odin t)

  ;; The Linux kernel's checkpatch: Devicetree.
  (set-indent-tabs-mode 'dts t)

  ;; And Kconfig.
  (set-indent-tabs-mode 'kconfig t)

  ;; New lines at column 0 (RET, o, O). Not set: t: indented as the mode indents.
  ;; (setf *indent-new-lines* nil)

  ;; An ( typed puts ) after the cursor, and the other pairs the same. Not set:
  ;; nil.
  ;; (setf *electric-pair-mode* t)

  ;; Every pair closes itself, : and ' too. Not set: :safe.
  ;; (setf *electric-pair-typing* :all)

  ;; :safe closes ' too.
  ;; (setf *electric-pair-safe-exclusions*
  ;;       (remove #\' *electric-pair-safe-exclusions*))

  ;; A pair of your own: m i, wrapping, typing.
  ;; (setf *pair-delimiters* (cons '(#\# . #\#) *pair-delimiters*))

  ;; No showing the bracket that pairs with the one at point. Not set: t.
  ;; (setf *show-paren-mode* nil)

  ;; Typing ) says nothing of its partner out of sight. Not set: t.
  ;; (setf *blink-matching-paren* nil)

  ;; g q, g Q and M-q fill to column 72. Not set: 80.
  ;; (setf *fill-column* 72)

  ;; git's commit messages to 70. Not set: 72.
  ;; (setf *git-commit-fill-column* 70)

  ;; Two spaces between sentences: M-e, M-a and filling go by them. Not set: nil:
  ;; one; a file may say its own.
  ;; (setf *sentence-end-double-space* t)

  ;; Full stops of your own abbreviations end no sentence, for M-e, M-a and
  ;; filling. Not set: none past Tuatara's own (e.g., Dr., Sept.).
  ;; (setf *extra-sentence-abbreviations* '("Ph.D." "Inc."))

  ;; Full stops of these of Tuatara's abbreviations end a sentence after all.
  ;; Not set: none.
  ;; (setf *removed-sentence-abbreviations* '("St." "No."))

  ;; Paragraphs of up to 8 MB filled. Not set: 1 MB.
  ;; (setf *fill-paragraph-limit* (* 8 1024 1024))

  ;; Filling at once, the column not asked. Not set: t.
  ;; (setf *fill-ask-column* nil)

  ;; M-x sort-lines ignores case. Not set: nil.
  ;; (setf *sort-fold-case* t)

  ;;; Snippets (a key and TAB, or C-c y: text with fields to fill in)

  ;; Folders of your snippets, a folder a mode (org-mode/, prog-mode/,
  ;; fundamental-mode/) and a file a snippet. Not set: nil: snippets/ in the
  ;; configuration folder, ~/.config/tuatara/snippets/.
  ;; (setf *snippet-directories*
  ;;       (list "~/.config/tuatara/snippets/" "~/notes/snippets/"))

  ;; A snippet's key and TAB no longer put it in; C-c y still does. Not set: t.
  ;; (setf *snippet-tab* nil)

  ;; A snippet's file: # name: Shell block, # key: <sh, # --, then its text; $1
  ;; ${1:text} ${1|a,b|} are fields TAB goes through, $0 where the cursor ends,
  ;; $CURRENT_YEAR $TM_FILENAME_BASE $AUTHOR values.

  ;;; Killing, yanking, marks, input methods

  ;; Kills stay in the editor, off the system clipboard. Not set: t.
  ;; (setf *use-system-clipboard* nil)

  ;; Kills of up to 32 M characters go on the clipboard. Not set: 8 M.
  ;; (setf *clipboard-max-size* (* 32 1024 1024))

  ;; The kill ring keeps the 60 newest kills. Not set: 120.
  ;; (setf *kill-ring-max* 60)

  ;; The kills together hold at most 256 M characters. Not set: 64 M.
  ;; (setf *kill-ring-max-size* (* 256 1024 1024))

  ;; Undo keeps at most 256 MB of changes a buffer; past it the oldest can no
  ;; longer be undone. Not set: nil, every change kept back to the file as opened.
  ;; (setf *undo-limit* (* 256 1024 1024))

  ;; Marks kept a buffer, for C-u C-SPC to go back through. Not set: 16.
  ;; (setf *mark-ring-max* 32)

  ;; Marks kept across buffers, for C-x C-SPC. Not set: 16.
  ;; (setf *global-mark-ring-max* 32)

  ;; C-u C-SPC, then C-SPC C-SPC ... go on back through the marks. Not set: nil.
  ;; (setf *set-mark-command-repeat-pop* t)

  ;; C-\ turns this input method on and off. Not set: nil: asked the first time;
  ;; C-x RET C-\ chooses.
  ;; (setf *default-input-method* "german-postfix")

  ;; Input methods of your own under SPC i. Not set: none.
  ;; (setf *input-method-keys*
  ;;       '(("g" "german" "german-postfix")
  ;;         ("f" "french" "french-postfix")))

  ;;; Questions and lists in the minibuffer; several cursors

  ;; No 3/120 before the prompt of a question with choices. Not set: t.
  ;; (setf *show-completion-count* nil)

  ;; The prompt under the choices, on the bottom row. Not set: t: above them.
  ;; (setf *prompt-above-candidates* nil)

  ;; The key hints after C-x, SPC ... take up to 20 rows; C-h after the prefix
  ;; lists every key. Not set: nil: as many as the frame has room for.
  ;; (setf *key-hint-rows* 20)

  ;; Files and folders asked for, the last used first. Not set: nil: in plain
  ;; order.
  ;; (setf *last-used-first* t)

  ;; Typing a slash in a path in the text (/etc/, ~/notes/, ./src/) lists
  ;; what that folder holds beside the cursor; TAB puts one in. Not set: t;
  ;; nil: no list.
  ;; (setf *complete-paths* nil)

  ;; The list of a path's folder by major mode, before the global setting.
  ;; (setf *complete-paths-modes* '((org-mode . nil) (prog-mode . t)))

  ;; Beside files their date, never an age such as 3 h ago. Not set: 14 days.
  ;; (setf *relative-age-limit* 0)

  ;; f holds up to 200,000 matches. Not set: 100,000; nil: as many as memory has
  ;; room for.
  ;; (setf *split-limit* 200000)

  ;; t makes up to 50,000 cursors. Not set: 10,000; nil: as many as memory has
  ;; room for.
  ;; (setf *cursors-limit* 50000)

  ;; c on an m v block puts up to 50,000 cursors. Not set: 10,000.
  ;; (setf *rectangle-cursors-limit* 50000)

  ;; Past f's, t's or a block's limit, asked whether to go on all the same. Not
  ;; set: nil.
  ;; (setf *split-limit-ask* t)

  ;; C-x / s in a large list or CSV matches from the top of the window for up
  ;; to 2/100 s at each key, then the rest in the background. Not set: 1/100.
  ;; (setf *search-filter-screen-seconds* 2/100)
  ;; C-x / s narrows a file of these modes, a record a line, as a CSV, by
  ;; hiding the lines left out; here also a mode of your own. Not set: logs,
  ;; JSON Lines, todo.txt, hosts, passwd, requirements, .gitignore and .env.
  ;; (setf *line-record-modes* (cons 'ini-mode *line-record-modes*))
  ;; While a pattern is typed (C-s, C-x / s), a text larger than 64 MB has its
  ;; matches counted and filtered once typing pauses for 1/2 s, the lines in
  ;; the window shown at once; a smaller one at each key. Not set: 1/2 and
  ;; 64 MB.
  ;; (setf *typing-pause* 3/10)
  ;; (setf *typing-pause-octets* (* 16 1024 1024))

  ;; Named patterns of your own for every search, [[:nzphone:]] here, as
  ;; [[:ipv4:]] and Tuatara's others are: a name, what it finds, and the
  ;; pattern, in which a backslash is written twice; one may use another.
  ;; nzphone: 021 123 4567, (09) 123 4567, +64 21 123 4567, 0064 9 123 4567,
  ;; +64 (0)21 123 4567, 0800 123 456. Not set: Tuatara's only.
  ;; (define-named-pattern "nzlocal" "an NZ number after its 0 or +64"
  ;;   "(?:[34679][ -]?[0-9]{3}[ -]?[0-9]{4}|2[0-9][ -]?[0-9]{3,4}[ -]?[0-9]{3,4}|(?:800|508)[ -]?[0-9]{3}[ -]?[0-9]{3})")
  ;; (define-named-pattern "nzphone" "a New Zealand phone number, national or international"
  ;;   "(?<![0-9+])(?:(?:\\+|00)64[ -]?(?:\\(0\\)[ -]?)?[[:nzlocal:]]|0[[:nzlocal:]]|\\(0[34679]\\)[ -]?[0-9]{3}[ -]?[0-9]{4})(?![0-9])")

  ;;; Showing: line numbers, the line and column of point, wrapping, spelling

  ;; Line numbers in every buffer: :absolute or :relative. Not set: nil: none.
  ;; (setf *line-numbers-default* :relative)

  ;; Line numbers by major mode, before *line-numbers-default*.
  ;; (setf *line-numbers-modes* '((prog-mode . :relative) (org-mode . nil)))

  ;; The change marks beside the lines of a file in git compare up to 50 MB
  ;; of it; past that the file has none. Not set: 200 MB.
  ;; (setf *vc-gutter-max-bytes* (* 50 1024 1024))

  ;; The line point is on lit only where C-x T h says. Not set: t.
  ;; (setf *global-hl-line-mode* nil)

  ;; The line lit by major mode, before the global setting.
  ;; (setf *hl-line-modes* '((csv-mode . nil)))

  ;; The line point is on, in a colour of your own.
  ;; (set-face :hl-line :bg "#1f2937")

  ;; The column point is in lit down the window, everywhere C-x T c does not say
  ;; otherwise. Not set: nil.
  ;; (setf *global-cursor-column-mode* t)

  ;; The column lit by major mode, before the global setting.
  ;; (setf *cursor-column-modes* '((prog-mode . t)))

  ;; The column, in a colour of its own (:hl-line's)
  ;; (set-face :cursor-column :bg "#1f2937")

  ;; Long lines wrap at a word's edge in every buffer, as C-x T l does in one. Not
  ;; set: nil.
  ;; (setf *global-soft-wrap-mode* t)

  ;; Wrapping by major mode, before the global setting.
  ;; (setf *soft-wrap-modes* '((org-mode . t) (csv-mode . nil)))

  ;; Wrapping at the window's edge, not at the fill column. Not set: t.
  ;; (setf *soft-wrap-at-fill-column* nil)

  ;; No mark beside the rows that go on. Not set: "↪".
  ;; (setf *soft-wrap-indicator* nil)

  ;; The row point is on where lines wrap, apart from :hl-line (the same)
  ;; (set-face :soft-wrap-row :bg "#254862")

  ;; The column of text C-x T o centres in the window. Not set: 80.
  ;; (setf *centred-text-width* 80)

  ;; Buffers of these modes start with their text centred. Not set: none.
  ;; (setf *centred-text-modes* '((org-mode . t)))

  ;; The text centred in every buffer, as C-x T o does in one. Not set: nil.
  ;; (setf *global-centred-text-mode* t)

  ;; A line down every window at the fill column, as C-x T v does in one. Not set:
  ;; nil.
  ;; (setf *global-visual-line-mode* t)

  ;; That line by major mode, before the global setting.
  ;; (setf *visual-line-modes* '((text-mode . t)))

  ;; The line's colour.
  ;; (set-face :visual-line :bg "#540b0c")

  ;; Characters that draw nothing shown as their codes (<FEFF>, <200B>) in every
  ;; buffer, as C-x T i does in one. Not set: nil.
  ;; (setf *global-invisible-characters-mode* t)

  ;; Shown as their codes by major mode, before the global setting.
  ;; (setf *invisible-characters-modes* '((csv-mode . t)))

  ;; Files opened safely show them as any buffer does. Not set: t: as their codes.
  ;; (setf *invisible-characters-when-safe* nil)

  ;; Words spelt wrong marked in every buffer of text and code. Not set: nil.
  ;; (setf *global-spellcheck-mode* t)

  ;; Spelling checked by major mode, before the global setting; in code t checks
  ;; comments and strings, :comments, :strings or :all every word.
  ;; (setf *spellcheck-modes*
  ;;       '((org-mode . t) (prog-mode . :comments) (csv-mode . nil)))

  ;; The languages words are checked in, right in any of them. Not set: the
  ;; locale's, else en_US.
  ;; (setf *spellcheck-languages* '("en_GB" "de_DE"))

  ;; What counts as a comment, by face, for spelling in code. Not set: :comment.
  ;; (setf *spellcheck-comment-faces* '(:comment))

  ;; What counts as a string. Not set: :string.
  ;; (setf *spellcheck-string-faces* '(:string))

  ;; Text in these faces never checked (Org's code, verbatim, links, blocks ...)
  ;; (push :org-table *spellcheck-excluded-faces*)

  ;; Words spelt wrong underlined with dots; :underline t a line (:wave)
  ;; (set-face :spellcheck :underline :dotted)

  ;; The underline's colour (the theme's warning yellow)
  ;; (set-face :spellcheck :underline-colour "#ff6b6b")

  ;;; Files and buffers: starting, opening, keeping

  ;; Start in the Org scratch buffer. Not set: "*lisp-scratch*".
  ;; (setf *startup-buffer* "*org-scratch*")

  ;; Only *org-scratch* made by itself. Not set: both; nil: neither, M-x
  ;; lisp-scratch makes one.
  ;; (setf *auto-scratch-buffers* '("*org-scratch*"))

  ;; Where a buffer with no file is: C-x C-f from it, M-s g. Not set: ~/; nil:
  ;; where Tuatara started.
  ;; (setf *default-directory* "~/notes/")

  ;; Buffers keep up with their files changed on disk. Not set: nil: asked; M-x
  ;; follow-file for one.
  ;; (setf *reload-changed-files* t)

  ;; A file's own settings (-*- mode: org -*-, Local Variables:) not taken. Not
  ;; set: t: its mode, tab width, tabs, fill column; nothing run.
  ;; (setf *file-local-variables* nil)

  ;; Files dropped on the window open safely, as C-u C-x C-f. Not set: nil.
  ;; (setf *open-dropped-files-safely* t)

  ;; Files in these folders always open safely. Not set: none.
  ;; (setf *safe-directories* '("~/Downloads/"))

  ;; Files with NUL octets (programs, pictures) open as usual. Not set: t: safely.
  ;; (setf *open-binary-files-safely* nil)

  ;; Files with NUL octets open as text, not in the hex view. Not set: t.
  ;; (setf *hex-view-binary-files* nil)

  ;; Files open at their top. Not set: t: where you left them.
  ;; (setf *save-place-mode* nil)

  ;; How many files' places are kept. Not set: 400.
  ;; (setf *save-place-limit* 1000)

  ;; No list of the files visited. Not set: t; C-x C-r.
  ;; (setf *remember-recent-files* nil)

  ;; C-x C-r offers the last 100 files visited. Not set: 25.
  ;; (setf *recent-files-max* 100)

  ;; Files no longer there kept in that list, shown not found. Not set: t: taken
  ;; out.
  ;; (setf *recent-files-auto-cleanup* nil)

  ;; How many bookmarks are kept: C-x m m sets one, C-x m b goes to one, C-x m l
  ;; manages them. Not set: 1000.
  ;; (setf *bookmarks-kept* 5000)

  ;; C-x m m asks no categories; C-x m l, c sets them. Not set: t: asked after the
  ;; name.
  ;; (setf *bookmark-set-asks-categories* nil)

  ;; The file selected shown while choosing one (C-x C-f, C-x C-r), opened safely.
  ;; Not set: nil.
  ;; (setf *preview-files* t)

  ;; The same while choosing a note with C-x n o. Not set: nil.
  ;; (setf *preview-formatted-files* t)

  ;; The line found shown while searching notes with C-x n g. Not set: nil.
  ;; (setf *preview-formatted-search* t)

  ;; The line found shown while searching files with M-s g. Not set: nil.
  ;; (setf *preview-file-search* t)

  ;; The buffer selected shown while choosing one (C-x b). Not set: nil.
  ;; (setf *preview-buffers* t)

  ;; Files, folders and windows as they were, when starting without files. Not
  ;; set: nil.
  ;; (setf *keep-session* t)

  ;; The session written every 60 seconds while it changes, for a crash. Not set:
  ;; 30.
  ;; (setf *keep-session-interval* 60)

  ;; No copies of changes not saved. Not set: t: written now and then to a copy of
  ;; their own, never the file; M-x recover-file.
  ;; (setf *auto-save* nil)

  ;; Where those copies are kept, for you alone. Not set: auto-save/ in
  ;; ~/.local/state/tuatara/.
  ;; (setf *auto-save-directory* "~/auto-save/")

  ;; Copied 2 seconds after the last change. Not set: 5.
  ;; (setf *auto-save-idle* 2)

  ;; And at least every 30 seconds while changes go on. Not set: 60.
  ;; (setf *auto-save-interval* 30)

  ;; Buffers of up to 1 GB copied. Not set: 256 MB.
  ;; (setf *auto-save-largest* (* 1024 1024 1024))

  ;; C-x i (insert-note-template) reads other files of up to 64 MB. Not set: 16
  ;; MB.
  ;; (setf *insert-file-limit* (* 64 1024 1024))

  ;; Your name: an export's author without #+author:, %author% and $AUTHOR. Not
  ;; set: the system's.
  ;; (setf *user-full-name* "Your Name")

  ;;; Windows: where buffers such as help and *Messages* are shown
  ;; One line a rule, for buffers by name ("*Messages*"), by kind (:help,
  ;; :messages) or by major mode ('help-mode); of the rules that fit a buffer, the
  ;; one made last counts. The places: :here this window, :other another (split
  ;; off one if need be), :below and :beside a new window below or right of this
  ;; one, :bottom and :right across the whole bottom or down the whole right side,
  ;; :nowhere. :size is a fraction of the screen (0.3), rows or columns (12), or
  ;; :fit; :keep t keeps the window for them, q closing it; :select t moves you
  ;; there. M-x describe-buffer-places lists the rules.

  ;; As standard.
  ;; (place-buffers :messages :bottom :size 0.3 :keep t :select t)

  ;; As standard.
  ;; (place-buffers :help :other)

  ;; As standard: you are moved there.
  ;; (place-buffers "*Apropos*" :other :select t)

  ;; Help down the right side.
  ;; (place-buffers :help :right :size 0.4)

  ;; Help in the window you are in.
  ;; (place-buffers :help :here)

  ;; As standard: exported PDFs down the right side, alone on a narrow screen.
  ;; (place-buffers :exported :right-or-alone)

  ;; Exported PDFs beside the window you are in.
  ;; (place-buffers :exported :beside)

  ;; The list of problems (C-c l) down the right side (across the bottom, 30%
  ;; high, selected)
  ;; (place-buffers 'diagnostics-mode :right :size 0.4)

  ;; :other splits side by side from 120 columns. Not set: 160.
  ;; (setf *split-width-threshold* 120)

  ;; :other splits one above the other from 60 rows. Not set: 80.
  ;; (setf *split-height-threshold* 60)

  ;; Scrolling starts 10 rows before point reaches the top or bottom of the
  ;; window, where there is text beyond it. Not set: 0, at the edge.
  ;; (setf *scroll-margin* 10)

  ;; When point leaves the window, or comes within *scroll-margin* rows of its
  ;; edge, its line goes to the middle of the window (as C-l). Not set: nil:
  ;; the window scrolls only as far as keeps point in view.
  ;; (setf *scroll-recenter* t)

  ;; No window under 6 rows when splitting. Not set: 4.
  ;; (setf *window-min-height* 6)

  ;; No window under 20 columns when splitting. Not set: 10.
  ;; (setf *window-min-width* 20)

  ;; Moved to the window help shows in (C-h f, C-h k ...), q there closing it. Not
  ;; set: nil.
  ;; (setf *help-window-select* t)

  ;;; Notes: files with formatted names
  ;; C-x n f finds or makes one, C-x n o opens one, C-x n n makes one, C-x n g
  ;; searches them.
  ;; In a header, {title} {date} {keywords} {identifier} are the file's own.

  ;; The folder of your notes. Not set: ~/.
  ;; (setf *formatted-file-directory* "~/notes/")

  ;; The keywords offered. Not set: none.
  ;; (setf *formatted-file-keywords* '("project" "meeting"))

  ;; What a new note asks, in order. Not set: '(:title :subdirectory :keywords
  ;; :file-type :template).
  ;; (setf *formatted-file-prompts*
  ;;       '(:subdirectory :title :keywords :file-type :template))

  ;; A new note's kind when not asked: :org :text :markdown. Not set: :org.
  ;; (setf *formatted-file-type* :text)

  ;; The ending C-x n f offers, RET taking it. Not set: none.
  ;; (setf *formatted-file-extension* "org")

  ;; Keywords kept in the order typed. Not set: t: sorted.
  ;; (setf *formatted-file-sort-keywords* nil)

  ;; The header of new Org notes.
  ;; (set-formatted-file-header :org (format nil "#+title: {title}~%~
  ;;                                         #+date: {date}~%~
  ;;                                         #+filetags: {keywords}~%~
  ;;                                         #+identifier: {identifier}~2%"))

  ;; A kind of your own: (name ending style header)
  ;; (push (list :rst ".rst" :text "") *formatted-file-types*)

  ;; The journal (C-x n j) and the calendar (C-x n a)

  ;; Where the journal is kept. Not set: nil: journal/ in
  ;; *formatted-file-directory*.
  ;; (setf *journal-directory* "~/journal/")

  ;; The keyword of journal entries' names. Not set: "journal".
  ;; (setf *journal-keyword* "diary")

  ;; Titles with the time; :day, :day-date-month-year-12h, "Week %V, %A", "", nil
  ;; to ask. Not set: :day-date-month-year.
  ;; (setf *journal-title-format* :day-date-month-year-24h)

  ;; An entry a week; :monthly, :yearly. Not set: :daily.
  ;; (setf *journal-interval* :weekly)

  ;; Entries in Markdown; :org, :text. Not set: nil: *formatted-file-type*.
  ;; (setf *journal-file-type* :markdown)

  ;; Plain names, the date as strftime writes it. Not set: nil: formatted names.
  ;; (setf *journal-file-name* "%Y-%m-%d.org")

  ;; Org entries get your notes' header. Not set: Denote's front matter.
  ;; (setf *journal-org-header* nil)

  ;; The calendar's weeks from Sunday. Not set: 1: Monday.
  ;; (setf *calendar-week-start-day* 0)

  ;; A year around point. Not set: 3, the month at point and those beside it;
  ;; nil: as many as fit side by side.
  ;; (setf *calendar-months* 12)

  ;; Months side by side, the rest in rows below. Not set: nil: as many as fit.
  ;; (setf *calendar-months-per-row* 3)

  ;; Days with agenda entries marked only on m. Not set: t: at once.
  ;; (setf *calendar-mark-days* nil)

  ;; C-c C-s and C-c C-d ask for the date without the calendar below. Not set: t,
  ;; as Org.
  ;; (setf *org-read-date-popup-calendar* nil)

  ;; Dates asked for at the prompt, not first chosen in the calendar below (h j k l
  ;; move, m marks, u unmarks, RET writes the days chosen at the prompt). Not set: t.
  ;; (setf *org-read-date-in-calendar* nil)

  ;;; Searching files as you type (M-s g, C-x n g)

  ;; Folders kept read in memory and current: C-x n o, C-x n g and M-s g there
  ;; answer at once. Not set: none.
  ;; (setf *indexed-directories* (list *formatted-file-directory*))

  ;; Indexes in memory only, read whole at each start. Not set: t: kept in the
  ;; cache.
  ;; (setf *indexes-kept-on-disk* nil)

  ;; Begin after 2 characters. Not set: 3.
  ;; (setf *search-files-min-input* 2)

  ;; Up to 4 GB of text in memory for searches and each index. Not set: 128 MB;
  ;; past it from disk.
  ;; (setf *search-files-limit* (* 4 1024 1024 1024))

  ;; Up to 5,000 matching lines listed. Not set: 2,000.
  ;; (setf *search-files-max-lines* 5000)

  ;; What marks a project, all of which M-s g searches. Not set: '(".git"
  ;; ".project").
  ;; (setf *project-root-markers* '(".git" ".project" "Cargo.toml"))

  ;; The mode line's sign for kept folders: a Nerd Font glyph. Not set: "U"; nil:
  ;; none.
  ;; (setf *indexing-indicator* "")

  ;; The sign lit while they are searched. Not set: "S"; nil: none.
  ;; (setf *searching-indicator* "")

  ;;; Running a build or the tests (M-x compile)

  ;; The command M-x compile offers before you have typed one, run from the
  ;; project's root. Not set: "make -k ".
  ;; (setf *compile-command* "cargo build ")

  ;; The most of a command's output *compile* keeps; past it the output is
  ;; counted, not kept, and the end says how much was left out. Not set: 256 MB;
  ;; nil: all of it.
  ;; (setf *compile-output-most* (* 1024 1024 1024))

  ;;; Language servers and formatting (C-c in code lists their keys)

  ;; Projects whose servers start by themselves (they run the project's code);
  ;; others: M-x lsp asks once. Not set: none.
  ;; (setf *lsp-trusted-projects* '("~/src/"))

  ;; Servers for these languages alone; '(:not markdown) all but these. Not set:
  ;; t: every one with a server.
  ;; (setf *lsp-languages* '(go rust c cpp python latex lua bash))

  ;; Your own servers before each language's usual ones: by name, or (("program"
  ;; "--stdio")); (markdown) for none.
  ;; (setf *language-servers* '((python "basedpyright") (markdown)))

  ;; Your own formatters before each language's usual ones, code on standard
  ;; input; (LANGUAGE) for none.
  ;; (setf *language-formatters* '((python "ruff" "format" "-")))

  ;; These languages formatted before each save. Not set: t: every one with a
  ;; formatter; none.
  ;; (setf *format-on-save* '(rust python))

  ;; Formatters run as any program. Not set: t: shut in the sandbox, the project
  ;; read only.
  ;; (setf *format-in-sandbox* nil)

  ;; A formatter stopped after 60 seconds, nothing changed. Not set: 30.
  ;; (setf *format-seconds* 60)

  ;; The text sent to servers 1 second after typing stops. Not set: 0.5.
  ;; (setf *lsp-sync-delay* 1)

  ;; What is known where the cursor rests shown after 1 second. Not set: 0.5.
  ;; (setf *lsp-idle-delay* 1)

  ;; A problem the cursor rests on said in the echo area. Not set: :box: under the
  ;; cursor; nil: not said.
  ;; (setf *lsp-diagnostics-display* :echo)

  ;; Problems underlined with dots; :line, :background on a ground of their
  ;; colour. Not set: :wave.
  ;; (setf *lsp-diagnostics-style* :dotted)

  ;; One kind's colour (also -warning, -info, -hint; their -ground faces for
  ;; :background)
  ;; (set-face :diagnostic-error :underline-colour "#ff6b6b")

  ;; A completion or a definition waits 5 seconds for its server at most, a key
  ;; stopping it. Not set: 2.
  ;; (setf *lsp-wait* 5)

  ;; No call's parameters in the echo area. Not set: t.
  ;; (setf *lsp-signature-help* nil)

  ;; No type or signature of the thing under the cursor in the echo area. Not set:
  ;; t.
  ;; (setf *lsp-hover-echo* nil)

  ;; The other uses of the name under the cursor not lit. Not set: t.
  ;; (setf *lsp-highlight-symbol* nil)

  ;; M-g o lists the mode's headings, not the server's names. Not set: t.
  ;; (setf *lsp-outline* nil)

  ;; No types and parameter names faint within the code. Not set: t; C-c i.
  ;; (setf *lsp-inlay-hints* nil)

  ;; What servers say of lines (3 references) faint at their ends, C-c k running
  ;; one. Not set: nil; C-c K.
  ;; (setf *lsp-code-lenses* t)

  ;; Code coloured as its server sees it. Not set: nil; C-c S.
  ;; (setf *lsp-semantic-tokens* t)

  ;; The face a kind of name is coloured in.
  ;; (push '("parameter" . :variable.other) *lsp-semantic-faces*)

  ;; More than 200 edits at once sent as the whole text. Not set: 1000.
  ;; (setf *lsp-most-changes* 200)

  ;; Servers not told of their project's files changed by other programs. Not set:
  ;; t.
  ;; (setf *lsp-watch-files* nil)

  ;; build/ folders not watched either (target, node_modules ...)
  ;; (push "build" *lsp-watch-skipped*)

  ;; Up to 20,000 folders of a project watched. Not set: 5000.
  ;; (setf *lsp-watched-most* 20000)

  ;; M-x lsp-events keeps each server's last 2000 messages. Not set: 500; 0: none.
  ;; (setf *lsp-events-kept* 2000)

  ;; And the first 10,000 characters of each. Not set: 2000.
  ;; (setf *lsp-event-length* 10000)

  ;; The places a question to a server names (references, definitions) show
  ;; their lines from up to 64 MB of files read; past it, the places alone.
  ;; Not set: 16 MB.
  ;; (setf *lsp-location-files-total* (* 64 1024 1024))

  ;;; Git (C-x g)

  ;; The status shows the 20 latest commits. Not set: 10.
  ;; (setf *git-recent-commits* 20)

  ;; L there lists the 1000 latest. Not set: 200.
  ;; (setf *git-log-length* 1000)

  ;; TAB on a file shows the first 3000 lines of its changes, and TAB on the
  ;; line after them 3000 more. Not set: 1000.
  ;; (setf *git-diff-lines* 3000)

  ;; Each section of the status (files in conflict, not tracked, changed,
  ;; staged, ignored) lists its first 3000 files, and TAB on the line after
  ;; them 3000 more. Not set: 1000.
  ;; (setf *git-section-lines* 3000)

  ;; A file up to 64 MB is looked through for the conflicts a merge left in
  ;; it when it is shown; a larger one only with M-x conflict-markers-mode.
  ;; Not set: 256 MB.
  ;; (setf *conflict-markers-max-bytes* (* 64 1024 1024))

  ;; A status on the screen is read again at most every 5 seconds when git
  ;; changes the repository (from a terminal too), nil never: r reads it
  ;; again. Not set: 2.
  ;; (setf *git-watch-seconds* 5)

  ;; The git menus (b for branches, Z for worktrees ... in the status) show
  ;; every action. Not set: :slim, the ones most used; ~ in a status switches.
  ;; (setf *git-menus* :full)

  ;; The slim menus show moving a branch too. Not set: the actions most used:
  ;; committing, amending, rewording, extending; checking out, making,
  ;; renaming, deleting branches; stashing, applying, popping, dropping;
  ;; making, showing, deleting worktrees. C-h in a menu lists the rest.
  ;; (push 'git-branch-reset *git-slim-actions*)

  ;;; Encryption and wiping

  ;; The GnuPG program for .gpg files. Not set: "gpg".
  ;; (setf *gpg-program* "/usr/bin/gpg2")

  ;; gpg-agent's pinentry asks in the terminal; :agent, :minibuffer. Not set:
  ;; :auto: its window on a desktop, else the terminal.
  ;; (setf *gpg-passphrase-from* :terminal)

  ;; No window of its risks when the minibuffer asks for a passphrase. Not set: t.
  ;; (setf *gpg-loopback-warning* nil)

  ;; A file wiped is written over 3 times before it is deleted. Not set: 1; more
  ;; help no SSD or btrfs.
  ;; (setf *wipe-passes* 3)

  ;; tar packing or extracting an archive (rimu's c and Z) is stopped after an
  ;; hour. Not set: a day.
  ;; (setf *archive-seconds* (* 60 60))

  ;; Unpacking an archive (Z in rimu) stops before the drive has less than
  ;; 10 GB free. Not set: 1 GB.
  ;; (setf *archive-free-reserve* (* 10 1024 1024 1024))

  ;; What an archive holds (C-c e in rimu) is shown up to 16 MB of its list.
  ;; Not set: 64 MB.
  ;; (setf *archive-listing-limit* (* 16 1024 1024))

  ;; The differences of two files (= in rimu, M-x diff-files) are shown up to
  ;; 16 MB of them. Not set: 64 MB.
  ;; (setf *differences-limit* (* 16 1024 1024))

  ;; M-x file-metadata names the owners of an encrypted file's keys from your
  ;; keyring (runs gpg --list-keys; the file is never given to it); nil: the
  ;; key IDs alone. Not set: :ask each time.
  ;; (setf *metadata-look-up-key-owners* t)

  ;;; Viewing what is not text: the sandbox, pictures, PDFs, office documents
  ;; Where Tuatara keeps what files you open leave behind, as on an encrypted
  ;; drive when the system's is not: its cache (compiled packages, previews,
  ;; pages drawn, the logs), what it remembers between sessions (recent files,
  ;; places, the session, auto-saved text) and the packages it fetches. Make
  ;; each folder yourself (mkdir -m 700); while one is not there, its drive
  ;; not mounted, Tuatara keeps those things in memory and says so. Move what
  ;; is in the standard folders there first. Not set: ~/.cache/tuatara/,
  ;; ~/.local/state/tuatara/ and ~/.local/share/tuatara/packages/.
  ;; (setf *cache-directory* "/run/media/you/Data/tuatara/cache/"
  ;;       *state-directory* "/run/media/you/Data/tuatara/state/"
  ;;       *packages-directory* "/run/media/you/Data/tuatara/packages/")

  ;; The program decoding a file runs shut in by bubblewrap (bwrap), with no
  ;; network and no file but the one; M-x check-health says whether it works.

  ;; A program decoding a file may run 30 seconds. Not set: 10.
  ;; (setf *sandbox-seconds* 30)

  ;; And take 2 GB of memory. Not set: 1 GB; nil: no limit.
  ;; (setf *sandbox-memory* (* 2 1024 1024 1024))

  ;; A large picture (a PNG, TIFF or WebP of a hundred megapixels) may be given
  ;; up to 8 GB to be drawn. Not set: 4 GB; a JPEG needs little whatever its size.
  ;; (setf *picture-memory* (* 8 1024 1024 1024))

  ;; All of one file's decoding together 4 GB. Not set: 2 GB, where systemd-run
  ;; holds it; nil: no limit.
  ;; (setf *sandbox-total-memory* (* 4 1024 1024 1024))

  ;; Files of up to 4 GB decoded, each copied into memory first. Not set: 1 GB;
  ;; nil: no limit.
  ;; (setf *sandbox-input-limit* (* 4 1024 1024 1024))

  ;; A program's output (a decoder's, the clipboard's) taken up to 256 MB;
  ;; more, and the program is stopped, its output not taken. Not set: 64 MB.
  ;; (setf *program-output-limit* (* 256 1024 1024))

  ;; No file decoded: pictures and PDFs as octets and text. Not set: "bwrap".
  ;; (setf *sandbox-program* nil)

  ;; Decoding programs may make every system call. Not set: t: mounting,
  ;; namespaces, modules, tracing, io_uring, BPF, sockets refused.
  ;; (setf *sandbox-seccomp* nil)

  ;; ImageMagick refuses pictures wider or taller than 16,384 pixels. Not set:
  ;; "64KP".
  ;; (setf *magick-largest-side* "16KP")

  ;; PDF pages dark, as the theme. Not set: nil; d in a PDF.
  ;; (setf *pdf-view-dark-pages* t)

  ;; Word, Excel and PowerPoint read as text, LibreOffice never run. Not set: t:
  ;; pages drawn by LibreOffice in the sandbox, T for text.
  ;; (setf *office-view-pages* nil)

  ;; A note read from an office file (Word, Excel, PowerPoint, OpenDocument)
  ;; holds up to 64 million characters, the rest left out and said so. Not
  ;; set: 16 million.
  ;; (setf *office-note-largest* (* 64 1024 1024))

  ;; And up to 10,000 columns of one value (a cell, a formula), cut there
  ;; ending in …. Not set: 2000.
  ;; (setf *office-value-longest* 10000)

  ;; A Word document's note names the stand-ins of up to 40 fonts not
  ;; installed. Not set: 16.
  ;; (setf *docx-read-most-stand-ins* 40)

  ;; O opens pictures, PDFs, sound, video in the desktop's program at once. Not
  ;; set: t: asks.
  ;; (setf *open-externally-ask* nil)

  ;; Only these kinds, by their contents. Not set: pictures, PDFs, sound, video.
  ;; (setf *open-externally-kinds* '(:pdf :png :jpeg :mp4))

  ;; These MIME types in these programs. Not set: the desktop's choice.
  ;; (setf *open-externally-programs*
  ;;       '(("application/pdf" . "zathura") ("video/*" "mpv" "--fs")))

  ;; What opens them otherwise. Not set: "xdg-open".
  ;; (setf *open-externally-program* "gio open")

  ;;; Exporting: PDFs of Org and LaTeX files
  ;; M-x org-export-latex-pdf and the -buffer, -xdg, -enhanced ones;
  ;; latex-compile-pdf.

  ;; Lualatex first, xelatex when it fails. Not set: '("xelatex" "lualatex"); full
  ;; paths for programs elsewhere.
  ;; (setf *latex-engines* '("lualatex" "xelatex"))

  ;; The enhanced export's text font, for notes setting none. Not set: the first
  ;; installed of Libertinus Serif, Noto Serif ...
  ;; (setf *latex-main-font* "Libertinus Serif")

  ;; Its sans-serif font. Not set: Libertinus Sans, Noto Sans ...
  ;; (setf *latex-sans-font* "Noto Sans")

  ;; Its font for code. Not set: your window's font when installed, else JetBrains
  ;; Mono ...
  ;; (setf *latex-mono-font* "JetBrains Mono")

  ;; Commands hiding their argument, for M-x check-redactions. Not set: '("R" "D"
  ;; "redact").
  ;; (setf *latex-redaction-commands* '("R" "D" "anon"))

  ;; Besides a note's own folder and your note templates' folder, where its export
  ;; may take #+SETUPFILE files and pictures from. Not set: none.
  ;; (setf *export-trusted-folders* '("~/notes/latex/" "~/notes/pictures/"))

  ;; The folder of the pictures your templates ask a note for: #+logo: a name
  ;; alone (logo.png) is the file of that name there, and a picture a template
  ;; asks for comes from it or the note's own folder, never from elsewhere.
  ;; Not set: logos/ in the configuration directory (~/.config/tuatara/logos/).
  ;; (setf *logos-directory* "~/notes/logos/")

  ;; A picture of up to 256 MB a LaTeX document shows saved with the template
  ;; made from it (make-template-from-latex). Not set: 64 MB.
  ;; (setf *latex-import-picture-limit* (* 256 1024 1024))

  ;; The most characters a note may grow to as its macro calls are put in
  ;; place for an export. Not set: 16 MB.
  ;; (setf *org-macro-limit* (* 32 1024 1024))

  ;;; Messages and debugging

  ;; Lines *Messages* keeps. Not set: 1000.
  ;; (setf *message-log-max* 5000)

  ;; Start in debug mode: what the editor does goes to *Debug*. Not set: nil; M-x
  ;; debug-mode, C-h E.
  ;; (setf *debug-mode* t)

  ;; Garbage left for its collection to come when it comes, maybe between two
  ;; keys. Not set: 0.3: collected after that many seconds with no key, once
  ;; there is enough of it.
  ;; (setf *idle-collection-seconds* nil)

  ;; Lines *Debug* keeps. Not set: 10000; nil: all.
  ;; (setf *debug-log-max* 50000)

  ;; An error shows the calls under way in *Backtrace*. Not set: nil; M-x
  ;; toggle-debug-on-error.
  ;; (setf *debug-on-error* t)
  )

;;; Org

(load-package "org"
  :config

  ;; Org's keys under the leader: SPC a the agenda and the clock, SPC O
  ;; links, SPC t L how links show.
  ;; (bind-key "t L" 'org-toggle-link-display :map :leader :mode 'org-mode)
  ;; (bind-key "O" nil :map :leader :label "org")
  ;; (bind-key "O l" 'org-insert-link :map :leader :mode 'org-mode)
  ;; (bind-key "O o" 'org-open-at-point :map :leader :mode 'org-mode)
  ;; (bind-key "O s" 'org-store-link :map :leader)
  ;; (bind-key "O b" 'org-update-all-dblocks :map :leader :mode 'org-mode)
  ;; (bind-key "a" nil :map :leader :label "org/agenda/clock")
  ;; (bind-key "a I" 'org-clock-in :map :leader)
  ;; (bind-key "a O" 'org-clock-out :map :leader)
  ;; (bind-key "a C" 'org-clock-cancel :map :leader)
  ;; (bind-key "a J" 'org-clock-goto :map :leader)
  ;; (bind-key "a c" 'org-capture :map :leader)
  ;; (bind-key "a a" 'org-agenda :map :leader)
  ;; (bind-key "a w" 'org-agenda-week-view :map :leader)
  ;; (bind-key "a m" 'org-agenda-month-view :map :leader)
  ;; (bind-key "a y" 'org-agenda-year-view :map :leader)

  ;;; Org: how notes look and behave

  ;; Org files open as written, not indented. Not set: t: as org-indent-mode.
  ;; (setf *org-startup-indented* nil)

  ;; A headline level's colour, bold or not (levels 1 to 8)
  ;; (set-face :org-level-1 :fg "#99c7ff" :bold t)

  ;; In the window, first-level headlines a quarter larger (up to 3)
  ;; (set-face :org-level-1 :height 1.25)

  ;; In the window, a level in a font and weight of its own.
  ;; (set-face :org-level-1 :font "DejaVu Sans Mono Bold")

  ;; Headlines show their stars. Not set: '("◉" "○" "✸" "✿"), by level.
  ;; (setf *org-bullets* nil)

  ;; A headline's stars before its sign shown faint, not hidden. Not set: t.
  ;; (setf *org-hide-leading-stars* nil)

  ;; List items' - + * shown as these signs. Not set: nil: as written.
  ;; (setf *org-list-bullets* '(("-" . "•") ("+" . "◦") ("*" . "▸")))

  ;; Checkboxes shown as these signs. Not set: nil: as written.
  ;; (setf *org-checkbox-signs* '(("[ ]" . "☐") ("[-]" . "◐") ("[X]" . "☑")))

  ;; - [ stays as typed. Not set: t: becomes - [ ] , the cursor after it.
  ;; (setf *org-complete-checkboxes* nil)

  ;; RET in NORMAL state on a checkbox line does nothing. Not set: t: cycles [ ]
  ;; [-] [X].
  ;; (setf *org-return-cycles-checkboxes* nil)

  ;; Tasks TODO, NEXT, DONE where a file has no #+TODO:. Not set: TODO WAIT
  ;; open, DONE CANCELLED closed.
  ;; (setf *org-standard-todo-keywords* '(("TODO" "NEXT") . ("DONE")))

  ;; C-c C-t, S-right and RET stop on CANCELLED too. Not set: they pass over
  ;; CANCELLED, which C-c C-x C sets.
  ;; (setf *org-todo-keywords-not-cycled* nil)

  ;; NEXT in WAIT's colour. Not set: WAIT and CANCELLED have their own.
  ;; (push '("NEXT" . :org-wait) *org-todo-keyword-faces*)

  ;; RET in NORMAL state on a headline with a TODO keyword does nothing. Not
  ;; set: t: its next keyword, TODO to WAIT to DONE and round.
  ;; (setf *org-return-cycles-todo* nil)

  ;; Ticking a checkbox logs nothing. Not set: t: its state and time in a
  ;; :LOGBOOK:.
  ;; (setf *org-log-checkboxes* nil)

  ;; A TODO with [n/m] stays when all its boxes are ticked. Not set: t: becomes
  ;; DONE.
  ;; (setf *org-todo-follows-checkboxes* nil)

  ;; TAB shows drawers (:LOGBOOK:) with the rest. Not set: t: they stay folded.
  ;; (setf *org-cycle-hide-drawers* nil)

  ;; Org files open with each task's drawers and notes shown. Not set: t:
  ;; folded to one line each, TAB opens one.
  ;; (setf *org-fold-metadata* nil)

  ;; TAB never folds a list item. Not set: t: as a headline.
  ;; (setf *org-cycle-include-plain-lists* nil)

  ;; Links and entities shown as written, [[target][label]], \dollar{}. Not set:
  ;; t: links as their labels, entities as their characters; C-c C-x L.
  ;; (setf *org-link-descriptive* nil)

  ;; RET on a link a new line. Not set: t: follows it; C-c C-o still does.
  ;; (setf *org-return-follows-link* nil)

  ;; #+begin_src desktop coloured as INI; Org's names (emacs-lisp, sh) known
  ;; already.
  ;; (push '("desktop" . "ini") *org-src-languages*)

  ;; C-c C-v t makes missing folders for every block. Not set: nil: :mkdirp yes
  ;; ones.
  ;; (setf *code-files-make-folders* t)

  ;; #+BEGIN: hello :who "you" made with C-c C-c or C-c C-x C-u.
  ;; (define-dblock-writer "hello" (params)
  ;;   (format nil "Hello, ~a~%" (getf params :who)))

  ;; A clock table your way: :formatter "mine".
  ;; (define-clocktable-formatter "mine" (tables params)
  ;;   (format nil "~d minutes~%"
  ;;           (reduce (quote +) tables :key (quote second))))

  ;; Saving leaves the table of contents. Not set: t: made again under :TOC:, as
  ;; toc-org.
  ;; (setf *org-toc-on-save* nil)

  ;; A table of contents three levels deep when its tag says not. Not set: 2.
  ;; (setf *org-toc-max-depth* 3)

  ;; Its links to the titles, not GitHub's anchors. Not set: "gh".
  ;; (setf *org-toc-style* "org")

  ;; The same for Markdown's table of contents. Not set: t.
  ;; (setf *markdown-toc-on-save* nil)

  ;;; Org: logging, the agenda, capture

  ;; Clock lines right under the headline. Not set: t: in :LOGBOOK:.
  ;; (setf *org-clock-into-drawer* nil)

  ;; Nothing noted when a task is done. Not set: :time: CLOSED: and the time.
  ;; (setf *org-log-done* nil)

  ;; Notes (state changes, repeats, notes taken) right under the planning line.
  ;; Not set: t: into the task's :LOGBOOK: drawer.
  ;; (setf *org-log-into-drawer* nil)

  ;; No note or LAST_REPEAT when a repeating task is done. Not set: :time.
  ;; (setf *org-log-repeat* nil)

  ;; The date a task had logged when it is rescheduled. Not set: nil.
  ;; (setf *org-log-reschedule* :time)

  ;; The deadline a task had logged when it changes. Not set: nil.
  ;; (setf *org-log-redeadline* :time)

  ;; The files the agenda reads, and where capture puts tasks; folders and * ?
  ;; patterns too. Not set: '("~/org/agenda.org").
  ;; (setf *org-agenda-files* '("~/notes/*.org"))

  ;; Changes made from the agenda saved at once: t for every agenda file, or a
  ;; list. Not set: nil: saved with s.
  ;; (setf *org-agenda-autosave-files* t)

  ;; The agenda lists what was closed and clocked each day. Not set: nil.
  ;; (setf *org-agenda-log-mode* t)

  ;; Deadlines listed today from 7 days before. Not set: 14.
  ;; (setf *org-deadline-warning-days* 7)

  ;; Agenda entries with this tag are told on the desktop as their time comes,
  ;; while Tuatara runs; nil tells none. Not set: "notify".
  ;; (setf *org-notify-tag* "remind")

  ;; Minutes before its time an entry tagged :notify: is told, as well as at
  ;; it; 0 for only at it. A NOTIFY_BEFORE property says otherwise for one
  ;; entry. Not set: 10.
  ;; (setf *org-notify-before* 15)

  ;; When an entry tagged :notify: with a date and no time is told that day;
  ;; nil never. Not set: "08:00".
  ;; (setf *org-notify-day-time* "07:30")

  ;; The program that shows notifications on the desktop, the agenda's and
  ;; packages'; they are kept in *Notifications* (C-h N) too. Not set:
  ;; "notify-send".
  ;; (setf *notify-program* "/usr/bin/notify-send")

  ;; C-x a a a shows one day: :day :week :month :year or a number of days. Not
  ;; set: :week.
  ;; (setf *org-agenda-span* :day)

  ;; Weeks start on Sunday, 1 Monday to 7 Sunday; nil on today. Not set: 1.
  ;; (setf *org-agenda-start-on-weekday* 7)

  ;; No time grid on today. Not set: 8:00 to 20:00 every two hours.
  ;; (setf *org-agenda-time-grid* nil)

  ;; The agenda's first column, each line's category (its file's name or
  ;; #+CATEGORY:), 16 wide, longer ones cut short with …. Not set: 12.
  ;; (setf *org-agenda-category-width* 16)

  ;; Lines with no time of day leave the time column empty. Not set: "all
  ;; day"; nil leaves the column out.
  ;; (setf *org-agenda-untimed-text* "")

  ;; Lines of an event, a date in the text with no Scheduled: or Deadline:,
  ;; leave the column of kinds empty. Not set: "Event:"; nil leaves it out.
  ;; (setf *org-agenda-event-kind-text* "")

  ;; Lines of a date in a task's text (not its SCHEDULED: or DEADLINE:) say
  ;; Date: instead. Not set: "Task:".
  ;; (setf *org-agenda-task-kind-text* "Date:")

  ;; Dates written in a heading or a checkbox's line stay in its title on
  ;; the agenda's days, as Org shows them. Not set: taken out there.
  ;; (setf *org-agenda-date-in-title* t)

  ;; No line for the time now in the grid. Not set: "← now ───".
  ;; (setf *org-agenda-current-time-string* nil)

  ;; An agenda shown is drawn again every 5 minutes, and at midnight; nil only
  ;; at midnight and when an agenda file changes. Not set: 10.
  ;; (setf *org-agenda-refresh-minutes* 5)

  ;; C-x a a w: the tasks tagged work (u: untagged tasks)
  ;; (push '("w" "Work" :tags-todo "work" :header "Work")
  ;;       *org-agenda-custom-commands*)

  ;; C-c C-q puts tags to end at column 77, as Org; positive: to start there. Not
  ;; set: 0: a space after the text.
  ;; (setf *org-tags-column* -77)

  ;; C-c C-w moves a heading under these files' headings, to that level.
  ;; (setf *org-refile-targets*
  ;;       '((:agenda-files . 3) (:this-file . 3)
  ;;         ("~/notes/projects.org" . 2)))

  ;; Your capture templates are there, a file each; M-x copy-capture-template
  ;; copies one of Tuatara's (t TODO, n note, j journal, m meeting). Not set:
  ;; nil: capture-templates/ in the configuration folder,
  ;; ~/.config/tuatara/capture-templates/.
  ;; (setf *capture-template-directories* (list "~/notes/capture/"))

  ;; Capture from anywhere with C-c c (C-x a c)
  ;; (bind-key "C-c c" 'org-capture)

  ;; Org text read up to 500 elements one within another, deeper ones as
  ;; plain text. Not set: 200.
  ;; (setf *org-deepest* 500)

  ;; A table formula nested up to 500 deep in brackets, signs and powers,
  ;; deeper ones refused. Not set: 200.
  ;; (setf *org-formula-deepest* 500)
  )

;;; Rimu: lists of folders

(load-package "rimu"
  :config

  ;; C-x d lists one, C-x C-j the current file's; in a list l opens, h goes up.

  ;; Details before each name, as ls -l. Not set: nil; I in a list.
  ;; (setf *rimu-details* t)

  ;; Permissions as d755, -644. Not set: :symbolic, drwxr-xr-x.
  ;; (setf *rimu-permissions* :octal)

  ;; Details without these: :inode :permissions :links :owner :group :size :time
  ;; :sizes :git. Not set: none.
  ;; (setf *rimu-hidden-columns* '(:inode :group))

  ;; Names starting with a dot shown. Not set: nil; H in a list.
  ;; (setf *rimu-dotfiles* t)

  ;; The latest changed first. Not set: :name; s in a list.
  ;; (setf *rimu-sort* :time)

  ;; Folders among the rest. Not set: t: first.
  ;; (setf *rimu-directories-first* nil)

  ;; Names with these endings dimmed. Not set: compiled files, backups ...
  ;; (setf *rimu-ignored-extensions* '(".o" "~"))

  ;; No preview beside the list. Not set: t; P in a list.
  ;; (setf *rimu-preview* nil)

  ;; Seconds the cursor rests on an entry before the preview shows it. Not set:
  ;; 0.1.
  ;; (setf *rimu-preview-delay* 0.3)

  ;; The preview shows up to 4 MB of a file's start. Not set: 1 MB.
  ;; (setf *rimu-preview-size* (* 4 1024 1024))

  ;; Each entry's last commit after its name. Not set: nil; G in a list.
  ;; (setf *rimu-git-info* t)

  ;; Each entry's size, a folder's with all under it. Not set: nil; S in a list.
  ;; (setf *rimu-directory-sizes* t)

  ;; D and x delete for good, asked twice. Not set: t: to the trash.
  ;; (setf *rimu-delete-to-trash* nil)

  ;; Lists read again only with r. Not set: t: when their folder changes.
  ;; (setf *rimu-follow-changes* nil)

  ;; A bookmark to a folder, or a list going out of a project, lists it in a new
  ;; list. Not set: t: in the list of its project used last (of no project for a
  ;; folder in none), moved there, when there is one.
  ;; (setf *rimu-reuse-lists* nil)

  ;; Copying, moving and deleting folders go down through 400 folders one
  ;; inside another; deeper is refused. Not set: 200.
  ;; (setf *deepest-folder-tree* 400)
  )

;;; CSV files

(load-package "csv"
  :config

  ;; C-c C-s sets the separator of one, C-x / L keeps its header at the top,
  ;; C-x / t takes its first line as a header or a record.

  ;; Fields always apart by semicolons. Not set: :detect: what the first line has
  ;; most of.
  ;; (setf *csv-separator* #\;)

  ;; The header of a CSV, or of a log once C-x / t makes one, kept at the top as
  ;; the rest scrolls. Not set: nil.
  ;; (setf *list-header-lock* t)

  ;; The first line a record like the rest, not a header naming the columns.
  ;; Not set: t.
  ;; (setf *csv-header* nil)
  )

;;; The terminal (M-x terminal)

(load-package "terminal"
  :config

  ;; Keys kept for the editor while you type in it, the rest going to the program.
  ;; Not set: as standard.
  ;; (setf *terminal-editor-keys* '("C-x" "M-x" "C-g" "C-\\" "C-q" "C-s"))

  ;; C-g to the program, C-\ alone stopping typing there.
  ;; (setf *terminal-editor-keys*
  ;;       (remove "C-g" *terminal-editor-keys* :test #'string=))

  ;; Keys that paste into it, a shell running nothing until RET. Not set: as
  ;; standard.
  ;; (setf *terminal-paste-keys* '("C-V" "S-<insert>"))

  ;; Columns of its title in the mode line. Not set: 40; nil: none.
  ;; (setf *terminal-title-max* 60)

  ;; Where it shows: :bottom :top :left :right, or :full, the window you are in.
  ;; Not set: :bottom.
  ;; (setf *terminal-place* :right)

  ;; At the bottom or top: rows, or a fraction of the height. Not set: 0.3.
  ;; (setf *terminal-size* 15)

  ;; At the left or right: columns, or a fraction of the width. Not set: 0.5.
  ;; (setf *terminal-width* 80)

  ;; One key to show it and type there, or hide it (the same terminal, still
  ;; running)
  ;; (bind-key "C-`" 'terminal-toggle)

  ;; What it runs. Not set: nil: your shell, $SHELL.
  ;; (setf *terminal-program* '("bash" "-l"))

  ;; Lines it keeps that have gone up off its screen. Not set: 100,000.
  ;; (setf *terminal-scrollback* 500000)

  ;; Seconds quiet before its history is compressed. Not set: 10; nil: never.
  ;; (setf *terminal-compress-after* 30)

  ;; Octets of pictures it keeps for kitty icat, yazi ... Not set: 64 MB; 0: none.
  ;; (setf *terminal-picture-memory* (* 256 1024 1024))
  )

;;; Packages

;; Where packages come from: a name for a git repository anyone hosts, or for a
;; folder of your disk, for load-package's :source, each kept in
;; *package-sources*. Not set: none.
;; (package-source "mine" "https://example.org/my-packages.git")

;; A package from a source: fetched in the background the first time (the
;; repository's newest tag by date), its folder example alone, kept for the next
;; start; asked about before it is compiled and loaded. M-x package-manager
;; lists them, M-x package-update fetches them again.
;; (load-package "example" :source "mine")

;; Its settings and keys, set once it has loaded (for one fetched, once the
;; fetching is done): the forms after :config.
;; (load-package "example" :source "mine"
;;   :config
;;   (setf *example-arguments* "-H")
;;   (bind-key "C-c r" 'example-run :mode 'csv))

;; A block left out as a whole: :disabled t, or :if with a form that gives
;; nil, as on another machine. Not set: every block read.
;; (load-package "example" :source "mine" :disabled t)
;; (load-package "looks" :if (string= (machine-instance) "laptop")
;;   :config
;;   (setf *gui-font* "DejaVu Sans Mono 14"))

;; Loaded later, so that Tuatara starts sooner: once the editor has started
;; (:defer t), when one of its commands is first run (:commands), or when a
;; buffer is first in one of its modes (:modes). :after waits for another
;; package to load first; :init runs before it loads. Each package loaded is
;; said in *Messages* with the time it took. Not set: loaded at once.
;; (load-package "example" :source "mine"
;;   :commands example-run example-run-buffer
;;   :modes csv
;;   :after "writing"
;;   :init
;;   (setf *example-program* "/usr/local/bin/example"))

;; A version of your choosing: a tag, branch or commit as written, or "latest"
;; for the newest commit. :file names its first file when it is not
;; example.lisp or load.lisp, :folder its folder when that is not example/. Not
;; set: the newest tag.
;; (load-package "example" :source "mine" :version "v1.2.0" :folder "lisp"
;;               :file "start.lisp")

;; A package from a folder of your disk, without naming a source first.
;; (load-package "mine" :source "~/src/mine/")

;; Never asked before a package is compiled and loaded. Not set: t: asked, y,
;; n, or v to read its source first.
;; (setf *package-ask-before-loading* nil)

;; A package fetched may take up to 2 GB on disk, its history and files
;; together; a larger fetch is ended and its files deleted. Not set: 512 MB.
;; (setf *package-most-octets* (* 2 1024 1024 1024))
