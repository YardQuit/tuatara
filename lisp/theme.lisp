;;;; lisp/theme.lisp --- The default faces.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Colours are the sixteen terminal colours by name, so the default theme
;;;; follows the terminal's palette on dark and light backgrounds alike. An init
;;;; file changes any face with SET-FACE-ATTRIBUTES.

(in-package #:tuatara.defaults)

;;; Text and the interface

(define-face :default "Text with no other face, and the background: plain, so the terminal's own
colours show until a theme sets them.")
(define-face :mode-line "The mode line under the selected window." :reverse t)
(define-face :mode-line-inactive "The mode lines under the windows other than the selected one." :reverse t :dim t)
(define-face :vertical-border "The line between windows side by side." :dim t)
(define-face :mode-line-directory "The buffer's directory at the end of the mode line.")
(define-face :minibuffer-prompt "The prompt of the minibuffer." :bold t)
(define-face :completion-selected "The selected completion candidate." :reverse t)
(define-face :completion-annotation "Annotations beside completion candidates: documentation, and what else has no face of its own." :dim t)
(define-face :completion-group-title "The title of a group of completion candidates, such as the directory of the files under it." :bold t)
(define-face :completion-group-line "The line either side of the title of a group of completion candidates." :dim t)
(define-face :completion-match-0 "The part of a completion candidate the first word typed matches." :fg :blue :bold t)
(define-face :completion-match-1 "The part of a completion candidate the second word typed matches." :fg :magenta :bold t)
(define-face :completion-match-2 "The part of a completion candidate the third word typed matches." :fg :green :bold t)
(define-face :completion-match-3 "The part of a completion candidate the fourth and later words typed match." :fg :yellow :bold t)
(define-face :completion-key "Key bindings beside the commands among completion candidates." :bold t)
(define-face :input-method-pending "Keys typed under an input method that wait for the next key to settle them (an a under swedish-postfix, waiting to see whether an a, an e or another key comes), underlined." :underline t)
(define-face :completion-major-mode "The major mode beside a buffer among completion candidates." :fg :cyan)
(define-face :completion-modified "The mark beside a buffer among completion candidates that has changes not saved." :fg :yellow)
(define-face :completion-safe "The mark beside a file among completion candidates that was opened safely, and opens so again." :fg :yellow :bold t)
(define-face :completion-formatted-keywords "The keywords of a file with a formatted name, from the name, in the list \\[open-file-formatted] shows." :fg :blue)
(define-face :completion-mode-none "A permission a file does not give (-), beside it among completion candidates." :dim t)
(define-face :completion-mode-directory "The d of a directory's permissions beside it among completion candidates." :fg :blue)
(define-face :completion-mode-link "The l of a symbolic link's permissions beside it among completion candidates." :fg :cyan)
(define-face :completion-mode-read "The permission to read (r) beside a file among completion candidates.")
(define-face :completion-mode-write "The permission to write (w) beside a file among completion candidates." :fg :red)
(define-face :completion-mode-exec "The permission to run or enter (x) beside a file among completion candidates." :fg :green)
(define-face :completion-mode-other "Other letters of a file's permissions (s, t, and the kinds of special files) beside it among completion candidates." :fg :yellow)
(define-face :popup "The list of completions beside the cursor (\\<lisp-mode>\\[completion-at-point]), and the documentation beside it: their text and ground." :fg :black :bg :white)
(define-face :popup-current "The chosen completion in the list beside the cursor." :fg :white :bg :blue :bold t)
(define-face :popup-annotation "What is said of a completion in the list beside the cursor: what it is." :fg :blue)
(define-face :popup-bar "The scroll bar of the list of completions beside the cursor." :bg :black)
(define-face :popup-heading "The first line of the documentation beside the list of completions: the name and its parameters." :bold t)
(define-face :key-hint-key "Keys in the panel that lists the bindings under a prefix key." :bold t)
(define-face :help-key-binding "Keys named in help, documentation and messages: those that run a command now." :bold t)
(define-face :key-hint-command "Commands in the key-hint panel shown after a prefix key.")
(define-face :key-hint-prefix "Prefix keys (further keymaps) in the key-hint panel." :fg :blue)
(define-face :menu-argument-on "An argument a menu has on (a switch, an option given a value): what its actions are given." :inherit :key-hint-key)
(define-face :menu-argument-off "An argument a menu has off: shown, not given." :dim t)
(define-face :header-line "Header lines of lists and tables, such as the column titles of *Health*." :bold t :underline t)
;;; Lists: one face for each kind of thing a list shows, the same in every
;;; list that shows it, so that a theme sets each once

(define-face :list-marked "An entry marked (>) in any list whose entries are marked, rimu's too, for the commands that act on those marked: the mark and the name, in the colour of warnings." :inherit :warning)
(define-face :list-deletion "An entry marked for deletion (D) in any list whose entries are marked, rimu's too: the mark and the name, in the colour of errors." :inherit :error)
(define-face :list-marked-other "An entry given a mark of another kind in a rimu list (a master's M, RIMU-MARK-MASTER): the mark and the name." :fg :cyan :bold t)
(define-face :list-filters "The line above a list narrowed by search filters (\\[search-filter-regex]), saying by what and how many entries are left." :inherit :warning)
(define-face :list-special "Something Tuatara made rather than a file of yours, in a list: the names in stars in the buffer list (\\[buffers]), *Messages* and *scratch*." :fg :green)
(define-face :list-read-only "Something that cannot be changed, in a list: the names of read-only buffers in the buffer list (\\[buffers])." :fg :blue)
(define-face :list-dim "What is of less interest in a list, set back: help buffers in the buffer list (\\[buffers]), packages not loaded, bookmarks whose file is gone, files ignored in rimu and in a git status, a value missing in the health report (\\[check-health]), a line saying more are not shown." :fg :bright-black)
(define-face :list-directory "The names of folders in a list: in rimu (\\[rimu]) and where a link to a folder leads, in the trash (\\[rimu-trash]) and among bookmarks (\\[bookmark-manager])." :fg :blue)
(define-face :list-date "Dates and times in a list or beside completion candidates: when a file last changed, the date in a formatted name, a rimu list's last commits (\\<rimu-mode>\\[rimu-toggle-git]), when a message was sent." :fg :blue)
(define-face :list-size "Sizes in a list or beside completion candidates: of files and buffers, and a rimu list's sizes column (\\<rimu-mode>\\[rimu-toggle-sizes])." :fg :cyan)
(define-face :list-note "What is said of an entry beside it in a list: the first line of a rimu list's last commit messages, the first sentence of a face's description in \\[list-faces], the details under a finding in a report." :fg :bright-black)
(define-face :list-name "The name of a thing set apart in a list, above what it holds: a font's family over its variants in \\[list-fonts]." :fg :magenta :bold t)
(define-face :list-label "The name of a field before its value, in a report or a message's details (Subject:, Verdict)." :fg :yellow)
(define-face :list-good "A state that is as it should be, in a list: a check that passed in the health report (\\[check-health])." :fg :green :bold t)
(define-face :list-bad "A state that is not as it should be, in a list: a check that failed in the health report (\\[check-health])." :fg :red :bold t)
(define-face :font-list-line "The line for the init file that chooses a font, in the list of fonts (\\[list-fonts])." :fg :green)

;;; Rimu

(define-face :rimu-symlink "The names of symbolic links in a rimu list." :fg :cyan)
(define-face :rimu-broken-symlink "Symbolic links in a rimu list that lead nowhere, and what they hold." :fg :red)
(define-face :rimu-special "The names of named pipes, sockets and devices in a rimu list." :fg :magenta)
(define-face :rimu-perm-write "The permission to write (w) of the group and of others, among the details of an entry in a rimu list." :fg :bright-black)
(define-face :rimu-set-id "The set-user, set-group and sticky letters (s, S, t, T) among the permissions of an entry in a rimu list." :fg :red :bold t)
(define-face :mode-line-sign-idle "The mode line's signs before [-] (P, U and S) while their work is not going on:
faint, barely seen. Unless set, its colour is worked out from the mode line's
as the theme in force has it, its ground moved a sixth of the way towards its
text; dim where the mode line's colours are not known." :dim t)

;;; Editing

(define-face :region "The active region (selection)." :reverse t)
(define-face :banked-line "Lines banked with NORMAL state's \\<normal>\\[bank-selected-lines],
set aside for \\[copy-selection-or-char], \\[delete-selection-or-char] or \\[yank-over-selection] to take together."
  :fg :white :bg :blue :extend t)
(define-face :cursor "The cursor in a window of Tuatara's own when no colour is set for it
(*CURSOR-COLOUR*): its colour, the text's as standard, and the character's under it."
  :reverse t)
(define-face :cursor-inactive "Point in a window the typing does not go to: a window not
selected, or the selected one while the minibuffer or a question in the echo
area waits for typing, the cursor standing there. A box round the character
in a window of its own, a line under it in a terminal."
  :underline :box)
(define-face :split-place "The places a split holds: the matches of
\\<normal>\\[split-selection], what the cursors select."
  :fg :black :bg :magenta)
(define-face :split-cursor "The cursors of a split other than the real one, in NORMAL state." :reverse t)
(define-face :split-cursor-insert "The cursors of a split other than the real one, in INSERT state."
  :fg :white :bg :red)
(define-face :job-queued
  "Text a job in the background (a replacement, a sort, a rectangle's change)
has still to work through (for a replacement only its matches), all of
what it changes again while the lines of its new text are counted, until it
is taken in; and a job's state in the list of jobs (\\[list-jobs]). What it
has been through looks as it does meanwhile."
  :extend t)
(define-face :job-waiting "Text a job waiting its turn behind another in the background will change." :extend t)
(define-face :search-current "The current match of an incremental search." :reverse t :bold t)
(define-face :search-other "The other matches of the current search in view." :bold t :underline t)
(define-face :search-outside-scope
  "Text outside the scope of a search or replacement limited to a line or the
region, faded so that the scope stands out."
  :dim t)
(define-face :replace-going
  "A match a replacement being typed (r r, r q) takes away, shown before RET
beside what takes its place: as a difference shows text taken away, on a
ground of its own so that it reads the same over any mode's colours."
  :fg :black :bg :red)
(define-face :replace-coming
  "What a replacement being typed (r r, r q) puts in place of a match, shown
beside the match before RET, as no text of the buffer: as a difference shows
text added, on a ground of its own."
  :fg :black :bg :green :bold t)
(define-face :match "A match found by search or replace." :bold t)
(define-face :paren-match "The bracket at point and the one it pairs with (show-paren-mode)." :fg :black :bg :cyan)
(define-face :paren-mismatch "A bracket at point that pairs with one of another kind, or with none (show-paren-mode)."
  :fg :white :bg :magenta)

;;; Gutter

(define-face :line-number "Line numbers in the gutter." :dim t)
(define-face :current-line-number "The current line's number in the gutter." :bold t)
(define-face :hl-line "The line point is on, in the selected window, out to its edge (\\[hl-line-mode], \\[global-hl-line-mode])." :extend t)
(define-face :cursor-column "The column point is in, down the selected window (\\[cursor-column-mode], \\[global-cursor-column-mode]): the same as :hl-line unless set apart, (set-face :cursor-column :bg ...) in the init file." :inherit :hl-line)
(define-face :soft-wrap-row "The row point is on, where long lines wrap (\\[soft-wrap-mode]), painted over the line's :hl-line: the same as it unless set apart, (set-face :soft-wrap-row :bg ...) in the init file." :inherit :hl-line :extend t)
(define-face :visual-line "The line down the window at the fill column (\\[visual-line-mode]): its background, over a source block's or the selection's, under the line point is on (:hl-line)." :bg :bright-black)
(define-face :wrap-indicator "The mark in the gutter beside a row that goes on from the one above, where long lines wrap (\\[soft-wrap-mode])." :fg :bright-black)
(define-face :vc-added "Gutter mark for lines added since the last commit." :fg :green :bold t)
(define-face :vc-changed "Gutter mark for lines changed since the last commit." :fg :yellow :bold t)
(define-face :vc-deleted "Gutter mark after lines deleted since the last commit." :fg :red :bold t)

;;; Syntax, named after the tree-sitter capture names they are used for

(define-face :comment "Comments." :fg :bright-black :italic t)
(define-face :string "String and character literals." :fg :green)
(define-face :escape "Escape sequences inside strings." :fg :cyan :bold t)
(define-face :raw-byte "Octets that are not UTF-8, each shown as \\xNN." :fg :red :bold t)
(define-face :invisible-character "A character that draws nothing, shown as its code, <FEFF> for the byte-order mark, while its buffer shows such characters (\\[invisible-characters-mode]): as escape sequences are, unless set apart." :inherit :escape)
(define-face :hex-offset "The offsets at the start of the hex view's rows." :dim t)
(define-face :hex-mirror "The octet at the hex view's cursor, marked in both its hex digits and its character, the cursor on the one typing goes to." :inherit :paren-match)
(define-face :hex-unprintable "The dots the hex view's text column shows for octets that are not printable." :dim t)
(define-face :keyword "Keywords of the language." :fg :magenta)
(define-face :function "Names of functions where they are defined or called." :fg :blue)
(define-face :type "Names of types and classes." :fg :yellow)
(define-face :constant "Constants and literals such as true, nil and numbers." :fg :red)
(define-face :number "Numeric literals." :inherit :constant)
(define-face :builtin "Built-in functions, types and variables." :fg :cyan)
(define-face :property "Fields, properties and keys." :fg :cyan)
(define-face :variable "Variables; plain by default.")
(define-face :operator "Operators; plain by default.")
(define-face :punctuation "Brackets, delimiters and separators; plain by default.")
(define-face :tag "Tags in markup languages." :fg :blue)
(define-face :attribute "Attributes, annotations and decorators." :fg :yellow)
(define-face :label "Labels." :fg :yellow)
(define-face :preprocessor "Preprocessor directives." :fg :magenta)
(define-face :heading "Headings in prose and markup." :fg :blue :bold t)
(define-face :emphasis "Emphasised text in markup." :italic t)
(define-face :strong "Strongly emphasised text in markup." :bold t)
(define-face :link "Links and URLs." :fg :blue :underline t)

;;; Syntax faces for finer tree-sitter captures. A capture such as
;;; keyword.control.conditional takes the face of its longest leading part that
;;; is a face (here :keyword), so these are only the parts a theme may want to
;;; colour apart from the part before them.

(define-face :constructor "Constructors: enum variants and structures built by name." :inherit :type)
(define-face :constant.character.escape "Escape sequences in strings, captured as tree-sitter names them." :inherit :escape)
(define-face :constant.numeric "Numbers, captured as tree-sitter names them." :inherit :number)
(define-face :string.regexp "Regular expressions written as strings." :inherit :string)
(define-face :variable.parameter "Parameters of functions and closures." :inherit :variable)
(define-face :variable.other "Other variables: fields and members." :inherit :property)
(define-face :punctuation.bracket "Brackets, braces and parentheses." :inherit :punctuation)
(define-face :punctuation.special "Punctuation with a meaning of its own, such as string interpolation." :inherit :punctuation)
(define-face :keyword.operator "Keywords that are operators, such as and, or and as." :inherit :keyword)
(define-face :keyword.directive "Preprocessor directives and pragmas, captured as tree-sitter names them." :inherit :preprocessor)
(define-face :keyword.storage.modifier "Storage modifiers such as mut, static and const." :inherit :keyword)
(define-face :function.method "Methods, where defined or called." :inherit :function)
(define-face :function.macro "Macros, where defined or called." :inherit :function)
(define-face :function.special "Special functions, such as preprocessor macros." :inherit :function)
(define-face :namespace "Namespaces, modules and packages.")

;;; Markup, differences and logs: the captures of Markdown, LaTeX, HTML, diffs
;;; and log files

(define-face :markup.heading "Headings in markup, and the marks that make them (the # of Markdown)." :inherit :heading)
(define-face :markup.heading.1 "Headings of the first level in markup." :inherit :markup.heading)
(define-face :markup.heading.2 "Headings of the second level in markup." :inherit :markup.heading)
(define-face :markup.heading.3 "Headings of the third level in markup." :inherit :markup.heading)
(define-face :markup.heading.4 "Headings of the fourth level in markup." :inherit :markup.heading)
(define-face :markup.heading.5 "Headings of the fifth level in markup." :inherit :markup.heading)
(define-face :markup.heading.6 "Headings of the sixth level in markup." :inherit :markup.heading)
(define-face :markup.list "The marks that begin the items of lists in markup." :fg :yellow)
(define-face :markup.bold "Bold text in markup." :inherit :strong)
(define-face :markup.italic "Italic text in markup." :inherit :emphasis)
(define-face :markup.strikethrough "Text struck through in markup; plain by default.")
(define-face :markup.link "Links in markup." :inherit :link)
(define-face :markup.link.url "The addresses links in markup lead to." :inherit :link)
(define-face :markup.link.label "The labels links in markup refer to, such as [label]." :fg :blue)
(define-face :markup.link.text "The text links in markup show." :fg :blue :bold t)
(define-face :markup.quote "Quotations in markup." :italic t)
(define-face :markup.raw "Code and verbatim text in markup." :inherit :string)
(define-face :markup.raw.block "Blocks of code and verbatim text in markup." :inherit :markup.raw)
(define-face :diff.plus "Lines a difference between files adds." :fg :green)
(define-face :diff.minus "Lines a difference between files takes away." :fg :red)
(define-face :diff.delta "Lines a difference between files changes." :fg :yellow)
(define-face :git-status-staged "A file staged, under Staged changes in a git status, apart from those not staged yet: in the theme's colour for added lines." :inherit :diff.plus)
(define-face :git-status-deleted "A file deleted and staged, under Staged changes in a git status: in the theme's colour for lines taken away." :inherit :diff.minus)
(define-face :git-status-branch "The branch you are on, at the top of a git status: lit up, to be seen at once." :inherit :heading)
(define-face :git-status-commit "The latest commits at the end of a git status, and the stashes: a colour of their own." :fg :red)
(define-face :conflict-marker "The marker lines of a conflict a merge left in a file (\\[conflict-markers-mode]): <<<<<<<, ||||||| and >>>>>>> with the names after them, and =======, out to the window's edge." :bold t :extend t)
(define-face :conflict-first "The first side of a conflict a merge left in a file, the lines after its <<<<<<< line: as the lines a difference takes away unless set." :inherit :diff.minus :extend t)
(define-face :conflict-base "The base of a conflict a merge left in a file, the lines after its ||||||| line, as they were before either side changed them." :fg :yellow :extend t)
(define-face :conflict-second "The second side of a conflict a merge left in a file, the lines after its ======= line: as the lines a difference adds unless set." :inherit :diff.plus :extend t)
(define-face :error "Errors marked in text, such as the error lines of a log." :fg :red :bold t)
(define-face :warning "Warnings marked in text, such as the warning lines of a log." :fg :yellow :bold t)
(define-face :mode-line-rewind "Rewind and the commit shown, in the mode line of a file or folder shown as a commit left it (\\[git-rewind]): lit up in the theme's warning colour, so that it is seen at once that this is not the file as it is now." :inherit :warning :reverse t)
(define-face :mode-line-narrowed "Narrow, in the mode line of a buffer narrowed to a part of it
\(\\[narrow-to-region], \\[narrow-to-section]): lit up in the theme's warning
colour, so that it is seen at once that the rest of the buffer is out of
sight and out of reach until \\[widen]."
  :inherit :warning :reverse t)
(define-face :mode-line-evidence "Evidence, in the mode line of a file in an
evidence folder (*EVIDENCE-FOLDERS*): lit up in the theme's error colour, so
that it is seen at once that the file opened safely and nothing is written
where it is."
  :inherit :error :reverse t)
(define-face :mode-line-sign "The mode line's signs before [-] while their work goes on: P while a job of the buffer runs or waits its turn (*processing-indicator*), U while a directory of *indexed-directories* is being read (*indexing-indicator*), S while one is searched (*searching-indicator*); in the theme's warning colour, to stand out." :inherit :warning)
(define-face :mode-line-recording "Rec in the mode line while keys are recorded (\\[start-recording-keys]), and how many plays are done while recorded keys play over and over (\\[play-recorded-keys]); as the signs of work, in the theme's warning colour." :inherit :mode-line-sign)
(define-face :terminal-bold "Text a program in a terminal set in bold." :bold t)
(define-face :terminal-italic "Text a program in a terminal set in italics." :italic t)
(define-face :terminal-underline "Text a program in a terminal underlined." :underline t)
(define-face :terminal-dim "Text a program in a terminal set faint." :dim t)

(define-face :terminal-reverse "Text a program in a terminal shown in reverse, its colours swapped." :reverse t)
(define-face :spellcheck "A word spelt wrong (\\[spellcheck-mode], \\[global-spellcheck-mode]): underlined, wavy in a window of its own as standard; (set-face :spellcheck :underline :dotted) in the init file for dots, :underline t for a line, :underline-colour for its colour." :underline :wave :underline-colour :yellow)
(define-face :info "Information marked in text, such as the info lines of a log." :fg :blue)
(define-face :diagnostic-error "The text a language server finds an error in: underlined, wavy." :underline :wave :underline-colour :red)
(define-face :diagnostic-warning "The text a language server warns of: underlined, wavy." :underline :wave :underline-colour :yellow)
(define-face :diagnostic-info "The text a language server says something of: underlined, dotted." :underline :dotted :underline-colour :blue)
(define-face :diagnostic-hint "The text a language server hints at: underlined, dotted." :underline :dotted :underline-colour :bright-black)
(define-face :diagnostic-error-ground "The text a language server finds an error in, with *lsp-diagnostics-style* :background: on the theme's error ground." :fg :white :bg :red)
(define-face :diagnostic-warning-ground "The text a language server warns of, with *lsp-diagnostics-style* :background: on the theme's warning ground." :fg :black :bg :yellow)
(define-face :diagnostic-info-ground "The text a language server says something of, with *lsp-diagnostics-style* :background: on the theme's information ground." :fg :black :bg :cyan)
(define-face :diagnostic-hint-ground "The text a language server hints at, with *lsp-diagnostics-style* :background: faint." :dim t)
(define-face :code-lens "What a language server says of a line (3 references, Run test), at its end: as an inlay hint." :inherit :inlay-hint)
(define-face :symbol-highlight "The name the cursor rests on, and its other uses the language server finds in the window: bold." :bold t)
(define-face :inlay-hint "What a language server shows within the code, no text of it (a type, a parameter's name): faint." :dim t :italic t)
(define-face :hint "Hints and debugging marked in text, such as the debug lines of a log." :fg :bright-black)
(define-face :compile-error "The lines of a compile's output (\\[compile]) that say an error, and the line saying the command failed: as errors are marked in text." :inherit :error)
(define-face :compile-warning "The lines of a compile's output (\\[compile]) that say a warning: as warnings are marked in text." :inherit :warning)
(define-face :compile-note "The lines of a compile's output (\\[compile]) that say a note or give help: as information is marked in text." :inherit :info)

;;; CSV and other delimited data: one colour per column, cycling

(define-face :csv-column-0 "Delimited data: columns 0, 8, 16 ...")
(define-face :csv-column-1 "Delimited data: columns 1, 9, 17 ..." :fg :red)
(define-face :csv-column-2 "Delimited data: columns 2, 10, 18 ..." :fg :green)
(define-face :csv-column-3 "Delimited data: columns 3, 11, 19 ..." :fg :yellow)
(define-face :csv-column-4 "Delimited data: columns 4, 12, 20 ..." :fg :blue)
(define-face :csv-column-5 "Delimited data: columns 5, 13, 21 ..." :fg :magenta)
(define-face :csv-column-6 "Delimited data: columns 6, 14, 22 ..." :fg :cyan)
(define-face :csv-column-7 "Delimited data: columns 7, 15, 23 ..." :fg :bright-black)
(define-face :csv-separator "Delimited data coloured sparsely (set-colouring): the separators between fields." :fg :cyan)

;;; Folds

(define-face :fold-ellipsis "The ellipsis standing for folded text, as an Org subtree folded shows." :dim t)

;;; Org, the agenda and the calendar

(define-face :org-level-1 "Org headlines of the first level, their stars and titles." :inherit :heading)
(define-face :org-level-2 "Org headlines of the second level." :inherit :heading)
(define-face :org-level-3 "Org headlines of the third level." :inherit :heading)
(define-face :org-level-4 "Org headlines of the fourth level." :inherit :heading)
(define-face :org-level-5 "Org headlines of the fifth level." :inherit :heading)
(define-face :org-level-6 "Org headlines of the sixth level." :inherit :heading)
(define-face :org-level-7 "Org headlines of the seventh level." :inherit :heading)
(define-face :org-level-8 "Org headlines of the eighth level and deeper." :inherit :heading)
(define-face :org-todo "Org's keywords of tasks not done (TODO)." :fg :red :bold t)
(define-face :org-done "Org's keywords of tasks done (DONE)." :fg :green :bold t)
(define-face :org-wait "Org's keyword of tasks open but waiting on something (WAIT), apart from TODO." :fg :yellow :bold t)
(define-face :org-cancelled "Org's keyword of tasks cancelled (CANCELLED), closed but apart from DONE." :dim t :bold t :italic t)
(define-face :org-headline-done "The titles of Org headlines whose task is done." :dim t)
(define-face :org-priority "An Org headline's priority, [#A]." :bold t)
(define-face :org-tag "An Org headline's tags, :work:urgent:." :bold t)
(define-face :org-archived "Org headlines tagged ARCHIVE." :dim t)
(define-face :org-document-title "The title an Org document gives itself (#+title:)." :bold t)
(define-face :org-document-info "What an Org document says of itself: its author, date and the like." :bold nil)
(define-face :org-document-info-keyword "The keywords before an Org document's title, author and date (#+title:)." :dim t)
(define-face :org-meta-line "Org's keyword lines (#+name:, #+caption:...)." :dim t)
(define-face :org-block "The contents of Org's blocks (#+begin_src... #+end_src)." :extend t)
(define-face :org-block-begin-line "The first line of an Org block, #+begin_src and its arguments." :dim t :extend t)
(define-face :org-block-end-line "The last line of an Org block, #+end_src." :inherit :org-block-begin-line)
(define-face :org-quote "The contents of Org's quote blocks." :inherit :org-block)
(define-face :org-verse "The contents of Org's verse blocks." :inherit :org-block)
(define-face :org-code "Code in Org text (~code~), and fixed-width lines (: ...)." :inherit :markup.raw)
(define-face :org-verbatim "Verbatim text in Org (=verbatim=)." :inherit :markup.raw)
(define-face :org-bold "Bold text in Org (*bold*)." :bold t)
(define-face :org-italic "Italic text in Org (/italic/)." :italic t)
(define-face :org-underline "Underlined text in Org (_underline_)." :underline t)
(define-face :org-macro "Macro calls in Org ({{{name}}})." :fg :magenta)
(define-face :org-macro-text "The text of an Org macro call shown while its braces are hidden
\({{{person(Ann Taranaki)}}} shows Ann Taranaki, {{{kbd(C-x)}}} C-x), as a link's label is in :org-link." :fg :green)
(define-face :org-link "Links in Org." :inherit :link)
(define-face :org-date "Timestamps in Org (<2026-10-01 Thu>)." :fg :cyan)
(define-face :org-special-keyword "Org's CLOSED: and CLOCK:." :dim t)
(define-face :org-scheduled-keyword "Org's SCHEDULED:, in the colour the agenda gives a task scheduled." :inherit :org-scheduled)
(define-face :org-deadline-keyword "Org's DEADLINE:, in the colour the agenda gives a deadline." :inherit :org-upcoming-deadline)
(define-face :org-drawer "Org's drawers' first and last lines (:PROPERTIES:, :END:)." :dim t)
(define-face :org-log-note "The notes a task's log keeps of its changes of state, wholly, the time
included: - State \"DONE\" from \"TODO\" [2026-10-01 Thu 18:00]." :dim t)
(define-face :org-property-value "The values in Org's property drawers." :fg :blue)
(define-face :org-table "Org's tables." :fg :blue)
(define-face :org-formula "Org's table formulas (#+TBLFM:)." :dim t)
(define-face :org-checkbox "The checkboxes of Org's list items, [ ] and [X]." :bold t)
(define-face :org-mode-line-clock "The clock running on an Org task, in the mode line: [0:22] (Task).")
(define-face :org-list-dt "The terms of Org's description lists (term :: description)." :bold t)
(define-face :org-footnote "Org's footnotes, [fn:1]." :inherit :link)
(define-face :org-agenda-structure "The agenda's headers: Week-agenda (W40):, Global list of TODO items." :fg :blue :bold t)
(define-face :org-agenda-date "A day's date in the agenda: Monday     28 September 2026." :inherit :org-agenda-structure)
(define-face :org-agenda-date-today "Today's date in the agenda." :inherit :org-agenda-date :underline t)
(define-face :calendar-month-header "The months' titles in the calendar (October 2026)." :bold t)
(define-face :calendar-weekday-header "The names of the weekdays over the calendar's columns (Mo Tu)." :fg :cyan)
(define-face :calendar-weekend-header "The names of Saturday and Sunday over the calendar's columns." :fg :yellow)
(define-face :calendar-today "Today in the calendar, on a bright ground so that it stands out." :bold t :reverse t)
(define-face :calendar-marked "Days the calendar marks: those with agenda entries." :fg :blue :bold t)
(define-face :calendar-journal "Days in the calendar with a journal entry, underlined and bold, in the
text's colour, or the colour of agenda entries on a day that has both." :underline t :bold t)
(define-face :calendar-chosen "Days chosen in the calendar for a date asked for, as Org's
date question has them chosen (\\[org-schedule], \\[org-time-stamp])." :fg :magenta :bold t :underline t)
(define-face :calendar-selected "The day the cursor is on in the calendar, both its digits on the cursor's
colour as the cursor's character is (set as the cursor's colour changes)." :reverse t :bold t)
(define-face :org-agenda-date-weekend "The date of a Saturday or a Sunday in the agenda." :inherit :org-agenda-date)
(define-face :org-agenda-date-weekend-today "Today's date in the agenda, on a Saturday or a Sunday." :inherit :org-agenda-date-today)
(define-face :org-time-grid "The lines of today's time grid in the agenda, 8:00 ┄┄┄┄┄." :fg :yellow)
(define-face :org-agenda-current-time "The time now in the agenda's time grid, ← now." :inherit :org-time-grid)
(define-face :org-scheduled "A task scheduled for a day to come, in the agenda." :fg :green)
(define-face :org-scheduled-today "A task scheduled for today, in the agenda." :fg :green)
(define-face :org-scheduled-previously "A task scheduled for a day gone and not done, in the agenda: Sched. 3x:." :fg :red)
(define-face :org-upcoming-deadline "A deadline in the second half of its warning, in the agenda." :fg :red)
(define-face :org-upcoming-distant-deadline "A deadline early in its warning, in the agenda.")
(define-face :org-imminent-deadline "A deadline today, or gone, in the agenda." :fg :red :bold t)
(define-face :org-agenda-done "A task done, and what was closed, in the agenda." :fg :green)
(define-face :org-agenda-calendar-event "An entry at a time or on a range of days, and what was clocked, in the agenda.")
(define-face :org-agenda-category "The category an agenda line begins with, the file's name or its CATEGORY (garden:)." :dim t)
(define-face :org-agenda-time "The time of an agenda line, 9:30-10:15." :fg :cyan)
(define-face :org-agenda-event-kind "The kind of an agenda line of a date in an entry's text: Event:, or Task: for a task's (*org-agenda-event-kind-text*, *org-agenda-task-kind-text*)." :inherit :org-agenda-calendar-event)
(define-face :org-agenda-untimed "The time column of an agenda line with no time of day, all day (*org-agenda-untimed-text*)." :dim t)
(define-face :org-agenda-tags "The tags at the end of an agenda line, :work:meeting:." :dim t)
(define-face :org-latex-and-related "LaTeX in Org text ($x^2$, \\alpha)." :fg :magenta)
(define-face :org-target "Org's targets, <<target>>." :underline t)
(define-face :org-cite "Org's citations, [cite:@key]." :inherit :link)
(define-face :org-hide "The stars of Org's headlines hidden in the indented view: the ground's colour (dim where it is not known)." :dim t)
(define-face :org-leading-stars "The stars of Org's headlines before the last, shown faint (*org-hide-leading-stars* nil)." :dim t)
(define-face :org-list-bullet "The signs list items show for their bullets (*org-list-bullets*)." :bold t)
(define-face :org-checkbox-todo "TODO in the agenda for a list item's checkbox kept as a task, apart from a headline's TODO." :bold t :inherit :org-checkbox)

;;; Themes

(define-theme terminal
  "The terminal's own palette: the faces as defined, in the sixteen terminal
colours, so the editor follows whatever colours the terminal uses.")

(define-theme plain
  "No colours at all, for terminals that have none: bold, underline, reverse
video and dim carry the distinctions. Chosen by itself when the terminal
reports no colours."
  :colourless
  (:mode-line :reverse t) (:mode-line-inactive :reverse t :dim t) (:vertical-border :dim t)
  (:region :reverse t) (:banked-line :reverse t :dim t) (:completion-selected :reverse t)
  (:split-place :underline t) (:split-cursor :reverse t) (:split-cursor-insert :reverse t :underline t)
  (:cursor :reverse t)
  (:search-current :reverse t :bold t) (:search-other :underline t :bold nil) (:search-outside-scope :dim t)
  (:paren-match :bold t :underline t) (:paren-mismatch :reverse t)
  (:minibuffer-prompt :bold t) (:completion-annotation :dim t)
  (:popup :reverse t) (:popup-current :bold t :underline t) (:popup-annotation :dim t) (:popup-bar :underline t)
  (:key-hint-key :bold t) (:key-hint-prefix :underline t)
  (:header-line :bold t :underline t) (:list-deletion :bold t :reverse t) (:list-marked :bold t :underline t)
  (:list-marked-other :bold t :italic t :underline t)
  (:list-special :bold t) (:list-read-only :italic t) (:list-dim :dim t) (:list-directory :bold t)
  (:list-date :dim t) (:list-note :italic t) (:list-good :bold t) (:list-bad :reverse t)
  (:rimu-symlink :underline t) (:rimu-broken-symlink :underline t :dim t)
  (:rimu-special :italic t) (:rimu-perm-write :dim t) (:rimu-set-id :bold t) (:mode-line-sign-idle :dim t)
  (:line-number :dim t) (:current-line-number :bold t)
  (:vc-added :bold t) (:vc-changed :bold t) (:vc-deleted :bold t)
  (:comment :dim t :italic t) (:keyword :bold t) (:escape :bold t) (:raw-byte :reverse t :bold nil)
  (:heading :bold t :underline t) (:link :underline t)
  (:visual-line :reverse t) (:wrap-indicator :dim t) (:hint :dim t)
  (:diagnostic-error-ground :reverse t) (:diagnostic-warning-ground :reverse t :dim t)
  (:diagnostic-info-ground :underline t) (:diagnostic-hint-ground :dim t)
  (:job-waiting :italic t)
  (:diff.plus :bold t) (:diff.minus :dim t) (:completion-modified :bold t)
  (:conflict-marker :reverse t) (:conflict-base :italic t)
  (:replace-going :dim t :underline t) (:replace-coming :reverse t :bold t)
  (:org-date :underline t) (:org-time-grid :dim t) (:org-agenda-done :dim t) (:org-wait :italic t)
  (:org-scheduled-previously :bold t) (:org-upcoming-deadline :bold t) (:org-imminent-deadline :bold t :reverse t)
  (:calendar-weekend-header :bold t) (:calendar-marked :reverse t :bold nil) (:calendar-journal :bold t :underline t)
  (:calendar-today :reverse nil :bold t :italic t) (:calendar-selected :reverse t :bold t :underline t)
  (:calendar-chosen :reverse t :italic t :underline t))

;; The ao themes take their colours from the Ao palette
;; (github.com/YardQuit/ao): the dark variant hex for hex, the light one with
;; its accents darkened for a white ground and its ground and neutrals from the
;; Fedora documentation site. Each palette names Ao's colours first, then the
;; roles the faces take: ao-light takes every face's role from ao-dark, so the
;; two differ only in their palettes. The mode line is the same steel blue in
;; both, keywords and comments are neither bold nor italic, and variables are
;; orange.

(define-theme ao-dark
  "Ao, dark: a deep navy ground with an amber accent, a violet selection and a
steel-blue mode line. From the Ao theme by the same author."
  :palette
  ((deep-abyss "#080d15") (midnight-thunder "#0d1526") (nightfall-blue "#1f2937") (stormy-night "#254862")
   (twilight-blue "#2c5484") (pitch-black "#000000") (winter-sky "#f3f4f6") (ao-white "#dadada")
   (slate-gray "#838a97") (blaze-orange "#ff9000") (lemon-zest "#ffba00") (leafy-green "#81be83")
   (sky-blue "#45b1e8") (dreamy-blue "#6eb0ff") (crystal-blue "#99c7ff") (ruby-glow "#fa7970")
   (walnut-brown "#987654") (rustic-red "#540b0c") (slate-purple "#d2a8ff") (light-purple "#7533bd")
   (deep-purple "#4c1785")
   ;; text and its grounds
   (ground deep-abyss) (ground-dim midnight-thunder) (active-ground stormy-night) (current-line nightfall-blue)
   (text ao-white) (text-dim slate-gray) (text-alt winter-sky) (comment slate-gray) (divider slate-gray)
   (code winter-sky) (quote-text winter-sky) (whitespace stormy-night)
   ;; accents
   (orange blaze-orange) (yellow lemon-zest) (green leafy-green) (red ruby-glow) (blue sky-blue)
   (blue-cooler crystal-blue) (blue-warmer dreamy-blue) (magenta slate-purple) (magenta-faint slate-purple)
   (brown walnut-brown) (link crystal-blue)
   (added leafy-green) (removed ruby-glow) (changed lemon-zest)
   (added-ground "#12301f") (removed-ground "#3a1416")
   ;; the mode line, selections, searches and marks
   (mode-line-ground twilight-blue) (mode-line-text winter-sky) (mode-line-inactive-ground pitch-black)
   (mode-line-directory crystal-blue) (completion-ground twilight-blue)
   (selection light-purple) (selection-text ao-white) (selection-secondary deep-purple)
   (search-ground lemon-zest) (search-text deep-abyss) (search-other-ground twilight-blue) (highlight-text winter-sky)
   (match-ground "#2a1740") (error-ground rustic-red) (split-ground "#4a3a6a")
   (cursor blaze-orange) (paren-ground pitch-black)
   (line-number-current blaze-orange) (line-number-current-ground pitch-black)
   (yellow-subtle "#3a2a12") (blue-subtle "#12253a") (teal-subtle "#10302b"))
  (:default :fg text :bg ground)
  (:mode-line :reverse nil :fg mode-line-text :bg mode-line-ground)
  (:mode-line-inactive :reverse nil :dim nil :fg mode-line-text :bg mode-line-inactive-ground)
  (:vertical-border :dim nil :fg divider)
  (:mode-line-directory :fg mode-line-directory) (:mode-line-narrowed :reverse nil :bold t :fg ground :bg yellow)
  (:mode-line-evidence :reverse nil :bold t :fg ground :bg red)
  (:minibuffer-prompt :bold nil :fg blue-warmer)
  (:completion-selected :reverse nil :bold t :bg completion-ground)
  (:completion-annotation :dim nil :fg text-dim)
  (:completion-group-title :bold t :fg blue) (:completion-group-line :dim nil :fg text-dim)
  (:completion-match-0 :fg blue-cooler) (:completion-match-1 :fg magenta)
  (:completion-match-2 :fg green) (:completion-match-3 :fg yellow)
  (:completion-key :bold t :fg magenta)
  (:completion-major-mode :fg blue) (:completion-modified :fg yellow) (:completion-safe :bold t :fg yellow)
  (:completion-formatted-keywords :fg blue-warmer)
  (:completion-mode-none :dim nil :fg text-dim) (:completion-mode-directory :fg blue-warmer)
  (:completion-mode-link :fg blue) (:completion-mode-read :fg text) (:completion-mode-write :fg red)
  (:completion-mode-exec :fg green) (:completion-mode-other :fg yellow)
  (:popup :fg text :bg ground-dim) (:popup-current :fg nil :bg completion-ground :bold t)
  (:popup-annotation :fg blue-warmer) (:popup-bar :bg text-dim)
  (:key-hint-key :bold t :fg magenta) (:help-key-binding :bold t :fg magenta)
  (:key-hint-command :fg text) (:menu-argument-off :dim nil :fg text-dim)
  (:key-hint-prefix :fg magenta)
  (:header-line :underline nil :fg text :bg ground-dim)
  (:list-marked-other :bold t :fg blue-warmer :underline t)
  (:list-special :fg green) (:list-read-only :fg blue) (:list-dim :fg text-dim) (:list-directory :fg blue-cooler)
  (:list-date :fg blue) (:list-size :fg blue-cooler) (:list-note :fg text-dim) (:list-name :bold t :fg blue-warmer)
  (:list-label :fg text) (:list-good :fg added :bold t) (:list-bad :fg removed :bold t) (:font-list-line :fg blue)
  (:rimu-symlink :fg blue) (:rimu-broken-symlink :fg red) (:rimu-special :fg magenta) (:rimu-perm-write :fg text-dim)
  (:rimu-set-id :bold t :fg red)
  (:region :reverse nil :fg selection-text :bg selection)
  (:hl-line :bg current-line) (:symbol-highlight :bg active-ground)
  (:diagnostic-error-ground :fg nil :bg error-ground) (:diagnostic-warning-ground :fg nil :bg yellow-subtle)
  (:diagnostic-info-ground :fg nil :bg blue-subtle) (:diagnostic-hint-ground :dim nil :bg ground-dim)
  (:wrap-indicator :fg comment)
  (:visual-line :bg active-ground)
  (:banked-line :fg selection-text :bg selection-secondary)
  (:split-place :fg nil :bg split-ground)
  (:split-cursor :reverse nil :fg mode-line-text :bg mode-line-ground)
  (:split-cursor-insert :fg nil :bg error-ground)
  (:search-current :reverse nil :bold nil :fg search-text :bg search-ground)
  (:search-other :bold nil :underline nil :fg highlight-text :bg search-other-ground)
  (:search-outside-scope :dim nil :fg text-dim)
  (:match :bold nil :fg text :bg match-ground)
  (:paren-match :fg cursor :bg paren-ground) (:paren-mismatch :fg highlight-text :bg error-ground)
  (:job-queued :bg blue-subtle)
  (:job-waiting :bg teal-subtle)
  (:line-number :dim nil :fg text-dim :bg ground)
  (:current-line-number :bold t :fg line-number-current :bg line-number-current-ground)
  (:vc-added :fg added) (:vc-changed :fg changed) (:vc-deleted :fg removed)
  (:comment :italic nil :fg comment) (:string :fg blue) (:escape :bold nil :fg yellow)
  (:raw-byte :fg red)
  (:hex-offset :dim nil :fg text-dim) (:hex-unprintable :dim nil :fg whitespace)
  (:keyword :fg red) (:function :fg magenta) (:type :fg text)
  (:constant :fg blue) (:builtin :fg green) (:property :fg green)
  (:variable :fg orange) (:operator :fg text) (:punctuation :fg text)
  (:tag :fg green) (:attribute :fg yellow) (:label :fg blue)
  (:preprocessor :fg text) (:heading :fg blue-cooler) (:link :fg link)
  (:constructor :fg orange) (:string.regexp :fg yellow) (:variable.parameter :fg yellow)
  (:punctuation.bracket :fg orange) (:punctuation.special :fg yellow) (:keyword.operator :fg blue)
  (:keyword.storage.modifier :fg green) (:function.method :fg green) (:function.macro :fg green)
  (:function.special :fg yellow) (:namespace :fg text)
  (:markup.heading :inherit nil :fg orange)
  (:markup.heading.1 :bold t :fg blue-cooler) (:markup.heading.2 :bold t :fg blue) (:markup.heading.3 :bold t :fg blue-warmer)
  (:markup.heading.4 :fg blue-cooler) (:markup.heading.5 :fg blue) (:markup.heading.6 :fg blue-warmer)
  (:markup.list :fg orange) (:markup.bold :fg blue-cooler) (:markup.italic :fg blue-cooler)
  (:markup.strikethrough :fg blue-cooler) (:markup.link :fg blue-cooler) (:markup.link.url :fg magenta)
  (:markup.link.label :fg blue-cooler) (:markup.link.text :fg blue-cooler)
  (:markup.quote :fg quote-text) (:markup.raw :fg quote-text) (:markup.raw.block :fg text)
  (:diff.plus :fg added) (:diff.minus :fg removed) (:diff.delta :fg changed) (:git-status-commit :fg blue)
  (:conflict-marker :fg text-alt :bg active-ground) (:conflict-first :inherit nil :bg removed-ground)
  (:conflict-base :fg nil :bg yellow-subtle) (:conflict-second :inherit nil :bg added-ground)
  (:replace-going :fg removed :bg removed-ground) (:replace-coming :fg added :bg added-ground :bold t)
  (:error :fg red) (:warning :fg yellow) (:info :fg blue) (:hint :fg brown) (:spellcheck :underline-colour yellow)
  (:diagnostic-error :underline-colour red) (:diagnostic-warning :underline-colour yellow)
  (:diagnostic-info :underline-colour blue) (:diagnostic-hint :underline-colour brown)
  (:csv-column-0 :fg text) (:csv-column-1 :fg red) (:csv-column-2 :fg green)
  (:csv-column-3 :fg yellow) (:csv-column-4 :fg blue) (:csv-column-5 :fg magenta)
  (:org-level-1 :inherit nil :bold t :fg blue-cooler) (:org-level-2 :inherit nil :bold t :fg blue)
  (:org-level-3 :inherit nil :bold t :fg blue-warmer) (:org-level-4 :inherit nil :bold t :fg blue-cooler)
  (:org-level-5 :inherit nil :bold t :fg blue) (:org-level-6 :inherit nil :bold t :fg blue-warmer)
  (:org-level-7 :inherit nil :bold t :fg blue-cooler) (:org-level-8 :inherit nil :bold t :fg text-dim)
  (:org-todo :bold nil :fg green) (:org-done :bold nil :fg comment) (:org-headline-done :dim nil :fg comment)
  (:org-wait :bold nil :fg yellow) (:org-cancelled :dim nil :bold nil :italic t :fg text-dim)
  (:org-priority :bold nil :fg magenta) (:org-tag :bold nil :fg green) (:org-archived :dim nil :fg text-dim)
  (:org-agenda-structure :bold t :fg blue) (:org-agenda-date :inherit nil :bold t :fg blue-cooler)
  (:org-agenda-date-today :inherit nil :bold t :underline t :fg blue-cooler)
  (:org-agenda-date-weekend :inherit nil :bold t :fg orange) (:org-agenda-date-weekend-today :inherit nil :bold t :underline nil :fg orange)
  (:calendar-weekday-header :fg blue) (:calendar-weekend-header :fg orange) (:calendar-marked :bold nil :fg blue) (:calendar-journal :bold t :underline t)
  (:calendar-today :reverse nil :underline nil :bold t :fg ground :bg blue) (:calendar-chosen :bold t :underline t :fg magenta)
  (:org-time-grid :fg text-dim) (:org-agenda-current-time :inherit nil :fg orange)
  (:org-scheduled :fg yellow) (:org-scheduled-today :bold t :fg yellow) (:org-scheduled-previously :bold t :fg yellow)
  (:org-upcoming-deadline :fg red) (:org-upcoming-distant-deadline :fg text) (:org-imminent-deadline :bold t :fg red)
  (:org-agenda-done :fg comment) (:org-agenda-calendar-event :fg text)
  (:org-agenda-category :dim nil :fg text-dim) (:org-agenda-time :fg blue) (:org-agenda-untimed :dim nil :fg text-dim) (:org-agenda-tags :dim nil :fg text-dim)
  (:org-document-title :bold t :fg orange) (:org-document-info :fg text-alt)
  (:org-document-info-keyword :dim nil :fg text-dim) (:org-meta-line :dim nil :fg text-dim)
  (:org-block :bg ground-dim) (:org-block-begin-line :dim nil :fg text-dim :bg ground-dim)
  (:org-code :inherit nil :fg code) (:org-verbatim :inherit nil :fg code) (:org-macro :fg magenta)
  (:org-link :inherit nil :fg link) (:org-macro-text :fg green) (:org-date :fg blue) (:org-special-keyword :dim nil :fg text-dim)
  (:org-drawer :dim nil :fg text-dim) (:org-log-note :dim nil :fg text-dim) (:org-property-value :fg blue-cooler) (:org-table :fg blue-cooler)
  (:org-formula :dim nil :fg text-alt) (:org-checkbox :bold nil :fg yellow) (:org-footnote :inherit nil :fg link)
  (:org-latex-and-related :fg magenta-faint) (:org-cite :inherit nil :fg link) (:org-hide :dim nil :fg ground)
  (:csv-column-6 :fg orange) (:csv-column-7 :fg blue-cooler) (:csv-separator :fg orange))

(define-theme ao-light
  "Ao, light: the same palette worked for a white ground, every accent darkened
until it reads at 4.5:1, with the Fedora neutrals around it and the same
steel-blue mode line. From the Ao theme by the same author."
  :faces-from ao-dark
  :palette
  ((docs-white "#ffffff") (docs-smoke-30 "#fafafa") (docs-smoke-70 "#f0f0f0") (docs-smoke-90 "#e1e1e1")
   (docs-smoke-blue "#d9e2f0") (docs-gray-10 "#c1c1c1") (docs-gray-30 "#8e8e8e") (docs-gray-70 "#5d5d5d")
   (docs-jet-30 "#424242") (docs-jet-70 "#222222") (docs-link "#1565c0")
   (blaze-orange "#a85f00") (lemon-zest "#946c00") (leafy-green "#3b7f3d") (sky-blue "#0a7ab3")
   (dreamy-blue "#0a66c2") (crystal-blue "#0a4fa8") (ruby-glow "#c5210f") (walnut-brown "#906c48")
   (slate-purple "#7d2ae8") (light-purple "#7529c7") (deep-purple "#4c0f8d") (slate-gray "#6a7282")
   (blaze-orange-pure "#ff9000") (twilight-blue "#2c5484") (winter-sky "#f3f4f6") (pitch-black "#000000")
   (selection-purple "#7533bd")
   ;; text and its grounds
   (ground docs-white) (ground-dim docs-smoke-30) (active-ground docs-smoke-90) (current-line docs-smoke-blue)
   (text docs-jet-70) (text-dim docs-gray-70) (text-alt docs-jet-30) (comment slate-gray) (divider docs-gray-30)
   (code docs-jet-70) (quote-text pitch-black) (whitespace docs-gray-10)
   ;; accents
   (orange blaze-orange) (yellow lemon-zest) (green leafy-green) (red ruby-glow) (blue sky-blue)
   (blue-cooler crystal-blue) (blue-warmer dreamy-blue) (magenta slate-purple) (magenta-faint light-purple)
   (brown walnut-brown) (link docs-link)
   (added "#2e6b30") (removed "#b3170a") (changed "#7a5900")
   (added-ground "#e6f4e6") (removed-ground "#fdeceb")
   ;; the mode line, selections, searches and marks
   (mode-line-ground twilight-blue) (mode-line-text winter-sky) (mode-line-inactive-ground pitch-black)
   (mode-line-directory "#99c7ff") (completion-ground blue-subtle)
   (selection selection-purple) (selection-text docs-white) (selection-secondary deep-purple)
   (search-ground "#ffd666") (search-text docs-jet-70) (search-other-ground "#cfe0f7") (highlight-text docs-jet-70)
   (match-ground "#f3e8fd") (error-ground "#f8cfcb") (split-ground "#ded0ff")
   (cursor blaze-orange-pure) (paren-ground docs-white)
   (line-number-current "#8a4e00") (line-number-current-ground docs-smoke-70)
   (yellow-subtle "#fdf3e0") (blue-subtle "#e7f0fb") (teal-subtle "#e2f4f0")))

(defvar *startup-theme* 'ao-dark
  "The theme in force when the editor starts, before the init file runs; the
init file changes it with (load-theme 'name).")

(defvar *colourless-theme* 'plain
  "The theme used when the display has no colours (fewer than eight), whatever
else was chosen, since colours would not show there.")

(defvar tuatara-user:*dark-theme* 'ao-dark
  "The dark theme toggle-light-dark-theme switches to. Set it in the init file,
for example (setf *dark-theme* 'terminal).")

(defvar tuatara-user:*light-theme* 'ao-light
  "The light theme toggle-light-dark-theme switches to. Set it in the init file.")

(defvar tuatara-user:*follow-desktop-style* t
  "True to have the window's theme follow the desktop's light or dark style:
*dark-theme* while it is dark, *light-theme* while it is light, switching when
the desktop does. It stops once a theme is chosen: (load-theme 'name) in the
init file, or \\[load-theme] or toggle-light-dark-theme. A terminal says
nothing of the desktop's style, so there the theme stays as it is.")

(defvar *theme-chosen* nil
  "True once a theme was chosen by name, in the init file or with a command:
the desktop's style is no longer followed.")

(defvar *style-followed* nil
  "The desktop's style the theme was last made to follow, :dark or :light.")

(defun follow-desktop-style (editor)
  "Apply *dark-theme* or *light-theme* when the desktop's style, as EDITOR's
frontend knows it, is not the one followed last; not while
*follow-desktop-style* is NIL or once a theme has been chosen."
  (when (and tuatara-user:*follow-desktop-style* (not *theme-chosen*))
    (let ((style (tuatara.frontend:frontend-desktop-style (editor-frontend editor))))
      (when (and style (not (eq style *style-followed*)))
        (setf *style-followed* style)
        (tuatara.display:apply-theme (if (eq style :dark) tuatara-user:*dark-theme* tuatara-user:*light-theme*))))))

(defun desktop-style-tick (editor)
  "Between commands: follow the desktop's style when it has switched."
  (follow-desktop-style editor)
  nil)

(defun same-theme-p (a b)
  "True when theme names A and B (symbols from any package) name the same theme."
  (let ((ta (and a (tuatara.display:find-theme a))) (tb (and b (tuatara.display:find-theme b))))
    (and ta (eq ta tb))))

(define-command toggle-light-dark-theme ()
  "Switch between the light and the dark theme: the two ao variants, unless the
init file sets *light-theme* and *dark-theme* to others. The desktop's light or
dark style is not followed after it."
  (let ((next (if (same-theme-p tuatara.display:*current-theme* tuatara-user:*dark-theme*)
                  tuatara-user:*light-theme*
                  tuatara-user:*dark-theme*)))
    (setf *theme-chosen* t)
    (tuatara.display:apply-theme next)
    (message "Theme ~(~a~)" tuatara.display:*current-theme*)))

(defun theme-candidates ()
  "Completion candidates for every theme, each with the first sentence of its description beside it."
  (mapcar (lambda (name)
            (let ((theme (tuatara.display:find-theme name)))
              (make-candidate (string-downcase (symbol-name name))
                              :annotation (list (list (first-sentence (tuatara.display:theme-doc theme)) :completion-annotation 1/2))
                              :value name)))
          (tuatara.display:theme-names)))

(define-command load-theme ()
  "Choose a colour theme, with each theme's description beside it. Your own face
settings (set-face in the init file) stay on top of any theme."
  (let ((chosen (completing-read "Load theme: " (theme-candidates) :require-match t)))
    (when (symbolp chosen)
      (setf *theme-chosen* t)
      (tuatara.display:apply-theme chosen)
      (message "Theme ~(~a~)" chosen))))

;;; The sixteen colours of a terminal, as the theme in force gives them

(defparameter *terminal-palette-roles*
  #(ground-dim red green yellow blue magenta blue-cooler text
    text-dim red green yellow blue magenta blue-cooler text-alt)
  "The roles of the theme in force that give the sixteen basic colours a
program in a terminal (\\[terminal]) chooses by their numbers: black, red,
green, yellow, blue, magenta, cyan and white, then the bright ones. A theme
without one of these roles leaves that colour to the terminal's palette.")

(defun theme-terminal-colour (index)
  "The colour, #rrggbb, the theme in force gives the basic colour INDEX (0-15)
of a terminal (*TERMINAL-PALETTE-ROLES*); NIL when it gives none."
  (let ((theme (and tuatara.display:*current-theme* (tuatara.display:find-theme tuatara.display:*current-theme*))))
    (and theme (< -1 index 16)
         (ignore-errors (tuatara.display:theme-role-colour theme (aref *terminal-palette-roles* index))))))
