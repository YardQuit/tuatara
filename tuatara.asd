;;;; tuatara.asd --- ASDF system definitions for Tuatara.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defsystem "tuatara"
  :description "Tuatara: an Emacs-shaped editor in Common Lisp."
  :author "Michael A Jones <yardquit@pm.me>"
  :license "GPL-3.0-or-later"
  :version "0.90.30"
  :depends-on ("tuatara/core")
  :in-order-to ((test-op (test-op "tuatara/tests"))))

(defsystem "tuatara/core"
  :description "The core: runtime, text model, buffers, syntax, display, command loop, modes, I/O
and the frontend interface. Depends on nothing under lisp/ or frontends/."
  :license "GPL-3.0-or-later"
  :depends-on ("alexandria" "cl-ppcre" (:require "sb-posix") (:require "sb-concurrency"))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "version")
               (:module "runtime"  :serial t :components
                        ((:file "package") (:file "records") (:file "host") (:file "resources") (:file "stopping")
                         (:file "debug") (:file "pool") (:file "threads") (:file "locks") (:file "jobs") (:file "hooks")
                         (:file "settings")))
               (:module "text"     :serial t :components
                        ((:file "package") (:file "utf8") (:file "mapping") (:file "rope") (:file "edits")
                         (:file "strings") (:file "octets") (:file "version") (:file "json") (:file "xml")
                         (:file "escapes")))
               (:module "buffer"   :serial t :components ((:file "package") (:file "buffer")))
               (:module "io"       :serial t :components
                        ((:file "package") (:file "posix") (:file "kinds") (:file "in-place") (:file "isolation")
                         (:file "files") (:file "watch") (:file "save") (:file "programs") (:file "processes")
                         (:file "scopes") (:file "sandbox") (:file "operations") (:file "deflate") (:file "zip")))
               (:module "dates"    :serial t :components ((:file "package") (:file "dates") (:file "zones")))
               (:module "syntax"   :serial t :components
                        ((:file "package") (:file "named-patterns") (:file "search") (:file "helper")
                         (:file "tree-sitter") (:file "parsing") (:file "highlight") (:file "navigation")))
               (:module "frontend" :serial t :components ((:file "package") (:file "interface")))
               (:module "display"  :serial t :components
                        ((:file "package") (:file "faces") (:file "width") (:file "fill") (:file "bidi")
                         (:file "layout") (:file "folds") (:file "hex") (:file "window") (:file "splits")))
               ;; the painter interface as messages to a frontend in another process, which paints with faces
               (:module "remote"   :pathname "frontend" :serial t :components ((:file "messages") (:file "remote")))
               (:module "command"  :serial t :components
                        ((:file "package") (:file "host") (:file "keymap") (:file "command") (:file "completion")
                         (:file "editor") (:file "panel") (:file "dispatch") (:file "menu") (:file "redisplay")
                         (:file "minibuffer") (:file "session") (:file "recorded-keys") (:file "keys") (:file "popup")
                         (:file "windows") (:file "frames") (:file "clients")))
               (:module "modes"    :serial t :components ((:file "package") (:file "modes")))))

(defsystem "tuatara/frontend-fake"
  :description "An in-memory frontend for tests: queued events in, a screen of strings out."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core")
  :pathname "frontends/fake/"
  :components ((:file "fake")))

(defsystem "tuatara/defaults"
  :description "The defaults compiled into the image: base editing commands, DONKEY modal editing, files, windows."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core" (:require "sb-introspect") (:require "sb-cltl2"))
  :pathname "lisp/"
  :serial t
  :components ((:file "package")
               ;; the features with Lisp packages of their own, each defined before any file names what it exports
               (:file "git/package")
               (:file "terminal/package")
               (:file "lsp/package")
               (:file "compile/package")
               (:file "packages/package")
               (:file "export/package")
               (:file "host")
               (:file "theme")
               (:file "garbage")
               (:module "modes"   :serial t :components
                        ((:file "base") (:file "special") (:file "list-preview") (:file "csv") (:file "languages")
                         (:file "more-languages-data") (:file "more-languages-markup")
                         (:file "more-languages-programming") (:file
                                                               "more-languages-build") (:file
                                                                                        "json-lines") (:file
                                                                                                       "git-commit")))
               (:module "editing" :serial t :components
                        ((:file "state") (:file "your-name") (:file "diff") (:file "tabs") (:file "choosing")
                         (:file "questions") (:file "commands") (:file "extended-command") (:file "sentences")
                         (:file "simple") (:file "pairs") (:file "comments") (:file "paren") (:file "case")
                         (:file "mouse") (:file "font") (:file "font-list") (:file "selection") (:file "lists")
                         (:file "narrowing") (:file "sort") (:file "records") (:file "enchant") (:file "toggles")
                         (:file "spellcheck") (:file "windows") (:file "frames") (:file "own-files")
                         (:file "snippets") (:file "fill") (:file "hl-line") (:file "cursor-column")
                         (:file "soft-wrap") (:file "centred-text") (:file "display-fill-column") (:file "user-lisp")
                         (:file "recorded-keys") (:file "shown-files") (:file "conflicts")))
               (:module "buffer-commands" :pathname "buffers" :components ((:file "commands")))
               (:module "files"   :serial t :components
                        ((:file "gathering") (:file "encrypted-names") (:file "visiting") (:file "standard-input")
                         (:file "files") (:file "trust") (:file "grammars") (:file "hash")
                         (:file "private-directories") (:file "outside-programs") (:file "external")
                         (:file "own-programs") (:file "glob") (:file "gitignore") (:file "listing") (:file "walk")
                         (:file "evidence") (:file "formatted") (:file "paths-in-text") (:file "rename-formatted")
                         (:file "recent") (:file "auto-save") (:file "session") (:file "wipe") (:file "archives")
                         (:file "differences") (:file "merging") (:file "places") (:file "file-places")
                         (:file "bookmarks") (:file "file-local") (:file "next-place")))
               (:module "viewers" :serial t :components
                        ((:file "hex") (:file "decode") (:file "pdf-view") (:file "image-view")
                         (:file "inline-pictures") (:file "office-view") (:file "pdf-reader") (:file "synctex")
                         (:file "metadata")))
               (:module "calendar" :serial t :components ((:file "calendar")))
               (:module "lisp-and-org" :pathname "modes" :serial t :components
                        ((:file "lisp") (:file "lisp-indent") (:file "tree-indent") (:file "latex") (:file "org")))
               (:module "word-completion" :pathname "editing" :components ((:file "word-completion")))
               (:module "org"     :serial t :components
                        ((:file "syntax") (:file "parts") (:file "parse") (:file "fontify") (:file "properties")
                         (:file "dates") (:file "indent") (:file "fold") (:file "edit") (:file "schedule")
                         (:file "repeat-menu") (:file "items") (:file "tables") (:file "formulas") (:file "tangle")
                         (:file "toc") (:file "links") (:file "linking") (:file "clock") (:file "checklist")
                         (:file "agenda") (:file "dblock") (:file "agenda-view") (:file "notify")
                         (:file "formatted-blocks") (:file "latex-fonts") (:file "latex-pictures") (:file "latex")
                         (:file "latex-languages") (:file "export-settings") (:file "keywords") (:file "pictures")
                         (:file "foreign-text")
                         (:file "docx") (:file "docx-templates") (:file "docx-read") (:file "office-read")
                         (:file "odf-read") (:file "template-import") (:file "docx-import") (:file "latex-import")
                         (:file "refile") (:file "capture") (:file "prose")
                         (:static-file "template-org" :pathname "../../data/latex-templates/org.tex")
                         (:static-file "template-article" :pathname "../../data/latex-templates/article.tex")
                         (:file "latex-templates") (:static-file "org-html-css" :pathname "../../data/org-html.css")
                         (:file "html")))
               (:module "formatted-links" :pathname "files" :components ((:file "formatted-links")))
               (:module "markdown-links" :pathname "modes" :components ((:file "markdown-links")))
               (:module "prose" :pathname "editing" :components ((:file "prose")))
               (:module "journal" :serial t :components ((:file "journal")))
               (:file "mode-line-signs")
               (:file "jobs")
               (:module "search"  :serial t :components
                        ((:file "replace") (:file "matcher") (:file "substitute") (:file "rewrite")
                         (:file "incremental") (:file "filters") (:file "saved-patterns") (:file "files") (:file "results")
                         (:file "indexed")))
               (:file "outline")
               (:module "compile" :serial t :components ((:file "compile")))
               (:module "export"  :serial t :components ((:file "export") (:file "redactions") (:file "templates") (:file "pdf-sync")
                                                          (:file "formulas")))
               (:module "git-run" :pathname "git" :serial t :components ((:file "run") (:file "look-only")))
               (:module "gutter"  :serial t :components ((:file "line-numbers") (:file "vc")))
               (:module "lisp-tools" :pathname "modes" :serial t :components
                        ((:file "lisp-complete") (:file "lisp-xref") (:file "lisp-debug") (:file "ielm")
                         (:file "lisp-macroexpand")))
               (:module "lsp"     :serial t :components
                        ((:file "tools") (:file "usual-tools") (:file "format") (:file "client") (:file "documents")
                         (:file "diagnostics") (:file "features") (:file "hints") (:file "semantic") (:file "symbols")
                         (:file "watching") (:file "folding") (:file "org-blocks")))
               (:module "git"     :serial t :components
                        ((:file "git") (:file "commits") (:file "stash") (:file "branches") (:file "log")
                         (:file "remote") (:file
                                           "history") (:file
                                                       "references") (:file
                                                                      "files") (:file
                                                                                "rewind") (:file
                                                                                           "repository") (:file
                                                                                                          "patches")))
               (:module "help"    :serial t :components
                        ((:file "help") (:file "faces") (:file "health") (:file "about") (:file "char") (:file "debug")
                         (:file "notifications") (:file "man")))
               (:module "buffers" :serial t :components ((:file "buffers")))
               (:module "terminal" :serial t :components ((:file "emulator") (:file "terminal")))
               (:module "rimu"    :serial t :components
                        ((:file "rimu") (:file "preview") (:file "operations") (:file "trash") (:file "sizes")
                         (:file "found")))
               ;; rimu's git column and its lists of a directory as a commit left it, in git's package
               (:module "git-rimu" :pathname "git" :components ((:file "rimu")))
               (:module "gpg"     :serial t :components ((:file "encrypted") (:file "gpg-commands") (:file "rimu")))
               (:module "modal-states" :pathname "modal" :components ((:file "states")))
               (:module "selecting" :serial t
                :components ((:file "selecting") (:file "selection-objects") (:file "paragraphs") (:file "mark-run")
                             (:file "rectangles") (:file
                                                   "registers") (:file
                                                                 "banked-lines") (:file "wrapping") (:file "split")
                             (:file "cursors") (:file "digraph-table")
                             (:file "input-method-table") (:file
                                                           "input-methods") (:file "digraphs") (:file "characters")))
               (:module "modal"   :serial t :components ((:file "modal") (:file "tutor")))
               (:static-file "starter-init" :pathname "../data/init.lisp")
               (:file "config")
               (:module "packages" :serial t
                :components ((:file "signing") (:file "declare") (:file "loading") (:file "compile-apart")
                             (:file "fetch") (:file "manager") (:file "sweep") (:file "unload")))
               (:file "scratch")
               (:file "daemon")
               (:file "servers")
               ;; files on another machine, through SFTP
               (:module "remote"  :serial t :components
                        ((:file "asking") (:file "sftp") (:file "files") (:file "directories")
                         (:file "language-servers")))
               (:file "setup")
               (:file "api")))

(defsystem "tuatara/frontend-tui"
  :description "The ncurses terminal frontend."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core")
  :pathname "frontends/tui/"
  :components ((:file "tui")))

(defsystem "tuatara/frontend-gtk"
  :description "The GTK4 frontend: Tuatara in a window of its own, painted with Pango and Cairo; the
libraries are loaded when a window opens."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core")
  :pathname "frontends/gtk/"
  :serial t
  :components ((:file "ffi") (:file "gtk") (:file "input") (:file "paint") (:file "window")))

(defsystem "tuatara/terminal"
  :description "Tuatara in the terminal or a window of its own: the defaults in either frontend, and
the program's entry point."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/frontend-tui" "tuatara/frontend-gtk" "tuatara/defaults")
  :pathname "frontends/tui/"
  :components ((:file "main") (:file "man-pages") (:file "ssh" :depends-on ("main"))
               (:file "client" :depends-on ("main" "ssh")) (:file "desktop" :depends-on ("main"))))

(defsystem "tuatara/tests"
  :description "Test suite (Parachute) and the benchmark harness that enforces their budgets."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core" "tuatara/frontend-fake" "tuatara/frontend-tui" "tuatara/defaults" "tuatara/terminal"
               "parachute")
  :pathname "test/"
  :serial t
  :components ((:file "package")
               (:file "fences")
               (:file "modal-layer")
               (:file "time-bound")
               (:file "harness")
               (:file "pool")
               (:file "garbage")
               (:file "docstrings")
               (:file "definitions")
               (:file "source-scan")
               (:file "chokepoints")
               (:file "dead-code")
               (:file "sizes")
               (:file "smoke")
               (:file "rope")
               (:file "rope-file")
               (:file "raw-bytes")
               (:file "version")
               (:file "dates")
               (:file "buffer")
               (:file "display")
               (:file "bidi")
               (:file "command")
               (:file "tabs")
               (:file "completion")
               (:file "search")
               (:file "incremental-search")
               (:file "search-constructs")
               (:file "search-agreement")
               (:file "search-filters")
               (:file "rewrite")
               (:file "jobs")
               (:file "files")
               (:file "sandbox")
               (:file "counting-lines")
               (:file "disk-changes")
               (:file "line-endings")
               (:file "sort")
               (:file "narrowing")
               (:file "sentences")
               (:file "csv-records")
               (:file "selecting-lines")
               (:file "editing-words")
               (:file "drawn-once")
               (:file "hex")
               (:file "gutter")
               (:file "toggles")
               (:file "help")
               (:file "describe-settings")
               (:file "man")
               (:file "man-pages")
               (:file "debug")
               (:file "hooks")
               (:file "buffers")
               (:file "rimu")
               (:file "pdf-view")
               (:file "external")
               (:file "latex-export")
               (:file "pdf-sync")
               (:file "latex-languages")
               (:file "html-export")
               (:file "docx-view")
               (:file "template-import")
               (:file "office-view")
               (:file "metadata")
               (:file "auto-save")
               (:file "session")
               (:file "encrypted")
               (:file "wipe")
               (:file "lsp")
               (:file "image-view")
               (:file "org")
               (:file "org-view")
               (:file "inline-pictures")
               (:file "org-links")
               (:file "org-tables")
               (:file "org-agenda")
               (:file "org-blocks")
               (:file "org-internals")
               (:file "org-parts")
               (:file "calendar")
               (:file "rimu-operations")
               (:file "rimu-found")
               (:file "windows")
               (:file "frames")
               (:file "mouse")
               (:file "formatted")
               (:file "recent")
               (:file "evidence")
               (:file "command-line")
               (:file "places")
               (:file "bookmarks")
               (:file "registers")
               (:file "file-local")
               (:file "snippets")
               (:file "git")
               (:file "git-look-only")
               (:file "compile")
               (:file "packages")
               (:file "package-trust")
               (:file "stable-surface")
               (:file "reuse-index")
               (:file "state-and-files")
               (:file "kept-files")
               (:file "job-endings")
               (:file "job-runner")
               (:file "search-results")
               (:file "job-guards")
               (:file "journal")
               (:file "capture")
               (:file "emulator")
               (:file "terminal")
               (:file "objects")
               (:file "split")
               (:file "many-places")
               (:file "gtk")
               (:file "tui")
               (:file "remote")
               (:file "daemon")
               (:file "daemon-over-ssh")
               (:file "ssh-servers")
               (:file "remote-files")
               (:file "language-servers-brought")
               (:file "tools")
               (:file "config")
               (:file "modes")
               (:file "themes")
               (:file "syntax")
               (:file "syntax-helpers")
               (:file "syntax-editing")
               (:file "prefix-keys")
               (:file "syntax-colours")
               (:file "languages")
               (:file "grammars")
               (:file "scratch")
               (:file "user-lisp")
               (:file "popup")
               (:file "word-completion")
               (:file "show-paren")
               (:file "conflicts")
               (:file "xref")
               (:file "backtrace")
               (:file "ielm")
               (:file "macroexpansion")
               (:file "lists")
               (:file "list-buffers")
               (:file "outline")
               (:file "simple")
               (:file "health")
               (:file "keys")
               (:file "picking")
               (:file "carried-settings")
               (:file "waits")
               (:file "recorded-keys")
               (:file "spellcheck")
               (:file "prose")
               (:file "ended-tests")
               (:file "feature-packages")
               (:file "package-runner")
               (:file "compile-warning-gate")
               (:file "bench-rope"))
  :perform (test-op (o c)
             (symbol-call :parachute :test :tuatara.tests :report :plain)))
