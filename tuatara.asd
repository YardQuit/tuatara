;;;; tuatara.asd --- ASDF system definitions for Tuatara.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defsystem "tuatara"
  :description "Tuatara: an Emacs-shaped editor in Common Lisp."
  :author "Michael A Jones <yardquit@pm.me>"
  :license "GPL-3.0-or-later"
  :version "0.90.13"
  :depends-on ("tuatara/core")
  :in-order-to ((test-op (test-op "tuatara/tests"))))

(defsystem "tuatara/core"
  :description "The core: runtime, text model, buffers, syntax, display, command loop, modes, I/O and the frontend interface. Depends on nothing under lisp/ or frontends/."
  :license "GPL-3.0-or-later"
  :depends-on ("alexandria" "cl-ppcre" (:require "sb-posix") (:require "sb-concurrency"))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "version")
               (:module "runtime"  :serial t :components ((:file "package") (:file "resources") (:file "stopping") (:file "pool") (:file "debug") (:file "threads") (:file "jobs")))
               (:module "dates"    :serial t :components ((:file "package") (:file "dates")))
               (:module "text"     :serial t :components ((:file "package") (:file "utf8") (:file "mapping") (:file "rope") (:file "edits") (:file "strings") (:file "version") (:file "json") (:file "xml")))
               (:module "buffer"   :serial t :components ((:file "package") (:file "buffer")))
               (:module "io"       :serial t :components ((:file "package") (:file "posix") (:file "kinds") (:file "files") (:file "sandbox") (:file "operations") (:file "zip")))
               ;; time zones, read from the system's zoneinfo files as io reads files
               (:module "zones"    :pathname "dates" :components ((:file "zones")))
               (:module "syntax"   :serial t :components ((:file "package") (:file "named-patterns") (:file "search") (:file "helper") (:file "tree-sitter") (:file "parsing") (:file "highlight") (:file "navigation")))
               (:module "frontend" :serial t :components ((:file "package") (:file "interface")))
               (:module "display"  :serial t :components ((:file "package") (:file "faces") (:file "width") (:file "layout") (:file "folds") (:file "hex") (:file "window") (:file "splits")))
               (:module "command"  :serial t :components ((:file "package") (:file "keymap") (:file "command") (:file "completion") (:file "editor") (:file "panel") (:file "dispatch") (:file "menu") (:file "redisplay") (:file "minibuffer") (:file "session") (:file "recorded-keys") (:file "keys") (:file "popup") (:file "windows")))
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
               (:file "theme")
               (:file "garbage")
               (:module "modes"   :serial t :components ((:file "base") (:file "special") (:file "list-preview") (:file "csv") (:file "languages") (:file "more-languages-data") (:file "more-languages-markup") (:file "more-languages-programming") (:file "more-languages-build") (:file "json-lines") (:file "git-commit")))
               (:module "editing" :serial t :components ((:file "state") (:file "your-name") (:file "diff") (:file "tabs") (:file "choosing") (:file "commands") (:file "extended-command") (:file "sentences") (:file "simple") (:file "pairs") (:file "comments") (:file "paren") (:file "case") (:file "mouse") (:file "font") (:file "font-list") (:file "selection") (:file "lists") (:file "narrowing") (:file "sort") (:file "records") (:file "enchant") (:file "toggles") (:file "spellcheck") (:file "windows") (:file "own-files") (:file "snippets") (:file "fill") (:file "hl-line") (:file "cursor-column") (:file "soft-wrap") (:file "centred-text") (:file "visual-line") (:file "user-lisp") (:file "recorded-keys") (:file "shown-files") (:file "conflicts")))
               (:module "files"   :serial t :components ((:file "visiting") (:file "files") (:file "trust") (:file "grammars") (:file "hash") (:file "external") (:file "own-programs") (:file "walk") (:file "formatted") (:file "paths-in-text") (:file "rename-formatted") (:file "recent") (:file "auto-save") (:file "session") (:file "wipe") (:file "archives") (:file "differences") (:file "places") (:file "bookmarks") (:file "file-local") (:file "next-place")))
               (:module "viewers" :serial t :components ((:file "hex") (:file "decode") (:file "pdf-view") (:file "image-view") (:file "office-view") (:file "pdf-reader") (:file "metadata")))
               (:module "calendar" :serial t :components ((:file "calendar")))
               (:module "lisp-and-org" :pathname "modes" :serial t :components ((:file "lisp") (:file "lisp-indent") (:file "tree-indent") (:file "org")))
               (:module "org"     :serial t :components ((:file "syntax") (:file "parts") (:file "fontify") (:file "dates") (:file "indent") (:file "fold") (:file "edit") (:file "schedule") (:file "repeat-menu") (:file "items") (:file "tables") (:file "formulas") (:file "tangle") (:file "toc") (:file "links") (:file "linking") (:file "clock") (:file "checklist") (:file "agenda") (:file "dblock") (:file "agenda-view") (:file "notify") (:file "formatted-blocks") (:file "latex-fonts") (:file "latex-pictures") (:file "latex") (:file "latex-languages") (:file "export-settings") (:file "docx") (:file "docx-templates") (:file "docx-read") (:file "office-read") (:file "odf-read") (:file "template-import") (:file "docx-import") (:file "latex-import") (:file "refile") (:file "capture") (:file "prose")
                                                   (:static-file "template-org" :pathname "../../data/latex-templates/org.tex")
                                                   (:static-file "template-article" :pathname "../../data/latex-templates/article.tex")
                                                   (:file "latex-templates")))
               (:module "journal" :serial t :components ((:file "journal")))
               (:module "search"  :serial t :components ((:file "replace") (:file "substitute") (:file "rewrite") (:file "jobs") (:file "incremental") (:file "filters") (:file "files") (:file "indexed") (:file "outline")))
               (:module "compile" :serial t :components ((:file "compile")))
               (:module "export"  :serial t :components ((:file "export") (:file "redactions") (:file "templates")))
               (:module "git-run" :pathname "git" :components ((:file "run")))
               (:module "gutter"  :serial t :components ((:file "line-numbers") (:file "vc")))
               (:module "lisp-tools" :pathname "modes" :serial t :components ((:file "lisp-complete") (:file "lisp-xref") (:file "lisp-debug") (:file "ielm") (:file "lisp-macroexpand")))
               (:module "lsp"     :serial t :components ((:file "tools") (:file "usual-tools") (:file "format") (:file "client") (:file "documents") (:file "diagnostics") (:file "features") (:file "hints") (:file "semantic") (:file "symbols") (:file "watching") (:file "folding") (:file "org-blocks")))
               (:module "git"     :serial t :components ((:file "git") (:file "commits") (:file "stash") (:file "branches") (:file "log") (:file "remote") (:file "history") (:file "references") (:file "files") (:file "rewind") (:file "repository") (:file "patches")))
               (:module "help"    :serial t :components ((:file "help") (:file "faces") (:file "health") (:file "about") (:file "char") (:file "debug") (:file "notifications")))
               (:module "buffers" :serial t :components ((:file "buffers")))
               (:module "terminal" :serial t :components ((:file "emulator") (:file "terminal")))
               (:module "rimu"    :serial t :components ((:file "rimu") (:file "preview") (:file "operations") (:file "trash") (:file "git") (:file "sizes") (:file "found")))
               (:module "gpg"     :serial t :components ((:file "encrypted") (:file "gpg-commands") (:file "rimu")))
               (:module "modal-states" :pathname "modal" :components ((:file "states")))
               (:module "selecting" :serial t
                :components ((:file "selecting") (:file "selection-objects") (:file "paragraphs") (:file "mark-run")
                             (:file "rectangles") (:file "banked-lines") (:file "wrapping") (:file "split")
                             (:file "cursors") (:file "digraph-table")
                             (:file "input-method-table") (:file "input-methods") (:file "digraphs") (:file "characters")))
               (:module "modal"   :serial t :components ((:file "modal") (:file "tutor")))
               (:static-file "starter-init" :pathname "../data/init.lisp")
               (:file "config")
               (:file "packages")
               (:file "scratch")
               (:file "setup")
               (:file "api")))

(defsystem "tuatara/frontend-tui"
  :description "The ncurses terminal frontend."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core")
  :pathname "frontends/tui/"
  :components ((:file "tui")))

(defsystem "tuatara/frontend-gtk"
  :description "The GTK4 frontend: Tuatara in a window of its own, painted with Pango and Cairo; the libraries are loaded when a window opens."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core")
  :pathname "frontends/gtk/"
  :serial t
  :components ((:file "ffi") (:file "gtk") (:file "input") (:file "paint") (:file "window")))

(defsystem "tuatara/terminal"
  :description "Tuatara in the terminal or a window of its own: the defaults in either frontend, and the program's entry point."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/frontend-tui" "tuatara/frontend-gtk" "tuatara/defaults")
  :pathname "frontends/tui/"
  :components ((:file "main")))

(defsystem "tuatara/tests"
  :description "Test suite (Parachute) and the benchmark harness that enforces their budgets."
  :license "GPL-3.0-or-later"
  :depends-on ("tuatara/core" "tuatara/frontend-fake" "tuatara/frontend-tui" "tuatara/defaults" "tuatara/terminal"
               "parachute")
  :pathname "test/"
  :serial t
  :components ((:file "package")
               (:file "fences")
               (:file "time-bound")
               (:file "harness")
               (:file "pool")
               (:file "garbage")
               (:file "docstrings")
               (:file "definitions")
               (:file "smoke")
               (:file "rope")
               (:file "rope-file")
               (:file "raw-bytes")
               (:file "version")
               (:file "dates")
               (:file "buffer")
               (:file "display")
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
               (:file "debug")
               (:file "buffers")
               (:file "rimu")
               (:file "pdf-view")
               (:file "external")
               (:file "latex-export")
               (:file "latex-languages")
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
               (:file "mouse")
               (:file "formatted")
               (:file "recent")
               (:file "places")
               (:file "bookmarks")
               (:file "file-local")
               (:file "snippets")
               (:file "git")
               (:file "compile")
               (:file "packages")
               (:file "package-trust")
               (:file "stable-surface")
               (:file "state-and-files")
               (:file "job-endings")
               (:file "job-runner")
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
               (:file "tools")
               (:file "config")
               (:file "modes")
               (:file "themes")
               (:file "syntax")
               (:file "languages")
               (:file "grammars")
               (:file "scratch")
               (:file "user-lisp")
               (:file "popup")
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
               (:file "compile-warning-gate")
               (:file "bench-rope"))
  :perform (test-op (o c)
             (symbol-call :parachute :test :tuatara.tests :report :plain)))
