;;;; lisp/modes/more-languages-programming.lisp --- The programming languages Helix knows, in major modes named as Emacs's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each language is parsed with the grammar and coloured, indented and
;;;; selected by the rules Helix uses for it (syntax/), in a major mode named as
;;;; the one Emacs or its best-known package edits it with. A file type another
;;;; mode already claims stays that mode's.
;;;;
;;;; Written by tools/languages/generate.py: change that, not this file.

(in-package #:tuatara.defaults)

(define-major-mode js-mode prog-mode
  "JavaScript source."
  :lighter "JavaScript"
  :comment "//"
  :extensions ("js" "mjs" "cjs" "rules" "es6" "pac" "gs")
  :file-names (".node_repl_history" "jakefile")
  :interpreters ("node"))

(define-language javascript
  "JavaScript source, parsed with tree-sitter-javascript."
  :grammar "javascript" :modes (js-mode) :indent 2 :queries "javascript" :injection-names "js|javascript" :definitions :textobjects)

(define-major-mode js-jsx-mode js-mode
  "JavaScript with JSX elements."
  :lighter "JSX"
  :comment "//"
  :extensions ("jsx"))

(define-language jsx
  "JavaScript with JSX elements, parsed with tree-sitter-javascript."
  :grammar "javascript" :modes (js-jsx-mode) :indent 2 :queries "jsx" :injection-names "jsx" :definitions :textobjects)

(define-major-mode typescript-mode prog-mode
  "TypeScript source."
  :lighter "TypeScript"
  :comment "//"
  :extensions ("ts" "mts" "cts")
  :interpreters ("deno" "bun" "ts-node"))

(define-language typescript
  "TypeScript source, parsed with tree-sitter-typescript."
  :grammar "typescript" :modes (typescript-mode) :indent 2 :queries "typescript" :injection-names "ts|typescript" :definitions :textobjects)

(define-major-mode tsx-mode typescript-mode
  "TypeScript with JSX elements."
  :lighter "TSX"
  :comment "//"
  :extensions ("tsx"))

(define-language tsx
  "TypeScript with JSX elements, parsed with tree-sitter-tsx."
  :grammar "tsx" :modes (tsx-mode) :indent 2 :queries "tsx" :injection-names "tsx" :definitions :textobjects)

(define-major-mode python-mode prog-mode
  "Python source."
  :lighter "Python"
  :comment "#"
  :extensions ("py" "pyi" "py3" "pyw" "ptl" "rpy" "cpy" "ipy" "pyt")
  :file-names (".python_history" ".pythonstartup" ".pythonrc")
  :patterns ("/[^/]*SConstruct$" "/[^/]*SConscript$" "/[^/]*sconstruct$")
  :interpreters ("python" "uv"))

(define-language python
  "Python source, parsed with tree-sitter-python."
  :grammar "python" :modes (python-mode) :indent 4 :queries "python" :injection-names "py(thon)?" :definitions :textobjects)

(define-major-mode go-mode prog-mode
  "Go source."
  :lighter "Go"
  :comment "//"
  :extensions ("go"))

(define-language go
  "Go source, parsed with tree-sitter-go."
  :grammar "go" :modes (go-mode) :indent 4 :queries "go" :injection-names "go" :definitions :textobjects)

(define-major-mode java-mode prog-mode
  "Java source."
  :lighter "Java"
  :comment "//"
  :extensions ("java" "jav" "pde"))

(define-language java
  "Java source, parsed with tree-sitter-java."
  :grammar "java" :modes (java-mode) :indent 2 :queries "java" :injection-names "java" :definitions :textobjects)

(define-major-mode c++-mode prog-mode
  "C++ source and its headers."
  :lighter "C++"
  :comment "//"
  :extensions ("cc" "hh" "c++" "cpp" "hpp" "ipp" "tpp" "cxx" "hxx" "ixx" "txx" "ino" "C" "H" "cu" "cuh" "cppm" "h++" "ii" "inl")
  :file-names (".hpp.in" ".h.in"))

(define-language cpp
  "C++ source and its headers, parsed with tree-sitter-cpp."
  :grammar "cpp" :modes (c++-mode) :indent 2 :queries "cpp" :injection-names "cpp" :definitions :textobjects)

(define-major-mode csharp-mode prog-mode
  "C# source."
  :lighter "C#"
  :comment "//"
  :extensions ("cs" "csx" "cake"))

(define-language c-sharp
  "C# source, parsed with tree-sitter-c-sharp."
  :grammar "c-sharp" :modes (csharp-mode) :indent 4 :queries "c-sharp" :injection-names "c-?sharp" :definitions :textobjects)

(define-major-mode lua-mode prog-mode
  "Lua source."
  :lighter "Lua"
  :comment "--"
  :extensions ("lua" "rockspec")
  :interpreters ("lua" "luajit"))

(define-language lua
  "Lua source, parsed with tree-sitter-lua."
  :grammar "lua" :modes (lua-mode) :indent 2 :queries "lua" :injection-names "lua" :definitions :textobjects)

(define-major-mode ruby-mode prog-mode
  "Ruby source."
  :lighter "Ruby"
  :comment "#"
  :extensions ("rb" "rake" "irb" "gemspec" "rabl" "jbuilder" "jb" "podspec" "rjs" "rbi" "rbs" "Brewfile")
  :file-names ("rakefile" "gemfile" "Rakefile" "Gemfile" "Podfile" "Vagrantfile" "Brewfile" "Guardfile" "Capfile" "Cheffile" "Hobofile" "Appraisals" "Rantfile" "Berksfile" "Berksfile.lock" "Thorfile" "Puppetfile" "Fastfile" "Appfile" "Deliverfile" "Matchfile" "Scanfile" "Snapfile" "Gymfile" ".irbrc")
  :interpreters ("ruby"))

(define-language ruby
  "Ruby source, parsed with tree-sitter-ruby."
  :grammar "ruby" :modes (ruby-mode) :indent 2 :queries "ruby" :injection-names "ruby" :definitions :textobjects)

(define-major-mode php-mode prog-mode
  "PHP source, in HTML or alone."
  :lighter "PHP"
  :comment "//"
  :extensions ("php" "php4" "php5" "phtml" "ctp")
  :interpreters ("php"))

(define-language php
  "PHP source, in HTML or alone, parsed with tree-sitter-php."
  :grammar "php" :modes (php-mode) :indent 4 :queries "php" :injection-names "php" :definitions :textobjects)

(define-language php-only
  "PHP code without the HTML around it, parsed with tree-sitter-php-only."
  :grammar "php-only" :modes nil :indent 4 :queries "php-only" :injection-names "php-only")

(define-major-mode perl-mode prog-mode
  "Perl source."
  :lighter "Perl"
  :comment "#"
  :extensions ("pl" "pm" "t" "psgi" "raku" "rakumod" "rakutest" "rakudoc" "nqp" "p6" "pl6" "pm6")
  :file-names ("latexmkrc" ".latexmkrc")
  :interpreters ("perl"))

(define-language perl
  "Perl source, parsed with tree-sitter-perl."
  :grammar "perl" :modes (perl-mode) :indent 2 :queries "perl" :definitions :textobjects)

(define-major-mode sql-mode prog-mode
  "SQL statements."
  :lighter "SQL"
  :comment "--"
  :extensions ("sql" "dsql"))

(define-language sql
  "SQL statements, parsed with tree-sitter-sql."
  :grammar "sql" :modes (sql-mode) :indent 4 :queries "sql" :injection-names "sql" :definitions :textobjects)

(define-language regex
  "Regular expressions inside other languages, parsed with tree-sitter-regex."
  :grammar "regex" :modes nil :indent 4 :queries "regex" :injection-names "regex")

(define-major-mode emacs-lisp-mode prog-mode
  "Emacs Lisp source. Only read and coloured here: nothing of it is evaluated."
  :lighter "ELisp"
  :comment (";" ";;")
  :extensions ("el"))

(define-language elisp
  "Emacs Lisp source, parsed with tree-sitter-elisp."
  :grammar "elisp" :modes (emacs-lisp-mode) :indent 4 :queries "elisp")

(define-major-mode zig-mode prog-mode
  "Zig source."
  :lighter "Zig"
  :comment "//"
  :extensions ("zig" "zon"))

(define-language zig
  "Zig source, parsed with tree-sitter-zig."
  :grammar "zig" :modes (zig-mode) :indent 4 :queries "zig" :injection-names "zig" :definitions :textobjects)

(define-major-mode haskell-mode prog-mode
  "Haskell source."
  :lighter "Haskell"
  :comment "--"
  :extensions ("hs" "hs-boot" "hsc")
  :interpreters ("runhaskell" "stack"))

(define-language haskell
  "Haskell source, parsed with tree-sitter-haskell."
  :grammar "haskell" :modes (haskell-mode) :indent 2 :queries "haskell" :injection-names "hs|haskell" :definitions :textobjects)

(define-major-mode kotlin-mode prog-mode
  "Kotlin source."
  :lighter "Kotlin"
  :comment "//"
  :extensions ("kt" "kts"))

(define-language kotlin
  "Kotlin source, parsed with tree-sitter-kotlin."
  :grammar "kotlin" :modes (kotlin-mode) :indent 4 :queries "kotlin" :definitions :textobjects)

(define-major-mode swift-mode prog-mode
  "Swift source."
  :lighter "Swift"
  :comment "//"
  :extensions ("swift" "swiftinterface"))

(define-language swift
  "Swift source, parsed with tree-sitter-swift."
  :grammar "swift" :modes (swift-mode) :indent 2 :queries "swift" :injection-names "swift" :definitions :textobjects)

(define-major-mode scala-mode prog-mode
  "Scala source."
  :lighter "Scala"
  :comment "//"
  :extensions ("scala" "sbt" "sc"))

(define-language scala
  "Scala source, parsed with tree-sitter-scala."
  :grammar "scala" :modes (scala-mode) :indent 2 :queries "scala" :definitions :textobjects)

(define-major-mode dart-mode prog-mode
  "Dart source."
  :lighter "Dart"
  :comment "//"
  :extensions ("dart"))

(define-language dart
  "Dart source, parsed with tree-sitter-dart."
  :grammar "dart" :modes (dart-mode) :indent 2 :queries "dart" :definitions :textobjects)

(define-major-mode elixir-mode prog-mode
  "Elixir source."
  :lighter "Elixir"
  :comment "#"
  :extensions ("ex" "exs")
  :file-names ("mix.lock")
  :interpreters ("elixir"))

(define-language elixir
  "Elixir source, parsed with tree-sitter-elixir."
  :grammar "elixir" :modes (elixir-mode) :indent 2 :queries "elixir" :injection-names "elixir|ex" :definitions :textobjects)

(define-major-mode erlang-mode prog-mode
  "Erlang source."
  :lighter "Erlang"
  :comment "%%"
  :extensions ("erl" "hrl" "app")
  :file-names ("rebar.config" "rebar.lock")
  :patterns ("/[^/]*\\.app\\.src$")
  :interpreters ("escript"))

(define-language erlang
  "Erlang source, parsed with tree-sitter-erlang."
  :grammar "erlang" :modes (erlang-mode) :indent 4 :queries "erlang" :injection-names "erl(ang)?" :definitions :textobjects)

(define-major-mode ocaml-mode prog-mode
  "OCaml source."
  :lighter "OCaml"
  :block-comment ("(*" "*)")
  :extensions ("ml")
  :interpreters ("ocaml" "ocamlrun" "ocamlscript"))

(define-language ocaml
  "OCaml source, parsed with tree-sitter-ocaml."
  :grammar "ocaml" :modes (ocaml-mode) :indent 2 :queries "ocaml" :injection-names "ocaml" :definitions :textobjects)

(define-major-mode ocaml-interface-mode prog-mode
  "OCaml interfaces (.mli)."
  :lighter "OCaml Interface"
  :block-comment ("(*" "*)")
  :extensions ("mli"))

(define-language ocaml-interface
  "OCaml interfaces (.mli), parsed with tree-sitter-ocaml-interface."
  :grammar "ocaml-interface" :modes (ocaml-interface-mode) :indent 2 :queries "ocaml-interface")

(define-major-mode groovy-mode prog-mode
  "Groovy source, Jenkinsfiles and Gradle builds."
  :lighter "Groovy"
  :comment "//"
  :extensions ("gradle" "groovy" "jenkinsfile")
  :file-names ("Jenkinsfile")
  :patterns ("/Jenkinsfile\\.[^/]*$")
  :interpreters ("groovy"))

(define-language groovy
  "Groovy source, Jenkinsfiles and Gradle builds, parsed with tree-sitter-groovy."
  :grammar "groovy" :modes (groovy-mode) :indent 2 :queries "groovy")

(define-major-mode r-mode prog-mode
  "R source."
  :lighter "R"
  :comment "#"
  :extensions ("r" "R")
  :file-names (".Rprofile" "Rprofile.site" ".RHistory")
  :interpreters ("r" "R"))

(define-language r
  "R source, parsed with tree-sitter-r."
  :grammar "r" :modes (r-mode) :indent 2 :queries "r" :injection-names "r|R" :definitions :textobjects)

(define-major-mode julia-mode prog-mode
  "Julia source."
  :lighter "Julia"
  :comment "#"
  :extensions ("jl")
  :interpreters ("julia"))

(define-language julia
  "Julia source, parsed with tree-sitter-julia."
  :grammar "julia" :modes (julia-mode) :indent 4 :queries "julia" :injection-names "julia" :definitions :textobjects)

(define-major-mode clojure-mode prog-mode
  "Clojure source. Only read and coloured here: nothing of it is evaluated."
  :lighter "Clojure"
  :comment (";" ";;")
  :extensions ("clj" "cljs" "cljc" "clje" "cljr" "cljx" "edn" "boot"))

(define-language clojure
  "Clojure source, parsed with tree-sitter-clojure."
  :grammar "clojure" :modes (clojure-mode) :indent 2 :queries "clojure" :injection-names "clojure|clj|edn|boot" :definitions :textobjects)

(define-major-mode scheme-mode prog-mode
  "Scheme source. Only read and coloured here: nothing of it is evaluated."
  :lighter "Scheme"
  :comment (";" ";;")
  :extensions ("ss" "scm" "sld")
  :interpreters ("scheme" "guile" "chicken"))

(define-language scheme
  "Scheme source, parsed with tree-sitter-scheme."
  :grammar "scheme" :modes (scheme-mode) :indent 2 :queries "scheme" :injection-names "scheme" :definitions :textobjects)

(define-major-mode racket-mode prog-mode
  "Racket source. Only read and coloured here: nothing of it is evaluated."
  :lighter "Racket"
  :comment (";" ";;")
  :extensions ("rkt" "rktd" "rktl" "scrbl" "zuo")
  :interpreters ("racket" "zuo"))

(define-language racket
  "Racket source, parsed with tree-sitter-scheme."
  :grammar "scheme" :modes (racket-mode) :indent 2 :queries "racket")

(define-major-mode fennel-mode prog-mode
  "Fennel source. Only read and coloured here: nothing of it is evaluated."
  :lighter "Fennel"
  :comment (";" ";;")
  :extensions ("fnl" "fnlm")
  :interpreters ("fennel"))

(define-language fennel
  "Fennel source, parsed with tree-sitter-fennel."
  :grammar "fennel" :modes (fennel-mode) :indent 2 :queries "fennel")

(define-major-mode vimrc-mode prog-mode
  "Vim script."
  :lighter "Vim"
  :comment "\""
  :extensions ("vim")
  :file-names (".vimrc" ".nvimrc" ".exrc"))

(define-language vim
  "Vim script, parsed with tree-sitter-vim."
  :grammar "vim" :modes (vimrc-mode) :indent 4 :queries "vim" :injection-names "vim")

(define-major-mode jq-mode prog-mode
  "jq programs."
  :lighter "jq"
  :comment "#"
  :extensions ("jq"))

(define-language jq
  "jq programs, parsed with tree-sitter-jq."
  :grammar "jq" :modes (jq-mode) :indent 2 :queries "jq" :injection-names "jq" :definitions :textobjects)

(define-language luap
  "Lua patterns inside Lua strings, parsed with tree-sitter-luap."
  :grammar "luap" :modes nil :indent 4 :queries "luap" :injection-names "luap")

(define-language rust-format-args-macro
  "The arguments of Rust's println!, format! and the rest, parsed with tree-sitter-rust."
  :grammar "rust" :modes nil :indent 4 :queries "rust-format-args-macro" :definitions :textobjects)

(define-language go-format-string
  "The format strings of Go, parsed with tree-sitter-go-format-string."
  :grammar "go-format-string" :modes nil :indent 4 :queries "go-format-string" :injection-names "go-format-string")

(define-language lua-format-string
  "The format strings of Lua, parsed with tree-sitter-lua-format-string."
  :grammar "lua-format-string" :modes nil :indent 4 :queries "lua-format-string" :injection-names "lua-format-string")

(define-major-mode ada-mode prog-mode
  "Ada source."
  :lighter "Ada"
  :comment "--"
  :extensions ("adb" "ads"))

(define-language ada
  "Ada source, parsed with tree-sitter-ada."
  :grammar "ada" :modes (ada-mode) :indent 3 :queries "ada" :injection-names "ada" :definitions :textobjects)

(define-major-mode d-mode prog-mode
  "D source."
  :lighter "D"
  :comment "//"
  :extensions ("d" "dd"))

(define-language d
  "D source, parsed with tree-sitter-d."
  :grammar "d" :modes (d-mode) :indent 4 :queries "d" :injection-names "d" :definitions :textobjects)

(define-major-mode elm-mode prog-mode
  "Elm source."
  :lighter "Elm"
  :comment "--"
  :extensions ("elm"))

(define-language elm
  "Elm source, parsed with tree-sitter-elm."
  :grammar "elm" :modes (elm-mode) :indent 4 :queries "elm" :injection-names "elm" :definitions :textobjects)

(define-major-mode fortran-mode prog-mode
  "Fortran source."
  :lighter "Fortran"
  :comment "!"
  :extensions ("f" "for" "f90" "f95" "f03" "F" "F90" "F95" "F03"))

(define-language fortran
  "Fortran source, parsed with tree-sitter-fortran."
  :grammar "fortran" :modes (fortran-mode) :indent 4 :queries "fortran" :injection-names "fortran")

(define-major-mode fsharp-mode prog-mode
  "F# source."
  :lighter "F#"
  :comment "//"
  :extensions ("fs" "fsx" "fsi" "fsscript"))

(define-language fsharp
  "F# source, parsed with tree-sitter-fsharp."
  :grammar "fsharp" :modes (fsharp-mode) :indent 4 :queries "fsharp" :injection-names "fsharp")

(define-major-mode gleam-mode prog-mode
  "Gleam source."
  :lighter "Gleam"
  :comment "//"
  :extensions ("gleam"))

(define-language gleam
  "Gleam source, parsed with tree-sitter-gleam."
  :grammar "gleam" :modes (gleam-mode) :indent 2 :queries "gleam" :injection-names "gleam" :definitions :textobjects)

(define-major-mode nim-mode prog-mode
  "Nim source."
  :lighter "Nim"
  :comment "#"
  :extensions ("nim" "nims" "nimble"))

(define-language nim
  "Nim source, parsed with tree-sitter-nim."
  :grammar "nim" :modes (nim-mode) :indent 2 :queries "nim" :injection-names "nim" :definitions :textobjects)

(define-major-mode odin-mode prog-mode
  "Odin source."
  :lighter "Odin"
  :comment "//"
  :extensions ("odin"))

(define-language odin
  "Odin source, parsed with tree-sitter-odin."
  :grammar "odin" :modes (odin-mode) :indent 4 :queries "odin" :definitions :textobjects)

(define-major-mode verilog-mode prog-mode
  "Verilog descriptions of hardware."
  :lighter "Verilog"
  :comment "//"
  :extensions ("v" "vh"))

(define-language verilog
  "Verilog descriptions of hardware, parsed with tree-sitter-verilog."
  :grammar "verilog" :modes (verilog-mode) :indent 2 :queries "verilog" :injection-names "verilog" :definitions :textobjects)

(define-major-mode v-mode prog-mode
  "V source."
  :lighter "V"
  :comment "//"
  :extensions ("vv" "vsh")
  :interpreters ("v"))

(define-language v
  "V source, parsed with tree-sitter-v."
  :grammar "v" :modes (v-mode) :indent 4 :queries "v" :definitions :textobjects)

(define-major-mode hare-mode prog-mode
  "Hare source."
  :lighter "Hare"
  :comment "//"
  :extensions ("ha"))

(define-language hare
  "Hare source, parsed with tree-sitter-hare."
  :grammar "hare" :modes (hare-mode) :indent 8 :queries "hare" :injection-names "hare" :definitions :textobjects)

(define-major-mode crystal-mode prog-mode
  "Crystal source."
  :lighter "Crystal"
  :comment "#"
  :extensions ("cr"))

(define-language crystal
  "Crystal source, parsed with tree-sitter-crystal."
  :grammar "crystal" :modes (crystal-mode) :indent 2 :queries "crystal" :definitions :textobjects)

(define-major-mode purescript-mode prog-mode
  "PureScript source."
  :lighter "PureScript"
  :comment "--"
  :extensions ("purs"))

(define-language purescript
  "PureScript source, parsed with tree-sitter-purescript."
  :grammar "purescript" :modes (purescript-mode) :indent 2 :queries "purescript" :injection-names "purescript" :definitions :textobjects)

(define-major-mode lean4-mode prog-mode
  "Lean 4 source."
  :lighter "Lean 4"
  :comment "--"
  :extensions ("lean"))

(define-language lean
  "Lean 4 source, parsed with tree-sitter-lean."
  :grammar "lean" :modes (lean4-mode) :indent 2 :queries "lean" :injection-names "lean")

(define-major-mode sml-mode prog-mode
  "Standard ML source."
  :lighter "SML"
  :block-comment ("(*" "*)")
  :extensions ("sml"))

(define-language sml
  "Standard ML source, parsed with tree-sitter-sml."
  :grammar "sml" :modes (sml-mode) :indent 4 :queries "sml" :injection-names "sml")

(define-major-mode prolog-mode prog-mode
  "Prolog source."
  :lighter "Prolog"
  :comment "%"
  :extensions ("prolog")
  :interpreters ("swipl"))

(define-language prolog
  "Prolog source, parsed with tree-sitter-prolog."
  :grammar "prolog" :modes (prolog-mode) :indent 4 :queries "prolog")

(define-major-mode tcl-mode prog-mode
  "Tcl scripts."
  :lighter "Tcl"
  :comment "#"
  :extensions ("tcl")
  :interpreters ("tclsh" "tclish" "jimsh" "wish"))

(define-language tcl
  "Tcl scripts, parsed with tree-sitter-tcl."
  :grammar "tcl" :modes (tcl-mode) :indent 4 :queries "tcl" :injection-names "tcl")

(define-major-mode pascal-mode prog-mode
  "Pascal and Delphi source."
  :lighter "Pascal"
  :comment "//"
  :extensions ("pas" "pp" "lpr" "lfm"))

(define-language pascal
  "Pascal and Delphi source, parsed with tree-sitter-pascal."
  :grammar "pascal" :modes (pascal-mode) :indent 2 :queries "pascal" :injection-names "pascal" :definitions :textobjects)

(define-major-mode matlab-mode prog-mode
  "MATLAB and Octave source."
  :lighter "MATLAB"
  :comment "%"
  :extensions ("m")
  :interpreters ("octave-cli" "matlab"))

(define-language matlab
  "MATLAB and Octave source, parsed with tree-sitter-matlab."
  :grammar "matlab" :modes (matlab-mode) :indent 2 :queries "matlab" :definitions :textobjects)

(define-major-mode vala-mode prog-mode
  "Vala source."
  :lighter "Vala"
  :comment "//"
  :extensions ("vala" "vapi"))

(define-language vala
  "Vala source, parsed with tree-sitter-vala."
  :grammar "vala" :modes (vala-mode) :indent 2 :queries "vala" :injection-names "vala" :definitions :textobjects)

(define-major-mode solidity-mode prog-mode
  "Solidity contracts."
  :lighter "Solidity"
  :comment "//"
  :extensions ("sol"))

(define-language solidity
  "Solidity contracts, parsed with tree-sitter-solidity."
  :grammar "solidity" :modes (solidity-mode) :indent 4 :queries "solidity" :injection-names "sol|solidity" :definitions :textobjects)

(define-major-mode gdscript-mode prog-mode
  "GDScript, the scripting language of the Godot engine."
  :lighter "GDScript"
  :comment "#"
  :extensions ("gd"))

(define-language gdscript
  "GDScript, the scripting language of the Godot engine, parsed with tree-sitter-gdscript."
  :grammar "gdscript" :modes (gdscript-mode) :indent 4 :queries "gdscript" :injection-names "gdscript" :definitions :textobjects)

(define-major-mode haxe-mode prog-mode
  "Haxe source."
  :lighter "Haxe"
  :comment "//"
  :extensions ("hx"))

(define-language haxe
  "Haxe source, parsed with tree-sitter-haxe."
  :grammar "haxe" :modes (haxe-mode) :indent 4 :queries "haxe" :injection-names "hx|haxe")

(define-major-mode janet-mode prog-mode
  "Janet source. Only read and coloured here: nothing of it is evaluated."
  :lighter "Janet"
  :comment "#"
  :extensions ("cgen" "janet" "jdn")
  :interpreters ("janet"))

(define-language janet
  "Janet source, parsed with tree-sitter-janet-simple."
  :grammar "janet-simple" :modes (janet-mode) :indent 2 :queries "janet" :injection-names "janet")

(define-major-mode hy-mode prog-mode
  "Hy source. Only read and coloured here: nothing of it is evaluated."
  :lighter "Hy"
  :comment (";" ";;")
  :extensions ("hy"))

(define-language hy
  "Hy source, parsed with tree-sitter-scheme."
  :grammar "scheme" :modes (hy-mode) :indent 1 :queries "hy")

(define-major-mode teal-mode prog-mode
  "Teal source: Lua with types."
  :lighter "Teal"
  :comment "--"
  :extensions ("tl"))

(define-language teal
  "Teal source: Lua with types, parsed with tree-sitter-teal."
  :grammar "teal" :modes (teal-mode) :indent 4 :queries "teal" :injection-names "teal")

(define-major-mode luau-mode prog-mode
  "Luau source: the Lua of Roblox."
  :lighter "Luau"
  :comment "--"
  :extensions ("luau"))

(define-language luau
  "Luau source: the Lua of Roblox, parsed with tree-sitter-luau."
  :grammar "luau" :modes (luau-mode) :indent 2 :queries "luau" :injection-names "luau" :definitions :textobjects)

(define-major-mode forth-mode prog-mode
  "Forth source."
  :lighter "Forth"
  :comment "\\"
  :extensions ("forth" "fth" "4th"))

(define-language forth
  "Forth source, parsed with tree-sitter-forth."
  :grammar "forth" :modes (forth-mode) :indent 3 :queries "forth" :injection-names "forth")

(define-major-mode llvm-mode prog-mode
  "LLVM intermediate representation."
  :lighter "LLVM"
  :comment (";" ";;")
  :extensions ("ll"))

(define-language llvm
  "LLVM intermediate representation, parsed with tree-sitter-llvm."
  :grammar "llvm" :modes (llvm-mode) :indent 2 :queries "llvm" :injection-names "llvm" :definitions :textobjects)

(define-major-mode asm-mode prog-mode
  "Assembly for the GNU assembler."
  :lighter "Asm"
  :comment "#"
  :extensions ("s" "S"))

(define-language gas
  "Assembly for the GNU assembler, parsed with tree-sitter-gas."
  :grammar "gas" :modes (asm-mode) :indent 8 :queries "gas" :injection-names "gas" :definitions :textobjects)

(define-major-mode nasm-mode prog-mode
  "Assembly for the Netwide Assembler."
  :lighter "NASM"
  :comment (";" ";;")
  :extensions ("asm" "nasm"))

(define-language nasm
  "Assembly for the Netwide Assembler, parsed with tree-sitter-nasm."
  :grammar "nasm" :modes (nasm-mode) :indent 8 :queries "nasm" :injection-names "n?asm" :definitions :textobjects)

(define-major-mode wat-mode prog-mode
  "WebAssembly in its text format."
  :lighter "WAT"
  :comment ";;"
  :extensions ("wat"))

(define-language wat
  "WebAssembly in its text format, parsed with tree-sitter-wat."
  :grammar "wat" :modes (wat-mode) :indent 4 :queries "wat")

(define-major-mode glsl-mode prog-mode
  "OpenGL shaders."
  :lighter "GLSL"
  :comment "//"
  :extensions ("glsl" "vert" "tesc" "tese" "geom" "frag" "comp"))

(define-language glsl
  "OpenGL shaders, parsed with tree-sitter-glsl."
  :grammar "glsl" :modes (glsl-mode) :indent 4 :queries "glsl" :injection-names "glsl" :definitions :textobjects)

(define-major-mode wgsl-mode prog-mode
  "WebGPU shaders."
  :lighter "WGSL"
  :comment "//"
  :extensions ("wgsl"))

(define-language wgsl
  "WebGPU shaders, parsed with tree-sitter-wgsl."
  :grammar "wgsl" :modes (wgsl-mode) :indent 4 :queries "wgsl" :definitions :textobjects)

(define-major-mode vhdl-mode prog-mode
  "VHDL descriptions of hardware."
  :lighter "VHDL"
  :comment "--"
  :extensions ("vhd" "vhdl"))

(define-language vhdl
  "VHDL descriptions of hardware, parsed with tree-sitter-vhdl."
  :grammar "vhdl" :modes (vhdl-mode) :indent 2 :queries "vhdl" :injection-names "vhdl")

(define-major-mode cython-mode prog-mode
  "Cython source."
  :lighter "Cython"
  :comment "#"
  :extensions ("pxd" "pxi" "pyx"))

(define-language cython
  "Cython source, parsed with tree-sitter-cython."
  :grammar "cython" :modes (cython-mode) :indent 4 :queries "cython")

(define-major-mode gnuplot-mode prog-mode
  "Gnuplot scripts."
  :lighter "Gnuplot"
  :comment "#"
  :extensions ("gnuplot" "plot" "plt"))

(define-language gnuplot
  "Gnuplot scripts, parsed with tree-sitter-gnuplot."
  :grammar "gnuplot" :modes (gnuplot-mode) :indent 4 :queries "gnuplot" :injection-names "gnuplot")

(define-major-mode scad-mode prog-mode
  "OpenSCAD models."
  :lighter "SCAD"
  :comment "//"
  :extensions ("scad"))

(define-language openscad
  "OpenSCAD models, parsed with tree-sitter-openscad."
  :grammar "openscad" :modes (scad-mode) :indent 2 :queries "openscad" :injection-names "openscad")

(define-major-mode smali-mode prog-mode
  "Smali: the Dalvik bytecode of Android apps, disassembled."
  :lighter "Smali"
  :comment "#"
  :extensions ("smali"))

(define-language smali
  "Smali: the Dalvik bytecode of Android apps, disassembled, parsed with tree-sitter-smali."
  :grammar "smali" :modes (smali-mode) :indent 4 :queries "smali" :injection-names "smali")

(define-major-mode yara-mode prog-mode
  "YARA rules, which describe files to find by what is in them."
  :lighter "YARA"
  :comment "//"
  :extensions ("yara" "yar"))

(define-language yara
  "YARA rules, which describe files to find by what is in them, parsed with tree-sitter-yara."
  :grammar "yara" :modes (yara-mode) :indent 2 :queries "yara")

(define-major-mode rego-mode prog-mode
  "Rego policies of the Open Policy Agent."
  :lighter "Rego"
  :comment "#"
  :extensions ("rego"))

(define-language rego
  "Rego policies of the Open Policy Agent, parsed with tree-sitter-rego."
  :grammar "rego" :modes (rego-mode) :indent 4 :queries "rego" :injection-names "rego")

(define-major-mode agda2-mode prog-mode
  "Agda source."
  :lighter "Agda"
  :comment "--"
  :extensions ("agda"))

(define-language agda
  "Agda source, parsed with tree-sitter-agda."
  :grammar "agda" :modes (agda2-mode) :indent 2 :queries "agda" :injection-names "agda")

(define-major-mode alloy-mode prog-mode
  "Grafana Alloy configuration."
  :lighter "Alloy"
  :comment "//"
  :extensions ("alloy"))

(define-language alloy
  "Grafana Alloy configuration, parsed with tree-sitter-alloy."
  :grammar "alloy" :modes (alloy-mode) :indent 2 :queries "alloy" :injection-names "alloy")

(define-major-mode basic-mode prog-mode
  "BASIC source."
  :lighter "Basic"
  :comment "REM"
  :extensions ("bas"))

(define-language basic
  "BASIC source, parsed with tree-sitter-basic."
  :grammar "basic" :modes (basic-mode) :indent 2 :queries "basic" :injection-names "basic" :definitions :textobjects)

(define-major-mode bass-mode prog-mode
  "Bass scripts: a Lisp for running commands. Only read and coloured here: nothing of it is evaluated."
  :lighter "Bass"
  :comment (";" ";;")
  :extensions ("bass"))

(define-language bass
  "Bass scripts: a Lisp for running commands, parsed with tree-sitter-bass."
  :grammar "bass" :modes (bass-mode) :indent 2 :queries "bass" :injection-names "bass")

(define-major-mode bicep-mode prog-mode
  "Bicep: templates of Azure resources."
  :lighter "Bicep"
  :comment "//"
  :extensions ("bicep" "bicepparam"))

(define-language bicep
  "Bicep: templates of Azure resources, parsed with tree-sitter-bicep."
  :grammar "bicep" :modes (bicep-mode) :indent 1 :queries "bicep")

(define-major-mode c3-mode prog-mode
  "C3 source."
  :lighter "C3"
  :comment "//"
  :extensions ("c3" "c3i" "c3t"))

(define-language c3
  "C3 source, parsed with tree-sitter-c3."
  :grammar "c3" :modes (c3-mode) :indent 4 :queries "c3" :injection-names "c3")

(define-major-mode cairo-mode prog-mode
  "Cairo programs, for Starknet."
  :lighter "Cairo"
  :comment "//"
  :extensions ("cairo"))

(define-language cairo
  "Cairo programs, for Starknet, parsed with tree-sitter-cairo."
  :grammar "cairo" :modes (cairo-mode) :indent 4 :queries "cairo" :injection-names "cairo" :definitions :textobjects)

(define-major-mode cel-mode prog-mode
  "CEL: the Common Expression Language."
  :lighter "CEL"
  :comment "//"
  :extensions ("cel"))

(define-language cel
  "CEL: the Common Expression Language, parsed with tree-sitter-cel."
  :grammar "cel" :modes (cel-mode) :indent 2 :queries "cel" :injection-names "cel")

(define-major-mode chuck-mode prog-mode
  "ChucK: programs that make music."
  :lighter "ChucK"
  :comment "//"
  :extensions ("ck"))

(define-language chuck
  "ChucK: programs that make music, parsed with tree-sitter-chuck."
  :grammar "chuck" :modes (chuck-mode) :indent 4 :queries "chuck" :injection-names "chuck$|^ck")

(define-major-mode circom-mode prog-mode
  "Circom circuits for zero-knowledge proofs."
  :lighter "Circom"
  :comment "//"
  :extensions ("circom"))

(define-language circom
  "Circom circuits for zero-knowledge proofs, parsed with tree-sitter-circom."
  :grammar "circom" :modes (circom-mode) :indent 4 :queries "circom" :injection-names "circom")

(define-major-mode clarity-mode prog-mode
  "Clarity smart contracts."
  :lighter "Clarity"
  :comment ";;"
  :extensions ("clar"))

(define-language clarity
  "Clarity smart contracts, parsed with tree-sitter-clarity."
  :grammar "clarity" :modes (clarity-mode) :indent 2 :queries "clarity" :injection-names "clarity")

(define-major-mode ql-mode prog-mode
  "CodeQL queries."
  :lighter "QL"
  :comment "//"
  :extensions ("ql" "qll"))

(define-language codeql
  "CodeQL queries, parsed with tree-sitter-ql."
  :grammar "ql" :modes (ql-mode) :indent 2 :queries "codeql" :injection-names "codeql" :definitions :textobjects)

(define-major-mode ebnf-mode prog-mode
  "Grammars in extended Backus-Naur form."
  :lighter "EBNF"
  :block-comment ("(*" "*)")
  :extensions ("ebnf"))

(define-language ebnf
  "Grammars in extended Backus-Naur form, parsed with tree-sitter-ebnf."
  :grammar "ebnf" :modes (ebnf-mode) :indent 4 :queries "ebnf" :injection-names "ebnf")

(define-major-mode eiffel-mode prog-mode
  "Eiffel source."
  :lighter "Eiffel"
  :comment "--"
  :extensions ("e"))

(define-language eiffel
  "Eiffel source, parsed with tree-sitter-eiffel."
  :grammar "eiffel" :modes (eiffel-mode) :indent 4 :queries "eiffel" :definitions :textobjects)

(define-major-mode freebasic-mode prog-mode
  "FreeBASIC source."
  :lighter "FreeBASIC"
  :comment "'"
  :extensions ("bi"))

(define-language freebasic
  "FreeBASIC source, parsed with tree-sitter-freebasic."
  :grammar "freebasic" :modes (freebasic-mode) :indent 4 :queries "freebasic" :injection-names "freebasic" :definitions :textobjects)

(define-major-mode glimmer-js-mode prog-mode
  "Glimmer components in JavaScript (Ember)."
  :lighter "GJS"
  :comment "//"
  :extensions ("gjs"))

(define-language gjs
  "Glimmer components in JavaScript (Ember), parsed with tree-sitter-glimmer-javascript."
  :grammar "glimmer-javascript" :modes (glimmer-js-mode) :indent 2 :queries "gjs" :definitions :textobjects)

(define-major-mode glimmer-ts-mode prog-mode
  "Glimmer components in TypeScript (Ember)."
  :lighter "GTS"
  :comment "//"
  :extensions ("gts"))

(define-language gts
  "Glimmer components in TypeScript (Ember), parsed with tree-sitter-glimmer-typescript."
  :grammar "glimmer-typescript" :modes (glimmer-ts-mode) :indent 2 :queries "gts" :definitions :textobjects)

(define-major-mode gren-mode prog-mode
  "Gren source."
  :lighter "Gren"
  :comment "--"
  :extensions ("gren"))

(define-language gren
  "Gren source, parsed with tree-sitter-gren."
  :grammar "gren" :modes (gren-mode) :indent 4 :queries "gren" :injection-names "gren" :definitions :textobjects)

(define-major-mode literate-haskell-mode haskell-mode
  "Literate Haskell: prose with Haskell code in it."
  :lighter "LitHaskell"
  :comment "--"
  :extensions ("lhs")
  :interpreters ("runhaskell" "stack"))

(define-language haskell-literate
  "Literate Haskell: prose with Haskell code in it, parsed with tree-sitter-haskell-literate."
  :grammar "haskell-literate" :modes (literate-haskell-mode) :indent 2 :queries "haskell-literate" :injection-names "lhs")

(define-major-mode hdl-mode prog-mode
  "The hardware description language of Nand2Tetris."
  :lighter "HDL"
  :extensions ("hdl"))

(define-language hdl
  "The hardware description language of Nand2Tetris, parsed with tree-sitter-hdl."
  :grammar "hdl" :modes (hdl-mode) :indent 4 :queries "hdl" :injection-names "hdl")

(define-major-mode hoon-mode prog-mode
  "Hoon: the language of Urbit."
  :lighter "Hoon"
  :comment "::"
  :extensions ("hoon"))

(define-language hoon
  "Hoon: the language of Urbit, parsed with tree-sitter-hoon."
  :grammar "hoon" :modes (hoon-mode) :indent 2 :queries "hoon" :injection-names "hoon")

(define-major-mode hurl-mode prog-mode
  "Hurl files: HTTP requests and what their answers should be."
  :lighter "Hurl"
  :comment "#"
  :extensions ("hurl"))

(define-language hurl
  "Hurl files: HTTP requests and what their answers should be, parsed with tree-sitter-hurl."
  :grammar "hurl" :modes (hurl-mode) :indent 2 :queries "hurl" :injection-names "hurl" :definitions :textobjects)

(define-major-mode ink-mode prog-mode
  "Ink: inkle's language for interactive stories."
  :lighter "Ink"
  :comment "//"
  :extensions ("ink"))

(define-language ink
  "Ink: inkle's language for interactive stories, parsed with tree-sitter-ink."
  :grammar "ink" :modes (ink-mode) :indent 4 :queries "ink" :injection-names "ink")

(define-major-mode inko-mode prog-mode
  "Inko source."
  :lighter "Inko"
  :comment "#"
  :extensions ("inko"))

(define-language inko
  "Inko source, parsed with tree-sitter-inko."
  :grammar "inko" :modes (inko-mode) :indent 2 :queries "inko" :injection-names "inko" :definitions :textobjects)

(define-major-mode koka-mode prog-mode
  "Koka source."
  :lighter "Koka"
  :comment "//"
  :extensions ("kk"))

(define-language koka
  "Koka source, parsed with tree-sitter-koka."
  :grammar "koka" :modes (koka-mode) :indent 2 :queries "koka" :injection-names "koka")

(define-major-mode koto-mode prog-mode
  "Koto scripts."
  :lighter "Koto"
  :comment "#"
  :extensions ("koto"))

(define-language koto
  "Koto scripts, parsed with tree-sitter-koto."
  :grammar "koto" :modes (koto-mode) :indent 2 :queries "koto" :injection-names "koto" :definitions :textobjects)

(define-major-mode mojo-mode prog-mode
  "Mojo source."
  :lighter "Mojo"
  :comment "#"
  :extensions ("mojo" "🔥"))

(define-language mojo
  "Mojo source, parsed with tree-sitter-mojo."
  :grammar "mojo" :modes (mojo-mode) :indent 4 :queries "mojo" :injection-names "mojo" :definitions :textobjects)

(define-major-mode move-mode prog-mode
  "Move smart contracts."
  :lighter "Move"
  :comment "//"
  :extensions ("move"))

(define-major-mode nearley-mode prog-mode
  "Nearley grammars."
  :lighter "Nearley"
  :comment "#"
  :extensions ("ne"))

(define-language nearley
  "Nearley grammars, parsed with tree-sitter-nearley."
  :grammar "nearley" :modes (nearley-mode) :indent 2 :queries "nearley")

(define-major-mode ohm-mode prog-mode
  "Ohm grammars."
  :lighter "Ohm"
  :comment "//"
  :extensions ("ohm"))

(define-language ohm
  "Ohm grammars, parsed with tree-sitter-ohm."
  :grammar "ohm" :modes (ohm-mode) :indent 2 :queries "ohm" :injection-names "ohm" :definitions :textobjects)

(define-major-mode pest-mode prog-mode
  "Pest grammars, for Rust."
  :lighter "Pest"
  :comment "//"
  :extensions ("pest"))

(define-language pest
  "Pest grammars, for Rust, parsed with tree-sitter-pest."
  :grammar "pest" :modes (pest-mode) :indent 4 :queries "pest" :injection-names "pest" :definitions :textobjects)

(define-major-mode picat-mode prog-mode
  "Picat source."
  :lighter "Picat"
  :comment "%"
  :extensions ("pi" "picat")
  :interpreters ("picat"))

(define-language picat
  "Picat source, parsed with tree-sitter-picat."
  :grammar "picat" :modes (picat-mode) :indent 4 :queries "picat" :injection-names "pi|picat" :definitions :textobjects)

(define-major-mode ponylang-mode prog-mode
  "Pony source."
  :lighter "Pony"
  :comment "//"
  :extensions ("pony"))

(define-language ponylang
  "Pony source, parsed with tree-sitter-ponylang."
  :grammar "ponylang" :modes (ponylang-mode) :indent 2 :queries "ponylang" :injection-names "pony" :definitions :textobjects)

(define-major-mode proverif-mode prog-mode
  "ProVerif models of protocols."
  :lighter "ProVerif"
  :block-comment ("(*" "*)")
  :extensions ("pv"))

(define-language proverif
  "ProVerif models of protocols, parsed with tree-sitter-proverif."
  :grammar "proverif" :modes (proverif-mode) :indent 8 :queries "proverif" :injection-names "proverif")

(define-major-mode prql-mode prog-mode
  "PRQL queries."
  :lighter "PRQL"
  :comment "#"
  :extensions ("prql"))

(define-language prql
  "PRQL queries, parsed with tree-sitter-prql."
  :grammar "prql" :modes (prql-mode) :indent 4 :queries "prql" :injection-names "prql")

(define-major-mode ptx-mode prog-mode
  "NVIDIA's PTX assembly."
  :lighter "PTX"
  :comment "//"
  :extensions ("ptx"))

(define-language ptx
  "NVIDIA's PTX assembly, parsed with tree-sitter-ptx."
  :grammar "ptx" :modes (ptx-mode) :indent 4 :queries "ptx" :injection-names "ptx")

(define-major-mode qml-mode prog-mode
  "QML: Qt's markup for user interfaces."
  :lighter "QML"
  :comment "//"
  :extensions ("qml"))

(define-language qml
  "QML: Qt's markup for user interfaces, parsed with tree-sitter-qmljs."
  :grammar "qmljs" :modes (qml-mode) :indent 4 :queries "qml" :definitions :textobjects)

(define-major-mode quint-mode prog-mode
  "Quint specifications."
  :lighter "Quint"
  :comment "//"
  :extensions ("qnt"))

(define-language quint
  "Quint specifications, parsed with tree-sitter-quint."
  :grammar "quint" :modes (quint-mode) :indent 2 :queries "quint")

(define-major-mode rescript-mode prog-mode
  "ReScript source."
  :lighter "ReScript"
  :comment "//"
  :extensions ("res"))

(define-language rescript
  "ReScript source, parsed with tree-sitter-rescript."
  :grammar "rescript" :modes (rescript-mode) :indent 2 :queries "rescript" :injection-names "rescript" :definitions :textobjects)

(define-major-mode ripple-mode prog-mode
  "Ripple components: a TypeScript framework for user interfaces."
  :lighter "Ripple"
  :comment "//"
  :extensions ("ripple"))

(define-language ripple
  "Ripple components: a TypeScript framework for user interfaces, parsed with tree-sitter-ripple."
  :grammar "ripple" :modes (ripple-mode) :indent 4 :queries "ripple")

(define-major-mode robot-mode prog-mode
  "Robot Framework tests."
  :lighter "Robot"
  :comment "#"
  :extensions ("robot" "resource"))

(define-language robot
  "Robot Framework tests, parsed with tree-sitter-robot."
  :grammar "robot" :modes (robot-mode) :indent 1 :queries "robot" :injection-names "robot")

(define-major-mode sage-mode prog-mode
  "SageMath source."
  :lighter "Sage"
  :comment "#"
  :extensions ("sage"))

(define-language sage
  "SageMath source, parsed with tree-sitter-python."
  :grammar "python" :modes (sage-mode) :indent 4 :queries "sage" :injection-names "sage" :definitions :textobjects)

(define-major-mode slang-mode prog-mode
  "Slang shaders."
  :lighter "Slang"
  :comment "//"
  :extensions ("slang"))

(define-language slang
  "Slang shaders, parsed with tree-sitter-slang."
  :grammar "slang" :modes (slang-mode) :indent 2 :queries "slang" :injection-names "slang" :definitions :textobjects)

(define-major-mode slint-mode prog-mode
  "Slint: a markup for user interfaces."
  :lighter "Slint"
  :comment "//"
  :extensions ("slint"))

(define-language slint
  "Slint: a markup for user interfaces, parsed with tree-sitter-slint."
  :grammar "slint" :modes (slint-mode) :indent 4 :queries "slint" :injection-names "slint" :definitions :textobjects)

(define-major-mode slisp-mode prog-mode
  "SlightLisp source. Only read and coloured here: nothing of it is evaluated."
  :lighter "SLisp"
  :comment (";" ";;")
  :extensions ("sl"))

(define-language slisp
  "SlightLisp source, parsed with tree-sitter-slisp."
  :grammar "slisp" :modes (slisp-mode) :indent 2 :queries "slisp" :injection-names "sl")

(define-major-mode sourcepawn-mode prog-mode
  "SourcePawn scripts."
  :lighter "SourcePawn"
  :comment "//"
  :extensions ("sp"))

(define-language sourcepawn
  "SourcePawn scripts, parsed with tree-sitter-sourcepawn."
  :grammar "sourcepawn" :modes (sourcepawn-mode) :indent 2 :queries "sourcepawn" :definitions :textobjects)

(define-major-mode spade-mode prog-mode
  "Spade: a language that describes hardware."
  :lighter "Spade"
  :comment "//"
  :extensions ("spade"))

(define-language spade
  "Spade: a language that describes hardware, parsed with tree-sitter-spade."
  :grammar "spade" :modes (spade-mode) :indent 4 :queries "spade" :injection-names "spade")

(define-major-mode sclang-mode prog-mode
  "SuperCollider source."
  :lighter "SCLang"
  :comment "//"
  :extensions ("scd" "quark"))

(define-language supercollider
  "SuperCollider source, parsed with tree-sitter-supercollider."
  :grammar "supercollider" :modes (sclang-mode) :indent 4 :queries "supercollider" :injection-names "supercollider")

(define-major-mode sway-mode prog-mode
  "Sway smart contracts, for Fuel."
  :lighter "Sway"
  :comment "//"
  :extensions ("sw"))

(define-language sway
  "Sway smart contracts, for Fuel, parsed with tree-sitter-sway."
  :grammar "sway" :modes (sway-mode) :indent 4 :queries "sway" :injection-names "sway" :definitions :textobjects)

(define-major-mode systemverilog-mode prog-mode
  "SystemVerilog descriptions of hardware."
  :lighter "SystemVerilog"
  :comment "//"
  :extensions ("sv" "svh"))

(define-language systemverilog
  "SystemVerilog descriptions of hardware, parsed with tree-sitter-systemverilog."
  :grammar "systemverilog" :modes (systemverilog-mode) :indent 2 :queries "systemverilog")

(define-major-mode t32-mode prog-mode
  "TRACE32 PRACTICE scripts."
  :lighter "T32"
  :comment (";" ";;")
  :extensions ("cmm" "t32"))

(define-language t32
  "TRACE32 PRACTICE scripts, parsed with tree-sitter-t32."
  :grammar "t32" :modes (t32-mode) :indent 2 :queries "t32" :injection-names "t32")

(define-major-mode tablegen-mode prog-mode
  "LLVM's TableGen records."
  :lighter "TableGen"
  :comment "//"
  :extensions ("td"))

(define-language tablegen
  "LLVM's TableGen records, parsed with tree-sitter-tablegen."
  :grammar "tablegen" :modes (tablegen-mode) :indent 2 :queries "tablegen" :injection-names "tablegen" :definitions :textobjects)

(define-major-mode tact-mode prog-mode
  "Tact smart contracts, for TON."
  :lighter "Tact"
  :comment "//"
  :extensions ("tact"))

(define-language tact
  "Tact smart contracts, for TON, parsed with tree-sitter-tact."
  :grammar "tact" :modes (tact-mode) :indent 4 :queries "tact" :injection-names "tact" :definitions :textobjects)

(define-major-mode tla-mode prog-mode
  "TLA+ specifications."
  :lighter "TLA+"
  :comment "\\*"
  :extensions ("tla"))

(define-language tlaplus
  "TLA+ specifications, parsed with tree-sitter-tlaplus."
  :grammar "tlaplus" :modes (tla-mode) :indent 1 :queries "tlaplus" :injection-names "tla")

(define-major-mode tolk-mode prog-mode
  "Tolk smart contracts, for TON."
  :lighter "Tolk"
  :comment "//"
  :extensions ("tolk"))

(define-language tolk
  "Tolk smart contracts, for TON, parsed with tree-sitter-tolk."
  :grammar "tolk" :modes (tolk-mode) :indent 4 :queries "tolk" :injection-names "tolk")

(define-major-mode tql-mode prog-mode
  "TQL: the Tenzir Query Language."
  :lighter "TQL"
  :comment "//"
  :extensions ("tql"))

(define-language tql
  "TQL: the Tenzir Query Language, parsed with tree-sitter-tql."
  :grammar "tql" :modes (tql-mode) :indent 2 :queries "tql" :injection-names "tql")

(define-major-mode ungrammar-mode prog-mode
  "Ungrammar: descriptions of concrete syntax trees."
  :lighter "Ungrammar"
  :comment "//"
  :extensions ("ungram" "ungrammar"))

(define-language ungrammar
  "Ungrammar: descriptions of concrete syntax trees, parsed with tree-sitter-ungrammar."
  :grammar "ungrammar" :modes (ungrammar-mode) :indent 2 :queries "ungrammar" :injection-names "ungrammar")

(define-major-mode unison-mode prog-mode
  "Unison source."
  :lighter "Unison"
  :comment "--"
  :extensions ("u"))

(define-language unison
  "Unison source, parsed with tree-sitter-unison."
  :grammar "unison" :modes (unison-mode) :indent 4 :queries "unison" :injection-names "unison" :definitions :textobjects)

(define-major-mode uxntal-mode prog-mode
  "Uxntal: the assembly of the Uxn virtual machine."
  :lighter "Uxntal"
  :block-comment ("(" ")")
  :extensions ("tal"))

(define-language uxntal
  "Uxntal: the assembly of the Uxn virtual machine, parsed with tree-sitter-uxntal."
  :grammar "uxntal" :modes (uxntal-mode) :indent 4 :queries "uxntal" :injection-names "tal")

(define-major-mode wast-mode prog-mode
  "WebAssembly test scripts."
  :lighter "WAST"
  :comment ";;"
  :extensions ("wast"))

(define-language wast
  "WebAssembly test scripts, parsed with tree-sitter-wast."
  :grammar "wast" :modes (wast-mode) :indent 4 :queries "wast")

(define-major-mode wesl-mode prog-mode
  "WESL shaders: WGSL extended."
  :lighter "WESL"
  :comment "//"
  :extensions ("wesl"))

(define-language wesl
  "WESL shaders: WGSL extended, parsed with tree-sitter-wesl."
  :grammar "wesl" :modes (wesl-mode) :indent 4 :queries "wesl" :definitions :textobjects)

(define-major-mode wit-mode prog-mode
  "WIT: the interfaces of WebAssembly components."
  :lighter "WIT"
  :comment "//"
  :extensions ("wit"))

(define-language wit
  "WIT: the interfaces of WebAssembly components, parsed with tree-sitter-wit."
  :grammar "wit" :modes (wit-mode) :indent 2 :queries "wit" :injection-names "wit")

(define-major-mode wren-mode prog-mode
  "Wren scripts."
  :lighter "Wren"
  :extensions ("wren"))

(define-language wren
  "Wren scripts, parsed with tree-sitter-wren."
  :grammar "wren" :modes (wren-mode) :indent 2 :queries "wren" :injection-names "wren" :definitions :textobjects)

(define-major-mode yuck-mode prog-mode
  "Yuck: the widgets of eww."
  :lighter "Yuck"
  :comment (";" ";;")
  :extensions ("yuck"))

(define-language yuck
  "Yuck: the widgets of eww, parsed with tree-sitter-yuck."
  :grammar "yuck" :modes (yuck-mode) :indent 2 :queries "yuck" :injection-names "yuck")
