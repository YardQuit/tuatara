#!/usr/bin/env python3
# tools/languages/generate.py --- Bring Helix's languages into Tuatara.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Bring Helix's languages into Tuatara: grammar pins (syntax/grammars.tsv),
Helix's query files with their license notice (syntax/{rules,...}/NAME.scm), and one
Lisp file of major modes and languages for each family of them
(lisp/modes/more-languages-*.lisp).

Usage: tools/languages/languages.py > tools/languages/languages.json, then
tools/languages/generate.py (writes everything for the languages in TABLE).
The Helix sources are only read: HELIX_SOURCE names them, ../helix
beside the project by default. Run from the project root."""
import json, os, re
from common import HELIX, helix_commit, lisp_list, lisp_string

ROOT = os.getcwd()
HELIX_COMMIT = helix_commit()
LANGS = {l['name']: l for l in json.load(open(os.path.join(ROOT, 'tools/languages/languages.json')))}

# helix name: (mode, lighter, parent, doc)   mode None: a language only other languages' rules use
TABLE = [
    ('toml', 'toml-mode', 'TOML', 'prog-mode', 'TOML configuration files.'),
    ('json', 'json-mode', 'JSON', 'prog-mode', 'JSON data.'),
    ('jsonc', 'jsonc-mode', 'JSONC', 'prog-mode', 'JSON with comments.'),
    ('json5', 'json5-mode', 'JSON5', 'prog-mode', 'JSON5 data: JSON with comments, unquoted names and more.'),
    ('yaml', 'yaml-mode', 'YAML', 'prog-mode', 'YAML data and configuration.'),
    ('docker-compose', 'docker-compose-mode', 'docker-compose', 'yaml-mode', 'Docker Compose files: the services of an application.'),
    ('github-action', 'github-action-mode', 'GitHub Action', 'yaml-mode', 'GitHub Actions workflows.'),
    ('gitlab-ci', 'gitlab-ci-mode', 'GitLab CI', 'yaml-mode', "GitLab CI's pipelines (.gitlab-ci.yml)."),
    ('woodpecker-ci', 'woodpecker-ci-mode', 'Woodpecker CI', 'yaml-mode', "Woodpecker CI's pipelines."),
    ('nestedtext', 'nestedtext-mode', 'NestedText', 'prog-mode', 'NestedText data.'),
    ('xml', 'xml-mode', 'XML', 'text-mode', 'XML documents.'),
    ('ini', 'ini-mode', 'INI', 'prog-mode', 'INI configuration files.'),
    ('markdown', 'markdown-mode', 'Markdown', 'text-mode', 'Markdown documents.'),
    ('markdown.inline', None, None, None, 'The text inside Markdown\'s blocks: emphasis, code spans, links.'),
    ('html', 'html-mode', 'HTML', 'text-mode', 'HTML documents.'),
    ('css', 'css-mode', 'CSS', 'prog-mode', 'Cascading Style Sheets.'),
    ('scss', 'scss-mode', 'SCSS', 'prog-mode', 'Sass style sheets in the SCSS syntax.'),
    ('javascript', 'js-mode', 'JavaScript', 'prog-mode', 'JavaScript source.'),
    ('jsx', 'js-jsx-mode', 'JSX', 'js-mode', 'JavaScript with JSX elements.'),
    ('typescript', 'typescript-mode', 'TypeScript', 'prog-mode', 'TypeScript source.'),
    ('tsx', 'tsx-mode', 'TSX', 'typescript-mode', 'TypeScript with JSX elements.'),
    ('python', 'python-mode', 'Python', 'prog-mode', 'Python source.'),
    ('go', 'go-mode', 'Go', 'prog-mode', 'Go source.'),
    ('gomod', 'go-mod-mode', 'Go Mod', 'prog-mode', 'Go module files (go.mod).'),
    ('java', 'java-mode', 'Java', 'prog-mode', 'Java source.'),
    ('cpp', 'c++-mode', 'C++', 'prog-mode', 'C++ source and its headers.'),
    ('c-sharp', 'csharp-mode', 'C#', 'prog-mode', 'C# source.'),
    ('lua', 'lua-mode', 'Lua', 'prog-mode', 'Lua source.'),
    ('ruby', 'ruby-mode', 'Ruby', 'prog-mode', 'Ruby source.'),
    ('php', 'php-mode', 'PHP', 'prog-mode', 'PHP source, in HTML or alone.'),
    ('php-only', None, None, None, 'PHP code without the HTML around it.'),
    ('perl', 'perl-mode', 'Perl', 'prog-mode', 'Perl source.'),
    ('sql', 'sql-mode', 'SQL', 'prog-mode', 'SQL statements.'),
    ('make', 'makefile-mode', 'Makefile', 'prog-mode', 'Makefiles.'),
    ('cmake', 'cmake-mode', 'CMake', 'prog-mode', 'CMake build files.'),
    ('nix', 'nix-mode', 'Nix', 'prog-mode', 'Nix expressions.'),
    ('diff', 'diff-mode', 'Diff', 'prog-mode', 'Differences between files: patches.'),
    ('git-config', 'gitconfig-mode', 'Gitconfig', 'prog-mode', "Git's configuration files."),
    ('git-ignore', 'gitignore-mode', 'Gitignore', 'prog-mode', "Git's lists of files to ignore."),
    ('git-attributes', 'gitattributes-mode', 'Gitattributes', 'prog-mode', "Git's attributes of files."),
    ('git-rebase', 'git-rebase-mode', 'Git Rebase', 'prog-mode', "Git's list of commits to rebase (git-rebase-todo)."),
    ('regex', None, None, None, 'Regular expressions inside other languages.'),
    ('elisp', 'emacs-lisp-mode', 'ELisp', 'prog-mode', 'Emacs Lisp source. Only read and coloured here: nothing of it is evaluated.'),
    ('zig', 'zig-mode', 'Zig', 'prog-mode', 'Zig source.'),
    ('haskell', 'haskell-mode', 'Haskell', 'prog-mode', 'Haskell source.'),
    ('kotlin', 'kotlin-mode', 'Kotlin', 'prog-mode', 'Kotlin source.'),
    ('swift', 'swift-mode', 'Swift', 'prog-mode', 'Swift source.'),
    ('scala', 'scala-mode', 'Scala', 'prog-mode', 'Scala source.'),
    ('dart', 'dart-mode', 'Dart', 'prog-mode', 'Dart source.'),
    ('elixir', 'elixir-mode', 'Elixir', 'prog-mode', 'Elixir source.'),
    ('erlang', 'erlang-mode', 'Erlang', 'prog-mode', 'Erlang source.'),
    ('ocaml', 'ocaml-mode', 'OCaml', 'prog-mode', 'OCaml source.'),
    ('ocaml-interface', 'ocaml-interface-mode', 'OCaml Interface', 'prog-mode', 'OCaml interfaces (.mli).'),
    ('latex', 'latex-mode', 'LaTeX', 'text-mode', 'LaTeX documents.'),
    ('bibtex', 'bibtex-mode', 'BibTeX', 'text-mode', 'BibTeX bibliographies.'),
    ('fish', 'fish-mode', 'Fish', 'prog-mode', 'Fish shell scripts.'),
    ('awk', 'awk-mode', 'AWK', 'prog-mode', 'AWK programs.'),
    ('just', 'just-mode', 'Just', 'prog-mode', 'Justfiles: the recipes of the just command runner.'),
    ('protobuf', 'protobuf-mode', 'Protobuf', 'prog-mode', 'Protocol Buffers definitions.'),
    ('graphql', 'graphql-mode', 'GraphQL', 'prog-mode', 'GraphQL schemas and queries.'),
    ('hcl', 'hcl-mode', 'HCL', 'prog-mode', 'HashiCorp configuration: Terraform, Nomad and the rest.'),
    ('vue', 'vue-mode', 'vue', 'html-mode', 'Vue single-file components.'),
    ('svelte', 'svelte-mode', 'Svelte', 'prog-mode', 'Svelte components.'),
    ('powershell', 'powershell-mode', 'PowerShell', 'prog-mode', 'PowerShell scripts.'),
    ('nginx', 'nginx-mode', 'Nginx', 'prog-mode', 'Nginx configuration files.'),
    ('systemd', 'systemd-mode', 'Systemd', 'prog-mode', 'systemd unit files.'),
    ('env', 'dotenv-mode', 'Dotenv', 'prog-mode', 'Environment files (.env).'),
    ('ssh_client_config', 'ssh-config-mode', 'SSH Config', 'prog-mode', 'OpenSSH client configuration.'),
    ('dot', 'graphviz-dot-mode', 'Dot', 'prog-mode', 'Graphviz graphs in the DOT language.'),
    ('meson', 'meson-mode', 'Meson', 'prog-mode', 'Meson build files.'),
    ('groovy', 'groovy-mode', 'Groovy', 'prog-mode', 'Groovy source, Jenkinsfiles and Gradle builds.'),
    ('r', 'r-mode', 'R', 'prog-mode', 'R source.'),
    ('julia', 'julia-mode', 'Julia', 'prog-mode', 'Julia source.'),
    ('clojure', 'clojure-mode', 'Clojure', 'prog-mode', 'Clojure source. Only read and coloured here: nothing of it is evaluated.'),
    ('scheme', 'scheme-mode', 'Scheme', 'prog-mode', 'Scheme source. Only read and coloured here: nothing of it is evaluated.'),
    ('racket', 'racket-mode', 'Racket', 'prog-mode', 'Racket source. Only read and coloured here: nothing of it is evaluated.'),
    ('fennel', 'fennel-mode', 'Fennel', 'prog-mode', 'Fennel source. Only read and coloured here: nothing of it is evaluated.'),
    ('vim', 'vimrc-mode', 'Vim', 'prog-mode', 'Vim script.'),
    ('log', 'log-mode', 'Log', 'prog-mode', 'Log files: their times, levels and messages told apart.'),
    ('jq', 'jq-mode', 'jq', 'prog-mode', 'jq programs.'),
    # the second batch
    ('comment', None, None, None, 'Comments in code: the TODO, FIXME and NOTE marks in them.'),
    ('jsdoc', None, None, None, 'JSDoc comments in JavaScript and TypeScript.'),
    ('luap', None, None, None, 'Lua patterns inside Lua strings.'),
    ('rust-format-args', None, None, None, "The format strings of Rust's println!, format! and the rest."),
    ('rust-format-args-macro', None, None, None, "The arguments of Rust's println!, format! and the rest."),
    ('go-format-string', None, None, None, 'The format strings of Go.'),
    ('lua-format-string', None, None, None, 'The format strings of Lua.'),
    ('ada', 'ada-mode', 'Ada', 'prog-mode', 'Ada source.'),
    ('d', 'd-mode', 'D', 'prog-mode', 'D source.'),
    ('elm', 'elm-mode', 'Elm', 'prog-mode', 'Elm source.'),
    ('fortran', 'fortran-mode', 'Fortran', 'prog-mode', 'Fortran source.'),
    ('fsharp', 'fsharp-mode', 'F#', 'prog-mode', 'F# source.'),
    ('gleam', 'gleam-mode', 'Gleam', 'prog-mode', 'Gleam source.'),
    ('nim', 'nim-mode', 'Nim', 'prog-mode', 'Nim source.'),
    ('odin', 'odin-mode', 'Odin', 'prog-mode', 'Odin source.'),
    ('verilog', 'verilog-mode', 'Verilog', 'prog-mode', 'Verilog descriptions of hardware.'),
    ('v', 'v-mode', 'V', 'prog-mode', 'V source.'),
    ('hare', 'hare-mode', 'Hare', 'prog-mode', 'Hare source.'),
    ('crystal', 'crystal-mode', 'Crystal', 'prog-mode', 'Crystal source.'),
    ('purescript', 'purescript-mode', 'PureScript', 'prog-mode', 'PureScript source.'),
    # ('idris' and 'cabal' have no grammar in Helix's languages.toml)
    ('lean', 'lean4-mode', 'Lean 4', 'prog-mode', 'Lean 4 source.'),
    ('beancount', 'beancount-mode', 'Beancount', 'prog-mode', 'Beancount ledgers: double-entry accounts in plain text.'),
    ('astro', 'astro-mode', 'Astro', 'html-mode', 'Astro components.'),
    ('sml', 'sml-mode', 'SML', 'prog-mode', 'Standard ML source.'),
    ('prolog', 'prolog-mode', 'Prolog', 'prog-mode', 'Prolog source.'),
    ('tcl', 'tcl-mode', 'Tcl', 'prog-mode', 'Tcl scripts.'),
    ('pascal', 'pascal-mode', 'Pascal', 'prog-mode', 'Pascal and Delphi source.'),
    ('matlab', 'matlab-mode', 'MATLAB', 'prog-mode', 'MATLAB and Octave source.'),
    ('vala', 'vala-mode', 'Vala', 'prog-mode', 'Vala source.'),
    ('solidity', 'solidity-mode', 'Solidity', 'prog-mode', 'Solidity contracts.'),
    ('gdscript', 'gdscript-mode', 'GDScript', 'prog-mode', 'GDScript, the scripting language of the Godot engine.'),
    ('haxe', 'haxe-mode', 'Haxe', 'prog-mode', 'Haxe source.'),
    ('janet', 'janet-mode', 'Janet', 'prog-mode', 'Janet source. Only read and coloured here: nothing of it is evaluated.'),
    ('hy', 'hy-mode', 'Hy', 'prog-mode', 'Hy source. Only read and coloured here: nothing of it is evaluated.'),
    ('teal', 'teal-mode', 'Teal', 'prog-mode', 'Teal source: Lua with types.'),
    ('luau', 'luau-mode', 'Luau', 'prog-mode', 'Luau source: the Lua of Roblox.'),
    ('nu', 'nushell-mode', 'Nu', 'prog-mode', 'Nushell scripts.'),
    ('elvish', 'elvish-mode', 'Elvish', 'prog-mode', 'Elvish shell scripts.'),
    ('forth', 'forth-mode', 'Forth', 'prog-mode', 'Forth source.'),
    ('dune', 'dune-mode', 'Dune', 'prog-mode', 'Dune build files, for OCaml.'),
    ('llvm', 'llvm-mode', 'LLVM', 'prog-mode', 'LLVM intermediate representation.'),
    ('gas', 'asm-mode', 'Asm', 'prog-mode', 'Assembly for the GNU assembler.'),
    ('nasm', 'nasm-mode', 'NASM', 'prog-mode', 'Assembly for the Netwide Assembler.'),
    ('wat', 'wat-mode', 'WAT', 'prog-mode', 'WebAssembly in its text format.'),
    ('glsl', 'glsl-mode', 'GLSL', 'prog-mode', 'OpenGL shaders.'),
    ('wgsl', 'wgsl-mode', 'WGSL', 'prog-mode', 'WebGPU shaders.'),
    ('vhdl', 'vhdl-mode', 'VHDL', 'prog-mode', 'VHDL descriptions of hardware.'),
    ('devicetree', 'dts-mode', 'DTS', 'prog-mode', 'Devicetree sources.'),
    ('kconfig', 'kconfig-mode', 'Kconfig', 'prog-mode', 'Kconfig files: the configuration menus of the Linux kernel.'),
    ('ld', 'ld-script-mode', 'LD-Script', 'prog-mode', 'Linker scripts of GNU ld.'),
    ('bitbake', 'bitbake-mode', 'BitBake', 'prog-mode', 'BitBake recipes, as the Yocto Project builds with.'),
    ('thrift', 'thrift-mode', 'Thrift', 'prog-mode', 'Apache Thrift definitions.'),
    ('capnp', 'capnp-mode', "Cap'n Proto", 'prog-mode', "Cap'n Proto schemas."),
    ('prisma', 'prisma-mode', 'Prisma', 'prog-mode', 'Prisma schemas.'),
    ('cython', 'cython-mode', 'Cython', 'prog-mode', 'Cython source.'),
    ('gnuplot', 'gnuplot-mode', 'Gnuplot', 'prog-mode', 'Gnuplot scripts.'),
    ('openscad', 'scad-mode', 'SCAD', 'prog-mode', 'OpenSCAD models.'),
    ('smali', 'smali-mode', 'Smali', 'prog-mode', 'Smali: the Dalvik bytecode of Android apps, disassembled.'),
    ('yara', 'yara-mode', 'YARA', 'prog-mode', 'YARA rules, which describe files to find by what is in them.'),
    ('rego', 'rego-mode', 'Rego', 'prog-mode', 'Rego policies of the Open Policy Agent.'),
    ('starlark', 'bazel-starlark-mode', 'Starlark', 'prog-mode', 'Starlark: the build files of Bazel and Buck.'),
    ('rst', 'rst-mode', 'ReST', 'text-mode', 'reStructuredText documents.'),
    ('typst', 'typst-mode', 'Typst', 'text-mode', 'Typst documents.'),
    ('djot', 'djot-mode', 'Djot', 'text-mode', 'Djot documents.'),
    ('mermaid', 'mermaid-mode', 'Mermaid', 'prog-mode', 'Mermaid diagrams.'),
    ('gemini', 'gemini-mode', 'Gemini', 'text-mode', 'Gemini documents (gemtext).'),
    ('wikitext', 'mediawiki-mode', 'MediaWiki', 'text-mode', 'MediaWiki pages.'),
    ('kdl', 'kdl-mode', 'KDL', 'prog-mode', 'KDL documents.'),
    ('hocon', 'hocon-mode', 'HOCON', 'prog-mode', 'HOCON configuration files.'),
    ('cue', 'cue-mode', 'CUE', 'prog-mode', 'CUE configuration and data.'),
    ('dhall', 'dhall-mode', 'Dhall', 'prog-mode', 'Dhall configuration.'),
    ('nickel', 'nickel-mode', 'Nickel', 'prog-mode', 'Nickel configuration.'),
    ('pkl', 'pkl-mode', 'Pkl', 'prog-mode', 'Pkl configuration.'),
    ('jsonnet', 'jsonnet-mode', 'Jsonnet', 'prog-mode', 'Jsonnet: templates that make JSON.'),
    ('ron', 'ron-mode', 'RON', 'prog-mode', 'Rusty Object Notation.'),
    ('properties', 'conf-javaprop-mode', 'Conf[JavaProp]', 'prog-mode', 'Java properties files.'),
    ('hosts', 'hosts-generic-mode', 'Hosts', 'prog-mode', 'The hosts file: names for addresses.'),
    ('passwd', 'etc-passwd-generic-mode', 'Passwd', 'prog-mode', "The password file: the system's users."),
    ('rpmspec', 'rpm-spec-mode', 'RPM-Spec', 'prog-mode', 'RPM spec files: how a package is built.'),
    ('debian', 'debian-control-mode', 'Debian-Control', 'prog-mode', "The control files of Debian's packages."),
    ('po', 'po-mode', 'PO', 'prog-mode', 'Translations in gettext PO files.'),
    ('ledger', 'ledger-mode', 'Ledger', 'text-mode', 'Ledger accounts.'),
    ('todotxt', 'todotxt-mode', 'Todo.txt', 'text-mode', 'todo.txt lists of tasks.'),
    ('pem', 'pem-mode', 'PEM', 'prog-mode', 'PEM files: certificates and keys in Base64.'),
    ('ldif', 'ldif-mode', 'LDIF', 'prog-mode', 'LDAP directories written out as LDIF.'),
    ('strace', 'strace-mode', 'Strace', 'prog-mode', 'The output of strace: system calls and what they returned.'),
    ('robots.txt', 'robots-txt-mode', 'Robots.txt', 'prog-mode', 'The robots.txt files of web sites.'),
    ('pip-requirements', 'pip-requirements-mode', 'pip-require', 'prog-mode', "Python's requirements files, for pip."),
    ('hyprlang', 'hyprlang-mode', 'Hyprlang', 'prog-mode', 'Hyprland configuration.'),
    ('caddyfile', 'caddyfile-mode', 'Caddyfile', 'prog-mode', 'Caddyfiles: the configuration of the Caddy web server.'),
    ('jinja', 'jinja2-mode', 'Jinja2', 'prog-mode', 'Jinja templates.'),
    ('twig', 'twig-mode', 'Twig', 'prog-mode', 'Twig templates.'),
    ('less', 'less-css-mode', 'Less', 'prog-mode', 'Less style sheets.'),
    ('pug', 'pug-mode', 'Pug', 'prog-mode', 'Pug templates for HTML.'),
    ('heex', 'heex-mode', 'HEEx', 'prog-mode', 'HEEx: the HTML templates of Phoenix.'),
    ('templ', 'templ-mode', 'templ', 'prog-mode', 'templ components: HTML in Go.'),
    ('git-commit', 'git-commit-mode', None, None, "Git's commit messages."),
    # the third batch
    ('markdown-rustdoc', None, None, None, "Rust's documentation comments, written in Markdown."),
    ('htmldjango', None, None, None, 'Django templates in HTML.'),
    ('edoc', None, None, None, "EDoc: the documentation in Erlang's comments."),
    ('adl', 'adl-mode', 'ADL', 'prog-mode', 'ADL: data models written once for many languages.'),
    ('agda', 'agda2-mode', 'Agda', 'prog-mode', 'Agda source.'),
    ('alloy', 'alloy-mode', 'Alloy', 'prog-mode', 'Grafana Alloy configuration.'),
    ('amber', 'amber-mode', 'Amber', 'prog-mode', 'Amber scripts, which compile to Bash.'),
    ('basic', 'basic-mode', 'Basic', 'prog-mode', 'BASIC source.'),
    ('bass', 'bass-mode', 'Bass', 'prog-mode', 'Bass scripts: a Lisp for running commands. Only read and coloured here: nothing of it is evaluated.'),
    ('bicep', 'bicep-mode', 'Bicep', 'prog-mode', 'Bicep: templates of Azure resources.'),
    ('blade', 'blade-mode', 'Blade', 'prog-mode', "Blade: Laravel's templates."),
    ('blueprint', 'blueprint-mode', 'Blueprint', 'prog-mode', "Blueprint: a markup for GTK's user interfaces."),
    ('c3', 'c3-mode', 'C3', 'prog-mode', 'C3 source.'),
    ('cairo', 'cairo-mode', 'Cairo', 'prog-mode', 'Cairo programs, for Starknet.'),
    ('cel', 'cel-mode', 'CEL', 'prog-mode', 'CEL: the Common Expression Language.'),
    ('chuck', 'chuck-mode', 'ChucK', 'prog-mode', 'ChucK: programs that make music.'),
    ('circom', 'circom-mode', 'Circom', 'prog-mode', 'Circom circuits for zero-knowledge proofs.'),
    ('clarity', 'clarity-mode', 'Clarity', 'prog-mode', 'Clarity smart contracts.'),
    ('codeql', 'ql-mode', 'QL', 'prog-mode', 'CodeQL queries.'),
    ('concerto', 'concerto-mode', 'Concerto', 'prog-mode', 'Concerto data models.'),
    ('cpon', 'cpon-mode', 'CPON', 'prog-mode', 'CPON: the ChainPack Object Notation of Silicon Heaven.'),
    ('cylc', 'cylc-mode', 'Cylc', 'prog-mode', 'Cylc workflow definitions.'),
    ('dbml', 'dbml-mode', 'DBML', 'prog-mode', 'DBML: database schemas written as markup.'),
    ('doxyfile', 'doxyfile-mode', 'Doxyfile', 'prog-mode', "Doxyfiles: Doxygen's configuration."),
    ('dtd', 'dtd-mode', 'DTD', 'prog-mode', 'XML document type definitions.'),
    ('dunstrc', 'dunstrc-mode', 'Dunstrc', 'prog-mode', 'The configuration of the dunst notification daemon.'),
    ('earthfile', 'earthfile-mode', 'Earthfile', 'prog-mode', 'Earthfiles: builds for Earthly.'),
    ('ebnf', 'ebnf-mode', 'EBNF', 'prog-mode', 'Grammars in extended Backus-Naur form.'),
    ('eex', 'eex-mode', 'EEx', 'prog-mode', 'EEx: templates of embedded Elixir.'),
    ('eiffel', 'eiffel-mode', 'Eiffel', 'prog-mode', 'Eiffel source.'),
    ('ejs', 'ejs-mode', 'EJS', 'prog-mode', 'EJS: templates of embedded JavaScript.'),
    ('erb', 'erb-mode', 'ERB', 'prog-mode', 'ERB: templates of embedded Ruby.'),
    ('esdl', 'esdl-mode', 'ESDL', 'prog-mode', 'Schemas of the Gel (EdgeDB) database.'),
    ('fga', 'fga-mode', 'FGA', 'prog-mode', 'OpenFGA models of authorisation.'),
    ('fidl', 'fidl-mode', 'FIDL', 'prog-mode', "FIDL: Fuchsia's interface definitions."),
    ('flatbuffers', 'flatbuffers-mode', 'FlatBuffers', 'prog-mode', 'FlatBuffers schemas.'),
    ('freebasic', 'freebasic-mode', 'FreeBASIC', 'prog-mode', 'FreeBASIC source.'),
    ('gherkin', 'feature-mode', 'Feature', 'text-mode', "Gherkin: the feature files of Cucumber."),
    ('ghostty', 'ghostty-mode', 'Ghostty', 'prog-mode', 'The configuration of the Ghostty terminal.'),
    ('glimmer', 'glimmer-mode', 'Glimmer', 'prog-mode', "Glimmer templates, Ember's."),
    ('gjs', 'glimmer-js-mode', 'GJS', 'prog-mode', 'Glimmer components in JavaScript (Ember).'),
    ('gts', 'glimmer-ts-mode', 'GTS', 'prog-mode', 'Glimmer components in TypeScript (Ember).'),
    ('gn', 'gn-mode', 'GN', 'prog-mode', 'GN build files, as Chromium builds with.'),
    ('godot-resource', 'godot-resource-mode', 'Godot-Resource', 'prog-mode', "Godot's scenes and resources."),
    ('gotmpl', 'go-template-mode', 'Go-Template', 'prog-mode', "Go's templates."),
    ('gowork', 'go-work-mode', 'Go Work', 'prog-mode', 'Go workspace files (go.work).'),
    ('gpr', 'gpr-mode', 'GPR', 'prog-mode', 'GNAT project files, for Ada.'),
    ('gren', 'gren-mode', 'Gren', 'prog-mode', 'Gren source.'),
    ('haskell-literate', 'literate-haskell-mode', 'LitHaskell', 'haskell-mode', 'Literate Haskell: prose with Haskell code in it.'),
    ('hdl', 'hdl-mode', 'HDL', 'prog-mode', 'The hardware description language of Nand2Tetris.'),
    ('hoon', 'hoon-mode', 'Hoon', 'prog-mode', 'Hoon: the language of Urbit.'),
    ('hurl', 'hurl-mode', 'Hurl', 'prog-mode', 'Hurl files: HTTP requests and what their answers should be.'),
    ('ink', 'ink-mode', 'Ink', 'prog-mode', "Ink: inkle's language for interactive stories."),
    ('inko', 'inko-mode', 'Inko', 'prog-mode', 'Inko source.'),
    ('jjdescription', 'jj-description-mode', 'JJ-Description', 'text-mode', 'The descriptions of Jujutsu changes, as jj asks the editor for them.'),
    ('kcl', 'kcl-mode', 'KCL', 'prog-mode', 'KCL configuration.'),
    ('klog', 'klog-mode', 'Klog', 'text-mode', 'klog records of time spent.'),
    ('koka', 'koka-mode', 'Koka', 'prog-mode', 'Koka source.'),
    ('koto', 'koto-mode', 'Koto', 'prog-mode', 'Koto scripts.'),
    ('lpf', 'lpf-mode', 'LPF', 'prog-mode', 'The constraints of Lattice FPGAs (LPF).'),
    ('mail', 'eml-mode', 'Mail', 'text-mode', 'Email messages saved as files (.eml): their headers and body.'),
    ('markdoc', 'markdoc-mode', 'Markdoc', 'text-mode', 'Markdoc documents.'),
    ('mojo', 'mojo-mode', 'Mojo', 'prog-mode', 'Mojo source.'),
    ('move', 'move-mode', 'Move', 'prog-mode', 'Move smart contracts.'),
    ('nearley', 'nearley-mode', 'Nearley', 'prog-mode', 'Nearley grammars.'),
    ('ohm', 'ohm-mode', 'Ohm', 'prog-mode', 'Ohm grammars.'),
    ('penrose', 'penrose-mode', 'Penrose', 'prog-mode', 'Penrose diagrams: their substance, style and domain.'),
    ('pest', 'pest-mode', 'Pest', 'prog-mode', 'Pest grammars, for Rust.'),
    ('picat', 'picat-mode', 'Picat', 'prog-mode', 'Picat source.'),
    ('pod', 'pod-mode', 'POD', 'text-mode', "Perl's documentation (POD)."),
    ('ponylang', 'ponylang-mode', 'Pony', 'prog-mode', 'Pony source.'),
    ('proverif', 'proverif-mode', 'ProVerif', 'prog-mode', 'ProVerif models of protocols.'),
    ('prql', 'prql-mode', 'PRQL', 'prog-mode', 'PRQL queries.'),
    ('ptx', 'ptx-mode', 'PTX', 'prog-mode', "NVIDIA's PTX assembly."),
    ('qml', 'qml-mode', 'QML', 'prog-mode', "QML: Qt's markup for user interfaces."),
    ('quarto', 'quarto-mode', 'Quarto', 'text-mode', 'Quarto documents: Markdown with code to run.'),
    ('quint', 'quint-mode', 'Quint', 'prog-mode', 'Quint specifications.'),
    ('rescript', 'rescript-mode', 'ReScript', 'prog-mode', 'ReScript source.'),
    ('ripple', 'ripple-mode', 'Ripple', 'prog-mode', 'Ripple components: a TypeScript framework for user interfaces.'),
    ('rmarkdown', 'poly-markdown+r-mode', 'PM-Rmd', 'text-mode', 'R Markdown documents: Markdown with R code.'),
    ('robot', 'robot-mode', 'Robot', 'prog-mode', 'Robot Framework tests.'),
    ('sage', 'sage-mode', 'Sage', 'prog-mode', 'SageMath source.'),
    ('scfg', 'scfg-mode', 'scfg', 'prog-mode', 'scfg configuration files (kanshi and others).'),
    ('shellcheckrc', 'shellcheckrc-mode', 'Shellcheckrc', 'prog-mode', "ShellCheck's configuration."),
    ('slang', 'slang-mode', 'Slang', 'prog-mode', 'Slang shaders.'),
    ('slint', 'slint-mode', 'Slint', 'prog-mode', 'Slint: a markup for user interfaces.'),
    ('slisp', 'slisp-mode', 'SLisp', 'prog-mode', 'SlightLisp source. Only read and coloured here: nothing of it is evaluated.'),
    ('smithy', 'smithy-mode', 'Smithy', 'prog-mode', 'Smithy interface definitions.'),
    ('snakemake', 'snakemake-mode', 'Snakemake', 'prog-mode', 'Snakemake workflows.'),
    ('sourcepawn', 'sourcepawn-mode', 'SourcePawn', 'prog-mode', 'SourcePawn scripts.'),
    ('spade', 'spade-mode', 'Spade', 'prog-mode', 'Spade: a language that describes hardware.'),
    ('spicedb', 'spicedb-mode', 'SpiceDB', 'prog-mode', 'SpiceDB schemas.'),
    ('strictdoc', 'strictdoc-mode', 'StrictDoc', 'text-mode', 'StrictDoc documents of requirements.'),
    ('styx', 'styx-mode', 'Styx', 'prog-mode', 'Styx configuration files.'),
    ('supercollider', 'sclang-mode', 'SCLang', 'prog-mode', 'SuperCollider source.'),
    ('sway', 'sway-mode', 'Sway', 'prog-mode', 'Sway smart contracts, for Fuel.'),
    ('systemverilog', 'systemverilog-mode', 'SystemVerilog', 'prog-mode', 'SystemVerilog descriptions of hardware.'),
    ('t32', 't32-mode', 'T32', 'prog-mode', 'TRACE32 PRACTICE scripts.'),
    ('tablegen', 'tablegen-mode', 'TableGen', 'prog-mode', "LLVM's TableGen records."),
    ('tact', 'tact-mode', 'Tact', 'prog-mode', 'Tact smart contracts, for TON.'),
    ('task', 'task-mode', 'Task', 'prog-mode', 'Taskwarrior tasks, as task edit writes them out.'),
    ('tera', 'tera-mode', 'Tera', 'prog-mode', 'Tera templates.'),
    ('textproto', 'textproto-mode', 'Textproto', 'prog-mode', 'Protocol Buffers in their text format.'),
    ('tlaplus', 'tla-mode', 'TLA+', 'prog-mode', 'TLA+ specifications.'),
    ('tolk', 'tolk-mode', 'Tolk', 'prog-mode', 'Tolk smart contracts, for TON.'),
    ('tql', 'tql-mode', 'TQL', 'prog-mode', 'TQL: the Tenzir Query Language.'),
    ('typespec', 'typespec-mode', 'TypeSpec', 'prog-mode', 'TypeSpec descriptions of APIs.'),
    ('ungrammar', 'ungrammar-mode', 'Ungrammar', 'prog-mode', 'Ungrammar: descriptions of concrete syntax trees.'),
    ('unison', 'unison-mode', 'Unison', 'prog-mode', 'Unison source.'),
    ('uxntal', 'uxntal-mode', 'Uxntal', 'prog-mode', 'Uxntal: the assembly of the Uxn virtual machine.'),
    ('vento', 'vento-mode', 'Vento', 'prog-mode', 'Vento templates.'),
    ('vhs', 'vhs-mode', 'VHS', 'prog-mode', 'VHS tapes: terminal recordings written as scripts.'),
    ('wast', 'wast-mode', 'WAST', 'prog-mode', 'WebAssembly test scripts.'),
    ('werk', 'werk-mode', 'Werk', 'prog-mode', 'Werk build files.'),
    ('wesl', 'wesl-mode', 'WESL', 'prog-mode', 'WESL shaders: WGSL extended.'),
    ('wit', 'wit-mode', 'WIT', 'prog-mode', 'WIT: the interfaces of WebAssembly components.'),
    ('wren', 'wren-mode', 'Wren', 'prog-mode', 'Wren scripts.'),
    ('xit', 'xit-mode', '[x]it!', 'text-mode', '[x]it! lists of tasks.'),
    ('xtc', 'xtc-mode', 'XTC', 'prog-mode', 'Xena traffic configurations (XTC, XPC, XOA).'),
    ('yuck', 'yuck-mode', 'Yuck', 'prog-mode', 'Yuck: the widgets of eww.'),
]

# modes Tuatara defines itself: only the language is added to them
EXISTING_MODES = {'git-commit-mode'}

# file types taken from or given to a language, where Helix and Emacs differ:
# .S is preprocessed GNU assembly (Emacs's asm-mode), .inc no one language's,
# .properties Java's properties (Emacs's conf-javaprop-mode)
# and bitbake's conf/*.conf, which would take any conf directory's files
OVERRIDES = {'pascal': {'drop': ['inc']}, 'php': {'drop': ['inc']}, 'sourcepawn': {'drop': ['inc']}, 'ini': {'drop': ['properties']}, 'nasm': {'drop': ['S']}, 'gas': {'add': ['S']},
             'bitbake': {'drop': [{'glob': 'conf/*.conf'}, {'glob': 'conf/*/*.{inc,conf}'}]}}

TABLE_NAMES = {row[0] for row in TABLE}

# the files the modes and languages are written to, in the order tuatara.asd
# loads them, each with what it holds and the languages in it; a language no
# other file names goes in the one without names. A mode another is made from
# stays in the same file, before it.
FAMILIES = [
    ('data', 'data, configuration and system files', {
        'toml', 'json', 'jsonc', 'json5', 'yaml', 'docker-compose', 'github-action', 'gitlab-ci', 'woodpecker-ci',
        'nestedtext', 'ini', 'git-config', 'git-ignore', 'git-attributes', 'protobuf', 'graphql', 'hcl', 'nginx',
        'systemd', 'env', 'ssh_client_config', 'log', 'beancount', 'thrift', 'capnp', 'prisma', 'kdl', 'hocon', 'cue',
        'dhall', 'nickel', 'pkl', 'jsonnet', 'ron', 'properties', 'hosts', 'passwd', 'debian', 'po', 'ledger',
        'todotxt', 'pem', 'ldif', 'strace', 'robots.txt', 'pip-requirements', 'hyprlang', 'caddyfile', 'adl',
        'concerto', 'cpon', 'dbml', 'doxyfile', 'dunstrc', 'esdl', 'fga', 'fidl', 'flatbuffers', 'ghostty',
        'godot-resource', 'kcl', 'klog', 'lpf', 'scfg', 'shellcheckrc', 'smithy', 'spicedb', 'styx', 'task',
        'textproto', 'typespec', 'xit', 'xtc'}),
    ('markup', 'markup, documents, style sheets and templates', {
        'xml', 'markdown', 'markdown.inline', 'html', 'css', 'scss', 'latex', 'bibtex', 'vue', 'svelte', 'dot',
        'comment', 'jsdoc', 'astro', 'rst', 'typst', 'djot', 'mermaid', 'gemini', 'wikitext', 'jinja', 'twig', 'less',
        'pug', 'heex', 'templ', 'markdown-rustdoc', 'htmldjango', 'edoc', 'blade', 'blueprint', 'dtd', 'eex', 'ejs',
        'erb', 'gherkin', 'glimmer', 'gotmpl', 'mail', 'markdoc', 'penrose', 'pod', 'quarto', 'rmarkdown',
        'strictdoc', 'tera', 'vento'}),
    ('programming', 'programming languages', None),
    ('build', 'shells, build files and version control', {
        'gomod', 'make', 'cmake', 'nix', 'diff', 'git-rebase', 'fish', 'awk', 'just', 'powershell', 'meson', 'nu',
        'elvish', 'dune', 'devicetree', 'kconfig', 'ld', 'bitbake', 'starlark', 'rpmspec', 'git-commit', 'amber',
        'cylc', 'earthfile', 'gn', 'gowork', 'gpr', 'jjdescription', 'snakemake', 'vhs', 'werk'}),
]

def family(name):
    """The file of FAMILIES that language NAME is written to."""
    return next((file for file, _, names in FAMILIES if names and name in names),
                next(file for file, _, names in FAMILIES if names is None))

KINDS = [('highlights', 'rules', 'The colouring rules'), ('indents', 'indents', 'The indentation rules'),
         ('textobjects', 'textobjects', 'The selection rules'),
         ('injections', 'injections', 'The rules for text in other languages in'), ('locals', 'locals', 'The rules for local names in'),
         ('tags', 'tags', 'The rules naming the definitions and headings in')]

def notice(what, name, kind, changed=False):
    """The license notice at the top of NAME's KIND query file taken from Helix, saying WHAT it holds; CHANGED, that a comment says what was changed."""
    return ('; This Source Code Form is subject to the terms of the Mozilla Public\n'
            '; License, v. 2.0. If a copy of the MPL was not distributed with this\n'
            '; file, You can obtain one at https://mozilla.org/MPL/2.0/.\n;\n'
            f'; {what}{"" if what.endswith(" in") else " for"} {name}: runtime/queries/{name}/{kind}.scm of\n'
            f'; Helix (https://github.com/helix-editor/helix) at commit {HELIX_COMMIT},\n'
            + ('; unchanged below this notice but where a comment says what was changed.\n\n' if changed
               else '; unchanged below this notice.\n\n'))

# Rust's rule for a test, copied by the languages like it: without anchors a
# #[test] took every later function in the same scope for a test, and the
# query read the whole file wherever it looked (1 s for a window of 128 KB)
RUST_TEST = ('''(; #[test]
 (attribute_item
   (attribute
     (identifier) @_test_attribute))
 ; allow other attributes like #[should_panic] and comments
 [
   (attribute_item)
   (line_comment)
 ]*
 ; the test function
 (function_item
   body: (_) @test.inside) @test.around
 (#eq? @_test_attribute "test"))''', '''(_ ; #[test]
 (attribute_item
   (attribute
     (identifier) @_test_attribute))
 .
 ; allow other attributes like #[should_panic] and comments
 [
   (attribute_item)
   (line_comment)
 ]*
 .
 ; the test function
 (function_item
   body: (_) @test.inside) @test.around
 (#eq? @_test_attribute "test"))''', 'the attributes, comments and function of a test next to each other: without, a #[test] took every later function for a test, and the rules read the whole file')

# patterns the tree-sitter library Tuatara is built with rejects, or that go
# wrong in it, as (old, new, why)
QUERY_FIXES = {
    ('solidity', 'highlights'): [
        ('(struct_expression type: ((expression (identifier)) @type .))',
         '(struct_expression type: (expression (identifier)) @type)',
         'tree-sitter 0.26 takes no group as the value of a field'),
    ],
    ('markdown', 'tags'): [
        ('(atx_heading (inline) @name) @definition.section',
         '(section . (atx_heading (inline) @name)) @definition.section\n(setext_heading (paragraph) @name) @definition.section',
         'the section a heading opens, so that a heading under another is outlined under it, and underlined (setext) headings too'),
    ],
    ('latex', 'highlights'): [
        ('((word) @punctuation.delimiter\n  (#eq? @punctuation.delimiter "&"))',
         '((word) @punctuation.special.separator\n  (#eq? @punctuation.special.separator "&"))',
         'the ampersand between the columns of a table has a meaning of its own, and themes leave delimiters plain; the sparse style colours it too'),
    ],
    ('nix', 'highlights'): [
        ('(doc_comment) @comment.documentation', '',
         'the grammar of nix-community, kept up, has no node of its own for a comment of documentation: such a comment is a comment'),
    ],
    ('sway', 'textobjects'): [RUST_TEST],
    ('cairo', 'textobjects'): [RUST_TEST],
}

# grammars whose repositories are archived with no source kept up in their
# place: left out; a language of them keeps its mode, its files opened in it
# with no colours or structure
DROPPED = {'rust-format-args', 'djot', 'cpon', 'move'}

# rules taken from the grammar's own repository, which Helix's no longer fit
# once its pin moved off a commit on no branch or tag: kept as they are
OWN_QUERIES = {
    'mojo': {'highlights', 'textobjects', 'locals'},
    'lean': {'highlights', 'injections'},
    'git-attributes': {'highlights'},
}

def copy_queries(name, done):
    """Copy NAME's query files, and those they inherit, into syntax/; return the kinds copied."""
    if name in done: return done[name]
    done[name] = set()
    qdir = os.path.join(HELIX, 'runtime', 'queries', name)
    for kind, directory, what in KINDS:
        src = os.path.join(qdir, kind + '.scm')
        if not os.path.exists(src): continue
        if kind in OWN_QUERIES.get(name, ()):
            done[name].add(kind); continue
        text = open(src, encoding='utf-8').read()
        fixes = QUERY_FIXES.get((name, kind), [])
        for bad, good, why in fixes:
            assert bad in text, (name, kind, bad)
            if '\n' in bad:
                old = '\n'.join(';   ' + line for line in bad.splitlines())
                text = text.replace(bad, f'; changed: {why}; it read\n{old}\n{good}')
            else:
                text = text.replace(bad, f'; changed: {why}; it read {bad}\n{good}')
        for m in re.finditer(r';+\s*inherits\s*:?\s*([a-z_,()-]+)\s*', text):
            for other in m.group(1).split(','):
                copy_queries(other, done)
        out = os.path.join(ROOT, 'syntax', directory, name + '.scm')
        # a language Tuatara had before keeps its own copy (checked against its grammar then)
        if os.path.exists(out) and name not in TABLE_NAMES:
            done[name].add(kind); continue
        with open(out, 'w', encoding='utf-8') as f:
            f.write(notice(what, name, kind, bool(fixes)) + text)
        done[name].add(kind)
    return done[name]

def existing_claims():
    """The extensions and file names Tuatara's own modes already claim."""
    exts, names = set(), set()
    for dirpath, _, files in os.walk(os.path.join(ROOT, 'lisp')):
        for fn in files:
            if not fn.endswith('.lisp') or fn.startswith('more-languages-'): continue
            text = open(os.path.join(dirpath, fn), encoding='utf-8').read()
            for m in re.finditer(r':extensions \(([^)]*)\)', text): exts.update(re.findall(r'"([^"]+)"', m.group(1)))
            for m in re.finditer(r':file-names \(([^)]*)\)', text): names.update(re.findall(r'"([^"]+)"', m.group(1)))
    return exts, names

def glob_regex(glob):
    """A regular expression for a path ending as the glob GLOB says."""
    out = ''
    i = 0
    while i < len(glob):
        c = glob[i]
        if glob.startswith('**/', i): out += '(?:.*/)?'; i += 3; continue
        if glob.startswith('**', i): out += '.*'; i += 2; continue
        if c == '*': out += '[^/]*'
        elif c == '?': out += '[^/]'
        elif c in '.+()^$|\\}': out += '\\' + c
        elif c == '[':
            j = glob.index(']', i); out += glob[i:j + 1]; i = j + 1; continue
        elif c == '{':
            j = glob.index('}', i)
            out += '(?:' + '|'.join(re.escape(a) for a in glob[i + 1:j].split(',')) + ')'; i = j + 1; continue
        else: out += c
        i += 1
    return ('' if glob.startswith('/') else '/') + out + '$'

def injection_names(regex):
    """Helix's injection-regex without its anchors, and without a group around all of it."""
    if not regex: return None
    if regex.startswith('^'): regex = regex[1:]
    if regex.endswith('$') and not regex.endswith('\\$'): regex = regex[:-1]
    if regex.startswith('(') and regex.endswith(')'):
        depth = 0
        for i, c in enumerate(regex):
            depth += {'(': 1, ')': -1}.get(c, 0)
            if depth == 0 and i < len(regex) - 1: break
        else:
            regex = regex[1:-1]
    return regex

def main():
    """Write the grammar pins, query files and mode files for the languages in TABLE."""
    claimed_exts, claimed_names = existing_claims()
    grammars = open(os.path.join(ROOT, 'syntax/grammars.tsv'), encoding='utf-8').read()
    pinned = {line.split('\t')[0] for line in grammars.splitlines() if line and not line.startswith('#')}
    rows, done = [], {}
    forms = {file: [] for file, _, _ in FAMILIES}
    for name, mode, lighter, parent, doc in TABLE:
        l = LANGS[name]
        grammar = l['grammar']
        dropped = name in DROPPED
        if grammar not in pinned and not dropped:
            pinned.add(grammar)
            rows.append('\t'.join([grammar, l['git'], l['rev'], 'unknown'] + ([l['subpath']] if l['subpath'] else [])))
        kinds = set() if dropped else copy_queries(name, done)
        exts, names, patterns = [], [], []
        override = OVERRIDES.get(name, {})
        types = [t for t in l['file_types'] if t not in override.get('drop', [])] + override.get('add', [])
        if mode in EXISTING_MODES: types = []
        for t in types:
            # a glob naming a few files by braces, {t,j}sconfig.json: each of them
            if isinstance(t, dict) and re.fullmatch(r'[^*?\[/{}]*\{[^}]*\}[^*?\[/{}]*', t['glob']):
                head, rest = t['glob'].split('{', 1)
                inside, tail = rest.split('}', 1)
                for file_name in (head + a + tail for a in inside.split(',')):
                    if file_name not in claimed_names: names.append(file_name); claimed_names.add(file_name)
                continue
            # an extension with a dot in it, js.map: a file is matched by its whole name
            if isinstance(t, str) and '.' in t:
                patterns.append(glob_regex('*.' + t))
                continue
            if isinstance(t, str):
                if t not in claimed_exts: exts.append(t); claimed_exts.add(t)
                # Kconfig, Caddyfile: a file of that name too, as Emacs has it
                if len(t) > 3 and t[0].isupper() and any(c.islower() for c in t) and t not in claimed_names:
                    names.append(t); claimed_names.add(t)
            else:
                g = t['glob']
                if not re.search(r'[*?\[/]', g):
                    if g not in claimed_names: names.append(g); claimed_names.add(g)
                elif re.fullmatch(r'\*\.[A-Za-z0-9_+-]+', g):
                    e = g[2:]
                    if e not in claimed_exts: exts.append(e); claimed_exts.add(e)
                else:
                    patterns.append(glob_regex(g))
        injection = injection_names(l['injection_regex'])
        unit = l['indent_unit']
        indent = (l['tab_width'] or 4) if unit == '\t' else (len(unit) if unit else 4)
        lang_symbol = name.replace('.', '-')
        if mode and mode not in EXISTING_MODES:
            opts = [f':lighter {lisp_string(lighter)}']
            block = l['block_comment'][0] if isinstance(l['block_comment'], list) else l['block_comment']
            # a comment token that is a block comment's two ends, OCaml's (**), comments no line
            if l['comment'] and not (isinstance(block, dict) and l['comment'] == block['start'] + block['end']):
                # a line is commented as Emacs comments one: ;; in the Lisps
                put = ';;' if l['comment'] == ';' else l['comment']
                opts.append(f':comment {lisp_string(put)}' if put == l['comment']
                            else f':comment ({lisp_string(l["comment"])} {lisp_string(put)})')
            elif block:
                # only comments with an end: each line commented between them
                opts.append(f':block-comment ({lisp_string(block["start"])} {lisp_string(block["end"])})')
            if exts: opts.append(f':extensions {lisp_list(exts)}')
            if names: opts.append(f':file-names {lisp_list(names)}')
            if patterns: opts.append(f':patterns {lisp_list(patterns)}')
            # the program a #! line names, its first word: v of v run
            interpreters = list(dict.fromkeys(i.split()[0] for i in l['shebangs'] if i.split()))
            if interpreters: opts.append(f':interpreters {lisp_list(interpreters)}')
            forms[family(name)].append(f'(define-major-mode {mode} {parent}\n  {lisp_string(doc)}\n  ' + '\n  '.join(opts) + ')')
        if dropped: continue
        lang_opts = [f':grammar {lisp_string(grammar)}', f':modes ({mode or ""})'.replace('()', 'nil'), f':indent {indent}',
                     f':queries {lisp_string(name)}']
        if injection: lang_opts.append(f':injection-names {lisp_string(injection)}')
        if 'textobjects' in kinds: lang_opts.append(':definitions :textobjects')
        pretty = lighter or name
        forms[family(name)].append(f'(define-language {lang_symbol}\n  {lisp_string(re.split(r"[.](?:\s|$)", doc)[0] + ", parsed with tree-sitter-" + grammar.replace("_", "-") + ".")}\n  '
                     + ' '.join(lang_opts) + ')')
    for file, holds, _ in FAMILIES:
        header = (f';;;; lisp/modes/more-languages-{file}.lisp --- The {holds} Helix knows, in major modes named as Emacs\'s.\n'
                  ';;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.\n;;;;\n'
                  ';;;; Each language is parsed with the grammar and coloured, indented and\n'
                  ';;;; selected by the rules Helix uses for it (syntax/), in a major mode named as\n'
                  ';;;; the one Emacs or its best-known package edits it with. A file type another\n'
                  ';;;; mode already claims stays that mode\'s.\n;;;;\n'
                  ';;;; Written by tools/languages/generate.py: change that, not this file.\n\n'
                  '(in-package #:tuatara.defaults)\n\n')
        with open(os.path.join(ROOT, f'lisp/modes/more-languages-{file}.lisp'), 'w', encoding='utf-8') as f:
            f.write(header + '\n\n'.join(forms[file]) + '\n')
    if rows:
        header = '# languages Helix knows (lisp/modes/more-languages-*.lisp), pinned where Helix pins them\n'
        with open(os.path.join(ROOT, 'syntax/grammars.tsv'), 'a', encoding='utf-8') as f:
            f.write(('' if header in grammars else header) + '\n'.join(rows) + '\n')
    print(f'{len(TABLE)} languages, {len(rows)} grammars pinned, {sum(len(v) for v in done.values())} query files')

main()
