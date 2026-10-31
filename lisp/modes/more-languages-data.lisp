;;;; lisp/modes/more-languages-data.lisp --- The data, configuration and system files Helix knows, in major modes named as Emacs's.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each language is parsed with the grammar and coloured, indented and
;;;; selected by the rules Helix uses for it (syntax/), in a major mode named as
;;;; the one Emacs or its best-known package edits it with. A file type another
;;;; mode already claims stays that mode's.
;;;;
;;;; Written by tools/languages/generate.py: change that, not this file.

(in-package #:tuatara.defaults)

(define-major-mode toml-mode prog-mode
  "TOML configuration files."
  :lighter "TOML"
  :comment "#"
  :extensions ("toml")
  :file-names ("pdm.lock" "poetry.lock" "Cargo.lock" "uv.lock" "containers.conf" "mounts.conf" "policy.conf" "registries.conf" "storage.conf")
  :patterns ("/containers\\.conf\\.d/[^/]*\\.conf$" "/containers\\.conf\\.modules/[^/]*\\.conf$"))

(define-language toml
  "TOML configuration files, parsed with tree-sitter-toml."
  :grammar "toml" :modes (toml-mode) :indent 2 :queries "toml" :injection-names "toml" :definitions :textobjects)

(define-major-mode json-mode prog-mode
  "JSON data."
  :lighter "JSON"
  :extensions ("json" "arb" "ipynb" "geojson" "gltf" "webmanifest" "avsc" "ldtk" "ldtkl" "sublime-build" "sublime-color-scheme" "sublime-commands" "sublime-completions" "sublime-keymap" "sublime-macro" "sublime-menu" "sublime-mousemap" "sublime-project" "sublime-settings" "sublime-theme" "sublime-workspace" "code-workspace")
  :file-names ("flake.lock" ".babelrc" ".bowerrc" ".jscrc" ".jslintrc" ".vuerc" "composer.lock" ".watchmanconfig" ".swift-format" "devenv.lock" "devbox.lock" "manifest.lock" "deno.lock")
  :patterns ("/[^/]*\\.js\\.map$" "/[^/]*\\.ts\\.map$" "/[^/]*\\.css\\.map$" "/\\.pytest_cache/v/cache/[^/]*$"))

(define-language json
  "JSON data, parsed with tree-sitter-json."
  :grammar "json" :modes (json-mode) :indent 2 :queries "json" :injection-names "json" :definitions :textobjects)

(define-major-mode jsonc-mode prog-mode
  "JSON with comments."
  :lighter "JSONC"
  :comment "//"
  :extensions ("jsonc")
  :file-names ("tsconfig.json" "jsconfig.json" "bun.lock" ".devcontainer.json" "devcontainer.json")
  :patterns ("/\\.vscode/[^/]*\\.json$"))

(define-language jsonc
  "JSON with comments, parsed with tree-sitter-json."
  :grammar "json" :modes (jsonc-mode) :indent 2 :queries "jsonc" :injection-names "jsonc" :definitions :textobjects)

(define-major-mode json5-mode prog-mode
  "JSON5 data: JSON with comments, unquoted names and more."
  :lighter "JSON5"
  :comment "//"
  :extensions ("json5"))

(define-language json5
  "JSON5 data: JSON with comments, unquoted names and more, parsed with tree-sitter-json5."
  :grammar "json5" :modes (json5-mode) :indent 4 :queries "json5" :injection-names "json5" :definitions :textobjects)

(define-major-mode yaml-mode prog-mode
  "YAML data and configuration."
  :lighter "YAML"
  :comment "#"
  :extensions ("yml" "yaml" "sublime-syntax" "bu")
  :file-names (".prettierrc" ".clangd" ".clang-format" ".clang-tidy" "yarn.lock" ".stylelintrc" ".puppeteerrc")
  :patterns ("/\\.gem/credentials$" "/\\.kube/config$" "/\\.kube/kuberc$"))

(define-language yaml
  "YAML data and configuration, parsed with tree-sitter-yaml."
  :grammar "yaml" :modes (yaml-mode) :indent 2 :queries "yaml" :injection-names "yml|yaml" :definitions :textobjects)

(define-major-mode docker-compose-mode yaml-mode
  "Docker Compose files: the services of an application."
  :lighter "docker-compose"
  :comment "#"
  :file-names ("docker-compose.yaml" "docker-compose.yml" "compose.yaml" "compose.yml"))

(define-language docker-compose
  "Docker Compose files: the services of an application, parsed with tree-sitter-yaml."
  :grammar "yaml" :modes (docker-compose-mode) :indent 2 :queries "docker-compose" :definitions :textobjects)

(define-major-mode github-action-mode yaml-mode
  "GitHub Actions workflows."
  :lighter "GitHub Action"
  :comment "#"
  :patterns ("/\\.github/workflows/[^/]*\\.yaml$" "/\\.github/workflows/[^/]*\\.yml$"))

(define-language github-action
  "GitHub Actions workflows, parsed with tree-sitter-yaml."
  :grammar "yaml" :modes (github-action-mode) :indent 2 :queries "github-action" :definitions :textobjects)

(define-major-mode gitlab-ci-mode yaml-mode
  "GitLab CI's pipelines (.gitlab-ci.yml)."
  :lighter "GitLab CI"
  :comment "#"
  :file-names (".gitlab-ci.yml"))

(define-language gitlab-ci
  "GitLab CI's pipelines (.gitlab-ci.yml), parsed with tree-sitter-yaml."
  :grammar "yaml" :modes (gitlab-ci-mode) :indent 2 :queries "gitlab-ci" :injection-names "gitlab-ci" :definitions :textobjects)

(define-major-mode woodpecker-ci-mode yaml-mode
  "Woodpecker CI's pipelines."
  :lighter "Woodpecker CI"
  :comment "#"
  :file-names (".woodpecker.yaml")
  :patterns ("/\\.woodpecker/[^/]*\\.yaml$" "/\\.woodpecker/[^/]*\\.yml$"))

(define-language woodpecker-ci
  "Woodpecker CI's pipelines, parsed with tree-sitter-yaml."
  :grammar "yaml" :modes (woodpecker-ci-mode) :indent 2 :queries "woodpecker-ci" :injection-names "woodpecker-ci" :definitions :textobjects)

(define-major-mode nestedtext-mode prog-mode
  "NestedText data."
  :lighter "NestedText"
  :comment "#"
  :extensions ("nt"))

(define-language nestedtext
  "NestedText data, parsed with tree-sitter-yaml."
  :grammar "yaml" :modes (nestedtext-mode) :indent 4 :queries "nestedtext" :injection-names "nestedtext" :definitions :textobjects)

(define-major-mode ini-mode prog-mode
  "INI configuration files."
  :lighter "INI"
  :comment "#"
  :extensions ("ini" "desktop" "cfg" "directory")
  :file-names ("mimeapps.list" ".editorconfig" ".npmrc" "hgrc" "npmrc" "rclone.conf" ".wslconfig" ".buckconfig" ".buckconfig.local")
  :patterns ("/\\.aws/config$" "/\\.buckconfig\\.d/[^/]*$" "/etc/buckconfig$" "/etc/buckconfig\\.d/[^/]*$"))

(define-language ini
  "INI configuration files, parsed with tree-sitter-ini."
  :grammar "ini" :modes (ini-mode) :indent 4 :queries "ini" :injection-names "ini")

(define-major-mode gitconfig-mode prog-mode
  "Git's configuration files."
  :lighter "Gitconfig"
  :comment "#"
  :extensions ("gitconfig")
  :file-names (".gitmodules" "gitconfig" ".gitconfig" "config.worktree")
  :patterns ("/\\.git/config$" "/\\.config/git/config$" "/\\.git/modules/[^/]*/config$"))

(define-language git-config
  "Git's configuration files, parsed with tree-sitter-git-config."
  :grammar "git-config" :modes (gitconfig-mode) :indent 4 :queries "git-config" :injection-names "git-config" :definitions :textobjects)

(define-major-mode gitignore-mode prog-mode
  "Git's lists of files to ignore."
  :lighter "Gitignore"
  :comment "#"
  :file-names (".gitignore_global" ".ignore" "CODEOWNERS" ".git-blame-ignore-revs")
  :patterns ("/git/ignore$" "/\\.git/info/exclude$" "/\\.config/helix/ignore$" "/\\.helix/ignore$" "/\\.[^/]*ignore$"))

(define-language git-ignore
  "Git's lists of files to ignore, parsed with tree-sitter-gitignore."
  :grammar "gitignore" :modes (gitignore-mode) :indent 4 :queries "git-ignore" :injection-names "git-ignore")

(define-major-mode gitattributes-mode prog-mode
  "Git's attributes of files."
  :lighter "Gitattributes"
  :comment "#"
  :file-names (".gitattributes")
  :patterns ("/\\.config/git/attributes$"))

(define-language git-attributes
  "Git's attributes of files, parsed with tree-sitter-gitattributes."
  :grammar "gitattributes" :modes (gitattributes-mode) :indent 4 :queries "git-attributes" :injection-names "git-attributes")

(define-major-mode protobuf-mode prog-mode
  "Protocol Buffers definitions."
  :lighter "Protobuf"
  :comment "//"
  :extensions ("proto"))

(define-language protobuf
  "Protocol Buffers definitions, parsed with tree-sitter-proto."
  :grammar "proto" :modes (protobuf-mode) :indent 2 :queries "protobuf" :injection-names "proto" :definitions :textobjects)

(define-major-mode graphql-mode prog-mode
  "GraphQL schemas and queries."
  :lighter "GraphQL"
  :comment "#"
  :extensions ("gql" "graphql" "graphqls"))

(define-language graphql
  "GraphQL schemas and queries, parsed with tree-sitter-graphql."
  :grammar "graphql" :modes (graphql-mode) :indent 2 :queries "graphql" :injection-names "graphql" :definitions :textobjects)

(define-major-mode hcl-mode prog-mode
  "HashiCorp configuration: Terraform, Nomad and the rest."
  :lighter "HCL"
  :comment "#"
  :extensions ("hcl" "tf" "nomad"))

(define-language hcl
  "HashiCorp configuration: Terraform, Nomad and the rest, parsed with tree-sitter-hcl."
  :grammar "hcl" :modes (hcl-mode) :indent 2 :queries "hcl" :injection-names "hcl|tf|nomad" :definitions :textobjects)

(define-major-mode nginx-mode prog-mode
  "Nginx configuration files."
  :lighter "Nginx"
  :comment "#"
  :file-names ("nginx.conf")
  :patterns ("/sites-available/[^/]*\\.conf$" "/sites-enabled/[^/]*\\.conf$" "/conf\\.d/[^/]*\\.conf$"))

(define-language nginx
  "Nginx configuration files, parsed with tree-sitter-nginx."
  :grammar "nginx" :modes (nginx-mode) :indent 4 :queries "nginx" :injection-names "nginx")

(define-major-mode systemd-mode prog-mode
  "systemd unit files."
  :lighter "Systemd"
  :comment "#"
  :extensions ("service" "automount" "device" "mount" "nspawn" "path" "scope" "slice" "socket" "swap" "target" "timer" "container" "kube" "network" "volume" "build" "image")
  :patterns ("/systemd/(?:.*/)?[^/]*\\.conf$" "/containers/systemd/(?:.*/)?[^/]*\\.pod$"))

(define-language systemd
  "systemd unit files, parsed with tree-sitter-ini."
  :grammar "ini" :modes (systemd-mode) :indent 4 :queries "systemd" :injection-names "systemd")

(define-major-mode dotenv-mode prog-mode
  "Environment files (.env)."
  :lighter "Dotenv"
  :comment "#"
  :file-names (".env" ".envrc" "_environment" "_environment.local" "_environment.required" ".bazeliskrc" ".gitlab-ci-local-env")
  :patterns ("/\\.env\\.[^/]*$" "/\\.envrc\\.[^/]*$" "/_environment-[^/]*$"))

(define-language env
  "Environment files (.env), parsed with tree-sitter-bash."
  :grammar "bash" :modes (dotenv-mode) :indent 4 :queries "env" :injection-names "env" :definitions :textobjects)

(define-major-mode ssh-config-mode prog-mode
  "OpenSSH client configuration."
  :lighter "SSH Config"
  :comment "#"
  :patterns ("/\\.ssh/config$" "/etc/ssh/ssh_config$" "/ssh_config\\.d/[^/]*\\.conf$"))

(define-language ssh_client_config
  "OpenSSH client configuration, parsed with tree-sitter-ssh-client-config."
  :grammar "ssh_client_config" :modes (ssh-config-mode) :indent 4 :queries "ssh_client_config")

(define-major-mode log-mode prog-mode
  "Log files: their times, levels and messages told apart."
  :lighter "Log"
  :extensions ("log"))

(define-language log
  "Log files: their times, levels and messages told apart, parsed with tree-sitter-log."
  :grammar "log" :modes (log-mode) :indent 4 :queries "log")

(define-major-mode beancount-mode prog-mode
  "Beancount ledgers: double-entry accounts in plain text."
  :lighter "Beancount"
  :comment (";" ";;")
  :extensions ("beancount" "bean"))

(define-language beancount
  "Beancount ledgers: double-entry accounts in plain text, parsed with tree-sitter-beancount."
  :grammar "beancount" :modes (beancount-mode) :indent 2 :queries "beancount" :injection-names "beancount")

(define-major-mode thrift-mode prog-mode
  "Apache Thrift definitions."
  :lighter "Thrift"
  :comment "//"
  :extensions ("thrift"))

(define-language thrift
  "Apache Thrift definitions, parsed with tree-sitter-thrift."
  :grammar "thrift" :modes (thrift-mode) :indent 2 :queries "thrift")

(define-major-mode capnp-mode prog-mode
  "Cap'n Proto schemas."
  :lighter "Cap'n Proto"
  :comment "#"
  :extensions ("capnp"))

(define-language capnp
  "Cap'n Proto schemas, parsed with tree-sitter-capnp."
  :grammar "capnp" :modes (capnp-mode) :indent 2 :queries "capnp" :injection-names "capnp")

(define-major-mode prisma-mode prog-mode
  "Prisma schemas."
  :lighter "Prisma"
  :comment "//"
  :extensions ("prisma"))

(define-language prisma
  "Prisma schemas, parsed with tree-sitter-prisma."
  :grammar "prisma" :modes (prisma-mode) :indent 2 :queries "prisma" :injection-names "prisma" :definitions :textobjects)

(define-major-mode kdl-mode prog-mode
  "KDL documents."
  :lighter "KDL"
  :comment "//"
  :extensions ("kdl"))

(define-language kdl
  "KDL documents, parsed with tree-sitter-kdl."
  :grammar "kdl" :modes (kdl-mode) :indent 4 :queries "kdl" :injection-names "kdl" :definitions :textobjects)

(define-major-mode hocon-mode prog-mode
  "HOCON configuration files."
  :lighter "HOCON"
  :comment "#"
  :patterns ("/(?:.*/)?src/[^/]*/resources/(?:.*/)?[^/]*\\.conf$" "/[^/]*scalafmt[^/]*\\.conf$" "/[^/]*scalafix[^/]*\\.conf$"))

(define-language hocon
  "HOCON configuration files, parsed with tree-sitter-hocon."
  :grammar "hocon" :modes (hocon-mode) :indent 2 :queries "hocon" :definitions :textobjects)

(define-major-mode cue-mode prog-mode
  "CUE configuration and data."
  :lighter "CUE"
  :comment "//"
  :extensions ("cue"))

(define-language cue
  "CUE configuration and data, parsed with tree-sitter-cue."
  :grammar "cue" :modes (cue-mode) :indent 4 :queries "cue" :injection-names "cue")

(define-major-mode dhall-mode prog-mode
  "Dhall configuration."
  :lighter "Dhall"
  :comment "--"
  :extensions ("dhall"))

(define-language dhall
  "Dhall configuration, parsed with tree-sitter-dhall."
  :grammar "dhall" :modes (dhall-mode) :indent 2 :queries "dhall" :injection-names "dhall" :definitions :textobjects)

(define-major-mode nickel-mode prog-mode
  "Nickel configuration."
  :lighter "Nickel"
  :comment "#"
  :extensions ("ncl"))

(define-language nickel
  "Nickel configuration, parsed with tree-sitter-nickel."
  :grammar "nickel" :modes (nickel-mode) :indent 2 :queries "nickel" :injection-names "nickel")

(define-major-mode pkl-mode prog-mode
  "Pkl configuration."
  :lighter "Pkl"
  :comment "//"
  :extensions ("pkl" "pcf")
  :file-names ("PklProject"))

(define-language pkl
  "Pkl configuration, parsed with tree-sitter-pkl."
  :grammar "pkl" :modes (pkl-mode) :indent 2 :queries "pkl" :injection-names "pkl")

(define-major-mode jsonnet-mode prog-mode
  "Jsonnet: templates that make JSON."
  :lighter "Jsonnet"
  :comment "//"
  :extensions ("libsonnet" "jsonnet"))

(define-language jsonnet
  "Jsonnet: templates that make JSON, parsed with tree-sitter-jsonnet."
  :grammar "jsonnet" :modes (jsonnet-mode) :indent 2 :queries "jsonnet")

(define-major-mode ron-mode prog-mode
  "Rusty Object Notation."
  :lighter "RON"
  :comment "//"
  :extensions ("ron"))

(define-language ron
  "Rusty Object Notation, parsed with tree-sitter-ron."
  :grammar "ron" :modes (ron-mode) :indent 4 :queries "ron" :injection-names "ron")

(define-major-mode conf-javaprop-mode prog-mode
  "Java properties files."
  :lighter "Conf[JavaProp]"
  :comment "#"
  :extensions ("properties" "prefs"))

(define-language properties
  "Java properties files, parsed with tree-sitter-properties."
  :grammar "properties" :modes (conf-javaprop-mode) :indent 4 :queries "properties" :injection-names "properties" :definitions :textobjects)

(define-major-mode hosts-generic-mode prog-mode
  "The hosts file: names for addresses."
  :lighter "Hosts"
  :comment "#"
  :file-names ("hosts"))

(define-language hosts
  "The hosts file: names for addresses, parsed with tree-sitter-hosts."
  :grammar "hosts" :modes (hosts-generic-mode) :indent 4 :queries "hosts")

(define-major-mode etc-passwd-generic-mode prog-mode
  "The password file: the system's users."
  :lighter "Passwd"
  :file-names ("passwd"))

(define-language passwd
  "The password file: the system's users, parsed with tree-sitter-passwd."
  :grammar "passwd" :modes (etc-passwd-generic-mode) :indent 4 :queries "passwd")

(define-major-mode debian-control-mode prog-mode
  "The control files of Debian's packages."
  :lighter "Debian-Control"
  :comment "#"
  :extensions ("dsc" "changes")
  :patterns ("/debian/control$" "/debian/copyright$" "/debian/watch$" "/debian/changelog$" "/debian/source/format$" "/debian/source/options$" "/debian/source/local-options$" "/debian/tests/control$" "/debian/upstream/metadata$" "/debian/rules$" "/etc/apt/sources\\.list\\.d/[^/]*\\.sources$"))

(define-language debian
  "The control files of Debian's packages, parsed with tree-sitter-debian."
  :grammar "debian" :modes (debian-control-mode) :indent 4 :queries "debian")

(define-major-mode po-mode prog-mode
  "Translations in gettext PO files."
  :lighter "PO"
  :comment "#"
  :extensions ("po" "pot"))

(define-language po
  "Translations in gettext PO files, parsed with tree-sitter-po."
  :grammar "po" :modes (po-mode) :indent 4 :queries "po" :definitions :textobjects)

(define-major-mode ledger-mode text-mode
  "Ledger accounts."
  :lighter "Ledger"
  :comment (";" ";;")
  :extensions ("ldg" "ledger" "journal"))

(define-language ledger
  "Ledger accounts, parsed with tree-sitter-ledger."
  :grammar "ledger" :modes (ledger-mode) :indent 4 :queries "ledger" :injection-names "ledger")

(define-major-mode todotxt-mode text-mode
  "todo.txt lists of tasks."
  :lighter "Todo.txt"
  :extensions ("todotxt")
  :patterns ("/[^/]*\\.todo\\.txt$"))

(define-language todotxt
  "todo.txt lists of tasks, parsed with tree-sitter-todotxt."
  :grammar "todotxt" :modes (todotxt-mode) :indent 4 :queries "todotxt")

(define-major-mode pem-mode prog-mode
  "PEM files: certificates and keys in Base64."
  :lighter "PEM"
  :extensions ("pem" "cert" "crt"))

(define-language pem
  "PEM files: certificates and keys in Base64, parsed with tree-sitter-pem."
  :grammar "pem" :modes (pem-mode) :indent 4 :queries "pem" :injection-names "pem")

(define-major-mode ldif-mode prog-mode
  "LDAP directories written out as LDIF."
  :lighter "LDIF"
  :comment "#"
  :extensions ("ldif"))

(define-language ldif
  "LDAP directories written out as LDIF, parsed with tree-sitter-ldif."
  :grammar "ldif" :modes (ldif-mode) :indent 4 :queries "ldif" :injection-names "ldif")

(define-major-mode strace-mode prog-mode
  "The output of strace: system calls and what they returned."
  :lighter "Strace"
  :extensions ("strace"))

(define-language strace
  "The output of strace: system calls and what they returned, parsed with tree-sitter-strace."
  :grammar "strace" :modes (strace-mode) :indent 4 :queries "strace")

(define-major-mode robots-txt-mode prog-mode
  "The robots.txt files of web sites."
  :lighter "Robots.txt"
  :comment "#"
  :file-names ("robots.txt"))

(define-language robots-txt
  "The robots.txt files of web sites, parsed with tree-sitter-robots-txt."
  :grammar "robots-txt" :modes (robots-txt-mode) :indent 4 :queries "robots.txt" :injection-names "robots[\\.-]txt" :definitions :textobjects)

(define-major-mode pip-requirements-mode prog-mode
  "Python's requirements files, for pip."
  :lighter "pip-require"
  :file-names ("requirements.txt" "constraints.txt"))

(define-language pip-requirements
  "Python's requirements files, for pip, parsed with tree-sitter-requirements."
  :grammar "requirements" :modes (pip-requirements-mode) :indent 4 :queries "pip-requirements" :injection-names "(pip-)?requirements(\\.txt)?")

(define-major-mode hyprlang-mode prog-mode
  "Hyprland configuration."
  :lighter "Hyprlang"
  :comment "#"
  :patterns ("/hypr/[^/]*\\.conf$"))

(define-language hyprlang
  "Hyprland configuration, parsed with tree-sitter-hyprlang."
  :grammar "hyprlang" :modes (hyprlang-mode) :indent 4 :queries "hyprlang")

(define-major-mode caddyfile-mode prog-mode
  "Caddyfiles: the configuration of the Caddy web server."
  :lighter "Caddyfile"
  :comment "#"
  :extensions ("Caddyfile" "caddyfile")
  :file-names ("Caddyfile" "caddyfile")
  :patterns ("/Caddyfile\\.[^/]*$" "/caddyfile\\.[^/]*$"))

(define-language caddyfile
  "Caddyfiles: the configuration of the Caddy web server, parsed with tree-sitter-caddyfile."
  :grammar "caddyfile" :modes (caddyfile-mode) :indent 4 :queries "caddyfile" :injection-names "caddyfile" :definitions :textobjects)

(define-major-mode adl-mode prog-mode
  "ADL: data models written once for many languages."
  :lighter "ADL"
  :comment "//"
  :extensions ("adl"))

(define-language adl
  "ADL: data models written once for many languages, parsed with tree-sitter-adl."
  :grammar "adl" :modes (adl-mode) :indent 2 :queries "adl" :injection-names "adl" :definitions :textobjects)

(define-major-mode concerto-mode prog-mode
  "Concerto data models."
  :lighter "Concerto"
  :comment "//"
  :extensions ("cto"))

(define-language concerto
  "Concerto data models, parsed with tree-sitter-concerto."
  :grammar "concerto" :modes (concerto-mode) :indent 2 :queries "concerto" :injection-names "concerto" :definitions :textobjects)

(define-major-mode cpon-mode prog-mode
  "CPON: the ChainPack Object Notation of Silicon Heaven."
  :lighter "CPON"
  :comment "//"
  :extensions ("cpon" "cp"))

(define-major-mode dbml-mode prog-mode
  "DBML: database schemas written as markup."
  :lighter "DBML"
  :comment (";" ";;")
  :extensions ("dbml"))

(define-language dbml
  "DBML: database schemas written as markup, parsed with tree-sitter-dbml."
  :grammar "dbml" :modes (dbml-mode) :indent 2 :queries "dbml" :injection-names "dbml")

(define-major-mode doxyfile-mode prog-mode
  "Doxyfiles: Doxygen's configuration."
  :lighter "Doxyfile"
  :comment "#"
  :file-names ("Doxyfile"))

(define-language doxyfile
  "Doxyfiles: Doxygen's configuration, parsed with tree-sitter-doxyfile."
  :grammar "doxyfile" :modes (doxyfile-mode) :indent 4 :queries "doxyfile" :injection-names "[Dd]oxyfile" :definitions :textobjects)

(define-major-mode dunstrc-mode prog-mode
  "The configuration of the dunst notification daemon."
  :lighter "Dunstrc"
  :comment "#"
  :patterns ("/dunst/dunstrc$"))

(define-language dunstrc
  "The configuration of the dunst notification daemon, parsed with tree-sitter-dunstrc."
  :grammar "dunstrc" :modes (dunstrc-mode) :indent 4 :queries "dunstrc")

(define-major-mode esdl-mode prog-mode
  "Schemas of the Gel (EdgeDB) database."
  :lighter "ESDL"
  :comment "#"
  :extensions ("gel" "esdl"))

(define-language esdl
  "Schemas of the Gel (EdgeDB) database, parsed with tree-sitter-esdl."
  :grammar "esdl" :modes (esdl-mode) :indent 2 :queries "esdl" :injection-names "esdl")

(define-major-mode fga-mode prog-mode
  "OpenFGA models of authorisation."
  :lighter "FGA"
  :comment "#"
  :extensions ("fga")
  :file-names ("fga.mod"))

(define-language fga
  "OpenFGA models of authorisation, parsed with tree-sitter-fga."
  :grammar "fga" :modes (fga-mode) :indent 2 :queries "fga" :injection-names "fga" :definitions :textobjects)

(define-major-mode fidl-mode prog-mode
  "FIDL: Fuchsia's interface definitions."
  :lighter "FIDL"
  :comment "//"
  :extensions ("fidl"))

(define-language fidl
  "FIDL: Fuchsia's interface definitions, parsed with tree-sitter-fidl."
  :grammar "fidl" :modes (fidl-mode) :indent 4 :queries "fidl" :injection-names "fidl")

(define-major-mode flatbuffers-mode prog-mode
  "FlatBuffers schemas."
  :lighter "FlatBuffers"
  :comment "//"
  :extensions ("fbs"))

(define-language flatbuffers
  "FlatBuffers schemas, parsed with tree-sitter-flatbuffers."
  :grammar "flatbuffers" :modes (flatbuffers-mode) :indent 1 :queries "flatbuffers" :injection-names "flatbuffers?|fbs")

(define-major-mode ghostty-mode prog-mode
  "The configuration of the Ghostty terminal."
  :lighter "Ghostty"
  :comment "#"
  :extensions ("ghostty")
  :patterns ("/ghostty/config$"))

(define-language ghostty
  "The configuration of the Ghostty terminal, parsed with tree-sitter-ghostty."
  :grammar "ghostty" :modes (ghostty-mode) :indent 2 :queries "ghostty")

(define-major-mode godot-resource-mode prog-mode
  "Godot's scenes and resources."
  :lighter "Godot-Resource"
  :comment (";" ";;")
  :extensions ("tscn" "tres" "godot" "gdextension"))

(define-language godot-resource
  "Godot's scenes and resources, parsed with tree-sitter-godot-resource."
  :grammar "godot-resource" :modes (godot-resource-mode) :indent 4 :queries "godot-resource" :injection-names "godot" :definitions :textobjects)

(define-major-mode kcl-mode prog-mode
  "KCL configuration."
  :lighter "KCL"
  :comment "//"
  :extensions ("kcl"))

(define-language kcl
  "KCL configuration, parsed with tree-sitter-kcl."
  :grammar "kcl" :modes (kcl-mode) :indent 2 :queries "kcl" :injection-names "kcl")

(define-major-mode klog-mode text-mode
  "klog records of time spent."
  :lighter "Klog"
  :extensions ("klg"))

(define-language klog
  "klog records of time spent, parsed with tree-sitter-klog."
  :grammar "klog" :modes (klog-mode) :indent 4 :queries "klog" :injection-names "klg")

(define-major-mode lpf-mode prog-mode
  "The constraints of Lattice FPGAs (LPF)."
  :lighter "LPF"
  :comment "#"
  :extensions ("lpf"))

(define-language lpf
  "The constraints of Lattice FPGAs (LPF), parsed with tree-sitter-lpf."
  :grammar "lpf" :modes (lpf-mode) :indent 4 :queries "lpf")

(define-major-mode scfg-mode prog-mode
  "scfg configuration files (kanshi and others)."
  :lighter "scfg"
  :comment "#"
  :patterns ("/kanshi/config$"))

(define-language scfg
  "scfg configuration files (kanshi and others), parsed with tree-sitter-scfg."
  :grammar "scfg" :modes (scfg-mode) :indent 4 :queries "scfg")

(define-major-mode shellcheckrc-mode prog-mode
  "ShellCheck's configuration."
  :lighter "Shellcheckrc"
  :comment "#"
  :file-names ("shellcheckrc" ".shellcheckrc"))

(define-language shellcheckrc
  "ShellCheck's configuration, parsed with tree-sitter-shellcheckrc."
  :grammar "shellcheckrc" :modes (shellcheckrc-mode) :indent 4 :queries "shellcheckrc" :injection-names "shellcheck(rc)?" :definitions :textobjects)

(define-major-mode smithy-mode prog-mode
  "Smithy interface definitions."
  :lighter "Smithy"
  :comment "//"
  :extensions ("smithy"))

(define-language smithy
  "Smithy interface definitions, parsed with tree-sitter-smithy."
  :grammar "smithy" :modes (smithy-mode) :indent 4 :queries "smithy" :injection-names "smithy")

(define-major-mode spicedb-mode prog-mode
  "SpiceDB schemas."
  :lighter "SpiceDB"
  :comment "//"
  :extensions ("zed"))

(define-language spicedb
  "SpiceDB schemas, parsed with tree-sitter-spicedb."
  :grammar "spicedb" :modes (spicedb-mode) :indent 2 :queries "spicedb" :injection-names "spicedb")

(define-major-mode styx-mode prog-mode
  "Styx configuration files."
  :lighter "Styx"
  :comment "//"
  :extensions ("styx"))

(define-language styx
  "Styx configuration files, parsed with tree-sitter-styx."
  :grammar "styx" :modes (styx-mode) :indent 2 :queries "styx" :injection-names "styx" :definitions :textobjects)

(define-major-mode task-mode prog-mode
  "Taskwarrior tasks, as task edit writes them out."
  :lighter "Task"
  :comment "#"
  :extensions ("task"))

(define-language task
  "Taskwarrior tasks, as task edit writes them out, parsed with tree-sitter-task."
  :grammar "task" :modes (task-mode) :indent 2 :queries "task" :injection-names "task")

(define-major-mode textproto-mode prog-mode
  "Protocol Buffers in their text format."
  :lighter "Textproto"
  :comment "#"
  :extensions ("txtpb" "textpb" "textproto"))

(define-language textproto
  "Protocol Buffers in their text format, parsed with tree-sitter-textproto."
  :grammar "textproto" :modes (textproto-mode) :indent 2 :queries "textproto" :definitions :textobjects)

(define-major-mode typespec-mode prog-mode
  "TypeSpec descriptions of APIs."
  :lighter "TypeSpec"
  :comment "//"
  :extensions ("tsp"))

(define-language typespec
  "TypeSpec descriptions of APIs, parsed with tree-sitter-typespec."
  :grammar "typespec" :modes (typespec-mode) :indent 2 :queries "typespec" :injection-names "tsp|typespec" :definitions :textobjects)

(define-major-mode xit-mode text-mode
  "[x]it! lists of tasks."
  :lighter "[x]it!"
  :extensions ("xit"))

(define-language xit
  "[x]it! lists of tasks, parsed with tree-sitter-xit."
  :grammar "xit" :modes (xit-mode) :indent 4 :queries "xit" :injection-names "xit")

(define-major-mode xtc-mode prog-mode
  "Xena traffic configurations (XTC, XPC, XOA)."
  :lighter "XTC"
  :comment (";" ";;")
  :extensions ("xtc" "xpc" "xoa"))

(define-language xtc
  "Xena traffic configurations (XTC, XPC, XOA), parsed with tree-sitter-xtc."
  :grammar "xtc" :modes (xtc-mode) :indent 4 :queries "xtc")
