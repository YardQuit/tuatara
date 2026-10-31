# tools/grammar-build.sh --- How a tree-sitter grammar is generated, compiled and known by what it compiles: sourced by tools/grammars.sh and tools/fuzz-grammars.sh.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# The copies of the array.h of tree-sitter in many grammars read one struct
# type as another, which GCC compiles wrongly unless told not to assume that
# never happens; the assertions of a grammar are left out, as a failed one
# would end the editor; and its character classes answer for any code point
# (syntax/safe-ctype.h); and the memory its scanner takes is counted with
# tree-sitter's, so that a parse's budget sees it (syntax/counted-memory.h).
# A scanner written in C++ (the YAML and Vue grammars have one) is compiled
# apart, with the same care but for the character classes and the memory:
# the headers of C++ take those back, and such scanners ask the wide ones
# (iswspace), which answer for any code point; its names are kept inside the
# grammar, as the same ones (Scanner::scan) are in several.
grammar_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
grammar_cflags=(-O2 -fPIC -shared -std=c11 -fno-strict-aliasing -DNDEBUG -include "$grammar_root/syntax/safe-ctype.h"
                -include "$grammar_root/syntax/counted-memory.h")
grammar_cxxflags=(-O2 -fPIC -fno-strict-aliasing -DNDEBUG -fvisibility=hidden)

# compile_grammar SRC LIB [--assertions]: compile the grammar whose parser.c
# (and scanner.c or scanner.cc) are in the directory SRC into the shared
# library LIB; with --assertions, its assertions are kept, as fuzzing wants
compile_grammar() {
  local src="$1" lib="$2" cflags=() cxxflags=() flag
  for flag in "${grammar_cflags[@]}"; do
    [ "${3:-}" = --assertions ] && [ "$flag" = -DNDEBUG ] || cflags+=("$flag")
  done
  for flag in "${grammar_cxxflags[@]}"; do
    [ "${3:-}" = --assertions ] && [ "$flag" = -DNDEBUG ] || cxxflags+=("$flag")
  done
  if [ -f "$src/scanner.cc" ]; then
    cc "${cflags[@]}" -I"$src" -c "$src/parser.c" -o "$lib.parser.o"
    c++ "${cxxflags[@]}" -I"$src" -c "$src/scanner.cc" -o "$lib.scanner.o"
    c++ -shared "$lib.parser.o" "$lib.scanner.o" -o "$lib"
    rm -f "$lib.parser.o" "$lib.scanner.o"
  else
    local files=("$src/parser.c")
    [ -f "$src/scanner.c" ] && files+=("$src/scanner.c")
    cc "${cflags[@]}" -I"$src" "${files[@]}" -o "$lib"
  fi
}

# grammar_git ARGUMENTS...: git run on a grammar's checkout, which never
# runs a program the checkout names for itself (a file monitor, hooks), as
# its .git could name one
grammar_git() {
  git -c core.fsmonitor=false -c core.hooksPath=/dev/null -c core.sshCommand=false "$@"
}

# generate_parser DIR CLI: generate the parser of the grammar in the directory
# DIR again from its grammar.js with the tree-sitter program CLI, into
# DIR/src/parser.c; when that fails, say why and end the script, so that a
# grammar is never compiled from the parser it had before. grammar.js is a
# program of the grammar's: it runs shut in (bwrap), with no network, no
# environment, the system's programs to read, and a copy of DIR without any
# .git to write, of which only the parser and what goes with it are taken
# back; without bwrap, nothing is generated.
generate_parser() {
  local dir="$1" cli="$2" copy="$1.generate" file
  if ! command -v bwrap >/dev/null; then
    echo "No bwrap (bubblewrap) to run the grammar.js of $dir shut in: nothing is compiled." >&2
    exit 1
  fi
  rm -rf "$copy"; mkdir -p "$copy"
  (cd "$dir" && tar --exclude=.git --exclude='node_modules/*/.git' -cf - .) | tar -x -C "$copy"
  rm -f "$copy/src/parser.c"
  if ! (cd "$copy" && bwrap --unshare-all --die-with-parent --new-session --clearenv \
          --ro-bind /usr /usr --symlink usr/lib64 /lib64 --symlink usr/lib /lib --symlink usr/bin /bin \
          --ro-bind "$cli" "$cli" --bind "$copy" "$copy" --chdir "$copy" \
          --tmpfs /tmp --proc /proc --dev /dev --setenv HOME /tmp --setenv PATH /usr/bin \
          -- "$cli" generate --js-runtime native) > "$dir.generate.log" 2>&1 || [ ! -f "$copy/src/parser.c" ]; then
    cat "$dir.generate.log" >&2
    rm -rf "$copy"
    echo "The parser of $dir could not be generated from its grammar.js: nothing is compiled." >&2
    exit 1
  fi
  for file in parser.c grammar.json node-types.json tree_sitter/parser.h tree_sitter/alloc.h tree_sitter/array.h; do
    if [ -f "$copy/src/$file" ] && [ ! -L "$copy/src/$file" ]; then
      mkdir -p "$(dirname "$dir/src/$file")"
      cp -- "$copy/src/$file" "$dir/src/$file"
    fi
  done
  rm -rf "$copy"
  grep -v "single element" "$dir.generate.log" >&2 || true
}

# grammar_inputs REPO SRC [FILE...]: the files of the repository REPO that
# compiling the grammar in its directory SRC reads, each once, as paths from
# REPO: its parser.c, its scanner and every header they include there, as the
# compiler finds them (a header beside them, one in tree_sitter/, one a
# relative path reaches), and the FILEs given (a grammar.js its parser is
# generated from). Nothing of the grammar is run: the compiler only reads.
grammar_inputs() {
  local repo="$1" src="$2" file include=()
  shift 2
  include=(-I"$src")
  # tree-sitter itself: its lib.c, with its include/ beside src/
  [ -d "$src/../include" ] && include+=(-I"$src/../include")
  {
    for file in "$src/parser.c" "$src/scanner.c" "$src/scanner.cc" "$src/lib.c"; do
      [ -f "$file" ] || continue
      case "$file" in
        *.cc) c++ -MM "${include[@]}" "$file" ;;
        *) cc -MM -std=c11 -D_POSIX_C_SOURCE=200112L -D_DEFAULT_SOURCE "${include[@]}" "$file" ;;
      esac
    done | sed 's/^[^:]*://; s/\\$//' | tr ' ' '\n'
    for file in "$@"; do [ -f "$repo/$file" ] && echo "$repo/$file"; done
  } | while IFS= read -r file; do
    [ -n "$file" ] || continue
    file="$(realpath -- "$file")"
    case "$file" in "$(realpath -- "$repo")"/*) echo "${file#"$(realpath -- "$repo")"/}" ;; esac
  done | LC_ALL=C sort -u
}

# grammar_inputs_hash REPO SRC [FILE...]: the SHA-256 of what compiling the
# grammar in SRC reads (GRAMMAR_INPUTS): of the list of each file's path and
# SHA-256, so that a file renamed, added or changed changes it
grammar_inputs_hash() {
  local repo="$1"
  grammar_inputs "$@" | (cd "$repo" && xargs -r -d '\n' sha256sum --) | sha256sum | cut -d' ' -f1
}

# commit_of DIR OBJECT: the commit OBJECT of the repository in DIR is or
# points to (a tag names one through a tag object), or nothing when the
# repository has no such object
commit_of() { grammar_git -C "$1" rev-parse -q --verify "$2^{commit}" 2>/dev/null || true; }

# fetch_grammar DIR URL COMMIT: DIR a checkout of the repository at URL at
# COMMIT, fetched afresh unless it is one already (its HEAD that commit,
# however the pin names it: a tag object names one). Its files are written
# as the commit holds them: no line endings changed, no filter run, whatever
# its .gitattributes say
fetch_grammar() {
  local dir="$1" url="$2" commit="$3" head verbatim="* -text -ident -filter -working-tree-encoding"
  head="$(commit_of "$dir" HEAD)"
  if [ -z "$head" ] || [ "$head" != "$(commit_of "$dir" "$commit")" ] ||
     [ "$(grammar_git -C "$dir" rev-parse --show-toplevel 2>/dev/null)" != "$dir" ] ||
     [ "$(cat "$dir/.git/info/attributes" 2>/dev/null)" != "$verbatim" ]; then
    rm -rf "$dir"; mkdir -p "$dir"
    grammar_git -C "$dir" init -q
    mkdir -p "$dir/.git/info"; echo "$verbatim" > "$dir/.git/info/attributes"
    grammar_git -C "$dir" fetch -q --depth 1 "$url" "$commit"
    grammar_git -C "$dir" checkout -q FETCH_HEAD
  fi
}

# pristine_grammar DIR: the checkout in DIR put back to its commit, every
# change and file of its own gone but node_modules/ (what its grammar.js
# requires, fetched there)
pristine_grammar() {
  grammar_git -C "$1" checkout -q -- . && grammar_git -C "$1" clean -q -fdx -e node_modules
}

# grammar_checked NAME DIR RECORDED HASH: true when the checkout DIR of
# grammar NAME is its commit with nothing changed or added (node_modules/
# aside) and HASH, what compiling it reads (GRAMMAR_INPUTS_HASH), is the one
# the file RECORDED has for it; else says why, false
grammar_checked() {
  local name="$1" dir="$2" recorded="$3" found="$4"
  if [ -n "$(grammar_git -C "$dir" status --porcelain --ignored --untracked-files=all 2>/dev/null | grep -v "^.. node_modules/" || true)" ]; then
    echo "$dir has been changed, or has files of its own: delete it to fetch it again. Nothing is compiled." >&2
    return 1
  fi
  if ! grep -qx "$found  $name" "$recorded" 2>/dev/null; then
    echo "What $name compiles hashes as $found, which ${recorded##*/} does not record. When its pin has moved, read what changed (tools/check-grammar-sources.sh $name), then record it (tools/grammars.sh --record $name). Nothing is compiled." >&2
    return 1
  fi
}
