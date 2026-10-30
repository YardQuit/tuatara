#!/usr/bin/env bash
# tools/grammars.sh --- Fetch and compile the tree-sitter library and grammars.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Fetch the tree-sitter library and the grammars listed in syntax/grammars.tsv,
# each at its pinned commit, and compile them (in the toolbox when there is one; a scanner
# written in C++ with the C++ compiler, the grammar linked with its library) into
# build/grammars/: libtuatara-syntax.so (tree-sitter with syntax/syntax.c) and
# one NAME.so per grammar, from the directory the manifest's fifth column
# names in its repository when it has one, and build/grammars/licenses/NAME/
# with its licence: its own licence files (LICENSE, COPYING, a LICENSES/
# folder; in that directory, else at the top of its repository), else syntax/licenses/NAME.txt for one that has none; a
# grammar with neither stops it. A grammar with syntax/patches/NAME.patch is patched
# and its parser generated again with the pinned tree-sitter program first;
# tree-sitter itself is patched with syntax/patches/tree-sitter.patch.
# Nothing is fetched or compiled while Tuatara runs.
# Usage: tools/grammars.sh  (does nothing when everything is up to date)
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/build/grammars"
sources="$root/build/grammar-sources"
stamp="$out/.built-from"
wanted="$(cat "$root/syntax/grammars.tsv" "$root/syntax/syntax.c" "$root/syntax/safe-ctype.h" "$root"/syntax/patches/*.patch "$root"/syntax/licenses/*.txt "$0" "$root/tools/grammar-build.sh" 2>/dev/null | sha256sum | cut -c1-16)"
if [ -f "$stamp" ] && [ "$(cat "$stamp")" = "$wanted" ]; then exit 0; fi
mkdir -p "$out" "$sources" "$root/build/tools"
. "$root/tools/inside.sh"
inside bash -euo pipefail -c '
root="$1"; out="$2"; sources="$3"
manifest() { grep -v "^#" "$root/syntax/grammars.tsv" | grep -v "^[[:space:]]*$"; }
fetch() {  # fetch repository $2 at commit $3 into $sources/$1
  local dir="$sources/$1"
  if [ "$(git -C "$dir" rev-parse HEAD 2>/dev/null)" != "$3" ] || [ "$(git -C "$dir" rev-parse --show-toplevel 2>/dev/null)" != "$dir" ]; then
    rm -rf "$dir"; mkdir -p "$dir"
    git -C "$dir" init -q
    git -C "$dir" fetch -q --depth 1 "$2" "$3"
    git -C "$dir" checkout -q FETCH_HEAD
  fi
}
cli="$root/build/tools/tree-sitter"
get_cli() {  # download the tree-sitter program from $1 unless it is there, checked against SHA-256 $2
  if [ ! -x "$cli" ] || [ "$(cat "$cli.sha256" 2>/dev/null)" != "$2" ]; then
    curl -sSfL -o "$cli.gz" "$1"
    echo "$2  $cli.gz" | sha256sum -c --quiet
    gunzip -f "$cli.gz"; chmod +x "$cli"
    echo "$2" > "$cli.sha256"
  fi
}
# first everything is fetched, the grammars before what they require
while IFS=$'"'"'\t'"'"' read -r name url commit license subpath; do
  echo "  $name ($license) at ${commit:0:12}" >&2
  case "$name" in
    tree-sitter-cli) get_cli "$url" "${commit#sha256:}" ;;
    *) fetch "$name" "$url" "$commit" ;;
  esac
done < <(manifest)
# then the licences, to go with the release: the licence files of the grammar
# itself, else those at the top of its repository, else the text in
# syntax/licenses/ for a repository that has none; nothing compiled without one
rm -rf "$out/licenses"; unlicensed=""
while IFS=$'"'"'\t'"'"' read -r name url commit license subpath; do
  [ "$name" = tree-sitter-cli ] && continue
  dir="$sources/$name"; kept="$out/licenses/$name"; found=""
  for place in "$dir${subpath:+/$subpath}" "$dir"; do
    # LICENSE, COPYING and the like, and the texts of a LICENSES/ folder (REUSE)
    found=$({ find "$place" -maxdepth 1 -type f -iregex ".*/\(licen[cs]e\|copying\|unlicense\)[^/]*"
              if [ -d "$place/LICENSES" ]; then find "$place/LICENSES" -maxdepth 1 -type f; fi; } | sort)
    [ -n "$found" ] && break
  done
  if [ -n "$found" ]; then
    mkdir -p "$kept"
    while IFS= read -r file; do cp "$file" "$kept/"; done <<< "$found"
  elif [ -f "$root/syntax/licenses/$name.txt" ]; then
    mkdir -p "$kept"; cp "$root/syntax/licenses/$name.txt" "$kept/LICENSE"
  else
    unlicensed="$unlicensed $name"
  fi
done < <(manifest)
if [ -n "$unlicensed" ]; then
  echo "tools/grammars.sh: no licence for:$unlicensed. Its text goes in syntax/licenses/NAME.txt, or the grammar is left out." >&2
  exit 1
fi
# then the library and the grammars are compiled, each grammar as
# tools/grammar-build.sh says
. "$root/tools/grammar-build.sh"
while IFS=$'"'"'\t'"'"' read -r name url commit license subpath; do
  dir="$sources/$name"
  # a grammar in a directory of its repository (tree-sitter-typescript/tsx)
  src="$dir/${subpath:+$subpath/}src"
  case "$name" in
    tree-sitter-cli|*/*) ;;
    tree-sitter)
      # with syntax/patches/tree-sitter.patch applied, as a grammar is
      git -C "$dir" checkout -q -- .
      [ -f "$root/syntax/patches/tree-sitter.patch" ] && git -C "$dir" apply "$root/syntax/patches/tree-sitter.patch"
      # without its assertions, as the grammars are: one that fails would end
      # the editor, and the patch makes the parser check what a grammar gives
      # it instead (a parse that meets a wrong value fails); what names a
      # file names it from the top of the sources, not from this machine
      cc -O2 -fPIC -shared -std=c11 -DNDEBUG -D_POSIX_C_SOURCE=200112L -D_DEFAULT_SOURCE \
         -ffile-prefix-map="$root/=" -ffile-prefix-map="$dir/=tree-sitter/" \
         -I"$dir/lib/include" -I"$dir/lib/src" \
         "$dir/lib/src/lib.c" "$root/syntax/syntax.c" -o "$out/libtuatara-syntax.so" ;;
    *)
      patch="$root/syntax/patches/$name.patch"
      made="$commit ${grammar_cflags[*]} $(sha256sum < "$root/syntax/safe-ctype.h" | cut -c1-16)"
      [ -f "$patch" ] && made="$made $(sha256sum < "$patch" | cut -c1-16)"
      [ -f "$src/scanner.cc" ] && made="$made c++ ${grammar_cxxflags[*]}"
      if [ ! -f "$out/$name.so" ] || [ "$(cat "$out/$name.commit" 2>/dev/null)" != "$made" ]; then
        if [ -f "$patch" ]; then
          git -C "$dir" checkout -q -- . && git -C "$dir" clean -q -fd -e node_modules
          git -C "$dir" apply "$patch"
          # a changed grammar.js needs its parser generated again, a changed scanner not
          if grep -q "^diff --git a/grammar.js" "$patch"; then
            echo "  patching $name and generating its parser" >&2
            generate_parser "$dir" "$cli"
          else
            echo "  patching $name" >&2
          fi
        fi
        compile_grammar "$src" "$out/$name.so"
        echo "$made" > "$out/$name.commit"
      fi ;;
  esac
done < <(manifest)
' grammars "$root" "$out" "$sources"
echo "$wanted" > "$stamp"
