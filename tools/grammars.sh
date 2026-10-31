#!/usr/bin/env bash
# tools/grammars.sh --- Fetch and compile the tree-sitter library and grammars.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Fetch the tree-sitter library and the grammars listed in syntax/grammars.tsv,
# each at its pinned commit, and compile them (in the toolbox when there is one; a scanner
# written in C++ with the C++ compiler, the grammar linked with its library) into
# build/grammars/: tuatara-syntax-helper (tree-sitter with syntax/syntax.c and
# syntax/helper.c, the program that runs the grammars for the editor) and
# one NAME.so per grammar, from the directory the manifest's fifth column
# names in its repository when it has one, and build/grammars/licenses/NAME/
# with its licence: its own licence files (LICENSE, COPYING, a LICENSES/
# folder; in that directory, else at the top of its repository), else syntax/licenses/NAME.txt for one that has none; a
# grammar with neither stops it. A grammar with syntax/patches/NAME.patch is patched
# and its parser generated again with the pinned tree-sitter program first;
# tree-sitter itself is patched with syntax/patches/tree-sitter.patch.
# Each is compiled from a checkout of its pinned commit with nothing changed
# or added (one that has been changed stops it: delete its folder in
# build/grammar-sources/ to fetch it again), and only when what compiling it
# reads (its parser, its scanner, the headers they include, and the
# grammar.js a parser is generated from) hashes as syntax/grammar-inputs.sha256
# records. When a pin moves, tools/check-grammar-sources.sh says what changed
# in that; once it is read, tools/grammars.sh --record NAME... records the new
# hashes (--record alone, every grammar's).
# Nothing is fetched or compiled while Tuatara runs.
# Usage: tools/grammars.sh                 (does nothing when everything is up to date)
#        tools/grammars.sh --record [NAME...]
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/build/grammars"
sources="$root/build/grammar-sources"
stamp="$out/.built-from"
record=""
if [ "${1:-}" = --record ]; then shift; record="all $*"; fi
wanted="$(cat "$root/syntax/grammars.tsv" "$root/syntax/grammar-inputs.sha256" "$root/syntax/syntax.c" "$root/syntax/helper.c" "$root/syntax/safe-ctype.h" "$root/syntax/counted-memory.h" "$root"/syntax/patches/*.patch "$root"/syntax/licenses/*.txt "$0" "$root/tools/grammar-build.sh" 2>/dev/null | sha256sum | cut -c1-16)"
if [ -z "$record" ] && [ -f "$stamp" ] && [ "$(cat "$stamp")" = "$wanted" ]; then exit 0; fi
mkdir -p "$out" "$sources" "$root/build/tools"
. "$root/tools/inside.sh"
inside bash -euo pipefail -c '
root="$1"; out="$2"; sources="$3"; record="$4"
manifest() { grep -v "^#" "$root/syntax/grammars.tsv" | grep -v "^[[:space:]]*$"; }
. "$root/tools/grammar-build.sh"
fetch() { fetch_grammar "$sources/$1" "$2" "$3"; }  # fetch repository $2 at commit $3 into $sources/$1
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
# tools/grammar-build.sh says, from its pinned commit with nothing changed and
# only when what it compiles is what was recorded
recorded="$root/syntax/grammar-inputs.sha256"
pristine() { pristine_grammar "$sources/$1"; }
inputs_of() {  # the SHA-256 of what compiling $1 (its manifest row $1 $2) reads, from its checkout as it is
  local name="$1" subpath="$2" dir="$sources/$1" extra=()
  case "$name" in
    tree-sitter) grammar_inputs_hash "$dir" "$dir/lib/src" ;;
    */*) grammar_inputs_hash "$dir" "$dir/src" grammar.js ;;
    *) grep -q "^diff --git a/grammar.js" "$root/syntax/patches/$name.patch" 2>/dev/null && extra=("${subpath:+$subpath/}grammar.js")
       grammar_inputs_hash "$dir" "$dir/${subpath:+$subpath/}src" ${extra[@]+"${extra[@]}"} ;;
  esac
}
checked() {  # stop unless the checkout of $1 (subpath $2) is its pinned commit as fetched, hashing as recorded
  grammar_checked "$1" "$sources/$1" "$recorded" "$(inputs_of "$1" "$2")" || exit 1
}
if [ -n "$record" ]; then
  # --record: the hashes of those named (all when none), written in the order of the manifest
  set -- $record; shift
  lines="$(while IFS=$'"'"'\t'"'"' read -r name url commit license subpath; do
    [ "$name" = tree-sitter-cli ] && continue
    wanted=1
    if [ $# -gt 0 ]; then wanted=0; for one in "$@"; do [ "$one" = "$name" ] && wanted=1; done; fi
    if [ "$wanted" = 1 ]; then
      pristine "$name"
      echo "$(inputs_of "$name" "$subpath")  $name"
    else
      grep "  $name\$" "$recorded" || true
    fi
  done < <(manifest))"
  { grep "^#" "$recorded"; echo "$lines"; } > "$recorded.new"
  mv "$recorded.new" "$recorded"
  echo "Recorded in $recorded: ${*:-every grammar}" >&2
  exit 0
fi
while IFS=$'"'"'\t'"'"' read -r name url commit license subpath; do
  dir="$sources/$name"
  # a grammar in a directory of its repository (tree-sitter-typescript/tsx)
  src="$dir/${subpath:+$subpath/}src"
  case "$name" in
    tree-sitter-cli|*/*) ;;
    tree-sitter)
      # with syntax/patches/tree-sitter.patch applied, as a grammar is
      pristine "$name"
      checked "$name" ""
      [ -f "$root/syntax/patches/tree-sitter.patch" ] && grammar_git -C "$dir" apply "$root/syntax/patches/tree-sitter.patch"
      # without its assertions, as the grammars are: one that fails would end
      # the program, and the patch makes the parser check what a grammar gives
      # it instead (a parse that meets a wrong value fails); what names a
      # file names it from the top of the sources, not from this machine;
      # its names seen by the grammars it loads, which take their memory
      # through it (syntax/counted-memory.h)
      cc -O2 -std=c11 -DNDEBUG -D_POSIX_C_SOURCE=200112L -D_DEFAULT_SOURCE \
         -ffile-prefix-map="$root/=" -ffile-prefix-map="$dir/=tree-sitter/" \
         -I"$dir/lib/include" -I"$dir/lib/src" \
         "$dir/lib/src/lib.c" "$root/syntax/syntax.c" "$root/syntax/helper.c" \
         -rdynamic -lpthread -ldl -o "$out/tuatara-syntax-helper"
      rm -f "$out/libtuatara-syntax.so" ;;
    *)
      patch="$root/syntax/patches/$name.patch"
      made="$commit ${grammar_cflags[*]} $(cat "$root/syntax/safe-ctype.h" "$root/syntax/counted-memory.h" | sha256sum | cut -c1-16)"
      [ -f "$patch" ] && made="$made $(sha256sum < "$patch" | cut -c1-16)"
      [ -f "$src/scanner.cc" ] && made="$made c++ ${grammar_cxxflags[*]}"
      if [ ! -f "$out/$name.so" ] || [ "$(cat "$out/$name.commit" 2>/dev/null)" != "$made" ]; then
        [ -f "$patch" ] && pristine "$name"
        checked "$name" "$subpath"
        if [ -f "$patch" ]; then
          grammar_git -C "$dir" apply "$patch"
          # a changed grammar.js needs its parser generated again, a changed scanner not
          if grep -q "^diff --git a/grammar.js" "$patch"; then
            # what its grammar.js requires, checked as it is
            for required in $(manifest | cut -f1 | grep "^$name/" || true); do checked "$required" ""; done
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
# a grammar left out of the manifest is left out of what is built
for made in "$out"/*.so; do
  name="$(basename "$made" .so)"
  manifest | cut -f1 | grep -qx "$name" || rm -f "$made" "$out/$name.commit"
done
' grammars "$root" "$out" "$sources" "$record"
[ -n "$record" ] && exit 0
echo "$wanted" > "$stamp"
