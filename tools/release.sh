#!/usr/bin/env bash
# tools/release.sh --- Put together Tuatara as it is given to others.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Put together build/release/tuatara/, Tuatara as it is given to others: the
# program and what it reads beside it, so that the directory runs wherever it
# is copied, nothing compiled there: grammars/ (the grammars, and
# tuatara-syntax-helper, the program with tree-sitter in it that runs them),
# terminal/ (the terminal library, its layout, Ghostty's license and in
# terminal/licenses/ those of what the library compiles in), syntax/
# (the grammars' rules), icons/, the license and the README; grammars/licenses/
# holds each grammar's licence (tools/grammars.sh), licenses/ those of SBCL and
# of the Lisp libraries the program carries. Beside it,
# build/release/tuatara-sources.tar.zst holds what the release is made from, to
# go with it wherever it is given: Tuatara at the last commit, the Lisp
# libraries as Quicklisp gave them, Ghostty at the
# commit of lisp/terminal/library.pin, and tree-sitter and every grammar at
# the commit of syntax/grammars.tsv, each repository without its history.
# The program is built from the last commit, copied with Quicklisp into a
# new folder of its own, /tmp/tuatara-release-build.XXXXXX/, so that the places of its
# sources it keeps are that folder's; it is refused when it still names this
# machine's home folder or user, as is the release when any file of it does.
# The grammars and the terminal library are
# built here first and copied there, never compiled again. build/tuatara, the
# program of this checkout, is left as it is. tools/appimage.sh and
# tools/distro-packages.sh make their packages of the release.
# Usage: tools/release.sh  (does nothing when the release is up to date)
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
. tools/inside.sh
# Quicklisp's setup.lisp where tools/quicklisp.lisp finds it
find_quicklisp_setup() {
  local setup
  for setup in "${QUICKLISP_SETUP:-}" "$HOME/quicklisp/setup.lisp" "$HOME/.local/share/quicklisp/setup.lisp"; do
    if [ -n "$setup" ] && [ -f "$setup" ]; then echo "$setup"; return; fi
  done
  echo "tools/release.sh: Quicklisp is not installed: see HACKING.org" >&2
  exit 1
}
find_quicklisp_setup >/dev/null
if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "tools/release.sh: changes not committed are left out of the release" >&2
fi
tools/grammars.sh
tools/terminal-library.sh
# nothing to do when the release there was made from this commit, nothing
# changed since it, with these scripts, the same grammars and terminal
# library, and the same Lisp libraries
stamp="$root/build/release/.built-from"
wanted="$(git rev-parse HEAD) $(cat "$0" tools/lisp-archives.sh tools/lisp-archives.sha256 tools/build-image.lisp tools/build-image.sh tools/inside.sh build/grammars/.built-from \
            build/terminal/.built-from "$(dirname "$(find_quicklisp_setup)")/dists/quicklisp/distinfo.txt" | sha256sum | cut -c1-16)"
if [ -z "$(git status --porcelain --untracked-files=no)" ] &&
   [ -f "$stamp" ] && [ "$(cat "$stamp")" = "$wanted" ] && [ -x build/release/tuatara/tuatara ] &&
   [ -f build/release/tuatara-sources.tar.zst ]; then
  echo "$root/build/release/tuatara"
  exit 0
fi
rm -f "$stamp"
quicklisp="$(dirname "$(find_quicklisp_setup)")"
# a new folder of this account's alone, which the toolbox sees at the same path;
# it and the folder the sources are packed in go however the script ends
work="$(mktemp -d /tmp/tuatara-release-build.XXXXXX)"
pack=""
trap 'rm -rf "$work" ${pack:+"$pack"}' EXIT
mkdir -p "$work/src/build" "$work/cache"
git archive HEAD | tar -x -C "$work/src"
cp -a build/grammars build/terminal "$work/src/build/"
cp -a "$quicklisp" "$work/quicklisp"
tools/lisp-archives.sh prepare "$work/quicklisp"
"$work/src/tools/build-image.sh" \
  QUICKLISP_SETUP="$work/quicklisp/setup.lisp" XDG_CACHE_HOME="$work/cache" \
  CL_SOURCE_REGISTRY="(:source-registry (:tree \"$work/src/\") :ignore-inherited-configuration)" \
  TUATARA_BUILD="$(git rev-parse --short=7 HEAD) nil $(date +%F)"
program="$work/src/build/tuatara"
if names_this_machine "$program"; then
  echo "tools/release.sh: $program names this machine's home folder or user, where shown above: not released" >&2
  exit 1
fi
out="$root/build/release/tuatara"
rm -rf "$out"
mkdir -p "$out/grammars" "$out/terminal" "$out/syntax"
cp "$program" "$out/"
# the sources go with the release (tuatara-sources.tar.zst, put together below)
pack="$(mktemp -d)"
top="$pack/tuatara-sources"
mkdir -p "$top/grammars" "$top/lisp"
# the licence of every Lisp system the program carries (tools/build-image.lisp
# lists them), from its sources, else from data/licenses/ for one SBCL carries
# compiled; the sources of each that is neither Tuatara nor part of SBCL
mkdir -p "$out/licenses/lisp" "$out/licenses/sbcl"
# beside the SBCL that built it: a system's package (share/licenses/) or SBCL's own installer (share/doc/)
inside sh -c 'top="$(dirname "$(command -v sbcl)")/.."
  for file in "$top/share/licenses/sbcl/COPYING" "$top/share/doc/sbcl/COPYING"; do
    [ -f "$file" ] && exec cat "$file"
  done' > "$out/licenses/sbcl/COPYING" || true
[ -s "$out/licenses/sbcl/COPYING" ] || { echo "tools/release.sh: SBCL's licence (COPYING) is not found" >&2; exit 1; }
unlicensed=""
while IFS=$'\t' read -r name directory; do
  case "$name" in
    tuatara|sb-*) continue ;;  # Tuatara's own, and SBCL's under its licence
    asdf|uiop|asdf-package-system) text=asdf ;;
    *) text="$name" ;;
  esac
  kept="$out/licenses/lisp/$text"
  if [ ! -d "$kept" ]; then
    found=""
    if [ -n "$directory" ]; then
      found=$(find "$directory" -maxdepth 1 -type f -iregex '.*/\(licen[cs]e\|copying\|copyright\)[^/]*' | sort)
    fi
    if [ -n "$found" ]; then
      mkdir -p "$kept"
      while IFS= read -r file; do cp "$file" "$kept/"; done <<< "$found"
    elif [ -f "data/licenses/$text.txt" ]; then
      mkdir -p "$kept"; cp "data/licenses/$text.txt" "$kept/LICENSE"
    else
      unlicensed="$unlicensed $name"
    fi
  fi
  if [ -n "$directory" ]; then
    mkdir -p "$top/lisp/$name"
    tar -C "$directory" --exclude=.git -cf - . | tar -x -C "$top/lisp/$name"
  fi
done < "$work/src/build/lisp-systems.tsv"
if [ -n "$unlicensed" ]; then
  echo "tools/release.sh: no licence for the Lisp systems:$unlicensed. Its text goes in data/licenses/NAME.txt." >&2
  exit 1
fi
cp build/grammars/*.so build/grammars/tuatara-syntax-helper "$out/grammars/"
cp -r build/grammars/licenses "$out/grammars/licenses"
cp build/terminal/libghostty-vt.so build/terminal/libtuatara-terminal.so build/terminal/layout.sexp \
   build/terminal/LICENSE-ghostty "$out/terminal/"
cp -r build/terminal/licenses "$out/terminal/licenses"
# their debugging information names the folders they were built in
strip --strip-debug "$out/terminal/libghostty-vt.so" "$out/terminal/libtuatara-terminal.so" \
      "$out/grammars/tuatara-syntax-helper"
for kind in rules indents textobjects injections locals tags; do
  cp -r "syntax/$kind" "$out/syntax/$kind"
done
# each grammar's repository, commit and license
cp syntax/grammars.tsv "$out/syntax/"
cp -r data/icons "$out/icons"
cp LICENSE README.org "$out/"
# nothing given to others names this machine, the program checked above
named=0
while IFS= read -r -d '' file; do
  if names_this_machine "$file"; then named=1; fi
done < <(find "$out" -type f ! -path "$out/tuatara" -print0)
if [ "$named" = 1 ]; then
  echo "tools/release.sh: files of $out name this machine's home folder or user, where shown above: not released" >&2
  exit 1
fi
rm -rf "$work"

. tools/grammar-build.sh
# the rest of the sources, each grammar exactly at its pinned commit (the
# patches of syntax/patches/ go with Tuatara's sources, applied as the build
# does); a grammar a grammar requires lives in its node_modules/
git archive --prefix=tuatara-sources/tuatara/ HEAD | tar -x -C "$pack"
git -C build/terminal-sources archive --prefix=tuatara-sources/ghostty/ HEAD | tar -x -C "$pack"
while IFS=$'\t' read -r name url commit licence subpath; do
  [ "$name" = tree-sitter-cli ] && continue
  mkdir -p "$top/grammars/$name"
  if [ "$(grammar_git -C "build/grammar-sources/$name" rev-parse -q --verify "HEAD^{commit}")" != \
       "$(grammar_git -C "build/grammar-sources/$name" rev-parse -q --verify "$commit^{commit}")" ]; then
    echo "tools/release.sh: build/grammar-sources/$name is not at the commit syntax/grammars.tsv pins: run tools/grammars.sh" >&2
    exit 1
  fi
  grammar_git -C "build/grammar-sources/$name" archive HEAD | tar -x -C "$top/grammars/$name"
done < <(grep -v '^#' syntax/grammars.tsv | grep -v '^[[:space:]]*$')
cat > "$top/README" <<EOF
The sources of Tuatara $(sed -n 's/^ *:version "\(.*\)"$/\1/p' tuatara.asd) ($(git rev-parse --short=7 HEAD)) and of what its
release is built with: tuatara/ (GPL-3.0-or-later), lisp/ (the Lisp libraries
compiled into the program, each under the licence in its folder), ghostty/
(the terminal library, MIT, with simdutf in its pkg/; Highway, Wuffs and
uucode, which it compiles in too, are fetched by its build.zig.zon and are
not in it, their licences in the release's terminal/licenses/), and grammars/, tree-sitter and each grammar at the commit
tuatara/syntax/grammars.tsv names, under the licence it names there, which the
build patches as tuatara/syntax/patches/ says. tuatara/HACKING.org says how to
build it.
EOF
tar -C "$pack" -cf - tuatara-sources | zstd -19 -T0 -q -f -o "$root/build/release/tuatara-sources.tar.zst"
echo "$wanted" > "$stamp"
echo "$out"
