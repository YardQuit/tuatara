#!/usr/bin/env bash
# tools/terminal-library.sh --- Build the terminal library M-x terminal draws with.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Build the terminal library M-x terminal draws with: libghostty-vt, from
# Ghostty at the commit lisp/terminal/library.pin names, with the Zig it names,
# into build/terminal/ (libghostty-vt.so and its C headers). Optimised, and for
# glibc 2.28 and later, so that the same file runs on older systems too, and
# layout.sexp, what its headers say of the enumerations and structures the
# emulator reads (lisp/terminal/layout.c, run with the C compiler), and
# libtuatara-terminal.so (lisp/terminal/shim.c, built by Zig for TARGET too); a
# release carries it in terminal/ beside the program, built for each processor
# (TARGET, x86_64-linux-gnu.2.28 unless given), so no one compiles it but us.
# Nothing is fetched or compiled while Tuatara runs.
# Usage: tools/terminal-library.sh [TARGET]  (does nothing when it is up to date)
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
pin="$root/lisp/terminal/library.pin"
target="${1:-x86_64-linux-gnu.2.28}"
field() { awk -F'\t' -v k="$1" '$1 == k { print $2 }' "$pin"; }
repository="$(field repository)" commit="$(field commit)" zig_wanted="$(field zig)"
out="$root/build/terminal"
[ "$target" = "x86_64-linux-gnu.2.28" ] || out="$root/build/terminal-$target"
sources="$root/build/terminal-sources"
stamp="$out/.built-from"
wanted="$(cat "$pin" "$0" "$root/lisp/terminal/layout.c" "$root/lisp/terminal/shim.c" | sha256sum | cut -c1-16)-$target"
if [ -f "$stamp" ] && [ "$(cat "$stamp")" = "$wanted" ] && [ -f "$out/libghostty-vt.so" ]; then exit 0; fi
if ! command -v zig >/dev/null || [ "$(zig version)" != "$zig_wanted" ]; then
  echo "tools/terminal-library.sh: Zig $zig_wanted is needed to build the terminal library (have: $(zig version 2>/dev/null || echo none)); HACKING.org says where to get it" >&2
  exit 1
fi
if [ "$(git -C "$sources" rev-parse HEAD 2>/dev/null)" != "$commit" ]; then
  rm -rf "$sources"; mkdir -p "$sources"
  git -C "$sources" init -q
  git -C "$sources" fetch -q --depth 1 "$repository" "$commit"
  git -C "$sources" checkout -q FETCH_HEAD
fi
rm -rf "$out.new"
(cd "$sources" && zig build -Demit-lib-vt -Doptimize=ReleaseSafe -Dtarget="$target" --prefix "$out.new" >/dev/null)
mkdir -p "$out"
cp "$out.new/lib/libghostty-vt.so.0.1.0" "$out/libghostty-vt.so"
rm -rf "$out/include"; cp -r "$out.new/include" "$out/include"
cp "$sources/LICENSE" "$out/LICENSE-ghostty"
cc -o "$out.new/layout" -I"$out/include" "$root/lisp/terminal/layout.c"
"$out.new/layout" > "$out/layout.sexp"
# what the library takes in a way Lisp cannot pass (lisp/terminal/shim.c), beside it: its
# calls into the library found when it is loaded, after the library
zig cc -target "$target" -O2 -fPIC -shared -o "$out/libtuatara-terminal.so" -I"$out/include" \
  "$root/lisp/terminal/shim.c"

rm -rf "$out.new"
echo "$wanted" > "$stamp"
