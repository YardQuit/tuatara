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
# Beside Ghostty's licence (LICENSE-ghostty), those of what the library
# compiles in go in build/terminal/licenses/: simdutf's, Highway's, Wuffs's,
# uucode's (with those of the Unicode data and the UTF-8 decoder it carries)
# and Zig's (its standard library and the C++ library it compiles in), each
# in a folder of its name, licenses.tsv naming each and its licence as an SPDX
# expression that joins others by AND; the build stops when one cannot be
# found.
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
# the licences of what the library compiles in, each in licenses/NAME/
licences="$out.new/licenses"
mkdir -p "$licences"
zig_env="$(zig env)"
zig_cache="$(sed -n 's/^ *\.global_cache_dir = "\(.*\)",$/\1/p' <<<"$zig_env")"
zig_lib="$(sed -n 's/^ *\.lib_dir = "\(.*\)",$/\1/p' <<<"$zig_env")"
missing() { echo "tools/terminal-library.sh: no licence for $1 ($2): not built" >&2; exit 1; }
# the hash Ghostty's build.zig.zon file $1 gives its dependency $2
zon_hash() {
  awk -v name=".$2 = .{" 'index($0, name) { found = 1 } found && /\.hash = "/ { sub(/.*\.hash = "/, ""); sub(/".*/, ""); print; exit }' "$sources/$1"
}
# file $3 of the package of Zig's cache with the hash $2, as a licence of $1
from_package() {
  local dir="$zig_cache/p/$2"
  mkdir -p "$licences/$1"
  if [ -d "$dir" ]; then cp "$dir/$3" "$licences/$1/" 2>/dev/null || missing "$1" "$3"
  else tar -xzOf "$dir.tar.gz" "$2/$3" > "$licences/$1/$(basename "$3")" 2>/dev/null || missing "$1" "$3"; fi
}
# the file at $3, which must have the SHA-256 $4, as licence $2 of $1: fetched
# once and kept in build/licence-cache/ by its SHA-256, so that the library
# builds again without the network
cache="$root/build/licence-cache"
from_address() {
  mkdir -p "$licences/$1" "$cache"
  if ! echo "$4  $cache/$4" | sha256sum -c --status 2>/dev/null; then
    curl -fsSL --max-time 60 --retry 3 -o "$cache/$4.part" "$3" || missing "$1" "$3"
    echo "$4  $cache/$4.part" | sha256sum -c --status || { rm -f "$cache/$4.part"; missing "$1" "$3 is not the file expected"; }
    mv "$cache/$4.part" "$cache/$4"
  fi
  cp "$cache/$4" "$licences/$1/$2"
}
# simdutf, copied into Ghostty's sources: its licences at its version, and the
# notice of the part of it taken from PyTorch, from its own text
simdutf_version=9.0.0
simdutf_h="$sources/pkg/simdutf/vendor/simdutf.h"
grep -q "^#define SIMDUTF_VERSION \"$simdutf_version\"" "$simdutf_h" || missing simdutf "simdutf.h is not version $simdutf_version"
from_address simdutf LICENSE-MIT "https://raw.githubusercontent.com/simdutf/simdutf/v$simdutf_version/LICENSE-MIT" \
  fc8dbc04e03ad4efc08a647ffe7f995b811a95bc04c0e85a56d5277c6593fa5f
from_address simdutf LICENSE-APACHE "https://raw.githubusercontent.com/simdutf/simdutf/v$simdutf_version/LICENSE-APACHE" \
  3d34610fc6b5e1b0bfe4e2f36171c2d62c28ef05cb8d704f5a0073be41a43b3d
awk '/begin file include\/simdutf\/internal\/isadetection.h/ { on = 1; next } on && /^\/\* From/ { keep = 1 }
     keep { print } keep && /\*\// { exit }' "$simdutf_h" > "$licences/simdutf/NOTICE-isadetection"
grep -q "Redistribution and use" "$licences/simdutf/NOTICE-isadetection" || missing simdutf "the notice in simdutf.h"
from_package highway "$(zon_hash pkg/highway/build.zig.zon highway)" LICENSE
from_package highway "$(zon_hash pkg/highway/build.zig.zon highway)" LICENSE-BSD3
for file in LICENSE LICENSE-APACHE LICENSE-MIT; do
  from_package wuffs "$(zon_hash pkg/wuffs/build.zig.zon wuffs)" "$file"
done
from_package uucode "$(zon_hash build.zig.zon uucode)" LICENSE.md
# the licences uucode's own names, which its package leaves out
uucode_commit=ea62149739404a73c202b48a33bf6dd2af4bd9b0
grep -q "uucode-$uucode_commit\.tar\.gz" "$sources/build.zig.zon" || missing uucode "build.zig.zon names no uucode $uucode_commit"
from_address uucode LICENSE_unicode "https://raw.githubusercontent.com/jacobsandlund/uucode/$uucode_commit/licenses/LICENSE_unicode" \
  1eda5a3b026870c737b22e8bcd4954338612c790db688242e003f41a4fa95175
from_address uucode LICENSE_Bjoern_Hoehrmann "https://raw.githubusercontent.com/jacobsandlund/uucode/$uucode_commit/licenses/LICENSE_Bjoern_Hoehrmann" \
  de219cece932aad5a817bf763393d8d149d378a15d2ad5320e3331eac07626dd
# Zig's own licence: beside its lib folder as ziglang.org gives it, or where a
# distribution puts it
mkdir -p "$licences/zig"
zig_licence=""
for file in "$zig_lib/../LICENSE" "$zig_lib/LICENSE" /usr/share/licenses/zig/LICENSE /usr/share/licenses/zig*/LICENSE* \
            /usr/share/doc/zig/LICENSE /usr/share/doc/zig*/LICENSE*; do
  if [ -f "$file" ]; then zig_licence="$file"; break; fi
done
[ -n "$zig_licence" ] || missing zig "LICENSE, beside $zig_lib or in /usr/share/licenses/zig/"
cp "$zig_licence" "$licences/zig/LICENSE"
cp "$zig_lib/libcxx/LICENSE.TXT" "$licences/zig/LICENSE-libcxx.TXT" 2>/dev/null || missing zig libcxx/LICENSE.TXT
printf '%s\t%s\n' \
  simdutf "(Apache-2.0 OR MIT) AND BSD-3-Clause" \
  highway "(Apache-2.0 OR BSD-3-Clause)" \
  wuffs "MIT AND Apache-2.0" \
  uucode "MIT AND Unicode-3.0" \
  zig "MIT AND Apache-2.0 WITH LLVM-exception" > "$licences/licenses.tsv"
# the library and its headers in place only now that every licence is there
mkdir -p "$out"
cp "$out.new/lib/libghostty-vt.so.0.1.0" "$out/libghostty-vt.so"
rm -rf "$out/include"; cp -r "$out.new/include" "$out/include"
cp "$sources/LICENSE" "$out/LICENSE-ghostty"
rm -rf "$out/licenses"; cp -r "$licences" "$out/licenses"
cc -o "$out.new/layout" -I"$out/include" "$root/lisp/terminal/layout.c"
"$out.new/layout" > "$out/layout.sexp"
# what the library takes in a way Lisp cannot pass (lisp/terminal/shim.c), beside it: its
# calls into the library found when it is loaded, after the library
zig cc -target "$target" -O2 -fPIC -shared -o "$out/libtuatara-terminal.so" -I"$out/include" \
  "$root/lisp/terminal/shim.c"

rm -rf "$out.new"
echo "$wanted" > "$stamp"
