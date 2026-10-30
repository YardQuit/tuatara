#!/usr/bin/env bash
# tools/lisp-archives.sh --- Check, fetch and add the archives of the Lisp libraries Tuatara is built with.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# The libraries are Quicklisp's, each archive named with its SHA-256 in
# tools/lisp-archives.sha256. This is the one place that list is read, by
# tools/ci-setup.sh, tools/release.sh and tools/build-image.sh. Usage:
#   tools/lisp-archives.sh check [QUICKLISP]    every listed archive is there and
#                                               has its hash; stops on any other
#   tools/lisp-archives.sh fetch [QUICKLISP]    download the listed archives over
#                                               HTTPS, kept only with their hash
#   tools/lisp-archives.sh index [QUICKLISP]    download the Quicklisp set's index
#   tools/lisp-archives.sh lock [QUICKLISP]     Quicklisp downloads nothing more,
#                                               so a new library stops a load
#   tools/lisp-archives.sh prepare [QUICKLISP]  check, then leave only the listed
#                                               libraries, unpacked from their
#                                               archives, and lock: for a copy
#                                               a release is compiled from
#   tools/lisp-archives.sh add LIBRARY [QUICKLISP]
#                                               download LIBRARY over HTTPS from
#                                               the set's index and add its line
#                                               to the list, for review
# QUICKLISP is the folder holding setup.lisp: $QUICKLISP_SETUP's, else
# ~/quicklisp, else ~/.local/share/quicklisp.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
list="$root/tools/lisp-archives.sha256"
# the Quicklisp set the list is made from; its libraries are checked one by
# one, so its index files are not
dist_url=https://beta.quicklisp.org/dist/quicklisp/2026-01-01

fail() { echo "tools/lisp-archives.sh: $*" >&2; exit 1; }

quicklisp_dir() {  # the folder given, else where Quicklisp is installed
  local dir setup
  if [ -n "${1:-}" ]; then echo "$1"; return; fi
  for setup in "${QUICKLISP_SETUP:-}" "$HOME/quicklisp/setup.lisp" "$HOME/.local/share/quicklisp/setup.lisp"; do
    if [ -n "$setup" ] && [ -f "$setup" ]; then dirname "$setup"; return; fi
  done
  fail "Quicklisp is not installed: see HACKING.org"
}

entries() {  # the list's lines: hash, then archive name
  [ -s "$list" ] || fail "tools/lisp-archives.sha256 is missing"
  grep -v '^#' "$list"
}

download() {  # download $1 to $2, kept only when its SHA-256 is $3
  curl -sSfL --retry 3 -o "$2.part" "$1" || { rm -f "$2.part"; fail "$1 could not be downloaded"; }
  if ! echo "$3  $2.part" | sha256sum -c --status; then
    rm -f "$2.part"
    fail "$1 is not the file expected: not used"
  fi
  mv "$2.part" "$2"
}

https_url() {  # the archive's address in the index, over HTTPS
  local index="$1" name="$2" url
  url=$(awk -v a="${name%.tgz}" '$6 == a { print $2 }' "$index" | sed 's|^http:|https:|')
  [ -n "$url" ] || fail "$name is not in the Quicklisp set $dist_url"
  echo "$url"
}

check() {
  local archives="$1/dists/quicklisp/archives" sha256 archive bad=0
  while read -r sha256 archive; do
    if [ ! -f "$archives/$archive" ]; then
      echo "tools/lisp-archives.sh: $archive is not in $archives: tools/lisp-archives.sh fetch downloads it" >&2
      bad=1
    elif ! echo "$sha256  $archives/$archive" | sha256sum -c --status; then
      echo "tools/lisp-archives.sh: $archives/$archive is not the archive tools/lisp-archives.sha256 lists: not used" >&2
      bad=1
    fi
  done < <(entries)
  [ "$bad" = 0 ] || exit 1
}

index() {
  local dist="$1/dists/quicklisp" file
  mkdir -p "$dist/archives" "$1/config" "$1/local-projects"
  for file in distinfo.txt systems.txt releases.txt; do
    curl -sSfL --retry 3 -o "$dist/$file" "$dist_url/$file" || fail "$dist_url/$file could not be downloaded"
  done
  touch "$dist/enabled.txt"
}

fetch() {
  local archives="$1/dists/quicklisp/archives" sha256 archive
  [ -s "$1/dists/quicklisp/releases.txt" ] || fail "$1 has no Quicklisp index: tools/lisp-archives.sh index makes one"
  mkdir -p "$archives"
  while read -r sha256 archive; do
    if [ -f "$archives/$archive" ] && echo "$sha256  $archives/$archive" | sha256sum -c --status; then continue; fi
    download "$(https_url "$1/dists/quicklisp/releases.txt" "$archive")" "$archives/$archive" "$sha256"
  done < <(entries)
}

lock() {
  mkdir -p "$1/config"
  # a proxy that answers nothing: anything Quicklisp would still download fails
  echo "http://127.0.0.1:9/" > "$1/config/proxy-url.txt"
}

prepare() {
  local archives="$1/dists/quicklisp/archives" file
  check "$1"
  # only what the list names is compiled: no other archive, nothing unpacked
  # earlier (Quicklisp unpacks the archives again), nothing of local-projects
  for file in "$archives"/*; do
    [ -e "$file" ] || continue
    grep -q "^[0-9a-f]\{64\}  $(basename "$file")$" "$list" || rm -f "$file"
  done
  rm -rf "$1/dists/quicklisp/software" "$1/local-projects"
  mkdir -p "$1/local-projects"
  lock "$1"
}

add() {
  local name="$1" dir="$2" work line url archive sha256 md5 size
  [ -n "$name" ] || fail "add: which library? (a Quicklisp project name, as releases.txt has it)"
  work="$(mktemp -d)"
  trap 'rm -rf "$work"' EXIT
  curl -sSfL --retry 3 -o "$work/releases.txt" "$dist_url/releases.txt" || fail "$dist_url/releases.txt could not be downloaded"
  line=$(awk -v p="$name" '$1 == p { print; exit }' "$work/releases.txt")
  [ -n "$line" ] || fail "$name is not in the Quicklisp set $dist_url"
  url=$(echo "$line" | awk '{ print $2 }' | sed 's|^http:|https:|')
  size=$(echo "$line" | awk '{ print $3 }')
  md5=$(echo "$line" | awk '{ print $4 }')
  archive=$(basename "$url")
  if grep -q "  $archive\$" "$list"; then
    echo "$archive is in the list already"
    return
  fi
  curl -sSfL --retry 3 -o "$work/$archive" "$url" || fail "$url could not be downloaded"
  [ "$(stat -c %s "$work/$archive")" = "$size" ] || fail "$archive is not the size the index gives"
  [ "$(md5sum "$work/$archive" | cut -d' ' -f1)" = "$md5" ] || fail "$archive is not the MD5 the index gives"
  sha256=$(sha256sum "$work/$archive" | cut -d' ' -f1)
  mkdir -p "$dir/dists/quicklisp/archives"
  cp "$work/$archive" "$dir/dists/quicklisp/archives/$archive"
  echo "$sha256  $archive" >> "$list"
  echo "Added to tools/lisp-archives.sha256, from $url:"
  echo "$sha256  $archive"
  echo "Review the library (licence, owner, what it loads) before committing the line;"
  echo "its dependencies are not added: run this for each the build reports missing."
}

command="${1:-}"
case "$command" in
  check|fetch|index|lock|prepare) shift; "$command" "$(quicklisp_dir "${1:-}")" ;;
  add) shift; add "${1:-}" "$(quicklisp_dir "${2:-}")" ;;
  *) fail "usage: tools/lisp-archives.sh check|fetch|index|lock|prepare [QUICKLISP] | add LIBRARY [QUICKLISP]" ;;
esac
