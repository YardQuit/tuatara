#!/usr/bin/env bash
# tools/build-image.sh --- Build build/tuatara, the editor saved as one executable.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Build build/tuatara, the whole editor saved as one executable that starts at
# once (in the toolbox when there is one), with the tree-sitter grammars beside it in
# build/grammars/. Usage: tools/build-image.sh [NAME=VALUE ...]
# Each NAME=VALUE is set for the build itself, where it runs (in the toolbox
# when there is one): XDG_CACHE_HOME=/tmp/x/cache keeps its compiled files
# there, TUATARA_BUILD="COMMIT CHANGED DAY" says what it is built from when this
# folder is no checkout of its own.
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
# the executable is dated by when its build began, so that a source changed
# while it was built is newer than it, and tools/tuatara-tui.sh builds again
start=$(date +%s)
# no library is compiled whose archive is not the one tools/lisp-archives.sha256 lists
for word in "$@"; do
  case "$word" in QUICKLISP_SETUP=*) QUICKLISP_SETUP="${word#QUICKLISP_SETUP=}" ;; esac
done
QUICKLISP_SETUP="${QUICKLISP_SETUP:-}" tools/lisp-archives.sh check
tools/grammars.sh
tools/terminal-library.sh
# The heap it is saved with: half this machine's memory, at most 16 GB and at
# least 2 GB, so that a replacement across a file of many gigabytes has room
# (TUATARA_HEAP_MB overrides). On another machine the executable decides again
# when it starts, by the same rule.
heap_mb=${TUATARA_HEAP_MB:-$(awk '/MemTotal/ { mb = int($2 / 2048); if (mb > 16384) mb = 16384; if (mb < 2048) mb = 2048; print mb }' /proc/meminfo)}
# what it is built from, for M-x version: the commit, whether the tree has
# changes not committed, and the day
commit=$(git rev-parse --short=7 HEAD 2>/dev/null || echo unknown)
changed=$([ -n "$(git status --porcelain --untracked-files=no 2>/dev/null)" ] && echo t || echo nil)
inside env TUATARA_BUILD="$commit $changed $(date +%F)" "$@" \
  sbcl --dynamic-space-size "${heap_mb}MB" --noinform --disable-ldb \
  --non-interactive --no-sysinit --no-userinit --load tools/build-image.lisp
touch -d "@$start" build/tuatara
