#!/usr/bin/env bash
# tools/benchmark/run.sh --- Compare Tuatara with Emacs, Neovim and Helix on large files.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# Usage: tools/benchmark/run.sh [--trial] [harness options...]
#
# Makes the benchmark's files (tools/benchmark/make-data.py), runs every
# editor on them in a pseudo-terminal (tools/benchmark/harness.py), each one
# pinned to the reference environment's processors and capped to its memory
# in a systemd user scope (tools/benchmark/reference-env.sh), and writes the report
# (tools/benchmark/report.py). Run it on an idle machine; the full run takes
# three hours or more. --trial makes small files and runs two repetitions with
# fewer keys, a few minutes per editor, to see that everything works.
# Options after it are passed to the harness (see its --help), for example
# --editors emacs,tuatara or --files csv.
#
# Where things go (BENCH_WORK, default ~/.cache/tuatara-benchmark):
#   data/ or trial-data/   the generated files, kept for the next run;
#   scratch/               each editor's empty home, made afresh for each run;
#   results.jsonl, report.md (trial-results.jsonl, trial-report.md).
# Set BENCH_EMACS_VLF to a directory holding vlf.el to give Emacs vlf, and
# BENCH_TUATARA to measure another build of Tuatara than build/tuatara.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
. "$here/reference-env.sh"
work=${BENCH_WORK:-${XDG_CACHE_HOME:-$HOME/.cache}/tuatara-benchmark}
prefix=""
sizes=()
options=()
if [ "${1:-}" = --trial ]; then
  shift
  prefix=trial-
  sizes=(--csv-mb 40 --huge-mb 200 --log-mb 5 --org-mb 1 --json-mb 2)
  options=(--repetitions 2 --typing-keys 40 --page-keys 15)
fi
if [ ! -x "${BENCH_TUATARA:-$root/build/tuatara}" ]; then
  echo "No Tuatara build: run tools/build-image.sh first." >&2
  exit 1
fi
for program in emacs nvim hx taskset systemd-run python3; do
  command -v "$program" >/dev/null || { echo "$program is not installed." >&2; exit 1; }
done
# Tuatara opens files in the temporary directories read-only, as come from outside
case "$work/" in
  /tmp/*|/var/tmp/*|"${TMPDIR:-/tmp}"/*)
    echo "BENCH_WORK must not be in a temporary directory: Tuatara opens files there read-only." >&2; exit 1 ;;
esac
mkdir -p "$work"
python3 -I "$here/make-data.py" "$work/${prefix}data" "${sizes[@]}"
taskset -c "$reference_spare" python3 -I "$here/harness.py" \
  --data "$work/${prefix}data" --scratch "$work/scratch" --out "$work/${prefix}results.jsonl" \
  --wrap "$reference_wrap" "${options[@]}" "$@"
python3 -I "$here/report.py" "$work/${prefix}results.jsonl" > "$work/${prefix}report.md"
rm -rf "$work/scratch"
echo "Report: $work/${prefix}report.md"
