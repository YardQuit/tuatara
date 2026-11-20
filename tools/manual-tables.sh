#!/usr/bin/env bash
# Generate the manual's reference tables from the program itself: every
# command with its keys, every setting with its default and the values it takes, every face.
# Writes doc/manual/generated/, puts the rows into the lines "#+table: commands
# NAME ..." of each chapter as doc/manual/generated/NN-name.org, and says which
# names no chapter covers yet.
# A chapter line "#+table: all-settings", "#+table: all-commands" or
# "#+table: mode-commands" takes every row of that kind. Each chapter also gets
# links to the chapters before and after it, in the order of OUTLINE.org.
# Given a package's repository (../tuatara-packages/tuatara-palimpsest), the
# tables are that package's (its own commands, settings and faces, the package
# loaded from there) and its doc/manual/ is written instead.
# Usage: tools/manual-tables.sh [PACKAGE-DIRECTORY]
set -euo pipefail
package=${1:+$(cd "$1" && pwd)/}
cd "$(dirname "$0")/.."
. tools/inside.sh
in_tree '(list :tuatara/defaults :tuatara/frontend-fake :tuatara/terminal)' \
  '(load "tools/manual-tables.lisp")' \
  "(manual-tables:run #p\"$PWD/\"${package:+ :package #p\"$package\"})"
