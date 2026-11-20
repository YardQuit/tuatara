#!/usr/bin/env bash
# tools/manual-publish.sh --- Publish the manual's web pages on GitHub Pages.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Build the pages (tools/manual-html.sh), refuse them if they name this
# account or its home directory, and commit them as the whole of the branch
# gh-pages, one signed commit on top of what REMOTE has, which is then
# pushed. GitHub Pages serves that branch. Given a package's repository
# (../tuatara-packages/tuatara-palimpsest), publish that package's manual
# from its build/manual/ on its gh-pages; REMOTE is then one of its remotes.
# Usage: tools/manual-publish.sh REMOTE [PACKAGE-DIRECTORY] [--no-push]
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
usage="Usage: tools/manual-publish.sh REMOTE [PACKAGE-DIRECTORY] [--no-push]"
remote="${1:?$usage}"
shift
push=yes
package=
for argument in "$@"; do
  case $argument in
    --no-push) push=no ;;
    -*) echo "$usage" >&2; exit 1 ;;
    *) package=$(cd "$argument" && pwd) ;;
  esac
done
tools/manual-html.sh ${package:+"$package"}
# the repository whose pages these are: Tuatara's own or the package's
repository="${package:-$PWD}"
name=$(basename "$repository")
[ -z "$package" ] && name=Tuatara
site="$repository/build/manual"
[ -f "$site/index.html" ] || { echo "No $site/index.html: the build made no pages" >&2; exit 1; }
names=0
while IFS= read -r -d '' file; do
  if names_this_machine "$file"; then names=1; fi
done < <(find "$site" -type f -print0)
[ "$names" = 0 ] || { echo "The pages name this machine: not published" >&2; exit 1; }
cd "$repository"
version="$(git describe --tags --always)"
pages="$(mktemp -d "$repository/build/.manual-pages.XXXXXX")"
# the branch a first publish starts from nothing, gone again when this ends
branch="manual-pages-$$"
trap 'git worktree remove --force "$pages" 2> /dev/null || rm -rf "$pages"
      git branch -D --quiet "$branch" 2> /dev/null || true' EXIT
status=0
git ls-remote --exit-code --heads "$remote" gh-pages > /dev/null || status=$?
case $status in
  0) git fetch --quiet "$remote" gh-pages
     git worktree add --quiet --detach "$pages" FETCH_HEAD ;;
  2) git worktree add --quiet --detach "$pages"
     git -C "$pages" checkout --quiet --orphan "$branch" ;;
  *) echo "Could not read the branches of $remote" >&2; exit 1 ;;
esac
git -C "$pages" rm -r -f --quiet --ignore-unmatch .
cp -r "$site/." "$pages/"
rm -f "$pages"/*-manual.pdf
# GitHub Pages would otherwise run the pages through Jekyll
touch "$pages/.nojekyll"
git -C "$pages" add --all
if git -C "$pages" diff --cached --quiet; then
  echo "The pages are as $remote has them already"
  exit 0
fi
git -C "$pages" commit --quiet -S -m "Manual for $name $version"
if [ "$push" = yes ]; then
  git -C "$pages" push "$remote" HEAD:refs/heads/gh-pages
else
  echo "Committed in $pages, not pushed: git -C $pages push $remote HEAD:refs/heads/gh-pages"
  trap - EXIT
fi
