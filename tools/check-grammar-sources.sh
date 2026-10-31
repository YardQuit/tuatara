#!/usr/bin/env bash
# tools/check-grammar-sources.sh --- Check where each grammar comes from, before its pin moves.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run before any pin of syntax/grammars.tsv moves, and at each period-check.
# For each repository the manifest names (tree-sitter, every grammar, and
# what a grammar's grammar.js requires), it asks:
#   - whether the repository is still where the manifest says: its host's
#     answer for the owner and name (GitHub's, Codeberg's, GitLab's API; a
#     redirect for git.sr.ht). A repository renamed, moved to another owner
#     or gone STOPS the check: the new home is judged first (who keeps it,
#     whether it is kept up), and the manifest changed by hand;
#   - whether the pinned commit is on a branch or a tag of that repository.
#     A pin on neither STOPS it: such a commit may be one pushed to someone
#     else's copy of the repository, which the host serves all the same;
#   - whether the repository is archived, or has had no commit for two years
#     (reported, not stopped on), and whether the pin is on a tag alone, its
#     branch rewritten since (WATCH);
#   - for each pin that differs from the one committed (git HEAD's manifest),
#     how far it moved, and the difference in what is compiled (src/, the
#     scanner and its headers, and grammar.js, which a patched grammar is
#     generated from), written to build/grammar-check/NAME.diff to be read by
#     hand before the bump goes in: tools/grammars.sh compiles a grammar only
#     when what it compiles hashes as syntax/grammar-inputs.sha256 records.
# Nothing in the repository is changed, and nothing fetched is run: only the
# history of each repository is fetched (no files), into
# build/grammar-check/, and for a moved pin the files of the old and the new
# commit. GitHub is asked through gh when it is installed, else with
# $GITHUB_TOKEN; without either, a repository on GitHub STOPS it, not asked
# about. A history that cannot be fetched STOPS it too.
# Usage: tools/check-grammar-sources.sh [NAME...]   (every repository when no NAME)
# Exit status: 0 nothing stops a bump, 1 something does (listed under STOP).
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
work="$root/build/grammar-check"
. "$root/tools/grammar-build.sh"
mkdir -p "$work"
stale_days=730
now=$(date +%s)
stops=() reports=() watches=() diffs=()

# the manifest's rows NAME URL COMMIT, those named on the command line or all
rows() {
  grep -v '^#' "$root/syntax/grammars.tsv" | grep -v '^[[:space:]]*$' | while IFS=$'\t' read -r name url commit license subpath; do
    [ "$name" = tree-sitter-cli ] && continue
    if [ $# -gt 0 ]; then
      for wanted in "$@"; do [ "$wanted" = "$name" ] && printf '%s\t%s\t%s\n' "$name" "$url" "$commit"; done
    else
      printf '%s\t%s\t%s\n' "$name" "$url" "$commit"
    fi
  done
}

# the pin and the repository of NAME in the manifest as git HEAD has it, or nothing
committed_pin() {
  { git -C "$root" show HEAD:syntax/grammars.tsv 2>/dev/null || true; } | awk -F'\t' -v n="$1" '$1 == n { print $3, $2 }'
}

# a JSON answer of GitHub's API for the path $1, or nothing
github() {
  if command -v gh >/dev/null; then
    gh api "$1" 2>/dev/null || true
  elif [ -n "${GITHUB_TOKEN:-}" ]; then
    curl -sSf -H "Authorization: Bearer $GITHUB_TOKEN" "https://api.github.com/$1" 2>/dev/null || true
  fi
}

# field $2 of the JSON object $1 (a string, a number, true or false), or nothing
field() {
  printf '%s' "$1" | python3 -I -c 'import json,sys
try:
    v = json.load(sys.stdin).get(sys.argv[1])
except Exception:
    v = None
print("" if v is None else (str(v).lower() if isinstance(v, bool) else v))' "$2"
}

# where URL's repository is now (OWNER/NAME as its host has it, empty when the
# host says it is gone, ? when it cannot be asked), and whether it is archived
where_now() {
  local url="$1" path answer
  path="${url#https://*/}"; path="${path%/}"; path="${path%.git}"
  case "$url" in
    https://github.com/*)
      if ! command -v gh >/dev/null && [ -z "${GITHUB_TOKEN:-}" ]; then echo "! ?"; return; fi
      answer=$(github "repos/$path")
      echo "$(field "$answer" full_name) $(field "$answer" archived)" ;;
    https://codeberg.org/*)
      answer=$(curl -sSf "https://codeberg.org/api/v1/repos/$path" 2>/dev/null || true)
      echo "$(field "$answer" full_name) $(field "$answer" archived)" ;;
    https://gitlab.com/*)
      answer=$(curl -sSf "https://gitlab.com/api/v4/projects/${path//\//%2F}" 2>/dev/null || true)
      echo "$(field "$answer" path_with_namespace) $(field "$answer" archived)" ;;
    https://git.sr.ht/*)
      # no API without a token: a repository moved answers with a redirect
      local code target
      read -r code target < <(curl -s -o /dev/null -w '%{http_code} %{redirect_url}\n' "$url" || echo "000 -")
      case "$code" in
        200) echo "$path ?" ;;
        30*) echo "${target#https://git.sr.ht/} ?" ;;
        *) echo " ?" ;;
      esac ;;
    *) echo "? ?" ;;
  esac
}

# fetch the history (commits only, no files) of every branch and tag of URL into $work/NAME.git
fetch_history() {
  local name="$1" url="$2" dir="$work/${1//\//_}.git"
  [ -d "$dir" ] || grammar_git init -q --bare "$dir" || return 1
  grammar_git -C "$dir" fetch -q --filter=tree:0 --prune --force "$url" '+refs/heads/*:refs/heads/*' '+refs/tags/*:refs/tags/*' 2>/dev/null || return 1
  echo "$dir"
}

# the files compiled from, at COMMIT of the repository fetched into DIR, fetched when missing
fetch_files() {
  local dir="$1" url="$2" commit="$3"
  grammar_git -C "$dir" cat-file -e "$commit^{tree}" 2>/dev/null || grammar_git -C "$dir" fetch -q --depth 1 "$url" "$commit" 2>/dev/null
}

while IFS=$'\t' read -r name url commit; do
  printf '%s ' "$name" >&2
  read -r full archived < <(where_now "$url")
  path="${url#https://*/}"; path="${path%/}"; path="${path%.git}"
  if [ "$full" = "!" ]; then
    stops+=("$name: $url not asked about: GitHub is asked through gh or with GITHUB_TOKEN, and there is neither")
  elif [ "$full" = "?" ]; then
    reports+=("$name: $url not asked about (a host this check does not know)")
  elif [ -z "$full" ]; then
    stops+=("$name: $url is gone (its host knows no such repository)")
    continue
  elif [ "${full,,}" != "${path,,}" ]; then
    stops+=("$name: $url is now $full (renamed or moved to another owner): judge the new home, then change the URL")
  fi
  [ "$archived" = true ] && reports+=("$name: $url is archived")
  if ! dir=$(fetch_history "$name" "$url"); then
    stops+=("$name: the history of $url could not be fetched")
    continue
  fi
  if ! grammar_git -C "$dir" cat-file -e "$commit^{commit}" 2>/dev/null; then
    stops+=("$name: the pin ${commit:0:12} is on no branch or tag of $url")
  else
    pin=$(grammar_git -C "$dir" rev-parse "$commit^{commit}")
    on=$(grammar_git -C "$dir" for-each-ref --contains "$pin" --format='%(refname:short)' refs/heads refs/tags | head -3 | paste -sd' ')
    if [ -z "$on" ]; then
      stops+=("$name: the pin ${commit:0:12} is on no branch or tag of $url")
    elif [ -z "$(grammar_git -C "$dir" for-each-ref --contains "$pin" --count=1 refs/heads)" ]; then
      watches+=("$name: the pin ${commit:0:12} is only on a tag ($on), not on a branch, of $url: its history was rewritten since")
    fi
  fi
  newest=$(grammar_git -C "$dir" for-each-ref --sort=-committerdate --count=1 --format='%(committerdate:unix)' refs/heads)
  if [ -n "$newest" ] && [ $(( (now - newest) / 86400 )) -gt "$stale_days" ]; then
    reports+=("$name: $url has had no commit since $(date -u -d "@$newest" +%Y-%m-%d)")
  fi
  # a pin moved since the last commit: how far, and what changed in what is compiled
  read -r old old_url < <(committed_pin "$name"; echo)
  if [ -n "$old" ] && [ "$old" != "$commit" ]; then
    # the old commit from where it was fetched: a grammar may have moved to another repository
    if fetch_files "$dir" "$old_url" "$old" && fetch_files "$dir" "$url" "$commit"; then
      out="$work/${name//\//_}.diff"
      { echo "$name: ${old:0:12} -> ${commit:0:12} (${old_url}${old_url:+ -> }$url)"
        grammar_git -C "$dir" log --oneline --no-decorate "$old..$commit" 2>/dev/null | head -50 || true
        echo
        grammar_git -C "$dir" diff --stat "$old" "$commit"
        echo
        # the scanner and headers in full; the generated parser's size only
        grammar_git -C "$dir" diff "$old" "$commit" -- grammar.js '*.h' '*scanner.c' '*scanner.cc' '*.c' ':!*parser.c'
      } > "$out"
      diffs+=("$name: $out")
    else
      stops+=("$name: the files of ${old:0:12} or ${commit:0:12} could not be fetched to compare")
    fi
  fi
done < <(rows "$@")
echo >&2

if [ ${#reports[@]} -gt 0 ]; then
  echo "Reported (not stopped on):"; printf '  %s\n' "${reports[@]}"
fi
if [ ${#watches[@]} -gt 0 ]; then
  echo "WATCH:"; printf '  %s\n' "${watches[@]}"
fi
if [ ${#diffs[@]} -gt 0 ]; then
  echo "Pins moved since the last commit: read each difference before the bump goes in:"; printf '  %s\n' "${diffs[@]}"
fi
if [ ${#stops[@]} -gt 0 ]; then
  echo "STOP:"; printf '  %s\n' "${stops[@]}"
  exit 1
fi
echo "Nothing stops a bump."
