#!/usr/bin/env bash
# tools/check-grammar-sources.sh --- Check where each grammar comes from, before its pin moves.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run before any pin of syntax/grammars.tsv moves, and at each period-check.
# For each repository the manifest names (tree-sitter, every grammar, and
# what a grammar's grammar.js requires), it asks:
#   - whether the repository is still where the manifest says: its host's
#     answer for the owner and name (GitHub's, Codeberg's, GitLab's API; a
#     redirect for git.sr.ht; for tangled.org, the records of the owner and
#     of the repository, under their DIDs). A repository renamed, moved to
#     another owner or gone STOPS the check: the new home is judged first
#     (who keeps it, whether it is kept up), and the manifest changed by
#     hand; on tangled.org, a DID changed (its keys, its server) since the
#     day syntax/grammar-reviews.tsv gives for the grammar is to be reviewed
#     (WATCH);
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

# field $2 of the JSON object $1 (a string, a number, true or false), or
# nothing; $2 may go down into objects and lists: value.repoDid, alsoKnownAs.0
field() {
  printf '%s' "$1" | python3 -I -c 'import json,sys
try:
    v = json.load(sys.stdin)
    for key in sys.argv[1].split("."):
        v = v[int(key)] if isinstance(v, list) else v.get(key)
except Exception:
    v = None
print("" if v is None or isinstance(v, (dict, list)) else (str(v).lower() if isinstance(v, bool) else v))' "$2"
}

# the endpoint of the service $2 (#atproto_pds) in the DID document $1, or nothing
did_service() {
  printf '%s' "$1" | python3 -I -c 'import json,sys
try:
    print(next(s["serviceEndpoint"] for s in json.load(sys.stdin)["service"] if s["id"] == sys.argv[1]))
except Exception:
    pass' "$2"
}

# what curl fetches from $1 over HTTPS alone, redirects too, or nothing
https_get() {
  curl -sS --proto =https --proto-redir =https --max-time 30 "$1" 2>/dev/null || true
}

# the DID document of the did:plc: DID $1 from the PLC directory, or nothing
# when it cannot be had or is another DID's
did_document() {
  local document
  case "$1" in
    did:plc:*) document=$(https_get "https://plc.directory/$1") ;;
    *) return ;;
  esac
  [ "$(field "$document" id)" = "$1" ] && printf '%s' "$document"
}

# the days (YYYY-MM-DD) on which the did:plc: DID $1 was changed after the day
# $2 (its keys or the servers it names), one a line; "?" when its history
# cannot be had
did_changes_after() {
  https_get "https://plc.directory/$1/log/audit" | python3 -I -c 'import json,sys
try:
    days = sorted({entry["createdAt"][:10] for entry in json.load(sys.stdin)})
except Exception:
    print("?"); sys.exit()
print("\n".join(day for day in days if day > sys.argv[1]))' "$2"
}

# the day the source of grammar $1 was last reviewed by hand, from
# syntax/grammar-reviews.tsv, or nothing
reviewed_on() {
  awk -F'\t' -v n="$1" '!/^#/ && $1 == n { print $2 }' "$root/syntax/grammar-reviews.tsv" 2>/dev/null
}

# the owner's record (sh.tangled.repo) of the repository NAME on the server
# PDS of the owner OWNER: the one keyed NAME, else one whose name is NAME
# among all the owner's records; "none" when there is none, nothing when it
# cannot be told
tangled_record() {
  local owner="$1" name="$2" pds="$3" record cursor="" page pages=0
  record=$(https_get "$pds/xrpc/com.atproto.repo.getRecord?repo=$owner&collection=sh.tangled.repo&rkey=$name")
  if [ -n "$(field "$record" value.knot)" ]; then printf '%s' "$record"; return; fi
  [ "$(field "$record" error)" = RecordNotFound ] || return
  while [ $pages -lt 50 ]; do
    page=$(https_get "$pds/xrpc/com.atproto.repo.listRecords?repo=$owner&collection=sh.tangled.repo&limit=100${cursor:+&cursor=$cursor}")
    record=$(printf '%s' "$page" | python3 -I -c 'import json,sys
try:
    records = json.load(sys.stdin)["records"]
except Exception:
    print("?"); sys.exit()
for r in records:
    if r.get("value", {}).get("name") == sys.argv[1]:
        print(json.dumps(r)); break' "$name")
    [ "$record" = "?" ] && return
    if [ -n "$record" ]; then printf '%s' "$record"; return; fi
    cursor=$(field "$page" cursor)
    [ -z "$cursor" ] && { echo none; return; }
    pages=$((pages + 1))
  done
}

# where the repository OWNER/NAME of tangled.org, grammar GRAMMAR, is now, as
# where_now says it, followed by a line "watch WHAT" for each thing to
# review. tangled.org keeps a repository as a record of its owner's, under
# the owner's DID, which stays the same through every change of the owner's
# handle: the manifest names the owner by that DID (a did:plc:; a handle or
# a did:web: names a domain, which can change hands), and the repository is
# where the owner's record of it says it is. A repository with a DID of its
# own declares its owner and name too, and when those differ it has moved;
# tangled.org must send git to the server those records name. When any of
# this cannot be asked, it cannot be told who keeps the repository, and the
# check stops. A change to either DID (its keys, its server) after the day
# the grammar's source was last reviewed is to be reviewed.
tangled_where() {
  local owner="${1%%/*}" name="${1#*/}" grammar="$2" document pds record repo_did repo_document knot manifest sent reviewed did day
  case "$owner" in
    did:plc:*) ;;
    *) echo "! ? $owner names a domain, which can change hands: name the owner by its did:plc: DID (tangled.org/did:plc:.../$name)"; return ;;
  esac
  document=$(did_document "$owner")
  pds=$(did_service "$document" "#atproto_pds")
  if [ -z "$pds" ]; then echo "! ? the DID document of $owner, who keeps it, could not be read"; return; fi
  record=$(tangled_record "$owner" "$name" "$pds")
  if [ "$record" = none ]; then echo "- ?"; return; fi
  if [ -z "$record" ]; then echo "! ? no record of $owner's is keyed $name or named so, as far as $pds could be asked"; return; fi
  repo_did=$(field "$record" value.repoDid)
  knot=$(field "$record" value.knot)
  if [ -n "$repo_did" ]; then
    case "$repo_did" in
      did:plc:*) ;;
      *) echo "! ? the repository is $repo_did, which names a domain that can change hands"; return ;;
    esac
    repo_document=$(did_document "$repo_did")
    manifest=$(https_get "$(did_service "$repo_document" "#atproto_pds")/xrpc/com.atproto.repo.getRecord?repo=$repo_did&collection=org.tangled.repo.manifest&rkey=self")
    if [ -z "$(field "$manifest" value.declaration.owner)" ]; then
      echo "! ? the repository $repo_did of $owner's record could not be asked who keeps it"; return
    fi
  fi
  # where tangled.org sends git
  sent=$(curl -s --proto =https --max-time 30 -o /dev/null -w '%{redirect_url}' "https://tangled.org/$owner/$name/info/refs?service=git-upload-pack" 2>/dev/null || true)
  case "$sent" in
    "https://$knot/${repo_did:-$owner/$name}/"*) ;;
    *) echo "! ? tangled.org sends git to ${sent:-nowhere}, not to $knot, the server the records name"; return ;;
  esac
  if [ -n "$repo_did" ]; then
    echo "$(field "$manifest" value.declaration.owner)/$(field "$manifest" value.declaration.name) ?"
  else
    echo "$owner/$name ?"
  fi
  reviewed=$(reviewed_on "$grammar")
  if [ -z "$reviewed" ]; then
    echo "watch no day of review for it in syntax/grammar-reviews.tsv: review who keeps it, then write the day there"
    return
  fi
  for did in "$owner" $repo_did; do
    while read -r day; do
      [ -z "$day" ] && continue
      if [ "$day" = "?" ]; then echo "watch the history of $did could not be read"; continue; fi
      echo "watch $did was changed on $day (its keys or its server), after the review of $reviewed: review it, then the day in syntax/grammar-reviews.tsv"
    done < <(did_changes_after "$did" "$reviewed")
  done
}

# where URL's repository, of grammar $2, is now: OWNER/NAME as its host has
# it (- when the host says it is gone, ? when it is a host this check does
# not know, ! when who keeps it cannot be told, followed by why), and
# whether it is archived; then a line "watch WHAT" for each thing to review
where_now() {
  local url="$1" path answer
  path="${url#https://*/}"; path="${path%/}"; path="${path%.git}"
  case "$url" in
    https://github.com/*)
      if ! command -v gh >/dev/null && [ -z "${GITHUB_TOKEN:-}" ]; then
        echo "! ? GitHub is asked through gh or with GITHUB_TOKEN, and there is neither"; return
      fi
      answer=$(github "repos/$path")
      echo "$(gone_or "$(field "$answer" full_name)") $(field "$answer" archived)" ;;
    https://codeberg.org/*)
      answer=$(curl -sSf "https://codeberg.org/api/v1/repos/$path" 2>/dev/null || true)
      echo "$(gone_or "$(field "$answer" full_name)") $(field "$answer" archived)" ;;
    https://gitlab.com/*)
      answer=$(curl -sSf "https://gitlab.com/api/v4/projects/${path//\//%2F}" 2>/dev/null || true)
      echo "$(gone_or "$(field "$answer" path_with_namespace)") $(field "$answer" archived)" ;;
    https://git.sr.ht/*)
      # no API without a token: a repository moved answers with a redirect
      local code target
      read -r code target < <(curl -s -o /dev/null -w '%{http_code} %{redirect_url}\n' "$url" || echo "000 -")
      case "$code" in
        200) echo "$path ?" ;;
        30*) echo "${target#https://git.sr.ht/} ?" ;;
        *) echo "- ?" ;;
      esac ;;
    https://tangled.org/*) tangled_where "$path" "$2" ;;
    *) echo "? ?" ;;
  esac
}

# $1, or - when it is empty
gone_or() {
  echo "${1:--}"
}

# fetch the history (commits only, no files) of every branch and tag of URL into $work/NAME.git
fetch_history() {
  local name="$1" url="$2" dir="$work/${1//\//_}.git"
  [ -d "$dir" ] || grammar_git init -q --bare "$dir" || return 1
  grammar_git -C "$dir" fetch -q --filter=tree:0 --prune --force "$url" '+refs/heads/*:refs/heads/*' '+refs/tags/*:refs/tags/*' 2>/dev/null || return 1
  echo "$dir"
}

# the files of COMMIT of the repository at URL, fetched when missing into
# DIR, a repository of whole commits beside the one of histories alone
fetch_files() {
  local dir="$1" url="$2" commit="$3"
  [ -d "$dir" ] || grammar_git init -q --bare "$dir" || return 1
  grammar_git -C "$dir" cat-file -e "$commit^{tree}" 2>/dev/null ||
    grammar_git -C "$dir" fetch -q --depth 1 "$url" "$commit" 2>/dev/null
}

while IFS=$'\t' read -r name url commit; do
  printf '%s ' "$name" >&2
  answer=$(where_now "$url" "$name")
  read -r full archived why <<< "${answer%%$'\n'*}"
  while read -r word line; do
    [ "$word" = watch ] && watches+=("$name: $line")
  done <<< "$answer"
  path="${url#https://*/}"; path="${path%/}"; path="${path%.git}"
  if [ "$full" = "!" ]; then
    stops+=("$name: who keeps $url cannot be told: $why")
  elif [ "$full" = "?" ]; then
    reports+=("$name: $url not asked about (a host this check does not know)")
  elif [ "$full" = - ]; then
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
    files="$work/${name//\//_}.files.git"
    if fetch_files "$files" "$old_url" "$old" && fetch_files "$files" "$url" "$commit"; then
      out="$work/${name//\//_}.diff"
      { echo "$name: ${old:0:12} -> ${commit:0:12} (${old_url}${old_url:+ -> }$url)"
        # the commits between, where the history has both
        grammar_git -C "$dir" log --oneline --no-decorate "$old..$commit" 2>/dev/null | head -50 || true
        echo
        grammar_git -C "$files" diff --stat "$old" "$commit"
        echo
        # the scanner and headers in full; the generated parser's size only
        grammar_git -C "$files" diff "$old" "$commit" -- grammar.js '*.h' '*scanner.c' '*scanner.cc' '*.c' ':!*parser.c'
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
