#!/usr/bin/env bash
# tools/check-updates.sh --- Report the pinned dependencies that have newer versions.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Compare what Tuatara is built with against what is published upstream, and
# write the result to build/bumpcheck.org:
#   - tree-sitter and each grammar of syntax/grammars.tsv: the pinned commit
#     against the repository's newest commit and newest version tag, and the
#     tree-sitter program's release against the newest;
#   - Ghostty and Zig of lisp/terminal/library.pin, and Ghostty's security
#     advisories published since the pinned commit whose fix it does not
#     hold (asked of GitHub's API, by gh or without a token);
#   - the Quicklisp release installed against the newest;
#   - the SBCL build/tuatara was saved with against the newest release;
#   - what tools/appimage.sh uses: appimagetool, the AppImage runtime and the
#     list of libraries left out;
#   - the Ubuntu, Fedora and Arch images of tools/containers.sh, whose digest
#     changes with each rebuilt image (security updates), asked of the
#     registry by skopeo.
# Nothing is changed: a pin is moved by hand, a grammar's only after its
# colouring rules are checked against the new version and
# tools/fuzz-grammars.sh passes for it. Only names and versions are asked of
# the servers (git ls-remote, the Quicklisp release list, the image's
# digest, the advisories). When the list of newer versions or advisories
# differs from the last report, the desktop shows a notice.
#
# tools/check-updates.sh --install runs it once a week from now on: writes a
# systemd user service and timer (tuatara-check-updates) under
# ~/.config/systemd/user/ and starts the timer; a week missed while the
# computer was off is caught up at the next start. Run it again after moving
# the project. --remove stops the timer and takes both away.
# Usage: tools/check-updates.sh [--install | --remove]
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
units="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"
unit=tuatara-check-updates

case "${1:-}" in
  --install)
    mkdir -p "$units"
    cat > "$units/$unit.service" <<EOF
[Unit]
Description=Report Tuatara's pinned dependencies that have newer versions

[Service]
Type=oneshot
ExecStart="$root/tools/check-updates.sh"
EOF
    cat > "$units/$unit.timer" <<EOF
[Unit]
Description=Check Tuatara's dependencies for newer versions once a week

[Timer]
OnCalendar=weekly
Persistent=true
RandomizedDelaySec=1h

[Install]
WantedBy=timers.target
EOF
    systemctl --user daemon-reload
    systemctl --user enable --now "$unit.timer"
    echo "Installed $units/$unit.{service,timer}; next check:"
    systemctl --user list-timers "$unit.timer" --no-pager | sed -n 2p
    exit 0 ;;
  --remove)
    systemctl --user disable --now "$unit.timer" 2>/dev/null || true
    rm -f "$units/$unit.service" "$units/$unit.timer"
    systemctl --user daemon-reload
    echo "Removed $unit.{service,timer}"
    exit 0 ;;
  "") ;;
  *) echo "Usage: tools/check-updates.sh [--install | --remove]" >&2; exit 1 ;;
esac

report="$root/build/bumpcheck.org"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# the newest version tag among the refs on standard input (v1.2.3, 1.2.3, ...)
newest_tag() {
  sed -n 's|^[0-9a-f]*\trefs/tags/\([^^]*\)$|\1|p' | { grep -E '^v?[0-9]+(\.[0-9]+)+$' || true; } | sort -V | tail -1
}
# the newest release named by its date (20251108) among the refs on standard input
newest_date_tag() {
  sed -n 's|^[0-9a-f]*\trefs/tags/\([0-9]\{8\}\)$|\1|p' | sort | tail -1
}
# one line NAME<TAB>PINNED<TAB>NEWEST for repository $2 pinned at commit $3, or
# nothing when the pin is its newest commit
check_repository() {
  local name="$1" repository="$2" commit="$3" refs head tag pinned
  if ! refs="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote "$repository" 2>/dev/null)"; then
    printf '%s\t%s\t%s\n' "$name" "${commit:0:12}" "(not reachable)"
    return
  fi
  head="$(awk '$2 == "HEAD" { print $1 }' <<<"$refs")"
  [ "$head" = "$commit" ] && return
  tag="$(newest_tag <<<"$refs")"
  # the pin by its tag where one names it
  pinned="$(awk -v c="$commit" '$1 == c && $2 ~ /^refs\/tags\// { sub(/^refs\/tags\//, "", $2); sub(/\^\{\}$/, "", $2); print $2; exit }' <<<"$refs")"
  printf '%s\t%s\t%s\n' "$name" "${pinned:-${commit:0:12}}" "${head:0:12}${tag:+ (newest tag $tag)}"
}
export -f check_repository newest_tag

# the grammars and tree-sitter, eight at a time; one line per repository and commit
grep -v '^#' "$root/syntax/grammars.tsv" | awk -F'\t' 'NF >= 4 && $1 != "tree-sitter-cli" && $3 ~ /^[0-9a-f]{40}$/ && !seen[$2 "\t" $3]++ { print $1 "\t" $2 "\t" $3 }' |
  xargs -P 8 -d '\n' -n 1 bash -c 'IFS=$'"'"'\t'"'"' read -r n r c <<<"$1"; check_repository "$n" "$r" "$c"' _ |
  sort > "$work/grammars"

# the tree-sitter program: its release, from the address it is fetched from
cli_url="$(awk -F'\t' '$1 == "tree-sitter-cli" { print $2 }' "$root/syntax/grammars.tsv")"
cli_pinned="$(sed -n 's|.*/releases/download/\([^/]*\)/.*|\1|p' <<<"$cli_url")"
cli_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/tree-sitter/tree-sitter 2>/dev/null | newest_tag || true)"

pin="$root/lisp/terminal/library.pin"
field() { awk -F'\t' -v k="$1" '$1 == k { print $2 }' "$pin"; }
check_repository ghostty "$(field repository)" "$(field commit)" > "$work/ghostty"
zig_pinned="$(field zig)"
zig_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://codeberg.org/ziglang/zig 2>/dev/null | newest_tag || true)"

# GitHub's REST API, read-only: by gh when it answers (installed and logged
# in), else by curl without a token; nothing written when neither answers
github_api() {
  local answer
  if command -v gh >/dev/null && answer="$(timeout 60 gh api "$1" 2>/dev/null)"; then
    printf '%s\n' "$answer"
  else
    curl -fsS --max-time 60 -H 'Accept: application/vnd.github+json' "https://api.github.com/$1" 2>/dev/null
  fi
}
# whether commit or tag $2 of GitHub repository $1 holds commit or tag $3:
# true when it does, 1 when it does not, 2 when GitHub did not say
github_holds() {
  case "$(github_api "repos/$1/compare/$3...$2" | jq -r 'if type == "object" then .status else empty end' 2>/dev/null)" in
    ahead | identical) return 0 ;;
    behind | diverged) return 1 ;;
    *) return 2 ;;
  esac
}
# every advisory of GitHub repository $1 as one JSON array: by gh's own
# paging, else page after page as each answer's Link names the next;
# nothing written when a page cannot be read
github_advisories() {
  local path="repos/$1/security-advisories?per_page=100" got all='[]' next
  if command -v gh >/dev/null && got="$(timeout 300 gh api --paginate "$path" 2>/dev/null)" &&
     all="$(jq -se 'if all(.[]; type == "array") then add else empty end' <<<"$got" 2>/dev/null)"; then
    echo "$all"
    return 0
  fi
  all='[]'
  next="https://api.github.com/$path"
  while [ -n "$next" ]; do
    got="$(curl -fsS --max-time 60 -D "$work/headers" -H 'Accept: application/vnd.github+json' "$next" 2>/dev/null)" || return 1
    jq -e 'type == "array"' >/dev/null 2>&1 <<<"$got" || return 1
    all="$(jq -s 'add' <<<"$all $got")"
    next="$(tr -d '\r' < "$work/headers" | sed -n 's/^[Ll]ink:.*<\([^>]*\)>; rel="next".*/\1/p')"
  done
  echo "$all"
}
# Ghostty's security advisories published since the pinned commit whose fix
# the pin does not hold, a line NAME<TAB>SEVERITY<TAB>FIX<TAB>WHAT for each,
# FIX "not fixed", or "not checked" when GitHub did not say whether the pin
# holds it (the fields' | written as Org's \vert{}); the fix is what the
# advisory names as patched: a commit, or a version by its tag
ghostty_slug="$(field repository | sed -n 's|^https://github.com/\([^/]*/[^/]*\)$|\1|p' | sed 's|\.git$||')"
none_read() { printf '%s\t\t\t%s\n' "(none read)" "$1" > "$work/advisories"; }
if ! command -v jq >/dev/null; then
  none_read "jq is not installed"
elif [ -z "$ghostty_slug" ]; then
  none_read "the repository is not on GitHub"
elif ! pin_date="$(github_api "repos/$ghostty_slug/commits/$(field commit)" |
                   jq -er 'if type == "object" then .commit.committer.date else empty end' 2>/dev/null)" ||
     ! advisories="$(github_advisories "$ghostty_slug")"; then
  none_read "GitHub did not answer: not reachable, or its hourly limit for asking without a token reached"
else
  # fields apart by the unit separator, which read does not run together as it does tabs
  jq -r --arg since "$pin_date" '.[] | select((.published_at // "") > $since)
    | [.ghsa_id // "", .severity // "",
       ([.vulnerabilities[]?.package.name // empty] | unique | join(", ")) + ": " + (.summary // ""),
       ([.vulnerabilities[]?.patched_versions // empty] | join(" "))]
    | map(gsub("[\t\n\r\u001f]"; " ") | gsub("\\|"; "\\vert{}")) | join("\u001f")' <<<"$advisories" |
    while IFS=$'\x1f' read -r id severity what patched; do
      fixed="not fixed"
      for fix in $(grep -oE '\bv?[0-9]+\.[0-9]+\.[0-9]+\b|\b[0-9a-f]{7,40}\b' <<<"$patched"); do
        [[ "$fix" =~ ^[0-9] && "$fix" == *.* ]] && fix="v$fix"
        if github_holds "$ghostty_slug" "$(field commit)" "$fix"; then fixed=""; break
        elif [ $? = 2 ]; then fixed="not checked"; fi
      done
      [ -z "$fixed" ] || printf '%s\t%s\t%s\t%s\n' "$id" "$severity" "$fixed" "$what"
    done > "$work/advisories"
fi

# where tools/quicklisp.lisp finds Quicklisp
for ql_dist in "$(dirname "${QUICKLISP_SETUP:-/nonexistent/setup.lisp}")" "$HOME/quicklisp" "$HOME/.local/share/quicklisp"; do
  ql_dist="$ql_dist/dists/quicklisp/distinfo.txt"
  [ -f "$ql_dist" ] && break
done
ql_pinned="$(sed -n 's/^version: //p' "$ql_dist" 2>/dev/null || true)"
ql_newest="$(curl -fsS --max-time 60 https://beta.quicklisp.org/dist/quicklisp.txt 2>/dev/null | sed -n 's/^version: //p' || true)"

sbcl_pinned="$("$root/build/tuatara" --version 2>/dev/null | sed -n 's/.* on SBCL \([0-9.]*\).*/\1/p' || true)"
sbcl_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/sbcl/sbcl 2>/dev/null |
  sed -n 's|^[0-9a-f]*\trefs/tags/sbcl-\([0-9.]*\)$|\1|p' | sort -V | tail -1 || true)"

# what tools/appimage.sh builds the AppImage with: appimagetool's release, the
# runtime's, the commit of the list of libraries left out, and the Ubuntu image, by its digest
appimage_field() { sed -n "s|^$1=||p" "$root/tools/appimage.sh"; }
appimagetool_pinned="$(appimage_field appimagetool_url | sed -n 's|.*/releases/download/\([^/]*\)/.*|\1|p')"
appimagetool_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/AppImage/appimagetool 2>/dev/null | newest_tag || true)"
runtime_pinned="$(appimage_field runtime_url | sed -n 's|.*/releases/download/\([^/]*\)/.*|\1|p')"
runtime_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/AppImage/type2-runtime 2>/dev/null | newest_date_tag || true)"
excludelist_commit="$(appimage_field excludelist_url | sed -n 's|.*/pkg2appimage/\([0-9a-f]\{40\}\)/.*|\1|p')"
check_repository "AppImage exclude list" https://github.com/AppImageCommunity/pkg2appimage "$excludelist_commit" > "$work/excludelist"
# the images packages are made in, each a line as version_line writes
. "$root/tools/containers.sh"
for image in "$ubuntu_image" "$fedora_image" "$arch_image"; do
  pinned="${image##*@sha256:}"
  if ! command -v skopeo >/dev/null; then
    printf '%s\t%s\t%s\n' "${image%@*} image" "${pinned:0:12}" "(skopeo not installed)"
    continue
  fi
  newest="$(timeout 60 skopeo inspect --raw "docker://${image%@*}" 2>/dev/null | sha256sum | cut -d' ' -f1 || true)"
  if [ -z "$newest" ] || [ "$newest" = "$(printf '' | sha256sum | cut -d' ' -f1)" ]; then
    printf '%s\t%s\t%s\n' "${image%@*} image" "${pinned:0:12}" "(not reachable)"
  elif [ "$newest" != "$pinned" ]; then
    printf '%s\t%s\t%s\n' "${image%@*} image" "${pinned:0:12}" "${newest:0:12}"
  fi
done > "$work/images"

# one line NAME<TAB>PINNED<TAB>NEWEST when NEWEST is a later version than PINNED
version_line() {
  if [ -z "$3" ]; then printf '%s\t%s\t%s\n' "$1" "${2:-?}" "(not reachable)"
  elif [ "${2#v}" != "${3#v}" ] && [ "$(printf '%s\n%s\n' "${2#v}" "${3#v}" | sort -V | tail -1)" = "${3#v}" ]; then printf '%s\t%s\t%s\n' "$1" "${2:-?}" "$3"; fi
}
{
  version_line "tree-sitter program" "$cli_pinned" "$cli_newest"
  cat "$work/ghostty"
  version_line zig "$zig_pinned" "$zig_newest"
  version_line quicklisp "$ql_pinned" "$ql_newest"
  version_line sbcl "$sbcl_pinned" "$sbcl_newest"
  version_line appimagetool "$appimagetool_pinned" "$appimagetool_newest"
  version_line "AppImage runtime" "$runtime_pinned" "$runtime_newest"
  cat "$work/excludelist"
  cat "$work/images"
} > "$work/tools"

table() { awk -F'\t' '{ printf "|"; for (i = 1; i <= NF; i++) printf " %s |", $i; printf "\n" }' "$1"; }
grammars_newer="$(wc -l < "$work/grammars")"
{
  echo "#+title: Newer versions of Tuatara's dependencies"
  echo "#+date: $(date '+%Y-%m-%d %H:%M')"
  echo
  echo "Written by tools/check-updates.sh. Nothing has been changed: a pin is moved"
  echo "by hand, a grammar's only after its rules are checked and"
  echo "tools/fuzz-grammars.sh passes."
  echo
  echo "* Ghostty security advisories the pin does not fix"
  if [ -s "$work/advisories" ]; then
    echo "| name | severity | fix | what |"; echo "|-"; table "$work/advisories"
  else
    echo "None published since the pinned commit, or the pin holds their fixes."
  fi
  echo
  echo "* Libraries and programs"
  if [ -s "$work/tools" ]; then
    echo "| name | built with | newest |"; echo "|-"; table "$work/tools"
  else
    echo "All are the newest."
  fi
  echo
  echo "* Grammars ($grammars_newer of $(grep -v '^#' "$root/syntax/grammars.tsv" | awk -F'\t' '$3 ~ /^[0-9a-f]{40}$/' | wc -l) with newer commits)"
  if [ -s "$work/grammars" ]; then
    echo "| name | pinned | newest |"; echo "|-"; table "$work/grammars"
  fi
} > "$work/report"

# a notice only when the newer versions are not those of the last report
summary() { sed -n '/^| name |/d; /^| /p' "$1" 2>/dev/null | sha256sum; }
changed=1
[ -f "$report" ] && [ "$(summary "$report")" = "$(summary "$work/report")" ] && changed=0
mkdir -p "$(dirname "$report")"
cp "$work/report" "$report"
tools_newer="$(wc -l < "$work/tools")"
advisories_open="$(awk -F'\t' '$3 == "not fixed"' "$work/advisories" | wc -l)"
advisories_unchecked="$(awk -F'\t' '$3 == "not checked"' "$work/advisories" | wc -l)"
message="$tools_newer libraries or programs and $grammars_newer grammars have newer versions"
[ "$advisories_unchecked" -gt 0 ] && message="$advisories_unchecked Ghostty security advisories not checked against the pin; $message"
[ "$advisories_open" -gt 0 ] && message="$advisories_open Ghostty security advisories not fixed in the pin; $message"
echo "$message; see $report"
if [ "$changed" = 1 ] && [ $((tools_newer + grammars_newer + advisories_open + advisories_unchecked)) -gt 0 ] && command -v notify-send >/dev/null; then
  notify-send -a Tuatara "Tuatara: newer dependencies" "$message. See build/bumpcheck.org." || true
fi
