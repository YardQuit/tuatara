# tools/inside.sh --- Run a command where Tuatara is built, say when build/tuatara is to be built again, and find
# this machine's names in a program: sourced by the other scripts.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# inside COMMAND...  runs COMMAND in the toolbox container named by
# $TUATARA_CONTAINER (tuatara by default, made by tools/toolbox-setup.sh) when
# that container exists and this is not already inside a container; else here,
# with SBCL, the C compilers and the libraries installed on this system. When
# it runs here and the program COMMAND runs (after env and its NAME=VALUE
# words) is not installed, it says so and where to find what is needed, and
# returns 127, as the shell does.
inside() {
  local name="${TUATARA_CONTAINER:-tuatara}"
  # grep -c reads to the end: -q would stop at the first match, and under the
  # callers' pipefail toolbox's broken pipe would fail the test
  if [ ! -e /run/.containerenv ] && command -v toolbox >/dev/null &&
     [ "$(toolbox list --containers 2>/dev/null | grep -c "[[:space:]]${name}[[:space:]]")" != 0 ]; then
    toolbox run --container "$name" "$@"
  else
    local program word
    for word in "$@"; do
      case "$word" in
        env|*=*) ;;
        *) program="$word"; break ;;
      esac
    done
    if [ -n "${program:-}" ] && ! command -v "$program" >/dev/null; then
      echo "No $program on this system, and no toolbox called $name to run it in." >&2
      echo "Install what Tuatara's HACKING.org lists under \"What you need\" (SBCL, Quicklisp," >&2
      echo "a C compiler, git and make), or on Fedora Atomic make the toolbox with" >&2
      echo "tools/toolbox-setup.sh." >&2
      return 127
    fi
    "$@"
  fi
}
# image_stale ROOT: true when ROOT/build/tuatara is not there, or older than a
# source it is made from: the Lisp, the grammars' list and patches and syntax.c,
# and what data/ puts into it (the starter init file, the LaTeX templates)
image_stale() {
  local image="$1/build/tuatara"
  [ ! -x "$image" ] ||
    [ -n "$(find "$1/tuatara.asd" "$1/src" "$1/lisp" "$1/frontends" "$1/syntax" "$1/data" -newer "$image" \
              \( -name '*.lisp' -o -name '*.asd' -o -name '*.scm' -o -name '*.c' -o -name '*.tsv' -o -name '*.patch' -o -name '*.tex' -o -name '*.txt' \) -print -quit)" ]
}
# names_this_machine FILE: true, and where (the octet each is found at, with
# the text around it), when FILE names this machine's home directory ($HOME, or
# the directory it leads to), the account's name ($USER) or the machine's host
# name; false when it names none of them. A host name is looked for as a whole
# word (a host called kiwi-box is not found in "kiwi-boxes"). One of letters
# alone may be a word of any text (a host called program, in a licence's
# "a new program"), so it is looked for only where a machine writes a host
# name: after @ (annt@program) or // (//program/). Names under three
# characters, and localhost, are not looked for.
names_this_machine() {
  local home real user name found=1 match pattern
  home="${HOME%/}"; real="$(realpath -m "$home")"; user="${USER:-$(id -un)}"
  for name in "$home" "$real" "$user" "$(hostname 2>/dev/null)" "$(hostname -f 2>/dev/null)"; do
    [ "${#name}" -ge 3 ] && [ "$name" != localhost ] || continue
    match=(-F); pattern="$name"
    if [ "$name" != "$home" ] && [ "$name" != "$real" ] && [ "$name" != "$user" ]; then
      match=(-F -w)
      if [[ "$name" =~ ^[[:alpha:]]+$ ]]; then match=(-E); pattern="(@|//)$name([^[:alnum:]_]|\$)"; fi
    fi
    if [ "$(grep -a -c "${match[@]}" -- "$pattern" "$1")" != 0 ]; then
      echo "$1 names $name $(grep -a -c "${match[@]}" -- "$pattern" "$1") times, at:" >&2
      grep -a -o -b "${match[@]}" -- "$pattern" "$1" | head -n 10 | while IFS=: read -r at _; do
        echo "  $at: $(dd if="$1" bs=1 skip=$((at > 40 ? at - 40 : 0)) count=120 status=none | tr -c '[:print:]' '.')" >&2
      done
      found=0
    fi
  done
  return $found
}
# in_tree [NAME=VALUE...] SYSTEMS FORM...  run inside, in the current
# directory's tree and no other one Quicklisp or ASDF knows of, an SBCL with
# the Lisp expression SYSTEMS (a system's name, or a list of them) loaded, then
# each FORM evaluated in turn. tools/fence.lisp is loaded, not called: a FORM
# that runs the editor calls (tuatara.fence:fence-person-data nil) first, and
# the tests fence themselves as they load. Ends with exit 1 and no form run
# when the tuatara system loaded is not of this tree. The NAME=VALUE words are
# put in the environment of the Lisp.
in_tree() {
  local environment=() args=() form
  while [[ "${1:-}" == [A-Z_]*=* ]]; do environment+=("$1"); shift; done
  local systems="$1" tree="$PWD/"
  shift
  for form in "$@"; do args+=(--eval "$form"); done
  inside env ${environment[@]+"${environment[@]}"} sbcl --dynamic-space-size 4GB --non-interactive \
    --load tools/quicklisp.lisp \
    --eval "(push #p\"$tree\" asdf:*central-registry*)" \
    --eval "(ql:quickload $systems :silent t)" \
    --load tools/fence.lisp \
    --eval "(unless (let ((loaded (asdf:system-source-directory :tuatara)))
                      (and loaded (uiop:pathname-equal (truename loaded) (truename #p\"$tree\"))))
              (format *error-output* \"~&The tree loaded is ~a, not $tree: nothing done.~%\"
                      (asdf:system-source-directory :tuatara))
              (uiop:quit 1))" \
    ${args[@]+"${args[@]}"}
}
