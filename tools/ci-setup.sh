#!/usr/bin/env bash
# tools/ci-setup.sh --- Install what a release is built with, on a new Ubuntu 24.04 machine.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# For .github/workflows/release.yml, on GitHub's machines; on your own, see
# HACKING.org instead. Installs, each pinned and checked by its SHA-256 before
# it is used:
#   - SBCL, its release from sbcl.org, in /usr/local;
#   - Zig, which builds the terminal library, in /opt/zig (on $GITHUB_PATH);
#   - Quicklisp in ~/quicklisp: its client, the Quicklisp set the release is
#     built from, and the archives of the libraries Tuatara is built with,
#     each as tools/lisp-archives.sha256 lists it. Quicklisp is then told to
#     download nothing, so a library new since that list stops the build
#     rather than being fetched unchecked.
# Everything comes over HTTPS. Usage: tools/ci-setup.sh
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

sbcl_url=https://downloads.sourceforge.net/project/sbcl/sbcl/2.6.8/sbcl-2.6.8-x86-64-linux-binary.tar.bz2
sbcl_sha256=5391773774b94554a015db9f992370d06937fb6f0cdb0b2142281aebef9e96c1
zig_url=https://ziglang.org/download/0.17.0/zig-x86_64-linux-0.17.0.tar.xz
zig_sha256=1cbe9df9f27e6b78d14ccbca43b6703a404ef79ef1c463de901d7f088d4e2026
# the Quicklisp client, as client-info.sexp of its version 2021-02-13 names it
ql_client_url=https://beta.quicklisp.org/client/2021-02-13/quicklisp.tar
ql_client_sha256=a8a3c8c91b51dd185175abad4d7c3999ebb4e2520be5a6cee2127035ac6c87be
ql_setup_url=https://beta.quicklisp.org/client/2021-02-11/setup.lisp
ql_setup_sha256=549fe3e7e0f2669daede98437c99cd60e02c0b8536d3d135c9aa9d346ed951b6
ql_asdf_url=https://beta.quicklisp.org/asdf/3.2.1/asdf.lisp
ql_asdf_sha256=51912f3f7c2c62c204f515d97346d56011528399bbf0a76a123318343ebd8bf0

fetch() {  # download $1 to $2, kept only when its SHA-256 is $3
  curl -sSfL --retry 3 -o "$2.part" "$1"
  if ! echo "$3  $2.part" | sha256sum -c --status; then
    rm -f "$2.part"
    echo "$1 is not the file expected: not used" >&2
    exit 1
  fi
  mv "$2.part" "$2"
}

echo "== SBCL"
fetch "$sbcl_url" "$work/sbcl.tar.bz2" "$sbcl_sha256"
tar -xjf "$work/sbcl.tar.bz2" -C "$work"
(cd "$work"/sbcl-*-linux && sudo INSTALL_ROOT=/usr/local sh install.sh >/dev/null)
sbcl --version

echo "== Zig"
fetch "$zig_url" "$work/zig.tar.xz" "$zig_sha256"
sudo mkdir -p /opt/zig
sudo tar -xJf "$work/zig.tar.xz" -C /opt/zig --strip-components=1
[ -n "${GITHUB_PATH:-}" ] && echo /opt/zig >> "$GITHUB_PATH"
/opt/zig/zig version

echo "== Quicklisp"
ql="$HOME/quicklisp"
dist="$ql/dists/quicklisp"
mkdir -p "$dist/archives" "$ql/config" "$ql/local-projects"
fetch "$ql_client_url" "$work/quicklisp.tar" "$ql_client_sha256"
tar -xf "$work/quicklisp.tar" -C "$ql"
fetch "$ql_setup_url" "$ql/setup.lisp" "$ql_setup_sha256"
fetch "$ql_asdf_url" "$ql/asdf.lisp" "$ql_asdf_sha256"
"$root/tools/lisp-archives.sh" index "$ql"
# the archives, each checked before it is unpacked or loaded; Quicklisp unpacks
# one it finds in archives/ and fetches nothing
"$root/tools/lisp-archives.sh" fetch "$ql"
"$root/tools/lisp-archives.sh" check "$ql"
# anything Quicklisp would still download fails
"$root/tools/lisp-archives.sh" lock "$ql"
echo "Quicklisp $(cat "$ql/quicklisp/version.txt"), $(sed -n 's/^version: //p' "$dist/distinfo.txt"), $(grep -vc '^#' "$root/tools/lisp-archives.sha256") libraries"
