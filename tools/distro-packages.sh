#!/usr/bin/env bash
# tools/distro-packages.sh --- Make Tuatara into Fedora, Ubuntu and Arch packages.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Make build/packages/: a Fedora package (.rpm), an Ubuntu package (.deb) and
# an Arch package (.pkg.tar.zst), each installed with the system's own package
# manager, which also fetches the libraries Tuatara needs (GTK 4, ncurses,
# OpenSSL's crypto, zstd, the C++ runtime) and offers the programs some of its
# features run, recommended (enchant for spelling, bubblewrap, ImageMagick,
# poppler's tools, git, libnotify) or suggested (GnuPG, xdg-utils, LibreOffice,
# TeX Live): the packages *outside-programs* names (lisp/files/external.lisp),
# which a test holds the lists here to. All three hold Tuatara as
# tools/release.sh lays it out, in
# /usr/lib/tuatara/, with the command tuatara in /usr/bin, the desktop entry
# and the icons; each is made by its system's own tools (rpmbuild, dpkg-deb,
# makepkg) in a container of the system's image pinned in tools/containers.sh,
# kept between runs. They are for Fedora 40, Ubuntu 24.04 and later, and Arch
# as it is.
#
# The version is Tuatara's (tuatara.asd) with the day and commit it is made
# from, as each system writes a snapshot's; the packager is git's user.name
# and user.email.
#
# --check installs each package made in a new container of its system, with
# nothing else installed first, checks that every library Tuatara loads came
# with it, and runs tuatara --version there and then Tuatara itself in a
# terminal on a Rust file, which must show the file's mode line and colour
# its code; the container is removed after.
# Usage: tools/distro-packages.sh [--check] [fedora] [ubuntu] [arch]
# (all three when none is named)
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
. tools/containers.sh

check=""
systems=()
for argument in "$@"; do
  case "$argument" in
    --check) check=t ;;
    fedora|ubuntu|arch) systems+=("$argument") ;;
    *) echo "Usage: tools/distro-packages.sh [--check] [fedora] [ubuntu] [arch]" >&2; exit 1 ;;
  esac
done
[ ${#systems[@]} -gt 0 ] || systems=(fedora ubuntu arch)

out="$root/build/packages"
stage="$out/root"
version=$(sed -n 's/^ *:version "\(.*\)"$/\1/p' tuatara.asd)
commit=$(git rev-parse --short=7 HEAD)
day=$(git log -1 --format=%cd --date=format:%Y%m%d HEAD)
changelog_day=$(LC_ALL=C git log -1 --format=%cd --date=format:'%a %b %d %Y' HEAD)
# who made the packages: git's name and address here, else the commit's author's
packager="$(git config user.name || git log -1 --format=%an HEAD) <$(git config user.email || git log -1 --format=%ae HEAD)>"
# each system's way of writing a snapshot's version, and the file named by it
rpm_release="0.${day}git$commit"
deb_version="$version~git$day.$commit"
arch_version="$version.r$day.g$commit"
rpm_file="tuatara-$version-$rpm_release.x86_64.rpm"
# the file's name with . for the ~, which GitHub takes out of the names of a release's files
deb_file="tuatara_${deb_version//\~/.}_amd64.deb"
arch_file="tuatara-$arch_version-1-x86_64.pkg.tar.zst"
package_file() { case "$1" in fedora) echo "$rpm_file" ;; ubuntu) echo "$deb_file" ;; arch) echo "$arch_file" ;; esac; }
summary="Text editor for the terminal and the desktop"
description="Tuatara edits text in the terminal and in a window of its own: Org
files, code with tree-sitter colours and language servers, CSV files of many
gigabytes, mail folders and folders of files, with DONKEY's modal keys or
plain ones."
# Tuatara's licence, Ghostty's (the terminal library), those of what the
# terminal library compiles in and each grammar's
licences="GPL-3.0-or-later AND MIT"
while IFS= read -r licence; do
  case " AND $licences AND " in *" AND $licence AND "*) ;; *) licences="$licences AND $licence" ;; esac
done < <({ cut -f2 build/terminal/licenses/licenses.tsv; grep -v '^#' syntax/grammars.tsv | cut -f4; } | sort -u)
# the libraries the program loads, by the names it asks for
libraries="libgtk-4.so.1 libcairo.so.2 libncursesw.so.6 libcrypto.so.3 libzstd.so.1 libstdc++.so.6"

tools/release.sh >/dev/null
rm -rf "$stage"
mkdir -p "$stage/usr/lib" "$stage/usr/bin" "$stage/usr/share/applications" \
         "$stage/usr/share/licenses/tuatara"
cp -a build/release/tuatara "$stage/usr/lib/tuatara"
ln -s ../lib/tuatara/tuatara "$stage/usr/bin/tuatara"
sed "s|@EXEC@|tuatara|" data/tuatara.desktop.in > "$stage/usr/share/applications/tuatara.desktop"
cp -r data/icons "$stage/usr/share/icons"
cp LICENSE "$stage/usr/share/licenses/tuatara/LICENSE"
cp build/release/tuatara/terminal/LICENSE-ghostty "$stage/usr/share/licenses/tuatara/LICENSE-ghostty"
cp -r build/release/tuatara/terminal/licenses "$stage/usr/share/licenses/tuatara/terminal"
cp syntax/grammars.tsv "$stage/usr/share/licenses/tuatara/grammars.tsv"
cp -r build/release/tuatara/grammars/licenses "$stage/usr/share/licenses/tuatara/grammars"
cp -r build/release/tuatara/licenses/lisp build/release/tuatara/licenses/sbcl "$stage/usr/share/licenses/tuatara/"
chmod -R u=rwX,go=rX "$stage"

# $1 a container, $2 a place in it: the staged files copied there
put_stage() {
  podman exec "$1" rm -rf "$2"
  podman exec "$1" mkdir -p "$2"
  podman cp "$stage/." "$1:$2/root"
}

fedora() {
  local container=tuatara-package-fedora
  pinned_container "$container" "$fedora_image" 'dnf -y -q install rpm-build >/dev/null'
  put_stage "$container" /tmp/package
  podman exec -i "$container" sh -c 'cat > /tmp/package/tuatara.spec' <<EOF
%global debug_package %{nil}
%global __os_install_post %{nil}
%global _build_id_links none
Name: tuatara
Version: $version
Release: $rpm_release
Summary: $summary
License: $licences
Packager: $packager
ExclusiveArch: x86_64
AutoProv: no
Requires: gtk4 ncurses-libs openssl-libs libzstd libstdc++
Recommends: enchant2 bubblewrap ImageMagick poppler-utils git-core libnotify
Suggests: gnupg2 xdg-utils libreoffice-writer libreoffice-calc libreoffice-impress texlive-xetex texlive-luatex

%description
$description

%install
cp -a /tmp/package/root/. %{buildroot}/

%files
/usr/lib/tuatara
/usr/bin/tuatara
/usr/share/applications/tuatara.desktop
/usr/share/icons/hicolor/*/apps/tuatara.*
%license /usr/share/licenses/tuatara

%changelog
* $changelog_day $packager - $version-$rpm_release
- Made from commit $commit
EOF
  podman exec "$container" rpmbuild -bb --quiet --define "_topdir /tmp/package/rpm" /tmp/package/tuatara.spec
  rm -f "$out"/tuatara-*.rpm
  podman cp "$container:/tmp/package/rpm/RPMS/x86_64/$rpm_file" "$out/"
}

ubuntu() {
  local container=tuatara-package-ubuntu deb=/tmp/package/root
  pinned_container "$container" "$ubuntu_image" 'true'
  put_stage "$container" /tmp/package
  podman exec "$container" sh -c "mkdir -p $deb/DEBIAN $deb/usr/share/doc/tuatara &&
    { cat $deb/usr/share/licenses/tuatara/LICENSE; printf '\n%s\n' 'The licences of SBCL (sbcl/), of the Lisp libraries compiled into the program (lisp/NAME/), of the terminal library (LICENSE-ghostty) and of what it compiles in (terminal/NAME/, terminal/licenses.tsv) and of each grammar (grammars/NAME/, grammars.tsv) are in /usr/share/licenses/tuatara/.'; } > $deb/usr/share/doc/tuatara/copyright"
  podman exec -i "$container" sh -c "cat > $deb/DEBIAN/control" <<EOF
Package: tuatara
Version: $deb_version
Architecture: amd64
Maintainer: $packager
Installed-Size: $(du -sk "$stage" | cut -f1)
Depends: libc6 (>= 2.39), libgtk-4-1, libncursesw6, libssl3t64 | libssl3, libzstd1, libstdc++6
Recommends: libenchant-2-2, bubblewrap, imagemagick, poppler-utils, git, libnotify-bin
Suggests: gnupg, xdg-utils, libreoffice-writer, libreoffice-calc, libreoffice-impress, texlive-xetex, texlive-luatex
Section: editors
Priority: optional
Description: $summary
$(sed 's/^/ /' <<<"$description")
EOF
  podman exec "$container" dpkg-deb --root-owner-group -Zzstd --build "$deb" "/tmp/package/$deb_file" >/dev/null
  rm -f "$out"/tuatara_*.deb
  podman cp "$container:/tmp/package/$deb_file" "$out/"
}

arch() {
  local container=tuatara-package-arch
  pinned_container "$container" "$arch_image" 'pacman -Syu --noconfirm --needed -q fakeroot >/dev/null &&
    useradd --create-home packager'
  put_stage "$container" /tmp/package
  podman exec -i "$container" sh -c 'cat > /tmp/package/PKGBUILD' <<EOF
pkgname=tuatara
pkgver=$arch_version
pkgrel=1
pkgdesc="$summary"
arch=(x86_64)
license=('${licences// AND /' '}')
depends=(glibc gcc-libs gtk4 cairo ncurses openssl zstd)
optdepends=('enchant: spelling'
            'bubblewrap: pictures and programs run shut in'
            'imagemagick: pictures'
            'poppler: PDF files'
            'git: the git interface'
            'libnotify: notifications told on the desktop'
            'gnupg: encrypted files'
            'xdg-utils: files opened in the desktop programs'
            'libreoffice-fresh: pages of Office documents'
            'texlive-xetex: PDFs of Org and LaTeX files'
            'texlive-luatex: PDFs of Org and LaTeX files')
options=('!strip' '!debug')

package() {
  cp -a "\$startdir/root/." "\$pkgdir/"
}
EOF
  podman exec "$container" chown -R packager /tmp/package
  podman exec --user packager --workdir /tmp/package --env PACKAGER="$packager" "$container" \
    makepkg --nodeps --noconfirm >/dev/null
  rm -f "$out"/tuatara-*.pkg.tar.zst
  podman cp "$container:/tmp/package/$arch_file" "$out/"
}

# $1 a system, $2 its image, $3 the command that installs its package
# /tmp/package/FILE there (and script, which gives Tuatara a terminal):
# installed in a new container, then run; the container is removed whether it
# passes or not
check_package() {
  local container="tuatara-check-$1" file status=0
  file=$(package_file "$1")
  podman rm -f -t 0 "$container" >/dev/null 2>&1 || true
  podman run -d --name "$container" "$2" sleep infinity >/dev/null
  podman exec "$container" mkdir -p /tmp/package &&
  podman cp "$out/$file" "$container:/tmp/package/$file" &&
  podman exec "$container" sh -c "$3" >/dev/null &&
  podman exec "$container" sh -c '
    set -e
    known=$(ldconfig -p)
    for name in '"$libraries"'; do
      echo "$known" | grep -q "^\s*$name " || { echo "$name is not installed" >&2; exit 1; }
    done
    tuatara --version
    cd /tmp
    printf "fn main() {}\n" > probe.rs
    TERM=xterm-256color timeout 10 script -qfc "stty rows 24 cols 80; tuatara probe.rs" screen >/dev/null 2>&1 || true
    grep -aq "probe.rs.*Rust" screen || { echo "Tuatara showed no mode line for probe.rs" >&2; exit 1; }
    grep -aqP "\x1b\[[0-9;]*mfn\x1b" screen || { echo "Tuatara did not colour probe.rs" >&2; exit 1; }' || status=$?
  podman rm -f -t 0 "$container" >/dev/null
  if [ "$status" -ne 0 ]; then
    echo "$1: $file failed its check" >&2
    exit 1
  fi
  echo "$1: $file installs and runs"
}

for system in "${systems[@]}"; do
  "$system"
done
rm -rf "$stage"
if [ -n "$check" ]; then
  for system in "${systems[@]}"; do
    case "$system" in
      fedora) check_package fedora "$fedora_image" "dnf -y -q install /tmp/package/$rpm_file util-linux-script" ;;
      ubuntu) check_package ubuntu "$ubuntu_image" "apt-get update -qq &&
                DEBIAN_FRONTEND=noninteractive apt-get install -y -qq /tmp/package/$deb_file" ;;
      arch) check_package arch "$arch_image" "pacman -Sy --noconfirm -q && pacman -U --noconfirm /tmp/package/$arch_file" ;;
    esac
  done
fi
for system in "${systems[@]}"; do
  echo "$out/$(package_file "$system")"
done
