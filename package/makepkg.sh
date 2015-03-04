#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}
distroot=$2

makepkg_collect () {
  local distdir
  distdir="$srcroot/dist/${dist:-linux}/${arch:-unknown}"
  mkdir -p "$distdir"
  find "$1" -maxdepth 1 -type f -exec cp -a {} "$distdir" \;
}

makepkg_debian () {
  local buildroot
  buildroot=$(mktemp -d) && {
    trap "rm -rf \"$buildroot\"" INT QUIT TERM EXIT
    "${srcroot}/mksource.sh" | tar -C "$buildroot" -xz
    cd "$buildroot/$package-$version"
    ln -sfn "package/$package/$dist" debian
    dpkg-buildpackage -us -uc
    makepkg_collect "$buildroot"
    rm -rf "$buildroot"
  }
}

makepkg_centos () {
  local rpmsrcdir

  rpmdev-setuptree; rpmsrcdir=$(rpm --eval '%{_sourcedir}')
  "${srcroot}/mksource.sh" >"$rpmsrcdir/$package-$version.tar.gz"
  cd "$srcroot/$package/$dist"
  rpmbuild -ba "$package.spec"
}

makepkg_bootstrap () {
  if echo "$dist" | grep -qE '^debian[67]$|^centos[567]$'
  then "$srcroot/../automation/bootstrap/$dist-bootstrap.sh"; fi
  case "$package" in
    leela-c)
      "$srcroot/../automation/bootstrap/zeromq-bootstrap.sh"
      ;;
  esac
}

if [ -n "$bootstrap" ]
then makepkg_bootstrap; fi

case "$1" in
  debian)
    makepkg_debian
    ;;
  centos)
    makepkg_centos
    ;;
esac
