#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}
distroot=$2

makepkg_debian () {
  local buildroot
  buildroot=$(mktemp -d) && {
    trap "rm -rf \"$buildroot\"" INT QUIT TERM EXIT
    "${srcroot}/mksource.sh" | tar -C "$buildroot" -xz
    cd "$buildroot/$package-$version/package/$package"
    ln -sfn "$dist" debian
    dpkg-buildpackage -us -uc
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

makepkg_boostrap () {
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
