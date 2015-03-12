#!/bin/sh

set -x
set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}
distroot=${distroot:-$srcroot/dist}

makepkg_collect_debian () {
  local distdir
  distdir="$distroot/${dist:-unknown-dist}/${arch:-unknown-arch}/${package:-unknown-package}"
  mkdir -p "$distdir"
  find "$1" -maxdepth 1 -type f -exec cp -a {} "$distdir" \;
}

makepkg_collect_centos () {
  local distdir
  distdir="$distroot/${dist:-unknown-dist}/${arch:-unknown-arch}/${package:-unknown-package}"
  mkdir -p "$distdir"
  find "$1" -type f -name '*.rpm' -exec cp -a {} "$distdir" \;
}

makepkg_debian () {
  local buildroot
  buildroot=$(mktemp -d) && {
    trap "rm -rf \"$buildroot\"" INT QUIT TERM EXIT
    "$srcroot/mksource.sh" | tar -C "$buildroot" -xz
    cd "$buildroot/$package-$version"
    ln -sfn "pkg/$package/$dist" debian
    env HOME=/home/leela dpkg-buildpackage -us -uc
    makepkg_collect_debian "$buildroot"
    rm -rf "$buildroot"
  }
}

makepkg_rpm_getdir () {
  local name
  local value
  name=$1
  value=$(rpm --eval "$2")
  if [ -d "$value" ]
  then eval "$name=\"$value\""
  else return 1
  fi
}

makepkg_centos () {
  local rpmdir
  local srpmdir
  local rpmsrcdir
  local rpmspcdir

  rpmdev-wipetree
  rpmdev-setuptree
  makepkg_rpm_getdir rpmspcdir %{_specdir}
  makepkg_rpm_getdir rpmsrcdir %{_sourcedir}
  makepkg_rpm_getdir rpmdir    %{_rpmdir}
  makepkg_rpm_getdir srpmdir   %{_srcrpmdir}

  "$srcroot/mksource.sh" >"$rpmsrcdir/$package-$version.tar.gz"
  cp "$srcroot/$package/$dist/$package.spec" "$rpmspcdir/"
  rpmbuild -ba "$rpmspcdir/$package.spec"
  makepkg_collect_centos "$rpmdir"
  makepkg_collect_centos "$srpmdir"
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
