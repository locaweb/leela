#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))/..}
distroot=$2

makepkg_collect () {
  local distdir
  distdir="$srcroot/package/dist/${dist:-unknown-dist}/${arch:-unknown-arch}"
  mkdir -p "$distdir"
  find "$1" -maxdepth 1 -type f -exec cp -a {} "$distdir" \;
}

makepkg_debian () {
  local buildroot
  buildroot=$(mktemp -d) && {
    trap "rm -rf \"$buildroot\"" INT QUIT TERM EXIT
    "$srcroot/package/mksource.sh" | tar -C "$buildroot" -xz
    cd "$buildroot/$package-$version"
    ln -sfn "package/$package/$dist" debian
    dpkg-buildpackage -us -uc
    makepkg_collect "$buildroot"
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
  local rpmsrcdir
  local rpmspcdir

  rpmdev-setuptree
  makepkg_rpm_getdir rpmspcdir %{_specdir}
  makepkg_rpm_getdir rpmsrcdir %{_sourcedir}

  "$srcroot/package/mksource.sh" >"$rpmsrcdir/$package-$version.tar.gz"
  cp "$srcroot/package/$package/$dist/$package.spec" "$rpmspcdir/"
  rpmbuild -ba "$rpmspcdir/$package.spec"
}

makepkg_bootstrap () {
  if echo "$dist" | grep -qE '^debian[67]$|^centos[567]$'
  then "$srcroot/automation/bootstrap/$dist-bootstrap.sh"; fi
  case "$package" in
    leela-c)
      "$srcroot/automation/bootstrap/zeromq-bootstrap.sh"
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
