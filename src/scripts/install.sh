#!/bin/sh

set -e

root=${1:-./dist}
layout=${2:-virtualenv}
srcroot=$(realpath "${3:-.}")

bin_pip=${bin_pip:-pip}
bin_python=${bin_python:-python}
bin_ghc=${bin_ghc:-ghc}
bin_cabal=${bin_cabal:-cabal}
bin_virtualenv=${bin_virtualenv:-virtualenv}

install_virtualenv () {
  bin_python="$root/bin/python"
  bin_pip="$root/bin/pip"

  test -d "$root" || "$bin_virtualenv" "$root"
  install_mkdirs
  install_usr
  install_etc
  "$bin_pip" install -q -r "$srcroot/pip-requires.txt"
  "$bin_pip" install -q -I "$srcroot"
  fixup_variables
}

install_debian () {
  local record file
  record="$srcroot/debian/leela.install"
  mkdir -p "$root"
  install_mkdirs
  "$bin_python" "$srcroot/setup.py" install \
    --install-layout=deb                    \
    --root="$srcroot/debian/tmp"            \
    --record="$record"
  for u in http://pypi.python.org/packages/source/T/Twisted/Twisted-13.0.0.tar.bz2             \
           http://pypi.python.org/packages/source/z/zope.interface/zope.interface-4.0.3.tar.gz \
           http://pypi.python.org/packages/source/w/wokkel/wokkel-0.7.1.tar.gz                 \
           http://pypi.python.org/packages/source/t/txredisapi/txredisapi-1.0.tar.gz           \
           http://pypi.python.org/packages/source/c/cyclone/cyclone-1.1.tar.gz                 \
           http://pypi.python.org/packages/source/t/thrift/thrift-0.9.0.tar.gz                 \
           https://github.com/driftx/Telephus/tarball/releases/1.0.0_beta1
  do
    oldpath=$(pwd)
    TMPDIR=$(mktemp -d) && cd "$TMPDIR" && {
      file="$TMPDIR"/$(basename "$u")
      wget --no-check-certificate -q -O"$file" "$u"
      if echo $u | grep -q Twisted
      then
        tar -x -j --xform 's,[^/]*/,,' -f "$file"
      else
        tar -x -z --xform 's,[^/]*/,,' -f "$file"
      fi
      rm -rf $srcroot/build
      "$bin_python" setup.py install \
        --quiet                      \
        --install-layout=deb         \
        --root="$srcroot/debian/tmp" \
        --record="$record.dep"
      cat "$record.dep" >>"$record"
      rm -r -f "$TMPDIR"
    }
    cd "$oldpath"
  done
  install_usr >>"$record"
  install_etc >>"$record"
}

install_usr () {
  local dst

  for f in dmproc timeline multicast
  do
    dst="/usr/bin/$f"
    install -m 0755 "$srcroot/src/dmproc/DarkMatter/$f" "$root$dst"
    echo "$dst"
  done
  for f in "$srcroot/usr/libexec/"*
  do
    dst=${f##$srcroot/}
    install -m 0755 "$f" "$root/$dst"
    echo "/$dst"
  done
}

install_etc () {
  local dst

  for f in "$srcroot/etc/default/"* "$srcroot/etc/leela.conf"
  do
    dst="${f##$srcroot/}"
    install -m 0600 "$f" "$root/$dst"
    echo "/$dst"
  done
  for f in "$srcroot/etc/init.d/"*
  do
    dst="${f##$srcroot/}"
    install -m 0755 "$f" "$root/$dst"
    echo "/$dst"
  done
}

fixup_variables () {
  for f in "$root/etc/init.d/"*
  do
    sed -i "s,\\\${CHDIR},$root,g" "$f"
  done
  sed -i "s,^# template:chdir\$,CHDIR=\"$root\",g" "$root/usr/libexec/leela-interact"
  sed -i "s,^# template:chdir\$,CHDIR=\"$root\",g" "$root/usr/libexec/leela-self-test"
  sed -i "s,^# template:bin_python\$,bin_python=\"$bin_python\",g" "$root/usr/libexec/leela-interact"
}

install_mkdirs () {
  mkdir -p "$root/usr/bin"
  mkdir -p "$root/usr/libexec"
  mkdir -p "$root/etc/default"
  mkdir -p "$root/var/run/leela"
  mkdir -p "$root/var/log/leela"
  mkdir -p "$root/etc/init.d"
}

if [ "$layout" = virtualenv ]
then
  install_virtualenv
elif [ "$layout" = debian ]
then
  install_debian
fi
