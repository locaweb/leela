#!/bin/sh

set -e

root=${1:-./dist}
layout=${2:-virtualenv}
srcroot=$(realpath "${3:-.}")

bin_pip=${bin_pip:-pip}
bin_python=${bin_python:-python2.7}
bin_virtualenv=${bin_virtualenv:-virtualenv}

install_virtualenv () {
  test -d "$root" || "$bin_virtualenv" "$root"
  install_mkdirs
  install_usr
  install_etc
  fixup_variables
  "$bin_pip" install -q -r "$srcroot/pip-requires.txt"
  "$bin_pip" install -q -I "$srcroot"
  fixup_variables
}

install_debian () {
  local record
  record="$srcroot/debian/leela.install"
  mkdir -p "$root"
  install_mkdirs
  "$bin_python" "$srcroot/setup.py" install \
    --install-layout=deb                    \
    --root="$srcroot/debian/tmp"            \
    --install-data="$srcroot/debian/tmp"    \
    --record="$record"
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
    dst=${f##$srcroot}
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
  sed -i "s,\\\${__ENVIRON__},CHDIR=\"$root\",g" "$root/usr/libexec/leela-interact"
  sed -i "s,\\\${bin_python:-python},$bin_python,g" "$root/usr/libexec/leela-interact"
  sed -i "s,\\\$@,--config=\"$root/etc/leela.conf\" \"$@\",g" "$root/usr/libexec/leela-interact"
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
