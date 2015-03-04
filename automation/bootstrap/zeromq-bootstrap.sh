#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

zeromq_zmqurl=${zeromq_zmqurl:-http://download.zeromq.org/zeromq-4.0.5.tar.gz}

zeromq_idd () {
  case "$1" in
    deb)
      deb_install tar gzip
      deb_install make gcc g++ pkg-config
      ;;
    rpm)
      rpm_install tar gzip
      rpm_install make gcc gcc-c++ pkgconfig
      ;;
  esac
}

zeromq_izmq () {
  local zmqdir

  zmqdir="$buildroot/zeromq-4.0.5"

  if ! has_file "$distroot/lib/libzmq.a"
  then
    msg_info "INSTALL zmq4"
    fetch_url "$zeromq_zmqurl" | tar -x -z -C "$buildroot"
    cd "$zmqdir" && {
      run_cmd_echo env CC=g++ CFLAGS="$CFLAGS -fPIC" CXXFLAGS="$CXXFLAGS -fPIC" ./configure --prefix="$distroot" --disable-shared --enable-static
      run_cmd_echo make
      run_cmd_echo make install
    }
  fi
}

if has_command dpkg apt-get
then zeromq_idd deb
elif has_command rpm yum
then zeromq_idd rpm
fi
check_command tar gzip make gcc g++ pkg-config

show_self zeromq_zmqurl="\"$zeromq_zmqurl\""
run_installer "${distroot:-/usr/local}" "$buildroot" zeromq_izmq
