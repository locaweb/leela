#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

jzmq_jzmqurl=${jzmq_jzmqurl:-https://github.com/zeromq/jzmq/archive/v3.1.0.tar.gz}

jzmq_idd () {
  debian_apt_get tar gzip wget
  debian_apt_get make gcc g++ libtool autoconf automake
}

jzmq_ijzmq () {
  local jzmqdir

  jzmqdir="$buildroot/jzmq-3.1.0"

  if ! has_file "$distroot/lib/libjzmq.so"
  then
    fetch_url "$jzmq_jzmqurl" | tar -x -z -C "$buildroot"
    cd "$jzmqdir" && {
      run_cmd_echo ./autogen.sh
      run_cmd_echo env LIBS="-lpthread -lrt" ./configure --prefix="$distroot"
      run_cmd_echo make
      run_cmd_echo make install
    }
  fi
}

show_self jzmq_jzmqurl="\"$jzmq_jzmqurl\""
if has_command dpkg apt-get
then jzmq_idd; fi

check_command wget tar libtool autoconf automake

run_installer "$distroot" "$buildroot" jzmq_ijzmq
