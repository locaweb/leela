#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

collectd_collectdurl=${collectd_collectdurl:-https://github.com/collectd/collectd/archive/collectd-5.4.1.tar.gz}

collectd_idd () {
  deb_install tar gzip
  deb_install libgcrypt11-dev libglib2.0-dev
  deb_install make gcc g++ flex bison libtool autoconf automake
}

collectd_icollectd () {
  local collectddir

  collectddir="$buildroot/collectd-collectd-5.4.1"

  if ! has_file "$distroot/sbin/collectd"
  then
    fetch_url "$collectd_collectdurl" | tar -x -z -C "$buildroot"
    cd "$collectddir" && {
      run_cmd_echo ./build.sh
      run_cmd_echo ./configure --prefix="$distroot"
      run_cmd_echo make
      run_cmd_echo make install
    }
  fi
}

show_self collectd_url="\"$collectd_collectdurl\""
if has_command dpkg apt-get
then collectd_idd; fi

check_command wget tar libtool autoconf automake flex bison

run_installer "$distroot" "$buildroot" collectd_icollectd
