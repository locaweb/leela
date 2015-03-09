#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

collectd_collectdurl=${collectd_collectdurl:-https://github.com/collectd/collectd/archive/collectd-5.4.1.tar.gz}

collectd_idd_debian () {
  deb_install tar gzip which
  deb_install libgcrypt11-dev libglib2.0-dev libperl-dev
  deb_install make gcc g++ flex bison libtool autoconf automake
}

collectd_idd_centos () {
  rpm_install tar gzip which
  rpm_install libtool-ltdl-devel libgcrypt-devel glib2-devel perl-devel
  rpm_install make gcc gcc-c++ flex bison byacc libtool autoconf automake
}

collectd_idd () {
  local collectddir

  collectddir="$buildroot/collectd-collectd-5.4.1"

  if ! has_file "$distroot/sbin/collectd"
  then
    fetch_url "$collectd_collectdurl" | tar -x -z -C "$buildroot"
    cd "$collectddir" && {
      run_cmd_echo mkdir -p libltdl/config
      run_cmd_echo ./build.sh
      run_cmd_echo ./configure --prefix="$distroot"
      run_cmd_echo make
      run_cmd_echo make install
      run_cmd_echo mkdir -p "$distroot/include/collectd" "$distroot/include/liboconfig"
      run_cmd_echo install -m 644 src/*.h "$distroot/include/collectd/"
      run_cmd_echo install -m 644 src/liboconfig/*.h "$distroot/include/liboconfig/"
    }
  fi
}

show_self collectd_url="\"$collectd_collectdurl\""
if has_command dpkg apt-get
then collectd_idd_debian; fi

if has_command yum rpm
then collectd_idd_centos; fi

check_command wget tar libtool autoconf automake flex bison

run_installer "$distroot" "$buildroot" collectd_idd
