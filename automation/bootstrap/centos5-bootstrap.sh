#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

centos5_bootstrap () {
  rpm_install wget openssl
  rpm_install_url epel-release http://download.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm
}

centos5_bootstrap
rpm_install cmake28 rpmdevtools
