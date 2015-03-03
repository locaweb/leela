#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

centos6_bootstrap () {
  rpm_install ca-certificates wget
}

centos6_bootstrap
rpm_install cmake git rpmdevtools
