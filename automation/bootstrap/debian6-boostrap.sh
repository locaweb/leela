#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

debian6_bootstrap () {
  deb_add_repo squeeze-backports "deb http://http.debian.net/debian-backports squeeze-backports main"
  deb_install apt-utils ca-certificates wget
}

debian6_bootstrap
(deb_install_args="-t squeeze-backports" deb_install cmake)
deb_install devscripts
