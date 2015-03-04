#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

deb_add_repo squeeze-backports "deb http://http.debian.net/debian-backports squeeze-backports main"
deb_install apt-utils wget git devscripts pkg-config
(deb_install_args="-t squeeze-backports" deb_install cmake)
