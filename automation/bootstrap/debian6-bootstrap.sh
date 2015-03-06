#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

adduser --shell /bin/sh --disabled-password --home /home/leela --system leela
deb_add_repo squeeze-backports "deb http://http.debian.net/debian-backports squeeze-backports main"
deb_install apt-utils wget git devscripts pkg-config
(deb_install_args="-t squeeze-backports" deb_install cmake)
