#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

adduser --shell /bin/sh --disabled-password --home /home/leela --system leela
deb_install apt-utils wget cmake git devscripts pkg-config
