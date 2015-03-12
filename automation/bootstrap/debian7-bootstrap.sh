#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

deb_install apt-utils adduser wget cmake git devscripts pkg-config

adduser --shell /bin/sh --disabled-password --home /home/leela --system leela
mkdir -p /home/leela
chown leela. /home/leela
