#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

adduser --home-dir /home/leela --shell /bin/sh leela || true

rpm_install wget cmake git rpmdevtools rpm-build pkgconfig
