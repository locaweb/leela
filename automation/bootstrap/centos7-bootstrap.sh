#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

(yumcommand="yum reinstall -y" rpm_install binutils)
rpm_install wget cmake git rpmdevtools pkgconfig
