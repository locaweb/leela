#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

chmod 755 /

adduser --home-dir /home/leela --shell /bin/sh leela
mkdir -p /home/leela
chown leela. /home/leela

(yumcommand="yum reinstall -y" rpm_install binutils)
rpm_install wget cmake git rpmdevtools pkgconfig
