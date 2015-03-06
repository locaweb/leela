#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

adduser --home-dir /home/leela --shell /bin/sh leela

rpm_install wget
if [ "$arch" = "i386" ]
then rpm_install_url epel-release http://download.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm
elif [ "$arch" = "amd64" ]
then rpm_install_url epel-release http://download.fedoraproject.org/pub/epel/5/x86_64/epel-release-5-4.noarch.rpm
fi
rpm_install cmake28 git rpmdevtools pkgconfig
