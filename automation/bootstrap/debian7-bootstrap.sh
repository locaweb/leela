#!/bin/sh

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

. "$srcroot/bootstrap-lib.sh"

debian7_bootstrap () {
  deb_install apt-utils ca-certificates wget
}

debian7_bootstrap
deb_install cmake devscripts
