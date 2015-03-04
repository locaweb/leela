#!/bin/sh

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))/..}

cd "$srcroot" && git archive --format=tar --prefix=$package-$version/ HEAD | gzip -6
