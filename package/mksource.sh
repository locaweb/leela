#!/bin/sh

srcroot=$(cd "$(dirname $0)" && pwd)

cd "$srcroot/.." && git archive --format=tar --prefix=$package-$version/ HEAD | gzip -6
