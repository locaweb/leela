#!/bin/sh

set -x

target=leela-stage0-$RANDOM
pkglist=iproute,iputils-ping,netbase

rm -rf "$target"
mkdir -p "$target"
debootstrap --verbose --variant=buildd --include=$pkglist wheezy "$target" http://cdn.debian.net/debian
tar -C "$target" -c . | docker import - leela/stage0
rm -rf "$target"
