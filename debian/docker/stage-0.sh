#!/bin/sh

set -x

target=leela-stage0-$RANDOM
pkglist=iproute,iputils-ping,netbase

make_target() {
  rm -rf "$target"
  mkdir -p "$target"
  debootstrap --verbose --variant=buildd --include=$pkglist $1 "$target" http://cdn.debian.net/debian
  tar -C "$target" -c . | docker import - leela/stage0-$1
  rm -rf "$target"
}

make_target wheezy
make_target squeeze
