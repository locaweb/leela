#!/bin/sh

SRCROOT=$(dirname $(readlink -f "$0"))

build_prepare () {
  sudo rm -rf "$SRCROOT/dist"
  mkdir -p "$SRCROOT/dist"
}

build_image () {
  sudo docker run -v "$1":/mnt/src/leela 1659228191c8 /mnt/src/leela/do-build.sh /mnt/src/leela "$2"
}

BUILDROOT=$(mktemp -d --tmpdir=/dev/shm) && {
  BUILDFILE=$(basename "$BUILDROOT")
  cp -p -r "$SRCROOT" "$BUILDROOT/leela"
  build_prepare
  build_image "$BUILDROOT/leela" "$BUILDFILE"
  sudo cp "$BUILDROOT/leela/$BUILDFILE" "$SRCROOT/dist/leela.tar.gz"
  sudo rm -rf "$BUILDROOT"
}
