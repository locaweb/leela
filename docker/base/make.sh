#!/bin/bash
set -e

target="/var/tmp/docker-rootfs-leela-base-$$-$RANDOM"

mkdir -p "$target"
debootstrap --verbose --variant=minbase --include=iproute,iputils-ping sid "$target"

pushd "$target"
tar -c . | docker import - leela base

popd
rm -rf "$target"
