#!/bin/bash

set -x

target=leela-docker-$RANDOM

mount_target ()
{
  rm -rf "$target"
  mkdir -p "$target"
}

umount_target ()
{
  for fs in $(grep "$target" /proc/self/mounts | cut -d\  -f2)
  do umount "$fs";  done
}

make_debian ()
{
  local arch
  local dist
  arch=$1
  dist=$2

  mount_target
  debootstrap --arch=$arch --foreign --variant=buildd --include=iproute,iputils-ping,netbase $dist "$target" "http://cdn.debian.net/debian"
  env DEBOOTSTRAP_DIR="$target/debootstrap" debootstrap --second-stage --second-stage-target="$target"
  umount_target

  tar -C "$target" -c . | docker import - leela/debian-$dist-$arch
  rm -rf "$target"
}

make_centos ()
{
  local arch
  local dist
  arch=$1
  dist=$2

  mount_target
  rinse --arch $arch --distribution centos-$dist --directory "$target"
  rm -rf "$target"/dev
  rm -rf "$target"/usr/{{lib,share}/locale,{lib,lib64}/gconv,bin/localedef,sbin/build-locale-archive}
  rm -rf "$target"/usr/share/{man,doc,info,gnome/help}
  rm -rf "$target"/usr/share/cracklib
  rm -rf "$target"/usr/share/i18n
  rm -rf "$target"/var/cache/yum
  rm -rf "$target"/etc/ld.so.cache "$target"/var/cache/ldconfig
  mkdir -m 0755 -p "$target"/var/cache/ldconfig
  mkdir -m 755 -p "$target"/var/cache/yum
  mkdir -m 755 "$target"/dev
  mkdir -m 755 "$target"/dev/pts
  mkdir -m 1777 "$target"/dev/shm
  mknod -m 600 "$target"/dev/console c 5 1
  mknod -m 600 "$target"/dev/initctl p
  mknod -m 666 "$target"/full c 1 7
  mknod -m 666 "$target"/null c 1 3
  mknod -m 666 "$target"/ptmx c 5 2
  mknod -m 666 "$target"/random c 1 8
  mknod -m 666 "$target"/tty c 5 0
  mknod -m 666 "$target"/tty0 c 4 0
  mknod -m 666 "$target"/urandom c 1 9
  mknod -m 666 "$target"/zero c 1 5

  mkdir -p "$target"/var/cache/rinse
  sed -i 's/\$releasever/'$dist/ "$target"/etc/yum.repos.d/*
  sed -i 's/enable=0/enable=1/g' "$target"/etc/yum.repos.d/CentOS-Base.repo
  if [ "$arch" = i386 ]
  then
    mkdir -p "$target"/etc/rpm
    echo "i686-redhat-linux" > "$target"/etc/rpm/platform
    echo "i386-redhat-linux" >> "$target"/etc/rpm/platform
  fi
  echo "NETWORKING=yes" | tee "$target/etc/sysconfig/network" >/dev/null
  umount_target

  tar -C "$target" -c . | docker import - leela/centos-$dist-$arch
  rm -rf "$target"
}

make_centos i386 6
make_centos i386 5
make_centos amd64 7
make_centos amd64 6
make_centos amd64 5
make_debian i386 wheezy
make_debian i386 squeeze
make_debian amd64 wheezy
make_debian amd64 squeeze
