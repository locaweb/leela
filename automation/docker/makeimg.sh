#!/bin/bash

set -e

srcroot=${srcroot:-$(dirname $(readlink -f "$0"))}

makeimg_mount_target () {
  target=$(mktemp -d)
  trap "makeimg_umount_target; makeimg_remove_target" INT QUIT TERM EXIT
}

makeimg_umount_target () {
  for fs in $(grep "$target" /proc/self/mounts | cut -d\  -f2)
  do makeimg_umount "$fs";  done
}

makeimg_remove_target () {
  rm -rf "$target"
}

makeimg_configure_debian () {
  debian_conf_post_script="$target/bootstrap/post-script"
  cp -a "$srcroot/../bootstrap" "$target/bootstrap"
  case "$dist" in
    7)
      name=wheezy
      cat <<EOF >"$debian_conf_post_script"
#!/bin/sh
chroot "$target" /usr/bin/env PATH=/sbin:/bin:/usr/sbin:/usr/bin /bootstrap/debian7-bootstrap.sh
EOF
      ;;
    6)
      name=squeeze
      cat <<EOF >"$debian_conf_post_script"
#!/bin/sh
chroot "$target" /usr/bin/env PATH=/sbin:/bin:/usr/sbin:/usr/bin /bootstrap/debian6-bootstrap.sh
EOF
      ;;
    *)
      return 1
      ;;
  esac

  chmod 755 "$debian_conf_post_script"
}

makeimg_debian () {
  local name
  local arch
  local dist
  local debian_conf_post_script
  arch=$1
  dist=$2

  echo "MAKE debian-$dist-$arch"
  makeimg_mount_target
  makeimg_configure_debian
  debootstrap --arch=$arch --foreign --variant=minbase --include=iproute,iputils-ping,netbase $name "$target" "${debian_mirror:-http://cdn.debian.net/debian}"
  env DEBOOTSTRAP_DIR="$target/debootstrap" debootstrap --second-stage --second-stage-target="$target"
  "$debian_conf_post_script"
  makeimg_umount_target

  tar -C "$target" -c . | docker import - leela/debian-$dist-$arch
  makeimg_remove_target
}

makeimg_configure_centos () {
  centos_conf_pre_script="$target/bootstrap/pre-script"
  centos_conf_post_script="$target/bootstrap/post-script"

  echo "CONF centos-$dist-$arch"
  cp -a "$srcroot/../bootstrap" "$target/bootstrap"

  cat <<EOF >"$centos_conf_pre_script"
#!/bin/sh
sed -i s/enabled=0/enabled=1/g "$target/etc/yum.repos.d/CentOS-Base.repo"
sed -i s/'\$releasever'/$dist/g "$target/etc/yum.repos.d/CentOS-Base.repo" "$target/etc/yum.conf"
if [ "$arch" = i386 ]
then
  mkdir -p "$target/etc/rpm"
  echo "i686-redhat-linux" >"$target/etc/rpm/platform"
  echo "i386-redhat-linux" >>"$target/etc/rpm/platform"
fi
EOF

  case "$dist" in
    5)
      cat <<EOF >"$centos_conf_post_script"
#!/bin/sh
chroot "$target" /usr/bin/env arch=$arch PATH=/sbin:/bin:/usr/sbin:/usr/bin /bootstrap/centos5-bootstrap.sh
EOF
      ;;

    6)
      cat <<EOF >"$centos_conf_post_script"
#!/bin/sh
chroot "$target" /usr/bin/env arch=$arch PATH=/sbin:/bin:/usr/sbin:/usr/bin /bootstrap/centos6-bootstrap.sh
EOF
      ;;

    7)
      cat <<EOF >"$centos_conf_post_script"
#!/bin/sh
chroot "$target" /usr/bin/env arch=$arch PATH=/sbin:/bin:/usr/sbin:/usr/bin /bootstrap/centos7-bootstrap.sh
EOF
      ;;

    *)
      echo "unknown dist version: \`$dist'"
      return 1
      ;;
  esac

  chmod 755 "$centos_conf_post_script" "$centos_conf_pre_script"
}

makeimg_centos () {
  local arch
  local dist
  local centos_conf_pre_script
  local centos_conf_post_script
  arch=$1
  dist=$2

  echo "MAKE centos-$dist-$arch"
  makeimg_mount_target
  makeimg_configure_centos
  rinse --arch $arch \
        --distribution centos-$dist \
        --directory "$target" \
        --after-post-install "$centos_conf_post_script" \
        --before-post-install "$centos_conf_pre_script"
  makeimg_umount_target

  tar -C "$target" -c . | docker import - leela/centos-$dist-$arch
  makeimg_remove_target
}

if echo "$@" | grep -q '\bcentos7.i386\b'
then makeimg_centos i386 7; fi

if echo "$@" | grep -q '\bcentos6.i386\b'
then makeimg_centos i386 6; fi

if echo "$@" | grep -q '\bcentos5.i386\b'
then makeimg_centos i386 5; fi

if echo "$@" | grep -q '\bcentos7.amd64\b'
then makeimg_centos amd64 7; fi

if echo "$@" | grep -q '\bcentos6.amd64\b'
then makeimg_centos amd64 6; fi

if echo "$@" | grep -q '\bcentos5.amd64\b'
then makeimg_centos amd64 5; fi

if echo "$@" | grep -q '\bdebian7.i386\b'
then makeimg_debian i386 7; fi

if echo "$@" | grep -q '\bdebian6.i386\b'
then makeimg_debian i386 6; fi

if echo "$@" | grep -q '\bdebian7.amd64\b'
then makeimg_debian amd64 7; fi

if echo "$@" | grep -q '\bdebian6.amd64\b'
then makeimg_debian amd64 6; fi
