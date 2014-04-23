#!/bin/bash

set -e
set -x

docker_build () {
  local tmpdir
  local curdir
  curdir=$(cd "$(dirname "$0")" && pwd)
  cname=$(basename "$1" .dockerfile)
  tmpdir="leela-stage1-$RANDOM" && {
    mkdir -p "$tmpdir"
    cp "$curdir/$1" "$tmpdir/Dockerfile"
    cp "$0" "$tmpdir/stage-1.sh"
    docker build --rm -t "leela/$cname" "$tmpdir" || rm -rf "$tmpdir"
    rm -rf "$tmpdir"
  }
}

stage1_installpkg_squeeze () {
  echo "deb http://cdn.debian.net/debian squeeze main non-free contrib" > /etc/apt/sources.list
  echo "deb http://ftp.br.debian.org/debian-backports squeeze-backports main" > /etc/apt/sources.list.d/bpo.list
  echo "deb http://ftp.br.debian.org/debian-backports squeeze-backports-sloppy main" >> /etc/apt/sources.list.d/bpo.list
  apt-get update && apt-get install -q --yes --force-yes \
    libncursesw5-dev libffi-dev libzmq3-dev zlib1g-dev python2.6-dev \
    wget ca-certificates debhelper devscripts coreutils
  apt-get -t squeeze-backports install -q --yes --force-yes collectd-dev
  adduser --system --home /home/leela --shell /bin/sh --uid 1000 leela
}

stage1_installpkg_wheezy () {
  echo "deb http://cdn.debian.net/debian wheezy main non-free contrib" > /etc/apt/sources.list
  echo "deb http://cdn.debian.net/debian wheezy-backports main" > /etc/apt/sources.list.d/bpo.list
  apt-get update && apt-get install -q --yes --force-yes \
    libncursesw5-dev libffi-dev libzmq3-dev zlib1g-dev libzookeeper-mt-dev python2.7-dev python2.6-dev \
    wget ca-certificates debhelper devscripts coreutils collectd-dev \
    build-essential g++ gcc autoconf automake libtool gettext
  adduser --system --home /home/leela --shell /bin/sh --uid 1000 leela
}

stage1_installpkg_centos6 () {
  yum install -y --nogpgcheck \
    ncurses-devel libffi-devel zlib-devel python-devel uuid-devel \
    wget ca-certificates rpmdevtools tar gcc gcc-c++ git make || true
  yum install -y --nogpgcheck \
    zeromq-devel -c "http://download.opensuse.org/repositories/home:/fengshuo:/zeromq/CentOS_CentOS-6/home:fengshuo:zeromq.repo" || true
  adduser -r --uid 1000 --shell /bin/sh --create-home --home-dir /home/leela leela
}

stage1_installpkg_centos5 () {
  yum install -y --nogpgcheck wget || true
  cat <<EOF >/etc/yum.repos.d/Locaweb.repo
[loca-core]
name=Locaweb Core Packages
baseurl=http://repo.linux.locaweb.com.br/team/core/\$basearch
enabled=1
gpgcheck=0

[loca-python]
name=Locaweb Python Packages
baseurl=http://repo.linux.locaweb.com.br/python/2.6/base/\$basearch
enabled=1
gpgcheck=0

[loca-core-noarch]
name=Locaweb Core Packages (noarch)
baseurl=http://repo.linux.locaweb.com.br/team/core/noarch
enabled=1
gpgcheck=0
EOF
  if [ "$arch" = "amd64" ]
  then
      wget http://dl.fedoraproject.org/pub/epel/5/x86_64/epel-release-5-4.noarch.rpm
  elif [ "$arch" = "i386" ]
  then
      wget http://dl.fedoraproject.org/pub/epel/5/i386/epel-release-5-4.noarch.rpm
  fi
  yum install -y --nogpgcheck \
    ncurses-devel libffi-devel zlib-devel uuid-devel \
    wget ca-certificates zeromq3 zeromq3-devel python2.6-devel python2.6 tar gcc gcc-c++ make collectd || true
  yum localinstall -y --nogpgcheck epel-release-5-4.noarch.rpm || true
  yum install -y --nogpgcheck git rpmdevtools || true
  adduser -r --uid 1000 --shell /bin/sh --create-home --home-dir /home/leela leela
}

stage1_installzmq () {
  wget -O - https://github.com/zeromq/zeromq4-x/archive/v4.0.4.tar.gz | tar -x -z -C /opt
  cd /opt/zeromq4-x-4.0.4
  ./autogen.sh
  ./configure --prefix=/opt/zmq4
  make; make install
}

stage1_installghc () {
  ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.3
  ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so

  wget -O - https://www.haskell.org/ghc/dist/7.8.2/ghc-7.8.2-x86_64-unknown-linux-deb7.tar.bz2 | tar -x -j -C /opt
  cd /opt/ghc-7.8.2
  ./configure
  make install

  wget -O - http://www.haskell.org/cabal/release/cabal-install-1.18.0.3/cabal-install-1.18.0.3.tar.gz | tar -x -z -C /opt
  cd /opt/cabal-install-1.18.0.3
  ./bootstrap.sh --global

  cabal update
  cabal install happy --symlink-bindir=/usr/bin
  cabal install alex --symlink-bindir=/usr/bin
}

stage1_installclj () {
  echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" > /etc/apt/sources.list.d/java.list
  echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
  echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
  apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886
  apt-get update && apt-get install -q --yes --force-yes \
    automake pkg-config libtool \
    oracle-java7-installer

  wget -O - https://github.com/zeromq/jzmq/archive/v2.2.2.tar.gz | tar -x -z -C /opt
  cd /opt/jzmq-2.2.2
  ./autogen.sh
  ./configure --prefix=/usr
  make
  make install

  wget -O /usr/bin/lein https://raw.github.com/technomancy/leiningen/stable/bin/lein
  chmod 755 /usr/bin/lein
}

if [ "$dist" = debian7 ]
then
  stage1_installpkg_wheezy
  if [ "$arch" = amd64 ]
  then
    stage1_installzmq
    stage1_installclj
    stage1_installghc
  fi
elif [ "$dist" = debian6 ]
then
  stage1_installpkg_squeeze
elif [ "$dist" = centos5 ]
then
  stage1_installpkg_centos5
elif [ "$dist" = centos6 ]
then
  stage1_installpkg_centos6
elif [ "$(basename $0)" = stage-1.sh ]
then
  if [ -z "$1" ]
  then
    for f in *.dockerfile
    do
      docker_build "$f"
    done
  else
    docker_build "$1"
  fi
fi
