#!/bin/sh

set -e
set -x

docker_build () {
  docker build -rm -t leela/dev .
}

stage1_installpkg_squeeze () {
  echo "deb http://cdn.debian.net/debian squeeze main non-free contrib" > /etc/apt/sources.list
  echo "deb http://ftp.br.debian.org/debian-backports squeeze-backports main" > /etc/apt/sources.list.d/bpo.list
  echo "deb http://ftp.br.debian.org/debian-backports squeeze-backports-sloppy main" >> /etc/apt/sources.list.d/bpo.list
  apt-get update && apt-get install -q --yes --force-yes \
    libncursesw5-dev libffi-dev libzmq3-dev zlib1g-dev python2.6-dev \
    wget ca-certificates debhelper devscripts coreutils
}

stage1_installpkg_wheezy () {
  echo "deb http://cdn.debian.net/debian wheezy main non-free contrib" > /etc/apt/sources.list
  echo "deb http://cdn.debian.net/debian wheezy-backports main" > /etc/apt/sources.list.d/bpo.list
  apt-get update && apt-get install -q --yes --force-yes \
    libncursesw5-dev libffi-dev libzmq3-dev zlib1g-dev libzookeeper-mt-dev python2.7-dev \
    wget ca-certificates debhelper devscripts coreutils
}

stage1_installpkg () {
  if [ "$1" = wheezy ]
  then
    stage1_installpkg_wheezy
  elif [ "$1" = squeeze ]
  then
    stage1_installpkg_squeeze
  fi
}

stage1_installzoo () {
  wget -O - http://ftp.unicamp.br/pub/apache/zookeeper/zookeeper-3.4.5/zookeeper-3.4.5.tar.gz | tar -x -z -C /opt
  cd /opt/zookeeper-3.4.5/src/c
  ./configure --prefix=/usr
  make
  make install
}

stage1_installghc () {
  if [ "$1" = wheezy ]
  then
    ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.3
    ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so
  elif [ "$1" = squeeze ]
  then
    ln -s libgmp.so.3 /usr/lib/libgmp.so
  fi

  wget -O - http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2 | tar -x -j -C /opt
  cd /opt/ghc-7.6.3
  ./configure
  make install

  wget -O - http://www.haskell.org/cabal/release/cabal-install-1.18.0.2/cabal-install-1.18.0.2.tar.gz | tar -x -z -C /opt
  cd /opt/cabal-install-1.18.0.2
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

if [ "$(basename $0)" = in-wheezy-target.sh ]
then
  stage1_installpkg wheezy
  stage1_installclj wheezy
  stage1_installghc wheezy
elif [ "$(basename $0)" = in-squeeze-target.sh ]
then
  stage1_installpkg squeeze
  stage1_installzoo squeeze
  stage1_installclj squeeze
  stage1_installghc squeeze
else
  docker_build
fi
