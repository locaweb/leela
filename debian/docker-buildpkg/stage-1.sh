#!/bin/sh

set -e
set -x

docker_build () {
  docker build -t leela/dev .
}

stage1_installpkg () {
  echo "deb http://cdn.debian.net/debian wheezy main non-free contrib" > /etc/apt/sources.list
  echo "deb http://cdn.debian.net/debian wheezy-backports main" > /etc/apt/sources.list.d/backports.list
  apt-get update && apt-get install -q --yes --force-yes \
    libncursesw5-dev libffi-dev libzmq3-dev zlib1g-dev libzookeeper-mt-dev \
    wget ca-certificates debhelper devscripts
}

stage1_installghc () {
  ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.3 || true
  ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so || true

  wget -O - http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2 | tar -x -j -C /opt
  cd /opt/ghc-7.6.3
  ./configure
  make install

  wget -O - http://www.haskell.org/cabal/release/cabal-install-1.18.0.2/cabal-install-1.18.0.2.tar.gz | tar -x -z -C /opt
  cd /opt/cabal-install-1.18.0.2
  ./bootstrap.sh --global

  cabal install happy --symlink-bindir=/usr/bin
  cabal install alex --symlink-bindir=/usr/bin
}

stage1_installclj () {
  echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" > /etc/apt/sources.list.d/java.list
  echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
  echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
  apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886
  apt-get update && apt-get -q --yes --force-yes \
    install oracle-java7-installer

  wget -O /usr/bin/lein https://raw.github.com/technomancy/leiningen/stable/bin/lein
  chmod 755 /usr/bin/lein
}

if [ "$(basename $0)" = in-target.sh ]
then
  stage1_installpkg
  stage1_installclj
  stage1_installghc
else
  docker_build
fi
