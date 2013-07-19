#!/bin/sh

set -e

echo "deb http://ftp.us.debian.org/debian sid main" >/etc/apt/sources.list.d/sid.list
echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main" > /etc/apt/sources.list.d/java.list
echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections
echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys EEA14886
apt-get update && apt-get install -q --yes --force-yes \
  unzip libzmq3 zlib1g libgmp10 oracle-java7-installer
