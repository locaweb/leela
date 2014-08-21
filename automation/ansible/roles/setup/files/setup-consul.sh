#!/usr/bin/env bash

basedir=$(dirname $0)

main() {
  place_conf
}

place_conf() {
  if [ ! -e /etc/consul ]; then
    mkdir /etc/consul
  fi

  cp -r ${basedir}/confs/consul/* /etc/consul/
}

main
