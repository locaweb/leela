#!/usr/bin/env bash

basedir=$(dirname $0)

main() {
  place_conf
}

place_conf() {
  cp -r $(basedir)/confs/consul/* /etc/consul/
}

main
