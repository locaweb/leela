#!/usr/bin/env bash

basedir=$(dirname $0)

main() {
  place_conf
}

place_conf() {
  cp ${basedir}/confs/blackbox/leela-blackbox /etc/default/
}

main
