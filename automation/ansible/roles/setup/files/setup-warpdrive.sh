#!/usr/bin/env bash

basedir=$(dirname $0)

main() {
  place_conf
}

place_conf() {
  cp $(basedir)/confs/warpdrive/leela-warpdrive /etc/default/
}

main
