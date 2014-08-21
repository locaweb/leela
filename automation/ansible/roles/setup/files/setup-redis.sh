#!/usr/bin/env bash

basedir=$(dirname $0)

main() {
  place_conf
  place_init
}

place_conf() {
  cp $(basedir)/confs/redis/* /etc/redis/
}

place_init() {
  cp $(basedir)/inits/redis-server /etc/redis/
}

main
