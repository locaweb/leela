#!/bin/sh

docker build -t leela:zookeeper $(dirname $(readlink -f "$0"))
