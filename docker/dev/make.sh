#!/bin/sh

docker build -t leela:dev $(dirname $(readlink -f "$0"))
