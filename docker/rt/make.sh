#!/bin/sh

docker build -t leela:rt $(dirname $(readlink -f "$0"))
