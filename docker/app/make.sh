#!/bin/sh

docker build -t leela:app $(dirname $(readlink -f "$0"))
