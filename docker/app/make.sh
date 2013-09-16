#!/bin/sh

cp dist/leela.tar.gz docker/app/leela.tar.gz
docker build -t leela:app $(dirname $(readlink -f "$0"))
rm -f docker/app/leela.tar.gz
