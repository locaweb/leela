#!/bin/sh

LEELA_ROOT=${LEELA_ROOT:-$(pwd)}

exec env PYTHONPATH="$LEELA_ROOT/src" LEELA_CFG="$LEELA_ROOT/etc/leela.conf" $@
