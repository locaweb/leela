#!/bin/sh

leela_root=${leela_root:-$(pwd)}
leela_pypath="$leela_root/src"
leela_cfg="$leela_root/etc/leela.conf"

echo "leela_env: [root: $leela_root]" >&2
echo "  PYTHONPATH: $leela_pypath"    >&2
echo "  LEELA_CFG:  $leela_cfg"       >&2

exec env PYTHONPATH="$leela_pypath" LEELA_CFG="$leela_cfg" $@
