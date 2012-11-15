#!/bin/sh

leela_trydmproc_interact () {
  leela_trylib_dmproc_start >/dev/null
  leela_trylib_dmproc_interact "$@"; echo
  leela_trylib_dmproc_stop
}
