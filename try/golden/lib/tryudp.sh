#!/bin/sh

leela_tryudp_1 () {
  leela_trylib_fifo_drop
  leela_trylib_fifo_creat
  leela_trylib_service_start udp >/dev/null

  echo "foobar: 0.75 1351101449" | leela_trylib_udp_write
  leela_trylib_fifo_read; echo

  leela_trylib_service_stop
}
