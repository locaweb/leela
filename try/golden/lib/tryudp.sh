#!/bin/sh

leela_tryudp_write_to_socket_and_read_from_fifo () {
  leela_trylib_fifo_drop
  leela_trylib_fifo_creat
  leela_trylib_service_start udp >/dev/null

  leela_trylib_udp_write
  leela_trylib_fifo_read; echo

  leela_trylib_service_stop
}
