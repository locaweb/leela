#!/bin/sh

leela_tryudp_write_to_socket_and_read_from_socket () {
  rm -f $dbusfile

  (leela_trylib_xsock_read $dbusfile; echo)&
  leela_trylib_wait_file $dbusfile

  leela_trylib_service_start udp >/dev/null
  leela_trylib_wait_inet $pidfile udp:6968 >/dev/null

  leela_trylib_udp_write $dbusfile
  wait

  leela_trylib_service_stop
}
