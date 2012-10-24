#!/bin/sh

leela_try_udp_writes_to_pipe () {
  leela_trylib_setup
  leela_trylib_start udp >/dev/null

  echo "foobar: 0.75 1351101449" | leela_trylib_udp_sendto
  leela_trylib_read; echo

  leela_trylib_stop
  leela_trylib_teardown
}
