#!/bin/sh

leela_tryhttp_write_to_socket_and_read_http () {
  rm -f $dbusfile

  leela_trylib_cassandra_interact -m truncate
  leela_trylib_service_start storage >/dev/null
  leela_trylib_wait_file $dbusfile
  leela_trylib_xsock_write $dbusfile
  leela_trylib_service_stop

  leela_trylib_service_start http >/dev/null
  leela_trylib_curl ${http_endpoint}${1}
  leela_trylib_service_stop http
}

leela_tryhttp_read_http () {
  rm -f $dbusfile

  leela_trylib_cassandra_interact -m truncate

  leela_trylib_service_start http >/dev/null
  leela_trylib_curl ${http_endpoint}${1}
  leela_trylib_service_stop http
}
