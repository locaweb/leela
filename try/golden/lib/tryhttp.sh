#!/bin/sh

leela_tryhttp_write_to_socket_and_read_http () {
  rm -f $dbusfile
  now=$($bin_date +%s)

  leela_trylib_cassandra_interact -m truncate
  $bin_sed s/__NOWUXTIME__/$now.0/ | leela_trylib_cassandra_interact -m write

  leela_trylib_service_start http >/dev/null
  leela_trylib_curl ${http_endpoint}${1} | $bin_sed s/$now/__NOWUXTIME__/
  leela_trylib_service_stop http
}

leela_tryhttp_read_http () {
  rm -f $dbusfile

  leela_trylib_cassandra_interact -m truncate

  leela_trylib_service_start http >/dev/null
  leela_trylib_curl ${http_endpoint}${1}
  leela_trylib_service_stop http
}
