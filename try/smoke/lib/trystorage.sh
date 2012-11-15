#!/bin/sh

leela_trystorage_write_to_socket_and_enum_events_from_cassandra () {
  rm -f $dbusfile

  leela_trylib_cassandra_interact -m truncate >/dev/null

  leela_trylib_service_start storage >/dev/null
  leela_trylib_wait_file $dbusfile
  leela_trylib_xsock_write $dbusfile
  leela_trylib_cassandra_interact -m enum

  leela_trylib_service_stop
}

leela_trystorage_write_to_socket_and_enum_data_from_cassandra () {
  rm -f $dbusfile

  leela_trylib_cassandra_interact -m truncate
  
  leela_trylib_service_start storage >/dev/null
  leela_trylib_wait_file $dbusfile
  leela_trylib_xsock_write $dbusfile
  leela_trylib_cassandra_interact -m enum

  leela_trylib_service_stop
}
