#!/bin/sh

leela_trystorage_write_to_fifo_and_enum_from_cassandra () {
  rm -f $dbusfile

  leela_trylib_cassandra_drop >/dev/null
  leela_trylib_cassandra_creat >/dev/null
  
  leela_trylib_service_start storage >/dev/null
  leela_trylib_wait_file $dbusfile
  leela_trylib_xsock_write $dbusfile
  echo "list events;" | leela_trylib_cassandra_execute

  leela_trylib_service_stop
}
