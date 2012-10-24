#!/bin/sh

leela_trystorage_1 () {
  leela_trylib_fifo_drop
  leela_trylib_fifo_creat
  leela_trylib_cassandra_drop >/dev/null
  leela_trylib_cassandra_creat >/dev/null
  
  leela_trylib_service_start storage >/dev/null
  echo -n "event 6|foobar 1351101449.0 0.75;" | leela_trylib_fifo_write
  echo "list events;" | leela_trylib_cassandra_execute | grep column=

  leela_trylib_service_stop
}
