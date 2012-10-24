#!/bin/sh

leela_trystorage_1 () {
  leela_trylib_setup
  leela_trylib_start storage >/dev/null

  

  leela_trylib_stop
  leela_trylib_teardown
}
