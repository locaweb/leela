#!/bin/sh

leela_tryxmpp_interact () {
  leela_trylib_service_start xmpp
  leela_trylib_xmpp_singleshot
  leela_trylib_service_stop
}
