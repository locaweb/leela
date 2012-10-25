#!/bin/sh

leela_tryxmpp_interact () {
  leela_trylib_service_start xmpp >/dev/null
  leela_trylib_xmpp_singleshot
  leela_trylib_service_stop
}
