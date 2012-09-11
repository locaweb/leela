#!/usr/bin/python
# -*- coding: utf-8; -*-
#
# All Rights Reserved.
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

from sleekxmpp import ClientXMPP
from leela.server.data import netprotocol
from leela.server import funcs
from leela.server import logger

class XmppStorage(object):

    def __init__(self, cfg):
        self.cfg  = cfg
        self.conn = ClientXMPP(cfg.get("xmpp", "user"), cfg.get("xmpp", "pwrd"))
        self.conn.auto_reconnect = True
        self.conn.auto_subscribe = False
        self.conn.auto_authorize = False
        self.conn.add_event_handler("session_start", self._session_start)

    def connect(self):
        if (self.cfg.has_option("xmpp", "host") and self.cfg.has_option("xmpp", "port")):
            addr = (self.cfg.get("xmpp", "host"), self.cfg.getint("xmpp", "port"))
            self.conn.connect(addr)
        else:
            self.conn.connect()
        self.conn.process(block=False)

    def disconnect(self):
        self.conn.disconnect()

    def _session_start(self, event):
        self.conn.send_presence()
        self.conn.get_roster()

    def store(self, e):
        t = funcs.timer_start()
        self.conn.send_message("cloud@futurama.locaweb.com.br", netprotocol.serialize_json(e))
        logger.debug("sending event to xmpp peer [e: %s, walltime: %f]" % (e, funcs.timer_stop(t)))
