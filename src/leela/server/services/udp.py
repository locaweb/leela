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
#

from twisted.internet import reactor
from twisted.application.service import Service
from leela.server import logger
from leela.server.network import protocols
from leela.server.storage import cassandra

class UdpService(Service, protocols.UdpProtocol):

    def __init__(self, cfg):
        broadcast = map(lambda s: s.strip(), cfg.get("udp", "broadcast").split(","))
        self.cfg  = cfg
        self.bus  = map(lambda fn: protocols.LeelaBus(fn, "w"), broadcast)
        self.conn = None

    def broadcast(self, events):
        def f(bus):
            bus.send_broadcast(events)
        map(f, self.bus)

    def recv_event(self, events):
        logger.debug("recv_events: %d" % len(events))
        self.broadcast(events)

    def startService(self):
        def f(bus):
            bus.connect()
            bus.autoretry(True)
        map(f, self.bus)
        self.conn = reactor.listenUDP(self.cfg.getint("udp", "port"), self, interface=self.cfg.get("udp", "address"))

    def stopService(self):
        self.conn.stopListening()
        def f(bus):
            bus.autoretry(False)
            bus.disconnect()
        map(f, self.bus)
