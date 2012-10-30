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

from twisted.application import service
from leela.server import funcs
from leela.server import logger
from leela.server.network import cassandra_proto
from leela.server.network import databus

def scale(e):
    e.set_time((e.year(), e.month(), e.day(), e.hour(), e.minute(), 0))

class StorageService(service.Service):

    def __init__(self, cfg, sock):
        self.cfg     = cfg
        self.dbus    = databus.listen_from(sock)
        self.storage = cassandra_proto.CassandraProto(cfg)

    def recv_broadcast(self, objects):
        t = funcs.timer_start()
        for obj in objects:
            scale(obj)
            obj.store(self.storage)
        logger.debug("wrote %d events [walltime: %s]" % (len(objects), funcs.timer_stop(t)))

    def startService(self):
        service.Service.startService(self)
        logger.warn("starting cassandra service")
        self.dbus.attach("storage", self)
        self.storage.startService()

    def stopService(self):
        logger.warn("stoppping cassandra service")
        self.dbus.detach("storage")
        self.storage.stopService()
        service.Service.stopService(self)
