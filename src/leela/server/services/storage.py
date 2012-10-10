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

import struct
from twisted.internet import defer
from telephus.pool import CassandraClusterPool
from leela.server import funcs
from leela.server import logger
from leela.server.network import protocols
from leela.server.storage import cassandra

def scale(e):
    e.set_time((e.year(), e.month(), e.day(), e.hour(), e.minute(), 0))

def parse_srvaddr(s):
    res = s.strip().split(":", 2)
    if (len(res) == 2):
        return((res[0], int(res[1], 10)))
    else:
        return((res[0], 9160))

def encode_string(s):
    try:
        return(s.encode("utf8"))
    except UnicodeError:
        return(s)

class StorageService(CassandraClusterPool):

    def __init__(self, cfg):
        self.cfg   = cfg
        self.bus   = protocols.LeelaBus(self.cfg.get("storage", "pipe"), "r")
        self.bus.attach("cassandra", self)
        parse_addr = lambda s: s.split(":")
        servers    = map(parse_srvaddr, self.cfg.get("storage", "server").split(","))
        keyspace   = self.cfg.get("storage", "keyspace")
        CassandraClusterPool.__init__(self, seed_list=servers, keyspace=keyspace, conn_timeout=60)

    def recv_broadcast(self, events):
        t = funcs.timer_start()
        for e in events:
            scale(e)
            (k0, v0) = cassandra.serialize_event(e, cassandra.DEFAULT_EPOCH)
            k = struct.pack(">i", k0)
            v = struct.pack(">d", v0)
            self.insert(key=encode_string(e.name()), column_family=cassandra.COLFAMILY, value=v, column=k)
        logger.debug("wrote %d events [walltime: %s]" % (len(events), funcs.timer_stop(t)))

    def startService(self):
        logger.warn("starting cassandra service")
        CassandraClusterPool.startService(self)
        self.bus.connect()
        self.bus.startReading()
        self.bus.autoretry(True)

    def stopService(self):
        logger.warn("stoppping cassandra service")
        self.bus.autoretry(False)
        self.bus.disconnect()
        CassandraClusterPool.stopService(self)
