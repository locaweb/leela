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

import time
import pycassa
import socket
from contextlib import contextmanager
from pycassa.pool import ConnectionPool
from pycassa.types import CompositeType
from pycassa.types import Int32Type
from pycassa.types import DoubleType
from pycassa.types import UTF8Type
from pycassa.system_manager import SystemManager
from leela.server import logger
from leela.server import funcs
from leela.server.data import event

DEFAULT_EPOCH = 2000

def connect(config, poolsize=None):
    if (poolsize is None):
        poolsize = config.getint("cassandra", "pool_size")
    server_list = config.get("cassandra", "server").split()
    return(pycassa.ConnectionPool(keyspace        = config.get("cassandra", "keyspace"),
                                  server_list     = server_list,
                                  pool_size       = len(server_list)*2,
                                  max_overflow    = len(server_list)*10,
                                  timeout         = 5,
                                  recycle         = 1000,
                                  pool_timeout    = config.getint("cassandra", "timeout"),
                                  max_retries     = config.getint("cassandra", "retries"),
                                  prefill         = False,
                                  use_threadlocal = True))

def create_schema(config):
    keyspace       = config.get("cassandra", "keyspace")
    manager        = config.get("cassandra", "system_manager")
    sysmgr         = pycassa.system_manager.SystemManager(manager, timeout=config.getint("cassandra", "timeout"))
    try:
        schema         = sysmgr.get_keyspace_column_families(config.get("cassandra", "keyspace"))
        events         = EventsStorage(None)
        if (events.name() not in schema):
            events.create(sysmgr, keyspace)
    finally:
        sysmgr.close()

def serialize_key(y, mo, d, h, mi, s, epoch):
    y  = ((y-epoch) << 26) & 0xfc000000
    mo = (mo << 22) & 0x3c00000
    d  = (d << 17) & 0x3e0000
    h  = (h << 12) & 0x1f000
    mi = (mi << 6) & 0xfc0
    s  = s & 0x3f
    k  = y | mo | d | h | mi | s
    return(k)

def unserialize_key(k, epoch):
    y  = ((k >> 26) & 0x3F) + epoch
    mo = ((k >> 22) & 0xF)
    d  = ((k >> 17) & 0x1F)
    h  = (k >> 12) & 0x1F
    mi = (k >> 6) & 0x3f
    s  = k & 0x3f
    return(y, mo, d, h, mi, s)

def serialize_event(e, epoch):
    timestamp = (e.year(), e.month(), e.day(), e.hour(), e.minute(), e.second())
    k = serialize_key(*timestamp, epoch=epoch)
    v = e.value()
    return((k, v))

def unserialize_event(name, k, v, epoch):
    timetuple = list(unserialize_key(k, epoch=epoch))
    timestamp = funcs.timetuple_timestamp(timetuple)
    return(event.Event(name, v, timestamp))

class CassandraCF(object):

    def __init__(self, conn):
        self.pool = conn

    @contextmanager
    def with_column(self, name):
        yield(pycassa.ColumnFamily(self.pool, name))

class EventsStorage(CassandraCF):

    def name(self):
        return("events")

    def create(self, sysmgr, keyspace):
        logger.info("creating %s column family on %s" % (self.name(), keyspace))
        row_type = UTF8Type()
        val_type = DoubleType()
        col_type = Int32Type(reversed=True)
        sysmgr.create_column_family(keyspace,
                                    self.name(),
                                    super                    = False,
                                    default_validation_class = val_type,
                                    key_validation_class     = row_type,
                                    comparator_type          = col_type)

    def store(self, e):
        (k, v) = serialize_event(e, DEFAULT_EPOCH)
        with self.with_column(self.name()) as cf:
            cf.insert(e.name(), {k: v})

    def load(self, name, start, finish, count):
        k_start  = serialize_key(*start, epoch=DEFAULT_EPOCH)
        k_finish = serialize_key(*finish, epoch=DEFAULT_EPOCH)
        f = lambda (k, v): unserialize_event(name, k, v, epoch=DEFAULT_EPOCH)
        with self.with_column(self.name()) as cf:
            cols = cf.get(name, column_start=k_start, column_finish=k_finish, column_reversed=True, column_count=count, include_timestamp=False)
            return(map(f, cols.iteritems()))
