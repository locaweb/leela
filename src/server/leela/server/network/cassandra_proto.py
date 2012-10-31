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
from leela.server.data import event
from leela.server.data import data
from leela.server.data import marshall

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

def unserialize_event(k, cols):
    f = lambda col: marshall.unserialize_event(k,
                                               struct.unpack(">I", col.column.name)[0],
                                               struct.unpack(">d", col.column.value)[0],
                                               marshall.DEFAULT_EPOCH)
    return(map(f, cols))

def unserialize_data(k, cols):
    f = lambda col: marshall.unserialize_data(k,
                                              struct.unpack(">I", col.column.name)[0],
                                              col.column.value,
                                              marshall.DEFAULT_EPOCH)
    return(map(f, cols))

def concat_map(f, xs):
    results = []
    map(lambda x: results.extend(f(x)), xs)
    return(results)

class CassandraProto(CassandraClusterPool):

    def __init__(self, cfg):
        self.cfg = cfg
        servers  = map(parse_srvaddr, self.cfg.get("storage", "server").split(","))
        keyspace = self.cfg.get("storage", "keyspace")
        CassandraClusterPool.__init__(self, seed_list=servers, keyspace=keyspace, conn_timeout=60)

    def store(self, s):
        if (s.kind() == event.Event.kind()):
            (k0, v0) = marshall.serialize_event(s, marshall.DEFAULT_EPOCH)
            k = struct.pack(">I", k0)
            v = struct.pack(">d", v0)
            return(self.insert(key=encode_string(s.name()), column_family="events", value=v, column=k))
        elif (s.kind() == data.Data.kind()):
            (k0, v0) = marshall.serialize_data(s, marshall.DEFAULT_EPOCH)
            k = struct.pack(">I", k0)
            v = encode_string(v0)
            return(self.insert(key=encode_string(s.name()), column_family="data", value=v, column=k))
        else:
            raise(RuntimeError("unknown data type: %s" % s.kind()))

    def enum(self, kind, klimit=100, limit=100):
        delay = defer.Deferred()
        if (kind == event.Event.kind()):
            f = lambda k: unserialize_event(k.key, k.columns)
            d = self.get_range_slices("events", count=klimit, column_count=limit)
            d.addCallback(lambda xs: concat_map(f, xs))
        else:
            f = lambda k: unserialize_data(k.key, k.columns)
            d = self.get_range_slices("data", count=klimit, column_count=limit)
            d.addCallback(lambda xs: concat_map(f, xs))
        d.addCallback(delay.callback)
        d.addErrback(delay.errback)
        return(delay)

    def load(self, kind, key, start, finish, limit=100):
        k0    = struct.pack(">I", marshall.serialize_key(*start, epoch=marshall.DEFAULT_EPOCH))
        k1    = struct.pack(">I", marshall.serialize_key(*finish, epoch=marshall.DEFAULT_EPOCH))
        delay = defer.Deferred()
        if (kind == event.Event.kind()):
            d = self.get_slice(key           = encode_string(key),
                               start         = k0,
                               finish        = k1,
                               reverse       = True,
                               count         = limit,
                               column_family = "events")
            d.addCallback(lambda cols: unserialize_event(key, cols))
        else:
            d = self.get_slice(key           = encode_string(key),
                               column_family = "data",
                               start         = k0,
                               finish        = k1,
                               reverse       = True,
                               count         = limit)
            d.addCallback(lambda cols: unserialize_data(key, cols))
        d.addCallback(delay.callback)
        d.addErrback(delay.errback)
        return(delay)
