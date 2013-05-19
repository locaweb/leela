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
import calendar
from twisted.internet import defer
from telephus.pool import CassandraClusterPool
from leela.server import funcs
from leela.server.data import event
from leela.server.data import data
from leela.server.data import marshall

CF_EVENTS = "events_%02d%04d"
CF_DATA   = "data_%02d%04d"

def parse_srvaddr(s):
    res = s.strip().split(":", 2)
    if (len(res) == 2):
        return((res[0], int(res[1], 10)))
    else:
        return((res[0], 9160))

def encode_string(s):
    try:
        return(s.encode("ascii"))
    except UnicodeError:
        return(s)

def wholemonth(start, finish):
    _, dt = calendar.monthrange(start[0], start[1])
    return (start[2] == 1 and start[3] == 0 and start[4] == 0 and start[5] == 0 and finish[2] == dt and finish[3] == 23 and finish[4] == 59 and finish[5] == 59)

def unserialize_event(k, cols):
    f = lambda col: marshall.unserialize_event(k,
                                               struct.unpack(">i", col.column.name)[0],
                                               struct.unpack(">d", col.column.value)[0],
                                               marshall.DEFAULT_EPOCH)
    return(map(f, cols))

def unserialize_data(k, cols):
    f = lambda col: marshall.unserialize_data(k,
                                              struct.unpack(">i", col.column.name)[0],
                                              col.column.value,
                                              marshall.DEFAULT_EPOCH)
    return(map(f, cols))

def concat_map(f, xs):
    results = []
    map(lambda x: results.extend(f(x)), xs)
    return(results)

def merge(d0, d1):
    d = defer.Deferred()
    def f0(xs):
        d1.addCallback(lambda ys: d.callback(xs + ys))
        d1.addErrback(d.errback)
    d0.addCallback(f0)
    d0.addErrback(d.errback)
    return(d)

class CassandraProto(CassandraClusterPool):

    def __init__(self, cfg):
        self.cfg = cfg
        servers              = map(parse_srvaddr, self.cfg.get("cassandra", "seed").split(","))
        keyspace             = self.cfg.get("cassandra", "keyspace")
        self.request_retries = self.cfg.getint("cassandra", "retries")
        CassandraClusterPool.__init__(self, seed_list=servers, keyspace=keyspace, conn_timeout=self.cfg.getint("cassandra", "timeout"))

    def store(self, s):
        if (s.kind() == event.Event.kind()):
            (k0, v0) = marshall.serialize_event(s, marshall.DEFAULT_EPOCH)
            k  = struct.pack(">i", k0)
            v  = struct.pack(">d", v0)
            cf = CF_EVENTS % (s.month(), s.year())
            return(self.insert(key=encode_string(s.name()), column_family=cf, value=v, column=k))
        elif (s.kind() == data.Data.kind()):
            (k0, v0) = marshall.serialize_data(s, marshall.DEFAULT_EPOCH)
            k  = struct.pack(">i", k0)
            v  = encode_string(v0)
            cf = CF_DATA % (s.month(), s.year())
            return(self.insert(key=encode_string(s.name()), column_family=cf, value=v, column=k))
        else:
            raise(RuntimeError("unknown data type: %s" % s.kind()))

    def enum(self, kind, klimit=100, limit=100):
        delay = defer.Deferred()
        if (kind == event.Event.kind()):
            f = lambda k: unserialize_event(k.key, k.columns)
            d = self.get_range_slices(CF_EVENTS, count=klimit, column_count=limit)
            d.addCallback(lambda xs: concat_map(f, xs))
        else:
            f = lambda k: unserialize_data(k.key, k.columns)
            d = self.get_range_slices(CF_DATA, count=klimit, column_count=limit)
            d.addCallback(lambda xs: concat_map(f, xs))
        d.addCallback(delay.callback)
        d.addErrback(delay.errback)
        return(delay)

    def load(self, kind, key, start, finish, limit=100):
        t0    = funcs.datetime_fromtimestamp(funcs.timetuple_timestamp(start))
        t1    = funcs.datetime_fromtimestamp(funcs.timetuple_timestamp(finish))
        diff  = (t1.month + 12 * (t1.year - t0.year)) - t0.month
        if (t0 > t1):
            raise(ValueError("start > finish"))
        if (diff > 1):
            raise(ValueError("range too wide"))
        k0    = struct.pack(">i", marshall.serialize_key(*start, epoch=marshall.DEFAULT_EPOCH))
        k1    = struct.pack(">i", marshall.serialize_key(*finish, epoch=marshall.DEFAULT_EPOCH))
        f     = None
        delay = defer.Deferred()
        if (kind == event.Event.kind()):
            cf1 = CF_EVENTS % (start[1], start[0])
            cf2 = CF_EVENTS % (finish[1], finish[0])
            f   = unserialize_event
        else:
            cf1 = CF_DATA % (start[1], start[0])
            cf2 = CF_DATA % (finish[1], finish[0])
            f   = unserialize_data
        if (cf1 == cf2):
            if (wholemonth(start, finish)):
                d = self.get_slice(key           = encode_string(key),
                                   count         = limit,
                                   column_family = cf1)
            else:
                d = self.get_slice(key           = encode_string(key),
                                   start         = k1,
                                   finish        = k0,
                                   count         = limit,
                                   column_family = cf1)
        else:
            d = merge(self.get_slice(key           = encode_string(key),
                                     start         = k1,
                                     finish        = k0,
                                     count         = limit,
                                     column_family = cf2),
                      self.get_slice(key           = encode_string(key),
                                     start         = k1,
                                     finish        = k0,
                                     count         = limit,
                                     column_family = cf1))
        d.addCallback(lambda xs: reversed(xs))
        d.addCallback(lambda cols: f(key, cols))
        d.addCallback(delay.callback)
        d.addErrback(delay.errback)
        return(delay)
