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
from leela.server.data import marshall
from telephus.pool import CassandraClusterPool

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

class CassandraProto(CassandraClusterPool):

    def __init__(self, cfg):
        self.cfg = cfg
        servers  = map(parse_srvaddr, self.cfg.get("storage", "server").split(","))
        keyspace = self.cfg.get("storage", "keyspace")
        CassandraClusterPool.__init__(self, seed_list=servers, keyspace=keyspace, conn_timeout=60)

    def store(self, s):
        if (s.kind() == "event"):
            (k0, v0) = marshall.serialize_event(s, marshall.DEFAULT_EPOCH)
            k = struct.pack(">i", k0)
            v = struct.pack(">d", v0)
            self.insert(key=encode_string(s.name()), column_family="events", value=v, column=k)
        elif (s.kind() == "data"):
            (k0, v0) = marshall.serialize_data(s, marshall.DEFAULT_EPOCH)
            k = struct.pack(">i", k0)
            v = encode_string(v0)
            self.insert(key=encode_string(s.name()), column_family="data", value=v, column=k)
        else:
            raise(RuntimeError("unknown data type: %s" % s.kind()))
