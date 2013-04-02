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

from leela.server.data.metric import *
import struct

TYPE_HOST            = 0x0000
TYPE_TIME            = 0x0001
TYPE_TIME_HI         = 0x0008
TYPE_PLUGIN          = 0x0002
TYPE_PLUGIN_INSTANCE = 0x0003
TYPE_TYPE            = 0x0004
TYPE_TYPE_INSTANCE   = 0x0005
TYPE_VALUES          = 0x0006
TYPE_INTERVAL        = 0x0007
TYPE_INTERVAL_HI     = 0x0009
TYPE_MESSAGE         = 0x0100
TYPE_SEVERITY        = 0x0101
TYPE_SIGNATURE       = 0x0200
TYPE_ENCRYPTION      = 0x0210
MY_TYPE_SUFFIX       = -1
 
DS_COUNTER           = 0
DS_GAUGE             = 1
DS_DERIVE            = 2
DS_ABSOLUTE          = 3

PA_PARSERS           = None
DS_PARSERS           = None

def parse_packet(packet):
    offset = 0
    psize  = len(packet)
    parts  = []
    while (offset < psize):
        pa_type, _    = struct.unpack_from(">2H", packet[offset:])
        item, pa_size = PA_PARSERS.get(pa_type, parse_skip)(packet[offset:])
        offset       += pa_size
        parts.append((pa_type, item))
    return(metrics(parts))

def valid(x):
    return(x not in (None, ""))

def join_(s, *args):
    return(s.join(filter(valid, args)))

def format(metric, value, context):
    host   = context.get(TYPE_HOST)
    plugin = join_("-", context.get(TYPE_PLUGIN), context.get(TYPE_PLUGIN_INSTANCE))
    mtype  = join_("-", context.get(TYPE_TYPE), context.get(TYPE_TYPE_INSTANCE))
    key    = join_(".", host, plugin, mtype, context.get(MY_TYPE_SUFFIX))
    if (TYPE_TIME_HI in context):
        mtime = context[TYPE_TIME_HI] / float(2**30)
    else:
        mtime = context[TYPE_TIME]
    if (metric == DS_COUNTER):
        return(Counter(key, value, mtime))
    elif (metric == DS_ABSOLUTE):
        return(Absolute(key, value, mtime))
    elif (metric == DS_DERIVE):
        return(Derive(key, value, mtime))
    elif (metric == DS_GAUGE):
        return(Gauge(key, value, mtime))
    else:
        raise(RuntimeError())

def metrics(parts):
    tree    = [TYPE_PLUGIN, TYPE_PLUGIN_INSTANCE, TYPE_TYPE, TYPE_TYPE_INSTANCE]
    context = {}
    types   = {DS_COUNTER: "counter",
               DS_ABSOLUTE: "absolute",
               DS_GAUGE: "gauge",
               DS_DERIVE: "derive"
              }
    metrics = []
    for (ptype, pvalue) in parts:
        if (ptype == TYPE_PLUGIN):
            for k in (TYPE_PLUGIN_INSTANCE, TYPE_TYPE, TYPE_TYPE_INSTANCE):
                if (k in context):
                    del(context[k])
        context[ptype] = pvalue
        if (ptype == TYPE_VALUES):
            counter = -1
            for metric, value in pvalue:
                counter += 1
                if (len(pvalue) > 1):
                    context[MY_TYPE_SUFFIX] = str(counter)
                elif (MY_TYPE_SUFFIX in context):
                    del(context[MY_TYPE_SUFFIX])
                metrics.append(format(metric, value, context))
    return(metrics)

def parse_string(packet):
    _, plen = struct.unpack_from(">2H", packet)
    offset  = 4
    value   = packet[offset:plen-1]
    return(value.decode("ascii"), plen)

def parse_skip(packet):
    _, plen = struct.unpack_from(">2H", packet)
    return(None, plen)

def parse_numeric(packet):
    _, plen = struct.unpack_from(">2H", packet)
    offset  = 4
    value   = struct.unpack_from(">q", packet[offset:])[0]
    return(value, plen)

def parse_values_(size, packet):
    vtypes = []
    values = []
    offset = 0
    for _ in range(size):
        vtypes.append(struct.unpack_from("B", packet[offset:])[0])
        offset += 1
    for vtype in vtypes:
        vpart, vsize = DS_PARSERS[vtype](packet[offset:])
        offset      += vsize
        values.append((vtype, vpart))
    return(values, offset)

def parse_values(packet):
    offset        = 4
    values        = struct.unpack_from(">H", packet[offset:])[0]
    offset       += 2
    vparts, vsize = parse_values_(values, packet[offset:])
    return(vparts, offset + vsize)

PA_PARSERS = {TYPE_HOST           : parse_string,
              TYPE_TIME           : parse_numeric,
              TYPE_TIME_HI        : parse_numeric,
              TYPE_PLUGIN         : parse_string,
              TYPE_PLUGIN_INSTANCE: parse_string,
              TYPE_TYPE           : parse_string,
              TYPE_TYPE_INSTANCE  : parse_string,
              TYPE_VALUES         : parse_values,
              TYPE_INTERVAL       : parse_numeric,
              TYPE_INTERVAL_HI    : parse_numeric,
              TYPE_MESSAGE        : parse_string,
              TYPE_SEVERITY       : parse_numeric
             }

DS_PARSERS = {DS_COUNTER : lambda p: (struct.Struct(">Q").unpack_from(p)[0], 8),
              DS_GAUGE   : lambda p: (struct.Struct("<d").unpack_from(p)[0], 8),
              DS_DERIVE  : lambda p: (struct.Struct(">q").unpack_from(p)[0], 8),
              DS_ABSOLUTE: lambda p: (struct.Struct(">Q").unpack_from(p)[0], 8)
             }
