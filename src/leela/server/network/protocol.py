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

from leela.server.data import event

def serialize_event(e):
    return("event %d|%s %d.0 %s;" % (len(e.name()), e.name(), e.unixtimestamp(), repr(e.value())))

def serialize_events(es):
    return("".join(map(serialize_event, es)))

def serialize_proc(cmd):
    return("proc %s;" % cmd)

def serialize_close():
    return("close;")

def parse_event(l):
    if (l.startswith("event ")):
        s  = l.index("|")
        k  = int(l[6:s], 10)
        n  = l[s+1:s+1+k]
        k  = l.index(" ", s+1+k)
        k1 = l.index(" ", k+1)
        t  = float(l[k+1:k1])
        if (l.endswith(";")):
            v = float(l[k1+1:-1])
        else:
            v = float(l[k1+1:])
        return(event.Event(n, v, t))
    else:
        raise(RuntimeError("error: bad format"))

def parse_events(l):
    not_null = lambda s: s!=""
    return(map(parse_event, filter(not_null, l.split(";"))))
