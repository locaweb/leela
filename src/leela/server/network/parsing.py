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

import re
import json
from leela.server.data import event

def render_event(e):
    return("event %d|%s %d.0 %s;" % (len(e.name()), e.name(), e.unixtimestamp(), repr(e.value())))

def render_events(es):
    if (len(es) == 0):
        return("")
    return("".join(map(render_event, es)))

def render_event_to_json(e):
    return(json.dumps({"name": e.name(), "value": e.value(), "timestamp": e.unixtimestamp()}))

def parse_string(s, w):
    if (s.startswith(w)):
        return(s[len(w):])
    raise(RuntimeError())

def parse_istring(s, w):
    if (s.lower().startswith(w.lower())):
        return(s[len(w):])
    raise(RuntimeError())

def parse_take(s, n):
    return(s[:n], s[n:])

def parse_takewhile(s, p):
    tmp = []
    for c in s:
        if (p(c)):
            tmp.append(c)
        else:
            break
    return(("".join(tmp), s[len(tmp):]))

def parse_double(s):
    (d, s) = parse_takewhile(s, lambda c: c in "0123456789.e-+")
    return(float(d), s)

def parse_int(s, signed=False):
    tmp = []
    for c in s:
        if (not c.isdigit()):
            if (not (signed and c in "+-")):
                break
        tmp.append(c)
    if (len(tmp) == 0):
        raise(RuntimeError())
    try:
        return(int("".join(tmp), 10), s[len(tmp):])
    except:
        raise(RuntimeError())

def parse_event(s):
    s      = parse_string(s, "event ")
    (l, s) = parse_int(s)
    s      = parse_string(s, "|")
    (n, s) = parse_take(s, l)
    s      = parse_string(s, " ")
    (t, s) = parse_double(s)
    s      = parse_string(s, " ")
    (v, s) = parse_double(s)
    if (s == ";"):
        return(event.Event(n, v, t))
    else:
        raise(RuntimeError())

def parse_status(s):
    s      = parse_string(s, "status ")
    (l, s) = parse_int(s)
    if (s == ";"):
        return(l)
    else:
        raise(RuntimeError())

def parse_event_legacy(s):
    (name, value) = string.split(": ", 2)
    if (" " in value):
        (lval, tval) = value.split(" ", 2)
        tval = long(tval, 10)
    else:
        lval = value
        tval = long(time.time())
    return(event.Event(name[:255], float(lval), tval))

def parse_from(s):
    items = []
    for i in s.split(";"):
        t = i.strip()
        if (t != ""):
            items.append(i)
    if (len(items) == 0):
        raise(RuntimeError())
    return(items)

def parse_select(s):
    m = re.match(r"^SELECT (.+?) FROM (.+?);$", s.strip(), re.I)
    r = {}
    if (m):
        return({ "select": { "proc": m.group(1),
                             "from": tuple(sorted(parse_from(m.group(2))))
                           }
               })
    raise(RuntimeError())

def parse_delete(s):
    m = re.match(r"^DELETE FROM leela.xmpp(?: WHERE key=(.+?))?;$", s.strip(), re.I)
    r = {}
    if (m):
        return({ "delete": {"key": m.group(1)}
               })
    raise(RuntimeError())

def parse_sql_(s):
    try:
        if (s[0] in "dD"):
            return(parse_delete(s))
        elif (s[0] in "sS"):
            return(parse_select(s))
    except:
        return({})

def parse_event_(s):
    try:
        return(parse_event(s))
    except:
        return(None)

def parse_status_(s):
    try:
        return(parse_status(s))
    except:
        return(-1)
