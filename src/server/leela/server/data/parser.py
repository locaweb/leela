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
import time
from leela.server.data import event

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
    if (s.startswith("nan") or s.startswith("NaN")):
        return(float("nan"), s[3:])
    elif (s.startswith("inf")):
        return(float("inf"), s[3:])
    elif (s.startswith("Infinity")):
        return(float("inf"), s[8:])
    elif (s.startswith("-inf")):
        return(float("-inf"), s[4:])
    elif (s.startswith("-Infinity")):
        return(float("-inf"), s[9:])
    else:
        (d, s) = parse_takewhile(s, lambda c: c in "0123456789.e-+")
        try:
            return(float(d), s)
        except ValueError:
            raise(RuntimeError())

def parse_int(s):
    tmp = []
    for c in s:
        if (not c.isdigit()):
            break
        tmp.append(c)
    if (len(tmp) == 0):
        raise(RuntimeError())
    try:
        return(int("".join(tmp), 10), s[len(tmp):])
    except ValueError:
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
    (name, value) = s.split(": ", 2)
    if (" " in value):
        (lval, tval) = value.split(" ", 2)
        tval = long(tval, 10)
    else:
        lval = value
        tval = long(time.time())
    return(event.Event(name[:255], float(lval), tval))

def parse_select(s):
    m = re.match(r"^SELECT (.+?) FROM (.+?);$", s.strip(), re.I)
    r = {}
    if (m):
        return({ "select": { "proc": m.group(1).strip(),
                             "regex": m.group(2).strip()
                           }
               })
    raise(RuntimeError())

def parse_delete(s):
    m = re.match(r"^DELETE FROM leela.xmpp(?: WHERE key=(.+?))?;$", s.strip(), re.I)
    r = {}
    if (m):
        tmp = (m.group(1) or "").strip()
        return({ "delete": {"key": tmp}
               })
    raise(RuntimeError())

def parse_sql(s):
    if (s[0] in "dD"):
        return(parse_delete(s))
    elif (s[0] in "sS"):
        return(parse_select(s))
    raise(RuntimeError())

def parse_sql_(s):
    try:
        return(parse_sql(s))
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
