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

import json
from leela.server.data import event
from leela.server.data import data

def render_json(x):
    return(json.dumps(x, allow_nan=True, sort_keys=True))

def render_event(e):
    return("event %d|%s %d.0 %s;" % (len(e.name()), e.name(), e.unixtimestamp(), repr(e.value())))

def render_data(e):
    value = render_json(e.value())
    return("data %d|%s %d.0 %d|%s;" % (len(e.name()), e.name(), e.unixtimestamp(), len(value), value))

def render_events(es):
    return("".join(map(render_event, es)))

def render_storable(s):
    if isinstance(s, event.Event):
        return(render_event(s))
    elif isinstance(s, data.Data):
        return(render_data(s))
    else:
        raise(RuntimeError())

def render_storable_to_json(e):
    return({"name": e.name(), "value": e.value(), "timestamp": e.unixtimestamp()})

def render_storable_to_json_(e):
    return(render_json(render_storable_to_json(e)))

def render_select(proc, regex):
    return("SELECT %s FROM %s;" % (proc, regex))

def render_storables(ss):
    return("".join(map(render_storable, ss)))
