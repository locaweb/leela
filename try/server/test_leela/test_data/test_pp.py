# -*- coding: utf-8; -*-
#
# Copyright 2012 Juliano Martinez
# Copyright 2012 Diego Souza
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

import random
from nose.tools import *
from leela.server.data import pp
from leela.server.data import event
from leela.server.data import data

def test_render_event():
    t = random.randint(0, 10)
    v = random.random()
    e = event.Event("foobar", v, t)
    eq_("event 6|foobar %s.0 %s;" % (e.unixtimestamp(), repr(v)), pp.render_event(e))

def test_render_events():
    t = random.randint(0, 10)
    v = random.random()
    es = 10 * (event.Event("foobar", v, t),)
    eq_("".join(map(pp.render_event, es)), pp.render_events(es))

def test_render_event_to_json():
    t = random.randint(0, 10)
    v = random.random()
    e = event.Event("foobar", v, t)
    eq_({"name": e.name(), "value": e.value(), "timestamp": e.unixtimestamp()}, pp.render_storable_to_json(e))

def test_render_data_to_json():
    t = random.randint(0, 10)
    v = random.random()
    e = data.Data("foobar", v, t)
    eq_({"name": e.name(), "value": e.value(), "timestamp": e.unixtimestamp()}, pp.render_storable_to_json(e))

def test_render_select():
    eq_("SELECT id FROM regex;", pp.render_select("id", "regex"))

def test_render_storables():
    t = random.randint(0, 10)
    v = random.random()
    es = 10 * (event.Event("foobar", v, t),) + 5 * (data.Data("foobaz", [v], t), )
    eq_("".join(map(pp.render_storable, es)), pp.render_storables(es))

