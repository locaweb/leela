#!/usr/bin/python
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
import collections
import mock
from nose.tools import *
from leela.server.data import event

def test_sort_function_uses_timestamp():
    events  = [event.Event("foobar", 0, random.randint(0, 100)) for _ in range(random.randint(30, 100))]
    events1 = event.sort(events)
    eq_(len(events), len(events1))
    for k in range(len(events) - 1):
        ok_(events1[k].timestamp() <= events1[k+1].timestamp())

def test_event_treats_timestamp_as_utc():
    e = event.Event("foobar", 0, 3661)
    eq_(1970, e.year())
    eq_(1, e.month())
    eq_(1, e.day())
    eq_(1, e.hour())
    eq_(1, e.minute())
    eq_(1, e.second())

def test_event_exports_timestamp_as_utc():
    e = event.Event("foobar", 0, 0)
    eq_(event.Timestamp._make((1970, 1, 1, 0, 0, 0)), e.timestamp())

def test_event_returns_unixtimestamp_asis():
    t = random.randint(0, 24*60*60)
    e = event.Event("foobar", 0, t)
    eq_(t, e.unixtimestamp())

def test_load_time_produces_correct_ranges():
    FakeStorage = collections.namedtuple("Storage", ("load",))
    storage = FakeStorage._make((mock.MagicMock(),))
    event.Event.load_time(storage, "foobar", 1970, 1, 1, 0)
    storage.load.assert_called_once_with("foobar",
                                         event.Timestamp._make((1970, 1, 1, 0, 0, 0)),
                                         event.Timestamp._make((1970, 1, 1, 0, 60, 60)),
                                         60*60)

def test_load_day_produces_correct_ranges():
    FakeStorage = collections.namedtuple("Storage", ("load",))
    storage = FakeStorage._make((mock.MagicMock(),))
    event.Event.load_day(storage, "foobar", 1970, 1, 1)
    storage.load.assert_called_once_with("foobar",
                                         event.Timestamp._make((1970, 1, 1, 0, 0, 0)),
                                         event.Timestamp._make((1970, 1, 1, 24, 60, 60)),
                                         24*60*60)

def test_load_month_produces_correct_ranges():
    FakeStorage = collections.namedtuple("Storage", ("load",))
    storage = FakeStorage._make((mock.MagicMock(),))
    event.Event.load_month(storage, "foobar", 1970, 1)
    storage.load.assert_called_once_with("foobar",
                                         event.Timestamp._make((1970, 1, 0, 0, 0, 0)),
                                         event.Timestamp._make((1970, 1, 31, 24, 60, 60)),
                                         31*24*60*60)

def test_load_month_produces_correct_ranges():
    FakeStorage = collections.namedtuple("Storage", ("load",))
    storage = FakeStorage._make((mock.MagicMock(),))
    event.Event.load_month(storage, "foobar", 1970, 1)
    storage.load.assert_called_once_with("foobar",
                                         event.Timestamp._make((1970, 1, 0, 0, 0, 0)),
                                         event.Timestamp._make((1970, 1, 31, 24, 60, 60)),
                                         31*24*60*60)

def test_set_time_with_the_seconds_component():
    t = random.randint(0, 24*60*60)
    n = random.randint(0, 59)
    e = event.Event("foobar", 0, t)
    o = e.timestamp()
    e.set_time((e.year(), e.month(), e.day(), e.hour(), e.minute(), n))
    eq_(o[:5], e.timestamp()[:5])
    eq_(n, e.second())

def test_set_time_with_the_minute_component():
    t = random.randint(0, 24*60*60)
    n = random.randint(0, 59)
    e = event.Event("foobar", 0, t)
    o = e.timestamp()
    e.set_time((e.year(), e.month(), e.day(), e.hour(), n, e.second()))
    eq_(o[:4], e.timestamp()[:4])
    eq_(o[5:], e.timestamp()[5:])
    eq_(n, e.minute())

def test_set_time_with_the_hour_component():
    t = random.randint(0, 24*60*60)
    n = random.randint(0, 23)
    e = event.Event("foobar", 0, t)
    o = e.timestamp()
    e.set_time((e.year(), e.month(), e.day(), n, e.minute(), e.second()))
    eq_(o[:3], e.timestamp()[:3])
    eq_(o[4:], e.timestamp()[4:])
    eq_(n, e.hour())

def test_set_time_with_the_day_component():
    t = random.randint(0, 24*60*60)
    n = random.randint(1, 28)
    e = event.Event("foobar", 0, t)
    o = e.timestamp()
    e.set_time((e.year(), e.month(), n, e.hour(), e.minute(), e.second()))
    eq_(o[:2], e.timestamp()[:2])
    eq_(o[3:], e.timestamp()[3:])
    eq_(n, e.day())

def test_set_time_with_the_month_component():
    t = random.randint(0, 24*60*60)
    n = random.randint(1, 12)
    e = event.Event("foobar", 0, t)
    o = e.timestamp()
    e.set_time((e.year(), n, e.day(), e.hour(), e.minute(), e.second()))
    eq_(o[0], e.timestamp()[0])
    eq_(o[2:], e.timestamp()[2:])
    eq_(n, e.month())

def test_set_time_with_the_year_component():
    t = random.randint(0, 24*60*60)
    n = random.randint(2000, 2100)
    e = event.Event("foobar", 0, t)
    o = e.timestamp()
    e.set_time((n, e.month(), e.day(), e.hour(), e.minute(), e.second()))
    eq_(o[1:], e.timestamp()[1:])
    eq_(n, e.year())
