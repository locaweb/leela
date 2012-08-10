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
import mock
import collections
import contextlib
from nose.tools import *
from leela.server.data import event
from leela.server.storage import cassandra

def _fakecm(emit):
    @contextlib.contextmanager
    def f(*args, **kwargs):
        yield(emit)
    return(f)

def test_serialize_event_looses_no_information():
    e = event.Event("foobar", random.random(), random.randint(0, 86400))
    (k, v) = cassandra.serialize_event(e, epoch=1970)
    e1 = cassandra.unserialize_event("foobar", k, v, epoch=1970)
    eq_(e.name(), e1.name())
    eq_(e.unixtimestamp(), e1.unixtimestamp())
    eq_(e.timestamp(), e1.timestamp())
    eq_(e.value(), e1.value())

def test_event_storage_uses_serialized_format():
    FakeCf = collections.namedtuple("ColumnFamily", ("insert",))
    column = FakeCf._make((mock.MagicMock(),))
    s = cassandra.EventsStorage(None)
    s.with_column = _fakecm(column)

    e = event.Event("foobar", random.random(), random.randint(0, 86400))
    (k,v) = cassandra.serialize_event(e, epoch=cassandra.DEFAULT_EPOCH)
    s.store(e)
    column.insert.assert_called_once_with(e.name(), {k: v})
