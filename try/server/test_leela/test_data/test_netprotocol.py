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

from nose.tools import *
from leela.server.data import netprotocol

def test_parse1_should_create_an_event_with_the_correct_name():
    e = netprotocol.parse1("foobar: 1")
    eq_("foobar", e.name())

def test_parse1_should_create_an_event_with_the_correct_value():
    e = netprotocol.parse1("foobar: 0.123456789")
    eq_(float("0.123456789"), e.value())

def test_parse1_should_create_an_event_with_the_correct_timestamp():
    e = netprotocol.parse1("foobar: 1 60")
    eq_(60, e.unixtimestamp())

def test_parse_should_read_multiple_events():
    es = netprotocol.parse("foobar: 0\nfoobar: 1\nfoobar: 2")
    eq_(3, len(es))
