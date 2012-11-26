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

import math
import random
import json
from nose.tools import *
from leela.server.data import parser
from leela.server.data import event
from leela.server.data import data

@raises(RuntimeError)
def test_parse_string_raise_if_string_does_not_starts_with():
    parser.parse_string("foobar", "x")

def test_parse_string_passes_if_prefix_is_found():
    eq_("bar", parser.parse_string("foobar", "foo"))

@raises(RuntimeError)
def test_parse_string_is_case_sensitive():
    parser.parse_string("foobar", "Foo")

@raises(RuntimeError)
def test_parse_istring_raise_if_string_does_not_starts_with():
    parser.parse_istring("foobar", "x")

def test_parse_istring_passes_if_prefix_is_found():
    eq_("bar", parser.parse_istring("foobar", "foo"))

def test_parse_istring_is_case_insensitive():
    eq_("bar", parser.parse_istring("foobar", "Foo"))

def test_parse_take_consumes_n_chars():
    eq_(("foo", "bar"), parser.parse_take("foobar", 3))

def test_parse_takewhile_consumes_while_predicate_is_true():
    eq_(("foo", "bar"), parser.parse_takewhile("foobar", lambda c: c in "foo"))

def test_parse_double_is_able_to_parse_nan():
    for nan in ["nan", "NaN"]:
        (num, s) = parser.parse_double("%sfoo" % nan)
        ok_(math.isnan(num))
        eq_("foo", s)

def test_parse_double_is_able_to_parse_inf():
    for inf in ["inf", "Infinity"]:
        (num, s) = parser.parse_double("%sfoo" % inf)
        ok_(math.isinf(num))
        eq_("foo", s)

def test_parse_double_is_able_to_parse_minus_inf():
    for inf in ["-inf", "-Infinity"]:
        (num, s) = parser.parse_double("%sfoo" % inf)
        ok_(math.isinf(num))
        eq_("foo", s)

def test_parse_double_is_able_to_parse_double_numbers():
    tmp = random.random()
    (num, s) = parser.parse_double("%sfoo" % repr(tmp))
    eq_(num, tmp)
    eq_("foo", s)

def test_parse_double_is_able_to_parse_exp_notation():
    (a, b, e) = (random.randint(0, 10), random.randint(0, 10), random.randint(0, 10))
    (num, s) = parser.parse_double("%d.%de%dfoo" % (a, b, e))
    eq_(float("%d.%de%d" % (a,b,e)), num)
    ok_("foo", s)

@raises(RuntimeError)
def test_parse_double_must_fail_if_cant_parse():
    parser.parse_double("foobar")

def test_parse_int():
    tmp = random.randint(0, 2**64)
    (num, s) = parser.parse_int("%dfoo" % tmp)
    eq_(tmp, num)
    eq_("foo", s)

@raises(RuntimeError)
def test_parse_raise_with_negative_numbers():
    parser.parse_int("-10")

def test_parse_event():
    t = random.random()
    v = random.random()
    e0 = event.Event("foobar", v, t)
    (e1, s) = parser.parse_event("event 6|foobar %s %s;" % (repr(t), repr(v)))
    eq_(e0.name(), e1.name())
    eq_(e0.value(), e1.value())
    eq_(e0.unixtimestamp(), e1.unixtimestamp())
    eq_("", s)

def test_parse_data():
    t = random.random()
    v = "{\"one\": 1}"
    d0 = data.Data("foobar", v, t)
    (d1, s) = parser.parse_data("data 6|foobar %s %d|%s;" % (repr(t), len(v), v))
    eq_(d0.name(), d1.name())
    eq_(json.loads(d0.value()), d1.value())
    eq_(d0.unixtimestamp(), d1.unixtimestamp())
    eq_("", s)

def test_parse_event_returns_leftover():
    t = random.random()
    v = random.random()
    e0 = event.Event("foobar", v, t)
    (_, s) = parser.parse_event("event 6|foobar %s %s;foobar" % (repr(t), repr(v)))
    eq_("foobar", s)

def test_parse_data_returns_leftover():
    t = random.random()
    v = "{\"one\": 1}"
    d0 = data.Data("foobar", v, t)
    (_, s) = parser.parse_data("data 6|foobar %s %d|%s;foobar" % (repr(t), len(v), v))
    eq_("foobar", s)

@raises(RuntimeError)
def test_parse_event_must_raise_on_error():
    parser.parse_event("foobar")

@raises(RuntimeError)
def test_parse_data_must_raise_on_error():
    parser.parse_data("foobar")

def test_parse_status():
    tmp = random.randint(0, 10)
    eq_((tmp, ""), parser.parse_status("status %d;" % tmp))

@raises(RuntimeError)
def test_parse_status_must_raise_on_error():
    parser.parse_status("foobar")

def test_parse_event_legacy():
    t = random.randint(0, 10)
    v = random.random()
    e0 = event.Event("foobar", v, t)
    e1 = parser.parse_event_legacy("foobar: %s %s" % (repr(v), t))
    eq_(e0.name(), e1.name())
    eq_(e0.value(), e1.value())
    eq_(e0.unixtimestamp(), e1.unixtimestamp())

def test_parse_select():
    eq_({"select": {"proc": "id", "regex": r"^foob(a|r)$"}},
        parser.parse_select("SELECT id FROM ^foob(a|r)$;"))

@raises(RuntimeError)
def test_parse_select_must_raise_on_error():
    parser.parse_select("foobar")

def test_parse_delete_without_where():
    eq_({"delete": {"key": ""}},
        parser.parse_delete("DELETE FROM leela.xmpp;"))

def test_parse_delete_with_where():
    eq_({"delete": {"key": "foobar"}},
        parser.parse_delete("DELETE FROM leela.xmpp WHERE key=foobar;"))

@raises(RuntimeError)
def test_parse_delete_delete_must_raise_on_error():
    parser.parse_delete("foobar")

def test_parse_sql_is_able_to_parse_both_select_and_delete():
    ok_("select" in parser.parse_sql("SELECT * FROM leela.xmpp;"))
    ok_("delete" in parser.parse_sql("DELETE FROM leela.xmpp;"))

@raises(RuntimeError)
def test_parse_sql_must_raise_on_error():
    parser.parse_sql("foobar")

def test_parse_sql__never_fails():
    eq_({}, parser.parse_sql_("foobar"))

def test_parse_event__never_fails():
    eq_((None, ""), parser.parse_event_("foobar"))

def test_parse_status__never_fails():
    eq_((-1, ""), parser.parse_status_("foobar"))

def test_parse_data__never_fails():
    eq_((None, ""), parser.parse_data_("foobar"))

def test_parse_status_return_leftover():
    x = random.randint(0, 10)
    eq_((x, "foobar"), parser.parse_status("status %d;foobar" % x))
