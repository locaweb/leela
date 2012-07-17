#!/usr/bin/python
# -*- coding: utf-8; -*-

import math
import funcs as f
import copycats
from datetime import datetime
from nose.tools import *
from leela import config
from leela import funcs
from leela.readata import dumper

def test_lasergun_replies_ok_when_everything_goes_fine():
    prefix    = "%s|timestamp||" % f.rand_hostname()
    service   = "timestamp"
    metrics   = [("pi", str(math.pi))]
    cassandra = copycats.Storage()
    with f.lasergun_ctx(cassandra) as (send,recv):
        send(prefix + "||".join(map("|".join, metrics)))
        ok_(float(recv()[0]) > 0)


def test_lasergun_replies_error_when_something_fails():
    hostname  = f.rand_hostname()
    cassandra = copycats.Storage()
    with f.lasergun_ctx(cassandra) as (send,recv):
        send("foobar")
        eq_("error", recv()[0])

def test_lasergun_inserts_data_onto_the_right_time_window():
    hostname  = f.rand_hostname()
    service   = "timestamp"
    prefix    = "%s|%s||" % (hostname, service)
    cassandra = copycats.Storage()
    metrics   = [("pi", str(math.pi))]
    now       = datetime.today()
    with f.lasergun_ctx(cassandra) as (send,recv):
        send(prefix + "||".join(map("|".join, metrics)))
        t_send = datetime.fromtimestamp(float(recv()[0]))
    result    = dumper.dump_day3(config, cassandra, hostname, service, now)
    timestamp = funcs.datetime_timestamp(datetime(t_send.year, t_send.month, t_send.day, t_send.hour, t_send.minute))
    ok_(timestamp in result["pi"])

def test_lasergun_overwrites_data_within_the_same_time_window():
    hostname  = f.rand_hostname()
    service   = "timestamp"
    prefix    = "%s|%s||" % (hostname, service)
    cassandra = copycats.Storage()
    metrics   = [("pi", str(math.pi))]
    now       = datetime.today()
    with f.lasergun_ctx(cassandra) as (send,recv):
        send(prefix + "||".join(map("|".join, metrics)))
        ok_(float(recv()[0]) > 0)
        send(prefix + "||".join(map("|".join, metrics)))
        ok_(float(recv()[0]) > 0)
        send(prefix + "||".join(map("|".join, metrics)))
        ok_(float(recv()[0]) > 0)
    result = dumper.dump_day3(config, cassandra, hostname, service, now)
    eq_(1, len(result["pi"]))
