#!/usr/bin/python
# -*- coding: utf-8; -*-

import math
import funcs as f
import copycats
from datetime import datetime
from nose.tools import *
from leela import config
from leela import funcs

def test_lasergun_replies_timestamp_when_everything_goes_fine():
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
        send("noise")
        eq_("error", recv()[0])
