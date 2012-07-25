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
# @author: Juliano Martinez
# @author: Diego Souza

import time
from datetime import datetime
from datetime import timedelta
from leela import funcs
from leela import config
from leela.lasergun.data import Lasergun

def dump_day3(config, cassandra, hostname, service, field_f, date):
    laser   = Lasergun(config, cassandra=cassandra, carbon=None)
    rawdata = laser.retrieve(hostname, service, funcs.datetime_timestamp(date))
    data    = dict([(k,v) for (k,v) in rawdata.iteritems() if (field_f(k))])
    f     = lambda slot: funcs.slot_to_timestamp(date.year, date.month, date.day, slot)
    return(funcs.service_map_slot(f, data))

def dump_day(config, cassandra, hostname, service, fields):
    return(dump_day3(config, cassandra, hostname, service, fields, datetime.today()))

def dump_last24(config, cassandra, hostname, service, fields):
    today          = datetime.today()
    yesterday      = today - timedelta(days=1)
    today_data     = dump_day3(config, cassandra, hostname, service, fields, today)
    yesterday_data = dump_day3(config, cassandra, hostname, service, fields, yesterday)
    mintime        = time.time() - (24.0 * 60 * 60)
    f = lambda k, v, acc: funcs.dict_update(funcs.dict_merge, acc, k, v)
    g = lambda kv: dict([(k,v) for (k,v) in kv.iteritems() if (k > mintime)])
    return(funcs.service_reduce(f, g, yesterday_data, dict(today_data)))
