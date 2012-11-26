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

import collections
import time
from datetime import timedelta
from leela.server import funcs

TFF       = 30*60 # 1/2 hour (fuge factor)
Timestamp = collections.namedtuple("Timestamp", ("year", "month", "day", "hour", "minute", "second"))

class Storable(object):

    @classmethod
    def kind(self):
        raise(RuntimeError("abstract method"))

    @classmethod
    def load_range(self, storage, k, ys, ms, ds, hs, mins, yf, mf, df, hf, minf):
        start  = Timestamp._make((ys, ms, ds, hs, mins, 0))
        finish = Timestamp._make((yf, mf, df, hf, minf, 60))
        return(storage.load(self.kind(), k, start, finish, 60*60))

    @classmethod
    def load_time(self, storage, k, y, m, d, h):
        start  = Timestamp._make((y, m, d, h, 0, 0))
        finish = Timestamp._make((y, m, d, h, 60, 60))
        return(storage.load(self.kind(), k, start, finish, 60*60))

    @classmethod
    def load_day(self, storage, k, y, m, d):
        start  = Timestamp._make((y, m, d, 0, 0, 0))
        finish = Timestamp._make((y, m, d, 24, 60, 60))
        return(storage.load(self.kind(), k, start, finish, 24*60*60))

    @classmethod
    def load_month(self, storage, k, y, m):
        start  = Timestamp._make((y, m, 0, 0, 0, 0))
        finish = Timestamp._make((y, m, 31, 24, 60, 60))
        return(storage.load(self.kind(), k, start, finish, 31*24*60*60))

    @classmethod
    def load_past24(self, storage, k):
        now      = time.time()
        pasttime = funcs.datetime_fromtimestamp(now) - timedelta(days=1, seconds=TFF)
        currtime = funcs.datetime_fromtimestamp(now) + timedelta(seconds=TFF)
        start    = Timestamp._make((pasttime.year, pasttime.month, pasttime.day, pasttime.hour, pasttime.minute, pasttime.second))
        finish   = Timestamp._make((currtime.year, currtime.month, currtime.day, currtime.hour, currtime.minute, currtime.second))
        return(storage.load(self.kind(), k, start, finish, 2*24*60*60))

    @classmethod
    def load_pastweek(self, storage, k):
        now      = time.time()
        pasttime = funcs.datetime_fromtimestamp(now) - timedelta(days=7, seconds=TFF)
        currtime = funcs.datetime_fromtimestamp(now) + timedelta(seconds=TFF)
        start    = Timestamp._make((pasttime.year, pasttime.month, pasttime.day, pasttime.hour, pasttime.minute, pasttime.second))
        finish   = Timestamp._make((currtime.year, currtime.month, currtime.day, currtime.hour, currtime.minute, currtime.second))
        return(storage.load(self.kind(), k, start, finish, 7*24*60*60))

    @classmethod
    def enum(self, storage, klimit=100, limit=100):
        return(storage.enum(self.kind(), klimit, limit))

    def __init__(self, name, value, timestamp):
        d = funcs.datetime_fromtimestamp(timestamp)
        self.n = funcs.norm_key(name)
        self.v = value
        self.t = timestamp
        self.d = Timestamp._make((d.year, d.month, d.day, d.hour, d.minute, d.second))

    def year(self):
        return(self.d.year)

    def month(self):
        return(self.d.month)

    def day(self):
        return(self.d.day)

    def hour(self):
        return(self.d.hour)

    def minute(self):
        return(self.d.minute)

    def second(self):
        return(self.d.second)

    def set_time(self, timetuple):
        self.t = funcs.timetuple_timestamp(timetuple)
        self.d = Timestamp._make(timetuple)

    def name(self):
        return(self.n)

    def value(self):
        return(self.v)

    def unixtimestamp(self):
        return(self.t)

    def timestamp(self):
        return(self.d)

    def store(self, storage):
        storage.store(self)

    def __str__(self):
        return(unicode(self).encode("utf-8"))

    def __unicode__(self):
        return(u"name:%s value:%s time:%d" % (self.n, repr(self.v), self.t))
