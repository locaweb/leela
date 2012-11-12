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

from twisted.internet import defer
from cyclone import web
from leela.server.network import webhandler
from leela.server.data import parser
from leela.server.data import pp

def render_series(events):
    results = []
    for e in events:
        results.append([e.unixtimestamp(), e.value()])
    return(results)

class Past24(webhandler.LeelaWebHandler):

    @web.asynchronous
    @defer.inlineCallbacks
    @webhandler.logexceptions
    def get(self, key):
        events = yield self.class_data.load_past24(self.storage, key)
        if (events == []):
            raise(web.HTTPError(404))
        self.finish({"status": 200,
                     "results": {key: {"series": render_series(events)}}
                    })

class PastWeek(webhandler.LeelaWebHandler):
        
    @web.asynchronous
    @defer.inlineCallbacks
    @webhandler.logexceptions
    def get(self, key):
        events = yield self.class_data.load_pastweek(self.storage, key)
        if (events == []):
            raise(web.HTTPError(404))
        self.finish({"status": 200,
                     "results": {key: {"series": render_series(events)}}
                    })

class Range(webhandler.LeelaWebHandler):

    @web.asynchronous
    @defer.inlineCallbacks
    @webhandler.logexceptions
    def get(self, key):
        start = self.get_argument("start")
        finish = self.get_argument("finish")
        ys =   int(start[:4], 10)
        ms =   int(start[4:6], 10)
        ds =   int(start[6:8], 10)
        hs =   int(start[9:11], 10)
        mins = int(start[11:], 10)
        yf =   int(finish[:4], 10)
        mf =   int(finish[4:6], 10)
        df =   int(finish[6:8], 10)
        hf =   int(finish[9:11], 10)
        minf = int(finish[11:], 10)
        events = yield self.class_data.load_range(self.storage, key, ys, ms, ds, hs, mins, yf, mf, df, hf, minf)
        if (events == []):
            raise(web.HTTPError(404))
        self.finish({"status": 200,
                     "results": {key: {"series": render_series(events)}}
                    })

class YearMonthDay(webhandler.LeelaWebHandler):

    @web.asynchronous
    @defer.inlineCallbacks
    @webhandler.logexceptions
    def get(self, year, month, day, key):
        year = int(year, 10)
        month = int(month, 10)
        day = int(day, 10)
        events = yield self.class_data.load_day(self.storage, key, year, month, day)
        if (events == []):
            raise(web.HTTPError(404))
        self.finish({"status": 200,
                     "results": {key: {"series": render_series(events)}}
                    })

class YearMonth(webhandler.LeelaWebHandler):

    @web.asynchronous
    @defer.inlineCallbacks
    @webhandler.logexceptions
    def get(self, year, month, key):
        year = int(year, 10)
        month = int(month, 10)
        events = yield self.class_data.load_month(self.storage, key, year, month)
        if (events == []):
            raise(web.HTTPError(404))
        self.finish({"status": 200,
                     "results": {key: {"series": render_series(events)}}
                    })

class CreateData(webhandler.LeelaWebHandler):

    @webhandler.logexceptions
    def put(self, key):
        if (len(self.request.body) > 8192):
            raise(web.HTTPError(400))
        try:
            data = parser.parse_json_data(self.request.body)
        except RuntimeError:
            raise web.HTTPError(400)
        if data.name() != key:
            raise web.HTTPError(400)
        self.databus.broadcast([data])
        self.set_status(201)
        self.finish({"status": 201, "results" : pp.render_storable_to_json(data)})
