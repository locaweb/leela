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
from leela.server import config
from leela.server import funcs
from leela.server.network import webhandler
from leela.server.data import parser
from leela.server.data import pp

def render_series(events):
    results = []
    for e in events:
        results.append([e.unixtimestamp(), e.value()])
    return(results)

@defer.inlineCallbacks
@webhandler.logexceptions
def sequece_load(f, key, success, failure):
    t = funcs.timer_start()
    d = lambda: {"walltime": funcs.timer_stop(t)}
    try:
        r = yield f()
        if (r == []):
            failure(404, debug=d())
        else:
            success({"status" : 200,
                     "results": {key: {"series": render_series(r)}},
                     "debug"  : d()
                    })
    except web.HTTPError, e:
        failure(e.status_code, exception=e, debug=d())
    except Exception, e:
        failure(500, exception=e, debug=d())

class Past24(webhandler.LeelaWebHandler):

    @web.asynchronous
    @webhandler.logexceptions
    def get(self, key):
        f = lambda: self.class_.load_past24(self.storage, key)
        sequece_load(f, key, self.finish, self.send_error)

class PastWeek(webhandler.LeelaWebHandler):

    @web.asynchronous
    @webhandler.logexceptions
    def get(self, key):
        f = lambda: self.class_.load_pastweek(self.storage, key)
        sequece_load(f, key, self.finish, self.send_error)

class Range(webhandler.LeelaWebHandler):

    @web.asynchronous
    @webhandler.logexceptions
    def get(self, key):
        start  = parser.parse_timespec(self.get_argument("start"))
        finish = parser.parse_timespec(self.get_argument("finish"))
        args   = list(start) + list(finish)
        f      = lambda: self.class_.load_range(self.storage, key, *args)
        sequece_load(f, key, self.finish, self.send_error)

class YearMonthDay(webhandler.LeelaWebHandler):

    @web.asynchronous
    @webhandler.logexceptions
    def get(self, year, month, day, key):
        f = lambda: self.class_.load_day(self.storage, key, int(year, 10), int(month, 10), int(day, 10))
        sequece_load(f, key, self.finish, self.send_error)

class YearMonth(webhandler.LeelaWebHandler):

    @web.asynchronous
    @webhandler.logexceptions
    def get(self, year, month, key):
        f = lambda: self.class_.load_month(self.storage, key, int(year, 10), int(month, 10))
        sequece_load(f, key, self.finish, self.send_error)

class CreateData(webhandler.LeelaWebHandler):

    @webhandler.logexceptions
    def put(self, key):
        if (len(self.request.body) > config.MAXPACKET):
            raise(web.HTTPError(400))
        data = parser.parse_json_data(self.request.body)
        if data.name() != key:
            raise web.HTTPError(400)
        self.databus.broadcast([data])
        self.set_status(201)
        self.finish({"status": 201, "results" : pp.render_storable_to_json(data)})
