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

import math
import time
from twisted.internet import defer
from cyclone import web
from leela.server import funcs
from leela.server import logger
from leela.server import config
from leela.server import version
from leela.server.network import resthandler
from leela.server.data import excepts
from leela.server.data import metric
from leela.server.data import parser
from leela.server.data import pp

NAN_ALLOW = 0
NAN_PURGE = 1
HOSTNAME  = config.hostname()

def read_nanopt(x, d=NAN_PURGE):
    return({ "allow": NAN_ALLOW,
             "purge": NAN_PURGE,
           }.get(x, d))

def render_series(events, nan=NAN_ALLOW):
    if (events == []):
        raise(excepts.NotFoundExcept())
    results = []
    accept  = lambda x: not isinstance(x, float) or (not (math.isnan(x) or math.isinf(x)))
    for e in events:
        if (accept(e.value()) or nan == NAN_ALLOW):
            results.append([e.unixtimestamp(), e.value()])
        elif (nan != NAN_PURGE):
            raise(RuntimeError("unknonw value"))
    return(results)

def relay_data(render, relay, data):
    size   = 0
    packet = []
    for x in data:
        packet.append(render(x))
        size += len(packet[-1])
        if (size > 16*1024):
            relay("".join(packet))
            packet = []
            size   = 0
    if (size > 0):
        relay("".join(packet))

def measure_reads(relay, value):
    name0 = "leela.%s.http.keys/s" % (HOSTNAME,)
    name1 = "leela.%s.http.reqs/s" % (HOSTNAME,)
    now   = time.time()
    try:
        relay_data(pp.render_metric, relay, [metric.Absolute(name0, len(value), now),
                                             metric.Absolute(name1, 1, now)
                                            ])
    except:
        logger.error("error instrumenting reads/s")
    return(value)

class EventsResource(resthandler.RestHandler):

    def load_events(self, key, cc):
        nan = read_nanopt(self.get_argument("nan", "purge"))
        t0  = funcs.timer_start()
        t1  = lambda: {"walltime": funcs.timer_stop(t0)}
        cc.addCallback(lambda r: self.finish({"status" : 200,
                                              "results": {key: {"series": render_series(measure_reads(self.timeline.relay, r), nan)}},
                                              "debug"  : t1()
                                             }))\
          .addErrback(self.catch)

class Past24(EventsResource):

    @web.asynchronous
    def get(self, key):
        self.load_events(key, self.class_.load_past24(self.storage, key))

class PastWeek(EventsResource):

    @web.asynchronous
    def get(self, key):
        self.load_events(key, self.class_.load_pastweek(self.storage, key))

class RangeRdonly(EventsResource):

    @web.asynchronous
    @resthandler.catch
    def get(self, key):
        start  = parser.parse_timespec(self.get_argument("start"))
        finish = parser.parse_timespec(self.get_argument("finish"))
        args   = list(start) + list(finish)
        self.load_events(key, self.class_.load_range(self.storage, key, *args))

class RangeDataRdwr(RangeRdonly):

    def put(self, key):
        return(self.post(key))

    @resthandler.catch
    def post(self, key):
        data = parser.parse_json_data(self.request.body, key)
        relay_data(pp.render_storable, self.relay.relay, data)
        self.set_status(201)
        self.finish({"status": 201,
                     "results": pp.render_storables_to_json(data)
                    })

class RangeMetricRdwr(RangeRdonly):

    @resthandler.catch
    def post(self, key):
        data = parser.parse_json_metric(self.request.body, key)
        relay_data(pp.render_metric, self.relay.relay, data)
        self.set_status(201)
        self.finish({"status": 201,
                     "results": pp.render_metrics_to_json(data)
                    })

class YearMonthDay(EventsResource):

    @web.asynchronous
    @resthandler.catch
    def get(self, year, month, day, key):
        self.load_events(key, self.class_.load_day(self.storage, key, int(year, 10), int(month, 10), int(day, 10)))

class YearMonth(EventsResource):

    @web.asynchronous
    @resthandler.catch
    def get(self, year, month, key):
        self.load_events(key, self.class_.load_month(self.storage, key, int(year, 10), int(month, 10)))

class Version(resthandler.RestHandler):

    def get(self):
        self.finish({"version": version.version, "major": version.major, "minor": version.minor, "build": version.build})
