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

from gevent import monkey
monkey.patch_all()

import math
import json
import bottle
import re
import pycassa
import argparse
import supay
import time
from gevent.wsgi import WSGIServer
from datetime import datetime
from leela.server import logger
from leela.server import funcs
from leela.server import config
from leela.server.data.event import Event
from leela.server.storage import cassandra

def reply_json(f):
    def call(*args, **kwargs):
        cc = bottle.request.GET.get("callback") or ""
        try:
            cache = int(bottle.request.GET.get("max-cache", "300"))
            data  = f(*args, **kwargs)
        except pycassa.NotFoundException, e:
            cache = 0
            bottle.response.status = 404
            data = {"error": 404, "message": "not found"}
        except Exception, e:
            cache = 0
            logger.exception("internal server error")
            bottle.response.status = 500
            data = {"error": 500, "message": "internal server error: %s" % str(e)}
        if (cache > 0):
            bottle.response.set_header("Cache-control", "max-age=%d" % cache)
        if (re.match("^[a-zA-Z0-9_\.]+$", cc)):
            bottle.response.content_type = "application/javascript; charset=utf-8"
            return("%s(%s);" % (cc, json.dumps(data)))
        else:
            bottle.response.content_type = "application/json; charset=utf-8"
            return(json.dumps(data))
    call.__name__ = f.__name__
    return(call)

def currying_plugin(*gparams, **gkparams):
    def proxy_f(f):
        def call(*params, **kparams):
            args   = gparams + params
            kwargs = dict(gkparams)
            kwargs.update(kparams)
            return(f(*args, **kwargs))
        return(call)
    return(proxy_f)

def events_to_json(events):
    result = {}
    for e in events:
        k = e.name()
        if (k not in result):
            result[k] = {"series": []}
        if (not math.isnan(e.value())):
            result[k]["series"].append((e.unixtimestamp(), e.value()))
    return(result)

def wrap_results(reqtime, events):
    data = events_to_json(events)
    uri  = bottle.request.path
    if (bottle.request.query_string):
        uri += "?" + bottle.request.query_string
    return({"results": data,
            "debug": {"request_uri": uri,
                      "request_time": reqtime
                     }
           })

@bottle.get("/v1/past24/:key#.+#")
@reply_json
def past24_json(key, cfg, connection):
    t       = funcs.timer_start()
    storage = cassandra.EventsStorage(connection)
    events  = Event.load_past24(storage, key)
    return(wrap_results(funcs.timer_stop(t), events))

@bottle.get("/v1/pastweek/:key#.+#")
@reply_json
def pastweek_json(key, cfg, connection):
    t       = funcs.timer_start()
    storage = cassandra.EventsStorage(connection)
    events  = Event.load_pastweek(storage, key)
    return(wrap_results(funcs.timer_stop(t), events))

@bottle.get("/v1/<year:int>/<month:int>/<day:int>/:key#.+#")
@reply_json
def day_json(year, month, day, key, cfg, connection):
    t       = funcs.timer_start()
    storage = cassandra.EventsStorage(connection)
    past    = bottle.request.GET.get("past")
    if (past is None):
        events = Event.load_day(storage, key, year, month, day)
    else:
        hour   = datetime.today().hour - int(past, 10)
        events = Event.load_time(storage, key, year, month, day, hour)
    return(wrap_results(funcs.timer_stop(t), events))

@bottle.get("/v1/<year:int>/<month:int>/:key#.+#")
@reply_json
def month_json(year, month, key, cfg, connection):
    t       = funcs.timer_start()
    storage = cassandra.EventsStorage(connection)
    past    = bottle.request.GET.get("past")
    events  = Event.load_month(storage, key, year, month)
    return(wrap_results(funcs.timer_stop(t), events))

class GEventServerAdapter(bottle.ServerAdapter):

    @classmethod
    def start(self, cfg, connection):
        bottle.install(currying_plugin(cfg=cfg, connection=connection))
        bottle.run(host=cfg.get("readata", "address"), port=cfg.getint("readata", "port"), server=self)

    def run(self, handler):
        WSGIServer((self.host, self.port), handler).serve_forever()

def cli_parser():
    parser = argparse.ArgumentParser("readata: rest interface to export data")
    parser.add_argument("-u", "--user",
                        dest="user",
                        type=str,
                        default="nobody",
                        help="the owner of this process [%(default)s]")
    parser.add_argument("-g", "--gid",
                        dest="gid",
                        type=str,
                        default="nogroup",
                        help="the group owner of this process [%(default)s]")
    parser.add_argument("-a", "--action",
                        dest="action",
                        type=str,
                        default="start",
                        choices=["start", "stop", "status"])
    parser.add_argument("-f", "--foreground",
                        dest="daemonize",
                        action="store_false",
                        default=True,
                        help="do not daemonize the process")
    parser.add_argument("-d", "--debug",
                        dest="debug",
                        action="store_true",
                        default=False,
                        help="turn debug messages on")
    parser.add_argument("-c", "--config",
                        dest="config",
                        type=str,
                        default=config.default_config_file(),
                        help="the config file to use [%(default)s]")
    return(parser)

def main_start(opts):
    cfg = config.read_config(opts.config)
    funcs.drop_privileges(opts.user, opts.gid)
    connection = cassandra.connect(cfg)
    logger.debug("starting server")
    GEventServerAdapter.start(cfg, connection)

def main():
    opts   = cli_parser().parse_args()
    daemon = supay.Daemon("leela-readata")
    if (opts.debug):
        logger.set_level(logger.DEBUG)
    else:
        logger.set_level(logger.INFO)
    if (opts.action == "start"):
        if (opts.daemonize):
            daemon.start()
            logger.use_syslog()
        else:
            logger.use_console()
        main_start(opts)
    elif (opts.action == "stop"):
        daemon.stop()
    elif (opts.action == "status"):
        daemon.status()

if (__name__ == "__main__"):
    main()
