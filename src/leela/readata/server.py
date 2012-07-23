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

from gevent import monkey
monkey.patch_all()

import json
import bottle
import re
import pycassa
import argparse
import supay
from gevent.wsgi import WSGIServer
from datetime import datetime
from leela import logger
from leela import funcs
from leela import config
from leela import storage
from leela.readata import dumper
from leela.lasergun import data

def reply_json(f):
    def call(*args, **kwargs):
        cc = bottle.request.GET.get("callback") or ""
        try:
            data = f(*args, **kwargs)
        except pycassa.NotFoundException, e:
            bottle.response.status = 404
            data = {"error": 404, "message": "not found"}
        except Exception, e:
            logger.exception("internal server error")
            bottle.response.status = 500
            data = {"error": 500, "message": "internal server error: %s" % str(e)}
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

def decorate_with_source(result, hostname, service, date):
    result["source"] = {"date": date,
                        "hostname": data.norm_hostname(hostname),
                        "service": data.norm_service(service)
                       }
    return(result)

def service_to_sorted_list(result):
    f = lambda k, v, acc: funcs.dict_set(acc, k, v)
    g = lambda kv: sorted([(k, v) for (k, v) in kv.iteritems()], key=lambda kv: kv[0])
    return(funcs.service_reduce(f, g, result, {}))

@bottle.get("/v1/<hostname>/<service>/<year>/<month>/past_24h")
@reply_json
def past24_json(hostname, service, year, month, cfg, cassandra):
    result = service_to_sorted_list(dumper.dump_last24(cfg, cassandra, hostname, service))
    decorate_with_source(result, hostname, service, "past_24h")
    return(result)

@bottle.get("/v1/<hostname>/<service>/<year>/<month>/<day>")
@reply_json
def day_json(hostname, service, year, month, day, cfg, cassandra):
    date    = datetime(int(year), int(month), int(day))
    datestr = funcs.datetime_date(date)
    result  = service_to_sorted_list(dumper.dump_day3(cfg, cassandra, hostname, service, date))
    decorate_with_source(result, hostname, service, datestr)
    return(result)

@bottle.get("/static/<path:path>")
def static(_, __, path):
    return static_file(path, root="/usr/share/leela/static/")

# legacy
@bottle.get("/json/<hostname>/<service>/<date>")
def legacy_json(hostname, service, date, **kwargs):
    datelen = len(date)
    if (datelen == 8):
        return(day_json(hostname, service, date[:4], date[4:6], date[6:], **kwargs))
    else:
        raise(RuntimeError("unsupported operation"))

class GEventServerAdapter(bottle.ServerAdapter):

    @classmethod
    def start(self, cfg, cassandra):
        bottle.install(currying_plugin(cassandra=cassandra, cfg=config))
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
    parser.add_argument("-b", "--background",
                        dest="daemonize",
                        action="store_true",
                        default=False,
                        help="daemonize the process")
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
    cassandra = storage.Storage(cfg)
    logger.debug("starting server")
    GEventServerAdapter.start(cfg, cassandra)

def main():
    opts   = cli_parser().parse_args()
    daemon = supay.Daemon("leela-readata")
    if (opts.debug):
        logger.set_level(logger.DEBUG)
    if (opts.action == "start"):
        if (opts.daemonize):
            daemon.start()
        main_start(opts)
    elif (opts.action == "stop"):
        daemon.stop()
    elif (opts.action == "status"):
        daemon.status()

if (__name__ == "__main__"):
    main()
