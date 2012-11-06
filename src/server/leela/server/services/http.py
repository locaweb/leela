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

from cyclone import web
from twisted.application import service
from twisted.application import internet
from leela.server.network import cassandra_proto
from leela.server.network import http_proto
from leela.server.network import webhandler

def http_service(cfg):
    sto = cassandra_proto.CassandraProto(cfg)
    app = web.Application([ (r"/v1/past24/(.*)", http_proto.Past24, {"storage": sto}),
                            (r"/v1/pastweek/(.*)", http_proto.PastWeek, {"storage": sto}),
                            (r"/v1/(/^\d+$/)/(/^\d+$/)/(/^\d+$/)/(.*)", http_proto.YearMonthDay, {"storage": sto}),
                            (r"/v1/(/^\d+$/)/(/^\d+$/)/(.*)", http_proto.YearMonth, {"storage": sto}),
                            (r".*", webhandler.Always404)
                          ])
    srv = service.MultiService()
    srv.addService(service.IService(internet.TCPServer(cfg.getint("http", "port"),
                                                       app,
                                                       interface=cfg.get("http", "address"))))
    srv.addService(sto)
    return(srv)
