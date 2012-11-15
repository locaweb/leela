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
from leela.server.network import resthandler
from leela.server.data import event
from leela.server.data import data
from leela.server.network.databus import mkbus

def x(*args):
    print(args)

class HttpService(service.Service):

    def __init__(self, cfg):
        self.cfg = cfg
        self.bus = mkbus(cfg.get("http", "broadcast"))
        self.sto = cassandra_proto.CassandraProto(self.cfg)
        self.databus = mkbus(cfg.get("http", "broadcast"))
        self.app = web.Application([
            (r"^/v1/past24/(.*)"                , http_proto.Past24      , {"storage": self.sto, "class_": event.Event}),
            (r"^/v1/pastweek/(.*)"              , http_proto.PastWeek    , {"storage": self.sto, "class_": event.Event}),
            (r"^/v1/range/(.*)"                 , http_proto.Range       , {"storage": self.sto, "class_": event.Event}),
            (r"^/v1/(\d+)/(\d+)/(\d+)/(.*)"     , http_proto.YearMonthDay, {"storage": self.sto, "class_": event.Event}),
            (r"^/v1/(\d+)/(\d+)/(.*)"           , http_proto.YearMonth   , {"storage": self.sto, "class_": event.Event}),

            (r"^/v1/data/past24/(.*)"           , http_proto.Past24      , {"storage": self.sto, "class_": data.Data}),
            (r"^/v1/data/pastweek/(.*)"         , http_proto.PastWeek    , {"storage": self.sto, "class_": data.Data}),
            (r"^/v1/data/range/(.*)"            , http_proto.Range       , {"storage": self.sto, "class_": data.Data}),
            (r"^/v1/data/(\d+)/(\d+)/(\d+)/(.*)", http_proto.YearMonthDay, {"storage": self.sto, "class_": data.Data}),
            (r"^/v1/data/(\d+)/(\d+)/(.*)"      , http_proto.YearMonth   , {"storage": self.sto, "class_": data.Data}),
            
            (r"^/v1/data/(.*)"                  , http_proto.CreateData  , {"databus": self.databus}),
            (r".*"                              , resthandler.Always404)
            ])
        self.srv = service.MultiService()
        self.srv.addService(service.IService(internet.TCPServer(self.cfg.getint("http", "port"),
                                                           self.app,
                                                           interface=self.cfg.get("http", "address"))))
        self.srv.addService(self.sto)


    def get(self):
        return self.srv
