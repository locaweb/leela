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

import cyclone.web
from twisted.internet import reactor
from twisted.application import service
from twisted.application import internet
from leela.server.network import http_proto

def http_service(cfg):
    app = cyclone.web.Application([ (r"/", http_proto.HttpProto())
                                  ])
    srv = internet.TCPServer(cfg.getint("http", "port"), app, interface=cfg.get("http", "address"))
    return(service.IService(srv))
