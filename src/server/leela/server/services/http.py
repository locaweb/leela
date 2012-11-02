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
from leela.server import logger
from leela.server.network import http_proto
from twisted.application import service, internet

class HttpService(service.Service):

    def __init__(self, cfg):
        self.cfg = cfg
        self.webapp = cyclone.web.Application([ (r"/", http_proto.HttpProto()) ])

    def startService(self):
        logger.warn("starting http service")
        application = service.Application("http")
        server = internet.TCPServer(self.cfg.get("port"), self.webapp, interface=self.cfg.get("host"))
        server.setServiceParent(application)

    def stopService(self):
        logger.warn("stoppping http service")
        service.Service.stopService(self)
