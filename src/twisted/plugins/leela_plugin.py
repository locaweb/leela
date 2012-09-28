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

from zope.interface import implements
from twisted.python import usage
from twisted.plugin import IPlugin
from twisted.application.service import IServiceMaker
from twisted.application import internet
from twisted.python import log
from wokkel.client import XMPPClient
from wokkel.xmppim import JID
from leela.server.services import xmpp
from leela.server.services import storage
from leela.server.services import udp
from leela.server import logger
from leela.server import config

class LeelaOption(usage.Options):
    optParameters = [ ["config", "", config.default_config_file(), "Leela config file to use"],
                      ["service", "", "xmpp", "What leela service to start (xmpp|storage|udp)"],
                      ["log-level", "", "warn", "The log level (debug|info|warn|error)"]
                    ]

class LeelaServiceMk(object):
    implements(IServiceMaker, IPlugin)
    tapname     = "leela"
    description = "Collects anything, monitors anything and analyzes anything"
    options     = LeelaOption

    def xmpp_service(self, cfg):
        host = None
        port = 5222
        user = cfg.get("xmpp", "user")
        pwrd = cfg.get("xmpp", "pwrd")
        if (cfg.has_option("xmpp", "host")):
            host = cfg.get("xmpp", "host")
            if (cfg.has_option("xmpp", "port")):
                port = cfg.getint("xmpp", "port")
        service = XMPPClient(JID(user), pwrd, host, port)
        lepres  = xmpp.PresenceHandler()
        leproto = xmpp.XmppService(cfg)
        lepres.setHandlerParent(service)
        leproto.setHandlerParent(service)
        return(service)

    def storage_service(self, cfg):
        return(storage.StorageService(cfg))

    def udp_service(self, cfg):
        return(udp.UdpService(cfg))

    def makeService(self, options):
        logmap = {"debug": logger.DEBUG,
                  "info": logger.INFO,
                  "warn": logger.WARNING,
                  "error": logger.ERROR
                 }
        logger.set_level(logmap.get(options["log-level"], "warn"))
        cfg = config.read_config(options["config"])
        if (options["service"] == "xmpp"):
            return(self.xmpp_service(cfg))
        elif (options["service"] == "storage"):
            return(self.storage_service(cfg))
        elif (options["service"] == "udp"):
            return(self.udp_service(cfg))
        else:
            raise(RuntimeError("error: unknown service"))

serviceMaker = LeelaServiceMk()
