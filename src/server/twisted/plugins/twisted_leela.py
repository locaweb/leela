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
from leela.server.services import http
from leela.server.services import udp
from leela.server.services import collectd
from leela.server import logger
from leela.server import config

def read_env(envstr):
    env = {}
    for var in envstr.split(","):
        if (':' not in var):
            continue
        (k, v) = var.split(":", 2)
        env[k.strip()] = v.strip()
    return(env)

class LeelaOption(usage.Options):
    optParameters = [ ["config"   , "", config.default_config_file(), "Leela config file to use"                             ],
                      ["service"  , "", "udp"                       , "What leela service to start (xmpp|storage|udp|http|collectd)"  ],
                      ["log-level", "", "warn"                      , "The log level (debug|info|warn|error)"                ],
                      ["setenv"   , "", ""                          , "Provides options to the service (e.g. setenv=a:b,b:c)"]
                    ]

class LeelaServiceMk(object):
    implements(IServiceMaker, IPlugin)
    tapname     = "leela"
    description = "Distributed, real time event processing and monitoring engine"
    options     = LeelaOption

    def xmpp_service(self, cfg, env):
        host = None
        port = 5222
        user = cfg.get("xmpp", "user")
        pwrd = cfg.get("xmpp", "pwrd")
        if (cfg.has_option("xmpp", "host")):
            host = cfg.get("xmpp", "host")
            if (cfg.has_option("xmpp", "port")):
                port = cfg.getint("xmpp", "port")
        service  = XMPPClient(JID(user), pwrd, host, port)
        presence = xmpp.PresenceHandler()
        leelasrv = xmpp.XmppService(cfg)
        presence.setHandlerParent(service)
        leelasrv.setHandlerParent(service)
        return(service)

    def storage_service(self, cfg, env):
        if (cfg.has_option("storage", "pipe")):
            pipe = env.get("pipe", cfg.get("storage", "pipe"))
        else:
            pipe = env["pipe"]
        return(storage.StorageService(cfg, pipe))

    def udp_service(self, cfg, env):
        return(udp.UdpService(cfg))

    def collectd_service(self, cfg, env):
        return(collectd.CollectdService(cfg))

    def http_service(self, cfg, env):
        srv = http.HttpService(cfg)
        return(srv.get())

    def makeService(self, options):
        logmap = {"debug": logger.DEBUG,
                  "info": logger.INFO,
                  "warn": logger.WARNING,
                  "error": logger.ERROR
                 }
        logger.set_level(logmap.get(options["log-level"], "warn"))
        cfg = config.read_config(options["config"])
        env = read_env(options["setenv"])
        if (options["service"] == "xmpp"):
            return(self.xmpp_service(cfg, env))
        elif (options["service"] == "storage"):
            return(self.storage_service(cfg, env))
        elif (options["service"] == "udp"):
            return(self.udp_service(cfg, env))
        elif (options["service"] == "http"):
            return(self.http_service(cfg, env))
        elif (options["service"] == "collectd"):
            return(self.collectd_service(cfg, env))
        else:
            raise(RuntimeError("error: unknown service"))

serviceMaker = LeelaServiceMk()
