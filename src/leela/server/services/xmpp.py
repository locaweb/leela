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

import fnmatch
import uuid
import json
import txredisapi as redis
from wokkel import xmppim
from twisted.internet import defer
from twisted.internet import task
from twisted.internet import reactor
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import UNIXClientEndpoint
from twisted.words.protocols.jabber.xmlstream import toResponse
from leela.server.network import parsing
from leela.server.network import protocols
from leela.server import funcs
from leela.server import logger

class PresenceHandler(xmppim.PresenceProtocol):

    def subscribedReceived(self, presence):
        pass

    def unsubscribedReceived(self, presence):
        pass

    def subscribeReceived(self, presence):
        self.subscribed(recipient=presence.sender, sender=presence.recipient)
        self.available(recipient=presence.sender, status=u"Zug Zug", sender=presence.recipient)

    def unsubscribeReceived(self, presence):
        self.unsubscribed(recipient=presence.sender, sender=presence.recipient)

    def probeReceived(self, presence):
        self.available(recipient=presence.sender, status=u"Zug Zug", sender=presence.recipient)

    def connectionMade(self):
        self.available()

class Connection(object):

    def __init__(self, key, peer, query, xmpp):
        self.peer  = peer
        self.query = query
        self.xmpp  = xmpp
        self.key   = key
        self.core  = protocols.DMProcProtocol()
        self.core.recv_event     = self.on_core_recv_event
        self.core.connectionLost = self.on_core_connection_lost
        self.core.connectionMade = self.on_core_connection_made

    def on_core_recv_event(self, e):
        bdy = parsing.render_event_to_json(e)
        xml = xmppim.Message(recipient=self.peer, body=unicode(bdy)).toElement()
        self.xmpp.send(xml)

    def on_core_connection_lost(self, _):
        logger.info("dmproc connection lost: %s" % self.key)
        del(self.xmpp.active[self.key])
        self.xmpp.bus.detach(self.key)

    def on_core_connection_made(self):
        logger.warn("dmproc connection established: %s" % self.key)
        self.core.send_proc(self.query["proc"].encode("utf8"))
        self.core.flush()
        self.xmpp.bus.attach(self.key, self)
        self.xmpp.active[self.key] = self

    def shutdown(self):
        logger.warn("disconnecting: %s" % self.key)
        self.core.send_close()

    def should_accept(self, e):
        for g in self.query["from"]:
            if (fnmatch.fnmatch(e.name(), g)):
                return(True)
        return(False)

    def recv_broadcast(self, events0):
        events = []
        for e in events0:
            if (self.should_accept(e)):
                events.append(e)
        self.core.send_events(events)
        self.core.flush()

    def factory(self):
        f = Factory()
        f.protocol = lambda: self.core
        return(f)

class XmppService(xmppim.MessageProtocol):

    def __init__(self, cfg):
        self.cfg     = cfg
        self.active  = {}
        self.redis   = None
        self.bus     = protocols.LeelaBus(self.cfg.get("xmpp", "pipe"), "r")
        self.core    = UNIXClientEndpoint(reactor, self.cfg.get("core", "socket"), 30, False)
        self.pooling = task.LoopingCall(self.redis_pooling)

    @defer.inlineCallbacks
    def redis_pooling(self):
        data0 = yield self.redis.hgetall("leela.xmpp")
        for (key, conn) in list(self.active.iteritems()):
            if (key not in data0):
                logger.warn("reaping connection: %s" % (key,))
                conn.shutdown()
        for (key, data1) in data0.iteritems():
            data = json.loads(data1)
            if (key not in self.active):
                logger.debug("spawning connection: %s: %s" % (key, data["sender"]))
                conn = Connection(key, xmppim.JID(data["sender"]), data["request"]["select"], self)
                self.core.connect(conn.factory())

    @defer.inlineCallbacks
    def handle_select(self, request, sender, cc):
        try:
            if (request["select"]["from"] == ("leela.xmpp",) and request["select"]["proc"] == "*"):
                data0 = yield self.redis.hgetall("leela.xmpp")
                tmp   = []
                for (key, data1) in data0.iteritems():
                    data = json.loads(data1)
                    sql  = "SELECT %s FROM %s;" % (data["request"]["select"]["proc"],
                                                   ",".join(data["request"]["select"]["from"])
                                                  )
                    if (xmppim.JID(data["sender"]).userhost() == sender.userhost()):
                        tmp.append({key: sql})
                cc({"status": 200,
                    "results": tmp
                   })
            else:
                key  = str(uuid.uuid1())
                self.redis.hset("leela.xmpp", key, json.dumps({"request": request, "sender": sender.full()}))
                cc({"status": 200,
                    "results": {"key": key}
                   })
        except:
            logger.exception()
            cc({"status": 500})

    @defer.inlineCallbacks
    def handle_delete(self, request, sender, cc):
        try:
            key   = request["delete"]["key"]
            data0 = yield self.redis.hget("leela.xmpp", key)
            if (data0 is not None):
                data = json.loads(data0)
                if (xmppim.JID(data["sender"]).userhost() == sender.userhost()):
                    yield self.redis.hdel("leela.xmpp", key)
                    cc({"status": 200})
                else:
                    cc({"status": 403})
            else:
                cc({"status": 404})
        except:
            logger.exception()
            cc({"status": 500})

    def handle_request(self, request, sender, cc):
        if ("select" in request):
            self.handle_select(request, sender, cc)
        elif ("delete" in request):
            self.handle_delete(request, sender, cc)
        else:
            cc({"status": 400})

    def onMessage(self, message):
        if (message.getAttribute("type") != "chat" or message.body is None):
            return
        sender = xmppim.JID(message.getAttribute("from"))
        req    = parsing.parse_sql_(unicode(message.body))
        def cc(msg):
            resp = toResponse(message, message.getAttribute("type"))
            resp.addElement("body", content=json.dumps(msg))
            self.send(resp)
        if (req is None):
            cc({"status": 400})
        else:
            logger.debug("message [%s] %s" % (message.getAttribute("from"), message.body))
            self.handle_request(req, sender, cc)

    @defer.inlineCallbacks
    def connectionMade(self):
        try:
            self.redis = yield redis.ConnectionPool(self.cfg.get("redis", "host"),
                                                    self.cfg.getint("redis", "port"),
                                                    self.cfg.getint("redis", "db"),
                                                    True)
            self.bus.connect()
            self.bus.autoretry(True)
            self.pooling.start(5)
            xmppim.MessageProtocol.connectionMade(self)
        except:
            logger.exception()

    @defer.inlineCallbacks
    def connectionLost(self, reason):
        xmppim.MessageProtocol.connectionLost(self, reason)
        funcs.suppress(self.pooling.stop)()
        yield funcs.suppress(self.redis.disconnect)()
        self.bus.autoretry(False)
        self.bus.disconnect()
