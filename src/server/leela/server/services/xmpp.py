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
import hashlib
from wokkel import xmppim
from twisted.internet import defer
from twisted.internet import task
from twisted.internet import reactor
from twisted.internet.protocol import Factory
from twisted.internet.endpoints import UNIXClientEndpoint
from twisted.words.protocols.jabber.xmlstream import toResponse
from leela.server.network import dmproc_proto
from leela.server.data import parser
from leela.server.data import pp
from leela.server import funcs
from leela.server import logger

class PresenceHandler(xmppim.PresenceProtocol):

    def subscribedReceived(self, presence):
        pass

    def unsubscribedReceived(self, presence):
        pass

    def subscribeReceived(self, presence):
        logger.warn("subscription received: %s/%s" % (presence.sender, presence.recipient))
        self.subscribe(presence.sender)
        self.subscribed(presence.sender)

    def unsubscribeReceived(self, presence):
        logger.warn("unsubscription received: %s/%s" % (presence.sender, presence.recipient))
        self.unsubscribed(presence.sender)
        self.unsubscribe(presence.sender)

    def probeReceibed(self, presence):
        self.available(presence.sender, status=u"Zug Zug")

    def connectionMade(self):
        xmppim.PresenceProtocol.connectionMade(self)
        self.available(status=u"Zug Zug")

class Connection(object):

    def __init__(self, key, peer, query, xmpp):
        self.peer  = peer
        self.query = query
        self.xmpp  = xmpp
        self.key   = key
        self.core  = dmproc_proto.DMProc()
        self.core.recv_event     = self.on_core_recv_event
        self.core.recv_status    = self.on_core_recv_status
        self.core.connectionLost = self.on_core_connection_lost
        self.core.connectionMade = self.on_core_connection_made

    def _sendmsg(self, status, msg):
        xml = xmppim.Message(recipient=self.peer).toElement()
        sql = pp.render_select(self.query["proc"], self.query["regex"])
        dbg = {"request": {"cmd": sql, "key": self.key}}
        self.xmpp.mkcc(xml, dbg)(status, msg)

    def on_core_recv_event(self, e):
        logger.debug("core_recv_event: %s" % e)
        msg = {"results": {"event": pp.render_event_to_json(e)}}
        msg.update(pp.render_event_to_json(e)) # do not break compatibility with previous versions
        self._sendmsg(200, msg)

    def on_core_connection_lost(self, _):
        logger.info("core_connection_lost: %s" % self.key)
        del(self.xmpp.active[self.key])

    def on_core_connection_made(self):
        logger.warn("core_connection_made: %s" % self.key)
        mode = self.core.proc_match(self.query["regex"].encode("utf8"))
        proc = self.query["proc"].encode("utf8")
        self.core.send_proc(mode, proc)
        self.core.flush()
        self.xmpp.active[self.key] = self

    def on_core_recv_status(self, s):
        logger.debug("core_recv_status: %d" % s)
        if (s == 0):
            self._sendmsg(200, {"results": {"key": self.key}})
        else:
            self._sendmsg(500, {"reason": "bad command"})

    def shutdown(self):
        logger.warn("disconnecting: %s" % self.key)
        self.core.send_close()

    def factory(self):
        f = Factory()
        f.protocol = lambda: self.core
        return(f)

class XmppService(xmppim.MessageProtocol):

    def __init__(self, cfg):
        self.cfg     = cfg
        self.active  = {}
        self.redis   = None
        self.core    = UNIXClientEndpoint(reactor, self.cfg.get("xmpp", "dmproc"))
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
            if ((request["select"]["regex"] == "leela.xmpp") and (request["select"]["proc"] == "*")):
                data0 = yield self.redis.hgetall("leela.xmpp")
                tmp   = []
                for (key, data1) in data0.iteritems():
                    data = json.loads(data1)
                    sql  = pp.render_select(data["request"]["select"]["proc"],
                                            data["request"]["select"]["regex"])
                    if (data["sender"] == sender.userhost()):
                        tmp.append({"key": key,
                                    "cmd": sql
                                   })
                cc(200, {"results": tmp})
            else:
                hcode = hashlib.sha512()
                hcode.update(sender.userhost())
                hcode.update(request["select"]["proc"])
                hcode.update(request["select"]["regex"])
                key   = hcode.hexdigest()
                self.redis.hsetnx("leela.xmpp", key, json.dumps({"request": request, "sender": sender.userhost()}))
        except:
            logger.exception()
            cc(500, {"reason": "internal server error"})

    @defer.inlineCallbacks
    def handle_delete_all(self, sender, cc):
        data0 = yield self.redis.hgetall("leela.xmpp")
        keys  = []
        for (key, data1) in data0.iteritems():
            data = json.loads(data1)
            if (data["sender"] == sender.userhost()):
                keys.append({"key": key})
                self.redis.hdel("leela.xmpp", key)
        cc(200, {"results": keys})

    @defer.inlineCallbacks
    def handle_delete_one(self, key, sender, cc):
        data0 = yield self.redis.hget("leela.xmpp", key)
        if (data0 is not None):
            data = json.loads(data0)
            if (data["sender"] == sender.userhost()):
                yield self.redis.hdel("leela.xmpp", key)
                cc(200, {"results": {"key": key}})
            else:
                cc(403, {"reason": "forbidden"})
        else:
            cc(404, {"reason": "not found"})

    def handle_delete(self, request, sender, cc):
        try:
            key = request["delete"]["key"]
            if (key is None):
                self.handle_delete_all(sender, cc)
            else:
                self.handle_delete_one(key, sender, cc)
        except:
            logger.exception()
            cc(500, {"reason": "internal server error"})

    def handle_request(self, request, sender, cc):
        if ("select" in request):
            self.handle_select(request, sender, cc)
        elif ("delete" in request):
            self.handle_delete(request, sender, cc)
        else:
            cc(400, {"reason": "unknow command"})

    def mkcc(self, envelope, debug={}):
        def f(status, results=None):
            if (results is None):
                envelope.addElement("body", content=json.dumps({"status": status, "debug": debug}))
            else:
                msg = dict(results)
                msg.update({"status": status, "debug": debug})
                envelope.addElement("body", content=json.dumps(msg))
            self.send(envelope)
        return(f)

    def onMessage(self, message):
        if (message.body is None):
            return
        sender = xmppim.JID(message.getAttribute("from"))
        req    = parser.parse_sql_(unicode(message.body))
        debug  = {"request": unicode(message.body)}
        resp   = xmppim.Message(recipient = sender).toElement()
        cc     = self.mkcc(resp, debug)
        if (req in [{}, None]):
            logger.debug("message [%s] - 400" % message.body)
            cc(400, {"reason": "parse error"})
        else:
            logger.debug("message [%s] %s - 200" % (message.getAttribute("from"), message.body))
            self.handle_request(req, sender, cc)

    @defer.inlineCallbacks
    def connectionMade(self):
        try:
            self.redis = yield redis.ConnectionPool(self.cfg.get("redis", "host"),
                                                    self.cfg.getint("redis", "port"),
                                                    self.cfg.getint("redis", "db"),
                                                    True)
            self.pooling.start(5)
            xmppim.MessageProtocol.connectionMade(self)
        except:
            logger.exception()

    def connectionLost(self, reason):
        xmppim.MessageProtocol.connectionLost(self, reason)
        funcs.suppress(self.pooling.stop)()
        funcs.suppress(self.redis.disconnect)()
