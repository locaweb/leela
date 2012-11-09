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

import socket
import errno
from twisted.internet import protocol
from twisted.internet import reactor
from leela.server import logger
from leela.server.data.pp import *
from leela.server.data.parser import *

def connect_to(sock):
    conn = lambda proto: reactor.connectUNIXDatagram(sock, proto, 32*1024)
    dbus = Databus(conn)
    dbus.connect()
    return(dbus)

def listen_from(sock):
    conn = lambda proto: reactor.listenUNIXDatagram(sock, proto, 32*1024)
    dbus = Databus(conn)
    dbus.connect()
    return(dbus)

def x(*args):
    print(args)

def mkbus(string):
    result = []
    for group in string.split(","):
        tmp = map(lambda s: s.strip(), group.split(";"))
        logger.warn("creating new broadcast group (RR): " + ", ".join(tmp))
        result.append(RoundRobin(map(lambda f: connect_to(f), tmp)))
    return(result)

class RoundRobin(object):

    def __init__(self, ring):
        self.ring = ring

    def getnext(self):
        x = self.ring.pop(0)
        self.ring.append(x)
        return(x)

class Databus(protocol.ConnectedDatagramProtocol):

    def __init__(self, connect):
        self.callbacks = {}
        self.connect   = lambda: connect(self)
        self.wqueue    = []

    def attach(self, gid, cc):
        self.callbacks[gid] = cc
        logger.info("registering new cc: %s/%d" % (gid, len(self.callbacks)))

    def detach(self, gid):
        if (gid in self.callbacks):
            del(self.callbacks[gid])
        logger.info("unregistering cc: %s/%d" % (gid, len(self.callbacks)))

    def send_broadcast(self, events):
        self.wqueue.extend(events)
        logger.debug("send_broadcast: [qlen=%d]" % len(self.wqueue))
        while (len(self.wqueue) > 0 and self.transport is not None):
            e = self.wqueue[:10]
            try:
                self.transport.write(render_events(e))
                del(self.wqueue[:10])
            except socket.error, se:
                map(lambda x: self.wqueue.insert(0, x), reversed(e))
                if (se.args[0] not in (errno.EWOULDBLOCK, errno.EAGAIN)):
                    self.transport.loseConnection()
                break

    def connectionLost(self, reason):
        logger.warn("connectionLost: %s" % reason.getErrorMessage())
        reactor.callLater(1, self.connect)

    def connectionFailed(self, reason):
        logger.warn("connectionLost: %s" % reason.getErrorMessage())
        reactor.callLater(1, self.connect)

    def connectionRefused(self):
        logger.warn("connectionRefused")
        reactor.callLater(1, self.connect)

    def stopProtocol(self):
        logger.warn("stopProtocol")

    def startProtocol(self):
        logger.warn("starProtocol")

    def datagramReceived(self, data, *args):
        tmp  = []
        msgs = []
        for c in data:
            tmp.append(c)
            if (c == ';'):
                tmp = "".join(tmp)
                x   = None
                if (tmp[0] == 'e'):
                    x = parse_event_(tmp)[0]
                elif (tmp[0] == 'd'):
                    x = parse_data_(tmp)[0]
                if (x is None):
                    logger.debug("error parsing: %s" % tmp)
                    tmp = []
                else:
                    tmp = []
                    msgs.append(x)
        if (len(msgs) > 0):
            for cc in self.callbacks.values():
                cc.recv_broadcast(msgs)
