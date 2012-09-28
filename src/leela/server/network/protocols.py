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

import struct
import uuid
from twisted.internet import protocol
from twisted.python import log
from leela.server.unix import pipe
from leela.server.data import event
from leela.server.network.parsing import *
from leela.server import logger

class DMProc(protocol.Protocol):

    def __init__(self, buffersz=65536):
        self.obuffer  = [0, []]
        self.ibuffer  = [0, buffer("")]
        self.buffersz = buffersz

    def dataReceived(self, data):
        self.ibuffer[1] = self.ibuffer[1] + buffer(data)
        while (len(self.ibuffer[1]) >= 2):
            if (self.ibuffer[0] == 0):
                (l,) = struct.unpack("!H", self.ibuffer[1][:2])
                self.ibuffer[0] = l
                self.ibuffer[1] = buffer(self.ibuffer[1], 2)
            fsize = self.ibuffer[0]
            if (len(self.ibuffer[1]) >= fsize):
                frame = self.ibuffer[1][:fsize]
                self.ibuffer[0] = 0
                self.ibuffer[1] = buffer(self.ibuffer[1], fsize)
                self.recv_frame(frame)

    def recv_frame(self, frame):
        try:
            for l in frame.split(";"):
                if (l.startswith("e")):
                    self.recv_event(parse_event(l + ";"))
                elif (l == "close;"):
                    self.recv_close()
                elif (l != ""):
                    raise(RuntimeError(l))
        except:
            self.transport.loseConnection()

    def send_data(self, msg):
        l = len(msg)
        if (self.obuffer[0]+l > self.buffersz):
            self.flush()
        self.obuffer[0] += l
        self.obuffer[1].append(msg)

    def flush(self):
        if (self.obuffer[0] > 0):
            self.transport.write(struct.pack("!H", self.obuffer[0]))
            self.transport.write("".join(self.obuffer[1]))
            self.obuffer[0] = 0
            self.obuffer[1] = []

    def send_event(self, e):
        self.send_data("%s;" % render_event(e))

    def send_events(self, es):
        if (len(es) > 0):
            self.send_data("%s;" % "".join(map(render_event, es)))

    def send_close(self):
        self.send_data("close;")
        self.flush()

    def send_proc(self, proc):
        self.send_data("proc %s;" % proc)

    def recv_event(self, e):
        """
        This is invoked whenever a new event is received.
        """

    def recv_close(self):
        """
        Invoked whenever a close msg is received
        """

class LeelaBus(pipe.UnixPipeReader):

    def __init__(self, fname):
        pipe.UnixPipeReader.__init__(self, fname)
        self.residue   = ""
        self.callbacks = {}

    def attach(self, gid, cc):
        self.callbacks[gid] = cc
        logger.info("registering new cc: %s/%d" % (gid, len(self.callbacks)))

    def detach(self, gid):
        if (gid in self.callbacks):
            del(self.callbacks[gid])
        logger.info("unregistering cc: %s/%d" % (gid, len(self.callbacks)))

    def recv_data(self, data0):
        data = self.residue + data0
        tmp  = []
        msgs = []
        for c in data:
            tmp.append(c)
            if (c == ';'):
                e   = parse_event_("".join(tmp))
                tmp = []
                if (e is None):
                    break
                msgs.append(e)
        self.residue = "".join(tmp)
        if (len(msgs) > 0):
            for cc in self.callbacks.values():
                cc.recv_broadcast(msgs)
