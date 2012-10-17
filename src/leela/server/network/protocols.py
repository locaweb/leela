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
import time
from twisted.internet import protocol
from leela.server.unix import pipe
from leela.server.data import event
from leela.server.network.parsing import *
from leela.server import logger

class DMProcProtocol(protocol.Protocol):

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
                c = l[:1]
                if (c == 'e'):
                    self.recv_event(parse_event(l + ";"))
                elif (c == 's'):
                    self.recv_status(parse_status(l + ";"))
                elif (c == 'c'):
                    parse_string(l, "close")
                    self.recv_close()
                elif (l != ""):
                    raise(RuntimeError())
        except:
            logger.error("bad frame: " + frame)
            logger.exception()
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
        self.send_data(render_event(e))

    def send_events(self, es):
        if (len(es) > 0):
            self.send_data(render_events(es))

    def send_close(self):
        self.send_data("close;")
        self.flush()

    def send_proc(self, proc):
        self.send_data("proc %s;" % proc)

    def recv_event(self, e):
        """
        This is invoked whenever a new event is received
        """

    def recv_close(self):
        """
        Invoked whenever a close msg is received
        """

    def recv_status(self):
        """
        Invoked whenever the servers sends a status message
        """

class UdpProtocol(protocol.DatagramProtocol):

    def parse1(self, string):
        (name, value) = string.split(": ", 2)
        if (" " in value):
            (lval, tval) = value.split(" ", 2)
            tval = long(tval, 10)
        else:
            lval = value
            tval = long(time.time())
        return(event.Event(name[:255], float(lval), tval))

    def datagramReceived(self, string, peer):
        tmp = []
        for l in string.splitlines():
            try:
                tmp.append(self.parse1(l))
            except:
                logger.exception()
        if (len(tmp) > 0):
            self.recv_event(tmp)

class LeelaBus(pipe.UnixPipe):

    def __init__(self, fname, mode="r"):
        pipe.UnixPipe.__init__(self, fname, mode)
        self._rlimit   = 42*1024*1024
        self.residue   = ""
        self.callbacks = {}

    def attach(self, gid, cc):
        self.callbacks[gid] = cc
        logger.info("registering new cc: %s/%d" % (gid, len(self.callbacks)))

    def detach(self, gid):
        if (gid in self.callbacks):
            del(self.callbacks[gid])
        logger.info("unregistering cc: %s/%d" % (gid, len(self.callbacks)))

    def set_residue(self, data):
        if (len(data) < self._rlimit):
            self.residue = data
        else:
            self.residue = ""

    def send_broadcast(self, events):
        self.write_chunks(map(render_event, events))

    def recv_data(self, data0):
        data = self.residue + data0
        tmp  = []
        msgs = []
        for c in data:
            tmp.append(c)
            if (c == ';'):
                e = parse_event_("".join(tmp))
                if (e is None):
                    logger.debug("error parsing: %s" % "".join(tmp))
                    tmp = []
                else:
                    tmp = []
                    msgs.append(e)
        self.set_residue("".join(tmp))
        if (len(msgs) > 0):
            for cc in self.callbacks.values():
                cc.recv_broadcast(msgs)
