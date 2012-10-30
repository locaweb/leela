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
from twisted.internet import protocol
from leela.server.data.parser import *
from leela.server.data.pp import *
from leela.server import logger

class DMProc(protocol.Protocol):

    @classmethod
    def proc_match(self, regex):
        return("match %d|%s" % (len(regex), regex))

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
                    self.recv_event(parse_event(l + ";")[0])
                elif (c == 's'):
                    self.recv_status(parse_status(l + ";")[0])
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
            frame = "".join(self.obuffer[1])
            logger.debug("dmproc: sending frame: %s" % frame)
            self.transport.write(struct.pack("!H", self.obuffer[0]))
            self.transport.write(frame)
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

    def send_proc(self, mode, proc):
        self.send_data("proc %s %s;" % (mode, proc))

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
