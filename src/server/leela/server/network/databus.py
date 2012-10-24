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

from leela.server import logger
from leela.server.data.pp import *
from leela.server.unix import pipe
from leela.server.data import event
from leela.server.data.parser import *

class Databus(pipe.UnixPipe):

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
