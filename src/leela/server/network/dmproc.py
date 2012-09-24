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
import threading
from leela.server.data import event
from leela.server.network import protocol

class DMProc(object):

    def __init__(self, f, on_data):
        self.on_data = on_data
        self.s = socket.socket(socket.AF_UNIX, socket.SOCK_SEQPACKET)
        self.s.connect(f)
        # TODO:use evio, there is no point in using a new thread per connection
        t = threading.Thread(target=self.consume)
        t.setDaemon(True)
        t.start()

    def consume(self):
        while (True):
            frame = self.s.recv(65536)
            if (frame == ""):
                break
            else:
                self.on_data(protocol.parse_events(frame))

    def proc(self, cmd):
        self.s.send(protocol.serialize_proc(cmd))

    def events(self, es):
        self.s.send(protocol.serialize_events(es))

    def event(self, e):
        self.s.send(protocol.serialize_event(e))

    def close(self):
        self.s.send(protocol.serialize_close())
        self.s.close()
