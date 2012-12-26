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

from twisted.internet import protocol
from leela.server import logger
from leela.server.data.parser import *

class UDP(protocol.DatagramProtocol):

    def datagramReceived(self, string, peer):
        try:
            if (string == "ping\n"):
                self.handle_ping(peer)
            else:
                self.handle_event(string)
        except:
            logger.exception()

    def handle_ping(self, peer):
        self.transport.write("pong\n", peer)

    def handle_event(self, string):
        tmp = []
        for l in string.splitlines():
            tmp.append(parse_event_legacy(l))
        if (len(tmp) > 0):
            self.recv_event(tmp)
