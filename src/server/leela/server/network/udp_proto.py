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
            if (string == "ping\n" or string == "ping"):
                self.handle_ping(peer)
            else:
                self.handle_event(string)
        except:
            logger.warn("error parsing packet [peer: %s]" % repr(peer))
            if (logger.level <= logger.DEBUG):
                logger.exception()

    def handle_ping(self, peer):
        self.transport.write("pong", peer)

    def handle_event(self, string):
        if (string.startswith("gauge ") or
            string.startswith("derive ") or
            string.startswith("counter ") or
            string.startswith("absolute ")):
            self.forward_packet(string)
        else:
            tmp = []
            for l in string.splitlines():
                e = parse_event_legacy(l)
                tmp.append("gauge %d|%s %s %d.0;" % (len(e.name()), e.name(), repr(e.value()), e.unixtimestamp()))
            if (len(tmp) > 0):
                self.forward_packet("".join(tmp))
