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
from twisted.python import log
from leela.server.data import event

def parse_string(s, w):
    if (s.startswith(w)):
        return(s[len(w):])
    return("")

def parse_take(s, n):
    return(s[:n], s[n:])

def parse_double(s):
    (d, s) = parse_int(s)
    s      = parse_string(s, ".")
    (r, s) = parse_int(s)
    return(float("%d.%d" % (d ,r)), s)

def parse_int(s):
    tmp = []
    for c in s:
        if (not c.isdigit()):
            break
        tmp.append(c)
    return(int("".join(tmp), 10), s[len(tmp):])

def parse_event(s):
    s      = parse_string(s, "event ")
    (l, s) = parse_int(s)
    s      = parse_string(s, "|")
    (n, s) = parse_take(s, l)
    s      = parse_string(s, " ")
    (t, s) = parse_double(s)
    s      = parse_string(s, " ")
    (v, s) = parse_double(s)
    if (s == ";"):
        return(event.Event(n, t, v))
    else:
        raise(RuntimeError())

def render_event(e):
    return("event %d|%s %d.0 %s" % (len(e.name()), e.name(), e.unixtimestamp(), repr(e.value())))

class DMProto(protocol.Protocol):

    def __init__(self):
        self.buffer = [0, []]
        self.limit  = 2**16-1

    def dataReceived(self, msg):
        try:
            for l in msg.split(";"):
                if (l.startswith("e")):
                    self.recv_event(parse_event(l + ";"))
                elif (l == "close;"):
                    self.recv_close()
                elif (l != ""):
                    raise(RuntimeError())
        except:
            log.err()

    def send_data(self, msg):
        l = len(msg)
        if (self.buffer[0]+l > self.limit):
            self.flush()
        self.buffer[0] += l
        self.buffer[1].append(msg)

    def flush(self):
        self.transport.write(struct.pack("!H", self.buffer[0]))
        self.transport.write("".join(self.buffer[1]))
        self.buffer[0] = 0
        self.buffer[1] = []

    def send_event(self, e):
        self.send_data("%s;" % render_event(e))

    def send_events(self, es):
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

    def connectionLost(self, reason):
        pass

    def connectionMade(self):
        pass
