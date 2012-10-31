#!/usr/bin/python
# -*- coding: utf-8; -*-

import sys; _stdout = sys.stdout; _stdin = sys.stdin; _stderr = sys.stderr;
import json
import os
import time
import argparse
import logging
from twisted.python import usage
from twisted.python import log
from twisted.internet import reactor
from twisted.application import service
from twisted.words.protocols.jabber.jid import JID
from wokkel import client
from wokkel import xmppim

counter = 0
def recv_message(times):
    def f(message):
        global counter
        if (message.body is None):
            return
        jid = xmppim.JID(message.getAttribute("from"))
        bdy = unicode(message.body)
        _stdout.write("%s: %s\n" % (jid.user, bdy))

        counter += 1
        if (counter >= times):
            reactor.callLater(0, reactor.stop)
    return(f)

def send_messages(xmpp, messages):
    def f(*args, **kwargs):
        for msg in messages:
            (to, bdy) = msg.split(": ", 2)
            xml = xmppim.Message(recipient = JID(to)).toElement()
            xml.addElement("body", content=bdy)
            xmpp.send(xml)
    return(f)

if __name__ == "__main__":
    log.startLogging(_stderr)
    parser = argparse.ArgumentParser("xmpp_interact")
    parser.add_argument("-n", "--recv-num",
                        dest    = "recv_num",
                        type    = int,
                        default = "0"
                       )
    parser.add_argument("-j", "--jid",
                        dest    = "jid",
                        type    = str,
                        default = "foobaz@localhost"
                       )
    parser.add_argument("-p", "--passwd",
                        dest    = "passwd",
                        type    = str,
                        default = "foobaz"
                       )
    parser.add_argument("-H", "--host",
                        dest    = "host",
                        type    = str,
                        default = "localhost"
                       )
    parser.add_argument("-P", "--port",
                        dest    = "port",
                        type    = int,
                        default = "5222"
                       )
    opts = parser.parse_args()
    application = service.Application("xmpp_singleshot")
    xmppClient  = client.XMPPClient(JID(opts.jid),
                                    opts.passwd,
                                    opts.host,
                                    opts.port
                                   )
    xmppClient.setServiceParent(application)
    presence = xmppim.PresenceProtocol()
    presence.setHandlerParent(xmppClient)
    presence.available()
    xmpp                = xmppim.MessageProtocol()
    xmpp.onMessage      = recv_message(opts.recv_num)
    sequence            = lambda xs: map(lambda f: f(), xs)
    functions           = (xmpp.connectionMade, send_messages(xmpp, _stdin.readlines()))
    xmpp.connectionMade = lambda: sequence(functions)
    xmpp.setHandlerParent(xmppClient)
    service.IService(application).startService()
    reactor.run()
