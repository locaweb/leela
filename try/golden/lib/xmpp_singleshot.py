import sys; _stdout = sys.stdout; _stdin = sys.stdin
import json
import os
import time
from twisted.python import usage
from twisted.internet import reactor
from twisted.application import service
from twisted.words.protocols.jabber.jid import JID
from wokkel import client
from wokkel import xmppim

options = json.loads(_stdin.readline())

def read_n_exit(message):
    global options
    if (message.body is None):
        return
    jid = xmppim.JID(message.getAttribute("from"))
    bdy = unicode(message.body)
    _stdout.write("%s: %s\n" % (jid.user, bdy))

    wait = options["read"] - 1
    options["read"] = wait
    if (wait == 0):
        reactor.callLater(1, reactor.stop)

def send_messages(message, super_f):
    global options
    def f(*args, **kwargs):
        super_f(*args, **kwargs)
        for msg in options["write"]:
            xml = xmppim.Message(recipient = JID(msg["to"])).toElement()
            xml.addElement("body", content=msg["msg"])
            message.send(xml)
    return(f)

application = service.Application("xmpp_singleshot")
xmppClient  = client.XMPPClient(JID(options.get("jid", "foobaz@localhost")),
                                options.get("passwd", "foobaz"),
                                options.get("host", "localhost"),
                                options.get("port", 5222))
xmppClient.setServiceParent(application)
presence = xmppim.PresenceProtocol()
presence.setHandlerParent(xmppClient)
presence.available()
message = xmppim.MessageProtocol()
message.onMessage = read_n_exit
message.connectionMade = send_messages(message, message.connectionMade)
message.setHandlerParent(xmppClient)

