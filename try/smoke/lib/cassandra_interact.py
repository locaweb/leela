# -*- coding: utf-8; -*-

import sys; _stdout = sys.stdout; _stdin = sys.stdin; _stderr = sys.stderr;
import argparse
import threading
from twisted.python import log
from twisted.internet import reactor
from twisted.internet import defer
from leela.server import config
from leela.server.data import parser
from leela.server.data import pp
from leela.server.data import event
from leela.server.data import data
from leela.server.network import cassandra_proto

def error_cc(e):
    raise("foobar")
    _stderr.write(str(e))
    _stderr.flush()
    # reactor.stop()

def print_obj(xs):
    for x in xs:
        if (isinstance(x, event.Event)):
            _stdout.write(pp.render_event(x))
        else:
            _stdout.write(pp.render_data(x))
        _stdout.write("\n")

@defer.inlineCallbacks
def write(storage):
    try:
        for l in _stdin.readlines():
            if (l[0] == 'e'):
                yield storage.store(parser.parse_event(l.strip())[0])
            elif (l[0] == 'd'):
                yield storage.store(parser.parse_data(l.strip())[0])
        storage.stopService()
        reactor.callLater(1, reactor.stop)
    except:
        reactor.stop()

@defer.inlineCallbacks
def enum(storage):
    try:
        v = yield event.Event.enum(storage)
        print_obj(v)
        v = yield data.Data.enum(storage)
        print_obj(v)
        storage.stopService()
        reactor.callLater(1, reactor.stop)
    except:
        reactor.stop()

@defer.inlineCallbacks
def truncate(storage):
    try:
        yield storage.truncate("data")
        yield storage.truncate("events")
        storage.stopService()
        reactor.callLater(1, reactor.stop)
    except:
        reactor.stop()

if __name__ == "__main__":
    log.startLogging(_stderr)
    aparser = argparse.ArgumentParser("dmproc_interact")
    aparser.add_argument("-m", "--mode",
                         dest    = "mode",
                         type    = str,
                         choices = ("enum", "write", "truncate")
                        )
    aparser.add_argument("-c", "--config",
                         dest    = "config",
                         type    = str,
                         default = "etc/leela/leela.conf"
                        )
    opts    = aparser.parse_args()
    cfg     = config.read_config(opts.config)
    storage = cassandra_proto.CassandraProto(cfg)
    storage.startService()
    if (opts.mode == "enum"):
        enum(storage)
    elif (opts.mode == "write"):
        write(storage)
    else:
        truncate(storage)
    reactor.run()
