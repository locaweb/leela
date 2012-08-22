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

import os
import sys
import time
import getopt
import datetime
import socket
import select
import errno
import argparse
import supay
import threading
import signal
import json
from Queue import Empty
from Queue import Full
from multiprocessing import Process
from multiprocessing import Queue
from leela.server import funcs
from leela.server import config
from leela.server.data import event
from leela.server.data import netprotocol
from leela.server.storage import cassandra
from leela.server.storage import xmpp
from leela.server import logger
from leela.server.data import mavg
from leela.server.data import ratelimit

def scale(e):
    e.set_time((e.year(), e.month(), e.day(), e.hour(), e.minute(), 0))

@funcs.logerrors(logger)
def monit_consumer(cont, cfg, opts, pipe):
    funcs.drop_privileges(opts.user, opts.gid)

    logger.debug("starting monitoring cpu.idle")
    avg = mavg.MAvg(samples=30)
    rl  = ratelimit.RateLimit(300)
    s   = xmpp.XmppStorage(cfg)
    s.connect()

    while (cont()):
        try:
            text = pipe.get(timeout=1)
            for e in netprotocol.parse(text):
                n = e.name()
                if (n.endswith(".cpu.cpu.idle")):
                    k = n[:-13]
                    scale(e)
                    val = avg.compute(k, e)
                    if (rl.should_emit(k)):
                        logger.debug("monito_consumer: sending message to xmpp peer")
                        s.store(event.Event(e.name(), val, long(time.time())))
        except Empty:
            pass
        except:
            if (opts.debug):
                logger.exception("monit_consumer: exceptiong caught")
    s.disconnect()
    logger.debug("monit_consumer: /bye")

@funcs.logerrors(logger)
def cassandra_consumer(cont, cfg, opts, pipe):
    funcs.drop_privileges(opts.user, opts.gid)

    logger.debug("connecting to cassandra...")
    storage  = cassandra.EventsStorage(cassandra.connect(cfg))
    text     = "undefined"
    while (cont()):
        try:
            text = pipe.get(timeout=1)
            t = funcs.timer_start()
            for e in netprotocol.parse(text):
                t1 = funcs.timer_start()
                scale(e)
                e.store(storage)
                tmp = funcs.timer_stop(t1)
                logger.debug("store event: %s [walltime: %f]" % (str(e), tmp))
            tmp = funcs.timer_stop(t)
            logger.debug("store done [walltime: %f]" % (tmp,))
        except Empty:
            pass
        except:
            if (opts.debug):
                logger.exception("cassandra_consumer: error writing data [text: %s]" % str(text))
    logger.debug("cassandra_consumer: /bye")

@funcs.logerrors(logger)
def server_consumer(cont, cfg, opts, pipes):
    host = cfg.get("lasergun", "address")
    port = cfg.getint("lasergun", "port")
    logger.debug("binding socket [addr=%s, port=%d]" % (host, port))
    sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM, proto=socket.IPPROTO_UDP)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind((host, port))

    funcs.drop_privileges(opts.user, opts.gid)
    while (cont()):
        try:
            (rlist, _, __) = select.select([sock], [], [], 1)
            if (len(rlist) == 0):
                continue
            data, peer = rlist[0].recvfrom(16*1024)
            map(lambda p: p.put_nowait(data), pipes)
        except Full:
            logger.warn("queue is full, dropping packages")
        except:
            if (opts.debug):
                logger.exception("server_consumer: error reading data")
    logger.debug("server_consumer: /bye")

def sighandler(*procs):
    def f(signum, frame):
        logger.debug("signum: %d. terminating all daemons" % signum)
        map(lambda p: p.terminate(), procs)
        return(0)
    return(f)

def cli_parser():
    parser = argparse.ArgumentParser("lasergun: receive and save metrics")
    parser.add_argument("-u", "--user",
                        dest="user",
                        type=str,
                        default="nobody",
                        help="the owner of this process [%(default)s]")
    parser.add_argument("-g", "--gid",
                        dest="gid",
                        type=str,
                        default="nogroup",
                        help="the group owner of this process [%(default)s]")
    parser.add_argument("-a", "--action",
                        dest="action",
                        type=str,
                        default="start",
                        choices=["start", "stop", "status"])
    parser.add_argument("-f", "--foregound",
                        dest="daemonize",
                        action="store_false",
                        default=True,
                        help="do not daemonize the process")
    parser.add_argument("-d", "--debug",
                        dest="debug",
                        action="store_true",
                        default=False,
                        help="turn debug messages on")
    parser.add_argument("-c", "--config",
                        dest="config",
                        type=str,
                        default=config.default_config_file(),
                        help="the config file to use [%(default)s]")
    parser.add_argument("--create-schema",
                        dest="create_schema",
                        action="store_false",
                        default=True,
                        help="Create the schema (column families) on cassandra")
    return(parser)

def main_start(opts):
    cfg = config.read_config(opts.config)
    q0  = Queue(10000)
    q1  = Queue(10000)

    if (opts.create_schema):
        cassandra.create_schema(cfg)

    logger.debug("starting server...")
    p0 = funcs.start_process(server_consumer, cfg, opts, [q0, q1])

    logger.debug("starting monitoring consumer...")
    funcs.start_process(monit_consumer, cfg, opts, q1)

    logger.debug("starting cassandra consumer...")
    for _ in range(cfg.getint("lasergun", "consumers")):
        funcs.start_process(cassandra_consumer, cfg, opts, q0)

    p0.join()
    q0.close()
    q0.cancel_join_thread()
    logger.debug("bye!")

def main():
    opts   = cli_parser().parse_args()
    daemon = supay.Daemon("leela-lasergun")
    if (opts.debug):
        logger.set_level(logger.DEBUG)
    if (opts.action == "start"):
        if (opts.daemonize):
            daemon.start()
            logger.use_syslog()
        else:
            logger.use_console()
        main_start(opts)
    elif (opts.action == "stop"):
        daemon.stop()
    elif (opts.action == "status"):
        daemon.status()

if (__name__ == "__main__"):
    main()
