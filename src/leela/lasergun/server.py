#!/usr/bin/python
# -*- coding: utf-8; -*-
#
# Copyright 2012 Juliano Martinez
# Copyright 2012 Diego Souza
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
# @author: Juliano Martinez
# @author: Diego Souza

import os
import bz2
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
from multiprocessing import Process
from multiprocessing import Pipe
from leela import funcs
from leela import config
from leela import storage
from leela.lasergun.data import Lasergun
from leela import logger

def cassandra_consumer(cont, cfg, opts, pipe):
    funcs.drop_privileges(opts.user, opts.gid)

    logger.debug("connecting to cassandra and [possibly] creating schema")
    cassandra = storage.Storage(cfg)
    cassandra.create_schema()
    lasergun = Lasergun(cfg, cassandra=cassandra, carbon=None)
    while (cont()):
        try:
            if (not pipe.poll(1)):
                continue
            (data, now) = pipe.recv()
            text = bz2.decompress(data)
            logger.debug("metrics: bzip=%d, text=%d" % (len(data), len(text)))
            lasergun.store(text, now)
        except:
            logger.exception("cassandra_consumer: error writing data, keeping calm and carrying on")

def carbon_consumer(cont, cfg, opts, pipe):
    funcs.drop_privileges(opts.user, opts.gid)

    carbon   = storage.Carbon(cfg)
    lasergun = Lasergun(cfg, cassandra=None, carbon=carbon)
    while (cont()):
        try:
            (data, now) = pipe.recv()
            text = bz2.decompress(data)
            lasergun.send2carbon(text, now)
        except:
            logger.exception("carbon_consumer: error writing data, keeping calm and carrying on")

def server_consumer(cont, cfg, opts, pipes):
    host = cfg.get("lasergun", "address")
    port = cfg.getint("lasergun", "port")
    logger.debug("binding socket [addr=%s, port=%d]" % (host, port))
    sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM, proto=socket.IPPROTO_UDP)
    sock.bind((host, port))

    funcs.drop_privileges(opts.user, opts.gid)
    while (cont()):
        (rlist, _, __) = select.select([sock], [], [], 1)
        if (len(rlist) == 0):
            continue
        data, peer = rlist[0].recvfrom(16*1024)
        text       = "<undefined>"
        now        = time.time()
        map(lambda p: p.send((data, now)), pipes)

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
    return(parser)

def main_start(opts):
    cfg  = config.read_config(opts.config)
    p0in, p0out = Pipe()
    p1in, p1out = Pipe()

    logger.debug("starting server...")
    p0 = funcs.start_process(server_consumer, cfg, opts, [p0out, p1out])

    logger.debug("starting cassandra consumer...")
    p1 = funcs.start_process(cassandra_consumer, cfg, opts, p0in)

    logger.debug("starting carbon consumer...")
    p2 = funcs.start_process(carbon_consumer, cfg, opts, p1in)

    p0.join()
    p1.terminate()
    p2.terminate()
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
