#!/usr/bin/python
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
from leela import funcs
from leela import storage
from leela import config
from leela.lasergun.data import Lasergun
from leela import logger

@funcs.suppress_e(lambda e: type(e) == socket.error and e.errno == errno.EBADF)
def start(cfg, sock, cassandra):
    while True:
        (rlist, _, __) = select.select([sock], [], [], 1)
        if (len(rlist) == 0):
            continue
        data, peer = rlist[0].recvfrom(16*1024)
        text       = "<undefined>"
        try:
            text = bz2.decompress(data)
            logger.debug("metrics: bzip=%d, text=%d" % (len(data), len(text)))
            now    = time.time()
            lasers = Lasergun(cfg)
            lasers.store(cassandra, text, now)
            funcs.suppress(sock.sendto)(str(now), peer)
        except:
            funcs.suppress(sock.sendto)("error", peer)
            logger.exception("invalid data %s from %s" % (text, str(peer)))

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
    parser.add_argument("-b", "--background",
                        dest="daemonize",
                        action="store_true",
                        default=False,
                        help="daemonize the process")
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
    host = cfg.get("lasergun", "address")
    port = cfg.getint("lasergun", "port")
    logger.debug("binding socket [addr=%s, port=%d]" % (host, port))
    sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM, proto=socket.IPPROTO_UDP)
    sock.bind((host, port))

    funcs.drop_privileges(opts.user, opts.gid)

    logger.debug("connecting to cassandra")
    cassandra = storage.Storage(cfg)
    logger.debug("creating schema")
    cassandra.create_schema()

    logger.debug("starting server")
    start(cfg, sock, cassandra)

def main():
    opts   = cli_parser().parse_args()
    daemon = supay.Daemon("leela-lasergun")
    if (opts.debug):
        logger.set_level(logger.DEBUG)
    if (opts.action == "start"):
        if (opts.daemonize):
            daemon.start()
        main_start(opts)
    elif (opts.action == "stop"):
        daemon.stop()
    elif (opts.action == "status"):
        daemon.status()

if (__name__ == "__main__"):
    main()
