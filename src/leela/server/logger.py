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

import os.path
import sys
import logging
from logging import DEBUG
from logging import INFO
from logging import WARNING
from logging import ERROR
from logging.handlers import SysLogHandler

def logger():
    if (logging.getLevelName(DEBUG) != "debug"):
        logging.addLevelName(DEBUG, "debug")
        logging.addLevelName(INFO, "info")
        logging.addLevelName(WARNING, "warning")
        logging.addLevelName(ERROR, "error")
    return(logging.getLogger("futurama(leela)"))

def set_level(level):
    logger().setLevel(level)

def use_syslog(address="/dev/log"):
    handler = SysLogHandler(address=address, facility="daemon")
    handler.setFormatter(logging.Formatter("%(asctime)s %(name)s %(filename)s:%(lineno)d: %(message)s", "%b %d %H:%M:%S"))
    logger().addHandler(handler)
    for k in ["sleekxmpp.xmlstream.cert", "sleekxmpp.xmlstream.xmlstream"]:
        logging.getLogger(k).addHandler(handler)

def use_console(device=sys.stderr):
    handler = logging.StreamHandler(device)
    handler.setFormatter(logging.Formatter("%(asctime)s %(name)s: %(message)s", "%b %d %H:%M:%S"))
    logger().addHandler(handler)
    for k in ["sleekxmpp.xmlstream.cert", "sleekxmpp.xmlstream.xmlstream"]:
        logging.getLogger(k).addHandler(handler)

def debug(*args, **kwargs):
    logger().debug(*args, **kwargs)

def info(*args, **kwargs):
    logger().info(*args, **kwargs)

def warn(*args, **kwargs):
    logger().warn(*args, **kwargs)

def error(*args, **kwargs):
    logger().error(*args, **kwargs)

def exception(*args, **kwargs):
    logger().exception(*args, **kwargs)
