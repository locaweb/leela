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

import sys
import logging
from twisted.python import log
from logging import DEBUG
from logging import INFO
from logging import WARNING
from logging import ERROR

this = sys.modules[__name__]

def when(p, f):
    def g(*args, **kwargs):
        if (p):
            f(*args, **kwargs)
    g.__name__ = f.__name__
    return(g)

def set_level(level, backend="twistex"):
    # yeah, pretty nasty hack ... only four lines though :-)
    if (backend == "twisted"):
        this.debug     = when(level<=DEBUG, log.msg)
        this.info      = when(level<=INFO, log.msg)
        this.warn      = when(level<=WARNING, log.msg)
        this.error     = when(level<=ERROR, log.msg)
        this.exception = logging.err
    else:
        logging.getLogger().setLevel(level)
        this.debug     = logging.debug
        this.info      = logging.info
        this.warn      = logging.warn
        this.error     = logging.error
        this.exception = logging.exception
