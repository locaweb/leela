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

from leela.server import funcs
from leela.server.data.event import Event
import time

def parse1(string):
    (name, value) = string.split(": ", 2)
    if (" " in value):
        (lval, tval) = value.split(" ", 2)
    else:
        lval = value
        tval = time.time()
    return(Event(name[:255], float(lval), long(tval)))

def parse(string):
    return(map(parse1, string.splitlines()))
