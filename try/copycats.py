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

from contextlib import contextmanager

class proxy_col(object):

    def __init__(self, mem):
        self.mem = mem

    def insert(self, k, v, **kwargs):
        if (k not in self.mem):
            self.mem[k] = {}
        self.mem[k].update(v)

    def get(self, k, **kwargs):
        return(self.mem[k])

class Storage(object):

    def __init__(self, storage={}):
        self.storage = {"day_scf": {}}

    @contextmanager
    def day_scf(self):
        mem = self.storage["day_scf"]
        yield(proxy_col(mem))
