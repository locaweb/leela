#!/usr/bin/python
# -*- coding: utf-8; -*-

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
