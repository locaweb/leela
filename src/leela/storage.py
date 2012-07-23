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

import time
import pycassa
from contextlib import contextmanager
from leela import logger
from pycassa.pool import ConnectionPool
from pycassa.types import FloatType
from pycassa.system_manager import SystemManager

class Storage(object):

    def __init__(self, config):
        self.config = config
        self.pool   = pycassa.ConnectionPool(
            keyspace        = self.config.get("cassandra", "keyspace"),
            server_list     = self.config.get("cassandra", "server").split(),
            pool_size       = self.config.getint("cassandra", "pool_size"),
            pool_timeout    = self.config.getint("cassandra", "timeout"),
            max_retries     = self.config.getint("cassandra", "retries"),
            prefill         = self.config.get("cassandra", "prefill"),
            use_threadlocal = True
        )

    def create_schema(self):
        retries        = 3
        keyspace       = self.config.get("cassandra", "keyspace")
        manager        = self.config.get("cassandra", "system_manager")
        systemManager  = pycassa.system_manager.SystemManager(manager, timeout=self.config.getint("cassandra", "timeout"))
        schema         = systemManager.get_keyspace_column_families(self.config.get("cassandra", "keyspace"))
        colfamilies    = [("day_scf", True), ("month_cf", False), ("year_cf", False)]
        while (len(colfamilies) > 0):
            (cf, supercol) = colfamilies.pop()
            if (cf not in schema):
                create_f = funcs.retry_on_fail(systemManager.create_column_family, retries)
                create_f(keyspace,
                         cf,
                         super                    = supercol,
                         default_validation_class = FloatType(reversed=True),
                         key_cache_size           = 9000,
                         row_cache_size           = 1200)
                logger.info("creating CF %s [super=%d]" % (cf, supercol))

    def column_family(self, name):
        return(pycassa.ColumnFamily(self.pool, name))

    @contextmanager
    def day_scf(self):
        cf = self.column_family("day_scf")
        try:
            yield(cf)
        finally:
            self.pool.put(cf)
