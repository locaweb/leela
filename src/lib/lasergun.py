# Copyright 2012 Juliano Martinez
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

from gevent import monkey
monkey.patch_all()

import re
import sys
import time
import syslog
import pycassa
import ConfigParser
from leela import config
from datetime import datetime
from hotqueue import HotQueue
from pycassa.types import LongType
from pycassa.types import UTF8Type
from pycassa.types import DoubleType
from Cheetah.Template import Template
from pycassa.pool import ConnectionPool
from pycassa.types import CompositeType
from pycassa.system_manager import SystemManager

syslog.openlog(sys.argv[0].split('/')[-1], syslog.LOG_PID, syslog.LOG_DAEMON)
cassandra = config.get('cassandra','server').split()

queue = HotQueue(
    config.get('hotqueue','queue'),
    host=config.get('hotqueue','host'),
    port=config.getint('hotqueue','port'),
    db=config.getint('hotqueue','db')
)

allColumnFamilyOptions = {
    'default_validation_class': DoubleType(),
    'key_cache_size': 9000,
    'row_cache_size': 1200
}

pool = pycassa.ConnectionPool(
    keyspace=config.get('cassandra','keyspace'),
    server_list=cassandra,
    pool_size=config.getint('cassandra', 'pool_size'),
    prefill=False
)

def prepare_cf(srtftime, hostname, service, timestamp):
    systemManager = pycassa.system_manager.SystemManager(
        config.get('cassandra','system_manager'), timeout=10
    )
    columnFamily = "%s_%s_%s" % (
            hostname,
            service,
            datetime.fromtimestamp(timestamp).strftime(srtftime)
    )

    for count in range(config.getint('cassandra','retries')):
        columnFamilies = systemManager.get_keyspace_column_families('leela')
        if columnFamily not in columnFamilies:
            try:
                systemManager.create_column_family(
                    'leela',
                    columnFamily,
                    **allColumnFamilyOptions
                )
                syslog.syslog('Creating Cassandra keyspace %s' % columnFamily)
                break
            except pycassa.cassandra.ttypes.SchemaDisagreementException:
                syslog.syslog('Problem creating %s, retrying' % (columnFamily))
                time.sleep(0.5)
            except pycassa.cassandra.ttypes.InvalidRequestException:
                syslog.syslog('Column Family %s, already existis' % (columnFamily))
                break
            except Exception, e:
                syslog.syslog('Exception %s on %s' % (e, columnFamily))
    return pycassa.ColumnFamily(pool, columnFamily)

def summarize(data):
    line, timestamp = data

    parsed = {}
    hostname, service = line.split('||')[0].split('|')
    service = service.capitalize()

    def accounting(values, name):
        total = 0
        count = 0
        for _, data in values:
            for key, value in data.iteritems():
                if key.startswith('%s||' % name):
                    total = total + float(value)
                    count = count + 1

        try:
            return total / float(count)
        except ZeroDivisionError:
            return 0

    for srtftime in ["%Y%m%d%H", "%Y%m%d"]:
        cf = prepare_cf(srtftime[:-2], hostname, service, timestamp)
        date = datetime.fromtimestamp(timestamp)
        _ts = time.mktime(
            datetime(
                year=date.year,
                month=date.month,
                day=date.day,
                hour=date.hour
                    if srtftime == "%Y%m%d%H" else 0
            ).timetuple()
        )

        for data in line.split('||')[1:]:
            name, _ = data.split('|')
            total = 0
            for count in range(config.getint('cassandra','retries')):
                try:
                    client = pycassa.ColumnFamily(
                        pool, '%s_%s_%s' % (
                            hostname,
                            service,
                            date.strftime(srtftime)
                        )
                    )
                    values = client.get_range(column_count=1000)
                    total = accounting(values, name)
                    cf.insert(service, {"%s||%s" % (name, _ts): total},
                        max_retries=config.getint('cassandra','retries'),
                        pool_timeout=60)
                    break
                except Exception, e:
                    syslog.syslog('Exception %s' % (e))
                    time.sleep(0.2)
            syslog.syslog("%s -> %s - %s||%s - %s" % (
                hostname, service, name, date.strftime(srtftime), total)
            )

def parse_and_save_datagram(line):
    parsed = {}
    hostname, service = line.split('||')[0].split('|')
    service = service.capitalize()

    now = datetime.now()
    timestamp = time.time()
    queue.put((line, timestamp))
    cf = prepare_cf("%Y%m%d%H", hostname, service, timestamp)
    for data in line.split('||')[1:]:
        name, value = data.split('|')
        for count in range(config.getint('cassandra','retries')):
            try:
                cf.insert(service, {"%s||%s" % (name, timestamp): float(value)},
                    max_retries=config.getint('cassandra','retries'),
                    pool_timeout=60)
                break
            except Exception, e:
                syslog.syslog('Exception %s' % (e))
                time.sleep(0.2)
        syslog.syslog("%s -> %s - %s||%s - %s" % (hostname, service, name, timestamp, float(value)))

def render_template(template, service, hostname, series):
    render = Template(file = template,
        searchList = [{'series': series, 'hostname': hostname, 'service': service}]
    )
    return str(render)
