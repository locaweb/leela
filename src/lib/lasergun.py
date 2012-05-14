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

import re
import sys
import time
import syslog
import pycassa
import ConfigParser
from socket import socket
from leela import config
from datetime import datetime
from hotqueue import HotQueue
from pycassa.types import UTF8Type
from pycassa.types import FloatType
from Cheetah.Template import Template
from pycassa.pool import ConnectionPool
from pycassa.types import CompositeType
from pycassa.system_manager import SystemManager

syslog.openlog(sys.argv[0].split('/')[-1], syslog.LOG_PID, syslog.LOG_DAEMON)
cassandra = config.get('cassandra','server').split()

queue_sum = HotQueue(
    config.get('hotqueue','queue_sum'),
    host=config.get('hotqueue','host'),
    port=config.getint('hotqueue','port'),
    db=config.getint('hotqueue','db')
)

queue_wrt = HotQueue(
    config.get('hotqueue','queue_wrt'),
    host=config.get('hotqueue','host'),
    port=config.getint('hotqueue','port'),
    db=config.getint('hotqueue','db')
)

allColumnFamilyOptions = {
    'default_validation_class': FloatType(reversed=True),
    'key_cache_size': 9000,
    'row_cache_size': 1200
}

pool = pycassa.ConnectionPool(
    keyspace=config.get('cassandra','keyspace'),
    server_list=cassandra,
    pool_size=config.getint('cassandra', 'pool_size'),
    use_threadlocal=True,
    pool_timeout=-1,
    max_retries=config.getint('cassandra', 'retries'),
    max_overflow=-1,
    prefill=config.get('cassandra','prefill')
)

def prepare_cfs():
    for columnFamily in ['hour', 'day', 'month', 'year']:
        try:
            systemManager = pycassa.system_manager.SystemManager(
                config.get('cassandra','system_manager'), timeout=30
            )

            columnFamilies = systemManager.get_keyspace_column_families('leela')
            if columnFamily not in columnFamilies:
                systemManager.create_column_family(
                    'leela',
                    columnFamily,
                    **allColumnFamilyOptions
                )
                syslog.syslog('Creating Cassandra keyspace %s' % columnFamily)
        except pycassa.cassandra.ttypes.SchemaDisagreementException:
            syslog.syslog('Problem creating %s, retrying' % (columnFamily))
            time.sleep(1)
        except Exception, e:
            syslog.syslog('Exception %s on %s, retrying' % (e, columnFamily))
            time.sleep(1)

@queue_sum.worker
def summarize(data):
    line, timestamp = data

    parsed = {}
    hostname, service = line.split('||')[0].split('|')
    date = datetime.fromtimestamp(timestamp)
    hostname = hostname.replace('-','_')
    service = service.capitalize()

    def accounting(values, name):
        total = 0
        count = 0
        for key, value in values.iteritems():
            if key.startswith('%s||' % name):
                total = total + float(value)
                count = count + 1

        try:
            return total / float(count)
        except ZeroDivisionError:
            return 0

    cf = pycassa.ColumnFamily(pool, 'day')
    _ts = time.mktime(
        datetime(
            year=date.year,
            month=date.month,
            day=date.day,
            hour=date.hour
        ).timetuple()
    )

    client = pycassa.ColumnFamily(pool, 'hour')
    for _data in line.split('||')[1:]:
        total = 0
        name, _ = _data.split('|')
        dkey = "%s:%s:%s" % (hostname, service, date.strftime('%Y%m%d'))
        try:
            hkey = "%s:%s:%s" % (hostname, service, date.strftime('%Y%m%d%H'))
            total = accounting(client.get(hkey, column_count=1000), name)
            cf.insert(dkey, {
                 "%s||%s" % (name, _ts): total
            })
        except pycassa.cassandra.ttypes.NotFoundException:
            queue_sum.put(data)
            syslog.syslog('Data not found on %s, data requeued' % (dkey))
            time.sleep(0.7)
        except Exception, e:
            queue_sum.put(data)
            syslog.syslog('Exception %s working on %s, data requeued' % (e, dkey))
            time.sleep(0.7)
        syslog.syslog("%s -> %s - %s||%s - %s" % (
            hostname, service, name, date.strftime('%Y%m%d'), total)
        )

    pool.put(client)
    del client
    pool.put(cf)
    del cf

def write_to_carbon(data):
    if config.has_section('graphite-carbon'):
        sock = socket()
        sock.connect((config.get('graphite-carbon', 'server'), int(config.get('graphite-carbon', 'port'))))
        sock.send("%s\n" % data)
        sock.close()

@queue_wrt.worker
def parse_and_save_datagram(data):
    line, timestamp = data

    parsed = {}
    hostname, service = line.split('||')[0].split('|')
    date = datetime.fromtimestamp(timestamp)
    hostname = hostname.replace('-','_')
    service = service.capitalize()

    cf = pycassa.ColumnFamily(pool, 'hour')
    for _data in line.split('||')[1:]:
        name, value = _data.split('|')
        hkey = "%s:%s:%s" % (hostname, service, date.strftime('%Y%m%d%H'))
        try:
            cf.insert(hkey, {
                "%s||%s" % (name, timestamp): float(value)
            })
            write_to_carbon("%s.%s.%s %s %s" % (hostname, service.lower(), name, value, int(timestamp)))
        except Exception, e:
            queue_wrt.put(data)
            syslog.syslog('Exception %s on %s - %f, data requeued' % (e, hkey, float(value)))
            time.sleep(0.7)
        syslog.syslog("%s -> %s - %s||%s - %s" % (hostname, service, name, timestamp, float(value)))

    queue_sum.put(data)
    pool.put(cf)
    del cf

def render_template(template, service, hostname, series):
    render = Template(file = template,
        searchList = [{'series': series, 'hostname': hostname, 'service': service}]
    )
    return str(render)
