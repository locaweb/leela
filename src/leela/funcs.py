#!/usr/bin/python
# -*- coding: utf-8; -*-

import os
import time
import pwd
from datetime import datetime
from leela import logger

def suppress(f):
    def g(*args, **kwargs):
        try:
            f(*args, **kwargs)
        except:
            pass
    g.__name__ = f.__name__
    return(g)

def suppress_e(p):
    def suppress_f(f):
        def g(*args, **kwargs):
            try:
                f(*args, **kwargs)
            except Exception as e:
                if (not p(e)):
                    raise
        g.__name__ = f.__name__
        return(g)
    return(suppress_f)


def retry_on_fail(f, retries=3, wait=0.3):
    def g(*args, **kwargs):
        attempt = 0
        while (True):
            try:
                return(f(*args, **kwargs))
            except:
                if (attempt > retries):
                    raise
                attempt += 1
                time.sleep(wait)
    g.__name__ = f.__name__
    return(g)

def dict_set(d, k, v):
    d[k] = v
    return(d)

def dict_update(f, d, k, v):
    if (k in d):
        d[k] = f(d[k], v)
    else:
        d[k] = v
    return(d)

def dict_merge(o, n):
    tmp = {}
    tmp.update(o)
    tmp.update(n)
    return(tmp)

def datetime_date(date):
    y = str(date.year)
    m = str(date.month).zfill(2)
    d = str(date.day).zfill(2)
    return(y + m + d)

def datetime_time(date):
    h = str(date.hour).zfill(2)
    m = str(date.minute).zfill(2)
    return(h + m)

def datetime_timestamp(date):
    zero = datetime.fromtimestamp(0)
    diff = (date - zero).total_seconds()
    return(int(diff))

def time_to_slot(hour, minute):
    if (hour > 23 or hour < 0):
        raise(RuntimeError("invalid range (0 ≤ hour < 24)"))
    if (minute > 59 or minute < 0):
        raise(RuntimeError("invalid range (0 ≤ minute < 60)"))
    return(hour * 60 + minute)

def slot_to_time(slot):
    if (slot < 0 or slot > 1439):
        raise(RuntimeError("invalid range (0 ≤ slot < 1439)"))
    return(divmod(slot, 60))

def slot_to_timestamp(year, month, day, slot):
    (hour, minute) = slot_to_time(slot)
    return(datetime_timestamp(datetime(year, month, day, hour, minute)))

def service_reduce(f, g, data, acc):
    for (k, v) in data.iteritems():
        acc = f(k, g(v), acc)
    return(acc)

def service_map_slot(h, data):
    f = lambda k, v, acc: dict_set(acc, k, v)
    g = lambda kv: dict([(h(k), v) for (k, v) in kv.iteritems()])
    return(service_reduce(f, g, data, {}))

def timer_start():
    return(time.time())

def timer_stop(t):
    return(time.time() - t)

def drop_privileges(uid, gid):
    if (os.getuid() == 0):
        logger.debug("dropping privileges [user=%s, group=%s]" % (uid, gid))
        user = pwd.getpwnam(uid)
        os.setgid(user.pw_gid)
        os.setuid(user.pw_uid)
    else:
        logger.debug("ignoring user/group altogether")
