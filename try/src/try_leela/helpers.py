# -*- coding: utf-8 -*-

import sys
import time

def make(session):
    session.execute("make (%(rnd_name.0)s)")
    a_guid = session.message()[1][-1]
    assert(session.message() is None)
    return(a_guid)

def link(session, a, l, b):
    session.execute("make %s -[%s]> %s" % (a, l, b))
    assert(session.message() is None)

def kill(session, a, l, b=None):
    if (b is None):
        session.execute("kill %s -[%s]> ()" % (a, l))
    else:
        session.execute("kill %s -[%s]> %s" % (a, l, b))
    assert(session.message() is None)

def kattr_put(session, a, name, value, ttl=None):
    if (ttl is None):
        ttl = ""
    else:
        ttl = " with ttl:%d" % (ttl,)
    session.execute("attr put %s \"%s\" %s%s" % (a, name, value, ttl))
    assert(session.message() is None)

def tattr_put(session, a, name, time, value, ttl=None):
    if (ttl is None):
        ttl = ""
    else:
        ttl = " with ttl:%d" % (ttl,)
    session.execute("attr put %s \"%s\" [%s] %s%s" % (a, name, time, value, ttl))
    assert(session.message() is None)

def kattr_del(session, a, name):
    session.execute("attr del %s \"%s\"" % (a, name))
    assert(session.message() is None)

def string_value(a):
    return("\"%s\"" % a)

def int32_value(a):
    return("(int32 %d)" % a)

def uint32_value(a):
    return("(uint32 %d)" % a)

def int64_value(a):
    return("(int64 %d)" % a)

def uint64_value(a):
    return("(uint64 %d)" % a)

def double_value(a):
    return("(double %s)" % repr(a))

def sleep(t):
    sys.stdout.write("(time.sleep %d)" % t)
    sys.stdout.flush()
    time.sleep(t * 2)

