# -*- coding: utf-8 -*-

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
