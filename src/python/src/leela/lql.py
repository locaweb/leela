# -*- coding: utf-8 -*-

import sys
import zmq
import time
import contextlib
try:
    import simplejson as json
except ImportError:
    import json

DEBUG = False

def debug(prefix, ss):
    if (DEBUG):
        for s in ss.splitlines():
            sys.stderr.write("[DEBUG] %s %s\n" % (prefix, s))

def escape(s):
    return(s.replace("\\", "\\\\").replace(")", "\\)").replace("]", "\\]"))

def context():
    ctx = zmq.Context()
    return(ctx)

@contextlib.contextmanager
def connection(ctx, endpoint):
    conn = Connection(ctx, endpoint)
    try:
        yield(conn)
    finally:
        conn.close()

def pp(data):
    if ("done" in data):
        return(".done")
    elif ("fail" in data):
        return("fail: %d" % data["fail"]["code"])
    else:
        return("chunk: ...")

class Cursor(object):

    def __init__(self, conn, chan, when):
        self.conn   = conn
        self.chan   = chan
        self.when0  = when
        self.read   = 0
        self.when   = when
        self.closed = False

    def close(self):
        self.closed = True
        self.conn.send("close %d;" % self.chan)
        t = time.time() - self.when0
        debug("timing", "total: %.4f / %d / %d/s" % (t, self.read, self.read/t))
        return(self.conn.recv())

    def _eof(self, data):
        return("done" in data or "fail" in data)

    def fetchall(self):
        while (True):
            for row in self.fetch():
                yield(row)
            if (self.closed):
                break

    def fetch(self, limit=64):
        if (self.closed):
            raise(RuntimeError("closed"))
        self.when = time.time()
        self.conn.send("fetch %d %d;" % (self.chan, limit))
        msg = self.conn.recv()
        self.read += len(msg)
        debug("timing", "fetch: %.4f / items: %d" % (time.time() - self.when, len(msg)))
        if (self._eof(msg[-1])):
            self.close()
        return(msg)

    def fetchone(self):
        return(self.fetch(1))

class Stmt(object):

    def __init__(self, conn):
        self.conn  = conn

    def cursor(self):
        when = time.time()
        self.conn.send(";")
        data = self.conn.recv()[0]
        if ("done" not in data):
            raise(RuntimeError(pp(data)))
        return(Cursor(self.conn, data["done"]["channel"], when))

    def done(self):
        for _ in self.cursor().fetchall():
            pass

    def send(self, line, nl=True):
        if (nl):
            nl = "\n"
        else:
            nl = ""
        self.conn.send("%s%s" % (nl, line), zmq.SNDMORE)

    def create(self):
        return(CreateStmt(self))

    def match(self):
        return(MatchStmt(self))

    def deref(self, key=None):
        stmt = DerefStmt(self)
        if (key is not None):
            stmt.deref(key)
        return(stmt)

class DerefStmt(object):

    def __init__(self, stmt, *args, **kwargs):
        self.stmt = stmt

    def deref(self, k):
        self.stmt.send("deref %s" % (k,))
        return(self)

    def cursor(self):
        return(self.stmt.cursor())

    def done(self):
        return(self.stmt.done())

class CreateStmt(object):

    def __init__(self, stmt, *args, **kwargs):
        self.stmt = stmt

    def node(self, a):
        self.stmt.send("create (%s)" % escape(a))
        return(self)

    def link(self, a, l, b):
        self.stmt.send("create (%s) -[%s]> (%s)" % (escape(a), escape(l), escape(b)))
        return(self)

    def cursor(self):
        return(self.stmt.cursor())

    def done(self):
        return(self.stmt.done())

class MatchStmt(object):

    def __init__(self, stmt):
        self.stmt = stmt
        self.stmt.send("match ")

    def select(self, node):
        self.stmt.send("(%s)" % escape(node), nl=False)
        return(self)

    def out(self, label, node=""):
        self.stmt.send(" -[%s]> (%s)" % (escape(label), escape(node)), nl=False)
        return(self)

    def or_(self):
        self.stmt.send("match ")
        return(self)

    def cursor(self):
        return(self.stmt.cursor())

    def done(self):
        return(self.stmt.done())

class Connection(object):

    def __init__(self, context, endpoint):
        self.socket = context.socket(zmq.REQ)
        self.socket.connect(endpoint)
        self.debug = []

    def send(self, s, flags=0):
        self.debug.append(s)
        if (flags == 0):
           debug(">", "".join(self.debug))
           self.debug = []
        self.socket.send_string(s, flags)

    def _recv(self):
        msg = self.socket.recv()
        debug("<", msg)
        return(json.loads(msg))

    def recv(self):
        tmp = [self._recv()]
        while (self.socket.getsockopt(zmq.RCVMORE)):
            tmp.append(self._recv())
        return(tmp)

    def using(self, username, namespace):
        self.send("begin using (%s) (%s)" % (escape(username), namespace), zmq.SNDMORE)
        return(Stmt(self))

    def close(self):
        self.socket.close()

