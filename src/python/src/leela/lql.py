# -*- coding: utf-8 -*-

# TODO: 
#   - Return a list of lists to the user. Why?
#   - Write Functional Tests
#       - Populate the test database
#       - Authentication tests
#       - Thread tests
#   - Handle the Authentication process. How?
#   - Handle the Zookeeper nodes as a Cursor
#   - Write the Man documentation
#   - Write the Pretty Printer (WarpDrive Protocol)
#   - Provide Threads
#   - Use Timeout
#   - Handle Deref

import sys
import zmq
import time
import contextlib
import unittest

DEBUG = True
#DEBUG = False

def debug(prefix, ss):
    if (DEBUG):
        for s in ss.splitlines():
            sys.stderr.write("[DEBUG] %s %s\n" % (prefix, s))

def escape(s):
    return(s.replace("\\", "\\\\").replace(")", "\\)").replace("]", "\\]"))

def context():
    ctx = zmq.Context()
    return(ctx)

def pp(data):
    if ("fail" in data):
        return("fail: %s" % data[2])

class Connection(object):

    def __init__(self, context, endpoint):
        self.context = context
        self.endpoint = endpoint
        self.debug = []

    def connect(self):
        self.socket = self.context.socket(zmq.REQ)
        self.socket.connect(self.endpoint[0])

    def channel(self):
        when = time.time()
        self.send("")
        data = self.recv()
        if ("done" not in data):
            raise(RuntimeError(pp(data)))
        return(Channel(self, int(data[1]), when, self.auth()))

    def execute(self, query):
        self.send(self.auth(), zmq.SNDMORE)
        self.send("begin", zmq.SNDMORE)
        self.send("%s" % (query), zmq.SNDMORE)
        return (self.channel())

    def send(self, s, flags=0):
        self.debug.append(s)
        if (flags == 0):
           debug(">", "".join(self.debug))
           self.debug = []
        self.socket.send_string(s, flags)

    def _recv(self):
        msg = self.socket.recv()
        debug("<", msg)
        return(msg)

    def recv(self):
        tmp = [self._recv()]
        while (self.socket.getsockopt(zmq.RCVMORE)):
            tmp.append(self._recv())
        return(tmp)

    def close(self):
        self.socket.close()

    def auth(self):
        return("usertest:0:0 0")

    @contextlib.contextmanager
    def cursor(self, query, timeout=-1, nowait=False):
        self.connect()
        try:
            channel = self.execute(query)
            yield(channel.fetchall())
            #channel.close(nowait)
        finally:
            channel.close(nowait)
            self.close()

class Channel(object):

    def __init__(self, conn, chan, when, auth):
        self.conn   = conn
        self.chan   = chan
        self.when0  = when
        self.read   = 0
        self.when   = when
        self.closed = False
        self.auth   = auth

    def close(self, nowait=False):
        self.closed = True
        self.conn.send(self.auth, zmq.SNDMORE)
        self.conn.send("close", zmq.SNDMORE)
        if nowait:
            self.conn.send("%d" % self.chan, zmq.SNDMORE)
            self.conn.send("nowait")
        else:
            self.conn.send("%d" % self.chan)

        t = time.time() - self.when0
        debug("timing", "total: %.4f / %d / %d/s" % (t, self.read, self.read/t))
        return(self.conn.recv())

    def _eof(self, data):
        return("done" in data or "fail" in data)

    def fetchall(self):
        while (True):
            if (self.closed):
                break
            for el in self.fetch():
                yield el

    def fetch(self):
        if (self.closed):
            raise(RuntimeError("closed"))
        self.when = time.time()
        self.conn.send(self.auth, zmq.SNDMORE)
        self.conn.send("fetch", zmq.SNDMORE)
        self.conn.send("%d" % self.chan)
        msg = self.conn.recv()
        self.read += len(msg)
        debug("timing", "fetch: %.4f / items: %d" % (time.time() - self.when, len(msg)))
        if (self._eof(msg)):
            if "done" in msg:
                msg.remove('done')
            else:
                msg.remove('fail')
            self.close()
        else:
            msg.remove('item')

        lt = []
        more = 1
        while len(msg) > 0 and more > 0:
            if 'list' == msg[0]:
                more = int(msg[1])
                msg  = msg[2:]
            elif 'name' == msg[0]:
                lt.append(msg)
                more = 0
            else:
                lt.append((msg[0], msg[2:2+int(msg[1])]))
                msg   = msg[2+int(msg[1]):]
                more -= 1
        return lt

    def fetchone(self):
        return(self.fetch())

#Library Tests

def setUpModule ():

    def gen_mc_name(num):
        for x in range(num):
            yield "hm" + "{0:04d}".format(x)

    def gen_ni_name(num):
        for k in range(8):
            x = num * 8 + k
            lt = []
            while x > 0:
                lt.append("%02x" % (x%256))
                x = x // 256
            while (len(lt) < 6):
                lt.append("00")
            yield ":".join(lt)

    def gen_sw_name(num):
        num *= 8 
        return ("sw%04d" % (num//64))

    def create(orig, label, dest):
        return (["make (%s)" % orig, "make (%s)" % dest, "make (%s) -[%s]> (%s)" % (orig, label, dest)])

    def create_mc(x):
        machine = "hm%04d" % x
        switch = gen_sw_name(x)

        lt = create("ITA", "machine", machine)
        lt.extend(create("ITA", "switch", switch))

        for mac in gen_ni_name(x):
            lt.extend(create(machine, "nic", mac))
            lt.extend(create(mac, "link", switch))

        return("using (test_namespace) %s;" % "\n".join(lt))

    if len(sys.argv) > 1: 
        conn = Connection(context(), ["tcp://warp0017.locaweb.com.br:4080"])
        conn.connect()
        for x in range(10):
            query = create_mc(x)
            chann = conn.execute(query)
            chann.close(nowait=True)

def tearDownModule ():
    pass

class UserLibTest (unittest.TestCase):

    def setUp(self):
        self.conn = Connection(context(), ["tcp://warp0017.locaweb.com.br:4080"])

    def test_database_is_empty (self):
        with self.conn.cursor("using (test_database) path (test_ITA);") as cursor:
            for row in cursor:
                self.assertEqual(row, [])

    def test_make_node(self):
        with self.conn.cursor("using (test_database) make (test_ITA);") as cursor:
            for row in cursor:
                pass
        with self.conn.cursor("using (test_database) path (test_ITA);") as cursor:
            for row in cursor:
                print row

if __name__ == '__main__':
    unittest.main(argv=[sys.argv[0]])
