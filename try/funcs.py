#!/usr/bin/python
# -*- coding: utf-8; -*-

import bz2
import random
import socket
import threading
import contextlib
from leela import config
from leela import funcs
from leela.lasergun import server

def rand_hostname(size=6):
    alphabet = "abcdefghijklmnopqrstxywz"
    hostname = ""
    suffix   = ""
    while (size > 0):
        k = random.randint(0, len(alphabet)-1)
        hostname += alphabet[k]
        suffix   += str(k)[-1]
        size     -= 1
    return(hostname + suffix)

@contextlib.contextmanager
def lasergun_ctx(cassandra):
    c = config.read_config()
    a = (c.get("lasergun", "address"), c.getint("lasergun", "port"))
    s = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM, proto=socket.IPPROTO_UDP)
    t = threading.Thread(target=server.start, args=(c, s, cassandra))
    funcs.retry_on_fail(s.bind)(a)
    try:
        t.setDaemon(True)
        t.start()
        s1 = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM, proto=socket.IPPROTO_UDP)
        send = lambda data: s1.sendto(bz2.compress(data), a)
        recv = lambda: s1.recvfrom(16*1024)
        yield(send, recv)
    finally:
        funcs.suppress(s.close)()
        funcs.suppress(s1.close)()
        t.join()
