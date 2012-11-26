# -*- coding: utf-8; -*-

import threading
import argparse
import socket
import struct
import time
import sys

def send_frame(s, msg):
    s.send(struct.pack("!H", len(msg)))
    s.send(msg)

def recv_frame(s):
    tmp = s.recv(2)
    if (len(tmp) == 2):
        return(s.recv(struct.unpack("!H", tmp)[0]))
    return(None)

def repeat(n, f):
    def g(*args, **kwargs):
        for _ in range(n):
            r = f(*args, **kwargs)
            if (r is not None):
                sys.stdout.write(r)
                sys.stdout.flush()
    return(g)

if __name__ == "__main__":
    parser = argparse.ArgumentParser("dmproc_interact")
    parser.add_argument("-n", "--recv-num",
                        dest    = "recv_num",
                        type    = int,
                        default = "1"
                       )
    parser.add_argument("-s", "--socket",
                        dest    = "socket",
                        type    = str,
                        default = "/tmp/dmproc"
                       )
    parser.add_argument("-d", "--databus",
                        dest    = "databus",
                        type    = str,
                        default = "/tmp/dmproc-databus"
                       )
    parser.add_argument("proc",
                        metavar = "proc",
                        type    = str
                       )
    opts  = parser.parse_args()
    sock0 = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock0.connect(opts.socket)
    t     = threading.Thread(target=repeat(opts.recv_num, recv_frame), args=(sock0,))
    t.start()
    send_frame(sock0, opts.proc)

    sock1 = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
    sock1.connect(opts.databus)
    for l in sys.stdin.readlines():
        sock1.send(l.strip())
    sock1.close()
    t.join()
