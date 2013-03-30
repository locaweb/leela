# -*- coding: utf-8 -*-

import contextlib
import itertools
import argparse
import tempfile
import random
import socket
import time
import sys
import os

SEED = "leela.dmproc.trial_timeline"

def fmt_size(n, units=[("%.0f"), ("%.0f K"), ("%.0f M"), ("%.2f G"), ("%.3f T")]):
    units = list(units)
    while (n > 750 and len(units) > 1):
        n = n / 1000.0
        u = units.pop(0)
    return(units[0] % (n,))

def debug(x):
    sys.stderr.write(x)
    sys.stderr.flush()

def strings(count):
    def take(n, i):
        r = set()
        for x in i:
            r.add("".join(x))
            if (n == len(r)):
                break
        r = list(r)
        random.shuffle(r)
        return(r)
    return(take(count, (itertools.permutations(SEED, len(SEED)))))

class measure(object):

    def __init__(self):
        self.t0    = time.time()
        self.s0    = 0
        self.total = 0
        self.avg   = None
        self.max   = None
        self.min   = None
        self.w     = 80
        self.units = ["%.0f /s", "%.0f K/s", "%0.f M/s", "%.2f G/s", "%.3f T/s"]
        self.dots  = [" - ", " \\ ", " | ", " / ", " - ", " \\ ", " | ", " / "]

    def measure(self, label, s1):
        t1          = time.time()
        diff        = t1 - self.t0
        self.s0    += s1
        self.total += s1
        if (diff > 0.06):
            dot = self.dots.pop(0); self.dots.append(dot)
            r = self.s0 / float(diff)
            if (self.avg is None):
                self.avg = [r, 2]
                self.min = r
                self.max = r
            else:
                self.max     = max(r, self.max)
                self.min     = min(r, self.min)
                self.avg[0] = self.avg[0] + (r - self.avg[0])/self.avg[1]
                self.avg[1] = self.avg[1] + 1
            self.t0 = t1
            self.s0 = 0
            if (r is not None):
                d = { "label": label
                    , "rate": fmt_size(r, units=self.units)
                    , "total": fmt_size(self.total)
                    , "avg": fmt_size(self.avg[0], units=self.units)
                    , "max": fmt_size(self.max, units=self.units)
                    , "min": fmt_size(self.min, units=self.units)
                    , "dot": dot
                    }
                l = "%(dot)s | %(label)s: %(avg)s | total: %(total)s | max: %(max)s | min: %(min)s" % d
                self.w = max(len(l), self.w)
                debug("\r" + " " * (self.w))
                debug("\r" + l)

    def done(self):
        debug("\n")

def mkload(opts, fh):
    opts.load = int(opts.load * 1000 * 1000)
    k = strings(opts.ksize)
    s = opts.load
    m = measure()
    debug("generating load test file ... [keys: %s, pktsz: %s, keylen: %s, load: %s]\n" % (fmt_size(len(k)), fmt_size(opts.pktsz), len(k[0]), fmt_size(s)))
    while (s > 0):
        p = []
        c = opts.pktsz
        while (True):
            i  = opts.load - s
            n  = k[i % len(k)]
            l  = "%s %s|%s %s %d.0;" % (opts.type, len(n), n, repr(random.random()), i)
            z  = len(l)
            s -= 1
            c -= z
            if (c >= 0):
                m.measure("mkload", z)
                p.append(l)
            else:
                if (opts.pktsz < z):
                    m.measure("mkload", z)
                    p.append(l)
                break
        fh.write("".join(p) + "\n")
    m.done()

@contextlib.contextmanager
def with_tempfile():
    (fh, fn) = tempfile.mkstemp()
    with (file(fn, "w+")) as ffh:
        os.unlink(fn)
        os.close(fh)
        yield(ffh)

def trial(opts):
    with with_tempfile() as fh:
        mkload(opts, fh)
        s = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM, 0)
        m = measure()
        debug("starting load test [sending file] ... \n")
        fh.seek(0)
        for l in fh:
            l = l.strip()
            m.measure("trial", len(l))
            s.sendto(l, 0, opts.socket)
        m.done()

if (__name__ == "__main__"):
    args = argparse.ArgumentParser()
    args.add_argument("--ksize",
                      type    = int,
                      default = 1000000,
                      dest    = "ksize",
                      help    = "number of distinct keys to generate [default: %(default)s]")
    args.add_argument("--load",
                      type    = float,
                      default = 1,
                      dest    = "load",
                      help    = "load size (M) [default: %(default)s]")
    args.add_argument("--pktsz",
                      type    = int,
                      default = 25000,
                      dest    = "pktsz",
                      help    = "apromixate size of the udp package %(default)s")
    args.add_argument("--socket",
                      type    = str,
                      default = "/tmp/databus",
                      dest    = "socket",
                      help    = "the unix socket to connect to [default: %(default)s]")
    args.add_argument("type",
                      type    = str,
                      choices = ["gauge", "counter", "absolute", "derive"],
                      help    = "the datatype to test")
    trial(args.parse_args())
