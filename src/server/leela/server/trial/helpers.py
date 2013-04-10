# -*- coding: utf-8 -*-

import contextlib
import itertools
import argparse
import tempfile
import struct
import random
import socket
import math
import time
import sys
import os

def fmt(n, units=[("%.0f"), ("%.0f K"), ("%.1f M"), ("%.2f G"), ("%.3f T")]):
    units = list(units)
    while (n > 750 and len(units) > 1):
        n = n / 1000.0
        u = units.pop(0)
    return(units[0] % (n,))

def fmt_time(t):
    h = math.floor(t / 3600)
    m = math.floor((t - h*3600) / 60)
    s = t - h*3600 - m*60
    return("%02d:%02d:%02d" % (h, m, s))

def debug(x):
    sys.stderr.write(x)
    sys.stderr.flush()

def frame(s):
    return(struct.pack(">H", len(s)) + s)

def framelen(s):
    return(struct.unpack(">H", s)[0])

def recv_frame(s):
    size = s.recv(2)
    if (size == ""):
        return(None)
    size = framelen(size)
    return(s.recv(size))

def send_frame(s, msg):
    s.send(frame(msg))

def strings(seed, count):
    def take(n, i):
        r = set()
        for x in i:
            r.add("".join(x))
            if (n == len(r)):
                break
        r = list(r)
        random.shuffle(r)
        return(r)
    return(take(count, (itertools.permutations(seed, len(seed)))))

class progress(object):

    def __init__(self):
        self.state = {"time": time.time(),
                      "curr": 0,
                      "rate": 0,
                      "total": 0,
                      "mean": [None, None],
                      "max": None,
                      "min": None
                     }
        self.width = 80
        self.lastd = time.time()
        self.start = time.time()
        self.units = ["%.0f /s", "%.0f K/s", "%.1f M/s", "%.2f G/s", "%.3f T/s"]
        self.dots  = [" - ", " \\ ", " | ", " / ", " - ", " \\ ", " | ", " / "]

    def measure(self, v):
        now                  = time.time()
        diff                 = now - self.state["time"]
        self.state["curr"]  += v
        self.state["total"] += v
        if (diff > 0.01):
            val = self.state["curr"] / float(diff)
            if (self.state["mean"][0] is None):
                self.state["max"]  = val
                self.state["min"]  = val
                self.state["mean"] = [val, 2.0]
            else:
                n                     = self.state["mean"][1]
                mean                  = self.state["mean"][0]
                self.state["max"]     = max(val, self.state["max"])
                self.state["min"]     = min(val, self.state["min"])
                self.state["mean"][0] = mean + (val - mean)/n
                self.state["mean"][1] = n + 1
            self.state["rate"] = val
            self.state["time"] = now
            self.state["curr"] = 0

    def dump_state(self, label):
        now = time.time()
        if (now - self.lastd > 0.05):
            self.lastd = now
            dot = self.dots.pop(0)
            self.dots.append(dot)
            d = { "label": label,
                  "rate": fmt(self.state["rate"], units=self.units),
                  "total": fmt(self.state["total"]),
                  "avg": fmt(self.state["mean"][0] or 0, units=self.units),
                  "max": fmt(self.state["max"] or 0, units=self.units),
                  "min": fmt(self.state["min"] or 0, units=self.units),
                  "dot": dot,
                  "elapsed": fmt_time(now - self.start)
                }
            l = "%(dot)s | %(label)s: %(avg)s | total: %(total)s | max: %(max)s | min: %(min)s | elapsed: %(elapsed)s" % d
            self.width = max(len(l), self.width)
            debug("\r" + " " * self.width)
            debug("\r" + l)

    def done(self):
        debug("\n")

@contextlib.contextmanager
def with_tempfile():
    (fh, fn) = tempfile.mkstemp()
    with (file(fn, "w+")) as ffh:
        os.unlink(fn)
        os.close(fh)
        yield(ffh)
