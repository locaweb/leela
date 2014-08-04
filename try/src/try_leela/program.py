# -*- coding: utf-8 -*-

import sys
import json
import shlex
import select
import threading
import contextlib
import subprocess
from try_leela import names

def close_(fd):
    try:
        fd.close()
    except:
        pass

class Session(object):

    def __init__(self, exe, tree):
        self.exe   = exe
        self.tree  = tree
        self.clean = True

    def execute(self, *stmt):
        while (not self.clean):
            self.message()
        env  = {"rnd_name.0": "%s::%s" % (names.rnd_name(), names.rnd_name()),
                "rnd_name.1": "%s::%s" % (names.rnd_name(), names.rnd_name()),
                "rnd_name.2": "%s::%s" % (names.rnd_name(), names.rnd_name()),
                "rnd_name.3": "%s::%s" % (names.rnd_name(), names.rnd_name())}
        stmt = [line % env for line in stmt]
        self.exe.stdin.write("%s\n" % json.dumps(["using (%s) %s;" % (self.tree, "\n".join(stmt))]))
        self.exe.stdin.flush()
        self.clean = False

    def execute_fetch(self, *stmt):
        self.execute(*stmt)
        rows = []
        for row in self.messages():
            if (row is None):
                break
            rows.append(row)
        return(rows)

    def execute_fmap(self, f, *stmt):
        return(f(self.execute_fetch(*stmt)))

    def message(self):
        return(self.messages().next())

    def messages(self):
        while (True):
            data = self.exe.stdout.readline()
            if (data == ""):
                break
            try:
                data = json.loads(data)[0]
            except:
                raise(RuntimeError("bad message: %s" % data))
            self.clean = data == None
            yield(data)

class Driver(object):

    def __init__(self, opts):
        self.user     = opts.username
        self.secret   = opts.secret
        self.logfile  = opts.logfile
        self.program  = opts.program
        self.timeout  = opts.timeout
        self.endpoint = opts.endpoint
        self.logfile  = opts.logfile

    @contextlib.contextmanager
    def openlog(self):
        if (self.logfile == "-"):
            yield(sys.stderr)
        else:
            fh = open(self.logfile, "a")
            try:
                yield(fh)
            finally:
                fh.close()

    @contextlib.contextmanager
    def session(self, tree):
        with self.openlog() as fh:
            exe = subprocess.Popen(shlex.split("timeout %d %s" % (self.timeout / 1000, self.program)),
                                   stdin     = subprocess.PIPE,
                                   stdout    = subprocess.PIPE,
                                   stderr    = fh,
                                   close_fds = True)
            try:
                exe.stdin.write("%s\n" % json.dumps({"secret": self.secret,
                                                     "timeout": self.timeout,
                                                     "endpoint": self.endpoint,
                                                     "username": self.user}))
                exe.stdin.flush()
                yield(Session(exe, tree))
            finally:
                close_(exe.stdin)
                close_(exe.stdout)
                exe.wait()
