# -*- coding: utf-8 -*-

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

    def __init__(self, exe):
        self.exe   = exe
        self.clean = True

    def execute(self, tree, *stmt):
        while (not self.clean):
            self.message()
        env  = {"rnd_name.0": names.rnd_name(),
                "rnd_name.1": names.rnd_name(),
                "rnd_name.2": names.rnd_name(),
                "rnd_name.3": names.rnd_name()}
        stmt = [line % env for line in stmt]
        self.exe.stdin.write("%s\n" % json.dumps("using (%s) %s;" % (tree, "\n".join(stmt))))
        self.exe.stdin.flush()
        self.clean = False

    def message(self):
        try:
            return(self.messages().next())
        except StopIteration:
            return(None)

    def messages(self):
        while (True):
            data = self.exe.stdout.readline()
            if (data == ""):
                break
            data       = json.loads(data)
            self.clean = data == None
            yield(data)

class Driver(object):

    def __init__(self, program, endpoint, user, secret, timeout):
        self.user     = user
        self.secret   = secret
        self.program  = program
        self.timeout  = timeout
        self.endpoint = endpoint

    @contextlib.contextmanager
    def session(self):
        with open("try_leela.stderr", "a") as fh:
            exe = subprocess.Popen(shlex.split(self.program),
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
                yield(Session(exe))
            finally:
                close_(exe.stdin)
                close_(exe.stdout)
                exe.wait()
