# -*- coding: utf-8 -*-

import sys; _stdout = sys.stdout; _stdin = sys.stdin; _stderr = sys.stderr;
from leela.server.network import cassandra_proto
from twisted.internet import reactor
from leela.server import config
from datetime import datetime
import traceback
import subprocess
import argparse
import httplib
import os.path
import select
import socket
import json
import math
import time
import sys
import re
import os

def initd(opts, name):
    return(os.path.join(opts.chdir, "etc/init.d", name))

def split(s):
    args = s.split(" ", 1)
    if (len(args) == 1):
        return(args[0], "")
    else:
        params = args[1].partition("#")[0].strip()
        return(args[0], split_args(params))

def split_args(s):
    args  = []
    quote = False
    for w in expand(s).split(" "):
        if (not quote and w.startswith("\"")):
            quote = True
            args.append([w])
        elif (quote):
            args[-1].append(w)
            if (w.endswith("\"")):
                quote = False
                args.append(" ".join(args.pop())[1:-1])
        else:
            args.append(w)
    return(dict(map(lambda s: s.split("=", 1), args)))

def expand(s):
    r = r"{now(?:(\+|-)(\d+(?:s|m|h|d)))?(\|.*?)?}"
    t = math.floor(time.time())
    g = re.search(r, s)
    while (g is not None):
        f = lambda t: datetime.fromtimestamp(t).strftime((g.group(3) or " %s.0")[1:])
        o = g.group(1)
        if (o is not None):
            a  = int(g.group(2)[:-1], 10)
            a *= {"s": 1, "m": 60, "h": 3600, "d": 86400}[g.group(2)[-1]]
            if (o == "+"):
                s = s[:g.start()] + f(t + a) + s[g.end():]
            elif (o == "-"):
                s = s[:g.start()] + f(t - a) + s[g.end():]
            else:
                raise(RuntimeError("the impossible happened!"))
        else:
            s = s[:g.start()] + f(t) + s[g.end():]
        g = re.search(r, s)
    s = s.replace("{now}", repr(t))
    s = s.replace("\\n", "\n")
    return(s)

def make_relative(series):
    t0 = series[0][0]
    for k in range(len(series)):
        series[k] = [series[k][0] - t0, series[k][1]]
    return(series)

def execute(opts, command, **env):
    env = dict(os.environ)
    env["CHDIR"] = opts.chdir
    p = subprocess.Popen(command, env=env, stdout=_stderr, stderr=_stderr)
    return(p)

def initd_start(opts, script):
    p = execute(opts, [initd(opts, script), "start"])
    return(p.wait())

def initd_stop(opts, script):
    p = execute(opts, [initd(opts, script), "stop"])
    return(p.wait())

def initd_restart(opts, script):
    initd_stop(opts, script)
    return(initd_start(opts, script))

def send_frame(s, data):
    size = struct.pack(">H", len(data))
    s.send(size + data)

def recv_frame(s):
    size = struct.unpack(">H", s.recv(2))
    return(s.recv(size))

def cassandra_truncate(opts, cf):
    storage = cassandra_proto.CassandraProto(opts.config)
    storage.startService()
    cc = storage.truncate(cf)
    cc.addCallback(lambda _: storage.stopService())
    cc.addCallback(lambda _: reactor.stop())
    reactor.run()
    return(0)

def udp_send(opts, message):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, 0)
    p = opts.config.getint("udp", "port")
    h = opts.config.get("udp", "address").replace("0.0.0.0", "localhost")
    s.sendto(message, 0, (h, p))
    r = select.select([s.fileno()], [], [], 1)[0]
    if (len(r) == 1):
        _stdout.write(s.recv(65535))
        _stdout.write("\n")
    s.close()
    return(0)

def echo(opts, string):
    _stdout.write(string)
    return(0)

def sleep(opts, seconds):
    time.sleep(float(seconds))
    return(0)

def http_request(opts, method, url, data=None):
    h = opts.config.get("http", "address")
    p = opts.config.getint("http", "port")
    e = "http://%s:%d" % (h, p)
    r = httplib.HTTPConnection(h, p)
    r.request(method, url, data)
    rply = json.loads(r.getresponse().read())
    if ("results" in rply):
        for k in rply["results"].keys():
            rply["results"][k]["series"] = make_relative(rply["results"][k]["series"])
    _stdout.write(json.dumps(rply, sort_keys=True))
    _stdout.write("\n")
    return(0)

def invoke(f):
    def g(opts, cmd, **kwargs):
        try:
            rc = f(opts, **kwargs)
            _stderr.write("%s: %d" % (cmd, rc))
            _stderr.write("\n")
            return(rc)
        except Exception, e:
            _stdout.write(traceback.format_exc())
            _stdout.write("%s: -1 (%s)" % (cmd, e))
            _stdout.write("\n")
            return(-1)
    return(g)

def run_script(opts, script):
    engine = {"initd-start": invoke(initd_start),
              "initd-stop": invoke(initd_stop),
              "initd-restart": invoke(initd_restart),
              "echo": invoke(echo),
              "sleep": invoke(sleep),
              "cassandra-truncate": invoke(cassandra_truncate),
              "udp-send": invoke(udp_send),
              "http-request": invoke(http_request)
             }
    for (cmd, args) in map(split, script.splitlines()):
        if (cmd.startswith("#")):
            continue
        engine[cmd](opts, cmd, **args)

if (__name__ == "__main__"):
    args = argparse.ArgumentParser()
    args.add_argument("--chdir",
                      dest     = "chdir",
                      default  = os.environ.get("CHDIR"),
                      help     = "The root dir you installed leela onto [default: %(default)s]")
    args.add_argument("--config",
                      dest     = "config",
                      default  = config.default_config_file(),
                      help     = "The leela config file to parse and use [default: %(default)s]")
    opts = args.parse_args()
    opts.config = config.read_config(opts.config)
    sys.exit(run_script(opts, sys.stdin.read()))
