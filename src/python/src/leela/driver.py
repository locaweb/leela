# -*- coding: utf-8 -*-

import sys
import json
from leela import lql
from leela.exception import LeelaError

def read_lines(fd):
    while (True):
        l = fd.readline()
        if (l == ""):
            break
        yield(json.loads(l)[0].encode("ascii"))

def run():
    session = json.loads(sys.stdin.readline())
    with lql.with_context([e.encode("ascii") for e in session["endpoint"]]) as ctx:
        for stmt in read_lines(sys.stdin):
            with lql.with_cursor(ctx, session["username"], session["secret"], session["timeout"]) as cursor:
                cursor.execute(stmt)
                while (cursor.next()):
                    try:
                        row = cursor.fetch()
                    except LeelaError, e:
                        row = ("fail", e.code, e.message)
                    sys.stdout.write("%s\n" % json.dumps([row]))
                    sys.stdout.flush()
                sys.stdout.write("[null]\n")
                sys.stdout.flush()

if (__name__ == "__main__"):
    run()
