#!/usr/bin/python

import re
import argparse
import readline
import traceback
from leela import lql

def resolve(cache, ctx, opts, guid):
    name = cache.get(guid)
    if (name is None):
        with lql.with_cursor(ctx, opts.username, opts.secret, opts.timeout) as cursor:
            cursor.execute("using (%s) name %s;" % (opts.tree, guid))
            if (cursor.next()):
                name        = cursor.fetch()[1][-2]
                cache[guid] = name
    return(name)

def gresolve(ctx, opts, guid):
    with lql.with_cursor(ctx, opts.username, opts.secret, opts.timeout) as cursor:
        cursor.execute("using (%s) guid (%s);" % (opts.tree, guid))
        if (cursor.next()):
            return(cursor.fetch()[1][-1])

def dump_name(cache, row):
    cache[row[1][-1]] = row[1][-2]
    print("name: %s | %s | %s | %s |" % row[1])

def dump_path(cache, ctx, opts, row):
    msg = []
    for (label, node) in row[1]:
        msg.append("-[%s]> (%s)" % (label, resolve(cache, ctx, opts, node)))
    print("path: %s" % (" ".join(msg),))

def dump_row(cache, ctx, opts, row):
    if (row[0] == "name"):
        dump_name(cache, row)
    elif (row[0] == "path"):
        dump_path(cache, ctx, opts, row)
    else:
        raise(RuntimeError())

def rewrite_path(cache, ctx, opts, q0):
    q = q0
    while (True):
        m = re.search(" (\([^\)]+?\))[ ;,]", q)
        if (m is None):
            break
        node = m.group()[2:-2]
        guid = gresolve(ctx, opts, node)
        q    = q.replace("(%s)" % node, guid, 1)
    return(q)

def execute(cache, ctx, opts, q):
    if (q.startswith("path")):
        q = rewrite_path(cache, ctx, opts, q)
    with lql.with_cursor(ctx, opts.username, opts.secret, opts.timeout) as cursor:
        rows = 0
        cursor.execute("using (%s) %s" % (opts.tree, q))
        while (cursor.next()):
            rows += 1
            dump_row(cache, ctx, opts, cursor.fetch())
        print("%d rows" % (rows,))

def read_eval_loop(ctx, opts):
    buff    = []
    cache   = {}
    prompt0 = "ilql # "
    prompt  = prompt0
    while (True):
        msg0 = raw_input(prompt)
        if (msg0 == ""):
            continue
        buff.append(msg0)
        if (msg0.endswith(";")):
            try:
                execute(cache, ctx, opts, " ".join(buff))
            except:
                print(traceback.format_exc())
            prompt = prompt0
            buff   = []
        else:
            prompt = ": "

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="lqli")
    parser.add_argument("-cluster", metavar="CLUSTER", type=str, nargs="*", required=True,
                        help="any subset of the leela cluster to connect to")
    parser.add_argument("-tree", metavar="TREE", type=str, required=True)
    parser.add_argument("-username", metavar="USERNAME", type=str, required=True)
    parser.add_argument("-secret", metavar="SECRET", type=str, default="")
    parser.add_argument("-timeout", metavar="TIMEOUT", type=int, default=30000)
    opts = parser.parse_args()
    with lql.with_context(opts.cluster) as ctx:
        read_eval_loop(ctx, opts)
