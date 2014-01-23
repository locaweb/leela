#!/usr/bin/python

import os
import re
import time
import yaml
import atexit
import argparse
import readline
import traceback
from leela import lql

def resolve(cache, ctx, opts, guid):
    name = cache.get(guid)
    if (name is None):
        try:
            with lql.with_cursor(ctx, opts.username, opts.secret, opts.timeout) as cursor:
                cursor.execute("using (%s) name %s;" % (opts.tree, guid))
                if (cursor.next()):
                    name        = cursor.fetch()[1][-2]
                    cache[guid] = name
        except:
            name = guid
    return(name)

def gresolve(ctx, opts, guid):
    with lql.with_cursor(ctx, opts.username, opts.secret, opts.timeout) as cursor:
        cursor.execute("using (%s) guid (%s);" % (opts.tree, guid))
        if (cursor.next()):
            return(cursor.fetch()[1][-1])

def gmake(ctx, opts, guid):
    with lql.with_cursor(ctx, opts.username, opts.secret, opts.timeout) as cursor:
        cursor.execute("using (%s) make (%s);" % (opts.tree, guid))
        if (cursor.next()):
            return(cursor.fetch()[1][-1])

def dump_name(cache, row):
    cache[row[1][-1]] = row[1][-2]
    print("name: %s | %s | %s | %s |" % row[1])

def dump_stat(row):
    for (key, val) in row[1]:
        print("stat: %s = %s" % (key, val))

def dump_path(cache, ctx, opts, row):
    msg = []
    for (label, node) in row[1]:
        msg.append("-[%s]> (%s)" % (label, resolve(cache, ctx, opts, node)))
    print("path: %s" % (" ".join(msg),))

def dump_tree(cache, ctx, opts, row, tree0):
    row = [(label, resolve(cache, ctx, opts, node)) for (label, node) in row[1]]
    if (len(row) == 0 or row[0][1] not in tree0.get(row[0][0], {})):
        if (len(tree0) > 0):
            print(yaml.dump({"->>": tree0}, default_flow_style=False))
        tree0 = {}
    tree = tree0
    for (label, node) in row:
        if (label not in tree):
            tree[label] = {}
        tree = tree[label]
        if (node not in tree):
            tree[node] = {}
        tree = tree[node]
    return(tree0)

def dump_list(cache, ctx, opts, row):
    if (row[0] == "name"):
        dump_name(cache, row)
    elif (row[0] == "path"):
        dump_path(cache, ctx, opts, row)
    elif (row[0] == "stat"):
        dump_stat(row)
    else:
        raise(RuntimeError())

def rewrite_guid(cache, ctx, opts, q0, f):
    q = q0
    while (True):
        m = re.search(" (\([^\)]+?\))[ ;,]", q)
        if (m is None):
            break
        node = m.group()[2:-2]
        guid = f(ctx, opts, node)
        q    = q.replace("(%s)" % node, guid, 1)
    return(q)

def execute(cache, ctx, opts, q):
    tree = None
    if (q.startswith("path")):
        q = rewrite_guid(cache, ctx, opts, q, gresolve)
    elif (q.startswith("kill ")):
        q = rewrite_guid(cache, ctx, opts, q, gresolve)
    elif (q.startswith("make ") and (") -[" in q or ") <[" in q)):
        q = rewrite_guid(cache, ctx, opts, q, gmake)
    elif (q.startswith(".tree")):
        q    = rewrite_guid(cache, ctx, opts, q, gresolve)
        q    = "path%s" % (q[5:],)
        tree = {}
    with lql.with_cursor(ctx, opts.username, opts.secret, opts.timeout) as cursor:
        stat = {"rows": 0, "time": time.time()}
        cursor.execute("using (%s) %s" % (opts.tree, q))
        while (cursor.next()):
            stat["rows"] = stat["rows"] + 1
            if (tree is None):
                dump_list(cache, ctx, opts, cursor.fetch())
            else:
                tree = dump_tree(cache, ctx, opts, cursor.fetch(), tree)
        if (tree is not None):
            dump_tree(cache, ctx, opts, (None, []), tree)
        stat["time"] = time.time() - stat["time"]
        print("rows: %(rows)d; elapsed: %(time).2fs" % stat)

def ignore_except(f, *args, **kwargs):
    try:
        f(*args, **kwargs)
    except:
        pass

def read_eval_loop(ctx, opts):
    buff    = []
    cache   = {}
    prompt0 = "ilql # "
    prompt  = prompt0
    history = os.path.join(os.path.expanduser("~"), ".lqli")
    ignore_except(readline.read_history_file, history)
    atexit.register(readline.write_history_file, history)
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
