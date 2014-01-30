# -*- coding: utf-8 -*-

from try_leela import program

args = None

def set_args(value):
    global args
    args = value

def driver():
    global args
    return(program.Driver(args.program, args.endpoint, args.username, args.secret, args.timeout))
