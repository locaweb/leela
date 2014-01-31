# -*- coding: utf-8 -*-
#!/usr/bin/python

import sys
import argparse
import unittest
import importlib
from try_leela import env

class Stream(object):

    def __init__(self, fh):
        self.write = fh.write
        self.flush = fh.flush

    def writeln(self, str):
        self.write(str)
        self.write("\n")
        self.flush()

def parse_args():
    parser = argparse.ArgumentParser(description = "leela testing engine")
    parser.add_argument("suite",
                        metavar = "SUITE",
                        choices = ("smoke", "perf", "integrity"),
                        help    = "The test suite you want to invoke")
    parser.add_argument("program",
                        metavar = "PROGRAM",
                        help    = "the program you want to test")
    parser.add_argument("--endpoint",
                        metavar = "ENDPOINT",
                        nargs   = "*",
                        default = ["tcp://localhost:4080"],
                        help    = "The leela endpoint you want to test")
    parser.add_argument("--username",
                        metavar = "USERNAME",
                        default = "leela",
                        help    = "The username to use to connect")
    parser.add_argument("--secret",
                        metavar = "SECRET",
                        default = "leela",
                        help    = "The secret to use to sign request messages")
    parser.add_argument("--logfile",
                        metavar = "LOGFILE",
                        default = "/dev/null",
                        help    = "The file to write the output of the driver program")
    parser.add_argument("--timeout-in-ms",
                        metavar = "TIMEOUT-IN-MS",
                        default = 60000,
                        type    = int,
                        dest    = "timeout",
                        help    = "The maximum time to wait for a response")
    return(parser.parse_args())

def main():
    args = parse_args()
    env.set_args(args)
    suite  = unittest.TestSuite()
    suite.addTests(unittest.TestLoader().discover("try_leela.suites.%s" % (args.suite), "test*.py"))
    runner = unittest.TextTestRunner()
    runner.run(suite)

if (__name__ == "__main__"):
    main()
