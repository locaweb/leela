#!/usr/bin/python

import os
from setuptools import find_packages
from setuptools import setup

def find_datafiles(root, path_f):
    result = []
    for (root, dirname, files) in os.walk(root):
        tr_root = path_f(root)
        if (tr_root != False):
            result.append((tr_root, map(lambda f: os.path.join(root, f), files)))
    return(result)

accept_f = lambda f: reduce(lambda acc, p: acc or f.startswith(p), ("./etc", "./usr"), False)
path_f   = lambda f: accept_f(f) and f[1:] or False
setup(
    name             = "leela-server",
    version          = "1.0.0",
    description      = "Collect, Monitor and Analyze anything - server module",
    author           = "Juliano Martinez, Diego Souza",
    author_email     = "juliano@martinez.io",
    url              = "http://leela.readthedocs.org",
    install_requires = ["gevent>=0.13.6", "pycassa", "pytz", "bottle", "supay", "argparse"],
    packages         = find_packages("src"),
    package_dir      = {"": "src"},
    data_files       = find_datafiles(".", path_f),
    entry_points     = {
        "console_scripts": [
            "leela-server = leela.server.lasergun:main",
            "leela-web    = leela.server.webapi:main"
        ],
    })
