#!/usr/bin/python

import os
import sys
from distutils.core import setup
from distutils import log

def find_packages(root, path_f):
    result = []
    for (root, dirname, files) in os.walk(root):
        tr_root = path_f(root)
        if ("__init__.py" in files):
            result.append(tr_root)
    return(result)

def find_datafiles(root, path_f):
    result = []
    for (root, dirname, files) in os.walk(root):
        tr_root = path_f(root)
        result.append((tr_root, map(lambda f: os.path.join(root, f), files)))
    return(result)

packages   = find_packages("./src/server", lambda f: f[13:].replace("/", "."))
packages  += ["twisted.plugins"]
datafiles  = find_datafiles("./etc", lambda f: f[2:])
datafiles += find_datafiles("./usr", lambda f: f[2:])

setup(
    name               = "leela-server",
    version            = "2.0.0",
    description        = "Collect, Monitor and Analyze anything - server module",
    author             = "Juliano Martinez, Diego Souza",
    author_email       = "juliano@martinez.io",
    url                = "http://leela.readthedocs.org",
    namespace_packages = ["leela"],
    packages           = packages,
    package_dir        = {"": "src/server"},
    package_data       = {"twisted": ["src/server/twisted/plugins/twisted_leela.py"]},
    data_files         = datafiles)

# Make Twisted regenerate the dropin.cache, if possible.  This is necessary
# because in a site-wide install, dropin.cache cannot be rewritten by
# normal users.
try:
    from leela.server.services import *
    from twisted.plugin import IPlugin, getPlugins
    list(getPlugins(IPlugin))
except:
    log.warn("[WARN] Failed to update Twisted plugin cache")
