#!/usr/bin/python

import os
import sys
from distutils.core import setup
from distutils.core import Extension

ffi_lql = Extension("_leela_lql",
                    sources             = ["src/ffi/python_lql.c"],
                    libraries           = ["leela"],
                    extra_compile_args  = ["-Wall"])

def read_version ():
    changelog = "../../../CHANGELOG.libleela-python"
    with open(changelog, "r") as fh:
        for entry in fh:
            if (entry.startswith("v")):
                version = entry.split(" ", 2)[0]
                return(version[1:])
    raise(RuntimeError("could not read package version"))

setup(name="leela",
      version      = read_version(),
      license      = "APACHE-2",
      description  = "Leela - scalable metrics monitoring engine",
      author       = "Diego Souza",
      author_email = "dsouza@c0d3.xxx",
      url          = "http://github.com/locaweb/leela",
      packages     = ["pyleela"],
      ext_modules  = [ffi_lql],
      package_dir  = {"": "src"})
