#!/usr/bin/python

from distutils.core import setup
from distutils.core import Extension

ffi_lql = Extension("_leela_lql",
                    sources             = ["src/ffi/python_lql.c"],
                    libraries           = ["leela"],
                    extra_compile_args  = ["-Wall"])

setup(name="leela",
      version      = "6.3.0",
      license      = "APACHE-2",
      description  = "Leela - scalable metrics monitoring engine",
      author       = "Diego Souza",
      author_email = "dsouza@c0d3.xxx",
      url          = "http://github.com/locaweb/leela",
      packages     = ["pyleela"],
      ext_modules  = [ffi_lql],
      package_dir  = {"": "src"})
