#!/usr/bin/python

from distutils.core import setup
from distutils.core import Extension

ffi_endpoint = Extension("_leela_endpoint",
                         sources             = ["src/ffi/python_endpoint.c"],
                         include_dirs        = ["../c/src"],
                         libraries           = ["leela"],
                         library_dirs        = ["../c"],
                         extra_compile_args  = ["-std=c99", "-Wall"])

ffi_lql = Extension("_leela_lql",
                    sources             = ["src/ffi/python_lql.c"],
                    include_dirs        = ["../c/src"],
                    libraries           = ["leela"],
                    library_dirs        = ["../c"],
                    extra_compile_args  = ["-std=c99", "-Wall"])

setup(name="leela",
      version      = "5.0.0",
      license      = "APACHE-2",
      description  = "Leela - scalable metrics monitoring engine",
      author       = "Diego Souza",
      author_email = "dsouza@c0d3.xxx",
      url          = "http://github.com/locaweb/leela",
      packages     = ["leela"],
      ext_modules  = [ffi_endpoint, ffi_lql],
      package_dir  = {"": "src"})
