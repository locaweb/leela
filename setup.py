#!/usr/bin/python

import os
from setuptools import find_packages
from setuptools import setup

setup( name             = "leela-server",
       version          = "0.0.12",
       description      = "Collect and Chart anything",
       author           = "Juliano Martinez, Diego Souza",
       author_email     = "juliano@martinez.io",
       url              = "https://github.com/ncode/leela-server",
       install_requires = ["gevent>=0.13.6", "pycassa", "bottle", "supay", "argparse"],
       packages         = find_packages("src"),
       package_dir      = {"": "src"},
       data_files       = [],
       entry_points     = {
           "console_scripts": [
               "leela-server = leela.lasergun.server:main",
               "leela-web    = leela.readata.server:main"
           ],
     )
