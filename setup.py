#!/usr/bin/python

import os
import sys
from setuptools import find_packages
from setuptools import setup

# source: http://stackoverflow.com/questions/7275295/how-do-i-write-a-setup-py-for-a-twistd-twisted-plugin-that-works-with-setuptools
# When pip installs anything from packages, py_modules, or ext_modules that
# includes a twistd plugin (which are installed to twisted/plugins/),
# setuptools/distribute writes a Package.egg-info/top_level.txt that includes
# "twisted".  If you later uninstall Package with `pip uninstall Package`,
# pip <1.2 removes all of twisted/ instead of just Package's twistd plugins.
# See https://github.com/pypa/pip/issues/355 (now fixed)
#
# To work around this problem, we monkeypatch
# setuptools.command.egg_info.write_toplevel_names to not write the line
# "twisted".  This fixes the behavior of `pip uninstall Package`.  Note that
# even with this workaround, `pip uninstall Package` still correctly uninstalls
# Package's twistd plugins from twisted/plugins/, since pip also uses
# Package.egg-info/installed-files.txt to determine what to uninstall,
# and the paths to the plugin files are indeed listed in installed-files.txt.
try:
    from setuptools.command import egg_info
    egg_info.write_toplevel_names
except (ImportError, AttributeError):
    pass
else:
    def _top_level_package(name):
        return name.split(".", 1)[0]

    def _hacked_write_toplevel_names(cmd, basename, filename):
        pkgs = dict.fromkeys(
            [_top_level_package(k)
                for k in cmd.distribution.iter_distribution_names()
                if _top_level_package(k) != "twisted"
            ]
        )
        cmd.write_file("top-level names", filename, "\n".join(pkgs) + "\n")

    egg_info.write_toplevel_names = _hacked_write_toplevel_names

def find_datafiles(root, path_f):
    result = []
    for (root, dirname, files) in os.walk(root):
        tr_root = path_f(root)
        if (tr_root != False):
            result.append((tr_root, map(lambda f: os.path.join(root, f), files)))
    return(result)

def install_deps():
    ver  = sys.version_info
    deps = ["gevent>=0.13.6", "pycassa", "bottle", "supay"]
    # if (ver[0] == 2 and ver[1] < 7):
    #     deps.append("argparse")
    return(deps)

accept_f = lambda f: reduce(lambda acc, p: acc or f.startswith(p), ("./etc", "./usr"), False)
path_f   = lambda f: accept_f(f) and f[1:] or False
setup(
    name             = "leela-server",
    version          = "1.0.0",
    description      = "Collect, Monitor and Analyze anything - server module",
    author           = "Juliano Martinez, Diego Souza",
    author_email     = "juliano@martinez.io",
    url              = "http://leela.readthedocs.org",
    # install_requires = install_deps(),
    namespace_packages = ["leela"],
    packages         = find_packages("src") + ["twisted.plugins"],
    package_dir      = {"": "src"},
    data_files       = find_datafiles(".", path_f),
    entry_points     = {
        "console_scripts": [
            "leela-server = leela.server.lasergun:main",
            "leela-web    = leela.server.webapi:main"
        ],
    })

# Make Twisted regenerate the dropin.cache, if possible.  This is necessary
# because in a site-wide install, dropin.cache cannot be rewritten by
# normal users.
try:
    from twisted.plugin import IPlugin, getPlugins
except ImportError:
    pass
else:
    list(getPlugins(IPlugin))
