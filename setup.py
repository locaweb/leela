#!/usr/bin/python

import os
from distutils.core import setup
from distutils import sysconfig

# N.B.: I dont want to change the structure of setup.py (for instance
# using console_scripts).This hack makes it compatible with previous
# versions and also allow me to test it using virtualenv.
prefix = sysconfig.PREFIX
etcprefix = ["%s" % prefix, "%s/.." % prefix][os.path.isdir("%s/../etc" % prefix)]

setup(name='leela-server',
    version='0.0.12',
    description='Collect and Chart anything',
    author='Juliano Martinez',
    author_email='juliano@martinez.io',
    url='https://github.com/ncode/leela-server',
    install_requires=['gevent>=0.13.6','supay','hotqueue'],
    packages=['leela'],
    package_dir={'leela': 'src/lib'},
    data_files=[('%s/sbin' % prefix, ['src/sbin/leela-server',
                                      'src/sbin/leela-web',
                                      'src/sbin/leela-write',
                                      'src/sbin/leela-summarize']),
                ('%s/etc/leela' % etcprefix, ['src/etc/leela.conf']),
                ('%s/share/leela/templates' % prefix, ['src/share/templates/line-irregular.tmpl']),
                ('%s/share/leela/static/js' % prefix, ['src/share/static/js/adapters/prototype-adapter.js',
                                                       'src/share/static/js/adapters/mootools-adapter.js',
                                                       'src/share/static/js/modules/canvas-tools.js',
                                                       'src/share/static/js/modules/exporting.js',
                                                       'src/share/static/js/themes/dark-green.js',
                                                       'src/share/static/js/jquery-1.7.2.min.js',
                                                       'src/share/static/js/themes/dark-blue.js',
                                                       'src/share/static/js/themes/skies.js',
                                                       'src/share/static/js/themes/grid.js',
                                                       'src/share/static/js/themes/gray.js',
                                                       'src/share/static/js/highcharts.js',
                                                       'src/share/static/js/leelaserver-bundle.js'])
        ]
    )
