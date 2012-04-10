#!/usr/bin/python
from distutils.core import setup

setup(name='leela-server',
    version='0.0.1',
    description='Collect and Chart anything',
    author='Juliano Martinez',
    author_email='juliano@martinez.io',
    url='https://github.com/ncode/leela-server',
    install_requires=['gevent>=0.13.6'],
    packages=['leela'],
    package_dir={'leela': 'src/lib'},
    data_files=[('/usr/sbin', ['src/sbin/leela-server', 'src/sbin/leela-web']),
                ('/etc/leela', ['src/etc/leela.conf']),
                ('/usr/share/leela/templates', ['src/share/templates/line-irregular.tmpl']),
                ('/usr/share/leela/js', ['src/share/js/adapters/prototype-adapter.js',
                                         'src/share/js/adapters/mootools-adapter.js',
                                         'src/share/js/modules/canvas-tools.js',
                                         'src/share/js/modules/exporting.js',
                                         'src/share/js/themes/dark-green.js',
                                         'src/share/js/jquery-1.7.2.min.js',
                                         'src/share/js/themes/dark-blue.js',
                                         'src/share/js/themes/skies.js',
                                         'src/share/js/themes/grid.js',
                                         'src/share/js/themes/gray.js',
                                         'src/share/js/highcharts.js']),
        ]
    )
