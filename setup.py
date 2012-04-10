#!/usr/bin/python
from distutils.core import setup

setup(name='leela-server',
    version='0.0.1',
    description='Collect and Chart anything',
    author='Juliano Martinez',
    author_email='juliano@martinez.io',
    url='https://github.com/ncode/leela-server',
    install_requires=[
        'gevent>=0.13.6'],
    data_files=[('/usr/sbin', ['src/sbin/leela-server']),
                ('/etc/leela', ['src/etc/leela.conf']),
                ('/usr/share/leela/templates', ['src/share/templates/line-irregular.tmpl']),
        ]
    )
