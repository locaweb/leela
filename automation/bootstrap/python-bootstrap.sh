#!/bin/sh

python_debian () {
  debian_install python-dev
}

python_centos () {
  rpm_install python26-devel python-devel
}

if has_command dpkg apt-get
then python_debian; fi

if has_command rpm yum
then python_centos; fi
