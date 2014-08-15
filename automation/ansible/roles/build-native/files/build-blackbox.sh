#!/usr/bin/env bash

basedir=$(dirname $0)

# ENVIRONMENT VARIABLES
export JAVA_HOME=/usr/lib/jvm/java-7-oracle
export JAVA_LIBRARY_PATH=/usr/local/lib
export LD_LIBRARY_PATH=/usr/local/lib

# INSTALL JZMQ AND ZEROMQ IF NOT INSTALLED
if [ ! -e /usr/lib/libjzmq.so ]; then
  cd /tmp; wget http://download.zeromq.org/zeromq-4.0.4.tar.gz; \
    tar -zxf zeromq-4.0.4.tar.gz; cd zeromq-4.0.4; ./configure; \
    make; make install

  cd /tmp; git clone https://github.com/zeromq/jzmq.git; cd jzmq; \
    ./autogen.sh; ./configure; make; make install
fi

# BUILD BLACKBOX
cd ${basedir}/leela/src/blackbox; lein uberjar
