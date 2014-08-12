# Dockerfile for Leela

FROM ubuntu:14.04

MAINTAINER Stephano Ferreira <stephano.ferreira@locaweb.com.br>

# Install base packages
RUN sed 's/main$/main universe/' -i /etc/apt/sources.list
RUN apt-get update && apt-get install -y software-properties-common \
    python-software-properties wget dnsmasq-base python2.7 vim less \
    iputils-ping unzip git build-essential uuid-dev libtool git autoconf \
    libzmq-dev pkg-config

# Install Java
RUN add-apt-repository ppa:webupd8team/java -y
RUN apt-get update
RUN echo debconf shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
RUN apt-get install -y oracle-java7-installer oracle-java7-set-default

# Install Leiningen
ENV LEIN_ROOT 1
RUN wget https://raw.github.com/technomancy/leiningen/stable/bin/lein \
    -O /usr/local/bin/lein
RUN chmod 0755 /usr/local/bin/lein
RUN lein upgrade

# Install Haskell Compiler
RUN apt-get install -y ghc

# Install Cassandra
ADD bundle/bin/install-cassandra /usr/bin/install-cassandra
RUN install-cassandra 2.0.9
ADD bundle/bin/pipework /usr/bin/
ADD bundle/bin/start-cassandra /usr/bin/
ADD bundle/bin/run-command /usr/bin/
ADD bundle/etc/cassandra.hosts /etc/dnsmasq.d/0hosts
ADD bundle/etc/dnsmasq.conf /etc/dnsmasq.conf
ADD bundle/etc/resolv.conf /etc/resolv.dnsmasq.conf
EXPOSE 9160 9042

# Install Consul
RUN wget https://dl.bintray.com/mitchellh/consul/0.3.1_linux_amd64.zip \
    -O /tmp/consul.zip; unzip /tmp/consul.zip -d /usr/local/bin

# Add leela source to Docker
COPY bundle/leela.tar.gz /tmp/
RUN tar -zxf /tmp/leela.tar.gz --directory=/opt/

# Build blackbox
ENV JAVA_HOME /usr/lib/jvm/java-7-oracle
ENV JAVA_LIBRARY_PATH /usr/local/lib
ENV LD_LIBRARY_PATH /usr/local/lib
RUN cd /tmp; wget http://download.zeromq.org/zeromq-4.0.4.tar.gz; \
    tar -zxf zeromq-4.0.4.tar.gz; cd zeromq-4.0.4; ./configure; \
    make; make install
RUN cd /tmp; git clone https://github.com/zeromq/jzmq.git; cd jzmq; \
    ./autogen.sh; ./configure; make; make install
RUN cd /opt/leela/src/blackbox; lein uberjar

# Build warpdrive
RUN cd /opt/leela/src/warpdrive; cabal update; cabal sanbox init; \
    cabal install --only-dependencies; cabal configure -O2; cabal build
