# Dockerfile for Leela

FROM ubuntu:14.04

MAINTAINER Stephano Ferreira <stephano.ferreira@locaweb.com.br>

# Install base packages
RUN sed 's/main$/main universe/' -i /etc/apt/sources.list
RUN apt-get update && apt-get install -y software-properties-common \
    python-software-properties wget dnsmasq-base python2.7 vim less \
    iputils-ping unzip

# Install Java
RUN add-apt-repository ppa:webupd8team/java -y
RUN apt-get update
RUN echo debconf shared/accepted-oracle-license-v1-1 select true | /usr/bin/debconf-set-selections
RUN apt-get install -y oracle-java7-installer oracle-java7-set-default

# Install Leiningen
ENV LEIN_ROOT 1
RUN wget https://raw.github.com/technomancy/leiningen/stable/bin/lein -O /usr/local/bin/lein
RUN chmod 0755 /usr/local/bin/lein
RUN lein upgrade

# Install Haskell Compiler
RUN apt-get install -y ghc

# Install Cassandra
ADD bin/install-cassandra /usr/bin/install-cassandra
RUN install-cassandra 2.0.9
ADD bin/pipework /usr/bin/
ADD bin/start-cassandra /usr/bin/
ADD bin/run-command /usr/bin/
ADD etc/cassandra.hosts /etc/dnsmasq.d/0hosts
ADD etc/dnsmasq.conf /etc/dnsmasq.conf
ADD etc/resolv.conf /etc/resolv.dnsmasq.conf
EXPOSE 9160 9042

# Install Consul
RUN wget https://dl.bintray.com/mitchellh/consul/0.3.1_linux_amd64.zip -O /tmp/consul.zip;
    unzip /tmp/consul.zip; mv /tmp/consul /usr/local/bin; rm /tmp/consul.zip
