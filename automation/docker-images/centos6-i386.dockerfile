FROM leela/stage0-centos-6-i386
MAINTAINER dgvncsz0f
ADD stage-1.sh /tmp/bootstrap.sh
RUN env dist=centos6 arch=i386 /tmp/bootstrap.sh