FROM leela/stage0-centos-5-i386
MAINTAINER dgvncsz0f
ADD stage-1.sh /tmp/install.sh
RUN env dist=centos5 arch=i386 /tmp/install.sh