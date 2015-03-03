FROM leela/stage0-centos-5-amd64
MAINTAINER dgvncsz0f
ADD stage-1.sh /tmp/install.sh
RUN env dist=centos5 arch=amd64 /tmp/install.sh