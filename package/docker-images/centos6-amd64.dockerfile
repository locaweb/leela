FROM leela/stage0-centos-6-amd64
MAINTAINER dgvncsz0f
ADD stage-1.sh /tmp/bootstrap.sh
RUN env dist=centos6 arch=amd64 /tmp/bootstrap.sh