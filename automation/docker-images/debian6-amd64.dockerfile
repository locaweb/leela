FROM leela/stage0-debian-squeeze-amd64
MAINTAINER dgvncsz0f
ADD stage-1.sh /tmp/bootstrap.sh
RUN env dist=debian6 arch=amd64 /tmp/bootstrap.sh