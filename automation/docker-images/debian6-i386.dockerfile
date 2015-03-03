FROM leela/stage0-debian-squeeze-i386
MAINTAINER dgvncsz0f
ADD stage-1.sh /tmp/bootstrap.sh
RUN env dist=debian6 arch=i386 /tmp/bootstrap.sh