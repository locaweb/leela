FROM leela/stage0-debian-wheezy-i386
MAINTAINER dgvncsz0f
ADD stage-1.sh /tmp/bootstrap.sh
RUN env dist=debian7 arch=i386 /tmp/bootstrap.sh