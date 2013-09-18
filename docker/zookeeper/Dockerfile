FROM bb0e0ec13618
MAINTAINER dgvncsz0f
RUN apt-get update && apt-get install -q --yes --force-yes zookeeper
CMD ["start-foreground"]
EXPOSE 2181
ENTRYPOINT ["/usr/share/zookeeper/bin/zkServer.sh"]
