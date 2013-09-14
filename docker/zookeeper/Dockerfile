FROM 47f8bde36e22
MAINTAINER dgvncsz0f
RUN apt-get update && apt-get install -q --yes --force-yes zookeeper
CMD ["start-foreground"]
EXPOSE 2181
ENTRYPOINT ["/usr/share/zookeeper/bin/zkServer.sh"]
