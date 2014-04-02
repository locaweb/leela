
srcroot   = $(shell cd $(CURDIR)/.. && pwd)
distdir   = "/leela/package/dist/$(basename $@)/$(dist).$(arch)"
version   = $(shell $(srcroot)/src/scripts/read-version.sh)
maybesudo = $(shell [ "$$UID" -ne 0 ] && echo sudo)

images:
	cd docker-images && { \
	  $(maybesudo) ./stage-0.sh; \
	  $(maybesudo) ./stage-1.sh; \
	}

%.debian: dist=debian7
%.debian: arch=amd64
%.debian:
	@echo $(MAKE) $(basename $@) dist=$(dist) arch=$(arch)
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/leela \
	  -e HOME=/home/leela \
	  -e package="$(basename $@)" \
	  -e version=$(version) \
	  -e distdir=$(distdir) \
	  -w /leela/package \
	  -u leela \
	  leela/$(dist)-$(arch) /bin/bash -l -c " \
	    ./mksource.sh | tar -x -z -C /home/leela; \
	    cd /home/leela/\$$package-\$$version/package/\$$package; \
	    ln -s $(dist) debian; \
	    dpkg-buildpackage -us -uc; \
	    /leela/package/collect.sh ../*.{deb,gz,changes,dsc}"

%.centos: dist=centos6
%.centos: arch=amd64
%.centos:
	@echo $(MAKE) $(basename $@) dist=$(dist) arch=$(arch)
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/leela \
	  -e HOME=/home/leela \
	  -e package="$(basename $@)" \
	  -e version=$(version) \
	  -e distdir=$(distdir) \
	  -w /leela/package \
	  -u leela \
	  leela/$(dist)-$(arch) /bin/bash -l -c " \
	    rpmdev-setuptree; \
	    ./mksource.sh >/home/leela/rpmbuild/SOURCES/\$$package-\$$version.tar.gz; \
	    cp /leela/package/\$$package/centos/\$$package.spec /tmp/package.spec; \
	    rpmbuild -ba /tmp/package.spec; \
	    /leela/package/collect.sh /home/leela/rpmbuild/SRPMS/* /home/leela/rpmbuild/RPMS/*/*"

clean:
	rm -rf $(srcroot)/package/dist

.PHONY: leela-c
