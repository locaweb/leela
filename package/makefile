
srcroot   = $(shell cd $(CURDIR)/.. && pwd)
distdir   = "/target/package/dist/$(basename $@)/$(dist).$(arch)"
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
	  -i -t -v $(srcroot):/target \
	  -e HOME=/home/leela \
	  -e package="$(basename $@)" \
	  -e version=$(version) \
	  -e distdir=$(distdir) \
	  -w /target/package \
	  -u leela \
	  leela/$(dist)-$(arch) /bin/bash -l -c " \
	    ./mksource.sh | tar -x -z -C \$$HOME; \
	    cd \$$HOME/\$$package-\$$version/package/\$$package; \
	    [ -d \"$(dist)\" ] && ln -s $(dist) debian; \
	    dpkg-buildpackage -us -uc; \
	    /target/package/collect.sh ../*.{deb,gz,changes,dsc}"

%.centos: dist=centos6
%.centos: arch=amd64
%.centos:
	@echo $(MAKE) $(basename $@) dist=$(dist) arch=$(arch)
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/target \
	  -e HOME=/home/leela \
	  -e package="$(basename $@)" \
	  -e version=$(version) \
	  -e distdir=$(distdir) \
	  -w /target/package \
	  -u leela \
	  leela/$(dist)-$(arch) /bin/bash -l -c " \
	    rpmdev-setuptree; \
	    ./mksource.sh >\$$HOME/rpmbuild/SOURCES/\$$package-\$$version.tar.gz; \
	    cp /target/package/\$$package/centos/\$$package.spec /tmp/package.spec; \
	    rpmbuild -ba /tmp/package.spec; \
	    /target/package/collect.sh \$$HOME/rpmbuild/SRPMS/* \$$HOME/rpmbuild/RPMS/*/*"

clean:
	rm -rf $(srcroot)/package/dist

.PHONY: leela-c
