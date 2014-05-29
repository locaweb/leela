
srcroot   = $(shell cd $(CURDIR)/.. && pwd)
distdir   = "/target/package/dist/$(basename $@)/$(dist).$(arch)"
version   = $(shell env component=$(component) $(srcroot)/src/scripts/read-version.sh)
maybesudo = $(shell [ "$$UID" -ne 0 ] && echo sudo)

images:
	cd docker-images && { \
	  $(maybesudo) ./stage-0.sh; \
	  $(maybesudo) ./stage-1.sh; \
	}

warpdrive:
	$(MAKE) leela-warpdrive.debian

blackbox:
	$(MAKE) leela-blackbox.debian

lib:
	$(MAKE) leela-lib.debian dist=debian7
	$(MAKE) leela-lib.debian dist=debian6
	$(MAKE) leela-lib.centos dist=centos5
	$(MAKE) leela-lib.centos dist=centos5 arch=i386
	$(MAKE) leela-lib.centos dist=centos6

collectd:
	$(MAKE) leela-collectd.debian dist=debian7
	$(MAKE) leela-collectd.debian dist=debian6
	$(MAKE) leela-collectd.centos dist=centos6
	$(MAKE) leela-collectd.centos dist=centos5
	$(MAKE) leela-collectd.centos dist=centos5 arch=i386

%.debian: dist=debian7
%.debian: arch=amd64
%.debian: component=.$(basename $@)
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
%.centos: component=.$(basename $@)
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
	    cp /target/package/\$$package/$(dist)/\$$package.spec /tmp/package.spec; \
	    rpmbuild -ba /tmp/package.spec; \
	    /target/package/collect.sh \$$HOME/rpmbuild/SRPMS/* \$$HOME/rpmbuild/RPMS/*/*"

clean:
	rm -rf $(srcroot)/package/dist
