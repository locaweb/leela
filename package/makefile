
srcroot   = $(shell cd $(CURDIR)/.. && pwd)
version   = $(shell $(srcroot)/src/scripts/read-version.sh)
maybesudo = $(shell [ "$$UID" -ne 0 ] && echo sudo)

images:
	cd docker-images && { \
	  $(gainroot) ./stage-0.sh; \
	  $(gainroot) ./stage-1.sh; \
	}

%.debian: dist=debian7
%.debian: arch=amd64
%.debian:
	@echo $(MAKE) $(basename $@) dist=$(dist) arch=$(arch)
	echo $(maybesudo) docker run \
	  -i -t -v $$HOME:$$HOME \
	  -w "$(srcroot)/package/$(basename $@)" \
	  leela/$(dist)-$(arch) dpkg-buildpackage -us -uc $(DEBUILDFLAGS)

%.centos: dist=centos6
%.centos: arch=amd64
%.centos:
	@echo $(MAKE) $(basename $@) dist=$(dist) arch=$(arch)
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/leela \
	  -e package=$(basename $@) \
	  -e version=$(version) \
	  -w "/leela/package/$(basename $@)/centos" \
	  leela/$(dist)-$(arch) sh -c 'rpmdev-setuptree; ../../mksource.sh >/rpmbuild/SOURCES/$$package-$$version.tar.gz; rpmbuild -bb $$package.spec'

.PHONY: leela-c
