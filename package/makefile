
srcroot   = $(shell cd $(CURDIR)/.. && pwd)
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
	ln -s -f -n $(dist) $(srcroot)/package/$(basename $@)/debian
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/leela \
	  -w "/leela/package/$(basename $@)" \
	  leela/$(dist)-$(arch) dpkg-buildpackage -us -uc

%.centos: dist=centos6
%.centos: arch=amd64
%.centos:
	@echo $(MAKE) $(basename $@) dist=$(dist) arch=$(arch)
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/leela \
	  -e package=$(basename $@) \
	  -e version=$(version) \
	  -w "/leela/package/$(basename $@)/centos" \
	  leela/$(dist)-$(arch) sh -c " \
	     rpmdev-setuptree; \
	     ../../mksource.sh >/rpmbuild/SOURCES/\$$package-\$$version.tar.gz; \
	     rpmbuild -bb \$$package.spec; \
	     mkdir -p ../rpm; \
	     cp -r /rpmbuild/* ../rpm"

clean:
	# N.B.: this assumes root user to build packages.
	#       Even more important, you do not use root user to
	#       clone/edit files.
	for f in $$(git ls-files -o "$(srcroot)"); \
	do \
	  if [ "$$(stat --printf=%u $$f)" -eq 0 ]; \
	  then \
	    $(maybesudo) rm -rf "$$f"; \
	  fi; \
	done

.PHONY: leela-c
