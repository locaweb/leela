
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
	mkdir -p "$(srcroot)/package/target/$(basename $@)/$(dist).$(arch)"
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/leela \
	  -e package=$(basename $@) \
	  -e version=$(version) \
	  -e target="/leela/package/target/$(basename $@)/$(dist).$(arch)"
	  -w "/leela/package" \
	  leela/$(dist)-$(arch) su leela -c " \
	    ./mksource.sh | tar -x -z -C /home/leela; \
	    cd /home/leela/leela-\$$package-\$$version/package/\$$package; \
	    dpkg-buildpackage -us -uc;"

%.centos: dist=centos6
%.centos: arch=amd64
%.centos:
	@echo $(MAKE) $(basename $@) dist=$(dist) arch=$(arch)
	mkdir -p "$(srcroot)/package/target/$(basename $@)/$(dist).$(arch)"
	$(maybesudo) docker run \
	  -i -t -v $(srcroot):/leela \
	  -e package=$(basename $@) \
	  -e version=$(version) \
	  -e target="/leela/package/target/$(basename $@)/$(dist).$(arch)" \
	  -w "/leela/package" \
	  leela/$(dist)-$(arch) su leela -c " \
	     rpmdev-setuptree; \
	     ./mksource.sh >/home/leela/rpmbuild/SOURCES/\$$package-\$$version.tar.gz; \
	     rpmbuild -ba \$$package.spec; \
	     cp -r /home/leela/rpmbuild/SRPMS/* /home/leela/rpmbuild/RPMS/*/* \$$target"

clean:
	rm -rf $(srcroot)/package/target

.PHONY: leela-c
