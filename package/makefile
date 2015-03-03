
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

warpgrep:
	$(MAKE) leela-warpgrep.debian

blackbox:
	$(MAKE) leela-blackbox.debian

leela-c:
	$(MAKE) leela-c.debian dist=debian7
	$(MAKE) leela-c.debian dist=debian6
	$(MAKE) leela-c.centos dist=centos5
	$(MAKE) leela-c.centos dist=centos5 arch=i386
	$(MAKE) leela-c.centos dist=centos6

collectd:
	$(MAKE) leela-collectd.debian dist=debian7
	$(MAKE) leela-collectd.debian dist=debian6
	$(MAKE) leela-collectd.centos dist=centos6
	$(MAKE) leela-collectd.centos dist=centos5
	$(MAKE) leela-collectd.centos dist=centos5 arch=i386

%.debian: dist=debian7
%.debian: arch=amd64
%.debian: package=$(basename $@)
%.debian: component=.$(package)
%.debian:
	@echo $(MAKE) $@ dist=$(dist) arch=$(arch)
	env dist=$(dist) \
	    version=$(version) \
	    package=$(package) \
	    ./makepkg.sh debian $(srcroot)/package/dist

%.centos: dist=centos6
%.centos: arch=amd64
%.centos: package=$(basename $@)
%.centos: component=.$(package)
%.centos:
	@echo $(MAKE) $@ dist=$(dist) arch=$(arch)
	env dist=$(dist) \
	    version=$(version) \
	    package=$(package) \
	    ./makepkg.sh centos $(srcroot)/package/dist

clean:
	rm -rf $(srcroot)/package/dist
