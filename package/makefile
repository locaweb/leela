
srcroot   = $(CURDIR)/..
distdir   = "/target/package/dist/$(package)/$(dist).$(arch)"
version   = $(shell env component=$(component) $(srcroot)/src/scripts/read-version.sh)

warpdrive:
	$(MAKE) leela-warpdrive.debian

warpgrep:
	$(MAKE) leela-warpgrep.debian

blackbox:
	$(MAKE) leela-blackbox.debian

libleela:
	$(MAKE) libleela.debian dist=debian7
	$(MAKE) libleela.debian dist=debian6
	$(MAKE) libleela.centos dist=centos5
	$(MAKE) libleela.centos dist=centos6

collectd:
	$(MAKE) leela-collectd.debian dist=debian7
	$(MAKE) leela-collectd.debian dist=debian6
	$(MAKE) leela-collectd.centos dist=centos6
	$(MAKE) leela-collectd.centos dist=centos5

%.debian: dist=debian7
%.debian: arch=amd64
%.debian: package=$(basename $@)
%.debian: component=.$(package)
%.debian:
	@echo $(MAKE) $@ bootstrap=$(bootstrap) dist=$(dist) arch=$(arch)
	env dist=$(dist) \
	    arch=$(arch) \
	    version=$(version) \
	    package=$(package) \
	    $(CURDIR)/makepkg.sh debian $(srcroot)/$(package)/dist

%.centos: dist=centos6
%.centos: arch=amd64
%.centos: package=$(basename $@)
%.centos: component=.$(package)
%.centos:
	@echo $(MAKE) $@ bootstrap=$(bootstrap) dist=$(dist) arch=$(arch)
	env dist=$(dist) \
	    version=$(version) \
	    package=$(package) \
	    $(CURDIR)/makepkg.sh centos $(srcroot)/$(package)/dist

clean:
	rm -rf $(srcroot)/package/dist
