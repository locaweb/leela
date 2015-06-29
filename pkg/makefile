
srcroot   = $(CURDIR)
version   = $(shell env component=$(component) $(srcroot)/../src/scripts/read-version.sh)

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
	    srcroot=$(srcroot) \
	    bootstrap=$(bootstrap) \
	    $(CURDIR)/makepkg.sh debian

%.centos: dist=centos6
%.centos: arch=amd64
%.centos: package=$(basename $@)
%.centos: component=.$(package)
%.centos:
	@echo $(MAKE) $@ bootstrap=$(bootstrap) dist=$(dist) arch=$(arch)
	env dist=$(dist) \
	    arch=$(arch) \
	    version=$(version) \
	    package=$(package) \
	    srcroot=$(srcroot) \
	    bootstrap=$(bootstrap) \
	    $(CURDIR)/makepkg.sh centos
