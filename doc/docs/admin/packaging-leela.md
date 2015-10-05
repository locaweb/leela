This document contains information about how to create packages for
debian and centos. But make sure you follow the
[preparation steps](../devel/environment.md#prepare-the-environment)
as packaging depends on that.

After you create the package it is available at
``/leela/pkg/dist``. You can change using the ``distroot`` variable.

# CLIENT LIBRARIES

## LIBLEELA

Debian packages:

```.shell
debian7.amd64 $ make -C /leela/pkg libleela.debian dist=debian7 arch=amd64
debian7.amd64 $ ls -1 /leela/pkg/dist/debian7/{arch}/libleela/
libleela-dev_{version}_{arch}.deb
libleela_{version}.dsc
libleela_{version}.tar.gz
libleela_{version}_{arch}.changes
libleela_{version}_{arch}.deb
```

Centos packages:

```.shell
centos6.amd64 $ make -C /leela/pkg libleela.centos dist=centos6 arch=amd64
centos6.amd64 $ ls -1 /leela/pkg/dist/centos6/{arch}/libleela/
libleela-{version}.src.rpm
libleela-{version}.{arch}.rpm
libleela-devel-{version}.{arch}.rpm
```

## LIBLEELA-PYTHON

It requires ``libleela-devel`` installed to build properly.

Debian packages:

```.bash
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela_*.deb
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela-dev_*.deb
debian7.amd64 $ make -C /leela/pkg libleela-python.debian dist=debian7 arch=amd64
```

Centos packages:

```.bash
centos7.amd64 $ yum install /leela/pkg/dist/centos6/{arch}/libleela/libleela*.rpm
centos7.amd64 $ make -C /leela/pkg libleela-python.centos dist=centos6 arch=amd64
centos7.amd64 $ ls -1 /leela/pkg/dist/centos6/{arch}/libleela-python
libleela-python-{version}.src.rpm
libleela-python-{version}.{arch}.rpm
```

## LIBLEELA-RUBY

There is no package. Users should install use the gem
`leela_ruby`. The gem is at `src/libs/ruby/leela_ruby`.

# CORE MODULES

# BLACKBOX

It requires ``libleela-devel`` installed to build properly and only
debian packages are defined.

```.bash
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela_*.deb
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela-dev_*.deb
debian7.amd64 $ make -C /leela/pkg leela-blackbox.debian dist=debian7 arch=amd64
```

# FRONTEND

## WARPDRIVE

It requires ``libleela-devel`` installed to build properly and only
debian packages are defined.

```.bash
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela_*.deb
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela-dev_*.deb
debian7.amd64 $ make -C /leela/pkg leela-warpdrive.debian dist=debian7 arch=amd64
debian7.amd64 $ ls -1 /leela/pkg/dist/debian7/amd64/leela-warpdrive/
leela-warpdrive_{version}.dsc
leela-warpdrive_{version}.tar.gz
leela-warpdrive_{version}_{arch}.changes
leela-warpdrive_{version}_{arch}.deb
```
