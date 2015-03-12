This document contains information about how to create packages for
debian and centos. But make sure you follow the
[preparation steps](../devel/environment.md#prepare-the-environment)
as packaging depends on that.

After you create the package it is available at
``/leela/pkg/dist``. You can change using the ``distroot`` variable.

# CLIENT LIBRARIES

## libleela

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

## libleela-python

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

## libleela-ruby

TODO:fixme

# warpdrive

It requires ``libleela-devel`` installed to build properly.

Debian packages:

```.bash
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela_*.deb
debian7.amd64 $ gdebi /leela/pkg/dist/debian7/{arch}/libleela/libleela-dev_*.deb
debian7.amd64 $ make -C /leela/pkg leela-warpdrive.debian dist=debian7 arch=amd64
```
