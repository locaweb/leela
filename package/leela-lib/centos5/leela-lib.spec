Name:           leela-lib
Group:          libs
Version:        %(../src/scripts/read-version.sh)
Release:        4%{?dist}
Summary:        Leela Library Package

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc >= 4, zeromq-devel >= 3.2.2

%package -n leela-c
Group:          libs
Summary:        Leela C Package
Requires:       zeromq3

%package -n leela-python
Group:          libs
Summary:        Leela Python Package
Requires:       leela-c

%description
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

%description -n leela-c
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

 This package provides a ansi C library that implements the warpdrive
 protocol.

%description -n leela-python
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

 This package provides a binding in python for the leela-c library.

%prep
%setup -q -n leela-lib-%{version}

%build
make -C src/c build

%install
mkdir -p $RPM_BUILD_ROOT/%{_libdir}
cp -p src/c/libleela.so* $RPM_BUILD_ROOT/%{_libdir}
chmod 755 $RPM_BUILD_ROOT/%{_libdir}/libleela.so*
mkdir -p $RPM_BUILD_ROOT/%{_includedir}/leela
cp -p src/c/src/leela/*.h $RPM_BUILD_ROOT/%{_includedir}/leela
chmod 644 $RPM_BUILD_ROOT/%{_includedir}/leela/*.h
pushd src/python && {
  for pyver in python2.7 python2.6
  do
    if command -v $pyver
    then
      $pyver setup.py clean -a;
      $pyver setup.py install --root=$RPM_BUILD_ROOT --install-lib=/usr/lib/$pyver/site-packages;
      break
    fi
  done
}
popd

%files -n leela-c
%defattr(-,root,root)
%{_includedir}/leela
%{_libdir}/libleela.so
%{_libdir}/libleela.so.4.0.0

%files -n leela-python
%defattr(-,root,root)
/usr/lib/python2.?/site-packages
