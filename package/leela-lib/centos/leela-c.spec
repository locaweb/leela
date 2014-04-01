Name:           leela-c
Group:          libs
Version:        %(../../../src/scripts/read-version.sh)
Release:        1%{?dist}
Summary:        Leela C library

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc

%description
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

 This package provides a ansi C library that implements the warpdrive
 protocol.

%prep
%setup -q -n %{name}-%{version}

%build
make -C src/c build

%install
mkdir -p $RPM_BUILD_ROOT/%{_libdir}
cp -p src/c/libleela.so* $RPM_BUILD_ROOT/%{_libdir}
chmod 755 $RPM_BUILD_ROOT/%{_libdir}/libleela.so*
mkdir -p $RPM_BUILD_ROOT/%{_includedir}/leela
cp -p src/c/src/leela/*.h $RPM_BUILD_ROOT/%{_includedir}/leela
chmod 644 $RPM_BUILD_ROOT/%{_includedir}/leela/*.h

%files
%defattr(-,root,root)
%{_includedir}/leela
%{_libdir}/libleela.so
%{_libdir}/libleela.so.4.0.0

%changelog
* Tue Apr 01 2014 dgvncsz0f
new release: %{version}-%{Release}
