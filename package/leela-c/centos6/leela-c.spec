Name:           leela-c
Group:          libs
Version:        %(env component=.leela-c ../../../src/scripts/read-version.sh)
Release:        1%{?dist}
Summary:        A client library for leela

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc >= 4

Requires:       libzmq3

%description
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

 This package provides a ansi C library that implements the warpdrive
 protocol.

%prep
%setup -q -n leela-c-%{version}
cmake -DCMAKE_INSTALL_PREFIX="$RPM_BUILD_ROOT/usr" -DLEELA_INSTALL_LIBDIR=%(basename %{_libdir})

%build
make

%install
make install

%files -n leela-c
%defattr(-,root,root)
%{_includedir}/leela
%{_includedir}/poly1305aes
%{_libdir}/libpoly1305aes.a
%{_libdir}/libleela.so*
