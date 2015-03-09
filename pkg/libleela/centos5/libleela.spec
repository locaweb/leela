Name:           libleela
Group:          Libraries
Version:        %(env component=.libleela "${srcroot:-../..}/../src/scripts/read-version.sh")
Release:        1
Summary:        Leela C Library

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc >= 3, cmake28 >= 2.8.9

%package -n libleela-devel
Group: Development/Libraries
Summary: Development files for %{name}
Requires: %{name} = %{version}-%{release}

%description -n libleela
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

 This package provides a ansi C library that implements the warpdrive
 protocol.

%description -n libleela-devel
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

 This package provides a ansi C library that implements the warpdrive
 protocol.

%prep
%setup -q -n libleela-%{version}
cmake28 -DLEELA_BUILD_LIBLEELA=on \
        -DCMAKE_INSTALL_PREFIX="$RPM_BUILD_ROOT/usr" \
        -DLEELA_INSTALL_LIBDIR=%(basename %{_libdir}) \
        -DLEELA_INSTALL_ARCDIR=%(basename %{_libdir})

%build
%{__make}

%install
%{__make} install

%files -n libleela
%defattr(-,root,root)
%{_libdir}/libleela.so*

%files -n libleela-devel
%{_includedir}/leela
%{_includedir}/poly1305aes
%{_libdir}/libpoly1305aes.a
