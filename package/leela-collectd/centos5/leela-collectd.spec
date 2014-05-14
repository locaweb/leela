Name:           leela-collectd
Group:          libs
Version:        %(env component=.leela-collectd ../src/scripts/read-version.sh)
Release:        1%{?dist}
Summary:        Leela Collectd Plugin

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc >= 4, zeromq3-devel >= 3.2.2

Requires:       zeromq3

%description
 Write plugin that sends collectd data to leela.

%prep
%setup -q -n leela-collectd-%{version}

%build
make -C src/collectd build

%install
mkdir -p $RPM_BUILD_ROOT/%{_libdir}/collectd
cp -p src/collectd/write_leela.so* $RPM_BUILD_ROOT/%{_libdir}/collectd/write_leela.so

%post
[ -x /etc/init.d/collectd ] && /etc/init.d/collectd restart || exit 0

%files
%defattr(-,root,root)
%{_libdir}/collectd/write_leela.so
