Name:           collectd-leela
Group:          libs
Version:        %(env component=.collectd-leela ../src/scripts/read-version.sh)
Release:        1%{?dist}
Summary:        Leela Collectd Plugin

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc >= 4, libleela-devel

Requires:       zeromq, bc, procps, libleela

%description
 Write plugin that sends collectd data to leela.

%prep
%setup -q -n collectd-leela-%{version}

%build
make -C src/collectd build

%install
mkdir -p $RPM_BUILD_ROOT/%{_bindir}
mkdir -p $RPM_BUILD_ROOT/%{_libdir}/collectd
cp -a src/collectd/write_leela.so* $RPM_BUILD_ROOT/%{_libdir}/collectd/write_leela.so
cp -a src/collectd/wl_cpu-scale.sh $RPM_BUILD_ROOT/%{_bindir}/wl_cpu-scale

%post
wl_cpu_scale=/etc/collectd/collectd.conf.d/wl_cpu-scale.conf
mkdir -p $(dirname $wl_cpu_scale)
rm -f $wl_cpu_scale
wl_cpu-scale >$wl_cpu_scale || rm -f $wl_cpu_scale
[ -x /etc/init.d/collectd ] && /etc/init.d/collectd restart || pkill -KILL collectd
exit 0

%files
%defattr(-,root,root)
%{_libdir}/collectd/write_leela.so
%{_bindir}/wl_cpu-scale
