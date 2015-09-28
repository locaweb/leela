Name:           collectd-leela
Group:          libs
Version:        %(env component=.collectd-leela ../src/scripts/read-version.sh)
Release:        1%{?dist}
Summary:        Leela Collectd Plugin
Obsoletes:      leela-collectd

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc >= 4, cmake >= 2.8.9, libleela-devel

Requires:       libleela, bc, procps

%description
 Write plugin that sends collectd data to leela.

%prep
%setup -q -n collectd-leela-%{version}
cmake -DLEELA_BUILD_COLLECTD=on \
      -DCMAKE_INSTALL_PREFIX="$RPM_BUILD_ROOT/usr" \
      -DLEELA_INSTALL_LIBDIR=%(basename %{_libdir})/collectd \
      -DLEELA_INSTALL_ARCDIR=%(basename %{_libdir})/collectd

%build
%{__make}

%install
mkdir -p "$RPM_BUILD_ROOT/%{_bindir}"
%{__make} install
install -m 0755 src/collectd/wl_cpu-scale.sh "$RPM_BUILD_ROOT/%{_bindir}/wl_cpu-scale"

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
