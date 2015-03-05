Name:           libleela-python
Group:          Libraries
Version:        %(env component=.libleela-python "${srcroot:../../..}/src/scripts/read-version.sh")
Release:        1
Summary:        Leela Python Library

License:        ASL 2.0
URL:            https://github.com/locaweb/leela
Source0:        %{name}-%{version}.tar.gz

BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildRequires:  make, gcc >= 4, libleela-devel, python-devel >= 2.6

Requires:       libleela, python

%define pyversions__ python2.5 python2.6 python2.7

%description
 Leela is a property-graph engine that allows storing properties as
 time-series, besides the usual key-value properties.

 The graph engine is fairly simple and not intended to substitute real
 graph databases but to describe the relationship of the structure you
 are monitoring, such as a datacenter.

%prep
%setup -q -n %{name}-%{version}

%build
cd src/libs/python && {
  for pyver in %{pyversions__}
  do
    if command -v $pyver
    then
      $pyver setup.py build
    fi
  done
}

%install
cd src/libs/python && {
  for pyver in %{pyversions__}
  do
    if command -v $pyver
    then
      $pyver setup.py install --root="$RPM_BUILD_ROOT"
    fi
  done
}

%clean
cd src/libs/python && {
  for pyver in %{pyversions__}
  do
    if command -v $pyver
    then
      $pyver setup.py clean -a
    fi
  done
}

%files
%defattr(-,root,root)
%{_libdir}/python2.?/site-packages/pyleela
%{_libdir}/python2.?/site-packages/_leela_lql.so
%{_libdir}/python2.?/site-packages/leela-%{version}-py2.?.egg-info
