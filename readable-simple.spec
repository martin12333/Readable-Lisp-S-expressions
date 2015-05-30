# This starter RPM spec file uses the Fedora conventions.
Name:           readable
Version:        1.0.8
Release:        1%{?dist}
Summary:        Improve Lisp readability (infix, sweet-expression indentation)
Group:		Development/Libraries
License:        MIT and GPLv2
URL:            http://readable.sourceforge.net/
Source0:        http://downloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz

BuildRequires:  guile clisp
BuildRequires:  python
Requires:       guile clisp

BuildRequires:   common-lisp-controller
Requires:        common-lisp-controller
Requires(post):  common-lisp-controller
Requires(preun): common-lisp-controller


# This is a single package; see "readable.spec" for a package spec
# that divides up the software into several smaller packages,
# which is helpful if someone doesn't want to install everything.

%global mydocs %{_defaultdocdir}/%{name}

# Scheme
%global GUILE_SITE %{_datadir}/guile/site/
%global readable_libdir %{GUILE_SITE}/readable

# Common Lisp.  "cl_name" is the name from Common Lisp, not the package name.
%global cl_name readable
%global common_lispdir            %{_datadir}/common-lisp
%global common_lisp_sourcedir     %{common_lispdir}/source
%global common_lisp_source_pkgdir %{common_lisp_sourcedir}/%{cl_name}
%global common_lisp_systemsdir    %{common_lispdir}/systems
%global pkg_asd_file %{cl_name}.asd

%description
This "readable" software improves the readability of Lisp S-expressions
by adding up to three tiers of new s-expression abbreviations.
These tiers are (oversimplified):

1. Curly-infix-expressions (c-expressions): Lists with {...} are infix, in a
   Lispy way: {a op b ...} maps to (op a b ...). No precedence, by intent.

2. Neoteric-expressions (n-expressions): An e(...) maps to
   (e ...), and e{...} with content maps to e({...}).

3. Sweet-expressions (t-expressions): Parentheses are deduced from indentation.

Unlike nearly all past efforts to improve s-expression readability, these
are general (the notation is independent from any underlying semantic)
and homoiconic (the underlying data structure is clear from the syntax).
They are also backwards-compatible; well-formatted traditional
s-expressions continue work normally.  Thus, it's easy to transition to
these, and you can use traditional forms whenever it's convenient.

This implementation supports Scheme and Common Lisp.
For Scheme these notations are defined in SRFI-105 and SRFI-110.

Note: *ONLY* the file sweet-clisp contains GPLv2 code; all other
files are licensed using the MIT license.

For more information, see: http://readable.sourceforge.net


%prep
%setup -q


%build
%configure
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install

%clean
# This "clean" section is defined for EPEL and really old Fedora:
rm -rf $RPM_BUILD_ROOT

%post
/usr/sbin/register-common-lisp-source %{cl_name}

%preun
/usr/sbin/unregister-common-lisp-source %{cl_name}

%files
%doc %{mydocs}/

%{_bindir}/*
%{_mandir}/*

# Scheme
%{readable_libdir}/*

# Common Lisp
%{common_lisp_source_pkgdir}/*
%{common_lisp_systemsdir}/%{pkg_asd_file}

%changelog

