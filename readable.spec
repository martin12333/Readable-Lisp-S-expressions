# This RPM spec file uses the Fedora conventions and makes subpackages.
Name:    readable
Version: 0.9.2-develop
Release: 1%{?dist}
Summary: Improve Lisp readability (infix, sweet-expression indentation)
Group:   Development/Libraries
License: MIT
URL:     http://readable.sourceforge.net/
Source0: http://downloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz

# guile is required to build the "unsweeten" tool.
# clisp is required for "make check" to work.
BuildRequires:  guile clisp
BuildRequires:  common-lisp-controller

# RPM variables

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
these notations, and you can use traditional forms whenever it's convenient.

Both Scheme and Common Lisp are supported.
For Scheme these notations are defined in SRFI-105 and SRFI-110.

Note: *ONLY* the file sweet-clisp contains GPLv2 code; all other
files are licensed using the MIT license.

For more information, see: http://readable.sourceforge.net


%prep
%setup -q

%build
%configure
make %{?_smp_mflags}

%check
make check

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install

%clean
# This "clean" section is defined for EPEL and really old Fedora:
rm -rf $RPM_BUILD_ROOT


# Main package just has documentation, since we have more than
# one implementation.
%files
%doc %{mydocs}/


########## SUBPACKAGES ############
# This is broken into several subpackages so people
# don't get unneeded dependencies


# Subpackage guile-readable
%package -n guile-readable
Summary: GNU guile library that implements the "readable" notations.

Requires: guile
Requires: readable

%description -n guile-readable
GNU guile library that implements the "readable" notations.

%files -n guile-readable
%{readable_libdir}/*



# Subpackage readable-tools
%package tools
Summary: Tools to support the "readable" notations for Lisp-based languages.

Requires: guile-readable

%description tools
Various tools (general-purpose and guile-specific)
for the "readable" notation for Lisp-based languages
(Common Lisp, Scheme, etc.).

%files tools
%{_bindir}/diff-s-sweet
%{_bindir}/sweeten
%{_bindir}/unsweeten

%{_bindir}/curly-guile
%{_bindir}/neoteric-guile
%{_bindir}/sweet-guile
%{_bindir}/sweet-run

%{_mandir}/man1/diff-s-sweet.1.gz
%{_mandir}/man1/sweeten.1.gz
%{_mandir}/man1/unsweeten.1.gz
%{_mandir}/man1/sweet-run.1.gz



# Subpackage cl-readable
# The following follows the conventions (package name, etc.) of
# http://fedoraproject.org/wiki/Packaging:Lisp
%package -n cl-readable
Summary: Common Lisp library that implements the "readable" notations.

Requires: readable
Requires:        common-lisp-controller
Requires(post):  common-lisp-controller
Requires(preun): common-lisp-controller

%description -n cl-readable
A portable Common Lisp library that implements the "readable" notations.

%post -n cl-readable
/usr/sbin/register-common-lisp-source %{cl_name}

%preun -n cl-readable
/usr/sbin/unregister-common-lisp-source %{cl_NAME}

%files -n cl-readable
%{common_lisp_source_pkgdir}/*
%{common_lisp_systemsdir}/%{pkg_asd_file}


# Subpackage readable-scsh
%package scsh
Summary: An extra tool to help scsh users use the readable notation.
Requires: readable-guile scsh

%description scsh
An extra tool to help scsh users use the readable notation.

%files scsh
%{_bindir}/sweet-scsh


# Subpackage readable-clisp
%package clisp
Summary: An extra tool to help clisp users use the readable notation.
Requires: cl-readable
Requires: clisp
License: MIT and GPLv2

%description clisp
An extra tool to help clisp users use the readable notation.

%files clisp
%{_bindir}/sweet-clisp
%{_mandir}/man1/sweet-clisp*


# Subpackage readable-sbcl
%package sbcl
Summary: An extra tool to help sbcl users use the readable notation.
Requires: cl-readable
Requires: sbcl

%description sbcl
An extra tool to help sbcl users use the readable notation.

%files sbcl
%{_bindir}/sweet-sbcl



# Subpackage readable-all... for those who want it all.
%package all
Summary: *All* the files for the "readable" notation for Lisp-based languages.
Requires: readable guile-readable readable-tools
Requires: cl-readable
Requires: readable-scsh
Requires: readable-clisp readable-sbcl

%description all
Provides all the files that support the "readable" notation for
various Lisp-based languages.


%changelog

