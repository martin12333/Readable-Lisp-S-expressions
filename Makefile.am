# Makefile.am for "readable" project

# Copyright (C) 2007-2013 by David A. Wheeler and Alan Manuel K. Gloria.
#
# This software is released as open source software under the "MIT" license:
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.


# Usual automake header, so some auxilliary files are tucked into m4/:

ACLOCAL_AMFLAGS = -I m4 --install
EXTRA_DIST = m4/DUMMY

# GNU Standards "General Conventions for Makefiles" says set SHELL.  The
# autoconf documentation section "The Make Macro SHELL" says do it this way:
SHELL = @SHELL@

# Guile original source files are in "src/".  At build time we create
# the guile library files in "readable/" and user-executable scripts in
# "bin/". The "bin/" scripts depend on the final GUILE_SITE location
# of the library.  These files will be installed by "make install".

# As a convenience for testing, we *also* create scripts in the *current*
# (top) directory that will depend on the local "readable/" versions,
# *EVEN IF* another version of the readable library is installed.
# This way, when testing, you can just use
# ./unsweeten (or whatever) and it will depend on the development version
# without having to set an environment variable (which is easy to forget).
# If we don't do this, it'll be easy to test the "wrong version" of the
# library, wasting time.

UNSWEETEN = ./unsweeten

CLEANFILES =

# Here is the usual POSIX-portable inference rule for unsweetening:
# .sscm.scm:
#	$(UNSWEETEN) < $< > $@
# .SUFFIXES: .sscm .scm
# We can't use that inference rule in this makefile, because POSIX doesn't
# allow us to portably declare a dependency in inference rules.
# Since we're *building* unsweeten, we *do* have such a dependency.
# A lot of makes will let you do it anyway, as an obvious extension,
# but in this Makefile we'll use the more portable approach
# of giving separate rules for each target.


# Rules for translating markdown (.md) files into HTML.
SUFFIXES = .md .html
.md.html:
	rm -f "$@"
	( cat head.html ; $(MARKDOWN) $< ; cat tail.html ) | \
	  $(SED) -e "s/@TITLE@/"$*"/g" > $@
	chmod a-w "$@"

EXTRA_DIST += head.html tail.html markdown-urls
EXTRA_DIST += markdown2.py

# Guile libraries.

if WITH_GUILE
readable_libdir = $(GUILE_SITE)/readable

readable_lib_DATA = readable/kernel.scm

# "Local" version so we can test things without installing them.
readable/kernel.scm: src/kernel.scm
	$(MKDIR_P) readable
	cp -p "$(srcdir)/src/kernel.scm" readable/kernel.scm

CLEANFILES += readable/kernel.scm
endif WITH_GUILE


# Common Lisp files.  We'll install source code in conventional places,
# and let the usual Common Lisp mechanisms (e.g., asdf) handle compiling
# as needed.

CLISP = @CLISP@

common_lispdir = $(datadir)/common-lisp
common_lisp_sourcedir = $(common_lispdir)/source
common_lisp_source_pkgdir = $(common_lisp_sourcedir)/readable
common_lisp_systemsdir = $(common_lispdir)/systems

pkg_asd_file=readable.asd

if WITH_COMMON_LISP
dist_common_lisp_source_pkg_DATA = \
 basic-curly.lisp neoteric.lisp backquote.lisp sweet.lisp

dist_common_lisp_source_pkg_DATA += $(pkg_asd_file)

# Install symlink so asdf can find these files. See:
# http://www.gnu.org/software/automake/manual/html_node/Extending.html
# We first try to "rm" the symlink, in case it already exists.
# Note that the LN_S must *not* use DESTDIR in its first parameter.

install-exec-hook-cl:
	$(MKDIR_P) "$(DESTDIR)$(common_lisp_source_pkgdir)"
	$(MKDIR_P) "$(DESTDIR)$(common_lisp_systemsdir)"
	$(RM) -f "$(DESTDIR)$(common_lisp_systemsdir)/$(pkg_asd_file)"
	$(LN_S)  "$(common_lisp_source_pkgdir)/$(pkg_asd_file)" \
	         "$(DESTDIR)$(common_lisp_systemsdir)/$(pkg_asd_file)"

install-exec-hook: install-exec-hook-cl
endif WITH_COMMON_LISP


# Scripts

script_sources = src/sweeten.sscm src/unsweeten \
                 src/curly-guile src/neoteric-guile src/sweet-guile \
                 src/sweet-run src/sweet-scsh \
                 src/diff-s-sweet \
                 src/kernel.scm \
                 src/srfi-curly-infix.scm

EXTRA_DIST += $(script_sources)

noinst_SCRIPTS =
bin_SCRIPTS =
dist_bin_SCRIPTS =

# This lists scripts that will be generated by "make", and later installed.
generated_bin_scripts =

if WITH_GUILE

generated_bin_scripts += bin/sweeten bin/unsweeten \
                         bin/curly-guile bin/neoteric-guile bin/sweet-guile \
                         bin/sweet-run \
                         bin/diff-s-sweet

# These are the local scripts in the currect directory,
# for testing and ease-of-use (so people don't have to install things).
# These files are *not* directly installed.
# It's the same list as above, with "." instead of "bin", but there doesn't
# seem to be a standard way in make to change *prefixes* without using
# GNU make extensions like $(NAME:...%...=...%...) or $(addprefix ...).
noinst_SCRIPTS += ./sweeten ./unsweeten \
                  ./curly-guile ./neoteric-guile ./sweet-guile \
                  ./sweet-run ./diff-s-sweet

if WITH_SCSH
generated_bin_scripts += bin/sweet-scsh
noinst_SCRIPTS        += ./sweet-scsh
endif WITH_SCSH
endif WITH_GUILE

# These bindir scripts are generated at build time, so they're not distributed
# (there's no dist_ prefix for bin_SCRIPTS here):
bin_SCRIPTS += $(generated_bin_scripts)

if WITH_COMMON_LISP
# These are scripts that are just copied straight.
dist_bin_SCRIPTS += sweet-sbcl
endif WITH_COMMON_LISP

if WITH_CLISP
# These are scripts that are just copied straight.
dist_bin_SCRIPTS += sweet-clisp
endif WITH_CLISP

# Clean up generated scripts, but don't clean up *all* scripts,
# since sweet-sbcl and sweet-clisp are not generated.
CLEANFILES += $(generated_bin_scripts)
CLEANFILES += $(noinst_SCRIPTS)



# my-subst substitutes info, esp. the library directory, which is different
# between the files in bin/ and the top-level directory.
# We could use autoconf's substitution engine, but that runs at *configure*
# time, which is the wrong time; per the GNU guidelines you're supposed to
# also be able to change the directory at the later *build* time.
# So we create a trivial script to do the substitutions, and run it
# at build time (e.g., "make" with no options).
# The setting GUILE_READLINE_AVAILABLE is from autoconf-generated configure.
# To add a substitution, add it here and in my-subst.

MY_SUBST = $(srcdir)/my-subst "$(GUILE)" "$(GUILE_READLINE_AVAILABLE)"



# For now, we'll just generate the "sweeten" script with traditional
# Scheme.  Eventually probably want to put sweeten into the library.

# The repetition below is annoying.  Need to look up a better way to
# do this.  If it weren't for my-subst, we could just do "cp -p $< $@".


EXTRA_DIST += $(srcdir)/my-subst

if WITH_GUILE
bin/sweeten: src/sweeten.sscm ./unsweeten my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" \
	   < "$(srcdir)/src/sweeten.sscm" | ./unsweeten > $@
	chmod a+x $@

bin/unsweeten: src/unsweeten my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" < $<  > $@
	chmod a+x $@

bin/curly-guile: src/curly-guile my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" < $<  > $@
	chmod a+x $@

bin/neoteric-guile: src/neoteric-guile my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" < $<  > $@
	chmod a+x $@

bin/sweet-guile: src/sweet-guile my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" < $<  > $@
	chmod a+x $@

bin/sweet-scsh: src/sweet-scsh my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" < $<  > $@
	chmod a+x $@

bin/sweet-run: src/sweet-run my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" < $<  > $@
	chmod a+x $@

bin/diff-s-sweet: src/diff-s-sweet my-subst
	$(MKDIR_P) bin
	$(MY_SUBST) "$(readable_libdir)" "" < $<  > $@
	chmod a+x $@

./sweeten: src/sweeten.sscm ./unsweeten my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" \
           < "$(srcdir)/src/sweeten.sscm" | ./unsweeten > $@
	chmod a+x $@

# Make "unsweeten" depend on the underlying library.
# That way, once "./unsweeten" is created, we know that it's ready to use
# (because its dependencies have been installed). Then other rules can just
# depend on "./unsweeten", and they won't start until unsweeten is ready.
./unsweeten: src/unsweeten my-subst readable/kernel.scm
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./curly-guile: src/curly-guile my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./neoteric-guile: src/neoteric-guile my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./sweet-guile: src/sweet-guile my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./sweet-scsh: src/sweet-scsh my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./sweet-run: src/sweet-run my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./diff-s-sweet: src/diff-s-sweet my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./sweeten.sscm: src/sweeten.sscm my-subst
	$(RM) -f $@
	$(MY_SUBST) "" "$(builddir)$(PATH_SEPARATOR)" < $<  > $@
	chmod a+x $@
	chmod a-w $@

# Not directly used; useful if you want to generate JUST this file:
,sweeten.scm : src/sweeten.sscm $(UNSWEETEN)
	$(RM) -f $@
	$(UNSWEETEN) $< > $@
	chmod a-w $@
endif WITH_GUILE

# Implementation and test cases for ANTLR implementation
# of sweet-expressions.  Again, don't install.
EXTRA_DIST += sweet.g
EXTRA_DIST += output/input.txt output/correct.txt output/go
EXTRA_DIST += output/scheme/Pair.java

# Distribute examples (but don't install them).
EXTRA_DIST += \
 examples/sweet-run-demo-guile \
 examples/sweet-run-demo-scsh \
 examples/sweet-run-demo-clisp \
 examples/list-of.sscm \
 examples/simple-nfx.sscm \
 examples/streams.sscm \
 examples/basic-sweet.sscm \
 examples/Makefile.example \
 examples/README


# Man pages
dist_man_MANS =

if WITH_GUILE
dist_man_MANS += man/unsweeten.1 man/sweet-run.1 \
                 man/sweeten.1 man/diff-s-sweet.1
endif WITH_GUILE
if WITH_CLISP
dist_man_MANS += man/sweet-clisp.1
endif WITH_CLISP

# Distribute some documentation.
# README is distributed anyway; this just makes it clearer.
# Distribute some description of the solution and tutorial,
# in markdown format.  The website has the updated version, but for
# those who just have this file, it'd be useful to have SOME useful
# information on what this code is trying to do and how to use it.
dist_doc_DATA = README COPYING COPYING.GPL AUTHORS ChangeLog

# Distribute demo Makefile as documentation, to help ease use.
dist_doc_DATA += Makefile-demo

dist_doc_DATA += README-ANTLR README-files

# These the markdown (.md) files that are on the Wiki and we'll
# duplicate in the distributed package:
MDMIRRORS = Problem.md Solution.md Install-howto.md \
            Scheme-tutorial.md Common-lisp-tutorial.md

# These are stub files that link to the website:
MDSTUBS = Rationale.md Retort.md Examples.md

# These are all markdown (.md) files, including stubs:
MDFILES = $(MDMIRRORS) $(MDSTUBS)

dist_doc_DATA += $(MDFILES)

# Generate, distribute, and install HTML files generated from .md files:
if ENABLE_HTML_FROM_MARKDOWN
dist_doc_DATA += $(MDFILES:.md=.html)
endif ENABLE_HTML_FROM_MARKDOWN

# Presentation in Open Document Format (.odp) about these formats:
dist_doc_DATA += readable-s-expressions.odp

# Include SRFI specifications as documentation:
dist_doc_DATA += SRFI-105.html SRFI-110.html

# Include in documentation some trivial example of a Common Lisp program,
# in ".lisp" and ".slisp" formats:
dist_doc_DATA += factorial.lisp
dist_doc_DATA += factorial.slisp

# This is an odd case.  Since we're distributing a tool for developers,
# it's useful to have worked examples.  So here we install the
# "sweeten.sscm" file as a worked example.
dist_doc_DATA += src/sweeten.sscm

# We no longer distribute these obsolete specs (see git if you want them).
# dist_doc_DATA += spec-indent.txt spec-almkglor.txt

# Distribute test drivers and testsuites.
EXTRA_DIST += \
 tests/curly-infix-test-cl \
 tests/curly-infix-testsuite-scm \
 tests/curly-infix-test-scm \
 tests/full-test-cl \
 tests/neoteric-test \
 tests/neoteric-testsuite \
 tests/sweet-test \
 tests/sweet-testsuite \
 tests/roundtrip-test \
 tests/roundtrip-testsuite \
 tests/pprint \
 tests/sweeten-testsuite \
 tests/sweeten-correct \
 tests/unsweeten-testsuite \
 tests/unsweeten-correct \
 tests/unsweeten-cl-testsuite \
 tests/unsweeten-cl-correct \
 tests/curly-infix-correct2 \
 tests/curly-infix-testsuite2 \
 tests/write-test \
 tests/write-test-correct

# TODO: Improve test harness & integration into overall system.

EXTRA_DIST += my-realpath
check: check-clisp check-guile

if WITH_CLISP
check-clisp: my-realpath
	@echo "Starting tests for CLISP"
	CL_SOURCE_REGISTRY="`$(srcdir)/my-realpath $(srcdir)`"/ \
	  $(CLISP) -lp "`$(srcdir)/my-realpath $(srcdir)`/tests/" \
	        $(srcdir)/tests/curly-infix-test-cl
	CL_SOURCE_REGISTRY="`$(srcdir)/my-realpath $(srcdir)`"/ \
	  $(CLISP) -lp "`$(srcdir)/my-realpath $(srcdir)`/tests/" \
	        $(srcdir)/tests/full-test-cl
else !WITH_CLISP
check-clisp:
endif !WITH_CLISP

if WITH_GUILE
check-guile: readable/kernel.scm my-realpath
	@echo "Starting tests for GUILE"
	@echo "About to run guile curly-infix tests"
	GUILE_LOAD_PATH="$(builddir)" $(srcdir)/tests/curly-infix-test-scm \
	            < $(srcdir)/tests/curly-infix-testsuite-scm
	@echo "About to run neoteric-testsuite"
	GUILE_LOAD_PATH="$(builddir)" $(srcdir)/tests/neoteric-test \
	            < $(srcdir)/tests/neoteric-testsuite
	@echo "About to run sweet-testsuite"
	GUILE_LOAD_PATH="$(builddir)" $(srcdir)/tests/sweet-test \
	            < $(srcdir)/tests/sweet-testsuite
	@echo "About to run write-test"
	GUILE_LOAD_PATH="$(builddir)" $(srcdir)/tests/write-test \
             | diff -b -u "$(srcdir)/tests/write-test-correct" -
	@echo "Running srfi-curly-infix, showing diffs from correct results:"
	GUILE_LOAD_PATH="$(builddir)" guile \
	            $(srcdir)/src/srfi-curly-infix.scm < \
	            $(srcdir)/tests/curly-infix-testsuite2 | \
	          diff -u "$(srcdir)/tests/curly-infix-correct2" -
	@echo "Running unsweeten, showing differences from correct results:"
	"$(builddir)/unsweeten" < "$(srcdir)/tests/unsweeten-testsuite" 2>&1 | \
	     diff -b -u "$(srcdir)/tests/unsweeten-correct" -
	test "`printf '\140(a b c ,d)\n' | \"$(builddir)/unsweeten\" -C`" = \
	     '`(a b c ,d)'
	"$(builddir)/unsweeten" -C < "$(srcdir)/tests/unsweeten-cl-testsuite" \
             | diff -b -u "$(srcdir)/tests/unsweeten-cl-correct" -
	@echo "Running sweeten, showing differences from correct results:"
	"$(builddir)/sweeten" < "$(srcdir)/tests/sweeten-testsuite" | \
	     diff -u "$(srcdir)/tests/sweeten-correct" -
	@echo "About to run roundtrip-test"
	PATH="$(builddir):$(srcdir)/tests:${PATH}" \
	     $(srcdir)/tests/roundtrip-test $(srcdir)/tests/roundtrip-testsuite
else !WITH_GUILE
check-guile:
endif !WITH_GUILE

# Developer conveniences:
# "make push" interpreted as "git push" when current branch is "develop".
# "make pull" interpreted as "git pull"
# "make test" interpreted as "make check"
push:
	[ "`git symbolic-ref HEAD`" = "refs/heads/develop" ] && \
	    git push origin develop

pull:
	git pull

test:
	make check

# SRFI-110 files to distribute.
srfi_110_fileset = SRFI-110.html sweet.g src/kernel.scm output/scheme/Pair.java
110.tar.gz: $(srfi_110_fileset)
	rm -fr 110/
	mkdir  110/
	cp -p $(srfi_110_fileset) 110/
	tar cvzf 110.tar.gz 110/
	rm -fr 110/

110: 110.tar.gz


# The following creates an "rpm" package, using an rpm spec file.
# It focuses on Fedora, but it should be a good starting point for
# other packages.  If you package for Fedora, be sure to set up your system:
# - yum install @development-tools
# - yum install fedora-packager
# For more information on RPM and Fedora, see:
# - http://fedoraproject.org/wiki/How_to_create_an_RPM_package
# - http://fedoraproject.org/wiki/Packaging:Guidelines
# Note that the results will be in "~/rpmbuild/RPMS/".

SPECFILE = readable.spec
EXTRA_DIST += $(SPECFILE) readable-simple.spec

rpm: $(DIST_ARCHIVES) $(SPECFILE)
	@echo "REMINDER: Run 'make dist' if you have updated the distribution"
	cp -p $(DIST_ARCHIVES) ~/rpmbuild/SOURCES/
	cp -p $(SPECFILE) ~/rpmbuild/SPECS/
	rpmbuild -ba $(SPECFILE)
	-rpmlint $(SPECFILE)

# Extract possibly-updated .md files from the wiki:
from-wiki:
	./get-from-wiki $(MDMIRRORS)

.PHONY: push pull test 110 rpm check-clisp check-guile from-wiki

