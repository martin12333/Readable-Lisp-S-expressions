# Makefile.am for "readable" project

# Usual automake header, so some auxilliary files are tucked into m4/:

ACLOCAL_AMFLAGS = -I m4 --install
EXTRA_DIST = m4/DUMMY

# Incantation per GNU Standards "General Conventions for Makefiles":
SHELL = /bin/sh

# The original source files are in "src/".  At build time we create
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

# Here is the usual POSIX-portable inference rule for unsweetening:
# .sscm.scm:
#	$(UNSWEETEN) < $< > $@
# .SUFFIXES: .sscm .scm
# We can't use that inference rule in this makefile, because POSIX doesn't
# allow us to portably declare a dependency in inference rules.
# Since we're *building* unsweeten, we *do* have such a dependency.
# A lot of makes will let you do it anyway, as an obvious extension,
# but here we'll use the more portable approach of giving separate rules
# for each target.

# Guile libraries.

readable_libdir = $(GUILE_SITE)/readable

readable_lib_DATA = readable/kernel.scm

readable/kernel.scm: src/kernel.scm
	$(MKDIR_P) readable
	cp -p "$(srcdir)/src/kernel.scm" readable/kernel.scm

CLEANFILES = readable/kernel.scm


# Scripts

script_sources = src/sweeten.sscm src/unsweeten \
                 src/curly-guile src/neoteric-guile src/sweet-guile \
                 src/sweet-run src/sweet-scsh \
                 src/diff-s-sweet \
                 src/kernel.scm \
                 src/srfi-curly-infix.scm

EXTRA_DIST += $(script_sources)

# These scripts are generated at build time, so they're not distributed
# (there's no dist_ prefix for bin_SCRIPTS here):
bin_SCRIPTS  = bin/sweeten bin/unsweeten
bin_SCRIPTS += bin/curly-guile bin/neoteric-guile bin/sweet-guile
bin_SCRIPTS += bin/sweet-run bin/sweet-scsh
bin_SCRIPTS += bin/diff-s-sweet
CLEANFILES  += $(bin_SCRIPTS)

# These are the local scripts, for testing, and are not installed.
noinst_SCRIPTS = ./sweeten ./unsweeten \
                 ./curly-guile ./neoteric-guile ./sweet-guile \
                 ./sweet-scsh ./sweet-run ./diff-s-sweet

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

bin/sweet-scsh: src/sweet-scsh
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

# Eventually we'll package this, but for now, just distribute
# the Common Lisp implementation.
EXTRA_DIST += readable.cl

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
dist_man_MANS = man/unsweeten.1 man/sweet-run.1 \
                man/sweeten.1 man/diff-s-sweet.1

# Distribute some documentation.
# README is distributed anyway; this just makes it clearer.
# Distribute some description of the solution and tutorial,
# in markdown format.  The website has the updated version, but for
# those who just have this file, it'd be useful to have SOME useful
# information on what this code is trying to do and how to use it.
dist_doc_DATA = README COPYING AUTHORS ChangeLog solution.md tutorial.md

# Presentation in Open Document Format (.odp) about these formats:
dist_doc_DATA += readable-s-expressions.odp

# Include SRFI specifications as documentation:
dist_doc_DATA += SRFI-105.html SRFI-110.html

# Distribute the specs.  They're not up-to-date, but they
# are referred to in the README, so distribute them too
# as users may be puzzled if they're not there.
dist_doc_DATA += spec-indent.txt spec-almkglor.txt

# This is an odd case.  Since we're distributing a tool for developers,
# it's useful to have worked examples.  So here we install the
# "sweeten.sscm" file as a worked example.
dist_doc_DATA += src/sweeten.sscm

# Distribute test drivers and testsuites.
EXTRA_DIST += \
 tests/curly-infix-test-cl \
 tests/curly-infix-testsuite-scm \
 tests/curly-infix-test-scm \
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
 tests/curly-infix-correct2 \
 tests/curly-infix-testsuite2

# TODO: Improve test harness & integration into overall system.

check: readable/kernel.scm
	CURLYFILE="$(srcdir)/readable.cl" $(srcdir)/tests/curly-infix-test-cl
	GUILE_LOAD_PATH="$(builddir)" $(srcdir)/tests/curly-infix-test-scm \
	            < $(srcdir)/tests/curly-infix-testsuite-scm
	GUILE_LOAD_PATH="$(builddir)" $(srcdir)/tests/neoteric-test \
	            < $(srcdir)/tests/neoteric-testsuite
	GUILE_LOAD_PATH="$(builddir)" $(srcdir)/tests/sweet-test \
	            < $(srcdir)/tests/sweet-testsuite
	@echo "Running srfi-curly-infix, showing diffs from correct results:"
	GUILE_LOAD_PATH="$(builddir)" guile \
	            $(srcdir)/src/srfi-curly-infix.scm < \
	            $(srcdir)/tests/curly-infix-testsuite2 | \
	          diff -u - "$(srcdir)/tests/curly-infix-correct2"
	@echo "Running unsweeten, showing differences from correct results:"
	"$(builddir)/unsweeten" < "$(srcdir)/tests/unsweeten-testsuite" | \
	     diff -b -u - "$(srcdir)/tests/unsweeten-correct"
	@echo "Running sweeten, showing differences from correct results:"
	"$(builddir)/sweeten" < "$(srcdir)/tests/sweeten-testsuite" | \
	     diff -u - "$(srcdir)/tests/sweeten-correct"
	PATH="$(builddir):$(srcdir)/tests:${PATH}" \
	     $(srcdir)/tests/roundtrip-test $(srcdir)/tests/roundtrip-testsuite

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
