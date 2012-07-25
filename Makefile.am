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
# *EVEN IF* another version of the reable library is installed.
# This way, when testing, you can just use
# ./unsweeten (or whatever) and it will depend on the development version
# without having to set an environment variable (which is easy to forget).
# If we don't do this, it'll be easy to test the "wrong version" of the
# library, wasting time.

UNSWEETEN = ./unsweeten

# Here is the usual POSIX-portable inference rule for unsweetening:
# .sscm.scm:
#	$(UNSWEETEN) < $< > $@
# We can't use that inference rule in this makefile, because POSIX doesn't
# allow us to portably declare a dependency in inference rules.
# Since we're *building* unsweeten, we *do* have such a dependency.
# A lot of makes will let you do it anyway, as an obvious extension,
# but here we'll use the more portable approach of giving separate rules
# for each target.

# Guile libraries.

readable_libdir = $(GUILE_SITE)/readable

readable_lib_DATA = readable/sweet-impl.scm

readable/sweet-impl.scm:
	$(MKDIR_P) readable
	cp -p "$(srcdir)/src/sweet-impl.scm" readable/sweet-impl.scm

CLEANFILES = readable/sweet-impl.scm


# Scripts

script_sources = src/sweeten.sscm src/sweeten_header src/unsweeten src/neoteric-guile src/sweet-guile src/sweet-impl.scm
EXTRA_DIST += $(script_sources)

# These scripts are generated at build time, so they're not distributed
# (there's no dist_ prefix for bin_SCRIPTS here):
bin_SCRIPTS = bin/sweeten bin/unsweeten bin/neoteric-guile bin/sweet-guile
CLEANFILES += $(bin_SCRIPTS)

# These are the local scripts.
noinst_SCRIPTS = ./sweeten ./unsweeten ./neoteric-guile ./sweet-guile
CLEANFILES += $(noinst_SCRIPTS)



# my-subst substitutes info, esp. the library directory, which is different
# between the files in bin/ and the top-level directory.
# We could use autoconf's substitution engine, but that runs at *configure*
# time, which is the wrong time; per the GNU guidelines you're supposed to
# also be able to change the directory at the later *build* time.
# So we create a trivial script to do the substitutions, and run it
# at build time (e.g., "make" with no options).

# For now, we'll just generate the "sweeten" script with traditional
# Scheme.  Eventually probably want to put sweeten into the library.

# The repetition below is annoying.  Need to look up a better way to
# do this.  If it weren't for my-subst, we could just do "cp -p $< $@".


EXTRA_DIST += $(srcdir)/my-subst

bin/sweeten: src/sweeten_header src/sweeten.sscm ./unsweeten
	$(MKDIR_P) bin
	$(srcdir)/my-subst "$(readable_libdir)" \
	              < "$(srcdir)/src/sweeten_header"  > $@
	./unsweeten < "$(srcdir)/src/sweeten.sscm" >> $@
	chmod a+x $@

bin/unsweeten: src/unsweeten
	$(MKDIR_P) bin
	$(srcdir)/my-subst "$(readable_libdir)" < $<  > $@
	chmod a+x $@

bin/neoteric-guile: src/neoteric-guile
	$(MKDIR_P) bin
	$(srcdir)/my-subst "$(readable_libdir)" < $<  > $@
	chmod a+x $@

bin/sweet-guile: src/sweet-guile
	$(MKDIR_P) bin
	$(srcdir)/my-subst "$(readable_libdir)" < $<  > $@
	chmod a+x $@


./sweeten: src/sweeten_header src/sweeten.sscm ./unsweeten
	$(srcdir)/my-subst "" < "$(srcdir)/src/sweeten_header"  > $@
	./unsweeten < "$(srcdir)/src/sweeten.sscm" >> $@
	chmod a+x $@

# Make "unsweeten" depend on the underlying library.
# That way, once "./unsweeten" is created, we know that it's ready to use
# (because its dependencies have been installed); other rules can just
# depend on "./unsweeten", and they won't start until unsweeten is ready.
./unsweeten: src/unsweeten readable/sweet-impl.scm
	$(srcdir)/my-subst "" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./neoteric-guile: src/neoteric-guile
	$(srcdir)/my-subst "" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./sweet-guile: src/sweet-guile
	$(srcdir)/my-subst "" < $<  > $@
	chmod a+x $@
	chmod a-w $@

./sweeten.sscm: src/sweeten.sscm
	$(srcdir)/my-subst "" < $<  > $@
	chmod a+x $@
	chmod a-w $@

# Not directly used; useful if you want to generate JUST this file:
,sweeten.scm : src/sweeten.sscm $(UNSWEETEN)
	$(UNSWEETEN) $< > $@
	chmod a-w $@


EXTRA_DIST += \
 tests/all-test \
 tests/curly-infix-test \
 tests/neoteric-cl-test \
 tests/neoteric-scm-test \
 tests/neoteric-scm-test.scm \
 tests/sweet-test \
 tests/sweet-testsuite \
 tests/test-cases-neoteric


# TODO: Improve test harness & integration into overall system.
# check:
#	PATH="`pwd`:$(srcdir)/tests:${PATH}" ./all-test

