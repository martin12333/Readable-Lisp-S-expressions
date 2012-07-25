# Makefile.am for "readable" project

ACLOCAL_AMFLAGS = -I m4 --install
EXTRA_DIST = m4/NOTES

UNSWEETEN = ./unsweeten
.sscm.scm:
	$(UNSWEETEN) $< > $@

sweeten.scm : sweeten.sscm

# lib_LTLIBRARIES = libwhine-1.0.la
# libwhine_1_0_la_SOURCES = src/whine.c src/whine.h
# libwhine_1_0_la_LDFLAGS = -version-info 0:0:0
# include_HEADERS = src/whine.h

# bin_PROGRAMS = hello
# hello_SOURCES = src/hello.c
# hello_LDADD = $(lib_LTLIBRARIES)

readable_libdir = $(GUILE_SITE)/readable

readable_lib_DATA = readable/sweet-impl.scm

