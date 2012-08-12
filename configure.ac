# configure.ac - configuration for "readable" project.
# For more information on the project, see: http://readable.sourceforge.net.
# For autotools info, see: http://www.dwheeler.com/autotools

# Initialize, but use more options.  Note parameters brackets and whitespace:
AC_INIT([readable], [0.4.1], [dwheeler@dwheeler.com],
        [readable], [http://readable.sourceforge.net/])
# Force autoconf to be more recent.
AC_PREREQ([2.67])
# Safety check - list a source file that wouldn't be in other projects:
AC_CONFIG_SRCDIR([src/kernel.scm])
# Put autotools auxiliary files in a subdir, so they don't clutter top dir:
AC_CONFIG_AUX_DIR([build-aux])

# Enable "automake" to simplify creating makefiles:
AM_INIT_AUTOMAKE([1.11 -Wall -Werror])
AC_CONFIG_FILES([Makefile])

# Look for the program "guile" and its site directory:
# (Don't use the guile.m4 macros, as they require guile-config,
# but we aren't linking into guile, and guile-config doesn't
# come standard on all "guile" packages on all distros)
AC_PATH_PROG(GUILE,guile)
AS_IF([test x"$GUILE" = x],[#then
  AC_MSG_ERROR([guile required but not found])
],[#else
  :
])
AC_SUBST(GUILE)
# Normally GUILE_SITE_DIR computes using guile-config, but
# we can query the guile interpreter itself directly anyway.
AC_MSG_CHECKING([for Guile site directory])
GUILE_SITE=`[$GUILE] -q -c "(display (%site-dir))"`
AC_MSG_RESULT($GUILE_SITE)
AC_SUBST(GUILE_SITE)
# The below is GUILE_MODULE_AVAILABLE code, but expanded and
# with AC_REQUIRE([GUILE_PROGS]) removed.
AC_MSG_CHECKING([if (ice-9 readline) is available])
[$GUILE] -q -c "(use-modules (ice-9 readline)) (exit ((lambda () 0)))" > /dev/null 2>&1
GUILE_READLINE_AVAILABLE=$?
AS_IF([test "$GUILE_READLINE_AVAILABLE" = "0"],[#then
  GUILE_READLINE_AVAILABLE=yes
],[#else
  GUILE_READLINE_AVAILABLE=no
])
AC_MSG_RESULT($GUILE_READLINE_AVAILABLE)
AC_SUBST([GUILE_READLINE_AVAILABLE])

# Perhaps check for: (more) programs, libraries, header files, types,
# structures, compiler characteristics, library functions, system services.

# Do final output.
AC_OUTPUT

