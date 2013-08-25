# configure.ac - configuration for "readable" project.
# For more information on the project, see: http://readable.sourceforge.net.
# For autotools info, see: http://www.dwheeler.com/autotools

# Version numbers generally follow the Semantic Versioning convention
# (MAJOR.MINOR.PATCH) as documented here: http://semver.org/
# There's a key exception: pre-release values DO NOT INCLUDE a hyphen,
# because hyphens are specifically forbidden by RPM-based tools
# (used by Red Hat Enterprise Linux, Fedora, CentOS, SuSE, etc.).

# Initialize, but use more options.  Note parameter brackets and whitespace.
# TO CHANGE THE VERSION NUMBER: Edit readable-simple.spec, readable.spec,
# readable.asd, and the line below:
AC_INIT([readable], [0.9.2develop], [dwheeler@dwheeler.com],
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

# Create arguments --without-guile and --without-clisp
AC_ARG_WITH([guile],
  [AS_HELP_STRING([--without-guile], [disable support for guile])],
  [], [with_guile=yes])  # By default, with_guile.
AC_ARG_WITH([clisp],
  [AS_HELP_STRING([--without-clisp], [disable support for clisp])],
  [], [with_clisp=yes])  # By default, with_clisp.


# Sanity-check arguments.
is_enabled () {
  test "x$1" != "xno"
}
yes_or_no () { # Returns TRUE if $1 is "yes" or "no".
  if test "x$1" = "xyes" ; then
    true
  elif test "x$1" = "xno" ; then
    true
  else
    false
  fi
}
AS_IF([! yes_or_no "$with_guile"],[
  AC_MSG_ERROR([with_guile must be 'yes' or 'no'])
])
AS_IF([! yes_or_no "$with_clisp"],[
  AC_MSG_ERROR([with_clisp must be 'yes' or 'no'])
])
AS_IF([test "$with_guile" = no -a "$with_clisp" = no],[
  AC_MSG_ERROR([Both --without-guile and --without-clisp specified - nothing to install])
])

# Look for Guile if necessary.
AS_IF([is_enabled "$with_guile"],[
  # Look for the program "guile" and its site directory:
  # (Don't use the guile.m4 macros, as they require guile-config,
  # but we aren't linking into guile, and guile-config doesn't
  # come standard on all "guile" packages on all distros)
  AC_PATH_PROG(GUILE,guile)
  AS_IF([test x"$GUILE" = x],[#then
    AC_MSG_ERROR([guile required but not found (--without-guile to disable most tools)])
  ],[#else
    :
  ])
  AC_SUBST(GUILE)

  # Normally GUILE_SITE_DIR computes using guile-config, but
  # we can query the guile interpreter itself directly anyway.
  AC_MSG_CHECKING([for Guile site directory])
  GUILE_SITE=`[$GUILE] -q -c "(display (%site-dir))"`
  # Patch GUILE_SITE so that $prefix and $datarootdir are honored;
  # this is necessary for "make distcheck" to work properly.
  # The funny assignment format is because sharp is also the m4 comment char.
  AS_CASE([$GUILE_SITE], [/usr/share/*],
    [GUILE_SITE="\$(datarootdir)${GUILE_SITE#/usr/share}"
    ])
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
])

AS_IF([is_enabled "$with_clisp"], [
  # Determine if "clisp" is available and has asdf properly installed.
  AC_PATH_PROG([CLISP_INSTALLED], [clisp])
  AS_IF([test x"$CLISP_INSTALLED" = x],[#then
    AC_MSG_ERROR([clisp required but not found (--without-clisp to skip clisp+asdf checking)])
  ],[#else
    # check if asdf is installed with clisp.
    AC_MSG_CHECKING([if (require :asdf) is available])
    echo '(require "asdf")' > asdf-check.lisp
    [$CLISP_INSTALLED] asdf-check.lisp > /dev/null 2>&1
    AS_IF([test "$?" = 0],[#then
      rm asdf-check.lisp
      AC_MSG_RESULT([yes])
      CLISP=$CLISP_INSTALLED
    ],[#else
      rm asdf-check.lisp
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([clisp found but without asdf (--without-clisp to skip clisp+asdf checking)])
    ])
  ])
  AC_SUBST([CLISP])
])

# Set up automake conditionals
AM_CONDITIONAL([WITH_GUILE], [is_enabled "$with_guile"])
AM_CONDITIONAL([WITH_CLISP], [is_enabled "$with_clisp"])


# Perhaps check for: (more) programs, libraries, header files, types,
# structures, compiler characteristics, library functions, system services.

# We'll use LN_S to install Common Lisp ASDF files:
AC_PROG_LN_S

# Do final output.
AC_OUTPUT

