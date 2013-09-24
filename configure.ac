# configure.ac - configuration for "readable" project.
# For more information on the project, see: http://readable.sourceforge.net.
# For autotools info, see: http://www.dwheeler.com/autotools

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

# Version numbers generally follow the Semantic Versioning convention
# (MAJOR.MINOR.PATCH) as documented here: http://semver.org/
# Pre-release values DO NOT INCLUDE a hyphen,
# because hyphens are specifically forbidden by RPM-based tools
# (used by Red Hat Enterprise Linux, Fedora, CentOS, SuSE, etc.).

# Initialize, but use more options.  Note parameter brackets and whitespace.
# TO CHANGE THE VERSION NUMBER: Edit readable-simple.spec, readable.spec,
# readable.asd, and the line below:
AC_INIT([readable], [0.9.3], [dwheeler@dwheeler.com],
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

# Supporting shell functions
is_empty () {
  test "x$1" = "x"
}

is_enabled () { # Return TRUE if $1 has non-no value; empty considered enabled
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

# Create arguments --without-guile, --without-common-lisp, --without-clisp,
# --with-markdown-command=VALUE / --without-markdown-command
AC_ARG_WITH([guile],
  [AS_HELP_STRING([--without-guile], [disable support for guile])],
  [], [with_guile=yes])  # By default, with_guile
AC_ARG_WITH([scsh],
  [AS_HELP_STRING([--without-scsh], [disable support for scsh])],
  [], [with_scsh=yes])  # By default, with_scsh
AC_ARG_WITH([common-lisp],
  [AS_HELP_STRING([--without-common-lisp], [disable support for Common Lisp])],
  [], [with_common_lisp=yes])  # By default, with_common_lisp
AC_ARG_WITH([clisp],
  [AS_HELP_STRING([--without-clisp], [disable support for clisp])],
  [], [with_clisp="$with_common_lisp"])
# We generate HTML files from markdown files. By default we do this
# using the markdown2.py file (MIT license), copied on 2013-09-17 from:
#   https://raw.github.com/trentm/python-markdown2/master/lib/markdown2.py
# We have our own copy in our directory so we can be certain to have one.
AC_ARG_WITH([markdown-command],
  [AS_HELP_STRING(
    [--with-markdown-command=COMMAND | --without-markdown-command],
    ["with" option gives command to translate markdown into HTML; "without" option disables generating HTML from markdown])],
  [],
  [with_markdown_command="python ./markdown2.py -x link-patterns --link-patterns-file markdown-urls"])

AC_ARG_ENABLE([reported-guile-site],
  [AS_HELP_STRING([--enable-reported-guile-site], [force use of the guile site value as reported by guile])],
  [reported_guile_site=${enableval}], [reported_guile_site=no])

# Special environment variables/command-line variables
AC_ARG_VAR([GUILE], [Path to guile executable])
AC_ARG_VAR([GUILE_SITE],
           [Force Guile site directory, usually /usr/share/guile/site])
AC_ARG_VAR([GUILE_READLINE_AVAILABLE],
           [Force readline to be used or avoided in guile])

# Sanity-check argument values.
AS_IF([yes_or_no "$with_guile"],[], [
  AC_MSG_ERROR([with_guile must be 'yes' or 'no'])
])
AS_IF([yes_or_no "$with_scsh"],[], [
  AC_MSG_ERROR([with_scsh must be 'yes' or 'no'])
])
AS_IF([yes_or_no "$with_common_lisp"],[], [
  AC_MSG_ERROR([with_common_lisp must be 'yes' or 'no'])
])
AS_IF([yes_or_no "$with_clisp"],[], [
  AC_MSG_ERROR([with_clisp must be 'yes' or 'no'])
])
AS_IF([yes_or_no "$reported_guile_site"],[], [
  AC_MSG_ERROR([reported_guile_site must be 'yes' or 'no'])
])

# Sanity-check combinations of argument values.
AS_IF([test "$with_guile" = no -a "$with_common_lisp" = no],[
  AC_MSG_ERROR([Both --without-guile and --without-common-lisp specified - nothing to install])
])
AS_IF([test "$with_common_lisp" = no -a "$with_clisp" = yes],[
  AC_MSG_ERROR([clisp requires common lisp])
])


# Look for Guile and guile-related information if necessary.
AS_IF([is_enabled "$with_guile"],[

  # Guile provides autoconf macros in guile.m4, but they require guile-config.
  # Sadly, guile-config doesn't come standard on all "guile" packages on all
  # distros, so using guile-config would make our package harder to install.
  # Instead, we implement these searches directly, making our package
  # much easier to install.
  # Since we aren't linking into guile, this isn't hard to do.

  # Look for the program "guile":
  AS_IF([is_empty "$GUILE"],[#then
    AC_PATH_PROG([GUILE],[guile])])
  AS_IF([is_empty "$GUILE"],[#then
    AC_MSG_ERROR([guile required but not found (--without-guile to disable most tools)])])
  AC_SUBST([GUILE])

  # Determine Guile's "site" directory, where libraries are stored.
  AS_IF([is_empty "$GUILE_SITE"], [
    # Guile's autoconf macro "GUILE_SITE_DIR" sets GUILE_SITE to the value of
    # the site directory, but as noted above, users often don't have
    # the guile autoconf macros installed.  In addition,
    # GUILE_SITE_DIR doesn't honor the $prefix value anyway, which causes
    # "make distcheck" to always fail, making GUILE_SITE_DIR problematic:
    #   http://lists.gnu.org/archive/html/guile-user/2009-01/msg00049.html
    #   http://lists.gnu.org/archive/html/guile-user/2009-01/msg00048.html

    # Query the guile interpreter directly to find guile site directory
    AC_MSG_CHECKING([for reported Guile site directory])
    REPORTED_GUILE_SITE=`[$GUILE] -q -c "(display (%site-dir))"`
    AC_MSG_RESULT([$REPORTED_GUILE_SITE])

    # If prefix ends in "_inst" (as happens in "make distcheck"),
    # use the reported guile site value only if forced.
    # In most cases we use the guile site directory value reported by guile.
    AS_CASE([$prefix],
            [*/_inst], [use_reported_site="$reported_guile_site"],
            [*],       [use_reported_site='yes'])

    AS_IF([is_empty "$REPORTED_GUILE_SITE"],[
      use_reported_site='no' # Don't use a useless value!
      AC_MSG_WARN([guile reported an empty GUILE_SITE value, using calculated value to compensate])])

    AS_IF([test "$use_reported_site" = 'yes'], [
      # Use guile site value as reported by guile
      GUILE_SITE="$REPORTED_GUILE_SITE"
      # Add space at the end to line up with previous line
      AC_MSG_CHECKING([using same Guile site directory  ])
      AC_MSG_RESULT([$GUILE_SITE])
    ], [
      # Use, for GUILE_SITE, a calculated value based on $prefix.
      # This is necesary for "make distcheck" to work, for example.
      GUILE_SITE='${datarootdir}/guile/site'
      AC_MSG_CHECKING([for calculated Guile site directory])
      AC_MSG_RESULT([$GUILE_SITE])
    ])
  ])
  AC_SUBST([GUILE_SITE])

  # Determine if "readline" is available in guile.
  # Below is code similar to GUILE_MODULE_AVAILABLE, but expanded and
  # with AC_REQUIRE([GUILE_PROGS]) removed:
  AC_MSG_CHECKING([if (ice-9 readline) is available])
  AS_IF([is_empty "$GUILE_READLINE_AVAILABLE"], [
    [$GUILE] -q -c "(use-modules (ice-9 readline)) (exit ((lambda () 0)))" > /dev/null 2>&1
    GUILE_READLINE_AVAILABLE=$?
    AS_IF([test "$GUILE_READLINE_AVAILABLE" = "0"],[
      GUILE_READLINE_AVAILABLE=yes
    ],[
      GUILE_READLINE_AVAILABLE=no
    ])
  ])
  AC_MSG_RESULT([$GUILE_READLINE_AVAILABLE])
  AS_IF([yes_or_no "$GUILE_READLINE_AVAILABLE"],[], [
    AC_MSG_ERROR([GUILE_READLINE_AVAILABLE must be 'yes' or 'no'])
  ])
  AC_SUBST([GUILE_READLINE_AVAILABLE])
])


AS_IF([is_enabled "$with_common_lisp"], [
  # We'll use LN_S to install Common Lisp ASDF files:
  AC_PROG_LN_S
])


AS_IF([is_enabled "$with_clisp"], [
  # Determine if "clisp" is available and has asdf properly installed.
  AC_PATH_PROG([CLISP_INSTALLED], [clisp])
  AS_IF([is_empty "$CLISP_INSTALLED"],[#then
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

# Fail if we need python but it's not available.
AS_CASE([$with_markdown_command], [python\ *], [
  AC_PATH_PROG([PYTHON_INSTALLED], [python])
  AS_IF([is_empty "$PYTHON_INSTALLED"],[#then
    AC_MSG_ERROR([python not found; install it, use --with-markdown-command=COMMAND, or use --without-markdown-command])
  ])])

# Set up automake conditionals
AM_CONDITIONAL([WITH_GUILE],       [is_enabled "$with_guile"])
AM_CONDITIONAL([WITH_SCSH],        [is_enabled "$with_scsh"])
AM_CONDITIONAL([WITH_COMMON_LISP], [is_enabled "$with_common_lisp"])
AM_CONDITIONAL([WITH_CLISP],       [is_enabled "$with_clisp"])
AM_CONDITIONAL([ENABLE_HTML_FROM_MARKDOWN],
                                   [is_enabled "$with_markdown_command"])
AC_SUBST([MARKDOWN], [$with_markdown_command])


# Perhaps check for: (more) programs, libraries, header files, types,
# structures, compiler characteristics, library functions, system services.

# Do final output.
AC_OUTPUT

