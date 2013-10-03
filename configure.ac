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


# This "configure" script tries to "automagically" configure the software
# for the user's system.
# In general we will try to generate a "configure" that will
# "work with what we have" but warn if major components aren't available.
# We'll check for file existence for convenience (most users will configure
# for their OWN system).  We'll also provide override mechanisms to skip that,
# to support generation for other systems (cross-"compiling").


# Version numbers generally follow the Semantic Versioning convention
# (MAJOR.MINOR.PATCH) as documented here: http://semver.org/
# Pre-release values DO NOT INCLUDE a hyphen,
# because hyphens are specifically forbidden by RPM-based tools
# (used by Red Hat Enterprise Linux, Fedora, CentOS, SuSE, etc.).

# Initialize, but use more options.  Note parameter brackets and whitespace.
# TO CHANGE THE VERSION NUMBER: Edit readable-simple.spec, readable.spec,
# readable.asd, and the line below:
AC_INIT([readable], [0.9.4], [dwheeler@dwheeler.com],
        [readable], [http://readable.sourceforge.net/])
AC_COPYRIGHT(
[[Copyright (C) 2007-2013 by David A. Wheeler and Alan Manuel K. Gloria.
This software is Free/libre/open source software and is
released under the "MIT" license.]])

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

is_permitted () { # Return TRUE if $1 has non-no value; empty = permitted
  test "x$1" != "xno"
}

yes_no_empty () { # Returns TRUE if $1 is "yes", "no", or empty.
  if test "x$1" = "xyes" ; then
    true
  elif test "x$1" = "xno" ; then
    true
  elif test "x$1" = "x" ; then
    true
  else
    false
  fi
}

# We generate HTML files from markdown files. By default we do this
# using the markdown2.py file (MIT license), copied from:
#   https://raw.github.com/trentm/python-markdown2/master/lib/markdown2.py
# We have our own copy in our directory so we can be certain to have one.
# Users can use MARKDOWN=... or --markdown=... to use their own.
default_markdown_command="python ./markdown2.py -x link-patterns --link-patterns-file markdown-urls"


# Create arguments for configure:
AC_ARG_WITH([guile],
  [AS_HELP_STRING([[--with-guile[=COMMAND] / --without-guile]],
    [require/disable guile; many tools use guile (default: auto-detect)])])
AC_ARG_ENABLE([guile-readline],
  [AS_HELP_STRING([--enable-guile-readline / --disable-guile-readline],
    [require/disable readline support within guile (default: auto-detect)])])
AC_ARG_WITH([scsh],
  [AS_HELP_STRING([[--with-scsh[=COMMAND] / --without-scsh]],
    [require/disable support for scsh (default: auto-detect)])])

AC_ARG_WITH([common-lisp],
  [AS_HELP_STRING([[--with-common-lisp[=DIR] / --without-common-lisp]],
    [[require/disable support for Common Lisp (DIR is library directory)]])])
AC_ARG_WITH([clisp],
  [AS_HELP_STRING([[--with-clisp[=COMMAND] / --without-clisp]],
  [require/disable support for clisp])])

AC_ARG_WITH([markdown],
  [AS_HELP_STRING(
    [[--with-markdown[=COMMAND] | --without-markdown]],
    [require/forbid translating markdown into HTML (with COMMAND if given)])])

# Special environment variables/command-line variables
AC_ARG_VAR([BIN_ENV],
  [Command to run env (default: auto-detect, often /usr/bin/env)])
AC_ARG_VAR([GUILE], [Command to run guile (default: auto-detect)])
AC_ARG_VAR([GUILE_SITE],
  [Guile site directory (default: auto-detect; often /usr/share/guile/site)])
AC_ARG_VAR([SCSH],
  [Command to run scsh (default: auto-detect; depends on guile)])
AC_ARG_VAR([COMMON_LISP_LIB_DIR],
  [Common Lisp library directory (default: $(datadir)/common-lisp)])
AC_ARG_VAR([CLISP], [Command to run clisp (default: auto-detect)])
AC_ARG_VAR([EXPECT], [Command to run expect (default: auto-detect)])
AC_ARG_VAR([MARKDOWN],
  [Command to convert markdown to html (the default uses python)])


# Interpret argument values not yes/no/empty as setting relevant variables
AS_IF([yes_no_empty "$with_guile"],[], [
  GUILE="$with_guile"
  with_guile=yes
])
AS_IF([yes_no_empty "$with_scsh"],[], [
  SCSH="$with_scsh"
  with_scsh=yes
])
AS_IF([yes_no_empty "$with_common_lisp"],[], [
  COMMON_LISP_LIB_DIR="$with_common_lisp"
  with_common_lisp=yes
])
AS_IF([yes_no_empty "$with_clisp"],[], [
  CLISP="$with_clisp"
  with_clisp=yes
])
AS_IF([yes_no_empty "$with_markdown"],[], [
  MARKDOWN="$with_markdown"
  with_markdown=yes
])

# Sanity-check argument values that MUST be yes/no/empty
AS_IF([yes_no_empty "$enable_guile_readline"],[], [
  AC_MSG_ERROR([enable-guile-readline must be 'yes', 'no', or no value])
])


# Sanity-check combinations of argument values.
AS_IF([test "$with_guile" = no && test "$with_common_lisp" = no],[
  AC_MSG_WARN([Both --without-guile and --without-common-lisp specified - nothing to install])
])
AS_IF([test "$with_common_lisp" = no && test "$with_clisp" = yes],[
  AC_MSG_ERROR([clisp requires common lisp])
])



# Setup basics
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_SED

AC_PATH_PROG([BIN_ENV], [env])
AS_IF([is_empty "$BIN_ENV"], [
  AC_MSG_ERROR([No executable "env" available.])])


### GUILE ###

# For purposes of configuration, do we have a working guile?
have_guile=no
AS_IF([is_permitted "$with_guile"],[
  echo >&2
  AC_PATH_PROG([GUILE],[guile])
  AS_IF([is_empty "$GUILE"], [], [have_guile=yes])
])
AM_CONDITIONAL([HAVE_GUILE], [is_enabled "$have_guile"])

# If we have guile, configure it.
GUILE_READLINE_AVAILABLE=no
have_scsh=no
AM_COND_IF([HAVE_GUILE], [
  # Guile provides autoconf macros in guile.m4, but they require guile-config.
  # Sadly, guile-config doesn't come standard on all "guile" packages on all
  # distros, so using guile-config would make our package harder to install.
  # Instead, we implement these searches directly, making our package
  # much easier to install.
  # Since we aren't linking into guile, this isn't hard to do.

  # Determine Guile's "site" directory, where libraries are stored.
  # Guile's autoconf macro "GUILE_SITE_DIR" sets GUILE_SITE to the value of
  # the site directory, but as noted above, users often don't have
  # the guile autoconf macros installed.  In addition,
  # GUILE_SITE_DIR doesn't honor the $prefix value anyway, which causes
  # "make distcheck" to always fail, making GUILE_SITE_DIR problematic:
  #   http://lists.gnu.org/archive/html/guile-user/2009-01/msg00049.html
  #   http://lists.gnu.org/archive/html/guile-user/2009-01/msg00048.html
  # We use:
  # 1. GUILE_SITE if not empty.  Otherwise,
  # 2. If prefix doesn't end in "/_inst" (a "make distcheck"), and
  #    guile reports a non-empty value, use that value.  Otherwise,
  # 3. Use the value '${datarootdir}/guile/site'
  AS_IF([is_empty "$GUILE_SITE"], [
    AS_CASE([$prefix],
      [*/_inst], [
        AC_MSG_NOTICE([[Prefix ends in /_inst, so this is a 'make distcheck'. GUILE_SITE is unset.]])],
      [*], [
        # Query the guile interpreter directly to find guile site directory
        AC_MSG_CHECKING([asking guile for its site directory])
        GUILE_SITE=`[$GUILE] -q -c "(display (%site-dir))"`
        AC_MSG_RESULT([$GUILE_SITE])
    ])
  ])
  AS_IF([is_empty "$GUILE_SITE"], [
    AC_MSG_NOTICE([[Setting GUILE_SITE to start with datarootdir (which is based on prefix)]])
    GUILE_SITE='${datarootdir}/guile/site'
  ])
  AC_MSG_CHECKING([[Guile site directory (GUILE_SITE)  ]])
  AC_MSG_RESULT([$GUILE_SITE])

  # Determine if "readline" is available in guile.
  AC_MSG_CHECKING([if (ice-9 readline) is available])
  AS_IF([is_permitted "$enable_guile_readline"],[
    [$GUILE] -q \
      -c "(use-modules (ice-9 readline)) (exit ((lambda () 0)))" \
      > /dev/null 2>&1
    GUILE_READLINE_AVAILABLE=$?
    AS_IF([test "$GUILE_READLINE_AVAILABLE" = "0"],[
      GUILE_READLINE_AVAILABLE=yes
    ],[
      GUILE_READLINE_AVAILABLE=no
    ])
  ])
  AC_MSG_RESULT([$GUILE_READLINE_AVAILABLE])
  AS_IF([test "$GUILE_READLINE_AVAILABLE" = no &&
         test "x$enable_guile_readline" = "xyes"], [
    AC_MSG_ERROR([Guile readline required but unavailable.])
  ])

  AS_IF([is_permitted "$with_scsh"],[
    AC_PATH_PROG([SCSH], [scsh])
    AS_IF([is_empty "$SCSH"], [], [have_scsh=yes])
  ])
], [
  # No guile.  Is that a complete failure?
  AS_IF([test "x$with_guile" = "xyes"], [
    AC_MSG_ERROR([Guile required but unavailable.])])
])
AC_SUBST([GUILE_READLINE_AVAILABLE])
AM_CONDITIONAL([HAVE_SCSH], [is_enabled "$have_scsh"])

AS_IF([test "$have_scsh" = no &&
       test "x$enable_scsh" = "xyes"], [
  AC_MSG_ERROR([scsh required but unavailable.])
])



### COMMON LISP ###

# Common Lisp by itself just installs to a directory, so we'll
# always do it if permitted to do so:
AS_IF([is_permitted "$with_common_lisp"], [
  echo >&2
  with_common_lisp=yes

  AC_MSG_CHECKING([Common Lisp lib dir (COMMON_LISP_LIB_DIR)])
  AS_IF([is_empty "$COMMON_LISP_LIB_DIR"], [
    COMMON_LISP_LIB_DIR='$(datadir)/common-lisp'
  ])
  AC_MSG_RESULT([$COMMON_LISP_LIB_DIR])

  # We'll use LN_S to install Common Lisp ASDF files:
  AC_PROG_LN_S
])
AM_CONDITIONAL([HAVE_COMMON_LISP], [is_enabled "$with_common_lisp"])

# If Common Lisp won't be installed, clisp possibilities are limited.
AS_IF([test "$with_common_lisp" = no], [
  AS_IF(
    [is_empty "$with_clisp"],
      [with_clisp=no],
    [test "$with_clisp" = yes],
      [AC_MSG_ERROR([Common Lisp forbidden yet Clisp required])])
])

have_clisp=no
have_clisp_asdf=no
AS_IF([is_permitted "$with_clisp"], [
  AC_PATH_PROG([CLISP], [clisp])
  AS_IF([is_empty "$CLISP"], [
    AS_IF([test "with_clisp" = yes], [
      AC_MSG_ERROR([clisp required but not found])])
  ], [
    have_clisp=yes
    # check if asdf is installed with clisp.
    AC_MSG_CHECKING([if (require "asdf") is available in clisp])
    echo '(require "asdf")' > asdf-check.lisp
    [$CLISP] asdf-check.lisp > /dev/null 2>&1
    AS_IF([test "$?" = 0],[
      have_clisp_asdf=yes
      AC_MSG_RESULT([$have_clisp_asdf])
    ],[
      AC_MSG_RESULT([$have_clisp_asdf])
      AC_MSG_WARN([clisp found but asdf does not work, so clisp disabled])
      AC_MSG_NOTICE([Consider running 'make clisp-asdf' to configure asdf for this user.])
      AC_MSG_NOTICE([After running 'make clisp-asdf' you must re-run './configure'.])
    ])
    rm -f asdf-check.lisp
  ])
])

AM_CONDITIONAL([HAVE_CLISP],       [is_enabled "$have_clisp_asdf"])


### Other stuff ###
echo >&2

AS_IF([is_enabled "$have_guile" || is_enabled "$have_clisp_asdf"], [
  AC_PATH_PROG([EXPECT], [expect])
  AS_IF([is_empty "$EXPECT"],[
    echo >&2
    AC_MSG_WARN([[expect not found; please install expect, or many command-line tools will fail]])
    echo >&2
    # Set EXPECT to a plausible value, in case it's installed later
    # or a later PATH makes "expect" visible.
    # An empty value is *certain* to fail later.
    EXPECT="$BIN_ENV expect"
  ])
])

# If users have not specified their own markdown processor,
# we'll try to use the pre-supplied markdown2 implementation.
# In that case, we'll look for python (which is required for markdown2).
# Markdown2 only requires a very old version of Python and it works with 3.
# Developers will need a more modern version of Python to use
# get-from-wiki, but get-from-wiki is a developer convenience
# (not a user command) so we won't worry about that here.
have_markdown=no
AS_IF([is_permitted "$with_markdown"], [
  AS_IF([is_empty "$MARKDOWN"], [
    MARKDOWN="$default_markdown_command"
    AM_PATH_PYTHON([2.4],
      [have_markdown=yes], [
      AC_MSG_WARN([[Python not found, and markdown2 requires it. HTML generation disabled]])])
  ], [
    have_markdown=yes
  ])
])
AS_IF([test "$with_markdown" = yes && test "$have_markdown" = no], [
  AC_MSG_ERROR([[Cannot generate HTML, yet markdown processing required.]])])
AM_CONDITIONAL([HAVE_MARKDOWN], [is_enabled "$have_markdown"])


# Emit a separate warning LAST, so users are more likely to notice.
AM_COND_IF([HAVE_GUILE], [], [
  echo >&2
  AC_MSG_WARN([GUILE NOT AVAILABLE.  Tools implemented with guile will not be available.])
  echo >&2
])

# Perhaps check for: (more) programs, libraries, header files, types,
# structures, compiler characteristics, library functions, system services.

# Do final output.
AC_OUTPUT

