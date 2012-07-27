# configure.ac - configuration for "readable" project.
# For more information on the project, see: http://readable.sourceforge.net.
# For autotools info, see: http://www.dwheeler.com/autotools

# Initialize, but use more options.  Note parameters brackets and whitespace:
AC_INIT([readable], [0.30], [dwheeler@dwheeler.com],
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
GUILE_PROGS
GUILE_SITE_DIR
GUILE_MODULE_AVAILABLE([GUILE_READLINE_AVAILABLE], [(ice-9 readline)])
AC_SUBST([GUILE_READLINE_AVAILABLE])

# Perhaps check for: (more) programs, libraries, header files, types,
# structures, compiler characteristics, library functions, system services.

# Do final output.
AC_OUTPUT

