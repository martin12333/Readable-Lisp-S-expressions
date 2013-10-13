If your system has "readable" pre-packaged, it's easiest to use that.  But if that's not the case, here's how to install the "readable" software.

To install the "readable" software directly from our site, you need to have the right prerequisites, download and uncompress the software, configure it, build it, and then actually install it.  In most cases it should be easy, but we've provided some tips to help you avoid (or fix) problems.

For the impatient: Source installs
==================================

If you're completely impatient, and you want to install from source, just install the packages *guile*, *expect*, *clisp*, and *asdf* (asdf is often the package "cl-asdf").  You might also install *sbcl*.  Then download the current stable version from [http://readable.sourceforge.net](http://readable.sourceforge.net) and run the following command-line commands on your Unix-like system (including Cygwin):

    tar xvzf readable-*.tar.gz
    cd readable-*
    ./configure --prefix=/usr
    make
    sudo make install

Below are more details about each of those steps, including various options you can use.  You don't need to install to system-wide locations (so you *could* skip the last step), though it's slightly easier to use if you do.

The "readable" software supports both Scheme (guile) and Common Lisp.  If you want, you can choose to install just the Scheme (guile) or just the Common Lisp portion.  In particular, if you don't already have an implementation of Common Lisp installed, you might try using the configuration option "--without-common-lisp"; setting up a usable Common Lisp system can take a few more steps as described below.


For the impatient: QuickLisp Common Lisp library install
========================================================

If you just want to *use* the "readable" library from Common Lisp code, QuickLisp is the easy solution.  This approach will not install tools such as "sweeten" or "unsweeten", but for a lot of Common Lisp users that may be okay.

First, install *[ASDF](http://common-lisp.net/project/asdf/)* (clisp users see below) and *[QuickLisp](http://www.quicklisp.org/)*.  A QuickLisp install should simply involve starting your implementation of Common Lisp, downloading QuickLisp, then running *(load "quicklisp.lisp")* and then *(quicklisp-quickstart:install)*.  Then download and install readable as a library using:

    (ql:quickload "readable")

That's it!  That will download the current "master" version using git, and by default put it under "~/quicklisp".  

If these impatient approaches aren't enough, let's go through it step-by-step.


Prerequisites
=============

You need a Unix-like (POSIX) system such as a Linux distribution (such as Red Hat Enterprise Linux, CentOS, Fedora, Debian, Ubuntu, Gentoo, or Slackware), FreeBSD, NetBSD, OpenBSD, MacOS, etc.  On Windows, just install and use Cygwin; that works very well.  Any modern GNU/Linux system should do nicely.

The following packages are required or strongly recommended:

*   *[GNU Guile](http://www.gnu.org/software/guile/guile.html)* (a Scheme implementation).  This is *required* if you want to use Scheme, and we *recommend* all users install guile because some of our handy support tools require guile.  If you interact with guile it's more convenient to use a guile with built-in readline support (most everything except Cygwin has this support), but this is optional.  You can use the configure option "--without-guile" if you don't want any of the Guile materials.
*   *expect*.  This is *required* by several of the command-line interface tools, for both Scheme and Common Lisp.  It's not required for the underlying libraries (e.g., the Common Lisp library), though, so the installer will merely give you a warning if you don't have it.

If you're going to use Common Lisp, you also need:

*   *clisp*, *sbcl*, or some other implementation of Common Lisp.  Cygwin includes a clisp package, so if you use Cygwin, that's the easiest option.  The "readable" software should work with any implementation of Common Lisp; please let us know if there's a problem.
*   *[ASDF](http://common-lisp.net/project/asdf/)* (typically packaged with the name "cl-asdf").  You may need to configure ASDF, as discussed below.  As a convenience, we include a copy if you don't have your own copy.

In most cases, users of Linux distributions can just use their package managers (such as yum, apt-get, or a GUI front-end for them) to easily install the relevant packages and any dependencies.

Below are some additional tips about prerequisites and specific circumstances.

Prerequisites if you use the development version
------------------------------------------------

If you plan to use the development (not stable) version directly from our git repository, you'll also need:

*   *git* (to download the software).
*   *autoconf*. You need autoconf version 2.67 or later.
*   *automake*.  You should use version 1.14 or later; we haven't tested older versions.
*   *guile development libraries* (e.g., package "guile-devel" on Fedora and Cygwin).  These need to match the version of guile you're using.
*   *python* (to generate some HTML files from markdown).  You should have version 2.7+ or version 3+.  Version 2.4+ will do in a pinch (you need version 2.7 to auto-download markdown files, but only 2.4+ to generate HTML from them.)

Installing on Red Hat Enterprise Linux (RHEL), CentOS, and Scientific Linux
---------------------------------------------------------------------------

The standard repositories of [Red Hat Enterprise Linux (RHEL)](http://www.redhat.com/products/enterprise-linux/), [CentOS](http://www.centos.org/), and [Scientific Linux](http://www.scientificlinux.org/) do not, at the time of this writing, include many Lisp-related packages.  The easy solution is to add additional repositories with such packages to your list of repositories.  Here are three likely additional repositories specifically designed for RHEL and CentOS; see their sites for instructions on how to add them to your list of repositories:

*   [RepoForge](http://repoforge.org/), formerly known as RPMForge.
*   [Extra Packages for Enterprise Linux (EPEL)](http://fedoraproject.org/wiki/EPEL), a Fedora Special Interest Group that creates, maintains, and manages packages for Enterprise Linux.  EPEL packages are usually based on their Fedora counterparts and will never conflict with or replace packages in the base Enterprise Linux distributions.
*   [ATRPMS](http://atrpms.net/)

You should normally pick only *one* of these additional repositories; there are [reports that these additional repositories do not mix well](http://www.scientificlinuxforum.org/?showtopic=266).  Of the three, you might look at RepoForge first; many have had good success with its predecessor RPMForge.  But all three have their supporters and adherents.

The [CentOS list of additional repositories](http://wiki.centos.org/AdditionalResources/Repositories) may be helpful, too.

You can also download the source code to the various projects and build them yourself.  If you choose to download and build clisp from source code, you will probably need to first download and build [libsigsegv](http://www.gnu.org/software/libsigsegv/), since building clisp requires libsigsegv.


Installing ASDF (for Common Lisp)
---------------------------------

If you use the Common Lisp implementation you'll normally need Another System Definition Facility ([ASDF](http://common-lisp.net/project/asdf/)), which is usually included in your implementation of Common Lisp.  Many Linux distributions include ASDF; just install package "cl-asdf".  Fedora, Debian, and Ubuntu are all known to include ASDF as the package "cl-asdf", and others probably use the same name too.

As a convenience, a copy of ASDF is included with the distribution (in file *external/asdf.lisp*), so that you can get started without downloading and installing other libraries.  As described below, the install process will even help you install ASDF in a way that makes clisp happy.

You do not have to use the included copy of ASDF.  If you don't already have ASDF, you can download it and follow its installation instructions.  The ASDF main page is: [http://common-lisp.net/project/asdf/](http://common-lisp.net/project/asdf/)

You can use the Common Lisp implementation files without ASDF or the normal installation script, if you want to.  You can even embed the files into your own project.  This isn't particularly recommended, since this may make it harder for your users to update their readable software.   See the INSTALL file if you're interested in applying this do-it-yourself approach.


Configuring ASDF
----------------

Once your implementation of Common Lisp can find (and load) ASDF, you may need to configure ASDF so it can find the "readable" library (once the readable library is installed).  You must either install the "readable" library in a place where ASDF will find it using ASDF's current configuration, or modify the ASDF configuration so ASDF can find the "readable" library.

If you set the "prefix" value to "/usr" during configuration, as discussed below, you don't need to change the ASDF configuration.  ASDF normally looks there.

However, if you are going to install the software using a prefix other than "/usr", you might want to modify the ASDF system configuration file "/etc/common-lisp/source-registry.conf" so that it can find files in common locations.  Here are plausible contents of "/etc/common-lisp/source-registry.conf" that may serve as an example:

    (:source-registry
      (:tree (:home "common-lisp")) ;; expands to e.g., "$HOME/common-lisp/"
      (:tree (:home "cl")) ;; expands to e.g., "$HOME/cl/"
      (:tree "/usr/local/share/common-lisp/source/")
      (:tree "/usr/share/common-lisp/source/")
      :inherit-configuration)


Download and uncompress
=======================

Obviously, you'll need to download our "readable" software.

We recommend that you get the current "stable" version, unless you have some special need.  Just use a web browser to view http://readable.sourceforge.net - click on "download files" - and download the current version.  Then uncompress the download and cd into its directory.  E.g., at the command line:

     tar xvzf readable-*.tar.gz
     cd readable-*

If you *instead* want to get the *development* version instead of the stable version, do this instead:

     git clone git://git.code.sf.net/p/readable/code readable-code
     cd readable-code
     git checkout develop  # Switch to "develop" branch
     autoreconf -i


Configure
=========

As usual, configure.  To accept all the defaults, installing to the directory "/usr/local" (e.g., commands will be put in "/usr/local/bin"):

    ./configure

Many people will want to install to the directory "/usr" (e.g., so commands will be put in "/usr/bin"), especially if you're installing Common Lisp files.  To do this, instead run:

    ./configure --prefix=/usr

The configure program tries to automatically figure out what to do and then do it.  It supports many options, but most are only useful for special situations.  Here are some configure options you might want to use:

    --without-guile         disable support for guile (and tools that depend on guile)
    --without-clisp         disable support for clisp
    --without-common-lisp   disable support for Common Lisp (this disables clisp and sbcl)

So if you won't be using Common Lisp, consider using the configure option "--without-common-lisp".  Note that if you use --without-guile you'll disable several tools that depend on guile.

If you change your mind later about your configuration options, just re-run configure with your new configuration options, and then run the later steps as described below (in particular, "make" and "make install").

If you need more information (to configure it for unusual circumstances), run "./configure --help" or read the file "INSTALL".  The file "config.log" logs what configure does, which can help you if configuration doesn't work as expected.

If you have clisp, but clisp has not been configured to be able to invoke ASDF, the build will detect and report the issue as a warning.  If this happens, you may run this command (after running ./configure) to fix the problem:

    make clisp-asdf

The "make clisp-asdf" command modifies the current user's configuration by creating (if necessary) the directory $(HOME)/lisp/asdf, putting a file or symbolic link there, compiling, and re-running configure.  See the INSTALL file for details if you're curious.


Build
=====

Now build the code, using:

    make

You can optionally run the testsuite.  To run the tests you need /usr/bin/env (almost everyone has this).  The full testsuite requires guile and clisp, and clisp must be able to invoke ASDF.   Run the testsuite by typing:

    make check


Install
=======

You actually don't have to install it to system-wide locations to use it, if you just want to play with it.  But if you're using this software seriously, you should probably install it to "real" system-wide locations.  To install it, just type:

    make install

This will probably require privileges, so you may need to run "su" first or use sudo ("sudo make install") to get those privileges.  Thus, on many systems you'll really type:

    sudo make install

Some systems use "common-lisp-controller", which requires registration of Common Lisp files that are installed to system-wide locations.  Such systems include Debian, Ubuntu, Fedora, Gentoo, and other systems with the installable command register-common-lisp-source.  A "make install" will *automatically* register your Common Lisp files for you, but only if you do a final install to /usr (this means the prefix is /usr and DESTDIR is not set).  If you install the software to a nonstandard system-wide location (e.g., /usr/local), you may need to separately register it using this privileged command:

    register-common-lisp-source readable


Do I need to install it to system-wide locations?
=================================================

Again, no; you don't need to install to system-wide locations.  Once it's built, you can use it.

If you have installed it to system-wide locations, then in most cases the programs will automatically find the files when they need them, which makes things a little easier.


If you have not installed the files to system-wide locations, then you need to make sure the system can find commands when you type them.  You can tell the system where the commands are by prefixing the command name with the directory it's in, e.g., prefix the command with "./" if the command is in the current directory.  Another approach is to modify your PATH. On a Bourne-like shell (the usual kind), if the current directory contains some of the programs you'd like to be able to run without prefixing them with "./", you can modify your PATH using:

    export PATH="$(pwd)/$PATH"


Tutorials
=========

Now you can use it!

If you've not used it before, take look one of our two tutorials:

* [Scheme-tutorial] - Scheme-focused tutorial
* [Common-lisp-tutorial] - Common-Lisp-focused tutorial

