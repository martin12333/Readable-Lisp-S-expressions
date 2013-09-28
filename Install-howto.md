To install the "readable" software directly from our site, you need to have the right prerequisites, download and uncompress the software, configure it, build it, and then actually install it.  In most cases it should be easy, but we've provided some tips to help you avoid (or fix) problems.

If you're completely impatient,  just install the packages guile, expect, clisp, and asdf (asdf is often the package "cl-asdf").  Then download the current stable version from http://readable.sourceforge.net and run the following command-line commands on your Unix-like system (including Cygwin):

    tar xvzf readable-*.tar.gz
    cd readable-*
    ./configure --prefix=/usr
    make
    sudo make install
    sudo register-common-lisp-source readable

Below are more details about each of those steps, including various options you can use.  You don't need to install to system-wide locations (so you *could* skip the last two steps), though it's easier to use if you do.

The "readable" software supports both Scheme (guile) and Common Lisp.  If you want, you can choose to install just the Scheme (guile) or just the Common Lisp portion.  In particular, if you don't already have an implementation of Common Lisp installed, you might try using the configuration option "--without-common-lisp"; setting up a usable Common Lisp system typically takes a few more steps as described below.


Prerequisites
=============

You need a Unix-like (POSIX) system such as a Linux distribution, *BSD, MacOS, etc.  On Windows, just install and use Cygwin; that works very well.  Any modern GNU/Linux system should do nicely.

The following packages are required or strongly recommended:

*   *[GNU Guile](http://www.gnu.org/software/guile/guile.html)* (a Scheme implementation).  This is *required* if you want to use Scheme, and we *recommend* all users install guile because some of our handy support tools require guile.  If you interact with guile it's more convenient to use a guile with built-in readline support (most everything except Cygwin has this support), but this is optional.  You can use the configure option "--without-guile" if you don't want any of the Guile materials.
*   *expect*.  This is *required* by several of the command-line interface tools, for both Scheme and Common Lisp.  It's not required for the underlying libraries (e.g., the Common Lisp library), though, so the installer will merely give you a warning if you don't have it.

If you're going to use Common Lisp, you also need:

*   *clisp*, *sbcl*, or some other implementation of Common Lisp.   You need clisp if you want to run our self-test suite.  Cygwin includes a clisp package, so if you use Cygwin, that's the easiest option.  The "readable" software should work with any implementation of Common Lisp; please let us know if there's a problem.
*   *[ASDF](http://common-lisp.net/project/asdf/)* (typically packaged with the name "cl-asdf").  You may need to configure ASDF, as discussed below.

In most cases, users of Linux distributions can just use their package managers (such as yum, apt-get, or a GUI front-end for them) to easily install the relevant packages and any dependencies.

Below are some additional tips about prerequisites for the development version, RHEL and CentOS, ASDF, using ASDF in clisp, and weird systems without /usr/bin/env.

Prerequisites if you use the development version
------------------------------------------------

If you plan to use the development (not stable) version directly from our git repository, you'll also need:

*   *git* (to download the software).
*   *autoconf*. You need autoconf version 2.67 or later.
*   *automake*.  You should use version 1.14 or later; we haven't tested older versions.
*   *guile development libraries* (e.g., package "guile-devel" on Fedora and Cygwin).  These need to match the version of guile you're using.
*   *python* (to generate some HTML files from markdown).  You need version 2.7+ or version 3+.

Red Hat Enterprise Linux (RHEL), CentOS, and Scientific Linux
-------------------------------------------------------------

The standard repositories of [Red Hat Enterprise Linux (RHEL)](http://www.redhat.com/products/enterprise-linux/), [CentOS](http://www.centos.org/), and [Scientific Linux](https://www.scientificlinux.org/) do not, at the time of this writing, include many Lisp-related packages.  The easy solution is to add additional repositories with such packages to your list of repositories.  Here are three likely additional repositories specifically designed for RHEL and CentOS; see their sites for instructions on how to add them to your list of repositories:

*   [RepoForge](http://repoforge.org/), formerly known as RPMForge.
*   [Extra Packages for Enterprise Linux (EPEL)](http://fedoraproject.org/wiki/EPEL), a Fedora Special Interest Group that creates, maintains, and manages packages for Enterprise Linux.  EPEL packages are usually based on their Fedora counterparts and will never conflict with or replace packages in the base Enterprise Linux distributions.
*   [ATRPMS](http://atrpms.net/)

You should normally pick only *one* of these additional repositories; there are [reports that these additional repositories do not mix well](http://www.scientificlinuxforum.org/?showtopic=266).  Of the three, you might look at RepoForge first; many have had good success with its predecessor RPMForge.  But all three have their supporters and adherents.

The [CentOS list of additional repositories](http://wiki.centos.org/AdditionalResources/Repositories) may be helpful, too.

You can also download the source code to the various projects and build them yourself.  If you choose to download and build clisp from source code, you will probably need to first download and build [libsigsegv](http://www.gnu.org/software/libsigsegv/), since building clisp requires libsigsegv.


ASDF
----

If you use the Common Lisp implementation you'll need Another System Definition Facility ([ASDF](http://common-lisp.net/project/asdf/)), which is usually included in your implementation of Common Lisp.  Many Linux distributions include ASDF; just install package "cl-asdf".  Fedora, Debian, and Ubuntu are all known to include ASDF as the package "cl-asdf", and others probably use the same name too.  If you don't already have ASDF, you can download it here:  http://common-lisp.net/project/asdf/

ASDF must be configured so it can find the "readable" library once the library is installed.  So you must install the "readable" library in a place where ASDF will currently find it, or modify the ASDF configuration so ASDF can find the "readable" library.  If you set the "prefix" value to "/usr" during configuration, as discussed below, that happens automatically (ASDF normally looks there).

If you are going to install the software using a prefix other than "/usr", you might want to modify the ASDF system configuration file "/etc/common-lisp/source-registry.conf" so that it can find files in common locations.  Here are plausible contents of "/etc/common-lisp/source-registry.conf" that may serve as an example:

    (:source-registry
      (:tree (:home "common-lisp")) ;; expands to e.g., "$HOME/common-lisp/"
      (:tree (:home "cl")) ;; expands to e.g., "$HOME/cl/"
      (:tree "/usr/local/share/common-lisp/source/")
      (:tree "/usr/share/common-lisp/source/")
      :inherit-configuration)

Using ASDF in clisp
-------------------

If you're using clisp (a Common Lisp implementation), you may need an additional step so that clisp can use ASDF.

The clisp of Cygwin does not (as of this writing) include ASDF.  If you are using clisp on Cygwin, one you way you can install it is to download the asdf.lisp file (from http://common-lisp.net/project/asdf/), then install asdf on clisp for this user by doing:

    mkdir -p ~/lisp/asdf
    cp asdf.lisp ~/lisp/asdf
    clisp
    (load (compile-file "asdf.lisp"))
    (exit)

If you use clisp and a Linux distribution's ASDF package (usually named "cl-asdf"), follow the above steps, but instead of the "cp asdf.lisp ~/lisp/asdf" command do this:

    ln -s /usr/share/common-lisp/source/cl-asdf/asdf.lisp ~/lisp/asdf/

The "ln -s" means that updates to your Linux distribution's ASDF will be used automagically.

After this point *(require "asdf")* should work from clisp.


Weird systems without /usr/bin/env
----------------------------------

If you use one of the rare systems that doesn't have the command "/usr/bin/env", then you'll need to install one.  Presuming that you have "/bin/env", and that "sudo" will give you administrator privileges, the following will fix that problem:

    sudo ln -s /bin/env /usr/bin/env

You could also modify the relevant scripts... but fixing your system is the better solution.

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

Many people will want to install to the directory "/usr" (e.g., so commands will be put in "/usr/bin").  To do this, instead run:

    ./configure --prefix=/usr


The configure program supports many options, though most are only useful for special situations.  Here are some configure options you might want to use:

    --without-guile         disable support for guile
    --without-scsh          disable support for scsh
    --without-common-lisp   disable support for Common Lisp

In particular, if you won't be using Common Lisp, consider using the configure option "--without-common-lisp"... in which case you don't need to install and configure either ASDF or an implementation of Common Lisp.

If you change your mind later about your configuration options, just re-run configure with your new configuration options, and then run the later steps as described below (in particular, "make" and "make install").

If you need more information (to configure it for unusual circumstances), run "./configure --help" or read the file "INSTALL".

Build
=====

Now build the code, using:

    make

This will do the minimum necessary, so depending on the circumstance it may appear to do nothing (and that's fine).

You can optionally run the testsuite.  To run the tests you need guile, clisp, a correctly-installed asdf.  Run the testsuite by typing:

    make check


Install
=======

You actually don't have to install it to system-wide locations to use it, if you just want to play with it.  But if you're using this software seriously, you should probably install it to "real" system-wide locations.  To install it, just type:

    make install

This will probably require privileges, so you may need to run "su" first or use sudo ("sudo make install") to get those privileges.  Thus, on many system you'll really type:

    sudo make install

If you're installing the Common Lisp libraries to the system-wide locations, and your system uses the "common-lisp-controller" (including Debian, Ubunutu, and Fedora), then there's one more step.  After you install a Common Lisp library to its final system-wide location, you need to tell common-lisp-controller that you've done so.  Note: If you set DESTDIR as part of "make install", this is *not* an installation to its final location.  This is also usually a privileged command, so again, you'll probably prefix this command with "sudo":

    register-common-lisp-source readable


Do I need to install it to system-wide locations?
=================================================

Again, no; you don't need to install to system-wide locations.  Once it's built, you can use it.

If you have installed it to system-wide locations, then in most cases the programs will automatically find the files when they need them, which makes things a little easier.  E.G., you don't need to include prefixes like "./" in the command names.

If you have not installed the files to system-wide locations, then you need to tell the system where the files are when you try to use them.  You can tell the system where the commands are by prefixing the name with "./" if the command is in the current directory.  Another approach is to modify your PATH. On a Bourne-like shell (the usual kind), if the current directory contains some of the programs you'd like to be able to run without prefixing them with "./", you can modify your PATH using:

    export PATH="$(pwd)/$PATH"



Tutorials
=========

Now you can use it!

If you've not used it before, take look one of our two tutorials:

* [Scheme-tutorial] - Scheme-focused tutorial
* [Common-lisp-tutorial] - Common-Lisp-focused tutorial

