To install the "readable" software directly from our site, you need to have the right prerequisites, download and uncompress the software, configure it, build it, and then actually install it.  In most cases it should be easy, but we've provided some tips to help you avoid (or fix) problems.

If you're completely impatient,  just install the packages guile, expect, clisp, and asdf (asdf is often the package "cl-asdf").  Then download the current stable version from http://readable.sourceforge.net and run the following command-line commands on your Unix-like system (including Cygwin):

    tar xvzf readable-*.tar.gz
    cd readable-*
    ./configure --prefix=/usr
    make
    sudo make install
    sudo register-common-lisp-source readable

Below are more details about each of those steps, including various options you can use.  For example, you can choose to install just the Scheme (guile) implementation, or just the Common Lisp implementation.

Prerequisites
=============

You need a Unix-like (POSIX) system such as a Linux distribution, *BSD, MacOS, etc.  On Windows, just install and use Cygwin; that works very well.  Any modern GNU/Linux system should do nicely.

The "readable" package includes support for both [GNU Guile](http://www.gnu.org/software/guile/guile.html) (a Scheme implementation) and Common Lisp.  The readable package also includes some optional support tools that internally use guile.

Thus, you *must* install [GNU Guile](http://www.gnu.org/software/guile/guile.html) if you want to use Scheme, and we *recommend* that all users install guile so you can use some of our handy support tools.  You can use the configure option "--without-guile" if you don't want any of the Guile materials.

If you plan to use Scheme, you also need to install expect (in addition to guile).  If you use Scheme it's more convenient to use a guile with built-in readline support (most everything except Cygwin does), but this is optional.

If you plan to use Common Lisp, you *must* have an implementation of Common Lisp.  Both clisp and SBCL are known to work, but the library should work on any implementation of Common Lisp.  (If it doesn't, please help us fix that!)  The tutorial uses clisp, but it doesn't have to be clisp.  You'll also need ASDF, as discussed below.  You can use the configure option "--without-common-lisp" (discussed below) if you don't want Common Lisp, in which case you don't need to install or configure ASDF as described below.


Prerequisites if you use the development version
------------------------------------------------

If you plan to use the development (not stable) version, you'll also need:

*   git (to download the software)
*   autoconf and automake (to build it).  You need autoconf version 2.67 or later.
*   guile development libraries (e.g., "guile-devel" on Fedora and Cygwin)
*   python (to generate some HTML files from markdown)


ASDF
----

If you use the Common Lisp implementation you'll need Another System Definition Facility (asdf), which is usually included in your implementation of Common Lisp.  Many Linux distributions include asdf; just install package "cl-asdf".  Fedora, Debian, and Ubuntu are all known to include asdf as the package "cl-asdf", and there are probably more that do as well.  If you don't already have asdf, you can download it here:  http://common-lisp.net/project/asdf/

ASDF must be configured so it can find the "readable" library once the library is installed.  So you must install the "readable" library in a place where ASDF will currently find it, or modify the ASDF configuration so ASDF can find the "readable" library.  If you set the "prefix" value to "/usr" during configuration, as discussed below, that happens automatically (ASDF normally looks there).

If you are going to install the software using a prefix other than "/usr", you might want to modify the ASDF system configuration file "/etc/common-lisp/source-registry.conf" so that it can find files in common locations.  Here are plausible contents of "/etc/common-lisp/source-registry.conf" that may serve as an example:

    (:source-registry
      (:tree (:home "common-lisp")) ;; expands to e.g., "$HOME/common-lisp/"
      (:tree (:home "cl")) ;; expands to e.g., "$HOME/cl/"
      (:tree "/usr/local/share/common-lisp/source/")
      (:tree "/usr/share/common-lisp/source/")
      :inherit-configuration)

clisp
-----

If you're using clisp (a Common Lisp implementation), you may need an additional step so that clisp can use asdf.

The clisp of Cygwin does not (as of this writing) include asdf.  If you are using clisp on Cygwin, one you way you can install it is to download the asdf.lisp file (from http://common-lisp.net/project/asdf/), then install asdf on clisp for this user by doing:

    mkdir -p ~/lisp/asdf
    cp asdf.lisp ~/lisp/asdf
    clisp
    (load (compile-file "asdf.lisp"))
    (exit)

If you use clisp and a Linux distribution's asdf package, follow the above steps, but instead of the "cp asdf.lisp ~/lisp/asdf" command do this:

    ln -s /usr/share/common-lisp/source/cl-asdf/asdf.lisp ~/lisp/asdf/

The "ln -s" means that updates to your Linux distribution's asdf will be used automagically.

After this point '(require "asdf")' should work from clisp.




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

If you use one of the rare systems that doesn't support /usr/bin/env, then you'll need to install one ("sudo ln -s /bin/env /usr/bin/env") or change the first lines of some of the scripts.


Configure
=========

As usual, configure.  To accept all the defaults, installing to the directory "/usr/local" (e.g., commands will be put in "/usr/local/bin"):

    ./configure

Many people will want to install to the directory "/usr" (e.g., so commands will be put in "/usr/bin").  To do this, instead run:

    ./configure --prefix=/usr


The tutorials describe some configure options. Here are some configure options you might want to use:

    --without-guile         disable support for guile
    --without-scsh          disable support for scsh
    --without-common-lisp   disable support for Common Lisp

If you need more information (to configure it for unusual circumstances), run "./configure --help" or read the file "INSTALL".

Build
=====

Now build the code, using:

    make

You can optionally run the testsuite; it's a good idea (you need guile, clisp, a correctly-installed adsf, and expect to run the tests):

    make check


Install
=======

You actually don't have to install it to system-wide locations to use it, if you just want to play with it.  But if you're using this software seriously, you should probably install it to "real" system-wide locations.  To install it, just type:

    make install

This will probably require privileges, so you may need to run "su" first or use sudo ("sudo make install") to get those privileges.  Thus, on many system you'll really type:

    sudo make install

If you're installing the Common Lisp libraries to the system-wide locations, and your system uses the "common-lisp-controller" (including Debian, Ubunutu, and Fedora), then there's one more step.  After you install a Common Lisp library to its final system-wide location, you need to tell common-lisp-controller that you've done so.  Note: If you set DESTDIR as part of "make install", this is *not* an installation to its final location.  This is also usually a privileged command, so again, you'll probably prefix this command with "sudo".  Anyway, just do this:

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

