Here is a list of some prepackaged implementations of readable Lisp capabilities (this is probably very incomplete):

*   [Quicklisp (for Common Lisp)](http://www.quicklisp.org/) includes readable libraries for Common Lisp, beginning [October 2013](http://blog.quicklisp.org/2013/10/october-2013-quicklisp-dist-update-now.html).  See [Install-howto] for information on how to install this.  This installs the entire suite of notations on any compliant Common Lisp implementation.
*   [Guile Scheme version 2.0.7 (released 2012-11-30) and later directly implement SRFI-105 (curly-infix)](https://www.gnu.org/software/guile/manual/html_node/SRFI_002d105.html) as part of the implementation.  Just use #!curly-infix.
* [Sagittarius Scheme](http://practical-scheme.net/wiliki/schemexref.cgi?SRFI-105) implements curly-infix.
*   [Mac OS X "Homebrew"](http://brew.sh/) includes our readable package, just use "brew install readable".  As of 2014-06-14 it only supported Scheme, not Common Lisp.  Our thanks to Elliott Cable for packaging it!

See [Install-howto] for how to install the readable software from source code if this list doesn't meet your needs.
