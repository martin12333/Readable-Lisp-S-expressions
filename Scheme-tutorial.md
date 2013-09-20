This tutorial describes shows how to download and use free-libre/open source software that makes Lisp notation more readable.

This tutorial focuses on Scheme; if you're using Common Lisp, look at the [Common-lisp-tutorial].  You don't need to be familiar with Lisp-like languages to understand this tutorial, though it helps.

Historically Lisp-derived systems represent programs as *s-expressions*, where an operation and its parameters is surrounded by parentheses in that order.  Thus, “2+3” is written as “(+ 2 3)” in a Lisp-derived language. Lisp notation is simple, but most people find it hard to read.  For more information on the problems of traditional Lisp notation, see [Problem].

To solve this, we've developed three tiers of notation, each building on the previous. These are simply new abbreviations for common cases; you can continue to use normally-formatted s-expressions wherever you want to. Curly-infix-expressions add infix notation; neoteric-expressions add the ability to write f(x) instead of (f x), and sweet-expressions deduce parentheses from indentation.  For more details, see [Solution].


Getting our implementations
===========================

To try all the Scheme demos, make sure you have:

*   [GNU Guile](http://www.gnu.org/software/guile/guile.html) (an implementation of the *Scheme* dialect of Lisp).  It's best if you use a guile with built-in readline support (normally it is), but the demos will work if it isn't.
*   expect

If you plan to use the development (not stable) version, you'll also need:

*   git (to download the software)
*   autoconf and automake (to build it)
*   guile development libraries (e.g., "guile-devel" on Fedora and Cygwin)
*   python (to generate some HTML files from markdown)

You should be running on a POSIX system; if you use Windows, you may need to install Cygwin first. Any modern GNU/Linux system will do nicely.

Download and prepare to configure
-----------------------------------

First, download the latest version of the demo and related files and get them ready to configure.  To get the current "stable" version, browse http://readable.sourceforge.net - click on "download files" - and download the current version.  Then uncompress and cd into them, e.g., at the command line:

     tar xvzf readable-*.tar.gz
     cd readable-*

If you *instead* want to get the *development* version, do this instead:

     git clone git://git.code.sf.net/p/readable/code readable-code
     cd readable-code
     git checkout develop  # Switch to "develop" branch
     autoreconf -i

If you use one of the rare systems that doesn't support /usr/bin/env, then you'll need to install one ("ln -s /bin/env /usr/bin/env") or change the first lines of some of the scripts.

Configure
---------

As usual, configure.  To accept all the defaults, installing to /usr/local:

    ./configure

If you want it to install to /usr (e.g., programs go in /usr/bin):

    ./configure --prefix=/usr

Build
------

Now build the code, using:

    make

Install
-------

You actually don't have to install it to use it, if you just want to play with it.  But if you're using this software seriously, you should probably install it to "real" locations.  If you do, just type:

    make install

This will probably require privileges, so you may need to run "su" first or use sudo ("sudo make install") to get those privileges.

The instructions below assume you haven't installed it.  If you *have* installed it, you don't need to include the "./" prefixes in the command names below.  Another way you can avoid the "./" prefixes is to include, in your PATH, the directory that contains these commands in your PATH. On a Bourne-like shell (the usual kind), you can do this to fix your path if your current directory has the commands:

    export PATH="$(pwd)/$PATH"



Using curly-infix-expressions (c-expressions)
=============================================

Let's first try out "curly-infix-expressions" (c-expressions). Let's use guile (a Scheme implementation) to do this.

Just run "./curly-guile".

Any of these three new notations (curly-infix-expressions, neoteric-expressions, and sweet-expressions) can also accept normally-formatted s-expressions. To demonstrate that, type at the command line:

     (+ 2 3)

and when you press enter you'll see 5.

But "curly-infix-expressions" add the ability to use {...} around infix operators. So just type this in:

    {2 + 3}

and you will see 5. Look! You now have a calculator! In curly-infix expressions, {a op b op c ...} is an abbreviation for (op a b c...).  It's important to realize this is a simple syntactic convenience — an abbreviation automatically handled by the system when it reads the input. Traditional Lisp actually includes many abbreviations, for example, 'x is a traditional abbreviation for (quote x), which when executed will just return the quoted x.  So curly-infix-expressions just includes an additional abbreviation and does not change how "Lisp works". Thus, if you enter:

    '{2 + 3}

it will respond with (+ 2 3).  That is, once it's read in, it will read in a quoted (+&nbsp;2&nbsp;3), and "executing" it will return the simple (+&nbsp;2&nbsp;3).  Note that the infix operator *must* be surrounded by whitespace - otherwise, it would have no way to know where the name of the operation begins or ends.

There is intentionally no support for precedence between different operators. While precedence is useful in some circumstances, in typical uses for Lisp-derived languages and sweet-expressions, it doesn't help and is often harmful. In particular, precedence can hide where different lists occur.  This lack of precedence is not a problem at all; usually you can just use curly braces or parentheses when mixing different infix operators:

    {2 + {3 * 4}}

You *can* "chain" the same infix operator, e.g., {2 + 3 + 4} is fine, and it will map to (+ 2 3 4). This works with comparisons, too, as long as the comparisons are the same.  Here's an example:

    {5 <= 7 <= 10}

In short, a *simple infix list* has (1) an odd number of parameters, (2) at least 3 parameters, and (3) all even parameters are the same symbol; it maps to "(even-parameter odd-parameters)".  By intent, there is no precedence and you *must* use another {...} for an embedded infix list.

If a curly-infix list has 0, 1, or 2 parameters, they are treated specially.  {} is a synonym for (), since they are both empty lists.  { e } maps to just e, and { - x } maps to (- x).

But what happens to infix lists that aren't any of these?  The answer is that they map to "($nfx$ parameters)".   You can then define a macro named "$nfx$" to process the list (e.g., to implement a precedence system), or define "$nfx$" as an error to prevent its use.  The dollar signs make it unlikely that you'd define or use this by accident.

Note that curly-infix-expressions intentionally do not force a particular precedence, nor do they automatically switch to infix operators recursively inside {...}. Many previous systems did that, but this turned out to interfere with many of Lisp's power (which needs homoiconicity and generality). It also does not attempt to guess when infix operators are used. [After many experiments with real problems, David A. Wheeler found that this rule works better for Lisps than those alternatives.](http://www.dwheeler.com/readable/version02.html)

The list elements inside {...} can be neoteric-expressions, not just ordinary list datums.  In particular, that means an expression of the form f(...) is treated the same as (f ...).  Thus, inside a curly-infix list, cos(0) is treated the same as (cos 0), which evaluates to 1.  So we can do this:

    {cos(0) + 1}

Just execute "(exit)" to get out.

(Note: If you want to try the draft curly-infix implementation in Common Lisp, run a Common Lisp implementation such as clisp, and then execute (load "readable.cl").).


Using Neoteric-expressions (n-expressions)
===========================================

Now let's do more with neoteric-expressions (which include curly-infix-expressions).  You'll need to have guile and expect for this to work; assuming you do, try this:

      ./neoteric-guile

We have reports that guile version 2.0's "autocompilation" feature can cause troubles.  If you see ";; compiling /blah/blah...", then just exit by typing **exit()** and **Enter**.  Then restart it (don't the clear cache first), and it should work.

Neoteric-expressions support curly-infix-expressions, including normally-formatted s-expressions.  In addition, neoteric-expressions add special meanings to the grouping symbols ( ), [ ], and { } if they *immediately* follow an expression (instead of being separated by whitespace).  In particular, any e( ... ) is mapped to (e ...), and any e{ ... } is mapped to (e { ... }).

This means that you can use more "traditional" functional notation, e.g., f(1 2) maps to (f 1 2). Just type in the name of a function, an opening "(", its parameters (space-separated), and a closing ")". Make sure that you do *not* have a space before the (prefixed) function name and the following "(". For example, type this in:

     cos(0)

and get a very reasonable response, 1. Here's another - try this:

    substring("Hello" 1 3)

This will produce "el".

You can nest them, just as you'd expect:

    substring("Hello" 1 string-length("xyz"))

Using function name prefixes is a nice way of showing negation, e.g., -(x) computes the value of 0 - x. So while curly infix by itself doesn't handle prefix functions, neoteric-expressions can handle them nicely:

    define(n 12)
    {-(n) / 2}

You can even use function name prefixes with traditional binary operators, such as:

    *(5 4)

This works with zero parameters, too; if you have a command called "help" (guile does), and choose not to give it any parameters, just type this (without pressing space before typing it in):

    help()

It's actually quite common to have a function call pass one parameter, where the parameter is calculated using infix notation, so there's a rule to simplify this common case. You can use f{x + 1}, which maps to (f {x + 1}) which then maps to (f (+ x 1)). This makes it easy to pass a single parameter which happens to be calculated using infix. For example, factorial{n - 1} maps to factorial({n - 1}) which maps to (factorial (- n 1)).  You can try out this simple example:

     not{#t and #f}

Just like traditional s-expressions, spaces separate parameters, so it's *important* that there be *no* space between the function name and the opening "(". Since spaces separate parameters, a space between the function name and the opening "(" would create two parameters instead of a single function call. The same is basically true for traditional s-expressions, too; (a b) and (ab) are not the same thing.

Here's the real rule: in neoteric-expressions, e(...) maps to (e ...), e{} maps to (e), other e{...} maps to (e {...}), e[ ... ] maps to ($bracket-apply$ e), and (. e) maps to e. The "$bracket-apply$" is so that you can write a macro to access arrays and other mappings.  The (. e) rule lets you escape expressions (e.g., for the sweet-expressions we'll describe next).  Note that "neoteric-expressions" used be called "modern-expressions"; you may see some older documents using that name.

Normally, people and pretty-printers will format Lisp code so that parameters inside a list are *separated* by whitespace, e.g., (a b c), so it turns out that this change in interpretation doesn't change the meaning of typically-formatted modern Lisp code (and you can pretty-print code to fix it in the rare cases you need to). What's more, typical Lisp-aware text editors can often work with neoteric-expressions as they are, without change... so if you don't want to change the way you work, but have a somewhat more readable notation, neoteric-expressions can help. But we still have to do all that parentheses-balancing, which hinders readability. Sweet-expressions, our next stop, address this.  To get out of this demo, type *exit()* *Enter* or *control-D* *Enter*.


Using Sweet-expressions (t-expressions) 
=======================================

Sweet-expressions take neoteric-expressions and infers parentheses from indentation. Let's try them out:

    ./sweet-guile


Again, we have reports that guile version 2.0's "autocompilation" feature can cause troubles.  If you see ";; compiling /blah/blah...", then just exit by typing **(exit)** and **Enter** **Enter**.  Then restart it (don't the clear cache first), and it should work.

In sweet-expressions, an indented line is a parameter of its parent, and later terms on a line are parameters of the first term. A line with exactly one term, and no child lines, is simply that item; otherwise those terms and its child lines are themselves a new list. Lines with *only* a ;-comment, and nothing else, are completely ignored - even their indentation is irrelevant. Whitespace-only lines at the beginning of a new expression are ignored, but a whitespace-only line (including a zero-length line) ends an expression. So, just type in your expression, and type a blank line (an extra Enter) to indicate that you're done.

Here's a trivial example; type this in, and enter a blank line (*Enter* *Enter*) to calculate it:

    substring "Hello"
      1
      3

What happens if the parameters are not constants, but something to be calculated? No problem, just put them on new lines and give them parameters. If something has parameters, then it must be something to calculate too! Here's another example (be sure to consistently indent the lines after the first one):

    substring
      "Hello"
      1
      string-length "xyz"

You can use parentheses, too; inside any grouping characters (...), [...], and {...}, indentation is ignored:

      substring
        "Hello"
        1
        string-length("xyz")

Here are some other valid sweet-expressions:

    if {7 < 5}
      {3 + 4}
      {5 * {2 + 3}}
    
    abs{0 - 5}

Here's a more substantial example:

    define fibfast(n)  ; Typical function notation
      if {n < 2}       ; Indentation, infix {...}
        n              ; Single expr = no new list
        fibup n 2 1 0  ; Simple function calls
    
    define fibup(max count n1 n2)
      if {max = count}
        {n1 + n2}
        fibup max {count + 1} {n1 + n2} n1

This has the same meaning as the following (and a sweet-expression reader would accept either):

    (define (fibfast n)
      (if (< n 2)
        n
        (fibup n 2 1 0)))

    (define (fibup max count n1 n2)
      (if (= max count)
        (+ n1 n2)
        (fibup max (+ count 1) (+ n1 n2) n1)))

If you're not sure what something means, you can "quote" it so it won't execute.  If you type ' followed by space, the indentation processing continues (starting at the ' mark indentation) but the whole thing will be quoted.  That way you can try things out!  (Another way to try out what things mean is the unsweeten command described below).  For example:

    ' foo bar1 bar2
        spam eggs eggs

Will produce:
(foo bar1 bar2 (spam eggs eggs))

Sometimes you want to have a parameter that is a list of lists, or where the function to be called is in fact determined by another calculation. This is indicated with the "\\\\" keyword; basically, at the beginning of line (but after indentation) "\\\\" maps into a null function name, so you can use forms like "let" easily.

Note: In our current implementation, users of guile 2.0 must have autocompilation enabled.  This is the default anyway.

When you're done with the demo, you can exit:

    exit()

Again, be sure to type *Enter* *Enter* (a blank line) to execute the expression.


Unsweeten: Translating Sweet-expressions into S-expressions
===========================================================

We have a filter called "unsweeten" that translates sweet-expressions into S-expressions.  You can use it to write programs using sweet-expressions, like a compiler.  Basically, you can write sweet-expressions, and use unsweeten to translate that into something your Lisp-based system can understand.  You can also use unsweeten to help you understand sweet-expressions - type in expressions, and see what they translate to!

You can run unsweeten interactively, just type:

    ./unsweeten

You can then type in sweet-expressions, such as this:

<pre>
define factorial(n)
  if {n <= 1}
    1
    {n * factorial{n - 1}}
</pre>

Remember to enter a blank line (*Enter* *Enter*) to end an expression.

You can use this tool to process files, say, via a makefile.  Then you can use sweet-expressions to write your code, and have it quickly translated to s-expressions.  The following portable Makefile snippet translates all ".sscm" (Sweet Scheme) files into ".scm" (Scheme) files; as this is Makefile be sure the first character on the last line is TAB:

    UNSWEETEN = unsweeten
    .sscm.scm:
        $(UNSWEETEN) $< > $@


Unsweeten also copies out comments, but only in certain cases.  Only a group of semicolon comments starting from either the file's very beginning, or after a blank line, are copied to the output.  Such semicolon comments will have indentation (if any) removed. Block comments and comments *inside* a datum are never copied.  Semicolon comments immediately after a datum aren't copied either (the reader has to consume them to see if it's reached the end of the datum - and once it is consumed unsweeten can't copy the comment out).

Unsweeten also has some special substitutions.  If a semicolon begins a line, the next character may cause it to do something special.  If line begins with ";#: or ";!", the line is copied back without the leading semicolon.  If a line begins with ";_", then the line is copied back without either of those first two characters.


Sweet-run: Running sweet-expression scripts
===========================================

The program "sweet-run" lets you write scripts using sweet-expression notation, invoking whatever interpreter you'd like.  Generally, you invoke sweet-run as the first line of a script; the rest of the lines will be interpreted as sweet-expressions (using unsweeten) and run.  The first lines *after* the first line usually declare what script to invoke.

Examples should make this clear.  Here's an example of a script that is written with sweet-expressions and runs on guile:

    #!/usr/bin/env sweet-run
    ;#!guile
    ;!#

    define factorial(n)
      if {n <= 1}
        1
        {n * factorial{n - 1}} ; f{...} => f({...})

    display "Now let's calculate factorial(22):\n"
    display factorial(22)

Similarly, here's an example of a script that is written with sweet-expressions and runs on scsh (Scheme shell):

    #!/usr/bin/env sweet-run
    ;#!scsh -s
    ;!#

    define argc length(command-line-arguments)

    if {argc > 0}
        display "Got arguments!\n"

    run head(-10 "README")

You can even do this with Common Lisp (e.g., clisp).  Since unsweeten expects Scheme syntax, at this time you have to limit yourself to the syntax in common between Scheme and Common Lisp.  This is not a limitation on the *concept* of sweet-expressions, it's just a lmitation of our current implementation.  So here's an example using clisp:

    #!/usr/bin/env sweet-run
    ;#!/usr/bin/env clisp

The "examples" subdirectory has files *sweet-run-demo-clisp*, *sweet-run-demo-guile*, and *sweet-run-demo-scsh* that demonstrate sweet-run.  If you've *installed* the readable system (with "make install"), you can just run them.  If you haven't installed the readable system, you'll need modify your PATH so that the sweet-run and unsweeten programs can be found.  On a Bourne shell system, if sweet-run and unsweeted are in your current directory, you can modify your PATH by running:

    export PATH="$(pwd)/$PATH"

Now you can run (for example) "example/sweet-run-demo-guile" to run that guile demo.


Sweeten: Translating S-expressions into Sweet-expressions
=========================================================

We also have a filter called "sweeten" that translates existing S-expressions into sweet-expressions (this is the *reverse* of unsweeten).  You can use sweeten to help you understand sweet-expressions, or to move an existing program to sweet-expressions.

Just put some S-expressions into a file and you can have them translated.  For example, you could the following in file "demo.scm":

    (define (doubleadd a b)
        (* (+ a b) 2))


You can run the sweetener by typing:

    ./sweeten < demo.scm

And you should see this as output:

    define doubleadd(a b) {{a + b} * 2}

You can also run it interactively, but you may find that you want to press an extra "Enter" after your expression.  (It's done this way so that when it translates files they look correct.)

Sweeten reads Scheme (or more specifically, Guile) S-expression syntax.  However, you can use it for other Lisp-like languages if you stick to the syntax that is common between them.  There is also a limitation of the sweeten implementation: Comments inside any parentheses will not be produced in the output.  (This is because it uses the underlying system "read" function, which throws this information away.) Still, it's a useful tool.

Readable library (Scheme)
=========================

These new notations are implemented in a Scheme library module.  They're written to easily port to other Scheme systems, and even non-Scheme systems, but guile is what we primarily use right now.

To use the modules from guile, simply say:

    (use-modules (readable kernel))

This exports the following functions:

*   curly-infix-read(. port): Read a curly-infix-expression datum from port.
*   neoteric-read sweet-read(. port): Read a neoteric-expression datum from port.
*   sweet-read sweet-read(. port): Read a sweet-expression datum from port.
*   replace-read(replace-read-with): Change the current "read" function to the function replace-read-with.
*   restore-traditional-read(): Restore the traditional read function.

For porting to other Scheme implementations, see the comments in the file `src/kernel.scm`.  On a sufficiently R5RS Scheme with SRFI-0 it can be loaded `(load "src/kernel.scm")` and will work, but without line source information and with its internal functions scattered all over your user's namespace.

More Complex Examples
=====================

Basic Expressions
-----------------

If you're familiar with traditional s-expressions, here are more examples. The left-hand-side are sweet-expressions; the right-hand-side are the transitional s-expression forms, though the sweet-expression reader can read them as well:

    factorial(z)         <==> (factorial z)
    foo(x y)             <==> (foo x y)
    bar(x y z)           <==> (bar x y z)
    factorial{x - 1}     <==> (factorial {x - 1}) <==> (factorial (- x 1))
    f(g(y) h(y) a)       <==> (f (g y) (h y) a)
    f({x + 2} y {z - 3}) <==> (f (+ x 2) y (- z 3))
    f{{x + 2} * {y - 3}} <==> (f (* (+ x 2) (- y 3)))


Grouping and Splitting
----------------------

A basic indentation processor can sometimes require an excessive amount of vertical space, or it can make it hard to show the relationship between items at the same list level.  The \\\\ marker can help resolve this.

When a \\\\ marker is first on a line (after optional indentation), it stands for nothing at all.  This is useful for indicating groups of lists, e.g.,:

      let*
        \\
          asunit        unit(m)
          length-asunit length(asunit)
        if ...

=>

    (let* ((asunit        (unit m))
           (length-asunit (length asunit)))
      (if ...))


The \\\\ marker is also useful, when first on a line, to show that this line is logically subordinate to the previous line, but nevertheless it is at the same list level. For example:

    arc-if
      complicated-condition1()
      \\ do-if-complicated-condition1-is-true()
      complicated-condition2()
      \\ do-if-complicated-condition1-is-true()
      ...

=>

    (arc-if
      (complicated-condition1)
      (do-if-complicated-condition1-is-true)
      (complicated-condition2)
      (do-if-complicated-condition1-is-true)
      ...)

If you have small items that need to be at the same list level, you can combine them on one line and separate them with \\\\.  For example, this is useful for keyword symbols:

    foo
      :amount \\ 100 \\ :from \\ ocean

=>

    (foo :amount 100 :from ocean)

But what if you actually want to refer to "\\\\"?  No problem, just use the expression "{\\\\}".  Exactly what this means will depend on whether or not your Lisp uses "slashification" (e.g., if "\" followed by any character means that character is part of a symbol).  If it doesn't use slashification, \\\\ means the symbol with a two-character name \\\\.  If it uses slashification, \\\\ means the symbol with the one-character name \\.  The good news is that you can use the marker \\\\ on practically any Lisp system, so you don't need to constantly change notation if you use different ones.

Sublist
-------

Sweet-expressions support another special abbreviation when not inside ( ... ), [ ... ], or { ... }.  A $ (aka SUBLIST) in the middle of list restarts list processing; the right-hand-side (including its sub-blocks) is the last parameter of the left-hand side.  This is really handy when you need to chain functions together, e.g., where a sequence of functions process in sequence the output of something more complex.  For example:

    onfail $ run $ head -10 "README"

has the same meaning as:

    (onfail (run (head -10 "README")))

Empty lines
-----------

Since empty lines end an expression, you can't have an empty line in the middle of one.  That's not as bad as it sounds, though.  You can instead use a semicolon comment (the indentation doesn't matter in this case) or \\\\ by itself at the correct indentation.

The reason is that this (1) makes interactive use pleasant, and (2) we don't want the interactive and file syntax to be different.

Collecting lists
----------------

When indentation is active, you can use <\* and \*\> as essentially parentheses, but indentation processing is still active, and inside the indent starts again at the edge.  This is useful for short expressions after a "let", as well as for creating long module definitions. E.G.:

    let <* x cos(a) *>
        {2 * x}

which becomes:

    (let ((x (cos a)))
        (* 2 x))


What's the big deal?
====================

If you aren't familiar with Lisp, you may say "what's the big deal"? After all, this looks a lot like traditional languages. Many have commented that it looks like Python, with its use of indenting, and of course nearly all other languages use infix notation.

But that's the point - the results look much more familiar (and thus are more acceptable to non-Lispers), but all of Lisp's more exotic capabilities still work. You can use techniques like quoting (') and quasi-quoting (\`) with lifting (,), which enable powerful capabilities. Many people have created "infix" notations with Lisp-like languages before, but they all failed to work with many other Lisp features. We think this approach succeeds instead, where others before have failed.


Closing Remarks
===============

These notations can take a few minutes to learn how to use, just like anything else new, but they are worth it.

You can see more Scheme-specific information about sweet-expressions in SRFI-110.

Although we used some specific implementations, note that these notations could be used with any Lisp-based system.
