This tutorial describes shows how to use the "readable" free-libre/open source software with Common Lisp.  The "readable" software adds new abbreviations to traditional Lisp notation, to make it far more readable.  You don't need to be very familiar with Lisp-like languages to understand this tutorial, though it greatly helps.

But first, here are some related pages you might find useful:

* [Install-howto] describes how to download and install the software that this tutorial describes how to use.
* [Scheme-tutorial] is a tutorial for Scheme (the tutorial you're reading focuses on Common Lisp)
* [Problem] provides more information on the problems of traditional Lisp notation.  Historically Lisp-derived systems represent programs as *s-expressions*, where an operation and its parameters is surrounded by parentheses in that order.  Thus, “2+3” is written as “(+ 2 3)” in a Lisp-derived language. Lisp notation is simple, but most people find it hard to read.
* [Solution] describes in more detail our solution to the [Problem] of traditional Lisp notation.  We've developed three tiers of notation, each building on the previous. These are simply new abbreviations for common cases; you can continue to use normally-formatted s-expressions wherever you want to. Curly-infix-expressions add infix notation; neoteric-expressions add the ability to write f(x) instead of (f x), and sweet-expressions deduce parentheses from indentation.

So let's get started!

Starting Common Lisp implementation and loading the readable library
====================================================================

You need to run your implementation of Common Lisp and then load the "readable" library.  Any Common Lisp implementation should work, as it only depends on standard Common Lisp mechanisms. The sbcl implementation is especially well-tested and known to work well.

Unfortunately, the clisp implementation of Common Lisp has a number of annoying limitations and problems, so we would recommend another implementation (such as sbcl).  That said, the readable notation does *work* on clisp if you want to use it, and we do include information below on how to work around clisp's problems.  As of May 2015 it is still a live project, but the last release of clisp was in 2010; as a result, the clisp-provided ASDF system is absurdly old.  We provide some work-arounds for various broken behaviors of clisp, but they are less than optimal.  If you want an especially portable implementation of Common Lisp, you might consider CCL (which is effectively very portable) or ECL (which is written completely in C).

You can load the "readable" library by using QuickLisp, or by invoking ASDF directly. QuickLisp is by far the easiest way, though using it doesn't install a few tools you might want.  Here is how to do it in each case.

Loading the "readable" library via QuickLisp
--------------------------------------------

QuickLisp makes things easy to start, assuming you have QuickLisp installed (see [Install-howto]).  You need to *first* start up up your implementation of Common Lisp.  E.G., for sbcl, this is normally:

    sbcl  # Use "clisp" to run clisp instead, obviously.

To load the "readable" library, at the command line or at the beginning of each file, do this:

    (ql:quickload "readable")

After that, you just tell "readable" which notation you want to use.  So for example, if you want to use the "sweet-expression" notation described later, you'd execute *(readable:enable-sweet)*.

Thus, the top of each of your files would say something like this:

    (ql:quickload "readable")
    (readable:enable-sweet) ; or whichever notation you've chosen.

... and at the end of the file you can insert:

    (readable:disable-readable)


Clisp and QuickLisp
-------------------

Clisp has various annoying limitations, including one that affects QuickLisp.  Clisp does *not* run its initialization (rc) files when it is loading a file; it only looks at its rc files when running interactively.  That means that QuickLisp is not immediately available in clisp, even if it's installed, when directly running files.

To work around this limitation of clisp, you can add the following text in your file before using QuickLisp:

    #-quicklisp
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))

An alternative workaround is to invoke clisp with "-i" (to force it to load an initialization file), e.g.:

    clisp -i ~/.clisp YOURFILE.lisp



Loading the "readable" library via ASDF
---------------------------------------

If you don't want to use QuickLisp, you can load the library directly using ASDF.

*First*, start up up your Common Lisp implementation.  E.G., for sbcl, this is normally:

    sbcl # Use "clisp" to run clisp instead, obviously.

If you installed "readable" in the standard system-wide location, that's it.  However, if you didn't install the "readable" library source files in the standard central place, you will need to tell ASDF the location of the library files.  Just set environment variable CL_SOURCE_REGISTRY.  We use this ourselves to test new versions of the "readable" library.  E.G., if the files are in the current directory, start up your implementation of Common Lisp this way (replace "clisp" as appropriate):

    CL_SOURCE_REGISTRY="$PWD" sbcl

*Second*, you need start asdf.  Usually this is just:

    (require "asdf")  ; Load "asdf".  You can use (load "path-to-asdf") if you must.

*Third*, you need to load the actual "readable" library, at the command line or at the beginning of each file.  That's easy with modern versions of ASDF:

    (asdf:load-system :readable)  ; Load the "readable" library

In older versions of ASDF, you needed to use <tt>(asdf:oos 'asdf:load-op :readable)</tt>. However, if your ASDF is so old that it doesn't include <tt>asdf:load-system</tt>, you should consider upgrading to ASDF 3.  For a discussion of the many advantages of ASDF version 3, ee [ASDF 3, or Why Lisp is Now an Acceptable Scripting Language](http://fare.tunes.org/files/asdf3/asdf3-2014.html).

After you've loaded the readable library, you can use it.  Generally, you run (readable:enable-...) where "..." selects the readable notation you want to use.  E.G., (readable:enable-sweet) enables sweet-expression processing as described below.  The text below will show the various options.  When you're all done, you can use (readable:disable-readable) to restore the readtable to the state before you enabled any readable notations.  You can enable a different readable notation at any time; you don't need to disable the current notation first.

*RECAP*: The normal sequence after you start up your Common Lisp implementation (e.g., at the top of a file) is:

    (require "asdf")
    (asdf:load-system :readable)
    (readable:enable-sweet) ; or whichever notation you've chosen.

... and at the end of the file you can insert:

    (readable:disable-readable)

Lowercase symbols (readtable-case)
------------------------------------------------------

[Common Lisp is actually case-sensitive](http://www.cliki.net/case%20sensitivity); by default it converts all symbols into upper case and then displays them in upper case.  If you prefer to enter and see normal symbols in lowercase, just set Common Lisp's built-in "readtable-case" capability to ":invert" by doing this:

    (setf (readtable-case *readtable*) :invert)

"Readtable-case" is a general capability of Common Lisp, not specific to the "readable" notations, but all readable notations work with it.  It's best to do this *before* enabling a readable notation. From here on, we'll assume you did this, though it is not required.

I think that using readtable-case :invert makes interacting with Common Lisp more pleasant.  The one issue is that when debugging some actoins will reveal that the symbols are actually (still) being stored as upper case for compatibility's sake.  I think that's a small price to pay.


A few other notes
-----------------

The instructions below assume that you don't "use" the package, so you'll have to include the name of the package to invoke anything in it.  If you find that inconvenient, you can omit all the "readable:" prefixes by first using the standard Common Lisp "use-package" command:

    (use-package :readable)

For your convenience, the full package includes the tools "sweet-clisp" and "sweet-sbcl" (if you have clisp or sbcl respectively).  These tools automatically start clisp or sbcl (respectively) and put you in sweet-expression mode (the last tier).  But let's start at the beginning first.


Using basic curly-infix-expressions (c-expressions)
=============================================

Let's first try out "curly-infix-expressions" (c-expressions).  We'll assume you've installed things, started up your favorite Common Lisp implementation, and that you've successfully loaded the "readable" library using either *(asdf:load-system :readable)* or *(ql:quickload "readable")*.

To enable basic c-expressions, run this command (which will modify our existing readtable to add these capabilities):

    (readable:enable-basic-curly)

Any of the readable notations (basic curly-infix-expressions, full curly-infix-expressions, neoteric-expressions, and sweet-expressions) can also accept normally-formatted s-expressions. To demonstrate that, type at the command line:

     (+ 2 3)

and when you press enter you'll see 5.

Basic curly-infix-expressions add the ability to use {...} around infix operators.  Basically, {...} also creates a list, but in the usual case the reader converts it into a list where the *even* parameter(s) becomes the *first* parameter.  So just type this in:

    {2 + 3}

and you will see 5. Look! You now have a calculator! In curly-infix expressions, {a op b op c ...} is an abbreviation for (op a b c...).  It's important to realize this is a simple syntactic convenience — this is just an abbreviation automatically handled by the system when it reads the input. Traditional Lisp actually includes many abbreviations, for example, 'x is a traditional abbreviation for (quote x), which when executed will just return the quoted x.  So curly-infix-expressions just adds an additional abbreviation and does not change how "Lisp works". For example, if you enter:

    '{2 + 3}

it will respond with (+ 2 3).  That is, once it's read in, it will read in a quoted (+&nbsp;2&nbsp;3), and "executing" it will return the simple (+&nbsp;2&nbsp;3).  Note that the infix operator *must* be surrounded by whitespace - otherwise, it would have no way to know where the name of the operation begins or ends.  This means that all of Lisp's capabilities, such as macros and quasiquoting, just keep working.

There is intentionally no support for precedence between different operators. While precedence is useful in some circumstances, in typical uses for Lisp-derived languages and sweet-expressions, it doesn't help and is often harmful. In particular, precedence can hide where different lists occur.  This lack of precedence is not a problem at all; you can just use curly braces or parentheses when mixing different infix operators:

    {2 + {3 * 4}}

You *can* "chain" the same infix operator, e.g., {2 + 3 + 4} is fine, and it will map to (+ 2 3 4). This works with comparisons, too, as long as the comparisons are the same.  Here's an example, which is just another way to write (<= 5 7 10):

    {5 <= 7 <= 10}

In short, a *simple infix list* has (1) an odd number of parameters, (2) at least 3 parameters, and (3) all even parameters are the same symbol; it maps to "(even-parameter odd-parameters)".  By intent, there is no precedence and you *must* use another {...} for an embedded infix list.

If a curly-infix list has 0, 1, or 2 parameters, they are treated specially.  {} is a synonym for (), since they are both empty lists.  { e } maps to just e, and { - x } maps to (- x).

But what happens to infix lists that aren't any of these?  The answer is that they map to "($nfx$ parameters)".   You can then define a macro named "$nfx$" to process the list (e.g., to implement a precedence system), or when processing lists, note this symbol specially.  The dollar signs make it unlikely that you'd define or use this by accident.

Note that curly-infix-expressions intentionally do not force a particular precedence, nor do they automatically switch to infix operators recursively inside {...}. Many previous systems did that, but this turned out to interfere with many of Lisp's power (which needs homoiconicity and generality). It also does not attempt to guess when infix operators are used. [After many experiments with real problems, David A. Wheeler found that this rule works better for Lisps than those alternatives.](http://www.dwheeler.com/readable/version02.html)

That's it!  That's all there is to basic c-expressions, they're really easy.

A big advantage of basic c-expressions is that they're really simple in concept and very simple to implement.  The sample implementation file basic-curly.lisp implements this in only 64 lines of (non-comment) code; if you don't want to bring in a separate library, you could simply copy that code into your program.  They also have practically no chance of conflicting with anything else; they only redefine "{" and "}" in the readtable, for example.

Like all readable notations, basic c-expressions work very well with macros, quasiquotation, and other capabilities of Lisp.  Many people have tried to implement infix systems with macros, but their syntax is terrible and they often have surprising (and unfortunate) interactions with macros.  Instead, the readable notations "just work".

You can disable basic curly-expressions (or any other readable notation) by just running this, which restores the readtable that was in effect before you enabled a readable notation:

    (readable:disable-readable)


Using full curly-infix
======================

Basic curly-infix provides useful features, but we have additional tiers that provide more improvements to standard Lisp syntax.  Let's move on to "full" curly-infix.

You can enable full curly-infix by running:

    (readable:enable-full-curly-infix)

In full curly-infix, the list elements inside {...} are (recursively) neoteric-expressions.   In neoteric-expressions, an expression of the form f(...) is treated the same as (f ...).  Thus, inside a curly-infix list, cos(0) is treated the same as (cos 0), which evaluates to 1.  So we can do this:

    {cos(0) + 1}

We can explain more about neoteric-expressions... but let's learn more by switching to neoteric-expressions entirely.  You can run "(readable:disable-readable)" if you want, but you don't have to; you can just enable a different readable notation directly, and it'll switch correctly.


Using neoteric-expressions (n-expressions)
==========================================

You can enable neoteric-expressions by running:

    (readable:enable-neoteric)

Introduction to neoteric-expressions
------------------------------------

The difference between enable-neoteric and enable-full-curly-infix is that enable-neoteric lets you type in neoteric-expressions at the topmost level.  With enable-full-curly-infix, you have to surround neoteric-expressions with {...}.

Neoteric-expressions support curly-infix-expressions, including normally-formatted s-expressions.  In addition, neoteric-expressions add special meanings to the grouping symbols ( ), [ ], and { } if they *immediately* follow an expression (instead of being separated by whitespace).  In particular, any e( ... ) is mapped to (e ...), and any e{ ... } is mapped to (e { ... }).

This means that you can use more "traditional" functional notation, e.g., f(1 2) maps to (f 1 2). Just type in the name of a function, an opening "(", its parameters (space-separated), and a closing ")". Make sure that you do *not* have a space before the (prefixed) function name and the following "(". For example, type this in:

     cos(0)

and get a very reasonable response, 1. Here's another - try this, to use subseq to get a substring:

    subseq("Hello" 1 3)

This will produce "el".

You can nest them, just as you'd expect:

    subseq("Hello" 1 length("xyz"))

Using function name prefixes is a nice way of showing negation, e.g., -(x) computes the value of 0 - x. So while curly infix by itself doesn't handle prefix functions, neoteric-expressions can handle them nicely:

    defun(neghalf (n)
        {-(n) / 2})

You can even use function name prefixes with traditional binary operators, such as:

    *(5 4)

This works with zero parameters, too.  For example, you can output a newline to the standard output stream by invoking <tt>[terpri](http://clhs.lisp.se/Body/f_terpri.htm)</tt> like this:

    terpri()

It's actually quite common to have a function call pass one parameter, where the parameter is calculated using infix notation, so there's a rule to simplify this common case. You can use f{x + 1}, which maps to (f {x + 1}) which then maps to (f (+ x 1)). This makes it easy to pass a single parameter which happens to be calculated using infix. For example, factorial{n - 1} maps to factorial({n - 1}) which maps to (factorial (- n 1)).  You can try out this simple example:

     not{t and nil}

Just like traditional s-expressions, spaces separate parameters, so it's *important* that there be *no* space between the function name and the opening "(". Since spaces separate parameters, a space between the function name and the opening "(" would create two parameters instead of a single function call. The same is basically true for traditional s-expressions, too; (a b) and (ab) are not the same thing.

Neoteric-expressions, more formally
-----------------------------------

Here's the real rule: in neoteric-expressions, e(...) maps to (e ...), e{} maps to (e), other e{...} maps to (e {...}), e[ ... ] maps to ($bracket-apply$ e), and (. e) maps to e. The "$bracket-apply$" is so that you can write a macro to access arrays and other mappings.  The (. e) rule lets you escape expressions (e.g., for the sweet-expressions we'll describe next).  Note that "neoteric-expressions" used to be called "modern-expressions"; you may see some older documents using that name.

Backwards compatibility
-----------------------------

The advantage of full-curly-infix is that it is totally and completely compatible with traditional s-expressions (since traditional s-expressions do not define what happens inside {...} anyway).  Enabling neoteric expressions can, in extremely unlikely scenarios, cause expressions to be interpreted differently... but they let you use n-expressions directly.

Normally, people and pretty-printers will format Lisp code so that parameters inside a list are *separated* by whitespace, e.g., (a b c), so it turns out that this change in interpretation doesn't change the meaning of typically-formatted modern Lisp code (and you can pretty-print code to fix it in the rare cases you need to). What's more, typical Lisp-aware text editors can often work with neoteric-expressions as they are, without change... so if you don't want to change the way you work, but have a somewhat more readable notation, neoteric-expressions can help.  If you're worried about some expression being interpreted differently, you can disable neoteric-expressions while you read them... or just run them through a pretty-printer (pretty-printers fix such weird formats normally anyway).

More examples
-------------

Here are more examples. The left-hand-side are neoteric-expressions (and thus are also valid sweet-expressions, as we'll discuss next).  The right-hand-side are the traditional s-expression forms.  Note that the neoteric-expression and sweet-expression readers accept the right-hand-side version as well:

    factorial(z)         <==> (factorial z)
    foo(x y)             <==> (foo x y)
    bar(x y z)           <==> (bar x y z)
    factorial{x - 1}     <==> (factorial {x - 1}) <==> (factorial (- x 1))
    f(g(y) h(y) a)       <==> (f (g y) (h y) a)
    f({x + 2} y {z - 3}) <==> (f (+ x 2) y (- z 3))
    f{{x + 2} * {y - 3}} <==> (f (* (+ x 2) (- y 3)))



Another clisp limitation: ugly vertical bars in symbols
=======================================================

If you use clisp, a widely-used Common Lisp implementation, then in many cases symbols will be written with vertical bars (|...|) whenever neoteric-expressions or sweet-expressions are active.  This behavior is peculiar to clisp.  This behavior is technically not *wrong*, but it is astoundingly weird and ugly, and this is absolutely *not* required by the Common Lisp specification.

If you use clisp and want *all* symbols to be displayed nicely, you can prevent this by setting \*print-escape\* like this:

    (setq *print-escape* nil)

The problem with setting *print-escape* is that this completely disables printing vertical bars, even when the vertical bar characters *do* need to be there.

Another partial solution (as described below) is to use the routines we provide for writing readable expressions (below).  Those routines produce nicer-looking output, and include work-arounds so that symbols are displayed normally even when you are using clisp.

The "sweet-clisp" script may help.  It uses both of these mechanisms (print-escape nil and special printing routines), and also works around some other clisp bugs, to prevent all those ugly vertical bars.

We hope that this will not happen in a future version of clisp.  The problem is in clisp's write function.  More specifically, the problem is in clisp source file "src/io.d" function "pr_symbol_part", which implements this weird behavior yet provides no mechanism to turn it off.   On 2013-05-06 David A. Wheeler raised the issue of adding such flexibility to clisp.  If clisp were more flexible, then the readable library could use that flexibility to inhibit these extraneous vertical bars.  At the time of this writing the clisp developers have refused to fix this.

A completely different solution, if you hate how the symbols look, is to switch to another Common Lisp implementation like sbcl.  This will also enable auto-loading of rc files as discussed above (a problem in clisp that impacts QuickLisp).


Using sweet-expressions (t-expressions) 
=======================================

Sweet-expressions start with neoteric-expressions and in addition infers parentheses from indentation.  You can enable neoteric-expressions by running:

    (readable:enable-sweet)

In sweet-expressions:

- An indented line is a parameter of its parent, and later terms on a line are parameters of the first term.
- A line with exactly one term, and no child lines, is simply that item; otherwise those terms and its child lines are themselves a new list.
- Lines with *only* a ;-comment, and nothing else, are completely ignored - even their indentation is irrelevant.
- Whitespace-only lines at the beginning of a new expression are ignored, but a whitespace-only line (including a zero-length line) ends an expression.

Here's a notional example, with a sweet-expression on the left and its equivalent traditional s-expression on the right (this won't *run* if you type it in, since we have no function called "a"):

    a b c d        =>   (a b c d
      e f g                (e f g)
      h i                  (h i)
      j k                  (j k
        l m                   (l m)
        n o                   (n o))
      p                    p
      q r s                (q r s))

So, just type in your expression, and type a blank line (an extra Enter) to indicate that you're done.  Don't indent the first line of an expression; we'll discuss what that does in a moment.

Here's a trivial *executable* example; type this in, and enter a blank line (*Enter* *Enter*) to calculate it:

    subseq "Hello"
      1
      3

Be sure to type *Enter* *Enter* (a blank line) to execute the expression.

What happens if the parameters are not constants, but something to be calculated? No problem, just put them on new lines and give them parameters. If something has parameters, then it must be something to calculate too! Here's another example (be sure to consistently indent the lines after the first one):

    subseq
      "Hello"
      1
      length "xyz"

You can use parentheses, too; inside any grouping characters (...), [...], and {...}, indentation is ignored:

      subseq
        "Hello"
        1
        length("xyz")

Here are some other valid sweet-expressions:

    if {7 < 5}
      {3 + 4}
      {5 * {2 + 3}}
    
    abs{0 - 5}

Here's a more substantial example:

    defun fibfast (n)
      if {n < 2}       ; Indentation, infix {...}
        n              ; Single expr = no new list
        fibup n 2 1 0  ; Simple function calls
    
    defun fibup (max count n1 n2)
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

Here's another example:

    defun factorial (n)
      if {n <= 1}
         1
         {n * factorial{n - 1}}

Note that infix is easily expressed with {...}.  You can call a function using an infix expression with f{...}, a common circumstance.  Naturally, this is simply a cleaner representation of:

    (defun factorial (n)
      (if (<= n 1)
          1
          (* n (factorial (- n 1)))))

Notice that even this trivial example must have *5* closing parentheses in traditional Lisp notation; real Lisp programs have many more.  In contrast, the sweet-expression notation automatically closes most of them, and the few left over are easily matched by eye.

Trying things out
-----------------

If you're not sure what something means, you can "quote" it so it won't execute.  If you type ' followed by space, the indentation processing continues (starting at the ' mark indentation) but the whole thing will be quoted.  That way you can try things out!  For example:

    ' foo bar1 bar2
        spam eggs1 eggs2

Will produce this (if readtable-case is :invert):
(foo bar1 bar2 (spam eggs1 eggs2))

This works with the other standard abbreviations (backquote, comma, and comma-at): use them at the beginning of a line and follow them with space, and they apply to the entire sweet-expression that follows.  Otherwise, they only apply to the next neoteric expression.


Advanced sweet-expression features
==================================

The goal of sweet-expressions is to easily and cleanly express programs and data.  The basics work well, but experience with them suggested that certain abbreviations would make them even more pleasant to use.  So, here are those advanced features, along with more details about using them.   We've tried to think ahead, to make sure you can use sweet-expressions to easily and clearly express things.


Grouping and Splitting
----------------------

Sometimes you want to have a parameter that is a list of lists, or where the function to be called is in fact determined by another calculation.   Also, a basic indentation processor can sometimes require an excessive amount of vertical space, or it can make it hard to show the relationship between items at the same list level.  The \\\\ marker can help resolve this.

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


Basically, at the beginning of line (but after indentation) "\\\\" maps into a null function name, so you can use forms like "let" easily.  The \\\\ marker is also useful, when first on a line, to show that this line is logically subordinate to the previous line, but nevertheless it is at the same list level. For example:

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

If you have small items that need to be at the same list level, you can combine them on one line and separate them with \\\\.  Basically, if \\\\ is not the first item on the line, it acts as if you split the line and restarted at the current indentation.  This is useful, for example, with keyword symbols:

    foo
      :amount \\ 100 \\ :from \\ ocean

=>

    (foo :amount 100 :from ocean)

But what if you actually want to refer to "\\\\"?  No problem, just use the expression "{\\\\}".  Common Lisp, and some other lisps, supports "slashification" (e.g., if "\" followed by any character means that character is part of a symbol).  But this symbol \\\\ is chosen so that it can be written in any Lisp.  If that Lisp doesn't use slashification, \\\\ means the symbol with a two-character name \\\\.  If it uses slashification, \\\\ means the symbol with the one-character name \\.  The good news is that you can use the marker \\\\ on practically any Lisp system, so you don't need to constantly change notation if you use different ones.

The readable-lisp implementation also supports a special line continuation extension.  If there's at least one expression on the line, and the line ends with "\\\\", then the "\\\\" is considered a line continuation.  That is, the line continues as if the newline never occurred.  This is similar to how many other languages handle "\\" at the end of a line.  The following line has to have the same indentation as the previous line (the line with the "\\\\" at the end).  You can keep doing this for as many lines as you'd like.  This extension is not part of the sweet-expression specification in SRFI-110, it's simply a permitted extension, but in Common Lisp this extension can be useful.  It has been added to the Scheme implementation for consistency with the Common Lisp implementation.


Sublist
-------

Sweet-expressions support another special abbreviation when not inside ( ... ), [ ... ], or { ... }.  A $ (aka SUBLIST) in the middle of list restarts list processing; the right-hand-side (including its sub-blocks) is the last parameter of that line's left-hand side list.  Even if there's only one n-expression on the left-hand-side, it's still considered a list (in this case, of one element).  This is really handy when you need to chain functions together, e.g., where a sequence of functions process in sequence the output of something more complex.  For example:

    onfail $ run $ head -10 "README"

has the same meaning as:

    (onfail (run (head -10 "README")))

This abbreviation is copied from Haskell.

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

Indent characters
-----------------

You can indent using one or more of the indent characters, which are space, tab, and the exclamation point (!). Lines after the first line need to be consistently indented, that is, the current line’s indentation, when compared to the previous line’s, are equal or one is a prefix of the other. Indentation processing does not occur inside ( ), [ ], and { }, whether they are prefixed or not; this makes sweet-expressions backwards-compatible with traditional s-expressions, and also provides an easy way to disable indentation processing if it’s inconvenient. 

These rules eliminate many concerns people have with indentation systems.  One objection that people raise about indentation-sensitive syntax is that horizontal whitespace can get lost in many transports (HTML readers, etc.). In addition, sometimes there are indented groups that you’d like to highlight; traditional whitespace indentation provides no opportunity to highlight indented groups specially.  By allowing "!" as an indent character (as well as space and tab), these problems disappear.  Also, some like to use spaces to indent; others like tabs. Python allows either. Sweet-expressions also allow people to use spaces, tabs, or a combination, as long as you are consistent, so developers can use whatever they prefer.

If you indent the *first* line, indentation processing is ignored; the system just reads the first expression (ignoring indentation) and returns it.  So if the line begins with indentation followed by "a b c", it will return "a" the first time (and "b" on the second, and "c" on the third).  This mechanism, called "initial indent", helps with backwards-compatibility; if an s-expression begins with indentation, it's clearly not a sweet-expression.  The goal is to make it easy to just have sweet-expressions enabled at all times, since very little code will be interpreted differently.


Convenience programs "sweet-clisp" and "sweet-sbcl"
===================================================

For your convenience, the full package includes "sweet-clisp" and "sweet-sbcl" (if you have clisp or sbcl respectively), which automatically put you in sweet-expression mode (the last tier) for that implementation of Common Lisp.  They are especially convenient for interactive use; you can also use them to run scripts in sweet-expression notation.  In both you can enter symbols in lowercase and see them in lowercase (they implement this by inverting the readtable-case, as discussed above).

The sweet-clisp will also work around some bugs and problems in clisp.   In particular, it works around another bug in the clisp REPL.  In the clisp standard REPL any top-level sweet-expressions (after the first one) that are not initially indented must be preceded by a  blank line.   If they aren't, then the first line of the second sweet-expression would be skipped (they are consumed by the REPL reader).   Note  that  this bug does not affect files run by clisp, re-implementations of the REPL on clisp, or other Common Lisp  implementations  (e.g.,  SBCL  doesn't have this problem).  The work-around over‐rides some system function definitions when the REPL  is  invoked, so it will trigger some warnings about "redefining functions".  If  you do not want the work-around performed, use the "-CREPL" option, which forces this program to use the standard  clisp  REPL (without a work-around) instead.  The REPL work-around is irrelevant for programs run directly without the REPL, obviously.  It also tries to work around the symbol vertical bar display, as discussed above.

The "sweet-clisp" program, if you download the entire package, by default uses write-readable to display REPL results. The variable \*repl-write-readable\* controls this; by default it is t, so write-readable is used; if it is nil, then the standard write routine is used.

Installing via QuickLisp doesn't install the full package, so users who *only* install using QuickLisp won't have working versions of tools such as sweet-clisp, sweet-sbcl, sweeten, or unsweeten.


Sweeten: Translating S-expressions into Sweet-expressions
=========================================================

We also have a filter called "sweeten" that translates existing S-expressions into sweet-expressions (this is the *reverse* of unsweeten).  You can use sweeten to help you understand sweet-expressions, or to move an existing program to sweet-expressions.

Just put some S-expressions into a file and you can have them translated.  For example, you could the following in file "demo.scm":

    (defun doubleadd (a b)
        (* (+ a b) 2))


You can run the sweetener by typing:

    ./sweeten -C < demo.scm

And you should see this as output:

    defun doubleadd (a b) {{a + b} * 2}

You can also run it interactively, but you may find that you want to press an extra "Enter" after your expression.  (It's done this way so that when it translates files they look correct.)

When processing Common Lisp code, be sure to use the "-C" option for sweeten.  This tells sweeten to use Common Lisp notation, such as #'function, instead of Scheme (its default).  Sweeten does have limitations.  First, it is written in Scheme, so its handling of Common Lisp is far from perfect.  There is also a general limitation of the sweeten implementation: Comments inside any parentheses will not be produced in the output.  Still, it's a useful tool.

Readable library
=========================

The "readable" library comes with some other functions you can call.

Reading functions
------------------

Various functions let you read using a specific notation:

*   curly-infix-read: Read a curly-infix-expression datum.
*   neoteric-read: Read a neoteric-expression datum.
*   sweet-read: Read a sweet-expression datum.


Writing readable expressions
----------------------------

Version 1.0.5 adds additional functions in Common Lisp to print expressions using these readable notations.  That way, you can easily print these notations as well as read them.  Their interfaces are intentionally similar to the standard Common Lisp functions, so they should be easy to use.

Function "write-readable" writes out its first parameter in a readable notation, similar to the standard function write.  It takes all the optional parameters of write (such as :stream), plus the optional ":notation" parameter for controlling the output notation.  By default, the output notation is the same as the input notation.  The ":notation" parameter can be 'basic-curly-infix, 'full-curly-infix, 'neoteric, or 'sweet.  "Write-readable" will always use at least basic-curly-infix notation.  Circularity detection is available; use ":circle t" to enable it.  It also includes similar functions print1-readable, princ-readable, and print-readable.  You can write to strings instead of the current output with write-to-string-readable, prin1-to-string-readable, and princ-to-string-readable.

The current implementation directly supports circularity detection in cons cells.  The implementation directly supports the following Common Lisp types: cons, symbol, number, character, pathname, string, and bit-vector.  Note that the "cons" is fully supported, which means that proper lists, dotted lists, and circular lists are all supported. Other types are currently partly supported by calling the underlying "write" implementation; this includes the types array (including vector), hash-table, function, readtable, package, stream, random-state, condition, and restart, as well as those created by defstruct, define-condition, or defclass.  In most cases this partial support is more than enough, but you should be aware of its limitations.   First, the contents of partially-supported types will be presented in traditional Lisp notation instead of a more readable notation (though it will still be a valid format).  Also, if you use circularity detection, the circularity detection in any partially-supported types will be separate and will not synchronize with the detection in fully-supported types. There are merely limitations of the current implementation, not of the fundamental concept.  Patches are welcome!

Here are some examples, presuming that you have set readtable-case to invert, use-package(:readable), and that the current notation is neoteric or sweet:

    write-readable '(+ 1 2)       ; Writes {1 + 2}
    write-readable '(+ 1 (* 3 4)) ; Writes {1 + {3 * 4}}
    write-readable '(cos x)       ; Writes cos(x)
    write-readable '(log 10 100)  ; Writes log(10 100)
    write-readable '(cos (* 2 x)) ; Writes cos{2 * x}

As noted earlier, the clisp implementation of "write" normally displays symbols in a really ugly way when the readtable includes letters (as it does when neoteric or sweet-expression reading is active).  The "write-readable" and related routines work around this problem as well.


Syntax for basic writing functions
-----------------------------------

**write-readable** object &key array base case circle escape gensym length level lines miser-width pprint-dispatch pretty radix readably right-margin stream notation => object

**prin1-readable** object &optional output-stream => object

**princ-readable** object &optional output-stream => object

**print-readable** object &optional output-stream => object


Arguments and values are the same as for write, with the addition of "notation" in some cases.
write-readable is the general entry point to the readable Lisp printer.  The "notation" corresponds to \*print-notation\*.

prin1-readable produces output suitable for input to read. It binds \*print-escape\* to true.

princ-readable is just like prin1 except that the output has no escape characters. It binds \*print-escape\* to false and \*print-readably\* to false. The general rule is that output from princ is intended to look good to people, while output from prin1 is intended to be acceptable to read.

print-readable is just like prin1-readable except that the printed representation of object is preceded by a newline and followed by a space.

Syntax for string writing functions
-----------------------------------

**write-to-string-readable** object &key array base case circle escape gensym length level lines miser-width pprint-dispatch pretty radix readably right-margin => string

**prin1-to-string-readable** object => string

**princ-to-string-readable** object => string

write-to-string-readable, prin1-to-string-readable, and princ-to-string-readable are used to create a string consisting of the printed representation of object in readable notation. Object is effectively printed as if by write, prin1, or princ, respectively, and the characters that would be output are made into a string.

write-to-string-readable is the general output function.  prin1-to-string acts like write-to-string with :escape t.  princ-to-string acts like write-to-string with :escape nil :readably nil. As a result, no escape characters are written.


What's the big deal?
====================

If you aren't familiar with Lisp, you may say "what's the big deal"? After all, this looks a lot like traditional languages. Many have commented that it looks like Python, with its use of indenting, and of course nearly all other languages use infix notation.

But that's the point - the results look much more familiar (and thus are more acceptable to non-Lispers), but all of Lisp's more exotic capabilities still work. You can use techniques like quoting (') and quasi-quoting (\`) with lifting (,), which enable powerful capabilities. Many people have created "infix" notations with Lisp-like languages before, but they all failed to work with many other Lisp features. We think this approach succeeds instead, where others before have failed.


Closing Remarks
===============


*RECAP*: If you use QuickLisp (and are willing to require your users to use it), start your files with:

    (ql:quickload "readable")
    (readable:enable-sweet) ; or whichever notation you've chosen.

If you use ASDF directly, start with:

    (require "asdf")
    (asdf:load-system :readable)
    (readable:enable-sweet) ; or whichever notation you've chosen.

In either case, at the end of each file you can insert:

    (readable:disable-readable)

You would typically load other modules before actually enabling a notation, to prevent the unlikely case that they'd be interpreted differently.  You might also set the readtable-case to :invert before enabling a notation, so normal symbols are displayed in lower case; you can do this using:

    (setf (readtable-case *readtable*) :invert)

The programs sweet-clisp and sweet-sbcl set up sweet-expression readers for those two implementations of Common Lisp, for your convenience.

Although we discussed some specific implementations, these notations can be used with any Lisp-based system.  You can even use the "unsweeten" tool to translate arbitrary Lisp text into traditional s-expression notation, so you could use sweet-expressions even with Lisp-based languages other than Common Lisp.

These notations can take a few minutes to learn how to use, just like anything else new, but we believe they are worth it.  We hope you like them and eventually believe it too.

