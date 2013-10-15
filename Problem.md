Many people find Lisp's s-expressions hard to read as a programming notation, and thus reject them both.  As noted below, even Lisp luminary Paul Graham agrees that Lisp notations have readability problems!  Yet s-expressions have a lot of benefits to them, and it turns out to be very hard to design a more readable notation that retains all the benefits of s-expressions.

David A. Wheeler, initiator of this "readable" project, has developed Lisp programs for decades.  Though he can read s-expressions well, he is still dissatisfied with their syntactic limitations. He believed it is possible to do better - and hopes that others will help make easier notations a reality.

Below is an introduction to the problem, some quotations that show that people really do find current s-expression notation a problem, and a brief discussion as to why past solutions haven't worked.

You can skip to our [Solution] if you already understand the problem.

# Introduction

Lisp-derived systems normally represent programs as s-expressions.  S-expressions can represent a list of items by surrounding the list with parentheses (...) and separating each item by at least one whitespace, e.g., "(1 2 3)".  When using S-expressions to define programs, the operation to be performed is listed first, followed by its parameters.  Thus, you would write "(f 1 2 3)" to invoke function "f" with the list of parameters "(1 2 3)".

Note that you cannot write “2+3” with traditional s-expressions - instead you write “(+ 2 3)”. You don't even write "f(1 2 3)" - the function's name is inside the list!  Most software developers find this nonstandard prefix notation hard to read.   In addition, many definitions end up with a huge number of nested parentheses, since doing almost anything creates another list.   Even if you are used to this, and have editing tools that help, this is a problem when trying to collaborate with others - it's ugly, and many developers will refuse to use such a clumsy notation.


# Quotations

In fact, a lot of people have commented about the lack of readability; here are a few quotes:

*   <i>I've used Lisp my whole programming life and I still don't find prefix math expressions natural.</i> - <a href="http://paulgraham.com/popular.html">Paul Graham</a>
*   <i>When you program, you spend more time reading code than writing it... a language that makes source code ugly is maddening to an exacting programmer, as clay full of lumps would be to a sculptor.</i> - <a href="http://www.paulgraham.com/pypar.html">Paul Graham</a>
*   <i>I have more faith that you could convince the world to use esperanto than \[to use] prefix notation.</i> - <a href="http://people.csail.mit.edu/gregs/ll1-discuss-archive-html/msg01571.html">Paul Prescod</a>
*   <i>Lisp has all the visual appeal of oatmeal with fingernail clippings mixed in.</i> - <a href="http://www.linuxjournal.com/article/2070">Larry Wall</a>
*   <i>After 13 years of doing Lisp and 3 or 4 years of Python, I agree: I prefer writing Lisp, but Python is easier to read. </i> - <a href="http://lemonodor.com/archives/001497.html">John Wiseman</a>
*   <i>LISP: ... mythically from ‘Lots of Irritating Superfluous Parentheses’</i> - <a href="http://www.catb.org/jargon/html/L/LISP.html">Jargon File</a>
*   <i>...  and don't ask me about the extraneous parenthesis.  I bet some LISP programmer felt alone and decided to make it a bit more homey.</i> - <a href="http://fortunes.cat-v.org/kernelnewbies/">Linus Torvalds</a>
*   <i>\[If only\] we could find characters or signs suited for expressing all our thoughts as clearly and as exactly as arithmetic expresses numbers...</i> - Gottfried Wilhelm Leibniz, 1677. <!-- Preface to the General Science, 1677. -->
*   <i>"A language should be designed in terms of an abstract syntax and it should have perhaps, several forms of concrete syntax: one which is easy to write and maybe quite abbreviated; another which is good to look at and maybe quite fancy... and another, which is easy to make computers manipulate... all should be based on the same abstract syntax... the abstract syntax is what the theoreticians will use and one or more of the concrete syntaxes is what the practitioners will use. </i> - <a href="http://www.infoq.com/interviews/Steele-Interviews-John-McCarthy">John McCarthy, creator of Lisp</a>

# Why is this unsolved?

A vast number of projects have tried to create "a more readable Lisp notation" and failed.  These include M-expressions (created by the creator of Lisp), IACL2, and Dylan.

David A. Wheeler reviewed as many of these old approaches he could find and believes he has identified why they failed.  The problem was that most past alternatives lost the advantages of s-expressions, in particular, that s-expressions are **generic** (they do not depend on some underlying semantic) and **homoiconic** (the underlying data structure is clear from the syntax).  These losses are important; Lisps are often used to process data structures that are also programs, so notations that aren't generic or homoiconic will interfere with the typical reasons that s-expressions and Lisps are used.  Now that we know why these efforts failed, we can avoid their mistakes when devising a new notation.

The "readable" project has identified ways to extend s-expressions so they can be more readable *without* losing their power.  As much as possible these improvements should be backwards-compatible, so that they will be easier to adopt.

If you are unwilling to consider that there might be a better approach, stop reading. But if you're interested in a better way, keep reading. After all, current Lisp notations did not fall from the sky.  Indeed, Lisp notation has changed over time; the quote operator ' was not in the original Lisps, and was added because that construct was so common that it was worth creating an abbreviation. We're just adding a few more abbreviations.

# Still interested?

See the [Solution] page that describes our approach to creating an easier-to-read notation for s-expressions.  It involves three incremental tiers of notations, with each tier building on top of the previous tier: curly-infix-expressions (to enable infix), neoteric-expressions (to also enable more traditional function invocation), and sweet-expressions (to also enable meaningful indentation, reducing the required number of parentheses).

If you want to try our implementation, read [Install-howto] to learn how to install the software, then look one of our two tutorials:

* [Scheme-tutorial] - Scheme-focused tutorial
* [Common-lisp-tutorial] - Common-Lisp-focused tutorial

We think you'll like our notations once you try them.
