As discussed in the [Problem] page, many developers find standard Lisp notation hard to read.  Early Lisp was even harder to read, so abbreviations were added; one example is being able to abbreviate (quote x) as 'x.  Our approach is to add *additional* abbreviations for common cases, so that expressions in any Lisp-based language will be even easier to read.  Well-formatted S-expressions should work as-is, but if you use our abbreviations, the result should be easier for humans to understand.

Unlike most past efforts to make Lisp more readable, our approach is **generic** (the notation does not depend on an underlying semantic) and **homoiconic** (the underlying data structure is clear from the syntax).  We believe these are necessary conditions for a readable Lisp notation.  Previous efforts, like McCarthy's M-expressions, failed because they lacked these properties.

Readable abbreviation specification
===================================

We have three notation tiers, each of which builds on the previous one. Curly-infix adds support for traditional infix notation; neoteric-expressions add support for traditional function notation; and sweet-expressions reduce the number of explicit parentheses needed (by deducing them from indentation).  Here's the complete specification of these abbreviations (where "&rArr;" means "maps to"):

1.   *Curly-infix-expressions* (*c-expressions*): Curly braces {...} contain an *infix list*.
    * A *simple* infix list {a op b op c op ...} represents one operation in infix order, that is, (op a b c ...).  It has (1) an odd number of parameters, (2) at least 3 parameters, and (3) all even parameters are the same symbol (aka eq? or eq).  It maps to "(even-parameter odd-parameters)".  E.g., {n <= 2} &rArr; (<= n 2), and {7 + 8 + 9} &rArr; (+ 7 8 9).
    * The *empty* {} maps to (), the *escaping* {e} maps to e, and the *unary-op* {e1 e2} maps to (e1 e2).  Thus, {$} &rArr; $, and {- x} &rArr; (- x).
    * Curly-infix lists beginning with the "." symbol have an unspecified mapping.
    * Other infix lists are mixed and map to "($nfx$ parameters)".  E.g., {2 + 3 * 4} &rArr; ($nfx$ 2 + 3 * 4)
    * By intent, there is no precedence; just use another list. E.g., {2 + {3 * 4}} &rArr; (+ 2 (* 3 4))
    * There are two variations of curly-infix, "basic" and "full".  In "basic" c-expressions, each element (in a regular or curly-infix list) is another basic c-expression.  In "full" c-expressions, each element is a neoteric-expression, as defined below; that means inside {} you can use f(x) as (f x).
    * Full c-expressions are defined for Scheme in [SRFI 105](http://srfi.schemers.org/srfi-105/).
2.   *Neoteric-expressions* (*n-expressions*): This includes curly-infix-expressions, and adds special meanings to some prefixed expressions.
    * An e(...) maps to (e ...).  E.g., f(1 2) &rArr; (f 1 2), exit() &rArr; (exit), and read(. port) &rArr; (read . port).
    * An e{} maps to (e), otherwise, e{...} maps to (e {...}). E.g., f{n - 1} &rArr; (f {n - 1}) &rArr; (f (- n 1)), and g{- x} &rArr; (g (- x)).
    * An e\[...] maps to ($bracket-apply$ e ...)
    * In the above, "e" is any datum expression. There must be no whitespace between e and the open paired character.
    * An unprefixed "( . e)" must evaluate as "e".
    * These recurse left-to-right.  E.g., f{n - 1}(x) &rArr; f({n - 1})(x) &rArr; (f (- n 1))(x) &rArr; ((f (- n 1)) x)
3.   *Sweet-expressions* (*t-expressions*): Includes neoteric-expressions, and deduce parentheses from indentation. Here are the basics:

    - A line with content consists of one or more n-expressions, separated by one or more spaces or tabs.
    - If a line is indented more than the previous line, that line is a child line, and the previous line is a parent to that child.
    - Later lines with the same indentation as the child are also children of that parent, until there is an intervening line with the parent’s indentation or less.
    - A line with only one n-expression, and no child lines, represents itself. Otherwise, the line represents a list; each n-expression on the line is an element of the list, and each of its child lines represents an element of the list (in order).
    - An empty line ends the expression; empty lines *before* expressions are ignored.
    - Indentation processing does not occur inside ( ), [ ], and { }, whether they are prefixed or not; they're just neoteric-expressions.  This makes sweet-expressions highly backwards-compatible.

    Sweet-expression rule clarifications:

    - Lines with only a ;-comment are completely ignored - even their indentation (if any) is irrelevant.
    - You can indent using one-or-more space, tab, and/or exclamation point (!) characters.
    - A line with only spaces and tabs is an empty line.  A line with at least one "!" is ignored if it only has indent characters.
    - An expression that *starts* indented enables "indented-compatibility" mode, where indentation is completely ignored (that line switches to neoteric-expressions).


    Sweet-expressions also include these advanced capabilities:

    - A \\\\ (aka SPLIT) starts a new line at the current indentation.  If it's immediately after indentation (aka GROUP in that case), it represents no symbol at all (at that indentation) - this is useful for lists of lists.
    - A $ (aka SUBLIST) in the middle of list restarts list processing; the right-hand-side (including its sub-blocks) is the last parameter of the left-hand side (of just that line). If there's no left-hand-side, the right-hand-side is put in a list.
    - A leading traditional abbreviation (quote, comma, backquote, or comma-at) or datum comment "#;", at the beginning of a sweet-expression line, and followed by space or tab or end-of-line, is that operator applied to the following sweet-expression.   Otherwise, it applies to the next neoteric-expression.
    - The markers &#8220;&lt;*&#8221; and &#8220;*&gt;&#8221; surround a <i>collecting list</i>, and <em>MUST</em> accept a list of 0 or more un-indented sweet-expressions.
    - The marker &#8220;$$$&#8221; is reserved for future use.

    Sweet-expression examples are shown below.  For Scheme, sweet-expressions are defined in [SRFI 110](http://srfi.schemers.org/srfi-110/).

See [Rationale] for why these rules are the way they are, and [Retort] if you were told that Lisp's s-expression notation can't be improved on.

Beginning an expression with indentation causes that line's indentation to be ignored, improving backwards compatibility.  We recommend that editors highlight these lines as warnings, to reduce the risk of their accidental use.  It might be also useful for an editor to highlight blank lines (as they separate expressions) and lines beginning at the left column.

Individual implementations may have *additional* abbreviations that are useful for their semantics; our goal is to devise general abbreviations that others can build on if they choose.

This is version 1.0 of our notation specification.


Quick Examples
==============

Here are some examples of the result, as well as what they map to (which is what you would have had to type before):

<table cellpadding="4" border="1" rules="cols">
<tr>
<th align="center">Sweet-expression</th>
<th align="center">(Awkward) S-expression</th>
</tr>
<tr>

<td align="left" valign="top">
<pre>
define fibfast(n)
  if {n &lt; 2}
    n
    fibup(n 2 1 0)
</pre>
</td>
<td align="left" valign="top">
<pre>
(define (fibfast n)
  (if (&lt; n 2)
    n
    (fibup n 2 1 0)))
</pre>
</td>
</tr>

<tr>
<td align="left" valign="top">
<pre>
define fibup(max count n1 n2)
  if {max = count}
    {n1 + n2}
    fibup max {count + 1} {n1 + n2} n1
</pre>
</td>
<td align="left" valign="top">
<pre>
(define (fibup max count n1 n2)
 (if (= max count)
  (+ n1 n2)
  (fibup max (+ count 1) (+ n1 n2) n1)))
</pre>
</td>
</tr>

<tr>
<td align="left" valign="top">
<pre>
define factorial(n)
  if {n &lt;= 1}
    1
    {n * factorial{n - 1}}
</pre>
</td>
<td align="left" valign="top">
<pre>
(define (factorial n)
  (if (&lt;= n 1)
    1
    (* n (factorial (- n 1)))))
</pre>
</td>
</tr>
</table>

Notice that infix operations and function calls are much easier to read, and notice how much easier it is to read when there are fewer parentheses.  Now you don't need to use indentation tools to keep the code indented correctly; the indentation is part of the code itself.

More Examples
==============

See the [Examples] page for many more examples, including with Lisp macros (they work just fine).

The code distribution comes with "sweeten.sscm"; this is a program that translates traditional S-expressions into sweet-expressions, and is itself written using sweet-expressions.

"Letterfall" by Alan Manuel Gloria is a game written using sweet-expressions; you can see it here: https://github.com/AmkG/letterfall


Credits
=======

*   David A. Wheeler was the initiator of the "readable" project.  He created the initial three tiers, and developed first drafts of the specifications and code.
*   Alan Manuel K. Gloria made a number of specification suggestions (e.g., adding the SUBLIST "$") and wrote significant pieces of the implementation.
*   Egil Möller developed SRFI-49, an indentation semantic for Scheme and sample implementation. The sweet-expression indentation semantics and implementation are based on this.

Also, thanks to the many other mailing list participants who provided feedback.


Trying them out
=============

We have two tutorials so you can try them out:

* [Scheme-tutorial] - Scheme-focused tutorial
* [Common-lisp-tutorial] - Common-Lisp-focused tutorial

We think you'll like them once you try them.

