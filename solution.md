As discussed in the [Problem] page, many developers find standard Lisp notation hard to read.  Early Lisp was even harder to read, so abbreviations were added; one example is being able to abbreviate (quote x) as 'x.  Our approach is to add *additional* abbreviations for common cases, so that expressions in any Lisp-based language will be even easier to read.  Well-formatted S-expressions should work as-is, but if you use our abbreviations, the result should be easier for humans to understand.

Unlike most past efforts to make Lisp more readable, our approach is **generic** (the notation does not depend on an underlying semantic) and **homoiconic** (the underlying data structure is clear from the syntax).  We believe these are necessary conditions for a readable Lisp notation (and their lack is why past efforts often failed).

Readable rules
==============

We have three tiers, each of which builds on the previous one:

1.   *Curly-infix-expressions* (*c-expressions*): Curly braces {...} contain an *infix list*. A *simple infix list* has (1) an odd number of parameters, (2) at least 3 parameters, and (3) all even parameters are the same symbol; it maps to "(even-parameter odd-parameters)".  Other infix lists map to "(nfx parameters)".   By intent, there is no precedence and you *must* use another {...} for an embedded infix list.
    * Example: {n <= 2} maps to (<= n 2)
    * Example: {2 * 3 * 4} maps to (* 2 3 4)
    * Example: {2 + 3 * 4} maps to (nfx 2 + 3 * 4)
    * Example: {2 + {3 * 4}} maps to (+ 2 (* 3 4))
2.   *Neoteric-expressions* (*n-expressions*): This includes curly-infix-expressions, and adds special meanings to some prefixed symbols. An e(...) maps to (e ...); an e{...} maps to e({...}); and an e[...] maps to (bracketaccess e ...), where "e" is any expression. There must be no whitespace between e and the open parenthesis. Also, an unprefixed "( . e)" must evaluate as "e".
    * Example: f(1 2) maps to (f 1 2)
    * Example: f{n - 1} maps to f({n - 1}) which maps to (f (- n 1))
    * Example: f{n - 1}(x) maps to f({n - 1})(x) which maps to (f (- n 1))(x) which maps to ((f (- n 1)) x)
3.   *Sweet-expressions* (*t-expressions*): Includes neoteric-expressions, and deduces parentheses from indentation. Basic rules:

    - An indented line is a parameter of its parent.
    - Later terms on a line are parameters of the first term.
    - A line with exactly one term, and no child lines, is simply that term; multiple terms are wrapped into a list.
    - An empty line ends the expression; empty lines *before* expressions are ignored.
    - Indentation processing does not occur inside ( ), [ ], and { }, whether they are prefixed or not; they're just neoteric-expressions.

    Sweet-expression rule refinements:

    - Lines with only a ;-comment are completely ignored - even their indentation (if any) is irrelevant.
    - A \\\\ (aka SPLIT) starts a new line at the current indentation.  If it's immediately after indentation (aka GROUP in that case), it represents no symbol at all (at that indentation) - this is useful for lists of lists.
    - A $ (aka SUBLIST) in the middle of list restarts list processing; the right-hand-side (including its sub-blocks) is the last parameter of the left-hand side.
    - A leading traditional abbreviation (quote, comma, backquote, or comma-at), followed by space or tab, is that operator applied to the sweet-expression starting at the same line.
    - You can indent using one-or-more space, tab, and/or exclamation point (!) characters.
    - A line with only indentation is an empty line.
    - If an expression *starts* indented, then indentation is completely ignored (that line switches to neoteric-expressions).

    Sweet-expression examples are shown below.

Curly-infix adds support for traditional infix notation; neoteric-expressions add support for traditional function notation; and sweet-expressions reduce the the number of explicit parentheses needed (by deducing them from indentation).  See [Rationale] for why these rules are the way they are.

Beginning an expression with indentation causes that line's indentation to be ignored, improving backwards compatibility.  We recommend that editors highlight these lines as warnings, to reduce the risk of their accidental use.

Individual implementations may have *additional* abbreviations that are useful for their semantics; our goal is to devise general abbreviations that others can build on if they choose.

We consider these "version 0.3" of these rules, though at this point we don't expect significant future changes.


Quick Examples
==============

Here are some examples of the result, as well as what they map to (which is what you would have had to type before):

<table cellpadding="4" border="1" rules="cols">
<tr>
<th align="center">Sweet-expression</th>
<th align="center">(Ugly) S-expression</th>
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

See [Examples] for many more examples.


Macros
======

Everyone **knows** macros need s-expressions.  Or do they?  We think not!  Here's an example implementation of `let*`:

<table>
<tr>
<th align="center">Sweet-expression</th>
<th align="center">(Ugly) S-expression</th>
</tr>

<tr>
<td>
<pre>
define-syntax let*
  syntax-rules ()
    \\
    ! let* ()
    !   body
    !   ...
    ! \\ begin
    ! !    body
    ! !    ...
    \\
    ! let*
    !   \\
    !     assignment
    !     assignments
    !     ...
    !   body
    !   ...
    ! \\ let (assignment)
    ! !    let*
    ! !      \\
    ! !        assignments
    ! !        ...
    ! !      body
    ! !      ...
</pre>
</td>
<td>
<pre>
(define-syntax let*
  (syntax-rules ()
    (
     (let* ()
       body
       ...)
      (begin
        body
        ...))
    (
     (let*
       (
        assignment
        assignments
        ...)
       body
       ...)
      (let (assignment)
        (let*
          (
           assignments
           ...)
          body
          ...)))))
</pre>
</td>
</tr>
</table>

Note that \\\\ as the first symbol after indentation represents "nothing".  This is helpful for creating lists of lists, by putting nothing after it.  If there are symbols after an initial \\\\, the \\\\ has no semantic meaning (nothing+something = something), but in this case it visually sets off the following symbols as being special in some way.

The "!" is an indent character, which is unusual, but notice that it can be used to visually create vertical markers to visually indicate an important group; in the above example, the pattern to match for syntax-rules has one `!` before it, while the resulting expansion has two `!` - thus giving a good visual indicator and guide.  The "!" can also be used when using a system that erases horizontal spaces and tabs (e.g., some email systems), resolving a common complaint about indentation-sensitive syntax.


Credits
=======

*   David A. Wheeler was the initiator of the"readable" project.  He created initial three tiers, and developed first drafts of the specifications and code.
*   Alan Manuel K. Gloria made a number of specification suggestions and wrote significant pieces of the implementation.
*   Egil Möller developed SRFI-49, an indentation semantic for Scheme and sample implementation. The sweet-expression indentation semantics and implementation are based on on this.

Also, thanks to the many other mailing list participants who provided feedback.


Trying them out
=============

See the [Tutorial] page for information on how you can try out these notations.  We think you'll like them.

