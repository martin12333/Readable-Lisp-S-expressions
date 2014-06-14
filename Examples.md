Here are a few examples of sweet-expressions (which include modern-expressions and curly infix), as defined in our [Solution] to the [Problem].  The first few use Scheme; they are followed by a Common Lisp example.  This section closes with pointers to other materials.

# Fib and fact

First, here are the examples we often use to show off sweet-expressions, using Scheme:

<pre>
    define fibfast(n)  ; Typical function notation
      if {n < 2}       ; Indentation, infix {...}
        n              ; Single expr = no new list
        fibup(n 2 1 0) ; Simple function calls
</pre>

<pre>
    define fibup(maxnum count n-1 n-2)
      if {maxnum = count}
        {n-1 + n-2}
        fibup maxnum {count + 1} {n-1 + n-2} n-1
</pre>

<pre>
    define factorial(n)
      if {n <= 1}
        1
        {n * factorial{n - 1}} ; f{...} => f({...})
</pre>

Note that you can use traditional math notation for functions; `fibfast(n)` maps to `(fibfast n)`. Infix processing is marked with { ... }; `{n <= 2}` maps to `(<= n 2)`. Indentation is significant, unless disabled by ( ... ), [ ... ], or { ... }. This example uses variable names with embedded "-" characters; that's not a problem, because the infix operators must be delimited (e.g., by whitespace and are only used as infix operators when { ... } requests them.

It's actually quite common to have a function call pass one parameter, where the parameter is calculated using infix notation. Thus, there's a rule to simplify this common case (the prefix {} rule). So factorial{n - 1} maps to factorial({n - 1}) which maps to (factorial (- n 1)).

Credit where credit is due: The Fibonacci number code is loosely based on [an example by Hanson Char](http://hansonchar.blogspot.com/2006/01/fibonacci-numbers-in-scheme.html).

# GCD

Here's another trivial Scheme program, a greatest common divisor function straight from Carl A. Gunter's "Semantics of Programming Languages" page 2:

<pre>
    (define (gcd x y)
      (if (= y 0)
        x
        (gcd y (rem x y))))
</pre>

A sweet-expression reader could accept the above, but this can be rewritten more clearly using sweet-expressions as:

<pre>
    define gcd(x y)
      if {y = 0}
        x
        gcd y rem(x y)
</pre>



Macros (Reimplementation of "let")
==================================

Everyone **knows** macros need s-expressions.  Or do they?  We think not!  Here's an example implementation of Scheme's `let*`:

<table>
<tr>
<th align="center">Sweet-expression</th>
<th align="center">(Awkward) S-expression</th>
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


# Fixnum

Here's a larger example in Scheme, reformatted from the example in the Scheme Fixnum book:

<pre>
    define solve-kalotan-puzzle
      lambda ()
        let
          \\
            parent1         amb('m 'f)
            parent2         amb('m 'f)
            kibi            amb('m 'f)
            kibi-self-desc  amb('m 'f)
            kibi-lied?      amb(#t #f)
          assert
            distinct? list(parent1 parent2)
          assert
            if eqv?(kibi 'm)
               not kibi-lied?
          assert
            if kibi-lied?
               xor
                 {eqv?(kibi-self-desc 'm) and eqv?(kibi 'f)}
                 {eqv?(kibi-self-desc 'f) and eqv?(kibi 'm)}
          assert
            if not(kibi-lied?)
               xor
                 {eqv?(kibi-self-desc 'm) and eqv?(kibi 'm)}
                 {eqv?(kibi-self-desc 'f) and eqv?(kibi 'f)}
          assert
            if eqv?(parent1 'm)
               and
                 eqv?(kibi-self-desc 'm)
                 xor
                   {eqv?(kibi 'f) and eqv?(kibi-lied? #f)}
                   {eqv?(kibi 'm) and eqv?(kibi-lied? #t)}
          assert
            if eqv?(parent1 'f)
               {eqv?(kibi 'f) and eqv?(kibi-lied? #t)}
          list parent1 parent2 kibi

    solve-kalotan-puzzle()
</pre>

# Add-if-all-numbers

Here's a Scheme example from Wikipedia - specifically the article "Scheme (programming language)". This example adds an arbitrary list of numbers, and if a non-numeric value is found in the list the procedure is aborted immediately and the constant value \#f (false) is returned. This is achieved by capturing the current continuation in the variable exit and using it as an "escape procedure".

First, here's the original Scheme:

<pre>
    (define (add-if-all-numbers lst)
      (call/cc
       (lambda (exit)
         (let loop ((lst lst) (sum 0))
           (if (null? lst) sum
             (if (not (number? (car lst))) (exit #f)
               (+ (car lst) (loop (cdr lst)))))))))
</pre>

Here's the same thing using sweet-expressions.  You could indent exactly as above, but if you have a marching series of increasing indentation, the "$" can help as shown here:

<pre>
     define add-if-all-numbers(lst)
       call/cc $ lambda (exit)
         let loop (lst(lst) sum(0))
           if null?(lst)
             sum
             if not(number?(car(lst)))
               exit #f
               {car(lst) + loop(cdr(lst))}
</pre>

The git repository https://sourceforge.net/p/readable/code/ has additional examples.  The src/sweeten.sscm has the first significant program ever written using sweet-expressions, and the "examples/" directory has additional small examples.




# Decision Learning Example (Common Lisp)

The notation is not limited to Scheme.  Here's [some Decision tree learning code](http://www.cs.cmu.edu/afs/cs/project/theo-11/www/decision-trees.lisp) in Common Lisp that accompanies the textbook "Machine Learning," Tom M. Mitchell, McGraw Hill, 1997; a longer fragment is in [Analysis].  It's been rewritten using sweet-expressions:

<pre>
    defun print.tree (tree &optional (depth 0))
      tab depth
      format t "~A~%" first(tree)
      loop for subtree in cdr(tree) do
        tab {depth + 1}
        format t "= ~A" first(subtree)
        if atom(second(subtree))
          format t " => ~A~%" second(subtree)
          progn
            terpri()
            print.tree second(subtree) {depth + 5}
</pre>

<pre>
    defun tab (n)
      loop for i from 1 to n do format(t " ")
</pre>

<pre>
    defun classify (instance tree)
      let
        $ val branch
        if atom(tree) return-from(classify tree)
        setq val get.value(first(tree) instance)
        setq branch second(assoc(val cdr(tree)))
        classify instance branch
</pre>


Larger programs
===============

The code distribution comes with "sweeten.sscm"; this is a program that translates traditional S-expressions into sweet-expressions, and is itself written using sweet-expressions.

"Letterfall" by Alan Manuel Gloria is a game written using sweet-expressions; you can see it here: https://github.com/AmkG/letterfall

Other pages
===========

The [Analysis] page shows many more examples, in many different Lisp variants.

The page [Hunchentoot-make-docstrings] shows a longer Common Lisp example.

See [Install-howto] for how to install our tools and code that implements this.

See [Common-lisp-tutorial] and [Scheme-tutorial] for our Common Lisp and Scheme tutorials.
