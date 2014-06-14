Some believe that traditional Lisp s-expression notation descended from the gods, and thus cannot be improved on. We think that's nonsense.  We believe that Lisp notation can be improved so that it retains its good properties (like homoiconicity and generality) while being easier to read. Here's a retort for our position, for those willing to consider alternatives to traditional Lisp s-expression notation.

The [perspective of people who think Lisp notation cannot be improved is well expressed in the Common Lisp FAQ](http://www.lispniks.com/faq/faq.html). So, here's a point-by-point rebuttal to that FAQ, as it existed on January 26, 2008, with the FAQ material in italics. The editor of the FAQ is Peter Seibel, who we respect; it's just that on this particular technical issue, we think he (and others) are wrong.  Let's see why we believe that, with the point-by-point rebuttal below.

What's remarkable is that much of their "evidence" actually supports our position, not theirs.  Also note that they presume solutions must have particular properties (e.g., be "Algol-like" or be incompatible with s-expression notation); we've demonstrated that this is not true.


* * * * *

<i>Q: What's up with all the parentheses? ... Lisp's most superficially obvious characteristic is its extensive use of parentheses to delimit expressions. Unfortunately many would-be Lispers get stuck on the parentheses and never get far enough into Lisp to see that they are a feature, not a bug.</i>

We disagree.  Most developers do not use Lisp for their real work, and part of the reason is that many developers perceive Lisp's poor readability (such as its overuse of parentheses) is more of a problem than Lisp's advantages.  Even some people who've gone far into Lisp believe that its syntax is a bug, not a feature.  David A. Wheeler started the readable project, after having used Lisp for over 30 years (starting in circa 1982, and having professionally using $120,000 equipment).

Lisp has advantages, sure.  But there's a reason why Lisp is widely referred to as "Lots of Irritating Silly Parentheses" (and worse).  Lots of vendors try to claim that a bug is a really feature.  But if a large number of people perceive something as a bug... it is likely to be a bug.

* * * * *

<i>.... Other folks figure that syntax is ultimately not that important and wonder why Lisp can't use a more "normal" syntax. It's not because Lispers have never thought of the idea—indeed, Lisp was originally intended to have a syntax much like FORTRAN, the predominant high-level language of the day, as John McCarthy describes in his history of Lisp3...</i>

In other words, even Lisp's creator (John McCarthy) believed that directly using s-expressions for Lisp programs was undesirable.

That is actually strong evidence that *traditional Lisp syntax has serious flaws*.  Nobody can say that Lisp's creator didn't understand Lisp.  Syntax is important if it significantly impedes readability, and Lisp notation is widely considered to be poor.  It was even considered poor by its creator.  Instead of justifying what even its creator admitted was a problem, it's time to fix it.

* * * * *

*However it wasn't simply inertia that caused Lispers to prefer S-expressions to M-expressions. People have tried, numerous times to create a more Algol-like syntax for Lisp...*

In other words, this is such a serious problem that a large number of people have tried to fix it. Again, this is evidence that *traditional Lisp syntax has serious flaws*.

Now, just because there's a problem, that does not mean there's a solution. It's actually true that lots of people have tried and failed, but we think there's a reason for it: People didn't really understand the requirements. Creating a new syntax is easy, but what people don't appreciate is that Lisp's syntax has some subtle advantages. For example, its syntax is generic (it does not imply a particular semantic) and homoiconic (the mapping of programming construct to syntactic tree is quite obvious). Previous efforts didn't realize that those were requirements, and so they failed.

But now that we understand the requirements, we can fix the problem.  We do *not* need an "Algol-like" syntax, and we are expressly *not* trying to create an "Algol-like" syntax.  What we are trying to create are clearer notations for s-expressions that are general and homoiconic; that's not the same thing as copying Algol.

* * * * *

*Guy Steele and Richard Gabriel recount the history of some of these attempts in their paper ["The Evolution of Lisp"](http://www.dreamsongs.com/NewFiles/HOPL2-Uncut.pdf) ...*

Guy Steele? The man who helped developed both Common Lisp and Scheme, but has now moved on from Lisp to lead the development of Java and later Fortress? Java and Fortress, both of which decidedly do *not* have an s-expression-based surface syntax for programming?

We agree with Guy Steele that no one language can be all things to all people. Also, Steele is a genius. But it's worth noting that although in this old paper he heaped scorn on those who tried to add Algol-like syntax to Lisp, Steele's work for many years has primarily been focused on making languages like Java and Fortress, which have a "programmer-friendly" syntax instead of Lisp's raw s-expression syntax.  Perhaps we can get better notations for Lisp-like languages, so that more people will find Lisp-based languages more acceptable in cases where they make sense.

* * * * *

*The idea of introducing Algol-like syntax into Lisp keeps popping up and has seldom failed to create enormous controversy between those who find the universal use of S-expressions a technical advantage (and don't mind the admitted relative clumsiness of S-expressions for numerical expressions) and those who are certain that algebraic syntax is more concise, more convenient, or even more natural (whatever that may mean, considering that all these notations are artificial).*

So Steele, Gabriel, and some others who argue for traditional s-expression syntax all admit that s-expressions are clumsy.  This is again evidence for our position, that traditional s-expression syntax is a *problem*.  Many people are dissatisfied with traditional Lisp notation, and even its proponents admit that it is clumsy!

Note that these authors assume that the only two possibilities are s-expressions and an "Algol-like syntax".  They do not consider a third alternative: An improved s-expression notation that is still general and homoiconic.  We advocate this third alternative.

* * * * *

*We conjecture that Algol-style syntax has not really caught on in the Lisp community as a whole for two reasons. First, there are not enough special symbols to go around. When your domain of discourse is limited to numbers or characters, there are only so many operations of interest, and it is not difficult to assign one special character to each and be done with it. But Lisp has a much richer domain of discourse, and a Lisp programmer often approaches an application as yet another exercise in language design; the style typically involves designing new data structures and new functions to operate on them - perhaps dozens or hundreds - and it's just too hard to invent that many distinct symbols (though the APL community certainly has tried). Ultimately one must always fall back on a general function-call notation; it's just that Lisp programmers don't wait until they fail.*

This "not enough special symbols" argument turns out to be irrelevant:

1.  You don't need hundreds of special symbols for any language.  Instead, we can just combine existing symbols to create compound symbols.  This already occurs in practically all programming languages already.  For example, pretty much every language today accepts "<=" as being "less than or equal to". We just don't need hundreds of new symbols.
2. More importantly, you don't need legions of special syntactic constructs.  It turns out that a relatively small set of general rules are enough to fix s-expression's problems, without hoardes of special cases. See our [Solution], which adds a few syntactic abbreviations that are **not** tied to any semantic, and do not require a massive set of special symbols.


* * * * *

*Second, and perhaps more important, Algol-style syntax makes programs look less like the data structures used to represent them. In a culture where the ability to manipulate representations of programs is a central paradigm, a notation that distances the appearance of a program from the appearance of its representation as data is not likely to be warmly received (and this was, and is, one of the principal objections to the inclusion of loop in Common Lisp).*

Here Steele and Gabriel are **extremely** insightful - which given their massive brains, is hardly surprising. We think they're exactly right; a syntax that makes programs look significantly less like the data structures used to represent them is a **problem** for a Lisp-like language.

This property is typically called "homoiconicity", and is very rare among programming languages. Lisps are one of the very few language groups that are homoiconic, and it's why Lisps are still used, decades after their development.

The whole point of a Lisp-like language is that you *can* treat code as data, and data as code. Any notation that makes this difficult means that you lose Lisp's advantages.  Homoiconicity is critical if you're going to treat a program as data. To do so, you must be able to easily "see" the program's format. If you can, you can do amazing manipulations.

Steele and Gabriel are correct in stating that there have been many efforts to create readable Lisp formats, and they all failed because they did not create formats that accurately represented the programs as data structures.  These other notations typically tried to just directly copy existing syntaxes like Fortran's or Algol's, with the notation that they would be "easier to read". They all failed.  One reason was that when the semantics changed underneath, their syntax could not easily access the new capabilities, and the constant maintenance eventually caused the approach to fail.  There is also a more general problem: It is difficult to manipulate programs-as-data when you cannot easily perceive the programs as data.

Now that we have a good diagnosis for why these previous efforts failed, we can avoid their mistakes! In short, any Lisp notation *must* be homoiconic.  We thank Gabriel, Steele, and others, who had this key insight. This insight is absolutely necessary for any future effort to even have a chance to succeed.

But what Gabriel and Steele failed to appreciate in their paper is that ***it's possible to have both homoiconicity and a more readable notation***. Unfortunately, their paper appears to quietly assume that a homoiconic notation *has* to have poor readability.  That assumption has meant that people who **could** have solved this problem in the past didn't even try. After all, why work on an impossible problem?

But now that we know why past efforts failed, we have a better chance at solving the problem. So let's solve the problem.

* * * * *

*On the other hand, precisely because Lisp makes it easy to play with program representations, it is always easy for the novice to experiment with alternative notations. Therefore we expect future generations of Lisp programmers to continue to reinvent Algol-style syntax for Lisp, over and over and over again, and we are equally confident that they will continue, after an initial period of infatuation, to reject it.  (Perhaps this process should be regarded as a rite of passage for Lisp hackers.)*

**Software developers certainly do reject it. The "it" they reject is Lisp itself.** Even if they learn Lisp-based languages, almost all of them avoid using Lisp-based languages for "real" projects. You have to read code to change it, or determine if it's correct.  Thus, for most developers, avoiding a hard-to-read notation is a wise decision.

* * * * *

*When Steele and Gabriel are talking about manipulating representations of programs, they are, of course, talking about macros. This is where the s-expression notation really shines. An Algol-style syntax is all well and good for languages that have a finite number of basic constructs - one can define a grammar that specifies how various syntactic constructs get translated into an abstract syntax tree (AST) that can then be processed by an interpreter or compiler. Open any compiler textbook and you'll learn how to write a parser that can turn the infix syntax: 1 \* 2 + 3 / 4 into the AST.... according to the precedence of the operators \*, +, and /. But in a language that supports macros we can't know, at language design time, all the possible legal language constructs. Thus we can't rely on having a grammar that knows how to translate all possible syntactic constructs into ASTs. Rather, we need a way to represent arbitrary ASTs. Which is what s-expressions are. Even if we know nothing about the precedence of \*, +, and / this s-expression: (+ (\* 1 2) (/ 3 4)) unambiguously represents the same AST shown above.  Likewise, when Lisp sees something like this: (with-whatever (something) (do-one-thing) (do-another-thing)) it can parse it into an AST without knowing anything about the internal syntax of the with-whatever construct. If with-whatever is a macro, it will be passed the AST, represented as an s-expression, and is responsible for producing a new s-expression representing the AST that should be used in place of the original with-whatever form.*

This is accurate as far as it goes, but it misses the point.

We completely agree that when we can't know at language design time all the possible legal language constructs, we need to be able to cleanly represent arbitrary ASTs. In short, it is *critically* important that a Lisp have a *general* and *homoiconic* notation.  And we agree that s-expressions are a general and homoiconic notation.

But we do **not** believe that the traditional textual form of s-expressions are the one-and-only **best** way to represent the underlying data structures that Lisp-like languages are based on, at least for programs and program-like data.  Steele and Gabriel never prove that s-expressions are the best possible ways to meet the need!  They do note important properties of s-expressions that weren't met by some other notations, and that's valuable, but their text seems to implicitly assume that no other notation could have those important properties.

The current notation for s-expressions was merely the **first** way that was created, one created in the 1950s. It wasn't even intended for use in programming!  It's just that no one created a better one, and so the Lisp programming community quickly ossified on a notation from the 1950s.  There's no reason we must stay there.

In our neoteric-expressions, the same things can be expressed as:

* {{1 * 2} + {3 / 4}}
* with-whatever(something() do-one-thing() do-another-thing())

Note that you can still "parse it into an AST without knowing anything about the internal syntax of the with-whatever construct"... so clearly traditional Lisp notation is *not* required to have a general and homoiconic notation.


* * * * *

*Q: "I have this cool idea for a new Lisp dialect / Lisp syntax that doesn't involve so many parentheses! It uses indentation to show structure!"  Since the advent of Python, this comes up on c.l.l. with some regularity. We've had 'sweet-expressions' and 'indented Lisp' and probably others.*

In other words, many people see the obvious: Lisp notation is hideously hard to read, and that syntactic use of indentation would be a big improvement.

* * * * *

*Realize a few things:*

*\* You should use an editor that makes writing Lisp easier, like Emacs or Vim. Both of these can indent based on parenthesis nesting, highlight matching parentheses, cut and paste entire blocks of code based on the parentheses, and so on. If your editor can't do all that, get a new editor.*

Yes, tools help, but tools don't fix bad notation. This answer misses the point, on two levels:

1.  Tools can make ugly notations easier to manipulate, but they don't do much where it counts: helping the humans understand the code.  Humans have to *read* code to understand it, and a notation that is difficult to read is a serious barrier. Tools can help readability somewhat (e.g., via character dimming, color-coding, etc.), but a good notation makes the workarounds unnecessary.
2.  Tools can also help easy-to-read notations, so easy-to-read notations always have the advantage over bad ones like traditional s-expressions.  You're much better off starting with a readable notation, and then using tools to help it even further. The idea that ugly notations are *required* is false, and obviously so. Every other popular programming language has exotic support in tools like emacs, vim, and Eclipse. Why settle for ugly when you can have readable **and** manipulable? To get a notation to be supported by tools, you need to have a common notation used by many programs; it does not have to be an ugly notation.

We advocate using a better notation, and developing the tools to match.

* * * * *

*Emacs users should check out parenface.el, which de-emphasizes parentheses with a diminished font color (or "face" in Emacs-speak). For instance, if your normal face is black-on-white, it'll make parentheses a shade of gray, so that they're less "in your face" while still being visible.*

Again, this is evidence **against** traditional Lisp notation, not **for** it. Bugs can hide in the text that's de-emphasized!  If you have to hide something, perhaps you shouldn't be using it.

If you have to use tools to reduce the problems of your notation, perhaps you should use a better notation, and then build tools to work with the better notation.



* * * * *

*\* Lispers already read code based on indentation, but their editors indent and re-indent code automatically. Removing the parentheses makes that harder.*

This is laughably missing the point.  If indentation were actually syntactically meaningful, you wouldn't **need** tools to fix indentation in most cases - the indentation would **already** be correct.  Tools you don't need are the easiest tools to create and maintain!

But even when you **do** need reindentation, parentheses are still not needed. You can just read in the code, and pretty-print it out... just like notations that only support parentheses. There are pretty-printers for many languages that don't require users to enter programs as S-expressions.

* * * * *

*\* After you've written and read enough Lisp, you stop seeing the parentheses. (Reports vary from a few days to a few weeks.) They don't disappear in some magical way, but you start to see the structure of the code rather than just "lots of fingernail clippings". For example, you can view (+ (\* 1 2) (/ 3 4)) as "(+ (\* 1 2) (/ 3 4))"...*

This statement is partly true, but it's also misleading. Let us explain why.

We agree that after you've written and read enough Lisp, you can mentally work around the parentheses and get to the "meat" of code. And yes, it doesn't even take that long.  David A. Wheeler, the founder of the readable project, can do it quite well. But it's an effort that is not required by other languages at all, and that's the problem.  Sure, if this was the **only** way to program a computer, or to manipulate symbols, you would gladly do it.  But other programming languages conclusively prove that you can have programming languages with better notations.

Also, ignoring parentheses breeds bugs.  The page <http://www.gregslepak.com/on-lisps-readability> shows one of the many examples of endless closing parentheses and brackets to close an expression, and the confusion that happens when indentation does not match the parentheses. bhurt's response to that article is telling: "I'm always somewhat amazed by the claim that the parens 'just disappear', as if this is a good thing. Bugs live in the difference between the code in your head and the code on the screen - and having the parens in the wrong place causes bugs. And autoindenting isn't the answer- I don't want the indenting to follow the parens, I want the parens to follow the indenting. The indenting I can see, and can see is correct."

The biggest problem, though, is working with others. Today, most developers avoid using Lisp-based languages, even when they'd be appropriate. The notation is just too hard to read. Yes, many will learn the Lisps - they're interesting, and have some very nice properties. But their bad notation overcomes their other advantages, and unnecessarily so.

Lots of people will buy Lisp books, play with Lisp, and go away with a better understanding of programming. But for the most part, they **will** go away. They will not choose to write serious programs in Lisp-based languages, even though mature tools and books are available, because they cannot easily work with others using an unreadable notation.

David A. Wheeler has been writing Lisp code for decades, and though he can read Lisp well, he still thinks Lisp's syntax is a serious problem. [John Wiseman](http://lemonodor.com/archives/001497.html) (with 13 years experience) had the same experience. So clearly there are people with long experience with Lisp, who nevertheless don't find raw S-expressions all that loveable.

The FAQ hints at a quote, but it doesn't give the full quote nor who said it. Here's the full quote: "Lisp has all the visual appeal of oatmeal with fingernail clippings mixed in." What's funny is that this is by Larry Wall, the creator of Perl. Perl itself has a reputation for being an ugly, write-only language! If even the creator of **Perl** finds Lisp syntax too ugly, and Perl itself has a reputation for ugly code, that should tell you something - Lisp must be *really* ugly.

S-expressions require users to represent math notations using this format:

    (+ (* 1 2) (/ 3 4))

Note that curly-infix-expressions allow users to write it this way:

    {{1 * 2} + {3 / 4}}

This is a big deal, but why it's a big deal isn't immediately obvious.  It's a big deal because this notation does **not** depend on any precedence rules, or registration.  It also doesn't depend on knowing what the final language is (or which operators are infix). Instead, **all** operators will work this same way. So the fact that you don't know exactly what the final language will be like, or which operators are infix operators, or what "+" actually means, doesn't matter... and you can change it without trouble. This notation exposes exactly where every list begins and ends, too. Curly infix adds only one abbreviation: if a list is surrounded by {...}, the operators are presented in infix order instead of in prefix order. This is just like 'x, which is just an abbreviation (quote x).  We're adding a small set of additional abbreviations.  And finally, this extension to s-expressions makes the resulting expressions ''much'' easier for humans to read and write, because it allows humans to the use the notation they've been trained to use for 15+ years and that nearly all books use.

Here are some interesting relevant quotes:

-   "I've used Lisp my whole programming life and I still don't find prefix math expressions natural." - [Paul Graham](http://paulgraham.com/popular.html)
-   "I have more faith that you could convince the world to use esperanto than [to use] prefix notation." - [Paul Prescod](http://people.csail.mit.edu/gregs/ll1-discuss-archive-html/msg01571.h%0Atml)

* * * * *

*Adding or removing parentheses - or adding indentation - to the tree notation makes no sense. To a Lisper that has learned to recognize the () notation as the structure denoted by the tree diagram, it makes just as little sense.*

That's not true - essentially all Lisp code is **already** indented.  Nobody would accept Lisp code today if it wasn't indented! Common Lisp even builds in a pretty-printer, specifically to indent code, and most major Scheme implementations (e.g., guile) include one too.

Since Lisp code **already** uses indentation, combined with ugly excess parentheses that experts recommend ignoring, let's get rid of the unneeded parentheses.

* * * * *

*Given all the above, if you go ahead and build or use an indentation-based Lisp ("IBL"), be prepared to abandon it after you get sufficiently into Lisp. And if you post about your IBL to c.l.l., be prepared for a less-than-welcoming response.*

Here we see the real problem. Almost all software developers have abandoned Lisp, correctly perceiving that Lisp notation is too ugly to be readable... and few want their resulting programs to be that hard to read. Hard-to-read programs are hard to improve later!

As a result, many of the people left on Common Lisp mailing lists are die-hards who believe that no improvements or innovations can occur in Lisp notation, and are actively hostile to others who **are** interested in improvements.

This is not univerally true, thankfully. Paul Graham is a widely-respected advocate of Lisp, and yet he's willing to publicly say that "Common Lisp and Scheme only directly support s-expressions; disadvantage: long-winded". He recommends [Syntax as abbreviation](http://www.paulgraham.com/arcll1.html), specifically, that additional notation would be an abbreviation for longer still-valid syntax. This just like 'x is today. In particular, he is an advocate of showing structure by indentation instead of parentheses (which would become optional where no ambiguity results). He also admits that infix notation would be nice.

Peter Norvig, another well-known Lisper, developed a Lisp variant that allowed abbreviations so infix is automatically noted and used. In short, some *are* willing to acknowledge that there are problems, and hunt for a solution.

* * * * *

*(We'd love to hear about an IBL-er abandoning it for parentheses, though. This would provide empirical evidence to which we could direct later newbies.)*

No doubt there will be several, but since documents like this will only report people abandoning indentation - and not those who **switch to** indentation - their reporting will be completely biased. It would be more interesting to see, as the alternatives become mature, what the numbers are in both camps.

So instead of pretending there is no problem, we've actually worked to solve it.  We already have implementations for Scheme (guile) and Common Lisp, including general-purpose tools that can help with other programming languages.  Please try out our solutions; we think you'll like them.

The [Solution] page gives a more detailed description of our solution.  For more information, please see <http://readable.sourceforge.net>.  This page was originally posted as <http://www.dwheeler.com/readable/retort-lisp-can-be-readable.html>.

