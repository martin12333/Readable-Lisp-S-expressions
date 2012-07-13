; Implementation of a revision of SRFI-49.  The original SRFI-49
; was developed by Egil Möller, and revised by David A. Wheeler. See:
;   http://srfi.schemers.org/srfi-49/srfi-49.html
;   http://www.dwheeler.com/readable/readable/trunk/spec-indent.txt


; WARNING - this approach will probably be abandoned; see sugar.scm.

;
; This provides "Indentation-sensitive syntax" for Scheme.
; This SRFI descibes a new syntax for Scheme, called I-expressions,
; with equal descriptive power as S-expressions. The syntax uses
; indentation to group expressions, and has no special cases for
; semantic constructs of the language. It can be used both for
; program and data input.

; The following code implements I-expressions as a GNU Guile module
; that can be loaded with (use-modules (sugar)).

; This is a major re-implementation of I-expressions by
; David A. Wheeler, with some small snippets from the code by
; Egil Möller.  The key is that this code is intentionally modelled
; after the production rules of the specification - so it's more likely
; to (1) be correct, and (2) have other people BELIEVE it's correct.


;----{ sugar.scm }----
; (define-module (sugar))

; TODO: Set up as guile module, but maximize code that is NOT
; guile specific.  E.G., use "define" not "define-public", and then
; "export" using separate commands.  This does use define-macro, which
; a common Scheme extension but is not in R5RS.

(define group 'group)
(define sugar-read-save read)
(define sugar-load-save primitive-load) ; guile-specific

; The second character is an embedded "tab"; this seems to be the most
; portable way to include the tab character.
(define hspaces '(#\space #\	))

(define-macro let1
  ; Simple single-variable "let"; lets "variable" to "value", then computes.
  ; Note: This is hygenic, even though it uses define-macro.
  (lambda (variable value . computations)
    `(let ((,variable ,value)) ,@computations)))

(define-macro call-production
  ; Call a production [which returns (indent . result)], call calculation
  ; [which modifies "result"], then return (newindent . newresult).
  ; This macro makes the source code look more like the BNF, increasing the
  ; chance we actually got it right.
  (lambda (production result-calculation)
    `(let* ((**return** ,production) ; not hygenic, don't use this var name
            (result (cdr **return**)))   ; intentionally NOT hygenic.
       (cons (car **return**) ,result-calculation))))

(define (ismember? item lyst)
  ; Returns true if item is member of lyst, else false.
  (pair? (member item lyst)))

(define (consume-hspace port)
  (cond
    ((ismember? (peek-char port) hspaces)
       (read-char port)
       (consume-hspace port))
    (#t #t))) ; Return value doesn't matter.

; TODO: Create sugar-error, and eventually a more robust recovery system.
(define (sugar-error message)
  (display "ERROR! ")
  (display message)
  (newline)
  message)

(define (indentation>? indentation1 indentation2)
  (let ((len1 (string-length indentation1))
        (len2 (string-length indentation2)))
    (and (> len1 len2)
         (string=? indentation2 (substring indentation1 0 len2)))))

(define (match-consume port c)
  ; if port's current character is c, consume it and return true. Else false.
  (cond
    ((eof-object? (peek-char port)) #f)
    ((char=? (peek-char port) c) (read-char port) #t)
    (#t #f))) ; no match.

; Each of the nonterminals (including -tail) is implemented by a function with
; the name sugar-NONTERMINAL.  Other than start-expr, each one accepts port
; and "indent" (the current indentation), and each returns
; (cons new-indentation result).  The recipient will need to unpack this -
; (car return) is new-indentation, (cdr return) is the result.

(define (sugar-body port indent)
  ; body -> expr body-tail
  ;   (cons $1 $last)
  (let1 body-expr (call-production (sugar-expr port indent) result)
    (call-production (sugar-body-tail port indent)
      (cons (cdr body-expr) result))))

(define (sugar-body-tail port indent)
  ; TODO: Must newline/EOF here, check.
  (match-consume port #\newline)
  (let1 new-indent (get-leading-space port)
    (cond
      ((string=? new-indent indent)
        ;   body-tail -> SAME body
        ;     $2
        (display "got sugar-body-tail, SAME\n")
        (call-production (sugar-body port indent)))
      ((indentation>? indent new-indent)
        ;   body-tail -> DEDENT
        ;     '()
        (cons new-indent '()))
      (#t (error "Bad indentation in body")))))

(define (sugar-head port indent)
  ; Do head productions.  They all start as "head -> s-expr ...", so
  ; read the s-expr, then call complete-head to finish the productions.
  (complete-head port indent (sugar-read-save port)))

(define (complete-head port indent first)
  ; Production - this completes the "head" production.
  ; We already got the initial s-expr "first"
  (consume-hspace port)
  (let1 c (peek-char port)
    (cond
      ((or (eof-object? c) (char=? c #\newline) (char=? c #\;))
        ; Production:  head -> s-expr hspace* comment?
        ;    (list $1)
        (consume-to-eol port)
        (cons indent (list first)))
      (#t
        ; Production:  head -> s-expr hspace* head
        ;    (append $1 $3)
        ; SPEC ERROR?!? should be (cons $1 $3)   ?!?!?!
        (call-production (sugar-head port indent) (cons first result))))))


(define (sugar-expr port indent)
  (let1 c (peek-char port)
    (cond
      ((eof-object? c)      (sugar-error "Unexpected EOF") (cons "" c))
      ; These shouldn't happen, but it's useful to detect coding errors:
      ((char=? c #\newline) (sugar-error "Unexpected newline") (cons "" c))
      ((char=? c #\space)   (sugar-error "Unexpected space") (cons "" c))
      ((char=? c #\;)       (sugar-error "Unexpected ;") (cons "" c))
      ; Handle abbreviations.
      ; TODO: process "SAME?"
      ((match-consume port #\')     ; expr -> QUOTE...
        (consume-hspace port)
        (call-production (sugar-expr port indent) (list 'quote result)))
      ((match-consume port #\`)     ; expr -> QUASIQUOTE...
        (consume-hspace port)
        (call-production (sugar-expr port indent) (list 'quasiquote result)))
      ((match-consume port #\,)
        (cond
          ((match-consume port #\@) ; expr -> UNQUOTE-SPLICING ...
            (read-char port)
            (consume-hspace port)
            (call-production (sugar-expr port indent)
                             (list 'unquote-splicing result)))
          (#t                       ; expr -> UNQUOTE ...
            (consume-hspace port)
            (call-production (sugar-expr port indent)
                             (list 'unquote result)))))
      ; GROUP and head.  The complication is that there isn't a way
      ; to distinguish between GROUP and the first non-terminal of head
      ; (which is s-expr).  No problem; we'll just read in the first term,
      ; (i.e., insert head processing here) and determine if it is group.
      (#t
        (let1 first-head (sugar-read-save port) ; first term of "head".
          (cond
            ((and (char-ci=? c #\g) (eqv? first-head group)) ; GROUP.
              (consume-hspace port)
              ; now there are 3 possibilities.
              ; TODO
              (cons indent first) ; STUB
             )
            (#t ; not GROUP; we have "expr -> head...".  Finish up "head".
              (let1 full-head (complete-head port indent first-head)
                ; Better be newline or EOF here. TODO: Check?
                (match-consume port #\newline)
                (let1 new-indent (get-leading-space port)
                  (cond
                    ((indentation>? new-indent indent) ; INDENT
                      ;   expr -> head INDENT body
                      ;    (append $1 $3)
                      (call-production (sugar-body port new-indent)
                             (append (cdr full-head) result)))
                    (#t
                      ;   expr -> head  ; followed by DEDENT or SAME
                      ;    (if (= (length $1) 1)  (car $1) $1)
                      ; TODO: Check if indent actually DEDENT or SAME,
                      ;       and not an error.
                      (cons new-indent
                        (if (= (length (cdr full-head)) 1)
                          (car (cdr full-head))
                          (cdr full-head))))))))))))))

; TODO - redefine as STUB
; (define (sugar-expr port indent)
;   (cons "" (read port))
; )

(define (consume-to-eol port)
  ; Consumes chars to end of line. Returns which character caused
  ; end-of-line (newline or EOF), WITHOUT consuming the newline/EOF.
  (let1 c (peek-char port)
    (cond
      ((eof-object? c) c)
      ((char=? c #\newline) c)
      (#t (read-char port) (consume-to-eol port)))))

(define (get-leading-space-list port lyst)
  (let1 c (peek-char port)
    (cond
      ((ismember? c hspaces)
        (read-char port)
        (get-leading-space-list port (append lyst (list c))))
      (#t lyst))))

(define (get-leading-space port)
  ; Returns all tabs and spaces available.
  (list->string (get-leading-space-list port '())))

(define (sugar-start-expr port)
  ; Read single complete I-expression.
  (let ((indentation (get-leading-space port)) ; text containing the indent
        (c (peek-char port)))
    (cond
      ((eof-object? c) c) ; EOF - return it, we're done.
      ((char=? c #\; )    ; comment - consume and see what's after it.
        (let1 c (consume-to-eol port)
          (cond
            ((eof-object? c) c) ; If EOF after comment, return it.
            (#t
              (read-char port) ; Newline after comment.  Consume NL
              (sugar-start-expr port))))) ; and try again
      ((char=? c #\newline)
        (read-char port) ; Newline (with no preceding comment).
        (sugar-start-expr port)) ; Consume and again
      (#t
        ; TODO: Handle  (> (string-length indentation) 0)
        (let* ((return (sugar-expr port ""))
               (new-indent (car return))
               (result (cdr return)))
          (if (string=? new-indent "")
              result
              (error "should have 0-length indent")))))))


(define (sugar-read . port)
  (if (null? port)
    (sugar-start-expr (current-input-port))
    (sugar-start-expr (car port))))

; Module system changed from guile 1.6 to 1.8.  For the moment, we'll
; keep it as straight, portable code, and then add module tweaks later.
; (export sugar-read)


; ----{ sugar.scm }----
; Copyright (C) 2005-2008 by David A. Wheeler and Egil Möller.
; All Rights Reserved; released using the Free-libre / open source software
; "MIT license":
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.


;   (define (readquote level port qt)
;     (read-char port)
;     (let ((char (peek-char port)))
;       (if (or (eqv? char #\space)
; 	      (eqv? char #\newline)
; 	      (eqv? char #\ht))
; 	  (list qt)
; 	  (list qt (sugar-read-save port)))))
;
;   (define (readitem level port)
;     (let ((char (peek-char port)))
;       (cond
;        ((eqv? char #\`)
; 	(readquote level port 'quasiquote))
;        ((eqv? char #\')
; 	(readquote level port 'quote))
;        ((eqv? char #\,)
; 	(readquote level port 'unquote))
;        (#t
; 	(sugar-read-save port)))))
;
;   (define (indentation>? indentation1 indentation2)
;     (let ((len1 (string-length indentation1))
; 	  (len2 (string-length indentation2)))
;       (and (> len1 len2)
; 	   (string=? indentation2 (substring indentation1 0 len2)))))
;
;   (define (indentationlevel port)
;     (define (indentationlevel)
;       (if (or (eqv? (peek-char port) #\space)
; 	      (eqv? (peek-char port) #\ht))
; 	  (cons
; 	   (read-char port)
; 	   (indentationlevel))
; 	  '()))
;     (list->string (indentationlevel)))
;
;   (define (clean line)
;     (cond
;      ((not (pair? line))
;       line)
;      ((null? line)
;       line)
;      ((eq? (car line) 'group)
;       (cdr line))
;      ((null? (car line))
;       (cdr line))
;      ((list? (car line))
;       (if (or (equal? (car line) '(quote))
; 	      (equal? (car line) '(quasiquote))
; 	      (equal? (car line) '(unquote)))
; 	  (if (and (list? (cdr line))
; 		   (= (length (cdr line)) 1))
; 	      (cons
; 	       (car (car line))
; 	       (cdr line))
; 	      (list
; 	       (car (car line))
; 	       (cdr line)))
; 	  (cons
; 	   (clean (car line))
; 	   (cdr line))))
;      (#t
;       line)))
;
;   ;; Reads all subblocks of a block
;   (define (readblocks level port)
;     (let* ((read (readblock-clean level port))
; 	   (next-level (car read))
; 	   (block (cdr read)))
;       (if (string=? next-level level)
; 	  (let* ((reads (readblocks level port))
; 		 (next-next-level (car reads))
; 		 (next-blocks (cdr reads)))
; 	    (if (eq? block '.)
; 		(if (pair? next-blocks)
; 		    (cons next-next-level (car next-blocks))
; 		    (cons next-next-level next-blocks))
; 		(cons next-next-level (cons block next-blocks))))
; 	  (cons next-level (list block)))))
;
;   ;; Read one block of input
;   (define (readblock level port)
;     (let ((char (peek-char port)))
;       (cond
;        ((eof-object? char)
; 	(cons -1 char))
;        ((eqv? char #\newline)
; 	(read-char port)
; 	(let ((next-level (indentationlevel port)))
; 	  (if (indentation>? next-level level)
; 	      (readblocks next-level port)
; 	      (cons next-level '()))))
;        ((or (eqv? char #\space)
; 	    (eq? char #\ht))
; 	(read-char port)
; 	(readblock level port))
;        (#t
; 	(let* ((first (readitem level port))
; 	       (rest (readblock level port))
; 	       (level (car rest))
; 	       (block (cdr rest)))
; 	  (if (eq? first '.)
; 	      (if (pair? block)
; 		  (cons level (car block))
; 		  rest)
; 	      (cons level (cons first block))))))))
;
;   ;; reads a block and handles group, (quote), (unquote) and
;   ;; (quasiquote).
;   (define (readblock-clean level port)
;     (let* ((read (readblock level port))
; 	   (next-level (car read))
; 	   (block (cdr read)))
;       (if (or (not (list? block)) (> (length block) 1))
; 	  (cons next-level (clean block))
; 	  (if (= (length block) 1)
; 	      (cons next-level (car block))
; 	      (cons next-level '.)))))
;
;   (define-public (sugar-filter)
;      (let ((result (sugar-read (current-input-port))))
; 	(if (eof-object? result)
; 	    result
;             (begin (display result) (newline) (sugar-filter)))))
;
;   (define-public (sugar-load filename)
;     (define (load port)
;       (let ((inp (sugar-read port)))
; 	(if (eof-object? inp)
; 	    #t
; 	    (begin
; 	      (eval inp)
; 	      (load port)))))
;     (load (open-input-file filename)))
;
;   (define-public (sugar-enable)
;     (set! read sugar-read)
;     (set! primitive-load sugar-load))
;
;   (define-public (sugar-disable)
;     (set! read sugar-read-save)
;     (set! primitive-load sugar-load-save))
;
;   (sugar-enable)
;

; I-expressions: Detailed Specification
; David A. Wheeler
; 2008-01-06
;
; SRFI-49 (http://srfi.schemers.org/srfi-49/srfi-49.html dated 2005/05/29)
; provides a pretty good system for indentation, but there are some issues
; with it.  The spec has a few errors, and the BNF productions don't
; include much information on the whitespace-handling (which may explain
; why the sample implementation has bugs in handling comments in certain
; constructs).  Instead, much of the whitespace handling is described
; only informally.  In addition, the sample code isn't obviously related
; to the BNF productions, so it's difficult to be confident that the code
; is correct even if its known bugs were fixed.
;
; So, below are step-by-step transforms of the
; SRFI-49 rules.  The first one is a mild "fix-up" of the SFRI rules;
; the second takes the first and adds whitespace rules that are
; (mostly) implicit in the text, as well as proposing a way to deal
; with "initial indent".  The goal is to make the specification more
; detailed, in a step-by-step fashion, and then create an implementation
; that is "obviously correct" from the detailed spec.
; That way, we can be much more confident that the code is correct.
;
; ===========================================================
; The SRFI-49 spec productions are modified as follows:
;
; (1) Fix spec bug: the "head" productions' "expr" are changed to "s-expr"
;
; (2) Fix spec bug: the rule for "head-> s-expr" is changed from "(list expr)"
;     to "(list $1)"
;
; (3) Fix spec bug: the missing rule for UNQUOTE-SPLICING has been added
;
; (4) Editorial: rules are reordered so the GROUP productions are adjacent
;
; (5) Relaxation of rule: This version adds a "start-expr" production,
;     which defines reading the first line of an expression, and adds a new
;     rule to permit indented initial lines:
;         start-expr -> INDENT expr DEDENT
;
;     Rationale: The original specification completely forbid initial
;     indentation, but this turns out to have been overly strict.  See below
;     for details about various alternatives. This alternative was selected
;     because it has the "obvious" meaning, yet does NOT require "read"
;     to store hidden indentation state after returning.  Many other
;     alternatives required hidden state or could be easily misleading
;     (resulting in subtle bugs).  This means that if initial expressions
;     are all indented, they must be separated with blank lines.
;
; (6) Proposed bug-fix: After the FIRST line of an expression, any
;     line with ONLY zero or more spaces and tabs, and no ;-comment,
;     ENDS the expression.
;
;     This is only a small change from the spec text as literally written.
;     A line with zero or more horizontal whitespace characters followed by
;     a ;-comment, aka an "empty line", is (still) ALWAYS ignored
;     and not considered for indentation processing. In addition,
;     a line with ONLY horizontal whitespace characters, aka a
;     "blank line" (it has only blanks), is (still) ignored and skipped
;     when reading the first line of an expression (and not considered
;     for identation processing).
;
;     Rationale: If the spec were followed literally, interactive use
;     would be quite unpleasant. That's because the results of an
;     expression would NEVER be written until after the next
;     expression's first line was entered, which is very confusing.
;     Here, pressing "Enter Enter" (a blank line) causes execution, which is
;     much clearer.
;
;     The modification also states that lines with JUST spaces and tabs
;     would be considered the same as a line with no characters at all.
;     Otherwise, printed and displayed text could not be understood with
;     certainty - a line that looked completely blank COULD mean either
;     "expression completed" OR "expression continues", depending on the
;     invisible whitespace indentation - leading to hard-to-debug code.
;     What's worse, many tools (including editors and email clients) quietly
;     remove trailing whitespace, so differentiating between lines that
;     have only spaces/tabs from lines with no characters can lead to
;     many quiet changes in code meaning.
;
;     Note that the sample implementation (in the spec) does not follow the
;     spec production rules - it DOES return on empty lines in
;     some circumstances.  So this is considered a bug in the production rules.
;
;     As noted in its spec, I-expressions were specifically designed
;     so that they will work the same way whether they are entered
;     interactively or via a file, enabling cut-and-paste
;     and avoiding the complexities of a "mode" flag (which can be hard
;     to get right).
;
;     To create vertical space, just use a ;-leading comment.  Note that
;     lines with only leading whitespace and ;-comments MAY, but
;     NEED NOT, align with other text - so quick "FIXME" comments, or
;     lengthy comments, need not match the indentation.
;
;     *** THIS IS UNDER DISCUSSION IN THE MAILING LIST. ***
;     TODO: If this proposal is acceptable (or another one found that's even
;     better), reword the clarifications to merge this in, and modify the
;     "more detailed spec" below to match.
;
; (7) Proposed Clarification: the term "group" ONLY has the
;     special meaning below if it begins with "G" or "g".  An I-expression
;     parser MUST NOT interpret it as an I-expression group otherwise.
;
;     Rationale: This permits using the underlying implementation's
;     symbol escape mechanisms to express solely the symbol group
;     WITHOUT giving it the special I-expression meaning.  The spec was
;     not clear what an I-expression reader should do with escaped
;     symbols that _mean_ group, but do not start with G or g.
;
;     TODO: If this proposal is acceptable (or another one found that's even
;     better), reword the clarifications to merge this in, and modify the
;     "more detailed spec" below to match.
;
; ===========================================================
; Updated SRFI-49 spec productions
;
; The modified production rules are:
;
;   start-expr -> expr
;     $1
;   start-expr -> INDENT expr DEDENT
;     $2 ; Use "most consistent" approach for handling toplevel indents.
;
;   ; Abbreviations; these are considered first, BEFORE reading as s-expr:
;   expr -> QUOTE expr
;    (list 'quote $2)
;   expr -> QUASIQUOTE expr
;    (list 'quasiquote $2)
;   expr -> UNQUOTE expr
;    (list 'unquote $2)
;   expr -> UNQUOTE-SPLICING expr  ; ,@
;    (list 'unquote-splicing $2)
;
;   ; Note: GROUP is defined so it's only meaningful if it's the first
;   ; non-abbreviation of the line; GROUP has no special effect elsewhere.
;   expr -> GROUP head INDENT body DEDENT
;    (append $2 $4)
;   expr -> GROUP INDENT body DEDENT
;    $3
;   expr -> GROUP head
;    (if (= (length $2) 1)
;        (car $2)
;      $2)
;   expr -> head INDENT body DEDENT
;    (append $1 $3)
;   expr -> head
;    (if (= (length $1) 1)
;        (car $1)
;      $1)
;
;   ; "head" is what happens on ONE line, and a head sequence ends with eol.
;   head-> s-expr head
;    (append $1 $2)
;   head-> s-expr
;    (list $1)
;
;   ; "body" is the set of children lines (from the point-of-view of head)
;   body -> expr body
;     (cons $1 $2)
;   body ->
;    '()
;
;
; ===========================================================
; Detailed version of spec
;
; The original spec described primarily in words what to do about whitespace.
; This can make it difficult to implement with certainty, so the following
; is a more detailed version of the production rules, but with whitespace
; rules made explicit as part of the productions (instead of being
; implicit in the English text).
;
; Instead of having an anonymous "whitespace preprocessor", we will
; define a special rule that can consume a line's initial indentation, and
; compare that with the currently-active indentation.
; It can then produce INDENT, DEDENT, or SAME (for the same indentation).
; During processing of the start-expr rule, the "current" indentation is
; the empty string, and it can start immediately (since this rule only
; applies to new lines).  For the rest of the rules, INDENT, DEDENT or
; SAME can only match if the currently-being-processed character is newline
; or EOF.  It will match INDENT if the new line is further indented,
; DEDENT for each level of dedenting, and SAME if it's the same level of
; indenting.
;
; Note that EOF can start a whole new expression, but can't be in
; the middle of one.  Thus, a ' followed by EOF is not legal, but
; EOF is perfectly legal result of start_expr (it's how a correctly-terminated
; file would end).
;
; A current proposal is to treat as line with only horizontal
; whitespace as if it's a line solely with newline - i.e.,
; as if the horizontal whitespace didn't even exist.
; After all, you can't see the difference when printing,
; and typically can't see them when editing either.
; This appears to be the safer alternative.
; That's what the text below does.
;
; The following is psuedocode for key functions, followed by
; modified productions that add specific rules for whitespace processing
; (instead of the English descriptions):
;
; define get-leading-hspace()
;    {sequence <- get/consume sequence of spaces and tabs}
;    if pair?(memv(peek-char() '(NL EOF)))
;       ""  ; if newline (no ;), treat as newline-by-self. Don't consume here.
;       sequence ; return new indent level
;
; Here's the whitespace processor, described
; in pseudocode using sweet-expressions 0.2.
; This is called when INDENT/SAME/DEDENT are being matched; we know we
; have one (it begins with a newline after start-expr), but we need
; to figure out which one.
;
; define process-line-begin()
;   ; Used by the productions OTHER that start-expr; consume the newline
;   ; (which is the current character if you peek), and return the new
;   ; indentation text.  Do NOT use this in start-expr, because start-expr
;   ; need NOT begin with a newline.
;   ; Use "whats-indent" to see if the indentation text returned is
;   ; an INDENT, SAME, DEDENT, or an error.
;   ; The following consumes the newline - and complains if it's NOT a newline
;   if {(read-char) char-ne? #\newline) error("process-line-begin miscalled")
;   {new-indent <- get-leading-hspace()}
;   cond
;     {peek() = #\; }       ; Comment-only line?
;       get-comment()       ; Yes, ignore comment-only lines -
;       consume-nl-ifany()  ; even their indentation is irrelevant.
;       process-line-begin()
;     #t
;       new-indent
;   ; Note: This won't consume "return" when we have a line with no characters,
;   ; or ONLY spaces/tabs.  Instead, it will return all the way back up to
;   ; start-expr.  If we're processing something, it will eventually return
;   ; to complete a start-expr, which returns with the expression.
;   ; If we're _already_ in start-expr, then start-expr will just consume the
;   ; blank line and keep going.
;
; define whats-indent(old-indent new-indent)
;   cond
;     {new-indent > old-indent}  return(INDENT)
;     {new-indent = old-indent}  return(SAME)
;     {new-indent < old-indent}  return(DEDENT)
;     #t                         error("Incomparable indents")

;
;
; Here are the modified production rules, making whitespace explicit.
; The expectation is that in the implementation, each of these nonterminals
; would be implemented by a function; one of its input parameters would be
; the current indentation, and each would return
; (new-indentation result-so-far).  A function can compare the indentation
; that it received when it started with a new-indentation returned
; using what-indent.  Note that some of the $values are changed in below,
; because when the whitespace symbols were inserted they caused the positions
; of the other non-terminals to change.  In some cases $last is used, so that
; it'd be more likely to be right as the text below was edited.
;
;   ; Definitions of whitespace:
;   eol -> comment? eol-final               ; eol = "end of line"
;   comment -> ";" (not NL|EOF)*   ; Note: does not consume NL or EOF.
;   eol-final -> NL | EOF
;   hspace -> SPACE | TAB
;
;   ; Start-up is special (esp. for EOF handling).
;   ; INDENT-NO-NL, DEDENT-NO-NL, and SAME-NO-NL are like INDENT, DEDENT, and
;   ; SAME, but they do NOT start with a newline.  Thus, they do NOT consume
;   ; an initial newline.  We use these, because in start-expr we are ALREADY
;   ; at the start of a new line, while in all other productions, we don't
;   ; transition to a new line until we see the newline character.
;
;   start-expr -> hspace* comment? EOF
;     ; Consume this EOF, in case it's interactive - all others should be peeked
;     ; Note: get hspace* using get-leading-hspace, and NOT by skipping the
;     ; space (because we need to retain those characters for examination)
;     ; and NOT by invoking process-line begin (because that presumes that we're
;     ; beginning with a newline, not necessarily true here)
;     $last ; EOF is legal at top level (here).  Note that '<EOF> isn't legal
;   start-expr -> hspace* comment? NL start-expr
;     $last ; skip any initial content-free lines.
;   start-expr -> INDENT-NO-NL expr DEDENT-NO-NL
;     $2 ; Use "most consistent" approach for handling toplevel indents.
;     ; Note that DEDENT-NO-NL does NOT consume a newline; that was already
;     ; done in expr.
;   start-expr -> expr SAME-NO-NL
;     $1 ; this is an expression starting at the left edge.
;     ; Notice that we have no way of knowing when we're "done" until we
;     ; read the next line, and see that it has effectively a left-edge-start
;     ; (because it's a whitespace-only line, or because it's another expr
;     ; starting at the left edge).  Note that SAME-NO-NL does NOT consume a
;     ; newline; that consumption was already done by expr.
;
;   ; "expr" describes a single expression/datum.
;   ; Abbreviations; these are considered first (have higher priority)
;   ; that the abbreviation processing built into the nonterminal "s-expr".
;   ; That way, we do NOT leave indentation processing merely because we had
;   ; one of the standard abbreviations, preventing certain misleading formats.
;   expr -> QUOTE hspace* SAME? expr
;    (list 'quote $last)
;   expr -> QUASIQUOTE hspace* SAME? expr
;    (list 'quasiquote $last)
;   expr -> UNQUOTE hspace* SAME? expr   ; , without a following @
;    (list 'unquote $last)
;   expr -> UNQUOTE-SPLICING hspace* SAME? expr  ; ,@
;    (list 'unquote-splicing $last)
;   ; Note: Abbreviations only accept expr at their tail; they
;   ; do NOT (by intent) accept "INDENT body DEDENT" instead of expr.
;   ; If they did, that would imply that abbreviations can have multiple
;   ; arguments (because bodies can have more than one entry) - but they can't!
;   ; Thus, if an abbreviation symbol is followed by hspace* newline,
;   ; the next line (that isn't a comment-only line) must have the SAME
;   ; indentation level. This prevents nonsense like a ' with two arguments.
;   ; Notice that an expr is required after an abbreviation,
;   ; so '<EOF> is not legal.
;
;   ; In actual code, you can't distinguish between GROUP and head until the
;   ; leading s-expr is read in.  So in the implementation:
;   ;   * peek at the first character - remember if it is G/g or not.
;   ;   * read in the s-expr
;   ;   * if the s-expr is the symbol "group", _and_ the first peeked character
;   ;     was G/g, then it is GROUP... else it is not.
;   ;
;   expr -> GROUP hspace* head INDENT body DEDENT
;    (append $3 $5)
;   expr -> GROUP hspace* comment? INDENT body DEDENT
;    $4
;   expr -> GROUP hspace* head
;    (if (= (length $last) 1)
;        (car $last)
;      $last)
;   expr -> head INDENT body DEDENT
;    (append $1 $3)
;   expr -> head  ; followed by DEDENT or SAME
;    (if (= (length $1) 1)
;        (car $1)
;      $1)
;
;   ; "head" describes multiple datums on one line:
;   head -> s-expr hspace* head ; typically hspace+.
;    (append $1 $3)
;   head -> s-expr hspace* comment?
;    (list $1)
;    ; this is the terminating production (the other one recurses to "head");
;    ; this production is followed by EOF or newline (newline is represented
;    ; here as part of the indentation token)
;
;   ; "body" describes the sequence of child lines.
;   ; It's impossible to have the sequence INDENT DEDENT, there MUST be
;   ; something in between.  Thus, when body is first called, it cannot
;   ; match the "empty" rule of the I-expression spec.  Thus, we'll rewrite
;   ; the rule as a body followed by body-tail, because that's
;   ; easier to implement.
;   body -> expr body-tail
;     (cons $1 $last)
;   body-tail -> SAME body
;     $1
;     ; It's another line at the SAME indentation, so we have another body.
;   body-tail ->
;     '()
;     ; No more children.  This will be a DEDENT in caller,
;     ; ending the sequence of bodies. But the DEDENT is actually
;     ; consumed by the caller (one of the expr productions),
;     ; not this production, so we'll write the production rule this way.
;     ; If it's DEDENT, we've ended the list of bodies.
;     ; Note: It would be illegal to be INDENT; INDENT would be handled
;     ; by expr, not body.
;
;   ; s-expr is a traditional s-expr, aka datum.  It does NOT begin with
;   ; ";", hspace, NL, or EOF.
;   ; To implement it, the I-expression reader presumably calls on
;   ; the _previous_ "read" routine for datum.
;   ; When processing "expr", the special definitions for
;   ; abbreviations QUOTE etc. take precedence, so that indentation
;   ; processing is not accidentally disabled. But, if you're processing
;   ; the later entries of "head" (i.e., datums that are NOT the first
;   ; datum on the line), the s-expr reader must handle the abbreviations.
;   ; Users could abuse this to make ugly-looking code, but unlike the
;   ; case of the initial expr, this wouldn't confuse the indentation...
;   ; once you're processing later datums on a line, if the line ends in
;   ; an abbreviation, then the next line's indentation is ignored since we
;   ; KNOW we're trying to read in exactly one datum.
;   ; Use a style checker if you want to curb the worst abuses.
;
;
;
; Note: I-expressions do not provide special syntax for improper lists,
; e.g., (a . b).  When you need them, just use s-expressions or cons.
; A _syntax_ for this would be easy, e.g., rules like:
;   head -> s-expr hspace+ "." hspace+ s-expr
; However, it'd be hard to IMPLEMENT, because "." is a leading character
; for many different circumstances (.9, ..., etc.), yet calling the
; underlying reader might not be effective.  E.G., clisp's "read" will
; fail if given a solo ".".  There doesn't seem to be a compelling need
; anyway; you can use s-expressions or cons to construct these,
; and there's an implementation headache to create
; another syntax in I-expressions to implement them.
;
; Other than in start-expr, "INDENT" matches newline followed by a deeper
; indentation than the current one.  Similarly for SAME and DEDENT.
; Thus, INDENT/SAME/DEDENT only match in non-initial lines streams that
; begin with newline or EOF... and obviously, INDENT can't match an EOF.
;
; On a match, all the matching characters (and only those) should be
; consumed.  The exception is EOF: Except for the first rule of
; start-expr, "peek" for EOF and don't consume it.
;
; Note some invariants:
; * You can't follow DEDENT/SAME/INDENT with any hspace - it'd be ambiguous.
;   - Therefore, expr, head, and body can't begin with an hspace.
;
; Goals:
; * Inside a line, you should be able to separate s-expr with hspace+
; * hspace* should be okay at the end of each line, with unchanged meaning.
;   - This is handled by a "head" production
;
; ========================
; Whitespace problems in original spec
;
; Unfortunately, there are several problems in the original spec
; regarding the handling of lines that contain 0+ spaces or tabs,
; possibly followed by a ;-leading comment.
;
; The original final I-expression spec says:
; "Unfortunately, [in Python] the syntaxes of file input and
; interactive input differs slightly...
; [In I-expressions,]
; Each line in a file is either empty (contains only whitepace and/or a
; comment), or contains some code, preceeded by some number of space and/or tab
; characters.
; In the following syntax definition, this initial space, as well as linebreaks,
; is not included in the rules. Instead, preceding any matching, the leading
; space of each line is compared to the leading space of the last non-empty line
; preceeding it, and then removed. If the line is preceeded by more space than
; the last one was, the special symbol INDENT is added..."
;
; Thus, in the original spec, a line with only horizontal whitespace,
; optionally trailed by a comment (presumably a ;-comment), was ignored after
; the first line of an expression.  But this says nothing about what
; happens on the FIRST line of an expression; some clarification is needed.
;
; In addition, the text above implies that in I-expressions,
; the file input and interactive input formats are the same.
; Yet this is improbable as stated.  The "obvious" reading of the spec
; suggests that blank lines ("Enter Enter") at the end of an expression
; would be ignored. But this would mean that in interactive use, the output
; of a first expression would only be produced after the first line of the
; second expression were entered.  This would lead to confusion like this
; (where ">" is an input prompt):
; > + 1 1
; > + 1 2
; 2
; > + 1 3
; 3
;
; The sample implementation given in SFRI-49 didn't really follow
; the spec of SFRI-49.  For example, it DID accept indentation of the
; first line (though with a problematic semantic), and it DID accept
; blank lines (in some cases) as ending an expression.
; This suggests that the spec is not quite accurate, and needs careful
; revision/clarification.
;
; The sections below discuss two issues:
; * Leading whitespace at start of expression reading
; * Blank and comment-only lines after initial line of an expression.
;
; ========================
; ISSUE: Leading whitespace at start of expression reading in I-expressions
;
; I propose a specific interpretation for leading whitespace in an
; indented I-expression, which I'll call the "most consistent" format.
; Below is an explanation of the problem, and my proposed resolution.
;
; Thoughts?  After fiddling with the alternatives, I'm getting very worried that
; it'd be easy to type in text that would APPEAR to mean one thing, but would
; ACTUALLY mean something else.  That's definitely something to avoid.
; My "most consistent" proposal completely avoids that, without being quite as
; strict as Python's "thou shalt always start any expression at the left edge".
;
; First, the initial situation:  The I-expression spec revised 2005/05/29
; does NOT permit an indentation at the beginning of an expression.
; The sample implementation does permit them; it simply skips horizontal
; whitespace on the first line (ignoring them).  The spec's completely
; forbidding them is easily done, but is overly strict; the
; "skip horizontal whitespace" approach has unforunate consequences
; (as discussed further below).
;
; What SHOULD be done if the start of an expression
; (I'll call that start-expr) begins with whitespace that
; is followed by content (and not just an
; ;-comments, newline (NL) or end-of-file (EOF))? E.G.:
;  start-expr -> hspace+ (not eol...)
;
; An example should make it clear. Imagine you read this (three lines,
; all indented to the same level at the TOPMOST level):
;    x
;    y
;    z
;
; One interpretation is that there should be 3 different results: x, y, and z.
; But consider how this would be read.
; You'd read in the indentation before x, and note
; that as the "topmost" indentation.  Then you'd read in the indentation
; before y, notice that it was the same as x's, and stop just before reading
; the "y" and return with just "x".
; But wait - if you did that, when you read "y" you would think that there
; was <i>no</i> indentation (the previous read consumed it), and thus z
; would be further indented... returning (y&nbsp;z).  Ooops, that can't be right.
;
; Since essentially the dawn of Lisp in the 1950s
; there has been a "read" function that reads an S-expression
; from the input and returns it.
; This is an extremely stable function interface, and one not easily changed
; in fundamental ways.
; In particular, no user of "read" expects it to <i>also</i> return some
; state - such as the indentation that was read the <i>last</i> time read
; was called - and certainly they aren't going to provide that information
; back to "read" anyway.
; Not only is this difficult to change for backwards-compatibility reasons,
; it's not clear you should - simple interfaces are a good idea, if you can
; get them, and adding such "indentation state" as a required parameter would
; certainly complicate the interface.
;
; In theory, you could "unget" all the indentation characters, so that the
; next read would work correctly.
; But support for this is rare; for example,
; Scheme doesn't even <i>have</i> a standard unget character function, and
; the Common Lisp standard only supports one character unget (not enough!).
;
; You could store "hidden state" inside the read function.
; Problem is, character-reading is not the exclusive domain of the read function;
; many other functions read characters, and they are unlikely to look at this
; hidden state.
; These functions tend to be low-level functions and in some implementations
; are difficult to override.  So you'd probably have inconsistent values
; from different reading functions, a recipe for subtle bugs.
; What's more, you would have to store hidden state for each possible input
; source, and this can become insane in the many implementations that support
; support ports of non-files (such as from strings).
; "Hidden state" could allow for all this, but the complications of
; <i>implementing</i> hidden state suggests that it'd be better to spec
; something that does <i>not</i> require hidden state.
;
;
;
;
; Possible solutions:
;
; 1. Simplest approach: Forbid it.  It's an error if it doesn't start on
; left line.  Python does this.  You could argue that the original spec requires
; this, since there's no production that accepts an initial INDENT.
; The xyz example above would then be illegal.
;
; But this is not very flexible; #2 (next) appears to be a better option.
;
; 2. Most consistent: Allow indentation on initial line (and consider that
; the indentation for that expression), as long all later lines have
; a further indentation OR are on the left edge (including a blank line
; ending in EOL or EOF, or a comment the left edge).
; Anything on the left edge ends the expression.
; This at least LETS you indent each expression if you like,
; with NO risk of misinterpretation of later lines.
; The result is that you can indent the first lines of expressions.. you just
; have to separate the different expressions with blank lines.
; To implement this:
; start-expr -> INDENT expr DEDENT
;
; The xyz example above would be illegal, and thus rejected.
; However, if you inserted blank lines between x, y, and z, you'd be okay.
;
; This is the most consistent and most flexible, and has no risk of
; misinterpretation, so I propose this one.
;
; 3. Original implementation ignores hspace:
; start-expr -> hspace+ start-expr
;   $2
;
; But when this is given the xyz example above, it will misleadingly
; produce (x y z).  That kind of surprise seems undesirable, esp. given
; that there is alternative #2.
;
; 4. Instead, could disable indent processing on initial hspace, to maximize
; backwards-compatibility and simplify some command line use:
; start-expr -> hspace+ s-expr
;   $2
;
; This would read in the "xyz" example as you would expect.  It would
; also read in old text like this as it was originally intended:
;   (define x 5) (define y 6)
;
; However, other formats will be misinterpreted, e.g.,
;   fact
;     5
; will be understood as the two separate expressions (requiring two reads):
;   fact
;   5
; and not as (fact 5).
;
; This is risky; on printouts, it might not at ALL be obvious when
; expressions are indented like this - resulting in hard-to-debug code
; and hidden defects.
;
; In general, I think it's much wiser to reject text that might
; be very easily misinterpreted by the reader.  So I suggest #2.
;
;
; ========================
; ISSUE: Blank and comment-only lines after initial line of an expression.
;
;     *** THIS IS UNDER DISCUSSION IN THE MAILING LIST. ***
;
; Both the original I-expression definition, and this revision,
; have the following rule: After the initial line of an expression,
; any line containing zero or more leading spaces and tabs,
; FOLLOWED by a ";"-leading comment, is COMPLETELY IGNORED, even in the
; middle of an expression.  In particular, its indentation is ignored.
;
; The proposed change is that lines with 0+ spaces or tabs,
; and no ;-leading comment, end an expression.  Note that a line
; WITH just spaces and tabs is treated the same as a line with no characters.
;
; This changes is necessary for reasonable interactive use.
; If lines with only zero or more horizontal whitespace were completely
; ignored, as the original spec stated, then interactive use is painful.
; Results would print only after the first line of the NEXT expression
; are entered.  Even the "sample implementation" in the spec didn't
; actually do this.
;
; To make interactivity pleasant, at least lines with absolutely no
; characters should end an expression after the first line.  That way,
; "Enter Enter" will cause an expression to be executed.
;
; Then the question becomes, how should a line with 1 or more
; spaces/tabs, and no ;-comments, be interpreted?  And should a line
; with the current indentation be treated differently than if it is not?
;
; Such lines COULD be interpreted as "continue the indentation"
; if they matched the current indentation... or even if there was
; at least one space/tab.  A minor argument FOR this alternative
; is that it makes "pretty" vertical spacing a possibility.
;
; But there is a serious problem to treating lines with no characters
; differently from lines with only spaces and tabs.
; The problems is that this could lead to mysterious bugs.
; With such a rule, printed or displayed text could not be understood
; with certainty; a line that looked empty COULD mean either
; "expression completed" OR "expression continues",
; leading to hard-to-debug code.  What's worse, many tools
; quietly remove trailing horizontal whitespace on a line
; (including many text editors and mailers); these deletions would
; generally be unnoticed, yet change the meaning of a program.
;
; The risk of mysterious, undetectable errors in code is serious,
; and one that leads at least Wheeler to recommend that space/tab-only
; lines (with no comments) be treated exactly like lines with no
; characters at all.  Certainly, it's sometimes valuable to create
; vertical space, but this can be done using comment-only lines
; (which may, but need not, have leading whitespace).
;
; It is true that Python 2.5 in _interactive_ mode distinguishes
; between lines with no characters (not even whitespace) and lines
; with only whitespace.  In interactive mode, a line with no characters
; ends an expression, while lines with whitespace are considered relevant
; (and can continue further lines).  However, Python 2.5 in
; _file-reading_ mode has different semantics - it ignores both types of
; lines (they have no effect on indentation).  This difference in modes
; has the problem that it can cause cutting-and-pasting from a file to
; an interactive session to fail, which is unfortunate.
; But it's even more difficult for a Lisp-based system; it's often even
; more difficult to be certain when a session is interactive or not -
; there is no standard way for doing so.  This distinction of
; interactive and non-interactive modes is very inconvenient, and
; should be avoided if practical.
;
; Note that this does NOT change how comment-only lines, preceded by
; only 0+ spaces and tabs, are interpreted - these are STILL completely
; ignored, and any indentation they have is considered irrelevant.
; Thus, people will not need to "line up" indentation
; of comments. This is useful for "FIXME" comments, or for
; long comments to explain a complicated circumstance which is deeply
; indented.  Since this ONLY applies to ;-comment-only lines,
; blank lines (without comments) still end an expression - so
; interactive use is still pleasant.  Most importantly, there's no
; need for a difference between interactive and non-interactive use -
; there's no need for a "mode" flag (which can be hard to get right),
; and you can always cut-and-paste from a file into an interactive session.
;
; ===================
; Notes about multiline comments
;
; Note that multiline comments (e.g., #| ... |#) are NOT considered
; comments by these rules. There is simply no way a
; library implementation can reliably detect
; such multicharacter sequences without disabling the reader's
; implementation of # prefixes, because they are limited in many
; languages to single character lookahead.  A #| will be considered
; an s-expr by the indentation processor.  The following uses of #|
; are known to be safe and not misleading:
;   * #| _inside_ an s-expression; in this case indentation processing
;     isn't happening.
;   * #|...|#, where the opening "#|" is the first non-whitespace on the
;     line, nothing trails the line after the closing |#, and
;     the opening #| is indented the same way as the first
;     non-comment-only line following the closing #|.
;
; ========================
; Possible Test patterns
;
; Below are some possible test patterns to help eliminate
; errors in an implementation.
;
; test = (first (second third?)?)?  (SPACE? semicolon - comment)?
; first = abbreviation? t1 (SPACE t2 (SPACE t3)?)?
; t1 = A | GROUP
; t2 = B
; t3 = C
;
; second = first | SPACE first
; third = SPACE first | SPACE SPACE second
;
;





; ; modern.scm (Scheme), 2008-01-03
; ;
; ; NOTE: NOT READY FOR PRODUCTION USE.
; ;
; ; Implements "modern Lisp expressions", aka mod-expressions.
; ; These implement "curly infix" and term-prefixing rules. E.G.:
; ;  [x y z]     => (x y z)
; ;  {3 + 4 + 5} => (+ 3 4 5)
; ;  f(x)        => (f x)
; ;  f{x + 3}    => (f (+ x 3)
; ;  x[z]        => (bracketaccess x z)
; ;
; ; Call "modern-read" to read a "modern Lisp expression", aka mod-expression.
; ;
; ; Copyright (C) 2008 by David A. Wheeler.
; ;
; ; NOTE: This would be really easy to implement in Scheme, except for one
; ; quirk: most Scheme implementations' "read" function CONSUMES [, ], {, and },
; ; instead of treating them as delimiters like space, (, or ).
; ; This is even true when the Scheme standards don't permit such characters
; ; at all, such as at the end of a number.
; ; The only solution is to re-implement "read" in Scheme, but one that
; ; DOES treat them as delimiters.  So a simple re-implemention is provided.
; ; If your Scheme _does_ treat these characters as delimiters,
; ; you can eliminate all of that re-implementation code, and just use your
; ; built-in "read" function (which probably has additional extensions that
; ; this simple reader does not).
; ;
; ; If you DO use an ordinary Scheme reader, there is a limitation:
; ; the vector notation #(...) could not contain modern notation.
; ; In code, just use vector(...) instead.  The best solution, of course,
; ; is to build this into your Scheme reader.
; ;
; ; You _could_ in a pinch use a standard Scheme reader that didn't
; ; consider {} or [] as delimiters.  But then closing characters } and ]
; ; must be PRECEDED by a delimiter like a space, and you CANNOT invoke
; ; prefixed [] and {} at all.
;
;
; ; Released under the "MIT license":
; ; Permission is hereby granted, free of charge, to any person obtaining a
; ; copy of this software and associated documentation files (the "Software"),
; ; to deal in the Software without restriction, including without limitation
; ; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; ; and/or sell copies of the Software, and to permit persons to whom the
; ; Software is furnished to do so, subject to the following conditions:
; ;
; ; The above copyright notice and this permission notice shall be included
; ; in all copies or substantial portions of the Software.
; ;
; ; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; ; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; ; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
; ; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; ; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; ; OTHER DEALINGS IN THE SOFTWARE.
;
;
; ; Configuration:
; (define modern-backwards-compatible #f) ; If true, "(" triggers old reader.
; (define modern-bracketaccess #t) ; If true, "f[...]" => [bracketaccess f ...]
;                                  ; if not, "f[...]" => [f ...].
;
; ; Preserve original read.
; (define old-read read)
;
; ; A few useful utilities:
;
; (define (debug-result marker value)
;   ; For debugging - you can insert this without adding let, etc., because
;   ; after printing it returns the original value.
;   (newline)
;   (display "DEBUG: ")
;   (display marker)
;   (display " ")
;   (write value)
;   (newline)
;   value)
;
; ; Define the tab character; a tab is immediately after the backslash.
; ; Unfortunately, this seems to be the only portable way to define the
; ; tab character in Scheme, so we'll do it once (here) and use it elsewhere.
; (define tab #\	)
;
; ; Unfortunately, since most Scheme readers will consume [, {, }, and ],
; ; we have to re-implement our own Scheme reader.  Ugh.
; ; If you fix your Scheme's "read" so that [, {, }, and ] are considered
; ; delimiters (and thus not consumed when reading symbols, numbers, etc.),
; ; you can just call old-read instead of using underlying-read below,
; ; with the limitation noted above about vector constants #(...).
; ; We WILL call old-read on string reading (that DOES seem to work
; ; in common cases, and lets us use the implementation's string extensions).
;
; (define modern-delimiters `(#\space #\newline #\( #\) #\[ #\] #\{ #\} ,tab))
;
; (define (read-until-delim port delims)
;   ; Read characters until eof or "delims" is seen; do not consume them.
;   ; Returns a list of chars.
;   (let ((c (peek-char port)))
;     (cond
;        ((eof-object? c) '())
;        ((ismember? (peek-char port) delims) '())
;        (#t (cons (read-char port) (read-until-delim port delims))))))
;
; (define (read-error message)
;   (display "Error: ")
;   (display message)
;   '())
;
; (define (read-number port starting-lyst)
;   (string->number (list->string
;     (append starting-lyst
;       (read-until-delim port modern-delimiters)))))
;
; (define (process-char port)
;   ; We've read #\ - returns what it represents.
;   (cond
;     ((eof-object? (peek-char port)) (peek-char port))
;     (#t
;       ; Not EOF. Read in the next character, and start acting on it.
;       (let ((c (read-char port))
;             (rest (read-until-delim port modern-delimiters)))
;         (cond
;           ((null? rest) c) ; only one char after #\ - so that's it!
;           (#t
;             (let ((rest-string (list->string (cons c rest))))
;               (cond
;                 ((string-ci=? rest-string "space") #\space)
;                 ((string-ci=? rest-string "newline") #\newline)
;                 ((string-ci=? rest-string "tab") tab) ; Scheme extension.
;                 (#t (read-error "Invalid character name"))))))))))
;
;
; (define (process-sharp port)
;   ; We've peeked a # character.  Returns what it represents.
;   ; Note: Since we have to re-implement process-sharp anyway,
;   ; the vector representation #(...) uses my-read-delimited-list, which in
;   ; turn calls modern-read2. Thus, modern-expressions CAN be used inside
;   ; a vector expression.
;   (read-char port) ; Remove #
;   (cond
;     ((eof-object? (peek-char port)) (peek-char port)) ; If eof, return eof.
;     (#t
;       ; Not EOF. Read in the next character, and start acting on it.
;       (let ((c (read-char port)))
;         (cond
;           ((char=? c #\t)  #t)
;           ((char=? c #\f)  #f)
;           ((ismember? c '(#\i #\e #\b #\o #\d #\x))
;             (read-number port (list #\# c)))
;           ((char=? c #\( )  ; Vector.
;             (list->vector (my-read-delimited-list #\) port)))
;           ((char=? c #\\) (process-char port))
;           (#t (read-error "Invalid #-prefixed string")))))))
;
; (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;
; (define (process-period port)
;   ; We've peeked a period character.  Returns what it represents.
;   (read-char port) ; Remove .
;   (let ((c (peek-char port)))
;     (cond
;       ((eof-object? c) '.) ; period eof; return period.
;       ((ismember? c digits)
;         (read-number port (list #\.)))  ; period digit - it's a number.
;       (#t
;         ; At this point, Scheme only requires support for "." or "...".
;         ; As an extension we can support them all.
;         (string->symbol (list->string (cons #\.
;           (read-until-delim port modern-delimiters))))))))
;
; (define (underlying-read port)
;   ; This tiny reader implementation REQUIRES a port value.
;   ; That way, while writing/modifying it, we
;   ; won't forget to pass the port to it.
;   ; Note: This reader is case-sensitive, which is consistent with R6RS
;   ; and guile, but NOT with R5RS.  Most people won't notice, and I
;   ; _like_ case-sensitivity.
;   (skip-whitespace port)
;   (let ((c (peek-char port)))
;     (cond
;       ((eof-object? c) c)
;       ((char=? c #\")      ; old readers tend to handle strings okay, call it.
;         (old-read port))   ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
;       ((ismember? c digits) ; Initial digit.
;         (read-number port '()))
;       ((char=? c #\#) (process-sharp port))
;       ((char=? c #\.) (process-period port))
;       ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
;         (read-char port)
;         (if (ismember? (peek-char port) digits)
;           (read-number port (list c))
;           (string->symbol (list->string (cons c
;             (read-until-delim port modern-delimiters))))))
;
;       ; We'll reimplement abbreviations, (, and ;.
;       ; These actually should be done by modern-read (and thus
;       ; we won't see them), but redoing it here doesn't cost us anything,
;       ; and it makes some kinds of testing simpler.  It also means that
;       ; this function is a fully-usable Scheme reader, and thus perhaps
;       ; useful for other purposes.
;       ((char=? c #\')
;         (read-char port)
;         (list 'quote
;           (underlying-read port)))
;       ((char=? c #\`)
;         (read-char port)
;         (list 'quasiquote
;           (underlying-read port)))
;       ((char=? c #\`)
;         (read-char port)
;           (cond
;             ((char=? #\@ (peek-char port))
;               (read-char port)
;               (list 'unquote-splicing
;                (underlying-read port)))
;            (#t
;             (list 'unquote
;               (underlying-read input-stream eof-error-p
;                             eof-value recursive-p)))))
;       ; The "(" calls modern-read, but since this one shouldn't normally
;       ; be used anyway (modern-read will get first crack at it), it
;       ; doesn't matter:
;       ((char=? c #\( )
;           (read-char port)
;           (my-read-delimited-list #\) port))
;       ((char=? c #\; )
;         (skip-line port)
;         (underlying-read port))
;       ((char=? c #\| )   ; Scheme extension, |...| symbol (like Common Lisp)
;         (read-char port) ; Skip |
;         (let ((newsymbol
;           (string->symbol (list->string
;             (read-until-delim port '(#\|))))))
;           (read-char port)
;           newsymbol))
;       (#t ; Nothing else.  Must be a symbol start.
;         (string->symbol (list->string
;           (read-until-delim port modern-delimiters)))))))
;
;
; ; End of Scheme reader re-implementation.
;
;
;
;
; ; Utility functions to implement the simple infix system:
;
; ; Return true if lyst has an even # of parameters, and the (alternating) first
; ; ones are "op".  Used to determine if a longer lyst is infix.
; ; Otherwise it returns false.
; ; If passed empty list, returns true (so recursion works correctly).
; (define (even-and-op-prefix op lyst)
;    (cond
;      ((null? lyst) #t)
;      ((not (pair? lyst)) #f) ; Not a list.
;      ((not (eq? op (car lyst))) #f) ; fail - operators not all equal?.
;      ((null? (cdr lyst)) #f) ; fail - odd # of parameters in lyst.
;      (#t (even-and-op-prefix op (cddr lyst))))) ; recurse.
;
; ; Return True if the lyst is in simple infix format (and should be converted
; ; at read time).  Else returns NIL.
; (define (simple-infix-listp lyst)
;   (and
;     (pair? lyst)           ; Must have list;  '() doesn't count.
;     (pair? (cdr lyst))     ; Must have a second argument.
;     (pair? (cddr lyst))    ; Must have a third argument (we check it
;                            ; this way for performance)
;     (symbol? (cadr lyst))  ; 2nd parameter must be a symbol.
;     (even-and-op-prefix (cadr lyst) (cdr lyst)))) ; even parameters equal??
;
; ; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
; (define (alternating-parameters lyst)
;   (if (or (null? lyst) (null? (cdr lyst)))
;     lyst
;     (cons (car lyst) (alternating-parameters (cddr lyst)))))
;
; ; Transform a simple infix list - move the 2nd parameter into first position,
; ; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
; (define (transform-simple-infix lyst)
;    (cons (cadr lyst) (alternating-parameters lyst)))
;
; (define (process-curly lyst)
;   (if (simple-infix-listp lyst)
;      (transform-simple-infix lyst) ; Simple infix expression.
;      (cons 'nfx lyst))) ; Non-simple; prepend "nfx" to the list.
;
;
; (define (my-read-delimited-list stop-char port)
;   ; like read-delimited-list of Common Lisp, but calls modern-read instead.
;   ; read the "inside" of a list until its matching stop-char, returning list.
;   ; This implements a common extension: (. b) return b.
;   ; That could be important for I-expressions, e.g., (. group)
;   (skip-whitespace port)
;   (let
;     ((c (peek-char port)))
;     (cond
;       ((eof-object? c) (read-error "EOF in middle of list") c)
;       ((char=? c stop-char)
;         (read-char port)
;         '()) ;(
;       ((ismember? c '(#\) #\] #\}))  (read-error "Bad closing character") c)
;       (#t
;         (let ((datum (modern-read2 port)))
;           (cond
;              ((eq? datum '.)
;                (let ((datum2 (modern-read2 port)))
;                  (skip-whitespace port)
;                  (cond
;                    ((not (eqv? (peek-char port) stop-char))
;                     (read-error "Bad closing character after . datum"))
;                    (#t
;                      (read-char port)
;                      datum2))))
;              (#t (cons datum
;                (my-read-delimited-list stop-char port)))))))))
;
; (define (my-is-whitespace c)
;   (ismember? c `(#\space #\newline ,tab)))
; ; TODO: Possibly support other whitespace chars, e.g.:
; ;    #\return
; ;   (code-char 10) (code-char 11)     ; LF, VT
; ;   (code-char 12) (code-char 13)))))  ; FF, CR
; ;   If so, also modify the "delimiters" list above.
;
;
; (define (skip-whitespace port)
;   ; Consume whitespace.
;   (cond
;     ((my-is-whitespace (peek-char port))
;       (read-char port)
;       (skip-whitespace port))))
;
; (define (modern-process-tail port prefix)
;     ; See if we've just finished reading a prefix, and if so, process.
;     ; This recurses, to handle formats like f(x)(y).
;     ; This implements prefixed (), [], and {}
;     (if (not (or (symbol? prefix) (pair? prefix)))
;       prefix  ; Prefixes MUST be symbol or cons; return original value.
;       (let ((c (peek-char port)))
;         (cond
;           ((eof-object? c) c)
;           ((char=? c #\( ) ; ).  Implement f(x).
;             (read-char port)
;             (modern-process-tail port ;(
;               (cons prefix (my-read-delimited-list #\) port))))
;           ((char=? c #\[ )  ; Implement f[x]
;             (read-char port)
;             (modern-process-tail port
;               (if modern-bracketaccess
;                 (cons 'bracketaccess (cons prefix
;                   (my-read-delimited-list #\] port)))
;                 (cons prefix (my-read-delimited-list #\] port)))))
;           ((char=? c #\{ )  ; Implement f{x}
;             (read-char port)
;             (modern-process-tail port
;               (list prefix
;                 (process-curly
;                   (my-read-delimited-list #\} port)))))
;           (#t prefix)))))
;
;
; (define (skip-line port)
;   ; Skip every character in the line - end on EOF or newline.
;   (let ((c (peek-char port)))
;     (cond
;       ((not (or (eof-object? c) (char=? c #\newline)))
;         (read-char port)
;         (skip-line port)))))
;
; (define (modern-read2 port)
;   ; Read using "modern Lisp notation".
;   ; This implements unprefixed (), [], and {}
;   (skip-whitespace port)
;   (modern-process-tail port
;     (let ((c (peek-char port)))
;       ; (display "modern-read2 peeked at: ")
;       ; (write c)
;       (cond
;         ; We need to directly implement abbreviations ', etc., so that
;         ; we retain control over the reading process.
;         ((eof-object? c) c)
;         ((char=? c #\')
;           (read-char port)
;           (list 'quote
;             (modern-read2 port)))
;         ((char=? c #\`)
;           (read-char port)
;           (list 'quasiquote
;             (modern-read2 port)))
;         ((char=? c #\,)
;           (read-char port)
;             (cond
;               ((char=? #\@ (peek-char port))
;                 (read-char port)
;                 (list 'unquote-splicing
;                  (modern-read2 port)))
;              (#t
;               (list 'unquote
;                 (modern-read2 port)))))
;         ((char=? c #\( ) ; )
;           (if modern-backwards-compatible
;             (underlying-read port)
;             (begin
;               (read-char port) ; (
;               (my-read-delimited-list #\) port))))
;         ((char=? c #\[ )
;             (read-char port)
;             (my-read-delimited-list #\] port))
;         ((char=? c #\{ )
;           (read-char port)
;           (process-curly
;             (my-read-delimited-list #\} port)))
;         ((char=? c #\; )  ; Handle ";" directly, so we don't lose control.
;           (skip-line port)
;           (modern-read2 port))
;         (#t (let ((result (underlying-read port)))
;                 ; (display "DEBUG result = ")
;                 ; (write result)
;                 ; (display "\nDEBUG peek after= ")
;                 ; (write (peek-char port))
;                 result))))))
;
;
;
