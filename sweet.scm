; Sweet-expression implementation in Scheme, 2007-10-10, version 0.15.
; Copyright (C) 2006-2007 by David A. Wheeler and Egil Möller.
;
; This prototype code implements a reader of "sweet-expressions",
; an s-expression
; format where traditional s-expressions are usually read as-is, but
; various extensions to improve program readability are allowed.
; It adds support for indentation (as per I-expressions),
; name-prefixed function names like fact(x), and it auto-detects
; infix expressions (if 3+ parameters, 1st parameter is not infix, and
; 2nd parameter IS infix, then it's infix).  To disable infix in an
; expression, surround the 2nd parameter with as(..).
; To make it pleasant to use interactively, if you enter a single complete
; datum as unindented text followed IMMEDIATELY by a newline, it's
; run immediately-  so x, load("filename"), and (3 + 4) run immediately.
;
; TODO: There's a subtle bug involving EOF interacting with convoluted
;       constructs involving inline comments; need to track it down and fix.
;       It appears to only occur when there are multiple indented lines, all of
;       which have inline ";" comments.  This MUST be fixed before serious use.
; TODO: Leading + - . for numbers doesn't work, e.g., -5 should be allowed.
; TODO: There are a few other TODOs, e.g., bare "." processing is needed.
; TODO: There are almost certainly other bugs; this is "testing" code.
;       I'm still trying out various ideas, and in the process of trying things
;       out, other stuff breaks.  Under construction, help wanted.
; TODO: Make more portable across diff. versions of guile and of Scheme,
;       then port to Common Lisp, ACL2, etc.
; TODO: Handling "empty" lines deserves more thinking.  E.G., perhaps a blank
;       line with at least one space or tab should be completely ignored
;       (instead of closing deeper indentations).  In some cases a '()
;       is returned from whitespace that perhaps should be ignored.  The
;       sweet-load routine does not exec '() anyway.  Perhaps comment-only
;       lines should NOT count for purposes of determining indent/outdent.
; TODO: If a single-token expressions is only followed by atmosphere on a line,
;       and it's unindented, accept and run it immediately (currently that
;       disables immediate execution).
; TODO: Add optional support for R6 changes, e.g., allowing [...] lists.
;
; This is written to work with GNU guile, as a module, though it's written
; to be as portable as practicable.  Start up by typing "guile", then;
;  (use-modules (sweet))
; Then you can use it:
;  (sweet-enable)  : switches to this mode from now on.
;  (sweet-disable) : undoes sweet-enable.
;  (sweet-load filename)    : load from a file in this mode.
;  (sweet-read)    : read just one sweet-expression, and return it.
;  (sweet-filter)  ; read from stdin and display s-expr translation
;  (sweet-filter-file filename)  ; read a file and display s-expr translation
; Indentation, name-prefixing, and infix all work, but the TODOs need completion.
; Precedence is intentionally NOT supported; see the docs for why. But
; chaining is supported, so (3 + 4 + 5) works as expected.
;
; Version information:
; 0.16: Repair some whitespace skipping code (more to do, see "errors") -
;       but at least it can now correctly read the example INCLUDING comments.
; 0.15: In sweet-load, don't try to exec '().
; 0.14: Add support for parameter chaining, e.g., (3 + 4 + 5); changed
;       terminology to discuss "name-prefixing" (vs. older "name-ending" term).
;
; Released under the "MIT license":
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
;
;  ----{ sweet.scm }----
; 
; Here is the grammar for the non-I-expressions (see the SFRI for
; I-expression grammar).  Note that this is slightly broader than the
; Scheme grammar, because we want to support local extensions. Thus
; this accepts symbols that are not REQUIRED to be supported by the Scheme
; spec.  In this implementation, we have two conflicting goals:
; (1) Allow running as a function without overriding "read"
; (which simplifies debugging), and (2) allow the use of local
; language extensions, particularly # stuff.  My solution is
; to have a function sweet-read that can be called directly,
; without overwriting "read", but which occasionally calls the
; underlying read implementation.  The side-effect is that
; sweet-read is disabled inside #... and string constants
; (because it calls the underlying reader).  If you set the "read"
; function to be sweet-read (as sweet-enable does), then
; no problem; this limitation is normally only important while debugging.
; 
; The grammar below is in LL(1) form, so it can be easily implemented
; using recursive descent (as this code does). Notation:
; "atm" is "atmosphere" per the Scheme spec.
; [ ... ] is optional (note the surrounding spaces);
; [...] is a regular expression (no surrounding spaces).
; Postfixed + is 1 or more, postfixed * is 0 or more, | means "or".
; 
; atm ::= newline | space | tab | semicolon anychar* newline
; 
; datum ::= '(' inside-list ')' |   ; Compound; check if infix, and if so, fix.
;           ( \' | \` | \, | ,@ ) datum |   ; abbreviation.
;                              ; quote, quasiquote, unquote, unquote-splicing
;           hashbegin | string | simplenumber | ; begin with #, ", and [0-9]
;                                               ; Call read-save to handle.
;           '.' ambiguousdot | '+' ambiguousplus | '-' ambiguousminus |
;           symbol [ ( inside-list ) ]
;                ; If (), name-prefixing; if it's infix, make ONE parameter.
;
; ambiguousminus ::= [0-9]... ;Call read-save to convert to number, then negate
; ambiguousminus ::= followed-by-atm-or-closing ; this is a lonely "-"
; ambiguousminus ::= followed-by-anything-else illegal, but quietly allow.
; ambiguousplus ::=.... just like ambiguousminus, but don't negate.
; 
; ambiguousdot ::= [0-9]+ #* [ letter [ '+' | '-' ] [0-9]+ ]
;                  ; Append "." as string, convert to number.
;                  ; I wish the syntax didn't allow numbers to begin with ".".
; ambiguousdot ::= \.\.    ; for ...
; ambiguousdot ::= (other non-atm) ; technically illegal, but quietly allow.
; ambiguousdot ::= atm     ; ILLEGAL, because not inside a list
;                  ; (This IS legal inside a list context, see below)
; 
; 
; inside-list ::= atm* [ morelist ]
; morelist ::= datum [ atm+ [ morelist | '.' atm+ datum atm* |
;                             '.' ambiguousdot morelist ] ]
;  ; Note: '.' is also in datum, but handle specially here.
;  ; inside lists is trickier than you might think!
;  ; This can be expanded into:
;  ; morelist ::= datum morelist-tail
;  ; morelist-tail ::= EMPTY | atm+ morelist-tail2
;  ; morelist-tail2 ::= EMPTY | morelist | '.' atm+ datum atm* |
;  ;                    '.' ambiguousdot morelist
;
; symbol ::= [ \ anychar |
;               any char but (,),atm,",#,  ]+
; abbreviation ::= ( \' | \` | \, | ,@ ) datum
; 
;
; Note that this LL(1) grammar is more complicated than you might
; expect, primarily because of: 
; * Leading ".": Could be a number (such as .5), the beginning of ...,
;   or a cons such as (4 . 5).
; * Leading "-": Could be a number (such as -5) or a function name, e.g.,
;                (- x 1) or -(x).
; * Leading "+": As with leading "-".
; Without these, all the "ambiguous" markers go away,
; datum and morelist become simple, and the basics are simply:
; 
; datum ::= '(' inside-list ')' | ( \' | \` | \, | ,@ ) datum |
;            hashbegin | string | number | symbol [ ( inside-list ) ]
;            Note: ENDS on ')', atmosphere (after beginning), EOF
; inside-list ::= atm* [ morelist ]
;            Note: ENDS on ')', EOF
; morelist ::= datum [ atm+ [ morelist | '.' atm+ datum atm* ] ]
;            Note: ENDS on ')', EOF
; 
; 
; 
; NOTE: 'define-module' is Guile-specific.
; Scheme has no standard module system, so this sample code
; prefixes almost everything with "sweet-" to prevent overlap
; with other names.
;
; NOTE: Use "eqv?", not "eq?", to compare characters; R5RS does not
; guarantee that two "eqv?" characters will match using "eq?".
; Use "memv" (not member or memq) to compare characters, same reason.


  (define-module (sweet))

  (define sweet-read-save read)
  (define sweet-load-save primitive-load)

  (define sweet-transform-infix #t)  ; If #f, NEVER transform infix expressions

  ; If #t, a complete datum starting on left-hand-edge will immediately
  ; return - this is pleasant for interactive use:
  (define sweet-edge-end #t) ;DEBUG

  ; Are all the characters in the list charlist allowed in an infix operator?
  ; TODO: Allow Unicode math symbols
  (define  (sweet-infix-operator-chars charlist)
    (cond
      ((null? charlist) #t)  ; Run out of characters, it's infix!
      ((not (memv (car charlist)
              '(#\+ #\- #\* #\/ #\< #\= #\> #\& #\| )))
        #f)   ; The front character is not in the permitted list.
      (#t (sweet-infix-operator-chars (cdr charlist))))) ; Look @ next char

  ; Is "op" a valid infix operator?
  (define  (sweet-infix-operator op)
    (cond
      ((not (symbol? op)) #f) ; Numbers, lists, etc. aren't symbols.
      ((eq? op '=>) #f) ; In Scheme, => is a predefined keyword and not infix.
      ((eq? op ':) #t)  ; Special case, ":" can be an infix operator.
      ((eq? op '|| ) #t)  ; Special case, "||" can be an infix operator.
      ; TODO: Check if on Unicode, length correct for UTF-8.
      ((> (string-length (symbol->string op)) 6)   #f) ; Infix ops <=6 chars
      ((= (string-length (symbol->string op)) 0)   #f) ; Ignore Null length
      ( (sweet-infix-operator-chars (string->list (symbol->string op)))
         #t)
      (#t #f)))       ; By default, op is NOT a valid infix operator.


  ; Return #t if lyst has an even # of parameters, and the (alternating) first
  ; ones are "op".  Used to determine if a longer lyst is infix.  Else #f.
  ; If passed empty list, returns #t.
  ; Probably should combine the DETECTION of infix lists with the CREATION
  ; of the final list (into a single function), but while various alternatives
  ; are being considered, I thought it might be safer to have separate functions.
  (define (even-and-op-prefix op lyst)
     (cond
       ((null? lyst) #t)
       ((not (pair? lyst)) #f)
       ; TODO: Detect if infix operator but different (no precedence!)
       ; (perhaps this is an error instead of silently being non-infix)
       ((not (eq? op (car lyst))) #f)  ; Not-equal operator.
       ((null? (cdr lyst)) #f) ; odd # of parameters in lyst.
       (else (even-and-op-prefix op (cddr lyst)))))

  ; Return #t if the lyst is in infix format (and should be converted)
  ; If 2nd param surrounded by as(), it won't be infix.
  (define (sweet-is-infix lyst)
    (and
      sweet-transform-infix  ; Are we transforming infix expressions?
      (pair? lyst)   ; Must have list;  '() doesn't count in Scheme.
      (pair? (cdr lyst))   ; Must have a second argument.
      (pair? (cddr lyst))   ; Must have a third argument (we check it
                            ; this way for performance)
      (not (sweet-infix-operator (car lyst)))
      (sweet-infix-operator (cadr lyst))
      (even-and-op-prefix (cadr lyst) (cdr lyst))))

  ; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  (define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
      lyst
      (cons (car lyst) (alternating-parameters (cddr lyst)))))

  ; The lyst is considered infix, so return transformed version:
  (define (sweet-infix-it lyst)
     ; Note: NO PRECEDENCE IS SUPPORTED, by intent.  See the docs.
     (cons (cadr lyst) (alternating-parameters lyst)))

  ; If 2nd parameter is "as(...)", remove it.
  (define (sweet-rm-no-infix lyst)
    (cond
      ((not (pair? lyst)) lyst)
      ((not (pair? (cdr lyst))) lyst)
      ((not (pair? (cddr lyst))) lyst)
      ((not (pair? (cadr lyst))) lyst)
      ; If 2nd param is as(..), reconstruct list without it, lifting contents.
      ((eq? (caadr lyst) 'as)
            (cons (car lyst) (cons (cadadr lyst) (cddr lyst))))
      (else lyst)))

  ; Look at lyst, and automatically interpret as infix if appropriate;
  ; else return lyst, removing the "as(...)" marking if there is one.
  (define (sweet-auto-infix lyst)
    (if (sweet-is-infix lyst)
        (sweet-infix-it lyst)
        (sweet-rm-no-infix lyst)))


; The following is the beginning of an implementation for "nfx(...)",
; with "unfx(...)" being very similar.  Not done, all experimental.
 
  ; If x is a singleton list in a list, return its car, else return x.
  ; Thus ((a))=>(a), else returns self.
  (define (sweet-solo x)
     (if (and (pair? x) (null? (cdr x))) (car x) x))

  ; Does list x have at least count elements? (not inc.
  ; the '3' of (1 2 . 3)?
  (define (list-min-count x kount)
     (cond
       ((<= kount 0) #t) ; A non-list is okay as a 0-length list (?).
       ((pair? x) (list-min-count (cdr x) (- kount 1)))
       (else #f)))

  ; CURRENTLY UNUSED - I tried this out; didn't seem to be worth the trouble.
  ;
  ; For x and down, interpret infixable expressions as infix;
  ; it's reversed by "unfx", continued by "nfx".  Also understands
  ; nfx1, unfx1, and nameprefix(...).
  ; Uses "sweet-is-infix" to see if a list is infix, and uses sweet-infix-it
  ; if it is.
  ; (nfx (3 + 2)) => (+ 3 2)   ; single list as argument becomes list.
  ; (nfx 3 + 2)   => (+ 3 2)
  ; (nfx (+ 3 2)) => (+ 3 2)
  ; (nfx (nameprefix f 3 + 2)) => (f (+ 3 2))
  (define (sweet-nfx-process . x)
    (cond
      ((not (pair? x))     x)
      ((eq? (car x) 'nfx)  (sweet-nfx-process (sweet-solo (cdr x))))
      ((eq? (car x) 'unfx) (sweet-unfx-process (sweet-solo (cdr x))))
      ((eq? (car x) 'nfx1)
        (if (not (sweet-is-infix (sweet-solo (cdr x))))
          (sweet-error "Not infix: " (cdr x))
          (map 'sweet-nfx-process (sweet-infix-it (sweet-solo (cdr x))))))
      ((eq? (car x) 'unfx1)
        (map 'sweet-nfx-process (sweet-nfx-process (sweet-solo (cdr x)))))
      ((eq? (car x) 'nameprefix) ; Do we have (nameprefix func 3 + 2)?
        (if (and (list-min-count x 5) (sweet-is-infix (cddr x)))
          (map 'sweet-nfx-process                      ; infix - 1 parameter!
            (list (cadr x) (sweet-infix-it (cddr x))))
          (map 'sweet-nfx-process (cdr x)))) ; not infix - normal handling.
      (else ; infix if can, then recurse inside each parm.
        (map 'sweet-nfx-process (sweet-auto-infix x)))))

; NOTE: Normally should do infix processing at read time, so that
; macros that "walk down the tree" all the way down will see operators
; in their usual places.

; Consume rest of line, until newline or end-of-file marker.
; It consumes the newline, if any.
  (define (sweet-skip-line port)
    (let ((char (read-char port)))
      (if (not (or
             (eqv? char #\newline)
             (eof-object? char)))
          (sweet-skip-line port))))

; If atmosphere on port, consume all of it. Doesn't return a useful value,
; its only purpose is the side-effect on the port.
  (define (sweet-skip-atmosphere port)
    (cond
      ((memv (peek-char port) '(#\space #\ht #\newline))
        (read-char port)
        (sweet-skip-atmosphere port))
      ((eqv? (peek-char port) #\; )
        (sweet-skip-line port)
        (sweet-skip-atmosphere port))))

; Support for readsymbol; return list of characters of symbol;
; stops reading on non-symbol.  For performance, this should probably
; eventually be rewritten to be tail-recursive and/or remove all the cons-ing.
; TODO: what should it do with chars '`,{}[]|!   Give an error?
; Currently this accepts far more chars as symbols than readers are REQUIRED
; to accept, but since the standard doesn't FORBID this, we'll just
; claim that accepting these other characters is a local extension.
; Note: STOPS unconsumed on ')', atmosphere (after beginning), EOF.
  (define (sweet-scansymbol port)
    (let ((char (peek-char port)))
      (cond
        ((memv char '(#\( #\) #\space #\ht #\newline #\; #\# #\" ))  '() )
        ((eof-object? char)  '() )
        ((eqv? char #\\ )
          (read-char port)  ; skip \.
          ; TODO: Handle \ before eof without intervening newline.
          ; It's an error, though incredibly unlikely in practice.
          (cons (read-char port) (sweet-scansymbol port)))
        ; TODO: forbid control chars & maybe some other characters.
        (#t
          (cons (read-char port) (sweet-scansymbol port))))))

; Read a symbol. If it's IMMEDIATELY followed by (), it's name-prefixed;
; if in that case it's infix, make it ONE parameter.
  (define (sweet-readsymbol port prefix)
   (let ((newsymboltext
            (string-append prefix (list->string (sweet-scansymbol port)))))
    (cond
      ((eqv? (peek-char port) #\( )  ; name-prefix symbol[ ( inside-list ) ]
        ; Not quite the same as non-name-prefixedbecause a cuddled infix
        ; expression must be interpreted as a SINGLE parameter.
        (read-char port)  ; Consume the opening paren.
        (let ((list-contents (sweet-inside-list port)))
          ; TODO: MUST be closing paren here - should check to be sure.
          (read-char port)  ; Consume the closing paren.
          (if (sweet-is-infix list-contents)
              (list (string->symbol newsymboltext)  ; Name-prefixing is different!
                    (sweet-infix-it list-contents))
              (cons (string->symbol newsymboltext) 
                    (sweet-rm-no-infix list-contents)))))
      (#t (string->symbol newsymboltext)))))

  ; Read an s-expression datum (the basic unit of s-expressions),
  ; IGNORING indentation.  Accept func(...) as equal to (func ...),
  ; and switch infix expressions to infix.
  (define (sweet-readdatum port)
    (sweet-skip-atmosphere port) ; Not strictly necessary, but good precaution.
                                 ; Permits space after quoting chars.
    (let ((char (peek-char port)))
      (case char
        ((#\()              ; Compound expression (a list)
          (read-char port)  ; Consume the opening paren.
          (let ((list-contents (sweet-inside-list port)))
            ; TODO: MUST be closing paren - check to be sure.
            (read-char port)  ; Consume the closing paren.
            (if (sweet-is-infix list-contents)
                (sweet-infix-it list-contents)
                (sweet-rm-no-infix list-contents))))
        ((#\') (read-char port) (list 'quote (sweet-readdatum port)))
        ((#\`) (read-char port) (list 'quasiquote (sweet-readdatum port)))
        ((#\,) (read-char port)
               (if (eqv? (peek-char port) #\@ )  ; Do we have ,@ ?
                 (begin
                   (read-char port)  ; remove the "@".
                   (list 'unquote-splicing (sweet-readdatum port))) ; NOT cons
                 (list 'unquote (sweet-readdatum port))))
        ((#\#) (sweet-read-save port)) ; Use old read for #stuff.
        ((#\") (sweet-read-save port)) ; Use old read for strings
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (sweet-read-save port)) ; Use old read for simple numbers
        ; TODO: Handle leading  . + -
        (else
          (if (eof-object? char)
             char  ; if eof, return eof to caller.
             (sweet-readsymbol port "" ))))))

; inside-list ::= atm* [ morelist ]
  (define (sweet-inside-list port)
    (sweet-skip-atmosphere port)
    (let ((char (peek-char port)))
      (if (eqv? char #\) )
         '()
         (sweet-morelist port))))

; morelist ::= datum [ atm+ [ morelist | '.' atm+ datum atm* ] ]
  (define (sweet-morelist port)
    (cons
      (sweet-readdatum port)
      (if (memv (peek-char port) '(#\space #\ht #\newline #\;))
         ; BIG TODO: Check for, and handle, '.' atm+ datum atm*
         (sweet-morelist port)
         '() )))


; Above this area, indentation is irrelevant.
; Below here, indentation is important.

  ; Convert '() to "".  We'll use '() as a special marker for
  ; "beginning of line" in the "level" value.
  (define (tochars level)
     (if (null? level) "" level))

  (define (sweet-readquote level port qt)
    (read-char port)
    (let ((char (peek-char port)))
      (if (or (eqv? char #\space)
	      (eqv? char #\newline)
	      (eqv? char #\ht))
	  (list qt)
	  (list qt (sweet-readitem level port)))))

  ; "readitem" considers indentation important, handling ' etc. with
  ; indenting logic if necessary.  It'll call readdatum in many cases,
  ; which transitions to ignoring indentation.
  (define (sweet-readitem level port)
    (let ((char (peek-char port)))
      (cond ; handle quoters specially - they may exploit indentation.
       ((eqv? char #\`)
	(sweet-readquote level port 'quasiquote))
       ((eqv? char #\')
	(sweet-readquote level port 'quote))
       ((eqv? char #\,)
	(sweet-readquote level port 'unquote))
        ; TODO: Check - Do we need to handle ,@ specially too?
       (#t
        (sweet-readdatum port))))) ; use sweet-read-save to disable readdatum

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length (tochars indentation1)))
	  (len2 (string-length (tochars indentation2))))
      (and (> len1 len2)
	   (string=? (tochars indentation2)
                     (substring (tochars indentation1) 0 len2)))))

  (define (indentationlevel port)
    (define (indentationlevel)
      (if (or (eqv? (peek-char port) #\space)
	      (eqv? (peek-char port) #\ht))
	  (cons 
	   (read-char port)
	   (indentationlevel))
	  '()))
    (list->string (indentationlevel)))

  (define (sweet-clean line)
    (cond
     ((not (pair? line))
      line)
     ((null? line)
      line)
     ((eq? (car line) 'group)
      (cdr line))
     ((null? (car line))
      (cdr line))
     ((list? (car line))
      (if (or (equal? (car line) '(quote))
	      (equal? (car line) '(quasiquote))
	      (equal? (car line) '(unquote)))
	  (if (and (list? (cdr line))
		   (= (length (cdr line)) 1))
	      (cons
	       (car (car line))
	       (cdr line))
	      (list
	       (car (car line))
	       (cdr line)))
	  (cons
	   (sweet-clean (car line))
	   (cdr line))))
     (#t
      line)))

  ;; Reads all subblocks of a block
  (define (readblocks level port)
    (let* ((read (readblock-clean level port))
	   (next-level (car read))
	   (block (cdr read)))
      (if (string=? next-level level)
	  (let* ((reads (readblocks level port))
		 (next-next-level (car reads))
		 (next-blocks (cdr reads)))
	    (if (eq? block '.)
		(if (pair? next-blocks)
		    (cons next-next-level (car next-blocks))
		    (cons next-next-level next-blocks))
		(cons next-next-level (cons block next-blocks))))
	  (cons next-level (list block)))))

  ;; Read one block of input
  ;; "level" is the indentation so far, OR
  ;;  '() if it's the first param cuddled to the left-hand edge.
  ;;  "firstinline" is true if we're the first item in the line.
  (define (readblock level port firstinline)
    (let ((char (peek-char port)))
      (cond
       ((eof-object? char)
	(cons -1 char))
       ((eqv? char #\newline)
	(read-char port)
	(if (null? level) ; beginning of expression?
         (readblock level port #t) ; yes, skip blank lines.
	 (let ((next-level (indentationlevel port)))
	  (if (indentation>? next-level level)
	      (readblocks next-level port)
	      (cons next-level '())))))
       ((or (eqv? char #\space)
	    (eqv? char #\ht))
	(read-char port)
	(readblock (tochars level) port firstinline))
       ((eqv? char #\;)   ; Handle comments - not in original I-expr code.
        (sweet-skip-line port)
	(readblock (tochars level) port #t))
       (#t
	(let* ((first (sweet-readitem level port)))
	  (if (and sweet-edge-end ; alternative: null OR "" level.
                   (null? level) (eqv? (peek-char port) #\newline))
            ; Immediate return - don't consume newline, or peek will block
            (cons '() (list first))
	    (let* ((rest (readblock level port #f))
	         (level (car rest))
	         (block (cdr rest)))
	      (if (eq? first '.)
	        (if (pair? block)
		  (cons level (car block))
		  rest)
 	        (if firstinline ; if first in line, check for infix.
 	          (cons level (sweet-auto-infix (cons first block)))
 	          (cons level (cons first block)))))))))))

  ;; reads a block and handles group, (quote), (unquote) and
  ;; (quasiquote).
  (define (readblock-clean level port)
    (let* ((read (readblock level port #t))
	   (next-level (car read))
	   (block (cdr read)))
      (if (eof-object? block) (cons -1 block)) ; New: return EOF if EOF.
      (if (or (not (list? block)) (> (length block) 1))
	  (cons next-level (sweet-clean block))
	  (if (= (length block) 1)
	      (cons next-level (car block))
	      (cons next-level '.)))))

  (define (sweet-read . port)
    (let* ((read (readblock-clean '() (if (null? port)  ; '() level.
					(current-input-port)
					(car port))))
	   (level (car read))
	   (block (cdr read)))
      (cond
       ((eq? block '.)
	'())
       (#t
	block))))

  ; Continuously read sweet-expressions from port and print their
  ; s-expression translation to stdout.
  (define (sweet-translate-port port)
     (let ((result (sweet-read port)))
       (if (eof-object? result)
	    result
            (begin
              (write result)
              (newline)
              (sweet-translate-port port)))))

  ; Continuously read sweet-expressions from stdin and print their
  ; s-expression translation to stdout.
  (define (sweet-filter)
     (let ((result (sweet-translate-port (current-input-port))))
	(if (eof-object? result)
	    result
            (sweet-filter))))

  ; Read the sweet-expressions from "filename" and print their
  ; s-expression translations to stdout.
  (define (sweet-filter-file filename)
    (call-with-input-file filename sweet-translate-port))

  (define (sweet-load filename)
    (define (load port)
      (let ((inp (sweet-read port))) ; sweet-read, not read.
	(if (eof-object? inp)
	    #t ; Return if done (should EOF be returned?)
	    (begin
              (if (not (null? inp))  ; Don't try to evaluate '().
                 (eval inp (interaction-environment))) ; TODO: CHANGE env?
              (load port)))))
    (load (open-input-file filename)))


  (define (sweet-enable)
    (set! read sweet-read)
    (set! primitive-load sweet-load))

  (define (sweet-disable)
    (set! read sweet-read-save)
    (set! primitive-load sweet-load-save))


; Guile-specific - declare these as public interface.
; Public interface - Guile's approach
  (export sweet-read-save sweet-load-save
          sweet-read sweet-load sweet-enable sweet-disable
          sweet-translate-port sweet-filter sweet-filter-file
          ; DEBUG: Remove these for production...
          sweet-readsymbol sweet-scansymbol sweet-readdatum
          sweet-infix-operator sweet-infix-operator-chars sweet-is-infix
          sweet-rm-no-infix sweet-infix-it sweet-auto-infix
          sweet-readitem sweet-skip-line sweet-skip-atmosphere
          sweet-inside-list sweet-morelist sweet-readquote sweet-readitem
          sweet-clean
          readblock-clean readblock readblocks)


; DEBUG: Don't auto-enable.
; (sweet-enable)

; ----{ sweet.scm }----
; This is based on Implementation of SRFI 49, directly from SRFI 49 at:
; http://srfi.schemers.org/srfi-49/srfi-49.html
; SRFI 49 implements an "Indentation-sensitive syntax" for Scheme,
; called I-expressions.
; Sweet-expressions built on I-expressions, but add support for
; name-prefixed calls and infix, and also add some additional
; semantics based on lessons learned using I-expressions
; (if a term begins at the left edge, and ends with a newline, it's
;  run immediately so that interactive use is comfortable).

