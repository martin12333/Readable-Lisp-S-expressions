; This is a simplified reference implementation of a curly-infix and
; neoteric reader, intended for a SRFI submission.
; If run, it invokes a curly-infix-reader
; (inside {...}, it accepts neoteric expressions).

  ; ------------------------------
  ; Curly-infix support procedures
  ; ------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix? op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f)
      ((not (equal? op (car lyst))) #f) ; fail - operators not the same
      ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
      (#t   (even-and-op-prefix? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  (define (simple-infix-list? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest is simple

  ; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  (define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
      lyst
      (cons (car lyst) (alternating-parameters (cddr lyst)))))

  ; Not a simple infix list - transform it.  Written as a separate procedure
  ; so that future experiments or SRFIs can easily replace just this piece.
  (define (transform-mixed-infix lyst)
     (cons 'nfx lyst))

  ; Given curly-infix lyst, map it to its final internal format.
  (define (process-curly lyst)
    (cond
     ((not (pair? lyst)) lyst) ; E.G., map {} to ().
     ((null? (cdr lyst)) ; Map {a} to a.
       (car lyst))
     ((and (pair? (cdr lyst)) (null? (cddr lyst))) ; Map {a b} to (a b).
       lyst)
     ((simple-infix-list? lyst) ; Map {a OP b [OP c...]} to (OP a b [c...])
       (cons (cadr lyst) (alternating-parameters lyst)))
     (#t  (transform-mixed-infix lyst))))


  ; ------------------------------------------------
  ; Key procedures to implement neoteric-expressions
  ; ------------------------------------------------

  ; Read the "inside" of a list until its matching stop-char, returning list.
  ; stop-char needs to be closing paren, closing bracket, or closing brace.
  ; This is like read-delimited-list of Common Lisp.
  ; This implements a useful extension: (. b) returns b.
  (define (my-read-delimited-list my-read stop-char port)
    (let*
      ((c   (peek-char port)))
      (cond
        ((eof-object? c) (read-error "EOF in middle of list") '())
        ((eqv? c #\;)
          (consume-to-eol port)
          (my-read-delimited-list my-read stop-char port))
        ((my-char-whitespace? c)
          (read-char port)
          (my-read-delimited-list my-read stop-char port))
        ((char=? c stop-char)
          (read-char port)
          '())
        ((or (eq? c #\)) (eq? c #\]) (eq? c #\}))
          (read-char port)
          (read-error "Bad closing character"))
        (#t
          (let ((datum (my-read port)))
            (cond
               ((eq? datum '.)
                 (let ((datum2 (my-read port)))
                   (consume-whitespace port)
                   (cond
                     ((eof-object? datum2)
                      (read-error "Early eof in (... .)\n")
                      '())
                     ((not (eqv? (peek-char port) stop-char))
                      (read-error "Bad closing character after . datum"))
                     (#t
                       (read-char port)
                       datum2))))
               (#t
                   (cons datum
                     (my-read-delimited-list my-read stop-char port)))))))))


  ; Implement neoteric-expression's prefixed (), [], and {}.
  ; At this point, we have just finished reading some expression, which
  ; MIGHT be a prefix of some longer expression.  Examine the next
  ; character to be consumed; if it's an opening paren, bracket, or brace,
  ; then the expression "prefix" is actually a prefix.
  ; Otherwise, just return the prefix and do not consume that next char.
  ; This recurses, to handle formats like f(x)(y).
  (define (neoteric-process-tail port prefix)
      (let* ((c (peek-char port)))
        (cond
          ((eof-object? c) prefix)
          ((char=? c #\( ) ; Implement f(x).
            (read-char port)
            (neoteric-process-tail port
                (cons prefix (my-read-delimited-list neoteric-read-real #\) port))))
          ((char=? c #\[ )  ; Implement f[x]
            (read-char port)
            (neoteric-process-tail port
                  (cons 'bracketaccess
                    (cons prefix
                      (my-read-delimited-list neoteric-read-real #\] port)))))
          ((char=? c #\{ )  ; Implement f{x}. Balance }
            (neoteric-process-tail port
              (let ((tail (neoteric-read port)))
                (if (eqv? tail '())
                  (list prefix) ; Map f{} to (f), not (f ()).
                  (list prefix tail)))))
          (#t prefix))))


  ; To implement neoteric-expressions, modify the reader so
  ; that [] and {} are also delimiters, and make the reader do this:
  ; (let* ((prefix
  ;           read-expression-as-usual
  ;       ))
  ;   (if (eof-object? prefix)
  ;     prefix
  ;     (neoteric-process-tail port prefix)))


  ; ------------------------------------------------
  ; Demo procedures to implement curly-infix and neoteric readers
  ; ------------------------------------------------

  ; This implements an entire reader, as a demonstration, but if you can
  ; update your existing reader you should just update that instead.
  ; This is a simple R5RS reader, with a few minor (common) extensions.
  ; The "my-read" is called if it has to recurse.
  (define (underlying-read my-read port)
    (let* ((c (peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\;)
          (consume-to-eol port)
          (my-read port))
        ((my-char-whitespace? c)
          (read-char port)
          (my-read port))
        ((char=? c #\( )
          (read-char port)
          (my-read-delimited-list my-read #\) port))
        ((char=? c #\[ )
          (read-char port)
          (my-read-delimited-list my-read #\] port))
        ((char=? c #\{ )
          (read-char port)
          (process-curly
              (my-read-delimited-list neoteric-read-real #\} port)))
        ; Handle missing (, [, { :
        ((char=? c #\) )
          (read-char port)
          (read-error "Closing parenthesis without opening")
          (my-read port))
        ((char=? c #\] )
          (read-char port)
          (read-error "Closing bracket without opening")
          (my-read port))
        ((char=? c #\} )
          (read-char port)
          (read-error "Closing brace without opening")
          (my-read port))
        ((char=? c #\") ; Strings are delimited by ", so can call directly
          (default-scheme-read port))
        ((char=? c #\')
          (read-char port)
          (list 'quote (my-read port)))
        ((char=? c #\`)
          (read-char port)
          (list 'quasiquote (my-read port)))
        ((char=? c #\,)
          (read-char port)
            (cond
              ((char=? #\@ (peek-char port))
                (read-char port)
                (list 'unquote-splicing (my-read port)))
              (#t
                (list 'unquote (my-read port)))))
        ((ismember? c digits) ; Initial digit.
          (read-number port '()))
        ((char=? c #\#) (process-sharp my-read port))
        ((char=? c #\.) (process-period port))
        ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
          (read-char port)
          (if (ismember? (peek-char port) digits)
            (read-number port (list c))
            (string->symbol (fold-case-maybe port
              (list->string (cons c
                (read-until-delim port neoteric-delimiters)))))))
        (#t ; Nothing else.  Must be a symbol start.
          (string->symbol (fold-case-maybe port
            (list->string
              (read-until-delim port neoteric-delimiters))))))))

  (define (curly-infix-read-real port)
    (underlying-read curly-infix-read-real port))

  (define (curly-infix-read . port)
    (if (null? port)
      (curly-infix-read-real (current-input-port))
      (curly-infix-read-real (car port))))

  ; Here's a real neoteric reader.
  ; The key part is that it implements [] and {} as delimiters, and
  ; after it reads in some datum (the "prefix"), it calls
  ; neoteric-process-tail to see if there's a "tail".
  (define (neoteric-read-real port)
    (let* ((prefix (underlying-read neoteric-read-real port)))
      (if (eof-object? prefix)
        prefix
        (neoteric-process-tail port prefix))))

  (define (neoteric-read . port)
    (if (null? port)
      (neoteric-read-real (current-input-port))
      (neoteric-read-real (car port))))


  ; ------------------
  ; Support procedures
  ; ------------------

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define linefeed (integer->char #x000A))        ; #\newline aka \n.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define tab (integer->char #x0009))
  (define line-tab (integer->char #x000b))
  (define form-feed (integer->char #x000c))
  (define line-ending-chars (list linefeed carriage-return))
  (define whitespace-chars
    (list tab linefeed line-tab form-feed carriage-return #\space))

  ; Should we fold case of symbols by default?
  ; #f means case-sensitive (R6RS); #t means case-insensitive (R5RS).
  ; Here we'll set it to be case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, I
  ; _like_ case-sensitivity, and the latest spec is case-sensitive,
  ; so let's start with #f (case-sensitive).
  ; This doesn't affect character names; as an extension,
  ; We always accept arbitrary case for them, e.g., #\newline or #\NEWLINE.
  (define foldcase-default #f)

  ; Returns a true value (not necessarily #t) if char ends a line.
  (define (char-line-ending? char) (memq char line-ending-chars))

  ; Returns true if item is member of lyst, else false.
  (define (ismember? item lyst)
     (pair? (member item lyst)))

  ; Create own version, in case underlying implementation omits some.
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (ismember? c whitespace-chars)))

  ; If fold-case is active on this port, return string "s" in folded case.
  ; Otherwise, just return "s".  This is needed to support our
  ; foldcase-default configuration value when processing symbols.
  ; The "string-foldcase" procedure isn't everywhere,
  ; so we use "string-downcase".
  (define (fold-case-maybe port s)
    (if foldcase-default
      (string-downcase s)
      s))

  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (peek-char port)))
      (cond
        ((not (or (eof-object? c)
                  (char-line-ending? c)))
          (read-char port)
          (consume-to-eol port)))))

  (define (consume-whitespace port)
    (let ((char (peek-char port)))
      (cond
        ((eof-object? char) char)
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (read-char port)
          (consume-whitespace port)))))

  ; Identifying the list of delimiter characters is harder than you'd think.
  ; This list is based on R6RS section 4.2.1, while adding [] and {},
  ; but removing "#" from the delimiter set.
  ; NOTE: R6RS has "#" has a delimiter.  However, R5RS does not, and
  ; R7RS probably will not - http://trac.sacrideo.us/wg/wiki/WG1Ballot3Results
  ; shows a strong vote AGAINST "#" being a delimiter.
  ; Having the "#" as a delimiter means that you cannot have "#" embedded
  ; in a symbol name, which hurts backwards compatibility, and it also
  ; breaks implementations like Chicken (has many such identifiers) and
  ; Gambit (which uses this as a namespace separator).
  ; Thus, this list does NOT have "#" as a delimiter, contravening R6RS
  ; (but consistent with R5RS, probably R7RS, and several implementations).
  ; Also - R7RS draft 6 has "|" as delimiter, but we currently don't.
  (define neoteric-delimiters
     (append (list #\( #\) #\[ #\] #\{ #\}  ; Add [] {}
                   #\" #\;)                 ; Could add #\# or #\|
             whitespace-chars))

  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (peek-char port)))
      (cond
         ((eof-object? c) '())
         ((ismember? c delims) '())
         (#t (cons (read-char port) (read-until-delim port delims))))))

  (define (read-error message)
    (display "Error: ")
    (display message)
    (display "\n")
    '())

  (define (read-number port starting-lyst)
    (string->number (list->string
      (append starting-lyst
        (read-until-delim port neoteric-delimiters)))))

  ; detect #| or |#
  (define (nest-comment port)
    (let ((c (read-char port)))
      (cond
        ((eof-object? c))
        ((char=? c #\|)
          (let ((c2 (peek-char port)))
            (if (char=? c2 #\#)
                (read-char port)
                (nest-comment port))))
        ((char=? c #\#)
          (let ((c2 (peek-char port)))
            (if (char=? c2 #\|)
                (begin
                  (read-char port)
                  (nest-comment port)))
            (nest-comment port)))
        (#t
          (nest-comment port)))))

  (define (process-sharp my-read port)
    ; We've peeked a # character.  Returns what it represents.
    (read-char port) ; Remove #
    (cond
      ((eof-object? (peek-char port)) (peek-char port)) ; If eof, return eof.
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port)))
          (cond
            ((char-ci=? c #\t)  #t)
            ((char-ci=? c #\f)  #f)
            ((ismember? c '(#\i #\e #\b #\o #\d #\x
                            #\I #\E #\B #\O #\D #\X))
              (read-number port (list #\# (char-downcase c))))
            ((char=? c #\( )  ; Vector.
              (list->vector (my-read-delimited-list my-read #\) port)))
            ((char=? c #\\) (process-char port))
            ; This supports SRFI-30 #|...|#
            ((char=? c #\|) (nest-comment port) (my-read port))
            ; If #!xyz, consume xyz and recurse.
            ; In a real reader, consider handling "#! whitespace" per SRFI-22,
            ; and consider "#!" followed by / or . as a comment until "!#".
            ((char=? c #\!) (my-read port) (my-read port))
            (#t (read-error "Unsupported # extension")))))))

  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (read-char port) ; Remove .
    (let ((c (peek-char port)))
      (cond
        ((eof-object? c) '.) ; period eof; return period.
        ((ismember? c digits)
          (read-number port (list #\.)))  ; period digit - it's a number.
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol
            (fold-case-maybe port
              (list->string (cons #\.
                (read-until-delim port neoteric-delimiters)))))))))

  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (peek-char port)) (peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t
              (let ((rest-string (list->string (cons c rest))))
                (cond
                  ; Implement R6RS character names, see R6RS section 4.2.6.
                  ; As an extension, we will ALWAYS accept character names
                  ; of any case, no matter what the case-folding value is.
                  ((string-ci=? rest-string "space") #\space)
                  ((string-ci=? rest-string "newline") #\newline)
                  ((string-ci=? rest-string "tab") tab)
                  ((string-ci=? rest-string "nul") (integer->char #x0000))
                  ((string-ci=? rest-string "alarm") (integer->char #x0007))
                  ((string-ci=? rest-string "backspace") (integer->char #x0008))
                  ((string-ci=? rest-string "linefeed") (integer->char #x000A))
                  ((string-ci=? rest-string "vtab") (integer->char #x000B))
                  ((string-ci=? rest-string "page") (integer->char #x000C))
                  ((string-ci=? rest-string "return") (integer->char #x000D))
                  ((string-ci=? rest-string "esc") (integer->char #x001B))
                  ((string-ci=? rest-string "delete") (integer->char #x007F))
                  ; Additional character names as extensions:
                  ((string-ci=? rest-string "ht") tab)
                  ((string-ci=? rest-string "cr") (integer->char #x000d))
                  ((string-ci=? rest-string "bs") (integer->char #x0008))
                  (#t (read-error "Invalid character name"))))))))))


  ; Record the original read location, in case it's changed later:
  (define default-scheme-read read)

  ; --------------
  ; Demo of reader
  ; --------------

  ; repeatedly read in curly-infix and write traditional s-expression.
  (define (process-input)
    (let ((result (curly-infix-read)))
      (cond
        ((not (eof-object? result))
          (write result)
          (display "\n")
          ; (force-output) ; flush, so can interactively control something else
          (process-input)))))

  (process-input)

