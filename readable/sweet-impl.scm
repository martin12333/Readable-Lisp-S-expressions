; sweet-impl.scm
; Implementation of the sweet-expressions project by readable mailinglist.
;
; Copyright (C) 2005-2012 by Egil Möller, David A. Wheeler,
;   and Alan Manuel K. Gloria.
;
; This software is released as open source software under the "MIT" license:
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

; This file includes code from SRFI-49, but significantly modified.

; -----------------------------------------------------------------------------
; Compatibility Layer
; -----------------------------------------------------------------------------
; The compatibility layer is composed of:
;
;   (module-contents (exports ...) body ...)
;   - a macro that should package the given body as a module, or whatever your
;     scheme calls it (chicken eggs?), preferably with one of the following
;     names, in order of preference, depending on your Scheme's package naming
;     conventions/support
;       (readable sweet-impl)
;       readable/sweet-impl
;       sweet-impl
;       sweetimpl
;   - The first element after the module-contents name is a list of exported
;     functions.  This module shall never export a macro or syntax, not even
;     in the future.
;
;   (my-peek-char port)
;   (my-read-char port)
;   (my-unread-char port)
;   - Performs I/O on a "port" object.
;   - The algorithm assumes that port objects have the following abilities:
;     * The port supports 2-character lookahead, by allowing
;       (my-unread-char port) after a (my-peek-char port).
;       See the R5RS portability layer for a way of implementing
;       this.
;     * The port automatically keeps track of source location
;       information.  On R5RS there is no source location
;       information that can be attached to objects, so as a
;       fallback you can just ignore source location, which
;       will make debugging using sweet-expressions more
;       difficult.
;   - "port" or fake port objects are created by the make-read function
;     below.
;   
;   (make-read function)
;   - The given function accepts exactly 1 argument, a "fake port" that can
;     be passed to my-peek-char et al.
;   - make-read creates a new function that supports your Scheme's reader
;     interface.  Usually, this means making a new function that accepts
;     either 0 or 1 parameters, defaulting to (current-input-port).
;   - If your Scheme doesn't support unlimited lookahead, you should make
;     the fake port that supports 2-char lookahead at this point.
;   - If your Scheme doesn't keep track of source location information
;     automatically with the ports, you may again need to wrap it here.
;   - If your Scheme needs a particularly magical incantation to attach
;     source information to objects, then you might need to use a weak-key
;     table in the attach-sourceinfo function below and then use that
;     weak-key table to perform the magical incantation.
;
;   (invoke-read read port)
;   - Accepts a read function, which is a (most likely built-in) function
;     that requires a *real* port, not a fake one.
;   - Should unwrap the fake port to a real port, then invoke the given
;     read function on the actual real port.
;
;   (get-sourceinfo port)
;   - Given a fake port, constructs some object (which the algorithm treats
;     as opaque) to represent the source information at the point that the
;     port is currently in.
;
;   (attach-sourceinfo pos obj)
;   - Attaches the source information pos, as constructed by get-sourceinfo,
;     to the given obj.
;   - obj can be any valid Scheme object.  If your Scheme can only track
;     source location for a subset of Scheme object types, then this function
;     should handle it gracefully.
;   - Returns an object with the source information attached - this can be
;     the same object, or a different object that should look-and-feel the
;     same as the passed-in object.
;   - If source information cannot be attached anyway (your Scheme doesn't
;     support attaching source information to objects), just return the
;     given object.
;
;   (replace-read-with f)
;   - Replaces your Scheme's current reader.
;   - Replace 'read and 'get-datum at the minimum.  If your Scheme
;     needs any kind of involved magic to handle load and loading
;     modules correctly, do it here.

; On Guile 2.0, the define-module part needs to occur separately from
; the rest of the compatibility checks, unfortunately.  Sigh.
(cond-expand
  (guile
    ; define the module
    ; this ensures that the user's module does not get contaminated with
    ; our compatibility functions/macros
    (define-module (readable sweet-impl))))
(cond-expand
; -----------------------------------------------------------------------------
; Guile Compatibility
; -----------------------------------------------------------------------------
  (guile

    ; properly get bindings
    (use-modules (guile))

    ; On Guile 1.x defmacro is the only thing supported outside-the-box
    ; This form still exists in Guile 2.x, fortunately.
    (defmacro module-contents (exports . body)
      `(begin (export ,@exports)
              ,@body))

    ; Guile was the original development environment, so the algorithm
    ; practically acts as if it is in Guile.
    ; Needs to be lambdas because otherwise Guile 2.0 acts strangely,
    ; getting confused on the distinction between compile-time,
    ; load-time and run-time (apparently, peek-char is not bound
    ; during load-time).
    (define (my-peek-char p)     (peek-char p))
    (define (my-read-char p)     (read-char p))
    (define (my-unread-char c p) (unread-char c p))

    (define (make-read f)
      (lambda args
        (let ((port (if (null? args) (current-input-port) (car args))))
          (f port))))

    (define (invoke-read read port)
      (read port))

    ; create a list with the source information
    (define (get-sourceinfo port)
      (list (port-filename port)
            (port-line port)
            (port-column port)))
    ; destruct the list and attach, but only to cons cells, since
    ; only that is reliably supported across Guile versions.
    (define (attach-sourceinfo pos obj)
      (cond
        ((pair? obj)
          (set-source-property! obj 'filename (list-ref pos 0))
          (set-source-property! obj 'line     (list-ref pos 1))
          (set-source-property! obj 'column   (list-ref pos 2))
          obj)
        (#t
          obj)))

    ; To properly hack into 'load and in particular 'use-modules,
    ; we need to hack into 'primitive-load.  On 1.8 and 2.0 there
    ; is supposed to be a current-reader fluid that primitive-load
    ; hooks into, but it seems (unverified) that each use-modules
    ; creates a new fluid environment, so that this only sticks
    ; on a per-module basis.  But if the project is primarily in
    ; sweet-expressions, we would prefer to have that hook in
    ; *all* 'use-modules calls.  So our primitive-load uses the
    ; 'read global variable if current-reader isn't set.

    (define %sugar-current-load-port #f)
    ; replace primitive-load
    (define primitive-load-replaced #f)
    (define (setup-primitive-load)
      (cond
        (primitive-load-replaced
           (values))
        (#t
          (module-set! (resolve-module '(guile)) 'primitive-load
            (lambda (filename)
              (let ((hook (cond
                            ((not %load-hook)
                              #f)
                            ((not (procedure? %load-hook))
                              (error "value of %load-hook is neither procedure nor #f"))
                            (#t
                              %load-hook))))
                (cond
                  (hook
                    (hook filename)))
                (let* ((port      (open-input-file filename))
                       (save-port port))
                  (define (load-loop)
                    (let* ((the-read
                             (or
                                 ; current-reader doesn't exist on 1.6
                                 (if (string=? "1.6" (effective-version))
                                     #f
                                     (fluid-ref current-reader))
                                 read))
                           (form (the-read port)))
                      (cond
                        ((not (eof-object? form))
                          ; in Guile only
                          (primitive-eval form)
                          (load-loop)))))
                  (define (swap-ports)
                    (let ((tmp %sugar-current-load-port))
                      (set! %sugar-current-load-port save-port)
                      (set! save-port tmp)))
                  (dynamic-wind swap-ports load-loop swap-ports)
                  (close-input-port port)))))
          (set! primitive-load-replaced #t))))

    (define (replace-read-with f)
      (setup-primitive-load)
      (set! read f))

    )
; -----------------------------------------------------------------------------
; R5RS Compatibility
; -----------------------------------------------------------------------------
  (else
    ; assume R5RS with define-syntax

    ; On R6RS, and other Scheme's, module contents must
    ; be entirely inside a top-level module structure.
    ; Use module-contents to support that.  On Scheme's
    ; where module declarations are separate top-level
    ; expressions, we expect module-contents to transform
    ; to a simple (begin ...), and possibly include
    ; whatever declares exported stuff on that Scheme.
    (define-syntax module-contents
      (syntax-rules ()
        ((module-contents exports body ...)
          (begin body ...))))

    ; We still need the my-* functions below even with the
    ; "!" as whitespace rule.  The algorithm acts as if the
    ; "port" automatically keeps track of source position.
    ; On Schemes where that is not true (e.g. Racket, where
    ; source information is passed into a reader and the
    ; reader is supposed to update it by itself) we can wrap
    ; the port with the source information, and update that
    ; source information in the my-* functions.

    ; R5RS only allows 1 character lookahead, but
    ; the "." as indentation rule needs 2.  Fortunately
    ; the indentation processor is relatively
    ; circumscribed, so that on exiting a reader
    ; we are (almost?) sure that there is no 2-character
    ; lookahead.
    (define (my-peek-char port)
      (let ((real-port (cdr port))
            (buffer    (car port)))
        (if buffer
            buffer
            (peek-char real-port))))
    ; handle 2nd character buffer
    (define (my-unread-char c port)
      (let ((buffer (car port)))
        (if buffer
            (error "internal error in sweet-impl, too many unreads")
            (set-car! port c)))
      ; return nothing of consequence
      (values))
    ; clear 2nd character buffer if set
    (define (my-read-char port)
      (let ((real-port (cdr port))
            (buffer    (car port)))
        (if buffer
            (begin (set-car! port #f)
                   buffer)
            (read-char real-port))))

    ; this wrapper function wraps a reader function
    ; that accepts a "fake" port above, and converts
    ; it to an R5RS-compatible function.  On Schemes
    ; which support source-information annotation,
    ; but use a different way of annotating
    ; source-information from Guile, this function
    ; should also probably perform that attachment
    ; on exit from the given inner function.
    (define (make-read f)
      (lambda args
        (let* ((real-port (if (null? args) (current-input-port) (car args)))
               (fake-port (cons #f real-port))
               (rv        (f fake-port)))
          ; pending character?
          (if (car fake-port)
              (error
                "internal error in sweet-impl, some unread input remaining")
              rv))))
    ; invoke the given "actual" reader, most likely
    ; the builtin one, but make sure to unwrap any
    ; fake ports.
    (define (invoke-read read port)
      (let ((real-port (cdr port))
            (buffer    (car port)))
        (if buffer
            (error
              "internal error in sweet-impl, some unread input before entering builtin read")
            (read real-port))))
    ; R5RS doesn't have any method of extracting
    ; or attaching source location information.
    (define (get-sourceinfo _) #f)
    (define (attach-sourceinfo _ x) x)

    ; Not strictly R5RS but we expect at least some Scheme's
    ; to allow this somehow.
    (define (replace-read-with f)
      (set! read f))))

; -----------------------------------------------------------------------------
; Module declaration and useful utilities
; -----------------------------------------------------------------------------
(module-contents
  ; exported functions
  (; tier read functions
   curly-infix-read modern-read sweet-read
   ; comparison functions
   compare-read-file ; compare-read-string
   ; replacing the reader
   replace-read restore-scheme-read)

  (define (ismember? item lyst)
    ; Returns true if item is member of lyst, else false.
    (pair? (member item lyst)))

  ; Define the tab character.
  ; Unfortunately, this seems to be the only portable way to define the
  ; tab character in Scheme, so we'll do it here and use it elsewhere.
  (define tab (integer->char 9)) ; assume ASCII
  (define carriage-return (integer->char 13)) ; assume ASCII

  (define (my-is-whitespace c)
    (ismember? c `(#\space #\newline ,tab ,carriage-return)))

  (define (skip-line port)
    ; Skip every character in the line - end on EOF or newline.
    (let ((c (my-peek-char port)))
      (cond
        ((not (or (eof-object? c)
                  (char=? c #\newline)
                  (char=? c carriage-return)))
          (my-read-char port)
          (skip-line port)))))

  (define (my-read-delimited-list my-read stop-char port)
    ; like read-delimited-list of Common Lisp, but calls the specified reader instead.
    ; read the "inside" of a list until its matching stop-char, returning list.
    ; This implements a useful extension: (. b) returns b.
    ; This is important as an escape for indented expressions, e.g., (. \\)
    (skip-whitespace port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port)))
      (cond
        ((eof-object? c) (read-error "EOF in middle of list") c)
        ((char=? c stop-char)
          (my-read-char port)
          (attach-sourceinfo pos '()))
        ((ismember? c '(#\) #\] #\}))  (read-error "Bad closing character") c)
        (#t
          (let ((datum (my-read port)))
            (cond
               ((eq? datum '.)
                 (let ((datum2 (my-read port)))
                   (skip-whitespace port)
                   (cond
                     ((not (eqv? (my-peek-char port) stop-char))
                      (read-error "Bad closing character after . datum"))
                     (#t
                       (my-read-char port)
                       datum2))))
               (#t
                 (attach-sourceinfo pos
                   (cons datum
                     (my-read-delimited-list my-read stop-char port))))))))))

; -----------------------------------------------------------------------------
; Read Preservation and Replacement
; -----------------------------------------------------------------------------

  (define default-scheme-read read)
  (define replace-read replace-read-with)
  (define (restore-scheme-read) (replace-read-with default-scheme-read))

; -----------------------------------------------------------------------------
; Scheme Reader re-implementation
; -----------------------------------------------------------------------------

; Unfortunately, since most Scheme readers will consume [, {, }, and ],
; we have to re-implement our own Scheme reader.  Ugh.
; If you fix your Scheme's "read" so that [, {, }, and ] are considered
; delimiters (and thus not consumed when reading symbols, numbers, etc.),
; you can just call default-scheme-read instead of using underlying-read below,
; with the limitation that vector constants #(...) will not support curly-infix
; or modern-function-expressions.
; We WILL call default-scheme-read on string reading (that DOES seem to work
; in common cases, and lets us use the implementation's string extensions).

  (define modern-delimiters `(#\space #\newline #\( #\) #\[ #\] #\{ #\}
            ,tab ,carriage-return))

  (define (skip-whitespace port)
    ; Consume whitespace.
    (let ((char (my-peek-char port)))
      (cond
        ((eqv? char #\;)
          (skip-line port)
          (skip-whitespace port))
        ((my-is-whitespace char)
          (my-read-char port)
          (skip-whitespace port)))))

  (define (read-until-delim port delims)
    ; Read characters until eof or "delims" is seen; do not consume them.
    ; Returns a list of chars.
    (let ((c (my-peek-char port)))
      (cond
         ((eof-object? c) '())
         ((ismember? (my-peek-char port) delims) '())
         (#t (cons (my-read-char port) (read-until-delim port delims))))))

  (define (read-error message)
    (display "Error: ")
    (display message)
    '())

  (define (read-number port starting-lyst)
    (string->number (list->string
      (append starting-lyst
        (read-until-delim port modern-delimiters)))))

  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (my-peek-char port)) (my-peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (my-read-char port))
              (rest (read-until-delim port modern-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t
              (let ((rest-string (list->string (cons c rest))))
                (cond
                  ((string-ci=? rest-string "space") #\space)
                  ((string-ci=? rest-string "newline") #\newline)
                  ((string-ci=? rest-string "ht") tab)  ; Scheme extension.
                  ((string-ci=? rest-string "tab") tab) ; Scheme extension.
                  (#t (read-error "Invalid character name"))))))))))


  (define (process-sharp top-read port)
    ; We've peeked a # character.  Returns what it represents.
    ; Note: Since we have to re-implement process-sharp anyway,
    ; the vector representation #(...) uses my-read-delimited-list, which in
    ; turn calls top-read.
    (my-read-char port) ; Remove #
    (cond
      ((eof-object? (my-peek-char port)) (my-peek-char port)) ; If eof, return eof.
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (my-read-char port)))
          (cond
            ((char=? c #\t)  #t)
            ((char=? c #\f)  #f)
            ((ismember? c '(#\i #\e #\b #\o #\d #\x))
              (read-number port (list #\# c)))
            ((char=? c #\( )  ; Vector.
              (list->vector (my-read-delimited-list top-read #\) port)))
            ((char=? c #\\) (process-char port))
            (#t (read-error "Invalid #-prefixed string")))))))

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (my-read-char port) ; Remove .
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) '.) ; period eof; return period.
        ((ismember? c digits)
          (read-number port (list #\.)))  ; period digit - it's a number.
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol (list->string (cons #\.
            (read-until-delim port modern-delimiters))))))))

  (define (underlying-read top-read port)
    ; Note: This reader is case-sensitive, which is consistent with R6RS
    ; and guile, but NOT with R5RS.  Most people won't notice, and I
    ; _like_ case-sensitivity.
    (skip-whitespace port)
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\")                           ; old readers tend to handle strings okay, call it.
          (invoke-read default-scheme-read port)) ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
        (#t
          ; attach the source information to
          ; the item read-in
          (attach-sourceinfo pos
            (cond
              ((ismember? c digits) ; Initial digit.
                (read-number port '()))
              ((char=? c #\#) (process-sharp top-read port))
              ((char=? c #\.) (process-period port))
              ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
                (my-read-char port)
                (if (ismember? (my-peek-char port) digits)
                  (read-number port (list c))
                  (string->symbol (list->string (cons c
                    (read-until-delim port modern-delimiters))))))

              ; We'll reimplement abbreviations, list open, and ;.
              ; These actually should be done by modern-read (and thus
              ; we won't see them), but redoing it here doesn't cost us anything,
              ; and it makes some kinds of testing simpler.  It also means that
              ; this function is a fully-usable Scheme reader, and thus perhaps
              ; useful for other purposes.
              ((char=? c #\')
                (my-read-char port)
                (list (attach-sourceinfo pos 'quote)
                  (top-read port)))
              ((char=? c #\`)
                (my-read-char port)
                (list (attach-sourceinfo pos 'quasiquote)
                  (top-read port)))
              ((char=? c #\`)
                (my-read-char port)
                  (cond
                    ((char=? #\@ (my-peek-char port))
                      (my-read-char port)
                      (list (attach-sourceinfo pos 'unquote-splicing)
                       (top-read port)))
                   (#t
                    (list (attach-sourceinfo pos 'unquote)
                      (top-read port)))))
              ; The "(" calls modern-read, but since this one shouldn't normally
              ; be used anyway (modern-read will get first crack at it), it
              ; doesn't matter:
              ((char=? c #\( )
                  (my-read-char port)
                  (my-read-delimited-list top-read #\) port))
              ((char=? c #\| )   ; Scheme extension, |...| symbol (like Common Lisp)
                (my-read-char port) ; Skip |
                (let ((newsymbol
                  (string->symbol (list->string
                    (read-until-delim port '(#\|))))))
                  (my-read-char port)
                  newsymbol))
              (#t ; Nothing else.  Must be a symbol start.
                (string->symbol (list->string
                  (read-until-delim port modern-delimiters))))))))))

; -----------------------------------------------------------------------------
; Curly Infix
; -----------------------------------------------------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating) first
  ; ones are "op".  Used to determine if a longer lyst is infix.
  ; Otherwise it returns false.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f) ; Not a list.
      ((not (eq? op (car lyst))) #f) ; fail - operators not all equal?.
      ((null? (cdr lyst)) #f) ; fail - odd # of parameters in lyst.
      (#t (even-and-op-prefix op (cddr lyst))))) ; recurse.

  ; Return True if the lyst is in simple infix format (and should be converted
  ; at read time).  Else returns NIL.
  (define (simple-infix-listp lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (symbol? (cadr lyst))  ; 2nd parameter must be a symbol.
      (even-and-op-prefix (cadr lyst) (cdr lyst)))) ; even parameters equal??

  ; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  (define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
      lyst
      (cons (car lyst) (alternating-parameters (cddr lyst)))))

  ; Transform a simple infix list - move the 2nd parameter into first position,
  ; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
  (define (transform-simple-infix lyst)
     (cons (cadr lyst) (alternating-parameters lyst)))

  (define (process-curly lyst)
    (if (simple-infix-listp lyst)
       (transform-simple-infix lyst) ; Simple infix expression.
       (cons 'nfx lyst))) ; Non-simple; prepend "nfx" to the list.

  (define (read-at-curly top-read port)
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eqv? c #\{)
          (my-read-char port)
          ; read in as infix
          (attach-sourceinfo pos
            (process-curly
              (my-read-delimited-list top-read #\} port))))
        (#t
          (underlying-read top-read port)))))

  (define (curly-infix-read-func port)
    (read-at-curly curly-infix-read-func port))

; -----------------------------------------------------------------------------
; Modern Expressions
; -----------------------------------------------------------------------------

  (define (modern-process-tail port prefix)
      ; See if we've just finished reading a prefix, and if so, process.
      ; This recurses, to handle formats like f(x)(y).
      ; This implements prefixed (), [], and {}
      (let* ((pos (get-sourceinfo port))
             (c   (my-peek-char port)))
        (cond
          ((eof-object? c) c)
          ((char=? c #\( ) ; Implement f(x).
            (my-read-char port)
            (modern-process-tail port
              (attach-sourceinfo pos
                (cons prefix (my-read-delimited-list modern-read-func #\) port)))))
          ((char=? c #\[ )  ; Implement f[x]
            (my-read-char port)
            (modern-process-tail port
                (attach-sourceinfo pos
                  (cons (attach-sourceinfo pos 'bracketaccess)
                    (cons prefix
                      (my-read-delimited-list modern-read-func #\] port))))))
          ((char=? c #\{ )  ; Implement f{x}
            (modern-process-tail port
              (attach-sourceinfo pos
                (list prefix
                  (read-at-curly modern-read-func port)))))
          (#t prefix))))

  (define (modern-read-func port)
    ; Read using "modern Lisp notation".
    ; This implements unprefixed (), [], and {}
    (skip-whitespace port)
    (modern-process-tail port
      (let* ((pos (get-sourceinfo port))
             (c   (my-peek-char port)))
        ; (write c)
        (cond
          ; We need to directly implement abbreviations ', etc., so that
          ; we retain control over the reading process.
          ((eof-object? c) c)
          (#t
            (attach-sourceinfo pos
              (cond
                ((char=? c #\')
                  (my-read-char port)
                  (list (attach-sourceinfo pos 'quote)
                    (modern-read-func port)))
                ((char=? c #\`)
                  (my-read-char port)
                  (list (attach-sourceinfo pos 'quasiquote)
                    (modern-read-func port)))
                ((char=? c #\,)
                  (my-read-char port)
                    (cond
                      ((char=? #\@ (my-peek-char port))
                        (my-read-char port)
                        (list (attach-sourceinfo pos 'unquote-splicing)
                         (modern-read-func port)))
                     (#t
                      (list (attach-sourceinfo pos 'unquote)
                        (modern-read-func port)))))
                ((char=? c #\( )
                   (my-read-char port)
                   (my-read-delimited-list modern-read-func #\) port))
                ((char=? c #\[ )
                    (my-read-char port)
                    (my-read-delimited-list modern-read-func #\] port))
                ((char=? c #\{ )
                  (my-read-char port)
                  (process-curly
                    (my-read-delimited-list modern-read-func #\} port)))
                (#t (let ((result (underlying-read modern-read-func port)))
                        result)))))))))

; -----------------------------------------------------------------------------
; Sweet Expressions
; -----------------------------------------------------------------------------

  (define split (string->symbol "\\\\"))
  (define split-char #\\ ) ; First (possibly only) character of split symbol.
  ; this is a special unique object that is used to
  ; represent the existence of the split symbol
  ; so that readblock-clean handles it properly.
  (define split-tag (cons '() '()))
  ; Return #t if char is space or tab.
  (define (char-horiz-whitespace? char)
    (or (eqv? char #\space)
        (eqv? char tab)))

  (define (consume-to-eol port)
    ; Consumes chars to end of line, WITHOUT consume the ending newline/EOF
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\newline) c)
        ((char=? c carriage-return) c)
        (#t (my-read-char port) (consume-to-eol port)))))

  (define (readquote level port qt)
    (let ((char (my-peek-char port)))
      (if (char-whitespace? char)
          (list qt)
          (list qt (modern-read-func port)))))

  (define (readitem level port)
    (let ((pos  (get-sourceinfo port))
          (char (my-peek-char port)))
      (cond
       ((eqv? char #\`)
        (my-read-char port)
        (attach-sourceinfo pos (readquote level port 'quasiquote)))
       ((eqv? char #\')
        (my-read-char port)
        (attach-sourceinfo pos (readquote level port 'quote)))
       ((eqv? char #\,)
        (my-read-char port)
        (cond
          ((eqv? (my-peek-char port) #\@)
            (my-read-char port)
            (attach-sourceinfo pos (readquote level port 'unquote-splicing)))
          (#t
            (attach-sourceinfo pos (readquote level port 'unquote)))))
       (#t
          (modern-read-func port)))))

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
            (len2 (string-length indentation2)))
      (and (> len1 len2)
             (string=? indentation2 (substring indentation1 0 len2)))))

  (define (accumulate-hspace port)
    (if (char-horiz-whitespace? (my-peek-char port))
        (cons (my-read-char port) (accumulate-hspace port))
        (if (not (eqv? (my-peek-char port) #\.))
            '()
            (let ((c (my-read-char port)))
              (if (char-horiz-whitespace? (my-peek-char port))
                (cons #\. (accumulate-hspace port)) ; period-as-indent
                (begin (my-unread-char c port) '()))))))

  (define (indentationlevel port)
    (let ((indent (accumulate-hspace port)))
      (cond
        ((eqv? (my-peek-char port) #\;)
          (consume-to-eol port) ; ALWAYS ignore comment-only lines.
          (if (eqv? (my-peek-char port) carriage-return) (my-read-char port))
          (if (eqv? (my-peek-char port) #\newline) (my-read-char port))
          (indentationlevel port))
        ; If ONLY whitespace on line, treat as "", because there's no way
        ; to (visually) tell the difference (preventing hard-to-find errors):
        ((eof-object? (my-peek-char port)) "")
        ((eqv? (my-peek-char port) #\newline) "")
        ((eqv? (my-peek-char port) carriage-return)
           (my-read-char port) "")
        (#t (list->string indent)))))

  ;; Reads all subblocks of a block
  ;; this essentially implements the "body" production
  ;; - return value:
  ;;   cons
  ;;     next-level ;
  ;;     (xs ...) ; the body
  (define (readblocks level port)
    (let* ((pos        (get-sourceinfo port))
           (read       (readblock-clean level port))
           (next-level (car read))
           (block      (cdr read)))
      (if (string=? next-level level)
            (let* ((reads (readblocks level port))
                   (next-next-level (car reads))
                   (next-blocks (cdr reads)))
              (if (eq? block '.)
                  (if (pair? next-blocks)
                      (cons next-next-level (car next-blocks))
                      (cons next-next-level next-blocks))
                  (cons next-next-level (attach-sourceinfo pos (cons block next-blocks)))))
            (cons next-level (attach-sourceinfo pos (list block))))))

  ;; Read one block of input
  ;; this essentially implements the "head" production
  ;; - return value:
  ;;   cons
  ;;     next-level ; the indentation of the line that ends this block
  ;;     expr ;       the read-in expression
  (define (readblock level port)
    (readblock-internal level port #t))
  (define (readblock-internal level port first-item?)
    (let* ((pos  (get-sourceinfo port))
           (char (my-peek-char port)))
      (cond
       ((eof-object? char)
          (cons -1 char))
       ((eqv? char #\;)
          (consume-to-eol port)
          (readblock level port))
       ((or (eqv? char #\newline)
            (eqv? char carriage-return))
          (my-read-char port)
          (if (and (eqv? char carriage-return)
                  (eqv? (my-peek-char port) #\newline))
            (my-read-char port))
          (let ((next-level (indentationlevel port)))
            (if (indentation>? next-level level)
                (readblocks next-level port)
                (cons next-level (attach-sourceinfo pos '())))))
       ((char-horiz-whitespace? char)
          (my-read-char port)
          (readblock-internal level port first-item?))
       (#t
          (let ((first (readitem level port)))
            (cond
              ((and first-item?
                    (or (equal? first '(quote))
                        (equal? first '(quasiquote))
                        (equal? first '(unquote))
                        (equal? first '(unquote-splicing))))
                (consume-horizontal-whitespace port)
                (let* ((sub-read (readblock-clean level port))
                       (outlevel (car sub-read))
                       (sub-expr (cdr sub-read)))
                  (cons outlevel (attach-sourceinfo pos `(,@first ,sub-expr)))))
              ((and (eq? char split-char) (eq? first split))
                ; consume horizontal, non indent whitespace
                (consume-horizontal-whitespace port)
                (if first-item?
                    ;; NB: need a couple of hacks to fix
                    ;; behavior when SPLIT-by-itself
                    (if (or (eqv? (my-peek-char port) #\newline)
                            (eqv? (my-peek-char port) carriage-return))
                        ; check SPLIT-by-itself
                        ; SPLIT-by-itself: some hacks needed
                        (let* ((sub-read (readblock level port))
                               (outlevel (car sub-read))
                               (sub-expr (cdr sub-read)))
                          (if (and (null? sub-expr) (string=? outlevel level)) ; check SPLIT followed by same indent line
                              ; blank SPLIT:
                              ; \
                              ; \
                              ; x
                              ; ===> x, not () () x
                              (readblock level port)
                              ; non-blank SPLIT: insert our
                              ; split-tag.  Without SPLIT-tag
                              ; readblock-clean will mishandle:
                              ; \
                              ;   x y
                              ; ==> ((x y)), which is a single
                              ; item list.  Single-item lists
                              ; are extracted, resulting in
                              ; (x y)
                              (cons outlevel (cons split-tag (attach-sourceinfo pos sub-expr)))))
                        ; not SPLIT-by-itself: just ignore it
                        (readblock-internal level port first-item?))
                    ; SPLIT-inline: end this block
                    (cons level (attach-sourceinfo pos '()))))
              (#t
                (let* ((rest (readblock-internal level port #f))
                       (level (car rest))
                       (block (cdr rest)))
                  ;; this check converts:
                  ;;  . foo
                  ;; ->
                  ;;  (. foo)
                  ;; ->
                  ;;  foo
                  ;; HOWEVER, it might not be compatible
                  ;; 100% with the "." as indentation
                  ;; whitespace thing.
                  (if (eq? first '.)
                      (if (pair? block)
                          (cons level (car block))
                          rest)
                      (cons level (attach-sourceinfo pos (cons first block))))))))))))

  ;; Consumes as much horizontal, non-indent whitespace as
  ;; possible.  Treat comments as horizontal whitespace too.
  (define (consume-horizontal-whitespace port)
    (let ((char (my-peek-char port)))
      (cond
        ((char-horiz-whitespace? char)
           (my-read-char port)
           (consume-horizontal-whitespace port))
        ((eqv? char #\;)
           (consume-to-eol port)))))

  ;; reads a block and handles (quote), (unquote),
  ;; (unquote-splicing) and (quasiquote).
  (define (readblock-clean level port)
    (let* ((read (readblock level port))
           (next-level (car read))
           (block (cdr read)))
      (cond
        ; remove split-tag
        ((and (pair? block) (eq? (car block) split-tag))
          (cons next-level (cdr block)))
        ; non-list and multi-item blocks.
        ((or (not (list? block)) (> (length block) 1))
          (cons next-level block))
        ; unwrap single-item blocks
        ((= (length block) 1)
          (if (eq? (car block) split-tag)
              ; "magically" remove split-tag
              (cons next-level '())
              (cons next-level (car block))))
        (#t
          (cons next-level '.)))))


  (define (sugar-start-expr port)
    ; Read single complete I-expression.
    (let* ((indentation (list->string (accumulate-hspace port)))
           (pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c) ; EOF - return it, we're done.
        ((eqv? c #\; )    ; comment - consume and see what's after it.
          (let ((d (consume-to-eol port)))
            (cond
              ((eof-object? d) d) ; If EOF after comment, return it.
              (#t
                (my-read-char port) ; Newline after comment.  Consume NL
                (sugar-start-expr port))))) ; and try again
        ((eqv? c #\newline)
          (my-read-char port) ; Newline (with no preceding comment).
          (sugar-start-expr port)) ; Consume and again
        ((eqv? c carriage-return)
          (my-read-char port) ; Consume carriage return.
          (if (eqv? (my-peek-char port) #\newline)
              (my-read-char port)) ; Consume newline if it follows CR.
          (sugar-start-expr port))
        ((> (string-length indentation) 0) ; initial indentation disables
          (modern-read-func port))
        (#t
          (let* ((read (readblock-clean "" port))
                 (level (car read))
                 (block (cdr read)))
            (cond
             ((eq? block '.)
                (attach-sourceinfo pos '()))
             (#t
                (attach-sourceinfo pos block))))))))

; -----------------------------------------------------------------------------
; Comparison Functions
; -----------------------------------------------------------------------------

  (define compare-read-file '()) ; TODO

; -----------------------------------------------------------------------------
; Exported Interface
; -----------------------------------------------------------------------------

  (define curly-infix-read (make-read curly-infix-read-func))
  (define modern-read (make-read modern-read-func))
  (define sweet-read (make-read sugar-start-expr))

  )

; vim: set expandtab shiftwidth=2 :
