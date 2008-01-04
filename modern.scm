; modern.scm (Scheme), 2008-01-03
;
; NOTE: NOT READY FOR PRODUCTION USE.
;
; Implements "modern Lisp notation".  E.G., f(x) => (f x),
; {3 + 4 + 5} => (+ 3 4 5), f{x + 3} => (f (+ x 3),
; x[z] => (bracketaccess x z).
;
; Call "modern-read" to read a "modern Lisp expression".
;
; Copyright (C) 2008 by David A. Wheeler.
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


; Configuration:
(define modern-backwards-compatible #t) ; If true, "(" triggers old reader.

; Preserve original read.
(define old-read read)

; Utility functions to implement the simple infix system:

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


(define (my-read-delimited-list stop-char port)
  ; like read-delimited-list, but call modern-read instead.
  ; Also, has ALL the read parameters.
  ; TODO: Handle (x . b)
  ; TODO: Handle Error on wrong stop-char.
  ; TODO: Handle EOF in middle.
  (let
    ((c (peek-char port)))
    ; (print "DEBUG my-read-delimited-list:") (write c)
    (cond
      ; TODO : if EOF
      ((char=? c stop-char)
        (read-char port)
        '())
      ((my-is-whitespace c)
        (read-char port)
        (my-read-delimited-list stop-char port))
      (#t
        (cons
         (modern-read2 port)
         (my-read-delimited-list stop-char port))))))

(define (my-is-whitespace c)
  (pair? (member c '(#\space #\newline))))
; TODO:
;   '(#\Space #\Tab #\newline #\Return (code-char 9)    ; Tab
;     (code-char 10) (code-char 11)     ; LF, VT
;     (code-char 12) (code-char 13)))))  ; FF, CR
  

(define (skip-whitespace port)
  ; Consume whitespace.
  (cond
    ((my-is-whitespace (peek-char port))
      (read-char port)
      (skip-whitespace port))))

;!(define (modern-process-tail port prefix)
;!  ; See if we've just finished reading a prefix, and if so, process.
;!  ; This recurses, to handle formats like f(x)(y).
;!  ; This implements prefixed (), [], and {}
;!  ; (display "Got to tail, prefix is:")
;!  ; (write prefix)
;!  ; (display "peek char is:")
;!  ; (write (peek-char NIL??? input-stream eof-error-p eof-value recursive-p))
;!  ; (display "Starting...")
;!  (if (not (or (symbol? prefix) (pair? prefix)))
;!    prefix  ; Prefixes MUST be symbol or cons; return original value.
;!    (let ((c (peek-char port)))
;!      (cond
;!        ; Portability note: In some Lisps, must check for EOF.
;!        ((char=? c #\( ) ; ).  Implement f(x).
;!          (read-char port)
;!          (modern-process-tail port ;(
;!            (cons prefix (my-read-delimited-list #\) port)))
;!        ((char=? c #\[ )
;!          (read-char port)
;!          (modern-process-tail port
;!            (cons 'bracketaccess (cons prefix
;!              (my-read-delimited-list #\] port)))))
;!        ((char=? c #\{ )
;!          (read-char port)
;!          (modern-process-tail port
;!            (list prefix
;!              (process-curly
;!                (my-read-delimited-list #\} port)))))
;!        (#t prefix))))))

; TODO - stub
(define (modern-process-tail port prefix)
  prefix
)

(define (modern-read2 port)
  ; Read using "modern Lisp notation".
  ; This implements unprefixed (), [], and {}
  (skip-whitespace port)
  ; TODO: Add (tail ...)
  (modern-process-tail port
    (let ((c (peek-char port)))
      (display "modern-read2 peeked at: ")
      (write c)
      (cond
        ; Portability note: In other Lisps, must check for EOF.
        ; Portability note: May need to change the below in other
        ; Common Lisp implementations - this works for clisp.
        ; We need to directly implement abbreviations ', etc., so that
        ; we retain control over the reading process.
        ((char=? c #\')
          (read-char port)
          (list 'quote
            (modern-read2 port)))
        ; PROBLEM.  DOES NOT WORK:
        ; ((char=? c #\`)
        ;   (read-char input-stream eof-error-p eof-value recursive-p)
        ;   (list 'system::backquote
        ;     (modern-read2 input-stream eof-error-p eof-value recursive-p)))
        ; ((char=? c #\,)
        ;   (read-char input-stream eof-error-p eof-value recursive-p)
        ;   (cond
        ;     ((char=? \#@
        ;           (peek-char NIL??? input-stream eof-error-p
        ;                          eof-value recursive-p))
        ;       (read-char input-stream eof-error-p eof-value recursive-p)
        ;       (list 'system::splice
        ;        (modern-read2 input-stream eof-error-p
        ;                      eof-value recursive-p)))
        ;    (#t 
        ;     (list 'system::unquote
        ;       (modern-read2 input-stream eof-error-p
        ;                     eof-value recursive-p)))))
        ((char=? c #\( ) ; )
          (if modern-backwards-compatible
            (old-read port)
            (begin
              (read-char port) ; (
              (my-read-delimited-list #\) port))))
        ((char=? c #\[ )
            (read-char port)
            (my-read-delimited-list #\] port))
        ((char=? c #\{ )
          (read-char port)
          (process-curly
            (my-read-delimited-list #\} port)))
        (#t (let ((result (old-read port)))
                (display "DEBUG result = ")
                (write result)
                (display "\nDEBUG peek after= ")
                (write (peek-char port))
                ; (print "DEBUG after-read:")
                ; (write (peek-char port))
                result))))))


(define (modern-read . port)
  (if (null? port)
    (modern-read2 (current-input-port))
    (modern-read2 (car port))))

