; Sweet-expression 0.2 pretty-printing implementation in Scheme,
; 2007-12-28, version 0.02.
; Copyright (C) 2006-2007 by David A. Wheeler
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

; TODO: Finish this code.
; TODO: Add support for "port"

(define (sweet-is-infix-operator? s)
  (cond
    ((not (symbol? s))  #f)
    ((eq? s 'and)  #t)
    ((eq? s 'or)  #t)
    ((eq? s 'xor)  #t)
    ((eq? s ':)  #t)
    (#t (infix-chars? (string->list (symbol->string s))))))

(define (infix-chars? lyst)
  (cond
    ((null? lyst) #t)
    ((pair? (member (car lyst) '(#\+ #\- #\* #\/ #\= #\< #\> #\& #\|)))
      (infix-chars? (cdr lyst)))
    (#t #f)))

(define (sweet-is-infixable-list lyst)
  ; Returns true if lyst can be represented as infix list; else false
  (cond
    ((not (pair? lyst)) #f) ; False if not a pair at all.
    ((not (symbol? (car lyst))) #f) ; First item not a symbol.
    ((not (pair? (cdr lyst))) #f)   ; Must have 2+ function parameters
    ((not (pair? (cddr lyst))) #f)
    ((not (list? lyst)) #f)         ; Must be a proper list.
    ((sweet-is-infix-operator? (car lyst)) #t)  ; True if infix operator.
    (#t #f))) ; By default, false.

(define (display-list x)
  ; call "display" on each item in x, if it's a list; else display x.
  (cond
    ((pair? x)
      (display (car x))
      (display-list (cdr x)))
    ((eq? x '()))               ; Do nothing for '().
    (#t (display x))))

(define (sweet-write-contents separators obj)
  ; Writes contents of "list" obj, without its surrounding markers.
  ; if it isn't a pair, it must be from a recursion with an improper list.
  ; Separators is a list of displayable separators.
  (cond
    ((eq? obj '()) ) ; Do nothing.
    ((pair? obj)
      (sweet-write (car obj))
      (if (not (eq? (cdr obj) '()))
        (begin
          (display-list separators)
          (sweet-write-contents separators (cdr obj)))))
    (#t (display ". ") (sweet-write obj))
))

(define function-call-translate #t)

(define (sweet-write obj)
  (cond
    ((not (pair? obj)) (write obj))
    ((and (eq? (car obj) 'quote)
          (eq? (cddr obj) '()))
      (display "'") (sweet-write (cadr obj)))
    ; TODO: quasiquote, comma, comma-splicing
    ((sweet-is-infixable-list obj) ; Infix.
      (display "{")
      (sweet-write-contents (list " " (car obj) " ") (cdr obj))
      (display "}"))
    ((and (symbol? (car obj))
          (pair? (cdr obj))
          (eq? (cddr obj) '())  ; Exactly one parameter.
          (sweet-is-infixable-list (cadr obj)))
      ; Function-call, one-parameter-infix: (f (- n 1)) => f{n - 1}.
      (display (car obj))
      ; Don't need to display "{" - handled by infix processing.
      (sweet-write-contents
        (string-append " " (symbol->string (caadr obj)) " ")
        (cdr obj)))
    ((and function-call-translate (symbol? (car obj))
          (or (pair? (cdr obj)) (eq? (cdr obj) '())))
      ; Function call.
      ; Require that cdr is pair or empty list; we don't want
      ; (a . b) to translate to a( . b)
      (display (car obj))
      (display "(")
      (sweet-write-contents " " (cdr obj))
      (display ")"))
    (#t                  ; Generic list format.
      (display "[")
      (sweet-write-contents " " obj)
      (display "]"))
  )
)

(define (sweet-write-testreport obj)
  (display "Writing ")
  (write obj)
  (newline)
  (display "  ")
  (sweet-write obj)
  (newline)
  (newline))

(sweet-write-testreport 'a)
(sweet-write-testreport ''a)
(sweet-write-testreport '())
(sweet-write-testreport '(a))
(sweet-write-testreport '(a b))
(sweet-write-testreport '(a b c))
(sweet-write-testreport '(a . b))
(sweet-write-testreport '(a b . c))
(sweet-write-testreport '(()))
(sweet-write-testreport '(() a))
(sweet-write-testreport '((x) a))
(sweet-write-testreport '((x y) a))
(sweet-write-testreport '((x y) (a b)))
(sweet-write-testreport '(+))
(sweet-write-testreport '(+ 3))
(sweet-write-testreport '(+ 3 4))
(sweet-write-testreport '(+ 3 4 5))
(sweet-write-testreport '(f +))
(sweet-write-testreport '(f (a b)))
(sweet-write-testreport '(f (- n 1)))
(sweet-write-testreport '(f (+ a b c)))
(sweet-write-testreport '(+ 3 (+ 4 5)))


