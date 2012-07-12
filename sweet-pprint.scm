; sweet-pprint.scm
; Sweet-expression pretty-printing

; Copyright (C) 2006-2012 by David A. Wheeler
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

; TODO: Finish this code.
; TODO: Add support for "port"
; TODO: Add support for vector
; TODO: (a . b) not working for some reason - need to debug.

(define LP #\( )
(define RP #\) )

(define (infix-chars? lyst)
  ; Returns true if the list only contains characters used in infix ops
  (cond
    ((null? lyst) #t)
    ((pair? (member (car lyst) '(#\+ #\- #\* #\/ #\= #\< #\> #\& #\|)))
      (infix-chars? (cdr lyst)))
    (#t #f)))

(define (modern-is-infix-operator? s)
  (cond
    ((not (symbol? s))  #f)
    ((eq? s 'and)  #t)
    ((eq? s 'or)  #t)
    ((eq? s 'xor)  #t)
    ((eq? s ':)  #t)
    (#t (infix-chars? (string->list (symbol->string s))))))

(define (modern-is-infixable-list lyst)
  ; Returns true if lyst can be represented as infix list; else false
  (cond
    ((not (pair? lyst)) #f) ; False if not a pair at all.
    ((not (symbol? (car lyst))) #f) ; First item not a symbol.
    ((not (pair? (cdr lyst))) #f)   ; Must have 2+ function parameters
    ((not (pair? (cddr lyst))) #f)
    ((not (list? lyst)) #f)         ; Must be a proper list.
    ((modern-is-infix-operator? (car lyst)) #t)  ; True if infix operator.
    (#t #f))) ; By default, false.

(define (list-item-chars x)
  ; call modern-charlist on each item in x
  (cond
    ((null? x) '())
    ((pair? x)
      (append
        (modern-charlist (car x))
        (list-item-chars (cdr x))))
    (#t (modern-charlist x))))

(define (modern-contents separators obj)
  ; Returns contents representing "list" obj, without its surrounding markers.
  ; if it isn't a pair, it must be from a recursion with an improper list.
  ; Separators is a list of displayable separators.
  (cond
    ((null? obj) '() ) ; Do nothing.
    ((pair? obj)
      (append (modern-charlist (car obj))
        (if (eqv? (cdr obj) '())
          '()
          (append
            separators
            (modern-contents separators (cdr obj))))))
    (#t (append '(#\. #\space) (modern-charlist obj)))))

(define function-call-translate #t)

(define (modern-charlist obj)
  ; create a list of characters representing obj.
  (cond
    ((symbol? obj) (string->list (symbol->string obj)))
    ((null? obj)   (string->list "()"))
    ((number? obj) (string->list (number->string obj)))
    ((boolean? obj) (list #\# (if obj #\t #\f)))
    ((char? obj)
      (append '(#\#) '(#\\)
        (cond
          ((eqv? obj #\newline) (string->list "newline"))
          ((eqv? obj #\space)   (string->list "space"))
          (#t (list obj)))))
    ; TODO: string
    ; OLD: ((not (pair? obj)) (write obj))
    ((and (pair? obj) (eq? (car obj) 'quote) (eq? (cddr obj) '()))
      (cons #\' (modern-charlist (cadr obj))))
    ; TODO: quasiquote, comma, comma-splicing
    ((modern-is-infixable-list obj) ; Infix?
      (append '(#\{)
       (modern-contents
          (append '(#\ ) (string->list (symbol->string (car obj))) '(#\ ))
                  (cdr obj))
      '(#\})))
    ((and (symbol? (car obj))
          (pair? (cdr obj))
          (eq? (cddr obj) '())  ; Exactly one parameter.
          (modern-is-infixable-list (cadr obj)))
      ; Function-call, one-parameter-infix: (f (- n 1)) => f{n - 1}.
      (append (modern-charlist (car obj))
        ; Don't need to display "{" - handled by infix processing.
        (modern-contents
          (append '(#\ ) (string->list (symbol->string (car obj))) '(#\ ))
                  (cdr obj))))
    ((and function-call-translate (symbol? (car obj))
          (or (pair? (cdr obj)) (eq? (cdr obj) '())))
      ; Function call.
      ; Require that cdr is pair or empty list; we don't want
      ; (a . b) to translate to a( . b)
      (append
        (modern-charlist (car obj))
        (list LP)
        (modern-contents '(#\space) (cdr obj))
        (list RP)))
    (#t                  ; Generic list format.
      (append '(#\[) (modern-contents '(#\space) obj) '(#\])))))


(define (modern-write obj)
  (display (list->string (modern-charlist obj))))

(define (modern-write-testreport obj)
  (display "Writing ")
  (write obj)
  (newline)
  (display "  ")
  ; (write (modern-charlist obj)) (newline)
  (display (list->string (modern-charlist obj)))
  (newline)
  (newline))

(modern-write-testreport 'a)
(modern-write-testreport ''a)
(modern-write-testreport '())
(modern-write-testreport '(a))
(modern-write-testreport '(a b))
(modern-write-testreport '(a b c))
(modern-write-testreport '(a . b))
(modern-write-testreport '(a b . c))
(modern-write-testreport '(()))
(modern-write-testreport '(() a))
(modern-write-testreport '((x) a))
(modern-write-testreport '((x y) a))
(modern-write-testreport '((x y) (a b)))
(modern-write-testreport '(+))
(modern-write-testreport '(+ 3))
(modern-write-testreport '(+ 3 4))
(modern-write-testreport '(+ 3 4 5))
(modern-write-testreport '(f +))
(modern-write-testreport '(f (a b)))
(modern-write-testreport '(f (- n 1)))
(modern-write-testreport '(f (+ a b c)))
(modern-write-testreport '(+ 3 (+ 4 5)))


