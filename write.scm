#!guile -s
!#
;;; write.scm
;;; Write expressions as c-expressions and n-expressions.

;;; Copyright (C) 2006-2013 David A. Wheeler.
;;; Portions of the write routines by Alex Shinn, public domain.
;;;
;;; This software is released as open source software under the "MIT" license:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(define debugger-output #f)
(define (debug-show marker data)
  (cond (debugger-output
         (display "DEBUG: ")
         (display marker)
         (display " = ")
         (write data)
         (display "\n")))
  data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST SUITE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define basic-tests
  '(
    (+ 4 5)
    (quote x)
    (a b c d e f g h i j k l m n o p q r s t u v w x y z)
    (a b c d e f g h i j k l m n o p q r s t u v w x y z . 2)
    (a 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
    (a 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
    (+ a b)
    (+ a b c)
    (+ a b c . improper)
    (+ 1 2 3 4 5)
    (+ 1 2 3 4 5 6)
    (+ 1 2 3 4 5 6 7)
    (sin (- theta))
    (fact (- n 1))
    (calculate (pi))
    (between current min max)
    (my-write . rest)
    (sin x)
    (- x)
    (-)
    (function +)
    (map + '(2 4 6))
    (current-time)
    (1 2 3)
    (4 5 . 6)
    5
    boring-symbol
    (+ (sqrt x) (sqrt y))
    `(1 2 ,@(+ a b))
    (syntax (a b c))
    #(v1 v2 (+ 2 3) (sin x))
    (define (is-infix-operator? x)
      (cond ((not (symbol? x)) #f)
            ((memq x special-infix-operators) #t)
            (#t
             (contains-only-punctuation?
               (string->list (symbol->string x))))))
    fin))


(display "curly-write-simple\n")
(for-each (lambda (v) (curly-write-simple v) (newline)) basic-tests)
(newline)
(newline)

(display "neoteric-write-simple\n")
(for-each (lambda (v) (neoteric-write-simple v) (newline)) basic-tests)
(newline)
(newline)


(define part '(a b))

(define demo1 (list 'begin part part 'end))

(define demo2 '(first . last))
(set-cdr! demo2 demo2)

(define nasty-quote '(quote x))
(set-cdr! nasty-quote nasty-quote)

(define demo3 '(dosomething (a1 a2) (b1 b2) (c1 c2)))
(set-car! (cdr demo3) (cadddr demo3))

(set! basic-tests
  (append basic-tests (list '(a b) demo1 demo2 demo3 nasty-quote)))



(display "curly-write-shared\n")
(for-each (lambda (v) (curly-write-shared v) (newline)) basic-tests)
(newline)

(display "curly-write-cyclic\n")
(for-each (lambda (v) (curly-write-cyclic v) (newline)) basic-tests)
(newline)

(display "neoteric-write-shared\n")
(for-each (lambda (v) (neoteric-write-shared v) (newline)) basic-tests)
(newline)

(display "neoteric-write-cyclic\n")
(for-each (lambda (v) (neoteric-write-cyclic v) (newline)) basic-tests)
(newline)

