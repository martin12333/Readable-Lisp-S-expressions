#!guile -s
!#
;;; write.scm
;;; Write expressions as c-expressions and n-expressions.


;;; Early draft, converted from sweeten.scm to s-expressions and
;;; to do straight writes instead of character lists.
;;; This may be moved into kernel.scm.

;;; Copyright (C) 2006-2013 David A. Wheeler.
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


(define boring-length 16)

(define special-infix-operators
  '(and or xor))

(define punct-chars
  (list #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\-
        #\.  #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
        #\_ #\` #\{ #\| #\} #\~))

; Returns #f if x has >1 element. Improper lists are #t.
; (define (multi-element-list? x)
;   (and (pair? x) (pair? (cdr x))))

; Returns #t if x is a list with exactly 1 element.  Improper lists are #f.
(define (list1? x)
  (and (pair? x) (null? (cdr x))))

; Returns #t if x is a list with exactly 2 elements.  Improper lists are #f.
(define (list2? x)
  (and (pair? x) (pair? (cdr x)) (null? (cddr x))))

; Returns #t if x is a list or improper list of 3+ elements.
; (define (list-3-plus? x)
;   (and (pair? x) (pair? (cdr x)) (pair? (cddr x))))

; Does x contain a list of ONLY punctuation characters?
; An empty list is considered true.
(define (contains-only-punctuation? x)
  (cond ((null? x) #t)
        ((not (pair? x)) #f)
        ((memq (car x) punct-chars)
         (contains-only-punctuation? (cdr x)))
        (#t #f)))

; Returns #t if x is a symbol that would typically be used in infix position.
(define (is-infix-operator? x)
  (cond ((not (symbol? x)) #f)
        ((memq x special-infix-operators) #t)
        (#t
         (contains-only-punctuation?
           (string->list (symbol->string x))))))

; A possibly-improper list is long and boring if its length is at least
; the boring-length, and it's boring (it contains no pairs up to that length).
; A long-and-boring list is almost certainly NOT a function call or a
; body of some executable sequence - it's almost certainly a long
; boring list of data instead. If it is, we want to display it differently.
; This doesn't get stuck on circular lists; it always terminates after
; num-to-go iterations.
(define (long-and-boring-tail? x num-to-go)
  (cond
    ((pair? (car x)) #f)
    ((not (pair? (cdr x))) #f)
    ((<= num-to-go 1) #t)
    (#t (long-and-boring-tail? (cdr x) (- num-to-go 1)))))
(define (long-and-boring? x)
  (cond ((not (pair? x)) #f)
        (#t (long-and-boring-tail? x boring-length))))

; Return #t if x should be represented using curly-infix notation {...}.
(define (represent-as-infix? x)
  (and (pair? x)
       (pair? (cdr x))                ; At least 2 elements.
       (is-infix-operator? (car x))
       (list? x)
       (<= (length x) 6)))

(define (represent-as-inline-infix? x)
  (and (represent-as-infix? x) (>= (length x) 3)))

; Return #t if x should be represented as a brace suffix
(define (represent-as-brace-suffix? x)
  (represent-as-infix? x))

; Define an association list mapping the Scheme procedure names which have
; abbreviations ==> the list of characters in their abbreviation
(define abbreviations
  '((quote (#\'))
    (quasiquote (#\`))
    (unquote (#\,))
    (unquote-splicing (#\, #\@))
    ; Scheme syntax-rules. Note that this will abbreviate any 2-element
    ; list whose car is "syntax", whether you want that or not!
    (syntax  (#\# #\'))
    (quasisyntax (#\# #\`))
    (unsyntax-splicing (#\# #\, #\@)
    (unsyntax (#\# #\,)))))

; return #t if we should as a traditional abbreviation, e.g., '
(define (represent-as-abbreviation? x)
  (and (list2? x)
       (assq (car x) abbreviations)))

; The car(x) is the symbol for an abbreviation; write the abbreviation.
(define (write-abbreviation x port)
  (for-each (lambda (c) (display c port))
    (cadr (assq (car x) abbreviations))))


; Return list x's *contents* represented as a list of characters.
; Each one must use neoteric-expressions, space-separated;
; it will be surrounded by (...) so no indentation processing is relevant.
(define (n-write-list-contents x port)
  (cond
    ((null? x) (values))
    ((pair? x)
      (n-write-simple (car x) port)
      (cond ((not (null? (cdr x)))
        (display " " port)
        (n-write-list-contents (cdr x) port))))
    (#t
      (display ". " port)
      (n-write-simple x port))))

(define (c-write-list-contents x port)
  (cond
    ((null? x) (values))
    ((pair? x)
      (c-write-simple (car x) port)
      (cond ((not (null? (cdr x)))
        (display " " port)
        (c-write-list-contents (cdr x) port))))
    (#t
      (display ". " port)
      (c-write-simple x port))))

; Return tail of an infix expression, as list of chars
; The "op" is the infix operator represented as a list of chars.
(define (infix-tail op x port)
  (cond
    ((null? x) (display "}" port))
    ((pair? x)
      (display " " port)
      (n-write-simple op port)
      (display " " port)
      (n-write-simple (car x) port)
      (infix-tail op (cdr x) port))
    (#t
      (display " " port)
      (n-write-simple x port)
      (display "}" port))))

; Return "x" as a list of characters, surrounded by {...}, for use as f{...}.
(define (as-brace-suffix x port)
  (display "{" port)
  (if (list2? x)
    (begin
      (n-write-list-contents x port)
      (display "}" port))
    (begin
      (n-write-simple (cadr x) port)
      (infix-tail (car x) (cddr x) port))))

(define (n-write-simple x port)
  (cond
    ((pair? x)
      (cond
        ((represent-as-abbreviation? x)              ; Format 'x
          (write-abbreviation x port)
          (n-write-simple (cadr x) port))
        ((long-and-boring? x)                        ; Format (a b c ...)
          (display "(" port)
          (n-write-list-contents x port)
          (display ")" port))
        ((symbol? (car x))
          (cond
            ((represent-as-inline-infix? x)          ; Format {a + b}
              (display "{" port)
              (n-write-simple (cadr x) port)
              (infix-tail (car x) (cddr x) port))
            ((and (list1? (cdr x))                   ; Format f{...}
              (pair? (cadr x))
              (represent-as-infix? (cadr x)))
                (n-write-simple (car x) port)
                (as-brace-suffix (cadr x) port))
            (#t                                      ; Format f(...)
              (n-write-simple (car x) port)
              (display "(" port)
              (n-write-list-contents (cdr x) port)
              (display ")" port))))
        (#t                                          ; Format (1 2 3 ...)
          (display "(" port)
          (n-write-list-contents x port)
          (display ")" port))))
    ((vector? x)
      (display "#( " port) ; Surround with spaces, easier to implement.
      (for-each (lambda (v) (n-write-simple v port) (display " " port))
        (vector->list x))
      (display ")" port))
    (#t (write x port))))                            ; Default format.


(define (c-write-simple x port)
  (cond
    ((pair? x)
      (cond
        ((represent-as-abbreviation? x)              ; Format 'x
          (write-abbreviation x port)
          (c-write-simple (cadr x) port))
        ((represent-as-inline-infix? x)              ; Format {a + b}
          (display "{" port)
          (n-write-simple (cadr x) port)
          (infix-tail (car x) (cddr x) port))
        (#t                                          ; Format (1 2 3 ...)
          (display "(" port)
          (c-write-list-contents x port)
          (display ")" port))))
    ((vector? x)
      (display "#( " port) ; Surround with spaces, easier to implement.
      (for-each (lambda (v) (c-write-simple v port) (display " " port))
        (vector->list x))
      (display ")" port))
    (#t (write x port))))                            ; Default format.


; Front entry - Use default port if none provided.
(define (neoteric-write-simple x . rest)
  (if (pair? rest)
      (n-write-simple x (car rest))
      (n-write-simple x (current-output-port))))

; Front entry - Use default port if none provided.
(define (curly-write-simple x . rest)
  (if (pair? rest)
      (c-write-simple x (car rest))
      (c-write-simple x (current-output-port))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST SUITE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define basic-tests
  '(
    (quote x)
    (a b c d e f g h i j k l m n o p q r s t u v w x y z)
    (a b c d e f g h i j k l m n o p q r s t u v w x y z . 2)
    (+ a b)
    (+ a b c)
    (+ a b c . improper)
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


(for-each (lambda (v) (curly-write-simple v) (newline)) basic-tests)

(newline)
(newline)

(for-each (lambda (v) (neoteric-write-simple v) (newline)) basic-tests)

(newline)

