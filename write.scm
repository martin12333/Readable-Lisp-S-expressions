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
(define (multi-element-list? x)
  (and (pair? x) (not (null? (cdr x)))))

; Returns #t if x is a list with exactly 1 element.  Improper lists are #f.
(define (list1? x)
  (and (pair? x) (null? (cdr x))))

; Returns #t if x is a list with exactly 2 elements.  Improper lists are #f.
(define (list2? x)
  (and (pair? x) (pair? (cdr x)) (null? (cddr x))))

; Returns #t if x is a list or improper list of 3+ elements.
(define (list-3-plus? x)
  (and (pair? x) (pair? (cdr x)) (pair? (cddr x))))

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

; An x is boring if it's not a list, or it's a list with ONLY non-pair members.
(define (boring? x)
  (cond ((not (pair? x)) #t)
        ((pair? (car x)) #f)
        (#t (boring? (cdr x)))))

; A list is long and boring if it's a list, its length is at least
; the boring-length, and it's boring.
; A long-and-boring list is almost certainly NOT a function call or a
; body of some executable sequence - it's almost certainly a long
; boring list of data instead. If it is, we want to display it differently.
(define (long-and-boring? x)
  (cond ((not (pair? x)) #f)
        ((not (list? x)) #f)
        ((< (length x) boring-length) #f)
        (#t (boring? x))))

; Support general-length-inner - help count length of possibly-improper list.
(define (general-length-inner x count-so-far)
  (cond ((null? x) count-so-far)
        ((not (pair? x)) (+ count-so-far 1))
        (#t
         (general-length-inner (cdr x) (+ count-so-far 1)))))

; Return length of x, which may be an improper list.
; If improper, count the two sides as two, so "(a . b)" is length 2.
(define (general-length x)
  (general-length-inner x 0))

; Return #t if x should be represented using curly-infix notation {...}.
(define (represent-as-infix? x)
  (and (pair? x)
       (is-infix-operator? (car x))
       (list? x)
       (<= (length x) 6)))

(define (represent-as-inline-infix? x)
  (and (represent-as-infix? x) (>= (length x) 3)))

; Return #t if x should be represented as a brace suffix
(define (represent-as-brace-suffix? x)
  (and (represent-as-infix? x) (>= (length x) 2)))

; Define an association list mapping the Lisp function names which have
; abbreviations ==> the list of characters in their abbreviation
(define abbreviations
  '((quote (#\'))
    (quasiquote (#\`))
    (unquote (#\,))
    (unquote-splicing (#\, #\@))))

; return #t if we should as a traditional abbreviation, e.g., '
(define (represent-as-abbreviation? x)
  (and (pair? x)
       (assq (car x) abbreviations)
       (pair? (cdr x))
       (null? (cddr x))))

; Return list x's *contents* represented as a list of characters.
; Each one must use neoteric-expressions, space-separated;
; it will be surrounded by (...) so no indentation processing is relevant.
(define (neoteric-write-unit-list x)
  (cond
    ((null? x) (values))
    ((pair? x)
      (neoteric-write-simple (car x))
      (cond ((not (null? (cdr x)))
        (display " ")
        (neoteric-write-unit-list (cdr x)))))
    (#t
      (display ". ")
      (neoteric-write-simple x))))

; Return tail of an infix expression, as list of chars
; The "op" is the infix operator represented as a list of chars.
(define (infix-tail op x)
  (cond
    ((null? x) (display "}"))
    ((pair? x)
      (display " ")
      (neoteric-write-simple op)
      (display " ")
      (neoteric-write-simple (car x))
      (infix-tail op (cdr x)))
    (#t
      (display " ")
      (neoteric-write-simple x)
      (display "}"))))

; Return "x" as a list of characters, surrounded by {...}, for use as f{...}.
(define (as-brace-suffix x)
  (display "{")
  (if (< (general-length x) 3)
    (begin
      (neoteric-write-unit-list x)
      (display "}"))
    (begin
      (neoteric-write-simple (cadr x))
      (infix-tail (car x) (cddr x)))))


; Return x represented as a neoteric-expression unit,
; as a list of characters that are part of one line.
; Indentation processing *may* be active, but the character sequence
; returned must not depend on that.
; This is widely-used. If speed's a problem, memoize this;
; you can erase the memoized information once display-sweeten-format() has
; displayed the result.
(define (neoteric-write-simple x)
  (cond
    ((pair? x)
      (cond
        ((represent-as-abbreviation? x)              ; Format 'x
          (display (list->string (cadr (assq (car x) abbreviations))))
          (neoteric-write-simple (cadr x)))
        ((or (long-and-boring? x) (not (list? x)))
          (display "(")                              ; Format (a b c ...)
          (neoteric-write-unit-list x)
          (display ")"))
        ((symbol? (car x))
          (cond
            ((represent-as-inline-infix? x)          ; Format {a + b}
              (display "{")
              (neoteric-write-simple (cadr x))
              (infix-tail (car x) (cddr x)))
            ((and (list1? (cdr x))
              (pair? (cadr x))
              (represent-as-brace-suffix? (cadr x))) ; Format f{...}
                (neoteric-write-simple (car x))
                (as-brace-suffix (cadr x)))
            (#t                                      ; Format f(...)
              (neoteric-write-simple (car x))
              (display "(")
              (neoteric-write-unit-list (cdr x))
              (display ")"))))
        (#t                                          ; Format (1 2 3 ...)
          (display "(")
          (neoteric-write-unit-list x)
          (display ")"))))
    (#t (write x))))                                 ; Everything else.



; Tests.
; TODO: Clean up.

(neoteric-write-simple '(quote x))
(newline)
(neoteric-write-simple '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(newline)
(neoteric-write-simple '(+ a b))
(newline)
(neoteric-write-simple '(+ a b c))
(newline)
(neoteric-write-simple '(sin (- theta)))
(newline)
(neoteric-write-simple '(fact (- n 1)))
(newline)
(neoteric-write-simple '(between current min max))
(newline)
(neoteric-write-simple '(sin x))
(newline)
(neoteric-write-simple '(current-time))
(newline)
(neoteric-write-simple '(1 2 3))
(newline)
(neoteric-write-simple 5)
(newline)
(neoteric-write-simple 'boring-symbol)
(newline)

(neoteric-write-simple '(+ (sqrt x) (sqrt y)))
(newline)
