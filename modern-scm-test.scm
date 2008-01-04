;  Test modern.scm.  Also has demo of its use.
; 
;  It should print T for the successful loading of the file,
;  followed by a bunch of NILs (the result of successful asserts) and
;  some other calculations, WITHOUT any "assertion fails" messages.
;  If there is an assertion error message of the form:
;   *** - ....
;  then you must fix something.
; 
;  Copyright (C) 2008 by David A. Wheeler.
;  Released under the "MIT license":
;  Permission is hereby granted, free of charge, to any person obtaining a
;  copy of this software and associated documentation files (the "Software"),
;  to deal in the Software without restriction, including without limitation
;  the rights to use, copy, modify, merge, publish, distribute, sublicense,
;  and/or sell copies of the Software, and to permit persons to whom the
;  Software is furnished to do so, subject to the following conditions:
;  
;  The above copyright notice and this permission notice shall be included
;  in all copies or substantial portions of the Software.
;  
;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;  OTHER DEALINGS IN THE SOFTWARE.


(load "modern.scm")

(define test-error 0)
(define test-correct 0)

(define (test calculation correct . comparitor)
  (if (null? comparitor)
     (set! comparitor equal?)
     (set! comparitor (car comparitor)))
  (display "Comparing with ")
  (write correct)
  (newline)
  (cond
    ((comparitor calculation correct)
      (set! test-correct (+ test-correct 1))
      (display "CORRECT\n"))
    (#t
      (set! test-error (+ test-error 1))
      (display "ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      (display "Got:\n")
      (display calculation)
      (display "\n\n")))
  (display "\n"))

(define (assert value)
  (test value #t))

(test 1 1)

; Test lower-level routines:

(assert (even-and-op-prefix '+ '(+ 4)))
(assert (even-and-op-prefix '+ '(+ 4 + 5)))
(assert (not (even-and-op-prefix '+ '(+ 4 - 5))))
(assert (not (even-and-op-prefix '+ '(+ 4 +))))

(assert (simple-infix-listp '(1 + 2)))
(assert (simple-infix-listp '(1 + 2 + 3)))
(assert (not (simple-infix-listp '(1 + 2 * 3))))
(assert (not (simple-infix-listp '(1 + 2 +))))
(assert (not (simple-infix-listp '(1 +))))
(assert (not (simple-infix-listp '(1))))
(assert (not (simple-infix-listp '())))

(test (alternating-parameters '(1 2 3)) '(1 3))
(test (alternating-parameters '(1 2 3 4 5)) '(1 3 5))

(test (transform-simple-infix '(1 + 3)) '(+ 1 3))
(test (transform-simple-infix '(1 + 3 + 5)) '(+ 1 3 5))

(define tc (open-input-file "test-cases-modern"))

(test (begin (skip-whitespace tc) (read-char tc))
      #\x)

(display "Now testing reader itself.\n")

(define (reader-test correct-value)
  (test (modern-read tc) correct-value))

(reader-test 'testing123)

(reader-test '(hi))

(reader-test '(hi))

(reader-test '(+ 3 4))

(reader-test 'q) ; Comment-only lines.
(reader-test 'a) ; Inline comments.
(reader-test 'b)

(reader-test ''x)

; Test quasi-quoting.
(reader-test '`(x))

(reader-test '(f x))
(reader-test '(f x))

(reader-test '(f a b))
(reader-test '(f))
(reader-test '(+ 3 (* 4 5)))

(reader-test '`(,(x)))

(reader-test '`(,@(x)))

(reader-test '(+ 3 (* 4 5)))

(reader-test '(bracketaccess f a))

(reader-test '(f (+ 2 3)))

(reader-test '((f a) b))

(reader-test '(+ 2 3))
(reader-test ''(+ 2 3))
(reader-test '(f a b c))
(reader-test '(f a b c))
(reader-test '(fibup n 2 1 0))
(reader-test '(if (fibup n 2 1 0)))


;!!   (reader-test ''(+ 2 3))
;!!     '{2 + 3}
;!!   
;!!   (reader-test '(f a b c))
;!!     f(a b c)
;!!   
;!!   (reader-test '(f a b c))
;!!     f(a
;!!       b c)
;!!   
;!!   (reader-test '(fibup n 2 1 0))
;!!       fibup(n 2 1 0)
;!!   
;!!   (reader-test '(if (fibup n 2 1 0)))
;!!   [if fibup(n 2 1 0)]
;!!   
;!!   (reader-test
;!!     '(defun fibup (max count n-1 n-2)
;!!      (if (= max count) (+ n-1 n-2) (fibup max (+ count 1) (+ n-1 n-2) n-1))))
;!!   
;!!   [defun fibup (max count n-1 n-2)
;!!     [if {max = count}
;!!       {n-1 + n-2}
;!!       [fibup max {count + 1} {n-1 + n-2} n-1]]]
;!!   
;!!   ; Calling "read-preserving-whitespace" with the modern-read table set
;!!   ; won't work; when you read this in:
;!!   ; [defun fibup (max count n-1 n-2)
;!!   ;   [if {max = count}
;!!   ;     {n-1 + n-2}
;!!   ;     [fibup max {count + 1} {n-1 + n-2} n-1]]]
;!!   ;
;!!   ; the calls to read-preserving-space STILL consume the space after
;!!   ; defun, fibup, etc., resulting in wrong interpretation.
;!!   
;!!   (princ "Problem coming up!")
;!!   
;!!   ; The backquote character will be caught by the sh shell, so
;!!   ; shell-escape it.  The backslash won't go to the Lisp interpreter.
;!!   (reader-test (quote \`x))
;!!    \`x
;!!   
;!!   ; PROBLEM: comma-lifting not working.
;!!   ; Need to re-implement backquote, comma, splicing :-(.
;!!   (reader-test (quote ((system::unquote x))))
;!!   \`[,x]
;!!   
;!!   ; Misc. notes - not used:
;!!   ; (assert (equal '{2 + 3} '(+ 2 3)))
;!!   ; 
;!!   ; (assert (equal '{2 * n} '(* 2 n)))
;!!   ; (assert (equal '{x eq y} '(eq x y)))
;!!   ; (assert (equal '{2 + 3 + 4} '(+ 2 3 4)))
;!!   ; (assert (equal '{2 + 3 * 4} '(NFX 2 + 3 * 4)))
;!!   ; (assert (equal '{(- x) / 2} '(/ (- x) 2)))
;!!   ; (assert (equal '{x = 3 * 4} '(nfx x = 3 * 4)))
;!!   ; (assert (equal '{x = 3} '(= x 3)))
;!!   
;!!   ; (assert (equal '{2 + {3 * 4}} '(+ 2 (* 3 4))))
;!!   ; (read)
;!!   ; '{2 + {3 * 4}}
;!!   ;(defun f1 (x)
;!!   ;  (princ "I'm f1"))
;!!   ;(defun f2 (x)
;!!   ;  (princ "I'm f2"))
;!!   ; (princ "Calling f2")
;!!   ; (f2 'call2)
;!!   ; (defun f2 (x) (funcall 'f1 x))
;!!   ; (setf (symbol-function 'f2) (function f1)
;!!   ; (princ "Calling f2, should print f1")
;!!   ; (f2 'call2)
;!!   ; (modern-read)
;!!   ; [defun fibfast (n)
;!!   ;   [if {n < 2}
;!!   ;     n
;!!   ;     fibup(n 2 1 0)]]
;!!   ; 
;!!   ; ; Demo its use:
;!!   ; 
;!!   ; [defun fibfast (n)
;!!   ;   [if {n < 2}
;!!   ;     n
;!!   ;     [fibup n 2 1 0]]]
;!!   ; 
;!!   ; [defun fibup (max count n-1 n-2)
;!!   ;   [if {max = count}
;!!   ;     {n-1 + n-2}
;!!   ;     [fibup max {count + 1} {n-1 + n-2} n-1]]]
;!!   ; 
;!!   ; [setf x 5]
;!!   ; [setf correct {1 <= x <= 10}]
;!!   ; 
;!!   ; ; Test that demo worked correctly:
;!!   ; (assert (= (fibfast 10) 55))
;!!   ; (assert correct)
;!!   ; 
;!!   ; ; More demos:
;!!   ; 
;!!   ; [setf y {3 + {4 * 5}}]
;!!   ; [setf z {{1 <= x <= 10} and {x > 0}}]

(display "Tests complete!")
(newline)
(display "  Errors: ")
(display test-error)
(display "  Correct: ")
(display test-correct)
(newline)
(quit)
