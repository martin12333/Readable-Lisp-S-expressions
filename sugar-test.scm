#!/usr/bin/env guile
!#

;  Test sugar.scm.  Also has demo of its use.
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
      (write calculation)
      (display "\n\n")))
  (display "\n"))

(define (assert value)
  (test value #t))

(test 1 1)

; Load up unit under test:

(load "sugar.scm")

; (define (reader-test correct-value)
;  (test (sugar-read tc) correct-value))

(define (dotests testsuite)
  (let ((correct (read testsuite)))
    (cond
      ((eof-object? correct) #t)
      (#t
        (test (sugar-read testsuite) correct)
        (dotests testsuite)))))


; (use-modules (ice-9 debug))
; (set-breakpoint! trace-subtree 2)
; (set! (bp-behaviour (get-breakpoint 3)) debug-here)
; (trace modern-process-tail)
; (trace my-read-delimited-list)
; (trace underlying-read)

; Test lower-level routines:


; sugar-testsuite has text of the form:
;    s-expression
;    i-expression (which should match)

(define full-testsuite (open-input-file "sugar-testsuite"))
(dotests full-testsuite)


(display "Tests complete!")
(newline)
(display "  Errors: ")
(display test-error)
(display "  Correct: ")
(display test-correct)
(newline)
(quit)

