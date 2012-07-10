#!/usr/bin/env guile
!#

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


; guile-specific stuff - set up and load using modules
(set! %load-path (append %load-path '("..")))
(use-modules (modern))

; Portable:

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
; We no longer run these tests - we now use the guile modules
; system and don't export these functions.  Thus, we don't have access
; to them to test them.

; (assert (even-and-op-prefix '+ '(+ 4)))
; (assert (even-and-op-prefix '+ '(+ 4 + 5)))
; (assert (not (even-and-op-prefix '+ '(+ 4 - 5))))
; (assert (not (even-and-op-prefix '+ '(+ 4 +))))
; 
; (assert (simple-infix-listp '(1 + 2)))
; (assert (simple-infix-listp '(1 + 2 + 3)))
; (assert (not (simple-infix-listp '(1 + 2 * 3))))
; (assert (not (simple-infix-listp '(1 + 2 +))))
; (assert (not (simple-infix-listp '(1 +))))
; (assert (not (simple-infix-listp '(1))))
; (assert (not (simple-infix-listp '())))
; 
; (test (alternating-parameters '(1 2 3)) '(1 3))
; (test (alternating-parameters '(1 2 3 4 5)) '(1 3 5))
; 
; (test (transform-simple-infix '(1 + 3)) '(+ 1 3))
; (test (transform-simple-infix '(1 + 3 + 5)) '(+ 1 3 5))

(define tc (open-input-file "test-cases-modern"))

; (test (begin (skip-whitespace tc) (read-char tc)) sharp backslash x )

(display "Now testing reader itself.\n")

(define (reader-test correct-value)
  (test (modern-read tc) correct-value))

; (use-modules (ice-9 debug))
; (set-breakpoint! trace-subtree 2)
; (set! (bp-behaviour (get-breakpoint 3)) debug-here)
; (trace modern-read2)
; (trace modern-process-tail)
; (trace my-read-delimited-list)
; (trace underlying-read)

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

(reader-test '(char=? c #\space))

(reader-test '(and #f #t))

(reader-test '(char=?-ci c #\[))

(reader-test '(= i 10))

(reader-test .2)

(reader-test '(a ... b))

(reader-test '(+ 5 6))

(reader-test '(- -8 9))

(reader-test '(string<? "\\a" "\\b"))

(reader-test '(define (fibfast n) (if (< n 2) n (fibup n 2 1 0))))
(reader-test
 '(define (fibup max count n-1 n-2)
    (if (= max count) (+ n-1 n-2)
      (fibup max (+ count 1) (+ n-1 n-2) n-1))))

(reader-test '(define (fibfast n) (if (< n 2) n (fibup n 2 1 0))))
(reader-test
 '(define (fibup max count n-1 n-2)
    (if (= max count) (+ n-1 n-2)
      (fibup max (+ count 1) (+ n-1 n-2) n-1))))

(reader-test '(a . b))

(reader-test '(a . b))

(reader-test 'b) ; ( . b) is not _required_ by Scheme, but it's common and
; it'll mean there's a portable way to express the group symbol in I-expr.

(reader-test 'hi)

(reader-test 'a123)

(reader-test '(x y))

(reader-test '#(0 1 2))

(reader-test '())

(reader-test '(f . b))

; Common Lisp example

(reader-test
 '(defun factorial (n)
   (if (<= n 1)
       1
       (* n (factorial (- n 1))))))

(reader-test
 '(defun factorial (n)
   (if (<= n 1)
       1
       (* n (factorial (- n 1))))))

; Scheme example

(reader-test
 '(define (factorial n)
   (if (<= n 1)
       1
       (* n (factorial (- n 1))))))

(reader-test
 '(define (factorial n)
   (if (<= n 1)
       1
       (* n (factorial (- n 1))))))

; BitC  example (slightly tweaked)
(reader-test
'(define (fact (: x int32))
  (cond ((< x 0) (- (fact (- x))))
        ((= x 0) 1)
        (otherwise
          (* x (fact (- x 1)))))))

(reader-test
'(define (fact (: x int32))
  (cond ((< x 0) (- (fact (- x))))
        ((= x 0) 1)
        (otherwise
          (* x (fact (- x 1)))))))

; Nested expression.

(reader-test '(and (<= 1 x 10)  (> x 0)))
(reader-test '(and (<= 1 x 10)  (> x 0)))

(reader-test
'(defun replace-html-chars (start end)
  "Replace '&lt;' to '&amp;lt;' and other chars in HTML.
This works on the current selection."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&" nil t) (replace-match "&amp;" nil t))
    (goto-char (point-min))
    (while (search-forward "<" nil t) (replace-match "&lt;" nil t))
    (goto-char (point-min))
    (while (search-forward ">" nil t) (replace-match "&gt;" nil t)))))

(reader-test
'(defun replace-html-chars (start end)
  "Replace '&lt;' to '&amp;lt;' and other chars in HTML.
This works on the current selection."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&" nil t) (replace-match "&amp;" nil t))
    (goto-char (point-min))
    (while (search-forward "<" nil t) (replace-match "&lt;" nil t))
    (goto-char (point-min))
    (while (search-forward ">" nil t) (replace-match "&gt;" nil t)))))

; This is AutoCAD Lisp.

(reader-test
'(DEFUN C:FIND ()
  (SETQ SA (GETSTRING T "\nEnter string for search parameter: "))
  (SETQ AR (SSGET "X" (LIST (CONS 1 SA))))
  (IF (= AR NIL) (ALERT "This string does not exist"))
  (SETQ SB (SSLENGTH AR))
  (C:CONT)))

(reader-test
'(DEFUN C:CONT ()
  (SETQ SB (- SB 1))
  (SETQ SC (SSNAME AR SB))
  (SETQ SE (ENTGET SC))
  (SETQ SJ (CDR (ASSOC 1 SE)))
  (IF (= SJ SA)
    (PROGN
      (SETQ H (CDR (ASSOC 10 SE)))
      (SETQ X1 (LIST (- (CAR H) 50) (- (CADR H) 50)))
      (SETQ X2 (LIST (+ 50 (CAR H)) (+ 50 (CADR H))))
      (COMMAND "ZOOM" "W" X1 X2))
    (C:CONT))
  (IF (= SB 0) (ALERT "END OF SELECTIONS"))
  (SETQ A (+ SB 1))
  (SETQ A (RTOS A 2 0))
  (SETQ A
    (STRCAT "\nThere are <" A "> selections Enter CONT to advance to next"))
  (IF (= SB 0) (EXIT))
  (PRINC A)
  (PRINC)))

(reader-test
'(DEFUN C:FIND ()
  (SETQ SA (GETSTRING T "\nEnter string for search parameter: "))
  (SETQ AR (SSGET "X" (LIST (CONS 1 SA))))
  (IF (= AR NIL) (ALERT "This string does not exist"))
  (SETQ SB (SSLENGTH AR))
  (C:CONT)))

(reader-test
'(DEFUN C:CONT ()
  (SETQ SB (- SB 1))
  (SETQ SC (SSNAME AR SB))
  (SETQ SE (ENTGET SC))
  (SETQ SJ (CDR (ASSOC 1 SE)))
  (IF (= SJ SA)
    (PROGN
      (SETQ H (CDR (ASSOC 10 SE)))
      (SETQ X1 (LIST (- (CAR H) 50) (- (CADR H) 50)))
      (SETQ X2 (LIST (+ 50 (CAR H)) (+ 50 (CADR H))))
      (COMMAND "ZOOM" "W" X1 X2))
    (C:CONT))
  (IF (= SB 0) (ALERT "END OF SELECTIONS"))
  (SETQ A (+ SB 1))
  (SETQ A (RTOS A 2 0))
  (SETQ A
    (STRCAT "\nThere are <" A "> selections Enter CONT to advance to next"))
  (IF (= SB 0) (EXIT))
  (PRINC A)
  (PRINC)))


(reader-test
'(if (eqv? parent1 'f)
  (and (eqv? kibi 'f) (eqv? kibi-lied? #t))))

(reader-test
'(if (eqv? parent1 'f)
  (and (eqv? kibi 'f) (eqv? kibi-lied? #t))))

(reader-test
'("hi" 3))




(display "Tests complete!")
(newline)
(display "  Errors: ")
(display test-error)
(display "  Correct: ")
(display test-correct)
(newline)
(quit)

