; Test Common Lisp library. Also has demo of its use.

; Run this file on a Unix-like system to test curly-infix.cl.
; You must have clisp.

; Copyright (C) 2007-2012 by David A. Wheeler.
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


; Assume ASDL is set up, e.g., by setting environment var CL_SOURCE_REGISTRY
; to the name of the directory of the .asd file, followed by "/".

(require "asdf")
(setf (readtable-case *readtable*) :invert)
(asdf:load-system :readable)
(use-package :readable)

; From http://www.cliki.net/Portable%20Exit
(defun my-quit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      ; #+sbcl (sb-ext:quit
      ;         :unix-code (typecase code (number code) (null 0) (t 1)))
      #+sbcl (sb-ext:exit :code code)
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; This group from <hebi...@math.uni.wroc.pl>
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'quit code)))

(defun mytest (input correct)
  (cond
    ((equal input correct)
      (princ "Success with: ")
      (prin1 input)
      (format t "~%"))
    (t
      (princ "FAIL!!!")
      (print input)
      (print correct)
      (my-quit 1))))

; Special test values:
(defvar fab '(f a b))
(defvar g5qx '(g #b101 (quote x)))
(defvar gp5qx '(g (#b101 quote x)))
(defvar hnn7 '(h () () 7))
(defvar x)
(defvar z)




(enable-full-curly-infix)

(mytest '{cos(x) + sin(x)} '(+ (cos x) (sin x)))
(mytest '{fibup(n 0 1)} '(fibup n 0 1))
(mytest '{- x} '(- x))
(mytest '{f()} '(f))
(mytest '{f(a b) (c d)} '((f a b) (c d)))

(mytest '(f a b) fab)
(mytest '{f(a b)} fab)

(mytest '(g #b101 (quote x)) g5qx)
(mytest '{g(#b101 (quote x))} g5qx)

(mytest '(g (#b101 quote x)) gp5qx)
(mytest '{g(#b101(quote x))} gp5qx)

(mytest '(h () () 7) hnn7)
(mytest '{h(() () 7)} hnn7)

(disable-readable)
(enable-neoteric)

(mytest 'f(a b) '(f a b))
(mytest 'g() '(g))
(mytest '{f(a b) (c d)} '((f a b) (c d)))

(mytest '(f a b) fab)
(mytest '{f(a b)} fab)

(mytest '(g #b101 (quote x)) g5qx)
(mytest '{g(#b101 (quote x))} g5qx)

(mytest '(g (#b101 quote x)) gp5qx)
(mytest '{g(#b101(quote x))} gp5qx)

(mytest '(h () () 7) hnn7)
(mytest '{h(() () 7)} hnn7)

(disable-readable)
(enable-sweet)

; Work around a bug in Windows SBCL readtable:
(setf (readtable-case *readtable*) :invert)

mytest
  ' f 1 2
  '(f 1 2)

(mytest '(f a b) fab)
(mytest '{f(a b)} fab)
mytest
  ' f a b
  fab

(mytest '(g #b101 (quote x)) g5qx)
(mytest '{g(#b101 (quote x))} g5qx)
mytest
  ' g #b101 (quote x)
  g5qx

(mytest '(g (#b101 quote x)) gp5qx)
(mytest '{g(#b101(quote x))} gp5qx)
mytest
  ' g #b101(quote x)
  gp5qx

(mytest '(h () () 7) hnn7)
(mytest '{h(() () 7)} hnn7)
mytest
  ' h () () 7
  hnn7

mytest
  ' foo k() m(1) n(2 3) m(3 4 5)
  '(foo (k) (m 1) (n 2 3) (m 3 4 5))

mytest
  ' foo
    a b \\ c d \\ e \\ f g h
    j k $ l m $ n
  '(foo (a b) (c d) e (f g h)
    (j k (l m n)))

mytest
  ' \\
    a b
    c d
  '((a b) (c d))

mytest
  ' let <* x $ 0 \\ y $ cos theta *>
    {{x ** 2} + {y ** 2}}
  '(let ((x 0) (y (cos theta))) (+ (** x 2) (** y 2)))

mytest
  ' let <*

x 0

y $ cos theta

*>
    {{x ** 2} + {y ** 2}}
  '(let ((x 0) (y (cos theta))) (+ (** x 2) (** y 2)))

mytest
  ' foo1
    a
    .
    b
  '(foo1 a . b)


mytest
  ' foo2 a . b
  '(foo2 a . b)

mytest
  ' foo3
    a
    #| comment |#
      b1 b2
        q1 q2
      c1 c2
    d
  '(foo3 a ((b1 b2 (q1 q2)) (c1 c2)) d)

mytest
  ' h #| ignored |# j k
  '(h j k)

mytest
  ` a b ,{3 + 4}
    x1
    ,
      {10 + 11}
  '(a b 7 x1 21)

mytest
  ` q
    ,@ cdr('(x y z))
  '(q y z)

mytest
  ' defun fact (n)
  ! if {n <= 1}
  !   1
  !   {n * fact{n - 1}}
  '(defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))

mytest
  ' foo1
  !   a1
  !   #;x
  !   b1
  '(foo1 a1 b1)

mytest
  ' foo2
  !   a2
  !   #;x b2
  !   c2
  '(foo2 a2 b2 c2)

mytest
  ` foo3
  !   a3
  !   ,@
  !     cdr
  !       ' b0 b1 (b2a b2b) b3
  !   c3
  '(foo3 a3 b1 (b2a b2b) b3 c3)

mytest
  ' yaa zaa
    #; x y z
  '(yaa zaa)

mytest
  ' ya1
    za1
    #; this line is ignored
    zb3
  '(ya1 za1 zb3)

mytest
  ' m1
    p1
    #;
      q1 q2
      r1 r2 r3
        f4
    p2
  '(m1 p1 p2)

mytest
  ' m7
    p8
    p9
    #; n1 n2
      q1 q2
      r1 r2 r3
        f4
  '(m7 p8 p9)

mytest
  ' x1 #; y z1
  '(x1 z1)

mytest
  ' a88 . #; qqq b89
  '(a88 . b89)

mytest
  ' foo
    #; bar
  '(foo)

mytest
  '    firstone          secondone      thirdone   
  '(firstone secondone thirdone)

mytest
  ' <* #; 1 *> <* #; 2 *> <* #; 3 *> <* stuff *>
  '(() () () (stuff))

mytest
  ' <*
aa1
ab1 ab2
*>
  '(aa1 (ab1 ab2))

mytest
  '(a (b c d e))
  ' a
    b c \\
    d e

mytest
  '(a (b c d))
  ' a
    b c \\
    d

mytest
  '(a b c (d e f (g h)))
  ' a b c
    d e \\
    f $ g h

mytest
  '(a b c (d e f g))
  ' a b c
    d e \\
    f $ g

mytest
  '(a b c (d e f g))
  ' a b c
    d e \\
    ; Test1
    f g

mytest
  '(a b c (d e f g))
  ' a b c
    d e \\ ; Test2
    f g

mytest
  '(a b c (q e f . g))
  ' a b c
    q e \\ ; Test "."
    f . g

mytest
  '(a b c (q e . z))
  ' a b c
    q e \\ ; Test "."
    . z

mytest
  '(1 (2 3 4 5 6))
  ' 1
    2 3 \\
    4 5 \\
    6

mytest
  179/104
  {11/13 + 7/8}

mytest
  '(* 4 5)
  '*(4 5)

; Now test writing back.

mytest
  "this-is-a-test"
  write-to-string-readable 'this-is-a-test

mytest
  "1"
  write-to-string-readable 1

mytest
  "(1)"
  write-to-string-readable '(1)

mytest
  "(1 2)"
  write-to-string-readable '(1 2)

mytest
  "(1 2 3)"
  write-to-string-readable '(1 2 3)

mytest
  "(aa)"
  write-to-string-readable '(aa) :notation 'basic-curly-infix

mytest
  "aa()"
  write-to-string-readable '(aa)

mytest
  "aa(bb)"
  write-to-string-readable '(aa bb)

mytest
  "aa(bb cc)"
  write-to-string-readable '(aa bb cc)

mytest
  "(a b c d e f g h i j k l m n o p q r s t u v)"
  write-to-string-readable '(a b c d e f g h i j k l m n o p q r s t u v)

mytest
  "(a b c d e f g h i j k l m n o p q r s t u v . z)"
  write-to-string-readable '(a b c d e f g h i j k l m n o p q r s t u v . z)

mytest
  "{4 + 5}"
  write-to-string-readable '(+ 4 5)

mytest
  "{4 + 5 + 6}"
  write-to-string-readable '(+ 4 5 6)

mytest
  "{a <= b}"
  write-to-string-readable '(<= a b)

mytest
  "{4 + {5 * 6}}"
  write-to-string-readable '(+ 4 (* 5 6))

mytest
  "{f(x) + 4 + 5 + 6}"
  write-to-string-readable '(+ (f x) 4 5 6)

mytest
  "foo(bar {1 + 2})"
  write-to-string-readable '(foo bar (+ 1 2))

mytest
  "spam(. eggs)"
  write-to-string-readable '(spam . eggs)

setq x '((aaa bbb ccc) . w)
setq x rplacd(x x)

mytest
  "#1=(aaa(bbb ccc) . #1#)"
  write-to-string-readable x :circle t


setq z '(1 2)
rplaca(z z)
rplacd(z z)

mytest
  "#1=(#1# . #1#)"
  write-to-string-readable z :circle t

(disable-readable)
(princ "Tests complete!")
(terpri)
(my-quit 0)

