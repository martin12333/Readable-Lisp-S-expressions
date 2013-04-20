; basic-curly.cl
; This implements "basic curly-infix-expressions" for Common Lisp.
; This is an easy-to-use infix notation that works
; well with other Common Lisp capabilities (e.g., quasiquoting and macros).
; It's homoiconic (you can see where lists start and end) and doesn't require
; registration of operators before use. For more information, see:
;   http://readable.sourceforge.net.
;
; Copyright (C) 2007-2013 by David A. Wheeler
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
;
;
; This implements an basic-curly-infix reader, an extension of s-expressions
; that can read "basic curly-infix lists".  A basic curly-infix list
; is surrounded by {...} instead (...), and the reader maps it as follows:
; * {a op b [op c [op d ...]]} => (op a b [c [d ...]]).
;   E.G., {x > 1} => (> x 1), and {a + b + c} => (+ a b c)
;   In short, a curly-infix list with an odd number of parameters, at least
;   three parameters, and all even parameters are "equal", maps to
;   a list with the first even parameter followed by the odd parameters.
; * {} => ()
; * {e} => e, so {5} => 5.
; * {e1 e2} => (e1 e2), e.g., {- x} => (- x).
; * Otherwise, {...} maps to ($nfx$ ...).
; A non-empty basic curly-infix list must be a proper list.
;
; There's no precedence, but that's no problem,
; just use another curly-infix list or traditional list:
;   {2 + {4 * 5}} => (+ 2 (* 4 5))
;   {{- x} / 2}   => {(- x) / 2}   => (/ (- x) 2)

(cl:in-package :readable)

; Utility functions to implement the simple infix system:

; Return true if lyst has an even # of parameters, and the (alternating) first
; ones are "op".  Used to determine if a longer lyst is infix.
; Otherwise it returns NIL (False).
; If passed empty list, returns true (so recursion works correctly).
(defun even-and-op-prefixp (op lyst)
   (cond
     ((null lyst) t)
     ((not (consp lyst)) nil) ; Not a list.
     ((not (equal op (car lyst))) nil) ; fail - operators not all equal.
     ((not (consp (cdr lyst))) nil) ; fail - wrong # or improper list.
     (t (even-and-op-prefixp op (cddr lyst))))) ; recurse.

; Return True if the lyst is in simple infix format (and should be converted
; at read time).  Else returns NIL.
(defun simple-infix-listp (lyst)
  (and
    (consp lyst)           ; Must have cons;  '() doesn't count.
    (consp (cdr lyst))     ; Must have a second argument.
    (consp (cddr lyst))    ; Must have a third argument (we check it
                           ; this way for performance)
    (even-and-op-prefixp (cadr lyst) (cdr lyst)))) ; even parameters equal?

; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
(defun alternating-parameters (lyst)
  (if (or (null lyst) (null (cdr lyst)))
    lyst
    (cons (car lyst) (alternating-parameters (cddr lyst)))))

; Transform not-simple infix list.  Written as a separate function so that
; future versions/specifications can easily replace just this piece.
(defun transform-mixed-infix (lyst)
  (cons '$nfx$ lyst))

; Take list that was in {...}, convert to final form.
(defun process-curly (lyst)
  (cond
    ((not (consp lyst)) ; E.G., map {} to ().
      lyst)
    ((null (cdr lyst)) ; Map {a} to a.
      (car lyst))
    ((and (consp (cdr lyst)) (null (cddr lyst))) ; Map {a b} to (a b).
      lyst)
    ((simple-infix-listp lyst) ; Map {a op b} to (op a b).
      (cons (cadr lyst) (alternating-parameters lyst)))
    (t
      (transform-mixed-infix lyst))))

; Read until }, then process list as infix list.
(defun curly-brace-infix-reader (stream char)
  (declare (ignore char))
  (let ((result (read-delimited-list #\} stream t)))
    (process-curly result)))

(defun enable-basic-curly ()
  ; The following install the {...} reader.
  ; See "Common Lisp: The Language" by Guy L. Steele, 2nd edition,
  ; pp. 542-548 and pp. 571-572.
  ; Invoke curly-brace-infix-reader when opening curly brace is read in:
  (set-macro-character #\{ #'curly-brace-infix-reader)
  ; This is necessary, else a cuddled closing brace will be part of an atom:
  (set-macro-character #\} (get-macro-character #\) nil))
  nil)

