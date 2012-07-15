
; List comporehension macro
; by almkglor
;
;
; Copyright (C) 2012 Alan Manuel K. Gloria.
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation
; files (the "Software"), to deal in the Software without
; restriction, including without limitation the rights to use,
; copy, modify, merge, publish, distribute, sublicense, and/or
; sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following
; conditions:
;
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the
; Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
; KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
; WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
; PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

; The list-of macro is highly similar to the stream-of
; macro in SRFI-41, except it's for lists.
;
; This module is intended to be a proof-of-point, that
; sweet-expressions work well with macros (or at least
; Scheme hygienic macros).

define-module
  example list-of
  :export
  \
    ; in Guile 1.6, module macros need to export the
    ; functions they use.
    list-of::concat-map
  :export-syntax
  \
    list-of

; Guile < 2.0 requires this
use-modules
  ice-9 syncase

define-syntax list-of
  syntax-rules (is in)
    ; base case
    \
    . list-of x
    . \ list x
    ; handle (var in x) clause
    \
    . list-of x
    .   var in expr
    .   clauses \ ...
    . \ list-of::concat-map
    . .   lambda (var)
    . .     list-of x
    . .       clauses \ ...
    . .   expr
    ; handle (var is x) clause
    \
    . list-of x
    .   var is expr
    .   clauses \ ...
    . \ let ((var expr))
    . .   list-of x
    . .     clauses \ ...
    ; handle (pred? x) clause
    \
    . list-of x
    .   pred?(args ...)
    .   clauses \ ...
    . \ if pred?(args ...)
    . .    list-of x
    . .      clauses \ ...
    . .    '()

define list-of::concat-map(f l)
  concat-map-loop(f l '())

define concat-map-loop(f l rv)
  cond
    pair?(l)
      concat-map-loop f cdr(l)
        append
          reverse
            f(car(l))
          rv
    null?(l)
      reverse(rv)
    #t
      error "list-of: internal error, produced sub-expression did not result in list!"

