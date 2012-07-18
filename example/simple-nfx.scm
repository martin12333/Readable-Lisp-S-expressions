; simple-nfx.scm
; a simple implementation for 'nfx macro.
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

; NOTE
; This very simple implementation only allows simple
; arithmetic precedence, and nothing else.

define-module
  example simple-nfx
  :export-syntax
  \
    nfx

cond-expand
  guile-2
    '() ; do nothing
  guile
    use-modules
      ice-9 syncase
    export-syntax nfx::helper

define-syntax nfx::helper
  syntax-rules (+ - * /)
    ; base case
    \
    . nfx::helper (x) ()
    . \ x
    ; * and / have highest priority - ignore pending adds
    \
    . nfx::helper (x * y terms ...) pending-add
    . \ nfx::helper ((* x y) terms ...) pending-add
    \
    . nfx::helper (x / y terms ...) pending-add
    . \ nfx::helper ((/ x y) terms ...) pending-add
    ; if + or -, and no pending adds, pend them
    \
    . nfx::helper (x + terms ...) ()
    . \ nfx::helper (terms ...) (x +)
    \
    . nfx::helper (x - terms ...) ()
    . \ nfx::helper (terms ...) (x -)
    ; if we reached here, then the next term isn't
    ; a multiplication/division.  Commit the
    ; pending add/subtract
    \
    . nfx::helper (y terms ...) (x +)
    . \ nfx::helper ((+ x y) terms ...) ()
    \
    . nfx::helper (y terms ...) (x -)
    . \ nfx::helper ((- x y) terms ...) ()

define-syntax nfx
  syntax-rules (+ - * /)
    \
    . nfx terms ...
    . \ nfx::helper (terms ...) ()

