; sweet.scm
; Implements sweet-expressions

; Copyright (C) 2006-2012 by David A. Wheeler
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
; To use (in guile), type:
;   (use-modules (sweet))
; Your %load-path must be correctly set up; see the README file.
;
; Implements and enables "sweet-expressions", which are modern-expressions
; plus indentation (using I-expressions).  So we'll chain two modules
; that implement them (respectively).

; This version auto-enables.

; A lot of this is guile-specific, because it uses guile's module system,
; but it can be easily changed for other Schemes

;----GUILE BEGINS
(define-module (sweet)
  :export (sweet-read sweet-read-save sweet-load))

(use-modules (modern))
(enable-modern)

(use-modules (sugar))

;----GUILE ENDS

(define sweet-read-save read)

; sugar auto-enables.

(define sweet-read sugar-read)

; Not needed here; see the file "sweet-filter":
; (define (sweet-filter)
;    (let ((result (sweet-read (current-input-port))))
; 	(if (eof-object? result)
; 	    result
;           (begin (write result) (newline) (sweet-filter)))))

(define sweet-load sugar-load)

; (define (enable-sweet)
;   (set! read sweet-read))

; (define (disable-sweet)
;   (set! read old-read))

; sweet-read-save allows external users to access "original" read

