; sweet.lisp
; Implements sweet-expressions from the "readable" approach for Lisp.

; WARNING: THIS IS NOT READY FOR PRODUCTION USE YET.
; For Common Lisp, use the "basic-curly-infix" mode (which is more mature),
; or the Scheme implementation (which has a Common Lisp mode)
; at this time.  In the meantime, we'd love help finishing this!

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



(cl:in-package :readable)

; We can't portably overide "read" directly, we have to manipulate
; the readtable.  Here we redirect EVERY character to a procedure,
; effectively taking over "read".
(defvar *sweet-redirect-readtable*
  "This table redirects any input to sweet-expression processing")

(defun t_expr_entry (stream char)
  (unread-char char stream)
  ; (setq *readtable* *sweet-redirect-readtable*)
  (setq *readtable* *original-readtable*)
  (princ "DEBUG: t_expr_entry received: ") (write char) (terpri)
)

(defun compute-sweet-redirect-readtable ()
  (setq *sweet-redirect-readtable*
    (let ((new (copy-readtable nil)))
      (set-syntax-from-char #\# #\' new) ; force # to not be dispatching char.
      (loop for ci from 0 upto 255 ; TODO: char-code-limit
         do (set-macro-character (character ci) #'t_expr_entry nil new))
      new)))

(defun enable-sweet ()
  (setq *original-readtable* (copy-readtable))
  ; Invoke curly-brace-infix-reader when opening curly brace is read in:
  ; (set-macro-character #\{ #'full-curly-brace-infix-reader) ; (
  ; This is necessary, else a cuddled closing brace will be part of an atom:
  ; (set-macro-character #\} (get-macro-character #\) nil))
  (compute-sweet-redirect-readtable)
  (setq *readtable* *sweet-redirect-readtable*)
  t) ; Meaning "Did it"

