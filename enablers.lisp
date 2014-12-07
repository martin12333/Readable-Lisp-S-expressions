;;; sweet.lisp
;;; Implements sweet-expressions from the "readable" approach for Lisp.

;;; Copyright (C) 2007-2014 by David A. Wheeler
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



(cl:in-package :readable)

; These macros enable various notations.
; These are macros so we can force compile-time modification
; of the readtable (using eval-when).
; The macros invoke functions with name + "-real" which do the real work.

(defmacro enable-basic-curly ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-basic-curly-real)))

(defmacro enable-full-curly-infix ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-full-curly-infix-real)))

; Synonym.
(defmacro enable-curly-infix ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-full-curly-infix-real)))

(defmacro enable-neoteric ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-neoteric-real)))

(defmacro enable-sweet ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-sweet-real)))

