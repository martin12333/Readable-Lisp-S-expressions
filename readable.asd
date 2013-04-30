;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; readable.asd - Common Lisp package for "readable" Lisp notations.
;;;; See http://readable.sourceforge.net for information on these notations:
;;;; - curly-infix: Add infix in {...}, so {a op b op c...} => (op a b c...)
;;;; - neoteric: Add suffix support, so f(...) => (f ...), f{...} => (f {...})
;;;; - sweet: Indentation implies parentheses

; For ASDF information, see:
; http://common-lisp.net/~mmommer/asdf-howto.shtml
; http://www.xach.com/lisp/asdf-tutorial/
; http://common-lisp.net/project/asdf/

(in-package #:cl-user)

(defpackage #:readable-asd
  #+clisp (:modern t)
  (:use :cl :asdf))

(defpackage #:readable
  #+clisp (:modern t)
  (:use :cl)
  (:export #:enable-basic-curly #:basic-curly-read
           #:enable-full-curly-infix #:enable-curly-infix
           #:curly-infix-read
           #:enable-neoteric #:neoteric-read
           #:enable-sweet #:sweet-read
           #:disable-readable
           #:$nfx$ #:$bracket-apply$))

(in-package #:readable-asd)

(defsystem readable
  :name "readable"
  :version "0.7.1"
  :maintainer "David A. Wheeler"
  :author "David A. Wheeler"
  :license "MIT"
  :description "Support 'readable' Lisp formats"
  :long-description "Common Lisp implementation of 'readable' Lisp formats - curly-infix-expressions, neoteric-expressions, and sweet-expressions, per http://readable.sourceforge.net."
  :serial t ;; the dependencies are linear.
  :components
    ((:file "basic-curly")
     (:file "neoteric")
     (:file "backquote") ; Re-implements backquote (needed by sweet)
     (:file "sweet")))

