; readable.asd - Common Lisp package definition.

; See:
; http://common-lisp.net/~mmommer/asdf-howto.shtml
; http://www.xach.com/lisp/asdf-tutorial/
; http://common-lisp.net/project/asdf/

(defpackage #:readable-asd
  (:use :cl :asdf))

(in-package :readable-asd)

(defsystem readable
  :name "readable"
  :version "0.7.1"
  :maintainer "David A. Wheeler"
  :author "David A. Wheeler"
  :license "MIT"
  :description "Support 'readable' Lisp formats"
  :long-description "Common Lisp implementation of 'readable' Lisp formats - curly-infix-expressions, neoteric-expressions, and sweet-expressions, per http://readable.sourceforge.net."
  :serial t ;; the dependencies are linear.
  :components ((:file "basic-curly")))

