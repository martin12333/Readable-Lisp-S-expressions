;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; readable.asd - Common Lisp package for "readable" Lisp notations.
;;;; See http://readable.sourceforge.net for information on these notations:
;;;; - curly-infix: Add infix in {...}, so {a op b op c...} => (op a b c...),
;;;;   {a b} => (a b), and {a} => a.  No precedence, by intent.
;;;;   In "full" curly-infix, datums inside {} are *neoteric* expressions.
;;;; - neoteric: Add suffix support, so f(...) => (f ...), f{...} => (f {...})
;;;; - sweet: Indentation implies parentheses.  Solo datums with no children
;;;;   represent themselves; otherwise the datums are wrapped into a list:
;;;;     define fibfast(n)   ; Typical function notation
;;;;       if {n < 2}        ; Indentation, infix {...}
;;;;          n              ; Single expr = no new list
;;;;          fibup n 2 1 0  ; Simple function calls
;;;;   ==>
;;;;     (define (fibfast n)
;;;;       (if (< n 2)
;;;;           n
;;;;           (fibup n 2 1 0)))


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
           #:enable-full-curly-infix #:enable-curly-infix #:curly-infix-read
           #:enable-neoteric #:neoteric-read
           #:enable-sweet #:sweet-read
           #:my-char-code-limit
           #:disable-readable
           #:readable-parse-error #:*noisy*
           #:$nfx$ #:$bracket-apply$))

(in-package #:readable-asd)

(defsystem readable
  :name "readable"
  :version "0.9.4"  ; ONLY digits and periods allowed.
  :maintainer "David A. Wheeler"
  :author "David A. Wheeler"
  :license "MIT"
  :description "Support 'readable' extensions to Lisp s-expressions"
  :long-description "Common Lisp implementation of 'readable' extensions to Lisp s-expresions - curly-infix-expressions, neoteric-expressions, and sweet-expressions, per http://readable.sourceforge.net.  This can be useful if you want your Lisp code to be easier to read."
  ; :serial t ;; the dependencies are (no longer) linear.
  :components
    ((:file "basic-curly")
     (:file "neoteric" :depends-on ("basic-curly"))
     (:file "backquote") ; Re-implements backquote, as needed by sweet
     (:file "sweet" :depends-on ("basic-curly" "neoteric" "backquote"))))

