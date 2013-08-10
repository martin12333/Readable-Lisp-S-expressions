; This is a trivial demo of sweet-expressions in Common Lisp.  Run as:
;     clisp -q -q factorial.lisp
; To use the NOT installed version of the readable library, prepend with:
;     CL_SOURCE_REGISTRY="$PWD" 
; where $PWD is wherever the files sweet.lisp (etc.) are installed.

(require "asdf")
(asdf:load-system :readable)
(readable:enable-sweet)
(setq *print-escape* nil) ; Optional



defun fact (x)
  if {x < 1}
    1
    {x * fact{x - 1}}

princ fact(5)



(readable:disable-readable)
