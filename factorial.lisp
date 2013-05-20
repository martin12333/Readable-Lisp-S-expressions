; Trivial sweet-expression Common Lisp demo.  Run as:
;     clisp -q -q factorial.lisp

; To run the NOT installed readable version, prepend that with:
;     CL_SOURCE_REGISTRY="$PWD" 

(require "asdf")
(asdf:load-system :readable)
(readable:enable-sweet)

(setq *print-escape* nil)

defun fact (x)
  if {x < 1}
    1
    {x * fact{x - 1}}

princ fact(5)

(readable:disable-readable)
