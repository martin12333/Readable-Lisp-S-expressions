; Trivial sweet-expression Common Lisp demo.  Run as:
;        clisp -q -q factorial.lisp

(require "asdf")
(asdf:load-system :readable)
(readable:enable-sweet)

(setq *print-escape* nil)

defun fact (x)
  if {x < 1}
    1
    {x * fact{x - 1}}

princ fact(5)

