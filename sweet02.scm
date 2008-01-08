; sweet.scm (Scheme), 2008-01-08
;
; Implements sweet-expression specification version 0.2.
; To use (in guile), type:
;   (use-modules (sweet))
; Your %load-path must be correctly set up; see the README file.
;
; NOTE: NOT READY FOR PRODUCTION USE; the "sugar" module doesn't work
; 100% correctly.
;
; Implements and enables "sweet-expressions", which are modern-expressions
; plus indentation (using I-expressions)

; Currently, it's just the command line; loading, etc. is to come.

(define-module (sweet02))

(use-modules (modern))
(enable-modern)

(use-modules (sugar))
; sugar auto-enables.


