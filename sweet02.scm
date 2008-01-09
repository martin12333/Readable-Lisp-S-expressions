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
; plus indentation (using I-expressions).  So we'll chain two modules
; that implement them (respectively).

; This version auto-enables.

; A lot of this is guile-specific, because it uses guile's module system.

(define-module (sweet02))

(use-modules (modern))
(enable-modern)

(use-modules (sugar))
; sugar auto-enables.

(define sweet-read sugar-read)

(define (sweet-filter)
   (let ((result (sweet-read (current-input-port))))
	(if (eof-object? result)
	    result
          (begin (write result) (newline) (sweet-filter)))))

(define (sweet-load filename)
  (define (load port)
    (let ((inp (sweet-read port)))
	(if (eof-object? inp)
	    #t
	    (begin
	      (eval inp)
	      (load port)))))
  (load (open-input-file filename)))

; (define (enable-sweet)
;   (set! read sweet-read))

; (define (disable-sweet)
;   (set! read old-read))

(export sweet-read)
(export sweet-load)
(export sweet-filter)

