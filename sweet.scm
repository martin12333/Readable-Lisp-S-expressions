; sweet.scm (Scheme), 2008-01-08
;
; Implements sweet-expression specification version 0.2.
; To use (in guile), type:
;   (use-modules (sweet))
; Your %load-path must be correctly set up; see the README file.
;
; NOTE: NOT READY FOR PRODUCTION USE; the "sugar" module (which implements
; indentation) doesn't work 100% correctly.  But it's enough to
; test it out; please do so!
;
; Implements and enables "sweet-expressions", which are modern-expressions
; plus indentation (using I-expressions).  So we'll chain two modules
; that implement them (respectively).

; This version auto-enables.

; A lot of this is guile-specific, because it uses guile's module system.

(define-module (sweet))

(use-modules (modern))
(enable-modern)

(use-modules (sugar))
; sugar auto-enables.

(define sweet-read sugar-read)

(define sweet-test-error 0)
(define sweet-test-correct 0)

(define (sweet-test filename)
  ; Load a test file, which should have pairs of these:
  ;    s-expr (correct answer)
  ;    sweet-expression (which should match it)
  (define (load port)
    (let ((correct (old-read port)))
      (cond
	((eof-object? correct)
            (display "TEST COMPLETE\n")
            (display "Correct answers:  ")
            (display sweet-test-correct)
            (display " test errors: ")
            (display sweet-test-error)
            (newline)
	    #t)
	(#t
	  ; "Sugar" has problems with blank lines; skip them for now
	  (if (eqv? #\newline (peek-char port)) (read-char port))
	  (if (eqv? #\newline (peek-char port)) (read-char port))
	  (if (eqv? #\newline (peek-char port)) (read-char port))
	  (let ((sweet-value (sweet-read port)))
            (cond
              ((equal? correct sweet-value)
                (set! sweet-test-correct (+ sweet-test-correct 1))
                (display "Correct for: ")
                (write correct)
                (newline))
              (#t
                (set! sweet-test-error (+ sweet-test-error 1))
                (display "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ERROR \n")
                (display " Should be:")
                (write correct)
                (display "\n Got: ")
                (write sweet-value)
                (newline))))
	  (load port)))))
  (load (open-input-file filename)))

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
(export sweet-test)


