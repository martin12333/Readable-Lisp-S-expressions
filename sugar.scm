; Implementation of SRFI 49, directly from SRFI 49 at:
; http://srfi.schemers.org/srfi-49/srfi-49.html
; This provides "Indentation-sensitive syntax" for Scheme.
; This SRFI descibes a new syntax for Scheme, called I-expressions,
; with equal descriptive power as S-expressions. The syntax uses
; indentation to group expressions, and has no special cases for
;  semantic constructs of the language. It can be used both for
;  program and data input.

; The following code implements I-expressions as a GNU Guile module
; that can be loaded with (use-modules (sugar)).

; CHANGES:
; * 2007-10-15 David A. Wheeler <dwheeler at dwheeler dot com>
;   - Changed "t" to "#t" (t is Common Lisp, #t is Scheme)
;   - Added sugar-filter.
; * 2006-06-10 David A. Wheeler <dwheeler at dwheeler dot com>
;   - Changed eq? to eqv? when comparing characters.
;     The "eq?" operator is not guaranteed to work
;     for comparing characters, or in comparing with end-of-file,
;     in the R5RS specification.
;   - sugar-load now calls sugar-read, not read;
;     that way, even if sugar has not been enabled, sugar-load will
;     correctly use sugar-read to read the contents.
;  ----{ sugar.scm }----

  (define-module (sugar))

  (define-public group 'group)

  (define-public sugar-read-save read)
  (define-public sugar-load-save primitive-load)

  (define (readquote level port qt)
    (read-char port)
    (let ((char (peek-char port)))
      (if (or (eqv? char #\space)
	      (eqv? char #\newline)
	      (eqv? char #\ht))
	  (list qt)
	  (list qt (sugar-read-save port)))))

  (define (readitem level port)
    (let ((char (peek-char port)))
      (cond
       ((eqv? char #\`)
	(readquote level port 'quasiquote))
       ((eqv? char #\')
	(readquote level port 'quote))
       ((eqv? char #\,)
	(readquote level port 'unquote))
       (#t
	(sugar-read-save port)))))

  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
	  (len2 (string-length indentation2)))
      (and (> len1 len2)
	   (string=? indentation2 (substring indentation1 0 len2)))))

  (define (indentationlevel port)
    (define (indentationlevel)
      (if (or (eqv? (peek-char port) #\space)
	      (eqv? (peek-char port) #\ht))
	  (cons 
	   (read-char port)
	   (indentationlevel))
	  '()))
    (list->string (indentationlevel)))

  (define (clean line)
    (cond
     ((not (pair? line))
      line)
     ((null? line)
      line)
     ((eq? (car line) 'group)
      (cdr line))
     ((null? (car line))
      (cdr line))
     ((list? (car line))
      (if (or (equal? (car line) '(quote))
	      (equal? (car line) '(quasiquote))
	      (equal? (car line) '(unquote)))
	  (if (and (list? (cdr line))
		   (= (length (cdr line)) 1))
	      (cons
	       (car (car line))
	       (cdr line))
	      (list
	       (car (car line))
	       (cdr line)))
	  (cons
	   (clean (car line))
	   (cdr line))))
     (#t
      line)))

  ;; Reads all subblocks of a block
  (define (readblocks level port)
    (let* ((read (readblock-clean level port))
	   (next-level (car read))
	   (block (cdr read)))
      (if (string=? next-level level)
	  (let* ((reads (readblocks level port))
		 (next-next-level (car reads))
		 (next-blocks (cdr reads)))
	    (if (eq? block '.)
		(if (pair? next-blocks)
		    (cons next-next-level (car next-blocks))
		    (cons next-next-level next-blocks))
		(cons next-next-level (cons block next-blocks))))
	  (cons next-level (list block)))))

  ;; Read one block of input
  (define (readblock level port)
    (let ((char (peek-char port)))
      (cond
       ((eof-object? char)
	(cons -1 char))
       ((eqv? char #\newline)
	(read-char port)
	(let ((next-level (indentationlevel port)))
	  (if (indentation>? next-level level)
	      (readblocks next-level port)
	      (cons next-level '()))))
       ((or (eqv? char #\space)
	    (eq? char #\ht))
	(read-char port)
	(readblock level port))
       (#t
	(let* ((first (readitem level port))
	       (rest (readblock level port))
	       (level (car rest))
	       (block (cdr rest)))
	  (if (eq? first '.)
	      (if (pair? block)
		  (cons level (car block))
		  rest)
	      (cons level (cons first block))))))))

  ;; reads a block and handles group, (quote), (unquote) and
  ;; (quasiquote).
  (define (readblock-clean level port)
    (let* ((read (readblock level port))
	   (next-level (car read))
	   (block (cdr read)))
      (if (or (not (list? block)) (> (length block) 1))
	  (cons next-level (clean block))
	  (if (= (length block) 1)
	      (cons next-level (car block))
	      (cons next-level '.)))))

  (define-public (sugar-read . port)
    (let* ((read (readblock-clean "" (if (null? port)
					(current-input-port)
					(car port))))
	   (level (car read))
	   (block (cdr read)))
      (cond
       ((eq? block '.)
	'())
       (#t
	block))))


  (define-public (sugar-filter)
     (let ((result (sugar-read (current-input-port))))
	(if (eof-object? result)
	    result
            (begin (display result) (newline) (sugar-filter)))))

  (define-public (sugar-load filename)
    (define (load port)
      (let ((inp (sugar-read port)))
	(if (eof-object? inp)
	    #t
	    (begin
	      (eval inp)
	      (load port)))))
    (load (open-input-file filename)))

  (define-public (sugar-enable)
    (set! read sugar-read)
    (set! primitive-load sugar-load))

  (define-public (sugar-disable)
    (set! read sugar-read-save)
    (set! primitive-load sugar-load-save))

  (sugar-enable)

; ----{ sugar.scm }----
; Copyright (C) 2005 by Egil Möller . All Rights Reserved.
; 
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the "Software"),
; to deal in the Software without restriction, including without limitation
; the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.

