; sugar.scm
; Provide "Indentation-sensitive syntax" for Scheme.
;
; Copyright (C) 2005-2012 by Egil Möller, David A. Wheeler,
;   and Alan Manuel K. Gloria.
;
; This software is released as open source software under the "MIT" license:
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
;
; This code implements indented expressions as a GNU Guile module
; that can be loaded using:
;     (use-modules (sugar))
; This command will only work if this file's directory is in %load-path;
; if the file is not installed anywhere, you can do this with:
;      (set! %load-path (append %load-path '(".")))
; You can also use this (esp. for testing):
;    export GUILE_LOAD_PATH="."
; but beware; the GUILE_LOAD_PATH has higher precedence, so while this
; is useful for testing, setting GUILE_LOAD_PATH to include "." can have
; security problems if used in "real" programs.
;
; Implementation based on SRFI 49 at:
; http://srfi.schemers.org/srfi-49/srfi-49.html
;
;  ----{ sugar.scm }----

;----PORTABILITY LAYER BEGINS
(define (my-eval inp)
  ; Scheme R5RS eval *requires* 2 arguments; other evals work differently.
  (eval inp (interaction-environment)))
;----PORTABILITY LAYER ENDS

;----GUILE BEGINS
(define-module (sugar)
  :export (sugar-read-save sugar-load-save
           sugar-read sugar-filter
           sugar-load
           sugar-enable sugar-disable
          ))

; these only exist in guile
(define sugar-load-save primitive-load)
(define sugar-current-load-port-save current-load-port)
;----GUILE ENDS


(define split (string->symbol "\\\\"))
(define split-char #\\ ) ; First (possibly only) character of split symbol.
; this is a special unique object that is used to
; represent the existence of the split symbol
; so that readblock-clean handles it properly.
(define split-tag (cons '() '()))

; #\ht is not as portable, so predefine "tab" as that char
(define tab (integer->char 9))

; We want to be able to read files that use CRLF (#\return #\newline)
; line endings, and ideally on systems that use just CR or just LF.
; Typically #\newline is defined as linefeed = (integer->char 10),
; though the code should work if it is 13 instead.
; Since #\return is not portable, predefine an equivalent.
(define carriage-return (integer->char 13))

; Return #t if char is space or tab.
(define (char-horiz-whitespace? char)
  (or (eqv? char #\space)
      (eqv? char tab)))

(define sugar-read-save read)

(define (consume-to-eol port)
  ; Consumes chars to end of line, WITHOUT consume the ending newline/EOF
  (let ((c (peek-char port)))
    (cond
      ((eof-object? c) c)
      ((char=? c #\newline) c)
      ((char=? c carriage-return) c)
      (#t (read-char port) (consume-to-eol port)))))

(define (readquote level port qt)
  (let ((char (peek-char port)))
    (if (char-whitespace? char)
        (list qt)
        (list qt (sugar-read-save port)))))

(define (readitem level port)
  (let ((char (peek-char port)))
    (cond
     ((eqv? char #\`)
      (read-char port)
        (readquote level port 'quasiquote))
     ((eqv? char #\')
      (read-char port)
        (readquote level port 'quote))
     ((eqv? char #\,)
      (read-char port)
      (cond
        ((eqv? (peek-char port) #\@)
          (read-char port)
          (readquote level port 'unquote-splicing))
        (#t
          (readquote level port 'unquote))))
     (#t
        (sugar-read-save port)))))

(define (indentation>? indentation1 indentation2)
  (let ((len1 (string-length indentation1))
          (len2 (string-length indentation2)))
    (and (> len1 len2)
           (string=? indentation2 (substring indentation1 0 len2)))))

(define (accumulate-hspace port)
  (if (char-horiz-whitespace? (peek-char port))
      (cons (read-char port) (accumulate-hspace port))
      (if (not (eqv? (peek-char port) #\.))
          '()
          (let ((c (read-char port)))
            (if (char-horiz-whitespace? (peek-char port))
              (cons #\. (accumulate-hspace port)) ; period-as-indent
              (begin (unread-char c port) '()))))))

(define (indentationlevel port)
  (let ((indent (accumulate-hspace port)))
    (cond
      ((eqv? (peek-char port) #\;)
        (consume-to-eol port) ; ALWAYS ignore comment-only lines.
        (if (eqv? (peek-char port) carriage-return) (read-char port))
        (if (eqv? (peek-char port) #\newline) (read-char port))
        (indentationlevel port))
      ; If ONLY whitespace on line, treat as "", because there's no way
      ; to (visually) tell the difference (preventing hard-to-find errors):
      ((eof-object? (peek-char port)) "")
      ((eqv? (peek-char port) #\newline) "")
      ((eqv? (peek-char port) carriage-return)
         (read-char port) "")
      (#t (list->string indent)))))

;; Reads all subblocks of a block
;; this essentially implements the "body" production
;; - return value:
;;   cons
;;     next-level ;
;;     (xs ...) ; the body
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
;; this essentially implements the "head" production
;; - return value:
;;   cons
;;     next-level ; the indentation of the line that ends this block
;;     expr ;       the read-in expression
(define (readblock level port)
  (readblock-internal level port #t))
(define (readblock-internal level port first-item?)
  (let ((char (peek-char port)))
    (cond
     ((eof-object? char)
        (cons -1 char))
     ((eqv? char #\;)
        (consume-to-eol port)
        (readblock level port))
     ((or (eqv? char #\newline)
          (eqv? char carriage-return))
        (read-char port)
        (if (and (eqv? char carriage-return)
                (eqv? (peek-char port) #\newline))
          (read-char port))
        (let ((next-level (indentationlevel port)))
          (if (indentation>? next-level level)
              (readblocks next-level port)
              (cons next-level '()))))
     ((char-horiz-whitespace? char)
        (read-char port)
        (readblock-internal level port first-item?))
     (#t
        (let ((first (readitem level port)))
          (cond
            ((and first-item?
                  (or (equal? first '(quote))
                      (equal? first '(quasiquote))
                      (equal? first '(unquote))
                      (equal? first '(unquote-splicing))))
              (consume-horizontal-whitespace port)
              (let* ((sub-read (readblock-clean level port))
                     (outlevel (car sub-read))
                     (sub-expr (cdr sub-read)))
                (cons outlevel `(,@first ,sub-expr))))
            ((and (eq? char split-char) (eq? first split))
              ; consume horizontal, non indent whitespace
              (consume-horizontal-whitespace port)
              (if first-item?
                  ;; NB: need a couple of hacks to fix
                  ;; behavior when SPLIT-by-itself
                  (if (or (eqv? (peek-char port) #\newline)
                          (eqv? (peek-char port) carriage-return))
                      ; check SPLIT-by-itself
                      ; SPLIT-by-itself: some hacks needed
                      (let* ((sub-read (readblock level port))
                             (outlevel (car sub-read))
                             (sub-expr (cdr sub-read)))
                        (if (and (null? sub-expr) (string=? outlevel level)) ; check SPLIT followed by same indent line
                            ; blank SPLIT:
                            ; \
                            ; \
                            ; x
                            ; ===> x, not () () x
                            (readblock level port)
                            ; non-blank SPLIT: insert our
                            ; split-tag.  Without SPLIT-tag
                            ; readblock-clean will mishandle:
                            ; \
                            ;   x y
                            ; ==> ((x y)), which is a single
                            ; item list.  Single-item lists
                            ; are extracted, resulting in
                            ; (x y)
                            (cons outlevel (cons split-tag sub-expr))))
                      ; not SPLIT-by-itself: just ignore it
                      (readblock-internal level port first-item?))
                  ; SPLIT-inline: end this block
                  (cons level '())))
            (#t
              (let* ((rest (readblock-internal level port #f))
                     (level (car rest))
                     (block (cdr rest)))
                ;; this check converts:
                ;;  . foo
                ;; ->
                ;;  (. foo)
                ;; ->
                ;;  foo
                ;; HOWEVER, it might not be compatible
                ;; 100% with the "." as indentation
                ;; whitespace thing.
                (if (eq? first '.)
                    (if (pair? block)
                        (cons level (car block))
                        rest)
                    (cons level (cons first block)))))))))))

;; Consumes as much horizontal, non-indent whitespace as
;; possible.  Treat comments as horizontal whitespace too.
(define (consume-horizontal-whitespace port)
  (let ((char (peek-char port)))
    (cond
      ((char-horiz-whitespace? char)
         (read-char port)
         (consume-horizontal-whitespace port))
      ((eqv? char #\;)
         (consume-to-eol port)))))

;; reads a block and handles (quote), (unquote),
;; (unquote-splicing) and (quasiquote).
(define (readblock-clean level port)
  (let* ((read (readblock level port))
         (next-level (car read))
         (block (cdr read)))
    (cond
      ; remove split-tag
      ((and (pair? block) (eq? (car block) split-tag))
        (cons next-level (cdr block)))
      ; non-list and multi-item blocks.
      ((or (not (list? block)) (> (length block) 1))
        (cons next-level block))
      ; unwrap single-item blocks
      ((= (length block) 1)
        (if (eq? (car block) split-tag)
            ; "magically" remove split-tag
            (cons next-level '())
            (cons next-level (car block))))
      (#t
        (cons next-level '.)))))


(define (sugar-start-expr port)
  ; Read single complete I-expression.
  (let* ((indentation (list->string (accumulate-hspace port)))
         (c (peek-char port)))
    (cond
      ((eof-object? c) c) ; EOF - return it, we're done.
      ((eqv? c #\; )    ; comment - consume and see what's after it.
        (let ((d (consume-to-eol port)))
          (cond
            ((eof-object? d) d) ; If EOF after comment, return it.
            (#t
              (read-char port) ; Newline after comment.  Consume NL
              (sugar-start-expr port))))) ; and try again
      ((eqv? c #\newline)
        (read-char port) ; Newline (with no preceding comment).
        (sugar-start-expr port)) ; Consume and again
      ((eqv? c carriage-return)
        (read-char port) ; Consume carriage return.
        (if (eqv? (peek-char port) #\newline)
            (read-char port)) ; Consume newline if it follows CR.
        (sugar-start-expr port))
      ((> (string-length indentation) 0) ; initial indentation disables
        (sugar-read-save port))
      (#t
        (let* ((read (readblock-clean "" port))
               (level (car read))
               (block (cdr read)))
          (cond
           ((eq? block '.)
              '())
           (#t
              block)))))))


(define (sugar-read . port)
  (if (null? port)
    (sugar-start-expr (current-input-port))
    (sugar-start-expr (car port))))


(define (sugar-filter)
   (let ((result (sugar-read (current-input-port))))
        (if (eof-object? result)
            result
          (begin (write result) (newline) (sugar-filter)))))

(define (sugar-load filename)
  (define (load port)
    (let ((inp (sugar-read port)))
        (if (eof-object? inp)
            #t
            (begin
              (my-eval inp)
              (load port)))))
  (load (open-input-file filename)))

;----GUILE BEGINS
(define %sugar-current-load-port #f)
;; This code is specifically written to imitate
;; Guile's load.c's primitive-load implementation.
;; Starting with Guile 1.8, there is a new
;; current-reader fluid that can be set to
;; any arbitrary reader.  However, we wish to retain
;; compatibility up to Guile 1.6.
(define (sugar-primitive-load filename)
  (let ((hook (cond
                ((not %load-hook)
                  #f)
                ((not (procedure? %load-hook))
                  (error "value of %load-hook is neither procedure nor #f"))
                (#t
                  %load-hook))))
    (cond
      (hook
        (hook filename)))
    (let* ((port      (open-input-file filename))
           (save-port port))
      (define (load-loop)
        (let ((form (sugar-read port)))
          (cond
            ((not (eof-object? form))
              ; in Guile only
              (primitive-eval form)
              (load-loop)))))
      (define (swap-ports)
        (let ((tmp %sugar-current-load-port))
          (set! %sugar-current-load-port save-port)
          (set! save-port tmp)))
      (dynamic-wind swap-ports load-loop swap-ports)
      (close-input-port port))))
(define (sugar-current-load-port)
  %sugar-current-load-port)

(define (sugar-enable)
  (set! read sugar-read)
  (set! primitive-load sugar-load)
  (set! current-load-port sugar-current-load-port))

(define (sugar-disable)
  (set! read sugar-read-save)
  (set! primitive-load sugar-load-save)
  (set! current-load-port sugar-current-load-port-save))

(sugar-enable)
;----GUILE ENDS

; ----{ sugar.scm }----
