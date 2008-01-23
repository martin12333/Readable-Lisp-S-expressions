; iformat.scm  version as of 2008-01-11
; This Scheme program reformats an S-expression into an I-expression.
; An I-expression can represent the same information, but can use indentation
; instead of parentheses.
;
; Copyright (C) 2006-2008 David A. Wheeler.
;
; Released under the "MIT license":
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

; To run, just:
;  (load "iformat.scm")
;  (iformat '(your expression) "")

; LIMITATIONS:
; This version changes EVERY list into indentation form, which is overly
; wordy.  A future version should add same-line information, which
; shortens the results dramatically.
;
; A future version should optionally support term-ending like f(x) to mean
; (f x), and {..} for infix operations.  See "sweet-expression" information
; at http://www.dwheeler.com/readable/.
;
; For efficiency, it should use another approach than string concatenation.
; Rewrite this using cons of strings, then flatten the list
; to a single string in a final pass.

(define maxwidth 78)
(define indent-increment '(#\space #\space))

(define LISTLP (list #\())
(define LISTRP (list #\)))


(define (oneline-list x)
  ; Return list x's contents, represented as a list of characters
  (cond
    ((null? x) '())
    ((pair? x) (append (oneline (car x)) (oneline-list (cdr x))))
    (#t (append '(#\space #\. #\space) (oneline x)))))

(define (oneline x)
  ; Return x represented in a single line, as a list of characters.
  (cond
    ((null? x) (string->list "()"))
    ((number? x) (string->list (number->string x)))
    ((symbol? x) (string->list (symbol->string x)))
    ((string? x) (string->list x)) ; TODO handle double-quote, backslash, nl
    ((pair? x) (append LISTLP (oneline-list x) LISTRP))
    ; TODO: Others, esp. vector
    ))

(define maximum-params 1)
(define max-length 78)

(define (append-parameters indent current-line initial remaining-params count)
  ; Try adding parameters from remaining-params to current-line, given
  ; that its initial parameter was initial, and we've added 'count' params
  ; so far.  We presume that the first parameter of the original list is
  ; NOT a list.
  ; ???  Returns the list (resulting-char-list remaining-params)
  (cond
    ((null? remaining-params) (cons current-line remaining-params)) ; only here
    ((>= count maximum-params)
      (append current-line '(#\newline)
        (iformat-body remaining-params (append indent indent-increment))))
    (#t (let ((new (append current-line '(#\space)
                           (oneline (car remaining-params)))))
      (if (> (length new) max-length)
        (append current-line '(#\newline)
          (iformat-body remaining-params (append indent indent-increment)))
        (append-parameters indent new initial
                           (cdr remaining-params) (+ count 1)))))))

(define (iformat-body m indent)
  ; Display a list of items at the same indent level
  (if (null? m) '()
    (append
      (iformat-top (car m) indent)
      (iformat-body (cdr m) indent))))

(define (iformat-list m indent)
  ; Display a list.  (car list) is shown at indent.  Append on same line
  ; the next items, as you can, and then append on new lines the remaining
  ; items at indent + indent-increment.
    (iformat-top  (car m) indent)
    (iformat-body (cdr m) (append indent indent-increment)))

(define (iformat-top m indent)
  (cond
    ( (not (pair? m)) ; if not a list (atom, etc), print it. () handled here
      (append indent (oneline m) '(#\newline)))
    ;  TODO: handle non-atomic quotes.  There are questions about quote meaning
    ;  so let's be cautious for now.  If the test fails, we'll just print
    ;  using the literal symbol quote, and that's safe to do.
    ((and (eq? (car m) 'quote) (pair? (cdr m))  ; simple quote - one symbol
          (null? (cddr m)) (symbol? (cadr m)))
      (append indent '(#\') (oneline (cadr m)) '(#\newline)))
    ; TODO: match backquote calls, etc.
    ; At this point we have a list - is it special somehow?
    ; TODO: are null? and list? the right tests?
    ( (and (null? (cdr m)) (not (pair? (car m)))) ; singleton list, no descend
      (append indent LISTLP (oneline (car m)) LISTRP '(#\newline)))
    ; TODO: If car is list, can often avoid using group - try doing so.
    ( (pair? (car m)) ; Is its car also a list? If so, use GROUP.
     (append indent (string->list "group\n")
        (iformat-top  (car m) (append indent indent-increment))
        (iformat-body (cdr m) (append indent indent-increment))))
    (#t
      ; Normal case: list with non-list as 1st element.
      (append
        (iformat-top  (car m) indent)
        (iformat-body (cdr m) (append indent indent-increment))))))

(define (iformat m indent)
  (display "DEBUG: Formatting: ") (write m) (newline) (newline)
  (display (list->string (iformat-top m (string->list indent))))
  (newline)
  (newline))


(iformat '(z a (b c) (d) ((e f) g (h)) i 'j) "")

; NEED TO FINISH:
; (define iformat-stream (mystream)
;   (read mystream)
;   )
; 
; (define iformat-file (filename)
;   (with-open-file
;     (inputfile filename :direction :input :if-does-not-exist NIL???)
;     (when (stream? name)  ; This is a race condition, shouldn'#t matter.
;        (iformat-stream inputfile))))


; Example of use:
;  (iformat '(z a (b c) (d) ((e f) g (h)) i) " ")
; " Z
;    A
;    B
;      C
;    (D)
;    GROUP
;      E
;        F
;      G
;      (H)
;    I
; "

