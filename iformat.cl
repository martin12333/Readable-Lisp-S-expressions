; iformat.cl  version as of 2007-12-29
; This Common Lisp program reformats an S-expression into an I-expression.
; An I-expression can represent the same information, but can use indentation
; instead of parentheses.
;
; Copyright (C) 2006-2007 David A. Wheeler.
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
;  (load "iformat.cl")
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
; Unfortunately, the Common Lisp standard doesn't have a good way of
; handling upper/lower case. By default, it'll convert to uppercase,
; and all the various Common Lisp options will screw it up in different ways.
; The best way to run this program is by using non-standard options, e.g.,
; clisp's "-modern" flag, so that the results look nice.
;
; For efficiency, it should use another approach than string concatenation.
; Rewrite this using cons of strings, then flatten the list
; to a single string in a final pass.

(defun iformat-body (m indent)
  (if (null m) ""
      (concatenate 'string
         (iformat (car m) indent)
         (iformat-body (cdr m) indent))))

(defun iformat-list (m indent)
  (if (null m) ""
    (concatenate 'string
      (iformat (car m) indent) (iformat-list (cdr m) indent))))

(defun iformat (m indent)
  (cond
    ( (not (consp m)) ; if not a list (atom, etc), print it. () handled here
      (format nil "~A~S~%" indent m))
    ;( (and (equalp (car m) 'quote)  ; Do we have a (quote X)?
    ;       (listp (cdr m))
    ;       (null (cddr m)))
    ;  TODO: be more careful, match only true QUOTE calls
    ;  TODO: handle non-atomic quotes.
    ( (equalp (car m) 'QUOTE)  ; Do we have a (quote X)? SHOULD BE MORE CAREFUL
      (format nil "~A'~S~%" indent (cadr m))) ; Print it specially.
    ; TODO: match backquote calls.
    ; At this point we have a list - is it special somehow?
    ; TODO: are null and listp the right tests?
    ( (and (null (cdr m)) (not (listp (car m)))) ; singleton list, no descend
      (format nil "~A~S~%" indent m)) ; surround with extra ().
    ( (listp (car m)) ; Is its car also a list? If so, must use GROUP.
      (concatenate 'string
         (format nil "~AGROUP~%" indent)
         (iformat-list m (concatenate 'string indent "  "))))
    (t
      (concatenate 'string  ; Normal case: list with non-list as 1st element.
         (format nil "~A~S~%" indent (car m))
         (iformat-body (cdr m) (concatenate 'string indent "  "))))))

; NEED TO FINISH:
; (defun iformat-stream (mystream)
;   (read mystream)
;   )
; 
; (defun iformat-file (filename)
;   (with-open-file
;     (inputfile filename :direction :input :if-does-not-exist nil)
;     (when (streamp name)  ; This is a race condition, shouldn't matter.
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

