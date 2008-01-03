; modern.cl (Common Lisp), 2008-01-03
;
; NOTE: NOT READY FOR PRODUCTION USE.
;
; Implements "modern Lisp notation".  E.G., f(x) => (f x),
; {3 + 4 + 5} => (+ 3 4 5), f{x + 3} => (f (+ x 3),
; x[z] => (bracketaccess x z).
;
; Note: This causes a minor change in the semantics of Common Lisp "read".
; "read" will now act like "read-preserving-whitespace"
; (and like Scheme's read) - it will NOT consume whitespace AFTER the
; expression read.  This is needed so that an indentation processor
; can be built on top of it.  It also makes more sense, frankly - why
; read in things when you don't need them?
;
; Copyright (C) 2008 by David A. Wheeler.
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


; Configuration:
(setf *backwards-compatible* t) ; If true, "(" triggers old reader.

; Preserve original read.
; Actually, this isn't needed in Common Lisp; it's more of a note,
; since other Lisps will need to replace "read" instead.
; Use read-preserving-whitespace, not "read", so that we can stack
; indentation processing on top of this.
; Note that #' is needed in CL.
(setf old-read #'read-preserving-whitespace)
(setf old-readtable *readtable*)
(setf modern-readtable (copy-readtable)) ; Create new readtable.
; (setf old-read-not-unpreserve #'read)
; (setf old-paren (get-macro-character #\( )) ;)

; Utility functions to implement the simple infix system:

; Return true if lyst has an even # of parameters, and the (alternating) first
; ones are "op".  Used to determine if a longer lyst is infix.
; Otherwise it returns NIL (False).
; If passed empty list, returns true (so recursion works correctly).
(defun even-and-op-prefix (op lyst)
   (cond
     ((null lyst) t)
     ((not (consp lyst)) nil) ; Not a list.
     ((not (eq op (car lyst))) nil) ; fail - operators not all equal.
     ((null (cdr lyst)) nil) ; fail - odd # of parameters in lyst.
     (t (even-and-op-prefix op (cddr lyst))))) ; recurse.

; Return True if the lyst is in simple infix format (and should be converted
; at read time).  Else returns NIL.
(defun simple-infix-listp (lyst)
  (and
    (consp lyst)           ; Must have list;  '() doesn't count.
    (consp (cdr lyst))     ; Must have a second argument.
    (consp (cddr lyst))    ; Must have a third argument (we check it
                           ; this way for performance)
    (symbolp (cadr lyst))  ; 2nd parameter must be a symbol.
    (even-and-op-prefix (cadr lyst) (cdr lyst)))) ; even parameters equal?

; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
(defun alternating-parameters (lyst)
  (if (or (null lyst) (null (cdr lyst)))
    lyst
    (cons (car lyst) (alternating-parameters (cddr lyst)))))

; Transform a simple infix list - move the 2nd parameter into first position,
; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
(defun transform-simple-infix (lyst)
   (cons (cadr lyst) (alternating-parameters lyst)))

(defun process-curly (lyst)
  (if (simple-infix-listp lyst)
     (transform-simple-infix lyst) ; Simple infix expression.
     (cons 'nfx lyst))) ; Non-simple; prepend "nfx" to the list.


(defun my-read-delimited-list (stop-char
           &optional (input-stream *standard-input*)
           eof-error-p eof-value recursive-p)
  ; like read-delimited-list, but call modern-read instead.
  ; Also, has ALL the read parameters.
  ; TODO: Handle (x . b)
  ; TODO: Handle Error on wrong stop-char.
  ; TODO: Handle EOF in middle.
  (let
    ((c (peek-char nil input-stream eof-error-p eof-value recursive-p)))
    (print "DEBUG my-read-delimited-list:") (write c)
    (cond
      ((char= c stop-char)
        (read-char input-stream eof-error-p eof-value recursive-p)
        '())
      ((consp (member c
                   '(#\Space #\Tab #\Newline #\Return (code-char 9)    ; Tab
                     (code-char 10) (code-char 11)     ; LF, VT
                     (code-char 12) (code-char 13))))  ; FF, CR
        (read-char input-stream eof-error-p eof-value recursive-p)
        (my-read-delimited-list stop-char input-stream
                                eof-error-p eof-value recursive-p))
      (t
        (cons
         (modern-read input-stream eof-error-p eof-value recursive-p)
         (my-read-delimited-list stop-char input-stream t nil t))))))

(defun call-old-paren (&optional (input-stream *standard-input*)
                               eof-error-p eof-value recursive-p)
  ; Call old paren processor.  Current char is the paren (unconsumed).
  ; We could do this, _if_ we could change read:
  ;  (funcall old-read input-stream
  ;         eof-error-p eof-value recursive-p) ;(
  ; but since there are problems doing this, we'll call the
  ; old macro reader instead:
  ;  (read-char input-stream eof-error-p eof-value recursive-p)
  ;  (funcall old-paren input-stream c))
  (setf *readtable* old-readtable)
  (let ((result
          (funcall old-read input-stream eof-error-p eof-value recursive-p)))
    (setf *readtable* modern-readtable)
    result))

(defun skip-whitespace (&optional (input-stream *standard-input*)
                        eof-error-p eof-value recursive-p)
  ; Consume whitespace.  Double-define to make SURE we get them all.
  (cond
    ( (consp (member (peek-char nil input-stream
                        eof-error-p eof-value recursive-p)
                   '(#\Space #\Tab #\Newline #\Return (code-char 9)    ; Tab
                     (code-char 10) (code-char 11)     ; LF, VT
                     (code-char 12) (code-char 13))))  ; FF, CR
      (read-char input-stream eof-error-p eof-value recursive-p)
      (skip-whitespace input-stream eof-error-p eof-value recursive-p))))

(defun modern-process-tail (prefix &optional (input-stream *standard-input*)
                               eof-error-p eof-value recursive-p)
  ; See if we've just finished reading a prefix, and if so, process.
  ; This recurses, to handle formats like f(x)(y).
  ; This implements prefixed (), [], and {}
  ; (princ "Got to tail, prefix is:")
  ; (write prefix)
  ; (princ "peek char is:")
  ; (write (peek-char nil input-stream eof-error-p eof-value recursive-p))
  ; (princ "Starting...")
  (if (not (or (symbolp prefix) (consp prefix)))
    prefix  ; Prefixes MUST be symbol or cons; return original value.
    (let ((c (peek-char nil input-stream
                        eof-error-p eof-value recursive-p)))
      (cond
        ((char= c #\( ) ; Implement f(x)
          (read-char input-stream eof-error-p eof-value recursive-p)
          (modern-process-tail
            (cons prefix (my-read-delimited-list #\)
                            input-stream eof-error-p eof-value t))
            input-stream eof-error-p eof-value recursive-p))
        ((char= c #\[ )
          (read-char input-stream eof-error-p eof-value recursive-p)
          (modern-process-tail
            (cons 'bracketaccess (cons prefix
              (my-read-delimited-list #\]
                    input-stream eof-error-p eof-value t)))
            input-stream eof-error-p eof-value recursive-p))
        ((char= c #\{ )
          (read-char input-stream eof-error-p eof-value recursive-p)
          (modern-process-tail
            (list prefix
              (process-curly
                (my-read-delimited-list #\}
                    input-stream eof-error-p eof-value t)))
            input-stream eof-error-p eof-value recursive-p))
        (t prefix)))))

(defun modern-read (&optional (input-stream *standard-input*)
                               eof-error-p eof-value recursive-p)
  ; Read using "modern Lisp notation".
  ; This implements unprefixed (), [], and {}
  (skip-whitespace input-stream eof-error-p eof-value recursive-p)
  ; TODO: Add (tail ...)
  (modern-process-tail
    (let ((c (peek-char nil input-stream eof-error-p eof-value recursive-p)))
      (cond
        ; NOTE: EOF. In some dialects, must check for that directly.
        ; TODO: Directly implement abbreviations like ', so NOT overriding
        ; "read" will still work reasonably well.
        ((char= c #\')
          (read-char input-stream eof-error-p eof-value recursive-p)
          (list 'quote
            (modern-read input-stream eof-error-p eof-value recursive-p)))
        ((char= c #\( ) ; )
          (if *backwards-compatible*
            ; We could do this, _if_ we could change read:
            ;  (funcall old-read input-stream
            ;         eof-error-p eof-value recursive-p) ;(
            ; Calling the old reader macro doesn't work because its "read"
            ; will consume following whitespace (eek!).
            ; (progn
            ;  (read-char input-stream eof-error-p eof-value recursive-p)
            ;  (funcall old-paren input-stream c))
            (call-old-paren input-stream eof-error-p eof-value recursive-p)
            (progn
              (read-char input-stream eof-error-p eof-value recursive-p)
              (my-read-delimited-list #\) 
                    input-stream eof-error-p eof-value t))))
        ((char= c #\[ )
            (read-char input-stream eof-error-p eof-value recursive-p)
            (my-read-delimited-list #\] 
                    input-stream eof-error-p eof-value t))
        ((char= c #\{ )
          (read-char input-stream eof-error-p eof-value recursive-p)
          (process-curly
            (my-read-delimited-list #\}
                    input-stream eof-error-p eof-value t)))
        (t (funcall old-read input-stream
                    eof-error-p eof-value recursive-p))))
    input-stream eof-error-p eof-value recursive-p))


; The following install the {...} reader.
; See "Common Lisp: The Language" by Guy L. Steele, 2nd edition,
; pp. 542-548 and pp. 571-572.


; Read until }, then process list as infix list.
; If it's a simple infix list (odd # parameters, 3+ parameters, all even
; parameters are equal symbols) then transform to infix. E.G.,
;   {3 + 4 + 5} => (+ 3 4 5).
; Otherwise, transform to (nfx list), and presume that some macro named
; "nfx" will take care of things.
; (defun curly-brace-infix-reader (stream char)
;   ; (declare ignore char)
;   (let ((result (read-delimited-list #\} stream t)))
;     (if (simple-infix-listp result)
;        (transform-simple-infix result) ; Simple infix expression.
;        (cons 'nfx result)))) ; Non-simple; prepend "nfx" to the list.
; 
; ; Invoke curly-brace-infix-reader when "{" is read in:
; (set-macro-character #\{ #'curly-brace-infix-reader)

; Invoke reader on {, [
; Eventually do it with "(" too, but afraid to do that while debugging...

(defun startup-modern-read (stream char)
  (unread-char char stream)
  (modern-read stream nil nil t))

(defun startup-modern-tail (stream char)
  (unread-char char stream)
  (modern-process-tail stream nil nil t))

; Now, set up readtable.

; When you see (, [, or {, invoke modern reader.
; (Actually, only the nil is truly necessary.  But since we're here anyway,
; we may as well directly invoke the reader.)
(set-macro-character #\[ #'startup-modern-read nil modern-readtable)
(set-macro-character #\{ #'startup-modern-read nil modern-readtable)
(set-macro-character #\( #'startup-modern-read nil modern-readtable)

; This is necessary, else a cuddled ] or } will be part of an atom: 
(set-macro-character #\] (get-macro-character #\) ) nil modern-readtable)
(set-macro-character #\} (get-macro-character #\) ) nil modern-readtable)

; Install it!
(setf *readtable* modern-readtable)


; Install it!
; The "symbol-function" stuff is because CL has separate "function" values
; vs. ordinary access values; not relevant to Scheme.

; (set-macro-character #\[ #'startup-modern-read)
; (set-macro-character #\{ #'startup-modern-read)
; (set-macro-character #\( #'startup-modern-read)

; (set-macro-character #\[ #'startup-modern-read)
; (set-macro-character #\{ #'startup-modern-read)
; (set-macro-character #\( #'startup-modern-read)


; In theory, we could redefine "read" and "read-preserving-whitespace"
; by overwriting their function cells, like this:
; (defun read (&optional (input-stream *standard-input*)
;                                eof-error-p eof-value recursive-p)
;   (funcall #'modern-read input-stream eof-error-p eof-value recursive-p))
; 
; (defun read-preserving-whitespace (&optional (input-stream *standard-input*)
;                                eof-error-p eof-value recursive-p)
;   (funcall #'modern-read input-stream eof-error-p eof-value recursive-p))
; However, this causes failure in clisp, with these kinds of messages:
; WARNING: DEFUN/DEFMACRO: redefining function READ in ..., was
; defined in C ** - Continuable Error

; MISC NOTES:
; In CL, call it using "funcall".  Neither is needed in Scheme.
