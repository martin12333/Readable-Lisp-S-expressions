; neoteric.cl
; Implements neoteric-expressions from the "readable" approach for Lisp.

; WARNING: THIS IS NOT READY FOR PRODUCTION USE YET.
; For Common Lisp, use the "basic-curly-infix" mode (which is more mature),
; or the Scheme implementation (which has a Common Lisp mode)
; at this time.  In the meantime, we'd love help finishing this!

; Copyright (C) 2007-2013 by David A. Wheeler
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

; Neoteric-expressions themselves are easy, but since we can't portably
; overide "read" directly, we have to manipulate the readtable.


(cl:in-package :readable)

; Stop reading characters after "." when you see one of these.
(defconstant neoteric-delimiters
  '(#\( #\) #\[ #\] #\{ #\} #\space #\tab #\newline #\return #\#
    #\' #\` #\,))

; Nonsense marker for eof - TODO - remove
(defconstant neoteric-eof-marker (cons 'eof '()))

(defvar *neoteric-underlying-readtable* (copy-readtable)
        "Use this table when reading neoteric atoms")

; TODO: Handle eof as directed by "read".  Not currently consistent.
; TODO: Rationalize error signaling/handling.
(defun read-error (message)
  (declare (ignore message))
  nil)

; TODO: If possible, make it so clisp doesn't keep responding with |...|
; around all tokens.  It's legal, but ugly.  This seems to happen because
; we set the alphanumeric letters to be macros.

; NOTE: clisp's "peek-char" has a serious bug; it defaults to CONSUME
; a following whitespace, contravening the Common Lisp spec:
;    http://www.lispworks.com/documentation/HyperSpec/Body/f_peek_c.htm
; We work around this by ALWAYS providing peek-char with 2 parameters
; ("nil" and the stream name) when we don't want to skip whitespace.

; Also: CL "read" really wants to consume trailing whitespace, which is bad.
; If we naively pass down "recursive" as "t" in all places, it'll consume
; the trailing whitespace. Thus, we have to be careful about calling read or
; calling any reads with recursive=t.

; Test to ensure that peek-char (as we use it) works.
(with-input-from-string (test-input "Q56 T78")
  (progn
    ; Read "Q56"; this should NOT consume the space after it.
    (read-preserving-whitespace test-input t nil)
    (let ((c (peek-char nil test-input)))
      (when (not (eql c #\space))
        (terpri) (terpri) (terpri)
        (princ "*** WARNING WARNING WARNING ***")
        (princ "Your Common Lisp implementation has a serious defect.")
        (princ "Procedure read-preserving-whitespace or peek-char")
        (princ "FAIL to preserve whitespace following expressions.")
        (princ "*** WARNING WARNING WARNING ***")
        (terpri) (terpri) (terpri)))))


;;; Key procedures to implement neoteric-expressions

; TODO: Make "..." illegal, and maybe anything with just multiple ".".

; Read a datum and ALLOW "." as a possible value:
(defun my-read-to-delimiter (input-stream start)
  (let*
    ((*readtable* *neoteric-underlying-readtable*) ; Temporary switch
     (clist
      (loop
        until (find (peek-char nil input-stream) neoteric-delimiters)
        collect (read-char input-stream)))
     (my-string (concatenate 'string start clist)))
    (if (string= my-string ".")
        '|.|
        (read-from-string my-string))))

(defun my-read-datum (input-stream)
  (let* ((c (peek-char t input-stream))) ; Consume leading whitespace
    (cond
      ((eql c #\.) ; Use specialized reader if starts with "."
        (my-read-to-delimiter input-stream ""))
      (t (read-preserving-whitespace input-stream t nil)))))

(defun my-read-delimited-list (stop-char input-stream)
 (handler-case
  (let* ((c (peek-char t input-stream))) ; First consume leading whitespace
    (cond
      ((eql c stop-char)
        (read-char input-stream)
        '())
      ; Balance ([{
      ((or (eql c #\)) (eql c #\]) (eql c #\}))
        (read-char input-stream)
        (read-error "Bad closing character"))
      (t
        ; Must preserve whitespace so "a ()" isn't read as "a()"
        (let ((datum (my-read-datum input-stream)))
          (cond
             ; Note: "." only counts as cdr-setting if it begins with "."
             ((and (eq datum '|.|) (eql c #\.))
               (let ((datum2 (read-preserving-whitespace input-stream t nil)))
                 ; (consume-whitespace input-stream)
                 (cond
                   ; ((eof-object? datum2)
                   ; (read-error "Early eof in (... .)\n")
                   ; '())
                   ; The following peek-char has side-effect of skipping
                   ; whitespace after last datum, so "(a . b )" works.
                   ((not (eql (peek-char t input-stream) stop-char))
                    (read-error "Bad closing character after . datum"))
                   (t
                     (read-char input-stream)
                     datum2))))
             (t
                 (cons datum
                   (my-read-delimited-list stop-char input-stream))))))))))


; Implement neoteric-expression's prefixed (), [], and {}.
; At this point, we have just finished reading some expression, which
; MIGHT be a prefix of some longer expression.  Examine the next
; character to be consumed; if it's an opening paren, bracket, or brace,
; then the expression "prefix" is actually a prefix.
; Otherwise, just return the prefix and do not consume that next char.
; This recurses, to handle formats like f(x)(y).
(defun neoteric-process-tail (input-stream prefix)
    (let* ((c (peek-char nil input-stream)))
      (cond
        ((eq c neoteric-eof-marker) prefix) ; TODO
        ((eql c #\( ) ; Implement f(x).
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
              (cons prefix (my-read-delimited-list #\) input-stream))))
        ((eql c #\[ )  ; Implement f[x]
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
                (cons '$bracket-apply$
                  (cons prefix
                    (my-read-delimited-list #\] input-stream)))))
        ((eql c #\{ )  ; Implement f{x}.
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
            (let ((tail (process-curly
                          (my-read-delimited-list #\} input-stream))))
              (if (null tail)
                (list prefix) ; Map f{} to (f), not (f ()).
                (list prefix tail)))))
        (t prefix))))


;;; Dispatch procedures.

; Read until }, then process list as infix list.
(defun neoteric-curly-brace (stream char)
  (declare (ignore char)) ; {
  (let ((result (my-read-delimited-list #\} stream)))
    (neoteric-process-tail stream (process-curly result))))

; Read constituents. We need to be careful reading consituents to ensure that
; trailing whitespace is NEVER consumed.  Otherwise
; '{a + {b * c}} will incorrectly be interpreted as (A (+ (* B C)))
; instead of the correct (+ A (* B C)).
; That's because if the whitespace after "+" is (incorrectly)
; consumed, it will be interpreted as '{a +{b * c}}.

(defun wrap-constituent (stream char)
  (unread-char char stream)
  (let ((saved-readtable *readtable*))
    (setq *readtable* *neoteric-underlying-readtable*)
    ; Do NOT make recursive, or spaces after atoms will be consumed.
    (let ((atom (read-preserving-whitespace stream t nil)))
      (setq *readtable* saved-readtable)
      (neoteric-process-tail stream atom))))

(defun wrap-continue (stream char)
  ; Call routine from original readtable, without removing, and
  ; invoke neoteric-process-tail.
  (neoteric-process-tail stream
    (funcall
      (get-macro-character char *neoteric-underlying-readtable*)
      stream char)))

(defun wrap-dispatch-tail (stream sub-char int)
  ; Call routine from original readtable, but leave our readtable in place,
  ; and invoke neoteric-process-tail.
  (neoteric-process-tail stream
    (funcall
      (get-dispatch-macro-character #\# sub-char
                                    *neoteric-underlying-readtable*)
      stream sub-char int)))

(defun wrap-dispatch-disabled-tail (stream sub-char int)
  ; Call routine from original readtable and disable temporarily our
  ; readtable.  Then invoke neoteric-process-tail.
  ; TODO: There's no obvious way to *prevent* this from consuming
  ; trailing whitespace if the top-level routine consumed trailing whitespace.
  (neoteric-process-tail stream
    (let ((*readtable* *neoteric-underlying-readtable*)) ; temporary switch.
      (funcall
        (get-dispatch-macro-character #\# sub-char
                                      *neoteric-underlying-readtable*)
        stream sub-char int))))

(defun wrap-dispatch-disabled-notail (stream sub-char int)
  ; Call routine from original readtable and disable temporarily our
  ; readtable.  Do NOT invoke neoteric-process-tail.
  ; There's no obvious way to *prevent* this from consuming
  ; trailing whitespace if the top-level routine consumed trailing whitespace.
    (let ((*readtable* *neoteric-underlying-readtable*)) ; temporary switch.
      (funcall
        (get-dispatch-macro-character #\# sub-char
                                      *neoteric-underlying-readtable*)
        stream sub-char int)))

(defun wrap-dispatch-special-read-tail (stream sub-char int)
  ; Get chars until a delimiter, then read it by disabling temporarily our
  ; readtable.  Then invoke neoteric-process-tail.
  (declare (ignore int))
  (unread-char sub-char stream)
  (neoteric-process-tail stream
    (let ((*readtable* *neoteric-underlying-readtable*)) ; temporary switch.
      (my-read-to-delimiter stream "#"))))

(defun wrap-paren (stream char)
  (neoteric-process-tail stream
    (my-read-delimited-list ; (
      (if (eql char #\[) #\] #\) )
      stream)))


;;; Enablers


(defun enable-neoteric ()
  (setq *original-readtable* (copy-readtable))
  (setq *neoteric-underlying-readtable* (copy-readtable))
  (set-macro-character #\{ #'neoteric-curly-brace nil
    *neoteric-underlying-readtable*) ; (
  (set-macro-character #\} (get-macro-character #\)) nil
    *neoteric-underlying-readtable*)
  (unless (get-macro-character #\[ )
    (set-macro-character #\[ #'wrap-paren nil
      *neoteric-underlying-readtable*))
  (unless (get-macro-character #\] ) ; (
    (set-macro-character #\] (get-macro-character #\) ) nil
      *neoteric-underlying-readtable*))

  ; Wrap all constituents.  Presume ASCII for now.
  ; TODO: Handle non-ASCII chars if platform supports them.
  ; TODO: Don't wrap if they aren't constituents any more.
  (dolist (c
    '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\: #\< #\= #\> #\? #\@
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      #\^ #\_
      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~
      #\rubout )) ; Rubout, really?!?  Yup, it's in the spec.
    (set-macro-character c #'wrap-constituent nil))

  ; These aren't constituents, but it still works, and we need to do this
  ; so symbols starting with an escape will work:
  (set-macro-character #\\ #'wrap-constituent nil)
  (set-macro-character #\| #'wrap-constituent nil)

  ; Wrap character pairs.
  (set-macro-character #\( #'wrap-paren nil) ; )
  (set-macro-character #\{ #'neoteric-curly-brace nil) ; (
  (set-macro-character #\} (get-macro-character #\) ) nil)
  (unless (get-macro-character #\[ )
    (set-macro-character #\[ #'wrap-paren nil)) ; (
  (unless (get-macro-character #\] )
    (set-macro-character #\] (get-macro-character #\) ) nil))

  ; Now deal with dispatch macro char; we'll just deal with default "#".
  ; set-dispatch-macro-character disp-char sub-char function
  ;                              &optional readtable 
  ;    Where "function" takes parameters (stream char arg).
  ; get-dispatch-macro-character disp-char sub-char &optional readtable
  ; See: http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node191.html
  ;
  ; No need to wrap "undefined" and "signals error" syntaxes.
  ; No need to wrap: ##  #'  #|...|#  #0..#9
  ; Status (TODO unless otherwise noted):
  ;   #( #)   = vector
  ;   #*      = bit-vector  
  ;   #,      = load-time eval
  ;   #:      = uninterned symbol
  ;   #=      = label following object
  ;   #\char  = character object         - Wrapped
  ;   #+      = read-time conditional    - wrapped
  ;   #-      = read-time conditional    - wrapped
  ;   #.      = evaluation
  ;   #A,#a   = array
  ;   #B,#b   = binary rational
  ;   #C,#c   = complex number
  ;   #O,#o   = octal rational
  ;   #P,#p   = pathname
  ;   #R,#r   = radix-n rational
  ;   #S,#s   = structure
  ;   #X,#x   = hexadecimal rational

  (set-dispatch-macro-character #\# #\\ #'wrap-dispatch-special-read-tail)

  ; TODO: For now, use wrap-dispatch-disabled-tail for almost everything.
  ; This is probably wrong, but isn't a bad placeholder.
  (set-dispatch-macro-character #\# #\* #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\, #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\: #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\= #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\. #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\A #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\a #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\B #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\b #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\C #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\c #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\O #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\o #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\P #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\p #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\R #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\r #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\S #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\s #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\X #'wrap-dispatch-disabled-tail)
  (set-dispatch-macro-character #\# #\x #'wrap-dispatch-disabled-tail)
  ; This is definitely wrong for feature expressions, but sometimes
  ; this will work anyway:
  (set-dispatch-macro-character #\# #\+ #'wrap-dispatch-disabled-notail)
  (set-dispatch-macro-character #\# #\- #'wrap-dispatch-disabled-notail)


  t) ; Return "t" meaning "it worked".


; Read until }, then process list as infix list.
; TODO: Make more efficient by rm copy-readtable.
(defun full-curly-brace-infix-reader (stream char)
  (declare (ignore char))
  (let* ((saved-original-readtable (copy-readtable *original-readtable*))
         (saved-readtable (copy-readtable *readtable*))
         (enabled (enable-neoteric))
         (result (my-read-delimited-list #\} stream))
         (processed-result (process-curly result)))
    (declare (ignore enabled))
    (setq *original-readtable* saved-original-readtable)
    (setq *readtable* saved-readtable)
    processed-result))

(defun enable-full-curly-infix ()
  (setq *original-readtable* (copy-readtable))
  ; Invoke curly-brace-infix-reader when opening curly brace is read in:
  (set-macro-character #\{ #'full-curly-brace-infix-reader) ; (
  ; This is necessary, else a cuddled closing brace will be part of an atom:
  (set-macro-character #\} (get-macro-character #\) nil))
  t) ; Meaning "Did it"


; TODO: sbcl reports the following error:
; ; file: /home/dwheeler/readable-code/neoteric.lisp
; ; in: DEFUN READABLE::MY-READ-DELIMITED-LIST
; ;     (READABLE::READ-ERROR "Bad closing character")
; ; caught STYLE-WARNING:
; ;   undefined function: READABLE::READ-ERROR


;   (defun neoteric-filter ()
;     (handler-case
;       (do ((result (neoteric-read) (neoteric-read)))
;         (nil nil)
;         (write result)
;         (terpri))
;       (end-of-file ())))
;   
;   (defun neoteric-load (filename)
;    (handler-case
;     (with-open-file (s (make-pathname :name filename) :direction :input)
;       (do ((result (neoteric-read s) (neoteric-read s)))
;         (nil nil)
;         (eval result)))
;     (end-of-file () )))


; TODO: Add writers, e.g., neoteric-write.

