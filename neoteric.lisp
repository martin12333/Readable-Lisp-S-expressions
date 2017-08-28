;;; neoteric.cl
;;; Implements neoteric-expressions from the "readable" approach for Lisp.

;;; Copyright (C) 2007-2013 by David A. Wheeler
;;;
;;; This software is released as open source software under the "MIT" license:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; Neoteric-expressions themselves are a very simple notation and their
;;; basic implementation is also simple.
;;; Unfortunately, some quirks in Common Lisp (CL) mean that we have to do
;;; some work-arounds as compared to other Lisps like Scheme:
;;; 1. CL "read" by default consumes trailing whitespace, even if the
;;;    the whitespace is NOT part of the datum being read at all.
;;;    This is an unfortunate quirk, and in my view, a bug.  As a result,
;;;    if you just do a "read", a later datum may look
;;;    like a neoteric tail.  E.G., given: "x (y)", a simple read of "x"
;;;    will consume the space after "x"; naively checking for a "tail"
;;;    would them make it look like "x(y)" instead, wrongly producing "(x y").
;;;    For example, if we naively pass down "recursive" as "t" in
;;;    all places, it'll use this default and consume trailing whitespace.
;;;    Thus, we have to be careful about calling read or calling any
;;;    reads with recursive=t. Instead, we'll typically call
;;;    "read-preserving-whitespace", call with recursive=nil, or specially
;;;    extract characters into a string for reading.
;;; 2. There's no portable way to directly replace the "read" procedure,
;;;    so we must manipulate the readtable to do what we want.
;;;    We end up wrapping all constituents so that we can prevent consuming
;;;    trailing whitespace, to distinguish "x(y)" from "x (y)".



(cl:in-package :readable)

(defvar *neoteric-underlying-readtable* (copy-readtable)
        "Use this table when reading neoteric atoms")

(defvar *neoteric-readtable* (copy-readtable)
        "Use this table when about to read a neoteric expression")


(defvar *noisy* t
        "If true, a parse error prints an error message to standard out")

; Work around SBCL nonsense that makes its "defconstant" useless.
; See: http://www.sbcl.org/manual/Defining-Constants.html
; This is disabled, because we already define it in "print".
; (defmacro define-constant (name value &optional doc)
;  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
;                      ,@(when doc (list doc))))

; Marker for eof
(define-constant my-eof-marker (make-symbol "my-eof-marker"))

(define-condition readable-parse-error (parse-error)
  ((text :initarg :text :reader text)))

(defun read-error (message)
  (when *noisy*
    (terpri *standard-output*)
    (princ "Error: " *standard-output*)
    (princ message *standard-output*)
    (terpri *standard-output*))
  (error 'readable-parse-error :text message))

; Unfortunately, clisp will write all symbols with |...| around them
; when neoteric- or sweet-expressions are enabled.
; The issue is in clisp file "src/io.d" function "pr_symbol_part".
; We hope to be able to modify clisp in the future so it can be disabled,
; but for now, just carry one.

; NOTE: clisp's "peek-char" has a serious bug; it defaults to CONSUME
; a following whitespace, contravening the Common Lisp spec:
;    http://www.lispworks.com/documentation/HyperSpec/Body/f_peek_c.htm
; We work around this by ALWAYS providing peek-char with 2 parameters
; ("nil" and the stream name) when we don't want to skip whitespace.

; Test to ensure that peek-char (as we use it) works.
(with-input-from-string (test-input "Q56 T78")
  (progn
    ; Read "Q56"; this should NOT consume the space after it.
    (read-preserving-whitespace test-input t nil)
    (let ((c (peek-char nil test-input)))
      (when (not (eql c #\space))
        (terpri) (terpri) (terpri)
        (princ "*** WARNING WARNING WARNING ***") (terpri)
        (princ "Procedure read-preserving-whitespace or peek-char") (terpri)
        (princ "FAIL to preserve whitespace following expressions.") (terpri)
        (princ "*** WARNING WARNING WARNING ***") (terpri)
        (terpri) (terpri) (terpri)
        (error "peek-char BUG")))))

; Set of constituents - these will be overridden.
; We should support arbitrary UTF-8 characters, but it can be complicated
; to do so portably.  For now, we'll define this as a variable so it
; can be overridden.
(defvar *constituents*
    '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
      #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\: #\< #\= #\> #\? #\@
      #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
      #\^ #\_
      #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~
      #\rubout )) ; Rubout, really?!?  Yup, it's in the spec.

; TODO: Add UTF-8 constituents to *constituents* if the underlying
; system supports them in the readtable.

;;; Key procedures to implement neoteric-expressions

; These delimiting characters stop reading of symbols or non-datums
; (e.g., after ".").  We'll define these as a "variable" so that other
; routines can reach in and change them, if truly necessary.
(defvar neoteric-delimiters
  '(#\( #\) #\[ #\] #\{ #\} #\space #\tab #\newline #\return #\#
    #\' #\` #\,))

(defun string-before-delimiter (input-stream start)
  (concatenate 'string start
    (loop for c = (peek-char nil input-stream nil my-eof-marker)
          until (or (eq c my-eof-marker) (find c neoteric-delimiters))
          collect (read-char input-stream))))

; Note: In traditional Common Lisp, symbols with just multiple "."
; are illegal, especially "...".  For the moment we'll allow it.

; Read a datum and ALLOW "." as a possible value:
(defun my-read-to-delimiter (input-stream start)
  (let* ((*readtable* *neoteric-underlying-readtable*) ; Temporary switch
         (my-string (string-before-delimiter input-stream start)))
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
        (read-error "Bad closing character."))
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
                   ; (read-error "Early eof in (... .).")
                   ; '())
                   ; The following peek-char has side-effect of skipping
                   ; whitespace after last datum, so "(a . b )" works.
                   ((not (eql (peek-char t input-stream) stop-char))
                    (read-error "Bad closing character after . datum."))
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
    (let* ((c (peek-char nil input-stream nil my-eof-marker)))
      (cond
        ((eq c my-eof-marker) prefix)
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

; Read preserving whitespace using the underlying readtable, then
; apply a neoteric tail if necessary.  This is necessary for handling
; various situations (including constituent characters) 
; to ensure that trailing whitespace is NEVER consumed before looking for
; the tail.  Otherwise '{a + {b * c}} will be incorrectly
; interpreted as (A (+ (* B C))) instead of the correct (+ A (* B C)).
; That's because if the whitespace after "+" is (incorrectly)
; consumed, it will be interpreted as '{a +{b * c}}.

(defun wrap-read-n-tail (stream char)
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

  ; This is more convoluted than you'd expect, because
  ; Common Lisp's "read" provides no simple way to *prevent*
  ; consuming trailing whitespace if the read is at the top level.
  ; When that happens, trailing whitespace will be consumed *BEFORE* the
  ; neoteric-tail check is performed.  If, after the whitespace, there's
  ; something that looks like a tail, the wrong result will occur.
  ; E.G., the neoteric expression:
  ;   '#B101 (quote x)
  ; would in the "obvious" implementation be the incorrect: (5 |QUOTE| |X|)
  ; instead of the correct 5 followed by x.
  ; To deal with this, we collect all the characters before a delimiter,
  ; put them into a string, and read from the string instead.
  (neoteric-process-tail stream
    (let* ((ctext (string-before-delimiter stream ""))
           (*readtable* *neoteric-underlying-readtable*)) ; temporary switch.
      (with-input-from-string (string-stream ctext)
        (funcall
          (get-dispatch-macro-character #\# sub-char
                                      *neoteric-underlying-readtable*)
          string-stream sub-char int)))))

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

(defun enable-neoteric-real ()
  (when (setup-enable 'neoteric)
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
    ; TODO: Don't wrap if they aren't constituents any more.
    (dolist (c *constituents*)
      (set-macro-character c #'wrap-read-n-tail nil))

    ; We need to do this so symbols starting with an escape will work:
    (set-macro-character #\\ #'wrap-read-n-tail nil)
    (set-macro-character #\| #'wrap-read-n-tail nil)

    ; This ensures that "hi"(5) => ("hi" 5)
    (set-macro-character #\" #'wrap-read-n-tail nil)

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
    ; How we wrap it depends on what will follow the macro char construct:
    ; - Datums-to-follow like #+ and #;.  No change; the default
    ;   neoteric readtable already handles datums.
    ; - Undefined or "signals error" - no change.
    ; - "Special-meaning" like #x.  These aren't followed by a datum. Instead,
    ;   this is a sequence of characters that represents some special value.
    ;   These characters (including the characters that started them)
    ;   are read until a delimiter, put in a string, and read from the string.
    ;   The result is then processed specially to look for
    ;   a neoteric tail.  That way, constructs like "#xa(#xb)" work and are
    ;   distinguished from  "#xa (#xb)". Use #'wrap-dispatch-disabled-tail.
    ;
    ; See Common Lisp hyperspec section 2.4.8
    ; http://www.lispworks.com/documentation/HyperSpec/Body/02_dh.htm
    ; Below is every standard # macro character syntax (except undefined
    ; and signals error), in order (note the "debatable" ones):

    ;   ##      = reference to #= label    - Intentionally not wrapped
    ;   #'      = function abbreviation    - Intentionally not wrapped
    ;   #(...)  = vector                   - Intentionally not wrapped
    ;   #*      = bit-vector               - Special-meaning, wrapped
    (set-dispatch-macro-character #\# #\* #'wrap-dispatch-disabled-tail)
    ;   #,      = (was) load-time eval [Steele] - Intentionally not wrapped
    ;   #0..9   = used for infix arguments - Can't really wrap anyway.
    ;   #:      = uninterned symbol        - Special-meaning, wrapped
    (set-dispatch-macro-character #\# #\: #'wrap-dispatch-disabled-tail)
    ;   #;      = datum comment (extension)- Intentionally not wrapped
    ;   #=      = label following object   - Intentionally not wrapped
    ;   #\char  = character object         - Special-meaning, wrapped
    (set-dispatch-macro-character #\# #\\ #'wrap-dispatch-special-read-tail)
    ;   #|...|# = balanced comment         - Intentionally not wrapped
    ;   #+      = read-time conditional    - Intentionally not wrapped
    ;   #-      = read-time conditional    - Intentionally not wrapped
    ;   #.      = read-time evaluation     - Intentionally not wrapped
    ;   #A,#a   = array                    - Not currently wrapped (debatable).
    ;   #B,#b   = binary rational          - Special-meaning, wrapped
    (set-dispatch-macro-character #\# #\B #'wrap-dispatch-disabled-tail)
    (set-dispatch-macro-character #\# #\b #'wrap-dispatch-disabled-tail)
    ;   #C,#c   = complex number           - Not currently wrapped (debatable).
    ;             Complex numbers, because of their format, are tricky to wrap,
    ;             and there's no compelling reason to do so.
    ;   #O,#o   = octal rational           - Special-meaning, wrapped
    (set-dispatch-macro-character #\# #\O #'wrap-dispatch-disabled-tail)
    (set-dispatch-macro-character #\# #\o #'wrap-dispatch-disabled-tail)
    ;   #P,#p   = pathname                 - Not wrapped currently (debatable).
    ;             In the future this might be wrapped for #p"hi"(5), but
    ;             it's not obvious it would ever be used that way.
    ;   #R,#r   = radix-n rational         - Special-meaning, wrapped
    (set-dispatch-macro-character #\# #\R #'wrap-dispatch-disabled-tail)
    (set-dispatch-macro-character #\# #\r #'wrap-dispatch-disabled-tail)
    ;   #S,#s   = structure                - Not currently wrapped (debatable).
    ;   #X,#x   = hexadecimal rational     - Special-meaning, wrapped
    (set-dispatch-macro-character #\# #\X #'wrap-dispatch-disabled-tail)
    (set-dispatch-macro-character #\# #\x #'wrap-dispatch-disabled-tail)

    ; Save in separate variable, so "sweet" can just create its own if needed
    (setq *neoteric-readtable* *readtable*))

  (values))


; Read until }, then process list as infix list.
(defun full-curly-brace-infix-reader (stream char)
  (declare (ignore char))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-neoteric-real)
    (let* ((result (my-read-delimited-list #\} stream))
           (processed-result (process-curly result)))
      processed-result)))

(defun enable-full-curly-infix-real ()
  (when (setup-enable 'full-curly-infix)
    ; Invoke curly-brace-infix-reader when opening curly brace is read in:
    (set-macro-character #\{ #'full-curly-brace-infix-reader) ; (
    ; This is necessary, else a cuddled closing brace will be part of an atom:
    (set-macro-character #\} (get-macro-character #\) nil)))
  (values)) ; Meaning "Did it"

(defun curly-infix-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-full-curly-infix-real)
    (read stream)))

(defun neoteric-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-neoteric-real)
    (read stream)))


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

