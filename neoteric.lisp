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

(defvar *neoteric-underlying-readtable* *readtable* "Call for neoteric atoms")

; TODO: If possible, make it so clisp doesn't keep responding with |...|
; around all tokens.  It's legal, but ugly.  This seems to happen because
; we set the alphanumeric letters to be macros.

; NOTE: clisp's "peek-char" has a serious bug; it defaults to CONSUME
; a following whitespace, contravening the Common Lisp spec:
;    http://www.lispworks.com/documentation/HyperSpec/Body/f_peek_c.htm
; We work around this by ALWAYS providing peek-char with 2 parameters
; ("nil" and the stream name) when don't want to skip whitespace.

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

; We need to be careful reading consituents to ensure that
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
      ; (princ "DEBUG atom=") (write atom) (terpri)
      ; (princ "DEBUG peek-char=") (write (peek-char nil stream)) (terpri)
      (neoteric-process-tail stream atom))))

(defun wrap-continue (stream char)
  ; Call routine from original readtable, without removing, and
  ; invoke neoteric-process-tail.
  (neoteric-process-tail stream
    (funcall
      (get-macro-character char *neoteric-underlying-readtable*)
      stream char)))

; Read until }, then process list as infix list.
(defun neoteric-curly-brace (stream char)
  (declare (ignore char)) ; {
  (let ((result (my-read-delimited-list #\} stream)))
    (neoteric-process-tail stream (process-curly result))))

; (defun my-read-delimited-list (stop-char input-stream)
;  (read-delimited-list stop-char input-stream))

(defun wrap-paren (stream char)
  (declare (ignore char))
  (neoteric-process-tail stream ; (
    (my-read-delimited-list #\) stream)))

; TODO: Handle [], other non-constituents, macro chars beginning with "#".
(defun enable-neoteric ()
  ; (setq *readtable* (copy-readtable *original-readtable*))
  (setq *neoteric-underlying-readtable* (copy-readtable))
  (set-macro-character #\{ #'neoteric-curly-brace nil
    *neoteric-underlying-readtable*) ; (
  (set-macro-character #\} (get-macro-character #\)) nil
    *neoteric-underlying-readtable*)

  ; (setq *readtable* (copy-readtable))
  ; Don't wrap {} or [] this way
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
      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\~))
    (set-macro-character c #'wrap-constituent nil))
  (set-macro-character #\( #'wrap-paren nil) ; )
  (set-macro-character #\{ #'neoteric-curly-brace nil) ; (
  (set-macro-character #\} (get-macro-character #\) ) nil)
  nil)

; Nonsense marker for eof
(defvar neoteric-eof-marker (cons 'eof '()))

; TODO: Ensure "a(. b)" and "(. x)" correctly handled.

; TODO: sbcl reports the following error:
; ; file: /home/dwheeler/readable-code/neoteric.lisp
; ; in: DEFUN READABLE::MY-READ-DELIMITED-LIST
; ;     (READABLE::READ-ERROR "Bad closing character")
; ; caught STYLE-WARNING:
; ;   undefined function: READABLE::READ-ERROR
;

; TODO
(defun read-error (message)
  (declare (ignore message))
  nil)

; TODO: Must be able to handle ".", e.g., "a(. b)" and "a(b . c)".
(defun my-read-delimited-list (stop-char input-stream)
 (handler-case
  (let*
    ((c (peek-char t input-stream))) ; Consume leading whitespace
    ; (princ "DEBUG enter my-read-delimited-list peek=") (write c) (terpri)
    (cond
      ; TODO:
      ; ((eof-object? c) (read-error "EOF in middle of list") '())
      ((eql c stop-char)
        (read-char input-stream)
        '())
      ; Balance ([{
      ((or (eql c #\)) (eql c #\]) (eql c #\}))
        (read-char input-stream)
        (read-error "Bad closing character"))
      (t
        ; Must preserve whitespace so "a ()" isn't read as "a()"
        (let ((datum (read-preserving-whitespace input-stream t nil)))
          ; (princ "DEBUG enter my-read-delimited-list post-datum peek=")
          ; (write (peek-char nil input-stream))
          ; (princ " datum=") (write datum) (terpri)
          (cond
             ; Note: "." only counts as cdr-setting if it begins with "."
             ((and (eql datum '|.|) (eql c #\.))
               (let ((datum2 (read-preserving-whitespace input-stream t nil)))
                 ; (consume-whitespace input-stream)
                 (cond
                   ; ((eof-object? datum2)
                   ; (read-error "Early eof in (... .)\n")
                   ; '())
                   ((not (eql (peek-char nil input-stream) stop-char))
                    (read-error "Bad closing character after . datum"))
                   (t
                     (read-char nil input-stream)
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
        ((eq c neoteric-eof-marker) prefix)
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
          ; (princ "DEBUG {} tail") (terpri)
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
            (let ((tail (process-curly
                          (my-read-delimited-list #\} input-stream))))
              (if (null tail)
                (list prefix) ; Map f{} to (f), not (f ()).
                (list prefix tail)))))
        (t prefix))))


; OLD JUNK:
; (Will delete once the new system is implemented.)

;   ; Read, then process it through neoteric-tail to manage suffixes.
;   ; Unlike Scheme, in Common Lisp we can force the reader to consider
;   ; {} and [] as delimiters, so we don't have to re-implement some parts of
;   ; the reader.  However, Common Lisp's approach to backquote and comma is
;   ; more complicated to implement.
;   (defun neoteric-read (&optional (input-stream *standard-input*)
;                           (eof-error-p t) (eof-value nil) (recursive-p nil))
;    (handler-case
;     (neoteric-process-tail input-stream
;       (let* ((c (peek-char t input-stream)))
;         (cond
;           ((eql c #\( )
;             (read-char nil input-stream)
;             (my-read-delimited-list #\) input-stream))
;           ((eql c #\{ )
;             (read-char nil input-stream)
;             (process-curly
;               (my-read-delimited-list #\} input-stream)))
;           ((eql c #\; )
;             (read-line input-stream) ; Consume rest of the line.
;             (neoteric-read input-stream eof-error-p eof-value recursive-p))
;           ((eql c #\' )
;             (read-char nil input-stream)
;             (list 'quote
;               (neoteric-read input-stream eof-error-p eof-value recursive-p)))
;           ; TODO: Add support for comma, backquote, etc.
;           ; The Common Lisp hyperspec says in
;           ; "2.4.6.1 Notes about Backquote":
;           ; Since the exact manner in which the Lisp reader will parse an
;           ; expression involving the backquote reader macro is not specified,
;           ; an implementation is free to choose any representation that
;           ; preserves the semantics described.
;           ; Often an implementation will choose a representation that
;           ; facilitates pretty printing of the expression, so that
;           ; (pprint `(a ,b)) will display `(a ,b) and not, for example,
;           ; (list 'a b). However, this is not a requirement.
;           ; Implementors who have
;           ; no particular reason to make one choice or another might wish
;           ; to refer to IEEE Standard for the Scheme Programming Language,
;           ; which identifies a popular choice of representation for such
;           ; expressions that might provide useful to be useful compatibility
;           ; for some user communities. There is no requirement, however,
;           ; that any conforming implementation use this particular
;           ; representation. This information is provided merely for
;           ; cross-reference purposes.
;           (t
;             ; Force recursive; even at the top level, when it reads what
;             ; it *thinks* is a whole expression, we may be "inside" a
;             ; longer list.  Otherwise, EOF handling can mess up.
;             (read input-stream eof-error-p eof-value t)))))
;     (end-of-file (e)
;       (if eof-error-p
;         (signal e) ; re-raise error
;         eof-value))))
;   
;   ; Remarkably, Common Lisp provides no standard way to exit an image.
;   ; Here's a mostly-portable mechanism to do so, from:
;   ; http://www.cliki.net/Portable%20Exit
;   (defun my-portable-exit (&optional code)
;         ;; This group from "clocc-port/ext.lisp"
;         #+allegro (excl:exit code)
;         #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
;         #+cmu (ext:quit code)
;         #+cormanlisp (win32:exitprocess code)
;         #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
;         #+lispworks (lw:quit :status code)
;         #+lucid (lcl:quit code)
;         #+sbcl (sb-ext:quit
;                 :unix-code (typecase code (number code) (null 0) (t 1)))
;         ;; This group from Maxima
;         #+kcl (lisp::bye)                         ; XXX Does this take an arg?
;         #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
;         #+(or openmcl mcl) (ccl::quit)
;         #+abcl (cl-user::quit)
;         #+ecl (si:quit)
;         ;; This group from 
;         #+poplog (poplog::bye)                    ; XXX Does this take an arg?
;         #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
;               kcl scl openmcl mcl abcl ecl)
;         (error 'not-implemented :proc (list 'quit code)))
;   
;   ; Common Lisp provides no way to fully override the "read" function
;   ; such as (defun read #'neoteric-read).
;   ; We partly simulate it here by writing our own REPL, and exiting Lisp
;   ; when it ends.  The simulation is quite imperfect (a call to "read"
;   ; reveals the facade), but it's a start.
;   ; Perhaps we should special-case "load" here.
;   (defun enable-neoteric ()
;     (handler-case
;       (do ((result (neoteric-read) (neoteric-read)))
;         (nil nil)
;         (write (eval result))
;         (terpri))
;       (end-of-file () (my-portable-exit 0))))
;   
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
;   
;   ; TODO: Handle improper lists, e.g., {a + b . c} should insert $nfx$, and
;   ; port(a . b) should generate (port a . b).
;   
;   ; TODO: Handle #|...|# comments inside neoteric-read.
;   
;   
;   ; Likely things to do from here:
;   
;   ; (enable-neoteric)
;   ; (write (neoteric-read))
;   
;   ; (do ((result (read) (read)))
;   ;   (nil nil)
;   ;   (write result)
;   ;   (terpri))
;   
;   
;   (defun enable-curly-infix ()
;     ; The following install the {...} reader.
;     ; See "Common Lisp: The Language" by Guy L. Steele, 2nd edition,
;     ; pp. 542-548 and pp. 571-572.
;     ; Invoke curly-brace-infix-reader when opening curly brace is read in:
;     (set-macro-character #\{ #'curly-brace-infix-reader)
;     ; This is necessary, else a cuddled closing brace will be part of an atom:
;     ; Borrow end of (.
;     (set-macro-character #\} (get-macro-character #\) nil))
;     ; Make "[" and "]" delimiters. For now, make same as ().
;     (set-macro-character #\[ (get-macro-character #\( nil))
;     (set-macro-character #\] (get-macro-character #\) nil))
;     nil)
;   
;   ; The following install the {...} reader.
;   ; See "Common Lisp: The Language" by Guy L. Steele, 2nd edition,
;   ; pp. 542-548 and pp. 571-572.
;   
;   ; Read until }, then process list as infix list.
;   ; This shouldn't be invoked once we use neoteric-read; it takes precedence.
;   (defun curly-brace-infix-reader (stream char)
;     (let ((result (read-delimited-list #\} stream t)))
;       (process-curly result)))
;   
