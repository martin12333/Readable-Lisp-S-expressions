; sweet.lisp
; Implements sweet-expressions from the "readable" approach for Lisp.

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



(cl:in-package :readable)

; We can't portably overide "read" directly, we have to manipulate
; the readtable.  Here we redirect EVERY character to a procedure,
; effectively taking over "read".
(defvar *sweet-redirect-readtable*
  "This table redirects any input to sweet-expression processing")

(defconstant line-ending-chars (list #\linefeed #\return))
(defun char-line-endingp (char) (member char line-ending-chars))

; Does character "c" begin a line comment (;) or end-of-line?
(defconstant initial-comment-eol (list #\; #\linefeed #\return))
(defun lcomment-eolp (c)
  (member c initial-comment-eol))

(defun my-peek-char (stream) (peek-char nil stream))
(defun my-read-char (stream) (read-char nil stream))

;    ; Top level - read a sweet-expression (t-expression).  Handle special
;    ; cases, such as initial indent; call it-expr for normal case.
;    (defun t-expr (stream)
;      (let* ((c (my-peek-char stream)))
;        ; Check EOF early (a bug in guile before 2.0.8 consumes EOF on peek)
;        (if (eof-objectp c)
;            c
;            (cond
;              ((lcomment-eolp c)
;                (consume-to-eol stream)
;                (consume-end-of-line stream)
;                (t-expr stream))
;              ((or (eql c form-feed) (eql c vertical-tab))
;                (consume-ff-vt stream)
;                (t-expr stream))
;              ((char-icharp c)
;                (let ((indentation-list (cons #\^ (accumulate-ichar stream))))
;                  (if (not (member (my-peek-char stream) initial-comment-eol))
;                      (let ((results (n-expr-or-scomment stream)))
;                        (if (not (eq (car results) 'scomment))
;                            (cadr results) ; Normal n-expr, return one value.
;                            (progn ; We have an scomment; skip and try again.
;                              (hspaces stream)
;                              (t-expr stream))))
;                      (progn ; Indented comment-eol, consume and try again.
;                        (if (member #\! indentation-list)
;                            (read-error "Empty line with '!'"))
;                        (consume-to-eol stream)
;                        (consume-end-of-line stream)
;                        (t-expr stream)))))
;              (t
;                (let* ((results (it-expr stream "^"))
;                       (results-indent (car results))
;                       (results-value (cadr results)))
;                  (if (string= results-indent "")
;                      (read-error "Closing *> without preceding matching <*")
;                      results-value)))))))


;;  ; Consume an end-of-line sequence, ('\r' '\n'? | '\n'), and nothing else.
;;  ; Don't try to handle reversed \n\r (LFCR); doing so will make interactive
;;  ; guile use annoying (EOF won't be correctly detected) due to a guile bug
;;  ; (in guile before version 2.0.8, peek-char incorrectly
;;  ; *consumes* EOF instead of just peeking).
;;  (defun consume-end-of-line (stream)
;;    (let ((c (my-peek-char stream)))
;;      (cond
;;        ((eql c #\return)
;;          (my-read-char stream)
;;          (if (eql (my-peek-char stream) #\linefeed)
;;              (my-read-char stream)))
;;        ((eql c #\linefeed)
;;          (my-read-char stream)))))
;;  
(defun consume-to-eol (stream)
  ; Consume every non-eol character in the current line.
  ; End on EOF or end-of-line char.
  ; Do NOT consume the end-of-line character(s).
  (let ((c (my-peek-char stream)))
    (when (not (char-line-endingp c))
        (my-read-char stream)
        (consume-to-eol stream))))

(defun t-expr (stream)
  (let* ((c (my-peek-char stream)))
    (cond
;      ((lcomment-eolp c)
;        (consume-to-eol stream)
;        (consume-end-of-line stream)
;        (t-expr stream))
      (t (read stream)))))


(defun t-expr-entry (stream char)
  (unread-char char stream)
  (princ "DEBUG entry: ") (write char) (terpri)
  (let ((*readtable* *neoteric-readtable*))
    (t-expr stream)))

; Set up a readtable that'll redirect everything.
(defun compute-sweet-redirect-readtable ()
  (setq *sweet-redirect-readtable*
    (let ((new (copy-readtable nil)))
      (set-syntax-from-char #\# #\' new) ; force # to not be dispatching char.
      (loop for ci from 0 upto 255 ; TODO: char-code-limit
         do (set-macro-character (character ci) #'t-expr-entry nil new))
      new)))

(defun enable-sweet ()
  (enable-neoteric)
  (compute-sweet-redirect-readtable)
  (setq *readtable* *sweet-redirect-readtable*)
  t) ; Meaning "Did it"


;   ; Period symbol.  A symbol starting with "." is not
;   ; validly readable in R5RS, R6RS, R7RS (except for
;   ; the peculiar identifier "..."); with the
;   ; R6RS and R7RS the print representation of
;   ; string->symbol(".") should be |.| .  However, as an extension, this
;   ; Scheme reader accepts "." as a valid identifier initial character,
;   ; in part because guile permits it.
;   ; For portability, use this formulation instead of '. in this
;   ; implementation so other implementations don't balk at it.
;   (setq period-symbol (intern "."))
; 
;   (setq line-ending-chars (list #\linefeed #\carriage-return))
; 
;   ; This definition of whitespace chars is derived from R6RS section 4.2.1.
;   ; R6RS doesn't explicitly list the #\space character, be sure to include!
;   (setq whitespace-chars-ascii
;      (list tab linefeed line-tab form-feed carriage-return #\space))
;   ; Note that we are NOT trying to support all Unicode whitespace chars.
;   (setq whitespace-chars whitespace-chars-ascii)
; 
;   ; If t, handle some constructs so we can read and print as Common Lisp.
;   (setq common-lisp nil)
; 
;   ; If t, return |...| symbols as-is, including the vertical bars.
;   (setq literal-barred-symbol nil)
; 
;   ; Returns a true value (not necessarily t)
;   (defun char-line-endingp (char) (member char line-ending-chars))
; 
;   ; Create own version, in case underlying implementation omits some.
;   (defun my-char-whitespacep (c)
;     (or (char-whitespacep c) (member c whitespace-chars)))
; 
; 
;   (defun consume-to-whitespace (stream)
;     ; Consume to whitespace
;     (let ((c (my-peek-char stream)))
;       (cond
;         ((eof-objectp c) c)
;         ((my-char-whitespacep c)
;           '())
;         (t
;           (my-read-char stream)
;           (consume-to-whitespace stream)))))
; 
;   (setq debugger-output nil)
;   ; Quick utility for debugging.  Display marker, show data, return data.
;   (defun debug-show (marker data)
;     (cond
;       (debugger-output
;         (princ "DEBUG: ")
;         (princ marker)
;         (princ " = ")
;         (write data)
;         (princ "\n")))
;     data)
; 
;   (defun my-read-delimited-list (my-read stop-char stream)
;     ; Read the "inside" of a list until its matching stop-char, returning list.
;     ; stop-char needs to be closing paren, closing bracket, or closing brace.
;     ; This is like read-delimited-list of Common Lisp, but it
;     ; calls the specified reader instead.
;     ; This implements a useful extension: (. b) returns b. This is
;     ; important as an escape for indented expressions, e.g., (. \\)
;     (consume-whitespace stream)
;     (let*
;       ((pos (get-sourceinfo stream))
;        (c   (my-peek-char stream)))
;       (cond
;         ((eof-objectp c) (read-error "EOF in middle of list") c)
;         ((char=p c stop-char)
;           (my-read-char stream)
;           (attach-sourceinfo pos '()))
;         ((member c '(#\) #\] #\}))  (read-error "Bad closing character") c)
;         (t
;           (let ((datum (my-read stream)))
;             (cond
;                ((and (eq datum period-symbol) (char=p c #\.))
;                  (let ((datum2 (my-read stream)))
;                    (consume-whitespace stream)
;                    (cond
;                      ((eof-objectp datum2)
;                       (read-error "Early eof in (... .)")
;                       '())
;                      ((not (eql (my-peek-char stream) stop-char))
;                       (read-error "Bad closing character after . datum")
;                       datum2)
;                      (t
;                        (my-read-char stream)
;                        datum2))))
;                (t
;                  (attach-sourceinfo pos
;                    (cons datum
;                      (my-read-delimited-list my-read stop-char stream))))))))))
; 
;   ; Identifying the list of delimiter characters is harder than you'd think.
;   ; This list is based on R6RS section 4.2.1, while adding [] and {},
;   ; but removing "#" from the delimiter set.
;   ; NOTE: R6RS has "#" has a delimiter.  However, R5RS does not, and
;   ; R7RS probably will not - http://trac.sacrideo.us/wg/wiki/WG1Ballot3Results
;   ; shows a strong vote AGAINST "#" being a delimiter.
;   ; Having the "#" as a delimiter means that you cannot have "#" embedded
;   ; in a symbol name, which hurts backwards compatibility, and it also
;   ; breaks implementations like Chicken (has many such identifiers) and
;   ; Gambit (which uses this as a namespace separator).
;   ; Thus, this list does NOT have "#" as a delimiter, contravening R6RS
;   ; (but consistent with R5RS, probably R7RS, and several implementations).
;   ; Also - R7RS draft 6 has "|" as delimiter, but we currently don't.
;   (setq neoteric-delimiters
;      (append (list #\( #\) #\[ #\] #\{ #\}  ; Add [] {}
;                    #\" #\;)                 ; Could add #\# or #\|
;              whitespace-chars))
; 
;   (defun consume-whitespace (stream)
;     (let ((char (my-peek-char stream)))
;       (cond
;         ((eof-objectp char))
;         ((eql char #\;)
;           (consume-to-eol stream)
;           (consume-whitespace stream))
;         ((my-char-whitespacep char)
;           (my-read-char stream)
;           (consume-whitespace stream)))))
; 
;   (defun read-until-delim (stream delims)
;     ; Read characters until eof or a character in "delims" is seen.
;     ; Do not consume the eof or delimiter.
;     ; Returns the list of chars that were read.
;     (let ((c (my-peek-char stream)))
;       (cond
;          ((eof-objectp c) '())
;          ((member c delims) '())
;          (t (my-read-char stream) (cons c (read-until-delim stream delims))))))
; 
;   (defun read-error (message)
;     (princ "Error: " (current-error-stream))
;     (princ message (current-error-stream))
;     (terpri (current-error-stream))
;     ; Guile extension, but many Schemes have exceptions
;     (throw 'readable)
;     '())
; 
;   ; Return the number by reading from stream, and prepending starting-lyst.
;   ; Returns nil if it's not a number.
;   (defun read-number (stream starting-lyst)
;     (string->number (concatenate 'string
;       (append starting-lyst
;         (read-until-delim stream neoteric-delimiters)))))
; 
;   ; Return list of digits read from stream; may be empty.
;   (defun read-digits (stream)
;     (let ((c (my-peek-char stream)))
;       (cond
;         ((member c digits)
;           (cons (my-read-char stream) (read-digits stream)))
;         (t '()))))
; 
; 
; 
;   ; Consume characters until "!#"
;   (defun non-nest-comment (stream)
;     (let ((c (my-read-char stream)))
;       (cond
;         ((eof-objectp c)
;           (values))
;         ((char=p c #\!)
;           (let ((c2 (my-peek-char stream)))
;             (if (char=p c2 #\#)
;                 (progn
;                   (my-read-char stream)
;                   (values))
;                 (non-nest-comment stream))))
;         (t
;           (non-nest-comment stream)))))
; 
;   (defun process-sharp-bang (stream)
;     (let ((c (my-peek-char stream)))
;       (cond
;         ((char=p c #\space) ; SRFI-22
;           (consume-to-eol stream)
;           (consume-end-of-line stream)
;           scomment-result) ; Treat as comment.
;         ((member c '(#\/ #\.)) ; Multi-line, non-nesting #!/ ... !# or #!. ...!#
;           (non-nest-comment stream)
;           scomment-result) ; Treat as comment.
;         ((char-alphabeticp c)  ; #!directive
;           (process-directive
;             (concatenate 'string (read-until-delim stream neoteric-delimiters)))
;           scomment-result)
;         ((or (eql c carriage-return) (eql c #\linefeed))
;           ; Extension: Ignore lone #!
;           (consume-to-eol stream)
;           (consume-end-of-line stream)
;           scomment-result) ; Treat as comment.
;         (t (read-error "Unsupported #! combination")))))
; 
;   ; A cons cell always has a unique address (as determined by eqp);
;   ; we'll use this special cell to tag unmatched datum label references.
;   ; An unmatched datum label will have the form
;   ; (cons unmatched-datum-label-tag NUMBER)
;   (setq unmatched-datum-label-tag
;     (cons 'unmatched-datum-label-tag 'label))
;   (defun is-matching-label-tagp (number value)
;     (and
;       (consp value)
;       (eq  (car value) unmatched-datum-label-tag)
;       (eql (cdr value) number)))
; 
;   ; Patch up datum, starting at position,
;   ; so that all labels "number" are replaced
;   ; with the value of "replace".
;   ; Note that this actually OVERWRITES PORTIONS OF A LIST with "set!",
;   ; so that we can maintain "eq?"-ness.  This is really wonky,
;   ; but datum labels are themselves wonky.
;   ; "skip" is a list of locations that don't need (re)processing; it
;   ; should be a hash table but those aren't portable.
;   (defun patch-datum-label-tail (number replace position skip)
;     (let ((new-skip (cons position skip)))
;       (cond
;         ((and (consp position)
;               (not (eq (car position) unmatched-datum-label-tag))
;               (not (memq position skip)))
;           (if (is-matching-label-tagp number (car position))
;               rplaca position replace) ; Yes, "set!" !!
;               (patch-datum-label-tail number replace (car position) new-skip))
;           (if (is-matching-label-tagp number (cdr position))
;               rplacd position replace) ; Yes, "set!" !!
;               (patch-datum-label-tail number replace (cdr position) new-skip)))
;         ((vectorp position)
;           (do ((len (vector-length position))
;                (k 0 (+ k 1)))
;             ((>= k len) (values))
;             (let ((x (aref position k)))
;               (if (is-matching-label-tagp number x)
;                   (vector-set! position k replace)
;                   (patch-datum-label-tail number replace x new-skip)))))
;         (t (values)))))
; 
;   (defun patch-datum-label (number starting-position)
;     (if (is-matching-label-tagp number starting-position)
;         (read-error "Illegal reference as the labelled object itself"))
;     (patch-datum-label-tail number starting-position starting-position '())
;     starting-position)
; 
;   ; Gobble up the to-gobble characters from stream, and return # ungobbled.
;   (defun gobble-chars (stream to-gobble)
;     (if (null to-gobble)
;         0
;         (cond
;           ((char-equal (my-peek-char stream) (car to-gobble))
;             (my-read-char stream)
;             (gobble-chars stream (cdr to-gobble)))
;           (t (string-length to-gobble)))))
; 
;   (setq scomment-result '(scomment ()))
; 
;   (defun process-sharp (no-indent-read stream)
;     ; We've read a # character.  Returns what it represents as
;     ; (stopper value); ('normal value) is value, ('scomment ()) is comment.
;     ; Since we have to re-implement process-sharp anyway,
;     ; the vector representation #(...) uses my-read-delimited-list, which in
;     ; turn calls no-indent-read.
;     ; TODO: Create a readtable for this case.
;     (let ((c (my-peek-char stream)))
;       (cond
;         ((eof-objectp c) scomment-result) ; If eof, pretend it's a comment.
;         ((char-line-endingp c) ; Extension - treat # EOL as a comment.
;           scomment-result) ; Note this does NOT consume the EOL!
;         (t ; Try out different readers until we find a match.
;           (my-read-char stream)
;           (or
;             (and common-lisp
;                  (parse-cl no-indent-read c stream))
;             (parse-hash no-indent-read c stream)
;             (parse-default no-indent-read c stream)
;             (read-error "Invalid #-prefixed string"))))))
; 
;   (defun parse-default (no-indent-read c stream)
;               (cond ; Nothing special - use generic rules
;                 ((char-equal c #\t)
;                   (if (member (gobble-chars stream '(#\r #\u #\e)) '(0 3))
;                       '(normal t)
;                       (read-error "Incomplete #true")))
;                 ((char-equal c #\f)
;                   (if (member (gobble-chars stream '(#\a #\l #\s #\e)) '(0 4))
;                       '(normal nil)
;                       (read-error "Incomplete #false")))
;                 ((member c '(#\i #\e #\b #\o #\d #\x
;                            #\I #\E #\B #\O #\D #\X))
;                   (let ((num (read-number stream (list #\# (char-downcase c)))))
;                     (if num
;                         `(normal ,num)
;                         (read-error "Not a number after number start"))))
;                 ((char=p c #\( )  ; Vector.
;                   (list 'normal (list->vector
;                     (my-read-delimited-list no-indent-read #\) stream))))
;                 ((char=p c #\u )  ; u8 Vector.
;                   (cond
;                     ((not (eql (my-read-char stream) #\8 ))
;                       (read-error "#u must be followed by 8"))
;                     ((not (eql (my-read-char stream) #\( ))
;                       (read-error "#u8 must be followed by left paren"))
;                     (t (list 'normal (list->u8vector
;                           (my-read-delimited-list no-indent-read #\) stream))))))
;                 ((char=p c #\\)
;                   (list 'normal (process-char stream)))
;                 ; Handle #; (item comment).
;                 ((char=p c #\;)
;                   (no-indent-read stream)  ; Read the datum to be consumed.
;                   scomment-result) ; Return comment
;                 ; handle nested comments
;                 ((char=p c #\|)
;                   (nest-comment stream)
;                   scomment-result) ; Return comment
;                 ((char=p c #\!)
;                   (process-sharp-bang stream))
;                 ((member c digits) ; Datum label, #num# or #num=...
;                   (let* ((my-digits (read-digits stream))
;                          (label (string->number (concatenate 'string
;                                                    (cons c my-digits)))))
;                     (cond
;                       ((eql (my-peek-char stream) #\#)
;                         (my-read-char stream)
;                         (list 'normal (cons unmatched-datum-label-tag label)))
;                       ((eql (my-peek-char stream) #\=)
;                         (my-read-char stream)
;                         (if (my-char-whitespacep (my-peek-char stream))
;                             (read-error "#num= followed by whitespace"))
;                         (list 'normal
;                           (patch-datum-label label (no-indent-read stream))))
;                       (t
;                         (read-error "Datum label #NUM requires = or #")))))
;                 ; R6RS abbreviations #' #` #, #,@
;                 ((char=p c #\')
;                   '(abbrev syntax))
;                 ((char=p c #\`)
;                   '(abbrev quasisyntax))
;                 ((char=p c #\,)
;                   (let ((c2 (my-peek-char stream)))
;                     (cond
;                       ((char=p c2 #\@)
;                         (my-read-char stream)
;                         '(abbrev unsyntax-splicing))
;                       (t
;                         '(abbrev unsyntax)))))
;                 ((or (char=p c #\space) (char=p c tab))
;                   ; Extension - treat # (space|tab) as a comment to end of line.
;                   ; This is not required by SRFI-105 or SRFI-110, but it's
;                   ; handy because "# " is a comment-to-EOL in many other
;                   ; languages (Bourne shells, Perl, Python, etc.)
;                   (consume-to-eol stream)
;                   scomment-result) ; Return comment
;                 (t nil)))
; 
;   (defun parse-cl (no-indent-read c stream)
;     ; These are for Common Lisp; the "unsweeten" program
;     ; can deal with the +++ ones.
;     (cond
;       ; In theory we could just abbreviate this as "function".
;       ; However, this won't work in expressions like (a . #'b).
;       ((char=p c #\')
;         '(abbrev +++SHARP-QUOTE-abbreviation+++))
;       ((char=p c #\:)
;         '(abbrev +++SHARP-COLON-abbreviation+++))
;       ((char=p c #\.)
;         '(abbrev +++SHARP-DOT-abbreviation+++))
;       ((char=p c #\+)
;         '(abbrev +++SHARP-PLUS-abbreviation+++))
;       ((char=p c #\P)
;         '(abbrev +++SHARP-P-abbreviation+++))
;       (t nil)))
; 
;   ; Translate "x" to Common Lisp representation if we're printing CL.
;   ; Basically we use a very unusual representation, and then translate it back
;   (defun translate-cl (x)
;     (if common-lisp
;       (case x
;         ((quasiquote)       '+++CL-QUASIQUOTE-abbreviation+++)
;         ((unquote)          '+++CL-UNQUOTE-abbreviation+++)
;         ((unquote-splicing) '+++CL-UNQUOTE-SPLICING-abbreviation+++)
;         (else x))
;       x))
;                   
;   ; detect #| or |#
;   (defun nest-comment (fake-stream)
;     (let ((c (my-read-char fake-stream)))
;       (cond
;         ((eof-objectp c)
;           (values))
;         ((char=p c #\|)
;           (let ((c2 (my-peek-char fake-stream)))
;             (if (char=p c2 #\#)
;                 (progn
;                   (my-read-char fake-stream)
;                   (values))
;                 (nest-comment fake-stream))))
;         ((and hash-pipe-comment-nestsp (char=p c #\#))
;           (let ((c2 (my-peek-char fake-stream)))
;             (if (char=p c2 #\|)
;                 (progn
;                   (my-read-char fake-stream)
;                   (nest-comment fake-stream))
;                 (values))
;             (nest-comment fake-stream)))
;         (t
;           (nest-comment fake-stream)))))
; 
;   (setq digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
; 
;   (defun process-period (stream)
;     ; We've peeked a period character.  Returns what it represents.
;     (my-read-char stream) ; Remove .
;     (let ((c (my-peek-char stream)))
;       (cond
;         ((eof-objectp c) period-symbol) ; period eof; return period.
;         ((member c '(#\' #\` #\, #\#)) period-symbol) ; End, for some CL code
;         ((member c digits) ; period digit - it's a number.
;           (let ((num (read-number stream (list #\.))))
;             (if num
;                 num
;                 (read-error "period digit must be a number"))))
;         (t
;           ; At this point, Scheme only requires support for "." or "...".
;           ; As an extension we can support them all.
;           (intern
;             (fold-case-maybe stream
;               (concatenate 'string (cons #\.
;                 (read-until-delim stream neoteric-delimiters)))))))))
; 
;   ; Read an inline hex escape (after \x), return the character it represents
;   (defun read-inline-hex-escape (stream)
;     (let* ((chars (read-until-delim stream (append neoteric-delimiters '(#\;))))
;            (n (string->number (concatenate 'string chars) 16)))
;       (if (eql #\; (my-peek-char stream))
;           (my-read-char stream)
;           (read-error "Unfinished inline hex escape"))
;       (if (not n)
;           (read-error "Bad inline hex escape"))
;       (code-char n)))
; 
;   ; We're inside |...| ; return the list of characters inside.
;   ; Do NOT call fold-case-maybe, because we always use literal values here.
;   (defun read-symbol-elements (stream)
;     (let ((c (my-read-char stream)))
;       (cond
;         ((eof-objectp c) '())
;         ((eql c #\|)    '()) ; Expected end of symbol elements
;         ((eql c #\\)
;           (let ((c2 (my-read-char stream)))
;             (cond
;               ((eof-objectp c) '())
;               ((eql c2 #\|)   (cons #\| (read-symbol-elements stream)))
;               ((eql c2 #\\)   (cons #\\ (read-symbol-elements stream)))
;               ((eql c2 #\a)   (cons (code-char #x0007)
;                                      (read-symbol-elements stream)))
;               ((eql c2 #\b)   (cons (code-char #x0008)
;                                      (read-symbol-elements stream)))
;               ((eql c2 #\t)   (cons (code-char #x0009)
;                                      (read-symbol-elements stream)))
;               ((eql c2 #\n)   (cons (code-char #x000a)
;                                      (read-symbol-elements stream)))
;               ((eql c2 #\r)   (cons (code-char #x000d)
;                                      (read-symbol-elements stream)))
;               ((eql c2 #\f)   (cons (code-char #x000c) ; extension
;                                      (read-symbol-elements stream)))
;               ((eql c2 #\v)   (cons (code-char #x000b) ; extension
;                                      (read-symbol-elements stream)))
;               ((eql c2 #\x) ; inline hex escape =  \x hex-scalar-value ;
;                 (cons 
;                       (read-inline-hex-escape stream)
;                       (read-symbol-elements stream))))))
;         (t (cons c (read-symbol-elements stream))))))
; 
;   ; Extension: When reading |...|, *include* the bars in the symbol, so that
;   ; when we print it out later we know that there were bars there originally.
;   (defun read-literal-symbol (stream)
;     (let ((c (my-read-char stream)))
;       (cond
;         ((eof-objectp c) (read-error "EOF inside literal symbol"))
;         ((eql c #\|)    '(#\|)) ; Expected end of symbol elements
;         ((eql c #\\)
;           (let ((c2 (my-read-char stream)))
;             (if (eof-objectp c)
;               (read-error "EOF after \\ in literal symbol")
;               (cons c (cons c2 (read-literal-symbol stream))))))
;         (t (cons c (read-literal-symbol stream))))))
; 
;   ; Read |...| symbol (like Common Lisp)
;   ; This is present in R7RS draft 9.
;   (defun get-barred-symbol (stream)
;     (my-read-char stream) ; Consume the initial vertical bar.
;     (intern (concatenate 'string
;       (if literal-barred-symbol
;         (cons #\| (read-literal-symbol stream))
;         (read-symbol-elements stream)))))
; 
;   ; This implements a simple Scheme "read" implementation from "stream",
;   ; but if it must recurse to read, it will invoke "no-indent-read"
;   ; (a reader that is NOT indentation-sensitive).
;   ; This additional parameter lets us easily implement additional semantics,
;   ; and then call down to this underlying-read procedure when basic reader
;   ; procedureality (implemented here) is needed.
;   ; This lets us implement both a curly-infix-ONLY-read
;   ; as well as a neoteric-read, without duplicating code.
;   (defun underlying-read (no-indent-read stream)
;     (consume-whitespace stream)
;     (let* ((pos (get-sourceinfo stream))
;            (c   (my-peek-char stream)))
;       (cond
;         ((eof-objectp c) c)
;         ((char=p c #\")
;           ; old readers tend to read strings okay, call it.
;           ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
;           (invoke-read default-scheme-read stream))
;         (t
;           ; attach the source information to the item read-in
;           (attach-sourceinfo pos
;             (cond
;               ((char=p c #\#)
;                 (my-read-char stream)
;                 (let ((rv (process-sharp no-indent-read stream)))
;                   (cond
;                     ((eq (car rv) 'scomment) (no-indent-read stream))
;                     ((eq (car rv) 'normal) (cadr rv))
;                     ((eq (car rv) 'abbrev)
;                       (list (cadr rv) (no-indent-read stream)))
;                     (t   (read-error "Unknown # sequence")))))
;               ((char=p c #\.) (process-period stream))
;               ((or (member c digits) (char=p c #\+) (char=p c #\-))
;                 (let*
;                   ((maybe-number (concatenate 'string
;                      (read-until-delim stream neoteric-delimiters)))
;                    (as-number (string->number maybe-number)))
;                   (if as-number
;                       as-number
;                       (intern (fold-case-maybe stream maybe-number)))))
;               ((char=p c #\')
;                 (my-read-char stream)
;                 (list (attach-sourceinfo pos 'quote)
;                   (no-indent-read stream)))
;               ((char=p c #\`)
;                 (my-read-char stream)
;                 (list (attach-sourceinfo pos (translate-cl 'quasiquote))
;                   (no-indent-read stream)))
;               ((char=p c #\,)
;                 (my-read-char stream)
;                   (cond
;                     ((char=p #\@ (my-peek-char stream))
;                       (my-read-char stream)
;                       (list (attach-sourceinfo pos
;                                (translate-cl 'unquote-splicing))
;                        (no-indent-read stream)))
;                    (t
;                     (list (attach-sourceinfo pos (translate-cl 'unquote))
;                       (no-indent-read stream)))))
;               ((char=p c #\( )
;                   (my-read-char stream)
;                   (my-read-delimited-list no-indent-read #\) stream))
;               ((char=p c #\) )
;                 (my-read-char stream)
;                 (read-error "Closing parenthesis without opening")
;                 (underlying-read no-indent-read stream))
;               ((char=p c #\[ )
;                   (my-read-char stream)
;                   (my-read-delimited-list no-indent-read #\] stream))
;               ((char=p c #\] )
;                 (my-read-char stream)
;                 (read-error "Closing bracket without opening")
;                 (underlying-read no-indent-read stream))
;               ((char=p c #\} )
;                 (my-read-char stream)
;                 (read-error "Closing brace without opening")
;                 (underlying-read no-indent-read stream))
;               ((char=p c #\| )
;                 ; Read |...| symbol (like Common Lisp and R7RS draft 9)
;                 (get-barred-symbol stream))
;               (t ; Nothing else.  Must be a symbol start.
;                 (intern (fold-case-maybe stream
;                   (concatenate 'string
;                     (read-until-delim stream neoteric-delimiters)))))))))))
; 
; ; -----------------------------------------------------------------------------
; ; Curly Infix
; ; -----------------------------------------------------------------------------
; 
;   ; Return true if lyst has an even # of parameters, and the (alternating)
;   ; first parameters are "op".  Used to determine if a longer lyst is infix.
;   ; If passed empty list, returns true (so recursion works correctly).
;   (defun even-and-op-prefixp (op lyst)
;     (cond
;       ((null lyst) t)
;       ((atom lyst) nil)
;       ((not (equal op (car lyst))) nil) ; fail - operators not the same
;       ((not (consp (cdr lyst)))  nil) ; Wrong # of parameters or improper
;       (t   (even-and-op-prefixp op (cddr lyst))))) ; recurse.
; 
;   ; Return true if the lyst is in simple infix format
;   ; (and thus should be reordered at read time).
;   (defun simple-infix-listp (lyst)
;     (and
;       (consp lyst)           ; Must have list;  '() doesn't count.
;       (consp (cdr lyst))     ; Must have a second argument.
;       (consp (cddr lyst))    ; Must have a third argument (we check it
;                              ; this way for performance)
;       (even-and-op-prefixp (cadr lyst) (cdr lyst)))) ; true if rest is simple
; 
;   ; Return alternating parameters in a list (1st, 3rd, 5th, etc.)
;   (defun alternating-parameters (lyst)
;     (if (or (null lyst) (null (cdr lyst)))
;         lyst
;         (cons (car lyst) (alternating-parameters (cddr lyst)))))
; 
;   ; Not a simple infix list - transform it.  Written as a separate procedure
;   ; so that future experiments or SRFIs can easily replace just this piece.
;   (defun transform-mixed-infix (lyst)
;      (cons '$nfx$ lyst))
; 
;   ; Given curly-infix lyst, map it to its final internal format.
;   (defun process-curly (lyst)
;     (cond
;      ((atom lyst) lyst) ; E.G., map {} to ().
;      ((null (cdr lyst)) ; Map {a} to a.
;        (car lyst))
;      ((and (consp (cdr lyst)) (null (cddr lyst))) ; Map {a b} to (a b).
;        lyst)
;      ((simple-infix-listp lyst) ; Map {a OP b [OP c...]} to (OP a b [c...])
;        (cons (cadr lyst) (alternating-parameters lyst)))
;      (t  (transform-mixed-infix lyst))))
; 
; 
;   (defun curly-infix-read-real (no-indent-read stream)
;     (let* ((pos (get-sourceinfo stream))
;             (c   (my-peek-char stream)))
;       (cond
;         ((eof-objectp c) c)
;         ((eql c #\;)
;           (consume-to-eol stream)
;           (curly-infix-read-real no-indent-read stream))
;         ((my-char-whitespacep c)
;           (my-read-char stream)
;           (curly-infix-read-real no-indent-read stream))
;         ((eql c #\{)
;           (my-read-char stream)
;           ; read in as infix
;           (attach-sourceinfo pos
;             (process-curly
;               (my-read-delimited-list neoteric-read-real #\} stream))))
;         (t
;           (underlying-read no-indent-read stream)))))
; 
;   ; Read using curly-infix-read-real
;   (defun curly-infix-read-nocomment (stream)
;     (curly-infix-read-real curly-infix-read-nocomment stream))
; 
; ; -----------------------------------------------------------------------------
; ; Neoteric Expressions
; ; -----------------------------------------------------------------------------
; 
;   ; Implement neoteric-expression's prefixed (), [], and {}.
;   ; At this point, we have just finished reading some expression, which
;   ; MIGHT be a prefix of some longer expression.  Examine the next
;   ; character to be consumed; if it's an opening paren, bracket, or brace,
;   ; then the expression "prefix" is actually a prefix.
;   ; Otherwise, just return the prefix and do not consume that next char.
;   ; This recurses, to handle formats like f(x)(y).
;   (defun neoteric-process-tail (stream prefix)
;       (let* ((pos (get-sourceinfo stream))
;              (c   (my-peek-char stream)))
;         (cond
;           ((eof-objectp c) prefix)
;           ((char=p c #\( ) ; Implement f(x)
;             (my-read-char stream)
;             (neoteric-process-tail stream (attach-sourceinfo pos (cons prefix
;                   (my-read-delimited-list neoteric-read-nocomment #\) stream)))))
;           ((char=p c #\[ )  ; Implement f[x]
;             (my-read-char stream)
;             (neoteric-process-tail stream (attach-sourceinfo pos
;               (cons (attach-sourceinfo pos '$bracket-apply$)
;                 (cons prefix
;                   (my-read-delimited-list neoteric-read-nocomment #\] stream))))))
;           ((char=p c #\{ )  ; Implement f{x}
;             (my-read-char stream)
;             (neoteric-process-tail stream (attach-sourceinfo pos
;               (let
;                 ((tail (process-curly
;                    (my-read-delimited-list neoteric-read-nocomment #\} stream))))
;                 (if (eql tail '())
;                     (list prefix) ; Map f{} to (f), not (f ()).
;                     (list prefix tail))))))
;           (t prefix))))
; 
; 
;   ; This is the "real" implementation of neoteric-read.
;   ; It directly implements unprefixed (), [], and {} so we retain control;
;   ; it calls neoteric-process-tail so f(), f[], and f{} are implemented.
;   ;  (if (eof-objectp (my-peek-char stream))
;   (defun neoteric-read-real (stream)
;     (let*
;       ((pos (get-sourceinfo stream))
;        (c   (my-peek-char stream))
;        (result
;          (cond
;            ((eof-objectp c) c)
;            ((char=p c #\( )
;              (my-read-char stream)
;              (attach-sourceinfo pos
;                (my-read-delimited-list neoteric-read-nocomment #\) stream)))
;            ((char=p c #\[ )
;              (my-read-char stream)
;              (attach-sourceinfo pos
;                (my-read-delimited-list neoteric-read-nocomment #\] stream)))
;            ((char=p c #\{ )
;              (my-read-char stream)
;              (attach-sourceinfo pos
;                (process-curly
;                  (my-read-delimited-list neoteric-read-nocomment #\} stream))))
;            ((my-char-whitespacep c)
;              (my-read-char stream)
;              (neoteric-read-real stream))
;            ((eql c #\;)
;              (consume-to-eol stream)
;              (neoteric-read-real stream))
;            (t (underlying-read neoteric-read-nocomment stream)))))
;       (if (eof-objectp result)
;           result
;           (neoteric-process-tail stream result))))
; 
;   (defun neoteric-read-nocomment (stream)
;     (neoteric-read-real stream))
; 
; ; -----------------------------------------------------------------------------
; ; Sweet Expressions (this implementation maps to the BNF)
; ; -----------------------------------------------------------------------------
; 
;   ; There is no standard Scheme mechanism to unread multiple characters.
;   ; Therefore, the key productions and some of their supporting procedures
;   ; return both the information on what ended their reading process,
;   ; as well the actual value (if any) they read before whatever stopped them.
;   ; That way, procedures can process the value as read, and then pass on
;   ; the ending information to whatever needs it next.  This approach,
;   ; which we call a "non-tokenizing" implementation, implements a tokenizer
;   ; via procedure calls instead of needing a separate tokenizer.
;   ; The ending information can be:
;   ; - "stopper" - this is returned by productions etc. that do NOT
;   ;     read past the of a line (outside of paired characters and strings).
;   ;     It is 'normal if it ended normally (e.g., at end of line); else it's
;   ;     'sublist-marker ($), 'group-split-marker (\\), 'collecting (<*),
;   ;     'collecting-end (*>), 'scomment (special comments like #|...|#), or
;   ;     'abbrevw (initial abbreviation with whitespace after it).
;   ; - "new-indent" - this is returned by productions etc. that DO read
;   ;     past the end of a line.  Such productions typically read the
;   ;     next line's indent to determine if they should return.
;   ;     If they should, they return the new indent so callers can
;   ;     determine what to do next.  A "*>" should return even though its
;   ;     visible indent level is length 0; we handle this by prepending
;   ;     all normal indents with "^", and "*>" generates a length-0 indent
;   ;     (which is thus shorter than even an indent of 0 characters).
; 
;   (setq group-split (intern "\\\\"))
;   (setq group-split-char #\\ ) ; First character of split symbol.
;   (setq non-whitespace-indent #\!) ; Non-whitespace-indent char.
;   (setq sublist (intern "$"))
;   (setq sublist-char #\$) ; First character of sublist symbol.
;   (setq period-symbol period-symbol)
; 
;   (defun indentation>p (indentation1 indentation2)
;     (let ((len1 (string-length indentation1))
;             (len2 (string-length indentation2)))
;       (and (> len1 len2)
;              (string= indentation2 (substring indentation1 0 len2)))))
; 
; 
;   ; Return t if char is space or tab.
;   (defun char-hspacep (char)
;     (or (eql char #\space)
;         (eql char tab)))
; 
;   ; Consume 0+ spaces or tabs
;   (defun hspaces (stream)
;     (cond ; Use "cond" as "when" for portability.
;       ((char-hspacep (my-peek-char stream))
;         (my-read-char stream)
;         (hspaces stream))))
; 
;   ; Return t if char is space, tab, or !
;   (defun char-icharp (char)
;     (or (eql char #\space)
;         (eql char tab)
;         (eql char non-whitespace-indent)))
; 
;   (defun accumulate-ichar (stream)
;     (if (char-icharp (my-peek-char stream))
;         (cons (my-read-char stream) (accumulate-ichar stream))
;         '()))
; 
;   (defun consume-ff-vt (stream)
;     (let ((c (my-peek-char stream)))
;       (cond
;         ((or (eql c form-feed) (eql c vertical-tab))
;           (my-read-char stream)
;           (consume-ff-vt stream)))))
; 
;   ; Do 2-item append, but report read-error if the LHS is not a proper list.
;   ; Don't use this if the lhs *must* be a list (e.g., if we have (list x)).
;   (defun my-append (lhs rhs)
;     (if (listp lhs)
;         (append lhs rhs)
;         (read-error "Must have proper list on left-hand-side to append data")))
; 
;   ; Read an n-expression.  Returns ('scomment '()) if it's an scomment,
;   ; else returns ('normal n-expr).
;   ; Note: If a *value* begins with #, process any potential neoteric tail,
;   ; so weird constructs beginning with "#" like nil() will still work.
;   (defun n-expr-or-scomment (stream)
;     (if (eql (my-peek-char stream) #\#)
;         (let* ((consumed-sharp (my-read-char stream))
;                (result (process-sharp neoteric-read-nocomment stream)))
;           (cond
;             ((eq (car result) 'normal)
;               (list 'normal (neoteric-process-tail stream (cadr result))))
;             ((eq (car result) 'abbrev)
;               (list 'normal
;                 (list (cadr result) (neoteric-read-nocomment stream))))
;             ((consp result) result)
;             (t (read-error "Unsupported hash"))))
;         (list 'normal (neoteric-read-nocomment stream))))
; 
;   ; Read an n-expression.  Returns ('normal n-expr) in most cases;
;   ; if it's a special marker, the car is the marker name instead of 'normal.
;   ; Markers only have special meaning if their first character is
;   ; the "normal" character, e.g., {$} is not a sublist.
;   ; Call "process-sharp" if first char is "#".
;   (defun n-expr (stream)
;     (let* ((c (my-peek-char stream))
;            (results (n-expr-or-scomment stream))
;            (type (car results))
;            (expr (cadr results)))
;       (if (eq (car results) 'scomment)
;           results
;           (cond
;             ((and (eq expr sublist) (eql c sublist-char))
;               (list 'sublist-marker '()))
;             ((and (eq expr group-split) (eql c group-split-char))
;               (list 'group-split-marker '()))
;             ((and (eq expr '<*) (eql c #\<))
;               (list 'collecting '()))
;             ((and (eq expr '*>) (eql c #\*))
;               (list 'collecting-end '()))
;             ((and (eq expr '$$$) (eql c #\$))
;               (read-error "Error - $$$ is reserved"))
;             ((and (eq expr period-symbol) (eql c #\.))
;               (list 'period-marker '()))
;             (t
;               results)))))
; 
;   ; Check if we have abbrev+whitespace.  If the current peeked character
;   ; is one of certain whitespace chars,
;   ; return 'abbrevw as the marker and abbrev-procedure
;   ; as the value (the cadr). Otherwise, return ('normal n-expr).
;   ; We do NOT consume the peeked char (so EOL can be examined later).
;   ; Note that this calls the neoteric-read procedure directly, because
;   ; quoted markers are no longer markers. E.G., '$ is just (quote $).
;   (defun maybe-initial-abbrev (stream abbrev-procedure)
;     (let ((c (my-peek-char stream)))
;       (if (or (char-hspacep c) (eql c carriage-return) (eql c linefeed))
;           (list 'abbrevw abbrev-procedure)
;           (list 'normal
;             (list abbrev-procedure (neoteric-read-nocomment stream))))))
; 
;   ; Read the first n-expr on a line; handle abbrev+whitespace specially.
;   ; Returns ('normal VALUE) in most cases.
;   (defun n-expr-first (stream)
;     (case (my-peek-char stream)
;       ((#\') 
;         (my-read-char stream)
;         (maybe-initial-abbrev stream 'quote))
;       ((#\`) 
;         (my-read-char stream)
;         (maybe-initial-abbrev stream (translate-cl 'quasiquote)))
;       ((#\,) 
;         (my-read-char stream)
;         (if (eql (my-peek-char stream) #\@)
;             (progn
;               (my-read-char stream)
;               (maybe-initial-abbrev stream (translate-cl 'unquote-splicing)))
;             (maybe-initial-abbrev stream (translate-cl 'unquote))))
;       ((#\#) 
;         (let* ((consumed-sharp (my-read-char stream))
;                (result (process-sharp neoteric-read-nocomment stream)))
;           (cond
;             ((eq (car result) 'normal)
;               (list 'normal (neoteric-process-tail stream (cadr result))))
;             ((eq (car result) 'abbrev)
;               (maybe-initial-abbrev stream (cadr result)))
;             (t result))))
;       (else
;         (n-expr stream))))
; 
;   ; Consume ;-comment (if there), consume EOL, and return new indent.
;   ; Skip ;-comment-only lines; a following indent-only line is empty.
;   (defun get-next-indent (stream)
;     (consume-to-eol stream)
;     (consume-end-of-line stream)
;     (let* ((indentation-as-list (cons #\^ (accumulate-ichar stream)))
;            (c (my-peek-char stream)))
;       (cond
;         ((eql c #\;)  ; A ;-only line, consume and try again.
;           (get-next-indent stream))
;         ((lcomment-eolp c) ; Indent-only line
;           (if (member #\! indentation-as-list)
;               (read-error "Ending indentation-only line must not use '!'")
;               "^"))
;         (t (concatenate 'string indentation-as-list)))))
; 
;   ; Utility function:
;   ; If x is a 1-element list, return (car x), else return x
;   (defun monify (x)
;     (cond
;       ((atom x) x)
;       ((null (cdr x)) (car x))
;       (t x)))
; 
;   ; Return contents (value) of collecting-tail.  It does *not* report a
;   ; stopper or ending indent, because it is *ONLY* stopped by collecting-end
;   (defun collecting-tail (stream)
;     (let* ((c (my-peek-char stream)))
;       (cond
;         ((eof-objectp c)
;          (read-error "Collecting tail: EOF before collecting list ended"))
;         ((lcomment-eolp c)
;           (consume-to-eol stream)
;           (consume-end-of-line stream)
;           (collecting-tail stream))
;         ((char-icharp c)
;           (let* ((indentation (accumulate-ichar stream))
;                  (c (my-peek-char stream)))
;             (cond
;               ((eql c #\;)
;                 (collecting-tail stream))
;               ((lcomment-eolp c)
;                 (if (member #\! indentation)
;                     (read-error "Collecting tail: False empty line with !")
;                     (collecting-tail stream)))
;               (t
;                 (read-error "Collecting tail: Only ; after indent")))))
;         ((or (eql c form-feed) (eql c vertical-tab))
;           (consume-ff-vt stream)
;           (if (lcomment-eolp (my-peek-char stream))
;               (collecting-tail stream)
;               (read-error "Collecting tail: FF and VT must be alone on line")))
;         (t
;           (let* ((it-full-results (it-expr stream "^"))
;                  (it-new-indent   (car it-full-results))
;                  (it-value        (cadr it-full-results)))
;             (cond
;               ((string= it-new-indent "")
;                 ; Specially compensate for "*>" at the end of a line if it's
;                 ; after something else.  This must be interpreted as EOL *>,
;                 ; which would cons a () after the result.
;                 ; Directly calling list for a non-null it-value has
;                 ; the same effect, but is a lot quicker and simpler.
;                 (if (null it-value)
;                     it-value
;                     (list it-value)))
;               (t (cons it-value (collecting-tail stream)))))))))
; 
;   ; Skip scomments and error out if we have a normal n-expr;
;   ; Basically implement this BNF:
;   ;    (scomment hspace*)* (n-expr error)?
;   ; This procedure is used after ". value".
;   (defun n-expr-error (stream full)
;     (if (not (eq (car full) 'normal))
;         (read-error "BUG! n-expr-error called but stopper not normal"))
;     (if (lcomment-eolp (my-peek-char stream))
;         full ; All done!
;         (let* ((n-full-results (n-expr stream))
;                (n-stopper      (car n-full-results))
;                (n-value        (cadr n-full-results)))
;           (cond
;             ((eq n-stopper 'scomment) ; Consume scomments.
;               (hspaces stream)
;               (n-expr-error stream full))
;             ((eq n-stopper 'normal)
;               (read-error "Illegal second value after ."))
;             (t ; We found a stopper, return it with the value from "full"
;               (list n-stopper (cadr full)))))))
; 
;   ; Returns (stopper value-after-period)
;   (defun post-period (stream)
;     (if (not (lcomment-eolp (my-peek-char stream)))
;         (let* ((pn-full-results (n-expr stream))
;                (pn-stopper      (car pn-full-results))
;                (pn-value        (cadr pn-full-results)))
;           (cond
;             ((eq pn-stopper 'scomment)
;               (hspaces stream)
;               (post-period stream))
;             ((eq pn-stopper 'normal)
;               (hspaces stream)
;               (n-expr-error stream pn-full-results))
;             ((eq pn-stopper 'collecting)
;               (hspaces stream)
;               (let ((ct (collecting-tail stream)))
;                 (hspaces stream)
;                 (n-expr-error stream (list 'normal ct))))
;             ((eq pn-stopper 'period-marker)
;               (list 'normal period-symbol))
;             (t ; Different stopper; respond as empty branch with that stopper
;               (list pn-stopper (list period-symbol)))))
;         (list 'normal period-symbol))) ; Empty branch.
; 
;   ; Returns (stopper computed-value).
;   ; The stopper may be 'normal, 'scomment (special comment),
;   ; 'abbrevw (initial abbreviation), 'sublist-marker, or 'group-split-marker
;   (defun head (stream)
;     (let* ((basic-full-results (n-expr-first stream))
;            (basic-special      (car basic-full-results))
;            (basic-value        (cadr basic-full-results)))
;       (cond
;         ((eq basic-special 'collecting)
;           (hspaces stream)
;           (let* ((ct-results (collecting-tail stream)))
;             (hspaces stream)
;             (if (not (lcomment-eolp (my-peek-char stream)))
;                 (let* ((rr-full-results (rest stream))
;                        (rr-stopper      (car rr-full-results))
;                        (rr-value        (cadr rr-full-results)))
;                   (list rr-stopper (cons ct-results rr-value)))
;                 (list 'normal (list ct-results)))))
;         ((eq basic-special 'period-marker)
;           (if (char-hspacep (my-peek-char stream))
;               (progn
;                 (hspaces stream)
;                 (let* ((ct-full-results (post-period stream))
;                        (ct-stopper      (car ct-full-results))
;                        (ct-value        (cadr ct-full-results)))
;                   (list ct-stopper (list ct-value))))
;               (list 'normal (list period-symbol))))
;         ((not (eq basic-special 'normal)) basic-full-results)
;         ((char-hspacep (my-peek-char stream))
;           (hspaces stream)
;           (if (not (lcomment-eolp (my-peek-char stream)))
;               (let* ((br-full-results (rest stream))
;                      (br-stopper      (car br-full-results))
;                      (br-value        (cadr br-full-results)))
;                 (list br-stopper (cons basic-value br-value)))
;               (list 'normal (list basic-value))))
;         (t 
;           (list 'normal (list basic-value))))))
; 
;   ; Returns (stopper computed-value); stopper may be 'normal, etc.
;   ; Read in one n-expr, then process based on whether or not it's special.
;   (defun rest (stream)
;     (let* ((basic-full-results (n-expr stream))
;            (basic-special      (car basic-full-results))
;            (basic-value        (cadr basic-full-results)))
;       (cond
;         ((eq basic-special 'scomment)
;           (hspaces stream)
;           (if (not (lcomment-eolp (my-peek-char stream)))
;               (rest stream)
;               (list 'normal '())))
;         ((eq basic-special 'collecting)
;           (hspaces stream)
;           (let* ((ct-results (collecting-tail stream)))
;             (hspaces stream)
;             (if (not (lcomment-eolp (my-peek-char stream)))
;                 (let* ((rr-full-results (rest stream))
;                        (rr-stopper      (car rr-full-results))
;                        (rr-value        (cadr rr-full-results)))
;                   (list rr-stopper (cons ct-results rr-value)))
;                 (list 'normal (list ct-results)))))
;         ((eq basic-special 'period-marker)
;           (if (char-hspacep (my-peek-char stream))
;               (progn
;                 (hspaces stream)
;                 (post-period stream))
;               (list 'normal (list period-symbol))))
;         ((not (eq basic-special 'normal)) (list basic-special '())) 
;         ((char-hspacep (my-peek-char stream))
;           (hspaces stream)
;           (if (not (lcomment-eolp (my-peek-char stream)))
;               (let* ((br-full-results (rest stream))
;                      (br-stopper      (car br-full-results))
;                      (br-value        (cadr br-full-results)))
;                 (list br-stopper (cons basic-value br-value)))
;               (list 'normal (list basic-value))))
;         (t (list 'normal (list basic-value))))))
; 
;   ; Returns (new-indent computed-value)
;   (defun body (stream starting-indent)
;     (let* ((i-full-results (it-expr stream starting-indent))
;            (i-new-indent   (car i-full-results))
;            (i-value        (cadr i-full-results)))
;       (if (string= starting-indent i-new-indent)
;           (if (eq i-value period-symbol)
;               (let* ((f-full-results (it-expr stream i-new-indent))
;                      (f-new-indent   (car f-full-results))
;                      (f-value        (cadr f-full-results)))
;                 (if (not (indentation>p starting-indent f-new-indent))
;                     (read-error "Dedent required after lone . and value line"))
;                 (list f-new-indent f-value)) ; final value of improper list
;               (let* ((nxt-full-results (body stream i-new-indent))
;                      (nxt-new-indent   (car nxt-full-results))
;                      (nxt-value        (cadr nxt-full-results)))
;                 (list nxt-new-indent (cons i-value nxt-value))))
;           (list i-new-indent (list i-value))))) ; dedent - end list.
; 
;   ; Returns (new-indent computed-value)
;   (defun it-expr-real (stream starting-indent)
;     (let* ((head-full-results (head stream))
;            (head-stopper      (car head-full-results))
;            (head-value        (cadr head-full-results)))
;       (if (and (not (null head-value)) (not (eq head-stopper 'abbrevw)))
;           ; The head... branches:
;           (cond
;             ((eq head-stopper 'group-split-marker)
;               (hspaces stream)
;               (if (lcomment-eolp (my-peek-char stream))
;                   (read-error "Cannot follow split with end of line")
;                   (list starting-indent (monify head-value))))
;             ((eq head-stopper 'sublist-marker)
;               (hspaces stream)
;               (if (lcomment-eolp (my-peek-char stream))
;                   (read-error "EOL illegal immediately after sublist"))
;               (let* ((sub-i-full-results (it-expr stream starting-indent))
;                      (sub-i-new-indent   (car sub-i-full-results))
;                      (sub-i-value        (cadr sub-i-full-results)))
;                 (list sub-i-new-indent
;                   (my-append head-value (list sub-i-value)))))
;             ((eq head-stopper 'collecting-end)
;               ; Note that indent is "", forcing dedent all the way out.
;               (list "" (monify head-value)))
;             ((lcomment-eolp (my-peek-char stream))
;               (let ((new-indent (get-next-indent stream)))
;                 (if (indentation>p new-indent starting-indent)
;                     (let* ((body-full-results (body stream new-indent))
;                            (body-new-indent (car body-full-results))
;                            (body-value      (cadr body-full-results)))
;                       (list body-new-indent (my-append head-value body-value)))
;                     (list new-indent (monify head-value)))))
;             (t
;               (read-error "Must end line with end-of-line sequence")))
;           ; Here, head begins with something special like GROUP-SPLIT:
;           (cond
;             ((or (eq head-stopper 'group-split-marker)
;                  (eq head-stopper 'scomment))
;               (hspaces stream)
;               (if (not (lcomment-eolp (my-peek-char stream)))
;                   (it-expr stream starting-indent) ; Skip and try again.
;                   (let ((new-indent (get-next-indent stream)))
;                     (cond
;                       ((indentation>p new-indent starting-indent)
;                         (body stream new-indent))
;                       ((string= starting-indent new-indent)
;                         (if (not (lcomment-eolp (my-peek-char stream)))
;                           (it-expr stream new-indent)
;                           (list new-indent (t-expr stream)))) ; Restart
;                       (t
;                         (read-error "GROUP-SPLIT EOL DEDENT illegal"))))))
;             ((eq head-stopper 'sublist-marker)
;               (hspaces stream)
;               (if (lcomment-eolp (my-peek-char stream))
;                   (read-error "EOL illegal immediately after solo sublist"))
;               (let* ((is-i-full-results (it-expr stream starting-indent))
;                      (is-i-new-indent   (car is-i-full-results))
;                      (is-i-value        (cadr is-i-full-results)))
;                 (list is-i-new-indent
;                   (list is-i-value))))
;             ((eq head-stopper 'abbrevw)
;               (hspaces stream)
;               (if (lcomment-eolp (my-peek-char stream))
;                   (progn
;                     (let ((new-indent (get-next-indent stream)))
;                       (if (not (indentation>p new-indent starting-indent))
;                           (read-error "Indent required after abbreviation"))
;                       (let* ((ab-full-results (body stream new-indent))
;                              (ab-new-indent   (car ab-full-results))
;                              (ab-value      (cadr ab-full-results)))
;                         (list ab-new-indent
;                           (append (list head-value) ab-value)))))
;                   (let* ((ai-full-results (it-expr stream starting-indent))
;                          (ai-new-indent (car ai-full-results))
;                          (ai-value    (cadr ai-full-results)))
;                     (list ai-new-indent
;                       (list head-value ai-value)))))
;             ((eq head-stopper 'collecting-end)
;               (list "" head-value))
;             (t 
;               (read-error "Initial head error"))))))
; 
;   ; Read it-expr.  This is a wrapper that attaches source info
;   ; and checks for consistent indentation results.
;   (defun it-expr (stream starting-indent)
;     (let* ((pos (get-sourceinfo stream))
;            (results (it-expr-real stream starting-indent))
;            (results-indent (car results))
;            (results-value (cadr results)))
;       (if (indentation>p results-indent starting-indent)
;           (read-error "Inconsistent indentation"))
;       (list results-indent (attach-sourceinfo pos results-value))))
; 
;   ; Top level - read a sweet-expression (t-expression).  Handle special
;   ; cases, such as initial indent; call it-expr for normal case.
;   (defun t-expr (stream)
;     (let* ((c (my-peek-char stream)))
;       ; Check EOF early (a bug in guile before 2.0.8 consumes EOF on peek)
;       (if (eof-objectp c)
;           c
;           (cond
;             ((lcomment-eolp c)
;               (consume-to-eol stream)
;               (consume-end-of-line stream)
;               (t-expr stream))
;             ((or (eql c form-feed) (eql c vertical-tab))
;               (consume-ff-vt stream)
;               (t-expr stream))
;             ((char-icharp c)
;               (let ((indentation-list (cons #\^ (accumulate-ichar stream))))
;                 (if (not (member (my-peek-char stream) initial-comment-eol))
;                     (let ((results (n-expr-or-scomment stream)))
;                       (if (not (eq (car results) 'scomment))
;                           (cadr results) ; Normal n-expr, return one value.
;                           (progn ; We have an scomment; skip and try again.
;                             (hspaces stream)
;                             (t-expr stream))))
;                     (progn ; Indented comment-eol, consume and try again.
;                       (if (member #\! indentation-list)
;                           (read-error "Empty line with '!'"))
;                       (consume-to-eol stream)
;                       (consume-end-of-line stream)
;                       (t-expr stream)))))
;             (t
;               (let* ((results (it-expr stream "^"))
;                      (results-indent (car results))
;                      (results-value (cadr results)))
;                 (if (string= results-indent "")
;                     (read-error "Closing *> without preceding matching <*")
;                     results-value)))))))
; 
;   ; Skip until we find a completely empty line (not even initial space/tab).
;   ; We use this after read error to resync to good input.
;   (defun read-to-blank-line (stream)
;     (consume-to-eol stream)
;     (consume-end-of-line stream)
;     (let* ((c (my-peek-char stream)))
;       (if (not (or (eof-objectp c) (char-line-endingp c)))
;         (read-to-blank-line stream))))
; 
;   ; Call on sweet-expression reader - use guile's nonstandard catch/throw
;   ; so that errors will force a restart.
;   (defun t-expr-catch (stream)
; 
;     ; Default guile stack size is FAR too small
;     (debug-set! stack 500000)
; 
;     (catch 'readable
;       (lambda () (t-expr stream))
;       (lambda (key . args) (read-to-blank-line stream) (t-expr-catch stream))))
; 
; ; -----------------------------------------------------------------------------
; ; Write routines
; ; -----------------------------------------------------------------------------
; 
;   ; A list with more than this length and no pairs is considered "boring",
;   ; and thus is presumed to NOT be a procedure call or execution sequence.
;   (setq boring-length 16)
; 
;   (setq special-infix-operators
;     '(and or xor))
; 
;   (setq punct-chars
;     (list #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\-
;           #\.  #\/ #\: #\; #\< #\= #\> #\p #\@ #\[ #\\ #\] #\^
;           #\- #\` #\{ #\| #\} #\~))
; 
;   ; Returns t if x is a list with exactly 1 element.  Improper lists are nil.
;   (defun list1p (x)
;     (and (consp x) (null (cdr x))))
; 
;   ; Returns t if x is a list with exactly 2 elements.  Improper lists are nil.
;   (defun list2p (x)
;     (and (consp x) (consp (cdr x)) (null (cddr x))))
; 
;   ; Does x contain a list of ONLY punctuation characters?
;   ; An empty list is considered true.
;   (defun contains-only-punctuationp (x)
;     (cond ((null x) t)
;           ((atom x) nil)
;           ((memq (car x) punct-chars)
;            (contains-only-punctuationp (cdr x)))
;           (t nil)))
; 
;   ; Returns t if x is a symbol that would typically be used in infix position.
;   (defun is-infix-operatorp (x)
;     (cond ((not (symbolp x)) nil)
;           ((memq x special-infix-operators) t)
;           (t
;            (contains-only-punctuation?
;              (string->list (symbol-name x))))))
; 
;   ; A possibly-improper list is long and boring if its length is at least
;   ; num-to-go long and it's boring (it contains no pairs up to that length).
;   ; A long-and-boring list is almost certainly NOT a function call or a
;   ; body of some executable sequence - it's almost certainly a long
;   ; boring list of data instead. If it is, we want to display it differently.
;   ; This doesn't get stuck on circular lists; it always terminates after
;   ; num-to-go iterations.
;   (defun long-and-boringp (x num-to-go)
;     (cond
;       ((consp (car x)) nil)
;       ((not (consp (cdr x))) nil)
;       ((<= num-to-go 1) t)
;       (t (long-and-boringp (cdr x) (- num-to-go 1)))))
; 
;   (defun list-no-longer-thanp (x num-to-go)
;     (cond
;       ((atom x) nil)
;       ((null (cdr x)) t) ; This is the last one!
;       ((not (consp (cdr x))) nil)
;       ((<= num-to-go 0) nil)
;       (t (list-no-longer-thanp (cdr x) (- num-to-go 1)))))
; 
;   ; Return t if x should be represented using curly-infix notation {...}.
;   (defun represent-as-infixp (x)
;     (and (consp x)
;          (consp (cdr x))                ; At least 2 elements.
;          (is-infix-operatorp (car x))
;          (list-no-longer-thanp x 6)))
; 
;   (defun represent-as-inline-infixp (x)
;     (and (represent-as-infixp x) (not (list2p x)))) ; Must be 3+ elements
; 
;   ; Return t if x should be represented as a brace suffix
;   (defun represent-as-brace-suffixp (x)
;     (represent-as-infixp x))
; 
;   ; Define an association list mapping the Scheme procedure names which have
;   ; abbreviations ==> the list of characters in their abbreviation
;   (setq abbreviations
;     '((quote (#\'))
;       (quasiquote (#\`))
;       (unquote (#\,))
;       (unquote-splicing (#\, #\@))
;       ; Scheme syntax-rules. Note that this will abbreviate any 2-element
;       ; list whose car is "syntax", whether you want that or not!
;       (syntax  (#\# #\'))
;       (quasisyntax (#\# #\`))
;       (unsyntax-splicing (#\# #\, #\@)
;       (unsyntax (#\# #\,)))))
; 
;   ; return t if we should as a traditional abbreviation, e.g., '
;   (defun represent-as-abbreviationp (x)
;     (and (list2p x)
;          (assq (car x) abbreviations)))
; 
;   ; The car(x) is the symbol for an abbreviation; write the abbreviation.
;   (defun write-abbreviation (x stream)
;     (mapc (lambda (c) (princ c stream))
;       (cadr (assq (car x) abbreviations))))
; 
; 
;   ; Return list x's *contents* represented as a list of characters.
;   ; Each one must use neoteric-expressions, space-separated;
;   ; it will be surrounded by (...) so no indentation processing is relevant.
;   (defun n-write-list-contents (x stream)
;     (cond
;       ((null x) (values))
;       ((consp x)
;         (n-write-simple (car x) stream)
;         (cond ((not (null (cdr x)))
;           (princ " " stream)
;           (n-write-list-contents (cdr x) stream))))
;       (t
;         (princ ". " stream)
;         (n-write-simple x stream))))
; 
;   (defun c-write-list-contents (x stream)
;     (cond
;       ((null x) (values))
;       ((consp x)
;         (c-write-simple (car x) stream)
;         (cond ((not (null (cdr x)))
;           (princ " " stream)
;           (c-write-list-contents (cdr x) stream))))
;       (t
;         (princ ". " stream)
;         (c-write-simple x stream))))
; 
;   ; Return tail of an infix expression, as list of chars
;   ; The "op" is the infix operator represented as a list of chars.
;   (defun infix-tail (op x stream)
;     (cond
;       ((null x) (princ "}" stream))
;       ((consp x)
;         (princ " " stream)
;         (n-write-simple op stream)
;         (princ " " stream)
;         (n-write-simple (car x) stream)
;         (infix-tail op (cdr x) stream))
;       (t
;         (princ " " stream)
;         (n-write-simple x stream)
;         (princ "}" stream))))
; 
;   ; Return "x" as a list of characters, surrounded by {...}, for use as f{...}.
;   (defun as-brace-suffix (x stream)
;     (princ "{" stream)
;     (if (list2p x)
;       (progn
;         (n-write-list-contents x stream)
;         (princ "}" stream))
;       (progn
;         (n-write-simple (cadr x) stream)
;         (infix-tail (car x) (cddr x) stream))))
; 
;   (defun n-write-simple (x stream)
;     (cond
;       ((consp x)
;         (cond
;           ((represent-as-abbreviationp x)              ; Format 'x
;             (write-abbreviation x stream)
;             (n-write-simple (cadr x) stream))
;           ((long-and-boringp x boring-length)          ; Format (a b c ...)
;             (princ "(" stream)
;             (n-write-list-contents x stream)
;             (princ ")" stream))
;           ((symbolp (car x))
;             (cond
;               ((represent-as-inline-infixp x)          ; Format {a + b}
;                 (princ "{" stream)
;                 (n-write-simple (cadr x) stream)
;                 (infix-tail (car x) (cddr x) stream))
;               ((and (list1p (cdr x))                   ; Format f{...}
;                 (consp (cadr x))
;                 (represent-as-infixp (cadr x)))
;                   (n-write-simple (car x) stream)
;                   (as-brace-suffix (cadr x) stream))
;               (t                                      ; Format f(...)
;                 (n-write-simple (car x) stream)
;                 (princ "(" stream)
;                 (n-write-list-contents (cdr x) stream)
;                 (princ ")" stream))))
;           (t                                          ; Format (1 2 3 ...)
;             (princ "(" stream)
;             (n-write-list-contents x stream)
;             (princ ")" stream))))
;       ((vectorp x)
;         (princ "#( " stream) ; Surround with spaces, easier to implement.
;         (mapc (lambda (v) (n-write-simple v stream) (princ " " stream))
;           (vector->list x))
;         (princ ")" stream))
;       (t (write x stream))))                            ; Default format.
; 
; 
;   (defun c-write-simple (x stream)
;     (cond
;       ((consp x)
;         (cond
;           ((represent-as-abbreviationp x)              ; Format 'x
;             (write-abbreviation x stream)
;             (c-write-simple (cadr x) stream))
;           ((represent-as-inline-infixp x)              ; Format {a + b}
;             (princ "{" stream)
;             (n-write-simple (cadr x) stream)
;             (infix-tail (car x) (cddr x) stream))
;           (t                                          ; Format (1 2 3 ...)
;             (princ "(" stream)
;             (c-write-list-contents x stream)
;             (princ ")" stream))))
;       ((vectorp x)
;         (princ "#( " stream) ; Surround with spaces, easier to implement.
;         (mapc (lambda (v) (c-write-simple v stream) (princ " " stream))
;           (vector->list x))
;         (princ ")" stream))
;       (t (write x stream))))                            ; Default format.
; 
; 
;   ; Front entry - Use default stream if none provided.
;   (defun neoteric-write-simple (x . rest)
;     (if (consp rest)
;         (n-write-simple x (car rest))
;         (n-write-simple x (current-output-stream))))
; 
;   ; Front entry - Use default stream if none provided.
;   (defun curly-write-simple (x . rest)
;     (if (consp rest)
;         (c-write-simple x (car rest))
;         (c-write-simple x (current-output-stream))))
; 
; 
