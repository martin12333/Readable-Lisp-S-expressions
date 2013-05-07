;;; sweet.lisp
;;; Implements sweet-expressions from the "readable" approach for Lisp.

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



(cl:in-package :readable)

; We can't portably overide "read" directly, we have to manipulate
; the readtable to implement sweet-expressions.
; This readtable basically redirects EVERY character to a specific procedure,
; effectively taking over "read":
(defvar *sweet-readtable*
  "This table redirects any input to sweet-expression processing")

; The underlying readtable is mostly a neoteric reader.  However,
; we must implement a slightly different underlying reader that
; reads #|...|# and #;datum. The problem is that if the underlying reader
; return no values, e.g., "(values)" - the Common Lisp
; "read" will instantly recurse *outside* of our control to read the next
; datum.  That's the wrong thing to do, because that no-values item might
; be the only thing on the line, and in that case it should
; operate as a placeholder for that indentation position.
; Thus, we'll specially wrap such cases and return a
; distinctive cons value "empty-values", to represent the "no value" case.
(defvar *underlying-sweet-readtable*
  "This table is basically neoteric-expressions with some tweaks")

; Work around SBCL nonsense that makes its "defconstant" useless.
; See: http://www.sbcl.org/manual/Defining-Constants.html
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

; Wrapping all character codes up to char-code-limit doesn't really work
; correctly.  This is the max char code that will be wrapped by readable's
; front readtable.
(defvar my-char-code-limit 255)

; These stubs could be used to attach position info
(defun attach-sourceinfo (pos value)
  (declare (ignore pos))
  value)
(defun get-sourceinfo (stream)
  (declare (ignore stream))
  nil)

(defun eof-objectp (c) (eq c my-eof-marker))

(defun string-length (s) (length s))

(define-constant period-symbol '|.|)

(define-constant scomment-result '(scomment ()))

; Marker for empty values.
; Several Common Lisp readtable constructs return nothing using (values),
; but unfortunately when that happens the build-in Common Lisp reader
; performs actions that we can't intercept.  So we'll override the "empty"
; returns with this instead, so that we can override the reader.
(define-constant empty-values (cons 'empty-values-cons nil))

(define-constant vertical-tab (code-char 11)) ; VT is decimal 11.
(define-constant form-feed #\page)            ; FF is decimal 12.

(define-constant whitespace-chars
   (list #\space #\tab #\linefeed #\newline #\return vertical-tab form-feed))

; If t, return |...| symbols as-is, including the vertical bars.
(defvar literal-barred-symbol nil)

(defun my-char-whitespacep (c)
  (member c whitespace-chars))

(define-constant line-ending-chars (list #\newline #\linefeed #\return))
(defun char-line-endingp (char)
  (or
    (eof-objectp char)
    (member char line-ending-chars)))

; Does character "c" begin a line comment (;) or end-of-line?
(define-constant initial-comment-eol '(#\; #\newline #\linefeed #\return))
(defun lcomment-eolp (c)
  (member c initial-comment-eol))

; (defun my-peek-char (stream) (peek-char nil stream))

(defun my-peek-char (stream)
  (let ((c (peek-char nil stream t nil my-eof-marker)))
    ; (princ "DEBUG: my-peek-char returned=") (write c) (terpri)
    c))
(defun my-read-char (stream) (read-char stream t nil nil))


; Consume an end-of-line sequence, ('\r' '\n'? | '\n')
(defun consume-end-of-line (stream)
  (let ((c (my-peek-char stream)))
    (cond
      ((eof-objectp c) (values))
      ((eql c #\return)
        (my-read-char stream)
        (if (eql (my-peek-char stream) #\linefeed)
            (my-read-char stream)))
      ((or (eql c #\linefeed) (eql c #\newline))
        (my-read-char stream)))))

; Consume every non-eol character in the current line.
; End on EOF or end-of-line char.
; Do NOT consume the end-of-line character(s).
(defun consume-to-eol (stream)
  (let ((c (my-peek-char stream)))
    (when (not (char-line-endingp c))
        (my-read-char stream)
        (consume-to-eol stream))))


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
;   ; Gobble up the to-gobble characters from stream, and return # ungobbled.
;   (defun gobble-chars (stream to-gobble)
;     (if (null to-gobble)
;         0
;         (cond
;           ((char-equal (my-peek-char stream) (car to-gobble))
;             (my-read-char stream)
;             (gobble-chars stream (cdr to-gobble)))
;           (t (string-length to-gobble)))))


; detect #| or |#
(define-constant hash-pipe-comment-nestsp t)
(defun nest-comment (stream)
  (let ((c (my-read-char stream)))
    (cond
      ((eof-objectp c)
        empty-values)
      ((char= c #\|)
        (let ((c2 (my-peek-char stream)))
          (if (and (not (eof-objectp c2)) (char= c2 #\#))
              (progn
                (my-read-char stream)
                empty-values)
              (nest-comment stream))))
      ((and hash-pipe-comment-nestsp (char= c #\#))
        (let ((c2 (my-peek-char stream)))
          (if (and (not (eof-objectp c2)) (char= c2 #\|))
              (progn
                (my-read-char stream)
                (nest-comment stream))
              empty-values)
          (nest-comment stream)))
      (t
        (nest-comment stream)))))

; Implement #|...|#
(defun wrap-comment-block (stream sub-char int)
  (declare (ignore sub-char int))
  (nest-comment stream))

; Implement #;datum
(defun wrap-comment-datum (stream sub-char int)
  (declare (ignore sub-char int))
  (if (my-char-whitespacep (my-peek-char stream))
    (read-error "#; must not be followed by whitespace")
    (let ((junk (neoteric-read-nocomment stream)))
      (declare (ignore junk))
      empty-values)))

; There is no standard mechanism to unread multiple characters.
; Therefore, the key productions and some of their supporting procedures
; return both the information on what ended their reading process,
; as well the actual value (if any) they read before whatever stopped them.
; That way, procedures can process the value as read, and then pass on
; the ending information to whatever needs it next.  This approach,
; which we call a "non-tokenizing" implementation, implements a tokenizer
; via procedure calls instead of needing a separate tokenizer.
; The ending information can be:
; - "stopper" - this is returned by productions etc. that do NOT
;     read past the of a line (outside of paired characters and strings).
;     It is 'normal if it ended normally (e.g., at end of line); else it's
;     'sublist-marker ($), 'group-split-marker (\\), 'collecting (<*),
;     'collecting-end (*>), 'scomment (special comments like #|...|#), or
;     'abbrevw (initial abbreviation with whitespace after it).
; - "new-indent" - this is returned by productions etc. that DO read
;     past the end of a line.  Such productions typically read the
;     next line's indent to determine if they should return.
;     If they should, they return the new indent so callers can
;     determine what to do next.  A "*>" should return even though its
;     visible indent level is length 0; we handle this by prepending
;     all normal indents with "^", and "*>" generates a length-0 indent
;     (which is thus shorter than even an indent of 0 characters).


(define-constant group-split '\\)
(define-constant group-split-char #\\ ) ; First character of split symbol.

(defvar non-whitespace-indent #\!) ; Non-whitespace-indent char.

(define-constant sublist '$)
(define-constant sublist-char #\$) ; First character of sublist symbol.


(defun indentation>p (indentation1 indentation2)
  (let ((len1 (string-length indentation1))
          (len2 (string-length indentation2)))
    (and (> len1 len2)
           (string= indentation2 (subseq indentation1 0 len2)))))

; Return t if char is space or tab.
(defun char-hspacep (char)
  (or (eql char #\space)
      (eql char #\tab)))

; Consume 0+ spaces or tabs
(defun hspaces (stream)
  (when (char-hspacep (my-peek-char stream))
      (my-read-char stream)
      (hspaces stream)))

; Return t if char is space, tab, or !
(defun char-icharp (char)
  (or (eql char #\space)
      (eql char #\tab)
      (eql char non-whitespace-indent)))

(defun accumulate-ichar (stream)
  (if (char-icharp (my-peek-char stream))
      (cons (my-read-char stream) (accumulate-ichar stream))
      '()))

(defun consume-ff-vt (stream)
  (let ((c (my-peek-char stream)))
    (cond
      ((or (eql c form-feed) (eql c vertical-tab))
        (my-read-char stream)
        (consume-ff-vt stream)))))

; Do 2-item append, but report read-error if the LHS is not a proper list.
; Don't use this if the lhs *must* be a list (e.g., if we have (list x)).
(defun my-append (lhs rhs)
  (if (listp lhs)
      (append lhs rhs)
      (read-error "Must have proper list on left-hand-side to append data")))

; Read an n-expression.  Returns ('scomment '()) if it's an scomment,
; else returns ('normal n-expr).
(defun n-expr-or-scomment (stream)
  (let ((result (my-read-datum stream))) ; Make it possible to return ".".
    (if (eq result empty-values)
      scomment-result
      (list 'normal result))))

; Read a straight-up n-expression.  Skip scomments.
(defun neoteric-read-nocomment (stream)
  (let ((result (my-read-datum stream))) ; Make it possible to return ".".
  (if (eq result empty-values)
      (neoteric-read-nocomment stream)
      result)))

; Read an n-expression.  Returns ('normal n-expr) in most cases;
; if it's a special marker, the car is the marker name instead of 'normal.
; Markers only have special meaning if their first character is
; the "normal" character, e.g., {$} is not a sublist.
; Call "process-sharp" if first char is "#".
(defun n-expr (stream)
  (let* ((c (my-peek-char stream))
         (results (n-expr-or-scomment stream))
         (type (car results))
         (expr (cadr results)))
    (declare (ignore type))
    ; (princ "DEBUG: n-expr, results=") (write results) (terpri)
    ; (princ "DEBUG: n-expr, car results=") (write (car results)) (terpri)
    ; (princ "DEBUG: n-expr, car results scomment=") (write (eq (car results) 'scomment)) (terpri)
    (if (eq (car results) 'scomment)
        results
        (cond
          ; TODO: Improve Workaround for symbol packaging:
          ((and (eql c sublist-char) (string= (symbol-name expr) "$"))
            (list 'sublist-marker '()))
          ((and (eql c group-split-char) (string= (symbol-name expr) "\\"))
            (list 'group-split-marker '()))
          ((and (eql c #\<) (string= (symbol-name expr) "<*"))
            (list 'collecting '()))
          ((and (eql c #\*) (string= (symbol-name expr) "*>"))
            (list 'collecting-end '()))
          ((and (eql c #\$) (string= (symbol-name expr) "$$$"))
            (read-error "Error - $$$ is reserved"))
          ((and (eql c #\.) (string= (symbol-name expr) "."))
            (list 'period-marker '()))
          (t
            results)))))

; Check if we have abbrev+whitespace.  If the current peeked character
; is one of certain whitespace chars,
; return 'abbrevw as the marker and abbrev-procedure
; as the value (the cadr). Otherwise, return ('normal n-expr).
; We do NOT consume the peeked char (so EOL can be examined later).
; Note that this calls the neoteric-read procedure directly, because
; quoted markers are no longer markers. E.G., '$ is just (quote $).
(defun maybe-initial-abbrev (stream abbrev-procedure)
  (let ((c (my-peek-char stream)))
    (if (or (char-hspacep c) (eql c #\return) (eql c #\linefeed)
            (eql c #\newline))
        (list 'abbrevw abbrev-procedure)
        (list 'normal
          (list abbrev-procedure (neoteric-read-nocomment stream))))))

; Read the first n-expr on a line; handle abbrev+whitespace specially.
; Returns ('normal VALUE) in most cases.
(defun n-expr-first (stream)
  (case (my-peek-char stream)
    ((#\') 
      (my-read-char stream)
      (maybe-initial-abbrev stream 'quote))
    ((#\`) 
      (my-read-char stream)
      (maybe-initial-abbrev stream 'backquote))
    ((#\,) 
      (my-read-char stream)
      (case (my-peek-char stream)
        (#\@
          (my-read-char stream)
          (maybe-initial-abbrev stream *comma-atsign*))
        (#\.
          (my-read-char stream)
          (maybe-initial-abbrev stream *comma-dot*))
        (otherwise
          (maybe-initial-abbrev stream *comma*))))
    (t
      (n-expr stream))))

; Consume ;-comment (if there), consume EOL, and return new indent.
; Skip ;-comment-only lines; a following indent-only line is empty.
(defun get-next-indent (stream)
  (consume-to-eol stream)
  (consume-end-of-line stream)
  (let* ((indentation-as-list (cons #\^ (accumulate-ichar stream)))
         (c (my-peek-char stream)))
    (cond
      ((eof-objectp c) "^") ; EOF ; end any existing expression.
      ((eql c #\;)  ; A ;-only line, consume and try again.
        (get-next-indent stream))
      ((lcomment-eolp c) ; Indent-only line
        (if (member #\! indentation-as-list)
            (get-next-indent stream)
            "^"))
      (t (concatenate 'string indentation-as-list)))))

; Utility function:
; If x is a 1-element list, return (car x), else return x
(defun monify (x)
  (cond
    ((atom x) x)
    ((null (cdr x)) (car x))
    (t x)))

; Return contents (value) of collecting-tail.  It does *not* report a
; stopper or ending indent, because it is *ONLY* stopped by collecting-end
(defun collecting-tail (stream)
  (let* ((c (my-peek-char stream)))
    (cond
      ((eof-objectp c)
       (read-error "Collecting tail: EOF before collecting list ended"))
      ((lcomment-eolp c)
        (consume-to-eol stream)
        (consume-end-of-line stream)
        (collecting-tail stream))
      ((char-icharp c)
        (let* ((indentation (accumulate-ichar stream))
               (c (my-peek-char stream)))
          (cond
            ((eql c #\;)
              (collecting-tail stream))
            ((lcomment-eolp c)
              (if (member #\! indentation)
                  (read-error "Collecting tail: False empty line with !")
                  (collecting-tail stream)))
            (t
              (read-error "Collecting tail: Only ; after indent")))))
      ((or (eql c form-feed) (eql c vertical-tab))
        (consume-ff-vt stream)
        (if (lcomment-eolp (my-peek-char stream))
            (collecting-tail stream)
            (read-error "Collecting tail: FF and VT must be alone on line")))
      (t
        (let* ((it-full-results (it-expr stream "^"))
               (it-new-indent   (car it-full-results))
               (it-value        (cadr it-full-results)))
          (cond
            ((string= it-new-indent "")
              ; Specially compensate for "*>" at the end of a line if it's
              ; after something else.  This must be interpreted as EOL *>,
              ; which would cons a () after the result.
              ; Directly calling list for a non-null it-value has
              ; the same effect, but is a lot quicker and simpler.
              (if (null it-value)
                  it-value
                  (list it-value)))
            (t (cons it-value (collecting-tail stream)))))))))

; Skip scomments and error out if we have a normal n-expr;
; Basically implement this BNF:
;    (scomment hspace*)* (n-expr error)?
; This procedure is used after ". value".
(defun n-expr-error (stream full)
  (if (not (eq (car full) 'normal))
      (read-error "BUG! n-expr-error called but stopper not normal"))
  (if (lcomment-eolp (my-peek-char stream))
      full ; All done!
      (let* ((n-full-results (n-expr stream))
             (n-stopper      (car n-full-results))
             (n-value        (cadr n-full-results)))
        (declare (ignore n-value))
        (cond
          ((eq n-stopper 'scomment) ; Consume scomments.
            (hspaces stream)
            (n-expr-error stream full))
          ((eq n-stopper 'normal)
            (read-error "Illegal second value after ."))
          (t ; We found a stopper, return it with the value from "full"
            (list n-stopper (cadr full)))))))

; Returns (stopper value-after-period)
(defun post-period (stream)
  (if (not (lcomment-eolp (my-peek-char stream)))
      (let* ((pn-full-results (n-expr stream))
             (pn-stopper      (car pn-full-results))
             (pn-value        (cadr pn-full-results)))
        (declare (ignore pn-value))
        (cond
          ((eq pn-stopper 'scomment)
            (hspaces stream)
            (post-period stream))
          ((eq pn-stopper 'normal)
            (hspaces stream)
            (n-expr-error stream pn-full-results))
          ((eq pn-stopper 'collecting)
            (hspaces stream)
            (let ((ct (collecting-tail stream)))
              (hspaces stream)
              (n-expr-error stream (list 'normal ct))))
          ((eq pn-stopper 'period-marker)
            (list 'normal period-symbol))
          (t ; Different stopper; respond as empty branch with that stopper
            (list pn-stopper (list period-symbol)))))
      (list 'normal period-symbol))) ; Empty branch.

; Returns (stopper computed-value).
; The stopper may be 'normal, 'scomment (special comment),
; 'abbrevw (initial abbreviation), 'sublist-marker, or 'group-split-marker
(defun head (stream)
  (let* ((basic-full-results (n-expr-first stream))
         (basic-special      (car basic-full-results))
         (basic-value        (cadr basic-full-results)))
    ; (princ "DEBUG: head=") (write basic-full-results) (terpri)
    (cond
      ((eq basic-special 'collecting)
        (hspaces stream)
        (let* ((ct-results (collecting-tail stream)))
          (hspaces stream)
          (if (not (lcomment-eolp (my-peek-char stream)))
              (let* ((rr-full-results (sweet-rest stream))
                     (rr-stopper      (car rr-full-results))
                     (rr-value        (cadr rr-full-results)))
                (list rr-stopper (cons ct-results rr-value)))
              (list 'normal (list ct-results)))))
      ((eq basic-special 'period-marker)
        (if (char-hspacep (my-peek-char stream))
            (progn
              (hspaces stream)
              (let* ((ct-full-results (post-period stream))
                     (ct-stopper      (car ct-full-results))
                     (ct-value        (cadr ct-full-results)))
                (list ct-stopper (list ct-value))))
            (list 'normal (list period-symbol))))
      ((not (eq basic-special 'normal)) basic-full-results)
      ((char-hspacep (my-peek-char stream))
        (hspaces stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (let* ((br-full-results (sweet-rest stream))
                   (br-stopper      (car br-full-results))
                   (br-value        (cadr br-full-results)))
              (list br-stopper (cons basic-value br-value)))
            (list 'normal (list basic-value))))
      (t 
        (list 'normal (list basic-value))))))

; Returns (stopper computed-value); stopper may be 'normal, etc.
; Read in one n-expr, then process based on whether or not it's special.
; NOTE: This non-terminal renamed from "rest" to avoid conflict with CL.
(defun sweet-rest (stream)
  (let* ((basic-full-results (n-expr stream))
         (basic-special      (car basic-full-results))
         (basic-value        (cadr basic-full-results)))
    (cond
      ((eq basic-special 'scomment)
        (hspaces stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (sweet-rest stream)
            (list 'normal '())))
      ((eq basic-special 'collecting)
        (hspaces stream)
        (let* ((ct-results (collecting-tail stream)))
          (hspaces stream)
          (if (not (lcomment-eolp (my-peek-char stream)))
              (let* ((rr-full-results (sweet-rest stream))
                     (rr-stopper      (car rr-full-results))
                     (rr-value        (cadr rr-full-results)))
                (list rr-stopper (cons ct-results rr-value)))
              (list 'normal (list ct-results)))))
      ((eq basic-special 'period-marker)
        (if (char-hspacep (my-peek-char stream))
            (progn
              (hspaces stream)
              (post-period stream))
            (list 'normal (list period-symbol))))
      ((not (eq basic-special 'normal)) (list basic-special '())) 
      ((char-hspacep (my-peek-char stream))
        (hspaces stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (let* ((br-full-results (sweet-rest stream))
                   (br-stopper      (car br-full-results))
                   (br-value        (cadr br-full-results)))
              (list br-stopper (cons basic-value br-value)))
            (list 'normal (list basic-value))))
      (t (list 'normal (list basic-value))))))

; Returns (new-indent computed-value)
(defun body (stream starting-indent)
  (let* ((i-full-results (it-expr stream starting-indent))
         (i-new-indent   (car i-full-results))
         (i-value        (cadr i-full-results)))
    (if (string= starting-indent i-new-indent)
        (if (eq i-value period-symbol)
            (let* ((f-full-results (it-expr stream i-new-indent))
                   (f-new-indent   (car f-full-results))
                   (f-value        (cadr f-full-results)))
              (if (not (indentation>p starting-indent f-new-indent))
                  (read-error "Dedent required after lone . and value line"))
              (list f-new-indent f-value)) ; final value of improper list
            (let* ((nxt-full-results (body stream i-new-indent))
                   (nxt-new-indent   (car nxt-full-results))
                   (nxt-value        (cadr nxt-full-results)))
              (list nxt-new-indent (cons i-value nxt-value))))
        (list i-new-indent (list i-value))))) ; dedent - end list.

; Returns (new-indent computed-value)
(defun it-expr-real (stream starting-indent)
  (let* ((head-full-results (head stream))
         (head-stopper      (car head-full-results))
         (head-value        (cadr head-full-results)))
; (princ "DEBUG: it-expr-real: head result=") (write head-full-results) (terpri)
    (if (and (not (null head-value)) (not (eq head-stopper 'abbrevw)))
        ; The head... branches:
        (cond
          ((eq head-stopper 'group-split-marker)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "Cannot follow split with end of line")
                (list starting-indent (monify head-value))))
          ((eq head-stopper 'sublist-marker)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "EOL illegal immediately after sublist"))
            (let* ((sub-i-full-results (it-expr stream starting-indent))
                   (sub-i-new-indent   (car sub-i-full-results))
                   (sub-i-value        (cadr sub-i-full-results)))
              (list sub-i-new-indent
                (my-append head-value (list sub-i-value)))))
          ((eq head-stopper 'collecting-end)
            ; Note that indent is "", forcing dedent all the way out.
            (list "" (monify head-value)))
          ((lcomment-eolp (my-peek-char stream))
            (let ((new-indent (get-next-indent stream)))
              (if (indentation>p new-indent starting-indent)
                  (let* ((body-full-results (body stream new-indent))
                         (body-new-indent (car body-full-results))
                         (body-value      (cadr body-full-results)))
                    (list body-new-indent (my-append head-value body-value)))
                  (list new-indent (monify head-value)))))
          (t
            (read-error "Must end line with end-of-line sequence")))
        ; Here, head begins with something special like GROUP-SPLIT:
        (cond
          ((or (eq head-stopper 'group-split-marker)
               (eq head-stopper 'scomment))
            (hspaces stream)
            (if (not (lcomment-eolp (my-peek-char stream)))
                (it-expr stream starting-indent) ; Skip and try again.
                (let ((new-indent (get-next-indent stream)))
                  (cond
                    ((indentation>p new-indent starting-indent)
                      (body stream new-indent))
                    ((string= starting-indent new-indent)
                      (if (not (lcomment-eolp (my-peek-char stream)))
                        (it-expr stream new-indent)
                        (list new-indent (t-expr stream)))) ; Restart
                    (t
                      (read-error "GROUP-SPLIT EOL DEDENT illegal"))))))
          ((eq head-stopper 'sublist-marker)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "EOL illegal immediately after solo sublist"))
            (let* ((is-i-full-results (it-expr stream starting-indent))
                   (is-i-new-indent   (car is-i-full-results))
                   (is-i-value        (cadr is-i-full-results)))
              (list is-i-new-indent
                (list is-i-value))))
          ((eq head-stopper 'abbrevw)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (progn
                  (let ((new-indent (get-next-indent stream)))
                    (if (not (indentation>p new-indent starting-indent))
                        (read-error "Indent required after abbreviation"))
                    (let* ((ab-full-results (body stream new-indent))
                           (ab-new-indent   (car ab-full-results))
                           (ab-value      (cadr ab-full-results)))
                      (list ab-new-indent
                        (append (list head-value) ab-value)))))
                (let* ((ai-full-results (it-expr stream starting-indent))
                       (ai-new-indent (car ai-full-results))
                       (ai-value    (cadr ai-full-results)))
                  (list ai-new-indent
                    (list head-value ai-value)))))
          ((eq head-stopper 'collecting-end)
            (list "" head-value))
          (t 
            (read-error "Initial head error"))))))

; Read it-expr.  This is a wrapper that attaches source info
; and checks for consistent indentation results.
(defun it-expr (stream starting-indent)
  (let* ((pos (get-sourceinfo stream))
         (results (it-expr-real stream starting-indent))
         (results-indent (car results))
         (results-value (cadr results)))
    (if (indentation>p results-indent starting-indent)
        (read-error "Inconsistent indentation"))
    (list results-indent (attach-sourceinfo pos results-value))))

; Top level - read a sweet-expression (t-expression).  Handle special
; cases, such as initial indent; call it-expr for normal case.
(defun t-expr (stream)
  (let* ((c (my-peek-char stream)))
        (cond
          ((lcomment-eolp c)
            (consume-to-eol stream)
            (consume-end-of-line stream)
            (t-expr stream))
          ((or (eql c form-feed) (eql c vertical-tab))
            (consume-ff-vt stream)
            (t-expr stream))
          ((char-icharp c)
            (let ((indentation-list (cons #\^ (accumulate-ichar stream))))
              (declare (ignore indentation-list))
              (if (not (member (my-peek-char stream) initial-comment-eol))
                  (let ((results (n-expr-or-scomment stream)))
                    (if (not (eq (car results) 'scomment))
                        (cadr results) ; Normal n-expr, return one value.
                        (progn ; We have an scomment; skip and try again.
                          (hspaces stream)
                          (t-expr stream))))
                  (progn ; Indented comment-eol, consume and try again.
                    (consume-to-eol stream)
                    (consume-end-of-line stream)
                    (t-expr stream)))))
          (t
            (let* ((results (it-expr stream "^"))
                   (results-indent (car results))
                   (results-value (cadr results)))
              (if (string= results-indent "")
                  (read-error "Closing *> without preceding matching <*")
                  results-value))))))

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
;    ; Default guile stack size is FAR too small
;    (debug-set! stack 500000)
;
;    (catch 'readable
;      (lambda () (t-expr stream))
;      (lambda (key . args) (read-to-blank-line stream) (t-expr-catch stream))))

(defun t-expr-entry (stream char)
  (unread-char char stream)
  ; (princ "DEBUG entry: ") (write char) (terpri)
  (let ((*readtable* *underlying-sweet-readtable*))
    (t-expr stream)))

; Set up a readtable that'll redirect everything.
(defun compute-sweet-redirect-readtable ()
  (setq *sweet-readtable*
    (let ((new (copy-readtable nil)))
      (set-syntax-from-char #\# #\' new) ; force # to not be dispatching char.
      (loop for ci from 0 upto my-char-code-limit
         do (set-macro-character (code-char ci) #'t-expr-entry nil new))
      new)))

(defun enable-sweet ()
  (enable-neoteric)

  ; Now create the underlying sweet readtable by tweaking neoteric readtable.
  ; This underlying table is called to read specific expressions.
  (setq *readtable* (copy-readtable *readtable*))
  ; Handle #|...|# and #; specially:
  (set-dispatch-macro-character #\# #\| #'wrap-comment-block)
  (set-dispatch-macro-character #\# #\; #'wrap-comment-datum)
  ; Re-implement backquote and comma, so indentation can happen inside them;
  ; Notice that (read stream t nil t) is replaced with (my-read-datum stream):
  (set-macro-character #\`
    #'(lambda (stream char)
        (declare (ignore char))
        (list 'backquote (my-read-datum stream))))
  (set-macro-character #\,
    #'(lambda (stream char)
        (declare (ignore char))
          (case (my-peek-char stream)
            (#\@ (my-read-char stream)
                 (list *comma-atsign* (my-read-datum stream)))
            (#\. (read-char stream t nil t)
                 (list *comma-dot* (my-read-datum stream)))
            (otherwise (list *comma* (my-read-datum stream))))))
  (setq *underlying-sweet-readtable* *readtable*)

  ; Now create the redirecting readtable.  The idea is that ANY input
  ; will be redirected (through this table) eventually to t-expr and it-expr,
  ; which process the indentation, and they'll call other procedures that
  ; in turn will invoke *underlying-sweet-readtable*.
  (compute-sweet-redirect-readtable)
  (setq *readtable* *sweet-readtable*)
  (values))

(defun sweet-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-sweet)
    (read stream)))

