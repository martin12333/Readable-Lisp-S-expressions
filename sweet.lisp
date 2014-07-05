;;; sweet.lisp
;;; Implements sweet-expressions from the "readable" approach for Lisp.

;;; Copyright (C) 2007-2014 by David A. Wheeler
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
; distinctive value "no-neoteric-value", to represent the "no value" case.
(defvar *underlying-sweet-readtable*
  "This table is basically neoteric-expressions with some tweaks")

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

; Note: Use define-constant to work around SBCL problem.

(define-constant period-symbol '|.|)

(define-constant scomment-result '(scomment ()))

; Marker for empty values.
; Several Common Lisp readtable constructs return nothing using (values),
; but unfortunately when that happens the build-in Common Lisp reader
; performs actions that we can't intercept.  So we'll override the "empty"
; returns with this instead, so that they will get passed back to us
; and allow us to override the reader.
(define-constant no-neoteric-value (make-symbol "no-neoteric-value"))

; Represent no value at all, in the sweet-expression processing.
(define-constant empty-value (make-symbol "empty-value"))

(define-constant datum-commentw-tag (make-symbol "datum-commentw"))

(define-constant vertical-tab (code-char 11)) ; VT is decimal 11.
(define-constant form-feed #\page)            ; FF is decimal 12.

(define-constant whitespace-chars
   (list #\space #\tab #\linefeed #\newline #\return vertical-tab form-feed))

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

(defun my-peek-char (stream)
  (let ((c (peek-char nil stream nil my-eof-marker)))
    ; (format t "DEBUG: my-peek-char: ~@C~%" c)
    c))
(defun my-read-char (stream)
  (let ((c (read-char stream t nil nil)))
    ; (format t "DEBUG: my-read-char: ~@C~%" c)
    c))


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

; detect #| or |#
(define-constant hash-pipe-comment-nestsp t)
(defun nest-comment (stream)
  (let ((c (my-read-char stream)))
    (cond
      ((eof-objectp c)
        no-neoteric-value)
      ((char= c #\|)
        (let ((c2 (my-peek-char stream)))
          (if (and (not (eof-objectp c2)) (char= c2 #\#))
              (progn
                (my-read-char stream)
                no-neoteric-value)
              (nest-comment stream))))
      ((and hash-pipe-comment-nestsp (char= c #\#))
        (let ((c2 (my-peek-char stream)))
          (if (and (not (eof-objectp c2)) (char= c2 #\|))
              (progn
                (my-read-char stream)
                (nest-comment stream))
              no-neoteric-value)
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
    datum-commentw-tag
    (let ((junk (neoteric-read-nocomment stream)))
      (declare (ignore junk))
      no-neoteric-value)))

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
;     'collecting-end (*>), 'scomment (special comments like #|...|#),
;     'datum-commentw, or
;     'abbrevw (initial abbreviation with whitespace after it).
; - "new-indent" - this is returned by productions etc. that DO read
;     past the end of a line.  Such productions typically read the
;     next line's indent to determine if they should return.
;     If they should, they return the new indent so callers can
;     determine what to do next.  A "*>" should return even though its
;     visible indent level is length 0; we handle this by prepending
;     all normal indents with "^", and "*>" generates a length-0 indent
;     (which is thus shorter than even an indent of 0 characters).

; Define let-splitter macro to simplify common code pattern.
(defmacro let-splitter ((full first-value second-value) expr &rest body)
  `(let* ((,full ,expr)
          (,first-value (car ,full))
          (,second-value (cadr ,full)))
         ,@body))

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
(declaim (inline char-hspacep))
(defun char-hspacep (char)
  (or (eql char #\space)
      (eql char #\tab)))

; Consume 0+ spaces or tabs.  Uses direct iteraction, not tail recursion.
(defun hspaces (stream)
  (loop
     while (char-hspacep (my-peek-char stream))
     do (my-read-char stream)))

; Return t if char is space, tab, or !
(declaim (inline char-icharp))
(defun char-icharp (char)
  (or (eql char #\space)
      (eql char #\tab)
      (eql char non-whitespace-indent)))

(declaim (inline accumulate-ichar))
(defun accumulate-ichar (stream)
  (loop while (char-icharp (my-peek-char stream))
        collect (my-read-char stream)))

(defun consume-ff-vt (stream)
  (let ((c (my-peek-char stream)))
    (cond
      ((or (eql c form-feed) (eql c vertical-tab))
        (my-read-char stream)
        (consume-ff-vt stream)))))

; Do 2-item append, but report read-error if the LHS is not a proper list.
; Don't use this if the lhs *must* be a list (e.g., if we have (list x)).
(defun my-append (lhs rhs)
  (cond
    ((eq lhs empty-value) rhs)
    ((eq rhs empty-value) lhs)
    ((listp lhs) (append lhs rhs))
    (t
      (read-error "Must have proper list on left-hand-side to append data"))))

; Read an n-expression.  Returns ('scomment '()) if it's an scomment,
; else returns ('normal n-expr).
(defun n-expr-or-scomment (stream)
  (let ((result (my-read-datum stream))) ; Make it possible to return ".".
    (cond
      ((eq result no-neoteric-value) scomment-result)
      ((eq result datum-commentw-tag) '(datum-commentw ()))
      (t (list 'normal result)))))

; Read a straight-up n-expression.  Skip scomments.
(defun neoteric-read-nocomment (stream)
  (let ((result (my-read-datum stream))) ; Make it possible to return ".".
    (cond
      ((eq result no-neoteric-value) (neoteric-read-nocomment stream))
      ((eq result datum-commentw-tag)
        (neoteric-read-nocomment stream) ; Consume the next n-expression.
        (neoteric-read-nocomment stream))
      (t result))))

; Read an n-expression.  Returns ('normal n-expr) in most cases;
; if it's a special marker, the car is the marker name instead of 'normal.
; Markers only have special meaning if their first character is
; the "normal" character, e.g., {$} is not a sublist.
; Call "process-sharp" if first char is "#".
(defun n-expr (stream)
  (let ((c (my-peek-char stream)))
    (let-splitter (results type expr)
                  (n-expr-or-scomment stream)
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
              (read-error "$$$ is reserved."))
            ((and (eql c #\.) (string= (symbol-name expr) "."))
              (list 'period-marker '()))
            (t
              results))))))

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

; Read and throw away "skippable" content (text that is semantically the
; same as whitespace but is complex).  This implements the sequence:
;   (scomment hs | datum-commentw hs n-expr hs)
(defun skippable (stopper stream)
  (cond
  ((eq stopper 'scomment)
    (hspaces stream))
  ((eq stopper 'datum-commentw)
    (hspaces stream)
    (if (not (lcomment-eolp (my-peek-char stream)))
      (progn
        (n-expr stream)
        (hspaces stream))
      (read-error "Datum comment start not followed a datum (EOL instead)")))
  (t (read-error "skippable: Impossible case"))))

; Utility declarations and functions

(defun conse (x y) ; cons, but handle "empty" values
  (cond
    ((eq y empty-value) x)
    ((eq x empty-value) y)
    (t (cons x y))))

(defun appende (x y) ; append, but handle "empty" values
  (cond
    ((eq y empty-value) x)
    ((eq x empty-value) y)
    (t (append y))))

(defun list1e (x) ; list, but handle "empty" values
  (if (eq x empty-value)
      '()
      (list x)))

(defun list2e (x y) ; list, but handle "empty" values
  (if (eq x empty-value)
      y
      (if (eq y empty-value)
         x
         (list x y))))

; If x is a 1-element list, return (car x), else return x
(defun monify (x)
  (cond
    ((atom x) x)
    ((null (cdr x)) (car x))
    (t x)))


; Read the contents of a collecting list and return it.
; Precondition: Have already read collecting start and horizontal spaces.
; Postcondition: Consumed the matching COLLECTING_END.
; Return contents (value) of collecting-content.  It does *not* report a
; stopper or ending indent, because it is *ONLY* stopped by collecting-end
(defun collecting-content (stream)
  (let* ((c (my-peek-char stream)))
    (cond
      ((eof-objectp c)
       (read-error "Collecting tail: EOF before collecting list ended."))
      ((lcomment-eolp c)
        (consume-to-eol stream)
        (consume-end-of-line stream)
        (collecting-content stream))
      ((char-icharp c)
        (let* ((indentation (accumulate-ichar stream))
               (c (my-peek-char stream)))
          (declare (ignore indentation))
          (if (lcomment-eolp c)
              (collecting-content stream)
              (read-error "Collecting tail: Only ; after indent."))))
      ((or (eql c form-feed) (eql c vertical-tab))
        (consume-ff-vt stream)
        (if (lcomment-eolp (my-peek-char stream))
            (collecting-content stream)
            (read-error "Collecting tail: FF and VT must be alone on line.")))
      (t
        (let-splitter (it-full-results it-new-indent it-value)
                      (it-expr stream "^")
          (cond
            ((string= it-new-indent "")
              ; Specially compensate for "*>" at the end of a line if it's
              ; after something else.  This must be interpreted as EOL *>,
              ; which would cons a () after the result.
              ; Directly calling list for a non-null it-value has
              ; the same effect, but is a lot quicker and simpler.
              (cond
                ((null it-value) it-value)
                ((eq it-value empty-value) '())
                (t (list it-value))))
            (t (conse it-value (collecting-content stream)))))))))

; Skip scomments and error out if we have a normal n-expr;
; Basically implement this BNF:
;    (scomment hspace*)* (n-expr error)?
; This procedure is used after ". value".
(defun n-expr-error (stream full)
  (if (not (eq (car full) 'normal))
      (read-error "BUG! n-expr-error called but stopper not normal."))
  (if (lcomment-eolp (my-peek-char stream))
      full ; All done!
      (let-splitter (n-full-results n-stopper n-value)
                    (n-expr stream)
        (declare (ignore n-value))
        (cond
          ((or (eq n-stopper 'scomment) (eq n-stopper 'datum-commentw))
            (skippable n-stopper stream)
            (n-expr-error stream full))
          ((eq n-stopper 'normal)
            (read-error "Illegal second value after '.'."))
          (t ; We found a stopper, return it with the value from "full"
            (list n-stopper (cadr full)))))))

; Read input after a lone ".", normally exactly one datum.
; Returns (stopper value-after-period)
(defun post-period (stream)
  (if (not (lcomment-eolp (my-peek-char stream)))
      (let-splitter (pn-full-results pn-stopper pn-value)
                    (n-expr stream)
        (declare (ignore pn-value))
        (cond
          ((or (eq pn-stopper 'scomment) (eq pn-stopper 'datum-commentw))
            (skippable pn-stopper stream)
            (post-period stream))
          ((eq pn-stopper 'normal)
            (hspaces stream)
            (n-expr-error stream pn-full-results))
          ((eq pn-stopper 'collecting)
            (hspaces stream)
            (let ((cl (collecting-content stream)))
              (hspaces stream)
              (n-expr-error stream (list 'normal cl))))
          ((eq pn-stopper 'period-marker)
            (list 'normal period-symbol))
          (t ; Different stopper; respond as empty branch with that stopper
            (list pn-stopper (list period-symbol)))))
      (list 'normal period-symbol))) ; Empty branch.

; Read the 1+ n-expressions on one line, and return them as a list.
; If there is exactly one n-expression on the line,
; it returns a list of exactly one item.
; Precondition: At beginning of line after indent
; Postcondition: At unconsumed EOL
; Returns (stopper computed-value).
; The stopper may be 'normal, 'scomment (special comment),
; 'abbrevw (initial abbreviation), 'sublist-marker, or 'group-split-marker
(defun line-exprs (stream indent)
  (let-splitter (basic-full-results basic-special basic-value)
                (n-expr-first stream)
    ; (princ "DEBUG: line-exprs=") (write basic-full-results) (terpri)
    (cond
      ((eq basic-special 'collecting)
        (hspaces stream)
        (let* ((cl-results (collecting-content stream)))
          (hspaces stream)
          (if (not (lcomment-eolp (my-peek-char stream)))
              (let-splitter (rr-full-results rr-stopper rr-value)
                            (rest-of-line stream indent)
                (list rr-stopper (cons cl-results rr-value)))
              (list 'normal (list cl-results)))))
      ((eq basic-special 'period-marker)
        (if (char-hspacep (my-peek-char stream))
            (progn
              (hspaces stream)
              (let-splitter (cl-full-results cl-stopper cl-value)
                            (post-period stream)
                (list cl-stopper (list cl-value))))
            (list 'normal (list period-symbol))))
      ((not (eq basic-special 'normal)) basic-full-results)
      ((char-hspacep (my-peek-char stream))
        (hspaces stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (let-splitter (br-full-results br-stopper br-value)
                          (rest-of-line stream indent)
              (list br-stopper (cons basic-value br-value)))
            (list 'normal (list basic-value))))
      (t 
        (list 'normal (list basic-value))))))

; Read the rest of the expressions on a line,
; after the first expression of the line.  This supports line-exprs.
; Precondition: At beginning of non-first expression on line (past hspace)
; Postcondition: At unconsumed EOL
; Returns (stopper computed-value); stopper may be 'normal, etc.
; Read in one n-expr, then process based on whether or not it's special.
(defun rest-of-line (stream indent)
  (let-splitter (basic-full-results basic-special basic-value)
                (n-expr stream)
    (cond
      ((or (eq basic-special 'scomment) (eq basic-special 'datum-commentw))
        (skippable basic-special stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (rest-of-line stream indent)
            (list 'normal '())))
      ((eq basic-special 'collecting)
        (hspaces stream)
        (let* ((cl-results (collecting-content stream)))
          (hspaces stream)
          (if (not (lcomment-eolp (my-peek-char stream)))
              (let-splitter (rr-full-results rr-stopper rr-value)
                            (rest-of-line stream indent)
                (list rr-stopper (cons cl-results rr-value)))
              (list 'normal (list cl-results)))))
      ((eq basic-special 'period-marker)
        (if (char-hspacep (my-peek-char stream))
            (progn
              (hspaces stream)
              (post-period stream))
            ; (list 'normal (list period-symbol)) ; To interpret as |.|
            (read-error "Cannot end line with '.'")))
      ((eq basic-special 'group-split-marker)
        ; Local extension - allow \\ as line-continuation, a
        ; capability useful in Common Lisp.
        ; This is *NOT* a SRFI-110 requirement!!
        (hspaces stream)
        (if (lcomment-eolp (my-peek-char stream))
          (let ((new-indent (get-next-indent stream)))
            (if (string= new-indent indent)
              (rest-of-line stream new-indent)
              (read-error "Line continuation indentation does not match.")))
          (list basic-special '())))
      ((not (eq basic-special 'normal)) (list basic-special '()))
      ((char-hspacep (my-peek-char stream))
        (hspaces stream)
        (if (not (lcomment-eolp (my-peek-char stream)))
            (let-splitter (br-full-results br-stopper br-value)
                          (rest-of-line stream indent)
              (list br-stopper (cons basic-value br-value)))
            (list 'normal (list basic-value))))
      (t (list 'normal (list basic-value))))))

; Read the sequence of 1+ child lines in an it_expr
; (e.g., after "line_expr"), each of which is itself an it_expr.
; It returns the list of expressions in the body and the new indent as
; (new-indent computed-value).
(defun body (stream starting-indent)
  (let-splitter (i-full-results i-new-indent i-value)
                (it-expr stream starting-indent)
    (if (string= starting-indent i-new-indent)
        (if (eq i-value period-symbol)
            (let-splitter (f-full-results f-new-indent f-value)
                          (it-expr stream i-new-indent)
              (if (not (indentation>p starting-indent f-new-indent))
                  (read-error "Dedent required after lone . and value line."))
              (list f-new-indent f-value)) ; final value of improper list
            (if (eq i-value empty-value)
                (body stream i-new-indent)
                (let-splitter (nxt-full-results nxt-new-indent nxt-value)
                              (body stream i-new-indent)
                  (list nxt-new-indent (conse i-value nxt-value)))))
        (list i-new-indent (list1e i-value))))) ; dedent - end list.

; Read a sweet-expression that doesn't have a special prefix.
; Returns (new-indent computed-value)
(defun it-expr-real (stream starting-indent)
  (let-splitter (line-full-results line-stopper line-value)
                (line-exprs stream starting-indent)
    ; (princ "DEBUG: it-expr-real: line-exprs result=") (write line-full-results) (terpri)
    (if (and (not (null line-value)) (not (eq line-stopper 'abbrevw)))
        ; Production line-exprs produced at least one n-expression:
        (cond
          ((eq line-stopper 'group-split-marker)
            (hspaces stream)
            ; This error can't happen due to \\ line continuation extension,
            ; but we will test it just in case:
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "Cannot follow split with end of line")
                (list starting-indent (monify line-value))))
          ((eq line-stopper 'sublist-marker)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "EOL illegal immediately after sublist."))
            (let-splitter (sub-i-full-results sub-i-new-indent sub-i-value)
                          (it-expr stream starting-indent)
              (list sub-i-new-indent
                (my-append line-value (list sub-i-value)))))
          ((eq line-stopper 'collecting-end)
            ; Note that indent is "", forcing dedent all the way out.
            (list ""
              (if (eq line-value empty-value)
                '()
                (monify line-value))))
          ((lcomment-eolp (my-peek-char stream))
            (let ((new-indent (get-next-indent stream)))
              (if (indentation>p new-indent starting-indent)
                  (let-splitter (body-full-results body-new-indent body-value)
                                (body stream new-indent)
                    (list body-new-indent (my-append line-value body-value)))
                  (list new-indent (monify line-value)))))
          (t
            (read-error "Unexpected text after n-expression")))
        ; Here, line-exprs begins with something special like GROUP-SPLIT:
        (cond
          ((eq line-stopper 'datum-commentw)
            (hspaces stream)
            (cond
              ((not (lcomment-eolp (my-peek-char stream)))
                (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                              (it-expr stream starting-indent)
                  (declare (ignore is-i-value))
                  (list is-i-new-indent empty-value)))
              (t
                (let ((new-indent (get-next-indent stream)))
                  (if (indentation>p new-indent starting-indent)
                    (let-splitter (body-full-results body-new-indent body-value)
                                  (body stream new-indent)
                      (declare (ignore body-value))
                      (list body-new-indent empty-value))
                    (read-error "#;+EOL must be followed by indent"))))))
          ((or (eq line-stopper 'group-split-marker)
               (eq line-stopper 'scomment))
            (hspaces stream)
            (if (not (lcomment-eolp (my-peek-char stream)))
                (it-expr stream starting-indent) ; Skip and try again.
                (let ((new-indent (get-next-indent stream)))
                  (cond
                    ((indentation>p new-indent starting-indent)
                      (body stream new-indent))
                    (t
                      (list new-indent empty-value))))))
          ((eq line-stopper 'sublist-marker)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (read-error "EOL illegal immediately after solo sublist."))
            (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                          (it-expr stream starting-indent)
              (list is-i-new-indent
                (list1e is-i-value))))
          ((eq line-stopper 'abbrevw)
            (hspaces stream)
            (if (lcomment-eolp (my-peek-char stream))
                (progn
                  (let ((new-indent (get-next-indent stream)))
                    (if (not (indentation>p new-indent starting-indent))
                        (read-error "Indent required after abbreviation."))
                    (let-splitter (ab-full-results ab-new-indent ab-value)
                                  (body stream new-indent)
                      (list ab-new-indent
                        (append (list line-value) ab-value)))))
                (let-splitter (ai-full-results ai-new-indent ai-value)
                              (it-expr stream starting-indent)
                  (list ai-new-indent
                    (list2e line-value ai-value)))))
          ((eq line-stopper 'collecting-end)
            (list "" line-value))
          (t 
            (read-error "Initial line-expression error."))))))

; Read it-expr.  This is a wrapper that attaches source info
; and checks for consistent indentation results, then calls it-expr-real.
(defun it-expr (stream starting-indent)
  (let ((pos (get-sourceinfo stream)))
    (let-splitter (results results-indent results-value)
                  (it-expr-real stream starting-indent)
      (if (indentation>p results-indent starting-indent)
          (read-error "Inconsistent indentation."))
      (list results-indent (attach-sourceinfo pos results-value)))))

 
; Read the rest of an initial-indent-expr (a sweet-expression with
; a special initial value).
(defun initial-indent-expr-tail (stream)
  (if (not (member (my-peek-char stream) initial-comment-eol))
      (let-splitter (results results-stopper results-value)
                    (n-expr-or-scomment stream)
        (cond
          ((member results-stopper '(scomment datum-commentw))
            (skippable results-stopper stream)
            (initial-indent-expr-tail stream))
          (t ; Normal n-expr, return one value.
            ; The following "if" is a work-around for a bug in clisp's REPL.
            ; Without it, in sequential initial-indent lines like this:
            ;   1 2 3
            ;   4 5 6
            ; the 2nd line's non-first n-expressions are skipped (e.g., 5 6).
            ; We work around this by consuming an EOL immediately following
            ; the last n-expression on an initial indent line.
            ; This work-around fails if the first line ends in space or tab,
            ; but clisp REPL users are highly unlikely to notice this.
            ; This work-around won't affect correctly-working systems
            ; because without this code, it'd just skip a blank EOL anyway.
            ; Correctly-working systems include clisp's file-execution code
            ; and a clisp loop of (write (read)), interestingly enough.
            (if (member (my-peek-char stream) initial-comment-eol)
              (my-read-char stream))
            results-value)))
      (progn
        (consume-to-eol stream)
        (consume-end-of-line stream)
        empty-value))) ; (t-expr-real stream)

; Read a sweet-expression (t-expression).  Handle special
; cases, such as initial indent; call it-expr for normal case.
(defun t-expr-real (stream)
  (let* ((c (my-peek-char stream)))
        (cond
          ((lcomment-eolp c)
            (consume-to-eol stream)
            (consume-end-of-line stream)
            (t-expr-real stream))
          ((or (eql c form-feed) (eql c vertical-tab))
            (consume-ff-vt stream)
            (if (not (lcomment-eolp (my-peek-char stream)))
              (read-error "FF and VT must be alone on line in a sweet-expr"))
            (t-expr-real stream))
          ((char-icharp c) ; initial-indent-expr
            (accumulate-ichar stream) ; consume and throw away ichars
            (initial-indent-expr-tail stream))
          (t
            (let-splitter (results results-indent results-value)
                          (it-expr stream "^")
              (if (string= results-indent "")
                  (read-error "Closing *> without preceding matching <*."))
              results-value)))))

; Top level - read a sweet-expression (t-expression).  Handle special values.
(defun t-expr (stream)
  (let* ((te (t-expr-real stream)))
    (if (eq te empty-value)
        (t-expr stream)
        te)))

; Transition to read a sweet-expression (t-expression).
; Lisp "read" tried to read a character, and got redirected here.
; We will unread that character, and then invoke our own reader.
(defun t-expr-entry (stream char)
  (unread-char char stream)
  ; (princ "DEBUG entry: ") (write char) (terpri)
  (let ((*readtable* *underlying-sweet-readtable*))
    (handler-case (t-expr stream)
      ; Specially handle EOF so the underlying reader will see it.
      (end-of-file () (values)))))

; Set up a readtable that'll redirect everything.
(defun compute-sweet-redirect-readtable ()
  (setq *sweet-readtable*
    (let ((new (copy-readtable nil)))
      (set-syntax-from-char #\# #\' new) ; force # to not be dispatching char.
      (loop for ci from 0 upto my-char-code-limit
         do (set-macro-character (code-char ci) #'t-expr-entry nil new))
      new)))

(defun enable-sweet ()
  (when (setup-enable 'sweet)
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
    (setq *readtable* *sweet-readtable*))
  (values))

(defun sweet-read (&optional (stream *standard-input*))
  (let ((*readtable* *readtable*) ; Setup to restore on return.
        (*readable-active* *readable-active*))
    (enable-sweet)
    (read stream)))

