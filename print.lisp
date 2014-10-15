;;;; Output routines for readable notations.
;;;; Use *print-notation* to decide what notation to use when writing,
;;;; which may be 'basic-curly-infix, 'full-curly-infix, 'neoteric, or 'sweet.

;;;; Some of this code is derived from SBCL, which was in turn
;;;; derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain.
;;;; Thus, this is derived from public domain code.

;;;; The key function here is output-object-readable; outer functions call it,
;;;; and internal printing functions recurse back to it.

(cl:in-package :readable)

(defvar *suppress-print-errors* nil
  #+sb-doc
  "Suppress printer errors when the condition is of the type designated by this
variable: an unreadable object representing the error is printed instead.")

; Track errors in output-object-readable:
(defvar *readable-in-print-error* nil)

; Track previously-printed items
(defvar *circularity-hash-table* nil)

; TODO: Determine output stream.  Just return stream provided for now.
(defun out-synonym-of (stream)
  stream)

; Work around SBCL nonsense that makes its "defconstant" useless.
; See: http://www.sbcl.org/manual/Defining-Constants.html
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

(defun allow-neoteric ()
  (member *print-notation* '(neoteric sweet)))

; The following functions are derived from the Scheme implementation
; in "kernel.scm"

; A list with more than this length and no pairs is considered "boring",
; and thus is presumed to NOT be a procedure call or execution sequence.
(define-constant boring-length 16)

(define-constant special-infix-operators '(and or xor))

(define-constant punct-chars
  '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\-
    #\.  #\/ #\: #\; #\< #\= #\> #\p #\@ #\[ #\\ #\] #\^
    #\- #\` #\{ #\| #\} #\~ ))

; Returns t if x is a list with exactly 1 element.  Improper lists are nil.
(defun list1p (x)
  (and (consp x) (null (cdr x))))

; Returns t if x is a list with exactly 2 elements.  Improper lists are nil.
(defun list2p (x)
  (and (consp x) (consp (cdr x)) (null (cddr x))))

; Does x contain a list of ONLY punctuation characters?
; An empty list is considered true.
(defun contains-only-punctuationp (x)
  (cond ((null x) t)
        ((atom x) nil)
        ((member (car x) punct-chars)
         (contains-only-punctuationp (cdr x)))
        (t nil)))

; Returns t if x is a symbol that would typically be used in infix position.
(defun is-infix-operatorp (x)
  (cond ((not (symbolp x)) nil)
        ((member x special-infix-operators) t)
        (t
         (contains-only-punctuationp
           (coerce (symbol-name x) 'list)))))

; A possibly-improper list is long and boring if its length is at least
; num-to-go long and it's boring (it contains no pairs up to that length).
; A long-and-boring list is almost certainly NOT a function call or a
; body of some executable sequence - it's almost certainly a long
; boring list of data instead. If it is, we want to display it differently.
; This doesn't get stuck on circular lists; it always terminates after
; num-to-go iterations.
(defun long-and-boringp (x num-to-go)
  (cond
    ((consp (car x)) nil)
    ((not (consp (cdr x))) nil)
    ((<= num-to-go 1) t)
    (t (long-and-boringp (cdr x) (- num-to-go 1)))))

(defun list-no-longer-thanp (x num-to-go)
  (cond
    ((atom x) nil)
    ((null (cdr x)) t) ; This is the last one!
    ((not (consp (cdr x))) nil)
    ((<= num-to-go 0) nil)
    (t (list-no-longer-thanp (cdr x) (- num-to-go 1)))))

; Return t if x should be represented using curly-infix notation {...}.
(defun represent-as-infixp (x)
  (and (consp x)
       (consp (cdr x))                ; At least 2 elements.
       (is-infix-operatorp (car x))
       (list-no-longer-thanp x 6)))

(defun represent-as-inline-infixp (x)
  (and (represent-as-infixp x) (not (list2p x)))) ; Must be 3+ elements

; Return t if x should be represented as a brace suffix
(defun represent-as-brace-suffixp (x)
  (represent-as-infixp x))

; Define an association list mapping the Scheme procedure names which have
; abbreviations ==> the list of characters in their abbreviation
(define-constant abbreviations
  '((quote (#\'))
    (function (#\# #\'))))

; return t if we should as a traditional abbreviation, e.g., '
(defun represent-as-abbreviationp (x)
  (and (list2p x)
       (assoc (car x) abbreviations)))

; The car(x) is the symbol for an abbreviation; write the abbreviation.
(defun write-abbreviation (x stream)
  (mapc (lambda (c) (princ c stream))
    (cadr (assoc (car x) abbreviations))))


; Return list x's *contents* represented as a list of characters.
; Each one must use neoteric-expressions, space-separated;
; it will be surrounded by (...) so no indentation processing is relevant.
(defun n-write-list-contents (x stream)
  (cond
    ((null x) (values))
    ((consp x)
      (output-object-readable (car x) stream)
      (cond ((not (null (cdr x)))
        (princ " " stream)
        (n-write-list-contents (cdr x) stream))))
    (t
      (princ ". " stream)
      (output-object-readable x stream))))

(defun c-write-list-contents (x stream)
  (cond
    ((null x) (values))
    ((consp x)
      (output-object-readable (car x) stream)
      (cond ((not (null (cdr x)))
        (princ " " stream)
        (c-write-list-contents (cdr x) stream))))
    (t
      (princ ". " stream)
      (output-object-readable x stream))))

; Return tail of an infix expression, as list of chars
; The "op" is the infix operator represented as a list of chars.
(defun infix-tail (op x stream)
  (cond
    ((null x) (princ "}" stream))
    ((consp x)
      (princ " " stream)
      (output-object-readable op stream)
      (princ " " stream)
      (output-object-readable (car x) stream)
      (infix-tail op (cdr x) stream))
    (t
      (princ " " stream)
      (output-object-readable x stream)
      (princ "}" stream))))

; Return "x" as a list of characters, surrounded by {...}, for use as f{...}.
(defun as-brace-suffix (x stream)
  (princ "{" stream)
  (if (list2p x)
    (progn
      (n-write-list-contents x stream)
      (princ "}" stream))
    (progn
      (output-object-readable (cadr x) stream)
      (infix-tail (car x) (cddr x) stream))))

;;;; NOTE: Keep recursing back to output-object-readable

;;; Main routine for outputting objects in current readable notation.
;;; Output OBJECT to STREAM observing all printer control variables
;;; except for *PRINT-PRETTY*. Note: if *PRINT-PRETTY* is non-NIL,
;;; then the pretty printer will be used for any components of OBJECT,
;;; just not for OBJECT itself.
(defun output-ugly-object-readable (object stream)
  ; (princ "DEBUG: output-ugly-object-readable") (princ object)
  (typecase object
    (list
      (cond
        ((null object)
         (write object :stream stream))
        ((represent-as-abbreviationp object)              ; Format 'x
          (write-abbreviation object stream)
          (output-object-readable (cadr object) stream))
        ((represent-as-inline-infixp object)              ; Format {a + b}
          (princ "{" stream)
          (output-object-readable (cadr object) stream)
          (infix-tail (car object) (cddr object) stream))
        ((and (allow-neoteric)
              (long-and-boringp object boring-length))    ; Format (a b c ...)
          (princ "(" stream)
          (n-write-list-contents object stream)
          (princ ")" stream))
        ((and (allow-neoteric)                            ; Format f{...}
              (symbolp (car object))
              (list1p (cdr object))
              (consp (cadr object))
              (represent-as-infixp (cadr object)))
          (output-object-readable (car object) stream)
          (as-brace-suffix (cadr object) stream))
        ((and (allow-neoteric)                            ; Format f(...)
              (symbolp (car object)))
          (output-object-readable (car object) stream)
          (princ "(" stream)
          (n-write-list-contents (cdr object) stream)
          (princ ")" stream))
        (t                                                ; Format (1 2 3 ...)
          (princ "(" stream)
          (c-write-list-contents object stream)
          (princ ")" stream))))
    (symbol
      ; Workaround: clisp displays symbols oddly if readtable set
      #+clisp
      (let ((*readtable* *original-readtable*))
        (write object :stream stream))
      #-clisp
      (write object :stream stream))
    ; TODO: Many other types, including vectors, structures, hash tables, etc.
    ; ((vectorp x)
    ;   (princ "#( " stream) ; Surround with spaces, easier to implement.
    ;   (mapc (lambda (v) (c-write-simple v stream) (princ " " stream))
    ;     (vector->list x))
    ;   (princ ")" stream))
    (t
      (write object :stream stream))))

; TODO: Implement.
; (defun check-for-circularity (object t)
;  (values nil :initiate))
(defun handle-circularity (marker stream)
  nil)
(defun compound-object-p (object)
  nil)
(defun setup-printer-state ()
  nil)

;;; Objects whose print representation identifies them EQLly don't
;;; need to be checked for circularity.
(defun uniquely-identified-by-print-p (x)
  (or (numberp x)
      (characterp x)
      (and (symbolp x)
           (symbol-package x))))

;;; Output OBJECT to STREAM observing all printer control variables.
;;; This code is straight from SBCL, including its hairiness.
(defun output-object-readable (object stream)
  (declare (ignore *circularity-counter*))
  ; (princ "DEBUG: Begin output-object-readable")
  (labels ((print-it (stream)
             ; (princ "DEBUG: Within print-it")
             (if *print-pretty*
                 ; TODO: (sb!pretty:output-pretty-object object stream)
                 (output-ugly-object-readable object stream)
                 (output-ugly-object-readable object stream)))
           (handle-it (stream)
             (if *suppress-print-errors*
                 (handler-bind ((condition
                                  (lambda (condition) nil
                                    (when (typep condition *suppress-print-errors*)
                                      (cond (*readable-in-print-error*
                                             (write-string "(error printing " stream)
                                             (write-string *readable-in-print-error* stream)
                                             (write-string ")" stream))
                                            (t
                                             ;; Give outer handlers a chance.
                                             (with-simple-restart
                                                 (continue "Suppress the error.")
                                               (signal condition))
                                             (let ((*print-readably* nil)
                                                   (*print-escape* t))
                                               (write-string
                                                "#<error printing a " stream)
                                               (let ((*readable-in-print-error* "type"))
                                                 (output-object-readable (type-of object) stream))
                                               (write-string ": " stream)
                                               (let ((*readable-in-print-error* "condition"))
                                                 (output-object-readable condition stream))
                                               (write-string ">" stream))))
                                      (return-from handle-it object)))))
                   (print-it stream))
                 (print-it stream)))
           (check-it (stream)
             (multiple-value-bind (marker initiate)
                 (check-for-circularity object t)
               (if (eq initiate :initiate)
                   (let ((*circularity-hash-table*
                          (make-hash-table :test 'eq)))
                     (check-it (make-broadcast-stream))
                     (let ((*circularity-counter* 0))
                       (check-it stream)))
                   ;; otherwise
                   (if marker
                       (when (handle-circularity marker stream)
                         (handle-it stream))
                       (handle-it stream))))))
    (cond (;; Maybe we don't need to bother with circularity detection.
           (or (not *print-circle*)
               (uniquely-identified-by-print-p object))
           (handle-it stream))
          (;; If we have already started circularity detection, this
           ;; object might be a shared reference. If we have not, then
           ;; if it is a compound object it might contain a circular
           ;; reference to itself or multiple shared references.
           (or *circularity-hash-table*
               (compound-object-p object))
           (check-it stream))
          (t
           (handle-it stream)))))


(defun write-readable (object &key
                     ((:stream stream) *standard-output*)
                     ((:escape *print-escape*) *print-escape*)
                     ((:radix *print-radix*) *print-radix*)
                     ((:base *print-base*) *print-base*)
                     ((:circle *print-circle*) *print-circle*)
                     ((:pretty *print-pretty*) *print-pretty*)
                     ((:level *print-level*) *print-level*)
                     ((:length *print-length*) *print-length*)
                     ((:case *print-case*) *print-case*)
                     ((:array *print-array*) *print-array*)
                     ((:gensym *print-gensym*) *print-gensym*)
                     ((:readably *print-readably*) *print-readably*)
                     ((:right-margin *print-right-margin*)
                      *print-right-margin*)
                     ((:miser-width *print-miser-width*)
                      *print-miser-width*)
                     ((:lines *print-lines*) *print-lines*)
                     ((:pprint-dispatch *print-pprint-dispatch*)
                      *print-pprint-dispatch*)
                     ((:suppress-errors *suppress-print-errors*)
                      *suppress-print-errors*)
                     ((:notation *print-notation*) *print-notation*))
  #+sb-doc
  "Output OBJECT to the specified stream, defaulting to *STANDARD-OUTPUT*."
  (output-object-readable object (out-synonym-of stream))
  object)

(defun prin1-readable (object &optional stream)
  #+sb-doc
  "Output a mostly READable printed representation of OBJECT on the specified
  STREAM."
  (let ((*print-escape* t))
    (output-object-readable object (out-synonym-of stream)))
  object)

(defun princ-readable (object &optional stream)
  #+sb-doc
  "Output an aesthetic but not necessarily READable printed representation
  of OBJECT on the specified STREAM."
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (output-object-readable object (out-synonym-of stream)))
  object)

(defun print-readable (object &optional stream)
  #+sb-doc
  "Output a newline, the mostly READable printed representation of OBJECT, and
  space to the specified STREAM."
  (let ((stream (out-synonym-of stream)))
    (terpri stream)
    (prin1 object stream)
    (write-char #\space stream)
    object))

(defun pprint-readable (object &optional stream)
  #+sb-doc
  "Prettily output OBJECT preceded by a newline."
  (let ((*print-pretty* t)
        (*print-escape* t)
        (stream (out-synonym-of stream)))
    (terpri stream)
    (output-object-readable object stream))
  (values))


;;; This produces the printed representation of an object as a string.
;;; The few ...-TO-STRING functions call this.
(defun stringify-object-readable (object)
  (let ((stream (make-string-output-stream)))
    (setup-printer-state)
    (output-object-readable object stream)
    (get-output-stream-string stream)))

(defun write-to-string-readable
    (object &key
            ((:escape *print-escape*) *print-escape*)
            ((:radix *print-radix*) *print-radix*)
            ((:base *print-base*) *print-base*)
            ((:circle *print-circle*) *print-circle*)
            ((:pretty *print-pretty*) *print-pretty*)
            ((:level *print-level*) *print-level*)
            ((:length *print-length*) *print-length*)
            ((:case *print-case*) *print-case*)
            ((:array *print-array*) *print-array*)
            ((:gensym *print-gensym*) *print-gensym*)
            ((:readably *print-readably*) *print-readably*)
            ((:right-margin *print-right-margin*) *print-right-margin*)
            ((:miser-width *print-miser-width*) *print-miser-width*)
            ((:lines *print-lines*) *print-lines*)
            ((:pprint-dispatch *print-pprint-dispatch*)
             *print-pprint-dispatch*)
            ((:suppress-errors *suppress-print-errors*)
             *suppress-print-errors*)
            ((:notation *print-notation*) *print-notation*))
  #+sb-doc
  "Return the printed representation of OBJECT as a string."
  (stringify-object-readable object))

(defun prin1-to-string-readable (object)
  #+sb-doc
  "Return the printed representation of OBJECT as a string with
   slashification on."
  (let ((*print-escape* t))
    (stringify-object-readable object)))

(defun princ-to-string-readable (object)
  #+sb-doc
  "Return the printed representation of OBJECT as a string with
  slashification off."
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (stringify-object-readable object)))

