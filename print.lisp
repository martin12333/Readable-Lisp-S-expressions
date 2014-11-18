;;;; Output routines for readable notations.
;;;; Use *print-notation* to decide what notation to use when writing,
;;;; which may be 'basic-curly-infix, 'full-curly-infix, 'neoteric, or 'sweet.

;;;; Some of this code is derived from SBCL, which was in turn
;;;; derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain.
;;;; Thus, this is derived from public domain code.
;;;; Much of the rest of the code is derived from the Scheme implementation
;;;; of the readable notation, which is also licensed under the MIT license.

;;;; Copyright (C) 2007-2014 by David A. Wheeler
;;;; This software is released as open source software under the "MIT" license:
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of this software and associated documentation files (the "Software"),
;;;; to deal in the Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;; and/or sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; The key function here is output-object-readable; outer functions call it,
;;;; and internal printing functions keep recursing back to it.

(cl:in-package :readable)

; Work around SBCL nonsense that makes its "defconstant" useless.
; See: http://www.sbcl.org/manual/Defining-Constants.html
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

(defvar *suppress-print-errors* nil
  #+sb-doc
  "Suppress printer errors when the condition is of the type designated by this
variable: an unreadable object representing the error is printed instead.")

(defvar *print-notation* nil
  #+sb-doc
  "Currently-active notation used in readable package's writers")

; Track errors in output-object-readable:
(defvar *readable-in-print-error* nil)

; WORKAROUND: This was already defined in basic-curly.lisp; we redeclare it
; here to inhibit compiling warnings.
(defvar *original-readtable*)

; TODO: Determine output stream.  Just return stream provided for now.
(defun out-synonym-of (stream)
  stream)

(defun allow-neoteric ()
  (member *print-notation* '(neoteric sweet)))

; The following functions are derived from the Scheme implementation
; in "kernel.scm"

; A list with more than this length and no pairs is considered "boring",
; and thus is presumed to NOT be a procedure call or execution sequence.
(define-constant boring-length 16)

(define-constant special-infix-operators '(and or xor))

(define-constant punct-chars
  `(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\-
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

; Write rest of list x's contents.
(defun write-list-contents-rest (x stream)
  (cond
    ((null x) (values))
    ((check-for-circularity x)
      (write-string " . " stream)
      (output-object-readable x stream))
    ((consp x)
      (princ " " stream)
      (output-object-readable (car x) stream)
      (write-list-contents-rest (cdr x) stream))
    (t
      (princ " . " stream)
      (output-object-readable x stream))))

(defun write-list-contents (x stream)
  (cond
    ((null x) (values))
    ((consp x)
      (output-object-readable (car x) stream)
      (write-list-contents-rest (cdr x) stream))
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
      (write-list-contents x stream)
      (princ "}" stream))
    (progn
      (output-object-readable (cadr x) stream)
      (infix-tail (car x) (cddr x) stream))))

;;; Main routine for outputting objects in current readable notation.
;;; Output OBJECT to STREAM observing all printer control variables
;;; except for *PRINT-PRETTY*. Note: if *PRINT-PRETTY* is non-NIL,
;;; then the pretty printer will be used for any components of OBJECT,
;;; just not for OBJECT itself.
(defun output-ugly-object-readable (object stream)
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
          (let ((*print-notation*
                  (if (eq *print-notation* 'full-curly-infix)
                      'neoteric
                      *print-notation*)))
               (output-object-readable (cadr object) stream)
               (infix-tail (car object) (cddr object) stream)))
        ((and (allow-neoteric)
              (long-and-boringp object boring-length))    ; Format (a b c ...)
          (princ "(" stream)
          (write-list-contents object stream)
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
          (write-list-contents (cdr object) stream)
          (princ ")" stream))
        (t                                                ; Format (1 2 3 ...)
          (princ "(" stream)
          (write-list-contents object stream)
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

; TODO: Implement fully.
;;; Could this object contain other objects? (This is important to
;;; the implementation of things like *PRINT-CIRCLE* and the dumper.)
(defun compound-object-p (x)
  (or (consp x)
      ; (%instancep x)
      (typep x '(array t *))))
(defun setup-printer-state ()
  nil)


;;;; circularity detection stuff

;;; When *PRINT-CIRCLE* is T, this gets bound to a hash table that
;;; (eventually) ends up with entries for every object printed. When
;;; we are initially looking for circularities, we enter a T when we
;;; find an object for the first time, and a 0 when we encounter an
;;; object a second time around. When we are actually printing, the 0
;;; entries get changed to the actual marker value when they are first
;;; printed.
(defvar *circularity-hash-table* nil)

;;; When NIL, we are just looking for circularities. After we have
;;; found them all, this gets bound to 0. Then whenever we need a new
;;; marker, it is incremented.
(defvar *circularity-counter* nil)

;;; Check to see whether OBJECT is a circular reference, and return
;;; something non-NIL if it is. If ASSIGN is true, reference
;;; bookkeeping will only be done for existing entries, no new
;;; references will be recorded. If ASSIGN is true, then the number to
;;; use in the #n= and #n# noise is assigned at this time.
;;;
;;; Note: CHECK-FOR-CIRCULARITY must be called *exactly* once with
;;; ASSIGN true, or the circularity detection noise will get confused
;;; about when to use #n= and when to use #n#. If this returns non-NIL
;;; when ASSIGN is true, then you must call HANDLE-CIRCULARITY on it.
;;; If CHECK-FOR-CIRCULARITY returns :INITIATE as the second value,
;;; you need to initiate the circularity detection noise, e.g. bind
;;; *CIRCULARITY-HASH-TABLE* and *CIRCULARITY-COUNTER* to suitable values
;;; (see #'OUTPUT-OBJECT for an example).
;;;
;;; Circularity detection is done in two places, OUTPUT-OBJECT and
;;; WITH-CIRCULARITY-DETECTION (which is used from PPRINT-LOGICAL-BLOCK).
;;; These checks aren't really redundant (at least I can't really see
;;; a clean way of getting by with the checks in only one of the places).
;;; This causes problems when mixed with pprint-dispatching; an object is
;;; marked as visited in OUTPUT-OBJECT, dispatched to a pretty printer
;;; that uses PPRINT-LOGICAL-BLOCK (directly or indirectly), leading to
;;; output like #1=#1#. The MODE parameter is used for detecting and
;;; correcting this problem.
(defun check-for-circularity (object &optional assign (mode t))
  (cond ((null *print-circle*)
         ;; Don't bother, nobody cares.
         nil)
        ((null *circularity-hash-table*)
          (values nil :initiate))
        ((null *circularity-counter*)
         (ecase (gethash object *circularity-hash-table*)
           ((nil)
            ;; first encounter
            (setf (gethash object *circularity-hash-table*) mode)
            ;; We need to keep looking.
            nil)
           ((:logical-block)
            (setf (gethash object *circularity-hash-table*)
                  :logical-block-circular)
            t)
           ((t)
            (cond ((eq mode :logical-block)
                   ;; We've seen the object before in output-object, and now
                   ;; a second time in a PPRINT-LOGICAL-BLOCK (for example
                   ;; via pprint-dispatch). Don't mark it as circular yet.
                   (setf (gethash object *circularity-hash-table*)
                         :logical-block)
                   nil)
                  (t
                   ;; second encounter
                   (setf (gethash object *circularity-hash-table*) 0)
                   ;; It's a circular reference.
                   t)))
           ((0 :logical-block-circular)
            ;; It's a circular reference.
            t)))
        (t
         (let ((value (gethash object *circularity-hash-table*)))
           (case value
             ((nil t :logical-block)
              ;; If NIL, we found an object that wasn't there the
              ;; first time around. If T or :LOGICAL-BLOCK, this
              ;; object appears exactly once. Either way, just print
              ;; the thing without any special processing. Note: you
              ;; might argue that finding a new object means that
              ;; something is broken, but this can happen. If someone
              ;; uses the ~@<...~:> format directive, it conses a new
              ;; list each time though format (i.e. the &REST list),
              ;; so we will have different cdrs.
              nil)
             ;; A circular reference to something that will be printed
             ;; as a logical block. Wait until we're called from
             ;; PPRINT-LOGICAL-BLOCK with ASSIGN true before assigning the
             ;; number.
             ;;
             ;; If mode is :LOGICAL-BLOCK and assign is false, return true
             ;; to indicate that this object is circular, but don't assign
             ;; it a number yet. This is neccessary for cases like
             ;; #1=(#2=(#2# . #3=(#1# . #3#))))).
             (:logical-block-circular
              (cond ((and (not assign)
                          (eq mode :logical-block))
                     t)
                    ((and assign
                          (eq mode :logical-block))
                     (let ((value (incf *circularity-counter*)))
                       ;; first occurrence of this object: Set the counter.
                       (setf (gethash object *circularity-hash-table*) value)
                       value))
                    (t
                     nil)))
             (0
              (if (eq assign t)
                  (let ((value (incf *circularity-counter*)))
                    ;; first occurrence of this object: Set the counter.
                    (setf (gethash object *circularity-hash-table*) value)
                    value)
                  t))
             (t
              ;; second or later occurrence
              (- value)))))))

;;; Handle the results of CHECK-FOR-CIRCULARITY. If this returns T then
;;; you should go ahead and print the object. If it returns NIL, then
;;; you should blow it off.
(defun handle-circularity (marker stream)
  (case marker
    (:initiate
     ;; Someone forgot to initiate circularity detection.
     (let ((*print-circle* nil))
       (error "trying to use CHECK-FOR-CIRCULARITY when ~
               circularity checking isn't initiated")))
    ((t :logical-block)
     ;; It's a second (or later) reference to the object while we are
     ;; just looking. So don't bother groveling it again.
     nil)
    (t
     (write-char #\# stream)
     (let ((*print-base* 10) (*print-radix* nil))
       (cond ((minusp marker)
              (write (- marker) :stream stream)
              (write-char #\# stream)
              nil)
             (t
              (write marker :stream stream)
              (write-char #\= stream)
              t))))))


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
  (labels ((print-it (stream)
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

