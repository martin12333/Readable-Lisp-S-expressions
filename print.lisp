;;;; Output routines for readable notations.
;;;; Use *print-notation* to decide what notation to use when writing.

;;;; Some of this code is derived from SBCL, which was in turn
;;;; derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain.
;;;; Thus, this is derived from public domain code.

(cl:in-package :readable)

(defvar *suppress-print-errors* nil
  #+sb-doc
  "Suppress printer errors when the condition is of the type designated by this
variable: an unreadable object representing the error is printed instead.")

; TODO: Determine output stream.  Just return stream provided for now.
(defun out-synonym-of (stream)
  stream)

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


;;; TODO: Output but possibly using extended notation (depending on
;;; the value of *print-notation*). Use the traditional values of "write".
;;; The following is a stub.
; (defun output-object-readable (object stream)
;    (output-object object stream))


;;; Objects whose print representation identifies them EQLly don't
;;; need to be checked for circularity.
(defun uniquely-identified-by-print-p (x)
  (or (numberp x)
      (characterp x)
      (and (symbolp x)
           (symbol-package x))))

; Track errors in output-object-readable:
(defvar *readable-in-print-error* nil)

;;; Output OBJECT to STREAM observing all printer control variables.
;;; This code is straight from SBCL, including its ugliness.
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

;;; Output OBJECT to STREAM observing all printer control variables
;;; except for *PRINT-PRETTY*. Note: if *PRINT-PRETTY* is non-NIL,
;;; then the pretty printer will be used for any components of OBJECT,
;;; just not for OBJECT itself.
; TODO
(defun output-ugly-object-readable (object stream)
  ; (princ "DEBUG: output-ugly-object-readable") (princ object)
  (write object :stream stream))


; (prin1-readable '(TESSSSSTING 1 2 3))

