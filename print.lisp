;;;; Output routines for readable notations.
;;;; Use *print-notation* to decide what notation to use when writing.

;;;; Some of this code is derived from SBCL, which was in turn
;;;; derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain.

(cl:in-package :readable)

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
  (stringify-object object))

(defun prin1-to-string-readable (object)
  #+sb-doc
  "Return the printed representation of OBJECT as a string with
   slashification on."
  (let ((*print-escape* t))
    (stringify-object object)))

(defun princ-to-string-readable (object)
  #+sb-doc
  "Return the printed representation of OBJECT as a string with
  slashification off."
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (stringify-object object)))

;;; This produces the printed representation of an object as a string.
;;; The few ...-TO-STRING functions above call this.
(defun stringify-object-readable (object)
  (let ((stream (make-string-output-stream)))
    (setup-printer-state)
    (output-object-readable object stream)
    (get-output-stream-string stream)))


;;; TODO: Output but possibly using extended notation (depending on
;;; the value of *print-notation*). Use the traditional values of "write".
;;; The following is a stub.
(defun output-object-readable (object stream)
    (output-object object stream))


