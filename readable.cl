; readable.cl
; Implements the "readable" approach for Lisp.
; On load, it turns on the curly-infix reader macro;
; {...} surrounds an infix expression, so {7 + 8} maps to (+ 7 8) on read.
;
; Copyright (C) 2007-2012 by David A. Wheeler
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
;
;
; Implements an infix reader macro: {...} surrounds an infix expression.
; E.G., {2 + 3 + 4} is transformed at read time into (+ 2 3 4), and
; {2 + {3 * 4}} transforms into (+ 2 (* 3 4)).
; This is useful when you want simple yet flexible infix capabilities,
; without complications. It's also a VERY simple implementation, so there's
; less to "go wrong".  You can optionally use it in conjunction with an
; execution/compile-time macro to support precedence, if you want.
;
; This reader macro is invoked on expressions surrounded by {...}, and
; presumes that all infix operators are surrounded by whitespace.
; If the expression has (1) an odd number of parameters,
; (2) at least 3 parameters, and (3) all the even parameters are equal symbols,
; then it's "simple infix" and maps to (even-parameter odd-parameters).
; Otherwise, it's not simple, and it maps to (nfx parameters).
; Thus:
;  {2 * n}       maps to (* 2 n)
;  {x eq y}      maps to (eq x y)
;  {2 + 3 + 4}   maps to (+ 2 3 4)   - chaining/fungibility works
;  {2 + {3 * 4}} maps to (+ 2 (* 3 4)) - Nesting works + keeps things simple
;  {2 + 3 * 4}   maps to (nfx 2 + 3 * 4) - non-simple.
;
; If you always use simple infix expressions, e.g., by never mixing
; operators in the same list, then every list (after reading)
; will have the traditional order of operator-first. This means that simple
; infix expressions can be used inside ANY execution/compile-time macro.
; In simple infix expressions you can use ANY function name as an
; infix operator, e.g., {"hi" equal "hi"} works quite well - and you do
; not need to register any function name before you use it.
; You can nest simple infix expressions to keep each one simple, e.g.,
; {2 + {3 * 4}}.  You can nest ordinary lists too, e.g., to use
; prefix functions.  Thus {(- x) / 2} maps to (/ (- x) 2).
;
; If you want automatic precedence, define a macro named "nfx"
; and have it implement the precedence. Note that if another macro recurses
; into the nfx expression BEFORE nfx has a chance to modify its parameters,
; the other macro will see the lists in a different order than it may expect.
; Precedence is intentionally NOT handled by this reader macro,
; because it's easy to get precedence quietly wrong when using a reader macro.
; Instead, this reader macro is designed to work WITH an execution-time
; macro if that's desired.
;
; If want to avoid using an "nfx" macro entirely, just define the
; "nfx" macro as an error.
;
; Warning: if you use an "nfx" macro, don't have nfx override the name
; of an existing function, or it will be confusing to use.  E.G., if
; "nfx" renames "=" as "setf", then you could have this confusing case:
; {x = 3 * 4} maps to (nfx x = 3 * 4) maps to (setf x (* 3 4)), but
; {x = 3} maps to (= x 3) which is a comparison, not a value-setting operation.
; Instead, create normal functions/macros for each infix operator and just
; use them directly, e.g., use "<-" for assignment and define a macro
; to do what you want.
;
; Pros:
; * Easy to use: Can use ANY operator name as infix, WITHOUT any registration
; * Syntax makes obvious where new lists occur (no "hidden list creation")
; * Easily understood/verified implementation - less to "go wrong"
; * Works 100% trivially with execution macros, particularly if only
;   "simple" infix forms are used.
; * No precedence list that must be memorized (unless you use an "nfx" macro)
; * Can work with an nfx macro (which COULD implement precedence) if needed
; Cons:
; * Doesn't directly support precedence (if you want that) - nfx macro
;   must do that.
; * Doesn't rename functions, or give them new infix names.  You can define
;   functions/macros with traditional infix names separately, if desired.


; Utility functions to implement the simple infix system:

; Return true if lyst has an even # of parameters, and the (alternating) first
; ones are "op".  Used to determine if a longer lyst is infix.
; Otherwise it returns NIL (False).
; If passed empty list, returns true (so recursion works correctly).
(defun even-and-op-prefix (op lyst)
   (cond
     ((null lyst) t)
     ((not (consp lyst)) nil) ; Not a list.
     ((not (eq op (car lyst))) nil) ; fail - operators not all equal.
     ((null (cdr lyst)) nil) ; fail - odd # of parameters in lyst.
     (t (even-and-op-prefix op (cddr lyst))))) ; recurse.

; Return True if the lyst is in simple infix format (and should be converted
; at read time).  Else returns NIL.
(defun simple-infix-listp (lyst)
  (and
    (consp lyst)           ; Must have list;  '() doesn't count.
    (consp (cdr lyst))     ; Must have a second argument.
    (consp (cddr lyst))    ; Must have a third argument (we check it
                           ; this way for performance)
    (symbolp (cadr lyst))  ; 2nd parameter must be a symbol.
    (even-and-op-prefix (cadr lyst) (cdr lyst)))) ; even parameters equal?

; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
(defun alternating-parameters (lyst)
  (if (or (null lyst) (null (cdr lyst)))
    lyst
    (cons (car lyst) (alternating-parameters (cddr lyst)))))

; Transform not-simple infix list.  Written as a separate function so that
; future versions/specifications can easily replace just this pieces.
(defun transform-mixed-infix (lyst)
  (cons 'nfx lyst))

; The following install the {...} reader.
; See "Common Lisp: The Language" by Guy L. Steele, 2nd edition,
; pp. 542-548 and pp. 571-572.


; Take list that was in {...}, convert to final form.
(defun process-curly (lyst)
  (cond
    ((not (consp lyst)) ; E.G., map {} to ().
      lyst)
    ((null (cdr lyst)) ; Map {a} to a.
      (car lyst))
    ((and (consp (cdr lyst)) (null (cddr lyst))) ; Map {a b} to (a b).
      lyst)
    ((simple-infix-listp lyst) ; Map {a op b} to (op a b).
      (cons (cadr lyst) (alternating-parameters lyst)))
    (t
      (transform-mixed-infix lyst))))

; Read until }, then process list as infix list.
; This shouldn't be invoked once we use neoteric-read; it takes precedence.
(defun curly-brace-infix-reader (stream char)
  (let ((result (read-delimited-list #\} stream t)))
    (process-curly result)))

; Invoke curly-brace-infix-reader when "{" is read in:
(set-macro-character #\{ #'curly-brace-infix-reader)

; This is necessary, else a cuddled } will be part of an atom:
(set-macro-character #\} (get-macro-character #\) nil))

; Make "[" and "]" delimiters. For now, make same as ().
(set-macro-character #\[ (get-macro-character #\( nil))

; This is necessary, else a cuddled } will be part of an atom:
(set-macro-character #\] (get-macro-character #\) nil))

; This is a no-op; we enable curly-infix on load.
; But for consistency, we'll provide the function anyway.
(defun enable-curly-infix ())

; Nonsense marker for eof
(defvar neoteric-eof-marker (cons 'eof '()))


(defun my-read-delimited-list (stop-char input-stream)
 (handler-case
  (let*
    ((c (peek-char t input-stream)))
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
        (let ((datum (neoteric-read input-stream)))
          (cond
             ((eql datum '|.|)
               (let ((datum2 (neoteric-read input-stream)))
                 (consume-whitespace input-stream)
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
                (cons 'bracketaccess
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


; Read, then process it through neoteric-tail to manage suffixes.
; Unlike Scheme, in Common Lisp we can force the reader to consider
; {} and [] as delimiters, so we don't have to re-implement some parts of
; the reader.  However, Common Lisp's approach to backquote and comma is
; more complicated to implement.
(defun neoteric-read (&optional (input-stream *standard-input*)
                        (eof-error-p t) (eof-value nil) (recursive-p nil))
 (handler-case
  (neoteric-process-tail input-stream
    (let* ((c (peek-char t input-stream)))
      (cond
        ((eql c #\( )
          (read-char nil input-stream)
          (my-read-delimited-list #\) input-stream))
        ((eql c #\{ )
          (read-char nil input-stream)
          (process-curly
            (my-read-delimited-list #\} input-stream)))
        ((eql c #\; )
          (read-line input-stream) ; Consume rest of the line.
          (neoteric-read input-stream eof-error-p eof-value recursive-p))
        ((eql c #\' )
          (read-char nil input-stream)
          (list 'quote
            (neoteric-read input-stream eof-error-p eof-value recursive-p)))
        ; TODO: Add support for comma, backquote, etc.
        ; The Common Lisp hyperspec says in
        ; "2.4.6.1 Notes about Backquote":
        ; Since the exact manner in which the Lisp reader will parse an
        ; expression involving the backquote reader macro is not specified,
        ; an implementation is free to choose any representation that
        ; preserves the semantics described.
        ; Often an implementation will choose a representation that
        ; facilitates pretty printing of the expression, so that
        ; (pprint `(a ,b)) will display `(a ,b) and not, for example,
        ; (list 'a b). However, this is not a requirement.
        ; Implementors who have
        ; no particular reason to make one choice or another might wish
        ; to refer to IEEE Standard for the Scheme Programming Language,
        ; which identifies a popular choice of representation for such
        ; expressions that might provide useful to be useful compatibility
        ; for some user communities. There is no requirement, however,
        ; that any conforming implementation use this particular
        ; representation. This information is provided merely for
        ; cross-reference purposes.
        (t
          ; Force recursive; even at the top level, when it reads what
          ; it *thinks* is a whole expression, we may be "inside" a
          ; longer list.  Otherwise, EOF handling can mess up.
          (read input-stream eof-error-p eof-value t)))))
  (end-of-file (e)
    (if eof-error-p
      (signal e) ; re-raise error
      eof-value))))

; Remarkably, Common Lisp provides no standard way to exit an image.
; Here's a mostly-portable mechanism to do so, from:
; http://www.cliki.net/Portable%20Exit
(defun my-portable-exit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:quit
              :unix-code (typecase code (number code) (null 0) (t 1)))
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; This group from 
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'quit code)))

; Common Lisp provides no way to fully override the "read" function
; such as (defun read #'neoteric-read).
; We partly simulate it here by writing our own REPL, and exiting Lisp
; when it ends.  The simulation is quite imperfect (a call to "read"
; reveals the facade), but it's a start.
; Perhaps we should special-case "load" here.
(defun enable-neoteric ()
  (handler-case
    (do ((result (neoteric-read) (neoteric-read)))
      (nil nil)
      (write (eval result))
      (terpri))
    (end-of-file () (my-portable-exit 0))))

(defun neoteric-filter ()
  (handler-case
    (do ((result (neoteric-read) (neoteric-read)))
      (nil nil)
      (write result)
      (terpri))
    (end-of-file ())))

(defun neoteric-load (filename)
 (handler-case
  (with-open-file (s (make-pathname :name filename) :direction :input)
    (do ((result (neoteric-read s) (neoteric-read s)))
      (nil nil)
      (eval result)))
  (end-of-file () )))

; TODO: Handle improper lists, e.g., {a + b . c} should insert nfx, and
; port(a . b) should generate (port a . b).

; TODO: Handle #|...|# comments inside neoteric-read.


; Likely things to do from here:

; (enable-neoteric)
; (write (neoteric-read))

; (do ((result (read) (read)))
;   (nil nil)
;   (write result)
;   (terpri))

