; curly-infix.cl
; Implements an infix reader macro: {...} surrounds an infix expression.
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
; * Doesn't support traditional prefix notation, e.g., - x. Must use (- x).
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

; Transform a simple infix list - move the 2nd parameter into first position,
; followed by all the odd parameters.  Thus (3 + 4 + 5) => (+ 3 4 5).
(defun transform-simple-infix (lyst)
   (cons (cadr lyst) (alternating-parameters lyst)))

; Transform not-simple infix list.  Written as a separate function so that
; future versions/specifications can easily replace just this pieces.
(defun transform-not-simple-infix (lyst)
  (cons 'nfx lyst))

; The following install the {...} reader.
; See "Common Lisp: The Language" by Guy L. Steele, 2nd edition,
; pp. 542-548 and pp. 571-572.


; Read until }, then process list as infix list.
; If it's a simple infix list (odd # parameters, 3+ parameters, all even
; parameters are equal symbols) then transform to infix. E.G.,
;   {3 + 4 + 5} => (+ 3 4 5).
; Otherwise, transform to (nfx list), and presume that some macro named
; "nfx" will take care of things.
(defun curly-brace-infix-reader (stream char)
  (declare (ignore char))
  (let ((result (read-delimited-list #\} stream t)))
    (if (simple-infix-listp result)
       (transform-simple-infix result) ; Simple infix expression.
       (transform-not-simple-infix result))))

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

; Nonsense "eof-marker" to mark end of file.
(defvar neoteric-eof-marker (cons 'eof '()))

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
              (cons prefix (read-delimited-list #\) input-stream t))))
        ((eql c #\[ )  ; Implement f[x]
          (read-char input-stream nil nil t) ; consume opening char
          (neoteric-process-tail input-stream
                (cons 'bracketaccess
                  (cons prefix
                    (read-delimited-list #\] input-stream t)))))
        ((eql c #\{ )  ; Implement f{x}
          ; Do not consume opening char, recurse instead.
          (neoteric-process-tail input-stream
              (list prefix
                ; Call neoteric-read-real, which handles {...} curly-infix.
                (neoteric-read input-stream t nil t))))
        (t prefix))))

; Read, then process it through neoteric-tail to manage suffixes.
(defun neoteric-read (&optional (input-stream *standard-input*)
                        (eof-error-p t) (eof-value nil) (recursive-p nil))
  (neoteric-process-tail input-stream
    ; Force recursive; even at the top level, when it reads what it *thinks*
    ; is a whole expression, we may be "inside" a longer list.  Otherwise,
    ; EOF handling can mess up in enable-neoteric:
    (read input-stream eof-error-p eof-value t)))

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
    (do ()
      (nil nil)
      (write (eval (neoteric-read)))
      (terpri))
    (end-of-file () (my-portable-exit 0))))

(defun neoteric-filter ()
  (handler-case
    (do ()
      (nil nil)
      (write (neoteric-read))
      (terpri))
    (end-of-file ())))

(defun neoteric-load (filename)
  (with-open-file (stream filename)
    (do ((result (neoteric-read)))
      (nil nil)
      (eval result))))

; (enable-neoteric)

; TODO: Handle comments inside neoteric-read.  This can be done by
;       setting the #| handler on entry to neoteric-read (if not already
;       set), and then restoring it if had been set earlier.


