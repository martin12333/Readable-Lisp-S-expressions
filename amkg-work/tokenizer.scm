
; port - an ordinary R5RS port with read-char and peek-char
; neoteric-read - a read function for neoteric, with the
;   following return protocol:
;     '() -> the reader got a comment starting with #,
;            such as #| ... |# or #;
;     `(,datum) -> the reader got a datum.
;   (lambda (port) (list (read x))) can be used if you don't
;   have a neoteric reader yet and you will not use the
;   #-comments.
; function - a function of the form:
;   (lambda (get-token)
;     ... (get-token) ... )
; The function is given the function get-token, which
; returns the next token.
(define (tokenize port neoteric-read function)
  ; variables
  (let (; Flag, #t/#f, set if we are at the start of a line
        (start-line #t)
        ; Stack of cons cells, '() if at beginning of expression.
        ; Each item in the stack is a string of the initial
        ; indent, with stack bottom always the empty string "".
        (indent-stack '())
        ; A number representing the number of pending DEDENT
        ; tokens to emit.
        (pending-dedents 0)
        ; Set to the eof object if end-of-file has been
        ; reached.  Note that pending-dedents is higher
        ; priority, since we need to emit dedents before
        ; we emit the actual eof.
        ; Store the actual eof here because R5RS has no
        ; method of generating an EOF.
        (end-of-file #f))

    (define (eol? c)
      (or
        (char=? c #\newline)
        (char=? c (integer->char 13))))
    (define (eat-line)
      (let ((c (peek-char port)))
        (if (eol? c)
            '()
            (begin
              (read-char port)
              (eat-line)))))

    (define (hspace? c)
      (or
        (char=? c #\space)
        (char=? c (integer->char 9))))
    (define (get-while predicate?)
      (let loop ((cs '()))
        (let ((c (peek-char port)))
          (if (predicate? c)
            (loop (cons (read-char port) cs))
            (list->string (reverse cs))))))
    (define (get-hspace*)
      (get-while hspace?))
    (define (get-indentation)
      (get-while
        (lambda (c)
          (or
            (hspace? c)
            (char=? c #\!)))))

    (define (compatible-indent? i1 i2)
      (if (< (string-length i1) (string-length i2))
          (compatible-indent? i2 i1)
          (string=?
            (substring i1 0 (string-length i2))
            i2)))

    (define (process-indentation current-indent)
      (let* ((top-indent (car indent-stack))
             (l-current-indent (string-length current-indent))
             (l-top-indent (string-length top-indent)))
        (cond
          ((not (compatible-indent? current-indent top-indent))
            'BADDENT)
          ((< l-top-indent l-current-indent)
            ; Greater indent; push on indent stack.
            (set! indent-stack (cons current-indent indent-stack))
            'INDENT)
          ((= l-top-indent l-current-indent)
            'SAME)
          (else ; (> l-top-indent l-current-indent)
            ; pop-off one
            (set! indent-stack (cdr indent-stack))
            ; start popping off
            (let loop ((pop-offs 1))
              (let* ((top-indent (car indent-stack))
                     (l-top-indent (string-length top-indent)))
                (cond
                  ((< l-top-indent l-current-indent)
                    ; This case:
                    ;|foo
                    ;|    bar
                    ;| quux
                    'BADDENT)
                  ((= l-top-indent l-current-indent)
                    ; stop popping off indentation levels.
                    ; we'll be returning a DEDENT ourselves, so
                    ; pending dedents is one minus the number of
                    ; indentations popped off.
                    (set! pending-dedents (- pop-offs 1))
                    'DEDENT)
                  (else
                    ; still need to pop off an indent level
                    (set! indent-stack (cdr indent-stack))
                    (loop (+ 1 pop-offs))))))))))

    (define (get-token)
      (cond
        ((> pending-dedents 0)
          (set! pending-dedents (- pending-dedents 1))
          'DEDENT)
        (end-of-file
          end-of-file)
        (else
          (let ((c (peek-char port)))
            (cond
              ((eof-object? c)
                ; On Guile, peeking an eof-object will
                ; actually consume it.  So set a flag
                ; that will prevent us from trying to
                ; peek a char again, as it will be
                ; somewhat confusing to terminal users.
                (set! end-of-file c)
                ; Now act as if we found an indentation
                ; at column 0.
                ; *technically* this is wrong if eof is
                ; not on a line by itself.
                (process-indentation ""))
              ; at start of line, and we have consumed
              ; at least one line.  Always get indent
              ; in such a case.
              ((and line-start
                    (not (null? indent-stack)))
                (let* ((current-indent (get-indentation))
                       (c (peek-char port)))
                  (set! line-start #f)
                  (cond
                    ; Is the line completely a comment line?
                    ((char=? c #\;)
                      ; eat the line and retry (effectively,
                      ; skips the entire line completely.)
                      (eat-line)
                      (set! line-start #t)
                      (get-token))
                    ; Is the line actually empty?
                    ((eol? c)
                      (eat-line)
                      (set! line-start #t)
                      ; act as if we got an empty
                      ; indentation first so that we can
                      ; set up dedents.
                      (let ((indent-code (process-indentation "")))
                        ; now that dedents have been set up, clear
                        ; the stack completely, since we are now in
                        ; the state "waiting for new top-level
                        ; expression".
                        (set! indent-stack '())
                        indent-code ))
                    (else
                      ; process the indentation
                      (process-indentation current-indent)))))
              (line-start
                ; in this state, we are waiting for a new top-level
                ; expression.  Check for initial indents as well as
                ; the possible errors.
                (let* ((current-indent (get-hspace*))
                      ((c (peek-char port))))
                  (cond
                    ((or (eol? c) (char=? c #\;))
                      ; line is completely comment or empty.  Ignore.
                      (eat-line)
                      (get-token))
                    ((char=? c #\!)
                      ; oh no, ! in initial indent.
                      ; we expect this to be an error.
                      'INITIAL_INDENT_WITH_BANG)
                    ((string=? current-indent "")
                      ; what we expect: no indent.  Set up state,
                      ; and begin processing for real.
                      (set! line-start #f)
                      (set! indent-stack (list ""))
                      (get-token))
                    (else
                      ; Indented s-expression.  We expect the
                      ; caller to stop calling get-token.
                      'INITIAL_INDENT_NO_BANG))))
              ; skip hspace
              ((hspace? c)
                (get-hspace*)
                (get-token))
              ((char=? c (integer->char 11)
                (read-char port)
                'VT))
              ((char=? c (integer->char 12)
                (read-char port)
                'FF))
              ; comment_eol
              ((or (char=? c #\;) (eol? c))
                (eat-line)
                (set! line-start #t)
                'EOL)
              ; escapes { \\ } [ . \\ ] ( . \\ )
              ((initial-delimiter? c)
                ; can't possibly be a comment.
                `(n-expr ,@(neoteric-read port)))
              (else
                (let ((datum (neoteric-read port)))
                  (cond
                    ((null? datum)
                      'SCOMMENT)
                    ((equal? datum '(\\))
                      'GROUP_SPLICE)
                    ((equal? datum '($))
                      'SUBLIST)
                    ; TODO: RESTART_BEGIN and RESTART_END
                    ; (they need more complex state to
                    ; handle, possibly a stack of indent
                    ; stacks or indent-stack-stack)
                    (else
                      `(n-expr ,@datum))))))))))

    (function get-token)))

(define (test-tokenize)
  (tokenize
    (current-input-port)
    (lambda (port) (list (read port)))
    (lambda (get-token)
      (let loop ()
        (let ((token (get-token)))
          (if (eof-object? token)
              '()
              (begin
                (write token)
                (newline)
                (loop))))))))

