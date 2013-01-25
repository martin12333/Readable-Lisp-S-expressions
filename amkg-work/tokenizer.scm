
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

