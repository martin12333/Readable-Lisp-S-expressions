
; to run the test, do:
; guile -e test-tokenize -s tokenizer.scm

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
(define (tokenize port peek-char read-char eof-object? neoteric-read function)

  ; queue operations
  (define (q-new) (cons '() '()))
  (define (q-empty? q) (null? (car q)))
  (define (q-push q i)
    (if (q-empty? q)
      (begin
        (set-car! q (list i))
        (set-cdr! q (car q)))
      (begin
        (set-cdr! (cdr q) (list i))
        (set-cdr! q (cdr (cdr q))))))
  (define (q-pop q)
    (let ((rv (car (car q))))
      (set-car! q (cdr (car q)))
      rv))

  ; variables
  (let (; Flag, #t/#f, set if we are in initial indent state
        (initial-indent #t)
        ; Flag, #t/#f, set if we are at the start of a line
        (start-line #t)
        ; Stack of cons cells, '() if at beginning of expression.
        ; Each item in the stack is a string of the initial
        ; indent, with stack bottom always the empty string "".
        (indent-stack '())
        ; A queue of tokens still to be emitted.
        (pending-q (q-new)))

    (define (eol? c)
      (or
        (char=? c #\newline)
        (char=? c (integer->char 13))))
    (define (eat-line)
      (let ((c (read-char port)))
        (if (eol? c)
            ; check for Windows line-end terminator.
            (if (char=? c (integer->char 13))
              (let ((c (peek-char port)))
                (if (char=? c (integer->char 10))
                  (begin
                    (read-char port)
                    '())
                  '()))
              '())
            (eat-line))))

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

    (define (process-abbreviation token sym)
      (let ((c (peek-char port)))
        (if (hspace? c)
            token
            (let loop ((x (neoteric-read port)))
              (if (null? x)
                  (loop (neoteric-read port))
                  (cons sym x))))))

    (define (process-eof c)
      (let stack-emptiness-check ()
         (if (null? indent-stack)
           (begin
             (q-push pending-q c)
             (get-token))
           (let stack-same-check ()
             (if (string=? "" (car indent-stack))
               (begin
                 (set! indent-stack (cdr indent-stack))
                 (q-push pending-q 'SAME)
                 (if (null? indent-stack)
                   (stack-emptiness-check)
                   (begin
                     (q-push pending-q 'RESTART_END)
                     (stack-same-check))))
               (let stack-dedent-loop ()
                 (if (string=? "" (car indent-stack))
                   (begin
                     (set! indent-stack (cdr indent-stack))
                     (if (null? indent-stack)
                       (stack-emptiness-check)
                       (begin
                         (q-push pending-q 'RESTART_END)
                         (stack-same-check))))
                   (begin
                     (q-push pending-q 'DEDENT)
                     (set! indent-stack (cdr indent-stack))
                     (stack-dedent-loop)))))))))

    (define (get-token)
      (if (not (q-empty? pending-q))
        (q-pop pending-q)
        (let retry ((c (peek-char port)))
          (cond
            ((and (eof-object? c) initial-indent)
              ; act as if we successfully entered
              ; indent processing state.
              ; this is needed to properly process
              ; pending RESTART_END's.
              ;(set! start-line #f)
              ;(set! indent-stack (cons "" indent-stack))
              ;(set! initial-indent #f)
              ;(retry c)
              (process-eof c))
            ((and (eof-object? c) (not initial-indent))
              (process-eof c))
            (initial-indent
              (let* ((indent (get-hspace*))
                     (c (peek-char port)))
                (cond
                  ((or (char=? c #\;) (eol? c))
                    ; skip comments and empty lines
                    (eat-line)
                    (get-token))
                  ((string=? indent "")
                    ; no indent.  Set up state
                    (set! start-line #f)
                    (set! indent-stack (cons "" indent-stack))
                    (set! initial-indent #f)
                    (get-token))
                  ((char=? c #\!)
                    ; note! we expect that this will cause an
                    ; error in the caller, and we will exit
                    ; the state.
                    'INITIAL_INDENT_WITH_BANG)
                  (else
                    ; read the next item as a plain
                    ; s-expression.  Put it in pending
                    ; queue and return INITIAL_INDENT_NO_BANG.
                    (let ((next (read port)))
                      (q-push pending-q `(DATUM ,next))
                      'INITIAL_INDENT_NO_BANG)))))
            (start-line
              (let* ((current-indent (get-indentation))
                     (c (peek-char port)))
                (set! start-line #f)
                (cond
                  ((char=? c #\;)
                    ; skip comments
                    (eat-line)
                    (set! start-line #t)
                    (get-token))
                  ((eol? c)
                    ; completely empty line.  Need to
                    ; emit multiple DEDENT or a SAME,
                    ; pop off the indent-stack, and
                    ; enter initial-indent mode.
                    (if (string=? "" (car indent-stack))
                      ; already at minimum indent.  Emit
                      ; SAME terminator.
                      (q-push pending-q 'SAME)
                      ; need to emit DEDENT's to match
                      ; INDENT's.
                      (let loop ()
                        (if (not (string=? "" (car indent-stack)))
                          (begin
                            (q-push pending-q 'DEDENT)
                            (set! indent-stack (cdr indent-stack))
                            (loop)))))
                    (set! indent-stack (cdr indent-stack))
                    (set! initial-indent #t)
                    (get-token))
                  ((not (compatible-indent? current-indent (car indent-stack)))
                    'BADDENT)
                  ((> (string-length current-indent) (string-length (car indent-stack)))
                    ; indent.  Push on indent-stack and emit INDENT
                    (set! indent-stack (cons current-indent indent-stack))
                    'INDENT)
                  ((= (string-length current-indent) (string-length (car indent-stack)))
                    ; same indentation.
                    'SAME)
                  (else
                    ; dedent.  Push as many dedents as needed to match
                    ; the number of indents.
                    (let loop ()
                      (cond
                        ((= (string-length current-indent) (string-length (car indent-stack)))
                          ; stable state.
                          (get-token))
                        ((> (string-length current-indent) (string-length (car indent-stack)))
                          ; went past the indent.
                          'BADDENT)
                        (else
                          ; keep popping.
                          (q-push pending-q 'DEDENT)
                          (set! indent-stack (cdr indent-stack))
                          (loop))))))))
            ; -- At this point, we are not at the start of the line.
            ; check for abbrevations.  for
            ; now check only ' ` , ,@
            ; We'll check for the syntax
            ; abbreviations in a later
            ; revision, when we move
            ; some hash-processing code
            ; to here.
            ((char=? c #\')
              (read-char port)
              (process-abbreviation 'QUOTE_SPACE 'quote))
            ((char=? c #\`)
              (read-char port)
              (process-abbreviation 'QUASIQUOTE_SPACE 'quasiquote))
            ((char=? c #\,)
              (read-char port)
              (let ((c (peek-char port)))
                (if (char=? c #\@)
                  (begin
                    (read-char port)
                    (process-abbreviation 'UNQUOTE_SPLICING_SPACE 'unquote-splicing))
                  (process-abbreviation 'UNQUOTE_SPACE 'unquote))))
            ((char=? c (integer->char 12))
              'FF)
            ((char=? c (integer->char 11))
              'VT)
            ((or (char=? c #\;) (eol? c))
              ; got a line end!  Go to start-line state
              (eat-line)
              (set! start-line #t)
              (get-token))
            ((hspace? c)
              ; eat horizontal whitespace
              ; when not at start line -
              ; whitespace in the middle
              ; of a line that is not part
              ; of a neoteric-expression
              ; is not significant
              (get-hspace*)
              ; SPEC TODO: We can emit
              ; HSPACE instead.
              (get-token))
            ((member c '(#\{ #\( #\[ ))
              (let ((rv (neoteric-read port)))
                (if (null? rv)
                  'SCOMMENT
                  `(DATUM ,@rv))))
            (else
              (let ((rv (neoteric-read port)))
                (cond
                  ((eof-object? rv)
                    (process-eof rv))
                  ((equal? rv '())
                    'SCOMMENT)
                  ((equal? rv '( \\ ))
                    (get-hspace*)
                    'GROUP_SPLICE)
                  ((equal? rv '($))
                    (get-hspace*)
                    'SUBLIST)
                  ((eq? (car rv) '.)
                    (get-hspace*)
                    'PERIOD)
                  ((equal? rv '(<*))
                    (get-hspace*)
                    (set! initial-indent #t)
                    'RESTART_BEGIN)
                  ((equal? rv '(*>))
                    (if (string=? (car indent-stack) "")
                      (begin
                        (set! indent-stack (cdr indent-stack))
                        (q-push pending-q 'SAME))
                      (let loop ()
                        (if (string=? (car indent-stack) "")
                          (set! indent-stack (cdr indent-stack))
                          (begin
                            (set! indent-stack (cdr indent-stack))
                            (q-push pending-q 'DEDENT)
                            (loop)))))
                    (q-push pending-q 'RESTART_END)
                    (get-token))
                  (else
                    `(DATUM ,@rv)))))))))

    (function get-token)))

(define (test-tokenize . _)
  (let ((port (open-input-file "tokenizer.test")))
    (let test-loop ()
      ; search for opening bracket
      (let init-bracket-loop ()
        (let ((c (read-char port)))
          (cond
            ((eof-object? c)
              (display "\nall tests passed!\n")
              (close-input-port port))
            ((not (char=? c #\[))
              (init-bracket-loop))
            (else
              ; start tokenizing, with eof-object being
              ; the closing bracket character
              (let ((tokens '()))
                (tokenize
                  port
                  peek-char
                  read-char
                  ; fake eof
                  (lambda (c)
                    (and (char? c) (char=? c #\])))
                  ; fake neoteric reader
                  (lambda (port)
                    (let ((c (peek-char port)))
                      (if (char=? c #\])
                        #\]
                        (begin
                          (list (read port))))))
                  (lambda (get-token)
                    (let loop ()
                      (let ((token (get-token)))
                        (if (and (char? token) (char=? token #\]))
                          (set! tokens (reverse tokens))
                          (begin
                            (set! tokens (cons token tokens))
                            (loop)))))))
                ; consume the closing bracket if it's still there
                (let loop-close-bracket ()
                  (let ((c (peek-char port)))
                    (if (char=? c #\])
                      (begin
                        (read-char port)
                        (loop-close-bracket)))))
                ; now read the reference data.
                (let ((references '()))
                  (let read-reference ()
                    (let ((ref (read port)))
                      (cond
                        ((or (eq? ref 'EOF) (eof-object? ref))
                          (set! references (reverse references)))
                        (else
                          (set! references (cons ref references))
                          (read-reference)))))
                  ; check the data
                  (if (equal? references tokens)
                    (begin
                      (display "passed..\n")
                      (test-loop))
                    (begin
                      (display "failed!\nexpected: ")
                      (write references)
                      (display "\nactual: ")
                      (write tokens)
                      (display "\n")
                      (close-input-port port) )))))))))))

