
(define (preprocess-port inport outport)
  (let
    (
      (dot-flag #f)
      (stack (list -1)))
    (define (stack-push i) (set! stack (cons i stack)))
    (define (stack-top) (car stack))
    (define (stack-pop) (let ((rv (car stack))) (set! stack (cdr stack)) rv))
    ;
    (define cr (integer->char 13))
    (define lf (integer->char 10))
    (define open-paren #\()
    (define close-paren #\))
    ;
    (define (get-indentation-level)
      (let loop ((i 0))
        (let ((c (peek-char inport)))
          (cond
            ((eof-object? c)     c)
            ((char=? c #\space)  (begin
                                   (write-char (read-char inport) outport)
                                   (loop (+ i 1))))
            (#t                  i)))))
    (define (copy-line)
      (let loop ()
        (let ((c (read-char inport)))
          (cond
            ((eof-object? c) c)
            ((char=? c cr)
              (write-char c outport)
              (if (char=? (peek-char inport) lf)
                (write-char (read-char inport) outport))
              '())
            ((char=? c lf)
              (write-char c outport)
              '())
            (#t
              (write-char c outport)
              (loop))))))
    (define (check-dot)
      (if (eqv? (peek-char inport) #\.)
        (begin
          (read-char inport)
          #t)
        #f))
    (define (process-indent current)
      (let
        (
          (now-dot-flag (check-dot))
          (top          (stack-top)))
        (cond
          ((> current top)
            (cond
              (dot-flag
                (error 'arne "line after dotted line is more indented"))
              (now-dot-flag
                (set! dot-flag #t))
              (#t
                (write-char open-paren outport)
                (set! dot-flag #f)))
            (stack-push current))
          ((= current top)
            (if (not dot-flag)
              (write-char close-paren outport))
            (if now-dot-flag
              (set! dot-flag #t)
              (begin
                (write-char open-paren outport)
                (set! dot-flag #f))))
          (#t
            (if (not dot-flag)
              (write-char close-paren outport))
            (let loop ()
              (if (>= current (stack-top))
                '()
                (begin
                  (write-char close-paren outport)
                  (stack-pop)
                  (loop))))
            (if now-dot-flag
              (set! dot-flag #t)
              (begin
                (write-char open-paren outport)
                (set! dot-flag #f)))))))
    (define (process)
      (let ((indent (get-indentation-level)))
        (cond
          ((eof-object? indent)  (begin
                                   (process-indent 0)
                                   '()))
          (#t                    (begin
                                   (process-indent indent)
                                   (if (eof-object? (copy-line))
                                     (begin
                                       (process-indent 0)
                                       '())
                                     (process)))))))
    ;
    (process)))

(define (preprocess-file infile outfile)
  (let ((inport #f) (outport #f))
    (dynamic-wind
      (lambda ()
        (set! inport (open-input-file infile)))
      (lambda ()
        (dynamic-wind
          (lambda ()
            (set! outport (open-output-file outfile)))
          (lambda () (preprocess-port inport outport))
          (lambda ()
            (close-output-port outport))))
      (lambda ()
        (close-input-port inport)))))

