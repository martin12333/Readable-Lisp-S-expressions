
; Guile script for preprocessing readable/kernel.scm
; into a form that BiwaScheme can work with

(define (main argv)
  (let* ((srcdir      (if (> (length argv) 1)
                          (list-ref argv 1)
                          "."))
         (kernel      (open-input-file (string-append srcdir "/readable/kernel.scm")))
         (biwascheme  (open-input-file (string-append srcdir "/biwascheme/sweetbiwa.html.in")))
         (biwaout     (open-output-file "./biwascheme/sweetbiwa.html")))
    (loop kernel biwascheme biwaout)))

(define replace-code "@BIWA_READABLE_KERNEL@")
(define (loop kernel biwascheme biwaout)
  (let ((line (read-line biwascheme)))
    (cond
      ((eof-object? line)
        (exit 0))
      ((find-substring line replace-code)
        => (lambda (start)
             (display (substring line 0 start) biwaout)
             (output-kernel kernel biwaout)
             (display (substring line (+ start (string-length replace-code ))) biwaout)
             (loop kernel biwascheme biwaout)))
      (#t
        (display line biwaout)
        (loop kernel biwascheme biwaout)))))

(define (read-line port)
  ; TODO
  #t)
(define (find-substring main out)
  ; TODO
  #t)
(define (output-kernel kernel biwaout)
  ; TODO
  #t)

(main (command-line))

