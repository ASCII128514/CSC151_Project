#lang racket
 (require racket/cmdline)
(define path (vector-ref (current-command-line-arguments) 0))
(require csv-reading)
(define make-food-csv-reader
  (make-csv-reader-maker
   '(
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
(define next-row (make-food-csv-reader (open-input-file path)))
(define csv-writer
  (lambda (out inp)
    (display out)
    ))
(display (next-row))
(display (next-row))
(display (next-row))