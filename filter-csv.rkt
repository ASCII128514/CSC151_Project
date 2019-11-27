#lang racket
(require racket/cmdline)
(require csv-reading)
; this is the command line argument of path
(define vec (current-command-line-arguments))
(when
  (< 2 (vector-length  vec))
  (error "usage: racket filter-csv.rkt InputFile OutputFile"))
(define input (vector-ref vec 0))
(define output (vector-ref vec 1))
(define make-food-csv-reader
  (make-csv-reader-maker
   '(
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
(define next-row (make-food-csv-reader (open-input-file input)))
; read the header for the file
(define lst (csv->list next-row))
(car lst)
#|(define csv-writer
  (lambda (lst output)
    (let ([out (open-output-file output)]
          [header (car lst)])
      (letrec ([kernel
        (lambda output)]))
    )))|#
;(csv-map (lambda (z) 1) next-row)