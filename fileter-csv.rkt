#lang racket
(require csv-reading)
(define make-food-csv-reader
  (make-csv-reader-maker
   '(
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))
(define next-row (make-food-csv-reader (open-input-file "./RAW_interactions.csv")))
(display (next-row))