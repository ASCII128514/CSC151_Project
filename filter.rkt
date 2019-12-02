#lang racket
(require racket/cmdline)
(require "csv-sort.rkt")
; file require
(define vec (current-command-line-arguments))
(when
  (< 4 (vector-length  vec))
  (error "usage: racket filter.rkt recipyPath ReviewPath outputRecipyPath outputReviewPath"))
(define recipies (vector-ref vec 0))
(define reviews (vector-ref vec 1))
(define sorted-recipies-output (vector-ref vec 2))
(define sorted-reviews-output (vector-ref vec 3))

(write-csv (csv-sort 1 recipies 12) sorted-recipies-output)
(write-csv (csv-sort 1 reviews 5) sorted-reviews-output)