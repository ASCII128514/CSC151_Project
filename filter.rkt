#lang racket
(require racket/cmdline)
(define vec (current-command-line-arguments))
(when
  (< 3 (vector-length  vec))
  (error "usage: racket filter.rkt recipyPath ReviewPath OutputFile"))

(define recipies (file->lines (vecter-ref 0)))
(define reviews (file->lines (vecter-ref 1)))
; filter the recipy to get clean data without all the columns
()
(sort recipies (lambda (r1 r2)))