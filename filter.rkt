#lang racket
(require racket/cmdline)
(define vec (current-command-line-arguments))
(when
  (< 2 (vector-length  vec))
  (error "usage: racket filter.rkt recipyPath ReviewPath"))
(define recipies (file->lines (vector-ref vec 0)))
(define reviews (file->lines (vector-ref vec 1)))

; define a split function to replace string-split to apply on csv lines.
(define split
  (lambda (input)
    (letrec ([kernel
              (lambda (str lst)
                (letrec ([scan
                          (lambda (str2 res symbol)
                            (if (equal? "" str2)
                                (list (list (list->string res)) str2)
                                (let ([char (string-ref str2 0)])
                                  (cond
                                    [(equal? symbol char)
                                     (list (list (list->string res)) (substring str2 1))]
                                    [else (scan (substring str2 1) (append res (list char)) symbol)]))))])
                  (if (equal? str "")
                      lst
                      (let ([char (string-ref str 0)])
                        (cond
                          [(= 34 (char->integer char))
                           (let ([temp (scan (substring str 1) '() char)])
                             (kernel (cadr temp) (append lst (car temp))))]
                          [(equal? char #\,)
                           (kernel (substring str 1) lst)]
                          [else
                           (let ([temp (scan (substring str 1) '() #\,)])
                             (kernel (cadr temp) (append lst (car temp))))])))))])
      (kernel input null))))

; filter the recipy to get clean data without all the columns
; name,id,minutes,contributor_id,submitted,tags,nutrition,n_steps,steps,description,ingredients,n_ingredients
(define csv-sort
  (lambda (position db leng)
    (sort
                            (filter (lambda
                                      (entry)
                                      (= leng (length entry)))
                                    (cdr (map split db)))
                            (lambda
                              (r1 r2)
                              (< (string->number (list-ref r1 position)) (string->number (list-ref r2 position)))))))
(define sorted-recipies (csv-sort 1 recipies 12))
(define sorted-reviews (csv-sort 1 reviews 5))
(display (take sorted-reviews 5))