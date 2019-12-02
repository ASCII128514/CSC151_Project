#lang racket
(provide (all-defined-out))

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
                           (let ([temp (scan str '() #\,)])
                             (kernel (cadr temp) (append lst (car temp))))])))))])
      (kernel input null))))

; filter the recipy to get clean data without all the columns
; name,id,minutes,contributor_id,submitted,tags,nutrition,n_steps,steps,description,ingredients,n_ingredients
(define csv-sort
  (lambda (position db leng)
    (let ([input (file->lines db)])
      (sort
                              (filter (lambda
                                        (entry)
                                        (and
                                          (= leng (length entry))
                                          (string->number (list-ref entry position))))
                                      (cdr (map split input)))
                              (lambda
                                (r1 r2)
                                (< (string->number (list-ref r1 position)) (string->number (list-ref r2 position))))))))



; data manipulation
;(define sorted-recipies (csv-sort 1 recipies 12))
;(define sorted-reviews (csv-sort 1 reviews 5))

; open the output port to the export the data.
(define write-line
  (lambda (lst port)
    (cond
      [(null? lst)
      (display #\newline port)]
      [(= 1 (length lst))
        (if
          (string-contains? (car lst )",")
          (write (car lst) port)
          (display (car lst) port))
          (write-line (cdr lst) port)]
      [else
        (if
          (string-contains? (car lst )",")
          (write (car lst) port)
          (display (car lst) port))
        (display "," port)
        (write-line (cdr lst) port)])))

(define write-csv
  (lambda (content path)
    (let ([output (open-output-file path)])
      (map
        (lambda (entry)
          (write-line entry output))
        content)
        (close-output-port output))))