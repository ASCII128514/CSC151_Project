#lang racket
(provide (all-defined-out))

;;; Procedure:
;;;   all-and
;;; Parameters:
;;;   lst, a list of values
;;; Purpose:
;;;   apply and on all the values in the  list
;;; Returns:
;;;   res: boolean, or the last true value in the list
;;; Preconditions:
;;;   no additional
;;; Postconditions:
;;;  if the list is null, it will return #t
;;;  res will be #f if there are one or more #f in the list
;;;  else res will return the last element in the list
(define all-and
  (lambda (lst)
    (letrec ([kernel
      (lambda (bool bools)
        (if (null? bools)
          bool
          (kernel (and bool (car bools)) (cdr bools))))])
          (kernel #t lst))))


;;; Procedure:
;;;   split
;;; Parameters:
;;;   str:string
;;; Purpose:
;;;   split a line of csv file into elements
;;; Returns:
;;;   lst: list of parsed csv values
;;; Preconditions:
;;;   no additional
;;; Postconditions:
;;;   split will return the parsed standard csv format in list form.
;;;   lst will be a list of strings
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
; read only 10,000 lines

;;; Procedure:
;;;   csv-sort
;;; Parameters:
;;;   position: list of indexes that has to be integer
;;;   db: file path, string
;;;   leng: integer, the length of each lists
;;; Purpose:
;;;   lst: a list of parsed csv files
;;; Returns:
;;;   lst: list of parsed csv values
;;; Preconditions:
;;;   no additional
;;; Postconditions:
;;;   split will return the parsed standard csv format in list form.
;;;   lst will be a list of strings
(define csv-sort
  (lambda (position db leng)
    (let ([input (take (file->lines db) 100000)])
    (let ([content (map split input)])
      (cons (car content)
        (sort
                                (filter (lambda
                                          (entry)
                                          (and
                                            (= leng (length entry))
                                            (all-and (map (lambda (t) (string->number (list-ref entry t))) position))))
                                        (cdr content))
                                (lambda
                                  (r1 r2)
                                  (< (string->number (list-ref r1 (car position))) (string->number (list-ref r2 (car position)))))))))))



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


;;; Procedure:
;;;   read-csv
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   read the csv line and parse it into list of list of data
;;; Returns:
;;;   lst: list of list of data
;;; Preconditions:
;;;   str is the relative file path
;;; Postconditions:
;;;   lst will return the parsed csv in standard format
(define read-csv
  (lambda (str)
    (let ([input (file->lines str)])
      (map split input))))