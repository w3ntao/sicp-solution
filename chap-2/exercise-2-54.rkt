#lang racket

(define (equal? list-0 list-1)
  (cond ((and (null? list-0) (null? list-1)) #t)
        ((or (null? list-0) (null? list-1)) #f)
        ((not (eq? (car list-0) (car list-1))) #f)
        (else (equal? (cdr list-0) (cdr list-1)))))


(display (equal? (list 1 2 3 4) (list 1 2 3 4)))
