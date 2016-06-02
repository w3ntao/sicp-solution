#lang racket

(define (list-ref items n)
  (if (= n 0) (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1
         (length (cdr items)))))

(define (reverse row-list)
  (define (reverse-iter old-list new-list n)
    (if (= n 0) new-list
        (reverse-iter old-list
                      (append new-list
                              (list (list-ref old-list (- n 1))))
                      (- n 1))))
  (reverse-iter row-list `() (length row-list)))

(define squares (list 1 4 9 16 25 36))

(define odds (list 1 3 5 7 9 11))



(append squares odds)

(reverse squares)

