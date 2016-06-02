#lang racket

(define nil '())

(define (list-ref items n)
  (if (= n 0) (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1
         (length (cdr items)))))

(define (reverse raw-list)
  (define (reverse-iter old-list new-list n)
    (if (= n 0) new-list
        (reverse-iter old-list
                      (append new-list
                              (list (list-ref old-list (- n 1))))
                      (- n 1))))
  (reverse-iter raw-list `() (length raw-list)))



(define (deep-reverse raw-list)
  (cond ((null? raw-list) nil)
        ((not (pair? raw-list)) raw-list)
        (else (append (deep-reverse (cdr raw-list))
                      (list (deep-reverse (car raw-list)))))))



(define squares (list 1 4 9 16 25 36))

(define odds (list 1 3 5 7 9 11))

;(define test-it 5)

(append squares odds)

(deep-reverse squares)

(define x-case (list (list 1 2) (list 3 4)))

(deep-reverse x-case)

