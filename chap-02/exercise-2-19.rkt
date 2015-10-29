#lang racket

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denonimation coin-values)) coin-values)))))

(define (length items)
  (if (null? items)
      0
      (+ 1
         (length (cdr items)))))

(define (no-more? raw-list)
  (= (length raw-list) 0))

(define (except-first-denomination raw-list)
  (cdr raw-list))

(define (first-denonimation raw-list)
  (car raw-list))

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins)


