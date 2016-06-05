#lang racket

(define (max-two-sum-of-three a b c)
  (cond ((and (> a c) (> b c)) (+ a b)
        ((and (> a b) (> c b)) (+ a c)))
        (else (+ b c))))

(max-two-sum-of-three 1 4 9)
