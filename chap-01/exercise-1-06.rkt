#lang racket

(define (new-if predicate then-do else-do)
  (cond (predicate then-do)
        (else else-do)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x) (< (abs(- (* guess guess) x)) 0.0001 ))

(define(abs x) (if (< x 0) (- x) x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(sqrt 4)

;(new-if (= 2 3) 0 5)
