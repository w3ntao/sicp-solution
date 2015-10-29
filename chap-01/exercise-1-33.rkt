#lang racket

(require (only-in "../ToolBox/Math/prime.rkt"
                  square
                  prime?))

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
  (accumulate-iter combiner (combiner (term a) null-value) term (next a) next b)))

(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (filtered-accumulate combiner filter (combiner (term a) null-value) term (next a) next b)
          (filtered-accumulate combiner filter null-value term (next a) next b))))

(define (sum-of-prime n)
  (filtered-accumulate + prime? 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (product-of-specific-prime n)
  (filtered-accumulate * (lambda (x) (= (gcd x n) 1)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(sum-of-prime 10)

(product-of-specific-prime 10)
