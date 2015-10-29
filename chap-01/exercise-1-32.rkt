#lang racket

(define (cube x)
  (* x x x))

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
  (accumulate-iter combiner (combiner (term a) null-value) term (next a) next b)))

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
  (combiner (term a) (accumulate-recur combiner null-value term (next a) next b))))

(define (product-iter n)
  (accumulate-iter * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (product-recur n)
  (accumulate-recur * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))


(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))


(define (sum-recur term a next b)
  (accumulate-recur + 0 term a next b))


(product-iter 3)

(sum-iter (lambda (x) x) 0 (lambda (x) (+ x 1)) 5)
