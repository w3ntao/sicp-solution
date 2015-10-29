#lang racket

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if(good-enough? guess x)
     guess
     (cube-root-iter (improve guess x) x)))

(define (good-enough? guess x) (< (abs(- (* guess guess guess) x)) 0.001 ))

(define(abs x) (if (< x 0) (- x) x))

(define (improve guess x) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(cube-root 27)
