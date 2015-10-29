#lang racket

(define (cube x)
  (* x x x))

(define (intergral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx
     (sum f
          (+ a dx)
          add-dx
          b)))

(define (another-intergral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx
     (sum f
          (+ a
             (/ dx 2.0))
          add-dx
          b)))

(define (sum term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x)
              (+ (term x) result))))
  (iter a 0.0))

(intergral cube 0 1 0.0001)

(another-intergral cube 0 1 0.0001)
