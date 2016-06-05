#lang racket

(define (average a b)
  (/ (+ a b)
     2))

(define (abs x)
  (if (< x 0) (- x)
      x))

(define (square x)
  (* x x))

(define (iterative-improve improve good-enough)
  (lambda (x)
    (if (good-enough x) x
        ((iterative-improve improve good-enough) (improve x)))))

(define (sqrt x)
  (define (improve guess)
    (average guess
             (/ x guess)))
  (define tolerance 0.00001)
  (define (good-enough guess)
    (< (abs (- (square guess)
               x))
       tolerance))
  ((iterative-improve improve good-enough) 1))

(define (fixed-point f first-guess)
  (define (improve guess)
    (f guess))
  (define tolerance 0.00001)
  (define (good-enough guess)
    (< (abs (- guess
               (f guess)))
       tolerance))
  ((iterative-improve improve good-enough) 1))

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1)
(sqrt 9.0)
