#lang racket

(define tolerance 0.0000000000001)

(define (fixed-point f first-guess)
  (define (close-enough v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough guess next) next
        (try next))))
  (try first-guess))

(define (abs x)
  (if (< x 0.0) (- x)
      x))

(define (golden-section-ratio)
  (- (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1) 1.0))

(golden-section-ratio)
