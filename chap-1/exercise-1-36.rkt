#lang racket

(define tolerance 0.0000000000001)

(define (fixed-point f first-guess)
  (define (close-enough v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough guess next) next
        (try next))))
  (try first-guess))

(define (abs x)
  (if (< x 0) (- x)
      x))

(fixed-point (lambda (x)
               (/ (log 1000)
                  (log x)))
             2)
