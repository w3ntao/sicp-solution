#lang racket

(define (even? n)
  (= (remainder n 2)
     0))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (mul a b)
  (if (= b 0)
      0
      (if (even? b)
          (mul (double a)
               (halve b))
          (+ a
             (mul a (- b 1))))))

(mul 7 4)
(mul 3 5)