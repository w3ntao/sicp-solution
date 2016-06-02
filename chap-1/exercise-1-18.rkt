#lang racket

(define (even? n)
  (= (remainder n 2)
     0))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (mul a b)
  (mul-iter a b 0))

(define (mul-iter a b temp)
  (cond ((= b 0) temp)
        ((even? b)
         (mul-iter (double a)
                   (halve b)
                   temp))
        (else
         (mul-iter a
                   (- b 1)
                   (+ temp a)))))

(mul 7 9)
(mul 3 5)