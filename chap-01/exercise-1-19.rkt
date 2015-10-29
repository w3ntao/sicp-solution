#lang racket

(require (only-in "../ToolBox/Math/prime.rkt"
                  square))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b)
                  a
                  (- count 1))))
  (fib-iter 1 0 n))

(define (fib-fast n)
  (define (fib-iter a b p q count)
    (cond ((= count 0)
           b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (square q) (* 2 p q))
                     (/ count 2)))
          (else
           (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))

(define t 200)

(fib t)

(fib-fast t)
